;; -*- lexical-binding: t -*-

;; header {{{

;; para achar o começo
;; 1. procura a regexp =\" para frente usando > ou point-max como limite
;;      se não achar, então desiste porque só pode estar pra trás
;; 2. procura a a regexp =\" pra trás, usa < como limite ou point-min
;;      se não achar, é porque não é um attr. retorna nil
;; 3. achou o igual canônico, mete o cursor em cima dele.
;; 4. skipa para o primeiro espaço pra trás. esse espaço sempre existe. sempre. esse é o BEGIN.
;; 5. volta pro igual canônico. agora procura pelo outro quote

;; }}}
;; declarations {{{

(require 'evil)
(require 'cl-lib)
(require 'thingatpt+)

;; https://www.emacswiki.org/emacs/StringAtPoint
(defun ash-forward-string (&optional arg)
  "Move forward to ARGth string."
  (setq arg (or arg 1))
  (if (not (bobp))
      (save-match-data
        (when (or (and (looking-at-p "\\s-*\"")
                       (not (looking-back "\\\\")))
                  (re-search-backward "[^\\\\]\"" nil nil))
          (looking-at "\\s-*\"")
          (goto-char (match-end 0))
          (forward-char -1))))
  (while (and (> arg 0)
              (not (eobp))
              (looking-at-p "\\s-*\""))
    (forward-sexp 1)
    (setq arg (1- arg)))
  (while (and (< arg 0)
              (not (bobp))
              (looking-at-p "\""))
    (forward-sexp -1)
    (setq arg (1+ arg)))
  (ignore))
(put 'string 'forward-op 'ash-forward-string)

;; close declarations }}}

(defun exato--find-delimiter-forward ()
  (save-excursion
    (when (looking-at " ")
      (skip-chars-forward " \n\t"))
    (let ((tag-close
           (save-excursion
             (if (search-forward ">" (point-max) t)
                 (1- (point))
               (point-max)))))
      (if (re-search-forward "=\"" tag-close t)
          (- (point) 2)
        nil))))

(defun exato--find-delimiter-backward ()
  (save-excursion
    (when (looking-at " ")
      (skip-chars-forward " \n\t"))
    (let ((tag-close
           (save-excursion
             (if (search-forward ">" (point-max) t)
                 (1- (point))
               (point-max)))))
      (if (re-search-forward "=\"" tag-close t)
          (- (point) 2)
        nil))))
(defun test/tap-string ()
  (interactive)
  (princ (goto-char (1- (cdr (bounds-of-thing-at-point 'string))))))
;; "uma string muito grande "

(defun test/print-point ()
  (interactive)
  (princ (point)))

(cl-defun nin/find= ()
  (let ((pos (xato/find-delimiter-forward)))
    (when pos
      (cl-return-from nin/find= pos)))
  (let ((pos (xato/find-delimiter-backward)))
    (when pos
      (cl-return-from nin/find= pos)))
  nil)

(defun nin/find-attr-begin ()
  (interactive)
  (let ((pos (nin/find=)))
    (if pos
        (save-excursion
          (goto-char pos)
          (skip-chars-backward "^ ")
          (point))
      nil)))

(cl-defun nin/find-attr-end ()
  (let ((pos (nin/find=)))
    (when pos
      (save-excursion
        (goto-char (1+ pos))
        (skip-chars-forward "[:alnum:]")
        (when (or (looking-at ">") (looking-at "\n") (looking-at " "))
          (cl-return-from nin/find-attr-end (point))))
      (save-excursion
        (goto-char (1+ pos))
        (when (looking-at "\"")
          (let ((limit (save-excursion
                         (if (search-forward ">" (point-max) t)
                             (1- (point))
                           (cl-return-from nin/find-attr-end nil))))
                (point-on-opening-quote (point)))
            (forward-char 1)
            (search-forward "\"" limit t)
            (when (> (point) point-on-opening-quote)
              (cl-return-from nin/find-attr-end (point)))))))
    nil))

(cl-defun nin/test-end ()
  (interactive)
  (let ((pos (nin/find-attr-end)))
    (when pos
      (goto-char pos)
      (cl-return-from nin/test-end pos)))
  (message "não achei nada"))

(cl-defun nin/test-begin ()
  (interactive)
  (let ((pos (nin/find-attr-begin)))
    (when pos
      (goto-char pos)
      (cl-return-from nin/test-begin pos)))
  (message "não achei nada"))

(defun evil-xml-attr-range ()
  (let ((start (nin/find-attr-begin))
        (finish (nin/find-attr-end)))
    (when (and start finish)
      (evil-range start finish))))

(evil-define-text-object evil-inner-xml-attr (count &optional beg end type)
  (evil-xml-attr-range))

(defun nin/test= ()
  (interactive)
  (let ((pos (nin/find=)))
    (if pos
        (goto-char pos))))

(defun try ()
  (interactive)
  (skip-chars-backward "[:alnum:]"))

(define-key evil-inner-text-objects-map "x" 'evil-inner-xml-attr)
