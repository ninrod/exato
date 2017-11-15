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

;; close declarations }}}
;; tests {{{

(defun test/print-point ()
  (interactive)
  (princ (point)))

(defun test/tap-string ()
  (interactive)
  (princ (bounds-of-thing-at-point 'string)))

(defun test/let-bounds ()
  (interactive)
  (let ((bounds (bounds-of-thing-at-point 'string)))
    (cond
     (bounds (princ bounds))
     (t
      (message "nao achei porra nenhuma")))))

(defun test/str-start ()
  (interactive)
  (let* ((start (exato--find-str-start)))
    (cond (start (goto-char start))
          (t
           (message "não achei bosta nenhuma")))))

(defun test/delim-forward ()
  (interactive)
  (let* ((delim (exato--find-delimiter-forward)))
    (cond
     (delim (goto-char delim))
     (t
      nil))
    ))

(defun test/delim-backward ()
  (interactive)
  (let* ((delim (exato--find-delimiter-backward)))
    (cond
     (delim (goto-char delim))
     (t
      nil))
    ))

(defun nin/test= ()
  (interactive)
  (let ((pos (nin/find=)))
    (if pos
        (goto-char pos))))

(defun test/search-backward ()
  (interactive)
  (search-backward "<" (point-min) t))
(defun test/search-forward ()
  (interactive)
  (search-forward ">" (point-max) t))

;; }}}

;; exato--find-delimiter-forward {{{

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

;; }}}
;; exato--find-delimiter-backward {{{

(defun exato--find-str-start ()
  (interactive)
  (condition-case nil
      (save-excursion
        (beginning-of-thing 'string)
        (point))
    (error nil)))

(defun exato--find-delimiter-backward ()
  (save-excursion
    (let* ((start (exato--find-str-start)))
      (cond (start (goto-char (1- start)))
            (t
             nil)))
    (when (looking-at " ")
      (skip-chars-forward " \n\t"))
    (let ((tag-open
           (save-excursion
             (if (search-backward "<" (point-min) t)
                 (point)
               (point-min)))))
      (if (re-search-forward "=\"" tag-open t)
          (- (point) 2)
        nil))))

;; }}}

;; connect evil machinery {{{

(defun evil-xml-attr-range ()
  (let ((start (nin/find-attr-begin))
        (finish (nin/find-attr-end)))
    (when (and start finish)
      (evil-range start finish))))

(evil-define-text-object evil-inner-xml-attr (count &optional beg end type)
  (evil-xml-attr-range))

(define-key evil-inner-text-objects-map "x" 'evil-inner-xml-attr)

;; }}}



