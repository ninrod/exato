;; -*- lexical-binding: t -*-

;; para achar o começo
;; procura o = para frente
;;      mas se achar: espaço, quotes ou >,
;;      então desiste porque ele só pode estar pra trás
;;  procura o = pra trás, usa < como limite.
;;      se achar, é porque não é um attr. desiste de tudo
;;  achou o igual, mete o cursor em cima dele.
;;  skip alnum pra trás. esse é o begin

(require 'evil)
(require 'cl-lib)

(cl-defun nin/find-forward= ()
  (save-excursion
    (when (looking-at " ")
      (skip-chars-forward " \n\t"))
    (while (and (not (looking-at "="))
                (not (looking-at " "))
                (not (looking-at ">"))
                (not (= (point) (point-max))))
      (forward-char 1))
    (when (looking-at "=")
      (cl-return-from nin/find-forward= (point))))
  nil)

(cl-defun nin/find-backward= ()
  (save-excursion
    (while (and (not (looking-at "="))
                (not (looking-at "<"))
                (not (= (point) (point-min))))
      (backward-char 1))
    (when (looking-at "=")
      (cl-return-from nin/find-backward= (point))))
  nil)


(cl-defun nin/find= ()
  (let ((pos (nin/find-forward=)))
    (when pos
      (cl-return-from nin/find= pos)))
  (let ((pos (nin/find-backward=)))
    (when pos
      (cl-return-from nin/find= pos)))
  nil)

(defun nin/find-attr-begin ()
  (interactive)
  (let ((pos (nin/find=)))
    (if pos
        (save-excursion
          (goto-char pos)
          (skip-chars-backward "[:alnum:]")
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

;; <a href="app/index.html" class="foo bar buz" id=none disable>