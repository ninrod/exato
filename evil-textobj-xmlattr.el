;; -*- lexical-binding: t -*-

;; para achar o começo
;; procura o = para frente
;;      mas se achar: espaço, quotes ou >,
;;      então desiste porque ele só pode estar pra trás
;;  procura o = pra trás, usa < como limite.
;;      se achar, é porque não é um attr. desiste de tudo
;;  achou o igual, mete o cursor em cima dele.
;;  skip alnum pra trás. esse é o begin

;; TODO tratar casos em que point está no "vazio"; seek forward via skip-chars?

(require 'cl-lib)

(defun nin/begin ()
  (interactive)
  (skip-chars-backward "[:alnum:]"))

(cl-defun nin/find-forward= ()
  (save-excursion
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

(cl-defun nin/find-attr-begin ()
  (interactive)
  (let ((pos (nin/find=)))
    (when pos
      (skip-chars-backward "[:alnum:]")
      (cl-return-from nin/find-attr-begin (point)))
    nil))

(cl-defun nin/find-attr-end ()
  (let ((pos (nin/find=)))
    (when pos
      (save-excursion
        (goto-char (1+ pos))
        (skip-chars-forward "[:alnum:]")
        (when (or (looking-at ">") (looking-at "\n") (looking-at " "))
          (backward-char 1)
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
            (backward-char 1)
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

(defun nin/try ()
  (interactive)
  (when (search-forward ">" (point-max) t)
    (backward-char 1)))

;; <a href="index.html" class="{{minha.classe}}foo bar" id=none >blah</a>
