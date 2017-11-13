;; -*- lexical-binding: t -*-

;; para achar o começo
;; procura o = para frente
;;      mas se achar: espaço, quotes ou >,
;;      então desiste porque ele só pode estar pra trás
;;  procura o = pra trás, usa < como limite.
;;      se achar, é porque não é um attr. desiste de tudo
;;  achou o igual, mete o cursor em cima dele.
;;  skip alnum pra trás. esse é o begin

(require 'cl-lib)

(defun nin/begin ()
  (interactive)
  (skip-chars-backward "[:alnum:]"))

(cl-defun nin/find-forward= ()
  (save-excursion
    (while (and (not (looking-at "="))
                (not (looking-at " "))
                (not (looking-at ">")))
      (forward-char 1))
    (when (looking-at "=")
      (message "o = tava pra frente!")
      (cl-return-from nin/find-forward= (point))))
  nil)

(defun nin/find= ()
  (interactive)
  (let* ((pos (nin/find-forward=)))
    (when pos (goto-char pos))))

;; <a href="index.html" class="foo bar" id=none>blah</a>