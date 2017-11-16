(require 'evil)
(require 'thingatpt+)

(defun test/skip-chars-backward= ()
  (interactive)
  (skip-chars-backward "[[:alpha:]]"))

(defun test/look-test ()
  (interactive)
  (cond ((looking-at "[[:alnum:]]")
         (message "looking at an alpha"))
        (t (message "did not work"))))

(defun test/print-point ()
  (interactive)
  (princ (point)))

(defun test/let-string-bounds ()
  (interactive)
  (let ((bounds (bounds-of-thing-at-point 'string)))
    (cond
     (bounds (princ bounds))
     (t
      (message "did not find anything")))))

(defun test/str-start ()
  (interactive)
  (let* ((start (exato--find-str-start)))
    (cond (start (goto-char start))
          (t
           (message "did not find anything")))))

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
      (message "did not find anything")))))

(defun test/find-delimiter ()
  (interactive)
  (let ((delimiter (exato--find-delimiter)))
    (cond (delimiter (goto-char delimiter))
          (t (message "did not find anything")))))

(defun test/find-xml-start ()
  (interactive)
  (let* ((start (exato--find-xml-attr-start)))
    (cond (start (goto-char start))
          (t
           nil))))

(defun test/find-xml-end ()
  (interactive)
  (let* ((end (exato--find-xml-attr-end)))
    (cond (end (goto-char end))
          (t
           nil))))

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