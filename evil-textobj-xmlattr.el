;; -*- lexical-binding: t -*-

;; header {{{

;;; evil-textobj-xmlattr.el --- EXATO: Evil Xml Attributes Text Object

;; Copyright (C) 2015 by Filipe Correa Lima da Silva

;; Author: Filipe Correa Lima da Silva <filipe.silva@gmail.com>
;; URL: https://github.com/ninrod/evil-textobj-xmlattr.el
;; Version: 0.0.1
;; Package-Requires: ((evil "1.2.13") (thingatpt+ "0"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; this is a scratch implementaion for an evil text object for hmtl/xml tag attributes,
;; inspired by https://github.com/whatyouhide/vim-textobj-xmlattr

;; examples:
;; dax: delete around xml tag attribute
;; dix: delete inside xml tag attribute

;;; Code:

;; }}}
;; declarations {{{

(require 'evil)
(require 'thingatpt+)

;; close declarations }}}

;; exato--find-str-start {{{

(defun exato--find-str-start ()
  (interactive)
  (condition-case nil
    (save-excursion
  (beginning-of-thing 'string)
  (point))
    (error nil)))

;; }}}
;; exato--find-str-end {{{

(defun exato--find-str-end ()
  (interactive)
  (condition-case nil
    (save-excursion
  (end-of-thing 'string)
  (1- (point)))
    (error nil)))

;; }}}

;; exato--find-delimiter-backward {{{

(defun exato--find-delimiter-backward ()
  (let ((str-start (exato--find-str-start)))
    (cond (str-start
           (save-excursion
             (goto-char (1- str-start))
             (cond ((looking-at "=")
                    (point))
                   (t
                    nil))))
          ((looking-at "[[:alpha:]]")
           (save-excursion
             (skip-chars-backward "[[:alpha:]]")
             (backward-char)
             (cond ((looking-at "=")
                    (point))
                   (t
                    nil))))
          (t nil))))

;; }}}
;; exato--find-delimiter-forward {{{

(defun exato--find-delimiter-forward ()
  (save-excursion
    (when (looking-at " ")
      (skip-chars-forward " \n\t"))
    (let ((tag-close
           (save-excursion
             (cond ((search-forward ">" (point-max) t)
                    (1- (point)))
                   (t
                    (point-max))))))
      (cond ((re-search-forward "=\"" tag-close t)
             (- (point) 2))
            ((re-search-forward "=" tag-close t)
             (1- (point)))
            (t
             nil)))))

;; }}}
;; exato--find-delimiter {{{

(defun exato--find-delimiter ()
  (interactive)
  (let* ((backward (exato--find-delimiter-backward))
         (forward (exato--find-delimiter-forward)))
    (cond (backward backward)
          (forward forward)
          (t nil))))

;; }}}

;; exato--find-xml-attr-start {{{

(defun exato--find-xml-attr-start ()
  (let ((delimiter (exato--find-delimiter)))
    (cond (delimiter
           (save-excursion
             (goto-char delimiter)
             (skip-chars-backward "^ ")
             (point)))
          (t
           nil))))

;; }}}
;; exato--find-xml-attr-end {{{

(defun exato--find-xml-attr-end ()
  (interactive)
  (let* ((delimiter (exato--find-delimiter)))
    (cond (delimiter
           (save-excursion
             (goto-char (1+ delimiter))
             (cond ((looking-at "\"")
                    (exato--find-str-end))
                   (t
                    (cond ((looking-at "[[:alpha:]]")
                           (save-excursion
                             (skip-chars-forward "[[:alpha:]]")
                             (backward-char)
                             (point)))
                          (t
                           nil))))))
          (t nil))))

;; }}}

;; connect to evil machinery {{{

(defun evil-xml-attr-inner-range ()
  (let ((start (exato--find-xml-attr-start))
        (finish (exato--find-xml-attr-end)))
    (cond ((and start finish)
           (evil-range start (1+ finish)))
          (t
           nil))))

(defun evil-xml-attr-outer-range ()
  (let ((start (exato--find-xml-attr-start))
        (finish (exato--find-xml-attr-end)))
    (cond ((and start finish)
           (evil-range (1- start) (1+ finish)))
          (t
           nil))))

(evil-define-text-object evil-inner-xml-attr (count &optional beg end type)
  (evil-xml-attr-inner-range))
(evil-define-text-object evil-outer-xml-attr (count &optional beg end type)
  (evil-xml-attr-outer-range))

(define-key evil-outer-text-objects-map "x" 'evil-outer-xml-attr)
(define-key evil-inner-text-objects-map "x" 'evil-inner-xml-attr)

(provide 'evil-textobj-xmlattr)

;; }}}