;;; helm-dictionary-maori.el --- Māori language dictionary for helm.

;; Copyright (C) 2013 Chris Barrett

;; Author: Chris Barrett <chris.d.barrett@me.com>
;; Version: 0.1

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
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

;; Māori language dictionary for helm.

;;; Code:

(require 'cl-lib)
(require 's)
(require 'dash)
(require 'dash-functional)

;;; Parsers
;;;
;;; Convert Elisp plist representations of the web page into strings.

(cl-defun mdict:a->string ((_a _href str)) (propertize str 'face 'italic))
(cl-defun mdict:i->string ((_i _ str)) (propertize str 'face 'italic))
(cl-defun mdict:b->string ((_b _ str)) (propertize str 'face 'bold))
(cl-defun mdict:strong->string ((_strong _ str)) (propertize str 'face 'bold))
(cl-defun mdict:br->string (&rest _) "\n")
(cl-defun mdict:em->string ((_em &rest content))
  (concat "\n\n"
          (propertize (apply 'concat (-map 'mdict:element->string content))
                      'face 'italic)))

(defun mdict:element->string (x)
  (cond
   ((equal "\n" x) "")
   ((stringp x) x)
   ((listp x)
    (cl-case (car x)
      ('a (mdict:a->string x))
      ('i (mdict:i->string x))
      ('em (mdict:em->string x))
      ('b (mdict:b->string x))
      ('strong (mdict:strong->string x))
      ('br (mdict:br->string x))
      ('comment "")
      ('article "")
      ('class "")
      ('header "")
      ('div (mdict:element->string (cdr x)))
      (nil "")
      (otherwise
       (apply 'concat (-map 'mdict:element->string x)))))
   (t
    "")))

(defun mdict:def->string (def)
  (let ((text (apply 'concat (-map 'mdict:element->string def))))
    (s-with-temp-buffer
      ;; Remove junk chars and insert.
      (insert (s-replace " " " " text))
      (goto-char (point-min))

      ;; Add a space in padding between numbers and word category.
      (save-excursion
        (while (search-forward-regexp (rx (group (+ digit) ".")) nil t)
          (just-one-space)))

      ;; Delete trailing braces.
      (save-excursion
        (while (search-forward-regexp (rx (group (any "()")) (* space) eol) nil t)
          (replace-match "" nil nil nil 1)))

      ;; Delete text references, eg:
      ;;
      ;;   Te Māhuri Textbook (Ed. 2): 181-187
      (save-excursion
        (while (search-forward-regexp
                (rx bol
                    (+ nonl) ":" (+ space) (+ (any digit "-")) ";"
                    eol) nil t)
          (replace-match "")))

      ;; Fill the text to the width of the helm window.
      (let ((fill-column (- (window-width)
                            (fringe-columns 'left)
                            (fringe-columns 'right))))
        (fill-region (point-min) (point-max)))

      (let ((delete-trailing-lines t))
        (delete-trailing-whitespace)))))

(defun mdict:plist->definitions (plist)
  (->> (assoc-in '(body div div article article) plist)
    (-drop 2)
    (-mapcat
     (lambda (section)
       (let ((word (cadr (assoc-in '(div h2) section))))
         (->> (cdr (assoc-in '(ul li) section))
           (-map 'mdict:def->string)
           (-remove (-compose 's-blank? 's-trim))
           (--map (concat word " : " it))))))))

;;; Main functions

(defun mdict:format-query-url (query)
  (concat "http://www.maoridictionary.co.nz/index.cfm"
          "?idiom=&phrase=&proverb=&loan=&search="
          "&dictionaryKeywords=" (url-hexify-string query)))

(defun maori-dictionary-search (query)
  "Search for QUERY and parse it into a list of definitions for helm."
  (let ((url (mdict:format-query-url query)))
    (with-current-buffer (url-retrieve-synchronously url)
      (mdict:plist->definitions
       (libxml-parse-html-region (point-min) (point-max))))))

(defun mdict:open-in-browser (_)
  (browse-url (mdict:format-query-url helm-pattern)))

(defvar helm-source-maori-dictionary
  '((name . "Māori Dictionary")
    (candidates . (lambda () (maori-dictionary-search helm-pattern)))
    (action . mdict:open-in-browser)
    (volatile)
    (multiline)
    (requires-pattern . 3)
    (delayed)))

;;;###autoload
(defun helm-dictionary-maori ()
  "Perform a Māori dictionary search at <http://www.maoridictionary.co.nz>."
  (helm helm-source-maori-dictionary))

(provide 'helm-dictionary-maori)

;; Local Variables:
;; lexical-binding: t
;; End:

;;; helm-dictionary-maori.el ends here
