;;; helm-dictionary-english-gb.el --- British English dictionary for Helm

;; Copyright (C) 2013 Chris Barrett

;; Author: Chris Barrett <chris.d.barrett@me.com>
;; Version: 20131010.0309

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

;; British English dictionary for Helm.

;;; Code:

(require 'helm-dictionary-xml)
(require 'dash)
(require 'dash-functional)
(require 'helm)

;; (english-gb-dictionary-search "chair")

;; (setq alist
;;       (let ((url (hengb:format-query-url "chair")))
;;         (with-current-buffer (url-retrieve-synchronously url)
;;           (hengb:normalise-line-endings-in-buffer)
;;           (libxml-parse-html-region (point-min) (point-max)))))


(defun hengb:format-query-url (query)
  (concat "http://oxforddictionaries.com/definition/english/" query))

(defun hengb:format-dict-item (heading element)
  (concat (propertize heading 'face 'bold) " : "
          (hdict:element->string element)))

(defun hengb:alist->definitions (alist)
  (let* ((container
          (hdict:assoc-in
           '(body
             (div :class "responsive_container" :nth 1)
             (div :class "responsive_row")
             (div :class "responsive_cell_center")
             (div :class "responsive_row")
             (div :class "responsive_cell_center")
             (div :id "ContentBox")
             article
             (div :id "mainContent"))
           alist))
         (word
          (hdict:element->string (hdict:assoc-in '(header h2) container)))
         (senses
          (->> container
            (hdict:assoc-in '((div :id "entryPageContent") div))
            (--filter (hdict:find-by-constraint '(section :class "senseGroup")
                                                (list it)))))
         (etymology
          (->> container
            (hdict:assoc-in '((div :id "entryPageContent")
                              div
                              (div :class "etymology")
                              div
                              p)))))
    `(,@(--map (hengb:format-dict-item word it) senses)
      ,(hengb:format-dict-item "Origin" etymology))))

(defun hengb:normalise-line-endings-in-buffer ()
  (goto-char (point-min))
  (while (search-forward "\r" nil t)
    (replace-match "")))

(defun english-gb-dictionary-search (query)
  "Search for QUERY and parse it into a list of definitions for helm."
  (let ((url (hengb:format-query-url query)))
    (with-current-buffer (url-retrieve-synchronously url)
      (hengb:normalise-line-endings-in-buffer)
      ;; Parse response.
      (hengb:alist->definitions
       (libxml-parse-html-region (point-min) (point-max))))))

(defvar helm-source-english-gb-dictionary
  '((name . "English Dictionary")
    (candidates . (lambda () (english-gb-dictionary-search helm-pattern)))
    (volatile)
    (multiline)
    (requires-pattern . 3)
    (delayed)))

;;;###autoload
(defun helm-dictionary-english-gb ()
  "Perform an English dictionary search at <http://oxforddictionaries.com>."
  (interactive)
  (helm helm-source-english-gb-dictionary))

(provide 'helm-dictionary-english-gb)

;; Local Variables:
;; lexical-binding: t
;; End:

;;; helm-dictionary-english-gb.el ends here
