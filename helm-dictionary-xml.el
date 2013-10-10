;;; helm-dictionary-xml.el --- Parser utilites for XML.

;; Copyright (C) 2013 Chris Barrett

;; Author: Chris Barrett <chris.d.barrett@me.com>
;; Version: 20131010.0303

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

;; Parser utilites for XML.

;;; Code:

(defun hdict:assoc-in (path alist)
  "Traverse xml ALIST along the given PATH of keys using `assoc'.
Return nil if the path cannot be followed.

Each element of PATH my be either a symbol or a list of the form
 (tag [:id|:class] \"label\"), which will attempt to match 'tag'
with the given id or class."
  (if (and path alist)
      (cl-destructuring-bind (cur &rest next) path
        ;; If the current item it the path is a list, treat it as a keyword list.
        (if (listp cur)
            (cl-destructuring-bind (tag &key id class) cur
              ;; Find the first tag matching the given class or id.
              (hdict:assoc-in
               next
               (cdr
                (cl-loop for x in alist
                         if (consp x)
                         if (or (and class (equal class (hdict:tag-class x)))
                                (and id (equal id (hdict:tag-id x))))
                         do (return x)))))

          (hdict:assoc-in next (cdr (assoc cur alist)))))
    alist))

(cl-defun hdict:tag-id ((&optional tag attrs &rest rest_))
  (cdr (assoc 'id attrs)))

(cl-defun hdict:tag-class ((&optional tag attrs &rest rest_))
  (cdr (assoc 'class attrs)))

;; Plist conversion.

(cl-defun hdict:a->string ((_a _href str)) (propertize str 'face 'italic))
(cl-defun hdict:i->string ((_i _ str)) (propertize str 'face 'italic))
(cl-defun hdict:b->string ((_b _ str)) (propertize str 'face 'bold))
(cl-defun hdict:strong->string ((_strong _ str)) (propertize str 'face 'bold))
(cl-defun hdict:br->string (&rest _) "\n")
(cl-defun hdict:em->string ((_em &rest content))
  (concat "\n\n"
          (propertize (apply 'concat (-map 'hdict:element->string content))
                      'face 'italic)))

(defun hdict:element->string (x)
  (cond
   ((equal "\n" x) "")
   ((stringp x) x)
   ((listp x)
    (cl-case (car x)
      ('a (hdict:a->string x))
      ('i (hdict:i->string x))
      ('em (hdict:em->string x))
      ('b (hdict:b->string x))
      ('strong (hdict:strong->string x))
      ('br (hdict:br->string x))
      ('comment "")
      ('article "")
      ('class "")
      ('header "")
      ('div (hdict:element->string (cdr x)))
      (nil "")
      (otherwise
       (apply 'concat (-map 'hdict:element->string x)))))
   (t
    "")))

(provide 'helm-dictionary-xml)

;; Local Variables:
;; lexical-binding: t
;; End:

;;; helm-dictionary-xml.el ends here
