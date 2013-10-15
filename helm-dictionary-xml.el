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

(require 's)
(require 'dash)

(cl-defun hdict:find-by-constraint ((tag &key id class (nth 0)) alist)
  "Find a tag grouping in ALIST matching a constraint."
  (cl-loop with counter = 0
           for elem in alist
           ;; Test whether this element matches the given id or class
           ;; constraint.
           if (listp elem) for (hd _attrs . bod) = elem
           if (and
               (equal tag hd)
               (or (null class) (ignore-errors (equal class (hdict:tag-class elem))))
               (or (null id)    (ignore-errors (equal id (hdict:tag-id elem)))))
           ;; If we have a match, check whether we're looking at the nth
           ;; match for the above constraint. If so, return the tag's body
           ;; from the loop.
           do (if (equal counter nth)
                  (cl-return bod)
                (cl-incf counter))))

(defun hdict:assoc-in (path alist)
  "Traverse xml ALIST along the given PATH of keys using `assoc'.
Return nil if the path cannot be followed.

Each element of PATH may be either a symbol or a tag constraint of the form:

 (tag [:nth n] [:id string] [:class string])

If nth is given, match the nth occurrence of the tag.

If class or id constraints are given, match only tags with those attributes. The nth
constraint then matches the nth occurence of the given tag or class."
  (if (and path alist)
      (cl-destructuring-bind (cur &rest next) path
        ;; If the current item it the path is a list, treat it as a constraint
        ;; list. Otherwise it's a symbol to match.
        (if (listp cur)
            (hdict:assoc-in next (hdict:find-by-constraint cur alist))
          (hdict:assoc-in next (cdr (assoc cur alist)))))
    alist))

(cl-defun hdict:tag-id ((&optional _tag attrs &rest rest_))
  (cdr (assoc 'id attrs)))

(cl-defun hdict:tag-class ((&optional _tag attrs &rest rest_))
  (cdr (assoc 'class attrs)))

;; Plist conversion.

(cl-defun hdict:a->string ((&optional _a _attrs content &rest rest_))
  (propertize (hdict:element->string content) 'face 'italic))

(cl-defun hdict:i->string ((&optional _i _ content))
  (propertize (hdict:element->string content) 'face 'italic))

(cl-defun hdict:b->string ((&optional _b _ content))
  (propertize (hdict:element->string content) 'face 'bold))

(cl-defun hdict:strong->string ((&optional _strong _ content))
  (propertize (hdict:element->string content) 'face 'bold))

(cl-defun hdict:em->string ((&optional _em &rest content))
  (concat "\n" (propertize (hdict:element->string content) 'face 'italic)))

(cl-defun hdict:list->string ((&optional _tag _attrs &rest elems))
  (hdict:element->string elems))

(defun hdict:element->string (x)
  (cond
   ((equal "\n" x) "")
   ((null x) "")
   ((symbolp x) "")
   ((stringp x) x)
   ((listp x)
    (cl-case (car x)
      ('a (hdict:a->string x))
      ('article (hdict:list->string x))
      ('audio "")
      ('b (hdict:b->string x))
      ('br "\n")
      ('canvas "")
      ('class "")
      ('comment "")
      ('div (hdict:element->string (cddr x)))
      ('em (hdict:em->string x))
      ('footer "")
      ('form "")
      ('header "")
      ('i (hdict:i->string x))
      ('id "")
      ('img "")
      ('link "")
      ('meta "")
      ('nav "")
      ('noscript "")
      ('ol (hdict:list->string x))
      ('option "")
      ('section (hdict:element->string (cddr x)))
      ('script "")
      ('span (hdict:element->string (cddr x)))
      ('strong (hdict:strong->string x))
      ('table "")
      ('ul (hdict:list->string x))
      ('video "")
      (otherwise
       (s-join "" (-map 'hdict:element->string x)))))
   (t
    "")))

(provide 'helm-dictionary-xml)

;; Local Variables:
;; lexical-binding: t
;; End:

;;; helm-dictionary-xml.el ends here
