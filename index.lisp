;; cl-facts
;; Copyright 2011, 2023 Thomas de Grivel <thodg@kmx.io>
;;
;; Permission is hereby granted to use this software granted
;; the above copyright notice and this permission paragraph
;; are included in all copies and substantial portions of this
;; software.
;;
;; THIS SOFTWARE IS PROVIDED "AS-IS" WITHOUT ANY GUARANTEE OF
;; PURPOSE AND PERFORMANCE. IN NO EVENT WHATSOEVER SHALL THE
;; AUTHOR BE CONSIDERED LIABLE FOR THE USE AND PERFORMANCE OF
;; THIS SOFTWARE.

(in-package :facts)

;;  Facts ordering

(defun compare/3 (a1 a2 a3 b1 b2 b3)
  (ecase (compare a1 b1)
    (-1 -1)
    (1 1)
    (0 (ecase (compare a2 b2)
         (-1 -1)
         (1 1)
         (0 (ecase (compare a3 b3)
              (-1 -1)
              (1 1)
              (0 0)))))))

(defun compare-facts-spo (a b)
  (compare/3 (fact-subject a) (fact-predicate a) (fact-object a)
             (fact-subject b) (fact-predicate b) (fact-object b)))

(defun compare-facts-pos (a b)
  (compare/3 (fact-predicate a) (fact-object a) (fact-subject a)
             (fact-predicate b) (fact-object b) (fact-subject b)))

(defun compare-facts-osp (a b)
  (compare/3 (fact-object a) (fact-subject a) (fact-predicate a)
             (fact-object b) (fact-subject b) (fact-predicate b)))

;;  Index operations

;;    skip lists

(defun make-index (compare)
  (make-usl :compare compare))

(defun index-get (index fact)
  (declare (type fact/v fact))
  (usl-find index fact))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf (rollback-function 'index-insert) 'index-delete)
  (setf (rollback-function 'index-delete) 'index-insert))

(defun index-insert (index fact)
  (declare (type fact/v fact))
  (usl-insert index fact))

(defun index-delete (index fact)
  (declare (type fact/v fact))
  (usl-delete index fact))

(defun index-each (index fn &key start end)
  (usl-each index fn :start start :end end))
