;; cl-facts
;; Copyright 2011-2022 Thomas de Grivel <thodg@kmx.io>
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

(defun lessp/3 (a1 a2 a3 b1 b2 b3)
  (or (lessp a1 b1)
      (and (not (lessp b1 a1))
           (or (lessp a2 b2)
               (and (not (lessp b2 a2))
                    (lessp a3 b3))))))

(defun fact-spo-lessp (a b)
  (or (null a)
      (and b
           (lessp/3 (fact-subject a) (fact-predicate a) (fact-object a)
                    (fact-subject b) (fact-predicate b) (fact-object b)))))

(defun fact-pos-lessp (a b)
  (or (null a)
      (and b
           (lessp/3 (fact-predicate a) (fact-object a) (fact-subject a)
                    (fact-predicate b) (fact-object b) (fact-subject b)))))

(defun fact-osp-lessp (a b)
  (or (null a)
      (and b
           (lessp/3 (fact-object a) (fact-subject a) (fact-predicate a)
                    (fact-object b) (fact-subject b) (fact-predicate b)))))

;;  Index operations

;;    skip lists

(defun make-index (lessp)
  (make-usl :lessp lessp))

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
