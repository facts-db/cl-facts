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

;;  Bindings

(defun binding-p (sym)
  (when (typep sym 'symbol)
    (char= #\? (char (symbol-name sym) 0))))

(defun collect-bindings (form &optional bindings)
  (typecase form
    (null bindings)
    (symbol (if (binding-p form)
                (pushnew form bindings)
                bindings))
    (cons (collect-bindings (car form)
                            (collect-bindings (cdr form)
                                              bindings)))
    (t bindings)))

(defun gensym-bindings (bindings)
  (mapcar (lambda (b)
            (cons b (gensym (concatenate 'string
                                         (string-upcase (subseq (string b) 1))
                                         "-"))))
          bindings))

;;  Ordering of join patterns

(defun fact-binding-count (x)
  (+ (if (binding-p (pop x)) 1 0)
     (if (binding-p (pop x)) 1 0)
     (if (binding-p (pop x)) 1 0)))

(defun pattern< (a b)
  (< (fact-binding-count a)
     (fact-binding-count b)))

(defun sort-bindings (pattern)
  "Transforms ((?s ?p ?o) (?s x y)) into ((?s x y) (?s ?p ?o)). Huge optimization."
  (sort pattern #'pattern<))

;;  Fact specifications

(defun expand-positive-spec (spec)
  (destructuring-bind (s p o &rest more-p-o) spec
    (labels ((expand/po (p-o-list result)
               (if (endp p-o-list)
                   result
                   (destructuring-bind (p o &rest list) p-o-list
                     (expand/po list (cons `(,s ,p ,o)
                                           result))))))
      (nreverse (expand/po more-p-o
                           (cons `(,s ,p ,o)
                                 nil))))))

(defun expand-negative-spec (spec)
  (destructuring-bind (not s p o &rest more-p-o) spec
    (assert (eq :not not))
    (labels ((expand/po (p-o-list result)
               (if (endp p-o-list)
                   result
                   (destructuring-bind (p o &rest list) p-o-list
                     (expand/po list (cons `(:not ,s ,p ,o)
                                           result))))))
      (nreverse (expand/po more-p-o
                           (cons `(:not ,s ,p ,o)
                                 nil))))))

(defun expand-spec (spec)
  (declare (type sequence spec))
  (let ((len (length spec)))
    (if (zerop (mod (- len 3) 2))
        (expand-positive-spec spec)
        (expand-negative-spec spec))))

(defun expand-specs (specs)
  "
Facts specification

For any values of subject S, predicate P, object O we can write a fact as a triple :
    (S P O)
A join between multiple facts is written as a set of facts :
    ((S1 P1 O1) ... (Sn Pn On))
For more predicates and objects for the same subject we can also write :
    ((S P1 O1 ... Pn On))
which is equivalent to :
    ((S P1 O1) ... (S Pn On))
"
  (mapcan #'expand-spec specs))
