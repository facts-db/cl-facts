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

(add ('result 'source ?s)
     (?s 'id "plop")
     (?s 'score 42))

(trace replace-bindings collect-bindings% with%)
(trace make-fact/v fact/v-subject fact/v-predicate fact/v-object)

(with ((?s ?p ?o))
  (format t "~&~S ~S ~S~&" ?s ?p ?o))

(llrbtree:map-tree (lambda (key value)
                     (format t "~&~S -> ~S~%" key value))
                   (db-pos-tree *db*)
                   :START (MAKE-FACT/V NIL nil NIL))

(macroexpand-1
 (third
  (macroexpand-1
   '(with (('result 'source ?p))
     (format t "~S~&" ?p)))))

(with (('result 'source ?p)
       (?p 'id ?id))
  (return (list ?p ?id)))

(with (('result 'source ?p)
       (?p 'id ?id)
       (?p 'score ?score))
  (format t "~S~%" (list (list ?p 'score ?score)
                         (list ?p 'id ?id))))
