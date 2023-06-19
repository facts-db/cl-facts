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

;;  Inspect database

(defun about (x)
  (let ((i (the fixnum 0)))
    (with ((x ?p ?o))
      (when (= 0 i)
        (incf i)
        (format t "~&(~S" x))
      (format t "~%  ~S ~S" ?p ?o))
    (unless (= 0 i)
      (format t ")~%")))
  (with ((?s x ?o))
    (format t "(~S ~A ~S)~%" ?s x ?o))
  (with ((?s ?p x))
    (format t "(~S ~S ~A)~%" ?s ?p x)))
