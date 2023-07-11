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

;;  Anonymous values

(defpackage :facts.anon)

(defvar *randomize-anon* nil)

(defun anon (&rest sym-hints)
  (let* ((sym (intern
               (with-output-to-string (out)
                 (write-string (string-upcase (first sym-hints)) out)
                 (dolist (sym-hint (rest sym-hints))
                   (write-char #\- out)
                   (write-string (string-upcase sym-hint) out)))
               (find-package :facts.anon))))
    (labels ((guess (count)
               (multiple-value-bind (n found) (intern
                                               (format nil "~A-~4,'0X"
                                                       sym count)
                                               :facts.anon)
                 (if found
                     (if *randomize-anon*
                         (guess (random most-positive-fixnum))
                         (guess (1+ count)))
                     (prog1 n
                       (setf (get sym 'anon-counter) count))))))
      (guess (or (get sym 'anon-counter) 0)))))

(defmacro with-anon ((&rest vars) &body body)
  `(let ,(mapcar (lambda (var)
                   `(,var (anon ,(symbol-name var))))
                 vars)
     ,@body))
