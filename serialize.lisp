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

(defun save-db (&key into (readably t))
  (etypecase into
    ((or string pathname) (with-open-file (stream into
                                                  :direction :output
                                                  :if-exists :supersede
                                                  :if-does-not-exist :create
                                                  :element-type 'character
                                                  :external-format :utf-8)
                            (save-db :into stream :readably readably)))
    (null (with-output-to-string (stream) (save-db :into stream :readably readably)))
    (stream (let ((*print-readably* readably))
              (format into "(~%")
              (with ((?s ?p ?o))
                (let ((*print-case* :downcase)
                      (*print-pretty* nil))
                  (format into " (~S ~S ~S)~%" ?s ?p ?o))))
            (format into ")~%")
            (force-output into))))

(defun load-db (src)
  (etypecase src
    (string (with-input-from-string (stream src) (load-db stream)))
    (pathname (with-open-file (stream src
                                      :element-type 'character
                                      :external-format :utf-8)
                (load-db stream)))
    (stream (load-db (read src)))
    (list (mapcar (lambda (fact)
                    (apply #'db-insert fact))
                  src))))
