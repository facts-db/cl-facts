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

(in-package :common-lisp-user)

(defpackage :facts
  (:use :cl :lessp :local-time :rollback)
  (:export #:anon #:with-anon
           #:with #:bound-p #:collect #:first-bound #:let-with
           #:without
           #:add #:add* #:rm
           #:db #:*db* #:*db-path* #:*db-log-path-defaults*
           #:clear-db #:db-path #:db-log-path #:save-db #:load-db
           #:transaction-var #:with-transaction
           #:binding-p #:collect-bindings #:sort-bindings))
