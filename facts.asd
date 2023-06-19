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

(defpackage :facts.system
  (:use :cl :asdf))

(in-package :facts.system)

(defsystem :facts
  :name "facts"
  :author "Thomas de Grivel <thoxdg@gmail.com>"
  :version "0.2"
  :description "in-memory graph database"
  :depends-on ("compare" "lessp" "local-time" "rollback")
  :components
  ((:file "package")
   (:file "fact" :depends-on ("package"))
   (:file "spec" :depends-on ("package"))
   (:file "anon" :depends-on ("package"))
   (:file "transaction" :depends-on ("package"))
   (:file "usl" :depends-on ("fact"))
   (:file "index" :depends-on ("usl"))
   (:file "database" :depends-on ("index" "transaction"))
   (:file "with" :depends-on ("database" "spec" "anon"))
   (:file "serialize" :depends-on ("with"))
   (:file "meta" :depends-on ("with"))))
