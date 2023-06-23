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

(facts:add ("Blade Runner" :is-a :movie
                           :director "Ridley Scott"
                           :actor "Harison Ford"
                           :actor "Rutger Hauer")
           ("Snow White" :is-a :movie
                         :director "William Cottrell"
                         :director "David Hand"))

(trace with/0 db-each usl-each usl-find)

(with ((?s ?p ?o))
  (format t "~&~S ~S ~S~&" ?s ?p ?o))

facts:*db*

(untrace usl-compare)

(clear-db)
