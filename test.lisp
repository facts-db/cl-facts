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

(untrace with/0 db-each index-each index-find fact/v-subject fact/v-predicate fact/v-object usl-find usl-each make-fact/v)

(with ((?s ?p ?o))
  (format t "~&~S ~S ~S~&" ?s ?p ?o))

(with ((?s ?p ?o)
       (:not ?s :actor "Harison Ford"))
  (format t "~&~S ~S ~S~&" ?s ?p ?o))

facts:*db*

(untrace usl-compare)

(clear-db)

(facts:add (?movie :is-a :movie
                   :title "Blade Runner"
                   :director "Ridley Scott"
                   :actor "Harrison Ford"
                   :actor "Rutger Hauer"))

(facts:add (?movie :is-a :movie
                   :title "Snow White and the Seven Dwarfs"
                   :director "William Cottrell"
                   :director "David Hand"))

(facts:with ((?movie :is-a :movie
                     :title ?title
                     :director ?director))
  (format t "~A directed ~A~%" ?director ?title))
