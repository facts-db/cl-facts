facts-db/cl-facts
=================

> 1.1 The world is the totality of facts, not of things.
>
> --  Ludwig Wittgenstein, Tractatus Logico Philosophicus

cl-facts is a small in-memory graph database for Common Lisp.
It features :
* a triple store based on unlabelled skip lists
* a very simple query language (facts:with)
* transactions using rollback functions
* logging and replay of transactions to/from disk
* dumping and loading the database to/from disk


Requirements
------------

You will need :
* https://git.kmx.io/facts-db/cl-compare
* https://git.kmx.io/facts-db/cl-rollback


Usage
-----

### FACTS:ADD &rest SPECS
Adds facts (triples) to the database. Triples can be grouped by subject.

```common-lisp
(facts:add ("Blade Runner" :is-a :movie
                           :director "Ridley Scott"
                           :actor "Harison Ford"
                           :actor "Rutger Hauer")
           ("Snow White" :is-a :movie
                         :director "William Cottrell"
                         :director "David Hand"))
```

```common-lisp
(facts:add (?movie :is-a :movie
                   :title "Blade Runner"
                   :director "Ridley Scott"
                   :actor "Harrison Ford"
                   :actor "Rutger Hauer"))

(facts:add (?movie :is-a :movie
                   :title "Snow White and the Seven Dwarfs"
                   :director "William Cottrell"
                   :director "David Hand"))
```

The second version with `?movie` will generate an anonymous symbol prefixed with `movie-`.
It is considered a more clean and efficient way to abstract identifiers.


### FACTS:RM &rest SPECS

```common-lisp
(facts:rm (?movie :actor "Harison Ford"))
```


### FACTS:WITH SPECS &body BODY

To follow Wittgenstein's view of the world, all queries get turned into
testing the presence or absence of triples (facts).

Variables are prefixed with a question mark symbol "?" and are
wildcards, matching everything. Nested queries get their variables
expanded, giving pattern matching abilities. For instance :

```common-lisp
(with ((?s ?p ?o)
  (format t "~&~S ~S ~S~&" ?s ?p ?o))

=>
"Blade Runner" :ACTOR "Harison Ford"
"Blade Runner" :ACTOR "Rutger Hauer"
"Blade Runner" :DIRECTOR "Ridley Scott"
"Blade Runner" :IS-A :MOVIE
"Snow White" :DIRECTOR "David Hand"
"Snow White" :DIRECTOR "William Cottrell"
"Snow White" :IS-A :MOVIE
```

Multiple queries on the same subject can be grouped together easily :

```common-lisp
(facts:with ((?movie :is-a :movie
                     :title ?title
                     :director ?director))
  (format t "~A directed ~A~%" ?director ?title))
```

Negative facts specifications will remove matching facts from the
results.


```common-lisp
(with ((?s ?p ?o)
       (:not ?s :actor "Harison Ford"))
  (format t "~&~S ~S ~S~&" ?s ?p ?o))

=>
"Snow White" :DIRECTOR "David Hand"
"Snow White" :DIRECTOR "William Cottrell"
"Snow White" :IS-A :MOVIE
```


TODO
----

 - binding of negation : :not resulting from another binding
 - barriers / hooks : functions that will be called when a spec is added
   or removed.

```

### FACTS:\*DB\*
The current facts database.

### FACTS:CLEAR-DB
Clears the database from every facts.

### FACTS:SAVE-DB &key INTO (READABLY T)
Dump the database facts into filespec INTO.

### FACTS:LOAD-DB SRC
Load the facts from SRC into \*db\*.

### FACTS:WITH-TRANSACTION &body BODY
Enclose BODY database operations into a transaction.

A transaction ensures that all database operations will succeed or be
reverted using their respective rollback functions.

Transactions can be nested safely.
