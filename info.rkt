#lang info
(define collection "neologia")
(define deps '("base"
               "rackunit-lib"
               "brag"
               "beautiful-racket"
               "beautiful-racket-lib"
               "br-parser-tools-lib"))
(define build-deps '("scribble-lib" "racket-doc"))
(define scribblings '(("scribblings/neologia.scrbl" ())))
(define pkg-desc "A language for creating and managing dictionaries and lexicons")
(define version "0.9")
(define pkg-authors '(robkleffner))
