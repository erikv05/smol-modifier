#lang plait

(require "smol-syntax.rkt")
(require [typed-in racket [random : (Number Number -> Number)]])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Randomize identifiers + numbers (helper)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; We'll keep two tables for mapping identifiers and numbers to new
;; identifiers and numbers, s.t. all instances of the old identifier
;; or number are replaced by the same new generated instance.
(define identifiers-mapping
  (hash (list)))  ; from Identifier -> Identifier

(define numbers-mapping
  (hash (list)))  ; from Number -> Number

;; Randomly generates new identifiers (for now, random numbers)
;; and stores in identifiers-mapping
;; In the future, we can randomly match adjectives to nouns, etc.
(define (random-identifier [old : Identifier]) : Identifier
  (let ([existing (hash-ref identifiers-mapping old)])
    (type-case (Optionof Identifier) existing
      [(some identifier) identifier]
      [(none)
       (let* ([new-id
           (string->symbol
          (string-append "r_"
                   (s-exp->string (number->s-exp (random 0 1000000)))))])
       (begin
         (hash-set! identifiers-mapping old new-id)
         new-id))])))

;; Randomly generates new numbers from old numbers
;; TODO: preserve division-by-zero cases by checking
;; if second argument is a 0
(define (random-number [old : Number]) : Number
  (let ([existing (hash-ref numbers-mapping old)])
    (type-case (Optionof Number) existing
      [(some number) number]
      [(none)
       (let* ([new-num (random 0 1000000)])
       (begin
         (hash-set! numbers-mapping old new-num)
         new-num))])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Randomize constants
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (randomize-constant [c : Constant]) : Constant
  (type-case Constant c
    [(numeric n)
     (numeric (random-number n))]       ; “scramble” the number
    [(logical b)
      ;; Don't scramble booleans for now
     (logical b)]
    [(textual s)
     ;; TODO: randomize strings in a sound way
     (textual (string-append "rnd_" s))]))