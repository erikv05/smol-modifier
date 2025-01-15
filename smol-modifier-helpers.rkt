#lang plait

(require "smol-syntax.rkt")
(require [typed-in racket [random : (Number Number -> Number)]])

;; We'll keep two tables for mapping identifiers and numbers to new
;; identifiers and numbers, s.t. all instances of the old identifier
;; or number are replaced by the same new generated instance.

(define identifiers-mapping
  (hash (list)))  ; from s-exp -> s-exp

(define numbers-mapping
  (hash (list)))  ; from Number -> Number

;; Randomly generates new identifiers (for now, random numbers)
;; In the future, we can randomly match adjectives to nouns, etc.
(define (random-identifier old)
  (let ([existing (hash-ref identifiers-mapping old)])
    (if (none? existing)
        existing
        (let* ([new-name
                (string->symbol
                 (string-append "r_"
                                (s-exp->string (number->s-exp (random 0 1000000)))))]
               [new-id new-name])
          (begin
            (hash-set! identifiers-mapping old new-id)
            new-id)))))

;; Randomly generates new numbers from old numbers
;; TODO: Does NOT preserve any arithmetic properties

; (define (random-number old)
;   (let ([existing (hash-ref numbers-mapping old)])
;     (if existing
;         existing
;         (let* ([new-num (random 0 1000000)])
;           (begin
;             (hash-set! numbers-mapping old new-num)
;             new-num)))))