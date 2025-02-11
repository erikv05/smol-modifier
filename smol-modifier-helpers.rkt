#lang plait

(require "smol-syntax.rkt")
(require [typed-in racket [number->string : (Number -> String)]])
(require [typed-in racket [random : (Number Number -> Number)]])
(require [typed-in racket [display : (Identifier -> Void)]])

;; -----------------------------------------
;; Randomize identifiers + numbers (helper)
;; -----------------------------------------

;; We'll keep two tables for mapping identifiers and numbers to new
;; identifiers and numbers, s.t. all instances of the old identifier
;; or number are replaced by the same new generated instance.
(define identifiers-mapping
  (make-hash (list)))  ; from Identifier -> Identifier

(define numbers-mapping
  (make-hash (list)))  ; from Number -> Number

;; Randomly generates new identifiers (for now, random numbers)
;; and stores in identifiers-mapping
;; In the future, we can randomly match adjectives to nouns, etc.
(define (random-identifier [old : Identifier]) : Identifier
  (if (member old (list '+ '/ '- '* 'eq? '> '>= '< '<= 'not 'and 'or 'if 'begin 'set! 'let 'lambda 'cond
  'else 'case))
      old
      (let ([existing (hash-ref identifiers-mapping old)])
        (type-case (Optionof Identifier) existing
          [(some identifier) identifier]
          [(none)
           (let* ([new-id
                   (string->symbol
                    (string-append "r_"
                                   (number->string (random 0 100))))])
             (begin
               (hash-set! identifiers-mapping old new-id)
               new-id))]))))

;; Randomly generates new numbers from old numbers
;; TODO: preserve division-by-zero cases by checking
;; if second argument is a 0
(define (random-number [old : Number]) : Number
  (if (= old 0)
      old
      (let ([existing (hash-ref numbers-mapping old)])
        (type-case (Optionof Number) existing
          [(some number) number]
          [(none)
           (let* ([new-num (random 0 100)])
             (begin
               (hash-set! numbers-mapping old new-num)
               new-num))]))))

;; -----------------------------------------
;; Randomize constants
;; -----------------------------------------
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

;; -----------------------------------------
;; Randomize expressions
;; -----------------------------------------
(define (randomize-exp [e : Expression]) : Expression
  (type-case Expression e
    [(ECon c)
     (ECon (randomize-constant c))]
    [(Var x)
     (Var (random-identifier x))]
    [(Lambda xs b)
     (Lambda (map random-identifier xs)
             (randomize-body b))]
    [(Let xes b)
     (Let (map (lambda (xe)
                 (pair (random-identifier (fst xe))
                       (randomize-exp (snd xe))))
               xes)
          (randomize-body b))]
    [(Begin es e2)
     (Begin (map randomize-exp es)
            (randomize-exp e2))]
    [(Set! x e2)
     (Set! (random-identifier x) (randomize-exp e2))]
    [(If e-cnd e-thn e-else)
     (If (randomize-exp e-cnd)
         (randomize-exp e-thn)
         (randomize-exp e-else))]
    [(Cond ebs ob)
     (Cond
      (map (lambda (eb)
             (pair (randomize-exp (fst eb))
                   (randomize-body (snd eb))))
           ebs)
      (type-case (Optionof Body) ob
        [(none) (none)]
        [(some b) (some (randomize-body b))]))]
    [(App e-f es)
      (if (member f (list 'eq?)
        (App f es)
        (App f (map randomize-exp es))))]))



;; -----------------------------------------
;; Randomize bodies
;; -----------------------------------------
(define (randomize-body [b : Body]) : Body
  (pair (map randomize-term (fst b))
        (randomize-exp (snd b))))

;; -----------------------------------------
;; Randomize definitions + terms
;; -----------------------------------------
(define (randomize-def [d : Definition]) : Definition
  (type-case Definition d
    [(Defvar x e)
    (Defvar (random-identifier x)
            (randomize-exp e))]
    [(Deffun f xs b)
     (Deffun (random-identifier f)
             (map random-identifier xs)
             (randomize-body b))]))

(define (randomize-term [t : Term]) : Term
  (type-case Term t
    [(definitive d)
     (definitive (randomize-def d))]
    [(expressive e)
     (expressive (randomize-exp e))]))

;; -----------------------------------------
;; Randomize a Program
;; -----------------------------------------
(define (randomize-program [p : Program]) : Program
  (map randomize-term p))