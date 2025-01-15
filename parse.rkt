#lang racket

(provide parse-program)

(require "smol-syntax.rkt")
(require (only-in plait some none pair fst snd))

(define (parse-program p)
  (map parse-t p))

(define (constant? c)
  (or (number? c)
      (boolean? c)
      (string? c)))

(define (parse-constant c)
  (match c
    [`,n #:when (number? n) (numeric n)]
    [`,b #:when (boolean? b) (logical b)]
    [`,s #:when (string? s) (textual s)]))

(define (parse-e sexpr)
  (match sexpr
    [`,c #:when (constant? c) (ECon (parse-constant c))]
    [`,x #:when (symbol? x) (Var x)]
    [`(begin ,@es ,e) (Begin (map parse-e es) (parse-e e))]
    [`(set! ,x ,e) (Set! x (parse-e e))]
    [`(if ,e_cnd ,e_thn ,e_els) (If (parse-e e_cnd) (parse-e e_thn) (parse-e e_els))]
    [`(cond ,@ebs [else ,b]) (Cond (map parse-eb ebs) (some (parse-b b)))]
    [`(cond ,@ebs) (Cond (map parse-eb ebs) (none))]
    [`(lambda (,@xs) ,@b) (Lambda xs (parse-b b))]
    [`(let (,@xes) ,@b) (Let (map parse-xe xes) (parse-b b))]
    [`(let* (,@xes) ,@b) (Let* (map parse-xe xes) (parse-b b))]
    [`(letrec (,@xes) ,@b) (Letrec (map parse-xe xes) (parse-b b))]
    [`(,e ,@es) (App (parse-e e) (map parse-e es))]))

; (define (Begin es e)
;   (cond
;     [(empty? es) e]
;     [else (Let (list (pair '|| (first es)) (pair (list) (Begin (rest es) e))))]))

(define (Let* xes b)
  (cond
    [(null? xes) (Let (list) b)]
    [else (Let (list (first xes)) (pair (list) (Let* (rest xes) b)))]))

(define (Letrec xes b)
  (Let (list)
       (pair (append (map xe->definitive xes) (fst b))
             (snd b))))

(define (parse-xe sexpr)
  (match sexpr
    [`[,x ,e] (pair x (parse-e e))]))

(define (xe->definitive xe)
  (definitive (Defvar (fst xe) (snd xe))))

(define (parse-eb sexpr)
  (match sexpr
    [`[,e ,@b] (pair (parse-e e) (parse-b b))]))

(define (parse-b sexpr)
  (match sexpr
    [`(,@ts ,e) (pair (map parse-t ts) (parse-e e))]))

(define (parse-t sexpr)
  (match sexpr
    [`(defvar ,x ,e) (definitive (Defvar x (parse-e e)))]
    [`(deffun (,x ,@xs) ,@b) (definitive (Deffun x xs (parse-b b)))]
    [`,e (expressive (parse-e e))]))
