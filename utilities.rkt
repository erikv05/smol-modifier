#lang racket

(provide (all-defined-out))

(define (boolean->string b)
  (if b "#t" "#f"))

(define (memq? x xs)
  (if (memq x xs)
      #t
      #f))

(define (has-duplicates? ls)
  (if (check-duplicates ls) #t #f))

(define (spy msg x)
  (displayln msg)
  (displayln x)
  x)