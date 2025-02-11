#lang racket

(require "parse.rkt")
(require "smol-modifier-helpers.rkt")

(define p (parse-program '((+ 2 3) (+ 3 4))))
(randomize-program p)