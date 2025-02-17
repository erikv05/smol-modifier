#lang racket

(require "parse.rkt")
(require "smol-syntax.rkt")
(require "smol-modifier-helpers.rkt")
(require (rename-in (only-in "smol-referential.rkt" [evaluate evaluate/smol])))

; (require "smol-modifier-helpers.rkt")

(define p (parse-program '((defvar set-left! (set! z (set! y #t))))))
(randomize-program p) 