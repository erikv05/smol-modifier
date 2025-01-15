#lang racket

(provide (rename-out [my-module-begin #%module-begin]))
(require smol-misinterpreters/parse)

(define-syntax-rule (my-module-begin t ...)
  (#%module-begin
    (define program (parse-program '(t ...)))
    (println "All good!")))
