#lang plait

(define m-ht (make-hash (list (pair 1 "apple") (pair 2 "banana"))))
(let ([x (hash-ref m-ht 3)])
(if (none? x)
        x
        (none)))