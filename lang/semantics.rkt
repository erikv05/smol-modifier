#lang racket
(require racket/sandbox)

(provide
 (rename-out [my-module-begin #%module-begin])
 evaluate)
(require smol-misinterpreters/parse)

(define-syntax-rule (my-module-begin t ...)
  (#%module-begin
   (define program '(t ...))
   (present-results (evaluate program))))

(require (rename-in (only-in smol-misinterpreters/smol-referential evaluate) [evaluate evaluate/smol]))
(require (rename-in (only-in smol-misinterpreters/smol-nested-def evaluate) [evaluate evaluate/nested-def]))
(require (rename-in (only-in smol-misinterpreters/smol-isolated-fun evaluate) [evaluate evaluate/isolated-fun]))
(require (rename-in (only-in smol-misinterpreters/smol-lazy evaluate) [evaluate evaluate/lazy]))
(require (rename-in (only-in smol-misinterpreters/smol-flat-env evaluate) [evaluate evaluate/flat-env]))
(require (rename-in (only-in smol-misinterpreters/smol-call-by-ref evaluate) [evaluate evaluate/call-by-ref]))
(require (rename-in (only-in smol-misinterpreters/smol-def-by-ref evaluate) [evaluate evaluate/def-by-ref]))
(require (rename-in (only-in smol-misinterpreters/smol-struct-by-ref evaluate) [evaluate evaluate/struct-by-ref]))
(require (rename-in (only-in smol-misinterpreters/smol-deep-closure evaluate) [evaluate evaluate/deep-closure]))
(require (rename-in (only-in smol-misinterpreters/smol-defs-copy-structs evaluate) [evaluate evaluate/defs-copy-structs]))
(require (rename-in (only-in smol-misinterpreters/smol-calls-copy-structs evaluate) [evaluate evaluate/calls-copy-structs]))
(require (rename-in (only-in smol-misinterpreters/smol-structs-copy-structs evaluate) [evaluate evaluate/structs-copy-structs]))
(require (rename-in (only-in smol-misinterpreters/smol-fun-not-val evaluate) [evaluate evaluate/fun-not-val]))
(require (rename-in (only-in smol-misinterpreters/smol-no-circularity evaluate) [evaluate evaluate/no-circularity]))
(require (rename-in (only-in smol-misinterpreters/smol-def-or-set evaluate) [evaluate evaluate/def-or-set]))
(require (rename-in (only-in smol-misinterpreters/smol-heapless evaluate) [evaluate evaluate/heapless]))
(require (rename-in (only-in smol-misinterpreters/smol-heapless-def-by-ref evaluate) [evaluate evaluate/heapless-def-by-ref]))
(require (rename-in (only-in smol-misinterpreters/smol-heapless-call-by-ref evaluate) [evaluate evaluate/heapless-call-by-ref]))
(require (rename-in (only-in smol-misinterpreters/smol-eager-thunk evaluate) [evaluate evaluate/eager-thunk]))

(define (program->result-string ev program)
  (define s
    (with-output-to-string
      (thunk
       (with-handlers (
                       [(lambda (_) #t) (lambda (_) (displayln "error"))]
                       )
         (with-limits 1 10
           (ev program))))))
  (let* ([s (if (string-suffix? s "\n")
                (substring s 0 (sub1 (string-length s)))
                s)]
         [s (string-replace s "\n" " ")])
    s))

(define misc-and-evals
  (make-hash
   (list
    (cons "DefOrSet" evaluate/def-or-set)
    (cons "NestedDef" evaluate/nested-def)
    (cons "IsolatedFun" evaluate/isolated-fun)
    (cons "Lazy" evaluate/lazy)
    (cons "FlatEnv" evaluate/flat-env)
    (cons "CallByRef" evaluate/call-by-ref)
    (cons "DefByRef" evaluate/def-by-ref)
    (cons "StructByRef" evaluate/struct-by-ref)
    (cons "DeepClosure" evaluate/deep-closure)
    (cons "DefsCopyStructs" evaluate/defs-copy-structs)
    (cons "CallsCopyStructs" evaluate/calls-copy-structs)
    (cons "StructsCopyStructs" evaluate/structs-copy-structs)
    (cons "FunNotVal" evaluate/fun-not-val)
    (cons "NoCircularity" evaluate/no-circularity)
    ; (cons "EagerThunk" evaluate/eager-thunk)
    ; (cons "HeapLess" evaluate/heapless)
    ; (cons "HeapLess&DefByRef" evaluate/heapless-def-by-ref)
    ; (cons "HeapLess&CallByRef" evaluate/heapless-call-by-ref)
    )))

(define (evaluate program-src)
  (define program (parse-program program-src))
  (define smol-result (program->result-string evaluate/smol program))
  (cons
   smol-result
   (for/hash ([(misc eval) misc-and-evals]
              #:unless (and (equal? misc "Lazy")
                            (string-contains? (format "~a" program) "!")))
     (define result (program->result-string eval program))
     (values misc result))))

(define (present-results results)
  ; (writeln results)
  (match-define (cons smol-result misc-and-results) results)
  (displayln smol-result)
  (for ([(misc result) misc-and-results])
    (unless (equal? smol-result result)
      (displayln (format "~a (~a)" result misc)))))