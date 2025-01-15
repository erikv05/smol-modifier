#lang racket

(require racket/sandbox)
(require csv-reading)
(require (only-in smol-misinterpreters/parse parse-program))
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
(require (rename-in (only-in smol-misinterpreters/smol-no-lazy-eval evaluate) [evaluate evaluate/no-lazy-eval]))


(define (string->terms str)
  (with-input-from-string str
    (thunk
     (let loop ([ts empty])
       (let ([t (read)])
         (if (eof-object? t)
             (reverse ts)
             (loop (cons t ts))))))))

(define cached-parse
  (let ([cache (make-hash)])
    (lambda (program)
      (hash-ref cache program
                (thunk (parse-program (string->terms program)))))))

(define (program->result-string ev program)
  (define s
    (with-output-to-string
      (thunk
       (with-handlers ([(lambda (_) #t) (lambda (_) (displayln "error"))])
         (with-limits 1 10
           (ev (cached-parse program)))))))
  (let* ([s (if (string-suffix? s "\n")
                (substring s 0 (sub1 (string-length s)))
                s)]
         [s (string-replace s "\n" " ")])
    s))

(define mises
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
   (cons "NoLazyEval" evaluate/no-lazy-eval)
   ))

(define (displayrow row)
  (displayln (string-join row ",")))

(define program (port->string (current-input-port)))
(define wanted-result (program->result-string evaluate/smol program))
(displayrow (list "Misinterpreter" "Result"))
(for ([nm-and-eval mises])
  (match-define (cons nm ev) nm-and-eval)
  (unless (and (equal? nm "Lazy")
               (string-contains? program "!"))
    (define actual-result (program->result-string ev program))
    (unless (equal? wanted-result actual-result)
      (displayrow (list nm actual-result)))))
