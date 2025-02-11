#lang racket

;; SMOL fuzzer
;; Kartik Chandra, fall 2023

(require (only-in plait some none pair fst snd type-case))
(require "smol-syntax.rkt")

(define (choose xs)
  (list-ref xs (random 0 (length xs))))

(define (make-Identifier!)
  (choose '(x y z a b c d e f g h i j k l m n o p q r s t u v w mpair set-left! set-right!)))

(define (make-Constant!)
  (define z (choose '(logical numeric textual)))
  (cond
    [(equal? z 'logical)
     (logical (choose '(#t #f)))]
    [(equal? z 'numeric)
     (numeric (choose '(0 1 2 3 4 5 6 7 8 9)))]
    [(equal? z 'textual)
     (textual (choose '("JRK" "JBT" "ABC" "DEF" "GHI" "JKL" "MNO" "PQR" "STU" "VWX" "YZ")))]
     ))

(define (make-Term!)
  (define z (choose '(expressive definitive)))
  (cond
    [(equal? z 'expressive)
     (expressive (make-Expression!))]
    [(equal? z 'definitive)
     (definitive (make-Definition!))]
     ))

(define (make-Expression! [n 0])
  (define z
    (if (= n 0)
        (choose '(ECon Var Set! App))
        (choose '(ECon Var Lambda Let Begin Set! If Cond App Case))))
  (cond
    [(equal? z 'ECon)
     (ECon (make-Constant!))]
    [(equal? z 'Var)
     (Var (make-Identifier!))]
    [(equal? z 'Lambda)
     (Lambda (make-Listof! make-Identifier!) (make-Body!))]
    [(equal? z 'Let)
     (Let (make-Listof! (lambda () (pair (make-Identifier!) (make-Expression!)))) (make-Body!))]
    [(equal? z 'Begin)
     (Begin (make-Listof! make-Expression!) (make-Expression!))]
    [(equal? z 'Set!)
     (Set! (make-Identifier!) (make-Expression!))]
    [(equal? z 'If)
     (If (make-Expression!) (make-Expression!) (make-Expression!))]
    [(equal? z 'Cond)
     (Cond (make-Listof! (lambda () (pair (make-Expression!) (make-Body!)))) (none))]
    [(equal? z 'App)
     (App (make-Expression!) (make-Listof! make-Expression!))]
     ))

(define (make-Definition!)
  (define z (choose '(defvar deffun)))
  (cond
    [(equal? z 'defvar)
     (Defvar (make-Identifier!) (make-Expression!))]
    [(equal? z 'deffun)
     (Deffun (make-Identifier!) (make-Listof! make-Identifier!) (make-Body!))]
     ))

(define (make-Listof! t!)
  (define t (t!))
  (if (choose '(#t #t #t #f)) (list t)
      (cons t (make-Listof! t!))))

(define (make-Program!) (make-Listof! make-Term!))
(define (make-Body!) (pair (make-Listof! make-Term!) (make-Expression!)))




(define (constant->string c)
  (type-case Constant c
    [(numeric n) n]
    [(logical b) b]
    [(textual t) t]))
(define (expression->string e)
  (type-case Expression e
    [(ECon c) (constant->string c)]
    [(Var v) v]
    [(Set! n e) `(set! ,n ,(expression->string e))]
    [(App f a) `(,(expression->string f) . ,(map expression->string a))]
    [else '???]))
(define (term->string t)
  (type-case Term t
    [(expressive e) (expression->string e)]
    [(definitive d) (definition->string d)]
    ))
(define (definition->string d)
  (type-case Definition d
    [(Defvar x e) `(defvar ,x ,(expression->string e))]
    [(Deffun f a b) `(deffun ,f ,a ,(map term->string (fst b)) ,(expression->string (snd b)))]
    ))

(define (program->string p)
  (map term->string p))


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

(define (program->result-string ev program)
  (define s
    (with-output-to-string
      (thunk
       (with-handlers ([(lambda (_) #t) (lambda (_) (displayln "error"))])
         (with-limits 1 10
           (ev program))))))
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
;  (cons "Lazy" evaluate/lazy)
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
   ))


(define (displayrow row)
  (displayln (string-join row ",")))

(file-stream-buffer-mode (current-output-port) 'none)
(define (try!)
  (display ".") (flush-output)
  (define program (make-Program!))
  (writeln (program->string program))
  ;(define program (list (expressive (Var 'z))))
  (define wanted-result (program->result-string evaluate/smol program))
  (define results
    (for/list ([nm-and-eval mises])
      (match-define (cons nm ev) nm-and-eval)
      (define actual-result (program->result-string ev program))
      (cons nm actual-result)))
  (define hits
    (filter
      (lambda (x)
        (and
          ;(member (car x) '("DefOrSet" "FunNotVal" "NestedDef" "CallByRef" "DefByRef" "StructByRef"))
          (not (equal? (cdr x) wanted-result)))) results))
  (if (empty? hits) (try!)
      (begin
        (newline)
        (writeln (program->string program))
        (writeln wanted-result)
        (writeln hits)))
  )

(try!)