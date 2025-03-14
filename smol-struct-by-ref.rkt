#lang plait
(require (typed-in racket/list
                   [append-map : (('a -> (Listof 'b)) (Listof 'a) -> (Listof 'b))]))
(require (typed-in racket
                   [hash-count : ((Hashof 'a 'b) -> Number)]
                   [hash-values : ((Hashof 'a 'b) -> (Listof 'b))]
                   [count : (('a -> Boolean) (Listof 'a) -> Number)]
                   [for-each : (('a -> 'b) (Listof 'a) -> Void)]
                   [string-join : ((Listof String) String -> String)]
                   [displayln : ('a -> Void)]
                   [list->vector : ((Listof 'a) -> (Vectorof 'a))]
                   [vector->list : ((Vectorof 'a) -> (Listof 'a))]
                   [number->string : (Number -> String)]))
(require (typed-in "utilities.rkt"
                   [boolean->string : (Boolean -> String)]
                   [has-duplicates? : ((Listof 'a) -> Boolean)]))
(require "smol-syntax.rkt")
(require (typed-in "parse.rkt"
                   [parse-program : ('a -> Program)]))

(define-syntax begin
  (syntax-rules ()
    [(begin e) e]
    [(begin e1 e2 ... en)
     (local [(define _ : Void e1)]
       (begin e2 ... en))]))

(define-syntax for
  (syntax-rules ()
    [(for ([x xs]) body ...)
     (for-each (lambda (x) (begin body ...)) xs)]))

;; --- runtime ---

(define-type Tag
  (TNum)
  (TStr)
  (TLgc)
  (TFun)
  (TVec))

(define-type Value
  (unit)
  (embedded [c : Constant])
  (primitive [o : PrimitiveOperator])
  (function [xs : (Listof Identifier)] [body : Body] [env : Environment])
  (vector [vs : (Vectorof (Boxof Value))]))

(define (value-eq? v1 v2)
  (cond
    [(and (unit? v1) (unit? v2)) #t]
    [(and (embedded? v1) (embedded? v2)) (equal? v1 v2)]
    [(and (primitive? v1) (primitive? v2)) (equal? v1 v2)]
    [else (eq? v1 v2)]))

(define (as-boolean v) : Boolean
  (type-case Value (unbox v)
    [(embedded c)
     (type-case Constant c
       [(logical b) b]
       [else (error 'smol "expecting a boolean")])]
    [else
     (error 'smol "expecting a boolean")]))
(define (from-logical [v : Boolean]) : Value
  (embedded (logical v)))

(define (as-numeric v) : Number
  (type-case Value (unbox v)
    [(embedded c)
     (type-case Constant c
       [(numeric n) n]
       [else (error 'smol "expecting a number")])]
    [else
     (error 'smol "expecting a number")]))
(define (from-numeric [n : Number]) : Value
  (embedded (numeric n)))

(define (as-vector v) : (Vectorof (Boxof Value))
  (type-case Value (unbox v)
    [(vector v)
     v]
    [else
     (error 'smol "expecting a vector")]))
(define (from-vector [n : (Vectorof (Boxof Value))]) : Value
  (vector n))
(define (as-pair v) : (Vectorof (Boxof Value))
  (let ([v (as-vector v)])
    (if (= (vector-length v) 2)
        v
        (error 'smol "expecting a pair"))))

(define (as-one vs) : (Boxof Value)
  (cond
    [(= (length vs) 1)
     (first vs)]
    [else
     (error 'smol "arity-mismatch, expecting one")]))
(define (as-two vs) : ((Boxof Value) * (Boxof Value))
  (cond
    [(= (length vs) 2)
     (values (first vs) (first (rest vs)))]
    [else
     (error 'smol "arity-mismatch, expecting two")]))
(define (as-three vs) : ((Boxof Value) * (Boxof Value) * (Boxof Value))
  (cond
    [(= (length vs) 3)
     (values (first vs) (first (rest vs)) (first (rest (rest vs))))]
    [else
     (error 'smol "arity-mismatch, expecting three")]))

(define-type-alias EnvironmentFrame (Listof (Pairof Identifier (Boxof (Optionof (Boxof Value))))))
(define-type-alias Environment (Listof EnvironmentFrame))

(define (load v)
  (box (some (box (primitive v)))))
(define the-primordial-env
  (make-the-primordial-env load))

(define (cmp f vs) (from-logical (cmp-helper f vs)))
(define (cmp-helper f vs) : Boolean
  (cond
    [(empty? vs) #t]
    [else
     (local ((define (rec v vs)
                 (cond
                   [(empty? vs) #t]
                   [else
                    (and (f v (first vs))
                         (rec (first vs) (rest vs)))])))
       (rec (first vs) (rest vs)))]))

(define (delta [p : PrimitiveOperator] [vs : (Listof (Boxof Value))]) : Value
  (type-case PrimitiveOperator p
    [(Add) (from-numeric (foldl (lambda (m n) (+ m n)) 0 (map as-numeric vs)))]
    [(Sub)
     (let ([vs (map as-numeric vs)])
       (from-numeric
        (foldl (lambda (m n) (- n m))
               (first vs)
               (rest vs))))]
    [(Mul) (from-numeric (foldl (lambda (m n) (* m n)) 1 (map as-numeric vs)))]
    [(Div)
     (let ([vs (map as-numeric vs)])
       (from-numeric
        (foldl (lambda (m n) (/ n m))
               (first vs)
               (rest vs))))]
    [(Eq) (cmp (lambda (a b) (value-eq? a b)) (map unbox vs))]
    [(Lt) (cmp (lambda (a b) (< (as-numeric a) (as-numeric b))) vs)]
    [(Gt) (cmp (lambda (a b) (> (as-numeric a) (as-numeric b))) vs)]
    [(Le) (cmp (lambda (a b) (<= (as-numeric a) (as-numeric b))) vs)]
    [(Ge) (cmp (lambda (a b) (>= (as-numeric a) (as-numeric b))) vs)]
    [(VecNew) (vector (list->vector vs))]
    [(VecLen)
     (local ((define v (as-one vs)))
       (from-numeric (vector-length (as-vector v))))]
    [(VecRef)
     (local ((define-values (v1 v2) (as-two vs))
             (define vvec (as-vector v1))
             (define vnum (as-numeric v2)))
       (unbox (vector-ref vvec vnum)))]
    [(VecSet)
     (local ((define-values (v1 v2 v3) (as-three vs))
             (define vvec (as-vector v1))
             (define vnum (as-numeric v2)))
       (begin
         (vector-set! vvec vnum v3)
         (unit)))]
    [(PairNew)
     (local ((define-values (v1 v2) (as-two vs)))
       (from-vector (list->vector vs)))]
    [(PairLft)
     (local ((define v (as-one vs)))
       (unbox (vector-ref (as-pair v) 0)))]
    [(PairRht)
     (local ((define v (as-one vs)))
       (unbox (vector-ref (as-pair v) 1)))]
    [(PairSetLft)
     (local ((define-values (vpr vel) (as-two vs)))
       (begin
         (vector-set! (as-pair vpr) 0 vel)
         (unit)))]
    [(PairSetRht)
     (local ((define-values (vpr vel) (as-two vs)))
       (begin
         (vector-set! (as-pair vpr) 1 vel)
         (unit)))]))

(define (env-declare [env : Environment] xs)
  (begin
    (when (has-duplicates? xs)
      (error 'smol "can't define a variable twice"))
    (local [(define (allocate x)
              (pair x (box (none))))]
      (cons (map allocate xs) env))))
(define (env-frame-lookup f x)
  (type-case EnvironmentFrame f
    [empty
     (none)]
    [(cons xv xvs)
     (if (eq? (fst xv) x)
         (some (snd xv))
         (env-frame-lookup xvs x))]))
(define (env-lookup-location [env : Environment] x)
  : (Boxof (Optionof (Boxof Value)))
  (type-case Environment env
    [empty
     (error 'smol "variable undeclared")]
    [(cons f fs)
     (type-case (Optionof (Boxof (Optionof (Boxof Value)))) (env-frame-lookup f x)
       [(none) (env-lookup-location fs x)]
       [(some loc) loc])]))
(define (env-init! [env : Environment])
  : (Identifier (Boxof Value) -> Void)
  (lambda (x [v : (Boxof Value)])
    (let ([loc (env-lookup-location env x)])
      (begin
        (set-box! loc (some v))
        (void)))))
(define (env-update! [env : Environment])
  : (Identifier Value -> Void)
  (lambda (x [v : Value])
    (set-box! (env-lookup env x) v)))
(define (env-lookup [env : Environment] x) : (Boxof Value)
  (let ([v (env-lookup-location env x)])
    (type-case (Optionof (Boxof Value)) (unbox v)
      [(none) (error 'smol "refer to a variable before assign it a value")]
      [(some v) v])))

(define (declared-identifiers [ts : (Listof Term)])
  (local [(define (xs-of-t [t : Term])
            : (Listof Identifier)
            (type-case Term t
              [(expressive e) (list)]
              [(definitive d)
               (type-case Definition d
                 [(Defvar x e) (list x)]
                 [(Deffun f xs b) (list f)])]))]
    (append-map xs-of-t ts)))

(define (eval-def [env : Environment] d)
  (type-case Definition d
    [(Defvar x e)
     (let ([v ((eval-exp env) e)])
       (let ([_ ((env-init! env) x (box v))])
         (void)))]
    [(Deffun f xs b)
     (let ([_ ((env-init! env) f (box (function xs b env)))])
       (void))]))

(define (eval-body [env : Environment] [xvs : (Listof (Pairof Identifier (Boxof Value)))] b)
  : Value
  (local [(define vs (map snd xvs))
          (define xs
            (append (map fst xvs)
                    (declared-identifiers (fst b))))
          (define new-env (env-declare env xs))]
    (begin
      ; bind arguments
      (for ([xv xvs])
        ((env-init! new-env) (fst xv) (snd xv)))
      ; evaluate starting terms
      (for ([t (fst b)])
        ((eval-term new-env) t))
      ; evaluate and return the result
      ((eval-exp new-env) (snd b)))))

(define (eval-arg [env : Environment])
  (lambda (e)
    (type-case Expression e
      [(Var x) (env-lookup env x)]
      [else (box ((eval-exp env) e))])))

(define (eval-exp [env : Environment])
  : (Expression -> Value)
  (lambda ([e : Expression])
    : Value
    (type-case Expression e
      [(ECon c) (embedded c)]
      [(Var x) (unbox (env-lookup env x))]
      [(Lambda xs b) (function xs b env)]
      [(Let xes b)
       (local [(define (ev-bind xv)
                 (let ([v ((eval-exp env) (snd xv))])
                   (pair (fst xv) (box v))))]
         (let ([xvs (map ev-bind xes)])
           (eval-body env xvs b)))]
      [(Begin es e)
       (let ([_ (map (eval-exp env) es)])
         ((eval-exp env) e))]
      [(Set! x e)
       (let ([v ((eval-exp env) e)])
         (let ([_ ((env-update! env) x v)])
           (unit)))]
      [(If e_cnd e_thn e_els)
       (let ([v ((eval-arg env) e_cnd)])
         (let ([l (as-boolean v)])
           ((eval-exp env)
            (if l e_thn e_els))))]
      [(Cond ebs ob)
       (local [(define (loop ebs) : Value
                 (type-case (Listof (Pairof Expression Body)) ebs
                   [empty
                    (type-case (Optionof Body) ob
                      [(none) (error 'smol "fall throught cond")]
                      [(some b)
                       (eval-body env (list) b)])]
                   [(cons eb ebs)
                    (let ([v ((eval-arg env) (fst eb))])
                      (let ([l (as-boolean v)])
                        (if l
                            (eval-body env (list) (snd eb))
                            (loop ebs))))]))]
         (loop ebs))]
      [(App e es)
       (let ([v ((eval-exp env) e)])
         (let ([vs (map (eval-arg env) es)])
           (type-case Value v
             [(function xs b env)
              (if (= (length xs) (length vs))
                  (eval-body env (map2 pair xs (map box (map unbox vs))) b)
                  (error 'smol "(arity-mismatch (length xs) (length vs))"))]
             [(primitive p)
              (delta p vs)]
             [else
              (error 'smol "(type-mismatch (TFun) v)")])))])))

(define (eval-term [env : Environment])
  (lambda (t)
    (type-case Term t
      [(definitive d)
       (let ([_ (eval-def env d)])
         (unit))]
      [(expressive e)
       ((eval-exp env) e)])))

(define (constant->string [c : Constant])
  (type-case Constant c
    [(logical l) (boolean->string l)]
    [(numeric n) (number->string n)]
    [(textual s) s]))

(define (self-ref [i : Number]) : String
  (foldr string-append
         ""
         (list "#" (number->string i) "#")))
(define (self-def [i : Number]) : String
  (foldr string-append
         ""
         (list "#" (number->string i) "=")))

(define (value->string [visited-vs : (Hashof (Vectorof (Boxof Value)) (Boxof (Optionof Number)))])
  (lambda ([v : Value]) : String
    (type-case Value v
      [(unit) "#<void>"]
      [(embedded c) (constant->string c)]
      [(primitive o) "#<procedure>"]
      [(function xs body env) "#<procedure>"]
      [(vector vs)
       (type-case (Optionof (Boxof (Optionof Number))) (hash-ref visited-vs vs)
         [(none)
          (let ([visited-vs (hash-set visited-vs vs (box (none)))])
            (let* ([s (foldr string-append
                             ""
                             (list
                              "#("
                              (string-join
                               (map (value->string visited-vs)
                                    (map unbox (vector->list vs)))
                               " ")
                              ")"))]
                   [boi (some-v (hash-ref visited-vs vs))])
              (type-case (Optionof Number) (unbox boi)
                [(none) s]
                [(some i) (string-append (self-def i) s)])))]
         [(some boi)
          (type-case (Optionof Number) (unbox boi)
            [(none)
             (let ([i (count some? (map unbox (hash-values visited-vs)))])
               (begin
                 (set-box! (some-v (hash-ref visited-vs vs)) (some i))
                 (self-ref i)))]
            [(some i)
             (self-ref i)])
          ])])))
(define (print-value [v : Value])
  (type-case Value v
    [(unit) (void)]
    [else (displayln ((value->string (hash (list))) v))]))

(define (exercute-terms-top-level [env : Environment])
  (lambda (ts) : Void
    (type-case (Listof Term) ts
      [empty (void)]
      [(cons t ts)
       (type-case Term t
         [(definitive d)
          (begin
            (eval-def env d)
            ((exercute-terms-top-level env) ts))]
         [(expressive e)
          (begin
            (print-value ((eval-exp env) e))
            ((exercute-terms-top-level env) ts))])])))

(define (evaluate [p : Program])
  (local [(define xs (declared-identifiers p))
          (define the-top-level-env (env-declare the-primordial-env xs))]
    ((exercute-terms-top-level the-top-level-env) p)))
