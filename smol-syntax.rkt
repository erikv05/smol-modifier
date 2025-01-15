#lang plait


(define-type-alias (Pairof 'a 'b) ('a * 'b))

(define-type-alias Identifier Symbol)
(define-type Constant
  ; logical
  (logical [l : Boolean])
  ; numeric
  (numeric [n : Number])
  ; textual
  (textual [s : String]))

(define-type Term
  (expressive [e : Expression])
  (definitive [d : Definition]))

(define-type Expression
  (ECon [c : Constant])
  (Var [x : Identifier])
  (Lambda [xs : (Listof Identifier)] [body : Body])
  (Let [xes : (Listof (Pairof Identifier Expression))] [body : Body])
  (Begin [es : (Listof Expression)] [e : Expression])
  (Set! [x : Identifier] [e : Expression])
  (If [e_cnd : Expression] [e_thn : Expression] [e_els : Expression])
  (Cond [ebs : (Listof (Pairof Expression Body))] [ob : (Optionof Body)])
  (App [e : Expression] [es : (Listof Expression)]))

(define-type Definition
  (Defvar [x : Identifier] [e : Expression])
  (Deffun [f : Identifier] [xs : (Listof Identifier)] [b : Body]))

(define-type-alias Body (Pairof (Listof Term) Expression))

(define-type-alias Program (Listof Term))

(define-type PrimitiveOperator
  (Add)
  (Sub)
  (Mul)
  (Div)
  (Eq)
  (Lt)
  (Gt)
  (Le)
  (Ge)
  (VecNew)
  (VecLen)
  (VecRef)
  (VecSet)
  (PairNew)
  (PairLft)
  (PairRht)
  (PairSetLft)
  (PairSetRht)
  )

(define (make-the-primordial-env load)
  (list
   (list
    (pair '+ (load (Add)))
    (pair '- (load (Sub)))
    (pair '* (load (Mul)))
    (pair '/ (load (Div)))
    (pair '= (load (Eq)))
    (pair 'eq? (load (Eq)))
    (pair 'equal? (load (Eq)))
    (pair '< (load (Lt)))
    (pair '> (load (Gt)))
    (pair '<= (load (Le)))
    (pair '>= (load (Ge)))
    (pair 'mvec (load (VecNew)))
    (pair 'vec-len (load (VecLen)))
    (pair 'vec-ref (load (VecRef)))
    (pair 'vec-set! (load (VecSet)))
    (pair 'vlen (load (VecLen)))
    (pair 'vref (load (VecRef)))
    (pair 'vset! (load (VecSet)))
    (pair 'pair (load (PairNew)))
    (pair 'mpair (load (PairNew)))
    (pair 'ipair (load (PairNew)))
    (pair 'left (load (PairLft)))
    (pair 'right (load (PairRht)))
    (pair 'set-left! (load (PairSetLft)))
    (pair 'set-right! (load (PairSetRht)))
)))
