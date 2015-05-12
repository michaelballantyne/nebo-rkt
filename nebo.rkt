#lang racket

(require math/array)

(provide
 constant
 xpos
 ypos
 zpos
 Δx
 Δy
 Δz
 evaluate
 field
 .*
 ./
 .-
 .+
 .sin
 .cos
 .neg
 point
 XFace
 YFace
 ZFace
 Vol
 Any
 unsafe-cast
 Bool
 .if
 .>
 index
 check)

(define (check e type)
  (when (not (eqv? (expr-type e) type)) (error "typecheck failed")))

(define XFace 'XFace)
(define YFace 'YFace)
(define ZFace 'ZFace)
(define Vol 'Vol)
(define Any 'Any)
(define Bool 'Bool)

(struct expr (type fn)
        #:property prop:procedure (struct-field-index fn))

(define Δx (make-parameter (expr Any (lambda (x y z) (error "Δx not defined")))))
(define Δy (make-parameter (expr Any (lambda (x y z) (error "Δy not defined")))))
(define Δz (make-parameter (expr Any (lambda (x y z) (error "Δz not defined")))))

(define (index:center->face index)
  ; The negative face is at the same index as the cell center, so:
  (cond
    [(= 0 index) 0];(error "relative index 0 isn't available for stencils translating between cell centers and faces")]
    ; To get the elements to the left, we subtract normally
    [(< index 0) index]
    ; To get elements to the right, we need to add one less than the count to the right.
    [(> index 0) (- index 1)]))

(define (index:face->center index)
  (- (index:center->face (- index))))

(define (type-join t1 t2)
  (cond
    [(eqv? t1 t2) t1]
    [(any? t1) t2]
    [(any? t2) t1]
    [else (error "can't join types")]))

(define (binop f)
  (lambda (e1 e2)
    (expr (type-join (expr-type e1) (expr-type e2))
          (lambda (x y z)
            (f (e1 x y z) (e2 x y z))))))

(define .+ (binop +))
(define .- (binop -))
(define .* (binop *))
(define ./ (binop /))

(define (unaryop f)
  (lambda (e)
    (expr
      (expr-type e)
      (lambda (x y z)
        (f (e x y z))))))

(define .neg (unaryop -))
(define .sin (unaryop sin))
(define .cos (unaryop cos))

(define (unsafe-cast e type)
  (expr
    type
    (expr-fn e)))

(define (constant val)
  (expr
    Any
    (lambda (x y z)
      val)))

(define (xpos)
  (expr
    Any
    (lambda (x y z)
      x)))

(define (ypos)
  (expr
    Any
    (lambda (x y z)
      y)))

(define (zpos)
  (expr
    Any
    (lambda (x y z)
      z)))

(struct index [x y z])

(define (vol? type)
  (eqv? type Vol))

(define (face? type)
  (or
    (eqv? type XFace)
    (eqv? type YFace)
    (eqv? type ZFace)))

(define (any? type)
  (eqv? type Any))

(define (bool? type)
  (eqv? type Bool))

(define (boolop op)
  (lambda (e1 e2)
    (type-join (expr-type e1) (expr-type e2)) ; errors if not joinable
    (expr
      Bool
      (lambda (x y z)
        (op (e1 x y z) (e2 x y z))))))

(define .> (boolop >))

(define (.if c t e)
  (when (not (bool? (expr-type c))) (error "type error"))
  (expr
    (type-join (expr-type t) (expr-type e))
    (lambda (x y z)
      (if (c x y z)
        (t x y z)
        (e x y z)))))

(define (offset source dest amount face)
  (cond
    ;[(or (any? source) (any? dest)) (error "doesn't make sense to offset single value")]
    [(and (vol? source) (eqv? dest face)) (index:center->face amount)]
    [(and (eqv? source face) (vol? dest)) (index:face->center amount)]
    [else amount]))
    ; Can we offset XFace -> YFace? Doesn't really make sense to me.
    ;[else (error "Not sure how to translate indices")]))

(define (point ReturnType e indices)
  (let ([x-offset (offset (expr-type e) ReturnType (index-x indices) XFace)]
        [y-offset (offset (expr-type e) ReturnType (index-y indices) YFace)]
        [z-offset (offset (expr-type e) ReturnType (index-z indices) ZFace)])
   (expr
    ReturnType
    (lambda (x y z)
      (e (+ x x-offset)
         (+ y y-offset)
         (+ z z-offset))))))

(define (evaluate expr xlen ylen zlen Δx-arg Δy-arg Δz-arg)
  (parameterize ([Δx (lambda (x y z) Δx-arg)]
                 [Δy (lambda (x y z) Δy-arg)]
                 [Δz (lambda (x y z) Δz-arg)])
    (build-array
     (vector zlen ylen xlen)
     (match-lambda
       [(vector z y x)
        (expr x y z)]))))

(define (field arr)
  (lambda (x y z)
    (array-ref arr (vector z y x))))
