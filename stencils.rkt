#lang racket

(require "nebo.rkt")

(provide (all-defined-out))

(define (upwindx f c)
  (.if (.> (check c XFace) (constant 0))
    (point XFace f [index -1 0 0])
    (point XFace f [index 1 0 0])))

(define (interpx Type e)
  (./ (.+ (point Type e [index -1 0 0]) (point Type e [index 1 0 0]))
     (constant 2)))

(define (divx e)
  (./ (.- (point XFace e [index 1 0 0]) (point XFace e [index -1 0 0]))
     Δy))

(define (gradx e)
  (./ (.- (point Vol e [index 1 0 0]) (point Vol e [index -1 0 0]))
     Δy))

