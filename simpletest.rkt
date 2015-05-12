#lang racket

(require "nebo.rkt")
(require "stencils.rkt")

(define a
  (evaluate
   (zpos)
   3 3 3
   1 1 1))

(evaluate
 (field a)
 3 3 3
 1 1 1)

(evaluate
   (.sin (xpos))
   4 4 4
   1 1 1)

(evaluate
   (.+ (constant 1) (interpx Vol (.sin (unsafe-cast (xpos) Vol))))
   4 4 4
   1 1 1)

(evaluate
   (xpos)
   4 4 4
   1 1 1)

(evaluate
   (upwindx (unsafe-cast (xpos) XFace) (unsafe-cast (xpos) XFace))
   4 4 4
   1 1 1)
