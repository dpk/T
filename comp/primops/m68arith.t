(herald m68arith
  (env (make-empty-early-binding-locale 'nil) primops))

;;; Copyright (c) 1985 Yale University
;;;     Authors: N Adams, R Kelsey, D Kranz, J Philbin, J Rees.
;;; This material was developed by the T Project at the Yale University Computer 
;;; Science Department.  Permission to copy this software, to redistribute it, 
;;; and to use it for any purpose is granted, subject to the following restric-
;;; tions and understandings.
;;; 1. Any copy made of this software must include this copyright notice in full.
;;; 2. Users of this software agree to make their best efforts (a) to return
;;;    to the T Project at Yale any improvements or extensions that they make,
;;;    so that these may be included in future releases; and (b) to inform
;;;    the T Project of noteworthy uses of this software.
;;; 3. All materials developed as a consequence of the use of this software
;;;    shall duly acknowledge such use, in accordance with the usual standards
;;;    of acknowledging credit in academic research.
;;; 4. Yale has made no warrantee or representation that the operation of
;;;    this software will be error-free, and Yale is under no obligation to
;;;    provide any services, by way of maintenance, update, or otherwise.
;;; 5. In conjunction with products arising from the use of this material,
;;;    there shall be no use of the name of the Yale University nor of any
;;;    adaptation thereof in any advertising, promotional, or sales literature
;;;    without prior written consent from Yale in each case.
;;;

;;; Copyright (c) 1985 David Kranz

(define-constant fixnum-equal?
  (primop fixnum-equal? ()
    ((primop.generate self node)
     (fixnum-comparator node 'jneq))
    ((primop.presimplify self node)
     (presimplify-to-conditional node))
    ((primop.make-closed self)
     (make-closed-conditional self))
    ((primop.conditional? self) t)
    ((primop.conditional-type self node)
     '#[type (proc #f (proc #f) (proc #f) top fixnum fixnum)])
    ((primop.type self node)
     '#[type (proc #f (proc #f boolean) fixnum fixnum)])))

(define-constant fixnum-less?
  (primop fixnum-less? ()
    ((primop.generate self node)
     (fixnum-comparator node 'jgeq))
    ((primop.presimplify self node)
     (presimplify-to-conditional node))
    ((primop.make-closed self)
     (make-closed-conditional self))
    ((primop.conditional? self) t)
    ((primop.conditional-type self node)
     '#[type (proc #f (proc #f) (proc #f) top fixnum fixnum)])
    ((primop.type self node)
     '#[type (proc #f (proc #f boolean) fixnum fixnum)])))

(define-constant char=
  (primop char= ()
    ((primop.generate self node)
     (character-comparator node 'jneq))
    ((primop.presimplify self node)
     (presimplify-to-conditional node))
    ((primop.conditional? self) t)
    ((primop.make-closed self)
     (make-closed-conditional self))
    ((primop.conditional-type self node)
     '#[type (proc #f (proc #f) (proc #f) top char char)])
    ((primop.type self node)
     '#[type (proc #f (proc #f boolean) char char)])))

(define-constant char<
  (primop char< ()
    ((primop.generate self node)
     (character-comparator node 'jgeq))
    ((primop.presimplify self node)
     (presimplify-to-conditional node))
    ((primop.make-closed self)
     (make-closed-conditional self))
    ((primop.conditional? self) t)
    ((primop.conditional-type self node)
     '#[type (proc #f (proc #f) (proc #f) top char char)])
    ((primop.type self node)
     '#[type (proc #f (proc #f boolean) char char)])))

(define-constant char->ascii
  (primop char->ascii ()
    ((primop.generate self node)
     (generate-char->ascii node))
    ((primop.rep-wants self)
     '(rep/char))
    ((primop.arg-specs self)
     '(scratch))
    ((primop.type self node)
     '#[type (proc #f (proc #f fixnum) char)])))

(define-constant ascii->char
  (primop ascii->char ()
    ((primop.generate self node)
     (generate-ascii->char node))
    ((primop.rep-wants self)
     '(rep/integer))
    ((primop.arg-specs self)
     '(scratch))
    ((primop.type self node)
     '#[type (proc #f (proc #f char) fixnum)])))

;;; ARITHMETIC
;;;===========================================================================

(define-constant fixnum-add
  (primop fixnum-add ()
    ((primop.generate self node)
     (generate-fixnum-binop node 'add t nil))
    ((primop.simplify self node)
     (simplify-fixnum-add node))
    ((primop.rep-wants self)
     '(* *))
    ((primop.type self node)
     '#[type (proc #f (proc #f fixnum) fixnum fixnum)])))

(define-constant fixnum-logior
  (primop fixnum-logior ()
    ((primop.generate self node)
     (generate-fixnum-binop node 'or t nil))
    ((primop.simplify self node)
     (simplify-fixnum-logior node))
    ((primop.rep-wants self)
     '(* *))
    ((primop.arg-specs self)
     '(scratch scratch))
    ((primop.type self node)
     '#[type (proc #f (proc #f fixnum) fixnum fixnum)])))

(define-constant fixnum-logxor
  (primop fixnum-logxor ()
    ((primop.generate self node)
     (generate-fixnum-binop node 'xor t nil))
    ((primop.simplify self node)
     (simplify-fixnum-logxor node))
    ((primop.rep-wants self)
     '(* *))
    ((primop.arg-specs self)
     '(scratch scratch))
    ((primop.type self node)
     '#[type (proc #f (proc #f fixnum) fixnum fixnum)])))

(define-constant fixnum-logand
  (primop fixnum-logand ()
    ((primop.generate self node)
     (generate-fixnum-binop node 'and t nil))
    ((primop.simplify self node)
     (simplify-fixnum-logand node))
    ((primop.rep-wants self)
     '(* *))
    ((primop.arg-specs self)
     '(scratch scratch))
    ((primop.type self node)
     '#[type (proc #f (proc #f fixnum) fixnum fixnum)])))

(define-constant (fixnum-lognot x)
   (fixnum-logxor x -1))                                      

(define-constant (fixnum-negate x)
  (fixnum-subtract 0 x))

(define-constant fixnum-subtract
  (primop fixnum-subtract ()
    ((primop.generate self node)
     (generate-fixnum-binop node 'sub nil nil))
    ((primop.rep-wants self)
     '(* *))
    ((primop.simplify self node)
     (simplify-fixnum-subtract node))
    ((primop.type self node)
     '#[type (proc #f (proc #f fixnum) fixnum fixnum)])))

(define-constant (fixnum-ash integer amount)
  (if (fixnum-less? amount 0) 
      (fixnum-ashr integer (fixnum-subtract 0 amount))
      (fixnum-ashl integer amount)))
                                    
(define-constant fixnum-ashl
 (primop fixnum-ashl ()
    ((primop.generate self node)
     (generate-fixnum-binop node 'ashl nil t))
    ((primop.simplify self node)
     (simplify-fixnum-shift node fixnum-ashl))
    ((primop.rep-wants self)
     '(* rep/integer))
    ((primop.arg-specs self)
     '(scratch scratch))
    ((primop.type self node)
     '#[type (proc #f (proc #f fixnum) fixnum fixnum)])))
                                    
(define-constant fixnum-ashr
 (primop fixnum-ashr ()
    ((primop.generate self node)
     (generate-fixnum-binop node 'ashr nil t))
    ((primop.simplify self node)
     (simplify-fixnum-shift node fixnum-ashr))
    ((primop.rep-wants self)
     '(* rep/integer))
    ((primop.arg-specs self)
     '(scratch scratch))
    ((primop.type self node)
     '#[type (proc #f (proc #f fixnum) fixnum fixnum)])))

(define-constant fixnum-add-with-overflow
  (primop fixnum-add-with-overflow ()
    ((primop.values-returned self) 1)                               
    ((primop.generate self node)
     (generate-op-with-overflow node 'add))
    ((primop.presimplify self node)
     (presimplify-to-funny-conditional node 1))
    ((primop.conditional? self) t)
    ((primop.make-closed self) primop/undefined-effect)
    ((primop.conditional-type self node)
     '#[type (proc #f (proc #f fixnum) (proc #f fixnum) top fixnum fixnum)])
    ((primop.type self node)
     '#[type (proc #f (proc #f boolean fixnum) fixnum fixnum)])))

(define-constant fixnum-multiply-with-overflow
  (primop fixnum-multiply-with-overflow ()
    ((primop.values-returned self) 1)                               
    ((primop.generate self node)
     (generate-op-with-overflow node 'multiply))
    ((primop.presimplify self node)
     (presimplify-to-funny-conditional node 1))
    ((primop.conditional? self) t)
    ((primop.make-closed self) primop/undefined-effect)
    ((primop.conditional-type self node)
     '#[type (proc #f (proc #f fixnum) (proc #f fixnum) top fixnum fixnum)])
    ((primop.type self node)
     '#[type (proc #f (proc #f boolean fixnum) fixnum fixnum)])))
      
(define-constant fixnum-subtract-with-overflow
  (primop fixnum-subtract-with-overflow ()
    ((primop.values-returned self) 1)                               
    ((primop.generate self node)
     (generate-op-with-overflow node 'subtract))
    ((primop.presimplify self node)
     (presimplify-to-funny-conditional node 1))
    ((primop.conditional? self) t)
    ((primop.make-closed self) primop/undefined-effect)
    ((primop.conditional-type self node)
     '#[type (proc #f (proc #f fixnum) (proc #f fixnum) top fixnum fixnum)])
    ((primop.type self node)
     '#[type (proc #f (proc #f boolean fixnum) fixnum fixnum)])))
      
(define-constant two-fixnums
  (primop two-fixnums ()
    ((primop.values-returned self) 2)                               
    ((primop.generate self node)
     (generate-two-fixnums node nil))
    ((primop.presimplify self node)
     (presimplify-to-funny-conditional node 2))
    ((primop.conditional? self) t)
    ((primop.make-closed self) primop/undefined-effect)
    ((primop.conditional-type self node)
     '#[type (proc #f (proc #f fixnum fixnum) (proc #f fixnum fixnum) 
          top top top)])
    ((primop.type self node)
     '#[type (proc #f (proc #f boolean fixnum fixnum) top top)])))

(define-constant two-fixnums-for-compare?
  (primop two-fixnums-for-compare? ()
    ((primop.generate self node)
     (generate-two-fixnums node t))
    ((primop.presimplify self node)
     (presimplify-to-conditional node))
    ((primop.make-closed self) primop/undefined-effect)
    ((primop.conditional? self) t)
    ((primop.conditional-type self node)
     '#[type (proc #f (proc #f) (proc #f) top top top)])
    ((primop.type self node)
     '#[type (proc #f (proc #f boolean) top top)])))


(define-constant fixnum-multiply
  (primop fixnum-multiply ()
    ((primop.generate self node)
     (generate-fixnum-multiply node))
    ((primop.simplify self node)
     (simplify-fixnum-multiply node))
    ((primop.rep-wants self) '(rep/integer rep/integer))
    ((primop.arg-specs self)
     '(scratch scratch))
    ((primop.type self node)
     '#[type (proc #f (proc #f fixnum) fixnum fixnum)])))

(define-constant fixnum-divide
  (primop fixnum-divide ()
    ((primop.generate self node)
     (generate-fixnum-divide node))
    ((primop.simplify self node)
     (simplify-fixnum-divide node))
    ((primop.rep-wants self) '(rep/integer rep/integer))
    ((primop.arg-specs self)
     '(scratch scratch))
    ((primop.type self node)
     '#[type (proc #f (proc #f fixnum) fixnum fixnum)])))

(define-constant fixnum-remainder
  (primop fixnum-remainder ()
    ((primop.generate self node)
     (generate-fixnum-remainder node))
    ((primop.rep-wants self) '(rep/integer rep/integer))
    ((primop.arg-specs self)
     '(scratch scratch))
    ((primop.type self node)
     '#[type (proc #f (proc #f fixnum) fixnum fixnum)])))


                                

