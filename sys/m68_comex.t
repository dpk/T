(herald m68_comex (env tsys))

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
                             
(define-constant vector->unit!
  (primop vector->unit! ()
    ((primop.generate self node)
     (emit m68/move .b (machine-num header/unit)
           (reg-offset (->register 'pointer node (leaf-value ((call-arg 2) node)) '*)
                       1)))
    ((primop.side-effects? self) t)
    ((primop.type self node)
     '#[type (proc #f (proc #f unit) vector)])))

(define-integrable (install-template1 unit code obj i)
  (let ((tp (fx+ (fx* i 4) 6)))
    (set (bref-16 unit (fx- tp 4)) (fx+ tp 2))   ; offset in closure
    (set (bref unit (fx- tp 5)) (bref code (fx- obj 5))) ; scratch
    (set (bref unit (fx- tp 6)) (bref code (fx- obj 6))) ; pointer
    (set (bref unit (fx- tp 2)) (bref code (fx- obj 2))) ; header
    (set (bref unit (fx- tp 1)) (bref code (fx- obj 1))) ; nargs
    (set (bref-16 unit tp) M68-JUMP-ABSOLUTE)            ; jump absolute
    ;++  flush (set (bref-16 unit tp) #x4EF9)
    (set (extend-pointer-elt unit (fx+ i 2))             ; auxiliary template
         (make-pointer code (fixnum-ashr obj 2)))))
