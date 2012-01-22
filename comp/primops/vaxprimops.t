(herald vaxprimops
        (env (make-empty-early-binding-locale 'nil) constants))

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

(define-constant call-foreign 
  (primop call-foreign ()
    ((primop.make-closed self)
     '(lambda args (error "DEFINE-FOREIGN cannot be interpreted")))
    ((primop.generate self node)
     (generate-foreign-call node))))


;;; The PRIMOP.TYPE values are suggestions, none of them are currently used.

;;; COMPARATORS
;;;===========================================================================

(define-constant eq?
  (primop eq? ()
    ((primop.generate self node)
     (eq?-comparator node))
    ((primop.presimplify self node)
     (presimplify-to-conditional node))
    ((primop.make-closed self)
     (make-closed-conditional self))
    ((primop.conditional? self) t)
    ((primop.conditional-type self node)
     '#[type (proc #f (proc #f) (proc #f) top top top)])
    ((primop.type self node)
     '#[type (proc #f (proc #f boolean) top top)])))

;;; TYPE PREDICATES
;;;===========================================================================

(define-local-syntax (define-type-predicate name variant . rest)
  `(define-constant ,name
     ,(xcase variant
        ((and)
         `(make-and-type-predicate ',name . ,rest))
        ((header)
         `(make-header-type-predicate ',name . ,rest)))))

(define-constant make-and-type-predicate 
  (primop make-and-type-predicate (name mask value)

    (((primop.simplify self node)
      (simplify-parameterized-primop self node)))

    ((primop.test-code self node arg)      
     (let ((reg (get-register 'scratch node '*)))
       (emit vax/bicb3 (machine-num mask) arg reg)
       (emit vax/cmpb  (machine-num value) reg)))
    ((primop.presimplify self node)
     (presimplify-predicate node))
    ((primop.make-closed self)
     (make-closed-predicate self))
    ((primop.type-predicate? self) t)
    ((primop.type self node)
     '#[type (proc #f (proc #f boolean) top)])
    ((primop.predicate-type self node)
     '#[type (proc #f (proc #f) (proc #f) top top top)])
    ((primop.variant-id self) name)))
    

(define-constant make-header-type-predicate
  (primop make-header-type-predicate (name header)

    (((primop.simplify self node)
      (simplify-parameterized-primop self node)))

    ((primop.test-code self node arg)
     (emit vax/cmpb arg (machine-num header)))
    ((primop.presimplify self node)
     (presimplify-predicate node))
    ((primop.make-closed self)
     (make-closed-predicate self))
    ((primop.type-predicate? self) t)
    ((primop.type self node)
     '#[type (proc #f (proc #f boolean) top)])
    ((primop.predicate-type self node)
     '#[type (proc #f (proc #f) (proc #f) top top top)])
    ((primop.variant-id self) name)))

(define-type-predicate list?            and #xFC tag/pair)         ; low 2 bits
(define-type-predicate extend?          and #xFC tag/extend)
(define-type-predicate immediate?       and #xFC tag/immediate)
(define-type-predicate bignum-header?   and #x80 header/bignum) ; low 7 bits
(define-type-predicate template-header? and #x80 header/template)
(define-type-predicate vcell-header?    and #x80 header/vcell)  ; 
(define-type-predicate bytev-header?    and #x80 header/bytev)  ; pure bit
(define-type-predicate text-header?     and #x80 header/text)   ; pure bit


(define-type-predicate char?                  header header/char)
(define-type-predicate nonvalue?              header header/nonvalue)
(define-type-predicate general-vector-header? and #x80 header/general-vector)
(define-type-predicate string-header?         and #x80 header/slice)
(define-type-predicate symbol-header?         and #x80 header/symbol)
(define-type-predicate foreign-header?        and #x80 header/foreign)
(define-type-predicate unit-header?           and #x80 header/unit)
(define-type-predicate true-header?           header header/true)
(define-type-predicate double-float-header?   header header/double-float)
(define-type-predicate vframe-header?         and #x80 header/vframe)
                                                    
(define-type-predicate weak-set-header?   and #x80 header/weak-set)
(define-type-predicate weak-alist-header? and #x80 header/weak-alist)
(define-type-predicate weak-table-header? and #x80 header/weak-table)
(define-type-predicate weak-cell-header?  and #x80 header/weak-cell)


(define-constant fixnum?
  (primop fixnum? ()
    ((primop.test-code self node arg)
     (emit vax/bitl (machine-num 3) arg))
    ((primop.presimplify self node)
     (presimplify-predicate node))
    ((primop.make-closed self)
     (make-closed-predicate self))
    ((primop.type-predicate? self) t)
    ((primop.type self node)
     '#[type (proc #f (proc #f boolean) top)])
    ((primop.predicate-type self node)
     '#[type (proc #f (proc #f) (proc #f) top top top)])))


;;; MAKE-VECTORS
;;;=========================================================================

(define-constant make-vector-extend
  (primop make-vector-extend ()
    ((primop.arg-specs self) '(* 9 1))   ;; AN and S1
    ((primop.generate self node)
     (generate-make-vector-extend node))))

(define-constant %make-extend
  (primop %make-extend ()
    ((primop.arg-specs self) '(9 1))   ;; AN and S1
    ((primop.generate self node)
     (generate-make-extend node))
    ((primop.type self node)
     '#[type (proc #f (proc #f top) template fixnum)])))


;;; MAKE-PAIR

(define-constant %make-pair
  (primop %make-pair ()
    ((primop.generate self node)
     (generate-make-pair node))
    ((primop.type self node)
     '#[type (proc #f (proc #f pair))])))

;;; ONE-ARG-PRIMITIVES
;;;==========================================================================


(define-constant descriptor->fixnum
  (primop descriptor->fixnum ()
    ((primop.generate self node)
     (receive (source target rep) (one-arg-primitive node)
       (emit vax/bicl3 (machine-num 3) source target)
       (really-rep-convert node target 'rep/pointer target rep)
       (mark-continuation node target)))
    ((primop.type self node)
     '#[type (proc #f (proc #f fixnum) top)])))

(define-constant descriptor-tag
  (primop descriptor-tag ()
    ((primop.generate self node)
     (receive (source target rep) (one-arg-primitive node)
       (emit vax/ashl (machine-num 2) source target)
       (emit vax/bicl2 (machine-num #xFFFFFFF0) target) ; get low 4 bits
       (really-rep-convert node target 'rep/pointer target rep)
       (mark-continuation node target)))
    ((primop.type self node)
     '#[type (proc #f (proc #f fixnum) top)])))

(define-constant header-type
  (primop header-type ()
    ((primop.generate self node)
     (receive (source target rep) (one-arg-primitive node)
       (emit vax/bicl3 (machine-num #xFFFFFf83) source target) ; get low 7 bits
       (really-rep-convert node target 'rep/pointer target rep) ; mask out tag
       (mark-continuation node target)))
    ((primop.type self node)
     '#[type (proc #f (proc #f fixnum) top)])))

(define-constant %chdr
  (primop %chdr ()
    ((primop.side-effects? self) t)
    ((primop.generate self node)
     (generate-%chdr node))))


