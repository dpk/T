(herald base)

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

(define initial-primop-env
  (make-definition-table 'initial-primop-env))

(define-local-syntax (define-initial-primop id . clauses)
  `(let ((primop ,(primop-code id '() clauses)))
     (add-primop initial-primop-env primop)
     (set (table-entry primop-table ',id) primop)
     (make-definition-entry (create-variable ',id)
                            initial-primop-env
                            '()
                            'constant
                            (node->vector (create-primop-node primop))
                            nil)))

;;; BASIC PRIMOPS
;;;============================================================================
;;; These are all known to alphatize, simplify, etc.

;;; Place marking primops
;;;============================================================================
;;; These are used by alpha to mark points in the tree.

(define-initial-primop undefined)

(define-initial-primop *primop
  ((primop.simplify self node)
   (simplify-*primop self node)))

(define-initial-primop undefined-effect
  ((primop.side-effects? self) t)
  ((primop.generate self node)
   (generate-undefined-effect node))
  ((primop.special? self) t)
  ((primop.simplify self node)
   (simplify-undefined-effect node))
  ((primop.type self node)
   '#[type (proc #f (proc #f) string)]))

(define-initial-primop Y
  ((primop.generate self node)
   (generate-labels node))
  ((primop.simplify self node)
   (simplify-y node))
  ((primop.side-effects? self) t)
  ((primop.special? self) t))

(define-initial-primop conditional
  ((primop.generate self node)
   (primop.generate (primop-value ((call-arg 3) node)) node))
  ((primop.conditional? self) t)
  ((primop.type self node)
   (if (node? node)
       (primop.conditional-type (primop-value ((call-arg 3) node)) node)
       '#[type top]))
  ((primop.simplify self node)
   (primop.simplify (primop-value ((call-arg 3) node)) node)))

(define-initial-primop test
  ((primop.generate self node)
   (destructure (((then else () type arg) (call-args node)))
     (primop.test-code (primop-value type)
                       node
                       (access-value node (leaf-value arg)))
     (if (primop.jump-on-equal? (primop-value type))
         (emit-jump 'jeql else then)       
         (emit-jump 'jneq else then))))
  ((primop.presimplify self node)
   (presimplify-to-conditional node))
  ((primop.simplify self node)
   (simplify-test node))
  ((primop.conditional-type self node)
   (primop.predicate-type (primop-value ((call-arg 4) node)) node))
  ((primop.conditional? self) t))

(define-initial-primop true?
  ((primop.test-code self node arg)
   (generate-nil-test arg))
  ((primop.presimplify self node)
   (presimplify-predicate node))           
  ((primop.make-closed self)
   (make-closed-predicate self))
  ((primop.jump-on-equal? self) t)       ; because we compare with nil
  ((primop.type-predicate? self) t)
  ((primop.predicate-type self node)
   '#[type (proc #f (proc #f) (proc #f) top top top)])
  ((primop.type self node)
   '#[type (proc #f (proc #f boolean) top)]))

(define-initial-primop *set-var
  ((primop.side-effects? self) t)
  ((primop.generate self node)
   (generate-set node
                 ((call-arg 2) node)
                 ((call-arg 3) node)))
  ((primop.uses-L-value? self) t)
  ((primop.definition? self) t)
  ((primop.definition-variant self) 'set))

(define-initial-primop single-set-var
  ((primop.side-effects? self) t)
  ((primop.generate self node)
   (generate-single-set node
                        ((call-arg 2) node)
                        ((call-arg 3) node)))
  ((primop.uses-L-value? self) t))

(define-initial-primop *locative
  ((primop.generate self node)
   (generate-locative node))
  ((primop.definition-variant self) 'set)
  ((primop.definition? self) t)
  ((primop.uses-L-value? self) t))

; Defining primops
;============================================================================
; These assign values to global variables.

(define-initial-primop *define
  ((primop.side-effects? self) t)
  ((primop.generate self node)
   (generate-define-var node))
  ((primop.definition? self) t)
  ((primop.uses-L-value? self) t)
  ((primop.definition-variant self) 'define))

(define-initial-primop *lset
  ((primop.side-effects? self) t)
  ((primop.generate self node)
   (generate-define-var node))
  ((primop.definition? self) t)
  ((primop.uses-L-value? self) t)
  ((primop.definition-variant self) 'lset))

;;; To deal with objects and their ilk.
(define-initial-primop proc+handler)

(define-initial-primop remove-state-object
  ((primop.side-effects? self) t)
  ((primop.generate self node)
   (generate-remove-state-object node)))

;;; The three location primops.  These generate code for locations the same
;;; way COMPARE does for EQ? etc.
;;;   (CAR-LOC (LAMBDA (X) (CONTENTS <cont> X) L) =>
;;;   (CONTENTS-LOCATION <cont> CAR-LOC L)
;;;
;;;   (CAR-LOC (LAMBDA (X) (SET-CONTENTS <cont> X A) L) =>
;;;   (SET-LOCATION <cont> CAR-LOC A L)  ;Value goes before arguments.
;;;

(define-initial-primop contents-location
  ((primop.generate self node)
   (generate-contents-location node))
  ((primop.type self node)
   (if (node? node)
       (primop.contents-type (primop-value ((call-arg 2) node)))
       '#[type top])))
 
(define-initial-primop set-location
  ((primop.side-effects? self) t)
  ((primop.generate self node)
   (generate-set-location node))
  ((primop.type self node)
   (if (node? node)
       (primop.set-type (primop-value ((call-arg 2) node)))
       '#[type top])))
 
(define-initial-primop locative-location)

(define-initial-primop make-cell
  ((primop.generate self node)
   (generate-make-cell node))
  ((primop.type self node)
   '#[type (proc #f (proc #f cell) top)]))

(define-initial-primop cell-value
  ((primop.location? self) t)
  ((primop.location-specs self) (fx- (fx* 1 4) 2))
  ((primop.rep-wants self) 'rep/pointer)
  ((primop.simplify self node)
   (simplify-location node))
  ((primop.type self node)
   '#[type (proc #f (proc #f top) cell)]))

(define-initial-primop lap 
  ((primop.special? self) t)
  ((primop.side-effects? self) t)
  ((primop.generate self node)        
   (clear-slots)   
   (lap-transduce (leaf-value ((call-arg 2) node)))))

(define-initial-primop lap-template 
  ((primop.side-effects? self) t)
  ((primop.generate self node)
   (generate-lap-template node)))


