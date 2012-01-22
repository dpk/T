(herald (front_end assign)
  (env t (orbit_top defs)))

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

;;;         ADDING CELLS FOR SIDE-AFFECTED LEXICAL VARIABLES
;;;============================================================================
;;;  There are two versions of this depending on whether or not MAKE-CELL
;;; takes an argument.
;;;
;;; (lambda (x) ... x ... (set x ...) ... (locative x) ...)
;;;   ==>
;;; (lambda (x)
;;;   (let ((x' (make-cell)))
;;;     (set-location x' x)
;;;      ... (location x') ... (set-location x' ...) ... x' ...))
;;;
;;; A warning is issued if a variable is set but never used.

(define (introduce-cell var)
  (let ((node (variable-binder var))
        (new-var (create-variable (variable-name var))))
    (hack-references var new-var)
    (let-nodes ((call (($ primop/make-cell) 1 (^ cont1)))
                 (cont1 (() (v new-var))
                   (($ primop/set-location) 1
                    (^ cont2) ($ primop/cell-value) (* var) (* new-var)))
                  (cont2 (#f) ()))
      (insert-call call cont2 node))))

;;; (lambda (x) ... x ... (set x ...) ... (locative x) ...)
;;;   ==>
;;; (lambda (x)
;;;   (let ((x' (make-cell x)))
;;;      ... (location x') ... (set-location x' ...) ... x' ...))

(define (hack-references var new-var)
  (if (not (any? true? (map-refs-safely (lambda (ref)
                                          (hack-reference ref new-var))
                                        var)))
      (user-message 'warning
                    "variable ~S is set but never referenced"
                    nil
                    (variable-name var))))

;;; Replace references to variables with indirections through locatives.
;;;   x              ==>  (contents x')
;;;   (set x y)      ==>  (set-contents x' y)
;;;   (locative x)   ==>  x'
;;; Returns T if the variable is being used, NIL if it is being set.

(define (hack-reference ref new-var)
  (let* ((parent (node-parent ref)) 
         (proc (call-proc parent)))
    (cond ((or (neq? (node-role ref) (call-arg 2))
               (not (primop-node? proc)))
           (dereference parent ref new-var)
           t)
          ((eq? (primop-value proc) primop/*set-var)
           (replace parent (assigner parent new-var))
           nil)
          ((eq? (primop-value proc) primop/*locative)
           (replace-call-with-value parent 
                                    (create-reference-node new-var))
           t)
          (else
           (dereference parent ref new-var)
           t))))

;;;  x ==> (contents-location cell-value x')

(define (dereference call ref new-var)
  (let ((new-v (create-variable (variable-name new-var))))
    (let-nodes ((new-call (($ primop/contents-location)
                           1
                           (^ l-node)
                           ($ primop/cell-value)
                           (* new-var)))
                 (l-node (() (v new-v)) ()))
      (insert-call new-call l-node (node-parent call))
      (replace ref (create-reference-node new-v)))))

;;;  (set x y) ==> (set-location cell-value y x')

(define (assigner call new-var)
  (let ((node (create-call-node 5 1)))
    (relate call-proc node (create-primop-node primop/set-location))
    (relate-four-call-args node
                           (detach ((call-arg 1) call))
                           (create-primop-node primop/cell-value)
                           (detach ((call-arg 3) call))
                           (create-reference-node new-var)) 
    node))

;;; This stuff is here to keep all of the MAKE-CELL calls in the same file.

;;; Make a thunk that creates a new cell.

(define (labels-make-cell-thunk)
  (let-nodes ((l1 (#f c1) (($ primop/make-cell) 1 (^ l2)))
               (l2 (#f v) (($ primop/set-location) 1
                           (^ l3) ($ primop/cell-value)
                           ''uninitialized-labels (* v)))
                (l3 (#f) ((* c1) 0 (* v))))
    l1))



