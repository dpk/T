(herald (front_end gen_interface)
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

;;; Interface between the front end and the code generator (in addition to the
;;; node tree).

;;; Is VAR defined in this file.

(define (defined-variable? var)
  (and (variable-definition var)
       (local-definition? (variable-definition var))))

;;; Return the node that is the definition of VAR.

(define (defined-variable-value var)
  (let ((def (variable-definition var)))
    (if def
        (get-variable-node-value var (definition-variant def))
        nil)))

(define (get-variable-node-value var variant)
  (let ((variant (if (eq? variant 'constant) 'define variant)))
    (iterate loop ((refs (variable-refs var)))
      (let ((ref (car refs)))
        (cond ((null? refs)
               nil)
              ((neq? variant (supports-definition ref))
               (loop (cdr refs)))
              (else
               (let ((val ((call-arg 3) (node-parent ref))))
                 (if (lambda-node? val) val nil))))))))

;;; Is VAR not a local variable.

(define (defined-variable-exported? var)
  (let ((def (variable-definition var)))
    (not (and def
              (memq? 'local (definition-data def))))))

;;; Get the type of the definition of VAR.

(define (defined-variable-variant var)
  (let ((def (variable-definition var)))
    (cond ((not def) 
           nil)
          ((eq? (definition-variant def) 'multiple)
           'define)
          (else
           (definition-variant def)))))

;;; Is VAR definined in this file or in the early binding environment.

(define (supported? var)
  (if (variable-definition var) t nil))


(define (primop-argument-type node)
  (let* ((proc (call-proc (node-parent node)))
         (type (primop.type (primop-value proc) (node-parent node))))
    (if (and type (proc-type? type))
        (vref (proc-type-args type) (relation-index (node-role node)))
        type/top)))

(define (primop-result-type var)
  (let ((type (primop-argument-type (variable-binder var))))
    (cond ((and type (proc-type? type))
           (vref (proc-type-args type)
                 (fx- (variable-number var) 1)))
          (else type/top))))
