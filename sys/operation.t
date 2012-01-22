(herald operation
        (env tsys (osys kernel)))

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

;;;; %OPERATION and friends

(define-integrable (operation? obj)
  (and (extend? obj) (eq? (extend-header obj) *operation-template*)))

;;; Create an operation.

(define (%operation default id h)
  (let ((op (%make-extend *operation-template* %%operation-size)))
    (set (%operation-default     op) default)
    (set (%operation-id          op) id)
    (set (%operation-handler     op) h)
    op))

;;; Say (RET procedure) to proceed from this error.
;;; The value returned will be the value of the call to the operation.

(define (no-default-method op args)
  (error "operation not handled~%  ~s"
	 (cons (%operation-id op) args)))

(define (%massage-default default)
  (cond ((extend? default) default)
        (else nil)))

(define (join . objects)
  (cond ((null? objects) (object nil))
        ((null? (cdr objects)) (car objects))
        ((null? (cddr objects)) (join2 (car objects) (cadr objects)))
        (else (join2 (car objects) (apply join (cdr objects))))))

(define (join2 lhs rhs)
  (cond ((joined? lhs)
         (join2 (joined-lhs lhs) (join2 (joined-rhs lhs) rhs)))
        (else 
         (let ((j (%make-extend *join-template* 2)))
           (set (joined-lhs j) lhs)
           (set (joined-rhs j) rhs)
           j))))

(define-integrable (joined? obj) 
  (and (extend? obj) (eq? (extend-header obj) *join-template*)))

(define (*object proc handler)
  (let ((b (%make-extend *bogus-entity-template* 2)))
    (set (bogus-entity-procedure b) proc)
    (set (bogus-entity-handler b) handler)
    b))

(define-integrable (bogus-entity? obj) 
  (and (extend? obj) (eq? (extend-header obj) *bogus-entity-template*)))

(define (%predicate id)
  (%operation false
              id
              (join-methods handle-operation
                ((print-type-string self) "Predicate"))))

;;; Must precede any DEFINE-OPERATION.

(define handle-operation
  (object nil
    ((identification self) (%operation-id self))
    ((set-identification self id)
          (if (not (%operation-id self))
       	(set (%operation-id self) id)))
    ((default-method self) (%operation-default self))
    ((get-loaded-file self)
     (get-loaded-file (%operation-default self)))  
    ((print-type-string self) "Operation")))

(define-operation (default-method op))


;;; SETTER and "settable operations."

(define (%settable-operation default id)
  (%operation default
              id
              (let ((the-setter
                     (%operation nil
                                 (cons 'setter (cons id '()))
                                 handle-operation)))
                (join-methods handle-operation
                  ((setter op) the-setter)))))

;;; Standard very-general-purpose operations

(define-operation (procedure? obj)
  (cond ((bogus-entity? obj) 
         (procedure? (bogus-entity-procedure obj)))
        ((closure? obj) 
         (fx> (template-nargs (extend-header obj)) 0))
        (else nil)))

(define-operation (argspectrum obj)
  (cond ((bogus-entity? obj)
         (argspectrum (bogus-entity-procedure obj)))
        ((procedure? obj)
         (cons (fx- (template-nargs (extend-header obj)) 1)
               (if (template-nary? (extend-header obj)) 't '())))
        ((frame? obj)
         (cons (fx-negate (fx+ (template-nargs (extend-header obj)) 1))
               (if (template-nary? (extend-header obj)) 't '())))
        (else
         (error "(~S ~S): object not callable" 'argspectrum obj))))

(define (arglist->argspectrum z)        ; see EVAL, OBJECT
  (iterate loop ((z z)
                 (n 0))
    (cond ((atom? z) (cons n (not (null? z))))
          (else (loop (cdr z) (fx+ n 1))))))

(define-operation (set-identification obj id) nil)   ;do nothing by default

(define-operation (get-proc-name obj)
  (cond ((template? obj)
         (template-definer obj))
        (else nil)))

(define identification
  (operation (lambda (obj)
               (cond ((bogus-entity? obj)
                      (identification (bogus-entity-procedure obj)))
                     ((and (closure? obj)
                           ;++ (template-definer (extend-header obj))???
                           (let* ((header (extend-header obj))
                                  (offset (template-definer-vcell-offset header)))
                             (and offset
                                  (extend-elt (template-unit header) offset))))
                      => (lambda (vcell)
                           (if (eq? (vcell-contents vcell) obj)
                               (vcell-id vcell)
                               nil)))
                     (else nil)))
    ((setter self) set-identification)
    ((identification self) 'identification)))

;;; IMMUTABLE? - for making objects be read-only.

(define-settable-operation (mutable? obj))

(define set-mutable? (setter mutable?))

(define-operation (set-immutable obj)
  (set-mutable? obj nil))

(define (immutable? obj) (not (mutable? obj)))

;;; randomness

(define-operation (insert self obj))
(define-operation (delete self obj))
(define-operation (hash self))
(define hashx hash)
;(define-operation (sort self))



