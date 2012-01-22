(herald (front_end envs)
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

;;;    This implements the early binding database for ORBIT.  DEFINE and its
;;; cousins generate early binding information for the variable defined.
;;;    Currently the only information saved is the type of the variable's value
;;; and an integrable value if the definition is declared to be constant.

;;;    A definition table contains the definition information for a given
;;; module.  This includes a table of mapping names to definitions and a list
;;; of the primops defined in the module.
;;;
;;;  (<table> <symbol>) => the definition for <symbol> in the locale.  This
;;;      is settable.
;;;  (WALK-DEFINITIONS <table> <proc>) => (<proc> <symbol> <definition>) for
;;;      each <symbol> in the table.
;;;  (ADD-PRIMOP <table> <primop>) adds the primop to the table's primop set.
;;;  (PRIMOP-LIST <table>) => a list of all the primops defined in the module.

;;;  The method for AUGMENT-CONTEXT is used by the HERALD special form.

(define (make-definition-table id)
  (let ((table (make-table id))
        (primops '()))
    (object (lambda (name)
              (table-entry table name))
      ((setter self)
       (lambda (name value)
         (set (table-entry table name) value)))
      ((walk-definitions self proc)
       (table-walk table proc))
      ((add-primop self primop) (push primops primop))
      ((primop-list self) primops)
      ((early-binding-table? self) t)
      ((module-id env) id)
      ((identification self) id)
      ((print self stream)
       (format stream "#{Early-binding-table ~D ~S}" (object-hash self) id)))))

(define-predicate early-binding-table?)
(define-predicate early-binding-env?)
(define-operation (module-id env))
(define-operation (walk-definitions env proc))
(define-operation (add-primop env primop))
(define-operation (primop-list env))
(define-operation (instantiate-definition-table env table))

(define (make-empty-definition-table id)
  (make-definition-table id))

(define (make-empty-definition-env id)
  (make-definition-env false id))

;;;   The information in a definition table is used by instantiating the table 
;;; in a definition environment and then using the environment for support.
;;; The compile-time environments mimic the behavior of T's run-time
;;; environments.

(define (make-definition-env super id)
  (let ((definition-table (make-table id))
        (primops '()))
    (object (lambda (name)
              (cond ((table-entry definition-table name)
                     => identity)
                    (else
                     (super name))))
      ((setter self)
       (lambda (name def)
         (set (table-entry definition-table name) def)))
      ((instantiate-definition-table self table)
       (let ((table (enforce early-binding-table? table)))
         (instantiate-table self table definition-table (locative primops))))
      ((walk-definitions self proc)
       (table-walk definition-table proc))
      ((add-primop self primop) (push primops primop))
      ((primop-list self) primops)
      ((module-id env) id)
      ((identification self) id)
      ((early-binding-env? self) t)
      ((augment-context self . rest)
       (get-definition-environment self nil rest))
      ((print self stream)
       (format stream "#{Early-binding-env ~D ~S}"
                      (object-hash self) id)))))

;;; Instantiate a definition-table by instantiating each of its definitions.
;;; The primops are added to the primop set of the environment.

(define (instantiate-table env table defs primops)
  (walk-definitions table
                    (lambda (name def)
                      (set (table-entry defs name)
                           (instantiate-definition def env))))
  (set (contents primops)
       (append (primop-list table) (contents primops)))
  (return))

;;; Construct a definition environment containing the modules designated by
;;; SPECS and with superior environment SUPER.

(define (get-definition-environment super id specs)
  (let ((new (make-definition-env super id)))
    (walk (lambda (spec)
            (instantiate-definition-table new
                                          (get-definition-table spec)))
          specs)
    new))

(lset *standard-definition-env* (make-empty-definition-env 'empty-env))

;;;   Change all CONSTANT definitions to DEFINE definitions.  This means that
;;; the environment's information will be used for error checking only, no
;;; integration will be done.

(define (weaken-definition-env env)
  (walk-definitions env
                    (lambda (name definition)
                      (ignore name)
                      (cond ((eq? 'constant (definition-variant definition))
                             (set (definition-value definition) nil)
                             (set (definition-variant definition) 'define)))))
  (return))


;;;                      DEFINITION structures
;;;============================================================================
;;;   Structure to hold information for a symbol in a particular environment.

(define-structure-type definition
  variant   ; What kind of definition this is, one of 'DEFINE etc.
  value     ; The value VARIABLE is bound to if VARIABLE is integrable.
  env       ; The environment this definition is instantiated in.
  type      ; The type of the value VARIABLE is bound to.
  data      ; Not currently used.
  (((print self stream)
    (format stream "#{Definition~_~S}" (object-hash self)))))

(let ((s (stype-master definition-stype)))
  (set (definition-env s) false))

(define (make-definition-entry var table data variant value type)
  (let ((s (make-definition)))
    (set (definition-data     s) data)
    (set (definition-variant  s) variant)
    (set (definition-value    s) value)
    (set (definition-env      s) nil)
    (set (definition-type     s) type)
    (set (variable-definition var) s)
    (if table (set (table (variable-name var)) s))
    s))

;;; A definition is instantiated by copying it and setting the ENV slot of the
;;; copy.  This is only necessary if the definition has a value (and sometimes
;;; not even then).

(define (instantiate-definition def env)
  (cond ((not (definition-value def))
         def)
        (else
         (let ((new (copy-structure def)))
           (set (definition-env new) env)
           new))))

;;;   Get the definition of a variable.  If the value of the variable is another
;;; variable the second variable's definition is returned instead.

(define (get-variable-definition variable)
  (let ((def (variable-definition variable)))
    (cond ((and def
                (eq? 'constant (definition-variant def))
                (definition->variable def))
           => get-variable-definition)
          (else def))))

;;;   A predicate that determines if REF is the definition of some variable.
;;; Returns the definiiton variant if it exists.

(define (supports-definition ref)
  (and (call-arg? (node-role ref))
       (let ((proc (call-proc (node-parent ref))))
         (if (and (eq? (call-arg 2) (node-role ref))
                  (primop-node? proc)
                  (primop.definition? (primop-value proc)))
             (primop.definition-variant (primop-value proc))
             nil))))

;;; Is this definition local to the module currently being compiled.

(define (local-definition? def)
  (eq? *new-env* (definition-env def)))
       
;;; Is NODE an integrable definition.

(define (integrable-definition? node)
  (cond ((and (reference-node? node)
              (get-variable-definition (reference-variable node)))
         => (lambda (def)
              (eq? 'constant (definition-variant def))))
        (else nil)))





