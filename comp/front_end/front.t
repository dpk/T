(herald (front_end front))

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

;;;                          Main Interface
;;;============================================================================

;;; Exciting Global Variables

(lset *new-env*           nil)   ; The new support environment
(lset *early-binding-env* nil)   ; Early binding environment,
(lset *syntax*            nil)   ;    syntax table,
(lset *shape*             nil)   ;        and shape used in compiling a file.

;;; Bind the variables that need to be available throughout the compilation.

(define (front-init early-binding-env cont)
  (bind ((*new-env* (make-definition-env early-binding-env '*new-env*))
         (*early-binding-env* early-binding-env))
    (cont)))

;;; Main entry point.

(define (make-code-tree+support exp syntax)
  (receive (nodes shape)
           (file-exps->nodes+shape (cddr (caddr exp))
                                   syntax)
    (return (rebuild nodes) shape)))


;;;                       Initialization
;;;===========================================================================

(define (information-filename filename)
  (filename-with-type filename *information-file-extension*))

(define (orbit-vax-init . directory)
  (orbit-vax-setup (if directory (car directory) '#f))
  (orbit-init 'base
              'constants
              'primops
              'arith
              'locations
              'low
              'predicates
              'open
              'aliases
              'carcdr
	      'genarith))

(define (orbit-m68-init . directory)
  (orbit-m68-setup (if directory (car directory) '#f))
  (orbit-init 'base
              'constants
              'primops
              'arith
              'locations
              'low
              'predicates
              'open
              'aliases
              'carcdr
	      'genarith))

;;; All the primops used by the compiler itself.  These must be in the base
;;; primop file

(lset primop/lap nil)
(lset primop/lap-template nil)
(lset primop/*primop             nil)
(lset primop/undefined           nil)
(lset primop/undefined-effect    nil)
(lset primop/Y                   nil)
(lset primop/conditional         nil)
(lset primop/test                nil)
(lset primop/true?               nil)
(lset primop/*set-var            nil)
(lset primop/*locative           nil)
(lset primop/*define             nil)
(lset primop/*lset               nil)
(lset primop/*define-constant    nil) ; Can be removed after back_end change
(lset primop/proc+handler        nil)
(lset primop/contents-location   nil)
(lset primop/set-location        nil)
(lset primop/make-cell           nil)
(lset primop/cell-value          nil)
(lset primop/single-set-var      nil)
(lset primop/remove-state-object nil)

;;; The names of the above primops together with flags to indicate which ones
;;; are exported to STANDARD-SUPPORT-ENV.

(define known-primops
  '((lap #f)
    (lap-template #f)
    (*primop             #f)
    (undefined           #t)
    (undefined-effect    #t)
    (Y                   #f)
    (conditional         #f)
    (test                #f)
    (true?               #t)
    (*set-var            #f)
    (*locative           #f)
    (*define             #f)
    (*lset               #f)
    (proc+handler        #f)
    (contents-location   #f)
    (set-location        #f)
    (make-cell           #t)
    (cell-value          #t)
    (remove-state-object #f)
    (single-set-var      #f)))

;;; *BASE-SUPPORT-ENV* is the standard support for the system.
;;; *STANDARD-SUPPORT-ENV* contains the support for the STANDARD-ENV.

;;; Initialize the compiler.  INIT-MODULE contains loadable definitions of the
;;; base primops.  MODULES are files whose INF files make up the systems
;;; standard support.

(define (orbit-init init-module . modules)
  (orbit-uninit)
  (bind ((*noise-stream* (terminal-output)))
    (load (module-name->filename init-module) orbit-env)
    (walk (lambda (s)
            (let* ((name (car s))
                   (primop (table-entry primop-table name)))
              (if (not primop)
                  (bug '"no early binding for ~S" name))
              (set (*value orbit-env (concatenate-symbol 'primop/ name))
                   primop)
              (if (not (cadr s))
                  (set (initial-primop-env (car s)) nil))))
          known-primops)
    (define base-early-binding-env
      (make-definition-env false 'base-early-binding-env))
    (instantiate-definition-table base-early-binding-env initial-primop-env)
    (walk (lambda (spec)
            (instantiate-definition-table base-early-binding-env
                                          (get-definition-table spec)))
          modules)
    (let ((table (make-definition-env false 'standard-early-binding-env)))
      (walk (lambda (name)
              (if (base-early-binding-env name)
                  (set (table name) (base-early-binding-env name))))
            *t-exports*)
      (define standard-early-binding-env table)
      (*define standard-env 'standard-early-binding-env
               standard-early-binding-env)
      (*define standard-env 'base-early-binding-env
               base-early-binding-env)
      standard-early-binding-env)))


;;; Uninitialize the compiler.

(define (orbit-uninit)
  (clean-table definition-tables)
  (clean-table constructed-primops) ; Remove pointers to old nodes
  (clean-table primop-table)
  (walk (lambda (s)
          (set (*value orbit-env (concatenate-symbol 'primop/ (car s)))
               '#f))
        known-primops))

;;; Primops are compiled by calling the compiler recursively.  When
;;; cross-compiling the primops need to be compiled using the running system's
;;; constants instead of the target system's.  Since the back end and the
;;; assembler get these constants from ORBIT-ENV the following unpleasantness
;;; is necessary.

(lset *compile-primops?* t)
(lset *cross-compiling?* nil)
(lset *target-system-constants* nil)
(lset *running-system-constants* nil)

(define (primop-compile-init running target)
  (let ((target-table (load-system-constants target))
        (running-table (load-system-constants running)))
    (bind (((print-env-warnings?) nil))
      (walk-table (lambda (ident value)
                    (*lset orbit-env ident value))
                  running-table))
    (set *cross-compiling?* t)
    (set *running-system-constants* running-table)
    (set *target-system-constants* target-table)))

(define system-constants-env
  (make-locale standard-env 'system-constants-env))

(define system-constants-syntax
  (env-syntax-table system-constants-env))

(define (install-system-constants table)
  (walk-table (lambda (ident value)
                (*set-value orbit-env ident value))
              table))

;;; Load a file using a bogus macro expansion for DEFINE and friends to create
;;; a table of the files definitions.

(define (load-system-constants file)
  (bind (((print-env-warnings?) nil))
    (let* ((table (make-table file))
           (filename (filename-with-type (->filename file) 't))
           (syntax (make-bogus-define table)))
      (set (syntax-table-entry system-constants-syntax 'define)
           syntax)
      (set (syntax-table-entry system-constants-syntax 'define-constant)
           syntax)
      (set (syntax-table-entry system-constants-syntax 'define-integrable)
           syntax)
      (load-quietly filename system-constants-env)
      table)))

(define (make-bogus-define table)
  (macro-expander (bogus-define ident value)
    (set (table-entry table ident) value)
    ''#f))





