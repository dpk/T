(herald (orbit_top oprimops))

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

;;;
;;;    PRIMITIVE OPERATIONS
;;;
;;; The operations on primops are all of the form PRIMOP.<name>
;;;

(define primop-table (make-table 'primop-table))

;;; %PRIMOP
;;;   The information about a primitive operation.

(define-structure-type %primop

;;; Data fields for the primop implemetation
  handler        ; A handler for this specific primop
  bitv           ; Bit set for coding primop predicates
  id             ; Symbol identifying this primop
  formals        ; Formal arguments to this primop if it has parameters
  source         ; Source code for the primop

;;; Methods that most primops have
  simplify       ; Simplify method
  integrate?     ; When to integrate this primop
  type           ; The type of this primop as a procedure
  generate       ; Code generation routine
  arg-specs      ; Code generation data
  rep-wants      ; Code generation data

  (((identification self) (%primop-id self))
    ((print self stream) (format stream "#{Primop~_~S~_~S}"
                         (object-hash self) (%primop-id self)))))

(define primop? %primop?)

(set (%primop-source (stype-master %primop-stype)) nil)

;;; System internal fields

(define primop.id          %primop-id)
(define primop.formals     %primop-formals)
(define primop.source      %primop-source)

(define (any-primop-id primop)
  (cond ((primop.variant-id primop)
         => identity)
        ((primop.id primop)
         => identity)
        (else
         'anonymous)))

;;;
;;;   Primop Predicates
;;;
;;; These are coded into the %PRIMOP-BITV field of %PRIMOP.  Every predicate
;;; has an associated index into primops bitvs that indicates the value of the
;;; predicate on the particular primop.

(define primop-predicate-table
  (make-table 'primop-predicate-table))

(lset *next-primop-predicate-index* 0)

(define-local-syntax (define-primop-predicate name)
  (let ((name (concatenate-symbol 'primop. name)))
    `(let ((mask (set-bit-field 0 *next-primop-predicate-index* 1 1)))
       (increment *next-primop-predicate-index*)
       (set (table-entry primop-predicate-table ',name) mask)
       (define ,name (primop-predicate mask)))))

(define (primop-predicate mask)
  (lambda (primop)
    (fxn= 0 (fixnum-logand mask (%primop-bitv primop)))))

(define-primop-predicate constructed?)
(define-primop-predicate side-effects?)
(define-primop-predicate settable?)
(define-primop-predicate uses-L-value?)
(define-primop-predicate definition?)
(define-primop-predicate special?)
(define-primop-predicate type-predicate?)
(define-primop-predicate conditional?)
(define-primop-predicate location?)
(define-primop-predicate recursive?)

;;;
;;;   Primop Fields
;;;
;;;  These are the operations that many but not all primops handle.

(define primop-field-table
  (make-table 'primop-field-table))

(define-local-syntax (define-primop-field name args . default)
  (let* ((title (concatenate-symbol 'primop. name))
         (field (concatenate-symbol '%primop- name))
         (primop (car args))
         (method (if default 
                     default
                     `(bug "~S has no method for ~S" ',primop ',title))))
    `(block (set (,field (stype-master %primop-stype))
                 (lambda ,args (ignorable . ,args) . ,method))
            (set (table-entry primop-field-table ',title) ',field)
            (define (,title . ,args)
              ((,field ,primop) . ,args)))))

;;; Simplification
(define-primop-field simplify    (primop node) nil)
(define-primop-field integrate?  (primop node)
  (and (eq? (node-role node) call-proc)
       (primop-type-check primop (node-parent node))))

;;; Code generation
(define-primop-field generate  (primop node))
(define-primop-field type      (primop node) type/top)
(define-primop-field arg-specs (primop) nil)
(define-primop-field rep-wants (primop) nil)

;;;
;;;   Primop Operations
;;;
;;; Non generic operations, specific to certain types of primops 

(define primop-operation-table
  (make-table 'primop-operation-table))

(define-local-syntax (define-primop-operation name args . default)
  (let* ((title (concatenate-symbol 'primop. name))
         (field (concatenate-symbol '%primop- name))
         (primop (car args))
         (method (if default 
                     default
                     `(error "~S has no method for ~S" ',primop ',title))))
    `(block (define-operation (,field %%handler . ,args) . ,default)
            (set (table-entry primop-operation-table ',title) ',field)
            (define (,title . ,args)
              (,field (%primop-handler ,primop) . ,args)))))

;;; Alphatize
(define-primop-operation make-closed (primop) (make-closed-primop primop))

;;; Simplification
(define-primop-operation presimplify (primop node) nil)

;;; Parameterized primops
(define-primop-operation constructor (primop))
(define-primop-operation arglist     (primop) '())
(define-primop-operation variant-id  (primop) nil)

;;; Creating support
(define-primop-operation definition-variant (primop))

;;; Flow of control
(define-primop-operation test-code    (primop node arg) nil)
(define-primop-operation compare-code (primop node arg) nil)

;;; Code Generation
(define-primop-operation values-returned (primop)
  (if (primop.side-effects? primop) 0 1))

;;; Locations
(define-primop-operation location-specs (primop) nil)
(define-primop-operation simplify-setter (self call) nil)
(define-primop-operation contents-type (self) type/top)
(define-primop-operation set-type (self) type/top)

;;; Conditionals and Predicates
(define-primop-operation conditional-type (primop node))
(define-primop-operation predicate-type (primop node))
(define-primop-operation primop.simplify-jump? (primop index) nil)
(define-primop-operation primop.known-values? (primop call)
  (return nil nil nil))

;;; Type Predicates
(define-primop-operation jump-on-equal? (primop) nil)

;;;
;;;   Parameterized Primops
;;;
;;; These are primops that produce new primops when called.

;;; BUG: Parameterized primops cannot produce parameterized primops
;;; BUG: Parameterized primops cannot be N-ary

(define (hash-primop-list l)
  (iterate loop ((l l) (hash 4324277))
    (if (null? l)
        (fixnum-logand hash *max-fixnum*)
        (loop (cdr l)
              (fixnum-add (fixnum-ashl hash 1)
                          (descriptor->fixnum (car l)))))))

(define constructed-primops 
  (create-%table 'constructed-primops
                 0
                 t
                 list?
                 hash-primop-list
                 alikev?))

(define (construct-primop base args)
  (let ((key (cons base args)))
    (cond ((table-entry constructed-primops key)
           => identity)
          (else
           (let ((new ((apply (primop.constructor base) args) base)))
             (set (table-entry constructed-primops key) new)         
             new)))))

;;;
;;;   Creating Primops
;;;
;;; The syntax for primops is (PRIMOP <id> <formals list> . <method-clauses>).
;;; They could be easily implemented as objects.  The implementation in this
;;; file is (supposedly) faster.
;;;

(define primop-syntax-table
  (make-syntax-table (env-syntax-table t-implementation-env)
                     'primop-syntax-table))

(set (env-for-syntax-definition primop-syntax-table) orbit-env)

(define (primop->executable primop)
  (primop-code (primop.id primop)
               (primop.formals primop)
               (primop.source primop)))

(define (primops->source-code primops)
  (map (lambda (p)
         `(set (table-entry primop-table
                            ',(primop.id p))
               ,(primop->executable p)))
       primops))

(define (primop-executables->source-code names sources)
  (map (lambda (name source)
         `(set (table-entry primop-table ',name) ,source))
       names
       sources))

(define (orbit-primop-compile source env)
  (bind ((*noise-flag* nil)
         (*debug-flag* nil)
         (*noise+error*    (error-output))
         (*noise+terminal* null-port)
         (*noise-stream*   null-port))
    (receive (comex supex)
             (compile `(,syntax/lambda () . ,source)
                      standard-early-binding-env
                      (env-syntax-table env)
                      (->filename 'primop)
                      '(primop))
      (ignore supex)
      comex)))

;;; Useful mask

(define constructed-mask
  (table-entry primop-predicate-table 'primop.constructed?))

(define (primop-code name formals clauses)
  (receive (bitv fields methods)
           (parse-primop-clauses (if formals (car clauses) clauses))
    `(make-primop ',name
                  ,bitv
                  ',formals
                  (object '#f
                    ,@(if formals
                          `(((%primop-constructor %%handler self) 
                             ,(make-primop-constructor formals
                                                       (cdr clauses))))
                          '())
                    . ,methods)
                  . ,fields)))

(define (make-primop name bitv formals handler . fields)
  (let ((p (make-%primop)))
    (set (%primop-id          p) name)
    (set (%primop-bitv        p) bitv)
    (set (%primop-formals     p) formals)
    (set (%primop-handler     p) handler)
    (iterate loop ((fields fields))
      (cond (fields
             (set ((car fields) p) (cadr fields))
             (loop (cddr fields)))))
    p))

(define (parse-primop-clauses clauses)
  (iterate loop ((to-do clauses) (fields '()) (methods '()) (bitv 0))
    (destructure ((((name . args) . body) (car to-do)))
      (cond ((null? to-do)
             (return bitv fields methods))
            ((table-entry primop-predicate-table name)
             => (lambda (mask)
                  (loop (cdr to-do) fields methods (fixnum-logior bitv mask))))
            ((table-entry primop-field-table name)
             => (lambda (field)
                  (loop (cdr to-do)          ;; Could do args checking
                        `(,field
                          (lambda ,args ,(make-ignorable args) . ,body)
                          . ,fields)
                        methods
                        bitv)))
            ((table-entry primop-operation-table name)
             => (lambda (operation)
                  (loop (cdr to-do)
                        fields
                        `(((,operation %%handler . ,args)
                           ,(make-ignorable args) . ,body) . ,methods)
                        bitv)))
            (else
             (bug "unknown primop operation in ~S" (car to-do)))))))

(define (make-ignorable vars)
  (iterate loop ((vars vars) (res '()))
    (cond ((null? vars)
           `(ignorable . ,res))
          ((atom? vars)
           `(ignorable ,vars . ,res))
          (else
           (loop (cdr vars) (cons (car vars) res))))))

(define (make-primop-constructor formals clauses)
  (receive (c-bitv c-fields c-methods)
           (parse-primop-clauses clauses)
    (let ((bitv (fixnum-logior c-bitv constructed-mask)))
      `(lambda ,formals
         (sub-primop-constructor ,bitv 
                                 (object '#f
                                   ((%primop-arglist %%handler self)
                                    (list . ,formals))
                                   . ,c-methods)
                                 . ,c-fields)))))

(define (sub-primop-constructor bitv handler . fields)
  (lambda (parent)
    (let ((p (copy-structure parent)))
      (set (%primop-bitv    p) (fixnum-logior bitv (%primop-bitv parent)))
      (set (%primop-handler p) (join handler (%primop-handler parent)))
      (iterate loop ((fields fields))
        (cond (fields
               (set ((car fields) p) (cadr fields))
               (loop (cddr fields)))))
      p)))



