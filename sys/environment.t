(herald environment
  (env tsys (osys weak) (osys table)))

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

;;;; code for value cells, environments, locales, and loaded-files.

;;; general remarks:
;;; - a locale is a kind of environment.
;;; - a loaded-file is a different kind of environment.
;;; - the interpreter creates other environments which are neither
;;;   loaded-files nor locales.
;;; - every locale maintains a population of its inferior loaded-files.
;;; - any environment may be used as a "shape" (see eval).
;;; - "id" usually abbreviates "loaded-file identifier."
          
(define-operation (locale-inferiors obj))
(define-operation (locale-walk obj proc))
(define-predicate locale?)

(define-operation (loaded-files env)
  (if (environment? env) 
      (loaded-files (env-superior env))
      (error "loaded files not found ~s" env)))

(define-settable-operation (loaded-file env id))

(define set-loaded-file (setter loaded-file))

(define-operation (env-syntax-table env) 
  (if (environment? env) 
      (env-syntax-table (env-superior env))
      (error "env-syntax-table not found ~s" env)))

(define-simple-switch print-env-warnings?
                      boolean?
                      t)

(define (env-warning msg id)
  (cond ((print-env-warnings?)
         (let ((out (error-output)))
           (format out "[~a~_~s]~_" msg id)
           (force-output out)))))

(define (check-rebinding vc dfining? who)
  (ignore who)
  (cond ((nonvalue? (vcell-contents vc))
         nil)   ; do nothing
        ((vcell-defined? vc)
         (env-warning (if dfining? "Redefining" "Assigning")
                      (vcell-id vc)))
        (dfining? 
         (env-warning "Defining" (vcell-id vc))))
  (if dfining? 
      (set-vcell-defined vc)
      (set-vcell-undefined vc))
  vc)

;;; non-local reference.

(define *value
  (object (lambda (env id)
            (cond ((env-lookup env id nil nil) => contents)
                  (else
                   (error "unbound variable~%  (~s ~s ~s)"
                          '*value env id))))
          ((setter self) *set-value)))

;;; non-local definition.

(define (*define env id val)
  (let ((vcell (env-lookup env id t t)))
    (distribute-vcells vcell)
    (define-contents vcell val)
    (set-identification val id)
    val))

(define (*lset env id val)
  (let ((vcell (env-lookup env id t t)))
    (distribute-vcells vcell)
    (set-contents vcell val)
    val))

(define (*set-value env id val)
  (set-contents (reluctantly-bind env id) val)
  val)

(define (reluctantly-bind env id)
  (cond ((env-lookup env id nil nil))
	(else
	 (env-warning "Binding" id)
	 (let ((vcell (env-lookup env id t t)))
	   (distribute-vcells vcell)
	   vcell))))
		

(define (*bound? env id)                ; ugh
  (cond ((env-lookup env id nil nil)
         => (lambda (vc)
              (or (not (vcell? vc))
                  (not (nonvalue? (vcell-contents vc))))))
        (else nil)))

;;; env-lookup is defined by open.t to be effectively the same as call.

(define-operation (env-superior env))
(define-predicate environment?)
(define-operation (virtual-vcell-table env))

;;; super is any environment, not necessarily a locale.

(define (make-locale super . maybe-id)
  (really-make-locale super
                      (if (null? maybe-id) nil (car maybe-id))
                      nil))
                                                
(define (make-inferior-locale superior name)
  (let ((inferior-env (make-locale superior name)))
    (*define superior name inferior-env)
    inferior-env))


(define (make-locale-table id boot-table)
  (let ((symbol-table   (if boot-table boot-table (make-symbol-table id)))
        (other-id-table (make-table id)))
    (object (lambda (id)
              (if (symbol? id)
                  (table-entry symbol-table id)
                  (table-entry other-id-table id)))
      ((setter self)
       (lambda (id val)
         (if (symbol? id)
             (set (table-entry symbol-table id) val)
             (set (table-entry other-id-table id) val))))
      ((locale-walk self proc)
       (table-walk symbol-table proc)
       (table-walk other-id-table proc)))))

;;; Syntax table below should be a delay, but delay isn't
;;; available in the VM.

(define (really-make-locale super id boot-table)
  (let ((locale-table       (make-locale-table id boot-table))
        (syntax-table       '#f)
        (loaded-files-table (make-string-table 'loaded-files))
        (lpop               '#f)
        (virtual-vcells     (make-table `(virtual-vcells ,id))))
    (labels
     ((env (object
             (lambda (id local? create?)
               (cond ((locale-table id))
                     (local?
                      (cond (create?                                     
                             (let ((new (make-vcell id)))
                               (fix-virtual-vcells env new)
                               (cond ((and super (super id nil nil))
                                      => (lambda (vcell)
                                           (env-warning "Shadowing" id)
                                           (fix-location-lists env vcell new)
					   (update-vcell-header-byte vcell))))
                               (set (locale-table id) new)
			       (update-vcell-header-byte new)
                               new))       
                            (else nil)))
                     ((and super (super id nil nil)))
                     (create?                         
                      (cond ((table-entry virtual-vcells id))
                            (else
                             (let ((vcell (make-vcell id)))
                               (set (table-entry virtual-vcells id) vcell)
                               vcell))))
                     (else nil)))
             ((env-superior self) super)
             ((env-syntax-table self)
              (cond (syntax-table)
                    (else
                     (set syntax-table 
                          (make-syntax-table
                           (if super (env-syntax-table super) nil)
                            id))
                     (set (env-for-syntax-definition syntax-table) env)
                     syntax-table)))
             ((loaded-files self) loaded-files-table)
             ((loaded-file self id)
              (let ((unit (table-entry loaded-files-table id)))
                (cond (unit => identity)
                      (else
                       (let ((superior (env-superior self)))
                         (if superior (loaded-file superior id) '#f))))))
             ((set-loaded-file self id unit)
              ;++ we should be saving the file write date and checking it.
              (set (table-entry loaded-files-table id) unit))
             ((virtual-vcell-table self) virtual-vcells)
             ((locale-inferiors self)
              (if (null? lpop)
                  (set lpop (make-population `(locale-inferiors ,self))))
              lpop)
             ((locale-walk self proc)
              (locale-walk locale-table proc))
             ((locale? self) t)
             ((print-info self) id)
             ((set-identification self val) (if (not id) (set id val)))
             ((get-environment self) self)
             ((get-loaded-file self)
              (get-loaded-file (env-superior self)))
             ((environment? self) t)
             ((print-type-string self) "Locale"))))
      (if super
          (add-to-population (locale-inferiors (env-locale super)) env)
          (add-to-population top-level-environments env))
;++ this seems pretty random
      (cond ((symbol? id)
             (let ((vcell (make-vcell id)))
               (set (vcell-contents vcell) env)
               (set (locale-table id) vcell))))
      env)))

(define (update-vcell-header-byte vcell)
  (if (fx= (vector-length (weak-alist-elements (vcell-locations vcell))) 0)
      (set (mref-8-u vcell -2) 0)
      (set (mref-8-u vcell -2) -1)))

(define (distribute-vcells vcell)
  (let ((alist (vcell-vcell-locations vcell)))
    (let* ((vec (weak-alist-elements alist))
	   (len (vector-length vec)))
      (do ((i 0 (fx+ i 2)))
	  ((fx>= i len))
	(set (extend-elt (vref vec i) (vref vec (fx+ i 1))) vcell)))))

;;; Move ENV units from OLD-VCELL to NEW-VCELL if the variable is not
;;; bound in ENV.

(define (fix-location-lists env old-vcell new-vcell)
  (cond ((not (env-lookup env (vcell-id old-vcell) t nil))
         (weak-alist-move (vcell-locations old-vcell)
                          (vcell-locations new-vcell)
                          (lambda (unit)
                            (eq? env (unit-env unit))))
         (weak-alist-move (vcell-vcell-locations old-vcell)
                          (vcell-vcell-locations new-vcell)
                          (lambda (unit)
                            (eq? env (unit-env unit))))
         (walk-population (locale-inferiors env)
                          (lambda (env)
                            (fix-location-lists env old-vcell new-vcell))))))


;;; If ENV does not bind the variable and has a virtual-vcell for it the
;;; virtual-vcell's locations are added to VCELL's.

(define (fix-virtual-vcells env vcell)
  (cond ((not (env-lookup env (vcell-id vcell) nil nil))
         (let ((table (virtual-vcell-table env)))
           (cond ((table-entry table (vcell-id vcell))
                  => (lambda (v)
                       (weak-alist-merge (vcell-locations v)
                                         (vcell-locations vcell))
                       (weak-alist-merge (vcell-vcell-locations v)
                                         (vcell-vcell-locations vcell))
                       (set (table-entry table (vcell-id vcell)) nil)))))
         (walk-population (locale-inferiors env)
                          (lambda (env)
                            (fix-virtual-vcells env vcell))))))
              
(define top-level-environments
        (make-population 'top-level-environments))

(define (env-locale env)
  (do ((e env (env-superior e)))
      ((locale? e) e)))

(define (make-empty-locale . maybe-id)
  (make-locale nil (if (null? maybe-id) nil (car maybe-id))))
