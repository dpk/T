(herald load_comex
        (env tsys (osys retrieve)))

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

;;; Operations on loaded files.
;;; Loaded-file is a subtype of compiled-code, at least for now.

(define-operation (loaded-file-source loaded-file)) ;returns a filename or #f
(define-operation (loaded-file-herald loaded-file)) ;always returns a herald

(define-operation (run-compiled-code code env))
(define-operation (relocate-compiled-code code env))
(define-predicate compiled-code?)

;;; Compiled expressions

;++ this should have a target-machine and a target-os field.
(define-structure-type comex
  module-name
  code                  ;Code vector (probably a byte vector)
  objects               ;General vector of interesting objects
  opcodes               ;Byte vector of things to do with the objects
;++  target-cpu
;++  target-os
  annotation)

;;; The OBJECTS and OPCODES vectors are in 1-1 correspondence.  Each
;;; opcode describes what should be done with the corresponding
;;; object.  The result of processing an object/opcode pair gets
;;; stored at the corresponding position when the closure ("unit")
;;; is being created.

;;; (define (run-compiled-code comex env)
;;;   ((map-vector (lambda (op obj)
;;;                  ((vref *op-procedures* op) obj env))
;;;                (comex-objects comex)
;;;                (comex-opcodes comex))))


(define (dump-keys obj)
  (if (comex? obj) 'comex nil))

(define (dump-accessors sym)
  (xcase sym
         ((comex) (stype-selectors comex-stype))))

(define (dump-makers sym)
  (xcase sym
         ((comex) make-comex)))

(define (comex-decoder key)
  (case key
    ((comex) (return make-comex (stype-selectors comex-stype)))
    (else    (return nil nil))))

;++ flush this
(define (read-comex-from-file spec)
  (with-open-ports ((s (open spec 'retrieve)))
    (if (fx= (retrieve-port-magic-number s) retrieve-magic-number)
        (set-decoder s comex-decoder)
        (error "bad magic dump number in ~S" spec))
    (read s)))

(define (read-comex port)
  (let* ((magic (get-bytes port 4))
         (comex (cond ((fx= magic retrieve-magic-number)
                       (let ((port (make-retrieve port)))
                         (set-decoder port comex-decoder)
                         (read port)))
                      (else
                       (error "bad magic dump number in ~S" port)))))
;++ flush
;                     (else
;                      (warning "obsolete dump file ~A~%"
;                               (port-name port))
;                      (close-port port)
;                      (re-open-port! port 'in)
;                      ;++ check for old magic number later
;                      (let ((port (make-old-retrieve port)))
;                        (set-decoders port dump-makers dump-accessors)
;                        (read port))))))
    ;++(let ((name (port-name port)))
    ;++  (if name (set (table-entry loaded-file-table name) comex)))
    comex
    ;++ what about the balancing #\]
    ))

(define (load-comex port env)
  (receive (unit code) (install-comex (read-comex port) env)
    (set (weak-table-entry code-unit-table code) unit)
    (add-to-population code-population code)
    ;; run top-level forms
    ((unit-top-level-forms unit))))

;++ check that target cpu and os are ok.
(define (install-comex comex env)
  (let* ((opcodes (comex-opcodes comex))
         (unit    (comex-objects comex))
         (elts    (vector-length unit))
         (code    (comex-code    comex)))
    (vector->unit! unit)
    (set (unit-env unit) env)        
    (do ((i (fx- elts 1) (fx- i 1)))
        ((fx< i 0)                                                       
         (adjust-unit-names unit)
         (purify! code)
         (return unit code))
      (let ((obj (extend-elt unit i)))
        (xselect (bref opcodes i)
          ((op/literal)                                      
           ;++ compiler bug
           (if (and (string? obj) (not (pure? (string-text obj))))
               (purify! (string-text obj)))
           (set (extend-elt unit i) obj))
          ((op/foreign)            
           (set (extend-elt unit i) (make-foreign-procedure obj)))
          ((op/closure)                   
           (set (extend-elt unit i)  ; obj is code-vector-offset
                (make-pointer code (fixnum-ashr obj 2))))
          ((op/template1)                                     
           (install-template1 unit code obj i))
          ((op/template2 op/template3))
          ((op/vcell-stored-definition)
           (let ((vcell (env-lookup env (car obj) t t)))
             (check-rebinding vcell t install-comex)
             (weak-alist-push! (vcell-vcell-locations vcell) unit i)
	     (distribute-vcells vcell)
             (*set vcell (make-pointer unit 
				       (fx- (fixnum-ashr (cdr obj) 2) 1)))))
          ((op/vcell-defined)
           (let ((vcell (env-lookup env obj t t)))
             (check-rebinding vcell t install-comex)
             (weak-alist-push! (vcell-vcell-locations vcell) unit i)
	     (distribute-vcells vcell)))
	  ((op/vcell-lset)
           (let ((vcell (env-lookup env obj t t)))
             (check-rebinding vcell nil install-comex)
             (weak-alist-push! (vcell-vcell-locations vcell) unit i)
	     (distribute-vcells vcell)))
          ((op/vcell)
           (let ((vcell (env-lookup env obj nil t)))
;             (check-rebinding vcell nil install-comex)
             (weak-alist-push! (vcell-vcell-locations vcell) unit i)
	     (set (extend-elt unit i) vcell)))
          ((op/variable-value)
           (let ((vcell (env-lookup env obj nil t)))
             (weak-alist-push! (vcell-locations vcell) unit i)
	     (set (mref-8-u vcell -2) -1)
	     (let ((value (vcell-contents vcell)))
	       (set (extend-elt unit i)
		    (make-link-snapper (if (nonvalue? value) obj value)
				       unit
				       i))))))))))
          
(define (adjust-unit-names unit)
  (cond ((file-system-present?)
         (let ((h (unit-herald unit))
               (s (unit-source-filename unit)))
           (if (not (herald? h))
               (set (unit-herald unit) (parse-herald (car h) (cdr h))))
           (if (not (filename? s))
               (set (unit-source-filename unit)
                    (apply make-filename s))))))
  (no-value))

;++ This population is currently used only by the GC.  We should
;++ change the GC to use the CODE-UNIT-TABLE
(define code-population (make-population 'code-population))

;;; Initialize *CODE-UNIT-TABLE*.  Weak tables must be available
;;; before this procedure is called.

;++ is the code-unit map really necessary?
(define code-unit-table
  (let ((table  (make-weak-table 'code-unit-table)))
    (walk (lambda (arg)
            (let ((code (car arg))
                  (unit (cdr arg)))
              (set (weak-table-entry table code) unit)))
            *code-unit-map*)
    table))
