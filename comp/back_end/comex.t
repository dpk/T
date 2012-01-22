(herald (back_end comex)
  (env t (orbit_top defs) (back_end closure)))

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

(define (get-template-definer l)
  (iterate loop ((l l))
   (let ((node (node-parent l)))
    (cond ((not node) 0)
          ((and (eq? (lambda-strategy l) strategy/heap)
                (continuation? l))
           0)
          ((or (primop-ref? (call-proc node) primop/*define)
	       (primop-ref? (call-proc node) primop/*lset))
           (let ((offset (cdr (ass (lambda (x y)
                                 (and (loc-list? y)
                                      (eq? x (loc-list-var y))))
                              (leaf-value ((call-arg 2) node))
                              (closure-env *unit*)))))
             (fx/ offset 4)))
          (else 
           (loop (node-parent node)))))))

(define (template-has-superior? node)
  (xselect (lambda-strategy node)
    ((strategy/stack)                                          
     (if (closure-vframe-lambdas (environment-closure (lambda-env node))) 0 1))
    ((strategy/heap strategy/hack) 0)))

(define-structure-type lap-template-struct
  pointer
  scratch
  nargs
  handler-tag
  strategy
  instructions)


(define (generate-lap-template node)
  (destructure (((#f i-node) (call-args node)))
    (let ((tem (make-lap-template-struct))
          (i-stream (leaf-value i-node)))
      (destructure (((pointer scratch nargs nary? strategy tag) (car i-stream)))
        (set (lap-template-struct-pointer tem) (eval pointer orbit-env)) ; arghh
        (set (lap-template-struct-scratch tem) (eval scratch orbit-env))
        (set (lap-template-struct-nargs tem) (cons nargs nary?))
        (set (lap-template-struct-strategy tem)
             (if (eq? strategy 'stack) 0 1))
        (set (lap-template-struct-handler-tag tem) tag)
        (set (lap-template-struct-instructions tem) (cdr i-stream))
        (lambda-queue tem)
        (free-register node AN)    ; where set (define) code expects
        (generate-move-address (template tem) AN)
        (mark-continuation node AN)))))

(define (process-lap-template tem)
  (emit-template tem (lap-template-struct-handler-tag tem))
  (set *lambda* (car (find (lambda (pair) (lambda-node? (car pair)))
                           (closure-env *unit*))))
  (lap-transduce (lap-template-struct-instructions tem))
  (process-lambda-queue))                                                                                

(define (create-comex filename h unit templates thing code)
  (if (fx>= (bytev-length code) 65536)
      (user-message-without-location 'error "Object file was too big~%" '#f))
  (let ((size (fx+ (fx+ (length unit) 4) (fx* (length templates) 2))) ; hack,
        (comex (make-comex)))                                         ; template
    (receive (objects opcodes)                                        ; in both
             (create-obj-op-vectors thing unit size filename h)
      (set (comex-module-name comex) version-number)
      (set (comex-code comex) code)
      (set (comex-objects comex) objects)
      (set (comex-opcodes comex) opcodes)           
      (set (comex-annotation comex) nil)
      comex)))

(define (create-obj-op-vectors thing unit size filename h)
  (let ((objects (make-vector size))
        (opcodes (make-bytev size)))
    (set (bref opcodes 0) op/literal)                         
    (vset objects 0 (->compiler-filename filename))
    (set (bref opcodes 1) op/literal)                         
    (vset objects 1 h)                       
    (set (bref opcodes 2) op/literal)                         
    (vset objects 2 'unit-env)                  
    (set (bref opcodes 3) op/closure)
    (vset objects 3 (code-vector-offset thing))
    (iterate loop ((a-list unit) (i 4))         
      (cond ((null? a-list)
             (return objects opcodes))
            ((closure? (caar a-list))
             (vset objects i
                   (code-vector-offset (cit->lambda (caar a-list))))
             (set (bref opcodes i) op/template1)
             (set (bref opcodes (fx+ i 1)) op/template2)
             (set (bref opcodes (fx+ i 2)) op/template3)
             (loop (cdr a-list) (fx+ i 3)))
            (else
             (receive (opcode obj) (comex-decipher (caar a-list))
               (vset objects i obj)
               (set (bref opcodes i) opcode)
               (loop (cdr a-list) (fx+ i 1))))))))


(define (->compiler-filename fn)
  (list (cond ((filename-fs fn))
              (else (fs-name (local-fs))))
        (filename-dir fn)
        (filename-name fn)
        (cond ((filename-type fn))
              (else 't))))




(define (comex-decipher obj)
  (cond ((foreign-name obj)
         => (lambda (name) (return op/foreign name)))
        ((and (node? obj) (lambda-node? obj))
         (return op/closure (code-vector-offset obj)))
        ((loc-list? obj)
         (vcell-status (loc-list-var obj)))
        ((not (variable? obj))
         (return op/literal obj))
        (else
         (return op/variable-value (variable-name obj)))))

(define (vcell-status var)
  (let ((name (variable-name var)))
    (cond ((not (defined-variable? var))
	   (return op/vcell name))
	  (else
	   (case (defined-variable-variant var)
	     ((set) (return op/vcell name))
	     ((lset) (return op/vcell-lset name))
	     (else
	      (let ((l (defined-variable-value var)))
		(cond ((and l
			    (let ((node ((call-arg 3) (node-parent l))))
			      (and (lambda-node? node)
				   (assq node (closure-env *unit*)))))
		       => (lambda (pair)
			    (return op/vcell-stored-definition
				    (cons name (cdr pair)))))
		      (else
		       (return op/vcell-defined name))))))))))

(define (cit->lambda closure)
  (variable-binder (car (closure-members closure))))

(define (static var-name)
  (let* ((a-list (closure-env *unit*))
         (val (ass (lambda (name var)
                     (and (loc-list? var)
			  (eq? (variable-name (loc-list-var var)) name)))
                   var-name
                   a-list)))
    (cond (val
           (fx- (cdr val)
                (fx+ (cond ((assq *lambda* (cddr a-list))
                            => cdr)
                           (else
                            (cdr (last a-list))))
                      tag/extend)))
          (else
           (error "static value not mentioned ~s" var-name)))))


(define (template-nary l)
  (xcond ((lambda-node? l)                             
          (cond ((object-lambda? l)
                 (lambda-rest-var ((call-arg 2) (lambda-body l))))
                (else       
                 (or (eq? (lambda-strategy l) strategy/vframe)
                     (eq? (lambda-strategy l) strategy/ezclose)
                     (lambda-rest-var l)))))
         ((lap-template-struct? l)
          (cdr (lap-template-struct-nargs l)))))


(define (get-template-annotation l)
  (xcond ((lambda-node? l)
          
(fx+ (fixnum-ashl (get-template-definer l) 3)
  (fx+ (fixnum-ashl (template-has-superior? l) 2)
     (fx+ (fixnum-ashl (if (eq? (lambda-strategy l) strategy/stack) 0 1) 1)
          (if (fxn= (environment-cic-offset (lambda-env l)) 0) 1 0)))))

         ((lap-template-struct? l)
     (fixnum-ashl (lap-template-struct-strategy l) 1))))

          
(define (get-template-cells l)
  (cond ((lap-template-struct? l)
          (fx+ (fixnum-ashl (lap-template-struct-pointer l) 8)
               (lap-template-struct-scratch l)))
         ((environment? (lambda-env l))
          (let ((offset (environment-cic-offset (lambda-env l))))
            (cond ((fxn= offset 0) offset)
                  (else
                   (let ((closure (environment-closure (lambda-env l))))
                     (fx+ (fixnum-ashl (closure-pointer closure) 8)
                          (closure-scratch closure)))))))
         (else 0)))
          

(define (get-template-nargs l)
  (xcond ((lambda-node? l)
          (select (lambda-strategy l)
            ((strategy/stack)
             (fx- 0 (fx+ (length (lambda-variables l)) 1)))
            ((strategy/vframe strategy/ezclose) -1)
            (else
             (cond ((object-lambda? l)
                    (let ((proc ((call-arg 2) (lambda-body l))))
                      (if (primop-ref? (call-proc (lambda-body proc))
                                       primop/undefined-effect)
                          0
                          (length (lambda-variables proc)))))     
                   (else
                    (length (lambda-variables l)))))))
         ((lap-template-struct? l)
          (car (lap-template-struct-nargs l)))))
