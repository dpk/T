(herald (back_end reg)
  (env t (orbit_top defs) (back_end closure) (back_end bookkeep)))

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

(define (generate-init continuation)
  (bind ((*unit-literals* '())
         (*unit-variables* '())
         (*unit-closures* '())
         (*unit-templates* '())
         (*unit* nil) 
         (*registers* (vector-fill (make-vector *no-of-registers*) nil))
         (*lambda* nil)
         (*stack-pos* 0)
         (*locations* (make-table 'locations))
         (*lambda-queue* '()))
    (continuation)))


(define (generate top-node)
  (generate-code (car (call-args (lambda-body top-node)))))


(lset *assembly-comments?* nil)
(lset *lambda-queue* '())         ;; queue of lambda bodies to process
(lset *stack-pos* 0)              ;; distance of stack-pointer from "frame"
(lset *max-temp* 0)               ;; maximum number of temporaries used
(lset *lambda* nil)               ;; the procedure being compiled
(lset *call-break?* nil)           
(lset *registers* nil)

(define-local-syntax (ass-comment string . rest)
  `(if *assembly-comments?*
       (emit-comment (format nil ,string ,@rest))))                      

;;; GENERATE-CODE Initialize lambda queue. Go.

(define (generate-code node)
  (set *stack-pos* 0)
  (allocate-registers node)                                          
  (process-lambda-queue))

(define (generate-code-for-object node)
  (set *stack-pos* 0)            
  (set *lambda* node)
  (let ((object-proc ((call-arg 2) (lambda-body node))))
    (mark-first-continuation (lambda-body object-proc))
    (emit-template node object-proc)
    (if (closure-env (environment-closure (lambda-env node)))
        (mark (lambda-self-var node) P))
    (mark-vars-in-regs (cdr (lambda-variables object-proc)))
    (if (n-ary? object-proc)
        (n-ary-setup object-proc))
    (allocate-call (lambda-body object-proc))
    (emit-tag object-proc))
  (generate-handler node)
  (process-lambda-queue))


(define (lambda-queue node)
  (push *lambda-queue* node))

(define (process-lambda-queue)
  (if *lambda-queue*
      (let ((thing (pop *lambda-queue*)))
        (xcond ((object-lambda? thing)
                (generate-code-for-object thing))
               ((lambda-node? thing)     
                (if (neq? (lambda-strategy thing) strategy/stack) 
                    (mark-first-continuation (lambda-body thing)))
                (generate-code thing))
               ((lap-template-struct? thing)
                (process-lap-template thing))))))

(define (mark-first-continuation node)
  (walk mark-first-continuation-1 (call-proc+args node)))


(define (mark-first-continuation-1 node)
  (cond ((lambda-node? node)
         (select (lambda-strategy node)
           ((strategy/stack)
            (set (closure-vframe-lambdas 
                       (environment-closure (lambda-env node))) t))
           ((strategy/open)                                        
            (mark-first-continuation (lambda-body node)))))))

;;; ALLOCATE-REGISTERS Sets *lambda* to be the lambda-node representing the
;;; environment the node argument is compiled in.  Generate code for the body.

(define (allocate-registers node)
    (select (lambda-strategy node)
      ((strategy/stack strategy/heap strategy/hack)
       (set *lambda* node)
       (emit-template node node))
      ((strategy/vframe strategy/ezclose)
       (set *lambda* (node-parent (node-parent node)))
       (emit-tag node))
      (else
       (set *lambda* (variable-binder (join-point-contour (lambda-env node))))
       (emit-tag node)))
    (initialize-registers node)
    (if (n-ary? node)
        (n-ary-setup node))
    (allocate-call (lambda-body node)))
    
;;; INITIALIZE-REGISTERS Here we mark the arguments of a closure as being in
;;; the argument registers.  For a heaped lambda there is also the environment
;;; in the P register.  For a join point the state is initialized.

(define-integrable (method-lambda node)
  (let ((p (node-parent node)))
    (if (primop-ref? (call-proc p) primop/proc+handler)
        (node-parent p)
        nil)))
   
(define (initialize-registers node)
  (xselect (lambda-strategy node)
    ((strategy/heap strategy/hack)                                       
     (ass-comment "Procedure ~s (lambda ~s ...)" 
             (lambda-name node)
             (append! (map variable-unique-name (lambda-variables node))
                      (cond ((lambda-rest-var node) => variable-unique-name)
                            (else '()))))
     (cond ((method-lambda node)
            => (lambda (obj)
                 (mark (lambda-self-var obj) P)
                 (set *lambda* obj)))
           (else
            (mark (lambda-self-var node) P)))
     (mark-vars-in-regs (cdr (lambda-variables node))))
    ((strategy/stack)
     (ass-comment "Continuation ~s (lambda ~s ...)"
             (lambda-name node)
             (append! (map variable-unique-name (lambda-variables node))
                      (cond ((lambda-rest-var node) => variable-unique-name)
                            (else '()))))
     (mark-vars-in-regs (lambda-variables node)))
    ((strategy/vframe)
     (ass-comment "Procedure ~s (lambda ~s ...)" 
             (lambda-name node)
             (map variable-unique-name (lambda-variables node)))
     (mark (lambda-self-var *lambda*) P)
     (mark-vars-in-regs (cdr (lambda-variables node))))
    ((strategy/ezclose)
     (ass-comment "Procedure ~s (lambda ~s ...)" 
             (lambda-name node)
             (map variable-unique-name (lambda-variables node)))
     (mark-vars-in-regs (cdr (lambda-variables node))))
    ((strategy/label)
     (ass-comment "Label procedure ~s (lambda ~s ...)" 
             (lambda-name node)
             (map variable-unique-name (lambda-variables node)))
     (cond ((join-point-contour-needed? (lambda-env node))
            (let ((contour (join-point-contour (lambda-env node))))
              (mark contour P)
              (if (closure-cit-offset (environment-closure 
                        (lambda-env (variable-binder contour))))
                  (generate-move (reg-offset P -2) TP)))))
     (walk (lambda (var arg-spec)
             (mark var (car arg-spec)))
          (if (continuation? node)
              (lambda-variables node)
              (cdr (lambda-variables node)))
          (join-point-arg-specs (lambda-env node)))
     (walk (lambda (pair)
             (mark (cdr pair) (car pair)))
           (join-point-global-registers (lambda-env node))))))



(define (mark-vars-in-regs vars)
  (do ((vars vars (cdr vars))
       (reg A1 (fx+ reg 1)))
      ((or (fx>= reg AN) (null? vars))
       (do ((vars vars (cdr vars))
            (reg (fx+ reg (fx+ *argument-registers* 1)) (fx+ reg 1)))
           ((null? vars))
         (cond ((and (car vars) (variable-refs (car vars)))
                (mark-temp (car vars) reg)))))
    (cond ((and (car vars) (variable-refs (car vars)))
           (mark (car vars) reg)))))
     
;;; A closure is n-ary if it has a non null rest arg.

(define n-ary? lambda-rest-var)

(define (n-ary-setup node)
  (cond ((used? (lambda-rest-var node))
         (generate-nary-setup node
                              (if (eq? (lambda-strategy node) strategy/stack)
                                  (length (lambda-variables node))
                                  (length (cdr (lambda-variables node))))))))



(define (allocate-primop-call node)
  (let* ((prim (primop-value (call-proc node))))
    (cond ((primop.conditional? prim)
           (allocate-conditional-primop node prim))
          ((and (eq? prim primop/contents-location)
                (neq? (leaf-value ((call-arg 2) node)) primop/cell-value))
           (allocate-location node prim))
          ((primop.special? prim)
           (primop.generate prim node))
          (else           
           (really-allocate-primop-call node prim)))))
                                       

;;; ALLOCATE-CONDITIONAL-PRIMOP When we come to a split we save the state of
;;; the world and traverse one arm, then restore the state and traverse the
;;; other.

(define (allocate-conditional-primop node prim)
  (primop.generate prim node)      
  (let ((then (then-cont node))
        (else (else-cont node)))
  (receive (then else) (cond ((or (leaf-node? then) 
                                  (leaf-node? else) 
                                  (fx< (lambda-trace then)
                                       (lambda-trace else)))
                              (return then else))
                             (t
                              (return else then)))
    (bind ((*registers* (copy-registers))
           (*stack-pos* *stack-pos*)
           (*lambda* *lambda*)) 
      (emit-tag then)  
      (cond ((lambda-node? then)
             (walk (lambda (n)
                     (kill-if-dead n then))
                   (cons else (cddr (call-args node))))
             (allocate-call (lambda-body then)))
            (t
             (allocate-conditional-continuation node then)))
      (return-registers))
    (restore-slots)
    (emit-tag else)  
    (cond ((lambda-node? else)
           (walk (lambda (n)
                   (kill-if-dead n else))
                 (cons then (cddr (call-args node))))
           (allocate-call (lambda-body else)))
          (t
           (allocate-conditional-continuation node else))))))
                                        
;; We must decide whether to try to delay dereferencing the location.
;; We do this if the value is used just once and in the next frob and
;; is an operand to a primop.


(define (allocate-location node prim)
  (let ((c (cont node)))
    (if (and (lambda-node? c)
             (let ((refs (variable-refs (car (lambda-variables c)))))
               (and refs
                    (null? (cdr refs))
                    (let ((p (node-parent (node-parent (car refs)))))
                      (or (and (eq? p c) 
                               (let ((proc (call-proc (lambda-body c))))
                                 (and (primop-node? proc)
                                      (neq? (primop-value proc) 
                                            primop/make-cell))))
                          (and (eq? (node-parent (node-parent p)) c)     
                               (let ((proc (call-proc (node-parent (car refs)))))
                                 (and (primop-node? proc)
                                      (neq? (primop-value proc)
                                            primop/contents-location)))                                                        
                               (let ((p (call-proc (lambda-body c))))
                                 (and (primop-node? p)
                                      (eq? (primop-value p) 
                                           primop/contents-location))))))
                    (reps-compatable? 
                      (primop.rep-wants (leaf-value ((call-arg 2) node)))
                      (variable-rep (car (lambda-variables c)))))))
        (generate-location-access node)
        (really-allocate-primop-call node prim))))


(define (reps-compatable? accessor-rep use-rep)
  (and (eq? (rep-size accessor-rep) (rep-size use-rep))
       (not (rep-converter accessor-rep use-rep))))

(define (really-allocate-primop-call node prim)
  (let ((c (cont node)))
    (cond ((lambda-node? c)
           (cond ((call-hoisted-cont node)
                  => (lambda (cont)
                       (walk (lambda (a-pair)
                               (or (memq? (car a-pair) (lambda-live c))
                                   (fx= (variable-number (car a-pair)) 0)
                                   (any? (lambda (node)
                                           (and (leaf-node? node)
                                                (eq? (leaf-value node) (car a-pair ))))
                                         (cdr (call-args node)))
                                   (kill (car a-pair))))
                             (closure-env (environment-closure (lambda-env cont)))))) )
           (primop.generate prim node)
           (walk (lambda (node)
                   (kill-if-dead node c))
                 (cdr (call-args node)))
           (allocate-call (lambda-body c)))
          (else                            
           (primop.generate prim node)
           (walk (lambda (node)
                   (if (leaf-node? node) (kill (leaf-value node))))
                 (cdr (call-args node)))
           (restore-continuation node c)
           (clear-slots)
           (let ((j (variable-known (leaf-value c))))
             (if (and j (not (n-ary? j))) 
                 (generate-jump j)
                 (generate-return (primop.values-returned prim))))))))

(define (access/make-closure node lam)
  (let* ((closure (environment-closure (lambda-env lam))))
    (cond ((eq? closure *unit*)
           (lambda-queue lam)
           (lookup node lam nil))
          (else
           (make-heap-closure node closure)
           nil))))


(define-local-syntax (dotimes spec . body)
  (let ((index (car spec))
        (limit (cadr spec)))
    `(do ((,index 0 (fx+ ,index 1)))
         ((fx= ,index ,limit))
       ,@body)))


;;; MAKE-STACK-CLOSURE Push a continuation on the stack.  For now there are no
;;; scratch values.  When there are we will need to push zeroes for all the
;;; scratch slots and fill them in after pushing the template.  This is because
;;; the GC assumes that anything on top of the stack until the first template
;;; is a valid pointer.

(define (make-stack-closure node cont)
  (let* ((closure (environment-closure (lambda-env cont)))
         (members (closure-members closure))
         (a-list (cdr (closure-env closure))))
    (walk (lambda (x)
            (lambda-queue (variable-binder x)))
          members)
    (do ((i (closure-scratch closure) (fx- i 1)))
        ((fx<= i 0))
      (generate-push (machine-num 0)))
    (walk (lambda (pair)                                          
            (let ((var (car pair)))
              (if (memq? var members)
                  (generate-push-address (template (variable-binder var)))
                  (generate-push (access-value node var)))))
          (reverse! (sublist a-list 0 (closure-pointer closure))))
    (generate-push-address (template cont))
    (walk (lambda (pair)
            (really-rep-convert node
                                (access-value node (car pair))
                                (variable-rep (car pair))
                                (reg-offset SP (cdr pair))
                                (variable-rep (car pair))))
          (nthcdr a-list (closure-pointer closure)))))
                                                            

(define (make-vframe-closure node l closure)
  (walk lambda-queue (closure-vframe-lambdas closure))
  (let ((a-list (cdr (closure-env closure))))
    (do ((i (closure-scratch closure) (fx- i 1)))
        ((fx<= i 0))
      (generate-push (machine-num 0)))
    (walk (lambda (pair)
            (generate-push (access-value node (car pair))))
          (reverse! (sublist a-list 0 (closure-pointer closure))))
    (let ((closure (environment-closure (lambda-env l))))
      (generate-push (machine-num (vframe-header (closure-pointer closure)
                                                 (closure-scratch closure)))))
    (walk (lambda (pair)
            (really-rep-convert node
                                (access-value node (car pair))
                                (variable-rep (car pair))
                                (reg-offset SP (cdr pair))
                                (variable-rep (car pair))))
          (nthcdr a-list (closure-pointer closure)))))
                                                            
  
(define (vframe-header p s)
  (+ (fixnum-ashl p 16) (fixnum-ashl s 8) header/vframe))

