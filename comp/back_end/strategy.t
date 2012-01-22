(herald (back_end strategy)
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

(define (variable-known var)
  (if (not (variable? var))
      nil
      (let ((type (variable-type var)))
        (cond ((and (node? type) (lambda-node? type))
               type)
              (else nil)))))

(define (variable-y-lambda var)
  (node-parent (node-parent (variable-binder var))))

(define (object-lambda? node)
  (and (lambda-node? node)
       (primop-ref? (call-proc (lambda-body node)) primop/proc+handler)))

(define (let-lambda? l)
  (lambda-node? (call-proc (node-parent l))))

(define (call-in-body? proc node)
  (fx> (lambda-trace proc) (lambda-trace (node-parent node))))

(define-local-syntax (define-lambda-strategies . strategies)
  `(block ,@(map! (lambda (strat)      
                    (let ((strat (concatenate-symbol 'strategy/ strat)))
                      `(define-constant ,strat ',strat)))
                   strategies)))


(define-lambda-strategies open label ezclose vframe stack heap hack)

(define (set-lambda-strategy! node)
  (cond ((lambda-strategy node))
        (else                
         (set (lambda-strategy node)
              (let* ((parent (node-parent node))
                     (proc   (call-proc parent)))
                (cond ((or (and (fx<= 2 (call-exits parent))
                                (call-exit? node))
                           (and (call-exit? node)
                                (primop-node? proc)))
                       strategy/open)
                      ((call-exit? node)
                       strategy/stack)
                      ((cons-on-stack? proc (call-arg-number (node-role node)))
                       strategy/hack)
                      (else
                       strategy/heap)))))))

(define (cons-on-stack? proc number) '#f)
   
(define db cons)
(define (lambda-depth lam) (car (lambda-db lam)))
(define (lambda-trace lam) (cdr (lambda-db lam)))

(define (analyze-top node)
  (analyze-lambda ((call-arg 1) (lambda-body node)) 0 0))
                                   
(define (analyze-lambda node depth -trace)
  (set (lambda-db node) (db depth -trace))
  (set-lambda-strategy! node)
  (let ((tr (analyze-body (lambda-body node) depth -trace)))
    (walk sort-by-db (if (continuation? node)
                         (lambda-variables node)
                         (cdr (lambda-variables node))))
    (fx+ tr 1)))

                             
(define (analyze-body node depth -trace)
  (let ((proc (call-proc node)))
    (cond ((primop-node? proc)
           (select (primop-value proc) 
             ((primop/conditional)
              (analyze-if node depth -trace))
             ((primop/Y) 
              (analyze-Y ((call-arg 1) node) ((call-arg 2) node) depth -trace))                
             ((primop/undefined-effect) -trace)
             (else
              (really-analyze-body (call-args node) depth -trace))))
          ((lambda-node? proc)
           (analyze-let node depth -trace))   
          (else  
           (really-analyze-body (call-args node) depth -trace)))))


(define (really-analyze-body args depth -trace)
  (iterate loop ((-trace -trace) (args args))
    (cond ((null? args) -trace)
          ((lambda-node? (car args))             
           (loop (analyze-lambda (car args) (fx+ depth 1) -trace) 
                 (cdr args)))
          (else
           (loop -trace (cdr args))))))


(define (analyze-if node depth -trace)
  (receive (trac other) (determine-if-trace ((call-arg 1) node) ((call-arg 2) node))
    (let ((-trace (if (lambda-node? trac)
                       (analyze-lambda trac (fx+ depth 1) -trace)
                       -trace)))
      (if (lambda-node? other)
          (analyze-lambda other (fx+ depth 1) -trace)
          -trace))))
                                      

(define (determine-if-trace th el)
  (cond ((leaf-node? th)
         (return el th))
        ((leaf-node? el)
         (return th el))
        (else
         (let ((th-body (lambda-body th))
               (el-body (lambda-body el)))
           (cond ((fx= (call-exits th-body) 0)
                  (if (and (leaf-node? (call-proc th-body))
                           (variable-known (leaf-value (call-proc th-body))))
                      (return th el)
                      (return el th)))
                 ((fx= (call-exits el-body) 0)
                  (if (and (leaf-node? (call-proc el-body))
                           (variable-known (leaf-value (call-proc el-body))))
                      (return el th)
                      (return th el)))
                 ((primop-node? (call-proc th-body))
                  (return th el))
                 (else 
                  (return el th)))))))
             

(define (analyze-let let-node depth -trace)
  (if (lambda-rest-var (call-proc let-node)) 
      (bug "nary-let not implemented yet"))
  (let ((lambdas (call-proc+args let-node)))
    (set (lambda-strategy (car lambdas)) strategy/open)
    (walk set-let-strategy!
          (lambda-variables (car lambdas))
          (cdr lambdas))            
    (analyze-lambda (car lambdas) (fx+ depth 1) -trace)
    (let ((lambdas (filter lambda-node? (cdr lambdas))))
      (cond ((null? lambdas) (fx+ -trace 1))
            ((and (null? (cdr lambdas))
                  (continuation? (car lambdas)))
             (let ((tr (analyze-lambda (car lambdas) (fx+ depth 1) (fx+ -trace 1))))
               (if (stack-below? (car lambdas))
                   (set (lambda-strategy (car lambdas)) strategy/stack))
               tr))
            (else                  
             (really-analyze-body lambdas (fx+ depth 1) (fx+ -trace 1)))))))

(define (set-let-strategy! var arg)
  (cond ((and var (lambda-node? arg))
         (set (variable-type var) arg)
         (set (lambda-strategy arg)
              (cond ((and (all-refs-are-calls? var) 
                          (not (and (lambda-rest-var arg)
                                    (used? (lambda-rest-var arg)))))
                     strategy/label)
                    ((continuation? arg)       
                     strategy/stack)
                    (else 
                     strategy/heap))))))
       

(define (analyze-Y cont master depth -trace)
  (let* ((lambdas (call-args (lambda-body master)))
         (strategy (get-labels-strategy master)))
    (walk (lambda (var l) 
            (set (lambda-strategy l) strategy)
            (if var (set (variable-type var) l)))
          (cdr (lambda-variables master))
          (cdr lambdas))                                  
    (set (lambda-strategy master) strategy)
    (set (lambda-strategy (car lambdas)) strategy/open)
    (let ((tr (cond ((lambda-node? cont)
                     (set (lambda-strategy cont) strategy/stack)
                     (analyze-lambda cont (fx+ depth 1) -trace))
                    (else
                     -trace))))
      (really-analyze-body lambdas (fx+ depth 1) tr))))
          

(define (get-labels-strategy master)
  (cond ((or (not (every? all-refs-are-calls? (cdr (lambda-variables master))))
             (any? lambda-rest-var (call-args (lambda-body master))))
         strategy/heap)
        ((and (need-to-pop-stack? (node-parent master))
              (not (constant-continuation? master)))
         strategy/vframe)
        (else
         strategy/label)))

(define (vframe-allowed? l)
  (eq? (lambda-db (node-parent (node-parent l))) 'vframe))
                                                             
(define (vframe-or-ezclose master)
  (cond ((constant-continuation? master)
         'ezclose)
        ((vframe-possible? master) 
         'vframe)
        (else 'label)))

(define (ezclose-allowed? l)
  (eq? (lambda-db (node-parent (node-parent l))) 'ezclose))

(define (sort-by-db var)
 (if var
  (set (variable-refs var)
       (sort-list! (variable-refs var)
              (lambda (ref1 ref2)
                (let ((l1 (node-parent (node-parent ref1)))
                      (l2 (node-parent (node-parent ref2))))
                  (cond ((fx< (lambda-trace l1) (lambda-trace l2)) t)
                        ((fx> (lambda-trace l1) (lambda-trace l2)) nil)
                        (else
                         (fx<= (lambda-depth l1) (lambda-depth l2))))))))))
              
                 
(define (stack-below? node)
  (if (eq? (node-role (node-parent (node-parent node))) call-proc)
      '#f
      (let ((body (lambda-body node)))
        (select (call-exits body)
          ((0) nil)
          ((1) (let ((exit (car (call-args body))))
                 (xcond ((lambda-node? (call-proc body))
                         (stack-below? (call-proc body)))
                        ((leaf-node? exit) nil)
                        ((eq? (lambda-strategy exit) strategy/stack) t)
                        ((eq? (lambda-strategy exit) strategy/open)
                         (stack-below? exit)))))
          ((2) (let ((exit1 ((call-arg 1) body))
                     (exit2 ((call-arg 2) body)))
                 (and (and (lambda-node? exit1) (stack-below? exit1))
                     (and (lambda-node? exit2) (stack-below? exit2)))))))))

(define (constant-continuation? node)
  (every? (lambda (var)
            (every? (lambda (ref)
                      (let ((cont ((call-arg 1) (node-parent ref))))
                        (and (leaf-node? cont) 
                             (labels-lambda? 
                              (variable-binder (leaf-value cont))))))
                    (variable-refs var)))
          (cdr (lambda-variables node))))
          
(define (vframe-possible? master)
  (every? (lambda (l)
            (every? (lambda (ref)
                      (or (eq? (node-role ref) call-proc)
                          (let ((proc (call-proc (node-parent ref))))
                            (or (primop-node? proc)
                                (vframe-call-ok? (reference-variable proc) master)))))
                    (variable-refs (lambda-cont-var l))))
          (cdr (call-args (lambda-body master)))))
                
(define (vframe-call-ok? var master) 
  (cond ((not (variable-binder var)) '#t)
        ((variable-known var)
         => (lambda (l)
              (eq? (node-parent (node-parent l)) master)))
        (else '#t)))



(define (labels-lambda? node)
  (labels-master-lambda? (node-parent (node-parent node))))

(define (labels-master-lambda? node)
  (and (eq? (node-role node) (call-arg 2))
       (primop-ref? (call-proc (node-parent node)) primop/y)))


