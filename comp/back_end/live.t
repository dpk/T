(herald (back_end live)
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

;;; Copyright (c) 1985 David Kranz

(define (analyze top-node)
  (analyze-top top-node)
  (live-analyze-top top-node)
  (collect-top top-node)
  (bind ((*noise-flag* t))
    (print-variable-info *unit-variables*))
  (type-analyze-top top-node)
  (rep-analyze-top top-node)
  (close-analyze-top top-node nil))



;;; Live variable analysis

(define (live-analyze-top node)
  (live-analyze (car (call-args (lambda-body node)))))
                
     
(define (live-analyze node)
  (cond ((lambda-node? node)
         (if (labels-master-lambda? node)
             (live-analyze-y node)
             (live-analyze-lambda node)))
        ((leaf-node? node)
         (live-analyze-leaf node))
        (else
         (bug "live-analyze called on a call-node ~S" node))))
                                         
(define (live-analyze-lambda node)
  (receive (live global? known) (live-analyze-body (lambda-body node))
   (let* ((live-1 (set-difference live (lambda-all-variables node)))
           (live (if (neq? (node-role node) call-proc)  ;; Let
                     live-1       
                     (set-difference live-1 (map (lambda (node) 
                                            (and (lambda-node? node)
                                                 (lambda-self-var node)))
                                          (call-args (node-parent node)))))))
    (set (lambda-live node) live)
    (select (lambda-strategy node)
      ((strategy/heap)    
       (walk maybe-change-to-heap known)
       (cond ((and (null? live) (not (known-lambda? node)))
              (set (lambda-env node) 'unit-internal-closure)
              (return live t known))
             (global? 
              (set (lambda-env node) 'unit-internal-template)
              (return live t known))
             (else
              (set (lambda-env node) nil)
              (return live nil known))))
      ((strategy/label)                
       (if (and (labels-lambda? node)
                (not (ezclose-allowed? node)))
           (walk change-to-vframe-or-heap (delq node known)))
       (set (lambda-env node) (if global? 'needs-link '#f))
       (return live global? known))
      ((strategy/ezclose)
       (walk maybe-cons-on-stack known)
       (set (lambda-env node) (if global? 'needs-link '#f))
       (return live global? known))
      ((strategy/vframe)
       (walk maybe-change-to-vframe known)
       (set (lambda-env node) (if global? 'needs-link '#f))
       (return live global? known))
      ((strategy/hack)
       (walk change-to-vframe-or-heap known)
       (set (lambda-env node) (if global? 'needs-link '#f))
       (return live global? known))
      ((strategy/stack)           
       (walk maybe-cons-on-stack known)
       (set (lambda-env node) (if global? 'needs-link '#f))
       (hoist-continuation node)
       (return '() nil known))
      (else
       (return live global? known))))))

(define (need-to-pop-stack? y-node)                         
  (and (leaf-node? ((call-arg 1) y-node))
       (iterate loop ((node (node-parent y-node)))
         (cond ((not (continuation? node)) nil) 
               ((eq? (lambda-strategy node) strategy/stack) t)
               (else (loop (node-parent (node-parent node))))))))
                                                             
(define (maybe-cons-on-stack l) 
  (and (lambda-live l) 
       (eq? (lambda-strategy l) strategy/label)
       (set-label-strategies (node-parent (node-parent l))
                             (cond ((ezclose-allowed? l)
                                    strategy/ezclose)
                                   ((and (cdr (lambda-live l))
                                         (vframe-allowed? l))
                                    strategy/vframe)
                                   (else
                                    strategy/label)))))

(define (change-to-vframe-or-heap l)
  (if (neq? (lambda-strategy l) strategy/heap)
      (set-label-strategies 
       (node-parent (node-parent l))
       (if (or (vframe-allowed? l)
               (ezclose-allowed? l))
            strategy/vframe 
            strategy/heap))))

(define (maybe-change-to-vframe l)
  (if (eq? (lambda-strategy l) strategy/ezclose)
      (set-label-strategies (node-parent (node-parent l)) strategy/vframe)))
      

(define (maybe-change-to-heap l) 
;  (and (or (lambda-live l) 
;           (neq? (lambda-strategy l) strategy/label))
       (set-label-strategies (node-parent (node-parent l))
                             strategy/heap))
  

(define (set-label-strategies node strategy)
  (walk (lambda (l) (set (lambda-strategy l) strategy))
        (cdr (call-args (lambda-body node))))
  (set (lambda-strategy node) strategy))


(define (live-analyze-leaf node)
  (cond ((literal-node? node)
         (cond ((or (addressable? (leaf-value node))
                    (primop? (leaf-value node)))
                (return '() nil '()))
               (else
                (return '() t '()))))
        ((primop-node? node)
         (cond ((foreign-name (primop-value node))
                (return '() t '()))
               (else 
                (return '() nil '()))))
        ((variable-known (reference-variable node))
         => (lambda (label)
              (select (lambda-strategy label)
                ((strategy/label)
                 (return (lambda-live label)
                         (eq? (lambda-env label) 'needs-link)
                         (if (let-lambda? label) 
                             '()  
                             (list label))))
                ((strategy/vframe) 
                 (return `(,(lambda-self-var (node-parent (node-parent label))))
                          nil
                          (list label)))
                ((strategy/ezclose)
                 (return '() nil (list label)))
                ((strategy/stack)
                 (return '() nil '()))
                (else 
                 (if (eq? (lambda-env label) 'unit-internal-closure)
                     (return '() t '())
                     (return `(,(lambda-self-var label)) nil '()))))))
        ((bound-to-continuation? (reference-variable node))
         (return '() nil '()))
        ((variable-binder (reference-variable node))
         (return `(,(reference-variable node)) nil '()))
        (else 
         (return '() t '()))))

(define (known-lambda? node)
  (let ((p (node-parent (node-parent node))))
    (cond ((node-parent p)
           => (lambda (p)
                (and (primop-node? (call-proc p))
                     (eq? (primop-value (call-proc p)) primop/Y))))
          (else nil))))


(define (live-analyze-body node)
  (iterate loop ((args (if (lambda-node? (call-proc node))  
                           (reverse (call-proc+args node))        ; let lambda last!
                           (call-proc+args node)))
                 (live '()) 
                 (global? nil) 
                 (known '()))
    (cond (args
           (receive (vars gl? kn) (live-analyze (car args))
             (loop (cdr args) 
                   (union vars live) 
                   (or global? gl?)
                   (union kn known))))
          ((call-hoisted-cont node)
           => (lambda (l)
                (return (union live (lambda-live l))
                        (or global? (eq? (lambda-env l) 'needs-link))
                        known)))
          (else
           (return live global? known)))))
                                       

(define (live-analyze-Y master)
  (if (and (not (lambda-db master))
           (eq? (lambda-strategy master) strategy/label))
      (set (lambda-db master) (vframe-or-ezclose master)))
  (destructure (((body-expr . label-exprs) (call-args (lambda-body master)))
                (strategy (lambda-strategy master)))
    (receive (global? known) (set-label-live label-exprs)
      (receive (l gl? kn) (live-analyze-lambda body-expr)
        (if (neq? (lambda-strategy master) strategy)
            (live-analyze-y master)
            (do ((exprs label-exprs (cdr exprs))
                 (live l (union live (lambda-live (car exprs)))))
              ((null? exprs)          
               (return (set-difference (delq! (lambda-self-var master) live)
                                (map lambda-self-var label-exprs))
                       (or global? gl?)
                       (set-difference (union known kn) label-exprs)))))))))



(define (set-label-live label-exprs)
  (iterate again ()
    (iterate loop ((lambdas label-exprs) 
                   (changed? nil) 
                   (global? nil) 
                   (known '()))
      (cond ((not (null? lambdas))           
             (let ((old-live (lambda-live (car lambdas)))
                   (old-global? (true? (lambda-env (car lambdas)))))
               (receive (live gl? kn) (live-analyze-lambda (car lambdas))
                 (cond ((and (set-eq? old-live live)
                             (eq? gl? old-global?))
                        (loop (cdr lambdas) 
                              changed? 
                              (or global? gl?)
                              (union kn known)))
                       (else
                        (loop (cdr lambdas) 
                              t 
                              (or global? gl?)
                              (union kn known)))))))
            (changed?
             (again))
            (else
             (return global? known))))))

(define (hoist-continuation cont)
  (let* ((call (node-parent cont))
         (live (hack-live (lambda-live cont) call)))
  (iterate loop ((call call))
    (let ((l (node-parent call)))       
      (cond ((or (primop-ref? (call-proc (node-parent l))
			      primop/remove-state-object)
	         (neq? (lambda-strategy l) strategy/open)
                 (intersection? (lambda-variables l) live)
                 (eq? (node-role l) call-proc)
                 (fxn= (call-exits (node-parent l)) 1))
             (set (call-hoisted-cont call) cont))
            (else
             (loop (node-parent l))))))))

(define (hack-live live call)
  (do ((args (cdr (call-args call)) (cdr args))
       (live live (if (and (lambda-node? (car args))
                           (eq? (lambda-strategy (car args)) strategy/hack))
                      (union live (lambda-live (car args)))
                      live)))
    ((null? args) live)))

             
(define (collect-top node)
  (set *unit-literals* '())
  (set *unit-variables* '())
  (collect (car (call-args (lambda-body node)))))

(define (collect node)
  (cond ((lambda-node? node)
         (walk collect (call-proc+args (lambda-body node))))
        ((literal-node? node)
         (let ((lit (literal-value node)))
           (or (addressable? lit)
               (primop? lit)
               (memq? lit *unit-literals*)
               (push *unit-literals* lit))))
        ((primop-node? node)
         (let ((prim (primop-value node)))
           (and (foreign-name prim)
                (not (memq? prim *unit-literals*))
                (push *unit-literals* prim))))
        (else 
         (let ((var (reference-variable node)))
           (or (variable-binder var)
               (memq? var *unit-variables*)
               (push *unit-variables* var))))))
