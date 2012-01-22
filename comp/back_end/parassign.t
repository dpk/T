(herald (back_end parassign)
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

(define-local-syntax (ass-comment string . rest)
  `(if *assembly-comments?*
       (emit-comment (format nil ,string ,@rest))))                      

;;; ALLOCATE-CALL The "top".  Dispatch on the type of call.

(define (allocate-call node)
  (if *call-break?* (breakpoint (pp-cps node)))
  (cond ((call-hoisted-cont node)
         => (lambda (cont) (make-stack-closure node cont))))
  (let ((proc (call-proc node)))
    (cond ((primop-node? proc)
           (ass-comment "~s" (pp-cps node))
           (allocate-primop-call node))
          ((lambda-node? proc)
           (generate-let node))
          ((variable-known (leaf-value proc))
           => (lambda (proc)                     
                (ass-comment "Call known procedure ~s" 
                         (cons (lambda-name proc) (cdr (pp-cps node))))
                (xcond ((fx= (call-exits node) 0)
                        (allocate-known-return node proc))
                       ((fx= (call-exits node) 1)
                        (allocate-known-call node proc)))))
          ((fx= (call-exits node) 0)
           (ass-comment "Return from procedure ~s" (pp-cps node))
           (allocate-return node))
          ((fx= (call-exits node) 1)
           (ass-comment "Call unknown procedure ~s" (pp-cps node))
           (allocate-general-call node))
          (else
           (bug "too many exits - ~s" node)))))
  
;;; ALLOCATE-LABEL-CALL If this is the path that arrives first, go to
;;; determine where the free variables of the join are to be kept.
;;; Make the state of the registers the way the join point wants it.
;;; Parallel assign and jump to the label.
                         
(define (allocate-known-call node proc)
  (xselect (lambda-strategy proc)
    ((strategy/label) (allocate-label-call node proc))
    ((strategy/heap) (allocate-known-heap-call node proc))
    ((strategy/ezclose) (allocate-ezclose-call node proc))
    ((strategy/vframe) (allocate-vframe-call node proc)))
  (if (call-in-body? proc node)
      (generate-jump proc)
      (generate-avoid-jump proc)))
      

(define (allocate-known-heap-call node proc)
  (let ((cont ((call-arg 1) node)))
    (parallel-assign-general node)
    (if (leaf-node? cont) (restore-continuation node cont)))
  (clear-slots)                            
  (generate-move (reg-offset P -2) TP)
  (if (n-ary? proc) 
      (generate-move (machine-num (length (call-args node))) NARGS)))


(define (allocate-label-call node proc)
  (let ((join (get-or-set-join-state node proc))
        (cont ((call-arg 1) node)))
    (parallel-assign node
                     (cdr (call-args node))
                     (join-point-arg-specs join)
                     nil
                     (join-point-global-registers join))
    (if (leaf-node? cont) (restore-continuation node cont))
    (cond ((and (join-point-contour-needed? join)
                (join-point-contour join))
           => (lambda (contour)
                (let ((b (variable-binder contour)))
                  (if (and (closure-cit-offset (environment-closure (lambda-env b)))
                           (neq? *lambda* b))
                      (generate-move (reg-offset P -2) TP)))))))
  (clear-slots))
                       

(define (allocate-vframe-call node proc)
  (cond ((lambda-node? ((call-arg 1) node))
         (parallel-assign-vframe node proc))
        (else                          
         (parallel-assign-vframe node proc)          
         (restore-vframe-continuation node proc)))
  (clear-slots)                            
  (if (n-ary? proc) (generate-move (length (call-args node)) NARGS)))
                    
                                                          
(define (allocate-ezclose-call node proc)
  (parallel-assign-known node)          
  (restore-ezclose-continuation node proc)
  (clear-slots)                            
  (if (n-ary? proc) (generate-move (length (call-args node)) NARGS)))


                         
(define (allocate-known-return node proc)
  (select (lambda-strategy proc)
    ((strategy/label) (allocate-label-return node proc))
    (else 
     (parallel-assign-return node)      
     (restore-continuation node (call-proc node))
     (clear-slots)
     (generate-jump proc))))
      



(define (allocate-label-return node proc)
  (let ((join (get-or-set-join-state node proc)))
    (cond ((n-ary? proc)
           (really-parallel-assign node '() '() (join-point-global-registers join)))
          (else
           (parallel-assign node
                            (call-args node)
                            (join-point-arg-specs join)
                            nil
                            (join-point-global-registers join)))))
  (restore-continuation node (call-proc node))
  (clear-slots)
  (generate-jump proc))

(define (allocate-conditional-continuation node proc-leaf)
  (let ((proc (variable-known (leaf-value proc-leaf))))
    (select (lambda-strategy proc)
      ((strategy/stack))
      (else
       (let ((join (get-or-set-join-state node proc)))
         (parallel-assign node
                          '()
                          (join-point-arg-specs join)
                          nil
                        (join-point-global-registers join)))))
   (restore-continuation node proc-leaf)
   (clear-slots)
   (generate-jump proc)))


  

(define (allocate-general-call node)
  (let ((cont ((call-arg 1) node)))
    (cond ((lambda-node? cont)     
           (parallel-assign-general node))
          (else                          
           (parallel-assign-general node)          
           (restore-continuation node cont))))
  (clear-slots)
  (generate-general-call (reference-variable (call-proc node))
			 (fx- (length (call-args node)) 1)))

                                   
(define (allocate-return node)
  (parallel-assign-return node)      
  (restore-continuation node (call-proc node))
  (clear-slots)
  (generate-return (length (call-args node))))
                         



(define (parallel-assign-general node)
  (parallel-assign node (cons (call-proc node) (cdr (call-args node)))
                        nil t '()))
                                 
(define (parallel-assign-known node)
  (parallel-assign node (cdr (call-args node)) nil nil '()))
                                            

(define (parallel-assign-vframe node proc)
  (if (not (lambda-env (node-parent (node-parent proc))))
      (parallel-assign node (cdr (call-args node)) nil nil '())   
      (parallel-assign node (cdr (call-args node)) nil nil 
               (list (cons P (lambda-self-var (node-parent (node-parent proc))))))))

(define (parallel-assign-return node)
  (parallel-assign node (call-args node) nil nil '()))


;;; PARALLEL-ASSIGN Cons a closure if necessary.  It is known that there
;;; will only be one that needs to be consed.

(define (parallel-assign node args p-list proc? solve-list)
  (let* ((pos-list (if p-list p-list (reg-positions (length args) proc?)))
         (closure (get-closure args)))
    (cond (closure
           (make-heap-closure node closure)
           (really-parallel-assign node args pos-list solve-list))
          (else
           (really-parallel-assign node args pos-list solve-list)))))

(define (get-closure args)
  (any (lambda (arg)               
         (and (lambda-node? arg)
              (eq? (lambda-strategy arg) strategy/heap)
              (neq? (environment-closure (lambda-env arg)) *unit*)
              (environment-closure (lambda-env arg))))
       args))


;;; do-now - register or temp pairs (source . target)
;;; trivial - immediate or lambda
;;; do-later - environment
;;; See implementor for this stuff. Hairy!!
                       
(define-structure-type arg-mover
  from
  from-rep
  to
  to-rep)                                  

(define (mover from from-rep to to-rep)
  (let ((a (make-arg-mover)))
    (set (arg-mover-from a) from)
    (set (arg-mover-from-rep a) from-rep)
    (set (arg-mover-to a) to)
    (set (arg-mover-to-rep a) to-rep)
    a))

(define (really-parallel-assign node args pos-list solve-list)
  (receive (do-now trivial do-later) (sort-by-difficulty args pos-list)
    (receive (do-now do-later) (add-on-free-list do-now do-later solve-list)
      (solve node do-now do-later)                                    
      (lock AN)                     ; contains closures
      (walk (lambda (pair)
              (if (lambda-node? (car pair))
                  (do-trivial-lambda node (car pair) (cdr pair))))
            trivial)
      (unlock AN)
      (do-indirects node do-later)
      (walk (lambda (pair)
              (if (not (lambda-node? (car pair)))
                  (do-immediate (car pair) (cdr pair))))
            trivial))))
                                                      

(define (add-on-free-list do-now do-later solve-list)
  (iterate loop ((pairs solve-list) (do-now do-now) (do-later do-later))
    (cond ((null? pairs)
           (return do-now do-later))
          ((or (register-loc (cdar pairs))
               (temp-loc (cdar pairs)))
           => (lambda (reg)
                (loop (cdr pairs)
                      (cons (mover reg (variable-rep (cdar pairs)) 
                                   (caar pairs) (variable-rep (cdar pairs)))
                            do-now)
                      do-later)))
          (else
           (loop (cdr pairs)
                 do-now
                 (if (fx= (caar pairs) P)
                     (append! do-later (list (cons (cdar pairs) P)))
                     (cons (cons (cdar pairs) (caar pairs))
                           do-later)))))))


(define (sort-by-difficulty args pos-list)
  (iterate loop ((args args) (do-now '()) (trivial '()) (do-later '())
                 (pos-list pos-list))
    (cond ((null? args)
           (return do-now trivial do-later))
          ((lambda-node? (car args)) 
           (let ((l (car args)))
             (cond ((eq? (environment-closure (lambda-env l)) *unit*)
                    (loop (cdr args)
                          do-now
                          trivial
                          (cons (cons l (car pos-list)) do-later)
                          (cdr pos-list)))
                   (else
                    (loop (cdr args)
                          do-now
                          (cons (cons l (car pos-list)) trivial)
                          do-later
                          (cdr pos-list))))))
          ((addressable? (leaf-value (car args)))
           (loop (cdr args)
                 do-now
                 (cons (cons (car args) (car pos-list)) trivial)
                 do-later
                 (cdr pos-list)))
          (else
           (let* ((val (leaf-value (car args)))
                  (value (cond ((and (variable? val) (variable-known val))
                               => lambda-self-var)
                              (else val))))
             (cond ((or (register-loc value) (temp-loc value))
                    => (lambda (reg)
                         (loop (cdr args)
                               (cons (mover reg (variable-rep value)
                                            (caar pos-list) (cdar pos-list))
                                     do-now)
                               trivial
                               do-later
                               (cdr pos-list))))
                   (else
                    (loop (cdr args)
                          do-now
                          trivial
                          (if (fx= (caar pos-list) P)
                              (append! do-later (list (cons value (car pos-list))))
                              (cons (cons value (car pos-list)) do-later))
                          (cdr pos-list)))))))))


(define (do-immediate node reg-rep)
  (generate-move (value-with-rep (leaf-value node) (cdr reg-rep)) 
                 (car reg-rep)))

(define (do-indirects node do-later) 
  (iterate loop ((items do-later))
    (if items
        (let ((item (car items))
              (contour (lambda-self-var *lambda*)))
          (receive (mover target) (cond ((and (node? (car item)) 
                                              (lambda-node? (car item)))
                                         (return indirect-lambda (cadr item)))
                                        ((atom? (cdr item))
                                         (return indirect-free-var (cdr item)))
                                        (else
                                         (return indirect-arg (cadr item))))
            (cond ((or (eq? (register-loc contour) target)
                       (eq? (temp-loc contour) target))        
                   (if (cdr items)
                       (loop (append (cdr items) (cons item '())))
                       (mover node item target)))
                  (else
                   (mover node item target)
                   (loop (cdr items)))))))))
        

(define (indirect-lambda node pair target) 
  (lambda-queue (car pair))
  (generate-move (lookup node (car pair) nil) target)
  (unmark-reg target)
  (lock target))

(define (indirect-free-var node pair target)
  (really-rep-convert node
                      (access-value node (car pair)) 
                      (variable-rep (car pair))
                      target
                      (variable-rep (car pair)))
  (unmark-reg target)
  (mark (car pair) target)
  (lock target))

(define (indirect-arg node pair target)
  (let ((to-rep (cddr pair)))
    (really-rep-convert node 
                        (access-value node (car pair))
                        (if (variable? (car pair))
                            (variable-rep (car pair)) 
                            'rep/pointer) 
                        target
                        to-rep)
    (unmark-reg target)
    (kill (car pair))
    (mark (car pair) target)
    (lock target)))
                   
(define (unmark-reg reg)
  (cond ((reg-node reg)
         => (lambda (var)
              (set (reg-node reg) nil)
              (if (register? reg)
                  (set (register-loc var) nil)
                  (set (temp-loc var) nil))))))

               
(define (solve node movers do-later)
  (let ((contour (lambda-self-var *lambda*))
        (vals (map (lambda (mover)
                       (reg-node (arg-mover-to mover)))
                     movers)))
    (cond ((and do-later
                (any (lambda (mover)
                        (if (eq? (reg-node (arg-mover-to mover)) contour)
                            mover
                            nil))
                      movers))
           => (lambda (mover)
                (if (neq? (arg-mover-from mover) (arg-mover-to mover))
                    (free-register node (register-loc contour)))
                (walk (lambda (val)
                        (if (neq? val contour) (kill val)))
                      vals)))
          (else
           (walk kill vals)))
    (walk (lambda (mover)
            (lock (arg-mover-to mover)))
          movers)
    (receive (movers self-movers) (separate-self-movers movers)
      (if (not (exchange-hack movers))
          (do-assignment movers node))
      (walk (lambda (mover)
              (really-rep-convert node (arg-mover-from mover)
                                       (arg-mover-from-rep mover)
                                       (arg-mover-to mover)
                                       (arg-mover-to-rep mover)))
            self-movers))))
                           
(define (do-assignment movers node)
  (iterate loop1 ((movers movers)
                  (targets (map arg-mover-to movers))
                  (temp nil))
    (cond ((null? movers))
        (else
         (iterate loop2 ((candidates targets))
           (cond ((null? candidates)
                  (let ((mover (car movers)))
                    (generate-move (arg-mover-to mover)
                                   (reg-offset TASK
                                     (if (eq? (arg-mover-to-rep mover) 'rep/pointer)
                                          task/extra-pointer
                                          task/extra-scratch)))
                    (really-rep-convert node
                                        (arg-mover-from mover)
                                        (arg-mover-from-rep mover)
                                        (arg-mover-to mover)
                                        (arg-mover-to-rep mover))
                    (loop1 (cdr movers)
                           (delq (arg-mover-to mover) targets)
                           (arg-mover-to mover))))
                 ((not (mem? from-reg-eq? (car candidates) movers))
                  (let ((mover (car (mem to-reg-eq? (car candidates) movers))))
                    (really-rep-convert node
                         (cond ((eq? (arg-mover-from mover) temp)
                                (if (eq? (arg-mover-to-rep mover) 'rep/pointer)
                                    (reg-offset TASK task/extra-pointer)
                                    (reg-offset TASK task/extra-scratch)))
                               (else
                                (arg-mover-from mover)))
                         (arg-mover-from-rep mover)
                         (arg-mover-to mover)
                         (arg-mover-to-rep mover))
                    (loop1 (delq mover movers)
                           (delq (arg-mover-to mover) targets)
                           temp)))
                 (else
                  (loop2 (cdr candidates)))))))))


(define (separate-self-movers movers)
  (iterate loop ((movers movers) (m '()) (s '()))
    (cond ((null? movers) (return m s))
          ((eq? (arg-mover-from (car movers)) (arg-mover-to (car movers)))
           (loop (cdr movers) m (cons (car movers) s)))
          (else
           (loop (cdr movers) (cons (car movers) m) s)))))

(define (to-reg-eq? reg mover) (fx= (arg-mover-to mover) reg))
(define (from-reg-eq? reg mover) (fx= (arg-mover-from mover) reg))


