(herald (back_end lookkup)
  (env t (orbit_top defs) (back_end closure) (back_end bookkeep)))

(define (all-important-refs-are-calls? var)
  (every? (lambda (ref)
	    (or (eq? (node-role ref) call-proc)
		(and (eq? (node-role ref) (call-arg 2))
		     (let ((call (node-parent ref)))
		       (or (primop-ref? (call-proc call) primop/*define)
			   (primop-ref? (call-proc call) primop/*lset))))))
	  (variable-refs var)))

(define (var-is-vcell? var)
  (and (not (all-important-refs-are-calls? var))
       (neq? var *the-environment*)))

;;; ACCESS-VALUE This is the primary routine to get addressability to values.
;;; Just a giant case statement.


(define (access-value node value)
  (cond ((and (variable? value)
	      (not (variable-binder value))
	      (var-is-vcell? value))
	 (let ((acc (lookup node (get-lvalue value) nil)))
	   (let ((reg (get-register 'pointer node '*)))
	     (generate-move acc reg)
	     (reg-offset reg 2))))
	(else
	 (really-access-value node value))))

(define (really-access-value node value)               
 (let ((value (cond ((and (variable? value) (variable-known value))
                     => lambda-self-var)
                    (else value))))
  (cond ((register-loc value)
         => (lambda (spec)
              (cond ((fixnum? spec))
                    (else
                     (cond ((pair? (car spec))
                            (unlock (caar spec))
                            (cond ((reg-node (caar spec))
                                   => (lambda (var) (kill-if-dying var node))))
                            (unlock (cdar spec)))
                           (else
                            (unlock (car spec))
                            (cond ((reg-node (car spec))
                                   => (lambda (var) (kill-if-dying var node))))))
                     (set (register-loc value) nil)))
              spec))
        ((temp-loc value))
        ((variable? value)
         (let ((binder (variable-binder value)))
           (cond ((not binder)
                  (lookup node value nil))
                 ((and (fx= (variable-number value) 0) 
                       (assq binder (closure-env *unit*)))
                  (lookup node binder nil))
                 (else
                  (lookup node value binder)))))
        ((primop? value)
         (if (eq? value primop/undefined)
             (machine-num 0)
             (lookup node value nil)))
        ((eq? value '#T)
         (machine-num header/true))
        ((or (eq? value '#F) (eq? value '()))
          nil-reg)
        ((addressable? value)
         (lit value))
        (else
         (lookup node value nil)))))


;;; LOOKUP If the value is a known procedure, if it is in the unit we get it
;;; from there, otherwise we get the variable which the known procedure is
;;; bound to.

(define (lookup node value lambda-bound?)
  (xselect (lambda-strategy *lambda*)
    ((strategy/stack strategy/ezclose)
     (fetch-from-stack node value lambda-bound?))
    ((strategy/vframe strategy/hack)
     (let ((contour (lambda-self-var *lambda*)))
       (->register 'pointer node contour '*)
       (fetch-from-vframe node contour value lambda-bound?)))
    ((strategy/heap)
     (let ((contour (lambda-self-var *lambda*)))
       (->register 'pointer node contour '*)
       (fetch-from-heap node contour value lambda-bound?)))))


                                
;;; ACCESS-FROM-UNIT Get from unit when there is a closure-internal-template.
;;; If we have one, just offset from template-pointer. If we are internal to
;;; a closure which has one, get it first and then offset into unit.


(define (access-from-unit node contour var)
  (let ((closure (environment-closure (lambda-env (variable-binder contour)))))
    (cond ((closure-cit-offset closure)
           => (lambda (current-offset) 
                (let ((cl? (or (and (node? var) (lambda-node? var))
                           (closure? var)))
                      (disp (fx- (cdr (assq var (closure-env *unit*))) 
                                 (fx+ current-offset tag/extend))))
                  (cond ((and (eq? (lambda-strategy *lambda*) strategy/heap)
                              (eq? contour (car (closure-members closure))))
                         (if cl?
                             (list (reg-offset TP (fx+ disp tag/extend)))
                             (reg-offset TP disp)))
                        ((register-loc (variable-binder (car (closure-members closure))))
                         => (lambda (reg)
                              (if cl?
                                  (list (reg-offset reg (fx+ disp tag/extend)))
                                  (reg-offset reg disp))))
                        (else
                         (let* ((c-reg (register-loc contour))
                                (reg (get-register 'pointer node '*)))
                           (generate-move
                              (reg-offset c-reg
                                          (fx- (fx- 0 tag/extend)
                                               (cdr (assq contour
                                                    (closure-env closure)))))
                              reg)                        
                           (mark (variable-binder (car (closure-members closure)))
                                 reg)
                           (if cl?
                               (list (reg-offset reg (fx+ disp tag/extend)))
                               (reg-offset reg disp))))))))
          (else nil))))


(define (get-env var)
  (lambda-env (variable-binder var)))
                                      

;;; Yukk.  Here we get a variable from a stack frame.  If it is in the frame
;;; we are OK.  Otherwise we chain down stack frames as long as they are there.
;;; These frames are all simple offsets from SP.  When we arrive at a pointer
;;; into the heap, we load that pointer into a register and go to the heap
;;; code to do the rest.

(define (fetch-from-vframe node contour value lambda-bound?)
  (iterate loop ((offset 0) (l (variable-binder contour)))
    (select (lambda-strategy l)
      ((strategy/label strategy/open)
       (loop offset (node-parent (node-parent l))))
      (else
       (cond ((not (lambda-env l))
              (loop offset (node-parent (node-parent l))))
             (else
              (let* ((env (lambda-env l))
                     (closure (environment-closure env)))
                (cond ((and lambda-bound? (assq value (closure-env closure)))
                       => (lambda (env-pair) 
                            (reg-offset (register-loc contour)
                                        (fx+ (fx- (cdr env-pair) tag/extend)
                                             (fx- offset
                                                  (environment-cic-offset env))))))
                      ((closure-link closure)
                       => (lambda (link)
                       (let ((accessor (reg-offset (register-loc contour)
                                                   (fx- (fx+ offset CELL)
                                                 (fx+ (environment-cic-offset env) tag/extend)))))
                         (into-register 'pointer node link accessor '*)
                         (xselect (lambda-strategy (variable-binder link))
                            ((strategy/heap) 
                             (fetch-from-heap node link value lambda-bound?))
                            ((strategy/vframe strategy/hack) 
                             (fetch-from-vframe node link value lambda-bound?))))))
                      ((labels-master-lambda? l)
                       (let* ((p (node-parent l))
                              (node ((call-arg 1) p)))
                         (cond ((lambda-node? node)
                                (loop (fx+ (fx- (closure-size closure) 
                                                (environment-cic-offset env))
                                           (fx+ offset
                                                (closure-size 
                                                  (environment-closure
                                                    (lambda-env node)))))
                                      (node-parent p)))
                               (else 
                                (loop (fx+ (fx- (closure-size closure) 
                                                (environment-cic-offset env))
                                           offset)
                                      (node-parent p))))))
                      (else
                       (loop (fx+ (fx- (closure-size closure) 
                                       (environment-cic-offset env))
                                  offset)
                             (node-parent (node-parent l))))))))))))

                                                 
(define (fetch-from-stack node value lambda-bound?)
  (iterate loop ((offset 0) (l *lambda*))
    (select (lambda-strategy l)
      ((strategy/open)
       (loop offset (node-parent (node-parent l))))
      ((strategy/label strategy/heap)
       (let* ((p (node-parent l))
              (node ((call-arg 1) p)))
         (cond ((and (labels-master-lambda? l) (lambda-node? node))
                (loop (fx+ (closure-size (environment-closure (lambda-env node)))
                           offset)
                      (node-parent p)))
               (else 
                (loop offset (node-parent p))))))
      (else
       (cond ((not (lambda-env l))
              (loop offset (node-parent (node-parent l))))
             (else
              (let ((closure (environment-closure (lambda-env l))))
                (cond ((and lambda-bound? (assq value (closure-env closure)))
                       => (lambda (env-pair) 
                            (reg-offset SP (fx+ offset 
                                                (fx+ *stack-pos* (cdr env-pair))))))
                      ((closure-link closure)
                       => (lambda (link)
                       (let ((accessor (reg-offset SP (fx+ *stack-pos*
                                                            (fx+ offset CELL)))))
                         (into-register 'pointer node link accessor '*)
                         (xselect (lambda-strategy (variable-binder link))
                            ((strategy/heap) 
                             (fetch-from-heap node link value lambda-bound?))
                            ((strategy/vframe strategy/hack) 
                             (fetch-from-vframe node link value lambda-bound?))))))
                      ((labels-master-lambda? l)
                       (let* ((p (node-parent l))
                              (node ((call-arg 1) p)))
                         (cond ((lambda-node? node)
                                (loop (fx+ (fx+ (closure-size closure)
                                                (closure-size (environment-closure
                                                                (lambda-env node))))
                                           offset)
                                      (node-parent p)))
                               (else 
                                (loop (fx+ (closure-size closure) offset)
                                      (node-parent p))))))
                      (else
                       (loop (fx+ (closure-size closure) offset)
                             (node-parent (node-parent l))))))))))))
                          


(define (closure-internal-closure? value closure)
  (cond ((neq? closure *unit*)
         (memq? value (closure-members closure)))
        (else
         (or (and (node? value) (lambda-node? value))
             (closure? value)))))

(define (fetch-from-heap node contour value lambda-bound?) 
  (iterate loop ((env (get-env contour)) (contour contour)) 
    (let ((a-list (closure-env (environment-closure env)))
          (current-offset (environment-cic-offset env)))
      (cond ((assq value a-list)
             => (lambda (pair)
                  (if (closure-internal-closure? value
                                                 (environment-closure env))
                      (list (reg-offset (register-loc contour)  ; *** hack
                                        (fx- (cdr pair) current-offset)))
                      (reg-offset (register-loc contour)
                                  (fx- (cdr pair)
                                       (fx+ current-offset tag/extend))))))
            ((and (not lambda-bound?) (access-from-unit node contour value)))
            ((neq? (environment-closure env) *unit*)
             (into-register 'pointer node (caadr a-list)
                (reg-offset  (register-loc contour)
                             (fx+ (fx- 0 current-offset) tag/extend))
                '*)
             (loop (get-env (caadr a-list)) (caadr a-list)))
            (else
             (bug "Couldn't find ~s~% in call ~s"
                  value
                  (pp-cps node)))))))

;;; Code to get a continuation off the stack.
;;; Search up the tree until we find it.
;;; This relies on generating code for the body of a labels FIRST.


(define (fetch-continuation-from-stack node var)
  (iterate loop ((offset 0) (l (node-parent node)))
    (cond ((eq? (variable-binder var) l)
           offset)
          (else
           (select (lambda-strategy l)
             ((strategy/stack)
              (loop (fx+ (closure-size (environment-closure (lambda-env l)))
                         offset)
                   (node-parent (node-parent l))))
             (else
              (loop offset (node-parent (node-parent l)))))))))

(define (restore-continuation node leaf)
  (let ((proc (call-proc node)))
    (let ((stop (cond ((primop-node? proc) nil)
                      ((variable-known (reference-variable proc))
                       => (lambda (l)
                            (let ((p (node-parent (node-parent l))))
                              (if (labels-master-lambda? p) p nil))))
                      (else nil))))
      (really-restore-continuation node (leaf-value leaf) stop))))

(define (restore-ezclose-continuation node proc)
  (really-restore-continuation node (leaf-value ((call-arg 1) node))
                                    (node-parent (node-parent proc))))

(define (restore-vframe-continuation node proc)
  (really-restore-continuation node (leaf-value ((call-arg 1) node))
                                    (node-parent (node-parent proc))))


(define (really-restore-continuation node var stop)
  (let* ((binder (variable-binder var))
         (y-lambda (node-parent (node-parent binder)))
         (n (fetch-continuation-from-stack node var)))
    (if (not (labels-master-lambda? y-lambda))
        (adjust-stack-pointer n)
        (select (lambda-strategy y-lambda)
          ((strategy/heap)           
           (if (eq? (node-role binder) (call-arg 1))
               (let ((pair (lambda-live y-lambda)))
                 (remove-stack (cdr pair) (fx+ n (car pair)) nil))
               (adjust-stack-pointer n)))
          (else                       
           (remove-stack y-lambda n stop))))))

(define (remove-stack y-lambda n stop)
  (iterate loop ((y-lambda y-lambda)
                 (n n))
    (if (or (null? y-lambda) (eq? y-lambda stop))
        (adjust-stack-pointer n)
        (select (lambda-strategy y-lambda)
          ((strategy/ezclose strategy/label)
           (let ((pair (lambda-live y-lambda)))
             (loop (cdr pair) (fx+ (car pair) n))))
          ((strategy/vframe)
           (adjust-stack-pointer n)
           (let ((pair (lambda-live y-lambda)))                   
             (generate-vframe-test (car pair))
             (loop (cdr pair) 0)))
          (else 
           (adjust-stack-pointer n))))))
