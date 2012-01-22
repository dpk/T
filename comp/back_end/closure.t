(herald (back_end closure)
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



;;; Closure analysis.
;;;=========================================================================
(lset *top-level-lambda* nil)

(define (close-analyze-top node variables)
    (set *unit-closures* nil)
    (set *unit-templates* nil)
    (let* ((l ((call-arg 1) (lambda-body node)))
           (env (list (lambda-self-var node)))
           (via (lambda-self-var l)))
      (bind ((*top-level-lambda* via)) 
        (close-analyze-body (lambda-body l) env via env via))
      (set *unit* (create-unit l)) 
      (create-environment l *unit* 16)
      (return (cddr (closure-env *unit*)) *unit-templates* l))) ; skip the 
                                                                ; *environment*
                                                                ; and top-level
(define (close-analyze-body node  senv svia henv hvia)
  (cond ((and (primop-node? (call-proc node))
              (eq? (primop-value (call-proc node)) primop/Y))
         (really-close-analyze-body
                       (cons ((call-arg 1) node) 
                             (call-args (lambda-body ((call-arg 2) node))))
                       senv svia henv hvia))
        (else
         (really-close-analyze-body (call-proc+args node)
                                    senv svia henv hvia))))


(define (really-close-analyze-body nodes senv svia henv hvia)
  (receive (live cics vframe stack)
           (accumulate-environment nodes senv svia henv hvia)
    (if cics (close-analyze-heap cics live henv hvia))
    (if stack (close-analyze-stack stack senv svia henv hvia))
    (if vframe (close-analyze-vframe vframe hvia svia henv senv))))

(define (close-analyze-heap cics live henv hvia)
  (let* ((cic-vars (map lambda-self-var cics))
         (live (set-difference live cic-vars))
         (global? (or (memq? hvia live)
                      (any? (lambda (node)
                              (eq? (lambda-env node) 'unit-internal-closure))
                            cics)))
         (inter (intersection live henv))               
         (link (if (or global? inter)
                    hvia 
                    nil))
         (delta (set-difference (delq! hvia live) henv)))  
    (xselect (lambda-strategy (variable-binder hvia))
      ((strategy/heap)                            
       (if (or global? (cdr inter))
           (create-closure link cic-vars delta nil 'heap)
           (create-closure nil cic-vars live nil 'heap)))
      ((strategy/vframe strategy/hack)
       (create-closure nil cic-vars (delq! hvia live) nil 'heap)))
    (walk (lambda (cic)
            (cond ((object-lambda? cic)
                   (destructure (((#f proc #f . methods) 
                                  (call-args (lambda-body cic))))
                     (walk (lambda (method)                     
                              (set (lambda-env method) (lambda-env cic))
                              (close-analyze-body (lambda-body method)
                                                  live
                                                  (lambda-self-var cic)
                                                  live
                                                  (lambda-self-var cic)))
                           (cons proc methods))))
                  (else
                   (close-analyze-body (lambda-body cic)
                                       live
                                       (lambda-self-var cic)
                                       live
                                       (lambda-self-var cic)))))
          cics)))



(define (close-analyze-vframe vframe hvia svia henv senv) 
  (let* ((live-1 (do ((vframe vframe (cdr vframe))
                    (live '() (union live (lambda-live (car vframe)))))
                   ((null? vframe) live)))
         (link (if (or (intersection? live-1 senv)
		       (memq? hvia live-1)
                       (any? (lambda (node)
                               (eq? (lambda-env node) 'needs-link))
                             vframe))
                   svia
                   nil))             
         (via (if (and link (eq? svia hvia)) hvia nil))
	 (live (delq! hvia live-1)))
    (xselect (lambda-strategy (car vframe)) 
      ((strategy/ezclose)
       (create-closure via nil (set-difference live senv) vframe 'ezclose)
       (walk (lambda (cic)
               (close-analyze-body (lambda-body cic)
                                   live
                                   (lambda-self-var cic)
                                   henv
                                   hvia))
             vframe))
      ((strategy/vframe)      
       (let* ((contour (lambda-self-var (node-parent 
                          (node-parent (car vframe)))))
              (xlive (delq! contour live)))
         (if (or link xlive)
             (create-closure via nil (set-difference xlive senv) vframe 'vframe)
             (walk (lambda (cic) (set (lambda-env cic) nil)) vframe))
         (walk (lambda (cic)
                 (close-analyze-body (lambda-body cic)
                                     xlive
                                     contour
                                     xlive
                                     contour))
               vframe))))))
  
;;; (proc+handler k object-proc method-names . methods)
;;; Must hack this by not returning the proc as a cic.  The parent lambda will
;;; masquerade as the proc until code generation

(define (accumulate-environment nodes senv svia henv hvia)
  (iterate loop ((nodes nodes) (live '()) (cics '()) (vframe '()) (stack '()))
    (cond ((null? nodes)
           (return live cics vframe (reverse! stack)))
          ((not (lambda-node? (car nodes)))
           (loop (cdr nodes) live cics vframe stack))
          (else
           (xselect (lambda-strategy (car nodes))
             ((strategy/heap)
              (cond ((object-lambda? (car nodes))
                     (let* ((args (cdddr (call-args (lambda-body (car nodes)))))
                            (new-cics (close-analyze-object (car nodes) args)))
                       (loop (cdr nodes)
                             (union (lambda-live (car nodes)) live)
                             (append new-cics cics)
                             vframe
                             stack)))
                    ((eq? (lambda-env (car nodes)) 'unit-internal-closure)
                     (push *unit-closures* (car nodes))
                     (let ((env (lambda-live (car nodes)))
                           (via (lambda-self-var (car nodes))))
                       (close-analyze-body (lambda-body (car nodes))
                                           env via env via)
                       (loop (cdr nodes) (union env live) cics vframe stack)))
                    (else
                     (loop (cdr nodes)
                           (union (lambda-live (car nodes)) live)
                           (cons (car nodes) cics)
                           vframe
                           stack))))
             ((strategy/open)
              (close-analyze-body (lambda-body (car nodes)) senv svia henv hvia)
              (loop (cdr nodes) live cics vframe stack))
             ((strategy/label)
              (close-analyze-label (car nodes) henv hvia)
              (loop (cdr nodes) live cics vframe stack))                
             ((strategy/stack)
              (loop (cdr nodes) live cics vframe (append! stack (list (car nodes)))))
             ((strategy/hack)
              (loop (cdr nodes) live cics vframe (cons (car nodes) stack)))
             ((strategy/ezclose)
              (loop (cdr nodes) live cics (cons (car nodes) vframe) stack))
             ((strategy/vframe)
              (loop (cdr nodes) live cics (cons (car nodes) vframe) stack)))))))

(define (close-analyze-object obj methods)
  (cond ((null? (lambda-live obj))
         (let ((proc (cadr (call-args (lambda-body obj)))))
           (push *unit-closures* obj)
           (let ((env (lambda-live obj))
                 (via (lambda-self-var obj)))
             (close-analyze-body (lambda-body proc) env via env via)
             (walk (lambda (node)
                     (let ((env (lambda-live node))
                           (via (lambda-self-var (node-parent (node-parent node)))))
                       (close-analyze-body (lambda-body node) env via env via)))
                    methods)))
           '())
        (else  
         (list obj))))
                                    


(define (close-analyze-stack nodes stackenv stackvia heapenv heapvia)
  (let* ((h (variable-binder heapvia))
         (live (do ((nodes nodes (cdr nodes))
                    (live '() (union live (lambda-live (car nodes)))))
                   ((null? nodes) live)))
         (link-set (if (or (intersection? live stackenv)
                           (and (memq? heapvia live)        
                                (or (neq? (lambda-strategy h) strategy/vframe)
                                    (lambda-env h)))    ; hack
                           (any? (lambda (node)
                                   (eq? (lambda-env node) 'needs-link))
                                 nodes))
                       `(,stackvia) 
                       '()))
         (closure-env (delq! heapvia (set-difference live stackenv))))
    (create-closure (if (and link-set (eq? stackvia heapvia)) heapvia nil)
                    (map lambda-self-var nodes)
                    closure-env
                    nil
                    'stack)
    (close-analyze-body (lambda-body (car nodes))
                        live
                        (lambda-self-var (car nodes))
                        heapenv heapvia)
    (walk (lambda (node)
            (close-analyze-body (lambda-body node)
                                live
                                (lambda-self-var node)
                                live
                                (lambda-self-var node)))
          (cdr nodes))))



                       


                                                             



(define (close-analyze-label node heapenv heapvia)
  (let* ((live (lambda-live node))
         (need-contour? (eq? (lambda-env node) 'needs-link))
         (b (variable-binder heapvia))
         (via (if (or (lambda-live b) (known-lambda? b)) 
                  *top-level-lambda* 
                  heapvia)))
    (set (lambda-env node) (create-join-point live via need-contour?))
    (walk (lambda (var) (set (variable-definition var) 'many)) live)
    (close-analyze-body (lambda-body node) '() via '() via)))


(define (set-eq? s1 s2)
  (if (fx= (length s1) (length s2))
      (every? (lambda (x) (memq? x s2)) s1)
      nil))      
        
;;; Environment structure is the lambda-env slot of each lambda which is
;;; strategy/stack or strategy/heap. The variables are sorted by size.
;;; (For stack closures) a continuation is represented as offset -1 in the
;;;  a-list.

(lset *unit* nil)
(lset *unit-closures* nil)
(lset *unit-templates* nil)
(lset *unit-literals* nil)                              
(lset *unit-variables* nil)

(define-structure-type environment
  closure    ; the closure this environment is a member of
  cic-offset ; offset of this environment's descriptor in the closure
  (((print self stream)
     (format stream "#{Environment_~S in Closure_~S}"
             (object-hash self)    
             (object-hash (environment-closure self))))))

(define-structure-type closure             
  members     ; list of closure-internal-closures (variables)
  vframe-lambdas 
  env         ; a-list of variables and offsets in the closure (in bytes)
  pointer     ; number of pointer slots
  scratch     ; number of scratch slots
  size        ; total size of closure (in bytes)
  cit-offset  ; offset of first
  link        ; superior closure
  (((print self stream)
     (format stream "#{Closure_~S with ~D vars, cics ~S}"
             (object-hash self)    
             (length (closure-env self))
             (map variable-unique-name
                  (closure-members self))))))

(define-structure-type join-point
  env                  ;;; free variables
  arg-specs            ;;; list of numbers for argument-positions
  global-registers     ;;; list of (register . variable)
  contour              ;;; nearest superior template
  contour-needed?
  )

(define (create-join-point env contour needed?)
  (let ((j (make-join-point)))
    (set (join-point-env j) env)
    (set (join-point-arg-specs j) nil)
    (set (join-point-global-registers j) 'not-yet-determined)
    (set (join-point-contour-needed? j) needed?)
    (set (join-point-contour j) contour)
    j))

                                              
(define-structure-type loc-list        ;;; appears in the unit
  var
  )


(define (create-loc-list var)
  (let ((l (make-loc-list)))
    (set (loc-list-var l) var)
    l))


(define (create-unit thing)
 (let ((unit (make-closure))) 
   (receive (a-list count) (do-unit-variables thing)   
     (do ((lits *unit-literals* (cdr lits))
          (count count (fx+ count CELL))
          (a-list a-list `((,(car lits) . ,count) ,@a-list)))
       ((null? lits)
        (do ((closures (reverse! *unit-closures*) (cdr closures))
             (count count (fx+ count CELL))
             (a-list a-list `((,(car closures) . ,count) ,@a-list)))
            ((null? closures)
             (do ((templates *unit-templates* (cdr templates))
                  (count count (fx+ count (fx* CELL 3)))
                  (a-list a-list `((,(car templates) . ,(fx+ count CELL)) ,@a-list)))
                 ((null? templates)
                  (set (closure-pointer unit) (fx- (fx/ count CELL) 1))
                  (set (closure-scratch unit) 0)
                  (set (closure-env unit)  (reverse! a-list))
                  (set (closure-cit-offset unit) nil)
                  unit) 
               (set (closure-cit-offset (car templates)) (fx+ count CELL))))
          (create-environment (car closures) unit count)))))))

(define *the-environment* (create-variable '*the-environment*))
                           
                                   
(define (do-unit-variables thing)
  (iterate loop ((a-list `((,*the-environment* . 12) (,thing . 16)))
                 (vars (delq! *the-environment* *unit-variables*)); header 0
                 (count 20))                                      ; id 4
    (cond ((null? vars) (return a-list count))                    ; filename 8
          (else                                                   ; env 12
           (let ((var (car vars)))                                ; thing 16
	     (receive (value? vcell?)
		      (cond ((defined-variable? var)
			     (if (null? (cdr (variable-refs var)))
				 (return nil t)
				 (return (all-important-refs-are-calls? var) t)))
			    ((all-important-refs-are-calls? var)
			     (return t nil))
			    (else
			     (return nil t)))
	       (if (and value? vcell?)
		   (loop `(,(cons var (fx+ count cell)) 
			   ,(cons (create-loc-list var) count)
			   ,@a-list)
			 (cdr vars)
			 (fx+ count (fx* CELL 2)))
		   (if value? 
		       (loop `(,(cons var count) ,@a-list)
			     (cdr vars)
			     (fx+ count CELL))
		       (loop `(,(cons (create-loc-list var) count) ,@a-list)
			     (cdr vars)
			     (fx+ count CELL))))))))))


(define (create-env-a-list pointer scratch)
  (do ((vars `(,@pointer . ,(sort-list! scratch scratch-compare)) (cdr vars))
       (count 0 (fx+ count (rep-size (variable-rep (car vars)))))
       (a-list '() `((,(car vars) . ,count) . ,a-list)))
      ((null? vars)
       (reverse! a-list))))

(define *dummy-var* (create-variable '*dummy-var*))

(define (create-closure link cics vars vframe-lambdas strategy)
  (let ((closure (make-closure)))
    (if (eq? strategy 'heap) 
        (walk cell-collapse vars)
        (walk (lambda (var) (set (variable-definition var) 'many)) vars))
    (receive (pointer scratch) (sort-vars vars)
      (let* ((scratch-slots (compute-scratch-slots scratch))
             (pvars (if (null? (cdr cics))
                        (if link (cons link pointer) pointer) 
                        (case (length pointer)
                            ((0)
                             (if link 
                                 (list link *dummy-var*)
                                 (list *dummy-var* *dummy-var*))) 
                            ((1)
                             (if link
                                 (list link (car pointer))
                                 (list *dummy-var* (car pointer))))
                            (else
                             (if link (cons link pointer) pointer)))))
             (pointer-slots (fx+ (length pvars) 
                                 (if cics (length cics) 1)))
             (var-a-list (create-env-a-list
                           (if cics 
                               `(,(car cics) ,@pvars ,@(cdr cics)) 
                               `(,*dummy-var* ,@pvars))
                           scratch)))            
          (set (closure-link closure) link)
          (set (closure-members closure) cics)   
          (set (closure-vframe-lambdas closure) vframe-lambdas)
          (set (closure-cit-offset closure) nil)
          (set (closure-env        closure) var-a-list)
          (set (closure-scratch    closure) scratch-slots)
          (set (closure-pointer    closure) (fx- pointer-slots 1))
          (set (closure-size       closure)
               (fx* (fx+ scratch-slots pointer-slots) CELL))
          (if (null? vframe-lambdas)
              (create-environments var-a-list closure cics)
              (create-vframe-environments closure vframe-lambdas))
          closure))))
                                                  
(define (cell-collapse var)
  (cond ((null? (variable-definition var))
         (set (variable-definition var) 
              (if (cell-collapsable? var) 'one 'many)))
        ((eq? (variable-definition var) 'one)
         (set (variable-definition var) 'many))))
       
(define (cell-collapsable? var)
  (every? (lambda (ref)
            (or (and (eq? (node-role ref) (call-arg 3))
                     (primop-ref? (call-proc (node-parent ref))
                                  primop/contents-location))
                (and (eq? (node-role ref) (call-arg 4))
                     (primop-ref? (call-proc (node-parent ref))
                                  primop/set-location))))
          (variable-refs var)))

(define (compute-scratch-slots scratch)
  (do ((vars scratch (cdr vars))
       (count 0 (fx+ count (rep-size (variable-rep (car vars))))))
      ((null? vars)
       (fixnum-ashr (fx+ count 3) 2))))           ; bytes->longwords

(define (create-environments var-a-list closure cics)
  (create-environment (variable-binder (car cics)) closure 0)
  (orbit-debug "~a (~d) ~s env = ~a~%" (lambda-strategy (variable-binder (car cics)))
          (object-hash (variable-binder (car cics)))
          (variable-name (car cics))
          (map (lambda (var) (variable-name (car var)))
               (closure-env closure)))
  (walk (lambda (cic)
          (create-environment (variable-binder cic)
                              closure
                              (cdr (assq cic var-a-list))))
        (cdr cics)))

(define (create-vframe-environments closure vframe-lambdas)
  (walk (lambda (cic)
          (set (lambda-env cic) nil))
         vframe-lambdas)
  (orbit-debug "~a (~d) ~s env = ~a~%" (lambda-strategy (car vframe-lambdas))
          (object-hash (car vframe-lambdas))
          (variable-name (lambda-self-var (car vframe-lambdas)))
          (map (lambda (var) (variable-name (car var)))
               (closure-env closure))) 
  (create-environment (node-parent (node-parent (car vframe-lambdas))) 
                      closure 0))



(define (create-environment node closure offset)
  (let ((env (make-environment)))
    (set (environment-closure    env) closure)
    (set (environment-cic-offset env) offset)
    (if (and (eq? 'unit-internal-template (lambda-env node))
             (neq? closure (car *unit-templates*)))
        (push *unit-templates* closure))
    (set (lambda-env node) env)))

(define (sort-vars vars)
  (iterate loop ((vars vars) (pointer '()) (scratch '()))
    (cond ((null? vars)
           (return pointer scratch))
           ((eq? (variable-rep (car vars)) 'rep/pointer)
            (loop (cdr vars) (cons (car vars) pointer) scratch))
           (else
            (loop (cdr vars) pointer (cons (car vars) scratch))))))

(define (bound-to-continuation? var)
  (and (variable-binder var)
       (any? (lambda (ref)
                 (let ((exits (call-exits (node-parent ref))))
                   (and (fx< exits 2)
                        (fx= (call-arg-number (node-role ref)) exits))))
               (variable-refs var))))


(define (continuation? node)
  (or (null? (lambda-variables node))
      (cond ((car (lambda-variables node))
             => (lambda (k) (not (bound-to-continuation? k))))
            (else t))))




(define (scratch-compare var1 var2)
  (fx> (rep-size (variable-rep var1)) (rep-size (variable-rep var2))))
