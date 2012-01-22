(herald type
  (env t (orbit_top defs)))

(define (set-consistant-type var type)
    (cond ((or (eq? type type/top)
               (eq? type (variable-type var))))
          ((eq? (variable-type var) type/top)
           (set (variable-type var) type))
          (else
           (error "Type conflict with variable ~s" var))))

(define (will-occur? binding-node ref-node)
  (iterate loop ((node (lambda-body binding-node)))
    (cond ((eq? node ref-node) '#t)
          (else
           (xselect (call-exits node)
             ((2) '#f)
             ((1)
              (let ((exit ((call-arg 1) node)))
                (if (lambda-node? exit)
                    (loop (lambda-body exit))
                    '#f)))
             ((0)
              (let ((exit (call-proc node)))
                (cond ((lambda-node? exit)
                       (loop (lambda-body exit)))
                      ((variable-known (reference-variable exit))
                       => (lambda (label)
                            (loop (lambda-body label))))
                      (else '#f)))))))))
           
(define (argument-type-wants var node)
  (let* ((proc (call-proc (node-parent node)))
         (type 
           (cond ((primop-node? proc)
                  (let ((type (primop.type (primop-value proc) 
                                           (node-parent node))))
                    (if (and type (proc-type? type))
                        (vref (proc-type-args type) 
                              (relation-index (node-role node)))
                        type/top)))
                 ((variable-known (reference-variable proc))
                  => (lambda (label)
                       (cond ((lambda-rest-var label) type/top)
                             ((variable-type 
                               (nth (lambda-variables label)
                                    (fx- (relation-index (node-role node)) 1)))))))
                 (else type/top))))
    (cond ((eq? type type/top)
           (return type/top '#f))
          ((will-occur? (variable-binder var) (node-parent node))
           (return type '#t))
          (else
           (return type '#f))))) 
                            
(define (argument-type-is node)
  (xcond ((lambda-node? node) type/top)
         ((reference-node? node)
          (variable-type (reference-variable node)))
         ((literal-node? node)
          (cond ((fixnum? (literal-value node)) type/fixnum)
                ((char? (literal-value node)) type/char)
                (else type/top)))))
                         
(define (type-analyze-uses var)
  (and var
  (set (variable-type var)
       (iterate loop ((refs (variable-refs var)) (type '#f))
         (cond ((null? refs) (or type type/top))
               (else
                (receive (ty proof?) (argument-type-wants var (car refs))
                  (cond (proof? ty)
                        ((or (eq? ty type/top)
                             (and type (neq? ty type)))
                         type/top)
                        (else
                         (loop (cdr refs) ty))))))))))
    
(define (type-analyze-defs bound-var i)
  (iterate loop ((refs (variable-refs bound-var)) (type '#f))
    (cond ((null? refs) type)
          (else
           (let ((ty (argument-type-is ((call-arg i) (node-parent (car refs))))))
             (cond ((or (eq? ty type/top)
                        (and type (neq? ty type)))
                    type/top)
                   (else
                    (loop (cdr refs) ty))))))))
                                         
(define (type-analyze-open-continuation var)
  (set-consistant-type var (primop-result-type var)))


(define (type-analyze-bound-lambda var node)
  (receive (start-i start-vars)
           (if (continuation? node)
               (return 1 (lambda-variables node))
               (return 2 (cdr (lambda-variables node))))
    (do ((i start-i (fx+ i 1))
         (vars start-vars (cdr vars)))
        ((null? vars))
      (set-consistant-type (car vars) (type-analyze-defs var i)))))

  
(define (type-analyze node)
  (let ((proc (call-proc node)))
    (cond ((lambda-node? proc)
           (type-analyze-let node))
          ((primop-ref? proc primop/y)
           (type-analyze-y node))
          (else
           (walk type-analyze-argument (call-proc+args node))))))

(define (type-analyze-argument node)
  (cond ((lambda-node? node)
         (walk type-analyze-uses (if (continuation? node)
                                     (lambda-variables node)
                                     (cdr (lambda-variables node))))
         (type-analyze (lambda-body node))
         (select (lambda-strategy node)
           ((strategy/open)
            (walk type-analyze-open-continuation (lambda-variables node)))))))

(define (type-analyze-let node)                                               
  (type-analyze (lambda-body (call-proc node)))
  (walk (lambda (var node)
          (cond ((and var (lambda-node? node))
                 (type-analyze (lambda-body node))
                 (if (eq? (lambda-strategy node) strategy/label)
                     (type-analyze-bound-lambda var node)))))
        (lambda-variables (call-proc node))
        (call-args node)))                                   

(define (type-analyze-y node)
  (destructure* (((cont master) (call-args node)))
    (let ((labels (call-args (lambda-body master))))
      (if (lambda-node? cont) (type-analyze (lambda-body cont)))
      (type-analyze (lambda-body (car labels)))
      (walk type-analyze-argument (cdr labels))
      (select (lambda-strategy master)
        ((strategy/label strategy/ezclose strategy/vframe)
         (walk type-analyze-bound-lambda
               (cdr (lambda-variables master))
               (cdr labels)))))))
            
(define (type-analyze-top node)
  (type-analyze (lambda-body node))
  (type-analyze (lambda-body node)))

