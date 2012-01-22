(herald type_sets)

;;; The various difficulties with n-ary procs are blithely ignored.

(define (simple-member? t1 t2)
  (fxn= 0 (logand t1 t2)))

(define (simple-union t1 t2)
  (logior t1 t2))

(define (simple-intersection t1 t2)
  (logand t1 t2))

(define (simple-subtract t1 t2)
  (logand t1 (lognot t2)))

(define (type-intersection type1 type2)
  (cond ((type-top? type1)
         type2)
        ((type-top? type2)
         type1)
        (else
         (create-type (simple-intersection (type-simple type1)
                                           (type-simple type2))
                      (proc-intersection (type-proc type1)
                                         (type-proc type2))))))

(define (type-union type1 type2)
  (cond ((or (type-top? type1)
             (type-top? type2))
         type/top)
        (else
         (create-type (simple-union (type-simple type1) (type-simple type2))
                      (proc-union (type-proc type1) (type-proc type2))))))

(define (type-subset? type1 type2)
  (or (type-top? type2)
      (and (fx= 0 (simple-subtract (type-simple type1) (type-simple type2)))
           (proc-subset? (type-proc type1) (type-proc type2)))))

(define (type-subtract type1 type2)
  (create-type (simple-subtract (type-simple type1) (type-simple type2))
               (proc-subtract (type-proc type1) (type-proc type2))))

(define (proc-mapper proc args1 args2)
  (cond ((fxn= (length args1)
               (length args2))
         '())
        (else
         (let ((args (map proc args1 args2)))
           (if (any? type-bottom? args)
           '()
           args)))))

(define (proc-union args1 args2) '())
;  (proc-mapper type-intersection args1 args2))

(define (proc-intersection args1 args2) '())
;  (proc-mapper type-union args1 args2))

(define (proc-subset? args1 args2) t)
;  (null? (proc-mapper type-subset args2 args1)))

(define (proc-subtract args1 args2) '()) 
;  (if (null? args2)
;      args1
;      (bug "subtracting non-empty procedure type ~S" args2)))


