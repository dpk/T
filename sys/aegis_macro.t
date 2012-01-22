(herald aegis_macros (env tsys))

(define-syntax (define-enumerated type-name . types)
  (ignore type-name)
  (do ((i 0 (fx+ i 1))
       (l types (cdr l))
       (z '() (cons `(define-constant ,(car l) ,i) z)))
      ((null? l)
       `(block ,@(reverse! z)))))

(define-syntax (define-set-of type-name . types)
  (ignore type-name)
  (do ((i 1 (fixnum-ashl i 1))
       (l types (cdr l))
       (z '() (cons `(define-constant ,(car l) ,i) z)))
      ((null? l)
       `(block ,@(reverse! z)))))

(define-syntax (set-of . types)
  (if (null? types) 
    0
    (iterate loop ((types types))
      (cond ((null? (cdr types)) (car types))
            (else
             `(fx-ior ,(car types) ,(loop (cdr types))))))))


(declare-tsys-syntax-exports 'define-enumerated 'define-set-of 'set-of)

