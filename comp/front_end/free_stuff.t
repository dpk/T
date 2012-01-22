(herald free_stuff)

(define (flist1 a x)
  (cons-from-freelist a x))

(define (flist2 a b x)
  (cons-from-freelist a 
  (cons-from-freelist b x)))

(define (flist3 a b c x) 
  (cons-from-freelist a
  (cons-from-freelist b
  (cons-from-freelist c x))))

(define (flist4 a b c d x)
  (cons-from-freelist a
  (cons-from-freelist b
  (cons-from-freelist c
  (cons-from-freelist d x)))))

(define (flist5 a b c d e x)
  (cons-from-freelist a
  (cons-from-freelist b
  (cons-from-freelist c
  (cons-from-freelist d
  (cons-from-freelist e x))))))

(define (free-copy-list l)
  (do ((l l (cdr l))
       (f '() (cons-from-freelist (car l) f)))
      ((null? l)
       (reverse! f))))

(define (free-copy-tree tree)
  (iterate label ((tree tree))
    (cond ((pair? tree)
           (cons-from-freelist (label (car tree))
                               (label (cdr tree))))
          (else
           tree))))

(define (return-tree-to-freelist tree)
  (iterate label ((tree tree))
    (cond ((pair? tree)
           (label (car tree))
           (label (cdr tree))
           (return-to-freelist tree))))
  (return))

