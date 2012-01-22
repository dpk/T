(herald m86_weak
  (env tsys))

;;; M68CONSTANTS

(define-constant header/weak-set   #b1000101)   ; Number headers...
(define-constant header/weak-alist #b1001101)
(define-constant header/weak-table #b1010101)

;;; M68PRIMOPS

(define-local-syntax (define-type-predicate name variant . rest)
  `(define-constant ,name
     ,(xcase variant
        ((and)
         `(make-and-type-predicate ',name . ,rest))
        ((header)
         `(make-header-type-predicate ',name . ,rest)))))

(define-type-predicate weak-set-header?   header header/weak-set)
(define-type-predicate weak-alist-header? header header/weak-alist)
(define-type-predicate weak-table-header? header header/weak-table)

;;; PREDICATES

(define-local-syntax (define-extend-predicate type . header-type)
  (let ((header-type (if (atom? header-type) type (car header-type))))
    `(define-constant (,(concatenate-symbol type '?) x)
       (and (extend? x)
            (,(concatenate-symbol header-type '-header?) (extend-header x))))))

(define-extend-predicate weak-set)
(define-extend-predicate weak-alist)
(define-extend-predicate weak-table)

;;; LOCATIONS

(define-local-syntax (define-accessor name offset arg-type qtype . contents)
  (let* ((s-type (if (eq? arg-type 'list) 'pair arg-type))
         (c-type (if (null? contents) 'top (car contents)))
         (type (->type `(object (proc #f (proc #f ,c-type) ,arg-type)
                          (setter #f (proc #f (proc #f ,s-type ,c-type))))))
         (contents-type (->type `(proc #f (proc #f ,c-type) top ,arg-type)))
         (set-type (->type `(proc #f (proc #f) top ,c-type ,s-type))))
    `(define-constant ,name
                      (make-location ',name
                                     ,(if (eq? arg-type 'list)
                                          (fx- (fx* offset 4) 3)
                                          (fx+ (fx* offset 4) 2))
                                     'rep/pointer
                                     ',qtype
                                     1
                                     ',type
                                     ',contents-type
                                     ',set-type))))

(define-accessor weak-set-elements   0 top weak-set?)    
(define-accessor weak-alist-elements 0 top weak-alist?)    
(define-accessor weak-table-table    0 top weak-table?)
(define-accessor weak-table-vector   1 top weak-table?)

;;; anywhere

(define-integrable (weak-semaphore-set? weak)
  (not (alt-bit-set? weak)))

(define-integrable (set-weak-semaphore weak)
  (cond ((weak-semaphore-set? weak)
         (error "simultaneous access on weak ~S" weak))
        (else
         (clear-alt-bit! weak))))

(define-integrable (clear-weak-semaphore weak)
  (set-alt-bit! weak))




