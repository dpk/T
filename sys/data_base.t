(herald properties (env tsys))

;;; Property lists implimented with tables.

(import t-implementation-env make-freelist
                             cons-from-this-freelist
                             return-to-this-freelist)


(define (property? obj)
  (and (pair? obj)
       (symbol? (car obj))
       (symbol? (cdr obj))))

(define (compare-property? key bucket)
  (and (eq? (car key) (car bucket))
       (eq? (cdr key) (cdr bucket))))

(define (property-hash key)
  (fx-xor (symbol-hash (car key)) (fx-ashl (symbol-hash (cdr key)) 8)))

(define property-freelist (make-freelist))

(define property-table
  (make-hash-table property? property-hash compare-property? nil 'property-table))

(define property
  (object (lambda (symbol property)
            (let* ((symbol   (enforce symbol? symbol))
                   (property (enforce symbol? property))
                   (key (cons-from-this-freelist property-freelist symbol property))
                   (val (table-entry property-table key)))
              (return-to-this-freelist property-freelist key)
              val))
          ((setter self) set-property)))

(define (set-property symbol property value)
  (let* ((symbol   (enforce symbol? symbol))
         (property (enforce symbol? property))
         (key      (cons-from-this-freelist property-freelist symbol property)))
    (set (table-entry property-table key) value)
    (return-to-this-freelist property-freelist key)
    (return)))

;;; Redundant cruft

(define-integrable (remove-property symbol property)
  (set (property symbol property) nil)
  (return))

(define-integrable get property)
(define-integrable put set-property)
