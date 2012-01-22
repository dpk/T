(herald struct (env tsys))

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

;;; (define-structure-type ship 
;;;   x
;;;   y  
;;;   (handler ((print self port) (...)) =>
;;; 
;;; (block (define ship-stype  (make-stype 'ship '(x y)))
;;;        (define make-ship   (stype-constructor ship-stype))
;;;        (define ship?       (stype-predicator ship-stype))
;;;        (define ship-x      (stype-selector ship-stype 'x))
;;;        (define ship-y      (stype-selector ship-stype 'y))
;;;        ship-stype)


;;; Structures look like this: e.g. (define-structure-type foo a b c) (VAX)

;;;    ----------------------------------------------
;;;--> |   foo-stype  template                      | -------
;;;    ----------------------------------------------       |
;;;    |        component a                         |       |
;;;    ---------------------------------------------|       |
;;;    |        component b                         |       | 
;;;    ---------------------------------------------|       |
;;;    |        component c                         |       |
;;;    ----------------------------------------------       |
;;;                                                         |
;;;                                                         |
;;;                                                         |
;;;                                                         |
;;;                                                         |
;;;    ---------------------------------------------------  |
;;;    |           *stype-template*                      |  |
;;;    ---------------------------------------------------  |
;;;    |                handler                          |  |
;;;    ---------------------------------------------------  |
;;;    |                predicator                       |  |
;;;    ---------------------------------------------------  |
;;;    |                constructor                      |  |
;;;    ---------------------------------------------------  |
;;;    |                selectors                        |  |
;;;    ---------------------------------------------------  |
;;;    |                master                           |  |
;;;    ---------------------------------------------------  |
;;;    |                id                               |  |
;;;    ---------------------------------------------------| |
;;;    |  pointer | scratch    |         8             | 0| |
;;;    ---------------------------------------------------| |
;;;    |  jump absolute opcode |     0      | template    |<-   H = 1, I = 0
;;;    ---------------------------------------------------|
;;;    |          *structure-template*                    |
;;;    ----------------------------------------------------
;;; 

(define (default-structure-handler stype)
  (object nil          
    ((crawl-exhibit self)
     (exhibit-structure self))
    ((print self port) (print-structure self port))
    ((structure-type self) stype)))


(define-operation (selector-id obj))              
        
(define-operation (structure-type obj) nil)    

(define-integrable (stype? obj)
  (and (extend? obj) (eq? (extend-header obj) *stype-template*)))
                             
(define-integrable (structure? obj)
  (true? (structure-type obj)))
     

(define (copy-structure struct)
  (let* ((struct (enforce structure? struct))
         (template (extend-header struct))
         (size (template-pointer-slots template)))
    (%copy-extend (%make-extend template size) struct size)))

(define (copy-structure! to-struct from-struct)
   (let ((to-struct (enforce structure? to-struct))
         (from-struct (enforce structure? from-struct)))
    (cond ((not (eq? (extend-header from-struct) (extend-header to-struct)))
           (copy-structure! (error "structure types don't match~%  ~s"
                                   `(copy-structure ,to-struct ,from-struct))
                            from-struct))
          (else
           (%copy-extend to-struct 
                         from-struct 
                         (template-pointer-slots (extend-header to-struct)))))))

    

(define (make-stype type-id specs handler)
  (let ((size (length specs)))
    (receive (template stype) (make-structure-template size)
      (set (stype-predicator stype)
           (object (lambda (obj)
                     (and (extend? obj) (eq? (extend-header obj) template)))
                   ((print self port)
                    (format port "#{Structure-predicator~_~S}" type-id))))
      (set (stype-constructor stype)
           (object (lambda ()
                     (copy-structure (stype-master stype)))
                   ((print self port)
                    (format port "#{Structure-constructor~_~s}"
                            type-id))))                    
      (set (stype-master stype)
           (%make-extend template size))
      (set (stype-selectors stype)
           (do ((index 0 (fx+ index 1))
                (specs specs (cdr specs))
                (sels '()
                      (cons (make-struct-selector stype
                                                  template
                                                  (car specs)
                                                  index)
                            sels)))
               ((null? specs) (reverse! sels))))
      (set (stype-id stype) type-id)
      (set (stype-handler stype) 
           (if handler
               (join handler (default-structure-handler stype))
               (default-structure-handler stype)))
      (let ((master (stype-master stype)))
        (walk1 (lambda (sel)
                 (set (sel master) *exhibit-structure-photon*))
              (stype-selectors stype)))
      stype)))

(define handle-stype
  (object nil
    ((print self port)
     (format port "#{Structure-type~_~s}" (stype-id self)))))

                                                           
(define (stype-selector stype id)
  (cond ((not (stype? stype))
         (error "attempt to take stype-selector of ~s which is not an stype"
                stype))
        ((mem (lambda (id sel) (eq? id (selector-id sel)))
              id
              (stype-selectors stype))
         => car)
        (else
         (error "structure type ~S has no such selector name~%  ~S"
                stype
                `(stype-selector ,stype ,id)))))


(define (print-structure struct port)
  (format port
          "#{Structure~_~s~_~s}"
          (stype-id (structure-type struct))
          (object-hash struct)))

(define (make-struct-selector stype template spec index)
  (let ((the-setter
         (lambda (obj new-value)
           (iterate loop ((obj obj))
             (cond ((and (extend? obj) 
                         (eq? (extend-header obj) template))
                    (set (extend-pointer-elt obj index) 
                         new-value))
                   (else
                    (loop (error '("attempt to alter the ~s component of ~s,~%"
                                 "which is not of structure type ~s, to be ~s")
                                 spec
                                 obj
                                 (stype-id stype)
                                 new-value))))))))
    (object (lambda (obj)
              (iterate loop ((obj obj))
                (cond ((and (extend? obj)
                            (eq? (extend-header obj) template))
                       (let ((probe (extend-pointer-elt obj index)))
                         (cond ((eq? *exhibit-structure-photon*
                                     probe)
                                (error '("attempt to access uninitialized ~s "
                                         "component of ~s")
                                       spec
                                       obj))
                               (else probe))))
                      (else
                       (loop
                        (error '("attempt to access the ~s component of ~s,~%"
                                 "which is not of structure type ~s")
                               spec
                               obj
                               (stype-id stype)))))))
            ((setter self) the-setter)
            ((selector-id self) spec)
            ((print self port)
             (format port "#{Selector~_~s~_~s}"
                     (stype-id stype)
                     spec)))))

(define (stype-compatible? stype id specs)
  (ignore id)
  (and (stype? stype)
       (iterate loop ((s specs)
                      (z (stype-selectors stype)))
         (cond ((null? s) (null? z))
               ((null? z) nil)
               ((neq? (car s) (selector-id (car z))) nil)
               (else (loop (cdr s) (cdr z)))))))

(define (exhibit-structure struct)
  (let ((sels (stype-selectors (structure-type struct))))
    (do ((s sels (cdr s))
         (i 0 (fx+ i 1)))
        ((null? s))
      (let ((val (extend-pointer-elt struct i))) ;;((car s) struct)))
        (crawl-print-component (selector-id (car s)) val)))))

(define *exhibit-structure-photon*      ; ugh
  (object nil
    ((print self port)
     (write-string port "Uninitialized structure slot"))))
