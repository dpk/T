(herald vector (env tsys))

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

;;; the size is converted to the number of longwords needed, represented as a
;;; t integer.  coincidentally (?) this is the number of longwords * 4 or
;;; the number of bytes to cons. we add 4 bytes for the header in size computation.
;;; vector types are limited to 23 bits for the number of elements

;(define-constant MAXIMUM-VECTOR-SIZE (fx- (fixnum-ashl 1 24) 1)) ; bootstrap?!
                                                                
;;; the MAXIMUM-VECTOR-SIZE is 23 bits.
(define-constant (acceptable-vector-size? i)   
  (and (fixnum? i) (fx>= i 0) (fx<= i 16777215)))

(define (make-bytev length)
  (let ((length (enforce acceptable-vector-size? length)))
    (make-vector-extend header/bytev length (fixnum-ashr (fx+ length 3) 2))))

(define (make-text length)
  (let ((length (enforce acceptable-vector-size? length)))
    (make-vector-extend header/text length (fixnum-ashr (fx+ length 3) 2))))

(define (make-vector length . fill)
  (let ((vec (make-vector-extend header/general-vector
                     (enforce acceptable-vector-size? length)
                     length))
        (fill (if (pair? fill) (car fill) '#f)))
    (if fill (vector-fill vec fill))
    vec))

(define (make-unit length)
  (make-vector-extend header/unit
                     (enforce acceptable-vector-size? length)
                     length))

(define (make-vcell id)
  (let ((v (make-vector-extend header/vcell 0 %%vcell-size)))
    (set (mref-integer v 0) header/nonvalue)
    (set (vcell-locations v) (make-weak-alist))
    (set (vcell-vcell-locations v) (make-weak-alist))
    (set (vcell-id v) id)
    v))

(define (make-foreign name)
  (let ((x (make-vector-extend header/foreign 0 2)))
    (set (foreign-name x) name)
    x))
  
(define (vector-replace target source len)
  (let ((target (enforce vector? target))
        (source (enforce vector? source))
        (len (enforce nonnegative-fixnum? len)))
    (%copy-extend target source len)))

(define (copy-vector vector)            
  (%copy-vector (enforce vector? vector)))

(define (copy-bytev bytev)
  (%copy-bytev (enforce bytev? bytev)))
           
(define (copy-text text)
  (%copy-text (enforce text? text)))

(define (vector . elements) (list->vector elements))

(define (list->vector l)
  (let ((l (enforce list? l)))
  (let ((len (length l)))
    (let ((vec (make-vector len)))
      (do ((i 0 (fx+ i 1))
           (l l (cdr l)))
          ((fx= i len) vec)
        (set (vref vec i) (car l)))))))

(define (vector->list v)
  (let ((v (enforce vector? v)))
    (do ((i (fx- (vector-length v) 1) (fx- i 1))
         (l '() (cons (vref v i) l)))
        ((fx< i 0) l))))

(define (vector-pos pred thing vector)
  (let ((len (vector-length vector)))
    (iterate loop ((i 0))
      (cond ((fx>= i len) nil)
            ((pred thing (vref vector i)) i)
            (else (loop (fx+ i 1)))))))

(define-integrable (vector-posq thing vector) (vector-pos eq? thing vector))

(define (walk-vector fn vec)
  (let ((vec (enforce vector? vec)))
    (let ((limit (fx- (vector-length vec) 1)))
      (cond ((fx>= limit 0)
             (iterate loop ((i 0))
               (cond ((fx>= i limit) 
                      (fn (vref vec i)))
                     (else
                      (fn (vref vec i))
                      (loop (fx+ i 1))))))))))


(define (%copy-vector vector)
  (let ((len (vector-length vector)))
    (%copy-extend (make-vector len) vector len)))  

(define (%copy-bytev bytev)
  (let ((len (bytev-length bytev)))
    (%copy-extend (make-bytev len)
                  bytev
                  (fixnum-ashr (fx+ len 3) 2))))

(define (%copy-text text)
  (let ((len (text-length text)))
    (%copy-extend (make-text len)
                  text
                  (fixnum-ashr (fx+ len 3) 2))))

(define (%copy-extend dest source cells)
  (do ((i 0 (fx+ i 1)))
      ((fx= i cells) dest)
    (set (extend-elt dest i) (extend-elt source i))))

(define (vector-fill vector value)      
  (let ((size (vector-length vector)))  
    (do ((i 0 (fx+ i 1)))
        ((fx>= i size) vector)
      (set (vref vector i) value))))

(define-handler general-vector
  (object nil
    ((hash self)
     (do ((i 0 (fx+ i 1))
          (h 0 (fx+ h (hash (vref self i)))))
         ((fx>= h (vector-length self)) h)))
    ((crawl-exhibit self)
     (exhibit-standard-extend self (vector-length self) 0 0))
    ((maybe-crawl-component self command)
     (cond ((and (nonnegative-fixnum? command)
                 (fx< command (vector-length self)))
            (crawl-push (vref self command)))
           (else nil)))
    ((print obj port)
     (write-char port *dispatch-char*)
     (write-char port *list-begin-char*)
     (iterate loop ((flag nil)
                    (i 0))
       (cond ((fx>= i (vector-length obj)))
             (else
              (if flag (space port))
              (cond ((fx>= i *print-length*)
                     (write-string port print-length-excess))
                    (else
                     (print (vref obj i) port)
                     (loop t (fx+ i 1)))))))
     (write-char port *list-end-char*))))

(define (*define-accessor name type offset)
  (let ((the-setter (lambda (x v)
		      (let ((x (enforce type x)))
			(set (extend-pointer-elt x offset) v)))))
    (object (lambda (x)          
	      (let ((x (enforce type x)))
		(extend-pointer-elt x offset)))
      ((setter self) the-setter)
      ((identification self) name))))

(define-operation (unguarded-accessor accessor))

(define (*define-vector-accessor name type fetch store)
  (let ((the-setter (lambda (x i v)
		      (cond ((not (type x))
			     (error "~s answered false to ~s" x (identification type)))
			    ((or (fixnum-less? i 0)
				 (fixnum-not-less? i (vector-length x)))
			     (error "~s index out of range"
				    (list 'set (list name x i) v)))
			    (else
			     (store x i v))))))
    (object (lambda (x i)
	      (cond ((not (type x))
		     (error "~s answered false to ~s" x (identification type)))
		    ((or (fixnum-less? i 0)
			 (fixnum-not-less? i (vector-length x)))
		     (error "~s index out of range"
			    (list name x i)))
		    (else
		     (fetch x i))))
      ((setter self) the-setter)
      ((identification self) name)
      ((unguarded-accessor self) fetch))))

