(herald list (env tsys))

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


;;;; list utilities, part 1

(define-constant dot-char #\.)
(define-integrable (dot-char? ch) (char= ch dot-char))

(lset *list-begin-char*  #\left-paren)
(lset *list-end-char*    #\right-paren)

;;;; miscellaneous

(define (list . l) l)

(define (cons* first . rest)            ; hackola definition
  (cond ((null? rest) first)
        (else 
         (do ((r rest (cdr r))
                      (q rest r))
                  ((null? (cdr r))
                   (set (cdr q) (car r))
                   (set (car r) first)
                   (if (neq? r rest) (set (cdr r) rest))
                   r)))))

(define (losing-non-null-list x)
  (error "expected a list, but got an atom instead~%  (... . ~s)"
         x)
  t)

(define (length l)
  (do ((i 0. (fx+ i 1))
       (l l (cdr l)))
      ((null-list? l) i)))

;;; this really belongs elsewhere.
;;; incredibly hacked-up version to bum the shit out.

(define (map1 proc l)
  (cond ((null-list? l) '())
        (else
         (let ((result (%make-pair)))
           (set (car result) (proc (car l)))
           (iterate loop ((l (cdr l)) (r result))
             (cond ((null-list? l)
                    (set (cdr r) '())
                    result)
                   (else
                    (let ((q (%make-pair)))
                      (set (cdr r) q)
                      (set (car q) (proc (car l)))
                      (loop (cdr l) q)))))))))

(define-constant value-of-walk (undefined-value "value of WALK"))

(define (walk1 proc l)
  (iterate loop ((l l))
    (cond ((null-list? l) value-of-walk )
          (else
           (proc (car l))
           (loop (cdr l))))))

;;; mem = membership in list

(define (mem pred obj list)
  (iterate mem ((list list))
    (cond ((null-list? list) nil)
          ((pred obj (car list)) list)
          (else (mem (cdr list))))))

(define (memq obj list)
  (iterate memq ((list list))
    (cond ((null-list? list) nil)
          ((eq? obj (car list)) list)
          (else (memq (cdr list))))))

(define (bad-list-index . rest)
  (error "illegal index into list~%  ~s" rest))

(define (nthcdr list index)
  (let ((index (enforce nonnegative-fixnum? index)))
    (iterate loop ((l list) (i index))
      (cond ((fx= i 0) l)
            ((null-list? l)
             (bad-list-index 'nthcdr list index))
            (else (loop (cdr l) (fx- i 1)))))))

;;; set-nthcdr

(define nth
  (object (lambda (list index)
            (let ((index (enforce nonnegative-fixnum? index)))
              (iterate loop ((l list) (i index))
                (cond ((null-list? l)
                       (bad-list-index 'nth list index))
                      ((fx= i 0) (car l))
                      (else (loop (cdr l) (fx- i 1)))))))
          ((setter self) set-nth)))

(define (set-nth %%list index value)
  (let ((index (enforce nonnegative-fixnum? index)))
    (iterate loop ((l %%list) (i index))
      (cond ((null-list? l)
             (bad-list-index 'set `(nth ,%%list ,index) value))   ; whattahack
            ((fx= i 0) (set (car l) value))
            (else (loop (cdr l) (fx- i 1)))))))

(define last
  (object (lambda (list)
            (car (lastcdr list)))
          ((setter self) set-last)))

(define (set-last list value)
  (set (car (lastcdr list)) value))

(define (lastcdr list)
  (let ((list (enforce pair? list)))
    (iterate loop ((list list))
      (if (not (pair? (cdr list))) 
          list
          (loop (cdr list))))))


;;;; reverse, append, ...
;;;           maclisp                           <t>

;;;     safe            unsafe          safe            unsafe
;;;
;;;     reverse         nreverse        reverse         reverse!
;;;     append          nconc           append          append!
;;;     ---             nreconc         append-reverse  append-reverse!

(define (append . lists)
  (labels (((append2 l1 l2)
            (if (null-list? l1) l2
                (cons (car l1) (append2 (cdr l1) l2)))))
    (cond ((null? lists) '())
          ((null? (cdr lists)) (car lists))
          ((null? (cddr lists)) (append2 (car lists) (cadr lists)))
          (else (append (car lists) (apply append (cdr lists)))))))

(define (append! . lists) ; note: (append! 'atom 'anything) => anything
  (cond ((null? lists)  '())
        ((null? (cdr lists)) (car lists))
        ((null? (cddr lists))
         (cond ((null-list? (car lists))
                (cadr lists))
               (else
                (set (cdr (lastcdr (car lists)))
                     (cadr lists))
                (car lists))))
        (else (append! (car lists)
                       (apply append! (cdr lists))))))

(define-recursive (append-reverse list seed)
  (if (null-list? list) 
    seed
    (append-reverse (cdr list) (cons (car list) seed))))

(define (append-reverse! old-list seed)
  (cond ((null-list? old-list) seed)
        (else
         (iterate loop ((old-cdr (cdr old-list))
                        (old-car seed)
                        (tail    old-list))
           (cond ((null-list? (cdr tail))
                  (set (cdr tail) old-car)
                  tail)
                 (else
                  (set (cdr tail) old-car)
                  (loop (cdr old-cdr) tail old-cdr)))))))

(define (reverse list)
  (append-reverse list '()))

(define (reverse! old-list)
  (append-reverse! old-list '()))

(define (copy-list list) (append list '()))

;;; ass = association-list lookup

(define (ass pred obj list)
  (iterate loop ((list list))
    (cond ((null-list? list) nil)
          ((pred obj (caar list)) (car list))
          (else (loop (cdr list))))))

(define-integrable (ass? pred obj list)
  (if (ass pred obj list) t nil))

(define (assq obj list)
  (iterate loop ((l list))
    (if (null-list? l) 
        nil
        (let ((z (car l)))
          (cond ((not (pair? z))
                 (loop (cons (error '("association list contains non-pair~%"
                                      "  (assq ~s ~s)")
                                    obj
                                    list)
                             (cdr l))))
                ((eq? obj (car z)) z)
                (else (loop (cdr l))))))))

(define-integrable (assq? obj list) (true? (assq obj list)))

;;; questionable cruft.  print-char wants rass.

(define (rass pred item alist)
  (iterate loop ((a alist))
    (cond ((null-list? a) nil)
          ((pred item (cdar a)) (car a))
          (else (loop (cdr a))))))

(define (rassq item alist)
  (iterate loop ((a alist))
    (cond ((null-list? a) nil)
          ((eq? item (cdar a)) (car a))
          (else (loop (cdr a))))))


(define (circular? move x)
  (if (null-list? x) 
      nil
      (iterate race ((slow-runner x) (fast-runner (move x)))
        (cond ((or (null-list? fast-runner) 
                   (null-list? (move fast-runner))) 
               nil)
              ((eq? slow-runner fast-runner) t) ;fast runner caught up!
              (else
               (race (move slow-runner) (move (move fast-runner))))))))

(define (proper-list? x)
  (if (atom? x) 
      (null? x)
      (proper-list? (cdr x))))

(define (sublist l start count)
  (iterate loop ((i count)
                 (ll (nthcdr l start))
                 (result '()))
    (cond ((fx<= i 0) (reverse! result))
          ((null-list? ll)
           (error "argument list is too short~%  (~s ~s ~s ~s)"
                  'sublist l start count))
          (else
           (loop (fx- i 1)
                 (cdr ll)
                 (cons (car ll) result))))))



;;; del = deletion from list

(define (del pred obj list)
  (iterate del ((list list))
    (cond ((null-list? list) '())
          ((pred obj (car list))
           (del (cdr list)))
          ((mem pred obj (cdr list))
           (cons (car list) (del (cdr list))))
          (else list))))

(define-integrable (delq obj list) (del eq? obj list))

(define (del! pred obj list)     
  (iterate del! ((list list))
    (cond ((null-list? list) '())
          ((pred obj (car list)) (del! (cdr list)))
          (else (set (cdr list) (del! (cdr list)))
                list))))


(define (delq! obj list)     
  (iterate delq! ((list list))
    (cond ((null-list? list) '())
          ((eq? obj (car list)) (delq! (cdr list)))
          (else (set (cdr list) (delq! (cdr list)))
                list))))



;;;; questionable cruft.  t compiler wants it.  print-char wants rass.
;;; what to do about sequence functions?  be common-lisp compatible?

(define (pos pred obj l)
  (iterate loop ((l l)
                 (n 0))
    (cond ((null-list? l) nil)
          ((pred obj (car l)) n)
          (else (loop (cdr l) (fx+ n 1))))))

(define-integrable (posq obj l) (pos eq? obj l))


;;; make this open-coded someday.
(define (displace x y)
  (let ((x (enforce pair? x))
        (y (enforce pair? y)))
    (set (car x) (car y))
    (set (cdr x) (cdr y))
    x))

;;; The pair handler

(define handle-pair
  (object nil
    ((hash self)
     (if (null? self) 0 (fx+ (hash (car self)) (hash (cdr self)))))
    ((display obj port)
     (write-pair port obj 0 nil))
    ((print obj port)
     (write-pair port obj 0 t))
    ((pretty-print obj port) (pp-list obj port))
    ((maybe-crawl-component pair command)
     (cond ((and (nonnegative-fixnum? command)
                 (fx< command (length pair)))
            (crawl-push (nth pair command)))
           (else nil)))))

(define print-level-excess "(...)")
(define print-length-excess "...")
(lset *print-level*  most-positive-fixnum)
(lset *print-length* most-positive-fixnum)

;;; Recursively prints the object.  Currently doesn't check for
;;; circular structure.

(define (write-pair port obj level slashify?)
  (let ((writec (if (iob? port) vm-write-char write-char))
        (writes (if (iob? port) vm-write-string write-string)))
    (cond ((null? obj)
           (writec port *list-begin-char*)
           (writec port *list-end-char*))
          ((atom? obj)               (print obj port))
          ((not (reasonable? obj))   (print-random obj port))
          ((fx> level *print-level*) (writes port print-level-excess))
          (else
           (writec port *list-begin-char*)
           (iterate loop ((l obj) (n 0) (flag '#f))
             (cond ((atom? l)
                    (cond ((not (null? l))
                           (space port)
                           (writec port dot-char)
                           (space port)
                           (write-pair port l (fx+ level 1) slashify?))))
                   (else
                    (if flag (space port))
                    (cond ((fx>= n *print-length*)
                           (writes port print-length-excess))
                          (else
                           (write-pair port (car l) (fx+ level 1) slashify?)
                           (loop (cdr l) (fx+ n 1) '#t))))))
           (writec port *list-end-char*)))))
