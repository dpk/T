(herald map (env tsys))

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

(define (map! proc l)
  (do ((z l (cdr z)))
      ((null? z) l)
    (set (car z) (proc (car z)))))

;;; Horrible cons-intensive definition.  Fix later.  There should be an
;;;  APPLY-MAP.

(define (map proc l . lists)
  (cond ((null? lists)
         (map1 proc l))
        (else
         (do ((l l (cdr l))
              (result '() (block0 (cons (apply proc (car l) (map1 car lists))
                                        result)
                                  (map! cdr lists))))
             ((or (null-list? l)
                  (any? null-list? lists))
              (reverse! result))))))

(define (mapcdr proc l . lists)
  (do ((l l (cdr l))
       (result '() (block0 (cons (apply proc l lists) result)
                           (map! cdr lists))))
      ((or (null-list? l)
           (any? null-list? lists))
       (reverse! result))))

(define (walk proc l . lists)
  (cond ((null? lists)
         (walk1 proc l))
        (else
         (do ((l l (cdr l)))
             ((or (null-list? l)
                  (any? null-list? lists))
              value-of-walk)
           (apply proc (car l) (map1 car lists))
           (map! cdr lists)))))

(define (walkcdr proc l . lists)
  (do ((l l (cdr l)))
      ((or (null-list? l)
           (any? null-list? lists))
       (undefined-value "value of WALKCDR"))
    (apply proc l lists)
    (map! cdr lists)))

(define (map-append proc l . lists)
  (apply append (apply map proc l lists)))

(define (map-append! proc l . lists)
  (apply append! (apply map proc l lists)))

;;; Thanks to Jim Meehan for the multiple list arguments.
;;; FUNCTIONS:
;;; ----------
;;; (ANY?      pred . lists)
;;; (EVERY?    pred . lists)
;;; (ANYCDR?   pred . lists)
;;; (EVERYCDR? pred . lists)
;;; (ANY       pred . lists)
;;; (EVERY     pred . lists)
;;; (ANYCDR    pred . lists)
;;; (EVERYCDR  pred . lists)

;;; test cases:
;;;
;;;     (any      pair? '())    => ()
;;;     (every    pair? '())    => t
;;;     (anycdr   pair? '())    => ()
;;;     (everycdr pair? '())    => ()
;;;
;;;     (any      pair? '(a))   => ()
;;;     (every    pair? '(a))   => ()
;;;     (anycdr   pair? '(a))   => t
;;;     (everycdr pair? '(a))   => ()
;;;
;;;     (any      pair? '((a))) => t
;;;     (every    pair? '((a))) => t
;;;     (anycdr   pair? '((a))) => t
;;;     (everycdr pair? '((a))) => ()
;;;
;;;     (any      null? '(() a)) => t
;;;     (every    null? '(() a)) => ()
;;;     (anycdr   null? '(() a)) => t
;;;     (everycdr null? '(() a)) => ()
;;;
;;;     (any      null? '())     => ()
;;;     (every    null? '())     => t
;;;     (anycdr   null? '())     => t
;;;     (everycdr null? '())     => t

;;; care must be taken here not to involve map in any's definition, since
;;; map calls any.


;(define (any pred l)    
;  (iterate any ((l l))
;    (cond ((null-list? l) nil)
;          ((pred (car l)))
;          (else (any (cdr l))))))

(define (any pred . lists) 
  (cond ((null? lists) (any-every-error 'ANY))
        ((null? (cdr lists)) ;special case for 1 list (no consing)
         (labels (((f l)
                   (cond ((null-list? l) nil)
                         ((pred (car l)))
                         (else (f (cdr l))))))
           (f (car lists))))
        (else
          (let ((xx lists)
                (yy (null-copy lists)))
            (labels (((g x y)
                      (cond ((null? x)
                             (or (apply pred yy) (g xx yy)))
                            ((null-list? (car x)) nil)
                            (else (set (car y) (caar x))
                                  (set (car x) (cdar x))
                                  (g (cdr x) (cdr y))))))
              (g xx yy))))))

;(define (anycdr pred l)          
;  (iterate anycdr ((l l))
;    (cond ((atom? l) (pred l))            ; huh?
;          ((pred l))
;          (else (anycdr (cdr l))))))

(define (anycdr pred . lists)
  (cond ((null? lists) (any-every-error 'ANYCDR))
        ((null? (cdr lists))
         (labels (((f l)
                   (cond ((atom? l) (pred l))
                         ((pred l))
                         (else (f (cdr l))))))
           (f (car lists))))
        (else
          (let ((xx lists)
                (yy (null-copy lists)))
            (labels (((g x y v end?)
                      (cond ((null? x)
                             (cond ((apply pred yy))
                                   (end? nil)
                                   (else (g xx yy nil nil))))
                            ((atom? (car x))
                             (set (car y) (car x))
                             (g (cdr x) (cdr y) v t))
                            (else (set (car y) (car x))
                                  (set (car x) (cdar x))
                                  (g (cdr x) (cdr y) v end?)))))
              (g xx yy nil nil))))))

;(define (every pred l)              
;  (iterate every ((l l))
;    (cond ((null-list? l) t)
;          ((pred (car l)) => (lambda (x)
;                               (if (null? (cdr l)) x
;                                 (every (cdr l)))))
;          (else nil))))

(define (every pred . lists) 
  (cond ((null? lists) (any-every-error 'EVERY))
        ((null? (cdr lists))
         (labels (((g v l) (cond ((null? v) nil)
                                 ((null-list? l) v)
                                 (else (g (pred (car l)) (cdr l))))))
           (g t (car lists))))
        (else
          (let ((xx lists)
                (yy (null-copy lists)))
            (labels (((outer x y v)
                      (labels (((inner x y)
                                (cond ((null? x)
                                       (let ((v (apply pred yy)))
                                         (and v (outer xx yy v))))
                                      ((null-list? (car x)) v)
                                      (else (set (car y) (caar x))
                                            (set (car x) (cdar x))
                                            (inner (cdr x) (cdr y))))))
                        (inner x y))))
              (outer xx yy t))))))

;(define (everycdr pred l)   
;  (iterate everycdr ((l l))
;    (cond ((atom? l) (pred l))            ; huh?
;          ((not (pred l)) nil)
;          (else (everycdr (cdr l))))))

(define (everycdr pred . lists)
  (cond ((null? lists) (any-every-error 'EVERYCDR))
        ((null? (cdr lists))
         (labels (((f l)
                   (cond ((atom? l) (pred l))
                         ((not (pred l)) nil)
                         (else (f (cdr l))))))
           (f (car lists))))
         (else 
           (let ((xx lists)
                 (yy (null-copy lists)))
             (labels (((g x y end?)
                       (cond ((null? x)
                              (cond ((apply pred yy) =>
                                     (lambda (v)
                                       (if end? v 
                                           (g xx yy nil))))
                                    (else nil)))
                             ((atom? (car x))
                              (set (car y) (car x))
                              (g (cdr x) (cdr y) t))
                             (else (set (car y) (car x))
                                   (set (car x) (cdar x))
                                   (g (cdr x) (cdr y) end?)))))
               (g xx yy nil))))))
                                                        
(define (any?      pred . l) (if (apply any      pred l) t nil))
(define (anycdr?   pred . l) (if (apply anycdr   pred l) t nil))
(define (every?    pred . l) (if (apply every    pred l) t nil))
(define (everycdr? pred . l) (if (apply everycdr pred l) t nil))

;(define-integrable any?      (compose true? any))
;(define-integrable anycdr?   (compose true? anycdr))
;(define-integrable every?    (compose true? every))
;(define-integrable everycdr? (compose true? everycdr))

(define (any-every-error name)
  (error "~s must take at least 1 list." name))

(define (null-copy l)
  (do ((x '() (cons nil x))
       (l l (cdr l)))
      ((null? l) x)))

;;; Miscellany

(define (*and . x) (every identity x))
(define (*or  . x) (any   identity x))

(define-integrable (*and? . x) (every? identity x))
(define-integrable (*or?  . x) (any?   identity x))

;(define-syntax (and? . x) `(if (and ,@x) t nil))
;(define-syntax (or?  . x) `(if (or  ,@x) t nil))

(define-integrable (*if pred con alt) (if pred con alt))
