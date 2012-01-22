(herald m68bookkeep (env t (orbit_top defs)))

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

(define-constant *pointer-registers* 5)
(define-constant *scratch-registers* 6)
(define-constant *argument-registers* 3)
(define-constant *real-registers* 11)
(define-constant *pointer-temps* 64)
(define-constant *scratch-temps* 5)
(define-constant *no-of-registers* 
                 (+ *pointer-temps* *scratch-temps* *real-registers*))
 
(define-constant *maximum-number-of-arguments* *pointer-temps*)             

(define-constant S0 0)
(define-constant S1 1)
(define-constant S2 2)
(define-constant S3 3)
(define-constant S4 4)
(define-constant S5 5)
(define-constant NARGS 5)
(define-constant P 6)
(define-constant A1 7)
(define-constant A2 8)
(define-constant A3 9)
(define-constant AN 10)
(define-constant AN-1 9)
(define-constant TP -1)
(define-constant nil-reg -2)
(define-constant SP -3)
(define-constant TASK -4)
(define-constant SCRATCH -5)


(define *pos-list1* (make-vector 4))
(define *pos-list2* (make-vector 5))
  

(let ((base  '((7 . rep/pointer)
               (8 . rep/pointer)
               (9 . rep/pointer))))
  (set (vref *pos-list1* 0) (sublist base 0 0))
  (set (vref *pos-list1* 1) (sublist base 0 1))
  (set (vref *pos-list1* 2) (sublist base 0 2))
  (set (vref *pos-list1* 3) (sublist base 0 3)))


(let ((base  '((6 . rep/pointer)
               (7 . rep/pointer)
               (8 . rep/pointer)
               (9 . rep/pointer))))
  (set (vref *pos-list2* 0) (sublist base 0 0))
  (set (vref *pos-list2* 1) (sublist base 0 1))
  (set (vref *pos-list2* 2) (sublist base 0 2))
  (set (vref *pos-list2* 3) (sublist base 0 3))
  (set (vref *pos-list2* 4) (sublist base 0 4)))






(define (reg-positions i proc?)       
  (cond ((fx<= i (if proc? 4 3))
         (vref (if proc? *pos-list2* *pos-list1*) i))
        (else
         (append (if proc? (vref *pos-list2* 4) (vref *pos-list1* 3))
                 (make-num-list (fx- i (if proc? 4 3)))))))

(define (make-num-list amount)
  (let ((end (fx+ (fx+ *real-registers* *argument-registers*) amount)))
    (do ((i (fx+ *real-registers* *argument-registers*) (fx+ i 1))
         (l '() (cons (cons i 'rep/pointer) l)))
        ((fx>= i end) (reverse! l)))))





(define (do-trivial-lambda call-node node reg-rep)
  (let ((offset (environment-cic-offset (lambda-env node)))
        (reg (car reg-rep))
        (hack (lambda (from to)
                (cond ((register? to)
                       (emit m68/lea from to))
                      (else
                       (emit m68/pea from)
                       (generate-pop to))))))
    (cond ((eq? (lambda-strategy node) strategy/hack)
           (hack (reg-offset SP (fx+ offset 2)) reg))
          ((fx= offset 0)
           (generate-move AN reg))
          (else                   
           (hack (reg-offset AN offset) reg)))
    (cond ((reg-node (car reg-rep))
                => kill))
    (lock (car reg-rep))))


;;; MAKE-HEAP-CLOSURE The first member of the closure corresponds to the
;;; template so we call %make-extend with this template and the size of the
;;; closure to be created.  Then we fill in the slots with the need variables
;;; and the addresses of templates for any closure-internal-closures.

(define (make-heap-closure node closure)
  (if *assembly-comments?* (emit-comment "consing heap closure"))
  (let* ((members (closure-members closure))
         (template-binder (variable-binder (car members))))
    (walk (lambda (var)
            (lambda-queue (variable-binder var)))
          members)
    (free-register node AN)
    (let ((cl (environment-closure (lambda-env template-binder))))
      (cond ((closure-cit-offset cl)
             (let ((acc (lookup node cl nil)))
               (free-register node AN)
               (generate-move acc AN)))
            (else
             (generate-move-address (template template-binder) AN))))
    (lock AN)
    (let ((hack (generate-extend node (closure-size closure))))
      (lock hack)
      (walk (lambda (pair)
        (let ((var (car pair))
              (offset (cdr pair)))
          (cond ((eq? var *dummy-var*))
                ((memq? var members)
                 (generate-move-address (template (variable-binder var)) hack)
                 (generate-move hack (reg-offset AN (fx- offset tag/extend))))
                (else
                 (really-rep-convert node
                                     (access-value node var)
                                     (variable-rep var)
                                     (reg-offset AN
                                                 (fx- offset tag/extend))
                                     (variable-rep var))))))
        (cdr (closure-env closure)))
        (unlock hack))
      (unlock AN)))

(define (generate-extend node n)
  (free-register node S1)
  (free-register node S2)
  (generate-move (machine-num (fx- n CELL)) S1)   ;; don't include template
  (let ((reg (get-register 'pointer node '*)))
    (generate-slink-jump slink/make-extend)
    reg))

(define (exchange-hack movers)
  (if (fxn= (length movers) 2)
      '#f
      (destructure (((m1 m2) movers))
        (cond ((and (eq? (arg-mover-from-rep m1) (arg-mover-to-rep m1))
                    (eq? (arg-mover-from-rep m2) (arg-mover-to-rep m1))
                    (fx= (arg-mover-from m1) (arg-mover-to m2))
                    (fx= (arg-mover-from m2) (arg-mover-to m1))
                    (register? (arg-mover-from m1))
                    (register? (arg-mover-from m2)))
               (emit m68/exg (arg-mover-from m1) (arg-mover-to m1))
               '#t)
              (else '#f)))))

