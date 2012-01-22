(herald (orbit_top util)
  (env t (orbit_top defs)))

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

;;; Messages
;;;==========================================================================

(lset *noise-stream*   t)  ;So NOISE works at top level.
(lset *noise-flag*     t)
(lset *debug-flag*     t)
(lset *noise+error*    t)                                     
(lset *noise+terminal* t)                                     

(define (bug f . rest)
  (apply error (list f "~%  (compiler error)") rest))

(define (orbit-warning f . rest)
  (apply format *noise+error* (list "~%;** Warning: " f) rest))

(define (noise f . rest)
  (apply format (if *noise-flag* *noise+terminal* *noise-stream*)
                (list "~&;;; " f) rest))

(define (orbit-debug . args)
  (if *debug-flag*
      (apply format (terminal-output) args)))

;;; (PP-CPS node . stream)
;;;===========================================================================
;;; Print CPS node tree in linear form.  Stream defaults to terminal output.

(define (pp-cps node . stream)
  (pp-cps-1 node 4 (if stream
                       (car stream)
                       (terminal-output))))

(define (pp-cps-1 node indent-to stream)
  (let ((z (pp-cps-2 node)))
    (cond ((lambda-node? node)
           (pp-cps-lambda node indent-to stream))
          ((object-node? node)
           (pp-cps-object node indent-to stream)))
    z))

(define (pp-cps-lambda node indent-to stream)
  (let ((vars (lambda-all-variables node)))
    (format stream "~S" (object-hash node))
    (set (hpos stream) indent-to)
    (writec stream #\()
    (format stream "~S" (map variable-unique-name vars))
    (set (hpos stream) (fx+ indent-to 18))     ;format sux
    (pretty-print (pp-cps-2 (lambda-body node)) stream)
    (format stream ")~%")
    (pp-cps-list (call-proc+args (lambda-body node)) indent-to stream)))

(define (pp-cps-object node indent-to stream)
  (format stream "~S" (object-hash node))
  (set (hpos stream) indent-to)
  (writec stream #\()
  (writec stream #\@)
  (format stream "~D" (object-number node))
  (set (hpos stream) (fx+ indent-to 18))     ;format sux
  (pretty-print `(,(pp-cps-2 (object-proc node))
                  ,(map pp-cps-2 (object-operations node))
                  ,(map pp-cps-2 (object-methods node)))
                stream)
  (format stream ")~%")
  (pp-cps-list (object-proc-pair node) indent-to stream)
  (pp-cps-list (object-operations node) indent-to stream)
  (pp-cps-list (object-methods node) indent-to stream))

(define (pp-cps-list list indent-to stream)
  (walk (lambda (node) (pp-cps-1 node (fx+ indent-to 1) stream))
        list))

(define (pp-cps-2 node)
  (cond ((not (node? node))
         `(not-a-node ,node))
        (else
         (xselect (node-variant node)
           ((lambda-node?)
            (lambda-name node))
           ((leaf-node?)
            (case (leaf-variant node)
                  ((literal)
                   `',(literal-value node))
                  ((primop)
                   (if (primop? (primop-value node)) ; Hack for DK's LAP junk.
                       (concatenate-symbol
                        "$"
                        (any-primop-id (primop-value node)))
                       (concatenate-symbol "$_"
                                           (object-hash (primop-value node)))))
                  (else
                   (variable-unique-name (reference-variable node)))))
           ((call-node?)
            (let ((stuff (map pp-cps-2 (call-proc+args node))))
              `(,(car stuff) ,(call-exits node) . ,(cdr stuff))))
           ((object-node?)
            (concatenate-symbol '@ (object-number node)))))))

(define (lambda-name node)
  (concatenate-symbol "^"
                      (variable-name (lambda-self-var node))
                      "_"
                      (variable-id (lambda-self-var node))))

;;; Returns a lexically unique name for the variable.

(define (variable-unique-name var)
  (cond ((variable? var)
         (let ((name (variable-name var)))
           (cond ((variable-binder var)
                  (concatenate-symbol name "_" (variable-id var)))
                 (else
                  name))))
        ((primop? var)
         (identification var))
        (else
         var)))

;;; Little utilities.
;;;========================================================================

(define (find pred l)
  (iterate loop ((l l))
    (cond ((null? l) nil)
          ((pred (car l)) (car l))
          (else (loop (cdr l))))))

(define (filter pred l)
  (iterate loop ((l l) (r '()))
    (cond ((null? l) (reverse! r))
          ((pred (car l)) (loop (cdr l) (cons (car l) r)))
          (else (loop (cdr l) r)))))

(define (filter! pred list)
  (iterate filter! ((list list))
    (cond ((null-list? list) '())
          ((pred (car list)) (set (cdr list) (filter! (cdr list))) list)
          (else (filter! (cdr list))))))

(define (select-from-table pred table)
  (let ((res '()))
    (table-walk table
                (lambda (key entry)
                  (if (pred key entry)
                      (push res `(,key . ,entry)))))
    res))

(define (table->list table)
  (select-from-table true table))

(define (table-push table key val)
  (modify (table-entry table key)
          (lambda (old)
            (if old
                (cons val old)
                (list val))))
  val)

(define (table-pop table key)
  (pop (table-entry table key)))

(define (free-table-push table key val)
  (modify (table-entry table key)
          (lambda (old)
            (if old
                (cons-from-freelist val old)
                (cons-from-freelist val '()))))
  val)

(define (free-table-pop table key)
  (let ((x (table-entry table key)))
    (cond ((null? x)
           '#f)
          (else
           (set (table-entry table key) (cdr x))
             (let ((y (car x)))
               (return-to-freelist x)
               y)))))

(define (merge-lists x y)
  (cond ((null? y) x)
        (else (do ((z x (cdr z))
                   (u y (let ((w (car z)))
                          (if (memq? w u) u (cons w u)))))
                  ((null? z) u)))))

(define (partition-list pred l)
  (iterate loop ((l l) (yes '()) (no '()))
    (cond ((null? l)
           (return (reverse! yes) (reverse! no)))
          ((pred (car l))
           (loop (cdr l) (cons (car l) yes) no))
          (else
           (loop (cdr l) yes (cons (car l) no))))))




