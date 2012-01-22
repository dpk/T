(herald hash
  (env tsys))

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

;;; Global hash table of weak pointers

;;; Thanks to Gerry Sussman, Carl Hewitt, and Multics Maclisp

;;; (object-hash obj)  =>  fixnum
;;;    Generates a unique numeric id for obj.
;;;
;;; (object-unhash fixnum)  =>  obj
;;;    Returns the object which has given id, if the object hasn't
;;;    been deleted by the GC.  Returns #F if object no longer exists.
;;;
;;; (eq? a b) if and only if (= (hash a) (hash b)).
;;;
;;; Numbers returned by "object-hash" really are unique - even if
;;; the object goes away, the number won't be recycled.  Ha.

;;; New version using weak table from table package.  Not much left
;;; here.  Keeping only one table makes OBJECT-UNHASH a little slow.
;;; Shouldn't matter. -- But it does, I have seen OBJECT-UNHASH take
;;; 15+ seconds on a Apollo Tern.  The problem is probably paging time
;;; which should be reduced with the new table implementation.

;;; These are not normal weak tables, the GC deals with them specially.
;;; OBJECT-HASH-TABLE must be weak in the values instead of the keys and
;;; neither will drop a pointer to a symbol.

;++ These should be LOCAL
(lset object-hash-table   (make-weak-table 'object-hash-table))
(lset object-unhash-table (make-weak-table 'object-unhash-table))
(lset generator -1)

;;; Could keep separate hash tables for gc-copyable vs. static
;;; objects.  Stars and planets?

(define object-hash
  (object (lambda (obj)
            (cond ((weak-table-entry object-hash-table obj))
                  (else
                   (defer-interrupts
                    (set generator (fx+ generator 1))
                    (if (fx>= generator most-positive-fixnum)
                        (error "cannot generate weak pointer - out of UID's"))
                    (set (weak-table-entry object-hash-table obj) generator)
                    (set (weak-table-entry object-unhash-table generator) obj)
                    generator))))
    ((re-initialize self)
     (set object-hash-table (make-weak-table 'object-hash-table))
     (set object-unhash-table (make-weak-table 'object-unhash-table))
     (set generator 0))))

(define (object-unhash n)
  (let ((n (enforce nonnegative-fixnum? n)))
    (weak-table-entry object-unhash-table n)))

(define @ object-unhash)
