(herald carcdr (env tsys (t3_primops aliases) (t3_primops open)))

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

;;;; car/cdr chains

(define-local-syntax (define-carcdr name)
  (let* ((x (symbol->string name))
         (ads (string-slice x 1 (- (string-length x) 2)))
         (sym (generate-symbol 'carcdr))
         (val (generate-symbol 'val))
         (stuff (do ((i (- (string-length ads) 1) (- i 1))
                     (stuff sym (list (xcase (char-upcase (nthchar ads i))
                                        ((#\A) 'CAR)
                                        ((#\D) 'CDR))
                                      stuff)))
                    ((< i 0) stuff))))
    `(block
      (define-constant ,name
        (object (lambda (,sym) ,stuff)
          ((setter self)
           (lambda (,sym ,val)
             (set ,stuff ,val)))))
      (declare type-safe-closed-form ,name))))

(define-carcdr caar)     
(define-carcdr cadr)
(define-carcdr cdar)
(define-carcdr cddr)

(define-carcdr caaar)
(define-carcdr caadr)
(define-carcdr cadar)
(define-carcdr caddr)
(define-carcdr cdaar)
(define-carcdr cdadr)
(define-carcdr cddar)
(define-carcdr cdddr)

(define-carcdr caaaar)
(define-carcdr caaadr)
(define-carcdr caadar)
(define-carcdr caaddr)
(define-carcdr cadaar)
(define-carcdr cadadr)
(define-carcdr caddar)
(define-carcdr cadddr)
(define-carcdr cdaaar)
(define-carcdr cdaadr)
(define-carcdr cdadar)
(define-carcdr cdaddr)
(define-carcdr cddaar)
(define-carcdr cddadr)
(define-carcdr cdddar)
(define-carcdr cddddr)
