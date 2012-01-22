(herald (assembler as_syntax t 0))

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

;;; Declarations for AS clients - i.e. machine description
;;; writers.  AS also presents an interface to the fore
;;; (as opposed to aft) end of the compiler.  That interface
;;; is partially described by the file AS.T; and partially
;;; described by the particular machine description being used.
                                                                              
;;; FG_SPEC
(define-syntax (define-fg bvl . specs)
  `(define ,(car bvl) (named-fg ,bvl ,@specs)))

(define-syntax (named-fg bvl . specs)
  (process-define-fg (car bvl) (cdr bvl) specs))

(define-syntax (define-data-fg bvl . specs)
  `(define ,(car bvl) (named-data-fg ,bvl ,@specs)))

(define-syntax (named-data-fg bvl . specs)
  (process-define-data-fg (car bvl) (cdr bvl) specs))

;;; Pseudo op definition

;;; Given specs list, return an alist of (symbol . proc)
;;; where proc will do the body of the spec corresponding
;;; to the symbol.

(define-syntax (pseudos-alist . specs)
    ``(,@(list ,@(map (lambda (spec)
                   ``(,',(caar spec) . ,,(make-lap-syntaxer spec)))
               specs))))


;;; Form is ((name . vars) . body)
;;; body should return something that can be passed to the enclosing 
;;;   fg constructor.

(define (make-lap-syntaxer form)
  (let ((parameter-name (generate-symbol 'lap-syntax)))
    `(lambda (,parameter-name)
        (destructure ((,(car form) ,parameter-name))
            ,@(cdr form)))))

;;; Utility

;;; define-fg expands into code with uses of VECTOR
(define-syntax (vector . l)
  (let ((vname (generate-symbol 'vector)))
    `(let ((,vname (make-vector ,(length l))))
       ,@(do ((i 0 (fx+ i 1))
              (elts l (cdr elts))
              (sets '() (cons `(set (vref ,vname ,i) ,(car elts))
                              sets)))
             ((null? elts)
              (reverse! sets)))
       ,vname)))



