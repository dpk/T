(herald cond (env tsys))

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

;;;; conditional macros

(define-safe-syntax (cond . clauses)
                    (+ (+ #f))
  (labels (((expand-cond clauses)
            (cond ((atom? clauses) 'no-more-cond-clauses)
                  ((atom? (car clauses))
                   (syntax-error "atomic ~s clause: ~s" 'cond (car clauses)))
                  ((atom? (cdar clauses))
                   `(,(t-syntax 'or) ,(caar clauses)
                                     ,(expand-cond (cdr clauses))))
                  ((eq? (cadar clauses) '=>)
                   `(cond-=>-aux ,(caar clauses)
                                 (,(t-syntax 'lambda) ()
                                   ,(caddr (car clauses)))
                                 (,(t-syntax 'lambda) ()
                                   ,(expand-cond (cdr clauses)))))
                  (else `(,(t-syntax 'if) ,(caar clauses)
                             ,(blockify (cdar clauses))
                             ,(expand-cond (cdr clauses)))))))
    (expand-cond clauses)))

(define-safe-syntax (xcond . clauses)
                    (+ (+ #f))   ;++ doesn't check for ELSE
  `(,(t-syntax 'cond) ,@clauses (else (no-op (losing-xcond)))))

(define-syntax (or . args)
  (labels (((expand-or args)
            (cond ((atom? args) ''#f)
                  ((atom? (cdr args)) (car args))
                  (else `(or-aux ,(car args)
                                 (,(t-syntax 'lambda) ()
                                       ,(expand-or (cdr args))))))))
    (expand-or args)))

(define-syntax (and . args)
  (labels (((expand-and args)
            (cond ((atom? args) ''#t)
                  ((atom? (cdr args)) (car args))
                  (else `(,(t-syntax 'if) ,(car args)
                                          ,(expand-and (cdr args))
                                          '#f)))))
    (expand-and args)))


(define-safe-syntax (case key . clauses)
                    (#f . (+ ((| (+ atom?) (! else)) . (* #f))))
  (labels (((expand-case-1 keyvar clauses)
            (if (atom? clauses)
                'case-fell-off-end
                (let ((clause (car clauses))
                      (lose (lambda ()
                              (syntax-error "bad ~s clause syntax: ~s"
                                            'case
                                            (car clauses)))))
                  (cond ((atom? clause) (lose))
                        ((list? (car clause))
                         `(,(t-syntax 'if)
                            (,(t-syntax 'or)
                              ,@(map (lambda (k) `(eq? ,keyvar ',k)) ; equiv?
                                     (car clause)))
                            ,(blockify (cdr clause))
                            ,(expand-case-1 keyvar (cdr clauses))))
                        ((eq? (car clause) 'else) (blockify (cdr clause)))
                        (else (lose)))))))
          (let ((keyvar (generate-symbol 'case)))
            `((,(t-syntax 'lambda) (,keyvar)
                ,(expand-case-1 keyvar clauses))
              ,key))))

(define-safe-syntax (xcase key . clauses)
                    (#f . (+ ((+ atom?) . (* #f))))
  (cond ((assq 'else clauses)
         (syntax-error "~s expression has ~s clause~%  ~s"
                       'xcase 'else `(xcase ,key ,@clauses))))
  `(,(t-syntax 'case) ,key ,@clauses (else (no-op (losing-xcase)))))

(define-safe-syntax (select key . clauses)
                    (#f . (+ ((| (+ #f) (! else)) . (* #f))))
  (labels (((expand-select-1 keyvar clauses)
            (if (atom? clauses)
                'select-fell-off-end
                (let ((clause (car clauses))
                      (lose (lambda ()
                              (syntax-error "bad ~s clause syntax: ~s"
                                            'select
                                            (car clauses)))))
                  (cond ((atom? clause) (lose))
                        ((list? (car clause))
                         `(,(t-syntax 'if)
                            (,(t-syntax 'or)
                              ,@(map (lambda (k) `(eq? ,keyvar ,k))  ; equiv?
                                     (car clause)))
                            ,(blockify (cdr clause))
                            ,(expand-select-1 keyvar (cdr clauses))
                            ))
                        ((eq? (car clause) 'else) (blockify (cdr clause)))
                        (else (lose)))))))
          (let ((keyvar (generate-symbol 'select)))
            `((,(t-syntax 'lambda) (,keyvar)
                ,(expand-select-1 keyvar clauses))
              ,key))))

(define-safe-syntax (xselect key . clauses)
                    (#f . (+ ((+ #f) . (* #f))))
  (cond ((assq 'else clauses)
         (syntax-error "~s expression has ~s clause~%  ~s"
                       'xselect 'else `(xselect ,key ,@clauses))))
  `(,(t-syntax 'select) ,key ,@clauses (else (no-op (losing-xselect)))))
