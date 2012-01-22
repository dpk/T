(herald let (env tsys))

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

;;;; Binding macros

;;; Macros for doing bindings of all sorts.

(define (valid-spec? spec)
  (and (pair? spec)
       (list? (cdr spec))
       (null? (cddr spec))))  ;(cdr '()) => ()

;++ add the name to LET

(define-safe-syntax (let specs . body)
                    ((* valid-spec?) . (* #f))
  (cond ((every? valid-spec? specs)
         `((,(t-syntax 'lambda) ,(map car specs) ,@body)
           ,@(map (lambda (x)
                    (cond ((atom? (cdr x)) 
                           'let-missing-initializer)
                          (else (cadr x))))
                  specs)))
        (else
         (syntax-error "illegal spec~%  ~S" `(let ,specs . ,body)))))

(define-safe-syntax (fluid-let specs . body)
                    ((* valid-spec?) . (* #f))
  (cond ((every? valid-spec? specs)
         (let ((temps  (map (lambda (binding)
                              (ignore binding)
                              (generate-symbol 'bind))
                            specs))
               (places (map car  specs))
               (vals   (map cadr specs))
               (handler (generate-symbol 'wind))
               (cell    (generate-symbol 'cell)))
           `((,(t-syntax 'lambda)
              ,temps
              ((,(t-syntax 'lambda)
                (,handler)
                (bind-handler ,handler
                              (,(t-syntax 'lambda) () . ,body)
                              ,handler))
               (,(t-syntax 'lambda)
                ()
                ,@(map (lambda (place temp)
                         `(,(t-syntax 'let) ((,cell ,place))
                                            (,(t-syntax 'set) ,place ,temp)
                                            (,(t-syntax 'set) ,temp ,cell)))
                       places temps))))
             ,@vals)))
        (else
         (syntax-error "illegal spec~%  ~S" `(bind ,specs ,@body)))))

(define-safe-syntax (bind specs . body)
                    ((* valid-spec?) . (* #f))
  (let ((temps  (map (lambda (binding)
                       (ignore binding)
                       (generate-symbol 'bind))
                     specs))
        (places (map car  specs))
        (vals   (map cadr specs))
        (handler (generate-symbol 'wind))
        (cell    (generate-symbol 'cell)))
    (cond ((null? temps)
           `((,(t-syntax 'lambda) () ,@body)))
          (else
           `((,(t-syntax 'lambda)
               ,temps
               ((,(t-syntax 'lambda)
                  (,handler)
                  (bind-handler ,handler
                                (,(t-syntax 'lambda) () . ,body)
                                ,handler))
                (,(t-syntax 'lambda)
                  ()
                  ,@(map (lambda (place temp)
                           `(,(t-syntax 'let) ((,cell ,place))
                              (,(t-syntax 'set) ,place ,temp)
                              (,(t-syntax 'set) ,temp ,cell)))
                         places temps))))
             ,@vals)))))

(define-safe-syntax (destructure specs . body)
                    ((* (#f #f)) . (* #f))
  (expand-destructure specs body))

;;; Note that EXPAND-DESTRUCTURE is called from other places.
;;; Difficult to write this without side-effects.  Try it sometime.

(define (expand-destructure specs body)
  (let ((a '()) (b '()))
    (walk (lambda (spec)
            (let ((foo (lambda (vars z val)
                         (cond ((null? vars))
                               ((atom? vars)
                                (push a `(,vars (,z ,val))))
                               (else
                                (let ((temp (generate-symbol z)))
                                  (push a `(,temp (,z ,val)))
                                  (push b `(,vars ,temp))))))))  
              (let ((vars (car spec)) (val (cadr spec)))
                (cond ((atom? vars)
                       ;; No destructuring called for; just do as for LET.
                       (push a spec))
                      ((pair? val)
                       ;; RHS is a call or special form; need to stow value.
                       (let ((temp (generate-symbol 'temp)))
                         (push a `(,temp ,val))
                         (push b `(,vars ,temp))))
                      (else
                       ;; RHS is a variable, LHS is pattern; take apart value.
                       (foo (car vars) 'car val)
                       (foo (cdr vars) 'cdr val))))))
          specs)
    `(,(t-syntax 'let) ,(reverse! a)
       ,(cond ((null? b) (blockify body))
              (else (expand-destructure (reverse! b) body))))))

;(define-syntax (let-destructured . rest)
;  `(,(t-syntax 'destructure) . ,rest))               ; change later

;;; What about BIND-DESTRUCTURED, LET*-DESTRUCTURED, and BIND*-DESTRUCTURED?

(define-safe-syntax (let* specs . rest)
                    ((* valid-spec?) . (* #f))
  (expand-star-macro specs rest (t-syntax 'let)))

(define-safe-syntax (destructure* specs . rest)
                    ((* (#f #f)) . (* #f))
  (expand-star-macro specs rest (t-syntax 'destructure)))

(define-safe-syntax (bind* specs . rest)
                    ((* valid-spec?) . (* #f))
  (expand-star-macro specs rest (t-syntax 'bind)))

(define (expand-star-macro specs rest mac)
  (cond ((null? (cdr specs))
         `(,mac ,specs . ,rest))
        (else `(,mac (,(car specs))
                     ,(expand-star-macro (cdr specs) rest mac)))))


(define-safe-syntax (receive vars form . body)
                    (formals-list? #f . (* #f))
  `(receive-values (,(t-syntax 'lambda) ,vars ,@body) 
                   (,(t-syntax 'lambda) () ,form)))


