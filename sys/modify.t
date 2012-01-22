(herald modify (env tsys))

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

;;;; modify-location and friends

;;; in
;;;     (modify-location form (lambda (fetch store) value))
;;; the continuation is called, being passed an access procedure fetch of no
;;; arguments which fetches the value in the location indicated by
;;; form, and an update procedure store of one argument, which will store a
;;; new value in that place.

(define-syntax (modify-location form cont)
  (cond ((symbol? form)
         (let ((new-value (generate-symbol 'value)))
           `(,cont (,(t-syntax 'lambda) () ,form)
                   (,(t-syntax 'lambda) (,new-value)
					(,(t-syntax 'set) ,form ,new-value)))))
        ;; atom case should err.
        ;; what about special form case?
        (else
         (let ((new-form (map (lambda (subform)
                                 (ignore subform)
                                 (generate-symbol 'subform))
                              form)))
           `(,(t-syntax 'let) ,(map list new-form form)
              (,cont (,(t-syntax 'lambda) () ,new-form)
                     (,(t-syntax 'lambda) (new-value)
			  (,(t-syntax 'set) ,new-form new-value))))))))

(define-syntax (modify form proc)
  (let ((fetch (generate-symbol 'fetch))
        (store (generate-symbol 'store)))
    `(,(t-syntax 'modify-location)
      ,form
      (,(t-syntax 'lambda) (,fetch ,store)
			   (,store (,proc (,fetch)))))))

(define-syntax (swap form new-value)
  (let ((fetch (generate-symbol 'fetch))
        (store (generate-symbol 'store)))
    `(,(t-syntax 'modify-location)
      ,form
      (,(t-syntax 'lambda) (,fetch ,store)
			   (,(t-syntax 'block0) (,fetch)
						(,store ,new-value))))))

(define-syntax (exchange form-1 form-2)
  (let ((fetch-1 (generate-symbol 'fetch))
        (store-1 (generate-symbol 'store))
        (fetch-2 (generate-symbol 'fetch))
        (store-2 (generate-symbol 'store)))
    `(,(t-syntax 'modify-location) ,form-1
       (,(t-syntax 'lambda) (,fetch-1 ,store-1)
         (,(t-syntax 'modify-location) ,form-2
           (,(t-syntax 'lambda) (,fetch-2 ,store-2)
             (,store-1 (,(t-syntax 'block0) (,fetch-2)
                               (,store-2 (,fetch-1))))))))))

(define-syntax (increment form)
  `(,(t-syntax 'modify) ,form 1+))

(define-syntax (decrement form)
  `(,(t-syntax 'modify) ,form -1+))

(define-syntax (push form thing)
  (let ((fetch (generate-symbol 'fetch))
        (store (generate-symbol 'store)))
    `(,(t-syntax 'modify-location) ,form
       (,(t-syntax 'lambda) (,fetch ,store)
         (,store (cons ,thing (,fetch)))))))

(define-syntax (pop form)
  `(,(t-syntax 'modify-location) ,form
     (,(t-syntax 'lambda) (fetch store)
       (,(t-syntax 'let) ((temp (fetch)))
         (store (cdr temp))
         (car temp)))))
