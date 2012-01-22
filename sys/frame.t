(herald frame (env tsys))

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

;;;; stuff for hacking stack frames

;;; Frame-previous returns frame in stack to which the given stack
;;; frame will return.  If there is no such "superior", return null.
       
(define (closure? obj)
  (and (extend? obj)
       (not (template-header? (extend-header obj)))
       (extend? (extend-header obj))))

(define (frame? obj)
  (and (closure? obj) (fx< (template-nargs (extend-header obj)) 0)))
                           
(define handle-stack-base
  (object nil
    ((frame-previous frame) (ignore frame) nil)
    ((get-environment frame)   (ignore frame) nil)
    ((frame-print-synopsis frame port) (ignore frame port) nil)
    ((print-type-string self) "Stack-base")))

(define handle-magic-frame                       
  (object nil
    ((get-environment frame)   (ignore frame) nil)
    ((frame-print-synopsis frame port) (ignore frame port) nil)
    ((print-type-string self) "Dynamic-state-transition")))

(define-handler vframe
  (object nil
    ((frame-previous vframe) 
     (make-pointer vframe (fx+ (vframe-pointer-slots vframe)
                               (vframe-scratch-slots vframe))))
    ((get-environment frame)   (ignore frame) nil)
    ((frame-print-synopsis frame port) (ignore frame port) nil)
    ((crawl-exhibit self)
     (exhibit-standard-extend self 
                              (vframe-pointer-slots self)
                              (vframe-scratch-slots self)
                              0))
    ((maybe-crawl-component self command)
     (cond ((and (nonnegative-fixnum? command)
                 (fx< command (vframe-pointer-slots self)))
            (crawl-push (extend-elt self command)))
           (else nil)))
    ((print-type-string self) "Stack environment")))

(define (frame-any pred frame)
  (cond ((frame? frame)
         (let ((prev (previous-continuation frame)))
           (iterate loop1 ((frame frame))
             (cond ((eq? frame prev) nil)
                   (else  
                    (let ((limit (frame-size frame)))
                      (iterate loop2 ((i 0))
                        (cond ((fx>= i limit) 
                               (loop1 (frame-previous frame)))
                              ((pred (extend-pointer-elt frame i)))
                              (else (loop2 (fx+ i 1)))))))))))
        (else nil)))
         
(define (frame-size thing)
  (cond ((frame? thing)
         (template-pointer-slots (extend-header thing)))
        ((vframe? thing)
         (vframe-pointer-slots thing))
        (else 0)))
                                         
(define-operation (frame-previous frame)
  (let ((frame (enforce frame? frame)))
    (let ((tem (extend-header frame)))
      (make-pointer frame (fx+ (template-pointer-slots tem)
                               (template-scratch-slots tem))))))

(define (supreme-frame? frame)
  (and (frame? frame) 
       (not (template-superior-bit? (extend-header frame)))))

(define (closure-size-info closure)
  (let ((tem (extend-header closure)))
    (return (template-pointer-slots tem) (template-scratch-slots tem))))
    
(define (template-definer tem)
  (let ((offset (template-definer-vcell-offset tem)))
    (if offset
        (vcell-id (extend-pointer-elt (template-unit tem) offset))
        nil)))

(define (template-unit tem)
  (let ((thing (template-enclosing-object tem)))
    (xcond ((not (bytev? thing)) thing)
           ((weak-table-entry code-unit-table thing)))))
    

(define (template-definer-vcell-offset template)
  (let ((template (if (fixnum-equal? (mref-16-u template -2) jump-absolute)
                      (extend-elt template 0)
                      template)))
    (let ((offset (fixnum-ashr (mref-16-u template -12) 3)))
      (if (fx= offset 0) 
          nil
          (fx- offset 1)))))
            

