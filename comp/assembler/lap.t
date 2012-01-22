(herald (tas lap t 0))

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

;;; Define a lap environment                

(define (new-lap-env machine parent lap-env-name)
  (let ((lap-env (make-locale parent lap-env-name)))
    ;(*define standard-env lap-env-name lap-env)  ; debugging only
    (*define parent lap-env-name lap-env)
    (set (machine-lap-env machine) lap-env)))

(define (*define-lap machine symbol value)
    (*define (machine-lap-env machine) symbol value))

;;; Pseudo-op definition

;;; (see also AS_SYNTAX)

(define (get-pseudo key alist)
    (cond ((assq key alist) => cdr)
          (else nil)))
    
;;; Lap processor.

;;; For testing lap

(lset *lap-reorders-blocks?* nil)

(define (test-lap items machine)
  (bind ((*current-assembly-labels* (make-labels-table 'lap-labels)))
    (emit-tag (generate-symbol 'lap-entry))
    (process-lap-list items machine)
    (table-entry *current-assembly-labels* '&&all&&)))
                 
;;; Walk the lap items, noting labels, and filling in jumps.

;;; Needed error checking:  emit-tag on existant tag, emits after emit-jump
;;; emit-jump after emit-jump.

(define (process-lap-list items machine)
  (let ((lap-env (machine-lap-env machine))
        (p-ops   (machine-pseudo-ops machine))
        (p-opnds (machine-pseudo-operands machine)))
    (do ((items items (cdr items)))
        ((null? items) '*)
      (let ((i (car items)))
        (bind ((*current-lap-item* i))
          (cond ((pair? i)      ; --instruction
                 (let ((fg (process-lap-item i lap-env p-ops p-opnds)))
                   (if (fg? fg) (emit-to-ib *current-ib* fg))))

                ((string? i)     ; --comment
                 (emit-comment-to-ib *current-ib* i))
  
                ((symbol? i)     ; --tag
                 (let ((prev *current-ib*))
                   (emit-tag i)
                   (cond (*lap-reorders-blocks?*
                          (if (empty? (ib-jump-op prev))
                              (emit-jump-to-ib prev jump-op/jabs *current-ib* nil)))
                         (else
                          (maybe-set-ib-follower prev *current-ib*)))))
                (else
                 (lap-error "cannot process item of this type"))))))))

;;; Process the "operands", then apply the "instruction"
;;; maker to the result.                                       

(define (process-lap-item item lap-env p-ops p-opnds)
  (destructure (((proc-exp . arg-exps) item))
    (cond ((get-pseudo (car item) p-ops)
           => (lambda (p) (p item)))
          (else
           (let ((args (map (lambda (a) (process-lap-operand a lap-env p-opnds)) 
                            arg-exps)))
             (apply (*value lap-env proc-exp) args)
             )))))
                              
(define (process-lap-operand opd lap-env p-operands)
    (let ((opd (process-lap-operand-1 opd lap-env p-operands)))
      (cond ((fixnum? opd) (vref *register-fgs* opd))
            ((and (pair? opd) (eq? (car opd) *as-number-marker*))
             (cdr opd))
            (else opd))))
   
;;; For operands, essentially, just evaluate the form in
;;; the lap-env, except that numbers are registers, and combinations
;;; beginning with the symbol LABEL are handled specially.

(define (process-lap-operand-1 opd lap-env p-operands)
    (cond ((symbol? opd)           ; speed hack to eval symbols
           (*value lap-env opd))
          ((and (pair? opd) (get-pseudo (car opd) p-operands))
           => (lambda (p) (p opd)))
          (else
           (eval opd lap-env))))

;;; To get an operand which is a number, and not translated to a register
;;; To be able to use a (NUMBER x) operand in lap, say
;;;     (*define-lap <machine> 'number make-as-number)

(define-constant *as-number-marker* (cons '*as-number-marker* nil))

(define (make-as-number n) (cons *as-number-marker* n))

;;; Error reporting

(lset *current-lap-item* nil)
                         
(define (lap-error cstring . args)
    (apply error 
           `("in lap item ~s~%   " ,cstring) 
           *current-lap-item* args))
