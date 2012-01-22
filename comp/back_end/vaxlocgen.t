(herald (back_end vaxlocgen)
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

;;; Copyright (c) 1985 David Kranz
                             

(define (generate-set-location node)    ;; cont type-primop value . args
  ((xselect (length (call-args node))
     ((4) generate-set-fixed-accessor)
     ((5) generate-set-vector-elt))
   node))


(define (generate-set-fixed-accessor node)
  (destructure (((#f type value loc) (call-args node)))
    (let* ((prim (leaf-value type))
           (do-it 
            (lambda (access)
              (cond ((and (eq? prim primop/cell-value)
                          (eq? (variable-definition (leaf-value loc)) 'one))
                     (let ((lc (access-value node (leaf-value loc))))
                       (generate-move access lc)
                       (cond ((and (register? lc) (temp-loc (leaf-value loc)))
                              => (lambda (lc)
                                   (set (temp-node lc) nil)
                                   (set (temp-loc (leaf-value loc)) nil))))))
                    (else
                     (let ((reg (->register 'pointer node (leaf-value loc) '*)))
                       (generate-move access
                             (reg-offset reg (primop.location-specs prim)))))))))
      (cond ((lambda-node? value)
             (let ((access (access/make-closure node value)))
               (if access (protect-access access) (lock AN))
               (do-it (if access access AN))
               (if access (release-access access) (unlock AN))))
            (else
             (let ((access (access-with-rep node (leaf-value value) 'rep/pointer)))
               (protect-access access)                         
               (do-it access)
               (release-access access)))))))
                    
(define (generate-set-vector-type-length node)
  (destructure (((#f vec val) (call-args node)))
    (let ((reg (->register 'pointer node (leaf-value vec) '*))
          (val (leaf-value val)))
      (lock reg)
      (let ((scratch (get-register 'scratch node '*)))
        (cond ((variable? val)
               (emit vax/ashl 
                     (machine-num (if (eq? (variable-rep val) 'rep/pointer) 6 8))
                      (access-value node val) scratch))
              (else 
               (emit vax/movl (machine-num (fixnum-ashl val 8)) scratch)))
        (emit vax/movb (reg-offset reg -2) scratch)
        (emit vax/movl scratch (reg-offset reg -2))
        (unlock reg)))))
               
                                                     
                    
(define (generate-set-vector-elt node)
  (destructure (((#f type value loc idex) (call-args node)))
    (let ((idex (leaf-value idex))
          (rep (primop.rep-wants (leaf-value type)))
	  (reg (->register 'pointer node (leaf-value loc) '*)))
      (lock reg)
      (cond ((eq? rep 'rep/pointer)
             (let* ((access (if (lambda-node? value)
                                (access/make-closure node value)
                                (access-value node (leaf-value value))))
                    (value-acc (if access access AN)))
               (if access (protect-access access) (lock AN))
               (let* ((i-acc (access-with-rep node idex 'rep/integer))
                      (i-reg (cond ((register? i-acc) i-acc)
                                   (else
                                    (let ((i (get-register 'scratch node '*)))
                                      (emit vax/movl i-acc i)
                                      i)))))
                 (generate-move value-acc (index (d@r reg tag/extend) i-reg))
		 (unlock reg)
                 (if access (release-access access) (unlock AN)))))
            (else                                                               
             (let* ((i-acc (access-with-rep node idex 'rep/integer))
                    (i-reg (cond ((and (register? i-acc)
                                       (eq? (rep-size rep) size/byte))
                                  i-acc)
                                 (else
                                  (let ((i (get-register 'scratch node '*)))
                                    (xselect (rep-size rep)
                                      ((size/byte)
                                       (emit vax/movl i-acc i))
                                      ((size/word)
                                       (emit vax/ashl (machine-num -1) i-acc i))
                                      ((size/long)
                                       (emit vax/ashl (machine-num -2) i-acc i)))
                                    i))))
                    (value (leaf-value value)))
                 (lock i-reg)
                 (cond ((variable? value)                       
                        (let ((acc (access-value node value)))
                          (protect-access acc)
                          (really-rep-convert node acc (variable-rep value)
                                   (index (d@r reg tag/extend) i-reg)
                                   rep)
                          (release-access acc)))
                       (else
                        (really-rep-convert node (value-with-rep value rep)
                                            rep
                                            (index (d@r reg tag/extend) i-reg)
                                            rep)))
                 (unlock i-reg)
                 (unlock reg)))))))


                   
                        
(define (generate-contents-location node)
  ((xselect (length (call-args node))
     ((3) generate-fixed-accessor)
     ((4) generate-vector-elt))
   node))

(define (generate-fixed-accessor node)
  (destructure (((cont type loc) (call-args node)))
   (if (or (leaf-node? cont) (used? (car (lambda-variables cont))))   
       (receive (t-spec t-rep) (continuation-wants cont)
         (let* ((type (leaf-value type))
                (base (leaf-value loc))
                (target (get-target-register node t-spec)))
           (cond ((and (eq? type primop/cell-value)
                       (eq? (variable-definition base) 'one))
                  (really-rep-convert node (access-value node base)
                                      'rep/pointer target t-rep))
                 (else
                  (let ((reg (->register 'pointer node base '*)))
                    (really-rep-convert node 
                               (reg-offset reg (primop.location-specs type))
                               'rep/pointer target t-rep))))
           (cond ((reg-node target) 
                  => (lambda (node) (set (register-loc node) nil))))
           (mark-continuation node target))))))


(define (generate-vector-type-length node)
  (destructure (((cont vec) (call-args node)))
    (receive (t-spec t-rep) (continuation-wants cont)
      (let* ((base (leaf-value vec))
             (target (get-target-register node t-spec))
             (reg (->register 'pointer node base '*))
             (temp (if (eq? (reg-type target) 'scratch) 
                       target 
                       (get-register 'scratch node '*))))
        (emit vax/ashl (machine-num -8) (reg-offset reg -2) temp)
        (if (eq? t-rep 'rep/pointer)
            (emit vax/ashl (machine-num 2) temp temp))
        (generate-move temp target)
        (cond ((reg-node target) 
               => (lambda (node) (set (register-loc node) nil))))
        (mark-continuation node target)))))


                                               
(define (generate-vector-elt node)
  (destructure (((cont type loc idex) (call-args node)))
    (receive (t-spec t-rep) (continuation-wants cont)
      (let* ((base (leaf-value loc))
             (type (leaf-value type))
             (idex (leaf-value idex))
             (t-reg (get-target-register node t-spec))
             (rep (primop.rep-wants type))
             (reg (->register 'pointer node base '*)))
        (lock reg) 
        (cond ((fixnum? idex)
               (really-rep-convert node 
                        (d@r reg (fx+ (if (eq? rep 'rep/pointer) 
                                          (fx* idex 4)
                                          idex)
                                      tag/extend))
                        rep t-reg t-rep))
              (else 
               (let* ((i-acc (access-with-rep node idex 'rep/integer))
                      (i-reg (cond ((and (register? i-acc)
                                         (or (eq? (rep-size rep) size/byte)
                                             (eq? rep 'rep/pointer)))
                                    i-acc)
                                   ((eq? rep 'rep/pointer)           
                                    (let ((i (get-register 'scratch node '*)))
                                       (emit vax/movl i-acc i)
                                       i))
                                   (else
                                    (let ((i (get-register 'scratch node '*)))
                                      (select (rep-size rep)
                                        ((size/byte)
                                         (emit vax/movl i-acc i))
                                        ((size/word)
                                         (emit vax/ashl (machine-num -1) i-acc i))
                                        ((size/long)
                                         (emit vax/ashl (machine-num -2) i-acc i)))
                                    i)))))
                 (really-rep-convert node (index (d@r reg tag/extend) i-reg)
                                     rep t-reg t-rep))))
          (unlock reg)
          (cond ((reg-node t-reg) 
                 => (lambda (node) (set (register-loc node) nil))))
          (mark-continuation node t-reg)))))


(define (generate-make-pointer node)
  (destructure (((cont loc idex) (call-args node)))
    (receive (t-spec t-rep) (continuation-wants cont)
      (let ((t-reg (get-target-register node t-spec))
            (reg (->register 'pointer node (leaf-value loc) '*)))
        (lock reg)
        (let* ((i-acc (access-with-rep node (leaf-value idex) 'rep/integer))
               (i-reg (cond ((register? i-acc) i-acc)
                            (else
                             (let ((i (get-register 'scratch node '*)))
                               (emit vax/movl i-acc i)
                               i)))))
          (emit vax/moval (index (d@r reg 4) i-reg) t-reg))
        (unlock reg)
        (cond ((reg-node t-reg) 
               => (lambda (node) (set (register-loc node) nil))))
        (mark-continuation node t-reg)))))
 

(define (generate-location-access node)
  ((xselect (length (call-args node))
     ((3) defer-fixed-accessor)
     ((4) defer-vector-elt))
   node))

(define (defer-fixed-accessor node)
  (destructure (((cont type loc) (call-args node)))
    (let* ((type (leaf-value type))
           (base (leaf-value loc))
           (reg (->register 'pointer node base '*)))
      (lock reg)
      (set (register-loc (car (lambda-variables cont)))
           (cons reg (primop.location-specs type)))
      (allocate-call (lambda-body cont)))))



(define (defer-vector-elt node)
  (destructure (((cont type loc index) (call-args node)))
    (let* ((base (leaf-value loc))
           (type (leaf-value type))
           (index (leaf-value index))   
           (rep (primop.rep-wants type))
           (reg (->register 'pointer node base '*)))
      (lock reg)                                                            
      (cond ((fixnum? index)
             (set (register-loc (car (lambda-variables cont)))
                  (cons reg (fx+ (if (eq? rep 'rep/pointer)   
                                     (fx* index 4)
                                     index)
                                 tag/extend))))
            (else
             (let* ((i-acc (access-with-rep node index 'rep/integer))
                    (i-reg (cond ((and (register? i-acc)
                                       (or (eq? (rep-size rep) size/byte)
                                           (eq? rep 'rep/pointer)))
                                  i-acc)
                                 (else
                                  (let ((i (get-register 'scratch node '*)))
                                    (if (eq? rep 'rep/pointer)
                                        (emit vax/movl i-acc i)
                                        (select (rep-size rep)         
                                          ((size/byte)
                                           (emit vax/movl i-acc i))
                                          ((size/word)
                                           (emit vax/ashl (machine-num -1) i-acc i))
                                          ((size/long)
                                           (emit vax/ashl (machine-num -2) i-acc i))))
                                      i)))))
               (unlock reg)
               (kill-if-dying index node)
               (lock reg)
               (lock i-reg)
               (set (register-loc (car (lambda-variables cont)))
                    (cons (cons reg i-reg) 2)))))
      (allocate-call (lambda-body cont)))))
          
          
                    
                    
(define (generate-%chdr node)
  (destructure (((#f vec val) (call-args node)))
    (let ((reg (->register 'pointer node (leaf-value vec) '*))
          (val (leaf-value val)))
      (lock reg)                                              
      (cond ((fixnum? val)
             (if (fx= val 1)
                 (emit vax/incl (reg-offset reg offset/string-base))
                 (emit vax/addl2 (machine-num val) 
                       (reg-offset reg offset/string-base)))
             (emit vax/subl2 (machine-num (fixnum-ashl val 8))
                   (reg-offset reg -2)))
            (else
             (let* ((n (access-with-rep node val 'rep/integer)))
               (emit vax/addl2 n (reg-offset reg offset/string-base))
               (let ((s (get-register 'scratch node '*)))
                 (emit vax/ashl (machine-num 8) n s)
                 (emit vax/subl2 s (reg-offset reg -2))))))
      (unlock reg))))
               

