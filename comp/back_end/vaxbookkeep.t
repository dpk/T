(herald vaxbookkeep)
  
(define-constant *pointer-registers* 6)
(define-constant *scratch-registers* 4)
(define-constant *argument-registers* 4)
(define-constant *real-registers* 10)
(define-constant *pointer-temps* 64)
(define-constant *scratch-temps* 5)
(define-constant *no-of-registers* 
                 (+ *pointer-temps* *scratch-temps* *real-registers*))

(define-constant *maximum-number-of-arguments* *pointer-temps*)             
        
(define-constant S0 0)
(define-constant S1 1)
(define-constant S2 2)
(define-constant S3 3)
(define-constant NARGS 3)
(define-constant P 4)
(define-constant A1 5)
(define-constant A2 6)
(define-constant A3 7)
(define-constant A4 8)
(define-constant AN 9)
(define-constant AN-1 8)
(define-constant TP -1)
(define-constant nil-reg -2)
(define-constant SP -3)
(define-constant TASK -4)


(define *pos-list1* (make-vector 5))
(define *pos-list2* (make-vector 6))
  

(let ((base  '((5 . rep/pointer)
               (6 . rep/pointer)
               (7 . rep/pointer)
               (8 . rep/pointer))))
  (set (vref *pos-list1* 0) (sublist base 0 0))
  (set (vref *pos-list1* 1) (sublist base 0 1))
  (set (vref *pos-list1* 2) (sublist base 0 2))
  (set (vref *pos-list1* 3) (sublist base 0 3))
  (set (vref *pos-list1* 4) (sublist base 0 4)))


(let ((base  '((4 . rep/pointer)
               (5 . rep/pointer)
               (6 . rep/pointer)
               (7 . rep/pointer)
               (8 . rep/pointer))))
  (set (vref *pos-list2* 0) (sublist base 0 0))
  (set (vref *pos-list2* 1) (sublist base 0 1))
  (set (vref *pos-list2* 2) (sublist base 0 2))
  (set (vref *pos-list2* 3) (sublist base 0 3))
  (set (vref *pos-list2* 4) (sublist base 0 4))
  (set (vref *pos-list2* 5) (sublist base 0 5)))






(define (reg-positions i proc?)       
  (cond ((fx<= i (if proc? 5 4))
         (vref (if proc? *pos-list2* *pos-list1*) i))
        (else
         (append (if proc? (vref *pos-list2* 5) (vref *pos-list1* 4))
                 (make-num-list (fx- i (if proc? 5 4)))))))

(define (make-num-list amount)
  (let ((end (fx+ (fx+ *real-registers* P) amount)))
    (do ((i (fx+ *real-registers* P) (fx+ i 1))
         (l '() (cons (cons i 'rep/pointer) l)))
        ((fx>= i end) (reverse! l)))))

(define (do-trivial-lambda call-node node reg-rep)
  (let ((offset (environment-cic-offset (lambda-env node))))
    (cond ((eq? (lambda-strategy node) strategy/hack)
           (generate-move-address (reg-offset SP (fx+ offset 2)) 
                                  (car reg-rep)))
          ((fx= offset 0)
           (generate-move AN (car reg-rep)))
          (else
           (generate-move-address (reg-offset AN offset) (car reg-rep))))
    (cond ((reg-node (car reg-rep))
                => kill))
    (lock (car reg-rep))))

                                                                            
;;; MAKE-HEAP-CLOSURE The first member of the closure corresponds to the
;;; template so we call %make-extend with this template and the size of the
;;; closure to be created.  Then we fill in the slots with the need variables
;;; and the addresses of templates for any closure-internal-closures.

(define (make-heap-closure node closure)
  (let* ((members (closure-members closure))
         (template-binder (variable-binder (car members))))
    (walk (lambda (var)
            (lambda-queue (variable-binder var)))
          members)
    (free-register node AN)
    (let ((cl (environment-closure (lambda-env template-binder))))
      (cond ((closure-cit-offset cl)
             (let ((acc (lookup node cl nil)))
               (free-register node AN)
               (generate-move acc AN)))
            (else
             (generate-move-address (template template-binder) AN))))
    (lock AN)
    (generate-extend node (closure-size closure))
    (walk (lambda (pair)
      (let ((var (car pair))
            (offset (cdr pair)))
        (cond ((eq? var *dummy-var*))
              ((memq? var members)
               (generate-move-address (template (variable-binder var))
                                      (reg-offset AN
                                                  (fx- offset tag/extend))))
              (else
               (really-rep-convert node
                                   (access-value node var)
                                   (variable-rep var)
                                   (reg-offset AN
                                               (fx- offset tag/extend))
                                   (variable-rep var))))))
      (cdr (closure-env closure)))
    (unlock AN)))


(define (generate-extend node n)
  (free-register node S1)
  (free-register node S2)
  (generate-move (machine-num (fx- n CELL)) S1)   ;; don't include template
  (generate-slink-jump slink/make-extend))

(define (exchange-hack movers)
  (ignore movers) '#f)
