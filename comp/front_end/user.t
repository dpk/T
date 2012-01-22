(herald (front_end user)
  (env t (orbit_top defs)))

;;; Print out the status of the locale variables encountered in the program.

;;; Blow off the old PRINT-VARIABLE-INFO (is it still necessary to do this?)
(define print-variable-info false)

;;; (VARIABLES
;;;   FREE          variables free in this file
;;;   EARLY-BOUND   declared variables used in this file that have not been
;;;                 entirely integrated
;;;   DEFINED       variables defined
;;;   LSET                    lset
;;;   SET                     set
;;;   ERROR         multiply defined variables
;;;   INTEGRATED    variables whose values were integrated
;;;   LOCAL         variables declared to be local to this file
;;;   UNREFERENCED  defined, lset, and set variables that are not referenced
;;; )

(define (new-print-variable-info defined free borrowed old-env)
  (receive (defs lsets error set local unreferenced early-bound1 integrated1)
           (sort-new-variables defined old-env)
    (receive (early-bound2 integrated2)
             (sort-borrowed-variables borrowed)
      (let ((stream *noise+terminal*))
        (format stream "~&(VARIABLES~%")
        (format stream "  FREE ~S~%" (get-free-variables free))
        (format stream "  EARLY-BOUND ~S~%" (append! early-bound1 early-bound2))
        (format stream "  DEFINED ~S~%" defs)
        (format stream "  LSET ~S~%" lsets)
        (format stream "  SET ~S~%" set)
        (format stream "  ERROR ~S~%" error)
        (format stream "  INTEGRATED ~S~%" (append! integrated1 integrated2))
        (format stream "  LOCAL ~S~%" local)
        (format stream "  UNREFERENCED ~S)~%" unreferenced)
        ))))

(define (sort-new-variables defined old-env)
  (let ((vars '()))
    (table-walk defined
                (lambda (#f var)
                  (push vars var)))
    (iterate loop ((v vars) (d '()) (l '()) (e '()) (s '()) (o '()) (u '())
                            (eb '()) (i '()))
      (cond ((null? v)
             (return d l e s o u eb i))
            (else
             (let* ((var (car v))
                    (n (variable-name var))
                    (def (variable-definition var))
                    (u (if (and (null? (cdr (variable-refs var)))
                                (not (memq? 'integrated (variable-flags var))))
                           (cons n u)
                           u))
                    (o (if (memq? 'local (definition-data def))
                           (cons n o)
                           o)))
               (xcase (definition-variant (variable-definition var))
                 ((set)
                  (if (lset-early-binding? n old-env)
                      (loop (cdr v) d l e (cons n s) o u (cons n eb) i)
                      (loop (cdr v) d l e (cons n s) o u eb i)))
                 ((multiple)
                  (loop (cdr v) d l (cons n e) s o u eb i))
                 ((lset)
                  (if (set-variable? var)
                      (loop (cdr v) d (cons n l) e (cons n s) o u eb i)
                      (loop (cdr v) d (cons n l) e s o u eb i))) 
                 ((define)
                  (loop (cdr v) (cons n d) l e s o u eb i))
                 ((constant)
                  (if (memq? 'integrated (variable-flags var))
                      (loop (cdr v) (cons n d) l e s o u eb (cons n i))
                      (loop (cdr v) (cons n d) l e s o u eb i))))))))))

(define (sort-borrowed-variables borrowed)
  (let ((eb '())
        (i '()))
    (table-walk borrowed
                (lambda (name var)
                  (if (not (null? (variable-refs var)))
                      (push eb name))
                  (if (memq? 'integrated (variable-flags var))
                      (push i name))))
    (return eb i)))

(define (get-free-variables free)
  (let ((f '()))
    (table-walk free
                (lambda (name var)
                  (ignore var)
                  (push f name)))
    f))

(define (set-variable? var)
  (memq? 'set (definition-data (variable-definition var))))

(define (lset-early-binding? name env)
  (and (env name)
       (eq? 'lset (definition-variant (env name)))))

;;; Mark the variable referenced by NODE (if any) as having been used.

(define (mark-reference-used node)
  (if (and (reference-node? node)
           (variable-definition (reference-variable node)))
      (mark-variable-used (reference-variable node))))

;;; Mark the variable as having been used so that it will show up as INTEGRATED
;;; in the locale variable information.

(define (mark-variable-used var)
  (if (not (memq? 'integrated (variable-flags var)))
      (push (variable-flags var) 'integrated)))



