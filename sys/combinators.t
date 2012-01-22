(herald combinators 
  (env tsys))

;;; Logical Connectives

;;; These should be rewritten later to offer some sense of efficiency. 
;;; What we have here is extremely elegant if a bit silly. Will work for
;;; the time being... 
;;;
;;; Perhaps set operations could be regular operations, with these 
;;;  definitions as default handlers... however this loses because, like
;;;  arithmetic, they want to be unbiased with respect to particular args...
;;;  sigh. Can win with negation anyway, since it is unary...

;;; Note elegance of things like (DISJOIN (.IN 0 10) (.IN 20 30)).

(DEFINE (DISJOIN . FNS)
  (LAMBDA ARGLIST (ANY?   (LAMBDA (FN) (APPLY FN ARGLIST)) FNS)))

(DEFINE (CONJOIN . FNS)
  (LAMBDA ARGLIST (EVERY? (LAMBDA (FN) (APPLY FN ARGLIST)) FNS)))

(DEFINE (COMPLEMENT FN)
  (LAMBDA ARGLIST (NOT (APPLY FN ARGLIST))))

;;; As an operation, the above would look like:
;;;
;;; (DEFINE-OPERATION (COMPLEMENT FN)
;;;   (LAMBDA ARGLIST (NOT (APPLY FN ARGLIST))))
;;;

;;; (COMPOSE f g ...)
;;;
;;; Composes procedures of 1 argument.  What would it mean to compose procedures
;;;  of more than 1 argument?  Returns a procedure which works like the 
;;;  composition of these.  Note, 
;;;
;;;     ((COMPOSE x1 x2 ... xN) y)  <=>  (x1 (x2 ... (xN y) ...))
;;;
;;;  so, for example,
;;;
;;;             CADR   is like  (COMPOSE CAR CDR)
;;;             CDDAR  is like  (COMPOSE CDR CDR CAR)
;;;
;;; Compiler should know about COMPOSE.  We wouldn't want ((COMPOSE ...) ...)
;;; to cons a closure.
;;;
;;; Last may a procedure of more than one argument.

(DEFINE (COMPOSE . PROCS)
  (COND ((NULL? PROCS) PROJ0)
        ((NULL? (CDR PROCS)) (CAR PROCS))
        (ELSE (LET ((PRELUDE (APPLY COMPOSE (CDR PROCS)))
                    (FINALLY (CAR PROCS)))
                (LAMBDA ARGS
                  (FINALLY (APPLY PRELUDE ARGS)))))))

