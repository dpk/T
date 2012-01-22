(herald sys5 (env tsys))


(define-constant file-mode/in     #o0)
(define-constant file-mode/out    #o1401)
(define-constant file-mode/append #o411)


(define-constant number-of-signals 23)   


;;; handler-types (Htype): A = asynchronous, E = exception, D = default,
;;; I = ignore
;;; (sig# handler-type handler description)

(define *signals*
  '(( 1   E    non-continuable  "hangup")
 ;   ( 2   A    sigint-handler    "interrupt")
 ;   ( 3   A    siquit-handler    "quit")
    ( 4   E    non-continuable  "illegal instruction")
    ( 5   E    non-continuable  "trace trap")
    ( 6   E    non-continuable  "software generated")
    ( 7   E    non-continuable  "software generated")
    ( 8   E    non-continuable  "floating point exception")
 ;   ( 9   D    default          "kill")
    (10   E    non-continuable  "bus error")
    (11   E    non-continuable  "segmentation violation")
    (12   E    non-continuable  "bad argument to system call")
    (13   E    non-continuable  "write on a pipe with no one to read it")
 ;   (14   D    default          "alarm clock")
 ;   (15   A    sigterm-handler   "software termination signal")
 ;   (16   D    default          "user defined signal 1")
 ;   (17   D    default          "user defined signal 2")
 ;   (18   D    default          "death of a child")
 ;   (19   D    default          "power fail")
 ;   (20   D    default          "virtual timer alarm")
 ;   (21   D    default          "profiling timer alarm")
 ;   (22   D    default          "reserved for future use")
 ;   (23   D    default          "A window change or mouse signal")
  ))

(define-constant %%SIGINT     2)
(define-constant %%SIGQUIT    3)
(define-constant %%SIGTERM    15)

(define-foreign csh (csh) rep/undefined)

(define-unimplemented (make-foreign-procedure sym))
(define-unimplemented (load-foreign file))

(define (initialize-local-system) nil)