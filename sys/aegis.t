(herald aegis (env tsys))

;;;; T/AEGIS interface

;;; Operating System Descriptor

(define local-os
  (lambda ()
    (object nil
      ((os-type self)               'aegis)
      ((aegis-os? self)             '#t)
      ((print-type-string self)     "Operating-system"))))


;;; Command line hacking

(define-foreign pgm_$get_args
  ("PGM_$GET_ARGS" (out    rep/integer-16-u argument-count)
                 (ignore rep/integer      arg-vector-addr))
                ignore)

(define-foreign pgm_$get_arg
  ("PGM_$GET_ARG" (in     rep/integer-16-u   arg-number)
                (ignore rep/extend         argument)
                (out    rep/integer        status)
                (in     rep/integer-16-u   maxlen))
      rep/integer-16-u)

(define (argc)
  (pgm_$get_args nil nil))

(define (aegis-get-arg n)
  (with-buffers ((buf))
    (receive (length status)
             (pgm_$get_arg n
                           (buffer-text buf)
                           nil
                           (max-buffer-length buf))
      (cond ((fx= 0 status)
             (set (buffer-length buf) length)
             (buffer->string buf))
            (else (local-os-error status))))))

(define (command-line)
  (do ((i (fx- (argc) 1) (fx- i 1))
       (l '() (cons (aegis-get-arg i) l)))
      ((fx< i 0) l)))


;;; Names of standard places to find things

;;; The T-SYSTEM-DIRECTORY is the place where the various files 
;;; (possibily) needed to startup the system are found, e.g.
;;; the image file, the fix file(s), the configuration file ...

(define (the-t-system-directory)
  'tsystem)

;;; The INIT-FILE-DIRECTORY is the directory in which the user's
;;; initialization file should be located.

(define (the-init-file-directory) "~")

;;; LUSER-TYPED-EOF-AT-TOP-LEVEL is a system dependent exception
;;; which occurs when the REPL gets an EOF.

(define (luser-typed-eof-at-top-level . args)
  (ignore args)
  (format (error-output) "** use (exit) to exit.~%"))

;;; Initialize the local OS.

(define (initialize-local-system) nil)
