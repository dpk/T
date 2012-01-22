(herald unix (env tsys))

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

;;;; UNIX interface

;;; Operating System Descriptor

(define local-os
  (lambda ()
    (object nil
      ((os-type self)               'unix)
      ((unix-os? self)             '#t)
      ((print-type-string self)     "Operating-system"))))

;;; Names of standard places to find things

;;; The T-SYSTEM-DIRECTORY is the place where the various files 
;;; (possibily) needed to startup the system are found, e.g.
;;; the image file, the fix file(s), the configuration file ...

(define (the-t-system-directory)
  'tsystem)

;;; The INIT-FILE-DIRECTORY is the directory in which the user's
;;; initialization file should be located.

(define (the-init-file-directory)
  'home)

;;; LUSER-TYPED-EOF-AT-TOP-LEVEL is a system dependent exception
;;; which occurs when the REPL gets an EOF.
;++ Bug: people might move ^Z to some other key.

(define (luser-typed-eof-at-top-level . args)
  (ignore args)
  (format (error-output)
          "** Use ^Z or (STOP) to suspend, or (EXIT) to exit.~%"))

;;; Boot-args offsets.  Boot-args is a system-global which is
;;; passed to big_bang on system startup.


 (define-constant boot/argc      4)
 (define-constant boot/argv      5)

;;; Command line        


(define (command-line)
  (let* ((boot-args (system-global slink/boot-args))
         (argc (mref-integer boot-args (fx* boot/argc 4)))
         (argv (gc-pair->extend (gc-pair->extend (vref boot-args boot/argv)))))
    (do ((i (fx- argc 1) (fx- i 1))
         (l '() (cons (asciz->string (extend-elt argv i)) l)))
        ((fx< i 0) l))))

(define (asciz->string asciz)
  (receive (asciz offset)
    (xcase (descriptor-tag asciz)
      ((0) (return (gc-pair->extend (gc-pair->extend asciz)) 0))  ; subtract 2
      ((1) (return (gc-extend->pair asciz) 3))                    ; add 1
      ((2) (return asciz 2))                                      ; OK
      ((3) (return (gc-pair->extend asciz) 1)))                   ; subtract 1
    (do ((i 0 (fx+ i 1)))
        ((fx= (mref-8-u asciz (fx- i offset)) 0)
         (let ((str (make-string i)))
           (do ((i (fx- i 1) (fx- i 1)))
               ((fx< i 0) str)
             (set (mref-8-u (string-text str) i)
                  (mref-8-u asciz (fx- i offset)))))))))



(define (string->asciz! string)
  (let ((len (string-length string)))
    (cond ((fx< len (text-length (string-text string)))
            ;; this could really be much more liberal
            ;; if it wanted to be hairy
            (set (string-elt string len) #\null)
            string)
          (else
           (let ((new-string (make-string (fx+ 1 len))))
             (string-replace new-string string len)
             (set (string-elt new-string len) #\null)
             new-string)))))


