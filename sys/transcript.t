(herald transcript (env tsys))

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

;;; Transcript facility.

;++ This should be moved to the libraries section of the manual.

(lset +transcript+ nil)
(lset +standard-output+ nil)
(lset +input-underflow+ nil)

(define (transcript-on filename)
  (cond (+transcript+
         (format (standard-output)
                 ";Transcript file ~a is already open.~%"
                 (port-name +transcript+))
         nil)
        (else
         (set +transcript+ (open filename 'out))
         (format +transcript+ ";;; T transcript file ~a~%~%" filename)
         (set +input-underflow+ (iob-underflow (standard-input)))
         (set (iob-underflow (standard-input))
              (make-echo-underflow +transcript+))
         (set +standard-output+ (standard-output))
         (let ((b (make-broadcast-port (standard-output) +transcript+)))
           ;++ what a crock
           (set (standard-output) b)
           (set (terminal-output) b)
           (set (repl-output) b))
         'ok)))

(define (transcript-off)
  (cond (+transcript+
         (set (standard-output) +standard-output+)
         (set (terminal-output) +standard-output+)
         (set (repl-output) +standard-output+)
         (set (iob-underflow (standard-input)) +input-underflow+)
         (format +transcript+ "~&~%;;; End of transcript file~%")
         (close +transcript+)
         (set +transcript+ nil)
         (set +input-underflow+ nil)
         (set +standard-output+ nil)
         'ok)
        (else nil)))

(define (make-echo-underflow out-iob)
  (lambda (in-iob block?)
    (let ((cnt (%vm-read-buffer in-iob block?))
          (buf (iob-buffer in-iob)))
      (do ((i 0 (fx+ i 1)))
          ((fx>= i cnt))
        (vm-write-char out-iob (text-elt buf i))))))
