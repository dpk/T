(herald retrieve
  (env tsys (osys dump_codes)))

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

;;; Retrieving dumped objects.
;;;   See dump_codes.t, dump.t as well.

;;; This is a vector containing a procedure to decode each possible type byte.
;;; Initially it is filled with an error routine.

(block
  (define *decode-dispatch-vec* (make-vector 256))

  (vector-fill *decode-dispatch-vec*
               (lambda (code in)
                 (ignore in)
                 (error "retrieve got an unknown type code ~S" code)))
  t)

;;; Puts the proper procedure into the dispatch vector.  There are four
;;; decoding procedures.

(define (add-dispatch code shared? data?)
  (receive (proc count)
           (cond ((and shared? data?)
                  (return decode-shared&data 8))
                 (data?
                  (return decode-data 4))
                 (shared?
                  (return decode-shared 2))
                 (else
                  (return decode-plain 1)))
    (do ((i code (fx+ 1 i)))
        ((fx>= i (fx+ code count)))
      (set (vref *decode-dispatch-vec* i) proc))))

;;; These are the four type-byte decoders.  They just extract the
;;; proper fields and return them along with the normalized type
;;; code.

(define (decode-plain code in)
  (ignore in)
  (return code nil nil))

(define (decode-shared code in)
  (ignore in)
  (return (fixnum-logand code #xFE) (fixnum-odd? code) nil))

(define (decode-data code in)
  (let ((data (fixnum-logand code #x3)))
    (return (fixnum-logand code #xFC) nil (get-bytes in (fx+ 1 data)))))

(define (decode-shared&data code in)
  (let ((data (fixnum-ashr (fixnum-logand code #x6) 1)))
    (return (fixnum-logand code #xF8)
            (fixnum-odd? code)
            (get-bytes in (fx+ 1 data)))))

;;; All of the handled types.
;;;             TYPE             SHARED?    SIZE FIELD?
(add-dispatch dump/null            nil         nil)
(add-dispatch dump/char            nil         nil)
(add-dispatch dump/true            nil         nil)
(add-dispatch dump/pair            t           nil)
(add-dispatch dump/coded           t           nil)
(add-dispatch dump/object-ref      nil         t)
(add-dispatch dump/positive-fixnum nil         t)
(add-dispatch dump/negative-fixnum nil         t)
(add-dispatch dump/string          t           t)
(add-dispatch dump/symbol          t           t)
(add-dispatch dump/vector          t           t)
(add-dispatch dump/byte-vector     t           t)
(add-dispatch dump/positive-bignum t           t)
(add-dispatch dump/negative-bignum t           t)
(add-dispatch dump/double-flonum   t           t)

;;; Opens a dumped file, reads in the counts of shared objects,
;;; makes vectors to hold the shared objects, and returns an object
;;; for the port.

(define-predicate retrieve-port?)
(define-operation (retrieve-port-magic-number self) 0)
(define-operation (set-decoder self decoder))

(define retrieve-magic-number -1)

(define (default-retrieve-decoder x) ;++ Code Gen bug
  (ignore x)
  (return nil nil))

(define (maybe-open-retrieve-file filename)
  (let ((in (maybe-open filename 'in)))
    (if (not in)
        '#f
        (let ((magic (get-bytes in 4)))
          (cond ((fx= magic retrieve-magic-number)
                 (let ((port (make-retrieve in)))
                   (set-decoder port default-retrieve-decoder) ;++ fix this
                   port))
;++ flush
;                (else
;                 (format t "~&** Warning: obsolete dump file ~A~%"
;                         (filename->string filename))
;                 (close in)
;                 (let ((in (maybe-open filename 'in)))
;                   (make-old-retrieve in))))))))
                (else
                 (error "bad magic dump number in ~S" filename)))))))

(define (make-retrieve in)
  (let* ((duplicate-count -1)
         (status (locative duplicate-count)))
    (receive (done? size)
             (check-port-status in (get-bytes in 4))
      (let ((objects (make-vector size))
            (decoder nil))  ;++ fix this richard
        (object nil
          ((read self)
           (if done?
               (end-of-file self)
               (receive (end? obj)
                        (check-port-status
                         in
                         (retrieve-object in status objects decoder))
                 (if end? (set done? t))
                 obj)))
          ((close self)
           (close in))
          ((set-decoder self d)
           (set decoder d))
          ((retrieve-port-magic-number self) retrieve-magic-number)
          ((retrieve-port? self) '#t)
          ((print-type-string self) "Retrieve-port"))))))

(define (check-port-status in obj)
  (let ((byte (readc in)))
    (cond ((eof? byte)
           (return t (error "corrupt dump file - unexpected EOF")))
          ((fx= dump/begin-object (char->ascii byte))
           (return nil obj))
          ((fxn= dump/end-of-file (char->ascii byte))
           (return t (error "corrupt dump file - missing BEGIN-OBJECT")))
          ((not (eof? (readc in)))
           (return t (error "corrupt dump file - END-OF-FILE inside file")))
          (else
           (return t obj)))))

;;; Read in and decode the next type byte.  Checks for end-of-file.

(define (get-next-code in)
  (let ((char (readc in)))
    (if (eof? char)
        (error "corrupt dump file - unexpected EOF")
        ((vref *decode-dispatch-vec* (char->ascii char))
           (char->ascii char)
           in))))

;;; Retrieves the next object.  This routine does EOF checking and
;;; adds shared objects to the vectors.  Pairs and vectors must
;;; be checked for sharing before their fields are retrieved so
;;; that circular ones will be reconstructed properly.

(define (retrieve-object in status objects decoder)
  (iterate next ()
    (receive (code shared? data)
             (get-next-code in)
      (let ((index (if shared? 
                       (modify (contents status)
                               (lambda (x) (fx+ x 1)))
                       nil)))
        (receive (thing accessors)
                 (select code
                   ((dump/object-ref) (return (vref objects data) nil))
                   ((dump/coded) (let* ((key (next))
                                        (data (next))
                                        (count (next)))
                                   (receive (maker accessors)
                                            (decoder key)
                                     (cond ((not maker)
                                            (error '"no maker for key ~S" key))
                                           ((fxn= count (length accessors))
                                            (error
                               '"wrong number of accessors for key ~S" key))
                                           (else
                                            (return (apply maker data)
                                                    accessors))))))
                   (else
                    (return (get-next-object in code data) nil)))
          (if shared? (set (vref objects index) thing))
          (select code
            ((dump/pair)
             (set (car thing) (next))
             (set (cdr thing) (next)))
            ((dump/vector)
             (do ((i 0 (fx+ 1 i)))
                 ((fx>= i data))
               (set (vref thing i) (next))))
            ((dump/coded)
             (walk (lambda (acc)
                     (set (acc thing) (next)))
                   accessors)))
          thing)))))

;;; Actually retrieves the next object.  Dispatches on the type code.

(define (get-next-object in code data)
  (select code
    ((dump/null)            nil)
    ((dump/true)            '#t)
    ((dump/char)            (readc in))
    ((dump/positive-fixnum) data)
    ((dump/negative-fixnum) (fx- 0 data))
    ((dump/pair)            (cons nil nil))
    ((dump/vector)          (make-vector data))
    ((dump/string)          (get-string in data))
    ((dump/symbol)          (string->symbol (get-string in data)))
    ((dump/double-flonum)   (get-double-float in data))
    ((dump/byte-vector)     (get-bytev in data))
    ((dump/positive-bignum) (get-bignum in data))
    ((dump/negative-bignum) (- (get-bignum in data)))
    (else
     (error "retrieve got an unknown type code ~S" code))))

;;; Routines to reconstruct the various types.

(define (get-string in size)
  (let ((string (make-string size)))
    (read-block in (string-text string) size)
    string))

(define (get-double-float in size)
  (let* ((sign (if (eq? (get-byte in) 1) 1 -1))
         (e (get-bytes in 4))
         (m (if (fx= size 0)
                (get-bytes in 4)
                (get-bignum in size))))
    (integer-encode-float sign m e)))

(define (get-bytev in size)
  (let ((bytev (make-bytev size)))
    (read-block in bytev size)
    bytev))

(define (get-bignum in size)
  (let ((num (create-bignum size)))
    (set-bignum-sign! num 1)
    (do ((i 0 (fx+ 1 i)))
        ((fx>= i size))
      (set (bignum-digit num i) (get-bytes in 4)))
    num))

;;; Read in various numbers of bytes.

(define (get-byte in)
  (char->ascii (readc in)))

(define (get-two-bytes in)
  (let ((value (get-byte in)))
    (fixnum-logior (fixnum-ashl (get-byte in) 8)
                   value)))

(define (get-bytes in count)
  (let ((end (fixnum-ashl count 3)))
    (do ((i 0 (fx+ 8 i))
         (val 0 (fixnum-logior (fixnum-ashl (get-byte in) i)
                               val)))
        ((fx>= i end)
         val))))
