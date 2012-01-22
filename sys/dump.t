(herald dump
  (env tsys (osys dump_codes)))

;;; A T-object dumper.  Li Kai's algorithm implemented by Richard
;;; Kelsey, modified for T3 by David Kranz

;;; Generation number.  This must be negative to avoid conflict with the first
;;; dumper which didn't have a generation number.

(define dump-magic-number -1)

(define-predicate dump-port?)
(define-operation (set-encoder self encoder))

;;;   Returns a port that handles the operations WRITE and CLOSE.
;;; The idea is to make a dump file look like any other to the user.
;;; Dumped objects are written into a vector.  When the dump port is closed
;;; the objects are written into the file.

(define (default-dump-encoder x)  ; back end bug
  (ignore x)
  (return nil nil nil))

(define (maybe-open-dump-file filename)
  (let ((things    (make-infinite-vector 60 false 'dump-vector))
        (status    (make-dump-status))
        (delimits  '())
        (encoder   default-dump-encoder)
        (objects   (make-table 'dumped-objects)))
    (object nil
      ((write self thing)
       (push delimits (dump-status-count status))
       (vectorize thing things status objects encoder)
       '#t)
      ((close self)
       (dump filename things status objects (reverse! (cons -1 delimits)))
       (recycle things)
       (recycle objects)
       '#t)
      ((set-encoder self e)
       (set encoder e))
      ((dump-port? self) '#t)
      ((port? self) '#t)
      ((print-info self) filename)
      ((print-type-string self) "Dump-port"))))

;;; Open a port on the file and write everything into it.  The first four
;;; bytes in the file are the generation number.  Then comes the number of
;;; duplicated objects.  Small objects are dumped in their own way.  Other
;;; objects are looked up in the OBJECTS table.  A table entry of 'ONE means
;;; that there is only one copy of the object and it can just be dumped.
;;; 'MANY means that this is the first of multiple copies.  The object
;;; is dumped with the 'shared' bit set and the table entry is set
;;; to the next duplicated object index.  Any other entry is an index which
;;; is dumped with the OBJECT-REF type.

(define (dump filename things status objects delimits)
  (with-open-ports ((port (open filename 'out)))
    (dump-port port things status objects delimits)))

(define (dump-port port things status objects delimits)
  (let ((count (dump-status-count status))
        (duplicates 0))
    (dump-bytes port dump-magic-number 4)
    (dump-bytes port (dump-status-duplicates status) 4)
    (do ((i 0 (fx+ i 1)))
        ((fx>= i count)
         (dump-byte port dump/end-of-file)
         (if (fxn= duplicates (dump-status-duplicates status))
             (error "dump's duplicate counts didn't match")))
      (let ((thing (things i)))
        (cond ((fx= i (car delimits))
               (dump-byte port dump/begin-object)
               (pop delimits)))
        (if (small-object? thing)
            (dump-small-object thing port)
            (let ((tag (table-entry objects thing)))
              (cond ((eq? tag 'one)
                     (dump-data port thing nil))
                    ((eq? tag 'many)
                     (set (table-entry objects thing) duplicates)
                     (set duplicates (fx+ duplicates 1))
                     (dump-data port thing t))
                    ((fixnum? tag)
                     (dump-unshared-code&size port dump/object-ref tag))
                    (else 
                     (error "funny dumped object tag ~S for ~S"
                            tag thing)))))))))

;;; A structure to hold the two counts associated with a dump port.  This is
;;; just a two place locative.

(define-structure-type dump-status
  count
  duplicates
  )

(let ((m (stype-master dump-status-stype)))
  (set (dump-status-count      m) 0)
  (set (dump-status-duplicates m) 0))

;;; Write THING into the infinite vector THINGS.  THING is walked recursively
;;; with each pointer being given a seperate slot in the vector.  Each pointer
;;; is looked up in the OBJECTS table to see if it has been encountered before.
;;; If it hasn't it is added to the table and any pointers it has are dealt
;;; with.

(define (vectorize thing things status objects encoder)
  (iterate label ((thing thing))
    (set (things (dump-status-count status)) thing)
    (modify (dump-status-count status)
            (lambda (x) (fx+ x 1)))
    (cond ((small-object? thing))
          ((table-entry objects thing)
           => (lambda (flag)
                (cond ((neq? flag 'many)
                       (modify (dump-status-duplicates status)
                               (lambda (x) (fx+ x 1)))
                       (set (table-entry objects thing) 'many)))))
          (else
           (set (table-entry objects thing) 'one)
           (cond ((pair? thing)
                  (label (car thing))
                  (label (cdr thing)))
                 ((vector? thing)
                  (do ((i 0 (fx+ 1 i)))
                      ((fx>= i (vector-length thing)))
                      (label (vref thing i))))
                 ((or (symbol? thing)
                      (bytev? thing)
                      (float? thing)
                      (bignum? thing)
                      (string? thing)))
                 (else
                  (receive (key data accessors)
                           (encoder thing)
                    (cond ((not key)
                           (label (error '"don't know how to dump ~S" thing)))
                          (else
                           (label key)
                           (do ((d data (cdr d))
                                (l '() (cons ((car d) thing) l)))
                               ((null-list? d)
                                (label (reverse! l))))
                                (label (length accessors))
                                (iterate loop ((l accessors))
                                  (cond ((null-list? l) (return))
                                        (else
                                         (label ((car l) thing))
                                         (loop (cdr l))))))))))))))
;                                (walk (lambda (proc) ; Compiler bug
;                                        (label (proc thing)))
;                                      accessors)


;;; The immediate types of object.

(define (small-object? thing)
  (or (null? thing)
      (fixnum? thing)
      (char? thing)
      (eq? thing '#t)))

(define (dump-small-object thing out)
  (cond ((null? thing)
         (dump-byte out dump/null))
        ((char? thing)
         (dump-byte out dump/char)
         (dump-byte out (char->ascii thing)))
        ((eq? thing '#t)
         (dump-byte out dump/true))
        ((not (fixnum? thing))
         (error "dump internal error, ~S is not a small object" thing))
        ((fx<= 0 thing)
         (dump-unshared-code&size out dump/positive-fixnum thing))
        ((fx= thing most-negative-fixnum)   ; no corresponding positive fixnum
         (dump-byte out (fx+ 3 dump/positive-fixnum)) ; four byte positive fix
         (dump-bytes out thing 4))
        (else
         (dump-unshared-code&size out dump/negative-fixnum (fx- 0 thing)))))

;;; Dump whatever non-pointer data an object may have.

(define (dump-data out thing shared)
  (cond ((symbol? thing)
         (dump-code&size out dump/symbol shared (symbol-print-length thing))
         (do ((i %%symbol-text-offset (fx+ i 1)))
             ((fx>= i (symbol-length thing)))
           (writec out (symbol-elt thing i))))
        ((pair? thing)
         (dump-code out dump/pair shared))
        ((vector? thing)
         (dump-code&size out dump/vector shared (vector-length thing)))
        ((string? thing)
         (dump-code&size out dump/string shared (string-length thing))
         (writes out thing))
        ((bytev? thing)
         (dump-code&size out dump/byte-vector shared (bytev-length thing))
         (do ((i 0 (fx+ i 1)))
             ((fx>= i (bytev-length thing)))
           (vm-write-byte out (bref-8 thing i))))
        ((bignum? thing)
         (let ((code (if (> 0 thing)
                         dump/negative-bignum
                         dump/positive-bignum))
               (length (bignum-length thing)))
           (dump-code&size out code shared length)
           (do ((i 0 (fx+ i 1)))
               ((fx>= i length))
             (dump-bytes out (bignum-digit thing i) 4))))
        ((double-float? thing)
         (dump-double-flonum out thing shared))
        (else
         (dump-code out dump/coded shared))))

(define (dump-double-flonum out flonum shared)
  (receive (sign m e)
           (integer-decode-float flonum)
    (cond ((fixnum? m)
           (dump-code&size out dump/double-flonum shared 0)
           (dump-byte out (if (eq? sign 1) 1 0))
           (dump-bytes out e 4)
           (dump-bytes out m 4))
          ((bignum? m)
           (let ((length (bignum-length m)))
             (dump-code&size out dump/double-flonum shared length)
             (dump-byte out sign)
             (dump-bytes out e 4)
             (do ((i 0 (fx+ i 1)))
                 ((fx>= i length))
               (dump-bytes out (bignum-digit m i) 4))))
          (else
           (vm-error 'dump "flonum decomposed oddly ~S" flonum)))))

;;; Write out a code that has no size field.

(define (dump-code out code shared)
  (dump-byte out (if shared (fx+ 1 code) code)))

;;; Write a code that has a size but no 'shared' field.

(define (dump-unshared-code&size out code num)
  (cond ((fx< num 0)
         (error "dump internal error, can't encode negative fixnum ~S" num))
        ((fx< num 256)
         (dump-byte out code)
         (dump-byte out num))
        ((fx< num 65536)
         (dump-byte out (fx+ 1 code))
         (dump-bytes out num 2))
        ((fx< num 16777216)
         (dump-byte out (fx+ 2 code))
         (dump-bytes out num 3))
        (else                           ; Should check here.
         (dump-byte out (fx+ 3 code))
         (dump-bytes out num 4))))

;;; Write a code that both size and 'shared' fields.

(define (dump-code&size out code shared num)
  (let ((code (if shared (fx+ 1 code) code)))
    (cond ((fx< num 0)
           (error "dump internal error, can't encode negative fixnum ~S" num))
          ((fx< num 256)
           (dump-byte out code)
           (dump-byte out num))
          ((fx< num 65536)
           (dump-byte out (fx+ 2 code))
           (dump-bytes out num 2))
          ((fx< num 16777216)
           (dump-byte out (fx+ 4 code))
           (dump-bytes out num 3))
          (else                           ; Should check here.
           (dump-byte out (fx+ 6 code))
           (dump-bytes out num 4)))))

;;; Write out various numbers of bytes.

(define (dump-byte out byte)
  (writec out (ascii->char (fixnum-logand byte #xFF))))

(define (dump-bytes out num count)
  (do ((i 0 (fx+ 8 i)))
      ((fx>= i (fx* count 8)))
    (dump-byte out (fixnum-ashr num i))))
