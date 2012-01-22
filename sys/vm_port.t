(herald vm_port
        (env tsys (osys buffer)))

;++ how to include e.g. Aegis_file.t in above? Logical names?

;;; The VM-system I/O and file routines.

;;; This file contains the virtual machine I/O interface.  An I/O
;;; buffer (IOB) is a system independent way to represent an open
;;; file, among other things.

;;; Character ports with current position information.
;;; Note: tab characters count as one horizontal position.

;++ All procedures in this file should be checking their arguments.
;++ They should also be checking the IOB-MODE to ensure that the
;++ operation is valid for that iob.

;++ Buffers will eventually have both read and write offsets, and
;++ update mode will be available, along with seeking and telling.

;;; End of file exception and object.

;++(define-exception (end-of-file port)
;++  (ignore port)
;++  eof)


(define eof
  (object nil
    ((print self port) (write-string port "#{End-of-file}"))))

(define-integrable (end-of-file port) (ignore port) eof)

;;; Input

;;; Internal used by retriever.  (This could be hand optimized (made
;;; a primop) if that became important.)

(define-recursive (VM-READ-BYTE iob)
  (cond ((fx< (iob-offset iob) (iob-limit iob))
         (let ((c (bref (iob-buffer iob) (iob-offset iob))))
           (set (iob-offset iob) (fx+ (iob-offset iob) 1))
           c))
        (else
         (if (eof? ((iob-underflow iob) iob nil))
             (end-of-file iob)
             (vm-read-byte iob)))))

;;; VM-READ-CHAR need not check for closed channels.  See
;;; CLOSE-port for an explanation.  (This could be hand optimized
;;; (made a primop) if it became important.)

(define-recursive (VM-READ-CHAR iob)
  (cond ((fx< (iob-offset iob) (iob-limit iob))
         (let ((c (text-elt (iob-buffer iob) (iob-offset iob))))
           (set (iob-offset iob) (fx+ (iob-offset iob) 1))
           ;++ the #\newline isn't portable?
           (cond ((charN= c #\newline)
                  (set (iob-h iob) (fx+ (iob-h iob) 1)))
                 (else
                  (set (iob-v iob) (fx+ (iob-v iob) 1))
                  (set (iob-prev-h iob) (iob-h iob))
                  (set (iob-h iob) 0)))
           c))
        (else
         (if (eof? ((iob-underflow iob) iob nil))
             (end-of-file iob)
             (vm-read-char iob)))))

(define-recursive (VM-MAYBE-READ-CHAR iob)
  (cond ((fx< (iob-offset iob) (iob-limit iob))
         (vm-read-char iob))
        (else
         (let ((val ((iob-underflow iob) iob t)))
           (cond ((eof? val)   (end-of-file iob))
                 ((false? val) nil)
                 (else
                  (vm-read-char iob)))))))

;;; Returns the last character read.  This procedure can only be
;;; called once without rereading the character.  UNREAD-CHAR cannot
;;; be called after calling PEEK-CHAR - this is a bug and should
;;; be fixed.  UNREAD-CHAR is probably more useful in writing
;;; recursive decent parsers then PEEK-CHAR (explain).  Note: it
;;; is an error (currently undetected) to unread and EOF.

(define (VM-UNREAD-CHAR iob)
  (cond ((fx> (iob-offset iob) 0)
         (cond ((iob-eof-flag? iob))
               (else
                (set (iob-offset iob) (fx- (iob-offset iob) 1))
                (cond ((char= (text-elt (iob-buffer iob) (iob-offset iob))
                              #\newline)
                       (set (iob-h iob) (iob-prev-h iob))
                       (set (iob-v iob) (fx- (iob-v iob) 1)))
                      (else 
                       (set (iob-h iob) (fx- (iob-h iob) 1)))))))
        (else
         ;; This could be made to work but it hardly seems worth
         ;; the effort.
         (non-continuable-error
          "consecutive attempt to UNREAD-CHAR on ~a" iob)))
   (no-value))

;;; This procedure can be called any number of times.

(define (VM-PEEK-CHAR IOB)
  (let ((val (vm-read-char iob)))
    (cond ((eof? val)
           (end-of-file iob))
          (else
           (vm-unread-char iob)
           val))))

;;; Block input

;;; Note: VM-READ-BLOCK allows reading of zero length blocks.
;++ use vm-read-partial-block to do this.
;++ fix to use VM-READ-PARTIAL-BLOCK

(define (make-extend-locative extend offset length)
  (let ((ptr (make-string 0)))
    (set (extend-elt ptr 0)   extend)
    (set (string-offset ptr) offset)
    (set (string-length ptr) length)
    ptr))

(define (old-VM-READ-BLOCK IOB EXTEND SIZE)
  (let ((size (enforce nonnegative-fixnum? size)))
    (iterate loop ((i 0))                         
      (cond ((fx>= i  size) i)
            (else
             (let ((val (vm-read-byte iob)))
               (cond ((eof? val)
                      (if (fx> i 0) i (end-of-file iob)))
                     (else
                      (set (bref extend i) val)
                      (loop (fx+ i 1))))))))))

;++ doesn't handle hpos or vpos
(define (vm-read-block iob extend size)
  (let* ((offset
          (iterate loop ((i 0))
            (cond ((and (fx< i size)
                        (fx< (iob-offset iob) (iob-limit iob)))
                   (set (text-elt extend i)
                        (text-elt (iob-buffer iob) (iob-offset iob)))
                   (set (iob-offset iob) (fx+ (iob-offset iob) 1))
                   (loop (fx+ i 1)))
                   (else i)))))
    (if (fx< offset size) 
        (%vm-read-partial-block 
         iob 
         (make-extend-locative extend offset (fx- size offset))))))

(define (VM-CLEAR-BUFFER iob)
  (set (iob-offset iob) (iob-limit iob)))

;++ what about vm-read-8-u, vm-read-integer, etc. and likewise
;++ vm-write-8 ...


;;; Output

;;; When a channel is closed it's limit is set to -1 so the test
;;; below fails on closed channels.
;;; Note: The only way for VPOS to advance is to use NEWLINE

(define-recursive (VM-WRITE-BYTE iob b)
  (cond ((fx< (iob-offset iob) (iob-limit iob))
         (set (bref (iob-buffer iob) (iob-offset iob)) b)
         (set (iob-offset iob) (fx+ (iob-offset iob) 1))
         (set (iob-h iob)      (fx+ (iob-h iob) 1))
         (no-value))
        (else
         ((iob-overflow iob) iob 1)
         (vm-write-byte iob b))))

(define-recursive (VM-WRITE-CHAR iob C)
  (cond ((fx< (iob-offset iob) (iob-limit iob))
         (set (text-elt (iob-buffer iob) (iob-offset iob)) c)
         (set (iob-offset iob) (fx+ (iob-offset iob) 1))
         (set (iob-h iob)      (fx+ (iob-h iob) 1))
         (no-value))
        (else
         ((iob-overflow iob) iob 1)
         (vm-write-char iob c))))

(define (VM-WRITE-SPACE iob)
  (cond ((or (fx>= (iob-h iob) (iob-wrap-column iob))
             (fx>= (iob-h iob) (iob-line-length iob)))
         (vm-newline iob))
        (else
         (vm-write-char iob #\space))))

(define (VM-WRITE-SPACES PORT N)           ;; Hack for FORMAT.
  (iterate loop ((i 0))
    (cond ((fx>= i n) (no-value))
          (else
           (vm-write-space port)
           (loop (fx+ i 1))))))

(define (VM-NEWLINE iob)
  ;; IOB-H must be set below IOB-INDENT before any calls to VM-WRITE-CHAR.
  ;; On some systems (Apollo) %VM-NEWLINE calls VM-WRITE-CHAR
  ;; Note: IOB-INDENT must be less than IOB-WRAP-COLUMN.
  (set (iob-h iob) 0)
  (%vm-newline iob)
  (if (iob-interactive? iob) (vm-force-output iob))
  (set (iob-v iob) (fx+ (iob-v iob) 1))
  (iterate loop ((i 0))
    (cond ((fx< i (iob-indent iob))
           (vm-write-char iob #\space)
           (loop (fx+ i 1)))
          (else
           (set (iob-h iob) i) 
           (no-value)))))

(define (VM-WRITE-FIXNUM IOB N RDX)
  (labels (((write-fx n)
            (cond ((fxN= n 0)
                   (write-fx (fx/ n rdx))
                   (let ((c (digit->char (fx-abs (fx-rem n rdx)) rdx)))
                     (vm-write-char iob c))))))
    (cond ((fx= n 0) (vm-write-char iob #\0))
          (else
           (if (fx< n 0) (vm-write-char iob negative-sign-char))
           (write-fx n)))))


;;; VM-WRITE-STRING and VM-WRITE-TEXT could be speeded up, by using
;;; MOVE-TEXT instead of VM-WRITE-CHAR.  Does it matter?

;++ these next three can be flushed since they're handled by the
;++ default ops.

(define (VM-WRITE-STRING IOB STR)
  (let ((len (string-length str)))
    (iterate loop ((i 0))
      (cond ((fx>= i len) (no-value))
            (else
             (vm-write-char iob (string-elt str i))
             (loop (fx+ 1 i)))))))

(define (VM-WRITE-TEXT IOB TEXT COUNT)
  (iterate loop ((i 0))
    (cond ((fx>= i count) (no-value))
          (else
           (vm-write-char iob (text-elt text i))
           (loop (fx+ 1 i))))))

(define (VM-WRITE-BLOCK IOB EXTEND OFFSET LENGTH)
  (let ((loc (make-extend-locative extend offset length)))
    (%vm-write-buffer iob)
    (%vm-write-block iob loc)))

(define (VM-FORCE-OUTPUT IOB)
  (%vm-write-buffer iob)
  (if (not (iob-interactive? iob)) (%vm-force-output iob))
  (no-value))

;;; File access.

(define (OPEN-PORT FILESPEC MODESPEC)
  (iterate loop ((fname filespec))
    (let ((val (%vm-open-file 'open-port
                              fname
                              modespec
                              default-buffer-size)))
      (cond ((iob? val) val)
            (else
             (receive vals
                      (error "(OPEN '~s '~s) failed - ~%~
                             **~10t [VM - ~s]~%~
                             **~10t Type (RET) or (RET filespec) to retry."
                             fname 
                             modespec 
                             (local-os-error-message val))
               (if (null? vals) 
                   (loop filespec)
                   (loop (car vals)))))))))

(define (MAYBE-OPEN-PORT FILESPEC MODESPEC)
  (let ((mode (mode->iob-mode 'maybe-open-port filespec modespec)))
  ;++ temp gross hack
    (cond ((iob-mode? mode iob/retrieve)
           (maybe-open-retrieve-file filespec))
          ((iob-mode? mode iob/dump)
           (maybe-open-dump-file filespec))
          (else
           (let ((val (%vm-open-file 'maybe-open-port
                                     filespec
                                     modespec
                                     default-buffer-size)))
             (if (iob? val) val '#f))))))
    
;++ should this do an implicit close?
(define (RE-OPEN-PORT! PORT MODESPEC)
  (cond ((not (iob-closed? port))
         (error "attempt to re-open an open file ~a" port))
        ((eq? 'anonymous (iob-id port))    
         (error "attempt to re-open an anonomous file ~a" port))
        (else                          
         (open-port port modespec)))
  (no-value))

;;; When a iob is closed it's limit is set to -1 so that it will
;;; fail the first test in VM-READ-CHAR, VM-READ-BYTE, VM-WRITE-CHAR, 
;;; and VM-WRITE-BYTE the
;;; overflow code will then generate a closed IOB error.

(define (CLOSE-PORT iob)
  (let ((iob (enforce iob? iob)))
    (cond ((iob-permanent? iob)
           (nc-error "attempt to close a permanent port - ~a" iob))
          ((iob-closed? iob)
           (no-value))
          (else
           (if (iob-writable? iob) (%vm-write-buffer iob))
           (if (iob-channel iob) (%vm-close-file iob))
        ;++(set (table-entry open-port-table iob) nil)
           (release-buffer-text %buffer-pool iob)
           (set (iob-buffer iob) '#f)
           (set (iob-mode   iob) iob/closed)
           (set (iob-xeno   iob) '#f)
           ;; make it fail in VM-READ-CHAR
           (set (iob-limit  iob) -1)
           (no-value)))))


(define (with-open-ports-handler proc . openers)
  (let ((ports '()))
    (unwind-protect
     (block (walk (lambda (opener) (push ports (opener)))
                  ;; careful - don't use map here!
                  openers)
            ;; thanks to nat for the (set ports ...)
            (apply proc (set ports (reverse! ports))))
     (walk (lambda (port)
             ;; deal with maybe-open.
             (cond (port 
                    (close port)
                    (if (iob? port) (release-buffer port)))))
           ports))))

(define (file-exists? filespec)
  (let ((val nil))
    (unwind-protect
      (let ()
        (set val (maybe-open-port filespec 'inquire))
        (if val t nil))
      (if val (close-port val)))))

;;; Standard I/O ports

;;; E.g. (READ (STANDARD-INPUT))
;;; (BIND (((TERMINAL-INPUT) FOO-port)) ...)

(define-simple-switch standard-input  input-port?)
(define-simple-switch standard-output output-port?)
(define-simple-switch standard-i/o    port?)

(define-simple-switch error-input     input-port?)
(define-simple-switch error-output    output-port?)
(define-simple-switch error-i/o       port?)

(define-simple-switch terminal-input  input-port?)
(define-simple-switch terminal-output output-port?)
(define-simple-switch terminal-i/o    port?)

(define-simple-switch debug-input     input-port?)
(define-simple-switch debug-output    output-port?)
(define-simple-switch debug-i/o       port?)

(define-simple-switch crawl-input     input-port?)
(define-simple-switch crawl-output    output-port?)
(define-simple-switch crawl-i/o       port?)


(define (initialize-standard-ports)
  (set (standard-input)  (create-iob 'standard-input
                                     %%standard-input
                                     (fx-ior iob/read
                                             (fx-ior iob/interactive
                                                     iob/permanent))
                                     512))
  (set (standard-output) (create-iob 'standard-output
                                     %%standard-output
                                     (fx-ior iob/write
                                             (fx-ior iob/interactive
                                                     iob/permanent))
                                     512))
  (set (standard-i/o)    (join (standard-input) (standard-output)))

  (set (error-input)     (standard-input))
  (set (error-output)    (standard-output))
  (set (error-i/o)       (join (error-input) (error-output)))

  (set (terminal-input)  (standard-input))
  (set (terminal-output) (standard-output))
  (set (terminal-i/o)    (join (terminal-input) (terminal-output)))

  (set (debug-input)     (error-input))
  (set (debug-output)    (error-output))
  (set (debug-i/o)       (join (debug-input) (debug-output)))

  (set (crawl-input)     (standard-input))
  (set (crawl-output)    (standard-output))
  (set (crawl-i/o)       (join (crawl-input) (crawl-output)))
    )


;;; Hack for no apparent reason.

(define-constant null-port
  (object nil
    ((read-char self)      eof)
    ((unread-char self)    (no-value))
    ((write-char self ch)  (ignore ch) (no-value))
    ((input-port? self)  '#t)
    ((output-port? self) '#t)
    ((port? self)        '#t)
    ((print self port) (format port "#{Null port}"))))

;;; GC hook: arrange to close open ports for ports to which
;;; there are no pointers.

;(define (gc-close-unreferenced-ports)
; (walk-table open-port-table
;             (lambda (port h)
;               (cond ((not (object-unhash h))
;                      (close-port port)
;                      (gc-message "port closed: ~s~%" port))))))

;;; If *POST-GC-AGENDA* doesn't have at least one element, then
;;; we're really losing.

;++ why not move this stuff to gc-aux.t
;(append! *post-gc-agenda*
;         (list (cons 'gc-close-unreferenced-ports
;                     gc-close-unreferenced-ports)))



;++ move this to the appropriate place someday
(initialize-standard-ports)
