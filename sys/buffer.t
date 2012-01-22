(herald buffer
  (env tsys))

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

;;; (import pool structure)
;;; (import (let valid-spec?))

;;; describe buffers

;++ T3 plans include:
;++  buffered i/o; update mode; re-openability; seeking & telling;
;++  TCP/IP interface.
;++ Change internal names to %buffer

;++ what should be integrated?
;++ pooled structures
;++ should these things have read-tables? probably.



;;; Buffer management.

;;; %buffer modes

;;; Major modes
(define-constant iob/closed      #x00)    ; mode field set to zero
(define-constant iob/read        #x01)
(define-constant iob/write       #x02)
(define-constant iob/append      #x04)
(define-constant iob/dump        #x08)    ;++ remove
(define-constant iob/retrieve    #x10)   ;++ remove
(define-constant iob/inquire     #x20)   ; used to get info about the file.

;;; Minor modes
(define-constant iob/interactive #x0100)
(define-constant iob/permanent   #x0200)  ; cannot be closed
(define-constant iob/socket      #x0400)  ; TCP/IP
(define-constant iob/window      #x0800)  ;
(define-constant iob/transcript  #x1000)  ;

;;; Mode predicates

(define-integrable (iob-mode? mode type) (fxN= 0 (fx-and mode type)))

(define-integrable (iob-closed? iob)      (fx-zero? (iob-mode iob)))
(define-integrable (iob-readable? iob)    (iob-mode? (iob-mode iob) iob/read))
(define-integrable (iob-writable? iob)    (iob-mode? (iob-mode iob) iob/write))
(define-integrable (iob-append? iob)      (iob-mode? (iob-mode iob) iob/append))
(define-integrable (iob-inquire? iob)     (iob-mode? (iob-mode iob) iob/inquire))
(define-integrable (iob-dump? iob)        (iob-mode? (iob-mode iob) iob/dump))
(define-integrable (iob-retrieve? iob)    (iob-mode? (iob-mode iob) iob/retrieve))

(define-integrable (iob-interactive? iob) (iob-mode? (iob-mode iob) iob/interactive))
(define-integrable (iob-permanent? iob)   (iob-mode? (iob-mode iob) iob/permanent))
(define-integrable (iob-socket? iob)      (iob-mode? (iob-mode iob) iob/socket))
(define-integrable (iob-window? iob)      (iob-mode? (iob-mode iob) iob/window))
(define-integrable (iob-transcript? iob)  (iob-mode? (iob-mode iob) iob/transcript))

;;; Convert a mode or mode list to an iob-mode.

(define (mode->iob-mode caller filespec modespec)
  (labels (((major mode item)
            (let ((val (case item
                         ((in)          iob/read)
                         ((out)         iob/write)
                         ((inquire)     iob/inquire)
                         ((append)      iob/append)
                         ((dump)        (fx-ior iob/write iob/dump))
                         ((retrieve)    (fx-ior iob/read iob/retrieve))
                         (else          (mode-error item)))))
              (fx-ior mode val)))
         ((minor mode items)
          (iterate loop ((mode mode) (items items))
            (if (null? items)
                mode
                (let ((val (case (car items)
                             ((interactive) iob/interactive)
                             ((permanent)   iob/permanent)
                             ((socket)      iob/socket)
                             ((window)      iob/window)
                             ((transcript)  iob/transcript)
                             (else          (mode-error (car items))))))
                  (loop (fx-ior mode val) (cdr items))))))
         ((mode-error item)
          (mode->iob-mode 
           caller 
           filespec
           (error "bad file mode ~s in - (~s ~a ~s ...)~
                  ~10tType (RET mode) to retry."
                  item
                  caller 
                  (if (iob? filespec) (iob-id filespec) filespec)
                  modespec))))
    (if (pair? modespec)
        (minor (major 0 (car modespec)) (cdr modespec))
        (major 0 modespec))))
                               
(define (unsupported-mode-error proc filespec mode)
  (error "unsupported file mode - (~s ~a ~s ...)~
          ~10tType (RET modespec) to retry."
          proc 
          (if (iob? filespec) (iob-id filespec) filespec)
          mode))
                               

;++ write-string, read-block, force-output, newline,
;++ peek-char, port->iob, close, and re-open can be flushed from
;++ IOB.

(define-structure-type iob
                       id          ; pathname
                       mode        ; type of buffer
                       rt          ; read-table
                       buffer      ; text to hold data (bytev?)
                       offset      ; current position in buffer
                       limit       ; end of data in buffer
                       underflow   ; underflow procedure
                       overflow    ; overflow procedure
                       xeno        ; system dependent descriptor
                                   ; or 'buffer.
                       h           ; hpos
                       prev-h      ; previous hpos
                       v           ; vpos
                       indent
                       wrap-column
                       line-length          
                       eof-flag?
  (((read-char       (#f obj))       (vm-read-char obj))
   ((write-char      (#f obj) c)     (vm-write-char obj c))
   ((maybe-read-char (#f obj))       (vm-maybe-read-char obj))
   ((newline         (#f obj))       (vm-newline obj))
   ((unread-char     (#f obj))       (vm-unread-char obj))
   ((peek-char       (#f obj))       (vm-peek-char obj))
   ((write-string    (#f obj) s)     (vm-write-string obj s))
   ((force-output    (#f obj))       (vm-force-output obj))
   ((read-block (#f obj) extend cnt)
    (vm-read-block obj extend cnt))
   ((hpos            (#f obj))      (if (iob-closed? obj)
                                              (closed-port-error obj)
                                              (iob-h obj)))
   ((vpos            (#f obj))       (if (iob-closed? obj)
                                               (closed-port-error obj)
                                               (iob-v obj)))
   ((line-length     (#f obj))       (iob-line-length obj))
   ((set-line-length (#f obj) len)   (set (iob-line-length obj) len))
   ((wrap-column     (#f obj))       (iob-wrap-column obj))
   ((set-wrap-column (#f obj) len)   (set (iob-wrap-column obj) len))
   ((input-port?     (#f obj))       (iob-readable? obj))
   ((output-port?    (#f obj))       (iob-writable? obj))
   ((interactive?    (#f obj))       (iob-interactive? obj))
   ((port? self)                           (ignore self) '#t)
   ((port->iob       (#f obj)) obj)
   ((port-read-table (#f obj))
    (cond ((iob-rt obj))
          (else standard-read-table)))
   ((set-port-read-table (#f obj) new-read-table)
    (set (iob-rt obj) new-read-table))
   ((port-name (#f obj))              (iob-id obj))
   ((set-port-name (#f obj) name)     (set (iob-id obj) name))
   ((close           (#f obj))        (close-port obj))
   ((re-open         (#f obj) mode)   (re-open-port! obj mode))
   ((display         (#f obj) stream)
    (iterate loop ((i 0))
      (let ((buffer (iob-buffer obj)))
        (cond ((fx>= i (iob-offset obj)) (no-value))
              (else
               (vm-write-char stream (text-elt buffer i))
               (loop (fx+ i 1)))))))
   ((print (#f obj) stream)
    (format stream "#{Port~_~a~_~a}"
                    (iob-id obj)
                    (object-hash obj)))))

;++ initialize the STYPE master.

(define standard-line-length 80)
(define standard-wrap-column (fx- standard-line-length 15))

(set (iob-id          (stype-master iob-stype)) nil)
(set (iob-mode        (stype-master iob-stype)) iob/closed)
(set (iob-buffer      (stype-master iob-stype)) '#f)
(set (iob-eof-flag?   (stype-master iob-stype)) '#f)
(set (iob-h           (stype-master iob-stype)) 0)
(set (iob-prev-h      (stype-master iob-stype)) 0)
(set (iob-v           (stype-master iob-stype)) 0)
(set (iob-indent      (stype-master iob-stype)) 0)
(set (iob-wrap-column (stype-master iob-stype)) standard-wrap-column)
(set (iob-line-length (stype-master iob-stype)) standard-line-length)
(set (iob-rt          (stype-master iob-stype)) '#f)

(define-constant buffer?       iob?)
(define-constant buffer-length iob-offset)
(define-constant buffer-text   iob-buffer)
(define-constant (buffer-empty? iob)
  (if (fx= (iob-offset iob) 0) '#t '#f))

(define buffer-elt
  (object (lambda (iob n)
            (text-elt (iob-buffer iob) n))  
    ((setter self)
     (lambda (iob n ch)
       (let ((ch (enforce char? ch)))
         (set (text-elt (iob-buffer iob) n) ch))))))

(define-integrable (max-buffer-length iob)
  (text-length (iob-buffer iob)))

(define (buffer-fill! iob char count)
  (let ((iob  (enforce buffer? iob))
        (char (enforce char?   char)))
    (do ((i 0 (fx+ i 1)))
        ((fx>= i count))
      (vm-write-char iob char)))
   (no-value))

(define (buffer->string! b)
  (let ((s (make-string 0)))
    (set (string-text s)   (iob-buffer b))
    (set (string-length s) (buffer-length b))
    s))

(define (buffer->string iob)
  (let* ((len  (buffer-length iob))
         (str  (make-string len))
         (text (string-text str)))
    (move-text (iob-buffer iob) 0 text 0 len)
    str))

(define (string->input-port str)
  (let* ((len  (string-length str))
         (iob  (get-buffer-of-size len))
         (text (iob-buffer iob)))
    (do ((i 0 (fx+ i 1)))
        ((fx>= i len)
         (set (iob-offset iob) 0)
         (set (iob-limit iob)  len)
         (set (iob-mode iob) iob/read)
         iob)
      (set (text-elt text i) (string-elt str i)))))

;++ Should return an update port, but for now it returns an input
;++ port.

(define string->buffer string->input-port)

;;; Make sure that the channel hasn't been closed

(define (iob-channel iob)
  (if (iob-closed? iob) (closed-port-error iob) (iob-xeno iob)))

(define (closed-port-error iob)
  (non-continuable-error "~s is closed." (iob-id iob)))


;++ Should this be lap? or primop. This uses indexing, on a machine
;++ with tags it would use pointers into objects.
;++ move it to the appropriate file.

(define-integrable (MOVE-TEXT SRC S-OFF DST D-OFF N)
  (do ((n n (fx- n 1))
       (s-off s-off (fx+ s-off 1))
       (d-off d-off (fx+ d-off 1)))
      ((fx<= n 0) (no-value))
    (set (text-elt dst d-off) (text-elt src s-off))))

;;; Make an I/O buffer.  Used by VM before pools are available.

(define (CREATE-IOB ID CHAN MODE SIZE)
  (let ((iob (make-iob)))
    (set (iob-buffer iob) (make-text size))
    (initialize-iob iob id chan mode)))

(define (ensure-iob-size text-pool iob size)
  (cond ((not (iob-buffer iob))
         (set (iob-buffer iob) (obtain-from-pool (text-pool size))))
        ((fx> size (max-buffer-length iob))
         (let ((text (iob-buffer iob)))
           (return-to-pool (text-pool (text-length text)) text)
           (set (iob-buffer iob) (obtain-from-pool (text-pool size)))))))

(define (initialize-iob iob id chan mode)
  (set (iob-id          iob) id)
  (set (iob-mode        iob) mode)
  (set (iob-offset      iob) 0)
  (set (iob-xeno        iob) chan)
  (set (iob-h           iob) 0)
  (set (iob-prev-h      iob) 0)
  (set (iob-v           iob) 0)
  (set (iob-indent      iob) 0)
  (set (iob-wrap-column iob) standard-wrap-column)
  (set (iob-line-length iob) standard-line-length)
  (set (iob-rt          iob) '#f)
  (set (iob-eof-flag?   iob) '#f)
  (cond ((iob-readable? iob)
         (set (iob-limit iob) 0)
         (set (iob-underflow iob) %vm-read-buffer)
         (set (iob-overflow iob) overflow-error))
        ((or (iob-writable? iob) (iob-append? iob))
         (set (iob-limit iob) (max-buffer-length iob))
         (set (iob-underflow iob) underflow-error)
         (set (iob-overflow iob) (lambda (iob size)
                                   (ignore size)
                                   (%vm-write-buffer iob)))))
  iob)

(define (overflow-error buf size)
  (ignore size)
  (error "buffer ~a overflowed." buf))

(define (underflow-error buf block?) (ignore buf block?) eof)

;;; There are ten pools, for buffers of various sizes.
;;;    0    1    2    3     4     5     6     7      8      9
;;;   64  128  256  512  1024  2048  4096  8192  16834  32768

;;; Return a pool from which one can obtain a buffer whose size
;;; is >= N.

(define (make-vector-of-pools maker type? min-size max-size)
  (let ((pools (make-vector 10)))
    (do ((i 0 (fx+ i 1))
         (n min-size (fixnum-ashl n 1)))
        ((fx> i 9))
      (set (vref pools i)
           (make-pool `(extend-pool ,i)
                      (lambda () (maker n))
                      1
                      type?)))
    (lambda (n)
      (cond ((fx<= n min-size)
             (vref pools 0))  ; speed hack for common case
            (else
             (let ((i (fixnum-howlong (fixnum-ashr (fx- n 1) 6))))
               (if (fx> n max-size)
                   (error "cannot allocate buffer of size ~a~%" n)
                   (vref pools i))))))))


(define-operation (obtain pool))
(define-operation (release pool))
(define-operation (release-buffer-text pool buffer))
(define-operation (get-i/o-buffer pool id chan mode size))

;;; Note: OVERFLOW below is a bit complicated and gross.  It makes
;;;       sure that the IOB can hold at least N additional characters.
;;;       If not the buffers size is increased by allocating a buffer
;;;       of the appropriate size, copying the contents of the old
;;;       buffer to the new, and finally exchanging the text pointers
;;;       of the two buffers creating a transparent side effect.

(define (make-buffer-pool)
  (let* ((iob-pool  (make-pool 'buffer-pool make-iob 1 iob?))
         (text-pool (make-vector-of-pools  make-text
                                           text?
                                           min-iob-size
                                           max-iob-size))
         (rel-text  (lambda (text)
                      (return-to-pool
                        (text-pool (text-length text)) text)))
         (underflow (lambda (iob #f) (end-of-file iob)))
         (overflow  (lambda (iob n)
                      (let* ((old-size (text-length (iob-buffer iob)))
                             (temp (obtain-from-pool
                                    (text-pool (fx+ old-size n)))))
                        (move-text (iob-buffer iob) 0 temp 0 old-size)
                        (exchange (iob-buffer iob) temp)
                        (return-to-pool (text-pool old-size) temp))
                      (set (iob-limit iob) (max-buffer-length iob))
                      (no-value)))
         (get-buffer (lambda (mode size)
                       (let ((iob  (obtain-from-pool iob-pool))
                             (text (obtain-from-pool (text-pool size))))
                         (set (iob-buffer iob) text)
                         (init-buffer iob mode underflow overflow)))))
    (object (lambda (mode size)
              (get-buffer mode size))
      ((obtain self)
       (get-buffer iob/write 0))
      ((release self obj)
       (let* ((iob  (enforce iob? obj))
              (text (iob-buffer iob)))
         (set (iob-buffer iob) '#f)
         (set (iob-id     iob) '#f)
         (set (iob-xeno   iob) '#f)
         (if text (rel-text text))
         (return-to-pool iob-pool iob)))
      ((release-buffer-text self obj)
       (let ((iob (enforce iob? obj)))
         (let ((text (iob-buffer iob)))
           (set (iob-buffer iob) '#f)
           (rel-text text))))
      ((get-i/o-buffer self file chan mode size)
       (receive (iob id)
                (if (iob? file)
                    (return file (iob-id file))
                    (return (obtain-from-pool iob-pool) file))
         (ensure-iob-size text-pool iob size)
         (initialize-iob iob id chan mode)))
      ((pool-statistics self stream)
       (pool-statistics iob-pool stream))
      ((print-type-string self) "Buffer pool"))))

;;; Initialize an ephemeral buffer

(define (init-buffer buf mode underflow overflow)
  (set (iob-mode        buf) mode)
  (set (iob-offset      buf) 0)
  (set (iob-h           buf) 0)
  (set (iob-prev-h      buf) 0)
  (set (iob-v           buf) 0)
  (set (iob-indent      buf) 0)
  (set (iob-wrap-column buf) standard-wrap-column)
  (set (iob-line-length buf) standard-line-length)
  (set (iob-rt          buf) '#f)
  (set (iob-eof-flag?   buf) '#f)
  (cond ((iob-readable? buf)
         (set (iob-limit     buf) 0)
         (set (iob-underflow buf) underflow)
         (set (iob-overflow  buf) overflow-error))
        ((iob-writable? buf)
         (set (iob-limit     buf) (max-buffer-length buf))
         (set (iob-underflow buf) underflow-error)
         (set (iob-overflow  buf) overflow)))
  buf)


;;; T's internal buffers.  There used for real and ephemeral I/O.
;;; This stuff will eventually be eliminated and the higher level
;;; stuff above will replace it.

(define-constant min-iob-size 64)
(define-constant max-iob-size 32768)

(define %buffer-pool (make-buffer-pool))

;;; Obtain a small buffer.

(define-integrable (GET-BUFFER)
  (%buffer-pool iob/write 0))

;;; Obtain a buffer whose size is >= N.

(define-integrable (GET-BUFFER-OF-SIZE SIZE)
  (let ((size (enforce fixnum? size)))
    (%buffer-pool iob/write size)))

;;; Release an iob.

(define-integrable (RELEASE-BUFFER iob)
  (release %buffer-pool iob))

;;; a portable interface to buffered i/o

(define (channel->port channel name modespec buffer-size)
  (let* ((mode (mode->iob-mode 'channel->port name modespec))
         (iob (get-i/o-buffer %buffer-pool name channel mode buffer-size)))
;++ (set (table-entry open-port-table iob) (object-hash iob))
    iob))
