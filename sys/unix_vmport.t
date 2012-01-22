(herald unix_vmport
  (env tsys
       (osys unix_fault)
       (osys buffer)))

;;; The Unix interface to I/O routines.

;;; This file contains the virtual machine I/O interface to the
;;; aegis operating system.  Where possible we use IOS calls rather
;;; than say NAME calls, so that T takes advantage of Extensible
;;; ports.

;;; Used on startup.  ** This must be a top level procedure **
;;; The losing 1's in UNIX-WRITE below are really %%stdout!

(define (%VM-BOOT-WRITE-TTY SYMBOL)
  (unix-write 1 (make-pointer symbol 0) (fx- (symbol-length symbol) 4))
  (let ((newline (make-text 1)))
    (set (text-elt newline 0) #\newline)
    (unix-write 1 newline 1)))

;;; Z system i/o

(define-constant %%standard-input  0)
(define-constant %%standard-output 1)
(define-constant %%error-input     2)
(define-constant %%error-output    1)

;;; Input

(define (%VM-READ-BUFFER IOB BLOCK?)
  (ignore block?)
  (let ((length (unix-read (iob-channel iob)
                           (iob-buffer  iob)
                           (max-buffer-length iob))))
    (xcond ((fx> length 0)
            (set (iob-offset iob) 0)
            (set (iob-limit iob) length)
            (set (iob-eof-flag? iob) nil)
            length)
           ((fx= length 0)
            (set (iob-eof-flag? iob) t)
            eof)
           ((fx= length -1)
            (local-os-error length)))))

(define-foreign unix-read (read (in rep/integer)
                                (in rep/extend)
                                (in rep/integer))
                rep/integer)

;;; Block input

(DEFINE (%VM-READ-PARTIAL-BLOCK IOB LOC)
  (let* ((length (string-length loc))
         (length (unix-partial-read (iob-channel iob) loc length)))
    (cond ((fx> length 0)
           (set (iob-eof-flag? iob) nil)
           length)
          ((fx= length 0)
           (set (iob-eof-flag? iob) t)
           (end-of-file iob))
          (else
           (local-os-error length)))))

(define-foreign unix-partial-read (read (in rep/integer)
                                        (in rep/string)
                                        (in rep/integer))
                rep/integer)

;;; Output

;;; Writing a zero length buffer is a no-op, not an error.

(define (%VM-WRITE-BUFFER IOB)
  (cond ((fx> (iob-offset iob) 0)
         (let ((offset (iob-offset iob))
               (length (unix-write (iob-channel iob)
                                   (iob-buffer  iob)
                                   (iob-offset  iob))))
           (set (iob-offset iob) 0)
           (cond ((fxN= length offset)         
                  (local-os-error length))))))
  (no-value))

(define-foreign unix-write (write (in rep/integer)
                                  (in rep/extend)
                                  (in rep/integer))
                rep/integer)

;;; Newline is system dependent because of OS's like VMS.

(define-integrable (%vm-newline iob)
  (vm-write-char iob #\newline))

;;; Block output

(define (%VM-WRITE-BLOCK IOB LOC)
  (let ((length (string-length loc)))
    (check-status (unix-write-string (iob-channel iob) loc length)))
  (no-value))

(define-foreign unix-write-string (write (in rep/integer)
                                         (in rep/string)
                                         (in rep/integer))
                rep/integer)

(define (%VM-FORCE-OUTPUT IOB)
  (check-status (unix-fsync (iob-channel iob)))
  (no-value))

(define-foreign unix-fsync (fsync (in rep/integer))
                rep/integer)


;;; The rest of this file doesn't have to be implemented to get
;;; the Z System booted.

;;; Pathnames

;;; Pathnames are system dependent objects used for naming files
;;; internally.  They should not be accessible outside of this file.

;;; FILESPEC    -   Something that ->FILENAME can handle
;;; FILENAME    -   The result of  ->FILENAME
;;; PATHNAME    -   A string in the LOCAL-OS format
;;; (->PATHNAME filespec) => pathname

;;; Internal utilities

(define-constant pathname-length string-length)

;;; ->PATHNAME provides the VM with a portable interface to the
;;; various file systems.
;++ this is consing and it shouldn't be. it should also be portable

(define (->pathname filespec)
  (let* ((buf (get-string-buffer-of-size 120))
         (str (string->asciz! 
               (cond ((string?   filespec) filespec)
                     ((iob? filespec) (iob-id filespec))
                     ((not (file-system-present?))
                      (error "Filespecs must be strings in VM."))
                     ((filename? filespec) (filename->string filespec))
                     (else
                      (filename->string (->filename filespec)))))))
    (set (string-length buf) 120)
    (unix-expand-path str buf)
    (set (string-length buf) (string-posq #\null buf))
    buf))

(define-foreign unix-expand-path (expand_path (in rep/string)
                                              (in rep/string))
                rep/integer)

;;; File opening and closing.
;++ What about pads, sockets, etc.
;++ There should be a population of all open IOB's.

;;; On the Apollo bench mark tests show a buffer size of 128 to
;;; be as effective as one of 512 or 1024.

(define default-buffer-size 512)

(define default-file-access #o644)

;;; (VM-OPEN-FILE name mode access size)
;;; CALLER   - a symbol identifying the caller, used for error output.
;;; NAME     - the file to be opened, either a FILESPEC or IOB.
;;; MODESPEC - a symbol or list of symbols parsable by MODE->IOB-MODE.
;;; ACCESS   - the access mode for the file if it is being created.
;;; SIZE     - the size of the buffer to be used with this port.

;++ this should take an access argument
(define (%vm-open-file caller fd modespec size)
  (let* ((path (->pathname (if (iob? fd) (iob-id fd) fd)))
         (mode (mode->iob-mode caller fd modespec))
         (chan (unix-open path
                          (cond ((iob-mode? mode iob/read)
                                 file-mode/in)
                                ((iob-mode? mode iob/write)
                                 file-mode/out)
                                ((iob-mode? mode iob/inquire)
                                 file-mode/in)
                                ((iob-mode? mode iob/append)
                                 file-mode/append)
                                (else
                                 (unsupported-mode-error caller fd modespec)))
                          default-file-access)))
    (release-string-buffer path)
    (cond ((fx< chan 0) nil)
	  (else
	   (let* ((size (if (iob-mode? mode iob/inquire) 0 size))
		  (iob (get-i/o-buffer %buffer-pool fd chan mode size)))
;++               (set (table-entry open-port-table iob) (object-hash iob))
	     iob)))))

(define-foreign unix-open (open (in rep/string)
                                (in rep/integer)
                                (in rep/integer))
                rep/integer)

(define (%VM-CLOSE-FILE IOB)
  (check-status (unix-close (iob-channel iob))))

(define-foreign unix-close (close (in rep/integer))
                rep/integer)
   


(comment
  (define-foreign unix-ftruncate (ftruncate (in rep/integer)
                                            (in rep/integer))
                  rep/integer)

  (define-foreign unix-lseek (lseek (in rep/integer)
                                    (in rep/integer)
                                    (in rep/integer))
                  rep/integer)

)

