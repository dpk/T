(herald bsd4_2 (env tsys))

(define file-mode/in     #o0)
(define file-mode/out    #o3001)
(define file-mode/append #o1011)

(define-constant number-of-signals 27)   ;4.2


;;; handler-types (Htype): A = asynchronous, E = exception, D = default,
;;; I = ignore
;;; (sig# handler-type handler description)

(define *signals*
  '(( 1   E    non-continuable  "hangup")
 ;   ( 2   A    sigint-handler    "interrupt")
 ;   ( 3   A    siquit-handler    "quit")
    ( 4   E    non-continuable  "illegal instruction")
    ( 5   E    non-continuable  "trace/BPT trap")
    ( 6   E    non-continuable  "IOT instruction")
    ( 7   E    non-continuable  "EMT instruction")
    ( 8   E    non-continuable  "floating point exception")
 ;   ( 9   D    default          "kill")
    (10   E    non-continuable  "memory protection violation")
    (11   E    non-continuable  "reference to non-existent memory")
    (12   E    non-continuable  "bad argument to a system call")
    (13   E    non-continuable  "broken pipe")
 ;   (14   D    default          "alarm clock")
 ;   (15   A    sigterm-handler   "software termination signal")
 ;   (16   D    default          "urgent condition on socket")
 ;   (17   D    default          "stop")
 ;   (18   D    default          "stop signal generated from keyboard")
 ;   (19   D    default          "continue after stop")
 ;   (20   D    default          "child status has changed")
 ;   (21   D    default          "background read attempted")
 ;   (22   D    default          "background write attempted")
 ;   (23   D    default          "i/o is possible")
    (24   E    non-continuable  "cpu time limit exceeded")
    (25   E    non-continuable  "file size limit exceeded")
  ;  (26   D    default          "virtual time alarm")
  ;  (27   D    default          "profiling timer alarm")
  ))

(define-constant %%SIGINT     2)
(define-constant %%SIGQUIT    3)
(define-constant %%SIGTERM    15)
(define-constant %%SIGSTOP    17)

;;; Stop the process and return to it's parent.  The process can be
;;; re-entered later.

(lset stop-system-agenda (make-agenda 'exit-agenda))
(lset continue-system-agenda (make-agenda 'exit-agenda))

(define (stop)
  (stop-system-agenda)
  (unix-kill 0 %%sigstop)
  (continue-system-agenda)
  repl-wont-print)

(define-foreign unix-kill (kill (in rep/integer)
                                (in rep/integer))
                rep/integer)

(define-foreign unix-getpid
  (getpid)
  rep/integer)

(define-foreign r-nlistone
  (nlistone (in rep/string filename)
	    (in rep/string functionName))
  rep/integer)

(define-integrable (nlistone file function)
  (r-nlistone (string->asciz! (copy-string file))
	      (string->asciz! (copy-string function))))



;;; loader for foreign code under Unix ... in particular, C
;;; by Dorab Patel <dorab@neptune.cs.ucla.edu>
;;; Original: Feb 29, 1984
;;; Modified for t2.8: May 22, 1984     dorab@neptune.cs.ucla.edu
;;; Modified for t3: Dec 24, 1986       dorab@neptune.cs.ucla.edu

(define (make-foreign-procedure sym)
  (let ((xeno (make-foreign sym))
	(addr (nlistone (check-arg file-exists?
				   (reloc-file)
				   make-foreign-procedure)
			(symbol->string sym))))
       (cond ((fxn= addr 0)
	      (set (mref-integer xeno 4) addr)
	      xeno)
	     (else
	      (error "foreign procedure \"~a\" does not exist in file \"~a\""
		     (symbol->string sym)
		     (reloc-file))))))


;;; loader for foreign code under Unix ... in particular, C
;;; by Dorab Patel <dorab@cs.ucla.edu>
;;; Original: Feb 29, 1984
;;; Modified for t2.8: May 22, 1984	dorab@cs.ucla.edu
;;; Modified for t3: Dec 24, 1986	dorab@cs.ucla.edu
;;; Bugfix in reloc-file: Dec 23, 1987	dorab@cs.ucla.edu

;;; Copyright Dorab Patel (C) 1984, 1986, 1987, 1988
;;; Permission is given to distribute this software free to anyone
;;; using it for a non-commercial purpose as long as the copyright notice
;;; is maintained.
;;; Comments/bug reports/fixes are encouraged.

;;; defined in dynload.c
;;; loads in objfile
;;; returns 0 if OK, else > 0
;;; **********************************************************************
(define-foreign r-loadhelp
  (loadhelp (in rep/string objfile)
	    (in rep/string relocfile)
	    (in rep/string tmpfile)
	    (in rep/string libstring)
	    (in rep/string otherstring))
  rep/integer)

(define-integrable
  (unix-load-help objfile relocfile tmpfile libstring otherstring)
  (r-loadhelp (string->asciz! (copy-string objfile))
	      (string->asciz! (copy-string relocfile))
	      (string->asciz! (copy-string tmpfile))
	      (string->asciz! (copy-string libstring))
	      (string->asciz! (copy-string otherstring))))

;;; searchpath is a general utility function that takes a colon-separated
;;; path list and a filename, and finds the first file that exists in that
;;; directory list.
;;; maybe it should be elsewhere ?
;;; *********************************************************************
(define (searchpath path file)
  (labels (
	   ;; convert a colon-separated path into a list.
	   ;; empty fields map to the current directory "."
	   ;; **********************
	   ((splitpath path)
	    (iterate
	     loop
	     ((xpath path) (rv '()))		; initialization
	     (if (string-empty? xpath)		; if end of loop with colon
		 (reverse! (cons "." rv))	; return with .
		 (let ((index (string-posq #\: xpath)))
		      (if index		; if a colon exists
			  (if (fx= index 0)
			      (loop (chdr xpath) (cons "." rv))
			      (loop (nthchdr xpath (fx+ index 1))
				    (cons (substring xpath 0 index)
					  rv)))
			  (reverse! (cons xpath rv)))))))) ; return from loop
	  
	  ;; start of searchpath
	  ;; *******************
	  (if (and (char= (char file) #\slash)		; if name starts with /
		   (file-exists? (->filename file)))	; and it exists
	      file					; return it
	      (iterate loop ((xpath (splitpath path)))
		       (cond ((null? xpath) '#f) ; not found
			     (else (let ((xfile	; form full path name
						(string-append (car xpath)
							       "/"
							       file)))
					(if (file-exists? (->filename xfile))
					    xfile
					    (loop (cdr xpath))))))))))

;;; reloc-file contains the full path name of the file containing
;;; all the namelist information for the currently running Tau process.
;;; it is used by make-foreign-procedure and load-unix
;;; (reloc-file) returns the pathname
;;; (set (reloc-file) val) is used to set the name of the Tau binary to "val"
;;; (insert reloc-file v) is used to change the value of reloc-file to "v"
;;; (delete reloc-file nil) is used to delete the current reloc-file
;;; **********************************************************************
(define reloc-file
  (let ((orig "/usr/local/t")		; default
	(x "/usr/local/t"))
       (object (lambda () x)
	       ((insert self v)
		(set x (enforce string? v)))
	       ((delete self v)	; need two args -- hack!
		(ignore v)
		(or (string-equal? x orig)	; if not orig
		    (not (file-exists? x))	; and it exists
		    (file-delete x)))		; then delete it
	       ((setter reloc-file)
		(lambda (val)
			(set orig (enforce string? val)))))))

(define (initialize-local-system)
  (cond ((searchpath (unix-getenv (copy-string "PATH")) 
                     (car (command-line)))
       => (lambda (tau)
		  (set (reloc-file) tau)	; set orig value of reloc-file
		  (insert reloc-file tau)	; set current value
		  (insert exit-agenda	; to remove reloc files on exit
			  (lambda () (delete reloc-file nil)))))
  (else (format (error-output)
		"Could not find full path name for ~a~%"
		(car (command-line))))))

;;; This is the function that will be called by a user to load in a compiled
;;; Unix file.
;;; returns #t if OK else #f
;;; **********************************************************************
(define (load-foreign filespec . xoptstrings)
  (labels (
	   
	   ;; makes a unique file name based on the process id
	   ;; ******************************
	   (mktemp
	    (let ((pid (unix-getpid)))
		 (lambda (template)
			 (string-append
			  template
			  (symbol->string (generate-symbol pid))))))

	   ;; prints the load message
	   ;; ******************************
	   ((print-load-message name)
	    (cond ((print-load-message?)
		   (let ((out (standard-output)))
			(comment-indent out (fx* *load-level* 2))
			(format out "Loading ~a~%" name)
			(no-value)))))

	   ) ; end of labels
	  
	  ;; the beginning of load-unix
	  ;; ******************************
	  (let ((pathname (filename->string
			   (check-arg file-exists?
				      (->filename filespec)
				      load-unix)))
		(libstring (if xoptstrings
			       (check-arg string? (car xoptstrings) load-unix)
			       ""))
		(otherstring (if (and xoptstrings (cdr xoptstrings))
				 (check-arg string? (cadr xoptstrings) load-unix)
				 ""))
		(tmpfile (check-arg (complement file-exists?)
				    (mktemp "/tmp/dyn")
				    load-unix)))
	       (print-load-message pathname)
	       (let ((retCode (unix-load-help pathname
					      (check-arg file-exists?
							 (reloc-file)
							 load-unix)
					      tmpfile
					      libstring
					      otherstring)))
		    (cond ((fxn= retCode 0)
			   (format (error-output)
				   (case retCode
					 ((1) "no space for ld command~%")
					 ((2) "sbrk(0) failed~%")
					 ((3) "could not bump to pagesize~%")
					 ((4) "not page aligned~%")
					 ((5) "ld command too long~%")
					 ((6) "ld command failed~%")
					 ((7) "could not open tmpfile~%")
					 ((8) "cant read header of tmpfile~%")
					 ((9) "bad magic number in tmpfile~%")
					 ((10) "not enough memory~%")
					 ((11) "loadpoint shifted~%")
					 ((12) "memory not page aligned~%")
					 ((13) "fseek fails~%")
					 ((14) "fread fails~%")
					 (else
					  (format nil
						  "no such return value:~d~~%"
						  retCode))))
			   '#f)
			  (else (delete reloc-file nil)	; delete old file
				(insert reloc-file tmpfile) ; set new file
				'#t))))))

;;; end of dynload.t
