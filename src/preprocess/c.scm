;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; preprocess/c.scm
;;;  
;;;   Copyright (c) 2014  Takashi Kato  <ktakashi@ymail.com>
;;;   
;;;   Redistribution and use in source and binary forms, with or without
;;;   modification, are permitted provided that the following conditions
;;;   are met:
;;;   
;;;   1. Redistributions of source code must retain the above copyright
;;;      notice, this list of conditions and the following disclaimer.
;;;  
;;;   2. Redistributions in binary form must reproduce the above copyright
;;;      notice, this list of conditions and the following disclaimer in the
;;;      documentation and/or other materials provided with the distribution.
;;;  
;;;   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;;;   "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;;;   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;;;   A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
;;;   OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;;;   SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
;;;   TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
;;;   PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
;;;   LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;;   NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;;   SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
;;;  

;; preprocessor for C

;; strip out preprocessor and comments
;; the result is close to gcc -E option
#!read-macro=char-set
#!read-macro=sagittarius/regex
(library (preprocess c)
    (export make-preprocessor)
    (import (rnrs)
	    (sagittarius)
	    (sagittarius regex)
	    (srfi :13 strings)
	    (srfi :14 char-sets)
	    (srfi :39 parameters)
	    (clos user)
	    (text parse)
	    (util hashtables)
	    (preprocess parameters))

  (define *macro-table* (make-parameter #f))

  ;; grammar reference
  ;;  http://msdn.microsoft.com/en-us/library/2scxys89.aspx

  (define (cs-pred cs) (lambda (c) (char-set-contains? cs c)))
  (define *except-linefeed* #[ \t\r])
  ;; 
  (define (skip-block-comment in)
    (define (next in)
      (let loop ((c (get-char in)))
	(cond ((eof-object? c) (error 'block-comment "unexpected EOF"))
	      ((char=? c #\*)
	       (let ((nc (get-char in)))
		 (cond ((eof-object? nc)
			(error 'block-comment "unexpected EOF"))
		       ((char=? nc #\/) (skip-block-comment in))
		       (else (loop (get-char in))))))
	      (else (loop (get-char in))))))
    (skip-while (cs-pred *except-linefeed*) in)
    (let ((c (peek-char in)))
      (cond ((eof-object? c) c)
	    ((char=? c #\/)
	     (let ((nc (peek-next-char in)))
	       (cond ((char=? nc #\/)
		      (error 'block-comment 
			     "line comment is not valid in this context"))
		     ((char=? nc #\*) (next in))
		     (else c))))
	    (else (get-char in)))))
  
  (define *identifier-set* #[a-zA-Z_])
  (define (read-identifier in)
    (let ((c (skip-block-comment in)))
      (string-append (list->string (list c))
		     (next-token-of (cs-pred *identifier-set*) in))))

  (define (read-until in mark)
    (call-with-string-output-port
     (lambda (out)
       (let loop ()
	 (let ((c (get-char in)))
	   (unless (char=? c mark)
	     (put-char out c)
	     (loop)))))))

  ;; read all character until it hits linefeed
  ;; '\' is an escape.
  ;; gcc actually allows '\   \n' ('\' followed by spaces then linefeed)
  ;; but we don't
  (define (read-token-string in)
    (skip-while (cs-pred *except-linefeed*) in)
    (call-with-string-output-port
     (lambda (out)
       (let loop ()
	 (let ((c (read-char in)))
	   (cond ((eof-object? c) 
		  (error 'read-token-string "unexpected EOF"))
		 ((char=? c #\\)
		  (let ((nc (read-char in)))
		    (cond ((char=? nc #\return)
			   (let ((nc2 (read-char in)))
			     (unless (char=? nc2 #\linefeed)
			       (put-char out nc2))
			     (loop)))
			  ((char=? nc #\linefeed) (loop))
			  (else (put-char out c) (put-char out nc) (loop)))))
		 ((or (char=? c #\return) (char=? c #\linefeed))
		  (when (char=? c #\return) 
		    (let ((nc (peek-char in)))
		      (when (char=? nc #\linefeed) (get-char in)))))
		 (else (put-char out c) (loop))))))))

  (define (parse-c-number token)
    (cond ((string-prefix? "0x" token)
	       (string->number (string-copy token 2) 16))
	      ((char=? #\0 (string-ref token 0))
	       (string->number (string-copy token 1) 8))
	      (else (string->number token))))
  (define (maybe-c-number token)
    (if (for-all (cs-pred #[0-9a-fx]) (string->list token))
	(parse-c-number token)
	token))

  (define-method handle-keyword ((t (eql :define)) in)
    (define (read-next in var param first)
      (list (if (string? param) :define :var-define) 
	    var param 
	    (maybe-c-number
	     (let ((token (read-token-string in)))
	       (if first
		   (string-append (list->string (list first)) token)
		   token)))))
    (let* ((var (read-identifier in))
	   (nc (skip-block-comment in)))
      (cond ((eof-object? nc) ;; it's valid just useless
	     (list :var-define var #f))
	    ((char=? nc #\linefeed)
	     (list :var-define var #f))
	    ((char=? nc #\()
	     (read-next in var (read-until in #\)) #f))
	    (else (read-next in var #t nc)))))

  (define-method handle-keyword ((t (eql :error)) in)
    (let ((msg (get-line in)))
      (error 'preprocess-error msg)))
  (define-method handle-keyword ((t (eql :line)) in) (get-line in) #f)

  (define (read-preprocessor1 in out)
    (define (read-rec sout)
      ;; now we have fun part...
      (let* ((name (read-identifier in)))
	(handle-keyword (string->keyword name) in)))
    (define (handle-comment in)
      (let loop ((c (get-char in)) (nl? #f))
	(cond ((char=? c #\*)
	       (let ((nc (get-char in)))
		 (if (char=? nc #\/) nl? (loop (get-char in) nl?))))
	      ((char=? c #\linefeed)
	       (loop (get-char in) #t))
	      (else (loop (get-char in) nl?)))))
    (let-values (((sout extract) (open-string-output-port)))
      (let loop ((c (get-char in)) (appear? #f))
	(cond ((eof-object? c)
	       (let ((r (extract)))
		 (if (string-null? r) c r)))
	      ((char=? c #\linefeed) (put-char sout c) (extract)) ;; stops at eol
	      ((char-whitespace? c)
	       (put-char sout c) (loop (get-char in) appear?))
	      ((char=? c #\#) 
	       (when appear? (error 'c-preprocessor "stray '#' in program"))
	       (read-rec sout))
	      ((char=? c #\/)
	       ;; could be comment
	       (let ((nc (get-char in)))
		 (cond ((char=? nc #\/) 
			(get-line in) (loop (get-char in) appear?))
		       ((char=? nc #\*) 
			(if (handle-comment in) 
			    (loop (get-char in) appear?)
			    (let ((r (extract)))
			      (if (string-null? r) #f r))))
		       (else
			(put-char sout c)
			(put-char sout nc)
			(loop (get-char in) #t)))))
	      (else (put-char sout c) (loop (get-char in) #t))))))


  ;; the crusial point of processing preprocessor is that
  ;; it's line oriented however it can have multiple lines
  ;; with '\' and comment can appear anywhere
  ;; so what we do here is that trying to read one preprocessor
  ;; then dispatch.
  ;; A preprocessor must start '#' before that it can have
  ;; comment or whitespace but if there is other identifier
  ;; then it is an error.
  (define (c-preprocess in out)
    (define (add-macro pp)
      (define (make-macro param&def) 
	(let ((param (string-split (car param&def) #/\s*,\s*/))
	      (def   (cadr param&def)))
	  (list param def)))
      (when (hashtable-contains? (*macro-table*) (cadr pp))
	;; should we make this an error or just an warning?
	(error 'define "multiple definition" pp))
      (case (car pp)
	((:define) 
	 (hashtable-set! (*macro-table*) (cadr pp) (make-macro (cddr pp))))
	((:var-define)
	 (if (caddr pp)
	     (hashtable-set! (*macro-table*) (cadr pp) (cadddr pp))
	     (hashtable-set! (*macro-table*) (cadr pp) #f)))))
    (let loop ()
      (let ((one-pp (read-preprocessor1 in out)))
	(cond ((not one-pp) (loop)) ;; like #line or so
	      ((eof-object? one-pp) (*macro-table*))
	      ((string? one-pp)
	       ;; TODO preprocess 
	       (put-string out one-pp) (loop))
	      (else 
	       (add-macro one-pp)
	       ;; TODO convert and store macro
	       (loop))))))


  (define (make-preprocessor)
    (lambda (in out)
      ;; returns macros
      (parameterize ((*macro-table* (make-string-hashtable)))
	(hashtable->alist (c-preprocess in out)))))
)
