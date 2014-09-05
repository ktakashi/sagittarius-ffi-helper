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
(library (preprocess c)
    (export make-preprocessor)
    (import (rnrs)
	    (sagittarius)
	    (srfi :13 strings)
	    (srfi :14 char-sets)
	    (clos user)
	    (text parse)
	    (preprocess parameters))

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

  (define (read-token-string in)
    ;; for now
    (read-identifier in))

  (define-method handle-keyword ((t (eql :define)) in)
    (define (read-next in var param)
      (list (if param :define :var-define) var (read-token-string in)))
    (let* ((var (read-identifier in))
	   (nc (skip-block-comment in)))
      (cond ((eof-object? nc) ;; it's valid just useless
	     (list :var-define var #t))
	    ((char=? nc #\linefeed)
	     (list :var-define var #t))
	    ((char=? nc #\()
	     (read-next in var (read-until in #\))))
	    (else (read-next in var #f)))))

  (define-method handle-keyword ((t (eql :error)) in)
    (let ((msg (get-line in)))
      (error 'preprocess-error msg)))

  (define (read-preprocessor1 in out)
    (define (read-rec sout)
      ;; now we have fun part...
      (let* ((name (read-identifier in)))
	(handle-keyword (string->keyword name) in)))
    (define (handle-comment in)
      (let loop ((c (get-char in)) (nl? #f))
	(cond ((char=? c #\*)
	       (let ((nc (get-char in)))
		 (if (char=? #\/) nl? (loop (get-char in) nl?))))
	      ((char=? c #\linefeed)
	       (loop (get-char in) #t))
	      (else (loop (get-char in) nl?)))))
    (let-values (((sout extract) (open-string-output-port)))
      (let loop ((c (get-char in)) (appear? #f))
	(cond ((eof-object? c)
	       (let ((r (extract)))
		 (if (string-null? r) c r)))
	      ((char=? c #\linefeed) (put-char out c) (extract)) ;; stops at eol
	      ((char-whitespace? c)
	       (put-char out c) (loop (get-char in) appear?))
	      ((char=? c #\#) 
	       (when appear? (error 'c-preprocessor "stray '#' in program"))
	       (read-rec sout))
	      ((char=? c #\/)
	       ;; could be comment
	       (let ((nc (get-char in)))
		 (cond ((char=? nc #\/) (get-line in))
		       ((char=? nc #\*) 
			(when (handle-comment in) 
			  (loop (get-char in) appear?)))
		       (else
			(put-char out c)
			(put-char out nc)
			(loop (get-char c) #t)))))
	      (else (put-char out c) (loop (get-char in) #t))))))


  ;; the crusial point of processing preprocessor is that
  ;; it's line oriented however it can have multiple lines
  ;; with '\' and comment can appear anywhere
  ;; so what we do here is that trying to read one preprocessor
  ;; then dispatch.
  ;; A preprocessor must start '#' before that it can have
  ;; comment or whitespace but if there is other identifier
  ;; then it is an error.
  (define (c-preprocess in out)
    (let loop ()
      (let ((one-pp (read-preprocessor1 in out)))
	(cond ((eof-object? one-pp) one-pp)
	      ((string? one-pp) (put-string out one-pp) (loop))
	      (else 
	       ;; TODO convert and store macro
	       (loop))))))


  (define (make-preprocessor)
    (lambda (in out)
      ;; returns macros
      (c-preprocess in out)))
)