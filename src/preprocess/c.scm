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
    (export make-preprocessor
	    *definitions*)
    (import (rename (rnrs) (error rnrs:error))
	    (sagittarius)
	    (sagittarius regex)
	    (sagittarius object)
	    (srfi :13 strings)
	    (srfi :14 char-sets)
	    (srfi :39 parameters)
	    (clos user)
	    (text parse)
	    (util file)
	    (util hashtables)
	    (packrat)
	    (match)
	    (prefix (parser c) c:) ;; sort of awkward
	    (preprocess parameters)
	    (pp))

  (define *macro-table* (make-parameter #f))
  (define *current-file* (make-parameter #f))
  (define *definitions* (make-parameter '()))

  (define (error who msg . irr)
    (apply rnrs:error who 
	   (if (*current-file*)
	       (format "~a: [~a]" msg (*current-file*))
	       msg)
	   irr))

  ;; grammar reference
  ;;  http://msdn.microsoft.com/en-us/library/2scxys89.aspx

  (define (cs-pred cs) (lambda (c) (char-set-contains? cs c)))
  (define *except-linefeed* #[ \t\r])
  ;; 
  (define (skip-block-comment in :optional (skip-white #t))
    (define (next in)
      (let loop ((c (get-char in)))
	(cond ((eof-object? c) (error 'block-comment "unexpected EOF"))
	      ((char=? c #\*)
	       (let ((nc (peek-char in)))
		 (cond ((eof-object? nc) (error 'block-comment "unexpected EOF"))
		       ((char=? nc #\/)
			(get-char in)
			(skip-block-comment in skip-white))
		       (else             (loop (get-char in))))))
	      (else (loop (get-char in))))))
    (when skip-white (skip-while (cs-pred *except-linefeed*) in))
    (let ((c (peek-char in)))
      (cond ((eof-object? c) c)
	    ((char=? c #\/)
	     (let ((nc (peek-next-char in)))
	       (cond ((char=? nc #\/)
		      (error 'block-comment 
			     "line comment is not valid in this context"))
		     ((char=? nc #\*) (get-char in) (next in))
		     (else c))))
	    (else (get-char in)))))
  
  (define *identifier-set* #[a-zA-Z0-9_])
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
    (define (strip-ul token) (string-trim-right token #[ulUL]))
    (let ((token (strip-ul token)))
      (cond ((string-prefix? "0x" token)
	     (string->number (string-copy token 2) 16))
	    ((and (> (string-length token) 1) (char=? #\0 (string-ref token 0)))
	     (string->number (string-copy token 1) 8))
	    (else (string->number token)))))
  (define (maybe-c-number token)
    (if (for-all (cs-pred #[0-9a-fxulUL]) (string->list token))
	(or (parse-c-number token)
	    token)
	token))

  (define-method handle-keyword (t in) (error t "not supported yet"))

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
    (error 'preprocess-error (get-line in)))
  (define-method handle-keyword ((t (eql :line)) in) (get-line in) #f)

  (define (handle-condition expr in)
    (define (read-nested in)
      (let-values (((out extract) (open-string-output-port)))
	(let ((pos (port-info in)))
	  (let loop ((nest 0))
	    (let ((nc (skip-block-comment in #f)))
	      (cond ((eof-object? nc)
		     (error "#if" "unexpected EOF" 
			    `(from ,pos)
			    `(to ,(port-info in))))
		    ((char=? nc #\#)
		     (let ((name (read-identifier in)))
		       (cond ((and (zero? nest)
				   (member name '("elif" "else" "endif")))
			      (values (extract) name))
			     ((string-prefix? "if" name)
			      (put-char out nc)
			      (put-string out name)
			      (loop (+ nest 1)))
			     ((and (not (zero? nest)) (string=? name "endif"))
			      (put-string out "#endif")
			      (loop (- nest 1)))
			     (else
			      (put-char out nc)
			      (put-string out name)
			      (loop nest)))))
		    (else (put-char out nc) (loop nest))))))))
    (define (satisfy-condition? expr)
      (define (check op e1 e2)
	(define (get e)
	  (if (symbol? e)
	      ;; undefined variable is 0
	      (hashtable-ref (*macro-table*) (symbol->string e) 0)
	      e))
	(op (get e1) (get e2)))
      (define (defined? var)
	(hashtable-contains? (*macro-table*) (symbol->string var)))
      ;; 'define BAR' is for some reason allowed and usual
      ;; c parser can't handle it so fixup
      (define (fixup-defined expr)
	(regex-replace-all #/defined\s*([^\(\s]+)/ expr
			   (lambda (m)
			     (string-append "defined(" (m 1) ")"))))
      ;; bit awkward 
      (let ((expr (fixup-defined expr)))
	(if (for-all (cs-pred *identifier-set*) (string->list expr))
	    (and (hashtable-contains? (*macro-table*) expr)
		 (not (eqv? (hashtable-ref (*macro-table*) expr) 0)))
	    (let* ((parser (c:make-condition-parser))
		   (expr   (parser (open-string-input-port expr))))
	      (let loop ((expr expr))
		(match expr
		  ;; not
		  (('! :expression e) (not (loop e)))
		  (('! . e)           (not (loop e)))
		  ((:expression e)    (loop e))
		  (('defined var)     (defined? var))
		  ((e1 '&& e2)        (and (loop e1) (loop e2)))
		  ((e1 '|\|\|| e2)    (or (loop e1) (loop e2)))
		  ((e1 '== e2)        (check =  e1 e2))
		  ((e1 '>= e2)        (check >= e1 e2))
		  ((e1 '<= e2)        (check <= e1 e2))
		  ((e1 '>  e2)        (check >  e1 e2))
		  ((e1 '<  e2)        (check <  e1 e2))
		  (var                (defined? var))))))))
    (define (proceed in)
      (let-values (((s mark) (read-nested in)))
	(let loop ((mark mark))
	  (unless (string=? mark "endif")
	    (let-values (((_ mark) (read-nested in))) (loop mark))))
	(call-with-string-output-port
	 (lambda (out)
	   (c-preprocess (open-string-input-port s) out)))))
    (if (satisfy-condition? expr)
	(proceed in)
	;; basically the same
	(let loop ()
	  (let-values (((_ mark) (read-nested in)))
	    (cond ((string=? mark "elif")
		   (let ((expr (read-token-string in)))
		     (if (satisfy-condition? expr)
			 (proceed in)
			 (loop))))
		  ((string=? mark "else") (proceed in))
		  (else #f))))))

  (define-method handle-keyword ((t (eql :if)) in)
    (handle-condition (read-token-string in) in))
  (define-method handle-keyword ((t (eql :ifdef)) in)
    (handle-condition (string-append "defined(" (read-identifier in) ")") in))
  (define-method handle-keyword ((t (eql :ifndef)) in)
    (handle-condition (string-append "! defined(" (read-identifier in) ")") in))

  (define-method handle-keyword ((t (eql :undef)) in)
    (let ((name (read-identifier in)))
      (hashtable-delete! (*macro-table*) name)
      #f))
  ;; don't care
  (define-method handle-keyword ((t (eql :pragma)) in)
    (get-line in) #f)

  (define-method handle-keyword ((t (eql :include)) in)
    (define (warn file path)
      ((*warning-handler*) (format "~a does not exist on ~a" file path))
      #f)
    (define (finish file)
      (parameterize ((*current-file* file))
	(if (file-exists? file)
	    (let-values (((dir base ext) (decompose-path file)))
	      (parameterize ((*current-path* dir))
		(call-with-input-file file
		  (lambda (in)
		    (call-with-string-output-port
		     (lambda (out)
		       (c-preprocess in out)))))))
	    (warn file (*current-path*)))))
    (let ((c (skip-block-comment in)))
      (cond ((char=? c #\") ;; local file
	     (let* ((name (read-until in #\"))
		    (file (build-path (*current-path*) name)))
	       (finish file)))
	    ((char=? c #\<)
	     (let* ((name (read-until in #\>))
		    (path (find (lambda (p)
				  (let ((file (build-path p name)))
				    (file-exists? file))) (*includes*))))
	       (if path
		   (finish (build-path path name))
		   (warn name (*includes*)))))
	    (else (error 'include "invalid #include directive")))))

  (define (read-preprocessor1 in out)
    (define (read-rec sout)
      ;; now we have fun part...
      (let ((name (read-identifier in)))
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
	((*warning-handler*) (format "multiple definition of ~s" (cadr pp))))
      (case (car pp)
	((:define) 
	 (hashtable-set! (*macro-table*) (cadr pp) (make-macro (cddr pp))))
	((:var-define)
	 (if (caddr pp)
	     (hashtable-set! (*macro-table*) (cadr pp) (cadddr pp))
	     (hashtable-set! (*macro-table*) (cadr pp) #f)))))
    ;; for now it's line oriented...
    
    (let loop ()
      (let ((one-pp (read-preprocessor1 in out)))
	(cond ((not one-pp) (loop)) ;; like #line or so
	      ((eof-object? one-pp))
	      ((string? one-pp) (put-string out one-pp) (loop))
	      (else (add-macro one-pp) (loop))))))


  (define (make-preprocessor)
    (lambda (in out)
      (define (do-preprocess expr)
	(define (r1 key)
	  (regex (string-append "([^a-zA-Z_]*)" key "([^a-zA-Z0-9_]+)")))
	(define (rn key)
	  (regex (string-append "([^a-zA-Z_]*)" key "\\s*\\(([^)]+)\\)")))
	(define (replace-it who v)
	  (lambda (m)
	    (string-append (m 1) (->string v) (m 2))))
	(define (do-replace who v)
	  (lambda (m)
	    (let ((args (string-split (m 2) #/\s*,\s*/))
		  (tmpl (car v)))
	      (unless (= (length args) (length tmpl))
		(error who (format "invalid arguments for ~a" tmpl) args))
	      (let loop ((args args) (tmpl tmpl) (r (cadr v)))
		(if (null? args)
		    (string-append (m 1) r)
		    (loop (cdr args) (cdr tmpl)
			  (regex-replace-all (r1 (car tmpl)) r
					     (replace-it (car tmpl)
							 (car args)))))))))
	(define keys (hashtable-keys-list (*macro-table*)))
	(let loop ((keys keys) (expr expr) (nest (+ (length keys) 1000)))
	  (if (or (null? keys) (= nest 0))
	      expr
	      (let ((v (hashtable-ref (*macro-table*) (car keys))))
		(if (pair? v)
		    (loop (cdr keys)
			  (regex-replace-all (rn (car keys))
					     expr (do-replace (car keys) v))
			  (- nest 1))
		    (loop (cdr keys)
			  (regex-replace-all (r1 (car keys))
					     expr (replace-it (car keys) v))
			  (- nest 1)))))))
      ;; returns macros
      (parameterize ((*macro-table* (make-string-hashtable)))
	(for-each (lambda (kv)
		    (hashtable-set! (*macro-table*) (car kv) (cdr kv)))
		  (*definitions*))
	(let ((s (call-with-string-output-port
		  (lambda (out) (c-preprocess in out)))))
	  (put-string out (do-preprocess s)))
	(hashtable->alist  (*macro-table*)))))
)
