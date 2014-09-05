;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; parser/c.scm - C parser
;;;  
;;;   Copyright (c) 2013  Takashi Kato  <ktakashi@ymail.com>
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

;; References:
;;   http://www.lysator.liu.se/c/ANSI-C-grammar-y.html
;;   http://www.lysator.liu.se/c/ANSI-C-grammar-l.html
(library (parser c)
    (export make-parser
	    make-condition-parser
	    *typedefs*)
    (import (rnrs)
	    (sagittarius)
	    (sagittarius control)
	    (match)
	    (packrat)
	    (srfi :14 char-sets)
	    (srfi :39 parameters)
	    (pp))

  ;; operator table
  (define-constant token1 '((#\; semicolon) (#\? quest)
			    (#\, comma) (#\: colon)
			    (#\( left-paren)   (#\) right-paren)
			    (#\[ left-bracket) (#\] right-bracket)
			    (#\{ left-brace)   (#\} right-brace)
			    
			    (#\= assign)
			    (#\. dot) (#\& addrof) (#\! bang)
			    (#\~ tilde) (#\- minus) (#\+ plus)
			    (#\* star) (#\/ slash) (#\% percent)
			    (#\< lt-op) (#\> gt-op) (#\^ xor)
			    (#\| or)
			    (#\# hash)))

  (define-constant token2 '((assign (#\= eq-op))
			    (colon (#\: #\]))
			    (dot    :dot) ;; special treat
			    (addrof (#\= and-assign) (#\& and-op))
			    (bang (#\= ne-op))
			    (minus (#\= sub-assign) (#\> ptr-op) (#\- dec-op))
			    (plus (#\= add-assign) (#\+ inc-op))
			    (star (#\= mul-assign))
			    (slash (#\= div-assign))
			    (percent (#\= mod-assign) (#\> #\}))
			    (lt-op (#\< left-op)  (#\= le-op)
				   (#\% #\{) (#\: #\]))
			    (gt-op (#\> right-op) (#\= ge-op))
			    (xor   (#\= xor-assign))
			    (or (#\= or-assign) (#\| or-op))
			    (hash (#\# hash-hash))))

  (define-constant token3 '((left-op (#\= left-assign)) 
			    (right-op (#\= right-assign))))

  (define-constant keywords '(auto break case char const continue default do
			      double else enum extern float for goto if inline 
			      int long register restrict return short signed
			      sizeof static struct switch typedef union
			      unsigned void volatile while _Bool _Complex
			      _Imaginary))

  (define *typedefs* (make-parameter '()))

  ;; TODO generator looses position information
  ;; when handling comment.
  ;; FIXME it's ugly now...
  (define (generator p)
    (let ((ateof #f)
	  (pos (top-parse-position (or (car (port-info p)) "<?>"))))
      (lambda ()
	(define (lookup-keyword id default)
	  (cond ((memv id keywords) => car)
		(else default)))
 
	(define (read-literal p delm)
	  (let loop ((r '()) (c (read-char p)))
	    (cond ((eof-object? c)
		   (error 'read-string "unexpected EOF object"
			  (list->string (reverse! r))))
		  ((char=? c #\\) 
		   (let ((c2 (read-char p)))
		     (loop (cons c2 r) (read-char p))))
		  ((char=? c delm) (list->string (reverse! r)))
		  (else (loop (cons c r) (read-char p))))))
	(define (read-string p) (read-literal p #\"))
	(define (read-constant p) (read-literal p #\'))
	(define (resolve-operator token x p)
	  (let ((alist (assq (cadr token) token2)))
	    (cond ((and (memq :dot alist) (char=? x #\.))
		   (read-char p)
		   (let ((y (read-char p)))
		     (if (char=? y #\.)
			 (values '... '...)
			 (error 'resolve-operator 
				(format "invalid lexical syntax ..~s" y)))))
		  ((assv x alist)
		   => (lambda (token2)
			(read-char p)
			(or (and-let* ((alist (assq (cadr token2) token3))
				       (y (peek-char p))
				       (slot (assv y alist)))
			      (read-char p)
			      (values (string->symbol (format "~a~a~a" 
							      (car token) x y))
				      (cadr slot)))
			    (values (string->symbol 
				     (format "~a~a" (car token) x))
				    (cadr token2)))))
		  ;; one letter operator must have the same name and kind
		  (else (values (car token) (car token))))))
	(define (read-number first second p)
	  (define (read-int acc p set radix)
	    (read-char p)			; discards the second letter
	    (let loop ((acc acc) (c (peek-char p)))
	      (if (char-set-contains? set c)
		  (loop (cons c acc) (begin (read-char p) (peek-char p)))
		  (string->number (list->string (reverse! acc)) radix))))
	  (define IS (string->char-set "uUlL"))
	  (define FS (string->char-set "fFlL"))
	  (define (read-is p int)
	    (let loop ((c (peek-char p)))
	      (if (char-set-contains? IS c)
		  (loop (begin (read-char p) (peek-char p)))
		  int)))
	  (define (read-hex p)
	    (let ((int (read-int '() p char-set:hex-digit 16)))
	      (read-is p int)))

	  (define (read-octet first p)
	    (let ((int (read-int (list first) p char-set:digit 8)))
	      (read-is p int)))
	  (define (read-float p)
	    ;; for now
	    ;; FIXME this is absolutely wrong
	    (let ((int (read-int '() p char-set:digit 10)))
	      (string->number (string-append "." (number->string int)) 10)))

	  (cond ((char=? first #\0)
		 (cond ((char=? second #\x) (read-hex p))
		       ((char-set-contains? char-set:digit second)
			(read-octet second p))
		       ;; one letter
		       (else (string->number (string first) 10))))
		((char-set-contains? char-set:digit second)
		 (let ((int (read-int (list second first) p char-set:digit 10))
		       (c (peek-char p)))
		   (cond ((char=? c #\.)
			  (let ((f (read-float p)))
			    (+ int f)))
			 ((char-set-contains? IS c)
			  (read-is p int))
			 (else int))))
		(else (string->number (string first) 10))))
	(define (position-update token kind old-pos)
	    (set! pos (update-parse-position pos token))
	    (values old-pos (cons kind token)))
	(let loop ()
	  (if ateof
	      (values pos #f)
	      (let ((x (read-char p))
		    (y (peek-char p)))
		(cond ((eof-object? x)
		       (set! ateof #t)
		       (values pos #f))
		      ((eof-object? y)
		       ;; probably error
		       (position-update x x pos))
		      ((char=? x y #\/)
		       ;; one line comment skip
		       (get-line p)
		       (loop))
		      ((char-set-contains? char-set:digit x)
		       ;; read number
		       (let ((n (read-number x y p)))
			 (position-update n 'constant pos)))
		      ((and (char=? x #\/) (char=? y #\*))
		       (read-char p)
		       ;; skip until it hits */
		       (let comment ()
			 (let ((x1 (read-char p))
			       (y1 (peek-char p)))
			   (cond ((or (eof-object? x1) (eof-object? y1))
				  (loop))
				 ((and (char=? x1 #\*) (char=? y1 #\/))
				  (read-char p)
				  (let ((x (read-char p)))
				    (position-update x x pos)
				    (loop)))
				 (else (comment))))))
		      ((char-whitespace? x) (loop))
		      ((or (char=? x #\")
			   (and (char=? x #\L) (char=? y #\")))
		       (when (char=? x #\L) (read-char p))
		       (let ((s (read-string p)))
			 (position-update s 'string-literal pos)))
		      ((or (char=? x #\')
			   (and (char=? x #\L) (char=? y #\')))
		       (when (char=? x #\L) (read-char p))
		       (let ((s (read-constant p)))
			 (position-update s 'constant pos)))
		      ((assv x token1)
		       => (lambda (token)
			    (if (or (char-whitespace? y)
				    (not (assq (cadr token) token2))
				    (char-set-contains? identifier-set y))
				(position-update x x pos)
				(let-values (((token name) 
					      (resolve-operator token y p)))
				  (position-update token name pos)))))
		      (else
		       (let read-id ((acc (list x)) (ch y))
			 (if (char-set-contains? identifier-set ch)
			     (read-id (cons ch acc) 
				      (begin (read-char p) (peek-char p)))
			     (let ((id (string->symbol
					(list->string (reverse! acc)))))
			       (position-update id
						(lookup-keyword id 'identifier)
						pos))))))))))))

  (define identifier-set
    (char-set-union char-set:letter+digit (list->char-set '(#\_))))

  (define condition-parser #f)

  (define parser
    (packrat-parser
     (begin
       (define (typedef-name results)
	 (let ((v (parse-results-token-value results)))
	   (if (memq v (*typedefs*))
	       (make-result v (parse-results-next results))
	       (make-expected-result (parse-results-position results)
				     'typedef-name))))
       (set! condition-parser conditional-expression)
       translation-unit)

     (primary-expression ((id <- 'identifier) id)
			 ((c  <- 'constant) c)
			 ((s  <- 'string-literal) s)
			 (('#\( e <- expression '#\)) e))

     (postfix-expression ((e <- primary-expression e* <- postfix-expression*)
			  (if (null? e*) e (cons e e*))))
     (postfix-expression* 
      (('#\[ e <- expression '#\] e* <- postfix-expression*) (cons e e*))
      (('#\( '#\) e* <- postfix-expression*) e*)
      (('#\( e <- argument-expression-list '#\) e* <- postfix-expression*)
       (cons e e*))
      (('#\. id <- 'identifier e* <- postfix-expression*) (cons id e*))
      ((o <- 'inc-op e* <- postfix-expression*) (cons o e*))
      ((o <- 'dec-op e* <- postfix-expression*) (cons o e*))
      (('ptr-op id <- 'identifier e* <- postfix-expression*) (cons id e*))
      ;; do we need this?
      (('#\( t <- type-name '#\) '#\{ i* <- initializer-list '#\})
       (cons t i*))
      (('#\( t <- type-name '#\) '#\{ i* <- initializer-list '#\, '#\})
       (cons t i*))
      (() '()))

     (argument-expression-list
      ((e <- assignment-expression e* <- argument-expression-list*)
       (if (null? e*) e (list e e*))))
     (argument-expression-list* (('#\, e <- argument-expression-list) e)
				(() '()))

     (unary-expression ((e <- postfix-expression) e)
		       ((o <- 'inc-op e <- unary-expression) (cons o e))
		       ((o <- 'dec-op e <- unary-expression) (cons o e))
		       ((o <- 'sizeof '#\( e <- type-name '#\)) (cons o e))
		       ((o <- 'sizeof e <- unary-expression) (cons o e))
		       ((o <- unary-operator e <- cast-expression) (cons o e)))

     (unary-operator (('#\&) '&)
		     (('#\*) '*)
		     (('#\+) '+)
		     (('#\-) '-)
		     (('#\~) '~)
		     (('#\!) '!))

     (cast-expression
      (('#\( t <- type-name '#\) e <- cast-expression) (cons t e))
      ((e <- unary-expression) e))

     (multiplicative-expression
      ((e <- cast-expression e* <- multiplicative-expression*) 
       (if (null? e*) e (list e e*))))
     (multiplicative-expression*
      (('#\* e <- multiplicative-expression) (cons '* e))
      (('#\/ e <- multiplicative-expression) (cons '/ e))
      (('#\% e <- multiplicative-expression) (cons '% e))
      (() '()))

     (additive-expression 
      ((e <- multiplicative-expression e* <- additive-expression*)
       (if (null? e*) e (cons e e*))))
     (additive-expression* (('#\+ e <- additive-expression) (list '+ e))
			   (('#\- e <- additive-expression) (list '- e))
			   (() '()))

     (shift-expression ((e <- additive-expression e* <- shift-expression*)
			(if (null? e*) e (cons e e*))))
     (shift-expression* ((o <- 'left-op e <- shift-expression) (list o e))
			((o <- 'right-op e <- shift-expression) (list o e))
			(() '()))

     (relational-expression
      ((e <- shift-expression e* <- relational-expression*)
       (if (null? e*) e (cons e e*))))
     (relational-expression* 
      (('#\< e <- relational-expression) (list '< e))
      (('#\> e <- relational-expression) (list '> e))
      ((o <- 'le-op e <- relational-expression) (list o e))
      ((o <- 'ge-op e <- relational-expression) (list o e))
      (() '()))

     (equality-expression
      ((e <- relational-expression e* <- equality-expression*)
       (if (null? e*) e (cons e e*))))
     (equality-expression* ((o <- 'eq-op e <- equality-expression) (list o e))
			   ((o <- 'ne-op e <- equality-expression) (list o e))
			   (() '()))

     (and-expression ((e <- equality-expression e* <- and-expression*)
		      (if (null? e*) e (cons e e*))))
     (and-expression* (('#\& e <- and-expression) (list '& e))
		      (() '()))

     (exclusive-or-expression
      ((e <- and-expression e* <- exclusive-or-expression*)
       (if (null? e*) e (cons e e*))))
     (exclusive-or-expression* (('#\^ e <- exclusive-or-expression) (list '^ e))
			       (() '()))

     (inclusive-or-expression
      ((e <- exclusive-or-expression e* <- inclusive-or-expression*) 
       (if (null? e*) e (cons e e*))))
     (inclusive-or-expression* 
      (('#\| e <- inclusive-or-expression) (list '\x7c; e
						 ))
      (() '()))

     (logical-and-expression
      ((e <- inclusive-or-expression e* <- logical-and-expression*)
       (if (null? e*) e (cons e e*))))
     (logical-and-expression* 
      ((o <- 'and-op e <- logical-and-expression) (list o e))
      (() '()))

     (logical-or-expression
      ((e <- logical-and-expression e* <- logical-or-expression*) 
       (if (null? e*) e (cons e e*))))
     (logical-or-expression* 
      ((o <- 'or-op e <- logical-or-expression) (list o e))
      (() '()))

     (conditional-expression ((e1 <- logical-or-expression
			       '#\? e2 <- expression
			       '#\: e3 <- conditional-expression)
			      (list e1 e2 e3))
			     ((e <- logical-or-expression) e))

     (assignment-expression ((ue <- unary-expression
			      ao <- assignment-operator
			      ae <- assignment-expression) 
			     (list ue ao ae))
			    ((ce <- conditional-expression) ce))

     (assignment-operator (('#\=) '=)
			  ((o <- 'mul-assign) o)
			  ((o <- 'div-assign) o)
			  ((o <- 'mod-assign) o)
			  ((o <- 'add-assign) o)
			  ((o <- 'sub-assign) o)
			  ((o <- 'left-assign) o)
			  ((o <- 'right-assign) o)
			  ((o <- 'and-assign) o)
			  ((o <- 'xor-assign) o)
			  ((o <- 'or-assign) o))

     (expression ((e <- assignment-expression e* <- expression*)
		  (if (null? e*)
		      (list :expression e)
		      (list :expression e e*))))
     (expression* (('#\, e <- assignment-expression) e)
		  (() '()))

     (constant-expression ((e <- conditional-expression) e))

     (declaration
      ((ds <- declaration-specifiers init <- init-declarator-list '#\;)
       ;; store typedeffed names so that it can continue.
       (when (and (pair? ds) (eq? (car ds) 'typedef))
	 (match init
	   ((((pointer? name) . rest))
	    (*typedefs* (cons name (*typedefs*))))
	   ;; FIXME
	   (_ (display "unknown format: " (current-error-port))
	      (display init (current-error-port)) 
	      (newline (current-error-port)))))
       (list ds init))
      ((ds <- declaration-specifiers '#\;) (list ds #f)))

     (declaration-specifiers 
      ((st <- storage-class-specifier ds <- declaration-specifiers)
       (cons st ds))
      ((t <- type-specifier ds <- declaration-specifiers) (cons t ds))
      ((t <- type-qualifier ds <- declaration-specifiers) (cons t ds))
      ((fs <- function-specifier ds <- declaration-specifiers) (cons fs ds))
      (() '()))
     
     (init-declarator-list ((i <- init-declarator i* <- init-declarator-list*)
			    (cons i i*)))
     (init-declarator-list*
      (('#\, i <- init-declarator i* <- init-declarator-list*) (cons i i*))
      (() '()))

     (init-declarator ((d <- declarator '#\= i <- initializer) (cons d i))
		      ((d <- declarator) (cons d #f)))

     (storage-class-specifier ((t <- 'typedef) t)
			      ((e <- 'extern) e)
			      ((s <- 'static) s)
			      ((a <- 'auto) a)
			      ((r <- 'register) r))

     (type-specifier ((t <- 'void) t)
		     ((t <- 'char) t)
		     ((t <- 'short) t)
		     ((t <- 'int) t)
		     ((t <- 'long) t)
		     ((t <- 'float) t)
		     ((t <- 'double) t)
		     ((t <- 'signed) t)
		     ((t <- 'unsigned) t)
		     ((t <- '_Bool) t)
		     ((t <- '_Complex) t)
		     ((st <- struct-or-union-specifier) st)
		     ((e <- enum-specifier) e)
		     ;; TODO TYPE_NAME for typedef or so
		     ((tn <- typedef-name) tn)
		     )

     (struct-or-union-specifier ((st <- struct-or-union
				  id <- 'identifier '#\{ 
				  decl <- struct-declaration-list
				  '#\}) (list st id decl))
				((st <- struct-or-union
				  '#\{
				  decl <- struct-declaration-list
				  '#\}) (list st #f decl))
				((st <- struct-or-union id <- 'identifier)
				 (list st #f '())))
     (struct-or-union ((s <- 'struct) s)
		      ((u <- 'union)  u))

     (struct-declaration-list
      ((d <- struct-declaration d* <- struct-declaration-list) (cons d d*))
      (() '()))

     (struct-declaration
      ((q <- specifier-qualifier-list decl <- struct-declarator-list '#\;)
       (cons q decl)))

     (specifier-qualifier-list
      ((ts <- type-specifier sql <- specifier-qualifier-list) (cons ts sql))
      ((tq <- type-qualifier sql <- specifier-qualifier-list) (cons tq sql))
      (() '()))
     
     (struct-declarator-list
      ((s <- struct-declarator s* <- struct-declarator-list*) (cons s s*)))
     (struct-declarator-list* (('#\, s <- struct-declarator) s)
			      (() '()))

     (struct-declarator
      ((d <- declarator) d)
      (('#\: e <- constant-expression) e)
      ((d <- declarator '#\: e <- constant-expression) (cons d e)))

     (enum-specifier
      (('enum '#\{ e* <- enumerator-list '#\}) (list 'enum #f e*))
      (('enum id <- 'identifier '#\{ e* <- enumerator-list '#\})
       (list 'enum id e*))
      (('enum id <- 'identifier) (list 'enum id '())))

     (enumerator-list ((e <- enumerator e* <- enumerator-list*) (cons e e*)))
     (enumerator-list* (('#\, e <- enumerator) e)
		       (() '()))

     (enumerator 
      ((id <- 'identifier '#\= e <- constant-expression) (cons id e))
      ((id <- 'identifier) id))

     (type-qualifier ((c <- 'const) c)
		     ((r <- 'restrict) r)
		     ((v <- 'volatile) v))

     (function-specifier ((i <- 'inline) i))

     (declarator ((p <- pointer dd <- direct-declarator) (cons p dd))
		 ((dd <- direct-declarator) (cons #f dd)))

     (direct-declarator ((id <- 'identifier d* <- direct-declarator*) 
			 (cons id d*))
			(('#\( d <- declarator '#\) d* <- direct-declarator*)
			 (cons d d*)))
     (direct-declarator* 
      (('#\[ t <- type-qualifier-list e <- assignment-expression '#\])
       (list t e))
      ;; static type
      (('#\[ 'static t <- type-qualifier-list e <- assignment-expression '#\])
       (list 'static t e))
      (('#\[ t <- type-qualifier-list 'static e <- assignment-expression '#\])
       (list t 'static e))
      ;; ??
      (('#\[ t <- type-qualifier-list '#\* '#\]) (list t '*))
      ;; since C99 direct declarator doesn't have to be
      ;; constant 
      ;;(('#\[ c <- constant-expression '#\]) c)
      (('#\[ '#\]) '())
      (('#\( p <- parameter-type-list '#\)) p)
      (('#\( i <- identifier-list '#\)) i)
      (('#\( '#\)) '())
      (() '()))

     (pointer (('#\* tq* <- type-qualifier-list p <- pointer)
	       (if (null? tq*)
		   (cons 'pointer p)
		   (cons* 'pointer tq* p)))
	      (('#\* tq* <- type-qualifier-list) (cons 'pointer tq*))
	      )

     ;; TODO correct?
     (type-qualifier-list ((t <- type-qualifier t* <- type-qualifier-list)
			   (if (null? t*) t (cons t t*)))
			  (() '()))

     (parameter-type-list ((ps <- parameter-list '#\, e <- '...) (cons ps e))
			  ((ps <- parameter-list) ps))
     (parameter-list ((p <- parameter-declaration p* <- parameter-list*)
		      (cons p p*)))
     (parameter-list* (('#\, p <- parameter-list) p)
		      (() '()))
     
     (parameter-declaration
      ((ds <- declaration-specifiers d <- declarator) (cons ds d))
      ((ds <- declaration-specifiers ad <- abstract-declarator) (cons ds ad))
      ((ds <- declaration-specifiers) (list ds)))
     
     (identifier-list ((id <- 'identifier id* <- identifier-list*)
		       (if (null? id*) id (cons id id*))))
     (identifier-list* (('#\, id <- 'identifier) id)
		       (() '()))

     (type-name
      ((s <- specifier-qualifier-list a <- abstract-declarator) (cons s a))
      ((s <- specifier-qualifier-list) s))

     (abstract-declarator
      ((p <- pointer d <- direct-abstract-declarator) (cons p d))
      ((p <- pointer) p)
      #;((d <- direct-abstract-declarator) d)
      )

     ;; TODO correct?
     (direct-abstract-declarator 
      (('#\( a <- abstract-declarator '#\)) a)
      (('#\[ '#\] d <- direct-abstract-declarator) d)
      (('#\[ t <- type-qualifier-list e <- assignment-expression '#\]
	d <- direct-abstract-declarator)
       (list (cons t e) d))
      ;; static
      (('#\[ 'static t <- type-qualifier-list e <- assignment-expression '#\]
	d <- direct-abstract-declarator)
       (list (cons* 'static t e) d))
      (('#\[ t <- type-qualifier-list 'static e <- assignment-expression '#\]
	d <- direct-abstract-declarator)
       (list (cons* t 'static e) d))
      (('#\[ '#\* '#\] d <- direct-abstract-declarator)
       (list (list '*) d))
      #;(('#\[ e <- constant-expression '#\] d <- direct-abstract-declarator)
       (cons e d))
      (('#\( '#\) d <- direct-abstract-declarator) d)
      (('#\( p <- parameter-type-list '#\) d <- direct-abstract-declarator)
       (list p d))
      (() '()))

     (initializer ((e <- assignment-expression) e)
		  (('#\{ l <- initializer-list '#\}) l)
		  (('#\{ l <- initializer-list '#\, '#\}) l))

     (initializer-list
      ((d <- designation i <- initializer i* <- initializer-list*) 
       (cons (cons d i) i*))
      ((i <- initializer i* <- initializer-list*) (cons i i*)))
     (initializer-list* 
      (('#\, d <- designation i <- initializer) (cons d i))
      (('#\, i <- initializer) i)
      (() '()))

     (designation ((dl <- designation-list '#\=) dl))
     (designation-list ((d <- designator dl <- designation-list '#\=)
			(cons d dl))
		       (() '()))

     (designator (('#\[ e <- constant-expression '#\]) e)
		 (('#\. id <- 'identifier) id))

     (statement ((s <- labeled-statement) s)
		((s <- compound-statement) s)
		((s <- expression-statement) s)
		((s <- selection-statement) s)
		((s <- iteration-statement) s)
		((s <- jump-statement) s))

     (labeled-statement 
      ((id <- 'identifier '#\: s <- statement) (list id s))
      ((c <- 'case e <- constant-expression '#\: s <- statement) (list c e s))
      ((d <- 'default '#\: s <- statement) (list d s)))

     (compound-statement (('#\{ bi* <- block-item-list '#\}) bi*))
     
     (block-item-list ((bi <- block-item bi* <- block-item-list) (cons bi bi*))
		      (() '()))

     (block-item ((d <- declaration) (list :declaration d))
		 ((s <- statement) (list :statement s)))

     (expression-statement ((e <- expression '#\;) e)
			   (('#\;) '()))

     (selection-statement
      ((i <- 'if '#\( e <- expression '#\) s1 <- statement
	else <- 'else s2 <- statement) (list i e s1 else s2))
      ((i <- 'if '#\( e <- expression '#\) s <- statement) (list i e s))
      ((switch <- 'switch '#\( e <- expression '#\) s <- statement)
       (list switch e s)))

     (iteration-statement
      ((w <- 'while '#\( e <- expression '#\) s <- statement) (list w e s))
      ((d <- 'do s <- statement w <- 'while '#\( e <- expression '#\))
       (list d s w e))
      ((f <- 'for 
	'#\( d <- declaration e1 <- %expression* '#\; e2 <- %expression* '#\) 
	s <- statement)
       (list f d e1 e2 s))
      ((f <- 'for
	'#\( e1 <- %expression* '#\;
	     e2 <- %expression* '#\;
	     e3 <- %expression* '#\)
	s <- statement)
       (list f e1 e2 e3 s)))

     ;; helper
     (%expression* ((e <- expression e* <- %expression*) (cons e e*))
		   (() '()))

     (jump-statement ((g <- 'goto id <- 'identifier '#\;) (list g id))
		     ((c <- 'continue '#\;) c)
		     ((b <- 'break '#\;) b)
		     ((r <- 'return '#\;) r)
		     ((r <- 'return e <- expression '#\;) (list r e)))

     (translation-unit ((e <- external-declaration t <- translation-unit*)
			(cons e t)))
     (translation-unit* ((e <- external-declaration t <- translation-unit*)
			 (cons e t))
			(() '()))

     (external-declaration ((fd <- function-definition) fd)
			   ((dc <- declaration) dc))

     ;; should we only use C99 or better support older one?
     (function-definition ((dspec <- declaration-specifiers
			    decl <- declarator 
			    args <- declaration-list ;; old type
			    body <- compound-statement)
			   (list :function dspec decl args body))
			  ;; following is not valid on C99
			  ((decl <- declarator
			    args <- declaration-list ;; old type
			    body <- compound-statement)
			   (list :function #f decl args body)))

     (declaration-list ((d <- declaration d* <- declaration-list) 
			(if (null? d*) d (list d d*)))
		       (() '()))
     ))

  (define (%make-parser parser)
    (define (read-c-file p)
      (let ((result (parser (base-generator->results (generator p)))))
	(if (parse-result-successful? result)
	    (parse-result-semantic-value result)
	    (error 'c-file-parser "C File Parse Error"
		   (let ((e (parse-result-error result)))
		     `((position ,(parse-position->string
				   (parse-error-position e)))
		       (expected ,(parse-error-expected e))
		       (message ,(parse-error-messages e))))))))
    (lambda maybe-port 
      (parameterize ((*typedefs* (*typedefs*)))
	(read-c-file (if (pair? maybe-port)
			 (car maybe-port)
			 (current-input-port))))))

  (define (make-parser) (%make-parser parser))
  (define (make-condition-parser) (%make-parser condition-parser))

)
