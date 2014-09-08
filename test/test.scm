(add-load-path "src")
(import (rnrs) 
	(sagittarius)
	(sagittarius control)
	(parser)
	(util file)
	(srfi :14)
	(srfi :26)
	(pp))

(define sample-c
"
//int Ack(int M, int N) { return(M ? (Ack(M-1,N ? Ack(M,(N-1)) : 1)) : N+1); }
//int main(int array, ...);
int main(int argc, char **argv) {
int i;
for (int i = 0;; i++) puts(\"hoge\");

}
")

(define (main args)
  (let ((files (find-files "test_files" :pattern ".c$"))
	(parser (make-parser 'c))
	(table (make-eq-hashtable)))
    (define (do-parse file) 
      (display file) (display '...)
      (call-with-input-file file
	(lambda (p)
	  (let loop ((e (peek-char p)) (r '()))
	    (if (eof-object? e)
		;; must be done in one go!
		(unless (null? (cdr r))
		  (error 'parsing "failed" r))
		(let ((v (parser p table)))
		  (loop (peek-char p) (cons v r))))) (display 'done)))
      (newline))
      ;;(pp (parser (open-string-input-port sample-c)))
      ;;(for-each (cut do-parse <>) files)
    ;;(do-parse "test_files/sample.c")
    (hashtable-set! table 'HWND 'void*)
    (do-parse "sqlcli1.h")
    ))
