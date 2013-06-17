(add-load-path ".")
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
  (let1 files (find-files "test_files" :pattern ".(c|h)$")
    (let1 parser (make-parser 'c)
      (define (do-parse file) 
	(display file) (display '...)
	(call-with-input-file file
	  (lambda (p) (parser p) (display 'done)))
	(newline))
      (pp (parser (open-string-input-port sample-c)))
      (for-each (cut do-parse <>) files)
      )))