(add-load-path "src")
(import (rnrs) 
	(sagittarius)
	(sagittarius control)
	(preprocess)
	(util file)
	(srfi :14)
	(srfi :26)
	(pp))

(define sample-c
"
# /* hoge */ define FOO 0x777
#define bar(a , b) \\
  do {         \\
  } while (0)
int foo;
// #error \"ok\"
#line 1 \"hoge\"

")

(define (main args)
  (let1 files (find-files "test_files" :pattern ".(c|h)$")
    (let1 parser (make-preprocessor 'c)
      (define (do-parse file) 
	(display file) (display '...)
	(call-with-input-file file
	  (lambda (p) (parser p (current-output-port)) (display 'done)))
	(newline))
      (*current-path* ".")
      (pp (parser (open-string-input-port sample-c) (current-output-port)))
      (for-each (cut do-parse <>) files)
      )))
