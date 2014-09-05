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
# /* hoge */ define FOO
int foo;
#error \"ok\"
")

(define (main args)
  (let1 files (find-files "test_files" :pattern ".(c|h)$")
    (let1 parser (make-preprocessor 'c)
      (define (do-parse file) 
	(display file) (display '...)
	(call-with-input-file file
	  (lambda (p) (parser p) (display 'done)))
	(newline))
      (*current-path* ".")
      (pp (parser (open-string-input-port sample-c) (current-output-port)))
      ;; (for-each (cut do-parse <>) files)
      )))
