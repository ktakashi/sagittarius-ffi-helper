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
#define bar 1
#if bar
#undef bar
# /* hoge */ define FOO 0x777
# define bar(a , b) \\
    do {            \\
      int a, b;     \\
    } while (0)
# ifdef FOO
int foo;
# endif
# ifndef FOO
int bar;
# endif
# line 1 \"hoge\"
#else
# error \"ng\"
#endif
#include <stdio.h>
#include <sample.h>

extern char *message;

int foo[Bar];

// from sample
int main()
{
  bar(i,j);
  bar(k,
      l);

  CALL(a, CONST_VAL);
  return CONST_VAL;
}

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
      (*includes* '("test_files"))
      (pp (parser (open-string-input-port sample-c) (current-output-port)))
      ;;(for-each (cut do-parse <>) files)
      )))
