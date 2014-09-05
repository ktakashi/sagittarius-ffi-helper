#ifndef SAMPLE__H
#define SAMPLE__H

#define CALL(a, b) test_func(a,b)
#define CONST_VAL  1

extern int test_func(int a, int b);

/*********************************************
 comment
**********************************************/

#undef UNDEFINED

#if defined HOGE
# error "sigh***"
#endif


#endif
