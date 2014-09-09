#ifndef INC_DEFS_H
#define INC_DEFS_H

#include "macros.h"

typedef intptr_t   word;
typedef uintptr_t uword;
typedef void    *  ptr;

struct st_type1
{
  int code[NUM_RET_TYPE];
  char *message;
};

union un_type1
{
  double    d;
  uintptr_t u;
};

union UnType2
{
  struct st_type1 st1;
  union un_type1  un1;
};

typedef struct st_type2_rec
{
  union {
    struct st_type1 st1;
    union  un_type1 un1;
  };
  union {
    struct st_type1 st1;
    union  un_type1 un1;
  } anon_u1;
  word n;
} st_type2, *pst_type2;

typedef enum {
  ITEM1,
  ITEM2
} enum1;

enum status {
  STATUS1,
  STATUS2 = 3
};

/* anonymous */
enum {
  ANON1 = 2,
  ANON2
};

SG_CDECL_BEGIN

SG_EXPORT void   function1(void);
SG_EXPORT void * function2(struct st_type1 *st1);
SG_EXPORT int    function3(union  un_type1 *un1);
SG_EXPORT void * function4(       st_type2 *st1);
SG_EXPORT void * function5(       st_type2 *st1, int v, char *cs);
SG_EXPORT int    fprintf(FILE *file, const char * fmt, ...);

SG_CDECL_END

#endif	/* INC_DEFS_H */
