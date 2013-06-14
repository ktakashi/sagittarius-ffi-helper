/* -*- mode: c -*-
 * $Id: SumCol.c,v 1.4 2003/03/05 10:16:18 djowel Exp $
 * http://www.bagley.org/~doug/shootout/
 */

/*
#include <stdio.h>
#include <stdlib.h>

#define MAXLINELEN 128
*/

int
main() {
    int sum = 0;
    char line[128];

    while (fgets(line, MAXLINELEN, stdin)) {
    sum += atoi(line);
    }
    printf("%d\n", sum);
    return(0);
}

