/* -*- mode: c -*-
 * $Id: Ackermann.c,v 1.4 2003/03/05 10:16:18 djowel Exp $
 * http://www.bagley.org/~doug/shootout/
 */

//#include <stdio.h>
//#include <stdlib.h>
//#include <unistd.h>

int Ack(int M, int N) { return(M ? (Ack(M-1,N ? Ack(M,(N-1)) : 1)) : N+1); }

int main(int argc, char *argv[]) {
    int n = ((argc == 2) ? atoi(argv[1]) : 1);

    printf("Ack(3,%d): %d\n", n, Ack(3, n));
    /* sleep long enough so we can measure memory usage */
    sleep(1);
    return(0);
}

