//random generator check
#include<stdlib.h>
#include<stdio.h>
#include<time.h>
#define P 10000000
main(int argc, char *argv[]) {
   int Q;
   for (Q = 1; Q <=10; Q++)
   {
   int n, i, k, t[8], r[Q + 1];
   double rr, qq;
   srand(time(0));
   for (k = 1; k <= Q; k++) r[k] = 0;
   for (i = 0; i < P; i++) {
      for (k = 0; k < 8; k++) t[k] = 0;
      for (n = 0; n < Q; n++)
         t[(long)rand()*8/RAND_MAX]++;
      k = 0;
      for (n = 0; n < 8; n++)
         if (t[n]) k++;
      r[k]++;
   }
//   printf("%d %d\n", Q, 1 << (Q - 1)*3);
   rr = 0;
   for (k = 1; k <= Q; k++)
      rr += (double)r[k]*k/P*12.5;
   qq  = 0;
   for (k = 1; k <= Q; k++) {
      qq += (double)r[k]*100/P;
      //printf("k = %d %d %.3f\n", k, r[k], (double)r[k]*100/P);
   }
   if (qq - 100.0 > 1e-7) printf("error!\n");
   printf("%d %.3f  %d\n", Q, rr, (int)(rr*64*14*16/100));
   }
 }