//random generator check
#include<stdio.h>
int Phi(int n, int k) {
   if (k == 1 || k == n) return 1;
   return Phi(n - 1, k - 1) + k*Phi(n - 1, k);
}
long q(int n, int m, int k) {
   int f = 1, i;
   for (i = n - k + 1; i <= n; i++) f *= i;
   return f*Phi(m, k);
}
main(int argc, char *argv[]) {
   int Q, k;
   long sum;
   double rr;
   for (Q = 1; Q <=10; Q++) {
      sum = 0;
      for (k = 1; k <= Q; k++) sum += k*q(8, Q, k);
      rr = 12.5*sum/(1L << 3*Q);
      printf("%d %.3f  %d\n", Q, rr, (int)(rr*64*14*16/100));
   }
 }