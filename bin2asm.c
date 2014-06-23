#include<stdio.h>
main(){
   int c, i = 0, h, ts=69, t, tiles = 0x19db;
//   getchar();
//   getchar();
   for(;;){
      if ((c = getchar()) == EOF)
         break;
      if (i%16 == 0) {
         if (i) printf("\n");
         printf ("          .byte ");
      }
      else
         printf(",");
      t = i%ts;
      if (t >= 8 && t < 24) {
         h = getchar();
         c += 256*h - tiles;
         if (c > 0)
             printf("<($%x+tiles),>($%x+tiles)", c, c);
         else if (c < 0)
             printf("0,0");
         else
             printf("<(tiles),>(tiles)");
         i++;
      }
      else
         if (c/16)
            printf ("$%x%x", c/16, c%16);
         else if (c%16 < 10)
            printf ("%x", c%16);
         else
            printf ("$%x",  c%16);
      i++;
   }
   printf("\n");
}
