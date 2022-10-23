BEGIN {
   c = sprintf("%c", 24)
   r = "\r"
}
{
    b = ""
    e = $0
    while (p = index(e, "\\")) {
      m = substr(e, p + 1, 1)
      b = b substr(e, 1, p - 1)
      if (m == "x") {
         if (rev) {b = b c; rev = 0}
         b = b c substr(e, p + 2, 1) c
         e = substr(e, p + 3)
      }
      else if (rev) {
         if (m == "g" || m == "b") {
            b = b c
            rev = 0
         }
         e = substr(e, p + 2)
      }
      else {
         if (m == "p" || m == "r") {
            b = b c
            rev = 1
         }
         e = substr(e, p + 2)
      }
   }
   print b e r
}
END {
   printf "%c", 26
}
