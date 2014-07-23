10 rem *** program loader for Xlife-8 for CPC package
20 rem *** by litwr, 2014, (C) GNU GPL

30 mode 0:i=3
35 cls:locate#0,1,8
40 print tab(i)"1. Xlife"
50 print tab(i)"2. Manpage"
60 print tab(i)"3. Notepad+4"
70 print tab(i)"4. Reset colors"
75 print tab(i)"5. Exit"
80 c$=inkey$:l=val(c$):if l<1 or l>5 goto 80

100 on l goto 200,300,400,500,700

200 run"xlife1"

300 mode 2:openin "manpage.txt"
310 line input#9,c$:if len(c$)>79 then print c$; else print c$
315 c$=inkey$:if c$<>"" then gosub 600:goto 315
320 if not eof goto 310
330 closein
340 c$=inkey$:if c$="" then 340 else goto 30

400 run"notepad4"

500 |era,"colors.cfg"
510 goto 35

600 c$=inkey$:if c$<>"" then goto 600
610 c$=inkey$:if c$="" then goto 610
620 c$=inkey$:if c$<>"" then goto 620
630 return

700 mode 1

