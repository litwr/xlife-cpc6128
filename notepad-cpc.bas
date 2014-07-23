 1 rem *** koi8-r encoding - it is only for litwr's cross-development environment
 2 rem *** notepad+4 cpc edition, the text file editor, v1 rev.3
 4 rem *** converted from Commodore plus/4
 6 rem *** by litwr, 2014, (C) GNU GPL, thanks to SyX
 7 rem *** the initial banner was made by Text Resizer by MIRKOSOFT
 8 defint a-z:cl=72:openout "dummy.zzz":shm=unt(himem):cs1=shm-cl+1:MEMORY cs1-1:closeout
 9 cs2=cs1+28:cs3=cs2+28
10 mc=80:cc$=chr$(233):cf$=chr$(127):mo$="ins":im=1
11 mode 2:u=PEEK(PEEK(&BE7e)*256+PEEK(&BE7d)):un$=chr$(u+65)+":"
12 ml=600:dim a$(ml)
14 window 1,80,1,24:window#1,1,80,25,25

16 on break cont

20 gosub 100
30 gosub 9700
40 gosub 2600:goto 40

50 data 3E,08,11,00,C0,21,50,C0,01,30,07,E5,D5,ED,B0,D1
55 data E1,47,3E,08,82,57,67,78,3D,20,ED,C9
60 data 3E,08,11,7F,C7,21,2F,C7,01,30,07,E5,D5,ED,B8,D1
65 data E1,47,7A,C6,08,57,67,78,3D,20,ED,C9
70 data CD,80,BC,30,04,32,0F,A6,C9,AF,32,10,A6,C9

100 cls
110 locate#0,23,23:print "Press Ctrl + H to get help":locate#0,10,6
112 PRINT "     Œ                                                 ŒŒ       Œ "
114 PRINT "       ŒƒƒŒ ƒƒƒƒ   ŒƒƒŒ  ƒƒŒ   ƒƒƒŒ  Œƒƒ  ŒŒŒŒ  ŒŒƒƒ "  
116 PRINT "      ƒ        ŒŒ  ƒƒƒƒ  ŒŒƒ  Œƒƒ            ƒƒƒƒƒ"
118 PRINT "     ƒƒ  ƒƒ   ƒƒƒƒ     ƒƒƒ    ƒƒƒƒƒ         ƒƒƒƒƒ   ƒƒƒƒƒ              ƒƒ " 
150 locate#0,62,11:print "Amstrad CPC Edition";
154 locate#0,49,12:print "v1r3, by litwr, (c) 2014 gnu gpl"
156 locate#0,68,14:print "Thanks to SyX"
160 for i=0 to cl-3:read c$:poke cs1+i,val("&"+c$):next i
162 efa=cs1+cl-1:cca=efa-1
164 poke cs3+6,peek(@cca):poke cs3+7,peek(@cca+1)
166 poke cs3+11,peek(@efa):poke cs3+12,peek(@efa+1)
170 for i=1 to 50:call &bd19:next i
180 c$=inkey$:if c$<>"" then 180
190 return

1000 l2=0:c$=""
1010 gosub 1100:if efs=0 then return
1020 if cch=13 then gosub 1100:if efs=0 or cch=10 then return
1030 if cch<32 then 1010
1040 c$=c$+chr$(cch):l2=l2+1:if l2<255 then 1010 else goto 3160

1100 call cs3:efs=peek(efa):cch=peek(cca):return

2000 cls#1:print chr$(12)tab(25)"Notepad +4 CPC Edition commands list":print
2005 print tab(30)chr$(24)"With the CONTROL key"chr$(24)
2010 print "H - help"tab(20)"N - new"tab(40)"L/S - load/save"tab(60)"U/D - page up/down"
2020 print "B/E - to begin/end"tab(20)"F - find forward"tab(40)"R - repeat find"tab(60)"C - cat & load"
2030 print "V - change disk"tab(20)"O - cursor home"tab(40)"Q - quit":print
2040 print tab(31)chr$(24)"With the TAB prefix"chr$(24)
2050 print "  A/C - toggle insert/overwrite mode"tab(42)"D/I - delete/insert a line"
2060 print "  J/K - to start/end of line"tab(42)"P/Q - erase begin/end of line"
2070 print "  V/W - scroll up/down"tab(42)"Any other key - cancel TAB":print
2090 print "del, clr, copy, return, cursors, ..."
2100 print:print:print tab(28)"Hit any key to continue"
2120 c$=inkey$:if c$="" then 2120

2200 rem show screen
2205 if fo then 2230 else fo=1
2210 i=ty:cls
2220 if i<lc and i-ty<24 then gosub 2400:i=i+1:goto 2220
2230 gosub 2310

2250 locate#1,1,1:print#1,f$;:locate#1,28,1:print#1,mo$;
2260 locate#1,33,1:print#1,un$;:return

2270 i=cy
2280 if i<lc and i-ty<24 then gosub 2510:if right$(a$(i),1)<>cc$ then i=i+1:goto 2280
2290 goto 2310

2300 rem show coors
2310 c$=str$(cx+1):d$=str$(cy+1):mid$(c$,1,1)="x":mid$(d$,1,1)="y"
2330 c$=c$+" "+d$:d$=str$(lc):mid$(d$,1,1)="/":c$=c$+d$:l=mc-len(c$)
2350 locate#1,l-2,1:print#1,"   "c$;:return

2400 if len(a$(i))<mc then print a$(i) else print a$(i);:return

2500 rem show line #i
2510 locate#0,1,i-ty+1:print chr$(18)a$(i);:return

2600 locate#0,cx+1,cy-ty+1:cursor 1 
2604 c$=inkey$:if c$="" then 2604
2608 i=asc(c$):fo=0:cursor 0
2610 if i=243 then 4000
2620 if i=242 then 4100
2630 if i=241 then 4200
2640 if i=240 then 4300
2650 if i=15 then 4400
2660 if i>31 and i<127 then 4500
2680 if i=127 then 4700
2690 if i=224 then 4800
2700 if i=13 then 4900
2710 if i=9 then 8000
2720 if i=8 then 2000
2730 if i=17 then memory shm:mode 1:print chr$(12)"Welcome to Basic":end
2740 if i=2 then 9300
2750 if i=5 then 9400
2760 if i=21 then 9500
2770 if i=4 then 9600
2780 if i=14 then 9700
2790 if i=6 then 9800
2800 if i=18 then 9900
2810 if i=12 then 3000
2820 if i=19 then 3200
2830 if i=22 then 3400
2840 if i=3 then 3500
2850 if i=16 then 3800
2890 goto 2600

3000 rem load
3010 cls#1:cls:s$="":print"disk "un$:print"enter file name to load":input s$:if s$="" goto 3100
3014 f$=s$:gosub 5900
3020 on error goto 3700:openin f$:cls:d$="":poke efa,1
3030 gosub 1000:if efs then gosub 7000:print chr$(13)lc;:goto 3030
3080 a$(lc)=a$(lc)+cf$:gosub 7100
3090 closein
3095 on error goto 0
3100 gosub 2205:goto 2310

3160 if len(c$)>mc then gosub 7200:goto 3160
3165 d$=c$:l=len(d$):gosub 1000
3170 if l+l2<255 then c$=d$+c$:return
3175 if l2<255 then 3190
3180 gosub 3190:goto 3160

3190 a$(lc)=d$+left$(c$,mc-l):c$=right$(c$,len(c$)-mc+l):goto 7100

3200 rem save
3210 cls#1:cls:s$="":print"disk "un$,f$
3212 print"Enter filename to save":print"  empty string - use the current one":print"  * - exit"
3214 input s$:c$=s$:if s$="*" then 3100
3216 if s$="" then c$=f$ else f$=c$
3218 if instr(c$,"*") or instr(c$,"?") then 3350
3220 on error goto 3710:openout c$
3240 if a$(0)=cf$ goto 3330
3250 for i=1 to lc
3260 s$=a$(i-1):l=len(s$)
3270 if l>1 then print#9,left$(s$,l-1);:s$=right$(s$,1)
3280 if s$=cf$ goto 3310
3290 print chr$(13)i;:if s$=cc$ then print#9:goto 3310
3300 print#9,s$;
3310 next i
3320 on error goto 0 
3330 closeout
3340 goto 3100

3350 cls:print "cannot open "c$:print ds$
3360 c$=inkey$:if c$="" then 3360 else goto 3100

3400 rem change drive letter
3410 cls:cls#1
3415 u=u+1:if u>1 then u=0
3420 un$=chr$(u+65)+":":if u=0 then |a else |b
3430 goto 2205

3500 rem directory & load
3510 cls#1:cls:dm$="":print"disk "un$:print"enter directory mask (*.* by default)":input dm$:if dm$="" then dm$="*.*"
3520 |dir,dm$
3630 print "You may use second cursor to copy filename from the list"
3640 s$="":input "Filename (empty string = exit)";s$:if s$="" then 3100
3650 goto 3014

3700 if err=14 then print " No memory - next lines are ignored" else print" Error";err,erl
3702 print "Hit a key"
3705 c$=inkey$:if c$="" then 3705 else resume 3080

3710 print" Error";err:print "Hit a key"
3715 c$=inkey$:if c$="" then 3715 else resume 3320

3800 rem delete char
3810 if mid$(a$(cy),cx+1,1)=cf$ then return
3820 if cx<len(a$(cy))-1 then cx=cx+1:goto 4700
3830 k=cy:if cy<lc-1 then gosub 6000
3840 gosub 4150
3850 if k<>cy then cx=0
3860 goto 4700

4000 rem cursor right
4010 if cx<len(a$(cy))-1 then cx=cx+1 else goto 4050
4020 goto 2310

4050 k=cy:gosub 4200
4060 if k<>cy then cx=0:goto 2310
4070 return

4100 rem cursor left
4110 if cx>0 then cx=cx-1 else if cy>0 then cx=len(a$(cy-1))-1:goto 4300
4120 goto 2310

4150 if cx>=len(a$(cy)) then cx=len(a$(cy))-1
4160 return

4200 rem cursor down
4210 e=0
4220 if cy<lc-1 then cy=cy+1
4230 if cy-ty>23 then ty=ty+1:e=1
4240 gosub 4150
4250 if e then call cs1:locate#0,1,24:print chr$(18)a$(ty+23);
4260 goto 2310

4300 rem cursor up
4305 e=0
4310 if cy>0 then cy=cy-1
4320 if cy-ty<0 then ty=ty-1:e=1
4330 gosub 4150
4340 if e then call cs2:locate#0,1,1:print chr$(18)a$(ty);
4350 goto 2310

4400 rem cursor home
4410 cy=ty:cx=0
4420 goto 2310

4500 rem small letter,digits,...
4510 if im then gosub 4820:goto 4000
4520 if cx=len(a$(cy))-1 then gosub 5000 else mid$(a$(cy),cx+1,1)=c$
4530 i=cy:gosub 2510
4540 goto 4000

4700 rem backspace
4710 if cx=0 then 5400
4720 cx=cx-1:a$(cy)=left$(a$(cy),cx)+right$(a$(cy),len(a$(cy))-cx-1)
4730 d$=right$(a$(cy),1)
4740 if d$<>cc$ and d$<>cf$ then gosub 5100 else i=cy:gosub 2510
4750 goto 2310

4800 rem shift+backspace
4810 c$=" "
4820 a$(cy)=left$(a$(cy),cx)+c$+right$(a$(cy),len(a$(cy))-cx)
4830 if len(a$(cy))>mc then 5500
4840 i=cy:goto 2510

4900 rem return
4910 if cx=len(a$(cy))-1 then gosub 7300 else gosub 7400
4920 goto 2205

5000 d$=right$(a$(cy),1)
5010 if d$=cf$ then return
5020 mid$(a$(cy),cx+1,1)=c$
5030 if cx=mc-1 then return
5040 if d$=cc$ then 5100
5050 return

5100 i=cy
5120 if i>=lc-1 goto 2270
5130 d$=a$(i):s$=right$(d$,1)
5140 if s$=cc$ or s$=cf$ then 2270 else gosub 5300
5150 i=i+1
5160 if a$(i)="" then gosub 5200:goto 2205
5170 goto 5120

5200 for k=i to lc-2
5210 a$(k)=a$(k+1)
5220 next k
5230 lc=lc-1
5240 return

5300 l=len(d$):s$=a$(i+1):ls=len(s$)
5310 if ls>mc-l then a$(i)=d$+left$(s$,mc-l):a$(i+1)=right$(s$,ls-mc+l) else a$(i)=d$+s$:a$(i+1)=""
5320 return

5400 if cy=0 then return
5410 gosub 6100:cx=len(a$(cy))-1:a$(cy)=left$(a$(cy),cx)
5420 gosub 5100
5430 goto 2310

5500 i=cy
5520 d$=right$(a$(i),1):a$(i)=left$(a$(i),mc):i=i+1
5530 if d$=cc$ or d$=cf$ then 5600
5540 a$(i)=d$+a$(i)
5550 if len(a$(i))>mc goto 5520
5560 goto 2270

5600 for k=lc to i+1 step -1
5610 a$(k)=a$(k-1)
5620 next k
5630 a$(i)=d$
5640 gosub 7100
5650 goto 2205

5900 cx=0:cy=0:ty=0:lc=0:a$(0)="":return

6000 cy=cy+1
6010 if cy-ty>23 then ty=ty+1
6020 return

6100 cy=cy-1
6110 if ty>cy then ty=cy
6120 return

7000 rem input and optionally split line 
7010 if len(c$)<mc then a$(lc)=c$+cc$ else gosub 7200:goto 7010

7100 if lc<ml-1 then lc=lc+1 else goto 7130
7110 a$(lc)="":return

7130 print"file too big, a line skipped":lc=lc-1
7140 return

7200 a$(lc)=left$(c$,mc):c$=right$(c$,len(c$)-mc):goto 7100

7300 if right$(a$(cy),1)=cf$ then 7600

7400 gosub 7500
7410 c$=a$(cy):a$(cy)=left$(c$,cx)+cc$
7420 a$(cy+1)=right$(c$,len(c$)-cx):cx=0
7430 c$=right$(a$(cy+1),1)
7440 if c$<>cc$ and c$<>cf$ then gosub 4200:goto 5100
7450 goto 4200

7500 for k=lc-1 to cy+1 step -1
7510 a$(k+1)=a$(k)
7520 next k
7530 goto 7100

7600 a$(cy)=left$(a$(cy),len(a$(cy))-1)+cc$
7610 gosub 6000:cx=0
7620 a$(cy)=cf$
7630 goto 7100

8000 rem esc
8010 c$=inkey$:if c$="" goto 8010
8020 i=asc(c$)
8030 if i=100 then 8200
8040 if i=105 then 8300
8050 if i=106 then 8400
8060 if i=107 then 8500
8070 if i=112 then 8600
8080 if i=113 then 8700
8090 if i=118 then 8800
8100 if i=119 then 8900
8110 if i=97 then 9000
8120 if i=99 then 9100
8130 if i=101 then 9200
8140 return

8200 rem esc+d
8210 cx=0
8220 if cy=lc-1 then a$(cy)=cf$:i=cy:gosub 2510:goto 2310
8230 for k=cy to lc-2
8240 a$(k)=a$(k+1)
8250 next k
8260 lc=lc-1:goto 2205

8300 rem esc+i
8310 for k=lc-1 to cy step -1
8320 a$(k+1)=a$(k)
8330 next k
8340 cx=0:a$(cy)=cc$:gosub 7100:goto 2205

8400 rem esc+j
8410 cx=0:goto 2310

8500 rem esc+k
8510 cx=len(a$(cy))-1:goto 2310

8600 rem esc+p
8610 c$=a$(cy):if cx=len(c$)-1 then 8200
8620 a$(cy)=right$(c$,len(c$)-cx-1):cx=0:c$=right$(c$,1)
8630 if c$=cf$ or c$=cc$ then i=cy:gosub 2510:goto 2310
8640 goto 5100

8700 rem esc+q
8710 if cx=0 then 8200
8720 c$=a$(cy):a$(cy)=left$(c$,cx)
8730 if right$(c$,1)=cf$ then a$(cy)=a$(cy)+cf$:i=cy:gosub 2510:goto 2310
8740 goto 5100

8800 rem esc+v
8810 if ty>=lc-1 then return
8820 ty=ty+1:if cy<ty then cy=ty
8830 call cs1:locate#0,1,24:print chr$(18);:if ty+23<lc then print a$(ty+23);
8840 goto 2310

8900 rem esc+w
8910 if ty=0 then return
8920 ty=ty-1:if cy-ty>23 then cy=cy-1
8930 call cs2:locate#0,1,1:print chr$(18)a$(ty);:goto 2310

9000 rem esc+a
9010 im=1:mo$="ins"
9020 goto 2250

9100 rem esc+c
9110 im=0:mo$="owr"
9120 goto 2250

9200 rem esc+o
9210 goto 9110

9300 rem to the begin
9310 cx=0:cy=0:if ty<>0 then ty=0:goto 2205
9320 goto 2310

9400 rem to the end
9410 cx=0:cy=lc-1:l=lc-24:if l<0 then l=0
9420 if ty<>l then ty=l:goto 2205
9430 goto 2310

9500 rem page up
9510 cx=0:l=ty-24:if l<0 then l=0
9520 cy=cy-24:if cy<0 then cy=0
9530 goto 9420

9600 rem page down
9610 cx=0:l=ty+24:if l>=lc then l=lc-24
9620 if l<0 then l=0
9630 cy=cy+l-ty:if cy>=lc then cy=lc-1
9640 goto 9420

9700 rem new
9705 gosub 5900
9710 a$(0)=cf$:lc=1:cx=0:cy=0:f$=""
9720 goto 2205

9800 rem search
9810 cls#1:cls:fs$="":input "Find";fs$:l=len(fs$):if l=0 then 2205
9820 s$=upper$(fs$):fs$=s$
9830 l2=cx+2:gosub 10000
9840 if fi=0 then 2205
9850 cx=fi-1:cy=j
9860 if cy-ty>23 then ty=cy-12
9870 goto 2205

9900 rem repeat find
9910 if fs$="" then return
9920 cls#1:print chr$(12)"seek "fs$:l=len(fs$):goto 9830

10000 for j=cy to lc-1
10010 s$=upper$(a$(j)):print chr$(13) j+1;
10020 fi=instr(l2,s$,fs$):if fi=0 and len(s$)=mc then gosub 10200
10030 if fi then return
10040 l2=1
10050 next j
10060 goto 180

10200 l3=len(fs$):g$=upper$(a$(j+1)):l4=len(g$)
10210 for i=l3-1 to 1 step -1
10220 if l4<l3-i then return
10225 if l2>mc-i+1 then 10240
10230 c$=left$(fs$,i):d$=right$(fs$,l3-i)
10235 if c$=right$(s$,i) and d$=left$(g$,l3-i) then fi=mc-i+1:return
10240 next i
10250 return

