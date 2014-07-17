    2 rem *** notepad cpc, the text file editor, v1
    4 rem *** converted from Commodore plus/4   
    6 rem *** by litwr, 2014, (C) GNU GPL
    8 cx=0:cy=0:ty=0:cc$=chr$(233):mc=80:cf$=chr$(230):mo$="ins":im=1:u=peek(&a700)
   10 mode 2:un$=chr$(u+65)
   12 ml=700:dim a$(ml)

   15 on error goto 20000

   20 gosub 100
   30 gosub 9700
   40 gosub 2600:goto 40

   82 'data225,252,225,97,0,0,0,0,0,160,0,0,0,0,0,0,0
   83 'data0,0,0,0,0,0,0,0,0,225,97,0,108,123,0,0,0,254,97
   84 'data225,160,160,97,108,236,251,123,226,160,226,0,108,236,251,123,225
   85 'data236,251,123,0,226,251,123,108,236,251,97,108,254,252,123,108,255,251,97
   86 'data225,97,251,97,225,97,225,97,0,160,108,123,225,236,226,126,225
   87 'data252,254,126,108,236,251,97,225,97,225,97,0,225,97,0,124,226,251,236
   88 'data124,126,124,126,0,226,226,0,0,124,226,0,0,226,226,126,225
   89 'data97,0,0,0,226,226,126,0,226,226,126,0,0,0,0,0,0,124,126

  100 print chr$(12);
  110 locate#0,23,23:print "Press Ctrl + H to get help"
  120 'for i=0 to 3:for k=0 to 35
  130 'readl:iflthenpoke3354+i*40+k,l
  140 'nextk:nexti
  150 locate#0,10,11:print "v1, by litwr, (c) 2014 gnu gpl"
  160 i=time
  170 if time-i<900 then 170
  180 c$=inkey$:if c$<>"" then 180
  190 return

 2000 print chr$(12)"            Notepad CPC":print
 2005 print "Commands list:":print
 2010 print "C=H - help          C=N - new"
 2020 print "C=L - load          C=S - save"
 2030 print "C=U - page up       C=D - page down"
 2040 print "C=B - begin         C=E - end"
 2050 print "C=F - find forward  C=R - repeat find"
 2070 print "C=C - cat&load      C=Q - quit"
 2075 print "C=V - change disk":print
 2080 print "ESC+A/C/D/I/J/K/O/P/Q/V/W/X":print
 2090 print "ins, del, home, return, cursors, ..."
 2100 print:print:printtab(17)"Hit any key to continue"
 2110 gosub 2900
 2120 c$=inkey$:if c$="" then 2120

 2200 rem show screen
 2205 if fo then return else fo=1
 2210 i=ty:cls
 2220 if i<lc and i-ty<24 then print a$(i):i=i+1:goto 2220
 2230 gosub 2400

 2250 locate#0,1,25:print f$;:locate#0,18,25:print mo$;
 2260 locate#0,23,25:print un$;:return

 2270 i=cy
 2280 if i<lc and i-ty<24 then gosub 2500:if right$(a$(i),1)<>cc$ then i=i+1:goto 2280
 2290 goto 2400

 2300 rem show coors
 2310 c$=str$(cx+1):d$=str$(cy+1)
 2320 c$=right$(c$,len(c$)-1):d$=right$(d$,len(d$)-1)
 2330 c$="x"+c$+" y"+d$
 2340 d$=str$(lc):c$=c$+"/"+right$(d$,len(d$)-1):l=mc-len(c$)
 2350 locate#0,l-2,25:print "   "c$;
 2360 return

 2400 'locate#0,cx+1,cy-ty+1
 2410 return

 2500 rem show line #i
 2510 locate#0,1,i-ty+1:print chr$(20)a$(i);
 2530 return

 2600 locate#0,cx+1,cy-ty+1:cursor 1 
 2604 c$=inkey$:if c$="" goto 2604
 2608 i=asc(c$):fo=0:cursor 0
 2610 if i=243 then 4000
 2620 if i=242 then 4100
 2630 if i=241 then 4200
 2640 if i=240 then 4300
 2650 'home###ifi=19then4400
 2660 if i>31 and i<128 then 4500
 2680 if i=127 then 4700
 2690 'shift+bs###ifi=148then4800
 2700 if i=13 then 4900
 2710 if i=27 then 8000
 2720 if i=8 then 2000
 2730 if i=17 then mode 1:print chr$(12)"Welcome to Basic":end
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

 2900 'remove cursor###poke65292,255:poke65293,255
 2910 return

 3000 rem load
 3010 cls:s$="":print"disk "un$:print"enter file name to load":input s$:if s$="" goto 3100
 3014 f$=s$:gosub 5900
 3020 openin f$:cls
 3030 input#9,c$
 3040 if c$="" goto 3070
 3050 i=asc(c$):ol=lc
 3055 if i=13 then 3070
 3060 if i=10 then gosub 7000 else if i>31 then gosub 7200
 3065 if ol<lc then print chr$(19)lc;
 3070 if not eof goto 3030
 3080 a$(lc)=a$(lc)+cf$:gosub 7100
 3090 closein
 3100 gosub 2200:goto 2400

 3120 cls:gosub 2900:print f$" bad"
 3130 c$=inkey$:if c$="" then 3130
 3140 gosub 9700:goto 3090

 3200 rem save
 3210 cls:s$="":print"disk "dn$,f$
 3212 print"Enter filename to save":print"  empty string - use the current one":print"  * - exit"
 3214 input s$:c$=s$:if s$="*" then 3100
 3216 if s$="" then c$=f$ else f$=c$
 3218 if instr(c$,"*") or instr(c$,"?") then 3350
 3220 openout c$
 3240 if a$(0)=cf$ goto 3090
 3250 for i=1 to lc
 3260 s$=a$(i-1):l=len(s$)
 3270 if l>1 then print#9,left$(s$,l-1);:s$=right$(s$,1)
 3280 if s$=cf$ goto 3310
 3290 if s$=cc$ then print#9:goto 3310
 3300 print#9,s$;:printchr$(13)i;
 3310 next i
 3330 goto 3090

 3350 cls:gosub 2900:print "cannot open "c$:print ds$
 3360 getkeyc$:goto 3100

 3400 rem change drive letter
 3410 u=u+1:if u>1 then u=0
 3420 un$=chr$(u+65)
 3430 gosub 2260:goto 2400

 3500 rem directory & load
 3510 cls:dm$="":print"disk "un$:print"enter directory mask (* by default)":input dm$:if dm$="" then dm$="*"
 3515 print"Hit any key to stop"
 3520 'open8,u,0,"$0:"+dm$
 3530 'get#8,c$:if st=0 then get#8,c$:k=0:else:print"bad mask or unit":printds$:getkeyc$:goto3090
 3540 'get#8,c$:get#8,c$
 3550 'if st then3610:else:get#8,c$:get#8,d$
 3560 'if c$="" then c$=chr$(0)
 3570 'if d$="" then d$=chr$(0)
 3580 'ol=asc(c$)+asc(d$)*256:s$="":getc$:ifc$<>""goto3610
 3590 'get#8,c$
 3600 'if c$<>"" then s$=s$+c$:goto3590:else:goto3700
 3610 'close8
 3620 'c$="":input"File number (empty string = exit)";c$:l=val(c$):if l=0 or l>k then3100
 3630 'open8,u,0,"$0:"+dm$:get#8,c$:get#8,c$:k=-1
 3640 'get#8,c$:get#8,c$:get#8,c$:get#8,c$
 3650 'get#8,c$:if c$<>"" then s$=s$+c$:goto3650:else:k=k+1:goto3670

 3670 'if l<>k then s$="":goto3640
 3680 'l=len(s$):i=instr(s$,chr$(34)):s$=right$(s$,l-i)
 3690 'l=len(s$):i=instr(s$,chr$(34)):s$=left$(s$,i-1):close8:scnclr:goto3014

 3700 'if len(s$)<>27 then 3780
 3710 'if left$(s$,3)="   " then s$=right$(s$,len(s$)-2):else:if left$(s$,2)="  " then s$=right$(s$,len(s$)-1)
 3725 'if right$(s$,3)="   " then s$=left$(s$,len(s$)-2):else:if right$(s$,2)="  " then s$=left$(s$,len(s$)-1)
 3730 'if right$(s$,2)="  " then s$=left$(s$,len(s$)-1)
 3740 'k=k+1:printusing"###";k;:print s$;ol
 3750 'goto3540

 3780 'if k>0 then print"     "ol;s$:goto3540
 3790 'print"   "s$:goto3540

 3800 rem delete char
 3810 if mid$(a$(cy),cx+1,1)=cf$ then return
 3820 if cx<len(a$(cy))-1 then cx=cx+1:goto4700
 3830 k=cy:if cy<lc-1 then gosub 6000
 3840 gosub 4150
 3850 if k<>cy then cx=0
 3860 goto 4700

 4000 rem cursor right
 4010 if cx<len(a$(cy))-1 then cx=cx+1 else goto 4050
 4020 goto 2400

 4050 k=cy:gosub 4200
 4060 if k<>cy then cx=0:goto 2400
 4070 return

 4100 rem cursor left
 4110 if cx>0 then cx=cx-1 else if cy>0 then cx=len(a$(cy-1))-1:goto 4300
 4120 goto 2400

 4150 if cx>=len(a$(cy)) then cx=len(a$(cy))-1
 4160 return

 4200 rem cursor down
 4210 e=0
 4220 if cy<lc-1 then cy=cy+1
 4230 if cy-ty>23 then ty=ty+1:e=1
 4240 gosub 4150
 4250 if e then gosub 2200
 4260 goto 2400

 4300 rem cursor up
 4305 e=0
 4310 if cy>0 then cy=cy-1
 4320 if cy-ty<0 then ty=ty-1:e=1
 4330 goto 4240

 4400 rem cursor home
 4410 cy=ty:cx=0
 4420 goto 2400

 4500 rem small letter,digits,...
 4505 if i=34 then q=1
 4510 if im then gosub 4820:goto 4000
 4520 if cx=len(a$(cy))-1 then gosub 5000 else mid$(a$(cy),cx+1,1)=c$
 4530 i=cy:gosub 2500
 4540 goto 4000

 4700 rem backspace
 4710 if cx=0 then5400
 4720 cx=cx-1:a$(cy)=left$(a$(cy),cx)+right$(a$(cy),len(a$(cy))-cx-1)
 4730 d$=right$(a$(cy),1)
 4740 if d$<>cc$ and d$<>cf$ then gosub 5100 else i=cy:gosub 2500
 4750 goto 2400

 4800 rem shift+backspace
 4810 c$=" "
 4820 a$(cy)=left$(a$(cy),cx)+c$+right$(a$(cy),len(a$(cy))-cx)
 4830 if len(a$(cy))>mc then 5500
 4840 i=cy:goto 2500

 4900 rem return
 4910 if cx=len(a$(cy))-1 then gosub 7300 else gosub 7400
 4920 goto 2200

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
 5160 if a$(i)="" then gosub 5200:goto 2200
 5170 goto 5120

 5200 for k=i to lc-2
 5210 a$(k)=a$(k+1)
 5220 nextk
 5230 lc=lc-1
 5240 return

 5300 l=len(d$):s$=a$(i+1):ls=len(s$)
 5310 if ls>mc-l then a$(i)=d$+left$(s$,mc-l):a$(i+1)=right$(s$,ls-mc+l) else a$(i)=d$+s$:a$(i+1)=""
 5320 return

 5400 if cy=0 thenreturn
 5410 gosub 6100:cx=len(a$(cy))-1:a$(cy)=left$(a$(cy),cx)
 5420 gosub 5100
 5430 goto 2400

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
 5650 goto 2200

 5900 if lc=0 then lc=1
 5910 cx=0:cy=0:ty=0
 5920 lc=lc-1:a$(lc)="":if lc>0 goto 5920 else return

 6000 cy=cy+1
 6010 if cy-ty>23 then ty=ty+1
 6020 return

 6100 cy=cy-1
 6110 if ty>cy then ty=cy
 6120 return

 7000 rem input line after eol
 7010 if len(a$(lc))<mc then a$(lc)=a$(lc)+cc$ else gosub 7100:a$(lc)=cc$

 7100 if lc<ml-1 then lc=lc+1 else print"file too big":end
 7110 return

 7200 rem add input char
 7210 if len(a$(lc))=mc then gosub 7100
 7220 a$(lc)=a$(lc)+c$
 7230 return

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
 8030 if i=68 then 8200
 8040 if i=73 then 8300
 8050 if i=74 then 8400
 8060 if i=75 then 8500
 8070 if i=80 then 8600
 8080 if i=81 then 8700
 8090 if i=86 then 8800
 8100 if i=87 then 8900
 8110 if i=65 then 9000
 8120 if i=67 then 9100
 8130 if i=79 then 9200
 8140 return

 8200 rem esc+d
 8210 cx=0
 8220 if cy=lc-1 then a$(cy)=cf$:i=cy:gosub 2500:goto 2400
 8230 for k=cy to lc-2
 8240 a$(k)=a$(k+1)
 8250 next k
 8260 lc=lc-1:goto 2200

 8300 rem esc+i
 8310 for k=lc-1 to cy step -1
 8320 a$(k+1)=a$(k)
 8330 next k
 8340 cx=0:a$(cy)=cc$:gosub 7100:goto 2200

 8400 rem esc+j
 8410 cx=0:goto 2400

 8500 rem esc+k
 8510 cx=len(a$(cy))-1:goto 2400

 8600 rem esc+p
 8610 c$=a$(cy):if cx=len(c$)-1 then 8200
 8620 a$(cy)=right$(c$,len(c$)-cx-1):cx=0:c$=right$(c$,1)
 8630 if c$=cf$ or c$=cc$ then i=cy:gosub 2500:goto 2400
 8640 goto 5100

 8700 rem esc+q
 8710 if cx=0 then 8200
 8720 c$=a$(cy):a$(cy)=left$(c$,cx)
 8730 if right$(c$,1)=cf$ then a$(cy)=a$(cy)+cf$:i=cy:gosub 2500:goto 2400
 8740 goto 5100

 8800 rem esc+v
 8810 if ty>=lc-1 then return
 8820 ty=ty+1:if cy<ty then cy=ty
 8830 goto 2200

 8900 rem esc+w
 8910 if ty=0 then return
 8920 ty=ty-1:if cy-ty>23 then cy=cy-1
 8930 goto 2200

 9000 rem esc+a
 9010 im=1:mo$="ins"
 9020 goto 2250

 9100 rem esc+c
 9110 im=0:mo$="owr"
 9120 goto 2250

 9200 rem esc+o
 9210 goto 9110

 9300 rem to the begin
 9310 cx=0:cy=0:if ty<>0 then ty=0:goto2200
 9320 goto 2400

 9400 rem to the end
 9410 cx=0:cy=lc-1:l=lc-24:if l<0 then l=0
 9420 if ty<>l then ty=l:goto 2200
 9430 goto 2400

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
 9720 goto 2200

 9800 rem search
 9810 scnclr:fs$="":input"Find";fs$:l=len(fs$):if l=0 then2200
 9820 s$=fs$:gosub 10100:fs$=s$
 9830 gosub 2900:l2=cx+2:gosub 10000
 9840 if fi=0 then2200
 9850 cx=fi-1:cy=j
 9860 if cy-ty>23 then ty=cy-12
 9870 goto 2200

 9900 rem repeat find
 9910 if fs$="" then return
 9920 print chr$(12)"seek "fs$:l=len(fs$):goto 9830

 10000 for j=cy to lc-1
 10010 s$=a$(j):gosub 10100:print chr$(13) j+1;
 10020 fi=instr(s$,fs$,l2)
 10030 if fi then return
 10040 l2=1
 10050 next j
 10060 goto 180

 10100 if s$="" then return
 10105 l1=len(s$)
 10110 for i=1 to l1
 10120 k=asc(mid$(s$,i,1)):n=k
 10130 if k>96 and k<123 then n=k-32:goto 10150
 10140 if k>192 and k<219 then n=k-128
 10150 if n<>k then mid$(s$,i,1)=chr$(n)
 10160 next i
 10170 return
 
 20000 print "an error's occured or run/stop's pressed at line"el

