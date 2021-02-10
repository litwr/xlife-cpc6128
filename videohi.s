showscn0 ld a,(startp+1)
         or a
         ret z

         ld hl,xcont2
         jp calllo

crsrcalc proc   ;in: de, b; sets: xcrsr, ycrsr, crsrx, crsry; outputs: xy; adjusts viewport in the zoom in mode
         local loop1,loopx,cont0,cont1,cont3,cont4,cont5,cont6,l1,l2,l3,l4,l7
         ld a,d
         and 7
         ld c,a
         ld a,e
         srl c
         rra
         srl c
         rra
         srl c
         rra
loop1    cp 10
         jr c,cont6

         inc c
         sub 10
         jr loop1

cont6    rlca
         rlca
         rlca
         ld h,a
         ld a,e
         and 7
         add a,h
         ;srl b
         ;srl b
         srl b
         rla
         ld (crsrx),a

         ;ld a,c
         ;rlca
         ;rlca
         ;rlca
         ;ld b,a
         ld a,d
         and $38
         rrca
         rrca
         rrca
         ld d,a
         ;add a,b
         ;ld (crsry),a
         ld a,c
         ld (crsry),a
         ld hl,ctab
         ld b,a
         add a,l
         ld l,a
         ld a,(hl)
         add a,d
         daa
         ld c,a
         ld h,0
         jr c,l2

         ld a,b
         cp $d
         jr c,l1

l2       inc h
l1       ld a,h
         ld (ycrsr),a
         ld a,c
         and $f
         ld (ycrsr+2),a
         ld a,c
         and $f0
         rrca
         rrca
         rrca
         rrca
         ld (ycrsr+1),a
         ld a,(crsrx)
         and 7
         ld c,a
         ld a,(crsrx)
         and $fc
         rrca
         rrca
         ld d,a
         srl a
         ld b,a
         ld hl,ctab
         add a,l
         ld l,a
         ld a,(hl)
         add a,c
         daa
         ld c,a
         ld h,0
         jr c,l4

         ld a,b
         cp $d
         jr c,l3

l4       inc h
l3       ld a,h
         ld (xcrsr),a
         ld a,c
         and $f
         ld (xcrsr+2),a
         ld a,c
         and $f0
         rrca
         rrca
         rrca
         rrca
         ld (xcrsr+1),a
         ld a,b
         ld (crsrx),a
         call xyout
         ld a,(zoom)
         or a
         ret z

         ld c,up
         ld a,(vptilecy)
         push af
         add a,8
         ld b,a
         pop af
         or a
         jp m,cont3

         ld c,down
         sub 8
         ld b,a
         ld a,(vptilecy)
         cp 24
         jr c,cont4

cont3    ld a,b
         ld (vptilecy),a
         jr cont1

cont4    ld c,left
         ld a,(vptilecx)
         push af
         add a,8
         ld b,a
         pop af
         or a
         jp m,cont5

         ld c,right
         sub 8
         ld b,a
         ld a,(vptilecx)
         cp 40
         jr c,cont0

cont5    ld a,b
         ld (vptilecx),a
cont1    ld iy,viewport
         ld hl,readhl
         call calllo
         push hl
         pop iy
         ld b,0
         add iy,bc
         ld hl,readhl
         call calllo
         ld (viewport),hl
         ld c,l
         ld b,h
         ld a,down
         ld hl,nextcell
         call calllo
         ld a,down
         ld hl,nextcell
         call calllo
         ld d,4
loopx    ld a,right
         ld hl,nextcell
         call calllo
         dec d
         jr nz,loopx

         ld hl,(viewport)
         ld de,44*tilesize
         add hl,de
         ld a,h
         cp b
         jr nz,l7

         ld a,l
         cp c
         jr z,cont0

l7       call setviewport
cont0    ld hl,showscnz
         jp calllo
         endp

nohide   call setbg0  ;must be before xyout
         call zerocc
         call incgen
         call initxt
         call showrules
         ld hl,showscn
         call calllo
xyout    ld b,3       ;must be after nohide
         ld hl,xcrsr
         ld de,$c7c2
         call digiout
         ld b,3
         ld hl,ycrsr
         ld de,$c7ca
         jp digiout

help     call printn
         db 12,"        *** XLIFE COMMANDS ***",$d,$a
         db 15,3,"!",15,2," randomize screen",$d,$a
         db 15,3,"%",15,2," set random density - default 42%",$d,$a
         db 15,3,"+",15,2,"/",15,3,"-",15,2," zoom in/out",$d,$a
         db 15,3,".",15,2,"/",15,3,"H",15,2," center/home cursor",$d,$a
         db 15,3,"?",15,2," show this help",$d,$a
         db 15,3,"B",15,2," benchmark",$d,$a
         db 15,3,"C",15,2," clear the screen",$d,$a
         db 15,3,"E",15,2," toggle the pseudocolor mode",$d,$a
         db 15,3,"g",15,2," toggle the run/stop mode",$d,$a
         db 15,3,"h",15,2," toggle the hiding (fastest) mode",$d,$a
         db 15,3,"l",15,2," load and transform a pattern",$d,$a
         db 15,3,"L",15,2," reload a pattern",$d,$a
         db 15,3,"o",15,2," one step",$d,$a
         db 15,3,"Q",15,2," quit",$d,$a
         db 15,3,"R",15,2," set the rules",$d,$a
         db 15,3,"S",15,2," save",$d,$a
         db 15,3,"T",15,2," toggle plain/torus topology",$d,$a
         db 15,3,"v",15,2," show some info",$d,$a
         db 15,3,"V",15,2," show comments to a pattern",$d,$a
         db 15,3,"X",15,2,"/",15,3,"Z",15,2," reload/set&save a palette",$d,$a
         db $d,$a
         db 15,1,"Use ",15,3,"cursor keys",15,1," to set a position and a "
         db 15,3,"space key",15,1," to toggle the current cell.",$d,$a
         db "Use a ",15,3,"shift",15,1," to speed up the movement$"
         jp KM_WAIT_CHAR

indens   proc
         local loop1

         call printn
         db 12,"SELECT DENSITY OR PRESS ",15,3,"ESC",15,2," TO EXIT",$d,$a
         db 15,3,"0",15,2," - 12.5%",$d,$a
         db 15,3,"1",15,2," - 28%",$d,$a
         db 15,3,"2",15,2," - 42%",$d,$a
         db 15,3,"3",15,2," - 54%",$d,$a
         db 15,3,"4",15,2," - 64%",$d,$a
         db 15,3,"5",15,2," - 73%",$d,$a
         db 15,3,"6",15,2," - 81%",$d,$a
         db 15,3,"7",15,2," - 88.5%",$d,$a
         db 15,3,"8",15,2," - 95%",$d,$a
         db 15,3,"9",15,2," - 100%$"

loop1    call KM_WAIT_CHAR
         cp $fc       ;esc
         ret z

         cp "0"
         jr c,loop1

         cp "0"+10
         jr nc,loop1

         sub $2f
         ld (density),a
         ret
         endp

inmode   proc
         local loop1

         call printn
         db $d,$a,$d,$a,15,2,"SELECT BENCHMARK MODE",$d,$a
         db 15,3," 0",15,2," - CALCULATIONS",$d,$a
         db 15,3," 1",15,2," - VIDEO",$d,$a
         db 15,3," 2",15,2," - BOTH",15,1,"$"

loop1    call KM_WAIT_CHAR
         cp "0"
         jr c,loop1

         cp "3"
         jr nc,loop1

         sub "1"
         ld (svfnlen),a
         ret
         endp

totext   ld a,(zoom)
         or a
         call z,split_off
         call SCR_CLEAR
         ld a,21
         call setbg
         xor a
         ld c,a
         ld b,a
         ld a,1
         call SCR_SET_INK
         ld a,9
         ld c,a
         ld b,a
         ld a,2
         call SCR_SET_INK
         ld a,3
         ld c,a
         ld b,a
         ;ld a,3
         call SCR_SET_INK
         ld a,21
         jp chgbr

tograph  proc
         ld a,(zoom)
tograph0 or a
         call z,split_on
         jp setcolor
         endp

crsrclr  proc
;removes cursor from screen
;in: zoom, crsrtile, crsrbyte, crsrbit, pseudoc
;must set IY
         local cont2,pgcur
         ld a,(zoom)
         or a
         jr nz,pgcur

         ld iy,(crsrtile)
         ld hl,readde
         call calllo
         ld a,(crsrbyte)
         rlca
         rlca
         rlca
         add a,d
         ld d,a
         ld a,(pseudoc)
         or a
         jr nz,cont2

         ld hl,vidmacx
         jp calllo

pgcur    xor a
         ld (crsrpgmk),a
         ld hl,showscnz
         call calllo
         ld a,1
         ld (crsrpgmk),a
         ld iy,(crsrtile)   ;do not remove!
         ret

cont2    ld hl,vidmacpx
         jp calllo
         endp

if 0
inputhex proc
;gets 2 hex digits and prints them
;out: ZF=1 - empty input
;in: h - hicur, l - lowcur
         local loop1,loop3,cont2,cont7
         call TXT_SET_CURSOR
         call TXT_PLACE_CURSOR
loop3    ld de,stringbuf
         ld c,0
loop1    call KM_WAIT_CHAR
         cp $d
         jp z,cont1x

         cp $fc       ;esc
         ret z

         cp $7f       ;backspace
         jr z,cont2

         cp "F"+1
         jr nc,loop1

         cp "A"
         jr nc,cont7

         cp "0"
         jr c,loop1

         cp "9"+1
         jr nc,loop1

cont7    ld b,a
         ld a,2       ;hex length limit
         cp c
         ld a,b
         jr z,loop1

         ld (de),a
         inc de
         inc c
         ld b,a
         call TXT_REMOVE_CURSOR
         ld a,b
         call TXT_OUTPUT
cont4    call TXT_PLACE_CURSOR
         jr loop1

cont2    dec de
         dec c
         jp m,loop3

         call delchr
         jr cont4
         endp
endif

delchr   proc
         call TXT_REMOVE_CURSOR
         call printn
         db 8,32,8,"$"
         ret
         endp

insteps  proc
         local loop1,loop3,cont1,cont2,cont4
         call printn
         db 12,"NUMBER OF GENERATIONS: ",15,1,"$"
         call TXT_PLACE_CURSOR
loop3    ld de,stringbuf
         ld c,0
loop1    call KM_WAIT_CHAR
         cp $d
         jr z,cont1

         cp $fc       ;esc
         ret z

         cp $7f       ;backspace
         jr z,cont2

         cp $3a
         jr nc,loop1

         cp "0"
         jr c,loop1

         ld b,a
         ld a,5
         cp c
         ld a,b
         jr z,loop1

         ld (de),a
         inc de
         inc c
         ld b,a
         call TXT_REMOVE_CURSOR
         ld a,b
         call TXT_OUTPUT
cont4    call TXT_PLACE_CURSOR
         jr loop1

cont1    call TXT_REMOVE_CURSOR
         ld l,e
         ld h,d
         ld a,c
         or a
         ret z

         ld bc,(~stringbuf)+1
         add hl,bc
         ret    ;hl - buffer length, de - buffer end

cont2    dec de
         dec c
         jp m,loop3

         call delchr
         jr cont4
         endp

chgclrs1 proc
         local l1
         ld a,(ix)
         call printdec
         jr nz,l1

         dec h
l1       call printn
         db "): $"
         ret
         endp

chgclrs2 proc
         local l1,l2
         dec c
         jr z,l1

         ld a,l
         or a
         jr z, l1

         ld l,10
         dec a
         jr z,l2

         ld l,20
l2       ld a,l
         add a,h
         ld (ix),a
         ret

l1       ld (ix),l
         ret
         endp

nofnchar db 37,40,41,44,46,47,58,59,60,61,62,63,91,92,93,95,124,126,127

loadmenu proc  ;must be after nofnchar!
         local loop1,loop1a,loop3,loop3a,loopx,exit,menu2,repeat
         local cont1,cont1a,cont2,cont2a,cont4,cont4a,cont7,cont7a,cont8,cont11

         call printn
         db 12,15,2,"INPUT FILENAME, AN EMPTY STRING MEANS TOSHOW DIRECTORY. PRESS "
         db 15,3,"TAB",15,2," TO USE RAMDISK, ",15,3,"*",15,2," TO CHANGE DRIVE, ",15,3,"ESC",15,2," TO EXIT"
         db 15,1,$a,"$"
         ld c,0
         call showdrv
         ;call TXT_PLACE_CURSOR   ;cursor on
loop3    ld de,fn
         ld c,0
loop1    call KM_WAIT_CHAR
         cp $d
         jr z,cont1

         cp $7f      ;backspace
         jr z,cont2

         cp $fc      ;esc
         jr nz,cont7

exit     call TXT_REMOVE_CURSOR   ;cursor off
         xor a
         ret

cont7    cp "*"
         jr nz,cont11

         call chgdrv
         jr loop1

cont11   cp 9        ;TAB
         jr nz,cont8

         call TXT_REMOVE_CURSOR     ;cursor off
         call ramdisk
         jr exit

cont8    and $7f
         cp 33
         jr c,loop1

         ld hl,nofnchar
         push bc
         ld bc,loadmenu-nofnchar
         cpir
         pop bc
         jr z,loop1

         ld b,a
         ld a,c
         cp 8
         ld a,b
         jr nc,loop1

         ld (de),a
         inc de
         inc c
         ld b,a
         call TXT_REMOVE_CURSOR
         ld a,b
         call TXT_OUTPUT
cont4    call TXT_PLACE_CURSOR
         jp loop1

cont1    call TXT_REMOVE_CURSOR     ;cursor off
         ld a,c
         or a
         jr z,menu2

         add a,3
         ld (fnlen),a
         ld a,"."
         ld (de),a
         inc de
         ld a,"8"
         ld (de),a
         inc de
         ld a,"L"
         ld (de),a
         ret    ;nz

cont2    dec de
         dec c
         jp m,loop3

         call delchr
         jr cont4

menu2    call setdirmsk
         ld a,b
         cp $fc    ;esc
         jr z,repeat

         call showdir
         call printn
         db 30,18,"ENTER FILE# OR ",15,3,"ESC",15,2,": ",15,1,"$"
loopx    call TXT_PLACE_CURSOR   ;cursor on
loop3a   ld de,stringbuf
         ld c,0
loop1a   call KM_WAIT_CHAR
         cp $fc       ;esc
         jr nz,cont7a

repeat   call TXT_REMOVE_CURSOR     ;cursor off
         jp loadmenu

cont7a   cp $d
         jr z,cont1a

         cp $7f       ;backspace
         jr z,cont2a

         cp "0"
         jr c,loop1a

         cp "9"+1
         jr nc,loop1a

         ld b,a
         ld a,c
         cp 2
         ld a,b
         jr z,loop1a

         ld (de),a
         inc de
         inc c
         ld b,a
         call TXT_REMOVE_CURSOR
         ld a,b
         call TXT_OUTPUT
cont4a   call TXT_PLACE_CURSOR
         jr loop1a

cont1a   call TXT_REMOVE_CURSOR
         ld a,c
         or a
         jr z,loopx

         push bc
         push de
         call TXT_PLACE_CURSOR
         call findfn
         pop de
         pop bc
         jr nz,loop1a

         call TXT_REMOVE_CURSOR
         or 1    ;set nz
         ret

cont2a   dec de
         dec c
         jp m,loop3a

         call delchr
         jr cont4a
         endp

inputdec proc
;gets up to 2 digits and prints them
;out: ZF=1 - empty input
;in: h - hicur, l - lowcur
;changes: a,bc,de,hl
;returns: BCD in HL, length in C
         local loop1,loop3,cont1,cont2,cont4,cont5
         call TXT_SET_CURSOR
         call TXT_PLACE_CURSOR
loop3    ld de,stringbuf
         ld c,0
loop1    call KM_WAIT_CHAR
         cp $d
         jp z,cont1

         cp $7f       ;backspace
         jr z,cont2

         cp "0"
         jr c,loop1

         cp "9"+1
         jr nc,loop1

         ld b,a
         ld a,2       ;hex length limit
         cp c
         jr z,loop1

         ld a,b
         xor $30
         ld (de),a
         inc de
         inc c
         call TXT_REMOVE_CURSOR
         ld a,b
         call TXT_OUTPUT
cont4    call TXT_PLACE_CURSOR
         jr loop1

cont1    ld a,c
         ld hl,(stringbuf)
         cp 2
         jr c,cont5

         ld a,l
         cp 3
         jr nc,loop1

         cp 2
         jp c,cont5

         ld a,h
         cp 7
         jr nc,loop1

cont5    call TXT_REMOVE_CURSOR
         ld a,c
         or a
         ret

cont2    dec de
         dec c
         jp m,loop3

         call delchr
         jr cont4
         endp

chgdrv   ld hl,(icurdev)
         ld a,(hl)
         xor 1
         ld (hl),a
         add a,"A"
         ld (drvlett),a
         call TXT_REMOVE_CURSOR
showdrv  call printn
         db $d
drvlett  db "A:$"
         ld a,c
         add a,3
         call TXT_SET_COLUMN
         jp TXT_PLACE_CURSOR

getsvfn  proc
         local loop1,loop3
         local cont1,cont2,cont4,cont7,cont11

         call printn
         db 12,15,2,"Enter filename (",15,3,"Esc",15,2," - exit, "
         db 15,3,"*",15,2," - drive)",15,1,$a,"$"
         ld c,0
         call showdrv
         ;call TXT_PLACE_CURSOR   ;cursor on
loop3    ld de,svfn
         ld c,0
loop1    call KM_WAIT_CHAR
         cp $d
         jr z,cont1

         cp "*"
         jr nz,cont11

         call chgdrv
         jr loop1

cont11   cp $7f      ;backspace
         jr z,cont2

         cp $fc      ;esc
         jr nz,cont7

         call TXT_REMOVE_CURSOR   ;cursor off
         xor a
         ret

cont7    and $7f
         cp 33
         jr c,loop1

         ld hl,nofnchar
         push bc
         ld bc,18
         cpir
         pop bc
         jr z,loop1

         ld b,a
         ld a,c
         cp 8           ;fn length limit
         ld a,b
         jr nc,loop1

         ld (de),a
         inc de
         inc c
         ld b,a
         call TXT_REMOVE_CURSOR
         ld a,b
         call TXT_OUTPUT
cont4    call TXT_PLACE_CURSOR
         jp loop1

cont1    call TXT_REMOVE_CURSOR     ;cursor off
         ld a,c
         or a
         ret z

         add a,3
         ld (svfnlen),a
         ld a,"."
         ld (de),a
         inc de
         ld a,"8"
         ld (de),a
         inc de
         ld a,"L"
         ld (de),a
         ret    ;nz

cont2    dec de
         dec c
         jp m,loop3

         call delchr
         jr cont4
         endp

chgcolors proc
         local loop,cont1,cont2,cont3,cont4,cont5,cont6,cont7,cont8,cont9,cont10,cont11,cont12,cont14
         ld ix,borderpc
         call printn
         db 12,15,2,"PRESS ",15,3,"ENTER",15,2," TO USE DEFAULT COLOR OR INPUT "
         db "A DECIMAL NUMBER OF GREY SCALE COLOR (0-26).",$d,$a,15,1,"THE PLAIN BORDER ($"
         ld hl,$1804
         call chgclrs1
         call inputdec
         jr z,cont1

         call chgclrs2
cont1    inc ix
         call printn
         db $d,$a,"THE TORUS BORDER ($"
         ld hl,$1805
         call chgclrs1
         call inputdec
         jr z,cont2

         call chgclrs2
cont2    inc ix
         call printn
         db $d,$a,"THE CURSOR OVER AN EMPTY CELL ($"
         ld hl,$2506
         call chgclrs1
         call inputdec
         jr z,cont3

         call chgclrs2
cont3    inc ix
         call printn
         db $d,$a,"THE CURSOR OVER A LIVE CELL ($"
         ld hl,$2307
         call chgclrs1
         call inputdec
         jr z,cont9

         call chgclrs2
cont9    inc ix
         call printn
         db $d,$a,"THE CURSOR OVER A NEW CELL ($"
         ld hl,$2208
         call chgclrs1
         call inputdec
         jr z,cont4

         call chgclrs2
cont4    inc ix
         call printn
         db $d,$a,"THE LIVE CELL ($"
         ld hl,$1509
         call chgclrs1
         call inputdec
         jr z,cont5

         call chgclrs2
cont5    inc ix
         call printn
         db $d,$a,"THE NEW CELL ($"
         ld hl,$140a
         call chgclrs1
         call inputdec
         jr z,cont6

         call chgclrs2
cont6    inc ix
         call printn
         db $d,$a,"THE EDIT BACKGROUND ($"
         ld hl,$1b0b
         call chgclrs1
         call inputdec
         jr z,cont7

         call chgclrs2
cont7    inc ix
         call printn
         db $d,$a,"THE GO BACKGROUND ($"
         ld hl,$190c
         call chgclrs1
         call inputdec
         jr z,cont8

         call chgclrs2
cont8    inc ix
         call printn
         db $d,$a,"THE TENTATIVE FRAME ($"
         ld hl,$1b0d
         call chgclrs1
         call inputdec
         jr z,cont12

         call chgclrs2
cont12   inc ix
         call printn
         db $d,$a,"THE FRAME OVER A LIVE CELL ($"
         ld hl,$220e
         call chgclrs1
         call inputdec
         jr z,cont14

         call chgclrs2
cont14   inc ix
         call printn
         db $d,$a,"THE FRAME OVER A NEW CELL ($"
         ld hl,$210f
         call chgclrs1
         call inputdec
         jr z,cont10

         call chgclrs2
cont10   inc ix
         call printn
         db $d,$a,"THE TENTATIVE CELLS ($"
         ld hl,$1b10
         call chgclrs1
         call inputdec
         jr z,cont11

         call chgclrs2
cont11   call printn
         db $d,$a,"TO SAVE THIS CONFIG?$"
loop     call KM_WAIT_CHAR
         or $20
         cp "n"
         ret z

         cp "y"
         jr nz,loop

         jp savecf
         endp

setviewport proc
         local cont1,cont2,cont3,cont4,cont5,cont6,cont7,cont8,cont10,loop12
         ld hl,(crsrtile)
         ld (viewport),hl
         ld ix,vptilecx
         ld a,2
         ld (vptilecx),a
         dec a
         ld (vptilecy),a
         ld hl,(ycrsr)
         ld a,l
         or h
         jr nz,cont1

         ld a,(ycrsr+2)
         cp 8
         jr nc,cont1

         dec (ix+1)
         ld hl,(viewport)      ;up
         ld de,tilesize*hormax
         add hl,de
         ld (viewport),hl
         jr cont2

cont1    ld a,(ycrsr)
         dec a
         jr nz,cont2

         ld a,(ycrsr+1)
         cp 8
         jr c,cont2
         jr nz,cont4

         ld a,(ycrsr+2)
         cp 4
         jr c,cont2

cont4    inc (ix+1)
         ld hl,(viewport)      ;down
         ld de,(~(tilesize*hormax))+1
         add hl,de
         ld (viewport),hl

cont2    ld hl,(xcrsr)
         ld a,l
         or h
         jr nz,cont3

         ld a,(xcrsr+2)
         cp 8
         jr nc,cont3

         dec (ix)
         dec (ix)
         ld hl,(viewport)      ;left2
         ld de,tilesize*2
         add hl,de
         ld (viewport),hl
         jr cont5

cont3    ld a,(xcrsr)
         or a
         jr nz,cont6

         ld a,(xcrsr+1)
         cp 1
         jr c,cont7
         jr nz,cont6

         ld a,(xcrsr+2)
         cp 6
         jr nc,cont6

cont7    dec (ix)
         ld hl,(viewport)      ;left1
         ld de,tilesize
         add hl,de
         ld (viewport),hl
         jr cont5

cont6    ld a,(xcrsr)
         dec a
         jr nz,cont8

         ld a,(xcrsr+1)
         cp 5
         jr nz,cont8

         ld a,(xcrsr+2)
         cp 2
         jr c,cont8

         inc (ix)
         inc (ix)
         ld hl,(viewport)      ;right2
         ld de,(~(tilesize*2))+1
         add hl,de
         ld (viewport),hl
         jr cont5

cont8    ld a,(xcrsr)
         dec a
         jr nz,cont5

         ld a,(xcrsr+1)
         cp 4
         jr c,cont5
         jr nz,cont10

         ld a,(xcrsr+2)
         cp 4
         jr c,cont5

cont10   inc (ix)
         ld hl,(viewport)      ;right1
         ld de,(~tilesize)+1
         add hl,de
         ld (viewport),hl
cont5    ld iy,(viewport)
         ld hl,fixvp
         call calllo
         ld (viewport),hl
         ld b,3
loop12   sla (ix)
         sla (ix+1)
         djnz loop12

         ld a,(crsrbyte)
         add a,(ix+1)
         ld (ix+1),a
         ld a,(crsrbit)
         call calcx
         add a,(ix)
         ld (ix),a
         ret
         endp

setdirmsk proc
         local loop1,loop2,loop3,loop5,cont1,cont2,cont3,cont4,cont6,cont8
         call printn
         db 12,15,2,"SET DIRECTORY MASK (",15,3,"ENTER",15,2," = *)",$d,$a,15,1,"$"
         call TXT_PLACE_CURSOR   ;cursor on
loop3    ld de,stringbuf
         ld c,0
loop1    call KM_WAIT_CHAR
         cp $d
         jp z,cont1

         cp $7f       ;backspace
         jr z,cont2

         cp $fc       ;esc
         ld b,a
         jp z,TXT_REMOVE_CURSOR

         and $7f
         cp 33
         jr c,loop1

         ld hl,nofnchar+2
         push bc
         ld bc,17
         cpir
         pop bc
         jr z,loop1

         ld a,8       ;fn length limit
         cp c
         ld a,b
         jr z,loop1

         cp "a"
         jr c,cont8

         cp "z"+1
         jr nc,cont8

         sub 32
cont8    ld (de),a
         inc de
         inc c
         call TXT_REMOVE_CURSOR
         ld a,b
         call TXT_OUTPUT
cont4    call TXT_PLACE_CURSOR
         jr loop1

cont1    ld a,c
         ld b,a
         ld de,dirname
         or a
         jr z,cont6

         ld hl,stringbuf
loop2    ld a,(hl)
         cp "*"
         jr z,cont6

         ld (de),a
         inc de
         inc hl
         djnz loop2

         ld a,8
         sub c
         jr z,cont3

         ld b,a
         ld a," "
         jr loop5

cont6    ld a,8
         sub c
         add a,b
         ld b,a
         ld a,"?"
loop5    ld (de),a
         inc de
         djnz loop5
cont3    jp TXT_REMOVE_CURSOR

cont2    dec de
         dec c
         jp m,loop3

         call delchr
         jr cont4
         endp

showrect proc
         local loop0,loop1,finish,finish0,lselect,cont1,cont2
         call SCR_CLEAR
         ld hl,$119
         call TXT_SET_CURSOR
         call printn
         db 15,2,"MOVE, ",15,3,"R",15,2,"OTATE, ",15,3,"F",15,2,"LIP, ",15,3
         db "ENTER",15,2,", ",15,3,"ESC",15,1,"  X   Y$"
         xor a
         ld (xdir),a
         ld (ydir),a
         ld (xchgdir),a
         call tograph0
         call showscn0
         call xyout
loop0    call drawrect
         call showtent
         ld hl,crsrset
         call calllo
loop1    call KM_WAIT_CHAR
         cp $f2
         jr z,lselect

         cp $f6       ;shifted cursor left
         jr z,lselect

         cp $f3
         jr z,lselect

         cp $f7       ;shifted cursor right
         jr z,lselect

         cp $f0
         jr z,lselect

         cp $f4       ;shifted cursor up
         jr z,lselect

         cp $f1
         jr z,lselect

         cp $f5       ;shifted cursor down
         jr z,lselect

         cp "."
         jr z,lselect

         cp "H"
         jr z,lselect

         cp "r"
         jr nz,cont1

         call clrrect
         ld a,(xchgdir)
         xor 1
         ld (xchgdir),a
         ld a,(xdir)
         ld l,a
         ld a,(ydir)
         xor 1
         ld (xdir),a
         ld a,l
         ld (ydir),a
         jr loop0

cont1    cp "f"
         jr nz,cont2

         call clrrect
         ld a,(xdir)
         xor 1
         ld (xdir),a
         jr loop0

cont2    cp $d
         jr z,finish

         cp $fc   ;escape
         jr z,finish0
         jr loop1

lselect  push af
         call clrrect
         pop af
         call dispat2
         jr loop0

finish   scf

finish0  push af
         call clrrect
         ;call totext    ;makes CLRSCN
         pop af
         ret
         endp

drawrect proc
         local x8pos,x8poscp,x8bit,y8pos,y8poscp,y8byte,rectulx,rectuly,xcut,ycut,localbase
         local cont1,cont2,cont3,cont4,cont5,cont6,cont7,cont8,cont10,cont11,cont12
         local loop10,loop11,xmove,ymove,loopdn,loopup,looprt,looplt,nextrt,nextlt,drrect1
;adjcell - iy
;**calls: pixel11
localbase equ $fff0        ;link to clrrect!
x8pos    equ localbase
x8poscp  equ localbase+1   ;link to clrrect!
x8bit    equ localbase+2
y8pos    equ t1
y8poscp  equ localbase+3   ;link to clrrect!
y8byte   equ localbase+4
rectulx  equ localbase+5
rectuly  equ localbase+6
xcut     equ localbase+7
ycut     equ localbase+8

         call xchgxy
         ld a,(crsrbyte)
         ld (y8byte),a
         ld a,(crsrbit)
         ld (x8bit),a
         call calcx
         xor a
         ld (xcut),a
         ld (ycut),a
         ld a,(crsrx)
         rlca
         rlca
         rlca
         add a,b
         ld (rectulx),a
         ld c,a
         ld a,(xdir)
         or a
         ld a,(x0)
         jr z,cont4

         ld b,a
         ld a,c
         sub b
         jr nc,cont2

         xor $ff
         jr z,cont10

         ld a,(xcut)
         inc a
         ld (xcut),a
cont10   ld a,c
         inc a
         jr cont7

cont4    add a,c
         jr c,cont5

         cp 161
         jr c,cont2

cont5    ld a,(xcut)
         inc a
         ld (xcut),a
         ld a,160

cont2    sub c
         jr nc,cont7

         xor $ff
         inc a
cont7    ld (x8pos),a
         ld (x8poscp),a
         ld a,(crsrbyte)
         ld b,a
         ld a,(crsry)
         rlca
         rlca
         rlca
         add a,b
         ld (rectuly),a
         ld c,a
         ld a,(ydir)
         or a
         jr z,cont3

         ld a,(y0)
         ld b,a
         ld a,c
         sub b
         jr nc,cont1

         xor $ff
         jr z,cont12

         ld a,(ycut)
         inc a
         ld (ycut),a

cont12   ld a,c
         inc a
         jr cont8

cont3    ld a,(y0)
         add a,c
         jr c,cont6

         cp 193
         jr c,cont1

cont6    ld a,(ycut)
         inc a
         ld (ycut),a
         ld a,192

cont1    sub c
         jr nc,cont8

         xor $ff
         inc a
cont8    ld (y8pos),a
         ld (y8poscp),a
         ld iy,(crsrtile)
         call ymove
         ld a,(ycut)
         or a
         jr nz,cont11

         call xmove
cont11   ld a,(x8poscp)
         ld (x8pos),a
         ld a,(y8poscp)
         ld (y8pos),a
         ld a,(crsrbyte)
         ld (y8byte),a
         ld a,(crsrbit)
         ld (x8bit),a
         ld iy,(crsrtile)
         call xmove
         ld a,(xcut)
         or a
         ret nz

ymove    ld a,(ydir)
         or a
         jr nz,loopup

loopdn   call drrect1
loop10   call pixel11      ;used: de,b
         ld hl,y8pos
         dec (hl)
         ret z

        ld a,8
        add a,d
        ld d,a
        ld hl,y8byte
        inc (hl)
        ld a,(hl)
        cp 8
        jr nz,loop10

         ld a,down
         call vnextcell
         xor a
         ld (y8byte),a
         jr loopdn

loopup   call drrect1
loop11   call pixel11
         ld hl,y8pos
         dec (hl)
         ret z

         ld a,d
         sub 8
         ld d,a
         ld hl,y8byte
         dec (hl)
         jp p,loop11

         ld a,up
         call vnextcell
         ld a,7
         ld (y8byte),a
         jr loopup

xmove    ld a,(xdir)
         or a
         jr nz,looplt

looprt   call drrect1
         call pixel11
         ld hl,x8pos
         dec (hl)
         ret z

         ld a,(x8bit)
         rrca
         jr c,nextrt

         ld (x8bit),a
         jr looprt

nextrt   ld a,right
         call vnextcell
         ld a,$80
         ld (x8bit),a
         jr looprt

looplt   call drrect1
         call pixel11
         ld hl,x8pos
         dec (hl)
         ret z

         ld a,(x8bit)
         rlca
         jr c,nextlt

         ld (x8bit),a
         jr looplt

nextlt   ld a,left
         call vnextcell
         ld a,1
         ld (x8bit),a
         jr looplt

drrect1  ld hl,readde
         call calllo
         ld a,(y8byte)
         rlca
         rlca
         rlca
         add a,d
         ld d,a
         ld a,(x8bit)   ;copy
         ld hl,$2010
         jp xcont1
         endp

vnextcell ld c,iyl
         ld b,iyh
         ld hl,nextcell
         call calllo
         ld iyl,c
         ld iyh,b
         ret

clrrect  proc       ;in: x8poscp, y8poscp
         local cl3
         local x8pos,x8poscp,x8bit,y8pos,y8poscp,y8byte,localbase
localbase equ $fff0         ;link to drawrect!
x8pos    equ localbase
x8poscp  equ localbase+1    ;link to drawrect!
x8bit    equ localbase+2
y8pos    equ t1
y8poscp  equ localbase+3    ;link to drawrect!
y8byte   equ localbase+4

         call xchgxy
         ld a,(y8poscp)
         ld (y8pos),a
         ld a,(crsrbyte)
         ld (y8byte),a
         ld a,(crsrbit)
         call calcx
         ld b,a
         ld a,(xdir)
         or a
         ld a,b
         jr z,cl3

         sub 8
         cpl
cl3      ld b,a
         ld a,(x8poscp)
         add a,b
         ld (x8pos),a
         ld (x8poscp),a

         ld iy,(crsrtile)
         ld hl,clrrectlo
         jp calllo
         endp

showtent proc        ;used: a,bc,de,hl,iy*,ix*
         local loop,l1,l3,exit
         ld a,(x0)
         ld b,a
         ld a,(y0)
         ld c,a
         push bc
         xor a
         ld h,a       ;$15
         ld l,a       ;$14
         ld (ppmode),a

loop     ld a,(memb9)
         cp h
         jr nz,l1

         ld a,(memb8)
         cp l
         jr z,exit

l1       ld de,EOP
         push hl
         add hl,de
         ld a,(hl)
         ld (x0),a
         ld b,a
         ld a,4    ;+4*256
         add a,h
         ld h,a
         ld a,(hl)
         ld (y0),a
         or b
         jr z,l3

         ld hl,putpixel
         call calllo
l3       pop hl
         inc hl
         jr loop

exit     pop bc
         ld a,c
         ld (y0),a
         ld a,b
         ld (x0),a
         ld a,1
         ld (ppmode),a
         ret
         endp

printdec proc   ;in:a   out: zf set if one digit
         local cont1,cont2,cont3,loop1,loop2
         push hl
         ld l,$ff
         cp 100
         jr c,cont1

loop1    inc l
         sub 100
         jr nc,loop1

         add a,100
         ld h,a
         ld a,l
         add a,"0"
         call TXT_OUTPUT
         ld a,h
cont3    ld l,$ff
loop2    inc l
         sub 10
         jr nc,loop2

         add a,10
         ld h,a
         ld a,l
         add a,"0"
         call TXT_OUTPUT
         ld a,h
cont2    add a,"0"
         call TXT_OUTPUT
         inc l
         pop hl
         ret

cont1    cp 10
         jr c,cont2
         jr cont3
         endp

infov    proc
         local cont1,cont2,loop1
         local sizex

         call SCR_CLEAR
         ld a,(fnlen)
         or a
         jr z,cont1

         call printn
         db  "Last loaded filename: $"

         ld hl,fn
loop1    ld a,(hl)
         call TXT_OUTPUT
         inc hl
         ld a,"."
         cp (hl)
         jr nz,loop1

cont1    call boxsz
         jr z,cont2

         ;xmin - d, ymin - e
         ;xmax - b, ymax - c
sizex     equ t1     ;connected to curx at boxsz and savepat
         ;cury - sizey - h

         call printn
         db $d,$a,"Active pattern size: $"

         ld a,(sizex)
         call printdec
         ld a,"x"
         call TXT_OUTPUT

         ld a,h
         call printdec
         call printn
         db $d,$a,"Box life bounds: $"

         ld a,d
         call printdec
         call printn
         db "<=X<=$"

         ld a,b
         call printdec
         ld a," "
         call TXT_OUTPUT
         ld a,e
         call printdec
         call printn
         db "<=Y<=$"

         ld a,c
         call printdec

cont2    call printn
         db $d,$a,"Rules: $"
         call showrules2
         jp KM_WAIT_CHAR
         endp

