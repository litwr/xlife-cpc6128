printhex macro       ;print hex (<$A0) number in AC
         local l1
         ld b,a
         and $f0
         rrca
         rrca
         rrca
         rrca
         xor $30
         call TXT_OUTPUT
         ld a,b
         and $f
         xor $30
         cp $3a
         jr c,l1

         add a,7
l1       call TXT_OUTPUT
         endm

showscn0 ld a,(zoom)
         or a
         ret nz

         ld hl,(tilecnt)
         ld a,h
         or l
         ret z

         ld hl,xcont2
         jp calllo

crsrcalc proc   ;in: de, b
         local loop1,loopx,cont0,cont1,cont3,cont4,cont5,cont6,l1,l2,l3,l4,l7
;*cont5    lda i1+1    ;start of coorditates calculation
;*         sec
;*         sbc #$20
;*         sta i1+1
;*         lsr i1+1
;*         ror i1
;*         lsr i1+1
;*         ror i1
;*         lsr i1+1
;*         ror i1
;*         ldy #0
;*cont7    sec
;*         lda i1
;*         sbc #$28
;*         tax
;*         lda i1+1
;*         sbc #0
;*         bmi cont6

;*         sta i1+1
;*         stx i1
;*         iny
;*         bne cont7

;*cont6    sty crsry
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
;*         lda ctab,y
;*         sed
;*         clc
;*         adc crsrbyte
;*         sta t1
         ld hl,ctab
         ld b,a
         add a,l
         ld l,a
         ld a,h
         add a,0
         ld h,a
         ld a,(hl)
         add a,d
         daa
         ld c,a
;*         ldx #$30
;*         bcs l2
         ld h,0
         jr c,l2

;*         cpy #$d
;*         bcc l1
         ld a,b
         cp $d
         jr c,l1

;*l2       inx
l2       inc h

;*l1       stx ycrsr
l1       ld a,h
         ld (ycrsr),a

;*         lda t1
;*         and #$f
;*         eor #$30
;*         sta ycrsr+2
         ld a,c
         and $f
         ld (ycrsr+2),a

;*         lda t1
;*         lsr
;*         lsr
;*         lsr
;*         lsr
;*         eor #$30
;*         sta ycrsr+1
         ld a,c
         and $f0
         rrca
         rrca
         rrca
         rrca
         ld (ycrsr+1),a
;*         ldx #8
;*         lda crsrbit
;*cont8    dex
;*         lsr
;*         bcc cont8

;*         lda i1
;*         sta crsrx
;*         lsr
         ld a,(crsrx)
         and 7
         ld c,a
         ld a,(crsrx)
         and $fc
         rrca
         rrca
         ld d,a
         srl a

;*         tay
;*         txa
;*         clc
;*         adc ctab,y
;*         sta t1
;*         ldx #$30
;*         bcs l4
         ld b,a
         ld l,low(ctab)
         add a,l
         ld l,a
         ld a,high(ctab)
         adc a,0
         ld h,a
         ld a,(hl)
         add a,c
         daa
         ld c,a
;*         ldx #$30
;*         bcs l4
         ld h,0
         jr c,l4

;*         cpy #$d
;*         bcc l3
         ld a,b
         cp $d
         jr c,l3

;*l4       inx
l4       inc h

;*l3       stx xcrsr
l3       ld a,h
         ld (xcrsr),a

;*         lda t1
;*         and #$f
;*         eor #$30
;*         sta xcrsr+2
         ld a,c
         and $f
         ld (xcrsr+2),a

;*         lda t1
;*         lsr
;*         lsr
;*         lsr
;*         lsr
;*         eor #$30
;*         sta xcrsr+1
;*         cld
         ld a,c
         and $f0
         rrca
         rrca
         rrca
         rrca
         ld (xcrsr+1),a
         ld a,b
         ld (crsrx),a
xyout    ld b,3
         ld hl,xcrsr
         ld de,$c7c2
         call digiout
         ld b,3
         ld hl,ycrsr
         ld de,$c7ca
         call digiout

;*         lda zoom
;*         beq exit
         ld a,(zoom)
         or a
         ret z

;*         ldy #up
;*         ldx #7
;*         lda vptilecy
;*         bmi cont3
         ld c,up
         ld b,7
         ld a,(vptilecy)
         or a
         jp m,cont3

;*         ldy #down
;*         cmp #24
;*         bcc cont4
         ld c,down
         cp 24
         jr c,cont4

;*         ldx #16
;*cont3    stx vptilecy
;*         bne cont1
         ld b,16
cont3    ld a,b
         ld (vptilecy),a
         jr cont1

;*cont4    ldy #left
;*         lda vptilecx
;*         bmi cont5
cont4    ld c,left
         ld a,(vptilecx)
         or a
         jp m,cont5

;*         ldy #right
;*         cmp #40
;*         bcc cont2
         ld c,right
         cp 40
         jr c,cont0

;*         ldx #32
;*cont5    stx vptilecx
;*cont1    lda (viewport),y
;*         tax
;*         iny
;*         lda (viewport),y
;*         sta viewport+1
;*         sta adjcell+1
;*         stx viewport
;*         stx adjcell
         ld b,32
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
         push hl
         pop bc

;*         ldy #down
;*         jsr nextcell
;*         dey
;*         jsr nextcell
;*         lda #4
;*         sta i2
;*loopx    ldy #right
;*         jsr nextcell
;*         dec i2
;*         bne loopx
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

;*         lda viewport
;*         clc
;*         adc #<44*tilesize
;*         tax
;*         lda viewport+1
;*         adc #>44*tilesize
;*         cmp adjcell+1
;*         bne l7

;*         cpx adjcell
;*         beq cont0

         ld hl,(viewport)
         ld de,44*tilesize
         add hl,de
         ld a,h
         cp b
         jr nz,l7

         ld a,l
         cp c
         jr z,cont0

;*l7       jsr setviewport
;*cont0    jsr showscnpg
l7       call setviewport
cont0    ld hl,showscnpg
         jp calllo
;*cont2    lda #0
;*         sta t1
;*         lda vptilecy
;*         asl
;*         asl
;*         adc vptilecy
;*         asl
;*         asl
;*         rol t1
;*         asl
;*         rol t1
;*         adc vptilecx
;*         sta $ff0d
;*         lda t1
;*         adc #0
;*         sta $ff0c
;*exit     rts
         endp

help     call printn
         db 12,"        *** XLIFE COMMANDS ***",$d,$a
         db 15,3,"!",15,2," randomize screen",$d,$a
         db 15,3,"%",15,2," set random density - default 59%",$d,$a
         db 15,3,"+",15,2,"/",15,3,"-",15,2," zoom in/out",$d,$a
         db 15,3,".",15,2,"/",15,3,"H",15,2," center/home cursor",$d,$a
         db 15,3,"?",15,2," show this help",$d,$a
         db 15,3,"B",15,2," benchmark",$d,$a
         db 15,3,"C",15,2," clear screen",$d,$a
         db 15,3,"E",15,2," toggle pseudocolor mode",$d,$a
         db 15,3,"g",15,2," toggle run/stop mode",$d,$a
         db 15,3,"h",15,2," toggle hide mode - about 20% faster",$d,$a
         db 15,3,"l",15,2," load and transform file",$d,$a
         db 15,3,"L",15,2," reload pattern",$d,$a
         db 15,3,"o",15,2," one step",$d,$a
         db 15,3,"Q",15,2," quit",$d,$a
         db 15,3,"R",15,2," set the rules",$d,$a
         db 15,3,"S",15,2," save",$d,$a
         db 15,3,"T",15,2," toggle plain/torus topology",$d,$a
         db 15,3,"v",15,2," show some info",$d,$a
         db 15,3,"V",15,2," show comments to the pattern",$d,$a
         db 15,3,"X",15,2,"/",15,3,"Z",15,2," reload/set&save palette",$d,$a
         db 15,3,"@",15,2," toggle drive",$d,$a
         db 15,1,"Use ",15,3,"cursor keys",15,1," to set the position and "
         db 15,3,"space key",15,1," to toggle the current cell.",$d,$a
         db "Use ",15,3,"shift",15,1," to speed up the movement$"
         jp KM_WAIT_CHAR

indens   proc
         local loop1

         call printn
         db 12,"SELECT DENSITY OR PRESS ",15,3,"ESC",15,1," TO EXIT",$d,$a
         db 15,3,"0",15,2," - 94%",$d,$a
         db 15,3,"1",15,2," - 82%",$d,$a
         db 15,3,"2",15,2," - 70%",$d,$a
         db 15,3,"3",15,2," - 59%",$d,$a
         db 15,3,"4",15,2," - 47%",$d,$a
         db 15,3,"5",15,2," - 35%",$d,$a
         db 15,3,"6",15,2," - 23%",$d,$a
         db 15,3,"7",15,2," - 12%$"

loop1    call KM_WAIT_CHAR
         cp $fc       ;esc
         ret z

         cp "0"
         jr c,loop1

         cp "8"
         jr nc,loop1

         xor #$30
         ld (density),a
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

;*         ldy #video
;*         lda (currp),y
;*         sta i1
;*         iny
;*         lda (currp),y
;*         sta i1+1
;*         #assign16 currp,crsrtile
         ld iy,(crsrtile)
         ld hl,readde
         call calllo
         ld a,(crsrbyte)
         add a,iyl
         ld l,a
         ld a,iyh
         adc a,0
         ld h,a
         ld a,(crsrbyte)
         rlca
         rlca
         rlca
         add a,d
         ld d,a
;*         ldy crsrbyte
;*         lda crsrbit
;*         and #$f0
;*         beq cont1

;*         lda pseudoc
;*         bne cont2
         ld a,(pseudoc)
         or a
         jr nz,cont2

;*         #vidmac1
;*exit     rts
         ld ix,vidmac
         ld (jsrfar+1),ix
         jp calllo1

pgcur    xor a
         ld (crsrpgmk),a
         ld hl,showscnpg
         call calllo
         ld a,1
         ld (crsrpgmk),a
         ld iy,(crsrtile)
         ret

;*cont2    lda #pc
;*         clc
;*         adc crsrbyte
;*         sta t1
;*         #vidmac1p
;*         rts
cont2    ld ix,pctable
         push hl
         pop iy
         ld hl,vidmacp
         call calllo
         ld iy,(crsrtile)
         ret

;*cont1    lda #8
;*         eor i1
;*         sta i1
;*         lda pseudoc
;*         bne cont3

;*         #vidmac2
;*         rts

;*cont3    lda #pc
;*         clc
;*         adc crsrbyte
;*         sta t1
;*         lda (currp),y
;*         sty 7
;*         sta i2
;*         ldy t1
;*         lda (currp),y
;*         tay
;*         and i2
;*         ldx 7
;*         sta pctemp1,x   ;old
;*         tya
;*         eor #$ff
;*         and i2
;*         sta pctemp2,x   ;new
;*         ldy crsrbyte
;*         #vidmac2p
;*         rts
;*         .bend
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

         call TXT_REMOVE_CURSOR
         call printn
         db 8,32,8,"$"
         jr cont4
         endp
endif

insteps  proc
         local loop1,loop3,cont1,cont2,cont4
         call printn
         db 12,"NUMBER OF GENERATIONS: $"
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
         ld a,7
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
         push de
         pop hl
         ld a,c
         or a
         ret z

         ld bc,(~stringbuf)+1
         add hl,bc
         ret    ;hl - buffer length, de - buffer end

cont2    dec de
         dec c
         jp m,loop3

         call TXT_REMOVE_CURSOR
         call printn
         db 8,32,8,"$"
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

nofnchar db 42,63,37,40,41,44,46,47,59,60,61,62,91,92,93,95,124,126,127

loadmenu proc
         local loop1,loop1a,loop3,loop3a,exit,menu2,repeat
         local cont1,cont1a,cont2,cont2a,cont4,cont4a,cont7,cont7a,cont8

         call printn
         db 12,15,2,"INPUT FILENAME, AN EMPTY STRING MEANS TOSHOW DIRECTORY, PRESS "
         db 15,3,"TAB",15,2," TO USE RAMDISKOR ",15,3,"ESC",15,2," TO EXIT",15,1,$d,$a,"$"
         call TXT_PLACE_CURSOR   ;cursor on
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

cont7    cp 9        ;TAB
         jr nz,cont8

         call TXT_REMOVE_CURSOR     ;cursor off
         call ramdisk
         jr exit

cont8    and $7f
         cp 33
         jr c,loop1

         ld hl,nofnchar
         push bc
         ld bc,19
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

         call TXT_REMOVE_CURSOR
         call printn
         db 8,32,8,"$"
         jr cont4

menu2    call setdirmsk
         ld a,b
         cp $fc    ;esc
         jr z,repeat

         ;call printn
         ;db 12,15,2,"USE ",15,3
;*         .text "run/stop"
;*         .byte 30
;*         .text " and "
;*         .byte 28
;*         .text "cbm key"
;*         .byte 30
;*         .text " as usual"
;*         .byte $d,0

         call showdir
         call printn
         db 30,18,"ENTER FILE# OR ",15,3,"ESC",15,2,": ",15,1,"$"
         call TXT_PLACE_CURSOR   ;cursor on
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
         jr z,menu2

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

         call TXT_REMOVE_CURSOR
         call printn
         db 8,32,8,"$"
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

         call TXT_REMOVE_CURSOR
         call printn
         db 8,32,8,"$"
         jr cont4
         endp

getsvfn  proc
         local loop1,loop3
         local cont1,cont2,cont4,cont7

         call printn
         db 12,15,2,"Enter filename or press ",15,3,"Esc",15,2," to exit",15,1,$d,$a,"$"
         call TXT_PLACE_CURSOR   ;cursor on
loop3    ld de,svfn
         ld c,0
loop1    call KM_WAIT_CHAR
         cp $d
         jr z,cont1

         cp $7f      ;backspace
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
         ld bc,19
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

         call TXT_REMOVE_CURSOR
         call printn
         db 8,32,8,"$"
         jr cont4
         endp

chgcolors proc
         local loop,cont1,cont2,cont3,cont4,cont5,cont6,cont7,cont8,cont9,cont10,cont11,cont12,cont14
         ld ix,borderpc
         call printn
         db 12,15,2,"PRESS ",15,3,"ENTER",15,2," TO USE DEFAULT COLOR OR INPUT "
         db "DECIMAL NUMBER OF GREY SCALE COLOR (0-26).",$d,$a,15,1,"THE PLAIN BORDER ($"
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
         db $d,$a,"THE CURSOR OVER EMPTY CELL ($"
         ld hl,$2206
         call chgclrs1
         call inputdec
         jr z,cont3

         call chgclrs2
cont3    inc ix
         call printn
         db $d,$a,"THE CURSOR OVER LIVE CELL ($"
         ld hl,$2107
         call chgclrs1
         call inputdec
         jr z,cont9

         call chgclrs2
cont9    inc ix
         call printn
         db $d,$a,"THE CURSOR OVER NEW CELL ($"
         ld hl,$2008
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
         db $d,$a,"THE FRAME OVER LIVE CELL ($"
         ld hl,$200e
         call chgclrs1
         call inputdec
         jr z,cont14

         call chgclrs2
cont14   inc ix
         call printn
         db $d,$a,"THE FRAME OVER NEW CELL ($"
         ld hl,$1f0f
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
;*         #assign16 viewport,crsrtile
         ld hl,(crsrtile)
         ld (viewport),hl
         ld ix,vptilecx
;*         ldx #2
;*         stx vptilecx
;*         dex
;*         stx vptilecy
         ld a,2
         ld (vptilecx),a
         dec a
         ld (vptilecy),a
;*         lda $fe5
;*         ora $fe6
;*         eor #$30
;*         bne cont1
         ld hl,(ycrsr)
         ld a,l
         or h
         jr nz,cont1
 
;*         lda $fe7
;*         cmp #$38
;*         bcs cont1
         ld a,(ycrsr+2)
         cp 8
         jr nc,cont1

;*         dec vptilecy
         dec (ix+1)
;*         lda viewport          ;up
;*         adc #<tilesize*20     ;CY=0
;*         sta viewport
;*         lda viewport+1
;*         adc #>tilesize*20
;*         sta viewport+1
;*         bne cont2
         ld hl,(viewport)      ;up
         ld de,tilesize*20
         add hl,de
         ld (viewport),hl
         jr cont2

;*cont1    lda $fe5
;*         cmp #$31
;*         bne cont2
cont1    ld a,(ycrsr)
         dec a
         jr nz,cont2

;*         lda $fe6
;*         cmp #$38
;*         bcc cont2
;*         bne cont4
         ld a,(ycrsr+1)
         cp 8
         jr c,cont2
         jr nz,cont4

;*         lda $fe7
;*         cmp #$34
;*         bcc cont2
         ld a,(ycrsr+2)
         cp 4
         jr c,cont2

;*cont4    inc vptilecy
;*         lda viewport          ;down
;*         sbc #<tilesize*20     ;CY=1
;*         sta viewport
;*         lda viewport+1
;*         sbc #>tilesize*20
;*         sta viewport+1
cont4    inc (ix+1)
         ld hl,(viewport)      ;down
         ld de,(~(tilesize*20))+1
         add hl,de
         ld (viewport),hl

;*cont2    lda $fe0
;*         ora $fe1
;*         eor #$30
;*         bne cont3
cont2    ld hl,(xcrsr)
         ld a,l
         or h
         jr nz,cont3

;*         lda $fe2
;*         cmp #$38
;*         bcs cont3
         ld a,(xcrsr+2)
         cp 8
         jr nc,cont3

;*         dec vptilecx
;*         dec vptilecx
         dec (ix)
         dec (ix)
;*         lda viewport          ;left2
;*         adc #<tilesize*2      ;CY=0
;*         sta viewport
;*         lda viewport+1
;*         adc #>tilesize*2
;*         sta viewport+1
;*         bne cont5
         ld hl,(viewport)      ;left2
         ld de,tilesize*2
         add hl,de
         ld (viewport),hl
         jr cont5

;*cont3    lda $fe0
;*         eor #$30
;*         bne cont6
cont3    ld a,(xcrsr)
         or a
         jr nz,cont6

;*         lda $fe1
;*         cmp #$31
;*         bcc cont7
;*         bne cont6
         ld a,(xcrsr+1)
         cp 1
         jr c,cont7
         jr nz,cont6

;*         lda $fe2
;*         cmp #$36
;*         bcs cont6
         ld a,(xcrsr+2)
         cp 6
         jr nc,cont6

;*cont7    dec vptilecx
cont7    dec (ix)
;*         lda viewport          ;left1
;*         adc #<tilesize        ;CY=0
;*         sta viewport
;*         lda viewport+1
;*         adc #>tilesize
;*         sta viewport+1
;*         bne cont5
         ld hl,(viewport)      ;left1
         ld de,tilesize
         add hl,de
         ld (viewport),hl
         jr cont5

;*cont6    lda $fe0
;*         cmp #$31
;*         bne cont8
cont6    ld a,(xcrsr)
         dec a
         jr nz,cont8

;*         lda $fe1
;*         cmp #$35
;*         bne cont8
         ld a,(xcrsr+1)
         cp 5
         jr nz,cont8

;*         lda $fe2
;*         cmp #$32
;*         bcc cont8
         ld a,(xcrsr+2)
         cp 2
         jr c,cont8

;*         inc vptilecx
;*         inc vptilecx
         inc (ix)
         inc (ix)
;*         lda viewport          ;right2
;*         sbc #<tilesize*2      ;CY=1
;*         sta viewport
;*         lda viewport+1
;*         sbc #>tilesize*2
;*         sta viewport+1
;*         bne cont5
         ld hl,(viewport)      ;right2
         ld de,(~(tilesize*2))+1
         add hl,de
         ld (viewport),hl
         jr cont5

;*cont8    lda $fe0
;*         cmp #$31
;*         bne cont5
cont8    ld a,(xcrsr)
         dec a
         jr nz,cont5

;*         lda $fe1
;*         cmp #$34
;*         bcc cont5
;*         bne cont10
         ld a,(xcrsr+1)
         cp 4
         jr c,cont5
         jr nz,cont10

;*         lda $fe2
;*         cmp #$34
;*         bcc cont5
         ld a,(xcrsr+2)
         cp 4
         jr c,cont5

;*cont10   inc vptilecx
cont10   inc (ix)
;*         lda viewport          ;right1
;*         sbc #<tilesize        ;CY=1
;*         sta viewport
;*         lda viewport+1
;*         sbc #>tilesize
;*         sta viewport+1
         ld hl,(viewport)      ;right1
         ld de,(~tilesize)+1
         add hl,de
         ld (viewport),hl

;*cont5    ldy #ul
;*         lda (viewport),y
;*         tax
;*         iny
;*         lda (viewport),y
;*         sta viewport+1
;*         stx viewport
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
         local devtxt,loop1,loop2,loop3,loop5,cont1,cont2,cont3,cont4,cont6,cont7,cont8
         ld a,(curdev)
         add a,"A"
         ld (devtxt+1),a
         call printn
devtxt   db 12,"A",15,2,": SET DIRECTORY MASK (",15,3,"ENTER",15,2," = *)",$d,$a,15,1,"$"
         call TXT_PLACE_CURSOR   ;cursor on
loop3    ld de,stringbuf
         ld c,0
loop1    call KM_WAIT_CHAR
         cp $d
         jp z,cont1

         cp "@"
         jr nz,cont7

         ld a,(curdev)
         xor 1
         ld (curdev),a
         jr setdirmsk

cont7    cp $7f       ;backspace
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

         call TXT_REMOVE_CURSOR
         call printn
         db 8,32,8,"$"
         jr cont4
         endp

showrect proc
         local loop0,loop1,finish,finish0,lselect,cont1,cont2
         call SCR_CLEAR
;*         clc
;*         ldy #0
;*         ldx #24
;*         jsr $fff0        ;set position for the text
        ld hl,$119
        call TXT_SET_CURSOR
;*         jsr $ff4f
;*         .byte 30
;*         .text "move, "
;*         .byte 28,"r",30
;*         .text "otate, "
;*         .byte 28,"f",30
;*         .text "lip, "
;*         .byte 28
;*         .text "enter"
;*         .byte 30
;*         .text ", "
;*         .byte 28
;*         .text "esc"
;*         .byte 144,0
         call printn
         db 15,2,"MOVE, ",15,3,"R",15,2,"OTATE, ",15,3,"F",15,2,"LIP, ",15,3
         db "ENTER",15,2,", ",15,3,"ESC",15,1,"  X   Y$"
;*         lda #0
;*         sta xdir
;*         sta ydir
;*         sta xchgdir
         xor a
         ld (xdir),a
         ld (ydir),a
         ld (xchgdir),a
;*         jsr tograph0
;*         jsr showscn0
         xor a
         call tograph0
         call showscn0
;*loop0    jsr drawrect
;*         jsr crsrset0
loop0    call drawrect
         call showtent
         ld hl,crsrset
         call calllo
;*loop1    jsr getkey
loop1    call KM_WAIT_CHAR

;*         cmp #$9d   ;cursor left
;*         beq lselect
         cp $f2
         jr z,lselect

         cp $f6       ;shifted cursor left
         jr z,lselect

;*         cmp #$1d   ;cursor right
;*         beq lselect
         cp $f3
         jr z,lselect

         cp $f7       ;shifted cursor right
         jr z,lselect

;*         cmp #$91   ;cursor up
;*         beq lselect
         cp $f0
         jr z,lselect

         cp $f4       ;shifted cursor up
         jr z,lselect

;*         cmp #$11   ;cursor down
;*         beq lselect
         cp $f1
         jr z,lselect

         cp $f5       ;shifted cursor down
         jr z,lselect

;*         cmp #"."   ;to center
;*         beq lselect
         cp "."
         jr z,lselect

;*         cmp #19    ;to home
;*         beq lselect
         cp "H"
         jr z,lselect

;*         cmp #"R"-"A"+$41
;*         bne cont1
         cp "r"
         jr nz,cont1

;*         jsr clrrect
;*         lda xchgdir
;*         eor #1
;*         sta xchgdir
;*         ldx xdir
;*         lda ydir
;*         eor #1
;*         sta xdir
;*         stx ydir
;*         bpl loop0
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

;*cont1    cmp #"F"-"A"+$41
;*         bne cont2
cont1    cp "f"
         jr nz,cont2

;*         jsr clrrect
;*         lda xdir
;*         eor #1
;*         sta xdir
;*         bpl loop0
         call clrrect
         ld a,(xdir)
         xor 1
         ld (xdir),a
         jr loop0

;*cont2    cmp #$d
;*         beq finish
cont2    cp $d
         jr z,finish

;*         cmp #$1b
;*         beq finish0
;*         bne loop1
         cp $fc   ;escape
         jr z,finish0
         jr loop1

;*lselect  pha
;*         jsr clrrect
;*         pla
;*         jsr dispat0
;*         jmp loop0
lselect  push af
         call clrrect
         pop af
         call dispat2
         jr loop0

;*finish   clc
finish   scf

;*finish0  php
;*         jsr clrrect
;*         jsr restbl
;*         jsr totext
;*         lda #147
;*         jsr $ffd2
;*         plp
;*         rts
finish0  push af
         call clrrect
         ;call totext    ;makes CLRSCN
         pop af
         ret
         endp

drawrect proc
         local x8pos,x8poscp,x8bit,y8pos,y8poscp,y8byte,rectulx,rectuly,xcut,ycut,localbase
         local cont1,cont2,cont3,cont4,cont5,cont6,cont7,cont8,cont10,cont11,cont12
         local loop1,loop10,loop11,xmove,ymove,loopdn,loopup,looprt,looplt,nextrt,nextlt,drrect1
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

;*         jsr xchgxy
         call xchgxy
;*         lda crsrbyte
;*         sta y8byte
;*         lda crsrbit
;*         sta x8bit
         ld a,(crsrbyte)
         ld (y8byte),a
         ld a,(crsrbit)
         ld (x8bit),a
;*         ldx #8
;*loop1    dex
;*         lsr
;*         bcc loop1
         ld b,8
loop1    dec b
         rrca
         jr nc,loop1

;*         stx m1+1
;*         sta xcut        ;0 -> xcut
;*         sta ycut
         xor a
         ld (xcut),a
         ld (ycut),a
;*         lda crsrx
;*         lsr
;*         asl
;*         asl
;*         asl
;*m1       adc #0
;*         sta rectulx
;*         ldx xdir
;*         beq cont4
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

;*         sec
;*         sbc x0
;*         bcs cont2
         ld b,a
         ld a,c
         sub b
         jr nc,cont2

;*         eor #$ff
;*         beq cont10
         xor $ff
         jr z,cont10

;*         inc xcut
;*cont10   lda rectulx
;*         adc #1
;*         bcc cont7
         ld a,(xcut)
         inc a
         ld (xcut),a
cont10   ld a,c
         inc a
         jr cont7

;*cont4    adc x0
;*         bcs cont5
cont4    add a,c
         jr c,cont5

;*         cmp #161
;*         bcc cont2
         cp 161
         jr c,cont2

;*cont5    lda #160
;*         inc xcut
cont5    ld a,(xcut)
         inc a
         ld (xcut),a
         ld a,160

;*cont2    sec
;*         sbc rectulx
;*         bcs cont7
cont2    sub c
         jr nc,cont7

;*         eor #$ff
;*         adc #1
         xor $ff
         inc a
;*cont7    sta x8pos
;*         sta x8poscp
;*         lda crsry
;*         asl
;*         asl
;*         asl
;*         adc crsrbyte
;*         sta rectuly
;*         ldx ydir
;*         beq cont3
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

;*         sec
;*         sbc y0
;*         bcs cont1
         ld a,(y0)
         ld b,a
         ld a,c
         sub b
         jr nc,cont1

;*         eor #$ff
;*         beq cont12
         xor $ff
         jr z,cont12

;*         inc ycut
         ld a,(ycut)
         inc a
         ld (ycut),a

;*cont12   lda rectuly
;*         adc #1
;*         bcc cont8
cont12   ld a,c
         inc a
         jr cont8

;*cont3    adc y0
;*         bcs cont6
cont3    ld a,(y0)
         add a,c
         jr c,cont6

;*         cmp #193
;*         bcc cont1
         cp 193
         jr c,cont1

;*cont6    lda #192
;*         inc ycut
cont6    ld a,(ycut)
         inc a
         ld (ycut),a
         ld a,192

;*cont1    sec
;*         sbc rectuly
;*         bcs cont8
cont1    sub c
         jr nc,cont8
 
;*         eor #$ff
;*         adc #1
         xor $ff
         inc a

;*cont8    sta y8pos
;*         sta y8poscp
;*         #assign16 adjcell,crsrtile
;*         jsr ymove
;*         lda ycut
;*         bne cont11
cont8    ld (y8pos),a
         ld (y8poscp),a
         ld iy,(crsrtile)
         call ymove
         ld a,(ycut)
         or a
         jr nz,cont11

;*         jsr xmove
         call xmove
;*cont11   lda x8poscp
;*         sta x8pos
;*         lda y8poscp
;*         sta y8pos
;*         lda crsrbyte
;*         sta y8byte
;*         lda crsrbit
;*         sta x8bit
;*         #assign16 adjcell,crsrtile
;*         jsr xmove
;*         lda xcut
;*         bne exit
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

;*ymove    lda ydir
;*         bne loopup
ymove    ld a,(ydir)
         or a
         jr nz,loopup

;*loopdn   jsr drrect1
;*loop10   jsr pixel11
;*         iny
;*         dec y8pos
;*         beq exit
loopdn   call drrect1
loop10   call pixel11      ;used: de,b
         ld hl,y8pos
         dec (hl)
         ret z

;*         sty y8byte
;*         cpy #8
;*         bne loop10
        ld a,8
        add a,d
        ld d,a
        ld hl,y8byte
        inc (hl)
        ld a,(hl)
        cp 8
        jr nz,loop10
 
;*         ldy #down
;*         jsr nextcell
;*         lda #0
;*         sta y8byte
;*         bpl loopdn
         ld a,down
         call vnextcell
         xor a
         ld (y8byte),a
         jr loopdn

;*loopup   jsr drrect1
loopup   call drrect1
;*loop11   jsr pixel11
;*         dec y8pos
;*         beq exit
loop11   call pixel11
         ld hl,y8pos
         dec (hl)
         ret z

;*         dey
;*         sty y8byte
;*         bpl loop11
         ld a,d
         sub 8
         ld d,a
         ld hl,y8byte
         dec (hl)
         jp p,loop11

;*         ldy #up
;*         jsr nextcell
;*         lda #7
;*         sta y8byte
;*         bpl loopup
         ld a,up
         call vnextcell
         ld a,7
         ld (y8byte),a
         jr loopup

;*exit     rts

;*xmove    lda xdir
;*         bne looplt
xmove    ld a,(xdir)
         or a
         jr nz,looplt

;*looprt   jsr drrect1
looprt   call drrect1
;*loop12   jsr pixel11
;*         dec x8pos
;*         beq exit
         call pixel11
         ld hl,x8pos
         dec (hl)
         ret z

;*         lda x8bit
;*         lsr
;*         bcs nextrt
         ld a,(x8bit)
         rrca
         jr c,nextrt

;*         sta x8bit
;*         txa
;*         lsr
;*         tax
;*         lda x8bit
;*         cmp #8
;*         bne loop12
         ld (x8bit),a
         jr looprt

;*         lda #8
;*         tax
;*         eor i1
;*         sta i1
;*         bne loop12

;*nextrt   ldy #right
;*         jsr nextcell
;*         lda #$80
;*         sta x8bit
;*         bne looprt
nextrt   ld a,right
         call vnextcell
         ld a,$80
         ld (x8bit),a
         jr looprt

;*looplt   jsr drrect1
;*loop15   jsr pixel11
;*         dec x8pos
;*         beq exit
looplt   call drrect1
         ;call crsrsetc
         call pixel11
         ld hl,x8pos
         dec (hl)
         ret z

;*         lda x8bit
;*         asl
;*         bcs nextlt
         ld a,(x8bit)
         rlca
         jr c,nextlt

;*         sta x8bit
;*         txa
;*         asl
;*         tax
;*         lda x8bit
;*         cmp #16
;*         bne loop15
         ld (x8bit),a
         jr looplt

;*         ldx #1
;*         lda i1
;*         sbc #8
;*         sta i1
;*         bcs loop15
;* 
;*nextlt   ldy #left
;*         jsr nextcell
;*         lda #1
;*         sta x8bit
;*         bne looplt
nextlt   ld a,left
         call vnextcell
         ld a,1
         ld (x8bit),a
         jr looplt

;*drrect1  ldy #video
;*         lda (adjcell),y
;*         tax
;*         iny
;*         lda (adjcell),y
;*         sta i1+1
;*         stx i1
;*         ldy y8byte
;*         lda x8bit
;*         and #$f
;*         bne cont14

;*         lda x8bit
;*         lsr
;*         lsr
;*         lsr
;*         lsr
;*         bpl cont15

;*cont14   tax
;*         lda #8
;*         eor i1
;*         sta i1
;*         txa

;*cont15   tax
;*         rts

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

clrrect  proc       ;in: x8poscp, y8poscp
         local cl3,lltpc,lrtpc,lx,lxpc11,lxpc01
         local x8pos,x8poscp,x8bit,y8pos,y8poscp,y8byte,mask,localbase
         local looplt,looprt,loopdn,loopup,looprt1,looplt1,looprtpc,loopltpc
         local xmove,nextrt,nextlt
         local clrect1,clrect2,clrect2pc,clrect3,xclrect
localbase equ $fff0         ;link to drawrect!
x8pos    equ localbase
x8poscp  equ localbase+1    ;link to drawrect!
x8bit    equ localbase+2
y8pos    equ t1
y8poscp  equ localbase+3    ;link to drawrect!
y8byte   equ localbase+4
mask     equ localbase+5

;*         jsr xchgxy
;*         lda y8poscp
;*         sta y8pos
;*         lda crsrbyte
;*         sta y8byte
;*         lda crsrbit
;*         sta x8bit
;*         jsr calcx
;*         and #3
;*         ldx xdir
;*         beq cl3
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

;*         sbc #4
;*         eor #$ff
;*cl3      clc
;*         adc x8poscp
;*         sta x8pos
;*         sta x8poscp
         sub 8
         cpl
cl3      ld b,a
         ld a,(x8poscp)
         add a,b
         ld (x8pos),a
         ld (x8poscp),a

;*         #assign16 adjcell,crsrtile
;*         lda ydir
;*         bne loopup
         ld iy,(crsrtile)
         ld a,(ydir)
         or a
         jr nz,loopup

;*loopdn   jsr xclrect
;*         beq exit
loopdn   call xclrect
         ret z

;*         inc y8byte
;*         lda y8byte
;*         cmp #8
;*         bne loopdn
         ld hl,y8byte
         inc (hl)
         ld a,(hl)
         cp 8
         jr nz,loopdn

;*         ldy #down
;*         jsr nextcell
;*         lda #0
;*         sta y8byte
;*         bpl loopdn
         ld a,down
         call vnextcell
         xor a
         ld (y8byte),a
         jr loopdn

;*loopup   jsr xclrect
;*         beq exit
loopup   call xclrect
         ret z

;*         dec y8byte
;*         bpl loopup
         ld a,(y8byte)
         dec a
         ld (y8byte),a
         jp p,loopup

;*         ldy #up
;*         jsr nextcell
;*         lda #7
;*         sta y8byte
;*         bpl loopup
         ld a,up
         call vnextcell
         ld a,7
         ld (y8byte),a
         jr loopup

;*xclrect  lda adjcell
;*         pha
;*         lda adjcell+1
;*         pha
;*         jsr xmove
;*         pla
;*         sta adjcell+1
;*         pla
;*         sta adjcell
xclrect  push iy
         call xmove
         pop iy

;*         lda x8poscp
;*         sta x8pos
;*         lda crsrbit
;*         sta x8bit
;*         dec y8pos      ;sets ZF
;*exit     rts
         ld a,(x8poscp)
         ld (x8pos),a
         ld a,(crsrbit)
         ld (x8bit),a
         ld hl,y8pos
         dec (hl)       ;sets ZF
         ret

;*xmove    lda xdir
;*         bne looplt
xmove    ld a,(xdir)
         or a
         jr nz,looplt

looprt   call clrect1
         ld a,(pseudoc)
         or a
         ld a,$80
         ld (hl),a
         jr nz,lrtpc

looprt1  call clrect2
         ret c
         ret z

         ld a,(hl)
         srl a
         srl a
         jr z,nextrt

         ld (hl),a
         inc e
         jr looprt1

lrtpc    call clrect3
looprtpc call clrect2pc
         ret c
         ret z

         ld a,(hl)
         srl a
         srl a
         jr z,nextrt

         ld (hl),a
         inc e
         jr looprtpc

;*nextrt   ldy #right
;*         jsr nextcell
;*         lda #$80
;*         sta x8bit
;*         bne looprt
nextrt   ld a,right
         call vnextcell
         jr looprt

looplt   call clrect1
         inc e
         inc e
         inc e
         ld a,(pseudoc)
         or a
         ld a,2
         ld (hl),a
         jr nz,lltpc

looplt1  call clrect2
         ret c
         ret z

         ld a,(hl)
         sla a
         sla a
         jr z,nextlt

         ld (hl),a
         dec e
         jr looplt1

lltpc    call clrect3
loopltpc call clrect2pc
         ret c
         ret z

         ld a,(hl)
         sla a
         sla a
         jr z,nextlt

         ld (hl),a
         dec e
         jr loopltpc

;*nextlt   ldy #left
;*         jsr nextcell
;*         lda #1
;*         sta x8bit
;*         bne looplt
nextlt   ld a,left
         call vnextcell
         jr looplt

clrect1  ld hl,readde
         call calllo
         ld a,(y8byte)
         ld b,a
         rlca
         rlca
         rlca
         add a,d
         ld d,a
         ld a,b
         ld hl,readc
         call calllo
         ld hl,x8bit
         ret

clrect2  rrca
         or (hl)
         and c
         ;ld a,0     ;00
         jr z,lx

         ld a,$c0   ;11
         jp pe,lx
         
         ld a,(hl)
         and c
         ld a,$40   ;01
         jr z,lx

         rlca       ;10
lx       ld (de),a
         ld a,(x8pos)
         sub 2
         ld (x8pos),a
         ret

clrect2pc rrca
         or (hl)
         ld (mask),a
         and c
         ;ld a,0
         jr z,lx
         jp pe,lxpc11
         
         ld a,(hl)
         and c
         jr z,lxpc01

         ld a,(hl)     ;lxpc10
         and b
         ld a,8
         jr z,lx       ;1000

         ld a,$80      ;1010
         jr lx

lxpc11   ld a,(mask)
         and b
         ld a,$c
         jr z,lx       ;1100

         ld a,$c0
         jp pe,lx      ;1111

         ld a,(hl)
         and b
         ld a,$48      ;1101
         jr z,lx

         ld a,$84      ;1110
         jr lx

lxpc01   ld a,(hl)
         rrca
         and b
         ld a,4
         jr z,lx       ;0100

         ld a,$40
         jr lx         ;0101

clrect3  ld a,b
         ld b,c
         add a,pc
         ld hl,readc
         call calllo
         ld a,c
         ld c,b
         ld b,a
         ld hl,x8bit
         ld a,(hl)
         ret
         endp

vnextcell push iy
         pop bc
         ld hl,nextcell
         call calllo
         push bc
         pop iy
         ret

;*showtent .block
showtent proc        ;used: a,bc,de,hl,iy*,ix*
         local loop,l1,l3,exit
;*         lda x0
;*         pha
;*         lda y0
;*         pha
;*         lda #0
;*         sta $14
;*         sta $15
;*         sta ppmode
         ld a,(x0)
         ld b,a
         ld a,(y0)
         ld c,a
         push bc
         xor a
         ld h,a       ;$15
         ld l,a       ;$14
         ld (ppmode),a

;*loop     lda $15
;*         cmp $b9
;*         bne l1
loop     ld a,(memb9)
         cp h
         jr nz,l1

;*         ldx $14
;*         cpx $b8
;*         beq exit
         ld a,(memb8)
         cp l
         jr z,exit

;*l1       eor #8
;*         sta $15
;*         ldx #0
;*         lda ($14,x)
;*         sta x0
;*         lda $15
;*         eor #4
;*         sta $15
;*         lda ($14,x)
;*         sta y0
;*         ora x0
;*         beq l3
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

;*         jsr putpixel
;*l3       lda $15
;*         eor #$c
;*         sta $15
;*         inc $14
;*         bne loop
         ld hl,putpixel
         call calllo
l3       pop hl
         inc hl

;*         inc $15
;*         bne loop
         jr loop

;*exit     pla
;*         sta y0
;*         pla
;*         sta x0
;*         inc ppmode
exit     pop bc
         ld a,c
         ld (y0),a
         ld a,b
         ld (x0),a
         ld a,1
         ld (ppmode),a
;*         rts
;*         .bend
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

;*infov    .block
infov    proc
         local cont1,cont2,loop1
         local sizex

;*         jsr $ff4f
;*         .byte 147,144,0
         call SCR_CLEAR

;*         lda fnlen
;*         beq cont1
         ld a,(fnlen)
         or a
         jr z,cont1
         
;*         jsr $ff4f
;*         .null "last loaded filename: "
         call printn
         db  "Last loaded filename: $"
;*         ldy #0
;*loop1    lda fn,y
;*         jsr $ffd2
;*         iny
;*         cpy fnlen
;*         bne loop1
         ld hl,fn
loop1    ld a,(hl)
         call TXT_OUTPUT
         inc hl
         ld a,"."
         cp (hl)
         jr nz,loop1

;*cont1    sei
;*         sta $ff3f
;*         jsr boxsz
;*         sta $ff3e
;*         cli
;*         beq cont2
cont1    call boxsz
         jr z,cont2

;*xmin     = i1
;*ymin     = i1+1
;*xmax     = adjcell
;*ymax     = adjcell+1
;*sizex    = adjcell2
;*sizey    = adjcell2+1
         ;xmin - d, ymin - e
         ;xmax - b, ymax - c
sizex     equ t1     ;connected to curx at boxsz and savepat
         ;cury - sizey - h

;*         jsr $ff4f
;*         .byte $d
;*         .null "active pattern size: "
         call printn
         db $d,$a,"Active pattern size: $"

;*         lda #0
;*         ldx sizex
;*         jsr $a45f      ;int -> str
;*         lda #"x"
;*         jsr $ffd2
;*         lda #0
         ld a,(sizex)
         call printdec
         ld a,"x"
         call TXT_OUTPUT

;*         ldx sizey
;*         jsr $a45f      ;int -> str
         ld a,h
         call printdec
;*         jsr $ff4f
;*         .byte $d
;*         .null "box life bounds: "
         call printn
         db $d,$a,"Box life bounds: $"

;*         lda #0
;*         ldx xmin
;*         jsr $a45f      ;int -> str
;*         jsr $ff4f
;*         .null "<=x<="
         ld a,d
         call printdec
         call printn
         db "<=X<=$"

;*         lda #0
;*         ldx xmax
;*         jsr $a45f      ;int -> str
;*         lda #" "
;*         jsr $ffd2
         ld a,b
         call printdec
         ld a," "
         call TXT_OUTPUT

;*         lda #0
;*         ldx ymin
;*         jsr $a45f      ;int -> str
;*         jsr $ff4f
;*         .null "<=y<="
         ld a,e
         call printdec
         call printn
         db "<=Y<=$"

;*         lda #0
;*         ldx ymax
;*         jsr $a45f      ;int -> str
         ld a,c
         call printdec

;*cont2    jsr $ff4f
;*         .byte $d
;*         .null "rules: "
cont2    call printn
         db $d,$a,"Rules: $"
;*         jsr showrules2
         call showrules2
;*         jmp getkey
;*         .bend
         jp KM_WAIT_CHAR
         endp

