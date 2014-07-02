chgtopology macro
         local l2

         ld a,(topology)
         or a
         jr z,l2

         xor a
         ld (topology),a
         ld hl,torus
         call calllo
         ld a,(bordertc)
         jr chgbr

l2       inc a
         ld (topology),a
         ld hl,plain
         call calllo
         ld a,(borderpc)
chgbr    ld b,a
         ld c,a
         jp SCR_SET_BORDER
         endm

dispat2  proc
         local cont2,cont3,cont4,cont5,cont6,cont7,cont8,cont10,cont11,cont12
         local cont14,cont15,cont16,cont16b,cont16c,cont16x,cont17,cont17a
         local cont17b,cont17c,cont17d,cont17e,cont17f,cont17g,cont17h,cont17i
         local cont17j,cont17q,cont17t,cont17w,cont18,cont40,cont41,cont42,cont43,cont44
         local cxdown,cxright,cxleft,cxup,cm4,cm5,contcur1,contcur2,contcur3
         local lsp1,lsp2,l2,l4,l5,l8,l11,cm4v,cm5v,finish,zoomin,zoomout
         local nozoom,exitload,nozoom3

         cp "g"
         jr nz,cont3 

         ld a,(mode)
         or a
         jr z,cont2

         dec a
         ld (mode),a
         jr z,setbg0

l5       call initxt
         call xyout
cont2    ld a,(bggo)
         call setbg
         ld a,1
l4       ld (mode),a
         ret

setbg0   ld a,(bgedit)
setbg    ld c,a
         ld b,a
         xor a
         jp SCR_SET_INK

cont3    cp "Q"
         jr nz,cont5

         ld a,3
         jr l4

cont5    cp "h"
         jr nz,cont4

         ld a,(mode)
         cp 2
         jr z,l5

         ld a,2
         ld (mode),a
         call SCR_CLEAR
setbg1   ld a,(bggo)
         jr setbg

cont4    cp "T"
         jr nz,cont6

         chgtopology

cont6    cp "o"
         jr nz,cont7

         ld a,(mode)
         or a
         ret nz

         ld hl,tilecnt
         or (hl)
         jr nz,l8

         inc hl
         or (hl)
         ret z

l8       call zerocc
         ld hl,generate
         call calllo
         ld hl,showscn
         call calllo
         ld hl,cleanup 
         jp calllo

cont7    cp "?"
         jr nz,cont8

         ld a,(mode)
         cp 2
         ret z

help0    call totext
         call help
         jp finish

cont8    cp "C"
         jr nz,cont10

         ld hl,(tilecnt)
         ld a,h
         or l
         ret z

         ld hl,clear
         jp calllo

cont10   cp "E"
         jr nz,cont11

         ld a,(pseudoc)
         dec a
         jr z,l11

         ld a,1
l11      ld (pseudoc),a
         ld hl,showscn
         jp calllo

cont11   cp "!"
         jr nz,cont12

;*         jsr random
;*         jmp showscn

cont12   cp "%"
         jr nz,cont14

         ld a,(mode)
         cp 2
         ret z

         call totext
         call indens
         jr finish

cont14   cp "B"
         jr nz,cont15

         call totext
         call insteps
         jr z,finish

         call steps2bin
         ld hl,decint
         call calllo
         jr c,finish

         call setbench
         call KL_TIME_PLEASE      ;get timer
         ld (stringbuf),hl
         ld (stringbuf+2),de
         ld hl,bloop
         call calllo
         call KL_TIME_PLEASE      ;get timer
         call calcspd
         call exitbench
         ld hl,calccells
         call calllo
         ;xor a
         ;ld (mode),a
         jr finish

cont15   cp "R"
         jr nz,cont16

         call totext
         call inborn
         jr z,finish

         ld ix,born
         call setrconst
         call instay
         ld ix,live
         call setrconst
         call fillrt
finish   call tograph
         call SCR_CLEAR
         call initxt
         ld hl,showscn
         call calllo
         call showrules
         jp xyout

cont16   cp $f3        ;cursor right
         jr nz,cont16x

         call crsrclr
         ld hl,vptilecx
         inc (hl)
         ld a,(crsrbit)
         cp 1
         jr z,cxright

         rrca
cont17q  ld (crsrbit),a
         jp crsrupd

cxright  ld b,$80
         ld a,right
contcur1 add a,iyl
         ld iyl,a
         ld a,0 
         adc a,iyh
         ld iyh,a
         ld hl,readhl
         call calllo
         ld a,low(plainbox)
         cp l
         jr nz,cm4

         ld a,high(plainbox)
         cp h
         jr nz,cm4

         ld a,(crsrbit)
         jr cont17q

cm4      ld (crsrtile),hl
cm5      ld a,b
         jr cont17q

cont16x  cp $f2        ;cursor left
         jr nz,cont40

         call crsrclr
         ld hl,vptilecx
         dec (hl)
         ld a,(crsrbit)
         cp $80
         jr z,cxleft

         rlca
         jr cont17q

cxleft   ld b,1
         ld a,left
         jr contcur1
;*cxleft   lda #1
;*cm6      ldx #0
;*cm1      sta t1
;*         stx i2
;*         lda (crsrtile),y
;*         tax
;*         iny
;*         lda (crsrtile),y
;*         cmp #>plainbox
;*         bne cm4
;*   
;*         cpx #<plainbox
;*         bne cm4

;*         ldx i2
;*         lda crsrbit,x
;*         sta t1
;*         bcs cm5

;*cm4      sta crsrtile+1
;*         stx crsrtile
;*cm5      lda t1
;*         ldx i2
;*         sta crsrbit,x
;*         jmp crsrset

cont40   cp $f4       ;shifted cursor up
         jr nz,cont41

         ld a,(vptilecy)
         sub 8
         ld (vptilecy),a
         ld c,up
cont42   push bc     ;preserve c!
         call crsrclr
         pop bc
         call shift
         jp crsrupd

cont41   cp $f5       ;shifted cursor down
         jr nz,cont43

         ld a,(vptilecy)
         add a,8
         ld (vptilecy),a  
         ld c,down
         jr cont42

cont43   cp $f6       ;shifted cursor left
         jr nz,cont44

         ld a,(vptilecx)
         sub 8
         ld (vptilecx),a
         ld c,left
         jr cont42

cont44   cp $f7       ;shifted cursor right
         jr nz,cont16b

         ld a,(vptilecx)
         add a,8
         ld (vptilecx),a
         ld c,right
         jr cont42

cont16b  cp $f0      ;cursor up
         jr nz,cont16c

         call crsrclr
         ld hl,vptilecy
         dec (hl)
         ld a,(crsrbyte)
         or a
         jr z,cxup

         dec a
contcur2 ld (crsrbyte),a
         jp crsrupd

;*cxup     lda #7
;*cm3      ldx #1
;*         bpl cm1
cxup     ld b,7
         ld a,up
contcur3 add a,iyl
         ld iyl,a
         ld a,0 
         adc a,iyh
         ld iyh,a
         ld hl,readhl
         call calllo
         ld a,low(plainbox)
         cp l
         jr nz,cm4v

         ld a,high(plainbox)
         cp h
         jr nz,cm4v

         ld a,(crsrbyte)
         jr contcur2

cm4v     ld (crsrtile),hl
cm5v     ld a,b
         jr contcur2
         
cont16c  cp $f1         ;cursor down
         jr nz,cont17

         call crsrclr
         ld hl,vptilecy
         inc (hl)
         ld a,(crsrbyte)
         cp 7
         jr z,cxdown

         inc a
         jr contcur2

cxdown   ld b,0
         ld a,down
         jr contcur3

cont17   cp 32          ;space
         jr nz,cont17c

;*         #assign16 adjcell,crsrtile
;*         jsr chkadd
         ld bc,(crsrtile)
         ld hl,chkadd
         call calllo

;*         ldy crsrbyte
;*         lda (crsrtile),y
;*         eor crsrbit
;*         sta (crsrtile),y
;*         ldy #sum
;*         and crsrbit
;*         beq lsp1
         ld a,(crsrbyte)
         add a,c
         ld iyl,a
         ld a,b
         adc a,0
         ld iyh,a
         ld a,(crsrbit)
         ld d,a
         ld hl,xoriy
         call calllo
         push bc
         pop iy
         and d
         jr z,lsp1

;*         clc
;*         lda (crsrtile),y
;*         adc #1
;*         sta (crsrtile),y
;*         lda #1
;*         jsr inctsum
;*lsp2     lda zoom
;*         beq lsp3
         ld hl,inciy
         call calllo
         ld a,1
         ld hl,inctsum
         call calllo
lsp2     ld a,(zoom)
         or a
         push af
         call z,crsrclr
         pop af

;*         jsr showscnpg
         ld hl,showscnpg
         call nz,calllo

;*lsp3     jsr infoout
;*         jmp crsrset
         ld hl,infoout
         call calllo
         ld hl,crsrset
         jp calllo

;*lsp1     sec
;*         lda (crsrtile),y
;*         sbc #1
;*         sta (crsrtile),y
;*         jsr dectsum
;*         bne lsp2
lsp1     ld hl,deciy
         call calllo
         ld hl,dectsum
         call calllo
         jr lsp2

cont17c  cp "."
         jr nz,cont17f

;*         jsr crsrclr
;*         lda #<tiles+(tilesize*249)
;*         sta crsrtile
;*         lda #>tiles+(tilesize*249)
;*         sta crsrtile+1
;*         lda #1
;*         sta crsrbyte
;*cont17t  sta crsrbit
         call crsrclr
         ld hl,tiles+(tilesize*249)
         ld (crsrtile),hl
         ld a,1
         ld (crsrbyte),a
cont17t  ld (crsrbit),a
         ld a,(zoom)
         or a
         jr z,crsrupd

;*         jsr setviewport
;*         jsr showscnpg
;*cont17u  jmp crsrset
crsrupd  ld hl,crsrset
         call calllo
         jp crsrcalc

cont17f  cp "H"           ;home
         jr nz,cont17a

         call crsrclr
         ld hl,tiles
         ld (crsrtile),hl
         xor a
         ld (crsrbyte),a
         ld a,$80
         jr cont17t

cont17a  cp "l"
         jr nz,cont17b

;*         lda zoom
;*         pha
;*         beq nozoom1
         ld a,(zoom)
         or a
         push af
         jr z,nozoom

;*         jsr zoomout
;*nozoom   jsr totext
;*         jsr loadmenu
;*         beq exitload
         call zoomout
nozoom   call totext
         call loadmenu
         jr z,exitload

;*cont17w  jsr loadpat
;*exitload jsr tograph
;*         jsr showrules
;*         jsr calccells
;*         jsr showscn
;*         pla
;*         bne zoomin
cont17w  call loadpat
exitload ld hl,calccells
         call calllo
         call finish
         pop af
         jr nz,zoomin 

;*         rts
         ret

cont17b  cp "L"
         jr nz,cont17d

;*         lda fnlen
;*         bne cont17v
         ld a,(fnlen)
         or a
         ret z

;*exit     rts

;*cont17v  lda zoom
;*         pha
;*         beq nozoom3
         ld a,(zoom)
         or a
         push af
         jr z,nozoom3

;*         jsr zoomout
;*nozoom3  jsr totext
;*         lda #147
;*         jsr $ffd2
;*         jmp cont17w
         call zoomout
nozoom3  call totext
         jr cont17w

cont17d  cp "+"
         jr nz,cont17e

zoomin   ;call crsrclr
         call split_off
         ld a,1
         ld (zoom),a
         call SCR_CLEAR
         call setviewport
         jp finish

cont17e  cp "-"
         jr nz,cont17g

zoomout  call split_on
         xor a
         ld (zoom),a
         jp finish

cont17g  cp "V"
         jr nz,cont17h

;*         jsr totext
;*         jsr $ff4f
;*         .byte 144,147,0
         call totext
         call SCR_CLEAR

;*         jsr curoff
;*         jsr showcomm
         call showcomm
;*         jmp finish
         jp finish

cont17h  cp "v"
         jr nz,cont17i

;*         jsr totext
;*         jsr curoff
;*         jsr infov
;*         jmp finish
         call totext
         call infov
         jp finish

cont17i  cp "Z"
         jr nz,cont17j

         call totext
         call chgcolors
l2       call setcolor
         jp finish

cont17j  cp "X"
         jr nz,cont18

         call totext
         call loadcf
         jr l2

cont18   cp "S"
         ret nz

;*         jsr boxsz
;*         beq cont20
         call boxsz
         ret z

         push bc
         push de
         push hl
;*         jsr totext
         call totext
;*         jsr getsvfn
         call getsvfn
;*         beq exitsave
         pop hl
         pop de
         pop bc
         jp z,finish

;*         jsr savepat
         call savepat
;*exitsave jmp finish
         jp finish

;*cont20   clc
;*         rts
         endp

;*shift    lda $543   ;shift st
;*         beq cont20

;*         lda (crsrtile),y
;*         tax
;*         iny
;*         lda (crsrtile),y
;*         dey
;*         cmp #>plainbox
;*         bne cm4x
;*   
;*         cpx #<plainbox
;*         beq cont20

;*cm4x     sta crsrtile+1
;*         stx crsrtile
;*         sec
;*         rts
;*         .bend
shift      proc     ;in: c - direction
           local cont
           ld iy,(crsrtile)
           ld b,0
           add iy,bc
           ld hl,readhl
           call calllo
           ld a,l
           cp low(plainbox)
           jr nz,cont

           ld a,h
           cp high(plainbox)
           ret z

cont       ld (crsrtile),hl
           ret           
           endp

setbench call SCR_CLEAR
         ld a,(mode)
         ld (temp+1),a
         ld a,2
         ld (mode),a
         ld a,(pseudoc)
         ld (temp),a
         xor a
         ld (pseudoc),a
         ret

exitbench ld a,(temp+1)
         ld (mode),a
         ld a,(temp)
         ld (pseudoc),a
         ret

