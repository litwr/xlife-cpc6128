;*tographx jsr tograph0
;*         jmp restbl

calcx    proc        ;$80 -> 0, $40 -> 1, ...
         local cl2
         ld b,$ff
cl2      inc b
         rlca
         jr nc,cl2

         ld a,b
         ret
         endp

crsrpg   xor a
         ld (i1),a
         push hl
         ld a,h
         sub 8
         ld h,a
         dec hl
         dec l
         ld a,(crsrpgmk)
         or a
         jr z,clrcur

         ld a,$f
         ld (hl),a
         inc l
         nexthlds
         rept 6
         nexthlc
         nexthlds
         endm
         ld a,$f
         ld (hl),a
         inc l
         ld (hl),a
         pop hl         
         ret

clrcur   nexthlls
         nexthlds
         ld a,h
         add a,48
         ld h,a
         xor a
         ld (hl),a
         inc l
         ld (hl),a
         pop hl         
         ret

showscnz proc
         local loop1,loop2,loop3,loop4,cont1,cont2,cont2a,cont4,cont5,cont6
;use: i1:2, temp:1
;ylimit - iyh, xlimit - iyl
         ld ix,(viewport)
         xor a
         ld (i1),a
         ld a,(crsrbyte)
         ld b,a
         ld a,8
         sub b
         ld (i1+1),a
         ld a,(crsrbit)
         call calcx
         ld a,8
         sub b
         ld (temp),a
         ld hl,$c800
         ld iyh,3
         ld a,(pseudoc)
         or a
         jp nz,showscnzp

loop3    ld iyl,5
loop4    ld a,(crsrtile)
         cp ixl
         jp nz,cont4

         ld a,(crsrtile+1)
         cp ixh
         jr nz,cont4

         ld a,1
         ld (i1),a
cont4    ld c,8
loop2    ld e,(ix)
         ld b,8
loop1    sla e
         jp nc,cont1

         nexthll 3
         nexthld $c
         nexthll 7
         nexthld $e
         nexthll 7     ;live cell char
         nexthld $e
         nexthll 7
         nexthld $e
cont2    nexthll 7
         nexthld $e
         nexthll 3
         ld (hl),$c
cont2a   ld a,h
         sub 40
         ld h,a
cont6    inc hl
         ld a,(i1)
         dec a
         jr nz,cont5

         ld a,(i1+1)
         cp c
         jr nz,cont5

         ld a,(temp)
         cp b
         call z,crsrpg
cont5    djnz loop1

         inc ix
         ld de,80-16
         add hl,de
         dec c
         jp nz,loop2 

         ld de,(~(80*8-16))+1
         add hl,de
         ld de,tilesize-8
         add ix,de
         dec iyl
         jp nz,loop4

         dec iyh
         jp z,crsrset

         ld de,tilesize*15
         add ix,de
         ld de,560
         add hl,de
         jp loop3

cont1    xor a
         cp (hl)     ;is it empty cell?
         ld (hl),a
         inc hl
         jp z,cont6

         rept 5
         nexthlds
         nexthlls
         endm
         ld (hl),a
         jp cont2a
         endp

showscnzp proc
         local m1,m2,m3,m4,loop1,loop2,loop3,loop4
         local cont1,cont2,cont2a,cont4,cont5,cont6,cont8,cont12
;use: i1:2, temp:1
;ylimit - iyh, xlimit - iyl
loop3    ld iyl,5
loop4    ld a,(crsrtile)
         cp ixl
         jp nz,cont4

         ld a,(crsrtile+1)
         cp ixh
         jr nz,cont4

         ld a,1
         ld (i1),a
cont4    xor a
         ld (m1+2),a
         ld a,count0
         ld (loop2+2),a
         inc a
         ld (m2+2),a
         inc a
         ld (m3+2),a
         inc a
         ld (m4+2),a
         ld c,8
loop2    ld a,(ix)
         and $c0
         ld d,a
m2       ld a,(ix)
         rlca
         and $30
         or d
         ld d,a
m3       ld a,(ix)
         rrca
         and $c
         or d
         ld d,a
m4       ld a,(ix)
         and 3
         or d
         ld d,a
m1       ld e,(ix)
         ld b,8
loop1    rlc d              ;pseudocolor
         sla e
         jp nc,cont1

         nexthll 3
         nexthld $c
         nexthll 7
         nexthld $e
         ld a,d
         rrca
         jr c,cont12
     
         nexthll 6     ;new cell char
         nexthld 6
         nexthll 6
         nexthld 6
         jp cont2

cont12   nexthll 7     ;live cell char
         nexthld $e
         nexthll 7
         nexthld $e
cont2    nexthll 7
         nexthld $e
         nexthll 3
         ld (hl),$c
cont2a   ld a,h
         sub 40
         ld h,a
cont6    inc hl
         ld a,(i1)
         dec a
         jr nz,cont5

         ld a,(i1+1)
         cp c
         jr nz,cont5

         ld a,(temp)
         cp b
         call z,crsrpg
cont5    djnz loop1

         dec c
         jr z,cont8

         ld a,(m1+2)
         inc a
         ld (m1+2),a
         rlca
         rlca
         add a,count0
         ld (loop2+2),a
         inc a
         ld (m2+2),a
         inc a
         ld (m3+2),a
         inc a
         ld (m4+2),a
         ld de,80-16
         add hl,de
         jp loop2

cont8    ld de,(~(80*7))+1
         add hl,de
         ld de,tilesize
         add ix,de
         dec iyl
         jp nz,loop4

         dec iyh
         jp z,crsrset

         ld de,tilesize*15
         add ix,de
         ld de,560
         add hl,de
         jp loop3

cont1    xor a
         cp (hl)     ;is it empty cell?
         ld (hl),a
         inc hl
         jp z,cont6

         rept 5
         nexthlds
         nexthlls
         endm
         ld (hl),a
         jp cont2a
         endp

showscn  call infoout
         ld a,(zoom)
         or a
         jp nz,showscnz

         ld hl,(tilecnt)
         ld a,h
         or l
         jp z,crsrset

xcont2   ld a,(pseudoc)
         or a
         jp nz,showscnp

showscn2 proc      ;must be after showscn
         local loop
         ld c,$c0
         ld hl,(startp)
         ld a,h
loop     ld iyh,a
         ld a,l
         ld iyl,a
         ld e,(iy+video)
         ld d,(iy+video+1)
         vidmac
         vidmaca
         vidmac
         vidmaca
         vidmac
         vidmaca
         vidmac
         vidmaca
         vidmac
         vidmaca
         vidmac
         vidmaca
         vidmac
         vidmaca
         vidmac
         ld h,(iy+next+1)
         xor a
         or h
         jp z, crsrset

         ld l,(iy+next)
         jp loop
         endp

vidmacx  proc
         ld c,$c0
         ld a,(crsrbyte)
         add a,iyl
         ld l,a
         ld a,iyh
         adc a,0
         ld h,a
         vidmac
         ret
         endp

infoout  proc
         ld hl,gencnt
         ld b,7
         ld de,$c782
         call digiout

         ld hl,cellcnt
         ld b,5
         ld de,$c792
         call digiout
         jp showtinfo
         endp

showscnp proc
         local loop
         ld iy,(startp)
         ld h,high(pctable)
         ld c,3
loop     ld e,(iy+video)
         ld d,(iy+video+1)
         vidmacp 0,count0
         vidmacpa
         vidmacp 1,count1
         vidmacpa
         vidmacp 2,count2
         vidmacpa
         vidmacp 3,count3
         vidmacpa
         vidmacp 4,count4
         vidmacpa
         vidmacp 5,count5
         vidmacpa
         vidmacp 6,count6
         vidmacpa
         vidmacp 7,count7
         ld a,(iy+next+1)
         or a
         jp z,crsrset

         ld b,(iy+next)
         ld iyh,a
         ld iyl,b
         jp loop
         endp

vidmacpx proc   ;in: iy,de
         local m0,m1,m2,m3,m4
         ld c,3
         ld a,(crsrbyte)
         ld (m0+2),a
         rlca
         rlca
         add a,count0
         ld (m1+2),a
         inc a
         ld (m2+2),a
         inc a
         ld (m3+2),a
         inc a
         ld (m4+2),a
         ld h,high(pctable)
m0       ld a,(iy)
         ld b,a
         rlca
         rlca
         and c
         ld l,a
m1       ld a,(iy)
         rlca
         rlca
         rlca
         rlca
         and $c
         or l
         ld l,a
         ld a,(hl)
         ld (de),a

         inc e
         ld a,b
         rrca
         rrca
         rrca
         rrca
         and c
         ld l,a
m2       ld a,(iy)
         rrca
         and $c
         or l
         ld l,a
         ld a,(hl)
         ld (de),a

         inc e
         ld a,b
         rrca
         rrca
         and c
         ld l,a
m3       ld a,(iy)
         rrca
         and $c
         or l
         ld l,a
         ld a,(hl)
         ld (de),a

         inc e
         ld a,b
         and c
         ld l,a
m4       ld a,(iy)
         and $c
         or l
         ld l,a
         ld a,(hl)
         ld (de),a
         ret
         endp

;*showtinfo
;*         .block
;*         lda tilecnt
;*         sta t1
;*         lda tilecnt+1
;*         lsr
;*         ror t1
;*         lsr
;*         ror t1
;*         ldx t1
;*         cpx #120
;*         bne cont1

;*         ldx #$31
;*         stx tcscr
;*         dex
;*         stx tcscr+1
;*         stx tcscr+2
;*         rts

;*cont1    lda #$20
;*         sta tcscr
;*         sta tcscr+1
;*         lda ttab,x
;*         tax
;*         and #$f
;*         eor #$30
;*         sta tcscr+2
;*         txa
;*         lsr
;*         lsr
;*         lsr
;*         lsr
;*         beq exit

;*         eor #$30
;*         sta tcscr+1
;*exit     rts 
;*         .bend
showtinfo  proc
           local cont1,cont2
           ld hl,(tilecnt)
           srl h
           rr l
           srl h
           rr l
           ld a,l
           cp 120
           jr nz,cont1

           ld a,1
           ld (tinfo),a
           ld hl,0
           ld (tinfo+1),hl
           jp cont2

cont1      ld hl,$0a0a
           ld (tinfo),hl
           ld hl,ttab
           add a,l
           ld l,a
           ld a,0
           adc a,h
           ld h,a
           ld a,(hl)
           and $f
           ld (tinfo+2),a
           ld a,(hl)
           and $f0
           rrca
           rrca
           rrca
           rrca
           jr z,cont2

           ld (tinfo+1),a
cont2      ld b,3
           ld hl,tinfo
           ld de,$c79e
           jp digiout
           endp

xchgxy   proc
         ld a,(xchgdir)
         or a
         ret z

         ld a,(x0)
         ld c,a
         ld a,(y0)
         ld (x0),a
         ld a,c
         ld (y0),a
         ret
         endp

;*crsrset1 .block
;*         ldy #video
;*         lda (crsrtile),y
;*         sta i1
;*         iny
;*         lda (crsrtile),y
;*         sta i1+1
;*         ldx crsrc
;*         ldy crsrbyte
;*         lda (crsrtile),y
;*         and crsrbit
;*         bne cont3
crsrset1 proc   ;out: b - bitmask, de - curpos
         local cont2,cont3
         ld ix,(crsrtile)
         ld e,(ix+video)
         ld d,(ix+video+1)
         ld a,(crsrbyte)
         rlca
         rlca
         rlca
         add a,d
         ld d,a

;*         ldx crsrocc
;*cont3    stx $ff16
;*         lda crsrbit
;*         and #$f
;*         bne cont2

;*         lda crsrbit
;*         lsr
;*         lsr
;*         lsr
;*         lsr
;*         bpl cont1

;*cont2    tax
;*         clc
;*         lda #8
;*         adc i1
;*         sta i1
;*         txa
;*cont1    tax
;*         rts
;*         .bend
         ld a,(crsrbit)
         ld hl,$201          ;it is the b-input for crsrcalc
xcont1   ld c,a
         and $c0
         jr z,cont2

cont3    ld b,h
         and $aa
         ret nz

         ld b,l
         ret

cont2    inc de
         ld a,c
         and $30
         jr nz,cont3

         inc de
         ld a,c
         and $c
         jr nz,cont3

         inc de
         ld a,c
         jr cont3
         endp

;*pixel11  lda vistab,x
;*         asl
;*         ora vistab,x
;*         ora (i1),y
;*         sta (i1),y
;*         rts
pixel11  ld a,(de)
         or b
         ld (de),a
         ret
;pixel11  proc
;         local l1,l2
;         ld a,$aa
;         and b
;         ld a,(de)
;         jr z,l1
;
;         and $55
;l2       or b
;         ld (de),a
;         ret
;
;l1       and $aa
;         jr l2
;         endp

;*crsrset0 jsr crsrset1
;*         lda vistab,x
;*         asl
;*         eor (i1),y
;*         sta (i1),y
;*         rts
crsrset  call crsrset1
         ld a,(zoom)
         or a
         ret nz

         jp pixel11

clrrectlo  proc       ;in: x8poscp, y8poscp
         local lltpc,lrtpc,lx,lxpc11,lxpc01,m7,m8
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
         call vnextcelllo
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
         call vnextcelllo
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
         call vnextcelllo
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
         call vnextcelllo
         jr looplt

clrect1  ld e,(iy+video)
         ld d,(iy+video+1) 
         ld a,(y8byte)
         ld b,a
         rlca
         rlca
         rlca
         add a,d
         ld d,a
         ld a,b
         ld (m8+2),a
m8       ld c,(iy)
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
         ;??add a,pc
         ld (m7+2),a
m7       ld a,(iy)
         ld c,b
         ld b,a
         ld hl,x8bit
         ld a,(hl)
         ret
         endp

