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

nexthld  macro data
         ld (hl),data
         ld a,h
         add a,8
         ld h,a
         dec l
         endm

nexthll  macro data
         ld (hl),data
         inc l
         endm

nexthlds macro
         ld (hl),a
         ld a,h
         add a,8
         ld h,a
         dec l
         endm

nexthlls macro
         xor a
         ld (hl),a
         inc l
         endm

nexthlc  macro
         ld a,(hl)
         xor $f
         ld (hl),a
         inc l
         ld a,(hl)
         xor $f
         endm

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

showscnpg proc
         local loop1,loop2,loop3,loop4,cont1,cont2,cont2a,cont4,cont5,cont6,cont12
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
loop2    ld d,(ix+pc)
         ld e,(ix)
         ld b,8
loop1    rlc d              ;pseudocolor
         sla e
         jp nc,cont1

         nexthll 3
         nexthld $c
         nexthll 7
         nexthld $e
         ld a,(pseudoc)
         or a
         jr z,cont12

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

showscn  call infoout
         ld a,(zoom)
         or a
         jp nz,showscnpg

         ld hl,(tilecnt)
         ld a,h
         or l
         jp z,crsrset

xcont2   ld a,(pseudoc)
         or a
         jp nz,showscnp

showscn2 proc      ;must be after showscn
         local loop
;*         #assign16 currp,startp
         ld c,8
         ld hl,(startp)
;*loop     ldy #video
;*         lda (currp),y
;*         sta i1
;*         iny
;*         lda (currp),y
;*         sta i1+1
loop     push hl
         pop iy
         ld e,(iy+video)
         ld d,(iy+video+1)
;*         ldy #0
;*         #vidmac1
         call vidmac
;*         iny
;*         #vidmac1
         inc hl
         ld a,d
         add a,c
         ld d,a
         call vidmac
;*         iny
;*         #vidmac1
         inc hl
         ld a,d
         add a,c
         ld d,a
         call vidmac
;*         iny
;*         #vidmac1
         inc hl
         ld a,d
         add a,c
         ld d,a
         call vidmac
;*         iny
;*         #vidmac1
         inc hl
         ld a,d
         add a,c
         ld d,a
         call vidmac
;*         iny
;*         #vidmac1
         inc hl
         ld a,d
         add a,c
         ld d,a
         call vidmac
;*         iny
;*         #vidmac1
         inc hl
         ld a,d
         add a,c
         ld d,a
         call vidmac
;*         iny
;*         #vidmac1
         inc hl
         ld a,d
         add a,c
         ld d,a
         call vidmac
;*         lda #8
;*         eor i1
;*         sta i1
;*         ldy #0
;*         #vidmac2
;*         iny
;*         #vidmac2
;*         iny
;*         #vidmac2
;*         iny
;*         #vidmac2
;*         iny
;*         #vidmac2
;*         iny
;*         #vidmac2
;*         iny
;*         #vidmac2
;*         iny
;*         #vidmac2
;*         ldy #next
;*         lda (currp),y
;*         tax
;*         iny
;*         lda (currp),y
;*         bne cont
         ld l,(iy+next)
         ld h,(iy+next+1)
         xor a
         or h
         jp nz,loop
;*         cpx #1
;*         bne cont
         ld a,l
         dec a
         jp z,crsrset
         jp loop
         
;*         jmp crsrset
         
;*cont     sta currp+1
;*         stx currp
;*         jmp loop
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
;*         #assign16 currp,startp
         ld iy,(startp)
         ld ix,pctable
         ld l,8
;*loop     ldy #video
;*         lda (currp),y
;*         sta i1
;*         iny
;*         lda (currp),y
;*         sta i1+1
loop     ld e,(iy+video)
         ld d,(iy+video+1)
;*         ldy #pc
;*         sty t1
;*         ldy #0
;*         #vidmac1p
;*         iny
;*         inc t1
         call vidmacp
         inc iy
         ld a,d
         add a,l
         ld d,a
;*         #vidmac1p
;*         iny
;*         inc t1
         call vidmacp
         inc iy
         ld a,d
         add a,l
         ld d,a
;*         #vidmac1p
;*         iny
;*         inc t1
         call vidmacp
         inc iy
         ld a,d
         add a,l
         ld d,a
;*         #vidmac1p
;*         iny
;*         inc t1
         call vidmacp
         inc iy
         ld a,d
         add a,l
         ld d,a
;*         #vidmac1p
;*         iny
;*         inc t1
         call vidmacp
         inc iy
         ld a,d
         add a,l
         ld d,a
;*         #vidmac1p
;*         iny
;*         inc t1
         call vidmacp
         inc iy
         ld a,d
         add a,l
         ld d,a
;*         #vidmac1p
;*         iny
;*         inc t1
         call vidmacp
         inc iy
         ld a,d
         add a,l
         ld d,a
;*         #vidmac1p
         call vidmacp
;*         lda #8
;*         eor i1
;*         sta i1
;*         ldy #0
;*         #vidmac2p
;*         iny
;*         #vidmac2p
;*         iny
;*         #vidmac2p
;*         iny
;*         #vidmac2p
;*         iny
;*         #vidmac2p
;*         iny
;*         #vidmac2p
;*         iny
;*         #vidmac2p
;*         iny
;*         #vidmac2p
;*         ldy #next
;*         lda (currp),y
;*         tax
;*         iny
;*         lda (currp),y
;*         bne cont
         ld c,(iy+next-7)
         ld a,(iy+next-6)
         ld iyh,a
         ld iyl,c
         or a
         jp nz,loop
;*         cpx #1
;*         bne cont
;*         jmp crsrset
         dec c
         jp nz,loop

;*cont     sta currp+1
;*         stx currp
;*         jmp loop
         jp crsrset
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

;*getsvfn  .block
;*scrfn    = $c00+40
;*         jsr $ff4f
;*         .byte 147,30
;*         .text "enter filename or press "
;*         .byte 28
;*         .text "esc"
;*         .byte 30
;*         .text " to exit"
;*         .byte 144,$d,0
;*loop3    ldy #0
;*         sty $ff0c
;*loop1    tya
;*         clc
;*         adc #<scrfn
;*         sta $ff0d
;*         jsr getkey
;*         cmp #27
;*         bne cont7
;*         
;*         jsr curoff
;*         ldy #0
;*         sta svfnlen
;*         rts

;*cont7    cmp #$d
;*         beq cont1

;*         cmp #$14   ;backspace
;*         beq cont2

;*         cmp #32
;*         bcc loop1

;*         cpy #15    ;fn length limit
;*         beq loop1

;*         sta svfn,y
;*loop8    jsr $ffd2
;*         iny
;*         bpl loop1

;*cont1    sty svfnlen    
;*         jmp curoff

;*cont2    dey
;*         bmi loop3

;*         dey
;*         jmp loop8
;*         .bend

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
         ld hl,$201
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

;*infov    .block
;*         jsr $ff4f
;*         .byte 147,144,0

;*         lda fnlen
;*         beq cont1
;*         
;*         jsr $ff4f
;*         .null "last loaded filename: "
;* 
;*         ldy #0
;*loop1    lda fn,y
;*         jsr $ffd2
;*         iny
;*         cpy fnlen
;*         bne loop1

;*cont1    sei
;*         sta $ff3f
;*         jsr boxsz
;*         sta $ff3e
;*         cli
;*         beq cont2

;*xmin     = i1
;*ymin     = i1+1
;*xmax     = adjcell
;*ymax     = adjcell+1
;*sizex    = adjcell2
;*sizey    = adjcell2+1
;*         jsr $ff4f
;*         .byte $d
;*         .null "active pattern size: "

;*         lda #0
;*         ldx sizex
;*         jsr $a45f      ;int -> str
;*         lda #"x"
;*         jsr $ffd2
;*         lda #0

;*         ldx sizey
;*         jsr $a45f      ;int -> str
;*         jsr $ff4f
;*         .byte $d
;*         .null "box life bounds: "

;*         lda #0
;*         ldx xmin
;*         jsr $a45f      ;int -> str
;*         jsr $ff4f
;*         .null "<=x<="

;*         lda #0
;*         ldx xmax
;*         jsr $a45f      ;int -> str
;*         lda #" "
;*         jsr $ffd2
;*         lda #0
;*         ldx ymin
;*         jsr $a45f      ;int -> str
;*         jsr $ff4f
;*         .null "<=y<="

;*         lda #0
;*         ldx ymax
;*         jsr $a45f      ;int -> str
;*cont2    jsr $ff4f
;*         .byte $d
;*         .null "rules: "
;*         jsr showrules2
;*         jmp getkey
;*         .bend

;*tograph0 lda #$18
;*         ora ntscmask
;*         sta $ff07
;*         lda #$3b
;*         sta $ff06
;*         lda #$18
;*         sta $ff14
;*         lda #$c8
;*         sta $ff12
;*         sei
;*         sta $ff3f
;*         lda #<irq194
;*         sta $fffe
;*         lda #194
;*         sta $ff0b
;*         cli
;*         rts

vidmac   proc       ;in: hl, de; changed: b
         local m1,m2,m3,m4
         ld ix,bitable
         ld a,(hl)
         rlca
         rlca
         ld b,a
         and 3
         ld (m1+2),a
m1       ld a,(ix)
         ld (de),a
         ld a,b
         rlca
         rlca
         ld b,a
         inc e
         and 3
         ld (m2+2),a
m2       ld a,(ix)
         ld (de),a
         ld a,b
         rlca
         rlca
         inc e
         and 3
         ld (m3+2),a
m3       ld a,(ix)
         ld (de),a
         ld a,(hl)
         inc e
         and 3
         ld (m4+2),a
m4       ld a,(ix)
         ld (de),a
         dec e
         dec e
         dec e
         ret
         endp

vidmacp  proc   ;in: iy,ix,de; changed: b,c,h
         local m1,m2,m3,m4
         ld a,(iy)
         ld h,a
         rlca
         rlca
         and 3
         ld c,a
         ld a,(iy+pc)
         ld b,a
         rlca
         rlca
         rlca
         rlca
         and $c
         or c
         ld (m1+2),a
m1       ld a,(ix)
         ld (de),a

         inc e
         ld a,h
         rrca
         rrca
         rrca
         rrca
         and 3
         ld c,a
         ld a,b
         rrca
         rrca
         and $c
         or c
         ld (m2+2),a
m2       ld a,(ix)
         ld (de),a

         inc de
         ld a,h
         rrca
         rrca
         and 3
         ld c,a
         ld a,b
         and $c
         or c
         ld (m3+2),a
m3       ld a,(ix)
         ld (de),a

         inc de
         ld a,h
         and 3
         ld c,a
         ld a,b
         rlca
         rlca
         and $c
         or c
         ld (m4+2),a
m4       ld a,(ix)
         ld (de),a
         dec e
         dec e
         dec e
         ret
         endp

