;*clear    .block
;*         jsr zerocc
;*         #inibcd gencnt,6
;*         #assign16 currp,startp
clear    proc
;clear all occupied tiles
         local cont1,cont2,loop,loop0,lnext
         call zerocc
         inibcd gencnt,6
         ld iy,(startp)
;*loop     ldy #sum
;*         lda (currp),y
;*         beq lnext
loop     ld a,(iy+sum)
         or a
         jr z,lnext

;*         lda #0
;*         sta (currp),y
;*         ldy #7
;*loop0    sta (currp),y
;*         dey
;*         bpl loop0
         xor a
         ld (iy+sum),a
         push iy
         ld b,8
loop0    ld (iy),a
         ld (iy+pc),a
         inc iy
         djnz loop0
         pop iy

;*         ldy #pc
;*loop1    sta (currp),y
;*         iny
;*         cpy #pc+8
;*         bne loop1

;*lnext    ldy #next
;*         lda (currp),y
;*         tax
;*         iny
;*         lda (currp),y
;*         bne cont1
;*
;*         cpx #1
;*         beq cont2
lnext    ld l,(iy+next)
         ld h,(iy+next+1)
         ld a,h
         or a
         jr nz,cont1

         ld a,l
         dec a
         jr z,cont2

;*cont1    sta currp+1
;*         stx currp
;*         jmp loop
cont1   push hl
        pop iy
        jr loop

;*cont2    jsr showscn
;*         jsr cleanup0
;*         jmp infoout
;*         .bend
cont2    call showscn
         call cleanup0
         jp infoout
         endp

;*fixcnt2  lda tab20,x
;*         adc (currp),y
;*         sta (currp),y
;*         lda tab21,x
;*         iny
;*         adc (currp),y
;*         sta (currp),y  
;*         lda tab22,x
;*         iny
;*         adc (currp),y
;*         sta (currp),y 
;*         lda tab23,x
;*         iny
;*         adc (currp),y
;*         sta (currp),y
;*         rts
fixcnt2  add a,c       ;in: a, bc, de
         ld c,a        ;chg: a, bc, hl
         ld a,0
         adc a,b
         ld b,a
         ld hl,tab20
         add hl,de
         ld a,(bc)
         add a,(hl)
         ld (bc),a
         ld hl,tab21
         add hl,de
         inc bc
         ld a,(bc)
         add a,(hl)
         ld (bc),a
         ld hl,tab22
         add hl,de
         inc bc
         ld a,(bc)
         add a,(hl)
         ld (bc),a
         ld hl,tab23
         add hl,de
         inc bc
         ld a,(bc)
         add a,(hl)
         ld (bc),a
         ret

;*fixcnt1e lda tab13,x
;*         adc (adjcell),y
;*         sta (adjcell),y
;*         lda tab12,x
;*         dey
;*         adc (adjcell),y
;*         sta (adjcell),y
;*         lda tab11,x
;*         dey
;*         adc (adjcell),y
;*         sta (adjcell),y
;*         lda tab10,x
;*         dey
;*         adc (adjcell),y
;*         sta (adjcell),y
;*         rts
fixcnt1x add a,c       ;in: a, bc, de
         ld c,a        ;chg: a, bc, hl
         ld a,0
         adc a,b
         ld b,a
         ld hl,tab13
         add hl,de
         ld a,(bc)
         add a,(hl)
         ld (bc),a
         ld hl,tab12
         add hl,de
         dec bc
         ld a,(bc)
         add a,(hl)
         ld (bc),a
         ld hl,tab11
         add hl,de
         dec bc
         ld a,(bc)
         add a,(hl)
         ld (bc),a
         ld hl,tab10
         add hl,de
         dec bc
         ld a,(bc)
         add a,(hl)
         ld (bc),a
         ret

;*chkaddt  lda t1
;*         bne chkadd

;*exit2    rts

chkaddt    ld a,(t1)
           or a
           ret z

;*chkadd   ldy #next
;*         lda (adjcell),y
;*         iny
;*         ora (adjcell),y
;*         bne exit2
chkadd     ld a,next    ;in: bc - adjcell or adjcell2
           add a,c      ;used: a, hl, de
           ld l,a
           ld a,0
           adc a,b
           ld h,a
           ld a,(hl)
           inc hl
           or (hl)
           ret nz

;*addnode  .block
;*         dey
;*         lda startp
;*         sta (adjcell),y
;*         iny
;*         lda startp+1
;*         sta (adjcell),y
;*         #assign16 startp,adjcell
;*         inc tilecnt
;*         bne exit

;*         inc tilecnt+1
;*exit     rts
;*         .bend
addnode  ld de,(startp)
         ld (hl),d
         dec hl
         ld (hl),e
         ld (startp),bc
         ld hl,(tilecnt)
         inc hl
         ld (tilecnt),hl
         ret

;*inctiles .block
;*         clc
;*         lda i1
;*         adc #tilesize
;*         sta i1
;*         bcc l1

;*         inc i1+1
;*l1       rts
;*         .bend
inctiles ld de,tilesize
         add iy,de
         ret

;*random   .block
;**uses: adjcell:2, adjcell2:2, i1:2, i2, t1, t2, t3, x0
;*         lda #<tiles+((hormax*4+3)*tilesize)  ;start random area
;*         sta adjcell
;*         lda #>tiles+((hormax*4+3)*tilesize)
;*         sta adjcell+1
;*         lda #0     ;dir: 0 - left, 1 - right
;*         sta t1
;*         lda #right
;*         sta i1+1
;*         lda #16    ;ver rnd max
;*         sta i1
;*         lda #14    ;hor rnd max
;*         sta i2
;*cont3    ldy #sum
;*         lda #0
;*         sta t2
;*         sta (adjcell),y
;*         lda #8
;*         sta t3
;*loop1    jsr rndbyte
;*         dec t3
;*         bne loop1

;*         jsr chkadd
;*         dec i2
;*         beq cont2

;*         ldy i1+1
;*cont4    lda (adjcell),y
;*         tax
;*         iny
;*         lda (adjcell),y
;*         stx adjcell
;*         sta adjcell+1
;*         bne cont3

;*cont2    dec i1
;*         beq cont5

;*         lda #14    ;hor rnd max
;*         sta i2
;*         lda t1
;*         ldy #left
;*         eor #1
;*         sta t1
;*         bne cont1

;*         ldy #right
;*cont1    sty i1+1
;*         ldy #down
;*         bne cont4

;*cont5
;*         .bend

calccells proc
         local cont1,loop2,loop4
         ld hl,(tilecnt)
         ld a,l
         or h
         ret z

         call zerocc
         ld iy,(startp)
loop2    push iy
         pop ix
         ld (ix+sum),0
         ld c,8
loop4    ld a,(iy)
         add a,low(tab3)
         ld l,a
         ld a,high(tab3)
         adc a,0
         ld h,a
         ld a,(hl)
         ld e,a
         ld a,(ix+sum)
         add a,e
         ld (ix+sum),a
         ld a,e
         call inctsum
         inc iy
         dec c
         jr nz,loop4

         ld l,(ix+next)
         ld h,(ix+next+1)
         ld a,h
         or a
         jr nz,cont1

         ld a,l
         dec a
         jp z,infoout

cont1    push hl
         pop iy
         jr loop2
         endp

inctsum  cellsum  ;in: A
         ret

;*dectsum  .block
;*         ldx #4
;*loop     dec cellcnt,x
;*         lda cellcnt,x
;*         cmp #$2f
;*         bne exit

;*         lda #$39
;*         sta cellcnt,x
;*         dex
;*         bpl loop

;*exit     rts         ;ZF=0
;*         .bend
dectsum  proc
         local loop
         ld hl,cellcnt+4
         ld b,5
loop     dec (hl)
         ld a,(hl)
         inc a
         ret nz

         ld (hl),9
         dec hl
         djnz loop
         ret
         endp

putpixel proc   ;in: x0,y0,xdir,ydir,xchgdir
         local loop1,loop2,loop3,cont1,cont2,cont3,cont4,cont5,cont7,cont8
         local cup,cdown,cleft,cright,m1
;x8pos  - ixl; x8bit - ixh; y8pos - d; y8byte - e; adjcell - bc
;*         jsr xchgxy
         call xchgxy

;*         ldx #8
;*         lda crsrbit
;*loop1    dex
;*         lsr
;*         bcc loop1
         ld b,8
         ld a,(crsrbit)
loop1    dec b
         rrca
         jr nc,loop1

;*         stx m1+1
;*         lda crsrx
;*         lsr
;*         asl
;*         asl
;*         asl
;*m1       adc #0
;*         ldx xdir
;*         beq cont4
         ld a,(crsrx)
         srl a
         rlca
         rlca
         rlca
         add a,b
         ld b,a
         ld a,(xdir)
         or a
         ld a,(x0)
         jr z,cont4

;*         sec
;*         sbc x0
;*         bcc exit
;*         bcs cont2
         ld d,a
         ld a,b
         sub d
         ret c

         jr cont2

;*cont4    adc x0
;*         bcs exit
;*
;*         cmp #160
;*         bcs exit
cont4    add a,b
         ret c

         cp 160
         ret nc

;*cont2    sta x8pos
;*         lda crsry
;*         asl
;*         asl
;*         asl
;*         adc crsrbyte
;*         ldx ydir
;*         beq cont3
cont2    ld ixl,a
         ld a,(crsry)
         rlca
         rlca
         rlca
         ld b,a
         ld a,(crsrbyte)
         add a,b
         ld b,a
         ld a,(ydir)
         or a
         ld a,(y0)
         jr z,cont3

;*         sec
;*         sbc y0
;*         bcc exit
;*         bcs cont1
         ld d,a
         ld a,b
         sub d
         ret c

         jr cont1

;*cont3    adc y0
;*         bcs exit
;*
;*         cmp #192
;*         bcc cont1
cont3    add a,b
         ret c

         cp 192
         ret nc

;*cont1    sta y8pos
;*         and #7
;*         sta y8byte
;*         lda y8pos
;*         lsr
;*         lsr
;*         lsr
;*         sec
;*         sbc crsry
;*         sta y8pos
;*         lda x8pos
;*         and #7
;*         sta x8bit
;*         lda crsrx
;*         lsr
;*         sta $e1
;*         lda x8pos
;*         lsr
;*         lsr
;*         lsr
;*         sec
;*         sbc $e1
;*         sta x8pos
cont1    ld d,a
         and 7
         ld e,a
         ld a,d
         and $f8
         rrca
         rrca
         rrca
         ld b,a
         ld a,(crsry)
         neg
         add a,b
         ld d,a
         ld a,ixl
         and 7
         ld ixh,a
         ld a,(crsrx)
         srl a
         ld b,a
         ld a,ixl
         and $f8
         rrca
         rrca
         rrca
         sub b
         ld ixl,a

;*         #assign16 adjcell,crsrtile
         ld bc,(crsrtile)

;*         lda y8pos
;*loop2    bmi cup
;*         bne cdown
         ld a,d
         or a
loop2    jp m,cup
         jr nz,cdown

;*         lda x8pos
;*loop3    bmi cleft
;*         bne cright
         ld a,ixl
         or a
loop3    jp m,cleft
         jp nz,cright

;*+         ldy ppmode
;*+         bne putpixel3
;*+         jmp putpixel2
;*         jsr chkadd	;uses adjcell!
;*         lda #7
;*         sec
;*         sbc x8bit
;*         tay
;*         lda bittab,y
;*         ldy y8byte
;*         ora (adjcell),y
;*         sta (adjcell),y
;*         sta $ff3e
;*         cli
;*         rts
         ld a,7
         sub ixh
         ld iy,bittab
         ld (m1+2),a
m1       ld a,(iy)
         ld d,a
         ld a,(ppmode)
         or a
         jr nz,cont5

;*putpixel2 .block
;*         tax
;*         jsr seti1
;*         txa
;*         and #$f
;*         beq l1

;*         tax
;*         lda i1
;*         eor #8
;*         sta i1
;*         bne l2

;*l1       txa
;*         lsr
;*         lsr
;*         lsr
;*         lsr
;*         tax
;*l2       lda vistab,x
;*         sta t2
;*         asl
;*         sta t3
;*         ora t2
;*         eor #$ff
;*         and (i1),y
;*         ora t3
;*         sta (i1),y
;*         rts
;*         .bend
         ld ixh,d
         push bc
         pop iy
         ld a,e      ;y8byte
         ld hl,readde
         call calllo
         rlca
         rlca
         rlca
         add a,d
         ld d,a
         ld a,ixh   ;x8bit count?
         ld c,a
         and $c0
         jr z,cont7

cont8    ld b,$88
         and $aa
         ret nz

         ld b,$44
         ld a,b
         ld (de),a
         ret

cont7    inc de
         ld a,c
         and $30
         jr nz,cont8

         inc de
         ld a,c
         and $c
         jr nz,cont8

         inc de
         ld a,c
         jr cont8

cont5    ld a,e      ;y8byte
         add a,c
         ld l,a
         ld a,b
         adc a,0
         ld h,a
         or (hl)
         ld (hl),d
         jp chkadd

;*cright   ldy #right     ;y=0, x=/=0
;*         jsr nextcell
;*         dec x8pos
;*         bpl loop3
cright   ld a,right
         call nextcell
         dec ixl
         jr loop3

;*cdown    ldy #down      ;y=/=0
;*         jsr nextcell
;*         dec y8pos
;*         bpl loop2
cdown    ld a,down
         call nextcell
         dec d        ;y8pos
         jr loop2

;*cup      ldy #up       ;y=/=0
;*         jsr nextcell
;*         inc y8pos  
;*         jmp loop2
cup      ld a,up
         call nextcell
         inc d       ;y8pos
         jr loop2

;*cleft    ldy #left      ;y=0, x=/=0
;*         jsr nextcell
;*         inc x8pos
;*         jmp loop3
cleft    ld a,left
         call nextcell
         inc ixl
         jr loop3
         endp

;*nextcell lda (adjcell),y
;*         tax
;*         iny
;*         lda (adjcell),y
;*         sta adjcell+1
;*         stx adjcell
;*         rts
nextcell add a,c       ;in: a, bc; changed: a, hl; set: bc
         ld l,a
         ld a,b
         adc a,0
         ld h,a
         ld c,(hl)
         inc hl
         ld b,(hl)
         ret

;*torus    .block
torus    proc
         local l5,l4,l3,l2
;*         jsr totiles     ;top border
         ld iy,tiles
;*         ldx #hormax
         ld b,hormax
;*l5       ldy #ul
;*         lda i1
;*         clc
;*         adc #<(hormax*(vermax-1)-1)*tilesize
;*         sta (i1),y
;*         lda i1+1
;*         adc #>(hormax*(vermax-1)-1)*tilesize
;*         iny
;*         sta (i1),y
l5       ld de,(hormax*(vermax-1)-1)*tilesize
         push iy
         pop hl
         add hl,de
         ld (iy+ul),l
         ld (iy+ul+1),h
;*         lda i1
;*         adc #<hormax*(vermax-1)*tilesize
;*         iny		;up
;*         sta (i1),y
;*         lda i1+1
;*         adc #>hormax*(vermax-1)*tilesize
;*         iny
;*         sta (i1),y
         ld de,hormax*(vermax-1)*tilesize
         push iy
         pop hl
         add hl,de
         ld (iy+up),l
         ld (iy+up+1),h
;*         lda i1
;*         adc #<(hormax*(vermax-1)+1)*tilesize
;*         iny		;ur
;*         sta (i1),y
;*         lda i1+1
;*         adc #>(hormax*(vermax-1)+1)*tilesize
;*         iny
;*         sta (i1),y
         ld de,(hormax*(vermax-1)+1)*tilesize
         push iy
         pop hl
         add hl,de
         ld (iy+ur),l
         ld (iy+ur+1),h
;*         jsr inctiles
         call inctiles
;*         dex
;*         bne l5
         djnz l5

;*         lda #<tiles+((vermax-1)*hormax*tilesize)  ;bottom border
;*         sta i1
;*         lda #>tiles+((vermax-1)*hormax*tilesize)
;*         sta i1+1
           ld iy,tiles+((vermax-1)*hormax*tilesize)
;*         ldx #hormax
           ld b,hormax
;*l4       ldy #dr
;*         lda i1
;*         sec
;*         sbc #<((vermax-1)*hormax-1)*tilesize
;*         sta (i1),y
;*         lda i1+1
;*         sbc #>((vermax-1)*hormax-1)*tilesize
;*         iny
;*         sta (i1),y
l4       ld de,(~(((vermax-1)*hormax-1)*tilesize))+1
         push iy
         pop hl
         add hl,de
         ld (iy+dr),l
         ld (iy+dr+1),h
;*         lda i1
;*         sbc #<(vermax-1)*hormax*tilesize
;*         iny		;down
;*         sta (i1),y
;*         lda i1+1
;*         sbc #>(vermax-1)*hormax*tilesize
;*         iny
;*         sta (i1),y
         ld de,(~((vermax-1)*hormax*tilesize))+1
         push iy
         pop hl
         add hl,de
         ld (iy+down),l
         ld (iy+down+1),h
;*         lda i1
;*         sbc #<((vermax-1)*hormax+1)*tilesize
;*         iny		;dl
;*         sta (i1),y
;*         lda i1+1
;*         sbc #>((vermax-1)*hormax+1)*tilesize
;*         iny
;*         sta (i1),y
         ld de,(~(((vermax-1)*hormax+1)*tilesize))+1
         push iy
         pop hl
         add hl,de
         ld (iy+dl),l
         ld (iy+dl+1),h
;*         jsr inctiles
         call inctiles
;*         dex
;*         bne l4
         djnz l4

;*         jsr totiles    ;left border
         ld iy,tiles
;*         ldx #vermax
         ld b,vermax
;*l3       ldy #left
;*         lda i1
;*         clc
;*         adc #<(hormax-1)*tilesize
;*         sta (i1),y
;*         lda i1+1
;*         adc #>(hormax-1)*tilesize
;*         iny
;*         sta (i1),y
l3       ld de,(hormax-1)*tilesize
         push iy
         pop hl
         add hl,de
         ld (iy+left),l
         ld (iy+left+1),h
;*         lda i1
;*         sec
;*         sbc #<tilesize
;*         iny		;ul
;*         sta (i1),y
;*         lda i1+1
;*         sbc #>tilesize
;*         iny
;*         sta (i1),y
         ld de,(~tilesize)+1
         push iy
         pop hl
         add hl,de
         ld (iy+ul),l
         ld (iy+ul+1),h
;*         lda i1
;*         clc
;*         adc #<(2*hormax-1)*tilesize
;*         ldy #dl
;*         sta (i1),y
;*         lda i1+1
;*         adc #>(2*hormax-1)*tilesize
;*         iny
;*         sta (i1),y
         ld de,(2*hormax-1)*tilesize
         push iy
         pop hl
         add hl,de
         ld (iy+dl),l
         ld (iy+dl+1),h
;*         lda i1
;*         adc #<tilesize*hormax
;*         sta i1
;*         lda i1+1
;*         adc #>tilesize*hormax
;*         sta i1+1
         ld de,hormax*tilesize
         add iy,de
;*         dex
;*         bne l3
         djnz l3

;*         lda #<tiles+((hormax-1)*tilesize)  ;right border
;*         sta i1
;*         lda #>tiles+((hormax-1)*tilesize)
;*         sta i1+1
         ld iy,tiles+((hormax-1)*tilesize)
;*         ldx #vermax
         ld b,vermax
;*l2       ldy #ur
;*         lda i1
;*         sec
;*         sbc #<(2*hormax-1)*tilesize
;*         sta (i1),y
;*         lda i1+1
;*         sbc #>(2*hormax-1)*tilesize
;*         iny
;*         sta (i1),y
l2       ld de,(~((2*hormax-1)*tilesize))+1
         push iy
         pop hl
         add hl,de
         ld (iy+ur),l
         ld (iy+ur+1),h
;*         lda i1
;*         sec
;*         sbc #<(hormax-1)*tilesize
;*         iny		;right
;*         sta (i1),y
;*         lda i1+1
;*         sbc #>(hormax-1)*tilesize
;*         iny
;*         sta (i1),y
         ld de,(~((hormax-1)*tilesize))+1
         push iy
         pop hl
         add hl,de
         ld (iy+right),l
         ld (iy+right+1),h
;*         lda i1
;*         clc
;*         adc #<tilesize
;*         iny		;dr
;*         sta (i1),y
;*         lda i1+1
;*         adc #>tilesize
;*         iny
;*         sta (i1),y
         ld de,tilesize
         push iy
         pop hl
         add hl,de
         ld (iy+dr),l
         ld (iy+dr+1),h
;*         lda i1
;*         adc #<tilesize*hormax
;*         sta i1
;*         lda i1+1
;*         adc #>tilesize*hormax
;*         sta i1+1
         ld de,hormax*tilesize
         add iy,de
;*         dex
;*         bne l2
         djnz l2

;*         ldy #ul    ;top left corner
;*         lda #<tiles + ((hormax*vermax-1)*tilesize)
;*         sta tiles,y
;*         lda #>tiles + ((hormax*vermax-1)*tilesize)
;*         iny
;*         sta tiles,y
         ld hl,tiles+ul
         ld (hl),low(tiles + ((hormax*vermax-1)*tilesize))
         inc hl
         ld (hl),high(tiles + ((hormax*vermax-1)*tilesize))

;*         ldy #ur    ;top right corner
;*         lda #<tiles+(hormax*(vermax-1)*tilesize)
;*         sta tiles+((hormax-1)*tilesize),y
;*         lda #>tiles+(hormax*(vermax-1)*tilesize)
;*         iny
;*         sta tiles+((hormax-1)*tilesize),y
         ld hl,tiles+((hormax-1)*tilesize)+ur
         ld (hl),low(tiles+(hormax*(vermax-1)*tilesize))
         inc hl
         ld (hl),high(tiles+(hormax*(vermax-1)*tilesize))

;*         ldy #dl   ;bottom left corner
;*         lda #<tiles+((hormax-1)*tilesize)
;*         sta tiles+(hormax*(vermax-1)*tilesize),y
;*         lda #>tiles+((hormax-1)*tilesize)
;*         iny
;*         sta tiles+(hormax*(vermax-1)*tilesize),y
         ld hl,tiles+(hormax*(vermax-1)*tilesize)+dl
         ld (hl),low(tiles+((hormax-1)*tilesize))
         inc hl
         ld (hl),high(tiles+((hormax-1)*tilesize))

;*         ldy #dr   ;bottom right corner
;*         lda #<tiles
;*         sta tiles+((vermax*hormax-1)*tilesize),y
;*         lda #>tiles
;*         iny
;*         sta tiles+((vermax*hormax-1)*tilesize),y
         ld hl,tiles+((vermax*hormax-1)*tilesize)+dr
         ld (hl),low(tiles)
         inc hl
         ld (hl),high(tiles)
;*         rts
;*         .bend
         ret
         endp

;*plain    .block
plain    proc
         local l5,l4,l3,l2
;*         jsr totiles     ;top border
         ld iy,tiles
;*         ldx #hormax
         ld b,hormax
         ld hl,plainbox
;*l5       ldy #ul
;*         lda #<plainbox
;*         sta (i1),y
;*         lda #>plainbox
;*         iny
;*         sta (i1),y
l5       ld (iy+ul),l
         ld (iy+ul+1),h
;*         lda #<plainbox
;*         iny		;up
;*         sta (i1),y
;*         lda #>plainbox
;*         iny
;*         sta (i1),y
         ld (iy+up),l
         ld (iy+up+1),h
;*         lda #<plainbox
;*         iny		;ur
;*         sta (i1),y
;*         lda #>plainbox
;*         iny
;*         sta (i1),y
         ld (iy+ur),l
         ld (iy+ur+1),h
;*         jsr inctiles
         call inctiles
;*         dex
;*         bne l5
         djnz l5

;*         lda #<tiles+((vermax-1)*hormax*tilesize)  ;bottom border
;*         sta i1
;*         lda #>tiles+((vermax-1)*hormax*tilesize)
;*         sta i1+1
;*         ldx #hormax
         ld iy,tiles+((vermax-1)*hormax*tilesize)
         ld b,hormax
;*l4       ldy #dr
;*         lda #<plainbox
;*         sta (i1),y
;*         lda #>plainbox
;*         iny
;*         sta (i1),y
l4       ld (iy+dr),l
         ld (iy+dr+1),h
;*         lda #<plainbox
;*         iny		;down
;*         sta (i1),y
;*         lda #>plainbox
;*         iny
;*         sta (i1),y
         ld (iy+down),l
         ld (iy+down+1),h
;*         lda #<plainbox
;*         iny		;dl
;*         sta (i1),y
;*         lda #>plainbox
;*         iny
;*         sta (i1),y
         ld (iy+dl),l
         ld (iy+dl+1),h
;*         jsr inctiles
;*         dex
;*         bne l4
         call inctiles
         djnz l4

;*         jsr totiles    ;left border
;*         ldx #vermax
         ld iy,tiles
         ld b,vermax
         ld de,tilesize*hormax
;*l3       ldy #left
;*         lda #<plainbox
;*         sta (i1),y
;*         lda #>plainbox
;*         iny
;*         sta (i1),y
l3       ld (iy+left),l
         ld (iy+left+1),h
;*         lda #<plainbox
;*         iny		;ul
;*         sta (i1),y
;*         lda #>plainbox
;*         iny
;*         sta (i1),y
         ld (iy+ul),l
         ld (iy+ul+1),h
;*         lda #<plainbox
;*         ldy #dl
;*         sta (i1),y
;*         lda #>plainbox
;*         iny
;*         sta (i1),y
         ld (iy+dl),l
         ld (iy+dl+1),h
;*         lda i1
;*         adc #<tilesize*hormax
;*         sta i1
;*         lda i1+1
;*         adc #>tilesize*hormax
;*         sta i1+1
         add iy,de
;*         dex
;*         bne l3
         djnz l3

;*         lda #<tiles+((hormax-1)*tilesize)  ;right border
;*         sta i1
;*         lda #>tiles+((hormax-1)*tilesize)
;*         sta i1+1
;*         ldx #vermax
         ld iy,tiles+((hormax-1)*tilesize)
         ld b,vermax
;*l2       ldy #ur
;*         lda #<plainbox
;*         sta (i1),y
;*         lda #>plainbox
;*         iny
;*         sta (i1),y
l2       ld (iy+ur),l
         ld (iy+ur+1),h
;*         lda #<plainbox
;*         iny		;right
;*         sta (i1),y
;*         lda #>plainbox
;*         iny
;*         sta (i1),y
         ld (iy+right),l
         ld (iy+right+1),h
;*         lda #<plainbox
;*         iny		;dr
;*         sta (i1),y
;*         lda #>plainbox
;*         iny
;*         sta (i1),y
         ld (iy+dr),l
         ld (iy+dr+1),h
;*         lda i1
;*         adc #<tilesize*hormax
;*         sta i1
;*         lda i1+1
;*         adc #>tilesize*hormax
;*         sta i1+1
         add iy,de
;*         dex
;*         bne l2
         djnz l2

;*         rts
;*         .bend
         ret
         endp

