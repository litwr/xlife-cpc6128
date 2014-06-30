;**boxsz
;**adddensity
;**rndbyte

inccurp  ld a,tilesize
         add a,iyl
         ld iyl,a
         ret nc

         inc iyh
         ret

;*boxsz    .block
boxsz    proc
         local loop0,loop2,loop3,cont2,cont3,cont4,cont5,cont6,cont7,cont8
         local curx

;*xmin     = i1
;*ymin     = i1+1
         ;xmin - d, ymin - e
;*xmax     = adjcell
;*ymax     = adjcell+1
         ;xmax - b, ymax - c
;*curx     = adjcell2
;*cury     = adjcell2+1
curx     equ $fffc     ;connected to infov
         ;cury - h

;*         lda #192
;*         sta ymin
;*         lda #160
;*         sta xmin
         ld de,$a0c0
;*         lda #0
;*         sta xmax
;*         sta ymax
;*         sta curx
;*         sta cury
         xor a
         ld b,a
         ld c,a
         ld (curx),a
         ld h,a

;*         lda #<tiles ;=0
;*         sta currp
;*         lda #>tiles
;*         sta currp+1
         ld iy,tiles

;*loop0    lda #0
;*         ldy #7
;*loop1    ora (currp),y
;*         dey
;*         bpl loop1
loop0    xor a
         ld ix,oriy
         ld (jsrfar+1),ix
         push iy
         call calllo1
         inc iy
         call calllo1
         inc iy
         call calllo1
         inc iy
         call calllo1
         inc iy
         call calllo1
         inc iy
         call calllo1
         inc iy
         call calllo1
         inc iy
         call calllo1
         pop iy

;*         ora #0
;*         beq cont7
         jp z,cont7

;*         pha
         ld ixl,a

;*loop2    asl
;*         iny
;*         bcc loop2
         ld l,$ff
loop2    rlca
         inc l
         jr nc,loop2
         
;*         sty t1
;*         lda curx
;*         asl
;*         asl
;*         asl
;*         tax
;*         adc t1
;*         cmp xmin
;*         bcs cont2
         ld a,(curx)
         rlca
         rlca
         rlca
         ld ixh,a
         add a,l
         cp d
         jr nc,cont2

;*         sta xmin
;*cont2    pla
;*         ldy #8
;*loop3    lsr
;*         dey
;*         bcc loop3
         ld d,a
cont2    ld a,ixl
         ld l,8
loop3    rrca
         dec l
         jr nc,loop3

;*         sty t1
;*         txa
;*         clc
;*         adc t1
;*         cmp xmax
;*         bcc cont3
         ld a,ixh
         add a,l
         cp b
         jr c,cont3
         
;*         sta xmax
;*cont3    ldy #0
;*loop4    lda (currp),y
;*         bne cont4
         ld b,a
cont3    push iy
         xor a
         ld l,a
         call calllo1
         jr nz,cont4

         inc l
         inc iy
         call calllo1
         jr nz,cont4

         inc l
         inc iy
         call calllo1
         jr nz,cont4

         inc l
         inc iy
         call calllo1
         jr nz,cont4

         inc l
         inc iy
         call calllo1
         jr nz,cont4

         inc l
         inc iy
         call calllo1
         jr nz,cont4

         inc l
         inc iy
         call calllo1
         jr nz,cont4

         inc l
         inc iy
         call calllo1

;*         iny
;*         bpl loop4

;*cont4    sty t1
;*         lda cury
;*         asl
;*         asl
;*         asl
;*         tax
;*         adc t1
;*         cmp ymin
;*         bcs cont5
cont4    pop iy
         ld a,h
         rlca
         rlca
         rlca
         ld ixh,a
         add a,l
         cp e
         jr nc,cont5

;*         sta ymin
;*cont5    ldy #7
;*loop5    lda (currp),y
;*         bne cont6
         ld e,a
         push iy
         push de
         ld de,7
         ld l,e
         add iy,de
         pop de
cont5    call calllo1
         jr nz,cont6

         dec l
         dec iy
         call calllo1
         jr nz,cont6

         dec l
         dec iy
         call calllo1
         jr nz,cont6

         dec l
         dec iy
         call calllo1
         jr nz,cont6

         dec l
         dec iy
         call calllo1
         jr nz,cont6

         dec l
         dec iy
         call calllo1
         jr nz,cont6

         dec l
         dec iy
         call calllo1
         jr nz,cont6

         dec l
         dec iy
         call calllo1

;*         dey
;*         bpl loop5

;*cont6    sty t1
;*         txa
;*         clc
;*         adc t1
;*         cmp ymax
;*         bcc cont7
cont6    pop iy
         ld a,ixh
         add a,l
         cp c
         jr c,cont7

;*         sta ymax
;*cont7    jsr inccurrp
;*         ldx curx
;*         inx
;*         cpx #20
;*         beq cont8
         ld c,a
cont7    call inccurp
         ld a,(curx)
         inc a
         cp 20
         jr z,cont8

;*         stx curx
;*         bne loop0
         ld (curx),a
         jp loop0

;*cont8    ldx #0
;*         stx curx
;*         ldy cury
;*         iny
;*         cpy #24
;*         beq cont1
cont8    xor a
         ld (curx),a
         inc h
         ld a,h
         cp 24
         jp nz,loop0
         
;*         sty cury
;*         jmp loop0
         
;*cont1    lda ymax
;*         sbc ymin
;*         adc #0
;*         sta cury
         ld a,c
         sub e
         inc a
         ld h,a

;*         sec
;*         lda xmax
;*         sbc xmin
;*         adc #0
;*         sta curx
         ld a,b
         sub d
         inc a
         ld (curx),a

;*         lda xmax
;*         ora ymax
;*         ora tiles
;*         rts
;*         .bend
         ld a,(tiles)   ;it is at the low mem
         or c
         or b
         ret
         endp

;*adddensity
;*         .block
;*         lda #$ff
;*         sta adjcell2+1
;*         lda density
;*         beq exit

;*loop     lda $ff1e
;*         lsr
;*         lsr
;*         eor $ff04
;*         eor $ff00
;*         and #7
;*         tax
;*         lda bittab,x
;*         eor #$ff
;*         and adjcell2+1
;*         sta adjcell2+1
;*         eor #$ff
;*         tax
;*         lda tab3,x
;*         cmp density
;*         bne loop

;*exit     lda adjcell2
;*         and adjcell2+1
;*         rts
;*         .bend

;*rndbyte  .block
;*         ldy #4
;*         ldx #0
;*         stx adjcell2
;*loop1    stx x0
;*         ldx #4
;*loop2    lda $ff1e
;*         lsr
;*         sta adjcell2+1
;*loop3    lsr adjcell2+1
;*         bne loop3

;*         lsr
;*         lsr
;*         rol x0
;*         lda $ff1e
;*         lsr
;*         lsr
;*         eor $ff02
;*         lsr
;*         rol x0
;*         dex
;*         bne loop2

;*         lda x0
;*         ora adjcell2
;*         sta adjcell2
;*         dey
;*         bne loop1

;*         jsr adddensity
;*         ldy t2
;*         inc t2
;*         ora (adjcell),y
;*         sta (adjcell),y
;*         tax
;*         lda tab3,x
;*         ldy #sum
;*         adc (adjcell),y
;*         sta (adjcell),y
;*         rts
;*         .bend

