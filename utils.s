;**zerocc
;**boxsz
;**adddensity
;**rndbyte

zerocc   inibcd cellcnt,4
         ret

;*boxsz    .block
;*xmin     = i1
;*ymin     = i1+1
;*xmax     = adjcell
;*ymax     = adjcell+1
;*curx     = adjcell2
;*cury     = adjcell2+1
;*         lda #192
;*         sta ymin
;*         lda #160
;*         sta xmin
;*         lda #0
;*         sta xmax
;*         sta ymax
;*         sta curx
;*         sta cury
;*         lda #<tiles ;=0
;*         sta currp
;*         lda #>tiles
;*         sta currp+1
;*loop0    lda #0
;*         ldy #7
;*loop1    ora (currp),y
;*         dey
;*         bpl loop1

;*         ora #0
;*         beq cont7

;*         pha
;*loop2    asl
;*         iny
;*         bcc loop2

;*         sty t1
;*         lda curx
;*         asl
;*         asl
;*         asl
;*         tax
;*         adc t1
;*         cmp xmin
;*         bcs cont2

;*         sta xmin
;*cont2    pla
;*         ldy #8
;*loop3    lsr
;*         dey
;*         bcc loop3

;*         sty t1
;*         txa
;*         clc
;*         adc t1
;*         cmp xmax
;*         bcc cont3
;*         
;*         sta xmax
;*cont3    ldy #0
;*loop4    lda (currp),y
;*         bne cont4

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

;*         sta ymin
;*cont5    ldy #7
;*loop5    lda (currp),y
;*         bne cont6

;*         dey
;*         bpl loop5

;*cont6    sty t1
;*         txa
;*         clc
;*         adc t1
;*         cmp ymax
;*         bcc cont7

;*         sta ymax
;*cont7    jsr inccurrp
;*         ldx curx
;*         inx
;*         cpx #20
;*         beq cont8

;*         stx curx
;*         bne loop0

;*cont8    ldx #0
;*         stx curx
;*         ldy cury
;*         iny
;*         cpy #24
;*         beq cont1

;*         sty cury
;*         jmp loop0
;*         
;*cont1    lda ymax
;*         sbc ymin
;*         adc #0
;*         sta cury
;*         sec
;*         lda xmax
;*         sbc xmin
;*         adc #0
;*         sta curx
;*         lda xmax
;*         ora ymax
;*         ora tiles
;*         rts
;*         .bend

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

