;*savepat  .block
;*sizex    = adjcell2
;*sizey    = adjcell2+1
;*xmin     = i1
;*ymin     = i1+1
;*curx     = adjcell
;*cury     = adjcell+1
;*         lda #8
;*         jsr io2
;*         ldy svfnlen
;*         lda #","
;*         sta svfn,y
;*         sta svfn+2,y
;*         lda #"u"
;*         sta svfn+1,y
;*         lda #"w"
;*         sta svfn+3,y
;*         tya
;*         clc
;*         adc #4
;*         ldx #<svfn
;*         ldy #>svfn
;*         jsr $ffbd    ;setnam
;*         jsr $ffc0    ;open file
;*         ldx #8
;*         jsr $ffc9    ;open channel for write
;*         bcs error

;*         jsr $ffb7    ;read st
;*         bne error

;*         lda sizex
;*         jsr $ffd2
;*         jsr $ffb7    ;read st
;*         bne error

;*         lda sizey
;*         jsr $ffd2
;*         ldy #0
;*loop1    jsr $ffb7    ;read st
;*         bne error

;*         lda live,y
;*         jsr $ffd2
;*         iny
;*         cpy #4
;*         bne loop1
;*         
;*         lda #0
;*         sta curx
;*         sta cury
;*         lda #<tiles ;=0
;*         sta currp
;*         lda #>tiles
;*         sta currp+1
;*loop0    ldy #0
;*loop2    sei
;*         sta $ff3f
;*         lda (currp),y
;*         sta $ff3e
;*         cli
;*         bne cont1

;*loop4    iny
;*         cpy #8
;*         bne loop2

;*         jsr inccurrp
;*         inc curx
;*         ldx curx
;*         cpx #20
;*         bne loop0

;*         ldx #0
;*         stx curx
;*         inc cury
;*         ldy cury
;*         cpy #24
;*         bne loop0
;*         beq eof

;*error    jsr $ffcc
;*         jsr showds
;*eof      jmp endio

;*cont1    ldx #$ff
;*loop3    inx
;*         asl
;*         bcs cont4
;*         beq loop4
;*         bcc loop3
;*         
;*cont4    sta i2
;*         stx t1
;*         jsr $ffb7    ;read st
;*         bne error

;*         lda curx
;*         asl
;*         asl
;*         asl
;*         adc t1
;*         sec
;*         sbc xmin
;*         jsr $ffd2
;*         jsr $ffb7    ;read st
;*         bne error

;*         sty t1
;*         lda cury
;*         asl
;*         asl
;*         asl
;*         adc t1
;*         sec
;*         sbc ymin
;*         jsr $ffd2
;*         lda i2
;*         jmp loop3
;*         .bend

;*showcomm .block
;*         ldx fnlen
;*         bne cont2

;*exit1    rts

;*cont2    lda fn-1,x
;*         cmp #"*"
;*         beq exit1

;*         lda #"#"
;*         cpx #16
;*         beq cont1

;*         inx
;*cont1    sta fn-1,x
;*         ;lda #","     ;check file type
;*         ;inx
;*         ;sta fn-1,x
;*         ;lda #"s"
;*         ;inx
;*         ;sta fn-1,x
;*         txa
;*         ldx #<fn
;*         ldy #>fn
;*         .bend

