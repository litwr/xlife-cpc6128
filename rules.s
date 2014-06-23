;live, born - word
;fillrt
;setrconst

fillrt   proc
         local l1,l2,l3,l5,l12,l22,l32,loop0,loop1,loop12
         local m1,m2,lnext,lnext2,fillrta
;*         ldy #0
;*         lda #$f0   ;beq
;*         jsr fillrta
;*         ldy #2
;*         lda #$d0   ;bne
         ld ix,live
         ld e,0
         ld a,$28     ;jr z
         call fillrta
         ld ix,born
         ld e,2
         ld a,$20     ;jr nz
;*fillrta  sta m1
;*         sta m2
;*         sty t1
fillrta  ld (m1),a
         ld (m2),a
;*         lda #<gentab
;*         sta adjcell
;*         lda #>gentab
;*         sta adjcell+1
         ld iy,gentab

;*         lda #0
;*         sta i1
;*         sta i1+1
         xor a
         ld h,a
         ld l,a

;*loop0    lda t1
;*         bne l5
loop0    ld a,e
         or a
         jr nz,l5
 
;*         sta (adjcell),y
         ld (iy),a

;*l5       lda i1+1
;*         and #1
;*m1       beq lnext
l5       ld a,h
         and 1
m1       jr z,lnext

;*         lda i1
;*         and #$f
;*         cmp #8
;*         beq l1
;*         bcs lnext
         ld a,l
         and $f
         cp 8
         jr z,l1
         jr nc,lnext

;*         tay
;*         lda #1
;*         cpy #0
;*         beq l2
         ld d,a
         or a
         ld a,1
         jr z,l2

;*loop1    asl
;*         dey
;*         bne loop1
loop1    sla a
         dec d
         jr nz,loop1

;*l2       ldy t1
;*         and live,y
;*         beq lnext
l2       and (ix)
         jr z,lnext

;*l3       ldy #0
;*         lda (adjcell),y
;*         ora #1
;*         sta (adjcell),y
;*lnext    lda i1+1
;*         and #2
;*m2       beq lnext2
l3       ld a,(iy)
         or 1
         ld (iy),a
lnext    ld a,h
         and 2
m2       jr z,lnext2

;*         lda i1
;*         lsr
;*         lsr
;*         lsr
;*         lsr
;*         cmp #8
;*         beq l12
;*         bcs lnext2
         ld a,l
         srl a
         srl a
         srl a
         srl a
         cp 8
         jr z,l12
         jr nc,lnext2

;*         tay
;*         lda #1
;*         cpy #0
;*         beq l22
         ld d,a
         or a
         ld a,1
         jr z,l22

;*loop12   asl
;*         dey
;*         bne loop12
loop12   sla a
         dec d
         jr nz,loop12

;*l22      ldy t1
;*         and live,y
;*         beq lnext2
l22      and (ix)
         jr z,lnext2

;*l32      ldy #0
;*         lda (adjcell),y
;*         ora #2
;*         sta (adjcell),y
l32      ld a,(iy)
         or 2
         ld (iy),a

;*lnext2   inc i1
;*         inc adjcell
;*         bne l4
lnext2   inc l
         inc iy

;*         inc i1+1
;*         inc adjcell+1
;*         bne loop0

;*l4       lda i1
;*         cmp #$89
;*         bne loop0
         ld a,l
         cp $89
         jr nz,loop0

         ld l,0
         inc h
         ld a,iyl
         add a,$77
         ld iyl,a
         ld a,iyh
         adc a,0
         ld iyh,a
;*         lda i1+1
;*         cmp #3
;*         bne loop0
         ld a,h
         cp 4
         jr nz,loop0 

;*         rts       ;ZF=1
         ret

;*l1       lda #1
;*         ldy t1
;*         and live+1,y
;*         beq lnext
;*         bne l3
l1       ld a,1
         and (ix+1)
         jr z,lnext
         jr l3

;*l12      lda #1
;*         ldy t1
;*         and live+1,y
;*         beq lnext2
;*         bne l32
l12      ld a,1
         and (ix+1)
         jr z,lnext2
         jr l32
         endp

setrconst proc        ;in: ix, l
        local cont1,cont2,loop1,loop2

        ld de,stringbuf
        xor a
        ld (ix),a
        ld (ix+1),a
loop2   dec l
        ret m

        ld a,(de)
        inc de
        xor $30
        cp 8
        jr z,cont2

        ld b,a
        or a
        ld a,1
        jr z,cont1

loop1   rlca
        djnz loop1

cont1   or (ix)
        ld (ix),a
        jr loop2

cont2   ld (ix+1),1
        jr loop2
        endp

showrules proc
        local loop1,loop2,loop3,loop4,loop5,cont1,cont2,cont4,cont5,showr0,showr1
        ld hl,$1519
        call TXT_SET_CURSOR
        ld a,2
        call TXT_SET_PEN
        call printn
        db "        $"
        ld a,21
        call TXT_SET_COLUMN
        ld ix,live
        ld b,0

;*        lda #1
;*loop1   bit live
;*        bne cont1
        ld a,1
loop1   ld c,a
        and (ix)
        ld a,c
        jr nz,cont1

loop2   sla a
        jr nz,loop1

        ld a,(ix+1)
        or a
        jr z,cont4

;*        lda #"8"
;*        jsr showr1
;*        beq cont3
        ld a,8
        call showr1
        ret z

;*cont4   lda #"/"
;*        jsr showr1
;*        beq cont3
cont4   ld a,"/"
        call showr1
        ret z

;*        lda #1
;*loop4   bit born
;*        bne cont5
        ld a,1
loop4   ld c,a
        and (ix+2)
        ld a,c
        jr nz,cont5

;*loop5   asl
;*        bne loop4
loop5   sla a
        jr nz,loop4

;*        lda born+1
;*        beq cont3
        ld a,(ix+3)
        or a
        ret z

;*        lda #"8"
;*        jmp showr1
        ld a,"8"
        jr showr1

;*cont5   jsr showr0
;*        bne loop5
cont5   call showr0
        jr nz,loop5
        ret

;*cont1   jsr showr0
;*        bne loop2
cont1   call showr0
        jr nz,loop2
        ret

;*showr0  sta t1
;*        ldx #$ff
showr0  ld c,a
        ld e,$ff
;*loop3   inx
;*        lsr
;*        bcc loop3
loop3   inc e
        rrca
        jr nc,loop3

        ld a,e
        xor $30
showr1  ld e,a
        ld a,b
        cp 10
        ld a,e
        jr nz,cont2

        ld a,"*"
cont2   call TXT_OUTPUT
        inc b
        ld a,b
        cp 11
        ld a,c
        ret
        endp

;*showrules2
;*        .block
;*        lda #1
;*loop1   bit live
;*        bne cont1

;*loop2   asl
;*        bne loop1

;*        lda live+1
;*        beq cont4

;*        lda #"8"
;*        jsr $ffd2
;*cont4   lda #"/"
;*        jsr $ffd2
;*        lda #1
;*loop4   bit born
;*        bne cont5

;*loop5   asl
;*        bne loop4

;*        lda born+1
;*        beq cont3

;*        lda #"8"
;*        jmp $ffd2

;*cont5   jsr showr0
;*        jmp loop5

;*cont1   jsr showr0
;*        jmp loop2

;*showr0  pha
;*        ldx #$ff
;*loop3   inx
;*        lsr
;*        bcc loop3

;*        txa
;*        eor #$30
;*        jsr $ffd2
;*        pla
;*cont3   rts
;*        .bend

inborn   proc
         local loop1,loop3,loop4,cont1,cont2,cont3,cont4
         call printn
         db 12,15,2,"THE RULES ARE DEFINED BY ",24,"BORN"
         db 24," AND ",24,"STAY",24," VALUES.  FOR EXAMPLE, "
         db 15,3,24,"CONWAYS'S LIFE",24,15,2," HAS BORN=3 AND STAY=23, "
         db 15,3,24,"SEEDS",24,15,2," - BORN=2 AND EMPTY STAY, "
         db 15,3,24,"HIGHLIFE",24,15,2," - BORN=36 AND STAY=23, "
         db 15,3,24,"LIFE WITHOUT DEATH",24,15,2," - BORN=3 AND STAY=012345678, ..."
         db 15,1,$d,$a,$d,$a
         db "BORN = $"
         call TXT_PLACE_CURSOR
loop3    ld de,stringbuf
         ld c,0
loop1    call KM_WAIT_CHAR
         cp $d
         jr z,cont1

         cp $7f      ;backspace
         jr z,cont2

         cp $fc      ;esc
         jr z,cont1

         cp "9"
         jr nc,loop1

         cp "1"
         jr c,loop1

         ld hl,stringbuf-1
loop4    inc hl
         push hl
         scf
         ccf
         sbc hl,de
         pop hl
         jr z,cont3

         cp (hl)
         jr z,loop1
         jr loop4

cont3    ld (de),a
         inc de
         inc c
         ld b,a
         call TXT_REMOVE_CURSOR
         ld a,b
         call TXT_OUTPUT
cont4    call TXT_PLACE_CURSOR
         jr loop1

cont1    cp $fc
         push af
         call TXT_REMOVE_CURSOR
         push de
         pop hl
         ld bc,(~stringbuf)+1
         add hl,bc
         pop af
         ret    ;z if esc, hl - buffer length, de - buffer start

cont2    dec de
         dec c
         jp m,loop3

         call TXT_REMOVE_CURSOR
         call printn
         db 8,32,8,"$"
         jr cont4
         endp

instay   proc
         local loop1,loop3,loop4,cont1,cont2,cont3,cont4
         call printn
         db $d,$a,"STAY = $"
         call TXT_PLACE_CURSOR
loop3    ld de,stringbuf
         ld c,0
loop1    call KM_WAIT_CHAR
         cp $d
         jr z,cont1

         cp $7f      ;backspace
         jr z,cont2

         cp "9"
         jr nc,loop1

         cp "0"
         jr c,loop1

         ld hl,stringbuf-1
loop4    inc hl
         push hl
         scf
         ccf
         sbc hl,de
         pop hl
         jr z,cont3

         cp (hl)
         jr z,loop1
         jr loop4

cont3    ld (de),a
         inc de
         inc c
         ld b,a
         call TXT_REMOVE_CURSOR
         ld a,b
         call TXT_OUTPUT
cont4    call TXT_PLACE_CURSOR
         jr loop1

cont1    call TXT_REMOVE_CURSOR     ;cursor off
         push de
         pop hl
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

