;*maketent .block
maketent proc          ;in: hl
         local loop    ;use: a, b, hl, ix, iy
;*         ldy #2
;*         lda ($14),y
         inc hl
         ld a,(hl)
;*         lsr     ;CY=1
         srl a
;*         sbc #1
         dec a
;*         sta $b8
         ld (memb8),a
         ld b,a
;*         iny
         inc hl
;*         ldx #0
;*         stx $b9
         xor a
         ld (memb9),a
         ld iy,EOP
         ld ix,EOP+1024
;*loop     lda ($14),y
;*         sta $800,x
;*         iny
;*         lda ($14),y
;*         sta $c00,x
;*         iny
;*         inx
;*         cpx $b8
;*         bne loop
;*         rts
loop      ld a,(hl)
          ld (iy),a
          inc hl
          ld a,(hl)
          ld (ix),a
          inc hl
          inc iy
          inc ix
          djnz loop
          ret
;*         .bend
         endp

;*ramdisk  .block
ramdisk  proc
         local loop
;*         jsr JPRIMM
         call printn
;*         .byte 147,30
;*         .text "enter file#"
;*         .byte $d,28,"0",144
         db 12,"ENTER FILE# OR HIT ",15,3,"ESC",$d,$a,"0",15,1
;*         .text " glider"
;*         .byte $d,28,"1",144
         db " GLIDER GUN",$d,$a,15,3,"1",15,1
;*         .text " small fish"
;*         .byte $d,28,"2",144
         db " SMALL FISH",$d,$a,15,3,"2",15,1
;*         .text " heavyweight spaceship"
;*         .byte $d,28,"3",144
         db " HEAVYWEIGHT SPACESHIP",$d,$a,15,3,"3",15,1
;*         .text " r-pentomino"
;*         .byte $d,28,"4",144
         db " R-PENTOMINO",$d,$a,15,3,"4",15,1
;*         .text " bunnies"
;*         .byte $d,28,"5",144
         db " BUNNIES",$d,$a,15,3,"5",15,1
;*         .text " lidka"
;*         .byte $d,28,"6",144
         db " LIDKA",$d,$a,15,3,"6",15,1
;*         .text " toad"
;*         .byte $d,28,"7",144
         db " BIG GLIDER",$d,$a,15,3,"7",15,1
;*         .text " bi-gun"
;*         .byte $d,28,"8",144
         db " BI-GUN",$d,$a,15,3,"8",15,1
;*         .text " acorn"
;*         .byte $d,28,"9",144
         db " ACORN",$d,$a,15,3,"9",15,1
;*         .null " switch engine puffer"
         db " SWITCH ENGINE PUFFER$"

;*loop     jsr getkey
;*         cmp #27
;*         bne cont
loop     call KM_WAIT_CHAR
         cp $fc       ;esc

;*         rts
         ret z

;*cont     cmp #$30
;*         bcc loop
         cp $30
         jr c,loop

;*         cmp #$3a
;*         bcs loop
         cp $3a
         jr nc,loop

;*         eor #$30
         xor $30
;*         .bend
         endp


;*loadram  .block   ;in: A
loadram  proc       ;use: a,hl,ix*,iy*,b*
;*         asl
         rlca
;*         tax
;*         lda ramptrs,x
;*         sta $14
;*         lda ramptrs+1,x
;*         sta $15
         ld l,low(ramptrs)
         add a,l
         ld l,a
         ld a,high(ramptrs)
         adc a,0
         ld h,a
         ld a,(hl)
         inc hl
         ld h,(hl)
         ld l,a
;*         ldy #0
;*         lda ($14),y
         ld a,(hl)
;*         sta x0       ;geometry
         ld (x0),a
;*         iny
;*         lda ($14),y
         inc hl
         ld a,(hl)
;*         sta y0       ;geometry
         ld (y0),a
;*         jsr maketent
;*         jsr showrect
;*         bcc puttent
         call maketent
         call showrect
         ret nc

         ;jr puttent
;*         rts
;*         .bend
         endp

;*puttent  .block
puttent  proc         ;in: memb8 - cells count
         local loop   ;use: a, bc, hl,ix*, iy*,de*
;*         lda #0
;*         sta currp
;*         lda #8
;*         sta currp+1
         ld hl,EOP
         ld bc,(memb8)
;*loop     ldy #0
;*         lda (currp),y
;*         sta x0
;*         lda currp+1
;*         pha
;*         eor #4
;*         sta currp+1
;*         lda (currp),y
;*         sta y0
;*         pla
;*         sta currp+1
;*         jsr putpixel
;*         inc currp
;*         bne l1
loop     ld a,(hl)
         ld (x0),a
         push hl
         inc h
         inc h
         inc h
         inc h
         ld a,(hl)
         ld (y0),a
         push bc
         ld hl,putpixel
         call calllo
         pop bc
         pop hl
         dec bc
         inc hl
         ld a,c
         or b
         jr nz,loop
;*         inc currp+1
;*l1       lda $b9
;*         eor #8
;*         cmp currp+1
;*         bne loop

;*         lda currp
;*         cmp $b8
;*         bne loop

;*         cmp #<960
;*         bne l2

;*         lda $b9
;*         cmp #>960
;*         beq l3         ;cf=1 if zf
         ld a,(memb8)
         or a
         ret nz           ;cf=0

         ld a,(memb9)
         cp 4             ;1024
         ccf
         ret
;*         .bend
         endp

