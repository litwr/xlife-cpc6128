;**this program doesn't contain code of the original Xlife
;**it is the conversion from 6502 port for Commodore +4 v3
;**written by litwr, 2013-14
;**it is under GNU GPL

         include "cpc.mac"
         include "xlife.mac"

         org $4000
         include "cpchi.asm"
EOP      dw $

         org $49
start    ld hl,initprg
         call callhi
mainloop proc
         local cont4,cont5
         call dispatcher
         ld a,(mode)
         or a
         jr z,mainloop

         cp 3
         ret z           ;to AMSDOS

         ld hl,(tilecnt)
         ld a,l
         or h
         jr nz,cont4

         ld (mode),a
         ld hl,setbg0
         call callhi
         jp mainloop

cont4    ld a,(mode)
         cp 2
         jr nz,cont5

         call generate     ;hide
         call cleanup
         jp mainloop

cont5    call zerocc
         call generate
         call showscn
         call cleanup
         jp mainloop
         endp

viewport db 0,0
startp   db 1,0
i1       db 0,0
tilecnt  db 0,0
cellcnt  db 0,0,0,0,0
gencnt   db 0,0,0,0,0,0,0
xcrsr    db 0,0,0
ycrsr    db 0,0,0
fnlen    db 0
fn       db 0,0,0,0,0,0,0,0,0,0,0,0
svfnlen  db 0
svfn     db 0,0,0,0,0,0,0,0,0,0,0,0
temp     db 0,0
crsrtile db low(tiles),high(tiles)
crsrbit  db $80    ;x bit position
crsrbyte db 0      ;y%8 - must be after crsrbit!
crsrx    db 0      ;x/8 - not at pseudographics
crsry    db 0      ;y/8
ttab     db 0,1,2,3,3,4,5,6,7,8,8,9,$10,$11,$12,$13,$13,$14
         db $15,$16,$17,$18,$18,$19,$20,$21,$22,$23,$23,$24
         db $25,$26,$27,$28,$28,$29,$30,$31,$32,$33,$33,$34
         db $35,$36,$37,$38,$38,$39,$40,$41,$42,$43,$43,$44
         db $45,$46,$47,$48,$48,$49,$50,$51,$52,$53,$53,$54
         db $55,$56,$57,$58,$58,$59,$60,$61,$62,$63,$63,$64
         db $65,$66,$67,$68,$68,$69,$70,$71,$72,$73,$73,$74
         db $75,$76,$77,$78,$78,$79,$80,$81,$82,$83,$83,$84
         db $85,$86,$87,$88,$88,$89,$90,$91,$92,$93,$93,$94
         db $95,$96,$97,$98,$98,$99
bittab   db 1,2,4,8,16,32,64,128
ctab     db 0,8,$16,$24,$32,$40,$48,$56,$64,$72,$80,$88,$96
         db 4,$12,$20,$28,$36,$44,$52,$60,$68,$76,$84
dirname  db "????????"      ;filename mask used to access directory
cfn      db "colors.cfg"
live     db 12,0
born     db 8,0  ;must be after born
vptilecx db 0
vptilecy db 0    ;must be after vptilex!
density  db 3
topology db 0    ;0 - torus
borderpc db 6    ;plain
bordertc db 19   ;torus
crsrc    db 20
crsrocc  db 16   ;over cell
crsroncc db 16   ;over new cell
livcellc db 9
newcellc db 15
bgedit   db 26
bggo     db 23
framec   db 17
framecellc db 17
frameocellc db 17
tentc    db 2

copyleft db "cr.txt"
errst    db 0   ;0 - do not print i/o-errors message, 1 - print
crsrpgmk db 1   ;0 - do not draw cursor during showscnpg, 1 - draw
splitst  db 1   ;1..255 - split on, 0 - off
ppmode   db 1   ;putpixel mode: 0 - tentative, 1 - active
memb8    db 0
memb9    db 0

         include "gentab.s"
tab3     db 0, 1, 1, 2, 1, 2, 2, 3, 1, 2, 2, 3, 2, 3, 3, 4
         db 1, 2, 2, 3, 2, 3, 3, 4, 2, 3, 3, 4, 3, 4, 4, 5
         db 1, 2, 2, 3, 2, 3, 3, 4, 2, 3, 3, 4, 3, 4, 4, 5
         db 2, 3, 3, 4, 3, 4, 4, 5, 3, 4, 4, 5, 4, 5, 5, 6
         db 1, 2, 2, 3, 2, 3, 3, 4, 2, 3, 3, 4, 3, 4, 4, 5
         db 2, 3, 3, 4, 3, 4, 4, 5, 3, 4, 4, 5, 4, 5, 5, 6
         db 2, 3, 3, 4, 3, 4, 4, 5, 3, 4, 4, 5, 4, 5, 5, 6
         db 3, 4, 4, 5, 4, 5, 5, 6, 4, 5, 5, 6, 5, 6, 6, 7
         db 1, 2, 2, 3, 2, 3, 3, 4, 2, 3, 3, 4, 3, 4, 4, 5
         db 2, 3, 3, 4, 3, 4, 4, 5, 3, 4, 4, 5, 4, 5, 5, 6
         db 2, 3, 3, 4, 3, 4, 4, 5, 3, 4, 4, 5, 4, 5, 5, 6
         db 3, 4, 4, 5, 4, 5, 5, 6, 4, 5, 5, 6, 5, 6, 6, 7
         db 2, 3, 3, 4, 3, 4, 4, 5, 3, 4, 4, 5, 4, 5, 5, 6
         db 3, 4, 4, 5, 4, 5, 5, 6, 4, 5, 5, 6, 5, 6, 6, 7
         db 3, 4, 4, 5, 4, 5, 5, 6, 4, 5, 5, 6, 5, 6, 6, 7
         db 4, 5, 5, 6, 5, 6, 6, 7, 5, 6, 6, 7, 6, 7, 7, 8

         include "tile.s"
         include "cpc.s"

generate proc
         local cont2,cont4,ll1,ll2,ll3,ll4,ll5,ll7
         local lr1,lr2,lr3,lr4,lr5,lr6,lr7,l2,l3,l4,l5,l6,loop3,loop8
;*         #assign16 currp,startp
         ld iy,(startp)
;*loop     ldy #sum
;*         lda (currp),y
loop     ld a,(iy+sum)
         or a
;*         bne cont3
;*         jmp lnext
         jp z, lnext

;*cont3    ldy #0		;up
;*         lda (currp),y
         ld a,(iy)
         or a
;*         beq ldown
         jr z,ldown
;*         
;*         tax
         ld d,0
         ld e,a
;*         ldy #up
;*         jsr iniadjc
         ld c,(iy+up)
         ld b,(iy+up+1)  ;bc=adjcell
         push bc
;*         clc
;*         ldy #count+31
;*         jsr fixcnt1e
         ld a,count+31
         call fixcnt1x
;*         ldy #count+7
;*         jsr fixcnt1
         ld b,iyh
         ld c,iyl
         push bc
         ld a,count+7
         call fixcnt1x
;*         ldy #count
;*         jsr fixcnt2
         pop bc
         ld a,count
         call fixcnt2
;*         jsr chkadd
         pop bc
         call chkadd
;*ldown    ldy #7
;*         lda (currp),y
ldown    ld a,(iy+7)
         or a
;*         beq lleft
         jr z,lleft
;*         
;*         tax
         ld d,0
         ld e,a
;*         ldy #down
;*         jsr iniadjc
         ld c,(iy+down)
         ld b,(iy+down+1)  ;bc=adjcell
         push bc
;*         clc
;*         ldy #count+3
;*         jsr fixcnt1e
         ld a,count+3
         call fixcnt1x
;*         
;*         ldy #count+27
;*         jsr fixcnt1
         ld b,iyh
         ld c,iyl
         push bc
         ld a,count+27
         call fixcnt1x

;*         ldy #count+28
;*         jsr fixcnt2
         pop bc
         ld a,count+28
         call fixcnt2
;*         jsr chkadd
         pop bc
         call chkadd

;*lleft    ldy #left
;*         jsr iniadjc
lleft
         ld c,(iy+left)
         ld b,(iy+left+1)
         push bc
         ld ixl,c
         ld ixh,b   ;ix=adjcell
;*         ldy #0
;*         sty t1   ;change indicator
         xor a
         ld (t1),a
;*         lda (currp),y
         ld a,(iy)
;*         and #$80
         and $80
;*         beq ll1
         jr z,ll1

;*         sta t1
         ld (t1),a
;*         ldy #count+3
;*         lda #1
;*         clc
;*         adc (adjcell),y
;*         sta (adjcell),y
         inc (ix+count+3)
;*         lda #1
;*         ldy #count+7
;*         adc (adjcell),y
;*         sta (adjcell),y
         inc (ix+count+7)
;*         ldy #ul
;*         jsr iniadjc2
;*         lda #1
;*         ldy #count+31
         ld l,(iy+ul)
         ld h,(iy+ul+1)
         push hl
         ld de,count+31
         add hl,de
;*         adc (adjcell2),y
;*         sta (adjcell2),y
         inc (hl)
         pop bc
;*         jsr chkadd2
         call chkadd

;*ll1      ldy #1
ll1
;*         lda (currp),y
         ld a,(iy+1)
;*         and #$80
         and $80
;*         beq ll2
         jr z,ll2

;*         sta t1
         ld (t1),a
;*         ldy #count+3
;*         lda #1
;*         clc
;*         adc (adjcell),y
;*         sta (adjcell),y
         inc (ix+count+3)
;*         lda #1
;*         ldy #count+7
;*         adc (adjcell),y
;*         sta (adjcell),y
         inc (ix+count+7)
;*         lda #1
;*         ldy #count+11
;*         adc (adjcell),y
;*         sta (adjcell),y
         inc (ix+count+11)
;*ll2      ldy #2
ll2
;*         lda (currp),y
         ld a,(iy+2)
;*         and #$80
         and $80
;*         beq ll3
         jr z,ll3

;*         sta t1
         ld (t1),a
;*         ldy #count+7
;*         lda #1
;*         clc
;*         adc (adjcell),y
;*         sta (adjcell),y
         inc (ix+count+7)
;*         lda #1
;*         ldy #count+11
;*         adc (adjcell),y
;*         sta (adjcell),y
         inc (ix+count+11)
;*         lda #1
;*         ldy #count+15
;*         adc (adjcell),y
;*         sta (adjcell),y
         inc (ix+count+15)
;*ll3      ldy #3
ll3
;*         lda (currp),y
         ld a,(iy+3)
;*         and #$80
         and $80
;*         beq ll4
         jr z,ll4

;*         sta t1
         ld (t1),a
;*         ldy #count+11
;*         lda #1
;*         clc
;*         adc (adjcell),y
;*         sta (adjcell),y
         inc (ix+count+11)
;*         lda #1
;*         ldy #count+15
;*         adc (adjcell),y
;*         sta (adjcell),y
         inc (ix+count+15)
;*         lda #1
;*         ldy #count+19
;*         adc (adjcell),y
;*         sta (adjcell),y
         inc (ix+count+19) 
;*ll4      ldy #4
ll4
;*         lda (currp),y
         ld a,(iy+4)
;*         and #$80
         and $80
;*         beq ll5
         jr z,ll5

;*         sta t1
         ld (t1),a
;*         ldy #count+15
;*         lda #1
;*         clc
;*         adc (adjcell),y
;*         sta (adjcell),y
         inc (ix+count+15)
;*         lda #1
;*         ldy #count+19
;*         adc (adjcell),y
;*         sta (adjcell),y
         inc (ix+count+19)
;*         lda #1
;*         ldy #count+23
;*         adc (adjcell),y
;*         sta (adjcell),y
         inc (ix+count+23)
;*ll5      ldy #5
ll5
;*         lda (currp),y
         ld a,(iy+5)
;*         and #$80
         and $80
;*         beq ll6
         jr z,ll6

;*         sta t1
         ld (t1),a
;*         ldy #count+19
;*         lda #1
;*         clc
;*         adc (adjcell),y
;*         sta (adjcell),y
         inc (ix+count+19)
;*         lda #1
;*         ldy #count+23
;*         adc (adjcell),y
;*         sta (adjcell),y
         inc (ix+count+23)
;*         lda #1
;*         ldy #count+27
;*         adc (adjcell),y
;*         sta (adjcell),y
         inc (ix+count+27)
;*ll6      ldy #6
ll6
;*         lda (currp),y
         ld a,(iy+6)
;*         and #$80
         and $80
;*         beq ll7
         jr z,ll7

;*         sta t1
         ld (t1),a
;*         ldy #count+23
;*         lda #1
;*         clc
;*         adc (adjcell),y
;*         sta (adjcell),y
         inc (ix+count+23)
;*         lda #1
;*         ldy #count+27
;*         adc (adjcell),y
;*         sta (adjcell),y
         inc (ix+count+27)
;*         lda #1
;*         ldy #count+31
;*         adc (adjcell),y
;*         sta (adjcell),y
         inc (ix+count+31)
;*ll7      ldy #7
ll7
;*         lda (currp),y
         ld a,(iy+7)
;*         and #$80
         and $80
;*         beq lexit
         jr z,lexit

;*         sta t1
         ld (t1),a
;*         ldy #count+27
;*         lda #1
;*         clc
;*         adc (adjcell),y
;*         sta (adjcell),y
         inc (ix+count+27)
;*         lda #1
;*         ldy #count+31
;*         adc (adjcell),y
;*         sta (adjcell),y
         inc (ix+count+31)
;*         ldy #dl
;*         jsr iniadjc2
         ld l,(iy+dl)
         ld h,(iy+dl+1)
         push hl
         ld de,count+3
         add hl,de
;*         lda #1
;*         ldy #count+3
;*         adc (adjcell2),y
;*         sta (adjcell2),y
         inc (hl)
         pop bc
;*         jsr chkadd2
         call chkadd
;*lexit    jsr chkaddt
lexit    pop bc
         call chkaddt
;*         ldy #right
;*         jsr iniadjc
         ld c,(iy+right)
         ld b,(iy+right+1)
         push bc
         ld ixl,c
         ld ixh,b     ;ix=adjcell

;*         ldy #0
;*         sty t1   ;change indicator
         xor a
         ld (t1),a
;*         lda (currp),y
;*         and #1
;*         beq lr1
         ld a,(iy)
         and 1
         jr z,lr1

;*         sta t1
         ld (t1),a
;*         ldy #count
;*         lda #$10
;*         clc
;*         adc (adjcell),y
;*         sta (adjcell),y
         ld a,$10
         add a,(ix+count)
         ld (ix+count),a
;*         lda #$10
;*         ldy #count+4
;*         adc (adjcell),y
;*         sta (adjcell),y
         ld a,$10
         add a,(ix+count+4)
         ld (ix+count+4),a
;*         ldy #ur
;*         jsr iniadjc2
         ld l,(iy+ur)
         ld h,(iy+ur+1)
         push hl
         ld de,count+28
         add hl,de
;*         lda #$10
;*         ldy #count+28
;*         adc (adjcell2),y
;*         sta (adjcell2),y
         ld a,$10
         add a,(hl)
         ld (hl),a
         pop bc
;*         jsr chkadd2
         call chkadd
;*lr1      ldy #1
;*         lda (currp),y
;*         and #1
;*         beq lr2
lr1      ld a,(iy+1)
         and 1
         jr z,lr2

;*         sta t1
         ld (t1),a
;*         ldy #count
;*         lda #$10
;*         clc
;*         adc (adjcell),y
;*         sta (adjcell),y
         ld a,$10
         add a,(ix+count)
         ld (ix+count),a
;*         lda #$10
;*         ldy #count+4
;*         adc (adjcell),y
;*         sta (adjcell),y
         ld a,$10
         add a,(ix+count+4)
         ld (ix+count+4),a
;*         lda #$10
;*         ldy #count+8
;*         adc (adjcell),y
;*         sta (adjcell),y
         ld a,$10
         add a,(ix+count+8)
         ld (ix+count+8),a
;*lr2      ldy #2
;*         lda (currp),y
;*         and #1
;*         beq lr3
lr2      ld a,(iy+2)
         and 1
         jr z,lr3

;*         sta t1
         ld (t1),a
;*         ldy #count+4
;*         lda #$10
;*         clc
;*         adc (adjcell),y
;*         sta (adjcell),y
         ld a,$10
         add a,(ix+count+4)
         ld (ix+count+4),a
;*         lda #$10
;*         ldy #count+8
;*         adc (adjcell),y
;*         sta (adjcell),y
         ld a,$10
         add a,(ix+count+8)
         ld (ix+count+8),a
;*         lda #$10
;*         ldy #count+12
;*         adc (adjcell),y
;*         sta (adjcell),y
         ld a,$10
         add a,(ix+count+12)
         ld (ix+count+12),a
;*lr3      ldy #3
;*         lda (currp),y
;*         and #1
;*         beq lr4
lr3      ld a,(iy+3)
         and 1
         jr z,lr4

;*         sta t1
         ld (t1),a
;*         ldy #count+8
;*         lda #$10
;*         clc
;*         adc (adjcell),y
;*         sta (adjcell),y
         ld a,$10
         add a,(ix+count+8)
         ld (ix+count+8),a
;*         lda #$10
;*         ldy #count+12
;*         adc (adjcell),y
;*         sta (adjcell),y
         ld a,$10
         add a,(ix+count+12)
         ld (ix+count+12),a
;*         lda #$10
;*         ldy #count+16
;*         adc (adjcell),y
;*         sta (adjcell),y 
         ld a,$10
         add a,(ix+count+16)
         ld (ix+count+16),a
;*lr4      ldy #4
;*         lda (currp),y
;*         and #1
;*         beq lr5
lr4      ld a,(iy+4)
         and 1
         jr z,lr5

;*         sta t1
         ld (t1),a
;*         ldy #count+12
;*         lda #$10
;*         clc
;*         adc (adjcell),y
;*         sta (adjcell),y
         ld a,$10
         add a,(ix+count+12)
         ld (ix+count+12),a
;*         lda #$10
;*         ldy #count+16
;*         adc (adjcell),y
;*         sta (adjcell),y
         ld a,$10
         add a,(ix+count+16)
         ld (ix+count+16),a
;*         lda #$10
;*         ldy #count+20
;*         adc (adjcell),y
;*         sta (adjcell),y
         ld a,$10
         add a,(ix+count+20)
         ld (ix+count+20),a
;*lr5      ldy #5
;*         lda (currp),y
;*         and #1
;*         beq lr6
lr5      ld a,(iy+5)
         and 1
         jr z,lr6

;*         sta t1
         ld (t1),a
;*         ldy #count+16
;*         lda #$10
;*         clc
;*         adc (adjcell),y
;*         sta (adjcell),y
         ld a,$10
         add a,(ix+count+16)
         ld (ix+count+16),a
;*         lda #$10
;*         ldy #count+20
;*         adc (adjcell),y
;*         sta (adjcell),y
         ld a,$10
         add a,(ix+count+20)
         ld (ix+count+20),a
;*         lda #$10
;*         ldy #count+24
;*         adc (adjcell),y
;*         sta (adjcell),y
         ld a,$10
         add a,(ix+count+24)
         ld (ix+count+24),a
;*lr6      ldy #6
;*         lda (currp),y
;*         and #1
;*         beq lr7
lr6      ld a,(iy+6)
         and 1
         jr z,lr7

;*         sta t1
         ld (t1),a
;*         ldy #count+20
;*         lda #$10
;*         clc
;*         adc (adjcell),y
;*         sta (adjcell),y
         ld a,$10
         add a,(ix+count+20)
         ld (ix+count+20),a
;*         lda #$10
;*         ldy #count+24
;*         adc (adjcell),y
;*         sta (adjcell),y
         ld a,$10
         add a,(ix+count+24)
         ld (ix+count+24),a
;*         lda #$10
;*         ldy #count+28
;*         adc (adjcell),y
;*         sta (adjcell),y
         ld a,$10
         add a,(ix+count+28)
         ld (ix+count+28),a
;*lr7      ldy #7
;*         lda (currp),y
;*         and #1
;*         beq rexit
lr7      ld a,(iy+7)
         and 1
         jr z,rexit

;*         sta t1
         ld (t1),a
;*         ldy #count+24
;*         lda #$10
;*         clc
;*         adc (adjcell),y
;*         sta (adjcell),y
         ld a,$10
         add a,(ix+count+24)
         ld (ix+count+24),a
;*         lda #$10
;*         ldy #count+28
;*         adc (adjcell),y
;*         sta (adjcell),y
         ld a,$10
         add a,(ix+count+28)
         ld (ix+count+28),a
;*         ldy #dr
;*         jsr iniadjc2
         ld l,(iy+dr)
         ld h,(iy+dr+1)
         push hl
         ld de,count
         add hl,de
;*         lda #$10
;*         ldy #count
;*         adc (adjcell2),y
;*         sta (adjcell2),y
         ld a,$10
         add a,(hl)
         ld (hl),a
         pop bc
;*         jsr chkadd2
         call chkadd
;*rexit    jsr chkaddt
rexit    pop bc
         call chkaddt
         
;*         ldy #1
;*         lda (currp),y
;*         beq l2
         ld d,0
         ld a,(iy+1)
         or a
         jr z,l2

;*         tax
         ld e,a
;*         clc
;*         ldy #count+3
;*         jsr fixcnt1
         ld a,count+3
         ld b,iyh
         ld c,iyl
         push bc
         call fixcnt1x
;*         ldy #count+4
;*         jsr fixcnt2
         ld a,count+4
         pop bc
         push bc
         call fixcnt2
;*         ldy #count+11
;*         jsr fixcnt1
         ld a,count+11
         pop bc
         call fixcnt1x
;*l2       ldy #2
;*         lda (currp),y
;*         beq l3
l2       ld a,(iy+2)
         or a
         jr z,l3

;*         tax
         ld e,a
;*         clc
;*         ldy #count+7
;*         jsr fixcnt1
         ld a,count+7
         ld b,iyh
         ld c,iyl
         push bc
         call fixcnt1x
;*         ldy #count+8
;*         jsr fixcnt2
         ld a,count+8
         pop bc
         push bc
         call fixcnt2
;*         ldy #count+15
;*         jsr fixcnt1
         ld a,count+15
         pop bc
         call fixcnt1x
         
;*l3       ldy #3
;*         lda (currp),y
;*         beq l4
l3       ld a,(iy+3)
         or a
         jr z,l4

;*         tax
         ld e,a
;*         clc
;*         ldy #count+11
;*         jsr fixcnt1
         ld a,count+11
         ld b,iyh
         ld c,iyl
         push bc
         call fixcnt1x
;*         ldy #count+12
;*         jsr fixcnt2
         ld a,count+12
         pop bc
         push bc
         call fixcnt2
;*         ldy #count+19
;*         jsr fixcnt1
         ld a,count+19
         pop bc
         call fixcnt1x
;*l4       ldy #4
;*         lda (currp),y
;*         beq l5
l4       ld a,(iy+4)
         or a
         jr z,l5

;*         tax
         ld e,a
;*         clc
;*         ldy #count+15
;*         jsr fixcnt1
         ld a,count+15
         ld b,iyh
         ld c,iyl
         push bc
         call fixcnt1x
;*         ldy #count+16
;*         jsr fixcnt2
         ld a,count+16
         pop bc
         push bc
         call fixcnt2
;*         ldy #count+23
;*         jsr fixcnt1
         ld a,count+23
         pop bc
         call fixcnt1x
;*l5       ldy #5
;*         lda (currp),y
;*         beq l6
l5       ld a,(iy+5)
         or a
         jr z,l6

;*         tax
         ld e,a
;*         clc
;*         ldy #count+19
;*         jsr fixcnt1
         ld a,count+19
         ld b,iyh
         ld c,iyl
         push bc
         call fixcnt1x
;*         ldy #count+20
;*         jsr fixcnt2
         ld a,count+20
         pop bc
         push bc
         call fixcnt2
;*         ldy #count+27
;*         jsr fixcnt1
         ld a,count+27
         pop bc
         call fixcnt1x
;*l6       ldy #6
;*         lda (currp),y
;*         beq lnext
l6       ld a,(iy+6)
         or a
         jr z,lnext

;*         tax
         ld e,a
;*         clc
;*         ldy #count+23
;*         jsr fixcnt1
         ld a,count+23
         ld b,iyh
         ld c,iyl
         push bc
         call fixcnt1x
;*         ldy #count+24
;*         jsr fixcnt2
         ld a,count+24
         pop bc
         push bc
         call fixcnt2
;*         ldy #count+31
;*         jsr fixcnt1
         ld a,count+31
         pop bc
         call fixcnt1x
;*lnext    ldy #next
;*         lda (currp),y
lnext
         ld l,(iy+next)
         ld h,(iy+next+1)
         ld a,h
         or a
         jr nz,cont2

         ld a,l
         dec a
         jr z,stage2
         
;*         tax
;*         iny
;*         lda (currp),y
;*         bne cont2
;*         
;*         cpx #1
;*         beq stage2
        
;*cont2    sta currp+1
;*         stx currp
;*         jmp loop
cont2      push hl
           pop iy
           jp loop

;*stage2   #assign16 currp,startp
stage2     ld iy,(startp)
           ld de,gentab
;*         .bend
           
;*         
;*genloop2 ldy #sum
;*         .block
;*         lda #0
;*         sta (currp),y
genloop2   ld (iy+sum),0
;*         lda pseudoc   ;commented = 5% slower
;*         beq cont4     ;with no pseudocolor
           ld a,(pseudoc)
           or a
           jr z, cont4

;*         ldx #8
;*         lda #0
;*         sta loop8+1
;*         lda #pc
;*         sta mpc+1
;*loop8    ldy #0
;*         lda (currp),y
;*mpc      ldy #pc
;*         sta (currp),y
;*         inc loop8+1
;*         inc mpc+1
;*         dex
;*         bne loop8
           push iy
           ld b,8
loop8      ld a,(iy)
           ld (iy+pc),a
           inc iy
           djnz loop8
           pop iy

;*cont4    #genmac count,0
;*         #genmac count+4,1
;*         #genmac count+8,2
;*         #genmac count+12,3
;*         #genmac count+16,4
;*         #genmac count+20,5
;*         #genmac count+24,6
;*         #genmac count+28,7
cont4    genmac count,0
         genmac count+4,1
         genmac count+8,2
         genmac count+12,3
         genmac count+16,4
         genmac count+20,5
         genmac count+24,6
         genmac count+28,7
;*         ldy #count
;*         lda #0
;*loop3    sta (currp),y
;*         iny
;*         cpy #count+32
;*         bne loop3
         ld b,32
         push iy
loop3    ld (iy+count),0
         inc iy
         djnz loop3
         pop iy         
;*         ldy #next
;*         lda (currp),y
;*         tax
;*         iny
;*         lda (currp),y
;*         bne gencont1

;*         cpx #1
;*         bne gencont1
         ld l,(iy+next)
         ld h,(iy+next+1)
         ld a,h
         or a
         jr nz,gencont1

         ld a,l
         dec a
         jr z,incgen
;*         .bend
;*rts2     rts

;*gencont1 sta currp+1
;*         stx currp
;*         jmp genloop2
gencont1 push hl
         pop iy
         jp genloop2
         endp

;*incgen   .block
;*         ldy #$30
;*         #incbcd gencnt+6
;*         sty gencnt+6
;*         #incbcd gencnt+5
;*         sty gencnt+5
;*         #incbcd gencnt+4
;*         sty gencnt+4
;*         #incbcd gencnt+3
;*         sty gencnt+3
;*         #incbcd gencnt+2
;*         sty gencnt+2
;*         #incbcd gencnt+1
;*         sty gencnt+1
;*         #incbcd gencnt
;*         sty gencnt
;*cont2    rts
;*         .bend
incgen   proc
         ld b,0
         ld hl,gencnt+6
;*         #incbcd gencnt+6
         incbcd2
         dec hl
;*         #incbcd gencnt+5
;*         sty gencnt+5
         incbcd2
         dec hl
;*         #incbcd gencnt+4
;*         sty gencnt+4
         incbcd2
         dec hl
;*         #incbcd gencnt+3
;*         sty gencnt+3
         incbcd2
         dec hl
;*         #incbcd gencnt+2
;*         sty gencnt+2
         incbcd2
         dec hl
;*         #incbcd gencnt+1
;*         sty gencnt+1
         incbcd2
         dec hl
;*         #incbcd gencnt
;*         sty gencnt
         incbcd2
;*cont2    rts
         ret
;*         .bend
         endp

;*cleanup  .block
;*         jsr incgen
;*         inc clncnt
;*         lda #$f
;*         and clncnt
;*         bne rts2
;*         .bend
cleanup  ld a,(clncnt)
         inc a
         ld (clncnt),a
         and $f
         ret nz

;*cleanup0 .block
cleanup0 proc
         local loop,cont2
;*         #assign16 currp,startp
         ld iy,(startp)
;*         #zero16 adjcell   ;mark 1st
         ld ix,0
         
;*loop     ldy #sum
;*         lda (currp),y
;*         beq delel
loop     ld a,(iy+sum)
         or a
         jr z,delel
;*         ldy #next
;*         lda (currp),y
         ld l,(iy+next)
;*         tax
         ld b,l
;*         iny
;*         lda (currp),y
;*         bne cont2
         ld h,(iy+next+1)
         ld a,h
         or a
         jr nz,cont2

;*         cpx #1
;*         bne cont2
         dec b
         ret z
;*         rts

;*cont2    ldy currp    ;save pointer to previous
;*         sty adjcell
;*         ldy currp+1
;*         sty adjcell+1
cont2      push iy
           pop ix
;*         sta currp+1
;*         stx currp
           push hl
           pop iy
;*         jmp loop
           jp loop

;*delel    lda tilecnt
;*         bne l2

;*         dec tilecnt+1
;*l2       dec tilecnt
delel    ld hl,(tilecnt)
         dec hl
         ld (tilecnt),hl

;*         ldy #next
;*         lda (currp),y
;*         sta i1
;*         iny
;*         lda (currp),y
;*         sta i1+1
         ld l,(iy+next)
         ld h,(iy+next+1)
         ld (i1),hl

;*         lda #0
;*         sta (currp),y
;*         dey
;*         sta (currp),y
         xor a
         ld (iy+next),a
         ld (iy+next+1),a
;*         #assign16 currp,i1
         ld iy,(i1)

;*         lda adjcell
;*         ora adjcell+1
;*         beq del1st
         ld a,ixh
         or ixl
         jr z,del1st

;*         lda i1
;*         sta (adjcell),y
;*         iny
;*         lda i1+1
;*         sta (adjcell),y
;*         bne loop
         ld hl,(i1)
         ld (ix+next),l
         ld (ix+next+1),h
         ld a,h
         or a
         jr nz,loop

;*         lda #1
;*         cmp i1
;*         bne loop
         dec l
         jr nz,loop

;*exit     rts
         ret

;*del1st   #assign16 startp,i1
del1st   ld hl,(i1)
         ld (startp),hl
;*         lda tilecnt
;*         bne loop

;*         lda tilecnt+1
;*         beq exit

;*         jmp loop
;*         .bend
         ld hl,(tilecnt)
         ld a,h
         or l
         ret z

         jp loop
         endp

dispatcher
         call KM_READ_CHAR
         ret nc

         ld hl,dispat2
         jp callhi

         include "video.s"
         include "tab12.s"
         include "initiles.s"
realnum2 db 0,0,0,0,0

split_off ld a,(splitst)
         or a
         ret z

         xor a
         ld (splitst),a
         ld hl,ticker_event_block
         call KL_DEL_FAST_TICKER
         ld a,1
         jp SCR_SET_MODE

split_on0 ld bc,&bc07				;; select CRTC register 7
         out (c),c
         inc b
         ld a,29
         out (c),a 
         ld hl,ticker_event_block
         ld b,%11000001				;; near address, express asynchronous event
         ld c,&80					;; rom select 
         ld de,ticker_function
         jp KL_NEW_FAST_TICKER

split_on ld a,(splitst)
         or a
         ret nz

         ld a,6
         ld (splitst),a
         ld (ticker_counter),a

         call MC_WAIT_FLYBACK
         halt
         halt
         call MC_WAIT_FLYBACK

         ld hl,ticker_event_block
         jp KL_ADD_FAST_TICKER

;; this is initialised by
;; the firmware; holds runtime state of ticker interrupt
ticker_event_block
         defs 10

;; this is the function called each 1/300th of a second
ticker_function proc
         local cont1,wait
         push af
         push bc
         push hl
;; The 1/300th of a second interrupt effectively splits
;; the screen into 6 sections of equal height. Each section
;; spans the entire width of the screen.
;;
;; We want to ensure that the effect is stationary so we reset
;; every 6 calls of this function. We need to ensure we are synced with vsync in
;; order that this works correctly.
         ld h,0
         ld a,(ticker_counter)
         dec a
         jr nz,cont1

         ld b,$65
wait     djnz wait
         inc h
         ld a,6
cont1    ld (ticker_counter),a

;; already in alternative register set

;; set mode now to ensure we set it early, so that other interrupt delay is not so important.

;; fetch current C register value stored in A'
         ex af,af'
         ld c,a
         ex af,af'
;; clear off mode bits
         res 1,c
         res 0,c
         ld a,h
;; combine with our wanted mode
         or c
;; write to hardware
         ld c,a
         and $fb    ;; ensure lower rom is active, but we don't want to modify C register ;)
         ld b,$7f
         out (c),a

;; now store back to A'
;; later firmware will also use this to set mode/rom once more, but now it is updated
;; with our new values we will get the mode we wanted.
         ex af,af'
         ld a,c			;; remember it otherwise firmware sets it back
         ex af,af'

;; ensure express asynchronous event is retriggered correctly
;; see SOFT968
         xor a
         LD (ticker_event_block+2),a
         pop hl
         pop bc
         pop af
         ret
         endp

ticker_counter defb 6
         end start

