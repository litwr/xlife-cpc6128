;**this program doesn't contain code of the original Xlife
;**it is the conversion from 6502 port for Commodore +4
;**written by litwr, 2013-14, v2
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
         dec a
         jr z,cont5

         call generate     ;hide
         call cleanup
         jp mainloop

cont5    call zerocc
         call generate
         call showscn
         call cleanup
         jp mainloop
         endp

topology db 0    ;0 - torus
vptilecx db 0
vptilecy db 0    ;must be after vptilex!
density  db 3
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
ctab     db 0,8,$16,$24,$32,$40,$48,$56,$64,$72,$80,$88,$96 ;no page cross
         db 4,$12,$20,$28,$36,$44,$52,$60,$68,$76,$84
bittab   db 1,2,4,8,16,32,64,128   ;no page cross
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

         org $100   ;???
gentab   ;page aligned!
         include "gentab.s"
         include "tab12.s"
tab3     db 0, 1, 1, 2, 1, 2, 2, 3, 1, 2, 2, 3, 2, 3, 3, 4 ;page aligned!
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
pctable  db 0,4,8,$c,0,$40,8,$48,0,4,$80,$84,0,$40,$80,$c0 ;page aligned!
ttab     db 0,1,2,3,3,4,5,6,7,8,8,9,$10,$11,$12,$13,$13,$14  ;ttab must not cross page
         db $15,$16,$17,$18,$18,$19,$20,$21,$22,$23,$23,$24
         db $25,$26,$27,$28,$28,$29,$30,$31,$32,$33,$33,$34
         db $35,$36,$37,$38,$38,$39,$40,$41,$42,$43,$43,$44
         db $45,$46,$47,$48,$48,$49,$50,$51,$52,$53,$53,$54
         db $55,$56,$57,$58,$58,$59,$60,$61,$62,$63,$63,$64
         db $65,$66,$67,$68,$68,$69,$70,$71,$72,$73,$73,$74
         db $75,$76,$77,$78,$78,$79,$80,$81,$82,$83,$83,$84
         db $85,$86,$87,$88,$88,$89,$90,$91,$92,$93,$93,$94
         db $95,$96,$97,$98,$98,$99
digifont db $3c,$66,$6e,$76,$66,$66,$3c,0  ;digifont must not cross page
         db $18,$18,$38,$18,$18,$18,$7e,0
         db $3c,$66,6,$c,$30,$60,$7e,0
         db $3c,$66,6,$e,6,$66,$3c,0
         db 6,$e,$1e,$36,$7f,6,6,0
         db $7e,$60,$7c,6,6,$66,$3c,0   ;5
         db $3c,$66,$60,$7c,$66,$66,$3c,0
         db $7e,$66,$c,$18,$18,$18,$18,0
         db $3c,$66,$66,$3c,$66,$66,$3c,0
         db $3c,$66,$66,$3e,6,$66,$3c,0  ;zoom equ digifont+79
         db 0,0,0,0,0,0,0                ;space
tinfo    db 0,0,0
dirname  db "????????"      ;filename mask used to access directory
cfn      db "colors.cfg"
live     db 12,0 ;must be after cfn
born     db 8,0  ;must be after born
crsrpgmk db 1   ;0 - do not draw cursor during showscnz, 1 - draw
splitst  db 1   ;1..255 - split on, 0 - off
ppmode   db 1   ;putpixel mode: 0 - tentative, 1 - active
memb8    db 0
memb9    db 0

         include "tile.s"
         include "cpc.s"

generate proc
         local cont10,ll1,ll2,ll3,ll4,ll5,ll7
         local lr1,lr2,lr3,lr4,lr5,lr6,lr7,l2,l3,l4,l5,l6,loop3

;*         #assign16 currp,startp
         ld hl,(startp)

;*loop3    ldy #0
;*         lda (currp),y
;*         ldy #count0
;*         #setcount
loop3    ld a,l
         ld iyl,a
         add a,count0
         ld l,a
         ld a,h
         ld iyh,a
         adc a,0
         ld h,a
         setcount 0

;*         ldy #1
;*         lda (currp),y
;*         ldy #count1
;*         #setcount
         inc hl
         setcount 1

;*         ldy #2
;*         lda (currp),y
;*         ldy #count2
;*         #setcount
         inc hl
         setcount 2

;*         ldy #3
;*         lda (currp),y
;*         ldy #count3
;*         #setcount
         inc hl
         setcount 3

;*         ldy #4
;*         lda (currp),y
;*         ldy #count4
;*         #setcount
         inc hl
         setcount 4

;*         ldy #5
;*         lda (currp),y
;*         ldy #count5
;*         #setcount
         inc hl
         setcount 5
;*         ldy #6
;*         lda (currp),y
;*         ldy #count6
;*         #setcount
         inc hl
         setcount 6

;*         ldy #7
;*         lda (currp),y
;*         ldy #count7
;*         #setcount
         inc hl
         setcount 7

;*         ldy #next+1
;*         lda (currp),y
;*         beq cont10
         ld a,(iy+next+1)
         or a
         jr z,cont10

;*         tax
;*         dey
;*         lda (currp),y
;*         sta currp
;*         stx currp+1
;*         jmp loop3
         ld h,a
         ld l,(iy+next)
         jp loop3

cont10
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
         ld e,a
;*         ldy #up
;*         jsr iniadjc
         ld c,(iy+up)
         ld b,(iy+up+1)  ;bc=adjcell
         push bc
         ld a,count7+3
         call fixcnt1x
         ld b,iyh
         ld c,iyl
         push bc
         ld a,count1+3
         call fixcnt1x
         pop bc
         ld a,count0
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
         ld e,a
;*         ldy #down
;*         jsr iniadjc
         ld c,(iy+down)
         ld b,(iy+down+1)  ;bc=adjcell
         push bc
         ld a,count0+3
         call fixcnt1x

;*         ldy #count+27
;*         jsr fixcnt1
         ld b,iyh
         ld c,iyl
         push bc
         ld a,count6+3
         call fixcnt1x

         pop bc
         ld a,count7
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
         or (iy)
         jp p,ll1

;*         sta t1
         ld (t1),a
         ispyr4 (ix+count0+3)
         ispyr4 (ix+count1+3)
         ld l,(iy+ul)
         ld h,(iy+ul+1)
         push hl
         ld de,count7+3
         add hl,de
         ispyr4 (hl)
         pop bc
;*         jsr chkadd2
         call chkadd

;*ll1      ldy #1
ll1
;*         lda (currp),y
         ld a,(iy+1)
         and a
         jp p,ll2

;*         sta t1
         ld (t1),a
         ispyr4 (ix+count0+3)
         ispyr4 (ix+count1+3)
         ispyr4 (ix+count2+3)
;*ll2      ldy #2
ll2
;*         lda (currp),y
         ld a,(iy+2)
         and a
         jp p,ll3

;*         sta t1
         ld (t1),a
         ispyr4 (ix+count1+3)
         ispyr4 (ix+count2+3)
         ispyr4 (ix+count3+3)
;*ll3      ldy #3
ll3
;*         lda (currp),y
         ld a,(iy+3)
         and a
         jp p,ll4

;*         sta t1
         ld (t1),a
         ispyr4 (ix+count2+3)
         ispyr4 (ix+count3+3)
         ispyr4 (ix+count4+3) 
;*ll4      ldy #4
ll4
;*         lda (currp),y
         ld a,(iy+4)
         and a
         jp p,ll5

;*         sta t1
         ld (t1),a
         ispyr4 (ix+count3+3)
         ispyr4 (ix+count4+3)
         ispyr4 (ix+count5+3)
;*ll5      ldy #5
ll5
;*         lda (currp),y
         ld a,(iy+5)
         and a
         jp p,ll6

;*         sta t1
         ld (t1),a
         ispyr4 (ix+count4+3)
         ispyr4 (ix+count5+3)
         ispyr4 (ix+count6+3)
;*ll6      ldy #6
ll6
;*         lda (currp),y
         ld a,(iy+6)
         and a
         jp p,ll7

;*         sta t1
         ld (t1),a
         ispyr4 (ix+count5+3)
         ispyr4 (ix+count6+3)
         ispyr4 (ix+count7+3)
;*ll7      ldy #7
ll7
;*         lda (currp),y
         ld a,(iy+7)
         and a
         jp p,lexit

;*         sta t1
         ld (t1),a
         ispyr4 (ix+count6+3)
         ispyr4 (ix+count7+3)
         ld l,(iy+dl)
         ld h,(iy+dl+1)
         push hl
         ld de,count0+3
         add hl,de
         ispyr4 (hl)
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
         rrca
         jr nc,lr1

;*         sta t1
         ld (t1),a
         ispyr8 (ix+count0)
         ispyr8 (ix+count1)
;*         ldy #ur
;*         jsr iniadjc2
         ld l,(iy+ur)
         ld h,(iy+ur+1)
         push hl
         ld de,count7
         add hl,de
         ispyr8 (hl)
         pop bc
;*         jsr chkadd2
         call chkadd
;*lr1      ldy #1
;*         lda (currp),y
;*         and #1
;*         beq lr2
lr1      ld a,(iy+1)
         rrca
         jr nc,lr2

;*         sta t1
         ld (t1),a
         ispyr8 (ix+count0)
         ispyr8 (ix+count1)
         ispyr8 (ix+count2)
;*lr2      ldy #2
;*         lda (currp),y
;*         and #1
;*         beq lr3
lr2      ld a,(iy+2)
         rrca
         jr nc,lr3

;*         sta t1
         ld (t1),a
         ispyr8 (ix+count1)
         ispyr8 (ix+count2)
         ispyr8 (ix+count3)
;*lr3      ldy #3
;*         lda (currp),y
;*         and #1
;*         beq lr4
lr3      ld a,(iy+3)
         rrca
         jr nc,lr4

;*         sta t1
         ld (t1),a
         ispyr8 (ix+count2)
         ispyr8 (ix+count3)
         ispyr8 (ix+count4)
;*lr4      ldy #4
;*         lda (currp),y
;*         and #1
;*         beq lr5
lr4      ld a,(iy+4)
         rrca
         jr nc,lr5

;*         sta t1
         ld (t1),a
         ispyr8 (ix+count3)
         ispyr8 (ix+count4)
         ispyr8 (ix+count5)
;*lr5      ldy #5
;*         lda (currp),y
;*         and #1
;*         beq lr6
lr5      ld a,(iy+5)
         rrca
         jr nc,lr6

;*         sta t1
         ld (t1),a
         ispyr8 (ix+count4)
         ispyr8 (ix+count5)
         ispyr8 (ix+count6)
;*lr6      ldy #6
;*         lda (currp),y
;*         and #1
;*         beq lr7
lr6      ld a,(iy+6)
         rrca
         jr nc,lr7

;*         sta t1
         ld (t1),a
         ispyr8 (ix+count5)
         ispyr8 (ix+count6)
         ispyr8 (ix+count7)
;*lr7      ldy #7
;*         lda (currp),y
;*         and #1
;*         beq rexit
lr7      ld a,(iy+7)
         rrca
         jr nc,rexit

;*         sta t1
         ld (t1),a
         ispyr8 (ix+count6)
         ispyr8 (ix+count7)
         ld l,(iy+dr)
         ld h,(iy+dr+1)
         push hl
         ld de,count0
         add hl,de
;*         lda #$10
;*         ldy #count
;*         adc (adjcell2),y
;*         sta (adjcell2),y
         ispyr8 (hl)
         pop bc
;*         jsr chkadd2
         call chkadd
;*rexit    jsr chkaddt
rexit    pop bc
         call chkaddt
         
;*         ldy #1
;*         lda (currp),y
;*         beq l2
         ld a,(iy+1)
         or a
         jr z,l2

;*         tax
         ld e,a
         ld a,count0+3
         ld b,iyh
         ld c,iyl
         push bc
         call fixcnt1x
         ld a,count1
         pop bc
         push bc
         call fixcnt2
         ld a,count2+3
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
         ld a,count1+3
         ld b,iyh
         ld c,iyl
         push bc
         call fixcnt1x
         ld a,count2
         pop bc
         push bc
         call fixcnt2
         ld a,count3+3
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
         ld a,count2+3
         ld b,iyh
         ld c,iyl
         push bc
         call fixcnt1x
         ld a,count3
         pop bc
         push bc
         call fixcnt2
         ld a,count4+3
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
         ld a,count3+3
         ld b,iyh
         ld c,iyl
         push bc
         call fixcnt1x
         ld a,count4
         pop bc
         push bc
         call fixcnt2
         ld a,count5+3
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
         ld a,count4+3
         ld b,iyh
         ld c,iyl
         push bc
         call fixcnt1x
         ld a,count5
         pop bc
         push bc
         call fixcnt2
         ld a,count6+3
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
         ld a,count5+3
         ld b,iyh
         ld c,iyl
         push bc
         call fixcnt1x
         ld a,count6
         pop bc
         push bc
         call fixcnt2
         ld a,count7+3
         pop bc
         call fixcnt1x

;*lnext    ldy #next
;*         lda (currp),y
lnext    ld a,(iy+next+1)
         or a
         jr z,stage2
         
         ld b,(iy+next)
         ld iyh,a
         ld iyl,b
         jp loop

;*stage2   #assign16 currp,startp
stage2     ld iy,(startp)
           ld b,high(tab3)
           ld d,high(gentab)
genloop2   ld a,count0
           add a,iyl
           ld l,a
           ld a,0
           adc a,iyh
           ld h,a
;*         .bend

         ld (iy+sum),0
         genmac 0
         inc hl
         genmac 1
         inc hl
         genmac 2
         inc hl
         genmac 3
         inc hl
         genmac 4
         inc hl
         genmac 5
         inc hl
         genmac 6
         inc hl
         genmac 7
         ld a,(iy+next+1)
         or a
         jr z,incgen

         ld c,(iy+next)
         ld iyh,a
         ld iyl,c
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
         local loop
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

         ld a,(iy+next+1)
         or a
         ret z

;*cont2    ldy currp    ;save pointer to previous
;*         sty adjcell
;*         ldy currp+1
;*         sty adjcell+1
           push iy
           pop ix
;*         sta currp+1
;*         stx currp
           ld b,(iy+next)
           ld iyl,b
           ld iyh,a
           jp loop

;*delel    lda tilecnt
;*         bne l2

;*         dec tilecnt+1
;*l2       dec tilecnt
delel    ld hl,(tilecnt)
         dec hl
         ld (tilecnt),hl

         xor a
         ld hl,count7+3
         ld c,iyl
         ld b,iyh
         add hl,bc
         rept 32
         ld (hl),a
         dec hl
         endm
         
;*         ldy #next
;*         lda (currp),y
;*         sta i1
;*         iny
;*         lda (currp),y
;*         sta i1+1
         ld c,(iy+next)
         ld b,(iy+next+1)
         ld (i1),bc

;*         lda #0
;*         sta (currp),y
;*         dey
;*         sta (currp),y
         ;xor a   ;ac=0 here
         ;ld (iy+next),a
         ;ld (iy+next+1),a
         ld (hl),a
         dec hl
         ld (hl),a
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
         jp nz,loop
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
tiles
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

ticker_counter db 6
         end start

