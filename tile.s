zerocc   inibcd cellcnt,4
         ret

zerogc   inibcd gencnt,6
         ret

clear    proc
         local cont2,loop,loop0,lnext
         call zerocc
         call zerogc
         ld iy,(startp)
loop     ld a,(iy+sum)
         or a
         jr z,lnext

         xor a
         ld (iy+sum),a
         push iy
         ld b,8
loop0    ld (iy),a    ;to clear pseudocolor info?
         inc iy
         djnz loop0
         pop iy

lnext    ld a,(iy+next+1)
         or a
         jr z,cont2

         ld b,(iy+next)
         ld iyh,a
         ld iyl,b
         jr loop

cont2    call showscn
         call cleanup0
         jp infoout
         endp

fixcnt2  add a,c       ;in: a, bc, e
         ld c,a        ;chg: a, bc, hl
         ld a,0
         adc a,b
         ld b,a
         ld h,high(tab20)
         ld l,e
         ld a,(bc)
         add a,(hl)
         ld (bc),a
         inc h
         inc c
         ld a,(bc)
         add a,(hl)
         ld (bc),a
         inc h
         inc bc
         ld a,(bc)
         add a,(hl)
         ld (bc),a
         inc h
         inc c
         ld a,(bc)
         add a,(hl)
         ld (bc),a
         ret

fixcnt1x add a,c       ;in: a, bc, e
         ld c,a        ;chg: a, bc, hl
         ld a,0
         adc a,b
         ld b,a
         ld h,high(tab13)
         ld l,e
         ld a,(bc)
         add a,(hl)
         ld (bc),a
         dec h
         dec c
         ld a,(bc)
         add a,(hl)
         ld (bc),a
         dec h
         dec bc
         ld a,(bc)
         add a,(hl)
         ld (bc),a
         dec h
         dec c
         ld a,(bc)
         add a,(hl)
         ld (bc),a
         ret

chkaddt    ld a,0
           or a
           ret z

chkadd     ld hl,next   ;in: bc - adjcell or adjcell2
           add hl,bc    ;used: a, hl, de
           ld a,(hl)
           inc l
           or (hl)
           ret nz

addnode  ld de,(startp)
         ld (hl),d
         dec l
         ld (hl),e
         ld (startp),bc
         ld hl,(tilecnt)
         inc hl
         ld (tilecnt),hl
         ret

inctiles ld de,tilesize
         add iy,de
         ret

rndbyte  proc      ;in: iy, b; use: a, hl, bc, i1
         local loop1
         ld a,8
         sub b
         push iy
         pop hl
         add a,l
         ld l,a
         ld a,0
         adc a,h
         ld h,a
         ld a,(density)
         ld ixl,a
loop1    ld a,(i1)
         rrca
         ld c,a
         ld a,r
         xor c
         ld (i1),a
         and 7
         ld bc,bittab
         add a,c
         ld c,a
         ld a,(bc)
         or (hl)
         ld (hl),a
         dec ixl
         jr nz,loop1

         ret
         endp

random   proc
         local cont1,cont2,cont3,cont4,loop1

         ld iy,tiles+((hormax*4+3)*tilesize)
         xor a     ;dir: 0 - left, 1 - right
         ld (t1),a
         ld a,right
         ld ixh,a
         ld de,$100e  ;d - vermax - i1, e - hormax - i2
cont3    ld a,8
         ld (iy+sum),a
         ld b,a
loop1    push bc
         call rndbyte    ;use ixl
         pop bc
         djnz loop1

         push de
         ld c,iyl
         ld b,iyh
         call chkadd
         pop de
         dec e
         jr z,cont2

         ld a,ixh
cont4    add a,iyl
         ld l,a
         ld a,0
         adc a,iyh
         ld h,a
         ld c,(hl)
         inc hl
         ld b,(hl)
         ld iyl,c
         ld iyh,b
         jr cont3

cont2    dec d
         jr z,calccells

         ld e,14
         ld a,(t1)
         xor 1
         ld (t1),a
         ld a,left
         jr nz,cont1

         ld a,right
cont1    ld ixh,a
         ld a,down
         jr cont4
         endp

calccells proc
         local loop2,loop4,cont1
         ld a,(startp+1)
         or a
         ret z

         call zerocc
         ld iy,(startp)
loop2    push iy
         pop ix
         ld c,8
         ld (ix+sum),c
loop4    ld l,(iy)
         ld h,high(tab3)
         ld a,(hl)
         ld hl,cellcnt+4
         add a,(hl)
         ld (hl),a
         sub 10
         jr c,cont1

         ld (hl),a
         dec hl
         call inctsum
cont1    inc iy
         dec c
         jr nz,loop4

         ld a,(ix+next+1)
         or a
         jp z,infoout

         ld iyh,a
         ld a,(ix+next)
         ld iyl,a
         jr loop2
         endp

inctsum  proc
         local loop
         ld b,5
loop     inc (hl)
         ld a,(hl)
         cp 10
         ret nz

         ld (hl),0
         dec hl
         djnz loop
         ret
         endp

dectsum  proc
         local loop
         ld hl,cellcnt+4
         ld b,5
loop     dec (hl)
         ret p

         ld (hl),9
         dec hl
         djnz loop
         ret
         endp

putpixel proc   ;in: x0,y0,xdir,ydir,xchgdir
         local loop2,loop3,cont1,cont2,cont3,cont4,cont5,cont7,cont8,cont9
         local cup,cdown,cleft,cright
         call xchgxy

         ld a,(crsrbit)
         call calcx

         ld a,(crsrx)
         rlca
         rlca
         rlca
         add a,b
         ld b,a
         ld a,(xdir)
         or a
         ld a,(x0)
         jr z,cont4

         ld d,a
         ld a,b
         sub d
         ret c

         jr cont2

cont4    add a,b
         ret c

         cp 160
         ret nc

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

         ld d,a
         ld a,b
         sub d
         ret c

         jr cont1

cont3    add a,b
         ret c

         cp 192
         ret nc

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
         ld b,a
         ld a,ixl
         and $f8
         rrca
         rrca
         rrca
         sub b
         ld ixl,a

         ld bc,(crsrtile)
         ld a,d
         or a
loop2    jp m,cup
         jr nz,cdown

         ld a,ixl
         or a
loop3    jp m,cleft
         jp nz,cright

         ld a,7
         sub ixh
         ld hl,bittab
         add a,l
         ld l,a
         ld d,(hl)
         ld a,(ppmode)
         or a
         jr nz,cont5

         ld iyl,c
         ld iyh,b
         ld l,d
         ld a,e      ;y8byte
         ld e,(iy+video)      ;call readde
         ld d,(iy+video+1)
         rlca
         rlca
         rlca
         add a,d
         ld d,a
         ld a,l
         ld c,a
         and $c0
         jr z,cont7

cont8    ld b,$54
         ld c,$aa
         and c
         jr z,cont9

         ld b,$a8
         ld c,$55
cont9    ld a,(de)
         and c
         or b
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
         ld a,d
         or (hl)
         ld (hl),a
         jp chkadd

cright   ld a,right
         call nextcell
         dec ixl
         jr loop3

cdown    ld a,down
         call nextcell
         dec d        ;y8pos
         jr loop2

cup      ld a,up
         call nextcell
         inc d       ;y8pos
         jr loop2

cleft    ld a,left
         call nextcell
         inc ixl
         jr loop3
         endp

nextcell add a,c       ;in: a, bc; changed: a, hl; set: bc
         ld l,a
         ld a,b
         adc a,0
         ld h,a
         ld c,(hl)
         inc hl
         ld b,(hl)
         ret

vnextcelllo
         add a,iyl
         ld l,a
         ld a,iyh
         adc a,0
         ld h,a
         ld a,(hl)
         ld iyl,a
         inc hl
         ld a,(hl)
         ld iyh,a
         ret

torus    proc
         local l5,l4,l3,l2
         ld iy,tiles
         ld b,hormax
l5       ld de,(hormax*(vermax-1)-1)*tilesize
         push iy
         pop hl
         add hl,de
         ld (iy+ul),l
         ld (iy+ul+1),h
         ld de,hormax*(vermax-1)*tilesize
         push iy
         pop hl
         add hl,de
         ld (iy+up),l
         ld (iy+up+1),h
         ld de,(hormax*(vermax-1)+1)*tilesize
         push iy
         pop hl
         add hl,de
         ld (iy+ur),l
         ld (iy+ur+1),h
         call inctiles
         djnz l5

           ld iy,tiles+((vermax-1)*hormax*tilesize)
           ld b,hormax
l4       ld de,(~(((vermax-1)*hormax-1)*tilesize))+1
         push iy
         pop hl
         add hl,de
         ld (iy+dr),l
         ld (iy+dr+1),h
         ld de,(~((vermax-1)*hormax*tilesize))+1
         push iy
         pop hl
         add hl,de
         ld (iy+down),l
         ld (iy+down+1),h
         ld de,(~(((vermax-1)*hormax+1)*tilesize))+1
         push iy
         pop hl
         add hl,de
         ld (iy+dl),l
         ld (iy+dl+1),h
         call inctiles
         djnz l4

         ld iy,tiles
         ld b,vermax
l3       ld de,(hormax-1)*tilesize
         push iy
         pop hl
         add hl,de
         ld (iy+left),l
         ld (iy+left+1),h
         ld de,(~tilesize)+1
         push iy
         pop hl
         add hl,de
         ld (iy+ul),l
         ld (iy+ul+1),h
         ld de,(2*hormax-1)*tilesize
         push iy
         pop hl
         add hl,de
         ld (iy+dl),l
         ld (iy+dl+1),h
         ld de,hormax*tilesize
         add iy,de
         djnz l3

         ld iy,tiles+((hormax-1)*tilesize)
         ld b,vermax
l2       ld de,(~((2*hormax-1)*tilesize))+1
         push iy
         pop hl
         add hl,de
         ld (iy+ur),l
         ld (iy+ur+1),h
         ld de,(~((hormax-1)*tilesize))+1
         push iy
         pop hl
         add hl,de
         ld (iy+right),l
         ld (iy+right+1),h
         ld de,tilesize
         push iy
         pop hl
         add hl,de
         ld (iy+dr),l
         ld (iy+dr+1),h
         ld de,hormax*tilesize
         add iy,de
         djnz l2

         ld hl,tiles+ul
         ld (hl),low(tiles + ((hormax*vermax-1)*tilesize))
         inc hl
         ld (hl),high(tiles + ((hormax*vermax-1)*tilesize))

         ld hl,tiles+((hormax-1)*tilesize)+ur
         ld (hl),low(tiles+(hormax*(vermax-1)*tilesize))
         inc hl
         ld (hl),high(tiles+(hormax*(vermax-1)*tilesize))

         ld hl,tiles+(hormax*(vermax-1)*tilesize)+dl
         ld (hl),low(tiles+((hormax-1)*tilesize))
         inc hl
         ld (hl),high(tiles+((hormax-1)*tilesize))

         ld hl,tiles+((vermax*hormax-1)*tilesize)+dr
         ld (hl),low(tiles)
         inc hl
         ld (hl),high(tiles)
         ret
         endp

plain    proc
         local l5,l4,l3,l2
         ld iy,tiles
         ld b,hormax
         ld hl,plainbox
l5       ld (iy+ul),l
         ld (iy+ul+1),h
         ld (iy+up),l
         ld (iy+up+1),h
         ld (iy+ur),l
         ld (iy+ur+1),h
         call inctiles
         djnz l5

         ld iy,tiles+((vermax-1)*hormax*tilesize)
         ld b,hormax
l4       ld (iy+dr),l
         ld (iy+dr+1),h
         ld (iy+down),l
         ld (iy+down+1),h
         ld (iy+dl),l
         ld (iy+dl+1),h
         call inctiles
         djnz l4

         ld iy,tiles
         ld b,vermax
         ld de,tilesize*hormax
l3       ld (iy+left),l
         ld (iy+left+1),h
         ld (iy+ul),l
         ld (iy+ul+1),h
         ld (iy+dl),l
         ld (iy+dl+1),h
         add iy,de
         djnz l3

         ld iy,tiles+((hormax-1)*tilesize)
         ld b,vermax
l2       ld (iy+ur),l
         ld (iy+ur+1),h
         ld (iy+right),l
         ld (iy+right+1),h
         ld (iy+dr),l
         ld (iy+dr+1),h
         add iy,de
         djnz l2

         ret
         endp

