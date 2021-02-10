calcx    proc        ;$80 -> 0, $40 -> 1, ...
         local cl2
         ld b,$ff
cl2      inc b
         rlca
         jr nc,cl2

         ld a,b
         ret
         endp

crsrpg   xor a
         ld (fi1+1),a
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
         nexthlds 8
         rept 6
         nexthlc
         nexthlds 8
         endm
         ld a,$f
         ld (hl),a
         inc l
         ld (hl),a
         pop hl
         ret

clrcur   nexthlls
         nexthlds 8
         ld a,h
         add a,48
         ld h,a
         xor a
         ld (hl),a
         inc l
         ld (hl),a
         pop hl
         ret

showscnz proc
         local loop1,loop2,loop3,loop4,cont1,cont2,cont4,cont5,cont6
;ylimit - iyh, xlimit - iyl
         ld ix,(viewport)
         xor a
         ld (fi1+1),a
         ld a,(crsrbyte)
         ld b,a
         ld a,8
         sub b
         ld (fi2+1),a
         ld a,(crsrbit)
         call calcx
         ld a,8
         sub b
         ld (m10+1),a
         ld hl,$c800
         ld iyh,3
         ld a,(pseudoc)
         or a
         jp nz,showscnzp

loop3    ld iyl,5
loop4    ld a,(crsrtile)
         cp ixl
         jp nz,cont4

         ld a,(crsrtile+1)
         cp ixh
         jr nz,cont4

         ld a,1
         ld (fi1+1),a
cont4    ld d,8
loop2    ld e,(ix)
         ld b,8
         ld c,b
loop1    sla e
         jp nc,cont1

         nexthll 3
         nexthld $c,c
         nexthll 7
         nexthld $e,c
         nexthll 7     ;live cell char
         nexthld $e,c
         nexthll 7
         nexthld $e,c
         nexthll 7
         nexthld $e,c
         nexthll 3
         ld (hl),$c
cont2    ld a,h
         sub 40
         ld h,a
cont6    inc hl
fi1      ld a,0
         dec a
         jr nz,cont5

fi2      ld a,0
         cp d
         jr nz,cont5

m10      ld a,0
         cp b
         call z,crsrpg
cont5    djnz loop1

         inc ix
         ld c,80-16      ;b=0
         add hl,bc
         dec d
         jp nz,loop2

         ld de,(~(80*8-16))+1
         add hl,de
         ld c,tilesize-8 ;b=0
         add ix,bc
         dec iyl
         jp nz,loop4

         dec iyh
         ret z

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
         nexthlds c
         nexthlls
         endm
         ld (hl),a
         jp cont2
         endp

showscnzp proc
         local m1,loop1,loop2,loop3,loop4
         local cont1,cont2,cont2a,cont4,cont5,cont6,cont8,cont12
;ylimit - iyh, xlimit - iyl
loop3    ld iyl,5
loop4    ld a,(crsrtile)
         cp ixl
         jp nz,cont4

         ld a,(crsrtile+1)
         cp ixh
         jr nz,cont4

         ld a,1
         ld (fi1+1),a
cont4    xor a
         ld (m1+2),a
         ld c,8
loop2    ;ld a,(m1+2)
         rlca
         rlca
         add a,count0
         add a,ixl
         ld e,a
         ld a,ixh
         adc a,0
         ld d,a
         ld a,(de)
         and $c0
         ld b,a
         inc de
         ld a,(de)
         rlca
         and $30
         or b
         ld b,a
         inc de
         ld a,(de)
         rrca
         and $c
         or b
         ld b,a
         inc de
         ld a,(de)
         and 3
         or b
         ld d,a
m1       ld e,(ix)
         ld b,8
loop1    rlc d              ;pseudocolor
         sla e
         jp nc,cont1

         nexthll 3
         nexthld $c,8
         nexthll 7
         nexthld $e,8
         ld a,d
         rrca
         jr c,cont12

         nexthll 6     ;new cell char
         nexthld 6,8
         nexthll 6
         nexthld 6,8
         jp cont2

cont12   nexthll 7     ;live cell char
         nexthld $e,8
         nexthll 7
         nexthld $e,8
cont2    nexthll 7
         nexthld $e,8
         nexthll 3
         ld (hl),$c
cont2a   ld a,h
         sub 40
         ld h,a
cont6    inc hl
         ld a,(fi1+1)
         dec a
         jr nz,cont5

         ld a,(fi2+1)
         cp c
         jr nz,cont5

         ld a,(m10+1)
         cp b
         call z,crsrpg
cont5    djnz loop1

         dec c
         jr z,cont8

         ld de,m1+2
         ld a,(de)
         inc a
         ld (de),a
         ld de,80-16
         add hl,de
         jp loop2

cont8    ld de,(~(80*7))+1
         add hl,de
         ld c,tilesize  ;b=0
         add ix,bc
         dec iyl
         jp nz,loop4

         dec iyh
         ret z

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
         nexthlds 8
         nexthlls
         endm
         ld (hl),a
         jp cont2a
         endp

showscn  call infoout
         ld a,(zoom)
         or a
         jp nz,showscnz

         ld a,(startp+1)
         or a
         jp z,crsrset

xcont2   ld a,(pseudoc)
         or a
         jp nz,showscnp

showscn2 proc      ;must be after showscn
         local loop
         ld c,$c0
         ld hl,(startp)
         ld a,h
loop     ld iyh,a
         ld a,l
         ld iyl,a
         ld e,(iy+video)
         ld d,(iy+video+1)
         vidmac
         vidmaca
         vidmac
         vidmaca
         vidmac
         vidmaca
         vidmac
         vidmaca
         vidmac
         vidmaca
         vidmac
         vidmaca
         vidmac
         vidmaca
         vidmac
         ld h,(iy+next+1)
         xor a
         or h
         jp z, crsrset

         ld l,(iy+next)
         jp loop
         endp

vidmacx  proc
         ld c,$c0
         ld a,(crsrbyte)
         add a,iyl
         ld l,a
         ld a,iyh
         adc a,0
         ld h,a
         vidmac
         ret
         endp

infoout  proc    ;must be before showtinfo
         ld hl,gencnt
         ld b,7
         ld de,$c782
         call digiout

         ld hl,cellcnt
         ld b,5
         ld de,$c792
         call digiout
         endp

showtinfo  proc          ;must be after infoout
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
           ld h,high(ttab)
           add a,low(ttab)
           ld l,a
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

showscnp proc
         local loop
         ld iy,(startp)
         ld h,high(pctable)
         ld c,3
loop     ld e,(iy+video)
         ld d,(iy+video+1)
         vidmacp 0,count0
         vidmacpa
         vidmacp 1,count1
         vidmacpa
         vidmacp 2,count2
         vidmacpa
         vidmacp 3,count3
         vidmacpa
         vidmacp 4,count4
         vidmacpa
         vidmacp 5,count5
         vidmacpa
         vidmacp 6,count6
         vidmacpa
         vidmacp 7,count7
         ld a,(iy+next+1)
         or a
         jp z,crsrset

         ld b,(iy+next)
         ld iyh,a
         ld iyl,b
         jp loop
         endp

vidmacpx proc   ;in: iy,de
         local m0,m1,m2,m3,m4
         ld c,3
         ld a,(crsrbyte)
         ld (m0+2),a
         rlca
         rlca
         add a,count0
         ld (m1+2),a
         inc a
         ld (m2+2),a
         inc a
         ld (m3+2),a
         inc a
         ld (m4+2),a
         ld h,high(pctable)
m0       ld a,(iy)
         ld b,a
         rlca
         rlca
         and c
         ld l,a
m1       ld a,(iy)
         rlca
         rlca
         rlca
         rlca
         and $c
         or l
         ld l,a
         ld a,(hl)
         ld (de),a

         inc e
         ld a,b
         rrca
         rrca
         rrca
         rrca
         and c
         ld l,a
m2       ld a,(iy)
         rrca
         and $c
         or l
         ld l,a
         ld a,(hl)
         ld (de),a

         inc e
         ld a,b
         rrca
         rrca
         and c
         ld l,a
m3       ld a,(iy)
         rrca
         and $c
         or l
         ld l,a
         ld a,(hl)
         ld (de),a

         inc e
         ld a,b
         and c
         ld l,a
m4       ld a,(iy)
         and $c
         or l
         ld l,a
         ld a,(hl)
         ld (de),a
         ret
         endp

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
         ld a,(crsrbit)
         ld hl,$201          ;it is the b-input for crsrcalc
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

crsrset  call crsrset1
         ld a,(zoom)
         or a
         ret nz

pixel11  ld a,(de)  ;must be after crsrset
         or b
         ld (de),a
         ret

clrrectlo  proc       ;in: x8poscp, y8poscp
         local lltpc,lrtpc,lx,lxpc11,lxpc01,m7,m8
         local x8pos,x8poscp,x8bit,y8pos,y8poscp,y8byte,mask,localbase
         local looplt,looprt,loopdn,loopup,looprt1,looplt1,looprtpc,loopltpc
         local xmove,nextrt,nextlt
         local clrect1,clrect2,clrect2pc,clrect3,xclrect
localbase equ $fff0         ;link to drawrect!
x8pos    equ localbase
x8poscp  equ localbase+1    ;link to drawrect!
x8bit    equ localbase+2
y8pos    equ t1
y8poscp  equ localbase+3    ;link to drawrect!
y8byte   equ localbase+4
mask     equ localbase+5

         ld a,(ydir)
         or a
         jr nz,loopup

loopdn   call xclrect
         ret z

         ld hl,y8byte
         inc (hl)
         ld a,(hl)
         cp 8
         jr nz,loopdn

         ld a,down
         call vnextcelllo
         xor a
         ld (y8byte),a
         jr loopdn

loopup   call xclrect
         ret z

         ld a,(y8byte)
         dec a
         ld (y8byte),a
         jp p,loopup

         ld a,up
         call vnextcelllo
         ld a,7
         ld (y8byte),a
         jr loopup

xclrect  push iy
         call xmove
         pop iy

         ld a,(x8poscp)
         ld (x8pos),a
         ld a,(crsrbit)
         ld (x8bit),a
         ld hl,y8pos
         dec (hl)       ;sets ZF
         ret

xmove    ld a,(xdir)
         or a
         jr nz,looplt

looprt   call clrect1
         ld a,(pseudoc)
         or a
         ld a,$80
         ld (hl),a
         jr nz,lrtpc

looprt1  call clrect2
         ret c
         ret z

         ld a,(hl)
         srl a
         srl a
         jr z,nextrt

         ld (hl),a
         inc e
         jr looprt1

lrtpc    call clrect3
looprtpc call clrect2pc
         ret c
         ret z

         ld a,(hl)
         srl a
         srl a
         jr z,nextrt

         ld (hl),a
         inc e
         jr looprtpc

nextrt   ld a,right
         call vnextcelllo
         jr looprt

looplt   call clrect1
         inc e
         inc e
         inc e
         ld a,(pseudoc)
         or a
         ld a,2
         ld (hl),a
         jr nz,lltpc

looplt1  call clrect2
         ret c
         ret z

         ld a,(hl)
         sla a
         sla a
         jr z,nextlt

         ld (hl),a
         dec e
         jr looplt1

lltpc    call clrect3
loopltpc call clrect2pc
         ret c
         ret z

         ld a,(hl)
         sla a
         sla a
         jr z,nextlt

         ld (hl),a
         dec e
         jr loopltpc

nextlt   ld a,left
         call vnextcelllo
         jr looplt

clrect1  ld e,(iy+video)
         ld d,(iy+video+1)
         ld a,(y8byte)
         ld (m8+2),a
         ld b,a
         rlca
         rlca
         rlca
         add a,d
         ld d,a
m8       ld c,(iy)
         ld hl,x8bit
         ret

clrect2  rrca
         or (hl)
         and c
         ;ld a,0     ;00
         jr z,lx

         ld a,$c0   ;11
         jp pe,lx

         ld a,(hl)
         and c
         ld a,$40   ;01
         jr z,lx

         rlca       ;10
lx       ld (de),a
         ld a,(x8pos)
         sub 2
         ld (x8pos),a
         ret

clrect2pc rrca
         or (hl)
         ld (mask),a
         and c
         ;ld a,0
         jr z,lx
         jp pe,lxpc11

         ld a,(hl)
         and c
         jr z,lxpc01

         ld a,(hl)     ;lxpc10
         and b
         ld a,8
         jr z,lx       ;1000

         ld a,$80      ;1010
         jr lx

lxpc11   ld a,(mask)
         and b
         ld a,$c
         jr z,lx       ;1100

         ld a,$c0
         jp pe,lx      ;1111

         ld a,(hl)
         and b
         ld a,$48      ;1101
         jr z,lx

         ld a,$84      ;1110
         jr lx

lxpc01   ld a,(hl)
         rrca
         and b
         ld a,4
         jr z,lx       ;0100

         ld a,$40
         jr lx         ;0101

clrect3  ld a,b
         ld b,c
         ;??add a,pc
         ld (m7+2),a
m7       ld a,(iy)
         ld c,b
         ld b,a
         ld hl,x8bit
         ld a,(hl)
         ret
         endp

