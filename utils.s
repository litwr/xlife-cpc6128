;**boxsz
;**adddensity

inccurrp  ld a,tilesize
         add a,iyl
         ld iyl,a
         ret nc

         inc iyh
         ret

boxsz    proc
         local loop0,loop2,loop3,cont2,cont3,cont4,cont5,cont6,cont7,cont8
         local curx

         ;xmin - d, ymin - e
         ;xmax - b, ymax - c
curx     equ t1     ;connected to infov
         ;cury - h

         ld de,$a0c0
         xor a
         ld b,a
         ld c,a
         ld (curx),a
         ld h,a
         ld iy,tiles
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
         jp z,cont7

         ld ixl,a
         ld l,$ff
loop2    rlca
         inc l
         jr nc,loop2

         ld a,(curx)
         rlca
         rlca
         rlca
         ld ixh,a
         add a,l
         cp d
         jr nc,cont2

         ld d,a
cont2    ld a,ixl
         ld l,8
loop3    rrca
         dec l
         jr nc,loop3

         ld a,ixh
         add a,l
         cp b
         jr c,cont3

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
cont4    pop iy
         ld a,h
         rlca
         rlca
         rlca
         ld ixh,a
         add a,l
         cp e
         jr nc,cont5

         ld e,a
cont5    push iy
         push de
         ld de,7
         ld l,e
         add iy,de
         pop de
         xor a
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
         jr nz,cont6

         dec l
         dec iy
         call calllo1
cont6    pop iy
         ld a,ixh
         add a,l
         cp c
         jr c,cont7

         ld c,a
cont7    call inccurrp
         ld a,(curx)
         inc a
         cp hormax
         jr z,cont8

         ld (curx),a
         jp loop0

cont8    xor a
         ld (curx),a
         inc h
         ld a,h
         cp vermax
         jp nz,loop0

         ld a,c
         sub e
         inc a
         ld h,a
         ld a,b
         sub d
         inc a
         ld (curx),a
         ld a,(tiles)   ;it is at the low mem
         or c
         or b
         ret
         endp

calccells1 proc   ;OUT: HL;  USED: A,BC,DE
         local ll,ls
         ;ld hl,calccells
         ;call calllo
         ld hl,readlow
         ld (jsrfar+1),hl
         ld hl,0
         ld bc,cellcnt
         ld de,10000
         call ls
         ld de,1000
         call ls
         ld de,100
         call ls
         ld e,10
         call ls
         ld e,1
ls       ld a,(bc)
         inc bc
         or a
ll       ret z

         add hl,de
         dec a
         jr ll
         endp

