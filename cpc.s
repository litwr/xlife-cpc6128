digiout  proc         ;in: b - length, de - scrpos, hl - data
         local loop
         ld c,$f
loop     ld a,(hl)
         push hl
         rlca
         rlca
         rlca
         add a,low(digifont)
         ld l,a
         ld h,high(digifont)
         domac
         dec e

         rept 5
         inc hl
         ld a,d
         add a,8
         ld d,a
         domac
         dec e
         endm

         inc hl
         ld a,d
         add a,8
         ld d,a
         domac

         pop hl
         inc hl
         ld a,d
         sub 6*8
         ld d,a
         inc e
         dec b
         jp nz,loop

         ret
         endp

callhi   proc           ;in: hl; changed: a
         local jsrfar

         ld (jsrfar+1),hl
         push af
         ld a,4
         call KL_BANK_SWITCH
         pop af
jsrfar   call $a000
         xor a
         jp KL_BANK_SWITCH
         endp

calllo   ld (jsrfar+1),hl   ;in: hl
calllo1  push af
         xor a
         call KL_BANK_SWITCH
         pop af
jsrfar   call 0
         push af
         ld a,4
         call KL_BANK_SWITCH
         pop af
         ret

readlow  proc
         local m1
         ld (m1+2),a
m1       ld a,(iy)
         ret
         endp

readhl   ld l,(iy)
         ld h,(iy+1)
         ret

setiy    ld (iy+sum),1
         ret

xoriy    ld a,(iy)
         xor d
         ld (iy),a
         ret

oriy     or (iy)
         ret

readde   ld e,(iy+video)      ;in: iy
         ld d,(iy+video+1)    ;out: de
         ret

fixvp    ld l,(iy+ul)
         ld h,(iy+ul+1)
         push hl
         pop iy
         ld l,(iy+left)
         ld h,(iy+left+1)
         ret

decint   proc
         local l1,l2,l3,l4
         ld hl,(realnum2)
         xor a
         or l
         jp nz,l1

         or h
         jp nz,l2

         ld de,(realnum2+2)
         or e
         jp nz,l3

         or d
         jp nz,l4

         scf
         ret

l4       dec d
l3       dec e
         ld (realnum2+2),de
l2       dec h
l1       dec l
         ld (realnum2),hl
         ret
         endp

bloop    proc
         local bl7,bl8
         ld hl,(tilecnt)
         ld a,l
         or h
         jp nz,bl7

         call incgen
         jp bl8

bl7      call generate
         call cleanup
bl8      call decint
         jp nc,bloop
         ret
         endp

