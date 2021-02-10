;live, born - word
;fillrt
;setrconst

fillrt1  proc
         local l2
         ld b,a
         or a
         ld a,1
         ret z

l2       rla
         djnz l2
         ret
         endp

fillrtsl proc
         add a,e
         call fillrt1
         ld ixl,a  ;adjcell -> ix
         ld a,0
         rla
         ld ixh,a
         ld a,l
         ret
         endp

fillrtsr proc
         adc a,0
         call fillrt1
         ld iyl,a   ;adjcell2 -> iy
         ld a,0
         rla
         ld iyh,a
         ret
         endp

fillrt2  proc
         local l1,l2,l3
         jr nc,l1

         ld a,(live)
         and ixl
         jr nz,l2

         ld a,(live+1)
         and ixh
         jr z,l3

l2       sla d
         ld a,(hl)
         or d
         ld (hl),a
         srl d
         jr l3

l1       ld a,(born)
         and ixl
         jr nz,l2

         ld a,(born+1)
         and ixh
         jr nz,l2

l3       endp

         proc
         local l1,l2
         ld a,e
         or a
         jr z,l1

         ld a,(live)
         and iyl
         jr nz,l2

         ld a,(live+1)
         and iyh
         ret z

l2       ld a,(hl)
         or d
         ld (hl),a
         ret

l1       ld a,(born)
         and iyl
         jr nz,l2

         ld a,(born+1)
         and iyh
         jr nz,l2

         ret
         endp

fillrt   proc
         local l0
         ld hl,gentab
l0       ld d,1        ;x -> l, t1 -> d
         ld (hl),0
         ld a,l
         and d
         ld e,a
         ld a,l
         and $e0
         rrca
         rrca
         rrca
         rrca
         rrca  ;CY=0
         ld c,a
         call fillrtsl
         and $1e
         rrca
         rra
         push af  ;saves CY
         call fillrtsr
         pop af  ;restore CY
         call fillrt2
         ld d,4
         ld a,l
         and 8
         rrca
         rrca
         rrca   ;CY=0
         ld e,a
         ld a,c
         call fillrtsl
         and $10
         rlca
         rlca
         rlca
         rla
         rl c
         ld a,l
         and 7
         rr c
         push af
         call fillrtsr
         pop af
         push af
         call fillrt2
         ld d,16
         pop af
         call fillrt2
         ld d,64
         ld a,l
         and d
         rlca
         rla
         adc a,0
         ld e,a
         ld a,l
         and $38
         rrca
         rrca
         rrca
         call fillrtsl
         rla
         rl c
         ld a,l
         and 7   ;sets CY=0
         rr c    ;restores CY
         push af
         call fillrtsr
         pop af
         call fillrt2
         inc l
         jr nz,l0
         ret
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

        ld a,"8"
        call showr1
        ret z

cont4   ld a,"/"
        call showr1
        ret z

        ld a,1
loop4   ld c,a
        and (ix+2)
        ld a,c
        jr nz,cont5

loop5   sla a
        jr nz,loop4

        ld a,(ix+3)
        or a
        ret z

        ld a,"8"
        jr showr1

cont5   call showr0
        jr nz,loop5
        ret

cont1   call showr0
        jr nz,loop2
        ret

showr0  ld c,a
        ld e,$ff
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

showrules2 proc
        local loop1,loop2,loop3,loop4,loop5,cont4,showr0
        ld c,1
loop1   ld a,(live)
        and c
        call nz,showr0

loop2   sla c
        jr nz,loop1

        ld a,(live+1)
        or a
        jr z,cont4

        ld a,"8"
        call TXT_OUTPUT
cont4   ld a,"/"
        call TXT_OUTPUT
        ld c,1
loop4   ld a,(born)
        and c
        call nz,showr0

loop5   sla c
        jr nz,loop4

        ld a,(born+1)
        or a
        ret z

        ld a,"8"
        jp TXT_OUTPUT

showr0  ld b,$ff
loop3   inc b
        rrca
        jr nc,loop3

        ld a,b
        xor "0"
        jp TXT_OUTPUT
        endp

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

         call delchr
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

         call delchr
         jr cont4
         endp

