           org $4000
           include "xlife2.sym"
           include "interface.s"
           include "videohi.s"
           include "rules.s"
           include "iohi.s"
           include "ramdisk.s"
           include "ramdata.s"

stringbuf  db 0,0,0,0,0,0,0,0,0

initxt     ld hl,$119
           call TXT_SET_CURSOR
           ld a,1
           call TXT_SET_PEN
           ld a,"G"
           call TXT_WR_CHAR
           ld a,19
           call TXT_SET_COLUMN
           ld a,"%"
           call TXT_WR_CHAR
           ld a,33
           call TXT_SET_COLUMN
           ld a,"X"
           call TXT_WR_CHAR
           ld a,37
           call TXT_SET_COLUMN
           ld a,"Y"
           jp TXT_WR_CHAR

setcolor   proc
           local l2,l3
           ld a,(mode)
           or a
           ld a,(bgedit)
           jr z,l3

           ld a,(bggo)
l3         call setbg
           ld a,(livcellc)
           ld c,a
           ld b,a
           ld a,1
           call SCR_SET_INK
           ld a,(newcellc)
           ld c,a
           ld b,a
           ld a,2
           call SCR_SET_INK
           ld a,(topology)
           or a
           ld a,(bordertc)
           jr z, l2

           ld a,(borderpc)
l2         jp chgbr
           endp

printn     proc    ;chg: a
           local exit,loop
           ex (sp),hl
loop       ld a,(hl)
           inc hl
           cp "$"
           jr z,exit

           call TXT_OUTPUT
           jr loop

exit       ex (sp),hl
           ret
           endp

realnum1   db 0,0,0,0,0
realnum3   db 0,0,0,0,0
steps2bin  proc          ;in: de,l,h=0
;converts ascii int pointed by de to real at realnum1 and int4 at realnum2
           local loop
           xor a
           ld b,l
           ld c,a
           push de
           ld de,realnum1
           ld l,a
           call INTEGER_TO_REAL   ;0 -> realnum1
           pop de
loop       dec de
           ld a,(de)
           ld h,0
           xor $30
           push de
           ld de,realnum2
           ld l,a
           xor a
           call INTEGER_TO_REAL   ;bcd -> realnum2
           ld a,c
           push bc
           call REAL_POW_10   ;REAL *10^A
           push hl
           pop de
           ld hl,realnum1
           call REAL_ADDITION   ;realnum1+realnum2 -> realnum1
           pop bc
           pop de
           inc c
           djnz loop

           push hl
           pop de
           ld hl,realnum2
           call MOVE_REAL    ;realnum1 -> realnum2
           jp REAL_TO_BINARY ;at realnum2
           endp

calcspd    proc
           local cont1

           push hl
           pop bc
           ld hl,stringbuf
           ld a,c
           sub (hl)
           ld (hl),a
           ld c,a

           inc hl
           ld a,b
           sbc a,(hl)
           ld (hl),a
           ld b,a

           inc hl
           ld a,e
           sbc a,(hl)
           ld (hl),a
           ld e,a

           inc hl
           ld a,d
           sbc a,(hl)
           ld (hl),a
           or b
           or e
           ld hl,stringbuf
           jr nz,cont1

           ld a,c
           cp 3
           jr nc,cont1
           
           ld a,3
           ld (hl),a
cont1      xor a
           call BINARY_TO_REAL  ;timer -> stringbuf
           ld de,realnum2
           ld l,3
           xor a
           ld h,a
           call INTEGER_TO_REAL     ;3 -> realnum2
           push hl
           pop de
           ld hl,stringbuf
           call REAL_DIVISION     ;timer/3 -> stringbuf
           push hl
           pop de
           ld hl,realnum2
           call MOVE_REAL     ;timer/3 -> realnum2
           call printn
           db 12,"TIME: $"
           call sprint
           ld d,0
           call prints
           ld hl,realnum1
           ld de,realnum2
           call REAL_DIVISION   ;realnum1/realnum2 -> realnum1
           ld a,6
           call REAL_POW_10   ;REAL *10^A
           call printn
           db "s",$d,$a,"SPEED: $"
           call sprint
           ld d,1
           call prints
           jp KM_WAIT_CHAR
           endp

sprint     proc   ;in: hl
           local loop,loop1,loop2,cont1
           push hl
           pop de
           ld hl,realnum3
           push hl
           call MOVE_REAL     ;(de) -> (hl)
           ld hl,stringbuf
           ld b,9
           xor a
loop1      ld (hl),a
           inc hl
           djnz loop1

           pop ix
           ld a,(ix+3)
           or $80
           ld (ix+3),a
           ld a,(ix+4)
           and $7f
           ld b,a
           ld a,32
           sub b
           ld b,a
           xor a
loop       srl (ix+3)
           rr (ix+2)
           rr (ix+1)
           rr (ix)
           djnz loop
           
           ld hl,stringbuf
           adc a,b
           ld (hl),a 
           ld de,0
           ld iyl,0
           ld bc,1
loop2      srl (ix+3)
           rr (ix+2)
           rr (ix+1)
           rr (ix)
           jr nc,cont1

           ld a,(hl)
           add a,c
           daa
           ld (hl),a
           inc hl
           ld a,(hl)
           adc a,b
           daa
           ld (hl),a
           inc hl
           ld a,(hl)
           adc a,e
           daa
           ld (hl),a
           inc hl
           ld a,(hl)
           adc a,d
           daa
           ld (hl),a
           inc hl
           ld a,(hl)
           adc a,iyl
           ld (hl),a
           ld hl,stringbuf

cont1      ld a,c
           add a,a
           daa
           ld c,a
           ld a,b
           adc a,a
           daa
           ld b,a
           ld a,e
           adc a,a
           daa
           ld e,a
           ld a,d
           adc a,a
           daa
           ld d,a
           ld a,iyl
           adc a,a
           daa
           ld iyl,a
           ld a,(ix)
           or (ix+1)
           or (ix+2)
           or (ix+3)
           jr nz,loop2
           ret
           endp

prints     proc
           local cont1,cont2,cont3,loop1
           ld hl,stringbuf+4
           ld b,4
loop1      ld a,(hl)
           or a
           jr nz,cont1

           dec hl
           dec b
           jr loop1

cont1      ld c,a
           ld a,b
           inc b
           cp d
           jr z,cont2
           jr c,cont2

           ld a,c
           and $f0
           jr nz,cont2

           ld a,c
           or $30
           call TXT_OUTPUT
           dec hl
           dec b
cont2      ld a,b
           dec a
           cp d
           jr nz,cont3

           ld a,"."
           call TXT_OUTPUT
cont3      ld a,(hl)
           ld c,a
           and $f0
           rrca
           rrca
           rrca
           rrca
           or $30
           call TXT_OUTPUT
           ld a,c
           and $f
           or $30
           call TXT_OUTPUT
           dec hl
           djnz cont2
           ret
           endp

initprg    call loadcf
           ld a,1
           ld (errst),a
           call copyr
           call setcolor
           call split_on0
           jp help0

