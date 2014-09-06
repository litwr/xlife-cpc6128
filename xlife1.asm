           include "cpc.mac"
           include "printn.inc"
           org $40

start      call prg
           call CAS_IN_DIRECT
           call CAS_IN_CLOSE

fn         db "XLIFE2.BIN"

prg        push hl
           ld hl,(icurdev)
           ld a,(hl)
           pop hl
           push af
           ld c,7
           call KL_INIT_BACK   ;restore AMSDOS
           pop af
           scf
           ccf
           sbc hl,de
           inc hl
           push hl
           ld hl,(icurdev)
           ld (hl),a

           ld a,4
           call KL_BANK_SWITCH   ;get ext 16K

           ld bc,end1-start1
           ld de,$4000
           ld hl,start1
           ldir

           ld b,prg-fn
           ld hl,fn
           ld de,$c000
           call CAS_IN_OPEN
           pop hl
           jr nc,error1

           ccf
           sbc hl,bc
           jr c,error2

           xor a
           call KL_BANK_SWITCH   ;get main 16K

           ld hl,fn
           ret

error2     call printn
           db "Not enough memory$"

error1     call KM_WAIT_CHAR
           pop hl
           ret

start1
           incbin "cpchi.bin"
end1
           end start

