           include "cpc.mac"
           org $40

start      call prg
           call CAS_IN_DIRECT
           call CAS_IN_CLOSE

fn         db "XLIFE2.BIN"
prg        ld c,7
           call KL_INIT_BACK   ;restore AMSDOS

           ld a,4
           call KL_BANK_SWITCH   ;get ext 16K

           ld bc,end1-start1
           ld de,$4000
           ld hl,start1
           ldir

           xor a
           call KL_BANK_SWITCH   ;get main 16K

           ld b,prg-fn
           ld hl,fn
           push hl
           ld de,$c000
           call CAS_IN_OPEN
           pop hl
           ret

start1
           incbin "cpchi.bin"
end1
           end start


