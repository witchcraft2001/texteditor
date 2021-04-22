        DEVICE ZXSPECTRUM128
;  ZX / IBM Text EDITOR  ver.1.0
;   October 1993   Hohlov Oleg
;        ___           ___
;        ___  Project  ___

        org #4200
        include "dss_equ.asm"
	include "bios_equ.asm"
	include "sp_equ.asm"	
	
EXEhead
                db	"EXE"
		db	0			; +3
		dw	EntryExec-EXEhead
		dw	0x0000			; +4
		dw	ExeEnd-EntryExec	; +8
		dw	0, 0			; +10
		dw	0			; +14
		dw	EntryExec		; +16
		dw	EntryExec
		dw	#4200
EntryExec
        include "ed.1.a80"
        include "ed.2.a80"
        include "ed.3.a80"

; Font6   insert "Bold.fnt"

END
        DB 13
StartText        
        DB "Hello friends! This is ported version of ZX/IBM Editor",13
        DB "Original version by Hohlov Oleg, (c) 1993.",13
        DB "Ported by Mikhaltchenkov Dmitry, (c) 2021.",13,13,0
EndText
ExeEnd
        savebin	"editor.exe",EXEhead,ExeEnd-EXEhead
