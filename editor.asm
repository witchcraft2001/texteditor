        DEVICE ZXSPECTRUM128
;  ZX / IBM Text EDITOR  ver.1.0
;   October 1993   Hohlov Oleg
;        ___           ___
;        ___  Project  ___

        org 25200
        include "dss_equ.asm"
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
		dw	25200 - 1
EntryExec
        include "ed.1.a80"
        include "ed.2.a80"
        include "ed.3.a80"

; Font6   insert "Bold.fnt"

END     DB 13,13,0
ExeEnd
        savebin	"editor.exe",EXEhead,ExeEnd-EXEhead
