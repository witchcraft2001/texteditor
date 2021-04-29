        DEVICE ZXSPECTRUM128
;  ZX / IBM Text EDITOR  ver.1.0
;   October 1993   Hohlov Oleg
;        ___           ___
;        ___  Project  ___

        org #4180 - #16
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
	dw	EntryExec - #80
EntryExec

        include "routines.asm"
        include "editor.asm"
        include "menu.asm"

; Font6   insert "Bold.fnt"

END
        DB 13
StartText
        DB 13,0
EndText

SCR_BUF equ #C000
SCR_BUF1 equ #C000 + 80*32*2
LineBuff equ #4100
SafeStack equ #8040

Colors.Black    equ 0
Colors.Blue     equ 1
Colors.Green    equ 2
Colors.Cyan     equ 3
Colors.Red      equ 4
Colors.Magenta  equ 5
Colors.Yellow   equ 6
Colors.White    equ 7
Colors.Bright   equ 8
Colors.Flash    equ #80


ExeEnd
        savebin	"ted.exe",EXEhead,ExeEnd-EXEhead
