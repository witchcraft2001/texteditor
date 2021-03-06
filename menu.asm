;________________________________

;  ZX/IBM Text Editor ver.1.0
;        _____________
;       __           __
;      __  MAIN MENU  __
;________________________________

Prt_HL_e
        dec e
        ret m
        ld a,(hl)
        inc hl
        call Print
        jr Prt_HL_e

;█ █ █ Пункт меню ~File~ █ █ █ 

FILES   ld a,15:ld hl,#0101
        ld de,#0C0E:call OpenWindow
        call OutFS
        DB 22,2,3,"New"
        DB 22,3,1,DIVIDER,14
        DB 22,4,3,"Load"
        DB 22,5,3,"Merge"
        DB 22,6,1,DIVIDER,14
        DB 22,7,3,"Save"
        DB 22,8,3,"save As"
        DB 22,9,3,"save Block"
        DB 22,10,1,DIVIDER,14
        DB 22,11,3,"Quit",0
        ld hl,FileMenu
        call Menu
        jp MAIN2

FileMenu DB 0,7
         DB 2,2,12,"n":DW New
         DB 4,2,12,"l":DW LoadText
         DB 5,2,12,"m":DW Merge
         DB 7,2,12,"s":DW SaveText
         DB 8,2,12,"a":DW SaveTextAs
         DB 9,2,12,"b":DW SaveBlock
         DB 11,2,12,"q":DW Quit

Quit    ld a,(IsModified)
        and a
        jr z,SureQuit        
        call NotSavedDialog
        jr nc,SureQuit
        jp MAIN2

SureQuit
        call WinBack
        ld sp,(SaveSP)
        ld a,(hMem)
        ld c,Dss.FreeMem
        rst #10
        ld bc,Dss.Exit
        rst #10

New     ld a,(IsModified)
        and a
        jr z,.saved
        call NotSavedDialog
        jp c,MAIN2
.saved  call BegText
        call RemoveBlock
        ld (hl),13
        inc hl
        ld a,(EOLN_Fl)
        or a
        jr z,New1
        ld (hl),10
        inc hl
New1    ld (SPACE),hl
        ld (hl),0
        call SetUnmodifiedFile
        ld hl,.nameless
        ld de,PrintFilename.dosName
        ld bc,13
        ldir
        ld hl,FileName
        ld d,h
        ld e,l
        ld (hl),0
        ld bc,127
        ldir
        jp EDIT
.nameless
        db "NAMELESS.TXT",0
 ;H -Y-KOOPДИHATA OKHA,
 ;DE-AДPEC БУФEPA ИMEHИ.

InpFlName
        push de
        ld de,#0524
        ld l,2
        ld a,%00010111
        call OpenWindow
        ld l,10
        inc h
        ld (PrintXY),hl
        call OutFS
        DB "Enter file name:",0
        inc h
        inc h
        ld l,4
        ld (PrintXY),hl
        pop de
        ld c,32
        call Input
        call CleanFileName
        cp 13
        scf
        ret nz
        ; ex hl,de
        ; push hl
        ; call IBM_MakeName
        ; pop de
        ld a,(de)
        cp 32
        ret
CleanFileName
        push de
        push af
.loop   ld a,(de)
        and a
        jr z,.end
        cp 13
        jr z,.end        
        inc de
        jr .loop
.end    xor a
        ld (de),a
        pop af
        pop de
        ret
hFile   db 0

SaveOverwriteMenu
        DB 0,2
        DB 17,33,7,"y":DW SaveTextAs.continue
        DB 17,42,6,"n":DW MAIN2

SaveText
        ld a,(IsModified)
        and a
        jp z,MAIN2
        ld a,(FileName)
        and a
        jr z,SaveTextAs
        ld hl,FileName
        jp SaveTextAs.save
SaveTextAs
        ld hl,FileName
        ld de,FlNameBuff
        push de
        ld bc,128
        ldir
        pop de
        ld h,3
        ld de,FlNameBuff
        call InpFlName
        jp c,MAIN2
        ld hl,LineBuff
        ld e,l
        ld d,h
        inc de
        ld (hl),0
        ld bc,44
        ldir
        ld hl,FlNameBuff
        ld bc,Dss.F_First
        ld de,LineBuff
        ld a,FileAttrib.Arch
        rst #10
        jr c,.continue
        ld a,(hl)
        and a
        jr z,.continue
        ld a,Colors.Yellow*16
        ld hl,#0C19
        ld de,#071E:call OpenWindow
        call OutFS
        DB 22,14,29,"File exists, overwrite?"
        db 22,16,25,DIVIDER,30
        DB 22,17,35,"Yes      No",0        
        ld hl,SaveOverwriteMenu
        call Menu
        jp MAIN2
.continue
        ld hl,FlNameBuff
.save   call CopyFileName
        ld hl,(TEXT)
        ld de,(SPACE)
SvText1 call SaveFile
        jp nc,EDIT
        ld a,%00110000
        ld hl,#0709
        ld de,#0319
        call OpenWindow
        call OutFS
        DB 22,8,13,"Writing error ...",0
        call Waitkey
        jp EDIT

;Сохраняет блок текста по адресу HL длиной DE в файл с названием в FlNameBuff
;по выходу CF=1 - ошибка записи
SaveFile
        ex de,hl
        or a
        sbc hl,de
        ex de,hl
        push hl
        push de
        ld a,(FlNameBuff+1)
        cp ":"
        call nz,RestoreCurDir
        ld c,Dss.Create
        ld hl,FlNameBuff
        ld a,FileAttrib.Arch
        rst #10
        pop de
        pop hl
        ret c
        ld (hFile),a
        ld c,Dss.Write
        rst #10
        call CloseFile
        ex af,af'
        call SetUnmodifiedFile
        ex af,af'
        ret

;Запись блока текста в файл
SaveBlock
        call BlockExist
        jp nc,FILES
        ld h,7
        ld de,FlNameBuff
        call InpFlName
        jp c,MAIN2
        ld hl,(BlockBeg)
        ld de,(BlockEnd)
        jr SvText1
        
WriteClipboardFile
        call BlockExist
        ret nc
        ld de,LineBuff
        ld hl,AppDir
        push de
        call CopyString
        pop de
        ld hl,ClipboardFileName
        push de
        call ConcatString
        pop hl
        ld c,Dss.Create
        ld a,FileAttrib.Arch
        rst 10h
        ret c
        ld (.file),a
        ex af,af'
        ld de,(BlockBeg)
        ld hl,(BlockEnd)
        push de
        and a
        sbc hl,de
        ex hl,de
        pop hl
        ex af,af'
        ld c,Dss.Write
        rst 10h
.close  ld a,0
.file   equ $-1
        ld c,Dss.Close
        rst 10h
        ret

InsertClipboardFile
        ld de,LineBuff
        ld hl,AppDir
        push de
        call CopyString
        pop de
        ld hl,ClipboardFileName
        push de
        call ConcatString
        pop hl
        ld c,Dss.Open
        ld a,FileAttrib.Arch
        rst 10h
        ret c
        ld (.file),a
        ld hl,0
        push hl
        pop ix
        ld bc,2 * 256 + Dss.Move_FP
        rst 10h
        ld a,h
        or l
        jr nz,.close            ;file too big
        push ix
        pop hl
        ld (.clipLen),hl
        ld de,(SPACE)
        add hl,de
        ld de,(RAMTOP)
        call cp_de_hl
        jr c,.close             ;file too big
        ld hl,(LineAddr)
        push ix
        push hl
        push hl
        push ix
        pop de
        add hl,de
        ex hl,de
        pop hl
        ld bc,(SPACE)
        call MoveMem
        ld hl,0
        push hl
        pop ix
        ld bc,Dss.Move_FP
        ld a,(.file)
        rst 10h
        pop hl
        pop de
        ld c,Dss.Read
        ld a,(.file)
        rst 10h
        ld de,0
.clipLen equ $-2
        ld hl,(SPACE)
        add hl,de
        ld (SPACE),hl
        ld hl,(LineAddr)
        ld (BlockBeg),hl
        add hl,de
        ld (LineAddr),hl
        ld (BlockEnd),hl
.close  ld a,0
.file   equ $-1
        ld c,Dss.Close
        rst 10h        
        ret

ClipboardFileName
        db "CLIPBRD.TXT",0

Merge   ld h,8
        ld de,FlNameBuff
        call InpFlName
        jp c,MAIN2
        ld hl,(SPACE)
        xor a
        call LoadTextFile
        ex af,af'
        call SetModifiedFile
        ex af,af'
        jp nc,EDIT
Cat1    call Waitkey
        jr z,Cat1
        jp MAIN2

FileName   ds 128
FlNameBuff ds 128

NotSavedDialog
        call StoreScrn
        ld a,Colors.Yellow*16
        ld hl,#0B19
        ld de,#081E:call OpenWindow
        call OutFS
        DB 22,13,31,"File is not saved!"
        DB 22,14,28,"Do you want to proceed?"
        db 22,16,25,DIVIDER,30
        DB 22,17,35,"Yes      No",0        
        ld hl,NotSavedDlgMenu
        call Menu
.no     call RestoreScrn
        scf
        ret
.yes    call RestoreScrn
        and a
        ret

NotSavedDlgMenu
        DB 0,2
        DB 17,33,7,"y":DW NotSavedDialog.yes
        DB 17,42,6,"n":DW NotSavedDialog.no

LoadText
        ld a,(IsModified)
        and a
        jr z,SureLoad
        call NotSavedDialog
        jp c,MAIN2
SureLoad
        ld h,4
        ld de,FlNameBuff
        call InpFlName
        jp c,MAIN2
        call BegText
        ld hl,(TEXT)
        xor a
        call LoadTextFile
        jp c,Cat1
        ld hl,FlNameBuff
        call CopyFileName
        call SetUnmodifiedFile
        jp EDIT

LoadTextFile
        ld (.flag),a
        push hl
        push hl
        inc hl
        ld a,(hl)
        cp ":"
        call nz,RestoreCurDir
        pop hl
        push de
        ex hl,de
        ld c,Dss.Open
        ld a,1
        rst #10        
        pop de
        pop hl
        jr nc,LoadTxt2
        ld a,0
.flag   equ $-1
        and a
        jr nz,.exit
        ld a,%00110000
        ld hl,#0709
        ld de,#031A:call OpenWindow
        call OutFS
        DB 22,8,13
.fileNotFound
        db "File not found ...",0
.exit   ld hl,LoadTextFile.fileNotFound
        scf
        ret
LoadTxt2
        ld (hFile),a
        ld de,(RAMTOP)
        ex de,hl
        or a
        sbc hl,de
        ex de,hl
        ld c,Dss.Read
        push hl
        rst #10        
        call CloseFile
        pop hl
        jr nc,LoadTxt21
        ld a,(LoadTextFile.flag)
        ld hl,.readError
        and a
        scf
        ret nz
        ld a,%00110000
        ld hl,#0709
        ld de,#0319
        call OpenWindow
        call OutFS
        DB 22,8,13
.readError
        db "Reading error ...",0
        scf
        ret
LoadTxt21        
        add hl,de
        ld (SPACE),hl
        ld (hl),0
        ld a,(EOLN_Fl)
        or a
        jr z,LoadTxt4
        dec hl
        ld (hl),10
LoadTxt4
        dec hl
        ld (hl),13
        call RemoveBlock
.end    and a
        ret
CloseFile
        push af
        push hl
        push de
        ld a,(hFile)
        ld c,Dss.Close
        rst #10
        pop de
        pop hl
        pop af
        ret

;█ █ █ Пункт меню ~Print~ █ █ █

PRINT    ld a,15:ld hl,#010C
         ld de,#050F:call OpenWindow
         call OutFS
         DB 22,2,14,"Print text"
         DB 22,3,14,"print Block"
         DB 22,4,14,"page Size",0
         ld hl,PRINT_Menu
         call Menu:jp MAIN2

PRINT_Menu DB 0,3
           DB 2,13,13,"p":DW PRT_Text
           DB 3,13,13,"b":DW PRT_Block
           DB 4,13,13,"s":DW Page_Size

PageSize   DW 0

Dr_PrtA push hl
        push de
        push bc
        ld c,Dss.Print
        call CallDss
        pop bc
        pop de
        pop hl
        ret nc
        jp PRT_Error

PRT_Error
        ld sp,(SaveSP)
        ld a,%01010111
        ld hl,#040C
        ld de,#051D
        call OpenWindow
        call OutFS
        DB 22,6,18
        DB "Printer Error ...",0
        jp Cat1

PRT_Text   ld hl,(TEXT):ld de,(SPACE)
           jr PRT_0

PRT_Block  call BlockExist:jp nc,PRINT
           ld hl,(BlockBeg)
           ld de,(BlockEnd)
PRT_0      
PRT_1   ld bc,(PageSize)
PRT_2   call cp_hl_de
        jp nc,MAIN2
        ld a,(hl):inc hl
        cp SPC:jr nz,PRT_4
        push bc:ld b,(hl)
        inc hl:res 7,b
PRT_3   ld a,32:call Dr_PrtA
        djnz PRT_3
        pop bc:jr PRT_2
PRT_4   cp 13:jr nz,PRT_5
        ld a,(hl):cp 10
        jr nz,$+3:inc hl
        ld a,13:call Dr_PrtA
        ld a,10:call Dr_PrtA
        dec bc:ld a,b:or c
        jr nz,PRT_2
        call PRT_Wait:jr PRT_1
PRT_5   call Dr_PrtA
        jr PRT_2

PRT_Wait
        push hl:push de
        call StoreScrn
        ld a,%00010111:ld hl,#040C
        ld de,#0411:call OpenWindow
        call OutFS
        DB 22,5,17,"Insert new page than"
        DB 22,6,18, "press any key ...",0
        call Waitkey:jr z,$-3
        call RestoreScrn
        pop de:pop hl:ret

Page_Size
        ld a,%00010111:ld hl,#050C
        ld de,#051A:call OpenWindow
        call OutFS
        DB 22,6,17,"Page Size: ",0
        ld hl,(PageSize):ld a,3
        call DecHL:call OutFS
        DB 22,7,17,"Enter new value:"
        DB 22,8,25,0
        ld de,PgSizeBuff:ld c,5
        call Input:cp 13:jp nz,MAIN2
        ld a,(de):cp 32:jp c,MAIN2
        ex de,hl:call MakeNumber
        ld (PageSize),de:jp MAIN2

PgSizeBuff DS 6

;█ █ █ Пункт меню ~SetUp~ █ █ █ 

SETUP   ld a,(AutoBrackets)
        and a
        ld a,#20
        jr z,.noBrackets
        ld a,#fb
.noBrackets
        ld (.brackets),a
        ld a,(Comprs_Fl)
        and a
        ld a,#20
        jr z,.noCompress
        ld a,#fb
.noCompress
        ld (.compress),a
        ld a,15
        ld hl,#0116
        ld de,#0716:call OpenWindow
        call OutFS
        DB 22,2,24,"EOLN code"
        DB 22,3,24,"Compress",SPC,9
.compress
        db #f9
        DB 22,4,24,"auto Brackets",SPC,4
.brackets
        db #f9
        db 22,5,22,DIVIDER,22
        DB 22,6,24,"Save settings",0
        ld a,(EOLN_Fl)
        and a
        jr z,.cr
        call OutFS
        DB 22,2,37,"CR/LF",0
        jr .next
.cr     call OutFS
        DB 22,2,40,"CR",0
.next   ld hl,SetUpMenu
        call Menu:jp MAIN2

SetUpMenu DB 0,4
          DB 2,23,20,"e":DW SwitchEOLN
          DB 3,23,20,"c":DW SwitchCompress
          DB 4,23,20,"b":DW SwitchBrackets
          DB 6,23,20,"s":DW SaveSettings

SwitchBrackets
        ld hl,AutoBrackets
        jr SwitchSettings
SwitchCompress
        ld hl,Comprs_Fl
        jr SwitchSettings
SwitchEOLN
        ld hl,EOLN_Fl
SwitchSettings
        ld a,(hl)
        and a
        ld a,1
        jr z,.next
        xor a
.next   ld (hl),a
        jp SETUP

ReadSettings
        ld de,LineBuff
        ld hl,AppDir
        push de
        call CopyString
        pop de
        push de
        ld hl,SettingsFileName
        call ConcatString
        pop hl
        ld c,Dss.Open
        ld a,1
        rst 10h
        ret c
        ld (.file),a
        ld hl,LineBuff
        ld de,6
        ld c,Dss.Read
        rst 10h
        jr c,.close
        ld hl,(LineBuff)
        ld de,#4554
        and a
        sbc hl,de
        jr nz,.close
        ld hl,LineBuff+2
        ld a,(hl)
        cp "C"
        jr nz,.close
        inc hl
        ld a,(hl)               ;End Of Line param
        ld (EOLN_Fl),a
        inc hl
        ld a,(hl)               ;Compress param
        ld (Comprs_Fl),a
        inc hl
        ld a,(hl)               ;AutoBrackets param
        ld (AutoBrackets),a
.close  ld a,0
.file   equ $-1
        ld c,Dss.Close
        rst 10h
        ret

SaveSettings
        ld de,LineBuff
        ld hl,AppDir
        push de
        call CopyString
        pop de
        push de
        ld hl,SettingsFileName
        call ConcatString
        pop hl
        ld c,Dss.Create
        ld a,FileAttrib.Arch
        rst 10h
        jp c,MAIN2
        ld (.file),a
        ex af,af'
        ld hl,LineBuff
        push hl
        ld (hl),"T"
        inc hl
        ld (hl),"E"
        inc hl
        ld (hl),"C"
        inc hl
        ld a,(EOLN_Fl)
        ld (hl),a               ;End Of Line param
        inc hl
        ld a,(Comprs_Fl)
        ld (hl),a               ;Compress param
        inc hl
        ld a,(AutoBrackets)
        ld (hl),a               ;AutoBrackets param
        pop hl
        ex af,af'
        ld de,6
        ld c,Dss.Write
        rst 10h
.close  ld a,0
.file   equ $-1
        ld c,Dss.Close
        rst 10h
        jp MAIN2
RestoreCurDir
        push hl
        push de
        push bc
        ld a,(CurrentDir)
        sub "A"
        ld c,Dss.ChDisk
        rst 10h
        ld hl,CurrentDir
        ld c,Dss.ChDir
        rst 10h
        pop bc
        pop de
        pop hl
        ret
SettingsFileName
        db "TED.CFG",0

;█ █ █ Пункт меню ~Info~ █ █ █

INFO    ld a,15:ld hl,#0b14
        ld de,#0A27:call OpenWindow
        call OutFS
        DB 22,12,26,"Text Editor v1.0 (May.2021)"
        DB 22,14,22,"based on ZX/IBM Text Editor sources"
        DB 22,15,23,"ver.1.0 (Oct.1993) by Hohlov Oleg"
        DB 22,16,24,"ported by Mikhaltchenkov Dmitry"
        db 22,17,20,DIVIDER,39
        DB 22,18,22,"Text Length: ",0
        ld hl,(SPACE):ld de,(TEXT)
        or a:sbc hl,de:ld a,3:call DecHL
        call OutFS
        DB 22,19,22,"Free Space:  ",0
        ld hl,(RAMTOP):ld de,(SPACE)
        or a:sbc hl,de:call DecHL
        jp Cat1

Comprs_Fl DB 0
EOLN_Fl   DB 1
;█ █ *** The END! *** (October 1993) █ █ 

