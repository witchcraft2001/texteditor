;________________________________

;  ZX/IBM Text Editor ver.1.0
;        _____________
;       __           __
;      __  MAIN MENU  __
;________________________________

Prt_HL_e        dec e
                ret m
                ld a,(hl)
                inc hl
                call Print
                jr Prt_HL_e

StoreScrn       ld de,0
                ld hl,#2050
                ld a,(EditorPages.Pg2)
                ld b,a
                ld ix,0
                ld c,Dss.WinCopy
                call CallDss
                ret             ;todo: разобраться для чего и реализовать под Спринтер

RestoreScrn     ld de,0         ;todo: разобраться для чего и реализовать под Спринтер
                ld hl,#2050
                ld a,(EditorPages.Pg2)
                ld b,a
                ld ix,0
                ld c,Dss.WinRest
                call CallDss
                ret

;█ █ █ Пункт меню ~File~ █ █ █ 

FILES   ld a,15:ld hl,#0101
        ld de,#0A0E:call OpenWindow
        call OutFS
        DB 22,2,3,"Save"
        DB 22,3,3,"Load"
        DB 22,4,3,"Catalogue"
        DB 22,5,3,"New"
        DB 22,6,3,"save Block"
        DB 22,7,3,"Merge"
        DB 22,8,3,"Erase file"
        DB 22,9,3,"Quit",0
        ld hl,FileMenu
        call Menu
        jp MAIN2

FileMenu DB 0,8
         DB 2,2,12,"s":DW SaveText
         DB 3,2,12,"l":DW LoadText
         DB 4,2,12,"c":DW Catalogue
         DB 5,2,12,"n":DW New
         DB 6,2,12,"b":DW SaveBlock
         DB 7,2,12,"m":DW Merge
         DB 8,2,12,"e":DW Erase
         DB 9,2,12,"q":DW Quit

Quit    
        ld sp,(SaveSP)
        ld a,(hMem)
        ld c,Dss.FreeMem
        rst #10
        ld bc,Dss.Exit
        rst #10

New     call BegText:call RemoveBlock
        ld (hl),13:inc hl
        ld a,(EOLN_Fl):or a:jr z,New1
         ld (hl),10:inc hl
New1    ld (SPACE),hl
        ld (hl),0:jp EDIT

 ;H -Y-KOOPДИHATA OKHA,
 ;DE-AДPEC БУФEPA ИMEHИ.

InpFlName
        push de
        ld de,#0520
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
hFile   db 0
FileName
        ds 128
FlNameBuff DS 128
MergeBuff  DS 13
EraseBuff  DS 13

Erase   ld h,9
        ld de,EraseBuff
        call InpFlName
        jp c,MAIN2
        ; call Check_Disk         ;todo: Реализовать под DSS
        ; jp nc,IBM_Erase
        ; ld c,#12
        ; call 15635
        jp MAIN2

SaveText ld h,3:ld de,FlNameBuff
         call InpFlName:jp c,MAIN2
         ld hl,(TEXT):ld de,(SPACE)
SvText1  ex de,hl:or a
         sbc hl,de:ex de,hl
        ;  call Check_Disk        ;todo: Реализовать под DSS
        ;  jp nc,IBM_Save
        ;  push hl:push de
        ;  ld c,#12:call 15635 ;Erase file
        ;  pop de:pop hl
        ;  ld c,#0B:call 15635 ;Save file
        ;  ld a,c:or b:jp nz,DiskFull
         jp EDIT

SaveBlock call BlockExist:jp nc,FILES
          ld h,7:ld de,MergeBuff
          call InpFlName:jp c,MAIN2
          ld hl,(BlockBeg)
          ld de,(BlockEnd)
          jr SvText1

Catalogue 
        ; call Check_Disk       ;todo: Реализовать под DSS
        ; jp nc,IBM_Cat
        ;   ld a,2:ld c,7:call 15635
Cat1       call Inkey
          jr z,Cat1
          jp MAIN2

Merge   ld h,8
        ld de,FlNameBuff
        call InpFlName
        jp c,MAIN2
        ld hl,(SPACE)
        call LoadTextFile
        jp c,Cat1
        jp EDIT


LoadText
        ld h,4
        ld de,FlNameBuff
        call InpFlName
        jp c,MAIN2
        call BegText
        ld hl,(TEXT)
        call LoadTextFile
        jp c,Cat1
        ld hl,FlNameBuff
        call CopyFileName
        jp EDIT

LoadTextFile
        push hl
        push de
        ex hl,de
        ld c,Dss.Open
        ld a,1
        rst #10        
        pop de
        pop hl
        jr nc,LoadTxt2
FileNFnd
        ld a,%00110000
        ld hl,#0709
        ld de,#031A:call OpenWindow
        call OutFS
        DB 22,8,13,"File not found ...",0
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
        ld a,%00110000
        ld hl,#0709
        ld de,#0319
        call OpenWindow
        call OutFS
        DB 22,8,13,"Reading error ...",0
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
        and a
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

Dr_Init   ld a,(Driver_Fl):or a
          jr z,Dr_Init1
           push hl:push de
           call #5B01
           pop de:pop hl
           ret
Dr_Init1  ld a,#98    ;Настройка адаптера 
          out (#7F),a ;           ______
          ld a,1      ;Установить STROBE 
          out (#5F),a
          ret

Dr_PrtA  push hl:push de:push bc
         ld hl,Driver_Fl:bit 0,(hl)
         jr z,Dr_Prt1:call #5B03
         pop bc:pop de:pop hl:ret nc
         jp PRT_Error
Dr_Prt1  out (#3F),a
          ;Выставить байт на шину 
Dr_Prt2  call #1F54    ;Проверка на BREAK
         jr nc,PRT_Error  ;Выход по BREAK 
         in a,(#5F)
          ;Считать состояние принтера 
         bit 5,a          ;Ошибка? 
         jr z,PRT_Error   ;Да - выход 
         bit 4,a          ;Готов? 
         jr nz,Dr_Prt2    ;Нет- повторить 
         xor a
         out (#5F),a   ;Выдать строб 
         inc a         ;A:=1
         nop           ;Пауза
         nop           ; ______
         out (#5F),a   ; STROBE:=1
         pop bc:pop de:pop hl:ret

PRT_Error ld sp,(SaveSP)
          ld a,%01010111:ld hl,#040C
          ld de,#051D:call OpenWindow
          call OutFS:DB 22,6,18
          DB "Printer Error ...",0
          jp Cat1

PRT_Text   ld hl,(TEXT):ld de,(SPACE)
           jr PRT_0

PRT_Block  call BlockExist:jp nc,PRINT
           ld hl,(BlockBeg)
           ld de,(BlockEnd)
PRT_0      call Dr_Init
PRT_1       ld bc,(PageSize)
PRT_2        call cp_hl_de:jp nc,MAIN2
             ld a,(hl):inc hl
             cp SPC:jr nz,PRT_4
              push bc:ld b,(hl)
              inc hl:res 7,b
PRT_3          ld a,32:call Dr_PrtA
              djnz PRT_3
              pop bc:jr PRT_2
PRT_4        cp 13:jr nz,PRT_5
              ld a,(hl):cp 10
              jr nz,$+3:inc hl
              ld a,13:call Dr_PrtA
              ld a,10:call Dr_PrtA
              dec bc:ld a,b:or c
              jr nz,PRT_2
              call PRT_Wait:jr PRT_1
PRT_5        call Dr_PrtA
            jr PRT_2

PRT_Wait   push hl:push de
           call StoreScrn
           ld a,%00010111:ld hl,#040C
           ld de,#0411:call OpenWindow
           call OutFS
    DB 22,5,17,"Insert new page than"
    DB 22,6,18, "press any key ...",0
           call Inkey:jr z,$-3
           call RestoreScrn
           pop de:pop hl:ret

Page_Size  ld a,%00010111:ld hl,#050C
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

SETUP   ld a,15:ld hl,#0115
        ld de,#050D:call OpenWindow
        call OutFS
        DB 22,2,23,"EOLN code"
        DB 22,3,23,"Compress"
        DB 22,4,23,"Driver",0
        ld hl,SetUpMenu
        call Menu:jp MAIN2

SetUpMenu DB 0,3
          DB 2,22,11,"e":DW EOLN_Code
          DB 3,22,11,"c":DW Compress
          DB 4,22,11,"d":DW Driver

Driver  ld a,%00010111:ld hl,#0518
        ld de,#040A:call OpenWindow
        call OutFS
        DB 22,6,26,"Inside"
        DB 22,7,26,"User's",0
        ld hl,Driver_Fl:call Menu
        jp MAIN2

Driver_Fl DB 0,2
          DB 6,25,8,"i":DW MAIN2
          DB 7,25,8,"u":DW MAIN2

EOLN_Code ld a,%00010111:ld hl,#0317
          ld de,#0409:call OpenWindow
          call OutFS
          DB 22,4,25,"CR"
          DB 22,5,25,"CR/LF",0
          ld hl,EOLN_Fl:call Menu
          jp MAIN2

EOLN_Fl   DB 1,2
          DB 4,24,7,0:DW MAIN2
          DB 5,24,7,0:DW MAIN2

Compress  ld a,%00010111:ld hl,#0418
          ld de,#0407:call OpenWindow
          call OutFS
          DB 22,5,26,"OFF"
          DB 22,6,26,"ON",0
          ld hl,Comprs_Fl:call Menu
          jp MAIN2

Comprs_Fl DB 0,2
          DB 5,25,5,0:DW MAIN2
          DB 6,25,5,0:DW MAIN2

;█ █ █ Пункт меню ~Info~ █ █ █

INFO    ld a,15:ld hl,#0113
        ld de,#0916:call OpenWindow
        call OutFS
        DB 22,2,21,"ZX/IBM Text Editor"
        DB 22,3,21,"ver.1.0 (Oct.1993)"
        DB 22,4,21,"Wr. by Hohlov Oleg"
        DB 22,5,21,"Tel.(0572)68-21-96"
        DB 22,7,21,"Text Length: ",0
        ld hl,(SPACE):ld de,(TEXT)
        or a:sbc hl,de:ld a,3:call DecHL
        call OutFS
        DB 22,8,21,"Free Space:  ",0
        ld hl,(RAMTOP):ld de,(SPACE)
        or a:sbc hl,de:call DecHL
        jp Cat1

;█ █ *** The END! *** (October 1993) █ █ 

