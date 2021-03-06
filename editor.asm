;__________________________________
;
; ZX / IBM Text EDITOR  ver.1.0
;          ____________
;         __          __
;        ___  EDITOR  ___
;__________________________________

;  OПEPAЦИИ C TEKCTOM 
;_________________________

Forward ld (ForwMEM),hl
        push bc
        ld bc,0
        ld a,13
        cpir
        inc de
        ld a,(hl)
        cp 10
        jr nz,$+3
        inc hl
        ld bc,(SPACE)
        push hl
        or a
        sbc hl,bc
        pop hl
        pop bc
        ret c
        dec de
        ld hl,(ForwMEM)
        ret
ForwMEM DW 0

Backward
        push de
        ld de,(TEXT)
        call cp_de_hl
        pop de:ret nc
        dec de
        push bc
        ld bc,0
        ld a,13
        dec hl
        cpdr
        cpdr
        inc hl
        inc hl
        ld a,(hl)
        cp 10
        jr nz,$+3
        inc hl
        pop bc
        scf
        ret

_NextLine
        ld hl,(LineAddr)
        ld de,(LineNum)
        call Forward
_NxtLn1 ld (LineAddr),hl
        ld (LineNum),de
        ret

_PrevLine
        ld hl,(LineAddr)
        ld de,(LineNum)
        call Backward
        jr _NxtLn1

;CF=1, ECЛИ БЛOK CУЩECTBУET 
BlockExist
        ld hl,(BlockBeg)
        ld de,(BlockEnd)
        call cp_hl_de
        ret

RemoveBlock
        ld hl,(TEXT)
        ld (BlockBeg),hl
        ld (BlockEnd),hl
        ret

SetCurXY
        push hl
        push de
        push bc
        ld b,1
        ld hl,(BegLine)
SetCur1 ld de,(LineAddr)
        call cp_hl_de
        jr nc,SetCur2
        call Forward
        inc b
        ld a,b
        cp 32
        jr c,SetCur1
        ld (BegLine),de
        ld b,1
SetCur2 ld a,(BegCol)
        ld c,a
        ld a,(CurCol)
        sub c
        ld c,a
        ld (CurX),bc
        pop bc
        pop de
        pop hl
        ret

CurChrAddr
        push af
        ld hl,LineBuff
        ld a,(CurCol)
        ld e,a
        ld d,0
        add hl,de
        ld e,l
        ld d,h
        pop af
        ret

SetLnNum push hl:push bc:ld hl,(TEXT)
         ld de,0:ld bc,(LineAddr)
SetNum1   push hl:or a:sbc hl,bc:pop hl
          jr nc,SetNum2:call Forward
         jr c,SetNum1:call BegText
          jr SetNum3
SetNum2  ld (LineNum),de
SetNum3  pop bc:pop hl:ret

BegText
        ld hl,(TEXT)
        ld (LineAddr),hl
        ld (BegLine),hl
        xor a
        ld h,a
        ld l,a
        ld (LineNum),hl
        ld (BegCol),a
        ld (CurCol),a
        ret

SetBegLine
        push de
        push hl
        ld hl,(LineAddr)
        ld a,b
        or a
        jr z,SetBegL2
SetBegL1
        call Backward
        djnz SetBegL1
SetBegL2
        ld (BegLine),hl
        pop hl
        pop de
        ret

SetLnAddr         ;BXOД:BC-HOMEP CTPOKИ 
        push hl:push de:push bc
        ld hl,(TEXT):ld de,0
SLnAdr1  ld a,c:cp e:jr nz,SLnAdr2
          ld a,b:cp d:jr z,SLnAdr3
SLnAdr2  call Forward:jr c,SLnAdr1
SLnAdr3 ld (LineNum),de:ld (LineAddr),hl
        ld b,15:call SetBegLine
        pop bc:pop de:pop hl:ret

LIST    ld bc,#1E01
        ld hl,(BegLine)
LIST1   call SetLnAttr
        ld (LineAttr),a
        ld a,#c0
        out (#89),a
        call Unpack
        ld a,c
        call PrintLine
        inc c
        djnz LIST1
        ret

;BЫXOД: A - ATPИБУT CTPOKИ HL,
;CF=1 -CTPOKA ПPИHAДЛEЖИT БЛOKУ 

SetLnAttr
        ld a,ScrnAttr
        ld de,(BlockEnd)
        call cp_hl_de
        ret nc
        ld de,(BlockBeg)
        call cp_hl_de
        ccf
        ret nc
        ld a,BlockAttr
        ret

;PACПAKOBKA CTPOKИ HL B БУФEP.
;BЫXOД: HL-CЛEД.CTPOKA.ECЛИ
;HL BЫШE TEKCTA,TO BOЗBPAT,HL
;HE MEHЯETCЯ,B БУФEPE ПPOБEЛЫ.
Unpack  push bc
        push hl
        ld hl,LineBuff
        ld bc,144
        ld de,LineBuff+1
        ld (hl),32
        ldir
        pop hl
        ld de,(SPACE)
        call cp_hl_de
        jr nc,Unpk3
        ld de,LineBuff
        ld b,128
Unpk0   ld a,(hl)
        inc hl
        cp SPC
        jr z,UnpSPC
        cp 9
        jr z,UnpTAB
        cp 13
        jr z,Unpk3
Unpk1   ld (de),a
        inc de
        djnz Unpk0
Unpk2   ld bc,0
        ld a,13
        cpir
Unpk3   ld a,(hl)
        cp 10
        jr nz,$+3
        inc hl
        pop bc
        call SetUnmodifiedLine
        ret
UnpTAB  ld a,e
        sub low LineBuff        ;Было "sub LineBuff"
        cpl          
        and 7
        inc a
        jr Unpk4
UnpSPC  ld a,(Comprs_Fl)
        or a
        ld a,SPC
        jr z,Unpk1
        ld a,(hl)
        inc hl
        res 7,a
Unpk4   inc de
        dec a
        jr z,Unpk0
        djnz Unpk4
        jr Unpk2

;ПEЧATЬ CTPOKИ ИЗ БУФEPA PACПAKOBЩИKA.
; BXOД: a-HOMEP CTPOKИ HA ЭKPAHE

PrintLine push hl
          push bc
          ld d,a
          ld e,0
          ld c,Bios.Lp_Set_Place
          call CallBios
          ld hl,LineBuff
          ld a,(BegCol)
          ld c,a
          ld b,0
          add hl,bc
          ld a,(LineAttr)
          ld e,a
          ld bc,#50 * 256 + Bios.Lp_Print_Ln
          call CallBios
          pop bc
          pop hl
          ret

PackBuff
        push hl
        push bc
        ld hl,LineBuff
        ld d,h
        ld e,l
        ld b,128
Pack1   ld c,0
Pack2   ld a,(hl)
        cp 32
        jr nz,Pack3
        inc c
        inc hl
        djnz Pack2
        jr Pack6
Pack3   cp 13
        jr z,Pack5
        ld a,c
        or a
        jr z,Pack5
        cp 1
        ld a,32
        jr z,Pack4
        ld a,(Comprs_Fl)
        or a
        ld a,32
        jr z,Pack7
        ld a,SPC
        ld (de),a
        inc de
        ld a,c
        set 7,a
Pack4   ld (de),a
        inc de
Pack5   ld a,(hl)
        ld (de),a
        inc de
        inc hl
        DJNZ Pack1
Pack6   ld a,13
        ld (de),a
        pop bc
        pop hl
        ld a,(EOLN_Fl)
        or a
        ret z
        inc de
        ld a,10
        ld (de),a
        ret
Pack7   ld (de),a
        inc de
        dec c
        jr nz,Pack7
        jr Pack5

_shift  push hl
        ld a,(hl)
        inc hl
        ld h,(hl)
        ld l,a
        call cp_hl_de
        jr c,_shift1
        add hl,bc
        ex de,hl
        ex (sp),hl
        ld (hl),e
        inc hl
        ld (hl),d
        dec hl
        ex (sp),hl
        ex de,hl
_shift1 pop hl
        ret

InsLine ld hl,LineBuff
        push hl
        inc de
        push de
        ld hl,(LineAddr)
        push hl
        ld a,13
        ld bc,0
        cpir
        ld a,(hl)
        cp 10
        jr nz,$+3
        inc hl
        push hl
        call InsText
        ret

PtrSL   DW 0
PtrSH   DW 0
PtrDL   DW 0
PtrDH   DW 0

InsText pop de
        ld a,4     ;Get arguments
        ld hl,PtrDH+1
InsTxt1 pop bc
        ld (hl),b
        dec hl
        ld (hl),c
        dec hl
        dec a
        jr nz,InsTxt1
        push de
        ld hl,(PtrSH) ;Calculate
        or a
        sbc hl,bc        ;offset
        ld bc,(PtrDL)
        add hl,bc
        ld bc,(PtrDH)
        or a
        sbc hl,bc
        ld b,h
        ld c,l         ;BC=OffSet
        ld hl,(SPACE)
        push hl
        jp m,InsTxt2          ;if(BC<0)
        add hl,bc
        jp c,Overflow
        jr InsTxt3
InsTxt2 add hl,bc
InsTxt3 ex de,hl
        ld hl,(RAMTOP)
        or a
        sbc hl,de
        jp c,Overflow
        ld (SPACE),de
        ld de,(PtrDH)
        ld hl,BlockBeg:call _shift
        ld hl,BlockEnd:call _shift
        ld hl,PtrSL:call _shift
        ld hl,PtrSH:call _shift
        ld h,d
        ld l,e
        add hl,bc
        ex de,hl
        pop bc
        call MoveMem
        ld hl,(PtrSL)
        ld de,(PtrDL)
        ld bc,(PtrSH)
        call MoveMem
        ld hl,(SPACE)
        ld (hl),0
        ret

Pack    push af
        ld a,(IsModifiedLine)
        and a
        jr z,.end
        push hl
        push de
        push bc
        call PackBuff
        call InsLine
        call SetUnmodifiedLine
        pop bc
        pop de
        pop hl
.end    pop af
        ret

;_____________________

; OШИБOЧHЫE CИTУAЦИИ 
;_____________________

Overflow ld hl,OvrflMssg
Ovrfl1   ld sp,(SaveSP):push hl
         ld hl,#0A07:ld de,#0512:ld a,#38
         call OpenWindow:call OutFS
         DB 22,13,14,"Press any key"
         DB 22,11,12,0
         pop hl:call OutHL:call Beep
Ovrfl2   call Waitkey:jr z,Ovrfl2
         jp MAIN1

DiskFull ld hl,DFullMssg:jr Ovrfl1

OvrflMssg DB "Not Enouth Memory",0

DFullMssg DB SPC,3,"Disk Full !",0
;_________________

;      ЭKPAH
;_________________

ClrScr
        ld      de,0
        ld      hl,#2052
        ld      b,ScrnAttr
        ld      c,Dss.Clear
        ld      a,#20
        call    CallDss
        ret

PrintMenu
        call OutFS
        DB 22,0,0,16,%00110000
        DB SPC,3,"File",SPC,3,"Edit"
        DB SPC,3,"Print",SPC,3,"SetUp"
        DB SPC,3,"Info",SPC,5 + 38,0

PrintKeyModes
        push hl
        call OutFS
        DB 22,31,0,16,%00111000,"  ",0
        ld hl,KeyModes
        ld a,(Graph_Fl)
        or a
        jr z,PrModes0
        call OutFS
        DB "Graphics ",0
        jr PrModes4
PrModes0
        bit 1,(hl)
        jr z,PrModes1
        call OutFS
        DB "Rus ",0
        jr PrModes2
PrModes1
        call OutFS:DB "Lat ",0
PrModes2
        bit 0,(hl)
        jr z,PrModes3
        call OutFS
        DB "Caps ",0
        jr PrModes4
PrModes3
        call OutFS
        DB "Lock ",0
PrModes4
        bit 2,(hl)
        jr z,PrModes5
        call OutFS
        DB "Insert",SPC,6,0
        jr PrModes6
PrModes5
        call OutFS
        DB SPC,12,0
PrModes6
        pop hl
        ret

PrintLineNum
         call OutFS
         DB 22,31,23,16,%00111000,"Line ",0
         ld hl,(LineNum):inc hl:ld c,34
PrintLN1 ld a,3:call DecHL
PrintLN2 ld a,(PrintXY):cp c:ret z
         ld a,32:call Print:jr PrintLN2

PrintCurCol
         call OutFS
         DB 22,31,34,16,%00111000,"Col ",0
         ld a,(CurCol):ld l,a:ld h,0
         inc hl:ld c,42:jr PrintLN1

PrintFilename
         call OutFS
         DB 22,31,80-19,16,%00111000,"Edit: "
.dosName DB "NAMELESS.TXT",0
         ret

PrintChrCode
         call OutFS
         DB 22,31,42,16,%00111000,0
         call CurChrAddr:ld l,(hl)
         ld h,0:ld c,49:jr PrintLN1

PrintModified
        ld a,(IsModified)
        and a
        ld a,32
        jr z,.next
        ld a,"*"
.next   ld (.flag),a
        call OutFS
        DB 22,31,80-21,16,%00111000
.flag   DB "  ", 0
        ret

PrintEdInfo
        call PrintMenu
        call PrintKeyModes
        call PrintLineNum
        call PrintCurCol
        ld c,80-21
        call PrintLN2
        call PrintModified
        call PrintFilename
        ld a,#c0
        out (#89),a
        ret

;____________________

;     PEДAKTOP
;____________________
MAIN0   ld c,Dss.CurDisk
        rst 10h
        add a,"A"
        ld (CurrentDir),a
        ld a,":"
        ld (CurrentDir+1),a
        ld hl,CurrentDir+2
        ld c,Dss.CurDir
        rst 10h
        ld hl,AppDir
        ld bc,256 + Dss.AppInfo
        rst 10h
MAIN1   call ReadSettings
        call ClrScr
MAIN2   call PrintEdInfo
        call LIST
MAIN3   ld hl,MainMenu
        call Menu
        jr EDIT

MainMenu DB 1,5
         DB 0,2,6,"f":DW FILES
         DB 0,9,6,"e":DW EDIT
         DB 0,16,7,"p":DW PRINT
         DB 0,24,7,"s":DW SETUP
         DB 0,32,6,"i":DW INFO

EDIT    ld iy,KeyModes
        call PrintEdInfo
        scf
EDIT1   jr nc,EDIT3
        call LIST
EDIT2   call PrintLineNum
        ld hl,(LineAddr)
        call SetLnAttr
        ld (LineAttr),a
        call Unpack
EDIT3   call SetCurXY
        ld a,(CurY)
        call PrintLine
        call PrintKeyModes
        call PrintCurCol
        call PrintChrCode
        call PrintModified
EDIT4   call ReadKey
        call Beep
        ld hl,EDIT1
        push hl
        ld a,d        
        cp #44
        jp z,MAINMENU
        ld a,e
        cp #1b          ;Esc
        jp z,COMMAND
        cp 8
        jp z,BACKSP
        cp 9
        jp z,TAB
        cp 13
        jp z,ENTER
        or a
        jr nz,.insert
        ld a,d
        cp #59
        jp z,PGUP
        cp #53
        jp z,PGDN
        cp #54
        jp z,LEFT
        cp #56
        jp z,RIGHT
        cp #58
        jp z,UP
        cp #52
        jp z,DOWN
        cp #4f
        jp z,DELETE
        cp #57
        jp z,HOME
        cp #51
        jp z,ENDLN
        call PrintKeyModes
        bit 5,b                 ;Ctrl
        jr z,.next
        ld a,d
        and #7f
        cp #15                  ;ctrl+y
        jp z,DeleteLine
        cp #2c                  ;ctrl+c
        jp z,EditCopyClipboard
        cp #2d                 ;ctrl+v
        jp z,EditInsertClipboard
        cp #2b                 ;ctrl+x
        jp z,EditCutClipboard
        CP #57                  ;Ctrl+PgUp
        jp z,JumpBegTxt
        CP #51		        ;Ctrl+PgDn
        jp z,JumpEndTxt
.next   pop de
        jp EDIT4
.insert pop hl
        ld b,a
        ld a,(Graph_Fl)
        or a
        ld a,b
        jr z,EDIT5
        call isalpha
        jr nc,EDIT5
        sub "A"
        ld de,Graph_Table
        call XLAT_b
EDIT5   push af
        call To_Line
        pop bc
        call AutoBracket
        call SetModifiedLine
        and a
        jp EDIT1

EditCopyClipboard
        call Pack
        call WriteClipboardFile
        scf
        ret

EditCutClipboard
        call Pack
        call WriteClipboardFile
        ret c
        call DelBlock
        ld a,1
        ld (IsModified),a
        scf
        ret
EditInsertClipboard
        call Pack
        call InsertClipboardFile
        ld a,1
        ld (IsModified),a
        scf
        ret

Graph_Fl DB 0

IsModified
        db 0
IsModifiedLine
        db 0

Graph_Table
        DB #C7, #D4, #BD, #B6, #B7, #BA, #C6, #D8, #CD, #B5, #B3, #DB, #BE, #CF, #DD, #DE, #D6, #C4, #D7, #D5, #B8, #F0, #D2, #D0, #D1, #D3 
        DB 0,0,0,0,0,0
        DB #C3, #C8, #D9, #B4, #BF, #B3, #CC, #CE, #CD, #B9, #BA, #B0, #BC, #CA, #B1, #B2, #DA, #C4, #C5, #C9, #BB, #FB, #C2, #C1, #CB, #C0 

AutoBracket
        ld a,1
AutoBrackets equ $-1
        and a
        ret z 
        ld a,b
        call Subst
        db 4
        db "(){}[]<>"
        cp b
        jr nz,$+5
        cp '"'
        ret nz 
        jp T_Line

MAINMENU
        pop de
        call Pack
        jp MAIN3

SetUnmodifiedFile
        xor a
        jr SetModifiedFile.set
SetModifiedFile
        ld a,1
.set    ld (IsModified),a
        ret

SetUnmodifiedLine
        xor a
        jr SetModifiedLine.set
SetModifiedLine
        call SetModifiedFile
        ld a,1        
.set    ld (IsModifiedLine),a
        ret
SetModifiedAndPack
        call SetModifiedLine
        jp Pack

SetBegCol
        sub 80
        jr nc,$+4
        ld a,#FF
        or %111
        inc a
        cp 48
        jr c,$+4
        ld a,48         ;128 - 80 (MAX_STR - SCR_LEN)
        ld (BegCol),a
        ret

LEFT    ld a,(CurCol)
        or a
        ret z ;CF=0
        dec a
        ld (CurCol),a
        ld b,a
        ld a,(BegCol)
        cp b
        ret z
        ccf
        ret nc
        sub 8
        jr nc,LEFT1
        xor a
LEFT1   ld (BegCol),a
LEFT2   call Pack
        scf
        ret

To_Line call T_Line
RIGHT   ld a,(CurCol)
RIGHT1  cp 127
        jr nc,RIGHT2
        inc a
RIGHT2  ld (CurCol),a
        ld b,a
        ld a,(BegCol)
        ld d,a
        add a,80
        ld e,a
        ld a,b
        call Interval
        ccf
        ret nc
        call SetBegCol
        jr LEFT2

HOME    ld a,(BegCol)
        ld b,a
        xor a
        ld (BegCol),a
        ld (CurCol),a
        cp b
        ret nc
        jr LEFT2

ENDLN   ld hl,LineBuff+127:ld b,127
ENDLN1   ld a,(hl):dec hl:cp 32
         ld a,b:jr nz,RIGHT1
        djnz ENDLN1
        ld a,(BegCol):add a,78:jr RIGHT1

PGDN    call Pack
        ld b,30
PGDN1   call _NextLine
        djnz PGDN1
        ld a,(CurY)
        dec a
        ld b,a
        call SetBegLine
        scf
        ret

PGUP    call Pack:ld b,30
PGUP1    call _PrevLine:djnz PGUP1
        ld b,30:ld hl,(BegLine)
PGUP2    call Backward:djnz PGUP2
        ld (BegLine),hl:scf:ret

DOWN    call Pack
        call _NextLine
        jr nc,DOWN2
        ld a,(CurY)
        cp 30
        jr c,DOWN2
        ld b,29
        call SetBegLine
        ld hl,#0100
        push hl
        ld hl,#1E50
        push hl
        call Scroll_Up
DOWN1   pop hl
        pop hl
DOWN2   pop hl
        jp EDIT2

UP      call Pack
        call _PrevLine
        jr nc,DOWN2
        ld a,(CurY)
        cp 1
        jr nz,DOWN2
        ld (BegLine),hl
        ld hl,#0100
        push hl
        ld hl,#1E50
        push hl
        call Scroll_Down
        jr DOWN1

LeadSpaces
        ld hl,(LineAddr)
        ld b,0
LeadSpc1
        ld a,(hl)
        cp 32
        jr nz,LeadSpc2
        inc hl
        inc b
        jr LeadSpc1
LeadSpc2
        ld a,(Comprs_Fl)
        or a
        ld a,b
        ret z
        ld a,(hl)
        cp SPC
        ld a,b
        ret nz
        inc hl
        ld b,(hl)
        res 7,b
        ld a,b
        ret

ENTER   call SetModifiedFile
        call IsOver
        jr z,ENTER1
        call CurChrAddr
        inc de
        ld a,(EOLN_Fl)
        or a
        jr z,$+3
        inc de
        ld bc,LineBuff+127
        call MoveMem
        ld (hl),13
        ld a,(EOLN_Fl)
        or a
        jr z,ENTER1
        inc hl
        ld (hl),10
ENTER1  call SetModifiedAndPack
        call LeadSpaces
        push af
ENTER2  call _NextLine
        jr c,ENTER4
        call Unpack
        call PackBuff
        inc de
        ld a,13
        ld (de),a
        ld a,(EOLN_Fl)
        or a
        jr z,ENTER3
        inc de
        ld a,10
        ld (de),a
ENTER3  call InsLine
        jr ENTER2
ENTER4  call LeadSpaces
        ld hl,(LineAddr)
        ld a,(hl)
        cp 13
        jr nz,ENTER5
        ld b,255
ENTER5  pop af
        cp b
        jr c,ENTER6
        ld a,b
ENTER6  ld (CurCol),a
        call SetBegCol
        ld a,(CurY)
        cp 30
        ret c
        ld hl,(BegLine)
        call Forward
        ld (BegLine),hl
        scf
        ret

DELETE  call SetModifiedLine
        call CurChrAddr
        ld bc,LineBuff+129
        ld a,(KeyModes)
        bit 2,a
        jr z,DEL2
DEL1    ld a,(hl)
        inc hl
        cp 32
        jr z,DEL3
        ld h,d
        ld l,e
DEL2    inc hl
        call MoveMem
        or a
        ret
DEL3    push hl
        or a
        sbc hl,bc
        pop hl
        jr c,DEL1
        ld hl,(LineAddr)
        call Forward
        ret nc
        call Pack
        ld hl,(LineAddr)
        call Forward
        ld de,(BlockBeg)
        call cp_hl_de
        jr nz,DEL5
DEL4    ld a,(de)
        inc de
        cp 13
        jr nz,DEL4
        ld a,(de)
        cp 10
        jr nz,$+3
        inc de
        ld (BlockBeg),de
DEL5    ld de,(BlockEnd)
        call cp_hl_de
        jr nz,DEL7
DEL6    ld a,(de)
        inc de
        cp 13
        jr nz,DEL6
        ld a,(de)
        cp 10
        jr nz,$+3
        inc de
        ld (BlockEnd),de
DEL7    dec hl
        ld a,(hl)
        cp 10
        jr nz,DEL8
        dec hl
        ld (hl),32
        inc hl
DEL8    ld (hl),32
        ld hl,(LineAddr)
        call Unpack
        call Pack
        scf
        ret

BACKSP  ld a,(CurCol):or a:ret z ;CF=0
        call CurChrAddr:ld a,(KeyModes)
        bit 2,a:jr nz,BACKSP1
          dec hl:ld (hl),32:jp BACKSP2
BACKSP1 dec de:ld bc,LineBuff+129
        call MoveMem
BACKSP2 jp LEFT

TAB     call Pack:ld hl,(LineAddr)
        call Backward:call Unpack
        call CurChrAddr:ld a,(CurCol)
        ld b,a:ld d,(hl)
TAB1     ld a,(hl):inc hl:inc b:cp 32
        jr nz,TAB1
        bit 7,b:jr nz,TAB4:ld c,b
TAB2     ld a,(hl):inc hl:inc b
         bit 7,b:jr nz,TAB3:cp 32
        jr z,TAB2:jr TAB5
TAB3    ld a,d:cp 32:jr z,TAB4
        ld b,c:jr TAB5
TAB4     ld a,(CurCol):and %11111000
         add a,9:ld b,a:jp p,TAB5
          ld b,128
TAB5    ld hl,(LineAddr):call Unpack
        dec b:ld a,b:jp RIGHT2

DecCodeBuff  DS 4

MakeNumber ; HL -AДPEC ЧИCЛA 
           ; BЫXOД: DE -ЧИCЛO 
        ld de,0
MkNum1   ld a,(hl):call isdigit:ret nc
         ex de,hl:add hl,hl:ld b,h:ld c,l
         add hl,hl:add hl,hl:add hl,bc
         ld b,0:sub "0":ld c,a:add hl,bc
         ex de,hl:inc hl
        jr MkNum1

COMMAND call OutFS
        DB 22,31,2,16,%00111000
        DB " Command:",SPC,12,0
        ld hl,#1F0B
        ld (PrintXY),hl
        ld (CurX),hl
        call ReadKey
        call Beep
        ld a,e
        call isdigit
        jr nc,CMND1
        ld de,DecCodeBuff
        ld (de),a
        inc de
        ld a,13
        ld (de),a
        dec de
        ld c,3
        call Input
        ex de,hl
        ld a,(hl)
        call isdigit
        ld a,32
        jr nc,CMND1
        call MakeNumber
        pop bc
        call PrintKeyModes
        ld a,e
        jp EDIT5
CMND1   call Pack
        ld de,CMND5
        push de
        call toupper
        ld b,11
        call Case
        DB "B":DW MarkBeg
        DB "E":DW MarkEnd
        DB "C":DW CopyBlock
        DB "D":DW DelBlock
        DB "M":DW MoveBlock
        DB "Q":DW ResetBlock
        DB "L":DW DeleteLine
        DB "J":DW JumpLine
        DB "S":DW Search
        DB "R":DW Replace
        DB "G":DW Graphics
        cp 33
        jr nc,CMND_Help
        pop de
CMND5   call PrintEdInfo
        scf
        ret

CMND_Help
        call OutFS
        DB "Help",0
        ld hl,#071A
        ld de,#101B
        ld a,15
        call OpenWindow
        call OutFS
        DB 22,8,#1C,"B - Mark block begin"
        DB 22,9,#1C,"E - Mark block end"
        DB 22,10,#1C,"C - Copy block"
        DB 22,11,#1C,"D - Delete block"
        DB 22,12,#1C,"M - Move block"
        DB 22,13,#1C,"Q - Reset block"
        DB 22,15,#1C,"S - Search"
        DB 22,16,#1C,"R - Replace"
        DB 22,17,#1C,"L - Delete line"
        DB 22,18,#1C,"G - Graphics On/Off"
        DB 22,19,#1C,"J - Jump to line ..."
        DB 22,21,#1C,"0..9 - Put decimal code",0
C_Help1 call Waitkey
        jr z,C_Help1
        ret

Graphics
        ld a,(KeyModes)
        bit 1,a
        ret nz
        ld hl,Graph_Fl
        ld a,(hl)
        cpl
        ld (hl),a
        ld hl,KeyModes
        res 1,(hl)
        ret

MarkBeg ld hl,(LineAddr)
        ld (BlockBeg),hl
        ret

MarkEnd ld hl,(LineAddr)
        ld a,13
        ld bc,0
        cpir
        ld a,(hl)
        cp 10
        jr nz,$+3
        inc hl
        ld (BlockEnd),hl
        ret

CopyBlock
        ld hl,(LineAddr)
        call SetLnAttr
        ret c
        call BlockExist
        ret nc
        push de
        push hl
        push de
        ld hl,(LineAddr)
        push hl
        push hl
        call InsText
        pop de
        ld hl,(LineAddr)
        call cp_hl_de
        jr nz,CopyBl1
        ld (BlockEnd),hl
CopyBl1 or a
        ret

MoveBlock
        call CopyBlock
        ret c

DelBlock
        call BlockExist
        ret nc
        push hl
        push hl
        push hl
        ld (LineAddr),hl
        ld hl,(SPACE)
        call cp_de_hl
        jr c,DelBl1
        dec de
        ld a,(de)
        cp 10
        jr nz,$+3
        dec de
DelBl1  push de
        call InsText
        ld b,7
        call SetBegLine
        call SetLnNum
ResetBlock
        ld hl,(BlockBeg)
        ld (BlockEnd),hl
        ret

DeleteLine
        call SetModifiedFile
        ld hl,(LineAddr)
        push hl
        push hl
        push hl
        ld bc,0
        ld a,13
        cpir
        ld a,(hl)
        cp 10
        jr nz,$+3
        inc hl
        ld de,(SPACE)
        call cp_hl_de
        jr c,DelLine1
        dec hl
        ld a,(hl)
        cp 10
        jr nz,$+3
        dec hl
DelLine1
        push hl
        call InsText
        scf
        ret

JumpEndTxt ld bc,65535
JumpTxt1
        call SetLnAddr
        scf
        ret

JumpBegTxt
        ld bc,0
        jr JumpTxt1

JumpLine
        call OutFS
        DB 22,31,2,"Line Number:",0
        ld de,JumpLnBuff
        ld a,13
        ld (de),a
        ld c,5
        call Input
        ex de,hl
        call MakeNumber
        ld b,d
        ld c,e
        ld a,b
        or c
        jr z,JumpTxt1
        dec bc
        jr JumpTxt1

JumpLnBuff DS 6
SearchBuff DS 16
RplcBuff   DS 16

Pattern ld a,%00001111
        ld hl,#0306
        ld de,#0516
        call OpenWindow
        call OutFS
        DB 22,4,13,"Search:",22,5,9,0
        ld de,SearchBuff
        ld c,15
        call Input
        cp 13
        jr nz,Pttrn1
        ld a,(de)
        cp 32
        ret nc
Pttrn1  pop de
        ret

;ПOИCK B БУФEPE C ПOЗИЦИИ C.
;BЫXOД: CF=0 -HAЙДEHO,C -ПOЗИЦИЯ,
; B -ДЛИHA ПOДCTPOKИ.

SubString
        ld hl,LineBuff
        xor a
        ld b,a
        ld (LineBuff+128),a
        add hl,bc
        ld a,129
        sub c
        ld c,a
        ld b,0
SubStr1 ld de,SearchBuff
        ld a,(de)
        cpir
        scf
        ret po
        push hl
        ld b,1
SubStr2 inc de
        ld a,(de)
        cp 13
        jr z,SubStr3
        inc b
        cp (hl)
        inc hl
        jr z,SubStr2
        pop hl
        ld b,0
        jr SubStr1
SubStr3 pop hl
        ld de,LineBuff+1
        sbc hl,de
        ld c,l
        ret

;BЫXOД: BC, CF=0 -HAИДEHO

FindStr ld hl,(LineAddr)
        ld de,(LineNum)
Find1   push hl
        push de
        call Unpack
        ld a,(CurCol)
        ld c,a
        call SubString
        pop de
        pop hl
        jr nc,FindOK
        call Forward
        ld a,0
        ld (CurCol),a
        ld (BegCol),a
        jr c,Find1
        scf
        ret
FindOK  ld (LineAddr),hl
        ld (LineNum),de
        ld a,c
        add a,b
        cp 128
        jr c,$+4
        ld a,127
        ld (CurCol),a
        call SetBegCol
        push bc
        ld b,7
        call SetBegLine
        pop bc
        or a
        ret

Search  call OutFS
        DB "Search",0
        call Pattern
        call FindStr
        ret nc
SrchNFnd
        call OutFS
        DB 22,6,12,"Not Found",0
        jp C_Help1

Replace call OutFS
        DB "Replace",0
        call Pattern
        xor a
        ld (Replace.all),a
        call OutFS
        DB 22,4,11,"Replace with:"
        DB 22,5,9,SPC,15,22,5,9,0
        ld c,15
        ld de,RplcBuff
        call Input
        cp 13
        ret nz
        call FindStr
        jr c,SrchNFnd
.loop   push bc
        call PrintLineNum
        call PrintCurCol
        call PrintModified
        pop bc
        ld a,0
.all    equ $-1        
        and a
        jr nz,.replace
        push bc
        call LIST
        ld hl,(LineAddr)
        call Unpack
        call SetCurXY
        call OutFS
        DB 22,31,0,16,%01110000
        DB SPC,26,"Replace ?",SPC,3
        DB "(Yes/All/No/Quit)",SPC,25,0
        pop bc
.loop1  push bc
        call ReadKey
        push af
        call Beep
        pop af
        pop bc
        cp "q"
        ret z
        cp "y"
        jr z,.replace
        cp "a"
        jr z,.setAll
        cp "n"
        jr nz,.loop1
.loop2  call FindStr
        jr nc,.loop:ret
.setAll ld a,1
        ld (Replace.all),a
        push bc
        call LIST
        ld hl,(LineAddr)
        call Unpack
        pop bc
.replace
        call SetModifiedLine
        ld hl,LineBuff
        ld e,c
        ld d,0
        add hl,de
        push hl
        ld e,b
        add hl,de
        pop de
        push de
        ld bc,RplcBuff
.loop3  ld a,(bc)
        cp 32
        inc bc
        inc de
        jr nc,.loop3
        dec de
        ld a,e
        sub low LineBuff ;Было "sub LineBuff", возможно сейчас не правильно написал
        cp 128
        jr c,$+4
        ld a,127
        ld (CurCol),a
        call SetBegCol
        ld bc,LineBuff+128
        ld a,32
        ld (bc),a
        call MoveMem
        ld hl,RplcBuff
        pop de
.loop4  ld a,(hl)
        cp 13
        jr z,.next
        ld (de),a
        inc de
        inc hl
        jr .loop4
.next   call Pack
        jr .loop2

EditorPages
.Pg0		db	#00
.Pg1		db	#00
.Pg2		db	#00
hMem            db      #00
