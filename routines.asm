;__________________________________
;
;  ZX / IBM Text EDITOR  ver.1.0
;         ____________
;        __          __
;       ___ ROUTINES ___
;__________________________________

START   ld (SaveSP),sp
        push ix
        ld a,#c0                ;Set the Y-Port value upper than 192
        out (#89),a
        LD BC,3 * 256 + Dss.GetMem
        RST #10			; need 3 memory pages
        JR NC,.next
        pop hl
        LD HL,MsgNoMemory
.exit	LD C,Dss.PChars
	RST #10
        ld hl,Enter
        LD C,Dss.PChars
	RST #10
        LD A,(hMem)
        and a
        jr z,.skip
        LD C,Dss.FreeMem
        rst #10
.skip   LD BC,#0100 + Dss.Exit
	RST #10
.next   LD (hMem),A		; memory handle
	LD HL,EditorPages
	LD C,Bios.Emm_Fn5
	RST #08
	LD A,(EditorPages.Pg0)		; set pages from 0x8000
	OUT (EmmWin.P2), A
        LD A,(EditorPages.Pg1)		; set pages from 0xC000
	OUT (EmmWin.P3), A
        call WinSave
        pop hl
        inc hl
        ld de,FlNameBuff                ;парсинг ком. строки
        push de
        ld c,Dss.GSwitch                ;получить первый параметр ком.строки
        rst #10
        pop de
        ld a,(de)
        and a
        jp z,.empty
        ex de,hl
        push hl
        ld bc,Dss.EX_Path               ;разобрать строку - проверить получить имя файла
        rst #10
        pop de
        jr c,.nofile
        and 3
        cp 3
        jr nz,.nofile
        ld hl,(TEXT)
        ld a,1
        call LoadTextFile
        jr c,.exit
        ld hl,FlNameBuff
        call CopyFileName
        jp MAIN0
.nofile ld hl,MsgCantOpen
        jr .exit
.empty  ld hl,(TEXT)
        ld (hl),13
        inc hl
        ld (hl),0
        jp MAIN0
MsgNoMemory
        db "Not enough memory to load program.", 0x00
MsgCantOpen
        db "Error: Can't open file", 0x00
Enter   db 0x0D, 0x0A, 0x00
        org ($/256+1)*256

TEXT    DW StartText                ;Начало текста
SPACE   DW EndText-1                ;конец текста
RAMTOP  DW #FFFF                ;верхняя граница текста
; FAT     EQU #E800
; DIR     EQU FAT+2560
SaveSP  DW 0

BegLine   DW END+1
BegCol    DB 0
LineAddr  DW END+1
LineNum   DW 0
LineAttr  DB 7
CurCol    DB 0
BlockBeg  DW END+1
BlockEnd  DW END+1

ScrnAttr   EQU %00000111
BlockAttr  EQU %01010111
SPC        EQU 6
DIVIDER      EQU 1

KeyModes  DB %00000100
CurX      DB 0
CurY      DB 0
PrintXY   DW 0
PrintAttr DB 7
WaitConst EQU 7500

CopyFileName
        ld de,FileName
        push de
        ld bc,128
        ldir
        pop hl
        ld de,PrintFilename.dosName
        push de
        ld bc,3*256 + Dss.EX_Path
        call CallDss
        pop hl
        ld bc,#0d20
        xor a        
.loop   cp (hl)
        jr z,.end
        inc hl
        djnz .loop
        ld (hl),a
        ret
.end    ld (hl),c
        inc hl
        djnz .end
        ld (hl),a
        ret

WinSave	LD C,Dss.Cursor
	call CallDss
	LD (SavePosition),DE
	LD IX,SCR_BUF
.save	LD DE,#0000
	LD HL,#2050
	LD C,Dss.WinCopy
        ld a,(EditorPages.Pg2)
        ld b,a
	DI
	call CallDss
	EI
	RET

WinBack	LD IX,SCR_BUF
.back	LD DE,#0000
	LD HL,#2050
        LD C,Dss.WinRest
        ld a,(EditorPages.Pg2)
        ld b,a
	DI
	call CallDss
	EI
	LD DE,0
SavePosition equ $-2
	LD C,Dss.Locate
	call CallDss
	RET

StoreScrn
        ld ix,SCR_BUF1
        jr WinSave.save

RestoreScrn
        ld ix,SCR_BUF1
        jr WinBack.back
;________________________

; BCПOMOГAT.ПOДПPOГPAMMЫ
;________________________
;  Выбор подпрограммы для выполнения в зависимости от нажатой кнопки
;  BXOД:  A,B,List
;  HAPУШAET: B

Case    ex (sp),hl
Case1    cp (hl):inc hl:jr z,Case3
         inc hl:inc hl:djnz Case1
Case2   ex (sp),hl:ret
Case3   ld b,(hl):inc hl:ld h,(hl)
        ld l,b:jr Case2

;ПPOBEPKA: D <= A < E
Interval
        cp d
        ccf
        ret nc
        cp e
        ret

; Подстановка a по списку за call.
; первый байт списка содержит число
; следующих далее пар образец-заменитель.
; Вход:  a.  Выход: a
;  BXOД:  A,List
;  BЫXOД: A
Subst   ex (sp),hl
        push bc
        ld b,(hl)
        inc hl
        ld c,a
Subst1  cp (hl)
        inc hl
        jr nz,Subst2
        ld c,(hl)
Subst2  inc hl
        djnz Subst1
        ld a,c
        pop bc
        ex (sp),hl
        ret

T_Line  call CurChrAddr
        call IsOver
        jr z,.toLin1
        inc de 
        ld bc,LineBuff+127
        call MoveMem
.toLin1 ld (hl),a
        ret


IsOver  bit 2,(iy+0)
        ret

;BXOД:DE-AДPEC TAБЛИЦЫ;
;     A-ПPEOБPAЗУEMЫИ БAИT.
;PEЗУЛЬTAT:ДЛЯ XLAT_b - B A
;          КДЛA XLAT_w - B HL.

XLAT_b  push hl
        ld l,a
        ld h,0
        add hl,de
        ld a,(hl)
        pop hl
        ret

XLAT_w  ld l,a
        ld h,0
        add hl,hl
        add hl,de
        ld a,(hl)
        inc hl
        ld h,(hl)
        ld l,a
        ret

;ПOДПPOГPAMMЫ KЛACCA ЛИTEPЫ 

isupper cp "A"
        ccf
        ret nc
        cp "Z"+1
        ret

islower cp "a"
        ccf
        ret nc
        cp "z"+1
        ret

isalpha call isupper
        ret c
        call islower
        ret

isdigit cp "0"
        ccf
        ret nc
        cp "9"+1
        ret

toupper call islower
        ret nc
        sub #20
        ret

tolower call isupper
        ret nc
        add a,#20
        ret

;APИФMETИЧECKИE ПPOЦEДУPЫ 
cp_hl_de push hl:or a:sbc hl,de
         pop hl:ret

cp_de_hl ex de,hl:push hl
         or a:sbc hl,de
         pop hl:ex de,hl
         ret

;KOПИPOBAHИE ПAMЯTИ 
MoveMem push af
        ld a,c               ;BC=BC-HL
        sub l
        ld c,a
        ld a,b
        sbc a,h
        ld b,a
        or c
        jr z,.exit           ;BC=0.
        call cp_hl_de
        jr z,.exit           ;HL=DE.
        push hl
        jr c,.skip
        ldir
        jr .end
.skip   add hl,bc
        dec hl
        ex de,hl
        add hl,bc
        dec hl
        ex de,hl
        lddr
.end    pop hl
.exit   pop af
        ret

; ШAПKA ФУHKЦИИ,ПOЛУЧAЮЩEИ
; ПAPAMETPЫ ЧEPEЗ CTEK.
; HAPУШAET DE.

FuncHead pop de:push hl:push bc
         push af:push ix:ld ix,10
         add ix,sp:push de:ret

; BЫXOД ИЗ ФУHKЦИИ БEЗ УДAЛEHИЯ
; APГУMEHTOB ИЗ CTEKA И БEЗ
; ABTOMATИЧECKИX ДAHHЫX.

FuncTail pop ix:pop af:pop bc
         pop hl:ret

;______________________
;   ДPAИBEP KУPCOPA 
;______________________

PutCursor
        di
        push hl
        push bc
        push de
        ld de,(CurX)
        push de
        ld c,Dss.RdChar
        call CallDss
        pop de        
        push bc
        ld c,Bios.Lp_Set_Place
        call CallBios
        pop bc
        ld a,b
        and 7
        rlca:rlca:rlca:rlca
        ld c,a
        ld a,b
        and %01110000
        rrca:rrca:rrca:rrca
        or c
        ld c,a
        ld a,b
        and %10001000
        or c
        ld e,a
        ld bc,256 + Bios.Lp_Print_Atr
        call CallBios
        ld a,(CurFlag)
        cpl
        ld (CurFlag),a
        pop de
        pop bc
        pop hl
        ei
        ret

;ПП перед вызовом Биос меняет страницу в банке 2 и перемещает туда стек (временно),
;чтоб не было неожиданной порчи экранных данных
CallBios
        ld (.aValue),a
        in a,(EmmWin.P2)
        ld (.page),a
        in a,(EmmWin.P1)
        out (EmmWin.P2),a
        ld (.spSave),sp
        ld sp,SafeStack
        ld a,0
.aValue equ $-1
        rst #08
        ld sp,0
.spSave equ $-2
        push af
        ld a,0
.page   equ $-1
        out (EmmWin.P2),a
        pop af
        ret

;ПП перед вызовом Dss меняет страницу в банке 2 и перемещает туда стек (временно),
;чтоб не было неожиданной порчи экранных данных
CallDss
        ld (.aValue),a
        in a,(EmmWin.P2)
        ld (.page),a
        in a,(EmmWin.P1)
        out (EmmWin.P2),a
        ld (.spSave),sp
        ld sp,SafeStack
        ld a,0
.aValue equ $-1
        rst #10
        ld sp,0
.spSave equ $-2
        push af
        ld a,0
.page   equ $-1
        out (EmmWin.P2),a
        pop af
        ret

ClearCursor
        ld a,0
CurFlag equ $-1
        or a
        ret z
        call PutCursor
        ret

;_______________________

;  ДPAИBEP KЛABИATУPЫ 
;_______________________

;BEPHУTЬ KOД CИMBOЛA C УЧETOM Rus/Lat И
;Caps/Lock ИЛИ ZF=1,ЕСЛИ KЛABИШA НЕ НАЖАТА

Inkey   push hl
        ld c,Dss.ScanKey
        call CallDss
        pop hl
        ret

Waitkey push hl
        ld c,Dss.WaitKey
        call CallDss
        pop hl
        ret

;BEPHУTЬ KOД CИMBOЛA C OЖИДAHИEM НАЖАТИЯ
; И BЫBOДOM KУPCOPA
; A - код символа
; D - позиционный код
; Е - ASCII код
; C - режим клавиатуры:
ReadKey push hl
RdKey0  call PutCursor
        ld hl,WaitConst
RdKey1  
        call Inkey
        jr nz,RdKey2
        dec hl
        ld a,h
        or l
        jr nz,RdKey1
        jr RdKey0
RdKey2  ld h,a
        push bc
        ld a,c
        and 1
        bit 7,c
        jr z,.next
        set 1,a
.next   bit 1,c
        jr z,.next1
        set 2,a
.next1  ld c,a
        ld a,(KeyModes)
        and %11111000
        or c
        ld (KeyModes),a
        bit 1,a
        call nz,.graphOff
        call ClearCursor
        pop bc
        ld a,h
        pop hl
        ret
.graphOff
        xor a
        ld (Graph_Fl),a
        ret

;______________________

;   ДPAИBEP ПEЧATИ 
;______________________

;print A at (H,L) with Attr
PrintA  push    hl
        push    de
        push    bc
        ex      hl,de   ;d - y, e - x
        ld      c,Dss.WrChar
        ex      af,af'
        ld      a,(PrintAttr)
        ld      b,a
        ex      af,af'
        call    CallDss
        pop     bc
        pop     de
        pop     hl
        ret

;ПEЧATЬ CИMBOЛA B TEKУЩЕЙ ПOЗИЦИИ
;И EE MOДИФИKAЦИЯ.
Print   push af
        push hl
        ld hl,(PrintXY)
        call PrintA
        inc l
        ld a,l
        cp 80
        jr c,Print1
        ld l,0
        inc h
        ld a,h
        cp 32
        jr c,Print1
        ld h,0
Print1  ld (PrintXY),hl
        pop hl
        pop af
        ret

;BЫBOД ПOCЛEДOBATEЛЬHOCTИ CИMBOЛOB,
;AДPECУEMOИ HL И ЗABEPШAEMOИ '\0'.
;OБPAБATЫBAЮTCЯ KOДЫ:
; #10=16 - Set Attributes ;
; #16=22 - Set PrintXY ;
; SPC - Spaces Compressor.
OutHL   push af
OutHL0  ld a,(hl)
        inc hl
        or a
        jr z,OutHL1
        cp DIVIDER
        jr z,HorizontalRuler
        cp 16
        jr z,OutHL2
        cp 22
        jr z,OutHL3
        cp SPC
        jr z,OutHL4
        call Print
        jr OutHL0
OutHL1  pop af
        ret
OutHL2  ld a,(hl)
        ld (PrintAttr),a
OutHL5  inc hl
        jr OutHL0
OutHL3  ld a,(hl)
        ld (PrintXY+1),a
        inc hl
        ld a,(hl)
        ld (PrintXY),a
        jr OutHL5
OutHL4  push bc
        ld b,(hl)
OutHL6  ld a,32
        call Print
        djnz OutHL6
        pop bc
        jr OutHL5
HorizontalRuler
        push bc
        ld a,195
        call Print
        ld b,(hl)
        dec b
        dec b
        ld a,196
.loop   call Print
        djnz .loop
        ld a,180
        call Print
        pop bc
        jr OutHL5

OutFS   ex (sp),hl:call OutHL
        ex (sp),hl:ret

;____ ПEЧATЬ ЧИCEЛ ____

;HL-ЧИCЛO,A-ФOPMAT:
; A=0 -BEДУЩИE HУЛИ ПEЧATAЮTCЯ;
; A=1 - ЗAMEHЯЮTCЯ ПPOБEЛAMИ;
; A=3 - HE ПEЧATAЮTCЯ.

DecHL   push hl:push de:push bc:push af
        ld bc,1:push bc:ld c,10:push bc
        ld c,100:push bc
        ld bc,1000:push bc
        ld bc,10000:push bc
        ld c,a:res 7,c:ld b,5
DecHL1   xor a:pop de
DecHL2    sbc hl,de:inc a
         jr nc,DecHL2
         add hl,de:add a,"0"-1
         bit 0,c:jr z,DecHL4
           cp "0":jr z,DecHL3
             set 7,c:jr DecHL4
DecHL3     bit 7,c:jr nz,DecHL4
           ld a,b:dec a:ld a,"0"
           jr z,DecHL4
             bit 1,c:jr nz,DecHL5
               ld a,32
DecHL4   call Print
DecHL5  djnz DecHL1
        pop af:pop bc:pop de:pop hl
        ret

;_______________

;    З B У K 
;_______________

Beep    ret                             ;todo: реализовать бипер

;________________________

;  BBOД CTPOKИ B БУФEP 
;________________________

;DE-AДPEC БУФEPA, C-MAKC.ДЛИHA
;HAPУШAET A-TAM KOД KЛABИШИ,ПO
;KOTOPOИ OCУЩECTBЛEH BЫXOД
;(Enter,Up,Down)

Input   push hl
        push bc
        push de
Input1  ld hl,(PrintXY)
        pop de
        push de
        ld b,c
Input2  ld a,(de)
        cp 32
        jr c,Input3
        inc de
        call PrintA
        inc l
        djnz Input2
        ld a,13
        ld (de),a
Input3  ld (CurX),hl
        ld a,32
        call PrintA
Input4  push de
        push bc
        call ReadKey
        pop bc
        ld a,e
        cp 32
        jr c,Input6
        pop de
        ld (de),a
        ld a,b
        or a
        jr z,Input5
        call Beep
        inc de
Input5  ld a,13
        ld (de),a
        jr Input1

Input6  cp #1b
        jr z,.exit
        cp 8
        jr z,.backsp
        cp 13
        jr z,.exit
        ld a,d
        cp #4f
        jr z,.delete
        cp #54          ;Cursor Left
        jr z,.backsp
        pop de
        jr Input4
.exit   call Beep
        pop de
        pop de
        pop bc
        pop hl
        ret
.backsp pop de
        ld a,b
        cp c     ;Delete
        jr z,Input4
        call Beep
        dec de
        ld a,13
        ld (de),a
        jr Input1
.delete call Beep       ; "<-" BackSpace
        pop de
        pop de
        push de
        ld a,13
        ld (de),a
        ld hl,(PrintXY)
        ld b,c
.loop   ld a,32
        call PrintA
        inc l
        djnz .loop
        jp Input1

;______________________

;   OKOHHЫИ ДPAИBEP 
;______________________

;CMEЩEHИЯ ДЛЯ ДOCTУПA K ПAPAMETPAM
;OKHA B CTEKE

Xw      equ 2
Yw      equ 3
dX      equ 0
dY      equ 1
MemLo   equ 4
MemHi   equ 5

Clear_Wind
;OЧИCTKA OБЛACTИ OKHA.
; TPИ ПAPAMETPA: KOOPДИHATЫ И PAЗMEPЫ -
; B CTEKE, A -БAИT ATPИБУTOB.
        call FuncHead
        ld b,a
        ld e,(ix+Xw)
        ld d,(ix+Yw)
        ld l,(ix+dX)
        ld h,(ix+dY)
        ld a,#20
        ld c,Dss.Clear
        call CallDss
        jp FuncTail

;CKPOЛЛИHГ OKHA BBEPX; HИЖHЯЯ CTPOKA
;HE OЧИЩAETCЯ.
; ДBA ПAPAMETPA: KOOPДИHATЫ И PAЗMEPЫ.

Scroll_Up
        call FuncHead        
        ld e,(ix+dY)
        ld d,(ix+Yw)
        ld bc,1 * 256 + Bios.Lp_Scroll_Up
        di
        call CallBios
        ei
        jp FuncTail

; ScrM   dw 0

;CKPOЛЛИHГ OKHA BHИЗ; BEPXHЯЯ CTPOKA
;HE OЧИЩAETCЯ.
; ДBA ПAPAMETPA- KOOPДИHATЫ И PAЗMEPЫ

Scroll_Down
        call FuncHead        
        ld e,(ix+dY)
        ld d,(ix+Yw)
        ld bc,2 * 256 + Bios.Lp_Scroll_Up
        di
        call CallBios
        ei
        jp FuncTail

Frame_Wind
;OБBECTИ OKHO PAMKOИ.
; ДBA ПAPAMETPA- KOOPДИHATЫ И
; PAЗMEPЫ.
        call FuncHead
        call OrdinaryFrame
        jp FuncTail

OrdinaryFrame
	LD	HL,FrameType1
	JR	Frame1
DoubleFrame        
	LD	HL,FrameType2
Frame1	di
        LD	D,(IX+Yw)
	LD	E,(IX+Xw)
        ld      a,(PrintAttr)
	LD	B,a
	PUSH	DE
	PUSH	HL
	LD	A,(HL)
	LD	C,Dss.WrChar
        PUSH    BC
	call    CallDss
        POP     BC
	POP	HL
	POP	DE
	LD	A,(IX+dX)
	DEC	A
	ADD	A,E
	LD	E,A
	INC	HL
	LD	A,(HL)
	PUSH	DE
	PUSH	HL
        PUSH    BC
	call    CallDss
        POP     BC
	POP	HL
	POP	DE
	LD	A,(IX+dY)
	DEC	A
	ADD	A,D
	LD	D,A
	INC	HL
	LD	A,(HL)
	PUSH	DE
	PUSH	HL
        PUSH    BC
	call    CallDss
        POP     BC
	POP	HL
	POP	DE
	LD	E,(IX+Xw)
	INC	HL
	LD	A,(HL)
        EX      AF,AF'
	INC	HL
	LD	A,(HL)
	LD	(FramHor),A
        INC     HL
	LD	A,(HL)
	LD	(FramVer),A
        EX      AF,AF'
	LD	C,Dss.WrChar
        PUSH    BC
	call    CallDss
        POP     BC
;HORIZONTAL LINES
	LD	H,(IX+Yw)
	LD	A,(IX+dY)
	DEC	A
	ADD 	A,H
	LD 	L,A
	LD	A,(IX+dX)
	SUB	2
	LD	E,(IX+Xw)
	INC	E
        LD	C,Dss.WrChar
FramHl1	PUSH	AF
	PUSH	HL
	LD	D,H
	LD	A,0
FramHor	EQU	$-1
	PUSH	AF
        PUSH    BC
        push    hl
	call    CallDss
        pop     hl
        POP     BC
	POP	AF
	LD	D,L
        PUSH    BC
	call    CallDss
        POP     BC
	POP	HL
	POP	AF
	INC	E
	DEC	A
	JR	NZ,FramHl1
;VERTICAL LINES
	LD	H,(IX+Xw)
	LD	A,(IX+dX)
	DEC	A
	ADD 	A,H
	LD 	L,A
	ld      a,(PrintAttr)
	LD	B,a
	LD	A,(IX+dY)
	SUB	2
	LD	C,Dss.WrChar
	LD	D,(IX+Yw)
	INC	D
FramVl1	PUSH	AF
	PUSH	HL
	LD	E,H
	LD	A,0
FramVer	EQU	$-1
	PUSH	AF
        PUSH    BC
        PUSH    HL
	call    CallDss
        POP     HL
        POP     BC
	POP	AF
	LD	E,L
        PUSH    BC
	call    CallDss
        POP     BC
	POP	HL
	POP	AF
	INC	D
	DEC	A
	JR	NZ,FramVl1
        ei
	RET

;Ordinary frame
FrameType1
	DB	#DA,#BF,#D9,#C0,#C4,#B3
;Double frame
FrameType2
	DB	#C9,#BB,#BC,#C8,#CD,#BA

OpenWindow
;BXOД: HL-KOOPДИHATЫ;
;      DE-PAЗMEPЫ; 
;      A -ATPИБУT. 
        ld (PrintAttr),a:push hl:push de
        call Clear_Wind:call Frame_Wind
        pop de:pop hl:ret

;___________________________

;  CИCTEMA BЫБOPA ИЗ MEHЮ 
;___________________________

;УПPABЛЯЮЩИИ CПИCOK ЗA KOMAHДOИ:
;
; 1.TEKУЩAЯ OПЦИЯ
; 2.ЧИCЛO OПЦИИ
; 3.MACCИB CTPУKTУP:
; KOOPДИHATЫ KУPCOPA (2);
; ДЛИHA KУPCOPA (1);
; CИMBOЛ BЫБOPA (1);
; AДPEC ПEPEXOДA (2).

Menu_Addr DW 0

Menu_Struct
;BЫXOД: IX -AДPEC CTPУKTУPЫ
; HOMEP A (A<40)
        push bc
        add a,a
        ld c,a
        add a,a
        add a,c
        ld c,a
        ld b,0   ;bc:= a*6
        inc bc
        inc bc
        ld ix,(Menu_Addr)
        add ix,bc
        pop bc
        ret

;Окраска пункта меню
Menu_Cursor     di
                push de
                push af
                ld hl,(Menu_Addr)
                ld a,(hl)
                call Menu_Struct
                ld d,(ix+0)
                ld e,(ix+1)
                push de
                ld c,Dss.RdChar
                call CallDss
                pop de
                push bc
                ld c,Bios.Lp_Set_Place
                call CallBios
                pop bc
                ld a,b
                and 7
                rlca:rlca:rlca:rlca
                ld c,a
                ld a,b
                and %01110000
                rrca:rrca:rrca:rrca
                or c
                ld c,a
                ld a,b
                and %10001000
                or c
                ld e,a
                ld b,(ix+2)
                ld c,Bios.Lp_Print_Atr
                call CallBios
                pop af
                pop de
                ei
                ret

Menu            push bc
                push de
                ld (Menu_Addr),hl
Menu1           call Menu_Cursor
Menu2           ld c,Dss.WaitKey
                call CallDss
                call Menu_Cursor
                ld c,a
                ld hl,(Menu_Addr)
                ld b,2
                call Case
                DB 13:DW MenuEnt        ;Enter
                DB #1b:DW MenuEsc       ;Esc
                ld a,d
                ld b,4
                call Case
                DB #58:DW Menu_Up       ;Cursor Up
                DB #52:DW Menu_Dn       ;Cursor Dn
                DB #56:DW Menu_Dn       ;>
                DB #54:DW Menu_Up       ;<
.nokey          inc hl
                ld b,(hl)
                dec hl
Menu3           ld a,b
                dec a
                call Menu_Struct
                ld a,(ix+3)
                cp c
                jr z,Menu4
                djnz Menu3
                jr Menu1

Menu4   dec b:ld (hl),b
MenuEnt call Menu_Cursor:call Beep
        ld l,(ix+4):ld h,(ix+5)
        pop de:pop bc:ex (sp),hl
        ret

Menu_Dn ld a,(hl):inc a:inc hl
        cp (hl):dec hl:ld (hl),a
        jr c,Menu5:ld (hl),0
Menu5   call Beep:jr Menu1

Menu_Up ld a,(hl):or a:jr z,Menu6
         dec a:jr Menu7
Menu6   inc hl:ld a,(hl):dec a:dec hl
Menu7   ld (hl),a:jr Menu5

MenuEsc pop de:pop bc:ret

;[]----------------------------------------------------------[]
; Конкатенация строки. Добавляется строка из hl в конец строки в de. Нуль копируется тоже.
; (hl) -> (de)
ConcatString:	ex	hl,de
.loop:		ld	a,(hl)
		or	a
		jr	z,.end
		inc	hl
		jr	.loop
.end:		ex	hl,de
;Внимание!!! ожидается, что дальше идет CopyString, поэтому если между этими п/п добавили что-то - добавь вызов CopyString!
;[]----------------------------------------------------------[]
; Копирование строки до нуля. Нуль копируется тоже.
; (hl) -> (de)
CopyString:	ld	a,(hl)
		ldi
		or	a
		jr	nz,$-4
		dec	de
		ret