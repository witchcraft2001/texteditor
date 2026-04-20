;__________________________________________________
;
;  Universal File Dialog
;  Open/Save with directory browsing
;__________________________________________________

; Entry:
;   HL = pointer to title string (0-terminated)
;   DE = pointer to filename buffer (128 bytes, 0-terminated,
;        pre-filled with initial name, updated on OK).
;   A  = mode: 0 = Open, non-0 = Save (changes primary button label)
; Exit:
;   CF = 0  -> user confirmed, name buffer holds the chosen name
;   CF = 1  -> user canceled

; -------- layout (80x32 screen) --------
FileDlg.X          equ 5
FileDlg.Y          equ 3
FileDlg.DX         equ 70
FileDlg.DY         equ 25
FileDlg.TitleY     equ FileDlg.Y + 1             ; 4
FileDlg.PathY      equ FileDlg.Y + 3             ; 6
FileDlg.NameY      equ FileDlg.Y + 5             ; 8
FileDlg.ListY      equ FileDlg.Y + 7             ; 10
FileDlg.ListRows   equ 15
FileDlg.ListYEnd   equ FileDlg.ListY + FileDlg.ListRows - 1  ; 24
FileDlg.BtnY       equ FileDlg.Y + FileDlg.DY - 2             ; 26

FileDlg.LabelX     equ FileDlg.X + 2              ; 7
FileDlg.FieldX     equ FileDlg.X + 9              ; 14
FileDlg.FieldW     equ FileDlg.DX - 10            ; 60
FileDlg.ListX      equ FileDlg.X + 2              ; 7
FileDlg.ListW      equ FileDlg.DX - 3             ; 67
FileDlg.CellW      equ 16
FileDlg.ListCols   equ FileDlg.ListW / FileDlg.CellW

FileDlg.AttrMain   equ %00010111
FileDlg.AttrSel    equ %01110000
FileDlg.AttrField  equ %01110000
FileDlg.AttrBtn    equ %01110001
FileDlg.AttrTitle  equ %00011111
FileDlg.AttrMark   equ %00110111

FileDlg.EntrySize  equ 16
FileDlg.EntryHdr   equ #C000                      ; count word (in Pg3 @ bank 3)
FileDlg.EntryBase  equ #C010
FileDlg.MaxEntries equ 1020

FileDlg.AttrDir    equ #10

FileDlg.CtlName    equ 0
FileDlg.CtlList    equ 1
FileDlg.CtlOk      equ 2
FileDlg.CtlCancel  equ 3

FileDlg.BtnOpenX   equ FileDlg.X + 18
FileDlg.BtnCancelX equ FileDlg.X + 34

FileDialog:
        ld (FileDlg.titlePtr),hl
        ld (FileDlg.userBuf),de
        ld (FileDlg.mode),a

        call StoreScrn

        in a,(EmmWin.P3)
        ld (FileDlg.savedP3),a
        ld a,(EditorPages.Pg3)
        out (EmmWin.P3),a

        xor a
        ld (FileDlg.action),a
        ld (FileDlg.activeCtl),a
        dec a
        ld (FileDlg.lastActiveCtl),a
        inc a
        ld (FileDlg.listColOffset),a
        ld hl,0
        ld (FileDlg.listCursor),hl

        ld hl,(FileDlg.userBuf)
        ld de,FileDlg.nameBuf
        call FileDlg_CopyStr
        call FileDlg_RecalcName

        call FileDlg_ReadPath
        call FileDlg_ScanDir
        call FileDlg_InitFocus

        call FileDlg_DrawFrame
        call FileDlg_DrawPath
        call FileDlg_DrawListFull
        call FileDlg_DrawButtons

FileDlg_Loop:
        call FileDlg_RedrawFocus
        call FileDlg_DrawName
        call FileDlg_PositionCursor
        call FileDlg_ReadKey
        call FileDlg_HandleKey
        jr nc,FileDlg_Loop

        ld a,(FileDlg.savedP3)
        out (EmmWin.P3),a
        call RestoreScrn

        ; sync editor's CurrentDir with the directory last shown
        ; in the dialog, so bare filenames resolve against it.
        ld hl,FileDlg.pathBuf
        ld de,CurrentDir
        ld bc,128
        ldir

        ld a,(FileDlg.action)
        or a
        jr z,.cancel
        ld hl,FileDlg.nameBuf
        ld de,(FileDlg.userBuf)
        call FileDlg_CopyStr
        or a
        ret
.cancel scf
        ret

;----------------------------------------------------------
;  Copy 0-term string HL -> DE (max 127 chars + NUL).
;----------------------------------------------------------
FileDlg_CopyStr:
        ld b,127
.l      ld a,(hl)
        ld (de),a
        or a
        ret z
        inc hl
        inc de
        djnz .l
        xor a
        ld (de),a
        ret

;----------------------------------------------------------
;  Recompute nameLen from nameBuf, reset cursor to end.
;----------------------------------------------------------
FileDlg_RecalcName:
        ld hl,FileDlg.nameBuf
        ld b,0
.l      ld a,(hl)
        or a
        jr z,.d
        inc hl
        inc b
        ld a,b
        cp 127
        jr c,.l
.d      ld a,b
        ld (FileDlg.nameLen),a
        ld (FileDlg.nameCursor),a
        xor a
        ld (FileDlg.nameOffset),a
        ret

;----------------------------------------------------------
;  Length of 0-term string HL -> A (saturated 255). HL preserved.
;----------------------------------------------------------
FileDlg_StrLenA:
        push hl
        ld b,0
.l      ld a,(hl)
        or a
        jr z,.d
        inc hl
        inc b
        ld a,b
        inc a
        jr nz,.l
.d      ld a,b
        pop hl
        ret

;----------------------------------------------------------
;  Read current path into pathBuf (drive + cwd).
;----------------------------------------------------------
FileDlg_ReadPath:
        ld c,Dss.CurDisk
        call CallDss
        add a,"A"
        ld (FileDlg.pathBuf),a
        ld a,":"
        ld (FileDlg.pathBuf+1),a
        ld hl,FileDlg.pathBuf+2
        ld c,Dss.CurDir
        call CallDss
        ret

;----------------------------------------------------------
;  Draw dialog frame, labels, dividers, and title.
;----------------------------------------------------------
FileDlg_DrawFrame:
        ld a,FileDlg.AttrMain
        ld hl,#100 * FileDlg.Y | FileDlg.X
        ld de,#100 * FileDlg.DY | FileDlg.DX
        call OpenWindow

        call OutFS
        DB 22,FileDlg.TitleY+1,FileDlg.X,DIVIDER,FileDlg.DX
        DB 22,FileDlg.PathY+1,FileDlg.X,DIVIDER,FileDlg.DX
        DB 22,FileDlg.NameY+1,FileDlg.X,DIVIDER,FileDlg.DX
        DB 22,FileDlg.ListYEnd+1,FileDlg.X,DIVIDER,FileDlg.DX
        DB 22,FileDlg.PathY,FileDlg.LabelX,"Path:"
        DB 22,FileDlg.NameY,FileDlg.LabelX,"Name:"
        DB 0
        call FileDlg_DrawTitle
        ret

FileDlg_DrawTitle:
        ld hl,(FileDlg.titlePtr)
        push hl
        call FileDlg_StrLenA
        pop hl
        ; centered x = X + (DX - len) / 2
        cpl
        add a,FileDlg.DX + 1
        srl a
        add a,FileDlg.X
        ld (PrintXY),a
        ld a,FileDlg.TitleY
        ld (PrintXY+1),a
        ld a,FileDlg.AttrTitle
        ld (PrintAttr),a
        call OutHL
        ld a,FileDlg.AttrMain
        ld (PrintAttr),a
        ret

;----------------------------------------------------------
;  Draw path field (tail of path shown if too long).
;----------------------------------------------------------
FileDlg_DrawPath:
        ld a,FileDlg.AttrMain
        ld (PrintAttr),a
        ld a,FileDlg.PathY
        ld (PrintXY+1),a
        ld a,FileDlg.FieldX
        ld (PrintXY),a
        ld hl,FileDlg.pathBuf
        call FileDlg_StrLenA
        cp FileDlg.FieldW
        jr c,.fit
        sub FileDlg.FieldW
        ld c,a
        ld b,0
        add hl,bc
.fit    ld c,FileDlg.FieldW
        call FileDlg_PrintPadded
        ret

;----------------------------------------------------------
;  Print HL string, pad with spaces to C columns (1..255).
;----------------------------------------------------------
FileDlg_PrintPadded:
        ld a,c
        or a
        ret z
        ld b,a
.copy   ld a,(hl)
        or a
        jr z,.pad
        push bc
        call Print
        pop bc
        inc hl
        djnz .copy
        ret
.pad    ld a,32
        push bc
        call Print
        pop bc
        djnz .pad
        ret

;----------------------------------------------------------
;  Draw name input field.
;----------------------------------------------------------
FileDlg_DrawName:
        call FileDlg_UpdateNameOffset
        ld a,(FileDlg.activeCtl)
        cp FileDlg.CtlName
        ld a,FileDlg.AttrMain
        jr nz,.attrSet
        ld a,FileDlg.AttrField
.attrSet
        ld (PrintAttr),a
        ld a,FileDlg.NameY
        ld (PrintXY+1),a
        ld a,FileDlg.FieldX
        ld (PrintXY),a
        ld hl,FileDlg.nameBuf
        ld a,(FileDlg.nameOffset)
        ld e,a
        ld d,0
        add hl,de
        ld c,FileDlg.FieldW
        call FileDlg_PrintPadded
        ld a,FileDlg.AttrMain
        ld (PrintAttr),a
        ret

;----------------------------------------------------------
;  Keep cursor visible inside name field window.
;----------------------------------------------------------
FileDlg_UpdateNameOffset:
        ld a,(FileDlg.nameCursor)
        ld b,a                       ; b = cursor
        ld a,(FileDlg.nameOffset)
        cp b
        jr z,.checkR
        jr c,.checkR
        ; offset > cursor -> pull offset down
        ld a,b
        ld (FileDlg.nameOffset),a
        ret
.checkR
        ld a,b
        sub FileDlg.FieldW - 1
        jr nc,.adj
        ret
.adj    ld c,a                       ; minimum offset
        ld a,(FileDlg.nameOffset)
        cp c
        ret nc
        ld a,c
        ld (FileDlg.nameOffset),a
        ret

;----------------------------------------------------------
;  Place hardware cursor inside name field.
;----------------------------------------------------------
FileDlg_PositionCursor:
        ld a,(FileDlg.activeCtl)
        cp FileDlg.CtlName
        jr z,.name
        cp FileDlg.CtlList
        jr z,.list
        cp FileDlg.CtlOk
        jr z,.ok
        ld a,FileDlg.BtnCancelX + 1
        ld (CurX),a
        ld a,FileDlg.BtnY
        ld (CurY),a
        ret
.ok
        ld a,FileDlg.BtnOpenX + 1
        ld (CurX),a
        ld a,FileDlg.BtnY
        ld (CurY),a
        ret
.list
        call FileDlg_GetCursorCell
        ld a,b
        add a,FileDlg.ListY
        ld (CurY),a
        ld a,c
        add a,a
        add a,a
        add a,a
        add a,a
        add a,FileDlg.ListX + 1
        ld (CurX),a
        ret
.name
        ld a,(FileDlg.nameCursor)
        ld b,a
        ld a,(FileDlg.nameOffset)
        neg
        add a,b
        add a,FileDlg.FieldX
        ld (CurX),a
        ld a,FileDlg.NameY
        ld (CurY),a
        ret

;----------------------------------------------------------
;  Draw button row.
;----------------------------------------------------------
FileDlg_DrawButtons:
        ld a,FileDlg.AttrMain
        ld (PrintAttr),a
        ld a,FileDlg.BtnY
        ld (PrintXY+1),a
        ld a,FileDlg.X + 1
        ld (PrintXY),a
        ld b,FileDlg.DX - 2
.clr    ld a,32
        push bc
        call Print
        pop bc
        djnz .clr

        ld a,(FileDlg.activeCtl)
        cp FileDlg.CtlOk
        ld a,FileDlg.AttrMain
        jr nz,.openAttr
        ld a,FileDlg.AttrBtn
.openAttr
        ld (PrintAttr),a
        ld a,FileDlg.BtnY
        ld (PrintXY+1),a
        ld a,FileDlg.BtnOpenX
        ld (PrintXY),a
        ld a,(FileDlg.mode)
        or a
        ld hl,FileDlg.btnOpen
        jr z,.o
        ld hl,FileDlg.btnSave
.o      call OutHL

        ld a,(FileDlg.activeCtl)
        cp FileDlg.CtlCancel
        ld a,FileDlg.AttrMain
        jr nz,.cancelAttr
        ld a,FileDlg.AttrBtn
.cancelAttr
        ld (PrintAttr),a
        ld a,FileDlg.BtnY
        ld (PrintXY+1),a
        ld a,FileDlg.BtnCancelX
        ld (PrintXY),a
        ld hl,FileDlg.btnCancel
        call OutHL

        ld a,FileDlg.AttrMain
        ld (PrintAttr),a
        ld a,FileDlg.BtnY
        ld (PrintXY+1),a
        ld a,FileDlg.X + 47
        ld (PrintXY),a
        ld hl,FileDlg.lblHints
        call OutHL
        ret

FileDlg.btnOpen   db " Open ",0
FileDlg.btnSave   db " Save ",0
FileDlg.btnCancel db " Cancel ",0
FileDlg.lblHints  db "Tab=switch  Esc=cancel",0

;----------------------------------------------------------
;  Read key with cursor only in the name field.
;----------------------------------------------------------
FileDlg_ReadKey:
        ld a,(FileDlg.activeCtl)
        cp FileDlg.CtlName
        jp z,ReadKey
.wait   push hl
        ld hl,WaitConst
.loop   call Inkey
        jr nz,.done
        dec hl
        ld a,h
        or l
        jr nz,.loop
        pop hl
        jr .wait
.done   ld h,a
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
        jr nz,.graphOff
        pop bc
        ld a,h
        pop hl
        ret
.graphOff
        xor a
        ld (Graph_Fl),a
        pop bc
        ld a,h
        pop hl
        ret

;----------------------------------------------------------
;  Scan current directory, populate Pg3 entry list.
;----------------------------------------------------------
FileDlg_ScanDir:
        xor a
        ld (FileDlg.EntryHdr),a
        ld (FileDlg.EntryHdr+1),a

        ld hl,FileDlg.mask
        ld de,FileDlg.findBuf
        ld a,#37
        ld b,0
        ld c,Dss.F_First
        call CallDss
        jr c,.done
.loop
        call FileDlg_MaybeAppend
        ld de,FileDlg.findBuf
        ld c,Dss.F_Next
        call CallDss
        jr nc,.loop
.done
        xor a
        ld (FileDlg.listColOffset),a
        ld hl,0
        ld (FileDlg.listCursor),hl
        ret

;----------------------------------------------------------
;  Filter entry from findBuf (skip "." self; keep "..").
;  Append 16-byte record (attr + 11-byte name) to Pg3 list.
;----------------------------------------------------------
FileDlg_MaybeAppend:
        ld a,(FileDlg.findBuf+33)
        cp "."
        jr nz,.keep
        ld a,(FileDlg.findBuf+34)
        cp "."
        jr z,.keep
        ret                          ; "." (self) - skip
.keep
        ld hl,(FileDlg.EntryHdr)
        ld de,FileDlg.MaxEntries
        or a
        sbc hl,de
        ret nc
        ld hl,(FileDlg.EntryHdr)
        add hl,hl
        add hl,hl
        add hl,hl
        add hl,hl
        ld de,FileDlg.EntryBase
        add hl,de
        ex de,hl
        ld a,(FileDlg.findBuf+32)
        ld (de),a
        inc de
        ld hl,FileDlg.findBuf+33
        ld bc,11
        ldir
        xor a
        ld (de),a
        inc de
        ld (de),a
        inc de
        ld (de),a
        inc de
        ld (de),a
        ld hl,(FileDlg.EntryHdr)
        inc hl
        ld (FileDlg.EntryHdr),hl
        ret

;----------------------------------------------------------
;  Entry index (HL) -> entry address (HL).
;----------------------------------------------------------
FileDlg_EntryAddr:
        add hl,hl
        add hl,hl
        add hl,hl
        add hl,hl
        ld de,FileDlg.EntryBase
        add hl,de
        ret

;----------------------------------------------------------
;  Draw the whole list area (after scan/cd).
;----------------------------------------------------------
FileDlg_DrawListFull:
        ld b,0
.row    ld c,0
.col    push bc
        ld a,b
        call FileDlg_DrawListCell
        pop bc
        inc c
        ld a,c
        cp FileDlg.ListCols
        jr c,.col
        inc b
        ld a,b
        cp FileDlg.ListRows
        jr c,.row
        ld hl,(FileDlg.listCursor)
        ld (FileDlg.lastCursor),hl
        ret

;----------------------------------------------------------
;  Incremental list repaint: grid is small enough to redraw fully.
;----------------------------------------------------------
FileDlg_RefreshList:
        jp FileDlg_DrawListFull

;----------------------------------------------------------
;  Compute cursor row/column inside visible grid.
;  Out: B=row, C=visible column.
;----------------------------------------------------------
FileDlg_GetCursorCell:
        ld hl,(FileDlg.listCursor)
        ld b,0
.div    ld a,h
        or a
        jr nz,.sub
        ld a,l
        cp FileDlg.ListRows
        jr c,.done
.sub    ld de,FileDlg.ListRows
        or a
        sbc hl,de
        inc b
        jr .div
.done   ld c,b
        ld a,(FileDlg.listColOffset)
        ld d,a
        ld a,c
        sub d
        ld c,a
        ld b,l
        ret

;----------------------------------------------------------
;  Draw one list cell. A=row, C=column.
;----------------------------------------------------------
FileDlg_DrawListCell:
        ld b,a
        ld a,b
        add a,FileDlg.ListY
        ld (PrintXY+1),a
        ld a,c
        add a,a
        add a,a
        add a,a
        add a,a
        add a,FileDlg.ListX
        ld (PrintXY),a
        push bc
        ld a,(FileDlg.listColOffset)
        add a,c
        ld l,a
        ld h,0
        ld d,h
        ld e,l
        add hl,hl
        add hl,hl
        add hl,hl
        add hl,hl
        or a
        sbc hl,de                     ; hl = col * 15
        ld a,b
        ld e,a
        ld d,0
        add hl,de                     ; hl = index
        pop bc
        push hl
        ld de,(FileDlg.EntryHdr)
        or a
        sbc hl,de
        pop hl
        jr c,.exists
        jr z,.blank
.blank
        ld a,FileDlg.AttrMain
        ld (PrintAttr),a
        ld c,FileDlg.CellW
        ld a,32
        ld b,c
.b1     push bc
        call Print
        pop bc
        djnz .b1
        ret
.exists
        push hl
        ld de,(FileDlg.listCursor)
        or a
        sbc hl,de
        pop hl
        ld a,FileDlg.AttrMain
        jr nz,.noHi
        ld a,(FileDlg.activeCtl)
        cp FileDlg.CtlList
        ld a,FileDlg.AttrMark
        jr nz,.noHi
        ld a,FileDlg.AttrSel
.noHi   ld (PrintAttr),a
        call FileDlg_EntryAddr
        call FileDlg_RenderEntry
        ret

;----------------------------------------------------------
;  Render entry row. HL -> 16-byte entry.
;  Writes exactly FileDlg.CellW chars at current PrintXY.
;----------------------------------------------------------
FileDlg_RenderEntry:
        ld a,(hl)
        ld (FileDlg.entryAttr),a
        inc hl
        ld (FileDlg.entryName),hl
        xor a
        ld (FileDlg.renderCnt),a

        ld a,32
        call FileDlg_Emit

        ld a,(FileDlg.entryAttr)
        bit 4,a
        jr z,.file
        ld a,"["
        call FileDlg_Emit
        ld hl,(FileDlg.entryName)
        call FileDlg_EmitName
        ld a,"]"
        call FileDlg_Emit
        jr .pad
.file
        ld hl,(FileDlg.entryName)
        call FileDlg_EmitName
.pad
        ld a,(FileDlg.renderCnt)
        ld b,a
        ld a,FileDlg.CellW
        sub b
        ret z
        ret c
        ld b,a
.pl     ld a,32
        push bc
        call Print
        pop bc
        djnz .pl
        ret

;----------------------------------------------------------
;  Emit one char (A), bump render counter.
;----------------------------------------------------------
FileDlg_Emit:
        push hl
        call Print
        pop hl
        push hl
        ld hl,FileDlg.renderCnt
        inc (hl)
        pop hl
        ret

;----------------------------------------------------------
;  Emit 8.3 name from 11-byte buffer HL. HL preserved.
;  Updates renderCnt.
;----------------------------------------------------------
FileDlg_EmitName:
        push hl
        ld b,8
.n      ld a,(hl)
        inc hl
        cp 32
        jr z,.nEnd
        push bc
        call FileDlg_Emit
        pop bc
        djnz .n
        jr .ext
.nEnd
        dec b
        jr z,.ext
.s      inc hl
        djnz .s
.ext    ld a,(hl)
        cp 32
        jr z,.done
        ld a,"."
        call FileDlg_Emit
        ld b,3
.e      ld a,(hl)
        inc hl
        cp 32
        jr z,.done
        push bc
        call FileDlg_Emit
        pop bc
        djnz .e
.done   pop hl
        ret

;----------------------------------------------------------
;  Build 0-term 8.3 name from 11-byte buffer HL into DE.
;  Returns DE past terminator. HL preserved.
;----------------------------------------------------------
FileDlg_NameToStr:
        push hl
        ld b,8
.n      ld a,(hl)
        inc hl
        cp 32
        jr z,.nEnd
        ld (de),a
        inc de
        djnz .n
        jr .ext
.nEnd
        dec b
        jr z,.ext
.s      inc hl
        djnz .s
.ext    ld a,(hl)
        cp 32
        jr z,.term
        ld a,"."
        ld (de),a
        inc de
        ld b,3
.e      ld a,(hl)
        inc hl
        cp 32
        jr z,.term
        ld (de),a
        inc de
        djnz .e
.term   xor a
        ld (de),a
        pop hl
        ret

;----------------------------------------------------------
;  Key dispatcher. IN: A char, D scan, E ASCII. CF=1 to exit loop.
;----------------------------------------------------------
FileDlg_HandleKey:
        ld a,e
        cp 27
        jp z,FileDlg_OnEsc
        cp 13
        jp z,FileDlg_OnEnter
        cp 9
        jp z,FileDlg_OnTab
        cp 8
        jp z,FileDlg_OnBackspace

        ld a,d
        and #7F
        cp #58
        jp z,FileDlg_OnUp
        cp #52
        jp z,FileDlg_OnDown
        cp #59
        jp z,FileDlg_OnPgUp
        cp #53
        jp z,FileDlg_OnPgDn
        cp #4F
        jp z,FileDlg_OnDel
        cp #54
        jp z,FileDlg_OnLeft
        cp #56
        jp z,FileDlg_OnRight
        cp #57
        jp z,FileDlg_OnHome
        cp #51
        jp z,FileDlg_OnEnd
        cp #3D
        jp z,FileDlg_Parent
        cp #41
        jp z,FileDlg_Refresh

        ld a,(FileDlg.activeCtl)
        cp FileDlg.CtlName
        jr nz,.no
        ld a,e
        cp 32
        jr c,.no
        cp 127
        jr nc,.no
        call FileDlg_InsertChar
.no     or a
        ret

FileDlg_OnEsc:
        xor a
        ld (FileDlg.action),a
        scf
        ret

;----------------------------------------------------------
;  Enter handling: act only on the focused control.
;----------------------------------------------------------
FileDlg_OnEnter:
        ld a,(FileDlg.activeCtl)
        cp FileDlg.CtlName
        jp z,FileDlg_OnEnterName
        cp FileDlg.CtlList
        jp z,FileDlg_ActivateSelected
        cp FileDlg.CtlOk
        jp z,FileDlg_OnEnterOk
        jp FileDlg_OnEsc

FileDlg_OnEnterName:
        ld a,(FileDlg.nameLen)
        or a
        ret z

        ld a,(FileDlg.nameBuf)
        cp "."
        jr nz,.accept
        ld a,(FileDlg.nameBuf+1)
        or a
        jr z,.clear                  ; single "."
        cp "."
        jr nz,.accept
        ld a,(FileDlg.nameBuf+2)
        or a
        jr nz,.accept
        ld hl,FileDlg.nameDotDot
        call FileDlg_ChDirRefresh
        or a
        ret
.clear
        call FileDlg_ClearName
        call FileDlg_DrawName
        or a
        ret
.accept
        ld a,1
        ld (FileDlg.action),a
        scf
        ret

FileDlg_OnEnterOk:
        ld a,(FileDlg.nameLen)
        or a
        ret z
        ld a,1
        ld (FileDlg.action),a
        scf
        ret

FileDlg.nameDotDot db "..",0

;----------------------------------------------------------
;  Activate currently highlighted list entry.
;----------------------------------------------------------
FileDlg_ActivateSelected:
        ld hl,(FileDlg.EntryHdr)
        ld a,h
        or l
        jr z,.nothing
        ld hl,(FileDlg.listCursor)
        ld de,(FileDlg.EntryHdr)
        or a
        sbc hl,de
        jr nc,.nothing
        ld hl,(FileDlg.listCursor)
        call FileDlg_EntryAddr
        ld a,(hl)
        inc hl
        bit 4,a
        jr z,.file
        ld de,FileDlg.tmpName
        call FileDlg_NameToStr
        ld hl,FileDlg.tmpName
        call FileDlg_ChDirRefresh
        or a
        ret
.file
        ld de,FileDlg.tmpName
        call FileDlg_NameToStr
        ld hl,FileDlg.tmpName
        ld de,FileDlg.nameBuf
        call FileDlg_CopyStr
        call FileDlg_RecalcName
        call FileDlg_DrawName
        ld a,1
        ld (FileDlg.action),a
        scf
        ret
.nothing
        or a
        ret

;----------------------------------------------------------
;  ChDir HL, then refresh path + list + name.
;----------------------------------------------------------
FileDlg_ChDirRefresh:
        ld c,Dss.ChDir
        call CallDss
        call FileDlg_ReadPath
        call FileDlg_ScanDir
        call FileDlg_ClearName
        call FileDlg_DrawPath
        call FileDlg_DrawListFull
        call FileDlg_DrawName
        ret

FileDlg_ClearName:
        xor a
        ld (FileDlg.nameBuf),a
        ld (FileDlg.nameLen),a
        ld (FileDlg.nameCursor),a
        ld (FileDlg.nameOffset),a
        ret

FileDlg_Parent:
        ld hl,FileDlg.nameDotDot
        call FileDlg_ChDirRefresh
        or a
        ret

FileDlg_Refresh:
        call FileDlg_ReadPath
        call FileDlg_ScanDir
        call FileDlg_DrawPath
        call FileDlg_DrawListFull
        or a
        ret

;----------------------------------------------------------
;  List navigation.
;----------------------------------------------------------
FileDlg_EnsureCursorVisible:
        call FileDlg_GetCursorCell
        ld a,c
        cp FileDlg.ListCols
        jr c,.ok
        sub FileDlg.ListCols-1
        ld (FileDlg.listColOffset),a
        ret
.ok     ld a,c
        bit 7,a
        ret z
        call FileDlg_GetCursorColumn
        ld (FileDlg.listColOffset),a
        ret

FileDlg_GetCursorColumn:
        ld hl,(FileDlg.listCursor)
        xor a
.div    ld b,a
        ld a,h
        or a
        jr nz,.sub
        ld a,l
        cp FileDlg.ListRows
        jr c,.done
.sub    ld a,b
        inc a
        push af
        ld de,FileDlg.ListRows
        or a
        sbc hl,de
        pop af
        jr .div
.done   ld a,b
        ret

FileDlg_ListUp:
        ld hl,(FileDlg.listCursor)
        ld a,h
        or l
        jr z,.stay
        dec hl
        ld (FileDlg.listCursor),hl
        call FileDlg_EnsureCursorVisible
        call FileDlg_RefreshList
.stay   or a
        ret

FileDlg_ListDown:
        ld hl,(FileDlg.EntryHdr)
        ld a,h
        or l
        jr z,.stay
        ld de,(FileDlg.listCursor)
        dec hl
        or a
        sbc hl,de
        jr z,.stay
        jr c,.stay
        ld hl,(FileDlg.listCursor)
        inc hl
        ld (FileDlg.listCursor),hl
        call FileDlg_EnsureCursorVisible
        call FileDlg_RefreshList
.stay   or a
        ret

FileDlg_ListPgUp:
        ld hl,(FileDlg.EntryHdr)
        ld a,h
        or l
        jr z,.stay
        ld hl,(FileDlg.listCursor)
        ld de,FileDlg.ListRows * FileDlg.ListCols
        or a
        sbc hl,de
        jr nc,.cok
        ld hl,0
.cok    ld (FileDlg.listCursor),hl
        call FileDlg_EnsureCursorVisible
        call FileDlg_DrawListFull
.stay   or a
        ret

FileDlg_ListPgDn:
        ld hl,(FileDlg.EntryHdr)
        ld a,h
        or l
        jr z,.stay
        dec hl
        ld de,(FileDlg.listCursor)
        ex de,hl                     ; hl=cursor, de=count-1
        ld bc,FileDlg.ListRows * FileDlg.ListCols
        add hl,bc
        push hl
        or a
        sbc hl,de
        pop hl
        jr c,.noClip
        ld h,d
        ld l,e
.noClip ld (FileDlg.listCursor),hl
        call FileDlg_EnsureCursorVisible
        call FileDlg_DrawListFull
.stay   or a
        ret

;----------------------------------------------------------
;  Name editing helpers.
;----------------------------------------------------------
FileDlg_NameLeft:
        ld a,(FileDlg.nameCursor)
        or a
        jr z,.d
        dec a
        ld (FileDlg.nameCursor),a
.d      or a
        ret

FileDlg_NameRight:
        ld a,(FileDlg.nameCursor)
        ld b,a
        ld a,(FileDlg.nameLen)
        cp b
        jr z,.d
        inc b
        ld a,b
        ld (FileDlg.nameCursor),a
.d      or a
        ret

FileDlg_NameHome:
        xor a
        ld (FileDlg.nameCursor),a
        ret

FileDlg_NameEnd:
        ld a,(FileDlg.nameLen)
        ld (FileDlg.nameCursor),a
        or a
        ret

FileDlg_OnBackspace:
        ld a,(FileDlg.activeCtl)
        cp FileDlg.CtlName
        jr z,.edit
        or a
        ret
.edit
        ld a,(FileDlg.nameCursor)
        or a
        jr z,.atLeft
        dec a
        ld (FileDlg.nameCursor),a
        call FileDlg_DeleteAtCursor
        or a
        ret
.atLeft
        ld a,(FileDlg.nameLen)
        or a
        jr nz,.d
        jp FileDlg_Parent
.d      or a
        ret

FileDlg_NameDel:
        ld a,(FileDlg.nameCursor)
        ld b,a
        ld a,(FileDlg.nameLen)
        cp b
        jr z,.d
        call FileDlg_DeleteAtCursor
.d      or a
        ret

;----------------------------------------------------------
;  Delete char at nameCursor; shift tail left; len--.
;----------------------------------------------------------
FileDlg_DeleteAtCursor:
        ld a,(FileDlg.nameCursor)
        ld e,a
        ld d,0
        ld hl,FileDlg.nameBuf
        add hl,de
        ld d,h
        ld e,l
        inc hl
.l      ld a,(hl)
        ld (de),a
        or a
        jr z,.done
        inc hl
        inc de
        jr .l
.done
        ld a,(FileDlg.nameLen)
        or a
        ret z
        dec a
        ld (FileDlg.nameLen),a
        ret

;----------------------------------------------------------
;  Insert char A at nameCursor; len++; cursor++.
;----------------------------------------------------------
FileDlg_InsertChar:
        ld b,a
        ld a,(FileDlg.nameLen)
        cp 126
        ret nc
        ; number of chars to move = len - cursor + 1 (incl. NUL)
        ld c,a
        ld a,(FileDlg.nameCursor)
        ld e,a                       ; e = cursor
        ld a,c
        sub e
        inc a
        ld c,a                       ; c = count to shift
        ; src = nameBuf + len, dest = src + 1
        ld a,(FileDlg.nameLen)
        ld e,a
        ld d,0
        ld hl,FileDlg.nameBuf
        add hl,de
        ld d,h
        ld e,l
        inc de
.shift  ld a,(hl)
        ld (de),a
        dec hl
        dec de
        dec c
        jr nz,.shift

        ; write new char at nameBuf + cursor
        ld a,(FileDlg.nameCursor)
        ld e,a
        ld d,0
        ld hl,FileDlg.nameBuf
        add hl,de
        ld (hl),b
        ld a,(FileDlg.nameLen)
        inc a
        ld (FileDlg.nameLen),a
        ld a,(FileDlg.nameCursor)
        inc a
        ld (FileDlg.nameCursor),a
        ret

;----------------------------------------------------------
;  Tab: cycle dialog focus.
;----------------------------------------------------------
FileDlg_OnTab:
        ld a,d
        cp #8F
        jr z,FileDlg_PrevFocus
        ld a,(FileDlg.activeCtl)
        inc a
        cp FileDlg.CtlCancel + 1
        jr c,.set
        xor a
.set    ld (FileDlg.activeCtl),a
        or a
        ret

FileDlg_PrevFocus:
        ld a,(FileDlg.activeCtl)
        or a
        jr nz,.set
        ld a,FileDlg.CtlCancel + 1
.set    dec a
        ld (FileDlg.activeCtl),a
        or a
        ret

;----------------------------------------------------------
;  Initial focus: Save -> name field, Open/Merge -> list,
;  unless a name is already prefilled.
;----------------------------------------------------------
FileDlg_InitFocus:
        ld a,(FileDlg.mode)
        or a
        jr nz,.name
        ld a,(FileDlg.nameLen)
        or a
        jr nz,.name
        ld a,FileDlg.CtlList
        ld (FileDlg.activeCtl),a
        ret
.name   xor a
        ld (FileDlg.activeCtl),a
        ret

;----------------------------------------------------------
;  Redraw controls whose visuals depend on focus.
;----------------------------------------------------------
FileDlg_RedrawFocus:
        ld a,(FileDlg.activeCtl)
        ld b,a
        ld a,(FileDlg.lastActiveCtl)
        cp b
        ret z
        ld a,b
        ld (FileDlg.lastActiveCtl),a
        call FileDlg_DrawListFull
        call FileDlg_DrawButtons
        ret

FileDlg_OnUp:
        ld a,(FileDlg.activeCtl)
        cp FileDlg.CtlList
        jp z,FileDlg_ListUp
        or a
        ret

FileDlg_OnDown:
        ld a,(FileDlg.activeCtl)
        cp FileDlg.CtlList
        jp z,FileDlg_ListDown
        or a
        ret

FileDlg_OnPgUp:
        ld a,(FileDlg.activeCtl)
        cp FileDlg.CtlList
        jp z,FileDlg_ListPgUp
        or a
        ret

FileDlg_OnPgDn:
        ld a,(FileDlg.activeCtl)
        cp FileDlg.CtlList
        jp z,FileDlg_ListPgDn
        or a
        ret

FileDlg_OnDel:
        ld a,(FileDlg.activeCtl)
        cp FileDlg.CtlName
        jp z,FileDlg_NameDel
        or a
        ret

FileDlg_OnLeft:
        ld a,(FileDlg.activeCtl)
        cp FileDlg.CtlName
        jp z,FileDlg_NameLeft
        cp FileDlg.CtlList
        jp z,FileDlg_ListLeft
        cp FileDlg.CtlOk
        jr z,.stay
        cp FileDlg.CtlCancel
        jr nz,.done
        ld a,FileDlg.CtlOk
        ld (FileDlg.activeCtl),a
        jr .done
.stay   or a
        ret
.done   or a
        ret

FileDlg_OnRight:
        ld a,(FileDlg.activeCtl)
        cp FileDlg.CtlName
        jp z,FileDlg_NameRight
        cp FileDlg.CtlList
        jp z,FileDlg_ListRight
        cp FileDlg.CtlOk
        jr z,.toCancel
        cp FileDlg.CtlCancel
        jr z,.stay
        or a
        ret
.toCancel
        ld a,FileDlg.CtlCancel
        ld (FileDlg.activeCtl),a
.stay   or a
        ret

FileDlg_OnHome:
        ld a,(FileDlg.activeCtl)
        cp FileDlg.CtlName
        jp z,FileDlg_NameHome
        or a
        ret

FileDlg_OnEnd:
        ld a,(FileDlg.activeCtl)
        cp FileDlg.CtlName
        jp z,FileDlg_NameEnd
        or a
        ret

FileDlg_ListLeft:
        ld hl,(FileDlg.listCursor)
        ld de,FileDlg.ListRows
        or a
        sbc hl,de
        jr nc,.set
        or a
        ret
.set    ld (FileDlg.listCursor),hl
        call FileDlg_EnsureCursorVisible
        call FileDlg_RefreshList
        or a
        ret

FileDlg_ListRight:
        ld hl,(FileDlg.EntryHdr)
        ld a,h
        or l
        ret z
        dec hl
        ex de,hl                     ; de = last index
        ld hl,(FileDlg.listCursor)
        ld bc,FileDlg.ListRows
        add hl,bc
        push hl
        or a
        sbc hl,de
        pop hl
        jr c,.set
        jr z,.set
        or a
        ret
.set    ld (FileDlg.listCursor),hl
        call FileDlg_EnsureCursorVisible
        call FileDlg_RefreshList
        or a
        ret

;----------------------------------------------------------
;  Data
;----------------------------------------------------------
FileDlg.titlePtr   dw 0
FileDlg.userBuf    dw 0
FileDlg.mode       db 0
FileDlg.action     db 0
FileDlg.savedP3    db 0
FileDlg.activeCtl  db 0
FileDlg.lastActiveCtl db 0

FileDlg.listCursor dw 0
FileDlg.listColOffset db 0
FileDlg.lastCursor dw 0

FileDlg.nameLen    db 0
FileDlg.nameCursor db 0
FileDlg.nameOffset db 0

FileDlg.entryAttr  db 0
FileDlg.entryName  dw 0
FileDlg.renderCnt  db 0

FileDlg.mask       db "*.*",0

FlDlgTitleOpen     db " Open File ",0
FlDlgTitleSave     db " Save File As ",0
FlDlgTitleMerge    db " Merge File ",0
FlDlgTitleBlock    db " Save Block As ",0

FileDlg.nameBuf    ds 128
FileDlg.pathBuf    ds 128
FileDlg.tmpName    ds 16
FileDlg.findBuf    ds 44
