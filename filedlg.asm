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

FileDlg.AttrMain   equ %00010111
FileDlg.AttrSel    equ %01110000
FileDlg.AttrField  equ %01110000
FileDlg.AttrBtn    equ %01110001
FileDlg.AttrTitle  equ %00011111

FileDlg.EntrySize  equ 16
FileDlg.EntryHdr   equ #C000                      ; count word (in Pg3 @ bank 3)
FileDlg.EntryBase  equ #C010
FileDlg.MaxEntries equ 1020

FileDlg.AttrDir    equ #10

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
        ld hl,0
        ld (FileDlg.listCursor),hl
        ld (FileDlg.listOffset),hl

        ld hl,(FileDlg.userBuf)
        ld de,FileDlg.nameBuf
        call FileDlg_CopyStr
        call FileDlg_RecalcName

        call FileDlg_ReadPath
        call FileDlg_ScanDir

        call FileDlg_DrawFrame
        call FileDlg_DrawPath
        call FileDlg_DrawListFull
        call FileDlg_DrawButtons

FileDlg_Loop:
        call FileDlg_DrawName
        call FileDlg_PositionCursor
        call ReadKey
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
        ld a,FileDlg.AttrField
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

        ld a,FileDlg.BtnY
        ld (PrintXY+1),a
        ld a,FileDlg.X + 18
        ld (PrintXY),a
        ld a,FileDlg.AttrBtn
        ld (PrintAttr),a
        ld a,(FileDlg.mode)
        or a
        ld hl,FileDlg.lblOpen
        jr z,.o
        ld hl,FileDlg.lblSave
.o      call OutHL

        ld a,FileDlg.BtnY
        ld (PrintXY+1),a
        ld a,FileDlg.X + 38
        ld (PrintXY),a
        ld hl,FileDlg.lblCancel
        call OutHL

        ld a,FileDlg.AttrMain
        ld (PrintAttr),a
        ret

FileDlg.lblOpen   db " Enter=Open ",0
FileDlg.lblSave   db " Enter=Save ",0
FileDlg.lblCancel db " Esc=Cancel ",0

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
        ld hl,0
        ld (FileDlg.listCursor),hl
        ld (FileDlg.listOffset),hl
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
.loop   push bc
        ld a,b
        call FileDlg_DrawListRow
        pop bc
        inc b
        ld a,b
        cp FileDlg.ListRows
        jr c,.loop
        ld hl,(FileDlg.listOffset)
        ld (FileDlg.lastOffset),hl
        ld hl,(FileDlg.listCursor)
        ld (FileDlg.lastCursor),hl
        ret

;----------------------------------------------------------
;  Incremental list repaint: only touch rows whose state changed.
;----------------------------------------------------------
FileDlg_RefreshList:
        ld hl,(FileDlg.listOffset)
        ld de,(FileDlg.lastOffset)
        or a
        sbc hl,de
        ld a,h
        or l
        jp nz,FileDlg_DrawListFull

        ; redraw old cursor row (to clear highlight)
        ld hl,(FileDlg.lastCursor)
        ld de,(FileDlg.lastOffset)
        or a
        sbc hl,de
        ld a,h
        or a
        jr nz,.skipOld
        ld a,l
        cp FileDlg.ListRows
        jr nc,.skipOld
        push af
        call FileDlg_DrawListRow
        pop af
.skipOld
        ld hl,(FileDlg.listCursor)
        ld de,(FileDlg.listOffset)
        or a
        sbc hl,de
        ld a,h
        or a
        jr nz,.skipNew
        ld a,l
        cp FileDlg.ListRows
        jr nc,.skipNew
        call FileDlg_DrawListRow
.skipNew
        ld hl,(FileDlg.listCursor)
        ld (FileDlg.lastCursor),hl
        ld hl,(FileDlg.listOffset)
        ld (FileDlg.lastOffset),hl
        ret

;----------------------------------------------------------
;  Draw one list row. A = row index (0..ListRows-1).
;----------------------------------------------------------
FileDlg_DrawListRow:
        push af
        add a,FileDlg.ListY
        ld (PrintXY+1),a
        ld a,FileDlg.ListX
        ld (PrintXY),a
        pop af

        ; global index = offset + A
        ld e,a
        ld d,0
        ld hl,(FileDlg.listOffset)
        add hl,de
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
        ld c,FileDlg.ListW
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
        ld a,FileDlg.AttrSel
.noHi   ld (PrintAttr),a
        call FileDlg_EntryAddr
        call FileDlg_RenderEntry
        ret

;----------------------------------------------------------
;  Render entry row. HL -> 16-byte entry.
;  Writes exactly FileDlg.ListW chars at current PrintXY.
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
        ld a,FileDlg.ListW
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
        jp z,FileDlg_ListUp
        cp #52
        jp z,FileDlg_ListDown
        cp #59
        jp z,FileDlg_ListPgUp
        cp #53
        jp z,FileDlg_ListPgDn
        cp #4F
        jp z,FileDlg_NameDel
        cp #54
        jp z,FileDlg_NameLeft
        cp #56
        jp z,FileDlg_NameRight
        cp #57
        jp z,FileDlg_NameHome
        cp #51
        jp z,FileDlg_NameEnd
        cp #3D
        jp z,FileDlg_Parent
        cp #41
        jp z,FileDlg_Refresh

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
;  Enter handling: empty name -> activate selected row;
;  ".." -> ChDir up; other -> return name to caller.
;----------------------------------------------------------
FileDlg_OnEnter:
        ld a,(FileDlg.nameLen)
        or a
        jp z,FileDlg_ActivateSelected

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
FileDlg_ListUp:
        ld hl,(FileDlg.listCursor)
        ld a,h
        or l
        jr z,.stay
        dec hl
        ld (FileDlg.listCursor),hl
        ld de,(FileDlg.listOffset)
        or a
        sbc hl,de
        jr nc,.done
        ld hl,(FileDlg.listCursor)
        ld (FileDlg.listOffset),hl
.done
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
        ; if cursor - offset >= ListRows, scroll
        ld de,(FileDlg.listOffset)
        push hl
        or a
        sbc hl,de
        ld a,l
        pop hl
        cp FileDlg.ListRows
        jr c,.done
        ld de,FileDlg.ListRows-1
        or a
        sbc hl,de
        ld (FileDlg.listOffset),hl
.done
        call FileDlg_RefreshList
.stay   or a
        ret

FileDlg_ListPgUp:
        ld hl,(FileDlg.EntryHdr)
        ld a,h
        or l
        jr z,.stay
        ld hl,(FileDlg.listCursor)
        ld de,FileDlg.ListRows
        or a
        sbc hl,de
        jr nc,.cok
        ld hl,0
.cok    ld (FileDlg.listCursor),hl
        ld de,(FileDlg.listOffset)
        push hl
        or a
        sbc hl,de
        pop hl
        jr nc,.redraw
        ld (FileDlg.listOffset),hl
.redraw call FileDlg_DrawListFull
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
        ld bc,FileDlg.ListRows
        add hl,bc
        push hl
        or a
        sbc hl,de
        pop hl
        jr c,.noClip
        ld h,d
        ld l,e
.noClip ld (FileDlg.listCursor),hl
        ; adjust offset: ensure cursor visible
        ld de,(FileDlg.listOffset)
        push hl
        or a
        sbc hl,de
        ld a,l
        pop hl
        cp FileDlg.ListRows
        jr c,.redraw
        ld de,FileDlg.ListRows-1
        or a
        sbc hl,de
        ld (FileDlg.listOffset),hl
.redraw call FileDlg_DrawListFull
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
;  Tab: copy selected entry name into name field.
;----------------------------------------------------------
FileDlg_OnTab:
        ld hl,(FileDlg.EntryHdr)
        ld a,h
        or l
        jr z,.d
        ld hl,(FileDlg.listCursor)
        ld de,(FileDlg.EntryHdr)
        or a
        sbc hl,de
        jr nc,.d
        ld hl,(FileDlg.listCursor)
        call FileDlg_EntryAddr
        inc hl
        ld de,FileDlg.nameBuf
        call FileDlg_NameToStr
        call FileDlg_RecalcName
.d      or a
        ret

;----------------------------------------------------------
;  Data
;----------------------------------------------------------
FileDlg.titlePtr   dw 0
FileDlg.userBuf    dw 0
FileDlg.mode       db 0
FileDlg.action     db 0
FileDlg.savedP3    db 0

FileDlg.listCursor dw 0
FileDlg.listOffset dw 0
FileDlg.lastCursor dw 0
FileDlg.lastOffset dw 0

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
