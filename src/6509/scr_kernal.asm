; ipc_kernal.asm
; version 0.73b modified by Vossi (c) 03/2019 for the v9958-card
; adds a software cursor in bios interrrupts
; assemble with xa65:
; xa src/6509/scr_kernal.asm -O PETSCII -o dist/prg/screen_kernal.prg

;--------------------------------------------------------------------
; KERNAL variables
;--------------------------------------------------------------------

RvsFlag = $0397

; VDP variables in zero page free space
vdp_color           = $f4          ; color register 7 mirror
vdp_cursor_char     = $f6          ; character under cursor
vdp_reverse         = $f7          ; reverse flag for VdpFont
vdp_copy_pointer    = $f8;+$f9     ; pointer for VdpFont


;--------------------------------------------------------------------
; I/O chip ports
;--------------------------------------------------------------------

CRTC_RegNo = $d800
CRTC_RegVal = $d801

; VDP addresses:
VDPAddress  = $d900
PatternTable = $10              ; highbyte pattern-table address
FontData = $a0                  ; highbyte font data load address in bank 1
FontSize = $04                  ; higbyte font size
VDPRamWrite = VDPAddress        ; VDP ports
VDPControl  = VDPAddress+1
VDPPalette  = VDPAddress+2

;--------------------------------------------------------------------
; KERNAL routines
;--------------------------------------------------------------------

SCROUT = $e00d

;--------------------------------------------------------------------
; Load address for the PRG file
;--------------------------------------------------------------------

    .word $0400
    * = $0400

ipc_buffer = $0805

    jmp ipc_1d_console
    rts

;--------------------------------------------------------------------
; IPC function 1D (console services) additional handler. Added V9958!
;--------------------------------------------------------------------
    
ipc_1d_console:
    cmp #$03
    bne console_not_03
    jsr VdpCursorClear  ; clear cursor = write old char non-reverse
	lda RvsFlag
	pha
	lda ipc_buffer+4
	sta RvsFlag	
	lda ipc_buffer+3
	jsr output_convert
	pla
	sta RvsFlag
    jsr VdpCursorSet    ; set cursor to screen = reverse new position
console_not_03:
    cmp #$07
    bne console_not_07
    inc $D000+79
console_not_07:
	rts

;--------------------------------------------------------------------
; CP437 screen output function.
; Performs appropriate conversion to PETSCII.
;--------------------------------------------------------------------

output_convert:
    tax
    inx
    bpl output_convert_1
    lda output_table_2-$80,x
output_convert_1:
    cpx #$21
    bcs output_convert_2
    lda output_table_1, x
output_convert_2:
    tax
    bmi output_convert_6
	jsr uppercase_convert
    jmp SCROUT
output_convert_6:
	lda #$0e
	sta CRTC_RegNo
	lda CRTC_RegVal
	and #$10
	tay
    dey
    bmi output_convert_8
    txa
    clc
    adc #$1f
    dex
    bmi output_convert_7
	adc #$40
output_convert_7:
    jmp SCROUT
output_convert_8:
    lda output_table_3-$80,x
    pha
    and #$7f
    cmp #$40
    bcc output_convert_9
    adc #$3f
output_convert_9:
    adc #$a0
    tax
    pla
    and #$80
    pha
    eor RvsFlag
    sta RvsFlag
    txa
    jsr SCROUT
    pla
    eor RvsFlag
    sta RvsFlag
    rts

uppercase_convert:
    cmp #$41
    bcc uppercase_convert_2
    cmp #$5B
    bcc uppercase_convert_1
    cmp #$61
    bcc uppercase_convert_2
    cmp #$7B
    bcs uppercase_convert_2
uppercase_convert_1:
    eor #$20
uppercase_convert_2:
    cmp #$60
    bcc uppercase_convert_3
    adc #$5F
uppercase_convert_3:
	rts
    
; Conversion of characters FF and 00-1F into meta-characters.
output_table_1:
    .byt $a0
    .byt $a0, $80, $80, $80, $80, $80, $80, $9d
    .byt $9e, $9d, $9e, $80, $80, $80, $80, $80
    .byt $97, $96, $9b, $80, $80, $80, $83, $9b
    .byt $98, $99, $97, $96, $80, $80, $98, $99

; Conversion of characters 7F-FE into meta-characters.
output_table_2:
    .byt $98
    .byt $80, $80, $80, $80, $80, $80, $80, $80
    .byt $80, $80, $80, $80, $80, $80, $80, $80
    .byt $80, $80, $80, $80, $80, $80, $80, $80
    .byt $80, $80, $80, $80, $9f, $80, $80, $80
    .byt $80, $80, $80, $80, $80, $80, $80, $80
    .byt $80, $80, $80, $80, $80, $80, $96, $97
    .byt $8a, $89, $8b, $86, $8d, $8d, $8d, $8d
    .byt $90, $91, $8e, $8f, $8c, $87, $88, $8e
    .byt $93, $8e, $8f, $8c, $87, $88, $8c, $8c
    .byt $93, $91, $8e, $8f, $8c, $87, $88, $8e
    .byt $8e, $8f, $8f, $93, $93, $91, $91, $88
    .byt $88, $92, $91, $81, $83, $82, $85, $84
    .byt $80, $80, $80, $80, $80, $80, $80, $80
    .byt $80, $80, $80, $80, $80, $80, $80, $80
    .byt $80, $80, $80, $80, $80, $80, $80, $80
    .byt $80, $9d, $9d, $9a, $80, $80, $95
    
; Conversion of meta-characters into actual PETSCII.
output_table_3:
    .byt $04, $80, $01, $02, $82, $81, $3d, $20
    .byt $3b, $3f, $06, $3f, $0b, $13, $11, $12
    .byt $0e, $10, $1d, $0d, $13, $4a, $5c, $5e
    .byt $7e, $76, $1a, $3d, $20, $4a, $ca, $6c
    .byt $00

;--------------------------------------------------------------------
; V9958-Card subroutines
;--------------------------------------------------------------------

VdpCursorSet:                       ; ***** print cursor and safe char under cursor *****
    ldy $cb                             ; load cursor column
    tya
    clc
    adc $c8                             ; add lowbyte screen line pointer
    sta VDPControl                      ; write VRAM address lowbyte
    lda #$00
    adc $c9                             ; load highbyte screen line pointer + carry
    and #$4f                            ; isolate low nibble + $dx to $4x for VRAM write
    sta VDPControl                      ; write VRAM address highbyte
    lda #$00
    ora($c8),y                          ; load char under cursur from screen memory
    sta vdp_cursor_char                 ; safe char under cursor
    eor #$80                            ; reverse char
    sta VDPRamWrite                     ; write char to VRAM
    rts

VdpCursorClear:                     ; ***** restore char under cursor position 
    lda $cb                             ; load cursor column
    clc
    adc $c8                             ; add lowbyte screen line pointer
    sta VDPControl                      ; write VRAM address lowbyte
    lda #$00
    adc $c9                             ; load highbyte screen line pointer + carry
    and #$4f                            ; isolate low nibble + $dx to $4x for VRAM write
    sta VDPControl                      ; write VRAM address highbyte
    lda vdp_cursor_char                 ; safe char under cursor
    nop
    sta VDPRamWrite                     ; write char to VRAM
    rts
