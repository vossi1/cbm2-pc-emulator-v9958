; ipc_v9958.asm
; version 0.73d modified by Vossi (c) 03/2019 for the v9958-card
; video buffer copy routine
; assemble with xa65:
; xa src/6509/scr_vdpio.asm -O PETSCII -o dist/prg/screen_vdpio.prg

CPU_ACCESS_BANK = $1
PROC_ADDR = $0100

;--------------------------------------------------------------------
; Zero page variables
;--------------------------------------------------------------------

src_addr = $50
page_count = $52
tmp_byte = $53

;--------------------------------------------------------------------
; I/O chip ports
;--------------------------------------------------------------------

CRTC_RegNo = $d800
CRTC_RegVal = $d801

; VDP addresses:
VDPAddress  = $d900
ScreenTable = $00               ; highbyte screen-table address
VDPRamWrite = VDPAddress        ; VDP ports
VDPControl  = VDPAddress+1

;--------------------------------------------------------------------
; KERNAL routines
;--------------------------------------------------------------------

PLOT = $fff0

;--------------------------------------------------------------------
; Load address for the PRG file
;--------------------------------------------------------------------

    .word $0400
    * = $0400

ipc_buffer = $0805

    jmp ipc_1d_console
    rts

;--------------------------------------------------------------------
; IPC function 1D (console services) additional handler.
;--------------------------------------------------------------------
    
ipc_1d_console:
    cmp #$05
    bne console_not_05
    jsr screen_convert
    lda ipc_buffer+3
    tay
    lda ipc_buffer+4
    tax
    clc
    jsr PLOT
    jmp VdpCursorSet
console_not_05:
    cmp #$06
    bne console_not_06
    jsr screen_init
    lda #$FF
    sta ipc_buffer+2
    lda #0
    sta ipc_buffer+3
    sta ipc_buffer+4
    jmp screen_search
console_not_06:
    cmp #$07
    bne console_not_07
    inc $D000+1999
console_not_07:
    rts

;--------------------------------------------------------------------
; String to search for in video memory
;--------------------------------------------------------------------

screen_marker:
    .byt $4d, $69, $43, $68, $41, $75

;--------------------------------------------------------------------
; Search for a magic string placed by the 8088
;--------------------------------------------------------------------

screen_search:
    ldx #$01
    stx CPU_ACCESS_BANK
    inx
    stx src_addr
    ldx #$00
    stx src_addr+1
screen_search_1:
    ldy #5
screen_search_2:
    lda (src_addr), y
    cmp screen_marker, y
    bne screen_search_3
    dey
    bpl screen_search_2
    lda src_addr+1
    sta screen_location+1
    sta ipc_buffer+4
    lda CPU_ACCESS_BANK
    sta screen_location
    sta ipc_buffer+3
    rts
screen_search_3:
    inc src_addr+1
    bne screen_search_2
    ldx CPU_ACCESS_BANK
    inx
    stx CPU_ACCESS_BANK
    cpx #$0D
    bne screen_search_2
    ldy #$0C
    sty CPU_ACCESS_BANK
screen_notfound:
    lda (src_addr), y
    sta $0180, y
    dey
    bpl screen_notfound
    lda #$0F
    sta CPU_ACCESS_BANK
    rts
    
    
;--------------------------------------------------------------------
; Convert the PC screen to video memory.
;--------------------------------------------------------------------
    
screen_convert:
    lda screen_location
    sta CPU_ACCESS_BANK
    lda screen_location+1
    sta src_addr+1
    ldy #$00
    sty src_addr
    sta page_count
    iny
    sty VDPControl              ; set VRAM-write-pointer to screen table + 1 (Y = 1)
    iny
    lda # (ScreenTable | $40)   ; highbyte screen table or $40 to start VRAM write 
    sta VDPControl              ; write highbyte screen table to VDP
    jsr PROC_ADDR
    lda #15
    sta CPU_ACCESS_BANK
    rts
    
;--------------------------------------------------------------------
; Initialize the screen conversion routine.
;--------------------------------------------------------------------

screen_init:
    ldx #screen_proc_end-screen_proc-1
screen_init_1:
    lda screen_proc,x
    sta PROC_ADDR,x
    dex
    bpl screen_init_1    
    rts
    
;--------------------------------------------------------------------
; V9958 screen conversion routine
;--------------------------------------------------------------------

screen_proc:
    lda (src_addr),y
    tax
    lda petscii_table_1,x
    sta tmp_byte
    iny
    lda (src_addr),y
    tax
    lda petscii_table_3,x
    ora tmp_byte
    cpy #$a1
    bne screen_notlast
    ldx page_count
    cpx #$0f
    beq screen_last
screen_notlast:
    sta VDPRamWrite             ; write character to VRAM / screen table
    iny
    bne screen_proc
    inc src_addr+1
    inc page_count
    bne screen_proc             ; = jmp, cause always not 0
screen_last:
    ldx #$00                    ; set VRAM-write-pointer to screen table
    stx VDPControl              ; write lowbyte screen table to VDP
    ldx # (ScreenTable | $40)   ; highbyte screen table or $40 to start VRAM write 
    stx VDPControl              ; write highbyte screen table to VDP
    pha
    pla
    pha
    pla
    sta VDPRamWrite             ; write first character to screen table
    rts
screen_proc_end:

;--------------------------------------------------------------------
; V9958-Card subroutines
;--------------------------------------------------------------------

VdpCursorSet:                       ; ***** print cursor *****
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
    ora($c8),y                          ; load char under cursor from screen memory
    eor #$80                            ; reverse char
    sta VDPRamWrite                     ; write char to VRAM
    rts

;--------------------------------------------------------------------
; Location of the screen memory
;--------------------------------------------------------------------

screen_location:
    .byt 4
    .byt $80

    .dsb ($0600-*), $AA

;--------------------------------------------------------------------
; ASCII to PETSCII (modified char ROM)
;--------------------------------------------------------------------
    
petscii_table_1:
    .byt $7f, $5f, $5f, $5f, $5f, $5f, $5f, $7c, $7d, $7c, $7d, $5f, $5f, $5f, $5f, $5f
    .byt $76, $75, $7a, $5f, $5f, $5f, $62, $7a, $77, $78, $76, $75, $5f, $5f, $77, $78
    .byt $20, $21, $22, $23, $24, $25, $26, $27, $28, $29, $2a, $2b, $2c, $2d, $2e, $2f
    .byt $30, $31, $32, $33, $34, $35, $36, $37, $38, $39, $3a, $3b, $3c, $3d, $3e, $3f
    .byt $00, $41, $42, $43, $44, $45, $46, $47, $48, $49, $4a, $4b, $4c, $4d, $4e, $4f
    .byt $50, $51, $52, $53, $54, $55, $56, $57, $58, $59, $5a, $1b, $1c, $1d, $1e, $1f
    .byt $40, $01, $02, $03, $04, $05, $06, $07, $08, $09, $0a, $0b, $0c, $0d, $0e, $0f
    .byt $10, $11, $12, $13, $14, $15, $16, $17, $18, $19, $1a, $5b, $5c, $5d, $5e, $77
    .byt $5f, $5f, $5f, $5f, $5f, $5f, $5f, $5f, $5f, $5f, $5f, $5f, $5f, $5f, $5f, $5f
    .byt $5f, $5f, $5f, $5f, $5f, $5f, $5f, $5f, $5f, $5f, $5f, $5f, $7e, $5f, $5f, $5f
    .byt $5f, $5f, $5f, $5f, $5f, $5f, $5f, $5f, $5f, $5f, $5f, $5f, $5f, $5f, $75, $76
    .byt $69, $68, $6a, $65, $6c, $6c, $6c, $6f, $6f, $6c, $65, $6f, $71, $71, $71, $6f
    .byt $72, $6d, $6e, $6b, $66, $67, $6b, $6b, $72, $70, $6d, $6e, $6b, $66, $67, $6d
    .byt $6d, $6e, $6e, $72, $72, $70, $70, $67, $67, $71, $70, $60, $62, $61, $64, $63
    .byt $5f, $5f, $5f, $5f, $5f, $5f, $5f, $5f, $5f, $5f, $5f, $5f, $5f, $5f, $5f, $5f
    .byt $5f, $5f, $5f, $5f, $5f, $5f, $5f, $5f, $5f, $7c, $7c, $79, $5f, $5f, $74, $7f

;--------------------------------------------------------------------
; Attribute conversion (MDA to reverse bit)
;--------------------------------------------------------------------
    
petscii_table_3:
    .byt $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
    .byt $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
    .byt $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
    .byt $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
    .byt $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
    .byt $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
    .byt $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
    .byt $80, $00, $00, $00, $00, $00, $00, $00, $80, $00, $00, $00, $00, $00, $00, $00
    .byt $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
    .byt $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
    .byt $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
    .byt $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
    .byt $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
    .byt $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
    .byt $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
    .byt $80, $00, $00, $00, $00, $00, $00, $00, $80, $00, $00, $00, $00, $00, $00, $00