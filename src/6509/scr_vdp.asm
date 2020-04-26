 ; ipc_vdp.asm
; version 0.73k modified by Vossi (c) 03/2019 for the v9958-card
; video buffer copy, full charset, vdp init
; added reverse-attribute-copy + cursor
; todo - optimize screen copy for speed!
; assemble with xa65:
; xa src/6509/scr_vdp.asm -O PETSCII -o dist/prg/screen_vdp.prg

PAL = 0         ; PAL=1, NTSC=0     selects V9938/58 PAL RGB-output, NTSC has a higher picture
; constants:
VDPREG18                = $0d       ; VDP reg 18 value (V/H screen adjust, $0d = Sony PVM 9")
VDPREG9                 = $80|PAL*2 ; VDP reg 9 value ($80 = NTSC, $82 = PAL / 212 lines)

CPU_ACCESS_BANK = $1

;--------------------------------------------------------------------
; Zero page variables
;--------------------------------------------------------------------

mem_pointer = $50               ; pointer for memory reading
page_count = $52                ; screen copy page counter
tmp_byte = $53                  ; temp storage for reverse bit composing
table_count = $54               ; counter for tamp color table

; VDP variables in zero page free space
vdp_color               = $f4          ; color register 7 mirror

;--------------------------------------------------------------------
; I/O chip ports
;--------------------------------------------------------------------

CRTC_RegNo = $d800
CRTC_RegVal = $d801

; VDP addresses:
VDPAddress  = $d900
PatternTable = $10              ; highbyte pattern-table address
ScreenTable = $00               ; highbyte screen-table address
ColorTable = $0a                ; highbyte color table (attribute table)
FontData = $a0                  ; highbyte font data load address in bank 1
FontSize = $08                  ; higbyte font size
VDPRamWrite = VDPAddress        ; VDP ports
VDPControl  = VDPAddress+1
VDPPalette  = VDPAddress+2

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
    jmp VdpInit
    rts

;--------------------------------------------------------------------
; IPC function 1D (console services) additional handler.
;--------------------------------------------------------------------
    
ipc_1d_console:
    cmp #$05
    bne console_not_05
    jsr screen_copy
    lda ipc_buffer+3
    tay
    lda ipc_buffer+4
    tax
    clc
    jsr PLOT
console_not_05:
    cmp #$06
    bne console_not_06
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
    stx mem_pointer
    ldx #$00
    stx mem_pointer+1
screen_search_1:
    ldy #5
screen_search_2:
    lda (mem_pointer), y
    cmp screen_marker, y
    bne screen_search_3
    dey
    bpl screen_search_2
    lda mem_pointer+1
    sta screen_location+1
    sta ipc_buffer+4
    lda CPU_ACCESS_BANK
    sta screen_location
    sta ipc_buffer+3
    rts
screen_search_3:
    inc mem_pointer+1
    bne screen_search_2
    ldx CPU_ACCESS_BANK
    inx
    stx CPU_ACCESS_BANK
    cpx #$0D
    bne screen_search_2
    ldy #$0C
    sty CPU_ACCESS_BANK
screen_notfound:
    lda (mem_pointer), y
    sta $0180, y
    dey
    bpl screen_notfound
    lda #$0F
    sta CPU_ACCESS_BANK
    rts
    
    
;--------------------------------------------------------------------
; Copy the PC screen to V9958 VRAM.
;--------------------------------------------------------------------
    
screen_copy:
    lda screen_location
    sta CPU_ACCESS_BANK
    lda screen_location+1
    ora #$0f
    sta mem_pointer+1           ; set pointer to last MDA-memory-Char+1 ($xfa0)
    lda #$a0
    sta mem_pointer
    ldy #$00
    sty VDPControl              ; set VRAM-write-pointer to screen table
    lda # (ScreenTable | $40)   ; highbyte screen table or $40 to start VRAM write 
    sta VDPControl              ; write highbyte screen table to VDP
    sty page_count
    sty table_count
    lda(mem_pointer),y
    sta VDPRamWrite             ; write first char from $fa0 to VRAM / screen table
    iny
    lda(mem_pointer),y          ; load attribute byte from MDA-memory
    tax
    lda reverse_table,x         ; convert to $80 if char is reverse
    asl                         ; shift reverse bit in carry
    rol tmp_byte                ; shift reverse bit from carry to temp-variable
    lda screen_location+1
    sta mem_pointer+1           ; set pointer to MDA-memory start
    lda #$00
    sta mem_pointer
    iny
screen_loop:
    lda (mem_pointer),y
    sta VDPRamWrite             ; write character to VRAM / screen table
    iny    
    lda (mem_pointer),y         ; load attribute byte from MDA-memory
    tax
    lda reverse_table,x         ; conert to $80 if char is reverse
    asl                         ; shift reverse bit in carry
    rol tmp_byte                ; shift reverse bit from carry to temp-variable
    tya
    and #$0f
    cmp #$0f                    ; 8. char of table?
    bne byte_not_full
    ldx table_count
    lda tmp_byte
    sta temp_color_table,x      ; store composed reverse attribute byte to table
    inx
    stx table_count
byte_not_full:
    cpy #$9f
    bne screen_notend
    ldx page_count
    cpx #$0f
    beq screen_end
screen_notend:
    iny
    bne screen_loop
    inc mem_pointer+1
    inc page_count
    bne screen_loop             ; = jmp, cause always not 0
screen_end:
                            ; * Reverse cursor bit in color-table
    lda $c8                     ; load screen line pointer lowbyte
    clc
    adc $cb                     ; add cursor column
    sta table_count             ; store in table-pointer
    lda #$00
    adc $c9                     ; load screen line pointer highbyte with carry
    and #$0f                    ; isolate low nibble (screen address 0 - $7cf)
    lsr
    ror table_count             ; divide with 8 (8 reverse bits in one byte)
    lsr
    ror table_count
    lsr
    ror table_count
    lda $cb                     ; load cursor column
    and #$07                    ; isolate bit #0-2 to find the right bit position
    tax
    lda bit_table,x             ; load bit value for XOR reversing
    ldx table_count
    eor temp_color_table,x      ; reverse the cursor position bit
    sta temp_color_table,x
                            ; * copy color table to VRAM
    ldx #$00
    stx VDPControl              ; set VRAM-write-pointer to screen table
    lda # (ColorTable | $40)    ; highbyte color table or $40 to start VRAM write 
    sta VDPControl              ; write highbyte screen table to VDP
    pha                         ; wait for VDP
    pla
color_loop:
    lda temp_color_table,x      ; copy color table to vram
    sta VDPRamWrite
    inx
    cpx #$fa                    ; 250 bytes reached
    bne color_loop 
    lda #15
    sta CPU_ACCESS_BANK
    rts

;--------------------------------------------------------------------
; V9958-Card subroutines
;--------------------------------------------------------------------

VdpInit:                            ; ***** init VDP - write register and clear VRAM *****
        sei
        ldx #$00
loop_init:
        lda VdpInitData,x                   ; load data from table
        sta VDPControl                      ; write to control port - first value, second reg|80
        inx
        cpx # VdpInitDataEnd - VdpInitData  ; $18 = end of table reached?
        bne loop_init                       ; last 2 bytes $00,$40 inits VRAM write at $0000
        tay                                 ; move last value $40 to Y as $4000 VRAM counter
        lsr                                 ; shift $40 right for screen table init value=$20 'space'

loop_clearvram:                     ; * clear vram and fill screen table with $20 *****
        sta VDPRamWrite                     ; write to VRAM
        pha 
        pla                                 ; wait 3+4 cycles = 3.5us for VDP (minimum CS high = 8us)
        inx
        bne loop_clearvram
        cpy #$38                            ; $900-$18 end of screen table reached 
        bne not_screentableend 
        txa                                 ; clear color table and rest of VRAM with $00
not_screentableend:
        dey
        bne loop_clearvram                  ; $40 reached? X starts at $20 -> clears only $3fdf bytes

loop_palette:                       ; * copy CGA color-palette *****
        lda VdpPaletteData,x                ; X already 0, palette pointer already 0 from init-data
        sta VDPPalette
        inx
        cpx # (VdpPaletteDataEnd - VdpPaletteData)
        bne loop_palette 
        
        lda vdp_color                       ; load color from color mirror variable
        sta VDPControl                      ; write new color value
        asl                                 ; move background-color bit# 0-3 to bit# 4-7
        asl
        asl
        asl
        sta tmp_byte                     ; safe new reverse text-color
        lda # (7 | $80)
        sta VDPControl                      ; write register 7
        lda vdp_color                       ; load color from color mirror variable
        lsr
        lsr
        lsr
        lsr
        ora tmp_byte                     ; compose reverse color combination
        sta VDPControl                      ; write reverse color value
        lda # (12 | $80)
        sta VDPControl                      ; write blink color register 
                                    ; * copy PC-font to pattern generator table *****
        lda # FontData                      ; load highbyte font data address
        sta mem_pointer+1                   ; set highbyte font data
        ldy #$00
        sty VDPControl                      ; write lowbyte pattern table to VDP, Y already $00
        lda # (PatternTable | $40)          ; highbyte pattern table or $40 to start VRAM write 
        sta VDPControl                      ; write highbyte pattern table to VDP
        sty mem_pointer                     ; set lowbyte font data to $00 
        ldx # FontSize                      ; load font size
loop_font:
        lda(mem_pointer),y                  ; load font data 
        sta VDPRamWrite                     ; write to VDP
        iny
        nop                                 ; wait for VDP                 
        bne loop_font
        inc mem_pointer+1                   ; increase higbyte font pointer
        dex
        bne loop_font
end_font:
        cli
        rts

VdpInitData:                        ; ***** VDP init data table *****
        .byt $04,$80,$50,$81,$03,$82,$2f,$83
        .byt $02,$84,$f0,$87,$08,$88,VDPREG9,$89
        .byt $00,$8a,$0f,$8c,$f0,$8d,$00,$90
        .byt VDPREG18,$92,$00,$40              ; last 2 bytes start VRAM write! 
        ; reg  0 $04 mode control 1 text mode 2 (bit#1-3 = M3 - M5)
        ; reg  1 $50 mode control 2 bit#1 16x16 sprites, bit#3-4 = M2-M1, #6 =1 display enable)
        ; reg  2 $03 name (screen) table base address $0000 ( * $400 + bit#0+1 = 1)
        ; reg  3 $2f color table base address $0A00 ( * $40 + bit#0-2 = 1)
        ; reg  4 $02 pattern (character) generator table base address $1000 (* $800)
        ; reg  7 $f0 text/overscan-backdrop color 
        ; reg  8 $08 bit#3 = 1 64k VRAM chips, bit#1 = 0 sprites disable, bit#5 0=transparent
        ; reg  9 $80 bit#1 = NTSC/PAL, #2 = EVEN/ODD, #3 = interlace, #7 = 192/212 lines
        ; reg 10 $00 color table base address $0000 bit#0-2 = A14-A16
        ; reg 12 $0f text/background blink color
        ; reg 13 $f0 blink periods ON/OFF - f0 = blinking off
        ; reg 14 $00 VRAM write addresss bit#0-2 = A14-A16
        ; reg 16 $00 color palette pointer to color 0
VdpInitDataEnd:

; ***** Color Palette - 16 colors, 2 byte/color RB, 0G each 3bit -> CGA-colors *****
VdpPaletteData:
    .byt $00,$00,$05,$00,$00,$05,$05,$05   ;   0=black     1=blue      2=green       3=cyan
    .byt $50,$00,$55,$00,$50,$03,$55,$05   ;   4=red       5=magenta   6=brown       7=lightgrey
    .byt $33,$03,$37,$03,$33,$07,$37,$07   ;   8=grey      9=lightblue a=lightgreen  b=lightcyan
    .byt $73,$03,$77,$03,$73,$07,$77,$07   ;   c=lightred  d=ltmagenta e=yellow      f=white
VdpPaletteDataEnd:

;--------------------------------------------------------------------
; Location of the screen memory
;--------------------------------------------------------------------

screen_location:
    .byt 4
    .byt $80

bit_table:
    .byt $80, $40, $20, $10, $08, $04, $02, $01

    .dsb ($0600-*), $AA

;--------------------------------------------------------------------
; Attribute conversion (MDA to reverse bit)
;--------------------------------------------------------------------
    
reverse_table:
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

temp_color_table: