

Data_Segment		equ 003Fh

; -----------------------------------------------------------------
; Virtual cursor position.
; This is where the cursor would be on the PC.
; -----------------------------------------------------------------

Data_CursorVirtual:		equ 0000h

; -----------------------------------------------------------------
; Physical cursor position.
; This is where the cursor actually is on the CBM.
; -----------------------------------------------------------------

Data_CursorPhysical:	equ 0002h

; -----------------------------------------------------------------
; Number of 256-byte sectors in disk sector.
; -----------------------------------------------------------------

Data_SectorSize:	equ 0004h

; -----------------------------------------------------------------
; Number of sectors on track.
; -----------------------------------------------------------------

Data_TrackSize:		equ 0005h

; -----------------------------------------------------------------
; Number of disk heads.
; -----------------------------------------------------------------

Data_NumHeads:		equ 0006h

; -----------------------------------------------------------------
; Debug flag - valid only in in debug mode.
; -----------------------------------------------------------------

Data_Debug:			equ 0007h

; -----------------------------------------------------------------
; Memory size in segments.
; -----------------------------------------------------------------

Data_MemSize:		equ 0008h

; -----------------------------------------------------------------
; Tick count helper.
; -----------------------------------------------------------------

Data_Ticks:			equ 000Ah

; -----------------------------------------------------------------
; Boot flag - set if the sustem is booting.
; -----------------------------------------------------------------

Data_Boot:			equ 000Ch

; -----------------------------------------------------------------
; SD card presence flags.
; -----------------------------------------------------------------

Data_SD:			equ 000Dh

; -----------------------------------------------------------------
; Video refresh counter.
; -----------------------------------------------------------------

Data_Refresh:		equ 000Eh

Data_Length			equ 15
