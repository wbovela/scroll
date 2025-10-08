compile to this filename
!to "scroll.prg",cbm

;macros
!source "macros.asm"

;define constants here

;placeholder for various temp parameters
PARAM1                  = $03
PARAM2                  = $04
PARAM3                  = $05
PARAM4                  = $06
PARAM5                  = $07

;placeholder for zero page pointers
ZEROPAGE_POINTER_1      = $17
ZEROPAGE_POINTER_2      = $19
ZEROPAGE_POINTER_3      = $21
ZEROPAGE_POINTER_4      = $23

VIC_SPRITE_X_POS        = $d000
VIC_SPRITE_Y_POS        = $d001
VIC_SPRITE_X_EXTEND     = $d010
VIC_SPRITE_ENABLE       = $d015
VIC_SCREENCTRL2         = $d016
VIC_SPRITE_DBL_HEIGHT   = $d017
VIC_MEMORY_CONTROL      = $d018
VIC_SPRITE_PRIORITY     = $d01b
VIC_SPRITE_MULTICOLOR   = $d01c
VIC_SPRITE_DBL_WIDTH    = $d01d
VIC_SPRITE_MULTICOLOR_1 = $d025
VIC_SPRITE_MULTICOLOR_2 = $d026
VIC_SPRITE_COLOR        = $d027
VIC_BORDER_COLOR        = $d020
VIC_BACKGROUND_COLOR    = $d021

JOYSTICK_PORT_II        = $dc00
; all active low
; 0001 (1)= up
; 0010 (2)= down
; 0100 (4)= left
; 1000 (8)= right
; 0001 0000 (16) = fire

CIA_PRA                 = $dd00

;address of the screen buffers 
SCREEN_CHAR             = $0400
SCREEN_CHAR1            = $3000

;address of color ram
SCREEN_COLOR            = $D800

; VIC_MEMORY_CONTROL $d018
VIC_SCREEN0 = %00010000 ; $0400
VIC_SCREEN1 = %11000000 ; $3000

;set number of loops to delay scrolling
SCROLL_DELAY_COUNT    = $00

;address of sprite pointers
SPRITE_POINTER_BASE		= SCREEN_CHAR + 1016
SPRITE_POINTER_BASE1	= SCREEN_CHAR1 + 1016

;sprite number constant
SPRITE_BASE                 = 128

SPRITE_LEFT                 = SPRITE_BASE + 0
SPRITE_RIGHT                = SPRITE_BASE + 1

;this creates a basic start
*=$0801

    ;SYS 2064
    !byte $0C,$08,$0A,$00,$9E,$20,$32,$30,$36,$34,$00,$00,$00,$00,$00

    jsr initDisplay
    jsr initSprites
    jsr initRasterIrq

    ;background black
    lda #0
    sta VIC_BACKGROUND_COLOR
    sta VIC_BORDER_COLOR

    ; set 38 column mode
    lda VIC_SCREENCTRL2
    and #%11110111  ; 0-2 = fine scroll, 3 = 38/40 columns mode, 4 = multicolor mode, 5-7 unused
    sta VIC_SCREENCTRL2

    ; set scroll delay to 0
    ldx  #$00
    stx  SCROLL_DELAY

    ; start at 7 for left scrolling
    lda  #$07
    sta  SCROLL_POS

    ; set scroll position to SCROLL_POS
    lda VIC_SCREENCTRL2
    and #$F8
    clc 
    adc SCROLL_POS
    sta VIC_SCREENCTRL2   

;------------------------------------------------------------
;
;    GameLoop
;
;------------------------------------------------------------
!zone GameLoop
GameLoop  
	lda #0
	sta VIC_BORDER_COLOR

	lda SCROLL_POS
	clc
	adc #48
	sta $0400 + (2 * 40) + 15
	sta $3000 + (2 * 40) + 15

	; wait for next frame  
	jsr waitFrame
	lda #$1
	sta VIC_BORDER_COLOR

	; right pressed
	lda #$08
	bit JOYSTICK_PORT_II
	bne .noRight

	lda VIC_MEMORY_CONTROL
	and #%00001111
	ora #VIC_SCREEN0
	sta VIC_MEMORY_CONTROL

	lda #SPRITE_RIGHT
	sta SPRITE_POINTER_BASE
	sta SPRITE_POINTER_BASE1
	jsr softScrollLeft
	inc VIC_BORDER_COLOR

	lda COLOR_SCROLL_PENDING
	beq .noRight
    
	jsr doColorScrollLeft
	lda #0
	sta VIC_BORDER_COLOR
  
.noRight
	; left pressed
	lda #$04
	bit JOYSTICK_PORT_II
	bne .noLeft

	lda VIC_MEMORY_CONTROL
	and #%00001111
	ora #VIC_SCREEN1
	sta VIC_MEMORY_CONTROL

	lda #SPRITE_LEFT
	sta SPRITE_POINTER_BASE
	sta SPRITE_POINTER_BASE1
	jsr softScrollRight
	inc VIC_BORDER_COLOR

	lda COLOR_SCROLL_PENDING
	beq .noLeft
  
	jsr doColorScrollRight
	lda #0
	sta VIC_BORDER_COLOR

.noLeft

	jmp  GameLoop         
  
;------------------------------------------------------------ 
;
; initDisplay
;
;------------------------------------------------------------

!zone initDisplay
initDisplay

	; clear screen
	jsr clearScreen

	; set character colour 
	ldy  #$00
	lda  #$01
.loopCharColour
	sta  SCREEN_COLOR+160,y   ; lines 4 to 24
	sta  SCREEN_COLOR+370,y
	sta  SCREEN_COLOR+580,y
	sta  SCREEN_COLOR+790,y

	iny
	;tya       ; increase colour
	cpy #210
	bne  .loopCharColour

	; set characters to A
	ldy  #$00
	ldx  #$40
	lda  #$00
.loopChar
	sta SCREEN_CHAR+160,y ; line 4 to 24 including
	sta SCREEN_CHAR+370,y
	sta SCREEN_CHAR+580,y
	sta SCREEN_CHAR+790,y
	; do screen1 with different character
	pha
	txa
	sta SCREEN_CHAR1+160,y
	sta SCREEN_CHAR1+370,y
    	sta SCREEN_CHAR1+580,y
	sta SCREEN_CHAR1+790,y
	pla

	iny
	tya       ; increase character
	cpy #210
	bne  .loopChar
	rts

;------------------------------------------------------------
;
;    softScrollLeft
;
;------------------------------------------------------------          
!zone softScrollLeft
softScrollLeft
	;check whether to execute the scroll
	ldx  SCROLL_DELAY
	cpx  #SCROLL_DELAY_COUNT
	beq  .doScroll
      
	inc  SCROLL_DELAY
	rts

.doScroll
    
	; reset the scroll delay to zero
	ldx #$00
	stx SCROLL_DELAY

	lda SCROLL_POS
	bne .notatzero

	lda  #$07
	sta  SCROLL_POS

	jsr hardScrollScreenLeft
	jmp .setScrollRegister

.notatzero
	; scroll left one pixel until SCROLL_POS = $FF
	dec  SCROLL_POS
	ldx  SCROLL_POS
	bpl  .setScrollRegister

.setScrollRegister
	; load the current value, clear bits #0-#2, add scroll position and write back
	lda VIC_SCREENCTRL2
	and #$F8
	clc 
	adc SCROLL_POS
	sta VIC_SCREENCTRL2
	rts

;------------------------------------------------------------
;
;    softScrollRight
;
;------------------------------------------------------------          
!zone softScrollRight
softScrollRight
	;check whether to execute the scroll
	ldx  SCROLL_DELAY
	cpx  #SCROLL_DELAY_COUNT
	beq  .doScroll
      
	inc  SCROLL_DELAY
	rts

.doScroll
    
	; reset the scroll delay to zero
	ldx #$00
	stx SCROLL_DELAY

	lda SCROLL_POS
	cmp #$07
	bcc .notatseven

	lda  #$00
	sta  SCROLL_POS

	jsr hardScrollScreenRight
	jmp .setScrollRegister

.notatseven
	inc  SCROLL_POS

.setScrollRegister
	; load the current value, clear bits #0-#2, add scroll position and write back
	lda VIC_SCREENCTRL2
	and #$F8
	clc 
	adc SCROLL_POS
	sta VIC_SCREENCTRL2
	rts

;---------------------------------------
;
;    waitFrame
;
;---------------------------------------
!zone waitFrame
	;wait for the raster to reach line $f8
	;this is keeping our timing stable
      
	;are we on line $F8 already? if so, wait for the next full screen
	;prevents mistimings if called too fast
waitFrame 
	lda $d012
	cmp #$F8
	beq waitFrame

	;wait for the raster to reach line $f8 (should be closer to the start of this line this way)
.WaitStep2
	lda $d012
	cmp #$F8
	bne .WaitStep2
      
	rts

	!src "sprites.asm"
	!src "irq.asm"
	!src "util.asm"

* = $2000
	!bin "scroll.spr"

;------------------------------------------------------------
;
;    hardScrollScreenLeft
;
;------------------------------------------------------------          
!zone hardScrollScreenLeft
hardScrollScreenLeft
	+first_to_backup_column 4, 24
	+scroll_char_ram_left 4, 14
	+scroll_char_ram_left 15, 24
	+backup_to_last_column 4, 24

	lda #$01
	sta COLOR_SCROLL_PENDING
  
	rts

!zone doColorScrollLeft
doColorScrollLeft

	+first_to_backup_column_color 4, 24
	+scroll_color_ram_left 4, 14
	+scroll_color_ram_left 15, 24
	+backup_to_last_column_color 4, 24
  
	lda #$00
	sta COLOR_SCROLL_PENDING

	rts

;------------------------------------------------------------
;
;    hardScrollScreenRight
;
;------------------------------------------------------------          
!zone hardScrollScreenRight
hardScrollScreenRight
	+last_to_backup_column 4, 24
	+scroll_char_ram_right 4, 14
	+scroll_char_ram_right 15, 24
	+backup_to_first_column 4, 24
  
	lda #$01
	sta COLOR_SCROLL_PENDING
  
	rts

!zone doColorScrollRight
doColorScrollRight
	+last_to_backup_column_color 4, 24
	+scroll_color_ram_right 4, 14
	+scroll_color_ram_right 15, 24
	+backup_to_first_column_color 4, 24
  
	lda #$00
	sta COLOR_SCROLL_PENDING

	rts

;---------------------------------------
;
; Game data goes here
;
;---------------------------------------

; are for keeping one column of screen information
BACKUP_COLUMN		!fill     25 
BACKUP_COLUMN_COLOR	!fill     25    

; the delay counter for scrolling
SCROLL_DELAY		!byte 0

; the current horizontal sroll position
SCROLL_POS		!byte     0

COLOR_SCROLL_PENDING	!byte 0
