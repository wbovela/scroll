;compile to this filename
!to "scroll.prg",cbm

;macros
!source "macros.asm"

;define constants here

;placeholder for various 8 bit temp parameters
PARAM1                  = $03
PARAM2                  = $04
PARAM3                  = $05
PARAM4                  = $06
PARAM5                  = $07

;various 16 bit temp parameters two bytes each
PARAM1_16B			= $FB	
XPOS_16B				= $FD	

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
VIC_CHARSET_MULTICOLOR_1= $d022
VIC_CHARSET_MULTICOLOR_2= $d023

NUMBER_OF_SPRITES_DIV_4	= 1

JOYSTICK_PORT_II        = $dc00
; all active low
; 0001 (1)= up
; 0010 (2)= down
; 0100 (4)= left
; 1000 (8)= right
; 0001 0000 (16) = fire

CIA_PRA                 = $dd00

;address of the screen buffers 
SCREEN_CHAR             = $CC00
;CREEN_CHAR1            = $3000

;address of color ram
SCREEN_COLOR            = $D800

; VIC_MEMORY_CONTROL $d018
VIC_SCREEN0 = %00010000 ; $0400
VIC_SCREEN1 = %11000000 ; $3000

;set number of loops to delay scrolling
SCROLL_DELAY_COUNT    = $00

;address of sprite pointers
SPRITE_POINTER_BASE   = SCREEN_CHAR + 1016

;sprite number constant
SPRITE_BASE                 = 64

SPRITE_LEFT                 = SPRITE_BASE + 0
SPRITE_RIGHT                = SPRITE_BASE + 1

;this creates a basic start
*=$0801

	;SYS 2064
	!byte $0C,$08,$0A,$00,$9E,$20,$32,$30,$36,$34,$00,$00,$00,$00,$00
	
	jsr clearScreen
	jsr setupMemory
	jsr initDisplay
	jsr initSprites
	jsr initRasterIrq

	;background black
	lda #1
	sta VIC_BACKGROUND_COLOR
	lda #0
	sta VIC_BORDER_COLOR

	;set charset multi colors
	lda #15
	sta VIC_CHARSET_MULTICOLOR_1
	lda #11
	sta VIC_CHARSET_MULTICOLOR_2

	; set 38 column mode
	;lda VIC_SCREENCTRL2
	lda #%11110111  ; 0-2 = fine scroll, 3 = 38/40 columns mode, 4 = multicolor mode, 5-7 unused
	sta VIC_SCREENCTRL2

	; set scroll delay to 0
	ldx  #$00
	stx  SCROLL_DELAY

	; start at 7 
	lda  #$00
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
	;lda #0
	;sta VIC_BORDER_COLOR

	; display scroll position
	;lda SCROLL_POS
	;clc
	;adc #48
	;sta SCREEN_CHAR+(2 * 40) + 15

	; wait for next frame  
	jsr waitFrame
	;lda #$1
	;sta VIC_BORDER_COLOR

	; right pressed
	lda #$08
	bit JOYSTICK_PORT_II
	bne .noRight

	lda #SPRITE_RIGHT
	sta SPRITE_POINTER_BASE
	jsr softScrollLeft
	;inc VIC_BORDER_COLOR

	lda COLOR_SCROLL_PENDING
	beq .noRight
    
	jsr doColorScrollLeft
	;lda #0
	;sta VIC_BORDER_COLOR
  
.noRight
	; left pressed
	lda #$04
	bit JOYSTICK_PORT_II
	bne .noLeft

	lda #SPRITE_LEFT
	sta SPRITE_POINTER_BASE
	jsr softScrollRight
	;inc VIC_BORDER_COLOR

	lda COLOR_SCROLL_PENDING
	beq .noLeft
  
	jsr doColorScrollRight
	;lda #0
	;sta VIC_BORDER_COLOR
	
.noLeft

	jmp  GameLoop         

;------------------------------------------------------------ 
;
; setupMemory
;
;------------------------------------------------------------
  
!zone setupMemory
setupMemory

	;set charset
	lda #%00111100	; char $F000, screen %CC00
	sta VIC_MEMORY_CONTROL

	;VIC bank
	lda CIA_PRA
	and #%11111100	; bank 3 $C000-$FFFF
	sta CIA_PRA

	;block interrupts 
	;since we turn ROMs off this would result in crashes if we didn't
	sei
          
	;save old configuration
	lda $01
	sta PARAM1
          
	;only RAM
	;to copy under the IO rom
	lda #%00110000
	sta $01
          
	;take source address from CHARSET
	LDA #<CHARSET_DATA
	STA ZEROPAGE_POINTER_1
	LDA #>CHARSET_DATA
	STA ZEROPAGE_POINTER_1 + 1
          
	;now copy
	jsr CopyCharSet
          
	;take source address from SPRITES
	lda #<SPRITE_DATA
	sta ZEROPAGE_POINTER_1
	lda #>SPRITE_DATA
	sta ZEROPAGE_POINTER_1 + 1
          
	jsr CopySprites
          
	;restore ROMs
	lda PARAM1
	sta $01
          
	cli
	rts
			
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
		
.loopCharColour	
	lda #$00				; lines 0 to 4 are black, no multicolor
	sta SCREEN_COLOR,y
	lda  #$08				; bit 3 makes multicolour
	sta SCREEN_COLOR+160,y   ; lines 4 to 21
	sta SCREEN_COLOR+370,y
	sta SCREEN_COLOR+580,y
	sta SCREEN_COLOR+790,y
	sta SCREEN_COLOR+880,y
	iny
	cpy #210
	bne  .loopCharColour

	ldy #$00				; char 1 on top 4 lines and bottom
	lda #$01
.loopTopLines
	sta SCREEN_CHAR,y
	iny
	cpy #200
	bne .loopTopLines
	
	ldy #$00				; char 1 on bottom 2 lines
	lda #$01
.loopBottomLines
	sta SCREEN_CHAR+880,y
	iny
	cpy #120
	bne .loopBottomLines
	
	
	; copy 40 columns and 17 lines from map coordinate x = 0
	; to the screen

	ldy #$00
	sty XPOS_16B
	sty XPOS_16B + 1
	sty PARAM1
	jsr getPointerToMapCharacter
	
.loopChar

!for .LINE, 0, 16 {
	lda MAP_DATA + 0 + 512*.LINE, y
	sta SCREEN_CHAR + (40 * (.LINE + 5)), y
}
	
	iny
	cpy #40
	bne .loopChar
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
	cmp #$FE
	beq waitFrame

	;wait for the raster to reach line $f8 (should be closer to the start of this line this way)
.WaitStep2
	lda $d012
	cmp #$FE
	bne .WaitStep2
      
	rts

!src "sprites.asm"
!src "irq.asm"
!src "util.asm"

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
BACKUP_COLUMN				!fill     25 
BACKUP_COLUMN_COLOR			!fill     25    

; the delay counter for scrolling
SCROLL_DELAY				!byte 0

; the current horizontal sroll position
SCROLL_POS				!byte     0

COLOR_SCROLL_PENDING		!byte 0

MAPTABLELOW
!for i,0,16 {
	!byte <(MAP_DATA + i * MAP_WIDTH)
}

MAPTABLEHIGH
!for i,0,16 {
	!byte >(MAP_DATA + i * MAP_WIDTH)
}


;---------------------------------------
;
; Char set and game map
;
;---------------------------------------

!src "UridiumL01Zinc.asm"

;---------------------------------------
;
; sprite data
;
;---------------------------------------

SPRITE_DATA
!bin "scroll.spr"


	