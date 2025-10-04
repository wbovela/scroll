;compile to this filename
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

VIC_SCREENCTRL2         = $d016
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

;address of the screen buffer
SCREEN_CHAR             = $0400

;address of color ram
SCREEN_COLOR            = $D800

;set number of loops to delay scrolling
SCROLL_DELAY_COUNT    = $00

;this creates a basic start
*=$0801

    ;SYS 2064
    !byte $0C,$08,$0A,$00,$9E,$20,$32,$30,$36,$34,$00,$00,$00,$00,$00

    jsr initDisplay
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

    lda SCROLL_POS
    clc
    adc #48
    sta $0400 + (2 * 40) + 15

    ; wait for next frame  
	jsr waitFrame

	; right pressed
	lda #$08
	bit JOYSTICK_PORT_II
	bne .noRight

	jsr  softScrollLeft

	lda COLOR_SCROLL_PENDING
	beq .noRight
	
	jsr doColorScrollLeft
	
.noRight
	; left pressed
	lda #$04
	bit JOYSTICK_PORT_II
	bne .noLeft

	jsr softScrollRight
	
	lda COLOR_SCROLL_PENDING
	beq .noLeft
	
	jsr doColorScrollRight
	
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
	jsr $e544

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
	lda  #$01
.loopChar
	sta SCREEN_CHAR+160,y ; line 4 to 24 including
	sta SCREEN_CHAR+370,y
	sta SCREEN_CHAR+580,y
	sta SCREEN_CHAR+790,y
	
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

;------------------------------------------------------------
;
;    hardScrollScreenLeft
;
;------------------------------------------------------------          
!zone hardScrollScreenLeft
hardScrollScreenLeft
	+first_to_backup_column 4, 24
	+scroll_char_ram_left 4, 24
	+backup_to_last_column 4, 24

	lda #$01
	sta COLOR_SCROLL_PENDING
	
	rts

!zone doColorScrollLeft
doColorScrollLeft

	+first_to_backup_column_color 4, 24
	+scroll_color_ram_left 4, 24
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
	+scroll_char_ram_right 4, 24
	+backup_to_first_column 4, 24
	
	lda #$01
	sta COLOR_SCROLL_PENDING
	
    rts

!zone doColorScrollRight
doColorScrollRight
	+last_to_backup_column_color 4, 24
	+scroll_color_ram_right 4, 24
	+backup_to_first_column_color 4, 24
	
	lda #$00
	sta COLOR_SCROLL_PENDING

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

!src "irq.asm"

;---------------------------------------
;
; Game data goes here
;
;---------------------------------------

; are for keeping one column of screen information
BACKUP_COLUMN		!fill     25 
BACKUP_COLUMN_COLOR	!fill     25    

; the delay counter for scrolling
SCROLL_DELAY  !byte 0

; the current horizontal sroll position
SCROLL_POS     !byte     0

COLOR_SCROLL_PENDING	!byte	0

     
