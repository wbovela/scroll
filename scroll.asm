;compile to this filename
!to "jmain.prg",cbm

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
    jsr waitFrame
    
    ; print scroll position
    lda SCROLL_POS
    clc
    adc #48
    sta $040a
    
    ; right pressed
    lda #$08
    bit JOYSTICK_PORT_II
    bne .noRight

    jsr  softScrollLeft

.noRight

	; left pressed
    lda #$04
    bit JOYSTICK_PORT_II
    bne .noLeft

    jsr softScrollRight

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
    sta  SCREEN_COLOR+200,y
    sta  SCREEN_COLOR+360,y
    sta  SCREEN_COLOR+520,y
    sta  SCREEN_COLOR+680,y
    iny
    ;tya       ; increase colour
    cpy #160
    bne  .loopCharColour

    ; set characters to A
    ldy  #$00
    lda  #$01
.loopChar
    sta SCREEN_CHAR+200,y
    sta SCREEN_CHAR+360,y
    sta SCREEN_CHAR+520,y
    sta SCREEN_CHAR+680,y
    iny
    tya       ; increase character
    cpy #160
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

    jsr hardScrollScreenLeft

.notatzero
    ; scroll left one pixel until SCROLL_POS = $FF
    dec  SCROLL_POS
    ldx  SCROLL_POS
    bpl  .setScrollRegister

.resetPosition
    ; Switch to scroll pos 7
    lda  #$07
    sta  SCROLL_POS

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
    bne .notatseven

    jsr hardScrollScreenRight

.notatseven
    inc  SCROLL_POS
    ldx  SCROLL_POS
    cpx #$07
    bne .setScrollRegister

.resetPosition
    ; Switch to scroll pos 0
    lda  #$00
    sta  SCROLL_POS

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
;    PARAM2 = start row
;    PARAM3 = end row
;------------------------------------------------------------          
!zone hardScrollScreenLeft
hardScrollScreenLeft
    +first_to_backup_column 5, 21
    +scroll_char_ram_left 5, 21
    +backup_to_last_column 5, 21

    ;+scroll_color_ram 5,21

    rts

;------------------------------------------------------------
;
;    hardScrollScreenRight
;    PARAM2 = start row
;    PARAM3 = end row
;------------------------------------------------------------          
!zone hardScrollScreenRight
hardScrollScreenRight
    +last_to_backup_column 5, 21
    +scroll_char_ram_right 5, 21
    +backup_to_first_column 5, 21

    ;+scroll_color_ram 5,21

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


;---------------------------------------
;
; Game data goes here
;
;---------------------------------------

; are for keeping one column of screen information
BACKUP_COLUMN  !fill     25     

; the delay counter for scrolling
SCROLL_DELAY  !byte 0

; the current horizontal sroll position
SCROLL_POS     !byte     0

     
