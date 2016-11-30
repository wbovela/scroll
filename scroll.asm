;compile to this filename
!to "jmain.prg",cbm

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

VIC_SCREENCTRL2          = $d016
VIC_BORDER_COLOR        = $d020
VIC_BACKGROUND_COLOR    = $d021

CIA_PRA                 = $dd00

;address of the screen buffer
SCREEN_CHAR             = $0400

;address of color ram
SCREEN_COLOR            = $D800

;address of the screen backup memory
SCREEN_BACK_CHAR        = $2000

;address of the screen backbuffer
SCREEN_BACK_COLOR       = $C800

;this creates a basic start
*=$0801

          ;SYS 2064
          !byte $0C,$08,$0A,$00,$9E,$20,$32,$30,$36,$34,$00,$00,$00,$00,$00

          ldy  #$00
          lda  #$01
.loopWhite
          sta  SCREEN_COLOR,y
          sta  SCREEN_COLOR+250,y
          sta  SCREEN_COLOR+500,y
          sta  SCREEN_COLOR+750,y
          iny
          bne  .loopWhite

          ;background black
          lda #0
          sta VIC_BACKGROUND_COLOR
          sta VIC_BORDER_COLOR

          ; set 38 column mode
          lda VIC_SCREENCTRL2
          and #%11110111
          sta VIC_SCREENCTRL2

          ; start at 7 for left scrolling
          lda  #$07
          sta  SCROLL_POS

          ; set scroll position to 7
          lda VIC_SCREENCTRL2
          and #$F8
          clc 
          adc SCROLL_POS
          sta VIC_SCREENCTRL2   

          ; set base screen
          lda  #$00
          sta  ACTIVE_SCREEN 

          ; start off by making a copy of the current screen to the backup
          jsr  copyBaseToBackup

;------------------------------------------------------------
;
;    GameLoop
;
;------------------------------------------------------------

GameLoop  
          jsr waitFrame

          ;lda #1
          ;sta VIC_BORDER_COLOR

          jsr  softScrollLeft
          ;jsr hardScrollScreen

          ;lda #0
          ;sta VIC_BORDER_COLOR

          jmp GameLoop         
    
;------------------------------------------------------------
;
;    softScrollLeft
;
;------------------------------------------------------------          
!zone softScrollLeft
softScrollLeft

          ldx  SCROLL_DELAY
          cpx  #$05
          beq  .doScroll
          
          inc  SCROLL_DELAY
          rts

.doScroll
          ldx  #$00
          stx  SCROLL_DELAY

          dec  SCROLL_POS
          ldx  SCROLL_POS
          bpl  .setScrollRegister

.resetPosition
          ; set zeropage pointer 3 to base screen line offset low table
          lda  #<SCREEN_LINE_OFFSET_TABLE_LO
          sta  ZEROPAGE_POINTER_3
          lda  #>SCREEN_LINE_OFFSET_TABLE_LO
          sta  ZEROPAGE_POINTER_3+1

          ; set zeropage pointer 4 to base screen line offset high table
          lda  #<SCREEN_LINE_OFFSET_TABLE_HI
          sta  ZEROPAGE_POINTER_4
          lda  #>SCREEN_LINE_OFFSET_TABLE_HI
          sta  ZEROPAGE_POINTER_4+1

          ; now call the hard scroll routine 
          jsr  hardScrollScreen

          ; reset the scroll position to 7
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
;    hardScrollScreen
;    ZEROPAGE_POINTER_3 = address of line offset table low 
;    ZEROPAGE_POINTER_4 = address of line offset table high
;------------------------------------------------------------          
!zone hardScrollScreen
hardScrollScreen
		
          ; copy the first column to backup table
          ldy  #$00
		
          ; take address of backup column
          lda  #<BACKUP_COLUMN
          sta  ZEROPAGE_POINTER_2
          lda  #>BACKUP_COLUMN
          sta  ZEROPAGE_POINTER_2+1		
.loop
          ; take address of first character on line y
          lda  (ZEROPAGE_POINTER_3),y
          sta  ZEROPAGE_POINTER_1
          lda  (ZEROPAGE_POINTER_4),y
          sta  ZEROPAGE_POINTER_1+1

          ; take 1st character on line y and store in table at position y
          sty  PARAM1
          ldy  #$00
          lda  (ZEROPAGE_POINTER_1),y
          ldy  PARAM1
          sta  (ZEROPAGE_POINTER_2),y

          ; do this for lines 0-24
          iny
          cpy  #24
          bne  .loop

          ; now we shift all columns on each row left by one character
          ldy  #$00

.nextRow
          ; take address of first character on line y
          lda  (ZEROPAGE_POINTER_3),y
          sta  ZEROPAGE_POINTER_1
          lda  (ZEROPAGE_POINTER_4),y
          sta  ZEROPAGE_POINTER_1+1          

          sty  PARAM1

          ; y=column 0
          ldy  #$00

.nextColumn
          ; take the character from column y+1 and store in column y
          iny
          lda  (ZEROPAGE_POINTER_1),y
          dey
          sta  (ZEROPAGE_POINTER_1),y

          ; next column
          iny

          ; stop at column 40
          cpy  #39
          bne  .nextColumn

          ; now do the next row, so get the row number from PARAM1 and increase it
          ldy  PARAM1
          iny

          ; stop after 25 rows
          cpy  #24
          bne  .nextRow

.rowsDone

          ; copy the backup column to the last column
          ldy  #$00
.loopLastColumn
          ; take address of first character on line y
          lda  (ZEROPAGE_POINTER_3),y
          sta  ZEROPAGE_POINTER_1
          lda  (ZEROPAGE_POINTER_4),y
          sta  ZEROPAGE_POINTER_1+1

          ; take address of backup column
          lda  #<BACKUP_COLUMN
          sta  ZEROPAGE_POINTER_2
          lda  #>BACKUP_COLUMN
          sta  ZEROPAGE_POINTER_2+1

          ; take 1st character on line y and store in table at position 39
          lda  (ZEROPAGE_POINTER_2),y

          sty  PARAM1
          ldy  #39
          sta  (ZEROPAGE_POINTER_1),y
          ldy  PARAM1

          ; do this for lines 0-24
          iny
          cpy  #24
          bne  .loopLastColumn

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
;    copyBaseToBackup
;
;---------------------------------------
!zone copyBaseToBackup
copyBaseToBackup
          ldy  #$00
.loop
          lda  SCREEN_CHAR,y
          sta  SCREEN_BACK_CHAR,y
          lda  SCREEN_CHAR+250,y
          sta  SCREEN_BACK_CHAR+250,y
          lda  SCREEN_CHAR+500,y
          sta  SCREEN_BACK_CHAR+500,y
          lda  SCREEN_CHAR+750,y
          sta  SCREEN_BACK_CHAR+750,y
          iny
          cpy  #250
          bne  .loop

          rts

;---------------------------------------
;    copyBackupToBase
;---------------------------------------
!zone copyBackupToBase
copyBackupToBase
          ldy  #$00
.loop
          lda  SCREEN_BACK_CHAR,y
          sta  SCREEN_CHAR,y
          lda  SCREEN_BACK_CHAR+250,y
          sta  SCREEN_CHAR+250,y
          lda  SCREEN_BACK_CHAR+500,y
          sta  SCREEN_CHAR+500,y
          lda  SCREEN_BACK_CHAR+750,y
          sta  SCREEN_CHAR+750,y
          iny
          cpy  #250
          bne  .loop

          rts

;---------------------------------------
;
;    SetVideoRamToBase
;    $0400
;---------------------------------------
!zone SetVideoRamToBase
SetVideoRamToBase
          lda $d018
          and #$0f
          ora #$10
          sta $d018
          rts

;---------------------------------------
;
;    SetVideoRamToBackup
;    $1000
;---------------------------------------
!zone SetVideoRamToBackup
SetVideoRamToBackup
          lda $d018
          and #$0f
          ora #$80
          sta $d018
          rts

;---------------------------------------
;
;	Game data goes here
;
;---------------------------------------

; are for keeping one column of screen information
BACKUP_COLUMN  !fill     25     

; the delay counter for scrolling
SCROLL_DELAY	!byte	0

; the current horizontal sroll position
SCROLL_POS     !byte     0

; indicated active screen 0=base, 1=backup
ACTIVE_SCREEN  !byte     0
         
; tables of address of first character on each line of base and backup screens (low and high parts)
SCREEN_LINE_OFFSET_TABLE_LO
          !byte ( SCREEN_CHAR +   0 ) & 0x00ff
          !byte ( SCREEN_CHAR +  40 ) & 0x00ff
          !byte ( SCREEN_CHAR +  80 ) & 0x00ff
          !byte ( SCREEN_CHAR + 120 ) & 0x00ff
          !byte ( SCREEN_CHAR + 160 ) & 0x00ff
          !byte ( SCREEN_CHAR + 200 ) & 0x00ff
          !byte ( SCREEN_CHAR + 240 ) & 0x00ff
          !byte ( SCREEN_CHAR + 280 ) & 0x00ff
          !byte ( SCREEN_CHAR + 320 ) & 0x00ff
          !byte ( SCREEN_CHAR + 360 ) & 0x00ff
          !byte ( SCREEN_CHAR + 400 ) & 0x00ff
          !byte ( SCREEN_CHAR + 440 ) & 0x00ff
          !byte ( SCREEN_CHAR + 480 ) & 0x00ff
          !byte ( SCREEN_CHAR + 520 ) & 0x00ff
          !byte ( SCREEN_CHAR + 560 ) & 0x00ff
          !byte ( SCREEN_CHAR + 600 ) & 0x00ff
          !byte ( SCREEN_CHAR + 640 ) & 0x00ff
          !byte ( SCREEN_CHAR + 680 ) & 0x00ff
          !byte ( SCREEN_CHAR + 720 ) & 0x00ff
          !byte ( SCREEN_CHAR + 760 ) & 0x00ff
          !byte ( SCREEN_CHAR + 800 ) & 0x00ff
          !byte ( SCREEN_CHAR + 840 ) & 0x00ff
          !byte ( SCREEN_CHAR + 880 ) & 0x00ff
          !byte ( SCREEN_CHAR + 920 ) & 0x00ff
          !byte ( SCREEN_CHAR + 960 ) & 0x00ff
          
SCREEN_LINE_OFFSET_TABLE_HI
          !byte ( ( SCREEN_CHAR +   0 ) & 0xff00 ) >> 8
          !byte ( ( SCREEN_CHAR +  40 ) & 0xff00 ) >> 8
          !byte ( ( SCREEN_CHAR +  80 ) & 0xff00 ) >> 8
          !byte ( ( SCREEN_CHAR + 120 ) & 0xff00 ) >> 8
          !byte ( ( SCREEN_CHAR + 160 ) & 0xff00 ) >> 8
          !byte ( ( SCREEN_CHAR + 200 ) & 0xff00 ) >> 8
          !byte ( ( SCREEN_CHAR + 240 ) & 0xff00 ) >> 8
          !byte ( ( SCREEN_CHAR + 280 ) & 0xff00 ) >> 8
          !byte ( ( SCREEN_CHAR + 320 ) & 0xff00 ) >> 8
          !byte ( ( SCREEN_CHAR + 360 ) & 0xff00 ) >> 8
          !byte ( ( SCREEN_CHAR + 400 ) & 0xff00 ) >> 8
          !byte ( ( SCREEN_CHAR + 440 ) & 0xff00 ) >> 8
          !byte ( ( SCREEN_CHAR + 480 ) & 0xff00 ) >> 8
          !byte ( ( SCREEN_CHAR + 520 ) & 0xff00 ) >> 8
          !byte ( ( SCREEN_CHAR + 560 ) & 0xff00 ) >> 8
          !byte ( ( SCREEN_CHAR + 600 ) & 0xff00 ) >> 8
          !byte ( ( SCREEN_CHAR + 640 ) & 0xff00 ) >> 8
          !byte ( ( SCREEN_CHAR + 680 ) & 0xff00 ) >> 8
          !byte ( ( SCREEN_CHAR + 720 ) & 0xff00 ) >> 8
          !byte ( ( SCREEN_CHAR + 760 ) & 0xff00 ) >> 8
          !byte ( ( SCREEN_CHAR + 800 ) & 0xff00 ) >> 8
          !byte ( ( SCREEN_CHAR + 840 ) & 0xff00 ) >> 8
          !byte ( ( SCREEN_CHAR + 880 ) & 0xff00 ) >> 8
          !byte ( ( SCREEN_CHAR + 920 ) & 0xff00 ) >> 8
          !byte ( ( SCREEN_CHAR + 960 ) & 0xff00 ) >> 8
 
SCREEN_BACK_LINE_OFFSET_TABLE_LO
          !byte ( SCREEN_BACK_CHAR +   0 ) & 0x00ff
          !byte ( SCREEN_BACK_CHAR +  40 ) & 0x00ff
          !byte ( SCREEN_BACK_CHAR +  80 ) & 0x00ff
          !byte ( SCREEN_BACK_CHAR + 120 ) & 0x00ff
          !byte ( SCREEN_BACK_CHAR + 160 ) & 0x00ff
          !byte ( SCREEN_BACK_CHAR + 200 ) & 0x00ff
          !byte ( SCREEN_BACK_CHAR + 240 ) & 0x00ff
          !byte ( SCREEN_BACK_CHAR + 280 ) & 0x00ff
          !byte ( SCREEN_BACK_CHAR + 320 ) & 0x00ff
          !byte ( SCREEN_BACK_CHAR + 360 ) & 0x00ff
          !byte ( SCREEN_BACK_CHAR + 400 ) & 0x00ff
          !byte ( SCREEN_BACK_CHAR + 440 ) & 0x00ff
          !byte ( SCREEN_BACK_CHAR + 480 ) & 0x00ff
          !byte ( SCREEN_BACK_CHAR + 520 ) & 0x00ff
          !byte ( SCREEN_BACK_CHAR + 560 ) & 0x00ff
          !byte ( SCREEN_BACK_CHAR + 600 ) & 0x00ff
          !byte ( SCREEN_BACK_CHAR + 640 ) & 0x00ff
          !byte ( SCREEN_BACK_CHAR + 680 ) & 0x00ff
          !byte ( SCREEN_BACK_CHAR + 720 ) & 0x00ff
          !byte ( SCREEN_BACK_CHAR + 760 ) & 0x00ff
          !byte ( SCREEN_BACK_CHAR + 800 ) & 0x00ff
          !byte ( SCREEN_BACK_CHAR + 840 ) & 0x00ff
          !byte ( SCREEN_BACK_CHAR + 880 ) & 0x00ff
          !byte ( SCREEN_BACK_CHAR + 920 ) & 0x00ff
          !byte ( SCREEN_BACK_CHAR + 960 ) & 0x00ff

SCREEN_BACK_LINE_OFFSET_TABLE_HI
          !byte ( ( SCREEN_BACK_CHAR +   0 ) & 0xff00 ) >> 8
          !byte ( ( SCREEN_BACK_CHAR +  40 ) & 0xff00 ) >> 8
          !byte ( ( SCREEN_BACK_CHAR +  80 ) & 0xff00 ) >> 8
          !byte ( ( SCREEN_BACK_CHAR + 120 ) & 0xff00 ) >> 8
          !byte ( ( SCREEN_BACK_CHAR + 160 ) & 0xff00 ) >> 8
          !byte ( ( SCREEN_BACK_CHAR + 200 ) & 0xff00 ) >> 8
          !byte ( ( SCREEN_BACK_CHAR + 240 ) & 0xff00 ) >> 8
          !byte ( ( SCREEN_BACK_CHAR + 280 ) & 0xff00 ) >> 8
          !byte ( ( SCREEN_BACK_CHAR + 320 ) & 0xff00 ) >> 8
          !byte ( ( SCREEN_BACK_CHAR + 360 ) & 0xff00 ) >> 8
          !byte ( ( SCREEN_BACK_CHAR + 400 ) & 0xff00 ) >> 8
          !byte ( ( SCREEN_BACK_CHAR + 440 ) & 0xff00 ) >> 8
          !byte ( ( SCREEN_BACK_CHAR + 480 ) & 0xff00 ) >> 8
          !byte ( ( SCREEN_BACK_CHAR + 520 ) & 0xff00 ) >> 8
          !byte ( ( SCREEN_BACK_CHAR + 560 ) & 0xff00 ) >> 8
          !byte ( ( SCREEN_BACK_CHAR + 600 ) & 0xff00 ) >> 8
          !byte ( ( SCREEN_BACK_CHAR + 640 ) & 0xff00 ) >> 8
          !byte ( ( SCREEN_BACK_CHAR + 680 ) & 0xff00 ) >> 8
          !byte ( ( SCREEN_BACK_CHAR + 720 ) & 0xff00 ) >> 8
          !byte ( ( SCREEN_BACK_CHAR + 760 ) & 0xff00 ) >> 8
          !byte ( ( SCREEN_BACK_CHAR + 800 ) & 0xff00 ) >> 8
          !byte ( ( SCREEN_BACK_CHAR + 840 ) & 0xff00 ) >> 8
          !byte ( ( SCREEN_BACK_CHAR + 880 ) & 0xff00 ) >> 8
          !byte ( ( SCREEN_BACK_CHAR + 920 ) & 0xff00 ) >> 8
          !byte ( ( SCREEN_BACK_CHAR + 960 ) & 0xff00 ) >> 8


