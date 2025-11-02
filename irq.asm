!zone initRasterIrq
initRasterIrq

	sei 

	lda #$7f
	sta $dc0d       ; disable all CIA interrupts
	sta $dd0d

	lda #$01
	sta $d01a        ; enable raster IRQ

	lda #48
	sta $d012       ; set raster line to 48

	lda #$1b
	sta $d011       ; clear MSB of raster line 

	lda #<irqHandlerTop
	sta $0314
	lda #>irqHandlerTop
	sta $0315

	lda $dc0d       ; acknowledge any pending CIA interrupts
	lda $dd0d

	lda #$07
	sta SCROLL_POS

	cli
	rts

!zone irqHandlerTop
irqHandlerTop

	lda $d019
	sta $d019       ; acknowledge the IRQ

	lda #<irqHandlerBottom
	sta $0314
	lda #>irqHandlerBottom
	sta $0315

	lda #89		; line 5 is raster lines 83..90 
	sta $d012      ; set raster line to 80

	; force fine scroll = 0 for top rows
	lda VIC_SCREENCTRL2
	and #$F8        ; clear scroll bits
	sta VIC_SCREENCTRL2
	
	; set vic to default memory ($0400 and $1000)
	; and bank 0
	lda #21
	sta VIC_MEMORY_CONTROL
	lda #151
	sta CIA_PRA
	
	lda VIC_SCREENCTRL2	; set multicolour off
	and #%11101111
	sta VIC_SCREENCTRL2
	
	lda #0
	sta VIC_BACKGROUND_COLOR

	jmp $ea31       ; jump to the kernal IRQ handler

!zone irqHandlerBottom
irqHandlerBottom

	lda $d019
	sta $d019       ; acknowledge the IRQ
    
	lda #<irqHandlerTop
	sta $0314
	lda #>irqHandlerTop
	sta $0315

	lda #48
	sta $d012      ; set raster line to 16
    
	; set scroll position to SCROLL_POS
	lda VIC_SCREENCTRL2
	and #$F8 
	ora SCROLL_POS
	sta VIC_SCREENCTRL2   

	;set charset
	lda #%00111100	; char $F000, screen %CC00
	sta VIC_MEMORY_CONTROL

	;VIC bank
	lda CIA_PRA
	and #%11111100	; bank 3 $C000-$FFFF
	sta CIA_PRA
	
	lda VIC_SCREENCTRL2	; set multicolour on
	ora #%00010000
	sta VIC_SCREENCTRL2
	
	lda #1
	sta VIC_BACKGROUND_COLOR
	
	jmp $ea31