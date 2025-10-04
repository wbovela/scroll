!zone initRasterIrq
initRasterIrq

    sei 

    lda #$7f
    sta $dc0d       ; disable all CIA interrupts
    sta $dd0d

    lda #$01
    sta $d01a        ; enable raster IRQ

    lda #48
    sta $d012       ; set raster line to 16

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

    ; lda #$6
    ; sta $d020

    lda $d019
    sta $d019       ; acknowledge the IRQ

    lda #<irqHandlerBottom
    sta $0314
    lda #>irqHandlerBottom
    sta $0315

    lda #80
    sta $d012      ; set raster line to 80

    ; force fine scroll = 0 for top rows
    lda VIC_SCREENCTRL2
    and #$F8        ; clear scroll bits
    sta VIC_SCREENCTRL2

    jmp $ea31       ; jump to the kernal IRQ handler

!zone irqHandlerBottom
irqHandlerBottom

    ; lda #$2
    ; sta $d020
 
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

    jmp $ea31