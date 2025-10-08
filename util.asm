;------------------------------------------------------------
;
;    clear screen $0400 and $3000
;
;------------------------------------------------------------
!zone clearScreen
clearScreen
	ldx #$00
	lda #$20
.loop
	sta SCREEN_CHAR, x
	sta SCREEN_CHAR+250, x
	sta SCREEN_CHAR+500, x
	sta SCREEN_CHAR+750, x
	
	sta SCREEN_CHAR1, x
	sta SCREEN_CHAR1+250, x
	sta SCREEN_CHAR1+500, x
	sta SCREEN_CHAR1+750, x
	
	inx
	bne .loop
	rts
	