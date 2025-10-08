
; -------------------------------------------------------
; Setup code for sprites
; -------------------------------------------------------

!zone initSprites
initSprites

	lda #%00000001  ; enable sprite 0 and 1
	sta VIC_SPRITE_ENABLE     ; enable sprite 0
	sta VIC_SPRITE_DBL_HEIGHT
	sta VIC_SPRITE_DBL_WIDTH
	lda #$00        
	sta VIC_SPRITE_PRIORITY
 
	lda #150
	sta VIC_SPRITE_X_POS      ; sprite 0 X
	lda #150
	sta VIC_SPRITE_Y_POS      ; sprite 0 Y

	lda #2
	sta VIC_SPRITE_COLOR      ; sprite 0 color = white

; -------------------------------------------------------
; Sprite pointers
; -------------------------------------------------------

	lda #SPRITE_LEFT
	sta SPRITE_POINTER_BASE		; pointer for sprite 0
	sta SPRITE_POINTER_BASE1		; pointer for sprite 
	rts

