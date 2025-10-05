!macro first_to_backup_column .startrow, .endrow {
    !for .i, .startrow, .endrow { 
        lda SCREEN_CHAR + (.i * 40)
        sta BACKUP_COLUMN + .i
    }
}

!macro first_to_backup_column_color .startrow, .endrow {
    !for .i, .startrow, .endrow { 
        lda SCREEN_COLOR + (.i * 40)
        sta BACKUP_COLUMN_COLOR + .i
    }
}

!macro backup_to_last_column .startrow, .endrow {
    !for .i, .startrow, .endrow {
    lda BACKUP_COLUMN + .i
    sta SCREEN_CHAR + (.i * 40) + 39
  }
}

!macro backup_to_last_column_color .startrow, .endrow {
    !for .i, .startrow, .endrow {
    lda BACKUP_COLUMN_COLOR + .i
    sta SCREEN_COLOR + (.i * 40) + 39
  }
}

!macro last_to_backup_column .startrow, .endrow {
    !for .i, .startrow, .endrow { 
        lda SCREEN_CHAR + (.i * 40) + 39
        sta BACKUP_COLUMN + .i
    }
}

!macro last_to_backup_column_color .startrow, .endrow {
    !for .i, .startrow, .endrow { 
        lda SCREEN_COLOR + (.i * 40) + 39
        sta BACKUP_COLUMN_COLOR + .i
    }
}

!macro backup_to_first_column .startrow, .endrow {
    !for .i, .startrow, .endrow {
    lda BACKUP_COLUMN + .i
    sta SCREEN_CHAR + (.i * 40)
  }
}

!macro backup_to_first_column_color .startrow, .endrow {
    !for .i, .startrow, .endrow {
    lda BACKUP_COLUMN_COLOR + .i
    sta SCREEN_COLOR + (.i * 40)
  }
}

!macro scroll_color_ram_left .startrow, .endrow {
  pha
  txa
  pha

  ldx #$00
.chllft

  !for .N, .startrow, .endrow {
    lda SCREEN_COLOR + (.N * 40) + 1, x
    sta SCREEN_COLOR + (.N * 40), x
  }
  
  inx
  cpx #39
  bne .chllft

  pla
  tax
  pla

  ; !for .i, .startrow, .endrow {
  ;   !for .j, 0, 38 { 
  ;     lda SCREEN_COLOR + (.i * 40) + (.j + 1)
  ;     sta SCREEN_COLOR + (.i * 40) + .j
  ;   }
  ; }
}

!macro scroll_color_ram_right .startrow, .endrow {
 pha
  txa
  pha

  ldx #38

.clrrgt
  !for .N, .startrow, .endrow {
    lda SCREEN_COLOR + (.N * 40), x 
    sta SCREEN_COLOR + (.N * 40) + 1, x 
  }

  dex
  bpl .clrrgt

  pla
  tax
  pla


  ; !for .i, .startrow, .endrow {
  ;   !for .j, 38, 0 { 
  ;     lda SCREEN_COLOR + (.i * 40) + .j
  ;     sta SCREEN_COLOR + (.i * 40) + (.j + 1)
  ;   }
  ; }
}

!macro scroll_char_ram_left .startrow, .endrow {
  pha
  txa
  pha

  ldx #$00
.chrlft

  !for .N, .startrow, .endrow {
    lda SCREEN_CHAR + (.N * 40) + 1, x
    sta SCREEN_CHAR + (.N * 40), x
  }
  
  inx
  cpx #39
  bne .chrlft

  pla
  tax
  pla

  ; !for .i, .startrow, .endrow {
  ;   !for .j, 0, 38 {
  ;     lda SCREEN_CHAR + (.i * 40) + (.j + 1)
  ;     sta SCREEN_CHAR + (.i * 40) + .j
  ;   }
  ; }
}

!macro scroll_char_ram_right .startrow, .endrow {
  pha
  txa
  pha

  ldx #38

.chrrgt
  !for .N, .startrow, .endrow {
    lda SCREEN_CHAR + (.N * 40), x 
    sta SCREEN_CHAR + (.N * 40) + 1, x 
  }

  dex
  bpl .chrrgt

  pla
  tax
  pla

  ; !for .i, .startrow, .endrow {
  ;   !for .j, 38, 0 {
  ;     lda SCREEN_CHAR + (.i * 40) + .j
  ;     sta SCREEN_CHAR + (.i * 40) + (.j + 1)
  ;   }
  ; }
}