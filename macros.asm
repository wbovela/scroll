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
  !for .i, .startrow, .endrow {
    !for .j, 0, 38 { 
      lda SCREEN_COLOR + (.i * 40) + (.j + 1)
      sta SCREEN_COLOR + (.i * 40) + .j
    }
  }
}

!macro scroll_color_ram_right .startrow, .endrow {
  !for .i, .startrow, .endrow {
    !for .j, 38, 0 { 
      lda SCREEN_COLOR + (.i * 40) + .j
      sta SCREEN_COLOR + (.i * 40) + (.j + 1)
    }
  }
}

!macro scroll_char_ram_left .startrow, .endrow {
  !for .i, .startrow, .endrow {
    !for .j, 0, 38 {
      lda SCREEN_CHAR + (.i * 40) + (.j + 1)
      sta SCREEN_CHAR + (.i * 40) + .j
    }
  }
}

!macro scroll_char_ram_right .startrow, .endrow {
  !for .i, .startrow, .endrow {
    !for .j, 38, 0 {
      lda SCREEN_CHAR + (.i * 40) + .j
      sta SCREEN_CHAR + (.i * 40) + (.j + 1)
    }
  }
}