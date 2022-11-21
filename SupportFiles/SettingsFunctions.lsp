(setq ifdef_settings_functions t)




(defun SETTINGS:read_settings (/ settings_fd line settings temp_list) 
  (if (setq settings_fd (open (findfile "Settings.txt") "r")) 
    (progn 
      (setq settings (SETTINGS:parse_settings_file settings_fd))
      (close settings_fd)
      settings
    )
    (progn 
      (alert "settings file not found")
      nil
    )
  )
)

(defun SETTINGS:parse_settings_file (settings_fd / settings temp_list line is_done 
                                     returned
                                    ) 
  (setq is_done t)
  (setq returned t)
  (while (and (= is_done t) (setq line (read-line settings_fd)))  ;while not at eof
    (if line 
      (if (= line "<")  ;if line is start of a group then add all items to list
        (progn 
          (setq settings (append settings 
                                 (list (SETTINGS:parse_settings_file settings_fd))
                         )
          )
        )
        (progn 

          (while (/= line ">") 
            (setq temp_list (append temp_list (list line)))
            (setq line (read-line settings_fd))
          )
          (if temp_list 
            (progn 
              (setq settings temp_list)
              (setq temp_list nil)
            )
          )
          (setq is_done nil)
        )
      )
    )
    ;(setq line (read-line settings_fd)) ;read a line
  )

  settings
)

(defun SETTINGS:write_settings (settings new_setting /) 
  (if (assoc (car new_setting) settings)  ;check if this setting exists already
    (progn 
      (if (member new_setting settings)  ;make sure were not setting it to the same value again
        settings
        (setq settings (subst new_setting 
                              (assoc (car new_setting) settings)
                              settings
                       )
        )
      )
    )
  )
)

(defun SETTINGS:save_settings (settings / setting_fd line settings) 
  (if (setq settings_fd (open (findfile "Settings.txt") "w")) 
    (progn 

      (SETTINGS:recursive_save_settings settings settings_fd)
      (close settings_fd)
      (print "settings Saved")
    )
    (print "could not open save file")
  )
)

(defun SETTINGS:recursive_save_settings (settings settings_fd) 
  (write-line "<" settings_fd)
  (foreach element settings 
    (if (listp element) 
      (SETTINGS:recursive_save_settings element settings_fd)
      (write-line element settings_fd)
    )
  )
  (write-line ">" settings_fd)
)


(defun SETTINGS:dialog (settings / dcl_id1) 
  (setq dcl_id1 (load_dialog "AITWindow.dcl"))
  (if (not (new_dialog "settings_Dialog" dcl_id1))  ; Initialize the dialog.
    (exit) ; Exit if this does not work.
  )
  (action_tile "settings_save_Button" "(SETTINGS:save_settings settings)")
  (start_dialog)
  (unload_dialog dcl_id1)
  (princ)
)
