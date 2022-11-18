(setq ifdef_settings_functions t)




(defun SETTINGS:read_settings (folder_path / setting_fd line settings temp_list) 
  (setq settings_fd (open (strcat folder_path "\\Settings.txt") "r"))
  (setq settings (SETTINGS:parse_settings_file settings_fd))
  (close settings_fd)
  settings
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
          (setq settings (append settings (list (recursiveList settings_fd))))
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

(defun SETTINGS:save_settings (settings / setting_fd line settings) 

  (print settings)
  (foreach n settings 
    
    (print n)
  )
  ;| (setq settings_fd (open (strcat folder_path "\\Settings.txt") "w"))
  (setq  settings (recursiveList settings_fd))
  (close settings_fd) |;
)

(defun SETTINGS:dialog (settings / dcl_id1) 
  (setq dcl_id1 (load_dialog "AITWindow.dcl"))
  (if (not (new_dialog "settings_Dialog" dcl_id1))  ; Initialize the dialog.
    (exit) ; Exit if this does not work.
  )
  (princ "\nsettings\n")
  (action_tile "settings_save_Button" "(SETTINGS:save_settings settings)")
  (start_dialog)
  (unload_dialog dcl_id1)
  (princ)
)
