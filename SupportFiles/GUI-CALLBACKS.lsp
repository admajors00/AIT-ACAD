(setq ifdef_gui_callbacks t)
(defun LM:DirectoryDialog (msg dir flag / Shell Fold Self Path) 
  (vl-catch-all-apply 
    (function 
      (lambda (/ ac HWND) 
        (if 
          (setq Shell (vla-getInterfaceObject (setq ac (vlax-get-acad-object)) 
                                              "Shell.Application"
                      )
                HWND  (vl-catch-all-apply 'vla-get-HWND (list ac))
                Fold  (vlax-invoke-method Shell 
                                          'BrowseForFolder
                                          (if (vl-catch-all-error-p HWND) 0 HWND)
                                          msg
                                          flag
                                          dir
                      )
          )
          (setq Self (vlax-get-property Fold 'Self)
                Path (vlax-get-property Self 'Path)
                Path (vl-string-right-trim "\\" 
                                           (vl-string-translate "/" "\\" Path)
                     )
          )
        )
      )
    )
  )
  (if Self (vlax-release-object Self))
  (if Fold (vlax-release-object Fold))
  (if Shell (vlax-release-object Shell))
  Path
)


(defun AIT:set_tool_folder_path_cb (/ project_folders) 
  (setq project_folder_path (get_tile "path_editBox"))
  (setq project_folders (vl-directory-files project_folder_path nil -1))
  (if 
    (and (member "SupportFiles" project_folders) 
         (member "ExternalTools" project_folders)
    )
    (progn 
      (setq tools_folder_path (strcat project_folder_path "\\ExternalTools"))
      (setq support_folder_path (strcat project_folder_path "\\SupportFiles"))
      (setq tools_file_names (vl-directory-files tools_folder_path nil 1))
      (start_list "tool_popupList" 3 0)
      (mapcar 'add_list tools_file_names)
      (end_list)

      (AIT:readSettings support_folder_path)
    )
    (progn 
      (set_tile "error" "Invalid Path")
    )
  )
)
(defun AIT:readSettings (folder_path / setting_fd line settings temp_list) 
  (setq settings_fd (open (strcat folder_path "\\Settings.txt") "r"))
  (princ (recursiveList settings_fd))
  (close settings_fd)
)
(defun recursiveList (settings_fd / settings temp_list line is_done returned) 
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
              (setq settings  temp_list)
              (setq temp_list nil)
            )
          )
          (setq is_done nil)
        )
      )
    )
    ;(setq line (read-line settings_fd)) ;read a line
  )
  (princ "\n")
  (princ settings)
  settings
)
(defun AIT:checkEntities (/) 
  (setq entl (entnext))
  (setq ent entl)
  (while (/= nil ent) 
    (setq ent_da (entget ent))
    (setq ent (entnext entl))
    (setq entl ent)
  )
)
(defun AIT:BFind (/ bet) 
  (setq bet 0)
)
(defun AIT:callback (/) 

  (setq number (+ number 1))

  (if (= number 2) 
    (alert "i mean it")
  )
  (if (= number 3) 
    (alert "dude chill")
  )
  (if (= number 4) 
    (progn 
      (alert "thats it")
      (AIT:crash)
    )
  )
)
(defun AIT:crash (/) 
  (princ "get got ")
  (AIT:crash)
)
(defun AIT:triCircle (iterations p1 p2 p3 / points x1 y1 x2 y2 x3 y3 A B C D Yc Xc R) 
  (setq points (list p1 p2 p3))
  (entmakex 
    (list (cons 0 "LINE") 
          (cons 10 (nth 2 points))
          (cons 11 (nth 1 points))
    )
  )
  (entmakex 
    (list (cons 0 "LINE") 
          (cons 10 (nth 1 points))
          (cons 11 (nth 0 points))
    )
  )
  (setq x1 (nth 0 (nth 0 points)))

  (AIT:triCircleCalc iterations)
)
(defun AIT:triCircleCalc (iterations / newPointX newPointY mag temp) 
  ;((P1x, P1y), (P2x, P2y), (P3x, P3y))
  ;(P3 P2 P1) or (Pn-1, Pn-2, Pn-3)
  (princ "\n iterations: ")
  (princ (itoa iterations))
  (princ "\n")


  (if (> iterations 0) 
    (progn 

      (setq iterations (- iterations 1))
      (Setq newPointX (+ 
                        (* 0.5 
                           (- (nth 0 (nth 1 points)) (nth 0 (nth 2 points)))
                        )
                        (nth 0 (nth 2 points))
                      )
      )
      (Setq newPointY (+ 
                        (* 0.5 
                           (- (nth 1 (nth 1 points)) (nth 1 (nth 2 points)))
                        )
                        (nth 1 (nth 2 points))
                      )
      )



      (setq points (cons (list newPointX newPointY 0.0) points))
      (setq points (reverse (cdr (reverse points))))

      ;(Line '(nth 0 points) '(nth 1 points))
      (entmakex 
        (list (cons 0 "LINE") 
              (cons 10 (nth 1 points))
              (cons 11 (nth 0 points))
        )
      )


      (AIT:triCircle iterations (nth 0 points) (nth 1 points) (nth 2 points))
    )
  )
  (nth 0 points)
)