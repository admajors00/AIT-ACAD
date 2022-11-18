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


(defun AIT:set_tool_folder_path_cb (/ project_folders settings tools_folder_path) 
  (setq project_folder_path (get_tile "path_EditBox"))
  (setq project_folders (vl-directory-files project_folder_path nil -1))
  (if 
    (and (member "SupportFiles" project_folders) 
         (member "ExternalTools" project_folders)
    )
    (progn 
      (setq tools_folder_path (strcat project_folder_path "\\ExternalTools"))
      (setq support_folder_path (strcat project_folder_path "\\SupportFiles"))
      (setq tools_file_names (vl-directory-files tools_folder_path nil 1))
      (start_list "tool_PopUpList" 3 0)
      (mapcar 'add_list tools_file_names)
      (end_list)

      (setq settings (SETTINGS:read_settings support_folder_path))
    )
    (progn 
      (set_tile "error" "Invalid Path")
    )
  )
  (append settings (list tools_folder_path) (list support_folder_path) (list tools_file_names))
)

(defun AIT:path_enter_Button_cb () 
  (setq results (AIT:set_tool_folder_path_cb))
  (setq settings (car results))
  (setq tools_folder_path (nth 1 results))
  (setq support_folder_path (nth 2 results))
  (setq tools_file_names (nth 3 results))
)

(defun AIT:browse_Button_cb () 
  (setq DirectoryDialog_result (LM:DirectoryDialog "Choose Directory" nil 0))
  (if DirectoryDialog_result 
    (set_tile "path_EditBox" DirectoryDialog_result)
  )
)
(defun AIT:run_Button_cb()
  (setq tool_name (strcat tools_folder_path "\\" (nth (read(get_tile "tool_PopUpList")) tools_file_names )))
  (load tool_name)
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