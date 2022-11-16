

;this is a test

(defun C:AITTool (/ dcl_id number points what_next cnt P1 P2 P3) 
  (vl-load-com)
  (if (/= (boundp GUI_CALLBACK_IFDEF) t)
      (load "GUI-CALLBACKS.lsp")
    )
  )
  
  (setq dcl_id (load_dialog "AITWindow.dcl")) ; Load the DCL file.
  (setq number 1)
  ; (action_tile "Exit" "(done_dialog)")
 ;|  (setq points (list (list 2.0 2.0 0.0)  ;p3
                     (list 1.0 2.0 0.0) ;p2
                     (list 2.0 1.0 0.0) ;p1
               )
  ) |;
  (setq what_next 2)
  (setq cnt 1)
  (while (>= what_next 2) 

    (if (not (new_dialog "ACADE_TOOL" dcl_id))  ; Initialize the dialog.
      (exit) ; Exit if this does not work.
    )

    (action_tile "STRIP_MTEXT_BUTTON" "(done_dialog 4)")
    (action_tile "TriCircle" "(done_dialog 5)")
    (action_tile "BFind" "(done_dialog 6)")
    (action_tile "troy_button" "(done_dialog 7)")
    (action_tile "button1" "(done_dialog 8)")

    (setq what_next (start_dialog))

    (cond 
      ((= what_next 4) ; Prompt user to
       (c:SMT) ; pick pt.
      )
      ((= what_next 5) ; Prompt user to
       (setq P1 (getPoint "\nStart Point : "))
       (setq P2 (getpoint "\nSecond Point: "))
       (setq P3 (getpoint "\nThird Point : "))
       (AIT:triCircle 100 P1 P2 P3) ; pick pt.
      )
      ((= what_next 6) ; Prompt user to
       (c:BFind) ; pick pt.
      )
      ((= what_next 7) ; Prompt user to
       (c:Troy) ; pick pt.
      )
      ((= what_next 8) ; Prompt user to
       (AIT:callback) ; pick pt.
      )

      ((= what_next 0)
       (prompt "\nuser cancelled dialog")
      )
    )
  )


  ;(start_dialog) ; Display the dialog box.
  (unload_dialog dcl_id) ; Unload the DCL file.
)

;; Add Support File Search Paths  -  Lee Mac
;; Adds a list of Support File Search Paths, excluding duplicates and invalid paths.
;; lst - [lst] list of paths to add, e.g. '("C:\\Folder1" "C:\\Folder2" ... )
;; Returns: [str] "ACAD" Environment String following modification

