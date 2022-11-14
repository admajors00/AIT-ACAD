

;this is a test

(defun C:AITTool (/ dcl_id number points what_next cnt) 
  (vl-load-com)
  (setq dcl_id (load_dialog "AITWindow.dcl")) ; Load the DCL file.
  (setq number 1)
  ; (action_tile "Exit" "(done_dialog)")
  (setq points (list (list 2.0 2.0 0.0)  ;p3
                     (list 1.0 2.0 0.0) ;p2
                     (list 2.0 1.0 0.0) ;p1
               )
  )
  (setq what_next 2)
  (setq cnt 1)
  (while (>= what_next 2) 
    
    (if (not (new_dialog "ACADE_TOOL" dcl_id))  ; Initialize the dialog.
      (exit) ; Exit if this does not work.
    )
    (action_tile  "button1"             "(AIT:callback)")
    (action_tile  "STRIP_MTEXT_BUTTON"  "(done_dialog 4)")
    (action_tile  "TriCircle"           "(AIT:triCircle 100)")
    (action_tile  "BFind"               "(done_dialog 5)")
    (action_tile  "troy_button"         "(done_dialog 6)")
    
    (setq what_next (start_dialog))
    
    (cond
      ((= what_next 4)                       ; Prompt user to
        (c:SMT)         ; pick pt.
      )
      ((= what_next 5)                       ; Prompt user to
        (c:BFind)         ; pick pt.
      )
      ((= what_next 6)                       ; Prompt user to
        (c:Troy)         ; pick pt.
      )
      ((= what_next 0)
        (prompt "\nuser cancelled dialog")
      )
    )
  )
  

  ;(start_dialog) ; Display the dialog box.
  (unload_dialog dcl_id) ; Unload the DCL file.
)


(princ "AITTOOL Loaded\n")

