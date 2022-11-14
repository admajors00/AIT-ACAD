(vl-load-com)

;this is a test

(defun C:AITTool (/ dcl_id number points) 
  (setq number 1)
  (setq dcl_id (load_dialog "AITWindow.dcl")) ; Load the DCL file.

  ; (action_tile "Exit" "(done_dialog)")
  (setq points (list (list 2.0 2.0 0.0)  ;p3
                     (list 1.0 2.0 0.0) ;p2
                     (list 2.0 1.0 0.0) ;p1
               )
  )
  (if (not (new_dialog "hello" dcl_id))  ; Initialize the dialog.
    (exit) ; Exit if this does not work.
  )
  (action_tile 
    "button1"
    "(AIT:callback)"
  )
  (action_tile 
    "TriCircle"
    "(AIT:triCircle 100)"
  )
  (action_tile 
    "BFind"
    "(AIT:BFind)"
  )
  (action_tile 
    "STRIP_MTEXT_BUTTON"
    "(AIT:STRIP_MTEXT)"
  )
  (start_dialog) ; Display the dialog box.
  (unload_dialog dcl_id) ; Unload the DCL file.
)


(princ "AITTOOL Loaded\n")

