(vl-load-com)
(if (/= ifdef_gui_callbacks nil) 
  (load "GUI-CALLBACKS.lsp")
)
(if (/= ifdef_settings_functions nil) 
  (load "SettingsFunctions.lsp")
)


 
(defun C:AITTool (/ dcl_id number what_next cnt P1 P2 P3 project_folder_path 
                  tools_folder_path tools_file_names DirectoryDialog_result 
                  settings_path settings support_folder_path
                 ) 

  (setq dcl_id (load_dialog "AITWindow.dcl")) ; Load the DCL file.
  (setq number 1)
  (setq what_next 2)
  (setq cnt 1)
  (while (>= what_next 2) 

    (if (not (new_dialog "ACADE_TOOL" dcl_id))  ; Initialize the dialog.
      (exit) ; Exit if this does not work.
    )   
    
    (action_tile "run_function_Button" "(AIT:run_Button_cb)")
    (action_tile "open_settings_Button" "(SETTINGS:dialog settings)")
    (action_tile "browse_Button" "(AIT:browse_Button_cb)" )
    (action_tile "path_enter_Button" "(AIT:path_enter_Button_cb)")

    (setq what_next (start_dialog))

    (cond 
      ((= what_next 4) ; Prompt user to
       (c:SMT)
       ; pick pt.
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
       (SETTINGS:dialog settings)
      )

      ((= what_next 0)
       (prompt "\nuser cancelled dialog")
      )
    )
  )
  (unload_dialog dcl_id) ; Unload the DCL file.


  (princ)
)






;;-------------------=={ Directory Dialog }==-----------------;;
;;                                                            ;;
;;  Displays a dialog prompting the user to select a folder   ;;
;;------------------------------------------------------------;;
;;  Author: Lee Mac, Copyright © 2011 - www.lee-mac.com       ;;
;;------------------------------------------------------------;;
;;  Arguments:                                                ;;
;;  msg  - message to display at top of dialog                ;;
;;  dir  - root directory (or nil)                            ;;
;;  flag - bit coded flag specifying dialog display settings  ;;
;;------------------------------------------------------------;;
;;  Returns:  Selected folder filepath, else nil              ;;
;;------------------------------------------------------------;;




;| Symbol 	 Type    	Description
msg 	 String 	Message to display to the top of the dialog (may be an empty string)
dir 	 String 	(Optional) Root Directory, user cannot navigate above this level
flag 	 Integer 	Bit flags controlling the behaviour of the dialog interface (detailed below)

Returns: Selected Directory, else nil if user presses Cancel



Flag Bit Values
Bit 	Result
0 	Standard behaviour (Default)
1 	Only file system folders can be selected. If this bit is set, the OK button is disabled if the user selects a folder that doesn't belong to the file system.
2 	The user is prohibited from browsing below the domain within a network
4 	Room for status text is provided under the dialog box
8 	Returns file system ancestors only. An ancestor is a subfolder that is beneath the root folder. If the user selects an ancestor of the root folder that is not part of the file system, the OK button is grayed.
16 	Shows an edit box in the dialog box for the user to type the name of an item.
32 	Validate the name typed in the edit box.
64 	Enable drag-and-drop capability within the dialog box, reordering, shortcut menus, new folders, delete, and other shortcut menu commands.
128 	The browse dialog box can display URLs.
256 	When combined with flag 64, adds a usage hint to the dialog box, in place of the edit box.
512 	Suppresses display of the 'New Folder' button
1024 	When the selected item is a shortcut, return the PIDL of the shortcut itself rather than its target.
4096 	Enables the user to browse the network branch for computer names. If the user selects anything other than a computer, the OK button is grayed.
8192 	Enables the user to browse the network branch for printer names. If the user selects anything other than a printer, the OK button is grayed.
16384 	Allows browsing for everything: the browse dialog box displays files as well as folders.
32768 	If combined with flag 64, the browse dialog box can display shareable resources on remote systems.
65536 	Windows7 & later: Allow folder junctions such as a library or a compressed file with a .zip file name extension to be browsed. |;


