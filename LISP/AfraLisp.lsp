;| ;;;DCLATT.LSP
;This program is for demonstration and tutorial purposes only.
--------------------------------------------
;This program, using Visual Lisp will extract attributes from
;a block and display them in a dialog box.
;The dialog box will be created "on the fly" with the relevant
;number of edit boxes ;to suit the number of attributes within
;the block.
;The new attribute data will then be retrieved from the dialog
;and the block updated.
;Written by Kenny Ramage - May 2002
;http://www.afralisp.net
-------------------------------------------
;Usage :
;Load by typing (load "DCLATT") at the command prompt.
;Type DCLATT at the command prompt to run.
;Select any block containing attributes.
;Replace any of the values in the text boxes to updated
;the attributes.
---------------------------------------------
;Dependencies : None that I know of.
;Not much in the way of error checking I'm afraid.
---------------------------------------------
;Limitations : Will only edit a certain number of attributes.
;System dependent.
;I don't recommend more than 10.
;I've had up to 14 on my display.
----------------------------------------------
----------------------------------------------

(prompt "\nType DCLATT to run...")

(defun c:dclatt (/ theblock thelist n taglist txtlist lg fname fn nu dcl_id l relist) 

  ;load the visual lisp extensions
  (vl-load-com)

  ;get the entity and entity name
  (setq theblock (car (entsel)))

  ;convert to vl object
  (setq theblock (vlax-ename->vla-object theblock))

  ;check if it's a block
  (if 
    (= (vlax-get-property theblock 'ObjectName) 
       "AcDbBlockReference"
    )

    ;if it is, do the following
    (progn 

      ;check if it has attributes
      (if 
        (= 
          (vlax-get-property theblock 
                             'HasAttributes
          )
          :vlax-true
        )

        ;if it has attributes, do the following
        (progn 

          ;get the attributes
          (getatt theblock)

          ;create the dialog
          (create_dialog)

          ;run the dialog
          (run_the_dialog)

          ;update the attributes
          (upatt)
        ) ;progn

        ;No attributes, inform the user
        (alert 
          "This Block has No Attributes!!
                                - Please try again."
        )
      ) ;if
    ) ;progn

    ;it's not a block, inform the user
    (alert "This is not a Block!! - Please try again.")
  ) ;if

  (princ)
);defun
------------------------

(defun getatt (enam) 

  ;retrieve the attributes
  (setq thelist (vlax-safearray->list 
                  (variant-value 
                    (vla-getattributes enam)
                  )
                )
  )

  ;process each attribute
  (foreach n thelist 

    ;get the tag attribute data
    (setq taglist (cons (vla-get-tagString n) taglist) ;get the text attribute data
          txtlist (cons (vla-get-textString n) txtlist) ;how many attributes?
          lg      (length taglist)
    ) ;setq
  ) ;foreach

  ;reverse the lists
  (setq taglist (reverse taglist)
        txtlist (reverse txtlist)
  )
);defun
-------------------------

(defun create_dialog () 

  ;create a temp DCL file
  (setq fname (vl-filename-mktemp "dcl.dcl"))

  ;open it to write
  (setq fn (open fname "w"))

  ;write the dialog header coding
  (write-line "temp : dialog { label = \"Edit Attributes\";" fn)

  ;reset the incremental control number
  (setq nu 0)

  ;start the loop to create the edit boxes
  (repeat lg 

    ;create the edit boxes
    (write-line ": edit_box {" fn)
    (setq l (strcat "\"" "eb" (itoa nu) "\"" ";"))
    (write-line (strcat "key = " l) fn)
    (setq l (nth nu taglist))
    (write-line (strcat "label = " "\"" l "\"" ";") fn)
    (setq l (nth nu txtlist))
    (write-line (strcat "value = " "\"" l "\"" ";") fn)
    (write-line "alignment = centered; edit_width = 20; }" fn)

    ;increment the counter
    (setq nu (1+ nu))
  ) ;repeat

  ;ok and cancel button
  (write-line "ok_only; }" fn)

  ;close the temp DCL file
  (close fn)
);defun
------------------------------

(defun run_the_dialog () 

  ;load the dialog file and definition
  (setq dcl_id (load_dialog fname))
  (if (not (new_dialog "temp" dcl_id)) 
    (exit)
  ) ;if

  (mode_tile "eb0" 2)

  ;if the OK button is selected
  (action_tile "accept" "(retatt)")


  ;start the dialog
  (start_dialog)

  ;unload the dialog
  (unload_dialog dcl_id)

  ;delete the temp DCL file
  (vl-file-delete fname)
);defun
---------------------------

(defun retatt () 

  ;reset the increment counter
  (setq nu 0)

  start
  the
  loop
  (repeat lg 

    ;retrieve the tile value
    (setq l (get_tile (strcat "eb" (itoa nu))))

    ;add it to the list
    (setq relist (cons l relist))

    ;increment the counter
    (setq nu (1+ nu))
  ) ;repeat

  (setq relist (reverse relist))

  ;close the dialog
  (done_dialog)
);defun
-----------------------------------------

(defun upatt () 

  ;reset the increment counter
  (setq nu 0)

  ;start the loop
  (repeat lg 

    ;update the attribute
    (vla-put-textstring (nth nu thelist) (nth nu relist))

    ;increment the counter
    (setq nu (1+ nu))
  ) ;repeat

  ;update the block
  (vla-update theblock)
);defun
---------------------------

;clean loading
(princ)
---------------------------

;End of ATTDCL.LSP
--------------------------- |;