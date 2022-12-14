(vl-load-com)

;this is a test

(defun C:AITTool (/ dcl_id number points)
  (setq number 1)
  (setq dcl_id (load_dialog "AITWindow.dcl")) ; Load the DCL file.

					; (action_tile "Exit" "(done_dialog)")
  (setq	points (list (list 2.0 2.0 0.0)	;p3
		     (list 1.0 2.0 0.0)	;p2
		     (list 2.0 1.0 0.0)	;p1



	       )
  )
  (if (not (new_dialog "hello" dcl_id))	; Initialize the dialog.
    (exit)				; Exit if this does not work.
  )
  (action_tile
    "button1" 
    "(AIT:checkEntities)")
  (action_tile
    "TriCircle"
    "(AIT:triCircle 100)"
  )
  (action_tile
    "BFind"
    "(AIT:BFind)"
  )
  (start_dialog)			; Display the dialog box.
  (unload_dialog dcl_id)		; Unload the DCL file.
)

(defun AIT:checkEntities ()
  (setq entl (entnext))
  (setq ent  entl)            
  (while (/= nil ent)
    (setq ent_da (entget ent))
    (setq ent (entnext entl))
    (setq entl ent)
  )
  )
                      
)

(defun AIT:callback ()

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

(defun AIT:crash ()
  (princ "get got ")
  (AIT:crash)


)
(defun AIT:triCircle
       (iterations / x1 y1 x2 y2 x3 y3 A B C D Yc Xc R P1 P2 P3)
;;;  (done_dialog 1)
;;;  (setq P1 (getPoint "\nStart Point : "))
;;;  (setq P2 (getpoint "\nSecond Point: "))
;;;  (setq P3 (getpoint "\nThird Point : "))
;;;  (start_dialog)
;;;  (setq points (list P1 P2 P3))
  (entmakex (list (cons 0 "LINE")
		  (cons 10 (nth 2 points))
		  (cons 11 (nth 1 points))
	    )
  )
  (entmakex (list (cons 0 "LINE")
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
      (Setq newPointX
	     (+	(* 0.5
		   (- (nth 0 (nth 1 points)) (nth 0 (nth 2 points)))
		)
		(nth 0 (nth 2 points))
	     )
      )
      (Setq newPointY
	     (+	(* 0.5
		   (- (nth 1 (nth 1 points)) (nth 1 (nth 2 points)))
		)
		(nth 1 (nth 2 points))
	     )
      )



      (setq
	points (cons (list newPointX newPointY 0.0) points)
      )
      (setq points (reverse (cdr (reverse points))))

					;(Line '(nth 0 points) '(nth 1 points))
      (entmakex	(list (cons 0 "LINE")
		      (cons 10 (nth 1 points))
		      (cons 11 (nth 0 points))
		)
      )


      (AIT:triCircle iterations)
    )
  )
  (nth 0 points)


)

(princ "AITTOOL Loaded\n")

