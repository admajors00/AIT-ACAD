(setq GUI_CALLBACK_IFDEF 1)
(defun AIT:checkEntities () 
  (setq entl (entnext))
  (setq ent entl)
  (while (/= nil ent) 
    (setq ent_da (entget ent))
    (setq ent (entnext entl))
    (setq entl ent)
  )
)
                      
(defun AIT:BFind()
  (princ)
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


      (AIT:triCircle iterations   (nth 0 points) (nth 1 points) (nth 2 points) )
    )
  )
  (nth 0 points)
)

