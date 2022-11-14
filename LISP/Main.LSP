(vl-load-com)

;this is a test



(defun C:AITTool (/ dcl_id number points) 
  (command "OPENDCL")
  (setq project
      '("YWt6A+0WAACQSFIyBuKTJrERKitqgNisiCxYPuRu5e7uTzvX/xcZ2C20xnxvLues/Ag5fE1juHw9"
    "NcqC9mWjH5ygFpzhXsFsAbvrH1tF6VAixkov+mtfb9ehRxGEoMUZ2BGsJL47mZvDCbFFieAFkUeZ"
    "RRGgyQuPt1wGaHGfS+K6aqC5QrCfchjO/nGGmn6gvMW2jADumWUT4eqHmmPwQ/dd5hhF1eRM8anL"
    "qS8U+a/ew/VMKZHs+94Jaf7hBf9AoDvDzby3dbbf+lxUZhu5ZLjG2mQlOyB1Qsym+UILe6sopwXi"
    "xGw5aKe0jQdrxMs7zyabllQ5kptfSM2SR7hPB8vm2fsvyyaeBucb6rFUFMO6D50S4T/jQ8XvMA/v"
    "5RrbZv41l5yvIoboW/FivFeQEew559O9wuG+eqJBlhcIMLesouk4wBKaReLRV7W7gxSKKUIHuXJd"
    "xqJ0mGepdWbaQjnxsZ9S3QXopRSOhF9JmQQswJPBsyIp85IygeGuwZr32DWidxI+kplAdMhfwaFn"
    "aKlht/E6gSNaFYER8oLT78FIjJ0WQC/GFZYj7GkbwD9RJgPv1Uzf1aIj0bH1ANzwUpWgGP753Gvj"
    "GFWDfGUlK6IdDRudWi8P9IbGujZSTkANEwOme2mQEUzMgNi2Q+C/UE+WxDMmrCCmyDf0MpzC50Y2"
    "9f3kck2qdKyiU23DpTjAF2yc+O/RZYdVXYnnFrMHxTcxzhoaBrAMS3NgZwTD/4mesPudfC6yZYc8"
    "ltuqPfeEB90RZqf8UujrfVwBGKjHU6wBUowqWoFYjNkY4bldgzuvQfeYKQUB54uidYNu8oG9jXWi"
    "gxYBpIGxTrV9B9lngQxKgZncQWi1L+/xZGrr80Ifmsnz8uqRYWE/UdNyP10WffUjWHw9wDmt3KPS"
    "HLDcEo1vqMN0xbMUFLmzmFNGIGsOMi3N3qvP305mdxUZ4JNaFRM/Gp8WPx0/HD8aPx3tq1jNMwh1"
    "eHVodVjlwkhnRVsR2RrKt6XfnLugZnEah890RvvodOYmzQrH1dNHQTqNoO0LCvkfdpj3q3JXiTdL"
    "XX3BGJIZtQNvw9eqoAhduRS2/orJS58XHaK5ilVVuwpcdm2uQ7pNU03ZK2LIlpBXrRIr/SvOFiC5"
    "oKQfMYHTE05HYb2ciRa/3GHkGfu1ZnAbXRDJJw3ZCQlyGcFeZxmFnp2SOdJHsAjSahngsW0OW8Av"
    "IhMvT3cpUxG3CyJDRSIXUyFoKYeZ/NWDNrjrAOe6DtuR1WbQMF1nKWeGxixigBEO64LA7ERm7GYr"
    "ausUbow+FvpSiAwGN7f53GRiHTW2+2yx/pzWD+xiIe+dUlFHlJQCaO9mS/DpGQsk1I7kG5F0pQvL"
    "Ji58OjbyZFN24+8pXDpW72KbRIXoBuPfUeq67hGcSFq42L5F/xFclB5LYYbooc6C+ZtFUl/usgmC"
    "CbzebbkfR+/rb3Iz6FWtChs1481ytCN99AgaPD5C00/nTvUr73f9f6GaPVeAy6rE1gvcX1iJ5dzp"
    "jq+Od4Lc1DtP/xau9kA53D/38B0YgzHpFpY2bbaRIf6hnLM=")
  )
  (dcl_project_import project nil nil)
; call the method to show the Hello World dialog box example
  (dcl_Form_Show ACADE_TOOL_FORM)
(princ)
)

(defun AIT:checkEntities () 
  (setq entl (entnext))
  (setq ent entl)
  (while (/= nil ent) 
    (setq ent_da (entget ent))
    (setq ent (entnext entl))
    (setq entl ent)
  )
)
(defun c:Untitled/Form1#OnInitialize (/) 
  (dcl-MessageBox 
    "To Do: code must be added to event handler\r\nc:Untitled/Form1#OnInitialize"
    "To do"
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
(defun AIT:triCircle (iterations / x1 y1 x2 y2 x3 y3 A B C D Yc Xc R P1 P2 P3) 
  ;;;  (done_dialog 1)
  ;;;  (setq P1 (getPoint "\nStart Point : "))
  ;;;  (setq P2 (getpoint "\nSecond Point: "))
  ;;;  (setq P3 (getpoint "\nThird Point : "))
  ;;;  (start_dialog)
  ;;;  (setq points (list P1 P2 P3))
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


      (AIT:triCircle iterations)
    )
  )
  (nth 0 points)
)

(princ "AITTOOL Loaded\n")

