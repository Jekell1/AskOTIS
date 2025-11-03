      *******************************************
       LCAS-ADJUST-FOR-MONTH-END SECTION.
      *******************************************
           DIVIDE ALP-YR BY 4 GIVING LCAS-HOLIDAY-FLAG
                                           REMAINDER LCAS-SUB.
           MOVE 0 TO NDTE-HOLD.
           IF ALP-MO NOT = NDTE-MM
              IF (SP-LCYRTYPE = 358 OR 360)
                      AND (ALP-MO = 1 OR 3 OR 5 OR 7 OR 8 OR 10 OR 12)
                 MOVE 1 TO NDTE-HOLD
              ELSE
                 IF SP-LCYRTYPE = 360 AND ALP-MO = 2
                    IF LCAS-SUB NOT = 0
                       MOVE -2 TO NDTE-HOLD
                    ELSE
                       MOVE -1 TO NDTE-HOLD.
           PERFORM INCREMENT-DAYS.

      ********************************************
