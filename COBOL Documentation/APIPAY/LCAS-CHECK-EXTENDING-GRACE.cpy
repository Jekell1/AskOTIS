      ********************************************
       LCAS-CHECK-EXTENDING-GRACE SECTION.
      ********************************************
           MOVE 0 TO LCAS-HOLIDAY-FLAG.
           IF SP-LCGRACECD = "S" OR "W"
              MOVE 1 TO LCAS-SUB
              PERFORM LCAS-CHECK-HOLIDAY
                 UNTIL LCAS-SUB > 16
                        OR GB-HOLIDAY(LCAS-SUB) = 0
              PERFORM LCAS-CHECK-DAY.

      ********************************************
