      ********************************************
       LCAS-CHECK-HOLIDAY SECTION.
      ********************************************
           MOVE NDTE-DATE TO LCAS-WKDATE.
           IF GB-HOLIDAY(LCAS-SUB) = LCAS-WKMMDD
              MOVE 1 TO LCAS-HOLIDAY-FLAG
              MOVE 1 TO NDTE-HOLD
              PERFORM INCREMENT-DAYS.
           ADD 1 TO LCAS-SUB.

      **************************************
