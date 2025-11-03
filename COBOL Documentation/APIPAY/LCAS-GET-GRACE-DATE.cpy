      *******************************************
       LCAS-GET-GRACE-DATE SECTION.
      *******************************************
           PERFORM LCAS-FIND-LAST-GRACE-DAY.
           MOVE 1 TO LCAS-HOLIDAY-FLAG.
           PERFORM LCAS-CHECK-EXTENDING-GRACE
               UNTIL LCAS-HOLIDAY-FLAG = 0.
           MOVE NUM-DATE TO LCAS-GRACE-DATE.

      *******************************************
