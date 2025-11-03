      *******************************************
       LCAS-FIND-LAST-GRACE-DAY SECTION.
      *******************************************
           MOVE NUM-MO TO ALP-MO.
           MOVE NUM-YR TO ALP-YR.

           MOVE LCAS-LCGRACE TO NDTE-HOLD.
           PERFORM INCREMENT-DAYS.
           MOVE NDTE-DATE TO NUM-DATE.
           PERFORM LCAS-ADJUST-FOR-MONTH-END.

       LCAS-FIND-LAST-GRACE-DAY-EXIT.
           EXIT.

      *******************************************
