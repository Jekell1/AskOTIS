      * COPYMEMBER: LIBLP/LPEDAY

      *****************************************************************
      *         EXTENSION/GRACE DAYS CALCULATION   (LN-FILE)
      *
      *   NAME: LPEDAY
      *   DESC: COMPUTES THE ORIGINAL NUMBER OF EXTENSION DAYS
      *                  & WHOLE MONTHS TO THE 1ST PAYMENT
      *         ALSO
      *         THE NUMBER OF GRACE DAYS 1ST PAYMENT WAS EXTENDED
      *
      *         N O T E:
      *                 AN EXACT COPY OF THIS ROUTINE (LPEDAYP) EXISTS
      *                 FOR THE PD-FILE, SO REMEMBER TO CHANGE IT!!
      *
      *   IN  :
      *   OUT : EDAY-EXTDAYS, EDAY-MONTHS, EDAY-GRACEDAYS
      *   USED:
      *   COPY: LPEDAYW
      * REV:
      *  JTG 043090 ADDED EDAY-MONTHS FOR SP-INSEXCEPT = 6
      *****************************************************************
       EXTENSION-DAYS-CALCULATION SECTION.

      * COMPUTE EXTENSION DAYS:
           MOVE LN-INTDATE TO NDTE-DATE.
           MOVE 1          TO NDTE-HOLD.
           PERFORM INCREMENT-MONTHS.

           MOVE NDTE-DATE         TO NUM-DATE.
           MOVE LN-ORIG-1STPYDATE TO SYS-DATE.
           MOVE SP-PYEXYEARTP     TO ELAPSED-YRTYPE.
           PERFORM TIMALL.
           MOVE ELAPSED-DAYS   TO EDAY-EXTDAYS.
           MOVE ELAPSED-MONTHS TO EDAY-MONTHS.

      * COMPUTE GRACE DAYS:
           MOVE LN-ORIG-1STPYDATE TO NUM-DATE.
           MOVE LN-1STPYDATE      TO SYS-DATE.
           PERFORM TIMALL.
           MOVE ELAPSED-DAYS TO EDAY-GRACEDAYS.
