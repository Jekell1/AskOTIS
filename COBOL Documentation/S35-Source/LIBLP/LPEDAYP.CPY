      * COPYMEMBER: LIBLP/LPEDAYP

      *****************************************************************
      *         EXTENSION/GRACE DAYS CALCULATION (PD-FILE)
      *
      *   NAME: LPEDAYP
      *   DESC: COMPUTES THE ORIGINAL NUMBER OF EXTENSION DAYS
      *                  & WHOLE MONTHS TO THE 1ST PAYMENT
      *         ALSO
      *         THE NUMBER OF GRACE DAYS 1ST PAYMENT WAS EXTENDED
      *
      *         N O T E:
      *                 AN EXACT COPY OF THIS ROUTINE (LPEDAY) EXISTS
      *                 FOR THE LN-FILE, SO REMEMBER TO CHANGE IT!!
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
           MOVE PD-INTDATE TO NDTE-DATE.
           MOVE 1 TO NDTE-HOLD.
           PERFORM INCREMENT-MONTHS.
           MOVE NDTE-DATE TO NUM-DATE.
           MOVE PD-ORIG-1STPYDATE TO SYS-DATE.
           MOVE SP-PYEXYEARTP TO ELAPSED-YRTYPE.
           PERFORM TIMALL.
           MOVE ELAPSED-DAYS TO EDAY-EXTDAYS.
           MOVE ELAPSED-MONTHS TO EDAY-MONTHS.

      * COMPUTE GRACE DAYS:
           MOVE PD-ORIG-1STPYDATE TO NUM-DATE.
           MOVE PD-1STPYDATE TO SYS-DATE.
           PERFORM TIMALL.
           MOVE ELAPSED-DAYS TO EDAY-GRACEDAYS.
