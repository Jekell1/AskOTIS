      *********************************************************
      *          INCREMENT DAYS TO A DATE
      *********************************************************
       C-INCR-DAYS SECTION.
           MOVE NUM-DATE TO SV-NUM-DATE.
           MOVE SYS-DATE TO SV-SYS-DATE.
           MOVE NDTE-DATE TO NUM-DATE.
           MOVE NUM-DATE TO P-DATE-IN.
           PERFORM C-CALL-JUL.
           MOVE P-JULIAN-DATE TO WS-JUL-WK.
           ADD WS-UP-WORK TO WS-JUL-WK.

           MOVE WS-JUL-WK       TO P-CJUL-JULIAN-DATE.
           PERFORM C-CALL-CJUL.
           MOVE P-CJUL-DATE-OUT TO NUM-DATE NDTE-DATE.

           MOVE NDTE-CCYY TO WS-NDTE-CCYY-S.
           MOVE NDTE-MM TO WS-NDTE-MM-S.
           MOVE NDTE-DD TO WS-NDTE-DD-S.

           MOVE SV-NUM-DATE TO NUM-DATE.
           MOVE SV-SYS-DATE TO SYS-DATE.

      *********************************************************
      *          INCREMENT DATES BY UNITS OF 1/2 MONTHS
      *          USES C-INCR-MONTHS BASE ROUTINE FOR IT
      *********************************************************
