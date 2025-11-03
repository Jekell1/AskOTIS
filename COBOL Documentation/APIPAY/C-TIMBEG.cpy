      *********************************************************
      *          INITIAL ROUTINES FOR ALL ELAPSED TIME ROUTINES
      *
      *   NAME:  C-TIMBEG
      *   DESC:  THIS ROUTINE SETS UP THE DATES IN REVERSE
      *          ORDER IF NUMDATE IS AFTER SYSDATE
      *          THIS ROUTINE ALSO CONVERTS DATES TO JULIAN
      *   IN  :
      *   OUT :
      *********************************************************
       C-TIMBEG SECTION.
           MOVE NUM-DATE TO SV-NUM-DATE.
           MOVE SYS-DATE TO SV-SYS-DATE.

           MOVE WS-DATE1 TO P-DATE-IN.
           PERFORM C-CALL-JUL.
           MOVE P-JULIAN-DATE TO WS-JUL-1.

           MOVE WS-DATE2 TO P-DATE-IN.
           PERFORM C-CALL-JUL.
           MOVE P-JULIAN-DATE TO WS-JUL-2.

           IF WS-JUL-1 > WS-JUL-2
              MOVE WS-DATE1 TO WS-HOLDDATE
              MOVE WS-DATE2 TO WS-DATE1
              MOVE WS-HOLDDATE TO WS-DATE2
              MOVE WS-JUL-1    TO WS-HOLDJUL
              MOVE WS-JUL-2    TO WS-JUL-1
              MOVE WS-HOLDJUL  TO WS-JUL-2
              MOVE -1 TO WS-XSIGN
           ELSE
              MOVE  1 TO WS-XSIGN.

           MOVE SV-NUM-DATE TO NUM-DATE.
           MOVE SV-SYS-DATE TO SYS-DATE.

      **********************************************************
      *          END ROUTINES FOR ALL ELAPSED TIME ROUTINE
      *
      *   NAME:  C-TIMEND
      *   DESC:  THIS ROUTINE SETS THE CORRECT SIGN ON ELAPSED
      *   IN  :
      *   OUT :
      * REV:
      *  JTG 050297 CHANGED FOR 998 ELAPSED UNIT PERIODS
      *********************************************************/
