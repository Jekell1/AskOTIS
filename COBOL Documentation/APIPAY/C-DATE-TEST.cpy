      ***********************************************************
      *   NAME: C-DATE-TEST
      *   DESC: IF A DATE IS VALID, RETURNS THE SAME
      *         DATE WHEN VALID, OR 0 WHEN NOT VALID
      *   IN  : NUM-DATE (CCYYMMDD)
      *   OUT : NUM-DATE (CCYYMMDD)
      *
      * MJD 141015 MADE SECTION
      ***********************************************************
       C-DATE-TEST SECTION.
           MOVE NUM-DATE TO WS-NDTE.
           IF (NOT WS-NDTE-MM-VALID) OR (NOT WS-NDTE-DD-VALID)
              MOVE 0 TO NUM-DATE
           ELSE
              MOVE NUM-DATE TO P-DATE-IN
              PERFORM C-CALL-JUL
              MOVE P-JULIAN-DATE TO WS-JUL-WK

              MOVE 0 TO NUM-DATE

              MOVE WS-JUL-WK       TO P-CJUL-JULIAN-DATE
              PERFORM C-CALL-CJUL
              MOVE P-CJUL-DATE-OUT TO NUM-DATE

              IF WS-NDTE NOT = NUM-DATE
                 MOVE 0 TO NUM-DATE
                 ELSE
                 MOVE WS-NDTE TO NUM-DATE
              END-IF
           END-IF.

      *********************************************************************
      *  NAME: C-CALL-JUL
      *  DESC: CONVERT GREGORIAN CALANDER DATE TO A JULIAN DATE
      *  IN  : DATE IN CCYYMMDD FORMAT
      *  OUT : JULIAN DATE FOR NUMBER OF DAYS SINCE 12/31/1899
      *********************************************************************
