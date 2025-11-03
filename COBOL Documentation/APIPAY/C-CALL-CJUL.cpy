      *********************************************************************
      *  NAME: C-CALL-CJUL
      *  DESC: CONVERT JULIAN DATE TO A GREGORIAN CALANDER DATE
      *  IN  : JULIAN DATE FOR NUMBER OF DAYS SINCE 12/31/1899
      *  OUT : DATE IN CCYYMMDD FORMAT
      *********************************************************************
       C-CALL-CJUL SECTION.
           MOVE P-CJUL-JULIAN-DATE TO CJUL-IN.
           COMPUTE CJWS-JUL-BY4 = CJUL-IN * 4.
           DIVIDE CJWS-JUL-BY4 BY 1461 GIVING    CJWS-DIV-RESULT
                                     REMAINDER CJWS-MOD-JUL.
           IF CJWS-MOD-JUL NOT = 0
              MOVE 1 TO CJWS-MOD-JUL.

           COMPUTE WS-CJUL-OUT-CCYY = (CJUL-IN * 4 / 1461) +
                                                          CJWS-MOD-JUL.
           COMPUTE WS-CJUL-OUT-DD ROUNDED = CJUL-IN -
                                  ( (WS-CJUL-OUT-CCYY - 1) * 1461 / 4 ).
           IF WS-CJUL-OUT-DD <
              ( CJUL-IN - ( (WS-CJUL-OUT-CCYY - 1) * 1461 / 4 ) )
              ADD 1 TO WS-CJUL-OUT-DD.

           DIVIDE WS-CJUL-OUT-CCYY BY 4 GIVING    CJWS-DIV-RESULT
                                      REMAINDER CJWS-MOD-JUL.
           IF CJWS-MOD-JUL NOT > 0 AND WS-CJUL-OUT-DD NOT < 60
              MOVE 1 TO CJWS-WORK
           ELSE
              MOVE 0 TO CJWS-WORK.

           COMPUTE WS-CJUL-OUT-DD = WS-CJUL-OUT-DD - CJWS-WORK.
           MOVE 13 TO CJWS-MM.
           PERFORM UNTIL (DYTOM(CJWS-MM) < WS-CJUL-OUT-DD)
                                            OR (CJWS-MM = 1)
              ADD -1 TO CJWS-MM
           END-PERFORM.

           IF WS-CJUL-OUT-DD = 59
              ADD CJWS-WORK TO WS-CJUL-OUT-DD.

           SUBTRACT DYTOM(CJWS-MM) FROM WS-CJUL-OUT-DD.
           MOVE CJWS-MM TO WS-CJUL-OUT-MM.
           ADD 1900     TO WS-CJUL-OUT-CCYY.
           MOVE WS-CJUL-OUT-MM TO CJUL-OUT-MM.
           MOVE WS-CJUL-OUT-DD TO CJUL-OUT-DD.
           MOVE WS-CJUL-OUT-YY TO CJUL-OUT-YY.
           MOVE WS-CJUL-OUT-CC TO CJUL-OUT-CC.
           MOVE CJUL-OUT TO P-CJUL-DATE-OUT.

      *********************************************************************
      *  NAME: C-CALL-NEWDATE
      *  DESC: THIS ROUTINE RETURNS A VALID DATE,
      *        THE DAY IS ADJUSTED BACK TO THE LAST VALID
      *        DAY OF THAT MONTH.
      *   IN  : NDTE-DATE-WORK (CCYYMMDD)
      *   OUT : NDTE-DATE (CCYYMMDD)
      *
      * MJD UPDATED FOR 4 DIGIT YEAR.
      *********************************************************************
