      * COMMENTED OUT MJD 2017/02/07
      * IT WAS ALREADY COMMENTED OUT EVERYWHERE IT WAS CALLED.
      * IT IS NOT NBEEDED WHEN USING 8 DIGIT YEAR IN CCYYMMDD FORMAT.
      * C-SET-WS-DATE-YYYY SECTION.
      *     IF WS-DATE1-YY NOT < EXT-JULIAN-CC
      *        ADD 1900 WS-DATE1-YY GIVING WS-DATE1-YYYY
      *     ELSE
      *        ADD 2000 WS-DATE1-YY GIVING WS-DATE1-YYYY.
      *     IF WS-DATE2-YY NOT < EXT-JULIAN-CC
      *        ADD 1900 WS-DATE2-YY GIVING WS-DATE2-YYYY
      *     ELSE
      *        ADD 2000 WS-DATE2-YY GIVING WS-DATE2-YYYY.
      * NOT NEEDED W/ 8 DIGIT YEAR
      *C-ADJ-WS-NDTE-YY-S SECTION.
      *    IF WS-NDTE-YY-S NOT < EXT-JULIAN-CC
      *       ADD 1900 TO  WS-NDTE-YY-S
      *    ELSE
      *       ADD 2000 TO WS-NDTE-YY-S.
      ***************************************************************
      * NAME: C-LEAP-YEAR-TEST
      * DESC: TEST A GIVEN YEAR YEAR TO DETERMINE IF IT A LEAP YEAR
      * IN  : LEAP-YEAR-CCYY
      * OUT : LEAP-YEAR-TRUE / LEAP-YEAR-FALSE
      *
      *
      * TO DETERMINE WHETHER A YEAR IS A LEAP YEAR, FOLLOW THESE STEPS:
      * 1. IF THE YEAR IS EVENLY DIVISIBLE BY 4, GO TO STEP 2.
      *           OTHERWISE, GO TO STEP 5.
      * 2. IF THE YEAR IS EVENLY DIVISIBLE BY 100, GO TO STEP 3.
      *           OTHERWISE, GO TO STEP 4.
      * 3. IF THE YEAR IS EVENLY DIVISIBLE BY 400, GO TO STEP 4.
      *           OTHERWISE, GO TO STEP 5.
      * 4. THE YEAR IS A LEAP YEAR (IT HAS 366 DAYS).
      * 5. THE YEAR IS NOT A LEAP YEAR (IT HAS 365 DAYS).
      ***************************************************************
       C-LEAP-YEAR-TEST SECTION.
           MOVE "F" TO LEAP-YEAR-FG.
           DIVIDE LEAP-YEAR-CCYY BY 4
                  GIVING ND-WS-DIV-RESULT REMAINDER ND-WS-MOD-4.
           IF ND-WS-MOD-4 NOT = 0
              MOVE "F" TO LEAP-YEAR-FG
           ELSE
              DIVIDE LEAP-YEAR-CCYY BY 100
                     GIVING ND-WS-DIV-RESULT REMAINDER ND-WS-MOD-4
              IF ND-WS-MOD-4 NOT = 0
                     MOVE "T" TO LEAP-YEAR-FG
              ELSE
                 DIVIDE LEAP-YEAR-CCYY BY 400
                     GIVING ND-WS-DIV-RESULT REMAINDER ND-WS-MOD-4
                 IF ND-WS-MOD-4 NOT = 0
                        MOVE "F" TO LEAP-YEAR-FG
                 ELSE
                        MOVE "T" TO LEAP-YEAR-FG
                 END-IF
              END-IF
           END-IF.

      ***************************************************************
      * NAME: CONVERT-00MMDDYY-TO-CCYYMMDD
      * DESC: CONVERT A DATE IN THE FORMATE OF 00MMDDYY TO CCYYMMDD
      * IN  : CONVERT-DATE (00MMDDYY WHERE CONVERT-DATE NOT = 0)
      * OUT : CONVERT-DATE (CCYYMMDD)
      ***************************************************************
