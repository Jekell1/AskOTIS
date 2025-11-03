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
       C-CALL-NEWDATE SECTION.
           MOVE NDTE-DATE-WORK TO ND-WS-NDTE.
           MOVE ND-WS-NDTE-CCYY TO LEAP-YEAR-CCYY.
           PERFORM C-LEAP-YEAR-TEST.
           IF (ND-WS-NDTE-MM > 0 AND ND-WS-NDTE-MM < 13) AND
              (ND-WS-NDTE-DD >
                   ( DYTOM(ND-WS-NDTE-MM + 1) - DYTOM(ND-WS-NDTE-MM) ) )
              IF ND-WS-NDTE-MM NOT = 2
                 COMPUTE ND-WS-NDTE-DD = DYTOM(ND-WS-NDTE-MM + 1)
                                     - DYTOM(ND-WS-NDTE-MM)
              ELSE
                 IF LEAP-YEAR-TRUE
                    MOVE 29 TO ND-WS-NDTE-DD
                 ELSE
                    MOVE 28 TO ND-WS-NDTE-DD
                 END-IF
              END-IF
           END-IF.
           MOVE ND-WS-NDTE TO NDTE-DATE.

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
