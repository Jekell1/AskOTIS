      *****************************************************************
      *         CALCULATE DEFERMENT ADJUSTMENT FOR UNIT PERIODS
      *         FROM ESTABLISHED LOAN RECORD
      *
      *   NAME: LPUPWK
      *   DESC: DETERMINE AJUSTMENT FOR LATE CHARGE USAGE
      *
      *   IN  : UPWK-UNITPER-CD         (LN-UNITPER-CD)
      *         UPWK-UNITPER-FREQ       (LN-UNITPER-FREQ)
      *
      *   OUT : UPWK-UNITPER-WK
      *         UPWK-ERRCD
      *
      *   COPY:
      * REV:
      *  071696 UNCOMMENTED CODE TO WORK WITH WEEKLY (A50 CONV)
      *****************************************************************
       UPWK-LC-CALC SECTION.
           MOVE " " TO UPWK-ERRCD.

           MOVE 100 TO UPWK-LC-PER-FAC
                       UPWK-LC-MIN-FAC
                       UPWK-LC-MAX-FAC
                       UPWK-LC-GRACE-FAC.

           IF SP-LCUNITPER-CD NOT = "A"
              GO TO UPWK-LC-CALC-EXIT.

           IF NOT (UPWK-UNITPER-CD = "M"
                             AND UPWK-UNITPER-FREQ = 1)
              IF UPWK-UNITPER-FREQ NOT = 1
                 PERFORM UPWK-CALC-LC-WK
              ELSE
              IF UPWK-UNITPER-CD = "W"
                 MOVE 20.0 TO UPWK-LC-PER-FAC
                              UPWK-LC-MIN-FAC
                              UPWK-LC-MAX-FAC
                 MOVE 30.0 TO UPWK-LC-GRACE-FAC
              ELSE
              IF UPWK-UNITPER-CD = "B" OR "S"
                 MOVE 50.0 TO UPWK-LC-PER-FAC
                              UPWK-LC-MIN-FAC
                              UPWK-LC-MAX-FAC
                              UPWK-LC-GRACE-FAC
              ELSE
                 PERFORM UPWK-CALC-LC-WK.

       UPWK-LC-CALC-EXIT.
           EXIT.

      *****************************************************************
      *    CALCULATE UNIT PERIOD WORKERS FOR LATE CHARGES
      *****************************************************************
