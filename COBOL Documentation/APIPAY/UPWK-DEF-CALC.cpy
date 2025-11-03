      *================================================================*
      * END COPYBOOK: LIBGB\AGEING.CPY                                *
      *================================================================*
      *================================================================*
      * EMBEDDED COPYBOOK: LIBLP\LPUPWK.CPY                           *
      *================================================================*
      * COPYMEMBER: LIBLP/LPUPWK
      *****************************************************************
      *         CALCULATE DEFERMENT ADJUSTMENT FOR UNIT PERIODS
      *         FROM ESTABLISHED LOAN RECORD
      *
      *   NAME: LPUPWK
      *   DESC: DETERMINE AJUSTMENT FOR DEFERMENT USAGE
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
       UPWK-DEF-CALC SECTION.
           MOVE " " TO UPWK-ERRCD.

      *M   MOVE 1 TO UPWK-UNITPER-WK.
      *MTHE ABOVE LINE IS TEMPORARY RE: STARED OUT LINES:
           IF SP-DEFUNITPER-CD NOT = "A"
              MOVE 1 TO UPWK-UNITPER-WK
              GO TO UPWK-DEF-CALC-EXIT.

           IF UPWK-UNITPER-FREQ NOT = 1
              PERFORM UPWK-CALC-DEF-WK
           ELSE
           IF UPWK-UNITPER-CD = "M"
              MOVE 1 TO UPWK-UNITPER-WK
           ELSE
           IF UPWK-UNITPER-CD = "W"
              MOVE 4 TO UPWK-UNITPER-WK
           ELSE
           IF UPWK-UNITPER-CD = "B" OR "S"
              MOVE 2 TO UPWK-UNITPER-WK
           ELSE
              PERFORM UPWK-CALC-DEF-WK.

       UPWK-DEF-CALC-EXIT.
           EXIT.

      *****************************************************************
      *    CALCULATE UNIT PERIOD WORKER FOR DEFERMENTS
      *****************************************************************
