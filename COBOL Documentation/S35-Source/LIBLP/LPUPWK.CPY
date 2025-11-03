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
       UPWK-CALC-DEF-WK SECTION.
           PERFORM UPWK-GET-UPER-INFO.
           IF UPWK-ERRCD = " "
              COMPUTE UPWK-UNITPER-WK =
                             UPWK-UNITPER-PER-YEAR / 12.

      ************************************************************************
      *         CALCULATE CONTRACTUAL ADJUSTMENT FOR UNIT PERIODS
      *         FROM ESTABLISHED LOAN RECORD
      *
      *   NAME: LPUPWK
      *   DESC: DETERMINE AJUSTMENT FOR CONTRACTUAL USAGE
      *
      *   IN  : UPWK-UNITPER-CD         (LN-UNITPER-CD)
      *         UPWK-UNITPER-FREQ       (LN-UNITPER-FREQ)
      *
      *   OUT : UPWK-UNITPER-WK
      *         UPWK-ERRCD
      *
      *   COPY:
      * REV:
      *  MLD 071696 UNCOMMENTED CODE TO WORK WITH WEEKLY (A50 CONV)
      *  JTG 071696 ADDED SP-BANKUNITPER-CD & SP-TRWBANKUNITPER-CD = "B"
      *             (FL AUTO)
      *  JTG 090897 CORRECTED TEST ON SP-BANKUNITPER-CD TO INCLUDE 'C' & 'D'
      ************************************************************************
       UPWK-CONTR-CALC SECTION.
           MOVE " " TO UPWK-ERRCD.

      *M   MOVE 1 TO UPWK-UNITPER-CONTR-WK.
      *MTHE ABOVE LINE IS TEMPORARY RE: STARED OUT LINES:
      *    IF NOT (SP-BANKUNITPER-CD = "A" OR "B")
           IF NOT (SP-BANKUNITPER-CD = "A" OR "B" OR "C" OR "D")
              MOVE 1 TO UPWK-UNITPER-CONTR-WK
              GO TO UPWK-CONTR-CALC-EXIT.

           IF UPWK-UNITPER-FREQ NOT = 1
              PERFORM UPWK-CALC-CONTR-WK
           ELSE
           IF UPWK-UNITPER-CD = "M"
              MOVE 1 TO UPWK-UNITPER-CONTR-WK
           ELSE
           IF UPWK-UNITPER-CD = "W"
              MOVE 4 TO UPWK-UNITPER-CONTR-WK
           ELSE
           IF UPWK-UNITPER-CD = "B" OR "S"
              MOVE 2 TO UPWK-UNITPER-CONTR-WK
           ELSE
              PERFORM UPWK-CALC-CONTR-WK.

       UPWK-CONTR-CALC-EXIT.
           EXIT.

      *****************************************************************
      *    CALCULATE UNIT PERIOD WORKER FOR CONTRACTUAL
      *****************************************************************
       UPWK-CALC-CONTR-WK SECTION.
           PERFORM UPWK-GET-UPER-INFO.
           IF UPWK-ERRCD = " "
              COMPUTE UPWK-UNITPER-CONTR-WK =
                             UPWK-UNITPER-PER-YEAR / 12.

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
       UPWK-CALC-LC-WK SECTION.
           PERFORM UPWK-GET-UPER-INFO.
           IF UPWK-ERRCD = " "
              COMPUTE UPWK-LC-PER-FAC
                      UPWK-LC-MAX-FAC
                      UPWK-LC-GRACE-FAC = 12 / UPWK-UNITPER-PER-YEAR.


      *****************************************************************
      *         GET INFORMATION ON UNIT PERIODS
      *         FROM ESTABLISHED LOAN RECORD
      *
      *   NAME: LPUPWK
      *   DESC: DETERMINE NO. OF UNIT PERIODS PER YEAR
      *                   NO. OF DAYS IN THE UNIT PERIOD
      *
      *   IN  : UPWK-UNITPER-CD         (LN-UNITPER-CD)
      *         UPWK-UNITPER-FREQ       (LN-UNITPER-FREQ)
      *
      *   OUT : UPWK-UNITPER-PER-YEAR
      *         UPWK-DAYS-IN-UNITPER
      *         UPWK-ERRCD
      *
      *   COPY:
      * REV:
      *  071696 UNCOMMENTED CODE TO WORK WITH WEEKLY (A50 CONV)
      *****************************************************************
       UPWK-GET-UPER-INFO SECTION.
           MOVE " " TO UPWK-ERRCD.

           IF (UPWK-UNITPER-FREQ = 0)
                 OR (NOT
                        (UPWK-UNITPER-CD = "M" OR "S" OR "W"
                                                 OR "B" OR "D" OR "Y")
                    )
              MOVE "E" TO UPWK-ERRCD
              GO TO UPWK-GET-UPER-INFO-EXIT.

           IF UPWK-UNITPER-CD = "M"
              COMPUTE UPWK-UNITPER-PER-YEAR = 12 / UPWK-UNITPER-FREQ
              COMPUTE UPWK-DAYS-IN-UNITPER = 30 * UPWK-UNITPER-FREQ
           ELSE
           IF UPWK-UNITPER-CD = "S"
              COMPUTE UPWK-UNITPER-PER-YEAR = 24 / UPWK-UNITPER-FREQ
              COMPUTE UPWK-DAYS-IN-UNITPER = 15 * UPWK-UNITPER-FREQ
           ELSE
           IF UPWK-UNITPER-CD = "W"
              COMPUTE UPWK-UNITPER-PER-YEAR = 52 / UPWK-UNITPER-FREQ
              COMPUTE UPWK-DAYS-IN-UNITPER = 7 * UPWK-UNITPER-FREQ
           ELSE
           IF UPWK-UNITPER-CD = "B"
              COMPUTE UPWK-UNITPER-PER-YEAR = 26 / UPWK-UNITPER-FREQ
              COMPUTE UPWK-DAYS-IN-UNITPER = 14 * UPWK-UNITPER-FREQ
           ELSE
           IF UPWK-UNITPER-CD = "D"
              COMPUTE UPWK-UNITPER-PER-YEAR = 365 / UPWK-UNITPER-FREQ
              COMPUTE UPWK-DAYS-IN-UNITPER = 1 * UPWK-UNITPER-FREQ
           ELSE
           IF UPWK-UNITPER-CD = "Y"
              COMPUTE UPWK-UNITPER-PER-YEAR = 1 / UPWK-UNITPER-FREQ
              COMPUTE UPWK-DAYS-IN-UNITPER = 365 * UPWK-UNITPER-FREQ.

       UPWK-GET-UPER-INFO-EXIT.
           EXIT.
