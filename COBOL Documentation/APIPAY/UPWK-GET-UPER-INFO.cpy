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
      *================================================================*
      * END COPYBOOK: LIBLP\LPUPWK.CPY                                *
      *================================================================*
      *================================================================*
      * EMBEDDED COPYBOOK: LIBLP\LPSCHD.CPY                           *
      *================================================================*
      * COPYMEMBER: LIBLP/LPSCHD
      *********************************************************
      *          UNPACK LOAN PAYMENT SCHEDULE
      *
      *   NAME:  LPSCHD
      *
      *   DESC:  EXPANDS (LT) LOAN PAYMENT SCHEDULE INTO
      *          A (SCHD) PER PAYMENT TABLE.
      *
      *   IN  :  LT-PAY-SCHLD-TBL (1 - 17)
      *   OUT :  SCHD-AMT (1 - 360)
      *
      *   USED:  SCHD-SUB, SCHD-SUB2 FOR TABLE INDICES.
      *          SCHD-SUB3 FOR A COUNTER.
      * REV:
      *  JTG 030293 REVISED TO HANDLE LT-PAY-SCHLD-TBL (17)
      *  BAH 160113 SPLIT OUT LTFILE, LTPFILE, PD0003
      *********************************************************
