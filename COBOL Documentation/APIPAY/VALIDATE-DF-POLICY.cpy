      *================================================================*
      * END COPYBOOK: LIBGB\DATER.CPY                                *
      *================================================================*
      *================================================================*
      * EMBEDDED COPYBOOK: LIBLP\DEFPOL.CPY                           *
      *================================================================*
      * COPYMEMBER: LIBLP/DEFPOL
      ******************************************************************
      *          DEFERMENT POLICY VIOLATION TEST
      *
      *    NAME: DEFPOL
      *    DESC: THIS ROUTINE TEST FOR DEFERMENT POLICY VIOLATIONS
      *
      *    IN  : DEFPOL-TRCD        TRANS CODE, EX. 'DF', 'D2'
      *          DEFPOL-DATE        DATE OF DEFERMENT TEST
      *
      *    OUT : DEFPOL-MAXIMUM     ' '  - DEFERMENT WITHIN POLICY
      *                             'Y'  - DEFERMENT AGAINST POLICY
      *                             'X'  - DEFERMENT EXCEEDS LOAN TERM
      *                             '1'  - DEFERMENT WITHIN 1ST XMONTHS
      *                             'D'  - DEFERMENT MONTHS BETWEEN
      *                             '2'  - DEFERMENT D2-D9 MONTHS BETWEEN
      *                             'R'  - LR DOES NOT EXIST
      *    COPY: DEFPOLW
      *  REV:
      * JTG 111197 ADDED BR-NODEF-1ST-X-MONTHS OPTION MERCURY
      * MJD 991220 ADDED SP-DEFPOLCD = "B" (WORLD PR#216)
      * BAH 070201 ADDED BR-MIN-MONTHS-DEF FOR REGACC PR# 162
      * BAH 070503 CHANGED 365 TO 360 FOR ABOVE REGACC TEST, PL# 588
      *  CS 070808 ADDED TEST IF BR-MIN-MONTHS-DEF NOT NUMERIC, SKIP READING
      *            FOR THE LR. MULLEN
      *  CS 080408 ADDED TESTS FOR BR-MIN-MONTHS-D2, IF A MULTIPLE(D2-D9)
      *            IS POSTED, MUST WAIT BR-MIN-MONTHS-D2 TO POST ANOTHER
      *            DF TRANSACTION REGACC PR#202
      *  CS 080408 REGACC WANTS THE FLAG BR-NODEF-1ST-X-MONTHS TO BE
      *            PAYS PAID RATHER THAN ELAPSED TIME REGACC PR#202
      *  CS 120806 ADDED DEFPOL-MAXIMUM "1","2","D"  REGACC PR#3189
      *            "1", ATTEMPTING DF IN VIOLATION OF 1ST XMONTHS
      *            "D", MONTHS BETWEEN 'DF' & CURRENT DEFERMENT/S VIOLATION
      *            "2", MONTHS BETWEEN 'D2' AND CURRENT DEFERMENT/S VIOLATION
      *            "D" AND "2" COME FROM THE 'LR', IF NO 'LR' SETS 'R'
      * BAH 131017 ADDED SP-DEFPOLCD "C" FOR HONOR, PR# 4382
      * BAH 151021 REMOVE B2FILE, PD0003
      * BAH 20190709 REMOVED GP-LR-TRAILER-TRIAD, PT0151 
      * BAH 20220803 HARD CODED START
      ******************************************************************
       VALIDATE-DF-POLICY  SECTION.

           MOVE " " TO DEFPOL-MAXIMUM.
           MOVE DEFPOL-TRCD TO DEFPOL-WORKER.
           IF DEFPOL-FTST = "F"
              MOVE "1" TO DEFPOL-FTST.


      *    IF GP-I-AM-REGACC
      *       IF BR-NODEF-1ST-X-MONTHS NOT = 0
      *          MOVE LN-1STPYDATE TO PDTH-DATE-WORK
      *          PERFORM PAID-THRU-CALCULATION
      *          MOVE LN-1STPYDATE TO NDTE-DATE
      *          MOVE -1 TO NDTE-HOLD
      *          PERFORM INCREMENT-MONTHS
      *          MOVE NDTE-DATE      TO NUM-DATE
      *          MOVE PDTH-DATE-FULL TO SYS-DATE
      *          PERFORM TIM360
      *          IF ELAPSED-MONTHS <  BR-NODEF-1ST-X-MONTHS
      *             MOVE "1" TO DEFPOL-MAXIMUM
      *             GO TO VALIDATE-DF-POLICY-EXIT.

           IF BR-NODEF-1ST-X-MONTHS NUMERIC
              IF BR-NODEF-1ST-X-MONTHS NOT = 0
                 MOVE LN-LOANDATE TO NUM-DATE
                 MOVE DEFPOL-DATE TO SYS-DATE
                 PERFORM TIM360
                 IF ELAPSED-DAYS <= (BR-NODEF-1ST-X-MONTHS * 30)
                    MOVE "Y" TO DEFPOL-MAXIMUM.

      *    IF GP-I-AM-REGACC
      *       IF DEFPOL-MAXIMUM NOT = SPACES
      *          GO TO VALIDATE-DF-POLICY-EXIT.

           IF SP-DEFPOLCD = " " OR "C"
              IF (LN-YTDNODEF + DEFPOL-NODF) > SP-DEFPOLMX(1)
                     OR (LN-TOTNODEF + DEFPOL-NODF) > SP-DEFPOLMX(2)
                 MOVE "Y" TO DEFPOL-MAXIMUM.

           IF SP-DEFPOLCD = "A"
              IF LN-SUM-ORGTERM NOT > SP-DEFPOLMX(3)
                 IF (LN-TOTNODEF + DEFPOL-NODF) > SP-DEFPOLMX(1)
                    MOVE "Y" TO DEFPOL-MAXIMUM
                 ELSE
                    NEXT SENTENCE
              ELSE
                 IF (LN-TOTNODEF + DEFPOL-NODF) > SP-DEFPOLMX(2)
                    MOVE "Y" TO DEFPOL-MAXIMUM.

           IF SP-DEFPOLCD = "B"
              IF ((LN-YTDNODEFACT + 1) > SP-DEFPOLMX(1)) OR
                 ((LN-TOTNODEFACT + 1) > SP-DEFPOLMX(2))
                    MOVE "Y" TO DEFPOL-MAXIMUM.

           IF (LN-TOTNODEF + DEFPOL-NODF) > LN-SUM-ORGTERM
              MOVE "X" TO DEFPOL-MAXIMUM.

      * CHECK MINIMUM MONTHS BETWEEN DEFERMENTS FINDING LAST TRUE DEFERMENT
      * BY READING LP
           IF SP-DEFPOLCD = "C"
              PERFORM DEFPOL-MINIMUM-MONTHS-BETWEEN.

       VALIDATE-DF-POLICY-EXIT.
           EXIT.


      ***********************************
      * HONOR PR# 4382  SP-DEFPOLCD "C"
      * THIS ROUTINE CAN BE USED BY ANYONE.
      ***********************************
