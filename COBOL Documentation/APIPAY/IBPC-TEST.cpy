      *================================================================*
      * END COPYBOOK: LIBLP\SETCTBL.CPY                                *
      *================================================================*
      *================================================================*
      * EMBEDDED COPYBOOK: LIBLP\LPIBPC.CPY                           *
      *================================================================*
      * COPYMEMBER: LIBLP/LPIBPC
      **************************************************************
      *         INTEREST BEARING OR PRECOMPUTE
      *               ACCOUNT STATUS TEST
      *
      *   NAME: LPIBPC
      *   DESC: THIS ROUTINE DETERMINES IF THE ACCOUNT
      *         SHOULD BE PROCESSED AS INTEREST BEARING
      *         OR PRECOMPUTE, BASED ON THE ACCOUNT'S
      *         CURRENT STATUS AND PAYMENT DATE.
      *
      *   IN : IBPC-DATE   (DATE OF PAYMENT)
      *   OUT: IBPC-FG     "I" - INTEREST BEARING
      *                    "P" - PRECOMPUTE
      *
      *        IBPC-STAT   CONTAINS "E" IF THE LOAN EXPIRED BEFORE
      *        IBPC-STAT-2 GOING INTO JUDGEMENT OR ACCELERATION OR
      *                    RATE CHANGE/REDUCTION OR IF LN-STATUSFG = " ".
      *                    IBPC-STAT-2 CONTAINS LN-STATUSFG.
      *
      *           OR
      *
      *        IBPC-STAT   IF THE LOAN EXPIRED AFTER THE EVENT IN
      *        IBPC-STAT-2 LN-STATUSFG, THE VALUE IN IBPC-STAT WILL BE
      *                    LN-STATUSFG AND IBPC-STAT-2 WILL CONTAIN "E".
      *
      *           OR
      *
      *        IBPC-STAT   IF THE LOAN WILL EXPIRE AFTER THE EVENT IN
      *        IBPC-STAT-2 LN-STATUSFG, THE VALUE IN IBPC-STAT WILL BE
      *                    LN-STATUSFG AND IBPC-STAT-2 WILL CONTAIN " ".
      *
      *
      *        IBPC-EXPIRED-FLAG
      *                    CONTAINS "E" IF TODAY IS PAST EXPIRATION
      *                    AND THE LAST PAYMENT OCCURRED BEFORE
      *                    EXPERATION.
      *                    IF PREVIOUSLY IBPC = "I":
      *                      AN "E" WOULD IMPLY THE PAYMENT MAY REQUIRE
      *                      TWO DIFFERENT RATES. THE RATES WOULD BE THE
      *                      RATE PREVAILING BEFORE EXPIRATION, AND
      *                      THE RATE IN EFFECT AFTER EXPERATION.
      *
      *                    IF PREVIOUSLY IBPC = "P":
      *                      AN "E" WOULD IMPLY THAT LATE CHARGES MAY
      *                      BE DUE AND ANTICIPATED INCOME NEEDS
      *                      TO BE EARNED.
      *                         NOTE: IBPC WILL BE SET TO "I".
      *
      *
      *
      *   USED: LN-LOANTYPE, LN-JDRATE, LN-INTREDU
      *         LN-LSTPAYMNT
      *   COPY TIM, TIMW, LPIBPCW, LPXDTE, LPXDTEW
      *        LPNDTE, LPNDTEW, LPMDTE, LPMDTEW
      * REV:
      *  042888 SLC ADDED THE EXPERATION, JUDGEMENT,
      *             ACCELERATION, AND RATE CHANGE/REDUCTION LOGIC.
      *             ADDED IBPC-EXPIRED-FLAG LOGIC.
      *  051989 JTG CORRECTED CHECK-PREV-STATUS TO INSURE
      *             CORRECT SETTING OF "P" RE: TRISTATE
      *  041097 JTG MADE SP-EXPR-FRMLA 'B' VALID AND ADDED
      *             88  SP-EXPR-FRMLA-ORIG         VALID "B".
      *             88  SP-EXPR-FRMLA-USE-EXPIRED  VALID "A" "B".
      **************************************************************************
       IBPC-TEST SECTION.

           MOVE " " TO IBPC-STAT IBPC-STAT-2 IBPC-EXPIRED-FLAG.
      * FIND THE EXPIRATION DATE.
           PERFORM EXPIRATION-DATE-CALCULATION.

      * NOW FOR THE COMPARISON!
           MOVE XDTE-DATE TO NUM-DATE.
           MOVE IBPC-DATE TO SYS-DATE.
           PERFORM TIM.

      * IF PAYDATE AFTER EXPIRATION,
      * SET THE STAT FLAGS TO 'E':
           MOVE LN-DATE-PAID-LAST TO NUM-DATE.
           IF LN-DATE-PAID-LAST = 0
              MOVE LN-INTDATE TO NUM-DATE.
           IF ELAPSED-DAYS > 0
              MOVE "E" TO IBPC-STAT IBPC-STAT-2
              IF ELAPSED-DAYS > 0
                 MOVE XDTE-DATE TO SYS-DATE
                 PERFORM TIM
      * IF LAST PAYMENT MADE BEFORE EXPIRATION,
      * AND EXPIRATION FORMULA INDICATES NOT TO CONTINUE AS WAS,
      * AND IF ACCOUNT STATUS CHANGED, ACCOUNT WILL USE
      * EXPIRED SITUATION,
      * THAN SET EXPIRED FLAG TO 'E':
                 IF ELAPSED-DAYS > 0
                  IF NOT SP-EXPR-FRMLA-CONT
                         AND (
                              (LN-STATUSFG = " ")
                                 OR (LN-STATUSFG = "J"
                                        AND SP-JUDGFRMLA NOT = " ")
                                 OR (LN-STATUSFG = "A"
                                        AND SP-ACC-FRMLA NOT = " ")
                                 OR (LN-STATUSFG = "R"
                                       AND SP-EXPR-REDU-FRMLA NOT = " ")
                             )
                    MOVE "E" TO IBPC-EXPIRED-FLAG.

           IF IBPC-STAT = "E"
              IF LN-STATUSFG = "J"
                 MOVE LN-JDDATE TO NUM-DATE
                 MOVE XDTE-DATE TO SYS-DATE
                 PERFORM TIM
                 IF ELAPSED-DAYS NOT < 0
                    MOVE "J" TO IBPC-STAT
                 ELSE
                    MOVE "J" TO IBPC-STAT-2
              ELSE
              IF LN-STATUSFG = "A"
                 MOVE LN-ACCELDATE TO NUM-DATE
                 MOVE XDTE-DATE TO SYS-DATE
                 PERFORM TIM
                 IF ELAPSED-DAYS NOT < 0
                    MOVE "A" TO IBPC-STAT
                 ELSE
                    MOVE "A" TO IBPC-STAT-2
              ELSE
              IF LN-STATUSFG = "R"
                 MOVE LN-REDUDATE TO NUM-DATE
                 MOVE XDTE-DATE TO SYS-DATE
                 PERFORM TIM
                 IF ELAPSED-DAYS NOT < 0
                    MOVE "R" TO IBPC-STAT
                 ELSE
                    MOVE "R" TO IBPC-STAT-2
              ELSE
                 MOVE " " TO IBPC-STAT-2
           ELSE
              MOVE LN-STATUSFG TO IBPC-STAT
              MOVE " " TO IBPC-STAT-2.

      * IF INTEREST BEARING (WILL REMAIN SO):
           IF LN-LOANTYPE = "I"
              MOVE "I" TO IBPC-FG
              GO TO IBPC-TEST-EXIT.

      * PRECOMPUTE TEST CONDITIONS:
           MOVE "P" TO IBPC-FG.

      * NOW THAT WE KNOW WHAT IT IS, SET THE FLAG TO 'I' IF NECESSARY.

      * WHEN NO STATUS CHANGE HAS OCCURED ON THE ACCOUNT AND
      * THE ACCOUNT EXPIRED TEST FOR CONTINUED P/C:
           IF IBPC-STAT = "E" AND IBPC-STAT-2 = " "
                 IF SP-EXPR-FRMLA-CONT
                    NEXT SENTENCE
                 ELSE
                    MOVE "I" TO IBPC-FG
           ELSE
      * IF THE ACCOUNT WAS BEFORE OR AFTER EXPIRATION
      * EITHER ACCELERATED OR RATE CHANGED/REDUCED, STATUS IS I/B:
           IF LN-ACCELDATE NOT = 0 OR LN-REDUDATE NOT = 0
              MOVE "I" TO IBPC-FG
           ELSE
      * IF ACCOUNT IS JUDGEMENT AND NOT EXPIRED, TEST
      * LN-JDRATE FOR IB/PC STATUS:
           IF IBPC-STAT = "J" AND IBPC-STAT-2 = " "
              IF LN-JDRATE NOT = 999.999
                 MOVE "I" TO IBPC-FG
              ELSE
                 NEXT SENTENCE
           ELSE
      * IF EXPIRED FIRST AND THAN JUDGEMENT, TEST
      * LN-JDRATE FOR IB/PC STATUS:
           IF IBPC-STAT = "E" AND IBPC-STAT-2 = "J"
              IF SP-EXPR-FRMLA-CONT AND LN-JDRATE = 999.999
                 NEXT SENTENCE
              ELSE
                 MOVE "I" TO IBPC-FG
           ELSE
      *  IF ACCOUNT WAS PREVIOUSLY IN JUDGEMENT AND NOW EXPIRED:
      *  - IF JUDGEMENT WAS I/B STAY I/B:
      *  - IF JUDGEMENT WAS P/C;
      *       IF JUDGEMENT FORMULA SAYS CONTINUE P/C, STAY P/C.
      *       ELSE IF EXPIRED FORMULA SAYS CONTINUE P/C, STAY P/C.
      *            ELSE GO TO I/B:
           IF IBPC-STAT = "J" AND IBPC-STAT-2 = "E"
              IF LN-JDRATE NOT = 999.999
                 MOVE "I" TO IBPC-FG
              ELSE
              IF SP-JUDGFRMLA = " "
                 NEXT SENTENCE
              ELSE
              IF SP-EXPR-FRMLA-CONT
                 NEXT SENTENCE
              ELSE
                 MOVE "I" TO IBPC-FG.

      *  IF THERE IS A TWO RATE SITUATION ACROSS EXPIRATION CHECK TO
      *  SEE IF THE ACCOUNT WAS P/C ASOF THE LAST PAYMENT. IF IT WAS
      *  SET THE IBPC-FG TO "P".

           IF IBPC-EXPIRED-FLAG = "E"
              PERFORM CHECK-PREV-STATUS.

       IBPC-TEST-EXIT.
           EXIT.

