       SETCTBL-EXIT.
           EXIT.
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
