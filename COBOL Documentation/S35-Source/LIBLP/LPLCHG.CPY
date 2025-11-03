      * COPYMEMBER: LIBLP/LPLCHG
      ***********************************************************************
      *   NAME: LPLCHG - LATE CHARGE CALCULATION
      *   DESC: THIS ROUTINE DETERMINES HOW MUCH
      *         TO CHARGE WHEN A LATE CHARGE IS
      *         ASSESED FOR A DELINQ.PAYMENT.
      *   IN  : LCHG-LATE (WHEN LCTYPE = C),
      *                   ITH DOLLARS LATE
      *         LCHG-LCPDTH-DATE (ADDED 070187 AMW)
      *         LCHG-CURPAY  CURRENT PAYMENT DUE IF USING PMT SCHD
      *                      OR SP-LCTYPE = 'M'.
      *         LCHG-DAYS-AFTER-DUE (ADDED 030399 GLF)
      *   OUT : LCHG-CHARGE
      *   USED: LN-REGPYAMT, SP-LCFRMLA, SP-LCTYPE
      *         SP-LCRATE, SP-LCMINCHG, SP-LCMAXAMT, LN-MAINTFEE
      *         LN-1STPYDA LN-1STPYDATE
      *   COPY: LPLCHGW
      *
      * REV:
      *  051286 JTG REDUCED REGPYAMT BY MAINT-FEES
      *  072286 JTG OPENED FORMULA "C" TO THIS ROUTINE
      *  052987 JTG OPENED FORMULA "D" TO THIS ROUTINE
      *  063087 AMW ADD LCTYPE OF E,F,G,H,I.
      *  090689 JTG ADDED LCTYPE 'J'
      *  061290 JTG ADDED LCTYPE 'K' AND 'L' (PACESETTER)
      *  091390 SLC ADDED LCHG-CURPAY, WHICH IS CURRENT PAYMENT IF
      *             LOAN HAS A PAY SCHD RECORD.
      *  060592 JTG ADDED LOGIC FOR LN-LC-CODE (ROSE SHANIS)
      *  062092 JTG ADDED LOGIC FOR LCTYPE 'M' (COLONIAL TN)
      *  080495 KEC REMOVED SP-LC-CODE FROM CALL "CL/ELREAD" USING
      *  111595 BAH ADDED SP-LCTYPE 'N' (MERCURY, TN)
      *  020296 BAH ADDED SP-LCTYPE 'O' (EAST TENNESSEE)
      *  103096 JTG CHANGED TEST ON LN-LC-CODEX TO TEST FOR "00", WHICH
      *             SHOULD ELIMINATE UNNECESSARY IO ON EL-FILE
      *  050797 JTG CHANGED CALL TO ELREAD TO PASS SP-LCUNITPER-CD
      *  122198 JTG ADDED SP-LCTYPE = 'P'  OHIO PRIOR 07/12/96, REGENCY #002
      *  030399 GLF ADDED LOGIC FOR LCHG-DAYS-AFTER-DUE 'Q' (MULLEN)
      *  040331 BAH ADDED SP-LC-ON-JUDGEMENTS FOR RIVERSIDE, PR# 2272, STATE
      *             LAW SAYS NO LATE CHARGES ON JUDGEMENT ACCOUNTS
      *  JTG 050512 ADDED LOGIC TO LET SP-LCUNITPER-CD = "A" WORK
      *             WHEN SP-LCTYPE = 'C', 'K', 'M' OR 'O'       CREATIVE #491
      *  BLM 100219 CHANGED THIS SETTING OF LCHG-WORK TO NOT INCLUDE ESCROW,
      *             CHANGED THIS LOGIC (OLD):
      *
      *    IF SP-LCTYPE = "A" OR "E" OR "F"
      *       IF LN-PAY-SCHLD-FG = "Y"
      *          SUBTRACT LN-MAINTFEE FROM LCHG-CURPAY GIVING LCHG-WORK
      *       ELSE
      *          SUBTRACT LN-MAINTFEE FROM LN-REGPYAMT GIVING LCHG-WORK.
      *                     TO THIS LOGIC (NEW):
      *
      *    IF SP-LCTYPE = "A" OR "E" OR "F"
      *       IF (LN-PAY-SCHLD-FG = "Y" AND LN-ESCROW-FG NOT = "Y" AND
      *                                     LN-ESCROW-FG NOT = "I")
      *          SUBTRACT LN-MAINTFEE FROM LCHG-CURPAY GIVING LCHG-WORK
      *       ELSE
      *          SUBTRACT LN-MAINTFEE FROM LN-REGPYAMT GIVING LCHG-WORK.
      * KEC 190219 REMOVED REDEFINE FOR LN-LC-CODEX; UNUSED
      *            REMOVED REDEFINE FOR SP-LC-CODEX; UNUSED
      ***********************************************************************
       LATE-CHARGE-CALCULATION SECTION.
           MOVE 0 TO LCHG-CHARGE.

           IF ( LN-LC-CODE NOT = ZEROES )
              PERFORM LATE-CHARGE-TEST-ELREAD.

           IF SP-LCFRMLA = "A" OR " "
              GO TO LATE-CHARGE-CALC-EXIT.

      * RIVERSIDE PR# 2272 3/31/04
           IF SP-LC-ON-JUDGEMENTS = "N"
              IF LN-JDDATE NOT = 0
                 GO TO LATE-CHARGE-CALC-EXIT.

           MOVE LN-UNITPER-CD TO UPWK-UNITPER-CD.
           MOVE LN-UNITPER-FREQ TO UPWK-UNITPER-FREQ.
           PERFORM UPWK-LC-CALC.

      ***************************************************
      *    "B" HAS THE FIXED LATE CHARGE IN SP-LCRATE
      ***************************************************
           IF SP-LCTYPE = "B"
              MOVE SP-LCRATE TO LCHG-CHARGE
              GO TO LATE-CHARGE-CALC-END.

      ***************************************************
      *    "A", "E", AND "F" USE REGULAR PAYMENT AMOUNT
      ***************************************************
           IF SP-LCTYPE = "A" OR "E" OR "F"
              IF (LN-PAY-SCHLD-FG = "Y" AND LN-ESCROW-FG NOT = "Y" AND
                                            LN-ESCROW-FG NOT = "I")
                 SUBTRACT LN-MAINTFEE FROM LCHG-CURPAY GIVING LCHG-WORK
              ELSE
                 SUBTRACT LN-MAINTFEE FROM LN-REGPYAMT GIVING LCHG-WORK.

      *****************************************************************
      *    "G", "H", AND "I" USE PROPER PAYMENT AMOUNT (ODD OR REGULAR)
      *****************************************************************
           IF SP-LCTYPE = "G" OR "H" OR "I"
              MOVE LN-REGPYAMT TO LCHG-WORK
              IF LN-1STPYAMT NOT = 0
                 MOVE LCHG-LCPDTH-DATE TO NDTE-DATE
                 SUBTRACT LN-TOTNODEF FROM 0 GIVING NDTE-HOLD
                 PERFORM INCREMENT-UNITPER
                 MOVE LN-1STPYDATE TO NUM-DATE
                 MOVE NDTE-DATE    TO SYS-DATE
                 IF SYS-DATE < NUM-DATE
                    MOVE LN-1STPYAMT TO LCHG-WORK.

      *****************************
      *    "E" AND "H" TRUNCATE
      *****************************
           IF SP-LCTYPE = "E" OR "H"
              MOVE 0 TO LCHG-DEC.

      *****************************
      *    "F" AND "I" ROUND UP
      *****************************
           IF SP-LCTYPE = "F" OR "I"
              IF LCHG-DEC > 0
                 MOVE 0 TO LCHG-DEC
                 ADD 1 TO LCHG-TRUNC.

      ***********************************************************
      *    CALCULATE LATE CHARGE FOR A,E,F,G,H,I  LC-TYPES
      ***********************************************************
           IF SP-LCTYPE = "A" OR "E" OR "F" OR "G" OR "H" OR "I"
              COMPUTE LCHG-CHARGE ROUNDED =
                       (LCHG-WORK * SP-LCRATE * 0.01
                                 * UPWK-LC-PER-FAC * 0.01)
              GO TO LATE-CHARGE-CALC-END.

      ************************************************************************
      *    "Q" FLAT CHARGE     MULLEN #804
      *  NOTE:
      *        CHARGE $15.00 IF PAYMENT IS MADE 5 DAYS PAST SPR GRACE PERIOD
      *        CHARGE $10.00 IF PAYMENT IS MADE PAST GRACE PERIOD
      ************************************************************************
           IF SP-LCTYPE = "Q"
              IF ( LCHG-DAYS-AFTER-DUE - SP-LCGRACE ) > 4
                 MOVE 15.00 TO LCHG-CHARGE
              ELSE
                 MOVE 10.00 TO LCHG-CHARGE.

      ***********************************************************
      *    CALCULATE LATE CHARGE FOR P  REGENCY, OHIO PRIOR TO
      *    0712/96 LC-TYPES #002
      ***********************************************************
           IF SP-LCTYPE = "P"
              IF LN-ORGTERM NOT = 0
                 COMPUTE LCHG-CHARGE ROUNDED =
                  ( LN-ANTICERN(1) - LN-UFERN(1) )
                         * (
                             2 / ( LN-ORGTERM * (LN-ORGTERM + 1) )
                           )
              GO TO LATE-CHARGE-CALC-END.

      ***********************************************************
      *    CALCULATE LATE CHARGE FOR C, J, K, L, M & N LC-TYPES
      * CALCULATED ON AMOUNT DELINQUENT
      ***********************************************************
           IF NOT (SP-LCTYPE = "C" OR "J" OR "K" OR "L")
              IF NOT (SP-LCTYPE = "M" OR "N" OR "O")
                 GO TO LATE-CHARGE-CALC-EXIT.

           IF SP-LCTYPE = "K" OR "L"
              IF LCHG-LATE > LN-REGPYAMT
                 MOVE LN-REGPYAMT TO LCHG-LATE.

           IF SP-LCTYPE = "M"
              SUBTRACT LN-MAINTFEE FROM LCHG-CURPAY
                                          GIVING LCHG-WORK
              IF LCHG-LATE > LCHG-WORK
                 MOVE LCHG-WORK TO LCHG-LATE.

           IF SP-LCTYPE = "O"
              IF LCHG-LATE > (LN-REGPYAMT - LN-MAINTFEE)
                 COMPUTE LCHG-LATE = LN-REGPYAMT - LN-MAINTFEE.

      * A FIXED PERCENT OF AMOUNT THAT IS DELINQUENT LESS MAINTFEE
      * TRUNCATED TO WHOLE DOLLARS
           IF SP-LCTYPE = "N"
              COMPUTE LCHG-TRUNC = LCHG-LATE - LN-MAINTFEE
              COMPUTE LCHG-CHARGE ROUNDED =
                            LCHG-TRUNC * SP-LCRATE /100.

           IF SP-LCTYPE = "C" OR "K" OR "M" OR "O"
              COMPUTE LCHG-CHARGE ROUNDED =
                   LCHG-LATE * SP-LCRATE * 0.01
                             * UPWK-LC-PER-FAC * 0.01.

           IF SP-LCTYPE = "J" OR "L"
              MOVE LCHG-LATE TO LCHG-TRUNC
              COMPUTE LCHG-CHARGE ROUNDED =
                            LCHG-TRUNC * SP-LCRATE / 100.

      ********************************************************
      *    COMPARE LATE CHARGE AGAINST MINIMUM AND MAXIMUM
      ********************************************************
       LATE-CHARGE-CALC-END.
           IF SP-LCUNITPER-CD NOT = "A"
              GO TO LATE-CHARGE-CALC-NORMAL-END.

           MULTIPLY .01 BY UPWK-LC-MIN-FAC.
           MULTIPLY .01 BY UPWK-LC-MAX-FAC.
           IF LCHG-CHARGE < (SP-LCMINCHG * UPWK-LC-MIN-FAC)
              MULTIPLY SP-LCMINCHG BY UPWK-LC-MIN-FAC
                                      GIVING LCHG-CHARGE.
           IF LCHG-CHARGE > (SP-LCMAXAMT * UPWK-LC-MAX-FAC)
              MULTIPLY SP-LCMAXAMT BY UPWK-LC-MAX-FAC
                                      GIVING LCHG-CHARGE.
           GO TO LATE-CHARGE-CALC-EXIT.

       LATE-CHARGE-CALC-NORMAL-END.
           IF LCHG-CHARGE < SP-LCMINCHG
              MOVE SP-LCMINCHG TO LCHG-CHARGE.
           IF LCHG-CHARGE > SP-LCMAXAMT
              MOVE SP-LCMAXAMT TO LCHG-CHARGE.

       LATE-CHARGE-CALC-EXIT.
           EXIT.

      ****************************************************************
      *  NOTE:
      *        IF ELREAD.C DETECTS AN INVALID CODE
      *        DUE TO THE OPTION NOT BEENING PURCHASED
      *        AND FX/ELCLR.C NOT HAVING BEEN RUN,
      *
      *        THE SPR FIELDS:
      *          SP-LC-INFO & SP-LCUNITPER-CD  ARE NOT CHANGED
      *        AND
      *          SP-LC-CODE WILL BE SET TO 0 CAUSING NO LATE CHARGES
      *
      ****************************************************************
       LATE-CHARGE-TEST-ELREAD SECTION.
           IF LN-LC-CODE NOT = SP-LC-CODE
              MOVE LN-LC-CODE TO SP-LC-CODE
              MOVE "CL/ELREAD" TO FORM-NAM
              CALL FORM-PROGX USING SP-LC-INFO SP-LCUNITPER-CD
              CANCEL FORM-PROGX.

