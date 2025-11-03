      * COPYMEMBER: LIBLP/LPBWIN
      **************************************************************
      *              COMPUTE BORROWER INTEREST PAID
      *
      *  NAME   :  LPBWIN
      *  DESC   :  THIS ROUTINE COMPUTES THE INTEREST
      *            A BORROWER CAN REPORT AS PAID ON THEIR
      *            TAX RETURNS. (1098)
      *  IN     :
      *  OUT    :  BWIN-INTPAID
      *  WORKERS:  BWIN-RIAMT BWIN-INTCHG BWIN-APCUR
      *  COPY   :
      *  USED   :  LP-IBPC LP-TRCD LP-APCUR LP-APINT LP-EARNED
      *            LP-CURBAL LN-INTCHG LN-EXTCHG LN-MAINTFEE
      *            LN-SERCHG LN-LNAMT LN-CURBAL LN-ORGTERM
      *            SP-1098FRMLA LN-MAINTFEE REB-AMOUNT(4) (5) (6)
      * REV:
      *  JPN 011988 ADDED LOGIC FOR FORMULA C ACTUARIAL METHOD,
      *             REPLACED ALL OCCURRENCES OF LN-CURBAL WITH
      *                     (LP-CURBAL + LP-APCUR)
      *  JTG 013089 CHANGED LOGIC FOR ALL FORMULA
      *             RE: PO PB RN SC TO USE LN-TOTINTPD
      *                 ALSO STOPPED REBATE AND CANCELS FROM
      *                 ADJUSTING
      *  SLC 011290 IF ACTUARIAL METHOD AND APR = 0, EXIT.
      *  SLC 011790 FIXED SO THAT LP-INTPAID IS CORRECT ON PAYOFFS.
      *             FIXED FORMULA B.
      *  SLC 060190 RULE OF 78THS HAS BEEN ADDED!!
      *  SLC 091790 PRO RATA NOW USES LN-SUM-LNAMT AND LN-SUM-FINCHG
      *  SLC 022891 ADDED OPTION TO INCLUDE LATE CHARGES IN INT PAID.
      *  JTG 012996 CORRECTED  NET-PRECOMP-PAYOFF SECTION, TO CORRECTLY
      *             CLOSE ACCOUNTS OUT IN P&L
      *  JTG 013097 REPLACED ALL OCCURRENCES (LP-CURBAL + LP-APCUR)
      *               WITH  (LP-CURBAL - LP-OT2BAL + LP-APCUR)
      *  JTG 021497 CORRECTED BUG IN RULE78-MAINLOGIC
      *  JTG 061097 CORRECTED NET-PRECOMP-PAYOFF TO HANDLE ADDON INTEREST
      *  JTG 091897 ADDED TEST FOR TRCD = 'AH', WORLD A90
      *   CS 042798 ADDED TEST FOR RENEWAL CODES "RB" AND "RO"
      *  GLF 033099 ADDED TEST FOR TRCD'S = "PN" AND "PZ"
      *  JTG 010424 RULE78-MAINLOGIC NOW USES LN-SUM-LNAMT
      *             ALSO CHANGED TO USE ADDON CHARGES AND PAY SCHEDULES
      *                                                COLONIAL, TN PR#J017
      *  BAH 021014 ADDED NEW TRANSACTION CODE OF "IN" INSURANCE LOSS, EXACTLY
      *             LIKE "PE", REGACC PR# 0007
      *  JTG 030205 ADDED LOGIC TO ACTUARIAL-METHOD TO HANDLE BALLOON
      *             PAYMENTS                                       CITIZEN J025
      *  JTG 040107 FIXED BUG IN LIBLP/LPBWIN FOR RULE 78THS, INTERNAL WORKER
      *             WAS ONLY 9(5)V99 AND WHEN RULE OF 78THS COMPUTATION WAS DONE
      *             ON LARGE FINANCE CHARGES AND LONG TERM, INTEREST
      *             PAID WAS TRUNCATED.                             COLTN  J042
      *   CS 070518 ATTEMPTING TO CORRECT 1098 INTEREST IF IB LOAN.SUNLA
      *             IT LOOKS LIKE THE REBATE ADJUSTMENT SECTION (MF,SF,INT)
      *             ATTEMPTS TO NET A MAX YOU CAN PAY IN INTEREST WHICH IS NOT
      *             NECESSARY ON AN IB LOAN. 1098 INTEREST SHOULD BE WHATS IN
      *             LN-TOTINTPD + LP-EARNED IN THE PMT RECORD OF REBATE?
      *   CS 080208 AFTER MUCH DEBATE RECEIVED PR#617, DO NOT INCLUDE SC,MF
      *             IN 1098 INTEREST PAID...PR#617 ST.LOUIS
      *   CS 080214 FIX THE REBATE ADJUSTMENT SECTION, PR#617
      *   CS 080227 CHANGED TO PERFORM REBATE-ADJUSMENT ONLY IF DOING AN "RI"
      *             PRECOMP LOANS WERE STILL WRONG PR#617
      *   CS 100129 ONLY INCLUDE INTEREST COMPONENT IN 1098 INTEREST PR#671
      *             EXCEPT FOR ACTUARIAL 'C' DISCLOSURE 671
      *  MJD 140306 FIXED A BUG CREATED WITH PR617&PR671.  CALCULATION WAS
      *             USING INTEREST ONLY FOR SP-1098FRMLA = "C" WHEN IT SHOULD
      *             HAVE BEEN USING ALL COMPONENTS WHEN CALCULATING FOR
      *             ACTUARIAL.                                     REGENCY PL834
      *  160113 BAH SPLIT OUT LTFILE, LTPFILE, RENAMED LT- LTP- PD0003
      **************************************************************************
       COMPUTE-BW-INTPAID SECTION.
           MOVE 0 TO BWIN-INTPAID BWIN-EXPINT.

           IF LP-TRCD NOT = "PY"
                   AND NOT = "PA"
                    AND NOT = "PE"
                     AND NOT = "SS"
                      AND NOT = "PO"
                       AND NOT = "PB"
                        AND NOT = "RN"
                         AND NOT = "SC"
                          AND NOT = "RB"
                           AND NOT = "RO"
                            AND NOT = "RD"
                             AND NOT = "DF"
                              AND NOT = "D2"
                               AND NOT = "D3"
                                AND NOT = "D4"
                                 AND NOT = "D5"
                                  AND NOT = "D6"
                                   AND NOT = "D7"
                                    AND NOT = "D8"
                                     AND NOT = "D9"
                                      AND NOT = "RI"
                                       AND NOT = "RS"
                                        AND NOT = "RM"
                                         AND NOT = "AH"
                                          AND NOT = "PN"
                                           AND NOT = "PZ"
                                            AND NOT = "IN"

              GO TO COMPUTE-BWIN-EXIT.

      ****************************************************
      *    NET OUT IF PRECOMP GONE TO IB.
      ****************************************************
           IF LN-LOANTYPE = "P" AND LP-IBPC = "I"
              IF (LP-CURBAL = 0) OR
                 (LP-TRCD = "PO" OR "RN" OR "PB" OR "SC"
                               OR "RB" OR "RO")
                 PERFORM NET-PRECOMP-PAYOFF
                 ADD LP-APINT TO BWIN-INTPAID
                 PERFORM BWIN-INCLUDE-LC
                 GO TO COMPUTE-BWIN-EXIT
              ELSE
                 IF LP-TRCD NOT = "RD"
                    MOVE LP-APINT TO BWIN-INTPAID
                    PERFORM BWIN-INCLUDE-LC
                    PERFORM UNEARNED-OR-IB-METHOD
                    GO TO COMPUTE-BWIN-EXIT
                 ELSE
                    SUBTRACT LP-APCUR FROM BWIN-INTPAID
                    PERFORM UNEARNED-OR-IB-METHOD
                    GO TO COMPUTE-BWIN-EXIT.

           IF LP-TRCD = "RI"
              PERFORM REBATE-ADJUSTMENT
              GO TO COMPUTE-BWIN-EXIT.

           IF LP-TRCD = "RS" OR "RM"
              IF SP-1098FRMLA = "C"
                 PERFORM REBATE-ADJUSTMENT
                 GO TO COMPUTE-BWIN-EXIT.

      ****************************************************
      *    DEFERMENTS
      ****************************************************
           IF LP-TRCD = "DF" OR "D2" OR "D3" OR "D4" OR "D5"
                             OR "D6" OR "D7" OR "D8" OR "D9"
              ADD LP-APINT TO BWIN-INTPAID
              GO TO COMPUTE-BWIN-EXIT.

      ****************************************************
      *    DEFERMENT REBATES
      ****************************************************
           IF LP-TRCD = "RD"
              SUBTRACT LP-APCUR FROM BWIN-INTPAID
              GO TO COMPUTE-BWIN-EXIT.

      ****************************************************
      *    INTEREST BEARING METHOD
      ****************************************************
           IF LP-IBPC = "I"
              ADD LP-APINT TO BWIN-INTPAID
              PERFORM BWIN-INCLUDE-LC
              PERFORM UNEARNED-OR-IB-METHOD
              GO TO COMPUTE-BWIN-EXIT.

      ****************************************************
      *    PRECOMPUTED METHODS
      ****************************************************
      * ?OVER EXPIRATION:
           MOVE LP-APINT TO BWIN-EXPINT.

           IF (LP-CURBAL = 0) OR
                   (LP-TRCD = "PO" OR "RN" OR "PB" OR "SC"
                                 OR "RB" OR "RO")
              PERFORM BWIN-INCLUDE-LC
              PERFORM NET-PRECOMP-PAYOFF
              GO TO COMPUTE-BWIN-EXIT.

      * PRORATA METHOD:
           IF SP-1098FRMLA = "A"
              PERFORM PRO-RATA-PRIN-METHOD
           ELSE
      * UNEARNED METHOD:
           IF SP-1098FRMLA = "B"
              PERFORM UNEARNED-OR-IB-METHOD
           ELSE
      * ACTUARIAL METHOD:
           IF SP-1098FRMLA = "C"
              PERFORM ACTUARIAL-METHOD
           ELSE
      * RULE 78THS METHOD:
           IF SP-1098FRMLA = "D"
              PERFORM RULE78-METHOD.

           PERFORM BWIN-INCLUDE-LC.

       COMPUTE-BWIN-EXIT.
           ADD BWIN-EXPINT TO BWIN-INTPAID.

      *************************************************************************
      *  ACTUARIAL METHOD
      *
      *  JTG 030205 ADDED LOGIC TO ACTUARIAL-METHOD TO HANDLE BALLOON
      *             PAYMENTS                                       CITIZEN J025
      *************************************************************************
       ACTUARIAL-METHOD SECTION.
           IF LN-APRATE = 0
              GO TO ACTUARIAL-METHOD-EXIT.

           IF LP-APCUR NOT = 0
              IF LP-TRCD = "PY" OR "PA" OR "PE" OR "SS" OR "AH"
                                OR "PN" OR "PZ" OR "IN"
                 MOVE LP-APCUR TO BWIN-INTPAID-WORKER
                 PERFORM COMPUTE-ACTUARIAL-REBATE.

       ACTUARIAL-METHOD-EXIT.
           EXIT.

      **************************************************
       COMPUTE-ACTUARIAL-REBATE SECTION.
      **************************************************
           MOVE BWIN-INTPAID-WORKER TO BWIN-APCUR.

      * COMPUTE THE TOTAL PRINCIPAL REDUCTION THRU THIS PAYMENT:
           COMPUTE BWIN-WORKER = LN-LNAMT
               - (LP-CURBAL - LP-OT2BAL + LP-APCUR) + BWIN-APCUR.

      * DETERMINE NO. OF WHOLE AND FRACTIONAL PAYMENTS:
           PERFORM GET-NO-OF-PMTS-NORMAL.

      * COMPUTE REBATE:
            PERFORM REBATE-ACTUARIAL.
            MOVE RBACT-REBATE TO BWIN-WORKER.

           COMPUTE BWIN-INTPAID =
              LN-FINCHG - RBACT-REBATE - (LN-TOTINTPD - LN-TOTDEF).

      ****************************************************************
       REBATE-ACTUARIAL SECTION.
      ****************************************************************
           MOVE LN-ORIG-1STPYDATE      TO RBACT-ORIG-1STPAY.
           MOVE LN-INTDATE          TO RBACT-INTDATE.
           MOVE LN-APRATE           TO RBACT-RATE.
           MOVE LN-TOTNODEF         TO RBACT-TOTNODEF.
           MOVE LN-ORGTERM          TO RBACT-ORGTERM.
           MOVE LN-1STPYAMT         TO RBACT-1STPYAMT.
           MOVE LN-REGPYAMT         TO RBACT-REGPYAMT.
           MOVE LN-LASTPYAMT        TO RBACT-LASTPYAMT.
           MOVE LN-UNITPER-CD       TO RBACT-UNITPER-CD.
           MOVE LN-UNITPER-FREQ     TO RBACT-UNITPER-FREQ.
           MOVE LN-FINCHG           TO RBACT-TOTCHG.

      * SETUP REBATE DATE:
           MOVE LN-INTDATE          TO NDTE-DATE.
           MOVE BWIN-WPAYS          TO NDTE-HOLD.
           PERFORM INCREMENT-UNITPER.
           COMPUTE NDTE-HOLD = BWIN-PPAYS * LN-DAYS-IN-UNITPER.
           PERFORM INCREMENT-DAYS.
           MOVE NDTE-DATE           TO RBACT-RBDATE.

           MOVE LN-INTDATE          TO NUM-DATE.
           MOVE RBACT-RBDATE        TO SYS-DATE.
           PERFORM TIMALL.
           MOVE ELAPSED-MONTHS      TO RBACT-ELAPSED-MONTHS.
           MOVE ELAPSED-REM         TO RBACT-ELAPSED-REM.
           MOVE ELAPSED-DAYS        TO RBACT-ELAPSED-DAYS.

           MOVE " "                 TO RBACT-SKIPDEF-FG.
           MOVE " "                 TO RBACT-EARN-FULL-MONTH.
           MOVE 7                   TO RBACT-SUB.
           MOVE 1                   TO RBACT-SUB2.
           MOVE "Y"                 TO RBACT-PENALTY-FG.

           PERFORM RBACT-ACTUARIAL.

       REBATE-ACTUARIAL-EXIT.
           EXIT.

      *******************************************
      *    RULE 78THS METHOD
      *******************************************
       RULE78-METHOD SECTION.
           IF LP-APCUR = 0
              GO TO RULE78-METHOD-EXIT.

           MOVE 0 TO BWIN-APCUR.
           PERFORM RULE78-MAINLOGIC.
           MOVE BWIN-INTPAID TO BWIN-INTPAID-SAV.
           MOVE LP-APCUR TO BWIN-APCUR.
           PERFORM RULE78-MAINLOGIC.
           SUBTRACT BWIN-INTPAID FROM BWIN-INTPAID-SAV
                                        GIVING BWIN-INTPAID.
       RULE78-METHOD-EXIT.
           EXIT.

      *-----------------------------------------------
      *    RULE OF 78THS MAIN LOGIC
      *-----------------------------------------------
       RULE78-MAINLOGIC SECTION.
      * COMPUTE THE TOTAL PRINCIPAL REDUCTION THRU THIS PAYMENT:
           COMPUTE BWIN-WORKER = LN-SUM-LNAMT
               - (LP-CURBAL - LP-OT2BAL + LP-APCUR) + BWIN-APCUR.

           IF LN-PAY-SCHLD-FG = "Y"
              PERFORM GET-NO-OF-PMTS-ADDON
           ELSE
              PERFORM GET-NO-OF-PMTS-NORMAL.

      * SETUP RULE OF 78THS UNIT CHARGE:
           MOVE 0 TO BWIN-INTCHG.
           IF LN-ANTICADJ(1) = 0
              IF (LN-REBATE(4) = " " OR "C")
                 IF LN-ANTICERN(1) NOT = (LN-INTCHG + LN-EXTCHG)
                    COMPUTE BWIN-INTCHG =
                         BWIN-INTCHG + LN-INTCHG + LN-EXTCHG
                 ELSE
                    ADD LN-ANTICERN(1) TO BWIN-INTCHG.
           IF LN-ANTICADJ(2) = 0
              IF (LN-REBATE(4) = " " OR "C")
                  ADD LN-ANTICERN(2) TO BWIN-INTCHG.
      *%   IF LN-ANTICADJ(3) = 0
      *%      IF (LN-REBATE(5) = " " OR "C")
      *%         IF LN-ANTICERN(3) NOT = LN-SERCHG
      *%            COMPUTE BWIN-INTCHG =
      *%                 BWIN-INTCHG + LN-SERCHG
      *%         ELSE
      *%            ADD LN-ANTICERN(3) TO BWIN-INTCHG.
      *%   IF LN-ANTICADJ(4) = 0
      *%      IF (LN-REBATE(6) = " " OR "C")
      *%         IF LN-ANTICERN(4) NOT = (LN-ORGTERM * LN-MAINTFEE)
      *%            COMPUTE BWIN-INTCHG =
      *%                 BWIN-INTCHG + (LN-ORGTERM * LN-MAINTFEE)
      *%         ELSE
      *%            ADD LN-ANTICERN(4) TO BWIN-INTCHG.

           COMPUTE BWIN-WORKER =
                     (LN-SUM-ORGTERM * (LN-SUM-ORGTERM + 1)) / 2.
           IF BWIN-WORKER NOT = 0
              COMPUTE BWIN-WORKER ROUNDED =
                         BWIN-INTCHG  / BWIN-WORKER.

      * COMPUTE INT. PAID THRU THIS PAYMENT:
           COMPUTE BWIN-INTPAID ROUNDED = BWIN-WORKER *
              (LN-SUM-ORGTERM - BWIN-PAYMENTS) *
                   (LN-SUM-ORGTERM - BWIN-PAYMENTS + 1) / 2.

       RULE78-MAINLOGIC-EXIT.
           EXIT.

      *---------------------------------------------------------
      * NORMAL PAYMENT SCHEDULE
      * DETERMINE NO. OF WHOLE AND FRACTIONAL PAYMENTS:
      *---------------------------------------------------------
      *  EXAMPLE:
      *                       TERM = 1     TERM = 2     TERM = 6
      *                     -----------  -----------  -----------
      *   LN-1STPYAMT    :  $100.00      $100.00      $100.00
      *   LN-REGPYAMT    :  $100.00 X 0  $120.00 X 0  $120.00 X 4
      *   LN-LASTPYAMT   :  $  0.00      $120.00      $130.00
      *                      ------       ------       ------
      *      TOTAL REPAY :  $100.00      $220.00      $710.00
      *---------------------------------------------------------
       GET-NO-OF-PMTS-NORMAL SECTION.
           MOVE 0 TO BWIN-PAYMENTS.
           IF BWIN-WORKER NOT < LN-1STPYAMT
              SUBTRACT LN-1STPYAMT FROM BWIN-WORKER
              MOVE 1 TO BWIN-PAYMENTS
           ELSE
              COMPUTE BWIN-PAYMENTS ROUNDED =
                      BWIN-WORKER / LN-1STPYAMT
              MOVE 0           TO BWIN-WORKER.
           IF LN-NOREGPY = 0
              COMPUTE BWIN-PAYMENTS ROUNDED =
                        BWIN-PAYMENTS + (BWIN-WORKER / LN-REGPYAMT)
           ELSE
              IF BWIN-WORKER NOT < (LN-NOREGPY * LN-REGPYAMT)
                 COMPUTE BWIN-PAYMENTS ROUNDED =
                        BWIN-PAYMENTS + LN-NOREGPY
                          + (BWIN-WORKER - (LN-NOREGPY * LN-REGPYAMT))
                                         / LN-LASTPYAMT
              ELSE
                 COMPUTE BWIN-PAYMENTS ROUNDED =
                       BWIN-PAYMENTS + BWIN-WORKER / LN-REGPYAMT.

      *---------------------------------------------------------
      * ADDON PAYMENT SCHEDULE
      * DETERMINE NO. OF WHOLE AND FRACTIONAL PAYMENTS:
      *---------------------------------------------------------
       GET-NO-OF-PMTS-ADDON SECTION.
           MOVE 0 TO BWIN-PAYMENTS
                     NDTE-HOLD.
           PERFORM GET-LT-P-REC.
           IF IO-FG = 9
              STRING LN1-KEY,"-","MISSING PAYMENT SCHEDULE"
                 DELIMITED BY "  " INTO MESS
              PERFORM SEND-MESS
              GO TO IO-ERROR
           ELSE
              MOVE 1 TO BWIN
              PERFORM GET-NO-OF-PMTS-ADDON-SUM
                 UNTIL BWIN > 17 OR BWIN-WORKER = 0.
           MOVE NDTE-HOLD TO BWIN-WPAYS.
           MOVE 0 TO NDTE-HOLD.

       GET-NO-OF-PMTS-ADDON-SUM SECTION.
           IF LTP-SCHLD-TERM(BWIN) = 0
              MOVE 18 TO BWIN
              GO TO GET-NO-OF-PMTS-ADDON-SUM-EXIT.

           COMPUTE BWIN-INTPAID-WORKER =
              LTP-SCHLD-PAYMNT(BWIN) * LTP-SCHLD-TERM(BWIN).
           IF BWIN-WORKER > BWIN-INTPAID-WORKER
              ADD LTP-SCHLD-TERM(BWIN) TO NDTE-HOLD
              SUBTRACT BWIN-INTPAID-WORKER FROM BWIN-WORKER
              GO TO GET-NO-OF-PMTS-ADDON-SUM-EXIT.

           MOVE 0 TO BWIN-CNT.
       GET-NO-OF-PMTS-ADDON-SUM-NEXT.
           IF BWIN-CNT < LTP-SCHLD-TERM(BWIN)
              IF BWIN-WORKER > LTP-SCHLD-PAYMNT(BWIN)
                 SUBTRACT LTP-SCHLD-PAYMNT(BWIN) FROM BWIN-WORKER
                 ADD 1 TO NDTE-HOLD BWIN-CNT
                 GO TO GET-NO-OF-PMTS-ADDON-SUM-NEXT
              ELSE
                 COMPUTE BWIN-PAYMENTS ROUNDED =
                    BWIN-WORKER / LTP-SCHLD-PAYMNT(BWIN)
                 ADD BWIN-WPAYS TO NDTE-HOLD
                 MOVE 0 TO BWIN-WORKER
              END-IF
           ELSE
              MOVE 0 TO BWIN-WORKER.

       GET-NO-OF-PMTS-ADDON-SUM-EXIT.
           ADD 1 TO BWIN.

      *******************************************
      *    PRO RATA METHOD
      *******************************************
       PRO-RATA-PRIN-METHOD SECTION.
           IF LP-APCUR NOT = 0
              IF LP-TRCD = "PY" OR "PA" OR "PE" OR "SS" OR "AH"
                                OR "PN" OR "PZ" OR "IN"
                 IF LN-SUM-LNAMT NOT = 0
                    IF SP-1098FRMLA = "C"
                       COMPUTE BWIN-INTPAID ROUNDED =
                          (LP-APCUR * LN-SUM-FINCHG) / LN-SUM-LNAMT
                    ELSE
                       COMPUTE BWIN-INTPAID ROUNDED =
                          (LP-APCUR * LN-SUM-INTCHG) / LN-SUM-LNAMT.

      *******************************************
      *    UNEARNED OR IB METHOD
      *******************************************
       UNEARNED-OR-IB-METHOD SECTION.
           MOVE LN-EARN-ORDER(1) TO SUB.
           PERFORM ADD-TO-BWIN-INTPAID.

           IF SP-1098FRMLA = "C"
              MOVE LN-EARN-ORDER(3) TO SUB
              PERFORM ADD-TO-BWIN-INTPAID
              MOVE LN-EARN-ORDER(4) TO SUB
              PERFORM ADD-TO-BWIN-INTPAID.

      *******************************************
      *    NET OUT INTEREST PAID ON PAYOFF
      * TEST SP-1098FRMLA:
      *    WHEN "C" INTEREST IN BASED OFF OF FIN CHARGE AND ALL COMPONENTS
      *    WHEN OTHER (A, B, D) USE INTEREST COMPONENT ONLY.
      *    USING LN-SUM-FINCHG HERE. "C" DOES NOT ALLOW ADDONS SO IT WILL
      *    NOT AFFECT THE CALCULATION. I WANT TO BE CONSISTANT EVERYWHERE |MJD
      *
      *******************************************
       NET-PRECOMP-PAYOFF SECTION.
           IF SP-1098FRMLA = "C"
              COMPUTE BWIN-INTPAID = BWIN-INTPAID +
                   LN-SUM-FINCHG - ( LN-ANTICADJ(1) + LN-ANTICADJ(2)
                                   + LN-ANTICADJ(3) + LN-ANTICADJ(4)
                                   + REB-AMOUNT(4) + REB-AMOUNT(5)
                                   + REB-AMOUNT(6) )
                   + LN-TOTINT
                   + LN-TOTDEF - REB-AMOUNT(7)
                   - LN-TOTINTPD
           ELSE
              COMPUTE BWIN-INTPAID = BWIN-INTPAID +
                   LN-SUM-INTCHG  - ( LN-ANTICADJ(1) + LN-ANTICADJ(2)
                                                 + REB-AMOUNT(4))
                   + LN-TOTINT
                   + LN-TOTDEF - REB-AMOUNT(7)
                   - LN-TOTINTPD.



      *    IF NOT (LN-PLCD = " " AND LN-PLDATE = 0)
      *       COMPUTE BWIN-INTPAID = BWIN-INTPAID
      *          - (LN-FINCHG - LN-ACCERN(1) - LN-ACCERN(3)
      *                                       - LN-ACCERN(4)).

           IF SP-1098-INC-LC = "Y"
              ADD LN-TOTLCHG TO BWIN-INTPAID.


           IF LP-TRCD = "RI"
              COMPUTE BWIN-INTPAID = BWIN-INTPAID + REB-AMOUNT(4) -
                                     LP-TRAMT.

      * COMPONENT INTEREST IS ONLY COMPUTED WHEN ACTUARIAL "C"
      * OTHER FORMULAS ARE INTEREST COMPONENT ONLY.
           IF SP-1098FRMLA = "C"
              IF LP-TRCD = "RS"
                 COMPUTE BWIN-INTPAID = BWIN-INTPAID + REB-AMOUNT(5) -
                                        LP-TRAMT
              ELSE
              IF LP-TRCD = "RD"
                 COMPUTE BWIN-INTPAID = BWIN-INTPAID + REB-AMOUNT(7) -
                                        LP-TRAMT
              ELSE
              IF LP-TRCD = "RM"
                 COMPUTE BWIN-INTPAID = BWIN-INTPAID + REB-AMOUNT(6) -
                                        LP-TRAMT.


      *******************************************
      *    REBATE OF CHARGES (ADJUSTMENTS)
      *
      * TEST SP-1098FRMLA:
      *    WHEN "C" INTERST IN BASED OFF OF FIN CHARGE AND ALL COMPONENTS
      *    WHEN OTHER (A, B, D) USE INTEREST COMPONENT ONLY.
      *    USING LN-SUM-FINCHG HERE. "C" DOES NOT ALLOW ADDONS SO IT WILL
      *    NOT AFFECT THE CALCULATION. I WANT TO BE CONSISTANT EVERYWHERE |MJD
      *
      *******************************************
       REBATE-ADJUSTMENT SECTION.
           IF LN-LOANTYPE = "I"
              GO TO SKIP-TO-IB
           ELSE
           IF SP-1098FRMLA NOT = "B"
              IF SP-1098FRMLA = "C"
                 COMPUTE BWIN-INTPAID =
                      LN-SUM-FINCHG - (LN-ANTICADJ(1) + LN-ANTICADJ(3)
                                  + LN-ANTICADJ(4) + REB-AMOUNT(4)
                                   + REB-AMOUNT(5) + REB-AMOUNT(6))
                    + LN-TOTINT
                    + LN-TOTDEF - REB-AMOUNT(7)
                    - (LN-ANTICERN(1) - LN-ACCERN(1))
                    - (LN-ANTICERN(3) - LN-ACCERN(3))
                    - (LN-ANTICERN(4) - LN-ACCERN(4))
                    - LN-TOTINTPD
              ELSE
                 COMPUTE BWIN-INTPAID =
                      LN-SUM-INTCHG - (LN-ANTICADJ(1) +LN-ANTICADJ(2)
                                             + REB-AMOUNT(4))
                    + LN-TOTINT
                    + LN-TOTDEF - REB-AMOUNT(7)
                    - (LN-ANTICERN(1) - LN-ACCERN(1))
                    - (LN-ANTICERN(2) - LN-ACCERN(2))
                    - LN-TOTINTPD.

       SKIP-TO-IB.
           IF SP-1098-INC-LC = "Y"
                ADD LN-TOTLCHG TO BWIN-INTPAID.

      *%PL671 ONLY INTEREST                                                     
      * PL834 COMPONENTS WERE NOT TAKING INTO ACCOUNT ADDONSS, ADDED BUCKET 2   
      *       TO CAPTURE ADDON INTEREST.                                        
      *       SP-1098FRMLA = "C" WILL HIT THIS BUT ADDONS ARE NOT ALLOWED       
      *       WHEN FOR THIS FORMULA SO WE ARE NOT TESTING IT HERE.              
                                                                                
           IF LP-TRCD = "RI"                                                    
              MOVE LN-EARN-ORDER(1) TO SUB                                      
              PERFORM ADD-TO-BWIN-INTPAID                                       
              MOVE LN-EARN-ORDER(2) TO SUB                                      
              PERFORM ADD-TO-BWIN-INTPAID                                       
           ELSE                                                                 
            IF SP-1098FRMLA = "C"                                               
               IF LP-TRCD = "RS"                                                
                  MOVE LN-EARN-ORDER(3) TO SUB                                  
                  PERFORM ADD-TO-BWIN-INTPAID                                   
               ELSE                                                             
                IF LP-TRCD = "RM"                                               
                   MOVE LN-EARN-ORDER(4) TO SUB                                 
                   PERFORM ADD-TO-BWIN-INTPAID                                  
                ELSE                                                            
                 IF LP-TRCD = "RD"                                              
                    COMPUTE BWIN-INTPAID = BWIN-INTPAID                         
                                  + REB-AMOUNT(7) - LP-TRAMT.

      *******************************************
      *    ADD EARNINGS TO INTEREST PAID
      *******************************************
       ADD-TO-BWIN-INTPAID SECTION.
           IF SUB > 3
      *       ADD WK-LXE-EARN(SUB - 3) TO BWIN-INTPAID
              ADD LXE-EARN(SUB - 3)    TO BWIN-INTPAID
           ELSE
              ADD LP-EARNED(SUB) TO BWIN-INTPAID.

      *******************************************
      *    TEST TO INCLUDE LATE CHARGES
      *******************************************
       BWIN-INCLUDE-LC SECTION.
           IF SP-1098-INC-LC = "Y"
              ADD LP-APLC TO BWIN-INTPAID.

      *-----------------------------------------------------------------------
      *    DUMMY SECTION TO INCLUDE IN COPY MEMBERS THAT NEED TO BE
      *    REFLECTED IN 'WHAT' COMMANDS FOR SUPPORT
      *-----------------------------------------------------------------------
       SCCS-COPY-ROUTINE SECTION. MOVE
           "@(#)./LIBLP/LPBWIN.CPY S35 >"
              TO MESS.

      *-----------------------------------------------------------------------

