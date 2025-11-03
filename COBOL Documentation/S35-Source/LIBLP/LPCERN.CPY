      * COPYMEMBER: LIBLP/LPCERN
      ***********************************************************************
      *         COMPUTE PAYMENT POSTING ANTICIPATED EARNINGS
      *VVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVV
      *    NEEDS WORK FOR BALLOON & UNIT PERIODS
      *^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
      *
      *   NAME: LPCERN
      *   DESC: THIS ROUTINE COMPUTES EARNINGS THRU
      *         A PERIOD IN TIME, BASED ON EARNINGS
      *         ALREADY TAKEN.
      *
      *   NEW   OLD
      *   ---  ----
      *     1  (01) - REBATE FORMULA USING PAYDATE
      *     2  (02) - SAME AS 1 WITH UPFRONT EARNINGS
      *     3  (03) - REBATE FORMULA USING PAID THRU
      *     4  (04) - SAME AS 3 WITH UPFRONT EARNINGS
      *     5  (10) - PRORATA PURCHASE BALANCE
      *     6  (17) - ACTUARIAL PRINCIPAL REDUCTION
      *     7  (18) - RULE OF 78THS VIA TIME
      *     8  (19) - RULE OF 78THS VIA PRINCIPAL REDUCTION
      *     9  (20) - PRORATE PURCHASE BALANCE (WHOLE MONTHS)
      *    10       - RULE OF 78THS VIA PRINCIPAL REDUCTION WHOLE
      *               PAYMENTS ONLY, BASED ON CONTRACTUAL PERCENTAGE.
      *    11       - ACTUARIAL PRINICPAL REDUCTION.
      *               A PERCENTAGE (DEALER DISCOUNT TO THE AMOUNT
      *               OF INTEREST CHARGEABLE) OF THE TRUE PRINCIPAL
      *               REDUCTION OF INTEREST CHARGEABLE
      *    12       - ACTUARIAL BASED ON TIME OF PAYMENT (PURCHASED DATE)
      *    13       - ACTUARIAL PRINICPAL REDUCTION, BASED ON A
      *               THE DIFFERENCE BETWEEN EARNINGS FOR THE
      *               YIELD (INTEREST AND DEALERL DISCOUNT)
      *               AND EARNINGS FOR INTEREST
      *    14       - PERCENTAGE USING ACTUARIAL PRINICPAL REDUCTION,
      *
      *                       CERN-APCUR - INT INCOME [RE: (6)]
      *               THE % = ----------------------------------
      *                              AMOUNT FINANCED
      *    15       - REAL INTEREST BEARING METHOD
      *    16       - PERCENTAGE OF DEALER DISCOUNT UNEARNED
      *               RE: IB METHOD PRINICAL REDUCTION
      *
      *                       APPLIED TO PRINCIPAL VIA. IB METHOD
      *               THE % = -----------------------------------
      *                       REMAINING PRINCIPAL  VIA. IB METHOD
      *
      *    17       - PERCENTAGE OF THE ORIGINAL CHARGE
      *
      *                       INTEREST INCOME [RE: (12)]
      *               THE % = -----------------------------------
      *                       ORIGINAL INTEREST CHARGE
      *
      *               WHERE (12) IS ACTUARIAL BASED ON TIME OF PAYMENT
      *
      *    18       - ACTUARIAL BASED ON TIME FOR INTEREST ONLY
      *               MIDSTREAMS EARN INTEREST ON ORIGINAL LOAN SCHEDULE
      *
      *    19       - ACTUARIAL BASED ON TIME OF PAYMENT
      *               SAME AS 12, EXCEPT THAT THIS FAIRCO, FL VERSION
      *    20       - EARNINGS ARE HELD IN UNEARNED UNTIL TIME OF PAYOFF
      *               (GRAND ISLAND, NE)
      *    21       - ACTUARIAL BASED ON TIME OF PAYMENT (INTEREST DATE)
      *    22       - DISCOUNT EARN FORMULA, BALANCE REMAINING METHOD
      *               ATLANTIC FINANCE
      *    23       - PRORATA EARN FORMULA, PERCENT OF TOTAL MONIES PAID
      *               SALES TAX PAACO FINANCE, 1469
      *    24       - THIS CASH EARN FORMULA COMPUTES EARNINGS ON THE
      *               COMPONENT IN THE SAME RATIO OF THE COMBINED
      *               INTEREST EARNINGS AND SERVICE CHARGE EARNINGS
      *               TO THE COMBINED ORIGINAL INTEREST ANTICIPATED
      *               AND THE ORIGINAL SERVICE CHARGE ANTICIPATED
      *               MEXICO, WORLD #408
      *
      *   IN  : CERN-APCUR           - CURRENT TRAMT
      *         CERN-PAYMNTD         - LN-TOTPAYMNTD + LN-TOTEXCPAYMNTD
      *                                PRIOR TO THIS TRANSACTION
      *         CERN-TOTLCHG         - LN-TOTLCHG PRIOR TO THIS TRANS.
      *         CERN-PAYDATE         - TODAYS DATE
      *         CERN
      *             0                - ALL COMPONENTS (DEFAULT)
      *             1 TO 6           - THAT COMPONENT
      *   NOTE:
      *         CERN-FIX-MODE-FG     - " " BY DEFAULT, CLEARED APON EXIT
      *                                "Y" IN FIX MODE
      *
      *   OUT : CERN-EARNINGS()        - CERN = 1 TO 6
      *         -------------------------------------------------------------
      *         - THE FOLLOWING FIELDS ARE USED BY SP-ERNFRMLA 15
      *         -------------------------------------------------------------
      *         CERN-EARNINGS-OWING  -EARNED EARNINGS FOR INTEREST & DISCOUNT
      *                                         LESS CERN-APCUR
      *         CERN-EARNINGS-ERNINT -IB INTEREST TAKEN ON THIS PAYMENT
      *         CERN-EARNINGS-PR-INT -PRIOR IB INTEREST OWING
      *
      *         -------------------------------------------------------------
      *         - THE FOLLOWING FIELDS ARE USED BY SP-ERNFRMLA 16
      *         -------------------------------------------------------------
      *         CERN-EARNINGS-OWING  -IB INTEREST OWING ON THIS PAYMENT
      *         CERN-EARNINGS-ERNINT -IB INTEREST TAKEN ON THIS PAYMENT
      *         CERN-EARNINGS-PR-INT -PRIOR IB INTEREST OWING
      *
      *
      * REV :
      * 091186 JTG REVISED CODES FOR "A" THRU "K"
      * 120186 JTG REVISED FOR ADDITION OF CODE Q
      * 121086 JTG REVISED FOR ADDITION OF CODE R
      * 030587 JTG REVISED FOR ADDITION OF CODE S
      * 020988 JTG REMOVE LOGIC FOR K WHICH WENT TO ACCRUAL
      * 051888 BAW REVISED FOR ADDITION OF CODE 20
      * 122288 JTG CHANGED NAME FROM 'LPEARN'
      * 062689 JTG CHANGED DEFAULT DAYS FROM 1 TO
      *            VALUE NEW FIELD SP-EARN-DAY
      * 091189 JTG ADDED WKDATE LOGIC FOR FORMULA 6 BUG
      * 102589 JTG FIXED BUG IN APR RECALC
      * 110289 SLC ADDED LOGIC FOR TFC CHANGE FROM ACCRUAL TO CASH.
      *            IF LOAN IS REVERTED TO CASH FORMULA 51 BECOMES 8.
      * 110689 SLC ADDED FORMULA 10 FOR RULE OF 78THS NO PARTIAL
      *            CHUNKS OF EARNINGS.
      * 112989 SLC NOW USE DISCLOSED-FINAMT INSTEAD OF FINANCED-AMT
      *            WHEN COMPUTING APR.
      * 121189 JTG CHANGED TO ALLOW DLDICS TO COMPUTE ACTUARIAL
      * 032990 JTG STOPPED PL AND SA FROM TAKING EARNINGS
      *            CORRECTED BUG IN ACTUARIAL RE: UPFRONT
      * 041090 JTG FORCED 6 TO CALC APR ON ALL SALEFIN ACCTS
      * 050290 JTG PUT TEST ON LN-REBATE(), SKIP COMPONENT
      * 061890 JTG STOPPED DIR LOANS FROM BEING VIEWED AS MIDSTREAM
      * 081090 JTG TEMPORARY STOP OF LEVEL TO FROM TAKING EARNINGS,
      *            UNTIL FORMULAS ARE ADDED FOR ADDON INSURANCE
      * 111491 JTG CORRECTED RECALC OF APR (APR-1STPYAMT)
      * 123091 JTG ADDED LOGIC FOR ERNFRMLA = 11
      * 060193 JTG ADDED LOGIC FOR ERNFRMLA = 12, BASED ON TIME
      * 012794 JTG ADDED LOGIC FOR ERNFRMLA = 13, DISCOUNT YIELD - INT
      * 062794 JTG ADDED LOGIC FOR ERNFRMLA = 14, DISCOUNT EARNINGS
      *            A % OF PRINCIPAL PAID RE: SP-ERNFRMAL = 6 (FAIRCO)
      * 072794 JTG ERNFRMLA = 15, REAL INTEREST BEARING METHOD -INSTANT AUTO, FL
      * 111794 JTG SP-ERNFRMLA = 16, REAL INTEREST BEARING METHOD
      *            DISCOUNT EARNINGS A % OF PRINCIPAL PAID FAIRCO, FL
      * 020695 JTG REWRITE ERNFRMLA 12 TO CORRECTLY WORK WITH MIDSTREAMS
      * 020795 JTG ADDED ERNFRMLA 17 AUTOPRIME FL
      * 031595 JTG ADDED USE OF SP-ERNACCRL-DFCD FOR ERNFRMLA 7
      * 032095 JTG ADDED FIELD FOR FIX MODE CERN-FIX-MODE-FG
      * 112495 JTG CORRECTED CERNINGS-6 RE: FL AUTO WHEN PAYMENTS ARE
      *            SHORT OF THE 1ST PERIOD AND LET THIS FORMULA CATCH UP INCOME
      * 053196 JTG ADDED FORMULA 19, FAIRCO, FL OLD VERSION OF 12
      * 061096 BAH ADDED ERNFRMLA 20 FOR GRAND ISLAND (NE), ALL EARNINGS
      *            TAKEN AT TIME OF PAYOFF
      * 121896 JTG FIXED BUG, WHEN ACCOUNTS WERE EITHER:
      *                   1. ACCELERATED AND SP-ERNACCEL-CD = " "
      *                   2. PUT IN JUDGEMENT AND SP-ERNJD-CD = " "
      *            THAN ALL EARNING COMPONENTS WERE CLOSED OUT.
      *            HOWEVER, THE BUG DID'T RECONIZE THIS AND CONTINUED TO
      *            EARN ALL NON REBATED COMPONENTS.
      * 011097 JTG ADDED SP-ERNFRMLA = 22 DISCOUNT EARN FORMULA,
      *            ATLANTIC FINANCE, BALANCE REMAINING METHOD
      * 022798 JTG CHANGED FORMULA 6 TO CORRECTLY WORK WITH MID STREAM
      *            PURCHASES, RE: AUTOPRIME
      * 032798 JTG ADDED CERN-TRCD TEST, RE: ERNFRMLA = 6, PREMIER FINANCE
      * 042198 JTG REWROTE ERNFRMLA = 6, TO WORK WITH NON MONTHLY PAPER
      *            RE: AUTOPRIME
      *            AND STOPPED EARNINGS ON 'SS' SALE OF SECURITY FOR
      *            ERNFRMLA = 6, 11, 13 & 14
      * 991118 JTG CHANGED PASSED VALUE FOR CERN-PAYMNTD TO BE SUM OF
      *            LN-TOTPAYMNTD + LN-TOTEXCPAYMNTD PRIOR TO THIS TRANSACTION
      *            RE: CHALLENGE FINANCE #258
      * JTG 000714 CHANGED RE: BETH'S CONVERSIONS TO TAKE NO INCOME WHEN
      *            LN-UFERN() = LN-ACCERN()
      * JTG 000801 CORRECTED RE: BETH'S CONVERSIONS TO TAKE NO INCOME WHEN
      *            LN-UFERN() = LN-ACCERN() AND LN-ANTICERN() = LN-ACCERN()
      *            AND LN-UFERN() NOT = 0
      * JTG 010521 NEW CODE '23' FOR SP-ERNFRMLA RE: SERVICE CHG  PAACO PR#1469
      * JTG 040412 CHANGED FORMULA 8 TO STOP TAKING EARNINGS WHEN 'OT' IS
      *            APPLIED TO PRINCIPAL WITH 'AP' AND EARNINGS GO NEGITIVE
      *            SINCE ORIGPURAMT IS < PURBAL          WORLD SANDY/JIM R #J044
      * JTG 050715 ADDED NEW FORMULAS FOR MEXICO
      *            SP-MFFRMLA-05 SP-RBFRMLA2 (G) SP-ERNFRMLA (24)
      *            ADDED CERN-LN-INT-EARN CERN-LN-SC-EARN
      *                  CERN-LN-MF-EARN CERN-LN-DF-EARN            WORLD #0408
      *  CS 121119 IF LN-ERNDELQ-STOP = "C", EARNINGS SHOULD BE ON A CASH
      *            FORMULA 6 EARNINGS METHOD. IF 'C', IF EARNINGS FORMULA = 52
      *            FORCE 6 TO ALL EARNINGS COMPONENTS. WORLD PR#828
      *  CS 010307 EARNING FORMULA 6 DIDNT CORRECTLY TAKE EARNINGS IF AN
      *            ACCOUNT HAD FEES/INSURANCE AND WAS CONSIDERED PART OF THE
      *            FINANCE CHARGE FOR DISCLOSURE PURPOSES, CHANGED CERN-
      *            COMPUTE-APR-6 TO ADD BACK THE FEES/INSURANCE TO THE AMOUNT
      *            FINANCED.  PR#861  WORLD (GA ACCOUNTS)
      **************************************************************************
       COMPUTE-CERNINGS SECTION.
           MOVE 0 TO CERN-EARNINGS(1) CERN-EARNINGS(2)
                     CERN-EARNINGS(3) CERN-EARNINGS(4)
                     CERN-EARNINGS(5) CERN-EARNINGS(6).
           MOVE 0 TO CERN-EARNINGS-OWING
                     CERN-EARNINGS-ERNINT
                     CERN-EARNINGS-PR-INT.
           MOVE 0               TO CERN-LN-INT-EARN CERN-LN-SC-EARN.
           MOVE CERN-PAYMNTD    TO CERN-SAVE-PAYMNTD.
           MOVE LN-UNITPER-CD   TO DATER-UNITPER-CD.
           MOVE LN-UNITPER-FREQ TO DATER-UNITPER-FREQ.

           IF CERN > 6
                OR LN-PLCD NOT = " "
                 OR LN-SERVACCT = "Y"
              GO TO COMPUTE-CERNINGS-EXIT.

           IF CERN = 0
              MOVE 1 TO CERN
              PERFORM COMPUTE-CERNINGS-COMPONENT 6 TIMES
           ELSE
              PERFORM COMPUTE-CERNINGS-COMPONENT.

       COMPUTE-CERNINGS-EXIT.
           MOVE 0 TO CERN.

      ***************************************************
      *    GET A COMPONENT'S CASH EARNINGS
      ***************************************************
       COMPUTE-CERNINGS-COMPONENT SECTION.

      *    IF LN-ERNDELQ-STOP = "C"
      *        IF SP-ERNFRMLA(CERN) = 52
      *          MOVE 6 TO SP-ERNFRMLA(CERN).

           MOVE 0 TO CERN-EARN.

      * TAKE NO INCOME WHEN LN-UFERN() = LN-ACCERN() = LN-ANTICERN()
           IF LN-UFERN(CERN) NOT = 0
              IF LN-UFERN(CERN) = LN-ACCERN(CERN)
                 IF LN-UFERN(CERN) =
                        ( LN-ANTICERN(CERN) + LN-ANTICADJ(CERN) )
                    GO TO COMPUTE-CERNINGS-COMP-EXIT.

      * TEST FOR INTEREST BEARING METHOD, DL DISC:
           IF CERN = 5
              IF SP-ERNFRMLA(5) = 15
                 MOVE CERN-EARNINGS(5) TO CERN-EARN
                 GO TO COMPUTE-CERNINGS-COMP-EXIT.

      * TEST FOR INTEREST BEARING:
           IF LN-ANTICERN(CERN) = 0 OR CERN = 2
              GO TO COMPUTE-CERNINGS-COMP-EXIT.

      * SKIP EARNINGS IF ACCOUNT WAS ACCELERATED OR PUT IN JUDGEMENT
      *  AND SPR'S CAUSED ALL UNEARN TO BE TAKEN INTO INCOME:
           IF (LN-ACCELDATE NOT = 0 AND SP-ERNACCEL-CD = " ")
                     OR (LN-JDDATE NOT = 0 AND SP-ERNJD-CD = " ")
              GO TO COMPUTE-CERNINGS-COMP-EXIT.

      * SKIP REBATED COMPONENTS:
           IF CERN = 1 AND
              ( LN-ANTICADJ(1) NOT = 0
                      OR (NOT (LN-REBATE(4) = " " OR "C")) )
              IF SP-ERNFRMLA(1) NOT = 15
                 GO TO COMPUTE-CERNINGS-COMP-EXIT
              ELSE
                 NEXT SENTENCE
           ELSE
           IF CERN = 3 AND
              ( LN-ANTICADJ(3) NOT = 0
                      OR (NOT (LN-REBATE(5) = " " OR "C")) )
              GO TO COMPUTE-CERNINGS-COMP-EXIT
           ELSE
           IF CERN = 4 AND
              ( LN-ANTICADJ(4) NOT = 0
                      OR (NOT (LN-REBATE(6) = " " OR "C")) )
              GO TO COMPUTE-CERNINGS-COMP-EXIT.

           IF (SP-ERNFRMLA(CERN) = 0 OR > 50
                   AND NOT (SP-ERNFRMLA(CERN) = 51
                               AND SP-ERNDELQFRMLA = "B"
                                AND LN-ERNDELQ-STOP = "Y"
                           )
                   AND NOT (SP-ERNFRMLA(CERN) = 52
                             AND SP-ERNDELQFRMLA = "R"
                             AND LN-ERNDELQ-STOP = "C"
                           )
              )
              GO TO COMPUTE-CERNINGS-COMP-EXIT.

           IF SP-ERNFRMLA(CERN) NOT > 4
              PERFORM COMPUTE-CERNINGS-1234
           ELSE
           IF SP-ERNFRMLA(CERN) = 5
              PERFORM COMPUTE-CERNINGS-5
           ELSE
           IF SP-ERNFRMLA(CERN) = 6 OR 52
              PERFORM COMPUTE-CERNINGS-6
           ELSE
           IF SP-ERNFRMLA(CERN) = 7
              PERFORM COMPUTE-CERNINGS-7
           ELSE
           IF SP-ERNFRMLA(CERN) = 8 OR 51 OR 10
              PERFORM COMPUTE-CERNINGS-8
           ELSE
           IF SP-ERNFRMLA(CERN) = 9
              PERFORM COMPUTE-CERNINGS-9
           ELSE
           IF SP-ERNFRMLA(CERN) = 11
              PERFORM COMPUTE-CERNINGS-11
           ELSE
           IF SP-ERNFRMLA(CERN) = 12 OR 21
              PERFORM COMPUTE-CERNINGS-12-21
           ELSE
           IF SP-ERNFRMLA(CERN) = 13
              PERFORM COMPUTE-CERNINGS-13
           ELSE
           IF SP-ERNFRMLA(CERN) = 14
              PERFORM COMPUTE-CERNINGS-14
           ELSE
           IF SP-ERNFRMLA(CERN) = 15
              PERFORM COMPUTE-CERNINGS-15
           ELSE
           IF SP-ERNFRMLA(CERN) = 16
              PERFORM COMPUTE-CERNINGS-16
           ELSE
           IF SP-ERNFRMLA(CERN) = 17
              PERFORM COMPUTE-CERNINGS-17
           ELSE
           IF SP-ERNFRMLA(CERN) = 19
              PERFORM COMPUTE-FAIRCO-19
           ELSE
           IF SP-ERNFRMLA(CERN) = 20
              PERFORM COMPUTE-CERNINGS-20
           ELSE
           IF SP-ERNFRMLA(CERN) = 22
              PERFORM COMPUTE-CERNINGS-22
           ELSE
           IF SP-ERNFRMLA(CERN) = 23
              PERFORM COMPUTE-CERNINGS-23
           ELSE
           IF SP-ERNFRMLA(CERN) = 24
              PERFORM COMPUTE-CERNINGS-24
           ELSE
              GO TO COMPUTE-CERNINGS-COMP-EXIT.

      * TEST FOR EARNINGS TAKEN GREATER THAN UNEARNED:
           IF CERN-EARN > (LN-ANTICERN(CERN) - LN-ACCERN(CERN))
              COMPUTE CERN-EARN = LN-ANTICERN(CERN) - LN-ACCERN(CERN).

       COMPUTE-CERNINGS-COMP-EXIT.
           MOVE CERN-EARN TO CERN-EARNINGS(CERN).
           IF CERN = 1
              MOVE CERN-EARN TO CERN-LN-INT-EARN
           ELSE
           IF CERN = 3
              MOVE CERN-EARN TO CERN-LN-SC-EARN.

           ADD 1 TO CERN.

      ******************************************
      * COMPUTE EARNINGS BY COMPUTING REBATE:
      *         (1 OR 2 OR 3 OR 4)
      ******************************************
       COMPUTE-CERNINGS-1234 SECTION.
           IF CERN = 1
              MOVE 7 TO REB-SUB
           ELSE
           IF CERN = 2 AND EADJ-ADJUST-FG(2) = "Y"
              MOVE CERN-EARNINGS(2) TO CERN-EARN
              GO TO COMPUTE-CERNINGS-1234-EXIT
           ELSE
           IF CERN = 3 AND EADJ-ADJUST-FG(3) = "Y"
              MOVE CERN-EARNINGS(3) TO CERN-EARN
              GO TO COMPUTE-CERNINGS-1234-EXIT
           ELSE
           IF CERN = 3
              MOVE 8 TO REB-SUB
           ELSE
           IF CERN = 4 AND EADJ-ADJUST-FG(4) = "Y"
              MOVE CERN-EARNINGS(4) TO CERN-EARN
              GO TO COMPUTE-CERNINGS-1234-EXIT
           ELSE
           IF CERN = 4
              MOVE 9 TO REB-SUB
           ELSE
              GO TO COMPUTE-CERNINGS-1234-EXIT.

           MOVE LN-ANTICERN(CERN) TO REB-TOTCHG.
           MOVE LN-ANTICTERM TO REB-ORGTERM.
           MOVE "PY" TO REB-LPTRCD.
           IF SP-ERNFRMLA(CERN) = 1 OR 2
              MOVE CERN-PAYDATE TO REB-PAYDATE.
           IF SP-ERNFRMLA(CERN) = 3 OR 4
              MOVE LN-ORIG-1STPYDATE TO PDTH-DATE-WORK
              PERFORM PAID-THRU-CALCULATION
              MOVE PDTH-DATE-FULL TO REB-PAYDATE.
           PERFORM REBATE.
           IF REB-SUB = 0
              GO TO COMPUTE-CERNINGS-1234-EXIT.
           IF CERN NOT = 1
              COMPUTE CERN-EARN = LN-ANTICERN(CERN)
                                       - REB-REBATE - LN-ACCERN(CERN)
              GO TO COMPUTE-CERNINGS-1234-EXIT.

      *-----------------------------------------------------
      * COMPUTE ADJUSTMENT FOR THOSE COMPONENTS WHICH ARE
      * REBATED UNDER THE INTEREST REBATE FORMULA
      *-----------------------------------------------------
           MOVE REB-REBATE TO EADJ-REBATE.
           PERFORM EADJ-EARN-ADJUSTMENT.

           IF EADJ-ADJUST-FG(1) = "Y"
              COMPUTE CERN-EARN =
                    LN-ANTICERN(1) - EADJ-ADJUST(1) - LN-ACCERN(1)
           ELSE
           IF EADJ-ADJUST-FG(2) = "Y"
              COMPUTE CERN-EARNINGS(2) =
                    LN-ANTICERN(2) - EADJ-ADJUST(2) - LN-ACCERN(2)
              IF LN-ANTICERN(2) = 0
                 ADD CERN-EARNINGS(2) TO CERN-EARN
              END-IF
           ELSE
           IF EADJ-ADJUST-FG(3) = "Y"
              COMPUTE CERN-EARNINGS(3) =
                    LN-ANTICERN(3) - EADJ-ADJUST(3) - LN-ACCERN(3)
              IF LN-ANTICERN(3) = 0
                 ADD CERN-EARNINGS(3) TO CERN-EARN
              END-IF
           ELSE
           IF EADJ-ADJUST-FG(4) = "Y"
              COMPUTE CERN-EARNINGS(4) =
                    LN-ANTICERN(4) - EADJ-ADJUST(4) - LN-ACCERN(4).

       COMPUTE-CERNINGS-1234-EXIT.
           EXIT.

      *************************************************
      *                    (5)
      * COMPUTE EARNINGS PRO-RATA PRINCIPLE BALANCE
      *************************************************
       COMPUTE-CERNINGS-5 SECTION.
           IF LN-PURBAL > 0
              COMPUTE CERN-EARN ROUNDED =
                  CERN-APCUR / LN-PURBAL
                        * (LN-ANTICERN(CERN) - LN-ACCERN(CERN)).



      ****************************************************
      *                    (6)
      * COMPUTE EARNINGS ACTUARIAL (PRINCIPAL REDUCTION):
      *         WITH PARTIAL PAYS VIEWED AS PARTIAL MONTHS
      *
      *    NOTE:
      *         THIS ROUTINE ONLY MAKES SENSE FOR INTEREST
      ****************************************************
       COMPUTE-CERNINGS-6 SECTION.
      * NO EARNINGS ON SALE OF SECURITY:
           IF (CERN-TRCD = "SS")
              MOVE 0 TO CERN-EARN
              GO TO COMPUTE-CERNINGS-6-EXIT.

           PERFORM CERN-SAVE-LN-SP.
           MOVE CERN-SAVE-PAYMNTD TO CERN-PAYMNTD.
           MOVE "A" TO SP-DISFRMLA.

      * SETUP UNIT PERIOD INFORMATION:
           MOVE LN-ORGTERM TO UPER-TERM.
           MOVE LN-UNITPER-CD TO UPER-UNITPER-CD.
           MOVE LN-UNITPER-FREQ TO UPER-UNITPER-FREQ.
           MOVE LN-INTDATE TO UPER-INTDATE.
           MOVE LN-ORIG-1STPYDATE TO UPER-1STPYDATE.
           PERFORM UPER-CALCULATION.

           IF (LN-SALEFIN = "Y")
                     AND (LN-ORIGPURAMT NOT = LN-LNAMT)
              COMPUTE CERN-PAYMNTD = LN-LNAMT - LN-ORIGPURAMT
              PERFORM COMPUTE-EARNINGS-PAYMENTS
              MOVE NDTE-DATE TO CERN-SAVE-WKDATE1
              MOVE CERN-WPAYS TO CERN-SAVE-WPAYS
              MOVE CERN-SAVE-PAYMNTD TO CERN-PAYMNTD.

           IF SP-CONTRFRMLA = "A"
              SUBTRACT CERN-TOTLCHG FROM CERN-PAYMNTD.
           PERFORM COMPUTE-EARNINGS-PAYMENTS.
           MOVE NDTE-DATE TO CERN-WKDATE1.
           MOVE CERN-WPAYS TO CERN-SAVE-WPAYS.

           ADD CERN-APCUR TO CERN-PAYMNTD.
           PERFORM COMPUTE-EARNINGS-PAYMENTS.
           MOVE NDTE-DATE TO CERN-WKDATE2.

      * DETERMINE REQUIREMENT FOR ESTABLISHING A NEW
      * APR FOR ACTUARIAL REBATE CALCULATION:
           IF (LN-SALEFIN = "Y")
                     AND (LN-ORIGPURAMT NOT = LN-LNAMT)

              PERFORM CERN-SET-WORKERS-CERN1
              MOVE CERN-SAVE-WKDATE1 TO CERN-WK-PURDATE
              MOVE CERN-WK-ORIG-1STPYDATE TO CERN-WK-1STPYDATE
              PERFORM CERN-MIDSTREAM-NEW
              MOVE CERN-WK-ANTICTERM   TO LN-ANTICTERM
              MOVE CERN-WK-ORGTERM     TO LN-ORGTERM
              MOVE CERN-WK-NOREGPY     TO LN-NOREGPY
              MOVE CERN-WK-1STPYAMT    TO LN-1STPYAMT
              MOVE CERN-WK-REGPYAMT    TO LN-REGPYAMT
              MOVE CERN-WK-LASTPYAMT   TO LN-LASTPYAMT
              MOVE CERN-WK-ORIG-1STPYDATE TO LN-ORIG-1STPYDATE
              MOVE CERN-WK-PURDATE     TO LN-INTDATE.

           PERFORM CERN-COMPUTE-APR-6.

           MOVE CERN-WKDATE1 TO REB-PAYDATE.
           PERFORM COMPUTE-EARN-ACTUARIAL-REBATE.
           IF CERN-SET = 0
              COMPUTE CERN-EARN = CERN-EARN + REB-REBATE
           ELSE
              COMPUTE CERN-EARN = CERN-EARN + REB-PRES-VALUE
              MOVE CERN-SV-INTDATE TO REB-PAYDATE
              PERFORM COMPUTE-EARN-ACTUARIAL-REBATE
              MOVE REB-PRES-VALUE TO CERN-PRES-VALUE.

           MOVE CERN-WKDATE2 TO REB-PAYDATE.
           PERFORM COMPUTE-EARN-ACTUARIAL-REBATE.
           IF CERN-SET = 0
              COMPUTE CERN-EARN =
                   LN-ANTICERN(CERN) - LN-ACCERN(CERN) - REB-REBATE
           ELSE
              COMPUTE CERN-EARN = CERN-EARN - REB-PRES-VALUE.

           PERFORM CERN-RESTORE-LN-SP.

       COMPUTE-CERNINGS-6-EXIT.
           EXIT.


      ************************************
      * COMPUTE ACTUARIAL REBATE
      ************************************
       COMPUTE-EARN-ACTUARIAL-REBATE SECTION.
           MOVE "Y" TO REB-SKIPDEF-FG.
           MOVE LN-ANTICTERM TO REB-ORGTERM.
           SUBTRACT LN-UFERN(CERN) FROM LN-ANTICERN(CERN)
                              GIVING REB-TOTCHG.

           IF SP-ERNFRMLA(CERN) = 13
              COMPUTE REB-TOTCHG =
                 REB-TOTCHG + LN-ANTICERN(1) - LN-UFERN(1).

           MOVE 7 TO REB-SUB.
           MOVE "F" TO SP-RBFRMLA(7).
           MOVE 0 TO SP-RBDEDCHG(7).
           MOVE 0 TO SP-RBNOMON(7).
           MOVE 0 TO SP-RBREDUC(7).
           MOVE 0 TO SP-RBMINRET(7).
           MOVE 0 TO SP-RBRECAST(7).
           MOVE 0 TO SP-RBEARLY(7).
           MOVE 0 TO SP-RBDEFER(7).
           MOVE 100 TO SP-RBPCT(7).
           MOVE 0 TO SP-RBMIN(7).
           MOVE "B" TO SP-RBDTE(7 1) SP-RBDTE(7 2).
           MOVE 0 TO SP-RBDAYS(7 1) SP-RBDAYS(7 2).
           MOVE 360 TO SP-RBYRTYPE(7 1) SP-RBYRTYPE(7 2) SP-YEARTYPE.
           PERFORM REBATE.

      ************************************
      * COMPUTE EARNING PAYMENTS
      ************************************
       COMPUTE-EARNINGS-PAYMENTS SECTION.
           IF CERN-PAYMNTD NOT > LN-1STPYAMT
              COMPUTE CERN-PAYMENTS ROUNDED =
                               CERN-PAYMNTD / LN-1STPYAMT
              MOVE LN-INTDATE TO NUM-DATE
              MOVE LN-1STPYDATE TO SYS-DATE
              PERFORM TIM365
              MULTIPLY CERN-PAYMENTS BY ELAPSED-DAYS
                             GIVING NDTE-HOLD ROUNDED
              MOVE LN-INTDATE TO NDTE-DATE
              PERFORM INCREMENT-DAYS
           ELSE
           IF CERN-PAYMNTD NOT > (LN-1STPYAMT
                                       + LN-NOREGPY * LN-REGPYAMT)
              COMPUTE CERN-PAYMENTS ROUNDED =
                 (CERN-PAYMNTD - LN-1STPYAMT) / LN-REGPYAMT
              MOVE LN-ORIG-1STPYDATE TO NDTE-DATE
              MOVE CERN-WPAYS TO NDTE-HOLD
              PERFORM INCREMENT-UNITPER
              MULTIPLY CERN-PPAYS BY UPER-DAYS-IN-UNITPER
                             GIVING NDTE-HOLD ROUNDED
              PERFORM INCREMENT-DAYS
           ELSE
              COMPUTE CERN-PAYMENTS ROUNDED =
                  ( CERN-PAYMNTD
                       - (LN-1STPYAMT + LN-NOREGPY * LN-REGPYAMT)
                  )
                    / LN-LASTPYAMT
              MOVE LN-ORIG-1STPYDATE TO NDTE-DATE
              MOVE LN-NOREGPY TO NDTE-HOLD
              PERFORM INCREMENT-UNITPER
              MULTIPLY CERN-PPAYS BY UPER-DAYS-IN-UNITPER
                             GIVING NDTE-HOLD ROUNDED
              PERFORM INCREMENT-DAYS.


       COMPUTE-EARNINGS-PAYMENTS-EXIT.
           EXIT.

      *********************************************************
      *                       (7)
      * COMPUTE EARNINGS RULE 78'S AS PAYMENTS ARE
      * RECEIVED, EARNING ADDITIONAL MONTH FOR 1 ELAPSED DAY.
      *********************************************************
       COMPUTE-CERNINGS-7 SECTION.
           MOVE SP-REBTBL(7) TO CERN-SV-REBTBL.
           MOVE SP-YEARTYPE  TO CERN-SV-YEARTYPE.
           PERFORM COMPUTE-EARN-78THS-REBATE.
           MOVE CERN-SV-REBTBL   TO SP-REBTBL(7).
           MOVE CERN-SV-YEARTYPE TO SP-YEARTYPE.

      ************************************
      * COMPUTE 78THS REBATE
      ************************************
       COMPUTE-EARN-78THS-REBATE SECTION.
           MOVE LN-ANTICTERM TO REB-ORGTERM.
           MOVE LN-ANTICERN(CERN) TO REB-TOTCHG.
           MOVE CERN-PAYDATE TO REB-PAYDATE.
           MOVE 7 TO REB-SUB.
           MOVE 1 TO SP-RBFRMLA(7).
           MOVE LN-UFERN(CERN) TO SP-RBDEDCHG(7).
           MOVE 0 TO SP-RBNOMON(7).
           MOVE 0 TO SP-RBREDUC(7).
           MOVE 0 TO SP-RBMINRET(7).
           MOVE 0 TO SP-RBRECAST(7).
           MOVE 0 TO SP-RBEARLY(7).
           MOVE 0 TO SP-RBDEFER(7).
           IF SP-ERNFRMLA(CERN) = 7
              IF SP-ERNACCRL-DFCD(CERN) NUMERIC
                 MOVE SP-ERNACCRL-DFCD(CERN) TO SP-RBDEFER(7).
           MOVE 100 TO SP-RBPCT(7).
           MOVE 0 TO SP-RBMIN(7).
      * CHANGED FOLLOWING TO 'X' FROM 'B'  AMW 082887
           MOVE "X" TO SP-RBDTE(7 1) SP-RBDTE(7 2).
           MOVE SP-ERN-DAYS(CERN) TO SP-RBDAYS(7 1) SP-RBDAYS(7 2).
           MOVE 360 TO SP-RBYRTYPE(7 1) SP-RBYRTYPE(7 2) SP-YEARTYPE.
           PERFORM REBATE.
           IF REB-SUB = 0
              GO TO EXIT-COMPUTE-EARN-78THS.
           COMPUTE CERN-EARN = LN-ANTICERN(CERN)
                         - REB-REBATE - LN-ACCERN(CERN).

       EXIT-COMPUTE-EARN-78THS.
           EXIT.

      *********************************************************
      *                    (8)
      * COMPUTE EARNINGS RULE 78'S AS PAYMENTS ARE
      * RECEIVED, EARNING RULE OF 78'S UNIT CHARGES FOR
      * MONIES RECEIVED.
      *********************************************************
       COMPUTE-CERNINGS-8 SECTION.
           IF LN-PURBAL = 0
            OR CERN-APCUR = 0
               GO TO COMPUTE-CERNINGS-8-EXIT.
      * SETUP TOTAL MONIES APPLIED TOWARDS ORIG.PURCHASE:
           SUBTRACT CERN-APCUR FROM LN-PURBAL
                                 GIVING CERN-WORKER.
           IF CERN-WORKER < 0
              MOVE LN-ORIGPURAMT TO CERN-WORKER
           ELSE
              SUBTRACT CERN-WORKER FROM LN-ORIGPURAMT
                                          GIVING CERN-WORKER.

      * --- STOP TAKINGS EARNINGS WHEN CERN-WORKER IS NEGATIVE
      * --- PURBAL IS GREATER THAN ORIGPURAMT RE: 'OT' AND 'AP' WORLD #J044
           IF CERN-WORKER < 0
              MOVE 0 TO CERN-EARN
              GO TO COMPUTE-CERNINGS-8-EXIT.

      * DETERMINE NO. OF WHOLE AND FRACTIONAL PAYMENTS:
           MOVE 0 TO CERN-PAYMENTS.
           IF CERN-WORKER NOT < LN-1STPYAMT
              SUBTRACT LN-1STPYAMT FROM CERN-WORKER
              MOVE 1 TO CERN-PAYMENTS
           ELSE
              COMPUTE CERN-PAYMENTS ROUNDED =
                   CERN-WORKER / LN-1STPYAMT
              GO TO COMPUTE-CERNINGS-8-TEST-EXTRA.

           IF CERN-WORKER NOT < (LN-NOREGPY * LN-REGPYAMT)
              COMPUTE CERN-WORKER = CERN-WORKER
                             - (LN-NOREGPY * LN-REGPYAMT)
              ADD LN-NOREGPY TO CERN-PAYMENTS
           ELSE
              COMPUTE CERN-PAYMENTS ROUNDED =
                   CERN-PAYMENTS + CERN-WORKER / LN-REGPYAMT
              GO TO COMPUTE-CERNINGS-8-TEST-EXTRA.

           IF LN-LASTPYAMT NOT = 0
              COMPUTE CERN-PAYMENTS ROUNDED =
                 CERN-PAYMENTS + CERN-WORKER / LN-LASTPYAMT.

       COMPUTE-CERNINGS-8-TEST-EXTRA.
           IF SP-ERNFRMLA(CERN) = 51
              PERFORM CALC-EXTRA-WPAYS.

           IF SP-ERNFRMLA(CERN) = 10
              PERFORM CHECK-PERCENT.

      * SETUP RULE OF 78THS UNIT CHARGE:
           COMPUTE CERN-WORKER =
                (LN-ANTICTERM * (LN-ANTICTERM + 1)) / 2.
           IF CERN-WORKER NOT = 0
              COMPUTE CERN-WORKER ROUNDED =
                 (LN-ANTICERN(CERN) - LN-UFERN(CERN)) / CERN-WORKER.
      * SETUP SUM OF WHOLE REMAINING PAYMENTS:
           COMPUTE CERN-SUMW =
              ((LN-ANTICTERM - CERN-WPAYS) *
                    (LN-ANTICTERM - CERN-WPAYS + 1)) / 2.
      * SUTUP SUM OF WHOLE REMAINING PAYMENTS PLUS 1:
           COMPUTE CERN-SUMW1 =
              ((LN-ANTICTERM - CERN-WPAYS - 1) *
                    (LN-ANTICTERM - CERN-WPAYS)) / 2.
      * COMPUTE UNEARNED THRU THIS PAYMENT:
           COMPUTE CERN-EARN ROUNDED = CERN-WORKER *
              (CERN-SUMW + CERN-PPAYS * (CERN-SUMW1 - CERN-SUMW)).
      * COMPUTE EARNINGS FOR THIS PAYMENT:
           COMPUTE CERN-EARN = LN-ANTICERN(CERN)
                         - CERN-EARN - LN-ACCERN(CERN).

       COMPUTE-CERNINGS-8-EXIT.
           EXIT.

      *************************************************
      *         (9)
      *    COMPUTE EARNINGS PRO-RATA BALANCE:
      *   (WHOLE PAYMENTS, AS PAYMENTS ARE RECEIVED)
      *************************************************
       COMPUTE-CERNINGS-9 SECTION.
           IF LN-REGPYAMT = 0
              GO TO COMPUTE-CERNINGS-9-EXIT.
      * COMPUTE PAYMENTS MADE, TRUNCATED:
           COMPUTE CERN-PAYMENTS = (LN-ORIGPURAMT - LN-EXTCHG
                          - LN-PURBAL + CERN-APCUR) /
                              LN-REGPYAMT.
      * COMPUTE PAYMENTS REMAINING AND NEW UNEARNED:
           COMPUTE CERN-EARN ROUNDED =
                                 (LN-ANTICTERM - CERN-WPAYS) *
                  ((LN-ANTICERN(CERN) - LN-UFERN(CERN)) / LN-ANTICTERM).
      * COMPUTE EARNED FROM UNEARNED:
           COMPUTE CERN-EARN = (LN-ANTICERN(CERN) - LN-ACCERN(CERN))
                                 - CERN-EARN.
       COMPUTE-CERNINGS-9-EXIT.
           EXIT.

      ***********************************************************
      *                        (11)
      *   COMPUTE EARNINGS ACTUARIAL (PRINCIPAL REDUCTION):
      *           USING PERCENT OF INT CHARGABLE REDUCTION
      *           WITH PARTIAL PAYS VIEWED AS PARTIAL MONTHS
      *
      *   NOTE:
      *         THIS ROUTINE IS ONLY FOR DEALER DISCOUNT
      *               (CERN = 5)
      **********************************************************
       COMPUTE-CERNINGS-11 SECTION.
      * NO EARNINGS ON SALE OF SECURITY:
           IF (CERN-TRCD = "SS")
              MOVE 0 TO CERN-EARN
              GO TO COMPUTE-CERNINGS-11-EXIT.

           MOVE 1 TO CERN.
           MOVE 5 TO CERN-SET.
           PERFORM COMPUTE-CERNINGS-6.

           COMPUTE CERN-EARN ROUNDED = CERN-EARN
                     * LN-ANTICERN(5) / CERN-PRES-VALUE.

       COMPUTE-CERNINGS-11-EXIT.
           MOVE 5 TO CERN.
           MOVE 0 TO CERN-SET.

      *******************************************************
      *                 (12 & 21)
      *
      *    (12) COMPUTE EARNINGS ACTUARIAL AS OF PAYMENT DATE
      *         USING PURCHASE DATE TO YIELD ACTUARIAL RATE
      *
      *    (21) COMPUTE EARNINGS ACTUARIAL AS OF PAYMENT DATE
      *         USING INTEREST DATE TO YIELD ACTUARIAL RATE
      *
      *******************************************************
       COMPUTE-CERNINGS-12-21 SECTION.
           PERFORM CERN-SAVE-LN-SP.
           MOVE "A" TO SP-DISFRMLA.

      * DETERMINE REQUIREMENT FOR ESTABLISHING A NEW
      * APR FOR ACTUARIAL REBATE CALCULATION:
           PERFORM CERN-SET-WORKERS-NEW.
           IF LN-SALEFIN = "Y"
              MOVE CERN-WK-PURDATE        TO NUM-DATE
              MOVE CERN-WK-ORIG-1STPYDATE TO SYS-DATE
              IF SYS-DATE < NUM-DATE
                       OR (LN-ORIGPURAMT NOT = LN-LNAMT)
                 PERFORM CERN-MIDSTREAM-NEW.

           PERFORM CERN-GET-APR-COMPONENT-NEW.
           PERFORM CERN-ACTUARIAL-REBATE-NEW.

           COMPUTE CERN-EARN = LN-ANTICERN(CERN)
                         - REGZ-UNEARNED - LN-ACCERN(CERN).

           PERFORM CERN-RESTORE-LN-SP.

       COMPUTE-CERNINGS-12-21-EXIT.
           EXIT.

      *******************************************************
      *                 (13)   (GULF ACCEPTANCE FL)
      *
      *   ACTUARIAL PRINICPAL REDUCTION, BASED ON A
      *       THE DIFFERENCE BETWEEN EARNINGS FOR THE
      *       YIELD (INTEREST AND DEALER DISCOUNT)
      *       AND EARNINGS FOR INTEREST
      *******************************************************
       COMPUTE-CERNINGS-13 SECTION.
      * NO EARNINGS ON SALE OF SECURITY:
           IF (CERN-TRCD = "SS")
              MOVE 0 TO CERN-EARN
              GO TO COMPUTE-CERNINGS-13-EXIT.

           MOVE CERN TO CERN-SAVE.
           MOVE 1 TO CERN.
           PERFORM COMPUTE-CERNINGS-6.
           MOVE CERN-SAVE TO CERN.
           MOVE CERN-EARN TO CERN-SAVE-INT-EARN.

           PERFORM CERN-SAVE-LN-SP.

      * -----------------------------------------------------------------
      *    THE FOLLOWING APR-... FIELDS HAVE BEEN SET VIA CERNINGS-6
      * -----------------------------------------------------------------
           SUBTRACT LN-ANTICERN(5) FROM APR-FINANCED.
      *     APR-INTDATE.
      *     APR-1STPYDATE.
      *     APR-ORGTERM.
      *     APR-1STPYAMT.
      *     APR-REGPYAMT.
      *     APR-LASTPYAMT.
      *     APR-UNITPER-CD.
      *     APR-UNITPER-FREQ.
      *     APR-DISFRMLA.
      * -----------------------------------------------------------------
      *    THE FOLLOWING FIELDS HAVE BEEN SET VIA CERNINGS-6
      * -----------------------------------------------------------------
      *    CERN-WKDATE1.
      *    CERN-WKDATE2.
      * ----------------------------------------------------------------
           PERFORM APR-REGZ.
           MOVE APR-APR TO LN-APRATE.

           MOVE CERN-WKDATE1 TO REB-PAYDATE.
           PERFORM COMPUTE-EARN-ACTUARIAL-REBATE.
           MOVE REB-REBATE TO CERN-EARN.

           MOVE CERN-WKDATE2 TO REB-PAYDATE.
           PERFORM COMPUTE-EARN-ACTUARIAL-REBATE.
           COMPUTE CERN-EARN = CERN-EARN - REB-REBATE
                                     - CERN-SAVE-INT-EARN.

           PERFORM CERN-RESTORE-LN-SP.

       COMPUTE-CERNINGS-13-EXIT.
           EXIT.


      *******************************************************
      *                 (14)   (FAIRCO, FL)
      *
      *   PERCENTAGE USING ACTUARIAL PRINICPAL REDUCTION,
      *
      *                  CERN-APCUR - INT INCOME [RE: (6)]
      *         THE % = ------------------------------------
      *                          AMOUNT FINANCED
      *
      *******************************************************
       COMPUTE-CERNINGS-14 SECTION.
      * NO EARNINGS ON SALE OF SECURITY:
           IF (CERN-TRCD = "SS")
              MOVE 0 TO CERN-EARN
              GO TO COMPUTE-CERNINGS-14-EXIT.

           MOVE CERN TO CERN-SAVE.
           MOVE 1 TO CERN.
           PERFORM COMPUTE-CERNINGS-6.
           MOVE CERN-SAVE TO CERN.

           PERFORM LOAN-CALCULATIONS.

           COMPUTE CERN-EARN ROUNDED =
               (CERN-APCUR - CERN-EARN)
                       / DISCLOSED-FINAMT * LN-ANTICERN(CERN).

           IF (CERN-EARN + LN-ACCERN(CERN)) > LN-ANTICERN(CERN)
              SUBTRACT LN-ACCERN(CERN) FROM LN-ANTICERN(CERN)
                                  GIVING CERN-EARN.

       COMPUTE-CERNINGS-14-EXIT.
           EXIT.

      *******************************************************
      *                 (15)   (INSTANT AUTO, KY)
      *
      *   REAL IB EARNINGS METHOD
      *
      *******************************************************
       COMPUTE-CERNINGS-15 SECTION.
           PERFORM CERN-SAVE-LN-SP.

           PERFORM LOAN-CALCULATIONS.
           SUBTRACT LN-DLDISC FROM FINANCED-AMOUNT
                                     GIVING APR-FINANCED.
           MOVE LN-INTDATE TO APR-INTDATE.
           MOVE LN-ORIG-1STPYDATE TO APR-1STPYDATE.
           MOVE LN-ORGTERM TO APR-ORGTERM.
           MOVE LN-1STPYAMT TO APR-1STPYAMT.
           MOVE LN-REGPYAMT TO APR-REGPYAMT.
           MOVE LN-LASTPYAMT TO APR-LASTPYAMT.
           MOVE LN-UNITPER-CD TO APR-UNITPER-CD.
           MOVE LN-UNITPER-FREQ TO APR-UNITPER-FREQ.
           MOVE "A" TO APR-DISFRMLA.
           PERFORM APR-REGZ.
           MOVE APR-APR TO LN-APRATE.

      * FIND ELAPSED DAYS FROM INTEREST PAID THRU DATE
      * THRU TODAY:
           IF LN-ERN-IB-PDTH-DATE = 0
              MOVE LN-INTDATE TO NUM-DATE
           ELSE
              MOVE LN-ERN-IB-PDTH-DATE TO NUM-DATE.
           MOVE CERN-PAYDATE TO SYS-DATE.
           MOVE SP-YEARTYPE TO ELAPSED-YRTYPE.
           PERFORM TIMALL.
           IF ELAPSED-DAYS < 0
              MOVE 0 TO ELAPSED-DAYS.

      *--------------------------------------------------------
      * SET PRIOR INTEREST OWING
      *
           MOVE LN-ERN-IB-INTOWE TO CERN-EARNINGS-PR-INT.
      *--------------------------------------------------------
      * COMPUTE INTEREST DUE THROUGH TODAY =
      *     INTEREST OWING + INTEREST DUE FROM LAST ASSESSMENT
      *
           IF SP-INTMETHOD = "O"
              MOVE 36000 TO CERN-DAYS
           ELSE
              MOVE 36500 TO CERN-DAYS.

           COMPUTE CERN-EARN ROUNDED =
               LN-ERN-IB-INTOWE
                    + (LN-ERN-IB-PRINBAL
                           * APR-APR / CERN-DAYS * ELAPSED-DAYS
                      ).
      *--------------------------------------------------------
      * TEST FOR ENOUGH PRINCIPAL TO COVER INTEREST DUE TODAY
      *
           IF CERN-APCUR < CERN-EARN
              SUBTRACT CERN-APCUR FROM CERN-EARN
                               GIVING CERN-EARNINGS-OWING
              MOVE CERN-APCUR TO CERN-EARN
                                  CERN-EARNINGS-ERNINT
           ELSE
              MOVE CERN-EARN TO CERN-EARNINGS-ERNINT.
      *--------------------------------------------------------
      * PRORATE EARNINGS OVER INTEREST AND DISCOUNT
      *
           COMPUTE CERN-EARNINGS(5) ROUNDED =
               LN-ANTICERN(5) * CERN-EARN
                      / (LN-ANTICERN(1) + LN-ANTICADJ(1)
                                               + LN-ANTICERN(5)).
           SUBTRACT CERN-EARNINGS(5) FROM CERN-EARN.

           IF (NOT (LN-REBATE(4) = " " OR "C"))
              MOVE 0 TO CERN-EARN.
      *--------------------------------------------------------

           PERFORM CERN-RESTORE-LN-SP.

       COMPUTE-CERNINGS-15-EXIT.
           EXIT.

      *******************************************************
      *                 (16)   (FAIRCO, FL)
      *       PERCENTAGE OF DEALER DISCOUNT UNEARNED
      *          RE: IB METHOD PRINICAL REDUCTION
      *
      *               APPLIED TO PRINCIPAL VIA. IB METHOD
      *       THE % = -----------------------------------
      *               REMAINING PRINCIPAL  VIA. IB METHOD
      *
      *******************************************************
       COMPUTE-CERNINGS-16 SECTION.
      * FIND ELAPSED DAYS FROM INTEREST PAID THRU DATE
      * THRU TODAY:
           IF LN-ERN-IB-PDTH-DATE = 0
              MOVE LN-INTDATE TO NUM-DATE
           ELSE
              MOVE LN-ERN-IB-PDTH-DATE TO NUM-DATE.
           MOVE CERN-PAYDATE TO SYS-DATE.
           MOVE SP-YEARTYPE TO ELAPSED-YRTYPE.
           PERFORM TIMALL.
           IF ELAPSED-DAYS < 0
              MOVE 0 TO ELAPSED-DAYS.

      *--------------------------------------------------------
      * SET PRIOR INTEREST OWING
      *
           MOVE LN-ERN-IB-INTOWE TO CERN-EARNINGS-PR-INT.
      *--------------------------------------------------------
      * COMPUTE INTEREST DUE THROUGH TODAY =
      *     INTEREST OWING + INTEREST DUE FROM LAST ASSESSMENT
      *
           IF SP-INTMETHOD = "O"
              MOVE 36000 TO CERN-DAYS
           ELSE
              MOVE 36500 TO CERN-DAYS.

           COMPUTE CERN-EARN ROUNDED =
               LN-ERN-IB-INTOWE
                + (LN-ERN-IB-PRINBAL
                        * LN-SMPRATE / CERN-DAYS * ELAPSED-DAYS
                  ).
      *--------------------------------------------------------
      * TEST FOR ENOUGH PRINCIPAL TO COVER INTEREST DUE TODAY
      *
           IF CERN-APCUR < CERN-EARN
              SUBTRACT CERN-APCUR FROM CERN-EARN
                               GIVING CERN-EARNINGS-OWING
              MOVE CERN-APCUR TO CERN-EARNINGS-ERNINT
              MOVE 0 TO CERN-EARN
           ELSE
              MOVE CERN-EARN TO CERN-EARNINGS-ERNINT
              MOVE 0 TO CERN-EARNINGS-OWING
              COMPUTE CERN-EARN = CERN-APCUR - CERN-EARN.
      *--------------------------------------------------------
      * PRORATE INTEREST PAID OVER DISCOUNT
      *
           IF LN-ERN-IB-PRINBAL = 0
              MOVE 0 TO CERN-EARN
           ELSE
              COMPUTE CERN-EARN ROUNDED =
                  CERN-EARN / LN-ERN-IB-PRINBAL
                         * (LN-ANTICERN(5) - LN-ACCERN(5)).

       COMPUTE-CERNINGS-16-EXIT.
           EXIT.

      ***********************************************************
      *                        (17)
      *     PERCENTAGE OF THE ORIGINAL CHARGE
      *
      *                INTEREST INCOME [RE: (12)]
      *     THE % = -----------------------------------
      *                ORIGINAL INTEREST CHARGE
      *
      *     [RE: 12] IS ACTUARIAL BASED ON TIME OF PAYMENT
      **********************************************************
       COMPUTE-CERNINGS-17 SECTION.
           MOVE CERN TO CERN-SAVE.
           MOVE 1 TO CERN.
           PERFORM COMPUTE-CERNINGS-12-21.
           MOVE CERN-SAVE TO CERN.

           COMPUTE REGZ-UNEARNED ROUNDED =
               (LN-ANTICERN(1) - LN-ACCERN(1) - CERN-EARN)
                     / LN-ANTICERN(1) * LN-ANTICERN(CERN).

           COMPUTE CERN-EARN = LN-ANTICERN(CERN)
                         - REGZ-UNEARNED - LN-ACCERN(CERN).

       COMPUTE-CERNINGS-17-EXIT.
           EXIT.

      *******************************************************
      *                 (18)
      *    COMPUTE EARNINGS ACTUARIAL AS OF PAYMENT DATE
      *
      *******************************************************
       COMPUTE-CERNINGS-18 SECTION.
           PERFORM CERN-SAVE-LN-SP.
           MOVE "A" TO SP-DISFRMLA.

      * DETERMINE REQUIREMENT FOR ESTABLISHING A NEW
      * APR FOR ACTUARIAL REBATE CALCULATION:
           IF CERN = 1
              PERFORM CERN-SET-WORKERS-CERN1
              PERFORM CERN-ACTUARIAL-REBATE-NEW
              COMPUTE CERN-EARN = LN-ANTICERN(CERN)
                         - REGZ-UNEARNED - LN-ACCERN(CERN)
              IF CERN-FIX-MODE-FG = "Y"
                 IF REGZ-UNEARNED > LN-ANTICERN(1)
                    COMPUTE CERN-EARN = -1 * LN-ACCERN(1)
                 ELSE
                    NEXT SENTENCE
              ELSE
                 IF REGZ-UNEARNED > LN-ANTICERN(1)
                    MOVE 0 TO CERN-EARN.

           PERFORM CERN-RESTORE-LN-SP.

       COMPUTE-CERNINGS-18-EXIT.
           EXIT.

      *******************************************************
      *                 (19)
      *    COMPUTE EARNINGS ACTUARIAL AS OF PAYMENT DATE
      *      THIS FORMULA IS THE OLD VERSION OF 12
      *
      *      AND IS NOT CORRECTLY HANDLING MIDSTREAMS
      *      BUT THIS IS THE WAY FAIRCO WANTS TO CONTINUE
      *
      *******************************************************
       COMPUTE-FAIRCO-19 SECTION.
           PERFORM CERN-SAVE-LN-SP.
           MOVE "A" TO SP-DISFRMLA.

      * DETERMINE REQUIREMENT FOR ESTABLISHING A NEW
      * APR FOR ACTUARIAL REBATE CALCULATION:
           IF CERN = 1
              PERFORM CERN-SET-WORKERS-CERN1
              PERFORM CERN-ACTUARIAL-REBATE-NEW
           ELSE
              PERFORM CERN-SET-WORKERS-NEW
              IF LN-SALEFIN = "Y"
                 MOVE CERN-WK-PURDATE        TO NUM-DATE
                 MOVE CERN-WK-ORIG-1STPYDATE TO SYS-DATE
                 IF SYS-DATE < NUM-DATE
                          OR (LN-ORIGPURAMT NOT = LN-LNAMT)
                    PERFORM CERN-MIDSTREAM-NEW
                 END-IF
              END-IF
              PERFORM CERN-GET-APR-COMPONENT-NEW
              PERFORM CERN-ACTUARIAL-REBATE-NEW.

           COMPUTE CERN-EARN = LN-ANTICERN(CERN)
                         - REGZ-UNEARNED - LN-ACCERN(CERN).

              IF CERN = 1
                 IF CERN-FIX-MODE-FG = "Y"
                    IF REGZ-UNEARNED > LN-ANTICERN(1)
                       COMPUTE CERN-EARN = -1 * LN-ACCERN(1)
                    ELSE
                       NEXT SENTENCE
                 ELSE
                    IF REGZ-UNEARNED > LN-ANTICERN(1)
                       MOVE 0 TO CERN-EARN.


       COMPUTE-FAIRCO-19-EXIT.
           EXIT.

      *************************************************
      *    SAVE OFF LOAN & SPR FIELDS
      *************************************************
       CERN-SAVE-LN-SP SECTION.
           MOVE LN-1STPYAMT       TO CERN-SV-1STPYAMT.
           MOVE LN-REGPYAMT       TO CERN-SV-REGPYAMT.
           MOVE LN-LASTPYAMT      TO CERN-SV-LASTPYAMT.
           MOVE LN-APRATE         TO CERN-SV-APRATE.
           MOVE LN-INTDATE        TO CERN-SV-INTDATE.
           MOVE LN-LOANDATE       TO CERN-SV-LOANDATE.
           MOVE LN-ORIG-1STPYDATE TO CERN-SV-1STPYDATE.
           MOVE LN-ORGTERM        TO CERN-SV-ORGTERM.
           MOVE LN-NOREGPY        TO CERN-SV-NOREGPY.
           MOVE LN-ANTICTERM      TO CERN-SV-ANTICTERM.

           MOVE SP-REBTBL(7)      TO CERN-SV-REBTBL.
           MOVE SP-YEARTYPE       TO CERN-SV-YEARTYPE.
           MOVE SP-DISFRMLA       TO CERN-SV-DISFRMLA.

      *************************************************
      *    RESTORE LOAN & SPR FIELDS
      *************************************************
       CERN-RESTORE-LN-SP SECTION.
           MOVE CERN-SV-1STPYAMT TO LN-1STPYAMT.
           MOVE CERN-SV-REGPYAMT TO LN-REGPYAMT.
           MOVE CERN-SV-LASTPYAMT TO LN-LASTPYAMT.
           MOVE CERN-SV-APRATE TO LN-APRATE.
           MOVE CERN-SV-INTDATE TO LN-INTDATE.
           MOVE CERN-SV-LOANDATE TO LN-LOANDATE.
           MOVE CERN-SV-1STPYDATE TO LN-ORIG-1STPYDATE.
           MOVE CERN-SV-ORGTERM TO LN-ORGTERM.
           MOVE CERN-SV-NOREGPY TO LN-NOREGPY.
           MOVE CERN-SV-ANTICTERM TO LN-ANTICTERM.

           MOVE CERN-SV-REBTBL TO SP-REBTBL(7).
           MOVE CERN-SV-YEARTYPE TO SP-YEARTYPE.
           MOVE CERN-SV-DISFRMLA TO SP-DISFRMLA.


      *************************************************
      *    ESTABLISH NEW APR   RE: CERN-EARNINGS-6
      *************************************************
       CERN-COMPUTE-APR-6 SECTION.
      *--------------------------------------------------------V
      *         --- LOGIC TO COMPUTE APR FOR ALL COMPONENTS
      *---------------------------------------------------------
      *    IF (LN-SALEFIN = "Y") AND (LN-ORIGPURAMT NOT = LN-LNAMT)
      *       COMPUTE DISCLOSED-FINAMT =
      *           LN-ORIGPURAMT - (LN-ANTICERN(1) + LN-ANTICERN(3)
      *                                           + LN-ANTICERN(4))
      *    ELSE
      *       PERFORM LOAN-CALCULATIONS.

      *    MOVE DISCLOSED-FINAMT TO APR-FINANCED.
      *    MOVE LN-ORGTERM TO APR-ORGTERM.

      *    MOVE LN-1STPYAMT TO APR-1STPYAMT.
      *    MOVE LN-REGPYAMT TO APR-REGPYAMT.
      *    MOVE LN-LASTPYAMT TO APR-LASTPYAMT.

      *    MOVE LN-INTDATE TO APR-INTDATE.
      *    MOVE LN-ORIG-1STPYDATE TO APR-1STPYDATE.
      *    MOVE LN-UNITPER-CD TO APR-UNITPER-CD.
      *    MOVE LN-UNITPER-FREQ TO APR-UNITPER-FREQ.
      *    MOVE SP-DISFRMLA TO APR-DISFRMLA.
      *    PERFORM APR-REGZ.
      *    MOVE APR-APR TO LN-APRATE.
      *--------------------------------------------------------^

      * SETUP NEW APR
           IF (LN-SALEFIN = "Y") AND (LN-ORIGPURAMT NOT = LN-LNAMT)
              COMPUTE DISCLOSED-FINAMT =
                   LN-ORIGPURAMT - (LN-ANTICERN(1) + LN-ANTICERN(3)
                                                    + LN-ANTICERN(4))
           ELSE
              PERFORM LOAN-CALCULATIONS.
           MOVE DISCLOSED-FINAMT TO APR-FINANCED.

      *----------------
           ADD DISCLOSED-FINAMT TOTAL-INSFINCHG TOTAL-FEEFINCHG
                GIVING APR-FINANCED.
      *----------------


           MOVE LN-ORGTERM TO APR-ORGTERM.
           COMPUTE CERN-WORKER =
                LN-ANTICERN(1) + LN-ANTICERN(3)
                  + LN-ANTICERN(4) + LN-UFERN(CERN)
                   - (LN-ANTICERN(CERN) + LN-ANTICADJ(CERN)).

           COMPUTE CERN-WORKER ROUNDED =
                          CERN-WORKER / LN-ANTICTERM.
           SUBTRACT CERN-WORKER FROM LN-REGPYAMT
                                   GIVING APR-REGPYAMT ROUNDED
                                          LN-REGPYAMT ROUNDED.
           IF LN-1STPYAMT > 0
              IF CERN-WORKER < LN-1STPYAMT
                 SUBTRACT CERN-WORKER FROM LN-1STPYAMT
                                   GIVING LN-1STPYAMT ROUNDED.
           MOVE LN-1STPYAMT TO APR-1STPYAMT.
           IF LN-LASTPYAMT > 0
              IF CERN-WORKER < LN-LASTPYAMT
                 SUBTRACT CERN-WORKER FROM LN-LASTPYAMT
                                  GIVING LN-LASTPYAMT ROUNDED.
           MOVE LN-LASTPYAMT TO APR-LASTPYAMT.
           MOVE LN-INTDATE TO APR-INTDATE.
           MOVE LN-ORIG-1STPYDATE TO APR-1STPYDATE.
           MOVE LN-UNITPER-CD TO APR-UNITPER-CD.
           MOVE LN-UNITPER-FREQ TO APR-UNITPER-FREQ.
           MOVE SP-DISFRMLA TO APR-DISFRMLA.
           PERFORM APR-REGZ.
           MOVE APR-APR TO LN-APRATE.

      *********************************************
      *    CALCULATE EXTRA WHOLE PAYMENTS
      *********************************************
       CALC-EXTRA-WPAYS SECTION.
           MOVE LN-1STPYDATE TO NDTE-DATE.
           DIVIDE SP-ERNDELQ-STOP BY 30 GIVING NDTE-HOLD.
           PERFORM INCREMENT-MONTHS.

           IF LN-SALEFIN = "Y" AND SP-ERNACCRL-START-CD(CERN) = " "
              MOVE LN-PURDATE TO NUM-DATE
           ELSE
              MOVE LN-INTDATE TO NUM-DATE.
           MOVE NDTE-DATE TO SYS-DATE.
           IF NUM-MO > S-MM
              ADD 12 TO S-MM.
           COMPUTE CERN-WPAYS = CERN-WPAYS + (S-MM - NUM-MO).

           IF LN-SALEFIN = "Y" AND SP-ERNACCRL-START-CD(CERN) = " "
              MOVE LN-PURDATE TO NDTE-DATE NUM-DATE
           ELSE
              MOVE LN-INTDATE TO NDTE-DATE NUM-DATE.
           IF SP-ERNACCRL-FG(CERN) NOT = "C"
              MOVE 31 TO NDTE-DD
              PERFORM NEW-DATE-CALCULATION.

      * FORCE LAST DAY OF MONTH FOR ENDDATE (SYS-DATE):
           MOVE 31 TO NDTE-DD.
           PERFORM NEW-DATE-CALCULATION.
           MOVE NDTE-DATE TO SYS-DATE.

           MOVE 360 TO ELAPSED-YRTYPE.
           PERFORM TIMALL.

           IF (SP-ERNACCRL-FG(CERN) = "A" AND NDTE-DATE = NUM-DATE) OR
              (SP-ERNACCRL-FG(CERN) = "B") OR
              (SP-ERN-DAYS(CERN) > 0 AND SP-ERNACCRL-FG(CERN) = "C"
                               AND ELAPSED-REM < SP-ERN-DAYS(CERN))
                SUBTRACT 1 FROM CERN-WPAYS.

           IF SP-ERNACCRL-DFCD(CERN) = "A"
              SUBTRACT LN-TOTNODEF FROM CERN-WPAYS.

           IF SP-ERNFRMLA(CERN) = 51 AND
              SP-ERNDELQ-START = 8
              GO TO EXIT-CALC-EXTRA-WPAYS.

       CHECK-PERCENT.
           COMPUTE CERN-WORKER = CERN-PPAYS * 100.

           MOVE 0 TO CERN-PPAYS.

           IF CERN-WORKER > SP-CONTPDTH
              ADD 1 TO CERN-WPAYS.

       EXIT-CALC-EXTRA-WPAYS.
           EXIT.

      **********************************************************
      *    SET LOAN AND SPR WORKERS FOR INTEREST
      **********************************************************
       CERN-SET-WORKERS-CERN1 SECTION.
           MOVE LN-LOANDATE       TO CERN-WK-LOANDATE.
           MOVE LN-INTDATE        TO CERN-WK-INTDATE.
           MOVE LN-PURDATE        TO CERN-WK-PURDATE.
           MOVE LN-ORIG-1STPYDATE TO CERN-WK-ORIG-1STPYDATE.
           MOVE LN-1STPYDATE      TO CERN-WK-1STPYDATE.
           MOVE LN-ORGTERM        TO CERN-WK-ORGTERM.
           MOVE LN-SUM-ORGTERM    TO CERN-WK-SUM-ORGTERM.
           MOVE LN-1STPYAMT       TO CERN-WK-1STPYAMT.
           MOVE LN-NOREGPY        TO CERN-WK-NOREGPY.
           MOVE LN-REGPYAMT       TO CERN-WK-REGPYAMT.
           MOVE LN-LASTPYAMT      TO CERN-WK-LASTPYAMT.
           MOVE LN-ORIGPURAMT     TO CERN-WK-ORIGPURAMT.
           ADD LN-INTCHG LN-EXTCHG GIVING CERN-WK-ANTICERN.
           MOVE LN-ANTICADJ(1)    TO CERN-WK-ANTICADJ.
           MOVE LN-ACCERN(1)      TO CERN-WK-ACCERN.
           MOVE LN-UFERN(1)       TO CERN-WK-UFERN.
           SUBTRACT CERN-WK-UFERN FROM CERN-WK-ANTICERN
                                         GIVING CERN-WK-FINCHG.
           MOVE LN-ORGTERM        TO CERN-WK-ANTICTERM.
           MOVE LN-TOTNODEF       TO CERN-WK-TOTNODEF.
           MOVE LN-APRATE         TO CERN-WK-APRATE.
           MOVE SP-DISFRMLA       TO CERN-WK-DISFRMLA.
           MOVE LN-REBATE(4)      TO CERN-WK-REBATE.
           MOVE " "               TO CERN-WK-INCLUDE-DEF-FG.

      **********************************************************
      *    SET LOAN AND SPR WORKERS FOR OTHER THAN INTEREST
      **********************************************************
       CERN-SET-WORKERS-NEW SECTION.
           MOVE LN-LOANDATE TO CERN-WK-LOANDATE.
           MOVE LN-INTDATE TO CERN-WK-INTDATE.
           MOVE LN-PURDATE TO CERN-WK-PURDATE.
           MOVE LN-ORIG-1STPYDATE TO CERN-WK-ORIG-1STPYDATE.
           MOVE LN-1STPYDATE TO CERN-WK-1STPYDATE.
           MOVE LN-ORGTERM TO CERN-WK-ORGTERM.
           MOVE LN-SUM-ORGTERM TO CERN-WK-SUM-ORGTERM.
           MOVE LN-1STPYAMT TO CERN-WK-1STPYAMT.
           MOVE LN-NOREGPY TO CERN-WK-NOREGPY.
           MOVE LN-REGPYAMT TO CERN-WK-REGPYAMT.
           MOVE LN-LASTPYAMT TO CERN-WK-LASTPYAMT.
           MOVE LN-ORIGPURAMT TO CERN-WK-ORIGPURAMT.
           MOVE LN-ANTICERN(CERN) TO CERN-WK-ANTICERN.
           MOVE LN-ANTICADJ(CERN) TO CERN-WK-ANTICADJ.
           MOVE LN-ACCERN(CERN) TO CERN-WK-ACCERN.
           MOVE LN-UFERN(CERN) TO CERN-WK-UFERN.
           SUBTRACT CERN-WK-UFERN FROM CERN-WK-ANTICERN
                                         GIVING CERN-WK-FINCHG.
           MOVE LN-ANTICTERM TO CERN-WK-ANTICTERM.
           MOVE LN-TOTNODEF TO CERN-WK-TOTNODEF.
           MOVE LN-APRATE TO CERN-WK-APRATE.
           MOVE SP-DISFRMLA TO CERN-WK-DISFRMLA.
           IF CERN = 1
              MOVE LN-REBATE(4) TO CERN-WK-REBATE
           ELSE
              IF CERN = 3
                 MOVE LN-REBATE(5) TO CERN-WK-REBATE
              ELSE
                 IF CERN = 4
                    MOVE LN-REBATE(6) TO CERN-WK-REBATE
                 ELSE
                    MOVE " " TO CERN-WK-REBATE.

      ***************************************************
      *    SETUP FOR MIDSTREAM PAPER ACTUARIAL CALC
      ***************************************************
       CERN-MIDSTREAM-NEW SECTION.
           MOVE CERN-WK-PURDATE TO CERN-WK-INTDATE
                                   CERN-WK-LOANDATE.

      * DETERMINE ELAPSED TIME REMAINING BETWEEN
      * PURCHASE DATE AND CURRECT MATURITY EXCLUDING DEFERMENTS:
           MOVE "Y" TO CERN-MDTE-FLAG
                       CERN-MDTE-ORGTERM-FG.
           PERFORM CERN-MATURITY-DATE-CALC.

           MOVE CERN-WK-PURDATE TO NUM-DATE.
           MOVE CERN-MDTE-DATE TO SYS-DATE.
           MOVE LN-UNITPER-CD TO DATER-UNITPER-CD.
           MOVE LN-UNITPER-FREQ TO DATER-UNITPER-FREQ.
           PERFORM TIMUPER.

      * SET FRACTIONAL PERIOD FROM PAYDATE TO NEXT PAYMENT DUE DATE:
           MOVE ELAPSED-UNITPER-REM TO CERN-PPAYS.

      * SET WHOLE PERIOD(S) FROM PURCHASE DATE TO MATURITY:
           MOVE ELAPSED-UNITPER TO CERN-WPAYS.
           IF ELAPSED-UNITPER-REM NOT = 0
              ADD 1 TO CERN-WPAYS.

           IF CERN-WPAYS < CERN-WK-ORGTERM
              MOVE CERN-WPAYS TO CERN-WK-ANTICTERM
                                 CERN-WK-ORGTERM
              MOVE CERN-WK-REGPYAMT TO CERN-WK-1STPYAMT
              SUBTRACT 1 FROM CERN-WK-ORGTERM
                                        GIVING CERN-WK-NOREGPY
              IF CERN-WK-LASTPYAMT NOT = 0
                 SUBTRACT 1 FROM CERN-WK-NOREGPY
              ELSE
                 NEXT SENTENCE
           ELSE
              MOVE CERN-WK-ORGTERM TO CERN-WK-ANTICTERM.

           MOVE CERN-WK-1STPYDATE TO NUM-DATE.
           MOVE CERN-WK-PURDATE TO SYS-DATE.
           PERFORM TIMUPER.
           IF ELAPSED-UNITPER NOT < 0
              PERFORM CERN-MIDSTREAM-NEW-PUR-AFTER.

      * ADJUST 1ST PAYMENT FOR LOAN DELINQUENCY OR PAID AHEAD
      * AT MIDSTREAM PURCHASE TIME:
           COMPUTE CERN-WORKER =
               CERN-WK-ORIGPURAMT
                  - ( CERN-WK-NOREGPY * CERN-WK-REGPYAMT
                                         + CERN-WK-LASTPYAMT ).
           IF CERN-WORKER > 0
              MOVE CERN-WORKER TO CERN-WK-1STPYAMT
              MOVE CERN-WK-1STPYDATE TO CERN-WK-ORIG-1STPYDATE
              GO TO CERN-MIDSTREAM-NEW-EXIT.

      *---------------------------------------------------------
      *  EXAMPLE:
      *                       TERM = 1     TERM = 2     TERM = 6
      *                     -----------  -----------  -----------
      *   LN-1STPYAMT    :  $100.00      $100.00      $100.00
      *   LN-REGPYAMT    :  $100.00 X 0  $120.00 X 0  $120.00 X 4
      *   LN-LASTPYAMT :  $  0.00      $120.00      $130.00
      *                      ------       ------       ------
      *      TOTAL REPAY :  $100.00      $220.00      $710.00
      *---------------------------------------------------------
           MOVE 0 TO CERN-WK-ANTICTERM
                     CERN-WK-ORGTERM.
           MOVE CERN-WK-ORIGPURAMT TO CERN-WORKER.
           IF CERN-WORKER > CERN-WK-LASTPYAMT
              ADD 1 TO CERN-WK-ANTICTERM
                       CERN-WK-ORGTERM
              SUBTRACT CERN-WK-LASTPYAMT FROM CERN-WORKER
           ELSE
      * SETUP FOR SINGLE PAY LOAN:
              MOVE CERN-WORKER TO CERN-WK-1STPYAMT
                                  CERN-WK-REGPYAMT
              MOVE 0 TO CERN-WK-NOREGPY
                        CERN-WK-LASTPYAMT
              MOVE 1 TO CERN-WK-ANTICTERM
                        CERN-WK-ORGTERM
              GO TO CERN-MIDSTREAM-NEW-1ST.
           DIVIDE CERN-WK-REGPYAMT INTO CERN-WORKER
                                         GIVING CERN-WORKNO.
           IF CERN-WORKNO = 0
      * SETUP FOR TWO PAY LOAN:
              MOVE CERN-WORKER TO CERN-WK-1STPYAMT
              MOVE CERN-WK-LASTPYAMT TO CERN-WK-REGPYAMT
              MOVE 0 TO CERN-WK-NOREGPY
              MOVE 2 TO CERN-WK-ANTICTERM
                        CERN-WK-ORGTERM
              GO TO CERN-MIDSTREAM-NEW-1ST.

           MOVE CERN-WORKNO TO CERN-WK-NOREGPY.
           COMPUTE CERN-WORKER =
                 CERN-WORKER - CERN-WORKNO * CERN-WK-REGPYAMT.
           ADD CERN-WORKNO TO CERN-WK-ANTICTERM
                              CERN-WK-ORGTERM.
           IF CERN-WORKER = 0
      * 1STPYAMT = REGPYAMT:
              MOVE CERN-WK-REGPYAMT TO CERN-WK-1STPYAMT
              SUBTRACT 1 FROM CERN-WK-NOREGPY
              GO TO CERN-MIDSTREAM-NEW-1ST.

      * ODD FIRST PAYMENT:
           MOVE CERN-WORKER TO CERN-WK-1STPYAMT.
           ADD 1 TO CERN-WK-ANTICTERM
                    CERN-WK-ORGTERM.

        CERN-MIDSTREAM-NEW-1ST.
           MOVE CERN-MDTE-DATE TO NDTE-DATE.
           ADD 1 CERN-WK-NOREGPY GIVING NDTE-HOLD.
           IF CERN-WK-LASTPYAMT = 0
              SUBTRACT 1 FROM NDTE-HOLD.
           MULTIPLY -1 BY NDTE-HOLD.
           PERFORM INCREMENT-UNITPER.
           MOVE NDTE-DATE TO CERN-WK-ORIG-1STPYDATE.

       CERN-MIDSTREAM-NEW-EXIT.
           EXIT.

        CERN-MIDSTREAM-NEW-PUR-AFTER SECTION.
           MOVE CERN-WK-1STPYDATE TO NDTE-DATE.
           MOVE ELAPSED-UNITPER TO NDTE-HOLD.
           IF ELAPSED-UNITPER-REM NOT < 0
              ADD 1 TO NDTE-HOLD.
           PERFORM INCREMENT-UNITPER.
           MOVE NDTE-DATE TO CERN-WK-1STPYDATE.

      ***************************************************
      *    GET NEW APR FOR COMPONENT
      ***************************************************
       CERN-GET-APR-COMPONENT-NEW SECTION.
           IF (LN-SALEFIN = "Y")
                      AND (SP-ERNFRMLA(CERN) NOT = 21)
              MOVE CERN-WK-PURDATE TO CERN-WK-INTDATE
                                      CERN-WK-LOANDATE.

           COMPUTE CERN-WK-FINANCED =
                 CERN-WK-1STPYAMT
                     + (CERN-WK-NOREGPY * CERN-WK-REGPYAMT)
                      + CERN-WK-LASTPYAMT
                       + CERN-WK-UFERN
                        - (CERN-WK-ANTICERN + CERN-WK-ANTICADJ).

           MOVE " " TO CERN-WK-INCLUDE-DEF-FG.
           PERFORM CERN-CALC-APR-NEW.

      ***************************************************
      *    CALCULATE NEW APR
      ***************************************************
       CERN-CALC-APR-NEW SECTION.
           MOVE CERN-WK-1STPYAMT TO APR-1STPYAMT.
           MOVE CERN-WK-REGPYAMT TO APR-REGPYAMT.
           MOVE CERN-WK-LASTPYAMT TO APR-LASTPYAMT.
           MOVE CERN-WK-ORGTERM TO APR-ORGTERM.
           MOVE CERN-WK-INTDATE TO APR-INTDATE.
           MOVE CERN-WK-ORIG-1STPYDATE TO APR-1STPYDATE.
           MOVE CERN-WK-FINANCED TO APR-FINANCED.
           MOVE LN-UNITPER-CD TO APR-UNITPER-CD.
           MOVE LN-UNITPER-FREQ TO APR-UNITPER-FREQ.

           MOVE CERN-WK-DISFRMLA TO APR-DISFRMLA.
           PERFORM APR-REGZ.
           MOVE APR-APR TO CERN-WK-APRATE.

      ************************************
      *    COMPUTE ACTUARIAL REBATE
      ************************************
       CERN-ACTUARIAL-REBATE-NEW SECTION.
           MOVE CERN-PAYDATE TO REGZ-DATE.
           MOVE CERN-WK-FINCHG TO REGZ-FINCHG.
           MOVE CERN-WK-LOANDATE TO REGZ-LOANDATE.
           MOVE CERN-WK-INTDATE TO REGZ-INTDATE.
           MOVE CERN-WK-ORIG-1STPYDATE TO REGZ-1STPYDATE.
           MOVE CERN-WK-ORGTERM TO REGZ-ORGTERM.
           MOVE CERN-WK-1STPYAMT TO REGZ-1STPYAMT.
           MOVE CERN-WK-REGPYAMT TO REGZ-REGPYAMT.
           MOVE CERN-WK-LASTPYAMT TO REGZ-LASTPYAMT.
           MOVE CERN-WK-APRATE TO REGZ-APRATE.
           MOVE CERN-WK-DISFRMLA TO REGZ-DISFRMLA.
           MOVE LN-UNITPER-CD TO REGZ-UNITPER-CD.
           MOVE LN-UNITPER-FREQ TO REGZ-UNITPER-FREQ.
           MOVE CERN-WK-INCLUDE-DEF-FG TO REGZ-INCLUDE-DEF-FG.
           MOVE CERN-WK-TOTNODEF TO REGZ-TOTNODEF.

           PERFORM REGZ-UNEARNED-CALC.

       CERN-MATURITY-DATE-CALC SECTION.
           MOVE CERN-WK-1STPYDATE TO NDTE-DATE.
           MOVE 0 TO NDTE-HOLD.
           IF CERN-MDTE-FLAG = " "
              MOVE CERN-WK-TOTNODEF TO NDTE-HOLD.

           IF CERN-MDTE-ORGTERM-FG = "Y"
              COMPUTE NDTE-HOLD = NDTE-HOLD + (CERN-WK-ORGTERM - 1)
           ELSE
              COMPUTE NDTE-HOLD = NDTE-HOLD + (CERN-WK-SUM-ORGTERM - 1).

           MOVE LN-UNITPER-CD TO DATER-UNITPER-CD.
           MOVE LN-UNITPER-FREQ TO DATER-UNITPER-FREQ.
           PERFORM INCREMENT-UNITPER.
           MOVE NDTE-DATE TO CERN-MDTE-DATE.
           MOVE " " TO CERN-MDTE-FLAG
                       CERN-MDTE-ORGTERM-FG.

      ***************************************************
      *    SETUP FOR MIDSTREAM PAPER ACTUARIAL CALC
      *      FORMULA 19, FAIRCO VERSION
      ***************************************************
       CERN-MIDSTREAM-NEW-19 SECTION.
           MOVE CERN-WK-PURDATE TO CERN-WK-INTDATE
                                   CERN-WK-LOANDATE.

      * DETERMINE ELAPSED TIME REMAINING BETWEEN
      * PAYDATE AND ORIGINAL MATURITY:
           MOVE "Y" TO CERN-MDTE-FLAG
                       CERN-MDTE-ORGTERM-FG.
           PERFORM CERN-MATURITY-DATE-CALC.

           MOVE CERN-WK-PURDATE TO NUM-DATE.
           MOVE CERN-MDTE-DATE TO SYS-DATE.
           MOVE LN-UNITPER-CD TO DATER-UNITPER-CD.
           MOVE LN-UNITPER-FREQ TO DATER-UNITPER-FREQ.
           PERFORM TIMUPER.

      * SET FRACTIONAL PERIOD FROM PAYDATE TO NEXT PAYMENT DUE DATE:
           MOVE ELAPSED-UNITPER-REM TO CERN-PPAYS.

      * SET WHOLE PERIOD(S) FROM PURCHASE DATE TO MATURITY:
           MOVE ELAPSED-UNITPER TO CERN-WPAYS.
           IF ELAPSED-UNITPER-REM NOT = 0
              ADD 1 TO CERN-WPAYS.

           IF CERN-WPAYS < CERN-WK-ORGTERM
              MOVE CERN-WPAYS TO CERN-WK-ANTICTERM
                                 CERN-WK-ORGTERM
              MOVE CERN-WK-REGPYAMT TO CERN-WK-1STPYAMT
              SUBTRACT 1 FROM CERN-WK-ORGTERM
                                        GIVING CERN-WK-NOREGPY
              IF CERN-WK-LASTPYAMT NOT = 0
                 SUBTRACT 1 FROM CERN-WK-NOREGPY
              ELSE
                 NEXT SENTENCE
           ELSE
              MOVE CERN-WK-ORGTERM TO CERN-WK-ANTICTERM.

           MOVE CERN-WK-ORIG-1STPYDATE TO NUM-DATE.
           MOVE CERN-WK-PURDATE TO SYS-DATE.
           PERFORM TIMUPER.
           IF ELAPSED-UNITPER NOT < 0
              PERFORM CERN-MIDSTREAM-NEW-19-AFTER.

      * ADJUST 1ST PAYMENT FOR LOAN DELINQUENCY OR PAID AHEAD
      * AT MIDSTREAM PURCHASE TIME:
           COMPUTE CERN-WORKER =
               CERN-WK-ORIGPURAMT
                  - ( CERN-WK-NOREGPY * CERN-WK-REGPYAMT
                                         + CERN-WK-LASTPYAMT ).
           IF CERN-WORKER > 0
              MOVE CERN-WORKER TO CERN-WK-1STPYAMT
              GO TO CERN-MIDSTREAM-NEW-19-EXIT.

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
           MOVE 0 TO CERN-WK-ANTICTERM
                     CERN-WK-ORGTERM.
           MOVE CERN-WK-ORIGPURAMT TO CERN-WORKER.
           IF CERN-WORKER > CERN-WK-LASTPYAMT
              ADD 1 TO CERN-WK-ANTICTERM
                       CERN-WK-ORGTERM
              SUBTRACT CERN-WK-LASTPYAMT FROM CERN-WORKER
           ELSE
      * SETUP FOR SINGLE PAY LOAN:
              MOVE CERN-WORKER TO CERN-WK-1STPYAMT
                                  CERN-WK-REGPYAMT
              MOVE 0 TO CERN-WK-NOREGPY
                        CERN-WK-LASTPYAMT
              MOVE 1 TO CERN-WK-ANTICTERM
                        CERN-WK-ORGTERM
              GO TO CERN-MIDSTREAM-NEW-19-1ST.
           DIVIDE CERN-WK-REGPYAMT INTO CERN-WORKER
                                         GIVING CERN-WORKNO.
           IF CERN-WORKNO = 0
      * SETUP FOR TWO PAY LOAN:
              MOVE CERN-WORKER TO CERN-WK-1STPYAMT
              MOVE CERN-WK-LASTPYAMT TO CERN-WK-REGPYAMT
              MOVE 0 TO CERN-WK-NOREGPY
              MOVE 2 TO CERN-WK-ANTICTERM
                        CERN-WK-ORGTERM
              GO TO CERN-MIDSTREAM-NEW-19-1ST.

           MOVE CERN-WORKNO TO CERN-WK-NOREGPY.
           COMPUTE CERN-WORKER =
                 CERN-WORKER - CERN-WORKNO * CERN-WK-REGPYAMT.
           ADD CERN-WORKNO TO CERN-WK-ANTICTERM
                              CERN-WK-ORGTERM.
           IF CERN-WORKER = 0
      * 1STPYAMT = REGPYAMT:
              MOVE CERN-WK-REGPYAMT TO CERN-WK-1STPYAMT
              SUBTRACT 1 FROM CERN-WK-NOREGPY
              GO TO CERN-MIDSTREAM-NEW-19-1ST.

      * ODD FIRST PAYMENT:
           MOVE CERN-WORKER TO CERN-WK-1STPYAMT.
           ADD 1 TO CERN-WK-ANTICTERM
                    CERN-WK-ORGTERM.

        CERN-MIDSTREAM-NEW-19-1ST.
           MOVE CERN-MDTE-DATE TO NDTE-DATE.
           ADD 1 CERN-WK-NOREGPY GIVING NDTE-HOLD.
           IF CERN-WK-LASTPYAMT = 0
              SUBTRACT 1 FROM NDTE-HOLD.
           MULTIPLY -1 BY NDTE-HOLD.
           PERFORM INCREMENT-UNITPER.
           MOVE NDTE-DATE TO CERN-WK-ORIG-1STPYDATE.

           GO TO CERN-MIDSTREAM-NEW-19-EXIT.

        CERN-MIDSTREAM-NEW-19-AFTER.
           MOVE CERN-WK-ORIG-1STPYDATE TO NDTE-DATE.
           MOVE ELAPSED-UNITPER TO NDTE-HOLD.
           IF ELAPSED-UNITPER-REM NOT < 0
              ADD 1 TO NDTE-HOLD.
           PERFORM INCREMENT-UNITPER.
           MOVE NDTE-DATE TO CERN-WK-ORIG-1STPYDATE.

       CERN-MIDSTREAM-NEW-19-EXIT.
           EXIT.

      *************************************************
      *                  (20)
      * COMPUTE EARNINGS PRO-RATA AT PAYOFF:
      *   EARNINGS AREN'T COMPUTED UNTIL TIME
      *   OF PAYOFF.   GRAND ISLAND, NE
      *************************************************
       COMPUTE-CERNINGS-20 SECTION.
           IF CERN-PRINBAL NOT = 0
              GO TO COMPUTE-CERNINGS-20-EXIT.

           COMPUTE CERN-EARN ROUNDED =
                (LN-ANTICERN(CERN) - LN-ACCERN(CERN)).

       COMPUTE-CERNINGS-20-EXIT.
           EXIT.

      *************************************************
      *                    (22)
      * COMPUTE EARNINGS BALANCE REMAINING METHOD
      *************************************************
       COMPUTE-CERNINGS-22 SECTION.
           IF LN-LOANTYPE = "I"
              COMPUTE CERN-WORKER =
                 LN-PURBAL - LN-OT2BAL - CERN-APCUR
              IF CERN-WORKER < LN-ANTICERN(CERN)
                 COMPUTE CERN-EARN =
                     LN-ANTICERN(CERN) - CERN-WORKER - LN-ACCERN(CERN)
              END-IF
           ELSE
              COMPUTE CERN-WORKER =
                 LN-PURBAL - LN-OT2BAL - CERN-APCUR
                        - ( LN-ANTICERN(1)
                                  - (LN-ACCERN(1) + CERN-EARNINGS(1))
                          )
              IF CERN-WORKER < LN-ANTICERN(CERN)
                 COMPUTE CERN-EARN =
                     LN-ANTICERN(CERN) - CERN-WORKER - LN-ACCERN(CERN).

      ********************************************************
      *                    (23)
      *    PRORATA EARN FORMULA, PERCENT OF TOTAL MONIES PAID
      *    SALES TAX PAACO FINANCE, 1469
      ********************************************************
       COMPUTE-CERNINGS-23 SECTION.
           MOVE LN-SUM-LNAMT TO CERN-WORKER.
           IF LN-LOANTYPE = "I"
              ADD LN-SUM-INTCHG LN-SUM-EXTCHG TO CERN-WORKER.
           COMPUTE CERN-EARN ROUNDED =
              (CERN-APCUR + CERN-APINT + CERN-APLC)
                      / CERN-WORKER
                       * (LN-ANTICERN(CERN) - LN-UFERN(CERN)).

      ********************************************************
      *                    (24)
      *     THIS CASH EARN FORMULA COMPUTES EARNINGS ON THE
      *     COMPONENT IN THE SAME RATIO OF THE COMBINED
      *     INTEREST EARNINGS AND SERVICE CHARGE EARNINGS
      *     TO THE COMBINED ORIGINAL INTEREST ANTICIPATED
      *     AND THE ORIGINAL SERVICE CHARGE ANTICIPATED
      *                                     MEXICO, WORLD #408
      ********************************************************
       COMPUTE-CERNINGS-24 SECTION.
           COMPUTE CERN-EARN ROUNDED =
              (CERN-LN-INT-EARN + CERN-LN-SC-EARN)
               * (LN-ANTICERN(4)) / (LN-ANTICERN(1) + LN-ANTICERN(3)).

      *-----------------------------------------------------------------------
      *    DUMMY SECTION TO INCLUDE IN COPY MEMBERS THAT NEED TO BE
      *    REFLECTED IN 'WHAT' COMMANDS FOR SUPPORT
      *-----------------------------------------------------------------------
       SCCS-COPY-ROUTINE SECTION. MOVE
           "@(#)./LIBLP/LPCERN.CPY A30 >"
           TO MESS.
      *-----------------------------------------------------------------------

