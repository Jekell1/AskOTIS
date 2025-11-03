      * COPYMEMBER: LIBLP/LPRATE
      **************************************************************
      *         SET INTEREST RATE AND INTEREST START DATE
      *                  (FOR IB ACCOUNTS)
      *
      *   NAME: LPRATE            (SET-PY-RATE             )
      *         WITH LPINDU LOGIC (INTEREST-DUE-CALCULATION)
      *   DESC: THIS ROUTINE IS USED TO DETERMINE
      *         THE INTEREST START DATE FOR CALCULATION,
      *         THE INTEREST BEARING RATE TO BE USED
      *         AND A CODE TO IDENTIFY IT'S MAKEUP.
      *         IT'S MAKEUP.
      *   IN  : RATE-DATE      (DATE OF PAYMENT)
      *         RATE-MAKERCD   LN-MAKERCD(X)
      *         RATE-LPTRCD    CURRENT TRANSACTION CODE.
      *   OUT :
      *   USED: LN-APRATE, LN-JDCD, LN-JDRATE, LN-INTREDU
      *         LN-ENDORATE, LN-OV, LPIBPC, LPIBPCW
      *         LN-INTPDTH-DATE, LN-INTDATE, LN-JDDATE
      *   COPY TIM, TIMW, LPRATEW, LPIBPC, LPIBPCW
      *
      * REV :
      *  042988 SLC ADDED THE REDUCTION, ACCELERATION,
      *             JUDGEMENT, AND EXPIRATION LOGIC.
      *             LPRATE NOW CALLS LPIBPC, LNINDU.
      *  012489 JTG SET IBPC-DATE FROM RATE-DATE
      *  012589 SLC LPINDU IS NOW PART OF THIS COPYFILE!
      *  110789 SLC COMPARE IS NOW DONE ON SP-MAX-IB-ACR-DAYS, NOT
      *             SP-IBINTACRFRMLA.
      *  051890 BAH REPLACES LN-DATE-PAID-LAST WITH LN-INTPDTH-DATE
      *             (INTERST ASSESSED THRU DATE 9(6)).
      *  111992 JTG ADDED LOGIC FOR SP-RBSPOPT1 = 9, WAGNER NC
      *  060495 SLC INDU-CURBAL (LN-CURBAL - LN-OT2BAL)
      *  111296 JTG CHANGED LN-INSTBL (LEVEL'S 9 & 10) TO FILLER
      *  012297 JTG ADDED SP-EXPR-FRMLA-ORIG TO USE THE LESSOR RATE
      *             (BETWEEN LN-OVRDRATE & EXPIRED RATES) ON ACCOUNTS
      *             NOT IN JUDGEMENT, ACCELERATED OR REDUCED
      *             --ADVANCED MORTGAGE, JUDY BRANIFF
      *  050697 JTG ADDED USE OF SETCTBL COPY MEMBER
      *  042798  CS ADDED RENEWAL CODES "RB" AND "RO"
      *  000107 JTG ADDED SP-RBREDUC = 9999.04 WISCONSIN IB REAL ESTATE
      *             PREPAY PENALTY, WISCONSIN #1031
      *  000225 MJD ADDED SP-RBREDUC = 9999.05 NEW SOUTH FLAT $75.00
      *             PREPAY PENALTY, (NEWS PR#1101)
      *  000406 GLB ADDED SP-RBREDUC = 9999.06 CAROLINA FINANCE(71252),
      *             NORTH CAROLINA 10% OR $25.00 (WHICHEVER IS < )
      *             PREPAY PENALTY, (CAROLINA PR#1149)
      *  000804 BAH NEW SOUTH, RBREDUC = 9999.03, WAS NOT STORING THE
      *             PENALTY IN INDU-PENALTY, IT WAS ADDING IT DIRECTLY
      *             INTO INDU-INTEREST,,,FAX FROM KEVIN
      *  JTG 000822 ADDED LOGIC FOR KENTUCKY 60 DAY NO CHARGE LAW, ADDED
      *             SP-RBSPOPT1() = 19, WORLD #233
      *  JTG 000914 CORRECTED BUG IN PASS TO SETCTBL, ROUTINE WAS PASSING
      *             LN-LNAMT INSTEAD OF INTEREST CHARGEABLE  WORLD #J011
      *  JTG 020722 CHANGED FOR (MIP) MONTHLY INSURANCE PREMIUMS   REGENCY #1801
      *  BAH 020828 ALLOWED RBREDUC(7) = 9999.01 TO WORK FOR IB'S, ADD PENALTY
      *             TO INDU-INTEREST, PIKCO PR# 1900
      *  JTG 021025 ALLOW WORLD TO ENTER 999.998 IN SP-ACC-RATE(1)
      *             AND IF DETECTED HERE SKIP USAGE OF ACCELERATION RATE
      *             TABLE AND FORCE LN-SMPRATE RE: LN-ACCRATE 999.998 WORLD     
      *  BAH 021216 PIKCO CHANGE (020828) CALCULATED A PENALTY ON EVERY
      *             PAYMENT !! NEEDS TO CHECK FOR "PO" AND "PA" ONLY
      *  BAH 040615 SP-RBSPOPT1(7) = 8, NEEDED TO INCLUDE LN-INTBAL BEFORE
      *             TESTING THE MINRET AMOUNT, PACESETTER PL# 446
      *
      *  JTG 041021 ADDED NEW LOGIC WHEN TESTING FOR INDU-MAX-ACCRUE = 1,
      *             THAT INDICATES THE ACCRUAL WANTS TO ESTABLISH
      *             THE MAXIMUM INTEREST TO ACCRUAL (W-MAX). THE ACCRUAL
      *             INCREMENTS LN-INTPDTH-DATE WITH SP-MAX-IB-ACR-DAYS
      *             TO ESTABLISH RATE-DATE FOR DETERMINING (W-MAX),
      *             THIS IS INCORRECT WHEN ACCOUNTS HAD STATUS CHANGE
      *             DONE AFTER LN-INTPDTH-DATE, EXAMPLE "JD" WHICH MAKES THE
      *             JUDGEMENT DATE THE NEW ASSESS FROM DATE THAT SHOULD
      *             BE INCREMENTED WITH SP-MAX-IB-ACR-DAYS.                           *                                                             REGWAN #0450
      *   CS 050518 ADD SP-EXPR-FRMLA 'C'   FINANCIAL MGT [PR#484]
      *             [HAVE QUESTIONS WAITING FOR JUDY R TO RESPOND]
      *   CS 050614 FIXED SP-EXPR-FRMLA 'C', IF REDUCTION FORMULA WAS TO
      *             CONTINUE REDUCED RATE WAS STILL PERFORMING EXPIRED RATES
      *  MJD 060906 ADDED LOGIC TO USE THE CORRECT INTEREST ASSESSED THROUGH
      *             DATE WHEN THE LOAN IS EXPIRED AND IN JUDGMENT,
      *             ACCELERATED OR REDUCTION STATUS.  THE DATE USED SHOULD
      *             BE THE DATE OF THE JUDGMENT, ACCELERATION OR REDUCTION IF
      *             THAT DATE IS GREATER THEN THE INTEREST PAID THROUGH DATE.
      *                                                             REGENCY #531
      *  MJD 061227 ADDED LOGIC FOR OHIO PREPAYMENT PENALTY (RBSPOPT1 = 25)
      *                                                            REGENCY #2878
      *  MJD 081210 TESTING LOGIC FOR MAKING MFEE'S WORK WITH IB
      *   CS 100609 ADDED INDU-ASSESS-PENALTY-16, ACCOUNTS THAT HAD AN INTEREST
      *             CALCULATION FROM LN-INTPDTH-DATE TO EXPIR DATE & THEN EXPIRED
      *             INT CALC THRU PAY DATE BOTH GO THRU INTEREST-DUE-CALCULATION
      *             AT END OF INTEREST-DUE-CALCUATION IT CALCULATES PENALTY &
      *             WAS CALCULATED TWICE RBSPOPT1 = 16  REGENCY PL689
      *   CS 100610 ABOVE ALSO HAPPENS WITH SP-RBREDUC(7) = 9999.03 USED THE
      *             FLAG INDU-ASSESS-PENALTY TO STOP MULTIPLE ASSESSMENTS OF
      *             PENALTY.  THIS SHOULD BE OK SINCE ONLY ONE PENALTY CAN BE
      *             ASSESSED & THEN IT EXITS THE PENALTY SECTION.  EVEN IF THE
      *             SPR IS SET UP WITH BOTH, IT WILL NEVER HIT BOTH PENALTY
      *             LOGIC. APPLIES TO SPECIAL OPTION1 = 16, 21, AND 25
      *             REDUCTION AMOUNTS 9999.03,9999.01 REGENCY PL#689
      *             ALSO ADDED HOLD-INDU-PENALTY
      *  MJD 120321 ADDED SP-OPT-CAL-CHGABLE = "3" THIS WILL USE PROCEEDS,
      *             SVC CHG, AND ALL INSURANCES EXCEPT FOR O3
      *   CS 120730 IF SP-LC-INT-ON-REPO-NON-PL = 'N' FORCE 0 TO LATE CHARGE
      *             RATE, AND 0 TO LATE CHARGE MINIMUM, WE WANT TO ADVANCE
      *             LCPDTH DATE EVEN THOUGH NOT COLLECTING OR ASSESSING THEM
      *                                                 REGACC PR#314
      *  BAH 160921 ADDED LT-REC ON THE CALL OF REBA01, LINKAGE ERROR AT
      *             CUSTOMER REBA01 WAS CHANGED 6 YEARS AGO! FIRST METRO #949
      *  BAH 180702 CHANGED A01 PASSING FIELDS TO BE IN LPRATEW SO REBATE 
      *             COPY MEMBERS DO NOT NEED TO BE IN HERE!
      *  CS 211123 AT THE BEGINNING OF SET-PY-RATE, SET INDU-ASSESS-PENALTY
      *            TO SPACES.
      *  BAH 2022.0131 ADDED CANCEL FORM-PROGX AFTER REBA02
      **************************************************************************
       SET-PY-RATE SECTION.
           MOVE SP-INTMETHOD TO INDU-INTMETHOD.
           MOVE SP-YEARTYPE TO INDU-YEARTYPE.
           MOVE 0 TO RATE-EXP-INT
                     INDU-INTEREST
                     INDU-INTEREST-2.
      *ADDED THIS, WHEN RUN IN EXTRACTIONS, AC1DAY, LNAGIB
      *FLAG WAS NOT GETTING RESET EVERY TIME
           MOVE SPACES TO INDU-ASSESS-PENALTY.

           IF SP-LC-INT-ON-REPO-NON-PL = "N"
              IF LN-REPOCD = "X" OR "S"
                IF LN-PLDATE = ZEROS
                  MOVE 0 TO INDU-INTEREST
                  GO TO SET-PY-RATE-EXIT.

      *------------------------------------------------
      *    999.999 - DEFAULT TO SPR RATES:
      *------------------------------------------------
           MOVE 999.999 TO INDU-RATE.

      *------------------------------------------------
      *    GET IBPC STATUS OF ACCOUNT
      *------------------------------------------------
           MOVE RATE-DATE TO IBPC-DATE.
           PERFORM IBPC-TEST.

      *------------------------------------------------
      *    SKIP PC ACCOUNTS THAT HAVE NOT EXPIRED
      *------------------------------------------------
           IF IBPC-FG = "P" AND IBPC-EXPIRED-FLAG = " "
              GO TO SET-PY-RATE-EXIT.

      *------------------------------------------------
      *    SETUP RATE FOR INTEREST CALCULATION
      *------------------------------------------------

      * IF JUDGEMENT SET RATE IF NOT (999.999):
      * APPLIES TO ACCELERATED, RATE CHANGED/REDUCED
      * AND EXPIRED LOANS THAT HAVE BECOME JUDGEMENTS.
           IF (IBPC-STAT = "J" AND IBPC-STAT-2 = " ") OR
              (IBPC-STAT-2 = "J" AND SP-EXPR-FRMLA NOT = "C")
              PERFORM LPRATE-JUDGEMENT
           ELSE
      * TEST FOR ACCOUNT IN EXPIRATION AND NO STATUS SET:
           IF SP-EXPR-FRMLA-USE-EXPIRED
                 AND (
                       (IBPC-STAT = "E" AND IBPC-STAT-2 = " ")
                       OR (IBPC-STAT = "J" AND IBPC-STAT-2 = "E"
                                            AND SP-JUDGFRMLA = "A")
                        OR
                             (IBPC-STAT = "A" AND IBPC-STAT-2 = "E"
                                             AND SP-ACC-FRMLA = "A")
                         OR
                              (IBPC-STAT = "R" AND IBPC-STAT-2 = "E"
                                        AND SP-EXPR-REDU-FRMLA = "A")

                         OR (IBPC-STAT = "E" AND IBPC-STAT-2 = "J")
                               AND (SP-EXPR-FRMLA = "C"
                                            AND SP-JUDGFRMLA = "A")


                     )
              PERFORM LPRATE-USE-EXPIRED-RATES
              GO TO SET-PY-RATE-EXIT
           ELSE
           IF IBPC-STAT = "J" AND IBPC-STAT-2 = "E"
              PERFORM LPRATE-JUDGEMENT
           ELSE
           IF IBPC-STAT = "A" AND IBPC-STAT-2 = "E"
              PERFORM LPRATE-ACCELERATION
           ELSE
           IF IBPC-STAT = "R" AND IBPC-STAT-2 = "E"
              MOVE LN-REDURATE TO INDU-RATE
           ELSE
           IF (IBPC-STAT = "A" AND IBPC-STAT-2 = " ")
                                      OR IBPC-STAT-2 = "A"
              PERFORM LPRATE-ACCELERATION
           ELSE
           IF (IBPC-STAT = "R" AND IBPC-STAT-2 = " ")
                                      OR IBPC-STAT-2 = "R"
              MOVE LN-REDURATE TO INDU-RATE
           ELSE
           IF IBPC-STAT = "E"
              MOVE LN-OVRDRATE TO INDU-RATE
           ELSE
           IF RATE-MAKERCD = "E"
              IF LN-ENDORATE NOT = 999.999
                 MOVE LN-ENDORATE TO INDU-RATE
              ELSE
              IF LN-OVRDRATE NOT = 999.999
                 MOVE LN-OVRDRATE TO INDU-RATE
              ELSE
                 NEXT SENTENCE
           ELSE
              IF LN-OVRDRATE NOT = 999.999
                 MOVE LN-OVRDRATE TO INDU-RATE.

      * SETUP (INDU-DATE-1) INTEREST START DATE:
           PERFORM LPRATE-SET-INTPDTH-DATE.

           IF (IBPC-STAT = "J" AND IBPC-STAT-2 = " ")
                 OR (IBPC-STAT-2 = "J")
                  OR (IBPC-STAT = "J"
                        AND IBPC-STAT-2 = "E"
                         AND (SP-JUDGFRMLA = " "
                                 OR (SP-JUDGFRMLA = "A"
                                         AND SP-EXPR-FRMLA-CONT)
                             )
                     )
              MOVE INDU-DATE-1 TO NUM-DATE
              MOVE LN-JDDATE   TO SYS-DATE
              IF SYS-DATE > NUM-DATE
                 MOVE LN-JDDATE TO INDU-DATE-1.

           IF (IBPC-STAT = "A" AND IBPC-STAT-2 = " ")
                 OR (IBPC-STAT-2 = "A")
                  OR (IBPC-STAT = "A"
                          AND IBPC-STAT-2 = "E"
                           AND (SP-ACC-FRMLA = " "
                                   OR (SP-ACC-FRMLA = "A"
                                           AND SP-EXPR-FRMLA-CONT)
                               )
                     )
              MOVE INDU-DATE-1  TO NUM-DATE
              MOVE LN-ACCELDATE TO SYS-DATE
              IF SYS-DATE > NUM-DATE
                 MOVE LN-ACCELDATE TO INDU-DATE-1.

           IF (IBPC-STAT = "R" AND IBPC-STAT-2 = " ")
                 OR (IBPC-STAT-2 = "R")
                  OR (IBPC-STAT = "R"
                        AND IBPC-STAT-2 = "E"
                         AND (SP-EXPR-REDU-FRMLA = " "
                                 OR (SP-EXPR-REDU-FRMLA = "A"
                                         AND SP-EXPR-FRMLA-CONT)
                             )
                     )
              MOVE INDU-DATE-1 TO NUM-DATE
              MOVE LN-REDUDATE TO SYS-DATE
              IF SYS-DATE > NUM-DATE
                 MOVE LN-REDUDATE TO INDU-DATE-1.

           MOVE RATE-LPTRCD TO INDU-LPTRCD.
           COMPUTE INDU-CURBAL = LN-CURBAL - LN-OT2BAL.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    NEW LOGIC TESTING FOR INDU-MAX-ACCRUE = 1, WHICH INDICATES
      *    THE ACCRUAL LP/AC1ERN WANTS TO ESTABLISH (W-MAX) THE MAXIMUM
      *    INTEREST TO ACCRUAL PER SP-MAX-IB-ACR-DAYS.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( INDU-MAX-ACCRUE = 1 AND SP-MAX-IB-ACR-DAYS NOT = 0 )
              IF ( SP-MAX-IB-ACR-DAYS NOT = 999 )
                 MOVE INDU-DATE-1        TO NDTE-DATE
                 MOVE SP-MAX-IB-ACR-DAYS TO NDTE-HOLD
                 PERFORM INCREMENT-DAYS
                 MOVE NDTE-DATE TO RATE-DATE.

           MOVE RATE-DATE TO INDU-DATE-2.
           PERFORM INTEREST-DUE-CALCULATION.

       SET-PY-RATE-EXIT.
           EXIT.

      ************************************************
      *    USE EXPIRED RATES
      ************************************************
       LPRATE-USE-EXPIRED-RATES SECTION.
           PERFORM LPRATE-SET-INTPDTH-DATE.
           IF IBPC-EXPIRED-FLAG = " "
              MOVE 0 TO RATE-EXP-INT
              PERFORM LPRATE-PREPARE-PMT-2
           ELSE
           IF IBPC-EXPIRED-FLAG = "E" AND IBPC-FG = "P"
              MOVE XDTE-DATE TO INDU-DATE-1
              MOVE 0 TO RATE-EXP-INT
              COMPUTE INDU-CURBAL = LN-CURBAL - LN-OT2BAL
              MOVE RATE-LPTRCD TO INDU-LPTRCD
              PERFORM LPRATE-PREPARE-PMT-2
           ELSE
           IF LN-STATUSFG = "J" AND IBPC-EXPIRED-FLAG = "E"
              MOVE INDU-DATE-1 TO NUM-DATE
              MOVE LN-JDDATE   TO SYS-DATE
              IF SYS-DATE > NUM-DATE
                 MOVE LN-JDDATE TO INDU-DATE-1
              END-IF
              PERFORM LPRATE-JUDGEMENT
              PERFORM LPRATE-CALC-IT
           ELSE
           IF LN-STATUSFG = "A" AND IBPC-EXPIRED-FLAG = "E"
              MOVE INDU-DATE-1  TO NUM-DATE
              MOVE LN-ACCELDATE TO SYS-DATE
              IF SYS-DATE > NUM-DATE
                 MOVE LN-ACCELDATE TO INDU-DATE-1
              END-IF
              PERFORM LPRATE-ACCELERATION
              PERFORM LPRATE-CALC-IT
           ELSE
           IF LN-STATUSFG = "R" AND IBPC-EXPIRED-FLAG = "E"
              MOVE INDU-DATE-1 TO NUM-DATE
              MOVE LN-REDUDATE TO SYS-DATE
              IF SYS-DATE > NUM-DATE
                 MOVE LN-REDUDATE TO INDU-DATE-1
              END-IF
              MOVE LN-REDURATE TO INDU-RATE
              PERFORM LPRATE-CALC-IT
           ELSE
           IF LN-STATUSFG = " " AND IBPC-EXPIRED-FLAG = "E"
              MOVE LN-OVRDRATE TO INDU-RATE
              PERFORM LPRATE-CALC-IT.


      ************************************************
      *    CALC IT ROUTINE
      ************************************************
       LPRATE-CALC-IT SECTION.
           PERFORM LPRATE-PREPARE-PMT-1.
           PERFORM INTEREST-DUE-CALCULATION.
           PERFORM LPRATE-PREPARE-PMT-2.

      ************************************************
      *    PREPARE-PMT-1 ROUTINE
      ************************************************
       LPRATE-PREPARE-PMT-1 SECTION.
           MOVE XDTE-DATE TO INDU-DATE-2.
           COMPUTE INDU-CURBAL = LN-CURBAL - LN-OT2BAL.
           MOVE RATE-LPTRCD TO INDU-LPTRCD.
           IF LN-STATUSFG = " " OR LN-STATUSFG = "R" AND
              INDU-RATE NOT = 999.997
              MOVE SP-INTMETHOD TO INDU-INTMETHOD
              MOVE SP-YEARTYPE  TO INDU-YEARTYPE
           ELSE
           IF LN-STATUSFG = " " AND INDU-RATE = 999.997
              MOVE SP-ACC-INTMETH TO INDU-INTMETHOD
              MOVE SP-ACC-YRTYPE  TO INDU-YEARTYPE.

      ************************************************
      *    PREPARE-PMT-2 ROUTINE
      ************************************************
       LPRATE-PREPARE-PMT-2 SECTION.
           MOVE INDU-INTEREST TO RATE-EXP-INT INDU-INTEREST-2.
           PERFORM LPRATE-SET-INTPDTH-DATE.
           MOVE INDU-DATE-1 TO NUM-DATE.
           MOVE XDTE-DATE TO SYS-DATE.
           IF SYS-DATE > NUM-DATE
              MOVE XDTE-DATE TO INDU-DATE-1.
           COMPUTE INDU-CURBAL = LN-CURBAL - LN-OT2BAL.
           MOVE RATE-LPTRCD TO INDU-LPTRCD.
           PERFORM LPRATE-EXPIRED-INT.
           IF (IBPC-STAT = "J" OR IBPC-STAT = "N") AND
              (IBPC-STAT-2 = "E" AND SP-EXPR-FRMLA NOT = "C")
              MOVE " " TO IBPC-STAT-2
              MOVE INDU-INTEREST-2 TO RATE-EXP-INT
              MOVE INDU-INTEREST TO INDU-INTEREST-2
              MOVE 0 TO INDU-INTEREST
              PERFORM LPRATE-COMPARE-RATES
              PERFORM INTEREST-DUE-CALCULATION
              IF (INDU-INTEREST-2 < INDU-INTEREST + RATE-EXP-INT)
                 MOVE INDU-INTEREST-2 TO INDU-INTEREST
              ELSE
                 ADD RATE-EXP-INT TO INDU-INTEREST.

      ************************************************
      *    COMPARE RATES ROUTINE
      ************************************************
       LPRATE-COMPARE-RATES SECTION.
           PERFORM LPRATE-SET-INTPDTH-DATE.
           MOVE INDU-DATE-1 TO NUM-DATE.
           MOVE XDTE-DATE TO SYS-DATE.
           IF SYS-DATE > NUM-DATE
              MOVE XDTE-DATE TO INDU-DATE-1.
           COMPUTE INDU-CURBAL = LN-CURBAL - LN-OT2BAL.
           MOVE RATE-LPTRCD TO INDU-LPTRCD.
           IF IBPC-STAT = "J"
              PERFORM LPRATE-JUDGEMENT
           ELSE
           IF IBPC-STAT = "R"
              MOVE LN-REDURATE TO INDU-RATE.

      ************************************************
      *    EXPIRED INTEREST ROUTINE
      *
      * THIS ROUTINE WALKS THRU THE SP-EXPR-RATETBL(5)
      *  DETERMINING WHICH RATE TO CHARGE AND FOR
      *  HOW LONG, COMPUTING INTEREST DUE FOR EACH
      *  BREAK.
      ************************************************
       LPRATE-EXPIRED-INT SECTION.
           MOVE 0 TO RATE-EXPR-TAB-BREAK(1) RATE-EXPR-TAB-BREAK(2)
                     RATE-EXPR-TAB-BREAK(3) RATE-EXPR-TAB-BREAK(4)
                     RATE-EXPR-TAB-BREAK(5).
           MOVE XDTE-DATE TO NDTE-DATE.
           SET EXPR TO 1.
           PERFORM LPRATE-CALC-BREAK-DATES
              UNTIL EXPR > 5 OR SP-EXPR-BREAK(EXPR) = 0.
           MOVE RATE-LPTRCD TO INDU-LPTRCD.
           SET EXPR TO 0.
       LPRATE-EXPIRED-INT-LOOP.
           SET EXPR UP BY 1.
           COMPUTE INDU-CURBAL = LN-CURBAL - LN-OT2BAL.
           MOVE SP-EXPR-YRTYPE(EXPR) TO INDU-YEARTYPE.
           MOVE SP-EXPR-INTMETH(EXPR) TO INDU-INTMETHOD.
           IF SP-EXPR-RATE(EXPR) = 999.999
              MOVE LN-OVRDRATE TO INDU-RATE
           ELSE
           IF ( SP-EXPR-FRMLA = "C"              ) AND
              ( SP-EXPR-RATE(EXPR) NOT > 999.995 ) AND
              ( LN-SMPRATE < SP-EXPR-RATE(EXPR)  )
                 MOVE LN-SMPRATE   TO INDU-RATE
                 MOVE SP-INTMETHOD TO INDU-INTMETHOD
                 MOVE SP-YEARTYPE  TO INDU-YEARTYPE
            ELSE
      *JTG HELP FOR JUDY, ADVANCE MORTGAGE 012297:
              MOVE SP-EXPR-RATE(EXPR) TO INDU-RATE
              IF INDU-RATE NOT > 999.995
                 IF SP-EXPR-FRMLA-ORIG
      *                  --ACCOUNT IS/HAS EXPIRED AND IS NOT
      *                    ACCELERATED/JUDGEMENT/REDUCED
                         AND IBPC-STAT = "E"
                          AND IBPC-STAT-2 = " "
                    IF LN-OVRDRATE < INDU-RATE
                       MOVE LN-OVRDRATE TO INDU-RATE
                       MOVE SP-INTMETHOD TO INDU-INTMETHOD
                       MOVE SP-YEARTYPE TO INDU-YEARTYPE.
      *JTG HELP FOR JUDY, ADVANCE MORTGAGE 012297:

           PERFORM LPRATE-FIND-THRU-DATE.
           PERFORM INTEREST-DUE-CALCULATION.
           ADD INDU-INTEREST TO RATE-EXP-INT.
           MOVE INDU-DATE-2 TO INDU-DATE-1.
           IF EXPR < 5
                AND (SP-EXPR-BREAK(EXPR) NOT = 0
                         AND RATE-EXPR-TAB-BREAK(EXPR) NOT = 0
                    )
              GO TO LPRATE-EXPIRED-INT-LOOP.
           MOVE RATE-EXP-INT TO INDU-INTEREST.

      ************************************************
      *    CALC BREAK DATES ROUTINE
      *
      * THIS ROUTINE CONVERTS ALL SP-EXPR-BREAK'S
      *  INTO DATES, BASED ON MONTHS AND DAYS FROM
      *  EXPIRATION.
      ************************************************
       LPRATE-CALC-BREAK-DATES SECTION.
      * SET SPR EXPR BREAK MONTHS & DAYS(.99 MONTHS):
           MOVE SP-EXPR-BREAK(EXPR) TO RATE-EXPR-BREAK.
           IF RATE-EXPR-BREAK > 360
              MOVE EXT-JULIAN-END TO NDTE-DATE
           ELSE
              MOVE XDTE-DATE TO NDTE-DATE
              MOVE RATE-EXPR-MONTHS TO NDTE-HOLD
              PERFORM INCREMENT-MONTHS
              COMPUTE NDTE-HOLD ROUNDED = RATE-EXPR-DAYS * 0.30
              PERFORM INCREMENT-DAYS.
           MOVE NDTE-DATE TO RATE-EXPR-TAB-BREAK(EXPR).
           SET EXPR UP BY 1.

      ************************************************
      *    FIND THRU DATES ROUTINE
      ************************************************
       LPRATE-FIND-THRU-DATE SECTION.
           MOVE RATE-EXPR-TAB-BREAK(EXPR) TO NUM-DATE.
           MOVE RATE-DATE TO SYS-DATE.
           PERFORM TIM.
           IF ELAPSED-DAYS < 0
                   OR RATE-EXPR-TAB-BREAK(EXPR) = 0
              MOVE RATE-DATE TO INDU-DATE-2
              MOVE 0 TO RATE-EXPR-TAB-BREAK(EXPR)
           ELSE
              MOVE INDU-DATE-1 TO NUM-DATE
              MOVE RATE-EXPR-TAB-BREAK(EXPR) TO SYS-DATE
              PERFORM TIM
              IF ELAPSED-DAYS NOT > 0
                 MOVE INDU-DATE-1 TO INDU-DATE-2
              ELSE
                 MOVE RATE-EXPR-TAB-BREAK(EXPR) TO INDU-DATE-2.

      ************************************************
      *    JUDGEMENT ROUTINE
      ************************************************
       LPRATE-JUDGEMENT SECTION.
      * 999.999 - ORIGINAL LOAN CONTRACTED RATE/METHOD:
           IF LN-JDRATE = 999.999
              MOVE LN-OVRDRATE TO INDU-RATE
              MOVE SP-INTMETHOD TO INDU-INTMETHOD
              MOVE SP-YEARTYPE TO INDU-YEARTYPE
           ELSE
           IF LN-JDRATE < 999.999
              MOVE LN-JDRATE TO INDU-RATE
      * 999.998 - EQUIVALENT SIMPLE INTEREST RATE:
      * 999.996 - A.P.R. RATE:
              IF LN-JDRATE = 999.998 OR 999.996
                 MOVE SP-INTMETHOD TO INDU-INTMETHOD
                 MOVE SP-YEARTYPE TO INDU-YEARTYPE
              ELSE
      * 999.997 - ACCELERATION RATES:
              IF LN-JDRATE = 999.997
                 MOVE SP-ACC-INTMETH TO INDU-INTMETHOD
                 MOVE SP-ACC-YRTYPE TO INDU-YEARTYPE
              ELSE
      * > 999.996 - JUST SOME JUDGEMENT RATE:
              IF LN-JDRATE < 999.996
                 MOVE SP-JUDGINTMETH TO INDU-INTMETHOD
                 MOVE SP-JUDGYRTYPE TO INDU-YEARTYPE.

      ************************************************
      *    ACCELERATION ROUTINE
      ************************************************
       LPRATE-ACCELERATION SECTION.
           IF LN-ACCRATE = 999.998
              MOVE 999.998      TO INDU-RATE
              MOVE SP-INTMETHOD TO INDU-INTMETHOD
              MOVE SP-YEARTYPE  TO INDU-YEARTYPE
           ELSE
           IF LN-ACCRATE = 999.996
              MOVE 999.996      TO INDU-RATE
              MOVE SP-INTMETHOD TO INDU-INTMETHOD
              MOVE SP-YEARTYPE  TO INDU-YEARTYPE
           ELSE
      * ALLOW WORLD TO ENTER 999.998 IN SP-ACC-RATE(1)
      * AND IF DETECTED HERE SKIP USAGE OF ACCELERATION RATE
      * TABLE AND FORCE LN-SMPRATE RE: LN-ACCRATE 999.998 WORLD #306
              IF ( LN-ACCRATE     = 999.997 ) AND
                 ( SP-ACC-RATE(1) = 999.998 )
                 MOVE 999.998      TO INDU-RATE
                 MOVE SP-INTMETHOD TO INDU-INTMETHOD
                 MOVE SP-YEARTYPE  TO INDU-YEARTYPE
              ELSE
      * DEFAULT OF EITHER 999.997 OR (0 TO 999.995)
                 MOVE SP-ACC-INTMETH TO INDU-INTMETHOD
                 MOVE SP-ACC-YRTYPE  TO INDU-YEARTYPE
                 MOVE LN-ACCRATE     TO INDU-RATE.

      ************************************************
      *    SET INTEREST PDTH (ASSESS FROM DATE)
      ************************************************
       LPRATE-SET-INTPDTH-DATE SECTION.
           MOVE LN-INTDATE TO INDU-DATE-1.
           IF LN-INTPDTH-DATE NOT = 0
              MOVE LN-INTPDTH-DATE TO INDU-DATE-1
           ELSE
              IF (LN-DATE-PAID-LAST NOT = 0)
                       AND (LN-DATE-PAID-LAST NOT = LN-LOANDATE)
                 MOVE LN-DATE-PAID-LAST TO INDU-DATE-1.

      *************************************************************************
      *   NAME:  LPINDU - INTEREST DUE CALCULATION
      *   DESC:  COMPUTE INTEREST DUE AT GIVEN PAY DATE
      *   IN  :  INDU-DATE-1  (LN-INTDATE OR LAST INTEREST ACCESS)
      *          INDU-DATE-2  (LP-PAYDATE)
      *          INDU-CURBAL (LN-CURBAL - LN-OT2BAL)
      *          INDU-RATE   (LN-EXPRRATE, LN-ENDORATE, ETC)
      *          INDU-LPTRCD (PAYMENT TRANSACTION CODE)
      *          INDU-YEARTYPE (YEAR TYPE BASED ON RATE BEING USED)
      *          INDU-INTMETHOD (INTEREST METHOD BASED ON RATE USED)
      *   OUT :  INDU-INTEREST
      *          INDU-DAYS
      *          INDU-PENALTY
      *   USED:
      * REV :
      *  092287 JTG REMOVED COMMA'S  ALSO CHANGED
      *             LOGIC FOR SELECTING RATE TABLE
      *             TO MAKE IT BETTER READ.
      *  092287 JTG MODIFIED TO ALLOW A SPACE IN
      *             BREAKTYPE IF SINGLE RATE TABLES.
      *  033188 JTG ADDED LOGIC FOR SPECIAL REFUND
      *             CONDITIONS
      *  041888 SLC CHANGED 99.9999 TO 999.999 TO MAKE
      *             COMPATABLE WITH NEW RATE DEFINITION.
      *             ADDED THE FOLLOWING OPTIONS:
      *             IF INDU-RATE = 999.998 USE LN-SMPRATE
      *             IF INDU-RATE = 999.997 USE ACCELERATION
      *             TABLE FROM SPR.
      *  050288 SLC YEARTYPE AND INT METH NOW PASSED TO INDU.
      *  032789 JTG CHANGED HOLLLYWODD FINANCE PO MAX
      *             60 INT. CALC ON RENEWAL TO TEST
      *             SPECIAL OPT 1 AND NOT RECAST CODE.
      *  080189 JTG ADDED LOGIC FOR SP-RBSPOPT1 = 5 SPRING (NJ)
      *  081089 JTG ADDED LOGIC FOR SP-RBSPOPT2 = 2 SPRING (NJ)
      *  100589 SLC ADDED SPECIAL OPTION TO CALC INTEREST BASE ON
      *             MAX NUMBER OF DAYS TO ACCRUE FOR IB ACCRUE REPORT
      *  012690 JTG ADDED OUTPUT OF INDU-DAYS
      *  053190 JTG ADDED SP-RBSPOPT1 = 8, PACESETTER
      *  083190 BAH ADDED REDUCTION OF SERVICE CHARGE FOR
      *             CHARGEABLE CODES 'I' AND 'P'
      *  031491 JTG ADDED REDUCTION OF INSURANCE PREMIUMS FOR
      *             CHARGEABLE CODES 'S' AND 'P'
      *  JTG 062995 ADDED SP-OPT-CAL-CHGABLE, MERCURY MI SERCHG
      *  JTG 092095 STOPPED REDUCTION OF CURRENT BALANCE ON 'PC' TYPE
      *             ACCOUNTS, WHEN THE SPR INTEREST CHARGABLE CODE
      *             EXCLUDED 'SERCHG AND /OR INSURANCE PREMIUMS'
      *  JTG 112795 ADDED SP-RBREDUC = 9999.02, 'NV' MERCURY ACTION SPRDER = 09
      *  JTG 120297 CHANGED TO ESTABLISH NO LIMIT ON SP-MAX-IB-ACR-DAYS
      *             CBC CAPITAL SINGLE PAY LOAN 12 MONTHS
      *  JTG 011698 PUT THE CODE IN FOR LOGIC THAT SHOULD HAVE BEEN ADDED
      *             BACK ON 092095, FOR STOPPING REDUCTION OF CURRENT BALANCE
      *             ON 'PC' TYPE ACCOUNTS, WHEN THE SPR INTEREST CHARGABLE CODE
      *             EXCLUDED 'SERCHG AND /OR INSURANCE PREMIUMS'
      *  JTG 990216 ADDED INTEREST-DUE-PENALTY-OH, SPECIAL OPTION #1 (16).
      *  JTG 991202 ADDED SP-RBREDUC = 9999.03, 'GA' NEW SOUTH PREPAY PENALTY
      *  JTG 000104 ADDED SP-RBREDUC = 9999.04 WISCONSIN IB REAL ESTATE
      *             PREPAY PENALTY, WISCONSIN #1031
      *             PLUS XFER OF PENALTY TO MSPENALTY IN LONPF SCREEN
      *  JTG 000124 ADDED SP-CAL-MAX-AT-ESTIMATE INSTANT AUTO #1047
      *  000225 MJD ADDED SP-RBREDUC = 9999.05 NEW SOUTH FLAT $75.00
      *             PREPAY PENALTY, (NEWS PR#1101)
      *  000804 BAH NEW SOUTH, RBREDUC = 9999.03, WAS NOT STORING THE
      *             PENALTY IN INDU-PENALTY, IT WAS ADDING IT DIRECTLY
      *             INTO INDU-INTEREST,,,FAX FROM KEVIN
      *  JTG 000822 ADDED LOGIC FOR KENTUCKY 60 DAY NO CHARGE LAW, ADDED
      *             SP-RBSPOPT1() = 19, WORLD #233
      *  JTG 000914 CORRECTED BUG IN PASS TO SETCTBL, ROUTINE WAS PASSING
      *             LN-LNAMT INSTEAD OF INTEREST CHARGEABLE  WORLD #J011
      *  BAH 010108 ADDED CLEAR OF INDU-PENALTY, NEEDED TO BE SURE IT WAS
      *             0 CAUSE IT WILL SUBTRACT THIS FROM NO-DEFERMENT IN
      *             SPINQ0 CAUSE IT CALLS CRNORC WITH "PO" WITH CREATES
      *             AN INTEREST PENALTY INCLUDED IN THE NO-DEFERMENT
      *             LENDMARK (KEVIN)
      * JTG 020722 CHANGED FOR (MIP) MONTHLY INSURANCE PREMIUMS   REGENCY #1801
      *  BAH 020828 ALLOWED RBREDUC(7) = 9999.01 TO WORK FOR IB'S, ADD PENALTY
      *             TO INDU-INTEREST, PIKCO PR# 1900
      *  BAH 021216 CHANGE ABOVE CALCULATED A PENALTY ON EVERY PAYMENT !!
      *             NEEDS TO CHECK FOR "PO" AND "PA" ONLY
      *  BAH 040623 ADDED RBSPOPT1 = 21, THRIFT INVESTMENT PR# 2334
      *  BAH 160921 ADDED LT-REC ON THE CALL OF REBA01, LINKAGE ERROR AT
      *             CUSTOMER REBA01 WAS CHANGED 6 YEARS AGO! FIRST METRO #949
      *  BAH 180702 CHANGED A01 PASSING FIELDS TO BE IN LPRATEW SO REBATE 
      *             COPY MEMBERS DO NOT NEED TO BE IN HERE!
      *   CS 211105 CHANGED INTEREST-DUE CALCULATION IF ELAPSED-YRTYPE IS 0
      **************************************************************************
       INTEREST-DUE-CALCULATION SECTION.
           MOVE 0 TO INDU-INTEREST INDU-WORKER 
                     INDU-AMOUNT   INDU-DAYS   INDU-PENALTY
                     INDU-ALL-INSPREM.
           MOVE INDU-DATE-1 TO NUM-DATE.
           MOVE INDU-DATE-2 TO SYS-DATE.
           MOVE INDU-YEARTYPE TO ELAPSED-YRTYPE.
           IF ELAPSED-YRTYPE = 0
              MOVE 0 TO ELAPSED-DAYS
           ELSE
              PERFORM TIMALL.
           PERFORM INTEREST-DUE-SPECIAL-OPTIONS.
           IF ELAPSED-DAYS NOT > 0
              GO TO INTEREST-DUE-EXIT.

           PERFORM SELECT-SPR-TABLE.
           IF LN-LOANTYPE = "I"
              IF SP-OPT-CAL-CHGABLE NOT = " "
                 IF SP-OPT-CAL-PROCEEDS OR SP-OPT-CAL-PRO-INS
                    SUBTRACT LN-SERCHG FROM INDU-CURBAL
                 END-IF
              ELSE
              IF SP-CAL-PROCEEDS(CTBL) OR SP-CAL-PRO-INS(CTBL)
                 SUBTRACT LN-SERCHG FROM INDU-CURBAL.

           IF LN-LOANTYPE = "I"
              IF SP-OPT-CAL-CHGABLE NOT = " "
                 IF SP-OPT-CAL-PROCEEDS OR SP-OPT-CAL-PRO-SC
                    PERFORM INDU-GET-ALL-INSPREM
                    SUBTRACT INDU-ALL-INSPREM FROM INDU-CURBAL
                 END-IF
              ELSE
              IF SP-CAL-PROCEEDS(CTBL) OR SP-CAL-PRO-SC(CTBL)
                 PERFORM INDU-GET-ALL-INSPREM
                 SUBTRACT INDU-ALL-INSPREM FROM INDU-CURBAL.

      * IF CHGABLE CODE 3 THEN INCLUDE PROCEEDS, SVC CHG, AND ALL INSURANCES
      *    EXCEPT FOR OTHER 3

           IF LN-LOANTYPE = "I"
              IF SP-OPT-CAL-CHGABLE NOT = " "
                 IF SP-OPT-CAL-PRO-INS-SC-NO-O3
                     IF LN-INSOURS(6) NOT = "M"
                        SUBTRACT LN-INSPREM(6) FROM INDU-CURBAL.


      ******************************************************
      *  NEEDED NOW THAT WE ARE GOING TO ALLOW MAINT FEE'S
      *    ON IB LOANS.
      *       VALID CODES FOR LN-MF-REBATE:
      *             SP - NO REBATE OR CANCEL
      *             C - CANCEL
      *             Y - REBATE NO CANCEL
      *             R - REBATE AFTER CANCEL
      ******************************************************
           IF LN-LOANTYPE = "I"
              IF NOT (LN-MF-REBATE = "Y" OR "R")
                 COMPUTE INDU-CURBAL = INDU-CURBAL -
                                          (LN-MAINTFEE * LN-ORGTERM).

           IF INDU-CURBAL < 0
              MOVE 0 TO INDU-CURBAL.

           IF INDU-RATE = 999.999
              GO TO VALIDATE-SPR-TABLE
           ELSE
           IF INDU-RATE = 999.998
              MOVE LN-SMPRATE TO INDU-RATE
           ELSE
           IF INDU-RATE = 999.996
              MOVE LN-APRATE TO INDU-RATE
           ELSE
           IF INDU-RATE = 999.997
              SET ASTP TO 0
              GO TO ACCEL-INT-LOOP.

           COMPUTE INDU-WORKER ROUNDED =
                   (INDU-RATE / 100) * INDU-CURBAL.

           GO TO INTEREST-DUE-FINAL.

      ********************************
      *    VALIDATE SELECTED TABLE
      ********************************
       VALIDATE-SPR-TABLE.
           IF NOT (SP-CAL-SIMPLE(CTBL))
              GO TO INTEREST-DUE-EXIT.
           IF NOT (SP-CAL-TBFRMLA-VALID(CTBL))
              GO TO INTEREST-DUE-EXIT.
           IF SP-CAL-BREAK-ON-TERM(CTBL)
              GO TO INTEREST-DUE-EXIT.

      ********************************
      *    INTEREST DUE LOOPS
      *    USING SP-CAL TABLES
      ********************************
           SET CSTP TO 0.
       INTEREST-DUE-LOOP.
           IF INDU-CURBAL = 0
              GO TO INTEREST-DUE-FINAL.
           SET CSTP UP BY 1.
           IF CSTP > SP-CAL-NORATES(CTBL)
              GO TO INTEREST-DUE-FINAL.
           IF SP-CAL-BREAK(CTBL CSTP) = 0
              MOVE INDU-CURBAL TO INDU-AMOUNT
           ELSE
              IF CSTP = 1
                 MOVE SP-CAL-BREAK(CTBL CSTP) TO INDU-AMOUNT
              ELSE
                 SUBTRACT SP-CAL-BREAK(CTBL CSTP - 1) FROM
                             SP-CAL-BREAK(CTBL CSTP)
                                GIVING INDU-AMOUNT.
           IF INDU-CURBAL > INDU-AMOUNT
              SUBTRACT INDU-AMOUNT FROM INDU-CURBAL
           ELSE
              MOVE INDU-CURBAL TO INDU-AMOUNT
              MOVE 0 TO INDU-CURBAL.
           COMPUTE INDU-WORKER ROUNDED = INDU-WORKER +
              (SP-CAL-RATE(CTBL CSTP) / 100) * INDU-AMOUNT.
           GO TO INTEREST-DUE-LOOP.

      ********************************
      *    INTEREST DUE LOOPS
      *    USING SP-ACC TABLE
      ********************************
       ACCEL-INT-LOOP.
           IF INDU-CURBAL = 0
              GO TO INTEREST-DUE-FINAL.
           SET ASTP UP BY 1.
           IF ASTP > 5
              GO TO INTEREST-DUE-FINAL.
           IF SP-ACC-RATE(ASTP) = 0 AND SP-ACC-BREAK(ASTP) = 0
              GO TO INTEREST-DUE-FINAL.
           IF SP-ACC-BREAK(ASTP) = 0
              MOVE INDU-CURBAL TO INDU-AMOUNT
           ELSE
              IF ASTP = 1
                 MOVE SP-ACC-BREAK(ASTP) TO INDU-AMOUNT
              ELSE
                 SUBTRACT SP-ACC-BREAK(ASTP - 1) FROM
                           SP-ACC-BREAK(ASTP) GIVING INDU-AMOUNT.
           IF INDU-CURBAL > INDU-AMOUNT
              SUBTRACT INDU-AMOUNT FROM INDU-CURBAL
           ELSE
              MOVE INDU-CURBAL TO INDU-AMOUNT
              MOVE 0 TO INDU-CURBAL.
           COMPUTE INDU-WORKER ROUNDED =
             INDU-WORKER + (SP-ACC-RATE(ASTP) / 100) * INDU-AMOUNT.
           GO TO ACCEL-INT-LOOP.

      ******************************************
      * AT THIS POINT INDU-WORKER HOLDS THE
      * CALCULATED ANNUAL CHARGE, WHICH MUST BE
      * CONVERTED TO A DAILY CHARGE.
      * THE DAILY CHARGE IS COMPUTED BASED ON
      * THE VALUE OF INDU-INTMETHOD:
      *           "O" - ORDINARY INTEREST (360)
      *           "E" - EXACT    INTEREST (365)
      ******************************************
       INTEREST-DUE-FINAL.
           MOVE 360          TO INDU-WK.
           IF INDU-INTMETHOD = "E"
              MOVE 365       TO INDU-WK.
           MOVE ELAPSED-DAYS TO INDU-DAYS.
           COMPUTE INDU-INTEREST ROUNDED =
              (INDU-WORKER / INDU-WK) * ELAPSED-DAYS.
       INTEREST-DUE-EXIT.
           PERFORM INTEREST-DUE-PENALTY.
           PERFORM INTEREST-DUE-MINIMUM.
           PERFORM INTEREST-DUE-MAXIMUM.
           MOVE SPACES TO INDU-LPTRCD.

      **************************************************
       INTEREST-DUE-PENALTY SECTION.
      **************************************************

      * SPRING FINANCE PRE-PAYOFF PENALTY:
      * IF PAYOFF IS PRIOR TO MATURITY TAKE PENALTY:
      
           IF SP-RBSPOPT1(7) NOT = 5
              GO TO INTEREST-DUE-PENALTY-21.

           IF INDU-LPTRCD NOT = "PO"
              GO TO INTEREST-DUE-PENALTY-EXIT.

           PERFORM MATURITY-DATE-CALCULATION.
           MOVE INDU-DATE-2 TO NUM-DATE.
           MOVE MDTE-DATE TO SYS-DATE.
           PERFORM TIM.
           IF ELAPSED-DAYS NOT > 0
              GO TO INTEREST-DUE-PENALTY-EXIT.

           PERFORM LOAN-CALCULATIONS.
           IF FINANCED-AMOUNT < 2000.01
              ADD 20.00 TO INDU-INTEREST
           ELSE
              IF FINANCED-AMOUNT < 5000.01
                 COMPUTE INDU-INTEREST ROUNDED =
                     INDU-INTEREST + FINANCED-AMOUNT * .01
              ELSE
                 ADD 100.00 TO INDU-INTEREST.
           GO TO INTEREST-DUE-PENALTY-EXIT.

       INTEREST-DUE-PENALTY-21.

      * THRIFT INVESTMENT CORP.

           IF SP-RBSPOPT1(7) NOT = 21
              GO TO INTEREST-DUE-PENALTY-MS.

           IF INDU-LPTRCD NOT = "PO"
              GO TO INTEREST-DUE-PENALTY-EXIT.

      * IF PREPAID WITHIN 12 MONTHS AFTER THE FIRST PAYMENT IS
      * IS DUE, YOU CAN CHARGE A PRE-PAYMENT PENALTY.

           MOVE LN-ORIG-1STPYDATE TO NUM-DATE.
           MOVE INDU-DATE-2 TO SYS-DATE
           PERFORM TIM365
           IF ELAPSED-MONTHS NOT < 12
              GO TO INTEREST-DUE-PENALTY-EXIT.

           IF INDU-ASSESS-PENALTY = "N"
              MOVE SPACES TO INDU-ASSESS-PENALTY
              GO TO NO-PENALTY-21.

           PERFORM LOAN-CALCULATIONS.
           IF FINANCED-AMOUNT < 2000.01
              ADD 20.00 TO INDU-INTEREST
           ELSE
              IF FINANCED-AMOUNT < 5000.01
                 COMPUTE INDU-INTEREST ROUNDED =
                     INDU-INTEREST + FINANCED-AMOUNT * .01
              ELSE
                 ADD 100.00 TO INDU-INTEREST.

           MOVE "N" TO INDU-ASSESS-PENALTY.

       NO-PENALTY-21.
           GO TO INTEREST-DUE-PENALTY-EXIT.

       INTEREST-DUE-PENALTY-MS.
           IF SP-RBREDUC(7) NOT = 9999.01
              GO TO INTEREST-DUE-PENALTY-NV.

      * 07/02/2018 MOVED A01-MS-PENALTY OUT OF REBATEW AND INTO LPRATEW AS
      *            INDU-A01-PENALTY, REMOVED CLEAR OF REB-MS-PENALTY
      *    MOVE 0 TO INDU-PENALTY A01-MS-PENALTY REB-MS-PENALTY.

           MOVE 0 TO INDU-PENALTY INDU-A01-PENALTY.

           IF NOT (INDU-LPTRCD = "PO" OR "PA")
              GO TO INTEREST-DUE-PENALTY-NV.

           IF INDU-ASSESS-PENALTY = "N"
              MOVE SPACES TO INDU-ASSESS-PENALTY
              GO TO NO-PENALTY-MS.

      * TEST IF WITHIN 5 YEARS
           MOVE LN-LOANDATE TO NUM-DATE
           MOVE INDU-DATE-2 TO SYS-DATE
           PERFORM TIM360
           IF ELAPSED-MONTHS < 0 OR > 60
              GO TO INTEREST-DUE-PENALTY-NV.

           IF ELAPSED-REM > 0
              ADD 1 TO ELAPSED-MONTHS.

           MOVE ELAPSED-MONTHS TO INDU-A01-ELAPSED-MO.
           MOVE INDU-DATE-2    TO INDU-A01-PAYDATE.
           MOVE 0              TO INDU-COMMON-REBATE.
           MOVE "CL/REBA01"    TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH INDU-COMMON LN-REC LTI-REC.
           CANCEL FORM-PROGX.

           MOVE INDU-A01-PENALTY TO INDU-PENALTY.

           COMPUTE INDU-INTEREST ROUNDED = INDU-INTEREST
                                         + INDU-PENALTY.

           MOVE "N" TO INDU-ASSESS-PENALTY.

       NO-PENALTY-MS.
           GO TO INTEREST-DUE-PENALTY-EXIT.

      * NEVADA PRE-PAYOFF PENALTY, MERCURY ACTION SPRDER = 09:
      * IF PAYOFF IS PRIOR TO MATURITY TAKE $25.00 PENALTY:
      
       INTEREST-DUE-PENALTY-NV.
           IF SP-RBREDUC(7) NOT = 9999.02
              GO TO INTEREST-DUE-PENALTY-OH.

           IF INDU-LPTRCD = "PO"
              PERFORM MATURITY-DATE-CALCULATION
              MOVE MDTE-DATE TO NUM-DATE
              MOVE INDU-DATE-2 TO SYS-DATE
              IF SYS-DATE < NUM-DATE
                 ADD 25.00 TO INDU-INTEREST.
           GO TO INTEREST-DUE-PENALTY-EXIT.

      * OHIO PRE-PAYOFF PENALTY, REGENCY:
      *    IF PREPAID WITHIN 5 YEARS AND NOT RENEWED
      *       CHARGE PENALTY OF 1% OF AMOUNT FINANCED
      
       INTEREST-DUE-PENALTY-OH.
           IF SP-RBSPOPT1(7) NOT = 16
              GO TO INTEREST-DUE-PENALTY-GA.

           IF INDU-LPTRCD = "PO"
              IF INDU-ASSESS-PENALTY = "N"
                MOVE SPACES TO INDU-ASSESS-PENALTY
                GO TO NO-PENALTY-OH.

           IF INDU-LPTRCD = "PO"
                MOVE LN-LOANDATE TO NUM-DATE
                MOVE INDU-DATE-2 TO SYS-DATE
                PERFORM TIM360
                IF ELAPSED-DAYS NOT > 1800
                  PERFORM LOAN-CALCULATIONS
                  COMPUTE INDU-INTEREST ROUNDED =
                      INDU-INTEREST + ( FINANCED-AMOUNT * 0.01 )
                MOVE "N"   TO INDU-ASSESS-PENALTY.
       NO-PENALTY-OH.
           GO TO INTEREST-DUE-PENALTY-EXIT.

      * GEORGIA PRE-PAYOFF PENALTY, NEW SOUTH (REAL ESTATE)
      * IF PAYOFF IS PRIOR TO 4TH YEAR TAKE PENALTY:
      
       INTEREST-DUE-PENALTY-GA.
           IF SP-RBREDUC(7) NOT = 9999.03
              GO TO INTEREST-DUE-PENALTY-WI.

           IF INDU-LPTRCD = "PO"
              IF INDU-ASSESS-PENALTY = "N"
                 MOVE SPACES TO INDU-ASSESS-PENALTY
                 MOVE HOLD-INDU-PENALTY TO INDU-PENALTY
                 MOVE 0 TO HOLD-INDU-PENALTY
                 GO TO NO-PENALTY-GA.

           MOVE 0 TO INDU-PENALTY HOLD-INDU-PENALTY.
           IF INDU-LPTRCD = "PO"
              MOVE LN-LOANDATE TO NUM-DATE
              MOVE INDU-DATE-2 TO SYS-DATE
              PERFORM TIM360
              IF ELAPSED-MONTHS < 12
                 COMPUTE INDU-PENALTY ROUNDED =
                                      (INDU-CURBAL * 0.05)
              ELSE
              IF ELAPSED-MONTHS < 24
                 COMPUTE INDU-PENALTY ROUNDED =
                                      (INDU-CURBAL * 0.04)
              ELSE
              IF ELAPSED-MONTHS < 36
                 COMPUTE INDU-PENALTY ROUNDED =
                                      (INDU-CURBAL * 0.02).

           COMPUTE INDU-INTEREST ROUNDED = INDU-INTEREST
                                         + INDU-PENALTY.

           MOVE "N" TO INDU-ASSESS-PENALTY.
           MOVE INDU-PENALTY TO HOLD-INDU-PENALTY.
       NO-PENALTY-GA.
           GO TO INTEREST-DUE-PENALTY-EXIT.

      * WISCONSIN PRE-PAYOFF PENALTY, WISCONSIN FINANCE (IB REAL ESTATE)
      * IF PAYOFF IS PRIOR TO 4TH YEAR TAKE PENALTY:
      
       INTEREST-DUE-PENALTY-WI.
           IF SP-RBREDUC(7) NOT = 9999.04
              GO TO INTEREST-DUE-PENALTY-NEWS.

           IF INDU-LPTRCD = "PO"
      * CALL SUBPROGRAM TO DETERMINE PENALTY:
              MOVE "X"         TO INDU-COMMON-RTNCD
              MOVE 0           TO INDU-A02-PENALTY
              MOVE INDU-DATE-2 TO INDU-A02-PAYDATE
              MOVE "CL/REBA02" TO FORM-NAM
              CALL FORM-PROGX USING FORM-PATH INDU-COMMON LN-REC SP-REC
              CANCEL FORM-PROGX
              IF INDU-COMMON-RTNCD = " "
                 COMPUTE INDU-INTEREST ROUNDED =
                      INDU-INTEREST + INDU-A02-PENALTY
                 MOVE INDU-A02-PENALTY TO INDU-PENALTY.

           GO TO INTEREST-DUE-PENALTY-EXIT.

      * NEW SOUTH PRE-PAYOFF PENALTY, WISCONSIN FINANCE  ($75.00 FLAT)
      * IF PAYOFF OS PAID OFF ANY TIME PRIOR TO THE LAST THREE MONTHS OF
      *    THE LOAN
      *
      * NOTE: IN CALCULATING THE 3 MONTHS, THE LOANS ORIGINAL MATURITY DATE
      *       IS USED WITHOUT REGARD FOR DEFERMENTS OR ADDONS.
      
       INTEREST-DUE-PENALTY-NEWS.
           IF SP-RBREDUC(7) NOT = 9999.05
              GO TO INTEREST-DUE-PENALTY-NC.

           IF INDU-LPTRCD = "PO"
      * CALCULATE ORIGINAL MATURITY DATE
              MOVE "Y" TO MDTE-FLAG
                          MDTE-ORGTERM-FG
              PERFORM MATURITY-DATE-CALCULATION
      * CALCULATE DATE 3 MONTHS PRIOR TO MATURITY DATE
              MOVE MDTE-DATE TO NDTE-DATE
              MOVE -3        TO NDTE-HOLD
              PERFORM INCREMENT-MONTHS
      * TEST PAYOFF DATE < MATURITY - 3 MONTHS
      *   IF PAYOFF OCCURS MORE THAN 3 MONTHS FROM MATURITY
      *   THEN ADD A FLAT RATE OF $75.00 AS A PREPAYMENT PENALTY
              MOVE NDTE-DATE TO NUM-DATE
              MOVE RATE-DATE TO SYS-DATE
              IF SYS-DATE NOT > NUM-DATE
                 MOVE 75.00 TO INDU-PENALTY
                 COMPUTE INDU-INTEREST ROUNDED = INDU-INTEREST
                                               + INDU-PENALTY.

           GO TO INTEREST-DUE-PENALTY-EXIT.

      * NORTH CAROLINA, PRE-PAYOFF PENALTY, CAROLINA FINANCE  (10% OR $25.00)
      * IF PAID OFF ANY TIME PRIOR TO MATURITY OF THE LOAN
      *
      * NOTE: THE LOANS ORIGINAL MATURITY DATE
      *       IS USED WITHOUT REGARD FOR DEFERMENTS OR ADDONS.
      
       INTEREST-DUE-PENALTY-NC.
           IF SP-RBREDUC(7) NOT = 9999.06
              GO TO INTEREST-DUE-PENALTY-25.

      * CALCULATE ORIGINAL MATURITY DATE
      
           IF INDU-LPTRCD = "PO"
              MOVE "Y" TO MDTE-FLAG
                          MDTE-ORGTERM-FG
              PERFORM MATURITY-DATE-CALCULATION
              MOVE MDTE-DATE TO NUM-DATE
              MOVE INDU-DATE-2 TO SYS-DATE
              IF SYS-DATE < NUM-DATE
                 COMPUTE INDU-PENALTY ROUNDED =
                    (INDU-CURBAL * .10)
                 IF INDU-PENALTY > 25.00
                    MOVE 25.00 TO INDU-PENALTY
                 END-IF
                 COMPUTE INDU-INTEREST ROUNDED = INDU-INTEREST
                                               + INDU-PENALTY.

           GO TO INTEREST-DUE-PENALTY-EXIT.


      * OHIO STATE PREPAYMENT PENALTY (EFF 01-JAN-2007)
      *
      * IF ORIG PRIN <= 75K
      *    PENALTY IS 1% OF LOAN NET BAL IF PAID < 5 YEARS FROM LOAN DATE
      * IF ORIG PRIN  > 75K
      *    IF PAID <=1 YEAR FROM LOAN DATE
      *       PENALTY IS 2% OF LOAN NET BAL
      *    IF PAID > 1  AND <= 2 YEARS FROM LOAN DATE
      *       PENALTY IS 1% OF LOAN NET BAL
      *    IF PAID > 2 YEARS FROM LOAN DATE
      *       NO PENALTY
      *
      * NOTES:
      *    PENALTY IS NOT CHARGED ON RENEWALS.
      *    PENALTY IS ADDED TO THE INTEREST DUE ON PAYOFF.
      
       INTEREST-DUE-PENALTY-25.
           IF SP-RBSPOPT1(7) NOT = 25
              GO TO INTEREST-DUE-PENALTY-EXIT.

           IF INDU-LPTRCD = "PO"
              IF INDU-ASSESS-PENALTY ="N"
                 MOVE SPACES TO INDU-ASSESS-PENALTY
                 GO TO NO-PENALTY-25.

           IF INDU-LPTRCD = "PO"
              PERFORM LOAN-CALCULATIONS
              IF FINANCED-AMOUNT <= 75000
                 MOVE LN-LOANDATE TO NUM-DATE
                 MOVE RATE-DATE TO SYS-DATE
                 PERFORM TIM360
                 IF ELAPSED-DAYS NOT > 1800
                    COMPUTE INDU-PENALTY = ( LN-CURBAL * 0.01 )
                    COMPUTE INDU-INTEREST ROUNDED = INDU-INTEREST
                                                  + INDU-PENALTY
                 END-IF
              ELSE
                 MOVE LN-LOANDATE TO NUM-DATE
                 MOVE RATE-DATE TO SYS-DATE
                 PERFORM TIM360
                 IF ELAPSED-DAYS NOT > 360
                    COMPUTE INDU-PENALTY = ( LN-CURBAL * 0.02 )
                    COMPUTE INDU-INTEREST ROUNDED = INDU-INTEREST
                                                  + INDU-PENALTY
                 ELSE IF ELAPSED-DAYS > 360 AND NOT > 720
                    COMPUTE INDU-PENALTY = ( LN-CURBAL * 0.01 )
                    COMPUTE INDU-INTEREST ROUNDED = INDU-INTEREST
                                                  + INDU-PENALTY
                 ELSE
                    MOVE 0 TO INDU-PENALTY.

           MOVE "N" TO INDU-ASSESS-PENALTY.

       NO-PENALTY-25.
           GO TO INTEREST-DUE-PENALTY-EXIT.

       INTEREST-DUE-PENALTY-EXIT.
           EXIT.

      ****************************************************
       INTEREST-DUE-SPECIAL-OPTIONS SECTION.
      ****************************************************
           IF INDU-LPTRCD = "PB" OR "RN" OR "RB" OR "RO"
              IF SP-RBSPOPT1(7) = 1 OR 19
                 IF ELAPSED-DAYS > 60
                    MOVE 60 TO ELAPSED-DAYS
                 ELSE
                    NEXT SENTENCE
              ELSE
              IF SP-RBSPOPT1(7) = 9
                 IF ELAPSED-DAYS > 90
                    MOVE 90 TO ELAPSED-DAYS.

      * SPRING FINANCE:
           IF SP-RBSPOPT2(7) = 2
              IF INDU-LPTRCD = "PB" OR "RN" OR "SC" OR "PO"
                                  OR "RB" OR "RO"
                 ADD 1 TO ELAPSED-DAYS.

      * CALCULATE MAX ACCRUED INT FOR IB ACCRUAL REPORT
      * NOTE: 999 = NO MAXIMUM
           IF ( INDU-MAX-ACCRUE = 1 AND SP-MAX-IB-ACR-DAYS NOT = 0 )
              IF ( SP-MAX-IB-ACR-DAYS NOT = 999 )
                 IF ( ELAPSED-DAYS > SP-MAX-IB-ACR-DAYS )
                    MOVE SP-MAX-IB-ACR-DAYS TO ELAPSED-DAYS.

      **************************************************
       INTEREST-DUE-MINIMUM SECTION.
      **************************************************
      * PACESETTER FINANCE:
           IF SP-RBSPOPT1(7) = 8
              IF INDU-LPTRCD = "PB" OR "RN" OR "SC" OR "PO"
                                  OR "RB" OR "RO"
                 ADD INDU-INTEREST LN-TOTINT LN-INTBAL GIVING
                                                     INDU-AMOUNT
                 IF INDU-AMOUNT < SP-RBMINRET(7)
                    COMPUTE INDU-INTEREST = INDU-INTEREST +
                       (SP-RBMINRET(7) - INDU-AMOUNT).

      **************************************************
       INTEREST-DUE-MAXIMUM SECTION.
      **************************************************
      * INSTANT AUTO #1047:
           IF SP-CAL-MAX-AT-ESTIMATE = "Y"
              IF (INDU-INTEREST + LN-TOTINT) > LN-INTCHG
                 COMPUTE INDU-INTEREST = LN-INTCHG - LN-TOTINT.

      *********************************
      *    SET THE PROPER RATE TABLE
      *    CTBL = 1 OR 2
      *********************************
       SELECT-SPR-TABLE SECTION.
           PERFORM LOAN-CALCULATIONS.
           MOVE INT-CHARGEABLE TO CTBL-INT-CHARGEABLE.
           MOVE LN-ORGTERM     TO CTBL-TERM.
           PERFORM SETCTBL.
           MOVE CTBL-INDEX TO CTBL.

      ******************************************************
      *    GET ALL INSURANCE PREMIUMS
      ******************************************************
       INDU-GET-ALL-INSPREM SECTION.
           MOVE 0 TO INDU-ALL-INSPREM.
           IF LN-INSOURS(1) NOT = "M"
              ADD LN-INSPREM(1) TO INDU-ALL-INSPREM.
           IF LN-INSOURS(2) NOT = "M"
              ADD LN-INSPREM(2) TO INDU-ALL-INSPREM.
           IF LN-INSOURS(3) NOT = "M"
              ADD LN-INSPREM(3) TO INDU-ALL-INSPREM.
           IF LN-INSOURS(4) NOT = "M"
              ADD LN-INSPREM(4) TO INDU-ALL-INSPREM.
           IF LN-INSOURS(5) NOT = "M"
              ADD LN-INSPREM(5) TO INDU-ALL-INSPREM.
           IF LN-INSOURS(6) NOT = "M"
              ADD LN-INSPREM(6) TO INDU-ALL-INSPREM.
           IF LN-INSOURS(7) NOT = "M"
              ADD LN-INSPREM(7) TO INDU-ALL-INSPREM.
           IF LN-INSOURS(8) NOT = "M"
              ADD LN-INSPREM(8) TO INDU-ALL-INSPREM.
       INDU-GET-ALL-INSPREM-EXIT.
           EXIT.

