      *-----------------------------------------------------------------------
      *================================================================*
      * END COPYBOOK: LIBGB\REBATE.CPY                                *
      *================================================================*
      *================================================================*
      * EMBEDDED COPYBOOK: LIBLP\LPPOFF.CPY                           *
      *================================================================*
      * COPYMEMBER: LIBLP/LPPOFF
      ***************************************************************
      *         COMPUTE PAYOFF FIGURES FOR A LOAN
      *                (AS OF A CURRENT DATE)
      *
      *   NAME: LPPOFF
      *   DESC: THIS ROUTINE IS USED TO COMPUTE THE
      *         PAYOFF FIGURES FOR LOAN PAYOFF. IT WILL
      *         YIELD THE FOLLOWING FIGURES:
      *
      *         -----------------------------------------------
      *         REMEMBER TO CHANGE FAPOFF ALSO
      *
      *         NOTE:
      *              THIS ROUTINE IS FOR ANY PROGRAM
      *              E X E C P T  LONPFA.C
      *         -----------------------------------------------
      *         ROUTINE   A U T O  C O P I E S   'LIBLP/MIPPO'
      *         -----------------------------------------------
      *
      *   IN  : POFF-PAYDATE                 DATE OF PAYMENT
      *         POFF-MAKERCD                 LN-MAKERCD(X)
      *         POFF-LPTRCD                  TO REFLECT THE TYPE OF PAYOFF
      *         POFF-LONPFA-UPD-FG           (NOT REQUIRED FOR THIS COPY MEMBER
      *                                       SEE LIBLP/FAPOFF)
      *         POFF-DEF-STOP           SET TO 'Y' WHEN STOPS ARE
      *                                      NECESSARY ON DEFERMENTS
      *         POFF-RECAST-FG               TO ALLOW SP-RBRECAST() = 1 TO
      *                                      PERFORM A RECAST OF INTEREST REFUND
      *                                      DEFAULT VALUE " "
      *         POFF-LCAP-BATCH-REFCD        SET FROM ROUTINES THAT POST BATCH
      *                                      PAYMENTS
      *         POFF-LCAP-BATCH-REFCD-LC-FG  SET FROM ROUTINES THAT POST BATCH
      *                                      PAYMENTS
      *
      *   OUT : POFF-REBATE(1)          CL REBATE
      *                    (2)          AH REBATE
      *                    (3)          PP REBATE
      *                    (4)          INT REBATE
      *                    (5)          SRV REBATE
      *                    (6)          FEE REBATE
      *                    (7)          DEF REBATE
      *                    (8)          N/A REBATE
      *                    (9)          OT1 REBATE
      *                    (10)         OT2 REBATE
      *                    (11)         OT3 REBATE
      *                    ----
      *                    (12)         OT4 REBATE
      *                    (13)         OT5 REBATE
      *         POFF-INS-TAX-RB(1)      CL REBATE INS TAX
      *                        (2)      AH REBATE INS TAX
      *                        (3)      PP REBATE INS TAX
      *                        (4)      O1 REBATE INS TAX
      *                        (5)      O2 REBATE INS TAX
      *                        (6)      O3 REBATE INS TAX
      *                        ----
      *                        (7)      O4 REBATE INS TAX
      *                        (8)      O5 REBATE INS TAX
      *         POFF-INT-REB-TBL(1)     CL ADDON INTEREST REBATE.
      *                         (2)     AH ADDON INTEREST REBATE.
      *                         (3)     PP ADDON INTEREST REBATE.
      *                         (4)     O1 ADDON INTEREST REBATE.
      *                         (5)     O2 ADDON INTEREST REBATE.
      *                         (6)     O3 ADDON INTEREST REBATE.
      *                         (7)     O4 ADDON INTEREST REBATE.
      *                         (8)     O5 ADDON INTEREST REBATE.
      *         POFF-OTDUE              OTHER CHARGES DUE
      *         POFF-LCDUE              LATE CHARGES DUE
      *         POFF-INTDUE             INTEREST DUE
      *         POFF-NETDUE             NET PAYOFF AMOUNT DUE
      *         POFF-PARTIALS           (FROM LPLCAP)
      *         POFF-LCPAID             (FROM LPLCAP)
      *  NOT USED YET
      *         POFF-NOLCHG             (FROM LPLCAP)
      *         POFF-ADDON-INT-EARNED   FOR LONPFA.C
      *         POFF-ADDON-INT-REBATE   FOR LONPFA.C
      *         POFF-LXM-MIP-CLPAID
      *         POFF-LXM-MIP-AHPAID
      *         POFF-LXM-MIP-CLPDTH-DATE
      *         POFF-LXM-MIP-AHPDTH-DATE
      *   USED:
      *   COPY: LPPOF2, DATERW, DATER, LPRATEW, REBATE, REBATEW
      *         LPLCAP, LPLCAPW, LPINDU, LPINDUW, LPIBPC, LPIBPCW
      *         LPAPRSW, LPAPRS, ADDONSPR, LPMDTEW, LPMDTE,
      *         LPITRMW, LPITRM, LPAMTSW, LPAMTS, LPAPRZW, LPAPRZ,
      *         LPUPERW, LPUPER, LPLCASW, LPLCAS, LPLCHGW, LPLCHG,
      *         SETCTBLW, SETCTBL, LPUPWKW, LPUPWK, RBCATW, RBCAT,
      *         SPPDTHW, SPPDTH, LPXDTE2, LPXDTE, LPSCHDW, LPSCHD,
      *
      * REV :
      *  040188 JTG ADDED MOVE OF LPTRCD TO INDU-LPTRCD
      *  052088 JTG ADDED POFF-PARTIALS & POFF-LCPAID
      *             FOR LONPFA.C TO PASS TO LONPF2.C.
      *  083188 SLC ADDED POFF-REBATE 9 - 10.
      *  103188 JTG CORRECTED LOGIC FOR 9 - 10
      *             CHANGED TO SKIP ROUITNE IF CURBAL = 0
      *  030389 JTG OPENED UP LOGIC FOR DEFERMENT REBATES
      *             TO BE INCLUDED IN PAYOFF
      *  102489 JTG PASSED INT REBATE TO REBATE ROUTINE FOR SERCHG
      *  011890 JTG ADDED TEST FOR SP-RBMIN(7) = 999.01 (GA)
      *  022090 SLC FIXED TEST FOR GEORGIA MIN REBATE ON INTEREST
      *             WAS NOT ADDING REBATE TO NET DUE BEFORE 0'ING IT
      *  032990 BAH FIXED TEST FOR RBCOMCD 'A' MINIMUM 1.00 WAS
      *             NOT ADDING REBATE TO NET DUE BEFORE 0'ING IT
      *  072590 JTG SET LCAP-LPAPLC AND LCAP-LPAPINT
      *  012991 JTG OPENED UP ABILITY FOR CL,AH,PP WRITTEN AFTER BOOKING
      *  072391 SLC NOW SETS INTEREST IF PRECOMP IN CASE THERE IS
      *             SOMETHING IN INTBAL. WAS CAUSING PAYOFF TO LEAVE
      *             SOMETHING IN CURBAL.
      *  072591 JTG CHANGED TO WORK WITHOUT LOGIC FOR LONPFA.C
      *  070192 JTG CHANGED TO USE LT-ANTIC-START-DATE AND NOT
      *             LT-INSEFF FOR INTEREST ADDON REBATES
      *  082292 JTG MODIFIED TO SET REB-LN(FIELDS) FOR ADDON
      *                 INTEREST ACTUARIAL REFUND
      *  083192 JTG STOPPED ADDON INT. REBATE FROM EXCEEDING ORG CHG
      *  100992 JTG CORRECTED TO SET REB-LN(FIELDS) FOR ADDON
      *                 INTEREST ACTUARIAL REFUND
      *  071896 JTG ADDED LOGIC FOR UNIT PERIODS
      *  051597 MJD ADDED POFF-INT-REB-TABLE. MOVED PAYOFF-INT-ADDONS
      *             TO LPPOF2 SO THAT IT CAN BE CALLED FROM LONPG7.C.
      *  052097 JTG ADDED 'LIBLP/ADDONSPR' TO GET CORRECT SPR RE: INS RB
      *  060397 JTG ADDED LOGIC FOR 'O3' REBATE (R3)
      *  061897 JTG CHANGED TO CLEAR POFF-INSEFF-TEST & POFF-INSEXP-TEST
      *  111197 BAH FIXED BUG REGARDING REB-SUB2 WHEN GETTING ADDON SPR'S
      *  010598 JTG MOVE OF POFF-RECAST-FG TO REB-RECAST RE: IDEAL A48 TO A60
      *  042798  CS ADDED TEST OF "RB" AND "RO"
      *  072898 JTG ADDED POFF-O2DUE RE: RENEWAL O2 MONIES MERCURY #192
      *  050198 MJD ADDED LOGIC FOR "RB" AND "RO" TRANSACTION/SOURCE CODES
      *  082698 JTG ALLOWED REFUND OF SERVICE CHARGE ON IB LOAN TYPES
      *  990826 JTG ADDED SCCS-COPY-ROUTINE
      *  MJD 000119 ADDED POFF-TOTNOLCHG (WORLD PR#218)
      *             NOT YET USED. MUST BE ENABLED BEFORE WORLD GOES TO A15
      *  BAH 000124 ADDED SP-RBCOMCD "B" FOR REGENCY PR# 1048 TO TEST CL&AH
      *             MINIMUM OF $10.00 FOR "PA"
      *  JTG 000822 ADDED LOGIC FOR KENTUCKY 60 DAY NO CHARGE LAW, ADDED
      *             SP-RBSPOPT1() = 19, WORLD #233
      *  JTG 010919 ADDED LOGIC FOR PAYOFF ON SP-CAL-RATETYPE(Z)LENDMARK PR#1045
      *  JTG 020307 ADDED "CT" COUNTY TAX CODE KENTUCKY AND PERCENTS
      *             ADDED "CY" CITY   TAX CODE KENTUCKY AND PERCENTS
      *             ADDED  FOR POFF-INS-TAX OCCURS 8              WORLD #288
      *  JTG 020402 CHANGED POFF-INS-TAX TO POFF-INS-TAX-RB       WORLD #288
      *  JTG 020801 CHANGED FOR (MIP) MONTHLY INSURANCE PREMIUMS   REGENCY #1801
      *  JTG 020927 SET THE FOLLOWING FIELDS FOR ADDON ACCOUNTS    MULLEN  #
      *             JGMULL  MOVE LN-LASTPYAMT  TO REB-LN-LASTPYAMT
      *             JGMULL  MOVE LN-APRATE     TO REB-LN-APRATE
      *             JGMULL  MOVE LN-SMPRATE    TO REB-LN-SMPRATE
      *  JTG 021220 ADDED POFF-LCAP-BATCH-REFCD POFF-LCAP-BATCH-REFCD-LC-FG
      *             ADDED NEW CODE FILE TYPE 'BR'-BATCH PAYMENT REFERENCE CODES
      *             BATCH PAYMENT REVISIONS                        REGACC #0006
      *  JTG 030129 ADDED LOGIC FOR REBATES ON ACCOUNTS CONVERTED FOR
      *             ALABAMA WITH ADDON INSURANCE THAT WAS NOT CONVERTED
      *             AS AN ADDON. THE LOGIC IS DEFAULTING TO LOAN DATE
      *             AND NOT INSURANCE EFFECTIVE DATE                    WORLD
      *  JTG 030305 ALLOW RBFRMLA '8' TO WORK FOR 'AH' & 'ALL COMPUTABLE INS'
      *                                                         PACESETTER #2012
      *  JTG 040913 ADDED NEW FIELDS FOR SP-RBFRMLA (0 D)
      *             RULE 78THS USING DAYS IN TERM AND ELAPSED DAYS INSTEAD
      *             OF UNIT PERIODS IN TERM AND ELAPSE UNIT PERIODS REGACC #0074
      *  BAH 050714 CALCULATE MIP MONTHLY INSURANCE ON RENEWALS, REGWAN PL# 499
      *  JTG 050714 ADDED NEW FORMULAS FOR MEXICO
      *             SP-MFFRMLA-05 SP-RBFRMLA2 (G) SP-ERNFRMLA (24)   WORLD #0408
      *  JTG 070426 REMOVED ADDED LOGIC FOR REBATES ON ACCOUNTS CONVERTED FOR
      *             ALABAMA WITH ADDON INSURANCE THAT WAS NOT CONVERTED
      *             AS AN ADDON. THE LOGIC IS DEFAULTING TO LOAN DATE
      *             AND NOT INSURANCE EFFECTIVE DATE            WORLD JWR BARB
      * MJD 080623 ADDED LOGIC FOR MEXICO:
      *             IF MFFRMLA "5" AND RBFRMLA "0G" THEN USE
      *             MFFEE AMOUNT ON PAGE 6 OF LOAN MAINT.   SOME LOANS
      *             COULD CAUSE MONTHLY MAINT FEE TO BE 0 (ZERO) DUE
      *             TO TRUNCATION OF SIG DIGITS (I.E. 300.00 BECOMES 00.00).
      * MJD 091016 ADDED LOGIC TO ALLOW CALCULATION OF MFEE REBATES ON IB
      *             ACCOUNTS.                                   WORLD PR#590
      * BAH 110307 ADDED SP-RBCOMCD "C" FOR REGENCY PR# 3743 TO TEST CL&AH
      *             MINIMUM OF $5.00
      * BAH 130115 ADDED SP-RBSPOPT1 27 FOR "CA" EARLY MULLEN PR# 4182
      * MJD 160223 ADDED LOGIC TO UPDATE POFF-LXR-REB-AMT FOR GA EARLY PROVISION
      *                                                           REGMGT PR#4915
      * BAH 2018.07.02 ADDED ADDONSPRW AND SET ADDONSPR-SUB, DONT SHARE
      *                REBATEW WORKERS ANYMORE!
      * BAH 0517.2023 DISABLED CALIFORNIA IB RECASTING FORMULA 2 (RCASTC) KM
      **************************************************************************
       PAYOFF-LOAN-ROUTINE SECTION.
           MOVE 0 TO POFF-REBATE(1)  POFF-REBATE(2)
                     POFF-REBATE(3)  POFF-REBATE(4)
                     POFF-REBATE(5)  POFF-REBATE(6)
                     POFF-REBATE(7)  POFF-REBATE(8)
                     POFF-REBATE(9)  POFF-REBATE(10)
                     POFF-REBATE(11) POFF-REBATE(12)
                     POFF-REBATE(13)
                     POFF-INS-TAX-RB(1) POFF-INS-TAX-RB(2)
                     POFF-INS-TAX-RB(3) POFF-INS-TAX-RB(4)
                     POFF-INS-TAX-RB(5) POFF-INS-TAX-RB(6)
                     POFF-INS-TAX-RB(7) POFF-INS-TAX-RB(8)
                     POFF-O2DUE
                     POFF-OTDUE     POFF-LCDUE
                     POFF-INTDUE    POFF-NETDUE
                     POFF-PARTIALS  POFF-LCPAID.
           MOVE 0 TO POFF-ADDON-INT-EARNED
                     POFF-ADDON-INT-REBATE.
           MOVE 0 TO POFF-INT-REB-TBL(1)  POFF-INT-REB-TBL(5)
                     POFF-INT-REB-TBL(2)  POFF-INT-REB-TBL(6)
                     POFF-INT-REB-TBL(3)  POFF-INT-REB-TBL(7)
                     POFF-INT-REB-TBL(4)  POFF-INT-REB-TBL(8).
           MOVE 0 TO POFF-LXM-MIP-CLPAID
                     POFF-LXM-MIP-AHPAID
                     POFF-LXM-MIP-CLPDTH-DATE
                     POFF-LXM-MIP-AHPDTH-DATE
                     POFF-LXR-REB-AMT.

           MOVE POFF-RECAST-FG TO REB-RECAST.
           MOVE SPACES TO POFF-COMMON-RTNCD POFF-COMMON-AREA.
           MOVE 0 TO POFF-COMMON-INTADJ.

           IF LN-CURBAL = 0
              GO TO PAYOFF-EXIT.

      *******************************************************
      *    CLEAR REBATE WORKERS FOR MEXICO SP-RBFRMLA2 0G
      *******************************************************

      ************************************************
      *    COMPUTE LATE CHARGES AND INTEREST OWING:
      ************************************************
           MOVE 0 TO REB-LN-INT-REBATE
                     REB-LN-SC-REBATE
                     REB-LN-MF-REBATE
                     REB-LN-DF-REBATE.

      * SET IB-PC FLAG:
           MOVE POFF-PAYDATE TO IBPC-DATE.
           PERFORM IBPC-TEST.

      * NOTE: LN-OT2BAL IS INCLUDED IN LN-CURBAL.
           MOVE LN-OT2BAL TO POFF-O2DUE.
           MOVE LN-OTHBAL TO POFF-OTDUE.
           ADD LN-CURBAL LN-OTHBAL LN-INTBAL LN-LCBAL
                                         GIVING POFF-NETDUE.

      * DETERMINE IF MIP INSURANCE PREMIUMS ARE DUE
      
           IF LN-MIP-FG = "Y"
              IF POFF-LPTRCD = "PO" OR "PB" OR "RN" OR "SC" OR
                               "RB" OR "RO"
                 PERFORM DETERMINE-MIP-REQUIRED
                 ADD POFF-LXM-MIP-CLPAID
                     POFF-LXM-MIP-AHPAID TO POFF-NETDUE.

      * COMPUTE INTEREST DUE IF IB:
           IF IBPC-FG = "I" AND LN-LOANTYPE = "P"
              MOVE LN-LCBAL TO POFF-LCDUE
              PERFORM PAYOFF-SET-INT-CHARGES
           ELSE
      * COMPUTE LATE CHARGES IF P/C AND NOT EXPIRING:
           IF IBPC-FG = "P" AND IBPC-EXPIRED-FLAG = " "
              PERFORM PAYOFF-SET-INT-CHARGES
              PERFORM PAYOFF-SET-LATE-CHARGES.

      * COMPUTE LATE CHARGES AND INTEREST IF P/C AND EXPIRING:
           IF (IBPC-FG = "P" AND IBPC-EXPIRED-FLAG = "E") OR
              (LN-LOANTYPE = "I")
              PERFORM PAYOFF-SET-LATE-CHARGES
              PERFORM PAYOFF-SET-INT-CHARGES.

      *********************************************
      *    COMPUTE FINANCE CHARGE REBATES:
      *********************************************

      * COMPUTE INTEREST REBATE IF (PC):

           IF IBPC-FG = "P"
              IF LN-INT-REBATE = " " OR = "C"
                 MOVE 7 TO REB-SUB
                 MOVE LN-INTCHG  TO REB-TOTCHG
                 MOVE LN-ORGTERM TO REB-ORGTERM
                 PERFORM PAYOFF-GET-REBATE
                 MOVE REB-REBATE TO POFF-REBATE(4)
                                    REB-LN-INT-REBATE
                 IF LN-ADDON-FG = "Y"
                    MOVE 0 TO POFF-INSEFF-TEST
                              POFF-INSEXP-TEST
                    PERFORM PAYOFF-INT-ADDONS
                    ADD POFF-ADDON-INT-REBATE TO POFF-REBATE(4)
                    SUBTRACT POFF-ADDON-INT-REBATE FROM POFF-NETDUE.

      * COMPUTE SERVICE REBATE IF (PC):

           PERFORM PAYOFF-SERCHG-REBATE.

      * TEST FOR GEORGIA MIN REBATE ON INTEREST:
           IF SP-RBMIN(7) = 999.01
              IF POFF-REBATE(5) = 0
                 IF POFF-REBATE(4) < 1.00
                    ADD POFF-REBATE(4) TO POFF-NETDUE
                    MOVE 0 TO POFF-REBATE(4).

      * COMPUTE MAINT FEE REBATE IF (PC):
      * ADDED LOGIC FOR MEXICO: IF MFFRMLA "5" AND RBFRMLA "0G" THEN USE
      *       MFFEE AMOUNT ON PAGE 6 OF LOAN MAINT.   SOME LOANS
      *       COULD CAUSE MONTHLY MAINT FEE TO BE 0 (ZERO) DUE
      *       TO TRUNCATION OF SIG DIGITS (I.E. 300.00 BECOMES 00.00).
           IF IBPC-FG = "P" OR
                        (IBPC-FG = "I" AND GP-ALLOW-MF-ON-IB = "Y")
              IF SP-MFFRMLA-05 AND
                    SP-RBFRMLA(9)  = "0" AND
                       SP-RBFRMLA2(9)  = "G" AND
                          LN-ANTICERN(4) NOT = 0
                 IF LN-MF-REBATE = " " OR = "C"
                    MOVE 9 TO REB-SUB
                    COMPUTE REB-TOTCHG = LN-ANTICERN(4) * LN-ORGTERM
                    MOVE LN-ORGTERM TO REB-ORGTERM
                    PERFORM PAYOFF-GET-REBATE
                    MOVE REB-REBATE TO POFF-REBATE(6)
                 END-IF
              ELSE
              IF LN-MAINTFEE NOT = 0
                 IF LN-MF-REBATE = " " OR = "C"
                    MOVE 9 TO REB-SUB
                    COMPUTE REB-TOTCHG = LN-MAINTFEE * LN-ORGTERM
                    MOVE LN-ORGTERM TO REB-ORGTERM
                    PERFORM PAYOFF-GET-REBATE
                    MOVE REB-REBATE TO POFF-REBATE(6).

      * COMPUTE DEFERMENT REBATE IF (PC):
           IF IBPC-FG = "P"
              IF LN-TOTDEF NOT = 0
                 IF LN-DF-REBATE = " " OR = "C"
                    MOVE 10 TO REB-SUB
                    MOVE LN-TOTDEF TO REB-TOTCHG
                    MOVE LN-ORGTERM TO REB-ORGTERM
                    MOVE POFF-DEF-STOP TO REB-DEF-STOP
                    PERFORM PAYOFF-GET-REBATE
                    MOVE REB-REBATE TO POFF-REBATE(7).

      *********************************************
      *    COMPUTE INSURANCE REBATES:
      *********************************************

      * CLEAR ITRM-TODAY SO INSURANCE INEFFECT WON'T
      * BE CALCULATED IN LPITRM.
           MOVE 0 TO ITRM-TODAY.

           IF LN-INSURED = "N"
              GO TO PAYOFF-OTHER-INS.

      * COMPUTE CREDIT LIFE REBATE:
           IF LN-CL-REBATE NOT = "Y"
              IF LN-INSPREM(1) NOT = 0
                 IF LN-INSOURS(1) NOT = "M"
                    PERFORM PAYOFF-SET-CL-REBSUB
                    MOVE LN-INSPREM(1) TO REB-TOTCHG
                    MOVE 1 TO ITRM-SUB
                    PERFORM INS-TERM-CALCULATION
                    MOVE ITRM-INSTERM TO REB-ORGTERM
                    IF LN-ADDON-FG = "Y"
                       MOVE 1 TO POFF-SUB
                       PERFORM PAYOFF-SET-ADDON-INS
                    END-IF
                    PERFORM PAYOFF-GET-REBATE
                    MOVE REB-REBATE  TO POFF-REBATE(1)
                    MOVE INS-TAX-RB TO POFF-INS-TAX-RB(1).

      * COMPUTE ACCIDENT HEALTH REBATE:
           IF LN-AH-REBATE NOT = "Y"
              IF LN-INSPREM(2) NOT = 0
                 IF LN-INSOURS(2) NOT = "M"
                    MOVE 5 TO REB-SUB
                    MOVE LN-INSPREM(2) TO REB-TOTCHG
                    MOVE 2 TO ITRM-SUB
                    PERFORM INS-TERM-CALCULATION
                    MOVE ITRM-INSTERM TO REB-ORGTERM
                    IF LN-ADDON-FG = "Y"
                       MOVE 2 TO POFF-SUB
                       PERFORM PAYOFF-SET-ADDON-INS
                    END-IF
                    PERFORM PAYOFF-GET-REBATE
                    MOVE REB-REBATE  TO POFF-REBATE(2)
                    MOVE INS-TAX-RB TO POFF-INS-TAX-RB(2).


      * TEST REBATE COMBINED CL/AH CODE:
           IF SP-RBCOMCD NOT = "A" AND NOT = "B" AND NOT = "C"
              GO TO PAYOFF-PP-INSURANCE.

      * TEST FOR SP-RBSPOPT1 = 2 FOR REBATE TO THE PENNY:
           PERFORM PAYOFF-SET-CL-REBSUB.
           IF SP-RBSPOPT1(REB-SUB) = 2 OR SP-RBSPOPT1(5) = 2
              IF POFF-LPTRCD = "PB" OR "RN" OR "SC" OR "RB" OR "RO"
                 GO TO PAYOFF-PP-INSURANCE.

           IF SP-RBCOMCD = "A"
              IF (POFF-REBATE(1) + POFF-REBATE(2)) < 1.00
                 ADD POFF-REBATE(1) POFF-REBATE(2) TO POFF-NETDUE
                 MOVE 0 TO POFF-REBATE(1) POFF-REBATE(2).
           IF SP-RBCOMCD = "B"
              IF (POFF-REBATE(1) + POFF-REBATE(2)) < 10.00
                 ADD POFF-REBATE(1) POFF-REBATE(2) TO POFF-NETDUE
                 MOVE 0 TO POFF-REBATE(1) POFF-REBATE(2).
           IF SP-RBCOMCD = "C"
              IF (POFF-REBATE(1) + POFF-REBATE(2)) < 5.00
                 ADD POFF-REBATE(1) POFF-REBATE(2) TO POFF-NETDUE
                 MOVE 0 TO POFF-REBATE(1) POFF-REBATE(2).

        PAYOFF-PP-INSURANCE.
      * COMPUTE PERSONNEL PROPERTY REBATE:
           IF LN-PP-REBATE NOT = "Y"
              IF LN-INSPREM(3) NOT = 0
                 IF LN-INSOURS(3) NOT = "M"
                    MOVE 6 TO REB-SUB
                    MOVE LN-INSPREM(3) TO REB-TOTCHG
                    MOVE 3 TO ITRM-SUB
                    PERFORM INS-TERM-CALCULATION
                    MOVE ITRM-INSTERM TO REB-ORGTERM
                    IF LN-ADDON-FG = "Y"
                       MOVE 3 TO POFF-SUB
                       PERFORM PAYOFF-SET-ADDON-INS
                    END-IF
                    PERFORM PAYOFF-GET-REBATE
                    MOVE REB-REBATE  TO POFF-REBATE(3)
                    MOVE INS-TAX-RB TO POFF-INS-TAX-RB(3).

       PAYOFF-OTHER-INS.
      * COMPUTE OTHER 1 INSURANCE REBATE:
           IF LN-O1-REBATE NOT = "Y"
              IF LN-INSPREM(4) NOT = 0
                 IF LN-INSOURS(4) NOT = "M"
                    MOVE 12 TO REB-SUB
                    MOVE LN-INSPREM(4) TO REB-TOTCHG
                    MOVE 4 TO ITRM-SUB
                    PERFORM INS-TERM-CALCULATION
                    MOVE ITRM-INSTERM TO REB-ORGTERM
                    IF LN-ADDON-FG = "Y"
                       MOVE 4 TO POFF-SUB
                       PERFORM PAYOFF-SET-ADDON-INS
                    END-IF
                    PERFORM PAYOFF-GET-REBATE
                    MOVE REB-REBATE  TO POFF-REBATE(9)
                    MOVE INS-TAX-RB TO POFF-INS-TAX-RB(4).


      * COMPUTE OTHER 2 INSURANCE REBATE:
           IF LN-O2-REBATE NOT = "Y"
              IF LN-INSPREM(5) NOT = 0
                 IF LN-INSOURS(5) NOT = "M"
                    MOVE 13 TO REB-SUB
                    MOVE LN-INSPREM(5) TO REB-TOTCHG
                    MOVE 5 TO ITRM-SUB
                    PERFORM INS-TERM-CALCULATION
                    MOVE ITRM-INSTERM TO REB-ORGTERM
                    IF LN-ADDON-FG = "Y"
                       MOVE 5 TO POFF-SUB
                       PERFORM PAYOFF-SET-ADDON-INS
                    END-IF
                    PERFORM PAYOFF-GET-REBATE
                    MOVE REB-REBATE  TO POFF-REBATE(10)
                    MOVE INS-TAX-RB TO POFF-INS-TAX-RB(5).

      * COMPUTE OTHER 3 INSURANCE REBATE:
           IF LN-O3-REBATE NOT = "Y"
              IF LN-INSPREM(6) NOT = 0
                 IF LN-INSOURS(6) NOT = "M"
                    MOVE 14 TO REB-SUB
                    MOVE LN-INSPREM(6) TO REB-TOTCHG
                    MOVE 6 TO ITRM-SUB
                    PERFORM INS-TERM-CALCULATION
                    MOVE ITRM-INSTERM TO REB-ORGTERM
                    IF LN-ADDON-FG = "Y"
                       MOVE 6 TO POFF-SUB
                       PERFORM PAYOFF-SET-ADDON-INS
                    END-IF
                    PERFORM PAYOFF-GET-REBATE
                    MOVE REB-REBATE  TO POFF-REBATE(11)
                    MOVE INS-TAX-RB TO POFF-INS-TAX-RB(6).

       PAYOFF-EXIT.
           EXIT.

      ****************************************
      *    SETUP FOR INSURANCE ADDON REFUND
      ****************************************
