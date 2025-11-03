       IDENTIFICATION DIVISION.
       PROGRAM-ID.       REBA01.
       DATE-WRITTEN.   10/21/88.
      ************************************************************************
      *                      (CL)
      *    DESCRIPTION:  REBATE REDUCTION CALC FOR MISSISSIPPI
      *                  CALLED FROM REBATE ROUTINE
      *                   IF RBREDUC = 9999.01
      *
      * WORLD DOES NOT CURRENTLY USE THIS PROGRAM/FORMULA  5/17/2023
      *
      * ** NOTE: **
      *     A01-ELAPSED-MO IS INCREASED BY 1 MONTH IF THE ELAPSED-REM
      *     IS > 0...% CHANGES BASED UPON WHICH YR YOU ARE IN, AND 1
      *     DAY REMAINER WOULD BE INTO THE NEXT MONTH (OR YEAR).
      *
      * REV:
      *  BAH 021397 COMPUTE INSURANCE REBATES AND SUBTRACT FROM
      *             CURRENT BALANCE AS NET BALANCE, PER "MS" AUDITORS (WDT)
      *  JTG 051297 ADDED USE OF 'LIBLP/SETCTBLW' & 'LIBLP/SETCTBL'
      *  JTG 052097 ADDED 'LIBLP/ADDONSPR' TO GET CORRECT SPR RE: INS RB
      *  JTG 060397 ADDED LOGIC FOR 'O3' INSURANCE
      *  JTG 092697 CHANGED 'REBATEW' TO CONFORM TO NEW VERSION
      *             RE: PERFORM 'LIBLP/ADDONRB' COMMON LOGIC TO COMPUTE
      *             APR RATE FOR ACTUARIAL REBATE FORMULAS
      *   CS 042798 ADD RB AND RO TO TEST FOR POFF-LPTRCD
      *  BAH 021799 ADDED DECIMAL TO REB-HALF-ORGTERM FOR DEDCHG 9999.02 WORLD
      *  BAH 022699 REMOVED DECIMAL FROM REB-HALF-ORGTERM AND ADDED
      *             REB-HALF-ORGTERM-DEC FOR 9999.04 (WORLD)
      *  MG  990929 FIXED VDUSTATUS/IO-FG/STAT PER DEVELOPMENT MEETING.
      *  BAH 000124 ADDED SP-RBCOMCD "B" FOR REGENCY PR# 1048 TO TEST CL&AH
      *             MINIMUM OF $10.00 FOR "PA"
      *  JTG 000525 CHANGED TO USE REBATEW FIELDS AND NOT HARD CODED FIELDS
      *  JTG 011026 CHANGED RBACT-APRATE TO RBACT-RATE FOR NEW REFUND FORMULA
      *             '0  6' AND SET REB-LN-SMPRATE               REGENCY PR#1631
      *  JTG 020307 ADDED "CT" COUNTY TAX CODE KENTUCKY AND PERCENTS
      *             ADDED "CY" CITY   TAX CODE KENTUCKY AND PERCENTS
      *             ADDED  FOR POFF-INS-TAX OCCURS 8              WORLD #288
      *  JTG 020402 CHANGED POFF-INS-TAX TO POFF-INS-TAX-RB       WORLD #288
      *  JTG 030129 ADDED LOGIC FOR REBATES ON ACCOUNTS CONVERTED FOR
      *             ALABAMA WITH ADDON INSURANCE THAT WAS NOT CONVERTED
      *             AS AN ADDON. THE LOGIC IS DEFAULTING TO LOAN DATE
      *             AND NOT INSURANCE EFFECTIVE DATE                    WORLD
      *  JTG 030305 ALLOW RBFRMLA '8' TO WORK FOR 'AH' & 'ALL COMPUTABLE INS'
      *                                                         PACESETTER #2012
      *  BAH 040330 WHEN THIS CHANGED TO USE REBATEW WORKERS AND RENAMED
      *             THE LINKAGE TO ABC-COMMON, THE FIELDS THAT GET COMPUTED
      *             IN HERE NEVER GOT PASSED BACK TO THE CALLING PROGRAM;
      *             A01-MS-PENALTY NEVER MADE IT BACK TO THE REBATE COPY
      *             MEMBER IN PAYOFF..!!! FINMGM/JUDY PL# 437
      *  JTG 040913 ADDED NEW FIELDS FOR SP-RBFRMLA (0 D)
      *             RULE 78THS USING DAYS IN TERM AND ELAPSED DAYS INSTEAD
      *             OF UNIT PERIODS IN TERM AND ELAPSE UNIT PERIODS REGACC #0074
      *   CS 060630 ADD LT-REC TO LINKAGE NEEDS FOR REBATE PR#534 CENTILL
      *  JTG 070426 REMOVED ADDED LOGIC FOR REBATES ON ACCOUNTS CONVERTED FOR
      *             ALABAMA WITH ADDON INSURANCE THAT WAS NOT CONVERTED
      *             AS AN ADDON. THE LOGIC IS DEFAULTING TO LOAN DATE
      *             AND NOT INSURANCE EFFECTIVE DATE            WORLD JWR BARB
      * BAH 110307 ADDED SP-RBCOMCD "C" FOR REGENCY PR# 3743 TO TEST CL&AH
      *             MINIMUM OF $5.00
      * BAH 160205 CHANGED LT TO LTI, PD0003
      * MJD 160613 ADDED LTI-FILE. NEED FOR CHANGES MADE TO REBATE. 
      *            HAD TO ADD REBA01-LTI-REC TO LINKAGE TO REPLACE LP01LTI
      *            RAN INTO CONFLICT WHEN ADDING LT COPY BOOKS.     71105#0918
      * BAH 2018.07.02 ADDED ADDONSPRW AND SET ADDONSPR-SUB 
      *                DONT SHARE REBATEW WORKERS ANYMORE!
      ************************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       I-O-CONTROL.
       DATA DIVISION.
       FILE SECTION.

       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)CL/REBA01.CBL S35 01/31/22-15:34:39 >".
      ****************************
      *     F I L E   I F N      *
      ****************************
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBLP/LP01CD.CPY".
       COPY "LIBLP/LP01CD_SQL.CPY".
       COPY "LIBLP/LPWSCD.CPY".
       COPY "LIBLP/LP01SP.CPY".
       COPY "LIBLP/LP01SPA.CPY".
       COPY "LIBLP/LP01SPB.CPY".
       COPY "LIBLP/LP01SPC.CPY".
       COPY "LIBLP/LP01SP_SQL.CPY".
       COPY "LIBLP/LPWSSP.CPY".
       COPY "LIBLP/LP01LTI.CPY".
       COPY "LIBLP/LP01LTI_SQL.CPY".
       COPY "LIBLP/LPWSLTI.CPY".

       01  ERRCD                     PIC X VALUE SPACES.

       COPY "LIBGB/REBATEW.CPY".

       COPY "LIBGB/RBACTW.CPY".
       COPY "LIBLP/ADDONSPRW.CPY".
       COPY "LIBLP/LPITRMW.CPY".
       COPY "LIBLP/LPPOFFW.CPY".
       COPY "LIBLP/LPMDTEW.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBLP/LPAMTSW.CPY".
       COPY "LIBLP/LPAPRSW.CPY".
       COPY "LIBLP/LPAPRZW.CPY".
       COPY "LIBLP/SETCTBLW.CPY".
       COPY "LIBLP/LPUPERW.CPY".
       COPY "LIBLP/LPUPWKW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".

       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".

      ****************************************
      *     LINKAGE PROGRAM SECTION:
      ****************************************
       COPY "LIBGB/GETFMW.CPY".
       01  INDU-COMMON.
           03  INDU-COMMON-RTNCD       PIC X.
           03  INDU-COMMON-REBATE      PIC S9(7)V99 COMP-3.
           03  INDU-COMMON-AREA        PIC X(40).
       COPY "LIBLP/LP01LN.CPY".
       01 REBA01-LTI-REC PIC X(LTI-SIZE).

       PROCEDURE DIVISION
            USING FORM-PATHNAME INDU-COMMON LN-REC REBA01-LTI-REC
                  EXIT-PATHNAME.

      ******************************************************
       MAIN-MODULE SECTION.
      ******************************************************

           PERFORM SQL-CONNECT.

      D    STOP MESS.
           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE REBA01-LTI-REC TO LTI-REC.
       MAIN-ROUTINE.
           MOVE INDU-COMMON TO REB-COMMON.
           MOVE 0 TO A01-MS-PENALTY.

      * TEST FOR REAL ESTATE SECURED:
           IF LN-COLLCD = "  "
              GO TO END-PROGRAM.

           PERFORM OPEN-CD1-FILE.
           MOVE "CO"      TO CD-TYPE.
           MOVE LN-COLLCD TO CD-CODE.
           PERFORM READ-CD1-FILE.
           PERFORM CLOSE-CD1-FILE.
           IF (IO-BAD) OR (CD-CO-SECURED NOT = "R")
              GO TO END-PROGRAM.

      * MUST LEAVE SP1-FILE OPEN BECAUSE ADDONSPR READS IT
           PERFORM OPEN-SP1-FILE.

           IF LN-INSPREM(1) NOT = 0 OR LN-INSPREM(2) NOT = 0
             OR LN-INSPREM(3) NOT = 0 OR LN-INSPREM(4) NOT = 0
              OR LN-INSPREM(5) NOT = 0 OR LN-INSPREM(6) NOT = 0
                 MOVE LN-ORGST TO SP-ORGST
                 MOVE LN-SPRCLASS TO SP-SPRCLASS
                 MOVE LN-SUBCLASS TO SP-SUBCLASS
                 MOVE LN-LAWCODE TO SP-LAWCODE
                 PERFORM READ-SP1-FILE
                 IF IO-BAD
                    GO TO IO-ERROR
                 ELSE
                    MOVE LN-OWNBR TO BR-NO
                    PERFORM OPEN-BR-FILE
                    PERFORM READ-BR-FILE
                    PERFORM CLOSE-BR-FILE
                    IF IO-BAD
                       GO TO IO-ERROR.


           MOVE 0 TO POFF-REBATE(1)  POFF-REBATE(2)  POFF-REBATE(3)
                     POFF-REBATE(9)  POFF-REBATE(10) POFF-REBATE(11)
                     POFF-REBATE(12) POFF-REBATE(13)
                     POFF-INS-TAX-RB(1) POFF-INS-TAX-RB(2)
                     POFF-INS-TAX-RB(3) POFF-INS-TAX-RB(4)
                     POFF-INS-TAX-RB(5) POFF-INS-TAX-RB(6)
                     POFF-INS-TAX-RB(7) POFF-INS-TAX-RB(8).

      * PREPAYMENT PENALTY 9999.01 MUST BE BASED ON NET BALANCE
      * CURRENT BALANCE LESS INTEREST REFUND AND ALL INSURANCE REFUNDS
      * AS PER STATE AUDITOR AT WDT 2/3/97 PR#442
           IF LN-CL-REBATE NOT = "Y"
              IF LN-INSPREM(1) NOT = 0
                 IF LN-INSOURS(1) NOT = "M"
                    PERFORM PAYOFF-SET-CL-REBSUB
                    MOVE LN-INSPREM(1) TO REB-TOTCHG
                    MOVE 1 TO ITRM-SUB
                    PERFORM INS-TERM-CALCULATION
                    MOVE ITRM-INSTERM TO REB-ORGTERM
                    MOVE "RC" TO REB-LPTRCD
                    IF LN-ADDON-FG = "Y"
                       MOVE 1 TO POFF-SUB
                       PERFORM PAYOFF-SET-ADDON-INS
                    END-IF
                    PERFORM PAYOFF-GET-REBATE
                    MOVE REB-REBATE TO POFF-REBATE(1)
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
                    MOVE "RA" TO REB-LPTRCD
                    IF LN-ADDON-FG = "Y"
                       MOVE 2 TO POFF-SUB
                       PERFORM PAYOFF-SET-ADDON-INS
                    END-IF
                    PERFORM PAYOFF-GET-REBATE
                    MOVE REB-REBATE TO POFF-REBATE(2)
                    MOVE INS-TAX-RB TO POFF-INS-TAX-RB(2).

      * TEST REBATE COMBINED CL/AH CODE:
           IF SP-RBCOMCD NOT = "A" AND NOT = "B" AND NOT = "C"
              GO TO PAYOFF-PP-INSURANCE.

      * TEST FOR SP-RBSPOPT1 = 2 FOR REBATE TO THE PENNY:
           PERFORM PAYOFF-SET-CL-REBSUB.
           IF SP-RBSPOPT1(REB-SUB) = 2 OR SP-RBSPOPT1(5) = 2
              IF POFF-LPTRCD = "PB" OR "RN" OR "SC"
                                  OR "RB" OR "RO"
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
      * COMPUTE PERSONAL PROPERTY REBATE:
           IF LN-PP-REBATE NOT = "Y"
              IF LN-INSPREM(3) NOT = 0
                 IF LN-INSOURS(3) NOT = "M"
                    MOVE 6 TO REB-SUB
                    MOVE LN-INSPREM(3) TO REB-TOTCHG
                    MOVE 3 TO ITRM-SUB
                    PERFORM INS-TERM-CALCULATION
                    MOVE ITRM-INSTERM TO REB-ORGTERM
                    MOVE "RP" TO REB-LPTRCD
                    IF LN-ADDON-FG = "Y"
                       MOVE 3 TO POFF-SUB
                       PERFORM PAYOFF-SET-ADDON-INS
                    END-IF
                    PERFORM PAYOFF-GET-REBATE
                    MOVE REB-REBATE TO POFF-REBATE(3)
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
                    MOVE "R1" TO REB-LPTRCD
                    IF LN-ADDON-FG = "Y"
                       MOVE 4 TO POFF-SUB
                       PERFORM PAYOFF-SET-ADDON-INS
                    END-IF
                    PERFORM PAYOFF-GET-REBATE
                    MOVE REB-REBATE TO POFF-REBATE(9)
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
                    MOVE "R2" TO REB-LPTRCD
                    IF LN-ADDON-FG = "Y"
                       MOVE 5 TO POFF-SUB
                       PERFORM PAYOFF-SET-ADDON-INS
                    END-IF
                    PERFORM PAYOFF-GET-REBATE
                    MOVE REB-REBATE TO POFF-REBATE(10)
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
                    MOVE "R3" TO REB-LPTRCD
                    IF LN-ADDON-FG = "Y"
                       MOVE 6 TO POFF-SUB
                       PERFORM PAYOFF-SET-ADDON-INS
                    END-IF
                    PERFORM PAYOFF-GET-REBATE
                    MOVE REB-REBATE TO POFF-REBATE(11)
                    MOVE INS-TAX-RB TO POFF-INS-TAX-RB(6).


      *  IF PREPAID WITHIN:
      *   1ST YR (0        - 12MO)  5% OF UNPAID PRIN BAL
      *   2ND YR (12MO 1DA - 24MO)  4% OF UNPAID PRIN BAL
      *   3RD YR (24MO 1DA - 36MO)  3% OF UNPAID PRIN BAL
      *   4TH YR (36MO 1DA - 48MO)  2% OF UNPAID PRIN BAL
      *   5TH YR (48MO 1DA - 60MO)  1% OF UNPAID PRIN BAL

      * COMPUTE REDUCED REBATE:
           COMPUTE A01-ELAPSED-MO = (A01-ELAPSED-MO - 1) / 12.

      *    COMPUTE A01-MS-PENALTY ROUNDED =
      *           (LN-CURBAL - REB-COMMON-REBATE)

      * NET BALANCE = CURRENT BALANCE LESS INTEREST REFUND, AND ALL
      * INSURANCE REFUNDS, STATE AUDITOR AT WDT, 2/13/97
           COMPUTE A01-MS-PENALTY ROUNDED =
                  ( LN-CURBAL - REB-COMMON-REBATE
                        - POFF-REBATE(1) - POFF-REBATE(2)
                         - POFF-REBATE(3) - POFF-REBATE(9)
                          - POFF-REBATE(10) - POFF-REBATE(11)
                           - POFF-REBATE(12) - POFF-REBATE(13)
                  )
                    * (5 - A01-ELAPSED-MO) / 100.

           COMPUTE REB-COMMON-REBATE ROUNDED =
                         REB-COMMON-REBATE - A01-MS-PENALTY.

           IF REB-COMMON-REBATE < 0
              MOVE 0 TO REB-COMMON-REBATE.

           GO TO END-PROGRAM.

       COPY "LIBGB/RBACT.CPY".
       COPY "LIBGB/REBATE.CPY".
       COPY "LIBLP/LPITRM.CPY".
       COPY "LIBLP/LPAMTS.CPY".
       COPY "LIBLP/LPMDTE.CPY".
       COPY "LIBGB/DATER.CPY".
       COPY "LIBLP/LPAPRS.CPY".
       COPY "LIBLP/SETCTBL.CPY".
       COPY "LIBLP/LPAPRZ.CPY".
       COPY "LIBLP/LPUPER.CPY".
       COPY "LIBLP/LPUPWK.CPY".
       COPY "LIBLP/ADDONSPR.CPY".

      ******************************************************
       MISC-ROUTINES SECTION.
      ******************************************************
       PAYOFF-SET-CL-REBSUB.
      * TEST FOR SINGLE OR JOINT LIFE:
           IF LN-INSURED = "J"
              MOVE 2 TO REB-SUB
            ELSE
              MOVE 1 TO REB-SUB.
      * TEST FOR LEVEL LIFE:
           IF LN-INSTYPES-LL
              ADD 2 TO REB-SUB.

       PAYOFF-SET-ADDON-INS.
           MOVE LN-INSEFF-DATE(POFF-SUB) TO REB-LN-LOANDATE
                                            REB-LN-PURDATE
                                            REB-LN-INTDATE
                                            REB-LN-INSEFF
                                            NDTE-DATE.

           MOVE LN-INSEXP-DATE(POFF-SUB) TO REB-LN-INSEXP.

           MOVE 1 TO NDTE-HOLD.
           PERFORM INCREMENT-MONTHS.
           MOVE NDTE-DATE            TO REB-LN-ORIG-1STPYDATE.
           MOVE LN-APRATE            TO REB-LN-APRATE.
           MOVE LN-SMPRATE           TO REB-LN-SMPRATE.
           MOVE REB-ORGTERM          TO REB-LN-ORGTERM.
           MOVE LN-REGPYAMT          TO REB-LN-REGPYAMT.
           MOVE 0                    TO REB-LN-TOTNODEF.
           MOVE LN-INSCOVR(POFF-SUB) TO REB-LN-INSCOVR.
           MOVE "N"                  TO REB-LN-SALEFIN.
           MOVE "Y"                  TO REB-ADDON-FG.

       PAYOFF-GET-REBATE.
           MOVE A01-PAYDATE   TO REB-PAYDATE.

           MOVE 0             TO REB-SUB2.
      *   CL:
           IF REB-SUB < 5
              MOVE 1 TO REB-SUB2
           ELSE
      *   AH&PP:
           IF REB-SUB < 7
              SUBTRACT 3 FROM REB-SUB GIVING REB-SUB2
           ELSE
      *   O1 & O2 & O3:
           IF REB-SUB = 12 OR 13 OR 14 OR 15 OR 16
              SUBTRACT 8 FROM REB-SUB GIVING REB-SUB2.

           IF REB-SUB2 NOT = 0
              MOVE REB-SUB2 TO ADDONSPR-SUB
              PERFORM FIND-ADDON-SPR
              PERFORM REBATE
              PERFORM FIND-ADDON-SPR-RESTORE
           ELSE
              PERFORM REBATE.

           SUBTRACT REB-REBATE FROM POFF-NETDUE.

      *************************************************
       IO-ROUTINE SECTION.
      *************************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/DECLARE_SQL.CPY".
           PERFORM CLOSE-SP1-FILE.
           PERFORM CLOSE-LTI1-FILE.
           PERFORM CLOSE-CD1-FILE.
           PERFORM CLOSE-BR-FILE.
       COPY "LIBLP/LPSPGS_SQL.CPY".
       COPY "LIBLP/LPLTIGS_SQL.CPY".
       COPY "LIBLP/LPCDGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBLP/LPCD1RN.CPY".
       COPY "LIBLP/LPSP1RN.CPY".
       COPY "LIBLP/LPLTI1RN.CPY".

       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO ERRCD.

      ******************************************************
       END-PROGRAM SECTION.
      ******************************************************
       ENDIT.
           PERFORM CLOSE-FILES.
           MOVE REB-COMMON TO INDU-COMMON.
           MOVE LTI-REC TO REBA01-LTI-REC.
       EXIT-PROG.
       STOP-RUN.
           EXIT PROGRAM.
