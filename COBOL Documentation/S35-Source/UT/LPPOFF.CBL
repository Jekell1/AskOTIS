      * THIS IS A TEST PROGRAM FOR TURNING LPPOFF COPY MEMBER INTO
      * A CALLED PROGRAM
       IDENTIFICATION DIVISION.
       PROGRAM-ID.      LPPOFF.
       DATE-WRITTEN.    07-03-2018.
      ****************************************************************
      *
      * DESC: THIS PROGRAM IS USED TO COMPUTE THE PAYOFF FIGURES
      *       FOR LOAN PAYOFF INCLUDING ALL REBATES
      *          CREATED FROM LIBLP/LPPOFF.CPY
      *
      * NOTE: LONPFA, POSTING PAYOFF, DOES NOT USE THIS CALLED PROGRAM!
      *          USES LIBLP/FAPOFF.CPY
      *
      * USAGE:
      *  
      *  MUST HAVE BR, LN AND SP ALREADY READ IN THE CALLING PROGRAM
      *  MUST HAVE POFF INPUT FIELDS FILLED IN AS IF YOU WERE CALLING
      *    THE SECTION FROM THE COPY MEMBER
      *****
      * THIS PROGRAM CALLS SEVERAL OTHER PROGRAMS
      *****
      *
      *  USAGE:
      *    MOVE HOLD-TODAYS-DATE  TO POFF-PAYDATE.
      *    MOVE LN-MAKERCD(1)     TO POFF-MAKERCD.
      *    MOVE "PO"              TO  POFF-LPTRCD.
      *
      *    MOVE "CL/LPPOFF" TO FORM-NAM.
      *    CALL FORM-PROGX USING FORM-PATH POFF-WORKERS BR-REC
      *                              LN-REC SP-REC.
      *    CANCEL FORM-PROGX.
      *
      *   IN  : POFF-PAYDATE            DATE OF PAYMENT
      *         POFF-MAKERCD            LN-MAKERCD(X)
      *         POFF-LPTRCD             TO REFLECT THE TYPE OF PAYOFF
      *         POFF-LONPFA-UPD-FG      ONLY REQUIRED FOR LIBLP-FAPOFF
      *         POFF-DEF-STOP           SET TO 'Y' WHEN STOPS ARE NEEDED
      *                                    ON DEFERMENTS - PASSED TO REBATE
      *                                    WILL STOP AND DISPLAY ERROR MESSAGE
      *         POFF-RECAST-FG          TO ALLOW SP-RBRECAST() = 1 TO
      *                                    PERFORM A RECAST OF INTEREST REFUND
      *                                    DEFAULT VALUE " "
      *         POFF-LCAP-BATCH-REFCD        SET FROM ROUTINES THAT POST BATCH
      *                                      PAYMENTS
      *         POFF-LCAP-BATCH-REFCD-LC-FG  SET FROM ROUTINES THAT POST BATCH
      *                                      PAYMENTS
      *
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
      *                    (12)         OT4 REBATE
      *                    (13)         OT5 REBATE
      *         POFF-INS-TAX-RB(1)      CL REBATE INS TAX
      *                        (2)      AH REBATE INS TAX
      *                        (3)      PP REBATE INS TAX
      *                        (4)      O1 REBATE INS TAX
      *                        (5)      O2 REBATE INS TAX
      *                        (6)      O3 REBATE INS TAX
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
      *         POFF-NOLCHG             (FROM LPLCAP) 
      *         POFF-ADDON-INT-EARNED   FOR LONPFA.C
      *         POFF-ADDON-INT-REBATE   FOR LONPFA.C
      *         POFF-LXM-MIP-CLPAID
      *         POFF-LXM-MIP-AHPAID
      *         POFF-LXM-MIP-CLPDTH-DATE
      *         POFF-LXM-MIP-AHPDTH-DATE
      *
      * REV:
      *  180703 BAH NEW, CREATED FROM LIBLP/LPPOFF.CPY
      ****************************************************************
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
           "@(#)UT/LPPOFF.CBL S35 05/17/23-11:39:40 >".
      **************************************************
      *     P R O G R A M   W O R K E R S              *
      **************************************************
       COPY "LIBGB/GB01GB.CPY".
       COPY "LIBGB/GB01GB_SQL.CPY".
       COPY "LIBGB/GBWSGB.CPY".
       COPY "LIBLP/LP01LTI.CPY".
       COPY "LIBLP/LP01LTI_SQL.CPY".
       COPY "LIBLP/LPWSLTI.CPY".
       COPY "LIBLP/LP01LTP.CPY".
       COPY "LIBLP/LP01LTP_SQL.CPY".
       COPY "LIBLP/LPWSLTP.CPY".
       COPY "LIBLP/LP01SP.CPY".
       COPY "LIBLP/LP01SPA.CPY".
       COPY "LIBLP/LP01SPB.CPY".
       COPY "LIBLP/LP01SPC.CPY".
       COPY "LIBLP/LP01SP_SQL.CPY".
       COPY "LIBLP/LPWSSP.CPY".
       01  ERRCD                  PIC X           VALUE " ".

       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBCL/REBATEW.CPY".
       COPY "LIBLP/LPIBPCW.CPY".
       COPY "LIBLP/LPITRMW.CPY".
       COPY "LIBLP/LPXDTEW.CPY".
       COPY "LIBLP/SETCTBLW.CPY".
       COPY "LIBLP/LPLCAPW.CPY".
       COPY "LIBLP/LPLCASW.CPY".
       COPY "LIBLP/LPLCHGW.CPY".
       COPY "LIBLP/LPSCHDW.CPY".
       COPY "LIBGB/SPPDTHW.CPY".
       COPY "LIBLP/LPRATEW.CPY".
       COPY "LIBLP/LPMDTEW.CPY".
       COPY "LIBLP/LPAMTSW.CPY".
       COPY "LIBLP/LPAPRZW.CPY".
       COPY "LIBLP/LPUPERW.CPY".
       COPY "LIBLP/LPUPWKW.CPY".
       COPY "LIBLP/ADDONSPRW.CPY".
       COPY "LIBGB/GPENVW.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".

      /*************************************************
      *   C O N T R O L   M O D U L E                  *
      **************************************************
       COPY "LIBGB/GETFMW.CPY".
      * COPYMEMBER: LIBCL/LPPOFFW  ** USED IN UT/LPPOFF TEST CALLED PROGRAM **
      *-------------------------------------------------------------------------
      *       PAYOFF ROUTINE WORKERS
      *
      * USED: UT/LPPOFF.CBL TEST CALLED PROGRAM 
      *
      * NOTE:
      *       THIS COPY MEMBER IS PASSED TO CALLED PROGRAMS.
      *       SO IF NEW FIELDS ARE ADDED, PUT THEM IN THE FILLER AREAS.
      *
      * REV:
      *  JTG 051089 CHANGED WORKERS TO COMP.
      *  BAH 090192 ADDED POFF-DEF-STOP FOR
      *             COLONIAL DEFERMENTS MESSAGE
      *  BAH 120392 ADDED POFF-RECAST-MESS FOR COSMO
      *             RECASTING TO ONLY DISPLAY MESSAGES
      *             WHEN TRULY RECASTING, NOT IN REPORTS
      *  JTG 022593 ADDED POFF-LPTRCD TO REFLECT PAYOFF TYPE
      *  MJD 051597 ADDED POFF-INT-REB-TBL. REFLECTS INTEREST REBATED FOR
      *             EACH INSURANCE TYPE.
      *  MJD 052097 ADDED A LEVEL TO POFF-INT-REB-TBL FOR FEES.
      *  JTG 060397 ADDED A LEVEL TO POFF-REBATE FOR 'O3' REBATE + 'O4'&'O5'
      *  JTG 061897 ADDED POFF-INSEFF-TEST & POFF-INSEXP-TEST TO BE USED
      *             BY LONPG7.C TO ISOLATE ADDON RECORDS PART OF CANCEL
      *             ADDED POFF-RTNCD WHICH IS TESTED IN LIBLP/LPPOF2 RE:LONPG7.C
      *  JTG 010598 ADDED POFF-RECAST-FG RE: IDEAL A48 TO A60
      *  JTG 072898 ADDED POFF-O2DUE RE: RENEWAL O2 MONIES MERCURY #192
      *  MJD 000119 ADDED POFF-NOLCHG (WORLD PR#218)
      *             NOT YET USED. MUST BE ENABLED BEFORE WORLD GOES TO A15
      *  JTG 020219 ADDED "CT" COUNTY TAX CODE KENTUCKY AND PERCENTS
      *             ADDED "CY" CITY   TAX CODE KENTUCKY AND PERCENTS
      *             ADDED LOGIC FOR INS-TAX                       WORLD #288
      *  JTG 020328 ADDED FILLER AREAS FOR FUTURE ADDITIONS TO THIS COPY
      *             MEMBER. THIS COPY MEMBER IS USED IN A NUMBER OF CALLS
      *             AND IS PASSED.                                WORLD #288
      *  JTG 020402 CHANGED POFF-INS-TAX TO POFF-INS-TAX-RB       WORLD #288
      *  JTG 020801 CHANGED FOR (MIP) MONTHLY INSURANCE PREMIUMS   REGENCY #1801
      *  JTG 021220 ADDED POFF-LCAP-BATCH-REFCD POFF-LCAP-BATCH-REFCD-LC-FG
      *             ADDED NEW CODE FILE TYPE 'BR'-BATCH PAYMENT REFERENCE CODES
      *             BATCH PAYMENT REVISIONS                        REGACC #0006
      * MJD 160223 ADDED POFF-LXR-REB-AMT FOR GA EARLY PROVISION  REGMGT PR#4915
      *             THIS IS THE AMOUNT OF THE SERVICE CHARGE THAT
      *             IS THE GEORGIA 4% CLOSING FEE EARLY PROVISION CALCULATED 
      *             IN REBATE-EP54 (LIBGB/REBATE).
      *             THIS IS INCLUDED IN THE SERVICE CHARGE 
      *             REBATE BUT NEEDS TO BE BROKEN OUT ON CERTAIN CONTRACTS. 
      *             IT WILL BE STORED HERE IF NOT = 0
      *-------------------------------------------------------------------------
       01  POFF-WORKERS.

      * INPUT FIELDS:
           03  POFF-PAYDATE                PIC 9(8) COMP-3.
           03  POFF-MAKERCD                PIC X.
           03  POFF-LONPFA-UPD-FG          PIC X.
           03  POFF-DEF-STOP               PIC X.
           03  POFF-LPTRCD                 PIC XX.

           03  POFF-INSEFF-TEST            PIC 9(8) COMP-3 VALUE 0.
           03  POFF-INSEXP-TEST            PIC 9(8) COMP-3 VALUE 0.
           03  POFF-RECAST-FG              PIC X           VALUE " ".
           03  POFF-LCAP-BATCH-REFCD       PIC X(5)        VALUE " ".
           03  POFF-LCAP-BATCH-REFCD-LC-FG PIC X           VALUE " ".
           03  FILLER                      PIC X(25).

      * OUTPUT FIELDS:
           03  POFF-REBATE          OCCURS 13
                                     PIC S9(7)V99   COMP-3.
           03  POFF-INS-TAX-RB      OCCURS 8
                                     PIC S9(7)V99   COMP-3.
           03  POFF-OTDUE            PIC S9(7)V99   COMP-3.
           03  POFF-O2DUE            PIC S9(7)V99   COMP-3.
           03  POFF-LCDUE            PIC S9(7)V99   COMP-3.
           03  POFF-INTDUE           PIC S9(7)V99   COMP-3.
           03  POFF-NETDUE           PIC S9(7)V99   COMP-3 VALUE 0.
           03  POFF-PARTIALS         PIC S9(7)V99   COMP-3.
           03  POFF-LCPAID           PIC S9(7)V99   COMP-3.
           03  POFF-ADDON-INT-EARNED PIC S9(7)V99   COMP-3.
           03  POFF-ADDON-INT-REBATE PIC S9(7)V99   COMP-3.
           03  POFF-INT-REB-TABLE.
               05  POFF-INT-REB-TBL OCCURS 9
                                     PIC S9(7)V99   COMP-3.
           03  POFF-INT-REB-TBLX    REDEFINES POFF-INT-REB-TABLE.
               05  POFF-CL-INT-REB   PIC S9(7)V99   COMP-3.
               05  POFF-AH-INT-REB   PIC S9(7)V99   COMP-3.
               05  POFF-PP-INT-REB   PIC S9(7)V99   COMP-3.
               05  POFF-O1-INT-REB   PIC S9(7)V99   COMP-3.
               05  POFF-O2-INT-REB   PIC S9(7)V99   COMP-3.
               05  POFF-O3-INT-REB   PIC S9(7)V99   COMP-3.
               05  POFF-O4-INT-REB   PIC S9(7)V99   COMP-3.
               05  POFF-O5-INT-REB   PIC S9(7)V99   COMP-3.
               05  POFF-FEES-INT-REB PIC S9(7)V99   COMP-3.
           03  POFF-NOLCHG                PIC S9(3)      COMP-3.
           03  POFF-LXM-MIP-CLPAID        PIC S9(7)V99   COMP-3.
           03  POFF-LXM-MIP-CLPDTH-DATE   PIC 9(8)       COMP-3.
           03  POFF-LXM-MIP-AHPAID        PIC S9(7)V99   COMP-3.
           03  POFF-LXM-MIP-AHPDTH-DATE   PIC 9(8)       COMP-3.
           03  POFF-LXR-REB-AMT           PIC S9(7)V99   COMP-3.
           03  FILLER                     PIC X(25).

      * WORKERS:
           03  POFF-SUB              PIC S99.
           03  POFF-ONE              PIC S9.
      * USED IN LIBLP/ADDONRB
           03  POFF-SP-DISFRMLA      PIC X.
      * USED IN LPPOF2 (PAYOFF-INT-ADDONS)
           03  POFF-RTNCD            PIC X.
      * USED IN CL/MIPPO
1801       03  POFF-CLPREM-CHG-FG    PIC X.
1801       03  POFF-AHPREM-CHG-FG    PIC X.

      * FIELDS USED FOR CALLING SUBPROGRAMS:
           03  POFF-FORM-PATH.
               05  POFF-FORM-PROG          PIC X(8).
               05  POFF-FORM-PROG-NO       PIC X.
           03  POFF-COMMON.
               05  POFF-COMMON-RTNCD       PIC X.
               05  POFF-COMMON-INTADJ      PIC S9(7)V99 COMP.
               05  POFF-COMMON-AREA.
                   07  POFF-COMMON-PAYDATE PIC 9(8)     COMP.
                   07  POFF-ORIG-INTDUE    PIC S9(7)V99 COMP.
                   07  POFF-RECAST-MESS    PIC X.
                   07  FILLER              PIC X(6).
288        03  FILLER                      PIC X(28).

       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBLP/LP01LN.CPY".
       01  SPX-REC           PIC X(SP-SIZE).

       PROCEDURE DIVISION
           USING FORM-PATHNAME POFF-WORKERS BR-REC LN-REC SPX-REC.

      **************************************************
      *      P R O G R A M   C O N T R O L             *
      **************************************************
       MAIN-PROGRAM SECTION.

           PERFORM SQL-CONNECT.

           MOVE FORM-PATHNAME TO FORM-PATH.

      D    STOP MESS.
           MOVE SPACES TO ERRCD.
 
           MOVE SPX-REC TO SP-REC.

           PERFORM GET-GPENV.

           MOVE EXT-FILPATH-BASE TO FILPATH-BASE.
           MOVE BR-MACHINE       TO FILPATH-MACHINE.
           MOVE BR-DATA-PATH     TO FILPATH-DATA.

           MOVE "B" TO LTI-PATH-OVERRIDE
                       LTP-PATH-OVERRIDE
                       GB-PATH-OVERRIDE.

      * NEED FOR LCAS ROUTINES (REPLACES GLOBRD)
           PERFORM OPEN-GB-FILE.
           MOVE GB-PATH-OWNBR TO GB-BRNO.
           MOVE 1             TO GB-NO.
           PERFORM READ-GB-FILE.
           PERFORM CLOSE-GB-FILE.
           IF IO-BAD
              GO TO END-ROUTINE.

      * MUST LEAVE SP1-FILE OPEN BECAUSE ADDONSPR READS IT
           PERFORM OPEN-SP1-FILE.

           PERFORM PAYOFF-LOAN-ROUTINE.

           GO TO END-ROUTINE.

      ***************************************************************
       PAYOFF-LOAN-ROUTINE SECTION.
      ***************************************************************
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
           MOVE 0 TO POFF-NOLCHG.
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

      * LN-OT2BAL IS INCLUDED IN LN-CURBAL.

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
      * COMPUTE LATE CHARGES IF PRECOMP AND NOT EXPIRING:
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

      * TEST FOR GEORGIA MINIMUM REBATE ON INTEREST:

           IF SP-RBMIN(7) = 999.01
              IF POFF-REBATE(5) = 0
                 IF POFF-REBATE(4) < 1.00
                    ADD POFF-REBATE(4) TO POFF-NETDUE
                    MOVE 0 TO POFF-REBATE(4).

      * COMPUTE MAINT FEE REBATE IF (PC):

      * ADDED LOGIC FOR MEXICO: IF MFFRMLA "5" AND RBFRMLA "0G" THEN USE
      *       MFFEE AMOUNT ON PAGE 6 OF LOAN MAINT.   SOME LOANS COULD
      *       CAUSE MONTHLY MAINT FEE TO BE 0 (ZERO) DUE TO TRUNCATION
      *       OF SIG DIGITS (I.E. 300.00 BECOMES 00.00).

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

      * CLEAR ITRM-TODAY SO INSURANCE INEFFECT WON'T BE CALCULATED
      * IN LPITRM.

           MOVE 0 TO ITRM-TODAY.

      * IF NO INSURANCE, SKIP
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
                    MOVE INS-TAX-RB  TO POFF-INS-TAX-RB(1).

      * COMPUTE ACCIDENT HEALTH A&H REBATE :

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
                    MOVE REB-REBATE TO POFF-REBATE(2)
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

      * COMPUTE PERSONAL PROPERTY REBATE:

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
      * SETUP FOR INSURANCE ADDON REFUND
      ****************************************
       PAYOFF-SET-ADDON-INS SECTION.
           MOVE LN-INSEFF-DATE(POFF-SUB) TO REB-LN-LOANDATE
                                            REB-LN-PURDATE
                                            REB-LN-INTDATE
                                            REB-LN-INSEFF
                                            NDTE-DATE.
           MOVE LN-INSEXP-DATE(POFF-SUB) TO REB-LN-INSEXP.

           MOVE 1               TO NDTE-HOLD.
           MOVE LN-UNITPER-CD   TO DATER-UNITPER-CD.
           MOVE LN-UNITPER-FREQ TO DATER-UNITPER-FREQ.
           PERFORM INCREMENT-UNITPER.
           MOVE NDTE-DATE            TO REB-LN-ORIG-1STPYDATE.
           MOVE REB-ORGTERM          TO REB-LN-ORGTERM.
           MOVE LN-REGPYAMT          TO REB-LN-REGPYAMT.
           MOVE LN-LASTPYAMT         TO REB-LN-LASTPYAMT.
           MOVE LN-APRATE            TO REB-LN-APRATE.
           MOVE LN-SMPRATE           TO REB-LN-SMPRATE.
           MOVE 0                    TO REB-LN-TOTNODEF.
           MOVE LN-INSCOVR(POFF-SUB) TO REB-LN-INSCOVR.
           MOVE "N"                  TO REB-LN-SALEFIN.
           MOVE "Y"                  TO REB-ADDON-FG.

      ****************************************
      *    SET REB-SUB FOR CREDIT LIFE REBATE
      ****************************************
       PAYOFF-SET-CL-REBSUB SECTION.
      * TEST FOR SINGLE OR JOINT LIFE:
           IF LN-INSURED = "J"
              MOVE 2 TO REB-SUB
            ELSE
              MOVE 1 TO REB-SUB.
      * TEST FOR LEVEL LIFE:
           IF LN-INSTYPES-LL
              ADD 2 TO REB-SUB.

      **************************************************
      *   GET ALL LATE CHARGES DUE THRU PAYOFF DATE:
      **************************************************
       PAYOFF-SET-LATE-CHARGES SECTION.

           MOVE POFF-PAYDATE                TO LCAP-PAYDATE.
           MOVE LN-1STPYDATE                TO LCAP-1STPYDATE.
           MOVE "AL"                        TO LCAP-LPTRCD.
           MOVE POFF-LPTRCD                 TO LCAP-POFF-LPTRCD.
           MOVE POFF-LCAP-BATCH-REFCD       TO LCAP-BATCH-REFCD.
           MOVE POFF-LCAP-BATCH-REFCD-LC-FG TO LCAP-BATCH-REFCD-LC-FG.
           MOVE 0                           TO LCAP-LPAPLC
                                               LCAP-LPAPINT.
           PERFORM LATE-CHARGE-APPLY.
           MOVE LCAP-PARTIALS               TO POFF-PARTIALS.
           MOVE LCAP-LCPAID                 TO POFF-LCPAID.
           MOVE LCAP-NOLCHG                 TO POFF-NOLCHG.
           ADD LN-LCBAL LCAP-OWE GIVING POFF-LCDUE.
           ADD LCAP-OWE                     TO POFF-NETDUE.

      ****************************************************
      *    GET ALL INTEREST CHARGES DUE THRU PAYOFF DATE:
      *    NOTE:
      *          NEW FIELDS SHOULD HAVE BEEN CREATED FOR
      *          THE LINK BETWEEN CL/LFPPO.C AND LPPOFF
      *          BUT THIS WOULD FORCE A RECOMPILE, SO
      *          POFF-O5-INT-REB WAS USED. JTG 010925
      ****************************************************
       PAYOFF-SET-INT-CHARGES SECTION.
           IF SP-CAL-RATETYPE(1) = "Z"
              MOVE "CL/LFPPO" TO FORM-NAM
              CALL FORM-PROGX USING FORM-PATH
                                    POFF-WORKERS
                                    LN-REC
                                    SP-REC
              CANCEL FORM-PROGX
              MOVE POFF-INTDUE     TO INDU-INTEREST
              MOVE POFF-O5-INT-REB TO INDU-PENALTY
              MOVE 0               TO POFF-O5-INT-REB
           ELSE
              MOVE POFF-PAYDATE TO RATE-DATE IBPC-DATE
              MOVE POFF-MAKERCD TO RATE-MAKERCD
              MOVE POFF-LPTRCD  TO RATE-LPTRCD
              PERFORM SET-PY-RATE.

      *    TEST FOR CALIFORNIA RECAST:
      *
      *    IF SP-RBRECAST(7) = 2
      *       IF LN-OVRDRATE NOT = 999.999
      *          MOVE LN-ORIG-1STPYDATE TO NDTE-DATE
      *          MOVE 2 TO NDTE-HOLD
      *          MOVE LN-UNITPER-CD   TO DATER-UNITPER-CD
      *          MOVE LN-UNITPER-FREQ TO DATER-UNITPER-FREQ
      *          PERFORM INCREMENT-UNITPER
      *          IF POFF-PAYDATE NOT > NDTE-DATE
      *             MOVE POFF-PAYDATE TO POFF-COMMON-PAYDATE
      *             ADD LN-INTBAL INDU-INTEREST
      *                           GIVING POFF-ORIG-INTDUE
      *             MOVE "RCASTC" TO FORM-NAM
      *             CALL FORM-PROGX USING FORM-PATH
      *                                   POFF-COMMON
      *                                   LN-REC
      *                                   SP-REC
      *             CANCEL FORM-PROGX
      *             IF POFF-COMMON-RTNCD = " "
      *                SUBTRACT POFF-COMMON-INTADJ
      *                                  FROM INDU-INTEREST.

      * CALIFORNIA "CA" STATE LAW  1/15/2013 MULLEN & COSMO
      * WHEN AN ACCOUNT IS PAID IN FULL (REGARDLESS OF THE PAYOFF METHOD)
      * ON OR BEFORE THE 3RD INSTALLMENT DUE DATE (CURRENT 1ST PAY DUE DATE
      * PLUS 2 MONTHS) BASED ON A 360 DAY YEAR TYPE, AS FOLLOWS:

           IF SP-RBSPOPT1(7) = 27
              PERFORM CALIFORNIA-SPECIAL-OPTION.

           ADD LN-INTBAL INDU-INTEREST GIVING POFF-INTDUE.
           ADD INDU-INTEREST TO POFF-NETDUE.

      **********************************
      *    GET THE REBATE:
      **********************************
       PAYOFF-GET-REBATE SECTION.
           MOVE POFF-PAYDATE TO REB-PAYDATE.
           MOVE POFF-LPTRCD TO REB-LPTRCD.

           MOVE 0 TO REB-SUB2.
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

           IF REB-SUB = 8
              MOVE REB-EP54-AMT TO POFF-LXR-REB-AMT
           END-IF.
           
           SUBTRACT REB-REBATE FROM POFF-NETDUE.

      **********************************
      *    CALCULATE SERCHG REBATE:
      **********************************
       PAYOFF-SERCHG-REBATE SECTION.
      *---------------------------------------------------
      *    ALLOW REFUND OF SERVICE CHARGE ON IB LOAN TYPES
      *---------------------------------------------------
      *    IF (IBPC-FG NOT = "P")
            IF ( LN-SERCHG = 0             ) OR
               ( LN-SC-REBATE = "R" OR "Y" )
               GO TO PAYOFF-SERCHG-REBATE-EXIT.

           MOVE 8 TO REB-SUB.
           MOVE LN-SERCHG TO REB-TOTCHG.
           MOVE LN-ORGTERM TO REB-ORGTERM.
           IF LN-INT-REBATE = " " OR "C"
              MOVE POFF-REBATE(4) TO REB-STATEWK
           ELSE
              MOVE LN-ANTICADJ(1) TO REB-STATEWK.
           PERFORM PAYOFF-GET-REBATE.
           MOVE REB-REBATE TO POFF-REBATE(5)
0408                          REB-LN-SC-REBATE.

           MOVE REB-EP54-AMT TO POFF-LXR-REB-AMT.

       PAYOFF-SERCHG-REBATE-EXIT.
           EXIT.

      * CALIFORNIA "CA" STATE LAW  1/15/2013 MULLEN & COSMO
      * WHEN AN ACCOUNT IS PAID IN FULL (REGARDLESS OF THE PAYOFF METHOD)
      * ON OR BEFORE THE 3RD INSTALLMENT DUE DATE (CURRENT 1ST PAY DUE DATE
      * PLUS 2 MONTHS) BASED ON A 360 DAY YEAR TYPE, AS FOLLOWS:

      ********************************************
       CALIFORNIA-SPECIAL-OPTION SECTION.
      ********************************************

           MOVE LN-ORIG-1STPYDATE TO NDTE-DATE.
           MOVE 2 TO NDTE-HOLD.
           MOVE LN-UNITPER-CD TO DATER-UNITPER-CD.
           MOVE LN-UNITPER-FREQ TO DATER-UNITPER-FREQ.
           PERFORM INCREMENT-UNITPER.
      * IF NOT WITHIN 3RD INSTALLMENT, EXIT OUT
           IF POFF-PAYDATE > NDTE-DATE
              GO TO EXIT-CALIFORNIA-SPECIAL-OPTION.

      * WITHIN 3RD INSTALLMENT, NOW CALCULATE ELAPSED DAYS SINCE LAST
      * PAYMENT/LOANDATE (IF NO PAYMENTS)

           MOVE 360 TO ELAPSED-YRTYPE.
           MOVE LN-DATE-PAID-LAST TO NUM-DATE.
           IF LN-DATE-PAID-LAST = 0
              MOVE LN-LOANDATE TO NUM-DATE.
           MOVE POFF-PAYDATE TO SYS-DATE.
           PERFORM TIMALL.

           IF LN-CURBAL > 0
      * 1 TO 225.00
              IF LN-CURBAL < 225.01
                 COMPUTE INDU-INTEREST ROUNDED =
                  ( LN-CURBAL * ELAPSED-DAYS ) / 1200
              ELSE
      * 225.01 TO 900.00
              IF LN-CURBAL < 900.01
                 COMPUTE INDU-INTEREST ROUNDED =
                  ( (56.25 + LN-CURBAL) * ELAPSED-DAYS ) / 1500
              ELSE
      * 900.01 TO 1650.00
              IF LN-CURBAL < 1650.01
                 COMPUTE INDU-INTEREST ROUNDED =
                  ( (375.00 + LN-CURBAL) * ELAPSED-DAYS ) / 2000
              ELSE
      * 1650.01 TO 2499.99
                 COMPUTE INDU-INTEREST ROUNDED =
                  ( (1387.50 + LN-CURBAL) * ELAPSED-DAYS ) / 3000.

       EXIT-CALIFORNIA-SPECIAL-OPTION.
           EXIT.


      *-----------------------------------------------------------------------
       COPY "LIBLP/MIPPO.CPY".
      *-----------------------------------------------------------------------

       COPY "LIBGB/DATER.CPY".
       COPY "LIBLP/SETCTBL.CPY".
       COPY "LIBLP/LPIBPC.CPY".
       COPY "LIBLP/LPITRM.CPY".
       COPY "LIBLP/LPPOF2.CPY".
       COPY "LIBLP/LPXDTE.CPY".
       COPY "LIBLP/ADDONSPR.CPY".
       COPY "LIBLP/LPLCAP.CPY".
       COPY "LIBLP/LPLCAS.CPY".
       COPY "LIBLP/LPLCHG.CPY".
       COPY "LIBLP/LPSCHD.CPY".
       COPY "LIBLP/LPRATE.CPY".
       COPY "LIBLP/LPMDTE.CPY".
       COPY "LIBLP/LPUPWK.CPY".
       COPY "LIBLP/LPAMTS.CPY".
       COPY "LIBLP/LPAPRZ.CPY".
       COPY "LIBLP/LPUPER.CPY".
       COPY "LIBGB/GPENV.CPY".

      ********************************************
       MISC-ROUTINES SECTION.
      ********************************************
       REBATE.
           MOVE "CL/REBATE" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH REBATE-LINK BR-REC
                                     LN-REC SP-REC.

      /*******************************************
      *     I N P U T  /  O U T P U T            *
      ********************************************
       IO-ROUTINES SECTION.
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/DECLARE_SQL.CPY".
           PERFORM CLOSE-SP1-FILE.
           PERFORM CLOSE-LTP1-FILE.
           PERFORM CLOSE-LTI1-FILE.
           PERFORM CLOSE-GB-FILE.
       COPY "LIBLP/LPSPGS_SQL.CPY".
       COPY "LIBLP/LPLTPGS_SQL.CPY".
       COPY "LIBLP/LPLTIGS_SQL.CPY".
       COPY "LIBGB/GBGBGS_SQL.CPY".
       COPY "LIBLP/LPLTI1RN.CPY".
       COPY "LIBLP/LPLTP1RN.CPY".
       COPY "LIBGB/GBGBRN.CPY".
       COPY "LIBLP/LPSP1RN.CPY".
       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO ERRCD.
       END-ROUTINE.
           PERFORM CLOSE-FILES.
       EXIT-PROG.
       STOP-RUN.
           EXIT PROGRAM.
