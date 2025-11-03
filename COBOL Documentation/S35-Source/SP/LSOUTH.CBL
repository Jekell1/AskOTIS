       IDENTIFICATION DIVISION.
       PROGRAM-ID.      LSOUTH.
       DATE-WRITTEN.    022197.
      *****************************************************************
      *    DESCRIPTION:  MONTHLY CL/AH/PP/O1/O2/O3   INS TAPE
      *
      * CURRENTLY BEING USED BY SUN, AMERICAN THRIFT, LOANS INC.
      *                         SMITHFIELD
      *
      * REPORTING TO LIFE OF THE SOUTH.
      *   AND WORLD STARTING 9/2004
      *
      * DEBRA MILLER 800-888-2738  X8446
      *
      *     CREATE /USR/DATA/R1/ED/ISSUE AND /USR/DATA/R1/ED/CANCEL
      * WORLD: (BECAUSE THEY HAVE TO RUN VOYAGR TOO)
      *     CREATE /USR/DATA/R1/ED/ISS.LO1.YYMM AND
      *            /USR/DATA/R1/ED/CAN.LO1.YYMM
      *     CREATE /USR/DATA/R1/ED/ISS.LO2.YYMM AND
      *            /USR/DATA/R1/ED/CAN.LO2.YYMM
      *
      * READS EACH BRANCHES TRFILE
      *
      *
      * REV:
      *  MJD 010798 ADDED GROUP LOGIC
      *  BAH 030399 ADDED O3 INSURANCE
      *  BAH 990721 ADDED SP-FILE BECAUSE OF A CHANGE TO LPITRM
      *  BAH 990928 FILL IN PFTI-APR, LAURA
      *  BAH 011012 ADD CR,LF TO EACH RECORD FOR WDT ONLY ! PR#1621
      *  020529 BAH WAS NOT TESTING OWNING BRANCH CORRECTLY, THE BRANCH
      *             THAT POSTED THE PAYOFF ALSO HAD THAT ACCOUNT NUMBER
      *             AND IT WAS TESTING LN-OWNBR = TR1-BRNO, YES, THEY
      *             MATCHED. THAT WASNT CORRECT TEST....REGWAN PR# 1816
      *  BAH 040123 WORLD "KY" INSURANCE TAX WAS PART OF THE REFUND
      *             BEING REPORTED, NEEDED TO BE SUBTRACTED OUT !!
      *                            ((TP-TRAMT - TP-INS-TAX-RB) * -1)
      *  BAH 040213 WORK-CANCEL-FG-O3 WAS NOT BEING TESTED IN THE KEY-DETAIL
      *             SECTION, SO IF THIS WAS ONLY AMOUNT IN WORK RECORD,
      *             IT WAS SKIPPED.  SMITHFIELD/LAURA
      *  BAH 040826 ALLOW WORLD TO USE, PR# 359
      *  BAH 040910 ADDED TAX SURCHARGE FOR PROPERTY INSURANCE, WORLD
      *  BLV 041124 FIXED EO-EXEC, MOVE ENDING MM & YY TO MM, YY,
      *             WOULD NOT RUN IN BATCH
      *  BAH 041217 TAX CITY AND COUNTY WERE GETTING WIPED OUT AFTER BEING
      *             SET, CREATED A HOLD FIELD
      *  BLF 050106 REMOVE TEMPORARY CHECK ON AD&D FEE CODE; IN NOV WE
      *             ONLY ADDED AD&D IF LOAN BOOKED IN NOV (SWITCHED TO
      *             LOTS 11/1, SO REFUNDS WERE ASSURANCT)
      *  BAH 050323 A "RP" REBATE HAD "FIRCL" IN THE REFNO SO IT WAS TESTED
      *             AS AN ADDON OF CREDIT LIFE, BUT HAD NO INSURANCE FOR
      *             CREDIT LIFE, SO IT WAS SKIPPED...NEED TO TEST THE ENTIRE
      *             REFNO FIELD FOR ADDONS...WORLD, JIM
      *  BAH 061219 ADDED CHECK FOR GP-I-AM-NEWSOUTH AND LN-ORGST = "FL" TO
      *             MOVE FEEAMT AND REBATE DIRECTLY AS TAX AMOUNT
      *             HURRICANE RELIEF LENDMARK PR# 2875
      *  BAH 070124 FORGOT TO DO "T1" AND "T2" ON THE ABOVE CHANGE PR# 2875
      *  BAH 100401 CHANGED SURCHARGE FROM 15 TO 18% FOR WORLD PR# 673
      *  BAH 100412 WORLD KY CITY AND COUNTY TAX CODES INCREASED TO 4
      *             CHARACTERS, ADDED CONVERSION TABLE, ADDED TAX CODES
      *             TO CANCEL RECORDS, WORLD PR# 681
      *  BAH 111101 ADD CR/LF FOR GP-I-AM-CASHCO PR# 3896
      *  BAH 111122 ADD PFTC-CANC-SSN FOR GP-I-AM-CASHCO ONLY PR# 3917
      *  BAH 120329 ADDED MORE FIELDS TO PFTC RECORD FOR GP-I-AM-CASHCO PR#4002
      *  BAH 120423 IF JOINT INSURED, JOINT SSNO WAS PUT IN PFTI-PRI-SSN PL#773
      *  BAH 150330 A&H NEEDS TO TEST SP-INSMAXBENIF, AMTHRIFT PR# 4722
      *  BAH 170421 "MS" LOANS WERE CODED AS SINGLE BY MISTAKE FOR YEARS,
      *             SPR'S HAVE BEEN FIXED BUT NOW NEED TO TEST LOANDATE
      *             AND FORCE CANCELS TO BE DUAL, WORLD PR#1240
      *  BAH 170426 DONT DO AMTHRIFT 4722 FOR WORLD! (A&H NEEDS TO TEST
      *             SP-INSMAXBENIF) WORLD PR#1240A
      *  20170928 BAH ACTIVATE 8 DIGIT DATES, PD0006
      * BAH 181214 GLOBAL LNFILE
      * BAH 20190930 REMOVED ACCESS-CALL LNFILE, TRP, TRL, TRS
      * BAH 20200827 REMOVED RESET ON BR1 
      * BLM 20210810 REMOVE TP-CODE, TP-POOLID NO LONGER IN KEY
      * MB  20231005 PERFORMANCE TUNING - ADDED STORED PROCEDURES USING
      *                 BRANCHLIST PROCESSING.
      *              ADDED STATE PARAMETER ARRAY PROCESSING
      *              EMBEDDED SQL QUERIES
      *              LN-REC HAS BEEN ADDED TO THE WK FILE, ALONG WITH HARDCODED 
      *              SIZE. IF THE SIZE OF LNFILE TABLE INCREASE, THE 78 LEVEL 
      *              ON WK-FILE WILL ALSO NEED TO BE INCREASED. ***
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "LIBGB/GBFSPR.CPY".

           SELECT PFTI-FILE ASSIGN TO PFTI-PATH
                           ORGANIZATION RELATIVE
                           ACCESS DYNAMIC
                           LOCK MODE AUTOMATIC WITH LOCK ON RECORD
                           RELATIVE KEY PFTI-KEY
                           FILE STATUS FILE-STAT.
           SELECT PFTC-FILE ASSIGN TO PFTC-PATH
                           ORGANIZATION RELATIVE
                           ACCESS DYNAMIC
                           LOCK MODE AUTOMATIC WITH LOCK ON RECORD
                           RELATIVE KEY PFTC-KEY
                           FILE STATUS FILE-STAT.
      * VALID LOANS FILE
       COPY "LIBGB/GBFSWKI.CPY".

       I-O-CONTROL.
       DATA DIVISION.
       FILE SECTION.

       COPY "LIBGB/GBFDPR.CPY".

      **************************************************
      *  XYCOR VOYAGER FILE LAYOUT - NEW ISSUES
      * RSZ 812
      **************************************************
       FD  PFTI-FILE
           LABEL RECORDS ARE STANDARD.

       01  PFTI-REC.
           03  PFTI-BASE.
               05  PFTI-TYPE                PIC X.
                   88  PFTI-ISSUE           VALUE "I".
                   88  PFTI-CANCEL          VALUE "C".
                   88  PFTI-TRANSFER        VALUE "X".
                   88  PFTI-INFO-CHANGE     VALUE "S".
                   88  PFTI-ADDRESS         VALUE "H".
      * 2 - 7
               05  PFTI-FRE                 PIC X(6).
      * 8 - 9
               05  PFTI-CLASS               PIC 9(2).
      * 10 - 17
               05  PFTI-PRODUCER            PIC 9(8).
               05  PFTI-CERT-NBR            PIC 9(10).
               05  PFTI-INSUROR             PIC 99.
           03  PFTI-ISSUE-INFO.
               05  PFTI-ACCOUNT-LOAN.
                   10  PFTI-ACCOUNT         PIC X(12).
                   10  PFTI-LOAN            PIC X(6).
               05  PFTI-EFF-DATE            PIC 9(6).
               05  PFTI-PRI-NAME.
                   10  PFTI-PRI-LNAME       PIC X(15).
                   10  PFTI-PRI-FNAME       PIC X(10).
                   10  PFTI-PRI-INIT        PIC X.
               05  PFTI-PRI-AGE             PIC 99.
               05  PFTI-PRI-SEX             PIC X.
      * 83
               05  PFTI-PRI-SSN             PIC 9(9).
               05  PFTI-SEC-NAME.
      * 92
                   10  PFTI-SEC-LNAME       PIC X(15).
                   10  PFTI-SEC-FNAME       PIC X(10).
                   10  PFTI-SEC-INIT        PIC X.
               05  PFTI-SEC-AGE             PIC 99.
               05  PFTI-SEC-SEX             PIC X.
               05  PFTI-SEC-SSN             PIC 9(9).
      * 130
               05  PFTI-MODE.
                   10  PFTI-MODE-1          PIC X.
                   10  PFTI-MODE-2          PIC X.
               05  PFTI-SKIP                PIC XX.
               05  PFTI-FIRST-PAYT          PIC X(6).
               05  PFTI-CERT-TYPE           PIC XX.
               05  PFTI-POLICY-FORM         PIC X(8).
      * 150
               05  PFTI-APR                 PIC S99V999.
      * 155
               05  PFTI-LOAN-TERM           PIC X(3).
               05  PFTI-LOAN-EXPIRY         PIC X(6).
               05  PFTI-PRI-BIRTH           PIC 9(6).
      * 170
               05  PFTI-SEC-BIRTH           PIC 9(6).
               05  PFTI-ISS-REMIT-NBR       PIC X(9).
      * 185
               05  PFTI-RES-STATE           PIC XX.
               05  PFTI-TYPE-INDEBTEDNESS   PIC X.
               05  FILLER                  PIC X(47).
           03  PFTI-ISSUE-COVERAGE.
               05  PFTI-ISS-RECS    OCCURS 4.
      * 235  264  293  322
                   10  PFTI-ISS-COV-CD      PIC X(4).
      * 239  268  297  326
                   10  PFTI-ISS-BEN         PIC S9(7)V99.
      * 248  277  306  335
                   10  PFTI-ISS-PRM         PIC S9(5)V99.
      * 255  284  313  342
                   10  PFTI-ISS-TERM        PIC 9(3).
      * 258  287  316  345
                   10  PFTI-ISS-EXP         PIC X(6).

               05  PFTI-ISS-TAX    OCCURS 4.
      * 351  365  379  393
                   10  PFTI-ISS-TAX-CODE    PIC XX.
      * 353  367  381  395
                   10  PFTI-ISS-TAX-AMT     PIC S9(4)V99.
      * 359  373  387  401
                   10  PFTI-ISS-SURCHG      PIC S9(4)V99.
      * 407
           03  PFTI-TAX-CITY-CODE           PIC X(4).
      * 411
           03  PFTI-TAX-COUNTY-CODE         PIC X(4).
      * 415
           03  PFTI-PRI-ADR1               PIC X(32).
      * 447
           03  PFTI-PRI-ADR2               PIC X(32).
      * 479
           03  PFTI-PRI-CITY               PIC X(20).
      * 499
           03  PFTI-PRI-STATE              PIC X(2).
      * 501
           03  PFTI-PRI-ZIP                PIC X(9).
      * 510 - 810
           03  PFTI-FILLER                 PIC X(301).
      * ONLY FILLED IN FOR WDT
           03  PFTI-CR                     PIC X.
      * 812
           03  PFTI-LF                     PIC X.


      **************************************************
      *  XYCOR VOYAGER FILE LAYOUT - REFUNDS/CANCELS
      **************************************************
       FD  PFTC-FILE
           LABEL RECORDS ARE STANDARD.

       01  PFTC-REC.
           03  PFTC-BASE.
               05  PFTC-TYPE                PIC X.
                   88  PFTC-ISSUE           VALUE "I".
                   88  PFTC-CANCEL          VALUE "C".
                   88  PFTC-TRANSFER        VALUE "X".
                   88  PFTC-INFO-CHANGE     VALUE "S".
               05  PFTC-FRE                 PIC X(6).
               05  PFTC-CLASS               PIC 9(2).
               05  PFTC-PRODUCER            PIC 9(8).
               05  PFTC-CERT-NBR            PIC 9(10).
               05  PFTC-INSUROR             PIC 99.
           03  PFTC-CANCEL-INFO.
               05  PFTC-REASON              PIC X(3).
      * 33 - 38
               05  PFTC-CANC-EFF-DATE       PIC X(6).
      * 39 - 54
               05  PFTC-CANC-LNAME          PIC X(16).
      * 55 - 70
               05  PFTC-CANC-FNAME          PIC X(16).
      * 71 - 234
      * 71 - 82
               05  FILLER                   PIC X(12).
      * 83 - 91 
               05  PFTC-CANC-SSN            PIC 9(9).
      * 92 - 96
               05  PFTC-CANC-APR            PIC S99V999.
      * 97 - 103 
               05  PFTC-CANC-AMTFIN         PIC S9(5)V99.
      * 104- 106
               05  PFTC-CANC-ORGTERM        PIC 9(3).
      * 107- 113
               05  PFTC-CANC-REGPYAMT       PIC S9(5)V99.
      * 114- 119
               05  PFTC-CANC-MDTE           PIC 9(6).
      * 120- 125
               05  PFTC-CANC-BDATE          PIC 9(6).
      * 126 - 234
               05  PFTC-CANC-FILLER         PIC X(109).
      * 235
           03  PFTC-CANCEL-COVERAGE.
               05  PFTC-CANC-RECS  OCCURS 4.
                   10  PFTC-CANC-COV-CD     PIC X(4).
                   10  PFTC-CANC-DATE       PIC 9(6).
                   10  PFTC-CANC-REF        PIC S9(5)V99.
                   10  PFTC-CANC-COVG       PIC S9(5)V99.
      * PFTC-CANC-TERM (ONLY FILLED IN FOR GP-I-AM-CASHCO)
                   10  PFTC-CANC-TERM       PIC 9(3).
                   10  FILLER               PIC X(2).
      * 351  365  379  393
               05  PFTC-CANC-TAX-INFO OCCURS 4.
                   10  PFTC-CANC-TAX-CODE  PIC XX.
                   10  PFTC-CANC-TAX       PIC S9(4)V99.
                   10  PFTC-CANC-SURCHG    PIC S9(4)V99.
      * 407
           03  PFTC-FILLER                 PIC X(106).
      * 513
           03  PFTC-TAX-CITY-CODE          PIC X(4).
      * 517
           03  PFTC-TAX-COUNTY-CODE         PIC X(4).
      * 521
           03  PFTC-FILLER2                PIC X(290).
      * ONLY FILLED IN FOR WDT
           03  PFTC-CR                     PIC X.
      * 812
           03  PFTC-LF                     PIC X.

      *************************************
      *   WORK FILE FOR VALID LOANS
      *************************************

       78  WK-LN-SIZE                    VALUE 2800. 

       FD  WK-FILE
           LABEL RECORDS ARE STANDARD.
       01  WK-REC.
           03  WK-KEY.
               05  WORK-OWNBR       PIC 9999.
               05  WORK-ACCTNO      PIC 9(8).
           03  WORK-NEW-CL          PIC S9(5)V99 COMP-3.
           03  WORK-NEW-CL-TAX      PIC S9(5)V99 COMP-3.
           03  WORK-CAN-CL          PIC S9(5)V99 COMP-3.
           03  WORK-CAN-CL-TAX      PIC S9(5)V99 COMP-3.
           03  WORK-CAN-CL-DATE     PIC 9(8) COMP-3.
           03  WORK-NEW-AH          PIC S9(5)V99 COMP-3.
           03  WORK-NEW-AH-TAX      PIC S9(5)V99 COMP-3.
           03  WORK-CAN-AH          PIC S9(5)V99 COMP-3.
           03  WORK-CAN-AH-TAX      PIC S9(5)V99 COMP-3.
           03  WORK-CAN-AH-DATE     PIC 9(8) COMP-3.
           03  WORK-NEW-PP          PIC S9(5)V99 COMP-3.
           03  WORK-NEW-PP-TAX      PIC S9(5)V99 COMP-3.
           03  WORK-CAN-PP          PIC S9(5)V99 COMP-3.
           03  WORK-CAN-PP-TAX      PIC S9(5)V99 COMP-3.
           03  WORK-CAN-PP-DATE     PIC 9(8) COMP-3.
           03  WORK-NEW-O1          PIC S9(5)V99 COMP-3.
           03  WORK-NEW-O1-TAX      PIC S9(5)V99 COMP-3.
           03  WORK-CAN-O1          PIC S9(5)V99 COMP-3.
           03  WORK-CAN-O1-TAX      PIC S9(5)V99 COMP-3.
           03  WORK-CAN-O1-DATE     PIC 9(8) COMP-3.
           03  WORK-NEW-O2          PIC S9(5)V99 COMP-3.
           03  WORK-NEW-O2-TAX      PIC S9(5)V99 COMP-3.
           03  WORK-CAN-O2          PIC S9(5)V99 COMP-3.
           03  WORK-CAN-O2-TAX      PIC S9(5)V99 COMP-3.
           03  WORK-CAN-O2-DATE     PIC 9(8) COMP-3.
           03  WORK-NEW-O3          PIC S9(5)V99 COMP-3.
           03  WORK-NEW-O3-TAX      PIC S9(5)V99 COMP-3.
           03  WORK-CAN-O3          PIC S9(5)V99 COMP-3.
           03  WORK-CAN-O3-TAX      PIC S9(5)V99 COMP-3.
           03  WORK-CAN-O3-DATE     PIC 9(8) COMP-3.
           03  WORK-NEW-AD-D        PIC S9(5)V99 COMP-3.
           03  WORK-NEW-NON-FILE    PIC S9(5)V99 COMP-3.
           03  WORK-ADDON-FG-CL     PIC X.
           03  WORK-REFUND-FG-CL    PIC X.
           03  WORK-CANCEL-FG-CL    PIC X.
           03  WORK-ADDON-FG-AH     PIC X.
           03  WORK-REFUND-FG-AH    PIC X.
           03  WORK-CANCEL-FG-AH    PIC X.
           03  WORK-ADDON-FG-PP     PIC X.
           03  WORK-REFUND-FG-PP    PIC X.
           03  WORK-CANCEL-FG-PP    PIC X.
           03  WORK-ADDON-FG-O1     PIC X.
           03  WORK-REFUND-FG-O1    PIC X.
           03  WORK-CANCEL-FG-O1    PIC X.
           03  WORK-ADDON-FG-O2     PIC X.
           03  WORK-REFUND-FG-O2    PIC X.
           03  WORK-CANCEL-FG-O2    PIC X.
           03  WORK-ADDON-FG-O3     PIC X.
           03  WORK-REFUND-FG-O3    PIC X.
           03  WORK-CANCEL-FG-O3    PIC X.
           03  WORK-TAX-CITY-CODE   PIC X(4).
           03  WORK-TAX-COUNTY-CODE PIC X(4).
           03  WORK-LN-REC          PIC X(WK-LN-SIZE).

       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)SP/LSOUTH.CBL S35 02/22/24-08:17:55 >".
      ****************************
      *     F I L E   I F N      *
      ****************************
       COPY "LIBLP/LP01TRL.CPY".
       COPY "LIBLP/LP01TRL_SQL.CPY".
       COPY "LIBLP/LPWSTRL.CPY".
       COPY "LIBLP/LP01TRP.CPY".
       COPY "LIBLP/LP01TRP_SQL.CPY".
       COPY "LIBLP/LPWSTRP.CPY".
       COPY "LIBLP/LP01TRS.CPY".
       COPY "LIBLP/LP01TRS_SQL.CPY".
       COPY "LIBLP/LPWSTRS.CPY".
       COPY "LIBLP/LP01BW.CPY".
       COPY "LIBLP/LP01BW_SQL.CPY".
       COPY "LIBLP/LPWSBW.CPY".
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBGB/GB01GR.CPY".
       COPY "LIBGB/GB01GR_SQL.CPY".
       COPY "LIBGB/GBWSGR.CPY".
       COPY "LIBLP/LP01SP.CPY".
       COPY "LIBLP/LP01SPA.CPY".
       COPY "LIBLP/LP01SPB.CPY".
       COPY "LIBLP/LP01SPC.CPY".
       COPY "LIBLP/LP01SP_SQL.CPY".
       COPY "LIBLP/LPWSSP.CPY".
       COPY "LIBLP/LP01LN.CPY".
       COPY "LIBLP/LP01LN_SQL.CPY".
       COPY "LIBLP/LPWSLN.CPY".

       01  PR-IFN               PIC X(2)     VALUE "PR".
       01  PFTI-IFN             PIC X(3)     VALUE "PFT".
       01  PFTC-IFN             PIC X(3)     VALUE "PFT".
       01  WK-IFN               PIC X(2)     VALUE "WK".

       01  PFTI-PATH.
           03  PFTI-PATH-BASE      PIC X(9).
           03  FILLER              PIC X VALUE "/".
           03  PFTI-PATH-MACHINE   PIC X(2).
           03  FILLER              PIC X(9) VALUE "/ED/ISSUE".
           03  FILLER              PIC X(10) VALUE SPACES.
       01  PFTC-PATH.
           03  PFTC-PATH-BASE      PIC X(9).
           03  FILLER              PIC X VALUE "/".
           03  PFTC-PATH-MACHINE   PIC X(2).
           03  FILLER              PIC X(10) VALUE "/ED/CANCEL".
           03  FILLER              PIC X(10) VALUE SPACES.

       01  PFTI-PATH-WORLD.
           03  PFTI-PATH-WORLD-BASE      PIC X(9).
           03  FILLER                    PIC X VALUE "/".
           03  PFTI-PATH-WORLD-MACHINE   PIC X(2).
           03  FILLER                    PIC X(10) VALUE "/ED/ISS.LO".
           03  PFTI-MACHINE              PIC 9.
           03  FILLER                    PIC X VALUE ".".
           03  PFTI-YR                   PIC 99.
           03  PFTI-MO                   PIC 99.
       01  PFTC-PATH-WORLD.
           03  PFTC-PATH-WORLD-BASE      PIC X(9).
           03  FILLER                    PIC X VALUE "/".
           03  PFTC-PATH-WORLD-MACHINE   PIC X(2).
           03  FILLER                    PIC X(10) VALUE "/ED/CAN.LO".
           03  PFTC-MACHINE              PIC 9.
           03  FILLER                    PIC X VALUE ".".
           03  PFTC-YR                   PIC 99.
           03  PFTC-MO                   PIC 99.


       01  PFTI-KEY             PIC 9(6)     VALUE 0.
       01  PFTC-KEY             PIC 9(6)     VALUE 0.

       01  WS-MACHINE.
           03  FILLER           PIC X.
           03  WS-NUMBER        PIC 9.

      **********************
      *   LINE WORKERS     *
      **********************
       01  LN-WORKERS.
           03  LN-FEED          PIC 999      VALUE 0.
           03  LN-COUNT         PIC 999      VALUE 99.
           03  LN-WK            PIC 999      VALUE 0.
       01  PAGE-NUMBER          PIC 9(4)     VALUE 0.

      **********************
      *   MISC WORKERS     *
      **********************
       01  TRP1-CONN-OPEN-SW    PIC X        VALUE "N".
       01  TRL1-CONN-OPEN-SW    PIC X        VALUE "N".
       01  TRS1-CONN-OPEN-SW    PIC X        VALUE "N".

       COPY "LIBGB/GRSSPW.CPY".

       01  PROC-BRANCH-NO       PIC 9(4)     VALUE 0.
       01  SUB                  PIC 9(2)     COMP-3.
       01  TAX-SUB              PIC 9(2)     COMP-3.
       01  SURCHG-SUB           PIC 9(2)     COMP-3.
       01  FIRST-TIME           PIC X        VALUE "Y".
       01  MESS-LIST            PIC X(5)     VALUE "MSEL.".

       01  SURCHARGE-REBATE     PIC S9(5)V99 COMP-3 VALUE 0.
       01  TAX-RATE             PIC S9(5)V9(5) COMP-3 VALUE 0.
       01  SURCHARGE-PERCENT    PIC S9(5)V9(5) COMP-3 VALUE 0.
       01  TC-FEE               PIC S9(5)V99 COMP-3 VALUE 0.
       01  TA-FEE               PIC S9(5)V99 COMP-3 VALUE 0.
       01  TP-FEE               PIC S9(5)V99 COMP-3 VALUE 0.
       01  T1-FEE               PIC S9(5)V99 COMP-3 VALUE 0.
       01  T2-FEE               PIC S9(5)V99 COMP-3 VALUE 0.
       01  T3-FEE               PIC S9(5)V99 COMP-3 VALUE 0.
       01  AD-D-WRITTEN         PIC X        VALUE SPACES.
       01  NON-FILE-WRITTEN     PIC X        VALUE SPACES.
       01  O2-WRITTEN           PIC X        VALUE SPACES.
       01  O3-WRITTEN           PIC X        VALUE SPACES.
       01  TOTAL-I-RECORDS      PIC S9(9)  COMP-3 VALUE 0.
       01  TOTAL-C-RECORDS      PIC S9(9)  COMP-3 VALUE 0.
       01  NEW-LIFE-AMT         PIC S9(9)V99  COMP-3 VALUE 0.
       01  NEW-AH-AMT           PIC S9(9)V99  COMP-3 VALUE 0.
       01  NEW-PP-AMT           PIC S9(9)V99  COMP-3 VALUE 0.
       01  NEW-AUTO-AMT         PIC S9(9)V99  COMP-3 VALUE 0.
       01  NEW-O2-AMT           PIC S9(9)V99  COMP-3 VALUE 0.
       01  NEW-O3-AMT           PIC S9(9)V99  COMP-3 VALUE 0.
       01  NEW-AD-D-AMT         PIC S9(9)V99  COMP-3 VALUE 0.
       01  NEW-NON-FILE-AMT     PIC S9(9)V99  COMP-3 VALUE 0.
       01  NEW-LIFE-COUNT       PIC S9(9)  COMP-3 VALUE 0.
       01  NEW-AH-COUNT         PIC S9(9)  COMP-3 VALUE 0.
       01  NEW-PP-COUNT         PIC S9(9)  COMP-3 VALUE 0.
       01  NEW-AUTO-COUNT       PIC S9(9)  COMP-3 VALUE 0.
       01  NEW-O2-COUNT         PIC S9(9)  COMP-3 VALUE 0.
       01  NEW-O3-COUNT         PIC S9(9)  COMP-3 VALUE 0.
       01  NEW-AD-D-COUNT       PIC S9(9)  COMP-3 VALUE 0.
       01  NEW-NON-FILE-COUNT   PIC S9(9)  COMP-3 VALUE 0.
       01  CANCEL-LIFE-AMT      PIC S9(9)V99  COMP-3 VALUE 0.
       01  CANCEL-AH-AMT        PIC S9(9)V99  COMP-3 VALUE 0.
       01  CANCEL-PP-AMT        PIC S9(9)V99  COMP-3 VALUE 0.
       01  CANCEL-AUTO-AMT      PIC S9(9)V99  COMP-3 VALUE 0.
       01  CANCEL-O2-AMT        PIC S9(9)V99  COMP-3 VALUE 0.
       01  CANCEL-O3-AMT        PIC S9(9)V99  COMP-3 VALUE 0.
       01  CANCEL-LIFE-COUNT    PIC S9(9)  COMP-3 VALUE 0.
       01  CANCEL-AH-COUNT      PIC S9(9)  COMP-3 VALUE 0.
       01  CANCEL-PP-COUNT      PIC S9(9)  COMP-3 VALUE 0.
       01  CANCEL-AUTO-COUNT    PIC S9(9)  COMP-3 VALUE 0.
       01  CANCEL-O2-COUNT      PIC S9(9)  COMP-3 VALUE 0.
       01  CANCEL-O3-COUNT      PIC S9(9)  COMP-3 VALUE 0.
       01  PRIMARY-SSNO         PIC 9(9) COMP-3 VALUE 0.
       01  PRIMARY-LNAME        PIC X(15).
       01  PRIMARY-FNAME        PIC X(10).
       01  PRIMARY-AGE          PIC 9(6).
       01  PRIMARY-BDATE        PIC 9(8).
       01  PRIMARY-ADR1         PIC X(24).
       01  PRIMARY-ADR2         PIC X(24).
       01  PRIMARY-CITY         PIC X(15).
       01  PRIMARY-STATE        PIC XX.
       01  PRIMARY-ZIP          PIC X(10).
       01  JOINT-AGE            PIC 9(6).
       01  JOINT-BDATE          PIC 9(8).
       01  JOINT-LNAME          PIC X(15).
       01  JOINT-FNAME          PIC X(10).
       01  JOINT-SSNO           PIC 9(9) COMP-3 VALUE 0.

       01  HOLD-LN-BR           PIC 9(4) VALUE 9999.
       01  RPT-END-DATE         PIC 9(8)            VALUE 0.
       01  BEGINNING-DATE       PIC 9(8)            VALUE 0.

       01  WORK-INSCOMP         PIC 99.
       01  HOLD-AD-D            PIC S9(5)V99 COMP-3.
       01  HOLD-NON-FILE        PIC S9(5)V99 COMP-3.
       01  HOLD-TAX-CITY-CODE   PIC X(4).
       01  HOLD-TAX-COUNTY-CODE PIC X(4).

      ***********************************
      *     S C R E E N   F I E L D S   *
      ***********************************
       COPY "LIBGB/EOBUF_EXT.CPY".
           03  BEG-BR          PIC 9999.
           03  END-BR          PIC 9999.
           03  BEG-IC          PIC 99.
           03  END-IC          PIC 99.
           03  BEG-LC          PIC 9(3).
           03  END-LC          PIC 9(3).
           03  MMYY            PIC 9(4).
           03  MM-YY REDEFINES MMYY.
               05  MM          PIC 9(2).
               05  YY          PIC 9(2).
           03  GROUP-FLAG      PIC X.
           03  ENT-GROUP       PIC X(4).

       01  VERSION-CONTROL     PIC X    VALUE "A".

       01  SV-BEG-BR           PIC 9999  VALUE 9999.
       01  SV-END-BR           PIC 9999  VALUE 9999.

      ***********************************
      *     P R I N T   B U F F E R S   *
      ***********************************
       01  HEAD1.
           03  H-DATE           PIC X(8)     VALUE " ".
           03  FILLER           PIC XX       VALUE " ".
           03  H-HR             PIC 99       VALUE 0.
           03  FILLER           PIC X        VALUE ":".
           03  H-MIN            PIC 99       VALUE 0.
           03  FILLER           PIC X(8)    VALUE " ".
           03  H-NAME           PIC X(40)    VALUE " ".
           03  FILLER           PIC X(8)    VALUE " ".
           03  FILLER           PIC X(5)     VALUE "PAGE:".
           03  H-PAGE-NO        PIC ZZZ9.

       01  HEAD2.
           03  FILLER           PIC X(10)    VALUE "USER ID:".
           03  H-USERID         PIC X(10).
           03  FILLER           PIC X(13)    VALUE " ".
           03  FILLER           PIC X(40) VALUE
           " INSURANCE DATA TAPE ".

       01  HEAD3.
           03  H-INSCO          PIC X(10) VALUE " ".
           03  FILLER           PIC X VALUE " ".
           03  FILLER           PIC X(25) VALUE " ".
           03  FILLER           PIC X(13) VALUE "FOR MONTH OF ".
           03  H-MMYY           PIC 99/99.

       01  HEAD5.
           03  FILLER           PIC X(4) VALUE " ".
           03  FILLER           PIC X(16) VALUE "NEW LIFE".
           03  FILLER           PIC X(13) VALUE "NEW A&H".
           03  FILLER           PIC X(14) VALUE "NEW PROP".
           03  FILLER           PIC X(14) VALUE "NEW OTH1 ".
           03  FILLER           PIC X(11) VALUE "NEW OTH2 ".
           03  FILLER           PIC X(8)  VALUE "NEW OTH3".

       01  HEAD5A.
           03  FILLER           PIC X(2) VALUE " ".
           03  FILLER           PIC X(11) VALUE "NEW AD&D".
           03  FILLER           PIC X(11) VALUE "NEW NONF".

       01  HEAD6.
           03  FILLER           PIC X(16) VALUE "LIFE REFUNDS".
           03  FILLER           PIC X(14) VALUE "A&H REFUNDS".
           03  FILLER           PIC X(14) VALUE "PROP REFNDS".
           03  FILLER           PIC X(14) VALUE "OTH1 REFNDS".
           03  FILLER           PIC X(13) VALUE "OTH2 REFNDS".
           03  FILLER           PIC X(9) VALUE "O3 REFNDS".


       01  DETAIL-LINE          PIC X(80) VALUE SPACES.

       01  DETAIL1 REDEFINES DETAIL-LINE.
           03  FILLER           PIC X(5).
           03  D-QTY1           PIC Z(6)9.
           03  FILLER           PIC X(7).
           03  D-QTY2           PIC Z(6)9.
           03  FILLER           PIC X(7).
           03  D-QTY3           PIC Z(6)9.
           03  FILLER           PIC X(7).
           03  D-QTY4           PIC Z(6)9.
           03  FILLER           PIC X(7).
           03  D-QTY5           PIC Z(6)9.
           03  FILLER           PIC X(4).
           03  D-QTY6           PIC Z(6)9.

       01  DETAIL2 REDEFINES DETAIL-LINE.
           03  D-AMT1           PIC Z,ZZZ,ZZ9.99-.
           03  FILLER           PIC X.
           03  D-AMT2           PIC Z,ZZZ,ZZ9.99-.
           03  FILLER           PIC X.
           03  D-AMT3           PIC Z,ZZZ,ZZ9.99-.
           03  FILLER           PIC X.
           03  D-AMT4           PIC Z,ZZZ,ZZ9.99-.
           03  FILLER           PIC X.
           03  D-AMT5           PIC Z,ZZZ,ZZ9.99-.
           03  D-AMT6           PIC ZZZ,ZZ9.99-.

       01  RANGE-LINE REDEFINES DETAIL-LINE.
           03  RANGE-SELECTION  PIC X(30).
           03  FILLER           PIC X(5).
           03  RANGE1           PIC X(10).
           03  RANGE1N REDEFINES RANGE1 PIC 9(10).
           03  FILLER           PIC X(2).
           03  RANGE2           PIC X(10).
           03  RANGE2N REDEFINES RANGE2 PIC 9(10).

       COPY "LIBWI/GRSCANW.CPY".
       COPY "LIBLP/LPWKPATH.CPY".
       COPY "LIBLP/LPSTATW.CPY".
       COPY "LIBLP/LPITRMW.CPY".
       COPY "LIBLP/LPBAGEW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBGB/GETPRW.CPY".
       COPY "LIBGB/MKTEMPW_CMD.CPY".
       COPY "LIBGB/SYSTEMW.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/GRWORKW.CPY".
       COPY "LIBGB/DACCESSW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBSP/LSOUTH_DEF.CPY".
       COPY "LIBSP/LSOUTH_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".
       COPY "LIBLP/ARRAYSPW.CPY". 

      *****************************************************************
      *
      *     P R O G R A M   C O N T R O L
      *
      *****************************************************************
       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBSP/LSOUTH_SCN.CPY".

       PROCEDURE DIVISION
           USING FORM-PATHNAME EXIT-PATHNAME.

       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
               PR-FILE PFTI-FILE PFTC-FILE WK-FILE.
       COPY "LIBGB/DECLRP.CPY".
           PERFORM CLOSE-TRS1-FILE.
           PERFORM CLOSE-TRP1-FILE.
           PERFORM CLOSE-TRL1-FILE.
           PERFORM CLOSE-SP1-FILE.
      *    PERFORM CLOSE-LN1-FILE.
           PERFORM CLOSE-BW1-FILE.
           PERFORM CLOSE-GR1-FILE.
           PERFORM CLOSE-BR-FILE.
           PERFORM CLOSE-PR-FILE.
           PERFORM CLOSE-TRP1-CONNECTION.
           PERFORM CLOSE-TRL1-CONNECTION.
           PERFORM CLOSE-TRS1-CONNECTION.
           CLOSE
               PFTI-FILE PFTC-FILE WK-FILE.
       COPY "LIBGB/DECLRP2.CPY".

      ********************************************
       MAIN-MODULE SECTION.
      ********************************************
           PERFORM SQL-CONNECT.

      D    STOP MESS.
           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "LSOUTH" TO VDU-FORM-NAME.

           PERFORM INITIALIZATION.
           IF EXT-EOBUF-ERRCD NOT = " "
              GO TO END-PROGRAM.

           IF GROUP-FLAG = "N"
              MOVE BEG-BR TO BR-NO
              PERFORM START-BR-FILE
              GO TO NEXT-BRANCH-NO.

           MOVE ENT-GROUP TO GR-ID.
           PERFORM SEARCH-GR-INIT.

       NEXT-GROUP-LOC.
           IF GROUP-FLAG NOT = "Y"
              GO TO NEXT-BRANCH-NO.

           PERFORM SEARCH-GR-NEXT.
           IF WGR-SRESULT = 0
      *        GO TO PROCESS-REPORT.
              GO TO END-LOAD-BRANCH-TABLE.

           MOVE WGR-SRESULT TO BR-NO.
           PERFORM READ-BR-FILE.
           IF IO-FG NOT = 0
              GO TO IO-ERROR.

      *     GO TO MOVE-BRANCH-PATH-INFO.
           GO TO LOAD-BRANCH-TABLE.  

       NEXT-BRANCH-NO.
           PERFORM READ-BR-FILE-NEXT.
           IF IO-FG = 9 OR BR-NO > END-BR
      *        GO TO PROCESS-REPORT.
              GO TO END-LOAD-BRANCH-TABLE.    

       COPY "LIBGB/GRSSP.CPY".
      * LOAD-BRANCH-TABLE.
      * END-LOAD-BRANCH-TABLE.

           IF BRANCHLIST-PROC-BRANCH-CTR = 0
               GO TO PROCESS-REPORT.

       MOVE-BRANCH-PATH-INFO.
           MOVE EXT-FILPATH-BASE TO FILPATH-BASE.
      *     MOVE BR-MACHINE       TO FILPATH-MACHINE.
      *     MOVE BR-DATA-PATH     TO FILPATH-DATA.
      *
           MOVE "B"     TO LN-PATH-OVERRIDE
                           TRP-PATH-OVERRIDE
                           TRS-PATH-OVERRIDE
                           TRL-PATH-OVERRIDE.
      *
      *     IF EXT-ROUTE-BUF = "00"
      *        STRING "CREATE IN PROGRESS............",BR-NO 
      *           DELIMITED "  " INTO MESS
      *        PERFORM SEND-LEGEND.
      *
      *     PERFORM OPEN-LN1-FILE.


           PERFORM SORT-TRP-FILE.
           PERFORM SORT-TRL-FILE.
           PERFORM SORT-TRS-FILE.

       END-OF-GROUP.
      *     PERFORM CLOSE-LN1-FILE.
      *     GO TO NEXT-GROUP-LOC.

           IF BRANCHLIST-PROC-BRANCH-CTR = BRANCHLIST-PGM-PROC-MAX
               INITIALIZE BRANCHLIST-TBL
                          PASS-BRANCHLIST
                          BRANCHLIST-PROC-BRANCH-CTR
                          BRANCHLIST-SUB
               GO TO NEXT-GROUP-LOC.

       PROCESS-REPORT.
           PERFORM CLOSE-WK-FILE.
           PERFORM OPEN-WK-FILE.

           MOVE 0 TO WORK-OWNBR WORK-ACCTNO.

           PERFORM START-WK-FILE.

           PERFORM RUN-REPORT.

           PERFORM PRINT-TOTALS.

           PERFORM PRINT-RANGE-LINES.

           GO TO END-PROGRAM.

      ***********************************
       INITIALIZATION SECTION.
      ***********************************
           MOVE LSOUTH-QUEUE-POS TO Q-POS.
           MOVE LSOUTH-COPIES-POS TO C-POS.
           MOVE LSOUTH-PRU-POS   TO P-POS.
           MOVE SPACES TO Q-BUF C-BUF P-BUF.

       COPY "LIBGB/EOINIT.CPY".


      * CREATE WORK FILE FOR VALID LOANS
           MOVE MKTEMP-DEFAULT TO MKTEMP-BUF.
           PERFORM MKTEMP-CALL.
           IF STAT NOT = "00"
              MOVE "CANNOT MAKE WORK FILE" TO MESS
              PERFORM SEND-MESS
              GO TO INITIALIZE-ENDIT.
           PERFORM OPEN-WK-FILE-OUTPUT.
           PERFORM CLOSE-WK-FILE.
           PERFORM OPEN-WK-FILE.

           PERFORM OPEN-BR-FILE.
           MOVE EXT-CONAME-USER-LOC TO BR-NO.
           PERFORM READ-BR-FILE.
           IF IO-FG NOT = 0
              MOVE "R1" TO BR-MACHINE.
           MOVE BR-MACHINE TO WS-MACHINE.

           PERFORM OPEN-GR1-FILE.
           PERFORM OPEN-SP1-FILE.


       INITIALIZE-BEG-RPT.
           IF EXT-ROUTE-BUF = "00"
              MOVE "CREATE IN PROGRESS................" TO MESS
              PERFORM SEND-LEGEND.

           MOVE EXT-FILPATH-BASE TO PFTI-PATH-BASE
                                    PFTC-PATH-BASE
                                    PFTI-PATH-WORLD-BASE
                                    PFTC-PATH-WORLD-BASE.
           MOVE EXT-FILPATH-MACHINE TO PFTI-PATH-MACHINE
                                       PFTC-PATH-MACHINE
                                       PFTI-PATH-WORLD-MACHINE
                                       PFTC-PATH-WORLD-MACHINE.
           MOVE YY TO PFTI-YR PFTC-YR.
           MOVE MM TO PFTI-MO PFTC-MO.
           MOVE WS-NUMBER TO PFTC-MACHINE PFTI-MACHINE.
           MOVE PFTI-PATH-WORLD TO PFTI-PATH.
           MOVE PFTC-PATH-WORLD TO PFTC-PATH.

      * NEW ISSUES
           PERFORM OPEN-PFTI-FILE-OUTPUT.
           PERFORM CLOSE-PFTI-FILE.
           PERFORM OPEN-PFTI-FILE.
      * REFUNDS/CANCELS
           PERFORM OPEN-PFTC-FILE-OUTPUT.
           PERFORM CLOSE-PFTC-FILE.
           PERFORM OPEN-PFTC-FILE.

      * RPT-END-DATE IS ALREADY SET, JUST NEED A BEGINNING FOR TR
           MOVE MM TO DATE-MMDDYY-MM.
           MOVE YY TO DATE-MMDDYY-YY.
           MOVE 01 TO DATE-MMDDYY-DD.
           PERFORM CONVERT-MMDDYY-TO-YYYYMMDD.
           MOVE DATE-YYYYMMDD TO BEGINNING-DATE.

           PERFORM OPEN-PR-FILE.

           IF BEG-BR = 0
              IF END-BR = 0
                 MOVE BEG-BR TO SV-BEG-BR
                 MOVE END-BR TO SV-END-BR
                 MOVE EXT-CONAME-USER-LOC TO BEG-BR
                                   END-BR.
           MOVE EXT-CONAME-CONAME TO H-NAME.
           MOVE EXT-CONAME-SYSDATE TO H-DATE.
           MOVE EXT-CONAME-USER-ID TO H-USERID.
           MOVE MMYY TO H-MMYY.

           MOVE 0 TO TOTAL-I-RECORDS TOTAL-C-RECORDS.

           PERFORM ARRAYSP-LOAD.

           MOVE 500 TO BRANCHLIST-PGM-PROC-MAX.

           GO TO INITIALIZE-EXIT.

       INITIALIZE-ENDIT.
           MOVE "X" TO EXT-EOBUF-ERRCD.
       INITIALIZE-EXIT.
           EXIT.

      *******************************************************
       EO-EXEC-AUDIT SECTION.
      *******************************************************
      * ALWAYS SET THE TRANS-PATH WITH DATE 2 FROM BATCH
           MOVE EXT-EOBUF-DATE2 TO NDTE-DATE RPT-END-DATE NUM-DATE.
           MOVE NUM-MO TO MM.
           MOVE NUM-YR TO YY.

      *******************************************************
       ENTER-PARAMETERS SECTION.
      *******************************************************
       ENTER-PARAM.
      * FOR RE-ENTRY ON F4 FROM ACCEPT.
           IF VDUSTATUS = "1U"
              IF EXT-ROUTE-BUF = "40" OR "50"
                 GO TO LC-02
              ELSE
                 GO TO DT-01.
       BR-01.
           MOVE "E  A000BR1." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "16" GO TO ALL-BR.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO BR-01.
           IF VDUSTATUS = "1<"
              MOVE "WI/GRSCAN" TO FORM-NAM
              CALL FORM-PROGX USING FORM-PATH GRSCAN-BUF EXIT-PATHNAME
              CANCEL FORM-PROGX
              IF GRSCAN-STATUS NOT = "00"
                 GO TO BR-01
              ELSE
                 MOVE "S  A000BR1." TO VDU
                 MOVE GRSCAN-KEY TO VDUBUF GR1-KEY
                 PERFORM SCREENIO
                 MOVE "00" TO VDUSTATUS.

           IF VDUSTATUS NOT = "00" GO TO BR-01.

           MOVE VDUBUF TO WGR-WORD ENT-GROUP.
           IF WGR-WORD-ZERO
              GO TO BRANCH-IS-NUMERIC.
           MOVE 0 TO WGR-MIN-LEVEL.
           PERFORM CHECK-MEMBER.
           IF WGR-ERR > 0
              MOVE WGR-MSG(WGR-ERR) TO MESS
              PERFORM SEND-MESS
              GO TO BR-01.
           IF WGR-TYPE = "A"
              MOVE "S  A000BR2." TO VDU
              MOVE SPACES TO VDUBUF
              PERFORM SCREENIO
              MOVE "Y" TO GROUP-FLAG
              MOVE 0 TO BEG-BR
              MOVE 9999 TO END-BR
              GO TO IC-01.
       BRANCH-IS-NUMERIC.
           MOVE "N" TO GROUP-FLAG.
           MOVE WGR-SRESULT TO BEG-BR.
           MOVE "SN N040BR1." TO VDU.
           MOVE BEG-BR TO VDUNUM0.
           PERFORM SCREENIO.
       BR-02.
           MOVE "ENNN040BR2." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "16" GO TO ALL-BR.
           IF VDUSTATUS = "1U" GO TO BR-01.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO BR-02.
           MOVE VDUNUM0 TO END-BR.
           IF BEG-BR NOT > END-BR GO TO IC-01.
           PERFORM INVALID-RANGE.
           GO TO BR-01.
       ALL-BR.
           MOVE "SNNN040BR1." TO VDU.
           MOVE 0 TO BEG-BR VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN040BR2." TO VDU.
           MOVE 9999 TO END-BR VDUNUM0.
           PERFORM SCREENIO.
           MOVE "N" TO GROUP-FLAG.
       IC-01.
           MOVE "ENNN020IC1." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "16" GO TO ALL-IC.
           IF VDUSTATUS = "1U" GO TO BR-02.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO IC-01.
           MOVE VDUNUM0 TO BEG-IC.
       IC-02.
           MOVE "ENNN020IC2." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "16" GO TO ALL-IC.
           IF VDUSTATUS = "1U" GO TO IC-01.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO IC-02.
           MOVE VDUNUM0 TO END-IC.
           IF BEG-IC NOT > END-IC GO TO LC-01.
           PERFORM INVALID-RANGE.
           GO TO IC-01.
       ALL-IC.
           MOVE "SNNN020IC1." TO VDU.
           MOVE 0 TO BEG-IC VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN020IC2." TO VDU.
           MOVE 99 TO END-IC VDUNUM0.
           PERFORM SCREENIO.
       LC-01.
           MOVE "ENNN020LC1." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "16" GO TO ALL-LC.
           IF VDUSTATUS = "1U" GO TO IC-02.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO IC-02.
           MOVE VDUNUM0 TO BEG-LC.
       LC-02.
           MOVE "ENNN020LC2." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "16" GO TO ALL-LC.
           IF VDUSTATUS = "1U" GO TO LC-01.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO LC-02.
           MOVE VDUNUM0 TO END-LC.
           IF BEG-LC > END-LC
              PERFORM INVALID-RANGE
              GO TO LC-01.
           GO TO DT-01.
       ALL-LC.
           MOVE "SNNN020LC1." TO VDU.
           MOVE 1 TO BEG-LC VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN020LC2." TO VDU.
           MOVE 99 TO END-LC VDUNUM0.
           PERFORM SCREENIO.
       DT-01.
           IF EXT-ROUTE-BUF = "40" OR "50"
              GO TO EXIT-PARM.
           MOVE "ENNN040DT1." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO LC-02.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO DT-01.
           MOVE VDUNUM0 TO MMYY.
           IF MM < 1 OR MM > 12
              MOVE "INVALID MONTH" TO MESS
              PERFORM SEND-MESS
              GO TO DT-01.

           MOVE MM TO DATE-MMDDYY-MM.
           MOVE YY TO DATE-MMDDYY-YY.
           MOVE 33 TO DATE-MMDDYY-DD.
           PERFORM CONVERT-MMDDYY-TO-YYYYMMDD.
           MOVE DATE-YYYYMMDD TO NDTE-DATE.
           PERFORM NEW-DATE-CALCULATION.
           MOVE NDTE-DATE TO RPT-END-DATE.

       EXIT-PARM.
           EXIT.

      *************************************************
       DISPLAY-PARAMETERS SECTION.
      *************************************************
           IF GROUP-FLAG = "Y"
              MOVE "S  A000BR1." TO VDU
              MOVE ENT-GROUP TO VDUBUF
              PERFORM SCREENIO
              MOVE "S  A000BR2." TO VDU
              MOVE SPACES TO VDUBUF
              PERFORM SCREENIO
           ELSE
              MOVE "SNNN040BR1." TO VDU
              MOVE BEG-BR TO VDUNUM0
              PERFORM SCREENIO
              MOVE "SNNN040BR2." TO VDU
              MOVE END-BR TO VDUNUM0
              PERFORM SCREENIO.

           MOVE "SNNN020IC1." TO VDU.
           MOVE BEG-IC TO VDUNUM0.
           PERFORM SCREENIO.

           MOVE "SNNN020IC2." TO VDU.
           MOVE END-IC TO VDUNUM0.
           PERFORM SCREENIO.

           MOVE "SNNN020LC1." TO VDU.
           MOVE BEG-LC TO VDUNUM0.
           PERFORM SCREENIO.

           MOVE "SNNN020LC2." TO VDU.
           MOVE END-LC TO VDUNUM0.
           PERFORM SCREENIO.


      *************************************************
       RUN-REPORT SECTION.
      *************************************************

       KEY-FILE-LOOP.
           PERFORM READ-WK-FILE-NEXT.
           IF IO-FG = 9
              GO TO END-RUN-REPORT.

           IF WORK-OWNBR NOT = HOLD-LN-BR
              MOVE WORK-OWNBR TO HOLD-LN-BR BR-NO
              IF EXT-ROUTE-BUF = "00"
                 STRING "CREATE IN PROGRESS............",BR-NO
                    DELIMITED "  " INTO MESS
                 PERFORM SEND-LEGEND
              END-IF
              PERFORM GET-LN-BW-PATH.

      *     MOVE WORK-ACCTNO TO LN-ACCTNO.

      * LN DATA NOW HELD IN WORK FILE
      *     PERFORM READ-LN1-FILE.
      *     IF IO-FG NOT = 0
      *        GO TO END-RUN-REPORT.
           MOVE WORK-LN-REC TO LN-REC.

      * IF THE LOAN ISNT REALLY PAID OUT
      * IF THE WORK RECORD HAS 0 IN ALL THE FIELDS, BUT THE DATES,
      * THEN IT PROBABLY HIT A PAYOFF AND REVERSAL, SO THE NUMBERS
      * ALL NETTED TO 0, SKIP IT !!!!
           IF LN-CURBAL NOT = 0
              IF WORK-NEW-CL = 0 AND
                  WORK-CAN-CL = 0 AND
                   WORK-NEW-AH = 0 AND
                    WORK-CAN-AH = 0 AND
                     WORK-NEW-PP = 0 AND
                      WORK-CAN-PP = 0 AND
                       WORK-NEW-O1 = 0 AND
                        WORK-CAN-O1 = 0 AND
                         WORK-NEW-O2 = 0 AND
                          WORK-CAN-O2 = 0 AND
                           WORK-NEW-O3 = 0 AND
                            WORK-CAN-O3 = 0 AND
                             WORK-NEW-AD-D = 0 AND
                              WORK-NEW-NON-FILE = 0
              GO TO KEY-FILE-LOOP.

           MOVE LN-ORGST     TO SP-ORGST.
           MOVE LN-SPRCLASS  TO SP-SPRCLASS.
           MOVE LN-SUBCLASS  TO SP-SUBCLASS.
           MOVE LN-LAWCODE   TO SP-LAWCODE.

           IF LPWSSP-SP1-KEY NOT = SP1-KEY
              MOVE SP1-KEY TO LPWSSP-SP1-KEY
      *        PERFORM READ-SP1-FILE
              PERFORM ARRAYSP-READ
              IF IO-FG NOT = 0
                 GO TO IO-ERROR.

           PERFORM KEY-DETAIL.

           GO TO KEY-FILE-LOOP.

       END-RUN-REPORT.
           EXIT.

      ******************************************************************
       SORT-TRP-FILE SECTION.
      ******************************************************************

           PERFORM OPEN-TRP1-FILE.

      *     MOVE BR-NO               TO TP-BRNO
      *                                 QTRP1-WBEG-BRNO
      *                                 QTRP1-WEND-BRNO.

           MOVE BEGINNING-DATE      TO TP-DATE SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO QTRP1-WBEG-DATE.
           MOVE RPT-END-DATE        TO  SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO QTRP1-WEND-DATE.

           MOVE BEG-LC              TO TP-CLASS
                                       QTRP1-WBEG-CLASS.
           MOVE END-LC              TO QTRP1-WEND-CLASS.

      *     MOVE 0                   TO TP-ACCTNO TP-LINE
      *                                 QTRP1-WBEG-ACCTNO
      *                                 QTRP1-WBEG-LINE.
      *     MOVE ALL "9"             TO QTRP1-WEND-ACCTNO
      *                                 QTRP1-WEND-LINE.

      *    MOVE TP-DATE              TO NDTE-DATE.
      *    MOVE 1                    TO NDTE-HOLD.
      *    PERFORM INCREMENT-DAYS.
      *    MOVE NDTE-DATE            TO SQL-DATE-YYYYMMDD.
      *    PERFORM SQL-SET-DATE.
      *    MOVE SQL-DATE-YYYY-MM-DD  TO QTRP1-WSBEG-DATE.
      *
      *    IF ( TP-CLASS = 999 )
      *       MOVE TP-CLASS          TO QTRP1-WSBEG-CLASS
      *    ELSE
      *       COMPUTE QTRP1-WSBEG-CLASS  = TP-CLASS + 1.
      *
      *    IF ( TP-ACCTNO = 99999999 )
      *       MOVE TP-ACCTNO         TO QTRP1-WSBEG-ACCTNO
      *    ELSE
      *       COMPUTE QTRP1-WSBEG-ACCTNO = TP-ACCTNO + 1.
      *
      *    IF ( TP-LINE = 999 )
      *       MOVE TP-LINE           TO QTRP1-WSBEG-LINE
      *    ELSE
      *       COMPUTE QTRP1-WSBEG-LINE   = TP-LINE + 1.

           PERFORM START-TRP1-FILE.   *> SSP TRP LN

           MOVE 0 TO PROC-BRANCH-NO.

       SORT-TRP-NEXT.
           PERFORM READ-TRP1-FILE-NEXT.   *> EMBEDDED TRP LN
           IF (IO-FG NOT = 0)              *> OR (TRP-PATH-OWNBR NOT = TP-BRNO)
              GO TO SORT-TRP-EXIT.

           IF TP-DATE > RPT-END-DATE
              GO TO SORT-TRP-EXIT.

           IF TP-CLASS < BEG-LC OR TP-CLASS > END-LC
              GO TO SORT-TRP-NEXT.

      * THIS CHECK ALSO IN STORED PROCEDURE 
           IF (TP-ADDON-TEXT = "ADD" OR "CNL" OR "ADJ") AND
               (TP-ADDON-TYPE = "CL" OR "AH" OR "PP" OR "O1" OR
                                 "O2" OR "O3")
               NEXT SENTENCE
           ELSE
           IF TP-TRCD NOT = "RC" AND NOT = "RA" AND
                       NOT = "CC" AND NOT = "CA" AND
                        NOT = "CP" AND NOT = "RP" AND
                         NOT = "C1" AND NOT = "R1" AND
                          NOT = "C2" AND NOT = "R2" AND
                           NOT = "C3" AND NOT = "R3"
                  GO TO SORT-TRP-NEXT.

           IF TP-BRNO NOT = PROC-BRANCH-NO
              MOVE TP-BRNO TO BR-NO PROC-BRANCH-NO
              PERFORM READ-BR-FILE
              IF IO-FG NOT = 0
                 GO TO IO-ERROR.

      * LNFILE DATA INCLUDE IN STORED PROCEDURE 
      *     MOVE TP-BRNO   TO LN-OWNBR.
      *     MOVE TP-ACCTNO TO LN-ACCTNO.
      *     PERFORM READ-LN1-FILE.
      *     IF IO-FG  NOT = 0
      *        GO TO SORT-TRP-NEXT.

      * ONLY REPORT FOR THE OWNING BRANCH ("G")
      * THIS CHECK ALSO IN STORED PROCEDURE
           IF TP-OTHBR-FG = "C"
              GO TO SORT-TRP-NEXT
      * THE "P" TRANSACTIONS ARE ONLY ADDONS AND REBATES/CANCELS
      * NEED TO SUBTRACT OUT THE INSURANCE TAX, BUT CHECK FOR
      * SPACES HERE SO WE CAN JUST SUBTRACT IT FURTHER DOWN
           ELSE
              IF TP-INS-TAX-RB = 2020202.02
                 MOVE 0 TO TP-INS-TAX-RB.

           MOVE 0 TO WORK-INSCOMP.

           IF (TP-REFNO = "ADDCL" OR "CNLCL" OR "ADJCL")
                        OR (TP-TRCD = "RC" OR "CC")
              IF LN-INSOURS(1) = "Y"
                 MOVE LN-INSCOMP(1) TO WORK-INSCOMP
              END-IF
           ELSE
              IF (TP-REFNO = "ADDAH" OR "CNLAH" OR "ADJAH")
                          OR (TP-TRCD = "RA" OR "CA")
                 IF LN-INSOURS(2) = "Y"
                    MOVE LN-INSCOMP(2) TO WORK-INSCOMP
                 END-IF
              ELSE
              IF (TP-REFNO = "ADDPP" OR "CNLPP" OR "ADJPP")
                           OR (TP-TRCD = "RP" OR "CP")
                    IF LN-INSOURS(3) = "Y"
                       MOVE LN-INSCOMP(3) TO WORK-INSCOMP
                    END-IF
              ELSE
              IF (TP-REFNO = "ADDO1" OR "CNLO1" OR "ADJO1")
                           OR (TP-TRCD = "R1" OR "C1")
                    IF LN-INSOURS(4) = "Y"
                       MOVE LN-INSCOMP(4) TO WORK-INSCOMP
                    END-IF
              ELSE
              IF (TP-REFNO = "ADDO2" OR "CNLO2" OR "ADJO2")
                           OR (TP-TRCD = "R2" OR "C2")
                    IF LN-INSOURS(5) = "Y"
                       MOVE LN-INSCOMP(5) TO WORK-INSCOMP
                    END-IF
              ELSE
              IF (TP-REFNO = "ADDO3" OR "CNLO3" OR "ADJO3")
                           OR (TP-TRCD = "R3" OR "C3")
                    IF LN-INSOURS(6) = "Y"
                        MOVE LN-INSCOMP(6) TO WORK-INSCOMP.


      * IF LN-INSCOMP = 0, THEN ADDON THEN "RV", JUST IGNORE IT
      *                  , OR NOT INSOURS
           IF WORK-INSCOMP = 0
              GO TO SORT-TRP-NEXT.

           IF WORK-INSCOMP < BEG-IC OR > END-IC
              GO TO SORT-TRP-NEXT.

           MOVE TP-BRNO   TO WORK-OWNBR.
           MOVE TP-ACCTNO TO WORK-ACCTNO.
           PERFORM READ-WK-FILE.
           IF IO-FG NOT = 0
              PERFORM CLEAR-WK-FILE.

           MOVE TP-INS-TAX-COUNTY-CD TO WORK-TAX-COUNTY-CODE
           MOVE TP-INS-TAX-CITY-CD TO WORK-TAX-CITY-CODE
           IF TP-ADDON-TYPE = "CL" AND
              TP-ADDON-TEXT = "ADD"
              MOVE "A" TO WORK-ADDON-FG-CL
              COMPUTE WORK-NEW-CL = WORK-NEW-CL + TP-APCUR
           ELSE
           IF TP-ADDON-TYPE = "AH" AND
              TP-ADDON-TEXT = "ADD"
              MOVE "A" TO WORK-ADDON-FG-AH
              COMPUTE WORK-NEW-AH = WORK-NEW-AH + TP-APCUR
           ELSE
           IF TP-ADDON-TYPE = "PP" AND
              TP-ADDON-TEXT = "ADD"
              MOVE "A" TO WORK-ADDON-FG-PP
              COMPUTE WORK-NEW-PP = WORK-NEW-PP + TP-APCUR
           ELSE
           IF TP-ADDON-TYPE = "O1" AND
              TP-ADDON-TEXT = "ADD"
              MOVE "A" TO WORK-ADDON-FG-O1
              COMPUTE WORK-NEW-O1 = WORK-NEW-O1 + TP-APCUR
           ELSE
           IF TP-ADDON-TYPE = "O2" AND
              TP-ADDON-TEXT = "ADD"
              MOVE "A" TO WORK-ADDON-FG-O2
              COMPUTE WORK-NEW-O2 = WORK-NEW-O2 + TP-APCUR
           ELSE
           IF TP-ADDON-TYPE = "O3" AND
              TP-ADDON-TEXT = "ADD"
              MOVE "A" TO WORK-ADDON-FG-O3
              COMPUTE WORK-NEW-O3 = WORK-NEW-O3 + TP-APCUR
           ELSE
           IF TP-ADDON-TYPE = "CL" AND
              TP-ADDON-TEXT = "CNL"
              MOVE "C" TO WORK-CANCEL-FG-CL
              COMPUTE WORK-CAN-CL = WORK-CAN-CL + TP-APCUR
              MOVE TP-PAYDATE TO WORK-CAN-CL-DATE
           ELSE
           IF TP-ADDON-TYPE = "AH" AND
              TP-ADDON-TEXT = "CNL"
              MOVE "C" TO WORK-CANCEL-FG-AH
              COMPUTE WORK-CAN-AH = WORK-CAN-AH + TP-APCUR
              MOVE TP-PAYDATE TO WORK-CAN-AH-DATE
           ELSE
           IF TP-ADDON-TYPE = "PP" AND
              TP-ADDON-TEXT = "CNL"
              MOVE "C" TO WORK-CANCEL-FG-PP
              COMPUTE WORK-CAN-PP = WORK-CAN-PP + TP-APCUR
              MOVE TP-PAYDATE TO WORK-CAN-PP-DATE
           ELSE
           IF TP-ADDON-TYPE = "O1" AND
              TP-ADDON-TEXT = "CNL"
              MOVE "C" TO WORK-CANCEL-FG-O1
              COMPUTE WORK-CAN-O1 = WORK-CAN-O1 + TP-APCUR
              MOVE TP-PAYDATE TO WORK-CAN-O1-DATE
           ELSE
           IF TP-ADDON-TYPE = "O2" AND
              TP-ADDON-TEXT = "CNL"
              MOVE "C" TO WORK-CANCEL-FG-O2
              COMPUTE WORK-CAN-O2 = WORK-CAN-O2 + TP-APCUR
              MOVE TP-PAYDATE TO WORK-CAN-O2-DATE
           ELSE
           IF TP-ADDON-TYPE = "O3" AND
              TP-ADDON-TEXT = "CNL"
              MOVE "C" TO WORK-CANCEL-FG-O3
              COMPUTE WORK-CAN-O3 = WORK-CAN-O3 + TP-APCUR
              MOVE TP-PAYDATE TO WORK-CAN-O3-DATE
           ELSE
           IF TP-TRCD = "RC"
              COMPUTE WORK-CAN-CL = WORK-CAN-CL +
                                ((TP-TRAMT - TP-INS-TAX-RB) * -1)
              MOVE "R" TO WORK-REFUND-FG-CL
              MOVE TP-PAYDATE TO WORK-CAN-CL-DATE
              MOVE TP-INS-TAX-RB TO WORK-CAN-CL-TAX
           ELSE
           IF TP-TRCD = "RA"
              COMPUTE WORK-CAN-AH = WORK-CAN-AH +
                                ((TP-TRAMT - TP-INS-TAX-RB) * -1)
              MOVE "R" TO WORK-REFUND-FG-AH
              MOVE TP-PAYDATE TO WORK-CAN-AH-DATE
              MOVE TP-INS-TAX-RB TO WORK-CAN-AH-TAX
           ELSE
           IF TP-TRCD = "RP"
              COMPUTE WORK-CAN-PP = WORK-CAN-PP +
                                ((TP-TRAMT - TP-INS-TAX-RB) * -1)
              MOVE "R" TO WORK-REFUND-FG-PP
              MOVE TP-PAYDATE TO WORK-CAN-PP-DATE
              MOVE TP-INS-TAX-RB TO WORK-CAN-PP-TAX
           ELSE
           IF TP-TRCD = "R1"
              COMPUTE WORK-CAN-O1 = WORK-CAN-O1 +
                                ((TP-TRAMT - TP-INS-TAX-RB) * -1)
              MOVE "R" TO WORK-REFUND-FG-O1
              MOVE TP-PAYDATE TO WORK-CAN-O1-DATE
              MOVE TP-INS-TAX-RB TO WORK-CAN-O1-TAX
           ELSE
           IF TP-TRCD = "R2"
              COMPUTE WORK-CAN-O2 = WORK-CAN-O2 +
                                ((TP-TRAMT - TP-INS-TAX-RB) * -1)
              MOVE "R" TO WORK-REFUND-FG-O2
              MOVE TP-PAYDATE TO WORK-CAN-O2-DATE
              MOVE TP-INS-TAX-RB TO WORK-CAN-O2-TAX
           ELSE
           IF TP-TRCD = "R3"
              COMPUTE WORK-CAN-O3 = WORK-CAN-O3 +
                                ((TP-TRAMT - TP-INS-TAX-RB) * -1)
              MOVE "R" TO WORK-REFUND-FG-O3
              MOVE TP-PAYDATE TO WORK-CAN-O3-DATE
              MOVE TP-INS-TAX-RB TO WORK-CAN-O3-TAX
           ELSE
           IF TP-TRCD = "CC"
              MOVE "C" TO WORK-CANCEL-FG-CL
              COMPUTE WORK-CAN-CL = WORK-CAN-CL +
                                ((TP-TRAMT - TP-INS-TAX-RB) * -1)
              MOVE TP-PAYDATE TO WORK-CAN-CL-DATE
              MOVE TP-INS-TAX-RB TO WORK-CAN-CL-TAX
           ELSE
           IF TP-TRCD = "CA"
              MOVE "C" TO WORK-CANCEL-FG-AH
              COMPUTE WORK-CAN-AH = WORK-CAN-AH +
                                ((TP-TRAMT - TP-INS-TAX-RB) * -1)
              MOVE TP-PAYDATE TO WORK-CAN-AH-DATE
              MOVE TP-INS-TAX-RB TO WORK-CAN-AH-TAX
           ELSE
           IF TP-TRCD = "CP"
              MOVE "C" TO WORK-CANCEL-FG-PP
              COMPUTE WORK-CAN-PP = WORK-CAN-PP +
                                ((TP-TRAMT - TP-INS-TAX-RB) * -1)
              MOVE TP-PAYDATE TO WORK-CAN-PP-DATE
              MOVE TP-INS-TAX-RB TO WORK-CAN-PP-TAX
           ELSE
           IF TP-TRCD = "C1"
              MOVE "C" TO WORK-CANCEL-FG-O1
              COMPUTE WORK-CAN-O1 = WORK-CAN-O1 +
                                ((TP-TRAMT - TP-INS-TAX-RB) * -1)
              MOVE TP-PAYDATE TO WORK-CAN-O1-DATE
              MOVE TP-INS-TAX-RB TO WORK-CAN-O1-TAX
           ELSE
           IF TP-TRCD = "C2"
              MOVE "C" TO WORK-CANCEL-FG-O2
              COMPUTE WORK-CAN-O2 = WORK-CAN-O2 +
                                ((TP-TRAMT - TP-INS-TAX-RB) * -1)
              MOVE TP-PAYDATE TO WORK-CAN-O2-DATE
              MOVE TP-INS-TAX-RB TO WORK-CAN-O2-TAX
           ELSE
           IF TP-TRCD = "C3"
              MOVE "C" TO WORK-CANCEL-FG-O3
              COMPUTE WORK-CAN-O3 = WORK-CAN-O3 +
                                ((TP-TRAMT - TP-INS-TAX-RB) * -1)
              MOVE TP-PAYDATE TO WORK-CAN-O3-DATE
              MOVE TP-INS-TAX-RB TO WORK-CAN-O3-TAX.

      * KY CITY AND COUNTY CODES STARTED CHANGING TO 4 DIGITS, DIDNT WANT
      * TO CHANGE BORROWER RECORDS !
           IF WORK-TAX-COUNTY-CODE = "600"
              MOVE "1000" TO WORK-TAX-COUNTY-CODE
           ELSE
           IF WORK-TAX-COUNTY-CODE = "601"
              MOVE "1001" TO WORK-TAX-COUNTY-CODE
           ELSE
           IF WORK-TAX-COUNTY-CODE = "605"
              MOVE "1005" TO WORK-TAX-COUNTY-CODE
           ELSE
           IF WORK-TAX-COUNTY-CODE = "606"
              MOVE "1006" TO WORK-TAX-COUNTY-CODE
           ELSE
           IF WORK-TAX-COUNTY-CODE = "607"
              MOVE "1007" TO WORK-TAX-COUNTY-CODE
           ELSE
           IF WORK-TAX-COUNTY-CODE = "613"
              MOVE "1013" TO WORK-TAX-COUNTY-CODE.

      * KY CITY AND COUNTY CODES STARTED CHANGING TO 4 DIGITS, DIDNT WANT
      * TO CHANGE BORROWER RECORDS !
           IF WORK-TAX-CITY-CODE = "502"
              MOVE "1002" TO WORK-TAX-CITY-CODE
           ELSE
           IF WORK-TAX-CITY-CODE = "503"
              MOVE "1003" TO WORK-TAX-CITY-CODE
           ELSE
           IF WORK-TAX-CITY-CODE = "504"
              MOVE "1004" TO WORK-TAX-CITY-CODE
           ELSE
           IF WORK-TAX-CITY-CODE = "508"
              MOVE "1008" TO WORK-TAX-CITY-CODE
           ELSE
           IF WORK-TAX-CITY-CODE = "509"
              MOVE "1009" TO WORK-TAX-CITY-CODE
           ELSE
           IF WORK-TAX-CITY-CODE = "510"
              MOVE "1010" TO WORK-TAX-CITY-CODE
           ELSE
           IF WORK-TAX-CITY-CODE = "511"
              MOVE "1011" TO WORK-TAX-CITY-CODE
           ELSE
           IF WORK-TAX-CITY-CODE = "512"
              MOVE "1012" TO WORK-TAX-CITY-CODE.

           MOVE LN-REC TO WORK-LN-REC.

           IF IO-FG = 0
              PERFORM REWRITE-WK-FILE
           ELSE
              PERFORM WRITE-WK-FILE.

           GO TO SORT-TRP-NEXT.

       SORT-TRP-EXIT.
           PERFORM CLOSE-TRP1-FILE.

      ******************************************************************
       SORT-TRL-FILE SECTION.
      ******************************************************************

           PERFORM OPEN-TRL1-FILE.

      *     MOVE BR-NO               TO TL-BRNO
      *                                 QTRL1-WBEG-BRNO
      *                                 QTRL1-WEND-BRNO.

           MOVE BEGINNING-DATE      TO TL-DATE SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO QTRL1-WBEG-DATE.
           MOVE RPT-END-DATE        TO  SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO QTRL1-WEND-DATE.

           MOVE BEG-LC              TO TL-CLASS
                                       QTRL1-WBEG-CLASS.
           MOVE END-LC              TO QTRL1-WEND-CLASS.

      *    MOVE 0                   TO TL-ACCTNO TL-LAWCODE TL-LINE
      *                                QTRL1-WBEG-ACCTNO
      *                                QTRL1-WBEG-LAWCODE
      *                                QTRL1-WBEG-LINE.
      *    MOVE ALL "9"             TO QTRL1-WEND-ACCTNO
      *                                QTRL1-WEND-LAWCODE
      *                                QTRL1-WEND-LINE.

           PERFORM START-TRL1-FILE.   *> SSP TRL LN

           MOVE 0 TO PROC-BRANCH-NO.

       SORT-TRL-NEXT.
           PERFORM READ-TRL1-FILE-NEXT.   *> EMBEDDED TRL LN
           IF (IO-FG NOT = 0)             *> OR (TRL-PATH-OWNBR NOT = TL-BRNO)
              GO TO SORT-TRL-EXIT.

           IF TL-DATE > RPT-END-DATE
              GO TO SORT-TRL-EXIT.

           IF TL-CLASS < BEG-LC OR TL-CLASS > END-LC
              GO TO SORT-TRL-NEXT.

           IF TL-BRNO NOT = PROC-BRANCH-NO
              MOVE TL-BRNO TO BR-NO PROC-BRANCH-NO
              PERFORM READ-BR-FILE
              IF IO-FG NOT = 0
                 GO TO IO-ERROR.

      * THIS CHECK ALSO IN STORED PROCEDURE 
           IF TL-INSCOMP(1) = 0 AND
              TL-INSCOMP(2) = 0 AND
              TL-INSCOMP(3) = 0 AND
              TL-INSCOMP(4) = 0 AND
              TL-INSCOMP(5) = 0 AND
              TL-INSCOMP(6) = 0 AND
              TL-FEECODE(1) = SPACES
                 GO TO SORT-TRL-NEXT.

           IF TL-INSCOMP(1) < BEG-IC
              IF TL-INSCOMP(2) < BEG-IC
                 IF TL-INSCOMP(3) < BEG-IC
                    IF TL-INSCOMP(4) < BEG-IC
                       IF TL-INSCOMP(5) < BEG-IC
                          IF TL-INSCOMP(6) < BEG-IC
                             IF TL-FEECODE(1) = SPACES
                                GO TO SORT-TRL-NEXT.
           IF TL-INSCOMP(1) > END-IC
              IF TL-INSCOMP(2) > END-IC
                 IF TL-INSCOMP(3) > END-IC
                    IF TL-INSCOMP(4) > END-IC
                       IF TL-INSCOMP(5) > END-IC
                          IF TL-INSCOMP(6) > END-IC
                             IF TL-FEECODE(1) = SPACES
                                GO TO SORT-TRL-NEXT.

      * LNFILE DATA INCLUDE IN STORED PROCEDURE
      *     MOVE TL-BRNO   TO LN-OWNBR.
      *     MOVE TL-ACCTNO TO LN-ACCTNO.
      *     PERFORM READ-LN1-FILE.
      *     IF IO-FG  NOT = 0
      *        GO TO SORT-TRL-NEXT.

           MOVE 0 TO WORK-INSCOMP.

           PERFORM FIND-TL-FEE
           IF TL-INSCOMP(1) NOT = 0 AND TL-INSOURS(1) = "Y"
              MOVE TL-INSCOMP(1) TO WORK-INSCOMP
           ELSE
             IF TL-INSCOMP(2) NOT = 0 AND TL-INSOURS(2) = "Y"
                MOVE TL-INSCOMP(2) TO WORK-INSCOMP
             ELSE
               IF TL-INSCOMP(3) NOT = 0 AND TL-INSOURS(3) = "Y"
                  MOVE TL-INSCOMP(3) TO WORK-INSCOMP
               ELSE
                 IF TL-INSCOMP(4) NOT = 0 AND TL-INSOURS(4) = "Y"
                    MOVE TL-INSCOMP(4) TO WORK-INSCOMP
                 ELSE
                   IF TL-INSCOMP(5) NOT = 0 AND TL-INSOURS(5) = "Y"
                      MOVE TL-INSCOMP(5) TO WORK-INSCOMP
                   ELSE
                   IF TL-INSCOMP(6) NOT = 0 AND TL-INSOURS(6) = "Y"
                      MOVE TL-INSCOMP(6) TO WORK-INSCOMP.


      * IF LN-INSCOMP = 0, THEN ADDON THEN "RV", JUST IGNORE IT
      *                  , OR NOT INSOURS
           IF WORK-INSCOMP = 0
              GO TO SORT-TRL-NEXT.

           MOVE TL-BRNO  TO WORK-OWNBR.
           MOVE TL-ACCTNO TO WORK-ACCTNO.
           PERFORM READ-WK-FILE.
           IF IO-FG NOT = 0
              PERFORM CLEAR-WK-FILE.

           IF (TL-INSOURS(1) = "Y") AND
              (TL-INSCOMP(1) NOT < BEG-IC AND NOT > END-IC)
              COMPUTE WORK-NEW-CL = WORK-NEW-CL + TL-INSPREM(1)
              ADD TC-FEE TO WORK-NEW-CL-TAX
           END-IF
           IF (TL-INSOURS(2) = "Y") AND
              (TL-INSCOMP(2) NOT < BEG-IC AND NOT > END-IC)
              COMPUTE WORK-NEW-AH = WORK-NEW-AH + TL-INSPREM(2)
              ADD TA-FEE TO WORK-NEW-AH-TAX
           END-IF
           IF (TL-INSOURS(3) = "Y") AND
              (TL-INSCOMP(3) NOT < BEG-IC AND NOT > END-IC)
              COMPUTE WORK-NEW-PP = WORK-NEW-PP + TL-INSPREM(3)
              ADD TP-FEE TO WORK-NEW-PP-TAX
           END-IF
           IF (TL-INSOURS(4) = "Y") AND
              (TL-INSCOMP(4) NOT < BEG-IC AND NOT > END-IC)
              COMPUTE WORK-NEW-O1 = WORK-NEW-O1 + TL-INSPREM(4)
              ADD T1-FEE TO WORK-NEW-O1-TAX
           END-IF
           IF (TL-INSOURS(5) = "Y") AND
              (TL-INSCOMP(5) NOT < BEG-IC AND NOT > END-IC)
              COMPUTE WORK-NEW-O2 = WORK-NEW-O2 + TL-INSPREM(5)
              ADD T2-FEE TO WORK-NEW-O2-TAX
           END-IF
           IF (TL-INSOURS(6) = "Y") AND
              (TL-INSCOMP(6) NOT < BEG-IC AND NOT > END-IC)
              COMPUTE WORK-NEW-O3 = WORK-NEW-O3 + TL-INSPREM(6)
              ADD T3-FEE TO WORK-NEW-O3-TAX
           END-IF
           COMPUTE WORK-NEW-AD-D = WORK-NEW-AD-D + HOLD-AD-D
           COMPUTE WORK-NEW-NON-FILE = WORK-NEW-NON-FILE
                                     + HOLD-NON-FILE
           MOVE HOLD-TAX-CITY-CODE TO WORK-TAX-CITY-CODE
           MOVE HOLD-TAX-COUNTY-CODE TO WORK-TAX-COUNTY-CODE.


      * KY CITY AND COUNTY CODES STARTED CHANGING TO 4 DIGITS, DIDNT WANT
      * TO CHANGE BORROWER RECORDS !
           IF WORK-TAX-COUNTY-CODE = "600"
              MOVE "1000" TO WORK-TAX-COUNTY-CODE
           ELSE
           IF WORK-TAX-COUNTY-CODE = "601"
              MOVE "1001" TO WORK-TAX-COUNTY-CODE
           ELSE
           IF WORK-TAX-COUNTY-CODE = "605"
              MOVE "1005" TO WORK-TAX-COUNTY-CODE
           ELSE
           IF WORK-TAX-COUNTY-CODE = "606"
              MOVE "1006" TO WORK-TAX-COUNTY-CODE
           ELSE
           IF WORK-TAX-COUNTY-CODE = "607"
              MOVE "1007" TO WORK-TAX-COUNTY-CODE
           ELSE
           IF WORK-TAX-COUNTY-CODE = "613"
              MOVE "1013" TO WORK-TAX-COUNTY-CODE.

      * KY CITY AND COUNTY CODES STARTED CHANGING TO 4 DIGITS, DIDNT WANT
      * TO CHANGE BORROWER RECORDS !
           IF WORK-TAX-CITY-CODE = "502"
              MOVE "1002" TO WORK-TAX-CITY-CODE
           ELSE
           IF WORK-TAX-CITY-CODE = "503"
              MOVE "1003" TO WORK-TAX-CITY-CODE
           ELSE
           IF WORK-TAX-CITY-CODE = "504"
              MOVE "1004" TO WORK-TAX-CITY-CODE
           ELSE
           IF WORK-TAX-CITY-CODE = "508"
              MOVE "1008" TO WORK-TAX-CITY-CODE
           ELSE
           IF WORK-TAX-CITY-CODE = "509"
              MOVE "1009" TO WORK-TAX-CITY-CODE
           ELSE
           IF WORK-TAX-CITY-CODE = "510"
              MOVE "1010" TO WORK-TAX-CITY-CODE
           ELSE
           IF WORK-TAX-CITY-CODE = "511"
              MOVE "1011" TO WORK-TAX-CITY-CODE
           ELSE
           IF WORK-TAX-CITY-CODE = "512"
              MOVE "1012" TO WORK-TAX-CITY-CODE.

           MOVE LN-REC TO WORK-LN-REC.

           IF IO-FG = 0
              PERFORM REWRITE-WK-FILE
           ELSE
              PERFORM WRITE-WK-FILE.

           GO TO SORT-TRL-NEXT.

       SORT-TRL-EXIT.
           PERFORM CLOSE-TRL1-FILE.

      ******************************************************************
       SORT-TRS-FILE SECTION.
      ******************************************************************

           PERFORM OPEN-TRS1-FILE.

      *     MOVE BR-NO               TO TS-BRNO
      *                                 QTRS1-WBEG-BRNO
      *                                 QTRS1-WEND-BRNO.

           MOVE BEGINNING-DATE      TO TS-DATE SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO QTRS1-WBEG-DATE.
           MOVE RPT-END-DATE        TO  SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO QTRS1-WEND-DATE.

           MOVE BEG-LC              TO TS-CLASS
                                       QTRS1-WBEG-CLASS.
           MOVE END-LC              TO QTRS1-WEND-CLASS.

      *     MOVE 0                   TO TS-ACCTNO TS-LAWCODE TS-LINE
      *                                 QTRS1-WBEG-ACCTNO
      *                                 QTRS1-WBEG-LAWCODE
      *                                 QTRS1-WBEG-LINE.
      *     MOVE ALL "9"             TO QTRS1-WEND-ACCTNO
      *                                 QTRS1-WEND-LAWCODE
      *                                 QTRS1-WEND-LINE.

           PERFORM START-TRS1-FILE.  *> SSP TRS LN

           MOVE 0 TO PROC-BRANCH-NO.

       SORT-TRS-NEXT.
           PERFORM READ-TRS1-FILE-NEXT.  *> EMBEDDED TRS LN
           IF (IO-FG NOT = 0)              *> OR (TRS-PATH-OWNBR NOT = TS-BRNO)
              GO TO SORT-TRS-EXIT.

           IF TS-DATE > RPT-END-DATE
              GO TO SORT-TRS-EXIT.

           IF TS-CLASS < BEG-LC OR TS-CLASS > END-LC
              GO TO SORT-TRS-NEXT.

           IF TS-BRNO NOT = PROC-BRANCH-NO
              MOVE TS-BRNO TO BR-NO PROC-BRANCH-NO
              PERFORM READ-BR-FILE
              IF IO-FG NOT = 0
                 GO TO IO-ERROR.

           IF TS-INSCOMP(1) = 0 AND
              TS-INSCOMP(2) = 0 AND
              TS-INSCOMP(3) = 0 AND
              TS-INSCOMP(4) = 0 AND
              TS-INSCOMP(5) = 0 AND
              TS-INSCOMP(6) = 0 AND
              TS-FEECODE(1) = SPACES
                 GO TO SORT-TRS-NEXT.

           IF TS-INSCOMP(1) < BEG-IC
              IF TS-INSCOMP(2) < BEG-IC
                 IF TS-INSCOMP(3) < BEG-IC
                    IF TS-INSCOMP(4) < BEG-IC
                       IF TS-INSCOMP(5) < BEG-IC
                          IF TS-INSCOMP(6) < BEG-IC
                             IF TS-FEECODE(1) = SPACES
                                   GO TO SORT-TRS-NEXT.

           IF TS-INSCOMP(1) > END-IC
              IF TS-INSCOMP(2) > END-IC
                 IF TS-INSCOMP(3) > END-IC
                    IF TS-INSCOMP(4) > END-IC
                       IF TS-INSCOMP(5) > END-IC
                          IF TS-INSCOMP(6) > END-IC
                             IF TS-FEECODE(1) = SPACES
                                GO TO SORT-TRS-NEXT.

      * LNFILE DATA INCLUDE IN STORED PROCEDURE 
      *     MOVE TS-BRNO   TO LN-OWNBR.
      *     MOVE TS-ACCTNO TO LN-ACCTNO.
      *     PERFORM READ-LN1-FILE.
      *     IF IO-FG  NOT = 0
      *        GO TO SORT-TRS-NEXT.

           MOVE 0 TO WORK-INSCOMP.

           PERFORM FIND-TS-FEE
           IF TS-INSCOMP(1) NOT = 0 AND TS-INSOURS(1) = "Y"
              MOVE TS-INSCOMP(1) TO WORK-INSCOMP
           ELSE
              IF TS-INSCOMP(2) NOT = 0 AND TS-INSOURS(2) = "Y"
                 MOVE TS-INSCOMP(2) TO WORK-INSCOMP
              ELSE
               IF TS-INSCOMP(3) NOT = 0 AND TS-INSOURS(3) = "Y"
                  MOVE TS-INSCOMP(3) TO WORK-INSCOMP
               ELSE
                 IF TS-INSCOMP(4) NOT = 0 AND TS-INSOURS(4) = "Y"
                    MOVE TS-INSCOMP(4) TO WORK-INSCOMP
                 ELSE
                    IF TS-INSCOMP(5) NOT = 0 AND TS-INSOURS(5) = "Y"
                       MOVE TS-INSCOMP(5) TO WORK-INSCOMP
                    ELSE
                     IF TS-INSCOMP(6) NOT = 0 AND TS-INSOURS(6) = "Y"
                        MOVE TS-INSCOMP(6) TO WORK-INSCOMP.

      * IF LN-INSCOMP = 0, THEN ADDON THEN "RV", JUST IGNORE IT
      *                  , OR NOT INSOURS
           IF WORK-INSCOMP = 0
              GO TO SORT-TRS-NEXT.

           MOVE TS-BRNO  TO WORK-OWNBR.
           MOVE TS-ACCTNO TO WORK-ACCTNO.
           PERFORM READ-WK-FILE.
           IF IO-FG NOT = 0
              PERFORM CLEAR-WK-FILE.

           IF (TS-INSOURS(1) = "Y") AND
              (TS-INSCOMP(1) NOT < BEG-IC AND NOT > END-IC)
              COMPUTE WORK-NEW-CL = WORK-NEW-CL + TS-INSPREM(1)
              ADD TC-FEE TO WORK-NEW-CL-TAX
           END-IF
           IF (TS-INSOURS(2) = "Y") AND
              (TS-INSCOMP(2) NOT < BEG-IC AND NOT > END-IC)
              COMPUTE WORK-NEW-AH = WORK-NEW-AH + TS-INSPREM(2)
              ADD TA-FEE TO WORK-NEW-AH-TAX
           END-IF
           IF (TS-INSOURS(3) = "Y") AND
              (TS-INSCOMP(3) NOT < BEG-IC AND NOT > END-IC)
              COMPUTE WORK-NEW-PP = WORK-NEW-PP + TS-INSPREM(3)
              ADD TP-FEE TO WORK-NEW-PP-TAX
           END-IF
           IF (TS-INSOURS(4) = "Y") AND
              (TS-INSCOMP(4) NOT < BEG-IC AND NOT > END-IC)
              COMPUTE WORK-NEW-O1 = WORK-NEW-O1 + TS-INSPREM(4)
              ADD T1-FEE TO WORK-NEW-O1-TAX
           END-IF
           IF (TS-INSOURS(5) = "Y") AND
              (TS-INSCOMP(5) NOT < BEG-IC AND NOT > END-IC)
              COMPUTE WORK-NEW-O2 = WORK-NEW-O2 + TS-INSPREM(5)
              ADD T2-FEE TO WORK-NEW-O2-TAX
           END-IF
           IF (TS-INSOURS(6) = "Y") AND
              (TS-INSCOMP(6) NOT < BEG-IC AND NOT > END-IC)
              COMPUTE WORK-NEW-O3 = WORK-NEW-O3 + TS-INSPREM(6)
              ADD T3-FEE TO WORK-NEW-O3-TAX
           END-IF
           COMPUTE WORK-NEW-AD-D = WORK-NEW-AD-D + HOLD-AD-D
           COMPUTE WORK-NEW-NON-FILE = WORK-NEW-NON-FILE
                                     + HOLD-NON-FILE
           MOVE HOLD-TAX-CITY-CODE TO WORK-TAX-CITY-CODE
           MOVE HOLD-TAX-COUNTY-CODE TO WORK-TAX-COUNTY-CODE.


      * KY CITY AND COUNTY CODES STARTED CHANGING TO 4 DIGITS, DIDNT WANT
      * TO CHANGE BORROWER RECORDS !
           IF WORK-TAX-COUNTY-CODE = "600"
              MOVE "1000" TO WORK-TAX-COUNTY-CODE
           ELSE
           IF WORK-TAX-COUNTY-CODE = "601"
              MOVE "1001" TO WORK-TAX-COUNTY-CODE
           ELSE
           IF WORK-TAX-COUNTY-CODE = "605"
              MOVE "1005" TO WORK-TAX-COUNTY-CODE
           ELSE
           IF WORK-TAX-COUNTY-CODE = "606"
              MOVE "1006" TO WORK-TAX-COUNTY-CODE
           ELSE
           IF WORK-TAX-COUNTY-CODE = "607"
              MOVE "1007" TO WORK-TAX-COUNTY-CODE
           ELSE
           IF WORK-TAX-COUNTY-CODE = "613"
              MOVE "1013" TO WORK-TAX-COUNTY-CODE.

      * KY CITY AND COUNTY CODES STARTED CHANGING TO 4 DIGITS, DIDNT WANT
      * TO CHANGE BORROWER RECORDS !
           IF WORK-TAX-CITY-CODE = "502"
              MOVE "1002" TO WORK-TAX-CITY-CODE
           ELSE
           IF WORK-TAX-CITY-CODE = "503"
              MOVE "1003" TO WORK-TAX-CITY-CODE
           ELSE
           IF WORK-TAX-CITY-CODE = "504"
              MOVE "1004" TO WORK-TAX-CITY-CODE
           ELSE
           IF WORK-TAX-CITY-CODE = "508"
              MOVE "1008" TO WORK-TAX-CITY-CODE
           ELSE
           IF WORK-TAX-CITY-CODE = "509"
              MOVE "1009" TO WORK-TAX-CITY-CODE
           ELSE
           IF WORK-TAX-CITY-CODE = "510"
              MOVE "1010" TO WORK-TAX-CITY-CODE
           ELSE
           IF WORK-TAX-CITY-CODE = "511"
              MOVE "1011" TO WORK-TAX-CITY-CODE
           ELSE
           IF WORK-TAX-CITY-CODE = "512"
              MOVE "1012" TO WORK-TAX-CITY-CODE.

           MOVE LN-REC TO WORK-LN-REC.

           IF IO-FG = 0
              PERFORM REWRITE-WK-FILE
           ELSE
              PERFORM WRITE-WK-FILE.

           GO TO SORT-TRS-NEXT.

       SORT-TRS-EXIT.
           PERFORM CLOSE-TRS1-FILE.

      ****************************
       COMPUTE-SURCHARGE SECTION.
      ****************************
      * 1. "TP" FEE AMOUNT DIVIDED BY PP INSURANCE AMOUNT EQUALS %
      * 2. DIVIDE 1.5% BY THE % COMPUTED IN STEP 1 WHICH WILL GIVE YOU WHAT
      *    PERCENT OF THE ORIGINAL TOTAL TAX WAS TAX AND WHAT WAS SURCHARGE
      * 3. MULTIPLY THE % FROM STEP 2 TIMES THE INSURANCE TAX REBATE REFUND
      *    AMOUNT IN THE "RP" TRAN RECORD - THIS WILL BE THE SURCHARGE REBATE
      * 4. SUBTRACT THE SURCHARGE REBATE FROM THE INSURANCE TAX REBATE REFUND
      *    AMOUNT AND THIS WILL BE THE TAX REBATE AMOUNT

      * MUST FIRST FIND THE ORIGINAL TAX AMOUNT, "TP"
           MOVE 1 TO SURCHG-SUB.
           MOVE 0 TO SURCHARGE-REBATE TAX-RATE SURCHARGE-PERCENT.

       FIND-TAX-FEECODE.
           IF SURCHG-SUB > 10
              GO TO EXIT-COMPUTE-SURCHARGE.

           IF LN-FEECODE(SURCHG-SUB) = SPACES
              ADD 1 TO SURCHG-SUB
              GO TO FIND-TAX-FEECODE.

           IF LN-FEECODE(SURCHG-SUB) = "TP"
              COMPUTE TAX-RATE ROUNDED =
                 ( LN-FEEAMT(SURCHG-SUB) / LN-INSPREM(3) )
              COMPUTE SURCHARGE-PERCENT ROUNDED =
                  ( ( 1.5 / TAX-RATE ) / 100  )
              COMPUTE SURCHARGE-REBATE ROUNDED =
                 ( SURCHARGE-PERCENT * WORK-CAN-PP-TAX )
              GO TO EXIT-COMPUTE-SURCHARGE.

           ADD 1 TO SURCHG-SUB.
           GO TO FIND-TAX-FEECODE.

       EXIT-COMPUTE-SURCHARGE.
           EXIT.

      **********************
       FIND-TS-FEE SECTION.
      **********************
           MOVE 1 TO SUB.
           MOVE 0 TO HOLD-AD-D HOLD-NON-FILE
                     HOLD-TAX-CITY-CODE HOLD-TAX-COUNTY-CODE
                     TC-FEE TA-FEE TP-FEE T1-FEE T2-FEE T3-FEE.
       NEXT-TS-FEE.
           IF SUB > 10
              GO TO EXIT-TS-FEE.
           IF TS-FEECODE(SUB) = SPACES
              ADD 1 TO SUB
              GO TO NEXT-TS-FEE.

           MOVE TS-INS-TAX-CITY-CD TO HOLD-TAX-CITY-CODE.
           MOVE TS-INS-TAX-COUNTY-CD TO HOLD-TAX-COUNTY-CODE.

           IF TS-FEECODE(SUB) = BR-ADD-INS-FEECODE
              MOVE 01 TO WORK-INSCOMP
              MOVE TS-FEEAMT(SUB) TO HOLD-AD-D
           ELSE
           IF TS-FEECODE(SUB) = "TC"
              MOVE TS-FEEAMT(SUB) TO TC-FEE
           ELSE
           IF TS-FEECODE(SUB) = "TA"
              MOVE TS-FEEAMT(SUB) TO TA-FEE
           ELSE
           IF TS-FEECODE(SUB) = "TP"
              MOVE TS-FEEAMT(SUB) TO TP-FEE
           ELSE
           IF TS-FEECODE(SUB) = "T1"
              MOVE TS-FEEAMT(SUB) TO T1-FEE
           ELSE
           IF TS-FEECODE(SUB) = "T2"
              MOVE TS-FEEAMT(SUB) TO T2-FEE
           ELSE
           IF TS-FEECODE(SUB) = "T3"
              MOVE TS-FEEAMT(SUB) TO T3-FEE.

           IF TS-FEECODE(SUB) = "90"
              MOVE 01 TO WORK-INSCOMP
              MOVE TS-FEEAMT(SUB) TO HOLD-NON-FILE.

           ADD 1 TO SUB.
           GO TO NEXT-TS-FEE.
       EXIT-TS-FEE.
           EXIT.

      **********************
       FIND-TL-FEE SECTION.
      **********************
           MOVE 1 TO SUB.
           MOVE 0 TO HOLD-AD-D HOLD-NON-FILE TC-FEE
                     HOLD-TAX-CITY-CODE HOLD-TAX-COUNTY-CODE
                     TC-FEE TA-FEE TP-FEE T1-FEE T2-FEE T3-FEE.
       NEXT-TL-FEE.
           IF SUB > 10
              GO TO EXIT-TL-FEE.
           IF TL-FEECODE(SUB) = SPACES
              ADD 1 TO SUB
              GO TO NEXT-TL-FEE.

           MOVE TL-INS-TAX-CITY-CD TO HOLD-TAX-CITY-CODE.
           MOVE TL-INS-TAX-COUNTY-CD TO HOLD-TAX-COUNTY-CODE.

           IF TL-FEECODE(SUB) = BR-ADD-INS-FEECODE
              MOVE 01 TO WORK-INSCOMP
              MOVE TL-FEEAMT(SUB) TO HOLD-AD-D
           ELSE
           IF TL-FEECODE(SUB) = "TC"
              MOVE TL-FEEAMT(SUB) TO TC-FEE
           ELSE
           IF TL-FEECODE(SUB) = "TA"
              MOVE TL-FEEAMT(SUB) TO TA-FEE
           ELSE
           IF TL-FEECODE(SUB) = "TP"
              MOVE TL-FEEAMT(SUB) TO TP-FEE
           ELSE
           IF TL-FEECODE(SUB) = "T1"
              MOVE TL-FEEAMT(SUB) TO T1-FEE
           ELSE
           IF TL-FEECODE(SUB) = "T2"
              MOVE TL-FEEAMT(SUB) TO T2-FEE
           ELSE
           IF TL-FEECODE(SUB) = "T3"
              MOVE TL-FEEAMT(SUB) TO T3-FEE.

           IF TL-FEECODE(SUB) = "90"
              MOVE 01 TO WORK-INSCOMP
              MOVE TL-FEEAMT(SUB) TO HOLD-NON-FILE.

           ADD 1 TO SUB.
           GO TO NEXT-TL-FEE.
       EXIT-TL-FEE.
           EXIT.

      ****************************
       KEY-DETAIL SECTION.
      ****************************

           MOVE SPACES TO PFTI-REC PFTC-REC.

           MOVE LN-ACCTNO TO PFTI-CERT-NBR PFTC-CERT-NBR.
           MOVE LN-OWNBR  TO PFTI-PRODUCER PFTC-PRODUCER.
           MOVE LN-CLASS  TO PFTI-CLASS    PFTC-CLASS.
           MOVE 0         TO PFTI-INSUROR  PFTC-INSUROR.

           IF LN-INSURED = "S"
              MOVE LN-SSNO(2) TO BW-SSNO
           ELSE
              MOVE LN-SSNO(1) TO BW-SSNO.
           PERFORM SETUP-INSURED.

           IF WORK-NEW-CL = 0 AND WORK-NEW-AH = 0 AND
              WORK-NEW-PP = 0 AND WORK-NEW-O1 = 0 AND
              WORK-NEW-O2 = 0 AND WORK-NEW-O3 AND
              WORK-NEW-AD-D = 0 AND WORK-NEW-NON-FILE = 0
              GO TO KEY-CANCEL.

      * CREATE ISSUE
           MOVE "I" TO PFTI-TYPE.

           MOVE PRIMARY-SSNO  TO PFTI-PRI-SSN.
           MOVE PRIMARY-LNAME TO PFTI-PRI-LNAME.
           MOVE PRIMARY-FNAME TO PFTI-PRI-FNAME.
           MOVE PRIMARY-AGE   TO PFTI-PRI-AGE.
           MOVE PRIMARY-BDATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY   TO PFTI-PRI-BIRTH.
           MOVE PRIMARY-ADR1 TO PFTI-PRI-ADR1.
           MOVE PRIMARY-ADR2 TO PFTI-PRI-ADR2.
           MOVE PRIMARY-CITY TO PFTI-PRI-CITY.
           MOVE PRIMARY-STATE TO PFTI-PRI-STATE.
           MOVE PRIMARY-ZIP TO PFTI-PRI-ZIP.

           MOVE JOINT-LNAME TO PFTI-SEC-LNAME.
           MOVE JOINT-FNAME TO PFTI-SEC-FNAME.
           MOVE JOINT-AGE TO PFTI-SEC-AGE.
           MOVE JOINT-BDATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO PFTI-SEC-BIRTH.
           MOVE JOINT-SSNO TO PFTI-SEC-SSN.

           MOVE LN-ORGTERM TO PFTI-LOAN-TERM.
           MOVE LN-APRATE TO PFTI-APR.

      * ALWAYS FILL IN THE TABLE STARTING FROM 1
           MOVE 1 TO SUB TAX-SUB.

           IF WORK-NEW-CL NOT = 0
              ADD 1 TO NEW-LIFE-COUNT
              ADD WORK-NEW-CL TO NEW-LIFE-AMT
              MOVE LN-INSCOVR(1) TO PFTI-ISS-BEN(1)
              MOVE WORK-NEW-CL TO PFTI-ISS-PRM(1)
              MOVE 1 TO ITRM-SUB
              PERFORM INS-TERM-CALCULATION
              MOVE ITRM-INSTERM TO PFTI-ISS-TERM(1)
              MOVE LN-INSEXP-DATE(1) TO DATE-YYYYMMDD
              PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
              MOVE DATE-MMDDYY TO PFTI-ISS-EXP(1)
              MOVE LN-INSEFF-DATE(1) TO DATE-YYYYMMDD
              PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
              MOVE DATE-MMDDYY TO PFTI-EFF-DATE
              MOVE LN-INSCOMP(1) TO PFTI-INSUROR
              MOVE 2 TO SUB
              IF LN-INSTYPES-LL
                 MOVE "LLIF" TO PFTI-ISS-COV-CD(1)
              ELSE
                 MOVE "DLIF" TO PFTI-ISS-COV-CD(1)
              END-IF
              IF WORK-NEW-CL-TAX NOT = 0
                 MOVE "TC" TO PFTI-ISS-TAX-CODE(TAX-SUB)
                 COMPUTE PFTI-ISS-SURCHG(TAX-SUB) ROUNDED =
                               ( LN-INSPREM(1) * .018 )
                 COMPUTE PFTI-ISS-TAX-AMT(TAX-SUB) ROUNDED =
                      WORK-NEW-CL-TAX - PFTI-ISS-SURCHG(TAX-SUB)
                 ADD 1 TO TAX-SUB
                 MOVE WORK-TAX-CITY-CODE TO PFTI-TAX-CITY-CODE
                 MOVE WORK-TAX-COUNTY-CODE TO PFTI-TAX-COUNTY-CODE.


           IF WORK-NEW-AH NOT = 0
              ADD 1 TO NEW-AH-COUNT
              ADD WORK-NEW-AH TO NEW-AH-AMT
              MOVE "A&H " TO PFTI-ISS-COV-CD(SUB)
              COMPUTE PFTI-ISS-BEN(SUB) = LN-REGPYAMT - LN-MAINTFEE
              MOVE WORK-NEW-AH TO PFTI-ISS-PRM(SUB)
              MOVE LN-INSCOMP(2) TO PFTI-INSUROR
              MOVE 2 TO ITRM-SUB
              PERFORM INS-TERM-CALCULATION
              MOVE ITRM-INSTERM TO PFTI-ISS-TERM(SUB)
              MOVE LN-INSEXP-DATE(2) TO DATE-YYYYMMDD
              PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
              MOVE DATE-MMDDYY TO PFTI-ISS-EXP(SUB)
              MOVE LN-INSEFF-DATE(2) TO DATE-YYYYMMDD
              PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
              MOVE DATE-MMDDYY TO PFTI-EFF-DATE
              ADD 1 TO SUB
              IF WORK-NEW-AH-TAX NOT = 0
                 MOVE "TA" TO PFTI-ISS-TAX-CODE(TAX-SUB)
                 COMPUTE PFTI-ISS-SURCHG(TAX-SUB) ROUNDED =
                                 ( LN-INSPREM(2) * .018 )
                 COMPUTE PFTI-ISS-TAX-AMT(TAX-SUB) ROUNDED =
                      WORK-NEW-AH-TAX - PFTI-ISS-SURCHG(TAX-SUB)
                 ADD 1 TO TAX-SUB
                 MOVE WORK-TAX-CITY-CODE TO PFTI-TAX-CITY-CODE
                 MOVE WORK-TAX-COUNTY-CODE TO PFTI-TAX-COUNTY-CODE.

           IF WORK-NEW-PP NOT = 0
              ADD 1 TO NEW-PP-COUNT
              ADD WORK-NEW-PP TO NEW-PP-AMT
              MOVE LN-INSCOVR(3) TO PFTI-ISS-BEN(SUB)
              MOVE WORK-NEW-PP TO PFTI-ISS-PRM(SUB)
              MOVE LN-INSCOMP(3) TO PFTI-INSUROR
              MOVE 3 TO ITRM-SUB
              PERFORM INS-TERM-CALCULATION
              MOVE ITRM-INSTERM TO PFTI-ISS-TERM(SUB)
              MOVE LN-INSEXP-DATE(3) TO DATE-YYYYMMDD
              PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
              MOVE DATE-MMDDYY TO PFTI-ISS-EXP(SUB)
              MOVE LN-INSEFF-DATE(3) TO DATE-YYYYMMDD
              PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
              MOVE DATE-MMDDYY TO PFTI-EFF-DATE
              IF LN-INSTYPES-PP2
                 MOVE "DPRO" TO PFTI-ISS-COV-CD(SUB)
              ELSE
                 MOVE "SPRO" TO PFTI-ISS-COV-CD(SUB)
              END-IF
              ADD 1 TO SUB
              IF WORK-NEW-PP-TAX NOT = 0
                 MOVE "TP" TO PFTI-ISS-TAX-CODE(TAX-SUB)
                 MOVE WORK-TAX-CITY-CODE TO PFTI-TAX-CITY-CODE
                 MOVE WORK-TAX-COUNTY-CODE TO PFTI-TAX-COUNTY-CODE
                 COMPUTE PFTI-ISS-SURCHG(TAX-SUB) ROUNDED =
                                    ( LN-INSPREM(3) * .018 )
                 IF WORK-NEW-PP-TAX < 0
                    COMPUTE PFTI-ISS-SURCHG(TAX-SUB) =
                            PFTI-ISS-SURCHG(TAX-SUB) * -1
                 END-IF
                 COMPUTE PFTI-ISS-TAX-AMT(TAX-SUB) ROUNDED =
                      WORK-NEW-PP-TAX - PFTI-ISS-SURCHG(TAX-SUB)
                 ADD 1 TO TAX-SUB.


           IF WORK-NEW-O1 NOT = 0
              MOVE LN-INSCOVR(4) TO PFTI-ISS-BEN(SUB)
              MOVE WORK-NEW-O1 TO PFTI-ISS-PRM(SUB)
              MOVE LN-INSCOMP(4) TO PFTI-INSUROR
              MOVE 4 TO ITRM-SUB
              PERFORM INS-TERM-CALCULATION
              MOVE ITRM-INSTERM TO PFTI-ISS-TERM(SUB)
              MOVE LN-INSEXP-DATE(4) TO DATE-YYYYMMDD
              PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
              MOVE DATE-MMDDYY TO PFTI-ISS-EXP(SUB)
              MOVE LN-INSEFF-DATE(4) TO DATE-YYYYMMDD
              PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
              MOVE DATE-MMDDYY TO PFTI-EFF-DATE
              MOVE "OTH1" TO PFTI-ISS-COV-CD(SUB)
              ADD 1 TO NEW-AUTO-COUNT
              ADD 1 TO SUB
              ADD WORK-NEW-O1 TO NEW-AUTO-AMT
              IF WORK-NEW-O1-TAX NOT = 0
                 MOVE "T1" TO PFTI-ISS-TAX-CODE(TAX-SUB)
                 MOVE WORK-TAX-CITY-CODE TO PFTI-TAX-CITY-CODE
                 MOVE WORK-TAX-COUNTY-CODE TO PFTI-TAX-COUNTY-CODE
                 COMPUTE PFTI-ISS-SURCHG(TAX-SUB) ROUNDED =
                                 ( LN-INSPREM(4) * .018 )
                 IF WORK-NEW-O1-TAX < 0
                    COMPUTE PFTI-ISS-SURCHG(TAX-SUB) =
                            PFTI-ISS-SURCHG(TAX-SUB) * -1
                 END-IF
                 COMPUTE PFTI-ISS-TAX-AMT(TAX-SUB) ROUNDED =
                   WORK-NEW-O1-TAX - PFTI-ISS-SURCHG(TAX-SUB)
                 ADD 1 TO TAX-SUB.

           IF WORK-NEW-O2 NOT = 0
              MOVE "N" TO O2-WRITTEN
              IF SUB NOT > 4
                 MOVE LN-INSCOVR(5) TO PFTI-ISS-BEN(SUB)
                 MOVE WORK-NEW-O2 TO PFTI-ISS-PRM(SUB)
                 MOVE LN-INSCOMP(5) TO PFTI-INSUROR
                 MOVE 5 TO ITRM-SUB
                 PERFORM INS-TERM-CALCULATION
                 MOVE ITRM-INSTERM TO PFTI-ISS-TERM(SUB)
                 MOVE LN-INSEXP-DATE(5) TO DATE-YYYYMMDD
                 PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
                 MOVE DATE-MMDDYY TO PFTI-ISS-EXP(SUB)
                 MOVE LN-INSEFF-DATE(5) TO DATE-YYYYMMDD
                 PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
                 MOVE DATE-MMDDYY TO PFTI-EFF-DATE
                 MOVE "OTH2" TO PFTI-ISS-COV-CD(SUB)
                 ADD 1 TO NEW-O2-COUNT
                 MOVE "Y" TO O2-WRITTEN
                 ADD 1 TO SUB
                 ADD WORK-NEW-O2 TO NEW-O2-AMT
                 IF WORK-NEW-O2-TAX NOT = 0
                    MOVE "T2" TO PFTI-ISS-TAX-CODE(TAX-SUB)
                    MOVE WORK-TAX-CITY-CODE TO PFTI-TAX-CITY-CODE
                    MOVE WORK-TAX-COUNTY-CODE TO
                                             PFTI-TAX-COUNTY-CODE
                    COMPUTE PFTI-ISS-SURCHG(TAX-SUB) ROUNDED =
                                    ( LN-INSPREM(5) * .018 )
                    IF WORK-NEW-O2-TAX < 0
                       COMPUTE PFTI-ISS-SURCHG(TAX-SUB) =
                               PFTI-ISS-SURCHG(TAX-SUB) * -1
                    END-IF
                    COMPUTE PFTI-ISS-TAX-AMT(TAX-SUB) ROUNDED =
                      WORK-NEW-O2-TAX - PFTI-ISS-SURCHG(TAX-SUB)
                    ADD 1 TO TAX-SUB.

           IF WORK-NEW-O3 NOT = 0
              MOVE "N" TO O3-WRITTEN
              IF SUB NOT > 4
                 MOVE LN-INSCOVR(6) TO PFTI-ISS-BEN(SUB)
                 MOVE WORK-NEW-O3 TO PFTI-ISS-PRM(SUB)
                 MOVE LN-INSCOMP(6) TO PFTI-INSUROR
                 MOVE 6 TO ITRM-SUB
                 PERFORM INS-TERM-CALCULATION
                 MOVE ITRM-INSTERM TO PFTI-ISS-TERM(SUB)
                 MOVE LN-INSEXP-DATE(6) TO DATE-YYYYMMDD
                 PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
                 MOVE DATE-MMDDYY TO PFTI-ISS-EXP(SUB)
                 MOVE LN-INSEFF-DATE(6) TO DATE-YYYYMMDD
                 PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
                 MOVE DATE-MMDDYY TO PFTI-EFF-DATE
                 MOVE "OTH3" TO PFTI-ISS-COV-CD(SUB)
                 ADD 1 TO NEW-O3-COUNT
                 MOVE "Y" TO O3-WRITTEN
                 ADD 1 TO SUB
                 ADD WORK-NEW-O3 TO NEW-O3-AMT
                 IF WORK-NEW-O3-TAX NOT = 0
                    MOVE "T3" TO PFTI-ISS-TAX-CODE(TAX-SUB)
                    MOVE WORK-TAX-CITY-CODE TO PFTI-TAX-CITY-CODE
                    MOVE WORK-TAX-COUNTY-CODE
                                            TO PFTI-TAX-COUNTY-CODE
                    COMPUTE PFTI-ISS-SURCHG(TAX-SUB) ROUNDED =
                                    ( LN-INSPREM(6) * .018 )
                    IF WORK-NEW-O3-TAX < 0
                        COMPUTE PFTI-ISS-SURCHG(TAX-SUB) =
                               PFTI-ISS-SURCHG(TAX-SUB) * -1
                    END-IF
                    COMPUTE PFTI-ISS-TAX-AMT(TAX-SUB) ROUNDED =
                       WORK-NEW-O3-TAX - PFTI-ISS-SURCHG(TAX-SUB)
                    ADD 1 TO TAX-SUB.

           IF WORK-NEW-AD-D NOT = 0
              MOVE "N" TO AD-D-WRITTEN
              IF SUB NOT > 4
                 MOVE LN-AD-D-INS-COVR TO PFTI-ISS-BEN(SUB)
                 MOVE WORK-NEW-AD-D TO PFTI-ISS-PRM(SUB)
                 MOVE 12 TO PFTI-ISS-TERM(SUB)
                 MOVE LN-LOANDATE TO DATE-YYYYMMDD NDTE-DATE
                 PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
                 MOVE DATE-MMDDYY TO PFTI-EFF-DATE 
                 MOVE 12 TO NDTE-HOLD
                 PERFORM INCREMENT-MONTHS
                 MOVE NDTE-DATE TO DATE-YYYYMMDD
                 PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
                 MOVE DATE-MMDDYY TO PFTI-ISS-EXP(SUB)
                 MOVE "AD&D" TO PFTI-ISS-COV-CD(SUB)
                 ADD 1 TO NEW-AD-D-COUNT
                 ADD WORK-NEW-AD-D TO NEW-AD-D-AMT
                 MOVE "Y" TO AD-D-WRITTEN
                 ADD 1 TO SUB.

           IF WORK-NEW-NON-FILE NOT = 0
              MOVE "N" TO NON-FILE-WRITTEN
              IF SUB NOT > 4
                 MOVE LN-LNAMT TO PFTI-ISS-BEN(SUB)
                 MOVE WORK-NEW-NON-FILE TO PFTI-ISS-PRM(SUB)
                 MOVE LN-ORGTERM TO PFTI-ISS-TERM(SUB)
                 MOVE LN-LOANDATE TO DATE-YYYYMMDD NDTE-DATE
                 PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
                 MOVE DATE-MMDDYY TO PFTI-EFF-DATE 
                 MOVE LN-ORGTERM TO NDTE-HOLD
                 PERFORM INCREMENT-MONTHS
                 MOVE NDTE-DATE TO DATE-YYYYMMDD
                 PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
                 MOVE DATE-MMDDYY TO PFTI-ISS-EXP(SUB)
                 MOVE "NONF" TO PFTI-ISS-COV-CD(SUB)
                 ADD 1 TO NEW-NON-FILE-COUNT
                 ADD WORK-NEW-NON-FILE TO NEW-NON-FILE-AMT
                 MOVE "Y" TO NON-FILE-WRITTEN
                 ADD 1 TO SUB.

       KEY-WRITE-ISSUE.

           ADD 1 TO PFTI-KEY.
           PERFORM WRITE-PFTI-FILE.
           ADD 1 TO TOTAL-I-RECORDS.

           MOVE 1 TO SUB.

      * CREATE A WHOLE NEW ISSUE RECORD, IF O2 OR O3 WERE WRITTEN, AND
      * ALL 4 INSURANCE SLOTS HAVE ALREADY BEEN FILLED
           IF WORK-NEW-O2 NOT = 0
              IF O2-WRITTEN = "N"
                 PERFORM CREATE-NEW-PFTI
                 MOVE LN-INSCOVR(5) TO PFTI-ISS-BEN(SUB)
                 MOVE WORK-NEW-O2 TO PFTI-ISS-PRM(SUB)
                 MOVE LN-INSCOMP(5) TO PFTI-INSUROR
                 MOVE 5 TO ITRM-SUB
                 PERFORM INS-TERM-CALCULATION
                 MOVE ITRM-INSTERM TO PFTI-ISS-TERM(SUB)
                 MOVE LN-INSEXP-DATE(5) TO DATE-YYYYMMDD
                 PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
                 MOVE DATE-MMDDYY TO PFTI-ISS-EXP(SUB)
                 MOVE LN-INSEFF-DATE(5) TO DATE-YYYYMMDD
                 PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
                 MOVE DATE-MMDDYY TO PFTI-EFF-DATE
                 MOVE "OTH2" TO PFTI-ISS-COV-CD(SUB)
                 ADD 1 TO NEW-O2-COUNT
                 ADD 1 TO SUB
                 ADD WORK-NEW-O2 TO NEW-O2-AMT.
                 IF WORK-NEW-O2-TAX NOT = 0
                    MOVE "T2" TO PFTI-ISS-TAX-CODE(TAX-SUB)
                    COMPUTE PFTI-ISS-SURCHG(TAX-SUB) ROUNDED =
                                 ( LN-INSPREM(5) * .018 )
                    COMPUTE PFTI-ISS-TAX-AMT(TAX-SUB) ROUNDED =
                       WORK-NEW-O2-TAX - PFTI-ISS-SURCHG(TAX-SUB)
                    ADD 1 TO TAX-SUB
                    MOVE WORK-TAX-CITY-CODE TO PFTI-TAX-CITY-CODE
                    MOVE WORK-TAX-COUNTY-CODE
                                            TO PFTI-TAX-COUNTY-CODE.

      * FIRST TEST IF SUB > 1, WHICH MEANS A NEW RECORD WAS
      * ALREADY CREATED
           IF WORK-NEW-O3 NOT = 0
              IF O3-WRITTEN = "N"
                 IF SUB NOT  > 1
                    PERFORM CREATE-NEW-PFTI
                 END-IF
                 MOVE LN-INSCOVR(6) TO PFTI-ISS-BEN(SUB)
                 MOVE WORK-NEW-O3 TO PFTI-ISS-PRM(SUB)
                 MOVE LN-INSCOMP(6) TO PFTI-INSUROR
                 MOVE 6 TO ITRM-SUB
                 PERFORM INS-TERM-CALCULATION
                 MOVE ITRM-INSTERM TO PFTI-ISS-TERM(SUB)
                 MOVE LN-INSEXP-DATE(6) TO DATE-YYYYMMDD
                 PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
                 MOVE DATE-MMDDYY TO PFTI-ISS-EXP(SUB)
                 MOVE LN-INSEFF-DATE(6) TO DATE-YYYYMMDD
                 PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
                 MOVE DATE-MMDDYY TO PFTI-EFF-DATE
                 MOVE "OTH3" TO PFTI-ISS-COV-CD(SUB)
                 ADD 1 TO NEW-O3-COUNT
                 ADD 1 TO SUB
                 ADD WORK-NEW-O3 TO NEW-O3-AMT
                 IF WORK-NEW-O3-TAX NOT = 0
                    MOVE "T3" TO PFTI-ISS-TAX-CODE(TAX-SUB)
                    COMPUTE PFTI-ISS-SURCHG(TAX-SUB) ROUNDED =
                              ( LN-INSPREM(6) * .018 )
                    COMPUTE PFTI-ISS-TAX-AMT(TAX-SUB) ROUNDED =
                      WORK-NEW-O3-TAX - PFTI-ISS-SURCHG(TAX-SUB)
                    ADD 1 TO TAX-SUB
                    MOVE WORK-TAX-CITY-CODE TO PFTI-TAX-CITY-CODE
                    MOVE WORK-TAX-COUNTY-CODE
                                            TO PFTI-TAX-COUNTY-CODE.

           IF WORK-NEW-AD-D NOT = 0
              IF AD-D-WRITTEN = "N"
                 IF SUB NOT  > 1
                    PERFORM CREATE-NEW-PFTI
                 END-IF
                 MOVE LN-AD-D-INS-COVR TO PFTI-ISS-BEN(SUB)
                 MOVE WORK-NEW-AD-D TO PFTI-ISS-PRM(SUB)
                 MOVE 12 TO PFTI-ISS-TERM(SUB)
                 MOVE LN-LOANDATE TO DATE-YYYYMMDD NDTE-DATE
                 PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
                 MOVE DATE-MMDDYY TO PFTI-EFF-DATE 
                 MOVE 12 TO NDTE-HOLD
                 PERFORM INCREMENT-MONTHS
                 MOVE NDTE-DATE TO DATE-YYYYMMDD
                 PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
                 MOVE DATE-MMDDYY TO PFTI-ISS-EXP(SUB)
                 MOVE "AD&D" TO PFTI-ISS-COV-CD(SUB)
                 ADD 1 TO SUB
                 ADD 1 TO NEW-AD-D-COUNT
                 ADD WORK-NEW-AD-D TO NEW-AD-D-AMT.

           IF WORK-NEW-NON-FILE NOT = 0
              IF NON-FILE-WRITTEN = "N"
                 IF SUB NOT > 1
                    PERFORM CREATE-NEW-PFTI
                 END-IF
                 MOVE LN-LNAMT TO PFTI-ISS-BEN(SUB)
                 MOVE WORK-NEW-NON-FILE TO PFTI-ISS-PRM(SUB)
                 MOVE LN-ORGTERM TO PFTI-ISS-TERM(SUB)
                 MOVE LN-LOANDATE TO DATE-YYYYMMDD NDTE-DATE
                 PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
                 MOVE DATE-MMDDYY TO PFTI-EFF-DATE
                 MOVE LN-ORGTERM TO NDTE-HOLD
                 PERFORM INCREMENT-MONTHS
                 MOVE NDTE-DATE TO DATE-YYYYMMDD
                 PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
                 MOVE DATE-MMDDYY TO PFTI-ISS-EXP(SUB)
                 MOVE "NONF" TO PFTI-ISS-COV-CD(SUB)
                 ADD 1 TO SUB
                 ADD 1 TO NEW-NON-FILE-COUNT
                 ADD WORK-NEW-NON-FILE TO NEW-NON-FILE-AMT.

      * ONLY WRITE IF O2 OR O3 WERE CREATED IN A SEPARATE RECORD
           IF SUB > 1
              ADD 1 TO PFTI-KEY
              PERFORM WRITE-PFTI-FILE
              ADD 1 TO TOTAL-I-RECORDS.

       KEY-CANCEL.
           IF WORK-CANCEL-FG-CL = " " AND
               WORK-CANCEL-FG-AH = " " AND
                WORK-CANCEL-FG-PP = " " AND
                 WORK-CANCEL-FG-O1 = " " AND
                  WORK-CANCEL-FG-O2 = " " AND
                   WORK-CANCEL-FG-O3 = " " AND
                   WORK-REFUND-FG-CL = " " AND
                    WORK-REFUND-FG-AH = " " AND
                     WORK-REFUND-FG-PP = " " AND
                      WORK-REFUND-FG-O1 = " " AND
                       WORK-REFUND-FG-O2 = " " AND
                        WORK-REFUND-FG-O3 = " "
              GO TO EXIT-KEY-DETAIL.

      * CREATE CANCEL/REFUND
           MOVE "C" TO PFTC-TYPE.
           MOVE PRIMARY-LNAME TO PFTC-CANC-LNAME.
           MOVE PRIMARY-FNAME TO PFTC-CANC-FNAME.

      * ALWAYS FILL IN THE TABLE STARTING FROM 1
           MOVE 1 TO SUB TAX-SUB.

           IF WORK-CANCEL-FG-CL = "C" OR WORK-REFUND-FG-CL = "R"
              ADD 1 TO CANCEL-LIFE-COUNT
              ADD WORK-CAN-CL TO CANCEL-LIFE-AMT
              MOVE LN-INSCOMP(1) TO PFTC-INSUROR
              MOVE LN-INSEFF-DATE(1) TO DATE-YYYYMMDD
              PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
              MOVE DATE-MMDDYY TO PFTC-CANC-EFF-DATE
              MOVE WORK-CAN-CL-DATE TO DATE-YYYYMMDD
              PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
              MOVE DATE-MMDDYY TO PFTC-CANC-DATE(1)
              MOVE WORK-CAN-CL TO PFTC-CANC-REF(1)
              MOVE LN-INSCOVR(1) TO PFTC-CANC-COVG(1)
              MOVE 2 TO SUB
              IF LN-INSTYPES-LL
                 MOVE "LLIF" TO PFTC-CANC-COV-CD(1)
              ELSE
                 MOVE "DLIF" TO PFTC-CANC-COV-CD(1)
              END-IF
              IF WORK-CAN-CL-TAX NOT = 0
                 MOVE WORK-CAN-CL-TAX TO PFTC-CANC-TAX(TAX-SUB)
                 MOVE "TC" TO PFTC-CANC-TAX-CODE(TAX-SUB)
                 ADD 1 TO TAX-SUB
                 MOVE WORK-TAX-CITY-CODE TO PFTC-TAX-CITY-CODE
                 MOVE WORK-TAX-COUNTY-CODE TO PFTC-TAX-COUNTY-CODE.

           IF WORK-CANCEL-FG-AH = "C" OR WORK-REFUND-FG-AH = "R"
              ADD 1 TO CANCEL-AH-COUNT
              ADD WORK-CAN-AH TO CANCEL-AH-AMT
              MOVE LN-INSCOMP(2) TO PFTC-INSUROR
              MOVE LN-INSEFF-DATE(2) TO DATE-YYYYMMDD
              PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
              MOVE DATE-MMDDYY TO PFTC-CANC-EFF-DATE
              MOVE "A&H " TO PFTC-CANC-COV-CD(SUB)
              MOVE WORK-CAN-AH-DATE TO DATE-YYYYMMDD
              PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
              MOVE DATE-MMDDYY TO PFTC-CANC-DATE(SUB)
              MOVE WORK-CAN-AH TO PFTC-CANC-REF(SUB)
              MOVE LN-INSCOVR(2) TO PFTC-CANC-COVG(SUB)
              ADD 1 TO SUB
              IF WORK-CAN-AH-TAX NOT = 0
                 MOVE WORK-CAN-AH-TAX TO PFTC-CANC-TAX(TAX-SUB)
                 MOVE "TA" TO PFTC-CANC-TAX-CODE(TAX-SUB)
                 ADD 1 TO TAX-SUB
                 MOVE WORK-TAX-CITY-CODE TO PFTC-TAX-CITY-CODE
                 MOVE WORK-TAX-COUNTY-CODE TO PFTC-TAX-COUNTY-CODE.

           IF WORK-CANCEL-FG-PP = "C" OR WORK-REFUND-FG-PP = "R"
              MOVE LN-INSCOMP(3) TO PFTC-INSUROR
              MOVE LN-INSEFF-DATE(3) TO DATE-YYYYMMDD
              PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
              MOVE DATE-MMDDYY TO PFTC-CANC-EFF-DATE
              MOVE WORK-CAN-PP-DATE TO DATE-YYYYMMDD
              PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
              MOVE DATE-MMDDYY TO PFTC-CANC-DATE(SUB)
              MOVE WORK-CAN-PP TO PFTC-CANC-REF(SUB)
              MOVE LN-INSCOVR(3) TO PFTC-CANC-COVG(SUB)
              PERFORM TEST-FOR-DUAL-PP2
              ADD 1 TO CANCEL-PP-COUNT
              ADD WORK-CAN-PP TO CANCEL-PP-AMT
              ADD 1 TO SUB
              IF WORK-CAN-PP-TAX NOT = 0
                 MOVE "TP" TO PFTC-CANC-TAX-CODE(TAX-SUB)
                 PERFORM COMPUTE-SURCHARGE
                 MOVE SURCHARGE-REBATE TO PFTC-CANC-SURCHG(TAX-SUB)
                 COMPUTE PFTC-CANC-TAX(TAX-SUB) =
                              ( WORK-CAN-PP-TAX - SURCHARGE-REBATE)
                 ADD 1 TO TAX-SUB
                 MOVE WORK-TAX-CITY-CODE TO PFTC-TAX-CITY-CODE
                 MOVE WORK-TAX-COUNTY-CODE TO PFTC-TAX-COUNTY-CODE.

           IF WORK-CANCEL-FG-O1 = "C" OR WORK-REFUND-FG-O1 = "R"
              MOVE LN-INSCOMP(4) TO PFTC-INSUROR
              MOVE LN-INSEFF-DATE(4) TO DATE-YYYYMMDD
              PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
              MOVE DATE-MMDDYY TO PFTC-CANC-EFF-DATE
              MOVE WORK-CAN-O1-DATE TO DATE-YYYYMMDD
              PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
              MOVE DATE-MMDDYY TO PFTC-CANC-DATE(SUB)
              MOVE WORK-CAN-O1 TO PFTC-CANC-REF(SUB)
              MOVE LN-INSCOVR(4) TO PFTC-CANC-COVG(SUB)
              ADD 1 TO CANCEL-AUTO-COUNT
              ADD WORK-CAN-O1 TO CANCEL-AUTO-AMT
              MOVE "OTH1" TO PFTC-CANC-COV-CD(SUB)
              ADD 1 TO SUB
              IF WORK-CAN-O1-TAX NOT = 0
                 MOVE "T1" TO PFTC-CANC-TAX-CODE(TAX-SUB)
                 MOVE WORK-CAN-O1-TAX TO PFTC-CANC-TAX(TAX-SUB)
                 ADD 1 TO TAX-SUB
                 MOVE WORK-TAX-CITY-CODE TO PFTC-TAX-CITY-CODE
                 MOVE WORK-TAX-COUNTY-CODE TO PFTC-TAX-COUNTY-CODE.

           IF WORK-CANCEL-FG-O2 = "C" OR WORK-REFUND-FG-O2 = "R"
              MOVE "N" TO O2-WRITTEN
              IF SUB NOT > 4
                 MOVE LN-INSCOMP(5) TO PFTC-INSUROR
                 MOVE LN-INSEFF-DATE(5) TO DATE-YYYYMMDD
                 PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
                 MOVE DATE-MMDDYY TO PFTC-CANC-EFF-DATE
                 MOVE WORK-CAN-O2-DATE TO DATE-YYYYMMDD
                 PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
                 MOVE DATE-MMDDYY TO PFTC-CANC-DATE(SUB)
                 MOVE WORK-CAN-O2 TO PFTC-CANC-REF(SUB)
                 MOVE LN-INSCOVR(5) TO PFTC-CANC-COVG(SUB)
                 ADD 1 TO CANCEL-O2-COUNT
                 ADD WORK-CAN-O2 TO CANCEL-O2-AMT
                 MOVE "OTH2" TO PFTC-CANC-COV-CD(SUB)
                 MOVE "Y" TO O2-WRITTEN
                 ADD 1 TO SUB
                 IF WORK-CAN-O2-TAX NOT = 0
                    MOVE WORK-CAN-O2-TAX TO PFTC-CANC-TAX(TAX-SUB)
                    MOVE "T2" TO PFTC-CANC-TAX-CODE(TAX-SUB)
                    ADD 1 TO TAX-SUB
                    MOVE WORK-TAX-CITY-CODE TO PFTC-TAX-CITY-CODE
                    MOVE WORK-TAX-COUNTY-CODE TO
                                        PFTC-TAX-COUNTY-CODE.

           IF WORK-CANCEL-FG-O3 = "C" OR WORK-REFUND-FG-O3 = "R"
              MOVE "N" TO O3-WRITTEN
              IF SUB NOT > 4
                 MOVE LN-INSCOMP(6) TO PFTC-INSUROR
                 MOVE LN-INSEFF-DATE(6) TO DATE-YYYYMMDD
                 PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
                 MOVE DATE-MMDDYY TO PFTC-CANC-EFF-DATE
                 MOVE WORK-CAN-O3-DATE TO DATE-YYYYMMDD
                 PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
                 MOVE DATE-MMDDYY TO PFTC-CANC-DATE(SUB)
                 MOVE WORK-CAN-O3 TO PFTC-CANC-REF(SUB)
                 MOVE LN-INSCOVR(6) TO PFTC-CANC-COVG(SUB)
                 ADD 1 TO CANCEL-O3-COUNT
                 ADD WORK-CAN-O3 TO CANCEL-O3-AMT
                 MOVE "OTH3" TO PFTC-CANC-COV-CD(SUB)
                 MOVE "Y" TO O3-WRITTEN
                 ADD 1 TO SUB
                 IF WORK-CAN-O3-TAX NOT = 0
                    MOVE WORK-CAN-O3-TAX TO PFTC-CANC-TAX(TAX-SUB)
                    MOVE "T3" TO PFTC-CANC-TAX-CODE(TAX-SUB)
                    ADD 1 TO TAX-SUB
                    MOVE WORK-TAX-CITY-CODE TO PFTC-TAX-CITY-CODE
                    MOVE WORK-TAX-COUNTY-CODE TO
                                        PFTC-TAX-COUNTY-CODE.

       KEY-WRITE-CANCEL.
           ADD 1 TO PFTC-KEY.
           PERFORM WRITE-PFTC-FILE.
           ADD 1 TO TOTAL-C-RECORDS.

           MOVE 1 TO SUB.

           IF WORK-CANCEL-FG-O2 = "C" OR WORK-REFUND-FG-O2 = "R"
              IF O2-WRITTEN = "N"
                 PERFORM CREATE-NEW-PFTC
                 MOVE LN-INSCOMP(5) TO PFTC-INSUROR
                 MOVE LN-INSEFF-DATE(5) TO DATE-YYYYMMDD
                 PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
                 MOVE DATE-MMDDYY TO PFTC-CANC-EFF-DATE
                 MOVE WORK-CAN-O2-DATE TO DATE-YYYYMMDD
                 PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
                 MOVE DATE-MMDDYY TO PFTC-CANC-DATE(SUB)
                 MOVE WORK-CAN-O2 TO PFTC-CANC-REF(SUB)
                 MOVE LN-INSCOVR(5) TO PFTC-CANC-COVG(SUB)
                 ADD 1 TO CANCEL-O2-COUNT
                 ADD WORK-CAN-O2 TO CANCEL-O2-AMT
                 MOVE "OTH2" TO PFTC-CANC-COV-CD(SUB)
                 MOVE "Y" TO O2-WRITTEN
                 ADD 1 TO SUB
                 IF WORK-CAN-O2-TAX NOT = 0
                    MOVE WORK-CAN-O2-TAX TO PFTC-CANC-TAX(TAX-SUB)
                    MOVE "T2" TO PFTC-CANC-TAX-CODE(TAX-SUB)
                    ADD 1 TO TAX-SUB
                    MOVE WORK-TAX-CITY-CODE TO PFTC-TAX-CITY-CODE
                    MOVE WORK-TAX-COUNTY-CODE TO
                                        PFTC-TAX-COUNTY-CODE.

           IF WORK-CANCEL-FG-O3 = "C" OR WORK-REFUND-FG-O3 = "R"
              IF O3-WRITTEN = "N"
                 IF SUB NOT  > 1
                    PERFORM CREATE-NEW-PFTC
                 END-IF
                 MOVE LN-INSCOMP(6)     TO PFTC-INSUROR
                 MOVE LN-INSEFF-DATE(6) TO DATE-YYYYMMDD
                 PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
                 MOVE DATE-MMDDYY      TO PFTC-CANC-EFF-DATE
                 MOVE WORK-CAN-O3-DATE TO DATE-YYYYMMDD
                 PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
                 MOVE DATE-MMDDYY   TO PFTC-CANC-DATE(SUB)
                 MOVE WORK-CAN-O3   TO PFTC-CANC-REF(SUB)
                 MOVE LN-INSCOVR(6) TO PFTC-CANC-COVG(SUB)
                 ADD 1 TO CANCEL-O3-COUNT
                 ADD WORK-CAN-O3 TO CANCEL-O3-AMT
                 MOVE "OTH3" TO PFTC-CANC-COV-CD(SUB)
                 MOVE "Y" TO O3-WRITTEN
                 ADD 1 TO SUB
                 IF WORK-CAN-O3-TAX NOT = 0
                    MOVE WORK-CAN-O3-TAX TO PFTC-CANC-TAX(TAX-SUB)
                    MOVE "T3" TO PFTC-CANC-TAX-CODE(TAX-SUB)
                    ADD 1 TO TAX-SUB
                    MOVE WORK-TAX-CITY-CODE TO PFTC-TAX-CITY-CODE
                    MOVE WORK-TAX-COUNTY-CODE TO
                                        PFTC-TAX-COUNTY-CODE.

           IF SUB > 1
              ADD 1 TO PFTC-KEY
              PERFORM WRITE-PFTC-FILE
              ADD 1 TO TOTAL-C-RECORDS.

       EXIT-KEY-DETAIL.
           EXIT.


      ******************************************
       SETUP-INSURED SECTION.
      ******************************************
           MOVE SPACES TO PRIMARY-LNAME PRIMARY-FNAME
                          JOINT-LNAME JOINT-FNAME
                          PRIMARY-ADR1 PRIMARY-ADR2
                          PRIMARY-CITY PRIMARY-STATE PRIMARY-ZIP.
           MOVE 0 TO PRIMARY-AGE PRIMARY-BDATE PRIMARY-SSNO
                     JOINT-AGE   JOINT-BDATE JOINT-SSNO.

           PERFORM READ-BW1-FILE.
           IF IO-FG = 9
              MOVE "*** NOT FOUND ***" TO BW-FNAME
              MOVE SPACES              TO BW-LNAME
              MOVE 0                   TO BW-BDATE BW-REFDATE.

      * SET NAME OF PRIMARY INSURED:
           MOVE BW-FNAME TO PRIMARY-FNAME.
           MOVE BW-LNAME TO PRIMARY-LNAME.
           MOVE BW-SSNO  TO PRIMARY-SSNO.
           MOVE BW-ADR1  TO PRIMARY-ADR1.
           MOVE BW-ADR2  TO PRIMARY-ADR2.
           MOVE BW-CITY  TO PRIMARY-CITY.
           MOVE BW-STATE TO PRIMARY-STATE.
           MOVE BW-ZIP   TO PRIMARY-ZIP.

      * SET AGE OF PRIMARY INSURED:
           MOVE BW-BDATE     TO BAGE-DATE PRIMARY-BDATE.
           MOVE BW-REFDATE   TO BAGE-REFDATE.
           MOVE RPT-END-DATE TO BAGE-TODAY.
           PERFORM BAGE-CALCULATION.
           MOVE BAGE-AGE TO PRIMARY-AGE.

           IF LN-INSURED NOT = "J"
              GO TO SETUP-INSURED-EXIT.

           PERFORM GET-JOINT-INSURED.

       SETUP-INSURED-EXIT.
           EXIT.


      ************************
       PRINT-TOTALS SECTION.
      ************************

           MOVE HEAD5 TO DETAIL-LINE.
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE NEW-LIFE-COUNT TO D-QTY1.
           MOVE NEW-AH-COUNT TO D-QTY2.
           MOVE NEW-PP-COUNT TO D-QTY3.
           MOVE NEW-AUTO-COUNT TO D-QTY4.
           MOVE NEW-O2-COUNT TO D-QTY5.
           MOVE NEW-O3-COUNT TO D-QTY6.
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE NEW-LIFE-AMT TO D-AMT1.
           MOVE NEW-AH-AMT TO D-AMT2.
           MOVE NEW-PP-AMT TO D-AMT3.
           MOVE NEW-AUTO-AMT TO D-AMT4.
           MOVE NEW-O2-AMT TO D-AMT5.
           MOVE NEW-O3-AMT TO D-AMT6.
           PERFORM WRITE-DETAIL-LINE.

           MOVE HEAD5A TO PR-REC.
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-PR-REC.

           MOVE NEW-AD-D-COUNT TO D-QTY1.
           MOVE NEW-NON-FILE-COUNT TO D-QTY2.
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE NEW-AD-D-AMT TO D-AMT1.
           MOVE NEW-NON-FILE-AMT TO D-AMT2.
           PERFORM WRITE-DETAIL-LINE.

           MOVE HEAD6 TO PR-REC.
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-PR-REC.

           MOVE CANCEL-LIFE-COUNT TO D-QTY1.
           MOVE CANCEL-AH-COUNT TO D-QTY2.
           MOVE CANCEL-PP-COUNT TO D-QTY3.
           MOVE CANCEL-AUTO-COUNT TO D-QTY4.
           MOVE CANCEL-O2-COUNT TO D-QTY5.
           MOVE CANCEL-O3-COUNT TO D-QTY6.
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE CANCEL-LIFE-AMT TO D-AMT1.
           MOVE CANCEL-AH-AMT TO D-AMT2.
           MOVE CANCEL-PP-AMT TO D-AMT3.
           MOVE CANCEL-AUTO-AMT TO D-AMT4.
           MOVE CANCEL-O2-AMT TO D-AMT5.
           MOVE CANCEL-O3-AMT TO D-AMT6.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "TOTAL ISSUE RECORDS CREATED: " TO DETAIL-LINE.
           MOVE TOTAL-I-RECORDS TO D-QTY3.
           MOVE 3 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "TOTAL CANCEL RECORDS CREATED: " TO DETAIL-LINE.
           MOVE TOTAL-C-RECORDS TO D-QTY3.
           PERFORM WRITE-DETAIL-LINE.

           MOVE SPACES TO DETAIL-LINE.
           PERFORM WRITE-DETAIL-LINE.

      *******************************
       PRINT-RANGE-LINES SECTION.
      *******************************

           MOVE 2 TO LN-FEED.

           IF GROUP-FLAG = "Y"
              MOVE "BRANCH GROUP" TO RANGE-SELECTION
              MOVE ENT-GROUP TO RANGE1
              PERFORM WRITE-DETAIL-LINE
           ELSE
              MOVE "BEG BR - END BR" TO RANGE-SELECTION
              MOVE BEG-BR TO RANGE1N
              MOVE END-BR TO RANGE2N
              PERFORM WRITE-DETAIL-LINE.

           MOVE "BEG INS - END INS" TO RANGE-SELECTION.
           MOVE BEG-IC TO RANGE1N.
           MOVE END-IC TO RANGE2N.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "BEG CL - END CL" TO RANGE-SELECTION.
           MOVE BEG-LC TO RANGE1N.
           MOVE END-LC TO RANGE2N.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "REPORT MMYY" TO RANGE-SELECTION.
           MOVE MMYY  TO RANGE1.
           PERFORM WRITE-DETAIL-LINE.

           PERFORM WRITE-DETAIL-LINE.

      ****************************************
       WRITE-DETAIL-LINE SECTION.
      ****************************************
           ADD LN-FEED LN-COUNT GIVING LN-WK.
           IF LN-WK > EXT-CONAME-RPT-LINES
              PERFORM HEAD-PAGE.
           MOVE DETAIL-LINE TO PR-REC.
           PERFORM WRITE-PR-REC.
           ADD LN-FEED TO LN-COUNT.
           MOVE 1 TO LN-FEED.
           MOVE SPACES TO DETAIL-LINE.

      *******************************
       HEAD-PAGE SECTION.
      *******************************
           MOVE "N" TO FIRST-TIME.
           ADD 1 TO PAGE-NUMBER.
           MOVE PAGE-NUMBER TO H-PAGE-NO.
           PERFORM GET-TIME.
           MOVE T-HH TO H-HR.
           MOVE T-MM TO H-MIN.

           MOVE HEAD1 TO PR-REC.
           MOVE 99 TO LN-FEED.
           PERFORM WRITE-PR-REC.

           MOVE HEAD2 TO PR-REC.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-PR-REC.

           MOVE HEAD3 TO PR-REC.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-PR-REC.

       END-HEAD-PAGE.
           MOVE 4 TO LN-COUNT.
           MOVE 2 TO LN-FEED.

       COPY "LIBLP/LPITRM.CPY".
       COPY "LIBGB/TIMEIO.CPY".
       COPY "LIBGB/DATER.CPY".
       COPY "LIBLP/LPBAGE.CPY".
       COPY "LIBGB/GRWORK.CPY".
       COPY "LIBGB/GRSERCH.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBLP/ARRAYSP.CPY". 

      **************************************************
       FORM-RESET SECTION.
      **************************************************
           MOVE EXT-CONAME-CONAME TO LSOUTH-CONAME.
           MOVE EXT-CONAME-SYSDATE TO LSOUTH-SYSDATE.
           DISPLAY LSOUTH-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE LSOUTH-FORM-FIELDS TO WR-BUF.
           MOVE LSOUTH-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      **************************************************
       SETUP-LINK-INFO SECTION.
      **************************************************
           MOVE LSOUTH-HELP--FILE TO HELP-LINK-FILE.
           MOVE LSOUTH-HELP--DIR TO HELP-LINK-DIR.

      **************************************************
       VDU-CONVERT-FIELD SECTION.
      **************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBSP/LSOUTH_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      **************************************
       MISC-PARA SECTION.
      **************************************
       COPY "LIBGB/GETPR.CPY".
       COPY "LIBGB/MKTEMP.CPY".
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/DACCESS.CPY".
       COPY "LIBGB/SYSTEM.CPY".
       COPY "LIBGB/RMFILE.CPY".
       CLEAR-WK-FILE.
           MOVE 0 TO WORK-NEW-CL WORK-CAN-CL WORK-CAN-CL-DATE
                     WORK-NEW-AH WORK-CAN-AH WORK-CAN-AH-DATE
                     WORK-NEW-PP WORK-CAN-PP WORK-CAN-PP-DATE
                     WORK-NEW-O1 WORK-CAN-O1 WORK-CAN-O1-DATE
                     WORK-NEW-O2 WORK-CAN-O2 WORK-CAN-O2-DATE
                     WORK-NEW-O3 WORK-CAN-O3 WORK-CAN-O3-DATE
                     WORK-NEW-AD-D WORK-NEW-NON-FILE
                     WORK-NEW-CL-TAX WORK-NEW-AH-TAX
                     WORK-NEW-PP-TAX WORK-NEW-O1-TAX
                     WORK-NEW-O2-TAX WORK-NEW-O3-TAX
                     WORK-CAN-CL-TAX WORK-CAN-AH-TAX
                     WORK-CAN-PP-TAX WORK-CAN-O1-TAX
                     WORK-CAN-O2-TAX WORK-CAN-O3-TAX.
           MOVE SPACES TO WORK-ADDON-FG-CL WORK-CANCEL-FG-CL
                          WORK-ADDON-FG-AH WORK-CANCEL-FG-AH
                          WORK-ADDON-FG-PP WORK-CANCEL-FG-PP
                          WORK-ADDON-FG-O1 WORK-CANCEL-FG-O1
                          WORK-ADDON-FG-O2 WORK-CANCEL-FG-O2
                          WORK-ADDON-FG-O3 WORK-CANCEL-FG-O3
                          WORK-REFUND-FG-CL WORK-REFUND-FG-AH
                          WORK-REFUND-FG-PP WORK-REFUND-FG-O1
                          WORK-REFUND-FG-O2 WORK-REFUND-FG-O3
                          WORK-TAX-CITY-CODE WORK-TAX-COUNTY-CODE.
       TEST-FOR-DUAL-PP2.
           IF LN-INSTYPES-PP2
              MOVE "DPRO" TO PFTC-CANC-COV-CD(SUB)
           ELSE
              MOVE "SPRO" TO PFTC-CANC-COV-CD(SUB).
      * WORLD, "MS" LOANS WERE CODED AS SINGLE BY MISTAKE FOR YEARS, THEY
      * NOW FIXED THE SPR'S BUT CANCELS ARE STILL GOING THRU AS SINGLE
      * LOANS PRIOR TO 03/08/17 NEED TO BE CODED AS DUAL CANCEL
           IF LN-ORGST = "MS"
              IF LN-LOANDATE < 20170308
                 MOVE "DPRO" TO PFTC-CANC-COV-CD(SUB).

       CREATE-NEW-PFTI.
           MOVE SPACES TO PFTI-REC.
           MOVE LN-ACCTNO TO PFTI-CERT-NBR.
           MOVE LN-OWNBR TO PFTI-PRODUCER.
           MOVE LN-CLASS TO PFTI-CLASS.
           MOVE 0 TO PFTI-INSUROR.
           MOVE "I" TO PFTI-TYPE.
           MOVE PRIMARY-SSNO TO PFTI-PRI-SSN.
           MOVE PRIMARY-LNAME TO PFTI-PRI-LNAME.
           MOVE PRIMARY-FNAME TO PFTI-PRI-FNAME.
           MOVE PRIMARY-AGE TO PFTI-PRI-AGE.
           MOVE PRIMARY-BDATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO PFTI-PRI-BIRTH.
           MOVE PRIMARY-ADR1 TO PFTI-PRI-ADR1.
           MOVE PRIMARY-ADR2 TO PFTI-PRI-ADR2.
           MOVE PRIMARY-CITY TO PFTI-PRI-CITY.
           MOVE PRIMARY-STATE TO PFTI-PRI-STATE.
           MOVE PRIMARY-ZIP TO PFTI-PRI-ZIP.
           MOVE JOINT-LNAME TO PFTI-SEC-LNAME.
           MOVE JOINT-FNAME TO PFTI-SEC-FNAME.
           MOVE JOINT-AGE TO PFTI-SEC-AGE.
           MOVE JOINT-BDATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO PFTI-SEC-BIRTH.
           MOVE JOINT-SSNO TO PFTI-SEC-SSN.
           MOVE LN-ORGTERM TO PFTI-LOAN-TERM.
       CREATE-NEW-PFTC.
           MOVE SPACES TO PFTC-REC.
           MOVE LN-ACCTNO TO PFTC-CERT-NBR.
           MOVE LN-OWNBR TO PFTC-PRODUCER.
           MOVE LN-CLASS TO PFTC-CLASS.
           MOVE 0 TO PFTC-INSUROR.
           MOVE "C" TO PFTC-TYPE.
       INVALID-RANGE.
           MOVE "INVALID RANGE" TO MESS.
           PERFORM SEND-MESS.
       SEND-LEGEND.
           MOVE MESS TO WR-BUF.
           MOVE MESS-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.
       GET-LN-BW-PATH.
      *    PERFORM CLOSE-LN1-FILE.
           PERFORM CLOSE-BW1-FILE.
           PERFORM READ-BR-FILE.
           IF IO-FG = 9
              GO TO IO-ERROR.

           MOVE EXT-FILPATH-BASE TO FILPATH-BASE.
           MOVE BR-MACHINE       TO FILPATH-MACHINE.
           MOVE BR-DATA-PATH     TO FILPATH-DATA.

           MOVE "B"        TO LN-PATH-OVERRIDE
                              BW-PATH-OVERRIDE.

      *    PERFORM OPEN-LN1-FILE.
           PERFORM OPEN-BW1-FILE.

       GET-JOINT-INSURED.
           MOVE LN-SSNO(2) TO BW-SSNO.
           PERFORM READ-BW1-FILE.
           IF IO-FG = 9
              MOVE "*** NOT FOUND ***" TO BW-FNAME
              MOVE SPACES TO BW-LNAME
              MOVE 0      TO BW-BDATE BW-REFDATE.

      * SET NAME OF JOINT INSURED:
           MOVE BW-FNAME TO JOINT-FNAME.
           MOVE BW-LNAME TO JOINT-LNAME.
           MOVE BW-SSNO TO JOINT-SSNO.

      * SET AGE OF JOINT INSURED:
           MOVE BW-BDATE     TO BAGE-DATE JOINT-BDATE.
           MOVE BW-REFDATE   TO BAGE-REFDATE.
           MOVE RPT-END-DATE TO BAGE-TODAY.
           PERFORM BAGE-CALCULATION.
           MOVE BAGE-AGE TO JOINT-AGE.

      ***********************************
       IO-ROUTINES SECTION.
      ***********************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBLP/LPTRSGS_SQL.CPY".
       COPY "LIBLP/LPTRPGS_SQL.CPY".
       COPY "LIBLP/LPTRLGS_SQL.CPY".
       COPY "LIBLP/LPSPGS_SQL.CPY".
       COPY "LIBLP/LPLNGS_SQL.CPY".
       COPY "LIBLP/LPBWGS_SQL.CPY".
       COPY "LIBGB/GBGRGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBPRINT.CPY".
       OPEN-PFTI-FILE.
           PERFORM OPEN-IT.
           MOVE PFTI-IFN TO E-FILE.
           OPEN I-O PFTI-FILE.
           IF IO-FG = 8
              GO TO OPEN-PFTI-FILE.
           IF IO-FG = 7
              CLOSE PFTI-FILE
              GO TO OPEN-PFTI-FILE.
       OPEN-PFTI-FILE-OUTPUT.
           PERFORM OPEN-IT.
           MOVE PFTI-IFN TO E-FILE.
           OPEN OUTPUT PFTI-FILE.
           IF IO-FG = 8
              GO TO OPEN-PFTI-FILE-OUTPUT.
           IF IO-FG = 7
              CLOSE PFTI-FILE
              GO TO OPEN-PFTI-FILE-OUTPUT.
       READ-PFTI-FILE.
           PERFORM READ-IT.
           MOVE PFTI-IFN TO E-FILE.
           MOVE PFTI-KEY TO E-KEYX.
           READ PFTI-FILE.
           IF IO-FG = 8
              GO TO READ-PFTI-FILE.
       WRITE-PFTI-FILE.
           PERFORM WRITE-IT.
           MOVE PFTI-IFN TO E-FILE.
           MOVE PFTI-KEY TO E-KEYX.
           WRITE PFTI-REC.
           IF IO-FG = 8
              GO TO WRITE-PFTI-FILE.
       REWRITE-PFTI-REC.
           PERFORM REWRITE-IT.
           MOVE PFTI-IFN TO E-FILE.
           MOVE PFTI-KEY TO E-KEYX.
           REWRITE PFTI-REC.
           IF IO-FG = 8
              GO TO REWRITE-PFTI-REC.
       CLOSE-PFTI-FILE.
           PERFORM CLOSE-IT.
           CLOSE PFTI-FILE.
       OPEN-PFTC-FILE.
           PERFORM OPEN-IT.
           MOVE PFTC-IFN TO E-FILE.
           OPEN I-O PFTC-FILE.
           IF IO-FG = 8
              GO TO OPEN-PFTC-FILE.
           IF IO-FG = 7
              CLOSE PFTC-FILE
              GO TO OPEN-PFTC-FILE.
       OPEN-PFTC-FILE-OUTPUT.
           PERFORM OPEN-IT.
           MOVE PFTC-IFN TO E-FILE.
           OPEN OUTPUT PFTC-FILE.
           IF IO-FG = 8
              GO TO OPEN-PFTC-FILE-OUTPUT.
           IF IO-FG = 7
              CLOSE PFTC-FILE
              GO TO OPEN-PFTC-FILE-OUTPUT.
       READ-PFTC-FILE.
           PERFORM READ-IT.
           MOVE PFTC-IFN TO E-FILE.
           MOVE PFTC-KEY TO E-KEYX.
           READ PFTC-FILE.
           IF IO-FG = 8
              GO TO READ-PFTC-FILE.
       WRITE-PFTC-FILE.
           PERFORM WRITE-IT.
           MOVE PFTC-IFN TO E-FILE.
           MOVE PFTC-KEY TO E-KEYX.
           WRITE PFTC-REC.
           IF IO-FG = 8
              GO TO WRITE-PFTC-FILE.
       CLOSE-PFTC-FILE.
           PERFORM CLOSE-IT.
           CLOSE PFTC-FILE.

       COPY "LIBGB/GBGR1RN.CPY".
       COPY "LIBLP/LPLN1RN.CPY".
       COPY "LIBLP/LPSP1RN.CPY".
       COPY "LIBLP/LPBW1RN.CPY".
       COPY "LIBGB/GBWKIN.CPY".
       COPY "LIBGB/GBBRRN.CPY".


      * COPYMEMBER: LIBLP/LPTR1RN
       LOAD-TRS1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( TRS-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( TRS-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO TRS-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO TRS-ALL-MACHINE
              MOVE EXT-FILPATH-DATA        TO TRS-ALL-DATA
              MOVE TRS-ALL-PATH             TO TRS-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( TRS-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO TRS-ALL-BASE
              MOVE FILPATH-MACHINE         TO TRS-ALL-MACHINE
              MOVE FILPATH-DATA            TO TRS-ALL-DATA
              MOVE TRS-ALL-PATH             TO TRS-PATH.

      *-----------------------------------------------------------------
       OPEN-TRS1-FILE.
           PERFORM LOAD-TRS1-FILE.
           PERFORM OPEN-IT.
           MOVE TRS-PATH-SQL TO E-FILE.

           IF TRS1-CONN-OPEN-SW = "N" 
               EXEC SQL CONNECT TO :SQL-CONNECT-SERVER AS TRS1_CONNECT
                    USER :SQL-CONNECT-USERID USING :SQL-CONNECT-PSWD
               END-EXEC
               MOVE "Y" TO TRS1-CONN-OPEN-SW
               EXEC SQL SET CONNECTION C1 END-EXEC.

      *-----------------------------------------------------------------
       START-TRS1-FILE.  *> SSP TRS LN
           PERFORM START-IT.
           MOVE TRS-PATH-OWNBR  TO TS-BRNO.
           MOVE TRS-PATH-SQL    TO E-FILE.
           MOVE TRS1-KEY        TO E-KEYX.

           IF ( TRS1-CURSOR-STAT-GOOD )
              PERFORM CLOSE-TRS1-FILE.

           EXEC SQL SET CONNECTION TRS1_CONNECT END-EXEC.

           EXEC SQL
            DECLARE TRS1_CURSOR CURSOR FOR
                :TRS1-RETURN-CODE = 
                 EXEC SSP_LSOUTH_TRSFILE_SELECT
                  ( :PASS-BRANCHLIST,
                    :QTRS1-WBEG-DATE,  :QTRS1-WEND-DATE,
                    :QTRS1-WBEG-CLASS, :QTRS1-WEND-CLASS) 
                END-EXEC.

      *     SELECT TRSFILE.TS_BRNO,
      *            CAST(TRSFILE.TS_DATE AS VARCHAR(10)),
      *            TRSFILE.TS_CLASS,
      *            TRSFILE.TS_ACCTNO,
      *            TRSFILE.TS_LAWCODE,
      *            TRSFILE.TS_LINE,
      *            TRSFILE.TS_FEECODE_1,
      *            TRSFILE.TS_FEECODE_2,
      *            TRSFILE.TS_FEECODE_3,
      *            TRSFILE.TS_FEECODE_4,
      *            TRSFILE.TS_FEECODE_5,
      *            TRSFILE.TS_FEECODE_6,
      *            TRSFILE.TS_FEECODE_7,
      *            TRSFILE.TS_FEECODE_8,
      *            TRSFILE.TS_FEECODE_9,
      *            TRSFILE.TS_FEECODE_10,
      *            TRSFILE.TS_FEECODE_11,
      *            TRSFILE.TS_FEECODE_12,
      *            TRSFILE.TS_FEECODE_13,
      *            TRSFILE.TS_FEECODE_14,
      *            TRSFILE.TS_FEECODE_15,
      *            TRSFILE.TS_FEECODE_16,
      *            TRSFILE.TS_FEECODE_17,
      *            TRSFILE.TS_FEECODE_18,
      *            TRSFILE.TS_FEECODE_19,
      *            TRSFILE.TS_FEECODE_20,
      *            TRSFILE.TS_FEEAMT_1,
      *            TRSFILE.TS_FEEAMT_2,
      *            TRSFILE.TS_FEEAMT_3,
      *            TRSFILE.TS_FEEAMT_4,
      *            TRSFILE.TS_FEEAMT_5,
      *            TRSFILE.TS_FEEAMT_6,
      *            TRSFILE.TS_FEEAMT_7,
      *            TRSFILE.TS_FEEAMT_8,
      *            TRSFILE.TS_FEEAMT_9,
      *            TRSFILE.TS_FEEAMT_10,
      *            TRSFILE.TS_FEEAMT_11,
      *            TRSFILE.TS_FEEAMT_12,
      *            TRSFILE.TS_FEEAMT_13,
      *            TRSFILE.TS_FEEAMT_14,
      *            TRSFILE.TS_FEEAMT_15,
      *            TRSFILE.TS_FEEAMT_16,
      *            TRSFILE.TS_FEEAMT_17,
      *            TRSFILE.TS_FEEAMT_18,
      *            TRSFILE.TS_FEEAMT_19,
      *            TRSFILE.TS_FEEAMT_20,
      *            TRSFILE.TS_INSCOMP_1,
      *            TRSFILE.TS_INSCOMP_2,
      *            TRSFILE.TS_INSCOMP_3,
      *            TRSFILE.TS_INSCOMP_4,
      *            TRSFILE.TS_INSCOMP_5,
      *            TRSFILE.TS_INSCOMP_6,
      *            TRSFILE.TS_INSCOMP_7,
      *            TRSFILE.TS_INSCOMP_8,
      *            TRSFILE.TS_INSCOMP_9,
      *            TRSFILE.TS_INSCOMP_10,
      *            TRSFILE.TS_INSOURS_1,
      *            TRSFILE.TS_INSOURS_2,
      *            TRSFILE.TS_INSOURS_3,
      *            TRSFILE.TS_INSOURS_4,
      *            TRSFILE.TS_INSOURS_5,
      *            TRSFILE.TS_INSOURS_6,
      *            TRSFILE.TS_INSOURS_7,
      *            TRSFILE.TS_INSOURS_8,
      *            TRSFILE.TS_INSOURS_9,
      *            TRSFILE.TS_INSOURS_10,
      *            TRSFILE.TS_INSPREM_1,
      *            TRSFILE.TS_INSPREM_2,
      *            TRSFILE.TS_INSPREM_3,
      *            TRSFILE.TS_INSPREM_4,
      *            TRSFILE.TS_INSPREM_5,
      *            TRSFILE.TS_INSPREM_6,
      *            TRSFILE.TS_INSPREM_7,
      *            TRSFILE.TS_INSPREM_8,
      *            TRSFILE.TS_INSPREM_9,
      *            TRSFILE.TS_INSPREM_10,
      *            TRSFILE.TS_INS_TAX_COUNTY_CD,
      *            TRSFILE.TS_INS_TAX_CITY_CD, 
      *            LNFILE.LN_OWNBR,
      *            LNFILE.LN_ACCTNO,
      *            LNFILE.LN_CLASS,
      *            LNFILE.LN_SUBCLASS,
      *            LNFILE.LN_LAWCODE,
      *            LNFILE.LN_SSNO1,
      *            LNFILE.LN_MAKERCD1,
      *            LNFILE.LN_SSNO2,
      *            LNFILE.LN_MAKERCD2,
      *            LNFILE.LN_SSNO3,
      *            LNFILE.LN_MAKERCD3,
      *            LNFILE.LN_SSNO4,
      *            LNFILE.LN_MAKERCD4,
      *            LNFILE.LN_ORGST,
      *            LNFILE.LN_SPRCLASS,
      *            CAST(LNFILE.LN_LOANDATE AS VARCHAR(10)),
      *            LNFILE.LN_APRATE,
      *            LNFILE.LN_LNAMT,
      *            LNFILE.LN_MAINTFEE,
      *            LNFILE.LN_ORGTERM,
      *            LNFILE.LN_REGPYAMT,
      *            LNFILE.LN_FEECODE_1,
      *            LNFILE.LN_FEECODE_2,
      *            LNFILE.LN_FEECODE_3,
      *            LNFILE.LN_FEECODE_4,
      *            LNFILE.LN_FEECODE_5,
      *            LNFILE.LN_FEECODE_6,
      *            LNFILE.LN_FEECODE_7,
      *            LNFILE.LN_FEECODE_8,
      *            LNFILE.LN_FEECODE_9,
      *            LNFILE.LN_FEECODE_10,
      *            LNFILE.LN_FEECODE_11,
      *            LNFILE.LN_FEECODE_12,
      *            LNFILE.LN_FEECODE_13,
      *            LNFILE.LN_FEECODE_14,
      *            LNFILE.LN_FEECODE_15,
      *            LNFILE.LN_FEECODE_16,
      *            LNFILE.LN_FEECODE_17,
      *            LNFILE.LN_FEECODE_18,
      *            LNFILE.LN_FEECODE_19,
      *            LNFILE.LN_FEECODE_20,
      *            LNFILE.LN_FEEAMT_1,
      *            LNFILE.LN_FEEAMT_2,
      *            LNFILE.LN_FEEAMT_3,
      *            LNFILE.LN_FEEAMT_4,
      *            LNFILE.LN_FEEAMT_5,
      *            LNFILE.LN_FEEAMT_6,
      *            LNFILE.LN_FEEAMT_7,
      *            LNFILE.LN_FEEAMT_8,
      *            LNFILE.LN_FEEAMT_9,
      *            LNFILE.LN_FEEAMT_10,
      *            LNFILE.LN_FEEAMT_11,
      *            LNFILE.LN_FEEAMT_12,
      *            LNFILE.LN_FEEAMT_13,
      *            LNFILE.LN_FEEAMT_14,
      *            LNFILE.LN_FEEAMT_15,
      *            LNFILE.LN_FEEAMT_16,
      *            LNFILE.LN_FEEAMT_17,
      *            LNFILE.LN_FEEAMT_18,
      *            LNFILE.LN_FEEAMT_19,
      *            LNFILE.LN_FEEAMT_20,
      *            LNFILE.LN_INSURED,
      *            LNFILE.LN_INSTYPES,
      *            LNFILE.LN_INSCOMP_1,
      *            LNFILE.LN_INSCOMP_2,
      *            LNFILE.LN_INSCOMP_3,
      *            LNFILE.LN_INSCOMP_4,
      *            LNFILE.LN_INSCOMP_5,
      *            LNFILE.LN_INSCOMP_6,
      *            LNFILE.LN_INSCOMP_7,
      *            LNFILE.LN_INSCOMP_8,
      *            LNFILE.LN_INSCOMP_9,
      *            LNFILE.LN_INSCOMP_10,
      *            LNFILE.LN_INSOURS_1,
      *            LNFILE.LN_INSOURS_2,
      *            LNFILE.LN_INSOURS_3,
      *            LNFILE.LN_INSOURS_4,
      *            LNFILE.LN_INSOURS_5,
      *            LNFILE.LN_INSOURS_6,
      *            LNFILE.LN_INSOURS_7,
      *            LNFILE.LN_INSOURS_8,
      *            LNFILE.LN_INSOURS_9,
      *            LNFILE.LN_INSOURS_10,
      *            CAST(LNFILE.LN_INSEFF_DATE_1 AS VARCHAR(10)),
      *            CAST(LNFILE.LN_INSEFF_DATE_2 AS VARCHAR(10)),
      *            CAST(LNFILE.LN_INSEFF_DATE_3 AS VARCHAR(10)),
      *            CAST(LNFILE.LN_INSEFF_DATE_4 AS VARCHAR(10)),
      *            CAST(LNFILE.LN_INSEFF_DATE_5 AS VARCHAR(10)),
      *            CAST(LNFILE.LN_INSEFF_DATE_6 AS VARCHAR(10)),
      *            CAST(LNFILE.LN_INSEFF_DATE_7 AS VARCHAR(10)),
      *            CAST(LNFILE.LN_INSEFF_DATE_8 AS VARCHAR(10)),
      *            CAST(LNFILE.LN_INSEFF_DATE_9 AS VARCHAR(10)),
      *            CAST(LNFILE.LN_INSEFF_DATE_10 AS VARCHAR(10)),
      *            CAST(LNFILE.LN_INSEXP_DATE_1  AS VARCHAR(10)),
      *            CAST(LNFILE.LN_INSEXP_DATE_2  AS VARCHAR(10)),
      *            CAST(LNFILE.LN_INSEXP_DATE_3  AS VARCHAR(10)),
      *            CAST(LNFILE.LN_INSEXP_DATE_4  AS VARCHAR(10)),
      *            CAST(LNFILE.LN_INSEXP_DATE_5  AS VARCHAR(10)),
      *            CAST(LNFILE.LN_INSEXP_DATE_6  AS VARCHAR(10)),
      *            CAST(LNFILE.LN_INSEXP_DATE_7  AS VARCHAR(10)),
      *            CAST(LNFILE.LN_INSEXP_DATE_8  AS VARCHAR(10)),
      *            CAST(LNFILE.LN_INSEXP_DATE_9  AS VARCHAR(10)),
      *            CAST(LNFILE.LN_INSEXP_DATE_10 AS VARCHAR(10)),
      *            LNFILE.LN_INSPREM_1,
      *            LNFILE.LN_INSPREM_2,
      *            LNFILE.LN_INSPREM_3,
      *            LNFILE.LN_INSPREM_4,
      *            LNFILE.LN_INSPREM_5,
      *            LNFILE.LN_INSPREM_6,
      *            LNFILE.LN_INSPREM_7,
      *            LNFILE.LN_INSPREM_8,
      *            LNFILE.LN_INSPREM_9,
      *            LNFILE.LN_INSPREM_10,
      *            LNFILE.LN_INSCOVR_1,
      *            LNFILE.LN_INSCOVR_2,
      *            LNFILE.LN_INSCOVR_3,
      *            LNFILE.LN_INSCOVR_4,
      *            LNFILE.LN_INSCOVR_5,
      *            LNFILE.LN_INSCOVR_6,
      *            LNFILE.LN_INSCOVR_7,
      *            LNFILE.LN_INSCOVR_8,
      *            LNFILE.LN_INSCOVR_9,
      *            LNFILE.LN_INSCOVR_10,
      *            LNFILE.LN_AD_D_INS_COVR,
      *            LNFILE.LN_CURBAL,
      *            LNFILE.LN_CL_REBATE,
      *            LNFILE.LN_AH_REBATE,
      *            LNFILE.LN_PP_REBATE,
      *            LNFILE.LN_INT_REBATE,
      *            LNFILE.LN_SC_REBATE,
      *            LNFILE.LN_MF_REBATE,
      *            LNFILE.LN_DF_REBATE,
      *            LNFILE.LN_EX_REBATE,
      *            LNFILE.LN_O1_REBATE,
      *            LNFILE.LN_O2_REBATE,
      *            LNFILE.LN_O3_REBATE,
      *            LNFILE.LN_O4_REBATE,
      *            LNFILE.LN_O5_REBATE,
      *            LNFILE.LN_O6_REBATE,
      *            LNFILE.LN_O7_REBATE,
      *            LNFILE.LN_UN_O7_REB,
      *            LNFILE.LN_UN_O8_REB,
      *            LNFILE.LN_UN_O9_REB,
      *            LNFILE.LN_UN_O10_REB,
      *            LNFILE.LN_UNITPER_CD,
      *            LNFILE.LN_UNITPER_FREQ,
      *         FROM #TEMP1_BRANCH_GROUP_TBL T1
      *         INNER JOIN DBO.TRSFILE TRSFILE
      *          ON TRSFILE.TS_BRNO = T1.BRNO
      *         INNER JOIN DBO.LNFILE LNFILE
      *          ON LNFILE.LN_OWNBR  = TRSFILE.TS_BRNO
      *          AND LNFILE.LN_ACCTNO = TRSFILE.TS_ACCTNO
      *         WHERE TRSFILE.TS_DATE  BETWEEN @QTRS1_WBEG_DATE  AND @QTRS1_WEND_DATE
      *           AND TRSFILE.TS_CLASS BETWEEN @QTRS1_WBEG_CLASS AND @QTRS1_WEND_CLASS
      *        ORDER BY T1.SEQ, TRSFILE.TS_DATE,
      *                TRSFILE.TS_CLASS,
      *                TRSFILE.TS_ACCTNO,
      *                TRSFILE.TS_LINE

           EXEC SQL
               OPEN TRS1_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE ZEROES TO QTRS1-WBEG-CLASS
                          QTRS1-WEND-CLASS.
           MOVE SPACES TO QTRS1-WBEG-DATE
                          QTRS1-WEND-DATE.

           MOVE STAT---GOOD TO TRS1-CURSOR-STAT.

           EXEC SQL SET CONNECTION C1 END-EXEC.

      *-----------------------------------------------------------------
       READ-TRS1-FILE-NEXT.  *> EMBEDDED TRS LN
           PERFORM READ-IT.
           MOVE TRS-PATH-OWNBR  TO TS-BRNO.
           MOVE TRS-PATH-SQL    TO E-FILE.
           MOVE TRS1-KEY        TO E-KEYX.
           INITIALIZE QTRS-REC QLN-REC.

           EXEC SQL SET CONNECTION TRS1_CONNECT END-EXEC.

           IF ( TRS1-CURSOR-STAT-GOOD )
              EXEC SQL
                FETCH TRS1_CURSOR INTO 
                   :QTS-BRNO,
                   :QTS-DATE ,
                   :QTS-CLASS,
                   :QTS-ACCTNO,
                   :QTS-LAWCODE,
                   :QTS-LINE,
                   :QTS-FEECODE-1,
                   :QTS-FEECODE-2,
                   :QTS-FEECODE-3,
                   :QTS-FEECODE-4,
                   :QTS-FEECODE-5,
                   :QTS-FEECODE-6,
                   :QTS-FEECODE-7,
                   :QTS-FEECODE-8,
                   :QTS-FEECODE-9,
                   :QTS-FEECODE-10,
                   :QTS-FEECODE-11,
                   :QTS-FEECODE-12,
                   :QTS-FEECODE-13,
                   :QTS-FEECODE-14,
                   :QTS-FEECODE-15,
                   :QTS-FEECODE-16,
                   :QTS-FEECODE-17,
                   :QTS-FEECODE-18,
                   :QTS-FEECODE-19,
                   :QTS-FEECODE-20,
                   :QTS-FEEAMT-1,
                   :QTS-FEEAMT-2,
                   :QTS-FEEAMT-3,
                   :QTS-FEEAMT-4,
                   :QTS-FEEAMT-5,
                   :QTS-FEEAMT-6,
                   :QTS-FEEAMT-7,
                   :QTS-FEEAMT-8,
                   :QTS-FEEAMT-9,
                   :QTS-FEEAMT-10,
                   :QTS-FEEAMT-11,
                   :QTS-FEEAMT-12,
                   :QTS-FEEAMT-13,
                   :QTS-FEEAMT-14,
                   :QTS-FEEAMT-15,
                   :QTS-FEEAMT-16,
                   :QTS-FEEAMT-17,
                   :QTS-FEEAMT-18,
                   :QTS-FEEAMT-19,
                   :QTS-FEEAMT-20,
                   :QTS-INSCOMP-1,
                   :QTS-INSCOMP-2,
                   :QTS-INSCOMP-3,
                   :QTS-INSCOMP-4,
                   :QTS-INSCOMP-5,
                   :QTS-INSCOMP-6,
                   :QTS-INSCOMP-7,
                   :QTS-INSCOMP-8,
                   :QTS-INSCOMP-9,
                   :QTS-INSCOMP-10,
                   :QTS-INSOURS-1,
                   :QTS-INSOURS-2,
                   :QTS-INSOURS-3,
                   :QTS-INSOURS-4,
                   :QTS-INSOURS-5,
                   :QTS-INSOURS-6,
                   :QTS-INSOURS-7,
                   :QTS-INSOURS-8,
                   :QTS-INSOURS-9,
                   :QTS-INSOURS-10,
                   :QTS-INSPREM-1,
                   :QTS-INSPREM-2,
                   :QTS-INSPREM-3,
                   :QTS-INSPREM-4,
                   :QTS-INSPREM-5,
                   :QTS-INSPREM-6,
                   :QTS-INSPREM-7,
                   :QTS-INSPREM-8,
                   :QTS-INSPREM-9,
                   :QTS-INSPREM-10,
                   :QTS-INS-TAX-COUNTY-CD,
                   :QTS-INS-TAX-CITY-CD, 
                   :QLN-OWNBR,
                   :QLN-ACCTNO,
                   :QLN-CLASS,
                   :QLN-SUBCLASS,
                   :QLN-LAWCODE,
                   :QLN-SSNO1,
                   :QLN-MAKERCD1,
                   :QLN-SSNO2,
                   :QLN-MAKERCD2,
                   :QLN-SSNO3,
                   :QLN-MAKERCD3,
                   :QLN-SSNO4,
                   :QLN-MAKERCD4,
                   :QLN-ORGST,
                   :QLN-SPRCLASS,
                   :QLN-LOANDATE ,
                   :QLN-APRATE,
                   :QLN-LNAMT,
                   :QLN-MAINTFEE,
                   :QLN-ORGTERM,
                   :QLN-REGPYAMT,
                   :QLN-FEECODE-1,
                   :QLN-FEECODE-2,
                   :QLN-FEECODE-3,
                   :QLN-FEECODE-4,
                   :QLN-FEECODE-5,
                   :QLN-FEECODE-6,
                   :QLN-FEECODE-7,
                   :QLN-FEECODE-8,
                   :QLN-FEECODE-9,
                   :QLN-FEECODE-10,
                   :QLN-FEECODE-11,
                   :QLN-FEECODE-12,
                   :QLN-FEECODE-13,
                   :QLN-FEECODE-14,
                   :QLN-FEECODE-15,
                   :QLN-FEECODE-16,
                   :QLN-FEECODE-17,
                   :QLN-FEECODE-18,
                   :QLN-FEECODE-19,
                   :QLN-FEECODE-20,
                   :QLN-FEEAMT-1,
                   :QLN-FEEAMT-2,
                   :QLN-FEEAMT-3,
                   :QLN-FEEAMT-4,
                   :QLN-FEEAMT-5,
                   :QLN-FEEAMT-6,
                   :QLN-FEEAMT-7,
                   :QLN-FEEAMT-8,
                   :QLN-FEEAMT-9,
                   :QLN-FEEAMT-10,
                   :QLN-FEEAMT-11,
                   :QLN-FEEAMT-12,
                   :QLN-FEEAMT-13,
                   :QLN-FEEAMT-14,
                   :QLN-FEEAMT-15,
                   :QLN-FEEAMT-16,
                   :QLN-FEEAMT-17,
                   :QLN-FEEAMT-18,
                   :QLN-FEEAMT-19,
                   :QLN-FEEAMT-20,
                   :QLN-INSURED,
                   :QLN-INSTYPES,
                   :QLN-INSCOMP-1,
                   :QLN-INSCOMP-2,
                   :QLN-INSCOMP-3,
                   :QLN-INSCOMP-4,
                   :QLN-INSCOMP-5,
                   :QLN-INSCOMP-6,
                   :QLN-INSCOMP-7,
                   :QLN-INSCOMP-8,
                   :QLN-INSCOMP-9,
                   :QLN-INSCOMP-10,
                   :QLN-INSOURS-1,
                   :QLN-INSOURS-2,
                   :QLN-INSOURS-3,
                   :QLN-INSOURS-4,
                   :QLN-INSOURS-5,
                   :QLN-INSOURS-6,
                   :QLN-INSOURS-7,
                   :QLN-INSOURS-8,
                   :QLN-INSOURS-9,
                   :QLN-INSOURS-10,
                   :QLN-INSEFF-DATE-1 ,
                   :QLN-INSEFF-DATE-2 ,
                   :QLN-INSEFF-DATE-3 ,
                   :QLN-INSEFF-DATE-4 ,
                   :QLN-INSEFF-DATE-5 ,
                   :QLN-INSEFF-DATE-6 ,
                   :QLN-INSEFF-DATE-7 ,
                   :QLN-INSEFF-DATE-8 ,
                   :QLN-INSEFF-DATE-9 ,
                   :QLN-INSEFF-DATE-10 ,
                   :QLN-INSEXP-DATE-1  ,
                   :QLN-INSEXP-DATE-2  ,
                   :QLN-INSEXP-DATE-3  ,
                   :QLN-INSEXP-DATE-4  ,
                   :QLN-INSEXP-DATE-5  ,
                   :QLN-INSEXP-DATE-6  ,
                   :QLN-INSEXP-DATE-7  ,
                   :QLN-INSEXP-DATE-8  ,
                   :QLN-INSEXP-DATE-9  ,
                   :QLN-INSEXP-DATE-10 ,
                   :QLN-INSPREM-1,
                   :QLN-INSPREM-2,
                   :QLN-INSPREM-3,
                   :QLN-INSPREM-4,
                   :QLN-INSPREM-5,
                   :QLN-INSPREM-6,
                   :QLN-INSPREM-7,
                   :QLN-INSPREM-8,
                   :QLN-INSPREM-9,
                   :QLN-INSPREM-10,
                   :QLN-INSCOVR-1,
                   :QLN-INSCOVR-2,
                   :QLN-INSCOVR-3,
                   :QLN-INSCOVR-4,
                   :QLN-INSCOVR-5,
                   :QLN-INSCOVR-6,
                   :QLN-INSCOVR-7,
                   :QLN-INSCOVR-8,
                   :QLN-INSCOVR-9,
                   :QLN-INSCOVR-10,
                   :QLN-AD-D-INS-COVR,
                   :QLN-CURBAL,
                   :QLN-CL-REBATE,
                   :QLN-AH-REBATE,
                   :QLN-PP-REBATE,
                   :QLN-INT-REBATE,
                   :QLN-SC-REBATE,
                   :QLN-MF-REBATE,
                   :QLN-DF-REBATE,
                   :QLN-EX-REBATE,
                   :QLN-O1-REBATE,
                   :QLN-O2-REBATE,
                   :QLN-O3-REBATE,
                   :QLN-O4-REBATE,
                   :QLN-O5-REBATE,
                   :QLN-O6-REBATE,
                   :QLN-O7-REBATE,
                   :QLN-UN-O7-REB,
                   :QLN-UN-O8-REB,
                   :QLN-UN-O9-REB,
                   :QLN-UN-O10-REB,
                   :QLN-UNITPER-CD,
                   :QLN-UNITPER-FREQ
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           EXEC SQL SET CONNECTION C1 END-EXEC.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-TRS1-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-TRS-FIELDS
              PERFORM GET-LN-FIELDS.

      *-----------------------------------------------------------------
       CLOSE-TRS1-FILE.

           PERFORM CLOSE-IT.
           IF ( TRS1-CURSOR-STAT-GOOD )
              EXEC SQL SET CONNECTION TRS1_CONNECT END-EXEC
              EXEC SQL
                  CLOSE TRS1_CURSOR
              END-EXEC
              MOVE STAT---BAD TO TRS1-CURSOR-STAT.

           EXEC SQL SET CONNECTION C1 END-EXEC.

      *-----------------------------------------------------------------
       CLOSE-TRS1-CONNECTION.

           IF TRS1-CONN-OPEN-SW = "Y"
               EXEC SQL SET CONNECTION TRS1_CONNECT END-EXEC
               EXEC SQL COMMIT END-EXEC
               EXEC SQL DISCONNECT TRS1_CONNECT END-EXEC
               EXEC SQL SET CONNECTION C1 END-EXEC
               MOVE "N" TO TRS1-CONN-OPEN-SW.
      *-----------------------------------------------------------------


      * COPY "LIBLP/LPTRP1RN.CPY".
      * COPYMEMBER: LIBLP/LPTRP1RN
      * 20220923 BAH ADDED TP-LCDUE *1570
       LOAD-TRP1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( TRP-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( TRP-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO TRP-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO TRP-ALL-MACHINE
              MOVE EXT-FILPATH-DATA        TO TRP-ALL-DATA
              MOVE TRP-ALL-PATH            TO TRP-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( TRP-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO TRP-ALL-BASE
              MOVE FILPATH-MACHINE         TO TRP-ALL-MACHINE
              MOVE FILPATH-DATA            TO TRP-ALL-DATA
              MOVE TRP-ALL-PATH            TO TRP-PATH.

      *-----------------------------------------------------------------
       OPEN-TRP1-FILE.
           PERFORM LOAD-TRP1-FILE.
           PERFORM OPEN-IT.
           MOVE TRP-PATH-SQL TO E-FILE.

           IF TRP1-CONN-OPEN-SW = "N" 
               EXEC SQL CONNECT TO :SQL-CONNECT-SERVER AS TRP1_CONNECT
                    USER :SQL-CONNECT-USERID USING :SQL-CONNECT-PSWD
               END-EXEC
               MOVE "Y" TO TRP1-CONN-OPEN-SW
               EXEC SQL SET CONNECTION C1 END-EXEC.

      *-----------------------------------------------------------------
       START-TRP1-FILE.  *> SSP TRP LN
           PERFORM START-IT.
           MOVE TRP-PATH-OWNBR  TO TP-BRNO.
           MOVE TRP-PATH-SQL    TO E-FILE.
           MOVE TRP1-KEY        TO E-KEYX.

           IF ( TRP1-CURSOR-STAT-GOOD )
              PERFORM CLOSE-TRP1-FILE.

           EXEC SQL SET CONNECTION TRP1_CONNECT END-EXEC.

           EXEC SQL
            DECLARE TRP1_CURSOR CURSOR FOR
                :TRP1-RETURN-CODE = 
                 EXEC SSP_LSOUTH_TRPFILE_SELECT
                  ( :PASS-BRANCHLIST,
                    :QTRP1-WBEG-DATE,  :QTRP1-WEND-DATE,
                    :QTRP1-WBEG-CLASS, :QTRP1-WEND-CLASS) 
                END-EXEC.

      *     EXEC SQL
      *      DECLARE TRP1_CURSOR CURSOR FOR
      *      SELECT TRPFILE.TP_BRNO,
      *            CAST(TRPFILE.TP_DATE AS VARCHAR(10)), 
      *            TRPFILE.TP_CLASS,
      *            TRPFILE.TP_ACCTNO,
      *            TRPFILE.TP_LINE,
      *            TRPFILE.TP_TRCD, 
      *            TRPFILE.TP_REFNO, 
      *            CAST(TRPFILE.TP_PAYDATE AS VARCHAR(10)), 
      *            TRPFILE.TP_TRAMT,
      *            TRPFILE.TP_APCUR,
      *            TRPFILE.TP_OTHBR_FG, 
      *            TRPFILE.TP_INS_TAX_RB,
      *            TRPFILE.TP_INS_TAX_COUNTY_CD, 
      *            TRPFILE.TP_INS_TAX_CITY_CD, 
      *            LNFILE.LN_OWNBR,
      *            LNFILE.LN_ACCTNO,
      *            LNFILE.LN_CLASS,
      *            LNFILE.LN_SUBCLASS,
      *            LNFILE.LN_LAWCODE,
      *            LNFILE.LN_SSNO1,
      *            LNFILE.LN_MAKERCD1,
      *            LNFILE.LN_SSNO2,
      *            LNFILE.LN_MAKERCD2,
      *            LNFILE.LN_SSNO3,
      *            LNFILE.LN_MAKERCD3,
      *            LNFILE.LN_SSNO4,
      *            LNFILE.LN_MAKERCD4,
      *            LNFILE.LN_ORGST,
      *            LNFILE.LN_SPRCLASS,
      *            CAST(LNFILE.LN_LOANDATE AS VARCHAR(10)),
      *            LNFILE.LN_APRATE,
      *            LNFILE.LN_LNAMT,
      *            LNFILE.LN_MAINTFEE,
      *            LNFILE.LN_ORGTERM,
      *            LNFILE.LN_REGPYAMT,
      *            LNFILE.LN_FEECODE_1,
      *            LNFILE.LN_FEECODE_2,
      *            LNFILE.LN_FEECODE_3,
      *            LNFILE.LN_FEECODE_4,
      *            LNFILE.LN_FEECODE_5,
      *            LNFILE.LN_FEECODE_6,
      *            LNFILE.LN_FEECODE_7,
      *            LNFILE.LN_FEECODE_8,
      *            LNFILE.LN_FEECODE_9,
      *            LNFILE.LN_FEECODE_10,
      *            LNFILE.LN_FEECODE_11,
      *            LNFILE.LN_FEECODE_12,
      *            LNFILE.LN_FEECODE_13,
      *            LNFILE.LN_FEECODE_14,
      *            LNFILE.LN_FEECODE_15,
      *            LNFILE.LN_FEECODE_16,
      *            LNFILE.LN_FEECODE_17,
      *            LNFILE.LN_FEECODE_18,
      *            LNFILE.LN_FEECODE_19,
      *            LNFILE.LN_FEECODE_20,
      *            LNFILE.LN_FEEAMT_1,
      *            LNFILE.LN_FEEAMT_2,
      *            LNFILE.LN_FEEAMT_3,
      *            LNFILE.LN_FEEAMT_4,
      *            LNFILE.LN_FEEAMT_5,
      *            LNFILE.LN_FEEAMT_6,
      *            LNFILE.LN_FEEAMT_7,
      *            LNFILE.LN_FEEAMT_8,
      *            LNFILE.LN_FEEAMT_9,
      *            LNFILE.LN_FEEAMT_10,
      *            LNFILE.LN_FEEAMT_11,
      *            LNFILE.LN_FEEAMT_12,
      *            LNFILE.LN_FEEAMT_13,
      *            LNFILE.LN_FEEAMT_14,
      *            LNFILE.LN_FEEAMT_15,
      *            LNFILE.LN_FEEAMT_16,
      *            LNFILE.LN_FEEAMT_17,
      *            LNFILE.LN_FEEAMT_18,
      *            LNFILE.LN_FEEAMT_19,
      *            LNFILE.LN_FEEAMT_20,
      *            LNFILE.LN_INSURED,
      *            LNFILE.LN_INSTYPES,
      *            LNFILE.LN_INSCOMP_1,
      *            LNFILE.LN_INSCOMP_2,
      *            LNFILE.LN_INSCOMP_3,
      *            LNFILE.LN_INSCOMP_4,
      *            LNFILE.LN_INSCOMP_5,
      *            LNFILE.LN_INSCOMP_6,
      *            LNFILE.LN_INSCOMP_7,
      *            LNFILE.LN_INSCOMP_8,
      *            LNFILE.LN_INSCOMP_9,
      *            LNFILE.LN_INSCOMP_10,
      *            LNFILE.LN_INSOURS_1,
      *            LNFILE.LN_INSOURS_2,
      *            LNFILE.LN_INSOURS_3,
      *            LNFILE.LN_INSOURS_4,
      *            LNFILE.LN_INSOURS_5,
      *            LNFILE.LN_INSOURS_6,
      *            LNFILE.LN_INSOURS_7,
      *            LNFILE.LN_INSOURS_8,
      *            LNFILE.LN_INSOURS_9,
      *            LNFILE.LN_INSOURS_10,
      *            CAST(LNFILE.LN_INSEFF_DATE_1 AS VARCHAR(10)),
      *            CAST(LNFILE.LN_INSEFF_DATE_2 AS VARCHAR(10)),
      *            CAST(LNFILE.LN_INSEFF_DATE_3 AS VARCHAR(10)),
      *            CAST(LNFILE.LN_INSEFF_DATE_4 AS VARCHAR(10)),
      *            CAST(LNFILE.LN_INSEFF_DATE_5 AS VARCHAR(10)),
      *            CAST(LNFILE.LN_INSEFF_DATE_6 AS VARCHAR(10)),
      *            CAST(LNFILE.LN_INSEFF_DATE_7 AS VARCHAR(10)),
      *            CAST(LNFILE.LN_INSEFF_DATE_8 AS VARCHAR(10)),
      *            CAST(LNFILE.LN_INSEFF_DATE_9 AS VARCHAR(10)),
      *            CAST(LNFILE.LN_INSEFF_DATE_10 AS VARCHAR(10)),
      *            CAST(LNFILE.LN_INSEXP_DATE_1  AS VARCHAR(10)),
      *            CAST(LNFILE.LN_INSEXP_DATE_2  AS VARCHAR(10)),
      *            CAST(LNFILE.LN_INSEXP_DATE_3  AS VARCHAR(10)),
      *            CAST(LNFILE.LN_INSEXP_DATE_4  AS VARCHAR(10)),
      *            CAST(LNFILE.LN_INSEXP_DATE_5  AS VARCHAR(10)),
      *            CAST(LNFILE.LN_INSEXP_DATE_6  AS VARCHAR(10)),
      *            CAST(LNFILE.LN_INSEXP_DATE_7  AS VARCHAR(10)),
      *            CAST(LNFILE.LN_INSEXP_DATE_8  AS VARCHAR(10)),
      *            CAST(LNFILE.LN_INSEXP_DATE_9  AS VARCHAR(10)),
      *            CAST(LNFILE.LN_INSEXP_DATE_10 AS VARCHAR(10)),
      *            LNFILE.LN_INSPREM_1,
      *            LNFILE.LN_INSPREM_2,
      *            LNFILE.LN_INSPREM_3,
      *            LNFILE.LN_INSPREM_4,
      *            LNFILE.LN_INSPREM_5,
      *            LNFILE.LN_INSPREM_6,
      *            LNFILE.LN_INSPREM_7,
      *            LNFILE.LN_INSPREM_8,
      *            LNFILE.LN_INSPREM_9,
      *            LNFILE.LN_INSPREM_10,
      *            LNFILE.LN_INSCOVR_1,
      *            LNFILE.LN_INSCOVR_2,
      *            LNFILE.LN_INSCOVR_3,
      *            LNFILE.LN_INSCOVR_4,
      *            LNFILE.LN_INSCOVR_5,
      *            LNFILE.LN_INSCOVR_6,
      *            LNFILE.LN_INSCOVR_7,
      *            LNFILE.LN_INSCOVR_8,
      *            LNFILE.LN_INSCOVR_9,
      *            LNFILE.LN_INSCOVR_10,
      *            LNFILE.LN_AD_D_INS_COVR,
      *            LNFILE.LN_CURBAL,
      *            LNFILE.LN_CL_REBATE,
      *            LNFILE.LN_AH_REBATE,
      *            LNFILE.LN_PP_REBATE,
      *            LNFILE.LN_INT_REBATE,
      *            LNFILE.LN_SC_REBATE,
      *            LNFILE.LN_MF_REBATE,
      *            LNFILE.LN_DF_REBATE,
      *            LNFILE.LN_EX_REBATE,
      *            LNFILE.LN_O1_REBATE,
      *            LNFILE.LN_O2_REBATE,
      *            LNFILE.LN_O3_REBATE,
      *            LNFILE.LN_O4_REBATE,
      *            LNFILE.LN_O5_REBATE,
      *            LNFILE.LN_O6_REBATE,
      *            LNFILE.LN_O7_REBATE,
      *            LNFILE.LN_UN_O7_REB,
      *            LNFILE.LN_UN_O8_REB,
      *            LNFILE.LN_UN_O9_REB,
      *            LNFILE.LN_UN_O10_REB,
      *            LNFILE.LN_UNITPER_CD,
      *            LNFILE.LN_UNITPER_FREQ
      *     FROM #TEMP1_BRANCH_GROUP_TBL T1
      *     INNER JOIN DBO.TRPFILE TRPFILE
      *      ON TRPFILE.TP_BRNO = T1.BRNO
      *     INNER JOIN DBO.LNFILE LNFILE
      *      ON LNFILE.LN_OWNBR  = TRPFILE.TP_BRNO
      *      AND LNFILE.LN_ACCTNO = TRPFILE.TP_ACCTNO
      *    WHERE TRPFILE.TP_DATE  BETWEEN @QTRP1_WBEG_DATE  AND @QTRP1_WEND_DATE
      *      AND TRPFILE.TP_CLASS BETWEEN @QTRP1_WBEG_CLASS AND @QTRP1_WEND_CLASS
      *      AND TRPFILE.TP_OTHBR_FG != 'C'
      *      AND  ((TRPFILE.TP_REFNO LIKE 'ADD%%' OR TRPFILE.TP_REFNO LIKE 'CNL%%' OR TRPFILE.TP_REFNO LIKE 'ADJ%%')  
      *         OR (TP_TRCD IN ('RC','RA','CC','CA','CP','RP','C1','R1','C2','R2','C3','R3')))
      *    ORDER BY TRPFILE.TP_DATE,
      *             TRPFILE.TP_CLASS,
      *             TRPFILE.TP_ACCTNO,
      *             TRPFILE.TP_LINE

           EXEC SQL
               OPEN TRP1_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE ZEROES TO QTRP1-WBEG-CLASS
                          QTRP1-WEND-CLASS
           MOVE SPACES TO QTRP1-WBEG-DATE
                          QTRP1-WEND-DATE.

           MOVE STAT---GOOD TO TRP1-CURSOR-STAT.

           EXEC SQL SET CONNECTION C1 END-EXEC.

      *-----------------------------------------------------------------
       READ-TRP1-FILE-NEXT.   *> EMBEDDED TRP LN
           PERFORM READ-IT.
           MOVE TRP-PATH-OWNBR  TO TP-BRNO.
           MOVE TRP-PATH-SQL    TO E-FILE.
           MOVE TRP1-KEY        TO E-KEYX.
           INITIALIZE QTRP-REC QLN-REC.

           EXEC SQL SET CONNECTION TRP1_CONNECT END-EXEC.

           IF ( TRP1-CURSOR-STAT-GOOD )
              EXEC SQL
                 FETCH TRP1_CURSOR INTO 
                   :QTP-BRNO,
                   :QTP-DATE , 
                   :QTP-CLASS,
                   :QTP-ACCTNO,
                   :QTP-LINE,
                   :QTP-TRCD, 
                   :QTP-REFNO, 
                   :QTP-PAYDATE , 
                   :QTP-TRAMT,
                   :QTP-APCUR,
                   :QTP-OTHBR-FG, 
                   :QTP-INS-TAX-RB,
                   :QTP-INS-TAX-COUNTY-CD, 
                   :QTP-INS-TAX-CITY-CD, 
                   :QLN-OWNBR,
                   :QLN-ACCTNO,
                   :QLN-CLASS,
                   :QLN-SUBCLASS,
                   :QLN-LAWCODE,
                   :QLN-SSNO1,
                   :QLN-MAKERCD1,
                   :QLN-SSNO2,
                   :QLN-MAKERCD2,
                   :QLN-SSNO3,
                   :QLN-MAKERCD3,
                   :QLN-SSNO4,
                   :QLN-MAKERCD4,
                   :QLN-ORGST,
                   :QLN-SPRCLASS,
                   :QLN-LOANDATE ,
                   :QLN-APRATE,
                   :QLN-LNAMT,
                   :QLN-MAINTFEE,
                   :QLN-ORGTERM,
                   :QLN-REGPYAMT,
                   :QLN-FEECODE-1,
                   :QLN-FEECODE-2,
                   :QLN-FEECODE-3,
                   :QLN-FEECODE-4,
                   :QLN-FEECODE-5,
                   :QLN-FEECODE-6,
                   :QLN-FEECODE-7,
                   :QLN-FEECODE-8,
                   :QLN-FEECODE-9,
                   :QLN-FEECODE-10,
                   :QLN-FEECODE-11,
                   :QLN-FEECODE-12,
                   :QLN-FEECODE-13,
                   :QLN-FEECODE-14,
                   :QLN-FEECODE-15,
                   :QLN-FEECODE-16,
                   :QLN-FEECODE-17,
                   :QLN-FEECODE-18,
                   :QLN-FEECODE-19,
                   :QLN-FEECODE-20,
                   :QLN-FEEAMT-1,
                   :QLN-FEEAMT-2,
                   :QLN-FEEAMT-3,
                   :QLN-FEEAMT-4,
                   :QLN-FEEAMT-5,
                   :QLN-FEEAMT-6,
                   :QLN-FEEAMT-7,
                   :QLN-FEEAMT-8,
                   :QLN-FEEAMT-9,
                   :QLN-FEEAMT-10,
                   :QLN-FEEAMT-11,
                   :QLN-FEEAMT-12,
                   :QLN-FEEAMT-13,
                   :QLN-FEEAMT-14,
                   :QLN-FEEAMT-15,
                   :QLN-FEEAMT-16,
                   :QLN-FEEAMT-17,
                   :QLN-FEEAMT-18,
                   :QLN-FEEAMT-19,
                   :QLN-FEEAMT-20,
                   :QLN-INSURED,
                   :QLN-INSTYPES,
                   :QLN-INSCOMP-1,
                   :QLN-INSCOMP-2,
                   :QLN-INSCOMP-3,
                   :QLN-INSCOMP-4,
                   :QLN-INSCOMP-5,
                   :QLN-INSCOMP-6,
                   :QLN-INSCOMP-7,
                   :QLN-INSCOMP-8,
                   :QLN-INSCOMP-9,
                   :QLN-INSCOMP-10,
                   :QLN-INSOURS-1,
                   :QLN-INSOURS-2,
                   :QLN-INSOURS-3,
                   :QLN-INSOURS-4,
                   :QLN-INSOURS-5,
                   :QLN-INSOURS-6,
                   :QLN-INSOURS-7,
                   :QLN-INSOURS-8,
                   :QLN-INSOURS-9,
                   :QLN-INSOURS-10,
                   :QLN-INSEFF-DATE-1 ,
                   :QLN-INSEFF-DATE-2 ,
                   :QLN-INSEFF-DATE-3 ,
                   :QLN-INSEFF-DATE-4 ,
                   :QLN-INSEFF-DATE-5 ,
                   :QLN-INSEFF-DATE-6 ,
                   :QLN-INSEFF-DATE-7 ,
                   :QLN-INSEFF-DATE-8 ,
                   :QLN-INSEFF-DATE-9 ,
                   :QLN-INSEFF-DATE-10 ,
                   :QLN-INSEXP-DATE-1  ,
                   :QLN-INSEXP-DATE-2  ,
                   :QLN-INSEXP-DATE-3  ,
                   :QLN-INSEXP-DATE-4  ,
                   :QLN-INSEXP-DATE-5  ,
                   :QLN-INSEXP-DATE-6  ,
                   :QLN-INSEXP-DATE-7  ,
                   :QLN-INSEXP-DATE-8  ,
                   :QLN-INSEXP-DATE-9  ,
                   :QLN-INSEXP-DATE-10 ,
                   :QLN-INSPREM-1,
                   :QLN-INSPREM-2,
                   :QLN-INSPREM-3,
                   :QLN-INSPREM-4,
                   :QLN-INSPREM-5,
                   :QLN-INSPREM-6,
                   :QLN-INSPREM-7,
                   :QLN-INSPREM-8,
                   :QLN-INSPREM-9,
                   :QLN-INSPREM-10,
                   :QLN-INSCOVR-1,
                   :QLN-INSCOVR-2,
                   :QLN-INSCOVR-3,
                   :QLN-INSCOVR-4,
                   :QLN-INSCOVR-5,
                   :QLN-INSCOVR-6,
                   :QLN-INSCOVR-7,
                   :QLN-INSCOVR-8,
                   :QLN-INSCOVR-9,
                   :QLN-INSCOVR-10,
                   :QLN-AD-D-INS-COVR,
                   :QLN-CURBAL,
                   :QLN-CL-REBATE,
                   :QLN-AH-REBATE,
                   :QLN-PP-REBATE,
                   :QLN-INT-REBATE,
                   :QLN-SC-REBATE,
                   :QLN-MF-REBATE,
                   :QLN-DF-REBATE,
                   :QLN-EX-REBATE,
                   :QLN-O1-REBATE,
                   :QLN-O2-REBATE,
                   :QLN-O3-REBATE,
                   :QLN-O4-REBATE,
                   :QLN-O5-REBATE,
                   :QLN-O6-REBATE,
                   :QLN-O7-REBATE,
                   :QLN-UN-O7-REB,
                   :QLN-UN-O8-REB,
                   :QLN-UN-O9-REB,
                   :QLN-UN-O10-REB,
                   :QLN-UNITPER-CD,
                   :QLN-UNITPER-FREQ
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           EXEC SQL SET CONNECTION C1 END-EXEC.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-TRP1-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-TRP-FIELDS
              PERFORM GET-LN-FIELDS.

      *-----------------------------------------------------------------
       CLOSE-TRP1-FILE.

           PERFORM CLOSE-IT.

           IF ( TRP1-CURSOR-STAT-GOOD )
              EXEC SQL SET CONNECTION TRP1_CONNECT END-EXEC
              EXEC SQL
                   CLOSE TRP1_CURSOR
              END-EXEC
              MOVE STAT---BAD TO TRP1-CURSOR-STAT
              EXEC SQL SET CONNECTION C1 END-EXEC.

      *-----------------------------------------------------------------
       CLOSE-TRP1-CONNECTION.

           IF TRP1-CONN-OPEN-SW = "Y"
               EXEC SQL SET CONNECTION TRP1_CONNECT END-EXEC
               EXEC SQL COMMIT END-EXEC
               EXEC SQL DISCONNECT TRP1_CONNECT END-EXEC
               EXEC SQL SET CONNECTION C1 END-EXEC
               MOVE "N" TO TRP1-CONN-OPEN-SW.
      *-----------------------------------------------------------------


      * COPY "LIBLP/LPTRL1RN.CPY".
      * COPYMEMBER: LIBLP/LPTR1RN
       LOAD-TRL1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( TRL-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( TRL-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO TRL-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO TRL-ALL-MACHINE
              MOVE EXT-FILPATH-DATA        TO TRL-ALL-DATA
              MOVE TRL-ALL-PATH             TO TRL-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( TRL-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO TRL-ALL-BASE
              MOVE FILPATH-MACHINE         TO TRL-ALL-MACHINE
              MOVE FILPATH-DATA            TO TRL-ALL-DATA
              MOVE TRL-ALL-PATH             TO TRL-PATH.

      *-----------------------------------------------------------------
       OPEN-TRL1-FILE.
           PERFORM LOAD-TRL1-FILE.
           PERFORM OPEN-IT.
           MOVE TRL-PATH-SQL TO E-FILE.

           IF TRL1-CONN-OPEN-SW = "N"
               EXEC SQL CONNECT TO :SQL-CONNECT-SERVER AS TRL1_CONNECT
                        USER :SQL-CONNECT-USERID USING :SQL-CONNECT-PSWD
               END-EXEC
               EXEC SQL SET CONNECTION C1 END-EXEC
               MOVE 'Y' TO TRL1-CONN-OPEN-SW.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       START-TRL1-FILE.   *> SSP TRL LN
           PERFORM START-IT.
           MOVE TRL-PATH-OWNBR  TO TL-BRNO.
           MOVE TRL-PATH-SQL    TO E-FILE.
           MOVE TRL1-KEY        TO E-KEYX.

           IF ( TRL1-CURSOR-STAT-GOOD )
              PERFORM CLOSE-TRL1-FILE.

      *  SELECT TRLFILE.TL_BRNO,
      *            CAST(TRLFILE.TL_DATE AS VARCHAR(10)),
      *            TRLFILE.TL_CLASS,
      *            TRLFILE.TL_ACCTNO,
      *            TRLFILE.TL_LAWCODE,
      *            TRLFILE.TL_LINE,
      *            TRLFILE.TL_FEECODE_1,
      *            TRLFILE.TL_FEECODE_2,
      *            TRLFILE.TL_FEECODE_3,
      *            TRLFILE.TL_FEECODE_4,
      *            TRLFILE.TL_FEECODE_5,
      *            TRLFILE.TL_FEECODE_6,
      *            TRLFILE.TL_FEECODE_7,
      *            TRLFILE.TL_FEECODE_8,
      *            TRLFILE.TL_FEECODE_9,
      *            TRLFILE.TL_FEECODE_10,
      *            TRLFILE.TL_FEECODE_11,
      *            TRLFILE.TL_FEECODE_12,
      *            TRLFILE.TL_FEECODE_13,
      *            TRLFILE.TL_FEECODE_14,
      *            TRLFILE.TL_FEECODE_15,
      *            TRLFILE.TL_FEECODE_16,
      *            TRLFILE.TL_FEECODE_17,
      *            TRLFILE.TL_FEECODE_18,
      *            TRLFILE.TL_FEECODE_19,
      *            TRLFILE.TL_FEECODE_20,
      *            TRLFILE.TL_FEEAMT_1,
      *            TRLFILE.TL_FEEAMT_2,
      *            TRLFILE.TL_FEEAMT_3,
      *            TRLFILE.TL_FEEAMT_4,
      *            TRLFILE.TL_FEEAMT_5,
      *            TRLFILE.TL_FEEAMT_6,
      *            TRLFILE.TL_FEEAMT_7,
      *            TRLFILE.TL_FEEAMT_8,
      *            TRLFILE.TL_FEEAMT_9,
      *            TRLFILE.TL_FEEAMT_10,
      *            TRLFILE.TL_FEEAMT_11,
      *            TRLFILE.TL_FEEAMT_12,
      *            TRLFILE.TL_FEEAMT_13,
      *            TRLFILE.TL_FEEAMT_14,
      *            TRLFILE.TL_FEEAMT_15,
      *            TRLFILE.TL_FEEAMT_16,
      *            TRLFILE.TL_FEEAMT_17,
      *            TRLFILE.TL_FEEAMT_18,
      *            TRLFILE.TL_FEEAMT_19,
      *            TRLFILE.TL_FEEAMT_20,
      *            TRLFILE.TL_INSCOMP_1,
      *            TRLFILE.TL_INSCOMP_2,
      *            TRLFILE.TL_INSCOMP_3,
      *            TRLFILE.TL_INSCOMP_4,
      *            TRLFILE.TL_INSCOMP_5,
      *            TRLFILE.TL_INSCOMP_6,
      *            TRLFILE.TL_INSCOMP_7,
      *            TRLFILE.TL_INSCOMP_8,
      *            TRLFILE.TL_INSCOMP_9,
      *            TRLFILE.TL_INSCOMP_10,
      *            TRLFILE.TL_INSOURS_1,
      *            TRLFILE.TL_INSOURS_2,
      *            TRLFILE.TL_INSOURS_3,
      *            TRLFILE.TL_INSOURS_4,
      *            TRLFILE.TL_INSOURS_5,
      *            TRLFILE.TL_INSOURS_6,
      *            TRLFILE.TL_INSOURS_7,
      *            TRLFILE.TL_INSOURS_8,
      *            TRLFILE.TL_INSOURS_9,
      *            TRLFILE.TL_INSOURS_10,
      *            TRLFILE.TL_INSPREM_1,
      *            TRLFILE.TL_INSPREM_2,
      *            TRLFILE.TL_INSPREM_3,
      *            TRLFILE.TL_INSPREM_4,
      *            TRLFILE.TL_INSPREM_5,
      *            TRLFILE.TL_INSPREM_6,
      *            TRLFILE.TL_INSPREM_7,
      *            TRLFILE.TL_INSPREM_8,
      *            TRLFILE.TL_INSPREM_9,
      *            TRLFILE.TL_INSPREM_10,
      *            TRLFILE.TL_INS_TAX_COUNTY_CD,
      *            TRLFILE.TL_INS_TAX_CITY_CD, 
      *            LNFILE.LN_OWNBR,
      *            LNFILE.LN_ACCTNO,
      *            LNFILE.LN_CLASS,
      *            LNFILE.LN_SUBCLASS,
      *            LNFILE.LN_LAWCODE,
      *            LNFILE.LN_SSNO1,
      *            LNFILE.LN_MAKERCD1,
      *            LNFILE.LN_SSNO2,
      *            LNFILE.LN_MAKERCD2,
      *            LNFILE.LN_SSNO3,
      *            LNFILE.LN_MAKERCD3,
      *            LNFILE.LN_SSNO4,
      *            LNFILE.LN_MAKERCD4,
      *            LNFILE.LN_ORGST,
      *            LNFILE.LN_SPRCLASS,
      *            CAST(LNFILE.LN_LOANDATE AS VARCHAR(10)),
      *            LNFILE.LN_APRATE,
      *            LNFILE.LN_LNAMT,
      *            LNFILE.LN_MAINTFEE,
      *            LNFILE.LN_ORGTERM,
      *            LNFILE.LN_REGPYAMT,
      *            LNFILE.LN_FEECODE_1,
      *            LNFILE.LN_FEECODE_2,
      *            LNFILE.LN_FEECODE_3,
      *            LNFILE.LN_FEECODE_4,
      *            LNFILE.LN_FEECODE_5,
      *            LNFILE.LN_FEECODE_6,
      *            LNFILE.LN_FEECODE_7,
      *            LNFILE.LN_FEECODE_8,
      *            LNFILE.LN_FEECODE_9,
      *            LNFILE.LN_FEECODE_10,
      *            LNFILE.LN_FEECODE_11,
      *            LNFILE.LN_FEECODE_12,
      *            LNFILE.LN_FEECODE_13,
      *            LNFILE.LN_FEECODE_14,
      *            LNFILE.LN_FEECODE_15,
      *            LNFILE.LN_FEECODE_16,
      *            LNFILE.LN_FEECODE_17,
      *            LNFILE.LN_FEECODE_18,
      *            LNFILE.LN_FEECODE_19,
      *            LNFILE.LN_FEECODE_20,
      *            LNFILE.LN_FEEAMT_1,
      *            LNFILE.LN_FEEAMT_2,
      *            LNFILE.LN_FEEAMT_3,
      *            LNFILE.LN_FEEAMT_4,
      *            LNFILE.LN_FEEAMT_5,
      *            LNFILE.LN_FEEAMT_6,
      *            LNFILE.LN_FEEAMT_7,
      *            LNFILE.LN_FEEAMT_8,
      *            LNFILE.LN_FEEAMT_9,
      *            LNFILE.LN_FEEAMT_10,
      *            LNFILE.LN_FEEAMT_11,
      *            LNFILE.LN_FEEAMT_12,
      *            LNFILE.LN_FEEAMT_13,
      *            LNFILE.LN_FEEAMT_14,
      *            LNFILE.LN_FEEAMT_15,
      *            LNFILE.LN_FEEAMT_16,
      *            LNFILE.LN_FEEAMT_17,
      *            LNFILE.LN_FEEAMT_18,
      *            LNFILE.LN_FEEAMT_19,
      *            LNFILE.LN_FEEAMT_20,
      *            LNFILE.LN_INSURED,
      *            LNFILE.LN_INSTYPES,
      *            LNFILE.LN_INSCOMP_1,
      *            LNFILE.LN_INSCOMP_2,
      *            LNFILE.LN_INSCOMP_3,
      *            LNFILE.LN_INSCOMP_4,
      *            LNFILE.LN_INSCOMP_5,
      *            LNFILE.LN_INSCOMP_6,
      *            LNFILE.LN_INSCOMP_7,
      *            LNFILE.LN_INSCOMP_8,
      *            LNFILE.LN_INSCOMP_9,
      *            LNFILE.LN_INSCOMP_10,
      *            LNFILE.LN_INSOURS_1,
      *            LNFILE.LN_INSOURS_2,
      *            LNFILE.LN_INSOURS_3,
      *            LNFILE.LN_INSOURS_4,
      *            LNFILE.LN_INSOURS_5,
      *            LNFILE.LN_INSOURS_6,
      *            LNFILE.LN_INSOURS_7,
      *            LNFILE.LN_INSOURS_8,
      *            LNFILE.LN_INSOURS_9,
      *            LNFILE.LN_INSOURS_10,
      *            CAST(LNFILE.LN_INSEFF_DATE_1 AS VARCHAR(10)),
      *            CAST(LNFILE.LN_INSEFF_DATE_2 AS VARCHAR(10)),
      *            CAST(LNFILE.LN_INSEFF_DATE_3 AS VARCHAR(10)),
      *            CAST(LNFILE.LN_INSEFF_DATE_4 AS VARCHAR(10)),
      *            CAST(LNFILE.LN_INSEFF_DATE_5 AS VARCHAR(10)),
      *            CAST(LNFILE.LN_INSEFF_DATE_6 AS VARCHAR(10)),
      *            CAST(LNFILE.LN_INSEFF_DATE_7 AS VARCHAR(10)),
      *            CAST(LNFILE.LN_INSEFF_DATE_8 AS VARCHAR(10)),
      *            CAST(LNFILE.LN_INSEFF_DATE_9 AS VARCHAR(10)),
      *            CAST(LNFILE.LN_INSEFF_DATE_10 AS VARCHAR(10)),
      *            CAST(LNFILE.LN_INSEXP_DATE_1  AS VARCHAR(10)),
      *            CAST(LNFILE.LN_INSEXP_DATE_2  AS VARCHAR(10)),
      *            CAST(LNFILE.LN_INSEXP_DATE_3  AS VARCHAR(10)),
      *            CAST(LNFILE.LN_INSEXP_DATE_4  AS VARCHAR(10)),
      *            CAST(LNFILE.LN_INSEXP_DATE_5  AS VARCHAR(10)),
      *            CAST(LNFILE.LN_INSEXP_DATE_6  AS VARCHAR(10)),
      *            CAST(LNFILE.LN_INSEXP_DATE_7  AS VARCHAR(10)),
      *            CAST(LNFILE.LN_INSEXP_DATE_8  AS VARCHAR(10)),
      *            CAST(LNFILE.LN_INSEXP_DATE_9  AS VARCHAR(10)),
      *            CAST(LNFILE.LN_INSEXP_DATE_10 AS VARCHAR(10)),
      *            LNFILE.LN_INSPREM_1,
      *            LNFILE.LN_INSPREM_2,
      *            LNFILE.LN_INSPREM_3,
      *            LNFILE.LN_INSPREM_4,
      *            LNFILE.LN_INSPREM_5,
      *            LNFILE.LN_INSPREM_6,
      *            LNFILE.LN_INSPREM_7,
      *            LNFILE.LN_INSPREM_8,
      *            LNFILE.LN_INSPREM_9,
      *            LNFILE.LN_INSPREM_10,
      *            LNFILE.LN_INSCOVR_1,
      *            LNFILE.LN_INSCOVR_2,
      *            LNFILE.LN_INSCOVR_3,
      *            LNFILE.LN_INSCOVR_4,
      *            LNFILE.LN_INSCOVR_5,
      *            LNFILE.LN_INSCOVR_6,
      *            LNFILE.LN_INSCOVR_7,
      *            LNFILE.LN_INSCOVR_8,
      *            LNFILE.LN_INSCOVR_9,
      *            LNFILE.LN_INSCOVR_10,
      *            LNFILE.LN_AD_D_INS_COVR,
      *            LNFILE.LN_CURBAL,
      *            LNFILE.LN_CL_REBATE,
      *            LNFILE.LN_AH_REBATE,
      *            LNFILE.LN_PP_REBATE,
      *            LNFILE.LN_INT_REBATE,
      *            LNFILE.LN_SC_REBATE,
      *            LNFILE.LN_MF_REBATE,
      *            LNFILE.LN_DF_REBATE,
      *            LNFILE.LN_EX_REBATE,
      *            LNFILE.LN_O1_REBATE,
      *            LNFILE.LN_O2_REBATE,
      *            LNFILE.LN_O3_REBATE,
      *            LNFILE.LN_O4_REBATE,
      *            LNFILE.LN_O5_REBATE,
      *            LNFILE.LN_O6_REBATE,
      *            LNFILE.LN_O7_REBATE,
      *            LNFILE.LN_UN_O7_REB,
      *            LNFILE.LN_UN_O8_REB,
      *            LNFILE.LN_UN_O9_REB,
      *            LNFILE.LN_UN_O10_REB,
      *            LNFILE.LN_UNITPER_CD,
      *            LNFILE.LN_UNITPER_FREQ
      *    FROM #TEMP1_BRANCH_GROUP_TBL T1
      *    INNER JOIN DBO.TRLFILE TRLFILE
      *      ON TRLFILE.TL_BRNO = T1.BRNO
      *     FROM  DBO.TRLFILE TRLFILE
      *    INNER JOIN DBO.LNFILE LNFILE
      *     ON LNFILE.LN_OWNBR  = TRLFILE.TL_BRNO
      *     AND LNFILE.LN_ACCTNO = TRLFILE.TL_ACCTNO
      *    WHERE TRLFILE.TL_DATE  BETWEEN @QTRL1_WBEG_DATE  AND @QTRL1_WEND_DATE
      *     AND  TRLFILE.TL_CLASS BETWEEN @QTRL1_WBEG_CLASS AND @QTRL1_WEND_CLASS
      *     AND (TRLFILE.TL_INSCOMP_1 > 0 OR
      *          TRLFILE.TL_INSCOMP_2 > 0 OR
      *          TRLFILE.TL_INSCOMP_3 > 0 OR
      *          TRLFILE.TL_INSCOMP_4 > 0 OR
      *          TRLFILE.TL_INSCOMP_5 > 0 OR
      *          TRLFILE.TL_INSCOMP_6 > 0 OR
      *          TRLFILE.TL_FEECODE_1 != ' ')
      *    ORDER BY T1.SEQ, TRLFILE.TL_DATE,
      *         TRLFILE.TL_CLASS,
      *         TRLFILE.TL_ACCTNO,
      *         TRLFILE.TL_LAWCODE,
      *         TRLFILE.TL_LINE

           EXEC SQL SET CONNECTION TRL1_CONNECT END-EXEC.

           EXEC SQL
            DECLARE TRL1_CURSOR CURSOR FOR
                :TRL1-RETURN-CODE = EXEC SSP_LSOUTH_TRLFILE_SELECT
              (:PASS-BRANCHLIST,  :QTRL1-WBEG-DATE,  :QTRL1-WEND-DATE,
               :QTRL1-WBEG-CLASS, :QTRL1-WEND-CLASS)

           END-EXEC.

           EXEC SQL
               OPEN TRL1_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE ZEROES TO QTRL1-WBEG-CLASS
                          QTRL1-WEND-CLASS.
           MOVE SPACES TO QTRL1-WBEG-DATE
                          QTRL1-WEND-DATE.

           MOVE STAT---GOOD TO TRL1-CURSOR-STAT.

           EXEC SQL SET CONNECTION C1 END-EXEC.

      *-----------------------------------------------------------------
       READ-TRL1-FILE-NEXT.   *> EMBEDDED TRL LN
           PERFORM READ-IT.
           MOVE TRL-PATH-OWNBR  TO TL-BRNO.
           MOVE TRL-PATH-SQL    TO E-FILE.
           MOVE TRL1-KEY        TO E-KEYX.
           INITIALIZE QTRL-REC QLN-REC.

           EXEC SQL SET CONNECTION TRL1_CONNECT END-EXEC.

           IF ( TRL1-CURSOR-STAT-GOOD )
              EXEC SQL
               FETCH TRL1_CURSOR INTO 
                   :QTL-BRNO,
                   :QTL-DATE,
                   :QTL-CLASS,
                   :QTL-ACCTNO,
                   :QTL-LAWCODE,
                   :QTL-LINE,
                   :QTL-FEECODE-1,
                   :QTL-FEECODE-2,
                   :QTL-FEECODE-3,
                   :QTL-FEECODE-4,
                   :QTL-FEECODE-5,
                   :QTL-FEECODE-6,
                   :QTL-FEECODE-7,
                   :QTL-FEECODE-8,
                   :QTL-FEECODE-9,
                   :QTL-FEECODE-10,
                   :QTL-FEECODE-11,
                   :QTL-FEECODE-12,
                   :QTL-FEECODE-13,
                   :QTL-FEECODE-14,
                   :QTL-FEECODE-15,
                   :QTL-FEECODE-16,
                   :QTL-FEECODE-17,
                   :QTL-FEECODE-18,
                   :QTL-FEECODE-19,
                   :QTL-FEECODE-20,
                   :QTL-FEEAMT-1,
                   :QTL-FEEAMT-2,
                   :QTL-FEEAMT-3,
                   :QTL-FEEAMT-4,
                   :QTL-FEEAMT-5,
                   :QTL-FEEAMT-6,
                   :QTL-FEEAMT-7,
                   :QTL-FEEAMT-8,
                   :QTL-FEEAMT-9,
                   :QTL-FEEAMT-10,
                   :QTL-FEEAMT-11,
                   :QTL-FEEAMT-12,
                   :QTL-FEEAMT-13,
                   :QTL-FEEAMT-14,
                   :QTL-FEEAMT-15,
                   :QTL-FEEAMT-16,
                   :QTL-FEEAMT-17,
                   :QTL-FEEAMT-18,
                   :QTL-FEEAMT-19,
                   :QTL-FEEAMT-20,
                   :QTL-INSCOMP-1,
                   :QTL-INSCOMP-2,
                   :QTL-INSCOMP-3,
                   :QTL-INSCOMP-4,
                   :QTL-INSCOMP-5,
                   :QTL-INSCOMP-6,
                   :QTL-INSCOMP-7,
                   :QTL-INSCOMP-8,
                   :QTL-INSCOMP-9,
                   :QTL-INSCOMP-10,
                   :QTL-INSOURS-1,
                   :QTL-INSOURS-2,
                   :QTL-INSOURS-3,
                   :QTL-INSOURS-4,
                   :QTL-INSOURS-5,
                   :QTL-INSOURS-6,
                   :QTL-INSOURS-7,
                   :QTL-INSOURS-8,
                   :QTL-INSOURS-9,
                   :QTL-INSOURS-10,
                   :QTL-INSPREM-1,
                   :QTL-INSPREM-2,
                   :QTL-INSPREM-3,
                   :QTL-INSPREM-4,
                   :QTL-INSPREM-5,
                   :QTL-INSPREM-6,
                   :QTL-INSPREM-7,
                   :QTL-INSPREM-8,
                   :QTL-INSPREM-9,
                   :QTL-INSPREM-10,
                   :QTL-INS-TAX-COUNTY-CD,
                   :QTL-INS-TAX-CITY-CD, 
                   :QLN-OWNBR,
                   :QLN-ACCTNO,
                   :QLN-CLASS,
                   :QLN-SUBCLASS,
                   :QLN-LAWCODE,
                   :QLN-SSNO1,
                   :QLN-MAKERCD1,
                   :QLN-SSNO2,
                   :QLN-MAKERCD2,
                   :QLN-SSNO3,
                   :QLN-MAKERCD3,
                   :QLN-SSNO4,
                   :QLN-MAKERCD4,
                   :QLN-ORGST,
                   :QLN-SPRCLASS,
                   :QLN-LOANDATE ,
                   :QLN-APRATE,
                   :QLN-LNAMT,
                   :QLN-MAINTFEE,
                   :QLN-ORGTERM,
                   :QLN-REGPYAMT,
                   :QLN-FEECODE-1,
                   :QLN-FEECODE-2,
                   :QLN-FEECODE-3,
                   :QLN-FEECODE-4,
                   :QLN-FEECODE-5,
                   :QLN-FEECODE-6,
                   :QLN-FEECODE-7,
                   :QLN-FEECODE-8,
                   :QLN-FEECODE-9,
                   :QLN-FEECODE-10,
                   :QLN-FEECODE-11,
                   :QLN-FEECODE-12,
                   :QLN-FEECODE-13,
                   :QLN-FEECODE-14,
                   :QLN-FEECODE-15,
                   :QLN-FEECODE-16,
                   :QLN-FEECODE-17,
                   :QLN-FEECODE-18,
                   :QLN-FEECODE-19,
                   :QLN-FEECODE-20,
                   :QLN-FEEAMT-1,
                   :QLN-FEEAMT-2,
                   :QLN-FEEAMT-3,
                   :QLN-FEEAMT-4,
                   :QLN-FEEAMT-5,
                   :QLN-FEEAMT-6,
                   :QLN-FEEAMT-7,
                   :QLN-FEEAMT-8,
                   :QLN-FEEAMT-9,
                   :QLN-FEEAMT-10,
                   :QLN-FEEAMT-11,
                   :QLN-FEEAMT-12,
                   :QLN-FEEAMT-13,
                   :QLN-FEEAMT-14,
                   :QLN-FEEAMT-15,
                   :QLN-FEEAMT-16,
                   :QLN-FEEAMT-17,
                   :QLN-FEEAMT-18,
                   :QLN-FEEAMT-19,
                   :QLN-FEEAMT-20,
                   :QLN-INSURED,
                   :QLN-INSTYPES,
                   :QLN-INSCOMP-1,
                   :QLN-INSCOMP-2,
                   :QLN-INSCOMP-3,
                   :QLN-INSCOMP-4,
                   :QLN-INSCOMP-5,
                   :QLN-INSCOMP-6,
                   :QLN-INSCOMP-7,
                   :QLN-INSCOMP-8,
                   :QLN-INSCOMP-9,
                   :QLN-INSCOMP-10,
                   :QLN-INSOURS-1,
                   :QLN-INSOURS-2,
                   :QLN-INSOURS-3,
                   :QLN-INSOURS-4,
                   :QLN-INSOURS-5,
                   :QLN-INSOURS-6,
                   :QLN-INSOURS-7,
                   :QLN-INSOURS-8,
                   :QLN-INSOURS-9,
                   :QLN-INSOURS-10,
                   :QLN-INSEFF-DATE-1 ,
                   :QLN-INSEFF-DATE-2 ,
                   :QLN-INSEFF-DATE-3 ,
                   :QLN-INSEFF-DATE-4 ,
                   :QLN-INSEFF-DATE-5 ,
                   :QLN-INSEFF-DATE-6 ,
                   :QLN-INSEFF-DATE-7 ,
                   :QLN-INSEFF-DATE-8 ,
                   :QLN-INSEFF-DATE-9 ,
                   :QLN-INSEFF-DATE-10 ,
                   :QLN-INSEXP-DATE-1  ,
                   :QLN-INSEXP-DATE-2  ,
                   :QLN-INSEXP-DATE-3  ,
                   :QLN-INSEXP-DATE-4  ,
                   :QLN-INSEXP-DATE-5  ,
                   :QLN-INSEXP-DATE-6  ,
                   :QLN-INSEXP-DATE-7  ,
                   :QLN-INSEXP-DATE-8  ,
                   :QLN-INSEXP-DATE-9  ,
                   :QLN-INSEXP-DATE-10 ,
                   :QLN-INSPREM-1,
                   :QLN-INSPREM-2,
                   :QLN-INSPREM-3,
                   :QLN-INSPREM-4,
                   :QLN-INSPREM-5,
                   :QLN-INSPREM-6,
                   :QLN-INSPREM-7,
                   :QLN-INSPREM-8,
                   :QLN-INSPREM-9,
                   :QLN-INSPREM-10,
                   :QLN-INSCOVR-1,
                   :QLN-INSCOVR-2,
                   :QLN-INSCOVR-3,
                   :QLN-INSCOVR-4,
                   :QLN-INSCOVR-5,
                   :QLN-INSCOVR-6,
                   :QLN-INSCOVR-7,
                   :QLN-INSCOVR-8,
                   :QLN-INSCOVR-9,
                   :QLN-INSCOVR-10,
                   :QLN-AD-D-INS-COVR,
                   :QLN-CURBAL,
                   :QLN-CL-REBATE,
                   :QLN-AH-REBATE,
                   :QLN-PP-REBATE,
                   :QLN-INT-REBATE,
                   :QLN-SC-REBATE,
                   :QLN-MF-REBATE,
                   :QLN-DF-REBATE,
                   :QLN-EX-REBATE,
                   :QLN-O1-REBATE,
                   :QLN-O2-REBATE,
                   :QLN-O3-REBATE,
                   :QLN-O4-REBATE,
                   :QLN-O5-REBATE,
                   :QLN-O6-REBATE,
                   :QLN-O7-REBATE,
                   :QLN-UN-O7-REB,
                   :QLN-UN-O8-REB,
                   :QLN-UN-O9-REB,
                   :QLN-UN-O10-REB,
                   :QLN-UNITPER-CD,
                   :QLN-UNITPER-FREQ
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           EXEC SQL SET CONNECTION C1 END-EXEC.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-TRL1-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-TRL-FIELDS
              PERFORM GET-LN-FIELDS.

      *-----------------------------------------------------------------
       CLOSE-TRL1-FILE.
           PERFORM CLOSE-IT.
           IF ( TRL1-CURSOR-STAT-GOOD )
               EXEC SQL SET CONNECTION TRL1_CONNECT END-EXEC
               EXEC SQL CLOSE TRL1_CURSOR END-EXEC
               EXEC SQL SET CONNECTION C1 END-EXEC
               MOVE STAT---BAD TO TRL1-CURSOR-STAT.

      *-----------------------------------------------------------------
       CLOSE-TRL1-CONNECTION.
           IF TRL1-CONN-OPEN-SW = "Y"
               EXEC SQL SET CONNECTION TRL1_CONNECT END-EXEC
               EXEC SQL COMMIT END-EXEC
               EXEC SQL DISCONNECT TRL1_CONNECT END-EXEC
               EXEC SQL SET CONNECTION C1 END-EXEC
               MOVE "N" TO TRL1-CONN-OPEN-SW.
      *-----------------------------------------------------------------


       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO EXT-EOBUF-ERRCD.

      ***********************************
       END-PROGRAM SECTION.
      ***********************************
       END-RUN.
           PERFORM CLOSE-FILES.
           IF EXT-ROUTE-BUF = "00"
              PERFORM PRU-OUT.
           MOVE MKTEMP-BUF TO ACCESS-BUF.
           PERFORM REMOVE-WORKFILE.
       EXIT-PROG.
           IF SV-BEG-BR NOT = 9999
              MOVE SV-BEG-BR TO BEG-BR
              MOVE SV-END-BR TO END-BR.
           IF EXT-ROUTE-BUF = "00"
              MOVE "SP/INSRMU" TO FORM-NAM
              MOVE FORM-PATH TO FORM-PATHNAME.
       STOP-RUN.
           IF EXT-ROUTE-BUF = "00" OR "40" OR "50"
              DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN
              DESTROY LSOUTH-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.
           EXIT PROGRAM.
