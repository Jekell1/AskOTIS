       IDENTIFICATION DIVISION.
       PROGRAM-ID.      LONPFB.
       DATE-WRITTEN.    031886.
      *********************************************************************
      *  DESC:  LOAN PROCESSING
      *         TRANSACTION POSTING
      *         PROGRAM 11 -  RC, RA, RP, R1, R2, R3, RI, RS, RM, RD
      *                       CC, CA, CP, C1, C2, C3, CD,
      * N O T E :
      *     THIS MUST BE CHANGED TO HANDLE 'C1' AND 'C2' DIFFERENTLY.
      *     MUST ALLOW FOR PARTIAL CANCELS WHEN THERE ARE ADDON.
      *     .............
      *     INSURANCE ADDONS CAN NOT USE PARTICIPATION FOR DEALERS:
      *********************************************************************
      *REV:
      * 053095 SLC COMBINED OTHBAL AND OT2BAL IN L-090
      * 060695 BLL CHANGED TO USE NEW GL ENTRY ROUTINE
      * MJD 071196 ADDED TEST OF LN-UNITPER-CD FOR WEEKLY PDTH.
      * BAH 082396 ADDED SET OF REB-LN-ORGTERM AND REB-LN-REGPYAMT FOR
      *            REBATE-GET-INS-AMT ON ADDONS (MULLEN)
      * JTG 051297 ADDED USE OF 'SETCTBL' AND 'SETCTBLW'
      * JTG 052097 ADDED 'LIBLP/ADDONSPR' TO GET CORRECT SPR RE: INS RB
      * MJD 060497 ADDED REBATE O3 LOGIC, INS-RB-TBL(6-8), REB-AMOUNT(11-13),
      *            AND U-INSOURS(6-8)
      * JTG 062797 ADDED "A" OPTIONS FOR DL-PARVPAID-CODE RE: #515
      * BAH 092997 PASSWORD OVERRIDING 'RI' AMOUNTS, MERC
      * JTG 092697 CHANGED TO PERFORM 'LIBLP/ADDONRB' COMMON LOGIC
      *            TO COMPUTE APR RATE FOR ACTUARIAL REBATE FORMULAS
      * GLF 101497 ALLOWED CTRL 'N'
      * MJD 101797 MOVED WK-UPDATE-BUF TO LP01WK
      * BLV 110397 MOVE IN BR-GL-TYPE-ALLOWED, BR-GL-ENTRY-DEFAULT TO
      *            ALLENT-BUF BEFORE GL DISTRIBUTION WINDOW
      * JTG 112697 CORRECTED BUG IN RECAPTURE WINDOW WHEN ADDONS EXISTED, COLTN
      * BAH 060398 ALLOW LONPF9 TO CALL LONPFB FOR EACH REBATE ON AUTOMATIC
      *            "PL" CODE, MERCURY #E 180 (BR-AUTO-PL-REF-AC)
      * BAH 062698 TILLNO IS NOW ALPHANUMERIC
      * BAH 073098 DISPLAY BALANCES EVEN ON REVERSALS, #487
      * JTG 073098 CORRECTED MANUAL OVERRIDE OF 'RI' ON P&L ACCOUNTS THAT
      *            CAUSED BOGUE EARNINGS, WORLD / FAIRCO
      * MJD 083398 ADDED ENVIRONMENT VARIABLE TO DETERMINE WHETHER OR NOT
      *            TO DISPLAY BALANCES ON REVERSALS. FIXED DISPLAY TO INDICATE
      *            INCLUDED O2 BAL WITH "SC" "RN" "PB" "RB" "RO" TRANSACTIONS
      * JTG 101298 ALLOWED DISPLAY OF NEGATIVE CURBAL REVOL REGENCY
      * JTG 102998 CORRECTED BR-AUTO-PL-REF-AC TO BE CONSISTENT, IT WAS
      *            YIELDING 'RI' VIA "PO" IN CRNOR2.C AND "RI" IN LONPFB.C.
      *            LONPFB.C NOW YIELDS 'RI' WITH "PO. THIS BUG WAS CAUSING
      *            AN INCORRECT LNAMT & BAD P&L EXPENSE.
      * GLB 990720 ADD CASH DRAWER PASSWORD OPTION TO ENTER DRAWER NO.
      *            BR-PASSWORD-DRAWER(Y/N) & CA-PASSWORD (4 CHAR PASSWORD)
      *            REGMGM PR# 901
      * MJD 990806 ADDED GP-INCL-OTH-IN-EARNED FLAG. IF "Y" THEN INCLUDE
      *            EARNINGS BUCKET 6 (OTHER) IN ACCUMULATED TOTALS
      *            OTHERWISE EXCLUDE BUCKET 6.
      * BAH 001121 ADD OVERRI-DL-SEQ PASSWORD SEQ 2, PASSWORD CHANGING THE
      *            "RI" CALCULATED AMT ONLY ON DIRECT LOANS, REGWAN PR# 1358
      * BAH 010126 AUTOMATIC-PL (HOLD-BP-TRCD = "RA", "RC", ETC...DO NOT
      *            RE-PERFORM REBATE CALCULATIONS FOR INSURANCES, JUST TAKE
      *            WHAT WE CALCULATED IN LONPF9 FOR THE AUTOMATIC P&L.
      *            WAS CAUSING PROBLEMS WITH RBCOMCD GIVING 0 REBATES IN
      *            LONPF9, THEN HERE WAS CALCULATING INDIVIDUAL AMOUNTS.
      * MG  010201 REMOVED "01 WK-LXE" AND ADDED LIBLP/LPWKLXE.
      * JTG 010402 CORRECTED RB-OVERRIDE ROUTINE TO NOT DISPLAY TRAMT
      *            WHEN HOLD-RB-TRCD NOT = " ",                    MERC PR#244
      * JTG 010503 CORRECTED BUG IN REBATE-GET-INS-AMT, ROUTINE WAS NOT TESTING
      *            FOR LN-ADDON-FG CORRECTLY WHEN 'R1' OR 'R2' OR 'R3'
      *            NEW CODE 'B' FOR SP-RBUNITPER-CD(OCCURS 16) PAACO PR#1466
      * JTG 020308 ADDED "CT" COUNTY TAX CODE KENTUCKY AND PERCENTS
      *            ADDED "CY" CITY   TAX CODE KENTUCKY AND PERCENTS
      *            OPENED UP USE OF LP-PRORATED-INS-REB TO HOLD PRORATED PORTION
      *            OF INSURANCE REFUND                                WORLD #288
      * JTG 020402 CHANGED TO TAKE INTO ACCOUNT REBATE OVERIDES FOR
      *            PRORATED PORTION OF INSURANCE SALES TAX REFUND     WORLD #288
      * BAH 020628 DONT ALLOW UP ARROW AND CHANGE TO REBATE IF NOT OUR
      *            INSURANCE  BR-NOT-OURS-REFUND NOT = "Y", REGACC PL# 370
      * JTG 020731 CHANGED FOR (MIP) MONTHLY INSURANCE PREMIUMS   REGENCY #1801
      * JTG 020927 SET THE FOLLOWING FIELDS FOR ADDON ACCOUNTS    MULLEN  #
      *            JGMULL  MOVE LN-LASTPYAMT  TO REB-LN-LASTPYAMT
      *            JGMULL  MOVE LN-APRATE     TO REB-LN-APRATE
      *            JGMULL  MOVE LN-SMPRATE    TO REB-LN-SMPRATE
      * BAH 021106 BUG IN VERIFY-REBATE-CANCEL SECTION, IF NOT LN-MIP-FG, YOU
      *            WERE EXITING THE ROUTINE AND NOT VALIDATING THAT YOU
      *            CANT TAKE ANOTHER REBATE, WAS ALLOWING MULITPLE REBATES
      *            HAD A "GO TO EXIT" LINE THAT I REMOVED.....
      * JTG 021112 ADDED NEW CODE FILE TYPE 'BR' -BATCH PAYMENT REFERENCE CODES
      *            BATCH PAYMENT REVISIONS                         REGACC #0006
      * JTG 030129 ADDED LOGIC FOR REBATES ON ACCOUNTS CONVERTED FOR
      *            ALABAMA WITH ADDON INSURANCE THAT WAS NOT CONVERTED
      *            AS AN ADDON. THE LOGIC IS DEFAULTING TO LOAN DATE
      *            AND NOT INSURANCE EFFECTIVE DATE                    WORLD
      * BAH 030224 FROM THE 021106 FIX I DID, IF LN-MIP-FG WAS "Y", IT SHOULD
      *            EXIT THE VERIFY-REBATE-CANCEL SECTION, BECAUSE NOW IT TELLS
      *            YOU INSURANCE IS MEMO AND YOU CANT CANCEL IT !! FIXED.
      * JTG 030306 ALLOW RBFRMLA '8' TO WORK FOR 'AH' & 'ALL COMPUTABLE INS'
      *                                                         PACESETTER #2012
      * BAH 040311 ADDED GP-BACKDATE-MEMO TO POP UP MEMO WINDOW WHEN
      *            BACKDATING PAYOFFS. THEY WILL BE FORCED TO ENTER SOMETHING.
      *            WORLD #344
      * JTG 040825 ADDED 'RI' 'RS' 'RM' AND 'AC' TO INTERBRANCH TRANS LIST
      *            AND PASSWORD PROTECTED                           WORLD #0357
      * JTG 040913 ADDED NEW FIELDS FOR SP-RBFRMLA (0 D)
      *            RULE 78THS USING DAYS IN TERM AND ELAPSED DAYS INSTEAD
      *            OF UNIT PERIODS IN TERM AND ELAPSE UNIT PERIODS REGACC #0074
      * BLF 041101 BARB & CINDY, FIX PATH NOT SET CORRECTLY FOR INTERBRANCH,
      *            CASH DRAWER ENTRY DID NOT WORK FOR INTERBRANCH!!
      * JTG 050708 ADDED REB-LN-INT-REBATE REB-LN-SC-REBATE REB-LN-MF-REBATE
      *            REB-LN-DF-REBATE
      *            ADDED NEW FORMULAS FOR MEXICO
      *            SP-MFFRMLA-05 SP-RBFRMLA2 (G) SP-ERNFRMLA (24)   WORLD #0408
      * JTG 070426 REMOVED ADDED LOGIC FOR REBATES ON ACCOUNTS CONVERTED FOR
      *            ALABAMA WITH ADDON INSURANCE THAT WAS NOT CONVERTED
      *            AS AN ADDON. THE LOGIC IS DEFAULTING TO LOAN DATE
      *            AND NOT INSURANCE EFFECTIVE DATE            WORLD JWR BARB
      *  CS 081113 ADD GP-INCL-OTH-IN-EARNED = 'R'. IF 'R' EXCLUDE OTHER AND
      *            MAINTFEE(ANTICERN(4) FROM EARNINGS   REGACC PR#210
      *  MJD 050818 CHANGED SP-RBFRMLA2 (G) TO USE THE LN-ANTICERN(4)
      *             MAINTENANCE FEE ANTICIPATED INSTEAD OF THE STORED
      *             LN-MAINTFEE * LN-ORGTERM
      *             THIS HAD BEEN FIXED IN LIBGB/REBATE BUT SOMEHOW WAS
      *             MISSED HERE.
      *   CS 120224 'RS' & 'RM' WROTE EARNINGS TO THE LP RECORD WHEN AN ACCOUNT
      *             WAS IN P&L STATUS. THE BOGUS EARNINGS HAPPEND ONLY WHEN YOU
      *             UP-ARROWED, DID NOT EVEN HAVE TO CHANGE THE AMOUNT. MOVED
      *             TEST REGARDING 'LN-SERVACCT = "Y" OR LN-PLCD NOT = SPACES
      *             SO P&L ACCOUNTS WILL NOT COMPUTE EARNINGS WORKER. THE
      *             TRFILE ALSO HAD BOGUS EARNINGS BUT GL WAS NOT UPDATED
      *                                             WORLD PR#791
      *  BAH 130111 ADDED LC-DISPLAY-ERN OPTION OF "I" FOR POFF-INTDUE
      *                                              AMERICAN ACC PR# 4190
      *  BAH 140814 ADDED TEST FOR LPSMNU IN HELP
      *   CS 141208 CHANGED OPEN-WINDOW 'DLPARV' BEGIN-Y FROM 11 TO 9, ENTIRE
      *             WINDOW WAS NOT BEING DISPLAYED.
      *  BAH 160118 SPLIT OUT LTFILE, PD0003
      *   CS 160222 REMOVE COPY LPWKLXE, ITS AN EARNINGS COPY MEMBER THAT IS
      *             PASSED IN LOAN PROCESSING VIA LP01WK USING WK-LX2-REC. 
      *             REPLACED WK-LX2-REC WITH WK-LXE-REC IN LP01WK 
      *             MOVED LPWKLXE TO LPWKLXE.NO
      *             SAVED VERSION LONPFB.160222
      *  BAH 160304 CHANGED TILLNO TO NUMERIC BUT NO ROOM ON SCREEN FOR 2 DIGITS
      * KEC 2016.0509 {PD#00003} PARADATA <A30> [JAY CISZEWSKI]
      *         CHANGED LN-APPNO TO USE LN-APP-NUMBER FOR MA1-KEY.
      * MJD 160613 ADDED LOGIC TO UPDATE LXR-REB-AMT FOR GA EARLY PROVISION 
      *                                                           REGMGT PR#4915
      * BAH 161117 ADDED 2 DIGIT FIELD "DRAWER" ABOVE PAYMENT LINE FOR 
      *            NEW CASH DRAWER ENTRY
      * BAH 170615 ACTIVATE 8 DIGIT DATES, PD0006
      * BLM 170609 PERFORM CONDITIONAL-SEND-MESS, ONLY DO SEND-MESS IF
      *            ROUTE-BUF = "00", WORLD PR# 1246
      * BAH 171129 FIXED WK-LXE-REC
      * BAH 2018.07.02 ADDED ADDONSPRW AND SET ADDONSPR-SUB INSTEAD OF
      *                REB-SUB2. DONT SHARE REBATEW WORKERS ANYMORE!
      * KEC 2018-0824 LOAN APP MODULE REMOVAL FROM A30 [LARM30];
      *         LEAVING LN-APP-NUMBER & EXT-WI-CL-APP-NUMBER
      *         HERE IN CASE NEEDED IN THE FUTURE.
      * BAH 181231 GLOBAL LPFILE
      *  CS 190103 GLOBAL LTIFILE
      * BAH 190116 GLOBAL LXFILE
      * BAH 20190701 REMOVED U-INSOURS FROM LPPFBUF
      * BAH 20220103 STOP REBATES OF INSURANCE ON INSURANCE NOT WRITTEN, #1531
      * BAH 2022.0314 REMOVED EXT-SPANISH-FORMS AND ALL MX FORMS
      * BAH 20220803 HARD CODED START
      *
      * JKC 2022-0930 CHANGED TO USE START-LP1-FILE-SCAN INSTEAD OF
      *          START-LP1-FILE; NEED TO USE START-LP1-FILE-SCAN WHEN
      *          START-LP1-FILE-NOT-GREATER IS USED FIRST; NEEDED FOR
      *          FOR DISPLAYING MULTIPLE PAGES IN AN UP & DOWN FUNCTION;
      *          NEED TO ADD ONE TO LP-SEQNO (IN WBEG) IN ORDER TO GET 
      *          THE DISPLAY TO WORK LIKE WITH VISION FILES.
      * SMR 2023-0418 MODIFIED FOR CHANGES TO LX FILES
      * BAH 2024.0311 REMOVED EXT-WI-CL-APP-NUMBER
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "LIBLP/LPFSWK.CPY".
       I-O-CONTROL.
       DATA DIVISION.
       FILE SECTION.
       COPY "LIBLP/LPFDWK.CPY".

       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)LP/LONPFB.CBL S35 04/19/23-08:37:09 >".
      **************************************************
      *     P R O G R A M   W O R K E R S              *
      **************************************************
       COPY "LIBLP/LP01DL.CPY".
       COPY "LIBLP/LP01DL_SQL.CPY".
       COPY "LIBLP/LPWSDL.CPY".
       COPY "LIBLP/LP01LP.CPY".
       COPY "LIBLP/LP01LP_SQL.CPY".
       COPY "LIBLP/LPWSLP.CPY".
       COPY "LIBLP/LP01LX.CPY".
       COPY "LIBLP/LPWSLX.CPY".
       COPY "LIBLP/LP01LXG.CPY".
       COPY "LIBLP/LPWSLXG.CPY".
       COPY "LIBLP/LP01LXE.CPY".
       COPY "LIBLP/LPWSLXE.CPY".
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
       COPY "LIBLP/LP01CA.CPY".
       COPY "LIBLP/LP01CA_SQL.CPY".
       COPY "LIBLP/LPWSCA.CPY".
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       01  WK-IFN              PIC X(2)     VALUE "WK".
       01  WK-KEY              PIC 9(2)     VALUE 0.
       COPY "LIBLP/LP01LN.CPY".

       01  REBATE-OVERRIDE-FG     PIC X              VALUE " ".
       01  IO-TYPE                PIC X(3)           VALUE " ".
       01  ELE                    PIC 99             VALUE 0.
       01  FIRST-ELE              PIC 99             VALUE 0.

      * POFF WORKERS FOR 'LIBLP/ADDONRB' ROUTINES:
       01  POFF-ONE               PIC S9.
       01  POFF-SP-DISFRMLA       PIC X.
       01  POFF-PAYDATE           PIC 9(8)     COMP.
       01  POFF-LPTRCD            PIC XX.

       01  DISP-REV-BAL           PIC X              VALUE "Y".
       01  LN-WK                  PIC 9(6)     COMP  VALUE 0.
       01  SUB                    PIC 9(6)     COMP  VALUE 0.
       01  SUBX                   PIC 9(6)     COMP  VALUE 0.
       01  ADJSUB                 PIC 9(6)     COMP  VALUE 0.
       01  WORKER                 PIC S9(7)V99 COMP.
       01  ADDON-INT-WORKER       PIC S9(6)V99 COMP.
       01  TOTEARN                PIC S9(6)V99 COMP.
       01  EARNINGS-WORKER        PIC S9(6)V99 COMP.
       01  DLRECAP-PERCENT        PIC S9(6)V999999999  COMP.
       01  DLRECAP-WORK           PIC S9(7)V99         COMP.
       01  RB-INS-TAX-WORKERS.
           03  RB-WITH-INSTAX     PIC S9(7)V99 COMP.
           03  RB-FEEAMT          PIC S9(7)V99 COMP.
           03  RB-PREMIUM         PIC S9(7)V99 COMP.

       01  GL-WORK                PIC S9(7)V99 COMP.
       01  SV-PASSWORD-DRAWER     PIC X        VALUE "N".

       01  CHARGE-EXCEEDED.
           03  FILLER             PIC X(27) VALUE
           "EXCEEDS ORIGINAL CHARGE OF ".
           03  CHARGE-EXCEED-AMT  PIC ZZZZZ9.99-.

       01  PUR-BAL-EXCEEDED.
           03  FILLER             PIC X(24)    VALUE
           "THE PURCHASE BALANCE IS".
           03  PURBAL-EXCEEDED    PIC ZZZZZ9.99-.


       01  TEST-USER-PROFILE           PIC X.

      **************************************************
      *         ORDER LINE BUFFER                      *
      **************************************************
       01  LN-BUF              VALUE SPACES.
           03  L-010           PIC 9(6).
           03  FILLER          PIC X.
           03  L-020           PIC XX.
           03  FILLER          PIC X.
           03  L-030           PIC X(5).
           03  L-040           PIC Z(5).99- BLANK ZERO.
           03  L-040RATE       REDEFINES L-040
                               PIC ZZZZ.999-.
           03  L-050           PIC Z(4).99- BLANK ZERO.
           03  L-060           PIC Z(4).99- BLANK ZERO.
           03  L-070           PIC Z(5).99- BLANK ZERO.
           03  L-080           PIC 9(4)     BLANK ZERO.
           03  L-090           PIC Z(5).99- BLANK ZERO.
           03  L-100           PIC Z(4).99- BLANK ZERO.
           03  L-110           PIC Z(5)9.99-.
           03  FILLER         REDEFINES L-110.
               05  FILLER      PIC X(9).
               05  L-120       PIC X.
       01  EARN-BUF            REDEFINES LN-BUF.
           03  D-INTYTD        PIC ZZZZ9.99-.
           03  D-TOTEARN       PIC ZZZZ9.99-.
           03 D-LCPDTH-MO      PIC 99.
           03 D-LCPDTH-DAYR    PIC 99.
           03  D-EARNED        PIC ZZZZ9.99-.
           03  D-DLPROC        PIC ZZZZ9.99-.
      *    CANNOT REDEFINE - SEE SETUP-LINE ROUTINE
       01  ENTRY-BUF           VALUE " ".
           03  EB-010          PIC 9(6).
           03  EB-020          PIC XX.
           03  EB-030          PIC X(5).
           03  EB-040          PIC Z(5).99- BLANK ZERO.
           03  EB-040RATE      REDEFINES EB-040
                               PIC ZZZZ.999-.
           03  EB-050          PIC Z(4).99- BLANK ZERO.
           03  EB-060          PIC Z(4).99- BLANK ZERO.
           03  EB-070          PIC Z(5).99- BLANK ZERO.
           03  EB-080          PIC 9(4)     BLANK ZERO.
           03  FILLER  REDEFINES EB-080.
               05 EB-080-MO    PIC 99.
               05 EB-080-DAYR  PIC 99.
           03  EB-090          PIC Z(5).99- BLANK ZERO.
           03  EB-100          PIC Z(4).99- BLANK ZERO.
           03  EB-110          PIC Z(5)9.99-.
           03  FILLER         REDEFINES EB-110.
               05  FILLER      PIC X(9).
               05  EB-120      PIC X.

       01  LINE-BUF            VALUE SPACES.
           03  D-LINE          OCCURS 8 TIMES
                               PIC X(80).
       01  LINE-LIST.
           03  FILLER          PIC X(28) VALUE
               "LN1 GL8 F21 GD8 F22 GA8 F23 ".
           03  FILLER          PIC X(24) VALUE
               "GL9 F31 GD9 F32 GA9 F33 ".
           03  FILLER          PIC X(24) VALUE
               "GL0 F41 GD0 F42 GA0 F43 ".
           03  FILLER          PIC X(32)    VALUE
               "GL1 F51 GD1 F52 GA1 F53 GL2 F61 ".
           03  FILLER          PIC X(40)    VALUE
               "GD2 F62 GA2 F63 GL3 F71 GD3 F72 GA3 F73 ".
           03  FILLER          PIC X(24)    VALUE
               "GL4 F81 GD4 F82 GA4 F83.".

       01  ENTRY-LINE          PIC X(48)    VALUE
           "010 020 030 040 050 060 070 080 090 100 110 120.".


       01  EARN-LIST.
           03  FILLER          PIC X(25) VALUE
           "H09 H10 H17 H21 PROCEEDS.".


       01  CHG010                     PIC S9(6)V99  COMP-3.
       01  CHG020                     PIC S9(6)V99  COMP-3.
       01  CHG030                     PIC S9(6)V99  COMP-3.
       01  CHG040                     PIC S9(6)V99  COMP-3.
       01  CHG050                     PIC S9(6)V99  COMP-3.
       01  CHG060                     PIC S9(6)V99  COMP-3.
       01  CHG070                     PIC S9(6)V99  COMP-3.
       01  CHG080                     PIC S9(6)V99  COMP-3.
       01  CHG-MESS.
           03  FILLER                 PIC X(16)  VALUE
           "ENTERED REFUNDS ".
           03  CHG-MESS2              PIC X(10)  VALUE
           "ARE OVER".
           03  FILLER                 PIC X(3)   VALUE
           "BY ".
           03  CHG080-MESS            PIC ZZZZZ9.99-.

       01  CALC-ADDON-INT-REBATE      PIC S9(6)V99 COMP-3 VALUE 0.
       01  CALC-REBATE                PIC S9(6)V99 COMP-3 VALUE 0.
       01  CALC-DLRVCHGS-ADJ          OCCURS 3
                                      PIC S9(6)V99 COMP-3 VALUE 0.
       01  CALC-DLPARVHELD-ADJ        PIC S9(6)V99 COMP-3 VALUE 0.
       01  CALC-DLPARVPAID-ADJ        PIC S9(6)V99 COMP-3 VALUE 0.

       01  OVERRIDES.
           03  OVERRIDE-DLRVCHGS      OCCURS 3
                                      PIC S9(7)V99    COMP-3.
           03  OVERRIDE-DLPARVHELD    PIC S9(7)V99    COMP-3.
           03  OVERRIDE-DLPARVPAID    PIC S9(7)V99    COMP-3.
           03  OVERRIDE-UNEARN        PIC S9(7)V99    COMP-3.

       01  OVERRIDE-LIST.
           03  FILLER                PIC X(48)   VALUE
           "01A 01B 010 02A 02B 020 03A 03B 030 04A 04B 040 ".
           03  FILLER                PIC X(48)   VALUE
           "05A 05B 050 06A 06B 060 07A 07B 070 08A 08B 080 ".
           03  FILLER                PIC X(12)   VALUE
           "09A 09B 090.".
       01  OVERRIDE-BUF.
           03  FILLER                OCCURS 3.
               05  ORIG-DLRVCHGS     PIC Z(5)9.99-.
               05  CALC-DLRVCHGS     PIC Z(5)9.99-.
               05  CHG-DLRVCHGS      PIC Z(5)9.99-.

           03  ORIG-DLPARVHELD       PIC Z(5)9.99-.
           03  CALC-DLPARVHELD       PIC Z(5)9.99-.
           03  CHG-DLPARVHELD        PIC Z(5)9.99-.

           03  ORIG-DLPARVPAID       PIC Z(5)9.99-.
           03  CALC-DLPARVPAID       PIC Z(5)9.99-.
           03  CHG-DLPARVPAID        PIC Z(5)9.99-.

           03  ORIG-UNEARN           PIC Z(5)9.99-.
           03  CALC-UNEARN           PIC Z(5)9.99-.
           03  CHG-UNEARN            PIC Z(5)9.99-.

           03  ORIG-ADDON            PIC Z(5)9.99-.
           03  CALC-ADDON            PIC Z(5)9.99-.
           03  CHG-ADDON             PIC Z(5)9.99-.

           03  ORIG-TOTAL            PIC Z(5)9.99-.
           03  CALC-TOTAL            PIC Z(5)9.99-.
           03  CHG-TOTAL             PIC Z(5)9.99-.


      * GLB  07/22/99
       COPY "LIBGB/CAPASSWDW.CPY".
       COPY "LIBGB/GPENVW.CPY".
       COPY "LIBLP/LPITRMW.CPY".
       COPY "LIBLP/LPWKPATH.CPY".
       COPY "LIBLP/LPIBPCW.CPY".
       COPY "LIBLP/LPXDTEW.CPY".
       COPY "LIBGB/SPPDTHW.CPY".
       COPY "LIBLP/LPMDTEW.CPY".
       COPY "LIBLP/LPBWINW.CPY".
       COPY "LIBWI/PFSWINDOWSW.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBGL/ALLENTW.CPY".
       COPY "LIBLP/LPAPRZW.CPY".
       COPY "LIBLP/LPUPERW.CPY".
       COPY "LIBLP/LPUPWKW.CPY".
       COPY "LIBLP/LPAPRSW.CPY".
       COPY "LIBLP/SETCTBLW.CPY".
       COPY "LIBLP/LPEADJW.CPY".
       COPY "LIBLP/LPAMTSW.CPY".
       COPY "LIBLP/ADDONSPRW.CPY".
       COPY "LIBGB/REBATEW.CPY".
       COPY "LIBLP/LPMENUW.CPY".
      *COPY "LIBLP/LPWKLXE.CPY".
       COPY "LIBGB/PASSWDW.CPY".
       01  OVERRI-SEQ               PIC 9 VALUE 1.
      * OVERRIDE "RI" ON DIRECT LOANS ONLY
       01  OVERRI-DL-SEQ            PIC 9 VALUE 2.

       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBWI/MEMOSCW.CPY".

       COPY "LIBCL/CLMENUW_EXT.CPY".
       COPY "LIBGB/SYSTEMW.CPY".
       COPY "LIBGB/RBACTW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBLP/LONPF_DEF.CPY".
       COPY "LIBLP/LPSMNU_DEF.CPY".
       COPY "LIBLP/LPLNMU_DEF.CPY".
       COPY "LIBLP/LPCAMU_DEF.CPY".
       COPY "LIBLP/LPVDMU_DEF.CPY".
       COPY "LIBWI/DLPARV_DEF.CPY".
       COPY "LIBLP/LONPF_WKS.CPY".
       COPY "LIBLP/LPSMNU_WKS.CPY".
       COPY "LIBLP/LPLNMU_WKS.CPY".
       COPY "LIBLP/LPCAMU_WKS.CPY".
       COPY "LIBLP/LPVDMU_WKS.CPY".
       COPY "LIBWI/DLPARV_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".


       COPY "LIBGB/GETFMW.CPY".
       COPY "LIBLP/LPPFBUF.CPY".
       COPY "LIBLP/LPLOANW.CPY".

       SCREEN SECTION.
       COPY "LIBLP/LONPF_SCN.CPY".
       COPY "LIBWI/DLPARV_SCN.CPY".
       COPY "LIBLP/LPSMNU_SCN.CPY".
       COPY "LIBLP/LPLNMU_SCN.CPY".
       COPY "LIBLP/LPCAMU_SCN.CPY".
       COPY "LIBLP/LPVDMU_SCN.CPY".

      **************************************************
      *   C O N T R O L   M O D U L E                  *
      **************************************************
       PROCEDURE DIVISION
             USING FORM-PATHNAME EXIT-PATHNAME PROG-BUF UPDATE-BUF.
       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
              WK-FILE.
       COPY "LIBGB/DECLARE.CPY".
           PERFORM CLOSE-SP1-FILE.
           PERFORM CLOSE-LTP1-FILE.
           PERFORM CLOSE-LTI1-FILE.
           PERFORM CLOSE-LP1-FILE.
           PERFORM CLOSE-DL1-FILE.
           PERFORM CLOSE-CA1-FILE.
           PERFORM CLOSE-BR-FILE.
           CLOSE WK-FILE.
       COPY "LIBGB/DECLAR2.CPY".

      **************************************************
      *    P R O G R A M   C O N T R O L
      **************************************************
       MAIN-PROGRAM SECTION.

           PERFORM SQL-CONNECT.

      D    STOP MESS.
           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "LONPF" TO VDU-FORM-NAME.

           MOVE SPACES TO ERRCD ROUTE-CD.

           MOVE USER-PID TO TEMP-PID.

           IF INTERRUPT-FG = "Y"
              MOVE " " TO VDU-WIMENU-SELECTION
           ELSE
              MOVE "C" TO VDU-WIMENU-SELECTION.
           MOVE "F" TO VDU-CLMENU-SELECTION.

           PERFORM RESTORE-SCREEN.

           PERFORM GET-GPENV.

           MOVE HOLD-SP1-KEY TO SP1-KEY.
           PERFORM OPEN-SP1-FILE.
      * MUST LEAVE SP1-FILE OPEN BECAUSE ADDONSPR READS IT
           PERFORM READ-SP1-FILE.
           IF IO-FG NOT = 0
              MOVE "INVALID STATE PARAMETER RECORD" TO MESS
              PERFORM CONDITIONAL-SEND-MESS
              MOVE "X" TO ERRCD
              GO TO END-ROUTINE.

      * ALLOW CNTRL 'N' INTERRUPT:
           MOVE "WI/MAKERS" TO VDU-CTRL-N-PROG.

           MOVE BRANCH TO BR-NO.
           PERFORM OPEN-BR-FILE.
           PERFORM READ-BR-FILE.
           PERFORM CLOSE-BR-FILE.
           IF IO-FG NOT = 0
              MOVE "INVALID BRANCH RECORD" TO MESS
              PERFORM CONDITIONAL-SEND-MESS
              MOVE "X" TO ERRCD
              GO TO END-ROUTINE.

           MOVE "Y" TO REB-RECAST
                       REB-DEF-STOP.

      *    -------------------------------------------------------------
      *    TEST ENVIRONMENT VARIABLE "RVLINES" TO DETERMINE WHETHER
      *    OR NOT TO DISPLAY BALANCES ON REVERSALS.
      *    -------------------------------------------------------------
           MOVE "RVLINES" TO GETENV-BUF.
           PERFORM GETENV-CALL.
           IF STAT = "00"
              MOVE "N" TO DISP-REV-BAL
           ELSE
              MOVE "Y" TO DISP-REV-BAL.

           IF HOLD-BP-TRCD NOT = " "
              PERFORM BATCH-PAYMENT-ROUTINE
           ELSE
              PERFORM SELECTION-MODULE.

           GO TO END-ROUTINE.

      **********************************************
       BATCH-PAYMENT-ROUTINE SECTION.
      **********************************************
           PERFORM SET-BAL-IBPC-LPREC.

           IF HOLD-BP-TRCD = "F9"
              MOVE "PO" TO LP-TRCD
              PERFORM REBATE-CANCEL-ROUTINE
              MOVE "RI" TO HOLD-BP-TRCD
                           LP-TRCD
           ELSE
      * DO NOT RE-PERFORM REBATE CALCULATIONS FOR INSURANCES, JUST TAKE
      * WHAT WE CALCULATED IN LONPF9 FOR THE AUTOMATIC P&L.
      * WAS CAUSING PROBLEMS WITH RBCOMCD GIVING 0 REBATES IN LONPF9,
      * THEN HERE WAS CALCULATING INDIVIDUAL AMOUNTS.
              IF NOT (HOLD-BP-TRCD = "RC" OR "RA" OR "RP" OR "R1" OR
                                      "R2" OR "R3" )
                 PERFORM REBATE-CANCEL-ROUTINE.

           MOVE LP-TRAMT TO LP-APCUR.

           SUBTRACT LP-APCUR FROM LP-CURBAL.

           MOVE LN-1STPYDATE TO PDTH-DATE-WORK.
           PERFORM PAID-THRU-CALCULATION.
           MOVE PDTH-DATE-FULL TO LP-PDTH-DATE.

           IF LP-TRCD = "RD" OR "CD"
              SUBTRACT LP-TRAMT FROM HOLD-LN-TOTDFMNTS HOLD-LN-TOTERN.

      * COMPUTE INTEREST PAID BY BORROWER
           PERFORM COMPUTE-BW-INTPAID.
           MOVE BWIN-INTPAID TO LP-INTPAID.

       BATCH-PAYMENT-ROUTINE-EXIT.
           PERFORM SAVE-SCREEN.

      **********************************************
       SELECTION-MODULE SECTION.
      **********************************************
           PERFORM INITIALIZE-ROUTINE.
           IF ROUTE-CD = "7"
              GO TO RETURN-TO-ENTRY-ROUTINE.

           PERFORM REBATE-CANCEL-ROUTINE.

       ENTRY-LINE-CAL.
           IF LP-REFNO = "CHECK"
              MOVE 0 TO LP-APCUR
           ELSE
              MOVE LP-TRAMT TO LP-APCUR.

       DISPLAY-ENTRY.
           PERFORM DISPLAY-ENTRY-LINE.
           IF ROUTE-CD = "7"
              GO TO RETURN-TO-ENTRY-ROUTINE.

           PERFORM ACCEPT-ENTRY-ROUTINE.
           IF ROUTE-CD = "7"
              GO TO RETURN-TO-ENTRY-ROUTINE.
           IF ROUTE-CD = "4"
              PERFORM TRANS-AMOUNT-ENTRY
              IF ROUTE-CD = "8"
                 GO TO RETURN-TO-REF-ROUTINE
              ELSE
                 IF ROUTE-CD = "7"
                    GO TO RETURN-TO-ENTRY-ROUTINE
                 ELSE
                    IF ROUTE-CD = "3"
                       GO TO ENTRY-LINE-CAL
                    ELSE
                       GO TO DISPLAY-ENTRY.

           MOVE LPLOAN-USERID TO TEST-USER-PROFILE.

      * MEMO ENTRY FOR BACKDATED REBATES
           IF (GP-BACKDATE-MEMO = "N" OR " ") OR
                  LP-PAYDATE = LP-TRDATE
               GO TO SKIP-BACKDATE-MEMO.

           IF GP-BACKDATE-MEMO = "Y" OR
              TEST-USER-PROFILE > GP-BACKDATE-MEMO
                MOVE LN-OWNBR         TO EXT-WI-CL-OWNBR
                MOVE LN-ACCTNO        TO EXT-WI-CL-ACCTNO
      *         MOVE LN-APP-NUMBER    TO EXT-WI-CL-APP-NUMBER
                MOVE 1                TO EXT-WI-CL-SSNO-NO
                MOVE LN-SSNO(1)       TO EXT-WI-CL-SSNO(1)
                MOVE "L"              TO MEMOSC-ONLY
      * WANT IT TO WORK EXACTLY LIKE REVERSAL MEMO, FORCES IT !
                MOVE "P"              TO MEMOSC-MEMO-TYPE
                MOVE "WI/MEMOSC" TO FORM-NAM
                CALL FORM-PROGX USING FORM-PATH MEMOSC-BUF EXIT-PATHNAME
                CANCEL FORM-PROGX.

       SKIP-BACKDATE-MEMO.
           MOVE "3" TO ROUTE-CD.
           MOVE 2 TO NEW-PROG-SUB.
           GO TO SELECTION-EXIT-PROG.

      * F1 BACK TO DATE:
       RETURN-TO-ENTRY-ROUTINE.
           MOVE "1" TO NEW-PROG-SUB.
           GO TO SELECTION-EXIT-PROG.

      * F4 TO RE-ENTER REFNO:
       RETURN-TO-REF-ROUTINE.
           MOVE "1" TO NEW-PROG-SUB.

      * EXITS PROGRAM WITH SAME FORM:
       SELECTION-EXIT-PROG.
           MOVE "P" TO ERRCD.
           PERFORM SAVE-SCREEN.

       SELECTION-MODULE-EXIT.
           EXIT.

      **********************************************
       INITIALIZE-ROUTINE SECTION.
      **********************************************
           PERFORM SET-BAL-IBPC-LPREC.

           PERFORM VERIFY-REBATE-CANCEL.
           IF ROUTE-CD = "7"
              GO TO INITIALIZE-ROUTINE-EXIT.

           MOVE "S  A060PMPT2." TO VDU.
           PERFORM SEND-INT-OR-LC.
           MOVE "S  A060PMPT4." TO VDU.
           PERFORM SEND-INT-OR-LC.

       INITIALIZE-ROUTINE-EXIT.
           EXIT.

      **********************************************
       VERIFY-REBATE-CANCEL SECTION.
      **********************************************
           MOVE 0 TO SUB.

      * VALIDATE CANCEL / REINSTATE FOR MIP INSURANCE
           IF LN-MIP-FG = "Y"
              IF LP-TRCD = "CC"
                 IF LN-INSPREM(1) = 0
                    MOVE "CREDIT LIFE WAS NOT WRITTEN" TO MESS
                    PERFORM CONDITIONAL-SEND-MESS
                    MOVE "7" TO ROUTE-CD
                    GO TO VERIFY-REBATE-CANCEL-EXIT
                 END-IF
                 IF LN-CL-REBATE = "Y" AND
                    LP-REFNO NOT = "CLRE"
                    MOVE "CREDIT LIFE ALREADY CANCELLED" TO MESS
                    PERFORM CONDITIONAL-SEND-MESS
                    MOVE "7" TO ROUTE-CD
                    GO TO VERIFY-REBATE-CANCEL-EXIT
                 END-IF
                 IF LN-CL-REBATE = " " AND
                    LP-REFNO = "CLRE"
                    MOVE "CREDIT LIFE ALREADY IN EFFECT" TO MESS
                    PERFORM CONDITIONAL-SEND-MESS
                    MOVE "7" TO ROUTE-CD
                    GO TO VERIFY-REBATE-CANCEL-EXIT
                 END-IF
              ELSE
              IF LP-TRCD = "CA"
                 IF LN-INSPREM(2) = 0
                    MOVE "A&H WAS NOT WRITTEN" TO MESS
                    PERFORM CONDITIONAL-SEND-MESS
                    MOVE "7" TO ROUTE-CD
                    GO TO VERIFY-REBATE-CANCEL-EXIT
                 END-IF
                 IF LN-AH-REBATE = "Y" AND
                    LP-REFNO NOT = "AHRE"
                    MOVE "A&H ALREADY CANCELLED" TO MESS
                    PERFORM CONDITIONAL-SEND-MESS
                    MOVE "7" TO ROUTE-CD
                    GO TO VERIFY-REBATE-CANCEL-EXIT
                 END-IF
                 IF LN-AH-REBATE = " " AND
                    LP-REFNO = "AHRE"
                    MOVE "A&H ALREADY IN EFFECT" TO MESS
                    PERFORM CONDITIONAL-SEND-MESS
                    MOVE "7" TO ROUTE-CD
                    GO TO VERIFY-REBATE-CANCEL-EXIT.
           IF LN-MIP-FG = "Y"
              GO TO VERIFY-REBATE-CANCEL-EXIT.

      ********************************************
      *    CODES FOR LN-(CL,AH,PP,O1,O2)-REBATE
      *
      *      SP - NO REBATE OR CANCEL
      *      Y - REBATE OR CANCEL
      *
      *    REBATE NOT ALLOWED AFTER CANCEL.
      *    CANCEL NOT ALLOWED AFTER REBATE.
      ********************************************
           IF NOT (LP-TRCD = "RC" OR "CC")
              GO TO VERIFY-REBATE-CANCEL-AH
           ELSE
              IF LN-INSOURS(1) = "M"
                 GO TO VERIFY-TRCD-MEMO
              ELSE
                 IF LN-CL-REBATE = "Y"
                    GO TO VERIFY-TRCD-ERROR.

           IF LN-INSTYPES-DL
              MOVE 1 TO SUB
           ELSE
              IF LN-INSTYPES-LL
                 MOVE 3 TO SUB
              ELSE
                 MOVE "INVALID INSURANCE TYPE FLAG" TO MESS
                 PERFORM CONDITIONAL-SEND-MESS
                 MOVE "7" TO ROUTE-CD
                 GO TO VERIFY-REBATE-CANCEL-EXIT.

           IF LN-INSURED = "J"
              ADD 1 TO SUB.
           GO TO VERIFY-TRCD-RBFRMLA.

       VERIFY-REBATE-CANCEL-AH.
           IF LP-TRCD = "RA" OR "CA"
              IF LN-INSOURS(2) = "M"
                 GO TO VERIFY-TRCD-MEMO
              ELSE
                 IF LN-AH-REBATE = "Y"
                    GO TO VERIFY-TRCD-ERROR
                 ELSE
                    IF LN-INSTYPES-AH
                       MOVE 5 TO SUB
                       GO TO VERIFY-TRCD-RBFRMLA
                    ELSE
                       MOVE "INVALID INSURANCE TYPE FLAG" TO MESS
                       PERFORM CONDITIONAL-SEND-MESS
                       MOVE "7" TO ROUTE-CD
                       GO TO VERIFY-REBATE-CANCEL-EXIT.

           IF LP-TRCD = "RP" OR "CP"
              IF LN-INSOURS(3) = "M"
                 GO TO VERIFY-TRCD-MEMO
              ELSE
                 IF LN-PP-REBATE = "Y"
                    GO TO VERIFY-TRCD-ERROR
                 ELSE
                    IF LN-INSTYPES-PP OR LN-INSTYPES-PP2
                       MOVE 6 TO SUB
                       GO TO VERIFY-TRCD-RBFRMLA
                    ELSE
                       MOVE "INVALID INSURANCE TYPE FLAG" TO MESS
                       PERFORM CONDITIONAL-SEND-MESS
                       MOVE "7" TO ROUTE-CD
                       GO TO VERIFY-REBATE-CANCEL-EXIT.

           IF LP-TRCD = "R1" OR "C1"
              IF LN-INSOURS(4) = "M"
                 GO TO VERIFY-TRCD-MEMO
              ELSE
                 IF LN-O1-REBATE = "Y"
                    GO TO VERIFY-TRCD-ERROR
                 ELSE
                    IF LN-INSTYPES-O1
                       MOVE 12 TO SUB
                       GO TO VERIFY-TRCD-RBFRMLA
                    ELSE
                       MOVE "INVALID INSURANCE TYPE FLAG" TO MESS
                       PERFORM CONDITIONAL-SEND-MESS
                       MOVE "7" TO ROUTE-CD
                       GO TO VERIFY-REBATE-CANCEL-EXIT.

           IF LP-TRCD = "R2" OR "C2"
              IF LN-INSOURS(5) = "M"
                 GO TO VERIFY-TRCD-MEMO
              ELSE
                 IF LN-O2-REBATE = "Y"
                    GO TO VERIFY-TRCD-ERROR
                 ELSE
                    IF LN-INSPREM(5) NOT = 0
                       MOVE 13 TO SUB
                       GO TO VERIFY-TRCD-RBFRMLA
                    ELSE
                       MOVE "INVALID INSURANCE TYPE FLAG" TO MESS
                       PERFORM CONDITIONAL-SEND-MESS
                       MOVE "7" TO ROUTE-CD
                       GO TO VERIFY-REBATE-CANCEL-EXIT.

           IF LP-TRCD = "R3" OR "C3"
              IF LN-INSOURS(6) = "M"
                 GO TO VERIFY-TRCD-MEMO
              ELSE
                 IF LN-O3-REBATE = "Y"
                    GO TO VERIFY-TRCD-ERROR
                 ELSE
                    IF LN-INSPREM(6) NOT = 0
                       MOVE 14 TO SUB
                       GO TO VERIFY-TRCD-RBFRMLA
                    ELSE
                       MOVE "INVALID INSURANCE TYPE FLAG" TO MESS
                       PERFORM CONDITIONAL-SEND-MESS
                       MOVE "7" TO ROUTE-CD
                       GO TO VERIFY-REBATE-CANCEL-EXIT.
 

      **************************************************
      *     CODES FOR LN-INT-REBATE,SC-REBATE,MF-REBATE
      *
      *       SP - NO REBATE OR CANCEL
      *       C - CANCEL
      *       Y - REBATE NO CANCEL
      *       R - REBATE AFTER CANCEL
      *
      *     REBATE ALLOWED AFTER CANCEL
      *     CANCEL NOT ALLOWED AFTER REBATE
      *
      *     NOTE:
      *          CI, CS AND CM NO LONGER ALLOWED
      **************************************************
           IF LP-TRCD = "RI" OR "RD" OR "CD"
              IF LP-IBPC = "I"
                 MOVE "PRE COMPUTE LOANS ONLY" TO MESS
                 PERFORM CONDITIONAL-SEND-MESS
                 MOVE "7" TO ROUTE-CD
                 GO TO VERIFY-REBATE-CANCEL-EXIT.

           IF LP-TRCD = "RI"
              IF LN-INT-REBATE = "Y" OR "R"
                 GO TO VERIFY-TRCD-ERROR
              ELSE
                 MOVE 7 TO SUB
                 GO TO VERIFY-TRCD-RBFRMLA.

           IF LP-TRCD = "RM"
              IF LN-MF-REBATE = "Y" OR "R"
                 GO TO VERIFY-TRCD-ERROR
              ELSE
                 MOVE 9 TO SUB
                 GO TO VERIFY-TRCD-RBFRMLA.

           IF LP-TRCD = "RS"
              IF LN-SC-REBATE = "Y" OR "R"
                 GO TO VERIFY-TRCD-ERROR
              ELSE
                 MOVE 8 TO SUB
                 GO TO VERIFY-TRCD-RBFRMLA.

           IF LP-TRCD = "CD"
              IF LN-DF-REBATE NOT = " "
                 GO TO VERIFY-TRCD-ERROR
              ELSE
                 MOVE 10 TO SUB
                 GO TO VERIFY-TRCD-RBFRMLA.

           IF LP-TRCD = "RD"
              IF LN-DF-REBATE = "Y" OR "R"
                 GO TO VERIFY-TRCD-ERROR
              ELSE
                 MOVE 10 TO SUB
                 GO TO VERIFY-TRCD-RBFRMLA.

       VERIFY-TRCD-MEMO.
           MOVE "THE INSURANCE IS MEMO ONLY!" TO MESS.
           PERFORM CONDITIONAL-SEND-MESS.
           MOVE "7" TO ROUTE-CD.
           GO TO VERIFY-REBATE-CANCEL-EXIT.

       VERIFY-TRCD-RBFRMLA.
           IF SUB NOT = 0
              IF SP-RBFRMLA(SUB) = " "
                 MOVE "NO REBATE FORMULA IN STATE PARAMETER" TO MESS
                 PERFORM CONDITIONAL-SEND-MESS
                 MOVE "7" TO ROUTE-CD.
           GO TO VERIFY-REBATE-CANCEL-EXIT.

       VERIFY-TRCD-ERROR.
           MOVE "INVALID PER PRIOR POSTING" TO MESS.
           PERFORM CONDITIONAL-SEND-MESS.
           MOVE "7" TO ROUTE-CD.

       VERIFY-REBATE-CANCEL-EXIT.
           EXIT.

      *************************************************
       REBATE-CANCEL-ROUTINE SECTION.
      *************************************************
           MOVE 0 TO LP-APOTH LP-APINT LP-APLC LP-TRAMT LP-LCPAID
                     MS-PENALTY.

           IF LN-MIP-FG = "Y"
              IF LP-TRCD = "CC" AND LP-REFNO = "CLRE"
                 MOVE LP-PAYDATE   TO NUM-DATE
                 MOVE LN-1STPYDA   TO NUM-DA
                 MOVE NUM-DATE     TO HOLD-LXM-MIP-CLPDTH
              ELSE
              IF LP-TRCD = "CA" AND LP-REFNO = "AHRE"
                 MOVE LP-PAYDATE   TO NUM-DATE
                 MOVE LN-1STPYDA   TO NUM-DA
                 MOVE NUM-DATE     TO HOLD-LXM-MIP-AHPDTH.


      * SETUP FOR DEALER PARTICIPATION RESERVE ADJUSTMENT:
      * ONLY ALLOWED FOR PC ACCOUNTS
           MOVE " " TO HOLD-DL-RVCHGS-CODE(1)
                       HOLD-DL-RVCHGS-CODE(2)
                       HOLD-DL-RVCHGS-CODE(3)
                       HOLD-DL-PARVHELD-CODE
                       HOLD-DL-PARVPAID-CODE.
           IF LP-TRCD = "RI"
                   AND (LN-SALEFIN = "Y")
                    AND (LN-SERVACCT NOT = "Y")
                     AND (LN-INTCHG NOT = 0)
              MOVE LN-DLNO TO DL-DLNO
              PERFORM OPEN-DL1-FILE
              PERFORM READ-DL1-FILE
              PERFORM CLOSE-DL1-FILE
              IF IO-FG = 0
                 MOVE DL-RVCHGS-CODE(1) TO HOLD-DL-RVCHGS-CODE(1)
                 MOVE DL-RVCHGS-CODE(2) TO HOLD-DL-RVCHGS-CODE(2)
                 MOVE DL-RVCHGS-CODE(3) TO HOLD-DL-RVCHGS-CODE(3)
                 MOVE DL-PARVHELD-CODE TO HOLD-DL-PARVHELD-CODE
                 MOVE DL-PARVPAID-CODE TO HOLD-DL-PARVPAID-CODE.

      * BEGIN REBATE TESTS:
           IF LP-TRCD = "RI" OR HOLD-BP-TRCD = "F9"
              PERFORM REBATE-INTEREST-ADJ
              GO TO REBATE-CANCEL-EXIT
           ELSE
              IF LP-TRCD = "RS"
                 PERFORM REBATE-SERCHG-ADJ
                 GO TO REBATE-CANCEL-EXIT
              ELSE
                 IF LP-TRCD = "RM"
                    PERFORM REBATE-MAINTFEE-ADJ
                    GO TO REBATE-CANCEL-EXIT
                 ELSE
                    IF LP-TRCD = "RD" OR "CD"
                       PERFORM REBATE-DEFERMENT
                       GO TO REBATE-CANCEL-EXIT.


      * INSURANCE CANCELLATIONS:
           IF LP-TRCD = "CC"
              IF LN-INSOURS(1) NOT = "M"
                 MOVE LN-INSPREM(1)   TO LP-TRAMT
                 IF BR-FORCE-VALID-CITY-COUNTY = "Y"
                    MOVE LN-FEEAMT(5) TO LP-PRORATED-INS-REB
                 END-IF
                 GO TO REBATE-CANCEL-EXIT.

           IF LP-TRCD = "CA"
              IF LN-INSOURS(2) NOT = "M"
                 MOVE LN-INSPREM(2)   TO LP-TRAMT
                 IF BR-FORCE-VALID-CITY-COUNTY = "Y"
                    MOVE LN-FEEAMT(6) TO LP-PRORATED-INS-REB
                 END-IF
                 GO TO REBATE-CANCEL-EXIT.

           IF LP-TRCD = "CP"
              IF LN-INSOURS(3) NOT = "M"
                 MOVE LN-INSPREM(3)   TO LP-TRAMT
                 IF BR-FORCE-VALID-CITY-COUNTY = "Y"
                    MOVE LN-FEEAMT(7) TO LP-PRORATED-INS-REB
                 END-IF
                 GO TO REBATE-CANCEL-EXIT.

           IF LP-TRCD = "C1"
              IF LN-INSOURS(4) NOT = "M"
                 MOVE LN-INSPREM(4)   TO LP-TRAMT
                 IF BR-FORCE-VALID-CITY-COUNTY = "Y"
                    MOVE LN-FEEAMT(8) TO LP-PRORATED-INS-REB
                 END-IF
                 GO TO REBATE-CANCEL-EXIT.

           IF LP-TRCD = "C2"
              IF LN-INSOURS(5) NOT = "M"
                 MOVE LN-INSPREM(5)   TO LP-TRAMT
                 IF BR-FORCE-VALID-CITY-COUNTY = "Y"
                    MOVE LN-FEEAMT(9) TO LP-PRORATED-INS-REB
                 END-IF
                 GO TO REBATE-CANCEL-EXIT.

           IF LP-TRCD = "C3"
              IF LN-INSOURS(6) NOT = "M"
                 MOVE LN-INSPREM(6)    TO LP-TRAMT
                 IF BR-FORCE-VALID-CITY-COUNTY = "Y"
                    MOVE LN-FEEAMT(10) TO LP-PRORATED-INS-REB
                 END-IF
                 GO TO REBATE-CANCEL-EXIT.

      * INSURANCE REBATES:
      D    STOP MESS.
           IF LP-TRCD = "RC"
              PERFORM REBATE-LN-CL
            ELSE
              IF LP-TRCD = "RA"
                 PERFORM REBATE-LN-AH
               ELSE
                 IF LP-TRCD = "RP"
                    PERFORM REBATE-LN-PP
                 ELSE
                   IF LP-TRCD = "R1"
                      PERFORM REBATE-LN-O1
                    ELSE
                      IF LP-TRCD = "R2"
                         PERFORM REBATE-LN-O2
                       ELSE
                         IF LP-TRCD = "R3"
                            PERFORM REBATE-LN-O3.

       REBATE-CANCEL-EXIT.
           EXIT.

      *************************************************
       TRANS-AMOUNT-ENTRY SECTION.
      *************************************************
           MOVE " " TO ROUTE-CD.
           MOVE "Y" TO REBATE-OVERRIDE-FG.
           MOVE "EN S052040." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10"
              MOVE "7" TO ROUTE-CD
              GO TO ENTER-APPLIED-EXIT.
           IF VDUSTATUS = "1U"
              MOVE "8" TO ROUTE-CD
              GO TO ENTER-APPLIED-EXIT.
           IF VDUSTATUS NOT = "00" AND NOT = "1D"
              GO TO TRANS-AMOUNT-ENTRY.
           IF VDUNUM2 < 0
              GO TO TRANS-AMOUNT-ENTRY.

           IF LP-TRCD = "RI"
              IF VDUNUM2 NOT = LP-TRAMT
                 MOVE OVERRI-SEQ TO PASSWDW-SEQ
                 MOVE "TO OVERRIDE INT REBATE" TO PASSWDW-PROMPT2
                 PERFORM VERIFY-PASSWORD
                 IF PASSWDW-STAT NOT = "OK"
                    GO TO TRANS-AMOUNT-ENTRY.
           IF LP-TRCD = "RI"
              IF VDUNUM2 NOT = LP-TRAMT
                 IF LN-SALEFIN = "N"
                    MOVE OVERRI-DL-SEQ TO PASSWDW-SEQ
                    MOVE "TO OVERRIDE 'RI' DIRECT LOANS"
                            TO PASSWDW-PROMPT2
                    PERFORM VERIFY-PASSWORD
                    IF PASSWDW-STAT NOT = "OK"
                       GO TO TRANS-AMOUNT-ENTRY.

           MOVE VDUNUM2 TO LP-TRAMT.
           IF LP-REFNO = "CHECK"
              MOVE 0 TO LP-APCUR
           ELSE
              MOVE LP-TRAMT TO LP-APCUR.

           IF LP-APCUR > LP-CURBAL
              PERFORM EXCEEDS-PRIN-BAL
              GO TO TRANS-AMOUNT-ENTRY.

           IF LN-SERVACCT = "N"
              IF LP-APCUR > LN-PURBAL
                 PERFORM EXCEEDS-PUR-BAL
                 GO TO TRANS-AMOUNT-ENTRY.

           MOVE 0 TO LP-PRORATED-INS-REB.

           IF LP-TRCD = "RC" OR "CC"
              MOVE LN-INSPREM(1) TO WORKER
              IF BR-FORCE-VALID-CITY-COUNTY = "Y"
                 ADD LN-FEEAMT(5) TO WORKER
                  MOVE LN-INSPREM(1) TO RB-PREMIUM
                  MOVE LN-FEEAMT(5)  TO RB-FEEAMT
                  MOVE LP-TRAMT      TO RB-WITH-INSTAX
                  PERFORM SET-INSTAX-RB-WORKAMT2
              END-IF
              IF LP-TRAMT > WORKER
                 MOVE WORKER TO CHARGE-EXCEED-AMT
                 GO TO ENTER-APPLIED-EXCEED-ERR.

           IF LP-TRCD = "RA" OR "CA"
              MOVE LN-INSPREM(2) TO WORKER
              IF BR-FORCE-VALID-CITY-COUNTY = "Y"
                 ADD LN-FEEAMT(6) TO WORKER
                  MOVE LN-INSPREM(2) TO RB-PREMIUM
                  MOVE LN-FEEAMT(6)  TO RB-FEEAMT
                  MOVE LP-TRAMT      TO RB-WITH-INSTAX
                  PERFORM SET-INSTAX-RB-WORKAMT2
              END-IF
              IF LP-TRAMT > WORKER
                 MOVE WORKER TO CHARGE-EXCEED-AMT
                 GO TO ENTER-APPLIED-EXCEED-ERR.

           IF LP-TRCD = "RP" OR "CP"
              MOVE LN-INSPREM(3) TO WORKER
              IF BR-FORCE-VALID-CITY-COUNTY = "Y"
                 ADD LN-FEEAMT(7) TO WORKER
                  MOVE LN-INSPREM(3) TO RB-PREMIUM
                  MOVE LN-FEEAMT(7)  TO RB-FEEAMT
                  MOVE LP-TRAMT      TO RB-WITH-INSTAX
                  PERFORM SET-INSTAX-RB-WORKAMT2
              END-IF
              IF LP-TRAMT > WORKER
                 MOVE WORKER TO CHARGE-EXCEED-AMT
                 GO TO ENTER-APPLIED-EXCEED-ERR.

           IF LP-TRCD = "R1" OR "C1"
              MOVE LN-INSPREM(4) TO WORKER
              IF BR-FORCE-VALID-CITY-COUNTY = "Y"
                 ADD LN-FEEAMT(8) TO WORKER
                  MOVE LN-INSPREM(4) TO RB-PREMIUM
                  MOVE LN-FEEAMT(8)  TO RB-FEEAMT
                  MOVE LP-TRAMT      TO RB-WITH-INSTAX
                  PERFORM SET-INSTAX-RB-WORKAMT2
              END-IF
              IF LP-TRAMT > WORKER
                 MOVE WORKER TO CHARGE-EXCEED-AMT
                 GO TO ENTER-APPLIED-EXCEED-ERR.

           IF LP-TRCD = "R2" OR "C2"
              MOVE LN-INSPREM(5) TO WORKER
              IF BR-FORCE-VALID-CITY-COUNTY = "Y"
                 ADD LN-FEEAMT(9) TO WORKER
                  MOVE LN-INSPREM(5) TO RB-PREMIUM
                  MOVE LN-FEEAMT(9)  TO RB-FEEAMT
                  MOVE LP-TRAMT      TO RB-WITH-INSTAX
                  PERFORM SET-INSTAX-RB-WORKAMT2
              END-IF
              IF LP-TRAMT > WORKER
                 MOVE WORKER TO CHARGE-EXCEED-AMT
                 GO TO ENTER-APPLIED-EXCEED-ERR.

           IF LP-TRCD = "R3" OR "C3"
              MOVE LN-INSPREM(6) TO WORKER
              IF BR-FORCE-VALID-CITY-COUNTY = "Y"
                 ADD LN-FEEAMT(10) TO WORKER
                  MOVE LN-INSPREM(6) TO RB-PREMIUM
                  MOVE LN-FEEAMT(10) TO RB-FEEAMT
                  MOVE LP-TRAMT      TO RB-WITH-INSTAX
                  PERFORM SET-INSTAX-RB-WORKAMT2
              END-IF
              IF LP-TRAMT > WORKER
                 MOVE WORKER TO CHARGE-EXCEED-AMT
                 GO TO ENTER-APPLIED-EXCEED-ERR.

           IF LP-TRCD = "RI"
              IF SP-RBFRMLA(SUB) = "A"
                 COMPUTE WORKER = LN-SUM-INTCHG + LN-SUM-EXTCHG
                                                      + LN-TOTDEF
                 IF LP-TRAMT > WORKER
                    MOVE WORKER TO CHARGE-EXCEED-AMT
                    GO TO ENTER-APPLIED-EXCEED-ERR
                 ELSE
                    PERFORM REBATE-INTEREST-ADJ
              ELSE
              IF SP-RBFRMLA(SUB) = "D"
                 COMPUTE WORKER = LN-SUM-INTCHG + LN-SUM-EXTCHG
                                                   + LN-SUM-SERCHG
                 IF LP-TRAMT > WORKER
                    MOVE WORKER TO CHARGE-EXCEED-AMT
                    GO TO ENTER-APPLIED-EXCEED-ERR
                 ELSE
                    PERFORM REBATE-INTEREST-ADJ
              ELSE
              COMPUTE WORKER = LN-SUM-INTCHG + LN-SUM-EXTCHG
              IF LP-TRAMT > WORKER
                 MOVE WORKER TO CHARGE-EXCEED-AMT
                 GO TO ENTER-APPLIED-EXCEED-ERR
              ELSE
                 PERFORM REBATE-INTEREST-ADJ.


           IF LP-TRCD = "RS"
              IF LP-TRAMT > LN-SUM-SERCHG
                 MOVE LN-SUM-SERCHG TO CHARGE-EXCEED-AMT
                 GO TO ENTER-APPLIED-EXCEED-ERR
              ELSE
                 PERFORM REBATE-SERCHG-ADJ.

           IF LP-TRCD = "RM"
              IF LP-TRAMT > LN-SUM-MAINTFEE
                 MOVE LN-SUM-MAINTFEE TO CHARGE-EXCEED-AMT
                 GO TO ENTER-APPLIED-EXCEED-ERR
              ELSE
                 PERFORM REBATE-MAINTFEE-ADJ.

           IF LP-TRCD = "RD" OR "CD"
              IF LP-TRAMT > LN-TOTDEF
                 MOVE LN-TOTDEF TO CHARGE-EXCEED-AMT
                 GO TO ENTER-APPLIED-EXCEED-ERR.

           MOVE "SN S052070." TO VDU.
           MOVE LP-APCUR TO VDUNUM2.
           PERFORM SCREENIO.
           GO TO ENTER-APPLIED-EXIT.

       ENTER-APPLIED-EXCEED-ERR.
           MOVE CHARGE-EXCEEDED TO MESS.
           PERFORM CONDITIONAL-SEND-MESS.
           GO TO TRANS-AMOUNT-ENTRY.

       ENTER-APPLIED-EXIT.
           IF ROUTE-CD NOT = " " AND LXG-GLNO(1) NOT = 0
              PERFORM SCREEN-LINES-RESET
              PERFORM DISPLAY-ENTRY-UP.

      *************************************************
       DISPLAY-ENTRY-LINE SECTION.
      *************************************************
           SUBTRACT LP-APCUR FROM LP-CURBAL.

           IF LP-CURBAL < 0
              PERFORM EXCEEDS-PRIN-BAL
              MOVE "7" TO ROUTE-CD
              GO TO DISPLAY-ENTRY-LN-EXIT.

      * TO POST REBATES WITH "CHECK" AFTER A PAYOFF
           IF LN-CURBAL NOT = 0
              IF LP-CURBAL = 0
                 PERFORM EXCEEDS-PRIN-BAL
                 MOVE "7" TO ROUTE-CD
                 GO TO DISPLAY-ENTRY-LN-EXIT.

           IF LN-SERVACCT = "N"
              IF LP-APCUR > 0
                 IF LP-APCUR > LN-PURBAL
                    PERFORM EXCEEDS-PUR-BAL
                    MOVE "7" TO ROUTE-CD
                    GO TO DISPLAY-ENTRY-LN-EXIT.

      * COMPUTE PAID THRU DATE
           MOVE LN-1STPYDATE TO PDTH-DATE-WORK.
           PERFORM PAID-THRU-CALCULATION.
           MOVE PDTH-DATE-FULL TO LP-PDTH-DATE.


           IF LP-TRCD = "RD" OR "CD"
              SUBTRACT LP-TRAMT FROM HOLD-LN-TOTDFMNTS HOLD-LN-TOTERN.

      * COMPUTE INTEREST PAID BY BORROWER
           PERFORM COMPUTE-BW-INTPAID.
           MOVE BWIN-INTPAID TO LP-INTPAID.
           ADD LP-INTPAID TO LN-YTDINTPD.

           MOVE 0 TO LP-DLPROC.
           PERFORM DISPLAY-EARNINGS.

           PERFORM SETUP-LINE.
           PERFORM SEND-ENTRY-LINE.

           IF (LN-INSCOMP(4) = 53 AND LN-INSPREM(4) NOT = 0)
            OR (LN-INSCOMP(5) = 53 AND LN-INSPREM(5) NOT = 0)
                MOVE "INSURANCE COMPANY 53???? REBATE" TO MESS
                PERFORM CONDITIONAL-SEND-MESS.

       DISPLAY-ENTRY-LN-EXIT.
           EXIT.

      *************************************************
       ACCEPT-ENTRY-ROUTINE SECTION.
      *************************************************

      * WE REALLY NEED TO TEST THE PASSWORD
      * DRAWER FLAG OF THE POSTING BRANCH IF DOING INTERBRANCH,
      * SO READ POSTING BRANCH THEN RESTORE OWNING BRANCH:

           IF PATH-MACHINE NOT = " "
              MOVE BRANCH TO BR-NO
              PERFORM OPEN-BR-FILE
              PERFORM READ-BR-FILE
              MOVE BR-PASSWORD-DRAWER TO SV-PASSWORD-DRAWER
              MOVE LN-OWNBR TO BR-NO
              PERFORM READ-BR-FILE
              PERFORM CLOSE-BR-FILE
           ELSE
              MOVE BR-PASSWORD-DRAWER TO SV-PASSWORD-DRAWER.

           MOVE "F1, ^-BACK, F7-GL ENTRY, DRAWER ENTRY   :" TO MESS.
           IF (LN-MIP-FG = "Y") AND (LP-TRCD = "CC" OR "CA")
              MOVE "F1-CANCEL, F7-GL ENTRY, DRAWER NO:" TO MESS.
           IF SV-PASSWORD-DRAWER = "Y"
              IF (LN-MIP-FG = "Y") AND (LP-TRCD = "CC" OR "CA")
                 MOVE "F1, F7-GL ENTRY, DRAWER ENTRY   :"
                                                          TO MESS
              ELSE
                 MOVE
                 "F1, ^-BACK, F7-GL ENTRY, DRAWER ENTRY   :"
                                                             TO MESS.

           IF SV-PASSWORD-DRAWER = "Y"
              INSPECT MESS REPLACING FIRST "ENTRY   :" BY "PASSWORD:"
           ELSE
              INSPECT MESS REPLACING FIRST "ENTRY   :" BY "NUMBER  :".
           PERFORM SEND-LEGEND.

           MOVE 0 TO CA-TILLNO.
           MOVE " " TO ROUTE-CD.

       ACCEPT-ENTER-TILLNO.
           MOVE LP-TRDATE TO CA-DATE.
           MOVE BRANCH    TO CA-BRNO.
           MOVE 0         TO CA-TILLNO.
           MOVE CA1-KEY TO HOLD-CA1-KEY.
           IF SV-PASSWORD-DRAWER = "Y"
              IF PATH-MACHINE NOT = " "
                 MOVE EXT-FILPATH-BASE TO CA-ALL-BASE
                 MOVE PATH-MACHINE TO CA-ALL-MACHINE
                 MOVE PATH-DATA TO CA-ALL-DATA
                 MOVE CA-ALL-PATH TO CA-PATH
                 MOVE "Y" TO CA-PATH-OVERRIDE
              END-IF
              PERFORM ENTER-CASH-DRAWER-PASSWORD
              IF CAPASSWD-OKAY = "N"
                 GO TO ACCEPT-ENTER-TILLNO
              END-IF
              IF CAPASSWD-OKAY = "V"
                 GO TO ACCEPT-CHECK-VDUSTATUS
              END-IF
              IF CAPASSWD-OKAY = "Y"
      *          MOVE "S  A010120." TO VDU
      *          MOVE CA-TILLNO TO NUMERIC-TILLNO
      *          PERFORM CONVERT-TILLNO-TO-ALPHA
      *          MOVE TEST-ALPHA-TILLNO TO VDUBUF
      * ADDED NEW FIELD "DRAWER" FOR 2 DIGIT DRAWER ENTRY
                 MOVE "SN N020DRAWER." TO VDU
                 MOVE CA-TILLNO TO VDUNUM0
                 PERFORM SCREENIO
              END-IF
           ELSE
      * ADDED NEW FIELD "DRAWER" SO WE CAN ENTER A 2 DIGIT DRAWER
              MOVE "EN N020DRAWER." TO VDU
              PERFORM SCREENIO.

       ACCEPT-CHECK-VDUSTATUS.
           IF VDUSTATUS = "10"
              PERFORM SCREEN-LINES-RESET
              IF LXG-GLNO(1) NOT = 0
                 PERFORM DISPLAY-ENTRY-UP
                 GO TO ACCEPT-ENTRY-EXIT
              ELSE
                 GO TO ACCEPT-ENTRY-EXIT.

           IF VDUSTATUS = "1U"
              IF (LN-MIP-FG = "Y") AND (LP-TRCD = "CC" OR "CA")
                 GO TO ACCEPT-F4-BYPASS
              ELSE
                 GO TO ACCEPT-F4-RTN.

           GO TO ACCEPT-F4-BYPASS.

       ACCEPT-F4-RTN.
      * DONT ALLOW UP ARROW AND CHANGE TO REBATE IF NOT OUR INSURANCE
           IF BR-NOT-OURS-REFUND NOT = "Y"
              IF LP-TRCD = "RC" AND LN-INSOURS(1) = "N"
                 PERFORM INSURANCE-NOT-OURS
                 GO TO ACCEPT-F4-BYPASS
              ELSE
              IF LP-TRCD = "RA" AND LN-INSOURS(2) = "N"
                 PERFORM INSURANCE-NOT-OURS
                 GO TO ACCEPT-F4-BYPASS
              ELSE
              IF LP-TRCD = "RP" AND LN-INSOURS(3) = "N"
                 PERFORM INSURANCE-NOT-OURS
                 GO TO ACCEPT-F4-BYPASS
              ELSE
              IF LP-TRCD = "R1" AND LN-INSOURS(4) = "N"
                 PERFORM INSURANCE-NOT-OURS
                 GO TO ACCEPT-F4-BYPASS
              ELSE
              IF LP-TRCD = "R2" AND LN-INSOURS(5) = "N"
                 PERFORM INSURANCE-NOT-OURS
                 GO TO ACCEPT-F4-BYPASS
              ELSE
              IF LP-TRCD = "R3" AND LN-INSOURS(6) = "N"
                 PERFORM INSURANCE-NOT-OURS
                 GO TO ACCEPT-F4-BYPASS.

           MOVE LN-ACCERN(1) TO HOLD-LN-ACCERN(1).
           MOVE LN-ACCERN(2) TO HOLD-LN-ACCERN(2).
           MOVE LN-ACCERN(3) TO HOLD-LN-ACCERN(3).
           MOVE LN-ACCERN(4) TO HOLD-LN-ACCERN(4).
           MOVE LN-ACCERN(5) TO HOLD-LN-ACCERN(5).
           MOVE LN-ACCERN(6) TO HOLD-LN-ACCERN(6).
           MOVE LN-TOTERN TO HOLD-LN-TOTERN.
           MOVE LN-TOTINT TO HOLD-LN-TOTINT.
           MOVE LN-TOTLCHG TO HOLD-LN-TOTLCHG.
           SUBTRACT LP-INTPAID FROM LN-YTDINTPD.
           ADD LP-APCUR TO LP-CURBAL.

      * ADDED TEST TO SKIP SETTING EARNINGS FOR  'PL' AND 'SERVICE'
           IF LN-PLCD = " " AND LN-SERVACCT = "N"
              IF LP-TRCD = "RI"
                 MOVE LN-EARN-ORDER(1) TO SUB
                 IF SUB > 3
      *             ADD LP-TRAMT TO WK-LXE-EARN(SUB - 3)
                    ADD LP-TRAMT TO LXE-EARN(SUB - 3)
                 ELSE
                    ADD LP-TRAMT TO LP-EARNED(SUB)
                 END-IF
              END-IF
              IF LP-TRCD = "RS"
                 MOVE LN-EARN-ORDER(3) TO SUB
                 IF SUB > 3
      *             ADD LP-TRAMT TO WK-LXE-EARN(SUB - 3)
                    ADD LP-TRAMT TO LXE-EARN(SUB - 3)
                 ELSE
                    ADD LP-TRAMT TO LP-EARNED(SUB)
                 END-IF
              END-IF
              IF LP-TRCD = "RM"
                 MOVE LN-EARN-ORDER(4) TO SUB
                 IF SUB > 3
      *             ADD LP-TRAMT TO WK-LXE-EARN(SUB - 3)
                    ADD LP-TRAMT TO LXE-EARN(SUB - 3)
                 ELSE
                    ADD LP-TRAMT TO LP-EARNED(SUB).

           IF LXG-GLNO(1) NOT = 0
              PERFORM DISBURSE-OTHER-INIT
              PERFORM SCREEN-LINES-RESET
              MOVE LP-REC TO WK-LP-REC
              PERFORM DISPLAY-ENTRY-UP
              MOVE WK-LP-REC TO LP-REC.
           MOVE "4" TO ROUTE-CD.
           GO TO ACCEPT-ENTRY-EXIT.

       ACCEPT-F4-BYPASS.
           MOVE LP-APOTH TO GL-WORK.

           IF VDUSTATUS = "1>"
              GO TO ACCEPT-GL-DISBURSE.

           IF VDUSTATUS NOT = "00"
              GO TO ACCEPT-ENTRY-ROUTINE.

      *    MOVE VDUBUF TO TEST-ALPHA-TILLNO.
      *    PERFORM CONVERT-TILLNO-TO-NUMERIC.
      *    MOVE NUMERIC-TILLNO TO CA-TILLNO LP-TILLNO.
           MOVE VDUNUM0 TO CA-TILLNO LP-TILLNO.

           IF GL-WORK = 0
              GO TO ACCEPT-ENTRY-CONT.
           IF LXG-GLNO(1) NOT = 0
              GO TO ACCEPT-ENTRY-CONT.

           IF CA-TILLNO NOT = 0
              PERFORM ALARM.

       ACCEPT-GL-DISBURSE.
           MOVE "Y" TO ALLENT-STATUS.
           PERFORM DISBURSE-OTHER-APPLIED.
           IF ALLENT-STATUS = "N"
              PERFORM DISBURSE-OTHER-INIT.
           GO TO ACCEPT-ENTRY-ROUTINE.

       ACCEPT-ENTRY-CONT.
           IF PATH-MACHINE NOT = " "
              MOVE EXT-FILPATH-BASE TO CA-ALL-BASE
              MOVE PATH-MACHINE TO CA-ALL-MACHINE
              MOVE PATH-DATA TO CA-ALL-DATA
              MOVE CA-ALL-PATH TO CA-PATH
              MOVE "Y" TO CA-PATH-OVERRIDE.

           MOVE LP-TRDATE TO CA-DATE.
           MOVE BRANCH TO CA-BRNO.
           PERFORM OPEN-CA1-FILE.
           PERFORM READ-CA1-FILE.
           PERFORM CLOSE-CA1-FILE.
           IF IO-FG NOT = 0
              MOVE "INVALID CASH DRAWER #" TO MESS
              PERFORM CONDITIONAL-SEND-MESS
              GO TO ACCEPT-ENTRY-ROUTINE.
           IF CA-CLOSED-HHMM NOT = 0
              MOVE "CASH DRAWER CLOSED" TO MESS
              PERFORM CONDITIONAL-SEND-MESS
              GO TO ACCEPT-ENTRY-ROUTINE.
      * TO POST REBATES WITH "CHECK" AFTER A PAYOFF
           IF LN-CURBAL NOT = 0
              IF LP-CURBAL = 0
                 PERFORM USE-PAYOFF
                 GO TO ACCEPT-ENTRY-ROUTINE.

       ACCEPT-ENTRY-EXIT.
           IF ROUTE-CD NOT = " "
              PERFORM CLEAR-LEGEND.

      ********************************************
       DISBURSE-OTHER-APPLIED SECTION.
      ********************************************
       DISBURSE-OTHER-INIT.
           MOVE 0 TO LXG-GLNO(1) LXG-GLNO(2) LXG-GLNO(3)
                     LXG-GLAMT(1) LXG-GLAMT(2) LXG-GLAMT(3).
       DISBURSE-GLNO-ENTRY.
           MOVE GL-WORK TO ALLENT-DISBURSE.
           MOVE BRANCH TO ALLENT-BRNO.
           MOVE BR-OTHER-BRANCH-GL TO ALLENT-OTHER-BR-FG.
           MOVE BR-GL-TYPE-ALLOWED TO ALLENT-GL-TYPE-ALLOWED.
           MOVE BR-GL-ENTRY-DEFAULT TO ALLENT-GL-ENTRY-DEFAULT.
           MOVE 3 TO ALLENT-NO-OF-ACCTS.
           MOVE "GL/ALLENT" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH ALLENT-BUF.
           CANCEL FORM-PROGX.
           IF ALLENT-STATUS = "N"
              GO TO DISBURSE-OTHER-EXIT.
           MOVE 1 TO SUB.
       MOVE-GL-LOOP.
           MOVE ALLENT-BR-ACCOUNT(SUB) TO LXG-GLNO(SUB).
           IF LP-TRCD = "W2"
              COMPUTE LXG-GLAMT(SUB) = ALLENT-AMT(SUB) * (-1)
           ELSE
              MOVE ALLENT-AMT(SUB) TO LXG-GLAMT(SUB).
           IF SUB NOT = 3
              ADD 1 TO SUB
              GO TO MOVE-GL-LOOP.
       DISBURSE-TOTAL-VERIFY.
           COMPUTE VDUNUM2 = LXG-GLAMT(1) + LXG-GLAMT(2) + LXG-GLAMT(3).
           IF GL-WORK = VDUNUM2
              GO TO DISBURSE-OTHER-EXIT.
           MOVE "OUT-OF-BALANCE ENTRY" TO MESS.
           PERFORM CONDITIONAL-SEND-MESS.
       DISBURSE-OTHER-EXIT.
           EXIT.

      ****************************************
       DISPLAY-ENTRY-DOWN SECTION.
      ****************************************

           MOVE SPACES TO LINE-BUF LN-BUF.
           MOVE 0 TO LN-WK.
           MOVE HOLD1-KEY-DOWN TO LP1-KEY.
           IF LP-SEQNO = 99999
              MOVE 9 TO IO-FG
              GO TO DISPLAY-DOWN-EXIT.

      * ***NOTE*** 'START-LP1-FILE-SCAN' MUST BE USED WHEN FIRST USING
      * 'START-LP1-FILE-NOT-GREATER'; USED WHEN DISPLAYING MULTIPLE
      * PAGES IN AN UP & DOWN FUNCTION; YOU NEED TO ADD ONE TO 
      * LP-SEQNO (IN WBEG) IN ORDER TO GET THE DISPLAY TO WORK
      * LIKE VISION FILES.
      
           MOVE LP1-KEY TO HOLD2-KEY-UP.
           PERFORM OPEN-LP1-FILE.
           PERFORM READ-LP1-FILE.
           IF IO-FG NOT = 0
              MOVE LP-PATH-OWNBR TO QLP1-WBEG-BRNO
                                    QLP1-WEND-BRNO
              MOVE LP-ACCTNO     TO QLP1-WBEG-ACCTNO
                                    QLP1-WEND-ACCTNO
              COMPUTE QLP1-WBEG-SEQNO = LP-SEQNO + 1
              MOVE ALL "9"       TO QLP1-WEND-SEQNO
      *-   -   -{ ADD ONE TO WHERE SCAN BEG FIELDS }
              IF ( LP-BRNO = 9999 )
                 MOVE LP-BRNO    TO QLP1-WSBEG-BRNO
              ELSE
                 COMPUTE QLP1-WSBEG-BRNO = LP-BRNO + 1
              END-IF
   
              IF ( LP-ACCTNO = 99999999 )
                 MOVE LP-ACCTNO  TO QLP1-WSBEG-ACCTNO
              ELSE
                 COMPUTE QLP1-WSBEG-ACCTNO = LP-ACCTNO + 1
              END-IF
   
              PERFORM START-LP1-FILE-SCAN.

           MOVE 0 TO ENTRY-FLAG.

       DISPLAY-NEXT-DOWN.
           IF ENTRY-FLAG = 2
              MOVE 9 TO IO-FG
              GO TO DISPLAY-BYPASS-DOWN.

           PERFORM READ-LP1-FILE-NEXT.
           IF IO-FG = 0
              IF (LP-ACCTNO NOT = LN-ACCTNO) OR
                   (LP-BRNO NOT = LP-PATH-OWNBR)
                 ADD 1 TO ENTRY-FLAG
                 GO TO DISPLAY-NEXT-DOWN.

       DISPLAY-BYPASS-DOWN.
           ADD 1 TO LN-WK.
           IF IO-FG = 0
              PERFORM SETUP-LINE.
           MOVE LN-BUF TO D-LINE(LN-WK).
           MOVE SPACE TO LN-BUF.
           IF LN-WK = 8
              GO TO DISPLAY-XFER-DOWN.
           IF IO-FG = 0
              GO TO DISPLAY-NEXT-DOWN.
           IF FULL-PAGE-FG = "Y"
              MOVE "N" TO FULL-PAGE-FG
              MOVE LN-OWNBR  TO LP-BRNO
              MOVE LN-ACCTNO TO LP-ACCTNO
              MOVE 99999     TO LP-SEQNO
              MOVE LP1-KEY TO HOLD2-KEY-UP
              PERFORM DISPLAY-ENTRY-UP
              GO TO DISPLAY-DOWN-EXIT.
       DISPLAY-XFER-DOWN.
           PERFORM DISPLAY-ALL-LINES.
       DISPLAY-DOWN-EXIT.
           IF IO-FG = 0
              MOVE "Y" TO FULL-PAGE-FG
              MOVE LP1-KEY TO HOLD1-KEY-DOWN
           ELSE
              MOVE LN-OWNBR  TO LP-BRNO
              MOVE LN-ACCTNO TO LP-ACCTNO
              MOVE 99999     TO LP-SEQNO
              MOVE LP1-KEY TO HOLD1-KEY-DOWN.
           PERFORM CLOSE-LP1-FILE.

      ****************************************
       DISPLAY-ENTRY-UP SECTION.
      ****************************************
           MOVE SPACES TO LINE-BUF LN-BUF.
           MOVE 9 TO LN-WK.
           MOVE HOLD2-KEY-UP TO LP1-KEY.
           IF LP-SEQNO = 0
              MOVE 9 TO IO-FG
              GO TO DISPLAY-UP-EXIT.

           PERFORM OPEN-LP1-FILE.
           MOVE LP1-KEY TO HOLD1-KEY-DOWN.

           MOVE LP-PATH-OWNBR TO QLP1-WBEG-BRNO
                                 QLP1-WEND-BRNO.
           MOVE LP-ACCTNO     TO QLP1-WBEG-ACCTNO
                                 QLP1-WEND-ACCTNO.
           MOVE ALL "0"       TO QLP1-WBEG-SEQNO.
           MOVE LP-SEQNO      TO QLP1-WEND-SEQNO.

           PERFORM START-LP1-FILE-NOT-GREATER.
           MOVE 0 TO ENTRY-FLAG.

       DISPLAY-NEXT-UP.
           IF ENTRY-FLAG = 2
              MOVE 9 TO IO-FG
              GO TO DISPLAY-BYPASS-UP.
           PERFORM READ-LP1-FILE-PREVIOUS.
           IF IO-FG = 0
              IF LP-ACCTNO NOT = LN-ACCTNO OR
                  (LP-BRNO NOT = LP-PATH-OWNBR)
                 ADD 1 TO ENTRY-FLAG
                 GO TO DISPLAY-NEXT-UP.
       DISPLAY-BYPASS-UP.
           SUBTRACT 1 FROM LN-WK.
           IF LN-WK < 1
              MOVE "Y" TO FULL-PAGE-FG
              GO TO DISPLAY-XFER-UP.
           IF IO-FG = 0
              PERFORM SETUP-LINE.
           MOVE LN-BUF TO D-LINE(LN-WK).
           MOVE SPACE TO LN-BUF.
           IF IO-FG = 0
              GO TO DISPLAY-NEXT-UP.
           IF FULL-PAGE-FG = "Y"
              MOVE "N" TO FULL-PAGE-FG
              MOVE LN-OWNBR  TO LP-BRNO
              MOVE LN-ACCTNO TO LP-ACCTNO
              MOVE 0         TO LP-SEQNO
              MOVE LP1-KEY TO HOLD1-KEY-DOWN
              PERFORM DISPLAY-ENTRY-DOWN
              GO TO DISPLAY-UP-EXIT.
       DISPLAY-XFER-UP.
           PERFORM DISPLAY-ALL-LINES.
       DISPLAY-UP-EXIT.
           IF IO-FG = 0
              MOVE "Y" TO FULL-PAGE-FG
              MOVE LP1-KEY TO HOLD2-KEY-UP
           ELSE
              MOVE LN-OWNBR  TO LP-BRNO
              MOVE LN-ACCTNO TO LP-ACCTNO
              MOVE 0         TO LP-SEQNO
              MOVE LP1-KEY TO HOLD2-KEY-UP.
           PERFORM CLOSE-LP1-FILE.

      *********************************
       SETUP-LINE SECTION.
      *********************************
           MOVE SPACES TO LN-BUF ENTRY-BUF.
           MOVE LP-PAYDATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO L-010 EB-010.
           MOVE LP-TRCD TO L-020 EB-020.
           MOVE LP-REFNO TO L-030 EB-030.
           IF LP-TRCD = "JD" OR "AC" OR "IR"
              MOVE LP-NEWSTAT-RATE TO L-040RATE EB-040RATE
           ELSE
              MOVE LP-TRAMT TO L-040 EB-040.

           IF (  (LP-TRCD = "PB" OR "RN" OR "SC" OR "RB" OR "RO")
                 OR ( (LP-TRCD = "RV") AND
                       (LP-REVTRCD = "PB" OR "RN" OR "SC"
                                            OR "RB" OR "RO")
                    )
              )  AND (LP-APOT2 NOT = 0)
              ADD LP-APOTH LP-APOT2 GIVING L-050 EB-050
              INSPECT L-050 REPLACING TRAILING " " BY "*"
              INSPECT EB-050 REPLACING TRAILING " " BY "*"
           ELSE
              MOVE LP-APOTH TO L-050 EB-050.

           ADD LP-APINT LP-APLC GIVING L-060 EB-060.
           MOVE LP-APCUR TO L-070 EB-070.

      * 7/30/98 BAH ALWAYS DISPLAY REVERSAL BALANCES, #487
      * 8/11/98 SUPPRESS BALANCE IF ENV 'RVLINES' IS SET (MJD AS PER JTG)
      
           IF (LP-TRCD = "RV" OR LP-REV NOT = " ") AND
               NOT (LP-TRCD = "RV" AND LP-REVSEQNO = 0) AND
                   DISP-REV-BAL = "N"
              MOVE "*" TO L-120 EB-120
           ELSE
              MOVE LP-PDTH-DATE TO NUM-DATE
              MOVE NUM-MO TO EB-080-MO
              IF LN-UNITPER-CD-DAYS
                 MOVE NUM-DA TO EB-080-DAYR
              ELSE
                 MOVE NUM-YR TO EB-080-DAYR
              END-IF
              ADD LP-OTHBAL LP-OT2BAL GIVING EB-090
              ADD LP-INTBAL LP-LCBAL GIVING EB-100
              MOVE LP-CURBAL TO EB-110
              MOVE EB-080 TO L-080
              ADD LP-OTHBAL LP-OT2BAL GIVING L-090
              ADD LP-INTBAL LP-LCBAL GIVING L-100
              MOVE LP-CURBAL TO L-110
              IF (LP-TRCD = "RV" OR LP-REV NOT = " ") AND
                  NOT (LP-TRCD = "RV" AND LP-REVSEQNO = 0)
                 MOVE "*" TO L-120 EB-120.

       END-SETUP-LINE.
           EXIT.

      ********************************************************************
      *                 SET-INSTAX-RB-WORKAMT2
      *
      *   THIS ROUTINE DETERMINES THE AMOUNT OF THE INS. TAX REBATE
      *   FOR A INSURANCE MANUAL OVERRIDE
      *
      *   NOTE:
      *         RB-WITH-INSTAX = RB-LESS-INSTAX + INS-TAX-RB
      *
      *                                           RB-LESS-INSTAX
      *         RB-WITH-INSTAX = RB-LESS-INSTAX + -------------- * FEEAMT
      *                                               PREMIUM
      *
      *                                                  FEEAMT
      *         RB-WITH-INSTAX = RB-LESS-INSTAX * ( 1 + --------- )
      *                                                  PREMIUM
      *
      *                             RB-WITH-INSTAX
      *         RB-LESS-INSTAX = --------------------
      *                                   FEEAMT
      *                           ( 1 + ---------- )
      *                                  PREMIUM
      *
      *     SO:
      *         LP-PRORATED-INS-REB    = RB-WITH-INSTAX - RB-LESS-INSTAX
      ********************************************************************
       SET-INSTAX-RB-WORKAMT2 SECTION.
      ********************************************************************
           COMPUTE LP-PRORATED-INS-REB ROUNDED =
               RB-WITH-INSTAX / ( 1 + RB-FEEAMT / RB-PREMIUM).
           COMPUTE LP-PRORATED-INS-REB = RB-WITH-INSTAX
                                       - LP-PRORATED-INS-REB.
   
       SET-INSTAX-RB-WORKAMT2-EXIT.
           EXIT.

      *************************************
       SAVE-SCREEN SECTION.
      *************************************
           MOVE 3 TO WK-KEY.
           PERFORM OPEN-WK-FILE.
           PERFORM READ-WK-FILE.
           IF IO-FG = 9
              GO TO IO-ERROR.
           IF VDUSTATUS = "16"
              IF WK-OLD-PROG-FORM = " "
                 MOVE 99 TO MENU-FG
                 GO TO EXIT-SAVE-SCREEN
              ELSE
                 MOVE WK-OLD-PROG-FORM TO NEW-PROG-FORM OLD-PROG-FORM
                 MOVE " " TO WK-OLD-PROG-FORM.

           MOVE ELE     TO WK-ELE.
           MOVE LP-REC  TO WK-LP-REC.
           MOVE LX-REC  TO WK-LX-REC.
           MOVE LXE-REC TO WK-LXE-REC.
           MOVE LXG-REC TO WK-LXG-REC.

           PERFORM REWRITE-WK-FILE.
       EXIT-SAVE-SCREEN.
           PERFORM CLOSE-WK-FILE.

      *************************************
       RESTORE-SCREEN SECTION.
      *************************************
           MOVE 3 TO WK-KEY.
           PERFORM OPEN-WK-FILE.
           PERFORM READ-WK-FILE.
           IF NEW-FORM NOT = OLD-FORM
              MOVE OLD-PROG-FORM TO WK-OLD-PROG-FORM
              MOVE NEW-PROG-FORM TO OLD-PROG-FORM.
           IF IO-FG = 0
              GO TO RESTORE-CLEAR.

           MOVE SPACES TO WK-SAVE-RECORD.
           MOVE 0      TO WK-ELE.
           PERFORM WRITE-WK-FILE.
           IF IO-FG = 9
              GO TO IO-ERROR.

           GO TO EXIT-RESTORE-SCREEN.
       RESTORE-CLEAR.
           IF WK-ELE NOT = 0
              SUBTRACT 1 FROM WK-ELE GIVING FIRST-ELE.

           MOVE WK-SAVE-RECORD TO LN-REC.
           MOVE WK-LP-REC      TO LP-REC.
           MOVE WK-LX-REC      TO LX-REC.
           MOVE WK-LXE-REC     TO LXE-REC.
           MOVE WK-LXG-REC     TO LXG-REC.
           MOVE 0              TO WK-ELE.
           PERFORM REWRITE-WK-FILE.
       EXIT-RESTORE-SCREEN.
           PERFORM CLOSE-WK-FILE.

      *******************************************************
       VERIFY-PASSWORD SECTION.
      *******************************************************
           MOVE " " TO PASSWDW-STAT.
           MOVE "LONPFB" TO PASSWDW-PGM.
           MOVE "PLEASE ENTER THE PASSWORD," TO PASSWDW-PROMPT1.
           MOVE "CL/PASSWD" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH PASSWDW-BUF.
           CANCEL FORM-PROGX.

      ***************************************************************
       SETUP-LINK-INFO SECTION.
      ***************************************************************
           IF VDU-FORM-NAME = "LPSMNU"
              MOVE LPSMNU-HELP--FILE TO HELP-LINK-FILE
              MOVE LPSMNU-HELP--DIR  TO HELP-LINK-DIR
           ELSE
           IF VDU-FORM-NAME = "DLPARV"
              MOVE DLPARV-HELP--FILE TO HELP-LINK-FILE
              MOVE DLPARV-HELP--DIR  TO HELP-LINK-DIR
           ELSE
              MOVE LONPF-HELP--FILE TO HELP-LINK-FILE
              MOVE LONPF-HELP--DIR TO HELP-LINK-DIR.

      *******************************************
       VDU-CONVERT-FIELD SECTION.
      *******************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBLP/LONPF_EVA.CPY".
              COPY "LIBLP/LPSMNU_EVA.CPY".
              COPY "LIBLP/LPLNMU_EVA.CPY".
              COPY "LIBLP/LPCAMU_EVA.CPY".
              COPY "LIBLP/LPVDMU_EVA.CPY".
              COPY "LIBWI/DLPARV_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.


      *************************************
       MISCELLANEOUS-ROUTINES SECTION.
      *************************************
       COPY "LIBGB/SYSTEM.CPY".
       COPY "LIBGB/GETENV.CPY".
       FORM-RESET.
       CONDITIONAL-SEND-MESS.
           IF EXT-ROUTE-BUF = "00"
              PERFORM SEND-MESS.
       INSURANCE-NOT-OURS.
           MOVE "INSURANCE IS NOT YOURS, CAN NOT CHANGE" TO MESS.
           PERFORM CONDITIONAL-SEND-MESS.
       SET-BAL-IBPC-LPREC.
           MOVE "Y" TO LP-MANUAL-RB.
           MOVE LN-OTHBAL TO LP-OTHBAL.
           MOVE LN-OT2BAL TO LP-OT2BAL.
           MOVE LN-INTBAL TO LP-INTBAL.
           MOVE LN-LCBAL TO LP-LCBAL.
           MOVE LN-CURBAL TO LP-CURBAL.

           MOVE LP-PAYDATE TO IBPC-DATE.
           PERFORM IBPC-TEST.
           MOVE IBPC-FG TO LP-IBPC.

           MOVE 0 TO HOLD-DLRVCHGS-ADJ(1)
                     HOLD-DLRVCHGS-ADJ(2)
                     HOLD-DLRVCHGS-ADJ(3)
                     HOLD-DLPARVHELD-ADJ
                     HOLD-DLPARVPAID-ADJ.

       SCREEN-LINES-RESET.
           MOVE "Y" TO FULL-PAGE-FG.
           MOVE LN-OWNBR  TO LP-BRNO.
           MOVE LN-ACCTNO TO LP-ACCTNO.
           MOVE 99999     TO LP-SEQNO.
           MOVE LP1-KEY TO HOLD2-KEY-UP.
           MOVE "7" TO ROUTE-CD.

       TEST-STAT.
           IF STAT NOT = "00"
              PERFORM FORM-IT
              GO TO IO-ERROR.
       CLEAR-LEGEND.
           MOVE SPACES TO MESS.
           PERFORM SEND-LEGEND.
       SEND-INT-OR-LC.
           IF LP-IBPC = "I"
              MOVE "   INT" TO VDUBUF
           ELSE
              MOVE "LT-CHG" TO VDUBUF.
           PERFORM SCREENIO.
      *CLEAR-REBATE-TABLE.
      *    MOVE LN-INSOURS(1) TO U-INSOURS(1).
      *    MOVE LN-INSOURS(2) TO U-INSOURS(2).
      *    MOVE LN-INSOURS(3) TO U-INSOURS(3).
      *    MOVE LN-INSOURS(4) TO U-INSOURS(4).
      *    MOVE LN-INSOURS(5) TO U-INSOURS(5).
      *    MOVE LN-INSOURS(6) TO U-INSOURS(6).
      *    MOVE LN-INSOURS(7) TO U-INSOURS(7).
      *    MOVE LN-INSOURS(8) TO U-INSOURS(8).
       DISPLAY-ALL-LINES.
           MOVE LINE-BUF TO WR-BUF.
           MOVE LINE-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.
       SEND-LEGEND.
           MOVE "S  A000MSEL." TO VDU.
           MOVE MESS TO VDUBUF.
           PERFORM SCREENIO.
       SEND-WSEL.
           MOVE "S  A000WSEL." TO VDU.
           MOVE MESS TO VDUBUF.
           PERFORM SCREENIO.
       USE-PAYOFF.
              MOVE "ACCOUNT WILL PAY OFF, USE A PAYOFF CODE:"
                                                       TO MESS.
              PERFORM SEND-LEGEND.
       EXCEEDS-PRIN-BAL.
           MOVE "EXCEEDS PRINCIPAL BAL" TO MESS.
           PERFORM CONDITIONAL-SEND-MESS.
       EXCEEDS-PUR-BAL.
           MOVE LN-PURBAL TO PURBAL-EXCEEDED.
           MOVE PUR-BAL-EXCEEDED TO MESS.
           PERFORM CONDITIONAL-SEND-MESS.
       SEND-ENTRY-LINE.
           MOVE ENTRY-BUF TO WR-BUF.
           MOVE ENTRY-LINE TO WR-LIST.
           PERFORM CALL-WRFORM.

      ****************************************************
       REBATE-INTEREST-ADJ SECTION.
      ****************************************************
           IF REBATE-OVERRIDE-FG = "Y"
              COMPUTE ADDON-INT-REBATE ROUNDED =
                 LP-TRAMT * CALC-ADDON-INT-REBATE
                      / (CALC-REBATE
                           + CALC-DLRVCHGS-ADJ(1)
                            + CALC-DLRVCHGS-ADJ(2)
                             + CALC-DLRVCHGS-ADJ(3)
                              + CALC-DLPARVHELD-ADJ
                               + CALC-DLPARVPAID-ADJ
                                + CALC-ADDON-INT-REBATE)
              IF ADDON-INT-REBATE > LN-ANTICERN(2)
                 MOVE LN-ANTICERN(2) TO ADDON-INT-REBATE.
           IF REBATE-OVERRIDE-FG = "Y"
              SUBTRACT ADDON-INT-REBATE FROM LP-TRAMT
              GO TO REBATE-INTEREST-ADJ-CONT.

           MOVE 7 TO REB-SUB.
           MOVE LN-INTCHG TO REB-TOTCHG.
           MOVE LN-ORGTERM TO REB-ORGTERM.
           MOVE LP-PAYDATE TO REB-PAYDATE.
           IF HOLD-BP-TRCD = "F9"
              MOVE "PO" TO REB-LPTRCD
           ELSE
              MOVE "RI" TO REB-LPTRCD.
           PERFORM REBATE.
           IF REB-SUB = 0
              MOVE 0 TO LP-TRAMT
           ELSE
              MOVE REB-REBATE TO LP-TRAMT
              IF SP-RBREDUC(7) = 9999.01
                 MOVE REB-MS-PENALTY TO LP-LCPAID MS-PENALTY.

      * PRORATION OF EARNINGS FOR INTEREST, SERVICE CHG AND MAINT FEES:
       REBATE-INTEREST-ADJ-CONT.

           MOVE 0 TO LP-DLRVCHGS-ADJ(1)
                     LP-DLRVCHGS-ADJ(2)
                     LP-DLRVCHGS-ADJ(3)
                     LP-DLPARVHELD-ADJ
                     LP-DLPARVPAID-ADJ.

      * ONLY ALLOWED FOR PC ACCOUNTS:
           IF LN-SERVACCT = "Y"
                    OR LN-PLCD NOT = " "
                     OR LN-INTCHG = 0
              GO TO REBATE-INTEREST-ADJ-ADDON.

      * COMPUTE RESERVE FROM CHARGES ADJUSTMENT:
           MOVE 0 TO ADJSUB.
           PERFORM COMPUTE-RVCHGS-ADJUSTMENT 3 TIMES.

      * COMPUTE DEALER PARTICIPATION RESERVE HELD ADJUSTMENT:
           IF HOLD-DL-PARVHELD-CODE = "A" OR "B"
              IF SP-PARVHELD-RB-CD = "Z"
                 MOVE 0 TO LP-DLPARVHELD-ADJ
              ELSE
              IF SP-PARVHELD-RB-CD = " "
                 COMPUTE LP-DLPARVHELD-ADJ ROUNDED =
                     LP-TRAMT / LN-INTCHG * LN-DLPARVHELD
              ELSE
              IF SP-PARVHELD-RB-CD = "A"
                 PERFORM COMPUTE-RECAPTURE-PRORATA
                 COMPUTE LP-DLPARVHELD-ADJ ROUNDED =
                           LN-DLPARVHELD * DLRECAP-PERCENT.

      * COMPUTE DEALER PARTICIPATION RESERVE PAID ADJUSTMENT:
           IF HOLD-DL-PARVPAID-CODE = " " OR "A"
              IF SP-PARVPAID-RB-CD = "Z"
                 MOVE 0 TO LP-DLPARVPAID-ADJ
              ELSE
              IF SP-PARVPAID-RB-CD = " "
                 COMPUTE LP-DLPARVPAID-ADJ ROUNDED =
                     LP-TRAMT / LN-INTCHG * LN-DLPARVPAID
              ELSE
              IF SP-PARVPAID-RB-CD = "A"
                 PERFORM COMPUTE-RECAPTURE-PRORATA
                 COMPUTE LP-DLPARVPAID-ADJ ROUNDED =
                           LN-DLPARVPAID * DLRECAP-PERCENT.

           IF REBATE-OVERRIDE-FG = " "
      * DETERMINE IF DEALER RECAPTURE CAUSES A NEGATIVE
      * EARNINGS RECAPTURE FOR THE FINANCE COMPANY:
              COMPUTE DLRECAP-WORK =
                   LP-DLRVCHGS-ADJ(1) + LP-DLRVCHGS-ADJ(2)
                    + LP-DLRVCHGS-ADJ(3) + LP-DLPARVHELD-ADJ
                     + LP-DLPARVPAID-ADJ - LP-TRAMT
              PERFORM DLRECAP-REDISBURSE

              COMPUTE CALC-REBATE =
                          LP-TRAMT - LP-DLRVCHGS-ADJ(1)
                                    - LP-DLRVCHGS-ADJ(2)
                                     - LP-DLRVCHGS-ADJ(3)
                                      - LP-DLPARVHELD-ADJ
                                       - LP-DLPARVPAID-ADJ
              MOVE LP-DLRVCHGS-ADJ(1) TO CALC-DLRVCHGS-ADJ(1)
              MOVE LP-DLRVCHGS-ADJ(2) TO CALC-DLRVCHGS-ADJ(2)
              MOVE LP-DLRVCHGS-ADJ(3) TO CALC-DLRVCHGS-ADJ(3)
              MOVE LP-DLPARVHELD-ADJ TO CALC-DLPARVHELD-ADJ
              MOVE LP-DLPARVPAID-ADJ TO CALC-DLPARVPAID-ADJ.

       REBATE-INTEREST-ADJ-ADDON.
      * IF ADDON INSURANCE INCREASE REBATE AMOUNT:
           IF LN-ADDON-FG = "Y"
              PERFORM REBATE-ADDONS
              ADD ADDON-INT-REBATE TO LP-TRAMT
              IF REBATE-OVERRIDE-FG = " "
                 MOVE ADDON-INT-REBATE TO CALC-ADDON-INT-REBATE.

           IF (LN-SERVACCT NOT = "Y") AND
              (LN-PLCD = " "        ) AND
              (LN-INTCHG NOT = 0    )
              IF (LN-LOANTYPE = "P" AND
                        ( (LN-DLPARVHELD NOT = 0)
                             OR (LN-DLPARVPAID NOT = 0)
                              OR (LN-DLRVCHGS(1) NOT = 0)
                               OR (LN-DLRVCHGS(2) NOT = 0)
                                OR (LN-DLRVCHGS(3) NOT = 0)
                        )
                 ) OR (LN-ANTICERN(2) NOT = 0)
                 PERFORM RB-OVERRIDE.

      * SET EARNING INTO LP-REC:
           SUBTRACT ADDON-INT-REBATE FROM LP-TRAMT
                                      GIVING EADJ-REBATE.
           IF HOLD-DL-PARVHELD-CODE = "B"
              SUBTRACT LP-DLPARVHELD-ADJ FROM EADJ-REBATE.
           IF HOLD-DL-PARVPAID-CODE = " "
              SUBTRACT LP-DLPARVPAID-ADJ FROM EADJ-REBATE.
           IF HOLD-DL-RVCHGS-CODE(1) = " "
              SUBTRACT LP-DLRVCHGS-ADJ(1) FROM EADJ-REBATE.
           IF HOLD-DL-RVCHGS-CODE(2) = " "
              SUBTRACT LP-DLRVCHGS-ADJ(2) FROM EADJ-REBATE.
           IF HOLD-DL-RVCHGS-CODE(3) = " "
              SUBTRACT LP-DLRVCHGS-ADJ(3) FROM EADJ-REBATE.
           PERFORM EADJ-EARN-ADJUSTMENT.

           IF LN-SERVACCT NOT = "Y" AND LN-PLCD = " "
              MOVE 1 TO SUBX
              PERFORM REBATE-INTEREST-ADJUST 4 TIMES.

      *************************************************
       COMPUTE-RVCHGS-ADJUSTMENT SECTION.
      *************************************************

           ADD 1 TO ADJSUB.
           IF HOLD-DL-RVCHGS-CODE(ADJSUB) = " "
              IF SP-DLRVCHGS-RB-CD(ADJSUB) = "Z"
                 MOVE 0 TO LP-DLRVCHGS-ADJ(ADJSUB)
              ELSE
              IF SP-DLRVCHGS-RB-CD(ADJSUB) = " "
                 COMPUTE LP-DLRVCHGS-ADJ(ADJSUB) ROUNDED =
                     LP-TRAMT / LN-INTCHG * LN-DLRVCHGS(ADJSUB)
              ELSE
              IF SP-DLRVCHGS-RB-CD(ADJSUB) = "A"
                 PERFORM COMPUTE-RECAPTURE-PRORATA
                 COMPUTE LP-DLRVCHGS-ADJ(ADJSUB) ROUNDED =
                           LN-DLRVCHGS(ADJSUB) * DLRECAP-PERCENT.

      ********************************
       REBATE-INTEREST-ADJUST SECTION.
           IF SUBX = 2
              MOVE ADDON-INT-EARNED TO EARNINGS-WORKER
           ELSE
              IF EADJ-ADJUST-FG(SUBX) NOT = "Y"
                 GO TO REBATE-INTEREST-ADJUST-EXIT
              ELSE
                 COMPUTE EARNINGS-WORKER =
                    LN-ANTICERN(SUBX) - LN-ACCERN(SUBX)
                                         - EADJ-ADJUST(SUBX).

           COMPUTE HOLD-LN-ACCERN(SUBX) = LN-ACCERN(SUBX)
                                         + EARNINGS-WORKER.
           ADD EARNINGS-WORKER TO HOLD-LN-TOTERN.
           MOVE LN-EARN-ORDER(SUBX) TO SUB.
           IF SUB > 3
      *       MOVE EARNINGS-WORKER TO WK-LXE-EARN(SUB - 3)
              MOVE EARNINGS-WORKER TO LXE-EARN(SUB - 3)
           ELSE
              MOVE EARNINGS-WORKER TO LP-EARNED(SUB).

       REBATE-INTEREST-ADJUST-EXIT.
           ADD 1 TO SUBX.

      **********************************************
       REBATE-SERCHG-ADJ SECTION.
      **********************************************

      * REBATE SERVICE CHARGE ADJUSTMENT ROUTINE
      * REB-EP54-AMT WILL HOLD AMOUNT OF GA EARLY PROVISION
      * IF APPLICABLE. (THIS IS ALSO INCLUDED IN REB-REBATE)

           IF REBATE-OVERRIDE-FG = "Y"
              GO TO REBATE-SERCHG-ADJ-CONT.

           MOVE 8 TO REB-SUB.
           MOVE LN-SERCHG TO REB-TOTCHG.
           MOVE LN-ORGTERM TO REB-ORGTERM.
           MOVE LP-PAYDATE TO REB-PAYDATE.
           PERFORM REBATE.
           IF REB-SUB = 0
              MOVE 0 TO LP-TRAMT
              MOVE 0 TO HOLD-LXR-REB-AMT
           ELSE
              MOVE REB-REBATE TO LP-TRAMT
              MOVE REB-EP54-AMT TO HOLD-LXR-REB-AMT.
     *     IF LN-SERVACCT = "Y" OR LN-PLCD NOT = " "
     *        GO TO EXIT-REBATE-SERCHG-ADJ.

       REBATE-SERCHG-ADJ-CONT.
           IF LN-SERVACCT = "Y" OR LN-PLCD NOT = " "
              GO TO EXIT-REBATE-SERCHG-ADJ.

           COMPUTE EARNINGS-WORKER =
                    LN-ANTICERN(3) - LN-ACCERN(3) - LP-TRAMT.
           COMPUTE HOLD-LN-ACCERN(3) = LN-ACCERN(3) + EARNINGS-WORKER.
           ADD EARNINGS-WORKER TO HOLD-LN-TOTERN.
           MOVE LN-EARN-ORDER(3) TO SUB.
           IF SUB > 3
              MOVE EARNINGS-WORKER TO LXE-EARN(SUB - 3)
           ELSE
              MOVE EARNINGS-WORKER TO LP-EARNED(SUB).

       EXIT-REBATE-SERCHG-ADJ.
           EXIT.

      ***********************************************
       REBATE-MAINTFEE-ADJ SECTION.
      ***********************************************
           IF REBATE-OVERRIDE-FG = "Y"
              GO TO REBATE-MAINTFEE-ADJ-CONT.

      * THE SETUP OF REBATES ARE FOR MEXICO SP-RBFRMLA2 0G
           IF SP-RBFRMLA2(9) = "G"
              IF LN-INT-REBATE = " "
                 MOVE 7          TO REB-SUB
                 MOVE LN-INTCHG  TO REB-TOTCHG
                 MOVE LN-ORGTERM TO REB-ORGTERM
                 MOVE LP-PAYDATE TO REB-PAYDATE
                 IF HOLD-BP-TRCD = "F9"
                    MOVE "PO" TO REB-LPTRCD
                 ELSE
                    MOVE "RI" TO REB-LPTRCD
                 END-IF
                 PERFORM REBATE
                 IF REB-SUB = 0
                    MOVE 0           TO REB-LN-INT-REBATE
                 ELSE
                    MOVE REB-REBATE  TO REB-LN-INT-REBATE
                 END-IF
              ELSE
                 MOVE LN-ANTICADJ(1) TO REB-LN-INT-REBATE
              END-IF
              IF LN-SC-REBATE = " "
                 MOVE 8          TO REB-SUB
                 MOVE LN-SERCHG  TO REB-TOTCHG
                 MOVE LN-ORGTERM TO REB-ORGTERM
                 MOVE LP-PAYDATE TO REB-PAYDATE
                 PERFORM REBATE
                 IF REB-SUB = 0
                    MOVE 0           TO REB-LN-SC-REBATE
                 ELSE
                    MOVE REB-REBATE  TO REB-LN-SC-REBATE
                 END-IF
              ELSE
                 MOVE LN-ANTICADJ(3) TO REB-LN-SC-REBATE
              END-IF
              MOVE LN-ANTICERN(4) TO  REB-TOTCHG
           ELSE
              COMPUTE REB-TOTCHG = LN-MAINTFEE * LN-ORGTERM.

           MOVE 9 TO REB-SUB.
      *--->COMPUTE REB-TOTCHG = LN-MAINTFEE * LN-ORGTERM.
           MOVE LN-ORGTERM TO REB-ORGTERM.
           MOVE LP-PAYDATE TO REB-PAYDATE.
           PERFORM REBATE.
           IF REB-SUB = 0
              MOVE 0 TO LP-TRAMT
           ELSE
              MOVE REB-REBATE TO LP-TRAMT.
      *    IF LN-SERVACCT = "Y" OR LN-PLCD NOT = " "
      *       GO TO EXIT-REBATE-MAINTFEE-ADJ.

       REBATE-MAINTFEE-ADJ-CONT.

           IF LN-SERVACCT = "Y" OR LN-PLCD NOT = " "
              GO TO EXIT-REBATE-MAINTFEE-ADJ.

           COMPUTE EARNINGS-WORKER =
                   LN-ANTICERN(4) - LN-ACCERN(4) - LP-TRAMT.
           COMPUTE HOLD-LN-ACCERN(4) = LN-ACCERN(4) + EARNINGS-WORKER.
           ADD EARNINGS-WORKER TO HOLD-LN-TOTERN.
           MOVE LN-EARN-ORDER(4) TO SUB.
           IF SUB > 3
              MOVE EARNINGS-WORKER TO LXE-EARN(SUB - 3)
           ELSE
              MOVE EARNINGS-WORKER TO LP-EARNED(SUB).

       EXIT-REBATE-MAINTFEE-ADJ.
           EXIT.

      **************************************************
       REBATE-DEFERMENT SECTION.
      **************************************************

           IF LP-TRCD = "CD"
              MOVE LN-TOTDEF TO LP-TRAMT
           ELSE
              MOVE 10 TO REB-SUB
              MOVE LN-TOTDEF TO REB-TOTCHG
              MOVE LN-ORGTERM TO REB-ORGTERM
              MOVE LP-PAYDATE TO REB-PAYDATE
              PERFORM REBATE
              IF REB-SUB = 0
                 MOVE 0 TO LP-TRAMT
              ELSE
                 MOVE REB-REBATE TO LP-TRAMT.

      *********************************************
       REBATE-LN-CL SECTION.
      *********************************************

      * TEST FOR SINGLE OR JOINT LIFE:
           IF LN-INSURED = "J"
              MOVE 2 TO REB-SUB
           ELSE
              MOVE 1 TO REB-SUB.
      * TEST FOR LEVEL LIFE:
           IF LN-INSTYPES-LL
              ADD 2 TO REB-SUB.

           IF LN-CL-REBATE NOT = "Y"
              MOVE 1 TO SUBX
              PERFORM REBATE-GET-INS-AMT.

      **********************
       REBATE-LN-AH SECTION.
           IF LN-AH-REBATE NOT = "Y"
              MOVE 5 TO REB-SUB
              MOVE 2 TO SUBX
              PERFORM REBATE-GET-INS-AMT.

      **********************
       REBATE-LN-PP SECTION.
           IF LN-PP-REBATE NOT = "Y"
              MOVE 6 TO REB-SUB
              MOVE 3 TO SUBX
              PERFORM REBATE-GET-INS-AMT.

      **********************
       REBATE-LN-O1 SECTION.
           IF LN-O1-REBATE NOT = "Y"
              MOVE 12 TO REB-SUB
              MOVE 4 TO SUBX
              PERFORM REBATE-GET-INS-AMT.

      **********************
       REBATE-LN-O2 SECTION.
           IF LN-O2-REBATE NOT = "Y"
              MOVE 13 TO REB-SUB
              MOVE 5 TO SUBX
              PERFORM REBATE-GET-INS-AMT.

      **********************
       REBATE-LN-O3 SECTION.
           IF LN-O3-REBATE NOT = "Y"
              MOVE 14 TO REB-SUB
              MOVE 6 TO SUBX
              PERFORM REBATE-GET-INS-AMT.

      *********************************************************
       REBATE-GET-INS-AMT SECTION.
      *********************************************************
           IF LN-INSPREM(SUBX) NOT = 0
              IF LN-INSOURS(SUBX) NOT = "M"
                 MOVE SUBX             TO ITRM-SUB
                 MOVE LP-PAYDATE       TO REB-PAYDATE ITRM-TODAY
                 PERFORM INS-TERM-CALCULATION
                 MOVE ITRM-INSTERM     TO REB-ORGTERM
                 MOVE LN-INSPREM(SUBX) TO REB-TOTCHG
      *  JTG 010503 CORRECTED BUG IN REBATE-GET-INS-AMT, ROUTINE WAS NOT TESTING
      *             FOR LN-ADDON-FG CORRECTLY WHEN 'R1' OR 'R2' OR 'R3'
      *             NEW CODE 'B' FOR SP-RBUNITPER-CD(OCCURS 16) PAACO PR#1466
      *          IF ( LP-TRCD = "R1" OR "R2" OR "R3")
      *                  OR ( LP-TRCD = "RC" OR "RA" OR "RP" )
      *                              AND ( LN-ADDON-FG = "Y" )
                 IF LN-ADDON-FG = "Y"
                    MOVE LN-INSEFF-DATE(SUBX)  TO REB-LN-LOANDATE
                                             REB-LN-PURDATE
                                             REB-LN-INTDATE
                                             REB-LN-INSEFF
                                             NDTE-DATE
                    MOVE LN-INSEXP-DATE(SUBX)  TO REB-LN-INSEXP
                    MOVE 1 TO NDTE-HOLD
                    MOVE LN-UNITPER-CD   TO DATER-UNITPER-CD
                    MOVE LN-UNITPER-FREQ TO DATER-UNITPER-FREQ
                    PERFORM INCREMENT-UNITPER
                    MOVE NDTE-DATE        TO REB-LN-ORIG-1STPYDATE
                    MOVE REB-ORGTERM      TO REB-LN-ORGTERM
                    MOVE LN-REGPYAMT      TO REB-LN-REGPYAMT
                    MOVE LN-LASTPYAMT     TO REB-LN-LASTPYAMT
                    MOVE LN-APRATE        TO REB-LN-APRATE
                    MOVE LN-SMPRATE       TO REB-LN-SMPRATE
                    MOVE 0                TO REB-LN-TOTNODEF
                    MOVE LN-INSCOVR(SUBX) TO REB-LN-INSCOVR
                    MOVE "N"              TO REB-LN-SALEFIN
                    MOVE "Y"              TO REB-ADDON-FG
                 END-IF
                 MOVE SUBX TO REB-SUB2 ADDONSPR-SUB
                 PERFORM FIND-ADDON-SPR
                 PERFORM REBATE
                 PERFORM FIND-ADDON-SPR-RESTORE
                 MOVE REB-REBATE TO LP-TRAMT
                 MOVE INS-TAX-RB TO LP-PRORATED-INS-REB.

      *********************************************************
       REBATE-ADDONS SECTION.
      *********************************************************
           MOVE 0 TO ADDON-INT-EARNED.
           IF REBATE-OVERRIDE-FG = " "
              MOVE 0 TO ADDON-INT-REBATE
           ELSE
              MOVE ADDON-INT-REBATE TO ADDON-INT-WORKER.

           PERFORM OPEN-LTI1-FILE.

           MOVE LTI-PATH-OWNBR  TO LTI-BRNO
                                   QLTI1-WBEG-BRNO
                                   QLTI1-WEND-BRNO.
           MOVE LN-ACCTNO       TO LTI-ACCTNO
                                   QLTI1-WBEG-ACCTNO
                                   QLTI1-WEND-ACCTNO.
           MOVE 0               TO LTI-SEQNO
                                   QLTI1-WBEG-SEQNO.
           MOVE ALL "9"         TO QLTI1-WEND-SEQNO.

           PERFORM START-LTI1-FILE.
       REBATE-ADDONS-NEXT.
           PERFORM READ-LTI1-FILE-NEXT.
           IF IO-FG NOT = 0
              GO TO END-REBATE-ADDONS.

           IF (LTI-BRNO NOT = LTI-PATH-OWNBR) OR
               (LTI-ACCTNO NOT = LN-ACCTNO)
      *       UNLOCK LTI-FILE
              GO TO END-REBATE-ADDONS.

      * COMMON LOGIC TO COMPUTE ADDON INTEREST REFUND:
           IF REBATE-OVERRIDE-FG = " "
              MOVE REB-PAYDATE TO POFF-PAYDATE
              MOVE REB-LPTRCD TO POFF-LPTRCD
              PERFORM ADDONRB-GET-INT-REBATE
              IF REB-SUB NOT = 0
                 MULTIPLY POFF-ONE BY REB-REBATE
                 ADD REB-REBATE TO ADDON-INT-REBATE
              ELSE
                 MOVE 0 TO REB-REBATE
              END-IF
           ELSE
              IF ( ADDON-INT-WORKER NOT = 0
                        AND ADDON-INT-WORKER > LTI-ANTICERN(1) )
                 SUBTRACT LTI-ANTICERN(1) FROM ADDON-INT-WORKER
                 MOVE LTI-ANTICERN(1) TO REB-REBATE
              ELSE
                 MOVE ADDON-INT-WORKER TO REB-REBATE
                 MOVE 0 TO ADDON-INT-WORKER.

       REBATE-ADDON-OVERRIDE-REBATE.
           MOVE REB-REBATE TO LTI-RB-WORKER.
           IF LN-SERVACCT NOT = "Y"  AND LN-PLCD = " "
              COMPUTE ADDON-INT-EARNED = ADDON-INT-EARNED
                 + LTI-ANTICERN(1) - LTI-ANTICADJ(1) - LTI-ACCERN(1)
                                                      - REB-REBATE.

           PERFORM REWRITE-LTI1-FILE.
           GO TO REBATE-ADDONS-NEXT.

       END-REBATE-ADDONS.
           PERFORM CLOSE-LTI1-FILE.

      ***********************************
       DISPLAY-EARNINGS SECTION.
      ***********************************
           IF HOLD-DISPLAY-ERN = "A"
              ADD HOLD-LN-ACCERN(1) HOLD-LN-ACCERN(2)
                  HOLD-LN-ACCERN(3)
                  HOLD-LN-ACCERN(5) GIVING TOTEARN
              IF NOT (GP-INCL-OTH-IN-EARNED-N OR
                                GP-INCL-OTH-IN-EARNED-R)
                 ADD HOLD-LN-ACCERN(6) TO TOTEARN
              END-IF
              IF NOT(GP-INCL-OTH-IN-EARNED-R)
                 ADD HOLD-LN-ACCERN(4) TO TOTEARN
              END-IF
              MOVE TOTEARN TO D-TOTEARN
           ELSE
           IF HOLD-DISPLAY-ERN = "U" AND
              LN-SERVACCT NOT = "Y"  AND
              LN-PLCD = " "
              COMPUTE TOTEARN = LN-ANTICERN(1) - HOLD-LN-ACCERN(1)
                              + LN-ANTICERN(2) - HOLD-LN-ACCERN(2)
                              + LN-ANTICERN(3) - HOLD-LN-ACCERN(3)
                              + LN-ANTICERN(5) - HOLD-LN-ACCERN(5)
                              - LP-TRAMT
              IF NOT(GP-INCL-OTH-IN-EARNED-N OR
                                             GP-INCL-OTH-IN-EARNED-R)
                 COMPUTE TOTEARN = TOTEARN
                                + LN-ANTICERN(6) - HOLD-LN-ACCERN(6)
              END-IF
              IF NOT(GP-INCL-OTH-IN-EARNED-R)
                 COMPUTE TOTEARN = TOTEARN
                                + LN-ANTICERN(4) - HOLD-LN-ACCERN(4)
              END-IF
              MOVE TOTEARN TO D-TOTEARN
           ELSE
           IF HOLD-DISPLAY-ERN = "T"
              MOVE HOLD-LN-TOTERN TO D-TOTEARN
           ELSE
      * "I" IS TODAY DUE INTEREST BUT CALCULATING IT IN THE POSTING PROGRAMS
      *     WAS TOO MESSY SO LEAVE IT BLANK, NOT ZERO, BUT BLANK, LONPF1 WILL
      *     REDISPLAY IT
           IF HOLD-DISPLAY-ERN = "I"
              NEXT SENTENCE
           ELSE
              MOVE 0 TO D-TOTEARN.

           COMPUTE D-EARNED ROUNDED = LP-EARNED(1) + LP-EARNED(2)
                                    + LP-EARNED(3) + LXE-EARN(1)
                                    + LXE-EARN(2)  + LXE-EARN(3).
      *                             + LP-EARNED(3) + WK-LXE-EARN(1)
      *                             + WK-LXE-EARN(2) + WK-LXE-EARN(3).
           MOVE LN-YTDINTPD TO D-INTYTD.
           MOVE LN-LCPDTH-DATE TO NUM-DATE.
           MOVE NUM-MO TO D-LCPDTH-MO.
           IF LN-UNITPER-CD-DAYS
              MOVE NUM-DA TO D-LCPDTH-DAYR
           ELSE
              MOVE NUM-YR TO D-LCPDTH-DAYR.
           MOVE LP-DLPROC TO D-DLPROC.
           MOVE EARN-BUF TO WR-BUF.
           MOVE EARN-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      ********************************************************************
       RB-OVERRIDE SECTION.
      ********************************************************************

      * RE-CAPTURE OVERRIDE
      *   THIS WORKS ONLY IF ALL RESERVES FROM CHARGES
      *   AND PARTICIPATINGS ARE DEALER CODED TO OFFSET RESERVE

           IF HOLD-BP-TRCD = " "
              MOVE "SNNS052040." TO VDU
              MOVE LP-TRAMT TO VDUNUM2
              PERFORM SCREENIO.

           IF BR-DLPARV-ADJ-FG = "N"
              GO TO EXIT-RB-OVERRIDE
           ELSE
              IF LN-ANTICERN(2) = 0
                      AND LN-DLRVCHGS(1) = 0
                       AND LN-DLRVCHGS(2) = 0
                        AND LN-DLRVCHGS(3) = 0
                         AND LN-DLPARVHELD = 0
                          AND LN-DLPARVPAID = 0
                 GO TO EXIT-RB-OVERRIDE.

           MOVE 15 TO WINDOW-LINES.
           MOVE 60 TO WINDOW-COLUMNS.
      *    MOVE 11 TO WINDOW-BEGIN-Y.
           MOVE 9  TO WINDOW-BEGIN-Y. 
           MOVE 10 TO WINDOW-BEGIN-X.
           MOVE "DLPARV" TO VDU-FORM-NAME.
           MOVE "WI/DLPARV" TO WINDOW-FORM-NAM..

           PERFORM OPEN-WINDOW.
           DISPLAY DLPARV-SCREEN.

           MOVE DLPARV-FORM-FIELDS TO WR-BUF. 
           MOVE DLPARV-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       RB-OVERRIDE-DISPLAY.
           MOVE " " TO OVERRIDE-BUF.
           MOVE LN-DLRVCHGS(1) TO ORIG-DLRVCHGS(1).
           MOVE LN-DLRVCHGS(2) TO ORIG-DLRVCHGS(2).
           MOVE LN-DLRVCHGS(3) TO ORIG-DLRVCHGS(3).
           MOVE LN-DLPARVHELD TO ORIG-DLPARVHELD.
           MOVE LN-DLPARVPAID TO ORIG-DLPARVPAID.
           MOVE LN-ANTICERN(1) TO ORIG-UNEARN.
           MOVE LN-ANTICERN(2) TO ORIG-ADDON.
           ADD LN-DLRVCHGS(1) LN-DLRVCHGS(2)
                LN-DLRVCHGS(3) LN-DLPARVHELD
                 LN-DLPARVPAID LN-ANTICERN(1)
                  LN-ANTICERN(2) GIVING ORIG-TOTAL.

           MOVE CALC-DLRVCHGS-ADJ(1) TO CALC-DLRVCHGS(1).
           MOVE CALC-DLRVCHGS-ADJ(2) TO CALC-DLRVCHGS(2).
           MOVE CALC-DLRVCHGS-ADJ(3) TO CALC-DLRVCHGS(3).
           MOVE CALC-DLPARVHELD-ADJ TO CALC-DLPARVHELD.
           MOVE CALC-DLPARVPAID-ADJ TO CALC-DLPARVPAID.
           MOVE CALC-REBATE TO CALC-UNEARN.
           MOVE CALC-ADDON-INT-REBATE TO CALC-ADDON.
           ADD CALC-DLRVCHGS-ADJ(1) CALC-DLRVCHGS-ADJ(2)
                CALC-DLRVCHGS-ADJ(3) CALC-DLPARVHELD-ADJ
                 CALC-DLPARVPAID-ADJ CALC-REBATE
                  CALC-ADDON-INT-REBATE GIVING CALC-TOTAL.

           MOVE LP-DLRVCHGS-ADJ(1) TO CHG-DLRVCHGS(1)
                                      OVERRIDE-DLRVCHGS(1)
                                      CHG010.
           MOVE LP-DLRVCHGS-ADJ(2) TO CHG-DLRVCHGS(2)
                                      OVERRIDE-DLRVCHGS(2)
                                      CHG020.
           MOVE LP-DLRVCHGS-ADJ(3) TO CHG-DLRVCHGS(3)
                                      OVERRIDE-DLRVCHGS(3)
                                      CHG030.
           MOVE LP-DLPARVHELD-ADJ TO CHG-DLPARVHELD
                                     OVERRIDE-DLPARVHELD
                                     CHG040.
           MOVE LP-DLPARVPAID-ADJ TO CHG-DLPARVPAID
                                     OVERRIDE-DLPARVPAID
                                     CHG050.
           SUBTRACT ADDON-INT-REBATE
                     LP-DLRVCHGS-ADJ(1)
                      LP-DLRVCHGS-ADJ(2)
                       LP-DLRVCHGS-ADJ(3)
                        LP-DLPARVHELD-ADJ
                         LP-DLPARVPAID-ADJ
                              FROM LP-TRAMT GIVING CHG-UNEARN
                                                   OVERRIDE-UNEARN
                                                   CHG060.
           MOVE ADDON-INT-REBATE TO CHG-ADDON
                                    CHG070.
           MOVE LP-TRAMT TO CHG-TOTAL.

           MOVE OVERRIDE-BUF TO WR-BUF.
           MOVE OVERRIDE-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       RB-OVERRIDE-ACCEPT.
           MOVE " " TO IO-TYPE.
           IF BR-DLPARV-ADJ-FG = "D"
              MOVE " RTN-CONTINUE:" TO MESS
              PERFORM SEND-WSEL
              MOVE "E  A010SEL." TO VDU
              PERFORM SCREENIO
              GO TO END-RB-OVERRIDE
           ELSE
              MOVE " F1-RESET, ^-CHANGE DISTRIBUTION, F7-ACCEPT:"
                                                            TO MESS
              PERFORM SEND-WSEL.
           MOVE "E  A010SEL." TO VDU.
           PERFORM SCREENIO.
           IF NOT (VDUSTATUS = "1U" OR "1>")
              GO TO RB-OVERRIDE-ACCEPT.
           IF VDUSTATUS = "1>"
              GO TO RB-OVERRIDE-EXIT.

       RB-OVERRIDE-DLRVCHGS-1.
           MOVE "F1, F7-DONE, ENTER RESERVE CHARGES #1 REFUND:"
                                                   TO MESS.
           PERFORM SEND-WSEL.
           MOVE "ENNS062010." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10"
              PERFORM ALARM
              GO TO RB-OVERRIDE-DISPLAY.
           IF VDUSTATUS = "1>"
              GO TO RB-OVERRIDE-TEST-DONE.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO RB-OVERRIDE-DLRVCHGS-1.
           IF VDUNUM2 < 0
              GO TO RB-OVERRIDE-DLRVCHGS-1.
           IF VDUNUM2 > LN-DLRVCHGS(1)
              GO TO RB-OVERRIDE-DLRVCHGS-1.
           MOVE VDUNUM2 TO CHG010.
           MOVE "SNNS062060." TO VDU.
           COMPUTE VDUNUM2
                    CHG060 = OVERRIDE-DLRVCHGS(1) - CHG010
                            + OVERRIDE-DLRVCHGS(2) - CHG020
                             + OVERRIDE-DLRVCHGS(3) - CHG030
                              + OVERRIDE-DLPARVHELD - CHG040
                               + OVERRIDE-DLPARVPAID - CHG050
                                + OVERRIDE-UNEARN.
           PERFORM SCREENIO.

       RB-OVERRIDE-DLRVCHGS-2.
           MOVE "F1, F7-DONE, ENTER RESERVE CHARGES #2 REFUND:"
                                                   TO MESS.
           PERFORM SEND-WSEL.
           MOVE "ENNS062020." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10"
              PERFORM ALARM
              GO TO RB-OVERRIDE-DISPLAY.
           IF VDUSTATUS = "1>"
              GO TO RB-OVERRIDE-TEST-DONE.
           IF VDUSTATUS = "1U"
              GO TO RB-OVERRIDE-DLRVCHGS-1.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO RB-OVERRIDE-DLRVCHGS-2.
           IF VDUNUM2 < 0
              GO TO RB-OVERRIDE-DLRVCHGS-2.
           IF VDUNUM2 > LN-DLRVCHGS(2)
              GO TO RB-OVERRIDE-DLRVCHGS-2.
           MOVE VDUNUM2 TO CHG020.
           MOVE "SNNS062060." TO VDU.
           COMPUTE VDUNUM2
                    CHG060 = OVERRIDE-DLRVCHGS(1) - CHG010
                            + OVERRIDE-DLRVCHGS(2) - CHG020
                             + OVERRIDE-DLRVCHGS(3) - CHG030
                              + OVERRIDE-DLPARVHELD - CHG040
                               + OVERRIDE-DLPARVPAID - CHG050
                                + OVERRIDE-UNEARN.
           PERFORM SCREENIO.

       RB-OVERRIDE-DLRVCHGS-3.
           MOVE "F1, F7-DONE, ENTER RESERVE CHARGES #3 REFUND:"
                                                   TO MESS.
           PERFORM SEND-WSEL.
           MOVE "ENNS062030." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10"
              PERFORM ALARM
              GO TO RB-OVERRIDE-DISPLAY.
           IF VDUSTATUS = "1>"
              GO TO RB-OVERRIDE-TEST-DONE.
           IF VDUSTATUS = "1U"
              GO TO RB-OVERRIDE-DLRVCHGS-2.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO RB-OVERRIDE-DLRVCHGS-3.
           IF VDUNUM2 < 0
              GO TO RB-OVERRIDE-DLRVCHGS-3.
           IF VDUNUM2 > LN-DLRVCHGS(3)
              GO TO RB-OVERRIDE-DLRVCHGS-3.
           MOVE VDUNUM2 TO CHG030.
           MOVE "SNNS062060." TO VDU.
           COMPUTE VDUNUM2
                    CHG060 = OVERRIDE-DLRVCHGS(1) - CHG010
                            + OVERRIDE-DLRVCHGS(2) - CHG020
                             + OVERRIDE-DLRVCHGS(3) - CHG030
                              + OVERRIDE-DLPARVHELD - CHG040
                               + OVERRIDE-DLPARVPAID - CHG050
                                + OVERRIDE-UNEARN.
           PERFORM SCREENIO.

       RB-OVERRIDE-DLPARVHELD.
           MOVE "F1, F7-DONE, ENTER PARTICIPATION HELD REFUND:"
                                                   TO MESS.
           PERFORM SEND-WSEL.
           MOVE "ENNS062040." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10"
              PERFORM ALARM
              GO TO RB-OVERRIDE-DISPLAY.
           IF VDUSTATUS = "1>"
              GO TO RB-OVERRIDE-TEST-DONE.
           IF VDUSTATUS = "1U"
              GO TO RB-OVERRIDE-DLRVCHGS-3.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO RB-OVERRIDE-DLPARVHELD.
           IF VDUNUM2 < 0
              GO TO RB-OVERRIDE-DLPARVHELD.
           IF VDUNUM2 > LN-DLPARVHELD
              GO TO RB-OVERRIDE-DLPARVHELD.
           MOVE VDUNUM2 TO CHG040.
           MOVE "SNNS062060." TO VDU.
           COMPUTE VDUNUM2
                    CHG060 = OVERRIDE-DLRVCHGS(1) - CHG010
                            + OVERRIDE-DLRVCHGS(2) - CHG020
                             + OVERRIDE-DLRVCHGS(3) - CHG030
                              + OVERRIDE-DLPARVHELD - CHG040
                               + OVERRIDE-DLPARVPAID - CHG050
                                + OVERRIDE-UNEARN.
           PERFORM SCREENIO.


       RB-OVERRIDE-DLPARVPAID.
           MOVE "F1, F7-DONE, ENTER PARTICIPATION PAID REFUND:"
                                                   TO MESS.
           PERFORM SEND-WSEL.
           MOVE "ENNS062050." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10"
              PERFORM ALARM
              GO TO RB-OVERRIDE-DISPLAY.
           IF VDUSTATUS = "1>"
              GO TO RB-OVERRIDE-TEST-DONE.
           IF VDUSTATUS = "1U"
              GO TO RB-OVERRIDE-DLPARVHELD.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO RB-OVERRIDE-DLPARVPAID.
           IF VDUNUM2 < 0
              GO TO RB-OVERRIDE-DLPARVPAID.
           IF VDUNUM2 > LN-DLPARVPAID
              GO TO RB-OVERRIDE-DLPARVPAID.
           MOVE VDUNUM2 TO CHG050.
           MOVE "SNNS062060." TO VDU.
           COMPUTE VDUNUM2
                    CHG060 = OVERRIDE-DLRVCHGS(1) - CHG010
                            + OVERRIDE-DLRVCHGS(2) - CHG020
                             + OVERRIDE-DLRVCHGS(3) - CHG030
                              + OVERRIDE-DLPARVHELD - CHG040
                               + OVERRIDE-DLPARVPAID - CHG050
                                + OVERRIDE-UNEARN.
           PERFORM SCREENIO.

       RB-OVERRIDE-UNEARNED.
           MOVE "F1, F7-DONE, ENTER UNEARNED INT. REFUND:" TO MESS.
           PERFORM SEND-WSEL.
           MOVE "ENNS062060." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10"
              PERFORM ALARM
              GO TO RB-OVERRIDE-DISPLAY.
           IF VDUSTATUS = "1U"
              GO TO RB-OVERRIDE-DLPARVPAID.
           IF VDUSTATUS = "1>"
              GO TO RB-OVERRIDE-TEST-DONE.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO RB-OVERRIDE-UNEARNED.
      *    IF VDUNUM2 < 0
      *       GO TO RB-OVERRIDE-UNEARNED.
           IF VDUNUM2 > (LN-ANTICERN(1)
                              + LN-DLRVCHGS(1)
                               + LN-DLRVCHGS(2)
                                + LN-DLRVCHGS(3)
                                 + LN-DLPARVHELD
                                  + LN-DLPARVPAID)
              GO TO RB-OVERRIDE-UNEARNED.
           MOVE VDUNUM2 TO CHG060.

       RB-OVERRIDE-ADDON.
           MOVE "F1, F7-DONE, ENTER ADDON INTEREST CHG REFUND:"
                                                   TO MESS.
           PERFORM SEND-WSEL.
           MOVE "ENNS062070." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10"
              PERFORM ALARM
              GO TO RB-OVERRIDE-DISPLAY.
           IF VDUSTATUS = "1U"
              GO TO RB-OVERRIDE-UNEARNED.
           IF VDUSTATUS = "1>"
              GO TO RB-OVERRIDE-TEST-DONE.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO RB-OVERRIDE-ADDON.
           IF LN-ANTICERN(2) < 0
              IF VDUNUM2 < LN-ANTICERN(2)
                 GO TO RB-OVERRIDE-ADDON
              ELSE
                 NEXT SENTENCE
           ELSE
              IF VDUNUM2 > LN-ANTICERN(2)
                 GO TO RB-OVERRIDE-ADDON.
           MOVE VDUNUM2 TO CHG070.

       RB-OVERRIDE-TEST-DONE.
           ADD CHG010 CHG020 CHG030
                CHG040 CHG050 CHG060
                 CHG070 GIVING CHG080.
           IF CHG080 > LP-TRAMT
              MOVE "ARE OVER" TO CHG-MESS2
              SUBTRACT LP-TRAMT FROM CHG080 GIVING CHG080-MESS
              MOVE CHG-MESS TO MESS
              PERFORM CONDITIONAL-SEND-MESS
              GO TO RB-OVERRIDE-DLRVCHGS-1.

           IF CHG080 < LP-TRAMT
              MOVE "ARE SHORT" TO CHG-MESS2
              SUBTRACT CHG080 FROM LP-TRAMT GIVING CHG080-MESS
              MOVE CHG-MESS TO MESS
              PERFORM CONDITIONAL-SEND-MESS
              GO TO RB-OVERRIDE-DLRVCHGS-1.

           GO TO RB-OVERRIDE-ACCEPT.

       RB-OVERRIDE-EXIT.
           MOVE CHG010 TO LP-DLRVCHGS-ADJ(1).
           MOVE CHG020 TO LP-DLRVCHGS-ADJ(2).
           MOVE CHG030 TO LP-DLRVCHGS-ADJ(3).
           MOVE CHG040 TO LP-DLPARVHELD-ADJ.
           MOVE CHG050 TO LP-DLPARVPAID-ADJ.
           MOVE CHG070 TO ADDON-INT-REBATE.

       END-RB-OVERRIDE.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY DLPARV-SCREEN.
           PERFORM CLOSE-WINDOW.

           MOVE "LONPF" TO VDU-FORM-NAME.
       EXIT-RB-OVERRIDE.
           EXIT.

      ******************************************
       COMPUTE-RECAPTURE-PRORATA SECTION.
      ******************************************
           IF LP-TRAMT = LN-INTCHG
              MOVE 1 TO DLRECAP-PERCENT
              GO TO COMPUTE-RECAPTURE-PRORATA-EXIT
           ELSE
              MOVE 0 TO DLRECAP-PERCENT.

           MOVE LN-LOANDATE TO NUM-DATE.
           MOVE LP-PAYDATE TO SYS-DATE.
           PERFORM TIM360.
           IF ELAPSED-REM > 0
              ADD 1 TO ELAPSED-MONTHS.
           COMPUTE DLRECAP-PERCENT ROUNDED =
                       1 - ELAPSED-MONTHS / LN-ORGTERM.
           IF DLRECAP-PERCENT < 0
              MOVE 0 TO DLRECAP-PERCENT.

       COMPUTE-RECAPTURE-PRORATA-EXIT.
           EXIT.

      ******************************************************************
       DLRECAP-REDISBURSE SECTION.
      ******************************************************************

      *  ROUTINE ADJUSTS DEALER RECAPTURE TO STOP
      *  A NEGATIVE ADJUSTMENT TO THE FINANCE COMPANIES
      *  EARNINGS ADJUSTMENT.  RE: 'RI'

           IF DLRECAP-WORK NOT > 0
              GO TO DLRECAP-REDISBURSE-EXIT.

           IF DLRECAP-WORK > LP-DLRVCHGS-ADJ(1)
              SUBTRACT LP-DLRVCHGS-ADJ(1) FROM DLRECAP-WORK
              MOVE 0 TO LP-DLRVCHGS-ADJ(1)
           ELSE
              SUBTRACT DLRECAP-WORK FROM LP-DLRVCHGS-ADJ(1)
              GO TO DLRECAP-REDISBURSE-EXIT.
           IF DLRECAP-WORK > LP-DLRVCHGS-ADJ(2)
              SUBTRACT LP-DLRVCHGS-ADJ(2) FROM DLRECAP-WORK
              MOVE 0 TO LP-DLRVCHGS-ADJ(2)
           ELSE
              SUBTRACT DLRECAP-WORK FROM LP-DLRVCHGS-ADJ(2)
              GO TO DLRECAP-REDISBURSE-EXIT.
           IF DLRECAP-WORK > LP-DLRVCHGS-ADJ(3)
              SUBTRACT LP-DLRVCHGS-ADJ(3) FROM DLRECAP-WORK
              MOVE 0 TO LP-DLRVCHGS-ADJ(3)
           ELSE
              SUBTRACT DLRECAP-WORK FROM LP-DLRVCHGS-ADJ(3)
              GO TO DLRECAP-REDISBURSE-EXIT.

           IF DLRECAP-WORK > LP-DLPARVHELD-ADJ
              SUBTRACT LP-DLPARVHELD-ADJ FROM DLRECAP-WORK
              MOVE 0 TO LP-DLPARVHELD-ADJ
           ELSE
              SUBTRACT DLRECAP-WORK FROM LP-DLPARVHELD-ADJ
              GO TO DLRECAP-REDISBURSE-EXIT.
           IF DLRECAP-WORK > LP-DLPARVPAID-ADJ
              SUBTRACT LP-DLPARVPAID-ADJ FROM DLRECAP-WORK
              MOVE 0 TO LP-DLPARVPAID-ADJ
           ELSE
              SUBTRACT DLRECAP-WORK FROM LP-DLPARVPAID-ADJ
              GO TO DLRECAP-REDISBURSE-EXIT.

       DLRECAP-REDISBURSE-EXIT.
           EXIT.

       COPY "LIBGB/DATER.CPY".
       COPY "LIBWI/PFSWINDOWS.CPY".
       COPY "LIBLP/LPIBPC.CPY".
       COPY "LIBLP/LPXDTE.CPY".
       COPY "LIBGB/SPPDTH.CPY".
       COPY "LIBLP/LPITRM.CPY".
       COPY "LIBLP/LPMDTE.CPY".
       COPY "LIBLP/LPBWIN.CPY".
       COPY "LIBLP/LPAPRZ.CPY".
       COPY "LIBLP/LPUPER.CPY".
       COPY "LIBLP/LPUPWK.CPY".
       COPY "LIBLP/LPAPRS.CPY".
       COPY "LIBLP/SETCTBL.CPY".
       COPY "LIBLP/LPEADJ.CPY".
       COPY "LIBLP/LPAMTS.CPY".
       COPY "LIBGB/REBATE.CPY".
       COPY "LIBGB/RBACT.CPY".
       COPY "LIBLP/ADDONSPR.CPY".
       COPY "LIBLP/ADDONRB.CPY".
       COPY "LIBGB/CAPASSWD.CPY".
       COPY "LIBGB/GPENV.CPY".
       COPY "LIBGB/SCREEN.CPY".

      ********************************************
       IO-ROUTINES SECTION.
      ********************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBLP/LPSPGS_SQL.CPY".
       COPY "LIBLP/LPLTPGS_SQL.CPY".
       COPY "LIBLP/LPLTIGS_SQL.CPY".
       COPY "LIBLP/LPLPGS_SQL.CPY".
       COPY "LIBLP/LPDLGS_SQL.CPY".
       COPY "LIBLP/LPCAGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBLP/LPDL1RN.CPY".
       COPY "LIBLP/LPLP1RN.CPY".
       COPY "LIBLP/LPLTI1IN.CPY".
       COPY "LIBLP/LPLTP1RN.CPY".
       COPY "LIBLP/LPSP1RN.CPY".
       COPY "LIBLP/LPCA1RN.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBLP/LPWKI.CPY".

       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO ERRCD.
       END-ROUTINE.
           PERFORM CLOSE-FILES.
       EXIT-PROG.
       STOP-RUN.
           EXIT PROGRAM.
