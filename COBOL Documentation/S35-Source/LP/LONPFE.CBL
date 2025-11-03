       IDENTIFICATION DIVISION.
       PROGRAM-ID.      LONPFE.
       DATE-WRITTEN.    031386.
      *************************************************************************
      *  DESC:  LOAN PROCESSING
      *         TRANSACTION POSTING
      *         PROGRAM 14 -     TRCD'S  SP, DC, AD(1-9),
      *                                  SE, SX OR ZX, SV
      * REV:
      *  042995 SLC ADDED SE
      *  051395 SLC ADDED SX AND SV.
      *  053095 SLC COMBINED OTHBAL AND OT2BAL IN L-090
      *  060695 BLL CHANGED TO USE NEW GL ENTRY ROUTINE
      *  070395 BAH FIXED "DC" CLEARING OT2BAL TO 0.
      *  082895 JTG 'AD' ADDED TEST SKIP ACCESS LATE CHARGES WHEN
      *             THE ACCOUNTIS NOT ALLOWED TO ACCESS
      *             LATE CHARGES:
      *  MJD 040996 ADDED PASSWORD FOR "DC" TRANSACTIONS THAT CHANGE
      *             PAID THRU.
      *  MJD 050296 ADDED GP-AUTO-REPO-WINDOW FOR RELIANCE. AUTOMATICALLY
      *             OPENS CHREPO WINDOW AT ENTER TILL # ON "SS" TRCD IF
      *             GP-AUTO-REPO-WINDOW = "Y".
      *  MJD 071196 ADDED TEST OF LN-UNITPER-CD FOR WEEKLY PDTH
      *  MJD 071996 ADDED LOGIC TO PRINT LN-1STPYDATE AS MM/DDWYY
      *             FOR WEEKLY UNIT PERIODS.
      *  JTG 101196 ADDED 'LXE-WORKER' & 'LXE-WORKER2' FOR LPCERN-- A50 VERSION
      *                   SP-ERNFRMLA 15 & 16, REAL INTEREST BEARINGING METHOD
      *                                            - INSTANT AUTO, KY
      *  JTG 032797 CORRECTED 'DC' POSTING, SOME CASES NOT ADVANCING
      *             LCPDTH CORRECTLY
      *  GLF 050797 ADDED COPY MEMBERS: SETCTBLW AND SETCTBL
      *  JTG 071597 ADDED TEST ON 'AD' TO INSURE LCPDTH DOSN'T FALL BEHIND PDTH
      *  GLF 090897 REPLACED 'IO' ON GP-FILE WITH GET 'GPENV'
      *  GLF 101497 ALLOWED CTRL 'N'
      *  MJD 101797 MOVED WK-UPDATE-BUF TO LP01WK
      *  BLV 110397 MOVE IN BR-GL-TYPE-ALLOWED, BR-GL-ENTRY-DEFAULT TO
      *             ALLENT-BUF BEFORE GL DISTRIBUTION WINDOW
      *  030498 BAH ADDED TEST OF 'DC' DATE CHANGE STAYING THE SAME BECAUSE
      *             OF AN INVALID DAY IN FEB.,29,30,31
      *  040198 BAH ADDED GP-REPODATE-PAYDATE FOR FEDERATED AND RAC
      *             ALSO, 'SV' WAS SETTING SOLDDATE, SHOULD BE REDEEMDATE !!
      *  BAH 062698 TILLNO IS NOW ALPHANUMERIC
      *  JTG 072098 REMOVED PASSWORD TEST ON POSTING A ALLOWABLE DELINQUENCY
      *             AND A DUE DATE CHANGE, VIA AD-DC-SEQ = 0, SINCE LONPF1
      *             COVERS TEST TO POST 'DC'
      *  BAH 073098 DISPLAY BALANCES EVEN ON REVERSALS, #487
      *  MJD 083398 ADDED ENVIR VARIABLE TO DETERMINE WHETHER OR NOT
      *             TO DISPLAY BALANCES ON REVERSALS. FIXED DISPLAY TO INDICATE
      *             INCLUDED O2 BAL WITH "SC" "RN" "PB" "RB" "RO" TRANSACTIONS
      *  JTG 101298 ALLOWED DISPLAY OF NEGATIVE CURBAL REVOL REGENCY
      *  JTG 121698 DISPLAYED 'R' VIA UNITPER-MASK LOGIC FOR REVOLVING ACCOUNT
      *  GLB 990720 ADD CASH DRAWER PASSWORD OPTION TO ENTER DRAWER NO.
      *             BR-PASSWORD-DRAWER(Y/N) & CA-PASSWORD (4 CHAR PASSWORD)
      *             REGMGM PR# 901
      *  MJD 990806 ADDED GP-INCL-OTH-IN-EARNED FLAG. IF "Y" THEN INCLUDE
      *             EARNINGS BUCKET 6 (OTHER) IN ACCUMULATED TOTALS
      *             OTHERWISE EXCLUDE BUCKET 6.
      *  BLV 102799 STARRED OFF LINE MOVING LN-DLPOOL TO LP-PRIOR-POOL IN
      *             INITIALIZE-ROUTINE: THIS LOGIC IS CORRECTLY IN INITIALIZE-
      *             SP, BUT THE MOVE IN INITIALIZE-ROUTINE PUT THE POOL ID IN
      *             REDEFINED FIELD LP-APINTOWE FOR OTHER TRANS TYPES LIKE
      *             DC, & REVERSAL OF DC WOULD PUT NEG # WITH POOL ID IN
      *             LP-INTBAL & LN-INTBAL
      *  JTG 000301 ADDED TEST ON TRCD = 'SX' AND 'SV' IN SELECTION-EXIT-PROG
      *             ROUTINE SINCE THESE TRANSACTIONS DO NOT COMPUTE LATE CHGS
      *             RE: TFC20 PROBLEM #09-486252 WHICH PUT $0.01 INTO
      *                 LP-APINTOWE AND DON'T KNOW WHY!!!!! REVERSAL OF 'SX'
      *                 PUT A -0.01 INTO LP-LCBAL
      *  JTG 000307 ADDED TEST ON TRCD = 'DC' IN SELECTION-EXIT-PROG
      *             ROUTINE SINCE THESE TRANSACTIONS DO NOT COMPUTE LATE CHGS
      *             RE: TFC20 PROBLEM #09-471352 WHICH PUT $0.01 INTO
      *                 LP-APINTOWE AND DON'T KNOW WHY!!!!! REVERSAL OF 'DC'
      *                 PUT A -0.01 INTO LP-LCBAL
      *  MG  010201 REMOVED "01 WK-LXE" AND ADDED LIBLP/LPWKLXE.
      *  JTG 010906 ALLOWED IB TYPE LOANS TO ACCELERATE     JIM R. (ALL) PR#1582
      *  JTG 020426 ADDED NEW RATE TYPE 'Z' AMORTIZED WHERE RUNTIME COMPUTES
      *             INTEREST ON A 30 DAY METHOD (REAL ESTATE) AND LOAN CALCS
      *             ARE DONE SIMPLE, REMOVED TIE TO CTBL, FORCING TESTS ON (1)
      *                                             LENDMARK PR#1045 & PR#1380
      *  CS  021112 ADDED CA-PATH FOR INTERBRANCH POSTING OF 'SP' [PR#308 WORLD]
      *  BLV 030325 FIXED READ OF BRANCH TO USE OWNING BRANCH, NOT BRANCH (WHICH
      *             IS POSTING BRANCH IF INTERBRANCH.  ALSO, FIXED TO CHECK
      *             CASH DRAWER PASSWORD FLAG OF POSTING BRANCH, NOT OWNING BR
      *  JTG 040722 CREATED NEW TRCD 'ZX' WHAT WORKS LIKE 'SX'     CENTRIX #2314
      *   CS 081113 ADD TEST GP-INCL-OTH-IN-EARNED = 'R'. IF 'R' EXCLUDE
      *             OTHER(ANTICERN(6) AND MAINTFEE(ANTICERN(4) FROM EARNED
      *             AND UNEARNED    REGACC PR#210
      *  BLM 100309 CREATED NEW TRCD 'ED' ESCROW DISBURSEMENT, CREATE FORCED
      *             PASSWORD FOR ALLOWING ESCROW BAL TO GO NEGATIVE:
      *             LP/LONPFE/2.  REGWAN PR# 3506
      *  BLM 100309 CREATED NEW TRCD 'EP' ESCROW PREPAYMENT, REGWAN PR# 3506
      *  BAH 130111 ADDED LC-DISPLAY-ERN OPTION OF "I" FOR POFF-INTDUE
      *                                              AMERICAN ACC PR# 4190
      *  BAH 140814 ADDED TEST FOR LPSMNU IN HELP
      *  BLM 141029 FIX ENTRY OF PO CODE, USE C$JUSTIFY INSTEAD OF MESSING WITH
      *             VDUPROCESS.  
      *   CS 150608 IN SETUP-LINE ADDED UNITPER-CD 'B' TO TEST ON 'D,W,S', 'B'
      *             WAS MOVING  NUM-MO TO NUM-DA AND THEN MOVING NUM-DATE TO
      *             EB-080 WHICH DISPLAYED PAIDTHRU AS MONTHLY. NOW MOVES
      *             DAY/MON TO EB-080  PL 883 AUTOMOTIVE SERVICES
      *  BAH 160125 SPLIT OUT LTFILE, PD0003
      *   CS 160222 REMOVE COPY LPWKLXE, ITS AN EARNINGS COPY MEMBER THAT IS
      *             PASSED IN LOAN PROCESSING VIA LP01WK USING WK-LX2-REC. 
      *             REPLACED WK-LX2-REC WITH WK-LXE-REC IN LP01WK 
      *             MOVED LPWKLXE TO LPWKLXE.NO
      *             SAVED VERSION LONPFE.160222
      *  BAH 160304 CHANGED TILLNO TO NUMERIC BUT NO ROOM ON SCREEN FOR 2 DIGITS
      *
      * KEC 2016.0311 {PD#00003} PARADATA <A30> [JAY CISZEWSKI]
      *         CHANGED CD-POOL???? TO CD-PL-????
      *
      * KEC 2016.0324 {PD#00003} PARADATA <A30> [JAY CISZEWSKI]
      *         CHANGED CDSCAN-WINDOW-BUF TO HAVE MATCHING CD1-KEY
      *         LAYOUTS (FOR EACH CODE TYPE; CD-TYPE).
      *         CDSCAN-CD1-KEY IS THE NEW (PRIMARY) WITH REDEFINES OF
      *         CDSCAN-??-KEY (NEW).
      *
      * BAH 161117 ADDED 2 DIGIT FIELD "DRAWER" ABOVE PAYMENT LINE FOR 
      *            NEW CASH DRAWER ENTRY
      * BAH 170619 ACTIVATE 8 DIGIT DATES, PD0006
      * BAH 171129 FIXED WK-LXE-REC
      * BAH 181231 GLOBAL LPFILE
      * BAH 190117 GLOBAL LXFILE
      * BAH 20190308 MOVED LP-REFNO REDEFINITION TO LPWSLP COPY MEMBER
      *              LP-POOLID, LP-TODD, LP-FROMDD
      * BAH 20190625 REMOVED LN-REVOLVING, PT0153
      * BAH 20200702 REMOVED CLOSE-BR-FILE #TST001
      * BAH 2022.0314 REMOVED EXT-SPANISH-FORMS AND ALL MX FORMS
      * BAH 2022.0818 REMOVED GLOBRD, READ GB-FILE INSTEAD
      * BAH 2022.0823 HARD CODED START
      * JKC 2022.0930 CHANGED TO USE START-LP1-FILE-SCAN INSTEAD OF
      *          START-LP1-FILE; NEED TO USE START-LP1-FILE-SCAN WHEN
      *          START-LP1-FILE-NOT-GREATER IS USED FIRST; NEEDED FOR
      *          FOR DISPLAYING MULTIPLE PAGES IN AN UP & DOWN FUNCTION;
      *          NEED TO ADD ONE TO LP-SEQNO (IN WBEG) IN ORDER TO GET 
      *          THE DISPLAY TO WORK LIKE WITH VISION FILES.
      * BAH 2024.0417 REMOVED CHREPOW, CALL PROGRAM INSTEAD OF WINDOW
      **************************************************************************
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
           "@(#)LP/LONPFE.CBL S35 09/30/22-15:47:28 >".
      **************************************************
      *     P R O G R A M   W O R K E R S              *
      **************************************************
       COPY "LIBLP/LP01LP.CPY".
       COPY "LIBLP/LP01LP_SQL.CPY".
       COPY "LIBLP/LPWSLP.CPY".
       COPY "LIBLP/LP01LX.CPY".
       COPY "LIBLP/LP01LX_SQL.CPY".
       COPY "LIBLP/LPWSLX.CPY".
       COPY "LIBLP/LP01LXE.CPY".
       COPY "LIBLP/LP01LXE_SQL.CPY".
       COPY "LIBLP/LPWSLXE.CPY".
       COPY "LIBLP/LP01LXG.CPY".
       COPY "LIBLP/LP01LXG_SQL.CPY".
       COPY "LIBLP/LPWSLXG.CPY".
       COPY "LIBLP/LP01LTP.CPY".
       COPY "LIBLP/LP01LTP_SQL.CPY".
       COPY "LIBLP/LPWSLTP.CPY".
       COPY "LIBLP/LP01LN.CPY".
       COPY "LIBLP/LP01SP.CPY".
       COPY "LIBLP/LP01SPA.CPY".
       COPY "LIBLP/LP01SPB.CPY".
       COPY "LIBLP/LP01SPC.CPY".
       COPY "LIBLP/LP01SP_SQL.CPY".
       COPY "LIBLP/LPWSSP.CPY".
       COPY "LIBLP/LP01CA.CPY".
       COPY "LIBLP/LP01CA_SQL.CPY".
       COPY "LIBLP/LPWSCA.CPY".
       COPY "LIBLP/LP01CD.CPY".
       COPY "LIBLP/LP01CD_SQL.CPY".
       COPY "LIBLP/LPWSCD.CPY".
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBGB/GB01GB.CPY".
       COPY "LIBGB/GB01GB_SQL.CPY".
       COPY "LIBGB/GBWSGB.CPY".
       COPY "LIBLP/LP01ES.CPY".
       COPY "LIBLP/LP01ES_SQL.CPY".
       COPY "LIBLP/LPWSES.CPY".
       01  WK-IFN              PIC XX       VALUE "WK".


       01  WK-KEY              PIC 9(2)     VALUE 0.

       01  ELE                 PIC 99                VALUE 0.
       01  TOTEARN             PIC S9(6)V99  COMP-3  VALUE 0.
       01  UNEARNED            PIC S9(6)V99  COMP-3  VALUE 0.
       01  LN-WK               PIC 9(6)      COMP-3  VALUE 0.
       01  GL-WORK             PIC S9(7)V99  COMP-3  VALUE 0.
       01  DISP-REV-BAL        PIC X                 VALUE "Y".

       01  SUB                 PIC 9(6) COMP-3       VALUE 0.

       01  TRAN-CODE.
           03  PART-1          PIC X.
           03  NUM-ALDEL       PIC X.
           03  NO-ALDEL REDEFINES NUM-ALDEL
                               PIC 9.

       01  TEMP-TRCD           PIC XX.

       01  SV-PASSWORD-DRAWER  PIC X                VALUE "N".

       01  D-ESCROW-BAL        PIC $ZZZ,ZZZ.99-.

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
           03  D-LCPDTH        PIC 9999.
           03  D-DEFMNTS       PIC 99.
           03  D-ALDEL         PIC 99        BLANK ZERO.
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
           03  FILLER          PIC X(4) VALUE
           "LN1 ".
           03  FILLER          PIC X(24) VALUE
           "GL8 F21 GD8 F22 GA8 F23 ".
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

       01  GL-LIST.
           03  FILLER          PIC X(36) VALUE
           "GL1 GL2 GL3 GD1 GD2 GD3 GA1 GA2 GA3.".

       01  EARN-LIST.
           03  FILLER          PIC X(34) VALUE
           "H09 H10 H17 H18 H181 H21 PROCEEDS.".

       01  PL-NEWAR-MESS.
           03  FILLER            PIC X(39) VALUE
           "THE AMOUNT OF REMAINING NEW P&L AR IS (".
           03  PL-NEWAR-MESS-AMT PIC $ZZZ,ZZ9.99.
           03  FILLER            PIC X(2)  VALUE
           "):".

       01  DUEDATE-MESS.
           03  FILLER          PIC X(29) VALUE
           "THE NEW 1ST PAYMENT DATE IS: ".
           03  DUEDATE         PIC 99/99/99.
           03  FILLER REDEFINES DUEDATE.
               05  DUE-MM      PIC 99.
               05  FILLER      PIC X.
               05  DUE-DD      PIC 99.
               05  FILLER      PIC X.
               05  DUE-YY      PIC 99.

       01  NEW-1STPYATTR       PIC X(4)  VALUE "FU".
       01  NEW-1STPYDATE       PIC 99/99/99.
       01  NEW-1STPYDATEX     REDEFINES NEW-1STPYDATE PIC X(8).
       01  NEW-1STPYLIST       PIC X(4)  VALUE "H02.".

       01  NEW-OLDMDTEDATE     PIC 99/99/99.
       01  NEW-MDTEATTR        PIC X(4)  VALUE "FU".
       01  NEW-MDTEDATE        PIC 99/99/99.
       01  NEW-MDTELIST        PIC X(4)  VALUE "H03.".

       01  LCPDTH-LIST         PIC X(4)  VALUE "H17.".
       01  D-H17               PIC 9999.

       COPY "LIBWI/CDSCANW.CPY".
       COPY "LIBGB/CAPASSWDW.CPY".
       COPY "LIBLP/LPWKPATH.CPY".
       COPY "LIBLP/LPLCASW.CPY".
       COPY "LIBLP/LPSCHDW.CPY".
       COPY "LIBLP/LPLCHGW.CPY".
       COPY "LIBLP/LPIBPCW.CPY".
       COPY "LIBLP/LPXDTEW.CPY".
       COPY "LIBGB/SPPDTHW.CPY".
       COPY "LIBLP/LPMDTEW.CPY".
       COPY "LIBGL/ALLENTW.CPY".
       COPY "LIBLP/LPLCAPW.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBLP/SETCTBLW.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBLP/LPMENUW.CPY".
       COPY "LIBGB/PASSWDW.CPY".
       01    AD-SEQ             PIC 9  VALUE 0.
       01    DC-SEQ             PIC 9  VALUE 1.
       01    ED-SEQ             PIC 9  VALUE 2.

       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/GPENVW.CPY".
       COPY "LIBCL/CLMENUW_EXT.CPY".
       COPY "LIBGB/SYSTEMW.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBLP/LPUPWKW.CPY".

       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBLP/LONPF_DEF.CPY".
       COPY "LIBLP/LPSMNU_DEF.CPY".
       COPY "LIBLP/LPLNMU_DEF.CPY".
       COPY "LIBLP/LPCAMU_DEF.CPY".
       COPY "LIBLP/LPVDMU_DEF.CPY".

       COPY "LIBLP/LONPF_WKS.CPY".
       COPY "LIBLP/LPSMNU_WKS.CPY".
       COPY "LIBLP/LPLNMU_WKS.CPY".
       COPY "LIBLP/LPCAMU_WKS.CPY".
       COPY "LIBLP/LPVDMU_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".

       COPY "LIBGB/GETFMW.CPY".
       COPY "LIBLP/LPLOANW.CPY".
       COPY "LIBLP/LPPFBUF.CPY".

       SCREEN SECTION.
       COPY "LIBLP/LONPF_SCN.CPY".
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
           PERFORM CLOSE-LP1-FILE.
           PERFORM CLOSE-ES1-FILE.
           PERFORM CLOSE-CD1-FILE.
           PERFORM CLOSE-CA1-FILE.
           PERFORM CLOSE-BR-FILE.
           PERFORM CLOSE-GB-FILE.
           CLOSE WK-FILE.
       COPY "LIBGB/DECLAR2.CPY".

      **************************************************
      *      P R O G R A M   C O N T R O L             *
      **************************************************
       MAIN-PROGRAM SECTION.
           PERFORM SQL-CONNECT.

      D    STOP MESS.
           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "LONPF" TO VDU-FORM-NAME.

           MOVE USER-PID TO TEMP-PID.

           IF INTERRUPT-FG = "Y"
              MOVE " " TO VDU-WIMENU-SELECTION
           ELSE
              MOVE "C" TO VDU-WIMENU-SELECTION.

           MOVE "F" TO VDU-CLMENU-SELECTION.

           PERFORM GET-GPENV.

           MOVE 0 TO IO-FG.
           MOVE SPACES TO ERRCD ROUTE-CD.
           PERFORM RESTORE-SCREEN.

           PERFORM OPEN-SP1-FILE.
           MOVE HOLD-SP1-KEY TO SP1-KEY.
           PERFORM READ-SP1-FILE.
           PERFORM CLOSE-SP1-FILE.
           IF IO-FG NOT = 0
              MOVE "INVALID STATE PARAMETER RECORD" TO MESS
              PERFORM SEND-MESS
              MOVE "X" TO ERRCD
              GO TO END-ROUTINE.

      * ALLOW CNTRL 'N' INTERRUPT:
           MOVE "WI/MAKERS" TO VDU-CTRL-N-PROG.

      * LEAVE BR-FILE OPEN
           MOVE LN-OWNBR TO BR-NO.
           PERFORM OPEN-BR-FILE.
           PERFORM READ-BR-FILE.
           IF IO-FG NOT = 0
              MOVE "INVALID BRANCH RECORD" TO MESS
              PERFORM SEND-MESS
              MOVE "X" TO ERRCD
              GO TO END-ROUTINE.

      * NEED FOR LCAS ROUTINES (REPLACES GLOBRD)
           PERFORM OPEN-GB-FILE.
           MOVE GB-PATH-OWNBR TO GB-BRNO.
           MOVE 1             TO GB-NO.
           PERFORM READ-GB-FILE.
           PERFORM CLOSE-GB-FILE.
           IF IO-BAD
              MOVE "INVALID GLOBAL RECORD" TO MESS
              PERFORM SEND-MESS
              MOVE "X" TO ERRCD
              GO TO END-ROUTINE.

           IF LP-TRCD = "DC"
              MOVE LN-1STPYDATE TO HOLD-NEW-1ST-DUE
              PERFORM MATURITY-DATE-CALCULATION
              MOVE MDTE-DATE    TO DATE-YYYYMMDD
              PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
              MOVE DATE-MMDDYY  TO NEW-OLDMDTEDATE
           ELSE
           IF LP-TRCD = "AD" OR "A2" OR "A3" OR "A4" OR "A5" OR
                        "A6" OR "A7" OR "A8" OR "A9"
              MOVE LN-TOTNOALDEL TO HOLD-NEW-1ST-DUE.

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

           PERFORM SELECTION-MODULE.
           IF LP-TRCD = "DC"
              MOVE LN-1STPYDATE  TO HOLD-NEW-1ST-DUE
           ELSE
           IF LP-TRCD = "AD" OR "A2" OR "A3" OR "A4" OR "A5" OR
                        "A6" OR "A7" OR "A8" OR "A9"
              MOVE LN-TOTNOALDEL TO HOLD-NEW-1ST-DUE.
           GO TO END-ROUTINE.

      ***************************************
       SELECTION-MODULE SECTION.
      ***************************************
       INITIAL-TRCD.
           PERFORM INITIALIZE-ROUTINE.
           IF ROUTE-CD = "7" OR "8"
              GO TO RET-TO-ENTRY-ROUTINE.

           IF LP-TRCD = "SE"
              PERFORM AMOUNT-ENTRY-ROUTINE
              IF ROUTE-CD = "1"
                 GO TO INITIAL-TRCD
              ELSE
                 IF ROUTE-CD = 7
                    GO TO RET-TO-ENTRY-ROUTINE.

           IF LP-TRCD = "ED"
              PERFORM DISBURSE-ESCROW-ROUTINE
              IF ROUTE-CD = "1"
                 GO TO INITIAL-TRCD
              ELSE
                 IF ROUTE-CD = 7
                    GO TO RET-TO-ENTRY-ROUTINE.

           IF LP-TRCD = "EP"
              PERFORM ESCROW-PREPAID-ROUTINE
              IF ROUTE-CD = "1"
                 GO TO INITIAL-TRCD
              ELSE
                 IF ROUTE-CD = 7
                    GO TO RET-TO-ENTRY-ROUTINE.

           IF LP-TRCD = "SX" OR "ZX" OR "SV"
              PERFORM SET-REPO-AND-STATUS
              IF ROUTE-CD = "1"
                 GO TO INITIAL-TRCD
              ELSE
                 IF ROUTE-CD = 7
                    GO TO RET-TO-ENTRY-ROUTINE.

           PERFORM DISPLAY-ENTRY-LINE.
           IF ROUTE-CD = "7"
              GO TO RET-TO-ENTRY-ROUTINE.


      *    --------------------------------------------------------------
      *    RELIANCE: IF POSTING AN "SX" OR "ZX" THEN BRING UP REPO STATUS
      *        WINDOW AUTOMATICALLY.
      *    --------------------------------------------------------------
           IF LP-TRCD = "SX" OR "ZX"
              IF GP-AUTO-REPO-WINDOW = "Y"
                 PERFORM CALL-REPO-WINDOW.

           PERFORM ACCEPT-ENTRY-ROUTINE.
           IF ROUTE-CD = "7"
              GO TO RET-TO-ENTRY-ROUTINE.

           MOVE "3" TO ROUTE-CD.
           MOVE 2 TO NEW-PROG-SUB.

           GO TO SELECTION-EXIT-PROG.

       RET-TO-ENTRY-ROUTINE.
           MOVE "1" TO NEW-PROG-SUB.
       SELECTION-EXIT-PROG.
           MOVE "P" TO ERRCD.
           IF LP-TRCD = "AD" OR "A2" OR "A3" OR "A4" OR "A5"
                             OR "A6" OR "A7" OR "A8" OR "A9"
                             OR "SX" OR "ZX" OR "SV" OR "DC"
              MOVE 0 TO LP-APINTOWE.

      * SET LFP FIELDS
           MOVE LN-EXTRA-PRIN-LFP        TO LP-EXTRA-PRIN-LFP.
           MOVE LN-REMAIN-INT-SINCE-LFP  TO LP-REMAIN-INT-SINCE-LFP.
           MOVE LN-REMAIN-PRIN-SINCE-LFP TO LP-REMAIN-PRIN-SINCE-LFP.

           PERFORM SAVE-SCREEN.

       SELECTION-MODULE-EXIT.
           EXIT.

      ******************************************
       INITIALIZE-ROUTINE SECTION.
      ******************************************

           MOVE LN-LCPDTH-DATE TO LCAP-LCPDTH-DATE LP-LCPDTH-DATE.
           MOVE LN-OTHBAL TO LP-OTHBAL.
           MOVE LN-OT2BAL TO LP-OT2BAL.
           MOVE LN-INTBAL TO LP-INTBAL.
           MOVE LN-LCBAL TO LP-LCBAL.
           MOVE LN-CURBAL TO LP-CURBAL.

      *    MOVE LN-DLPOOL TO LP-PRIOR-POOLID.

           MOVE LP-PAYDATE TO IBPC-DATE.
           PERFORM IBPC-TEST.
           MOVE IBPC-FG TO LP-IBPC.

           IF LP-TRCD = "EP"
              GO TO INITIALIZE-EXIT.

           IF LP-TRCD = "ED"
              MOVE LN-ACCTNO TO ES-ACCTNO
              PERFORM OPEN-ES1-FILE
              PERFORM READ-ES1-FILE
              PERFORM CLOSE-ES1-FILE
              IF IO-BAD
                 MOVE "ERROR: ESCROW RECORD NOT FOUND FOR ACCOUNT!"
                    TO MESS
                 PERFORM SEND-MESS
                 MOVE "X" TO ERRCD
                 GO TO END-ROUTINE
              ELSE
                 GO TO INITIALIZE-EXIT.

           IF LP-TRCD = "SX" OR "ZX" OR "SV"
              GO TO INITIALIZE-EXIT
           ELSE
           IF LP-TRCD = "SE"
              GO TO INITIALIZE-SEND-PMPT
           ELSE
           IF LP-TRCD = "SP"
              GO TO INITIALIZE-SP
           ELSE
           IF LP-TRCD = "DC"
              GO TO INITIALIZE-DC
           ELSE
           IF LP-TRCD = "AD" OR "A2" OR "A3" OR "A4" OR "A5" OR
                        "A6" OR "A7" OR "A8" OR "A9"
      *       MOVE AD-DC-SEQ TO PASSWDW-SEQ
              MOVE AD-SEQ TO PASSWDW-SEQ
              MOVE "TO POST ALLOWABLE DELINQ:" TO PASSWDW-PROMPT2
              PERFORM VERIFY-PASSWORD
              IF PASSWDW-STAT NOT = "OK"
                 MOVE "7" TO ROUTE-CD
                 MOVE "ALLOWABLE DELINQ POSTING REQUIRES PASSWORD"
                                                TO MESS
                 PERFORM SEND-MESS
                 GO TO INITIALIZE-EXIT
              ELSE
                 GO TO INITIALIZE-AD.

       INITIALIZE-DC.
           MOVE LN-ORIG-1STPYDATE TO NDTE-DATE.
           MOVE HOLD-NEW-1ST-DUE  TO LP-OLD1ST-PYDATE.

      * MOVED LP-REFNO REDEFINITIONS TO LPWSLP COPY MEMBER FOR SQL REASONS
           MOVE LP-REFNO          TO LPWSLP-LP-REFNO.

           IF LPWSLP-LP-FROMDD = LPWSLP-LP-TODD
              MOVE "'FROM' AND 'TO' DATE CAN NOT BE THE SAME." TO MESS
              PERFORM SEND-MESS
              MOVE "7" TO ROUTE-CD
              GO TO INITIALIZE-EXIT.

      *    ------------------------------------------------------
      *    IF NEW DAY >= ORIG DAY
      *       IF NEW DAY > OLD DAY AND FROM DAY < ORIG DAY
      *    ------------------------------------------------------
           IF LPWSLP-LP-TODD >= NDTE-DD
              IF LPWSLP-LP-TODD > LPWSLP-LP-FROMDD AND
                  LPWSLP-LP-FROMDD < NDTE-DD
                 MOVE LP-LCPDTH-DATE TO NDTE-DATE
                 MOVE LPWSLP-LP-TODD TO NDTE-DD
                 MOVE -1             TO NDTE-HOLD
                 PERFORM INCREMENT-MONTHS
                 MOVE NDTE-DATE TO LP-LCPDTH-DATE
                                   LCAP-LCPDTH-DATE

                 MOVE LN-ORIG-1STPYDATE TO NDTE-DATE
                 MOVE LPWSLP-LP-TODD    TO NDTE-DD
                 PERFORM NEW-DATE-CALCULATION
                 MOVE NDTE-DATE         TO DATE-YYYYMMDD
                 PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
                 MOVE DATE-MMDDYY       TO DUEDATE
              ELSE
                 MOVE LP-LCPDTH-DATE TO NDTE-DATE
                 MOVE LPWSLP-LP-TODD TO NDTE-DD
                 PERFORM NEW-DATE-CALCULATION
                 MOVE NDTE-DATE      TO LP-LCPDTH-DATE
                                        LCAP-LCPDTH-DATE

                 MOVE LN-ORIG-1STPYDATE TO NDTE-DATE
                 MOVE LPWSLP-LP-TODD    TO NDTE-DD
                 PERFORM NEW-DATE-CALCULATION
                 MOVE NDTE-DATE         TO DATE-YYYYMMDD
                 PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
                 MOVE DATE-MMDDYY       TO DUEDATE
           ELSE
      *    ------------------------------------------------------
      *    IF FROM DAY >= ORIG DAY
      *      (CHANGES PAID THRU)
      *    ------------------------------------------------------
           IF LPWSLP-LP-FROMDD >= NDTE-DD
              MOVE DC-SEQ TO PASSWDW-SEQ
              MOVE "TO CHANGE PAID THRU:" TO PASSWDW-PROMPT2
              PERFORM VERIFY-PASSWORD
              IF PASSWDW-STAT NOT = "OK"
                 MOVE "7" TO ROUTE-CD
                 MOVE "CHANGING PAID THRU REQUIRES PASSWORD" TO MESS
                 PERFORM SEND-MESS
                 GO TO INITIALIZE-EXIT
              END-IF
              MOVE LP-LCPDTH-DATE TO NDTE-DATE
              MOVE 01             TO NDTE-DD
              MOVE 1              TO NDTE-HOLD
              PERFORM INCREMENT-MONTHS
              MOVE LPWSLP-LP-TODD TO NDTE-DD
              MOVE NDTE-DATE      TO LP-LCPDTH-DATE
                                     LCAP-LCPDTH-DATE

              MOVE LN-ORIG-1STPYDATE TO NDTE-DATE
              MOVE 01                TO NDTE-DD
              MOVE 1                 TO NDTE-HOLD
              PERFORM INCREMENT-MONTHS
              MOVE LPWSLP-LP-TODD    TO NDTE-DD
              MOVE NDTE-DATE         TO DATE-YYYYMMDD
              PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
              MOVE DATE-MMDDYY       TO DUEDATE.

      *----------------------------------------------------------------
      * THIS 'IF' GETS HIT ON A SECOND DUE DATE CHANGE
      * EX:
      *      ORIG-1STPAY = 4/8/96
      *          FROM-DD = 4/8/96
      *  CHANGE TO NEW DAY 6, FIRST TIME THRU, HITS THE 'IF' ABOVE
      *    WHICH BUMPS LCPDTH TO 4/96 AND BUMPS 1STPAY TO 5/6/96
      * NOW;
      *  CHANGE TO NEW DAY 5, HITS THE 'IF' BELOW
      *    WHICH BUMPS 1STPAY TO 5/5/96, BECAUSE IT USES ORIG-1STPAY
      *    SHOULD LEAVE LCPDTH ALONE !!
      *
      *----------------------------------------------------------------
           IF LPWSLP-LP-TODD < NDTE-DD
              MOVE LP-LCPDTH-DATE TO NDTE-DATE
              MOVE 01             TO NDTE-DD
              PERFORM NEW-DATE-CALCULATION
              MOVE LPWSLP-LP-TODD TO NDTE-DD
              MOVE NDTE-DATE      TO LP-LCPDTH-DATE
                                     LCAP-LCPDTH-DATE

              MOVE LN-ORIG-1STPYDATE TO NDTE-DATE
              MOVE LPWSLP-LP-TODD    TO NDTE-DD
              MOVE 1                 TO NDTE-HOLD
              PERFORM INCREMENT-MONTHS
              MOVE LPWSLP-LP-TODD    TO NDTE-DD
              MOVE NDTE-DATE         TO DATE-YYYYMMDD
              PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
              MOVE DATE-MMDDYY       TO DUEDATE.

      * THE NEW 1ST PAYMENT DATE IS:
           MOVE DUEDATE-MESS TO MESS.
           PERFORM SEND-MESS.

           IF DUE-DD NOT = LPWSLP-LP-TODD
              MOVE "NOTICE: DAY IS NOT WHAT YOU REQUESTED !!" TO MESS
              PERFORM SEND-MESS.

           COMPUTE DATE-MMDDYY = (DUE-MM * 10000) + (DUE-DD * 100) +
                                  DUE-YY.
           PERFORM CONVERT-MMDDYY-TO-YYYYMMDD.
           MOVE DATE-YYYYMMDD TO NDTE-DATE.
           MOVE NDTE-DATE TO LN-1STPYDATE
                             LP-NEW1ST-PYDATE.

           MOVE LN-1STPYDATE    TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY     TO ALP-DATE-EDIT.
           MOVE LN-UNITPER-CD   TO DATER-UNITPER-CD.
           MOVE LN-UNITPER-FREQ TO DATER-UNITPER-FREQ.
           PERFORM UNITPER-MASK.
           MOVE ALP-DATE-EDIT   TO NEW-1STPYDATE.
           MOVE "SV" TO NEW-1STPYATTR.

           MOVE NEW-1STPYDATEX TO WR-BUF.
           MOVE NEW-1STPYLIST  TO WR-LIST.
           PERFORM CALL-WRFORM.

           PERFORM MATURITY-DATE-CALCULATION.
           MOVE MDTE-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO NEW-MDTEDATE.
           MOVE "SV"        TO NEW-MDTEATTR.
           MOVE NEW-MDTEDATE TO WR-BUF.
           MOVE NEW-MDTELIST TO WR-LIST.
           PERFORM CALL-WRFORM.

           MOVE LCAP-LCPDTH-DATE TO NUM-DATE.
           IF LN-UNITPER-CD = "D" OR "W" OR "S" OR "B"
              MOVE NUM-DA   TO NUM-YR
              MOVE NUM-MO   TO NUM-DA
              MOVE NUM-DATE TO D-H17
           ELSE
              MOVE NUM-MO   TO NUM-DA
              MOVE NUM-DATE TO D-H17.
           MOVE D-H17 TO WR-BUF.
           MOVE LCPDTH-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

           GO TO INITIALIZE-SEND-PMPT.

       INITIALIZE-AD.
           MOVE LP-TRCD TO TRAN-CODE TEMP-TRCD.
           IF NUM-ALDEL NUMERIC
              ADD NO-ALDEL TO LN-TOTNOALDEL
           ELSE
              ADD 1        TO LN-TOTNOALDEL.

      *JTG ADDED TEST FOR ACCOUNT NOT ALLOWED TO ACCESS LATE CHARGES:
           IF (SP-LCFRMLA NOT = " ")
                      AND (
                            (LP-IBPC = "P" OR LN-LOANTYPE = "I")
                                                AND LN-JDDATE = 0
                          )
              MOVE LP-PAYDATE TO NUM-DATE
              MOVE LP-TRCD    TO TEMP-TRCD
              PERFORM CHECK-FOR-AL
              IF ROUTE-CD = "7"
                 MOVE "MUST DO AN 'AL' FOR DATE OF ALLOWABLE DELQ"
                    TO MESS
                 PERFORM SEND-MESS
                 GO TO INITIALIZE-EXIT.
           MOVE TEMP-TRCD TO LP-TRCD.

      * TEST FOR LCPDTH FALLING BEHIND PDTH:
           MOVE LN-1STPYDATE TO PDTH-DATE-WORK.
           PERFORM PAID-THRU-CALCULATION.
           IF PDTH-DATE-FULL > LP-LCPDTH-DATE
              MOVE PDTH-DATE-FULL TO LP-LCPDTH-DATE
                                     LCAP-LCPDTH-DATE.

           GO TO INITIALIZE-SEND-PMPT.

       INITIALIZE-SP.

      * MOVED REDEFINITION OF LP-REFNO TO LPWSLP COPY MEMBER
           MOVE LP-REFNO TO LPWSLP-LP-REFNO.

           IF LPWSLP-LP-POOLID = ZERO
              IF LN-DLPOOL = ZERO
                 MOVE "LOAN NOT ASSIGNED TO ANY POOL" TO MESS
                 PERFORM SEND-MESS
                 MOVE "7" TO ROUTE-CD
                 GO TO INITIALIZE-EXIT
              ELSE
                 MOVE LN-DLPOOL TO LP-PRIOR-POOLID.
           IF LPWSLP-LP-POOLID NOT = ZERO
              IF LN-DLPOOL NOT = ZERO
                 MOVE "LOAN ALREADY ASSIGNED TO A POOL" TO MESS
                 PERFORM SEND-MESS
                 MOVE "8" TO ROUTE-CD
                 GO TO INITIALIZE-EXIT.
           IF LPWSLP-LP-POOLID NOT = ZERO
              MOVE "PL"             TO CD-TYPE
              MOVE LPWSLP-LP-POOLID TO CD-PL-POOLID
              MOVE SPACES           TO CD-PL-FILLER
              PERFORM OPEN-CD1-FILE
              PERFORM READ-CD1-FILE
              PERFORM CLOSE-CD1-FILE
              IF IO-FG NOT = 0
                  MOVE "POOL IS NOT FOUND IN POOL FILE" TO MESS
                  PERFORM SEND-MESS
                  MOVE "8" TO ROUTE-CD
                  GO TO INITIALIZE-EXIT.

       INITIALIZE-SEND-PMPT.
           MOVE "S  A060PMPT2." TO VDU.
           PERFORM SEND-INT-OR-LC.
           MOVE "S  A060PMPT4." TO VDU.
           PERFORM SEND-INT-OR-LC.

       INITIALIZE-EXIT.
           EXIT.

      *****************************************************************
       SET-REPO-AND-STATUS SECTION.
      *****************************************************************

      * 'SX' OR 'ZX' OR 'SV' SET TO REPO ONHAND/SET REPO REDEEMED
           IF LP-TRCD ="SX" OR "ZX"
              MOVE "X" TO LN-REPOCD
              MOVE LP-TRDATE TO LN-REPODATE
              IF GP-REPODATE-PAYDATE = "Y"
                 MOVE LP-PAYDATE TO LN-REPODATE
              END-IF
           ELSE
           IF LP-TRCD ="SV"
              MOVE "V" TO LN-REPOCD
              MOVE LP-TRDATE TO LN-REDEEMDATE
              IF GP-REPODATE-PAYDATE = "Y"
                 MOVE LP-PAYDATE TO LN-REDEEMDATE.

      ************************************************
       AMOUNT-ENTRY-ROUTINE SECTION.
      ************************************************
           MOVE " " TO ROUTE-CD.

       TRANS-AMT-ENTRY.
           IF LN-STATE-WORKER NOT = 0
              SUBTRACT LN-STATE-WORKER2 FROM LN-STATE-WORKER
                                          GIVING PL-NEWAR-MESS-AMT
              MOVE PL-NEWAR-MESS TO MESS
              PERFORM SEND-MESS.

           MOVE "F1, ENTER THE AMOUNTT OF P&L TO SHIFT TO NEW AR:"
                                                         TO MESS.
           PERFORM SEND-LEGEND.
           MOVE "ENNS052040." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" OR "1U"
              PERFORM ACCEPT-SCREEN-RESET
              PERFORM DISPLAY-ENTRY-UP
              MOVE "7" TO ROUTE-CD
              GO TO AMOUNT-ENTRY-EXIT.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO TRANS-AMT-ENTRY.

           MOVE VDUNUM2 TO LP-TRAMT.

           IF LP-TRAMT = 0
              GO TO TRANS-AMT-ENTRY.

           IF LP-TRAMT > 0
              IF (LP-TRAMT + LN-STATE-WORKER) > LN-LNAMT
              MOVE
              "AMOUNT WILL CAUSE 'NEW AR' TO BE > THAN LOAN AMOUNT"
                                                     TO MESS
              PERFORM SEND-MESS
              GO TO TRANS-AMT-ENTRY.

           IF LP-TRAMT < 0
              IF (LP-TRAMT + LN-STATE-WORKER) < 0
              MOVE "REDUCTION WILL CAUSE 'NEW AR' TO GO NEGATIVE"
                                                     TO MESS
              PERFORM SEND-MESS
              GO TO TRANS-AMT-ENTRY.

           IF LP-TRAMT < 0
              IF (LP-TRAMT + LN-STATE-WORKER) < LN-STATE-WORKER2
              MOVE
              "REDUCTION WILL CAUSE 'NEW AR' TO GO < AMT COLLECTED"
                                                     TO MESS
              PERFORM SEND-MESS
              GO TO TRANS-AMT-ENTRY.

       AMOUNT-ENTRY-EXIT.
           EXIT.

      ***************************************************************
       DISBURSE-ESCROW-ROUTINE SECTION.
      ***************************************************************

      * DISBURSE ESCROW ROUTINE - ENTER AMT & DISBURSEMENT FLAG
           MOVE " " TO ROUTE-CD.

       ESCROW-AMT-ENTRY.
           MOVE "F1, ENTER THE AMOUNT OF ESCROW DISBURSEMENT:"
                                                         TO MESS.
           PERFORM SEND-LEGEND.
           MOVE "ENNS052040." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" OR "1U"
              PERFORM ACCEPT-SCREEN-RESET
              PERFORM DISPLAY-ENTRY-UP
              MOVE "7" TO ROUTE-CD
              GO TO ESCROW-ENTRY-EXIT.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO ESCROW-AMT-ENTRY.

           MOVE VDUNUM2 TO LP-TRAMT.

           IF LP-TRAMT = 0
              GO TO ESCROW-AMT-ENTRY.

           IF LP-TRAMT < 0
              MOVE "NEGATIVE DISBUSEMENT NOT ALLOWED." TO MESS
              PERFORM SEND-MESS
              GO TO ESCROW-AMT-ENTRY.

           IF LP-TRAMT > ES-BALANCE
              MOVE
             "DISBURSEMENT EXCEEDS ESCROW BALANCE OF &&&&&&&&&&&&"
                 TO MESS
              MOVE ES-BALANCE TO D-ESCROW-BAL
              INSPECT MESS REPLACING FIRST "&&&&&&&&&&&&"
                                         BY D-ESCROW-BAL
              PERFORM SEND-MESS
              MOVE ED-SEQ TO PASSWDW-SEQ
              MOVE "TO ALLOW NEGATIVE ESCROW BALANCE:"
                                      TO PASSWDW-PROMPT2
              PERFORM PASSWORD-MUST-EXIST
              IF PASSWDW-STAT NOT = "OK"
                 GO TO ESCROW-AMT-ENTRY.

       ESCROW-ENTRY-EXIT.
           EXIT.

      ***************************************************************
       ESCROW-PREPAID-ROUTINE SECTION.
      ***************************************************************

      * ESCROW PREPAID ROUTINE - ENTER AMT

           MOVE " " TO ROUTE-CD.

       PREPAID-AMT-ENTRY.
           MOVE "F1, ENTER THE AMOUNT OF ESCROW PREPAYMENT:"
                                                         TO MESS.
           PERFORM SEND-LEGEND.
           MOVE "ENNS052040." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" OR "1U"
              PERFORM ACCEPT-SCREEN-RESET
              PERFORM DISPLAY-ENTRY-UP
              MOVE "7" TO ROUTE-CD
              GO TO PREPAID-ENTRY-EXIT.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO PREPAID-AMT-ENTRY.

           MOVE VDUNUM2 TO LP-TRAMT.

           IF LP-TRAMT = 0
              GO TO PREPAID-AMT-ENTRY.

           IF LP-TRAMT < 0
              MOVE "NEGATIVE PREPAYMENT NOT ALLOWED." TO MESS
              PERFORM SEND-MESS
              GO TO PREPAID-AMT-ENTRY.

       PREPAID-ENTRY-EXIT.
           EXIT.

      *****************************************
       DISPLAY-ENTRY-LINE SECTION.
      *****************************************

      * COMPUTE PAID THRU DATE
           MOVE LN-1STPYDATE TO PDTH-DATE-WORK.
           PERFORM PAID-THRU-CALCULATION.
           MOVE PDTH-DATE-FULL TO LP-PDTH-DATE.

           MOVE 0 TO LP-DLPROC.

           PERFORM DISPLAY-EARNINGS.
           PERFORM SETUP-LINE.
           PERFORM SEND-ENTRY-LINE.

       DISPLAY-ENTRY-LN-EXIT.
           EXIT.

      *************************************
       ACCEPT-ENTRY-ROUTINE SECTION.
      *************************************
       ACCEPT-ENTRY-RTN.

      * WE REALLY NEED TO TEST THE PASSWORD
      * DRAWER FLAG OF THE POSTING BRANCH IF DOING INTERBRANCH,
      * SO READ POSTING BRANCH THEN RESTORE OWNING BRANCH:

           IF PATH-MACHINE NOT = " "
              MOVE BRANCH TO BR-NO
              PERFORM READ-BR-FILE
              MOVE BR-PASSWORD-DRAWER TO SV-PASSWORD-DRAWER
              MOVE LN-OWNBR TO BR-NO
              PERFORM READ-BR-FILE
           ELSE
              MOVE BR-PASSWORD-DRAWER TO SV-PASSWORD-DRAWER.

           MOVE "F1, F7-GL, DRAWER NO:" TO MESS.
           IF SV-PASSWORD-DRAWER = "Y"
              MOVE "F1, F7-GL, DRAWER PASSWORD:" TO MESS.
           PERFORM SEND-LEGEND.

           MOVE 0 TO CA-TILLNO.
           MOVE " " TO ROUTE-CD.

           MOVE "S  A010120." TO VDU.
           PERFORM SCREENIO.

       ACCEPT-ENTER-TILLNO.
           MOVE LP-TRDATE TO CA-DATE.
           MOVE BRANCH    TO CA-BRNO.
           MOVE 0         TO CA-TILLNO.
           MOVE CA1-KEY TO HOLD-CA1-KEY.

           IF SV-PASSWORD-DRAWER = "Y"
             IF PATH-MACHINE NOT = SPACES
                MOVE EXT-FILPATH-BASE TO CA-ALL-BASE
                MOVE PATH-MACHINE TO CA-ALL-MACHINE
                MOVE PATH-DATA    TO CA-ALL-DATA
                MOVE CA-ALL-PATH  TO CA-PATH
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
      *         MOVE "S  A010120." TO VDU
      *         MOVE CA-TILLNO TO NUMERIC-TILLNO
      *         PERFORM CONVERT-TILLNO-TO-ALPHA
      *         MOVE TEST-ALPHA-TILLNO TO VDUBUF
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
              IF LP-TRCD = "DC"
                 MOVE HOLD-NEW-1ST-DUE TO DATE-YYYYMMDD
                 PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
                 MOVE DATE-MMDDYY      TO ALP-DATE-EDIT
                 MOVE LN-UNITPER-CD    TO DATER-UNITPER-CD
                 MOVE LN-UNITPER-FREQ  TO DATER-UNITPER-FREQ
                 PERFORM UNITPER-MASK
                 MOVE ALP-DATE-EDIT    TO NEW-1STPYDATE
                 MOVE " "              TO NEW-1STPYATTR
                 MOVE NEW-1STPYDATEX TO WR-BUF
                 MOVE NEW-1STPYLIST TO WR-LIST
                 PERFORM CALL-WRFORM

                 MOVE " "              TO NEW-MDTEATTR
                 MOVE NEW-OLDMDTEDATE TO WR-BUF
                 MOVE NEW-MDTELIST TO WR-LIST
                 PERFORM CALL-WRFORM

                 IF (IBPC-FG = "P" OR LN-LOANTYPE = "I")
                                      AND (SP-LCFRMLA NOT = " ")
                                       AND (LN-ACCELDATE = 0)
                    MOVE LN-LCPDTH-DATE TO NUM-DATE
                                           LCAP-LCPDTH-DATE
                    IF LN-UNITPER-CD = "D" OR "W" OR "S" OR "B" 
                       MOVE NUM-DA TO NUM-YR
                       MOVE NUM-MO TO NUM-DA
                       MOVE NUM-DATE TO D-H17
                    ELSE
                       MOVE NUM-MO TO NUM-DA
                       MOVE NUM-DATE TO D-H17
                    END-IF
                    MOVE D-H17 TO WR-BUF
                    MOVE LCPDTH-LIST TO WR-LIST
                    PERFORM CALL-WRFORM.

           IF VDUSTATUS = "10"
              PERFORM ACCEPT-SCREEN-RESET
              IF LXG-GLNO(1) NOT = 0
                 PERFORM DISPLAY-ENTRY-UP
                 GO TO ACCEPT-ENTRY-EXIT
              ELSE
                 GO TO ACCEPT-ENTRY-EXIT.

           IF LP-CURBAL = 0
              IF LP-INTBAL NOT = 0 OR LP-OTHBAL NOT = 0 OR
                 LP-LCBAL NOT = 0 OR LP-OT2BAL NOT = 0
                 PERFORM MUST-BE-ALL-ZERO
                 GO TO ACCEPT-ENTRY-RTN.

           MOVE LP-APOTH TO GL-WORK.

           IF VDUSTATUS = "1>"
              GO TO ACCEPT-GL-DISBURSE.

           IF VDUSTATUS NOT = "00"
              GO TO ACCEPT-ENTRY-RTN.

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
           GO TO ACCEPT-ENTRY-RTN.
       ACCEPT-ENTRY-CONT.
      *INTERBRANCH
           IF PATH-MACHINE NOT = SPACES
              MOVE EXT-FILPATH-BASE TO CA-ALL-BASE
              MOVE PATH-MACHINE TO CA-ALL-MACHINE
              MOVE PATH-DATA    TO CA-ALL-DATA
              MOVE CA-ALL-PATH  TO CA-PATH
              MOVE "Y" TO CA-PATH-OVERRIDE.

           MOVE LP-TRDATE TO CA-DATE.
           MOVE BRANCH TO CA-BRNO.
           PERFORM OPEN-CA1-FILE.
           PERFORM READ-CA1-FILE.
           PERFORM CLOSE-CA1-FILE.
           IF IO-FG NOT = 0
              MOVE "INVALID CASH DRAWER #" TO MESS
              PERFORM SEND-MESS
              GO TO ACCEPT-ENTRY-RTN.
           IF CA-CLOSED-HHMM NOT = 0
              MOVE "CASH DRAWER CLOSED" TO MESS
              PERFORM SEND-MESS
              GO TO ACCEPT-ENTRY-RTN.
           IF LP-CURBAL = 0
              IF LP-OTHBAL = 0
               IF LP-OT2BAL = 0
                 IF LP-INTBAL = 0
                    IF LP-LCBAL = 0
                       MOVE "^-BACK, ENTER PAYOFF CODE:" TO MESS
                       PERFORM SEND-LEGEND
                       GO TO ACCEPT-PO-CODE.
           GO TO ACCEPT-ENTRY-EXIT.
       ACCEPT-PO-CODE.
      * DO NOT USE "RESET ON" OPTION BECAUSE IT WILL CLEAR CODE FROM SCAN.
           MOVE "ENNA060SEL." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" OR "1U"
              MOVE "SNNA060SEL." TO VDU
              MOVE SPACES TO VDUBUF
              PERFORM SCREENIO
              GO TO ACCEPT-ENTRY-RTN.
           IF VDUSTATUS = "1<"
              PERFORM CODE-SCAN
              MOVE CD-CODE TO HOLD-POCD
              GO TO ACCEPT-PO-CODE.
           IF VDUSTATUS NOT = "00"
              GO TO ACCEPT-PO-CODE.

           MOVE "PO"   TO CD-TYPE.
      * NEED TO LEFT JUSTIFY THE "SEL" FIELD 
           CALL "C$JUSTIFY" USING VDUBUF, "L".
           MOVE VDUBUF TO CD-CODE.

      * CLEAR SEL FIELD AFTER SCAN OR ENTRY.
           MOVE "SNNA060SEL." TO VDU.
           MOVE SPACES TO VDUBUF.
           PERFORM SCREENIO.

           PERFORM OPEN-CD1-FILE.
           PERFORM READ-CD1-FILE.
           PERFORM CLOSE-CD1-FILE.
           IF IO-FG NOT = 0
              MOVE "INVALID CODE" TO MESS
              PERFORM SEND-MESS
              GO TO ACCEPT-PO-CODE.
           MOVE CD-CODE TO HOLD-POCD.
           GO TO ACCEPT-ENTRY-EXIT.
       ACCEPT-SCREEN-RESET.
           MOVE "Y" TO FULL-PAGE-FG.
           MOVE LN-OWNBR  TO LP-BRNO.
           MOVE LN-ACCTNO TO LP-ACCTNO.
           MOVE 99999     TO LP-SEQNO.
           MOVE LP1-KEY TO HOLD2-KEY-UP.
           MOVE "7" TO ROUTE-CD.
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
      * BAH CHANGED TO USE LN-OWNBR FOR INTERBRANCH PAYMENTS:
      *    MOVE BRANCH TO ALLENT-BRNO.
           MOVE LN-OWNBR TO ALLENT-BRNO.
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
           PERFORM SEND-MESS.
       DISBURSE-OTHER-EXIT.
           EXIT.

      **************************************************
       DISPLAY-ENTRY-DOWN SECTION.
      **************************************************
           MOVE SPACES TO LINE-BUF LN-BUF.
           MOVE 0 TO LN-WK.

           MOVE HOLD1-KEY-DOWN TO LP1-KEY.
           IF LP-SEQNO = 99999
              MOVE 9 TO IO-FG
              GO TO DISPLAY-DOWN-EXIT.

           MOVE LP1-KEY TO HOLD2-KEY-UP.

      * ***NOTE*** 'START-LP1-FILE-SCAN' MUST BE USED WHEN FIRST USING
      * 'START-LP1-FILE-NOT-GREATER'; USED WHEN DISPLAYING MULTIPLE
      * PAGES IN AN UP & DOWN FUNCTION; YOU NEED TO ADD ONE TO 
      * LP-SEQNO (IN WBEG) IN ORDER TO GET THE DISPLAY TO WORK
      * LIKE VISION FILES.
      
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
              IF (LP-BRNO NOT = LP-PATH-OWNBR) OR
                  (LP-ACCTNO NOT = LN-ACCTNO)
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

      *****************************************
       DISPLAY-ENTRY-UP SECTION.
      *****************************************
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
           MOVE LP-SEQNO      TO QLP1-WEND-SEQNO.
           MOVE ALL "0"       TO QLP1-WBEG-SEQNO.

           PERFORM START-LP1-FILE-NOT-GREATER.
           MOVE 0 TO ENTRY-FLAG.

       DISPLAY-NEXT-UP.
           IF ENTRY-FLAG = 2
              MOVE 9 TO IO-FG
              GO TO DISPLAY-BYPASS-UP.
           PERFORM READ-LP1-FILE-PREVIOUS.
           IF IO-FG = 0
              IF (LP-BRNO NOT = LP-PATH-OWNBR) OR
                  (LP-ACCTNO NOT = LN-ACCTNO)
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
              IF LN-UNITPER-CD = "D" OR "W" OR "S" OR "B"
                 MOVE NUM-DA TO NUM-YR
                 MOVE NUM-MO TO NUM-DA
                 MOVE NUM-DATE TO EB-080
              ELSE
                 MOVE NUM-MO TO NUM-DA
                 MOVE NUM-DATE TO EB-080
              END-IF
              ADD LP-OTHBAL LP-OT2BAL GIVING EB-090
              ADD LP-INTBAL LP-LCBAL GIVING EB-100
              MOVE LP-CURBAL TO EB-110
              MOVE EB-080 TO L-080
              ADD LP-OTHBAL LP-OT2BAL GIVING L-090
              ADD LP-INTBAL LP-LCBAL GIVING L-100
              MOVE LP-CURBAL TO L-110
              IF (LP-TRCD = "RV" OR LP-REV NOT = " ")
                    AND NOT (LP-TRCD = "RV" AND LP-REVSEQNO=0)
                 MOVE "*" TO L-120 EB-120.

       END-SETUP-LINE.
           EXIT.

      ***************************************************
       VERIFY-PASSWORD SECTION.
      ***************************************************
           MOVE " " TO PASSWDW-STAT.
           MOVE "LONPFE" TO PASSWDW-PGM.
           MOVE "PLEASE ENTER THE PASSWORD," TO PASSWDW-PROMPT1.
           MOVE "CL/PASSWD" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH PASSWDW-BUF.
           CANCEL FORM-PROGX.

      ***************************************************
      *   LIKE VERIFY-PASSWORD BUT WILL GIVE BAD STATUS
      *   IF PASSWORD NOT SETUP.  IF PASSWORD ON FILE,
      *   WILL BEHAVE JUST LIKE VERIFY
      ***************************************************
       PASSWORD-MUST-EXIST SECTION.
      ***************************************************
           MOVE "EX" TO PASSWDW-STAT.
           MOVE "LONPFE" TO PASSWDW-PGM.
           MOVE "PLEASE ENTER THE PASSWORD," TO PASSWDW-PROMPT1.
           MOVE "CL/PASSWD" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH PASSWDW-BUF.
           CANCEL FORM-PROGX.

      ***************************************************
       CODE-SCAN SECTION.
      ***************************************************
           MOVE "PO"   TO CDSCAN-TYPE.
           MOVE SPACES TO CDSCAN-CODE.

           MOVE "WI/CDSCAN" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH CDSCAN-BUF EXIT-PATHNAME.
           CANCEL FORM-PROGX.
           IF CDSCAN-STATUS NOT = "00" OR CDSCAN-CODE = SPACES
               MOVE 9 TO IO-FG
               GO TO EXIT-CODE-SCAN.

           MOVE "SNNA060SEL." TO VDU.
           MOVE CDSCAN-CODE TO VDUBUF.
           PERFORM SCREENIO.

           MOVE CDSCAN-CODE TO VDUBUF CD-CODE.
           MOVE 0 TO IO-FG.

       EXIT-CODE-SCAN.
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
      *    MOVE LN-REC  TO WK-SAVE-RECORD.
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
           MOVE WK-SAVE-RECORD TO LN-REC.
           MOVE WK-LP-REC      TO LP-REC.
           MOVE WK-LX-REC      TO LX-REC.
           MOVE WK-LXE-REC     TO LXE-REC.
           MOVE WK-LXG-REC     TO LXG-REC.
           MOVE 0              TO WK-ELE.
           PERFORM REWRITE-WK-FILE.
       EXIT-RESTORE-SCREEN.
           PERFORM CLOSE-WK-FILE.

      *************************************
       CHECK-FOR-AL SECTION.
      *************************************
           MOVE LP-PAYDATE   TO LCAP-PAYDATE.
           MOVE LN-1STPYDATE TO LCAP-1STPYDATE.
           MOVE "AL"         TO LCAP-LPTRCD.
           MOVE LP-APLC      TO LCAP-LPAPLC.
           MOVE LP-APINT     TO LCAP-LPAPINT.
           PERFORM LATE-CHARGE-APPLY.
           IF LCAP-LCPDTH-DATE NOT = LN-LCPDTH-DATE
              MOVE "7" TO ROUTE-CD.

       EXIT-CHECK-FOR-AL.
           EXIT.

      ***************************************************
       CALL-REPO-WINDOW SECTION.
      ***************************************************
           MOVE LP-BRNO   TO EXT-WI-CL-OWNBR.
           MOVE LP-ACCTNO TO EXT-WI-CL-ACCTNO.
           MOVE "WI/CHREPO" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH EXIT-PATHNAME.
           CANCEL FORM-PROGX.

       COPY "LIBGB/CAPASSWD.CPY".
       COPY "LIBGB/GPENV.CPY".

      **************************************************************************
       SETUP-LINK-INFO SECTION.
      **************************************************************************
           IF VDU-FORM-NAME = "LPSMNU"
              MOVE LPSMNU-HELP--FILE TO HELP-LINK-FILE
              MOVE LPSMNU-HELP--DIR  TO HELP-LINK-DIR
           ELSE
              MOVE LONPF-HELP--FILE TO HELP-LINK-FILE
              MOVE LONPF-HELP--DIR TO HELP-LINK-DIR.

      **************************************************************************
       VDU-CONVERT-FIELD SECTION.
      **************************************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBLP/LONPF_EVA.CPY".
              COPY "LIBLP/LPSMNU_EVA.CPY".
              COPY "LIBLP/LPLNMU_EVA.CPY".
              COPY "LIBLP/LPCAMU_EVA.CPY".
              COPY "LIBLP/LPVDMU_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      *************************************
       MISCELLANEOUS-ROUTINES SECTION.
      *************************************
       COPY "LIBGB/SYSTEM.CPY".
       COPY "LIBGB/GETENV.CPY".

       FORM-RESET.

       DISPLAY-EARNINGS.
           IF HOLD-DISPLAY-ERN = "A"
              ADD HOLD-LN-ACCERN(1) HOLD-LN-ACCERN(2)
                  HOLD-LN-ACCERN(3)
                  HOLD-LN-ACCERN(5) GIVING TOTEARN
              IF NOT (GP-INCL-OTH-IN-EARNED-N  OR
                                     GP-INCL-OTH-IN-EARNED-R)
                 ADD HOLD-LN-ACCERN(6) TO TOTEARN
              END-IF
              IF NOT (GP-INCL-OTH-IN-EARNED-R)
                 ADD HOLD-LN-ACCERN(4) TO TOTEARN
              END-IF
              MOVE TOTEARN TO D-TOTEARN
           ELSE
           IF HOLD-DISPLAY-ERN = "U" AND
              LN-SERVACCT NOT = "Y" AND
              LN-PLCD = " "
              COMPUTE UNEARNED =
                 (LN-ANTICERN(1) - HOLD-LN-ACCERN(1)) +
                 (LN-ANTICERN(2) - HOLD-LN-ACCERN(2)) +
                 (LN-ANTICERN(3) - HOLD-LN-ACCERN(3)) +
                 (LN-ANTICERN(5) - HOLD-LN-ACCERN(5))
              IF NOT (GP-INCL-OTH-IN-EARNED-N OR
                                    GP-INCL-OTH-IN-EARNED-R)
                 COMPUTE UNEARNED = UNEARNED
                         + (LN-ANTICERN(6) - HOLD-LN-ACCERN(6))
              END-IF
              IF NOT (GP-INCL-OTH-IN-EARNED-R)
                 COMPUTE UNEARNED = UNEARNED
                         + (LN-ANTICERN(4) - HOLD-LN-ACCERN(4))
              END-IF
              MOVE UNEARNED TO D-TOTEARN
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

           MOVE LN-YTDINTPD TO D-INTYTD.
           IF (IBPC-FG = "P" OR LN-LOANTYPE = "I")
                               AND (SP-LCFRMLA NOT = " ")
                                AND (LN-ACCELDATE = 0)
              MOVE LCAP-LCPDTH-DATE TO NUM-DATE
              IF LN-UNITPER-CD = "D" OR "W" OR "S" OR "B"
                 MOVE NUM-DA TO NUM-YR
                 MOVE NUM-MO TO NUM-DA
                 MOVE NUM-DATE TO D-LCPDTH
              ELSE
                 MOVE NUM-MO TO NUM-DA
                 MOVE NUM-DATE TO D-LCPDTH.
           MOVE LN-TOTNODEF TO D-DEFMNTS.
           MOVE LN-TOTNOALDEL TO D-ALDEL.
      * DUMMY MOVE. NO EARNINGS CALCED:
           MOVE LP-EARNED(1) TO D-EARNED.
           MOVE LP-DLPROC TO D-DLPROC.
           MOVE EARN-BUF TO WR-BUF.
           MOVE EARN-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.
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
       DISPLAY-ALL-LINES.
           MOVE LINE-BUF TO WR-BUF.
           MOVE LINE-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.
       SEND-LEGEND.
           MOVE "S  A000MSEL." TO VDU.
           MOVE MESS TO VDUBUF.
           PERFORM SCREENIO.
       MUST-BE-ALL-ZERO.
           MOVE "BRING OTHBAL, INTBAL & LCBAL TO 0 FIRST!" TO MESS.
           PERFORM SEND-MESS.
       EXCEEDS-PRIN-BAL.
           MOVE "EXCEEDS PRINCIPAL BAL" TO MESS.
           PERFORM SEND-MESS.
       SEND-ENTRY-LINE.
           MOVE ENTRY-BUF TO WR-BUF.
           MOVE ENTRY-LINE TO WR-LIST.
           PERFORM CALL-WRFORM.
      *CONVERT-TILLNO-TO-NUMERIC.
      *    MOVE 0 TO NUMERIC-TILLNO.
      *    IF TEST-ALPHA-TILLNO = "1"
      *       MOVE 1 TO NUMERIC-TILLNO
      *    ELSE
      *    IF TEST-ALPHA-TILLNO = "2"
      *       MOVE 2 TO NUMERIC-TILLNO
      *    ELSE
      *    IF TEST-ALPHA-TILLNO = "3"
      *       MOVE 3 TO NUMERIC-TILLNO
      *    ELSE
      *    IF TEST-ALPHA-TILLNO = "4"
      *       MOVE 4 TO NUMERIC-TILLNO
      *    ELSE
      *    IF TEST-ALPHA-TILLNO = "5"
      *       MOVE 5 TO NUMERIC-TILLNO
      *    ELSE
      *    IF TEST-ALPHA-TILLNO = "6"
      *       MOVE 6 TO NUMERIC-TILLNO
      *    ELSE
      *    IF TEST-ALPHA-TILLNO = "7"
      *       MOVE 7 TO NUMERIC-TILLNO
      *    ELSE
      *    IF TEST-ALPHA-TILLNO = "8"
      *       MOVE 8 TO NUMERIC-TILLNO
      *    ELSE
      *    IF TEST-ALPHA-TILLNO = "9"
      *       MOVE 9 TO NUMERIC-TILLNO
      *    ELSE
      *    IF TEST-ALPHA-TILLNO = "A"
      *       MOVE 10 TO NUMERIC-TILLNO
      *    ELSE
      *    IF TEST-ALPHA-TILLNO = "B"
      *       MOVE 11 TO NUMERIC-TILLNO
      *    ELSE
      *    IF TEST-ALPHA-TILLNO = "C"
      *       MOVE 12 TO NUMERIC-TILLNO
      *    ELSE
      *    IF TEST-ALPHA-TILLNO = "D"
      *       MOVE 13 TO NUMERIC-TILLNO
      *    ELSE
      *    IF TEST-ALPHA-TILLNO = "E"
      *       MOVE 14 TO NUMERIC-TILLNO
      *    ELSE
      *    IF TEST-ALPHA-TILLNO = "F"
      *       MOVE 15 TO NUMERIC-TILLNO
      *    ELSE
      *    IF TEST-ALPHA-TILLNO = "G"
      *       MOVE 16 TO NUMERIC-TILLNO
      *    ELSE
      *    IF TEST-ALPHA-TILLNO = "H"
      *       MOVE 17 TO NUMERIC-TILLNO
      *    ELSE
      *    IF TEST-ALPHA-TILLNO = "I"
      *       MOVE 18 TO NUMERIC-TILLNO
      *    ELSE
      *    IF TEST-ALPHA-TILLNO = "J"
      *       MOVE 19 TO NUMERIC-TILLNO
      *    ELSE
      *    IF TEST-ALPHA-TILLNO = "K"
      *       MOVE 20 TO NUMERIC-TILLNO
      *    ELSE
      *    IF TEST-ALPHA-TILLNO = "L"
      *       MOVE 21 TO NUMERIC-TILLNO
      *    ELSE
      *    IF TEST-ALPHA-TILLNO = "M"
      *       MOVE 22 TO NUMERIC-TILLNO
      *    ELSE
      *    IF TEST-ALPHA-TILLNO = "N"
      *       MOVE 23 TO NUMERIC-TILLNO
      *    ELSE
      *    IF TEST-ALPHA-TILLNO = "O"
      *       MOVE 24 TO NUMERIC-TILLNO
      *    ELSE
      *    IF TEST-ALPHA-TILLNO = "P"
      *       MOVE 25 TO NUMERIC-TILLNO
      *    ELSE
      *    IF TEST-ALPHA-TILLNO = "Q"
      *       MOVE 26 TO NUMERIC-TILLNO
      *    ELSE
      *    IF TEST-ALPHA-TILLNO = "R"
      *       MOVE 27 TO NUMERIC-TILLNO
      *    ELSE
      *    IF TEST-ALPHA-TILLNO = "S"
      *       MOVE 28 TO NUMERIC-TILLNO
      *    ELSE
      *    IF TEST-ALPHA-TILLNO = "T"
      *       MOVE 29 TO NUMERIC-TILLNO
      *    ELSE
      *    IF TEST-ALPHA-TILLNO = "U"
      *       MOVE 30 TO NUMERIC-TILLNO
      *    ELSE
      *    IF TEST-ALPHA-TILLNO = "V"
      *       MOVE 31 TO NUMERIC-TILLNO
      *    ELSE
      *    IF TEST-ALPHA-TILLNO = "W"
      *       MOVE 32 TO NUMERIC-TILLNO
      *    ELSE
      *    IF TEST-ALPHA-TILLNO = "X"
      *       MOVE 33 TO NUMERIC-TILLNO
      *    ELSE
      *    IF TEST-ALPHA-TILLNO = "Y"
      *       MOVE 34 TO NUMERIC-TILLNO
      *    ELSE
      *    IF TEST-ALPHA-TILLNO = "Z"
      *       MOVE 35 TO NUMERIC-TILLNO.
      *CONVERT-TILLNO-TO-ALPHA.
      *    MOVE SPACES TO TEST-ALPHA-TILLNO.
      *    IF NUMERIC-TILLNO < 10
      *       MOVE NUMERIC-TILLNO1 TO TEST-ALPHA-TILLNO
      *    ELSE
      *    IF NUMERIC-TILLNO = 10
      *       MOVE "A" TO TEST-ALPHA-TILLNO
      *    ELSE
      *    IF NUMERIC-TILLNO = 11
      *       MOVE "B" TO TEST-ALPHA-TILLNO
      *    ELSE
      *    IF NUMERIC-TILLNO = 12
      *       MOVE "C" TO TEST-ALPHA-TILLNO
      *    ELSE
      *    IF NUMERIC-TILLNO = 13
      *       MOVE "D" TO TEST-ALPHA-TILLNO
      *    ELSE
      *    IF NUMERIC-TILLNO = 14
      *       MOVE "E" TO TEST-ALPHA-TILLNO
      *    ELSE
      *    IF NUMERIC-TILLNO = 15
      *       MOVE "F" TO TEST-ALPHA-TILLNO
      *    ELSE
      *    IF NUMERIC-TILLNO = 16
      *       MOVE "G" TO TEST-ALPHA-TILLNO
      *    ELSE
      *    IF NUMERIC-TILLNO = 17
      *       MOVE "H" TO TEST-ALPHA-TILLNO
      *    ELSE
      *    IF NUMERIC-TILLNO = 18
      *       MOVE "I" TO TEST-ALPHA-TILLNO
      *    ELSE
      *    IF NUMERIC-TILLNO = 19
      *       MOVE "J" TO TEST-ALPHA-TILLNO
      *    ELSE
      *    IF NUMERIC-TILLNO = 20
      *       MOVE "K" TO TEST-ALPHA-TILLNO
      *    ELSE
      *    IF NUMERIC-TILLNO = 21
      *       MOVE "L" TO TEST-ALPHA-TILLNO
      *    ELSE
      *    IF NUMERIC-TILLNO = 22
      *       MOVE "M" TO TEST-ALPHA-TILLNO
      *    ELSE
      *    IF NUMERIC-TILLNO = 23
      *       MOVE "N" TO TEST-ALPHA-TILLNO
      *    ELSE
      *    IF NUMERIC-TILLNO = 24
      *       MOVE "O" TO TEST-ALPHA-TILLNO
      *    ELSE
      *    IF NUMERIC-TILLNO = 25
      *       MOVE "P" TO TEST-ALPHA-TILLNO
      *    ELSE
      *    IF NUMERIC-TILLNO = 26
      *       MOVE "Q" TO TEST-ALPHA-TILLNO
      *    ELSE
      *    IF NUMERIC-TILLNO = 27
      *       MOVE "R" TO TEST-ALPHA-TILLNO
      *    ELSE
      *    IF NUMERIC-TILLNO = 28
      *       MOVE "S" TO TEST-ALPHA-TILLNO
      *    ELSE
      *    IF NUMERIC-TILLNO = 29
      *       MOVE "T" TO TEST-ALPHA-TILLNO
      *    ELSE
      *    IF NUMERIC-TILLNO = 30
      *       MOVE "U" TO TEST-ALPHA-TILLNO
      *    ELSE
      *    IF NUMERIC-TILLNO = 31
      *       MOVE "V" TO TEST-ALPHA-TILLNO
      *    ELSE
      *    IF NUMERIC-TILLNO = 32
      *       MOVE "W" TO TEST-ALPHA-TILLNO
      *    ELSE
      *    IF NUMERIC-TILLNO = 33
      *       MOVE "X" TO TEST-ALPHA-TILLNO
      *    ELSE
      *    IF NUMERIC-TILLNO = 34
      *       MOVE "Y" TO TEST-ALPHA-TILLNO
      *    ELSE
      *    IF NUMERIC-TILLNO = 35
      *       MOVE "Z" TO TEST-ALPHA-TILLNO.

       COPY "LIBGB/DATER.CPY".
      *COPY "LIBLP/SETCTBL.CPY".
       COPY "LIBLP/LPUPWK.CPY".
       COPY "LIBLP/LPLCAP.CPY".
       COPY "LIBLP/LPMDTE.CPY".
       COPY "LIBLP/LPLCAS.CPY".
       COPY "LIBLP/LPSCHD.CPY".
       COPY "LIBLP/LPLCHG.CPY".
       COPY "LIBLP/LPIBPC.CPY".
       COPY "LIBLP/LPXDTE.CPY".
       COPY "LIBGB/SPPDTH.CPY".
       COPY "LIBGB/SCREEN.CPY".

      ********************************************
       IO-ROUTINES SECTION.
      ********************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBLP/LPSPGS_SQL.CPY".
       COPY "LIBLP/LPLXGGS_SQL.CPY".
       COPY "LIBLP/LPLXEGS_SQL.CPY".
       COPY "LIBLP/LPLXGS_SQL.CPY".
       COPY "LIBLP/LPLTPGS_SQL.CPY".
       COPY "LIBLP/LPLPGS_SQL.CPY".
       COPY "LIBLP/LPESGS_SQL.CPY".
       COPY "LIBLP/LPCDGS_SQL.CPY".
       COPY "LIBLP/LPCAGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBGBGS_SQL.CPY".
       COPY "LIBLP/LPLP1RN.CPY".
       COPY "LIBLP/LPSP1RN.CPY".
       COPY "LIBLP/LPCA1RN.CPY".
       COPY "LIBLP/LPCD1RN.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBGB/GBGBRN.CPY".
       COPY "LIBLP/LPWKI.CPY".
       COPY "LIBLP/LPLTP1RN.CPY".
       COPY "LIBLP/LPES1RN.CPY".

       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO ERRCD.
       END-ROUTINE.
           PERFORM CLOSE-FILES.
       EXIT-PROG.
       STOP-RUN.
           EXIT PROGRAM.
