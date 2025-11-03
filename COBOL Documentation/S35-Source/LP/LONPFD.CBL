       IDENTIFICATION DIVISION.
       PROGRAM-ID.      LONPFD.
       DATE-WRITTEN.    031386.
      **************************************************************************
      *  DESC:  LOAN PROCESSING
      *         TRANSACTION POSTING
      *         PROGRAM 13 -     TRCD'S  TS,TP
      * REV:
      *  053095 SLC COMBINED OTHBAL AND OT2BAL IN L-090
      *  060695 BLL CHANGED TO USE NEW GL ENTRY ROUTINE
      *  MJD 030596 ADDED PAYOFF CODE SCAN
      *  MJD 071196 ADDED TEST OF LN-UNITPER-CD FOR WEEKLY PDTH.
      *  JTG 101196 ADDED 'LXE-WORKER' & 'LXE-WORKER2' FOR LPCERN-- A50 VERSION
      *                   SP-ERNFRMLA 15 & 16, REAL INTEREST BEARINGING METHOD
      *  JTG 051297 ADDED USE OF 'SETCTBL' AND 'SETCTBLW'
      *  BLV 072497 DON'T ALLOW ENTRY OF DLRVPRIN1, 2 IF GP FLAGS PROHIBIT
      *  GLF 090897 REPLACED 'IO' ON GP-FILE WITH GET 'GPENV'
      *  GLF 101497 ALLOWED CTRL 'N'
      *  MJD 101797 MOVED WK-UPDATE-BUF TO LP01WK
      *  BLV 110397 MOVE IN BR-GL-TYPE-ALLOWED, BR-GL-ENTRY-DEFAULT TO
      *             ALLENT-BUF BEFORE GL DISTRIBUTION WINDOW
      *  BAH 062698 TILLNO IS NOW ALPHANUMERIC
      *  JTG 071098 CHANGED COBOL NAME FROM HOLD-LN-DLSCHG TO HOLD-LN-ANTIC-DISC
      *  BAH 073098 DISPLAY BALANCES EVEN ON REVERSALS, #487
      *  MJD 083398 ADDED ENVIRONMENT VARIABLE TO DETERMINE WHETHER OR NOT
      *             TO DISPLAY BALANCES ON REVERSALS. FIXED DISPLAY TO INDICATE
      *             INCLUDED O2 BAL WITH "SC" "RN" "PB" "RB" "RO" TRANSACTIONS
      *  JTG 100798 REMOVED DISPLAY OF ANTIC. INTEREST ON 'IB' ACCOUNTS
      *  JTG 101298 ALLOWED DISPLAY OF NEGATIVE CURBAL REVOL REGENCY
      *  JTG 990519 STOPPED REFUND OF INTEREST IF IB FOR "TS"
      *  GLB 990720 ADD CASH DRAWER PASSWORD OPTION TO ENTER DRAWER NO.
      *             BR-PASSWORD-DRAWER(Y/N) & CA-PASSWORD (4 CHAR PASSWORD)
      *             REGMGM PR# 901
      *  MJD 990806 ADDED GP-INCL-OTH-IN-EARNED FLAG. IF "Y" THEN INCLUDE
      *             EARNINGS BUCKET 6 (OTHER) IN ACCUMULATED TOTALS
      *             OTHERWISE EXCLUDE BUCKET 6.
      *  MG  010201 REMOVED "01 WK-LXE" AND ADDED LIBLP/LPWKLXE.
      *  CS  080204 IF GP-I-AM-MIDATL ALLOW INPUT OF RSVPRIN3 & REMOVE MAINT FEE
      *              INCREASED TP-BUF SO SEND LONPFD, LONPFZ, LONPF8
      *                                                    MIDATL PR#3136
      *  MJD 080228 TESTING UP FRONT DEALER DISCOUNT - IN PROGRESS
      *   CS 081113 ADD TEST GP-INCL-OTH-IN-EARNED = 'R'. IF 'R' EXCLUDE
      *             OTHER(ANTICERN(6) AND MAINTFEE(ANTICERN(4) FROM EARNINGS
      *                                                     REGACC  PR#210
      *   CS 090413 ALLOW UNITED ACCEPTANCE(GP-I-AM-UNITED) ACCESS TO
      *             RESERVE PRIN 3  PR#3344  UNITED ACCEPT.
      *  BAH 100224 PASSWORD CHANGING THE CALCULATED ANTICIPATED INTEREST,
      *                                                        UNITED PR# 3549
      *  BAH 130111 ADDED LC-DISPLAY-ERN OPTION OF "I" FOR POFF-INTDUE
      *                                              AMERICAN ACC PR# 4190
      *  BAH 140814 ADDED TEST FOR LPSMNU IN HELP
      *  BLM 141029 FIX ENTRY OF PO CODE, USE C$JUSTIFY INSTEAD OF MESSING WITH
      *             VDUPROCESS.  
      *  BAH 160125 SPLIT OUT LTFILE, PD0003
      *   CS 160222 REMOVE COPY LPWKLXE, ITS AN EARNINGS COPY MEMBER THAT IS
      *             PASSED IN LOAN PROCESSING VIA LP01WK USING WK-LX2-REC. 
      *             REPLACED WK-LX2-REC WITH WK-LXE-REC IN LP01WK 
      *             MOVED LPWKLXE TO LPWKLXE.NO
      *             SAVED VERSION LONPFD.160222
      *  BAH 160304 CHANGED TILLNO TO NUMERIC BUT NO ROOM ON SCREEN FOR 2 DIGITS
      *
      * KEC 2016.0325 {PD#00003} PARADATA <A30> [JAY CISZEWSKI]
      *         CHANGED CDSCAN-WINDOW-BUF TO HAVE MATCHING CD1-KEY
      *         LAYOUTS (FOR EACH CODE TYPE; CD-TYPE).
      *         CDSCAN-CD1-KEY IS THE NEW (PRIMARY) WITH REDEFINES OF
      *         CDSCAN-??-KEY (NEW).
      *   CS 160817 IF GP-I-AM-MIDA DISPLAY RECOURSE CODE, WHEN "P", DISPLAY
      *             PARTIAL AMOUNT FROM LOAN PR#5162
      * BAH 161117 ADDED 2 DIGIT FIELD "DRAWER" ABOVE PAYMENT LINE FOR 
      *            NEW CASH DRAWER ENTRY
      * BAH 170619 ACTIVATE 8 DIGIT DATES, PD0006
      * BAH 171129 FIXED WK-LXE-REC
      * BAH 181231 GLOBAL LPFILE
      * BAH 190116 GLOBAL LXFILE
      * BAH 2022.0314 REMOVED EXT-SPANISH-FORMS AND ALL MX FORMS
      * BAH 20220803 HARD CODED START
      *
      * JKC 2022-0930 CHANGED TO USE START-LP1-FILE-SCAN INSTEAD OF
      *          START-LP1-FILE; NEED TO USE START-LP1-FILE-SCAN WHEN
      *          START-LP1-FILE-NOT-GREATER IS USED FIRST; NEEDED FOR
      *          FOR DISPLAYING MULTIPLE PAGES IN AN UP & DOWN FUNCTION;
      *          NEED TO ADD ONE TO LP-SEQNO (IN WBEG) IN ORDER TO GET 
      *          THE DISPLAY TO WORK LIKE WITH VISION FILES.
      *
      * SMR 2023-0418 MODIFIED FOR CHANGES TO LX FILES.
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
           "@(#)LP/LONPFD.CBL S35 04/19/23-13:04:30 >".
      **************************************************
      *     P R O G R A M   W O R K E R S              *
      **************************************************
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
       COPY "LIBLP/LP01CD.CPY".
       COPY "LIBLP/LP01CD_SQL.CPY".
       COPY "LIBLP/LPWSCD.CPY".
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBLP/LP01LN.CPY".
       COPY "LIBLP/LP01CK.CPY".
       COPY "LIBLP/LP01CK_SQL.CPY".
       COPY "LIBLP/LPWSCK.CPY".
       01  WK-IFN              PIC X(2)     VALUE "WK".
       01  WK-KEY              PIC 9(2)     VALUE 0.

       01  ACTION              VALUE SPACES.
           03  ACTION-1        PIC X(5).
           03  ACTION-2        PIC X.
       01  ELE                 PIC 99                VALUE 0.
       01  TOTEARN             PIC S9(6)V99  COMP-3.
       01  UNEARNED            PIC S9(6)V99  COMP-3.
       01  LN-WK               PIC 9(6)      COMP-3       VALUE 0.
       01  LCAP-LCPDTH-DATE    PIC 9(8)      COMP-3.
       01  GL-WORK             PIC S9(7)V99  COMP-3 VALUE +0.
       01  DISP-REV-BAL        PIC X                VALUE "Y".

       01  REB-FIELD.
           03  REB-DISPLAY   PIC ZZZ,ZZ9.99.
       01  REBATE-WORKERS.
           03  MAINT-REB-A   PIC X(20).
           03  SCHG-REB-A    PIC X(20).
           03  INT-REB-A     PIC X(20).
           03  MAINT-REB     PIC S9(7)V99 COMP-3.
           03  SCHG-REB      PIC S9(7)V99 COMP-3.
           03  INT-REB       PIC S9(7)V99 COMP-3.
           03  MF-FLAG       PIC 9        COMP-3 VALUE 0.
           03  SC-FLAG       PIC 9        COMP-3 VALUE 0.
           03  INT-FLAG      PIC 9        COMP-3 VALUE 0.

       01  SCR-ALL.
           03  SCR-1        PIC X(9).
           03  SCR-2        PIC S9.
           03  SCR-3        PIC XX.
       01  SCRX-ALL REDEFINES SCR-ALL.
           03  SCRX-1        PIC X(8).
           03  SCRX-2        PIC S9.
           03  SCRX-3        PIC XX.

       01  RESET-VDU.
           03  RESET-VDU-1   PIC XX.
           03  RESET-VDU-2   PIC S9.
           03  RESET-VDU-3   PIC X.

       01  SUB                 PIC 9(6) COMP-3       VALUE 0.
       01  SUB1                PIC 9(6) COMP-3       VALUE 0.

       01  PUR-BAL-EXCEEDED.
           03  FILLER          PIC X(20)    VALUE
           "THE PURCHASE BAL IS".
           03  PURBAL-EXCEEDED PIC ZZZZ9.99-.

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
           03  L-040RATE       REDEFINES  L-040
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
           03  FILLER  REDEFINES D-LCPDTH.
               05 D-LCPDTH-MO   PIC 99.
               05 D-LCPDTH-DAYR PIC 99.
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
       01  D-TP-BUF.
           03  D-F73.
               05  D-F73A      PIC X(11).
               05  D-F73B      PIC X(10).
               05  D-F73C      PIC ZZZ,ZZ9.99-.
           03  D-F82           PIC XXX.
           03  D-F83.
               05  D-F83A      PIC X(11).
               05  D-F83B      PIC X(10).
               05  D-F83C      PIC ZZZ,ZZ9.99-.

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
           03  FILLER          PIC X(25) VALUE
           "H09 H10 H17 H21 PROCEEDS.".


       COPY "LIBGB/CAPASSWDW.CPY".
       COPY "LIBWI/CDSCANW.CPY".
       COPY "LIBLP/LPITRMW.CPY".
       COPY "LIBLP/LPWKPATH.CPY".
       COPY "LIBLP/LPIBPCW.CPY".
       COPY "LIBLP/LPXDTEW.CPY".
       COPY "LIBGB/SPPDTHW.CPY".
       COPY "LIBLP/LPMDTEW.CPY".
       COPY "LIBLP/LPBWINW.CPY".
       COPY "LIBLP/LPEADJW.CPY".
       COPY "LIBGL/ALLENTW.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBGB/REBATEW.CPY".
       COPY "LIBLP/LPAMTSW.CPY".
       COPY "LIBLP/LPAPRSW.CPY".
       COPY "LIBLP/SETCTBLW.CPY".
       COPY "LIBLP/LPAPRZW.CPY".
       COPY "LIBLP/LPUPERW.CPY".
       COPY "LIBLP/LPUPWKW.CPY".
       COPY "LIBLP/LPMENUW.CPY".
      *COPY "LIBLP/LPWKLXE.CPY".

       COPY "LIBLP/LPUERNW.CPY".
       COPY "LIBLP/LPCERNW.CPY".
       COPY "LIBLP/LPREGZW.CPY".
       COPY "LIBGB/PASSWDW.CPY".
       78  TP-ANTIC-SEQ              VALUE 1.


       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBCL/CLMENUW_EXT.CPY".
       COPY "LIBGB/SYSTEMW.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/RBACTW.CPY".
       COPY "LIBGB/GPENVW.CPY".

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
       COPY "LIBLP/LPPFBUF.CPY".
       COPY "LIBLP/LPLOANW.CPY".

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
           PERFORM CLOSE-LTI1-FILE.
           PERFORM CLOSE-LP1-FILE.
           PERFORM CLOSE-CK1-FILE.
           PERFORM CLOSE-CD1-FILE.
           PERFORM CLOSE-CA1-FILE.
           PERFORM CLOSE-BR-FILE.
           CLOSE WK-FILE.
       COPY "LIBGB/DECLAR2.CPY".

      **************************************************
      *      P R O G R A M   C O N T R O L             *
      **************************************************
       MAIN-PROGRAM SECTION.

           PERFORM SQL-CONNECT.

           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "LONPF" TO VDU-FORM-NAME.

           MOVE USER-PID TO TEMP-PID.

           IF INTERRUPT-FG = "Y"
              MOVE " " TO VDU-WIMENU-SELECTION
           ELSE
              MOVE "C" TO VDU-WIMENU-SELECTION.
           MOVE "F" TO VDU-CLMENU-SELECTION.

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

           MOVE BRANCH TO BR-NO.
           PERFORM OPEN-BR-FILE.
           PERFORM READ-BR-FILE.
           PERFORM CLOSE-BR-FILE.
           IF IO-FG NOT = 0
              MOVE "INVALID BRANCH RECORD" TO MESS
              PERFORM SEND-MESS
              MOVE "X" TO ERRCD
              GO TO END-ROUTINE.

      * GET GLOBAL PARAMETER RECORD (FROM GPENV):
           PERFORM GET-GPENV.

           MOVE LN-LCPDTH-DATE TO LCAP-LCPDTH-DATE.

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

           GO TO END-ROUTINE.

      *********************************
       SELECTION-MODULE SECTION.
      *********************************

           PERFORM INITIALIZE-ROUTINE.
           IF ROUTE-CD = "7"
              GO TO RET-TO-ENTRY-ROUTINE.

           PERFORM DISPLAY-ENTRY-LINE.
           IF ROUTE-CD = "7"
              GO TO RET-TO-ENTRY-ROUTINE.

           IF LP-TRCD = "TP"
              PERFORM DISBURSE-PURCHASED-AMOUNTS
              IF IO-FG NOT = 0
                 PERFORM ACCEPT-SCREEN-RESET
                 PERFORM DISPLAY-ENTRY-UP
                 GO TO RET-TO-ENTRY-ROUTINE
              END-IF
      ******
      ****** TESTING UP FRONT DEALER DISC MJD 2008-02-28
      ******
      ******
      ******
           MOVE 0 TO UERN
           PERFORM COMPUTE-UERNINGS
           MOVE 1 TO SUB
           PERFORM SET-EARNINGS 6 TIMES.
      ******
      ******
      ******


           PERFORM ACCEPT-ENTRY-ROUTINE.
           IF ROUTE-CD = "7"
              GO TO RET-TO-ENTRY-ROUTINE.

           MOVE "3" TO ROUTE-CD.
           IF LP-TRCD = "TS"
              MOVE 5 TO NEW-PROG-SUB
            ELSE
              IF LP-TRCD = "TP"
                 MOVE "Z" TO NEW-PROG-SUB.

           GO TO SELECTION-EXIT-PROG.

       RET-TO-ENTRY-ROUTINE.
           MOVE "1" TO NEW-PROG-SUB.
       SELECTION-EXIT-PROG.
           MOVE "P" TO ERRCD.
           PERFORM SAVE-SCREEN.
       SELECTION-MODULE-EXIT.
           EXIT.

      **********************************************
       INITIALIZE-ROUTINE SECTION.
      **********************************************
           MOVE LN-OTHBAL TO LP-OTHBAL.
           MOVE LN-OT2BAL TO LP-OT2BAL.
           MOVE LN-INTBAL TO LP-INTBAL.
           MOVE LN-LCBAL TO LP-LCBAL.
           MOVE LN-CURBAL TO LP-CURBAL.

           MOVE LP-PAYDATE TO IBPC-DATE.
           PERFORM IBPC-TEST.
           MOVE IBPC-FG TO LP-IBPC.

           MOVE "S  A060PMPT2." TO VDU.
           PERFORM SEND-INT-OR-LC.
           MOVE "S  A060PMPT4." TO VDU.
           PERFORM SEND-INT-OR-LC.

           MOVE LN-CURBAL TO LP-INTDUE.

       INITIALIZE-ROUTINE-EXIT.
           EXIT.

      ************************************
       DISPLAY-ENTRY-LINE SECTION.
      ************************************
           SUBTRACT LP-APCUR FROM LP-CURBAL.

           IF LP-CURBAL < 0
              PERFORM EXCEEDS-PRIN-BAL
              MOVE "7" TO ROUTE-CD
              GO TO DISPLAY-ENTRY-LN-EXIT.

           IF LN-SERVACCT = "N"
              IF LP-APCUR > 0
                 IF LP-APCUR > LN-PURBAL
                    PERFORM EXCEEDS-PUR-BAL
                    MOVE "7" TO ROUTE-CD
                    GO TO DISPLAY-ENTRY-LN-EXIT.

      * IF LP-TRCD = "TS":
      * IF ACCOUNT WAS PARTIALLY PURCHASED, TAKE FULL
      *  REMAINDER OF ANTICIPATED EARNINGS. (BIG APPLE)
      * IF ACCOUNT WAS FULL PURCHASE, TAKE EARNINGS
      *  THROUGH TODAY.
           IF LP-TRCD = "TS"
              IF LN-PURBAL NOT = LN-CURBAL
                 PERFORM SET-LP-EARNED
              ELSE
                 PERFORM SET-LP-EARNED.

      * COMPUTE PAID THRU DATE
           MOVE LN-1STPYDATE TO PDTH-DATE-WORK.
           PERFORM PAID-THRU-CALCULATION.
           MOVE PDTH-DATE-FULL TO LP-PDTH-DATE.

           ADD LP-APLC TO HOLD-LN-TOTERN HOLD-LN-TOTLCHG.
           ADD LP-APINT TO HOLD-LN-TOTERN HOLD-LN-TOTINT.

      * COMPUTE INTEREST PAID BY BORROWER
           PERFORM COMPUTE-BW-INTPAID.
           MOVE BWIN-INTPAID TO LP-INTPAID.
           ADD LP-INTPAID TO LN-YTDINTPD.

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
           IF LP-TRCD = "TP"
              MOVE "F1-CANCEL, DRAWER ENTRY   :" TO MESS
           ELSE
              IF LP-TRCD = "TS"
                MOVE "F1-CANCEL, F7-GL ENTRY, DRAWER ENTRY   :" TO MESS.
           IF BR-PASSWORD-DRAWER = "Y"
              INSPECT MESS REPLACING FIRST "ENTRY   :" BY "PASSWORD:"
           ELSE
              INSPECT MESS REPLACING FIRST "ENTRY   :" BY "NUMBER  :".
           PERFORM SEND-LEGEND.

           MOVE 0 TO CA-TILLNO.
           MOVE " " TO ROUTE-CD.

           MOVE "S  A010120." TO VDU.
           PERFORM SCREENIO.

      * - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       ACCEPT-ENTER-TILLNO.
           MOVE LP-TRDATE TO CA-DATE.
           MOVE BRANCH    TO CA-BRNO.
           MOVE 0         TO CA-TILLNO.
           MOVE CA1-KEY TO HOLD-CA1-KEY.
           IF BR-PASSWORD-DRAWER = "Y"
              PERFORM ENTER-CASH-DRAWER-PASSWORD
              IF CAPASSWD-OKAY = "N"
                 GO TO ACCEPT-ENTER-TILLNO
              END-IF
              IF CAPASSWD-OKAY = "V"
                 GO TO ACCEPT-CHECK-VDUSTATUS
              END-IF
              IF CAPASSWD-OKAY = "Y"
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
      * - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF VDUSTATUS = "10"
              PERFORM ACCEPT-SCREEN-RESET
              IF LP-TRCD = "TP" OR LXG-GLNO(1) NOT = 0
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
              IF LP-TRCD NOT = "TP"
                 GO TO ACCEPT-GL-DISBURSE.

           IF VDUSTATUS NOT = "00"
              GO TO ACCEPT-ENTRY-RTN.

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
           MOVE LP-TRDATE TO CA-DATE.
           MOVE BRANCH    TO CA-BRNO.
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
      * DO NOT USE "RESET ON" IT WILL CLEAR CODE FROM SCAN.
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

      * CLEAR SEL FIELD
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

       ACCEPT-ENTRY-EXIT.
           IF ROUTE-CD NOT = " "
              PERFORM CLEAR-LEGEND.

      *****************************************
       ACCEPT-SCREEN-RESET SECTION.
      *****************************************
           MOVE "Y" TO FULL-PAGE-FG.

           MOVE LN-OWNBR  TO LP-BRNO.
           MOVE LN-ACCTNO TO LP-ACCTNO.
           MOVE 99999     TO LP-SEQNO.
           MOVE LP1-KEY   TO HOLD2-KEY-UP.

           MOVE "7" TO ROUTE-CD.


      *******************************************************
       VERIFY-PASSWORD SECTION.
      *******************************************************
           MOVE " " TO PASSWDW-STAT.
           MOVE "LONPFD" TO PASSWDW-PGM.
           MOVE "PLEASE ENTER THE PASSWORD," TO PASSWDW-PROMPT1.
           MOVE "CL/PASSWD" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH PASSWDW-BUF.
           CANCEL FORM-PROGX.

      *******************************************
       DISBURSE-PURCHASED-AMOUNTS SECTION.
      *******************************************
           PERFORM DISBURSE-PUR-DISPLAY.
           MOVE ZEROS TO TP-BUF.
           MOVE SUB1 TO SUB.
       DISBURSE-PUR-RECOURSE.
           MOVE "E  A030F" TO SCRX-1.
           MOVE SUB TO SCRX-2.
           MOVE "2." TO SCRX-3.
           MOVE SCRX-ALL TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10"
              MOVE 9 TO IO-FG
              GO TO DISBURSE-PUR-EXIT.
           IF VDUSTATUS NOT = "00" AND NOT = "1D"
              GO TO DISBURSE-PUR-RECOURSE.
           MOVE VDUBUF TO HOLD-LN-RECOURSE.
           IF HOLD-LN-RECOURSE NOT = "F"
                            AND NOT = "P"
                             AND NOT = "N"
              GO TO DISBURSE-PUR-RECOURSE.
           IF SUB > 3
              COMPUTE RESET-VDU-2 = SUB - 4
           ELSE
              COMPUTE RESET-VDU-2 = SUB + 6.
           IF HOLD-LN-RECOURSE NOT = "P"
              MOVE 0 TO HOLD-LN-PARTRCAMT
              MOVE "GA" TO RESET-VDU-1
              MOVE "." TO RESET-VDU-3
              MOVE RESET-VDU TO MESS
              PERFORM RESET-MESS
              GO TO DISBURSE-PUR-DLRVPRIN-1.
       DISBURSE-PUR-PARTRCAMT.
           MOVE "E  S062GA" TO SCR-1.
           MOVE SUB TO SCR-2.
           IF SCR-2 < 4
              ADD 6 TO SCR-2
           ELSE
              SUBTRACT 4 FROM SCR-2.
           MOVE "." TO  SCR-3.
           MOVE SCR-ALL TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10"
              MOVE 9 TO IO-FG
              GO TO DISBURSE-PUR-EXIT.
           IF VDUSTATUS = "1U"
              GO TO DISBURSE-PUR-RECOURSE.
           IF VDUSTATUS NOT = "00" AND NOT = "1D"
              GO TO DISBURSE-PUR-PARTRCAMT.
           IF VDUNUM2 < 0
              GO TO DISBURSE-PUR-PARTRCAMT.
           MOVE VDUNUM2 TO HOLD-LN-PARTRCAMT.
           IF HOLD-LN-PARTRCAMT > LN-CURBAL
              GO TO DISBURSE-PUR-PARTRCAMT.
       DISBURSE-PUR-DLRVPRIN-1.
           ADD 1 TO SUB.
           MOVE "E  S062GA" TO SCR-1.
           MOVE SUB TO SCR-2.
           IF SCR-2 < 4
              ADD 6 TO SCR-2
           ELSE
              SUBTRACT 4 FROM SCR-2.
           MOVE "." TO  SCR-3.
           MOVE SCR-ALL TO VDU.
           IF GP-DLRVPRIN-1-FG NOT = "Y"
              MOVE "S  S062GA" TO SCR-1
              MOVE SCR-ALL TO VDU
              MOVE 0 TO VDUNUM2
              PERFORM SCREENIO
              GO TO SKIP-DLRVPRIN-1.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10"
              MOVE 9 TO IO-FG
              GO TO DISBURSE-PUR-EXIT.
           IF VDUSTATUS = "1U"
              SUBTRACT 1 FROM SUB
              GO TO DISBURSE-PUR-RECOURSE.
           IF VDUSTATUS NOT = "00" AND NOT = "1D"
              SUBTRACT 1 FROM SUB
              GO TO DISBURSE-PUR-DLRVPRIN-1.
           IF VDUNUM2 < 0
              SUBTRACT 1 FROM SUB
              GO TO DISBURSE-PUR-DLRVPRIN-1.
       SKIP-DLRVPRIN-1.
           MOVE VDUNUM2 TO HOLD-LN-DLRVPRIN(1).
           PERFORM DISBURSE-PUR-TEST.
           IF IO-FG NOT = 0
              SUBTRACT 1 FROM SUB
              GO TO DISBURSE-PUR-DLRVPRIN-1.
       DISBURSE-PUR-DLRVPRIN-2.
           ADD 1 TO SUB.
           MOVE "E  S062GA" TO SCR-1.
           MOVE SUB TO SCR-2.
           IF SCR-2 < 4
              ADD 6 TO SCR-2
           ELSE
              SUBTRACT 4 FROM SCR-2.
           MOVE "." TO  SCR-3.
           MOVE SCR-ALL TO VDU.
           IF GP-DLRVPRIN-2-FG NOT = "Y"
              MOVE "S  S062GA" TO SCR-1
              MOVE SCR-ALL TO VDU
              MOVE 0 TO VDUNUM2
              PERFORM SCREENIO
              GO TO SKIP-DLRVPRIN-2.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10"
              MOVE 9 TO IO-FG
              GO TO DISBURSE-PUR-EXIT.
           IF VDUSTATUS = "1U"
              SUBTRACT 2 FROM SUB
              GO TO DISBURSE-PUR-DLRVPRIN-1.
           IF VDUSTATUS NOT = "00" AND NOT = "1D"
              SUBTRACT 1 FROM SUB
              GO TO DISBURSE-PUR-DLRVPRIN-2.
           IF VDUNUM2 < 0
              SUBTRACT 1 FROM SUB
              GO TO DISBURSE-PUR-DLRVPRIN-2.
       SKIP-DLRVPRIN-2.
           MOVE VDUNUM2 TO HOLD-LN-DLRVPRIN(2).
           PERFORM DISBURSE-PUR-TEST.
           IF IO-FG NOT = 0
              SUBTRACT 1 FROM SUB
              GO TO DISBURSE-PUR-DLRVPRIN-2.

           GO TO DISBURSE-PUR-DLSCHG.

      *DISBURSE-PUR-DLRVPRIN-3.
      *    ADD 1 TO SUB.
      *    MOVE "E  S062GA" TO SCR-1.
      *    MOVE SUB TO SCR-2.
      *    IF SCR-2 < 4
      *       ADD 6 TO SCR-2
      *    ELSE
      *       SUBTRACT 4 FROM SCR-2.
      *    MOVE "." TO  SCR-3.
      *    MOVE SCR-ALL TO VDU.
      *    IF GP-DLRVPRIN-3-FG NOT = "Y"
      *       MOVE "S  S062GA" TO SCR-1
      *       MOVE SCR-ALL TO VDU
      *       MOVE 0 TO VDUNUM2
      *       PERFORM SCREENIO
      *       GO TO SKIP-DLRVPRIN-3.
      *    PERFORM SCREENIO.
      *    IF VDUSTATUS = "10"
      *       MOVE 9 TO IO-FG
      *       GO TO DISBURSE-PUR-EXIT.
      *    IF VDUSTATUS = "1U"
      *       SUBTRACT 2 FROM SUB
      *       GO TO DISBURSE-PUR-DLRVPRIN-2.

      *    IF VDUSTATUS NOT = "00" AND NOT = "1D"
      *       SUBTRACT 1 FROM SUB
      *       GO TO DISBURSE-PUR-DLRVPRIN-3
      *    IF VDUNUM2 < 0
      *       SUBTRACT 1 FROM SUB
      *       GO TO DISBURSE-PUR-DLRVPRIN-3.

      *SKIP-DLRVPRIN-3.
      *    MOVE VDUNUM2 TO HOLD-LN-DLRVPRIN(3).
      *    PERFORM DISBURSE-PUR-TEST.
      *    IF IO-FG NOT = 0
      *       SUBTRACT 1 FROM SUB
      *       GO TO DISBURSE-PUR-DLRVPRIN-3.


       DISBURSE-PUR-DLSCHG.
           ADD 1 TO SUB.
           MOVE "E  S062GA" TO SCR-1.
           MOVE SUB TO SCR-2.
           IF SCR-2 < 4
              ADD 6 TO SCR-2
           ELSE
              SUBTRACT 4 FROM SCR-2.
           MOVE "." TO  SCR-3.
           MOVE SCR-ALL TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10"
              MOVE 9 TO IO-FG
              GO TO DISBURSE-PUR-EXIT.
           IF VDUSTATUS = "1U"
              SUBTRACT 2 FROM SUB
              GO TO DISBURSE-PUR-DLRVPRIN-2.

           IF VDUSTATUS NOT = "00" AND NOT = "1D"
              SUBTRACT 1 FROM SUB
              GO TO DISBURSE-PUR-DLSCHG.
           IF VDUNUM2 < 0
              SUBTRACT 1 FROM SUB
              GO TO DISBURSE-PUR-DLSCHG.
           MOVE VDUNUM2 TO HOLD-LN-ANTIC-DISC.
           PERFORM DISBURSE-PUR-TEST.
           IF IO-FG NOT = 0
              SUBTRACT 1 FROM SUB
              GO TO DISBURSE-PUR-DLSCHG.

       DISBURSE-PUR-ANTIC-INT.
           IF LN-LOANTYPE NOT = "P" OR INT-FLAG NOT = 1
              GO TO DISBURSE-PUR-ANTIC-SC.

           ADD 1 TO SUB.
           MOVE "E NS062GA" TO SCR-1.
           MOVE SUB TO SCR-2.
           IF SCR-2 < 4
              ADD 6 TO SCR-2
           ELSE
              SUBTRACT 4 FROM SCR-2.
           MOVE "." TO  SCR-3.
           MOVE SCR-ALL TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10"
              MOVE 9 TO IO-FG
              GO TO DISBURSE-PUR-EXIT.
           IF VDUSTATUS = "1U"
              SUBTRACT 2 FROM SUB
              GO TO DISBURSE-PUR-DLSCHG.
           IF VDUSTATUS NOT = "00" AND NOT = "1D"
              SUBTRACT 1 FROM SUB
              GO TO DISBURSE-PUR-ANTIC-INT.
           IF VDUNUM2 < 0
              SUBTRACT 1 FROM SUB
              GO TO DISBURSE-PUR-ANTIC-INT.

           IF VDUNUM2 NOT = INT-REB
              MOVE TP-ANTIC-SEQ TO PASSWDW-SEQ
              MOVE "TO OVERRIDE ANTIC INT" TO PASSWDW-PROMPT2
              PERFORM VERIFY-PASSWORD
              IF PASSWDW-STAT NOT = "OK"
                 SUBTRACT 1 FROM SUB
                 GO TO DISBURSE-PUR-ANTIC-INT.

           MOVE VDUNUM2 TO HOLD-LN-ANTIC-INT.
           PERFORM DISBURSE-PUR-TEST.
           IF IO-FG NOT = 0
              SUBTRACT 1 FROM SUB
              GO TO DISBURSE-PUR-ANTIC-INT.
       DISBURSE-PUR-ANTIC-SC.
           IF SC-FLAG NOT = 1
              GO TO DISBURSE-PUR-ANTIC-MF.
           ADD 1 TO SUB.
           MOVE SUB TO SCR-2.
           MOVE "E NS062GA" TO SCR-1.
           IF SCR-2 < 4
              ADD 6 TO SCR-2
           ELSE
              SUBTRACT 4 FROM SCR-2.
           MOVE "." TO  SCR-3.
           MOVE SCR-ALL TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10"
              MOVE 9 TO IO-FG
              GO TO DISBURSE-PUR-EXIT.
           IF VDUSTATUS = "1U"
              SUBTRACT 2 FROM SUB
              IF INT-FLAG = 1
                 GO TO DISBURSE-PUR-ANTIC-INT
              ELSE
                 GO TO DISBURSE-PUR-DLSCHG.
           IF VDUSTATUS NOT = "00" AND NOT = "1D"
              SUBTRACT 1 FROM SUB
              GO TO DISBURSE-PUR-ANTIC-SC.
           IF VDUNUM2 < 0
              SUBTRACT 1 FROM SUB
              GO TO DISBURSE-PUR-ANTIC-SC.
           MOVE VDUNUM2 TO HOLD-LN-ANTIC-SC.
           PERFORM DISBURSE-PUR-TEST.
           IF IO-FG NOT = 0
              SUBTRACT 1 FROM SUB
              GO TO DISBURSE-PUR-ANTIC-SC.
       DISBURSE-PUR-ANTIC-MF.
           IF MF-FLAG NOT = 1
              GO TO DISBURSE-PUR-PAYCODE.
           ADD 1 TO SUB.
           MOVE "E NS062GA" TO SCR-1.
           MOVE SUB TO SCR-2.
           IF SCR-2 < 4
              ADD 6 TO SCR-2
           ELSE
              SUBTRACT 4 FROM SCR-2.
           MOVE "." TO  SCR-3.
           MOVE SCR-ALL TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10"
              MOVE 9 TO IO-FG
              GO TO DISBURSE-PUR-EXIT.
           IF VDUSTATUS = "1U"
              SUBTRACT 2 FROM SUB
              IF SC-FLAG = 1
                 GO TO DISBURSE-PUR-ANTIC-SC
              ELSE
              IF INT-FLAG = 1
                 GO TO DISBURSE-PUR-ANTIC-INT
              ELSE
                 GO TO DISBURSE-PUR-DLSCHG.
           IF VDUSTATUS NOT = "00" AND NOT = "1D"
              SUBTRACT 1 FROM SUB
              GO TO DISBURSE-PUR-ANTIC-MF.
           IF VDUNUM2 < 0
              SUBTRACT 1 FROM SUB
              GO TO DISBURSE-PUR-ANTIC-MF.
           MOVE VDUNUM2 TO HOLD-LN-ANTIC-MF.
           PERFORM DISBURSE-PUR-TEST.
           IF IO-FG NOT = 0
              SUBTRACT 1 FROM SUB
              GO TO DISBURSE-PUR-ANTIC-MF.
           GO TO DISBURSE-PUR-PAYCODE.
       DISBURSE-PUR-TEST.
           MOVE 0 TO IO-FG.
           COMPUTE HOLD-LN-PROCEEDS =
               LN-CURBAL - (HOLD-LN-DLRVPRIN(1) + HOLD-LN-DLRVPRIN(2)
                            + HOLD-LN-DLRVPRIN(3)
                             + HOLD-LN-ANTIC-DISC + HOLD-LN-ANTIC-INT
                              + HOLD-LN-ANTIC-SC + HOLD-LN-ANTIC-MF).
           MOVE "S  A320F73." TO VDU.
           MOVE SPACES TO D-F73.
           MOVE "ANT. ERN:" TO D-F73B.
           COMPUTE D-F73C = HOLD-LN-ANTIC-DISC + HOLD-LN-ANTIC-INT +
                            HOLD-LN-ANTIC-SC + HOLD-LN-ANTIC-MF.
           MOVE D-F73 TO VDUBUF.
           PERFORM SCREENIO.
           MOVE "S  A320F83." TO VDU.
           MOVE SPACES TO D-F83.
           MOVE "PROCEEDS:" TO D-F83B.
           MOVE HOLD-LN-PROCEEDS TO D-F83C.
           MOVE D-F83 TO VDUBUF.
           PERFORM SCREENIO.
           IF HOLD-LN-PROCEEDS < 0
              MOVE "EXCEEDS PURCHASED AMOUNT" TO MESS
              PERFORM SEND-MESS
              MOVE 9 TO IO-FG.
       DISBURSE-PUR-PAYCODE.
           PERFORM ENTER-DEALER-CHECK-PAYCODE.
           IF HOLD-CKCODE = "4"
              SUBTRACT 1 FROM SUB
              IF MF-FLAG = 1
                 GO TO DISBURSE-PUR-ANTIC-MF
              ELSE
              IF SC-FLAG = 1
                 GO TO DISBURSE-PUR-ANTIC-SC
              ELSE
              IF INT-FLAG = 1
                 GO TO DISBURSE-PUR-ANTIC-INT
              ELSE
                 GO TO DISBURSE-PUR-DLSCHG.
           MOVE 0 TO IO-FG.
       DISBURSE-PUR-EXIT.
           EXIT.

      **************************************
       ENTER-DEALER-CHECK-PAYCODE SECTION.
      **************************************
       ENTER-DEALER-CHK-PAYCD.
           IF HOLD-LN-PROCEEDS NOT > 0
              MOVE "M" TO HOLD-CKCODE
              GO TO EXIT-ENTER-DEALER-CHK-PAYCD.

           MOVE "^-BACK, CHECK :S-SYSTEM,M-MANUAL,B-BATCH:" TO MESS.
           PERFORM SEND-LEGEND.
           MOVE "E RA060SEL." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
               MOVE "4" TO HOLD-CKCODE
               GO TO EXIT-ENTER-DEALER-CHK-PAYCD.
           IF VDUSTATUS NOT = "00"
              GO TO ENTER-DEALER-CHK-PAYCD.
           MOVE VDUBUF TO ACTION.
           IF ACTION-1 NOT = SPACES  OR
              ACTION-2 NOT = "M"
                    AND NOT = "S"
                     AND NOT = "B"
              GO TO ENTER-DEALER-CHK-PAYCD.
           MOVE ACTION-2 TO HOLD-CKCODE.
           IF HOLD-CKCODE = "B"
              GO TO EXIT-ENTER-DEALER-CHK-PAYCD.
       ENTER-CHECK-NO.
           IF HOLD-CKCODE NOT = "M"
              GO TO ENTER-FREE-TYPE.
           MOVE "^-BACK, ENTER MANUAL CHECK #:" TO MESS.
           PERFORM SEND-LEGEND.
           MOVE "EN N060CKNO." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-DEALER-CHK-PAYCD.
           IF VDUSTATUS NOT = "00"
              GO TO ENTER-CHECK-NO.
           IF VDUNUM0 = 0
              MOVE "INVALID CHECK #" TO MESS
              PERFORM SEND-LEGEND
              GO TO ENTER-CHECK-NO.
           MOVE BRANCH TO CK-BRNO.
           MOVE VDUNUM0 TO HOLD-CKNO CK-NO.
           PERFORM OPEN-CK1-FILE.
           PERFORM READ-CK1-FILE.
           IF IO-FG = 0
              MOVE "CHECK ALREADY EXISTS!" TO MESS
              PERFORM SEND-MESS
              PERFORM CLOSE-CK1-FILE
              GO TO ENTER-CHECK-NO.
           PERFORM CLOSE-CK1-FILE.
           GO TO EXIT-ENTER-DEALER-CHK-PAYCD.
       ENTER-FREE-TYPE.
           MOVE "FREE TYPING? (Y/N):" TO MESS.
           PERFORM SEND-LEGEND.
           MOVE "E  A060SEL." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-DEALER-CHK-PAYCD.
           IF VDUSTATUS NOT = "00"
              GO TO ENTER-FREE-TYPE.
           IF VDUBUF = "     N"
              GO TO EXIT-ENTER-DEALER-CHK-PAYCD.
           IF VDUBUF NOT = "     Y"
              GO TO ENTER-FREE-TYPE.

           MOVE SPACES TO HOLD-FREE1 HOLD-FREE2.

       ENTER-FREE-TYPE1.
           MOVE "ENTER FREE TYPING 1:" TO MESS.
           PERFORM MSG-LINE.
           MOVE "R RA400MSEL." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS NOT = "00"
              GO TO ENTER-FREE-TYPE1.
           MOVE VDUBUF TO HOLD-FREE1.
       ENTER-FREE-TYPE2.
           MOVE "ENTER FREE TYPING 2:" TO MESS.
           PERFORM MSG-LINE.
           MOVE "R RA400MSEL." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-FREE-TYPE1.
           IF VDUSTATUS NOT = "00"
              GO TO ENTER-FREE-TYPE2.
           MOVE VDUBUF TO HOLD-FREE2.

           MOVE SPACES TO MESS.
           PERFORM MSG-LINE.

       EXIT-ENTER-DEALER-CHK-PAYCD.
           EXIT.

      ***************************************
       DISBURSE-PUR-DISPLAY SECTION.
      ***************************************
           MOVE SPACES TO LINE-BUF WR-BUF.
           MOVE LINE-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

           MOVE 0 TO INT-FLAG SC-FLAG MF-FLAG.
           MOVE 9 TO SUB.

           IF LN-MAINTFEE NOT = 0
              IF LN-MF-REBATE = " "
                            OR = "C"
              MOVE "ANTIC. MAINT. FEE:" TO MAINT-REB-A
              SUBTRACT 1 FROM SUB
              PERFORM COMPUTE-MAINT-REBATE
              MOVE 1 TO MF-FLAG
              IF REB-SUB NOT = 0
                 MOVE REB-REBATE TO MAINT-REB
              ELSE
                 MOVE 0 TO MAINT-REB.
       SKIP.
           IF LN-SERCHG NOT = 0
              IF LN-SC-REBATE = " "
                            OR = "C"
              MOVE "ANTIC. SERV. CHG :" TO SCHG-REB-A
              SUBTRACT 1 FROM SUB
              PERFORM COMPUTE-SCHG-REBATE
              MOVE 1 TO SC-FLAG
              IF REB-SUB NOT = 0
                 MOVE REB-REBATE TO SCHG-REB
              ELSE
                 MOVE 0 TO SCHG-REB.

           MOVE 0 TO INT-REB.
           IF LN-LOANTYPE = "P"
              IF LN-INT-REBATE = " "
                            OR = "C"
              MOVE "ANTIC. INTEREST  :" TO INT-REB-A
              SUBTRACT 1 FROM SUB
              PERFORM COMPUTE-INT-REBATE
              MOVE 1 TO INT-FLAG
              IF REB-SUB NOT = 0
                 MOVE REB-REBATE TO INT-REB.

           SUBTRACT 4 FROM SUB.
           MOVE SUB TO SUB1.

           MOVE "S  A200GD" TO SCR-1.
           MOVE SUB TO SCR-2.
           IF SCR-2 < 4
              ADD 6 TO SCR-2
           ELSE
              SUBTRACT 4 FROM SCR-2.
           MOVE "." TO SCR-3.
           MOVE SCR-ALL TO VDU.
           MOVE "RECOURSE         :" TO VDUBUF.
           PERFORM SCREENIO.
      
           ADD 1 TO SUB.

           MOVE "S  A200GD" TO SCR-1.
           MOVE SUB TO SCR-2.
           IF SCR-2 < 4
              ADD 6 TO SCR-2
           ELSE
              SUBTRACT 4 FROM SCR-2.
           MOVE "." TO SCR-3.
           MOVE SCR-ALL TO VDU.
           MOVE "RESERVE PRIN 1   :" TO VDUBUF.
           PERFORM SCREENIO.

           ADD 1 TO SUB.

           MOVE "S  A200GD" TO SCR-1.
           MOVE SUB TO SCR-2.
           IF SCR-2 < 4
              ADD 6 TO SCR-2
           ELSE
              SUBTRACT 4 FROM SCR-2.
           MOVE "." TO SCR-3.
           MOVE SCR-ALL TO VDU.
           MOVE "RESERVE PRIN 2   :" TO VDUBUF.
           PERFORM SCREENIO.

           ADD 1 TO SUB.

           MOVE "S  A200GD" TO SCR-1.
           MOVE SUB TO SCR-2.
           IF SCR-2 < 4
              ADD 6 TO SCR-2
           ELSE
              SUBTRACT 4 FROM SCR-2.
           MOVE "." TO SCR-3.
           MOVE SCR-ALL TO VDU.
           MOVE "DISCOUNT         :" TO VDUBUF.
           PERFORM SCREENIO.
           ADD 1 TO SUB.

           MOVE SUB TO SCR-2.
           IF SCR-2 < 4
              ADD 6 TO SCR-2
           ELSE
              SUBTRACT 4 FROM SCR-2.
           IF INT-FLAG = 1
              MOVE "S  A200GD" TO SCR-1
              MOVE "." TO SCR-3
              MOVE SCR-ALL TO VDU
              MOVE INT-REB-A TO VDUBUF
              PERFORM SCREENIO
              MOVE "S  A200GA" TO SCR-1
              MOVE "." TO SCR-3
              MOVE INT-REB TO REB-DISPLAY
              MOVE SCR-ALL TO VDU
              MOVE REB-FIELD TO VDUBUF
              PERFORM SCREENIO
              ADD 1 TO SUB.

           MOVE SUB TO SCR-2.
           IF SCR-2 < 4
              ADD 6 TO SCR-2
           ELSE
              SUBTRACT 4 FROM SCR-2.
           IF SC-FLAG = 1
              MOVE "S  A200GD" TO SCR-1
              MOVE "." TO SCR-3
              MOVE SCR-ALL TO VDU
              MOVE SCHG-REB-A TO VDUBUF
              PERFORM SCREENIO
              MOVE "S  A200GA" TO SCR-1
              MOVE "." TO SCR-3
              MOVE SCHG-REB TO REB-DISPLAY
              MOVE SCR-ALL TO VDU
              MOVE REB-FIELD TO VDUBUF
              PERFORM SCREENIO
              ADD 1 TO SUB.

           MOVE SUB TO SCR-2.
           IF SCR-2 < 4
              ADD 6 TO SCR-2
           ELSE
              SUBTRACT 4 FROM SCR-2.
           IF MF-FLAG = 1
              MOVE "S  A200GD" TO SCR-1
              MOVE "." TO SCR-3
              MOVE SCR-ALL TO VDU
              MOVE MAINT-REB-A TO VDUBUF
              PERFORM SCREENIO
              MOVE "S  A200GA" TO SCR-1
              MOVE "." TO SCR-3
              MOVE MAINT-REB TO REB-DISPLAY
              MOVE SCR-ALL TO VDU
              MOVE REB-FIELD TO VDUBUF
              PERFORM SCREENIO.

           MOVE "ANT. ERN:" TO D-F73B.
           MOVE "PROCEEDS:" TO D-F83B.

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
           PERFORM OPEN-LP1-FILE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      * ***NOTE*** 'START-LP1-FILE-SCAN' MUST BE USED WHEN FIRST USING
      * 'START-LP1-FILE-NOT-GREATER'; USED WHEN DISPLAYING MULTIPLE
      * PAGES IN AN UP & DOWN FUNCTION; YOU NEED TO ADD ONE TO 
      * LP-SEQNO (IN WBEG) IN ORDER TO GET THE DISPLAY TO WORK
      * LIKE VISION FILES.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
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

      **************************************************
       DISPLAY-ENTRY-UP SECTION.
      **************************************************
           MOVE SPACES TO LINE-BUF LN-BUF.
           MOVE 9 TO LN-WK.
           MOVE HOLD2-KEY-UP TO LP1-KEY.
           IF LP-SEQNO = 0
              MOVE 9 TO IO-FG
              GO TO DISPLAY-UP-EXIT.

           MOVE LP1-KEY TO HOLD1-KEY-DOWN.
           PERFORM OPEN-LP1-FILE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *  DEFAULT THE WBEG AND WEND
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
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
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      * ALWAYS DISPLAY REVERSAL BALANCES, #487
      * 8/11/98 SUPPRESS BALANCE IF ENV 'RVLINES' IS SET (MJD AS PER JTG)
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
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
           MOVE 0 TO WK-ELE.
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

      *********************************************
       CODE-SCAN SECTION.
      *********************************************

           MOVE "PO"   TO CDSCAN-TYPE.
           MOVE SPACES TO CDSCAN-CODE.

           MOVE "WI/CDSCAN" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH CDSCAN-BUF EXIT-PATHNAME.
           CANCEL FORM-PROGX.
           IF CDSCAN-STATUS NOT = "00" OR CDSCAN-CODE = SPACES
               MOVE 9 TO IO-FG
               GO TO EXIT-CODE-SCAN.

           MOVE "SNNA060SEL."  TO VDU.
           MOVE CDSCAN-CODE TO VDUBUF.
           PERFORM SCREENIO.

           MOVE CDSCAN-CODE TO CD-CODE.
           MOVE 0 TO IO-FG.

       EXIT-CODE-SCAN.
           EXIT.

       COPY "LIBLP/LPUERN.CPY".
       COPY "LIBLP/LPCERN.CPY".
       COPY "LIBLP/LPREGZ.CPY".

       COPY "LIBGB/CAPASSWD.CPY".
       COPY "LIBLP/LPEADJ.CPY".
       COPY "LIBGB/GPENV.CPY".


      ***************************************************************
       SETUP-LINK-INFO SECTION.
      ***************************************************************
           IF VDU-FORM-NAME = "LPSMNU"
              MOVE LPSMNU-HELP--FILE TO HELP-LINK-FILE
              MOVE LPSMNU-HELP--DIR  TO HELP-LINK-DIR
           ELSE
              MOVE LONPF-HELP--FILE TO HELP-LINK-FILE
              MOVE LONPF-HELP--DIR TO HELP-LINK-DIR.

      ***************************************************************
       VDU-CONVERT-FIELD SECTION.
      ***************************************************************
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
       SET-LP-EARNED.
           MOVE 0 TO REB-REBATE.
           MOVE LN-EARN-ORDER(1) TO SUB.
           IF LN-PURBAL = LN-CURBAL
              IF LP-IBPC = "P"
                 PERFORM COMPUTE-INT-REBATE
              END-IF
           ELSE
              MOVE 0 TO REB-SUB.
           IF REB-SUB = 0
              MOVE 0 TO REB-REBATE
           ELSE
              MOVE REB-REBATE TO EADJ-REBATE
              PERFORM EADJ-EARN-ADJUSTMENT
              IF EADJ-ADJUST-FG(1) = "Y"
                 MOVE EADJ-ADJUST(1) TO REB-REBATE
              ELSE
                 MOVE 0 TO REB-REBATE.
           COMPUTE HOLD-LN-ACCERN(1) = HOLD-LN-ACCERN(1) - REB-REBATE +
                                       (LN-ANTICERN(1) - LN-ACCERN(1)).
           COMPUTE HOLD-LN-TOTERN    = HOLD-LN-TOTERN - REB-REBATE +
                                       (LN-ANTICERN(1) - LN-ACCERN(1)).
           IF SUB > 3
      *       COMPUTE WK-LXE-EARN(SUB - 3) = LN-ANTICERN(1) -
              COMPUTE LXE-EARN(SUB - 3) = LN-ANTICERN(1) -
                                          LN-ACCERN(1) - REB-REBATE
           ELSE
              COMPUTE LP-EARNED(SUB)   = LN-ANTICERN(1) - LN-ACCERN(1)
                                         - REB-REBATE.

           MOVE LN-EARN-ORDER(2) TO SUB.
           IF REB-SUB = 0
              MOVE 0 TO REB-REBATE
           ELSE
           IF EADJ-ADJUST-FG(2) = "Y"
              MOVE EADJ-ADJUST(2) TO REB-REBATE
           ELSE
              MOVE 0 TO REB-REBATE.
           COMPUTE HOLD-LN-ACCERN(2) = HOLD-LN-ACCERN(2) - REB-REBATE +
                                       (LN-ANTICERN(2) - LN-ACCERN(2)).
           COMPUTE HOLD-LN-TOTERN    = HOLD-LN-TOTERN - REB-REBATE +
                                      (LN-ANTICERN(2) - LN-ACCERN(2)).
           IF SUB > 3
      *       COMPUTE WK-LXE-EARN(SUB - 3) = LN-ANTICERN(2) -
              COMPUTE LXE-EARN(SUB - 3) = LN-ANTICERN(2) -
                                             LN-ACCERN(2) - REB-REBATE
           ELSE
              COMPUTE LP-EARNED(SUB)   = LN-ANTICERN(2) - LN-ACCERN(2)
                                         - REB-REBATE.

           MOVE 0 TO REB-REBATE.
           IF EADJ-ADJUST-FG(3) = "Y"
              IF REB-SUB = 0
                 MOVE 0 TO REB-REBATE
              ELSE
              IF EADJ-ADJUST-FG(3) = "Y"
                 MOVE EADJ-ADJUST(3) TO REB-REBATE
              ELSE
                 MOVE 0 TO REB-REBATE
           ELSE
           IF EADJ-ADJUST-FG(3) NOT = "Y"
              IF LN-PURBAL = LN-CURBAL
                 PERFORM COMPUTE-SCHG-REBATE
              ELSE
                 MOVE 0 TO REB-SUB.
           IF REB-SUB = 0 AND EADJ-ADJUST-FG(3) NOT = "Y"
              MOVE 0 TO REB-REBATE.
           MOVE LN-EARN-ORDER(3) TO SUB.
           COMPUTE HOLD-LN-ACCERN(3) = HOLD-LN-ACCERN(3) - REB-REBATE +
                                       (LN-ANTICERN(3) - LN-ACCERN(3)).
           COMPUTE HOLD-LN-TOTERN    = HOLD-LN-TOTERN - REB-REBATE +
                                       (LN-ANTICERN(3) - LN-ACCERN(3)).
           IF SUB > 3
      *       COMPUTE WK-LXE-EARN(SUB - 3) = LN-ANTICERN(3) -
              COMPUTE LXE-EARN(SUB - 3) = LN-ANTICERN(3) -
                                          LN-ACCERN(3) - REB-REBATE
           ELSE
              COMPUTE LP-EARNED(SUB)   = LN-ANTICERN(3) - LN-ACCERN(3) -
                                         REB-REBATE.

           MOVE 0 TO REB-REBATE.
           IF EADJ-ADJUST-FG(4) = "Y"
              IF REB-SUB = 0
                 MOVE 0 TO REB-REBATE
              ELSE
              IF EADJ-ADJUST-FG(4) = "Y"
                 MOVE EADJ-ADJUST(4) TO REB-REBATE
              ELSE
                 MOVE 0 TO REB-REBATE
           ELSE
           IF EADJ-ADJUST-FG(4) NOT = "Y"
              IF LN-PURBAL = LN-CURBAL
                 PERFORM COMPUTE-MAINT-REBATE
              ELSE
                 MOVE 0 TO REB-SUB.
           IF REB-SUB = 0 AND EADJ-ADJUST-FG(4) NOT = "Y"
              MOVE 0 TO REB-REBATE.
           MOVE LN-EARN-ORDER(4) TO SUB.
           COMPUTE HOLD-LN-ACCERN(4) = HOLD-LN-ACCERN(4) - REB-REBATE +
                                       (LN-ANTICERN(4) - LN-ACCERN(4)).
           COMPUTE HOLD-LN-TOTERN    = HOLD-LN-TOTERN - REB-REBATE +
                                       (LN-ANTICERN(4) - LN-ACCERN(4)).
           IF SUB > 3
      *       COMPUTE WK-LXE-EARN(SUB - 3) = LN-ANTICERN(4) -
              COMPUTE LXE-EARN(SUB - 3) = LN-ANTICERN(4) -
                                          LN-ACCERN(4) - REB-REBATE
           ELSE
              COMPUTE LP-EARNED(SUB)   = LN-ANTICERN(4) - LN-ACCERN(4) -
                                         REB-REBATE.

           MOVE LN-EARN-ORDER(5) TO SUB.
           COMPUTE HOLD-LN-ACCERN(5) = HOLD-LN-ACCERN(5) +
                                       (LN-ANTICERN(5) - LN-ACCERN(5)).
           COMPUTE HOLD-LN-TOTERN    = HOLD-LN-TOTERN +
                                       (LN-ANTICERN(5) - LN-ACCERN(5)).
           IF SUB > 3
      *       COMPUTE WK-LXE-EARN(SUB - 3) = LN-ANTICERN(5) -
              COMPUTE LXE-EARN(SUB - 3) = LN-ANTICERN(5) - LN-ACCERN(5)
           ELSE
              COMPUTE LP-EARNED(SUB)   = LN-ANTICERN(5) - LN-ACCERN(5).

           MOVE LN-EARN-ORDER(6) TO SUB.
           COMPUTE HOLD-LN-ACCERN(6) = HOLD-LN-ACCERN(6) +
                                       (LN-ANTICERN(6) - LN-ACCERN(6)).
           COMPUTE HOLD-LN-TOTERN    = HOLD-LN-TOTERN +
                                       (LN-ANTICERN(6) - LN-ACCERN(6)).
           IF SUB > 3
      *       COMPUTE WK-LXE-EARN(SUB - 3) = LN-ANTICERN(6) -
              COMPUTE LXE-EARN(SUB - 3) = LN-ANTICERN(6) -
                                             LN-ACCERN(6)
           ELSE
              COMPUTE LP-EARNED(SUB)   = LN-ANTICERN(6) - LN-ACCERN(6).


       DISPLAY-EARNINGS.
           IF HOLD-DISPLAY-ERN = "A"
              ADD HOLD-LN-ACCERN(1) HOLD-LN-ACCERN(2)
                  HOLD-LN-ACCERN(3)
                  HOLD-LN-ACCERN(5) GIVING TOTEARN
              IF NOT( GP-INCL-OTH-IN-EARNED-N OR
                                        GP-INCL-OTH-IN-EARNED-R)
                 ADD HOLD-LN-ACCERN(6) TO TOTEARN
              END-IF
              IF NOT(GP-INCL-OTH-IN-EARNED-R)
                 ADD HOLD-LN-ACCERN(4) TO TOTEARN
              END-IF
              MOVE TOTEARN TO D-TOTEARN
           ELSE
           IF HOLD-DISPLAY-ERN = "U" AND
              LN-PLCD = " " AND
              LP-TRCD = "TP"
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

              IF NOT GP-INCL-OTH-IN-EARNED-R
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
           MOVE LCAP-LCPDTH-DATE TO NUM-DATE.
           MOVE NUM-MO TO D-LCPDTH-MO.
           IF LN-UNITPER-CD-DAYS
              MOVE NUM-DA TO D-LCPDTH-DAYR
           ELSE
              MOVE NUM-YR TO D-LCPDTH-DAYR.
           COMPUTE D-EARNED = LXE-EARN(1)  + LXE-EARN(2) +
                              LXE-EARN(3)  + LP-EARNED(1) +
                              LP-EARNED(2) + LP-EARNED(3).
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
       COMPUTE-INT-REBATE.
           IF LN-INT-REBATE = " "
                         OR = "C"
              MOVE 7 TO REB-SUB
              MOVE LN-INTCHG TO REB-TOTCHG
              MOVE LN-ORGTERM TO REB-ORGTERM
              MOVE LP-PAYDATE TO REB-PAYDATE
              PERFORM REBATE.
       COMPUTE-SCHG-REBATE.
           IF LN-SERCHG NOT = 0
              IF LN-SC-REBATE = " "
                            OR = "C"
                 MOVE 8 TO REB-SUB
                 MOVE LN-SERCHG TO REB-TOTCHG
                 MOVE LN-ORGTERM TO REB-ORGTERM
                 MOVE LP-PAYDATE TO REB-PAYDATE
                 PERFORM REBATE.
       COMPUTE-MAINT-REBATE.
           IF LN-MAINTFEE NOT = 0
              IF LN-MF-REBATE = " "
                            OR = "C"
                 MOVE 9 TO REB-SUB
                 COMPUTE REB-TOTCHG = LN-MAINTFEE * LN-ORGTERM
                 MOVE LN-ORGTERM TO REB-ORGTERM
                 MOVE LP-PAYDATE TO REB-PAYDATE
                 PERFORM REBATE.
       SEND-INT-OR-LC.
           IF LP-IBPC = "I"
              MOVE "   INT" TO VDUBUF
            ELSE
              MOVE "LT-CHG" TO VDUBUF.
           PERFORM SCREENIO.
      ****** TESTING UP FRONT DEALER DISC MJD 2008-02-28
      ******
      ******
       SET-EARNINGS.
           MOVE UERN-EARNINGS(SUB) TO LN-UFERN(SUB) LN-ACCERN(SUB).
           ADD 1 TO SUB.
      ******
      ******
       DISPLAY-ALL-LINES.
           MOVE LINE-BUF TO WR-BUF.
           MOVE LINE-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.
       MSG-LINE.
           MOVE MESS TO TMMSG-MESS.
           PERFORM CALL-TMMSG.
      *    CALL TMMSG USING MESS.
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
       EXCEEDS-PUR-BAL.
           MOVE LN-PURBAL TO PURBAL-EXCEEDED.
           MOVE PUR-BAL-EXCEEDED TO MESS.
           PERFORM SEND-MESS.
       SEND-ENTRY-LINE.
           MOVE ENTRY-BUF TO WR-BUF.
           MOVE ENTRY-LINE TO WR-LIST.
           PERFORM CALL-WRFORM.
      *-----------------------------------------------------------------

       COPY "LIBGB/DATER.CPY".
       COPY "LIBLP/LPITRM.CPY".
       COPY "LIBLP/LPIBPC.CPY".
       COPY "LIBLP/LPXDTE.CPY".
       COPY "LIBGB/SPPDTH.CPY".
       COPY "LIBLP/LPMDTE.CPY".
       COPY "LIBLP/LPAMTS.CPY".
       COPY "LIBLP/LPBWIN.CPY".
       COPY "LIBGB/REBATE.CPY".
       COPY "LIBLP/LPAPRS.CPY".
       COPY "LIBLP/SETCTBL.CPY".
       COPY "LIBLP/LPAPRZ.CPY".
       COPY "LIBLP/LPUPER.CPY".
       COPY "LIBLP/LPUPWK.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/RBACT.CPY".

      ********************************************
       IO-ROUTINES SECTION.
      ********************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBLP/LPSPGS_SQL.CPY".
       COPY "LIBLP/LPLTPGS_SQL.CPY".
       COPY "LIBLP/LPLTIGS_SQL.CPY".
       COPY "LIBLP/LPLPGS_SQL.CPY".
       COPY "LIBLP/LPCKGS_SQL.CPY".
       COPY "LIBLP/LPCDGS_SQL.CPY".
       COPY "LIBLP/LPCAGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBLP/LPLP1RN.CPY".
       COPY "LIBLP/LPLTP1RN.CPY".
       COPY "LIBLP/LPLTI1RN.CPY".
       COPY "LIBLP/LPSP1RN.CPY".
       COPY "LIBLP/LPCA1RN.CPY".
       COPY "LIBLP/LPCD1RN.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBLP/LPWKI.CPY".
       COPY "LIBLP/LPCK1RN.CPY".

       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO ERRCD.
       END-ROUTINE.
           PERFORM CLOSE-FILES.
       EXIT-PROG.
       STOP-RUN.
           EXIT PROGRAM.
