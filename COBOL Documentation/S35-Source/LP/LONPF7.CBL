       IDENTIFICATION DIVISION.
       PROGRAM-ID.      LONPF7.
       DATE-WRITTEN.    021784.
      **************************************************************************
      *  DESC:  LOAN PROCESSING
      *         TRANSACTION POSTING
      *         PROGRAM 7 - AL,AI,AP,WL,WI,WO,OT,O2,W2,OP,RE
      * REV :
      *  060695 BLL ADD NEW GL ENTRY ROUTINE
      *  MJD 100395 ADDED PASSWORD REQUIREMENT FOR "OT" TRANSACTIONS.
      *             ADDED PASSWORD REQUIREMENT FOR "AP" TRANSACTIONS.
      *  MJD 022096 ADDED SEPARATE PASSWORDS FOR WI, WL AND WO/W2.
      *  MJD 030596 ADDED PAYOFF CODE SCAN
      *  MJD 071196 ADDED TEST OF LN-UNITPER-CD FOR WEEKLY PDTH.
      *             USE PDTH-DATE-FULL INSTEAD OF PDTH-DATE.
      *  JTG 101196 ADDED 'LXE-WORKER' & 'LXE-WORKER2' FOR LPCERN-- A50 VERSION
      *  GLF 050797 ADDED COPY MEMBERS: SETCTBLW AND SETCTBL
      *  GLF 101497 ALLOWED CTRL 'N'
      *  MJD 101797 MOVED WK-UPDATE-BUF TO LP01WK
      *  BLV 110397 MOVE IN BR-GL-TYPE-ALLOWED, BR-GL-ENTRY-DEFAULT TO
      *             ALLENT-BUF BEFORE GL DISTRIBUTION WINDOW
      *  BAH 062698 TILLNO IS NOW ALPHANUMERIC
      *  BAH 073098 DISPLAY BALANCES EVEN ON REVERSALS, #487
      *  MJD 081198 ADDED ENVIRONMENT VARIABLE TO DETERMINE WHETHER OR NOT
      *             TO DISPLAY BALANCES ON REVERSALS. FIXED DISPLAY TO INDICATE
      *             INCLUDED O2 BAL WITH "SC" "RN" "PB" "RB" "RO" TRANSACTIONS
      *  JTG 120798 MODIFIED FOR REVOLVING OTHER CHARGES
      *  JTG 990405 CHANGED TO USE LN-OWNBR FOR GL DISTRIBUTION ON
      *             INTERBRANCH PAYMENTS
      *  JTG 990408 CHANGED TO PASS LN-OWNBR TO GL/ALLENT.C FOR INTERBRANCH
      *             PAYMENTS:
      *  JTG 990517 CHANGED TO ACCEPT INTERBRANCH PATH FOR CASH DRAWER
      *  GLB 990720 ADD CASH DRAWER PASSWORD OPTION TO ENTER DRAWER NO.
      *             BR-PASSWORD-DRAWER(Y/N) & CA-PASSWORD (4 CHAR PASSWORD)
      *             REGMGM PR# 901
      *  JTG 990728 CHANGED LOGIC TO NOT CHANGE SIGN ON REVOLVING OT AMOUNTS
      *  JTG 990729 ADDED CD-RO-NUM TO PROVIDE AN ALPHA KEY RE: CD-RO-CODE
      *  JTG 990730 ADDED 'WI' AND 'WL' FOR REVOLVING
      *  MJD 990803 ADDED GP-INCL-OTH-IN-EARNED FLAG. IF "Y" THEN INCLUDE
      *             EARNINGS BUCKET 6 (OTHER) IN ACCUMULATED TOTALS
      *             OTHERWISE EXCLUDE BUCKET 6.
      *  JTG 990811 STOPPED 'OT' REVOLVING FROM BEING LESS THAN ZERO PER JWR
      *  JTG 990901 ADDED 'AP' LOGIC FOR REVOLVING, #47 REGENCY
      *  JTG 991201 CORRECTED BUG WITH 'AP' APPLIED TO OTHER NOT GOING TO
      *             PRINCIPAL AND GOING TO AP-INTEREST, CHALLENGE FINANCE
      *  MJD 000119 ADDED LN-TOTNOLCHG (WORLD PR#218)
      *  MG  010201 REMOVED "01 WK-LXE" AND ADDED LIBLP/LPWKLXE.
      *  JTG 010615 ADDED NEW TRCD "OP" FOR AUTOMATIC OT/AP COMBINATION
      *             ADDED NEW TRCD "RE" FOR REPO EXENSE ADDON,   REGACC PR#1478
      *  GLB 010627 ADDED TEST OF LN-REVOLVING TO SKIP TESTING 0 BALS.
      *             PROBLEM IF REVOLVING AND CURBAL = 0 AND PARTIAL
      *             PAYMENT OF REMAINING INTEREST, LC, OTHER, ETC.
      *             SEE PARA "ACCEPT-F4-BYPASS"
      *  BAH 010712 PASSWORD A POSITIVE (DECREASE) "RE", REGACC PR# 1478
      *  JTG 010911 CHANGED ENTRY OF 'O2' ON P&L TO STOP UPDATE OF GL MERC #260
      *  JTG 010914 'RE' ON 'IB' WILL ACT LIKE 'OT' TRANSACTION   REGACC PR#1575
      *  JTG 010921 CORRECTED 'RE' ON 'IB' WILL ACT LIKE 'OT'     REGACC PR#1575
      *  JTG 020628 ADDED RP-RATE-ALLOW-OVERRIDE                  LENDMARK #1841
      *  CS  021119 IF BR-PASSWORD-DRAWER = "Y" AND POSTING INTERBRANCH, WAS
      *             USING THE WRONG CASH DRAWER FILE TO VERIFY.  ADDED CA-PATH
      *             LOGIC IN ACCEPT-ENTRY-ENTER-TILLNO
      *  CS  021119 MODIFIED PASSWORD FOR INTERBRANCH POSTING OF "OT" AND AP
      *             WORLD [PR#310]
      *  BLV 030325 FIXED TO CHECK CASH DRAWER PASSWORD FLAG OF POSTING BRANCH,
      *             NOT OWNING BR
      *  JTG 030702 ACCOUNTS BYPASS LATE CHARGE ASSESS ROUTINE WHEN
      *             SP-LCFRMLA = ' ' BUT ACCOUNT IS SETUP FOR DEALER LATE
      *             CHARGE FORMULA                                  MIDATL #J029
      *   CS 081113 ADD TEST GP-INCL-OTH-IN-EARNED = 'R'. IF 'R' EXCLUDE
      *             OTHER(ANTICERN(6) AND MAINTFEE(ANTICERN(4) FROM EARNED
      *             UNEARNED   REGACC PR#210
      *   CS 081211 ADD PASSWORD PROTECTION ON INTERBRANCH "WO" AND "W2"
      *             PASSWORD MUST EXIST                        REGACC PR#231
      *  BAH 100415 ADDED TEST OF SP-LC-ON-JUDGEMENTS FOR THE "AL", IT WAS
      *             NOT COMPUTING ANY LATE CHARGES FOR THE "AL" IF ACCOUNT
      *             HAD A JUDGEMENT DATE, REGACC PL# 682
      *   CS 100421 DON'T ALLOW -0- AMOUNTS FOR OP,AP,WL,WI,WO,OT,W2,O2,RE
      *                                                 BAY COUNTRY PL#685
      *  BAH 100622 UNDID ABOVE CHANGE FOR NOW...CANT GET PAST APINT FOR AP
      *   CS 100623 CORRECT 'DONT ALLOW -0- PAYMENTS', WHEN AP WAS NOT
      *             ALLOWING 0 IN LATE/INT   PR#685
      *   CS 101006 "AP" DOESNT LET YOU PAST OT AMOUNT IF YOU ARE TRYING
      *             TO AP OWING INT OR LC ORIGINAL REQ PL#685
      *  BAH 130111 ADDED LC-DISPLAY-ERN OPTION OF "I" FOR POFF-INTDUE
      *                                              AMERICAN ACC PR# 4190
      * BAH 140814 ADDED TEST FOR LPSMNU IN HELP
      * BLM 141029 FIX ENTRY OF PO CODE, USE C$JUSTIFY INSTEAD OF MESSING WITH
      *            VDUPROCESS.  
      *  CS 150501 IF F4 AND SETUP-FOR-RVBAL, VDU-FORM-NAME NEEDS RESET TO
      *            LONPF, I WAS GETTING 'MISSING FIELDS'
      *  BAH 160128 SPLIT OUT LTFILE, PD0003
      *   CS 160222 REMOVE COPY LPWKLXE, ITS AN EARNINGS COPY MEMBER THAT IS
      *             PASSED IN LOAN PROCESSING VIA LP01WK USING WK-LX2-REC. 
      *             REPLACED WK-LX2-REC WITH WK-LXE-REC IN LP01WK 
      *             MOVED LPWKLXE TO LPWKLXE.NO
      *             SAVED VERSION LONPF7.160222
      *  BAH 160304 CHANGED TILLNO TO NUMERIC BUT NO ROOM ON SCREEN FOR 2 DIGITS
      *
      * KEC 2016.0325 {PD#00003} PARADATA <A30> [JAY CISZEWSKI]
      *         CHANGED CDSCAN-WINDOW-BUF TO HAVE MATCHING CD1-KEY
      *         LAYOUTS (FOR EACH CODE TYPE; CD-TYPE).
      *         CDSCAN-CD1-KEY IS THE NEW (PRIMARY) WITH REDEFINES OF
      *         CDSCAN-??-KEY (NEW).
      * BAH 161117 ADDED 2 DIGIT FIELD "DRAWER" ABOVE PAYMENT LINE FOR
      *            NEW CASH DRAWER ENTRY
      *  BLM 160728 ADDED LOGIC FOR BATCH POST OF "RE", REGACC PR# 382
      *   CS 170214 ADDED LOGIC FOR BATCH POST OF "O2", SOUTHERN AUTO PR5239
      *  BAH 170615 ACTIVATE 8 DIGIT DATES, PD0006
      *  BLM 170609 PERFORM CONDITIONAL-SEND-MESS, ONLY DO SEND-MESS IF
      *             ROUTE-BUF = "00", WORLD PR# 1246
      * BAH 171129 FIXED WK-LXE-REC
      * BAH 181227 GLOBAL LPFILE
      * BAH 190116 GLOBAL LXFILE
      * KEC 190219 A32 FFR - REMOVED REDEFINE FOR LN-LC-CODEX; UNUSED
      * KEC 20190717 COMMENTED OUT THE USE OF REVOL-SIGN-WORKERS.
      * BAH 20200707 REMOVED CLOSE-BR-FILE #TST001
      * BAH 2020.0720 TEST GP-NO-GL-ON-O2-WHEN-PL FOR "OP", DONT ALLOW
      *               GL # TO BE ENTERED                        #1453
      * BAH 2021.0202 DO NOT ALLOW CD-BR-FILE-TYPE "C", UMC DEBIT CARD, PR#1490
      * BAH 2022.0314 REMOVED EXT-SPANISH-FORMS AND ALL MX FORMS
      * BAH 20220818 REMOVED GLOBRD, READ GB-FILE INSTEAD
      * BAH 20220823 HARD CODED START
      * JKC 2022-0930 CHANGED TO USE START-LP1-FILE-SCAN INSTEAD OF
      *          START-LP1-FILE; NEED TO USE START-LP1-FILE-SCAN WHEN
      *          START-LP1-FILE-NOT-GREATER IS USED FIRST; NEEDED FOR
      *          FOR DISPLAYING MULTIPLE PAGES IN AN UP & DOWN FUNCTION;
      *          NEED TO ADD ONE TO LP-SEQNO (IN WBEG) IN ORDER TO GET 
      *          THE DISPLAY TO WORK LIKE WITH VISION FILES.
      * BAH 2024.0430 FIXED PASSWORD WAIVER-SEQ, MISSING ELSE ON AN IF
      **************************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
      * USED FOR RECORD AREA ONLY
       COPY "LIBLP/LPFSWK.CPY".
       I-O-CONTROL.
       DATA DIVISION.
       FILE SECTION.
       COPY "LIBLP/LPFDWK.CPY".

       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)LP/LONPF7.CBL S35 04/19/23-08:07:43 >".
      **************************************************
      *     P R O G R A M   W O R K E R S              *
      **************************************************
       COPY "LIBLP/LP01LN.CPY".
       COPY "LIBLP/LP01LP.CPY".
       COPY "LIBLP/LP01LP_SQL.CPY".
       COPY "LIBLP/LPWSLP.CPY".
      * NEEDED FOR SAVING TO WKFILE
       COPY "LIBLP/LP01LX.CPY".
       COPY "LIBLP/LP01LXG.CPY".
       COPY "LIBLP/LP01LXE.CPY".
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
       COPY "LIBGB/GB01GB.CPY".
       COPY "LIBGB/GB01GB_SQL.CPY".
       COPY "LIBGB/GBWSGB.CPY".
       01  WK-IFN              PIC XX       VALUE "WK".
       01  WK-KEY              PIC 9(2)     VALUE 0.

       01  ELE                 PIC 99                VALUE 0.
       01  TOTEARN             PIC S9(6)V99   COMP   VALUE 0.
       01  UNEARNED            PIC S9(6)V99   COMP   VALUE 0.
       01  LN-WK               PIC 9(6)       COMP   VALUE 0.
       01  PRIN-WORKER         PIC S9(5)V99   COMP   VALUE 0.
       01  GL-WORK             PIC S9(7)V99   COMP   VALUE 0.
       01  TEST-AMT            PIC S9(7)V99   COMP   VALUE 0.
       01  TEST-AMT2           PIC S9(7)V99   COMP   VALUE 0.
       01  DISP-REV-BAL        PIC X                 VALUE "Y".
       01  IB-RE-TRCD          PIC XX.

       01  SUB                 PIC 9(6) COMP         VALUE 0.

       01  LCBAL-EXCEEDED.
           03  FILLER                 PIC X(31) VALUE
           "EXCEEDS LATE CHARGE BALANCE OF ".
           03  LCBAL-EXCEED-AMT       PIC ZZZZ9.99.
       01  INTBAL-EXCEEDED.
           03  FILLER                 PIC X(35) VALUE
           "EXCEEDS INTEREST CHARGE BALANCE OF ".
           03  INTBAL-EXCEED-AMT       PIC ZZZZ9.99.
       01  PUR-BAL-EXCEEDED.
           03  FILLER          PIC X(20)    VALUE
           "THE PURCHASE BAL IS".
           03  PURBAL-EXCEEDED PIC ZZZZ9.99-.
      *01  PATH-LIST                   PIC X(13)
      *    VALUE "MACHINE DATA.".
      *01  PATH-BUF.
      *    03  PATH-MACHINE            PIC X(2).
      *    03  PATH-DATA               PIC X(6).

       01  SV-PASSWORD-DRAWER          PIC X VALUE "N".
       01  GLNO                PIC 9(11)            VALUE 0.
       01  FILLER              REDEFINES GLNO.
           03  GLNO-BR         PIC 9(4).
           03  GLNO-NUMBER     PIC 9(7).

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
           03  L-050           PIC Z(4).99- BLANK ZERO.
           03  L-060           PIC Z(4).99- BLANK ZERO.
           03  L-070           PIC Z(5).99- BLANK ZERO.
           03  L-080           PIC 9(4)     BLANK ZERO.
           03  L-090           PIC Z(5).99- BLANK ZERO.
           03  L-100           PIC Z(4).99- BLANK ZERO.
           03  L-110           PIC Z(5)9.99.
           03  L-120           PIC X.
       01  EARN-BUF            REDEFINES LN-BUF.
           03  D-INTYTD        PIC ZZZZ9.99-.
           03  D-TOTEARN       PIC ZZZZ9.99-.
           03  D-LCPDTH.
               05 D-LCPDTH-MO   PIC 99.
               05 D-LCPDTH-DAYR PIC 99.
           03  D-DEFMNTS       PIC 99.
           03  D-ALDEL         PIC 99         BLANK ZERO.
           03  D-EARNED        PIC ZZZZ9.99-.
           03  D-DLPROC        PIC ZZZZ9.99-.
      *    CANNOT REDEFINE - SEE SETUP-LINE ROUTINE
       01  ENTRY-BUF           VALUE " ".
           03  EB-010          PIC 9(6).
           03  EB-020          PIC XX.
           03  EB-030          PIC X(5).
           03  EB-040          PIC Z(5).99- BLANK ZERO.
           03  EB-050          PIC Z(4).99- BLANK ZERO.
           03  EB-060          PIC Z(4).99- BLANK ZERO.
           03  EB-070          PIC Z(5).99- BLANK ZERO.
           03  EB-080          PIC 9(4)     BLANK ZERO.
           03  FILLER          REDEFINES EB-080.
               05 EB-080-MO    PIC 99.
               05 EB-080-DAYR  PIC 99.
           03  EB-090          PIC Z(5).99- BLANK ZERO.
           03  EB-100          PIC Z(4).99- BLANK ZERO.
           03  EB-110          PIC Z(5)9.99.
           03  EB-120          PIC X.

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

      *-----------------------------------------------------------------
      *COPY "LIBLP/LP01LTR.CPY".
       COPY "LIBLP/LP01LTI.CPY".
       COPY "LIBGB/CAPASSWDW.CPY".
       COPY "LIBLP/LPWKPATH.CPY".
       COPY "LIBLP/LPIBPCW.CPY".
       COPY "LIBLP/LPSCHDW.CPY".
       COPY "LIBLP/LPRATEW.CPY".
       COPY "LIBGB/REBATEW.CPY".
       COPY "LIBLP/LPXDTEW.CPY".
       COPY "LIBLP/LPLCASW.CPY".
       COPY "LIBGB/SPPDTHW.CPY".
       COPY "LIBLP/LPLCHGW.CPY".
       COPY "LIBLP/LPLCAPW.CPY".
       COPY "LIBLP/LPBWINW.CPY".
       COPY "LIBLP/LPAMTSW.CPY".
       COPY "LIBLP/LPMDTEW.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/RBACTW.CPY".
       COPY "LIBLP/LPUPERW.CPY".
       COPY "LIBLP/SETCTBLW.CPY".
       COPY "LIBLP/LPUPWKW.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBLP/LPMENUW.CPY".
       COPY "LIBGB/GPENVW.CPY".

       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBCL/CLMENUW_EXT.CPY".
       COPY "LIBGL/ALLENTW.CPY".
       COPY "LIBGB/PASSWDW.CPY".
       78  WAIVER-SEQ             VALUE 0.
       78  OT-SEQ                 VALUE 1.
       78  AP-SEQ                 VALUE 2.
       78  WI-SEQ                 VALUE 3.
       78  WL-SEQ                 VALUE 4.
       78  RE-POSITIVE-SEQ        VALUE 5.
       78  OT-SEQ-INTERBRANCH     VALUE 6.
       78  AP-SEQ-INTERBRANCH     VALUE 7.
       78  WOW2-SEQ-INTERBRANCH   VALUE 8.

       COPY "LIBWI/CDSCANW.CPY".
       COPY "LIBGB/SYSTEMW.CPY".
       COPY "LIBGB/GETENVW.CPY".
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

      /*************************************************
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

           MOVE 0 TO IO-FG.
           MOVE SPACES TO ERRCD.

           PERFORM RESTORE-SCREEN.

           PERFORM GET-GPENV.

           PERFORM OPEN-SP1-FILE.
           MOVE HOLD-SP1-KEY TO SP1-KEY.
           PERFORM READ-SP1-FILE.
           PERFORM CLOSE-SP1-FILE.
           IF IO-BAD
              MOVE "INVALID STATE PARAMETER RECORD" TO MESS
              PERFORM CONDITIONAL-SEND-MESS
              MOVE "X" TO ERRCD
              GO TO END-ROUTINE.

      * ALLOW CNTRL 'N' INTERRUPT:
           MOVE "WI/MAKERS" TO VDU-CTRL-N-PROG.

      * MUST BE LN-OWNBR FOR GL DISTRIBUTION ON INTERBRANCH PAYMENTS
           MOVE LN-OWNBR TO BR-NO.
           PERFORM OPEN-BR-FILE.
           PERFORM READ-BR-FILE.
           IF IO-BAD
              MOVE "INVALID BRANCH RECORD" TO MESS
              PERFORM CONDITIONAL-SEND-MESS
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
              PERFORM CONDITIONAL-SEND-MESS
              MOVE "X" TO ERRCD
              GO TO END-ROUTINE.

           MOVE 0 TO INDU-INTEREST LCAP-APP LCAP-OWE.
           MOVE LN-LCPDTH-DATE TO LCAP-LCPDTH-DATE.

      * TEST ENVIRONMENT VARIABLE "RVLINES" TO DETERMINE WHETHER
      * OR NOT TO DISPLAY BALANCES ON REVERSALS.
           MOVE "RVLINES" TO GETENV-BUF
           PERFORM GETENV-CALL
           IF STAT = "00"
              MOVE "N" TO DISP-REV-BAL
           ELSE
              MOVE "Y" TO DISP-REV-BAL.

      * TEST FOR BATCH PAYMENT:
           IF HOLD-BP-TRCD NOT = " "
              PERFORM BATCH-PAYMENT-ROUTINE
              GO TO END-ROUTINE.

      * 'RE' ON 'IB' WILL ACT LIKE 'OT' TRANSACTION   REGACC PR#1575
      * WILL FAKE OUT PROGRAM TO THINK IT IS POSTING AN 'OT'
      
           IF LP-TRCD = "RE" AND LN-LOANTYPE = "I"
              MOVE "RE" TO IB-RE-TRCD
              MOVE "OT" TO LP-TRCD
           ELSE
              MOVE " "  TO IB-RE-TRCD.

           PERFORM SELECTION-MODULE.

           GO TO END-ROUTINE.

      *******************************************
       BATCH-PAYMENT-ROUTINE SECTION.
      *******************************************

      *    BATCH PAYMENT LOGIC  'OT'

           PERFORM SET-BAL-IBPC-LPREC.

      * COMPUTE PAID THRU DATE
           MOVE LN-1STPYDATE TO PDTH-DATE-WORK.
           PERFORM PAID-THRU-CALCULATION.
           MOVE PDTH-DATE-FULL TO LP-PDTH-DATE.
      * SET LCPDTH
           MOVE LN-LCPDTH-DATE TO LP-LCPDTH-DATE.

           IF LP-TRCD = "OT"
              ADD LP-INTDUE TO LP-OTHBAL.
           IF LP-TRCD = "RE"
              ADD LP-TRAMT   TO LP-INTDUE
              ADD LP-TRAMT   TO LP-OTHBAL
              MOVE 0 TO LP-TRAMT.
           IF LP-TRCD = "O2"
              ADD LP-TRAMT TO LP-APINTOWE
              ADD LP-TRAMT TO LP-OT2BAL
              ADD LP-TRAMT TO LP-CURBAL

              MOVE "BR"     TO CD-TYPE
              MOVE LP-REFNO TO CD-CODE
              PERFORM GET-CD
              IF IO-GOOD
                 IF (CD-BR-FILE-TYPE NOT = "LC" AND NOT = "C")
                    IF CD-BR-OT-GL-BRANCH = 0
                        MOVE LN-OWNBR TO GLNO-BR
                    ELSE
                        MOVE CD-BR-OT-GL-BRANCH TO GLNO-BR
                    END-IF
                    MOVE CD-BR-OT-GL-ACCT TO GLNO-NUMBER
                    MOVE GLNO             TO LXG-GLNO(1)
                    MOVE LP-TRAMT         TO LXG-GLAMT(1)
                    MOVE 0                TO LP-TRAMT.


       BATCH-PAYMENT-ROUTINE-EXIT.
           PERFORM SAVE-SCREEN.

      ***************************************
       SELECTION-MODULE SECTION.
      ***************************************
       INITIAL-TRCD.
           PERFORM INITIALIZE-ROUTINE.
           IF ROUTE-CD = "7"
              GO TO RET-TO-ENTRY-ROUTINE.

           IF LP-TRCD NOT = "AL" AND NOT = "AI"
              MOVE 1 TO BYPASS-AMT-ENT-FG
              GO TO AMOUNT-ENTRY.

       ENTRY-LINE-CAL.
           PERFORM ENTRY-LINE-CALCULATIONS.
           GO TO DISPLAY-ENTRY.

       AMOUNT-ENTRY.
           PERFORM AMOUNT-ENTRY-ROUTINES.
           IF ROUTE-CD = "1"
              GO TO INITIAL-TRCD.
           IF ROUTE-CD = "3"
              GO TO ENTRY-LINE-CAL.
           IF ROUTE-CD = "7"
              GO TO RET-TO-ENTRY-ROUTINE.
           IF ROUTE-CD = "8"
              GO TO RET-TO-REF-ENTRY.

       DISPLAY-ENTRY.
           PERFORM DISPLAY-ENTRY-LINE.

           IF ROUTE-CD = "7"
              GO TO RET-TO-ENTRY-ROUTINE.

           PERFORM ACCEPT-ENTRY.
           IF ROUTE-CD = "4"
              GO TO AMOUNT-ENTRY.
           IF ROUTE-CD = "7"
              GO TO RET-TO-ENTRY-ROUTINE.

           IF NOT (LP-TRCD = "OT" OR "WO" OR "AP" OR "O2" OR
                             "W2" OR "OP" OR "RE")
              ADD LCAP-APP TO LP-APINTOWE
              ADD LCAP-OWE TO LP-APINTOWE
              IF LP-TRCD = "AL"
                 MOVE 0 TO LP-INTDUE
              ELSE
                 MOVE INDU-INTEREST TO LP-INTDUE.

           MOVE "3" TO ROUTE-CD.
           MOVE 2 TO NEW-PROG-SUB.

           GO TO SELECTION-EXIT-PROG.
       RET-TO-ENTRY-ROUTINE.
           MOVE "1" TO NEW-PROG-SUB.
           GO TO SELECTION-EXIT-PROG.
       RET-TO-REF-ENTRY.
           MOVE "1" TO NEW-PROG-SUB.
       SELECTION-EXIT-PROG.
           MOVE "P" TO ERRCD.
           IF LP-TRCD = "O2" OR "W2"
              MOVE 0 TO LP-APCUR LP-APOT2 LP-INTDUE.
           PERFORM SAVE-SCREEN.
       SELECTION-MODULE-EXIT.
           EXIT.

      *************************************
       SET-BAL-IBPC-LPREC SECTION.
      *************************************
           MOVE LN-OTHBAL TO LP-OTHBAL.
           MOVE LN-OT2BAL TO LP-OT2BAL.
           MOVE LN-INTBAL TO LP-INTBAL.
           MOVE LN-LCBAL TO LP-LCBAL.
           MOVE LN-CURBAL TO LP-CURBAL.

           MOVE LP-PAYDATE TO IBPC-DATE.
           PERFORM IBPC-TEST.
           MOVE IBPC-FG TO LP-IBPC.

      ******************************************
       INITIALIZE-ROUTINE SECTION.
      ******************************************
           PERFORM SET-BAL-IBPC-LPREC.

      * NEED TO ALLOW "WI" FOR PC LOANS BECAUSE OF SPLIT BALANCES
           IF LP-TRCD = "AI"
              IF LP-IBPC = "P"
                 MOVE "INTEREST BEARING LOANS ONLY" TO MESS
                 PERFORM CONDITIONAL-SEND-MESS
                 MOVE "7" TO ROUTE-CD
                 GO TO INITIALIZE-ROUTINE-EXIT.

      * NEED TO ALLOW "WL" FOR IB LOANS THAT WERE PC WITH LCBAL
           IF LP-TRCD = "AL"
              IF ( SP-LCFRMLA = " "          ) AND
                 ( LN-LC-CODE = ZEROES       ) 
                 MOVE "NO LATE CHARGE FORMULA FOR THIS CLASS" TO MESS
                 PERFORM CONDITIONAL-SEND-MESS
                 MOVE "7" TO ROUTE-CD
                 GO TO INITIALIZE-ROUTINE-EXIT.

      * IF "WL"  GET PASSWORD, IF NEEDED:
           IF LP-TRCD = "WL"
              MOVE WL-SEQ TO PASSWDW-SEQ
              MOVE "TO ALLOW WAIVERS..........:" TO PASSWDW-PROMPT2
              PERFORM VERIFY-PASSWORD
              IF PASSWDW-STAT NOT = "OK"
                 MOVE "WAIVERS ARE NOT ALLOWED WITHOUT PASSWORD"
                                                           TO MESS
                 PERFORM CONDITIONAL-SEND-MESS
                 MOVE "7" TO ROUTE-CD
                 GO TO INITIALIZE-ROUTINE-EXIT.
      * IF "WI" GET PASSWORD, IF NEEDED:
           IF LP-TRCD = "WI"
              MOVE WI-SEQ TO PASSWDW-SEQ
              MOVE "TO ALLOW WAIVERS..........:" TO PASSWDW-PROMPT2
              PERFORM VERIFY-PASSWORD
              IF PASSWDW-STAT NOT = "OK"
                 MOVE "WAIVERS ARE NOT ALLOWED WITHOUT PASSWORD"
                                                           TO MESS
                 PERFORM CONDITIONAL-SEND-MESS
                 MOVE "7" TO ROUTE-CD
                 GO TO INITIALIZE-ROUTINE-EXIT.
      * IF "WO" OR "W2", GET PASSWORD, IF NEEDED:
           IF LP-TRCD = "OT"
              IF PATH-MACHINE = " "
                 MOVE OT-SEQ TO PASSWDW-SEQ
              ELSE
                 MOVE OT-SEQ-INTERBRANCH TO PASSWDW-SEQ.

           IF LP-TRCD = "WO" OR "W2"
              IF PATH-MACHINE NOT = SPACES
                 MOVE WOW2-SEQ-INTERBRANCH TO PASSWDW-SEQ
                 MOVE "TO POST 'WO OR W2' INTERBR" TO PASSWDW-PROMPT2
                 PERFORM PASSWORD-MUST-EXIST
                 IF PASSWDW-STAT NOT = "OK"
                    MOVE "OPTION NOT ALLOWED INTERBRANCH" TO MESS
                    PERFORM CONDITIONAL-SEND-MESS
                    MOVE "7" TO ROUTE-CD
                    GO TO INITIALIZE-ROUTINE-EXIT
                 ELSE
                    NEXT SENTENCE
              ELSE
                 MOVE WAIVER-SEQ TO PASSWDW-SEQ
                 MOVE "TO ALLOW WAIVERS..........:" TO PASSWDW-PROMPT2
                 PERFORM VERIFY-PASSWORD
                 IF PASSWDW-STAT NOT = "OK"
                    MOVE "WAIVERS ARE NOT ALLOWED WITHOUT PASSWORD"
                                                           TO MESS
                    PERFORM CONDITIONAL-SEND-MESS
                    MOVE "7" TO ROUTE-CD
                    GO TO INITIALIZE-ROUTINE-EXIT.

      * IF "OT"  GET PASSWORD:
           IF LP-TRCD = "OT"
              IF PATH-MACHINE = " "
                 MOVE OT-SEQ TO PASSWDW-SEQ
              ELSE
                 MOVE OT-SEQ-INTERBRANCH TO PASSWDW-SEQ
              END-IF
              MOVE "TO ASSESS OTHER CHARGES" TO PASSWDW-PROMPT2
              PERFORM VERIFY-PASSWORD
              IF PASSWDW-STAT NOT = "OK"
                 MOVE "CANNOT ASSESS OTHER CHARGES WITHOUT PASSWORD"
                                                           TO MESS
                 PERFORM CONDITIONAL-SEND-MESS
                 MOVE "7" TO ROUTE-CD
                 GO TO INITIALIZE-ROUTINE-EXIT.

      * "AP" INTERBRANCH POSTING
           IF LP-TRCD = "AP"
              IF PATH-MACHINE NOT = " "
                 MOVE AP-SEQ-INTERBRANCH TO PASSWDW-SEQ
                 MOVE "TO ADJUST PRINCIPLE INTERBR"
                                   TO PASSWDW-PROMPT2
                 PERFORM VERIFY-PASSWORD
                 IF PASSWDW-STAT NOT = "OK"
                   MOVE "CANNOT ADJUST PRINCIPLE WITHOUT PASSWORD"
                                                         TO MESS
                 PERFORM CONDITIONAL-SEND-MESS
                 MOVE "7" TO ROUTE-CD
                 GO TO INITIALIZE-ROUTINE-EXIT.


      * IF "AP" OR "OP" OR "RE" GET PASSWORD:
           IF LP-TRCD = "AP" OR "OP" OR "RE"
              IF PATH-MACHINE = " "
              MOVE AP-SEQ TO PASSWDW-SEQ
              MOVE "TO ADJUST PRINCIPLE" TO PASSWDW-PROMPT2
              PERFORM VERIFY-PASSWORD
              IF PASSWDW-STAT NOT = "OK"
                 MOVE "CANNOT ADJUST PRINCIPLE WITHOUT PASSWORD"
                                                           TO MESS
                 PERFORM CONDITIONAL-SEND-MESS
                 MOVE "7" TO ROUTE-CD
                 GO TO INITIALIZE-ROUTINE-EXIT.

           MOVE "S  A060PMPT2." TO VDU.
           PERFORM SEND-INT-OR-LC.
           MOVE "S  A060PMPT4." TO VDU.
           PERFORM SEND-INT-OR-LC.

           IF ENTER-AMT-FG = "A"
              MOVE 0 TO LP-APOTH LP-APINT LP-APLC LP-APOT2
            ELSE
              IF ENTER-AMT-FG = "O"
                 SUBTRACT LP-APOTH FROM LP-OTHBAL
                 MOVE 0 TO LP-APINT LP-APLC
               ELSE
                 MOVE LN-REGPYAMT TO LP-TRAMT.

           MOVE 0 TO LP-TRAMT.

           IF LP-IBPC = "P"
              AND IBPC-EXPIRED-FLAG = " "
              GO TO INITIALIZE-ROUTINE-EXIT.

           IF NOT (LP-TRCD = "AL" OR "AI" OR "WL")
              GO TO INITIALIZE-ROUTINE-EXIT.

       INITIALIZE-INTEREST.

           MOVE LP-PAYDATE TO RATE-DATE IBPC-DATE.
           MOVE MAKER-CD TO RATE-MAKERCD.
           MOVE LP-TRCD TO RATE-LPTRCD.
           PERFORM SET-PY-RATE.

       INITIALIZE-ROUTINE-EXIT.
           EXIT.

      *************************************************
       ENTRY-LINE-CALCULATIONS SECTION.
      *************************************************

           IF LP-TRCD = "AL"
      * SORRY TO MAKE THIS COMPLICATED 'IF' STATEMENT. I HAD TO ALLOW
      * THE "AL" TO COMPUTE LATE CHARGES ON JUDGEMENTS IF YOU HAVE THE
      * SPR FLAG SET TO TAKE LATE CHARGES ON JUDEGEMENTS !!
              IF (LP-IBPC = "P" OR LN-LOANTYPE = "I")
                 IF LN-JDDATE = 0 OR ( LN-JDDATE NOT = 0 AND
                                        SP-LC-ON-JUDGEMENTS NOT = "N")
                    MOVE 0 TO LCAP-TRAMT
                    MOVE LP-PAYDATE TO LCAP-PAYDATE
                    MOVE LN-1STPYDATE TO LCAP-1STPYDATE
                    MOVE LP-TRCD TO LCAP-LPTRCD
                    MOVE LP-APLC TO LCAP-LPAPLC
                    MOVE LP-APINT TO LCAP-LPAPINT
                    PERFORM LATE-CHARGE-APPLY
                    MOVE LCAP-PARTIALS TO HOLD-PARTIALS
                    MOVE LCAP-LCPAID TO HOLD-LCPAID
                    MOVE LCAP-NOLCHG TO LP-NOLCHG
                    ADD LCAP-APP TO LP-APLC
                    ADD LCAP-OWE TO LP-LCBAL
                    MOVE LCAP-LCPDTH-DATE TO LP-LCPDTH-DATE.

           IF LP-TRCD = "AI"
              IF LP-IBPC = "I"
                 COMPUTE LP-INTBAL = LP-INTBAL + INDU-INTEREST.

           COMPUTE LP-APCUR =
               LP-TRAMT - LP-APINT - LP-APOTH - LP-APOT2 - LP-APLC.
       ENTRY-LINE-EXIT.
           EXIT.

      *************************************************
       AMOUNT-ENTRY-ROUTINES SECTION.
      *************************************************
       TRANS-AMT-ENTRY.
           MOVE " " TO ROUTE-CD.

           IF BYPASS-AMT-ENT-FG NOT = 0

              IF LP-TRCD = "OP" OR "RE"
                 GO TO APPLIED-CURBAL
              ELSE
              IF LP-TRCD = "AP"
                 GO TO APPLIED-OTH-ENTRY
              ELSE
                 GO TO APPLIED-INT-LC-ENTRY.

       TRANS-AMT-ENT.
           MOVE " F1, ENTER TRANSACTION AMOUNT:" TO MESS.
           PERFORM SEND-LEGEND.

           MOVE "ENNS052040." TO VDU.
           PERFORM SCREENIO.
           IF F1-KEY
              MOVE "7" TO ROUTE-CD
              GO TO ENTER-APPLIED-EXIT.

           IF F4-KEY
              GO TO TRANS-AMT-ENTRY.

           IF UP-KEY
              MOVE "8" TO ROUTE-CD
              GO TO ENTER-APPLIED-EXIT.
           IF NOT (ENTER-KEY OR DOWN-KEY)
              GO TO TRANS-AMT-ENT.

           IF VDUNUM2 < 0
              GO TO TRANS-AMT-ENT.

           IF VDUNUM2 = 0
              IF ENTER-AMT-FG = "Y"
                 MOVE "SN S052040." TO VDU
                 MOVE LP-TRAMT TO VDUNUM2
                 PERFORM SCREENIO
                 MOVE LP-TRAMT TO VDUNUM2.

       AMOUNT-CHK-TRAMT.
           IF VDUNUM2 NOT = LP-TRAMT
              MOVE VDUNUM2 TO LP-TRAMT
              MOVE "A" TO ENTER-AMT-FG
              MOVE "1" TO ROUTE-CD
              GO TO ENTER-APPLIED-EXIT.

           MOVE VDUNUM2 TO LP-TRAMT.

           IF ENTER-AMT-FG = "Y"
              MOVE "N" TO ENTER-AMT-FG
              MOVE "3" TO ROUTE-CD
              GO TO ENTER-APPLIED-EXIT.

      *******************************************************
      *    APPLIED TO OTHER ENTRY ROUTINE
      *******************************************************
       APPLIED-OTH-ENTRY.
           IF NOT (LP-TRCD = "OT" OR "WO" OR "W2" OR "O2" OR "AP")
              GO TO VERIFY-PRIN-RTN.

           MOVE "F1, ENTER AMOUNT OF OTHER CHARGE:" TO MESS
           PERFORM SEND-LEGEND.

           MOVE "ENNS042050." TO VDU.
           PERFORM SCREENIO.
           IF F1-KEY
              MOVE "7" TO ROUTE-CD
              GO TO ENTER-APPLIED-EXIT.

           IF F4-KEY
              GO TO APPLIED-OTH-ENTRY.

           IF UP-KEY
              IF LP-TRCD = "OT" OR "WO" OR "W2" OR "O2" OR "AP"
                 GO TO APPLIED-OTH-ENTRY
              ELSE
                 GO TO TRANS-AMT-ENTRY.
           IF NOT (ENTER-KEY OR DOWN-KEY)
              GO TO APPLIED-OTH-ENTRY.

      *PR#685
           IF LP-TRCD NOT = "AP"
           IF VDUNUM2 = 0
              MOVE "0.00 NOT ALLOWED" TO MESS
              PERFORM CONDITIONAL-SEND-MESS
              GO TO APPLIED-OTH-ENTRY.


           IF VDUNUM2 < 0
              IF NOT (LP-TRCD = "WO" OR "AP" OR "W2" OR "O2" OR "OT")
                 GO TO APPLIED-OTH-ENTRY
              ELSE
                 IF LP-TRCD = "OT"
                    MOVE VDUNUM2 TO LP-APOTH
                    GO TO VERIFY-OTH-CHGS.
           IF VDUNUM2 < 0
              IF (LP-OTHBAL NOT < 0) OR
                 (VDUNUM2 < LP-OTHBAL)
                 GO TO APPLIED-OTH-ENTRY.
           IF NOT (LP-TRCD = "O2" OR "W2")
              MOVE VDUNUM2 TO LP-APOTH
           ELSE
              MOVE VDUNUM2 TO LP-APOT2.
           IF LP-TRCD = "WO" OR = "OT"
              GO TO VERIFY-OTH-CHGS
           ELSE
              IF LP-TRCD = "O2" OR "W2"
                 GO TO VERIFY-PRIN-ADJUST.

       APPLIED-INT-LC-ENTRY.
           IF LP-TRCD = "WO" OR "OT" OR "O2" OR "W2"
              GO TO APPLIED-OTH-ENTRY.

           IF NOT (LP-TRCD = "WL" OR "WI" OR "AP")
              MOVE 0 TO BYPASS-AMT-ENT-FG
              GO TO TRANS-AMT-ENTRY.

           MOVE " F1, ENTER INT/LC AMOUNT:" TO MESS.
           PERFORM SEND-LEGEND.

           MOVE "ENNS042060." TO VDU.
           PERFORM SCREENIO.
           IF F1-KEY
              MOVE "7" TO ROUTE-CD
              GO TO ENTER-APPLIED-EXIT.

           IF F4-KEY
              GO TO APPLIED-INT-LC-ENTRY.

           IF UP-KEY
              IF LP-TRCD = "WL" OR "WI"
                 GO TO APPLIED-INT-LC-ENTRY
               ELSE
                 MOVE 0 TO BYPASS-AMT-ENT-FG
                 GO TO APPLIED-OTH-ENTRY.
           IF NOT (ENTER-KEY OR DOWN-KEY)
              GO TO APPLIED-INT-LC-ENTRY.

           IF VDUNUM2 = 0
              IF LP-TRCD = "WI" OR "WL"
                 MOVE "0.00 NOT ALLOWED" TO MESS
                 PERFORM CONDITIONAL-SEND-MESS
                 GO TO APPLIED-INT-LC-ENTRY.
      *ANOTHER ATTEMPT TO BLOCK 0 AMOUNT TRANSACTIONS.
      *HOWEVER 'AP' IS FOR OTHER, INTEREST AND LATE CHARGE
            IF VDUNUM2 = 0
               IF LP-APOTH = 0
                 IF LP-TRCD = "AP"
                   MOVE "APPLIED TO OT AND LC/INT CANNOT BE 0" TO MESS
                   PERFORM CONDITIONAL-SEND-MESS
                   GO TO APPLIED-OTH-ENTRY.


           IF VDUNUM2 < 0
              IF NOT (LP-TRCD = "WL" OR "WI" OR "AP")
                 GO TO APPLIED-INT-LC-ENTRY.

           IF VDUNUM2 < 0
              COMPUTE TEST-AMT = LP-LCBAL + LP-INTBAL
              IF ( (LP-TRCD = "WL") AND
                   (LP-LCBAL NOT < 0 OR VDUNUM2 < LP-LCBAL)
                 ) OR
                 ( (LP-TRCD = "WI") AND
                   (LP-INTBAL NOT < 0 OR VDUNUM2 < LP-INTBAL)
                 ) OR
                 ( (LP-TRCD = "AP") AND
                   (TEST-AMT NOT < 0 OR VDUNUM2 < TEST-AMT)
                 )
                 GO TO APPLIED-INT-LC-ENTRY.

           IF VDUNUM2 = 0
              MOVE 0 TO LP-APLC LP-APINT
           ELSE
           IF VDUNUM2 > 0 AND (LP-TRCD NOT = "WL" AND NOT = "WI")
              IF VDUNUM2 NOT < LP-LCBAL
                 MOVE LP-LCBAL TO LP-APLC
                 COMPUTE LP-APINT = VDUNUM2 - LP-LCBAL
              ELSE
                 MOVE VDUNUM2 TO LP-APLC
           ELSE
           IF VDUNUM2 < 0 AND (LP-TRCD NOT = "WL" AND NOT = "WI")
              IF VDUNUM2 < LP-LCBAL
                 MOVE LP-LCBAL TO LP-APLC
                 COMPUTE LP-APINT = VDUNUM2 + LP-LCBAL
              ELSE
                 MOVE VDUNUM2 TO LP-APLC.

           IF LP-TRCD = "WI"
              MOVE VDUNUM2 TO LP-APINT
           ELSE
              IF LP-TRCD = "WL"
                 MOVE VDUNUM2 TO LP-APLC.

           IF LP-TRCD = "AP" OR "O2" OR "W2"
              GO TO VERIFY-PRIN-ADJUST.

           IF LP-TRCD NOT = "WL" AND NOT = "WI"
              GO TO VERIFY-PRIN-RTN.

       VERIFY-WAIVERS.
           MOVE 0 TO LP-TRAMT.
           IF LP-TRCD = "WI" AND
              (LP-APINT NOT > LP-INTBAL OR LP-APINT < 0)
              IF BYPASS-AMT-ENT-FG = 0
                 SUBTRACT LP-APINT FROM LP-INTBAL
                 GO TO ENTER-APPLIED-EXIT
              ELSE
                 GO TO VERIFY-PRIN-CONT
           ELSE
              IF LP-TRCD = "WI" AND LP-APINT > LP-INTBAL
                 PERFORM EXCEEDS-INT-BAL
              ELSE
                 IF LP-TRCD = "WL" AND
                    (LP-APLC NOT > LP-LCBAL OR LP-APLC < 0)
                    IF BYPASS-AMT-ENT-FG = 0
                       SUBTRACT LP-APLC FROM LP-LCBAL
                       GO TO ENTER-APPLIED-EXIT
                    ELSE
                       GO TO VERIFY-PRIN-CONT
                 ELSE
                    IF LP-TRCD = "WL" AND LP-APLC > LP-LCBAL
                       PERFORM EXCEEDS-LC-BAL.

           GO TO APPLIED-INT-LC-ENTRY.

       VERIFY-PRIN-ADJUST.
           MOVE LN-OTHBAL TO LP-OTHBAL.
           MOVE LN-OT2BAL TO LP-OT2BAL.
           MOVE LN-INTBAL TO LP-INTBAL.
           MOVE LN-LCBAL TO LP-LCBAL.
           MOVE LN-CURBAL TO LP-CURBAL.
           IF LP-APOTH > 0 AND LP-TRCD NOT = "O2"
              IF LP-APOTH > LP-OTHBAL
                 PERFORM EXCEEDS-OTH-BAL
                 MOVE 0 TO LP-APOTH
                 GO TO APPLIED-OTH-ENTRY.
           IF LP-APOT2 > 0 AND LP-TRCD = "W2"
              IF LP-APOT2 > LP-OT2BAL
                 PERFORM EXCEEDS-OT2-BAL
                 MOVE 0 TO LP-APOT2
                 GO TO APPLIED-OTH-ENTRY.
           IF LP-APINT > 0 OR LP-APLC > 0
              COMPUTE TEST-AMT = LP-APINT + LP-APLC
              COMPUTE TEST-AMT2 = LP-INTBAL + LP-LCBAL
              IF TEST-AMT > TEST-AMT2
                 PERFORM EXCEEDS-INT-BAL
                 MOVE 0 TO LP-APINT LP-APLC
                 GO TO APPLIED-INT-LC-ENTRY.

           IF LP-TRCD NOT = "O2" AND NOT = "W2"
              SUBTRACT LP-APOTH FROM LP-OTHBAL
           ELSE
           IF LP-TRCD = "W2"
              SUBTRACT LP-APOT2 FROM LP-OT2BAL
           ELSE
              ADD LP-APOT2 TO LP-OT2BAL.
           SUBTRACT LP-APINT FROM LP-INTBAL.
           SUBTRACT LP-APLC FROM LP-LCBAL.

           IF NOT (LP-TRCD = "WI" OR "WL" OR "W2")
              COMPUTE TEST-AMT =
                   (LP-APOTH + LP-APOT2 + LP-APLC + LP-APINT) * -1
              IF TEST-AMT > LP-CURBAL
                 PERFORM EXCEEDS-PRIN-BAL
                 MOVE "7" TO ROUTE-CD
                 GO TO ENTER-APPLIED-EXIT
              ELSE
                ADD LP-APOTH LP-APINT LP-APLC LP-APOT2
                                        TO LP-CURBAL.

           IF LP-TRCD = "W2"
              COMPUTE TEST-AMT = LP-APOT2 * (-1)
              IF TEST-AMT > LP-CURBAL
                 PERFORM EXCEEDS-PRIN-BAL
                 MOVE "7" TO ROUTE-CD
                 GO TO ENTER-APPLIED-EXIT
              ELSE
                 SUBTRACT LP-APOT2 FROM LP-CURBAL.

           IF LP-TRCD NOT = "O2" AND NOT = "W2"
              MOVE LP-APOTH TO LP-APINTOWE
           ELSE
              MOVE LP-APOT2 TO LP-APINTOWE.

           MOVE LP-APINT TO LP-INTDUE.
           MOVE LP-APLC TO LP-LCPARTIALS.
           MOVE 0 TO LP-APOTH LP-APINT LP-APLC.
           GO TO ENTER-APPLIED-EXIT.

       VERIFY-OTH-CHGS.
           IF NOT (LP-TRCD = "O2" OR "W2")
              MOVE LP-APOTH TO LP-INTDUE
              MOVE LN-OTHBAL TO LP-OTHBAL
           ELSE
              MOVE LP-APOT2 TO LP-INTDUE
              MOVE LN-OT2BAL TO LP-OT2BAL.

           IF LP-TRCD = "OT"
              ADD LP-APOTH TO LP-OTHBAL
              MOVE 0 TO LP-APOTH
              GO TO ENTER-APPLIED-EXIT
           ELSE
              IF LP-TRCD = "O2"
                 ADD LP-APOT2 TO LP-OT2BAL
                 GO TO ENTER-APPLIED-EXIT.

           IF LP-APOTH > 0 AND LP-TRCD NOT = "O2"
              IF LP-APOTH > LP-OTHBAL
                 PERFORM EXCEEDS-OTH-BAL
                 GO TO APPLIED-OTH-ENTRY.
           IF LP-TRCD NOT = "O2"
              SUBTRACT LP-APOTH FROM LP-OTHBAL
           ELSE
              ADD LP-APOT2 TO LP-OT2BAL.
           MOVE 0 TO LP-APOTH.
           GO TO ENTER-APPLIED-EXIT.

      *---------------------------------------------------------------
      *    APPLIED TO CURBAL ENTRY ROUTINE  ('OP' OR 'RE')
      *---------------------------------------------------------------
       APPLIED-CURBAL.
           IF NOT (LP-TRCD = "OP" OR "RE" OR "AP")
              GO TO VERIFY-PRIN-RTN.

           MOVE "S  A000MSEL:FR." TO VDU.
           MOVE "ENTER AMOUNT TO APPLY TO PRINCIPAL, - INCREASE:"
                                                          TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "ENNS052070." TO VDU.
           PERFORM SCREENIO.
           IF F1-KEY
              MOVE "7" TO ROUTE-CD
              GO TO ENTER-APPLIED-EXIT.
           IF NOT (ENTER-KEY OR DOWN-KEY)
              GO TO APPLIED-CURBAL.
           MOVE VDUNUM2 TO LP-APCUR.

           IF VDUNUM2 = 0
              MOVE "0.00 NOT ALLOWED" TO MESS
              PERFORM CONDITIONAL-SEND-MESS
              GO TO APPLIED-CURBAL.


           IF LP-APCUR > 0
              MOVE "THIS WILL  D E C R E A S E  THE BALANCE" TO MESS
              PERFORM CONDITIONAL-SEND-MESS
           ELSE
           IF LP-APCUR < 0
              MOVE "THIS WILL  I N C R E A S E  THE BALANCE" TO MESS
              PERFORM CONDITIONAL-SEND-MESS.


      * IF "RE" AND ENTER A POSITIVE(DECREASE PRIN) AMOUNT, GET PASSWORD
           IF LP-TRCD = "RE" AND LP-APCUR > 0
              MOVE RE-POSITIVE-SEQ TO PASSWDW-SEQ
              MOVE "TO DECREASE PRINCIPLE" TO PASSWDW-PROMPT2
              PERFORM VERIFY-PASSWORD
              IF PASSWDW-STAT NOT = "OK"
                 GO TO APPLIED-CURBAL.

           GO TO VERIFY-PRIN-CONT.
      *---------------------------------------------------------------

       VERIFY-PRIN-RTN.
           COMPUTE PRIN-WORKER =
                   LP-TRAMT - LP-APOTH - LP-APINT - LP-APLC - LP-APOT2.

           IF LP-IBPC = "P"
              COMPUTE LCAP-WRK = LCAP-APP + LCAP-OWE
              IF LP-APINT > LCAP-WRK
                 PERFORM EXCEEDS-OTH-BAL
                 GO TO APPLIED-INT-LC-ENTRY.

           IF PRIN-WORKER < 0
              MOVE "INITIATE REVERSALS WITH 'F6' OR 'RV' CODE" TO MESS
              PERFORM CONDITIONAL-SEND-MESS
              GO TO APPLIED-INT-LC-ENTRY.

           IF PRIN-WORKER > LP-CURBAL
              PERFORM EXCEEDS-PRIN-BAL
              GO TO APPLIED-INT-LC-ENTRY.

           IF LP-APOTH > LP-OTHBAL AND LP-TRCD NOT = "O2"
              PERFORM EXCEEDS-OTH-BAL
              GO TO APPLIED-OTH-ENTRY.

           IF LN-SERVACCT = "N"
              IF PRIN-WORKER > LN-PURBAL
                 PERFORM EXCEEDS-PUR-BAL
                 GO TO APPLIED-INT-LC-ENTRY.

           MOVE "SN S052070." TO VDU.
           MOVE PRIN-WORKER TO VDUNUM2 LP-APCUR.
           PERFORM SCREENIO.

       VERIFY-PRIN-CONT.
           SUBTRACT LP-APINT FROM LP-INTBAL.
           SUBTRACT LP-APLC FROM LP-LCBAL.
           IF LP-TRCD NOT = "O2"
              SUBTRACT LP-APOTH FROM LP-OTHBAL
           ELSE
              ADD LP-APOT2 TO LP-OT2BAL.

           MOVE 0 TO BYPASS-AMT-ENT-FG.

       ENTER-APPLIED-EXIT.
           IF (ROUTE-CD NOT = " ") AND (LXG-GLNO(1) NOT = 0)
              PERFORM SCREEN-DISPLAY-RESET
              PERFORM DISPLAY-ENTRY-UP.

      *************************************************
       DISPLAY-ENTRY-LINE SECTION.
      *************************************************

           SUBTRACT LP-APCUR FROM LP-CURBAL.

           COMPUTE TEST-AMT = LP-APCUR * -1.

           IF LP-CURBAL < TEST-AMT
              PERFORM EXCEEDS-PRIN-BAL
              MOVE "7" TO ROUTE-CD
              GO TO DISPLAY-ENTRY-LINE-EXIT.

           IF LP-CURBAL = 0
              PERFORM EQUALS-PRIN-BAL
              MOVE "7" TO ROUTE-CD
              GO TO DISPLAY-ENTRY-LINE-EXIT.

           IF LN-SERVACCT = "N"
              IF LP-APCUR > 0
                 IF LP-APCUR > LN-PURBAL
                    PERFORM EXCEEDS-PUR-BAL
                    MOVE "7" TO ROUTE-CD
                    GO TO DISPLAY-ENTRY-LINE-EXIT.

      * COMPUTE PAID THRU DATE
           MOVE LN-1STPYDATE TO PDTH-DATE-WORK.
           PERFORM PAID-THRU-CALCULATION.
           MOVE PDTH-DATE-FULL TO LP-PDTH-DATE.

      * COMPUTE INTEREST PAID BY BORROWER
           PERFORM COMPUTE-BW-INTPAID.
           MOVE BWIN-INTPAID TO LP-INTPAID.
           ADD LP-INTPAID TO LN-YTDINTPD.

           MOVE 0 TO LP-DLPROC.

           PERFORM DISPLAY-EARNINGS.

      * 'RE' ON 'IB' WILL ACT LIKE 'OT' TRANSACTION   REGACC PR#1575
      * RESTORE 'RE' FROM FAKE OUT PROGRAM TO THINK IT IS POSTING AN 'OT'
      * ----THIS SET IS FOR INPUT LINE DISPLAY OF 'RE'
      
           IF IB-RE-TRCD = "RE"
              MOVE "RE" TO LP-TRCD.
           PERFORM SETUP-LINE.
           PERFORM SEND-ENTRY-LINE.

      * 'RE' ON 'IB' WILL ACT LIKE 'OT' TRANSACTION   REGACC PR#1575
      * RESTORE 'RE' FROM FAKE OUT PROGRAM TO THINK IT IS POSTING AN 'OT'
      * ----THIS LET THE PROGRAM ACT LIKE A 'OT'

           IF IB-RE-TRCD = "RE"
              MOVE "OT" TO LP-TRCD.


       DISPLAY-ENTRY-LINE-EXIT.
           EXIT.

      *************************************************
       ACCEPT-ENTRY SECTION.
      *************************************************

      * 3/25/03 (WORLD) HEY, WE REALLY NEED TO TEST THE PASSWORD
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

           IF LP-TRCD = "AL" OR "AI"
              MOVE "F1-CANCEL, F7-GL ENTRY, DRAWER ENTRY   :"
                                                           TO MESS
           ELSE
              MOVE "F1-CANCEL, ^-BACK, F7-GL ENTRY, DRAWER ENTRY   :"
                                                           TO MESS.

           IF SV-PASSWORD-DRAWER = "Y"
              INSPECT MESS REPLACING FIRST "ENTRY   :" BY "PASSWORD:"
           ELSE
              INSPECT MESS REPLACING FIRST "ENTRY   :" BY "NUMBER  :".
           PERFORM SEND-LEGEND.

           MOVE 0 TO CA-TILLNO.
           MOVE " " TO ROUTE-CD.

       ACCEPT-ENTRY-ENTER-TILLNO.
           MOVE LP-TRDATE TO CA-DATE.
           MOVE BRANCH    TO CA-BRNO.
           MOVE 0         TO CA-TILLNO.
           MOVE CA1-KEY TO HOLD-CA1-KEY.
           IF SV-PASSWORD-DRAWER = "Y"
      *CS 11/19/02  WHEN POSTING INTERBRANCH WE WERE NOT GETTING THE
      *   RIGHT CASH DRAWER FILE
              IF PATH-MACHINE NOT = " "
                 MOVE EXT-FILPATH-BASE TO CA-ALL-BASE
                 MOVE PATH-MACHINE TO CA-ALL-MACHINE
                 MOVE PATH-DATA TO CA-ALL-DATA
                 MOVE CA-ALL-PATH TO CA-PATH
                 MOVE "Y" TO CA-PATH-OVERRIDE
              END-IF

              PERFORM ENTER-CASH-DRAWER-PASSWORD
              IF CAPASSWD-OKAY = "N"
                 GO TO ACCEPT-ENTRY-ENTER-TILLNO
              END-IF
              IF CAPASSWD-OKAY = "V"
                 GO TO ACCEPT-ENTRY-CHECK-KEY
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

       ACCEPT-ENTRY-CHECK-KEY.

           IF F1-KEY
              PERFORM SCREEN-DISPLAY-RESET
              IF LXG-GLNO(1) NOT = 0
                 PERFORM DISPLAY-ENTRY-UP
                 GO TO ACCEPT-ENTRY-EXIT
              ELSE
                 GO TO ACCEPT-ENTRY-EXIT.

           IF F4-KEY
              GO TO ACCEPT-ENTRY.

           IF UP-KEY
              IF NOT (LP-TRCD = "AL" OR "AI")
                 GO TO ACCEPT-ENTRY-RE-ENTER.

           GO TO ACCEPT-ENTRY-BYPASS.

       ACCEPT-ENTRY-RE-ENTER.
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
           ADD LP-APINT TO LP-INTBAL.
           ADD LP-APLC TO LP-LCBAL.
           ADD LP-APOTH TO LP-OTHBAL.
           SUBTRACT LP-APOT2 FROM LP-OT2BAL LP-CURBAL.
           MOVE 1 TO BYPASS-AMT-ENT-FG.
           IF LXG-GLNO(1) NOT = 0
              PERFORM DISBURSE-OTHER-INIT
              PERFORM SCREEN-DISPLAY-RESET
              MOVE LP-REC TO WK-LP-REC
              PERFORM DISPLAY-ENTRY-UP
              MOVE WK-LP-REC TO LP-REC.
           MOVE "4" TO ROUTE-CD.
           GO TO ACCEPT-ENTRY-EXIT.

       ACCEPT-ENTRY-BYPASS.
           IF LP-CURBAL = 0
              IF NOT ( LP-INTBAL = 0 AND
                       LP-OTHBAL = 0 AND
                       LP-LCBAL  = 0 AND
                       LP-OT2BAL = 0
                     )
                 PERFORM MUST-BE-ALL-ZERO
                 GO TO ACCEPT-ENTRY.

      * RE: PR#260
      *     WHEN AN 'O2' IS POSTED TO A P&L ACCOUNT
      *     DO NOT ALLOW GL ENTRIES
      
           IF (GP-NO-GL-ON-O2-WHEN-PL = "Y") AND
              ((LP-TRCD = "O2" OR "OP") AND LN-PLCD NOT = " ")
              MOVE 0           TO GL-WORK
           ELSE
           IF (LP-TRCD = "AP" OR "O2" OR "W2")
              MOVE LP-APINTOWE TO GL-WORK
           ELSE
           IF (LP-TRCD = "OP")
              SUBTRACT LP-APCUR FROM 0 GIVING GL-WORK
           ELSE
              MOVE LP-APOTH    TO GL-WORK.

      *  GP-NO-GL-ON-O2-WHEN-PL = 'Y'
      *       WHEN AN 'O2' IS POSTED TO A P&L ACCOUNT
      *       DO NOT ALLOW GL ENTRIES
      
           IF F7-KEY
              IF ( NOT ((GP-NO-GL-ON-O2-WHEN-PL = "Y") AND
                        ((LP-TRCD = "O2" OR "OP") AND LN-PLCD NOT = " ")
                       )
                 )
                 GO TO ACCEPT-ENTRY-GL-DISBURSE.
           IF NOT ENTER-KEY
              GO TO ACCEPT-ENTRY.

           MOVE VDUNUM0 TO CA-TILLNO LP-TILLNO.

           IF GL-WORK = 0
              GO TO ACCEPT-ENTRY-CONT.
           IF LXG-GLNO(1) NOT = 0
              GO TO ACCEPT-ENTRY-CONT.
           IF CA-TILLNO NOT = 0
              PERFORM ALARM.
       ACCEPT-ENTRY-GL-DISBURSE.
           MOVE "Y" TO ALLENT-STATUS.
           PERFORM DISBURSE-OTHER-APPLIED.
           IF ALLENT-STATUS = "N"
              PERFORM DISBURSE-OTHER-INIT.
           GO TO ACCEPT-ENTRY.

       ACCEPT-ENTRY-CONT.
           IF PATH-MACHINE NOT = " "
              MOVE EXT-FILPATH-BASE TO CA-ALL-BASE
              MOVE PATH-MACHINE TO CA-ALL-MACHINE
              MOVE PATH-DATA TO CA-ALL-DATA
              MOVE CA-ALL-PATH TO CA-PATH
              MOVE "Y" TO CA-PATH-OVERRIDE.

           MOVE BRANCH    TO CA-BRNO.
           MOVE LP-TRDATE TO CA-DATE.
           PERFORM OPEN-CA1-FILE.
           PERFORM READ-CA1-FILE.
           PERFORM CLOSE-CA1-FILE.
           IF (IO-BAD) AND (NOT F1-KEY)
              MOVE "INVALID CASH DRAWER #" TO MESS
              PERFORM CONDITIONAL-SEND-MESS
              GO TO ACCEPT-ENTRY.
           IF CA-CLOSED-HHMM NOT = 0
              MOVE "CASH DRAWER CLOSED" TO MESS
              PERFORM CONDITIONAL-SEND-MESS
              GO TO ACCEPT-ENTRY.
           IF LP-CURBAL = 0
              IF LP-OTHBAL = 0
                 IF LP-OT2BAL = 0
                    IF LP-INTBAL = 0
                       IF LP-LCBAL = 0
                          MOVE "^-BACK, ENTER PAYOFF CODE:" TO MESS
                          PERFORM SEND-LEGEND
                          GO TO ACCEPT-ENTRY-PO-CODE.
           GO TO ACCEPT-ENTRY-EXIT.
       ACCEPT-ENTRY-PO-CODE.
      * DO NOT USE "RESET ON" IT WILL CLEAR CODE FROM SCAN
           MOVE "ENNA060SEL." TO VDU.
           PERFORM SCREENIO.

           IF F1-KEY OR UP-KEY
              MOVE "SNNA060SEL." TO VDU
              MOVE SPACES TO VDUBUF
              PERFORM SCREENIO
              GO TO ACCEPT-ENTRY.

           IF F6-KEY
              PERFORM CODE-SCAN
              MOVE CD-CODE TO HOLD-POCD
              GO TO ACCEPT-ENTRY-PO-CODE.

           IF NOT ENTER-KEY
              GO TO ACCEPT-ENTRY-PO-CODE.

           MOVE "PO"   TO CD-TYPE.
      * NEED TO LEFT JUSTIFY THE "SEL" FIELD 
           CALL "C$JUSTIFY" USING VDUBUF, "L".
           MOVE VDUBUF TO CD-CODE.

      * CLEAR SEL AREA OF SCREEN.
           MOVE "SNNA060SEL." TO VDU.
           MOVE SPACES TO VDUBUF.
           PERFORM SCREENIO.

           PERFORM GET-CD.
           IF IO-BAD
              MOVE "INVALID CODE" TO MESS
              PERFORM CONDITIONAL-SEND-MESS
              GO TO ACCEPT-ENTRY-PO-CODE.

           MOVE CD-CODE TO HOLD-POCD.

       ACCEPT-ENTRY-EXIT.
           IF ROUTE-CD NOT = " "
              PERFORM CLEAR-LEGEND.

      ********************************************
       SCREEN-DISPLAY-RESET SECTION.
      ********************************************
           MOVE "Y" TO FULL-PAGE-FG.
           MOVE LN-OWNBR  TO LP-BRNO.
           MOVE LN-ACCTNO TO LP-ACCTNO.
           MOVE 99999     TO LP-SEQNO.
           MOVE LP1-KEY   TO HOLD2-KEY-UP.
           MOVE "7" TO ROUTE-CD.

      ********************************************
       DISBURSE-OTHER-INIT SECTION.
      ********************************************
           MOVE 0 TO LXG-GLNO(1)  LXG-GLNO(2)  LXG-GLNO(3)
                     LXG-GLAMT(1) LXG-GLAMT(2) LXG-GLAMT(3).

      ********************************************
       DISBURSE-OTHER-APPLIED SECTION.
      ********************************************

           MOVE GL-WORK TO ALLENT-DISBURSE.
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
           IF LP-TRCD NOT = "W2"
              IF GL-WORK = VDUNUM2
                 GO TO DISBURSE-OTHER-EXIT.
           IF LP-TRCD = "W2"
              IF GL-WORK * (-1) = VDUNUM2
                 GO TO DISBURSE-OTHER-EXIT.
           MOVE "OUT-OF-BALANCE ENTRY" TO MESS.
           PERFORM CONDITIONAL-SEND-MESS.
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

      * ***NOTE*** 'START-LP1-FILE-SCAN' MUST BE USED WHEN FIRST USING
      * 'START-LP1-FILE-NOT-GREATER'; USED WHEN DISPLAYING MULTIPLE
      * PAGES IN AN UP & DOWN FUNCTION; YOU NEED TO ADD ONE TO 
      * LP-SEQNO (IN WBEG) IN ORDER TO GET THE DISPLAY TO WORK
      * LIKE VISION FILES.
      
           MOVE LP1-KEY TO HOLD2-KEY-UP.
           PERFORM OPEN-LP1-FILE.
           PERFORM READ-LP1-FILE.
           IF IO-BAD
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
           IF IO-GOOD
              IF (LP-ACCTNO NOT = LN-ACCTNO) OR
                   (LP-PATH-OWNBR NOT = LP-BRNO)
                 ADD 1 TO ENTRY-FLAG
                 GO TO DISPLAY-NEXT-DOWN.
       DISPLAY-BYPASS-DOWN.
           ADD 1 TO LN-WK.
           IF IO-GOOD
              PERFORM SETUP-LINE.
           MOVE LN-BUF TO D-LINE(LN-WK).
           MOVE SPACE TO LN-BUF.
           IF LN-WK = 8
              GO TO DISPLAY-XFER-DOWN.
           IF IO-GOOD
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
           IF IO-GOOD
              MOVE "Y" TO FULL-PAGE-FG
              MOVE LP1-KEY TO HOLD1-KEY-DOWN
           ELSE
              MOVE LN-OWNBR  TO LP-BRNO
              MOVE LN-ACCTNO TO LP-ACCTNO
              MOVE 99999     TO LP-SEQNO
              MOVE LP1-KEY TO HOLD1-KEY-DOWN.
           PERFORM CLOSE-LP1-FILE.

      ******************************************
       DISPLAY-ENTRY-UP SECTION.
      ******************************************
           MOVE SPACES TO LINE-BUF LN-BUF.
           MOVE 9 TO LN-WK.
           MOVE HOLD2-KEY-UP TO LP1-KEY.
           IF LP-SEQNO = 0
              MOVE 9 TO IO-FG
              GO TO DISPLAY-UP-EXIT.

           MOVE LP1-KEY TO HOLD1-KEY-DOWN.
           PERFORM OPEN-LP1-FILE.

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
           IF IO-GOOD
              IF (LP-ACCTNO NOT = LN-ACCTNO) OR
                   (LP-PATH-OWNBR NOT = LP-BRNO)
                 ADD 1 TO ENTRY-FLAG
                 GO TO DISPLAY-NEXT-UP.
       DISPLAY-BYPASS-UP.
           SUBTRACT 1 FROM LN-WK.
           IF LN-WK < 1
              MOVE "Y" TO FULL-PAGE-FG
              GO TO DISPLAY-XFER-UP.
           IF IO-GOOD
              PERFORM SETUP-LINE.
           MOVE LN-BUF TO D-LINE(LN-WK).
           MOVE SPACE TO LN-BUF.
           IF IO-GOOD
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
           IF IO-GOOD
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
           MOVE LP-TRAMT TO L-040 EB-040.
           IF (LP-TRCD = "AP") 
              MOVE LP-APINTOWE TO L-050 EB-050
              ADD LP-INTDUE LP-LCPARTIALS GIVING L-060 EB-060
              COMPUTE L-070 = 0 - LP-APINTOWE
                                - LP-INTDUE
                                - LP-LCPARTIALS
              COMPUTE EB-070 = 0 - LP-APINTOWE
                                 - LP-INTDUE
                                 - LP-LCPARTIALS
           ELSE
           IF LP-TRCD="O2" OR "W2"
              MOVE LP-APOT2 TO L-050 EB-050
              ADD LP-APINT LP-APLC GIVING L-060 EB-060
              MOVE LP-APCUR TO L-070
              COMPUTE EB-070 = LP-APOT2 * (-1)
           ELSE
           IF (  (LP-TRCD = "PB" OR "RN" OR "SC" OR "RB" OR "RO")
                 OR ( (LP-TRCD = "RV") AND
                         (LP-REVTRCD = "PB" OR "RN" OR "SC"
                                            OR "RB" OR "RO")
                    )
              )  AND (LP-APOT2 NOT = 0)
              ADD LP-APOTH LP-APOT2 GIVING L-050 EB-050
              INSPECT L-050 REPLACING TRAILING " " BY "*"
              INSPECT EB-050 REPLACING TRAILING " " BY "*"
              ADD LP-APINT LP-APLC GIVING L-060 EB-060
              MOVE LP-APCUR TO L-070 EB-070
           ELSE
              MOVE LP-APOTH TO L-050 EB-050
              ADD LP-APINT LP-APLC GIVING L-060 EB-060
              MOVE LP-APCUR TO L-070 EB-070.


      * 7/30/98 BAH ALWAYS DISPLAY BALANCES, #487
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
              IF LP-TRCD NOT = "O2" AND NOT = "W2"
                 MOVE LP-OTHBAL TO EB-090
              ELSE
                 MOVE LP-OT2BAL TO EB-090
              END-IF
              ADD LP-INTBAL LP-LCBAL GIVING EB-100
              MOVE LP-CURBAL TO EB-110
              MOVE EB-080 TO L-080
              COMPUTE L-090 = LP-OTHBAL + LP-OT2BAL
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

      * 'RE' ON 'IB' WILL ACT LIKE 'OT' TRANSACTION   REGACC PR#1575
      * RESTORE 'RE' FROM FAKE OUT PROGRAM TO THINK IT IS POSTING AN 'OT'
      *    ----THIS SET IS ASSURE LONPF2.C FUNCTIONS AS 'RE'

           IF IB-RE-TRCD = "RE"
              MOVE "RE" TO LP-TRCD.

           MOVE 3 TO WK-KEY.
           PERFORM OPEN-WK-FILE.
           PERFORM READ-WK-FILE.
           IF IO-BAD
              GO TO IO-ERROR.

           IF F3-KEY
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

      ****************************************
       RESTORE-SCREEN SECTION.
      ****************************************
           MOVE 3 TO WK-KEY.
           PERFORM OPEN-WK-FILE.
           PERFORM READ-WK-FILE.
           IF NEW-FORM NOT = OLD-FORM
              MOVE OLD-PROG-FORM TO WK-OLD-PROG-FORM
              MOVE NEW-PROG-FORM TO OLD-PROG-FORM.
           IF IO-GOOD
              GO TO RESTORE-CLEAR.

           MOVE SPACES TO WK-SAVE-RECORD.
           MOVE 0      TO WK-ELE.
           PERFORM WRITE-WK-FILE.
           IF IO-BAD
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

      ***************************************************
       VERIFY-PASSWORD SECTION.
      ***************************************************
           MOVE " " TO PASSWDW-STAT.
           MOVE "LONPF7" TO PASSWDW-PGM.
           MOVE "PLEASE ENTER THE PASSWORD," TO PASSWDW-PROMPT1.
           MOVE "CL/PASSWD" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH PASSWDW-BUF.
           CANCEL FORM-PROGX.

      ***************************************************
       PASSWORD-MUST-EXIST SECTION.
      ***************************************************
           MOVE "EX" TO PASSWDW-STAT.
           MOVE "LONPF7" TO PASSWDW-PGM.
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

           MOVE "SNNA060SEL."  TO VDU.
           MOVE CDSCAN-CODE TO VDUBUF.
           PERFORM SCREENIO.

           MOVE CDSCAN-CODE TO VDUBUF  CD-CODE.
           MOVE 0 TO IO-FG.

       EXIT-CODE-SCAN.
           EXIT.


      **********************************************
       SETUP-LINK-INFO SECTION.
      **********************************************
           IF VDU-FORM-NAME = "LPSMNU"
              MOVE LPSMNU-HELP--FILE TO HELP-LINK-FILE
              MOVE LPSMNU-HELP--DIR  TO HELP-LINK-DIR
           ELSE
              MOVE LONPF-HELP--FILE TO HELP-LINK-FILE
              MOVE LONPF-HELP--DIR TO HELP-LINK-DIR.

      *****************************************
       VDU-CONVERT-FIELD SECTION.
      *****************************************
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
       CONDITIONAL-SEND-MESS.
           IF EXT-ROUTE-BUF = "00"
              PERFORM SEND-MESS.
       GET-CD.
           PERFORM OPEN-CD1-FILE.
           PERFORM READ-CD1-FILE.
           PERFORM CLOSE-CD1-FILE.
       DISPLAY-EARNINGS.
           IF HOLD-DISPLAY-ERN = "A"
              COMPUTE TOTEARN =
                 HOLD-LN-ACCERN (1) + HOLD-LN-ACCERN (2) +
                 HOLD-LN-ACCERN (3) +
                 HOLD-LN-ACCERN (5)
              IF NOT( GP-INCL-OTH-IN-EARNED-N OR
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
                 COMPUTE UNEARNED =
                    UNEARNED + (LN-ANTICERN(6) - HOLD-LN-ACCERN(6))
              END-IF
              IF NOT (GP-INCL-OTH-IN-EARNED-R)
                 COMPUTE UNEARNED =
                    UNEARNED + (LN-ANTICERN(4) - HOLD-LN-ACCERN(4))
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

           MOVE LN-TOTNODEF TO D-DEFMNTS.
           MOVE LN-TOTNOALDEL TO D-ALDEL.
      * BAH 011889
           COMPUTE D-EARNED ROUNDED = LP-EARNED(1) + LP-EARNED(2)
                                    + LP-EARNED(3).
           MOVE LP-DLPROC TO D-DLPROC.
           MOVE EARN-BUF TO WR-BUF.
           MOVE EARN-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.
       TIM-DAYS-ONLY.
           MOVE 365 TO ELAPSED-YRTYPE.
           MOVE 9 TO ELAPSED-MMREM-FG.
           PERFORM TIM365.
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
           MOVE "BRING OTHBAL, INTBAL, & LCBAL TO 0 FIRST!" TO MESS.
           PERFORM CONDITIONAL-SEND-MESS.
       EXCEEDS-OTH-BAL.
           MOVE "EXCEEDS OTHER BALANCE" TO MESS.
           PERFORM CONDITIONAL-SEND-MESS.
       EXCEEDS-OT2-BAL.
           MOVE "EXCEEDS OTHER 2 BALANCE" TO MESS.
           PERFORM CONDITIONAL-SEND-MESS.
       EXCEEDS-INT-BAL.
           MOVE LN-INTBAL TO INTBAL-EXCEED-AMT.
           MOVE INTBAL-EXCEEDED TO MESS.
           PERFORM CONDITIONAL-SEND-MESS.
       EXCEEDS-LC-BAL.
           MOVE LN-LCBAL TO LCBAL-EXCEED-AMT.
           MOVE LCBAL-EXCEEDED TO MESS.
           PERFORM CONDITIONAL-SEND-MESS.
       EXCEEDS-PRIN-BAL.
           MOVE "EXCEEDS PRINCIPAL BAL" TO MESS.
           PERFORM CONDITIONAL-SEND-MESS.
       EQUALS-PRIN-BAL.
           MOVE "MUST DO PAYOFF TO BRING BALANCE TO ZERO." TO MESS.
           PERFORM CONDITIONAL-SEND-MESS.
       EXCEEDS-PUR-BAL.
           MOVE LN-PURBAL TO PURBAL-EXCEEDED.
           MOVE PUR-BAL-EXCEEDED TO MESS.
           PERFORM CONDITIONAL-SEND-MESS.
       SEND-ENTRY-LINE.
           MOVE ENTRY-BUF TO WR-BUF.
           MOVE ENTRY-LINE TO WR-LIST.
           PERFORM CALL-WRFORM.

       COPY "LIBGB/CAPASSWD.CPY".
       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/RBACT.CPY".
       COPY "LIBLP/LPUPER.CPY".
       COPY "LIBLP/SETCTBL.CPY".
       COPY "LIBLP/LPUPWK.CPY".
       COPY "LIBLP/LPIBPC.CPY".
       COPY "LIBLP/LPSCHD.CPY".
       COPY "LIBLP/LPAMTS.CPY".
       COPY "LIBLP/LPRATE.CPY".
       COPY "LIBLP/LPXDTE.CPY".
       COPY "LIBLP/LPLCAS.CPY".
       COPY "LIBGB/SPPDTH.CPY".
       COPY "LIBLP/LPLCHG.CPY".
       COPY "LIBLP/LPLCAP.CPY".
       COPY "LIBLP/LPBWIN.CPY".
       COPY "LIBLP/LPMDTE.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/GPENV.CPY".

      ********************************************
       IO-ROUTINES SECTION.
      ********************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBLP/LPSPGS_SQL.CPY".
       COPY "LIBLP/LPLTPGS_SQL.CPY".
       COPY "LIBLP/LPLPGS_SQL.CPY".
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

       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO ERRCD.

       END-ROUTINE.
           PERFORM CLOSE-FILES.
       EXIT-PROG.
       STOP-RUN.
           EXIT PROGRAM.
