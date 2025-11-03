       IDENTIFICATION DIVISION.
       PROGRAM-ID.      LONPF9.
       DATE-WRITTEN.    031386.
      *************************************************************
      *  DESC:  LOAN PROCESSING
      *         TRANSACTION POSTING
      *         PROGRAM 9 -     TRCD'S  JD,PL,PI,PW,
      *                                 P2,P3,P4,P5,RP,IR,AC
      *                                 BK,BD
      * REV:
      *  053095 SLC COMBINED OTHBAL AND OT2BAL IN L-090
      *  060695 BLL CHANGED TO USE NEW GL ENTRY ROUTINE
      *  MJD 091895 ADDED DS-DATE LOGIC
      *  MJD 030596 ADDED PAYOFF CODE SCAN.
      *  JTG 043096 ADDED LOGIC TO UPDATE HOLD-AFPDLLP
      *  MJD 071196 ADDED TEST OF LN-UNITPER-CD FOR WEEKLY PDTH.
      *  BAH 081596 WAS NOT ALLOWING 'Y' OR 'N' FOR ACCEPTING NEGATIVE
      *             DEALER RESERVE
      *  JTG 101196 ADDED 'LXE-WORKER' & 'LXE-WORKER2' FOR LPCERN-- A50 VERSION
      *                   SP-ERNFRMLA 15 & 16, REAL INTEREST BEARINGING METHOD
      *  GLF 050797 ADDED COPY MEMBERS: SETCTBLW AND SETCTBL
      *  GLF 101497 ALLOWED CTRL 'N'
      *  MJD 101797 MOVED WK-UPDATE-BUF TO LP01WK
      *  BLV 110397 MOVE IN BR-GL-TYPE-ALLOWED, BR-GL-ENTRY-DEFAULT TO
      *             ALLENT-BUF BEFORE GL DISTRIBUTION WINDOW
      *  BAH 060398 ADDED BR-AUTO-PL-REF-AC TO AUTOMATICALLY REBATE EVERYTHING
      *             AND ACCELERATE WHEN A "PL" POSTED, MERCURY #E 180
      *  MJD 060498 EXPANDED LP-REC TO 239 BYTES
      *  BAH 060898 ADDED TRCD "BK" FOR BANKRUPT STATUS, MERCURY #E 174
      *  BAH 062698 TILLNO IS NOW ALPHANUMERIC
      *  BAH 070898 ADDED TRCD "BD" TO REMOVE BANKRUPT STATUS, MERCURY # 195
      *  BAH 073098 DISPLAY BALANCES EVEN ON REVERSALS, #487
      *  JTG 073098 CORRECTED POSTING OF 'JD' OR 'AC' ON P&L ACCOUNTS THAT
      *             CAUSED BOGUE EARNINGS, WORLD
      *  MJD 083398 ADDED ENVIRONMENT VARIABLE TO DETERMINE WHETHER OR NOT
      *             TO DISPLAY BALANCES ON REVERSALS. FIXED DISPLAY TO INDICATE
      *             INCLUDED O2 BAL WITH "SC" "RN" "PB" "RB" "RO" TRANSACTIONS
      *  JTG 092998 CORRECTED BR-AUTO-PL-REF-AC TO AUTOMATICALLY REBATE
      *             EVERYTHING TO INSURE BALANCE REMAINS ABOVE ZERO & OT2
      *  JTG 101298 ALLOWED DISPLAY OF NEGATIVE CURBAL REVOL REGENCY
      *  JTG 102998 CORRECTED BR-AUTO-PL-REF-AC TO BE CONSISTENT, IT WAS
      *             YIELDING 'RI' VIA "PO" IN CRNOR2.C AND "RI" IN LONPFB.C.
      *             LONPFB.C NOW YIELDS 'RI' WITH "PO. THIS BUG WAS CAUSING
      *             AN INCORRECT LNAMT & BAD P&L EXPENSE.
      *  JTG 990408 CHANGED TO PASS LN-OWNBR TO GL/ALLENT.C FOR INTERBRANCH
      *             PAYMENTS:
      *  JTG 990412 ALLOWED 'BK' AND 'BD' FOR REVOLVING REGENCY #XXX
      *  GLB 990720 ADD CASH DRAWER PASSWORD OPTION TO ENTER DRAWER NO.
      *             BR-PASSWORD-DRAWER(Y/N) & CA-PASSWORD (4 CHAR PASSWORD)
      *             REGMGM PR# 901
      *  MJD 990730 ADDED GPENV(W). NEEDED FOR CHANGES TO LPEARN.
      *  MJD 990803 ADDED GP-INCL-OTH-IN-EARNED FLAG. IF "Y" THEN INCLUDE
      *             EARNINGS BUCKET 6 (OTHER) IN ACCUMULATED TOTALS
      *             OTHERWISE EXCLUDE BUCKET 6.
      *  JTG 990823 CHANGED TO DISPLAY LP-CURBAL ON ENTRY LINE EB-BUF EB-110
      *  JTG 000217 ADDED DL-DISC-CODE-NO-PL-EXP            ACE #980
      *  JTG 000228 CORRECTED DL-DISC-CODE-NO-PL-EXP  FOR BR-AUTO-PL-REF-AC
      *             ACE #980
      *  BAH 000512 ADDED 2 MORE READ-LN1-FILE IN UPDATE-AUTOMATIC-PL , MERC
      *             HAD 2 PEOPLE WITH SAME ACCOUNT UP, POSTING, AND WEIRD
      *             THINGS HAPPENED, THE "AC" DIDNT NOT TAKE PLACE BECAUSE OF
      *              "RE-INPUT YOUR CHANGES" AND THE P&L TOOK MONEY OFF THE
      *             BALANCE BECAUSE IT DIDNT HAVE THE RIGHT BALANCE.
      *  BAH 010102 ADDED MOVE OF LN-CURBAL TO LP-CURBAL AFTER AUTOMATIC-PL
      *             JUST LIKE THE "AC" DID, THE BALANCE WAS NOT COMING OUT
      *             RIGHT ON THE "PL", BUT LP-INTDUE WAS CORRECT !! PL# 310
      *             REGWAN
      *  MG  010201 REMOVED "01 WK-LXE" AND ADDED LIBLP/LPWKLXE.
      *  BAH 010612 REGACC, IF ACCOUNT IN "REPO ON HAND" OR "REPO SOLD" STATUS,
      *             FORCE THE USER TO USE THE REPO DATE AS THE EFFECTIVE
      *             DATE FOR ANY REBATES, OR ENTER PASSWORD, PR# 1478
      *             78  REPO-EFF-SEQ        VALUE 1.
      *  BAH 010831 BR-AUTO-PL-REC-AC WAS NOT CHECKING SP-ERNACCEL-CD " " TO
      *             TAKE THE REST OF EARNINGS INTO INCOME ON THE "AC".,PR# 259
      *  JTG 010904 ALLOWED IB TYPE LOANS TO ACCELERATE     JIM R. (ALL) PR#1582
      *  BAH 011004 BR-AUTO-PL-REC-AC WAS PUTTING THE EARNINGS IN THE "AC" AND
      *             THE "PL", GLNO'S WERE ALSO MUCKED !! KEVIN/MERCURY
      *  BAH 011011 BR-AUTO-PL-REF-AC WAS NOT ACCEL'ING AN "IB", THIS WAS
      *             MISSED BY THE 010904 CHANGE !!
      *  BAH 020322 BR-AUTO-PL-REF-AC WAS NOT SETTING LP-TILLNO ON THE 'PL'
      *             REGACC PL#356
      *  BAH 020626 ENTER A CHARGE OFF REASON CODE IF GP-PL-REPO-REASON = "Y"
      *             IF LS-X-REASON-CODE NOT SPACES, MOVE TO HOLD-PLREASON AND
      *             DONT ASK FOR CHARGE OFF REASON, REGACC PR# 1775
      *  JTG 030205 ADDED LOGIC TO ACTUARIAL-METHOD TO HANDLE BALLOON
      *             PAYMENTS  ADDED LIBGB/RBACT                     CITIZEN J025
      *  JTG 040825 ADDED 'RI' 'RS' 'RM' AND 'AC' TO INTERBRANCH TRANS LIST
      *             AND PASSWORD PROTECTED                           WORLD #0357
      *  BAH 050223 ADDED BR-AUTO-PL-REF-AC VALUES OF "2" AND "3"
      *            "2" = PL OR P2 WILL AUTO REFUND,  "3" = PL OR P2 OR P3
      *            WILL AUTO REFUND, LENDMARK PR# 2473
      *  BLF 060420 PERFORM ENTRY OF REASON CODE IF GP-PL-REPO-REASON = Y OR P
      *             (USED TO ONLY DO THIS IS FLAG WAS "Y"), LENDMARK PR# 2770
      *   CS 061127 ADDED LOGIC TO GET THE RIGHT CASH DRAWER FILE WHEN POSTING
      *             INTERBRANCH.... PR#505 WORLD
      *   CS 081113 ADD TEST GP-INCL-OTH-IN-EARNED = 'R'. IF 'R' EXCLUDE
      *             OTHER(ANTIC(6) AND MAINTFEE(ANTIC(4) FROM EARNINGS
      *                                                  REGACC PR#210
      *  BAH 111215 LP-PRORATED-INS-REB WAS NOT GETTING SET FOR INS-TAX-RB AMOUNTS
      *             ON THE REBATES CREATED FROM THE P&L, REGENCY PL# 766
      *  BAH 130111 ADDED LC-DISPLAY-ERN OPTION OF "I" FOR POFF-INTDUE
      *                                              AMERICAN ACC PR# 4190
      *  BAH 140814 ADDED TEST FOR LPSMNU IN HELP
      *  BAH 140926 MOVE "F" TO FULL-DISPLAY INSTEAD OF "P", AUTOMOTIVE PL#860
      *  BLM 141029 FIX ENTRY OF P&L REASON CODE, WASN'T WORKING WITH ONE
      *             CHARACTER CODE, USE C$JUSTIFY INSTEAD OF MESSING WITH
      *             VDUPROCESS.  ALSO FIXED PO REASON CODE
      *  BAH 160125 SPLIT OUT LTFILE, PD0003
      *  BAH 160209 SPLIT OUT LSFILE, PD0003
      *   CS 160222 REMOVE COPY LPWKLXE, ITS AN EARNINGS COPY MEMBER THAT IS
      *             PASSED IN LOAN PROCESSING VIA LP01WK USING WK-LX2-REC. 
      *             REPLACED WK-LX2-REC WITH WK-LXE-REC IN LP01WK 
      *             MOVED LPWKLXE TO LPWKLXE.NO
      *             SAVED VERSION LONPF9.160222
      *  BAH 160304 CHANGED TILLNO TO NUMERIC BUT NO ROOM ON SCREEN FOR 2 DIGITS
      *
      * KEC 2016.0324 {PD#00003} PARADATA <A30> [JAY CISZEWSKI]
      *         CHANGED CDSCAN-WINDOW-BUF TO HAVE MATCHING CD1-KEY
      *         LAYOUTS (FOR EACH CODE TYPE; CD-TYPE).
      *         CDSCAN-CD1-KEY IS THE NEW (PRIMARY) WITH REDEFINES OF
      *         CDSCAN-??-KEY (NEW).
      * BLM 160513 FIX DS-DATE FOR NEW FORMAT, PD0003
      * BAH 161117 ADDED 2 DIGIT FIELD "DRAWER" ABOVE PAYMENT LINE FOR 
      *            NEW CASH DRAWER ENTRY
      * KEC 2017.0105 {PR#00000} PARADATA <A30> [JAY CISZEWSKI]
      *          CHANGED DS-DATE TO DS-DATE-CCYYMM FOR CONSISTENCY
      *          WITH THE DATE FIELDS IN BY & UD FILE.
      *
      * BAH 170615 ACTIVATE 8 DIGIT DATES, PD0006
      * BAH 171129 FIXED WK-LXE-REC
      * BAH 181231 GLOBAL LPFILE
      * BAH 190116 GLOBAL LXFILE
      * BAH 20190308 CHANGED LP-P4P5-TRCD & P4P5-DLRVNO TO USE LPWSLP
      *              MOVED LP-REFNO REDEFINITIONS TO LPWSLP COPY MEMBER
      * BAH 20190625 REMOVED LN-REVOLVING, PT0153
      * BAH 20190701 REMOVED U-INSOURS,AVAILABLE-CREDIT FROM LPPFBUF
      * BAH 20200707 REMOVED CLOSE-BR-FILE #TST001
      * BAH 2022.0314 REMOVED EXT-SPANISH-FORMS AND ALL MX FORMS
      *   CS 220322 ADDED BATCH PAYMENT ROUTINE FOR POSTING PL VIA BATCH.
      *             WILL NOT ALLOW BR-AUTO-PL-REF-AC  PR#1548
      * BAH 20220803 HARD CODED START
      *
      * JKC 2022-0930 CHANGED TO USE START-LP1-FILE-SCAN INSTEAD OF
      *          START-LP1-FILE; NEED TO USE START-LP1-FILE-SCAN WHEN
      *          START-LP1-FILE-NOT-GREATER IS USED FIRST; NEEDED FOR
      *          FOR DISPLAYING MULTIPLE PAGES IN AN UP & DOWN FUNCTION;
      *          NEED TO ADD ONE TO LP-SEQNO (IN WBEG) IN ORDER TO GET 
      *          THE DISPLAY TO WORK LIKE WITH VISION FILES.
      *
      * SMR 2023-0418 MODIFIED FOR CHANGES TO LX FILES
      ******************************************************************
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
           "@(#)LP/LONPF9.CBL S35 04/19/23-08:25:21 >".
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
       COPY "LIBLP/LP01LN.CPY".
       COPY "LIBLP/LP01LN_SQL.CPY".
       COPY "LIBLP/LPWSLN.CPY".
       COPY "LIBLP/LP01DL.CPY".
       COPY "LIBLP/LP01DL_SQL.CPY".
       COPY "LIBLP/LPWSDL.CPY".
       COPY "LIBLP/LP01LTP.CPY".
       COPY "LIBLP/LP01LTP_SQL.CPY".
       COPY "LIBLP/LPWSLTP.CPY".
       COPY "LIBLP/LP01LSX.CPY".
       COPY "LIBLP/LP01LSX_SQL.CPY".
       COPY "LIBLP/LPWSLSX.CPY".
       COPY "LIBLP/LP01SP.CPY".
       COPY "LIBLP/LP01SPA.CPY".
       COPY "LIBLP/LP01SPB.CPY".
       COPY "LIBLP/LP01SPC.CPY".
       COPY "LIBLP/LP01SP_SQL.CPY".
       COPY "LIBLP/LPWSSP.CPY".
       COPY "LIBLP/LP01CA.CPY".
       COPY "LIBLP/LP01CA_SQL.CPY".
       COPY "LIBLP/LPWSCA.CPY".
       COPY "LIBLP/LP01DS.CPY".
       COPY "LIBLP/LP01DS_SQL.CPY".
       COPY "LIBLP/LPWSDS.CPY".
       COPY "LIBLP/LP01CD.CPY".
       COPY "LIBLP/LP01CD_SQL.CPY".
       COPY "LIBLP/LPWSCD.CPY".
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       01  WK-IFN              PIC XX       VALUE "WK".
       01  WK-KEY              PIC 9(2)     VALUE 0.

       01  WORKER              PIC S9(8)V99 COMP.
       01  ELE                 PIC 99               VALUE 0.
       01  TOTEARN             PIC S9(6)V99 COMP-3  VALUE 0.
       01  UNEARNED            PIC S9(6)V99 COMP-3  VALUE 0.
       01  PLEXP               PIC S9(6)V99 COMP-3  VALUE 0.
       01  LN-WK               PIC 9(6)     COMP-3  VALUE 0.
       01  LCAP-LCPDTH-DATE    PIC 9(8)     COMP-3  VALUE 0.
       01  GL-WORK             PIC S9(7)V99 COMP-3  VALUE 0.

      *01  TEST-ALPHA-TILLNO      PIC X.
      *01  NUMERIC-TILLNO         PIC 99.
      *01  NUMERIC-TILLNOX REDEFINES NUMERIC-TILLNO.
      *    03  FILLER             PIC X.
      *    03  NUMERIC-TILLNO1    PIC 9.

       01  DISP-REV-BAL        PIC X                 VALUE "Y".
       01  SLOT                PIC 9(6) COMP-3       VALUE 0.
       01  SUB                 PIC 9(6) COMP-3       VALUE 0.
       01  SUB1                PIC 9(6) COMP-3       VALUE 0.
       01  SUB2                PIC 9(6) COMP-3       VALUE 0.
       01  EARN-SUB            PIC 9(6) COMP-3       VALUE 0.
       01  SV-PASSWORD-DRAWER  PIC X                 VALUE "N".

       01  HOLD-LP-REC         PIC X(LP-SIZE) VALUE SPACES.
       01  HOLD-LX-REC         PIC X(LX-SIZE) VALUE SPACES.
       01  HOLD-LXE-REC        PIC X(LXE-SIZE) VALUE SPACES.
       01  HOLD-LXG-REC        PIC X(LXG-SIZE) VALUE SPACES.

       01  REBATE-DISP-TAB       PIC X(22)     VALUE
           "RCRARPRIRSRMRDRER1R2R3".
       01  REBATE-DISP-RED      REDEFINES REBATE-DISP-TAB.
           03  REB-TYPE          PIC XX  OCCURS 11.

       01  PUR-BAL-EXCEEDED.
           03  FILLER          PIC X(20)    VALUE
           "THE PURCHASE BAL IS".
           03  PURBAL-EXCEEDED PIC ZZZZ9.99-.

       01  DLR-TEST.
           03  FILLER          PIC X(5).
           03  DLR-ANSWER      PIC X.

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
           03  L-040RATE      REDEFINES L-040
                               PIC ZZZZ.999-.
           03  FILLER         REDEFINES L-040.
               05  L-040D      PIC X.
               05  L-040DLNO   PIC 9999.99-.
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
       01  EARN-BUF           REDEFINES LN-BUF.
           03  D-INTYTD        PIC ZZZZ9.99-.
           03  D-TOTEARN       PIC ZZZZ9.99-.
           03 D-LCPDTH-MO      PIC 99.
           03 D-LCPDTH-DAYR    PIC 99.
           03  D-EARNED        PIC ZZZZ9.99-.
           03  D-DLPROC        PIC ZZZZ9.99-.
      * CANNOT REDEFINE - SEE SETUP-LINE ROUTINE
       01  EB-BUF           VALUE " ".
           03  EB-010          PIC 9(6).
           03  EB-020          PIC XX.
           03  EB-030          PIC X(5).
           03  EB-040          PIC Z(5).99- BLANK ZERO.
           03  EB-040RATE     REDEFINES EB-040
                               PIC ZZZZ.999-.
           03  FILLER         REDEFINES EB-040.
               05  EB-040D     PIC X.
               05  EB-040DLNO  PIC 9999.99-.
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

       COPY "LIBLP/LP01LTI.CPY".
       COPY "LIBGB/CAPASSWDW.CPY".
       COPY "LIBWI/CDSCANW.CPY".
       COPY "LIBLP/PAYOFXW.CPY".
       COPY "LIBLP/LPRATEW.CPY".
       COPY "LIBGB/REBATEW.CPY".
       COPY "LIBLP/LPIBPCW.CPY".
       COPY "LIBLP/LPWKPATH.CPY".
       COPY "LIBLP/LPAMTSW.CPY".
       COPY "LIBLP/LPXDTEW.CPY".
       COPY "LIBGB/SPPDTHW.CPY".
       COPY "LIBGL/ALLENTW.CPY".
       COPY "LIBLP/LPMDTEW.CPY".
       COPY "LIBLP/LPBWINW.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/RBACTW.CPY".
       COPY "LIBLP/LPUPERW.CPY".
       COPY "LIBLP/SETCTBLW.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBLP/LPEARNW.CPY".
       COPY "LIBGB/GPENVW.CPY".
       COPY "LIBLP/LPMENUW.CPY".

       COPY "LIBCL/CRNOR2W.CPY".
       COPY "LIBLP/LPPOFFW.CPY".
       COPY "LIBLP/LP01NO.CPY".
       COPY "LIBGB/PASSWDW.CPY".
       78  REPO-EFF-SEQ              VALUE 1.

       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBCL/CLMENUW_EXT.CPY".
       COPY "LIBGB/SYSTEMW.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBLP/AFPDLLPW.CPY".
       COPY "LIBLP/LPCDELW.CPY".
       COPY "LIBLP/LPCEPPW.CPY".

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
           PERFORM CLOSE-LSX1-FILE.
           PERFORM CLOSE-LP1-FILE.
           PERFORM CLOSE-LN1-FILE.
           PERFORM CLOSE-DS1-FILE.
           PERFORM CLOSE-DL1-FILE.
           PERFORM CLOSE-CD1-FILE.
           PERFORM CLOSE-CA1-FILE.
           PERFORM CLOSE-BR-FILE.
           CLOSE WK-FILE.
       COPY "LIBGB/DECLAR2.CPY".

      **************************************************
       MAIN-PROGRAM SECTION.
      **************************************************

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

           PERFORM OPEN-LN1-FILE.

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

           PERFORM GET-GPENV.

           MOVE BRANCH TO BR-NO.
           PERFORM OPEN-BR-FILE.
           PERFORM READ-BR-FILE.
           IF IO-FG NOT = 0
              MOVE "INVALID BRANCH RECORD" TO MESS
              PERFORM SEND-MESS
              MOVE "X" TO ERRCD
              GO TO END-ROUTINE.

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

           IF HOLD-BP-TRCD NOT = " "
              PERFORM BATCH-PAYMENT-ROUTINE
           ELSE
              PERFORM SELECTION-MODULE.

           GO TO END-ROUTINE.

      *********************************
       BATCH-PAYMENT-ROUTINE SECTION.
      *********************************
           MOVE LN-OTHBAL TO LP-OTHBAL.
           MOVE LN-OT2BAL TO LP-OT2BAL.
           MOVE LN-INTBAL TO LP-INTBAL.
           MOVE LN-LCBAL  TO LP-LCBAL.
           MOVE LN-CURBAL TO LP-CURBAL LP-INTDUE.
           MOVE LP-PAYDATE TO IBPC-DATE.
           PERFORM IBPC-TEST.
           MOVE IBPC-FG TO LP-IBPC.
           MOVE LN-MAKERCD(1) TO MAKER-CD.

           MOVE LP-PAYDATE TO IBPC-DATE.
           MOVE LN-1STPYDATE TO PDTH-DATE-WORK.
           PERFORM PAID-THRU-CALCULATION.
           MOVE PDTH-DATE-FULL TO LP-PDTH-DATE.
           MOVE LN-LCPDTH-DATE TO LP-LCPDTH-DATE.
           MOVE 0 TO HOLD-PRIOR-DF-POSTDATE.
       BATCH-PAYMENT-ROUTINE-EXIT.
           PERFORM SAVE-SCREEN.

      *********************************
       SELECTION-MODULE SECTION.
      *********************************

           PERFORM INITIALIZE-ROUTINE.
           IF ROUTE-CD = "7"
              GO TO RET-TO-ENTRY-ROUTINE.

           IF (LP-TRCD = "PL" AND
                           (BR-AUTO-PL-REF-AC = "Y" OR "2" OR "3"))
                  OR
              (LP-TRCD = "P2" AND (BR-AUTO-PL-REF-AC = "2" OR "3"))
                  OR
              (LP-TRCD = "P3" AND (BR-AUTO-PL-REF-AC = "3"))

              PERFORM GET-PAYOFF-REBATES
              PERFORM SET-PAYOFF-LINES
              PERFORM DISPLAY-ALL-LINES
      *  VERIFY PAYOFF WON'T CAUSE NEGATIVES
              MOVE 0 TO WORKER
              PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 13
                  ADD POFF-REBATE(SUB) TO WORKER
              END-PERFORM
      * JTG 082696 LOGIC FROM LONPFC.C:
      * WHEN OT2BAL EXISTS, INSURE AT LEAST ($0.01 + LP-OT2BAL) REMAINS
      * IN LP-CURBAL:
              IF LP-OT2BAL NOT = 0
                 IF WORKER >= (LN-CURBAL - LN-OT2BAL)
                    PERFORM REFUNDS-IN-EXCESS
                    MOVE "7" TO ROUTE-CD
                    GO TO RET-TO-ENTRY-ROUTINE
                 END-IF
              ELSE
      * JTG 082696 LOGIC FROM LONPFC.C:
      * WHEN NO OT2BAL, DON'T LET LP-PRINBAL GO NEGATIVE:
                 IF WORKER > LN-CURBAL
                    PERFORM REFUNDS-IN-EXCESS
                    MOVE "7" TO ROUTE-CD
                    GO TO RET-TO-ENTRY-ROUTINE
                 END-IF
      * NEED TO RESET TRCD
           ELSE
              PERFORM DISPLAY-ENTRY-LINE
              IF ROUTE-CD = "7"
                 GO TO RET-TO-ENTRY-ROUTINE.

      * REGACC, IF ANY REBATES, MUST USE REPO DATE AS EFFECTIVE DATE
           IF (LP-TRCD = "PL" AND
                           (BR-AUTO-PL-REF-AC = "Y" OR "2" OR "3"))
                      OR
              (LP-TRCD = "P2" AND (BR-AUTO-PL-REF-AC = "2" OR "3"))
                      OR
              (LP-TRCD = "P3" AND (BR-AUTO-PL-REF-AC = "3"))

              IF GP-REPO-CODE = "A"
                 IF LN-REPOCD = "X" OR "S"
                    IF LP-PAYDATE NOT = LN-REPODATE
      * IF REBATES
                       IF WORKER NOT = 0
                          MOVE REPO-EFF-SEQ TO PASSWDW-SEQ
                          MOVE "TO ACCEPT REPO REBATE DATE" TO
                                            PASSWDW-PROMPT2
                          PERFORM VERIFY-PASSWORD
                          IF PASSWDW-STAT NOT = "OK"
                             MOVE "MUST USE REPO DATE FOR REFUNDS"
                                          TO MESS
                             PERFORM SEND-MESS
                             MOVE "7" TO ROUTE-CD
                             PERFORM ACCEPT-SCREEN-RESET
                             PERFORM DISPLAY-ENTRY-UP
                             GO TO RET-TO-ENTRY-ROUTINE.

           PERFORM ACCEPT-ENTRY-ROUTINE.
           IF ROUTE-CD = "7"
              GO TO RET-TO-ENTRY-ROUTINE.

           IF (GP-PL-REPO-REASON = "Y" OR GP-PL-REPO-REASON = "P")
              IF LP-TRCD = "PL" OR "P2" OR "P3" OR "P4" OR "P5"
                 PERFORM ENTER-PLREASON-CODE
                 IF ROUTE-CD = "7"
                    PERFORM ACCEPT-SCREEN-RESET
                    PERFORM DISPLAY-ENTRY-UP
                    GO TO RET-TO-ENTRY-ROUTINE.


      D    STOP MESS.
           IF (LP-TRCD = "PL" AND
                           (BR-AUTO-PL-REF-AC = "Y" OR "2" OR "3"))
                 OR
              (LP-TRCD = "P2" AND (BR-AUTO-PL-REF-AC = "2" OR "3"))
                 OR
              (LP-TRCD = "P3" AND (BR-AUTO-PL-REF-AC = "3"))

      * PERFORM INDIVIDUAL UPDATES OF EACH TRANSACTION
              PERFORM UPDATE-AUTOMATIC-PL
      * NEED THE ORIGINAL P&L RECORD SET BACK UP
              MOVE HOLD-LP-REC  TO WK-LP-REC LP-REC
              MOVE HOLD-LX-REC  TO WK-LX-REC LX-REC
              MOVE HOLD-LXE-REC TO WK-LXE-REC LXE-REC
              MOVE HOLD-LXG-REC TO WK-LXG-REC LXG-REC
              MOVE LN-CURBAL TO LP-INTDUE LP-CURBAL
              MOVE CA-TILLNO TO LP-TILLNO
              PERFORM SETUP-LONPF-BUFFER
              MOVE "  " TO HOLD-BP-TRCD
              IF LP-DLNO = 0
                 MOVE LN-DLNO TO DL-DLNO
              ELSE
                 MOVE LP-DLNO TO DL-DLNO
              END-IF
              PERFORM GET-DL
              IF IO-BAD
                 MOVE "PROBLEM GETTING DEALER RECORD" TO MESS
                 PERFORM SEND-MESS
                 GO TO RET-TO-ENTRY-ROUTINE
              END-IF
      * IF POSTING A 'P&L' CODE AND DL-DISC-CODE-NO-PL-EXP
      *  THAN THE REMAINING DEALER DISCOUNT UNEARNED
      *  WILL BE TAKEN INTO INCOME AND NOT REDUCE THE P&L EXPENSE
              IF DL-DISC-CODE-NO-PL-EXP
                 PERFORM COMPUTE-EARNINGS
                 ADD EARN-UNEARN(5) TO HOLD-LN-ACCERN(5)
                 ADD EARN-UNEARN(5) TO HOLD-LN-TOTERN
                 MOVE LN-EARN-ORDER(5) TO SUB
                 IF SUB > 3
                    MOVE EARN-UNEARN(5) TO LXE-EARN(SUB - 3)
                 ELSE
                    MOVE EARN-UNEARN(5) TO LP-EARNED(SUB)
                 END-IF
              END-IF
              PERFORM DISPLAY-EARNINGS.

           MOVE "3" TO ROUTE-CD.
           MOVE 2 TO NEW-PROG-SUB.
           IF LP-TRCD = "IR"
              MOVE INDU-INTEREST TO LP-INTDUE.

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
           MOVE LN-LCBAL  TO LP-LCBAL.
           MOVE LN-CURBAL TO LP-CURBAL.

           MOVE LP-PAYDATE TO IBPC-DATE.
           PERFORM IBPC-TEST.
           MOVE IBPC-FG TO LP-IBPC.

           MOVE "S  A060PMPT2." TO VDU.

      *  JTG 010904 ALLOWED IB TYPE LOANS TO ACCELERATE       JIM R. ALL PR#1582
      *    IF LP-TRCD = "AC" AND LP-IBPC = "I"
      *       MOVE "CAN NOT ACCELERATE I/B ACCOUNTS" TO MESS
      *       PERFORM SEND-MESS
      *       MOVE 7 TO ROUTE-CD
      *       GO TO INITIALIZE-EXIT.

           IF LP-TRCD = "IR" AND LP-IBPC = "P"
              MOVE "CAN'T CHANGE/REDUCE INTEREST ON P/C ACCOUNTS"
                                                            TO MESS
              PERFORM SEND-MESS
              MOVE 7 TO ROUTE-CD
              GO TO INITIALIZE-EXIT.

      * PASS- FIELDS ONLY NEEDED FOR TESTING REBATES ON JUDGES & ACCEL
           IF LP-TRCD = "JD" OR "AC"
              PERFORM CALL-PAYOFF.

           IF LP-TRCD NOT = "JD"
              GO TO INITIALIZE-TEST-AC.

           IF LP-IBPC = "P"
              IF HOLD-LN-JDRATE NOT = 999.999
                 IF LN-INT-REBATE NOT = "Y" AND NOT = "R"
                    IF PASS-INT-REB NOT = 0
                      MOVE "INTEREST MUST BE REBATED BEFORE JUDGEMENT"
                          TO MESS
                      PERFORM SEND-MESS
                      MOVE "7" TO ROUTE-CD
                      GO TO INITIALIZE-EXIT.

           IF (LP-IBPC = "P" AND HOLD-LN-JDRATE NOT = 999.999) OR
              (LP-IBPC NOT = "P"                             )
                 IF (LN-SERCHG NOT = 0 AND SP-RBFRMLA(8) NOT = " ") AND
                    (LN-SC-REBATE NOT = "Y" AND NOT = "R"         )
                    IF PASS-SC-REB NOT = 0
                       MOVE
                       "SERVICE CHARGE MUST BE REBATED BEFORE JUDGEMENT"
                           TO MESS
                       PERFORM SEND-MESS
                       MOVE "7" TO ROUTE-CD
                       GO TO INITIALIZE-EXIT.

           IF (LP-IBPC = "P" AND HOLD-LN-JDRATE NOT = 999.999) OR
               LP-IBPC NOT = "P"
                 IF LN-MAINTFEE NOT = 0 AND SP-RBFRMLA(9) NOT = " "
                    AND LN-MF-REBATE NOT = "Y" AND NOT = "R"
                    IF PASS-MF-REB NOT = 0
                       MOVE "MAINT FEE MUST BE REBATED BEFORE JUDGEMENT"
                           TO MESS
                       PERFORM SEND-MESS
                       MOVE "7" TO ROUTE-CD
                       GO TO INITIALIZE-EXIT.

       INITIALIZE-TEST-AC.
           IF LP-TRCD NOT = "AC"
              GO TO INITIALIZE-CONTINUE.
           IF LP-IBPC NOT = "P"
              GO TO INITIALIZE-CONTINUE.

           IF LN-INT-REBATE NOT = "Y" AND NOT = "R"
              IF PASS-INT-REB NOT = 0
                 MOVE "INTEREST MUST BE REBATED BEFORE ACCELERATING"
                    TO MESS
                 PERFORM SEND-MESS
                 MOVE "7" TO ROUTE-CD
                 GO TO INITIALIZE-EXIT.

           IF LN-SERCHG NOT = 0 AND SP-RBFRMLA(8) NOT = " "
              AND LN-SC-REBATE NOT = "Y" AND NOT = "R"
                 IF PASS-SC-REB NOT = 0
                    MOVE
                    "SERVICE CHARGE MUST BE REBATED BEFORE ACCELERATING"
                       TO MESS
                    PERFORM SEND-MESS
                    MOVE "7" TO ROUTE-CD
                    GO TO INITIALIZE-EXIT.

           IF LN-MAINTFEE NOT = 0 AND SP-RBFRMLA(9) NOT = " "
              AND LN-MF-REBATE NOT = "Y" AND NOT = "R"
                 IF PASS-SC-REB NOT = 0
                    MOVE "MAINT FEE MUST BE REBATED BEFORE ACCELERATING"
                       TO MESS
                    PERFORM SEND-MESS
                    MOVE "7" TO ROUTE-CD
                    GO TO INITIALIZE-EXIT.

       INITIALIZE-CONTINUE.
           MOVE "S  A060PMPT4." TO VDU.
           PERFORM SEND-INT-OR-LC.

           IF LP-TRCD = "IR"
              MOVE LP-PAYDATE TO RATE-DATE IBPC-DATE
              MOVE MAKER-CD TO RATE-MAKERCD
              PERFORM SET-PY-RATE
              ADD INDU-INTEREST TO LP-INTBAL
              GO TO INITIALIZE-EXIT.

           IF LP-TRCD = "PW"
              MOVE LN-CURBAL TO LP-APCUR
              GO TO INITIALIZE-EXIT.

      *    "JDPLPIRP".

           MOVE LN-CURBAL TO LP-INTDUE.

       INITIALIZE-EXIT.
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

      * STOPPED SETTING OF EARNINGS ON P&L & SERVICE ACCOUNTS:
           IF LN-PLCD = " " AND LN-SERVACCT = "N"
              IF (LP-TRCD = "AC" AND SP-ERNACCEL-CD = " ") OR
                 (LP-TRCD = "JD" AND SP-ERNJD-CD = " ")
                 PERFORM SET-LP-EARNED.

           IF LP-DLNO = 0
              MOVE LN-DLNO TO DL-DLNO
           ELSE
              MOVE LP-DLNO TO DL-DLNO.
           PERFORM GET-DL.
           IF IO-BAD
              MOVE "7" TO ROUTE-CD
              MOVE "PROBLEM GETTING DEALER RECORD" TO MESS
              PERFORM SEND-MESS
              GO TO DISPLAY-ENTRY-LN-EXIT.

      *    IF POSTING A 'P&L' CODE AND DL-DISC-CODE-NO-PL-EXP
      *     THAN THE REMAINING DEALER DISCOUNT UNEARNED
      *          WILL BE TAKEN INTO INCOME AND NOT REDUCE
      *          THE P&L EXPENSE
      
           IF LP-TRCD = "PL" OR "P2" OR "P3" OR "P4" OR "P5"
              IF DL-DISC-CODE-NO-PL-EXP
                 PERFORM COMPUTE-EARNINGS
                 ADD EARN-UNEARN(5) TO HOLD-LN-ACCERN(5)
                 ADD EARN-UNEARN(5) TO HOLD-LN-TOTERN
                 MOVE LN-EARN-ORDER(5) TO SUB
                 IF SUB > 3
                    MOVE EARN-UNEARN(5) TO LXE-EARN(SUB - 3)
                 ELSE
                    MOVE EARN-UNEARN(5) TO LP-EARNED(SUB).

           IF LP-TRCD = "P4" OR "P5"
              PERFORM TEST-DEALER-RESERVE
              IF ROUTE-CD = "7"
                 GO TO DISPLAY-ENTRY-LN-EXIT
              ELSE
                 MOVE PLEXP TO LP-LCPARTIALS.

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

      * TEST FOR CALCULATION OF AFPDLLP AMOUNT: (RAC)
           IF LN-PLCD = " "
              IF LP-TRCD = "PL" OR "P2" OR "P3" OR "P4" OR "P5"
                 IF LP-DLNO = 0
                    MOVE LN-DLNO TO DL-DLNO
                 ELSE
                    MOVE LP-DLNO TO DL-DLNO
                 END-IF
      *           MOVE LN-DLNO TO DL-DLNO
                 PERFORM GET-DL
                 IF IO-FG = 0 AND DL-AFP-DEALER = "Y"
                    MOVE LP-TRDATE TO AFPDLLP-TODAY
                    MOVE LP-TRDATE TO LN-PLDATE
                    MOVE "PL" TO LN-PLCD
                    PERFORM AFPDLLP
                    MOVE 0 TO LN-PLDATE
                    MOVE " " TO LN-PLCD
                    MOVE AFPDLLP-DUE-DEALER TO HOLD-AFPDLLP.

           PERFORM DISPLAY-EARNINGS.

           PERFORM SETUP-LINE.
           PERFORM SEND-ENTRY-LINE.

       DISPLAY-ENTRY-LN-EXIT.
           EXIT.

      *************************************
       ACCEPT-ENTRY-ROUTINE SECTION.
      *************************************
       ACCEPT-ENTRY-RTN.
           MOVE "F1-CANCEL, F7-GL ENTRY, DRAWER ENTRY   :" TO MESS.

      * (WORLD) WE REALLY NEED TO TEST THE PASSWORD
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

           IF SV-PASSWORD-DRAWER = "Y"
              INSPECT MESS REPLACING FIRST "ENTRY   :" BY "PASSWORD:"
           ELSE
              INSPECT MESS REPLACING FIRST "ENTRY   :" BY "NUMBER  :".
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
              PERFORM ACCEPT-SCREEN-RESET
              IF LXG-GLNO(1) NOT = 0 OR (LP-TRCD = "PL" AND
                         (BR-AUTO-PL-REF-AC = "Y" OR "2" OR "3"))
                                     OR
            (LP-TRCD = "P2" AND (BR-AUTO-PL-REF-AC = "2" OR "3"))
                                     OR
            (LP-TRCD = "P3" AND (BR-AUTO-PL-REF-AC = "3"))
                 PERFORM DISPLAY-ENTRY-UP
                 GO TO ACCEPT-ENTRY-EXIT
              ELSE
                 GO TO ACCEPT-ENTRY-EXIT.

           IF LP-CURBAL = 0
              IF LP-TRCD NOT = "IR" AND
                 (LP-INTBAL NOT = 0 OR LP-OTHBAL NOT = 0 OR
                  LP-LCBAL NOT = 0)
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
      * DO NOT USE "RESET ON" OPTION IT WILL CLEAR CODE FROM SCAN.
           MOVE "ENNA060SEL." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" OR "1U"
              MOVE "SNNA060SEL." TO VDU
              MOVE SPACES TO VDUBUF
              PERFORM SCREENIO
              GO TO ACCEPT-ENTRY-RTN.
           IF VDUSTATUS = "1<"
              MOVE "PO" TO CDSCAN-TYPE
              PERFORM CODE-SCAN
              MOVE CD-CODE TO HOLD-POCD
              GO TO ACCEPT-PO-CODE.
           IF VDUSTATUS NOT = "00"
              GO TO ACCEPT-PO-CODE.
           MOVE "PO"   TO CD-TYPE.
      * NEED TO LEFT JUSTIFY THE "SEL" FIELD 
           CALL "C$JUSTIFY" USING VDUBUF, "L".
           MOVE VDUBUF TO CD-CODE.

      * CLEAR SEL FIELDS AFTER ENTRY.
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
           MOVE LP1-KEY   TO HOLD2-KEY-UP.

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
      * NEEDED FOR AUTOMATIC UPDATE AC-PL
           MOVE LX-REC  TO HOLD-LX-REC.
           MOVE LXG-REC TO HOLD-LXG-REC.

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
              IF LP-ACCTNO NOT = LN-ACCTNO OR
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
              MOVE "N"       TO FULL-PAGE-FG
              MOVE LN-OWNBR  TO LP-BRNO
              MOVE LN-ACCTNO TO LP-ACCTNO
              MOVE 99999     TO LP-SEQNO
              MOVE LP1-KEY   TO HOLD2-KEY-UP
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
              MOVE LP1-KEY   TO HOLD1-KEY-DOWN.
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
              MOVE LP1-KEY   TO HOLD1-KEY-DOWN
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
              MOVE LP1-KEY   TO HOLD2-KEY-UP.
           PERFORM CLOSE-LP1-FILE.

      *******************************************
       ENTER-PLREASON-CODE SECTION.
      *******************************************
       ENTER-PLREASON.
           MOVE " " TO ROUTE-CD.
           MOVE "^-BACK, F6-SCAN, ENTER CHARGEOFF REASON:" TO MESS.
           PERFORM SEND-LEGEND.

      * FIRST CHECK TO SEE IF THE LSX-REASON-CODE HAS A VALUE
      * IF SO, THEN MOVE THAT CODE TO THE PLREASON CODE AND GET OUT !
            MOVE LN-OWNBR  TO LSX-BRNO.
            MOVE LN-ACCTNO TO LSX-ACCTNO.
            MOVE 1         TO LSX-SEQNO.
            PERFORM OPEN-LSX1-FILE.
            PERFORM READ-LSX1-FILE.
            PERFORM CLOSE-LSX1-FILE.
            IF IO-FG = 0
               IF (LSX-REASON-CODE NOT = SPACES)
                  MOVE LSX-REASON-CODE TO HOLD-PLREASON
                  GO TO EXIT-ENTER-PLREASON.

      * DO NOT USE "RESET ON" OR IT WILL CLEAR CODE FROM SCAN.
           MOVE "ENNA060SEL." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1<"
              MOVE "CH" TO CDSCAN-TYPE
              PERFORM CODE-SCAN
              MOVE CD-CODE TO HOLD-PLREASON
              GO TO ENTER-PLREASON.
           IF VDUSTATUS = "1U"
              MOVE "7" TO ROUTE-CD
              GO TO EXIT-ENTER-PLREASON.
           IF VDUSTATUS NOT = "00"
              GO TO ENTER-PLREASON.

           IF VDUBUF = SPACES
              GO TO ENTER-PLREASON.

           MOVE "CH"   TO CD-TYPE.
      * NEED TO LEFT JUSTIFY THE "SEL" FIELD
           CALL "C$JUSTIFY" USING VDUBUF, "L".
           MOVE VDUBUF TO CD-CODE.

      * CLEAR SEL FIELDS AFTER ENTRY.
           MOVE "SNNA060SEL." TO VDU.
           MOVE SPACES TO VDUBUF.
           PERFORM SCREENIO.

           PERFORM OPEN-CD1-FILE.
           PERFORM READ-CD1-FILE.
           PERFORM CLOSE-CD1-FILE.
           IF IO-FG NOT = 0
              MOVE "INVALID REASON CODE" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-PLREASON.

           MOVE CD-CODE TO HOLD-PLREASON.

       EXIT-ENTER-PLREASON.
           EXIT.

      **************************************************
       SETUP-AC-LINE SECTION.
      **************************************************
           IF LN-ACCELDATE NOT = 0
              GO TO SETUP-AC-LINE-EXIT.

           MOVE "AC" TO LP-TRCD.
           MOVE HOLD-LN-JDRATE TO LP-NEWSTAT-RATE.
           MOVE 0 TO LP-TRAMT LP-APCUR.
           MOVE 0 TO LP-APOTH LP-APOT2 LP-APINT LP-APLC
                     LP-OTHBAL LP-OT2BAL LP-INTBAL LP-LCBAL
           MOVE " " TO LP-REV.
           MOVE LN-1STPYDATE TO PDTH-DATE-WORK.
           PERFORM PAID-THRU-CALCULATION.
           MOVE PDTH-DATE-FULL TO LP-PDTH-DATE.
           IF (LP-TRCD = "AC" AND SP-ERNACCEL-CD = " ")
               PERFORM SET-LP-EARNED.

           PERFORM SETUP-LINE.
           ADD 1 TO SUB1.
           MOVE LN-BUF TO D-LINE(SUB1).

       SETUP-AC-LINE-EXIT.
           EXIT.

      **************************************************
       SETUP-LINE SECTION.
      **************************************************
           MOVE SPACES TO LN-BUF EB-BUF.
           MOVE LP-PAYDATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO L-010 EB-010.
           MOVE LP-TRCD TO L-020 EB-020.
           MOVE LP-REFNO TO L-030 EB-030.
           IF LP-TRCD = "JD" OR "IR" OR "AC"
              MOVE LP-NEWSTAT-RATE TO L-040RATE EB-040RATE
           ELSE
           IF (LP-TRCD = "P4" OR "P5")
                  AND (LP-DLNO NOT = 0)
              MOVE "D" TO L-040D EB-040D
              MOVE LP-DLNO TO L-040DLNO EB-040DLNO
           ELSE
              MOVE LP-TRAMT TO L-040 EB-040.
           IF (LP-TRCD = "P4" OR "P5")
                    AND (LP-LCPARTIALS NOT = LP-CURBAL)
              MOVE LP-LCPARTIALS TO L-050 EB-050
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
           ELSE
              MOVE LP-APOTH TO L-050 EB-050.

           ADD LP-APINT LP-APLC GIVING L-060 EB-060.
           MOVE LP-APCUR TO L-070 EB-070.

      * 7/30/98 BAH ALWAYS DISPLAY BALANCES, EVEN ON REVERSALS #487
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
              MOVE EB-080 TO L-080
              COMPUTE L-090 =  LP-OTHBAL + LP-OT2BAL
              ADD LP-INTBAL LP-LCBAL GIVING L-100
              MOVE LP-CURBAL TO L-110 EB-110.
              IF (LP-TRCD = "RV" OR LP-REV NOT = " ") AND
                  NOT (LP-TRCD = "RV" AND LP-REVSEQNO=0)
                 MOVE "*" TO L-120 EB-120.

      ******************************************
       GET-PAYOFF-REBATES SECTION.
      ******************************************
           MOVE LN-ACCTNO          TO CRNOR2-ACCTNO.
           MOVE TRANS-DATE         TO CRNOR2-SYSDATE.
           MOVE "PO"               TO CRNOR2-LPTRCD.
           MOVE "Y"                TO CRNOR2-GET-PAYOFF-ONLY.
           MOVE "Y"                TO CRNOR2-RECAST-FG.
           MOVE "CL/CRNOR2" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH CRNOR2-LINK
                                          NO-REC POFF-WORKERS
                                          LN-REC.
           CANCEL FORM-PROGX.

      **********************************************
       SET-PAYOFF-LINES SECTION.
      **********************************************

           MOVE LP-REC  TO WK-LP-REC HOLD-LP-REC.
           MOVE LX-REC  TO WK-LX-REC HOLD-LX-REC.
           MOVE LXE-REC TO WK-LXE-REC HOLD-LXE-REC.
           MOVE LXG-REC TO WK-LXG-REC HOLD-LXG-REC.

           PERFORM CLEAR-LP-FILE.

           MOVE TRANS-DATE TO LP-PAYDATE.
           MOVE LN-OTHBAL TO LP-OTHBAL.
           MOVE LN-INTBAL TO LP-INTBAL.
           MOVE LN-LCBAL TO LP-LCBAL.
           MOVE LN-CURBAL TO LP-CURBAL.

           MOVE 0 TO SUB SUB1.
           MOVE " " TO LINE-BUF.
       SET-PAYOFF-LINES-CONT.
           ADD 1 TO SUB.
           IF SUB < 14
      * INTEREST, SC, MF, DF, N/A
              IF SUB > 3 AND < 9
                 IF POFF-REBATE(SUB) = 0
                    GO TO SET-PAYOFF-LINES-CONT
                 ELSE
                    PERFORM SEND-PAYOFF-LINES-REC
              ELSE
      * INSURANCES
                 IF POFF-REBATE(SUB) NOT = 0
                    PERFORM SEND-PAYOFF-LINES-REC
                 ELSE
                    IF SUB < 4
                       IF LN-INSPREM(SUB) NOT = 0 AND
                           LN-REBATE(SUB) = " "
      * MUST STILL DO INSURANCES IF REBATE IS 0 BUT THERE WAS A PREMIUM, AND
      * IT HASNT ALREADY BEEN REBATED
                          PERFORM SEND-PAYOFF-LINES-REC
                       ELSE
                          GO TO SET-PAYOFF-LINES-CONT
                    ELSE
                       IF LN-INSPREM(SUB - 5) NOT = 0 AND
                           LN-REBATE(SUB) = " "
                          PERFORM SEND-PAYOFF-LINES-REC
                       ELSE
                          GO TO SET-PAYOFF-LINES-CONT
           ELSE
              PERFORM SETUP-AC-LINE
              MOVE WK-LP-REC  TO LP-REC
              MOVE WK-LX-REC  TO LX-REC HOLD-LX-REC
              MOVE WK-LXE-REC TO LXE-REC HOLD-LXE-REC
              MOVE WK-LXG-REC TO LXG-REC HOLD-LXG-REC
              COMPUTE LP-CURBAL = LP-CURBAL
                 - POFF-REBATE(1) - POFF-REBATE(2)
                  - POFF-REBATE(3) - POFF-REBATE(4)
                   - POFF-REBATE(5) - POFF-REBATE(6)
                    - POFF-REBATE(7) - POFF-REBATE(8)
                     - POFF-REBATE(9) - POFF-REBATE(10)
                      - POFF-REBATE(11) - POFF-REBATE(12)
                       - POFF-REBATE(13)
               MOVE LN-1STPYDATE TO PDTH-DATE-WORK
               PERFORM PAID-THRU-CALCULATION
               MOVE PDTH-DATE-FULL TO LP-PDTH-DATE
               PERFORM COMPUTE-BW-INTPAID
               MOVE BWIN-INTPAID TO LP-INTPAID
               MOVE LP-REC TO HOLD-LP-REC.

           PERFORM SETUP-LINE.
           ADD 1 TO SUB1.
           MOVE LN-BUF TO D-LINE(SUB1).
           IF SUB > 12
              GO TO SET-PAYOFF-LINES-EXIT.

           MOVE SUB TO SUB2.
           IF SUB > 3 AND < 9
              GO TO SET-PAYOFF-LINES-CONT
           ELSE
           IF SUB = 9
              MOVE " " TO INS-RB-TBL(4)
              MOVE 4 TO SUB2
           ELSE
           IF SUB = 10
              MOVE " " TO INS-RB-TBL(5)
              MOVE 5 TO SUB2
           ELSE
           IF SUB = 11
              MOVE " " TO INS-RB-TBL(6)
              MOVE 6 TO SUB2
           ELSE
           IF SUB = 12
              MOVE " " TO INS-RB-TBL(7)
              MOVE 7 TO SUB2
           ELSE
           IF SUB = 13
              MOVE " " TO INS-RB-TBL(8)
              MOVE 8 TO SUB2
           ELSE
              MOVE " " TO INS-RB-TBL(SUB).

           IF POFF-REBATE(SUB) NOT = 0
              MOVE "Y" TO INS-RB-TBL(SUB2).

           GO TO SET-PAYOFF-LINES-CONT.

       SET-PAYOFF-LINES-EXIT.
           EXIT.

      **************************************************
       UPDATE-AUTOMATIC-PL SECTION.
      **************************************************
           MOVE 0 TO SUB.
       UPDATE-NEXT-PL-TRANS.
           ADD 1 TO SUB.
           IF SUB > 13
              GO TO UPDATE-AUTOMATIC-PL-AC.

           IF SUB = 1
              IF POFF-REBATE(1) NOT = 0
                 MOVE "RC" TO HOLD-BP-TRCD
              ELSE
                 IF LN-INSPREM(SUB) NOT = 0 AND LN-REBATE(SUB) = " "
                    MOVE "RC" TO HOLD-BP-TRCD
                 ELSE
                    GO TO UPDATE-NEXT-PL-TRANS.
           IF SUB = 2
              IF POFF-REBATE(2) NOT = 0
                 MOVE "RA" TO HOLD-BP-TRCD
              ELSE
                 IF LN-INSPREM(SUB) NOT = 0 AND LN-REBATE(SUB) = " "
                    MOVE "RA" TO HOLD-BP-TRCD
                 ELSE
                    GO TO UPDATE-NEXT-PL-TRANS.
           IF SUB = 3
              IF POFF-REBATE(3) NOT = 0
                 MOVE "RP" TO HOLD-BP-TRCD
              ELSE
                 IF LN-INSPREM(SUB) NOT = 0 AND LN-REBATE(SUB) = " "
                    MOVE "RP" TO HOLD-BP-TRCD
                 ELSE
                    GO TO UPDATE-NEXT-PL-TRANS.
           IF SUB = 4
              IF POFF-REBATE(4) NOT = 0
                 MOVE "RI" TO HOLD-BP-TRCD
              ELSE
                 GO TO UPDATE-NEXT-PL-TRANS.
           IF SUB = 5
              IF POFF-REBATE(5) NOT = 0
                 MOVE "RS" TO HOLD-BP-TRCD
              ELSE
                 GO TO UPDATE-NEXT-PL-TRANS.
           IF SUB = 6
              IF POFF-REBATE(6) NOT = 0
                 MOVE "RM" TO HOLD-BP-TRCD
              ELSE
                 GO TO UPDATE-NEXT-PL-TRANS.
           IF SUB = 7
              IF POFF-REBATE(7) NOT = 0
                 MOVE "RD" TO HOLD-BP-TRCD
              ELSE
                 GO TO UPDATE-NEXT-PL-TRANS.
           IF SUB = 8
              GO TO UPDATE-NEXT-PL-TRANS.
           IF SUB = 9
              IF POFF-REBATE(9) NOT = 0
                 MOVE "R1" TO HOLD-BP-TRCD
              ELSE
                 IF LN-INSPREM(SUB - 5) NOT = 0 AND LN-REBATE(SUB) = " "
                    MOVE "R1" TO HOLD-BP-TRCD
                 ELSE
                    GO TO UPDATE-NEXT-PL-TRANS.
           IF SUB = 10
              IF POFF-REBATE(10) NOT = 0
                 MOVE "R2" TO HOLD-BP-TRCD
              ELSE
                 IF LN-INSPREM(SUB - 5) NOT = 0 AND LN-REBATE(SUB) = " "
                    MOVE "R2" TO HOLD-BP-TRCD
                 ELSE
                    GO TO UPDATE-NEXT-PL-TRANS.
           IF SUB = 11
              IF POFF-REBATE(11) NOT = 0
                 MOVE "R3" TO HOLD-BP-TRCD
              ELSE
                 IF LN-INSPREM(SUB - 5) NOT = 0 AND LN-REBATE(SUB) = " "
                    MOVE "R3" TO HOLD-BP-TRCD
                 ELSE
                    GO TO UPDATE-NEXT-PL-TRANS.
           IF SUB = 12
              IF POFF-REBATE(12) NOT = 0
                 MOVE "R4" TO HOLD-BP-TRCD
              ELSE
                 IF LN-INSPREM(SUB - 5) NOT = 0 AND LN-REBATE(SUB) = " "
                    MOVE "R4" TO HOLD-BP-TRCD
                 ELSE
                 GO TO UPDATE-NEXT-PL-TRANS.
           IF SUB = 13
              IF POFF-REBATE(13) NOT = 0
                 MOVE "R5" TO HOLD-BP-TRCD
              ELSE
                 IF LN-INSPREM(SUB - 5) NOT = 0 AND LN-REBATE(SUB) = " "
                    MOVE "R5" TO HOLD-BP-TRCD
                 ELSE
                    GO TO UPDATE-NEXT-PL-TRANS.

           PERFORM SETUP-LP-RECORD.
           PERFORM SETUP-LONPF-BUFFER.
           MOVE LN-ACCTNO TO LOAN-KEY.
           MOVE LN-SSNO(1) TO BORROWER-KEY.
      * MOVES UPDATED LN-REC TO WK-SAVE-REC EVERYTIME.
           PERFORM SAVE-SCREEN.

      * REBATE PROGRAM TO GET LP-RECORD SETUP PROPERLY

      *   THIS SET OF 'F9' TRIGGERS LONPFB.C TO DO A REFUND OF INTEREST
      *   WITH "PO" IN REB-LPTRCD, LIKE CRNOR2.C
           IF HOLD-BP-TRCD = "RI"
              MOVE "F9" TO HOLD-BP-TRCD.

           MOVE "LP/LONPFB" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH EXIT-PATHNAME
                                          PROG-BUF UPDATE-BUF.
           CANCEL FORM-PROGX.

      * UPDATE PROGRAM
           MOVE "LP/LONPF2" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH EXIT-PATHNAME
                                          PROG-BUF UPDATE-BUF.
           CANCEL FORM-PROGX.

      * READ UPDATED LOAN
           PERFORM READ-LN1-FILE.

           GO TO UPDATE-NEXT-PL-TRANS.

       UPDATE-AUTOMATIC-PL-AC.

      * READ UPDATED LOAN (ONE MORE TIME, IN CASE THERE WERE NO REBATES)
           PERFORM READ-LN1-FILE.

           IF LN-ACCELDATE NOT = 0
              GO TO UPDATE-AUTOMATIC-PL-EXIT.

      * CLEAR OUT LXG , IN CASE THEY DID F7-GL ON THE P&L
           PERFORM CLEAR-LXG-FILE.

           MOVE LP-BRNO   TO LXG-BRNO.
           MOVE LP-ACCTNO TO LXG-ACCTNO.

      * START WITH THE PL RECORD, EVERYTHING VERY SIMILAR
           MOVE HOLD-LP-REC TO LP-REC.

           MOVE "AC" TO LP-TRCD.
           MOVE HOLD-LN-JDRATE TO LP-NEWSTAT-RATE.
           MOVE LN-CURBAL TO LP-CURBAL LP-INTDUE.
           MOVE CA-TILLNO TO LP-TILLNO.
           PERFORM SETUP-LONPF-BUFFER.
           IF (LP-TRCD = "AC" AND SP-ERNACCEL-CD = " ")
               PERFORM SET-LP-EARNED.
           MOVE LN-ACCTNO TO LOAN-KEY.
           MOVE LN-SSNO(1) TO BORROWER-KEY.
           PERFORM SAVE-SCREEN.

      * UPDATE PROGRAM
           MOVE "LP/LONPF2" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH EXIT-PATHNAME
                                          PROG-BUF UPDATE-BUF.
           CANCEL FORM-PROGX.

       UPDATE-AUTOMATIC-PL-EXIT.

      * READ UPDATED LOAN (ONE MORE TIME, IN PREPARATION FOR THE "PL")
           PERFORM READ-LN1-FILE.

      **************************************************
       SETUP-LP-RECORD SECTION.
      **************************************************
           PERFORM CLEAR-LP-FILE.
           MOVE LN-STATUSFG TO LP-STATUSFG.
           MOVE LN-ACCTNO TO LP-ACCTNO.
           MOVE TRANS-DATE TO LP-TRDATE LP-PAYDATE.
           MOVE LN-SSNO(1) TO LP-SSNO.
           MOVE LN-LCPDTH-DATE TO LP-LCPDTH-DATE.
           MOVE POFF-REBATE(SUB) TO LP-TRAMT LP-APCUR.
           IF SUB < 4
              MOVE POFF-INS-TAX-RB(SUB) TO LP-PRORATED-INS-REB
           ELSE
              IF SUB > 8
                 MOVE POFF-INS-TAX-RB(SUB - 5) TO LP-PRORATED-INS-REB.

      *         POFF-INS-TAX-RB(1)      CL REBATE INS TAX
      *                        (2)      AH REBATE INS TAX
      *                        (3)      PP REBATE INS TAX
      *                        (4)      O1 REBATE INS TAX
      *                        (5)      O2 REBATE INS TAX
      *                        (6)      O3 REBATE INS TAX
      *                        ----
      *                        (7)      O4 REBATE INS TAX
      *                        (8)      O5 REBATE INS TAX

           MOVE CA-TILLNO TO LP-TILLNO.
           MOVE HOLD-BP-TRCD TO LP-TRCD.

           MOVE SPACES    TO LXG-REC.
           MOVE LP-BRNO   TO LXG-BRNO.
           MOVE LP-ACCTNO TO LXG-ACCTNO.
           MOVE LP-SEQNO  TO LXG-SEQNO.
           MOVE 0 TO LXG-GLNO(1) LXG-GLNO(2) LXG-GLNO(3)
                     LXG-GLAMT(1) LXG-GLAMT(2) LXG-GLAMT(3).

      **************************************************
       SETUP-LONPF-BUFFER SECTION.
      **************************************************
           MOVE 0 TO ENTRY-FLAG BYPASS-AMT-ENT-FG.
           MOVE "F" TO FULL-DISPLAY.
           MOVE " " TO REV-REBATE FULL-PAGE-FG ENTER-AMT-FG
                       HOLD1-KEY-DOWN HOLD2-KEY-UP.

           MOVE 0 TO HOLD-PARTIALS.
           MOVE 0 TO HOLD-LCPAID.
           MOVE 0 TO HOLD-DLPARVHELD-ADJ.
           MOVE 0 TO HOLD-AFPDLLP.

           MOVE LN-TOTERN TO HOLD-LN-TOTERN.
           MOVE LN-TOTSERACCT TO HOLD-LN-TOTSERACCT.
           MOVE LN-TOTINT TO HOLD-LN-TOTINT.
           MOVE LN-TOTDEF TO HOLD-LN-TOTDFMNTS.
           MOVE LN-TOTLCHG TO HOLD-LN-TOTLCHG.
      *
      *    MOVE LN-JDRATE TO HOLD-LN-JDRATE.
      * NEED VALUE PASSED FROM LONPF1 FOR THE AUTOMATIC "AC"

           PERFORM VARYING SUB1 FROM 1 BY 1 UNTIL SUB1 > 13
              MOVE 0 TO REB-AMOUNT(SUB1)
              IF SUB1 < 7
                 MOVE LN-ACCERN(SUB1) TO HOLD-LN-ACCERN(SUB1)
              END-IF
              IF SUB1 < 9
                 MOVE " " TO INS-RB-TBL(SUB1)
              END-IF
           END-PERFORM.

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
           MOVE 0 TO WK-ELE.
           PERFORM REWRITE-WK-FILE.
       EXIT-RESTORE-SCREEN.
           PERFORM CLOSE-WK-FILE.

      *******************************
       VERIFY-PASSWORD SECTION.
      *******************************
           MOVE " "      TO PASSWDW-STAT.
           MOVE "LONPF9" TO PASSWDW-PGM.
           MOVE "PLEASE ENTER THE PASSWORD," TO PASSWDW-PROMPT1.
           MOVE "CL/PASSWD" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH PASSWDW-BUF.
           CANCEL FORM-PROGX.

      **************************************************
       TEST-DEALER-RESERVE SECTION.
      **************************************************

      * TEST DEALER RESERVE BALANCE TO P&L EXPENSE

           MOVE " " TO ROUTE-CD.
           PERFORM COMPUTE-EARNINGS.
           COMPUTE PLEXP = LN-CURBAL - EARN-TOT-UNEARN.
           PERFORM OPEN-DS1-FILE.
           MOVE TRANS-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-YYYYMM.
           MOVE DATE-YYYYMM TO DS-DATE-CCYYMM.
           MOVE LN-OWNBR TO DS-BRANCH
           IF LP-DLNO = 0
              MOVE LN-DLNO TO DS-DLNO
           ELSE
              MOVE LP-DLNO TO DS-DLNO.
           MOVE LN-CLASS TO DS-CLASS.
           PERFORM READ-DS1-FILE.
           PERFORM CLOSE-DS1-FILE.
           IF IO-FG NOT = 0
              MOVE "7" TO ROUTE-CD
              MOVE "PROBLEM GETTING DEALER RESERVE BALANCES" TO MESS
              PERFORM SEND-MESS
              GO TO TEST-DEALER-RESERVE-EXIT.

      * IF DL-DISC-CODE-NO-PL-EXP THAN THE REMAINING DEALER DISCOUNT
      *    UNEARNED WILL BE TAKEN INTO INCOME AND NOT REDUCE
      *    THE P&L EXPENSE
      
           IF DL-DISC-CODE-NO-PL-EXP
              ADD EARN-UNEARN(5) TO PLEXP.

      * REMOVED LP-REFNO REDEFINITIONS, MOVED THEM TO LPWSLP
           MOVE LP-REFNO TO LPWSLP-LP-REFNO.

           IF LPWSLP-LP-P4P5-TRCD = "PR"
              IF PLEXP > DS-DLRVPRIN(LPWSLP-LP-P4P5-DLRVNO)
                 PERFORM INSUFF-RSV-PRIN
              ELSE
                 GO TO TEST-DEALER-RESERVE-EXIT.
           IF LPWSLP-LP-P4P5-TRCD = "PC"
              IF PLEXP > DS-DLRVCHGS(LPWSLP-LP-P4P5-DLRVNO)
                 PERFORM INSUFF-RSV-CHGS
              ELSE
                 GO TO TEST-DEALER-RESERVE-EXIT.
           IF LPWSLP-LP-P4P5-TRCD = "PP"
              IF PLEXP > DS-DLPARVHELD
                 PERFORM INSUFF-RSV-PARTIC
              ELSE
                 GO TO TEST-DEALER-RESERVE-EXIT.

      * IF THE BRANCH FLAG IS NOT 'Y' THEN THEY DONT WANT
      * TO BE ASKED THE QUESTION TO OVERRIDE
           IF BR-NEGDLRSV NOT = "Y"
              MOVE "THE RESERVE WOULD BE NEGATIVE!!" TO MESS
              PERFORM SEND-MESS
              MOVE "7" TO ROUTE-CD
              GO TO TEST-DEALER-RESERVE-EXIT.

           MOVE SPACES TO MESS.
           PERFORM SEND-LEGEND.
       TEST-DEALER-RESERVE-QUESTION.
           MOVE "ACCEPT NEGATIVE RESERVE AMOUNT (Y/N):" TO MESS.
           PERFORM SEND-LEGEND.
           MOVE "E RA060SEL." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS NOT = "00"
              GO TO TEST-DEALER-RESERVE-QUESTION.
           MOVE VDUBUF TO DLR-TEST.
           IF DLR-ANSWER = "Y"
              GO TO TEST-DEALER-RESERVE-EXIT.
           IF DLR-ANSWER NOT = "N"
              GO TO TEST-DEALER-RESERVE-QUESTION.
           MOVE "7" TO ROUTE-CD.
       TEST-DEALER-RESERVE-EXIT.
           IF ROUTE-CD = "7"
              PERFORM ACCEPT-SCREEN-RESET
              PERFORM DISPLAY-ENTRY-UP.

      *****************************************************
       DISPLAY-EARNINGS SECTION.
      *****************************************************
           MOVE SPACES TO EARN-BUF.
           IF HOLD-DISPLAY-ERN = "A"
              COMPUTE TOTEARN = HOLD-LN-ACCERN(1) + HOLD-LN-ACCERN(2)
                              + HOLD-LN-ACCERN(3) + HOLD-LN-ACCERN(5)
              IF NOT (GP-INCL-OTH-IN-EARNED-N OR
                                          GP-INCL-OTH-IN-EARNED-R)
                 ADD HOLD-LN-ACCERN(6) TO TOTEARN
              END-IF
              IF NOT(GP-INCL-OTH-IN-EARNED-R)
                 ADD HOLD-LN-ACCERN(4) TO TOTEARN
              END-IF
              MOVE TOTEARN TO D-TOTEARN
           ELSE
              IF HOLD-DISPLAY-ERN = "T"
                 MOVE HOLD-LN-TOTERN TO D-TOTEARN
              ELSE
              IF HOLD-DISPLAY-ERN = "U"
                 MOVE 0 TO D-TOTEARN
                 IF LN-PLCD = " " AND LN-SERVACCT NOT = "Y"
                    COMPUTE UNEARNED =
                            (LN-ANTICERN(1) - HOLD-LN-ACCERN(1))
                          + (LN-ANTICERN(2) - HOLD-LN-ACCERN(2))
                          + (LN-ANTICERN(3) - HOLD-LN-ACCERN(3))
                          + (LN-ANTICERN(5) - HOLD-LN-ACCERN(5))
                    IF NOT (GP-INCL-OTH-IN-EARNED-N OR
                                            GP-INCL-OTH-IN-EARNED-R)
                       COMPUTE UNEARNED = UNEARNED
                          + (LN-ANTICERN(6) - HOLD-LN-ACCERN(6))
                    END-IF
                    IF NOT(GP-INCL-OTH-IN-EARNED-R)
                       COMPUTE UNEARNED = UNEARNED
                          + (LN-ANTICERN(4) - HOLD-LN-ACCERN(4))
                    END-IF
                    MOVE UNEARNED TO D-TOTEARN
              ELSE
      * 1/14/2013 #4190 I TRIED TO COMPUTE THE TDUE-INTDUE HERE BUT JUST
      * COULDNT. YOU NEED LPPOFF BUT CANT BECUASE THE MIP STUFF IS HARD
      * CODED IN THIS PROGRAM AND ALSO IS BURIED IN LPPOFF SO THEY CONFLICT.
      * I ALSO TRIED CRNORC BUT THEN YOU GET THE OLD LN-REC AND IT DOESNT
      * REFLECT WHAT YOU ARE CURRENTLY POSTING. KM SAID OK TO LEAVE BLANK
      * LONPF1 WILL RECALC IT AGAIN WHEN ON THE DATE
                   IF HOLD-DISPLAY-ERN = "I"
                      NEXT SENTENCE
                   ELSE
                   IF HOLD-DISPLAY-ERN = "P"
                      MOVE LN-TOTPAYMNTD TO D-TOTEARN.

           MOVE LN-YTDINTPD TO D-INTYTD.
           MOVE LCAP-LCPDTH-DATE TO NUM-DATE.
           MOVE NUM-MO TO D-LCPDTH-MO.
           IF LN-UNITPER-CD-DAYS
              MOVE NUM-DA TO D-LCPDTH-DAYR
           ELSE
              MOVE NUM-YR TO D-LCPDTH-DAYR.

           COMPUTE D-EARNED ROUNDED = LP-EARNED(1) + LP-EARNED(2)
                                    + LP-EARNED(3) + LXE-EARN(1)
                                    + LXE-EARN(2) + LXE-EARN(3).
      *                             + LP-EARNED(3) + WK-LXE-EARN(1) +
      *                               WK-LXE-EARN(2) + WK-LXE-EARN(3).
           MOVE LP-DLPROC TO D-DLPROC.
           MOVE EARN-BUF TO WR-BUF.
           MOVE EARN-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      *******************************************************
       SET-LP-EARNED SECTION.
      *******************************************************
           MOVE 0 TO SLOT.

       SET-LP-EARNED-AGAIN.
           IF SLOT < 6
              ADD 1 TO SLOT
              PERFORM SET-LP-EARNED-ROUTINE
              GO TO SET-LP-EARNED-AGAIN.

           GO TO SET-LP-EARNED-EXIT.

       SET-LP-EARNED-ROUTINE.
           MOVE LN-EARN-ORDER(SLOT) TO EARN-SUB.
           IF EARN-SUB > 3
              COMPUTE LXE-EARN(EARN-SUB - 3) =
                      LN-ANTICERN(SLOT) - LN-ACCERN(SLOT)
              ADD LXE-EARN(EARN-SUB - 3) TO
                      HOLD-LN-TOTERN HOLD-LN-ACCERN(SLOT)
           ELSE
               COMPUTE LP-EARNED(EARN-SUB) =
                       LN-ANTICERN(SLOT) - LN-ACCERN(SLOT)
               ADD LP-EARNED(EARN-SUB) TO
                       HOLD-LN-TOTERN HOLD-LN-ACCERN(SLOT).

       SET-LP-EARNED-EXIT.
           EXIT.

      *********************************************
       CODE-SCAN SECTION.
      *********************************************

           MOVE SPACES TO CDSCAN-CODE.

           MOVE "WI/CDSCAN" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH CDSCAN-BUF EXIT-PATHNAME.
           CANCEL FORM-PROGX.

           IF ( CDSCAN-STATUS NOT = "00"    ) OR
              ((CDSCAN-TYPE = "CH") AND (CDSCAN-CODE = SPACES )) OR
              ((CDSCAN-TYPE = "PO") AND (CDSCAN-CODE = SPACES ))
               MOVE 9 TO IO-FG
               GO TO EXIT-CODE-SCAN.

           MOVE "SNNA060SEL." TO VDU.
           MOVE CDSCAN-CODE TO VDUBUF.
           PERFORM SCREENIO.

           MOVE CDSCAN-CODE TO VDUBUF  CD-CODE.

           MOVE 0 TO IO-FG.

       EXIT-CODE-SCAN.
           EXIT.


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
       SEND-PAYOFF-LINES-REC.
           MOVE REB-TYPE(SUB) TO LP-TRCD.
           MOVE POFF-REBATE(SUB) TO LP-TRAMT LP-APCUR.
           MOVE 0 TO LP-APOTH LP-APOT2 LP-APINT LP-APLC
                     LP-OTHBAL LP-OT2BAL LP-INTBAL LP-LCBAL.
           MOVE " " TO LP-REV.
           SUBTRACT LP-APCUR FROM LP-CURBAL.
           MOVE LN-1STPYDATE TO PDTH-DATE-WORK.
           PERFORM PAID-THRU-CALCULATION.
           MOVE PDTH-DATE-FULL TO LP-PDTH-DATE.
       GET-DL.
           PERFORM OPEN-DL1-FILE.
           PERFORM READ-DL1-FILE.
           PERFORM CLOSE-DL1-FILE.
       INSUFF-RSV-PRIN.
           MOVE "INSUFFICIENT AMOUNT IN RESERVE FROM PRINCIPAL"
                                                          TO MESS.
           PERFORM SEND-MESS.
       INSUFF-RSV-CHGS.
           MOVE "INSUFFICIENT AMOUNT IN RESERVE FROM CHARGES"
                                                          TO MESS.
           PERFORM SEND-MESS.
       INSUFF-RSV-PARTIC.
           MOVE "INSUFFICIENT AMOUNT IN PARTICIPATING RESERVE"
                                                          TO MESS.
           PERFORM SEND-MESS.
       CALL-PAYOFF.
           MOVE 0 TO IO-FG.
           MOVE LN-ACCTNO TO PASS-PRLNNO.
           MOVE LP-PAYDATE TO PASS-TRANS-DATE.
           IF LN-SALEFIN = "Y"
              MOVE "SC" TO PASS-CURRENT-PD-SRCD
           ELSE
              MOVE "BW" TO PASS-CURRENT-PD-SRCD.
           MOVE "LP/PAYOFX" TO FORM-NAM.
           MOVE "CALCULATING - PLEASE WAIT" TO MESS.
           PERFORM SEND-LEGEND.
           CALL FORM-PROGX USING FORM-PATH PASS-PAYOFF-FLDS.
           CANCEL FORM-PROGX.
           MOVE " " TO MESS.
           PERFORM SEND-LEGEND.
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
       EXCEEDS-PUR-BAL.
           MOVE LN-PURBAL TO PURBAL-EXCEEDED.
           MOVE PUR-BAL-EXCEEDED TO MESS.
           PERFORM SEND-MESS.
       REFUNDS-IN-EXCESS.
           MOVE "REFUNDS EXCEED PAYOFF BALANCE, POSTING NOT ALLOWED"
                                                            TO MESS.
           PERFORM SEND-MESS.
           PERFORM ACCEPT-SCREEN-RESET.
           PERFORM DISPLAY-ENTRY-UP.
       SEND-ENTRY-LINE.
           MOVE EB-BUF TO WR-BUF.
           MOVE ENTRY-LINE TO WR-LIST.
           PERFORM CALL-WRFORM.

      * SECTIONED COPY MEMBERS
       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/RBACT.CPY".
       COPY "LIBLP/LPUPER.CPY".
       COPY "LIBLP/SETCTBL.CPY".
       COPY "LIBLP/LPIBPC.CPY".
       COPY "LIBLP/LPAMTS.CPY".
       COPY "LIBLP/LPRATE.CPY".
       COPY "LIBLP/LPXDTE.CPY".
       COPY "LIBGB/SPPDTH.CPY".
       COPY "LIBLP/LPMDTE.CPY".
       COPY "LIBLP/LPBWIN.CPY".
       COPY "LIBLP/LPEARN.CPY".
       COPY "LIBGB/GPENV.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBLP/AFPDLLP.CPY".
       COPY "LIBLP/LPCDEL.CPY".
       COPY "LIBLP/LPCEPP.CPY".
       COPY "LIBLP/LPLPCL.CPY".
       COPY "LIBLP/LPLXGCL.CPY".
       COPY "LIBGB/CAPASSWD.CPY".

      ********************************************
       IO-ROUTINES SECTION.
      ********************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBLP/LPSPGS_SQL.CPY".
       COPY "LIBLP/LPLTPGS_SQL.CPY".
       COPY "LIBLP/LPLSXGS_SQL.CPY".
       COPY "LIBLP/LPLPGS_SQL.CPY".
       COPY "LIBLP/LPLNGS_SQL.CPY".
       COPY "LIBLP/LPDSGS_SQL.CPY".
       COPY "LIBLP/LPDLGS_SQL.CPY".
       COPY "LIBLP/LPCDGS_SQL.CPY".
       COPY "LIBLP/LPCAGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBLP/LPLP1RN.CPY".
       COPY "LIBLP/LPSP1RN.CPY".
       COPY "LIBLP/LPCA1RN.CPY".
       COPY "LIBLP/LPDS1RN.CPY".
       COPY "LIBLP/LPCD1RN.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBLP/LPWKI.CPY".
       COPY "LIBLP/LPLN1RN.CPY".
       COPY "LIBLP/LPDL1RN.CPY".
       COPY "LIBLP/LPLTP1RN.CPY".
       COPY "LIBLP/LPLSX1RN.CPY".

       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO ERRCD.
       END-ROUTINE.
           PERFORM CLOSE-FILES.
       EXIT-PROG.
       STOP-RUN.
           EXIT PROGRAM.
