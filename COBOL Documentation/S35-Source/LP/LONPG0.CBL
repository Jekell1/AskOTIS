       IDENTIFICATION DIVISION.
       PROGRAM-ID.      LONPG0.
       DATE-WRITTEN.    032587.
      *****************************************************************
      *                 (LP)
      *  DESCRIPTION:   LOAN ADDON DRIVER
      *****************************************************************
      *----------------------------------------------------------
      *----------------------------------------------------------
      *
      *                I  M  P  O  R  T  A  N  T
      *----------------------------------------------------------
      *
      * NOTE:
      *       IF THESE (NOT ALLOWED) ADDON CONDITIONS CHANGE,
      *          REMEMBER TO CHANGE UPDATES TO 'DS' & 'TY'
      *----
      *       IF ( LN-POCD-NONLOAN )
      *        OR (LN-CURBAL = 0)
      *         OR (LN-JDDATE NOT = 0)
      *          OR (LN-PLDATE NOT = 0)
      *           OR (LN-ACCELDATE NOT = 0)
      *            OR (LN-INT-REBATE = "Y" OR "R")
      *              OR (LN-BNKRPTDATE NOT = 0)
      *                   MOVE 8 TO LOAN-ERROR
      *                   GO TO END-PROCESS-ORIGINAL-LOAN.
      *
      *       IF NOT (LN-UNITPER-CD = "M" AND LN-UNITPER-FREQ = 1)
      *          MOVE 7 TO LOAN-ERROR
      *          GO TO END-PROCESS-ORIGINAL-LOAN.

      *    TEST CD-GC-FEE-EARNINGS-FG = 'Y' AND DON'T ALLOW ADDONS
      *    SINCE LOANS WITH CD-GC-FEE-EARNINGS-FG = 'Y' UPDATE
      *    THE LOAN'S ADDON ANTICIPATED
      *    PERFORM TEST-LN1-FEEUNERN.
      *       IF FEE-UNEARN NOT = 0
      *          MOVE 6 TO LOAN-ERROR
      *          GO TO END-PROCESS-ORIGINAL-LOAN.

      *----------------------------------------------------------
      *
      *  N O T E:
      *          LONPG3 IS CALLED IF ADDON TYPE IS ADDON
      *          LONPG4 UPDATE
      *
      *          LONPG5 IS CALLED IF ADDON TYPE IS ADJUSTMENT
      *          LONPG6 UPDATE
      *
      *          LONPG7 IS CALLED IF ADDON TYPE IS CANCEL
      *          LONPG8 UPDATE
      **************************************************************************
      * REV:
      *  MJD 051397 USE LN/PD-SMPRATE INSTEAD OF LN/PD-EFFRATE.
      *  MJD 052797 IF LOAN-KEY NOT = 0, THEN SKIP ACCOUNT NUMBER ENTRY
      *             AND GO DIRECTLY TO PROCESS-LOAN. (F1 EXIT FROM LONPG(3,5,7)
      *  JTG 082197 CHANGED TO USE 'LPZLCL' TO CLEAR-CALC-AREA-ZL
      *  GLF 090297 REMOVED ALL USE OF GLOBAL
      *  GLF 090897 REPLACED 'IO' ON GP-FILE WITH GET 'GPENV'
      *  GLF 101797 ADDED MOVE LN-APPNO TO EXT-WI-CL-APPNO
      *  JTG 030698 CORRECTED DISPLAY WHEN USING FIXED ANNUAL RATES, CASTLE #660
      *  JTG 061598 STOPPED THE POSTING OF ADDONS TO BANKRUPT ACCOUNTS
      *             NOTE: STATISTIC DELINQUENCY UPDATE IF ALLOWED
      *  KEC 090498 ADDED 'MOVE LC-LOANTYPE TO PD-LOANTYPE';
      *              FOR REGENCY REVOLVING.
      *  JTG 101598 MODIFIED TO NOT ALLOW ADDONS (VIA: LONPG) TO REVOLVING
      *             ACCOUNTS, REGENCY REVOL
      *  JTG 121698 DISPLAYED 'R' VIA UNITPER-MASK LOGIC FOR REVOLVING ACCOUNT
      *  JTG 991109 ALLOWED ACCOUNTS TO HAVE ADDON'S WHEN IN REPO STATUS,
      *             CHANGED UPD OF DSFILE, TYFILE & LPEOD5TR, NEWPORT #244, A15
      *  JTG 000113 ADDED SP-INSREJECT-FG OPTION 'C', WHICH WILL CAUSE WRITING
      *             NO. OF MONTHS INSURANCE NOT IN VIOLATION, WISCONSIN #1032
      *  BAH 000208 PASSWORD TO NOT ALLOW ADJUSTMENT, REGACC PR# 1024
      *  MJD 000217 ADDED GP-OT-INS-TERM (TFC PR#48) IF 'Y' THEN ALLOW
      *             ALLOW TERM ENTRY FOR O2O3O4O5 INSURANCE.
      *  MJD 000601 ADDED LOGIC TO PREVENT LOAN ADDONS IF THE ACCOUNT
      *             IS NON MONTHLY. ALSO REPLACED IO-FG TEST WITH
      *             IO-BAD & IO-GOOD, REPLACED NON FILE RELATED TESTS
      *             OF IO-FG WITH ERROR FLAGS
      * JTG 020304 ADDED "CT" COUNTY TAX CODE KENTUCKY AND PERCENTS
      *            ADDED "CY" CITY   TAX CODE KENTUCKY AND PERCENTS
      *            REMOVED THE NOT NEEDED REFERENCE TO CIP/COP        WORLD #288
      * BLV 020823 PER JOHN, DON'T ALLOW ADD-ON IF LN-MIP-FG IS "Y",
      *            REGWAN PR# 1801
      * JTG 021212 ADDED "CT" COUNTY TAX CODE KENTUCKY AND PERCENTS
      *            ADDED "CY" CITY   TAX CODE KENTUCKY AND PERCENTS
      *            STOPPED POSTING ADDONS IF BR-FORCE-VALID-CITY-COUNTY = "Y"
      *            AND BORROWER #1 RESIDES IN ON FILE CITY/COUNTY     WORLD #288
      * JTG 030404 CORECTED READ ON LN-COLLCD, MOVED 'CO' TO CD1-KEY
      * BAH 050525 WAS NOT ALLOWING AN ADDON IF BR-FORCE-VALID-CITY-COUNTY = "Y"
      *            AND BW HAD VALID CODES! TESTING ALLOW-BW-UPDATE-FG = "Y",
      *            SHOULD BE TESTING ALLOW-BW-UPDATE-FG = "N", LENDMARK
      *  CS 050616 WAS NOT ALLOWING AN ADDON IF BR-FORCE-VALID-CITY-COUTY = "Y"
      *            BUT NO ENTRY EXISTED FOR STATE/CITY/COUNTY IN CDFILE. FOUND
      *            WHEN TRYING TO BOOK AN ADDON
      * JTG 050623 ADDED CHANGES FOR REPORTING AND TAKING FEE CODE EARNINGS
      *            CREATED CD-GC-FEE-EARNINGS-FG         LENDMARK #2542
      * BAH 051014 DISPLAY A WARNING MESSAGE IF PAY SCHEDULE UP TO 14 ALREADY
      *            KEVIN/PFSC
      * BLM 090903 DON'T PERFORM GET-L-P-REC, JUST LOAD KEY & READ LT.  ACCT
      *            WITH PMT SCHEDULE & NO COLLATERAL ENDED UP WITH SPACES IN
      *            PMT SCHEDULE & GAVE PMT SCHEDULE TOO LARGE WARNING
      *  CS 100309 ADDED CHECK IF LN-ESCROW-FG 'Y', ADDONS ARE NOT ALLOWED,
      *            REGWAN PR# 3506
      * MJD 140723 ADDED SP-ADDON-RATE-FG = "A"           MULLEN/KSTREET PR#4556
      * BAH 140814 ADDED TEST FOR LPSMNU IN HELP
      * BAH 160118 SPLIT OUT LTFILE, PD0003
      *
      * KEC 2016.0315 {PD#00003} PARADATA <A30> [JAY CISZEWSKI]
      *         CHANGED CD-FEE???? TO CD-GC-????.
      *
      * KEC 2016.0509 {PD#00003} PARADATA <A30> [JAY CISZEWSKI]
      *         CHANGED LN-APPNO TO USE LN-APP-NUMBER FOR MA1-KEY.
      *  CS 170406 ACTIVATE 8 DIGIT DATES
      *  CS 170410 MOVED THE TEST ON LOAN-ERROR = 9 AFTER PROCESS ORIGINAL
      *            LOAN. I ENTERED A VALID ACCT NUMBER, F1'D, AND ENTERED
      *            AN ACCT NUMBER THAT DIDNT EXIST, THE TEST ON LN-PAY-SCHD-
      *            FG WAS "Y" SO IT WENT TO IO ERROR ON THE READ OF LT1-FILE 
      * BAH 170619 ACTIVATE 8 DIGIT DATES, PD0006
      * BAH 180119 REMOVED LPBAGE, NOT USED, PL#986
      * KEC 2018-0824 LOAN APP MODULE REMOVAL FROM A30 [LARM30];
      *         LEAVING LN-APP-NUMBER & EXT-WI-CL-APP-NUMBER
      *         HERE IN CASE NEEDED IN THE FUTURE.
      * BAH 190122 SPLIT CDFILE
      * BAH 20190625 REMOVED LC-REVOLVING, PT#0153
      * BAH 20190627 REMOVED LN-REVOLVING, PT0153
      * BAH 2022.0314 REMOVED EXT-SPANISH-FORMS AND ALL MX FORMS
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
           "@(#)LP/LONPG0.CBL S35 04/17/24-08:30:51 >".
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBLP/LP01DL.CPY".
       COPY "LIBLP/LP01DL_SQL.CPY".
       COPY "LIBLP/LPWSDL.CPY".
       COPY "LIBLP/LP01LC.CPY".
       COPY "LIBLP/LP01LC_SQL.CPY".
       COPY "LIBLP/LPWSLC.CPY".
       COPY "LIBLP/LP01SP.CPY".
       COPY "LIBLP/LP01SPA.CPY".
       COPY "LIBLP/LP01SPB.CPY".
       COPY "LIBLP/LP01SPC.CPY".
       COPY "LIBLP/LP01SP_SQL.CPY".
       COPY "LIBLP/LPWSSP.CPY".
       COPY "LIBLP/LP01BW.CPY".
       COPY "LIBLP/LP01BW_SQL.CPY".
       COPY "LIBLP/LPWSBW.CPY".
       COPY "LIBLP/LP01CD.CPY".
       COPY "LIBLP/LP01CD_SQL.CPY".
       COPY "LIBLP/LPWSCD.CPY".
       COPY "LIBLP/LP01CDB.CPY".
       COPY "LIBLP/LP01CDB_SQL.CPY".
       COPY "LIBLP/LPWSCDB.CPY".
       COPY "LIBLP/LP01PD.CPY".
       COPY "LIBLP/LP01LTC.CPY".
       COPY "LIBLP/LP01LTC_SQL.CPY".
       COPY "LIBLP/LPWSLTC.CPY".
       COPY "LIBLP/LP01LTP.CPY".
       COPY "LIBLP/LP01LTP_SQL.CPY".
       COPY "LIBLP/LPWSLTP.CPY".
       COPY "LIBLP/LP01LN.CPY".
       COPY "LIBLP/LP01LN_SQL.CPY".
       COPY "LIBLP/LPWSLN.CPY".

       01  WK-KEY          PIC 9(2).

       01  LOAN-ERROR      PIC 9(01).
       01  SCAN-ERROR      PIC X(01).
       01  CODE-ERROR      PIC X(01).

       01  MSEL-LIST       PIC X(5)      VALUE "MSEL.".
       01  ELE             PIC 9(6).
       01  IO-TYPE         PIC X(3).
       01  NEW-FLAG        PIC 9.
       01  FIRST-ELE       PIC 99 VALUE 0.
       01  DO-STATE-CY-EXIST-FG  PIC X        VALUE "N".
       01  DO-STATE-CT-EXIST-FG  PIC X        VALUE "N".
       01  ALLOW-BW-UPDATE-FG    PIC X        VALUE "N".
       01  ALLOW-CITY-SCAN-FG    PIC X        VALUE "N".
       01  ALLOW-COUNTY-SCAN-FG  PIC X        VALUE "N".

       01  FEE-UNEARN      PIC S9(7)V99 VALUE 0.


      *----------------------------------------------------------------
      * REPLACED SEVERAL OCCURANCES OF IO-FG WITH BW-ERR. IO-FG WAS USED
      * AS AN ERROR FLAG UNRELATED TO I/O. ADDING BA-FILE READ CHANGED
      * IO-FG WHICH AFFECTED FLOW OF PROGRAM.
      *----------------------------------------------------------------
       01  BW-ERR             PIC X        VALUE "N".
       01  HOLD-BWNAME        PIC X(33).
       01  FEECODE-TABLE      VALUE " ".
           03  FEECODE        OCCURS 4 TIMES
                              PIC XX.

       01  IX1             PIC 99.
       01  IX2             PIC 99.
       01  SUB             PIC 9(6) COMP-3.
       01  INSPECT-W       PIC X(86).
       01  ADD-ON          PIC X(25).
      *-----------------------------------------------------------------
       01  ALP-BW.
           03  ALP-BW123   PIC X(3).
           03  ALP-HYPHEN1 PIC X.
           03  ALP-BW45    PIC X(2).
           03  ALP-HYPHEN2 PIC X.
           03  ALP-BW6789  PIC X(4).
       01  NUM-BW          PIC 9(9).
       01  NUM-BWX REDEFINES NUM-BW.
           03  NUM-BW123   PIC X(3).
           03  NUM-BW45    PIC X(2).
           03  NUM-BW6789  PIC X(4).
       01  W-LNNO        PIC 9(8).

      *----------------------------------------------------------------
      * LONPGW CONTAINS  DISPLAY-ORIG-BUF
      *                  DISPLAY-ORIG-LIST
      *                  1STPY-BUF
      *                  1STPY-LIST
      *                  DISPLAY-BUF1
      *                  DEF-BUF
      *                  DEF-LIST
      *                  DF2-BUF
      *                  DF2-LIST
      *                  LONPC2-BUF
      *                  LONPC2-LIST
      *                  MESSAGE-BUF
      *                  MESSAGE-LIST
      *                  MESS-LIST
      *                  MAX
      *                  SPEC-TABLE
      *                  SPEC-TABLE2 (SPEC, CHFG)
      *----------------------------------------------------------------
       COPY "LIBLP/LONPGW.CPY".

       01  SCREEN-LINES.
           03  SCREEN-LINE1     PIC X(20).
           03  SCREEN-LINE2     PIC X(20).
           03  SCREEN-LINE3     PIC X(20).
       01  MSG-LINES           REDEFINES SCREEN-LINES.
           03  MSG-LINE1        PIC X(30).
           03  MSG-LINE2        PIC X(30).
       01  TERM-LOW             PIC 9(3)    COMP.
       01  TERM-HIGH            PIC 9(3)    COMP.
       01  PROCEEDS-LOW         PIC 9(6)V99 COMP.
       01  PROCEEDS-HIGH        PIC 9(6)V99 COMP.

       COPY "LIBWI/BWLNSCW.CPY".
       COPY "LIBWI/BWLNS2W.CPY".
       COPY "LIBWI/MEMOSCW.CPY".

       COPY "LIBLP/LPWKPATH.CPY".
       COPY "LIBGB/SYSTEMW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/GPENVW.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBLP/LPUPWKW.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBLP/LPMENUW.CPY".
       COPY "LIBLP/LPITRMW.CPY".
       COPY "LIBLP/LPMDTEW.CPY".
       COPY "LIBCL/CLMENUW_EXT.CPY".
       COPY "LIBLP/LPSTATW.CPY".
       COPY "LIBGB/PASSWDW.CPY".
       01  ADJ-SEQ            PIC 99 VALUE 00.
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBLP/LONPG_DEF.CPY".
       COPY "LIBLP/LPSMNU_DEF.CPY".
       COPY "LIBLP/LPLNMU_DEF.CPY".
       COPY "LIBLP/LPCAMU_DEF.CPY".
       COPY "LIBLP/LPVDMU_DEF.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBLP/LONPG_WKS.CPY".
       COPY "LIBLP/LPSMNU_WKS.CPY".
       COPY "LIBLP/LPLNMU_WKS.CPY".
       COPY "LIBLP/LPCAMU_WKS.CPY".
       COPY "LIBLP/LPVDMU_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".

       COPY "LIBGB/GETFMW.CPY".
       COPY "LIBLP/LPLOANW.CPY".
       COPY "LIBLP/LPPFBUF.CPY".

       SCREEN SECTION.
       COPY "LIBLP/LONPG_SCN.CPY".
       COPY "LIBLP/LPSMNU_SCN.CPY".
       COPY "LIBLP/LPLNMU_SCN.CPY".
       COPY "LIBLP/LPCAMU_SCN.CPY".
       COPY "LIBLP/LPVDMU_SCN.CPY".

       PROCEDURE DIVISION
           USING FORM-PATHNAME EXIT-PATHNAME PROG-BUF UPDATE-BUF.
       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
               WK-FILE.
       COPY "LIBGB/DECLARE.CPY".
           PERFORM CLOSE-SP1-FILE.
           PERFORM CLOSE-LTP1-FILE.
           PERFORM CLOSE-LTC1-FILE.
           PERFORM CLOSE-LN1-FILE.
           PERFORM CLOSE-LC1-FILE.
           PERFORM CLOSE-DL1-FILE.
           PERFORM CLOSE-CDB1-FILE.
           PERFORM CLOSE-CD1-FILE.
           PERFORM CLOSE-BW1-FILE.
           PERFORM CLOSE-BR-FILE.
           CLOSE WK-FILE.
       COPY "LIBGB/DECLAR2.CPY".

      ***********************************
      *   C O N T R O L   M O D U L E   *
      ***********************************
       MAIN-PROGRAM SECTION.

           PERFORM SQL-CONNECT.

      D    STOP MESS.
           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "LONPG" TO VDU-FORM-NAME.

           MOVE USER-PID TO TEMP-PID.
           MOVE " " TO ERRCD.

           MOVE BRANCH TO BR-NO.
           PERFORM OPEN-BR-FILE.
           PERFORM READ-BR-FILE.
           PERFORM CLOSE-BR-FILE.
           IF IO-BAD
              GO TO IO-ERROR.

           PERFORM GET-GPENV.

           PERFORM RESTORE-SCREEN.
           PERFORM CREATE-TRAILER.

       MAIN-MODULE.
           PERFORM FORM-RESET.

           MOVE " " TO MESS
           PERFORM MSG-LINE.

      * DO NOT ALLOW F9-INTERRUPT UNTIL AFTER LOAN IS RECORD IS READ
           MOVE " " TO VDU-CLMENU-SELECTION.

           PERFORM ENTER-KEYS.
           IF IO-TYPE = "CLR"
              GO TO MAIN-MODULE.
           IF IO-TYPE = "END"
              GO TO END-ROUTINE.

           GO TO MAIN-MODULE.

      ******************************************************************
       ENTER-KEYS SECTION.
      ******************************************************************
       ENTER-KEYS-NO.
           MOVE 0 TO NEW-FLAG
                     ELE.
           MOVE SPACES TO IO-TYPE.

           IF LOAN-KEY NOT = 0
              MOVE SPACES TO WR-BUF
              MOVE DISPLAY-RESET-LIST TO WR-LIST
              PERFORM CALL-WRFORM
              PERFORM NEW-RECORD-SETUP
              MOVE LOAN-KEY TO W-LNNO ADDON-LN-ACCTNO
              MOVE 0 TO LOAN-KEY
              GO TO PROCESS-LOAN
           ELSE
              PERFORM NEW-RECORD-SETUP.

       ENTER-LC-CODE.
      * DISABLE CTRL 'N' INTERRUPT:
           MOVE " " TO VDU-CTRL-N-PROG.

           IF NOT (BR-INS-ADDON = "Y" OR "O")
              MOVE "ADDONS ARE NOT ALLOWED" TO MESS
              PERFORM SEND-MESS
              GO TO MENU-KEY.

           MOVE "ENTER ACCOUNT NUMBER, F6-SCAN, F7-MENU" TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "EN N160LOANKEY." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "1>"
              GO TO MENU-KEY.

           IF VDUSTATUS = "1<"
              PERFORM BWLN-SCAN
              IF SCAN-ERROR = "Y"
                 GO TO ENTER-LC-CODE
              ELSE
                 MOVE LN-ACCTNO TO W-LNNO ADDON-LN-ACCTNO
                 GO TO PROCESS-LOAN.

           IF VDUSTATUS NOT = "00"
              GO TO ENTER-LC-CODE.

           IF VDUNUM0 = 0
              MOVE "ACCOUNT NUMBER REQUIRED FOR ADDONS!"
                    TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-LC-CODE.

           MOVE VDUNUM0 TO W-LNNO ADDON-LN-ACCTNO.

       PROCESS-LOAN.
           MOVE 0 TO LOAN-ERROR.
           PERFORM PROCESS-ORIGINAL-LOAN.
           IF LOAN-ERROR = 9
              MOVE "INVALID LOAN NUMBER" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-LC-CODE.
             
           IF LN-PAY-SCHLD-FG = "Y"
              MOVE LN-OWNBR  TO LTP-BRNO
              MOVE LN-ACCTNO TO LTP-ACCTNO
              MOVE 1         TO LTP-SEQNO
              PERFORM OPEN-LTP1-FILE
              PERFORM READ-LTP1-FILE
              PERFORM CLOSE-LTP1-FILE
              IF IO-FG NOT = 0
                 MOVE "PAYMENT SCHEDULE MISSING" TO MESS
                 PERFORM SEND-MESS
                 GO TO IO-ERROR
              ELSE
                 IF LTP-SCHLD-TERM(14) NOT = 0
                   MOVE "WARNING: PAY SCHEDULE GETTING TOO LARGE (14)"
                           TO MESS
                    PERFORM SEND-MESS.
           IF LOAN-ERROR = 6
              MOVE "ADDON NOT ALLOWED LOAN FEES WITH FEE EARNINGS (Y)"
                                                               TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-LC-CODE.
           IF LOAN-ERROR = 7
              MOVE "ADDONS NOT ALLOWED ON NON-MONTHLY ACCOUNTS" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-LC-CODE.
           IF LOAN-ERROR = 8
              MOVE "ADDON IS NOT ALLOWED ON THIS LOAN" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-LC-CODE.

           IF SP-ADDON-ORG-MATURITY = "O"
              MOVE "Y" TO MDTE-ORGTERM-FG.

           PERFORM MATURITY-DATE-CALCULATION.
           IF TRANS-DATE NOT < MDTE-DATE
              MOVE "ADDONS NOT ALLOWED ON OR AFTER MATURITY " TO MESS
              PERFORM SEND-MESS
              MOVE "CLR" TO IO-TYPE
              GO TO EXIT-ENTER-KEYS.

           MOVE TEMP-PATH  TO EXT-WI-CL-WK-PATH.
           MOVE TRANS-DATE TO EXT-WI-CL-TRANS-DATE.
           MOVE LN-OWNBR   TO EXT-WI-CL-OWNBR.
           MOVE LN-ACCTNO  TO EXT-WI-CL-ACCTNO.
      *    MOVE LN-APP-NUMBER TO EXT-WI-CL-APP-NUMBER.
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 4
              MOVE LN-SSNO(SUB)    TO EXT-WI-CL-SSNO(SUB)
              MOVE LN-MAKERCD(SUB) TO EXT-WI-CL-MAKERCD(SUB)
           END-PERFORM.
           MOVE LN-DLNO                   TO EXT-WI-CL-DLNO.
           MOVE 1                         TO EXT-WI-CL-SSNO-NO.
           MOVE LN-SALEFIN                TO EXT-WI-CL-SALEFIN.
           MOVE EXT-CONAME-DAILY-PASSWORD TO EXT-WI-CL-PASSWD.
           MOVE LN-PAY-SCHLD-FG           TO EXT-WI-CL-PAY-SCHLD-FG.
           MOVE EXT-CONAME-SYSDATE        TO EXT-WI-CL-SYSDATE.

      * ALLOW F9-INTERRUPT
      
           MOVE "F" TO VDU-CLMENU-SELECTION.

      * ALLOW CTRL 'N' INTERRUPT:
      
           MOVE "WI/MAKERS" TO VDU-CTRL-N-PROG.

       ENTER-ADDON-CODE.
           STRING "F1,F2,F3,F4,F5,F9,   "
                  "ENTER: '1'-ADDON, '2'-ADJUSTMENT, '3'-CANCEL"
                  "                    "
                  DELIMITED BY SIZE INTO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "E  A010ADONTYPE." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "1U"
              MOVE "CLR" TO IO-TYPE
              GO TO EXIT-ENTER-KEYS.

           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO EXIT-ENTER-KEYS.

           IF VDUSTATUS = "12"
              PERFORM CALL-MEMO
              GO TO ENTER-ADDON-CODE.
           IF VDUSTATUS = "16"
              PERFORM DISPLAY-ALL-LOANS
              GO TO ENTER-ADDON-CODE.
           IF VDUSTATUS = "18"
              PERFORM SEND-PAYMENT-LINES
              GO TO ENTER-ADDON-CODE.
           IF VDUSTATUS = "1:"
              PERFORM SEND-PAYOFF
              GO TO ENTER-ADDON-CODE.

           IF VDUSTATUS = "1>"
              GO TO MENU-KEY.

           IF VDUSTATUS NOT = "00"
              GO TO ENTER-ADDON-CODE.

      * F3 - ADDON (LONPG3)
           IF VDUBUF = "1"
              MOVE 3 TO NEW-PROG-SUB
              MOVE "P" TO ERRCD
              MOVE "END" TO IO-TYPE
              GO TO EXIT-ENTER-KEYS.

      * F4 - ADJUSTMENT (LONPG5)
           IF VDUBUF = "2"
              MOVE ADJ-SEQ TO PASSWDW-SEQ
              MOVE "TO ALLOW AN ADJUSTMENT....:" TO PASSWDW-PROMPT2
              PERFORM VERIFY-PASSWORD
              IF PASSWDW-STAT NOT = "OK"
                 GO TO ENTER-ADDON-CODE
              ELSE
                 MOVE 5 TO NEW-PROG-SUB
                 MOVE "P" TO ERRCD
                 MOVE "END" TO IO-TYPE
                 GO TO EXIT-ENTER-KEYS.

      * F5 - CANCEL (LONPG7)
           IF VDUBUF = "3"
              MOVE 7 TO NEW-PROG-SUB
              MOVE "P" TO ERRCD
              MOVE "END" TO IO-TYPE
              GO TO EXIT-ENTER-KEYS.

           GO TO ENTER-ADDON-CODE.

      * END KEY ENTRY:
       MENU-KEY.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY LONPG-SCREEN.
           DESTROY ALL CONTROLS.

           PERFORM MENU-SELECTION.

           MOVE 0 TO ELE.
           PERFORM SAVE-SCREEN.
           IF MENU-FG = 99
              GO TO ENTER-KEYS.

           MOVE "END" TO IO-TYPE.

       EXIT-ENTER-KEYS.
           EXIT.

      ******************************************************************
       NEW-RECORD-SETUP SECTION.
      ******************************************************************
           MOVE 1 TO NEW-FLAG.

           PERFORM CLEAR-PD-FILE.

      * CANT BE IN COPY MEMBER BECAUSE THESE ARE IN REDEFINE OF
      * FREE TYPE WHICH MUST BE SPACES IN "LONPB0"
           MOVE 0 TO PD-PRPAID-AHEAD PD-DUAL-LOAN-ADJ.

           MOVE SPACES TO SCREEN-LINES 
                          HOLD-BWNAME.

           MOVE TRANS-DATE TO DATE-YYYYMMDD PD-LOANDATE.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY  TO DEF-LOANDATE.

           MOVE BR-STATE TO DEF-ORGST
                            PD-ORGST
                            PD-OWNST.
           MOVE BRANCH TO DEF-ORGBR
                          PD-ORGBR
                          PD-OWNBR.

           IF BR-DEFAULT-LN-PURCD NOT = " "
              MOVE BR-DEFAULT-LN-PURCD TO PD-PURCD
                                          CD-CODE
              MOVE "PU"                TO CD-TYPE
              MOVE 8 TO ELE
              PERFORM SEND-CD-DESC
              IF CODE-ERROR = "Y"
                 MOVE " " TO CHFG(8).

           MOVE 999.99 TO PD-SERVPER.
           MOVE 999.999 TO PD-ENDORATE
                           PD-OVRDRATE.
           MOVE "C" TO PD-OVRDRATE-FG PD-INS-FG(1) PD-INS-FG(2)
                       PD-INS-FG(3) PD-SERCHG-FG PD-MAINTFEE-FG
                       PD-EXTCHG-FG PD-REGPYAMT-FG.
           MOVE 0 TO TERM-LOW TERM-HIGH
                     PROCEEDS-LOW PROCEEDS-HIGH.

           MOVE DEF-BUF TO WR-BUF.
           MOVE DEF-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.
           MOVE "N" TO STAT.
           MOVE 9999.99 TO PD-DLNO
                           DL-DLNO.
           PERFORM GET-DL1.

           IF BR-LC-CODE = "Y"
              MOVE DL-LC-CODE TO PD-LC-CODE.

           MOVE 0 TO ELE
                     PD-CAN-LOAN-MAX.

           PERFORM CREATE-TRAILER.

      ******************************************************************
       RESTORE-SCREEN SECTION.
      ******************************************************************
           MOVE 8 TO WK-KEY.
           PERFORM OPEN-WK-FILE.
           PERFORM READ-WK-FILE.
           IF NEW-PROG-FORM NOT = OLD-PROG-FORM
              MOVE OLD-PROG-FORM TO WK-OLD-PROG-FORM.
           MOVE NEW-PROG-FORM TO OLD-PROG-FORM.
           IF IO-GOOD
              GO TO RESTORE-CLEAR.
           MOVE " " TO WK-SAVE-RECORD.
           MOVE 0 TO WK-ELE.
           PERFORM WRITE-WK-FILE.
           IF IO-BAD
              GO TO IO-ERROR.

           GO TO EXIT-RESTORE-SCREEN.

       RESTORE-CLEAR.
           IF WK-ELE NOT = 0
              SUBTRACT 1 FROM WK-ELE GIVING FIRST-ELE.

           MOVE WK-SAVE-RECORD TO PD-RECORD.
           MOVE 0 TO WK-ELE.
           PERFORM REWRITE-WK-FILE.

       EXIT-RESTORE-SCREEN.
           IF FIRST-ELE = 0
              MOVE 0 TO NEW-FLAG
           ELSE
              MOVE 1 TO NEW-FLAG.
           PERFORM CLOSE-WK-FILE.

      ******************************************************************
       CREATE-TRAILER SECTION.
      ******************************************************************
           MOVE 6 TO WK-KEY.
           PERFORM OPEN-WK-FILE.
           PERFORM READ-WK-FILE.
           MOVE " " TO WK-SAVE-RECORD.
           IF IO-GOOD
              PERFORM REWRITE-WK-FILE
           ELSE
              MOVE 0 TO WK-ELE
              PERFORM WRITE-WK-FILE
              IF IO-BAD
                 GO TO IO-ERROR.

           PERFORM CLOSE-WK-FILE.


      *************************************
       SAVE-SCREEN SECTION.
      *************************************
           MOVE 8 TO WK-KEY.
           PERFORM OPEN-WK-FILE.
           PERFORM READ-WK-FILE.
           IF IO-BAD
              GO TO IO-ERROR.

           IF MENU-RESUME = "Y"
              MOVE "N" TO MENU-RESUME
              IF WK-OLD-PROG-FORM = "  "
                 MOVE 99 TO MENU-FG
                 GO TO EXIT-SAVE-SCREEN
              ELSE
                 MOVE WK-OLD-PROG-FORM TO NEW-PROG-FORM
                                          OLD-PROG-FORM
                 MOVE " " TO WK-OLD-PROG-FORM.

           MOVE ELE TO WK-ELE.
           MOVE PD-RECORD TO WK-SAVE-RECORD.
           PERFORM REWRITE-WK-FILE.

       EXIT-SAVE-SCREEN.
           PERFORM CLOSE-WK-FILE.

      ***********************************************
       SAVE-ORIG-LOAN-REC SECTION.
      ***********************************************
           MOVE 9 TO WK-KEY.
           PERFORM OPEN-WK-FILE.
           PERFORM READ-WK-FILE.
           IF IO-BAD
              MOVE " " TO WK-SAVE-RECORD
              MOVE LN-REC TO WK-SAVE-REC
              PERFORM WRITE-WK-FILE
              IF IO-BAD
                 GO TO IO-ERROR
              END-IF
           ELSE
              MOVE LN-REC TO WK-SAVE-REC
              PERFORM REWRITE-WK-FILE.

       SAVE-ORIG-LOAN-REC-EXIT.
           PERFORM CLOSE-WK-FILE.

      *****************************************
       PROCESS-ORIGINAL-LOAN SECTION.
      *****************************************

           MOVE BRANCH TO LN-OWNBR.
           MOVE W-LNNO TO LN-ACCTNO.
           PERFORM OPEN-LN1-FILE.
           PERFORM READ-LN1-FILE.
           PERFORM CLOSE-LN1-FILE.
           IF IO-BAD
              MOVE 9 TO LOAN-ERROR
              GO TO END-PROCESS-ORIGINAL-LOAN.

           IF (LN-ESCROW-FG = "Y" OR LN-ESCROW-FG = "I")
              MOVE 8 TO LOAN-ERROR
              GO TO END-PROCESS-ORIGINAL-LOAN.

           IF BR-FORCE-VALID-CITY-COUNTY NOT = "Y"
              MOVE "Y" TO ALLOW-BW-UPDATE-FG
           ELSE
              MOVE LN-SSNO(1) TO BW-SSNO
              PERFORM OPEN-BW1-FILE
              PERFORM GET-BW1
              PERFORM CLOSE-BW1-FILE
              PERFORM SET-CY-CT-SCAN
              IF ALLOW-CITY-SCAN-FG = "Y" OR
                 ALLOW-COUNTY-SCAN-FG = "Y"
                 PERFORM VALIDATE-REQUIRED-FIELD
              ELSE
                 MOVE "Y" TO ALLOW-BW-UPDATE-FG.

      *****************************************************
      *           I  M  P  O  R  T  A  N  T
      *
      * NOTE:
      *       IF THESE (NOT ALLOWED) ADDON CONDITIONS,
      *       REMEMBER TO CHANGE UPDATES TO 'DS' & 'TY'
      *****************************************************
           IF ( LN-POCD-NONLOAN )
            OR (LN-CURBAL = 0)
             OR (LN-JDDATE NOT = 0)
              OR (LN-PLDATE NOT = 0)
               OR (LN-ACCELDATE NOT = 0)
                OR (LN-INT-REBATE = "Y" OR "R")
                  OR (LN-BNKRPTDATE NOT = 0)
                    OR (LN-MIP-FG = "Y")
                     OR (ALLOW-BW-UPDATE-FG = "N")
                        MOVE 8 TO LOAN-ERROR
                        GO TO END-PROCESS-ORIGINAL-LOAN.

           IF NOT (LN-UNITPER-CD = "M" AND LN-UNITPER-FREQ = 1)
             MOVE 7 TO LOAN-ERROR
             GO TO END-PROCESS-ORIGINAL-LOAN.

      * TEST CD-FEE-EARNINGS-FG = 'Y' AND DON'T ALLOW ADDONS
      * SINCE LOANS WITH CD-GC-FEE-EARNINGS-FG = 'Y' UPDATE
      * THE LOAN'S ADDON ANTICIPATED
      
           PERFORM TEST-LN1-FEEUNERN.
           IF FEE-UNEARN NOT = 0
              MOVE 6 TO LOAN-ERROR
              GO TO END-PROCESS-ORIGINAL-LOAN.

           MOVE " " TO DISPLAY-ORIG-BUF.

      *  SAVE LOAN RECORD TO VERIFY RECORD HASN'T CHANGED WHEN UPDATE
      *  TAKES PLACE:

           PERFORM SAVE-ORIG-LOAN-REC.

      * STORE THE ORIGINAL LOAN SOURCE CODE IN POCD FOR THE UPDATE
      * PROGRAM.
           MOVE LN-SRCD TO PD-POCD.

           MOVE LN-CLASS TO LC-CODE.
           PERFORM GET-LC.

           MOVE LC-CODE     TO PD-CLASS.
           MOVE LC-SMALLOAN TO PD-SMALLOAN.
           MOVE LC-SALEFIN  TO PD-SALEFIN.
           MOVE LC-SERVACCT TO PD-SERVACCT.
           MOVE LC-LOANTYPE TO PD-LOANTYPE.
           MOVE 1 TO PD-STMT-COUNTER.

           PERFORM OPEN-BW1-FILE.
           MOVE 1 TO IX1.
       PROCESS-NEXT-BW.
           MOVE LN-SSNO(IX1) TO BW-SSNO PD-SSNO(IX1).
           PERFORM NUMBW-TO-ALPBW.
           MOVE " " TO BW-LNAME BW-FNAME.
           IF BW-SSNO NOT = 0
              IF IX1 < 3
                 PERFORM GET-BW1
                 MOVE ALP-BW TO D-SSNO(IX1)
                 MOVE INSPECT-W TO D-BORR(IX1)
                 MOVE LN-MAKERCD(IX1) TO D-MAKERCD(IX1)
              END-IF
              MOVE LN-MAKERCD(IX1) TO PD-MAKERCD(IX1).
           IF IX1 = 1
              PERFORM LPSTAT-DISPLAY
              MOVE LPS-DISPLAY-LNNO TO D-LOAN-NUM.
           IF IX1 < 4
              ADD 1 TO IX1
              GO TO PROCESS-NEXT-BW.
           PERFORM CLOSE-BW1-FILE.

           MOVE LN-ORGST TO PD-ORGST.
           MOVE LN-SPRCLASS TO PD-SPRCLASS.
           MOVE LN-SUBCLASS TO PD-SUBCLASS.

           MOVE LN-LOANDATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY   TO ALP-DATE-EDIT.
           MOVE LN-UNITPER-CD TO DATER-UNITPER-CD.
           MOVE LN-UNITPER-FREQ TO DATER-UNITPER-FREQ.
           PERFORM UNITPER-MASK.
           MOVE ALP-DATE-EDIT TO D-LOANDATE.

           MOVE LN-LAWCODE TO PD-LAWCODE.
           PERFORM MATURITY-DATE-CALCULATION.
           MOVE MDTE-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO D-MDTE.
           MOVE LN-ORGTERM TO D-OTRM.
           IF LN-ORGTERM NOT = LN-SUM-ORGTERM
              MOVE LN-SUM-ORGTERM TO D-UTRM.
           MOVE PD-LOANDATE TO PD-INTDATE.
           MOVE LN-FILEDATE TO PD-FILEDATE.

           PERFORM GET-SP.

      * DISPLAY OF PD-SMPRATE FOR EFFECTIVE IS CORRECT:
           MOVE LN-SMPRATE TO PD-SMPRATE
                              D-EFF
                              D-SMP.
           MOVE LN-EFFRATE TO PD-EFFRATE.
           MOVE LN-APRATE  TO PD-APRATE
                              D-APR.

           IF SP-ADDON-RATE-FG = "F"
              MOVE SP-ADDON-RATE TO PD-OVRDRATE
              MOVE "F" TO PD-OVRDRATE-FG
           ELSE
           IF SP-ADDON-RATE-FG = "A"
              MOVE LN-APRATE TO PD-OVRDRATE
                                PD-SMPRATE
                                D-EFF
                                D-SMP
              MOVE "A" TO PD-OVRDRATE-FG
           ELSE
              MOVE "E" TO PD-OVRDRATE-FG.

           MOVE PD-OVRDRATE    TO D-OVR.
           MOVE PD-OVRDRATE-FG TO D-OVR-FG.

           MOVE PD-1STPYDATE TO PD-ORIG-1STPYDATE DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO D-1STPYDATE.
           MOVE PD-ORGTERM  TO D-ORGTERM.
           MOVE LN-ORGST    TO D-ORGST PD-ORGST PD-OWNST.
           MOVE LN-ORGBR    TO D-ORGBR PD-ORGBR PD-OWNBR.
           MOVE SP-SUBCLASS TO D-SUBCLASS.
           MOVE SP-DESC     TO D-SUBDESC.

           MOVE "PU"     TO CD-TYPE.
           MOVE LN-PURCD TO CD-CODE PD-PURCD.
           PERFORM GET-CD.

           MOVE "CO"      TO CD-TYPE.
           MOVE LN-COLLCD TO CD-CODE D-COLCD PD-COLLCD.
           PERFORM GET-CD.
           MOVE CD-DESC TO D-COLDESC.

           MOVE 0 TO PD-PROCEEDS.

      * SETUP SO INSURANCE ENTRY WILL BE CORRECT
      
           MOVE LN-INSCODES TO D-INSCODE PD-INSCODES.

           MOVE LN-DLNO TO D-DLNO DL-DLNO PD-DLNO.
           PERFORM GET-DL1.
           MOVE DL-SHORT TO D-DLDESC.

           MOVE LN-COLLID TO PD-COLLID.

           PERFORM GET-COLLATERAL.

           MOVE SPACES TO D-COLLATERAL-01.

           IF LTC-COLLATERAL-INFO = SPACES
              GO TO DISPLAY-IC.

           IF LTC-MAKE(1) NOT = SPACES OR
               LTC-MODEL(1) NOT = SPACES OR
                LTC-VIN(1) NOT = SPACES
                     STRING LTC-YEAR(1) "  "
                            LTC-MAKE(1) "  "
                            LTC-MODEL(1) "  "
                            LTC-VIN(1) "  "
                      DELIMITED BY "   " INTO D-COLLATERAL-01
                      GO TO DISPLAY-IC.

           MOVE LTC-COLL-INFO1(1) TO D-COLID01.
           MOVE LTC-COLL-INFO2(1) TO D-COLID02.
           MOVE LTC-COLL-INFO3(1) TO D-COLID03.
           MOVE LTC-COLL-INFO4(1) TO D-COLID04.

       DISPLAY-IC.
           PERFORM VARYING IX1 FROM 1 BY 1 UNTIL IX1 > 4
              MOVE LN-INSCOMP(IX1) TO D-1COMP(IX1)
              MOVE LN-INSPREM(IX1) TO D-1PREM(IX1)
              MOVE IX1 TO ITRM-SUB
              PERFORM INS-TERM-CALCULATION
              MOVE ITRM-INSTERM TO D-1TERM(IX1)
              MOVE LN-INSCOVR(IX1) TO D-1COVR(IX1)
              MOVE LN-INSOURS(IX1) TO D-1OUR(IX1)
           END-PERFORM.

           MOVE 1 TO IX1.
           MOVE 5 TO IX2.
       DISP-NEXT-2IC.
           MOVE LN-INSCOMP(IX2) TO D-2COMP(IX1).
           MOVE LN-INSPREM(IX2) TO D-2PREM(IX1).
           IF GP-OT-INS-TERM = "Y"
              MOVE IX2 TO ITRM-SUB
              PERFORM INS-TERM-CALCULATION
              MOVE ITRM-INSTERM TO D-2TERM(IX1)
           ELSE
              MOVE LN-INSEXP-DATE(IX2) TO DATE-YYYYMMDD
              PERFORM CONVERT-YYYYMMDD-TO-MMDDYY 
              MOVE DATE-MMDDYY TO D-2EXP(IX1).
           MOVE LN-INSCOVR(IX2) TO D-2COVR(IX1).
           MOVE LN-INSOURS(IX2) TO D-2OUR(IX1).
           MOVE IX2 TO ITRM-SUB.
           MOVE PD-LOANDATE TO ITRM-TODAY.
           PERFORM INS-TERM-CALCULATION.
           IF ITRM-INEFFECT-FG = "Y"
              MOVE "N" TO CHFG(IX2 + 7).

           IF IX2 < 6
              ADD 1 TO IX1 IX2
              GO TO DISP-NEXT-2IC.

           MOVE DISPLAY-ORIG-BUF TO WR-BUF.
           MOVE DISPLAY-ORIG-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      * MOVE REST OF CODES AND DATES NEEDED FROM ORIGINAL LOAN
      
           MOVE LN-CONFIDENTCD TO PD-CONFIDENTCD.
           MOVE LN-RECASTCD TO PD-RECASTCD.
           MOVE LN-STATUSFG TO PD-STATUSFG.
           MOVE LN-STATFLAGS TO PD-STATFLAGS.

           MOVE LN-REDUDATE TO PD-REDUDATE.
           MOVE LN-LEGALDATE TO PD-LEGALDATE.
           MOVE LN-BNKRPTDATE TO PD-BNKRPTDATE.
           MOVE LN-DEBTAIDATE TO PD-DEBTAIDATE.
           MOVE LN-REPODATE TO PD-REPODATE.
           MOVE LN-REPOCD TO PD-REPOCD.

           MOVE LN-PURDATE TO PD-PURDATE.
           MOVE LN-SUM-REGPYAMT TO PD-SUM-REGPYAMT.
           MOVE 0 TO LOAN-ERROR.

       END-PROCESS-ORIGINAL-LOAN.
           EXIT.

      *********************************************************
       TEST-LN1-FEEUNERN SECTION.
      *********************************************************

      * READ FEES TALLYING ALL FEES FLAGGED AS EARNING TYPE
      * AND DON'T ALLOW ADDONS
           PERFORM OPEN-CDB1-FILE.

           MOVE 0 TO FEE-UNEARN.

           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 10
              IF LN-FEECODE(SUB) NOT = 0
                 MOVE "GC"            TO CDB-TYPE
                 MOVE LN-OWNBR        TO CDB-BRANCH
                 MOVE LN-FEECODE(SUB) TO CDB-CODE
                 PERFORM READ-CDB1-FILE
                 IF (IO-GOOD) AND (CDB-GC-FEE-EARNINGS-FG = "Y")
                    ADD LN-FEEAMT(SUB) TO FEE-UNEARN
                 END-IF
              END-IF
           END-PERFORM.

       EXIT-TEST-LN1-FEEUNERN.
           PERFORM CLOSE-CDB1-FILE.

      *****************************************
       BWLN-SCAN SECTION.
      *****************************************
           MOVE "N"        TO SCAN-ERROR.
           MOVE TRANS-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-ALPDATE.
           MOVE ALP-DATE   TO BWLNSC-SYSDATE.
           MOVE 0          TO BWLNSC-ACCTNO.
           MOVE "WI/BWLNSC " TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH BWLNSC-BUF EXIT-PATHNAME.
           CANCEL FORM-PROGX.
           IF BWLNSC-STATUS NOT = "00" OR BWLNSC-ACCTNO = 0
               MOVE "Y" TO SCAN-ERROR
               GO TO EXIT-BWLN-SCAN.

           MOVE BWLNSC-ACCTNO TO LN-ACCTNO.
           MOVE BWLNSC-SSNO   TO BW-SSNO BORROWER-KEY.
           MOVE "N"           TO SCAN-ERROR.

       EXIT-BWLN-SCAN.
           EXIT.

      *****************************************
       VERIFY-PASSWORD SECTION.
      *****************************************
           MOVE " " TO PASSWDW-STAT.
           MOVE "LONPG0" TO PASSWDW-PGM.
           MOVE "PLEASE ENTER THE PASSWORD," TO PASSWDW-PROMPT1.
           MOVE "CL/PASSWD" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH PASSWDW-BUF.
           CANCEL FORM-PROGX.

      ******************************************************************
      *
      *     C O P Y - L I B R A R Y - S E C T I O N S
      *
      ******************************************************************
       COPY "LIBLP/LPMENU.CPY".
       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/GPENV.CPY".
       COPY "LIBLP/LPUPWK.CPY".
       COPY "LIBLP/LPPDCL.CPY".
       COPY "LIBLP/LPITRM.CPY".
       COPY "LIBLP/LPMDTE.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBLP/LPSTAT.CPY".

      **********************************************************
       FORM-RESET SECTION.
      **********************************************************
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DISPLAY LONPG-SCREEN UPON TCLP-MAIN-WINDOW-HANDLE.

           MOVE "S  A200CONAME." TO VDU.
           MOVE EXT-CONAME-CONAME TO VDUBUF.
           PERFORM SCREENIO.
           MOVE "S  8080SYSDATE." TO VDU.
           MOVE EXT-CONAME-SYSDATE TO VDU-DATE-YYYYMMDD.
           PERFORM SCREENIO.

           MOVE LONPG-FORM-FIELDS TO WR-BUF.
           MOVE LONPG-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.
       FORM-RESET-EXIT.
           EXIT.

      *********************************************************
       SETUP-LINK-INFO SECTION.
      *********************************************************
           IF VDU-FORM-NAME = "LPSMNU"
              MOVE LPSMNU-HELP--FILE TO HELP-LINK-FILE
              MOVE LPSMNU-HELP--DIR  TO HELP-LINK-DIR
           ELSE
              MOVE LONPG-HELP--FILE TO HELP-LINK-FILE
              MOVE LONPG-HELP--DIR TO HELP-LINK-DIR.

      *********************************************************
       VDU-CONVERT-FIELD SECTION.
      *********************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBLP/LONPG_EVA.CPY".
              COPY "LIBLP/LPSMNU_EVA.CPY".
              COPY "LIBLP/LPLNMU_EVA.CPY".
              COPY "LIBLP/LPCAMU_EVA.CPY".
              COPY "LIBLP/LPVDMU_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ******************************************************************
       MISC-PARAGRAPHS SECTION.
      ******************************************************************
       COPY "LIBGB/CHKSEC.CPY".
       COPY "LIBGB/SYSTEM.CPY".
       CALL-MEMO.
           MOVE LN-OWNBR  TO EXT-WI-CL-OWNBR.
           MOVE LN-ACCTNO TO EXT-WI-CL-ACCTNO.
      *    MOVE LN-APP-NUMBER TO EXT-WI-CL-APP-NUMBER.
           MOVE 1                TO EXT-WI-CL-SSNO-NO.
           MOVE LN-SSNO(1)       TO EXT-WI-CL-SSNO(1).
           MOVE "T"              TO MEMOSC-ONLY.
           MOVE "WI/MEMOSC" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH MEMOSC-BUF EXIT-PATHNAME.
           CANCEL FORM-PROGX.
       DISPLAY-ALL-LOANS.
           MOVE "Y"                               TO BWLNS2-SCAN-ONLY.
           MOVE EXT-WI-CL-SSNO(EXT-WI-CL-SSNO-NO) TO BWLNS2-KEY.
           MOVE TRANS-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-ALPDATE.
           MOVE ALP-DATE                          TO BWLNS2-SYSDATE.
           MOVE "WI/BWLNS2" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH BWLNS2-BUF EXIT-PATHNAME.
           CANCEL FORM-PROGX.
       SEND-PAYMENT-LINES.
           MOVE LN-OWNBR      TO EXT-WI-CL-OWNBR.
           MOVE LN-ACCTNO     TO EXT-WI-CL-ACCTNO.
      *    MOVE LN-APP-NUMBER TO EXT-WI-CL-APP-NUMBER.
           MOVE 1             TO EXT-WI-CL-SSNO-NO.
           MOVE LN-SSNO(1)    TO EXT-WI-CL-SSNO(1).
           MOVE "WI/LPLINE" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH EXIT-PATHNAME.
           CANCEL FORM-PROGX.

       SEND-PAYOFF.
           MOVE LN-OWNBR      TO EXT-WI-CL-OWNBR.
           MOVE LN-ACCTNO     TO EXT-WI-CL-ACCTNO.
      *    MOVE LN-APP-NUMBER TO EXT-WI-CL-APP-NUMBER.
           MOVE 1             TO EXT-WI-CL-SSNO-NO.
           MOVE LN-SSNO(1)    TO EXT-WI-CL-SSNO(1).
           MOVE "WI/WIPOFF" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH EXIT-PATHNAME.
           CANCEL FORM-PROGX.
      *-----------------------------------------------------------------
       GET-LC.
           PERFORM OPEN-LC1-FILE.
           PERFORM READ-LC1-FILE.
           PERFORM CLOSE-LC1-FILE.
           IF IO-BAD
              MOVE "INVALID" TO LC-DESC.

      *-----------------------------------------------------------------
       GET-CD.
           MOVE "N" TO CODE-ERROR.
           PERFORM OPEN-CD1-FILE.
           PERFORM READ-CD1-FILE.
           PERFORM CLOSE-CD1-FILE.
           IF IO-BAD
              MOVE "Y" TO CODE-ERROR
              MOVE " INVALID CODE" TO CD-DESC.

      *-----------------------------------------------------------------
       GET-DL1.
           PERFORM OPEN-DL1-FILE.
           PERFORM READ-DL1-FILE.
           PERFORM CLOSE-DL1-FILE.
           IF IO-BAD
              MOVE " INVALID DEALER" TO DL-SHORT.

      *-----------------------------------------------------------------
       GET-BW1.
           PERFORM READ-BW1-FILE.
           IF IO-BAD
              MOVE " " TO BW-FNAME
              MOVE "NOT FOUND" TO BW-LNAME
              MOVE "Y" TO BW-ERR.

           PERFORM APPEND-BW-NAMES.

      *-----------------------------------------------------------------
       GET-SP.
           MOVE PD-ORGST TO SP-ORGST.
           MOVE PD-SPRCLASS TO SP-SPRCLASS.
           MOVE PD-SUBCLASS TO SP-SUBCLASS.
           MOVE PD-LAWCODE TO SP-LAWCODE.
           PERFORM OPEN-SP1-FILE.
           PERFORM READ-SP1-FILE.

           IF IO-BAD
              MOVE "STATE PARA DELETED" TO MESS
              PERFORM SEND-MESS
              GO TO IO-ERROR.

           PERFORM CLOSE-SP1-FILE.

      *-----------------------------------------------------------------
       MOVE-MSG.
           MOVE PD-MSG1 TO MSG-LINE1.
           MOVE PD-MSG2 TO MSG-LINE2.

      *-----------------------------------------------------------------
       GET-COLLATERAL.
           MOVE LN-OWNBR  TO LTC-BRNO.
           MOVE LN-ACCTNO TO LTC-ACCTNO.
           MOVE 1         TO LTC-SEQNO.
           PERFORM OPEN-LTC1-FILE.
           PERFORM READ-LTC1-FILE.
           IF IO-BAD 
              MOVE SPACES TO LTC-COLLATERAL-INFO.
           PERFORM CLOSE-LTC1-FILE.
      *-----------------------------------------------------------------
       SEND-CD-DESC.
           PERFORM GET-CD.
           MOVE "S  A000  1." TO VDU.
           MOVE ELE TO VDUROW.
           MOVE CD-DESC TO VDUBUF.
           PERFORM SCREENIO.

      *-----------------------------------------------------------------
       APPEND-BW-NAMES.
           MOVE BW-LNAME TO INSPECT-W.
           MOVE BW-FNAME TO ADD-ON.
           PERFORM ADD-IT.

      *-----------------------------------------------------------------
       ADD-IT.
           INSPECT INSPECT-W REPLACING FIRST "  " BY " +".
           INSPECT INSPECT-W REPLACING FIRST
              "+                        " BY ADD-ON.

      *-----------------------------------------------------------------
       ADD-IT2.
           PERFORM ADD-IT.
           MOVE "/" TO ADD-ON.
           PERFORM ADD-IT.

      *-----------------------------------------------------------------
       NUMBW-TO-ALPBW.
           MOVE " " TO ALP-BW.
           IF BW-SSNO NOT = 0
              MOVE BW-SSNO TO NUM-BW
              MOVE NUM-BW123 TO ALP-BW123
              MOVE NUM-BW45 TO ALP-BW45
              MOVE NUM-BW6789 TO ALP-BW6789
              MOVE "-" TO ALP-HYPHEN1 ALP-HYPHEN2.

      *-----------------------------------------------------------------
       MSG-LINE.
           PERFORM CALL-TMMSG.

      *-----------------------------------------------------------------
       SEND-LEGEND.
           MOVE LEGEND TO WR-BUF.
           MOVE MSEL-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.


      ******************************************************
       SET-CY-CT-SCAN SECTION.
      ******************************************************

      * SET FLAGS TO ALLOW OR DISALLOW SCAN ON CITY & COUNTY
      * DEPENDING IF THE STATE IS IN CODE FILE (TYPE CY & CT)
           IF BR-FORCE-VALID-CITY-COUNTY NOT = "Y"
              MOVE "N" TO ALLOW-CITY-SCAN-FG
           ELSE
              MOVE " " TO CD-CY-CITY
              PERFORM DO-STATE-CY-EXIST
              IF DO-STATE-CY-EXIST-FG = "Y"
                 MOVE "Y" TO ALLOW-CITY-SCAN-FG
              ELSE
                 MOVE "N" TO ALLOW-CITY-SCAN-FG.

           MOVE "N" TO ALLOW-COUNTY-SCAN-FG.
           IF BR-FORCE-VALID-CITY-COUNTY = "Y"
              MOVE " " TO CD-CT-COUNTY
              PERFORM DO-STATE-CT-EXIST
              IF DO-STATE-CT-EXIST-FG = "Y"
                 MOVE "Y" TO ALLOW-COUNTY-SCAN-FG.
       SET-CY-CT-SCAN-EXIT.
           EXIT.

      ********************************************************
       VALIDATE-REQUIRED-FIELD SECTION.
      ***********************************************************

      * MUST VERIFY IF BR-FORCE-VALID-CITY-COUNTY = "Y", THAT THE
      * FIELD ENTRIES IN CITY & COUNTY EXIST IN THE CODE-FILE FOR
      * TYPES CT & CY

           MOVE "N" TO ALLOW-BW-UPDATE-FG.
           IF BR-FORCE-VALID-CITY-COUNTY NOT = "Y"
              MOVE "Y" TO ALLOW-BW-UPDATE-FG
              GO TO VALIDATE-REQUIRED-FIELD-EXIT.

           MOVE BW-CITY-TAX-CD  TO CD-CY-CITY.
           PERFORM DO-STATE-CY-EXIST
           IF DO-STATE-CY-EXIST-FG NOT = "Y" OR
              CD-CY-CITY NOT = BW-CITY-TAX-CD
              MOVE "N" TO ALLOW-BW-UPDATE-FG
              MOVE "CITY NOT ON FILE FOR STATE" TO MESS
              PERFORM SEND-MESS
              GO TO VALIDATE-REQUIRED-FIELD-EXIT.

       CHECK-FOR-VALID-COUNTY.
           IF ALLOW-COUNTY-SCAN-FG NOT = "Y"
              GO TO VALIDATE-REQUIRED-FIELD-EXIT.

           MOVE BW-COUNTY TO CD-CT-COUNTY.
           PERFORM DO-STATE-CT-EXIST.

           IF DO-STATE-CT-EXIST-FG NOT = "Y" OR
              CD-CT-COUNTY NOT = BW-COUNTY
              MOVE "N" TO ALLOW-BW-UPDATE-FG
              MOVE "COUNTY NOT ON FILE FOR STATE" TO MESS
              PERFORM SEND-MESS
              GO TO VALIDATE-REQUIRED-FIELD-EXIT.

       VALIDATE-REQUIRED-FIELD-END.
           MOVE "Y" TO ALLOW-BW-UPDATE-FG.
       VALIDATE-REQUIRED-FIELD-EXIT.
           EXIT.

      ******************************************************************
       DO-STATE-CY-EXIST SECTION.
      ******************************************************************

      * DETERMINE IF A VALID CD-CY-REC EXISTS FOR THE BORROWERS STATE

           MOVE " " TO DO-STATE-CY-EXIST-FG.

           PERFORM OPEN-CD1-FILE.
           MOVE "CY"       TO CD-TYPE.
           MOVE BW-STATE   TO CD-CY-STATE.
      *  SETUP CD-CY-CITY BEFORE PERFORM DO-STATE-CY-EXIST
           MOVE SPACES     TO CD-CY-FILLER.

           PERFORM START-CD1-FILE.
   
           PERFORM READ-CD1-FILE-NEXT.
           IF IO-BAD
              GO TO DO-STATE-CY-EXIST-EXIT.

           IF (CD-TYPE = "CY") AND (CD-CY-STATE = BW-STATE)
              MOVE "Y" TO DO-STATE-CY-EXIST-FG.
   
       DO-STATE-CY-EXIST-EXIT.
           PERFORM CLOSE-CD1-FILE.

      ******************************************************************
       DO-STATE-CT-EXIST SECTION.
      ******************************************************************

      * DETERMINE IF A VALID CD-CT-REC EXISTS FOR THE BORROWERS STATE

           MOVE " " TO DO-STATE-CT-EXIST-FG.

           PERFORM OPEN-CD1-FILE.
           MOVE "CT"       TO CD-TYPE.
           MOVE BW-STATE   TO CD-CT-STATE.
      * SETUP CD-CT-COUNTY BEFORE PERFORM DO-STATE-CT-EXIST
           MOVE SPACES     TO CD-CT-FILLER.
           PERFORM START-CD1-FILE.
   
           PERFORM READ-CD1-FILE-NEXT.
           IF IO-BAD
              GO TO DO-STATE-CT-EXIST-EXIT.

           IF (CD-TYPE = "CT") AND (CD-CT-STATE = BW-STATE)
              MOVE "Y" TO DO-STATE-CT-EXIST-FG.
   
       DO-STATE-CT-EXIST-EXIT.
           PERFORM CLOSE-CD1-FILE.

      ******************************************************************
       IO-ROUTINE SECTION.
      ******************************************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBLP/LPSPGS_SQL.CPY".
       COPY "LIBLP/LPLTPGS_SQL.CPY".
       COPY "LIBLP/LPLTCGS_SQL.CPY".
       COPY "LIBLP/LPLNGS_SQL.CPY".
       COPY "LIBLP/LPLCGS_SQL.CPY".
       COPY "LIBLP/LPDLGS_SQL.CPY".
       COPY "LIBLP/LPCDBGS_SQL.CPY".
       COPY "LIBLP/LPCDGS_SQL.CPY".
       COPY "LIBLP/LPBWGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBLP/LPDL1RN.CPY".
       COPY "LIBLP/LPLC1RN.CPY".
       COPY "LIBLP/LPSP1RN.CPY".
       COPY "LIBLP/LPBW1RN.CPY".
       COPY "LIBLP/LPCD1RN.CPY".
       COPY "LIBLP/LPCDB1RN.CPY".
       COPY "LIBLP/LPWKI.CPY".
       COPY "LIBLP/LPLTC1RN.CPY".
       COPY "LIBLP/LPLTP1RN.CPY".
       COPY "LIBLP/LPLN1RN.CPY".
       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO ERRCD.

      ******************************************************************
       END-ROUTINE SECTION.
      ******************************************************************
           PERFORM CLOSE-FILES.
       EXIT-PROG.
      * DO NOT PUT A DESTROY SCREEN HERE, IT CALLS OTHER PROGRAMS THAT
      * THE FORM DISPLAYED (LONPG3, LONPG4, ETC.)
       STOP-RUN.
           EXIT PROGRAM.
