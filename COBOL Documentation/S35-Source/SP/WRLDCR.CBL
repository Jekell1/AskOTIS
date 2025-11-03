       IDENTIFICATION     DIVISION.
       PROGRAM-ID.          WRLDCR.
       AUTHOR.                  CS.
       DATE-WRITTEN.    09/23/2010.
      **************************************************************************
      *
      * DESCRIPTION:  UPLOAD CREDIT SCORES INTO BORROWER & UPDATE
      *               THE LOAN CREDIT SCORE FOR NEW ACCTS.
      *                 WORLD PR# 707
      * WORLD USES 4/27/2023
      *
      *               INPUT FILE FOR WORLD  /USR/DATA/R#/SP/OCORE.YYMMDD
      *
      * WHEN WORLD STARTED SUBMITTING TO EQUIFAX (OCT 2010):
      *
      *    FIRST RAN A FIX TO ZERO OUT ALL THE SCORES (FX/ZSCORE). THIS
      *    MOVED 0 TO BW-CREDIT-SCORE & ALL 4 SCORES IN BOTH BW CREDIT
      *    SCORE TABLES.
      *
      *    THEN, THEY USED THE EXTRACT (SP/WRLDEQ) TO SEND A SEMI-ANNUAL
      *    TYPE EXTRACTION.  THIS WOULD HAVE SENT ANY BORROWER WITH AN
      *    OPEN LOAN, WITH 0 IN THE EQ-LOAN-NUMBER.  THIS FIRST FILE
      *    WAS /USR/DATA/R?/SP/SCORE.101031.
      *
      *    EQUIFAX SENDS BACK THE SAME FILE WITH THE SCORE FILLED IN;
      *    THE FIRST ONE WAS /USR/DATA/R?/SP/OCORE.101031.  THEY USE
      *    THIS PROGRAM (SP/WRLDCR) TO UPDATE THESE SCORES.  SINCE
      *    IT WAS A SEMI-ANNUAL TYPE FILE, ALL THE EQ-LOAN-NUMBER
      *    FIELDS WERE 0, SO THIS UPDATE WOULD PUT THE SCORE INTO
      *    BW-CREDIT-SCORE, AND INTO THE FIRST BW-CRED-SCORE SLOT,
      *    AFTER BUMPING DOWN THE EXISTING SCORES IN BW-CRED-SCORE
      *    TABLE.
      *
      *    AFTER RUNNING THIS FIRST UPDATE, WORLD RAN A FIX, FX/FXLNSC,
      *    WHICH MOVED THE BW-CREDIT-SCORE TO THE BW-BNKRPT-SCORE(1),
      *    AND MOVES BW-CREDIT-SCORE TO LN-CRSCORE FOR THEIR LOANS.
      *
      *    THEN FOR THE NEXT 5 MONTHS, THEY RUN THE MONTHLY EXTRACT,
      *    WHICH ONLY SENDS NEW LOANS (NB,FB,DM,ML,RF) WITH NO OTHER
      *    OPEN LOANS WITHIN THE DATE RANGE.  THESE SCORES FROM NEW
      *    LOANS COME BACK WITH A VALUE IN EQ-LOAN-NUMBER, SO THIS
      *    UPDATE MOVES THE SCORE TO BW-CREDIT-SCORE, BW-CRED-SCORE(1)
      *    AND BW-BNKRPT-SCORE(1).
      *
      *    SO, THE FIELD BW-BNKRPT-SCORE(1) SHOULD BE THE "ORIGINAL"
      *    SCORE, EITHER FROM THE FIRST SCORE THAT GOT MOVED IN BY
      *    THE FIX, OR FROM THE FIRST TIME A BORROWER GOT A SCORE
      *    FROM A NEW LOAN IN THE MONTHLY UPDATE.
      *
      *    SUBSEQUENT SEMI-ANNUAL UPDATES WILL UPDATE SCORES TO
      *    BW-CREDIT-SCORE & BW-CRED-SCORE(1) FOR ALL OPEN LOANS.
      *    BW-BNKRPT-SCORE(1) WILL NOT CHANGE, IT IS THE ORIGINAL SCORE.
      *
      *    CREDIT SCORE STATS ARE EXTRACTED USING BW-CREDIT-SCORE FOR
      *    THE CURRENT SCORE & BW-BNKRPT-SCORE(1) FOR THE ORIGINAL
      *    SCORE.
      *
      *
      * REV: 102510 CS INPUT FILE CHANGED FROM OSCORE.YYMMDD TO OCORE.YYMMDD
      *      101109 CS IF INVALID SSNO, PRINT THE BORROWER NAME FROM THE
      *                EQ-FILE.  WORLD PR#707
      *      101115 CS WHEN DETERMINING EXCEPTIONS, SKIP CHECK ON 2ND CHAR
      *                OF BW-REFNO   WORLD PR#722
      *      140604 CS REPLACED MKDIR-CMD WITH C$MAKEDIR
      * MJD 170911 ACTIVATE 8 DIGIT DATES, PD0006
      * BAH 20191004 REMOVED ACCESS-CALL BWFILE
      * BAH 20231207 VALIDATE IMPORT NUMERICS THAT ARE USED
      **************************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "LIBGB/GBFSPR.CPY".
           SELECT EQUIFAX-FILE ASSIGN EQUIFAX-PATH
                  ORGANIZATION LINE SEQUENTIAL
                  ACCESS SEQUENTIAL
                  LOCK MODE AUTOMATIC WITH LOCK ON RECORD
                  FILE STATUS FILE-STAT.

       I-O-CONTROL.
       DATA DIVISION.
       FILE SECTION.

       COPY "LIBGB/GBFDPR.CPY".

       FD  EQUIFAX-FILE
           LABEL RECORDS ARE STANDARD.

       01  EQUIFAX-REC.
           03  EQ-BW-LNAME                 PIC X(16).
           03  EQ-BW-FNAME                 PIC X(16).
           03  EQ-BW-ADR1                  PIC X(24).
           03  EQ-BW-CITY                  PIC X(15).
           03  EQ-BW-STATE                 PIC X(2).
           03  EQ-BW-ZIP                   PIC X(10).
           03  EQ-BW-SSNO                  PIC 9(9).
           03  EQ-BR-NO                    PIC 9(4).
           03  EQ-LN-ACCTNO                PIC 9(6).
           03  EQ-LN-CLASS                 PIC 9(2).
           03  EQ-LN-LOANDATE              PIC 999999.
           03  EQ-LN-SRCD                  PIC X(2).
           03  EQ-BEACON-SCORE             PIC 9(3).
           03  EQ-FACT-ACT-VARIANCE-CD     PIC X(3).
           03  EQ-CRITERIA-CODE            PIC X.

           03  EQ-CR                      PIC X.
           03  EQ-LF                      PIC X.



       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)SP/WRLDCR.CBL S35 12/08/23-07:36:41 >".
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBGB/GB01GR.CPY".
       COPY "LIBGB/GB01GR_SQL.CPY".
       COPY "LIBGB/GBWSGR.CPY".
       COPY "LIBLP/LP01BW.CPY".
       COPY "LIBLP/LP01BW_SQL.CPY".
       COPY "LIBLP/LPWSBW.CPY".
       COPY "LIBLP/LP01LN.CPY".
       COPY "LIBLP/LP01LN_SQL.CPY".
       COPY "LIBLP/LPWSLN.CPY".

       01  EQUIFAX-PATH.
           03  EQUIFAX-PATH-BASE          PIC X(09).
           03  FILLER                     PIC X(01) VALUE "/".
           03  EQUIFAX-PATH-MACHINE       PIC X(02).
           03  FILLER                     PIC X(10) VALUE "/SP/OCORE.".
           03  EQUIFAX-PATH-DATE          PIC 9(06).

      * NOT REFERENCED
      * 01  EQUIFAX-RUN-DATE     PIC 9(6).


       01  HOLD-BRNO            PIC 9(4) VALUE 0.

      *-----------------------------------------------------------------
      * LINE WORKERS
      *-----------------------------------------------------------------
       01  LN-WORKERS.
           03  LN-FEED          PIC 999      VALUE 0.
           03  LN-COUNT         PIC 999      VALUE 99.
           03  LN-WK            PIC 999      VALUE 0.
       01  PAGE-NUMBER          PIC 9(4)     VALUE 0.

       01  SUB                  PIC 9(2)     COMP-3.

       01  WORK-EQUIFAX-SCORE.
           03 WORK-SCORE        PIC 9(4)  OCCURS 4.

       01  TEST-ACT-VARIANCE-TBLE.
           03  TEST-CD.
               05  TEST-CODE    PIC X OCCURS 3.
           03  TEST-CRITERIA    PIC X.

       01  STEP-COUNT           PIC 9(4) VALUE 1.


       COPY "LIBGB/EOBUF_EXT.CPY".
           03  BEG-BR             PIC 9(04).
           03  END-BR             PIC 9(04).
           03  END-NEW-LOANDATE   PIC 9(8).
       01  VERSION-CONTROL        PIC X    VALUE "B".

       01  MESS-LIST            PIC X(05) VALUE "MESS.".

       01  SV-BEG-BR           PIC 9999  VALUE 9999.
       01  SV-END-BR           PIC 9999  VALUE 9999.

      *-----------------------------------------------------------------
      * PRINT BUFFERS
      *-----------------------------------------------------------------
       01  HEAD1.
           03  H-DATE           PIC X(08)     VALUE " ".
           03  FILLER           PIC XX       VALUE " ".
           03  H-HR             PIC 99       VALUE 0.
           03  FILLER           PIC X        VALUE ":".
           03  H-MIN            PIC 99       VALUE 0.
           03  FILLER           PIC X(08)    VALUE " ".
           03  H-NAME           PIC X(40)    VALUE " ".
           03  FILLER           PIC X(08)    VALUE " ".
           03  FILLER           PIC X(05)     VALUE "PAGE:".
           03  H-PAGE-NO        PIC ZZZ9.

       01  HEAD2.
           03  FILLER           PIC X(10)    VALUE "USER ID:".
           03  H-USERID         PIC X(12).
           03  FILLER           PIC X(13)    VALUE " ".
           03  FILLER           PIC X(40) VALUE
           "    CREDIT SCORE BORROWER UPLOAD   ".
       01  HEAD3.
           03  FILLER           PIC X(06)    VALUE "BRNO".
           03  FILLER           PIC X(05)    VALUE SPACES.
           03  FILLER           PIC X(06)    VALUE "SSNO-1".
           03  FILLER           PIC X(02)    VALUE SPACES.
           03  FILLER           PIC X(32)    VALUE "NAME".
           03  FILLER           PIC X(01)    VALUE SPACES.
           03  FILLER           PIC X(7)     VALUE "FACT CD".
           03  FILLER           PIC X        VALUE " ".
           03  FILLER           PIC X(8)     VALUE "CRITERIA".

       01  DETAIL-LINE              PIC X(80) VALUE SPACES.
       01  DETAIL-LINE1 REDEFINES DETAIL-LINE.
           03  D-BRNO               PIC Z(4).
           03  FILLER               PIC X(2).
           03  D-SSNO               PIC 999/99/9999.
           03  FILLER               PIC X(2).
           03  D-BORROWER           PIC X(32).
           03  FILLER               PIC X(2).
           03  D-FACT-ACT-CODE      PIC X(4).
           03  FILLER               PIC X(7).
           03  D-MSG1               PIC X(12).

       01  RANGE-LINE REDEFINES DETAIL-LINE.
           03  RANGE-SELECTION  PIC X(30).
           03  FILLER           PIC X(05).
           03  RANGE1N          PIC Z(09)9.
           03  FILLER           PIC X(20).

      *    "/$REL/SH/EC /$REL/SH/WRLDCR.SH".
       01  WRLDCR-CMD.
           03  FILLER              PIC X    VALUE "/".
           03  WRLDCR-REL          PIC X(7).
           03  FILLER              PIC X(4) VALUE "/SH/".
           03  WRLDCR-EC           PIC X(3) VALUE "EC ".
           03  FILLER              PIC X    VALUE "/".
           03  WRLDCR-REL2         PIC X(7).
           03  FILLER              PIC X(4) VALUE "/SH/".
           03  WRLDCR-SHELL        PIC X(10) VALUE "WRLDCR.SH ".
           03  FILLER              PIC X     VALUE " ".
           03  WRLDCR-DATA         PIC X(11).

       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/SYSTEMW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBGB/GETPRW.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/DACCESSW.CPY".
       COPY "LIBGB/EOSPOOLW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBSP/WRLDCR_DEF.CPY".
       COPY "LIBSP/WRLDCR_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

      *****************************************************************
      *
      *     P R O G R A M   C O N T R O L
      *
      *****************************************************************
       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBSP/WRLDCR_SCN.CPY".

      *----------------------------------------------------------------
       PROCEDURE DIVISION
           USING FORM-PATHNAME EXIT-PATHNAME.

       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
               PR-FILE EQUIFAX-FILE.
       COPY "LIBGB/DECLRP.CPY".
           PERFORM CLOSE-LN1-FILE.
           PERFORM CLOSE-BW1-FILE.
           PERFORM CLOSE-BR-FILE.
           PERFORM CLOSE-PR-FILE.
           CLOSE
               EQUIFAX-FILE.
       COPY "LIBGB/DECLRP2.CPY".

      *************************************************
       MAIN-MODULE SECTION.
      *************************************************
           PERFORM SQL-CONNECT.

      D    STOP MESS.
           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "WRLDCR" TO VDU-FORM-NAME.

           PERFORM INITIALIZATION.
           IF EXT-EOBUF-ERRCD NOT = " "
              GO TO END-PROGRAM.

           IF EXT-ROUTE-BUF = "00"
              MOVE "PROCESSING CREDIT SCORE UPLOAD" TO MESS
              PERFORM SEND-LEGEND.

           MOVE 0 TO HOLD-BRNO.

           PERFORM LOAD-EQUIFAX-FILE.
           PERFORM OPEN-EQUIFAX-FILE.

       NEXT-EQUIFAX.
           PERFORM READ-EQUIFAX-FILE.
           IF IO-FG NOT = 0
              PERFORM CLOSE-BW1-FILE
              PERFORM CLOSE-LN1-FILE
              PERFORM PRINT-RANGE-LINES
              GO TO END-PROGRAM.

           IF EQ-BR-NO NOT NUMERIC
              MOVE "BR-NO NOT NUMERIC" TO D-MSG1
              PERFORM WRITE-DETAIL-LINE
              GO TO NEXT-EQUIFAX.

           IF EQ-BR-NO < BEG-BR OR EQ-BR-NO > END-BR
              GO TO NEXT-EQUIFAX.

           IF EQ-BW-SSNO NOT NUMERIC
              MOVE "SSNO NOT NUMERIC" TO D-MSG1
              PERFORM WRITE-DETAIL-LINE
              GO TO NEXT-EQUIFAX.
           IF EQ-LN-ACCTNO NOT NUMERIC
              MOVE "ACCTNO NOT NUMERIC" TO D-MSG1
              PERFORM WRITE-DETAIL-LINE
              GO TO NEXT-EQUIFAX.
           IF EQ-BEACON-SCORE NOT NUMERIC
              MOVE "BEACON SCORE NOT NUMERIC" TO D-MSG1
              PERFORM WRITE-DETAIL-LINE
              GO TO NEXT-EQUIFAX.

           IF EQ-BR-NO NOT = HOLD-BRNO
              IF HOLD-BRNO NOT = 0
                 PERFORM CLOSE-BW1-FILE
                 PERFORM CLOSE-LN1-FILE
              END-IF
              MOVE EQ-BR-NO TO BR-NO
              PERFORM GET-BR-SET-PATHS
              IF IO-FG NOT = 0
                 MOVE 0 TO HOLD-BRNO
                 GO TO NEXT-EQUIFAX.

           MOVE SPACES TO TEST-ACT-VARIANCE-TBLE.

           MOVE EQ-BW-SSNO TO BW-SSNO.
           PERFORM READ-BW1-FILE.
           IF IO-FG NOT = 0
              MOVE BR-NO TO D-BRNO
              MOVE EQ-BW-SSNO TO D-SSNO
              STRING EQ-BW-LNAME, " ", EQ-BW-FNAME DELIMITED BY "   "
                                    INTO D-BORROWER
              MOVE "INVALID SSNO" TO D-MSG1
              PERFORM WRITE-DETAIL-LINE
              GO TO NEXT-EQUIFAX.

           MOVE EQ-BEACON-SCORE TO BW-CREDIT-SCORE.

           MOVE EQ-FACT-ACT-VARIANCE-CD TO TEST-CD.
           MOVE EQ-CRITERIA-CODE TO TEST-CRITERIA.
           MOVE TEST-ACT-VARIANCE-TBLE TO BW-REPCODE.

           INSPECT BW-REPCODE REPLACING ALL " " BY "O".

           IF (TEST-CODE(1) NOT = SPACES)  OR
              (TEST-CODE(3) NOT  = "N" AND NOT = " ")OR
              (TEST-CRITERIA = SPACES)
              PERFORM WRITE-FACT-ACT-EXCEPT.


      *IF EQ-LN-ACCTNO IS 0, ITS NOT A 'NB','FB','DM','ML' OR 'RF' SOURCE
      *CODE. EQ-LN-ACCTNO IS ALSO 0 IF LOAN IS ONE OF THESE SOURCE CODES BUT
      *ANOTHER OPEN NON-PL ACCT EXISTS FOR THE CUSTOMER.

      *IF BW-CRED-SCORE(1) IS NOT ZERO, MOVE EACH SCORE DOWN TO THE
      *NEXT SCORE LEVEL (QUARTER)
           IF EQ-LN-ACCTNO = 0
              PERFORM SET-BW-EXISTING-CREDIT-SCORE
           ELSE
              MOVE EQ-BEACON-SCORE TO BW-CRED-SCORE(1)
              MOVE EQ-BEACON-SCORE TO BW-BNKRPT-SCORE(1).

           PERFORM REWRITE-BW1-FILE.

           IF EQ-LN-ACCTNO NOT = 0
              MOVE EQ-BR-NO     TO LN-OWNBR
              MOVE EQ-LN-ACCTNO TO LN-ACCTNO
              PERFORM READ-LN1-FILE
              IF IO-FG = 0
                 MOVE EQ-BEACON-SCORE TO LN-CRSCORE
                 PERFORM REWRITE-LN1-FILE
              ELSE
                 GO TO SKIP-LN-UPDATE.


       SKIP-LN-UPDATE.
           GO TO NEXT-EQUIFAX.


      ***********************************
       INITIALIZATION SECTION.
      ***********************************
           MOVE WRLDCR-QUEUE-POS TO Q-POS.
           MOVE WRLDCR-COPIES-POS TO C-POS.
           MOVE WRLDCR-PRU-POS TO P-POS.
           MOVE SPACES TO Q-BUF C-BUF P-BUF.
       COPY "LIBGB/EOINIT.CPY".

           PERFORM OPEN-BR-FILE.

           IF BEG-BR = 0
              IF END-BR = 0
                 MOVE BEG-BR TO SV-BEG-BR
                 MOVE END-BR TO SV-END-BR
                 MOVE EXT-CONAME-USER-LOC TO BEG-BR
                                             END-BR.
           MOVE EXT-CONAME-CONAME TO H-NAME.
           MOVE EXT-CONAME-SYSDATE TO H-DATE.
           MOVE EXT-CONAME-USER-ID TO H-USERID.

           MOVE "REL" TO GETENV-BUF.
           PERFORM GETENV-CALL.
           MOVE GETENV-BUF TO WRLDCR-REL WRLDCR-REL2.
           CALL "C$TOLOWER" USING WRLDCR-EC, VALUE 3.
           CALL "C$TOLOWER" USING WRLDCR-SHELL, VALUE 10.
           MOVE EXT-FILPATH-FIL TO WRLDCR-DATA.
           MOVE WRLDCR-CMD TO SYSTEM-BUF.
           PERFORM SYSTEM-CALL.

           IF EXT-ROUTE-BUF = "00"
              PERFORM CREATE-SPOOL-DIR
              IF STAT NOT = "00"
                 MOVE "CANNOT CREATE SPOOL DIRECTORY" TO MESS
                 PERFORM SEND-MESS
                 GO TO INITIALIZE-ENDIT
              END-IF
              MOVE EO-SPOOL TO PRU-FILE.

           PERFORM OPEN-PR-FILE.

           GO TO INITIALIZE-EXIT.

       INITIALIZE-ENDIT.
           MOVE "X" TO EXT-EOBUF-ERRCD.
       INITIALIZE-EXIT.
           EXIT.

      *******************************************************
       EO-EXEC-AUDIT SECTION.
      *******************************************************
           MOVE EXT-EOBUF-DATE2 TO END-NEW-LOANDATE DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-YYMMDD.
           MOVE DATE-YYMMDD         TO EQUIFAX-PATH-DATE.

      *******************************************************
       ENTER-PARAMETERS SECTION.
      *******************************************************
       ENTER-PARAM.
      * FOR RE-ENTRY ON F4 FROM ACCEPT.
              IF VDUSTATUS = "1U"
                  IF EXT-ROUTE-BUF = "00"
                     GO TO END-NEW-LN-DATE
                  ELSE
                     GO TO BR-02.

       BR-01.
           MOVE "ENNN040BR1." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "16" GO TO ALL-BR.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO BR-01.
           MOVE VDUNUM0 TO BEG-BR.
       BR-02.
           MOVE "ENNN040BR2." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO BR-01.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO BR-02.
           MOVE VDUNUM0 TO END-BR.
           IF BEG-BR > END-BR
              MOVE "INVALID BRANCH RANGE" TO MESS
              PERFORM SEND-MESS
              GO TO BR-01.
           GO TO END-NEW-LN-DATE.

       ALL-BR.
           MOVE "SNNN040BR1." TO VDU.
           MOVE 0 TO BEG-BR VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN040BR2." TO VDU.
           MOVE 9999 TO END-BR VDUNUM0.
           PERFORM SCREENIO.
      *----------------------------------------------------------------
       END-NEW-LN-DATE.
           IF EXT-ROUTE-BUF NOT = "00"
              GO TO EXIT-PARM.

           MOVE "ERNM060DATE2." TO VDU.
           PERFORM SCREENIO.
           IF F7-KEY GO TO EXIT-PARM.
           IF F1-KEY GO TO EXIT-PARM.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO BR-02.

      *----MOVE VDUNUM0 TO END-NEW-LOANDATE DATE-MMDDYY.
           MOVE VDU-DATE-YYYYMMDD TO END-NEW-LOANDATE DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-YYMMDD.
           MOVE DATE-YYMMDD TO EQUIFAX-PATH-DATE.

           PERFORM LOAD-EQUIFAX-FILE.
           MOVE EQUIFAX-PATH TO ACCESS-BUF.
           PERFORM ACCESS-CALL.
           IF STAT NOT = "00"
              MOVE "CREDIT SCORE FILE NOT AVAILABLE" TO MESS
              PERFORM SEND-MESS
              MOVE "1>" TO VDUSTATUS
              GO TO EXIT-PARM.

       EXIT-PARM.
           EXIT.


      *****************************************************************
       CREATE-SPOOL-DIR SECTION.
      *****************************************************************

           MOVE EXT-FILPATH-BASE    TO EO-SPOOL-BASE.
           MOVE EXT-FILPATH-MACHINE TO EO-SPOOL-MACHINE.
           MOVE EXT-FILPATH-DATA    TO EO-SPOOL-DATA.
           MOVE "WAC"               TO EO-SPOOL-PCODE.
           MOVE END-NEW-LOANDATE    TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO EO-SPOOL-DATE.
           MOVE " " TO EO-SPOOL-FILE.
           MOVE EO-SPOOL TO DIR-ACCESS-BUF.
           PERFORM DIR-ACCESS-CALL.
           IF STAT NOT = "00"
              CALL "C$MAKEDIR" USING EO-SPOOL GIVING STAT-99
              IF STAT NOT = "00"
                 GO TO EXIT-CREATE-SPOOL-DIR.

      *****************************************************************
      * THEY ARE ALLOWED TO RUN THIS UPDATE MORE THAN ONCE ON THE
      * SAME TRANSACTION DATE, SO CHECK TO SEE IF REPORT EXISTS, IF
      * SO, INCREMENT THE STEP # SO WE DON'T WIPE OUT PRIOR REPORTS
      *****************************************************************
           MOVE "/"        TO EO-SPOOL-SLASH.
           MOVE "WRLDCR"   TO EO-SPOOL-NAME.
           MOVE 1 TO STEP-COUNT.
       TRY-NEXT-STEP-NO.
           MOVE STEP-COUNT TO EO-SPOOL-STEP.
           MOVE EO-SPOOL TO ACCESS-BUF.
           PERFORM ACCESS-CALL.
           IF STAT NOT = "00"
              MOVE "00" TO STAT
              GO TO EXIT-CREATE-SPOOL-DIR.
           IF STEP-COUNT < 999
              ADD 1 TO STEP-COUNT
              GO TO TRY-NEXT-STEP-NO.
       EXIT-CREATE-SPOOL-DIR.
           EXIT.

      *************************************************
       DISPLAY-PARAMETERS SECTION.
      *************************************************
           MOVE "SNNN040BR1." TO VDU.
           MOVE BEG-BR TO VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN040BR2." TO VDU.
           MOVE END-BR TO VDUNUM0.
           PERFORM SCREENIO.


      *******************************
       PRINT-RANGE-LINES SECTION.
      *******************************
           MOVE 2 TO LN-FEED.
           MOVE "BEGINNING BRANCH     "          TO RANGE-SELECTION.
           MOVE BEG-BR                           TO RANGE1N.
           PERFORM WRITE-DETAIL-LINE.
           MOVE "ENDING    BRANCH     "          TO RANGE-SELECTION.
           MOVE END-BR                           TO RANGE1N.
           PERFORM WRITE-DETAIL-LINE.
           MOVE "END LOAN DATE"                  TO RANGE-SELECTION.
           MOVE END-NEW-LOANDATE                 TO RANGE1N.
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      *********************************************************
       WRITE-FACT-ACT-EXCEPT SECTION.
      **********************************************************

      * WRITE BORROWER WITH ANY OF THE FACT ACT CODES SET TO 'Y'

           MOVE BW-OWNBR          TO D-BRNO.
           MOVE BW-SSNO           TO D-SSNO.
           INSPECT D-SSNO REPLACING ALL "/" BY "-".
           STRING BW-LNAME, " ", BW-FNAME DELIMITED BY "   " INTO
                                                 D-BORROWER.
           MOVE BW-REPCODE        TO D-FACT-ACT-CODE.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.


      ****************************
       WRITE-DETAIL-LINE SECTION.
      ****************************
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
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-PR-REC.

       END-HEAD-PAGE.
           MOVE 4 TO LN-COUNT.
           MOVE 2 TO LN-FEED.

       COPY "LIBGB/TIMEIO.CPY".
       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/SCREEN.CPY".


      *********************************************************
       FORM-RESET SECTION.
      *********************************************************
           MOVE EXT-CONAME-CONAME TO WRLDCR-CONAME.
           MOVE EXT-CONAME-SYSDATE TO WRLDCR-SYSDATE.
           DISPLAY WRLDCR-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE WRLDCR-FORM-FIELDS TO WR-BUF.
           MOVE WRLDCR-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      *********************************************************
       SETUP-LINK-INFO SECTION.
      *********************************************************
           MOVE WRLDCR-HELP--FILE TO HELP-LINK-FILE.
           MOVE WRLDCR-HELP--DIR  TO HELP-LINK-DIR.

      *********************************************************
       VDU-CONVERT-FIELD SECTION.
      *********************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBSP/WRLDCR_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      **************************************
       MISC-PARA SECTION.
      **************************************
       COPY "LIBGB/GETPR.CPY".
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/DACCESS.CPY".
       COPY "LIBGB/GETENV.CPY".
       COPY "LIBGB/SYSTEM.CPY".
       INVALID-RANGE.
           MOVE "INVALID RANGE" TO MESS.
           PERFORM SEND-MESS.
       SEND-LEGEND.
           MOVE MESS TO WR-BUF.
           MOVE MESS-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.
       GET-BR-SET-PATHS.
           PERFORM READ-BR-FILE.
           IF IO-FG = 0
              MOVE EXT-FILPATH-BASE TO FILPATH-BASE
              MOVE BR-MACHINE       TO FILPATH-MACHINE
              MOVE BR-DATA-PATH     TO FILPATH-DATA
              MOVE "B"        TO BW-PATH-OVERRIDE
                                 LN-PATH-OVERRIDE
              PERFORM OPEN-BW1-FILE
              MOVE BR-NO TO HOLD-BRNO
              PERFORM OPEN-LN1-FILE.

       SET-BW-EXISTING-CREDIT-SCORE.
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 4
              MOVE BW-CRED-SCORE(SUB) TO WORK-SCORE(SUB)
           END-PERFORM.

           PERFORM VARYING SUB FROM 4 BY -1 UNTIL SUB < 2
              MOVE WORK-SCORE(SUB - 1) TO WORK-SCORE(SUB)
           END-PERFORM.
           MOVE EQ-BEACON-SCORE  TO WORK-SCORE(1)
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 4
              MOVE WORK-SCORE(SUB) TO BW-CRED-SCORE(SUB)
           END-PERFORM.


      **************************************
       IO-ROUTINES SECTION.
      **************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBLP/LPLNGS_SQL.CPY".
       COPY "LIBLP/LPBWGS_SQL.CPY".
       COPY "LIBGB/GBGRGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBPRINT.CPY".

       LOAD-EQUIFAX-FILE.
           MOVE EXT-FILPATH-BASE    TO EQUIFAX-PATH-BASE.
           MOVE EXT-FILPATH-MACHINE TO EQUIFAX-PATH-MACHINE.
       OPEN-EQUIFAX-FILE.
           PERFORM OPEN-IT.
           MOVE EQUIFAX-PATH TO E-FILE.
           OPEN INPUT EQUIFAX-FILE.
           IF IO-FG = 8
              GO TO OPEN-EQUIFAX-FILE.
           IF IO-FG = 7
              CLOSE EQUIFAX-FILE
              GO TO OPEN-EQUIFAX-FILE.
       READ-EQUIFAX-FILE.
           PERFORM READ-IT.
           MOVE EQUIFAX-PATH TO E-FILE.
           READ EQUIFAX-FILE.
           IF IO-FG = 8
              GO TO READ-EQUIFAX-FILE.
       CLOSE-EQUIFAX-FILE.
           PERFORM CLOSE-IT.
           CLOSE EQUIFAX-FILE.

       COPY "LIBGB/EOSPOOLIO.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBLP/LPBW1IN.CPY".
       COPY "LIBLP/LPLN1IN.CPY".

       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO EXT-EOBUF-ERRCD.

      **************************************
       END-PROGRAM SECTION.
      **************************************
       END-RUN.
           PERFORM CLOSE-FILES.
           IF EXT-ROUTE-BUF = "00"
              PERFORM PRU-OUT.

           MOVE MV-CMD TO MV-BUF.
           MOVE EQUIFAX-PATH TO MV-FILE1 MV-FILE2.
           INSPECT MV-FILE2 REPLACING FIRST "    " BY ".UPD".
           PERFORM SYSTEM-CALL.
           IF STAT-BAD
              MOVE "UNABLE TO RENAME FILE" TO MESS
              PERFORM SEND-MESS.

       EXIT-PROG.
           IF SV-BEG-BR NOT = 9999
              MOVE SV-BEG-BR TO BEG-BR
              MOVE SV-END-BR TO END-BR.

           IF EXT-ROUTE-BUF = "00"
              MOVE EXIT-PATH TO FORM-NAM
              MOVE FORM-PATH TO FORM-PATHNAME.
       STOP-RUN.
           IF EXT-ROUTE-BUF = "00" OR "40" OR "50"
              DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN
              DESTROY WRLDCR-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.
           EXIT PROGRAM.
