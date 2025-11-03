       IDENTIFICATION DIVISION.
       PROGRAM-ID. WDAYGL.
      ********************************************************************
      *                   (GL)
      *       WORKDAY INTEGRATION GT EXTRACTION
      *              /USR/DATA/R1/FTP/WORKDAY/WDAYGL_YYMMDD.CSV
      *  S35 WILL NOT BE IN THE SAME OUTPUT ORDER DUE TO THE SORTED BY IN S35
      *  A15 IS BY DATE1 AND THEN WHATEVER BRANCH UPDATED FIRST, SECOND, ETC.
      * 
      *
      * REV: 
      * 181129 BLM NEW, WORLD PR# 1353
      * 190124 BLM DON'T SKIP ACCT 1022000 GT'S, EXCLUDE "EOD-DEPOSIT"
      *            GT'S, CHANGE COMPANYREFERENCEID TO WAC, SEND ALL 7 DIGITS
      *            OF ACCTNO, PUT GT-REF IN MEMO,
      *            ADD LINECOMPANYREFERENCEIDTYPE,
      *            ADD LINECOMPANYREFERENCEID, INCREASE OUTPUT REC TO 500 BYTES,
      *            WORLD PR# 1353A
      * 190205 BLM SUPPRESS LEADING ZEROS ON COST CENTER, CHANGE
      *            "LEDGERACCOUNTREFERENCEID" IN HEADER TO 
      *            "EXTERNALCODE LEDGERACCOUNTREFERENCEID", WORLD PR# 1353A
      * 190214 BLM CHANGE OUTPUT DATE TO LAST DATE OF MONTH.  DON'T ALLOW
      *            DATE RANGE TO SPAN MONTHS.  WORLD PR# 1353B
      * 231113 RMZ PERFORMANCE TUNING - ADDED STORED PROCEDURE
      * 240116 RMZ CHANGED JULIAN-BEG(END,CC) TO EXT-JULIAN-BEG(END,CC)
      * 240806 RMZ ADDED LTOUCH-DATE TO UPDATE SCRIPTS. (S35Q-108)
      * 2025.0102 BAH USING VIRTUAL BRANCHES AND THOSE HAVE "SC" IN THE
      *               BR-STATE, NEED REAL STATE HERE FOR ACCOUNTING SO THE
      *               COLLECTION CENTER STATE IS REAL BRANCH #1696 S35Q-155
      * 250304 RMZ CORRECTED 'SKIPPED COUNT' PROCESSING. BRANCH/GROUP  
      *            SKIPPED COUNTS WERE NOT BEING COUNTED CORRECTLY.
      *            (S35Q-167)
      *
      ********************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "LIBGB/GBFSPR.CPY".
           SELECT WK-FILE ASSIGN OUTPUT WK-PATH
                          ORGANIZATION LINE SEQUENTIAL
                          LOCK MODE AUTOMATIC WITH LOCK ON RECORD
                          FILE STATUS FILE-STAT.
 
       DATA DIVISION.
       FILE SECTION.
       COPY "LIBGB/GBFDPR.CPY".

      *****************************************************************
      *    WORKDAY GL TRANSACTION EXTRACTION FILE.
      *****************************************************************
       FD  WK-FILE
           RECORD IS VARYING IN SIZE FROM 10 TO 500 CHARACTERS
           LABEL RECORDS ARE STANDARD.
       01  WK-REC.
           03  WK-BYTE             PIC X OCCURS 500 TIMES.

       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(37) VALUE
           "@(#)GL/WDAYGL S35 03/04/25-06:13:23 >".

       COPY "LIBGL/GL01GT.CPY".
       COPY "LIBGL/GL01GT_SQL.CPY".
       COPY "LIBGL/GLWSGT.CPY".
       COPY "LIBGL/GL01GL.CPY".
       COPY "LIBGL/GL01GL_SQL.CPY".
       COPY "LIBGL/GLWSGL.CPY".
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBGB/GB01GR.CPY".
       COPY "LIBGB/GB01GR_SQL.CPY".
       COPY "LIBGB/GBWSGR.CPY".

       EXEC SQL BEGIN DECLARE SECTION END-EXEC.            
       01  QWS-WBEG-DATE        PIC X(10). 
       01  QWS-WEND-DATE        PIC X(10).  
       01  QWS-WBEG-SOURCE-CODE PIC 9.
       01  QWS-WEND-SOURCE-CODE PIC 9.
       01  QWS-PREV-EXTRACTED   PIC X.
       01  QWS-TOTAL-SKIPPED    PIC 9(10)  VALUE 0.
       EXEC SQL END DECLARE SECTION END-EXEC.

       01  GT2-CONN-OPEN-SW      PIC X        VALUE "N".
       01  GT2-TOTAL-OPEN-SW     PIC X        VALUE "N".

       01  WK-PATH.
           03  WK-PATH-DIR.
               05  WK-PATH-FIL PIC X(13).
               05  WK-PATH-WD  PIC X(12) VALUE "FTP/WORKDAY/".
           03  WK-PATH-FILE.
               05  FILLER      PIC X(7) VALUE "WDAYGL_".
               05  WK-PATH-YMD PIC 9(6).
               05  FILLER      PIC X(4) VALUE ".CSV".

       
       01  PR-IFN                      PIC X(3)      VALUE "PR".
       01  WK-IFN                      PIC X(3)      VALUE "WK".

       01  LN-WORKERS.
           03  LN-FEED        PIC 999   VALUE 0.
           03  LN-COUNT       PIC 999   VALUE 99.
           03  LN-WK          PIC 999   VALUE 0.
       01  PAGE-NUMBER        PIC 999   VALUE 0.
 
       01  OUTPUT-FILE-WORKERS.
           03  OUTPUT-LINE-ORDER    PIC 9(8) VALUE 0.
           03  OUTPUT-STATE.
               05  FILLER           PIC X(3) VALUE "WAC".
               05  OUTPUT-DASH      PIC X.
               05  GTSTATE          PIC XX.
           03  OUTPUT-GTDATE.
               05  FILLER           PIC X VALUE """".
               05  GTDATE-YYYYMMDD  PIC 9999/99/99.
               05  FILLER           PIC X VALUE """".
           03  OUTPUT-ACCTNO        PIC 9(7).
           03  OUTPUT-DEBIT         PIC 9(9).99 BLANK ZERO.
           03  OUTPUT-CREDIT        PIC 9(9).99 BLANK ZERO.
           03  OUTPUT-BRANCH        PIC ZZZ9.
           03  OUTPUT-EXPL          PIC X(32).

       01  TOTAL-DEBITS       PIC S9(10)V99 COMP-3 VALUE +0.
       01  TOTAL-CREDITS      PIC S9(10)V99 COMP-3 VALUE +0.
       01  TOTAL-DRCR         PIC S9(10)V99 COMP-3 VALUE +0.
       01  TOT-EXTRACTED      PIC S9(10)    COMP-3 VALUE +0.
       01  TOT-SKIPPED        PIC S9(10)    COMP-3 VALUE +0.
       01  TOT-SKIPPED-BGR    PIC S9(10)    COMP-3 VALUE +0.
       01  TOT-ERRORS         PIC S9(10)    COMP-3 VALUE +0.
       01  EXCEPTION-FG       PIC X.

       01  FIRST-TIME         PIC X VALUE "Y".
       01  RECORDS-FOUND      PIC X VALUE " ".
           88  NO-RECORDS-FOUND     VALUE "N".

       01  MESS-LIST          PIC X(5) VALUE "MESS.".

       COPY "LIBGB/EOBUF_EXT.CPY".
           03  SOURCE-CODE            PIC 9.
           03  ALL-SRC-CODES  REDEFINES SOURCE-CODE PIC X.
           03  BEG-DATE               PIC 9(8).
           03  FILLER REDEFINES BEG-DATE.
               05  BEG-DATE-YYYYMM    PIC 9(6).
               03  FILLER             PIC 99.
           03  END-DATE               PIC 9(8).
           03  FILLER REDEFINES END-DATE.
               05  END-DATE-YYYYMM    PIC 9(6).
               03  FILLER             PIC 99.
           03  BEG-BR                 PIC 9(4).
           03  END-BR                 PIC 9(4).
           03  GROUP-FLAG             PIC X.
           03  ENT-GROUP              PIC X(4).
           03  PREV-EXTRACTED         PIC X.

       01  VERSION-CONTROL    PIC X VALUE "A".

       01  SV-BEG-BR          PIC 9(4)   VALUE 9999.
       01  SV-END-BR          PIC 9(4)   VALUE 9999.
       01  SV-GROUP-FLAG      PIC X      VALUE SPACES.
       01  SV-ENT-GROUP       PIC X(4)   VALUE SPACES.

       01  HLINE-1.
           03  HD-DATE        PIC X(8).
           03  FILLER         PIC X(3)    VALUE SPACES.
           03  HD-TIME        PIC X(8)    VALUE SPACES.
           03  FILLER         PIC X(30)   VALUE SPACES.
           03  HD-CONAME      PIC X(40).
           03  FILLER         PIC X(34)   VALUE SPACES.
           03  FILLER         PIC X(5)    VALUE "PAGE ".
           03  HD-PAGE        PIC ZZ9.

       01  HLINE-2.
           03  FILLER         PIC X(9)    VALUE "USERID:".
           03  HD-USERID      PIC X(10)   VALUE SPACES.
           03  FILLER         PIC X(26)   VALUE SPACES.
           03  H-TITLE        PIC X(42)   VALUE
               "WORKDAY INTEGRATION TRANSACTION EXTRACTION".

       01  HLINE-3.
           03  FILLER         PIC X(11)   VALUE "REFERENCE  ".
           03  FILLER         PIC X(5)    VALUE "     ".
           03  FILLER         PIC X(12)   VALUE "GL ACCT # ".
           03  FILLER         PIC X(30)   VALUE
               "DESC          SRC UPD/PER ".
           03  FILLER         PIC X(13) VALUE "    DEBIT  ".
           03  FILLER         PIC X(14) VALUE "      CREDIT ".
           03  FILLER         PIC X(34)   VALUE
               "EXPLANATION".
           03  FILLER         PIC X(5)    VALUE " DATE".
           03  FILLER         PIC X(8)   VALUE "    LINE".

       01  DETAIL-LINE           VALUE SPACES.
           03  D-REFERENCE    PIC X(14).
           03  FILLER         PIC X.
           03  D-BRANCH       PIC ZZZ9.
           03  D-DASH         PIC X.
           03  D-ACCTNO       PIC 9999999.
           03  FILLER         PIC X.
           03  D-DESC         PIC X(15).
           03  FILLER         PIC X.
           03  D-SOURCE       PIC X.
           03  FILLER         PIC X(2).
           03  D-UPDATE       PIC X.
           03  FILLER         PIC X(2).
           03  D-PERIOD       PIC ZZ.
           03  FILLER         PIC X.
           03  D-DEBIT-AMT    PIC ZZZ,ZZZ,ZZ9.99-.
           03  FILLER         PIC X.
           03  D-CREDIT-AMT   PIC ZZZ,ZZZ,ZZ9.99-.
           03  FILLER         PIC X  .
           03  D-EXPL         PIC X(32).
           03  FILLER         PIC X.
           03  D-DATE         PIC 99/99/99.
           03  D-LINE-NO      PIC ZZZZZ9.

       01  RANGE-LINE REDEFINES DETAIL-LINE.
           03  RANGE-DESC         PIC X(33).
           03  RANGE-SEL1         PIC X(20).
           03  FILLER REDEFINES RANGE-SEL1.
               05  RANGE-DATE1    PIC 9999/99/99.
               05  FILLER         PIC X(10).
           03  FILLER             PIC X(5).
           03  RANGE-SEL2         PIC X(40).
           03  FILLER REDEFINES RANGE-SEL2.
               05  RANGE-DATE2    PIC 9999/99/99.
               05  FILLER         PIC X(30).
           03  FILLER             PIC X(34).

       01  TOTAL-LINE.
           03  TOTS OCCURS 3.
               05  TOT-TYPE       PIC X(16).
               05  TOT-AMT        PIC ZZZZZ,ZZZ,ZZ9.99-.
               05  TOT-NO REDEFINES TOT-AMT 
                                  PIC ZZZZ,ZZZ,ZZZ,ZZ9- BLANK ZERO.
               05  FILLER         PIC XX VALUE "  ".

       COPY "LIBGL/WDAYGL_CMD.CPY".
       COPY "LIBWI/GRSCANW.CPY".
       COPY "LIBGB/SYSTEMW.CPY".
       COPY "LIBGB/GRWORKW.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/DACCESSW.CPY".
       COPY "LIBGB/GETPRW.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBGL/WDAYGL_DEF.CPY".
       COPY "LIBGL/WDAYGL_WKS.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBGL/WDAYGL_SCN.CPY".

       PROCEDURE DIVISION
            USING FORM-PATHNAME EXIT-PATHNAME.
       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
              WK-FILE PR-FILE.
       COPY "LIBGB/DECLRP.CPY".
           PERFORM CLOSE-GT2-FILE.
           PERFORM CLOSE-GT2-CONNECTION.
           PERFORM CLOSE-GR1-FILE.
           PERFORM CLOSE-BR-FILE.
           PERFORM CLOSE-PR-FILE.
           CLOSE WK-FILE.

       COPY "LIBGB/DECLRP2.CPY".

      *****************************************************
       MAIN-PROGRAM SECTION.
      *****************************************************

           PERFORM SQL-CONNECT.

      D    STOP MESS.
           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "WDAYGL" TO VDU-FORM-NAME.

           PERFORM INITIALIZATION.
           IF EXT-EOBUF-ERRCD NOT = " "
              GO TO END-PROGRAM.

           IF GROUP-FLAG = "Y"
              PERFORM OPEN-GR1-FILE
              MOVE ENT-GROUP TO GR-ID
              PERFORM SEARCH-GR-INIT.

           MOVE BEG-DATE TO GT-DATE1 SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE
           MOVE SQL-DATE-YYYY-MM-DD TO QGT2-WBEG-DATE1.

           MOVE END-DATE TO SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE
           MOVE SQL-DATE-YYYY-MM-DD TO QGT2-WEND-DATE1.

           PERFORM START-TOTAL-GT2-FILE.
           PERFORM READ-TOTAL-GT2-FILE-NEXT. *> EMBEDDED - GTFILE
           PERFORM CLOSE-TOTAL-GT2-FILE.

           PERFORM START-GT2-FILE. *> SSP - GTFILE
   
       NEXT-GT-RECORD.
           PERFORM READ-GT2-FILE-NEXT. *> EMBEDDED - GTFILE
           IF IO-BAD
              GO TO EXIT-PROCESS-GT.

      * TEST DATE; GT-DATE1 STORED AS CCYYMMDD
           IF GT-DATE1 > END-DATE
              GO TO EXIT-PROCESS-GT.

      * TEST G/L SOURCE CODE OPTION
           IF ALL-SRC-CODES NOT = "*"
              IF GT-SOURCE NOT = SOURCE-CODE
                 ADD 1 TO TOT-SKIPPED
                 GO TO NEXT-GT-RECORD.

      * SKIP EOD-DEPOSIT TRANSACTIONS
           IF GT-EXPL = "EOD-DEPOSIT"
              ADD 1 TO TOT-SKIPPED
              GO TO NEXT-GT-RECORD.

      * TEST PREVIOUSLY EXTRACTED OPTION
           IF PREV-EXTRACTED = "S"
              IF (GT-SMITH-SENT = "X" OR "E")
                 ADD 1 TO TOT-SKIPPED
                 GO TO NEXT-GT-RECORD.
           IF PREV-EXTRACTED = "O"
              IF NOT (GT-SMITH-SENT = "X" OR "E")
                 ADD 1 TO TOT-SKIPPED
                 GO TO NEXT-GT-RECORD.

      * TEST G/L BRANCH / GROUP
           IF GROUP-FLAG = "Y"
              MOVE GT-BRANCH TO WGR-SRESULT
              PERFORM SEARCH-GR-VERIFY
              IF WGR-ERR > 0
                 ADD 1 TO TOT-SKIPPED-BGR
                 GO TO NEXT-GT-RECORD.
           IF GROUP-FLAG NOT = "Y"
              IF GT-BRANCH < BEG-BR OR GT-BRANCH > END-BR
                 ADD 1 TO TOT-SKIPPED-BGR
                 GO TO NEXT-GT-RECORD.

           MOVE "N" TO FIRST-TIME.
           PERFORM CREATE-WORK-RECORD.
           IF EXCEPTION-FG = " "
              PERFORM UPDATE-GT-EXT-FLAG.

           GO TO NEXT-GT-RECORD.

       EXIT-PROCESS-GT.
           PERFORM CLOSE-GT2-FILE.

       END-ROUTINE.
           IF FIRST-TIME = "Y"
              MOVE "N" TO RECORDS-FOUND
              GO TO END-PROGRAM.

           PERFORM PRINT-GRAND-TOTALS.

           PERFORM PRINT-RANGE-LINES.

           GO TO END-PROGRAM.

      *****************************************************
       CREATE-WORK-RECORD SECTION.
      *****************************************************
           MOVE SPACES TO WK-REC EXCEPTION-FG.
           ADD 1 TO OUTPUT-LINE-ORDER.
           MOVE GT-DATE1  TO NDTE-DATE.
           MOVE 31        TO NDTE-DD.
           PERFORM NEW-DATE-CALCULATION.
           MOVE NDTE-DATE TO GTDATE-YYYYMMDD.
           INSPECT GTDATE-YYYYMMDD REPLACING ALL "/" BY "-".
           MOVE GT-BRANCH TO BR-NO.

           PERFORM READ-BR-FILE.
           IF IO-BAD
              MOVE "B" TO EXCEPTION-FG
              PERFORM EXCEPTION-ERROR
              ADD 1 TO TOT-ERRORS
              GO TO EXIT-CREATE-WORK-REC.
      * #1696 USE COLLECTION CENTER STATE
      * VIRTUAL BRANCHES LEGALLY NEED TO USE "SC" FOR THEIR STATE EVERYWHERE
      * EXCEPT HERE, ACCOUNTING NEEDS THE ACTUAL STATE HERE
      * ONLY FOR BRANCHES 7000-7099
           IF GT-BRANCH > 6999 AND < 7100
              MOVE BR-CC-STATE TO GTSTATE
              MOVE "-" TO OUTPUT-DASH
           ELSE
              IF GT-BRANCH < 9000
                 MOVE BR-STATE TO GTSTATE
                 MOVE "-" TO OUTPUT-DASH
              ELSE
                 MOVE SPACES TO GTSTATE OUTPUT-DASH.
           MOVE GT-ACCTNO TO OUTPUT-ACCTNO.
           IF GT-DRCRFG = 1
              MOVE GT-AMT TO OUTPUT-DEBIT
              ADD GT-AMT  TO TOTAL-DEBITS
              MOVE SPACES TO OUTPUT-CREDIT
           ELSE
              MOVE GT-AMT TO OUTPUT-CREDIT
              ADD GT-AMT  TO TOTAL-CREDITS
              MOVE SPACES TO OUTPUT-DEBIT.
           MOVE GT-BRANCH TO OUTPUT-BRANCH.
           MOVE GT-EXPL   TO OUTPUT-EXPL.
           INSPECT OUTPUT-EXPL REPLACING ALL "," BY ";".
           CALL "C$JUSTIFY" USING OUTPUT-BRANCH, "L".

           STRING
               "1", ",",
               OUTPUT-LINE-ORDER, ",",
      * COMPANY_REFERENCE_ID
               S-1 ",",
               "WAC", ",",
               "USD", ",",
               "ACTUALS", ",",
               OUTPUT-GTDATE, ",",
               OUTPUT-EXPL, ",",
      * ACCOUNT_SET_ID
               S-2 ",",
               "STANDARD", ",",
      * LEDGER_ACCOUNT_ID
               S-3 ",",
               OUTPUT-ACCTNO, ",",
               OUTPUT-DEBIT, ",",
               OUTPUT-CREDIT, ",",
               "USD", ",",
               GT-REF, ",",
      * COMPANY_REFERENCE_ID
               S-1 ",",
               OUTPUT-STATE, ",",
               ",",
               ",",
               ",",
               OUTPUT-BRANCH
               WS-CR
               DELIMITED BY "  " INTO WK-REC.
           PERFORM WRITE-WK-FILE.
           IF IO-BAD
              GO TO IO-ERROR.
           ADD 1 TO TOT-EXTRACTED.
       EXIT-CREATE-WORK-REC.
           EXIT.

      *****************************************************
       EXCEPTION-ERROR SECTION.
      *****************************************************
           MOVE GT-REF             TO D-REFERENCE.
           MOVE GT-BRANCH          TO D-BRANCH.
           MOVE "-"                TO D-DASH.
           MOVE GT-ACCTNO          TO D-ACCTNO.
           MOVE GT-SOURCE          TO D-SOURCE.
           MOVE GT-FLAG            TO D-UPDATE.
           MOVE GT-PERIOD          TO D-PERIOD.
           IF EXCEPTION-FG = "B"
              MOVE "INVALID BRANCH: &&&&" TO D-DESC
              INSPECT D-DESC REPLACING FIRST "&&&&" BY D-BRANCH.
           IF GT-AMT < 0
              MOVE GT-AMT          TO D-CREDIT-AMT
           ELSE
              MOVE GT-AMT          TO D-DEBIT-AMT.
           MOVE GT-LINE            TO D-LINE-NO.
           MOVE GT-EXPL            TO D-EXPL.
           MOVE GT-DATE1           TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY        TO D-DATE.
           PERFORM WRITE-DETAIL-LINE.

      *************************************
       HEAD-PAGE SECTION.
      *************************************
           ADD 1 TO PAGE-NUMBER.
           MOVE PAGE-NUMBER TO HD-PAGE.
           PERFORM GET-TIME.
           MOVE TIME-EDIT TO HD-TIME.
 
           MOVE HLINE-1 TO PR-REC.
           MOVE 99 TO LN-FEED.
           PERFORM WRITE-PR-REC.
 
           MOVE HLINE-2 TO PR-REC.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-PR-REC.
 
           IF TOT-ERRORS > 0
              MOVE HLINE-3 TO PR-REC
              MOVE 2 TO LN-FEED
              PERFORM WRITE-PR-REC.
 
           MOVE SPACES TO PR-REC.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-PR-REC.
 
           IF TOT-ERRORS > 0
              MOVE 5 TO LN-COUNT
           ELSE
              MOVE 3 TO LN-COUNT.
           MOVE 2 TO LN-FEED.


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

      ****************************************
       PRINT-GRAND-TOTALS SECTION.
      ****************************************
           MOVE "TOTAL EXTRACTED:"  TO TOT-TYPE(1).
           MOVE TOT-EXTRACTED       TO TOT-NO(1).
           MOVE "TOTAL SKPPED:"     TO TOT-TYPE(2).
           ADD TOT-SKIPPED-BGR      TO TOT-SKIPPED.
           MOVE TOT-SKIPPED         TO TOT-NO(2).
           IF TOT-ERRORS > 0
              MOVE "TOTAL ERRORS:"  TO TOT-TYPE(3)
              MOVE TOT-ERRORS       TO TOT-NO(3)
           ELSE
              MOVE SPACES           TO TOT-TYPE(3)
              MOVE 0                TO TOT-NO(3).

           ADD LN-FEED LN-COUNT GIVING LN-WK.
           IF LN-WK > EXT-CONAME-RPT-LINES
              PERFORM HEAD-PAGE.

           MOVE TOTAL-LINE TO PR-REC.
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-PR-REC.

           MOVE "TOTAL DEBITS:"  TO TOT-TYPE(1).
           MOVE TOTAL-DEBITS     TO TOT-AMT(1).
           MOVE "TOTAL CREDITS:" TO TOT-TYPE(2).
           MOVE TOTAL-CREDITS    TO TOT-AMT(2).
           ADD TOTAL-DEBITS TOTAL-CREDITS GIVING TOTAL-DRCR.
           MOVE "TOTAL :"        TO TOT-TYPE(3).
           MOVE TOTAL-DRCR       TO TOT-AMT(3).

           MOVE TOTAL-LINE TO PR-REC.
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-PR-REC.


      ****************************************
       INITIALIZATION SECTION.
      ****************************************
           MOVE WDAYGL-QUEUE-POS TO Q-POS.
           MOVE WDAYGL-COPIES-POS TO C-POS.
           MOVE WDAYGL-PRU-POS   TO P-POS.
           MOVE SPACES TO Q-BUF C-BUF P-BUF.
       COPY "LIBGB/EOINIT.CPY".

           IF BEG-DATE = 0
              MOVE 19000101 TO BEG-DATE.

           IF EXT-ROUTE-BUF = "00"
              MOVE "WORKDAY GL TRANS EXTRACT IN PROGRESS..." TO MESS
              MOVE MESS-LIST TO WR-LIST
              MOVE MESS TO WR-BUF
              PERFORM CALL-WRFORM.

           MOVE EXT-CONAME-CONAME  TO HD-CONAME.
           MOVE EXT-CONAME-SYSDATE TO HD-DATE.
           MOVE EXT-CONAME-USER-ID TO HD-USERID.

           IF EXT-ROUTE-BUF = "70"
              MOVE EXT-EOBUF-MESS TO H-TITLE.

           IF EXT-EOBUF-GROUP-FG = "Y"
              MOVE BEG-BR TO SV-BEG-BR
              MOVE END-BR TO SV-END-BR
              MOVE GROUP-FLAG TO SV-GROUP-FLAG
              MOVE ENT-GROUP TO SV-ENT-GROUP
              MOVE EXT-EOBUF-GROUP-PRPATH TO PRU-FILE
              IF EXT-EOBUF-BRNO NUMERIC
                 MOVE EXT-EOBUF-BRNO TO BEG-BR END-BR
                 MOVE "N" TO GROUP-FLAG 
              ELSE
                 MOVE EXT-EOBUF-GROUP TO ENT-GROUP
                 MOVE 0 TO BEG-BR
                 MOVE 9999 TO END-BR
                 MOVE "Y" TO GROUP-FLAG.

       SETUP-OUTPUT-PATH.
           MOVE EXT-FILPATH-GLOBAL TO WK-PATH-FIL.
           MOVE END-DATE           TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-YYMMDD.
           MOVE DATE-YYMMDD        TO WK-PATH-YMD.
           CALL "C$TOLOWER" USING WK-PATH, VALUE 42.
           MOVE WK-PATH-DIR TO DIR-ACCESS-BUF.
           PERFORM DIR-ACCESS-CALL.
           IF STAT-BAD
              MOVE "&&&&&&&&&&&&&&&&&&&&&&&&& DOES NOT EXIST" TO MESS
              INSPECT MESS REPLACING FIRST "&&&&&&&&&&&&&&&&&&&&&&&&&"
                                            BY WK-PATH-DIR
              PERFORM SEND-MESS
              GO TO ENDIT.

           MOVE WK-PATH TO ACCESS-BUF.
           PERFORM ACCESS-CALL.
           IF STAT-GOOD
              MOVE "OUTPUT FILE &&&&&&&&&&&&&&&&& EXISTS, ABORTED"
                 TO MESS
              INSPECT MESS REPLACING FIRST "&&&&&&&&&&&&&&&&&" BY
                                            WK-PATH-FILE
              PERFORM SEND-MESS
              GO TO ENDIT.
           PERFORM OPEN-WK-FILE-OUTPUT.
           PERFORM CREATE-HEADER-RECORD.

           PERFORM OPEN-PR-FILE.
           PERFORM OPEN-BR-FILE.
           PERFORM OPEN-GT2-FILE.

           GO TO EXIT-INITIALIZE.
 
       ENDIT.
           MOVE "X" TO EXT-EOBUF-ERRCD.
       EXIT-INITIALIZE.
           EXIT.

      *******************************
       EO-EXEC-AUDIT SECTION.
      *******************************
           MOVE EXT-EOBUF-DATE1 TO BEG-DATE.
           MOVE EXT-EOBUF-DATE2 TO END-DATE.

      *****************************************************
       ENTER-PARAMETERS SECTION.
      *****************************************************

           IF UP-KEY
              GO TO E-PREV-EXTRACTED.

       E-BEG-BR.
           MOVE "E  A000BR1." TO VDU.
           PERFORM SCREENIO.
           IF F1-KEY GO TO EXIT-PARM.
           IF F3-KEY GO TO ALL-BRS.
           IF F7-KEY GO TO EXIT-PARM.

           IF F6-KEY
              MOVE "WI/GRSCAN" TO FORM-NAM
              CALL FORM-PROGX USING FORM-PATH GRSCAN-BUF EXIT-PATHNAME
              CANCEL FORM-PROGX
              IF GRSCAN-STATUS NOT = "00"
                 GO TO E-BEG-BR
              ELSE
                 MOVE "S  A000BR1." TO VDU
                 MOVE GRSCAN-KEY TO VDUBUF
                 PERFORM SCREENIO
                 MOVE GRSCAN-KEY TO VDUBUF
                 MOVE "00" TO VDUSTATUS.
           IF NOT ENTER-KEY
              GO TO E-BEG-BR.

           MOVE VDUBUF TO WGR-WORD ENT-GROUP.

           IF WGR-WORD-ZERO
              GO TO BRANCH-IS-NUMERIC.

           MOVE 0 TO WGR-MIN-LEVEL.
           PERFORM CHECK-MEMBER.
           IF WGR-ERR > 0
              MOVE WGR-MSG(WGR-ERR) TO MESS
              PERFORM SEND-MESS
              GO TO E-BEG-BR.
           IF WGR-TYPE = "A"
              MOVE "S  A000BR2." TO VDU
              MOVE SPACES TO VDUBUF
              PERFORM SCREENIO
              MOVE "Y" TO GROUP-FLAG
              MOVE 0 TO BEG-BR
              MOVE 9999 TO END-BR
              GO TO E-SOURCE-CODE.

       BRANCH-IS-NUMERIC.
           MOVE "N" TO GROUP-FLAG.
           MOVE WGR-SRESULT TO BEG-BR.

           MOVE "SN N040BR1." TO VDU.
           MOVE BEG-BR TO VDUNUM0.
           PERFORM SCREENIO.

        E-END-BR.
           MOVE "ENNN040BR2." TO VDU.
           PERFORM SCREENIO.
           IF F1-KEY GO TO EXIT-PARM.
           IF F3-KEY GO TO ALL-BRS.
           IF UP-KEY GO TO E-BEG-BR.
           IF F7-KEY GO TO EXIT-PARM.
           IF NOT ENTER-KEY GO TO E-END-BR.
           MOVE VDUNUM0 TO END-BR.
           IF BEG-BR > END-BR
              MOVE "INVALID BRANCH RANGE" TO MESS
              PERFORM SEND-MESS
              GO TO E-BEG-BR.

           GO TO E-SOURCE-CODE.
       ALL-BRS.
           MOVE "SNNN040BR1." TO VDU.
           MOVE 1 TO BEG-BR VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN040BR2." TO VDU.
           MOVE 9999 TO END-BR VDUNUM0.
           PERFORM SCREENIO.
           MOVE "N" TO GROUP-FLAG.


       E-SOURCE-CODE.
           MOVE "ENNN010SRC." TO VDU.
           PERFORM SCREENIO.
           IF F1-KEY GO TO EXIT-PARM.
           IF F3-KEY GO TO SEND-ALL-SRC.
           IF UP-KEY GO TO E-BEG-BR.
           IF F7-KEY GO TO EXIT-PARM.
           IF NOT ENTER-KEY
              GO TO E-SOURCE-CODE.
           MOVE VDUNUM0 TO SOURCE-CODE.
           GO TO E-BEG-DATE.
       SEND-ALL-SRC.
            MOVE "S  A010SRC." TO VDU.
            MOVE "*" TO ALL-SRC-CODES VDUBUF.
            PERFORM SCREENIO.

       E-BEG-DATE.
           IF EXT-ROUTE-BUF = "40" OR "50"
              GO TO E-PREV-EXTRACTED.
           MOVE "ERNM060DATE1." TO VDU
           PERFORM SCREENIO.
           IF F1-KEY GO TO EXIT-PARM.
           IF UP-KEY GO TO E-SOURCE-CODE.
           IF F7-KEY GO TO EXIT-PARM.
           IF NOT ENTER-KEY GO TO E-BEG-DATE.
           MOVE VDU-DATE-YYYYMMDD TO BEG-DATE.
       E-END-DATE.
           MOVE "ERNM060DATE2." TO VDU
           PERFORM SCREENIO.
           IF F1-KEY GO TO EXIT-PARM.
           IF UP-KEY GO TO E-BEG-DATE.
           IF F7-KEY GO TO EXIT-PARM.
           IF NOT ENTER-KEY GO TO E-END-DATE.
           MOVE VDU-DATE-YYYYMMDD TO END-DATE.
           IF BEG-DATE-YYYYMM NOT = END-DATE-YYYYMM
              MOVE "DATE RANGE CANNOT CROSS MONTHS" TO MESS
              PERFORM SEND-MESS
              GO TO E-BEG-DATE.
       E-PREV-EXTRACTED.
           MOVE "E  A000PREV." TO VDU.
           PERFORM SCREENIO.
           IF F1-KEY GO TO EXIT-PARM.
           IF UP-KEY GO TO E-END-DATE.
           IF F7-KEY GO TO EXIT-PARM.
           IF NOT ENTER-KEY
              GO TO E-PREV-EXTRACTED.
           MOVE VDUBUF TO PREV-EXTRACTED.
           IF PREV-EXTRACTED = "S" GO TO EXIT-PARM.
           IF PREV-EXTRACTED = "I" GO TO EXIT-PARM.
           IF PREV-EXTRACTED = "O" GO TO EXIT-PARM.
           GO TO E-PREV-EXTRACTED.

       EXIT-PARM.
           EXIT.

      *****************************************
       DISPLAY-PARAMETERS SECTION.
      *****************************************

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

           MOVE "S  A010SRC." TO VDU.
           MOVE ALL-SRC-CODES TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "S  A010PREV." TO VDU.
           MOVE PREV-EXTRACTED TO VDUBUF.
           PERFORM SCREENIO.

      *****************************************************
       CREATE-HEADER-RECORD SECTION.
      *****************************************************
           MOVE SPACES TO WK-REC.
           MOVE WS-HEADER-REC TO WK-REC.
           PERFORM WRITE-WK-FILE.
           IF IO-BAD
              GO TO IO-ERROR.

      ********************************************
      * UPDATE GT EXTRACTION FLAG
      * GT-SMITH-SENT:
      *   " " = NOT THRU GTFORC, NOT THRU EXTRACTION
      *    Y  = THRU GTFORC, NOT THRU EXTRACTION
      *    E  = NOT THRU GTFORC, THRU EXTRACTION
      *    X  = THRU GTFORC AND THRU EXTRACTION
      ********************************************
       UPDATE-GT-EXT-FLAG SECTION.
           IF (GT-SMITH-SENT = "X" OR "E")
              GO TO EXIT-UPDATE-GT-FLAG.
           IF GT-SMITH-SENT = "Y"
              MOVE "X" TO GT-SMITH-SENT
           ELSE
              MOVE "E" TO GT-SMITH-SENT.
           PERFORM REWRITE-GT2-FILE. *> EMBEDDED - GTFILE
       EXIT-UPDATE-GT-FLAG.
           EXIT.

      *******************************
       PRINT-RANGE-LINES SECTION.
      *******************************

           MOVE 3 TO LN-FEED.
           IF GROUP-FLAG = "Y"
              MOVE "GROUP SELECTED                 :" TO RANGE-DESC 
              MOVE ENT-GROUP                          TO RANGE-SEL1
           ELSE
              MOVE "BEG/ENDING BRANCH RANGE        :" TO RANGE-DESC
              MOVE BEG-BR                             TO RANGE-SEL1
              MOVE END-BR                             TO RANGE-SEL2.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "SOURCE CODE                    :" TO RANGE-DESC.
           MOVE SOURCE-CODE                        TO RANGE-SEL1.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "BEG/ENDING DATE RANGE          :" TO RANGE-DESC.
           MOVE BEG-DATE                           TO RANGE-DATE1.
           MOVE END-DATE                           TO RANGE-DATE2.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "INCLUDE PREVIOUSLY EXTRACTED   :" TO RANGE-DESC.
           MOVE PREV-EXTRACTED                     TO RANGE-SEL1.
           IF PREV-EXTRACTED = "S"
              MOVE "SKIP IF PREVIOUSLY EXTRACTED " TO RANGE-SEL2.
           IF PREV-EXTRACTED = "I" 
              MOVE "INCLUDE IF PREV EXTRACTED    " TO RANGE-SEL2.
           IF PREV-EXTRACTED = "O" 
              MOVE "ONLY IF PREVIOUSLY EXTRACTED " TO RANGE-SEL2.
           PERFORM WRITE-DETAIL-LINE.

       END-RANGE.
           EXIT.

      **********************************************************
       FORM-RESET SECTION.
      **********************************************************
           MOVE EXT-CONAME-CONAME  TO WDAYGL-CONAME.
           MOVE EXT-CONAME-SYSDATE TO WDAYGL-SYSDATE.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DISPLAY WDAYGL-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE WDAYGL-FORM-FIELDS TO WR-BUF.
           MOVE WDAYGL-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      **********************************************************
       SETUP-LINK-INFO SECTION.
      **********************************************************
           MOVE WDAYGL-HELP--FILE TO HELP-LINK-FILE.
           MOVE WDAYGL-HELP--DIR  TO HELP-LINK-DIR.

      **********************************************************
       VDU-CONVERT-FIELD SECTION.
      **********************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBGL/WDAYGL_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/GRWORK.CPY".
       COPY "LIBGB/GRSERCH.CPY".
       COPY "LIBGB/TIMEIO.CPY".

      *********************************
       MISC SECTION.
      *********************************
       COPY "LIBGB/GETPR.CPY".
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/DACCESS.CPY".
       COPY "LIBGB/SYSTEM.CPY".
       COPY "LIBGB/GETENV.CPY".
 
      *****************************************************
       IO-ROUTINES SECTION.
      *****************************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGL/GLGTGS_SQL.CPY".
       COPY "LIBGB/GBGRGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBPRINT.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBGB/GBGR1RN.CPY".

      * COPY "LIBGL/GLGT2IN.CPY".
      * COPYMEMBER: LIBGL/GLGT2IN
       LOAD-GT2-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( GT-PATH-OVERRIDE = "Y" )
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( GT-PATH-OVERRIDE = SPACES )
              MOVE EXT-FILPATH-BASE        TO GT-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO GT-ALL-MACHINE
              MOVE GT-ALL-PATH             TO GT-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( GT-PATH-OVERRIDE = "B" )
              MOVE FILPATH-BASE            TO GT-ALL-BASE
              MOVE FILPATH-MACHINE         TO GT-ALL-MACHINE
              MOVE GT-ALL-PATH             TO GT-PATH.

      *-----------------------------------------------------------------
       OPEN-GT2-FILE.
           PERFORM LOAD-GT2-FILE.
           PERFORM OPEN-IT.
           MOVE GT-PATH-SQL TO E-FILE.

           IF GT2-CONN-OPEN-SW = "N" 
              MOVE "Y"         TO GT2-CONN-OPEN-SW
              EXEC SQL CONNECT TO :SQL-CONNECT-SERVER
                               AS GT2_CONNECT 
                   USER :SQL-CONNECT-USERID USING :SQL-CONNECT-PSWD
              END-EXEC.

           IF GT2-TOTAL-OPEN-SW = "N" 
              MOVE "Y"         TO GT2-TOTAL-OPEN-SW
              EXEC SQL CONNECT TO :SQL-CONNECT-SERVER
                               AS GT2_TOTAL_CONNECT
                   USER :SQL-CONNECT-USERID USING :SQL-CONNECT-PSWD
              END-EXEC.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

           EXEC SQL SET CONNECTION C1 END-EXEC.

      *-----------------------------------------------------------------
       START-GT2-FILE. *> SSP - GTFILE
           PERFORM START-IT.
           MOVE GT-PATH-SQL TO E-FILE.
           MOVE GT2-KEY TO E-KEYX.
           IF ( GT2-CURSOR-STAT-GOOD )
              PERFORM CLOSE-GT2-FILE.

      *     IF ( QGT2-WEND-DATE1 = SPACES )
      *        MOVE GT-DATE1            TO SQL-DATE-YYYYMMDD
      *        PERFORM SQL-SET-DATE
      *        MOVE SQL-DATE-YYYY-MM-DD TO QGT2-WBEG-DATE1

      *        MOVE EXT-JULIAN-END      TO SQL-DATE-YYYYMMDD
      *        PERFORM SQL-SET-DATE
      *        MOVE SQL-DATE-YYYY-MM-DD TO QGT2-WEND-DATE1.

           MOVE BEG-DATE                TO SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD     TO QWS-WBEG-DATE.
 
           IF ( QGT2-WEND-DATE1 = SPACES )
               MOVE EXT-JULIAN-END      TO SQL-DATE-YYYYMMDD
               PERFORM SQL-SET-DATE
               MOVE SQL-DATE-YYYY-MM-DD TO QWS-WEND-DATE
           ELSE  
               MOVE END-DATE            TO SQL-DATE-YYYYMMDD
               PERFORM SQL-SET-DATE
               MOVE SQL-DATE-YYYY-MM-DD TO QWS-WEND-DATE.

           IF ALL-SRC-CODES  = "*"
               MOVE 0                   TO QWS-WBEG-SOURCE-CODE
               MOVE 9                   TO QWS-WEND-SOURCE-CODE
           ELSE
               MOVE SOURCE-CODE         TO QWS-WBEG-SOURCE-CODE
                                           QWS-WEND-SOURCE-CODE.

           MOVE PREV-EXTRACTED          TO QWS-PREV-EXTRACTED.   

      *  STORED PROCEDURE:
      *
      *    SELECT GTFILE.GT_FLAG, 
      *           GTFILE.GT_REF, 
      *           CAST(GTFILE.GT_DATE1  AS VARCHAR(10)), 
      *           GTFILE.GT_SOURCE,    
      *           CAST(GTFILE.GT_DATE2  AS VARCHAR(10)), 
      *           GTFILE.GT_BRANCH,
      *           GTFILE.GT_ACCTNO,
      *           CAST(GTFILE.GT_DATE3  AS VARCHAR(10)),
      *           GTFILE.GT_DRCRFG,
      *           GTFILE.GT_LINE,
      *           GTFILE.GT_AMT,
      *           GTFILE.GT_EXPL,
      *           GTFILE.GT_PERIOD,
      *           GTFILE.GT_SMITH_SENT
      *      FROM DBO.GTFILE GTFILE 
      *     WHERE  (GTFILE.GT_DATE1     BETWEEN @QWS_WBEG_DATE 
      *                                     AND @QWS_WEND_DATE)
      *       AND  (GTFILE.GT_SOURCE    BETWEEN @QWS_WBEG_SOURCE_CODE
      *                                     AND @QWS_WEND_SOURCE_CODE)                 
      *       AND ((@QWS_PREV_EXTRACTED       = 'I')
      *        OR  (@QWS_PREV_EXTRACTED       = 'S'
      *       AND   GTFILE.GT_SMITH_SENT NOT IN ('X', 'E'))
      *        OR  (@QWS_PREV_EXTRACTED       = 'O'
      *       AND   GTFILE.GT_SMITH_SENT     IN ('X', 'E')))  
      *       AND  (GTFILE.GT_EXPL           != 'EOD-DEPOSIT') 
      *     ORDER BY GTFILE.GT_DATE1,
      *              GTFILE.GT_SOURCE,
      *              GTFILE.GT_DATE2,
      *              GTFILE.GT_BRANCH,
      *              GTFILE.GT_ACCTNO,
      *              GTFILE.GT_DATE3,
      *              GTFILE.GT_DRCRFG,
      *              GTFILE.GT_LINE;  

           EXEC SQL SET CONNECTION GT2_CONNECT END-EXEC.

           EXEC SQL
            DECLARE GT2_CURSOR CURSOR FOR
                :GT2-RETURN-CODE = EXEC SSP_WDAYGL_GTFILE_SELECT 
              ( :QWS-WBEG-DATE,       
                :QWS-WEND-DATE,        
                :QWS-WBEG-SOURCE-CODE,
                :QWS-WEND-SOURCE-CODE,
                :QWS-PREV-EXTRACTED ) 
           END-EXEC.

           EXEC SQL
               OPEN GT2_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE SPACES TO QGT2-WBEG-DATE1
                          QGT2-WEND-DATE1.

           MOVE STAT---GOOD TO GT2-CURSOR-STAT.

           EXEC SQL SET CONNECTION C1 END-EXEC. 

      *-----------------------------------------------------------------
       READ-GT2-FILE-NEXT. *> EMBEDDED - GTFILE
           PERFORM READ-IT.
           MOVE GT-PATH-SQL TO E-FILE.
           MOVE GT2-KEY TO E-KEYX.
           INITIALIZE QGT-REC.

           EXEC SQL SET CONNECTION GT2_CONNECT END-EXEC.

           IF ( GT2-CURSOR-STAT-GOOD )
              EXEC SQL
                FETCH GT2_CURSOR 
                  INTO 
                    :QGT-FLAG, 
                    :QGT-REF, 
                    :QGT-DATE1, 
                    :QGT-SOURCE,
                    :QGT-DATE2, 
                    :QGT-BRANCH,
                    :QGT-ACCTNO,
                    :QGT-DATE3, 
                    :QGT-DRCRFG, 
                    :QGT-LINE,
                    :QGT-AMT,
                    :QGT-EXPL,
                    :QGT-PERIOD,
                    :QGT-SMITH-SENT
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-GT2-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-GT-FIELDS.

           EXEC SQL SET CONNECTION C1 END-EXEC.

      *-----------------------------------------------------------------
       REWRITE-GT2-FILE. *> EMBEDDED - GTFILE

           ACCEPT GT-LTOUCH-DATE FROM CENTURY-DATE.
           PERFORM REWRITE-IT.
           MOVE GT-PATH-SQL TO E-FILE.
           MOVE GT2-KEY TO E-KEYX.
           PERFORM SET-GT-FIELDS.

           MOVE '2' TO SQL-UPDATE-CONN-USE-SW. *> USE UPDATE_CONNECT
           PERFORM SQL-SWAP-UPDATE-CONNECTION.

           EXEC SQL
            UPDATE DBO.GTFILE
             SET                   
              GTFILE.GT_FLAG        = :QGT-FLAG, 
              GTFILE.GT_REF         = :QGT-REF, 
              GTFILE.GT_DATE1       = :QGT-DATE1, 
              GTFILE.GT_SOURCE      = :QGT-SOURCE,
              GTFILE.GT_DATE2       = :QGT-DATE2, 
              GTFILE.GT_BRANCH      = :QGT-BRANCH,
              GTFILE.GT_ACCTNO      = :QGT-ACCTNO,
              GTFILE.GT_DATE3       = :QGT-DATE3, 
              GTFILE.GT_DRCRFG      = :QGT-DRCRFG, 
              GTFILE.GT_LINE        = :QGT-LINE,
              GTFILE.GT_AMT         = :QGT-AMT,
              GTFILE.GT_EXPL        = :QGT-EXPL,
              GTFILE.GT_PERIOD      = :QGT-PERIOD,
              GTFILE.GT_SMITH_SENT  = :QGT-SMITH-SENT,
              GTFILE.GT_LTOUCH_DATE = :QGT-LTOUCH-DATE 
       
            WHERE GTFILE.GT_SOURCE  = :QGT-SOURCE
              AND GTFILE.GT_DATE2   = :QGT-DATE2
              AND GTFILE.GT_BRANCH  = :QGT-BRANCH
              AND GTFILE.GT_ACCTNO  = :QGT-ACCTNO
              AND GTFILE.GT_DATE3   = :QGT-DATE3
              AND GTFILE.GT_DRCRFG  = :QGT-DRCRFG
              AND GTFILE.GT_LINE    = :QGT-LINE
           END-EXEC.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      * NOTE: THE WHERE CONSTRAINT IN THE ABOVE REWRITE NEEDS TO USE
      *       THE PRIMARY KEY WHERE CONSTRAINT; CANNOT DO A REWRITE ON
      *       THE SECONDARY KEY SINCE THE Q??#-KEY IS CHANGED AND WILL
      *       NOT FIND THE ORIGINAL KEY, AS WELL AS Q??#-KEY COULD
      *       MATCH ANOTHER RECORD/ROW THAT SHOULD NOT CHANGE!!!
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO REWRITE-GT2-FILE.

      *    IF ( IO-FG = 0 )
      *       EXEC SQL COMMIT END-EXEC
      *       PERFORM SQL-IO-VALIDATION.

           MOVE  '1' TO SQL-UPDATE-CONN-USE-SW. *> USE C1 CONNECT
           PERFORM SQL-SWAP-UPDATE-CONNECTION.

      *-----------------------------------------------------------------
       CLOSE-GT2-FILE.

           PERFORM CLOSE-IT.

           IF ( GT2-CURSOR-STAT-GOOD )
              EXEC SQL SET CONNECTION GT2_CONNECT END-EXEC
              EXEC SQL
                   CLOSE GT2_CURSOR
              END-EXEC
              EXEC SQL SET CONNECTION C1 END-EXEC
              MOVE STAT---BAD TO GT2-CURSOR-STAT.

      *-----------------------------------------------------------------
       START-TOTAL-GT2-FILE. *> EMBEDDED - GTFILE TOTALS
           PERFORM START-IT.
           MOVE GT-PATH-SQL TO E-FILE.
           MOVE GT2-KEY TO E-KEYX.
           IF ( GT2-CURSOR-STAT-GOOD )
              PERFORM CLOSE-TOTAL_GT2-FILE.

      *     IF ( QGT2-WEND-DATE1 = SPACES )
      *        MOVE GT-DATE1            TO SQL-DATE-YYYYMMDD
      *        PERFORM SQL-SET-DATE
      *        MOVE SQL-DATE-YYYY-MM-DD TO QGT2-WBEG-DATE1

      *        MOVE EXT-JULIAN-END      TO SQL-DATE-YYYYMMDD
      *        PERFORM SQL-SET-DATE
      *        MOVE SQL-DATE-YYYY-MM-DD TO QGT2-WEND-DATE1.

           MOVE BEG-DATE                TO SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD     TO QWS-WBEG-DATE.
 
           IF ( QGT2-WEND-DATE1 = SPACES )
               MOVE EXT-JULIAN-END      TO SQL-DATE-YYYYMMDD
               PERFORM SQL-SET-DATE
               MOVE SQL-DATE-YYYY-MM-DD TO QWS-WEND-DATE
           ELSE  
               MOVE END-DATE            TO SQL-DATE-YYYYMMDD
               PERFORM SQL-SET-DATE
               MOVE SQL-DATE-YYYY-MM-DD TO QWS-WEND-DATE.

           IF ALL-SRC-CODES  = "*"
               MOVE 0                   TO QWS-WBEG-SOURCE-CODE
               MOVE 9                   TO QWS-WEND-SOURCE-CODE
           ELSE
               MOVE SOURCE-CODE         TO QWS-WBEG-SOURCE-CODE
                                           QWS-WEND-SOURCE-CODE.

           MOVE PREV-EXTRACTED          TO QWS-PREV-EXTRACTED.   

      *  STORED PROCEDURE:
      *
      * SELECT SUM(CASE WHEN (GTFILE.GT_SOURCE < @QWS_WBEG_SOURCE_CODE
      *                   OR  GTFILE.GT_SOURCE > @QWS_WEND_SOURCE_CODE)
      *                   OR  (@QWS_PREV_EXTRACTED    = 'S'
      *                  AND   GTFILE.GT_SMITH_SENT  IN ('X', 'E'))
      *                   OR  (@QWS_PREV_EXTRACTED    = 'O'
      *                  AND   GTFILE.GT_SMITH_SENT NOT IN ('X', 'E'))
      *                   OR  (GTFILE.GT_EXPL         = 'EOD-DEPOSIT') 
      *                 THEN 1
      *                 ELSE 0
      *             END) AS QWS_TOTAL_SKIPPED 
      *   FROM DBO.GTFILE GTFILE 
      *  WHERE (GTFILE.GT_DATE1     BETWEEN @QWS_WBEG_DATE 
      *                                 AND @QWS_WEND_DATE);


           EXEC SQL SET CONNECTION GT2_TOTAL_CONNECT END-EXEC.

           EXEC SQL
            DECLARE GT2_TOTAL_CURSOR CURSOR FOR
                :GT2-RETURN-CODE = EXEC SSP_WDAYGL_GTFILE_TOTAL_SELECT 
              ( :QWS-WBEG-DATE,       
                :QWS-WEND-DATE,        
                :QWS-WBEG-SOURCE-CODE,
                :QWS-WEND-SOURCE-CODE,
                :QWS-PREV-EXTRACTED ) 
           END-EXEC.

           EXEC SQL
               OPEN GT2_TOTAL_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE SPACES TO QGT2-WBEG-DATE1
                          QGT2-WEND-DATE1.

           MOVE STAT---GOOD TO GT2-CURSOR-STAT.

           EXEC SQL SET CONNECTION C1 END-EXEC. 

      *-----------------------------------------------------------------
       READ-TOTAL-GT2-FILE-NEXT. *> EMBEDDED - GTFILE TOTALS
           PERFORM READ-IT.
           MOVE GT-PATH-SQL TO E-FILE.
           MOVE GT2-KEY TO E-KEYX.
           INITIALIZE QGT-REC.

           EXEC SQL SET CONNECTION GT2_TOTAL_CONNECT END-EXEC.

           IF ( GT2-CURSOR-STAT-GOOD )
              EXEC SQL
                FETCH GT2_TOTAL_CURSOR 
                  INTO 
                    :QWS-TOTAL-SKIPPED 
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-GT2-FILE-NEXT.

           IF ( IO-FG = 0 )
              ADD QWS-TOTAL-SKIPPED TO TOT-SKIPPED.

           EXEC SQL SET CONNECTION C1 END-EXEC.

      *-----------------------------------------------------------------
       CLOSE-TOTAL-GT2-FILE.

           PERFORM CLOSE-IT.

           IF ( GT2-CURSOR-STAT-GOOD )
              EXEC SQL SET CONNECTION GT2_TOTAL_CONNECT END-EXEC
              EXEC SQL
                   CLOSE GT2_TOTAL_CURSOR
              END-EXEC
              EXEC SQL SET CONNECTION C1 END-EXEC
              MOVE STAT---BAD TO GT2-CURSOR-STAT.

      *-----------------------------------------------------------------
       CLOSE-GT2-CONNECTION.

           IF GT2-CONN-OPEN-SW = "Y"
              EXEC SQL SET CONNECTION GT2_CONNECT END-EXEC
              EXEC SQL COMMIT END-EXEC
              EXEC SQL DISCONNECT GT2_CONNECT END-EXEC
              EXEC SQL SET CONNECTION C1 END-EXEC
              MOVE "N" TO GT2-CONN-OPEN-SW.
  
           IF GT2-TOTAL-OPEN-SW = "Y"
              EXEC SQL SET CONNECTION GT2_TOTAL_CONNECT END-EXEC
              EXEC SQL COMMIT END-EXEC
              EXEC SQL DISCONNECT GT2_TOTAL_CONNECT END-EXEC
              EXEC SQL SET CONNECTION C1 END-EXEC
              MOVE "N" TO GT2-TOTAL-OPEN-SW.
			  
      *-----------------------------------------------------------------


       OPEN-WK-FILE-OUTPUT.
           PERFORM OPEN-IT.
           MOVE WK-IFN TO E-FILE.
           OPEN OUTPUT WK-FILE.
           IF IO-FG = 8
              GO TO OPEN-WK-FILE-OUTPUT.
           IF IO-FG = 7
              CLOSE WK-FILE
              GO TO OPEN-WK-FILE-OUTPUT.
       WRITE-WK-FILE.
           PERFORM WRITE-IT.
           MOVE WK-IFN TO E-FILE.
           WRITE WK-REC.
           IF IO-FG = 8
              GO TO WRITE-WK-FILE.
       CLOSE-WK-FILE.
           PERFORM CLOSE-IT.
           CLOSE WK-FILE.
       COPY "LIBGB/FERRORS.CPY".

      ******************************************
       END-PROGRAM SECTION.
      ******************************************
       END-RUN.
           PERFORM CLOSE-FILES.
           IF EXT-ROUTE-BUF = "00"
              PERFORM PRU-OUT.
           IF NO-RECORDS-FOUND
              MOVE "NO RECORDS FOUND FOR SELECTED RANGE" TO MESS
              PERFORM SEND-MESS.
       EXIT-PROG.

           IF SV-BEG-BR NOT = 9999
              IF EXT-EOBUF-GROUP-FG = "Y"
                 MOVE SV-BEG-BR TO BEG-BR
                 MOVE SV-END-BR TO END-BR
                 MOVE SV-GROUP-FLAG TO GROUP-FLAG
                 MOVE SV-ENT-GROUP TO ENT-GROUP
              ELSE
                 MOVE SV-BEG-BR TO BEG-BR
                 MOVE SV-END-BR TO END-BR.

           IF EXT-ROUTE-BUF = "00"
              MOVE "GL/GLEXMU" TO FORM-NAM
              MOVE FORM-PATH TO FORM-PATHNAME.
       STOP-RUN.
           IF EXT-ROUTE-BUF = "00" OR "40" OR "50"
              DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN
              DESTROY WDAYGL-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.
           EXIT PROGRAM.
