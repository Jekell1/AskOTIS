       IDENTIFICATION DIVISION.
       PROGRAM-ID. WDAYBK.
      ********************************************************************
      *                   (GL)
      *       WORKDAY INTEGRATION GT BANK DEPOSIT EXTRACTION
      *       WORLD PR# 1353, CREATES TAB-DELIMITED FILE IN
      *                       /USR/DATA/R1/FTP/WORKDAY/WDAYBK_YYMMDD.TXT
      *                       NET DEPOSIT EXCEPTIONS WRITTEN TO
      *                       /USR/DATA/R1/FTP/WORKDAY/WDAYNET_YYMMDD.CSV
      *       NOTE, THIS STARTED OUT BEING A NICE STRAIGHT-FORWARD 
      *       EXTRACTION, BUT THEN THE PR WAS CHANGED TO SORT BY GT-REF.
      *       SINCE THEY ADDED A REQUIREMENT TO SEQUENTIALLY NUMBER EACH
      *       REF WITH A HEADER KEY, THEN EACH RECORD WITHIN THAT GROUP
      *       WITH A LINE KEY, AND ACCUMULATE THE NET DEPOSIT AND SAVE
      *       THE TOTAL IN LINE KEY 1, THE PROGRAM HAS TO DOUBLE BACK & 
      *       RE-READ THE WORK FILE (WK2FILE) TO NUMBER THE RECORDS. THIS 
      *       IS CONFUSING, BUT THEN THEY NEEDED TO SKIP THE REF GROUPS
      *       THAT NET TO ZERO, SO EVEN MORE CONFUSING.  PUT SOME MORE
      *       NOTES IN THE ACCUMULATE-NET-DEPOSIT ROUTINE.  BLM
      *
      * REV:
      * 181209 BLM NEW, WORLD PR# 1353
      * 190123 BLM MAJOR REVISIONS, 4 FIELDS ADDED TO EXTRACTION: 
      *                 1) LEDGER ACCT (FULL 7 DIGIT GL ACCT)
      *                 2) HEADER KEY, FILE NOW ORDERED BY GT-REF, THIS KEY 
      *                      INCREMENTS EVERY TIME REFERENCE CHANGES
      *                 3) LINE KEY, NUMBERS MULTIPLE LINES WITH SAME REFERENCE
      *                 4) TRANSACTION AMT: NET DEPOSIT, TOTAL OF AMTS WITH
      *                      SAME REF, FIELD ONLY ON LINE KEY1
      *            CHANGED PROGRAM TO SORT GT BY REFERENCE, AND ADD ROUTINES
      *            TO DETERMINE HEADER KEYS AND LINE KEYS.
      *            REVERSE SIGNS ON AMOUNTS & PRINT WITH LEADING SIGN.
      *            CHANGE TO GET WORKDAY BANK ACCT FROM BRFILE, AND USE
      *            BR# IN REF FOR COST CENTER & RETRIEVING WORKDAY BANK 
      *            ACCT, NOT GT-BRANCH. INCREASE OUTPUT REC TO 400 BYTES.
      *            WORLD PR# 1353A
      * 190128 BLM ZERO FILL BRANCH (COST CENTER), IF NET-DEPOSIT FOR REF
      *            = 0, EXCLUDE RECORDS, UPDATE GT & PRINT AS EXCEPTIONS.
      *            WORLD PR# 1353A
      * 190205 BLM REMOVED COLUMN FOR COMPANY ID, THIS WAS THE FIELD
      *            OUTPUT-STATE, FORMAT WAS WAC-GA, WAC-TX, ETC.  REMOVED
      *            FROM HEADER & RECORD.  WORLD PR# 1353A
      * 190219 BLM WRITE NET DEPOSIT RECORDS TO AN EXCEPTION FILE, 
      *            ../FTP/WORKDAY/WDAYNET_YYMMDD.CSV, IN THE FORMAT OF
      *            WDAYGL, WORLD PR# 1353C
      * 231109 RMZ PERFORMANCE TUNING - ADDED STORED PROCEDURE
      * 240116 RMZ CHANGED JULIAN-BEG(END,CC) TO EXT-JULIAN-BEG(END,CC)
      * 240806 RMZ ADDED LTOUCH-DATE TO UPDATE SCRIPTS. (S35Q-108)     
      * 2025.0102 BAH USING VIRTUAL BRANCHES AND THOSE HAVE "SC" IN THE
      *               BR-STATE, NEED REAL STATE HERE FOR ACCOUNTING SO THE
      *               COLLECTION CENTER STATE IS REAL BRANCH #1696 S35Q-155
      * 0225.0303 RMZ CODE "ADD 1 TO TOTAL-INVALID-ZERO" WAS MISSING FROM
      *               EXCEPTION-PRINT PARAGRAPH. ADDED IT TO PROGRAM. 
      *               (S35Q-167)
      *
      ********************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "LIBGB/GBFSPR.CPY".
       COPY "LIBGB/GBFSWK2I.CPY".
           SELECT WK-FILE ASSIGN OUTPUT WK-PATH
                          ORGANIZATION LINE SEQUENTIAL
                          LOCK MODE AUTOMATIC WITH LOCK ON RECORD
                          FILE STATUS FILE-STAT.
 
      *-----------------------------------------------------------------
           SELECT EXCEPT-FILE ASSIGN MKTEMP-BUF
                  ORGANIZATION INDEXED
                  ACCESS MODE DYNAMIC
                  LOCK MODE AUTOMATIC WITH LOCK ON RECORD
                  RECORD KEY EXCEPT-KEY
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

      *****************************************************************
      *    SORTED GTFILE
      *****************************************************************
       FD  WK2-FILE
           LABEL RECORDS ARE STANDARD.
       01  WK2-REC.
           03  WK2-KEY.
               05  WK2-REF          PIC X(14).
               05  FILLER REDEFINES WK2-REF.
                   07  WK2-LP       PIC X(3).
                   07  WK2-YYMMDD   PIC 9(6).
                   07  WK2-DASH     PIC X.
                   07  WK2-BRNO     PIC 9(4).
               05  WK2-GT1-KEY      PIC X(36).
           03  WK2-AMT              PIC S9(9)V99 COMP-3.
           03  WK2-NET-DEPOSIT      PIC S9(9)V99 COMP-3.
           03  WK2-LINE-KEY         PIC 99.
      *****************************************************************
      *    EXCEPTION WORK FILE.  TYPES:
      *    B = BANK (ACCT = 1022)
      *    X = INVALID GL
      *    Y = BRANCH
      *    Y = BRANCH
      *****************************************************************
       FD  EXCEPT-FILE
           LABEL RECORDS ARE STANDARD.
       01  EXCEPT-REC.
           03  EXCEPT-KEY.
               05 EXCEPT-TYPE           PIC X.
               05 EXCEPT-REF            PIC X(14).
               05 EXCEPT-GT1-KEY        PIC X(36).

       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)GL/WDAYBK.CBL S35 12/22/23-11:10:15 >".

       COPY "LIBGL/GL01GT.CPY".
       COPY "LIBGL/GL01GT_SQL.CPY".
       COPY "LIBGL/GLWSGT.CPY".
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
               05  FILLER      PIC X(12) VALUE "FTP/WORKDAY/".
           03  WK-PATH-FILE.
               05  WK-PATH-NAM PIC X(7) VALUE "WDAYBK_".
               05  WK-PATH-YMD PIC 9(6).
               05  WK-PATH-CSZ PIC X(4) VALUE ".TXT".
           03  FILLER          PIC X.

       01  WK2PATH             PIC X(35).

       01  WK-NET-PATH.
           03  WK-NET-PATH-DIR.
               05  WK-NET-PATH-FIL PIC X(13).
               05  FILLER          PIC X(12) VALUE "FTP/WORKDAY/".
           03  WK-NET-PATH-FILE.
               05  FILLER          PIC X(8) VALUE "WDAYNET_".
               05  WK-NET-PATH-YMD PIC 9(6).
               05  FILLER          PIC X(4) VALUE ".CSV".

       01  PR-IFN                      PIC X(3)      VALUE "PR".
       01  WK2-IFN                     PIC X(8)      VALUE "SORTFILE".
       01  WK-IFN                      PIC X(3)      VALUE "WK".

       01  LN-WORKERS.
           03  LN-FEED        PIC 999   VALUE 0.
           03  LN-COUNT       PIC 999   VALUE 99.
           03  LN-WK          PIC 999   VALUE 0.
       01  PAGE-NUMBER        PIC 999   VALUE 0.
 
       01  OUTPUT-FILE-WORKERS.
           03  OUTPUT-DATE          PIC 99/99/9999.
           03  OUTPUT-AMOUNT        PIC ---------9.99.
           03  OUTPUT-BANK-ACCT.
               05  FILLER           PIC X(3) VALUE "BA-".
               05  BANKACCT         PIC 9(4).
           03  OUTPUT-BRANCH        PIC ZZZ9.
           03  OUTPUT-ACCTNO        PIC 9(7).
           03  OUTPUT-HEADER-KEY    PIC ZZZZZZ9.
           03  OUTPUT-LINE-KEY      PIC Z9.
           03  OUTPUT-NET-DEPOSIT   PIC ---------9.99.
      *  NEED THESE FOR NET ZERO EXCEPTION FILE:
           03  OUTPUT-LINE-ORDER    PIC 9(8) VALUE 0.
           03  OUTPUT-STATE.
               05  FILLER           PIC X(3) VALUE "WAC".
               05  OUTPUT-DASH      PIC X.
               05  GTSTATE          PIC XX.
           03  OUTPUT-GTDATE.
               05  FILLER           PIC X VALUE """".
               05  GTDATE-YYYYMMDD  PIC 9999/99/99.
               05  FILLER           PIC X VALUE """".
           03  OUTPUT-DEBIT         PIC 9(9).99 BLANK ZERO.
           03  OUTPUT-CREDIT        PIC 9(9).99 BLANK ZERO.
           03  OUTPUT-EXPL          PIC X(32).

       01  SAVE-WK2-KEY       PIC X(36).
       01  HOLD-REF           PIC X(14).
       01  HEADER-KEY         PIC 9(7)  VALUE 0.
       01  LINE-KEY           PIC 9(2)  VALUE 0.

       01  NET-DEPOSIT        PIC S9(10)V99 COMP-3 VALUE +0.
       01  TOTAL-DEBITS       PIC S9(10)V99 COMP-3 VALUE +0.
       01  TOTAL-CREDITS      PIC S9(10)V99 COMP-3 VALUE +0.
       01  TOTAL-DRCR         PIC S9(10)V99 COMP-3 VALUE +0.
       01  TOTAL-EXTRACTED    PIC S9(10)    COMP-3 VALUE +0.
       01  TOTAL-SKIPPED      PIC S9(10)    COMP-3 VALUE +0.
       01  TOTAL-BANK         PIC S9(10)    COMP-3 VALUE +0.
       01  TOTAL-BANK-DR      PIC S9(10)V99 COMP-3 VALUE +0.
       01  TOTAL-BANK-CR      PIC S9(10)V99 COMP-3 VALUE +0.
       01  TOTAL-INVALID-BR   PIC S9(10)    COMP-3 VALUE +0.
       01  TOTAL-INVALID-ZERO PIC S9(10)    COMP-3 VALUE +0.
       01  EXCEPTION-FG       PIC X.

       01  FIRST-TIME         PIC X VALUE "Y".
       01  RECORDS-FOUND      PIC X VALUE " ".
           88  NO-RECORDS-FOUND     VALUE "N".

       01  MESS-LIST          PIC X(5) VALUE "MESS.".

       COPY "LIBGB/EOBUF_EXT.CPY".
           03  SOURCE-CODE            PIC 9.
           03  ALL-SRC-CODES  REDEFINES SOURCE-CODE PIC X.
           03  BEG-DATE               PIC 9(8).
           03  END-DATE               PIC 9(8).
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
           03  FILLER         PIC X(22)   VALUE SPACES.
           03  H-TITLE        PIC X(44)   VALUE
               "WORKDAY INTEGRATION BANK TRANSACTION EXTRACT".

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
           03  TOT-DESC           PIC X(18).
           03  TOT-NO             PIC ZZ,ZZZ,ZZZ,ZZ9- BLANK ZERO.
           03  TOTS OCCURS 3.
               05  TOT-TYPE       PIC X(16).
               05  TOT-AMT        PIC ZZZ,ZZZ,ZZ9.99-.


       COPY "LIBGL/WDAYBK_CMD.CPY".
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
       COPY "LIBGB/MKTEMPW_CMD.CPY".
       COPY "LIBGB/GETPRW.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBGL/WDAYBK_DEF.CPY".
       COPY "LIBGL/WDAYBK_WKS.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBGL/WDAYBK_SCN.CPY".

       PROCEDURE DIVISION
            USING FORM-PATHNAME EXIT-PATHNAME.
       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
              EXCEPT-FILE WK-FILE PR-FILE WK2-FILE.
       COPY "LIBGB/DECLRP.CPY".
           PERFORM CLOSE-GT2-FILE.
           PERFORM CLOSE-GT2-CONNECTION.
           PERFORM CLOSE-GT1-FILE.
           PERFORM CLOSE-GR1-FILE.
           PERFORM CLOSE-BR-FILE.
           PERFORM SQL-CLOSE-UPDATE-CONNECTION.
           PERFORM CLOSE-PR-FILE.
           CLOSE
               EXCEPT-FILE WK-FILE WK2-FILE.

       COPY "LIBGB/DECLRP2.CPY".

      **********************************************
       MAIN-PROGRAM SECTION.
      **********************************************

           PERFORM SQL-CONNECT.

      D    STOP MESS.
           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "WDAYBK" TO VDU-FORM-NAME.

           PERFORM INITIALIZATION.
           IF EXT-EOBUF-ERRCD NOT = " "
              GO TO END-PROGRAM.

           IF GROUP-FLAG = "Y"
              PERFORM OPEN-GR1-FILE
              MOVE ENT-GROUP TO GR-ID
              PERFORM SEARCH-GR-INIT.

           MOVE BEG-DATE TO GT-DATE1 SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
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
              GO TO PROCESS-SORT-RECORDS.

      * TEST DATE; GT-DATE1 STORED AS CCYYMMDD
           IF GT-DATE1 > END-DATE
              GO TO PROCESS-SORT-RECORDS.

      * TEST G/L SOURCE CODE OPTION
           IF ALL-SRC-CODES NOT = "*"
              IF GT-SOURCE NOT = SOURCE-CODE
                 ADD 1 TO TOTAL-SKIPPED
                 GO TO NEXT-GT-RECORD.

      * TEST PREVIOUSLY EXTRACTED OPTION
           IF PREV-EXTRACTED = "S"
              IF (GT-SMITH-SENT = "X" OR "E")
                 ADD 1 TO TOTAL-SKIPPED
                 GO TO NEXT-GT-RECORD.
           IF PREV-EXTRACTED = "O"
              IF NOT (GT-SMITH-SENT = "X" OR "E")
                 ADD 1 TO TOTAL-SKIPPED
                 GO TO NEXT-GT-RECORD.

      * TEST G/L BRANCH / GROUP
           IF GROUP-FLAG = "Y"
              MOVE GT-BRANCH TO WGR-SRESULT
              PERFORM SEARCH-GR-VERIFY
              IF WGR-ERR > 0
                 ADD 1 TO TOTAL-SKIPPED
                 GO TO NEXT-GT-RECORD.
           IF GROUP-FLAG NOT = "Y"
              IF GT-BRANCH < BEG-BR OR GT-BRANCH > END-BR
                 ADD 1 TO TOTAL-SKIPPED
                 GO TO NEXT-GT-RECORD.

      * SKIP NON-BANK TRANSACTIONS
           IF GT-EXPL NOT = "EOD-DEPOSIT"
              GO TO NEXT-GT-RECORD.
           IF (GT-ACCTNO = 1022000 AND GT-EXPL = "EOD-DEPOSIT")
              MOVE "B" TO EXCEPTION-FG
              PERFORM CREATE-EXCEPTION
              PERFORM UPDATE-GT-EXT-FLAG
              GO TO NEXT-GT-RECORD.

       CONTINUE-PROCESSING.
           MOVE "N" TO FIRST-TIME.
           PERFORM CREATE-SORT-RECORD.

           GO TO NEXT-GT-RECORD.

       PROCESS-SORT-RECORDS.
           PERFORM CLOSE-GT2-FILE.
           PERFORM OPEN-GT1-FILE.

           MOVE SPACES TO WK2-KEY.
           PERFORM START-WK2-FILE.

           MOVE SPACES TO HOLD-REF.
           MOVE 0      TO HEADER-KEY.

       READ-NEXT-SORT-REC.
           PERFORM READ-WK2-FILE-NEXT.
           IF IO-BAD
              GO TO END-PROCESS.

           IF WK2-REF NOT = HOLD-REF
              IF WK2-LINE-KEY = 1
                 PERFORM ACCUMULATE-NET-DEPOSIT
                 IF NET-DEPOSIT = 0
                    GO TO READ-NEXT-SORT-REC.
           IF WK2-LINE-KEY = 1
              ADD 1 TO HEADER-KEY.

           PERFORM CREATE-WORK-RECORD.
           PERFORM UPDATE-GT-EXT-FLAG.

           GO TO READ-NEXT-SORT-REC.

       END-PROCESS.
      *    PERFORM CLOSE-WK-FILE.
           PERFORM PROCESS-EXCEPTIONS.

       END-ROUTINE.
           IF FIRST-TIME = "Y"
              MOVE "N" TO RECORDS-FOUND
              GO TO END-PROGRAM.

           PERFORM PRINT-GRAND-TOTALS.

           PERFORM PRINT-RANGE-LINES.

           GO TO END-PROGRAM.

      ***********************************************
       CREATE-SORT-RECORD SECTION.
      ***********************************************
            MOVE GT-REF    TO WK2-REF.
            MOVE GT1-KEY   TO WK2-GT1-KEY.
            COMPUTE WK2-AMT =
               GT-AMT * -1.
            MOVE 0         TO WK2-NET-DEPOSIT.
            MOVE 1         TO WK2-LINE-KEY.
            PERFORM WRITE-WK2-FILE.
            IF IO-FG = 9
               GO TO IO-ERROR.

      *---------------------------------------------------
      * ACCUMULATE NET DEPOSIT
      * THIS ROUTINE HAS THE FIRST WK2 FOR REF LOADED,
      * WILL READ THE REST FOR THAT REF & INCREMENT
      * WK2-LINE-KEY AND ACCUMULATE NET-DEPOSIT WHICH IS
      * ONLY STORED & EXTRACTED FOR LINE KEY 1.  THEN
      * LINE 1 IS RE-READ & UPDATED WITH NET-DEPOSIT.
      * AFTER EXITING THIS ROUTINE, THE FIRST LINE GETS
      * PROCESSED, THEN THE OTHER LINES FOR THIS
      * REF WILL BE READ & WILL SKIP THIS ROUTINE SINCE
      * WK2-LINE-KEY WILL BE > 1.
      * 1/19/18: TO FURTHER COMPLICATE MATTERS, NEED TO
      * SKIP RECORDS THAT NET TO ZERO.  IF THIS HAPPENS,
      * EXCEPTION RECS ARE WRITTEN, THE GT'S ARE UPDATED,
      * AND THE WK2 RECORDS ARE DELETED SO THEY WON'T BE
      * IN THE OUTPUT FILE.  
      *---------------------------------------------------
      ***********************************************
       ACCUMULATE-NET-DEPOSIT SECTION.
      ***********************************************
           MOVE WK2-AMT TO NET-DEPOSIT.
           MOVE WK2-KEY TO SAVE-WK2-KEY.
           MOVE WK2-REF TO HOLD-REF.
           MOVE 1       TO LINE-KEY.
       READ-NEXT-DEPOSIT.
           PERFORM READ-WK2-FILE-NEXT.
           IF IO-BAD
              GO TO RESTART-WK2-FILE.

           IF WK2-REF = HOLD-REF
              ADD WK2-AMT TO NET-DEPOSIT
              ADD 1 TO LINE-KEY
              MOVE LINE-KEY TO WK2-LINE-KEY
              PERFORM REWRITE-WK2-FILE
              GO TO READ-NEXT-DEPOSIT.
       RESTART-WK2-FILE.
           IF NET-DEPOSIT = 0
              PERFORM ZERO-DEPOSIT-EXCEPTION
              GO TO EXIT-ACCUMULATE-NET-DEPOSIT.
           MOVE SAVE-WK2-KEY TO WK2-KEY.
           PERFORM START-WK2-FILE.
           PERFORM READ-WK2-FILE-NEXT.
           MOVE 1 TO WK2-LINE-KEY.
           MOVE NET-DEPOSIT TO WK2-NET-DEPOSIT.
           PERFORM REWRITE-WK2-FILE.
       EXIT-ACCUMULATE-NET-DEPOSIT.
           EXIT.
      *---------------------------------------------------
      * IF NET DEPOSIT IS ZERO, CREATE EXCEPTION LINES
      * & DON'T INCLUDE RECORS IN OUTPUT FILE
      * NEED STATE FROM BR-FILE, SO IF BRANCH IS INVALID
      * WRITE INVALID BRANCH EXCEPTION INSTEAD OF NET 
      * EXCEPTION
      *---------------------------------------------------
      ****************************************************
       ZERO-DEPOSIT-EXCEPTION SECTION.
      ****************************************************
           MOVE SAVE-WK2-KEY TO WK2-KEY.
           PERFORM START-WK2-FILE.
       NEXT-ZERO-DEPOSIT.
           PERFORM READ-WK2-FILE-NEXT.
           IF IO-BAD
              GO TO EXIT-ZERO-DEPOSIT.

           IF WK2-REF NOT = HOLD-REF
              GO TO EXIT-ZERO-DEPOSIT.

           MOVE WK2-GT1-KEY TO GT1-KEY.
           PERFORM READ-GT1-FILE.
           IF IO-BAD
              GO TO IO-ERROR.
           MOVE GT-BRANCH TO BR-NO.
           PERFORM READ-BR-FILE.
           IF IO-BAD
              MOVE "Y" TO EXCEPTION-FG
           ELSE
              MOVE "Z" TO EXCEPTION-FG.

           PERFORM CREATE-EXCEPTION.
           PERFORM UPDATE-GT-EXT-FLAG.
           PERFORM DELETE-WK2-FILE.

           GO TO NEXT-ZERO-DEPOSIT.
       EXIT-ZERO-DEPOSIT.
           MOVE SAVE-WK2-KEY TO WK2-KEY.
           PERFORM START-WK2-FILE.

      *************************************************
       CREATE-WORK-RECORD SECTION.
      *************************************************
           MOVE WK2-GT1-KEY TO GT1-KEY.
           PERFORM READ-GT1-FILE. *> EMBEDDED - GTFILE
           IF IO-BAD
              GO TO IO-ERROR.

           MOVE SPACES TO WK-REC EXCEPTION-FG.
           MOVE GT-DATE1 TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYYYY.
           MOVE DATE-MMDDYYYY TO OUTPUT-DATE.

           MOVE WK2-AMT TO OUTPUT-AMOUNT.

           MOVE WK2-BRNO TO BR-NO.
           PERFORM READ-BR-FILE.
           IF IO-BAD
              MOVE "Y" TO EXCEPTION-FG
              PERFORM CREATE-EXCEPTION
              GO TO EXIT-CREATE-WORK-REC.
           MOVE BR-WKDAY-BANKID TO BANKACCT.

           MOVE WK2-BRNO TO OUTPUT-BRANCH.
           MOVE GT-ACCTNO       TO OUTPUT-ACCTNO.
           MOVE HEADER-KEY      TO OUTPUT-HEADER-KEY.
           MOVE WK2-LINE-KEY    TO OUTPUT-LINE-KEY.
           MOVE WK2-NET-DEPOSIT TO OUTPUT-NET-DEPOSIT.

           STRING
               OUTPUT-DATE, X'09',
               OUTPUT-AMOUNT, X'09',
               OUTPUT-BANK-ACCT, X'09',
               OUTPUT-BRANCH, X'09',
               OUTPUT-ACCTNO, X'09',
               OUTPUT-HEADER-KEY, X'09',
               OUTPUT-LINE-KEY, X'09',
               OUTPUT-NET-DEPOSIT
               WS-CR
               DELIMITED BY SIZE INTO WK-REC.
           PERFORM WRITE-WK-FILE.
           IF IO-BAD
              GO TO IO-ERROR.

           ADD 1 TO TOTAL-EXTRACTED.
           IF GT-DRCRFG = 1
              ADD GT-AMT  TO TOTAL-DEBITS
           ELSE
              ADD GT-AMT  TO TOTAL-CREDITS.
       EXIT-CREATE-WORK-REC.
           EXIT.

      *************************************************
       CREATE-EXCEPTION SECTION.
      *************************************************
           MOVE EXCEPTION-FG  TO EXCEPT-TYPE.
           MOVE GT-REF        TO EXCEPT-REF.
           MOVE GT1-KEY       TO EXCEPT-GT1-KEY.
           PERFORM WRITE-EXCEPT-FILE.
           IF IO-BAD
              GO TO IO-ERROR.
       FINISH-CREATE-EXCEPTION-REC.
           EXIT.

      ****************************************************
       PROCESS-EXCEPTIONS SECTION.
      ****************************************************
           MOVE "Z" TO EXCEPTION-FG.
           PERFORM PROCESS-EXCEPTION-TYPE.
           MOVE "B" TO EXCEPTION-FG.
           PERFORM PROCESS-EXCEPTION-TYPE.
           MOVE "Y" TO EXCEPTION-FG.
           PERFORM PROCESS-EXCEPTION-TYPE.

      ****************************************************
       PROCESS-EXCEPTION-TYPE SECTION.
      ****************************************************
           MOVE EXCEPTION-FG TO EXCEPT-TYPE.
           MOVE SPACES       TO EXCEPT-REF.
           MOVE SPACES       TO EXCEPT-GT1-KEY.
           PERFORM START-EXCEPT-FILE.
       NEXT-EXCEPTION.
           PERFORM READ-EXCEPT-FILE-NEXT.
           IF IO-BAD
              GO TO EXIT-PROCESS-EXCEPTION-TYPE.
           IF EXCEPT-TYPE NOT = EXCEPTION-FG
              GO TO EXIT-PROCESS-EXCEPTION-TYPE.
           MOVE EXCEPT-GT1-KEY TO GT1-KEY.
           PERFORM READ-GT1-FILE. *> EMBEDDED - GTFILE
           IF IO-BAD
              GO TO IO-ERROR.
           PERFORM EXCEPTION-PRINT.
           GO TO NEXT-EXCEPTION.
       EXIT-PROCESS-EXCEPTION-TYPE.
           EXIT.

      *************************************************
       EXCEPTION-PRINT SECTION.
      *************************************************
           MOVE GT-REF             TO D-REFERENCE.
           MOVE GT-BRANCH          TO D-BRANCH.
           MOVE "-"                TO D-DASH.
           MOVE GT-ACCTNO          TO D-ACCTNO.
           MOVE GT-SOURCE          TO D-SOURCE.
           MOVE GT-FLAG            TO D-UPDATE.
           MOVE GT-PERIOD          TO D-PERIOD.
           IF EXCEPTION-FG = "B"
              MOVE "1022 EXCEPTION" TO D-DESC
              ADD 1 TO TOTAL-BANK
              IF GT-DRCRFG = 1
                 ADD GT-AMT  TO TOTAL-BANK-DR
              ELSE
                 ADD GT-AMT  TO TOTAL-BANK-CR.
           IF EXCEPTION-FG = "Z"
              MOVE "NET DEPOSIT 0" TO D-DESC
              ADD 1 TO TOTAL-INVALID-ZERO
              PERFORM CREATE-NET-ZERO-EXCEPTION-REC.
           IF EXCEPTION-FG = "Y"
              MOVE "INVALID BR &&&&" TO D-DESC
              INSPECT D-DESC REPLACING FIRST "&&&&" BY D-BRANCH
              ADD 1 TO TOTAL-INVALID-BR.

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

      ************************************************
       CREATE-NET-ZERO-EXCEPTION-REC SECTION.
      ************************************************
           IF TOTAL-INVALID-ZERO = 1
              MOVE WK-NET-PATH TO WK-PATH
              PERFORM OPEN-WK-FILE-OUTPUT
              PERFORM CREATE-NET-HEADER-RECORD.

           MOVE SPACES TO WK-REC.
           ADD 1 TO OUTPUT-LINE-ORDER.
           MOVE GT-DATE1 TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO NDTE-DATE.
           MOVE 31 TO NDTE-DD.
           PERFORM NEW-DATE-CALCULATION.
           MOVE NDTE-DATE TO DATE-MMDDYY.
           PERFORM CONVERT-MMDDYY-TO-YYYYMMDD.
           MOVE DATE-YYYYMMDD TO GTDATE-YYYYMMDD.
           INSPECT GTDATE-YYYYMMDD REPLACING ALL "/" BY "-".
           MOVE GT-BRANCH TO BR-NO.
           PERFORM READ-BR-FILE.
           IF IO-BAD
              MOVE "XX" TO BR-STATE.
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
               S-1, ",",
               "WAC", ",",
               "USD", ",",
               "ACTUALS", ",",
               OUTPUT-GTDATE, ",",
               OUTPUT-EXPL, ",",
      * ACCOUNT_SET_ID
               S-2, ",",
               "STANDARD", ",",
      * LEDGER_ACCOUNT_ID
               S-3, ",",
               OUTPUT-ACCTNO, ",",
               OUTPUT-DEBIT, ",",
               OUTPUT-CREDIT, ",",
               "USD", ",",
               GT-REF, ",",
      * COMPANY_REFERENCE_ID
               S-1, ",",
               OUTPUT-STATE, ","
               ",",
               ",",
               ",",
               OUTPUT-BRANCH
               WS-CR
               DELIMITED BY "  " INTO WK-REC.
           PERFORM WRITE-WK-FILE.
           IF IO-BAD
              GO TO IO-ERROR.

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
 
           MOVE HLINE-3 TO PR-REC.
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-PR-REC.

           MOVE SPACES TO PR-REC.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-PR-REC.

           MOVE 5 TO LN-COUNT
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
           PERFORM HEAD-PAGE.

           IF TOTAL-SKIPPED > 0
              MOVE "TOTAL SKIPPED    :" TO TOT-DESC
              MOVE TOTAL-SKIPPED        TO TOT-NO
              MOVE TOTAL-LINE TO PR-REC
              MOVE 2 TO LN-FEED
              PERFORM WRITE-PR-REC.

           IF TOTAL-INVALID-ZERO > 0
              MOVE "TOTAL ZERO DEP   :"  TO TOT-DESC
              MOVE TOTAL-INVALID-ZERO    TO TOT-NO
              MOVE TOTAL-LINE TO PR-REC
              MOVE 2 TO LN-FEED
              PERFORM WRITE-PR-REC.

           IF TOTAL-INVALID-BR > 0
              MOVE "TOTAL INVALID BR :"  TO TOT-DESC
              MOVE TOTAL-INVALID-BR      TO TOT-NO
              MOVE TOTAL-LINE TO PR-REC
              MOVE 2 TO LN-FEED
              PERFORM WRITE-PR-REC.

           MOVE "EXTRACTED        :" TO TOT-DESC.
           MOVE TOTAL-EXTRACTED      TO TOT-NO.
           MOVE "TOTAL DEBITS:"      TO TOT-TYPE(1).
           MOVE TOTAL-DEBITS         TO TOT-AMT(1).
           MOVE "TOTAL CREDITS:"     TO TOT-TYPE(2).
           MOVE TOTAL-CREDITS        TO TOT-AMT(2).
           ADD TOTAL-DEBITS TOTAL-CREDITS GIVING TOTAL-DRCR.
           MOVE "TOTAL :"            TO TOT-TYPE(3).
           MOVE TOTAL-DRCR           TO TOT-AMT(3).
           MOVE TOTAL-LINE TO PR-REC.
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-PR-REC.

           MOVE "BANK (1022)      :" TO TOT-DESC.
           MOVE TOTAL-BANK           TO TOT-NO.
           MOVE "TOTAL DEBITS:"      TO TOT-TYPE(1).
           MOVE TOTAL-BANK-DR        TO TOT-AMT(1).
           MOVE "TOTAL CREDITS:"     TO TOT-TYPE(2).
           MOVE TOTAL-BANK-CR        TO TOT-AMT(2).
           ADD TOTAL-BANK-DR TOTAL-BANK-CR GIVING TOTAL-DRCR.
           MOVE "TOTAL :"            TO TOT-TYPE(3).
           MOVE TOTAL-DRCR           TO TOT-AMT(3).
           MOVE TOTAL-LINE TO PR-REC.
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-PR-REC.

      ****************************************
       INITIALIZATION SECTION.
      ****************************************
           MOVE WDAYBK-QUEUE-POS TO Q-POS.
           MOVE WDAYBK-COPIES-POS TO C-POS.
           MOVE WDAYBK-PRU-POS   TO P-POS.
           MOVE SPACES TO Q-BUF C-BUF P-BUF.
       COPY "LIBGB/EOINIT.CPY".
           IF BEG-DATE = 0
              MOVE 19000101 TO BEG-DATE.
           IF END-DATE = 0
              MOVE 19000101 TO END-DATE.
           IF END-DATE = 99999999
              MOVE 99991231 TO END-DATE.

           IF EXT-ROUTE-BUF = "00"
              MOVE "WORKDAY GL TRANS BANK EXTRACT IN PROGRESS" TO MESS
              MOVE MESS-LIST TO WR-LIST
              MOVE MESS TO WR-BUF
              PERFORM CALL-WRFORM.

           MOVE EXT-CONAME-CONAME  TO HD-CONAME.
           MOVE EXT-CONAME-SYSDATE TO HD-DATE.
           MOVE EXT_CONAME-USER-ID TO HD-USERID.

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

           MOVE EXT-FILPATH-GLOBAL TO WK-PATH-FIL WK-NET-PATH-FIL.
           MOVE END-DATE        TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-YYMMDD.
           MOVE DATE-YYMMDD     TO WK-PATH-YMD WK-NET-PATH-YMD.
           CALL "C$TOLOWER" USING WK-PATH, VALUE 42.
           CALL "C$TOLOWER" USING WK-NET-PATH, VALUE 43.
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
           MOVE WK-NET-PATH TO ACCESS-BUF.
           PERFORM ACCESS-CALL.
           IF STAT-GOOD
              MOVE "EXCEPTION FILE &&&&&&&&&&&&&&&&&& EXISTS, ABORTED"
                 TO MESS
              INSPECT MESS REPLACING FIRST "&&&&&&&&&&&&&&&&&&" BY
                                            WK-NET-PATH-FILE
              PERFORM SEND-MESS
              GO TO ENDIT.

           PERFORM SQL-OPEN-UPDATE-CONNECTION.
           PERFORM OPEN-WK-FILE-OUTPUT.
           PERFORM CREATE-HEADER-RECORD.

           MOVE MKTEMP-DEFAULT TO MKTEMP-BUF.
           PERFORM MKTEMP-CALL.
           MOVE MKTEMP-BUF TO WK2PATH.
           PERFORM OPEN-WK2-FILE-OUTPUT.
           PERFORM CLOSE-WK2-FILE.
           PERFORM OPEN-WK2-FILE.

           MOVE MKTEMP-DEFAULT TO MKTEMP-BUF.
           PERFORM MKTEMP-CALL.
           PERFORM OPEN-EXCEPT-FILE-OUTPUT.
           PERFORM CLOSE-EXCEPT-FILE.
           PERFORM OPEN-EXCEPT-FILE.

           PERFORM OPEN-BR-FILE.
           PERFORM OPEN-GT2-FILE.
           PERFORM OPEN-PR-FILE.

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

      *******************************
       ENTER-PARAMETERS SECTION.
      *******************************

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
           IF F3-KEY GO TO ALLDT.
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
           GO TO E-PREV-EXTRACTED.
       ALLDT.
           MOVE "S  8080DATE1." TO VDU.
           MOVE EXT-JULIAN-BEG  TO BEG-DATE VDU-DATE-YYYYMMDD.
           PERFORM SCREENIO.
           MOVE "S  8080DATE2." TO VDU.
           MOVE EXT-JULIAN-END  TO END-DATE VDU-DATE-YYYYMMDD.
           PERFORM SCREENIO.

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

      ************************************************
       CREATE-HEADER-RECORD SECTION.
      ************************************************
           CALL "C$TOLOWER" USING HEADER1, VALUE 4.
           CALL "C$TOLOWER" USING HEADER2, VALUE 6.
           CALL "C$TOLOWER" USING HEADER3, VALUE 12.
           CALL "C$TOLOWER" USING HEADER4, VALUE 11.
           CALL "C$TOLOWER" USING HEADER5, VALUE 11.
           CALL "C$TOLOWER" USING HEADER6, VALUE 10.
           CALL "C$TOLOWER" USING HEADER7, VALUE 8.
           CALL "C$TOLOWER" USING HEADER8, VALUE 15.
           MOVE SPACES TO WK-REC.
           STRING HEADER1, X'09',
                  HEADER2, X'09',
                  HEADER3, X'09',
                  HEADER4, X'09',
                  HEADER5, X'09',
                  HEADER6, X'09',
                  HEADER7, X'09',
                  HEADER8,
                  WS-CR
               DELIMITED BY SIZE INTO WK-REC.
           PERFORM WRITE-WK-FILE.
           IF IO-BAD
              GO TO IO-ERROR.

      ************************************************
       CREATE-NET-HEADER-RECORD SECTION.
      ************************************************
           MOVE SPACES TO WK-REC.
           MOVE WS-NET-HEADER-REC TO WK-REC.
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
      ************************************************
       UPDATE-GT-EXT-FLAG SECTION.
      ************************************************
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
           MOVE EXT-CONAME-CONAME  TO WDAYBK-CONAME.
           MOVE EXT-CONAME-SYSDATE TO WDAYBK-SYSDATE.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DISPLAY WDAYBK-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE WDAYBK-FORM-FIELDS TO WR-BUF.
           MOVE WDAYBK-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      **********************************************************
       SETUP-LINK-INFO SECTION.
      **********************************************************
           MOVE WDAYBK-HELP--FILE TO HELP-LINK-FILE.
           MOVE WDAYBK-HELP--DIR  TO HELP-LINK-DIR.

      **********************************************************
       VDU-CONVERT-FIELD SECTION.
      **********************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBGL/WDAYBK_EVA.CPY".
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
       COPY "LIBGB/MKTEMP.CPY".
       COPY "LIBGB/RMFILE.CPY".
 
      ************************************************
       IO-ROUTINES SECTION.
      ************************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGL/GLGTGS_SQL.CPY".
       COPY "LIBGB/GBGRGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBPRINT.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBGB/GBGR1RN.CPY".

      * COPY "LIBGL/GLGT1RN.CPY".
      * COPYMEMBER: LIBGL/GLGT1R
       LOAD-GT1-FILE.
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
       OPEN-GT1-FILE.
           PERFORM LOAD-GT1-FILE.
           PERFORM OPEN-IT.
           MOVE GT-PATH-SQL TO E-FILE.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       READ-GT1-FILE. *> EMBEDDED - GTFILE
           PERFORM READ-IT.
           MOVE GT-PATH-SQL TO E-FILE.
           MOVE GT1-KEY TO E-KEYX.
           INITIALIZE QGT-REC.

           MOVE GT-SOURCE           TO QGT-SOURCE.
           MOVE GT-DATE2            TO SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO QGT-DATE2.
           MOVE GT-BRANCH           TO QGT-BRANCH.
           MOVE GT-ACCTNO           TO QGT-ACCTNO.
           MOVE GT-DATE3            TO SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO QGT-DATE3.
           MOVE GT-DRCRFG           TO QGT-DRCRFG.
           MOVE GT-LINE             TO QGT-LINE.

           EXEC SQL
            SELECT GTFILE.GT_FLAG, 
                   GTFILE.GT_REF, 
                   CAST(GTFILE.GT_DATE1 AS VARCHAR(10)), 
                   GTFILE.GT_SOURCE,
                   CAST(GTFILE.GT_DATE2 AS VARCHAR(10)), 
                   GTFILE.GT_BRANCH,
                   GTFILE.GT_ACCTNO,
                   CAST(GTFILE.GT_DATE3 AS VARCHAR(10)), 
                   GTFILE.GT_DRCRFG, 
                   GTFILE.GT_LINE,
                   GTFILE.GT_AMT,
                   GTFILE.GT_EXPL,
                   GTFILE.GT_PERIOD,
                   GTFILE.GT_SMITH_SENT
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
              FROM DBO.GTFILE
             WHERE GTFILE.GT_SOURCE = :QGT-SOURCE
               AND GTFILE.GT_DATE2  = :QGT-DATE2
               AND GTFILE.GT_BRANCH = :QGT-BRANCH
               AND GTFILE.GT_ACCTNO = :QGT-ACCTNO
               AND GTFILE.GT_DATE3  = :QGT-DATE3
               AND GTFILE.GT_DRCRFG = :QGT-DRCRFG
               AND GTFILE.GT_LINE   = :QGT-LINE
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-GT1-FILE.

           IF ( IO-FG = 0 )
              PERFORM GET-GT-FIELDS.

      *-----------------------------------------------------------------
       CLOSE-GT1-FILE.
           PERFORM CLOSE-IT.
 

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

      *        MOVE JULIAN-END          TO SQL-DATE-YYYYMMDD
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
                :GT2-RETURN-CODE = EXEC SSP_WDAYBK_GTFILE_SELECT 
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
              GTFILE.GT_SOURCE	    = :QGT-SOURCE,
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
       START-TOTAL-GT2-FILE. *> SSP - GTFILE TOTALS
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
      * SELECT SUM(CASE WHEN (GTFILE.GT_SOURCE           
      *                                       < @QWS_WBEG_SOURCE_CODE
      *                   OR  GTFILE.GT_SOURCE           
      *                                       > @QWS_WEND_SOURCE_CODE)
      *                   OR  (@QWS_PREV_EXTRACTED       = 'S'
      *                  AND   GTFILE.GT_SMITH_SENT     IN ('X', 'E'))
      *                   OR  (@QWS_PREV_EXTRACTED       = 'O'
      *                  AND   GTFILE.GT_SMITH_SENT NOT IN ('X', 'E'))
      *                 THEN 1
      *                 ELSE 0
      *             END) AS QWS_TOTAL_SKIPPED 
      *
      *   FROM DBO.GTFILE GTFILE 
      *  WHERE (GTFILE.GT_DATE1     BETWEEN @QWS_WBEG_DATE 
      *                                 AND @QWS_WEND_DATE);

           EXEC SQL SET CONNECTION GT2_TOTAL_CONNECT END-EXEC.

           EXEC SQL
            DECLARE GT2_TOTAL_CURSOR CURSOR FOR
                :GT2-RETURN-CODE = EXEC SSP_WDAYBK_GTFILE_TOTAL_SELECT 
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
              ADD QWS-TOTAL-SKIPPED TO TOTAL-SKIPPED.

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


       COPY "LIBGB/GBWK2IN.CPY".
   
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
       OPEN-EXCEPT-FILE-OUTPUT.
           PERFORM OPEN-IT.
           MOVE MKTEMP-BUF TO E-FILE.
           OPEN OUTPUT EXCEPT-FILE.
           IF IO-FG = 8
              GO TO OPEN-EXCEPT-FILE-OUTPUT.
           IF IO-FG = 7
              CLOSE EXCEPT-FILE
              GO TO OPEN-EXCEPT-FILE-OUTPUT.
           UNLOCK EXCEPT-FILE.
       OPEN-EXCEPT-FILE.
           PERFORM OPEN-IT.
           MOVE MKTEMP-BUF TO E-FILE.
           OPEN I-O EXCEPT-FILE.
           IF IO-FG = 8
              GO TO OPEN-EXCEPT-FILE.
           IF IO-FG = 7
              CLOSE EXCEPT-FILE
              GO TO OPEN-EXCEPT-FILE.
           UNLOCK EXCEPT-FILE.
       READ-EXCEPT-FILE-NEXT.
           PERFORM READ-IT.
           MOVE MKTEMP-BUF TO E-FILE.
           MOVE EXCEPT-KEY TO E-KEYX.
           READ EXCEPT-FILE NEXT.
           IF IO-FG = 8
              GO TO READ-EXCEPT-FILE-NEXT.
       START-EXCEPT-FILE.
           PERFORM START-IT.
           MOVE MKTEMP-BUF TO E-FILE.
           MOVE EXCEPT-KEY TO E-KEYX.
           START EXCEPT-FILE KEY NOT < EXCEPT-KEY.
           UNLOCK EXCEPT-FILE.
       WRITE-EXCEPT-FILE.
           PERFORM WRITE-IT.
           MOVE MKTEMP-BUF TO E-FILE.
           MOVE EXCEPT-KEY TO E-KEYX.
           WRITE EXCEPT-REC.
           IF IO-FG = 8
              GO TO WRITE-EXCEPT-FILE.
           UNLOCK EXCEPT-FILE.
       CLOSE-EXCEPT-FILE.
           PERFORM CLOSE-IT.
           CLOSE EXCEPT-FILE.
       COPY "LIBGB/FERRORS.CPY".

      ******************************************
       END-PROGRAM SECTION.
      ******************************************
       END-RUN.
           PERFORM CLOSE-FILES.
           MOVE MKTEMP-BUF TO ACCESS-BUF.
           PERFORM REMOVE-WORKFILE.
           MOVE WK2PATH TO ACCESS-BUF.
           PERFORM REMOVE-WORKFILE.
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
              DESTROY WDAYBK-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.
           EXIT PROGRAM.
