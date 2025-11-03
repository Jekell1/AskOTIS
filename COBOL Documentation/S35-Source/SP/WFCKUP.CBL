       IDENTIFICATION     DIVISION.
       PROGRAM-ID.          WFCKUP.
       AUTHOR.                 BLM.
       DATE-WRITTEN.    03/05/2015.
      ******************************************************************
      *
      * DESCRIPTION:  WELLS FARGO  CHECK RECONCILIATION UPDATE
      *                 WORLD
      *
      * INPUT FILE = ../../SP/WFCHECKS, GLOBAL SP, RSZ 80
      * SAVES AS ../../SP/WFCHECKS.YYMMDD WHEN DONE RUNNING
      *
      * REV:
      *  150316 BLM NEW POSITIVE PAY, WELLS FARGO BANK, WORLD PR# 1055
      *  150501 BLM ADJUST TRAILER LAYOUT SO TOTAL DEBITS PRINTS
      *             CORRECTLY, WORLD PR# 1055A
      *  170914  CS ACTIVATE 8 DIGIT DATES
      * KEC 2018.0511 REMOVED USE OF ENT-GROUP AND/OR GROUP-FLAG.
      *               THIS WAS DONE IN RELATION TO THE EOBUF EXTERNAL ISSUE
      *               WHICH IF NOT BEING SET WILL CAUSE ISSUES FROM THOSE
      *               BYTE POSITIONS COULD BE SET FROM PREVIOUS
      *               PROGRAM WITH EOBUF THAT SET SOMETHING IN THOSE BYTES.
      * BAH 2018.1101 GLOBAL CKFILE
      * KEC 2015.1026 REMOVE NOTES PAYABLE A32 - RMNP32
      *               REMOVED THE REMAINING LIBNP COPY MEMBERS WHICH
      *               WERE FOR CHECKS THAT WERE IN ACCOUNTS PAYABLE (AP);
      *               USING THE AP CHECKS COPY MEMBERS BUT NEED TO DO
      *               A 'REPLACING LEADING' OF "CK" TO "APCK" SO IT CAN
      *               WORK WITH LOAN PROCESSING (LP) CHECKS.
      * BAH 2019.0930 REMOVED ACCESS-CALL CKFILE
      * BAH 2019.1104 REMOVED LIBAP CHECKS, PT0192
      * MB  2023.1103 PERFORMANCE TUNING - EMBEDDED CKFILE CALLS
      * BAH 2024.0530 REMOVE BR-MACHINE NOT = DATA-PATH-FIL-MACHINE S35Q-57
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "LIBGB/GBFSPR.CPY".

           SELECT CHECK-FILE ASSIGN CHECK-PATH
                  ORGANIZATION LINE SEQUENTIAL
                  ACCESS SEQUENTIAL
                  LOCK MODE AUTOMATIC WITH LOCK ON RECORD
                  FILE STATUS FILE-STAT.

       I-O-CONTROL.
       DATA DIVISION.
       FILE SECTION.
 
       COPY "LIBGB/GBFDPR.CPY".


       FD  CHECK-FILE
           LABEL RECORDS ARE STANDARD.
       01  CHECK-HEADER.
           03  CHECK-HEADER-ID               PIC X(2).
           03  FILLER                        PIC X(25).
           03  CHECK-HEADER-DATE1            PIC 9(6).
           03  CHECK-HEADER-DATE2            PIC 9(6).
           03  FILLER                        PIC X(41).
           03  CHECK-HEADER-CARR-RET         PIC X.
       01  CHECK-REC.
           03  CHECK-REPORT-ID               PIC X(3).
           03  CHECK-BRNO                    PIC 9(4).
           03  CHECK-NUMBER                  PIC 9(6).  
           03  CHECK-TRANSACTION             PIC 9(3).
           03  CHECK-ISSUE-DATE              PIC 9(6).
           03  CHECK-AMOUNT                  PIC 9(8)V99.
           03  CHECK-PAID-DATE               PIC 9(6).
           03  CHECK-PAYEE                   PIC X(43).
       01  CHECK-TRAILER.
           03  CHECK-TRAILER-ID              PIC X(2).
           03  FILLER                        PIC X.
           03  CHECK-TRAILER-CRED-COUNT      PIC 9(8).
           03  FILLER                        PIC X.
           03  CHECK-TRAILER-CRED-AMT        PIC 9(9)V99.
           03  FILLER                        PIC XX.
           03  CHECK-TRAILER-DEBIT-COUNT     PIC 9(7).
           03  FILLER                        PIC X.
           03  CHECK-TRAILER-DEBIT-AMT       PIC 9(9)V99.
           03  FILLER                        PIC X(36).

       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(37) VALUE
           "@(#)SP/WFCKUP S35 02/28/24-12:58:03 >".
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
      *COPY "LIBGB/GB01GR.CPY".
      *COPY "LIBGB/GB01GR_SQL.CPY".
      *COPY "LIBGB/GBWSGR.CPY".
       COPY "LIBLP/LP01CK.CPY".
       COPY "LIBLP/LP01CK_SQL.CPY".
       COPY "LIBLP/LPWSCK.CPY".

       01  CHECK-PATH.
           03  CHECK-PATH-BASE          PIC X(09).
           03  FILLER                   PIC X(01) VALUE "/".
           03  CHECK-PATH-MACHINE       PIC X(02).
           03  FILLER                   PIC X(12) VALUE "/SP/WFCHECKS".

       01  SAVE-CHECK-PATH.
           03  SAVE-CHECK-PATH-BASE     PIC X(09).
           03  FILLER                   PIC X(01) VALUE "/".
           03  SAVE-CHECK-PATH-MACHINE  PIC X(02).
           03  FILLER                   PIC X(13) VALUE "/SP/WFCHECKS.".
           03  SAVE-CHECK-PATH-DATE     PIC 9(06).
           

       01  HOLD-BRNO            PIC 9(5) VALUE 0.
      *-----------------------------------------------------------------
      * LINE WORKERS
      *-----------------------------------------------------------------
       01  LN-WORKERS.
           03  LN-FEED          PIC 999      VALUE 0.
           03  LN-COUNT         PIC 999      VALUE 99.
           03  LN-WK            PIC 999      VALUE 0.
       01  PAGE-NUMBER          PIC 9(4)     VALUE 0.
 
       01  WS-AMT               PIC 9(6).99.

       01  BR-TOTAL-EXCEPTIONS  PIC S9(9)    COMP-3   VALUE 0.
       01  BR-TOTAL-PREVIOUS    PIC S9(9)    COMP-3   VALUE 0.
       01  BR-TOTAL-RECS        PIC S9(9)    COMP-3   VALUE 0.
       01  BR-TOTAL-AMOUNT      PIC S9(9)V99 COMP-3   VALUE 0.
       01  GRAND-TOTAL-RECS     PIC S9(9)    COMP-3   VALUE 0.
       01  GRAND-TOTAL-EXCEPT   PIC S9(9)    COMP-3   VALUE 0.
       01  GRAND-TOTAL-PREVIOUS PIC S9(9)    COMP-3   VALUE 0.
       01  GRAND-TOTAL-AMOUNT   PIC S9(9)V99 COMP-3   VALUE 0.

       COPY "LIBGB/EOBUF_EXT.CPY".
           03  BEG-BR          PIC 9(04).
           03  END-BR          PIC 9(04).
           03  REPORT-ONLY     PIC X.
       01  VERSION-CONTROL     PIC X    VALUE "A".

       01  EXITPROG             PIC X(9)     VALUE "LP/LPCKMU".

       01  MESS-LIST            PIC X(05) VALUE "MESS.".

      *-----------------------------------------------------------------
      * PRINT BUFFERS
      *-----------------------------------------------------------------
       01  HEAD1.
           03  H-DATE           PIC X(08)     VALUE " ".
           03  FILLER           PIC XX       VALUE " ".
           03  H-HR             PIC 99       VALUE 0.
           03  FILLER           PIC X        VALUE ":".
           03  H-MIN            PIC 99       VALUE 0.
           03  FILLER           PIC X(23)    VALUE " ".
           03  H-NAME           PIC X(40)    VALUE " ".
           03  FILLER           PIC X(29)    VALUE " ".
           03  FILLER           PIC X(05)     VALUE "PAGE:".
           03  H-PAGE-NO        PIC ZZZ9.
 
       01  HEAD2.
           03  FILLER           PIC X(10)    VALUE "USER ID:".
           03  H-USERID         PIC X(12).
           03  FILLER           PIC X(10)    VALUE " ".
           03  FILLER           PIC X(34) VALUE
           "WELLS FARGO REVERSE POSITIVE PAY: ".
           03  HEAD2-DATE1      PIC 99/99/99.
           03  FILLER           PIC X(3) VALUE " - ".
           03  HEAD2-DATE2      PIC 99/99/99.
       01  HEAD3.
           03  FILLER           PIC X(5)     VALUE " BR #".
           03  FILLER           PIC X(8)     VALUE " CHK #  ".
           03  FILLER           PIC X(12)    VALUE "    AMOUNT  ".
           03  FILLER           PIC X(9)     VALUE "ISS DATE ".
           03  FILLER           PIC X(11)    VALUE " PAID DATE ".
           03  FILLER           PIC X(32)    VALUE "PAYEE".
           03  FILLER           PIC X(10)    VALUE "EXCEPTION".
 
       01  DETAIL-LINE          PIC X(132) VALUE SPACES.
       01  DETAIL-LINE1 REDEFINES DETAIL-LINE.
           03  D-BRNO           PIC Z(4)9.
           03  FILLER           PIC X(1).
           03  D-NUMBER         PIC Z(6).
           03  FILLER           PIC X.
           03  D-AMOUNT         PIC Z(7).99 BLANK ZERO.
           03  FILLER           PIC X(2).
           03  D-ISSUE-DATE     PIC 99/99/99 BLANK ZERO.
           03  FILLER           PIC X(2).
           03  D-PAID-DATE      PIC 99/99/99 BLANK ZERO.
           03  FILLER           PIC X(2).
           03  D-PAYEE          PIC X(30).
           03  FILLER           PIC X(2).
           03  D-MSG1           PIC X(40).

       01  DETAIL-LINE2 REDEFINES DETAIL-LINE.
           03  D-TOTAL-BRNO     PIC Z(4).
           03  FILLER           PIC X(2).
           03  D-TOTAL-TAG      PIC X(15).
           03  FILLER           PIC X(05).                 
           03  D-TOTAL-REC      PIC Z(8)9.
           03  FILLER           PIC X(05).                 
           03  D-TOT-AMT.
               05  D-TOTAL-AMOUNT PIC ZZZ,ZZZ,ZZZ.99.
               05  FILLER         PIC X(15).                 
           03  FILLER REDEFINES D-TOT-AMT.
               05  D-PREV-TAG     PIC X(15).
               05  FILLER         PIC X(05).                 
               05  D-TOTAL-PREV   PIC Z(8)9.

       01  RANGE-LINE REDEFINES DETAIL-LINE.
           03  RANGE-SELECTION  PIC X(30).
           03  FILLER           PIC X(05).
           03  RANGE1           PIC X(30).
           03  FILLER REDEFINES RANGE1.
               05  RANGE1N      PIC Z(09)9.
               05  FILLER       PIC X(20).
           03  FILLER           PIC X(02).
 
      *  "/$REL/SH/EC /$REL/SH/CHMOD.SH".
       01  CHMOD-CMD.
           03  FILLER           PIC X    VALUE "/".
           03  CHMOD-REL        PIC X(7).
           03  FILLER           PIC X(4) VALUE "/SH/".
           03  CHMOD-EC         PIC X(3) VALUE "EC ".
           03  FILLER           PIC X    VALUE "/".
           03  CHMOD-REL2       PIC X(7).
           03  FILLER           PIC X(4) VALUE "/SH/".
           03  CHMOD-SHELL      PIC X(10) VALUE "CHMOD.SH ".
           03  CHMOD-DATA       PIC X(11).
           03  FILLER           PIC X     VALUE " ".
           03  CHMOD-FILENAME   PIC X(15).

       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBGB/GETPRW.CPY".
       COPY "LIBGB/SYSTEMW.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBSP/WFCKUP_DEF.CPY".
       COPY "LIBSP/WFCKUP_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".
 
      /***************************************
      *     P R O G R A M   C O N T R O L    *
      ****************************************
       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBSP/WFCKUP_SCN.CPY".
       PROCEDURE DIVISION
            USING FORM-PATHNAME.
       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
               PR-FILE CHECK-FILE.
       COPY "LIBGB/DECLRP.CPY".
           PERFORM CLOSE-CK1-FILE.
           PERFORM CLOSE-BR-FILE.
           PERFORM CLOSE-PR-FILE.
           PERFORM SQL-CLOSE-UPDATE-CONNECTION.
           CLOSE
               CHECK-FILE.
       COPY "LIBGB/DECLRP2.CPY".
 
      ****************************************
       MAIN-MODULE SECTION.
      ****************************************
           PERFORM SQL-CONNECT.

      D    STOP MESS.
           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "WFCKUP" TO VDU-FORM-NAME.

           PERFORM INITIALIZATION.
           IF EXT-EOBUF-ERRCD NOT = " "
              GO TO END-PROGRAM.

           MOVE 0 TO HOLD-BRNO.

           PERFORM OPEN-CHECK-FILE.
      D    STOP MESS.
       NEXT-CHECK.
           PERFORM READ-CHECK-FILE-NEXT.
           IF IO-FG NOT = 0
              PERFORM CLOSE-CK1-FILE
              PERFORM PRINT-BR-TOTALS
              GO TO END-ROUTINE.

           IF CHECK-HEADER-ID = "H1"
              MOVE CHECK-HEADER-DATE1 TO HEAD2-DATE1
              MOVE CHECK-HEADER-DATE2 TO HEAD2-DATE2
              GO TO NEXT-CHECK.
           IF CHECK-TRAILER-ID = "T1"
              GO TO END-ROUTINE.

           IF (CHECK-BRNO < BEG-BR OR CHECK-BRNO > END-BR)
              GO TO NEXT-CHECK.

           IF CHECK-NUMBER = 0
              MOVE CHECK-BRNO       TO D-BRNO
              MOVE CHECK-NUMBER     TO D-NUMBER
              MOVE CHECK-AMOUNT     TO D-AMOUNT
              MOVE CHECK-ISSUE-DATE TO D-ISSUE-DATE
              MOVE CHECK-PAID-DATE  TO D-PAID-DATE
              MOVE CHECK-PAYEE      TO D-PAYEE
              MOVE "ZERO CHECK NUMBER" TO D-MSG1
              PERFORM WRITE-DETAIL-LINE
              ADD 1 TO BR-TOTAL-EXCEPTIONS GRAND-TOTAL-EXCEPT
              GO TO NEXT-CHECK.

      *
      *    IF CHECK-BRNO = 990
      *       PERFORM AP-CHECK-ROUTINE
      *       GO TO NEXT-CHECK.

           IF CHECK-BRNO NOT = HOLD-BRNO
              PERFORM CLOSE-CK1-FILE
              PERFORM PRINT-BR-TOTALS
              PERFORM GET-BR-SET-PATHS
              IF IO-FG NOT = 0
                 GO TO NEXT-CHECK.

           MOVE CHECK-BRNO    TO CK-BRNO.
           MOVE CHECK-NUMBER  TO CK-NO.
           PERFORM READ-CK1-FILE.   *>  EMBEDDED CK
           IF IO-FG NOT = 0
              MOVE CHECK-BRNO       TO D-BRNO
              MOVE CHECK-NUMBER     TO D-NUMBER
              MOVE CHECK-AMOUNT     TO D-AMOUNT
              MOVE CHECK-ISSUE-DATE TO D-ISSUE-DATE
              MOVE CHECK-PAID-DATE  TO D-PAID-DATE
              MOVE CHECK-PAYEE      TO D-PAYEE
              MOVE "MISSING CHECK RECORD" TO D-MSG1
              PERFORM WRITE-DETAIL-LINE
              ADD 1 TO BR-TOTAL-EXCEPTIONS GRAND-TOTAL-EXCEPT
      *       UNLOCK CK-FILE
              GO TO NEXT-CHECK.

           IF CK-CLEARED-FG = "Y"
      *       UNLOCK CK-FILE
              ADD 1 TO BR-TOTAL-PREVIOUS GRAND-TOTAL-PREVIOUS
              GO TO NEXT-CHECK.

           IF CK-VOID-DATE NOT = 0
              MOVE CHECK-BRNO       TO D-BRNO
              MOVE CHECK-NUMBER     TO D-NUMBER
              MOVE CHECK-AMOUNT     TO D-AMOUNT
              MOVE CHECK-ISSUE-DATE TO D-ISSUE-DATE
              MOVE CHECK-PAID-DATE  TO D-PAID-DATE
              MOVE CHECK-PAYEE      TO D-PAYEE
              MOVE "CHECK VOIDED"   TO D-MSG1
              PERFORM WRITE-DETAIL-LINE
              ADD 1 TO BR-TOTAL-EXCEPTIONS GRAND-TOTAL-EXCEPT
      *       UNLOCK CK-FILE
              GO TO NEXT-CHECK.

           IF CHECK-AMOUNT NOT = CK-AMT
              MOVE CHECK-BRNO       TO D-BRNO
              MOVE CHECK-NUMBER     TO D-NUMBER
              MOVE CHECK-AMOUNT     TO D-AMOUNT
              MOVE CHECK-ISSUE-DATE TO D-ISSUE-DATE
              MOVE CHECK-PAID-DATE  TO D-PAID-DATE
              MOVE CHECK-PAYEE      TO D-PAYEE
              MOVE CK-AMT           TO WS-AMT
              STRING "CHECK AMOUNTS DONT MATCH: ",WS-AMT 
                DELIMITED BY "  " INTO D-MSG1
              PERFORM WRITE-DETAIL-LINE
              ADD 1 TO BR-TOTAL-EXCEPTIONS GRAND-TOTAL-EXCEPT
      *       UNLOCK CK-FILE
              GO TO NEXT-CHECK.

           MOVE "Y" TO CK-CLEARED-FG.
      *    MOVE CHECK-PAID-DATE TO CK-CLEARED-DATE.
           MOVE CHECK-PAID-DATE TO DATE-MMDDYY.
           PERFORM CONVERT-MMDDYY-TO-YYYYMMDD.
           MOVE DATE-YYYYMMDD TO CK-CLEARED-DATE.
           IF REPORT-ONLY NOT = "Y"
              PERFORM REWRITE-CK1-FILE.

           ADD 1 TO BR-TOTAL-RECS GRAND-TOTAL-RECS.
           ADD CHECK-AMOUNT TO BR-TOTAL-AMOUNT GRAND-TOTAL-AMOUNT.

           GO TO NEXT-CHECK.
 
       END-ROUTINE.
           PERFORM PRINT-GRAND-TOTALS.
           PERFORM PRINT-RANGE-LINES.

           IF REPORT-ONLY NOT = "Y"
              MOVE CHECK-PATH TO ACCESS-BUF
              PERFORM ACCESS-CALL
              IF STAT = "00"
                 MOVE EXT-CONAME-SYSDATE TO ALP-DATE
                 PERFORM CONVERT-ALPDATE-TO-YYMMDD 
                 MOVE DATE-YYMMDD TO SAVE-CHECK-PATH-DATE
                 MOVE EXT-FILPATH-BASE    TO SAVE-CHECK-PATH-BASE
                 MOVE EXT-FILPATH-MACHINE TO SAVE-CHECK-PATH-MACHINE
                 MOVE SPACES TO SYSTEM-BUF
                 STRING MV-CMD," ",CHECK-PATH," ",SAVE-CHECK-PATH
                        DELIMITED BY "  " INTO SYSTEM-BUF
                 PERFORM SYSTEM-CALL.

           GO TO END-PROGRAM.

 
      ***********************************
       INITIALIZATION SECTION.
      ***********************************
           MOVE WFCKUP-QUEUE-POS TO Q-POS.
           MOVE WFCKUP-COPIES-POS TO C-POS.
           MOVE WFCKUP-PRU-POS   TO P-POS.
           MOVE SPACES TO Q-BUF C-BUF P-BUF.
       COPY "LIBGB/EOINIT.CPY".
 
           MOVE "PROCESSING CHECK RECONCILIATION" TO MESS.
           PERFORM SEND-LEGEND.

           PERFORM OPEN-BR-FILE.
           PERFORM OPEN-PR-FILE.
 
           MOVE EXT-CONAME-CONAME TO H-NAME.
           MOVE EXT-CONAME-SYSDATE TO H-DATE.
           MOVE EXT-CONAME-USER-ID TO H-USERID.

           MOVE 0 TO BR-TOTAL-RECS GRAND-TOTAL-RECS.
           MOVE 0 TO BR-TOTAL-EXCEPTIONS GRAND-TOTAL-EXCEPT.
           MOVE 0 TO BR-TOTAL-PREVIOUS GRAND-TOTAL-PREVIOUS.
           MOVE 0 TO BR-TOTAL-AMOUNT GRAND-TOTAL-AMOUNT.

           PERFORM SQL-OPEN-UPDATE-CONNECTION.

           GO TO INITIALIZE-EXIT.

       INITIALIZE-ENDIT.
           MOVE "X" TO EXT-EOBUF-ERRCD.
       INITIALIZE-EXIT.
           EXIT.

      *******************************************************
       EO-EXEC-AUDIT SECTION.
      *******************************************************

      *******************************************************
       ENTER-PARAMETERS SECTION.
      *******************************************************
       ENTER-PARAM.
      * FOR RE-ENTRY ON F4 FROM ACCEPT.
           IF VDUSTATUS = "1U"
              GO TO BR-02.
      * CHMOD 770
           MOVE "REL" TO GETENV-BUF.
           PERFORM GETENV-CALL.
           MOVE GETENV-BUF TO CHMOD-REL CHMOD-REL2.
           CALL "C$TOLOWER" USING CHMOD-EC, VALUE 3.
           CALL "C$TOLOWER" USING CHMOD-SHELL, VALUE 10.
           MOVE EXT-FILPATH-FIL TO CHMOD-DATA.
           MOVE "SP/WFCHECKS" TO CHMOD-FILENAME.
           MOVE CHMOD-CMD TO SYSTEM-BUF.
           PERFORM SYSTEM-CALL.

           PERFORM LOAD-CHECK-FILE.
           MOVE CHECK-PATH TO ACCESS-BUF.
           PERFORM ACCESS-CALL.
           IF STAT NOT = "00"
              MOVE "WFCHECKS FILE NOT AVAILABLE" TO MESS
              PERFORM SEND-MESS
              MOVE "1>" TO VDUSTATUS
              GO TO EXIT-PARM.

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
           GO TO ENTER-REPORT-ONLY.
       ALL-BR.
           MOVE "SNNN040BR1." TO VDU.
           MOVE 0 TO BEG-BR VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN040BR2." TO VDU.
           MOVE 9999 TO END-BR VDUNUM0.
           PERFORM SCREENIO.
      *----------------------------------------------------------------
       ENTER-REPORT-ONLY.
           MOVE "E  A010REPONLY." TO VDU.
           PERFORM SCREENIO.
           IF UP-KEY GO TO BR-02.
           IF F1-KEY GO TO EXIT-PARM.
           IF F7-KEY GO TO EXIT-PARM.
           IF NOT ENTER-KEY
              GO TO ENTER-REPORT-ONLY.
           MOVE VDUBUF TO REPORT-ONLY.
           IF REPORT-ONLY NOT = "Y"
              IF REPORT-ONLY NOT = "N"
                 GO TO ENTER-REPORT-ONLY.

       EXIT-PARM.
           EXIT.

      *************************************************
       DISPLAY-PARAMETERS SECTION.
      *************************************************
           MOVE "SNNN040BR1." TO VDU
           MOVE BEG-BR TO VDUNUM0
           PERFORM SCREENIO
           MOVE "SNNN040BR2." TO VDU
           MOVE END-BR TO VDUNUM0
           PERFORM SCREENIO.

      *************************************
       PRINT-BR-TOTALS SECTION.
      *************************************
           IF BR-TOTAL-RECS = 0 AND
               BR-TOTAL-EXCEPTIONS = 0 AND
                BR-TOTAL-PREVIOUS = 0 AND
                 BR-TOTAL-AMOUNT = 0
                   GO TO EXIT-PRINT-BR-TOTALS.

           MOVE HOLD-BRNO TO D-TOTAL-BRNO.
           MOVE "BRANCH TOTAL: " TO D-TOTAL-TAG.
           MOVE BR-TOTAL-RECS TO D-TOTAL-REC.
           MOVE BR-TOTAL-AMOUNT TO D-TOTAL-AMOUNT.
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.
           MOVE "EXCEPTIONS  : " TO D-TOTAL-TAG.
           MOVE BR-TOTAL-EXCEPTIONS TO D-TOTAL-REC.
           MOVE "PREV CLEARED: " TO D-PREV-TAG.
           MOVE BR-TOTAL-PREVIOUS TO D-TOTAL-PREV.
           PERFORM WRITE-DETAIL-LINE.
           MOVE SPACES TO DETAIL-LINE.
           PERFORM WRITE-DETAIL-LINE.

       EXIT-PRINT-BR-TOTALS.
           EXIT.

      *************************************
       PRINT-GRAND-TOTALS SECTION.
      *************************************
           MOVE 2 TO LN-FEED.
           MOVE "GRAND TOTAL  :" TO D-TOTAL-TAG.
           MOVE GRAND-TOTAL-RECS TO D-TOTAL-REC.
           MOVE GRAND-TOTAL-AMOUNT TO D-TOTAL-AMOUNT.
           PERFORM WRITE-DETAIL-LINE.
           MOVE "EXCEPTIONS   :" TO D-TOTAL-TAG.
           MOVE GRAND-TOTAL-EXCEPT TO D-TOTAL-REC.
           MOVE "PREV CLEARED : " TO D-PREV-TAG.
           MOVE GRAND-TOTAL-PREVIOUS TO D-TOTAL-PREV.
           PERFORM WRITE-DETAIL-LINE.
           MOVE 2 TO LN-FEED.
           MOVE "BANK TRAILER  " TO D-TOTAL-TAG.
           PERFORM WRITE-DETAIL-LINE.
           MOVE "TOTAL DEBITS:" TO D-TOTAL-TAG.
           MOVE CHECK-TRAILER-DEBIT-COUNT TO D-TOTAL-REC. 
           MOVE CHECK-TRAILER-DEBIT-AMT   TO D-TOTAL-AMOUNT. 
           PERFORM WRITE-DETAIL-LINE.

      *******************************
       PRINT-RANGE-LINES SECTION.
      *******************************

           MOVE 2 TO LN-FEED.
           MOVE "BEGINNING BRANCH     "          TO RANGE-SELECTION
           MOVE BEG-BR                           TO RANGE1N
           PERFORM WRITE-DETAIL-LINE
           MOVE "ENDING    BRANCH     "          TO RANGE-SELECTION
           MOVE END-BR                           TO RANGE1N
           PERFORM WRITE-DETAIL-LINE.
           MOVE "REPORT ONLY          " TO RANGE-SELECTION.
           MOVE REPORT-ONLY TO RANGE1.
           PERFORM WRITE-DETAIL-LINE.

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
 
      *******************************
       GET-BR-SET-PATHS SECTION.
      *******************************
           MOVE CHECK-BRNO TO BR-NO.
           PERFORM READ-BR-FILE.
           IF IO-FG NOT = 0
              MOVE CHECK-BRNO   TO D-BRNO
              MOVE CHECK-NUMBER TO D-NUMBER
              MOVE CHECK-AMOUNT TO D-AMOUNT
              MOVE "INVALID BRANCH" TO D-MSG1
              MOVE 2 TO LN-FEED
              PERFORM WRITE-DETAIL-LINE
              MOVE 9 TO IO-FG
              ADD 1 TO GRAND-TOTAL-EXCEPT
              MOVE 0 TO HOLD-BRNO
           ELSE
      * S35Q-57
      *       IF BR-MACHINE NOT = EXT-FILPATH-MACHINE
      *          MOVE 9 TO IO-FG
      *          MOVE 0 TO HOLD-BRNO
      *       ELSE
                 MOVE "Y"              TO CK-PATH-OVERRIDE
                 MOVE EXT-FILPATH-BASE TO CK-ALL-BASE
                 MOVE BR-MACHINE       TO CK-ALL-MACHINE
                 MOVE BR-DATA-PATH     TO CK-ALL-DATA
                 MOVE CK-ALL-PATH      TO CK-PATH
                 PERFORM OPEN-CK1-FILE
                 MOVE 2 TO LN-FEED
                 MOVE CHECK-BRNO TO HOLD-BRNO.
              
           MOVE 0 TO BR-TOTAL-RECS BR-TOTAL-EXCEPTIONS BR-TOTAL-AMOUNT
                     BR-TOTAL-PREVIOUS.

       EXIT-GET-BR-SET-PATHS.
           EXIT.

       COPY "LIBGB/TIMEIO.CPY".
       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/SCREEN.CPY".
 
      *********************************************************
       FORM-RESET SECTION.
      *********************************************************
           MOVE EXT-CONAME-CONAME TO WFCKUP-CONAME.
           MOVE EXT-CONAME-SYSDATE TO WFCKUP-SYSDATE.
           DISPLAY WFCKUP-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE WFCKUP-FORM-FIELDS TO WR-BUF.
           MOVE WFCKUP-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      *********************************************************
       SETUP-LINK-INFO SECTION.
      *********************************************************
           MOVE WFCKUP-HELP--FILE TO HELP-LINK-FILE.
           MOVE WFCKUP-HELP--DIR  TO HELP-LINK-DIR.

      *********************************************************
       VDU-CONVERT-FIELD SECTION.
      *********************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBSP/WFCKUP_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      **************************************
       MISC-PARA SECTION.
      **************************************
       COPY "LIBGB/GETPR.CPY".
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/SYSTEM.CPY".
       COPY "LIBGB/GETENV.CPY".
       INVALID-RANGE.
           MOVE "INVALID RANGE" TO MESS.
           PERFORM SEND-MESS.
       SEND-LEGEND.
           MOVE MESS TO WR-BUF.
           MOVE MESS-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      **************************************
       IO-ROUTINES SECTION.
      **************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBLP/LPCKGS_SQL.CPY".
      *COPY "LIBGB/GBGRGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBPRINT.CPY".
       LOAD-CHECK-FILE.
           MOVE EXT-FILPATH-BASE     TO CHECK-PATH-BASE.
           MOVE EXT-FILPATH-MACHINE  TO CHECK-PATH-MACHINE.
       OPEN-CHECK-FILE.
           PERFORM OPEN-IT.
           MOVE CHECK-PATH TO E-FILE.
           OPEN INPUT CHECK-FILE.
           IF IO-FG = 8
              GO TO OPEN-CHECK-FILE.
           IF IO-FG = 7
              CLOSE CHECK-FILE
              GO TO OPEN-CHECK-FILE.
       READ-CHECK-FILE-NEXT.
           PERFORM READ-IT.
           MOVE CHECK-PATH TO E-FILE.
           READ CHECK-FILE NEXT.
           IF IO-FG = 8
              GO TO READ-CHECK-FILE-NEXT.
       CLOSE-CHECK-FILE.
           PERFORM CLOSE-IT.
           CLOSE CHECK-FILE.

       COPY "LIBGB/GBBRRN.CPY".

      * COPY "LIBLP/LPCK1IN.CPY".
      * COPYMEMBER: LIBLP/LPCK1IN
       LOAD-CK1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( CK-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( CK-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO CK-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO CK-ALL-MACHINE
              MOVE EXT-FILPATH-DATA        TO CK-ALL-DATA
              MOVE CK-ALL-PATH             TO CK-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( CK-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO CK-ALL-BASE
              MOVE FILPATH-MACHINE         TO CK-ALL-MACHINE
              MOVE FILPATH-DATA            TO CK-ALL-DATA
              MOVE CK-ALL-PATH             TO CK-PATH.

      *-----------------------------------------------------------------
       OPEN-CK1-FILE.
           PERFORM LOAD-CK1-FILE.
           PERFORM OPEN-IT.
           MOVE CK-PATH-SQL TO E-FILE.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       READ-CK1-FILE.   *>  EMBEDDED CK
           PERFORM READ-IT.
           MOVE CK-PATH-OWNBR  TO CK-BRNO. 
           MOVE CK-PATH-SQL    TO E-FILE.
           MOVE CK1-KEY        TO E-KEYX.

           INITIALIZE QCK-REC.

           MOVE CK-BRNO   TO QCK-BRNO.
           MOVE CK-NO     TO QCK-NO.

           EXEC SQL
            SELECT CKFILE.CK_BRNO,
                   CKFILE.CK_NO,
                   CKFILE.CK_AMT,
                   CAST(CKFILE.CK_VOID_DATE AS VARCHAR(10)),
                   CKFILE.CK_CLEARED_FG,
                   CAST(CKFILE.CK_CLEARED_DATE AS VARCHAR(10))
            INTO :QCK-BRNO,
                 :QCK-NO,
                 :QCK-AMT,
                 :QCK-VOID-DATE,
                 :QCK-CLEARED-FG,
                 :QCK-CLEARED-DATE
            FROM DBO.CKFILE
            WHERE CKFILE.CK_BRNO = :QCK-BRNO
              AND CKFILE.CK_NO = :QCK-NO
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-CK1-FILE.

           IF ( IO-FG = 0 )
              PERFORM GET-CK-FIELDS.

      *-----------------------------------------------------------------
       REWRITE-CK1-FILE.

           MOVE CK-PATH-OWNBR  TO CK-BRNO. 
           ACCEPT CK-LTOUCH-DATE FROM CENTURY-DATE.

           PERFORM REWRITE-IT.

           MOVE CK-PATH-SQL    TO E-FILE.
           MOVE CK1-KEY        TO E-KEYX.

           PERFORM SET-CK-FIELDS.

           MOVE '2' TO SQL-UPDATE-CONN-USE-SW.    *> CHANGE CONNECTION TO UPDATE_CONNECT
           PERFORM SQL-SWAP-UPDATE-CONNECTION.

           EXEC SQL
            UPDATE DBO.CKFILE
             SET CKFILE.CK_LTOUCH_DATE  = :QCK-LTOUCH-DATE,
                 CKFILE.CK_CLEARED_FG   = :QCK-CLEARED-FG,
                 CKFILE.CK_CLEARED_DATE = :QCK-CLEARED-DATE

             WHERE CKFILE.CK_BRNO = :QCK-BRNO
               AND CKFILE.CK_NO = :QCK-NO

           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE '1' TO SQL-UPDATE-CONN-USE-SW.    *> CHANGE CONNECTION BACK TO C1
           PERFORM SQL-SWAP-UPDATE-CONNECTION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO REWRITE-CK1-FILE.

      *    IF ( IO-FG NOT = 0 )
      *       EXEC SQL COMMIT END-EXEC
      *       PERFORM SQL-IO-VALIDATION.

      *-----------------------------------------------------------------
       CLOSE-CK1-FILE.

           PERFORM CLOSE-IT.
      *-----------------------------------------------------------------


       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO EXT-EOBUF-ERRCD.
 
      **************************************
       END-PROGRAM SECTION.
      **************************************
       END-RUN.
           PERFORM CLOSE-FILES.
           IF EXT-ROUTE-BUF = "00"
              PERFORM PRU-OUT.
       EXIT-PROG.
           IF EXT-ROUTE-BUF = "00"
              MOVE EXITPROG TO FORM-NAM
              MOVE FORM-PATH TO FORM-PATHNAME.
       STOP-RUN.
           IF EXT-ROUTE-BUF = "00" OR "40" OR "50"
              DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN
              DESTROY WFCKUP-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.
           EXIT PROGRAM.
