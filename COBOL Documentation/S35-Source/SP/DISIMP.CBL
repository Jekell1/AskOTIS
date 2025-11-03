       IDENTIFICATION     DIVISION.
       PROGRAM-ID.          DISIMP.
       DATE-WRITTEN.    03/21/2017.
      ******************************************************************
      *
      * PROGRAM TO UPLOAD A FILE FROM BK DATABASE THAT WILL UPDATE DISCHARGE
      * DATE ON BK INFO SCREEN, PAGE 2, FIELD 9.
      * CREATES A LOAN MEMO
      *
      * USED DAILY (2023/03/13)
      *
      * INPUT FILE = /USR/DATA/R1/FTP/BK/DISCHG
      * DONE FILE = /USR/DATA/R1/FTP/BK/DONE/DISCHG.YYMMDD
      *
      * REV:
      *  170321 BAH WORLD, PR#1234
      *  180308 CS  IF ACCOUNT HAS LTB-REAFF-DATE AND LTB-RESCIND-DATE IS
      *             0, DO NOT UPDATE DISCHARGE DATE, AND WRITE A MEMO THAT
      *             DISCHARGE IMPORT WAS ATTEMPTED   WORLD PR#1309 
      *
      * KEC 2018.0511 PR#00000 PARADATA A30 JAY & BETH
      *         REMOVED USE OF ENT-GROUP AND/OR GROUP-FLAG.
      *         THIS WAS DONE IN RELATION TO THE EOBUF EXTERNAL ISSUE
      *         WHICH IF NOT BEING SET WILL CAUSE ISSUES FROM THOSE
      *         BYTE POSITIONS COULD BE SET FROM PREVIOUS
      *         PROGRAM WITH EOBUF THAT SET SOMETHING IN THOSE BYTES.
      *
      * BAH 181214 GLOBAL LNFILE
      * BAH 20191004 REMOVED ACCESS-CALL LNFILE
      * BAH 20200925 CLEAR LTB-GROSS-BAL, #1461
      * RMZ 20231005 PERFORMANCE TUNING - IMPROVED SQL CALLS.
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "LIBGB/GBFSPR.CPY".
           SELECT INPUT-FILE ASSIGN INPUT-PATH
                  ORGANIZATION LINE SEQUENTIAL
                  ACCESS SEQUENTIAL
                  LOCK MODE AUTOMATIC WITH LOCK ON RECORD
                  FILE STATUS FILE-STAT.

       I-O-CONTROL.
       DATA DIVISION.
       FILE SECTION.
 
       COPY "LIBGB/GBFDPR.CPY".

       FD  INPUT-FILE
           LABEL RECORDS ARE STANDARD.
       01  IN-REC.
           03  IN-BRNO                    PIC 9(4).
           03  IN-BRNOX REDEFINES IN-BRNO PIC X(4).
           03  IN-NUMBER                  PIC 9(6).
           03  IN-NUMBERX REDEFINES IN-NUMBER PIC X(6).
           03  IN-DATE-MMDDYY             PIC 9(6).

       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)SP/DISIMP.CBL S35 11/22/23-12:59:02 >".
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBGB/GB01GR.CPY".
       COPY "LIBGB/GB01GR_SQL.CPY".
       COPY "LIBGB/GBWSGR.CPY".
       COPY "LIBLP/LP01LN.CPY".
       COPY "LIBLP/LP01LN_SQL.CPY".
       COPY "LIBLP/LPWSLN.CPY".
       COPY "LIBLP/LP01LM.CPY".
       COPY "LIBLP/LP01LM_SQL.CPY".
       COPY "LIBLP/LPWSLM.CPY".
       COPY "LIBLP/LP01LTB.CPY".
       COPY "LIBLP/LP01LTB_SQL.CPY".
       COPY "LIBLP/LPWSLTB.CPY".
       01  INPUT-PATH.
           03  INPUT-PATH-BASE      PIC X(9).
           03  FILLER               PIC X VALUE "/".
           03  INPUT-PATH-MACHINE   PIC X(2).
           03  INPUT-PATH-DIR       PIC X(8) VALUE "/FTP/BK/".
           03  FILLER               PIC X(6) VALUE "DISCHG".

       01  INPUT-SAVE-PATH.
           03  INPUT-SAVE-BASE        PIC X(9).
           03  FILLER                 PIC X VALUE "/".
           03  INPUT-SAVE-MACHINE     PIC X(2).
           03  INPUT-SAVE-DIR         PIC X(8) VALUE "/FTP/BK/".
           03  FILLER                 PIC X(12) VALUE "DONE/DISCHG.".
           03  INPUT-SAVE-DATE-YYMMDD PIC 9(6).
           

       01  HOLD-BRNO            PIC 9(4) VALUE 0.
       01  HOLD-SYSDATE         PIC 9(8).
      *-----------------------------------------------------------------
      * LINE WORKERS
      *-----------------------------------------------------------------
       01  LN-WORKERS.
           03  LN-FEED          PIC 999      VALUE 0.
           03  LN-COUNT         PIC 999      VALUE 99.
           03  LN-WK            PIC 999      VALUE 0.
       01  PAGE-NUMBER          PIC 9(4)     VALUE 0.
 
       01  WS-MEMO.
           03  FILLER        PIC X(29) VALUE 
               "DISIMP - SET DISCHARGE DATE ".
           03  WS-MEMO-NEW-DATE PIC 99/99/99.
      
       01  DISCHARGE-EXCEPT-MEMO.
           03  FILLER        PIC X(30) VALUE
               "DISIMP - EXCEPT:DISCHARGE DTE ".
           03  DISCHARGE-MEMO-DATE PIC 99/99/99.

       01  RECORD-COUNTER       PIC 9(10)       VALUE 0.
       01  MISSING-BRANCH       PIC 9(10)       VALUE 0.
       01  MISSING-LOAN-FILE    PIC 9(10)       VALUE 0.
       01  EXCEPTION-COUNTER    PIC 9(10)       VALUE 0.
       01  UPDATED-COUNTER      PIC 9(10)       VALUE 0.

 
      * SCREEN FIELDS
      *-----------------------------------------------------------------
       COPY "LIBGB/EOBUF_EXT.CPY".
           03  BEG-BR          PIC 9(04).
           03  END-BR          PIC 9(04).
           03  REPORT-ONLY     PIC X.
       01  VERSION-CONTROL     PIC X    VALUE "A".

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
           "   IMPORT TO SET DISCHARGE DATE".
       01  HEAD3.
           03  FILLER           PIC X(04)    VALUE "BRNO".
           03  FILLER           PIC X(06)    VALUE SPACES.
           03  FILLER           PIC X(13)    VALUE "ACCTNO".
 
 
       01  DETAIL-LINE          PIC X(80) VALUE SPACES.
       01  DETAIL-LINE1 REDEFINES DETAIL-LINE.
           03  D-BRNO           PIC Z(4).
           03  FILLER           PIC X(2).
           03  D-NUMBER         PIC Z(9).
           03  FILLER           PIC X(2).
           03  D-TEXT           PIC X(40).

       01  RANGE-LINE REDEFINES DETAIL-LINE.
           03  RANGE-SELECTION  PIC X(30).
           03  FILLER           PIC X(05).
           03  RANGE1           PIC X(30).
           03  FILLER REDEFINES RANGE1.
               05  RANGE1N      PIC Z(09)9.
               05  FILLER       PIC X(20).
           03  FILLER           PIC X(02).
 
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
       COPY "LIBSP/DISIMP_DEF.CPY".
       COPY "LIBSP/DISIMP_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

      ****************************************
      *     P R O G R A M   C O N T R O L    *
      ****************************************
       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBSP/DISIMP_SCN.CPY".

       PROCEDURE DIVISION
            USING FORM-PATHNAME EXIT-PATHNAME.
       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
               PR-FILE INPUT-FILE.
       COPY "LIBGB/DECLRP.CPY".
           PERFORM CLOSE-LTB1-FILE.
           PERFORM CLOSE-LN1-FILE.
           PERFORM CLOSE-LM1-FILE.
           PERFORM CLOSE-BR-FILE.
           PERFORM SQL-CLOSE-UPDATE-CONNECTION.
           PERFORM CLOSE-PR-FILE.
           CLOSE
               INPUT-FILE.
       COPY "LIBGB/DECLRP2.CPY".
 
      ********************************************
       MAIN-MODULE SECTION.
      ********************************************
           PERFORM SQL-CONNECT.
      D    STOP MESS.

           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "DISIMP" TO VDU-FORM-NAME.

      * FTP/BK NEEDS TO BE LOWER CASE
           CALL "C$TOLOWER" USING INPUT-PATH-DIR, VALUE 8.
           CALL "C$TOLOWER" USING INPUT-SAVE-DIR, VALUE 8.
           MOVE EXT-FILPATH-BASE    TO INPUT-PATH-BASE
                                       INPUT-SAVE-BASE.
           MOVE EXT-FILPATH-MACHINE TO INPUT-PATH-MACHINE
                                       INPUT-SAVE-MACHINE.

           PERFORM INITIALIZATION.
           IF EXT-EOBUF-ERRCD NOT = " "
              GO TO END-PROGRAM.

           MOVE INPUT-PATH TO ACCESS-BUF.
           PERFORM ACCESS-CALL.
           IF STAT NOT = "00"
              MOVE "DISCHG UPLOAD FILE NOT AVAILABLE" TO MESS
              PERFORM SEND-MESS
              GO TO END-PROGRAM.

           IF EXT-ROUTE-BUF = "00"
              MOVE "PROCESSING DISCHARGE DATE UPLOAD" TO MESS
              PERFORM SEND-LEGEND.

           PERFORM OPEN-INPUT-FILE.

           MOVE 0 TO HOLD-BRNO.
           MOVE 0 TO RECORD-COUNTER MISSING-BRANCH MISSING-LOAN-FILE
                     EXCEPTION-COUNTER UPDATED-COUNTER.
   
       NEXT-RECORD.
           PERFORM READ-INPUT-FILE-NEXT.
           IF IO-FG NOT = 0 
              PERFORM CLOSE-LN1-FILE
              PERFORM CLOSE-LM1-FILE
              PERFORM CLOSE-LTB1-FILE
              GO TO END-ROUTINE.

           ADD 1 TO RECORD-COUNTER.

           INSPECT IN-BRNOX REPLACING ALL " " BY "0".
           IF IN-BRNOX NOT NUMERIC
              GO TO NEXT-RECORD.

           IF IN-BRNO < BEG-BR OR > END-BR
              GO TO NEXT-RECORD.

           INSPECT IN-NUMBERX REPLACING ALL " " BY "0".
           IF IN-NUMBERX NOT NUMERIC
              GO TO NEXT-RECORD.

           IF IN-BRNO NOT = HOLD-BRNO
              MOVE IN-BRNO TO HOLD-BRNO BR-NO
              PERFORM SET-FILE-PATHS
              IF IO-FG NOT = 0
                 MOVE 0 TO HOLD-BRNO
                 GO TO NEXT-RECORD
              ELSE
                 PERFORM OPEN-LN1-FILE
                 PERFORM OPEN-LM1-FILE
                 PERFORM OPEN-LTB1-FILE.

           MOVE IN-NUMBER TO LN-ACCTNO.
           PERFORM READ-LN1-FILE. *> EMBEDDED - LNFILE
           IF IO-FG NOT = 0
              ADD 1 TO EXCEPTION-COUNTER
              MOVE BR-NO        TO D-BRNO
              MOVE IN-NUMBER    TO D-NUMBER
              MOVE "MISSING ACCOUNT" TO D-TEXT
              PERFORM WRITE-DETAIL-LINE
              GO TO NEXT-RECORD.
        

      * EXCEPTION IF ACCOUNT NOT BANKRUPT
           IF LN-BNKRPTDATE = 0
              ADD 1 TO EXCEPTION-COUNTER
              MOVE BR-NO        TO D-BRNO
              MOVE IN-NUMBER    TO D-NUMBER
              MOVE "ACCOUNT NOT BANKRUPT" TO D-TEXT
              PERFORM WRITE-DETAIL-LINE
              GO TO NEXT-RECORD.

      * EXCEPTION IF ACCOUNT ALREADY DISCHARGED
           MOVE LN-OWNBR  TO LTB-BRNO.
           MOVE LN-ACCTNO TO LTB-ACCTNO.
           MOVE 1         TO LTB-SEQNO.
           PERFORM READ-LTB1-FILE. *> EMBEDDED - LTBFILE
           IF IO-FG = 0 
              IF LTB-DISCHARGE-DATE NOT = 0
                 ADD 1 TO EXCEPTION-COUNTER
                 MOVE BR-NO        TO D-BRNO
                 MOVE IN-NUMBER    TO D-NUMBER
                 MOVE "ACCOUNT ALREADY DISCHARGED" TO D-TEXT
                 PERFORM WRITE-DETAIL-LINE
                 GO TO NEXT-RECORD
              ELSE
                 IF (LTB-REAFF-DATE NOT = 0 AND LTB-RESCIND-DATE = 0)
                    ADD 1 TO EXCEPTION-COUNTER
                    MOVE BR-NO     TO D-BRNO
                    MOVE IN-NUMBER TO D-NUMBER
                    MOVE "ACCOUNT HAS REAFFIRMATION DATE" TO D-TEXT
                    PERFORM WRITE-DETAIL-LINE
                    IF REPORT-ONLY = "N"
                       PERFORM DISCHARGE-MEMO-EXC
                    END-IF
                    GO TO NEXT-RECORD 
                 ELSE
                    PERFORM TEST-REWRITE
           ELSE
              PERFORM TEST-WRITE.

           ADD 1 TO UPDATED-COUNTER.

           IF REPORT-ONLY = "N"
              PERFORM CREATE-MEMO.

           GO TO NEXT-RECORD.

       END-ROUTINE.
           PERFORM PRINT-TOTALS.
           PERFORM PRINT-RANGE-LINES.

           IF REPORT-ONLY = "N"
              MOVE INPUT-PATH TO ACCESS-BUF
              PERFORM ACCESS-CALL
              IF STAT = "00"
                 MOVE HOLD-SYSDATE TO DATE-YYYYMMDD
                 PERFORM CONVERT-YYYYMMDD-TO-YYMMDD
                 MOVE DATE-YYMMDD TO INPUT-SAVE-DATE-YYMMDD
                 MOVE SPACES TO SYSTEM-BUF
                 STRING MV-CMD," ",INPUT-PATH," ",INPUT-SAVE-PATH
                        DELIMITED BY "  " INTO SYSTEM-BUF
                 PERFORM SYSTEM-CALL.

           GO TO END-PROGRAM.


      ********************************************
       DISCHARGE-MEMO-EXC SECTION.
      ********************************************
           MOVE LN-OWNBR  TO LM-BRNO.
           MOVE LN-ACCTNO TO LM-ACCTNO.
           SUBTRACT HOLD-SYSDATE FROM 99999999 GIVING LM-DATE.
           MOVE 1 TO LM-SEQ.

           MOVE IN-DATE-MMDDYY     TO DISCHARGE-MEMO-DATE.
           MOVE DISCHARGE-EXCEPT-MEMO TO LM-MEMO.
           MOVE "B" TO LM-MEMO-TYPE.

           MOVE 1 TO LM-SEQ.
       WRITE-DIS-EXCEPT-MEMO.
           PERFORM WRITE-LM1-FILE. *> EMBEDDED - LMFILE
           IF IO-FG NOT = 0
              IF LM-SEQ NOT = 999
                 ADD 1 TO LM-SEQ
                 GO TO WRITE-DIS-EXCEPT-MEMO
              ELSE
                 MOVE BR-NO TO D-BRNO
                 MOVE LN-ACCTNO TO D-NUMBER
                 MOVE "MAX LINES FOR MEMO REACHED" TO D-TEXT
                 PERFORM WRITE-DETAIL-LINE.

       DISCHARGE-MEMO-EXC-EXIT.
           EXIT.


      *******************************
       CREATE-MEMO SECTION.
      *******************************

           MOVE LN-OWNBR  TO LM-BRNO.
           MOVE LN-ACCTNO TO LM-ACCTNO.
           SUBTRACT HOLD-SYSDATE FROM 99999999 GIVING LM-DATE.
           MOVE 1 TO LM-SEQ.

           MOVE IN-DATE-MMDDYY TO WS-MEMO-NEW-DATE.
           MOVE WS-MEMO TO LM-MEMO.
           MOVE "S" TO LM-MEMO-TYPE.

           MOVE 1 TO LM-SEQ.
       WRITE-MEMO.
           PERFORM WRITE-LM1-FILE. *> EMBEDDED - LMFILE
           IF IO-FG NOT = 0
              IF LM-SEQ NOT = 999
                 ADD 1 TO LM-SEQ
                 GO TO WRITE-MEMO
              ELSE
                 MOVE BR-NO TO D-BRNO
                 MOVE LN-ACCTNO TO D-NUMBER
                 MOVE "MAX LINES FOR MEMO REACHED" TO D-TEXT
                 PERFORM WRITE-DETAIL-LINE.

      ***********************************
       INITIALIZATION SECTION.
      ***********************************
           MOVE DISIMP-QUEUE-POS TO Q-POS.
           MOVE DISIMP-COPIES-POS TO C-POS.
           MOVE DISIMP-PRU-POS   TO P-POS.
           MOVE SPACES TO Q-BUF C-BUF P-BUF.
       COPY "LIBGB/EOINIT.CPY".
 
           IF BEG-BR = 0
              IF END-BR = 0
                 MOVE BEG-BR TO SV-BEG-BR
                 MOVE END-BR TO SV-END-BR
                 MOVE EXT-CONAME-USER-LOC TO BEG-BR
                                             END-BR.


           MOVE EXT-CONAME-CONAME TO H-NAME.
           MOVE EXT-CONAME-USER-ID TO H-USERID.
           MOVE EXT-CONAME-SYSDATE TO H-DATE ALP-DATE.
           PERFORM CONVERT-ALPDATE-TO-YYYYMMDD.
           MOVE DATE-YYYYMMDD TO HOLD-SYSDATE.
               
           PERFORM OPEN-BR-FILE.
           PERFORM SQL-OPEN-UPDATE-CONNECTION.
           PERFORM OPEN-PR-FILE.

           GO TO INITIALIZE-EXIT.

       INITIALIZE-ENDIT.
           MOVE "X" TO EXT-EOBUF-ERRCD.
       INITIALIZE-EXIT.
           EXIT.

      *******************************************************
       EO-EXEC-AUDIT SECTION.
      *******************************************************
           MOVE INPUT-PATH TO ACCESS-BUF.
           PERFORM ACCESS-CALL.
           IF STAT NOT = "00"
              MOVE "DISCHG INPUT FILE MISSING" TO MESS
              PERFORM SEND-MESS
              MOVE "X" TO EXT-EOBUF-ERRCD.

      *******************************************************
       ENTER-PARAMETERS SECTION.
      *******************************************************
       ENTER-PARAM.
           IF EXT-ROUTE-BUF = "00"
              MOVE INPUT-PATH TO ACCESS-BUF
              PERFORM ACCESS-CALL
              IF STAT NOT = "00"
                 MOVE "DISCHG INPUT FILE MISSING" TO MESS
                 PERFORM SEND-MESS
                 MOVE "1>" TO VDUSTATUS
                 GO TO EXIT-PARM.

      * FOR RE-ENTRY ON F4 FROM ACCEPT.
           IF VDUSTATUS = "1U"
              GO TO ENTER-REPORT-ONLY.
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
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO BR-02.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO ENTER-REPORT-ONLY.
           MOVE VDUBUF TO REPORT-ONLY.
           IF REPORT-ONLY NOT = "Y" AND NOT = "N"
              GO TO ENTER-REPORT-ONLY.

       EXIT-PARM.
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
           MOVE "S  A010REPONLY." TO VDU.
           MOVE REPORT-ONLY TO VDUBUF.
           PERFORM SCREENIO.


      *************************************
       PRINT-TOTALS SECTION.
      *************************************
           MOVE 4 TO LN-FEED.

           MOVE "TOTAL RECORDS IN............" TO RANGE-SELECTION.
           MOVE RECORD-COUNTER TO RANGE1N.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "TOTAL UPDATED............" TO RANGE-SELECTION.
           MOVE UPDATED-COUNTER TO RANGE1N.
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "MISSING BRANCH RECORDS......" TO RANGE-SELECTION.
           MOVE MISSING-BRANCH TO RANGE1N.
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "FILES ON OTHER MACHINE......" TO RANGE-SELECTION.
           MOVE MISSING-LOAN-FILE TO RANGE1N.
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "TOTAL EXCEPTIONS............" TO RANGE-SELECTION.
           MOVE EXCEPTION-COUNTER TO RANGE1N.
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.
           MOVE 1 TO LN-FEED.
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
           MOVE "REPORT ONLY" TO RANGE-SELECTION.
           MOVE REPORT-ONLY                      TO RANGE1
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
 
      *********************************************
       FORM-RESET SECTION.
      *********************************************
           MOVE EXT-CONAME-CONAME TO DISIMP-CONAME.
           MOVE EXT-CONAME-SYSDATE TO DISIMP-SYSDATE.
           DISPLAY DISIMP-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE DISIMP-FORM-FIELDS TO WR-BUF.
           MOVE DISIMP-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.


      *********************************************
       SETUP-LINK-INFO SECTION.
      *********************************************
           MOVE DISIMP-HELP--FILE TO HELP-LINK-FILE.
           MOVE DISIMP-HELP--DIR  TO HELP-LINK-DIR.

      *********************************************
       VDU-CONVERT-FIELD SECTION.
      *********************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE  
              COPY "LIBSP/DISIMP_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

       COPY "LIBGB/TIMEIO.CPY".
       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBLP/LPLTBCL.CPY".
 
      **************************************
       MISC-PARA SECTION.
      **************************************
       COPY "LIBGB/GETPR.CPY".
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/SYSTEM.CPY".
       TEST-REWRITE.
           IF REPORT-ONLY = "N"
              MOVE IN-DATE-MMDDYY TO DATE-MMDDYY
              PERFORM CONVERT-MMDDYY-TO-YYYYMMDD
              MOVE DATE-YYYYMMDD  TO LTB-DISCHARGE-DATE
              MOVE HOLD-SYSDATE   TO LTB-DISCHARGE-SYSDATE
              PERFORM REWRITE-LTB1-FILE. *> EMBEDDED - LTBFILE
       TEST-WRITE.
           IF REPORT-ONLY = "N"
              PERFORM CLEAR-LTB-FILE
              MOVE LN-OWNBR  TO LTB-BRNO
              MOVE LN-ACCTNO TO LTB-ACCTNO
              MOVE 1         TO LTB-SEQNO
              MOVE IN-DATE-MMDDYY TO DATE-MMDDYY
              PERFORM CONVERT-MMDDYY-TO-YYYYMMDD
              MOVE DATE-YYYYMMDD  TO LTB-DISCHARGE-DATE
              MOVE HOLD-SYSDATE   TO LTB-DISCHARGE-SYSDATE
              PERFORM WRITE-LTB1-FILE. *> EMBEDDED - LTBFILE
       SEND-LEGEND.
           MOVE MESS TO WR-BUF.
           MOVE MESS-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.
       SET-FILE-PATHS.
           PERFORM CLOSE-LN1-FILE.
           PERFORM CLOSE-LM1-FILE.
           PERFORM CLOSE-LTB1-FILE.

           PERFORM READ-BR-FILE.
           IF IO-FG = 0
              MOVE EXT-FILPATH-BASE TO FILPATH-BASE
              MOVE BR-MACHINE       TO FILPATH-MACHINE
              MOVE BR-DATA-PATH     TO FILPATH-DATA

              MOVE "B" TO LN-PATH-OVERRIDE
                          LM-PATH-OVERRIDE
                          LTB-PATH-OVERRIDE
           ELSE
              MOVE IN-BRNO   TO D-BRNO
              MOVE IN-NUMBER TO D-NUMBER
              MOVE "INVALID BRANCH NO." TO D-TEXT
              ADD 1 TO MISSING-BRANCH EXCEPTION-COUNTER
              PERFORM WRITE-DETAIL-LINE
              MOVE 9 TO IO-FG.
 
      **************************************************
       IO-ROUTINES SECTION.
      **************************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBLP/LPLTBGS_SQL.CPY".
       COPY "LIBLP/LPLNGS_SQL.CPY".
       COPY "LIBLP/LPLMGS_SQL.CPY".
       COPY "LIBGB/GBGRGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBPRINT.CPY".
       OPEN-INPUT-FILE.
           PERFORM OPEN-IT.
           MOVE INPUT-PATH TO E-FILE.
           OPEN INPUT INPUT-FILE.
           IF IO-FG = 8
              GO TO OPEN-INPUT-FILE.
           IF IO-FG = 7
              CLOSE INPUT-FILE
              GO TO OPEN-INPUT-FILE.
       READ-INPUT-FILE-NEXT.
           PERFORM READ-IT.
           MOVE INPUT-PATH TO E-FILE.
           READ INPUT-FILE NEXT.
           IF IO-FG = 8
              GO TO READ-INPUT-FILE-NEXT.
       CLOSE-INPUT-FILE.
           PERFORM CLOSE-IT.
           CLOSE INPUT-FILE.

       COPY "LIBGB/GBBRRN.CPY".

      * COPY "LIBLP/LPLN1RN.CPY".
      * COPYMEMBER: LIBLP/LPLN1RN
       LOAD-LN1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( LN-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( LN-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO LN-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO LN-ALL-MACHINE
              MOVE EXT-FILPATH-DATA        TO LN-ALL-DATA
              MOVE LN-ALL-PATH             TO LN-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( LN-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO LN-ALL-BASE
              MOVE FILPATH-MACHINE         TO LN-ALL-MACHINE
              MOVE FILPATH-DATA            TO LN-ALL-DATA
              MOVE LN-ALL-PATH             TO LN-PATH.

      *-----------------------------------------------------------------
       OPEN-LN1-FILE.
           PERFORM LOAD-LN1-FILE.
           PERFORM OPEN-IT.
           MOVE LN-PATH-SQL TO E-FILE.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       READ-LN1-FILE. *> EMBEDDED - LNFILE
           PERFORM READ-IT.
           MOVE LN-PATH-OWNBR  TO LN-OWNBR.
           MOVE LN-PATH-SQL    TO E-FILE.
           MOVE LN1-KEY        TO E-KEYX.
           INITIALIZE QLN-REC.

           MOVE LN-OWNBR  TO QLN-OWNBR.
           MOVE LN-ACCTNO TO QLN-ACCTNO.

           EXEC SQL
            SELECT LNFILE.LN_OWNBR,
                   LNFILE.LN_ACCTNO,
                   CAST(LNFILE.LN_BNKRPTDATE AS VARCHAR(10)) 
              INTO  
                  :QLN-OWNBR,
                  :QLN-ACCTNO,
                  :QLN-BNKRPTDATE
              FROM DBO.LNFILE
             WHERE LNFILE.LN_OWNBR  = :QLN-OWNBR
               AND LNFILE.LN_ACCTNO = :QLN-ACCTNO
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-LN1-FILE.

           IF ( IO-FG = 0 )
              PERFORM GET-LN-FIELDS.

      *-----------------------------------------------------------------
       CLOSE-LN1-FILE.
           PERFORM CLOSE-IT.
 

      * COPY "LIBLP/LPLM1IN.CPY".
      * COPYMEMBER: LIBLP/LPLM1IN
       LOAD-LM1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( LM-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( LM-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO LM-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO LM-ALL-MACHINE
              MOVE EXT-FILPATH-DATA        TO LM-ALL-DATA
              MOVE LM-ALL-PATH             TO LM-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( LM-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO LM-ALL-BASE
              MOVE FILPATH-MACHINE         TO LM-ALL-MACHINE
              MOVE FILPATH-DATA            TO LM-ALL-DATA
              MOVE LM-ALL-PATH             TO LM-PATH.

      *-----------------------------------------------------------------
       OPEN-LM1-FILE.
           PERFORM LOAD-LM1-FILE.
           PERFORM OPEN-IT.
           MOVE LM-PATH-SQL TO E-FILE.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       WRITE-LM1-FILE. *> EMBEDDED - LMFILE

           MOVE LM-PATH-OWNBR  TO LM-BRNO.
           ACCEPT LM-LTOUCH-DATE FROM CENTURY-DATE.
           PERFORM WRITE-IT.
           MOVE LM-PATH-SQL    TO E-FILE.
           MOVE LM1-KEY        TO E-KEYX.
           PERFORM SET-LM-FIELDS.

           MOVE '2' TO SQL-UPDATE-CONN-USE-SW. *> USE UPDATE_CONNECT
           PERFORM SQL-SWAP-UPDATE-CONNECTION.

           EXEC SQL
            INSERT INTO DBO.LMFILE
             (LMFILE.LM_BRNO,
              LMFILE.LM_ACCTNO,
              LMFILE.LM_DATE_REVERSED,
              LMFILE.LM_SEQ,
              LMFILE.LM_LTOUCH_DATE,
              LMFILE.LM_LTOUCH_TIME_HHMM,
              LMFILE.LM_LTOUCH_USERID,
              LMFILE.LM_MEMO,
              LMFILE.LM_MEMO_TYPE)
            VALUES
             (:QLM-BRNO,
              :QLM-ACCTNO,
              :QLM-DATE-REVERSED,
              :QLM-SEQ,
              :QLM-LTOUCH-DATE,
              :QLM-LTOUCH-TIME-HHMM,
              :QLM-LTOUCH-USERID,
              :QLM-MEMO,
              :QLM-MEMO-TYPE)
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO WRITE-LM1-FILE.

      *    IF ( IO-FG = 0 )
      *       EXEC SQL COMMIT END-EXEC
      *       PERFORM SQL-IO-VALIDATION.

           MOVE  '1' TO SQL-UPDATE-CONN-USE-SW. *> USE C1 CONNECT
           PERFORM SQL-SWAP-UPDATE-CONNECTION.

      *-----------------------------------------------------------------
       REWRITE-LM1-FILE. *> EMBEDDED - LMFILE

           MOVE LM-PATH-OWNBR  TO LM-BRNO.
           ACCEPT LM-LTOUCH-DATE FROM CENTURY-DATE.
           PERFORM REWRITE-IT.
           MOVE LM-PATH-SQL    TO E-FILE.
           MOVE LM1-KEY        TO E-KEYX.
           PERFORM SET-LM-FIELDS.

           MOVE '2' TO SQL-UPDATE-CONN-USE-SW. *> USE UPDATE_CONNECT
           PERFORM SQL-SWAP-UPDATE-CONNECTION.

           EXEC SQL
            UPDATE DBO.LMFILE
             SET
              LMFILE.LM_BRNO = :QLM-BRNO,
              LMFILE.LM_ACCTNO = :QLM-ACCTNO,
              LMFILE.LM_DATE_REVERSED = :QLM-DATE-REVERSED,
              LMFILE.LM_SEQ = :QLM-SEQ,
              LMFILE.LM_LTOUCH_DATE = :QLM-LTOUCH-DATE,
              LMFILE.LM_LTOUCH_TIME_HHMM = :QLM-LTOUCH-TIME-HHMM,
              LMFILE.LM_LTOUCH_USERID = :QLM-LTOUCH-USERID,
              LMFILE.LM_MEMO = :QLM-MEMO,
              LMFILE.LM_MEMO_TYPE = :QLM-MEMO-TYPE
             WHERE LMFILE.LM_BRNO = :QLM-BRNO
               AND LMFILE.LM_ACCTNO = :QLM-ACCTNO
               AND LMFILE.LM_DATE_REVERSED = :QLM-DATE-REVERSED
               AND LMFILE.LM_SEQ = :QLM-SEQ
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO REWRITE-LM1-FILE.

      *    IF ( IO-FG = 0 )
      *       EXEC SQL COMMIT END-EXEC
      *       PERFORM SQL-IO-VALIDATION.

           MOVE  '1' TO SQL-UPDATE-CONN-USE-SW. *> USE C1 CONNECT
           PERFORM SQL-SWAP-UPDATE-CONNECTION.

      *-----------------------------------------------------------------
       CLOSE-LM1-FILE.

           PERFORM CLOSE-IT.


      * COPY "LIBLP/LPLTB1IN.CPY".
      * COPYMEMBER: LIBLP/LPLTB1IN
      * REV:
       LOAD-LTB1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( LTB-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( LTB-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO LTB-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO LTB-ALL-MACHINE
              MOVE EXT-FILPATH-DATA        TO LTB-ALL-DATA
              MOVE LTB-ALL-PATH             TO LTB-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( LTB-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO LTB-ALL-BASE
              MOVE FILPATH-MACHINE         TO LTB-ALL-MACHINE
              MOVE FILPATH-DATA            TO LTB-ALL-DATA
              MOVE LTB-ALL-PATH             TO LTB-PATH.

      *-----------------------------------------------------------------
       OPEN-LTB1-FILE.
           PERFORM LOAD-LTB1-FILE.
           PERFORM OPEN-IT.
           MOVE LTB-PATH-SQL TO E-FILE.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       READ-LTB1-FILE. *> EMBEDDED - LTBFILE
           PERFORM READ-IT.
           MOVE LTB-PATH-OWNBR  TO LTB-BRNO. 
           MOVE LTB-PATH-SQL    TO E-FILE.
           MOVE LTB1-KEY        TO E-KEYX.
           INITIALIZE QLTB-REC.

           MOVE LTB-BRNO   TO QLTB-BRNO.
           MOVE LTB-ACCTNO TO QLTB-ACCTNO.
           MOVE LTB-SEQNO  TO QLTB-SEQNO.

           EXEC SQL
            SELECT LTBFILE.LTB_BRNO,
                   LTBFILE.LTB_ACCTNO,
                   LTBFILE.LTB_SEQNO,
                   CAST(LTBFILE.LTB_LTOUCH_DATE
                                                  AS VARCHAR(10)),
                   CAST(LTBFILE.LTB_FILEDATE
                                                  AS VARCHAR(10)),
                   LTBFILE.LTB_CASE_NO,
                   LTBFILE.LTB_CHAPTER,
                   LTBFILE.LTB_MISC1,
                   LTBFILE.LTB_MISC2,
                   LTBFILE.LTB_MISC3,
                   CAST(LTBFILE.LTB_BDDATE AS VARCHAR(10)),
                   LTBFILE.LTB_WHO_FILED,
                   LTBFILE.LTB_ENDORSER_FILED,
                   LTBFILE.LTB_CLAIM_AMT,
                   LTBFILE.LTB_ACTIVE_INACTIVE,
                   LTBFILE.LTB_INT_LC_DUE,
                   LTBFILE.LTB_OTH_DUE,
                   LTBFILE.LTB_TDUEAMT,
                   LTBFILE.LTB_NAME_1,
                   LTBFILE.LTB_ADR1_1,
                   LTBFILE.LTB_ADR2_1,
                   LTBFILE.LTB_CITY_1,
                   LTBFILE.LTB_STATE_1,
                   LTBFILE.LTB_ZIP_1,
                   LTBFILE.LTB_PHONE_1,
                   LTBFILE.LTB_NAME_2,
                   LTBFILE.LTB_ADR1_2,
                   LTBFILE.LTB_ADR2_2,
                   LTBFILE.LTB_CITY_2,
                   LTBFILE.LTB_STATE_2,
                   LTBFILE.LTB_ZIP_2,
                   LTBFILE.LTB_PHONE_2,
                   LTBFILE.LTB_NAME_3,
                   LTBFILE.LTB_ADR1_3,
                   LTBFILE.LTB_ADR2_3,
                   LTBFILE.LTB_CITY_3,
                   LTBFILE.LTB_STATE_3,
                   LTBFILE.LTB_ZIP_3,
                   LTBFILE.LTB_PHONE_3,
                   CAST(LTBFILE.LTB_REAFF_DATE
                                                  AS VARCHAR(10)),
                   CAST(LTBFILE.LTB_REAFF_SYSDATE
                                                  AS VARCHAR(10)),
                   LTBFILE.LTB_REAFF_BALANCE,
                   LTBFILE.LTB_REAFF_PAYAMT,
                   LTBFILE.LTB_REAFF_NO_PAYS,
                   LTBFILE.LTB_INTRATE,
                   CAST(LTBFILE.LTB_RESCIND_DATE
                                                  AS VARCHAR(10)),
                   LTBFILE.LTB_PARTIAL_FULL,
                   CAST(LTBFILE.LTB_DISCHARGE_DATE
                                                  AS VARCHAR(10)),
                   CAST(LTBFILE.LTB_DISCHARGE_SYSDATE
                                                  AS VARCHAR(10)),
                   LTBFILE.LTB_GROSS_BAL
             INTO 
                 :QLTB-BRNO,
                 :QLTB-ACCTNO,
                 :QLTB-SEQNO,
                 :QLTB-LTOUCH-DATE,
                 :QLTB-FILEDATE,
                 :QLTB-CASE-NO,
                 :QLTB-CHAPTER,
                 :QLTB-MISC1,
                 :QLTB-MISC2,
                 :QLTB-MISC3,
                 :QLTB-BDDATE,
                 :QLTB-WHO-FILED,
                 :QLTB-ENDORSER-FILED,
                 :QLTB-CLAIM-AMT,
                 :QLTB-ACTIVE-INACTIVE,
                 :QLTB-INT-LC-DUE,
                 :QLTB-OTH-DUE,
                 :QLTB-TDUEAMT,
                 :QLTB-NAME-1,
                 :QLTB-ADR1-1,
                 :QLTB-ADR2-1,
                 :QLTB-CITY-1,
                 :QLTB-STATE-1,
                 :QLTB-ZIP-1,
                 :QLTB-PHONE-1,
                 :QLTB-NAME-2,
                 :QLTB-ADR1-2,
                 :QLTB-ADR2-2,
                 :QLTB-CITY-2,
                 :QLTB-STATE-2,
                 :QLTB-ZIP-2,
                 :QLTB-PHONE-2,
                 :QLTB-NAME-3,
                 :QLTB-ADR1-3,
                 :QLTB-ADR2-3,
                 :QLTB-CITY-3,
                 :QLTB-STATE-3,
                 :QLTB-ZIP-3,
                 :QLTB-PHONE-3,
                 :QLTB-REAFF-DATE,
                 :QLTB-REAFF-SYSDATE,
                 :QLTB-REAFF-BALANCE,
                 :QLTB-REAFF-PAYAMT,
                 :QLTB-REAFF-NO-PAYS,
                 :QLTB-INTRATE,
                 :QLTB-RESCIND-DATE,
                 :QLTB-PARTIAL-FULL,
                 :QLTB-DISCHARGE-DATE,
                 :QLTB-DISCHARGE-SYSDATE,
                 :QLTB-GROSS-BAL
             FROM DBO.LTBFILE
            WHERE LTBFILE.LTB_BRNO   = :QLTB-BRNO
              AND LTBFILE.LTB_ACCTNO = :QLTB-ACCTNO
              AND LTBFILE.LTB_SEQNO  = :QLTB-SEQNO
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-LTB1-FILE.

           IF ( IO-FG = 0 )
              PERFORM GET-LTB-FIELDS.

      *-----------------------------------------------------------------
       WRITE-LTB1-FILE. *> EMBEDDED - LTBFILE

           MOVE LTB-PATH-OWNBR  TO LTB-BRNO.
           ACCEPT LTB-LTOUCH-DATE FROM CENTURY-DATE.
           PERFORM WRITE-IT.
           MOVE LTB-PATH-SQL    TO E-FILE.
           MOVE LTB1-KEY        TO E-KEYX.
           PERFORM SET-LTB-FIELDS.

           MOVE '2' TO SQL-UPDATE-CONN-USE-SW. *> USE UPDATE_CONNECT
           PERFORM SQL-SWAP-UPDATE-CONNECTION.

           EXEC SQL
            INSERT INTO DBO.LTBFILE
             (LTBFILE.LTB_BRNO,
              LTBFILE.LTB_ACCTNO,
              LTBFILE.LTB_SEQNO,
              LTBFILE.LTB_LTOUCH_DATE,
              LTBFILE.LTB_FILEDATE,
              LTBFILE.LTB_CASE_NO,
              LTBFILE.LTB_CHAPTER,
              LTBFILE.LTB_MISC1,
              LTBFILE.LTB_MISC2,
              LTBFILE.LTB_MISC3,
              LTBFILE.LTB_BDDATE,
              LTBFILE.LTB_WHO_FILED,
              LTBFILE.LTB_ENDORSER_FILED,
              LTBFILE.LTB_CLAIM_AMT,
              LTBFILE.LTB_ACTIVE_INACTIVE,
              LTBFILE.LTB_INT_LC_DUE,
              LTBFILE.LTB_OTH_DUE,
              LTBFILE.LTB_TDUEAMT,
              LTBFILE.LTB_NAME_1,
              LTBFILE.LTB_ADR1_1,
              LTBFILE.LTB_ADR2_1,
              LTBFILE.LTB_CITY_1,
              LTBFILE.LTB_STATE_1,
              LTBFILE.LTB_ZIP_1,
              LTBFILE.LTB_PHONE_1,
              LTBFILE.LTB_NAME_2,
              LTBFILE.LTB_ADR1_2,
              LTBFILE.LTB_ADR2_2,
              LTBFILE.LTB_CITY_2,
              LTBFILE.LTB_STATE_2,
              LTBFILE.LTB_ZIP_2,
              LTBFILE.LTB_PHONE_2,
              LTBFILE.LTB_NAME_3,
              LTBFILE.LTB_ADR1_3,
              LTBFILE.LTB_ADR2_3,
              LTBFILE.LTB_CITY_3,
              LTBFILE.LTB_STATE_3,
              LTBFILE.LTB_ZIP_3,
              LTBFILE.LTB_PHONE_3,
              LTBFILE.LTB_REAFF_DATE,
              LTBFILE.LTB_REAFF_SYSDATE,
              LTBFILE.LTB_REAFF_BALANCE,
              LTBFILE.LTB_REAFF_PAYAMT,
              LTBFILE.LTB_REAFF_NO_PAYS,
              LTBFILE.LTB_INTRATE,
              LTBFILE.LTB_RESCIND_DATE,
              LTBFILE.LTB_PARTIAL_FULL,
              LTBFILE.LTB_DISCHARGE_DATE,
              LTBFILE.LTB_DISCHARGE_SYSDATE,
              LTBFILE.LTB_GROSS_BAL)
            VALUES
             (:QLTB-BRNO,
              :QLTB-ACCTNO,
              :QLTB-SEQNO,
              :QLTB-LTOUCH-DATE,
              :QLTB-FILEDATE,
              :QLTB-CASE-NO,
              :QLTB-CHAPTER,
              :QLTB-MISC1,
              :QLTB-MISC2,
              :QLTB-MISC3,
              :QLTB-BDDATE,
              :QLTB-WHO-FILED,
              :QLTB-ENDORSER-FILED,
              :QLTB-CLAIM-AMT,
              :QLTB-ACTIVE-INACTIVE,
              :QLTB-INT-LC-DUE,
              :QLTB-OTH-DUE,
              :QLTB-TDUEAMT,
              :QLTB-NAME-1,
              :QLTB-ADR1-1,
              :QLTB-ADR2-1,
              :QLTB-CITY-1,
              :QLTB-STATE-1,
              :QLTB-ZIP-1,
              :QLTB-PHONE-1,
              :QLTB-NAME-2,
              :QLTB-ADR1-2,
              :QLTB-ADR2-2,
              :QLTB-CITY-2,
              :QLTB-STATE-2,
              :QLTB-ZIP-2,
              :QLTB-PHONE-2,
              :QLTB-NAME-3,
              :QLTB-ADR1-3,
              :QLTB-ADR2-3,
              :QLTB-CITY-3,
              :QLTB-STATE-3,
              :QLTB-ZIP-3,
              :QLTB-PHONE-3,
              :QLTB-REAFF-DATE,
              :QLTB-REAFF-SYSDATE,
              :QLTB-REAFF-BALANCE,
              :QLTB-REAFF-PAYAMT,
              :QLTB-REAFF-NO-PAYS,
              :QLTB-INTRATE,
              :QLTB-RESCIND-DATE,
              :QLTB-PARTIAL-FULL,
              :QLTB-DISCHARGE-DATE,
              :QLTB-DISCHARGE-SYSDATE,
              :QLTB-GROSS-BAL)
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO WRITE-LTB1-FILE.

      *    IF ( IO-FG = 0 )
      *       EXEC SQL COMMIT END-EXEC
      *       PERFORM SQL-IO-VALIDATION.

           MOVE  '1' TO SQL-UPDATE-CONN-USE-SW. *> USE C1 CONNECT
           PERFORM SQL-SWAP-UPDATE-CONNECTION.

      *-----------------------------------------------------------------
       REWRITE-LTB1-FILE. *> EMBEDDED - LTBFILE

           MOVE LTB-PATH-OWNBR  TO LTB-BRNO.
           ACCEPT LTB-LTOUCH-DATE FROM CENTURY-DATE.
           PERFORM REWRITE-IT.
           MOVE LTB-PATH-SQL    TO E-FILE.
           MOVE LTB1-KEY        TO E-KEYX.
           PERFORM SET-LTB-FIELDS.

           MOVE '2' TO SQL-UPDATE-CONN-USE-SW. *> USE UPDATE_CONNECT
           PERFORM SQL-SWAP-UPDATE-CONNECTION.

           EXEC SQL
            UPDATE DBO.LTBFILE
             SET
              LTBFILE.LTB_BRNO = :QLTB-BRNO,
              LTBFILE.LTB_ACCTNO = :QLTB-ACCTNO,
              LTBFILE.LTB_SEQNO = :QLTB-SEQNO,
              LTBFILE.LTB_LTOUCH_DATE =
                 :QLTB-LTOUCH-DATE,
              LTBFILE.LTB_FILEDATE =
                 :QLTB-FILEDATE,
              LTBFILE.LTB_CASE_NO = :QLTB-CASE-NO,
              LTBFILE.LTB_CHAPTER = :QLTB-CHAPTER,
              LTBFILE.LTB_MISC1 = :QLTB-MISC1,
              LTBFILE.LTB_MISC2 = :QLTB-MISC2,
              LTBFILE.LTB_MISC3 = :QLTB-MISC3,
              LTBFILE.LTB_BDDATE =
                 :QLTB-BDDATE,
              LTBFILE.LTB_WHO_FILED = :QLTB-WHO-FILED,
              LTBFILE.LTB_ENDORSER_FILED = :QLTB-ENDORSER-FILED,
              LTBFILE.LTB_CLAIM_AMT = :QLTB-CLAIM-AMT,
              LTBFILE.LTB_ACTIVE_INACTIVE = :QLTB-ACTIVE-INACTIVE,
              LTBFILE.LTB_INT_LC_DUE = :QLTB-INT-LC-DUE,
              LTBFILE.LTB_OTH_DUE = :QLTB-OTH-DUE,
              LTBFILE.LTB_TDUEAMT = :QLTB-TDUEAMT,
              LTBFILE.LTB_NAME_1 = :QLTB-NAME-1,
              LTBFILE.LTB_ADR1_1 = :QLTB-ADR1-1,
              LTBFILE.LTB_ADR2_1 = :QLTB-ADR2-1,
              LTBFILE.LTB_CITY_1 = :QLTB-CITY-1,
              LTBFILE.LTB_STATE_1 = :QLTB-STATE-1,
              LTBFILE.LTB_ZIP_1 = :QLTB-ZIP-1,
              LTBFILE.LTB_PHONE_1 = :QLTB-PHONE-1,
              LTBFILE.LTB_NAME_2 = :QLTB-NAME-2,
              LTBFILE.LTB_ADR1_2 = :QLTB-ADR1-2,
              LTBFILE.LTB_ADR2_2 = :QLTB-ADR2-2,
              LTBFILE.LTB_CITY_2 = :QLTB-CITY-2,
              LTBFILE.LTB_STATE_2 = :QLTB-STATE-2,
              LTBFILE.LTB_ZIP_2 = :QLTB-ZIP-2,
              LTBFILE.LTB_PHONE_2 = :QLTB-PHONE-2,
              LTBFILE.LTB_NAME_3 = :QLTB-NAME-3,
              LTBFILE.LTB_ADR1_3 = :QLTB-ADR1-3,
              LTBFILE.LTB_ADR2_3 = :QLTB-ADR2-3,
              LTBFILE.LTB_CITY_3 = :QLTB-CITY-3,
              LTBFILE.LTB_STATE_3 = :QLTB-STATE-3,
              LTBFILE.LTB_ZIP_3 = :QLTB-ZIP-3,
              LTBFILE.LTB_PHONE_3 = :QLTB-PHONE-3,
              LTBFILE.LTB_REAFF_DATE =
                 :QLTB-REAFF-DATE,
              LTBFILE.LTB_REAFF_SYSDATE =
                 :QLTB-REAFF-SYSDATE,
              LTBFILE.LTB_REAFF_BALANCE = :QLTB-REAFF-BALANCE,
              LTBFILE.LTB_REAFF_PAYAMT = :QLTB-REAFF-PAYAMT,
              LTBFILE.LTB_REAFF_NO_PAYS = :QLTB-REAFF-NO-PAYS,
              LTBFILE.LTB_INTRATE = :QLTB-INTRATE,
              LTBFILE.LTB_RESCIND_DATE =
                 :QLTB-RESCIND-DATE,
              LTBFILE.LTB_PARTIAL_FULL = :QLTB-PARTIAL-FULL,
              LTBFILE.LTB_DISCHARGE_DATE =
                 :QLTB-DISCHARGE-DATE,
              LTBFILE.LTB_DISCHARGE_SYSDATE = :QLTB-DISCHARGE-SYSDATE,
              LTBFILE.LTB_GROSS_BAL = :QLTB-GROSS-BAL
             WHERE LTBFILE.LTB_BRNO = :QLTB-BRNO
               AND LTBFILE.LTB_ACCTNO = :QLTB-ACCTNO
               AND LTBFILE.LTB_SEQNO = :QLTB-SEQNO
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO REWRITE-LTB1-FILE.

      *    IF ( IO-FG = 0 )
      *       EXEC SQL COMMIT END-EXEC
      *       PERFORM SQL-IO-VALIDATION.

           MOVE  '1' TO SQL-UPDATE-CONN-USE-SW. *> USE C1 CONNECT
           PERFORM SQL-SWAP-UPDATE-CONNECTION.

      *-----------------------------------------------------------------
       CLOSE-LTB1-FILE.

           PERFORM CLOSE-IT.


       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO EXT-EOBUF-ERRCD.
 
      **************************************************
       END-PROGRAM SECTION.
      **************************************************
       END-RUN.
           PERFORM CLOSE-FILES.
           IF EXT-ROUTE-BUF = "00"
              PERFORM PRU-OUT.

       EXIT-PROG.
           IF SV-BEG-BR NOT = 9999
              MOVE SV-BEG-BR TO BEG-BR
              MOVE SV-END-BR TO END-BR.

           IF EXT-ROUTE-BUF = "00"
              MOVE "LP/LPMENU" TO FORM-NAM
              MOVE FORM-PATH TO FORM-PATHNAME.
              
       STOP-RUN.
           IF EXT-ROUTE-BUF = "00" OR "40" OR "50"
              DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN
              DESTROY DISIMP-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.
           EXIT PROGRAM.
