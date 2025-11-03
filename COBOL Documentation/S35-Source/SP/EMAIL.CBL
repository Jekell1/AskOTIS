       IDENTIFICATION     DIVISION.
       PROGRAM-ID.          EMAIL.
       AUTHOR.                 BAH.
       DATE-WRITTEN.    12/06/2004.
      ******************************************************************
      *
      * DESCRIPTION:  UPLOAD EMAIL INTO BORROWER TRAILER RECORD
      * WORLD WANTS THIS KEPT
      *
      * REV:
      * 041206 BAH NEW FOR REGACC, PR# 92
      * KEC 2018.0511 REMOVED USE OF ENT-GROUP AND/OR GROUP-FLAG.
      *         THIS WAS DONE IN RELATION TO THE EOBUF EXTERNAL ISSUE
      *         WHICH IF NOT BEING SET WILL CAUSE ISSUES FROM THOSE
      *         BYTE POSITIONS COULD BE SET FROM PREVIOUS
      *         PROGRAM WITH EOBUF THAT SET SOMETHING IN THOSE BYTES.
      * BAH 181004 GLOBAL BWFILE
      * BAH 20191007 REMOVED ACCESS-CALL BWFILE
      *
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "LIBGB/GBFSPR.CPY".
           SELECT EMAIL-FILE ASSIGN EMAIL-PATH
                  ORGANIZATION LINE SEQUENTIAL
                  ACCESS SEQUENTIAL
                  LOCK MODE AUTOMATIC WITH LOCK ON RECORD
                  FILE STATUS FILE-STAT.

       I-O-CONTROL.
       DATA DIVISION.
       FILE SECTION.

       COPY "LIBGB/GBFDPR.CPY".
       FD  EMAIL-FILE
           LABEL RECORDS ARE STANDARD.
       01  EMAIL-REC.
           03  EMAIL-BRNO                  PIC 9(4).
           03  EMAIL-BRNOX REDEFINES EMAIL-BRNO PIC X(4).
           03  EMAIL-SSNO                  PIC 9(9).
           03  EMAIL-SSNOX REDEFINES EMAIL-SSNO PIC X(9).
           03  EMAIL-EMAIL                 PIC X(40).

       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(40) VALUE
           "@(#)SP/EMAIL.CBL S35 02/21/24-13:20:36 >".
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBGB/GB01GR.CPY".
       COPY "LIBGB/GB01GR_SQL.CPY".
       COPY "LIBGB/GBWSGR.CPY".
       COPY "LIBLP/LP01BW.CPY".
       COPY "LIBLP/LP01BW_SQL.CPY".
       COPY "LIBLP/LPWSBW.CPY".

       01  EMAIL-PATH.
           03  EMAIL-PATH-BASE          PIC X(09).
           03  FILLER                   PIC X(01)   VALUE "/".
           03  EMAIL-PATH-MACHINE       PIC X(02).
           03  FILLER                   PIC X(28)   VALUE "/SP/EMAIL".

       01  HOLD-BRNO            PIC 9(4) VALUE 0.

      *-----------------------------------------------------------------
      * LINE WORKERS
      *-----------------------------------------------------------------
       01  LN-WORKERS.
           03  LN-FEED          PIC 999      VALUE 0.
           03  LN-COUNT         PIC 999      VALUE 99.
           03  LN-WK            PIC 999      VALUE 0.
       01  PAGE-NUMBER          PIC 9(4)     VALUE 0.

       01  GRAND-TOTAL-RECS     PIC 9(10)       VALUE 0.

       COPY "LIBGB/EOBUF_EXT.CPY".
           03  BEG-BR          PIC 9(04).
           03  END-BR          PIC 9(04).
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
           "   BORROWER EMAIL UPLOAD   ".
       01  HEAD3.
           03  FILLER           PIC X(09)    VALUE "BRANCH NO".
           03  FILLER           PIC X(03)    VALUE SPACES.
           03  FILLER           PIC X(13)   VALUE "TOTAL RECORDS".
           03  FILLER           PIC X(03)    VALUE SPACES.
           03  FILLER           PIC X(12)   VALUE "            ".
           03  FILLER           PIC X(03)    VALUE SPACES.
           03  FILLER           PIC X(11)   VALUE "           ".


       01  DETAIL-LINE          PIC X(80) VALUE SPACES.
       01  DETAIL-LINE1 REDEFINES DETAIL-LINE.
           03  D-BRNO           PIC Z(4).
           03  FILLER           PIC X(2).
           03  D-SSNO           PIC 999/99/9999.
           03  FILLER           PIC X(2).
           03  D-MSG1           PIC X(40).

       01  DETAIL-LINE2 REDEFINES DETAIL-LINE.
           03  FILLER           PIC X(6).
           03  D-TOTAL-TAG      PIC X(15).
           03  FILLER           PIC X(08).
           03  D-TOTAL-REC      PIC Z(8)9.

       01  RANGE-LINE REDEFINES DETAIL-LINE.
           03  RANGE-SELECTION  PIC X(30).
           03  FILLER           PIC X(05).
           03  RANGE1N          PIC Z(09)9.

       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBGB/GETPRW.CPY".
      *  "/$REL/SH/EC /$REL/SH/EMAIL.SH".
       01  EMAIL-CMD.
           03  FILLER           PIC X    VALUE "/".
           03  EMAIL-REL        PIC X(7).
           03  FILLER           PIC X(4) VALUE "/SH/".
           03  EMAIL-EC         PIC X(3) VALUE "EC ".
           03  FILLER           PIC X    VALUE "/".
           03  EMAIL-REL2       PIC X(7).
           03  FILLER           PIC X(4) VALUE "/SH/".
           03  EMAIL-SHELL      PIC X(10) VALUE "EMAIL.SH ".
           03  EMAIL-FIL        PIC X(11).


       COPY "LIBGB/SYSTEMW.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBSP/EMAIL_DEF.CPY".
       COPY "LIBSP/EMAIL_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

      *****************************************************************
      *
      *     P R O G R A M   C O N T R O L
      *
      *****************************************************************
       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBSP/EMAIL_SCN.CPY".

       PROCEDURE DIVISION
           USING FORM-PATHNAME EXIT-PATHNAME.

       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
               PR-FILE EMAIL-FILE.
       COPY "LIBGB/DECLRP.CPY".
           PERFORM CLOSE-BW1-FILE.
           PERFORM CLOSE-BR-FILE.
           PERFORM CLOSE-PR-FILE.
           CLOSE
               EMAIL-FILE.
       COPY "LIBGB/DECLRP2.CPY".

      *********************************************
       MAIN-MODULE SECTION.
      *********************************************
           PERFORM SQL-CONNECT.

           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "EMAIL" TO VDU-FORM-NAME.

           PERFORM INITIALIZATION.
           IF EXT-EOBUF-ERRCD NOT = " "
              GO TO END-PROGRAM.

           MOVE "PROCESSING EMAIL UPLOAD" TO MESS.
           PERFORM SEND-LEGEND.

           MOVE 0 TO HOLD-BRNO.

           PERFORM OPEN-EMAIL-FILE.

       NEXT-EMAIL.
           MOVE SPACES TO EMAIL-REC.
           PERFORM READ-EMAIL-FILE.
           IF IO-FG NOT = 0
              PERFORM CLOSE-BW1-FILE
              GO TO END-ROUTINE.

           INSPECT EMAIL-BRNOX REPLACING ALL " " BY "0".
           IF EMAIL-BRNOX NOT NUMERIC
              GO TO NEXT-EMAIL.

           IF EMAIL-BRNO < BEG-BR OR EMAIL-BRNO > END-BR
              GO TO NEXT-EMAIL.

           INSPECT EMAIL-SSNOX REPLACING ALL " " BY "0".
           IF EMAIL-SSNOX NOT NUMERIC
              GO TO NEXT-EMAIL.

           IF EMAIL-BRNO NOT = HOLD-BRNO
              IF HOLD-BRNO NOT = 0
                 PERFORM CLOSE-BW1-FILE
              END-IF
              PERFORM GET-BR-SET-PATHS
              IF IO-FG NOT = 0
                 MOVE 0 TO HOLD-BRNO
                 GO TO NEXT-EMAIL.


           MOVE EMAIL-SSNO TO BW-SSNO.
           PERFORM READ-BW1-FILE.
           IF IO-FG NOT = 0
      *       UNLOCK BW-FILE
              MOVE BR-NO TO D-BRNO
              MOVE EMAIL-SSNO TO D-SSNO
              MOVE "INVALID SSNO" TO D-MSG1
              PERFORM WRITE-DETAIL-LINE
              GO TO NEXT-EMAIL.

           MOVE EMAIL-EMAIL TO BW-EMAIL.
           PERFORM REWRITE-BW1-FILE.
           IF IO-FG NOT = 0
              GO TO IO-ERROR.

           ADD 1 TO GRAND-TOTAL-RECS.

           GO TO NEXT-EMAIL.

       END-ROUTINE.
           PERFORM PRINT-GRAND-TOTALS.
           PERFORM PRINT-RANGE-LINES.
           GO TO END-PROGRAM.

      ***********************************
       INITIALIZATION SECTION.
      ***********************************
           MOVE EMAIL-QUEUE-POS TO Q-POS.
           MOVE EMAIL-COPIES-POS TO C-POS.
           MOVE EMAIL-PRU-POS   TO P-POS.
           MOVE SPACES TO Q-BUF C-BUF P-BUF.
       COPY "LIBGB/EOINIT.CPY".

           PERFORM OPEN-BR-FILE.
           PERFORM OPEN-PR-FILE.

           IF BEG-BR = 0
              IF END-BR = 0
                 MOVE BEG-BR TO SV-BEG-BR
                 MOVE END-BR TO SV-END-BR
                 MOVE EXT-CONAME-USER-LOC TO BEG-BR
                                             END-BR.
           MOVE EXT-CONAME-CONAME TO H-NAME.
           MOVE EXT-CONAME-SYSDATE TO H-DATE.
           MOVE EXT-CONAME-USER-ID TO H-USERID.

           MOVE 0 TO  GRAND-TOTAL-RECS.


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

      * CHMOD /SP/EMAIL FILE
           MOVE "REL" TO GETENV-BUF.
           PERFORM GETENV-CALL.
           MOVE GETENV-BUF TO EMAIL-REL EMAIL-REL2.
           CALL "C$TOLOWER" USING EMAIL-EC, VALUE 3.
           CALL "C$TOLOWER" USING EMAIL-SHELL, VALUE 10.
           MOVE EXT-FILPATH-FIL TO EMAIL-FIL.
           MOVE EMAIL-CMD TO SYSTEM-BUF.
           PERFORM SYSTEM-CALL.

           MOVE EXT-FILPATH-BASE      TO EMAIL-PATH-BASE.
           MOVE EXT-FILPATH-MACHINE   TO EMAIL-PATH-MACHINE.
           MOVE EMAIL-PATH TO ACCESS-BUF.
           PERFORM ACCESS-CALL.
           IF STAT NOT = "00"
              MOVE "EMAIL FILE NOT AVAILABLE" TO MESS
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
           GO TO EXIT-PARM.
       ALL-BR.
           MOVE "SNNN040BR1." TO VDU.
           MOVE 0 TO BEG-BR VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN040BR2." TO VDU.
           MOVE 9999 TO END-BR VDUNUM0.
           PERFORM SCREENIO.

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
       PRINT-GRAND-TOTALS SECTION.
      *************************************
           MOVE 2 TO LN-FEED.
           MOVE "GRAND TOTAL :" TO D-TOTAL-TAG.
           MOVE GRAND-TOTAL-RECS TO D-TOTAL-REC.
           PERFORM WRITE-DETAIL-LINE.

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
           MOVE 2 TO LN-FEED.
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

      ************************************************
       FORM-RESET SECTION.
      ************************************************
           MOVE EXT-CONAME-CONAME TO EMAIL-CONAME.
           MOVE EXT-CONAME-SYSDATE TO EMAIL-SYSDATE.
           DISPLAY EMAIL-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE EMAIL-FORM-FIELDS TO WR-BUF.
           MOVE EMAIL-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      ************************************************
       SETUP-LINK-INFO SECTION.
      ************************************************
           MOVE EMAIL-HELP--FILE TO HELP-LINK-FILE.
           MOVE EMAIL-HELP--DIR TO HELP-LINK-DIR.

      ************************************************
       VDU-CONVERT-FIELD SECTION.
      ************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBSP/EMAIL_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      **************************************
       MISC-PARA SECTION.
      **************************************
       COPY "LIBGB/GETPR.CPY".
       COPY "LIBGB/GETENV.CPY".
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/SYSTEM.CPY".
       INVALID-RANGE.
           MOVE "INVALID RANGE" TO MESS.
           PERFORM SEND-MESS.
       SEND-LEGEND.
           MOVE MESS TO WR-BUF.
           MOVE MESS-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.
       GET-BR-SET-PATHS.
           MOVE EMAIL-BRNO TO BR-NO.
           PERFORM READ-BR-FILE.
           IF IO-FG = 0
              MOVE EXT-FILPATH-BASE TO FILPATH-BASE
              MOVE BR-MACHINE       TO FILPATH-MACHINE
              MOVE BR-DATA-PATH     TO FILPATH-DATA
              MOVE "B"        TO BW-PATH-OVERRIDE
              PERFORM OPEN-BW1-FILE
              MOVE EMAIL-BRNO TO HOLD-BRNO.

      **************************************
       IO-ROUTINES SECTION.
      **************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBLP/LPBWGS_SQL.CPY".
       COPY "LIBGB/GBGRGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBPRINT.CPY".

       OPEN-EMAIL-FILE.
           PERFORM OPEN-IT.
           MOVE EMAIL-PATH TO E-FILE.
           OPEN INPUT EMAIL-FILE.
           IF IO-FG = 8
              GO TO OPEN-EMAIL-FILE.
           IF IO-FG = 7
              CLOSE EMAIL-FILE
              GO TO OPEN-EMAIL-FILE.
       READ-EMAIL-FILE.
           PERFORM READ-IT.
           MOVE EMAIL-PATH TO E-FILE.
           READ EMAIL-FILE.
           IF IO-FG = 8
              GO TO READ-EMAIL-FILE.
       CLOSE-EMAIL-FILE.
           PERFORM CLOSE-IT.
           CLOSE EMAIL-FILE.
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBLP/LPBW1IN.CPY".

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
           IF SV-BEG-BR NOT = 9999
              MOVE SV-BEG-BR TO BEG-BR
              MOVE SV-END-BR TO END-BR.
           IF EXT-ROUTE-BUF = "00"
              MOVE EXIT-PATH TO FORM-NAM
              MOVE FORM-PATH TO FORM-PATHNAME.
       STOP-RUN.
           IF EXT-ROUTE-BUF = "00" OR "40" OR "50"
              DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN
              DESTROY EMAIL-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.
           EXIT PROGRAM.
