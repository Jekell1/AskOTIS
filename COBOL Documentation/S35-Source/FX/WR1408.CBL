       IDENTIFICATION DIVISION.
       PROGRAM-ID.      WR1408.
      *****************************************************
      *   READ IN A FIXED LENGTH ASCII FILE TO SET LN-ACTIONCD
      *
      *   THEY WANT TO SCRUB DATA AND REMOVE DECEASED BORROWERS
      *
      * INPUT FILE;
      *  OTIS BRANCH (4)
      *  OTIS ACCOUNT (6)
      *  ACTION CODE (4)
      *
      *
      * 20191211 BAH WORLD, SET LN-ACTIONCD FROM FILE, #1408
      * 20191219 CS WORLD ADD ACTION CODE MEMO
      *            FX/WR1480: CHANGED ACTION CODE FROM XXXX TO XXXX
      * 2024.1004 BAH BROUGHT BACK FROM QA S35Q-136
      **************************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "LIBGB/GBFSPR.CPY".
           SELECT WRLD-FILE ASSIGN TO WRLD-PATH
                           ORGANIZATION LINE SEQUENTIAL
                           ACCESS SEQUENTIAL
                           LOCK MODE AUTOMATIC WITH LOCK ON RECORD
                           FILE STATUS FILE-STAT.

       I-O-CONTROL.
       DATA DIVISION.
       FILE SECTION.
       COPY "LIBGB/GBFDPR.CPY".

       FD  WRLD-FILE
           LABEL RECORDS ARE STANDARD.
       01  WRLD-REC.
      *  OTIS BRANCH (4)
           03  W-BRNO           PIC 9(4).
           03  W-BRNOX REDEFINES W-BRNO PIC X(4).
      *  OTIS ACCOUNT (6)
           03  W-NUMBER         PIC 9(6).
           03  W-NUMBERX REDEFINES W-NUMBER PIC X(6).
      *  ACTION CODE
           03  W-ACTIONCODE     PIC X(4).


       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)FX/WR1408.CBL S34 05/21/21-13:29:27 >".
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBLP/LP01LN.CPY".
       COPY "LIBLP/LP01LN_SQL.CPY".
       COPY "LIBLP/LPWSLN.CPY".
       COPY "LIBLP/LP01LM.CPY".
       COPY "LIBLP/LP01LM_SQL.CPY".
       COPY "LIBLP/LPWSLM.CPY".

      ******************************************************************
      *    PROGRAM WORKERS
      ******************************************************************
       01  HOLD-BRNO            PIC 9(4) VALUE 0.

       01  WRLD-PATH.
           03  WRLD-PATH-BASE    PIC X(9).
           03  FILLER            PIC X VALUE "/".
           03  WRLD-PATH-MACHINE PIC X(2).
           03  FILLER            PIC X(15) VALUE "/SP/ACTIONCODE".

       01  LN-WORKERS.
           03  LN-FEED          PIC 99     VALUE 0.
           03  LN-COUNT         PIC 999    VALUE 99.
           03  LN-WK            PIC 999    VALUE 0.
       01  PAGE-NUMBER          PIC 9(4)   VALUE 0.
 
       01  RECORD-COUNTER       PIC 9(6)      VALUE 0 COMP-3.
       01  RECORDS-WRITTEN      PIC 9(6)      VALUE 0 COMP-3.

       01  TODAYS-DATE          PIC 9(8).

       01  ACTIONCD-MEMO.
           03  FILLER                 PIC X(36) VALUE
               "FX/WR1408: CHANGED ACTION CODE FROM ".
           03  PRIOR-ACTIONCD         PIC X(5).
           03  FILLER                 PIC X(3) VALUE "TO ".
           03  NEW-ACTIONCD           PIC X(4).

       01  HOLD-MEMO                  PIC X(60).

       01  HOLD-LM-SEQ                PIC 9(3).

       COPY "LIBGB/EOBUF_EXT.CPY".
           03  REPORT-ONLY     PIC X.

       01  VERSION-CONTROL     PIC X    VALUE "A".

       01  HEAD1.
           03  H-DATE               PIC X(8) VALUE SPACES.
           03  FILLER               PIC XX   VALUE SPACES.
           03  H-HR                 PIC 99   VALUE 0.
           03  FILLER               PIC X    VALUE ":".
           03  H-MIN                PIC 99   VALUE 0.
           03  FILLER               PIC X(8) VALUE SPACES.
           03  H-NAME               PIC X(40).
           03  FILLER               PIC X(8).
           03  FILLER               PIC X(5)  VALUE "PAGE ".
           03  H-PAGE-NO            PIC ZZZ9.

       01  HEAD2.
           03  FILLER               PIC X(10) VALUE "USER ID:".
           03  H-USERID             PIC X(10).
           03  FILLER               PIC X(10) VALUE SPACES.
           03  H-TITLE              PIC X(50) VALUE
               "SET ACTIONCODE FROM FILE".

       01  DETAIL-LINE              PIC X(80).
       01  DETAIL-LINE1 REDEFINES DETAIL-LINE.
           03  D-BRNO        PIC Z(4).
           03  FILLER        PIC X(2).
           03  D-NUMBER      PIC 9(6).
           03  FILLER        PIC X(12).
           03  D-TEXT        PIC X(30).

       01  DETAIL-LINE2 REDEFINES DETAIL-LINE.
           03  R-COUNTERX    PIC X(20).
           03  R-COUNTER     PIC Z(8)9.99.
           03  R-COUNTER-ALPHA REDEFINES R-COUNTER PIC X(12).
           03  FILLER        PIC X(2).

 
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBGB/SYSTEMW.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/GETPRW.CPY".
       COPY "LIBGB/GETENVW.CPY".

       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBFX/WR1408_DEF.CPY".
       COPY "LIBFX/WR1408_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBFX/WR1408_SCN.CPY".

       PROCEDURE DIVISION
            USING FORM-PATHNAME EXIT-PATHNAME.
       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
              PR-FILE WRLD-FILE.
       COPY "LIBGB/DECLRP.CPY".
           PERFORM CLOSE-LN1-FILE.
           PERFORM CLOSE-LM1-FILE.
           PERFORM CLOSE-BR-FILE.
           CLOSE PR-FILE WRLD-FILE.
       COPY "LIBGB/DECLRP2.CPY".

      ******************************************************************
       MAIN-PROGRAM SECTION.
      ******************************************************************

           PERFORM SQL-CONNECT.

      D    STOP MESS.
           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "WR1408" TO VDU-FORM-NAME.

           MOVE EXT-FILPATH-BASE    TO WRLD-PATH-BASE.
           MOVE EXT-FILPATH-MACHINE TO WRLD-PATH-MACHINE.

           PERFORM INITIALIZATION.
           IF EXT-EOBUF-ERRCD NOT = " "
               GO TO END-PROGRAM.

           PERFORM SET-LN-FIELD.

           PERFORM PRINT-TOTALS.

           GO TO END-PROGRAM.

      *****************************
       SET-LN-FIELD SECTION.
      *****************************
           PERFORM OPEN-WRLD-FILE.

           MOVE 0 TO HOLD-BRNO.
           MOVE 0 TO RECORD-COUNTER.
       NEXT-REC.
           MOVE SPACES TO WRLD-REC.
           PERFORM READ-WRLD-FILE.
           IF IO-FG NOT = 0 
              GO TO EXIT-SET-LN.
 
           ADD 1 TO RECORD-COUNTER.

           INSPECT W-BRNOX      REPLACING ALL " " BY "0".
           INSPECT W-NUMBERX    REPLACING ALL " " BY "0".

           IF HOLD-BRNO NOT = W-BRNO
              MOVE W-BRNO TO HOLD-BRNO BR-NO
              PERFORM GET-LN-PATH
              IF IO-FG NOT = 0
                 MOVE 0 TO HOLD-BRNO
                 GO TO NEXT-REC
              ELSE
                 PERFORM OPEN-LN1-FILE 
                 PERFORM OPEN-LM1-FILE.                                  

           MOVE W-BRNO   TO LN-OWNBR.
           MOVE W-NUMBER TO LN-ACCTNO.
           PERFORM READ-LN1-FILE.
           IF IO-FG NOT = 0
              MOVE BR-NO             TO D-BRNO
              MOVE W-NUMBER          TO D-NUMBER
              MOVE "MISSING ACCOUNT" TO D-TEXT
              PERFORM WRITE-DETAIL-LINE
              GO TO NEXT-REC.
     
           MOVE LN-ACTIONCD  TO PRIOR-ACTIONCD.
           MOVE W-ACTIONCODE TO LN-ACTIONCD.

           IF REPORT-ONLY = "N"
              PERFORM REWRITE-LN1-FILE
              PERFORM CREATE-ACTIONCD-MEMO
              ADD 1 TO RECORDS-WRITTEN.

           GO TO NEXT-REC.

       EXIT-SET-LN.
           EXIT.

      ***********************************************
       CREATE-ACTIONCD-MEMO SECTION.
      ***********************************************
           IF LN-ACTIONCD NOT = PRIOR-ACTIONCD
              MOVE LN-ACTIONCD    TO NEW-ACTIONCD
              MOVE ACTIONCD-MEMO  TO HOLD-MEMO
              PERFORM CREATE-MEMO.

      ***********************************************
       CREATE-MEMO SECTION.
      ***********************************************
           MOVE LN-ACCTNO TO LM-ACCTNO.
           SUBTRACT TODAYS-DATE FROM 99999999 GIVING LM-DATE.
           MOVE "S"       TO LM-MEMO-TYPE.
           MOVE 1         TO HOLD-LM-SEQ.
       WRITE-MEMO.
           MOVE HOLD-MEMO   TO LM-MEMO.
           MOVE HOLD-LM-SEQ TO LM-SEQ.
           PERFORM WRITE-LM1-FILE.
           IF IO-FG NOT = 0
              IF LM-SEQ NOT = 999
                 ADD 1 TO HOLD-LM-SEQ
                 GO TO WRITE-MEMO
              ELSE
                 MOVE BR-NO TO D-BRNO
                 MOVE LN-ACCTNO TO D-NUMBER
                 MOVE "MAX LINES FOR MEMO REACHED" TO D-TEXT
                 PERFORM WRITE-DETAIL-LINE.

      ******************************************************************
       INITIALIZATION SECTION.
      ******************************************************************
           MOVE " " TO EXT-EOBUF-ERRCD.

           MOVE WR1408-QUEUE-POS  TO Q-POS.
           MOVE WR1408-COPIES-POS TO C-POS.
           MOVE WR1408-PRU-POS    TO P-POS.
           MOVE SPACES            TO Q-BUF C-BUF P-BUF.
       COPY "LIBGB/EOINIT.CPY".

           MOVE 0 TO RECORD-COUNTER RECORDS-WRITTEN.

           MOVE SPACES TO DETAIL-LINE.

           MOVE EXT-CONAME-CONAME  TO H-NAME.
           MOVE EXT-CONAME-SYSDATE TO H-DATE.
           MOVE EXT-CONAME-USER-ID TO H-USERID.

           ACCEPT TODAYS-DATE FROM CENTURY-DATE.

           PERFORM OPEN-BR-FILE.
           PERFORM OPEN-PR-FILE.

           GO TO EXIT-INITIALIZE.
       ENDIT.
           MOVE "X" TO EXT-EOBUF-ERRCD.
       EXIT-INITIALIZE.
           EXIT.
 
      ******************************************************************
       ENTER-PARAMETERS SECTION.
      ******************************************************************

           MOVE WRLD-PATH TO ACCESS-BUF.
           PERFORM ACCESS-CALL.
           IF STAT NOT = "00"
              MOVE "SP/ACTIONCODE DOES NOT EXIST" TO MESS
              PERFORM SEND-MESS
              MOVE "1>" TO VDUSTATUS
              GO TO EXIT-PARM.

       REPORT-YN.
           MOVE "E  A010REPONLY." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1>"
              MOVE "X" TO EXT-EOBUF-ERRCD
              GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" GO TO REPORT-YN.
           MOVE VDUBUF TO REPORT-ONLY.
           IF NOT (REPORT-ONLY = "Y" OR "N")
              GO TO REPORT-YN.

       EXIT-PARM.
           EXIT.

      **************************************
       WRITE-DETAIL-LINE SECTION.
      **************************************
           ADD LN-COUNT LN-FEED GIVING LN-WK.
           IF LN-WK > EXT-CONAME-RPT-LINES
              PERFORM HEAD-PAGE.
           MOVE DETAIL-LINE TO PR-REC.
           PERFORM WRITE-PR-REC.
           ADD LN-FEED TO LN-COUNT.
           MOVE 1 TO LN-FEED.
           MOVE SPACES TO DETAIL-LINE.

      **************************************
       HEAD-PAGE SECTION.
      **************************************
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

       END-HEAD-PAGE.
           MOVE 4 TO LN-FEED.
           MOVE 2 TO LN-COUNT.

      ****************************************
       PRINT-TOTALS SECTION.
      ****************************************

           MOVE "REPORT ONLY ?" TO R-COUNTERX.
           MOVE REPORT-ONLY     TO R-COUNTER-ALPHA.
           MOVE 4 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "TOTAL RECORDS......." TO R-COUNTERX.
           MOVE RECORD-COUNTER         TO R-COUNTER.
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "RECORDS FIXED....." TO R-COUNTERX.
           MOVE RECORDS-WRITTEN      TO R-COUNTER.
           PERFORM WRITE-DETAIL-LINE.

           MOVE 2 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.


      *******************************************************
       EO-EXEC-AUDIT SECTION.
      *******************************************************
           MOVE WRLD-PATH TO ACCESS-BUF.
           PERFORM ACCESS-CALL.
           IF STAT NOT = "00"
              MOVE "ACTIONCODE FILE MISSING" TO MESS
              PERFORM SEND-MESS
              MOVE "X" TO EXT-EOBUF-ERRCD.

      ******************************************************************
       DISPLAY-PARAMETERS SECTION.
      ******************************************************************
           MOVE "S  A010REPONLY." TO VDU.
           MOVE REPORT-ONLY TO VDUBUF.
           PERFORM SCREENIO.

       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/TIMEIO.CPY".
       COPY "LIBGB/DATER.CPY".

      ****************************************************
       FORM-RESET SECTION.
      ****************************************************
           MOVE EXT-CONAME-CONAME TO WR1408-CONAME.
           MOVE EXT-CONAME-SYSDATE TO WR1408-SYSDATE.
           DISPLAY WR1408-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE WR1408-FORM-FIELDS TO WR-BUF. 
           MOVE WR1408-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM. 

      ****************************************************
       SETUP-LINK-INFO SECTION.
      ****************************************************
           MOVE WR1408-HELP--FILE TO HELP-LINK-FILE.
           MOVE WR1408-HELP--DIR  TO HELP-LINK-DIR.

      ****************************************************
       VDU-CONVERT-FIELD SECTION.
      ****************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBFX/WR1408_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ************************************
       MISC-PARAGRAPH SECTION.
      ************************************
       COPY "LIBGB/GETPR.CPY".
       COPY "LIBGB/ACCESS.CPY".
       GET-LN-PATH.
           PERFORM CLOSE-LN1-FILE.
           PERFORM CLOSE-LM1-FILE.
           PERFORM READ-BR-FILE.
           IF IO-FG = 0
              MOVE EXT-FILPATH-BASE TO FILPATH-BASE
              MOVE BR-MACHINE       TO FILPATH-MACHINE
              MOVE BR-DATA-PATH     TO FILPATH-DATA
              MOVE "B"              TO LN-PATH-OVERRIDE
              MOVE EXT-FILPATH-BASE TO FILPATH-BASE
              MOVE BR-MACHINE       TO FILPATH-MACHINE
              MOVE BR-DATA-PATH     TO FILPATH-DATA
              MOVE "B"              TO LM-PATH-OVERRIDE
           ELSE
              MOVE W-BRNO TO D-BRNO
              MOVE W-NUMBER TO D-NUMBER
              MOVE "INVALID BRANCH NO." TO D-TEXT
              PERFORM WRITE-DETAIL-LINE
              MOVE 9 TO IO-FG.

      *********************************
       IO-ROUTINE SECTION.
      *********************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBLP/LPLNGS_SQL.CPY".
       COPY "LIBLP/LPLMGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".

       COPY "LIBGB/GBPRINT.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBLP/LPLN1IN.CPY".
       COPY "LIBLP/LPLM1IN.CPY".
       OPEN-WRLD-FILE.
           PERFORM OPEN-IT.
           MOVE "ACTIONCODE" TO E-FILE.
           OPEN INPUT WRLD-FILE.
           IF IO-FG = 8
              GO TO OPEN-WRLD-FILE.
           IF IO-FG = 7
              CLOSE WRLD-FILE
              GO TO OPEN-WRLD-FILE.
       READ-WRLD-FILE.
           PERFORM READ-IT.
           MOVE "ACTIONCODE" TO E-FILE.
           READ WRLD-FILE.
           IF IO-FG = 8
              GO TO READ-WRLD-FILE.
       CLOSE-WRLD-FILE.
           PERFORM CLOSE-IT.
           CLOSE WRLD-FILE.
       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO EXT-EOBUF-ERRCD.
 
      ***********************************************
       END-PROGRAM SECTION.
      ***********************************************
       END-RUN.
           PERFORM CLOSE-FILES.
           IF EXT-ROUTE-BUF = "00"
              PERFORM PRU-OUT.

           IF EXT-ROUTE-BUF = "00"
              MOVE "FX/FXMENU" TO FORM-NAM
              MOVE FORM-PATH TO FORM-PATHNAME.
       EXIT-PROG.
       STOP-RUN.
           IF EXT-ROUTE-BUF = "00" OR "40" OR "50"
              DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN
              DESTROY WR1408-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.
           EXIT PROGRAM.
