       IDENTIFICATION DIVISION.
       PROGRAM-ID.      BR1618.
      *****************************************************
      *   READ IN A FIXED LENGTH ASCII FILE TO SET B2-BRANCH-CLOSED
      *    LOGS CHANGE IN THE CHG.YYMMDD FILE
      *
      *    ../../SP/BRCLOSED  (GLOBAL SP)
      *
      * INPUT FILE;
      *  OTIS BRANCH (4)
      *  CLOSED FLAG (1)
      *
      *
      * 20230727 BAH SET B2-BRANCH-CLOSED #1618
      ******************************************************
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
      *  CLOSED FLAG
           03  W-FLAG           PIC X.

       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)FX/BR1618.CBL S35 07/27/23-16:28:14 >".
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".

      ******************************************************************
      *    PROGRAM WORKERS
      ******************************************************************
       01  WRLD-PATH.
           03  WRLD-PATH-BASE    PIC X(9).
           03  FILLER            PIC X VALUE "/".
           03  WRLD-PATH-MACHINE PIC X(2).
           03  FILLER            PIC X(15) VALUE "/SP/BRCLOSED".

       01  LN-WORKERS.
           03  LN-FEED          PIC 99     VALUE 0.
           03  LN-COUNT         PIC 999    VALUE 99.
           03  LN-WK            PIC 999    VALUE 0.
       01  PAGE-NUMBER          PIC 9(4)   VALUE 0.
 
       01  RECORD-COUNTER       PIC 9(6)      VALUE 0 COMP-3.
       01  RECORDS-WRITTEN      PIC 9(6)      VALUE 0 COMP-3.

       01  TODAYS-DATE          PIC 9(8).

       01  LMC-DETAIL                  PIC X(55)      VALUE SPACES.
       01  FILLER                      REDEFINES LMC-DETAIL.
           03  LMC-BRNO                PIC 9(04).
           03  FILLER                  PIC X(01).
           03  LMC-FIELD               PIC X(03).
           03  FILLER                  PIC X(01).
           03  LMC-DESC                PIC X(09).
           03  FILLER                  PIC X(01).
           03  LMC-ORG-TEXT            PIC X(16).
           03  LMC-TO-X                PIC X(04).
           03  LMC-CHG-TEXT            PIC X(16).

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
               "SET BRANCH CLOSED FLAG FROM FILE".

       01  DETAIL-LINE              PIC X(80).
       01  DETAIL-LINE1 REDEFINES DETAIL-LINE.
           03  D-BRNO        PIC Z(4).
           03  FILLER        PIC X(2).
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
       COPY "LIBGB/MKTEMPW_CMD.CPY".
       COPY "LIBGB/GETENVW.CPY".

       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBFX/BR1618_DEF.CPY".
       COPY "LIBFX/BR1618_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBFX/BR1618_SCN.CPY".

       PROCEDURE DIVISION
            USING FORM-PATHNAME EXIT-PATHNAME.
       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
              PR-FILE WRLD-FILE.
       COPY "LIBGB/DECLRP.CPY".
           PERFORM CLOSE-BR-FILE.
           CLOSE PR-FILE WRLD-FILE.
       COPY "LIBGB/DECLRP2.CPY".

      ******************************************************************
       MAIN-PROGRAM SECTION.
      ******************************************************************

           PERFORM SQL-CONNECT.

      D    STOP MESS.
           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "BR1618" TO VDU-FORM-NAME.

           MOVE EXT-FILPATH-BASE    TO WRLD-PATH-BASE.
           MOVE EXT-FILPATH-MACHINE TO WRLD-PATH-MACHINE.

           PERFORM INITIALIZATION.
           IF EXT-EOBUF-ERRCD NOT = " "
               GO TO END-PROGRAM.

           PERFORM SET-BR-FIELD.

           PERFORM PRINT-TOTALS.

           GO TO END-PROGRAM.

      *****************************
       SET-BR-FIELD SECTION.
      *****************************
           PERFORM OPEN-WRLD-FILE.

           MOVE 0 TO RECORD-COUNTER.
       NEXT-REC.
           MOVE SPACES TO WRLD-REC.
           PERFORM READ-WRLD-FILE.
           IF IO-FG NOT = 0 
              GO TO EXIT-SET-BR.
 
           ADD 1 TO RECORD-COUNTER.

           INSPECT W-BRNOX      REPLACING ALL " " BY "0".

           MOVE W-BRNO TO BR-NO.
           PERFORM READ-BR-FILE.
           IF IO-FG NOT = 0
              MOVE W-BRNO               TO D-BRNO
              MOVE "INVALID BRANCH NO." TO D-TEXT
              PERFORM WRITE-DETAIL-LINE
              GO TO NEXT-REC.

           IF W-FLAG NOT = BR-BRANCH-CLOSED
              IF REPORT-ONLY = "N"
                 PERFORM LOG-FIELD-CHANGE
                 MOVE W-FLAG TO BR-BRANCH-CLOSED
                 PERFORM REWRITE-BR-FILE
                 ADD 1 TO RECORDS-WRITTEN.

           GO TO NEXT-REC.

       EXIT-SET-BR.
           EXIT.

      ******************************************************************
       INITIALIZATION SECTION.
      ******************************************************************
           MOVE " " TO EXT-EOBUF-ERRCD.

           MOVE BR1618-QUEUE-POS  TO Q-POS.
           MOVE BR1618-COPIES-POS TO C-POS.
           MOVE BR1618-PRU-POS    TO P-POS.
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
              MOVE "SP/BRCLOSED DOES NOT EXIST" TO MESS
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
       LOG-FIELD-CHANGE SECTION.
      *******************************************************

      * ONLY HERE IF FIELD WAS CHANGED!

           MOVE SPACES                  TO LMC-DETAIL.
           MOVE BR-NO                   TO LMC-BRNO.
           MOVE "FX "                   TO LMC-FIELD.
           MOVE "BR CLOSED"             TO LMC-DESC.
           MOVE " TO "                  TO LMC-TO-X.
           MOVE BR-BRANCH-CLOSED        TO LMC-ORG-TEXT.
           MOVE W-FLAG                  TO LMC-CHG-TEXT.

           MOVE "F"           TO ERRLOGW-ACTION.
           MOVE LMC-DETAIL    TO ERRLOGW-DETAIL.
           MOVE FORM-PATHNAME TO ERRLOGW-PROGX.

           MOVE "ERRLOG" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH ERRLOGW-AREA.
           CANCEL FORM-PROGX.

      *******************************************************
       EO-EXEC-AUDIT SECTION.
      *******************************************************
           MOVE WRLD-PATH TO ACCESS-BUF.
           PERFORM ACCESS-CALL.
           IF STAT NOT = "00"
              MOVE "BRCLOSED FILE MISSING" TO MESS
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
           MOVE EXT-CONAME-CONAME TO BR1618-CONAME.
           MOVE EXT-CONAME-SYSDATE TO BR1618-SYSDATE.
           DISPLAY BR1618-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE BR1618-FORM-FIELDS TO WR-BUF. 
           MOVE BR1618-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM. 

      ****************************************************
       SETUP-LINK-INFO SECTION.
      ****************************************************
           MOVE BR1618-HELP--FILE TO HELP-LINK-FILE.
           MOVE BR1618-HELP--DIR  TO HELP-LINK-DIR.

      ****************************************************
       VDU-CONVERT-FIELD SECTION.
      ****************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBFX/BR1618_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ************************************
       MISC-PARAGRAPH SECTION.
      ************************************
       COPY "LIBGB/GETPR.CPY".
       COPY "LIBGB/ACCESS.CPY".

      *********************************
       IO-ROUTINE SECTION.
      *********************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".

       COPY "LIBGB/GBPRINT.CPY".
       COPY "LIBGB/GBBRIN.CPY".
       OPEN-WRLD-FILE.
           PERFORM OPEN-IT.
           MOVE "CREDITLIM" TO E-FILE.
           OPEN INPUT WRLD-FILE.
           IF IO-FG = 8
              GO TO OPEN-WRLD-FILE.
           IF IO-FG = 7
              CLOSE WRLD-FILE
              GO TO OPEN-WRLD-FILE.
       READ-WRLD-FILE.
           PERFORM READ-IT.
           MOVE "CREDITLIM" TO E-FILE.
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
              DESTROY BR1618-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.
           EXIT PROGRAM.
