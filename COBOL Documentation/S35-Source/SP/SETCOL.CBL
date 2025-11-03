       IDENTIFICATION DIVISION.
       PROGRAM-ID.      SETCOL.
      ******************************************************************
      *    WORLD
      *
      * READ IN A COMMA DELIMITED FILE AND SET LN-COLLECTOR
      * FILE HAS 0D0A ON END OF EACH RECORD
      * FILE=/USR/DATA/R1/SP/COLLECTOR   GLOBAL SP
      *
      * REV:
      * 011121 BAH NEW FOR AUTOMOTIVE CREDIT, PR# 1645
      * 040402 BAH BRING UP AGAIN FROM A10 WITH CHANGES IN IT
      * KEC 2018.0404 PR#00000 PARADATA A30 JAY CISZEWSKI
      *     ADDED SETTING LN-LTOUCH-DATE WITHIN THE HARDCODED
      *     WRITE-LN1-FILE AND REWRITE-LN1-FILE.
      * 2018.12.06 WORLD NEEDS BATCHABLE AND BRANCH RANGE, #1352
      * BAH 181217 GLOBAL LNFILE
      * BAH 20191002 REMOVED ACCESS-CALL LNFILE
      *
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "LIBGB/GBFSPR.CPY".
           SELECT COLLECTOR-FILE ASSIGN TO COLLECTOR-PATH
                           ORGANIZATION LINE SEQUENTIAL
                           ACCESS SEQUENTIAL
                           LOCK MODE AUTOMATIC WITH LOCK ON RECORD
                           FILE STATUS FILE-STAT.
       I-O-CONTROL.
       DATA DIVISION.
       FILE SECTION.
       COPY "LIBGB/GBFDPR.CPY".

       FD  COLLECTOR-FILE
           RECORD IS VARYING IN SIZE FROM 0 TO 20 CHARACTERS
           LABEL RECORDS ARE STANDARD.
       01  COLLECTOR-REC.
           03  COLLECTOR-BYTE   PIC X OCCURS 20 TIMES.



       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)SP/SETCOL.CBL S35 02/22/24-08:19:30 >".
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBLP/LP01LN.CPY".
       COPY "LIBLP/LP01LN_SQL.CPY".
       COPY "LIBLP/LPWSLN.CPY".

      ******************************************************************
      *
      *    PROGRAM WORKERS
      *
      ******************************************************************
       01  HOLD-BRNO            PIC 9(4) VALUE 0.

       01  COLLECTOR-PATH.
           03  COLLECTOR-PATH-BASE    PIC X(09).
           03  FILLER                 PIC X(01)  VALUE "/".
           03  COLLECTOR-PATH-MACHINE PIC X(02).
           03  FILLER                 PIC X(23)  VALUE "/SP/COLLECTOR".

       01  COLL-REC.
           03  COLLECTOR-BRANCH  PIC 9(4).
           03  COLLECTOR-NUMBER  PIC 9(6).
           03  COLLECTOR-ID      PIC X(3).

       01  OUT-FIELD            PIC X(40).
       01  OUT-FIELD-X REDEFINES OUT-FIELD.
           03  OUT-BYTE         PIC X OCCURS 40 TIMES.
       01  OUT-NUMBER-X REDEFINES OUT-FIELD.
           03  FILLER           PIC X(25).
           03  OUT-NUMBER       PIC 9(15).

       01  OUT-SUB          PIC S9(6) COMP-3.
       01  IN-SUB           PIC S9(6) COMP-3.
       01  START-SUB        PIC S9(6) COMP-3.
       01  HOLD-END-SUB     PIC S9(6) COMP-3.
       01  END-SUB          PIC S9(6) COMP-3.

       01  FIRST-TIME           PIC X VALUE "Y".

       01  LN-WORKERS.
           03  LN-FEED          PIC 99     VALUE 0.
           03  LN-COUNT         PIC 999    VALUE 99.
           03  LN-WK            PIC 999    VALUE 0.
       01  PAGE-NUMBER          PIC 9(4)   VALUE 0.

       01  COLLECTOR-COUNTER      PIC 9(6)   VALUE 0 COMP-3.
       01  MISSING-COUNTER      PIC 9(6)   VALUE 0 COMP-3.
       01  REWRITE-COUNTER      PIC 9(6)   VALUE 0 COMP-3.
       01  BAD-BR-COUNTER       PIC 9(6)   VALUE 0 COMP-3.
       01  ACCESS-COUNTER       PIC 9(6)   VALUE 0 COMP-3.
       01  OUT-OF-BRANCH-RANGE  PIC 9(6)   VALUE 0 COMP-3.

       01  ERRCD          PIC X.

       COPY "LIBGB/EOBUF_EXT.CPY".
           03  BEG-BR          PIC 9(04).
           03  END-BR          PIC 9(04).
           03  REPORT-ONLY     PIC X.
       01  VERSION-CONTROL     PIC X    VALUE "A".

       01  SV-BEG-BR           PIC 9999  VALUE 9999.
       01  SV-END-BR           PIC 9999  VALUE 9999.

       01  MESS-DISP-LIST    PIC X(4)  VALUE "MES.".
       01  MESS-DISPLAY      PIC X(50) VALUE "PROCESSING FILE".

      *----------------------------------------------------------------
       01  HEAD1.
           03  H-DATE               PIC X(8) VALUE SPACES.
           03  FILLER               PIC XX   VALUE SPACES.
           03  H-HR                 PIC 99   VALUE 0.
           03  FILLER               PIC X    VALUE ":".
           03  H-MIN                PIC 99   VALUE 0.
           03  FILLER               PIC X(10) VALUE SPACES.
           03  H-NAME               PIC X(25).
           03  FILLER               PIC X(10).
           03  FILLER               PIC X(5)  VALUE "PAGE ".
           03  H-PAGE-NO            PIC ZZZ9.
       01  HEAD2.
           03  FILLER               PIC X(10) VALUE "USER ID:".
           03  H-USERID             PIC X(10).
           03  FILLER               PIC X(5) VALUE SPACES.
           03  FILLER               PIC X(40) VALUE
               "SET COLLECTOR ID".

       01  DETAIL-LINE              PIC X(80).
       01  DETAIL-LINE1 REDEFINES DETAIL-LINE.
           03  D-BRNO        PIC 9(4).
           03  FILLER        PIC X(2).
           03  D-NUMBER      PIC Z(6).
           03  FILLER        PIC X(2).
           03  D-COLLECTOR   PIC X(3).
           03  FILLER        PIC X(2).
           03  D-TEXT        PIC X(20).

       01  DETAIL-LINE2 REDEFINES DETAIL-LINE.
           03  FILLER        PIC X(20).
           03  R-COUNTERX    PIC X(28).
           03  R-COUNTER     PIC Z(8)9.99.
           03  R-COUNTER-TEXT REDEFINES R-COUNTER PIC X(12).
           03  FILLER        PIC X(33).


       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBGB/SYSTEMW.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/GETPRW.CPY".
       COPY "LIBGB/MKTEMPW_CMD.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBSP/SETCOL_DEF.CPY".
       COPY "LIBSP/SETCOL_WKS.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

      ****************************************
      *     P R O G R A M   C O N T R O L    *
      ****************************************
       COPY "LIBGB/GETFMW.CPY".

       SCREEN SECTION.
       COPY "LIBSP/SETCOL_SCN.CPY".

       PROCEDURE DIVISION
             USING FORM-PATHNAME EXIT-PATHNAME.

       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
             PR-FILE COLLECTOR-FILE.
       COPY "LIBGB/DECLRP.CPY".
           PERFORM CLOSE-BR-FILE.
           CLOSE
             PR-FILE COLLECTOR-FILE.
       COPY "LIBGB/DECLRP2.CPY".

      ******************************************************************
      *
      *    M A I N - P R O G R A M
      *
      ******************************************************************
       MAIN-PROGRAM SECTION.
           PERFORM SQL-CONNECT.

           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "SETCOL" TO VDU-FORM-NAME.

           PERFORM INITIALIZATION.
           IF ERRCD NOT = " "
              GO TO END-PROGRAM.

           MOVE EXT-FILPATH-BASE       TO COLLECTOR-PATH-BASE.
           MOVE EXT-FILPATH-MACHINE    TO COLLECTOR-PATH-MACHINE.
           MOVE COLLECTOR-PATH TO ACCESS-BUF.
           PERFORM ACCESS-CALL.
           IF STAT NOT = "00"
              MOVE SPACES TO MESS
              STRING COLLECTOR-PATH, " ", "DOES NOT EXIST"
                     DELIMITED BY "  " INTO MESS
              PERFORM SEND-MESS
              GO TO END-PROGRAM.

           PERFORM SET-NEW-COLLECTOR.

           MOVE "N" TO FIRST-TIME.

           PERFORM PRINT-TOTALS.

           GO TO END-PROGRAM.

      *********************************************
       SET-NEW-COLLECTOR SECTION.
      *********************************************

           PERFORM OPEN-COLLECTOR-FILE.

           MOVE MESS-DISPLAY TO WR-BUF.
           MOVE MESS-DISP-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

           MOVE 0 TO HOLD-BRNO.

           MOVE 0 TO COLLECTOR-COUNTER.
       NEXT-COLLECTOR.
           MOVE SPACES TO COLLECTOR-REC COLL-REC.
           PERFORM READ-COLLECTOR-FILE.
           IF IO-FG NOT = 0
              PERFORM CLOSE-LN1-FILE
              PERFORM CLOSE-COLLECTOR-FILE
              GO TO EXIT-SET-NEW-COLLECTOR.
           IF COLLECTOR-BYTE(1) = X"1A"
              PERFORM CLOSE-LN1-FILE
              PERFORM CLOSE-COLLECTOR-FILE
              GO TO EXIT-SET-NEW-COLLECTOR.


           PERFORM CONVERT-COLLECTOR-REC.

           ADD 1 TO COLLECTOR-COUNTER.

           IF COLLECTOR-BRANCH < BEG-BR OR COLLECTOR-BRANCH > END-BR
              ADD 1 TO OUT-OF-BRANCH-RANGE
              GO TO NEXT-COLLECTOR.

           IF HOLD-BRNO NOT = COLLECTOR-BRANCH
              PERFORM CLOSE-LN1-FILE
              MOVE COLLECTOR-BRANCH TO HOLD-BRNO BR-NO
              PERFORM READ-BR-FILE
              IF IO-FG NOT = 0
                 ADD 1 TO BAD-BR-COUNTER
                 MOVE 0 TO HOLD-BRNO
                 MOVE COLLECTOR-BRANCH TO D-BRNO
                 MOVE "INVALID BRANCH" TO D-TEXT
                 PERFORM WRITE-DETAIL-LINE
                 GO TO NEXT-COLLECTOR
              ELSE
                 MOVE "Y"              TO LN-PATH-OVERRIDE
                 MOVE EXT-FILPATH-BASE TO LN-ALL-BASE
                 MOVE BR-MACHINE       TO LN-ALL-MACHINE
                 MOVE BR-DATA-PATH     TO LN-ALL-DATA
                 MOVE LN-ALL-PATH      TO LN-PATH
                 PERFORM OPEN-LN1-FILE.

           MOVE COLLECTOR-BRANCH TO LN-OWNBR.
           MOVE COLLECTOR-NUMBER TO LN-ACCTNO.
           PERFORM READ-LN1-FILE.
           IF IO-FG NOT = 0
              ADD 1 TO MISSING-COUNTER
              MOVE BR-NO TO D-BRNO
              MOVE COLLECTOR-NUMBER TO D-NUMBER
              MOVE COLLECTOR-ID TO D-COLLECTOR
              MOVE "INVALID ACCOUNT NO." TO D-TEXT
              PERFORM WRITE-DETAIL-LINE
              GO TO NEXT-COLLECTOR.

           MOVE COLLECTOR-ID TO LN-COLLECTOR.

           IF REPORT-ONLY = "N"
              PERFORM REWRITE-LN1-FILE
              ADD 1 TO REWRITE-COUNTER.

           GO TO NEXT-COLLECTOR.

       EXIT-SET-NEW-COLLECTOR.
           EXIT.

      *********************************************
       CONVERT-COLLECTOR-REC SECTION.
      *********************************************

           MOVE 1 TO IN-SUB START-SUB.
           MOVE SPACES TO OUT-FIELD.
       FIND-BRANCH.
           IF COLLECTOR-BYTE(IN-SUB) NOT = ","
              ADD 1 TO IN-SUB
              GO TO FIND-BRANCH.
           MOVE IN-SUB TO HOLD-END-SUB.

           SUBTRACT 1 FROM HOLD-END-SUB GIVING END-SUB.
           MOVE 40 TO OUT-SUB.
       CONVERT-BRANCH.
           IF COLLECTOR-BYTE(END-SUB) NOT = ","
              MOVE COLLECTOR-BYTE(END-SUB) TO OUT-BYTE(OUT-SUB).
           SUBTRACT 1 FROM OUT-SUB END-SUB.
           IF END-SUB NOT < START-SUB
              GO TO CONVERT-BRANCH.

           INSPECT OUT-FIELD REPLACING ALL " " BY "0".
           MOVE OUT-NUMBER TO COLLECTOR-BRANCH.

           ADD 1 TO HOLD-END-SUB GIVING IN-SUB START-SUB.
           MOVE SPACES TO OUT-FIELD.
       FIND-NUMBER.
           IF COLLECTOR-BYTE(IN-SUB) NOT = ","
              ADD 1 TO IN-SUB
              GO TO FIND-NUMBER.
           MOVE IN-SUB TO HOLD-END-SUB.

           SUBTRACT 1 FROM HOLD-END-SUB GIVING END-SUB.
           MOVE 40 TO OUT-SUB.
       CONVERT-NUMBER.
           IF COLLECTOR-BYTE(END-SUB) NOT = ","
              IF COLLECTOR-BYTE(END-SUB) NUMERIC
                 MOVE COLLECTOR-BYTE(END-SUB) TO OUT-BYTE(OUT-SUB)
                 SUBTRACT 1 FROM OUT-SUB END-SUB
              ELSE
                 SUBTRACT 1 FROM END-SUB.
           IF END-SUB NOT < START-SUB
              GO TO CONVERT-NUMBER.

           INSPECT OUT-FIELD REPLACING ALL " " BY "0".
           MOVE OUT-NUMBER TO COLLECTOR-NUMBER.

           ADD 1 TO HOLD-END-SUB GIVING IN-SUB START-SUB.
           MOVE SPACES TO OUT-FIELD.
           MOVE 1 TO OUT-SUB.
       FIND-ID.
           IF COLLECTOR-BYTE(IN-SUB) NOT = ","
              MOVE COLLECTOR-BYTE(IN-SUB) TO OUT-BYTE(OUT-SUB)
              ADD 1 TO IN-SUB OUT-SUB
              IF OUT-SUB NOT > 3
                 GO TO FIND-ID.

           MOVE OUT-FIELD TO COLLECTOR-ID.

      ******************************************************************
       INITIALIZATION SECTION.
      ******************************************************************

           MOVE SETCOL-QUEUE-POS TO Q-POS.
           MOVE SETCOL-COPIES-POS TO C-POS.
           MOVE SETCOL-PRU-POS TO P-POS.
           MOVE SPACES TO Q-BUF C-BUF P-BUF.


       COPY "LIBGB/EOINIT.CPY".

           MOVE SPACES TO EXT-EOBUF-ERRCD.

           MOVE SPACES TO DETAIL-LINE.
           MOVE EXT-CONAME-CONAME TO H-NAME.
           MOVE EXT-CONAME-SYSDATE TO H-DATE.
           MOVE EXT-CONAME-USER-ID TO H-USERID.

           PERFORM OPEN-BR-FILE.
           PERFORM OPEN-PR-FILE.

           IF BEG-BR = 0
              IF END-BR = 0
                 MOVE BEG-BR TO SV-BEG-BR
                 MOVE END-BR TO SV-END-BR
                 MOVE EXT-CONAME-USER-LOC TO BEG-BR
                                             END-BR.

           GO TO EXIT-INITIALIZE.
       INITIALIZE-ENDIT.
           MOVE "X" TO EXT-EOBUF-ERRCD.
       EXIT-INITIALIZE.
           EXIT.

      ************************************
       ENTER-PARAMETERS SECTION.
      ************************************
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
           GO TO REPORT-YN.
       ALL-BR.
           MOVE "SNNN040BR1." TO VDU.
           MOVE 0 TO BEG-BR VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN040BR2." TO VDU.
           MOVE 9999 TO END-BR VDUNUM0.
           PERFORM SCREENIO.
       REPORT-YN.
           MOVE "E  A010REPONLY." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO BR-02.
           IF VDUSTATUS NOT = "00" GO TO REPORT-YN.
           MOVE VDUBUF TO REPORT-ONLY.
           IF NOT (REPORT-ONLY = "Y" OR "N")
              GO TO REPORT-YN.
       EXIT-PARM.
           EXIT.

      ************************************
       DISPLAY-PARAMETERS SECTION.
      ************************************
           MOVE "SNNN040BR1." TO VDU.
           MOVE BEG-BR TO VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN040BR2." TO VDU.
           MOVE END-BR TO VDUNUM0.
           PERFORM SCREENIO.
           MOVE "S  A010REPONLY." TO VDU.
           MOVE REPORT-ONLY TO VDUBUF.
           PERFORM SCREENIO.

      ************************************
       EO-EXEC-AUDIT SECTION.
      ************************************
       EXIT-EO-EXEC-AUDIT.
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
           MOVE 4 TO LN-FEED.
           MOVE "BEGINNING BRANCH     "          TO R-COUNTER-TEXT.
           MOVE BEG-BR                           TO R-COUNTER.
           PERFORM WRITE-DETAIL-LINE

           MOVE "ENDING    BRANCH     "          TO R-COUNTER-TEXT.
           MOVE END-BR                           TO R-COUNTER.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "REPORT ONLY ? " TO R-COUNTERX.
           MOVE REPORT-ONLY TO R-COUNTER-TEXT.
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "TOTAL RECORDS READ......." TO R-COUNTERX.
           MOVE COLLECTOR-COUNTER TO R-COUNTER.
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "OUT OF BRANCH RANGE............" TO R-COUNTERX.
           MOVE OUT-OF-BRANCH-RANGE TO R-COUNTER.
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "BAD ACCOUNT NUMBERS............" TO R-COUNTERX.
           MOVE MISSING-COUNTER TO R-COUNTER.
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "LOAN RECORDS CHANGED..........." TO R-COUNTERX.
           MOVE REWRITE-COUNTER TO R-COUNTER.
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "INVALID BRANCH................." TO R-COUNTERX.
           MOVE BAD-BR-COUNTER TO R-COUNTER.
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "ACCESS ON LN-FILE.............." TO R-COUNTERX.
           MOVE ACCESS-COUNTER TO R-COUNTER.
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE 2 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.


       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/TIMEIO.CPY".
       COPY "LIBGB/DATER.CPY".

      ****************************************************
       FORM-RESET SECTION.
      ****************************************************
           MOVE EXT-CONAME-CONAME TO SETCOL-CONAME.
           MOVE EXT-CONAME-SYSDATE TO SETCOL-SYSDATE.
           DISPLAY SETCOL-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE SETCOL-FORM-FIELDS TO WR-BUF. |MJD001
           MOVE SETCOL-FORM-LIST TO WR-LIST. |MJD001
           PERFORM CALL-WRFORM. |MJD001


      ****************************************************
       SETUP-LINK-INFO SECTION.
      ****************************************************
           MOVE SETCOL-HELP--FILE TO HELP-LINK-FILE.
           MOVE SETCOL-HELP--DIR  TO HELP-LINK-DIR.

      ****************************************************
       VDU-CONVERT-FIELD SECTION.
      ****************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBSP/SETCOL_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.


      ************************************
       MISC-PARAGRAPH SECTION.
      ************************************
       COPY "LIBGB/GETPR.CPY".
       COPY "LIBGB/RMFILE.CPY".
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/SYSTEM.CPY".
       COPY "LIBGB/MKTEMP.CPY".
       COPY "LIBGB/GETENV.CPY".

      *********************************
       IO-ROUTINE SECTION.
      *********************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBLP/LPLNGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBPRINT.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBLP/LPLN1IN_NOBI.CPY".

       OPEN-COLLECTOR-FILE.
           PERFORM OPEN-IT.
           MOVE "COLLECTOR" TO E-FILE.
           OPEN INPUT COLLECTOR-FILE.
           IF IO-FG = 8
              GO TO OPEN-COLLECTOR-FILE.
           IF IO-FG = 7
              CLOSE COLLECTOR-FILE
              GO TO OPEN-COLLECTOR-FILE.
       READ-COLLECTOR-FILE.
           PERFORM READ-IT.
           MOVE "COLLECTOR" TO E-FILE.
           READ COLLECTOR-FILE.
           IF IO-FG = 8
              GO TO READ-COLLECTOR-FILE.
       CLOSE-COLLECTOR-FILE.
           PERFORM CLOSE-IT.
           CLOSE COLLECTOR-FILE.

       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO EXT-EOBUF-ERRCD.

      *********************************
       END-PROGRAM SECTION.
      *********************************
       END-RUN.
           PERFORM CLOSE-FILES.
           IF EXT-ROUTE-BUF = "00"
              PERFORM PRU-OUT.

       EXIT-PROG.
           IF SV-BEG-BR NOT = 9999
              MOVE SV-BEG-BR TO BEG-BR
              MOVE SV-END-BR TO END-BR.
           IF EXT-ROUTE-BUF = "00"
              MOVE "SP/UTMENU" TO FORM-NAM
              MOVE FORM-PATH TO FORM-PATHNAME.
       STOP-RUN.
           IF EXT-ROUTE-BUF = "00" OR "40" OR "50"
              DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN
              DESTROY SETCOL-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.
           EXIT PROGRAM.
