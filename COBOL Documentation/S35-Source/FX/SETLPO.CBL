       IDENTIFICATION DIVISION.
       PROGRAM-ID.      SETLPO.
      **************************************************************************
      * READ IN A COMMA DELIMITED FILE AND SET LN-LPO
      *
      * FILE HAS 0D0A ON END OF EACH RECORD
      * FILE=../../SP/LOANLPO   (GLOBAL SP)
      *
      * OPERATIONS WOULD LIKE TO MARK ACQUISITION ACCOUNTS BOARDED MANUALLY
      * WITH AN LPO CODE CORRESPONDING TO THAT PURCHASE.
      *
      * REV:
      *  2020.0714 NEW #1450
      **************************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "LIBGB/GBFSPR.CPY".
           SELECT LPO-FILE ASSIGN TO LPO-PATH
                           ORGANIZATION LINE SEQUENTIAL
                           ACCESS SEQUENTIAL
                           LOCK MODE AUTOMATIC WITH LOCK ON RECORD
                           FILE STATUS FILE-STAT.
       I-O-CONTROL.
       DATA DIVISION.
       FILE SECTION.
       COPY "LIBGB/GBFDPR.CPY".

       FD  LPO-FILE
           RECORD IS VARYING IN SIZE FROM 0 TO 20 CHARACTERS
           LABEL RECORDS ARE STANDARD.
       01  LPO-FILE-REC.
           03  LPO-BYTE   PIC X OCCURS 20 TIMES.



       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)FX/SETLPO.CBL S34 05/25/21-13:00:56 >".
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

       01  LPO-PATH.
           03  LPO-PATH-BASE    PIC X(09).
           03  FILLER           PIC X(01)  VALUE "/".
           03  LPO-PATH-MACHINE PIC X(02).
           03  FILLER           PIC X(23)  VALUE "/SP/LOANLPO".

       01  LPO-REC.
           03  LPO-BRANCH  PIC 9(4).
           03  LPO-NUMBER  PIC 9(6).
           03  LPO-ID      PIC X(4).

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

       01  LPO-COUNTER          PIC 9(6)   VALUE 0 COMP-3.
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

       01  MESS-DISP-LIST    PIC X(4) VALUE "MES.".
       01  MESS-DISPLAY.
           03  MESS-TITLE       PIC X(21)  VALUE SPACES.
           03  FILLER           PIC X(29)   VALUE " ".

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
           03  H-TITLE              PIC X(40) VALUE
               "SET LOAN LPO".

       01  DETAIL-LINE              PIC X(80).
       01  DETAIL-LINE1 REDEFINES DETAIL-LINE.
           03  D-BRNO        PIC 9(4).
           03  FILLER        PIC X(2).
           03  D-NUMBER      PIC Z(6).
           03  FILLER        PIC X(2).
           03  D-LPO         PIC X(4).
           03  FILLER        PIC X(2).
           03  D-TEXT        PIC X(20).

       01  DETAIL-LINE2 REDEFINES DETAIL-LINE.
           03  FILLER        PIC X(20).
           03  R-COUNTERX    PIC X(28).
           03  R-COUNTER     PIC Z(8)9.99.
           03  R-COUNTER-TEXT REDEFINES R-COUNTER PIC X(12).


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
       COPY "LIBFX/SETLPO_DEF.CPY".
       COPY "LIBFX/SETLPO_WKS.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

      ****************************************
      *     P R O G R A M   C O N T R O L    *
      ****************************************
       COPY "LIBGB/GETFMW.CPY".

       SCREEN SECTION.
       COPY "LIBFX/SETLPO_SCN.CPY".

       PROCEDURE DIVISION
             USING FORM-PATHNAME EXIT-PATHNAME.

       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
              PR-FILE LPO-FILE.
       COPY "LIBGB/DECLRP.CPY".
           PERFORM CLOSE-BR-FILE.
           CLOSE PR-FILE LPO-FILE.
       COPY "LIBGB/DECLRP2.CPY".

      ******************************************************************
      *
      *    M A I N - P R O G R A M
      *
      ******************************************************************
       MAIN-PROGRAM SECTION.

           PERFORM SQL-CONNECT.

           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "SETLPO" TO VDU-FORM-NAME.

           PERFORM INITIALIZATION.
           IF ERRCD NOT = " "
              GO TO END-PROGRAM.

           MOVE EXT-FILPATH-BASE       TO LPO-PATH-BASE.
           MOVE EXT-FILPATH-MACHINE    TO LPO-PATH-MACHINE.
           MOVE LPO-PATH TO ACCESS-BUF.
           PERFORM ACCESS-CALL.
           IF STAT NOT = "00"
              MOVE SPACES TO MESS
              STRING LPO-PATH, " ", "DOES NOT EXIST"
                     DELIMITED BY "  " INTO MESS
              PERFORM SEND-MESS
              GO TO END-PROGRAM.

           PERFORM SET-NEW-LPO.

           MOVE "N" TO FIRST-TIME.

           PERFORM PRINT-TOTALS.

           GO TO END-PROGRAM.

      *********************************
       SET-NEW-LPO SECTION.
      *********************************

           PERFORM OPEN-LPO-FILE.

           MOVE "PROCESSING FILE:" TO MESS-TITLE.
           MOVE MESS-DISPLAY TO WR-BUF.
           MOVE MESS-DISP-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

           MOVE 0 TO HOLD-BRNO.

           MOVE 0 TO LPO-COUNTER.
       NEXT-LPO.
           MOVE SPACES TO LPO-FILE-REC LPO-REC.
           PERFORM READ-LPO-FILE.
           IF IO-FG NOT = 0
              PERFORM CLOSE-LN1-FILE
              PERFORM CLOSE-LPO-FILE
              GO TO EXIT-SET-NEW-LPO.

           IF LPO-BYTE(1) = X"1A"
              PERFORM CLOSE-LN1-FILE
              PERFORM CLOSE-LPO-FILE
              GO TO EXIT-SET-NEW-LPO.

           PERFORM CONVERT-LPO-REC.

           ADD 1 TO LPO-COUNTER.

           IF LPO-BRANCH < BEG-BR OR LPO-BRANCH > END-BR
              ADD 1 TO OUT-OF-BRANCH-RANGE
              GO TO NEXT-LPO.

           IF HOLD-BRNO NOT = LPO-BRANCH
              PERFORM CLOSE-LN1-FILE
              MOVE LPO-BRANCH TO HOLD-BRNO BR-NO
              PERFORM READ-BR-FILE
              IF IO-FG NOT = 0
                 ADD 1           TO BAD-BR-COUNTER
                 MOVE 0          TO HOLD-BRNO
                 MOVE LPO-BRANCH TO D-BRNO
                 MOVE "INVALID BRANCH" TO D-TEXT
                 PERFORM WRITE-DETAIL-LINE
                 GO TO NEXT-LPO
              ELSE
                 MOVE "Y"              TO LN-PATH-OVERRIDE
                 MOVE EXT-FILPATH-BASE TO LN-ALL-BASE
                 MOVE BR-MACHINE       TO LN-ALL-MACHINE
                 MOVE BR-DATA-PATH     TO LN-ALL-DATA
                 MOVE LN-ALL-PATH      TO LN-PATH
                 PERFORM OPEN-LN1-FILE.

           MOVE LPO-BRANCH TO LN-OWNBR.
           MOVE LPO-NUMBER TO LN-ACCTNO.
           PERFORM READ-LN1-FILE.
           IF IO-FG NOT = 0
              ADD 1           TO MISSING-COUNTER
              MOVE BR-NO      TO D-BRNO
              MOVE LPO-NUMBER TO D-NUMBER
              MOVE LPO-ID     TO D-LPO
              MOVE "INVALID ACCOUNT NO." TO D-TEXT
              PERFORM WRITE-DETAIL-LINE
              GO TO NEXT-LPO.

           MOVE LPO-ID TO LN-LPO.

           IF REPORT-ONLY = "N"
              PERFORM REWRITE-LN1-FILE
              ADD 1 TO REWRITE-COUNTER.

           GO TO NEXT-LPO.

       EXIT-SET-NEW-LPO.
           EXIT.

      *********************************
       CONVERT-LPO-REC SECTION.
      *********************************

           MOVE 1 TO IN-SUB START-SUB.
           MOVE SPACES TO OUT-FIELD.
       FIND-BRANCH.
           IF LPO-BYTE(IN-SUB) NOT = ","
              ADD 1 TO IN-SUB
              GO TO FIND-BRANCH.
           MOVE IN-SUB TO HOLD-END-SUB.

           SUBTRACT 1 FROM HOLD-END-SUB GIVING END-SUB.
           MOVE 40 TO OUT-SUB.
       CONVERT-BRANCH.
           IF LPO-BYTE(END-SUB) NOT = ","
              MOVE LPO-BYTE(END-SUB) TO OUT-BYTE(OUT-SUB).
           SUBTRACT 1 FROM OUT-SUB END-SUB.
           IF END-SUB NOT < START-SUB
              GO TO CONVERT-BRANCH.

           INSPECT OUT-FIELD REPLACING ALL " " BY "0".
           MOVE OUT-NUMBER TO LPO-BRANCH.

           ADD 1 TO HOLD-END-SUB GIVING IN-SUB START-SUB.
           MOVE SPACES TO OUT-FIELD.
       FIND-NUMBER.
           IF LPO-BYTE(IN-SUB) NOT = ","
              ADD 1 TO IN-SUB
              GO TO FIND-NUMBER.
           MOVE IN-SUB TO HOLD-END-SUB.

           SUBTRACT 1 FROM HOLD-END-SUB GIVING END-SUB.
           MOVE 40 TO OUT-SUB.
       CONVERT-NUMBER.
           IF LPO-BYTE(END-SUB) NOT = ","
              IF LPO-BYTE(END-SUB) NUMERIC
                 MOVE LPO-BYTE(END-SUB) TO OUT-BYTE(OUT-SUB)
                 SUBTRACT 1 FROM OUT-SUB END-SUB
              ELSE
                 SUBTRACT 1 FROM END-SUB.
           IF END-SUB NOT < START-SUB
              GO TO CONVERT-NUMBER.

           INSPECT OUT-FIELD REPLACING ALL " " BY "0".
           MOVE OUT-NUMBER TO LPO-NUMBER.

           ADD 1 TO HOLD-END-SUB GIVING IN-SUB START-SUB.
           MOVE SPACES TO OUT-FIELD.
           MOVE 1 TO OUT-SUB.
       FIND-ID.
           IF LPO-BYTE(IN-SUB) NOT = ","
              MOVE LPO-BYTE(IN-SUB) TO OUT-BYTE(OUT-SUB)
              ADD 1 TO IN-SUB OUT-SUB
              IF OUT-SUB NOT > 4
                 GO TO FIND-ID.

           MOVE OUT-FIELD TO LPO-ID.

      **************************************
       INITIALIZATION SECTION.
      **************************************

           MOVE SETLPO-QUEUE-POS TO Q-POS.
           MOVE SETLPO-COPIES-POS TO C-POS.
           MOVE SETLPO-PRU-POS TO P-POS.
           MOVE SPACES TO Q-BUF C-BUF P-BUF.

       COPY "LIBGB/EOINIT.CPY".

           MOVE SPACES TO EXT-EOBUF-ERRCD.

           MOVE SPACES             TO DETAIL-LINE.
           MOVE EXT-CONAME-CONAME  TO H-NAME.
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
           MOVE LPO-COUNTER TO R-COUNTER.
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
           MOVE EXT-CONAME-CONAME TO SETLPO-CONAME.
           MOVE EXT-CONAME-SYSDATE TO SETLPO-SYSDATE.
           DISPLAY SETLPO-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE SETLPO-FORM-FIELDS TO WR-BUF.
           MOVE SETLPO-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      ****************************************************
       SETUP-LINK-INFO SECTION.
      ****************************************************
           MOVE SETLPO-HELP--FILE TO HELP-LINK-FILE.
           MOVE SETLPO-HELP--DIR  TO HELP-LINK-DIR.

      ****************************************************
       VDU-CONVERT-FIELD SECTION.
      ****************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBFX/SETLPO_EVA.CPY".
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

       OPEN-LPO-FILE.
           PERFORM OPEN-IT.
           MOVE "LPO" TO E-FILE.
           OPEN INPUT LPO-FILE.
           IF IO-FG = 8
              GO TO OPEN-LPO-FILE.
           IF IO-FG = 7
              CLOSE LPO-FILE
              GO TO OPEN-LPO-FILE.
       READ-LPO-FILE.
           PERFORM READ-IT.
           MOVE "LPO" TO E-FILE.
           READ LPO-FILE.
           IF IO-FG = 8
              GO TO READ-LPO-FILE.
       CLOSE-LPO-FILE.
           PERFORM CLOSE-IT.
           CLOSE LPO-FILE.
       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO EXT-EOBUF-ERRCD.

      *************************************************
       END-PROGRAM SECTION.
      *************************************************
       END-RUN.
           PERFORM CLOSE-FILES.
           IF EXT-ROUTE-BUF = "00"
              PERFORM PRU-OUT.

       EXIT-PROG.
           IF SV-BEG-BR NOT = 9999
              MOVE SV-BEG-BR TO BEG-BR
              MOVE SV-END-BR TO END-BR.
           IF EXT-ROUTE-BUF = "00"
              MOVE "FX/FXMENU" TO FORM-NAM
              MOVE FORM-PATH   TO FORM-PATHNAME.
       STOP-RUN.
           IF EXT-ROUTE-BUF = "00" OR "40" OR "50"
              DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN
              DESTROY SETLPO-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.
           EXIT PROGRAM.
