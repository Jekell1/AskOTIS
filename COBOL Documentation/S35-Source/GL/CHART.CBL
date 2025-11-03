       IDENTIFICATION DIVISION.
       PROGRAM-ID.  CHART.
      *****************************************************************
      *
      *        G/L FILE CHART OF ACCOUNTS
      *
      *  MG  000124 FIXED VDUSTATUS/IO-FG/STAT PER DEVELOPMENT MEETING.
      *  BLM 090120 IF GP-I-AM-WORLD, ENTER OPTION TO PRINT SPANISH GL
      *             DESCRIPTIONS, WORLD PR# 616
      *  BLM 090219 SPANISH DESCRIPTION OPTION ONLY IF MEXICO,
      *             WORLD PR# 616
      *  BLM 090227 IF PRINTING SPANISH DESCRIPTION BUT IT IS BLANK
      *             PRINT ENGLISH DESCRIPTION, WORLD PR# 616
      *****************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "LIBGB/GBFSPR.CPY".
       DATA DIVISION.
       FILE SECTION.
       COPY "LIBGB/GBFDPR.CPY".
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(42) VALUE
           "@(#)./GL/CHART.CBL S35 09/20/22-14:05:18 >".
       COPY "LIBGL/GL01GL.CPY".
       COPY "LIBGL/GL01GL_SQL.CPY".
       COPY "LIBGL/GLWSGL.CPY".
       01  PR-IFN          PIC X(2) VALUE "PR".

       01  FIRST-TIME      PIC X     VALUE "Y".
       01  RECORDS-FOUND   PIC X     VALUE " ".
           88  NO-RECORDS-FOUND      VALUE "N".
       01  HOLD-BRANCH     PIC 9(4)  VALUE 0.

       01  LN-WORKERS.
           03  LN-FEED     PIC 999   VALUE 0.
           03  LN-COUNT    PIC 999   VALUE 99.
           03  LN-WK       PIC 999   VALUE 0.
       01  PAGENO          PIC 9(5)  VALUE 0.

       01  MESS-LIST       PIC X(5)   VALUE "MESS.".

       COPY "LIBGB/EOBUF_EXT.CPY".
           03  BEG-BRANCH  PIC 9(4).
           03  END-BRANCH  PIC 9(4).
           03  BEG-ACT     PIC 9(7).
           03  END-ACT     PIC 9(7).

       01  VERSION-CONTROL PIC X VALUE "A".

       01  HEADER-1.
           03  H-SYSDATE   PIC X(8)  VALUE SPACES.
           03  FILLER      PIC X(12) VALUE SPACES.
           03  H-CONAME    PIC X(40) VALUE SPACES.
           03  FILLER      PIC X(8)  VALUE SPACES.
           03  FILLER      PIC X(7)  VALUE "PAGE:".
           03  PAGE-1      PIC ZZZZZ.
       01  HEADER-2.
           03  FILLER      PIC X(10) VALUE "USERID:".
           03  H-USERID    PIC X(10) VALUE SPACES.
           03  FILLER      PIC X(9)  VALUE SPACES.
           03  FILLER      PIC X(21) VALUE
               "G/L CHART OF ACCOUNTS".
           03  FILLER      PIC X(30) VALUE SPACES.
       01  HEADER-3.
           03  FILLER      PIC X(14) VALUE SPACES.
           03  FILLER      PIC X(7)  VALUE "BRANCH ".
           03  H-BEGDEP    PIC ZZZ9.
           03  FILLER      PIC X(4)  VALUE " TO ".
           03  H-ENDDEP    PIC ZZZ9.
           03  FILLER      PIC X(11) VALUE " / ACCOUNT ".
           03  H-BEGACT    PIC ZZZZZZ9.
           03  FILLER      PIC X(4)  VALUE " TO ".
           03  H-ENDACT    PIC ZZZZZZ9.
           03  FILLER      PIC X(15) VALUE SPACES.
       01  HEADER-4.
           03  FILLER      PIC X(10)   VALUE SPACES.
           03  FILLER      PIC X(39)   VALUE
               "BRANCH ACCOUNT  D/C  CLEAR  DESCRIPTION".

       01  DETAIL-LINE     PIC X(80) VALUE SPACES.
       01  DETAIL-1        REDEFINES DETAIL-LINE.
           03  FILLER      PIC X(12).
           03  D-BRANCH    PIC Z(4).
           03  D-DASH      PIC X.
           03  D-ACCOUNT   PIC 9(7).
           03  FILLER      PIC X(3).
           03  D-TYPE      PIC X.
           03  FILLER      PIC X(5).
           03  D-CLEAR     PIC X.
           03  FILLER      PIC X(4).
           03  D-DESCR     PIC X(36).
           03  FILLER      PIC X(3).

       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/GETPRW.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBGL/CHART_DEF.CPY".
       COPY "LIBGL/CHART_WKS.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBGL/CHART_SCN.CPY".
       PROCEDURE DIVISION
           USING FORM-PATHNAME EXIT-PATHNAME.
       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON PR-FILE.
       COPY "LIBGB/DECLRP.CPY".
           PERFORM CLOSE-GL1-FILE.
           PERFORM CLOSE-PR-FILE.
       COPY "LIBGB/DECLRP2.CPY".

      ***********************************************
       MAIN-PROGRAM SECTION.
      ***********************************************
           PERFORM SQL-CONNECT.

           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "CHART" TO VDU-FORM-NAME.

           PERFORM INITIALIZATION.
           IF EXT-EOBUF-ERRCD NOT = " "
              GO TO END-PROGRAM.

       READ-GLFILE-NEXT.
           PERFORM READ-GL1-FILE-NEXT.
           IF IO-BAD
              GO TO END-ROUTINE.

           IF GL-BRANCH > END-BRANCH
              GO TO END-ROUTINE.
           IF GL-BRANCH < BEG-BRANCH
              GO TO READ-GLFILE-NEXT.

           IF GL-ACCOUNT < BEG-ACT OR GL-ACCOUNT > END-ACT
              GO TO READ-GLFILE-NEXT.

           IF FIRST-TIME = "Y"
              MOVE "N" TO FIRST-TIME
              MOVE GL-BRANCH TO HOLD-BRANCH.

           IF GL-BRANCH NOT = HOLD-BRANCH
              MOVE GL-BRANCH TO HOLD-BRANCH
              PERFORM HEAD-PAGE.

           PERFORM DETAIL-LINE-PROCESS.

           GO TO READ-GLFILE-NEXT.
       END-ROUTINE.
           IF FIRST-TIME = "Y"
              MOVE "N" TO RECORDS-FOUND.
           GO TO END-PROGRAM.

      ***********************************************
       INITIALIZATION SECTION.
      ***********************************************
           MOVE CHART-QUEUE-POS TO Q-POS.
           MOVE CHART-COPIES-POS TO C-POS.
           MOVE CHART-PRU-POS   TO P-POS.
           MOVE SPACES TO Q-BUF C-BUF P-BUF.
       COPY "LIBGB/EOINIT.CPY".

           IF EXT-ROUTE-BUF = "00"
              MOVE "REPORT IN PROGRESS......" TO MESS
              MOVE MESS-LIST TO WR-LIST
              MOVE MESS TO WR-BUF
              PERFORM CALL-WRFORM.

           MOVE EXT-CONAME-SYSDATE TO H-SYSDATE.
           MOVE EXT-CONAME-CONAME TO H-CONAME.
           MOVE EXT-CONAME-USER-ID TO H-USERID.
           MOVE BEG-BRANCH TO H-BEGDEP.
           MOVE END-BRANCH TO H-ENDDEP.
           MOVE BEG-ACT TO H-BEGACT.
           MOVE END-ACT TO H-ENDACT.

           PERFORM OPEN-PR-FILE.

           PERFORM OPEN-GL1-FILE.
           MOVE BEG-BRANCH TO GL-BRANCH
                              QGL1-WBEG-BRANCH.
           MOVE END-BRANCH TO QGL1-WEND-BRANCH.
           MOVE BEG-ACT    TO GL-ACCOUNT
                              QGL1-WBEG-ACCOUNT.
           MOVE END-ACT    TO QGL1-WEND-ACCOUNT.

           PERFORM START-GL1-FILE.

           GO TO EXIT-INITIALIZE.
       ENDIT.
           MOVE "X" TO EXT-EOBUF-ERRCD.
       EXIT-INITIALIZE.
           EXIT.

      ***********************************************
       EO-EXEC-AUDIT SECTION.
      ***********************************************

      ***********************************************
       ENTER-PARAMETERS SECTION.
      ***********************************************
       ENTER-PARM.

           IF UP-KEY
              GO TO ENDACT.

       BEGDEP.
           MOVE "ENNN040BEGDEP." TO VDU.
           PERFORM SCREENIO.
           IF F1-KEY GO TO EXIT-PARM.
           IF F3-KEY GO TO ALLDEP.
           IF F7-KEY GO TO EXIT-PARM.
           IF NOT ENTER-KEY GO TO BEGDEP.
           MOVE VDUNUM0 TO BEG-BRANCH.
           GO TO ENDDEP.
       ALLDEP.
           MOVE "SNNN040BEGDEP." TO VDU.
           MOVE 0 TO BEG-BRANCH VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN040ENDDEP." TO VDU.
           MOVE 9999 TO END-BRANCH VDUNUM0.
           PERFORM SCREENIO.
           GO TO BEGACT.
       ENDDEP.
           MOVE "ENNN040ENDDEP." TO VDU.
           PERFORM SCREENIO.
           IF F1-KEY GO TO EXIT-PARM.
           IF UP-KEY GO TO BEGDEP.
           IF F7-KEY GO TO EXIT-PARM.
           IF NOT ENTER-KEY GO TO ENDDEP.
           MOVE VDUNUM0 TO END-BRANCH.
           IF BEG-BRANCH > END-BRANCH
              MOVE "INVALID BRANCH RANGE" TO MESS
              PERFORM SEND-MESS
              GO TO BEGDEP.
       BEGACT.
           MOVE "ENNN070BEGACT." TO VDU.
           PERFORM SCREENIO.
           IF F1-KEY GO TO EXIT-PARM.
           IF UP-KEY GO TO ENDDEP.
           IF F3-KEY GO TO ALLACT.
           IF F7-KEY GO TO EXIT-PARM.
           IF NOT ENTER-KEY GO TO BEGACT.
           MOVE VDUNUM0 TO BEG-ACT.
           GO TO ENDACT.
       ALLACT.
           MOVE "SNNN070BEGACT." TO VDU.
           MOVE 0 TO BEG-ACT VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN070ENDACT." TO VDU.
           MOVE 9999999 TO END-ACT VDUNUM0.
           PERFORM SCREENIO
           GO TO EXIT-PARM.
       ENDACT.
           MOVE "ENNN070ENDACT." TO VDU.
           PERFORM SCREENIO.
           IF F1-KEY GO TO EXIT-PARM.
           IF UP-KEY GO TO BEGACT.
           IF F7-KEY GO TO EXIT-PARM.
           IF NOT ENTER-KEY GO TO ENDACT.
           MOVE VDUNUM0 TO END-ACT.
           IF BEG-ACT > END-ACT
              MOVE "INVALID ACCOUNT NUMBER RANGE" TO MESS
              PERFORM SEND-MESS
              GO TO BEGACT.

       EXIT-PARM.
           EXIT.

      ***********************************************
       DISPLAY-PARAMETERS SECTION.
      ***********************************************
           MOVE "SNNN040BEGDEP." TO VDU.
           MOVE BEG-BRANCH TO VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN040ENDDEP." TO VDU.
           MOVE END-BRANCH TO VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN070BEGACT." TO VDU.
           MOVE BEG-ACT TO VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN070ENDACT." TO VDU.
           MOVE END-ACT TO VDUNUM0.
           PERFORM SCREENIO.

      **********************************************
       DETAIL-LINE-PROCESS SECTION.
      **********************************************

           MOVE SPACES TO DETAIL-1.
           MOVE "-" TO D-DASH.
           MOVE GL-BRANCH TO D-BRANCH.
           MOVE GL-ACCOUNT TO D-ACCOUNT.
           MOVE GL-DESCRIPTION TO D-DESCR.
           MOVE GL-TYPE TO D-TYPE.
           MOVE GL-CLEAR TO D-CLEAR.
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      ***********************************************
       WRITE-DETAIL-LINE SECTION.
      ***********************************************
           ADD LN-FEED LN-COUNT GIVING LN-WK.
           IF LN-WK > EXT-CONAME-RPT-LINES
              PERFORM HEAD-PAGE.
           MOVE DETAIL-LINE TO PR-REC.
           PERFORM WRITE-PR-REC.
           ADD LN-FEED TO LN-COUNT.
           MOVE 1 TO LN-FEED.
           MOVE SPACES TO DETAIL-LINE.

      ***********************************************
       HEAD-PAGE SECTION.
      ***********************************************
           MOVE "N" TO FIRST-TIME.
           ADD 1 TO PAGENO.
           MOVE PAGENO TO PAGE-1.
           MOVE HEADER-1 TO PR-REC.
           MOVE 99 TO LN-FEED.
           PERFORM WRITE-PR-REC.

           MOVE HEADER-2 TO PR-REC.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-PR-REC.

           MOVE HEADER-3 TO PR-REC.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-PR-REC.

           MOVE HEADER-4 TO PR-REC.
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-PR-REC.

       END-HEAD-PAGE.
           MOVE 5 TO LN-COUNT.
           MOVE 2 TO LN-FEED.

      **********************************************************
       FORM-RESET SECTION.
      **********************************************************
           MOVE EXT-CONAME-CONAME TO CHART-CONAME.
           MOVE EXT-CONAME-SYSDATE TO CHART-SYSDATE.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DISPLAY CHART-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE CHART-FORM-FIELDS TO WR-BUF.
           MOVE CHART-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      **********************************************************
       SETUP-LINK-INFO SECTION.
      **********************************************************
           MOVE CHART-HELP--FILE TO HELP-LINK-FILE.
           MOVE CHART-HELP--DIR  TO HELP-LINK-DIR.

      **********************************************************
       VDU-CONVERT-FIELD SECTION.
      **********************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBGL/CHART_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/DATER.CPY".

      **************************************
       MISC SECTION.
      **************************************
       COPY "LIBGB/GETPR.CPY".
       COPY "LIBGB/ACCESS.CPY".

      **************************************
       IO-ROUTINES SECTION.
      **************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGL/GLGLGS_SQL.CPY".
       COPY "LIBGB/GBPRINT.CPY".
       COPY "LIBGL/GLGL1RN.CPY".
       COPY "LIBGB/FERRORS.CPY".

      **********************************************************
       END-PROGRAM SECTION.
      **********************************************************
       END-RUN.
           PERFORM CLOSE-FILES.
           IF EXT-ROUTE-BUF = "00"
              PERFORM PRU-OUT.
           IF NO-RECORDS-FOUND
              MOVE "NO RECORDS FOUND" TO MESS
              PERFORM SEND-MESS.
       EXIT-PROG.
           IF EXT-ROUTE-BUF = "00"
              MOVE EXIT-PATHNAME TO FORM-PATHNAME.
       STOP-RUN.
           IF EXT-ROUTE-BUF = "00" OR "40" OR "50"
              DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN
              DESTROY CHART-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.
           EXIT PROGRAM.
