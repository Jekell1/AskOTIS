       IDENTIFICATION DIVISION.
       PROGRAM-ID.       PERLST.
      ******************************************
      *
      *        G/L FILE SPREAD SHEET
      *        PRINTS EACH PERIOD PER ACCOUNT
      *
      *  MG  000207 FIXED VDUSTATUS/IO-FG/STAT PER DEVELOPMENT MEETING.
      *****************************************
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
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)GL/PERLST.CBL S34 06/10/21-11:36:56 >".
       COPY "LIBGL/GL01GL.CPY".
       COPY "LIBGL/GL01GL_SQL.CPY".
       COPY "LIBGL/GLWSGL.CPY".
       01  PR-IFN          PIC X(2) VALUE "PR".

       01  FIRST-TIME      PIC X     VALUE "Y".
       01  RECORDS-FOUND   PIC X     VALUE " ".
           88  NO-RECORDS-FOUND      VALUE "N".
       01  HOLD-BRANCH     PIC 9(4)  VALUE 0.

       01  REV             PIC 99    VALUE 0.
       01  SUB             PIC 99    VALUE 0.
       01  ACC-DEPTBAL     PIC S9(7)V99 COMP-3 VALUE 0.
       01  ACC-DEPT-TABLE.
           03  ACC-DEPT    PIC S9(7)V99 COMP-3 OCCURS 12.
       01  ACC-COMPBAL     PIC S9(7)V99 COMP-3 VALUE 0.
       01  ACC-COMP-TABLE.
           03  ACC-COMP    PIC S9(7)V99 COMP-3 OCCURS 12.

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
           03  FILLER      PIC X(38) VALUE SPACES.
           03  H-CONAME    PIC X(40) VALUE SPACES.
           03  FILLER      PIC X(34) VALUE SPACES.
           03  FILLER      PIC X(7)  VALUE "PAGE:".
           03  PAGE-1      PIC ZZZZZ.
       01  HEADER-2.
           03  FILLER      PIC X(10) VALUE "USERID:".
           03  H-USERID    PIC X(10) VALUE SPACES.
           03  FILLER      PIC X(37) VALUE SPACES.
           03  FILLER      PIC X(18) VALUE
               "G/L PERIOD LISTING".
           03  FILLER      PIC X(57) VALUE SPACES.
       01  HEADER-3.
           03  FILLER      PIC X(41) VALUE SPACES.
           03  FILLER      PIC X(7)  VALUE "BRANCH ".
           03  H-BEGDEP    PIC 9(4)  VALUE 0.
           03  FILLER      PIC X(4)  VALUE " TO ".
           03  H-ENDDEP    PIC 9(4)  VALUE 0.
           03  FILLER      PIC X(11) VALUE " / ACCOUNT ".
           03  H-BEGACT    PIC 9999999 VALUE 0.
           03  FILLER      PIC X(4)  VALUE " TO ".
           03  H-ENDACT    PIC 9999999 VALUE 0.
       01  HEADER-4.
           03  FILLER      PIC X(33)   VALUE
               "ACCOUNT    TYPE/CLEAR (OPEN BAL.)".
       01  HEADER-5.
           03  FILLER      PIC X(11) VALUE "  PERIOD  1".
           03  FILLER      PIC X(11) VALUE "  PERIOD  2".
           03  FILLER      PIC X(11) VALUE "  PERIOD  3".
           03  FILLER      PIC X(11) VALUE "  PERIOD  4".
           03  FILLER      PIC X(11) VALUE "  PERIOD  5".
           03  FILLER      PIC X(11) VALUE "  PERIOD  6".
           03  FILLER      PIC X(11) VALUE "  PERIOD  7".
           03  FILLER      PIC X(11) VALUE "  PERIOD  8".
           03  FILLER      PIC X(11) VALUE "  PERIOD  9".
           03  FILLER      PIC X(11) VALUE "  PERIOD 10".
           03  FILLER      PIC X(11) VALUE "  PERIOD 11".
           03  FILLER      PIC X(11) VALUE "  PERIOD 12".

       01  DETAIL-LINE     PIC X(132) VALUE SPACES.
       01  DETAIL-1        REDEFINES DETAIL-LINE.
           03  D-BRANCH    PIC Z(4).
           03  D-DASH      PIC X.
           03  D-ACCOUNT   PIC 9(7).
           03  FILLER      PIC X(3).
           03  D-TYPE      PIC X.
           03  D-SLASH     PIC X.
           03  D-CLEAR     PIC X.
           03  FILLER      PIC X(4).
           03  FILL-1      PIC X.
           03  D-OPEN-BAL  PIC Z(7).ZZ-.
           03  FILL-2      PIC X.
           03  FILLER      PIC X(2).
           03  D-DESCR     PIC X(36).
           03  FILLER      PIC X(60).

       01  DETAIL-2        REDEFINES DETAIL-LINE.
           03  D-CYBAL     PIC Z(7).ZZ- OCCURS 12 TIMES.

       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/GETPRW.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBGL/PERLST_DEF.CPY".
       COPY "LIBGL/PERLST_WKS.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBGL/PERLST_SCN.CPY".
       PROCEDURE DIVISION
           USING FORM-PATHNAME EXIT-PATHNAME.
       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON PR-FILE.
       COPY "LIBGB/DECLRP.CPY".
           PERFORM CLOSE-GL1-FILE.
           PERFORM CLOSE-PR-FILE.
       COPY "LIBGB/DECLRP2.CPY".

      *********************************************
       MAIN-PROGRAM SECTION.
      *********************************************
           PERFORM SQL-CONNECT.

           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "PERLST" TO VDU-FORM-NAME.
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
              PERFORM TOTAL-ROUTINE.

           PERFORM DETAIL-LINE-PROCESS.

           GO TO READ-GLFILE-NEXT.
       END-ROUTINE.
           IF FIRST-TIME = "Y"
              MOVE "N" TO RECORDS-FOUND
           ELSE
              PERFORM TOTAL-ROUTINE
              PERFORM COMPANY-ROUTINE.
           GO TO END-PROGRAM.

      *********************************************
       INITIALIZATION SECTION.
      *********************************************
           MOVE PERLST-QUEUE-POS TO Q-POS.
           MOVE PERLST-COPIES-POS TO C-POS.
           MOVE PERLST-PRU-POS   TO P-POS.
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

           MOVE 1 TO SUB.
       CLEAR-TABLE.
           MOVE 0 TO ACC-DEPT(SUB) ACC-COMP(SUB).
           IF SUB < 12
              ADD 1 TO SUB
              GO TO CLEAR-TABLE.

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

      *********************************************
       EO-EXEC-AUDIT SECTION.
      *********************************************

      *********************************************
       ENTER-PARAMETERS SECTION.
      *********************************************
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

      *********************************************
       DISPLAY-PARAMETERS SECTION.
      *********************************************
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

      *********************************************
       DETAIL-LINE-PROCESS SECTION.
      *********************************************

           MOVE SPACES TO DETAIL-1.
           MOVE "-" TO D-DASH.
           MOVE GL-BRANCH TO D-BRANCH.
           MOVE GL-ACCOUNT TO D-ACCOUNT.
           MOVE GL-DESCRIPTION TO D-DESCR.
           MOVE GL-TYPE TO D-TYPE.
           MOVE "/" TO D-SLASH.
           MOVE GL-CLEAR TO D-CLEAR.
           IF GL-TYPE = "C"
              PERFORM CREDIT-REVERSE.
           IF GL-OPENBAL NOT = 0
              MOVE "(" TO FILL-1
              MOVE ")" TO FILL-2
              ADD GL-OPENBAL TO ACC-DEPTBAL
              ADD GL-OPENBAL TO ACC-COMPBAL
              MOVE GL-OPENBAL TO D-OPEN-BAL.
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE SPACES TO DETAIL-2.
           MOVE 1 TO SUB.
       BALANCE-LOOP.
           IF GL-CYBAL(SUB) NOT = 0
              ADD GL-CYBAL(SUB) TO ACC-DEPT(SUB)
              ADD GL-CYBAL(SUB) TO ACC-COMP(SUB)
              MOVE GL-CYBAL(SUB) TO D-CYBAL(SUB).
           ADD 1 TO SUB.
           IF SUB NOT > 12
              GO TO BALANCE-LOOP.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.


      *********************************************
       WRITE-DETAIL-LINE SECTION.
      *********************************************
           ADD LN-FEED LN-COUNT GIVING LN-WK.
           IF LN-WK > EXT-CONAME-RPT-LINES
              PERFORM HEAD-PAGE.
           MOVE DETAIL-LINE TO PR-REC.
           PERFORM WRITE-PR-REC.
           ADD LN-FEED TO LN-COUNT.
           MOVE 1 TO LN-FEED.
           MOVE SPACES TO DETAIL-LINE.

      *********************************************
       HEAD-PAGE SECTION.
      *********************************************
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
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-PR-REC.

           MOVE HEADER-5 TO PR-REC.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-PR-REC.

       END-HEAD-PAGE.
           MOVE 5 TO LN-COUNT.
           MOVE 2 TO LN-FEED.

      ******************************************
       CREDIT-REVERSE SECTION.
      ******************************************

      * REVERSE CREDIT ACCOUNT BALS  

           MULTIPLY -1 BY GL-OPENBAL.
           MOVE 1 TO REV.
       REVERSE-LOOP.
           MULTIPLY -1 BY GL-CYBAL(REV).
           ADD 1 TO REV.
           IF REV < 13
              GO TO REVERSE-LOOP.

      ******************************************
       TOTAL-ROUTINE SECTION.
      ******************************************

           INSPECT DETAIL-1 REPLACING ALL " " BY "*".
           PERFORM WRITE-DETAIL-LINE.

           MOVE "BRANCH OPEN BAL:" TO DETAIL-1.
           MOVE ACC-DEPTBAL TO D-OPEN-BAL.
           MOVE "(" TO FILL-1.
           MOVE ")" TO FILL-2.
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE 0 TO ACC-DEPTBAL.
           PERFORM MOVE-TOTALS.
           PERFORM WRITE-DETAIL-LINE.
           MOVE 90 TO LN-COUNT.

      ******************************************
       COMPANY-ROUTINE SECTION.
      ******************************************

           INSPECT DETAIL-1 REPLACING ALL " " BY "*".
           PERFORM WRITE-DETAIL-LINE.

           MOVE "COMPANY OPEN BAL:" TO DETAIL-1.
           MOVE ACC-COMPBAL TO D-OPEN-BAL.
           MOVE "(" TO FILL-1.
           MOVE ")" TO FILL-2.
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE ACC-COMP-TABLE TO ACC-DEPT-TABLE.
           PERFORM MOVE-TOTALS.
           PERFORM WRITE-DETAIL-LINE.

      ******************************************
       MOVE-TOTALS SECTION.
      ******************************************
           MOVE 0 TO SUB.
       MOVE-TOT-CONT.
           ADD 1 TO SUB.
           IF SUB > 12
               GO TO MOVE-TOT-EXIT.
           MOVE ACC-DEPT(SUB) TO D-CYBAL(SUB).
           MOVE 0 TO ACC-DEPT(SUB).
           GO TO MOVE-TOT-CONT.
       MOVE-TOT-EXIT.
           EXIT.

      ******************************************
       FORM-RESET SECTION.
      ******************************************
           MOVE EXT-CONAME-CONAME TO PERLST-CONAME.
           MOVE EXT-CONAME-SYSDATE TO PERLST-SYSDATE.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DISPLAY PERLST-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE PERLST-FORM-FIELDS TO WR-BUF.
           MOVE PERLST-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      ******************************************
       SETUP-LINK-INFO SECTION.
      ******************************************
           MOVE PERLST-HELP--FILE TO HELP-LINK-FILE.
           MOVE PERLST-HELP--DIR  TO HELP-LINK-DIR.

      **********************************************************
       VDU-CONVERT-FIELD SECTION.
      **********************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBGL/PERLST_EVA.CPY".
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

      ******************************************
       END-PROGRAM SECTION.
      ******************************************
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
              DESTROY PERLST-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.
           EXIT PROGRAM.
