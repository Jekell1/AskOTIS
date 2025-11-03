       IDENTIFICATION DIVISION.
       PROGRAM-ID.      GRLST3.
      ******************************************************************
      *                 (GB)
      *
      *  DESCRIPTION:   BRANCH GROUP FILE LISTING
      *
      *  REV:
      *  BLV 040622 FIX GROUP-SUB IN TOTAL-LEVEL-CHECK
      *  BLF 061110 EXPANDED ALL GROUP SUBSCRIPTS TO ACCOMODATE
      *             EXPANDED GROUP SEARCH TABLES, WORLD PR# XXXX
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "LIBGB/GBFSPR.CPY".
       I-O-CONTROL.
       DATA DIVISION.
       FILE SECTION.

       COPY "LIBGB/GBFDPR.CPY".
      /
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)LS/GRLST3.CBL S34 06/10/21-12:27:15 >".

       COPY "LIBGB/GB01GR.CPY".
       COPY "LIBGB/GB01GR_SQL.CPY".
       COPY "LIBGB/GBWSGR.CPY".
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
      ***************************
      *  FILE PATH DEFINITIONS  *
      ***************************

      *    **********************************
      *    *      PROGRAM WORKERS
      *    **********************************

       01  PR-IFN              PIC X(11)    VALUE "0DPRINT".

       01  LN-WORKERS.
           03  LN-FEED         PIC 99       VALUE 0.
           03  LN-COUNT        PIC 99       VALUE 61.
           03  LN-WK           PIC 99       VALUE 0.
       01  PAGE-NUMBER         PIC 999      VALUE 0.

       01  SUB                 PIC 9999     VALUE 0.
       01  SAVE-SUB            PIC 9999     VALUE 0.
       01  LEVEL-SUB           PIC 9999.
       01  TOT-SUB             PIC 99.
       01  GROUP-SUB           PIC 99.
       01  PREV-SUB            PIC 9999.
       01  TOTAL-BRANCHES      PIC 9(4)     VALUE 0.
       01  TOTAL-R1-BRS        PIC 9(4)     VALUE 0.
       01  TOTAL-R2-BRS        PIC 9(4)     VALUE 0.
       01  TOTAL-BAD-BRS       PIC 9(4)     VALUE 0.
      *-----------------------------------------------------------------
       01  TOTAL-TABLE.
           03  TOTAL-TAB        OCCURS 10.
               05  BRANCH-COUNT PIC 9(4).
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

       01  PREV-SCURR-TABLE.
           03  PREV-SCURR      PIC 9999 COMP OCCURS 15.

       01  FIRST-TIME          PIC X        VALUE "Y".
       01  MESS-LIST           PIC X(5)     VALUE "MESS.".
       01  ERRCD               PIC X        VALUE SPACES.

       01  HOLD-GR-KEY         PIC X(4).

       01  HOLD-LEVEL          PIC 9(4) VALUE 0.
       01  HOLD-PREVIOUS-LEVEL PIC 9(4) VALUE 0.

       01  R1                  PIC XX VALUE X'7231'.
       01  R2                  PIC XX VALUE X'7232'.

      *    ****************************************
      *    *            PRINT BUFFERS
      *    ****************************************
       01  HEAD1.
           03  HD-DATE         PIC X(8)     VALUE SPACES.
           03  FILLER          PIC XX       VALUE SPACES.
           03  HD-TIME         PIC X(8).
           03  FILLER          PIC X(28)    VALUE SPACES.
           03  HD-NAME         PIC X(40)    VALUE SPACES.
           03  FILLER          PIC X(37)    VALUE SPACES.
           03  FILLER          PIC X(5)     VALUE "PAGE ".
           03  HD-PAGE-NO      PIC ZZZ9     VALUE SPACES.
       01  HEAD2.
           03  FILLER          PIC X(10)    VALUE "USER ID:".
           03  HD-USERID       PIC X(10).
           03  FILLER          PIC X(31)    VALUE SPACES.
           03  FILLER          PIC X(30)    VALUE
           "  BRANCH GROUP FILE LISTING ".
           03  FILLER          PIC X(45)    VALUE SPACES.
           03  FILLER          PIC X(6)     VALUE "GRLST3".
       01  HD-CONTINUED.
           03  FILLER          PIC X(30)    VALUE SPACES.
           03  FILLER          PIC X(7) VALUE "CONT'D ".
           03  FILLER          PIC X(20) VALUE SPACES.

       01  DETAIL-LINE                      VALUE SPACES.
           03  D-GROUP         OCCURS 8.
               05  D-ID        PIC X(5).
           03  D-BRANCHES      OCCURS 9.
               05  D-BRNO      PIC Z999.
               05  D-COUNT REDEFINES D-BRNO  PIC ZZZ9.
               05  D-PAREN1    PIC X.
               05  D-MACHINE   PIC XX.
               05  D-PAREN2    PIC X.
               05  FILLER      PIC XX.
           03  FILLER          PIC X(2).

       01  GRLST3-WINDOW-HANDLE PIC X(10).

       COPY "LIBGB/GRWORKW.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBGB/PHONE.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/GETPRW.CPY".
       COPY "LIBWI/GRSCANW.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBLS/GRLST3_DEF.CPY".
       COPY "LIBLS/GRLST3_WKS.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

      /***************************************
      *     P R O G R A M   C O N T R O L    *
      ****************************************
       COPY "LIBGB/GETFMW.CPY".

       SCREEN SECTION.
       COPY "LIBLS/GRLST3_SCN.CPY".

       PROCEDURE DIVISION
             USING FORM-PATHNAME EXIT-PATHNAME.

       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
               PR-FILE.
       COPY "LIBGB/DECLARE.CPY".
           PERFORM CLOSE-GR1-FILE.
           PERFORM CLOSE-BR-FILE.
           CLOSE
               PR-FILE.
       COPY "LIBGB/DECLAR2.CPY".

      ************************************************
       MAIN-MODULE SECTION.
      ************************************************

           PERFORM SQL-CONNECT.

       COPY "LIBGB/CHKSEC.CPY".
           MOVE "GRLST3" TO VDU-FORM-NAME.

           DISPLAY FLOATING GRAPHICAL WINDOW
                   SCREEN LINE 01 * VDU-WIN-L-OFFSET
                   SCREEN COL 01 * VDU-WIN-C-OFFSET
                   CELL SIZE IS LABEL FONT
                   SIZE 80 + VDU-WIN-W-MOD
                   LINES 24 + VDU-WIN-H-MOD
                   BLANK SCREEN
                   HANDLE IS GRLST3-WINDOW-HANDLE.

           PERFORM FORM-RESET.

           PERFORM INITIALIZATION.

           MOVE 1 TO WGR-TCURR WGR-TDEX1.
           PERFORM DO-LINE.

       READ-GRFILE-NEXT.
           PERFORM SEARCH-GR-NEXT.
           IF WGR-SRESULT = 0
              IF SAVE-SUB > 0
                 PERFORM WRITE-DETAIL-LINE
              END-IF
              PERFORM PRINT-SUMMARY-COUNTS
              GO TO END-PROGRAM.

           MOVE WGR-SSTABLE TO SUB.
       TITLE-LOOP.
           ADD 1 TO SUB.
           IF SUB > WGR-SDEPTH
              GO TO SKIP-TITLES.
           MOVE WGR-SCURR(SUB) TO WGR-TCURR.
           MOVE SUB TO WGR-TDEX1.
           PERFORM DO-LINE.

           GO TO TITLE-LOOP.
       SKIP-TITLES.
           MOVE WGR-SCURR(WGR-TPLUS) TO WGR-TCURR.
           MOVE WGR-TPLUS TO WGR-TDEX1.

           PERFORM DO-LINE.

           GO TO READ-GRFILE-NEXT.

      ****************************************
       INITIALIZATION SECTION.
      ****************************************

           MOVE GRLST3-QUEUE-POS TO Q-POS.
           MOVE GRLST3-COPIES-POS TO C-POS.
           MOVE GRLST3-PRU-POS TO P-POS.
           MOVE SPACES TO Q-BUF C-BUF P-BUF.
           PERFORM PRU-IN.
           IF STAT NOT = "00"
              GO TO END-PROGRAM.

       REC-GRID.
           MOVE "E  A000GRID." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO END-PROGRAM.

           IF VDUSTATUS = "1<"
              MOVE "WI/GRSCAN" TO FORM-NAM
              CALL FORM-PROGX USING FORM-PATH GRSCAN-BUF EXIT-PATHNAME
              CANCEL FORM-PROGX
              IF GRSCAN-STATUS NOT = "00"
                 GO TO REC-GRID
              ELSE
                 MOVE "S  A000GRID." TO VDU
                 MOVE GRSCAN-KEY TO VDUBUF
                 PERFORM SCREENIO.
           MOVE VDUBUF TO WGR-WORD HOLD-GR-KEY.
           MOVE 0 TO WGR-MIN-LEVEL.
           PERFORM CHECK-MEMBER.
           IF WGR-ERR > 0
              MOVE WGR-MSG(WGR-ERR) TO MESS
              PERFORM SEND-MESS
              GO TO REC-GRID.
       ACCEPT-IT.
           MOVE "E  A000ACCEPT." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO END-PROGRAM.
           IF VDUSTATUS = "18" GO TO REC-GRID.
           IF VDUBUF = "Y" GO TO CONTINUE-IT.
           IF VDUBUF = "N" GO TO REC-GRID.
           GO TO ACCEPT-IT.
       CONTINUE-IT.
           PERFORM OPEN-GR1-FILE.
           PERFORM OPEN-BR-FILE.
           MOVE HOLD-GR-KEY TO GR-ID.
           PERFORM SEARCH-GR-INIT.

           MOVE " LOC/GROUP FILE LISTING IN PROGRESS " TO WR-BUF.
           MOVE MESS-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

           PERFORM OPEN-PR-FILE.

      ***************************************************
       DO-LINE SECTION.
      ***************************************************
           IF WGR-SID(WGR-TCURR) NOT NUMERIC
              GO TO DO-LINE-ALPHA.

           ADD 1 TO SAVE-SUB.

           MOVE 9 TO HOLD-PREVIOUS-LEVEL.
           MOVE WGR-SID(WGR-TCURR) TO WGR-WORD.
           MOVE WGR-NUMB TO BR-NO D-BRNO(SAVE-SUB).
           PERFORM READ-BR-FILE.
           IF IO-FG NOT = 0
              MOVE "**" TO BR-MACHINE.
           IF BR-MACHINE = R1
              ADD 1 TO TOTAL-R1-BRS
           ELSE IF BR-MACHINE = R2
              ADD 1 TO TOTAL-R2-BRS
           ELSE
              ADD 1 TO TOTAL-BAD-BRS.
           ADD 1 TO TOTAL-BRANCHES.
           MOVE "(" TO D-PAREN1(SAVE-SUB).
           MOVE BR-MACHINE TO D-MACHINE(SAVE-SUB).
           MOVE ")" TO D-PAREN2(SAVE-SUB).
           IF SAVE-SUB = 9
              PERFORM WRITE-DETAIL-LINE
              MOVE 0 TO SAVE-SUB.
           GO TO EXIT-DO-LINE.
       DO-LINE-ALPHA.
           IF HOLD-PREVIOUS-LEVEL = 9
              IF SAVE-SUB > 0
                 PERFORM WRITE-DETAIL-LINE
                 MOVE 0 TO SAVE-SUB.
           MOVE WGR-SID(WGR-TCURR) TO GR-ID.
           PERFORM READ-GR1-FILE.
           IF IO-FG NOT = 0
              MOVE 1 TO GR-LEVEL.
           MOVE GR-ID TO D-ID(GR-LEVEL).
      *    IF GR-LEVEL = 9
      *       MOVE GR-MEMBER-LOC(1) TO D-LOC.
       DO-LINE-CONTINUE.
           EXIT.

       EXIT-DO-LINE.
           EXIT.


      *************************************
       PRINT-SUMMARY-COUNTS SECTION.
      *************************************

           MOVE "                         BRANCH COUNT SUMMARY"
              TO PR-REC.
           MOVE 99 TO LN-FEED.
           PERFORM WRITE-PR-REC.
           MOVE "                 *** GROUP ***        # BRANCHES"
              TO PR-REC.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-PR-REC.
           MOVE 2 TO LN-FEED LN-COUNT.
           MOVE ZEROS TO TOTAL-TABLE.
           MOVE HOLD-GR-KEY TO GR-ID.
           PERFORM SEARCH-GR-INIT.

       SUMMARY-READ-GRFILE-NEXT.
           PERFORM SEARCH-GR-NEXT.
           IF WGR-SRESULT = 0
              MOVE ZEROS TO WGR-SCURR-TABLE
              MOVE 15 TO WGR-TPLUS
              PERFORM TOTAL-LEVEL-CHECK
              GO TO EXIT-PRINT-SUMMARY-COUNTS.

           IF FIRST-TIME = "Y"
              MOVE "N" TO FIRST-TIME
              MOVE WGR-SCURR-TABLE TO PREV-SCURR-TABLE
           ELSE
              PERFORM TOTAL-LEVEL-CHECK.

           PERFORM CHECK-ADD-TOTALS.
           GO TO SUMMARY-READ-GRFILE-NEXT.

       EXIT-PRINT-SUMMARY-COUNTS.
           EXIT.

      ******************************************************************
       TOTAL-LEVEL-CHECK SECTION.
      ******************************************************************

           MOVE 15 TO GROUP-SUB.
       LEVEL-TOTAL-LOOP.

           IF GROUP-SUB = 0
              GO TO EXIT-TOTAL-LEVEL-CHECK.

           IF WGR-SCURR(GROUP-SUB) = PREV-SCURR(GROUP-SUB)
              GO TO CONTINUE-LOOP.

           MOVE PREV-SCURR(GROUP-SUB) TO PREV-SUB.

           IF PREV-SUB = 0
              GO TO CONTINUE-LOOP.

           IF WGR-SID(PREV-SUB) NUMERIC
              GO TO CONTINUE-LOOP.

           MOVE WGR-SID(PREV-SUB) TO GR-ID.
           PERFORM READ-GR1-FILE.

           MOVE GR-LEVEL TO TOT-SUB.

       DO-GROUP-TOTALS.

           MOVE GR-ID TO D-ID(GR-LEVEL).
           MOVE BRANCH-COUNT(TOT-SUB) TO D-COUNT(1).
           PERFORM WRITE-SUMMARY-LINE.
           MOVE ZEROS TO TOTAL-TAB(TOT-SUB).
           GO TO CONTINUE-LOOP.

       CONTINUE-LOOP.

           SUBTRACT 1 FROM GROUP-SUB.

           IF GROUP-SUB < WGR-SSTABLE
              GO TO EXIT-TOTAL-LEVEL-CHECK.

           GO TO LEVEL-TOTAL-LOOP.

       EXIT-TOTAL-LEVEL-CHECK.
           MOVE WGR-SCURR-TABLE TO PREV-SCURR-TABLE.

      ******************************************************************
       CHECK-ADD-TOTALS SECTION.
      ******************************************************************

           MOVE WGR-TPLUS TO GROUP-SUB.

       CHECK-TOTAL-LOOP.

           IF GROUP-SUB = 0
              GO TO EXIT-CHECK-TOTALS.

           MOVE WGR-SCURR(GROUP-SUB) TO LEVEL-SUB.

           IF WGR-SID(LEVEL-SUB) IS NUMERIC
              MOVE 9 TO TOT-SUB
              GO TO CHECK-TOTAL-FLAG.

           MOVE WGR-SID(LEVEL-SUB) TO GR-ID.
           PERFORM READ-GR1-FILE.
           MOVE GR-LEVEL TO TOT-SUB.

       CHECK-TOTAL-FLAG.

           ADD 1 TO BRANCH-COUNT(TOT-SUB).

      *-----------------------------------------------------------------
       CONTINUE-CHECK-TOTALS.

           SUBTRACT 1 FROM GROUP-SUB.

           IF GROUP-SUB > 0
              GO TO CHECK-TOTAL-LOOP.

       EXIT-CHECK-TOTALS.
           EXIT.

      *************************************
       WRITE-DETAIL-LINE SECTION.
      *************************************
           IF GR-LEVEL NOT = HOLD-LEVEL
              MOVE HOLD-LEVEL TO HOLD-PREVIOUS-LEVEL
              MOVE GR-LEVEL TO HOLD-LEVEL
              MOVE 2 TO LN-FEED.
           ADD LN-FEED LN-COUNT GIVING LN-WK.
           IF LN-WK GREATER THAN EXT-CONAME-RPT-LINES
              PERFORM HEAD-PAGE.
           MOVE DETAIL-LINE TO PR-REC.
           PERFORM WRITE-PR-REC.

           ADD LN-FEED TO LN-COUNT.
           MOVE SPACES TO DETAIL-LINE.
           MOVE 1 TO LN-FEED.

      *************************************
       WRITE-SUMMARY-LINE SECTION.
      *************************************
           ADD LN-FEED LN-COUNT GIVING LN-WK.
           IF LN-WK GREATER THAN 62
              PERFORM HEAD-PAGE.
           MOVE DETAIL-LINE TO PR-REC.
           PERFORM WRITE-PR-REC.
           ADD LN-FEED TO LN-COUNT.
           MOVE SPACES TO DETAIL-LINE.
           MOVE 1 TO LN-FEED.

      *************************************
       HEAD-PAGE SECTION.
      *************************************
           ADD 1 TO PAGE-NUMBER.
           MOVE PAGE-NUMBER TO HD-PAGE-NO.
           PERFORM GET-TIME.
           MOVE TIME-EDIT TO HD-TIME.
           MOVE EXT-CONAME-CONAME TO HD-NAME.
           MOVE EXT-CONAME-USER-ID TO HD-USERID.
           MOVE EXT-CONAME-SYSDATE TO HD-DATE.
           MOVE HEAD1 TO PR-REC.
           MOVE 99 TO LN-FEED.
           PERFORM WRITE-PR-REC.

           MOVE HEAD2 TO PR-REC.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-PR-REC.

       END-HEAD-PAGE.
           MOVE 1 TO LN-COUNT.
           IF HOLD-PREVIOUS-LEVEL NOT = 0
              MOVE HD-CONTINUED TO PR-REC
              PERFORM WRITE-PR-REC
              ADD 1 TO LN-COUNT.
           MOVE 2 TO LN-FEED.

       COPY "LIBGB/GRSERCH.CPY".
       COPY "LIBGB/GRWORK.CPY".
       COPY "LIBGB/TIMEIO.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/DATER.CPY".

      ****************************************************
       FORM-RESET SECTION.
      ****************************************************
           MOVE EXT-CONAME-CONAME TO GRLST3-CONAME.
           MOVE EXT-CONAME-SYSDATE TO GRLST3-SYSDATE.
           DISPLAY GRLST3-SCREEN UPON GRLST3-WINDOW-HANDLE.
           MOVE GRLST3-FORM-FIELDS TO WR-BUF.
           MOVE GRLST3-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      ****************************************************
       SETUP-LINK-INFO SECTION.
      ****************************************************
           MOVE GRLST3-HELP--FILE TO HELP-LINK-FILE.
           MOVE GRLST3-HELP--DIR  TO HELP-LINK-DIR.

      ****************************************************
       VDU-CONVERT-FIELD SECTION.
      ****************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBLS/GRLST3_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.


      **************************************************
       MISC SECTION.
      **************************************************
       COPY "LIBGB/PHONEIO.CPY".
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/GETPR.CPY".

      **************************************************
       IO-ROUTINES SECTION.
      **************************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/GBGRGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBPRINT.CPY".
       COPY "LIBGB/GBGR1RN.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO ERRCD.

      **************************************************
       END-PROGRAM SECTION.
      **************************************************
       END-PROG.
           PERFORM CLOSE-FILES.
           PERFORM PRU-OUT.
       EXIT-PROG.
           MOVE "LP/LPL2MU" TO FORM-NAM.
           MOVE FORM-PATH TO FORM-PATHNAME.
       STOP-RUN.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY GRLST3-SCREEN.
           DESTROY GRLST3-WINDOW-HANDLE.
           EXIT PROGRAM.
