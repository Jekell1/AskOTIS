       IDENTIFICATION DIVISION.
       PROGRAM-ID.      GRLIST.
      ******************************************************************
      *
      *                 (LS)
      *
      *  DESCRIPTION:   GROUP FILE LISTING
      *
      *  REV:
      *  KEC 031198 ADDED PRINTING OF 'HOME BRANCH' AND 'APPROVAL LIMIT'
      *  BLF 061116 EXPANDED ALL GROUP SUBSCRIPTS TO ACCOMODATE
      *             EXPANDED GROUP SEARCH TABLES, WORLD PR# 502
      *  SMR 051523 CHANGED GBGR1IN TO GBGR1RN 
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
       77  SCCS-IDENTIFIER PIC X(43) VALUE
           "@(#)./LS/GRLIST.CBL S35 05/15/23-08:54:32 >".

       COPY "LIBGB/GB01GR.CPY".
       COPY "LIBGB/GB01GR_SQL.CPY".
       COPY "LIBGB/GBWSGR.CPY".
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
      ***************************
      *  FILE PATH DEFINITIONS  *
      ***************************
       01  PR-IFN              PIC X(11)    VALUE "0DPRINT".

       01  LN-WORKERS.
           03  LN-FEED         PIC 99       VALUE 0.
           03  LN-COUNT        PIC 99       VALUE 61.
           03  LN-WK           PIC 99       VALUE 0.
       01  PAGE-NUMBER         PIC 999      VALUE 0.

       01  SUB                 PIC 9999     VALUE 0.


       01  MESS-LIST           PIC X(5)     VALUE "MESS.".
       01  ERRCD               PIC X        VALUE SPACES.

       01  HOLD-GR-KEY         PIC X(4).

       01  INSPECT-ZIP         PIC X(10)    VALUE SPACES.
       01  INSPECT-DESC        PIC X(20)    VALUE SPACES.
       01  NAME-WORKER         PIC X(49)    VALUE SPACES.
       01  HOLD-NAME           PIC X(20).
       01  HOLD-PHONE          PIC 9(10).
       01  HOLD-ADDR1          PIC X(30).
       01  HOLD-CITY           PIC X(15).
       01  HOLD-STATE          PIC X(4).
       01  HOLD-ZIP            PIC X(10).
       01  HOLD-CONTACT        PIC X(20).
       01  HOLD-LEVEL          PIC 9(4) VALUE 0.
       01  HOLD-PREVIOUS-LEVEL PIC 9(4) VALUE 0.

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
           03  FILLER          PIC X(6)     VALUE "GRLIST".
       01  HD-CONTINUED.
           03  FILLER          PIC X(30)    VALUE SPACES.
           03  FILLER          PIC X(7) VALUE "CONT'D ".
           03  HOLD-CONT-NAME  PIC X(20) VALUE SPACES.

       01  DETAIL-LINE                      VALUE SPACES.
           03  D-LEVEL1        PIC X(132).
       01  DETAIL-LINE2 REDEFINES DETAIL-LINE.
           03  FILLER          PIC X(10).
           03  D-LEVEL2        PIC X(122).
       01  DETAIL-LINE3 REDEFINES DETAIL-LINE.
           03  FILLER          PIC X(15).
           03  D-LEVEL3        PIC X(117).
       01  DETAIL-LINE4 REDEFINES DETAIL-LINE.
           03  FILLER          PIC X(20).
           03  D-LEVEL4        PIC X(112).
       01  DETAIL-LINE5 REDEFINES DETAIL-LINE.
           03  FILLER          PIC X(25).
           03  D-LEVEL5        PIC X(107).
       01  DETAIL-LINE6 REDEFINES DETAIL-LINE.
           03  FILLER          PIC X(30).
           03  D-LEVEL6        PIC X(102).
       01  DETAIL-LINE7 REDEFINES DETAIL-LINE.
           03  FILLER          PIC X(35).
           03  D-LEVEL7        PIC X(97).
       01  DETAIL-LINE8 REDEFINES DETAIL-LINE.
           03  FILLER          PIC X(40).
           03  D-LEVEL8        PIC X(92).
       01  DETAIL-LINE9 REDEFINES DETAIL-LINE.
           03  FILLER          PIC X(45).
           03  D-LEVEL9        PIC X(87).

       01  FORMAT-LINE                      VALUE SPACES.
           03  D-NAME          PIC X(20).
           03  FILLER          PIC X.
           03  D-ID            PIC X(4).
           03  FILLER          PIC X(3).
           03  D-LOC           PIC Z99 BLANK ZERO.
           03  FILLER          PIC X(3).
           03  D-NAME-AREA1.
               05  D-PHONE         PIC X(20).
               05  FILLER          PIC X(01).
               05  D-ADDR1         PIC X(30).
               05  FILLER          PIC X(01).
               05  D-HOME-BRANCH   PIC X(80).
           03  D-NAME-AREA2 REDEFINES D-NAME-AREA1.
               05  D-CONTACT       PIC X(20).
               05  FILLER          PIC X(112).
           03  D-NAME-AREA3 REDEFINES D-NAME-AREA1.
               05  FILLER          PIC X(52).
               05  D-APPROVAL-AMT  PIC Z,ZZZ,ZZ9.
               05  FILLER          PIC X(71).

       01  GRLIST-WINDOW-HANDLE PIC X(10).

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
       COPY "LIBLS/GRLIST_DEF.CPY".
       COPY "LIBLS/GRLIST_WKS.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

      ****************************************
      *     P R O G R A M   C O N T R O L    *
      ****************************************
       COPY "LIBGB/GETFMW.CPY".

       SCREEN SECTION.
       COPY "LIBLS/GRLIST_SCN.CPY".

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

      *****************************************************
       MAIN-MODULE SECTION.
      *****************************************************

           PERFORM SQL-CONNECT.

       COPY "LIBGB/CHKSEC.CPY".
           MOVE "GRLIST" TO VDU-FORM-NAME.

           DISPLAY FLOATING GRAPHICAL WINDOW
                   SCREEN LINE 01 * VDU-WIN-L-OFFSET
                   SCREEN COL 01 * VDU-WIN-C-OFFSET
                   CELL SIZE IS LABEL FONT
                   SIZE 80 + VDU-WIN-W-MOD
                   LINES 24 + VDU-WIN-H-MOD
                   BLANK SCREEN
                   HANDLE IS GRLIST-WINDOW-HANDLE.

           PERFORM FORM-RESET.

           PERFORM INITIALIZATION.

           MOVE 1 TO WGR-TCURR WGR-TDEX1.

           PERFORM DO-LINE.

       READ-GRFILE-NEXT.
           PERFORM SEARCH-GR-NEXT.
           IF WGR-SRESULT = 0
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
       SET-PR.
           MOVE GRLIST-QUEUE-POS TO Q-POS.
           MOVE GRLIST-COPIES-POS TO C-POS.
           MOVE GRLIST-PRU-POS TO P-POS.
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
           IF VDUBUF = "N" GO TO CONTINUE-IT.
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

      *********************************************************
       DO-LINE SECTION.
      *********************************************************
           IF WGR-SID(WGR-TCURR) NOT NUMERIC
              GO TO DO-LINE-ALPHA.

           MOVE WGR-SID(WGR-TCURR) TO WGR-WORD.
           MOVE WGR-NUMB TO BR-NO D-ID D-LOC.
           PERFORM READ-BR-FILE.
           IF IO-FG NOT = 0
              MOVE "** INVALID BRANCH **" TO BR-NAME.
           MOVE BR-NAME TO HOLD-NAME.
           MOVE BR-ADR1 TO HOLD-ADDR1.
           MOVE BR-CITY TO HOLD-CITY.
           MOVE BR-STATE TO HOLD-STATE.
           MOVE BR-ZIP TO HOLD-ZIP.
           MOVE BR-PHONE TO HOLD-PHONE.
           MOVE SPACES TO HOLD-CONTACT.
           GO TO DO-LINE-CONTINUE.
       DO-LINE-ALPHA.
           MOVE WGR-SID(WGR-TCURR) TO GR-ID D-ID.
           PERFORM READ-GR1-FILE.
           IF IO-FG NOT = 0
              MOVE "** INVALID GROUP **" TO GR-NAME.
      *    UNLOCK GR-FILE.
           IF GR-LEVEL = 9
              MOVE GR-MEMBER-LOC(1) TO D-LOC.
           MOVE GR-NAME TO HOLD-NAME.
           MOVE GR-ADDR-1 TO HOLD-ADDR1.
           MOVE GR-CITY TO HOLD-CITY.
           MOVE GR-STATE TO HOLD-STATE.
           MOVE GR-ZIP TO HOLD-ZIP.
           MOVE GR-PHONE TO HOLD-PHONE.
           MOVE GR-CONTACT TO HOLD-CONTACT.
       DO-LINE-CONTINUE.
           MOVE HOLD-NAME TO D-NAME.
           MOVE HOLD-PHONE TO TEL-BUF.
           PERFORM SEND-PHONE.
           MOVE TEL-EDIT TO D-PHONE.
           MOVE HOLD-ADDR1 TO D-ADDR1.

           STRING "HOME: ",GR-HOME-BRANCH
                     DELIMITED BY "  " INTO D-HOME-BRANCH.

           PERFORM WRITE-FORMAT-LINE.

           MOVE GR-CONTACT TO D-CONTACT.
           MOVE SPACES TO NAME-WORKER.
           MOVE HOLD-CITY TO NAME-WORKER.
           INSPECT NAME-WORKER REPLACING FIRST "  " BY ",?".
           MOVE HOLD-STATE TO INSPECT-DESC.
           INSPECT NAME-WORKER REPLACING FIRST "                    "
             BY INSPECT-DESC.
           INSPECT NAME-WORKER REPLACING FIRST "  " BY "??".
           MOVE HOLD-ZIP TO INSPECT-ZIP.
           INSPECT NAME-WORKER REPLACING FIRST "          " BY
             INSPECT-ZIP.
           INSPECT NAME-WORKER REPLACING ALL "?" BY " ".
           MOVE NAME-WORKER TO D-ADDR1.

           MOVE GR-LN-APPROVAL-LIMIT TO D-APPROVAL-AMT.

           PERFORM WRITE-FORMAT-LINE.
       EXIT-DO-LINE.
           EXIT.

      *************************************
       WRITE-FORMAT-LINE SECTION.
      *************************************
           IF GR-LEVEL < 2
              MOVE FORMAT-LINE TO D-LEVEL1.
           IF GR-LEVEL = 2
              MOVE FORMAT-LINE TO D-LEVEL2.
           IF GR-LEVEL = 3
              MOVE FORMAT-LINE TO D-LEVEL3.
           IF GR-LEVEL = 4
              MOVE FORMAT-LINE TO D-LEVEL4.
           IF GR-LEVEL = 5
              MOVE FORMAT-LINE TO D-LEVEL5.
           IF GR-LEVEL = 6
              MOVE FORMAT-LINE TO D-LEVEL6.
           IF GR-LEVEL = 7
              MOVE FORMAT-LINE TO D-LEVEL7.
           IF GR-LEVEL = 8
              MOVE FORMAT-LINE TO D-LEVEL8.
           IF GR-LEVEL = 9
              MOVE FORMAT-LINE TO D-LEVEL9.
           IF GR-LEVEL NOT = HOLD-LEVEL
              MOVE HOLD-LEVEL TO HOLD-PREVIOUS-LEVEL
              MOVE HOLD-NAME TO HOLD-CONT-NAME
              MOVE GR-LEVEL TO HOLD-LEVEL
              MOVE 2 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.
           MOVE SPACES TO FORMAT-LINE.

      ************************************
       WRITE-DETAIL-LINE SECTION.
      ************************************
           ADD LN-FEED LN-COUNT GIVING LN-WK.
           IF LN-WK GREATER THAN 60
              PERFORM HEAD-PAGE.
           MOVE DETAIL-LINE TO PR-REC.
           PERFORM WRITE-PR-REC.
           ADD LN-FEED TO LN-COUNT.
           MOVE SPACES TO DETAIL-LINE.
           MOVE 1 TO LN-FEED.

      ***************************
       HEAD-PAGE SECTION.
      ***************************
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
           MOVE EXT-CONAME-CONAME TO GRLIST-CONAME.
           MOVE EXT-CONAME-SYSDATE TO GRLIST-SYSDATE.
           DISPLAY GRLIST-SCREEN UPON GRLIST-WINDOW-HANDLE.
           MOVE GRLIST-FORM-FIELDS TO WR-BUF.
           MOVE GRLIST-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      ****************************************************
       SETUP-LINK-INFO SECTION.
      ****************************************************
           MOVE GRLIST-HELP--FILE TO HELP-LINK-FILE.
           MOVE GRLIST-HELP--DIR  TO HELP-LINK-DIR.

      ****************************************************
       VDU-CONVERT-FIELD SECTION.
      ****************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBLS/GRLIST_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ********************************************************
       MISC SECTION.
      ********************************************************
       COPY "LIBGB/PHONEIO.CPY".
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/GETPR.CPY".

      ********************************************************
       IO-ROUTINES SECTION.
      ********************************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/GBGRGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBPRINT.CPY".
       COPY "LIBGB/GBGR1RN.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO ERRCD.

      ********************************************************
       END-PROGRAM SECTION.
      ********************************************************
       END-PROG.
           PERFORM CLOSE-FILES.
           PERFORM PRU-OUT.
       EXIT-PROG.
           MOVE "LP/LPL2MU" TO FORM-NAM.
           MOVE FORM-PATH TO FORM-PATHNAME.
       STOP-RUN.
           DESTROY GRLIST-WINDOW-HANDLE.
           EXIT PROGRAM.
