       IDENTIFICATION DIVISION.
       PROGRAM-ID.      GRLST4.
      ************************************************************
      *                 (LS)
      *
      *  DESCRIPTION:   GROUP SUMMARY FILE LISTING
      *                 WORLD PR# 469
      *
      *  REV:
      *  BLF 061110 EXPANDED ALL GROUP SUBSCRIPTS TO ACCOMODATE
      *             EXPANDED GROUP SEARCH TABLES, WORLD PR# XXXX
      ************************************************************
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

       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)LS/GRLST4.CBL S34 06/10/21-12:27:34 >".

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

       01  TOTAL-GROUPS        PIC 9(4)     VALUE 0.

       01  MESS-LIST           PIC X(5)     VALUE "MESS.".
       01  ERRCD               PIC X        VALUE SPACES.

       01  BEG-GROUP           PIC X(4).
       01  END-GROUP           PIC X(4).


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
           "  GROUP SUMMARY FILE LISTING  ".
           03  FILLER          PIC X(45)    VALUE SPACES.
           03  FILLER          PIC X(6)     VALUE "GRLST4".
       01  HEAD3.
           03  FILLER          PIC X(6)     VALUE "ID ".
           03  FILLER          PIC X(7)     VALUE "LEVEL ".
           03  FILLER          PIC X(12)    VALUE "  GROUP NAME".

       01  DETAIL-LINE                      VALUE SPACES.
           03  D-ID            PIC X(4).
           03  FILLER          PIC X(4).
           03  D-LEVEL         PIC 9.
           03  FILLER          PIC X(6).
           03  D-NAME          PIC X(20).
           03  FILLER          PIC X(97).

       01  TOTAL-LINE REDEFINES DETAIL-LINE.
           03  FILLER          PIC X(10).
           03  D-TOTAL-PROMPT  PIC X(30).
           03  D-TOTAL         PIC ZZZ9.
           03  FILLER          PIC X(88).

       01  GRLST4-WINDOW-HANDLE PIC X(10).

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
       COPY "LIBLS/GRLST4_DEF.CPY".
       COPY "LIBLS/GRLST4_WKS.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

      /***************************************
      *     P R O G R A M   C O N T R O L    *
      ****************************************
       COPY "LIBGB/GETFMW.CPY".

       SCREEN SECTION.
       COPY "LIBLS/GRLST4_SCN.CPY".

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


      ****************************************************
       MAIN-MODULE SECTION.
      ****************************************************
           PERFORM SQL-CONNECT.

       COPY "LIBGB/CHKSEC.CPY".
           MOVE "GRLST4" TO VDU-FORM-NAME.

           DISPLAY FLOATING GRAPHICAL WINDOW
                   SCREEN LINE 01 * VDU-WIN-L-OFFSET
                   SCREEN COL 01 * VDU-WIN-C-OFFSET
                   CELL SIZE IS LABEL FONT
                   SIZE 80 + VDU-WIN-W-MOD
                   LINES 24 + VDU-WIN-H-MOD
                   BLANK SCREEN
                   HANDLE IS GRLST4-WINDOW-HANDLE.

           PERFORM FORM-RESET.

           IF EXT-ROUTE-BUF NOT = "00"                                          
              MOVE "PROGRAM NOT BATCHABLE" TO MESS                              
              PERFORM SEND-MESS        
              MOVE "99" TO EXT-ROUTE-BUF                                        
              GO TO END-PROGRAM.                                                

           PERFORM INITIALIZATION.

           MOVE BEG-GROUP TO GR-ID.
           PERFORM START-GR1-FILE.

       READ-GRFILE-NEXT.
           PERFORM READ-GR1-FILE-NEXT.
           IF IO-BAD
              PERFORM PRINT-TOTAL-LINES
              GO TO END-PROGRAM.
           IF GR-ID > END-GROUP
              PERFORM PRINT-TOTAL-LINES
              GO TO END-PROGRAM.

           PERFORM DO-LINE.
           ADD 1 TO TOTAL-GROUPS.

           GO TO READ-GRFILE-NEXT.

      ****************************************
       INITIALIZATION SECTION.
      ****************************************
           MOVE GRLST4-QUEUE-POS TO Q-POS.
           MOVE GRLST4-COPIES-POS TO C-POS.
           MOVE GRLST4-PRU-POS TO P-POS.
           MOVE SPACES TO Q-BUF C-BUF P-BUF.
           PERFORM PRU-IN.
           IF STAT NOT = "00"
              GO TO END-PROGRAM.

       REC-BEG-GRID.
           MOVE "E  A000GRID1." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "16" GO TO ALL-GROUPS.
           IF VDUSTATUS = "1>" GO TO END-PROGRAM.

           IF VDUSTATUS = "1<"
              MOVE "WI/GRSCAN" TO FORM-NAM
              CALL FORM-PROGX USING FORM-PATH GRSCAN-BUF EXIT-PATHNAME
              CANCEL FORM-PROGX
              IF GRSCAN-STATUS NOT = "00"
                 GO TO REC-BEG-GRID
              ELSE
                 MOVE "S  A000GRID1." TO VDU
                 MOVE GRSCAN-KEY TO VDUBUF
                 PERFORM SCREENIO.
           MOVE VDUBUF TO WGR-WORD BEG-GROUP.
           MOVE 0 TO WGR-MIN-LEVEL.
           PERFORM CHECK-MEMBER.
           IF WGR-ERR > 0
              MOVE WGR-MSG(WGR-ERR) TO MESS
              PERFORM SEND-MESS
              GO TO REC-BEG-GRID.
       REC-END-GRID.
           MOVE "E  A000GRID2." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U" GO TO REC-BEG-GRID.
           IF VDUSTATUS = "1>" GO TO END-PROGRAM.

           IF VDUSTATUS = "1<"
              MOVE "WI/GRSCAN" TO FORM-NAM
              CALL FORM-PROGX USING FORM-PATH GRSCAN-BUF EXIT-PATHNAME
              CANCEL FORM-PROGX
              IF GRSCAN-STATUS NOT = "00"
                 GO TO REC-END-GRID
              ELSE
                 MOVE "S  A000GRID2." TO VDU
                 MOVE GRSCAN-KEY TO VDUBUF
                 PERFORM SCREENIO.
           MOVE VDUBUF TO WGR-WORD END-GROUP.
           MOVE 0 TO WGR-MIN-LEVEL.
           PERFORM CHECK-MEMBER.
           IF WGR-ERR > 0
              MOVE WGR-MSG(WGR-ERR) TO MESS
              PERFORM SEND-MESS
              GO TO REC-END-GRID.
           GO TO ACCEPT-IT.
       ALL-GROUPS.
           MOVE "S  A000GRID1." TO VDU.
           MOVE "    " TO BEG-GROUP VDUBUF.
           PERFORM SCREENIO.
           MOVE "S  A000GRID2." TO VDU.
           MOVE "ZZZZ" TO END-GROUP VDUBUF.
           PERFORM SCREENIO.
       ACCEPT-IT.
           MOVE "E  A000ACCEPT." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO END-PROGRAM.
           IF VDUSTATUS = "18" GO TO REC-END-GRID.
           IF VDUBUF = "Y" GO TO CONTINUE-IT.
           IF VDUBUF = "N" GO TO REC-END-GRID.
           GO TO ACCEPT-IT.
       CONTINUE-IT.
           PERFORM OPEN-GR1-FILE.
           PERFORM OPEN-BR-FILE.

           MOVE " GROUP SUMMARY FILE LISTING IN PROGRESS " TO WR-BUF.
           MOVE MESS-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

           PERFORM OPEN-PR-FILE.

      ******************************************
       DO-LINE SECTION.
      ******************************************
           MOVE GR-ID TO D-ID.
           MOVE GR-LEVEL TO D-LEVEL.
           MOVE GR-NAME TO D-NAME.
           PERFORM WRITE-DETAIL-LINE.

       EXIT-DO-LINE.
           EXIT.

      ***************************
       PRINT-TOTAL-LINES SECTION.
      ***************************
           MOVE 2 TO LN-FEED.
           MOVE "TOTAL NUMBER OF GROUPS   :" TO D-TOTAL-PROMPT.
           MOVE TOTAL-GROUPS TO D-TOTAL.
           PERFORM WRITE-DETAIL-LINE.

      ***************************
       WRITE-DETAIL-LINE SECTION.
      ***************************
           ADD LN-FEED LN-COUNT GIVING LN-WK.
           IF LN-WK GREATER THAN EXT-CONAME-RPT-LINES
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

           MOVE HEAD3 TO PR-REC.
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-PR-REC.
       END-HEAD-PAGE.
           MOVE 1 TO LN-COUNT.
           MOVE 2 TO LN-FEED.

       COPY "LIBGB/GRWORK.CPY".
       COPY "LIBGB/TIMEIO.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/DATER.CPY".

      ****************************************************
       FORM-RESET SECTION.
      ****************************************************
           MOVE EXT-CONAME-CONAME TO GRLST4-CONAME.
           MOVE EXT-CONAME-SYSDATE TO GRLST4-SYSDATE.
           DISPLAY GRLST4-SCREEN UPON GRLST4-WINDOW-HANDLE.
           MOVE GRLST4-FORM-FIELDS TO WR-BUF.
           MOVE GRLST4-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      ****************************************************
       SETUP-LINK-INFO SECTION.
      ****************************************************
           MOVE GRLST4-HELP--FILE TO HELP-LINK-FILE.
           MOVE GRLST4-HELP--DIR  TO HELP-LINK-DIR.

      ****************************************************
       VDU-CONVERT-FIELD SECTION.
      ****************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBLS/GRLST4_EVA.CPY".
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
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY GRLST4-SCREEN.
           DESTROY GRLST4-WINDOW-HANDLE.
           EXIT PROGRAM.
