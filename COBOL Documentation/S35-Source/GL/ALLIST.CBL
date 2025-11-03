       IDENTIFICATION DIVISION.
       PROGRAM-ID. ALLIST.
      ******************************************************************
      *  DIR : GL
      *  DESC: GL ALLOCATION-DIST LISTING
      *
      *  MG  000124 FIXED VDUSTATUS/IO-FG/STAT PER DEVELOPMENT MEETING.
      *  CS  011023 ADDED EXITPROG
      * BLM  131112 CHANGED RRFORM LOGIC TO SCREENIO B4 A30 CONVERSION
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

       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(37) VALUE
           "@(#)GL/ALLIST S35 10/25/22-09:18:36 >".
       COPY "LIBGL/GL01AL.CPY".
       COPY "LIBGL/GL01AL_SQL.CPY".
       COPY "LIBGL/GLWSAL.CPY".
       COPY "LIBGL/GL01GL.CPY".
       COPY "LIBGL/GL01GL_SQL.CPY".
       COPY "LIBGL/GLWSGL.CPY".

      *******************************
      *    PROGRAM WORKERS
      *******************************

       01  LN-WORKERS.
           03  LN-FEED           PIC 99       VALUE 0.
           03  LN-COUNT          PIC 99       VALUE 62.
           03  LN-WK             PIC 99       VALUE 0.
       01  PAGE-NUMBER           PIC 999      VALUE 0.

       01  WORK-GLNO1.
           03  WORK-BRANCH       PIC 9(4).
           03  WORK-ACCOUNT      PIC 9(7).
       01  WORK-GLNO REDEFINES WORK-GLNO1  PIC 9(11).

       01  SUB                   PIC 99.
       01  MESS-LIST             PIC X(5)  VALUE "MESS.".
       01  SCREEN-BUF            VALUE SPACES.
           03  BEG-BRNO          PIC 9(4).
           03  BEG-ALID          PIC X(12).
           03  END-ALID          PIC X(12).
           03  ERRCD             PIC X.

       01  CONT-BUF              PIC X.

      **********************************
      *    PRINT BUFFERS
      **********************************

       01  HEAD1.
           03  HD-DATE           PIC X(8)     VALUE SPACES.
           03  FILLER            PIC XX       VALUE SPACES.
           03  HD-TIME           PIC X(8) VALUE SPACES.
           03  FILLER            PIC X(33)    VALUE SPACES.
           03  HD-NAME           PIC X(53)    VALUE SPACES.
           03  FILLER            PIC X(5)     VALUE "PAGE ".
           03  HD-PAGE-NO        PIC ZZ9      VALUE SPACES.

       01  HEAD2.
           03  FILLER            PIC X(10)    VALUE "USER ID:".
           03  HD-USERID         PIC X(28).
           03  H-TITLE           PIC X(57).
           03  FILLER            PIC X(17) VALUE SPACES.

       01  HEAD3.
           03  FILLER            PIC X(40) VALUE
           "     ALLOCATION ID   NO OF ACCTS    TYPE".
           03  FILLER            PIC X(18) VALUE
           "    G/L ALLOCATION".
           03  FILLER            PIC X(41).
           03  FILLER            PIC X(22) VALUE
           "ALLOCATION DESCRIPTION".

       01  DETAIL-LINE           PIC X(132).

       01  DETAIL-LINE1 REDEFINES DETAIL-LINE.
           03  FILLER            PIC X(5).
           03  D-ALIDNO          PIC X(12).
           03  FILLER            PIC X(3).
           03  D-NO-ACCTS        PIC Z9.
           03  FILLER            PIC X(14).
           03  D-TYPE            PIC XX.
           03  FILLER            PIC X(5).
           03  D-GLBR            PIC 9(4).
           03  D-DASH            PIC X.
           03  D-GLAC            PIC 9(7).
           03  FILLER            PIC X(3).
           03  D-PCT             PIC ZZZ.99-.
           03  FILLER            PIC X.
           03  D-GLDS            PIC X(30).
           03  FILLER            PIC X(3).
           03  D-DESC            PIC X(30).

       01  RANGE-LINE REDEFINES DETAIL-LINE.
           03 FILLER             PIC X(35).
           03 RANGE-SELECTION.
              04  R-NUM          PIC 9(12).
              04  R-NUMX  REDEFINES R-NUM.
                  05  FILLER     PIC X(8).
                  05  RNUMX      PIC 9(4).

       COPY "LIBGB/GETPRW.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/DATERW.CPY".

       01  ALLIST-WINDOW-HANDLE  PIC X(10).
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBGL/ALLIST_DEF.CPY".
       COPY "LIBGL/ALLIST_WKS.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

      ****************************************
      *     P R O G R A M   C O N T R O L    *
      ****************************************
       COPY "LIBGB/GETFMW.CPY".

       SCREEN SECTION.
       COPY "LIBGL/ALLIST_SCN.CPY".

       PROCEDURE DIVISION
           USING FORM-PATHNAME EXIT-PATHNAME.
       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
               PR-FILE.
       COPY "LIBGB/DECLARE.CPY".
           PERFORM CLOSE-GL1-FILE.
           PERFORM CLOSE-AL1-FILE.
           CLOSE PR-FILE.
       COPY "LIBGB/DECLAR2.CPY".

      ***********************************************
       MAIN-MODULE SECTION.
      ***********************************************
      D    STOP FORM-PATHNAME.
           PERFORM SQL-CONNECT.

           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "ALLIST" TO VDU-FORM-NAME.

           DISPLAY STANDARD GRAPHICAL WINDOW
                   SCREEN LINE 01 * VDU-WIN-L-OFFSET
                   SCREEN COL 01 * VDU-WIN-C-OFFSET
           COLOR IS VDU-WCOLOR-TCLP,CONTROL VALUE IS VDU-WCTRL-TCLP
                   CELL SIZE IS LABEL FONT
                   SIZE 80 + VDU-WIN-W-MOD
                   LINES 24 + VDU-WIN-H-MOD
                   BLANK SCREEN
      *             WITH NO SCROLL
      *             WITH NO WRAP
                   HANDLE IS ALLIST-WINDOW-HANDLE.

       COPY "LIBGB/CHKSEC.CPY".
           PERFORM FORM-RESET.

           IF EXT-ROUTE-BUF NOT = "00"
              MOVE "PROGRAM NOT BATCHABLE" TO MESS
              PERFORM SEND-MESS
              MOVE "99" TO EXT-ROUTE-BUF
              GO TO END-PROGRAM.

           PERFORM INITIALIZATION.
           IF ERRCD NOT = " "
              GO TO END-PROGRAM.

           MOVE BEG-ALID TO AL-ID
                            QAL1-WBEG-ID.
           MOVE END-ALID TO QAL1-WEND-ID.

           PERFORM START-AL1-FILE.

       NEXT-AL.
           PERFORM READ-AL1-FILE-NEXT.
           IF IO-BAD
              PERFORM PRINT-RANGE-LINES
              GO TO END-PROGRAM.

           IF AL-ID > END-ALID
              GO TO NEXT-AL.

           MOVE AL-ID TO D-ALIDNO.
           MOVE AL-NO-ACCTS TO D-NO-ACCTS.
           MOVE AL-TYPE TO D-TYPE.
           MOVE AL-DESC TO D-DESC.
           MOVE 1 TO SUB.
           MOVE 3 TO LN-FEED.
       PRINT-GL-LOOP.
           PERFORM SETUP-DISTRIBUTION.
           IF SUB = 1 OR
               AL-ACCT(SUB) NOT = 0
                 PERFORM WRITE-DETAIL-LINE
                 MOVE 1 TO LN-FEED.
           ADD 1 TO SUB.
           IF SUB < 11 GO TO PRINT-GL-LOOP.
           GO TO NEXT-AL.

      **********************************************
       SETUP-DISTRIBUTION SECTION.
      **********************************************
           PERFORM OPEN-GL1-FILE.

           MOVE AL-ACCT(SUB) TO WORK-GLNO GL-BR-ACCOUNT.
           IF GL-BRANCH = 0
              MOVE BEG-BRNO TO GL-BRANCH.

           PERFORM READ-GL1-FILE.
           PERFORM CLOSE-GL1-FILE.
           IF IO-BAD
               MOVE "*** GL NOT ON FILE ***" TO GL-DESCRIPTION.

           MOVE WORK-BRANCH TO D-GLBR.
           MOVE "-" TO D-DASH.
           MOVE WORK-ACCOUNT TO D-GLAC.
           MOVE GL-DESCRIPTION TO D-GLDS.
           MOVE AL-PCT(SUB) TO D-PCT.
           MOVE AL-ACCT(SUB) TO WORK-GLNO.


      *************************************
       INITIALIZATION SECTION.
      *************************************
           MOVE ALLIST-QUEUE-POS TO Q-POS.
           MOVE ALLIST-COPIES-POS TO C-POS.
           MOVE ALLIST-PRU-POS   TO P-POS.
           MOVE SPACES TO Q-BUF C-BUF P-BUF.
       SETRP.
           PERFORM PRU-IN.
           IF STAT-BAD
              GO TO ENDIT.
       ENTER-BRNO.
           MOVE "ENNN040BEGBRNO." TO VDU.
           PERFORM SCREENIO.
           IF F1-KEY GO TO SETRP.
           IF F7-KEY GO TO ENDIT.
           IF NOT ENTER-KEY GO TO ENTER-BRNO.
           MOVE VDUNUM0 TO BEG-BRNO.
       ENTER-BEG-ALLOC.
           MOVE "E RA120BEGALID." TO VDU.
           PERFORM SCREENIO.
           IF F1-KEY GO TO SETRP.
           IF F3-KEY GO TO ALL-ALLOC.
           IF UP-KEY GO TO ENTER-BRNO.
           IF F7-KEY GO TO ENDIT.
           IF NOT ENTER-KEY GO TO ENTER-BEG-ALLOC.
           MOVE VDUBUF TO BEG-ALID.
       ENTER-END-ALLOC.
           MOVE "E RA120ENDALID." TO VDU.
           PERFORM SCREENIO.
           IF F1-KEY GO TO SETRP.
           IF UP-KEY GO TO ENTER-BEG-ALLOC.
           IF F7-KEY GO TO ENDIT.
           IF NOT ENTER-KEY GO TO ENTER-END-ALLOC.
           MOVE VDUBUF TO END-ALID.
           IF BEG-ALID > END-ALID
              GO TO ENTER-BEG-ALLOC.
           GO TO OK-YN.
       ALL-ALLOC.

           MOVE "S  A000BEGALID." TO VDU.
           MOVE SPACES TO BEG-ALID VDUBUF.
           PERFORM SCREENIO.
           MOVE "S  A000ENDALID." TO VDU.
           MOVE ALL "Z" TO END-ALID.
           CALL "C$TOLOWER" USING END-ALID, VALUE 12.
           MOVE END-ALID TO VDUBUF.
           PERFORM SCREENIO.
       OK-YN.
           MOVE "E RA010CONT." TO VDU.
           PERFORM SCREENIO.
           IF F1-KEY GO TO SETRP.
           IF UP-KEY GO TO ENTER-END-ALLOC.
           IF F7-KEY GO TO ENDIT.
           IF NOT ENTER-KEY GO TO OK-YN.
           MOVE VDUBUF TO CONT-BUF.
           IF CONT-BUF NOT = "Y"
              IF CONT-BUF NOT = "N"
                 GO TO OK-YN.

           MOVE " " TO ERRCD.
           MOVE BEG-ALID TO AL1-KEY.

           MOVE "GENERAL LEDGER ALLOCATION " TO H-TITLE WR-BUF.
           MOVE MESS-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

           PERFORM OPEN-AL1-FILE.
           PERFORM OPEN-PR-FILE.

           GO TO EXIT-INITIALIZE.
       ENDIT.
           MOVE "X" TO ERRCD.
       EXIT-INITIALIZE.
           EXIT.

      *******************************
       WRITE-DETAIL-LINE SECTION.
      *******************************
           ADD LN-FEED LN-COUNT GIVING LN-WK.
           IF LN-WK GREATER THAN 61
              PERFORM HEAD-PAGE.
           MOVE DETAIL-LINE  TO PR-REC.
           PERFORM WRITE-PR-REC.
           ADD LN-FEED TO LN-COUNT.
           MOVE 1 TO LN-FEED.
           MOVE SPACES TO DETAIL-LINE.

      ****************************
       HEAD-PAGE SECTION.
      ****************************
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
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-PR-REC.
           MOVE HEAD3 TO PR-REC.
           PERFORM WRITE-PR-REC.
       END-HEAD-PAGE.
           MOVE 7 TO LN-COUNT.
           MOVE 2 TO LN-FEED.

      **********************************************************
       FORM-RESET SECTION.
      **********************************************************
           MOVE EXT-CONAME-CONAME TO ALLIST-CONAME.
           MOVE EXT-CONAME-SYSDATE TO ALLIST-SYSDATE.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DISPLAY ALLIST-SCREEN UPON ALLIST-WINDOW-HANDLE.
           MOVE ALLIST-FORM-FIELDS TO WR-BUF.
           MOVE ALLIST-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      **********************************************************
       SETUP-LINK-INFO SECTION.
      **********************************************************
           MOVE ALLIST-HELP--FILE TO HELP-LINK-FILE.
           MOVE ALLIST-HELP--DIR  TO HELP-LINK-DIR.

      **********************************************************
       VDU-CONVERT-FIELD SECTION.
      **********************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBGL/ALLIST_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

       COPY "LIBGB/TIMEIO.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/DATER.CPY".

      **********************************************************
       MISC SECTION.
      **********************************************************
       COPY "LIBGB/GETPR.CPY".
       COPY "LIBGB/ACCESS.CPY".
       PRINT-RANGE-LINES.
           MOVE SPACES TO DETAIL-LINE.
           MOVE 4 TO LN-FEED.
           MOVE "BRANCH : " TO RANGE-LINE.
           MOVE BEG-BRNO TO RNUMX.
           PERFORM WRITE-DETAIL-LINE.
           MOVE "BEG ALLOC ID : " TO RANGE-LINE.
           MOVE BEG-ALID TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
           MOVE "END ALLOC ID : " TO RANGE-LINE.
           MOVE END-ALID TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.

      **********************************************************
       IO-ROUTINES SECTION.
      **********************************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGL/GLGLGS_SQL.CPY".
       COPY "LIBGL/GLALGS_SQL.CPY".
       COPY "LIBGB/GBPRINT.CPY".
       COPY "LIBGL/GLGL1RN.CPY".
       COPY "LIBGL/GLAL1RN.CPY".
       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO ERRCD.

      **********************************************************
       END-PROGRAM SECTION.
      **********************************************************
       END-PROG.
           PERFORM CLOSE-FILES.
           PERFORM PRU-OUT.
       EXIT-PROG.
           MOVE EXIT-PATHNAME TO FORM-PATHNAME.
       STOP-RUN.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY ALLIST-SCREEN.
           DESTROY ALLIST-WINDOW-HANDLE.
           EXIT PROGRAM.
