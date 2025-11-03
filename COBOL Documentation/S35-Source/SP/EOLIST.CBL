       IDENTIFICATION DIVISION.
       PROGRAM-ID.      EOLIST.
       DATE-WRITTEN.    1-10-92.
      *****************************************************
      *                      (SP)
      *    DESCRIPTION:  BATCH FILE LISTING
      *****************************************************
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
           "@(#)SP/EOLIST.CBL S34 06/19/21-12:01:32 >".
      ****************************
      *     F I L E   I F N      *
      ****************************
       COPY "LIBGB/GB01EO.CPY".
       COPY "LIBGB/GB01EO_SQL.CPY".
       COPY "LIBGB/GBWSEO.CPY".
       01  PR-IFN               PIC X(2) VALUE "PR".

      **********************
      *   LINE WORKERS     *
      **********************
       01  LN-WORKERS.
           03  LN-FEED          PIC 99       VALUE 0.
           03  LN-COUNT         PIC 999      VALUE 99.
           03  LN-WK            PIC 999      VALUE 0.
       01  PAGE-NUMBER          PIC 9(4)     VALUE 0.

      **********************
      *   MISC WORKERS     *
      **********************
       01  FIRST-TIME           PIC X        VALUE "Y".
       01  MESS-LIST            PIC X(5)     VALUE "MESS.".
       01  HOLD-CODE            PIC X(3)     VALUE SPACES.
       01  RECORDS-FOUND        PIC X        VALUE SPACES.
           88  NO-RECORDS-FOUND              VALUE "N".
       01  CODE-MESS            PIC X(30).

      ***********************************
      *     S C R E E N   F I E L D S   *
      ***********************************
       01  SCREEN-BUF          VALUE " ".
           03  ERRCD           PIC X.
           03  BEG-CODE        PIC X(3).
           03  END-CODE        PIC X(3).

       01  ACCEPT-BUF           PIC X        VALUE " ".

      ***********************************
      *     P R I N T   B U F F E R S   *
      ***********************************

       01  HEAD1.
           03  H-DATE           PIC X(8)     VALUE " ".
           03  FILLER           PIC XX       VALUE " ".
           03  H-HR             PIC 99       VALUE 0.
           03  FILLER           PIC X        VALUE ":".
           03  H-MIN            PIC 99       VALUE 0.
           03  FILLER           PIC X(6)     VALUE " ".
           03  H-NAME           PIC X(40)    VALUE " ".
           03  FILLER           PIC X(5)     VALUE " ".
           03  FILLER           PIC X(5)     VALUE "PAGE ".
           03  H-PAGE-NO        PIC ZZZ9.

       01  HEAD2.
           03  FILLER           PIC X(10) VALUE "USER ID:".
           03  H-USERID         PIC X(10).
           03  FILLER           PIC X(6)  VALUE " ".
           03  H-TITLE          PIC X(31) VALUE " ".

       01  HEAD3.
           03  FILLER       PIC X(11) VALUE "CODE STEP".
           03  FILLER       PIC X(17) VALUE "PROGRAM  COPIES".
           03  FILLER       PIC X(16) VALUE "TRANSFER  NEXT".
           03  FILLER       PIC X(11) VALUE "COMMENT".

       01  DETAIL-LINE          PIC X(80) VALUE SPACES.

       01  D-LINE REDEFINES DETAIL-LINE.
           03  D-CODE           PIC XXX.
           03  FILLER           PIC X(2).
           03  D-STEP           PIC ZZZ9.
           03  FILLER           PIC X.
           03  D-DIR            PIC X(2).
           03  D-SLASH          PIC X.
           03  D-PROGRAM        PIC X(6).
           03  FILLER           PIC X(3).
           03  D-COPIES         PIC 9.
           03  FILLER           PIC X(8).
           03  D-TRANSFER       PIC X.
           03  FILLER           PIC X(6).
           03  D-NEXT           PIC ZZZ9.
           03  FILLER           PIC X(2).
           03  D-COMMENT        PIC X(36).

       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBGB/GETPRW.CPY".
       COPY "LIBWI/EOSCANW.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       01  EOLIST-WINDOW-HANDLE PIC X(10).
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBSP/EOLIST_DEF.CPY".
       COPY "LIBSP/EOLIST_WKS.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

       COPY "LIBGB/GETFMW.CPY".

       SCREEN SECTION.
       COPY "LIBSP/EOLIST_SCN.CPY".

       PROCEDURE DIVISION
             USING FORM-PATHNAME EXIT-PATHNAME.

       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON PR-FILE.
       COPY "LIBGB/DECLARE.CPY".
           PERFORM CLOSE-EO1-FILE.
           PERFORM CLOSE-PR-FILE.
       COPY "LIBGB/DECLAR2.CPY".

      ***************************************
       MAIN-MODULE SECTION.
      ***************************************
           PERFORM SQL-CONNECT.

       COPY "LIBGB/CHKSEC.CPY".

           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "EOLIST" TO VDU-FORM-NAME.

           DISPLAY FLOATING GRAPHICAL WINDOW
                   SCREEN LINE 01 * VDU-WIN-L-OFFSET
                   SCREEN COL 01 * VDU-WIN-C-OFFSET
                   CELL SIZE IS LABEL FONT
                   SIZE 80 + VDU-WIN-W-MOD
                   LINES 24 + VDU-WIN-H-MOD
                   BLANK SCREEN
                   HANDLE IS EOLIST-WINDOW-HANDLE.

           PERFORM FORM-RESET.

           PERFORM INITIALIZATION.
           IF ERRCD NOT = " "
              GO TO END-PROGRAM.
       NEXT-EO.
           PERFORM READ-EO1-FILE-NEXT.
           IF IO-FG = 9
              GO TO END-ROUTINE.
           IF EO-PCODE < BEG-CODE OR > END-CODE
              GO TO NEXT-EO.

           IF EO-STEP = 0
              GO TO NEXT-EO.
           PERFORM DETAIL-LINE-PROCESS.

           GO TO NEXT-EO.
       END-ROUTINE.
           IF FIRST-TIME = "Y"
              MOVE "Y" TO RECORDS-FOUND.
           GO TO END-PROGRAM.

      ***********************************
       INITIALIZATION SECTION.
      ***********************************

           MOVE " " TO ERRCD.
       SETRP.
           MOVE EOLIST-QUEUE-POS TO Q-POS.
           MOVE EOLIST-COPIES-POS TO C-POS.
           MOVE EOLIST-PRU-POS TO P-POS.
           MOVE SPACES TO Q-BUF C-BUF P-BUF.
           PERFORM PRU-IN.
           IF STAT NOT = "00"
              GO TO ENDIT.
       CODE-01.
           MOVE "S  A060MSEL." TO VDU.
           MOVE
            "F7-EXIT, F1, F6-SCAN, F3-ALL, ENTER BEGINNING PROCEDURE:"
                                                 TO VDUBUF.
           PERFORM SCREENIO.
           MOVE "E  A030CODE1." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO SETRP.
           IF VDUSTATUS = "16" GO TO ALL-CODES.
           IF VDUSTATUS = "1<"
              MOVE "S  A030CODE1." TO CODE-MESS
              PERFORM EO-SCAN.
           IF VDUSTATUS = "1>" GO TO ENDIT.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO CODE-01.
           MOVE VDUBUF TO BEG-CODE.
       CODE-02.
           MOVE "S  A060MSEL." TO VDU.
           MOVE "F7-EXIT, F1, F6-SCAN, ^, ENTER ENDING PROCEDURE:"
                                                 TO VDUBUF.
           PERFORM SCREENIO.
           MOVE "E  A030CODE2." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO SETRP.
           IF VDUSTATUS = "1U" GO TO CODE-01.
           IF VDUSTATUS = "1<"
              MOVE "S  A030CODE2." TO CODE-MESS
              PERFORM EO-SCAN.
           IF VDUSTATUS = "1>" GO TO ENDIT.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO CODE-02.
           MOVE VDUBUF TO END-CODE.
           IF BEG-CODE > END-CODE
              MOVE "INVALID CODE RANGE" TO MESS
              PERFORM SEND-MESS
              GO TO CODE-01.
           GO TO OK-YN.
       ALL-CODES.
           MOVE "S  A030CODE1." TO VDU.
           MOVE "   " TO BEG-CODE VDUBUF.
           PERFORM SCREENIO.
           MOVE "S  A030CODE2." TO VDU.
           MOVE "ZZZ" TO END-CODE VDUBUF.
           PERFORM SCREENIO.
       OK-YN.
           MOVE "S  A060MSEL." TO VDU.
           MOVE "F7-EXIT, F1, ^, ACCEPT (Y/N):" TO VDUBUF.
           PERFORM SCREENIO.
           MOVE "E  A010ACCEPT." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO SETRP.
           IF VDUSTATUS = "1U" GO TO CODE-02.
           IF VDUSTATUS = "1>" GO TO ENDIT.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO OK-YN.
           MOVE VDUBUF TO ACCEPT-BUF.
           IF ACCEPT-BUF = "N" GO TO ENDIT.
           IF ACCEPT-BUF NOT = "Y" GO TO OK-YN.


           MOVE EXT-CONAME-CONAME TO H-NAME.
           MOVE EXT-CONAME-SYSDATE TO H-DATE.
           MOVE EXT-CONAME-USER-ID TO H-USERID.

           MOVE " BATCH PROCEDURE LISTING" TO H-TITLE.

           PERFORM OPEN-EO1-FILE.
           MOVE BEG-CODE TO EO-PCODE.
           MOVE 0 TO EO-STEP.
           PERFORM START-EO1-FILE.

           MOVE "LISTING IN PROGRESS...................." TO WR-BUF.
           MOVE MESS-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

           PERFORM OPEN-PR-FILE.

           GO TO EXIT-INITIALIZE.
       ENDIT.
           MOVE "X" TO ERRCD.
       EXIT-INITIALIZE.
           EXIT.

      ***********************************
       EO-SCAN SECTION.
      ***********************************
           MOVE SPACES TO EOSCAN-KEY.
           MOVE "WI/EOSCAN" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH EOSCAN-BUF EXIT-PATHNAME.
           CANCEL FORM-PROGX.
           IF EOSCAN-STATUS NOT = "00" OR EOSCAN-KEY = SPACES
              MOVE "X" TO VDUSTATUS
              GO TO EXIT-EO-SCAN.

           MOVE CODE-MESS TO VDU.
           MOVE EOSCAN-KEY TO VDUBUF.
           PERFORM SCREENIO.
           MOVE EOSCAN-KEY TO VDUBUF.
           MOVE "00" TO VDUSTATUS.

       EXIT-EO-SCAN.
           EXIT.

      ***********************************
       DETAIL-LINE-PROCESS SECTION.
      ***********************************
           IF EO-PCODE NOT = HOLD-CODE
              MOVE 2 TO LN-FEED
              MOVE EO-PCODE TO D-CODE
              MOVE EO-PCODE TO HOLD-CODE.
           MOVE EO-STEP TO D-STEP.
           MOVE EO-DIR TO D-DIR.
           MOVE "/" TO D-SLASH.
           MOVE EO-PROGRAM TO D-PROGRAM.
           MOVE EO-COPIES TO D-COPIES.
           MOVE EO-TRANSFER TO D-TRANSFER.
           MOVE EO-ERROR-STEP TO D-NEXT.
           MOVE EO-COMMENT TO D-COMMENT.
           PERFORM WRITE-DETAIL-LINE.
           MOVE 1 TO LN-FEED.

      ***********************************
       WRITE-DETAIL-LINE SECTION.
      ***********************************
           ADD LN-FEED LN-COUNT GIVING LN-WK.
           IF LN-WK > EXT-CONAME-RPT-LINES
              PERFORM HEAD-PAGE.
           MOVE DETAIL-LINE TO PR-REC.
           PERFORM WRITE-PR-REC.
           ADD LN-FEED TO LN-COUNT.
           MOVE 1 TO LN-FEED.
           MOVE SPACES TO DETAIL-LINE.

      ***********************************
       HEAD-PAGE SECTION.
      ***********************************
           MOVE "N" TO FIRST-TIME.
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

           MOVE HEAD3 TO PR-REC.
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-PR-REC.

       END-HEAD-PAGE.
           MOVE 4 TO LN-COUNT.
           MOVE 2 TO LN-FEED.

       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/TIMEIO.CPY".
       COPY "LIBGB/DATER.CPY".

      ****************************************************
       FORM-RESET SECTION.
      ****************************************************
           MOVE EXT-CONAME-CONAME TO EOLIST-CONAME.
           MOVE EXT-CONAME-SYSDATE TO EOLIST-SYSDATE.
           DISPLAY EOLIST-SCREEN UPON EOLIST-WINDOW-HANDLE.
           MOVE EOLIST-FORM-FIELDS TO WR-BUF.
           MOVE EOLIST-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      ****************************************************
       SETUP-LINK-INFO SECTION.
      ****************************************************
           MOVE EOLIST-HELP--FILE TO HELP-LINK-FILE.
           MOVE EOLIST-HELP--DIR  TO HELP-LINK-DIR.

      ****************************************************
       VDU-CONVERT-FIELD SECTION.
      ****************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBSP/EOLIST_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ****************************************************
       MISC SECTION.
      ****************************************************
       COPY "LIBGB/GETPR.CPY".
       COPY "LIBGB/ACCESS.CPY".

      ****************************************************
       IO-ROUTINE SECTION.
      ****************************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/GBEOGS_SQL.CPY".
       COPY "LIBGB/GBPRINT.CPY".
       COPY "LIBGB/GBEO1RN.CPY".
       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO ERRCD.

      ****************************************************
       END-PROGRAM SECTION.
      ****************************************************
       END-RUN.
           PERFORM CLOSE-FILES.
           PERFORM PRU-OUT.
           IF NO-RECORDS-FOUND
              MOVE "NO RECORDS FOUND" TO MESS
              PERFORM SEND-MESS.
       EXIT-PROG.
           MOVE "SP/EOMENU" TO FORM-NAM.
           MOVE FORM-PATH TO FORM-PATHNAME.
       STOP-RUN.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY EOLIST-SCREEN.
           DESTROY EOLIST-WINDOW-HANDLE.
           EXIT PROGRAM.
