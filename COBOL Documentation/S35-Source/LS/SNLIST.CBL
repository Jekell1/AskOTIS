       IDENTIFICATION DIVISION.
       PROGRAM-ID.      SNLIST.
       DATE-WRITTEN.    111010.
      ******************************************************************
      *
      *                 (LS)
      *  DESCRIPTION:   SECURITY NAME FILE LISTING
      *
      * REV:
      * KEC 2015.1112 REMOVED USE OF SN-DIR
      *
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
           "@(#)LS/SNLIST S34 07/26/21-12:54:44 >".
      **********************************
      *       F I L E   I F N          *
      **********************************
       COPY "LIBGB/GB01SN.CPY".
       COPY "LIBGB/GB01SN_SQL.CPY".
       COPY "LIBGB/GBWSSN.CPY".
       01  PR-IFN          PIC X(4) VALUE "0APR".

      **********************
      *   LINE WORKERS     *
      **********************
       01  LN-WORKERS.
           03  LN-FEED          PIC 99       VALUE 0.
           03  LN-COUNT         PIC 9(3)     VALUE 99.
           03  LN-WK            PIC 9(3)     VALUE 0.
       01  PAGE-NUMBER          PIC 9(4)     VALUE 0.

      **********************
      *   MISC WORKERS     *
      **********************
       01  FIRST-TIME           PIC X        VALUE "Y".
       01  RECORDS-FOUND        PIC X        VALUE " ".
           88  NO-RECORDS-FOUND              VALUE "N".
       01  MESS-LIST            PIC X(5)     VALUE "MESS.".
       01  ERRCD                PIC X.

       01  BEG-SQ          PIC 9(8).
       01  END-SQ          PIC 9(8).
       01  BEG-FORM        PIC X(6).
       01  END-FORM        PIC X(6).
       01  REPORT-ORDER    PIC X.
       01  PURCHASABLE-FG  PIC X.

      ***********************************
      *     P R I N T   B U F F E R S   *
      ***********************************
       01  HEAD1.
           03  H-DATE           PIC X(8)     VALUE " ".
           03  FILLER           PIC XX       VALUE " ".
           03  H-HR             PIC 99       VALUE 0.
           03  FILLER           PIC X        VALUE ":".
           03  H-MIN            PIC 99       VALUE 0.
           03  FILLER           PIC X(5)     VALUE " ".
           03  H-NAME           PIC X(40)    VALUE " ".
           03  FILLER           PIC X(11)    VALUE " ".
           03  FILLER           PIC X(5)     VALUE "PAGE ".
           03  H-PAGE-NO        PIC ZZZ9.

       01  HEAD2.
           03  FILLER           PIC X(10)    VALUE "USER ID:".
           03  H-USERID         PIC X(10).
           03  FILLER           PIC X(5)     VALUE " ".
           03  FILLER           PIC X(41)    VALUE
           " - - - SECURITY NAME FILE LISING  - - -".

       01  HEAD3.
           03  FILLER           PIC X(34) VALUE
           "        TRANSACTION    SEQUENCE   ".
           03  FILLER           PIC X(38) VALUE
           "PURCHASABLE  DESCRIPTION".

       01  HEAD4.
           03  FILLER           PIC X(34) VALUE
           "SEQUENCE             TRANSACTION  ".
           03  FILLER           PIC X(38) VALUE
           "PURCHASABLE  DESCRIPTION".

       01  DETAIL-LINE          PIC X(80) VALUE SPACES.

       01  DETAIL1 REDEFINES DETAIL-LINE.
           03  FILLER           PIC X(8).
           03  D-FORM           PIC X(10).
           03  FILLER           PIC X(5).
           03  D-SEQ            PIC Z(8).
           03  FILLER           PIC X(6).
           03  D-PURCHASABLE    PIC X.
           03  FILLER           PIC X(9).
           03  D-DESC           PIC X(30).

       01  DETAIL2 REDEFINES DETAIL-LINE.
           03  D-SEQ2           PIC Z(8).
           03  FILLER           PIC X(7).
           03  FILLER           PIC X(6).
           03  D-FORM2          PIC X(10).
           03  FILLER           PIC X(6).
           03  D-PURCHASABLE2   PIC X.
           03  FILLER           PIC X(9).
           03  D-DESC2          PIC X(30).

       01  RANGE-LINE REDEFINES DETAIL-LINE.
           03  FILLER           PIC X(30).
           03  RANGE-SELECTION  PIC X(50).

       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBGB/GETPRW.CPY".
       01  SNLIST-WINDOW-HANDLE PIC X(10).
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBLS/SNLIST_DEF.CPY".
       COPY "LIBLS/SNLIST_WKS.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

      /***************************************
      *     P R O G R A M   C O N T R O L    *
      ****************************************
       COPY "LIBGB/GETFMW.CPY".

       SCREEN SECTION.
       COPY "LIBLS/SNLIST_SCN.CPY".

       PROCEDURE DIVISION
             USING FORM-PATHNAME EXIT-PATHNAME.

       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
               PR-FILE.
       COPY "LIBGB/DECLARE.CPY".
           PERFORM CLOSE-SN2-FILE.
           PERFORM CLOSE-SN1-FILE.
           PERFORM CLOSE-PR-FILE.
       COPY "LIBGB/DECLAR2.CPY".

      *********************************************
       MAIN-MODULE SECTION.
      *********************************************
           PERFORM SQL-CONNECT.

       COPY "LIBGB/CHKSEC.CPY".

           MOVE "SNLIST" TO VDU-FORM-NAME.

           IF EXT-ROUTE-BUF NOT = "00"                                          
              MOVE "PROGRAM NOT BATCHABLE" TO MESS                              
              PERFORM SEND-MESS        
              MOVE "99" TO EXT-ROUTE-BUF                                        
              GO TO END-PROGRAM.                                                

           DISPLAY FLOATING GRAPHICAL WINDOW
                   SCREEN LINE 01 * VDU-WIN-L-OFFSET
                   SCREEN COL 01 * VDU-WIN-C-OFFSET
                   CELL SIZE IS LABEL FONT
                   SIZE 80 + VDU-WIN-W-MOD
                   LINES 24 + VDU-WIN-H-MOD
                   BLANK SCREEN
                   HANDLE IS SNLIST-WINDOW-HANDLE.

           PERFORM FORM-RESET.

      D    STOP MESS.


           PERFORM INITIALIZATION.
           IF ERRCD NOT = " "
              GO TO END-PROGRAM.

           IF REPORT-ORDER = "2"
              GO TO NEXT-SN2.

       NEXT-SN1.
           PERFORM READ-SN1-FILE-NEXT.
           IF IO-BAD
              GO TO END-ROUTINE.

           IF SN-FORM < BEG-FORM OR SN-FORM > END-FORM
              GO TO NEXT-SN1.

           IF PURCHASABLE-FG = "O"
              IF SN-PURCHASABLE = "N"
                 GO TO NEXT-SN1.
           IF PURCHASABLE-FG = "S"
              IF SN-PURCHASABLE = "Y"
                 GO TO NEXT-SN1.

           PERFORM DETAIL-LINE-PROCESS.

           GO TO NEXT-SN1.
       NEXT-SN2.
           PERFORM READ-SN2-FILE-NEXT.
           IF IO-BAD
              GO TO END-ROUTINE.

           IF SN-SEQ > END-SQ
              GO TO END-ROUTINE.

           IF SN-SEQ < BEG-SQ
              GO TO NEXT-SN2.
           IF PURCHASABLE-FG = "O"
              IF SN-PURCHASABLE = "N"
                 GO TO NEXT-SN2.
           IF PURCHASABLE-FG = "S"
              IF SN-PURCHASABLE = "Y"
                 GO TO NEXT-SN2.

           PERFORM DETAIL-LINE-PROCESS2.

           GO TO NEXT-SN2.
       END-ROUTINE.
           IF FIRST-TIME = "Y"
              MOVE "N" TO RECORDS-FOUND
              GO TO END-PROGRAM.

           PERFORM PRINT-RANGE-LINES.
           GO TO END-PROGRAM.

      ***********************************
       INITIALIZATION SECTION.
      ***********************************
       SETRP.
           MOVE SNLIST-QUEUE-POS TO Q-POS.
           MOVE SNLIST-COPIES-POS TO C-POS.
           MOVE SNLIST-PRU-POS TO P-POS.
           MOVE SPACES TO Q-BUF C-BUF P-BUF.
           PERFORM PRU-IN.
           IF STAT-BAD
              GO TO ENDIT.
       RRORD.
           MOVE "E  A000ORD." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO SETRP.
           IF VDUSTATUS = "1>" GO TO ENDIT.
           IF VDUSTATUS NOT = "00" AND NOT = "1D"
              GO TO RRORD.
           MOVE VDUBUF TO REPORT-ORDER.

           IF REPORT-ORDER = "1" GO TO TR-01.
           IF REPORT-ORDER NOT = "2" GO TO RRORD.
           GO TO SQ-01.

       TR-01.
           MOVE "E  A000TR1." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO SETRP.
           IF VDUSTATUS = "1U" GO TO RRORD.
           IF VDUSTATUS = "1>" GO TO ENDIT.
           IF VDUSTATUS = "16" GO TO ALL-TR.
           IF VDUSTATUS NOT = "00" AND NOT = "1D"
              GO TO TR-01.
           MOVE VDUBUF TO BEG-FORM.
       TR-02.
           MOVE "E  A000TR2." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO SETRP.
           IF VDUSTATUS = "1U" GO TO TR-01.
           IF VDUSTATUS = "1>" GO TO ENDIT.
           IF VDUSTATUS NOT = "00" AND NOT = "1D"
              GO TO TR-02.
           MOVE VDUBUF TO END-FORM.
           IF END-FORM = "  " GO TO TR-02.
           IF BEG-FORM > END-FORM
              MOVE " INVALID TRANS RANGE !" TO MESS
              PERFORM SEND-MESS
              GO TO TR-01.
           GO TO PURCHASABLE-SIO.
       ALL-TR.
           MOVE "S  A000TR1." TO VDU.
           MOVE "  " TO BEG-FORM VDUBUF
           PERFORM SCREENIO.
           MOVE "S  A000TR2." TO VDU.
           MOVE "ZZZZZZ" TO END-FORM VDUBUF
           PERFORM SCREENIO.
           GO TO PURCHASABLE-SIO.
       SQ-01.
           MOVE "EN N080SQ1." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO SETRP.
           IF VDUSTATUS = "1U" GO TO RRORD.
           IF VDUSTATUS = "1>" GO TO ENDIT.
           IF VDUSTATUS = "16" GO TO ALL-SQ.
           IF VDUSTATUS NOT = "00" AND NOT = "1D"
              GO TO SQ-01.
           MOVE VDUNUM0 TO BEG-SQ.
       SQ-02.
           MOVE "EN N080SQ2." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO SETRP.
           IF VDUSTATUS = "1U" GO TO SQ-01.
           IF VDUSTATUS = "1>" GO TO ENDIT.
           IF VDUSTATUS NOT = "00" AND NOT = "1D"
              GO TO SQ-02.
           MOVE VDUNUM0 TO END-SQ.
           IF END-SQ = 0 GO TO SQ-02.
           IF BEG-SQ > END-SQ
              MOVE " INVALID SEQUENCE RANGE !" TO MESS
              PERFORM SEND-MESS
              GO TO SQ-01.
           GO TO PURCHASABLE-SIO.
       ALL-SQ.
           MOVE "SN N080SQ1." TO VDU.
           MOVE 0 TO BEG-SQ VDUNUM0
           PERFORM SCREENIO.
           MOVE "SN N080SQ2." TO VDU.
           MOVE 99999999 TO END-SQ VDUNUM0
           PERFORM SCREENIO.
           GO TO PURCHASABLE-SIO.
       PURCHASABLE-SIO.
           MOVE "E  A000PURCHASABLE." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO SETRP.
           IF VDUSTATUS = "1U"
              IF REPORT-ORDER = "1"
                 GO TO RRORD 
              ELSE
                 GO TO SQ-01.
           IF VDUSTATUS = "1>" GO TO ENDIT.
           IF VDUSTATUS NOT = "00" AND NOT = "1D"
              GO TO PURCHASABLE-SIO.
           MOVE VDUBUF TO PURCHASABLE-FG.
           IF PURCHASABLE-FG NOT = "I" AND NOT = "S" AND NOT = "O"
              GO TO PURCHASABLE-SIO.

       OK-YN.
           MOVE " " TO ERRCD.
           MOVE "E  A000ACCEPT." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO SETRP.
           IF VDUSTATUS = "1U" GO TO PURCHASABLE-SIO.
           IF VDUSTATUS = "1>" GO TO ENDIT.
           IF VDUSTATUS NOT = "00" AND NOT = "1D"
              GO TO OK-YN.
           IF VDUBUF NOT = "Y" AND NOT = "N"
              GO TO OK-YN.


      D    STOP MESS.
           IF REPORT-ORDER = "1"
              PERFORM OPEN-SN1-FILE
              MOVE 0        TO BEG-SQ
              MOVE 99999999 TO END-SQ
              MOVE BEG-FORM TO SN-FORM
              PERFORM START-SN1-FILE
           ELSE
              PERFORM OPEN-SN2-FILE
              MOVE BEG-SQ   TO SN-SEQ
              MOVE BEG-FORM TO SN-FORM
              PERFORM START-SN2-FILE.

           MOVE " " TO ERRCD.
           MOVE EXT-CONAME-CONAME TO H-NAME.
           MOVE EXT-CONAME-SYSDATE TO H-DATE.
           MOVE EXT-CONAME-USER-ID TO H-USERID.

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
       DETAIL-LINE-PROCESS SECTION.
      ***********************************
           MOVE SN-FORM        TO D-FORM.
           MOVE SN-SEQ         TO D-SEQ.
           MOVE SN-PURCHASABLE TO D-PURCHASABLE.
           MOVE SN-DESC        TO D-DESC.
           PERFORM WRITE-DETAIL-LINE.
           MOVE 1 TO LN-FEED.

      ***********************************
       DETAIL-LINE-PROCESS2 SECTION.
      ***********************************
           MOVE SN-FORM        TO D-FORM2.
           MOVE SN-SEQ         TO D-SEQ2.
           MOVE SN-PURCHASABLE TO D-PURCHASABLE2.
           MOVE SN-DESC        TO D-DESC2.
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

           IF REPORT-ORDER = "1"
              MOVE HEAD3 TO PR-REC
           ELSE
              MOVE HEAD4 TO PR-REC.
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-PR-REC.

       END-HEAD-PAGE.
           MOVE 4 TO LN-COUNT.
           MOVE 2 TO LN-FEED.

      ***********************************
       PRINT-RANGE-LINES SECTION.
      ***********************************
           MOVE 2 TO LN-FEED.
           MOVE "BEGINNING TRANSACTION" TO RANGE-LINE.
           MOVE BEG-FORM TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
           MOVE "ENDING TRANSACTION" TO RANGE-LINE.
           MOVE END-FORM TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "BEGINNING SEQUENCE" TO RANGE-LINE.
           MOVE BEG-SQ TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
           MOVE "ENDING SEQUENCE" TO RANGE-LINE.
           MOVE END-SQ TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "REPORT ORDER" TO RANGE-LINE.
           IF REPORT-ORDER = "1"
              MOVE "DIR / TRANS" TO RANGE-SELECTION
           ELSE
              MOVE "SEQUENCE" TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.

       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/TIMEIO.CPY".
       COPY "LIBGB/DATER.CPY".

      ****************************************************
       FORM-RESET SECTION.
      ****************************************************
           MOVE EXT-CONAME-CONAME TO SNLIST-CONAME.
           MOVE EXT-CONAME-SYSDATE TO SNLIST-SYSDATE.
           DISPLAY SNLIST-SCREEN UPON SNLIST-WINDOW-HANDLE.
           MOVE SNLIST-FORM-FIELDS TO WR-BUF.
           MOVE SNLIST-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      ****************************************************
       SETUP-LINK-INFO SECTION.
      ****************************************************
           MOVE SNLIST-HELP--FILE TO HELP-LINK-FILE.
           MOVE SNLIST-HELP--DIR  TO HELP-LINK-DIR.

      ****************************************************
       VDU-CONVERT-FIELD SECTION.
      ****************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBLS/SNLIST_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ****************************************************
       MISC SECTION.
      ****************************************************
       COPY "LIBGB/GETPR.CPY".
       COPY "LIBGB/ACCESS.CPY".

      ****************************************************
       IO-ROUTINES SECTION.
      ****************************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/GBSNGS_SQL.CPY".
       COPY "LIBGB/GBPRINT.CPY".
       COPY "LIBGB/GBSN1RN.CPY".
       COPY "LIBGB/GBSN2RN.CPY".
       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO ERRCD.

      ****************************************************
       END-PROGRAM SECTION.
      ****************************************************
       END-RUN.
           PERFORM CLOSE-FILES.
           PERFORM PRU-OUT.
           IF NO-RECORDS-FOUND
              MOVE " NO RECORDS FOUND FOR SELECTED RANGE !" TO MESS
              PERFORM SEND-MESS.
       EXIT-PROG.
           MOVE "GB/SEMENU" TO FORM-NAM.
           MOVE FORM-PATH TO FORM-PATHNAME.
       STOP-RUN.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY SNLIST-SCREEN.
           DESTROY SNLIST-WINDOW-HANDLE.
           EXIT PROGRAM.
