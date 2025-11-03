       IDENTIFICATION DIVISION.
       PROGRAM-ID.      BMLIST.
       DATE-WRITTEN. 01-30-84
      *****************************************************
      *                      (LP)
      *    DESCRIPTION:  BORROWER MEMO FILE LIST
      *
      * CURRENT BRANCH ONLY, DOES NOT CROSS DATA PATHS
      *
      *  REV:
      *   JFF 101995 ADDED BORROWER SCAN
      *   GLF 110998 MADE MEMO DATE 2000 COMPATIBLE
      *   BAH 000107 END-DATE RANGE TEST WAS USING BM-DATE INSTEAD
      *              OF BMDATE, TFC
      *   BAH 110919 REPLACED RRFORM WITH SCREEN
      *   BAH 170425 ACTIVATE 8 DIGIT DATES, PD0006
      *   BAH 20220803 HARD CODED START
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

      *****************************
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)LS/BMLIST.CBL J35 12/21/23-09:19:47 >".
       COPY "LIBLP/LP01BM.CPY".
       COPY "LIBLP/LP01BM_SQL.CPY".
       COPY "LIBLP/LPWSBM.CPY".
       COPY "LIBLP/LP01BW.CPY".
       COPY "LIBLP/LP01BW_SQL.CPY".
       COPY "LIBLP/LPWSBW.CPY".

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
       01  BEG-SS          PIC 9(9).
       01  END-SS          PIC 9(9).

       01  NAME-WORKER     PIC X(34).
       01  NAME            PIC X(16).
       01  HOLD-SSNO       PIC 9(9)    VALUE ZERO.
       01  HOLD-DATE       PIC 9(8)    VALUE ZERO.
      * BM-DATE SUBTRACTED FROM 9'S
       01  TEST-BM-DATE    PIC 9(8)    VALUE ZERO.
       01  BORROWER-FG     PIC X(3)    VALUE "   ".
           88  BORROWER-BEG            VALUE "BEG".
           88  BORROWER-END            VALUE "END".

       01  NUM-BW          PIC 9(9).
       01  NUM-BWX REDEFINES NUM-BW.
           03  NUM-BW123   PIC X(3).
           03  NUM-BW45    PIC X(2).
           03  NUM-BW6789  PIC X(4).
       01  ALP-BW.
           03  ALP-BW123   PIC X(3).
           03  ALP-HYPHEN1 PIC X.
           03  ALP-BW45    PIC X(2).
           03  ALP-HYPHEN2 PIC X.
           03  ALP-BW6789  PIC X(4).

      ***********************************
      *     S C R E E N   F I E L D S   *
      ***********************************
       01  SCREEN-BUF          VALUE " ".
           03  ERRCD           PIC X.
           03  W-BEG-SS        PIC X(11).
           03  W-END-SS        PIC X(11).
           03  BEG-DATE        PIC 9(8).
           03  END-DATE        PIC 9(8).


       01  ACCEPT-BUF      PIC X    VALUE " ".

      ***********************************
      *     P R I N T   B U F F E R S   *
      ***********************************
       01  HEAD1.
           03  H-DATE           PIC X(8)     VALUE " ".
           03  FILLER           PIC XX       VALUE " ".
           03  H-HR             PIC 99       VALUE 0.
           03  FILLER           PIC X        VALUE ":".
           03  H-MIN            PIC 99       VALUE 0.
           03  FILLER           PIC X(31)    VALUE " ".
           03  H-NAME           PIC X(40)    VALUE " ".
           03  FILLER           PIC X(37)    VALUE " ".
           03  FILLER           PIC X(5)     VALUE "PAGE ".
           03  H-PAGE-NO        PIC ZZZ9.

       01  HEAD2.
           03  FILLER           PIC X(10)    VALUE "USER ID:".
           03  H-USERID         PIC X(10).
           03  FILLER           PIC X(26)    VALUE " ".
           03  FILLER           PIC X(41)    VALUE
           " - - - BORROWER MEMO FILE LISTING - - -".
           03  FILLER           PIC X(45)    VALUE " ".

       01  HEAD3.
           03  FILLER      PIC X(11)    VALUE "BORROWER NO".
           03  FILLER      PIC X(8)     VALUE " ".
           03  FILLER      PIC XXXX     VALUE "NAME".
           03  FILLER      PIC X(30)    VALUE " ".
           03  FILLER      PIC XXXX     VALUE "DATE".
           03  FILLER      PIC X(6)     VALUE " ".
           03  FILLER      PIC XXX      VALUE "SEQ".
           03  FILLER      PIC X(8)     VALUE " ".
           03  FILLER      PIC XXXX     VALUE "MEMO".

       01  DETAIL-LINE          PIC X(132) VALUE SPACES.

       01  D-LINE REDEFINES DETAIL-LINE.
           03  D-SSNO      PIC 999/99/9999.
           03  FILLER      PIC XXX.
           03  D-NAME      PIC X(34).
           03  FILLER      PIC XXX.
           03  D-DATE      PIC 99/99/99.
           03  FILLER      PIC X(5).
           03  D-SEQ       PIC Z9.
           03  FILLER      PIC XXX.
           03  D-MEMO      PIC X(60).

       01  RANGE-LINE REDEFINES DETAIL-LINE.
           03  FILLER           PIC X(20).
           03  RANGE-SELECTION  PIC X(16).

       COPY "LIBWI/BWSCANW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/GETPRW.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       01  BMLIST-WINDOW-HANDLE PIC X(10).
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBLS/BMLIST_DEF.CPY".
       COPY "LIBLS/BMLIST_WKS.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

      ****************************************
      *     P R O G R A M   C O N T R O L    *
      ****************************************
       COPY "LIBGB/GETFMW.CPY".

       SCREEN SECTION.
       COPY "LIBLS/BMLIST_SCN.CPY".

       PROCEDURE DIVISION
             USING FORM-PATHNAME EXIT-PATHNAME.

       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON PR-FILE.
       COPY "LIBGB/DECLARE.CPY".
           PERFORM CLOSE-BW1-FILE.
           PERFORM CLOSE-BM1-FILE.
           PERFORM CLOSE-PR-FILE.
       COPY "LIBGB/DECLAR2.CPY".

      *************************************
       MAIN-MODULE SECTION.
      *************************************
           PERFORM SQL-CONNECT.

       COPY "LIBGB/CHKSEC.CPY".

           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "BMLIST" TO VDU-FORM-NAME.

           DISPLAY FLOATING GRAPHICAL WINDOW
                   SCREEN LINE 01 * VDU-WIN-L-OFFSET
                   SCREEN COL 01 * VDU-WIN-C-OFFSET
                   CELL SIZE IS LABEL FONT
                   SIZE 80 + VDU-WIN-W-MOD
                   LINES 24 + VDU-WIN-H-MOD
                   BLANK SCREEN
                   HANDLE IS BMLIST-WINDOW-HANDLE.

           PERFORM FORM-RESET.

           PERFORM INITIALIZATION.
           IF ERRCD NOT = " "
              GO TO END-PROGRAM.

       NEXT-BM1.
           PERFORM READ-BM1-FILE-NEXT.
           IF IO-FG = 9 OR (BM-PATH-OWNBR NOT = BM-BRNO)
              GO TO END-ROUTINE.

           IF BM-SSNO > END-SS
              GO TO END-ROUTINE.

           COMPUTE DATE-YYYYMMDD = 99999999 - BM-DATE.
           MOVE DATE-YYYYMMDD TO TEST-BM-DATE.

           IF TEST-BM-DATE < BEG-DATE
              GO TO NEXT-BM1.

           IF TEST-BM-DATE > END-DATE
              GO TO NEXT-BM1.

           PERFORM DETAIL-LINE-PROCESS.

           GO TO NEXT-BM1.
       END-ROUTINE.
           IF FIRST-TIME = "Y"
              MOVE " NO RECORDS FOUND FOR SELECTED RANGE !" TO MESS
              PERFORM SEND-MESS
              MOVE "X" TO FIRST-TIME
              MOVE SPACES TO PR-REC
              MOVE 99 TO LN-FEED
              PERFORM WRITE-PR-REC.

           PERFORM PRINT-RANGE-LINES.

           GO TO END-PROGRAM.

      ***********************************
       INITIALIZATION SECTION.
      ***********************************
       SETRP.
           MOVE " " TO ERRCD.
           MOVE BMLIST-QUEUE-POS TO Q-POS.
           MOVE BMLIST-COPIES-POS TO C-POS.
           MOVE BMLIST-PRU-POS TO P-POS.
           MOVE SPACES TO Q-BUF C-BUF P-BUF.
           PERFORM PRU-IN.
           IF STAT NOT = "00"
              GO TO ENDIT.
       SS-01.
           MOVE "ERNX110SS1." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO SETRP.
           IF VDUSTATUS = "16" GO TO ALL-SS.
           IF VDUSTATUS = "1>" GO TO ENDIT.
           IF VDUSTATUS = "1<"
              MOVE "BEG" TO BORROWER-FG
              PERFORM BORROWER-SCAN
              IF NOT IO-GOOD
                 GO TO SS-01.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO SS-01.
           MOVE VDUNUM0 TO BEG-SS NUM-BW.
           PERFORM NUMBW-TO-ALPBW.
           MOVE ALP-BW TO W-BEG-SS.

       SS-02.
           MOVE "ERNX110SS2." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO SETRP.
           IF VDUSTATUS = "1U" GO TO SS-01.
           IF VDUSTATUS = "1>" GO TO ENDIT.
           IF VDUSTATUS = "1<"
              MOVE "END" TO BORROWER-FG
              PERFORM BORROWER-SCAN
              IF IO-FG NOT = 0
                 GO TO SS-02.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO SS-02.
           MOVE VDUNUM0 TO END-SS NUM-BW.
           PERFORM NUMBW-TO-ALPBW.
           MOVE ALP-BW TO W-END-SS.

           IF BEG-SS > END-SS
              MOVE " INVALID BORROWER NUMBER RANGE !" TO MESS
              PERFORM SEND-MESS
              GO TO SS-01.

           GO TO DT-01.
       ALL-SS.
           MOVE "S  A110SS1." TO VDU.
           MOVE "000-00-0000" TO W-BEG-SS VDUBUF.
           PERFORM SCREENIO.
           MOVE "S  A110SS2." TO VDU.
           MOVE "999-99-9999" TO W-END-SS VDUBUF.
           PERFORM SCREENIO.
           MOVE 0 TO BEG-SS.
           MOVE 999999999 TO END-SS.

       DT-01.
           MOVE "ERNM060DATE1." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO SETRP.
           IF VDUSTATUS = "16" GO TO ALL-DT.
           IF VDUSTATUS = "1U" GO TO SS-02.
           IF VDUSTATUS = "1>" GO TO ENDIT.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO DT-01.
           MOVE VDU-DATE-YYYYMMDD TO BEG-DATE.

       DT-02.
           MOVE "ERNM060DATE2." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO SETRP.
           IF VDUSTATUS = "1U" GO TO DT-01.
           IF VDUSTATUS = "1>" GO TO ENDIT.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO DT-02.
           MOVE VDU-DATE-YYYYMMDD TO END-DATE.

           IF END-DATE < BEG-DATE
              MOVE "INVALID DATE RANGE !" TO MESS
              PERFORM SEND-MESS
              GO TO DT-01.

           GO TO OK-YN.
       ALL-DT.
           MOVE "S  8080DATE1." TO VDU.
           MOVE EXT-JULIAN-BEG TO BEG-DATE VDU-DATE-YYYYMMDD
           PERFORM SCREENIO.
           MOVE "S  8080DATE2."   TO VDU.
           MOVE EXT-JULIAN-END TO END-DATE VDU-DATE-YYYYMMDD
           PERFORM SCREENIO.

       OK-YN.
           MOVE " " TO ERRCD.

           MOVE "E  A010ACCEPT." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U" GO TO DT-02.
           IF VDUSTATUS = "1>" GO TO ENDIT.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO OK-YN.
           MOVE VDUBUF TO ACCEPT-BUF.
           IF ACCEPT-BUF = "N" GO TO SS-01.
           IF ACCEPT-BUF NOT = "Y" GO TO OK-YN.



           MOVE EXT-CONAME-CONAME TO H-NAME.
           MOVE EXT-CONAME-SYSDATE TO H-DATE.
           MOVE EXT-CONAME-USER-ID TO H-USERID.

           MOVE BEG-SS TO BM-SSNO.

           PERFORM OPEN-BW1-FILE.
           PERFORM OPEN-BM1-FILE.

           MOVE BM-PATH-OWNBR TO BM-BRNO
                                 QBM1-WBEG-BRNO
                                 QBM1-WEND-BRNO.
           MOVE BEG-SS        TO BM-SSNO
                                 QBM1-WBEG-SSNO.
           MOVE END-SS        TO QBM1-WEND-SSNO.
           IF BEG-DATE = EXT-JULIAN-BEG
             MOVE 0           TO BM-DATE
           ELSE
              COMPUTE BM-DATE = 99999999 - BEG-DATE.
           MOVE BM-DATE       TO QBM1-WBEG-DATE-REV.
           MOVE 0             TO BM-SEQ
                                 QBM1-WBEG-SEQ.
           MOVE ALL "9"       TO QBM1-WEND-DATE-REV
                                 QBM1-WEND-SEQ.

           PERFORM START-BM1-FILE.

           MOVE "LISTING IN PROGRESS...................." TO WR-BUF.
           MOVE MESS-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

           PERFORM OPEN-PR-FILE.

           GO TO EXIT-INITIALIZE.
       ENDIT.
           MOVE "X" TO ERRCD.
       EXIT-INITIALIZE.
           EXIT.

      ****************************
       DETAIL-LINE-PROCESS SECTION.
      ****************************
           IF BM-SSNO NOT = HOLD-SSNO
              PERFORM FIND-NAME.
           IF TEST-BM-DATE NOT = HOLD-DATE
               MOVE TEST-BM-DATE TO HOLD-DATE DATE-YYYYMMDD
               PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
               MOVE DATE-MMDDYY TO D-DATE.
           MOVE BM-SEQ  TO D-SEQ.
           MOVE BM-MEMO TO D-MEMO.
           PERFORM WRITE-DETAIL-LINE.
           MOVE 1 TO LN-FEED.

      ****************************
       FIND-NAME SECTION.
      ****************************
           MOVE BM-SSNO TO D-SSNO, HOLD-SSNO, BW-SSNO.
           INSPECT D-SSNO REPLACING ALL "/" BY "-".
           PERFORM READ-BW1-FILE.
           IF IO-FG = 9
               MOVE "*** NO BORROWER MASTER RECORD ***" TO D-NAME
               GO TO END-FIND-NAME.
           MOVE SPACES TO NAME-WORKER.
           MOVE BW-LNAME TO NAME-WORKER.
           INSPECT NAME-WORKER REPLACING FIRST "  " BY ",?".
           MOVE BW-FNAME TO NAME.
           INSPECT NAME-WORKER REPLACING FIRST "                "
               BY NAME.
           INSPECT NAME-WORKER REPLACING ALL "?" BY " ".
           MOVE NAME-WORKER TO D-NAME.
           MOVE TEST-BM-DATE TO HOLD-DATE DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO D-DATE.
           MOVE 2 TO LN-FEED.
       END-FIND-NAME.
           EXIT.

      ****************************
       WRITE-DETAIL-LINE SECTION.
      ****************************
           ADD LN-FEED LN-COUNT GIVING LN-WK.
           IF LN-WK > EXT-CONAME-RPT-LINES AND FIRST-TIME NOT = "X"
              PERFORM HEAD-PAGE
              PERFORM FIND-NAME.
           MOVE DETAIL-LINE TO PR-REC.
           PERFORM WRITE-PR-REC.
           ADD LN-FEED TO LN-COUNT.
           MOVE 1 TO LN-FEED.
           MOVE SPACES TO DETAIL-LINE.

      *******************************
       HEAD-PAGE SECTION.
      *******************************
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

      *******************************
       PRINT-RANGE-LINES SECTION.
      *******************************
           MOVE 2 TO LN-FEED.
           MOVE "BEGINNING BORROWER" TO RANGE-LINE.
           MOVE W-BEG-SS TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
           MOVE "ENDING BORROWER" TO RANGE-LINE.
           MOVE W-END-SS TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "BEGINNING DATE" TO RANGE-LINE.
           MOVE BEG-DATE TO RANGE-SELECTION.
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.
           MOVE "ENDING DATE" TO RANGE-LINE.
           MOVE END-DATE TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.

      *************************
       BORROWER-SCAN SECTION.
      *************************
           MOVE 0   TO BWSCAN-NO.
           MOVE "S" TO BWSCAN-SCAN-TYPE.
           MOVE "WI/BWSCAN" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH BWSCAN-BUF EXIT-PATHNAME.
           CANCEL FORM-PROGX.
           IF BWSCAN-STATUS NOT = "00" OR BWSCAN-NO = 0
              MOVE 9 TO IO-FG
              GO TO EXIT-BORROWER-SCAN.

           MOVE BWSCAN-NO TO BW-SSNO.
           PERFORM NUMBW-TO-ALPBW.

           IF ( BORROWER-FG = "BEG" )
               MOVE "S  A110SS1." TO VDU
               MOVE ALP-BW TO W-BEG-SS VDUBUF
           ELSE
               MOVE "S  A110SS2." TO VDU
               MOVE ALP-BW TO W-END-SS VDUBUF.
           PERFORM SCREENIO.

           MOVE ALP-BW TO VDUBUF.
           MOVE 0 TO IO-FG.

       EXIT-BORROWER-SCAN.
           EXIT.

       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/TIMEIO.CPY".
       COPY "LIBGB/SCREEN.CPY".

      *********************************************************
       FORM-RESET SECTION.
      *********************************************************
           MOVE EXT-CONAME-CONAME TO BMLIST-CONAME.
           MOVE EXT-CONAME-SYSDATE TO BMLIST-SYSDATE.
           DISPLAY BMLIST-SCREEN UPON BMLIST-WINDOW-HANDLE.
           MOVE BMLIST-FORM-FIELDS TO WR-BUF.
           MOVE BMLIST-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      *********************************************************
       SETUP-LINK-INFO SECTION.
      *********************************************************
           MOVE BMLIST-HELP--FILE TO HELP-LINK-FILE.
           MOVE BMLIST-HELP--DIR  TO HELP-LINK-DIR.

      *********************************************************
       VDU-CONVERT-FIELD SECTION.
      *********************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBLS/BMLIST_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ****************************************
       MISC-PARAGRAPHS SECTION.
      ****************************************
       COPY "LIBGB/GETPR.CPY".
       COPY "LIBGB/ACCESS.CPY".
       NUMBW-TO-ALPBW.
           MOVE " " TO ALP-BW.
           MOVE NUM-BW123 TO ALP-BW123
           MOVE NUM-BW45 TO ALP-BW45
           MOVE NUM-BW6789 TO ALP-BW6789
           MOVE "-" TO ALP-HYPHEN1 ALP-HYPHEN2.

      **********************************
       IO-ROUTINE SECTION.
      **********************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBLP/LPBWGS_SQL.CPY".
       COPY "LIBLP/LPBMGS_SQL.CPY".
       COPY "LIBGB/GBPRINT.CPY".
       COPY "LIBLP/LPBM1RN.CPY".
       COPY "LIBLP/LPBW1RN.CPY".
       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO ERRCD.

      **********************************
       END-PROGRAM SECTION.
      **********************************
       END-RUN.
           PERFORM CLOSE-FILES.
           PERFORM PRU-OUT.
       EXIT-PROG.
           MOVE "LP/LPLSMU" TO FORM-NAM.
           MOVE FORM-PATH TO FORM-PATHNAME.
       STOP-RUN.
           DESTROY BMLIST-WINDOW-HANDLE.
           EXIT PROGRAM.
