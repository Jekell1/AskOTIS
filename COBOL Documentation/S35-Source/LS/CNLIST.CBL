       IDENTIFICATION DIVISION.
       PROGRAM-ID.      CNLIST.
       DATE-WRITTEN. 10-09-01
      ******************************************************************
      *
      *                      (LP)
      *    DESCRIPTION:    COLLECTOR ID LISTING
      *
      * REV.
      *   BAH 110919 REPLACED RRFORM WITH SCREEN
      *   BAH 190131 SPLIT CDFILE
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

      ******************************************************************
      *
      *    W O R K I N G - S T O R A G E   S E C T I O N
      *
      ******************************************************************
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)LS/CNLIST.CBL S35 02/23/24-10:16:19 >".
       COPY "LIBLP/LP01CDB.CPY".
       COPY "LIBLP/LP01CDB_SQL.CPY".
       COPY "LIBLP/LPWSCDB.CPY".

       01  LN-WORKERS.
           03  LN-FEED          PIC 99       VALUE 0.
           03  LN-COUNT         PIC 999      VALUE 99.
           03  LN-WK            PIC 999      VALUE 0.
       01  PAGE-NUMBER          PIC 9(4)     VALUE 0.

      ******************************************************************
      *
      *   MISC WORKERS
      *
      ******************************************************************
       01  FIRST-TIME           PIC X        VALUE "Y".
       01  MESS-LIST            PIC X(5)     VALUE "MESS.".

      ******************************************************************
      *
      *     S C R E E N   F I E L D S
      *
      ******************************************************************
       01  SCREEN-BUF          VALUE " ".
           03  ERRCD           PIC X.
           03  BEG-BR          PIC 9(4).
           03  END-BR          PIC 9(4).
           03  COLLECTOR1      PIC X(3).
           03  COLLECTOR2      PIC X(3).

       01  ACCEPT-BUF             PIC X          VALUE " ".

      ******************************************************************
      *
      *     P R I N T   B U F F E R S
      *
      ******************************************************************
       01  HEAD1.
           03  H-DATE           PIC X(8)     VALUE " ".
           03  FILLER           PIC XX       VALUE " ".
           03  H-HR             PIC 99       VALUE 0.
           03  FILLER           PIC X        VALUE ":".
           03  H-MIN            PIC 99       VALUE 0.
           03  FILLER           PIC X(10)    VALUE " ".
           03  H-NAME           PIC X(30)    VALUE " ".
           03  FILLER           PIC X(10)    VALUE " ".
           03  FILLER           PIC X(5)     VALUE "PAGE ".
           03  H-PAGE-NO        PIC ZZZ9.

       
       01  HEAD2.
           03  FILLER           PIC X(10)    VALUE "USER ID:".
           03  H-USERID         PIC X(10).
           03  FILLER           PIC X(5)    VALUE " ".
           03  FILLER           PIC X(36)    VALUE
           " - - COLLECTOR ID FILE LISTING  - - ".
       
       01  HEAD3.
           03  FILLER           PIC X(80)   VALUE ALL "=".

       
       01  HEAD4.
           03  FILLER      PIC X(7)    VALUE "BRNO   ".
           03  FILLER      PIC X(9)    VALUE "COLCTID  ".
           03  FILLER      PIC X(11)    VALUE "DESCRIPTION".

      ******************************************************************
      *
      *    D E T A I L - L I N E
      *
      ******************************************************************
       01  DETAIL-LINE          PIC X(80) VALUE SPACES.

       
       01  D-LINE1 REDEFINES DETAIL-LINE.
           03  D-BRANCH    PIC 9(4).
           03  FILLER      PIC X(4).
           03  D-CODE      PIC X(3).
           03  FILLER      PIC X(6).
           03  D-DESC      PIC X(35).
           03  FILLER      PIC X(4).
           03  D-PHONE     PIC 999/999/9999 BLANK ZERO.
           03  FILLER      PIC XX.
           03  D-EXT       PIC Z(5).

       
       01  RANGE-LINE REDEFINES DETAIL-LINE.
           03  FILLER           PIC X(22).
           03  RANGE-SELECTION  PIC X(16).

      ******************************************************************
      *
      *     C O P Y - L I B R A R Y - W O R K E R S
      *
      ******************************************************************
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBGB/GETPRW.CPY".
       01  CNLIST-WINDOW-HANDLE PIC X(10).
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBLS/CNLIST_DEF.CPY".
       COPY "LIBLS/CNLIST_WKS.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

      ****************************************
      *     P R O G R A M   C O N T R O L    *
      ****************************************
       COPY "LIBGB/GETFMW.CPY".

       SCREEN SECTION.
       COPY "LIBLS/CNLIST_SCN.CPY".

       PROCEDURE DIVISION
             USING FORM-PATHNAME EXIT-PATHNAME.

       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
               PR-FILE.
       COPY "LIBGB/DECLARE.CPY".
           PERFORM CLOSE-CDB1-FILE.
           PERFORM CLOSE-PR-FILE.
       COPY "LIBGB/DECLAR2.CPY".

      ******************************************************************
       MAIN-MODULE SECTION.
      ******************************************************************
           PERFORM SQL-CONNECT.

       COPY "LIBGB/CHKSEC.CPY".

           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "CNLIST" TO VDU-FORM-NAME.

           DISPLAY FLOATING GRAPHICAL WINDOW
                   SCREEN LINE 01 * VDU-WIN-L-OFFSET
                   SCREEN COL 01 * VDU-WIN-C-OFFSET
                   CELL SIZE IS LABEL FONT
                   SIZE 80 + VDU-WIN-W-MOD
                   LINES 24 + VDU-WIN-H-MOD
                   BLANK SCREEN
                   HANDLE IS CNLIST-WINDOW-HANDLE.

           PERFORM FORM-RESET.

           PERFORM INITIALIZATION.

           IF ERRCD NOT = " "
              GO TO END-PROGRAM.

       NEXT-CODE-CN.
           PERFORM READ-CDB1-FILE-NEXT.
           IF IO-FG = 9
              GO TO END-ROUTINE.

           IF CDB-TYPE NOT = "CN"
              GO TO END-ROUTINE.

           IF CDB-BRANCH  < BEG-BR OR CDB-BRANCH > END-BR
              GO TO NEXT-CODE-CN.

           IF CDB-CODE  < COLLECTOR1  OR CDB-CODE > COLLECTOR2
              GO TO NEXT-CODE-CN.

           PERFORM DETAIL-LINE-PROCESS.

           GO TO NEXT-CODE-CN.

       END-ROUTINE.
           IF FIRST-TIME = "Y"
              MOVE " NO RECORDS FOUND FOR SELECTED RANGE !" TO MESS
              PERFORM SEND-MESS
              MOVE SPACES TO PR-REC
              MOVE 99 TO LN-FEED
              PERFORM WRITE-PR-REC.

           PERFORM PRINT-RANGE-LINES.
           GO TO END-PROGRAM.

      ******************************************************************
       INITIALIZATION SECTION.
      ******************************************************************
           MOVE " " TO ERRCD.

       SETRP.
           MOVE CNLIST-QUEUE-POS TO Q-POS.
           MOVE CNLIST-COPIES-POS TO C-POS.
           MOVE CNLIST-PRU-POS TO P-POS.
           MOVE SPACES TO Q-BUF C-BUF P-BUF.
           PERFORM PRU-IN.

           IF STAT NOT = "00"
              GO TO ENDIT.
       
       BR-01.
           MOVE "ENNN040BR1." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO SETRP.
           IF VDUSTATUS = "16" GO TO ALL-BR.
           IF VDUSTATUS = "1>" GO TO ENDIT.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO BR-01.
           MOVE VDUNUM0 TO BEG-BR.

       
       BR-02.
           MOVE "ENNN040BR2." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO SETRP.
           IF VDUSTATUS = "1U" GO TO BR-01.
           IF VDUSTATUS = "1>" GO TO ENDIT.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO BR-02.
           MOVE VDUNUM0 TO END-BR.

           IF BEG-BR > END-BR
              MOVE "INVALID BRANCH RANGE" TO MESS
              PERFORM SEND-MESS
              GO TO BR-01.

           GO TO COL1.

       
       ALL-BR.
           MOVE "SNNN040BR1." TO VDU.
           MOVE 0 TO VDUNUM0 BEG-BR.
           PERFORM SCREENIO.
           MOVE "SNNN040BR2." TO VDU.
           MOVE 9999 TO VDUNUM0 END-BR.
           PERFORM SCREENIO.

       
       COL1.
           MOVE "E  A030COL1." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO SETRP.
           IF VDUSTATUS = "1U" GO TO BR-02.
           IF VDUSTATUS = "16" GO TO ALL-COLL.
           IF VDUSTATUS = "1>" GO TO ENDIT.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO COL1.
           MOVE VDUBUF TO COLLECTOR1.

       
       COL2.
           MOVE "E  A030COL2." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO SETRP.
           IF VDUSTATUS = "1U" GO TO COL1.
           IF VDUSTATUS = "1>" GO TO ENDIT.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO COL2.
           MOVE VDUBUF TO COLLECTOR2.

           IF COLLECTOR1> COLLECTOR2
              MOVE "INVALID RANGE " TO MESS
              PERFORM SEND-MESS
              GO TO COL1.

           GO TO OK-YN.

       
       ALL-COLL.
           MOVE "S  A030COL1." TO VDU.
           MOVE SPACES TO VDUBUF COLLECTOR1.
           PERFORM SCREENIO.
           MOVE "S  A030COL2." TO VDU.
           MOVE "ZZZ"  TO VDUBUF COLLECTOR2.
           PERFORM SCREENIO.

       OK-YN.
           MOVE "E  A010ACCEPT." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO SETRP.
           IF VDUSTATUS = "1U" GO TO COL2.
           IF VDUSTATUS = "1>" GO TO ENDIT.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO OK-YN.
           MOVE VDUBUF TO ACCEPT-BUF.
           IF ACCEPT-BUF = "N" GO TO BR-01.
           IF ACCEPT-BUF NOT = "Y" GO TO OK-YN.

           PERFORM OPEN-CDB1-FILE.
           MOVE "CN"       TO CDB-TYPE
                              QCDB1-WBEG-TYPE
                              QCDB1-WEND-TYPE.
           MOVE BEG-BR     TO CDB-BRANCH
                              QCDB1-WBEG-BRANCH
           MOVE END-BR     TO QCDB1-WEND-BRANCH.
           MOVE COLLECTOR1 TO CDB-CODE
                              QCDB1-WBEG-CODE.
           MOVE COLLECTOR2 TO QCDB1-WEND-CODE.

           PERFORM START-CDB1-FILE.
           IF IO-BAD
              GO TO ENDIT.

           MOVE EXT-CONAME-CONAME TO H-NAME.
           MOVE EXT-CONAME-SYSDATE TO H-DATE.
           MOVE EXT-CONAME-USER-ID TO H-USERID.

           MOVE "LISTING IN PROGRESS...................." TO WR-BUF.
           MOVE MESS-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

           PERFORM OPEN-PR-FILE.

           GO TO EXIT-INITIALIZE.

       ENDIT.
           MOVE 1 TO ERRCD.

       EXIT-INITIALIZE.
           EXIT.

      ******************************************************************
       DETAIL-LINE-PROCESS SECTION.
      ******************************************************************
           ADD 2 LN-COUNT GIVING LN-WK.

           IF LN-WK > EXT-CONAME-RPT-LINES
              PERFORM HEAD-PAGE.

           MOVE CDB-BRANCH TO D-BRANCH.
           MOVE CDB-CODE   TO D-CODE.
           MOVE CDB-DESC   TO D-DESC.
           MOVE CDB-CN-PHONE  TO D-PHONE.
           IF D-PHONE NOT = 0
              INSPECT D-PHONE REPLACING ALL "/" BY "-".
           MOVE CDB-CN-PHONE-EXT TO D-EXT.
           PERFORM WRITE-DETAIL-LINE.

       END-DETAIL-LN-PROCESS.
           EXIT.

      ******************************************************************
       WRITE-DETAIL-LINE SECTION.
      ******************************************************************
           MOVE DETAIL-LINE TO PR-REC.
           PERFORM WRITE-PR-REC.
           ADD LN-FEED TO LN-COUNT.
           MOVE 1 TO LN-FEED.
           MOVE SPACES TO DETAIL-LINE.

      ******************************************************************
       HEAD-PAGE SECTION.
      ******************************************************************
       HEAD-ROUTINE.
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
       
           MOVE HEAD4 TO PR-REC.
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-PR-REC.

       END-HEAD-PAGE.
           MOVE 5 TO LN-COUNT.
           MOVE 2 TO LN-FEED.

      ******************************************************************
       PRINT-RANGE-LINES SECTION.
      ******************************************************************
           MOVE 2 TO LN-FEED.
           MOVE "BEGINNING BRANCH" TO RANGE-LINE.
           MOVE BEG-BR             TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
       
           MOVE "ENDING BRANCH   " TO RANGE-LINE.
           MOVE END-BR             TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
       
           MOVE "BEGIN COLLECTOR ID" TO RANGE-LINE.
           MOVE COLLECTOR1         TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
       
           MOVE "END   COLLECTOR ID" TO RANGE-LINE.
           MOVE COLLECTOR2         TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
       

      ******************************************************************
      *
      *     C O P Y - L I B R A R Y - S E C T I O N S
      *
      ******************************************************************
       COPY "LIBGB/TIMEIO.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/DATER.CPY".

      ****************************************************
       FORM-RESET SECTION.
      ****************************************************
           MOVE EXT-CONAME-CONAME TO CNLIST-CONAME.
           MOVE EXT-CONAME-SYSDATE TO CNLIST-SYSDATE.
           DISPLAY CNLIST-SCREEN UPON CNLIST-WINDOW-HANDLE.
           MOVE CNLIST-FORM-FIELDS TO WR-BUF.
           MOVE CNLIST-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      ****************************************************
       SETUP-LINK-INFO SECTION.
      ****************************************************
           MOVE CNLIST-HELP--FILE TO HELP-LINK-FILE.
           MOVE CNLIST-HELP--DIR  TO HELP-LINK-DIR.

      ****************************************************
       VDU-CONVERT-FIELD SECTION.
      ****************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBLS/CNLIST_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ******************************************************************
       MISC SECTION.
      ******************************************************************
       COPY "LIBGB/GETPR.CPY".
       COPY "LIBGB/ACCESS.CPY".

      ******************************************************************
       IO-ROUTINE SECTION.
      ******************************************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBLP/LPCDBGS_SQL.CPY".
       COPY "LIBGB/GBPRINT.CPY".
       COPY "LIBLP/LPCDB1RN.CPY".
       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO ERRCD.

      ******************************************************************
       END-PROGRAM SECTION.
      ******************************************************************
       END-RUN.
           PERFORM CLOSE-FILES.
           PERFORM PRU-OUT.

       EXIT-PROG.
           MOVE "LP/LPLSMU" TO FORM-NAM.
           MOVE FORM-PATH TO FORM-PATHNAME.

       STOP-RUN.
           DESTROY CNLIST-WINDOW-HANDLE.
           EXIT PROGRAM.
