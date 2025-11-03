       IDENTIFICATION DIVISION.
       PROGRAM-ID.      STLIST.
       DATE-WRITTEN. 1/27/84
      *****************************************************
      *                      (LP)
      *    DESCRIPTION:  STATE FILE LISTING
      * REV:
      * BLM 150314 ADD SECURITY, MUST HAVE BEEN CONVERTED
      *            BEFORE WE SET THAT UP???
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
           "@(#)LS/STLIST.CBL S35 12/12/23-10:51:48 >".
       COPY "LIBLP/LP01ST.CPY".
       COPY "LIBLP/LP01ST_SQL.CPY".
       COPY "LIBLP/LPWSST.CPY".

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

       01  OK                   PIC X        VALUE " ".
       01  ERRCD                PIC X        VALUE " ".

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
           03  FILLER           PIC X(10)    VALUE "USER ID:".
           03  H-USERID         PIC X(10).
           03  FILLER           PIC X(6)     VALUE " ".
           03  FILLER           PIC X(31)    VALUE
           " - - - STATE FILE LISTING - - -".

       01  HEAD3.
           03  FILLER           PIC X(30)    VALUE " ".
           03  FILLER           PIC X(4)     VALUE "CODE".
           03  FILLER           PIC X(8)     VALUE " ".
           03  FILLER           PIC X(11)    VALUE "DESCRIPTION".

       01  DETAIL-LINE          PIC X(80) VALUE SPACES.

       01  D-LINE REDEFINES DETAIL-LINE.
           03  FILLER           PIC X(31).
           03  D-CODE           PIC XX.
           03  FILLER           PIC X(9).
           03  D-DESC           PIC X(16).

       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBGB/GETPRW.CPY".

       01  STLIST-WINDOW-HANDLE PIC X(10).
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBLS/STLIST_DEF.CPY".
       COPY "LIBLS/STLIST_WKS.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

      ****************************************
      *     P R O G R A M   C O N T R O L    *
      ****************************************
       COPY "LIBGB/GETFMW.CPY".

       SCREEN SECTION.
       COPY "LIBLS/STLIST_SCN.CPY".

       PROCEDURE DIVISION
             USING FORM-PATHNAME EXIT-PATHNAME.
       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON PR-FILE.
       COPY "LIBGB/DECLARE.CPY".
           PERFORM CLOSE-ST1-FILE.
           PERFORM CLOSE-PR-FILE.
       COPY "LIBGB/DECLAR2.CPY".

      *****************************
       MAIN-MODULE SECTION.
      *****************************
           PERFORM SQL-CONNECT.

       COPY "LIBGB/CHKSEC.CPY".
      D    STOP MESS.

           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "STLIST" TO VDU-FORM-NAME.

           DISPLAY FLOATING GRAPHICAL WINDOW
                   SCREEN LINE 01 * VDU-WIN-L-OFFSET
                   SCREEN COL 01 * VDU-WIN-C-OFFSET
                   CELL SIZE IS LABEL FONT
                   SIZE 80 + VDU-WIN-W-MOD
                   LINES 24 + VDU-WIN-H-MOD
                   BLANK SCREEN
                   HANDLE IS STLIST-WINDOW-HANDLE.

           PERFORM FORM-RESET.

           IF EXT-ROUTE-BUF NOT = "00"                                          
              MOVE "PROGRAM NOT BATCHABLE" TO MESS                              
              PERFORM SEND-MESS        
              MOVE "99" TO EXT-ROUTE-BUF                                        
              GO TO END-PROGRAM.                                                

           PERFORM INITIALIZATION.
           IF ERRCD NOT = " "
              GO TO END-PROGRAM.

       NEXT-ST.
           PERFORM READ-ST1-FILE-NEXT.
           IF IO-FG = 9
              GO TO END-ROUTINE.

           PERFORM DETAIL-LINE-PROCESS.

           GO TO NEXT-ST.
       END-ROUTINE.
           IF FIRST-TIME = "Y"
              MOVE " NO RECORDS FOUND " TO MESS
              PERFORM SEND-MESS.
           GO TO END-PROGRAM.

      ***********************************
       INITIALIZATION SECTION.
      ***********************************
           MOVE " " TO ERRCD.
       GET-PR.
           MOVE STLIST-QUEUE-POS TO Q-POS.
           MOVE STLIST-COPIES-POS TO C-POS.
           MOVE STLIST-PRU-POS TO P-POS.
           MOVE SPACES TO Q-BUF C-BUF P-BUF.
           PERFORM PRU-IN.
           IF STAT NOT = "00" GO TO END-INIT.
       OK-YN.
           MOVE "E  A010ACCEPT." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO GET-PR.
           IF VDUSTATUS = "1>" GO TO END-INIT.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO OK-YN.
           MOVE VDUBUF TO OK.
           IF OK = "N" GO TO END-INIT.
           IF OK NOT = "Y" GO TO OK-YN.

           MOVE "LISTING IN PROGRESS...................." TO MESS.
           MOVE MESS TO WR-BUF.
           MOVE MESS-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

           MOVE " " TO ERRCD.
           MOVE EXT-CONAME-CONAME TO H-NAME.
           MOVE EXT-CONAME-SYSDATE TO H-DATE.
           MOVE EXT-CONAME-USER-ID TO H-USERID.

           PERFORM OPEN-ST1-FILE.
           MOVE SPACES TO ST-CODE.
           PERFORM START-ST1-FILE.
           PERFORM OPEN-PR-FILE.

           GO TO EXIT-INITIALIZE.
       END-INIT.
           MOVE "X" TO ERRCD.
       EXIT-INITIALIZE.
           EXIT.

      ****************************
       DETAIL-LINE-PROCESS SECTION.
      ****************************
           MOVE ST-CODE TO D-CODE.
           MOVE ST-DESC TO D-DESC.
           PERFORM WRITE-DETAIL-LINE.
           MOVE 1 TO LN-FEED.

      ****************************
       WRITE-DETAIL-LINE SECTION.
      ****************************
           ADD LN-FEED LN-COUNT GIVING LN-WK.
           IF LN-WK > EXT-CONAME-RPT-LINES
              PERFORM HEAD-PAGE.
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

       COPY "LIBGB/TIMEIO.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/DATER.CPY".


      ****************************************************
       FORM-RESET SECTION.
      ****************************************************
           MOVE EXT-CONAME-CONAME TO STLIST-CONAME.
           MOVE EXT-CONAME-SYSDATE TO STLIST-SYSDATE.
           DISPLAY STLIST-SCREEN UPON STLIST-WINDOW-HANDLE.
           MOVE STLIST-FORM-FIELDS TO WR-BUF.
           MOVE STLIST-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      ****************************************************
       SETUP-LINK-INFO SECTION.
      ****************************************************
           MOVE STLIST-HELP--FILE TO HELP-LINK-FILE.
           MOVE STLIST-HELP--DIR  TO HELP-LINK-DIR.

      ****************************************************
       VDU-CONVERT-FIELD SECTION.
      ****************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBLS/STLIST_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ***************************
       MISC SECTION.
      ***************************
       COPY "LIBGB/GETPR.CPY".
       COPY "LIBGB/ACCESS.CPY".

      ***************************
       IO-ROUTINE SECTION.
      ***************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBLP/LPSTGS_SQL.CPY".
       COPY "LIBGB/GBPRINT.CPY".
       COPY "LIBLP/LPST1RN.CPY".
       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO ERRCD.

      ***************************
       END-PROGRAM SECTION.
      ***************************
       END-RUN.
           PERFORM CLOSE-FILES.
           PERFORM PRU-OUT.
       EXIT-PROG.
           MOVE EXIT-PATH TO FORM-NAM.
           MOVE FORM-PATH TO FORM-PATHNAME.
       STOP-RUN.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY STLIST-SCREEN.
           DESTROY STLIST-WINDOW-HANDLE.
           EXIT PROGRAM.
