       IDENTIFICATION DIVISION.
       PROGRAM-ID.      GLLLST.
       DATE-WRITTEN. 12/6/21.
      *****************************************************
      *                      (LS)
      *    DESCRIPTION:  GL LOG PROGRAM
      * REV:
      * AMM 120621 NEW GLLOG PROGRAM 
      * SMR 05/15/23 CHANGED GLGLLOGS1IN TO GLGLLOGS1RN 
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
       77  SCCS-IDENTIFIER PIC X(43) VALUE
           "@(#)./LS/GLLLST.CBL S35 05/15/23-08:52:28 >".
       COPY "LIBGL/GL01GLLOGS.CPY".
       COPY "LIBGL/GL01GLLOGS_SQL.CPY".
       COPY "LIBGL/GLWSGLLOGS.CPY".

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
      *     S C R E E N   F I E L D S   *
      ***********************************
       01  SCREEN-BUF          VALUE " ".
           03  BEG-GL1LOGS-KEY.
               05  BEG-ID              PIC X(6).
           03  END-GLLOGS1-KEY.
               05  END-ID              PIC X(6).

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
           " - - - - GL LOG LISTING - - - -".

       01  HEAD3.
           03  FILLER      PIC X(03) VALUE " ".
           03  FILLER      PIC X(25) VALUE "ID   SYSDATE  SEQNO  BEGD".
           03  FILLER      PIC X(25) VALUE "ATE   ENDDATE   BRNO   TI".
           03  FILLER      PIC X(26) VALUE "ME    RECS PURGED   USERID".

       01  DETAIL-LINE          PIC X(80) VALUE SPACES.

       01  D-LINE REDEFINES DETAIL-LINE.
           03  FILLER           PIC X(01).
           03  D-ID             PIC X(6).
           03  FILLER           PIC X(01).
           03  D-SYSTEM-DATE    PIC 9999/99/99.        
           03  FILLER           PIC X(01).
           03  D-SEQNO          PIC 9(2).
           03  FILLER           PIC X(01).
           03  D-BEG-DATE       PIC 9999/99/99.
           03  FILLER           PIC X(01).
           03  D-END-DATE       PIC 9999/99/99.
           03  FILLER           PIC X(01).
           03  D-BRNO-RANGE     PIC 9(4).
           03  FILLER           PIC X(01).
           03  D-TIME           PIC 99/99/99.
           03  FILLER           PIC X(01).
           03  D-RECS-PURGED    PIC ZZ,ZZZ,ZZZ,ZZ9.
           03  FILLER           PIC X(01).
           03  D-USERID         PIC X(10).

      *-------------------------------------------------------{ RANGES }
       01  RANGE-LINE-01              REDEFINES DETAIL-LINE.
           03  R01-PROMPT             PIC X(17).
           03  FILLER                 PIC X(01).
           03  R01-FIELD              PIC X(50).

       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBGB/GETPRW.CPY".
       COPY "LIBGB/DATERW.CPY".

       01  GLLLST-WINDOW-HANDLE PIC X(10).
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBLS/GLLLST_DEF.CPY".
       COPY "LIBLS/GLLLST_WKS.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

      /***************************************
      *     P R O G R A M   C O N T R O L    *
      ****************************************
       COPY "LIBGB/GETFMW.CPY".

       SCREEN SECTION.
       COPY "LIBLS/GLLLST_SCN.CPY".

       PROCEDURE DIVISION
             USING FORM-PATHNAME EXIT-PATHNAME.
       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON PR-FILE.
       COPY "LIBGB/DECLARE.CPY".
           PERFORM CLOSE-GLLOGS1-FILE.
           PERFORM CLOSE-PR-FILE.
       COPY "LIBGB/DECLAR2.CPY".

      *****************************
       MAIN-MODULE SECTION.
      *****************************
           PERFORM SQL-CONNECT.

       COPY "LIBGB/CHKSEC.CPY".

      D    STOP MESS.

           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "GLLLST" TO VDU-FORM-NAME.

           DISPLAY FLOATING GRAPHICAL WINDOW
                   SCREEN LINE 01 * VDU-WIN-L-OFFSET
                   SCREEN COL 01 * VDU-WIN-C-OFFSET
                   CELL SIZE IS LABEL FONT
                   SIZE 80 + VDU-WIN-W-MOD
                   LINES 24 + VDU-WIN-H-MOD
                   BLANK SCREEN
                   HANDLE IS GLLLST-WINDOW-HANDLE.

           PERFORM FORM-RESET.

           IF EXT-ROUTE-BUF NOT = "00"                                          
              MOVE "PROGRAM NOT BATCHABLE" TO MESS                              
              PERFORM SEND-MESS        
              MOVE "99" TO EXT-ROUTE-BUF                                        
              GO TO END-PROGRAM.                                                

           PERFORM INITIALIZATION.
           IF ERRCD NOT = " "
              GO TO END-PROGRAM.

       NEXT-GLLOGS.
           PERFORM READ-GLLOGS1-FILE-NEXT.
           IF IO-FG = 9
              GO TO END-ROUTINE.

           PERFORM DETAIL-LINE-PROCESS.

           GO TO NEXT-GLLOGS.
       END-ROUTINE.
           PERFORM PRINT-RANGE-LINES.
           IF FIRST-TIME = "Y"
              MOVE " NO RECORDS FOUND " TO MESS
              PERFORM SEND-MESS.
           GO TO END-PROGRAM.

      ***********************************
       INITIALIZATION SECTION.
      ***********************************

           MOVE " " TO ERRCD.
       GET-PR.
           MOVE GLLLST-QUEUE-POS TO Q-POS.
           MOVE GLLLST-COPIES-POS TO C-POS.
           MOVE GLLLST-PRU-POS TO P-POS.
           MOVE SPACES TO Q-BUF C-BUF P-BUF.

           PERFORM PRU-IN.
           IF STAT NOT = "00" GO TO END-INIT.
      *SET BEGINNING ID
       ID-01.
           MOVE "E  A060BEG-ID." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO GET-PR.
           IF VDUSTATUS = "16" GO TO ALL-ID.
           IF VDUSTATUS = "1>" GO TO END-INIT.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO ID-01.
           MOVE VDUBUF TO BEG-ID.
      *SET ENDING ID
       ID-02.
           MOVE "E  A060END-ID." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO GET-PR.
           IF VDUSTATUS = "1U" GO TO ID-01.
           IF VDUSTATUS = "1>" GO TO END-INIT.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO ID-02.
           MOVE VDUBUF TO END-ID.
           IF BEG-ID > END-ID  
               MOVE "INVALID ID RANGE !" TO MESS
               PERFORM SEND-MESS
               GO TO ID-01.
           GO TO OK-YN.

      *F3 ALL CLASS   
       ALL-ID.
           MOVE "S  A060BEG-ID." TO VDU.
           MOVE SPACES TO BEG-ID  VDUBUF.
           PERFORM SCREENIO.
           MOVE "S  A060END-ID." TO VDU.
           MOVE ALL "Z" TO END-ID  VDUBUF.
           PERFORM SCREENIO.
       OK-YN.
           MOVE "E  A010ACCEPT." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO ID-02.
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

           PERFORM OPEN-GLLOGS1-FILE.
           MOVE BEG-ID TO GLLOGS-ID.
           MOVE ZEROES TO GLLOGS-SYSTEM-DATE.
           MOVE ZEROES TO GLLOGS-SEQNO.
           PERFORM START-GLLOGS1-FILE.
           PERFORM OPEN-PR-FILE.

           GO TO EXIT-INITIALIZE.
       END-INIT.
           MOVE "X" TO ERRCD.
       EXIT-INITIALIZE.
           EXIT.

      ****************************
       DETAIL-LINE-PROCESS SECTION.
      ****************************
           MOVE GLLOGS-ID          TO D-ID.
           MOVE GLLOGS-SYSTEM-DATE TO D-SYSTEM-DATE.
           MOVE GLLOGS-SEQNO       TO D-SEQNO.
           MOVE GLLOGS-BEG-DATE    TO D-BEG-DATE.
           MOVE GLLOGS-END-DATE    TO D-END-DATE.
           MOVE GLLOGS-BRNO-RANGE  TO D-BRNO-RANGE.
           MOVE GLLOGS-TIME        TO D-TIME.
           INSPECT D-TIME REPLACING ALL "/" BY ":".
           MOVE GLLOGS-RECS-PURGED TO D-RECS-PURGED.
           MOVE GLLOGS-USERID      TO D-USERID.
                       
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

      ****************************************************
       FORM-RESET SECTION.
      ****************************************************
           MOVE EXT-CONAME-CONAME TO GLLLST-CONAME.
           MOVE EXT-CONAME-SYSDATE TO GLLLST-SYSDATE.
           DISPLAY GLLLST-SCREEN UPON GLLLST-WINDOW-HANDLE.
           MOVE GLLLST-FORM-FIELDS TO WR-BUF.
           MOVE GLLLST-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      ****************************************************
       SETUP-LINK-INFO SECTION.
      ****************************************************
           MOVE GLLLST-HELP--FILE TO HELP-LINK-FILE.
           MOVE GLLLST-HELP--DIR  TO HELP-LINK-DIR.

      ****************************************************
       VDU-CONVERT-FIELD SECTION.
      ****************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBLS/GLLLST_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ***************************
       MISC SECTION.
      ***************************
       COPY "LIBGB/GETPR.CPY".
       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/ACCESS.CPY".

       PRINT-RANGE-LINES.
           MOVE 3 TO LN-FEED.
           MOVE "BEGINNING ID :" TO R01-PROMPT.
           MOVE BEG-ID           TO R01-FIELD.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "ENDING    ID :" TO R01-PROMPT.
           MOVE END-ID           TO R01-FIELD.
           PERFORM WRITE-DETAIL-LINE.

      ***************************
       IO-ROUTINE SECTION.
      ***************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGL/GLGLLOGSGS_SQL.CPY".
       COPY "LIBGB/GBPRINT.CPY".
       COPY "LIBGL/GLGLLOGS1RN.CPY".
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
           DESTROY GLLLST-SCREEN.
           DESTROY GLLLST-WINDOW-HANDLE.
           EXIT PROGRAM.
