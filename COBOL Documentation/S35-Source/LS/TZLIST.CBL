       IDENTIFICATION DIVISION.
       PROGRAM-ID.      TZLIST.
       DATE-WRITTEN. 02-10-84
      **************************************************************
      *                      (LS)
      *    DESCRIPTION:  TIME ZONE FILE LISTING
      *
      * REV:
      *  111222 BAH REPLACED RRFORM WITH SCREEN
      **************************************************************
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
           "@(#)LS/TZLIST.CBL S35 12/15/22-09:19:52 >".
       COPY "LIBLP/LP01TZ.CPY".
       COPY "LIBLP/LP01TZ_SQL.CPY".
       COPY "LIBLP/LPWSTZ.CPY".
       01  PR-IFN               PIC X(11)    VALUE "0APRINT".

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
       01  RECORDS-FOUND        PIC X         VALUE " ".
           88  NO-RECORDS-FOUND               VALUE "N".
       01  MESS-LIST            PIC X(5)     VALUE "MESS.".

      ***********************************
      *     S C R E E N   F I E L D S   *
      ***********************************
       01  SCREEN-BUF          VALUE " ".
           03  ERRCD           PIC X.
           03  BEG-KEY.
               05  BEG-AC      PIC 9(03).
               05  BEG-PF      PIC 9(03).
           03  END-KEY.
               05  END-AC      PIC 9(03).
               05  END-PF      PIC 9(03).

       01  OK                   PIC X        VALUE " ".

      *=================================================================
      *
      *    H E A D E R - B U F F E R S
      *
      *=================================================================
       01  HEAD1.
           03  H-DATE                    PIC X(08)     VALUE SPACES.
           03  FILLER                    PIC X(02)     VALUE SPACES.
           03  H-HR                      PIC 9(02)     VALUE ZEROES.
           03  FILLER                    PIC X(01)     VALUE ":".
           03  H-MIN                     PIC 9(02)     VALUE ZEROES.
           03  FILLER                    PIC X(05)     VALUE SPACES.
           03  H-NAME                    PIC X(40)     VALUE SPACES.
           03  FILLER                    PIC X(08)     VALUE SPACES.
           03  FILLER                    PIC X(08)     VALUE "PAGE ".
           03  H-PAGE-NO                 PIC ZZZ9.

      *-----------------------------------------------------------------
       01  HEAD2.
           03  FILLER                    PIC X(10)     VALUE "USER ID:".
           03  H-USERID                  PIC X(10).
           03  FILLER                    PIC X(04)     VALUE SPACES.
           03  FILLER                    PIC X(32)     VALUE
           "- - TIME ZONE  FILE LISTING - -".

      *-----------------------------------------------------------------
       01  HEAD3.
           03  FILLER PIC X(30) VALUE SPACES.
           03  FILLER PIC X(30) VALUE SPACES.
           03  FILLER PIC X(38) VALUE "    TIME          DAYLIGHT    ".

      *-----------------------------------------------------------------
       01  HEAD4.
           03  FILLER PIC X(30) VALUE "DIALING CODE   DESCRIPTION    ".
           03  FILLER PIC X(30) VALUE "                          STAT".
           03  FILLER PIC X(38) VALUE "E   ZONE  FACTOR  SAVING     ".

      *=================================================================
      *
      *    D E T A I L - L I N E
      *
      *=================================================================
       01  DETAIL-LINE                      PIC X(132)     VALUE SPACES.
       01  D-LINE1                REDEFINES DETAIL-LINE.
           03  FILLER                       PIC X(03).
           03  D-AC                         PIC 9(03).
           03  D-DASH                       PIC X(01).
           03  D-PF                         PIC 9(03).
           03  FILLER                       PIC X(05).
           03  D-DESC                       PIC X(40).
           03  FILLER                       PIC X(03).
           03  D-STATE                      PIC X(02).
           03  FILLER                       PIC X(06).
           03  D-ZONE                       PIC X(01).
           03  FILLER                       PIC X(06).
           03  D-FACTOR                     PIC 9-.
           03  FILLER                       PIC X(07).
           03  D-DLS                        PIC X(01).
      *-----------------------------------------------------------------
       01  RANGE-LINE             REDEFINES DETAIL-LINE.
           03  FILLER                       PIC X(22).
           03  RANGE-AC                     PIC 9(03).
           03  RANGE-DASH                   PIC X(01).
           03  RANGE-PF                     PIC 9(03).
           03  FILLER                       PIC X(09).
           03  FILLER                       PIC X(94).

      ******************************************************************
      *
      *    C O P Y - L I B R A R Y - W O R K E R S
      *
      ******************************************************************
       COPY "LIBWI/TZSCANW.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBGB/GETPRW.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       01  TZLIST-WINDOW-HANDLE PIC X(10).
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBLS/TZLIST_DEF.CPY".
       COPY "LIBLS/TZLIST_WKS.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

      /***************************************
      *     P R O G R A M   C O N T R O L    *
      ****************************************
       COPY "LIBGB/GETFMW.CPY".

       SCREEN SECTION.
       COPY "LIBLS/TZLIST_SCN.CPY".

       PROCEDURE DIVISION
             USING FORM-PATHNAME EXIT-PATHNAME.

       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
               PR-FILE.
       COPY "LIBGB/DECLARE.CPY".
           PERFORM CLOSE-TZ1-FILE.
           PERFORM CLOSE-PR-FILE.
       COPY "LIBGB/DECLAR2.CPY".

      *************************************
       MAIN-MODULE SECTION.
      *************************************

           PERFORM SQL-CONNECT.

       COPY "LIBGB/CHKSEC.CPY".

           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "TZLIST" TO VDU-FORM-NAME.

           DISPLAY FLOATING GRAPHICAL WINDOW
                   SCREEN LINE 01 * VDU-WIN-L-OFFSET
                   SCREEN COL 01 * VDU-WIN-C-OFFSET
                   CELL SIZE IS LABEL FONT
                   SIZE 80 + VDU-WIN-W-MOD
                   LINES 24 + VDU-WIN-H-MOD
                   BLANK SCREEN
                   HANDLE IS TZLIST-WINDOW-HANDLE.

           PERFORM FORM-RESET.

           IF EXT-ROUTE-BUF NOT = "00"                                          
              MOVE "PROGRAM NOT BATCHABLE" TO MESS                              
              PERFORM SEND-MESS        
              MOVE "99" TO EXT-ROUTE-BUF                                        
              GO TO END-PROGRAM.                                                

           PERFORM INITIALIZATION.
           IF ERRCD NOT = " "
              GO TO END-PROGRAM.

       NEXT-TZ.
           PERFORM READ-TZ1-FILE-NEXT.
           IF IO-FG = 9
              GO TO END-ROUTINE.

           IF TZ1-KEY > END-KEY
              GO TO END-ROUTINE.

           PERFORM DETAIL-LINE-PROCESS.

           GO TO NEXT-TZ.
       END-ROUTINE.
           IF FIRST-TIME = "Y"
              MOVE "N" TO RECORDS-FOUND
              GO TO END-PROGRAM.

           PERFORM PRINT-RANGE-LINES.
           GO TO END-PROGRAM.

      ***********************************
       INITIALIZATION SECTION.
      ***********************************
           MOVE " " TO ERRCD.
       GETPR.
           MOVE TZLIST-QUEUE-POS TO Q-POS.
           MOVE TZLIST-COPIES-POS TO C-POS.
           MOVE TZLIST-PRU-POS TO P-POS.
           MOVE SPACES TO Q-BUF C-BUF P-BUF.
           PERFORM PRU-IN.
           IF STAT NOT = "00"
              GO TO END-INIT.
        AC-01.
           MOVE "ENNN030AC1." TO VDU.
           PERFORM SCREENIO.
           IF F1-KEY GO TO GETPR.
           IF F3-KEY GO TO ALL-SELECTED.
           IF F7-KEY GO TO END-INIT.
           IF F6-KEY
              MOVE 0 TO TZSCAN-TZ
              PERFORM TZ-SCAN
              IF IO-FG NOT = 0
                 GO TO AC-01
              ELSE
                MOVE "SNNN030AC1." TO VDU
                MOVE TZSCANW-AC TO VDUNUM0 BEG-AC
                PERFORM SCREENIO
                MOVE "SNNN030PF1." TO VDU
                MOVE TZSCANW-PF TO VDUNUM0 BEG-PF
                PERFORM SCREENIO
                GO TO AC-02.
           IF NOT ENTER-KEY GO TO AC-01.
           MOVE VDUNUM0 TO BEG-AC.
       PF-01.
           MOVE "ENNN030PF1." TO VDU.
           PERFORM SCREENIO.
           IF F1-KEY GO TO GETPR.
           IF UP-KEY GO TO AC-01.
           IF F3-KEY
              MOVE "SNNN030PF1." TO VDU
              MOVE ZEROS TO VDUNUM0 BEG-PF
              PERFORM SCREENIO
              GO TO AC-02.
           IF F7-KEY GO TO END-INIT.
           IF F6-KEY
              MOVE BEG-AC TO TZSCANW-AC
              MOVE 0      TO TZSCANW-PF
              MOVE "Y"    TO TZSCAN-PF-ONLY
              PERFORM TZ-SCAN
              IF IO-FG NOT = 0
                 GO TO PF-01
              ELSE
                MOVE "SNNN030PF1." TO VDU
                MOVE TZSCANW-PF TO VDUNUM0 BEG-PF
                PERFORM SCREENIO
                GO TO AC-02.
           IF NOT ENTER-KEY GO TO PF-02.
           MOVE VDUNUM0 TO BEG-PF.

        AC-02.
           MOVE "ENNN030AC2." TO VDU.
           PERFORM SCREENIO.
           IF F1-KEY GO TO GETPR.
           IF F3-KEY GO TO END-SELECTED.
           IF UP-KEY GO TO PF-01.
           IF F7-KEY GO TO END-INIT.
           IF F6-KEY
              MOVE BEG-AC TO TZSCANW-AC
              MOVE BEG-PF TO TZSCANW-PF
              MOVE "Y"    TO TZSCAN-PF-AUTOSTART
              PERFORM TZ-SCAN
              IF IO-FG NOT = 0
                 GO TO AC-02
              ELSE
                MOVE "SNNN030AC2." TO VDU
                MOVE TZSCANW-AC TO VDUNUM0 END-AC
                PERFORM SCREENIO
                MOVE "SNNN030PF2." TO VDU
                MOVE TZSCANW-PF TO VDUNUM0 END-PF
                PERFORM SCREENIO
                GO TO OK-YN.

           IF NOT ENTER-KEY GO TO AC-02.
           MOVE VDUNUM0 TO END-AC.
       PF-02.
           MOVE "ENNN030PF2." TO VDU.
           PERFORM SCREENIO.
           IF F1-KEY GO TO GETPR.
           IF UP-KEY GO TO AC-02.
           IF F3-KEY
              MOVE "SNNN030PF2." TO VDU
              MOVE 999 TO VDUNUM0 END-PF
              PERFORM SCREENIO
              GO TO OK-YN.
           IF F7-KEY GO TO END-INIT.
           IF F6-KEY
              MOVE END-AC TO TZSCANW-AC
              MOVE 0      TO TZSCANW-PF
              IF END-AC = BEG-AC
                 MOVE BEG-PF TO TZSCANW-PF
                 MOVE "Y"    TO TZSCAN-PF-AUTOSTART
              ELSE
                 MOVE 0      TO TZSCANW-PF
              END-IF
              MOVE "Y"    TO TZSCAN-PF-ONLY
              PERFORM TZ-SCAN
              IF IO-FG NOT = 0
                 GO TO PF-02
              ELSE
                MOVE "SNNN030PF2." TO VDU
                MOVE TZSCANW-PF TO VDUNUM0 END-PF
                PERFORM SCREENIO
                GO TO OK-YN.
           IF NOT ENTER-KEY GO TO PF-02.
           MOVE VDUNUM0 TO END-PF.
           GO TO OK-YN.

       ALL-SELECTED.
           MOVE "SNNN030AC1." TO VDU.
           MOVE ZEROS TO VDUNUM0 BEG-AC.
           PERFORM SCREENIO.
           MOVE "SNNN030PF1." TO VDU.
           MOVE ZEROS TO VDUNUM0 BEG-PF.
           PERFORM SCREENIO.
       END-SELECTED.
           MOVE "SNNN030AC2." TO VDU.
           MOVE 999 TO VDUNUM0 END-AC.
           PERFORM SCREENIO.
           MOVE "SNNN030PF2." TO VDU.
           MOVE 999 TO VDUNUM0 END-PF.
           PERFORM SCREENIO.

       OK-YN.
           IF END-AC < BEG-AC
              MOVE " INVALID AREA CODE RANGE" TO MESS
              PERFORM SEND-MESS
              GO TO AC-02.
           IF BEG-AC = END-AC
              IF END-PF < BEG-PF
                 MOVE " INVALID PREFIX RANGE" TO MESS
                 PERFORM SEND-MESS
                 GO TO PF-02.

           MOVE "E  A010ACCEPT." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO GETPR.
           IF VDUSTATUS = "1U" GO TO PF-02.
           IF VDUSTATUS = "1>" GO TO END-INIT.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO OK-YN.
           MOVE VDUBUF TO OK.

           IF OK = "N" GO TO AC-01.
           IF OK NOT = "Y" GO TO OK-YN.

           MOVE "LISTING IN PROGRESS...................." TO WR-BUF.
           MOVE MESS-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

           PERFORM OPEN-PR-FILE.
           PERFORM OPEN-TZ1-FILE.
           MOVE BEG-KEY TO TZ1-KEY.

           PERFORM START-TZ1-FILE.

           MOVE EXT-CONAME-CONAME TO H-NAME.
           CALL "C$JUSTIFY" USING H-NAME "C".
           MOVE EXT-CONAME-SYSDATE TO H-DATE.
           MOVE EXT-CONAME-USER-ID TO H-USERID.
           GO TO EXIT-INITIALIZE.
       END-INIT.
           MOVE 1 TO ERRCD.
       EXIT-INITIALIZE.
           EXIT.

      ******************************************************************
       DETAIL-LINE-PROCESS SECTION.
      ******************************************************************

           ADD 2 LN-COUNT GIVING LN-WK.

           IF LN-WK > EXT-CONAME-RPT-LINES
              PERFORM HEAD-PAGE.

           MOVE TZ-CODE                   TO D-AC.
           MOVE "-"                       TO D-DASH.
           MOVE TZ-PREFIX                 TO D-PF.
           MOVE TZ-DESC                   TO D-DESC.
           MOVE TZ-STATE                  TO D-STATE.
           MOVE TZ-ZONE                   TO D-ZONE.
           MOVE TZ-FACTOR                 TO D-FACTOR.
           MOVE TZ-DLS                    TO D-DLS.

           MOVE 2 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

       EXIT-DETAIL-LINE-PROCESS.
           EXIT.

      ****************************
       WRITE-DETAIL-LINE SECTION.
      ****************************
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

           MOVE HEAD4 TO PR-REC.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-PR-REC.

       END-HEAD-PAGE.
           MOVE 5 TO LN-COUNT.
           MOVE 2 TO LN-FEED.

      *******************************
       PRINT-RANGE-LINES SECTION.
      *******************************
           MOVE 2 TO LN-FEED.
           MOVE "BEGINNING CODE    : " TO RANGE-LINE.
           MOVE BEG-AC                 TO RANGE-AC.
           MOVE BEG-PF                 TO RANGE-PF.
           MOVE "-"                    TO RANGE-DASH.
           PERFORM WRITE-DETAIL-LINE.
           MOVE "ENDING CODE       : " TO RANGE-LINE.
           MOVE END-AC                 TO RANGE-AC.
           MOVE END-PF                 TO RANGE-PF.
           MOVE "-"                    TO RANGE-DASH.
           PERFORM WRITE-DETAIL-LINE.

      ****************************************************
       TZ-SCAN SECTION.
      ****************************************************
           MOVE "WI/TZSCAN" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH TZSCAN-BUF EXIT-PATHNAME.
           CANCEL FORM-PROGX.
           IF (TZSCAN-STATUS NOT = "00") OR TZSCAN-KEY = 0
              MOVE 9 TO IO-FG
              GO TO TZ-SCAN-EXIT.

           MOVE 0               TO IO-FG.
       TZ-SCAN-EXIT.
           EXIT.

       COPY "LIBGB/TIMEIO.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/DATER.CPY".


      ****************************************************
       FORM-RESET SECTION.
      ****************************************************
           MOVE EXT-CONAME-CONAME TO TZLIST-CONAME.
           MOVE EXT-CONAME-SYSDATE TO TZLIST-SYSDATE.
           DISPLAY TZLIST-SCREEN UPON TZLIST-WINDOW-HANDLE.
           MOVE TZLIST-FORM-FIELDS TO WR-BUF.
           MOVE TZLIST-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM. 

      ****************************************************
       SETUP-LINK-INFO SECTION.
      ****************************************************
           MOVE TZLIST-HELP--FILE TO HELP-LINK-FILE.
           MOVE TZLIST-HELP--DIR  TO HELP-LINK-DIR.

      ****************************************************
       VDU-CONVERT-FIELD SECTION.
      ****************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBLS/TZLIST_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ***********************************************************
       MISC SECTION.
      ***********************************************************
       COPY "LIBGB/GETPR.CPY".
       COPY "LIBGB/ACCESS.CPY".

      ***********************************************************
       IO-ROUTINE SECTION.
      ***********************************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBLP/LPTZGS_SQL.CPY".
       COPY "LIBGB/GBPRINT.CPY".
       COPY "LIBLP/LPTZ1RN.CPY".
       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO ERRCD.

      ***********************************************************
       END-PROGRAM SECTION.
      ***********************************************************
       END-RUN.
           PERFORM CLOSE-FILES.
           PERFORM PRU-OUT.
           IF NO-RECORDS-FOUND
              MOVE "NO RECORDS FOUND FOR SELECTED RANGE!" TO MESS
              PERFORM SEND-MESS.
       EXIT-PROG.
           MOVE "LP/LPLSMU" TO FORM-NAM.
           MOVE FORM-PATH TO FORM-PATHNAME.
       STOP-RUN.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY TZLIST-SCREEN.
           DESTROY TZLIST-WINDOW-HANDLE.
           EXIT PROGRAM.
