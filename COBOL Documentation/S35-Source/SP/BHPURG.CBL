       IDENTIFICATION DIVISION.
       PROGRAM-ID.      BHPURG.
       DATE-WRITTEN.    050301.
      ******************************************************************
      *
      *                      (SP)
      *      DESCRIPTION: BULLETIN BOARD HISTORY PURGE
      *
      * REV :
      * BLV 010720 CHANGE BH-FILE TO BE IN DESCENDING DATE ORDER,
      *            WORLD PR# 272
      * BAH 170725 ACTIVATE 8 DIGIT DATES, PD0006
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
      *    W O R K I N G - S T O R A G E - S E C T I O N
      *
      ******************************************************************
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)SP/BHPURG.CBL S34 10/29/21-10:11:22 >".

      ******************************************************************
      *
      *    F I L E   I F N
      *
      ******************************************************************
       COPY "LIBSP/SP01BH.CPY".
       COPY "LIBSP/SP01BH_SQL.CPY".
       COPY "LIBSP/SPWSBH.CPY".
      *-----------------------------------------------------------------
       01  MSEL-LIST            PIC X(5)   VALUE "MSEL.".
       01  LEGEND               PIC X(70)  VALUE SPACES.
       01  LN-FEED              PIC 99.

      ******************************************************************
      *
      *     S C R E E N   F I E L D S
      *
      ******************************************************************
       COPY "LIBGB/EOBUF_EXT.CPY".
      *-----------------------------------------------------------------
           03  BEG-USERID      PIC X(10).
           03  END-USERID      PIC X(10).
           03  BEG-DATE        PIC 9(8).
           03  END-DATE        PIC 9(8).

       01  VERSION-CONTROL     PIC X     VALUE "A".

       01  BEG-HISTORY-DATE      PIC 9(8).
       01  END-HISTORY-DATE      PIC 9(8).

      ******************************************************************
      *
      *     C O P Y - L I B R A R Y - W O R K E R S
      *
      ******************************************************************
       COPY "LIBWI/PFSWINDOWSW.CPY".
       COPY "LIBWI/TIMERW.CPY".
       COPY "LIBSP/BHPURG_CMD.CPY".
       COPY "LIBGB/GETPRW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/DACCESSW.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/TIMEWK.CPY".
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       COPY "LIBGB/SYSTEMW.CPY".
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       COPY "LIBGB/EOSPOOLW.CPY".
       COPY "LIBGB/PASSWDW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBSP/BHPURG_DEF.CPY".
       COPY "LIBWI/TIMER_DEF.CPY".
       COPY "LIBSP/BHPURG_WKS.CPY".
       COPY "LIBWI/TIMER_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

      ******************************************************************
      *
      *     P R O G R A M   C O N T R O L
      *
      ******************************************************************
       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBSP/BHPURG_SCN.CPY".
       COPY "LIBWI/TIMER_SCN.CPY".

      *-----------------------------------------------------------------
       PROCEDURE DIVISION
            USING FORM-PATHNAME EXIT-PATHNAME.

       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
               PR-FILE.
       COPY "LIBGB/DECLRP.CPY".
           PERFORM CLOSE-BH1-FILE.
           CLOSE
               PR-FILE.
       COPY "LIBGB/DECLRP2.CPY".

      ******************************************************************
      *
      *   M A I N - M O D U L E
      *
      ******************************************************************
       MAIN-MODULE SECTION.
           PERFORM SQL-CONNECT.

      D    STOP MESS.
           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "BHPURG"      TO FORM-PROG VDU-FORM-NAME.

           PERFORM INITIALIZATION.

           IF EXT-EOBUF-ERRCD NOT = " "
              GO TO END-PROGRAM.

           PERFORM OPEN-BH1-FILE.
           MOVE BEG-USERID       TO BH-USERID.
           MOVE END-HISTORY-DATE TO BH-DATE.
           MOVE 0                TO BH-TIME.
           PERFORM START-BH1-FILE.

      *-----------------------------------------------------------------
       NEXT-BH-REPORT.
           PERFORM READ-BH1-FILE-NEXT.
           IF IO-FG = 9
              GO TO END-ROUTINE.

           IF ( BH-USERID < BEG-USERID ) OR ( BH-USERID > END-USERID )
              GO TO NEXT-BH-REPORT.

           IF ( BH-DATE > BEG-HISTORY-DATE ) OR
               ( BH-DATE < END-HISTORY-DATE )
              GO TO NEXT-BH-REPORT.

           PERFORM DELETE-BH1-FILE.

           GO TO NEXT-BH-REPORT.

       END-ROUTINE.
           GO TO END-PROGRAM.

      ******************************************************************
       INITIALIZATION SECTION.
      ******************************************************************
           MOVE BHPURG-QUEUE-POS TO Q-POS.
           MOVE BHPURG-COPIES-POS TO C-POS.
           MOVE BHPURG-PRU-POS   TO P-POS.
           MOVE SPACES TO Q-BUF C-BUF P-BUF.

       COPY "LIBGB/EOINIT.CPY".

           IF EXT-ROUTE-BUF = "00"
              MOVE "BULLETIN BOARD HISTORY PURGE IN PROGRESS" TO MESS
              PERFORM PROGRESS-MSG.

           COMPUTE BEG-HISTORY-DATE = 99999999 - BEG-DATE.

           COMPUTE END-HISTORY-DATE = 99999999 - END-DATE.

           GO TO EXIT-INITIALIZE.

       ENDIT.
           MOVE "X" TO EXT-EOBUF-ERRCD.

       EXIT-INITIALIZE.
           EXIT.

      ******************************************************************
       EO-EXEC-AUDIT SECTION.
      ******************************************************************
           MOVE EXT-EOBUF-DATE1 TO BEG-DATE.
           MOVE EXT-EOBUF-DATE2 TO END-DATE.

      ******************************************************************
       ENTER-PARAMETERS SECTION.
      ******************************************************************
       ENTER-PARM.

       BEGIN-USERID.
           MOVE LEGEND-USERID-01 TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "E  A100BEG-USER." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "16" GO TO ALL-USERIDS.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO BEGIN-USERID.
           MOVE VDUNUM0 TO BEG-USERID.

      *-----------------------------------------------------------------
       ENDING-USERID.
           MOVE LEGEND-USERID-02 TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "E  A030END-USER." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO BEGIN-USERID.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO ENDING-USERID.
           MOVE VDUNUM0 TO END-USERID.

           IF BEG-USERID > END-USERID
              MOVE "INVALID USER ID RANGE" TO MESS
              PERFORM SEND-MESS
              GO TO BEGIN-USERID.

           GO TO BEGIN-DATE.

      *-----------------------------------------------------------------
       ALL-USERIDS.
           MOVE "S  A030BEG-USER." TO VDU.
           MOVE SPACES             TO BEG-USERID VDUBUF.
           PERFORM SCREENIO.
           MOVE "S  A030END-USER." TO VDU.
           MOVE BABY-ZS            TO END-USERID VDUBUF.
           PERFORM SCREENIO.

      *-----------------------------------------------------------------
       BEGIN-DATE.
           IF ( EXT-ROUTE-BUF NOT = "00" ) AND ( VDUSTATUS = "1U" )
              GO TO ENDING-USERID.

           IF ( EXT-ROUTE-BUF = "40" OR "50" )
              GO TO EXIT-PARM.

           MOVE LEGEND-DATE-01 TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "ENNM080BEG-DATE." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.

           IF VDUSTATUS = "1U" GO TO ENDING-USERID.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO BEGIN-DATE.

           IF ( VDUNUM0 = ZEROES )
              GO TO BEGIN-DATE.

           MOVE VDU-DATE-YYYYMMDD TO BEG-DATE.

      *-----------------------------------------------------------------
       ENDING-DATE.
           IF EXT-ROUTE-BUF NOT = "00" AND VDUSTATUS = "1U"
              GO TO ENDING-USERID.

           MOVE LEGEND-DATE-02 TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "ENNM080END-DATE." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.

           IF VDUSTATUS = "1U" GO TO BEGIN-DATE.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO ENDING-DATE.

           IF ( VDUNUM0 = ZEROES )
              GO TO ENDING-DATE.

           MOVE VDU-DATE-YYYYMMDD TO END-DATE.

           IF END-DATE < BEG-DATE
              MOVE "INVALID DATE RANGE" TO MESS
              PERFORM SEND-MESS
              GO TO BEGIN-DATE.

      *=================================================================
       ENTER-ACCEPT-YN.

           MOVE LEGEND-ACCEPT TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "E  A010ACCEPT." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENDING-DATE.
           IF (        F1-KEY ) GO TO EXIT-PARM.
           IF (        F7-KEY ) GO TO EXIT-PARM.
           IF NOT ( ENTER-KEY ) GO TO ENTER-ACCEPT-YN.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - -{ TIMER  }
           IF ( VDUBUF = "T" ) PERFORM TIMER-ROUTINE
                               MOVE TIMER-ACCEPT-YN TO VDUBUF.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( VDUBUF     = "N" ) GO TO ENTER-PARM.
           IF ( VDUBUF NOT = "Y" ) GO TO ENTER-ACCEPT-YN.

      * MUST SET THIS SO EOINIT DOES NOT STOP ON THE ACCEPT FIELD AGAIN!
           MOVE "Y" TO NOACCEPT-BUF.
       EXIT-PARM.
           EXIT.

      ******************************************************************
       DISPLAY-PARAMETERS SECTION.
      ******************************************************************
           MOVE "S  A100BEG-USER." TO VDU.
           MOVE BEG-USERID         TO VDUBUF.
           PERFORM SCREENIO.
      
           MOVE "S  A100END-USER." TO VDU.
           MOVE END-USERID         TO VDUBUF.
           PERFORM SCREENIO.

      ******************************************************************
      *
      *     C O P Y - L I B R A R Y - S E C T I O N S
      *
      ******************************************************************
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/TIMEIO.CPY".

       COPY "LIBWI/PFSWINDOWS.CPY".
       COPY "LIBWI/TIMER.CPY".

      ************************************************
       FORM-RESET SECTION.
      ************************************************
           MOVE EXT-CONAME-CONAME TO BHPURG-CONAME.
           MOVE EXT-CONAME-SYSDATE TO BHPURG-SYSDATE.
           DISPLAY BHPURG-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE BHPURG-FORM-FIELDS TO WR-BUF.
           MOVE BHPURG-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      ************************************************
       SETUP-LINK-INFO SECTION.
      ************************************************
           MOVE BHPURG-HELP--FILE TO HELP-LINK-FILE.
           MOVE BHPURG-HELP--DIR TO HELP-LINK-DIR.

      ************************************************
       VDU-CONVERT-FIELD SECTION.
      ************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBSP/BHPURG_EVA.CPY".
              COPY "LIBWI/TIMER_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ******************************************************************
       MISC-PARAGRAPHS SECTION.
      ******************************************************************
      *-----------------------------------------------------------------
       COPY "LIBGB/SYSTEM.CPY".
       COPY "LIBGB/GETPR.CPY".
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/DACCESS.CPY".
      *-----------------------------------------------------------------
       PROGRESS-MSG.
           MOVE MESS TO WR-BUF.
           MOVE MSEL-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.
      *-----------------------------------------------------------------
       SEND-LEGEND.
           MOVE LEGEND TO WR-BUF.
           MOVE MSEL-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      ******************************************************************
       IO-ROUTINE SECTION.
      ******************************************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBSP/SPBHGS_SQL.CPY".
       COPY "LIBGB/GBPRINT.CPY".
       COPY "LIBSP/SPBH1IN.CPY".
       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO EXT-EOBUF-ERRCD.

      ******************************************************************
       END-PROGRAM SECTION.
      ******************************************************************
       END-RUN.
           PERFORM CLOSE-FILES.

       EXIT-PROG.
           IF EXT-ROUTE-BUF = "00"
              MOVE "LP/EMPUMU" TO FORM-NAM
              MOVE FORM-PATH TO FORM-PATHNAME.

       STOP-RUN.
           IF EXT-ROUTE-BUF = "00" OR "40" OR "50"
              DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN
              DESTROY BHPURG-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.
           EXIT PROGRAM.
