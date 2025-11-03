       IDENTIFICATION DIVISION.
       PROGRAM-ID.      FXRCST.
       DATE-WRITTEN.    021094.
      *************************************************************
      *  DESCRIPTION:  CHANGE ALL 'OLD' STATUSES TO 'NEW' STATUSES
      *                 IN THE RCFILE FOR DATE RANGE ENTERED
      *
      * REV:
      * MJD 070695 ADDED BRANCH RANGE.
      *
      * KEC 2015.1120 {PD#00003} PARADATA <A30> [JAY CISZEWSKI]
      *         FIXED RC-TRANS-DATE FROM YYYMMDD TO CCYYMMDD.
      *  BAH 2022.1006 HARD CODED STARTS
      *
      *************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       DATA DIVISION.
       FILE SECTION.
 
 
      ******************************************
      *      WORKING STORAGE SECTION
      ******************************************
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)FX/FXRCST.CBL A32 10/05/20-13:53:04 >".
       COPY "LIBGB/GB01RC.CPY".
       COPY "LIBGB/GB01RC_SQL.CPY".
       COPY "LIBGB/GBWSRC.CPY".

       01  ERRCD             PIC X.
 
       01  ACCEPT-BUF        PIC X     VALUE " ".

       01  BEG-DATE           PIC 9(8).
       01  END-DATE           PIC 9(8).
       01  BEG-BRNO           PIC 9(4).
       01  END-BRNO           PIC 9(4).
       01  SOURCE-STAT        PIC XX.
       01  TARGET-STAT        PIC XX.


       01  MESS-LIST         PIC X(5)  VALUE "MESS.".
       01  MESS-BUF          PIC X(40) VALUE SPACES.
 
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/ACCESSW.CPY".

       01  FXRCST-WINDOW-HANDLE PIC X(10).
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBFX/FXRCST_DEF.CPY".
       COPY "LIBFX/FXRCST_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".

      ****************************************
      *     P R O G R A M   C O N T R O L    *
      ****************************************
       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBFX/FXRCST_SCN.CPY".

       PROCEDURE DIVISION
            USING FORM-PATHNAME EXIT-PATHNAME.

      **********************************
      *       MAIN PROGRAM CONTROL
      **********************************
       MAIN-PROGRAM SECTION.

           PERFORM SQL-CONNECT.

           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "FXRCST" TO VDU-FORM-NAME.

           DISPLAY FLOATING GRAPHICAL WINDOW
                   SCREEN LINE 01 * VDU-WIN-L-OFFSET
                   SCREEN COL 01 * VDU-WIN-C-OFFSET
           COLOR IS VDU-WCOLOR-TCLP,CONTROL VALUE IS VDU-WCTRL-TCLP
                   CELL SIZE IS LABEL FONT
                   SIZE 80 + VDU-WIN-W-MOD
                   LINES 24 + VDU-WIN-H-MOD
                   BLANK SCREEN
                   HANDLE IS FXRCST-WINDOW-HANDLE.

       COPY "LIBGB/CHKSEC.CPY".
           PERFORM FORM-RESET.

       ENTER-DATE1.
           MOVE "ERNM060DATE1." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO END-ROUTINE.
           IF VDUSTATUS = "1>" GO TO END-ROUTINE.
           IF VDUSTATUS NOT = "00" AND NOT = "1D"
              GO TO ENTER-DATE1.
           MOVE VDU-DATE-YYYYMMDD TO BEG-DATE.
       ENTER-DATE2.
           MOVE "ERNM060DATE2." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO END-ROUTINE.
           IF VDUSTATUS = "1U" GO TO ENTER-DATE1.
           IF VDUSTATUS = "1>" GO TO END-ROUTINE.
       ENTER-BR1.
           MOVE "ENNN040BR1." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO END-ROUTINE.
           IF VDUSTATUS = "1U" GO TO ENTER-DATE2.
           IF VDUSTATUS = "1>" GO TO END-ROUTINE.
           IF VDUSTATUS = "16" GO TO ALL-BR.
           IF VDUSTATUS NOT = "00" AND NOT = "1D"
              GO TO ENTER-BR1.
           MOVE VDUNUM0 TO BEG-BRNO.
       ENTER-BR2.
           MOVE "ENNN040BR2." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO END-ROUTINE.
           IF VDUSTATUS = "1U" GO TO ENTER-BR1.
           IF VDUSTATUS = "1>" GO TO END-ROUTINE.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO ENTER-BR2.
           IF VDUNUM0 NOT < BEG-BRNO 
                MOVE VDUNUM0 TO END-BRNO
           ELSE
               GO TO ENTER-BR2.
           GO TO ENTER-STAT1.
       ALL-BR.
           MOVE "SNNN040BR1." TO VDU.
           MOVE 0 TO BEG-BRNO VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN040BR2." TO VDU.
           MOVE 9999 TO END-BRNO VDUNUM0.
           PERFORM SCREENIO.

       ENTER-STAT1.
           MOVE "E  A000STAT1." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO END-ROUTINE.
           IF VDUSTATUS = "1U" GO TO ENTER-BR2.
           IF VDUSTATUS = "1>" GO TO END-ROUTINE.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO ENTER-STAT1.
           MOVE VDUBUF TO SOURCE-STAT
                          RC-STATUS.
           IF NOT RC-STATUS-VALID
              GO TO ENTER-STAT1.
           
       ENTER-STAT2.
           MOVE "E  A000STAT2." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO END-ROUTINE.
           IF VDUSTATUS = "1U" GO TO ENTER-STAT1.
           IF VDUSTATUS = "1>" GO TO END-ROUTINE.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO ENTER-STAT2.
           MOVE VDUBUF TO TARGET-STAT
                          RC-STATUS.
           IF NOT RC-STATUS-VALID
              GO TO ENTER-STAT2.
           IF TARGET-STAT = SOURCE-STAT
              GO TO ENTER-STAT2.

       CHECK-RANGES.
           MOVE " " TO ERRCD.

       ENTER-ACCEPT.
           MOVE "E  A010ACCEPT." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO END-ROUTINE.
           IF VDUSTATUS NOT = "00" GO TO ENTER-ACCEPT.
           MOVE VDUBUF TO ACCEPT-BUF.
           IF ACCEPT-BUF = "N" GO TO END-ROUTINE.
           IF ACCEPT-BUF NOT = "Y" GO TO ENTER-ACCEPT.

           PERFORM OPEN-RC1-FILE.

           MOVE BEG-BRNO    TO RC-BRNO
                               QRC1-WBEG-BRNO.
           MOVE END-BRNO    TO QRC1-WEND-BRNO.
           IF BEG-DATE = 0
              MOVE 19000101 TO RC-TRANS-DATE SQL-DATE-YYYYMMDD
           ELSE
              MOVE BEG-DATE TO RC-TRANS-DATE SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO QRC1-WBEG-TRANS-DATE.
           IF END-DATE = 0
              MOVE 19000101 TO SQL-DATE-YYYYMMDD
           ELSE
              MOVE END-DATE TO SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO QRC1-WEND-TRANS-DATE.

           PERFORM START-RC1-FILE.

           MOVE "PROGRAM IN PROGRESS..........." TO MESS-BUF.
           PERFORM SEND-LEGEND.

       NEXT-RC.
           PERFORM READ-RC1-FILE-NEXT.
           IF IO-FG = 9
              GO TO END-ROUTINE.

           IF (RC-TRANS-DATE < BEG-DATE) OR (RC-TRANS-DATE > END-DATE)
              GO TO NEXT-RC.

           IF (RC-BRNO < BEG-BRNO) OR (RC-BRNO > END-BRNO)
              GO TO NEXT-RC.

           IF RC-STATUS NOT = SOURCE-STAT
              GO TO NEXT-RC.

           MOVE TARGET-STAT TO RC-STATUS.

           PERFORM REWRITE-RC1-FILE.
           GO TO NEXT-RC.

       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/DATER.CPY".

      *******************************************************
       FORM-RESET SECTION.
      *******************************************************
           MOVE EXT-CONAME-CONAME TO FXRCST-CONAME.
           MOVE EXT-CONAME-SYSDATE TO FXRCST-SYSDATE.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DISPLAY FXRCST-SCREEN UPON FXRCST-WINDOW-HANDLE.
           MOVE FXRCST-FORM-FIELDS TO WR-BUF.
           MOVE FXRCST-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.


      *******************************************************
       SETUP-LINK-INFO SECTION.
      *******************************************************
           MOVE FXRCST-HELP--FILE TO HELP-LINK-FILE.
           MOVE FXRCST-HELP--DIR  TO HELP-LINK-DIR.

      *******************************************************
       VDU-CONVERT-FIELD SECTION.
      *******************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBFX/FXRCST_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ******************************
       MISC-PARA SECTION.
      ******************************
       COPY "LIBGB/ACCESS.CPY".
       SEND-LEGEND.
           MOVE MESS-BUF TO WR-BUF.
           MOVE MESS-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.
 
      *******************************************
       IO-ROUTINE SECTION.
      *******************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/DECLARE_SQL.CPY".
           PERFORM CLOSE-RC1-FILE.
       COPY "LIBGB/GBRCGS_SQL.CPY".
       COPY "LIBGB/GBRC1IN.CPY".
 
       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO ERRCD.
 
      *******************************************
       END-ROUTINE SECTION.
      *******************************************
       END-RUN.
           PERFORM CLOSE-FILES.
       EXIT-PROG.
           MOVE "FX/FXMENU" TO FORM-NAM.
           MOVE FORM-PATH TO FORM-PATHNAME.
       STOP-RUN.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY FXRCST-SCREEN.
           DESTROY FXRCST-WINDOW-HANDLE.
           EXIT PROGRAM.
