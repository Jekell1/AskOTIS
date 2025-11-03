       IDENTIFICATION DIVISION.
       PROGRAM-ID.      ELAPS.
       DATE-WRITTEN.    101695.
      **************************************************************************
      *          ELAPSED TIME WORK WINDOW
      * REV:
      *  JTG 062696 INCREASED TIME FIELDS TO ZZZZ9-.
      *  JTG 031897 ADDED OPTION FOR ODD UNIT PERIOD CODES
      *  JTG 040497 ADDED OPTION FOR 362 OR 361 YEAR TYPE
      *  JTG 990409 REMOVED F9 PASSWORD REQUIREMENT FOR UNITPER LOGIC
      *  JTG 040113 ADDED 'F9-UNITPER' TO PROMPT
      *  BAH 170417 ACTIVATE 8 DIGIT DATES, PD0006
      *  BAH 170505 ENTER FULL 8 DIGIT DATE
      **************************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       I-O-CONTROL.
       DATA DIVISION.
       FILE SECTION.

       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(40) VALUE
           "@(#)WI/ELAPS.CBL S34 05/24/21-13:34:02 >".
      **************************************************
      *     P R O G R A M   W O R K E R S              *
      **************************************************
       01  ERRCD               PIC X.
       01  PASSWD.
           03  PASSWDD         PIC X(6)     VALUE X"333533363335".
           03  PASSWRD        REDEFINES PASSWDD
                               PIC 9(6).

       01 ELAPS-WINDOW-HANDLE  PIC X(10).


      *---------------------------------------------------
      *    HEAD DISPLAY FIELDS
      *---------------------------------------------------
       01  HEAD-LIST              PIC X(16)  VALUE "HEAD1:S HEAD2:S.".
       01  HEAD-BUF.
           03  FILLER             PIC X(40)  VALUE
           "               ---358     ---360     ---".
           03  HEAD-362361        PIC 999.
           03  FILLER             PIC X(35)  VALUE
           "     ---365     ---367     ---999  ".
      *---------------------------------------------------
      *    UNIT PERIOD FIELDS
      *---------------------------------------------------
       01  YR362361               PIC 9(3).
       01  UNITPER-FG             PIC X     VALUE "N".
       01  UPERCD                 PIC X.
       01  FREQ                   PIC 999.
       01  XX-VDUNUM9.
           03  X-VDUNUM9          PIC ZZ9.999999999.

       01  UNITPER-NUM-LIST       PIC X(41) VALUE
           "362361 PERYR DAYSIN UPERCD FREQ UPER REM.".
       01  UNITPER-NUM-BUF.
           03  X-362361           PIC 999.
           03  X-PERYR            PIC ZZ9.999999999.
           03  X-DAYSIN           PIC ZZZZZ9.
           03  X-UPERCD           PIC X.
           03  X-FREQ             PIC ZZ9.
           03  X-UPER             PIC ZZZZ9- BLANK ZERO.
           03  X-REM              PIC ZZZZ9- BLANK ZERO.

       01  UNITPER-LIST.
           03  FILLER             PIC X(47) VALUE
           "362361A:S UPERCDA:S FREQA:S PERYRA:S DAYSINA:S ".
           03  FILLER             PIC X(15) VALUE
           "UPERA:S REMA:S.".
       01  UNITPER-BUF.
           03  FILLER             PIC X(13) VALUE
               "TIME 362/361:".
           03  FILLER             PIC X(13) VALUE
               "UNITPER CODE:".
           03  FILLER             PIC X(13) VALUE
               '   "    FREQ:'.
           03  FILLER             PIC X(23) VALUE
               "UNIT PERIODS PER YEAR :".
           03  FILLER             PIC X(23) VALUE
               "DAYS IN A UNIT PERIOD :".
           03  FILLER             PIC X(23) VALUE
               "ELAPSED UNIT PERIODS  :".
           03  FILLER             PIC X(23) VALUE
               '   "    REMAINING     :'.

      *---------------------------------------------------
      *    ELAPSED FIELDS
      *---------------------------------------------------
       01  FROM-DATE              PIC 9(8) COMP.
       01  TO-DATE                PIC 9(8) COMP.
       01  DISPLAY-LIST           PIC X(16)     VALUE
           "L01 L02 L03 L04.".
       01  DISPLAY-BUF            VALUE " ".
           03  DISPLAY-LINE       OCCURS 4
                                  PIC X(78).
       01  DISPLAY-DLINE.
           03  FILLER             PIC X(11) VALUE " DAYS     :".
           03  FILLER             PIC X(5)  VALUE " ".
           03  D-358              PIC ZZZZ9-.
           03  FILLER             PIC X(5)  VALUE " ".
           03  D-360              PIC ZZZZ9-.
           03  FILLER             PIC X(5)  VALUE " ".
           03  D-362-361          PIC ZZZZ9-.
           03  FILLER             PIC X(5)  VALUE " ".
           03  D-365              PIC ZZZZ9-.
           03  FILLER             PIC X(5)  VALUE " ".
           03  D-367              PIC ZZZZ9-.
           03  FILLER             PIC X(5)  VALUE " ".
           03  D-999              PIC ZZZZ9-.
       01  DISPLAY-YLINE.
           03  FILLER             PIC X(11) VALUE " YEARS    :".
           03  FILLER             PIC X(5)  VALUE " ".
           03  Y-358              PIC ZZZZ9-.
           03  FILLER             PIC X(5)  VALUE " ".
           03  Y-360              PIC ZZZZ9-.
           03  FILLER             PIC X(5)  VALUE " ".
           03  Y-362-361          PIC ZZZZ9-.
           03  FILLER             PIC X(5)  VALUE " ".
           03  Y-365              PIC ZZZZ9-.
           03  FILLER             PIC X(5)  VALUE " ".
           03  Y-367              PIC ZZZZ9-.
           03  FILLER             PIC X(5)  VALUE " ".
           03  Y-999              PIC ZZZZ9-.
       01  DISPLAY-MLINE.
           03  FILLER             PIC X(11) VALUE " MONTHS   :".
           03  FILLER             PIC X(5)  VALUE " ".
           03  M-358              PIC ZZZZ9-.
           03  FILLER             PIC X(5)  VALUE " ".
           03  M-360              PIC ZZZZ9-.
           03  FILLER             PIC X(5)  VALUE " ".
           03  M-362-361          PIC ZZZZ9-.
           03  FILLER             PIC X(5)  VALUE " ".
           03  M-365              PIC ZZZZ9-.
           03  FILLER             PIC X(5)  VALUE " ".
           03  M-367              PIC ZZZZ9-.
           03  FILLER             PIC X(5)  VALUE " ".
           03  M-999              PIC ZZZZ9-.
       01  DISPLAY-RLINE.
           03  FILLER             PIC X(11) VALUE " REM DAYS :".
           03  FILLER             PIC X(5)  VALUE " ".
           03  R-358              PIC ZZZZ9-.
           03  FILLER             PIC X(5)  VALUE " ".
           03  R-360              PIC ZZZZ9-.
           03  FILLER             PIC X(5)  VALUE " ".
           03  R-362-361          PIC ZZZZ9-.
           03  FILLER             PIC X(5)  VALUE " ".
           03  R-365              PIC ZZZZ9-.
           03  FILLER             PIC X(5)  VALUE " ".
           03  R-367              PIC ZZZZ9-.
           03  FILLER             PIC X(5)  VALUE " ".
           03  R-999              PIC ZZZZ9-.

       01  LEGENDYR.
           03  FILLER          PIC X(50)   VALUE
           " F7-EXIT, ENTER YEAR TYPE (362 OR 361)        :".
       01  LEGENDU.
           03  FILLER          PIC X(50)   VALUE
           " F7-EXIT, ENTER UNIT PERIOD CODE (M,W,S,B,Y,D):".
       01  LEGENDF.
           03  FILLER          PIC X(50)   VALUE
           " F7-EXIT, ^, ENTER UNIT PERIOD FREQUENCY:".
       01  LEGEND1.
           03  FILLER          PIC X(28)   VALUE
           " F7-EXIT, F3-DECR, F4-INCR, ".
           03  FILLER          PIC X(28)   VALUE
           "F9-UNITPER, ENTER FROM DATE:".
       01  LEGEND2.
           03  FILLER          PIC X(28)   VALUE
           " F7-EXIT, F3-DECR, F4-INCR, ".
           03  FILLER          PIC X(26)   VALUE
           "F9-UNITPER, ENTER TO DATE:".
       01  LEGEND                PIC X(70).
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBLP/LPUPERW.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBWI/ELAPS_DEF.CPY".
       COPY "LIBWI/ELAPS_WKS.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/HELPLINK.CPY".


      /*************************************************
      *   C O N T R O L   M O D U L E                  *
      **************************************************
       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBWI/ELAPS_SCN.CPY".
       PROCEDURE DIVISION
             USING FORM-PATHNAME EXIT-PATHNAME.

      **************************************************
      *      P R O G R A M   C O N T R O L             *
      *************************************************
       MAIN-PROGRAM SECTION.

           PERFORM SQL-CONNECT.

       COPY "LIBGB/CHKSEC.CPY".
      D    STOP MESS.
           DISPLAY FLOATING WINDOW
                   SCREEN LINE 02 * VDU-WIN-L-OFFSET
                   SCREEN COL 02 * VDU-WIN-C-OFFSET
           COLOR IS VDU-WCOLOR-TCLP,CONTROL VALUE IS VDU-WCTRL-TCLP
                   CELL SIZE IS LABEL FONT
                   SIZE 78 + VDU-WIN-W-MOD
                   LINES 15 + VDU-WIN-H-MOD
                   BLANK SCREEN
                   HANDLE IS ELAPS-WINDOW-HANDLE.

           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "ELAPS" TO VDU-FORM-NAME.
           PERFORM FORM-RESET.

           MOVE " " TO VDU-WIMENU-SELECTION
                       VDU-CLMENU-SELECTION.

       MAIN-MODULE.
      D    STOP MESS.
           MOVE " " TO ERRCD.

      * SET UP DEFAULTS:
           MOVE "N" TO UNITPER-FG.
           MOVE 0 TO FROM-DATE
                     TO-DATE.
           MOVE "M" TO UPERCD.
           MOVE 1 TO FREQ.
           MOVE 362 TO YR362361
                       HEAD-362361.
           MOVE HEAD-LIST TO WR-LIST.
           MOVE HEAD-BUF TO WR-BUF.
           PERFORM CALL-WRFORM.
           GO TO ENTER-DATE1.

       ENTER-362361.
           MOVE LEGENDYR TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "ENNN030362361." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>"
              GO TO END-ROUTINE.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO ENTER-362361.
           MOVE VDUNUM0 TO YR362361
                           HEAD-362361.
           IF NOT (YR362361 = 362 OR 361)
              GO TO ENTER-362361.
           MOVE HEAD-LIST TO WR-LIST.
           MOVE HEAD-BUF TO WR-BUF.
           PERFORM CALL-WRFORM.

       ENTER-UPERCD.
           MOVE LEGENDU TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "E  A010UPERCD." TO VDU.
           PERFORM SCREENIO.
           IF UP-KEY
              GO TO ENTER-362361.
           IF VDUSTATUS = "1>"
              GO TO END-ROUTINE.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO ENTER-UPERCD.
           MOVE VDUBUF TO UPERCD.
           IF NOT (UPERCD = "M" OR "W" OR "S" OR "Y" OR "D" OR "B")
              GO TO ENTER-UPERCD.

       ENTER-FREQ.
           MOVE LEGENDF TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "ENNN030FREQ." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-UPERCD.
           IF VDUSTATUS = "1>"
              GO TO END-ROUTINE.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO ENTER-FREQ.
           MOVE VDUNUM0 TO FREQ.
           IF FREQ = 0
              GO TO ENTER-FREQ.

           MOVE UPERCD TO UPER-UNITPER-CD.
           MOVE FREQ TO UPER-UNITPER-FREQ.
           MOVE 20010101 TO UPER-INTDATE
                            UPER-1STPYDATE.
           MOVE 12 TO UPER-TERM.
           PERFORM UPER-CALCULATION.

           MOVE "S  A130PERYR." TO VDU.
           MOVE UPER-UNITPER-PER-YEAR TO X-VDUNUM9.
           MOVE XX-VDUNUM9 TO VDUBUF.
           PERFORM SCREENIO.
           MOVE "SNNS050DAYSIN." TO VDU.
           MOVE UPER-DAYS-IN-UNITPER TO VDUNUM0.
           PERFORM SCREENIO.

       ENTER-DATE1.
           MOVE LEGEND1 TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "ERND080DATE1." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "1>"
              GO TO END-ROUTINE.

           IF UNITPER-FG = "Y"
              IF VDUSTATUS = "1U"
                 GO TO ENTER-UPERCD.

           IF VDUSTATUS = "16" OR "18"
              IF FROM-DATE NOT = 0
                 MOVE FROM-DATE TO NDTE-DATE
                 IF VDUSTATUS = "18"
                    MOVE 1 TO NDTE-HOLD
                 ELSE
                    MOVE -1 TO NDTE-HOLD
                 END-IF
                 PERFORM NEW-DATE-CALCULATION
                 MOVE NDTE-DATE TO FROM-DATE
                 MOVE "S  A100DATE1." TO VDU
                 MOVE FROM-DATE TO VDU-DATE-8-EDIT
                 PERFORM SCREENIO
                 PERFORM DISPLAY-FIELDS
                 GO TO ENTER-DATE1.
           IF VDUSTATUS = "1("
              IF UNITPER-FG = "Y"
                 MOVE UNITPER-LIST TO WR-LIST
                 MOVE SPACES TO WR-BUF
                 PERFORM CALL-WRFORM
                 MOVE UNITPER-NUM-LIST TO WR-LIST
                 MOVE SPACES TO WR-BUF
                 PERFORM CALL-WRFORM
                 MOVE "N" TO UNITPER-FG
                 GO TO ENTER-DATE1
              ELSE
      *          MOVE "ENNN060UPER." TO VDU
      *          PERFORM SCREENIO
                 MOVE UNITPER-LIST TO WR-LIST
                 MOVE UNITPER-BUF TO WR-BUF
                 PERFORM CALL-WRFORM
                 MOVE YR362361 TO X-362361
                 MOVE "M" TO UPERCD X-UPERCD
                 MOVE 1 TO FREQ X-FREQ
                 MOVE 0 TO X-UPER
                           X-REM
                 MOVE 1 TO X-PERYR
                 MOVE 30 TO X-DAYSIN
                 MOVE UNITPER-NUM-LIST TO WR-LIST
                 MOVE UNITPER-NUM-BUF TO WR-BUF
                 PERFORM CALL-WRFORM
                 MOVE "Y" TO UNITPER-FG
                 GO TO ENTER-UPERCD.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO ENTER-DATE1.
           MOVE VDU-DATE-YYYYMMDD TO FROM-DATE.

       ENTER-DATE2.
           MOVE LEGEND2 TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "ERND080DATE2." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "1>"
              GO TO END-ROUTINE.
           IF VDUSTATUS = "1U"
              GO TO ENTER-DATE1.
           IF VDUSTATUS = "16" OR "18"
              IF TO-DATE NOT = 0
                 MOVE TO-DATE TO NDTE-DATE
                 IF VDUSTATUS = "18"
                    MOVE 1 TO NDTE-HOLD
                 ELSE
                    MOVE -1 TO NDTE-HOLD
                 END-IF
                 PERFORM NEW-DATE-CALCULATION
                 MOVE NDTE-DATE TO TO-DATE
                 MOVE "S  A080DATE2." TO VDU
                 MOVE TO-DATE TO VDU-DATE-8-EDIT
                 PERFORM SCREENIO
                 PERFORM DISPLAY-FIELDS
                 GO TO ENTER-DATE2.

           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO ENTER-DATE2.

           MOVE VDU-DATE-YYYYMMDD TO TO-DATE.

           PERFORM DISPLAY-FIELDS.
           GO TO ENTER-DATE1.

      ***************************************
       DISPLAY-FIELDS SECTION.
      ***************************************
           MOVE FROM-DATE TO NUM-DATE.
           MOVE TO-DATE   TO SYS-DATE.

           MOVE 358 TO ELAPSED-YRTYPE.
           PERFORM TIMALL.
           MOVE ELAPSED-DAYS TO D-358.
           MOVE ELAPSED-MONTHS TO M-358.
           MOVE ELAPSED-REM TO R-358.

           MOVE 360 TO ELAPSED-YRTYPE.
           PERFORM TIMALL.
           MOVE ELAPSED-DAYS TO D-360.
           MOVE ELAPSED-MONTHS TO M-360.
           MOVE ELAPSED-REM TO R-360.

           MOVE YR362361 TO ELAPSED-YRTYPE.
           PERFORM TIMALL.
           MOVE ELAPSED-DAYS TO D-362-361.
           MOVE ELAPSED-MONTHS TO M-362-361.
           MOVE ELAPSED-REM TO R-362-361.

           MOVE 365 TO ELAPSED-YRTYPE.
           PERFORM TIMALL.
           MOVE ELAPSED-DAYS TO D-365.
           MOVE ELAPSED-MONTHS TO M-365.
           MOVE ELAPSED-REM TO R-365.

           MOVE 367 TO ELAPSED-YRTYPE.
           PERFORM TIMALL.
           MOVE ELAPSED-DAYS TO D-367.
           MOVE ELAPSED-MONTHS TO M-367.
           MOVE ELAPSED-REM TO R-367.

           MOVE 999 TO ELAPSED-YRTYPE.
           PERFORM TIMALL.
           MOVE ELAPSED-DAYS TO D-999.
           MOVE ELAPSED-MONTHS TO M-999.
           MOVE ELAPSED-REM TO R-999.

           MOVE DISPLAY-DLINE TO DISPLAY-LINE(1).
           MOVE DISPLAY-MLINE TO DISPLAY-LINE(3).
           MOVE DISPLAY-RLINE TO DISPLAY-LINE(4).
           MOVE DISPLAY-LIST TO WR-LIST.
           MOVE DISPLAY-BUF TO WR-BUF.
           PERFORM CALL-WRFORM.

           IF UNITPER-FG = "Y"
              MOVE UPERCD TO DATER-UNITPER-CD
              MOVE FREQ   TO DATER-UNITPER-FREQ
              PERFORM TIMUPER
              MOVE "SNNS050UPER."  TO VDU
              MOVE ELAPSED-UNITPER TO VDUNUM0
              PERFORM SCREENIO
              MOVE "SNNS050REM."       TO VDU
              MOVE ELAPSED-UNITPER-REM TO VDUNUM0
              PERFORM SCREENIO.


       COPY "LIBGB/DATER.CPY".
       COPY "LIBLP/LPUPER.CPY".
       COPY "LIBGB/SCREEN.CPY".

      ***************************************
       FORM-RESET SECTION.
      ***************************************
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY ELAPS-SCREEN.
           DISPLAY ELAPS-SCREEN UPON ELAPS-WINDOW-HANDLE.
           MOVE ELAPS-FORM-FIELDS TO WR-BUF.
           MOVE ELAPS-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      ***************************************
       SETUP-LINK-INFO SECTION.
      ***************************************
           MOVE ELAPS-HELP--FILE TO HELP-LINK-FILE.
           MOVE ELAPS-HELP--DIR TO HELP-LINK-DIR.

      ***************************************
       VDU-CONVERT-FIELD SECTION.
      ***************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBWI/ELAPS_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      *************************************
       MISC-ROUTINES SECTION.
      *************************************
       COPY "LIBGB/ACCESS.CPY".
       SEND-LEGEND.
           MOVE "WSEL:D." TO WR-LIST.
           MOVE LEGEND TO WR-BUF.
           PERFORM CALL-WRFORM.

      *************************************
       IO-ROUTINES SECTION.
      *************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/DECLARE_SQL.CPY".

       COPY "LIBGB/FERRORS.CPY".
       END-ROUTINE.
           PERFORM CLOSE-FILES.
       EXIT-PROG.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY ELAPS-SCREEN.
           DESTROY ELAPS-WINDOW-HANDLE.
       STOP-RUN.
           MOVE "LP/LPMENU" TO FORM-NAM.
           MOVE FORM-PATH TO FORM-PATHNAME.
           EXIT PROGRAM.
