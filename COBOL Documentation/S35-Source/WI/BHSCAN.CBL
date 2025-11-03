       IDENTIFICATION DIVISION.
       PROGRAM-ID.      BHSCAN.
      **********************************************************************
      *
      *  SCAN BHFILE - BULLETIN BOARD HISTORY FILE SCAN
      *  CALLED FROM: WI/BBINQ2
      *
      * REV.
      * BLV 010719 CHANGED BH DATE TO BE MOST CURRENT FIRST, WORLD PR# 272
      * BAH 170406 ACTIVATE 8 DIGIT DATES, PD0006
      * BAH 2020.0610 CHANGED BH1-KEY TO 22
      **********************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       I-O-CONTROL.
       DATA DIVISION.
       FILE SECTION.

      *****************************************************************
      *
      *    W O R K I N G - S T O R A G E - S E C T I O N
      *
      *****************************************************************
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(37) VALUE
           "@(#)WI/BHSCAN S35 04/22/24-14:10:05 >".

       COPY "LIBSP/SP01BH.CPY".
       COPY "LIBSP/SP01BH_SQL.CPY".
       COPY "LIBSP/SPWSBH.CPY".
      *-----------------------------------------------------------------

       01  BHSCAN-LINES             PIC 99  VALUE 17.
       01  BHSCAN-COLUMNS           PIC 99  VALUE 70.
       01  BHSCAN-BEGIN-Y           PIC 99  VALUE 3.
       01  BHSCAN-BEGIN-X           PIC 99  VALUE 3.
       01  ERRCD                    PIC X    VALUE SPACES.

       01  ONE                      PIC S9     VALUE 1.
       01  SUB                      PIC 99.
       01  SCAN-USERID              PIC X(10).
       01  SCAN-DATE                PIC 9(8).

       78  AM-CMD                VALUE X"616D".
       78  PM-CMD                VALUE X"706D".

      ******************************************************************
      *
      *    F L D S E L
      *
      ******************************************************************
       01  FLDSEL-STAT              PIC XX.
       01  FLDSEL-FIELD             PIC X(4)   VALUE "010.".
       01  KEY-SUBSCRIPT REDEFINES FLDSEL-FIELD.
           03  KEY-SUB              PIC 99.
           03  FILLER               PIC XX.

       01  FIRST-FIELD              PIC X(4)  VALUE "010.".
       01  LAST-FIELD               PIC X(4)  VALUE "090.".

       01  FLDSEL-LIST.
           03  FILLER               PIC X(36)  VALUE
           "010 020 030 040 050 060 070 080 090.".

       01  FLDSEL-BUF.
           03  FLDSEL-DATE        PIC X(8).
           03  FILLER             PIC X(2).
           03  FLDSEL-TIME        PIC X(8).
           03  FILLER             PIC X(2).
           03  FLDSEL-GROUP       PIC X(4).
           03  FILLER             PIC X(2).
           03  FLDSEL-ACTION      PIC X(40).

       01  DISPLAY-BUF.
           03  D-LINE               OCCURS 9 TIMES.
               05  D-DATE           PIC 99/99/99.
               05  FILLER           PIC X(2).
               05  D-TIME           PIC X(8).
               05  FILLER           PIC X(2).
               05  D-GROUP          PIC X(4).
               05  FILLER           PIC X(2).
               05  D-ACTION         PIC X(40).
           03  D-PAGENO             PIC ZZ9.
           03  D-END-MORE           PIC X(7).

       01  DISPLAY-LIST.
           03  FILLER               PIC X(36)  VALUE
           "010 020 030 040 050 060 070 080 090 ".
           03  FILLER               PIC X(15)  VALUE
           "PGNO ENDMORE:F.".

       01  CURRENT-PAGE             PIC S999.
       01  PAGE-COUNT               PIC 999.
       01  MAX-PAGES                PIC 999    VALUE 50.
       01  PAGE-KEY-TABLE.
           03  PAGE-KEY             PIC X(22) OCCURS 50 TIMES.
       01  KEY-SELECT-TABLE.
           03  SELECT-KEY           PIC X(22) OCCURS 9 TIMES.

      ******************************************************************
      *
      *     C O P Y - L I B R A R Y- W O R K E R S
      *
      ******************************************************************
       COPY "LIBLP/LPWKPATH.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBWI/BHSCAN_CMD.CPY".

       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       01  BHSCAN-WINDOW-HANDLE    PIC X(10).
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/FLDSELW.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBWI/BHSCAN_DEF.CPY".
       COPY "LIBWI/BHSCAN_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".

       COPY "LIBGB/GETFMW.CPY".
       COPY "LIBWI/BHSCANW.CPY".

       SCREEN SECTION.
       COPY "LIBWI/BHSCAN_SCN.CPY".

       PROCEDURE DIVISION
           USING FORM-PATHNAME BHSCAN-BUF EXIT-PATHNAME.

      *****************************************************************
       MAIN-PROGRAM SECTION.
      *****************************************************************

           PERFORM SQL-CONNECT.

       COPY "LIBGB/CHKSEC.CPY".

           MOVE " "      TO VDU-WIMENU-SELECTION.
           MOVE "XX"     TO BHSCAN-STATUS.

           MOVE "BHSCAN" TO VDU-FORM-NAME.

           DISPLAY FLOATING GRAPHICAL WINDOW
                   AT LINE BHSCAN-BEGIN-Y
                   AT COL  BHSCAN-BEGIN-X
                   SIZE    BHSCAN-COLUMNS + VDU-WIN-W-MOD
                   LINES   BHSCAN-LINES + VDU-WIN-H-MOD
           COLOR IS VDU-WCOLOR-SCAN,CONTROL VALUE IS VDU-WCTRL-SCAN
                   CELL SIZE IS LABEL FONT
                   BLANK SCREEN
                   HANDLE  IS BHSCAN-WINDOW-HANDLE.

           PERFORM FORM-RESET.

           PERFORM OPEN-BH1-FILE.
           GO TO BEGIN-SCAN.

       ENTER-DATE.

           MOVE LEGEND-01 TO MESS.
           PERFORM SEND-LEGEND.

           MOVE "ERNM060T-DATE." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "16"
              MOVE 0 TO SCAN-DATE
              GO TO BEGIN-SCAN.

           IF VDUSTATUS = "1>"
              GO TO END-ROUTINE.

           IF VDUSTATUS = "1U"
              GO TO ENTER-DATE.

           IF VDUSTATUS NOT = "00"
              GO TO ENTER-DATE.

           MOVE VDU-DATE-YYYYMMDD TO SCAN-DATE.

       BEGIN-SCAN.
           MOVE FIRST-FIELD TO FLDSEL-FIELD.
           MOVE 1           TO PAGE-COUNT ONE.
           MOVE 0           TO CURRENT-PAGE.

           MOVE BHSCAN-USERID TO BH-USERID.

           IF ( SCAN-DATE NOT = 99999999 )
              MOVE SCAN-DATE TO BH-DATE
           ELSE
              MOVE ZEROES    TO BH-DATE.

           MOVE ZEROES    TO BH-TIME.

           MOVE BH1-KEY     TO PAGE-KEY(1).
      D    STOP MESS.

      *----------------------------------------------------------------
       START-BUILD-PAGENO.
           ADD ONE TO CURRENT-PAGE.

           IF CURRENT-PAGE < 1
              MOVE FIRST-FIELD TO FLDSEL-FIELD
              MOVE 1           TO CURRENT-PAGE.

           IF CURRENT-PAGE > PAGE-COUNT
              MOVE FIRST-FIELD TO FLDSEL-FIELD
              MOVE 1           TO CURRENT-PAGE.

           MOVE SPACES                 TO DISPLAY-BUF.
           MOVE 1                      TO SUB.
           MOVE PAGE-KEY(CURRENT-PAGE) TO BH1-KEY.

           PERFORM START-BH1-FILE.

       BUILD-PAGENO.
           PERFORM NEXT-VALID-HISTORY-FILE.
           IF IO-FG NOT = 0 OR SUB > 9
              GO TO DISPLAY-PAGE.

           MOVE BH1-KEY      TO SELECT-KEY(SUB).

           COMPUTE DATE-YYYYMMDD = 99999999 - BH-DATE.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO D-DATE(SUB).
           PERFORM CONVERT-TIME.
           MOVE TIME-EDIT  TO D-TIME(SUB).
           MOVE BH-GROUP   TO D-GROUP(SUB).
           MOVE BH-ACTION  TO D-ACTION(SUB).

           ADD 1 TO SUB.
           GO TO BUILD-PAGENO.

       DISPLAY-PAGE.
           IF IO-FG NOT = 0
              IF SUB = 1
                 MOVE "NO RECORDS FOUND!" TO MESS
                 PERFORM SEND-LEGEND
                 MOVE DISPLAY-BUF TO WR-BUF
                 MOVE DISPLAY-LIST TO WR-LIST
                 PERFORM CALL-WRFORM
                 GO TO ENTER-DATE.

           MOVE "MORE..." TO D-END-MORE.

           IF CURRENT-PAGE = PAGE-COUNT
              IF IO-FG = 0
                 IF PAGE-COUNT < MAX-PAGES
                    ADD 1 TO PAGE-COUNT
                    MOVE BH1-KEY TO PAGE-KEY(PAGE-COUNT)
                 ELSE
                    MOVE "MAXIMUM PAGES REACHED!" TO MESS
                    PERFORM SEND-LEGEND
                    MOVE "OVRFLOW" TO D-END-MORE
              ELSE
                 MOVE "* END *" TO D-END-MORE.

           MOVE CURRENT-PAGE TO D-PAGENO.

           MOVE DISPLAY-BUF TO WR-BUF.
           MOVE DISPLAY-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      *----------------------------------------------------------------
      *    SELECT FIELD FROM PAGES
      *----------------------------------------------------------------
       SELECT-FIELD-FROM-DISPLAY.
           MOVE LEGEND-02 TO MESS.
           PERFORM SEND-LEGEND.

           PERFORM CALL-FLDSEL.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      * TEST UP ARROW:
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF FLDSEL-STAT = "1U"
              IF CURRENT-PAGE = 1
                 PERFORM ALARM
                 GO TO SELECT-FIELD-FROM-DISPLAY
              ELSE
                 MOVE -1 TO ONE
                 MOVE LAST-FIELD TO FLDSEL-FIELD
                 GO TO START-BUILD-PAGENO.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      * TEST PAGE UP:
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF FLDSEL-STAT = "1P"
              IF CURRENT-PAGE = 1
                 PERFORM ALARM
                 GO TO SELECT-FIELD-FROM-DISPLAY
              ELSE
                 MOVE -1 TO ONE
                 MOVE FIRST-FIELD TO FLDSEL-FIELD
                 GO TO START-BUILD-PAGENO.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      * TEST PAGE DOWN OR DOWN ARROW:
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF FLDSEL-STAT = "1N" OR FLDSEL-STAT = "1D"
              IF CURRENT-PAGE NOT < PAGE-COUNT
                 PERFORM ALARM
                 GO TO SELECT-FIELD-FROM-DISPLAY
              ELSE
                 MOVE 1 TO ONE
                 MOVE FIRST-FIELD TO FLDSEL-FIELD
                 GO TO START-BUILD-PAGENO.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      * TEST HOME:
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF FLDSEL-STAT = "1H"
              MOVE 0 TO CURRENT-PAGE
              MOVE 1 TO ONE
              MOVE FIRST-FIELD TO FLDSEL-FIELD
              GO TO START-BUILD-PAGENO.

           IF FLDSEL-STAT = "1>"
              GO TO END-ROUTINE.

           IF FLDSEL-STAT = "10"
              GO TO ENTER-DATE.

           IF NOT (FLDSEL-STAT = "00" OR "1<")
              PERFORM ALARM
              GO TO SELECT-FIELD-FROM-DISPLAY.

           IF (FLDSEL-STAT = "00")
              MOVE SELECT-KEY(KEY-SUB) TO BHSCAN-BH-KEY
              MOVE SCAN-USERID         TO BHSCAN-USERID
              MOVE "00"                TO BHSCAN-STATUS
              GO TO END-ROUTINE.

           GO TO SELECT-FIELD-FROM-DISPLAY.

      *****************************************************************
       NEXT-VALID-HISTORY-FILE SECTION.
      *****************************************************************
           PERFORM READ-BH1-FILE-NEXT.
           IF ( IO-FG = 9 )
              GO TO EXIT-VALID-BH.

           IF ( BH-USERID NOT = BHSCAN-USERID )
              MOVE 9 TO IO-FG
              GO TO EXIT-VALID-BH.

       EXIT-VALID-BH.
           EXIT.

      ******************************************************************
       CONVERT-TIME SECTION.
      ******************************************************************
           MOVE BH-TIME TO H-AND-M.
           MOVE 0 TO CUR-SS CUR-HD.
           IF H-AND-M NOT GREATER THAN 1159
              MOVE AM-CMD TO T-AMPM
           ELSE
              MOVE PM-CMD TO T-AMPM.
           IF H-AND-M IS NOT LESS THAN 1300
              SUBTRACT 1200 FROM H-AND-M.
           IF CUR-HH = 0
              MOVE 12 TO CUR-HH.
           MOVE CUR-HH TO T-HH.
           MOVE CUR-MM TO T-MM.
           MOVE ":" TO T-COL.
           MOVE " " TO T-SPACE.

      *****************************************************************
      *
      *     C O P Y - L I B R A R Y - S E C T I O N S
      *
      *****************************************************************
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/FLDSEL.CPY".

      *************************************************
       FORM-RESET SECTION.
      *************************************************
           DISPLAY BHSCAN-SCREEN UPON BHSCAN-WINDOW-HANDLE.
           MOVE BHSCAN-FORM-FIELDS TO WR-BUF.
           MOVE BHSCAN-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      *************************************************
       SETUP-LINK-INFO SECTION.
      *************************************************
           MOVE BHSCAN-HELP--FILE TO HELP-LINK-FILE.
           MOVE BHSCAN-HELP--DIR  TO HELP-LINK-DIR.

      *************************************************
       VDU-CONVERT-FIELD SECTION.
      *************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBWI/BHSCAN_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      *****************************************************************
       MISC-PARAGRAPHS SECTION.
      *****************************************************************
       COPY "LIBGB/ACCESS.CPY".

       SEND-LEGEND.
           MOVE "S  A600WSEL:D." TO VDU.
           MOVE MESS TO VDUBUF.
           PERFORM SCREENIO.

      *****************************************************************
       IO-ROUTINES SECTION.
      *****************************************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/DECLARE_SQL.CPY".
           PERFORM CLOSE-BH1-FILE.
       COPY "LIBSP/SPBHGS_SQL.CPY".
       COPY "LIBSP/SPBH1RN.CPY".
       COPY "LIBGB/FERRORS.CPY".

      *****************************************************************
       END-ROUTINE SECTION.
      *****************************************************************
           PERFORM CLOSE-FILES.

       EXIT-PROG.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY BHSCAN-SCREEN.
           DESTROY BHSCAN-WINDOW-HANDLE.

       STOP-RUN.
           EXIT PROGRAM.
