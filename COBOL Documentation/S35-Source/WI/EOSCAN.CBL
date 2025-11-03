       IDENTIFICATION DIVISION.
       PROGRAM-ID.      EOSCAN.
      *****************************************************
      *  BATCH PROCEDURE FILE WINDOW SCAN
      *   CALLED FROM : MN/EOMAIN, SP/BREXEC, SP/EODEL, SP/EOEXEC
      *                 SP/EOLIST, SP/GREXEC
      *****************************************************
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
       77  SCCS-IDENTIFIER PIC X(55) VALUE
           "@(#)WI/EOSCAN A30 07/07/14-15:47:15 10/01/14-09:55:56 >".
       COPY "LIBGB/GB01EO.CPY".
       COPY "LIBGB/GB01EO_SQL.CPY".
       COPY "LIBGB/GBWSEO.CPY".

       01  EOSCAN-LINES          PIC 99    VALUE 16.
       01  EOSCAN-COLUMNS        PIC 99    VALUE 40.
       01  EOSCAN-BEGIN-Y        PIC 99    VALUE 3.
       01  EOSCAN-BEGIN-X        PIC 99    VALUE 40.
       01  ERRCD                   PIC X      VALUE SPACES.
       01  ONE                     PIC S9     VALUE 1.
       01  SUB                     PIC 99.

       01  FLDSEL-FIELD            PIC X(4)   VALUE "010.".
       01  FLDSEL-STAT             PIC XX.
       01  FLDSEL-LIST.
           03  FILLER              PIC X(36)  VALUE
           "010 020 030 040 050 060 070 080 090.".
       01  FLDSEL-BUF.
           05  FLDSEL-KEY          PIC X(3).
           05  FILLER              PIC XX.
           05  FLDSEL-NAME         PIC X(30).

       01  FIRST-FIELD             PIC X(4)  VALUE "010.".
       01  LAST-FIELD              PIC X(4)  VALUE "090.".

       01  DISPLAY-LIST.
           03  FILLER              PIC X(51)  VALUE
           "010 020 030 040 050 060 070 080 090 PGNO ENDMORE:F.".
       01  DISPLAY-BUF.
           03  D-LINE              OCCURS 9 TIMES.
               05  D-KEY           PIC X(3).
               05  FILLER          PIC XXX.
               05  D-COMMENT       PIC X(30).
           03  D-PAGENO            PIC ZZ9.
           03  D-END-MORE          PIC X(7).

       01  CURRENT-PAGE            PIC S999.
       01  PAGE-COUNT              PIC 999.
       01  MAX-PAGES               PIC 999    VALUE 100.
       01  PAGE-KEY-01.
           03  PAGE-KEY-WORK       OCCURS 100 TIMES.
               05  PAGE-KEY        PIC XXX.


       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/ACCESSW.CPY".

       01 EOSCAN-WINDOW-HANDLE PIC X(10).
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/FLDSELW.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBWI/EOSCAN_DEF.CPY".
       COPY "LIBWI/EOSCAN_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".

       COPY "LIBGB/GETFMW.CPY".
       COPY "LIBWI/EOSCANW.CPY".

       SCREEN SECTION.
       COPY "LIBWI/EOSCAN_SCN.CPY".

       PROCEDURE DIVISION
           USING FORM-PATHNAME EOSCAN-BUF EXIT-PATHNAME.

      ********************************
       MAIN-PROGRAM SECTION.
      ********************************

           PERFORM SQL-CONNECT.

       COPY "LIBGB/CHKSEC.CPY".
           MOVE "XX" TO EOSCAN-STATUS.
           MOVE " " TO VDU-WIMENU-SELECTION.

           MOVE "EOSCAN" TO VDU-FORM-NAME.

           DISPLAY FLOATING GRAPHICAL WINDOW
                   AT LINE EOSCAN-BEGIN-Y
                   AT COL  EOSCAN-BEGIN-X
                   SIZE    EOSCAN-COLUMNS + VDU-WIN-W-MOD
                   LINES   EOSCAN-LINES + VDU-WIN-H-MOD
           COLOR IS VDU-WCOLOR-SCAN,CONTROL VALUE IS VDU-WCTRL-SCAN
                   CELL SIZE IS LABEL FONT
                   BLANK SCREEN
                   HANDLE  IS EOSCAN-WINDOW-HANDLE.

           PERFORM FORM-RESET.

           MOVE "S  A300TITLE." TO VDU.
           MOVE "  BATCH PROCEDURE SELECTION" TO VDUBUF.
           PERFORM SCREENIO.

       BEGIN-BUILD-PAGENO.
           MOVE FIRST-FIELD TO FLDSEL-FIELD.
           MOVE 1           TO PAGE-COUNT ONE.
           MOVE 0           TO CURRENT-PAGE.

           PERFORM OPEN-EO1-FILE.
           MOVE SPACES      TO PAGE-KEY(PAGE-COUNT).
       START-BUILD-PAGENO.
           ADD ONE TO CURRENT-PAGE.
           IF CURRENT-PAGE < 1
              MOVE FIRST-FIELD TO FLDSEL-FIELD
              MOVE 1 TO CURRENT-PAGE.
           IF CURRENT-PAGE > PAGE-COUNT
              MOVE FIRST-FIELD TO FLDSEL-FIELD
              MOVE 1 TO CURRENT-PAGE.

           MOVE SPACES TO DISPLAY-BUF.
           MOVE 1 TO SUB.
           MOVE PAGE-KEY(CURRENT-PAGE) TO EO-PCODE.
           MOVE 0 TO EO-STEP.
           PERFORM START-EO1-FILE.

       BUILD-PAGENO.
           PERFORM READ-EO1-FILE-NEXT.
           IF IO-FG NOT = 0 OR SUB > 9
              GO TO DISPLAY-PAGE.
           IF EO-STEP NOT = 0
              GO TO BUILD-PAGENO.
           MOVE EO-PCODE TO D-KEY(SUB).
           MOVE EO-COMMENT TO D-COMMENT(SUB).
           ADD 1 TO SUB.
           GO TO BUILD-PAGENO.

       DISPLAY-PAGE.
           IF IO-FG NOT = 0
              IF SUB = 1
                 MOVE DISPLAY-BUF TO WR-BUF
                 MOVE DISPLAY-LIST TO WR-LIST
                 PERFORM CALL-WRFORM
                 MOVE "NO RECORDS FOUND!" TO MESS
                 PERFORM SEND-MESS
                 GO TO END-ROUTINE.

           MOVE "MORE..." TO D-END-MORE.
           IF CURRENT-PAGE = PAGE-COUNT
              IF IO-FG = 0
                 IF PAGE-COUNT < MAX-PAGES
                    ADD 1 TO PAGE-COUNT
                    MOVE EO-PCODE TO PAGE-KEY(PAGE-COUNT)
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

       SELECT-FIELD-FROM-DISPLAY.
           MOVE "F7 EXIT, SELECT CODE(RTN):" TO MESS.
           PERFORM SEND-LEGEND.
           PERFORM CALL-FLDSEL.

      * TEST UP ARROW:
           IF FLDSEL-STAT = "1U"
              IF CURRENT-PAGE = 1
                 PERFORM ALARM
                 GO TO SELECT-FIELD-FROM-DISPLAY
              ELSE
                 MOVE -1 TO ONE
                 MOVE LAST-FIELD TO FLDSEL-FIELD
                 GO TO START-BUILD-PAGENO.

      * TEST PAGE UP:
           IF FLDSEL-STAT = "1P"
              IF CURRENT-PAGE = 1
                 PERFORM ALARM
                 GO TO SELECT-FIELD-FROM-DISPLAY
              ELSE
                 MOVE -1 TO ONE
                 MOVE FIRST-FIELD TO FLDSEL-FIELD
                 GO TO START-BUILD-PAGENO.

      * TEST PAGE DOWN OR DOWN ARROW:
           IF FLDSEL-STAT = "1N" OR FLDSEL-STAT = "1D"
              IF CURRENT-PAGE NOT < PAGE-COUNT
                 PERFORM ALARM
                 GO TO SELECT-FIELD-FROM-DISPLAY
              ELSE
                 MOVE 1 TO ONE
                 MOVE FIRST-FIELD TO FLDSEL-FIELD
                 GO TO START-BUILD-PAGENO.

      * TEST HOME:
           IF FLDSEL-STAT = "1H"
              MOVE 0 TO CURRENT-PAGE
              MOVE 1 TO ONE
              MOVE FIRST-FIELD TO FLDSEL-FIELD
              GO TO START-BUILD-PAGENO.

           IF FLDSEL-STAT = "1>"
              GO TO END-ROUTINE.
           IF NOT (FLDSEL-STAT = "00" OR "1<")
              PERFORM ALARM
              GO TO SELECT-FIELD-FROM-DISPLAY.

           MOVE FLDSEL-KEY TO EOSCAN-KEY.
           MOVE "00"       TO EOSCAN-STATUS.

           GO TO END-ROUTINE.

       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/FLDSEL.CPY".
       COPY "LIBGB/DATER.CPY".

      *************************************************
       FORM-RESET SECTION.
      *************************************************
           DISPLAY EOSCAN-SCREEN UPON EOSCAN-WINDOW-HANDLE.
           MOVE EOSCAN-FORM-FIELDS TO WR-BUF.
           MOVE EOSCAN-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      *************************************************
       SETUP-LINK-INFO SECTION.
      *************************************************
           MOVE EOSCAN-HELP--FILE TO HELP-LINK-FILE.
           MOVE EOSCAN-HELP--DIR  TO HELP-LINK-DIR.

      *************************************************
       VDU-CONVERT-FIELD SECTION.
      *************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBWI/EOSCAN_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      *****************************
       MISC-PARAGRAPHS SECTION.
      *****************************
       COPY "LIBGB/ACCESS.CPY".
       SEND-LEGEND.
           MOVE "S  A600WSEL:D." TO VDU.
           MOVE MESS TO VDUBUF.
           PERFORM SCREENIO.

      *****************************
       IO-ROUTINES SECTION.
      *****************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/DECLARE_SQL.CPY".
           PERFORM CLOSE-EO1-FILE.
       COPY "LIBGB/GBEOGS_SQL.CPY".
       COPY "LIBGB/GBEO1RN.CPY".

       COPY "LIBGB/FERRORS.CPY".

      *****************************
       END-ROUTINE SECTION.
      *****************************
           PERFORM CLOSE-FILES.
       EXIT-PROG.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY EOSCAN-SCREEN.
           DESTROY EOSCAN-WINDOW-HANDLE.

       STOP-RUN.
           EXIT PROGRAM.
