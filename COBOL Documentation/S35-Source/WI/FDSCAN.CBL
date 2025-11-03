       IDENTIFICATION DIVISION.
       PROGRAM-ID.      FDSCAN.
      *****************************************************
      *  FORM DESCRIPTOR FILE WINDOW SCAN
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
           "@(#)WI/FDSCAN A30 07/08/14-11:06:58 10/01/14-09:55:49 >".
       COPY "LIBLP/LP01FD.CPY".
       COPY "LIBLP/LP01FD_SQL.CPY".
       COPY "LIBLP/LPWSFD.CPY".

       01  FDSCAN-LINES          PIC 99    VALUE 15.
       01  FDSCAN-COLUMNS        PIC 99    VALUE 39.
       01  FDSCAN-BEGIN-Y        PIC 99    VALUE 8.
       01  FDSCAN-BEGIN-X        PIC 99    VALUE 41.

       01  ERRCD                   PIC X      VALUE SPACES.
       01  ONE                     PIC S9     VALUE 1.
       01  SUB                     PIC 99.
       01  SAVE-FMTYPE             PIC XX.

       01  FLDSEL-FIELD            PIC X(4)   VALUE "010.".
       01  FLDSEL-STAT             PIC XX.
       01  FLDSEL-LIST.
           03  FILLER              PIC X(36)  VALUE
           "010 020 030 040 050 060 070 080 090.".
       01  FLDSEL-BUF.
           05  FLDSEL-KEY          PIC 9(3).
           05  FILLER              PIC XX.
           05  FLDSEL-NAME         PIC X(30).

       01  FIRST-FIELD             PIC X(4)  VALUE "010.".
       01  LAST-FIELD              PIC X(4)  VALUE "090.".

       01  DISPLAY-LIST.
           03  FILLER              PIC X(51)  VALUE
           "010 020 030 040 050 060 070 080 090 PGNO ENDMORE:F.".
       01  DISPLAY-BUF.
           03  D-LINE              OCCURS 9 TIMES.
               05  D-KEY           PIC 9(3).
               05  FILLER          PIC XX.
               05  D-NAME          PIC X(30).
           03  D-PAGENO            PIC ZZ9.
           03  D-END-MORE          PIC X(7).

       01  CURRENT-PAGE            PIC S999.
       01  PAGE-COUNT              PIC 999.
       01  MAX-PAGES               PIC 999    VALUE 50.
       01  PAGE-KEY-01.
           03  PAGE-KEY                OCCURS 50 TIMES
                                       PIC 999.

      *01  MORE-LIST.
      *    03  FILLER     PIC X(39)  VALUE
      *    "TITLE ADR1 ADR2 ADR3 PHONE EMPL SPOUSE ".
      *    03  FILLER     PIC X(16)  VALUE
      *    "LIA OPEN CLOSED.".
      *01  MORE-BUF.
      *    03  M-NAME    PIC X(30).
      *    03  M-ADR1    PIC X(30).
      *    03  M-ADR2    PIC X(30).
      *    03  M-ADR3    PIC X(30).
      *    03  M-PHONE   PIC X(14).
      *    03  M-EMPL    PIC X(20).
      *    03  M-SPOUSE  PIC X(28).
      *    03  M-LIA     PIC Z,ZZZ,ZZ9.99.
      *    03  M-OPEN    PIC ZZ.
      *    03  M-CLOSED  PIC ZZ.


       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       01  FDSCAN-WINDOW-HANDLE PIC X(10).
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBWI/FDSCAN_DEF.CPY".
       COPY "LIBWI/FDSCAN_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/FLDSELW.CPY".

       COPY "LIBGB/GETFMW.CPY".
       COPY "LIBWI/FDSCANW.CPY".

       SCREEN SECTION.
       COPY "LIBWI/FDSCAN_SCN.CPY".

       PROCEDURE DIVISION
           USING FORM-PATHNAME FDSCAN-BUF EXIT-PATHNAME.

      ***************************
      *      MAIN PROGRAM MODULE
      ***************************
       MAIN-PROGRAM SECTION.

           PERFORM SQL-CONNECT.

       COPY "LIBGB/CHKSEC.CPY".

           MOVE "XX"          TO FDSCAN-STATUS.
           MOVE FDSCAN-FMTYPE TO SAVE-FMTYPE.
           MOVE " "           TO VDU-WIMENU-SELECTION.
           MOVE "FDSCAN"      TO VDU-FORM-NAME.

           DISPLAY FLOATING GRAPHICAL WINDOW
                   AT LINE FDSCAN-BEGIN-Y
                   AT COL  FDSCAN-BEGIN-X
                   SIZE    FDSCAN-COLUMNS + VDU-WIN-W-MOD
                   LINES   FDSCAN-LINES + VDU-WIN-H-MOD
           COLOR IS VDU-WCOLOR-SCAN,CONTROL VALUE IS VDU-WCTRL-SCAN
                   CELL SIZE IS LABEL FONT
                   BLANK SCREEN
                   HANDLE  IS FDSCAN-WINDOW-HANDLE.

           PERFORM FORM-RESET.

       MAIN-MODULE.

           IF FDSCAN-ALLOW-CHG NOT = " "
              MOVE " F7, F3-TOGGLE, SELECT FORM:" TO MESS
           ELSE
              MOVE " F7-EXIT, SELECT A FORM:" TO MESS.
           PERFORM SEND-LEGEND.
           PERFORM SEND-TITLE.

       BEGIN-BUILD-PAGENO.
           MOVE FIRST-FIELD TO FLDSEL-FIELD.
           MOVE 1     TO PAGE-COUNT ONE.
           MOVE 0     TO CURRENT-PAGE.
           MOVE 0     TO PAGE-KEY-01.
           PERFORM OPEN-FD1-FILE.
           MOVE 0     TO PAGE-KEY(PAGE-COUNT).
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
           MOVE PAGE-KEY(CURRENT-PAGE) TO FD-FMNO.
           PERFORM START-FD1-FILE.

      *************************************
      *    BUILD PAGE
      *************************************
       BUILD-PAGENO.
           PERFORM READ-FD1-FILE-NEXT.
           IF IO-FG NOT = 0 OR SUB > 9
              GO TO DISPLAY-PAGE.
      * LOANS ONLY, NOTES ARE GONE
           IF FD-TYPE NOT = "L"
              GO TO BUILD-PAGENO.
           IF FD-FMTYPE NOT = SAVE-FMTYPE
              GO TO BUILD-PAGENO.
           MOVE FD-FMNO TO D-KEY(SUB)
           MOVE FD-DESC TO D-NAME(SUB).
           ADD 1 TO SUB.
           GO TO BUILD-PAGENO.

      *************************************
      *    DISPLAY PAGE
      *************************************
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
                    MOVE FD-FMNO TO PAGE-KEY(PAGE-COUNT)
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

      *************************************
      *    SELECT FIELD FROM PAGES
      *************************************
       SELECT-FIELD-FROM-DISPLAY.
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
           IF FLDSEL-STAT = "16"
              IF FDSCAN-ALLOW-CHG NOT = " "
                 IF FDSCAN-FMTYPE = SAVE-FMTYPE
                    MOVE FDSCAN-ALLOW-CHG TO SAVE-FMTYPE
                    GO TO MAIN-MODULE
                 ELSE
                    MOVE FDSCAN-FMTYPE TO SAVE-FMTYPE
                    GO TO MAIN-MODULE.
           IF NOT (FLDSEL-STAT = "00")
              PERFORM ALARM
              GO TO SELECT-FIELD-FROM-DISPLAY.

           MOVE FLDSEL-KEY TO FDSCAN-KEY.
           MOVE "00"       TO FDSCAN-STATUS.

           GO TO END-ROUTINE.

       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/FLDSEL.CPY".
       COPY "LIBGB/DATER.CPY".

      *************************************************
       FORM-RESET SECTION.
      *************************************************
           DISPLAY FDSCAN-SCREEN UPON FDSCAN-WINDOW-HANDLE.
           MOVE FDSCAN-FORM-FIELDS TO WR-BUF.
           MOVE FDSCAN-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      *************************************************
       SETUP-LINK-INFO SECTION.
      *************************************************
           MOVE FDSCAN-HELP--FILE TO HELP-LINK-FILE.
           MOVE FDSCAN-HELP--DIR  TO HELP-LINK-DIR.

      *************************************************
       VDU-CONVERT-FIELD SECTION.
      *************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBWI/FDSCAN_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ************************************************
       MISC-PARAGRAPHS SECTION.
      ************************************************
       COPY "LIBGB/ACCESS.CPY".
       SEND-TITLE.
           MOVE "S  A300TITLE." TO VDU.
           IF SAVE-FMTYPE = "S"
              MOVE " SOLICITATION FORM SELECTION" TO VDUBUF
           ELSE
              IF SAVE-FMTYPE = "C"
                 MOVE "  COLLECTION FORM SELECTION" TO VDUBUF
              ELSE
                 MOVE "    GENERAL FORM SELECTION" TO VDUBUF.
           PERFORM SCREENIO.

       SEND-LEGEND.
           MOVE "S  A600WSEL:D." TO VDU.
           MOVE MESS TO VDUBUF.
           PERFORM SCREENIO.


      ********************************************
       IO-ROUTINES SECTION.
      ********************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/DECLARE_SQL.CPY".
           PERFORM CLOSE-FD1-FILE.
       COPY "LIBLP/LPFDGS_SQL.CPY".
       COPY "LIBLP/LPFD1RN.CPY".

       COPY "LIBGB/FERRORS.CPY".

      ********************************************
       END-ROUTINE SECTION.
      ********************************************
           PERFORM CLOSE-FILES.
       EXIT-PROG.
       STOP-RUN.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY FDSCAN-SCREEN.
           DESTROY FDSCAN-WINDOW-HANDLE.
           EXIT PROGRAM.

