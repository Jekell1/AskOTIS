       IDENTIFICATION DIVISION.
       PROGRAM-ID.      ICSCAN.
       DATE-WRITTEN.    052899.
      ******************************************************************
      *
      *                   INSURANCE COMPANY WINDOW SCAN
      *
      * REV.
      * KEC 2018-0919 ***NEW*** LOCAL TO GLOBAL
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       I-O-CONTROL.
       DATA DIVISION.
       FILE SECTION.

      ******************************************************************
      *
      *     W O R K I N G - S T O R A G E - S E C T I O N
      *
      ******************************************************************
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)WI/ICSCAN.CBL S35 05/03/24-09:55:45 >".

      ******************************************************************
      *    W O R K E R S
      ******************************************************************
       COPY "LIBLP/LP01IC.CPY".
       COPY "LIBLP/LP01IC_SQL.CPY".
       COPY "LIBLP/LPWSIC.CPY".

       01  ICSCAN-LINES          PIC 99    VALUE 15.
       01  ICSCAN-COLUMNS        PIC 99    VALUE 39.
       01  ICSCAN-BEGIN-Y        PIC 99    VALUE 6.
       01  ICSCAN-BEGIN-X        PIC 99    VALUE 2.
       01  ERRCD                   PIC X      VALUE SPACES.
       01  ONE                     PIC S9     VALUE 1.
       01  SUB                     PIC 99.

      ******************************************************************
      *
      *    F L D S E L
      *
      ******************************************************************
       01  FLDSEL-FIELD                        PIC X(4)   VALUE "010.".
       01  FLDSEL-STAT                         PIC XX.
       01  FLDSEL-LIST.
           03  FILLER                          PIC X(36)  VALUE
           "010 020 030 040 050 060 070 080 090.".

       01  FLDSEL-BUF.
           05  FLDSEL-KEY                      PIC X(2).
           05  FILLER                          PIC X(2).
           05  FLDSEL-NAME                     PIC X(31).

       01  FIRST-FIELD                         PIC X(4)  VALUE "010.".
       01  LAST-FIELD                          PIC X(4)  VALUE "090.".

      ******************************************************************
      *
      *    D I S P L A Y
      *
      ******************************************************************
       01  DISPLAY-LIST.
           03  FILLER                        PIC X(51)  VALUE
           "010 020 030 040 050 060 070 080 090 PGNO ENDMORE:F.".

      *-----------------------------------------------------------------
       01  DISPLAY-BUF.
           03  D-LINE                        OCCURS 9 TIMES.
               05  D-KEY                     PIC X(2).
               05  FILLER                    PIC XX.
               05  D-NAME                    PIC X(31).
           03  D-PAGENO                      PIC ZZ9.
           03  D-END-MORE                    PIC X(7).

       01  CURRENT-PAGE                      PIC S999.
       01  PAGE-COUNT                        PIC 999.
       01  MAX-PAGES                         PIC 999    VALUE 50.
       01  MAX-LINES                         PIC 9(3)   VALUE 09.

       01  PAGE-KEY-01.
           03  PAGE-KEY-WORK       OCCURS 50 TIMES.
               05  PAGE-KEY        PIC 99.

      ******************************************************************
      *
      *     C O P Y - L I B R A R Y - W O R K E R S
      *
      ******************************************************************
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/FLDSELW.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBWI/ICSCAN_DEF.CPY".
       COPY "LIBWI/ICSCAN_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       01  ICSCAN-WINDOW-HANDLE    PIC X(10).

       COPY "LIBGB/GETFMW.CPY".
       COPY "LIBWI/ICSCANW.CPY".

       SCREEN SECTION.
       COPY "LIBWI/ICSCAN_SCN.CPY".

      ******************************************************************
      *
      *    P R O C E D U R E - D I V I S I O N
      *
      ******************************************************************
       PROCEDURE DIVISION
           USING FORM-PATHNAME ICSCAN-BUF EXIT-PATHNAME.

      ******************************************************************
      *
      *    M A I N - P R O G R A M
      *
      ******************************************************************
       MAIN-PROGRAM SECTION.

           PERFORM SQL-CONNECT.

       COPY "LIBGB/CHKSEC.CPY".

           MOVE "XX"     TO ICSCAN-STATUS.
           MOVE " " TO VDU-WIMENU-SELECTION.

           MOVE "ICSCAN" TO VDU-FORM-NAME.

           DISPLAY FLOATING GRAPHICAL WINDOW
                   AT LINE ICSCAN-BEGIN-Y
                   AT COL  ICSCAN-BEGIN-X
                   SIZE    ICSCAN-COLUMNS + VDU-WIN-W-MOD
                   LINES   ICSCAN-LINES + VDU-WIN-H-MOD
           COLOR IS VDU-WCOLOR-SCAN,CONTROL VALUE IS VDU-WCTRL-SCAN
                   CELL SIZE IS LABEL FONT
                   BLANK SCREEN
                   HANDLE  IS ICSCAN-WINDOW-HANDLE.

           PERFORM FORM-RESET.

           MOVE "S  A300TITLE." TO VDU.
           MOVE " INSURANCE COMPANY SELECTION " TO VDUBUF.
           PERFORM SCREENIO.

       BEGIN-BUILD-PAGENO.

           MOVE FIRST-FIELD TO FLDSEL-FIELD.
           MOVE 1           TO PAGE-COUNT ONE.
           MOVE 0           TO CURRENT-PAGE.

           PERFORM OPEN-IC1-FILE.
           MOVE 0           TO PAGE-KEY(PAGE-COUNT).

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

           MOVE PAGE-KEY(CURRENT-PAGE) TO IC-CODE.

           PERFORM START-IC1-FILE.

       BUILD-PAGENO.
           PERFORM READ-IC1-FILE-NEXT.
           IF ( IO-BAD ) OR ( IC-BRNO NOT = IC-PATH-OWNBR )
              MOVE 9 TO IO-FG
              GO TO DISPLAY-PAGE.

           IF SUB > MAX-LINES
              GO TO DISPLAY-PAGE.

           MOVE IC-CODE TO D-KEY(SUB)
           MOVE IC-NAME TO D-NAME(SUB).

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

           IF ( CURRENT-PAGE = PAGE-COUNT )
              IF IO-FG = 0
                 IF ( PAGE-COUNT < MAX-PAGES )
                    ADD 1 TO PAGE-COUNT
                    MOVE IC-CODE TO PAGE-KEY(PAGE-COUNT)
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

           MOVE FLDSEL-KEY TO ICSCAN-CODE.
           MOVE "00"       TO ICSCAN-STATUS.

           IF FLDSEL-STAT = "1<"
              GO TO SELECT-FIELD-FROM-DISPLAY.

           GO TO END-ROUTINE.

       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/FLDSEL.CPY".
       COPY "LIBGB/DATER.CPY".

      *************************************************
       FORM-RESET SECTION.
      *************************************************
           DISPLAY ICSCAN-SCREEN UPON ICSCAN-WINDOW-HANDLE.
           MOVE ICSCAN-FORM-FIELDS TO WR-BUF.
           MOVE ICSCAN-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      *************************************************
       SETUP-LINK-INFO SECTION.
      *************************************************
           MOVE ICSCAN-HELP--FILE TO HELP-LINK-FILE.
           MOVE ICSCAN-HELP--DIR  TO HELP-LINK-DIR.

      *************************************************
       VDU-CONVERT-FIELD SECTION.
      *************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBWI/ICSCAN_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ******************************************************************
       MISC-PARAGRAPHS SECTION.
      ******************************************************************
       COPY "LIBGB/ACCESS.CPY".
       SEND-LEGEND.
           MOVE "S  A600WSEL:D." TO VDU.
           MOVE MESS TO VDUBUF.
           PERFORM SCREENIO.

      ******************************************************************
       IO-ROUTINES SECTION.
      ******************************************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/DECLARE_SQL.CPY".
           PERFORM CLOSE-IC1-FILE.
       COPY "LIBLP/LPICGS_SQL.CPY".
       COPY "LIBLP/LPIC1RN.CPY".
       COPY "LIBGB/FERRORS.CPY".

      ******************************************************************
       END-ROUTINE SECTION.
      ******************************************************************

           PERFORM CLOSE-FILES.

       EXIT-PROG.

       STOP-RUN.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY ICSCAN-SCREEN.
           DESTROY ICSCAN-WINDOW-HANDLE.
           EXIT PROGRAM.
