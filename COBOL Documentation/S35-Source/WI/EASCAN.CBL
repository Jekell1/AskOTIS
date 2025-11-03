       IDENTIFICATION DIVISION.
       PROGRAM-ID.      EASCAN.
      ******************************************************************
      *
      *  EXTRACTION ACTION FILE WINDOW SCAN
      *    CALLED FROM MN/EQMAIN
      *
      *=================================================================
      * REV:
      * KEC 2016.0301 {PD#00003} PARADATA <A30> [JAY CISZEWSKI]
      *         FIXED KEYS & RECS FOR EA-REC (HOLD/ORIG/SAVE/WK).
      *         CHANGED CLNO/CLASS 2 TO 3 BYTES.
      *
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


       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(37) VALUE
           "@(#)WI/EASCAN A34 10/20/20-10:17:25 >".
       COPY "LIBLP/LP01EA.CPY".
       COPY "LIBLP/LP01EA_SQL.CPY".
       COPY "LIBLP/LPWSEA.CPY".

       01  EASCAN-LINES              PIC 99  VALUE 15.
       01  EASCAN-COLUMNS            PIC 99  VALUE 78.
       01  EASCAN-BEGIN-Y            PIC 99  VALUE 2.
       01  EASCAN-BEGIN-X            PIC 99  VALUE 2.
       01  ERRCD                   PIC X      VALUE SPACES.
       01  ONE                     PIC S9     VALUE 1.
       01  SUB                     PIC 99.

       01  FLDSEL-FIELD            PIC X(4)   VALUE "010.".
       01  KEY-SUBSCRIPT REDEFINES FLDSEL-FIELD.
           03  KEY-SUB             PIC 99.
           03  FILLER              PIC XX.

       01  FLDSEL-STAT             PIC XX.
       01  FLDSEL-LIST.
           03  FILLER              PIC X(36)  VALUE
           "010 020 030 040 050 060 070 080 090.".
       01  FLDSEL-BUF.
           05  FILLER              PIC X(78).

       01  FIRST-FIELD             PIC X(4)  VALUE "010.".
       01  LAST-FIELD              PIC X(4)  VALUE "090.".

       01  DISPLAY-LIST.
           03  FILLER              PIC X(51)  VALUE
           "010 020 030 040 050 060 070 080 090 PGNO ENDMORE:F.".
       01  DISPLAY-BUF.
           03  D-LINE              OCCURS 9 TIMES.
               05  D-BRANCH        PIC ZZZ9.
               05  FILLER          PIC X.
               05  D-CLASS         PIC 9(2).
               05  FILLER          PIC X.
               05  D-GROUP         PIC X(6).
               05  FILLER          PIC X.
               05  D-SEQ           PIC 9(2).
               05  FILLER          PIC X.
               05  D-DESC          PIC X(60).
           03  D-PAGENO            PIC ZZ9.
           03  D-END-MORE          PIC X(7).

       01  CURRENT-PAGE            PIC S999.
       01  PAGE-COUNT              PIC 999.
       01  MAX-PAGES               PIC 999    VALUE 10.
       01  PAGE-KEY-01.
           03  PAGE-KEY            OCCURS 50 TIMES.
               05  PAGE-KEY-BRNO   PIC 9(4).
               05  PAGE-KEY-CLASS  PIC 9(3).
               05  PAGE-KEY-GROUP  PIC X(6).
               05  PAGE-KEY-SEQ    PIC 99.
       01  KEY-SELECT-TABLE.
           03  SELECT-KEY          PIC X(15) OCCURS 9 TIMES.

       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       01  EASCAN-WINDOW-HANDLE    PIC X(10).
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/FLDSELW.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBWI/EASCAN_DEF.CPY".
       COPY "LIBWI/EASCAN_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".

       COPY "LIBGB/GETFMW.CPY".
       COPY "LIBWI/EASCANW.CPY".

       SCREEN SECTION.
       COPY "LIBWI/EASCAN_SCN.CPY".

       PROCEDURE DIVISION
           USING FORM-PATHNAME EASCAN-BUF EXIT-PATHNAME.

      *    ***************************
      *    *      MAIN PROGRAM MODULE
      *    ***************************
       MAIN-PROGRAM SECTION.

           PERFORM SQL-CONNECT.

       COPY "LIBGB/CHKSEC.CPY".
      D    STOP MESS.
           MOVE " "  TO VDU-WIMENU-SELECTION.
           MOVE "XX" TO EASCAN-STATUS.

           MOVE "EASCAN" TO VDU-FORM-NAME.

           DISPLAY FLOATING GRAPHICAL WINDOW
                   AT LINE EASCAN-BEGIN-Y
                   AT COL  EASCAN-BEGIN-X
                   SIZE    EASCAN-COLUMNS + VDU-WIN-W-MOD
                   LINES   EASCAN-LINES + VDU-WIN-H-MOD
           COLOR IS VDU-WCOLOR-SCAN,CONTROL VALUE IS VDU-WCTRL-SCAN
                   CELL SIZE IS LABEL FONT
                   BLANK SCREEN
                   HANDLE  IS EASCAN-WINDOW-HANDLE.

           PERFORM FORM-RESET.

       BEGIN-BUILD-PAGENO.
           MOVE FIRST-FIELD TO FLDSEL-FIELD.
           MOVE 1 TO PAGE-COUNT ONE.
           MOVE 0 TO CURRENT-PAGE.
           PERFORM OPEN-EA1-FILE.
           IF EASCAN-BRNO NOT = 0
              MOVE EASCAN-BRNO TO PAGE-KEY-BRNO(PAGE-COUNT)
           ELSE
              MOVE 0 TO PAGE-KEY-BRNO(PAGE-COUNT).
           IF EASCAN-CLASS NOT = 0
              MOVE EASCAN-CLASS TO PAGE-KEY-CLASS(PAGE-COUNT)
           ELSE
              MOVE 0 TO PAGE-KEY-CLASS(PAGE-COUNT).
           IF EASCAN-GROUP NOT = SPACES
              MOVE EASCAN-GROUP TO PAGE-KEY-GROUP(PAGE-COUNT)
           ELSE
              MOVE SPACES TO PAGE-KEY-GROUP(PAGE-COUNT).
           MOVE 0 TO PAGE-KEY-SEQ(PAGE-COUNT).
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
           MOVE PAGE-KEY(CURRENT-PAGE) TO EA1-KEY.

           PERFORM START-EA1-FILE.

      *************************************
      *    BUILD PAGE
      *************************************
       BUILD-PAGENO.
           PERFORM READ-EA1-FILE-NEXT.
           IF IO-FG NOT = 0 OR SUB > 9
              GO TO DISPLAY-PAGE.

           IF EASCAN-BRNO NOT = 0
              IF EASCAN-BRNO NOT = EA-BRANCH
                 GO TO DISPLAY-PAGE.

           IF EASCAN-CQ-FG NOT = EA-CQ-FG
              GO TO BUILD-PAGENO.

           IF EASCAN-CLASS NOT = 0
              IF EASCAN-CLASS NOT = EA-CLASS
                 GO TO BUILD-PAGENO.

           IF EASCAN-GROUP NOT = SPACES
              IF EASCAN-GROUP NOT = EA-GROUP-ID
                 GO TO BUILD-PAGENO.

           MOVE EA-BRANCH TO D-BRANCH(SUB).
           MOVE EA-CLASS TO D-CLASS(SUB).
           MOVE EA-GROUP-ID TO D-GROUP(SUB).
           MOVE EA-SEQNO TO D-SEQ(SUB).
           MOVE EA-DESC TO D-DESC(SUB).
           MOVE EA1-KEY TO SELECT-KEY(SUB).
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
                    MOVE EA1-KEY TO PAGE-KEY(PAGE-COUNT)
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

           MOVE SELECT-KEY(KEY-SUB) TO EASCAN-KEY.

           MOVE "00" TO EASCAN-STATUS.

           GO TO END-ROUTINE.

       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/FLDSEL.CPY".
       COPY "LIBGB/DATER.CPY".

      *************************************************
       FORM-RESET SECTION.
      *************************************************
           DISPLAY EASCAN-SCREEN UPON EASCAN-WINDOW-HANDLE.
           MOVE EASCAN-FORM-FIELDS TO WR-BUF.
           MOVE EASCAN-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      *************************************************
       SETUP-LINK-INFO SECTION.
      *************************************************
           MOVE EASCAN-HELP--FILE TO HELP-LINK-FILE.
           MOVE EASCAN-HELP--DIR  TO HELP-LINK-DIR.

      *************************************************
       VDU-CONVERT-FIELD SECTION.
      *************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBWI/EASCAN_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      *    *************************
      *    *   MISC PARAGRAPHS
      *    *************************
       MISC-PARAGRAPHS SECTION.
       COPY "LIBGB/ACCESS.CPY".
       SEND-LEGEND.
           MOVE "S  A600WSEL:D." TO VDU.
           MOVE MESS TO VDUBUF.
           PERFORM SCREENIO.


      ******************************************
       IO-ROUTINES SECTION.
      ******************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/DECLARE_SQL.CPY".
           PERFORM CLOSE-EA1-FILE.
       COPY "LIBLP/LPEAGS_SQL.CPY".
       COPY "LIBLP/LPEA1RN.CPY".
       COPY "LIBGB/FERRORS.CPY".

      ******************************************
       END-ROUTINE SECTION.
      ******************************************
           PERFORM CLOSE-FILES.
       EXIT-PROG.
       STOP-RUN.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY EASCAN-SCREEN.
           DESTROY EASCAN-WINDOW-HANDLE.
           EXIT PROGRAM.

