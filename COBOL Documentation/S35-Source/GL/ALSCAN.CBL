       IDENTIFICATION DIVISION.
       PROGRAM-ID.      ALSCAN.
      *****************************************************
      *  GL ALLOCATION FILE SCAN
      *
      *  MG  000124 FIXED VDUSTATUS/IO-FG/STAT PER DEVELOPMENT MEETING.
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
       77  SCCS-IDENTIFIER PIC X(43) VALUE
           "@(#)./GL/ALSCAN.CBL S35 09/20/22-13:04:48 >".
       COPY "LIBGL/GL01AL.CPY".
       COPY "LIBGL/GL01AL_SQL.CPY".
       COPY "LIBGL/GLWSAL.CPY".

       01  ALSCAN-LINES        PIC 99  VALUE 18.
       01  ALSCAN-COLUMNS      PIC 99  VALUE 57.
       01  ALSCAN-BEGIN-Y      PIC 99  VALUE 5.
       01  ALSCAN-BEGIN-X      PIC 99  VALUE 22.
       01  ERRCD           PIC X VALUE " ".
       01  SUB             PIC 99.
       01  ONE             PIC S9 VALUE 1.

       01  FLDSEL-STAT     PIC XX.
       01  FIRST-FIELD          PIC X(4)  VALUE "010.".
       01  LAST-FIELD           PIC X(4)  VALUE "100.".

       01  FLDSEL-FIELD  PIC X(4)  VALUE "010.".

       01  FLDSEL-LIST.
           03  FILLER     PIC X(40) VALUE
               "010 020 030 040 050 060 070 080 090 100.".
       01  FLDSEL-BUF.
           03  FLDSEL-KEY      PIC X(14).
           03  FLDSEL-DESC     PIC X(34).
           03  FLDSEL-NO-ACCTS PIC Z9.

       01  DISPLAY-LIST.
           03  FILLER     PIC X(40) VALUE
                   "010 020 030 040 050 060 070 080 090 100 ".
           03  FILLER     PIC X(13) VALUE
                   "PGNO ENDMORE.".

       01  DISPLAY-BUF.
           03  D-LINE      OCCURS 10 TIMES.
               05  D-ID         PIC X(14).
               05  D-DESC       PIC X(34).
               05  D-NO-ACCTS   PIC Z9.
           03  D-PAGE           PIC ZZ9.
           03  D-END-MORE       PIC X(7).

       01  PAGE-KEY-TABLE.
           03  PAGE-KEY    PIC X(12)   OCCURS 100.
       01  PAGE-NUMBER     PIC 999.
       01  MAX-PAGES       PIC 999.
       01  TABLE-SIZE      PIC 999     VALUE 100.

       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/DATERW.CPY".
       01  ALSCAN-WINDOW-HANDLE  PIC X(10).
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBGL/ALSCAN_DEF.CPY".
       COPY "LIBGL/ALSCAN_WKS.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/FLDSELW.CPY".
       COPY "LIBGB/HELPLINK.CPY".

       COPY "LIBGB/GETFMW.CPY".
       COPY "LIBGL/ALSCANW.CPY".

       SCREEN SECTION.
       COPY "LIBGL/ALSCAN_SCN.CPY".

       PROCEDURE DIVISION
           USING FORM-PATHNAME ALSCAN-BUF EXIT-PATHNAME.

      **************************************
       MAIN-PROGRAM SECTION.
      **************************************

           PERFORM SQL-CONNECT.

           MOVE "ALSCAN" TO VDU-FORM-NAME.
           MOVE "XX"     TO ALSCAN-STATUS.

           DISPLAY FLOATING GRAPHICAL WINDOW
                    SCREEN LINE ALSCAN-BEGIN-Y * VDU-WIN-L-OFFSET
                    SCREEN COL ALSCAN-BEGIN-X * VDU-WIN-C-OFFSET
           COLOR IS VDU-WCOLOR-SCAN,CONTROL VALUE IS VDU-WCTRL-SCAN
                   CELL SIZE IS LABEL FONT
                   SIZE ALSCAN-COLUMNS + VDU-WIN-W-MOD
                   LINES ALSCAN-LINES + VDU-WIN-H-MOD
                   BLANK SCREEN
                   HANDLE IS ALSCAN-WINDOW-HANDLE.

       COPY "LIBGB/CHKSEC.CPY".
           PERFORM FORM-RESET.

           PERFORM OPEN-AL1-FILE.
           IF ALSCAN-KEY NOT = SPACES
              MOVE "S  A000RANGE." TO VDU
              MOVE ALSCAN-KEY TO VDUBUF
              PERFORM SCREENIO
              GO TO SKIP-ENTER-BEG-ALLOC.
       ENTER-BEG-ALLOC.
           MOVE "R  A110RANGE." TO VDU.
           PERFORM SCREENIO.
           IF F7-KEY OR F1-KEY
              GO TO END-ROUTINE.
           IF NOT ENTER-KEY
              GO TO ENTER-BEG-ALLOC.
       SKIP-ENTER-BEG-ALLOC.
           MOVE FIRST-FIELD TO FLDSEL-FIELD.
           MOVE 1 TO MAX-PAGES ONE.
           MOVE 0 TO PAGE-NUMBER.
           MOVE VDUBUF TO PAGE-KEY(MAX-PAGES).
       DISPLAY-PAGE.
           ADD ONE TO PAGE-NUMBER.
           IF PAGE-NUMBER < 1
              MOVE FIRST-FIELD TO FLDSEL-FIELD
              MOVE 1 TO PAGE-NUMBER.
           IF PAGE-NUMBER > MAX-PAGES
              MOVE FIRST-FIELD TO FLDSEL-FIELD
              MOVE 1 TO PAGE-NUMBER.
           MOVE SPACES TO DISPLAY-BUF.
           MOVE 1 TO SUB.
           MOVE PAGE-KEY(PAGE-NUMBER) TO AL1-KEY.

           PERFORM START-AL1-FILE.
       MOVE-DISPLAY.
           PERFORM READ-AL1-FILE-NEXT.
           IF IO-BAD OR SUB > 10
              GO TO SEND-DISPLAY.

           IF AL-NO-ACCTS > ALSCAN-MAX-ACCTS
              GO TO MOVE-DISPLAY.

           MOVE AL-ID TO D-ID(SUB).
           MOVE AL-DESC TO D-DESC(SUB).
           MOVE AL-NO-ACCTS TO D-NO-ACCTS(SUB).
           ADD 1 TO SUB.
           GO TO MOVE-DISPLAY.
       SEND-DISPLAY.
           IF IO-BAD
              IF SUB = 1
                 MOVE "NO ALLOCATIONS" TO D-ID(4)
                 MOVE DISPLAY-LIST TO WR-LIST
                 MOVE DISPLAY-BUF TO WR-BUF
                 PERFORM CALL-WRFORM
                 GO TO ENTER-BEG-ALLOC.
           MOVE "MORE..." TO D-END-MORE.
           IF PAGE-NUMBER = MAX-PAGES
              IF IO-GOOD
                 IF MAX-PAGES < TABLE-SIZE
                    ADD 1 TO MAX-PAGES
                    MOVE AL1-KEY TO PAGE-KEY(MAX-PAGES)
                 ELSE
                    MOVE "OVERFLOW" TO D-END-MORE
              ELSE
                 MOVE "* END *" TO D-END-MORE.
           MOVE PAGE-NUMBER TO D-PAGE.
           MOVE DISPLAY-LIST TO WR-LIST.
           MOVE DISPLAY-BUF TO WR-BUF.
           PERFORM CALL-WRFORM.
       SELECT-DISPLAY.
           PERFORM CALL-FLDSEL.
           IF FLDSEL-STAT = "1U"
              IF PAGE-NUMBER = 1
                 PERFORM ALARM
                 GO TO SELECT-DISPLAY
              ELSE
                 MOVE -1 TO ONE
                 MOVE LAST-FIELD TO FLDSEL-FIELD
                 GO TO DISPLAY-PAGE.
           IF FLDSEL-STAT = "1P"
              IF PAGE-NUMBER = 1
                 PERFORM ALARM
                 GO TO SELECT-DISPLAY
              ELSE
                 MOVE -1 TO ONE
                 MOVE FIRST-FIELD TO FLDSEL-FIELD
                 GO TO DISPLAY-PAGE.
           IF FLDSEL-STAT = "1N" OR FLDSEL-STAT = "1D"
              IF PAGE-NUMBER NOT < MAX-PAGES
                 PERFORM ALARM
                 GO TO SELECT-DISPLAY
              ELSE
                 MOVE 1 TO ONE
                 MOVE FIRST-FIELD TO FLDSEL-FIELD
                 GO TO DISPLAY-PAGE.
           IF FLDSEL-STAT = "1H"
              MOVE 0 TO PAGE-NUMBER
              MOVE 1 TO ONE
              MOVE FIRST-FIELD TO FLDSEL-FIELD
              GO TO DISPLAY-PAGE.
           IF FLDSEL-STAT = "1>"
              MOVE SPACES TO ALSCAN-KEY
              GO TO END-ROUTINE.
           IF FLDSEL-STAT = "10"
              GO TO ENTER-BEG-ALLOC.
           IF FLDSEL-STAT NOT = "00"
              PERFORM ALARM
              GO TO SELECT-DISPLAY.

           MOVE FLDSEL-KEY TO ALSCAN-KEY.
           MOVE "00"       TO ALSCAN-STATUS.

           GO TO END-ROUTINE.

      **********************************************************
       FORM-RESET SECTION.
      **********************************************************
           DISPLAY ALSCAN-SCREEN UPON ALSCAN-WINDOW-HANDLE.
           MOVE ALSCAN-FORM-FIELDS TO WR-BUF.
           MOVE ALSCAN-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      **********************************************************
       SETUP-LINK-INFO SECTION.
      **********************************************************
           MOVE ALSCAN-HELP--FILE TO HELP-LINK-FILE.
           MOVE ALSCAN-HELP--DIR  TO HELP-LINK-DIR.

      **********************************************************
       VDU-CONVERT-FIELD SECTION.
      **********************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBGL/ALSCAN_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/FLDSEL.CPY".

      ******************************
       MISC-PARAGRAPHS SECTION.
      ******************************
       COPY "LIBGB/ACCESS.CPY".

      ******************************
       IO-ROUTINES SECTION.
      ******************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/DECLARE_SQL.CPY".
           PERFORM CLOSE-AL1-FILE.
       COPY "LIBGL/GLALGS_SQL.CPY".
       COPY "LIBGL/GLAL1RN.CPY".
       COPY "LIBGB/FERRORS.CPY".

      ******************************
       END-ROUTINE SECTION.
      ******************************
           PERFORM CLOSE-FILES.
       EXIT-PROG.
       STOP-RUN.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY ALSCAN-SCREEN.
           DESTROY ALSCAN-WINDOW-HANDLE.
           EXIT PROGRAM.
