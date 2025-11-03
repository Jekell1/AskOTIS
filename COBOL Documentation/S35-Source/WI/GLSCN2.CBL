       IDENTIFICATION DIVISION.
       PROGRAM-ID.      GLSCN2.
       DATE-WRITTEN.    031496.
      *****************************************************************
      *               GL NUMBER WINDOW SCAN
      *  WAS GLSCAN, CHANGED TO GLSCN2, DUPLICATE PROGRAM IN GL/WI
      *
      *
      *  DESCRIPTION:  WINDOW SCAN FOR GLFILE.
      *
      *          OUT:   GLSCN2-BRANCH     ( GL-BRANCH )
      *                 GLSCN2-ACCOUNT    ( GL-ACCOUNT )
      *  REV:
      *    150331 CS DUPLICATE PROGRAM IN WI/GL, CHANGED WI PROGRAM FROM
      *              GLSCAN TO GLSCN2
      *    210712 BLM  MOVE 0 TO PAGE-KEY(1), NOT LOW-VALUES
      *****************************************************************
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
           "@(#)WI/GLSCN2 S34 07/12/21-14:14:38 >".
       COPY "LIBGL/GL01GL.CPY".
       COPY "LIBGL/GL01GL_SQL.CPY".
       COPY "LIBGL/GLWSGL.CPY".

       01  GLSCN2-LINES          PIC 99  VALUE 16.
       01  GLSCN2-COLUMNS        PIC 99  VALUE 39.
       01  GLSCN2-BEGIN-Y        PIC 99  VALUE 04.
       01  GLSCN2-BEGIN-X        PIC 99  VALUE 39.

       01  ERRCD                   PIC X      VALUE SPACES.
       01  ONE                     PIC S9     VALUE 1.
       01  SUB                     PIC 99.

       01  CURRENT-PAGE            PIC S999.
       01  PAGE-COUNT              PIC 999.
       01  MAX-PAGES               PIC 999    VALUE 50.
       01  PAGE-KEY-01.
           03  PAGE-KEY-WORK       OCCURS 50 TIMES.
               05  PAGE-KEY        PIC 9(11).
      *--------05  PAGE-KEY        PIC 9(10).

      *----------------------------------------------------------------
      * INPUT BUFFERS:
      *----------------------------------------------------------------
       01  FLDSEL-FIELD            PIC X(4)   VALUE "010.".
       01  FLDSEL-STAT             PIC XX.
       01  FIRST-FIELD             PIC X(4)  VALUE "010.".
       01  LAST-FIELD              PIC X(4)  VALUE "090.".

       01  FLDSEL-BUF.
           05  FLDSEL-KEY          PIC X(11).
           05  FLDSEL-KEYX         REDEFINES FLDSEL-KEY.
               07 FLDSEL-BR        PIC 9(3).
               07 FILLER           PIC X.
               07 FLDSEL-ACCT      PIC 9(7).
           05  FILLER              PIC XX.
           05  FLDSEL-NAME         PIC X(29).

       01  FLDSEL-LIST.
           03  FILLER              PIC X(36)  VALUE
           "010 020 030 040 050 060 070 080 090.".

      *----------------------------------------------------------------
      * DISPLAY BUFFERS:
      *----------------------------------------------------------------

       01  DISPLAY-BUF.
           03  D-LINE              OCCURS 9 TIMES.
               05  D-KEY           PIC 999/9999999.
               05  D-KEYX          REDEFINES D-KEY.
                   07  D-BR        PIC 9(3).
                   07  D-DASH      PIC X.
                   07  D-ACCT      PIC 9(7).
               05  FILLER          PIC XX.
               05  D-NAME          PIC X(22).
           03  D-PAGENO            PIC ZZ9.
           03  D-END-MORE          PIC X(7).

       01  DISPLAY-LIST.
           03  FILLER              PIC X(51)  VALUE
           "010 020 030 040 050 060 070 080 090 PGNO ENDMORE:F.".

      *----------------------------------------------------------------

       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       01  GLSCN2-WINDOW-HANDLE    PIC X(10).
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/FLDSELW.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBWI/GLSCN2_DEF.CPY".
       COPY "LIBWI/GLSCN2_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".

       COPY "LIBGB/GETFMW.CPY".
       COPY "LIBWI/GLSCN2W.CPY".

       SCREEN SECTION.
       COPY "LIBWI/GLSCN2_SCN.CPY".

       PROCEDURE DIVISION
           USING FORM-PATHNAME GLSCN2-BUF EXIT-PATHNAME.

      *****************************************************************
      *          MAIN PROGRAM MODULE
      *****************************************************************
       MAIN-PROGRAM SECTION.

           PERFORM SQL-CONNECT.

       COPY "LIBGB/CHKSEC.CPY".
      D    STOP MESS.
           MOVE " " TO VDU-WIMENU-SELECTION.

           MOVE "GLSCN2" TO VDU-FORM-NAME.

           DISPLAY FLOATING GRAPHICAL WINDOW
                   AT LINE GLSCN2-BEGIN-Y
                   AT COL  GLSCN2-BEGIN-X
                   SIZE    GLSCN2-COLUMNS + VDU-WIN-W-MOD
                   LINES   GLSCN2-LINES + VDU-WIN-H-MOD
           COLOR IS VDU-WCOLOR-SCAN,CONTROL VALUE IS VDU-WCTRL-SCAN
                   CELL SIZE IS LABEL FONT
                   BLANK SCREEN
                   HANDLE  IS GLSCN2-WINDOW-HANDLE.

           PERFORM FORM-RESET.

           MOVE "S  A300TITLE." TO VDU.
           MOVE "   GL ACCOUNT NUMBER SCAN  " TO VDUBUF.
           PERFORM SCREENIO.

       BEGIN-BUILD-PAGENO.
           MOVE FIRST-FIELD TO FLDSEL-FIELD.
           MOVE 1 TO PAGE-COUNT ONE.
           MOVE 0 TO CURRENT-PAGE.
           PERFORM OPEN-GL1-FILE.
           MOVE 0 TO PAGE-KEY(PAGE-COUNT).
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
           MOVE PAGE-KEY(CURRENT-PAGE) TO GL1-KEY.
           PERFORM START-GL1-FILE.

      *----------------------------------------------------------------
      *    BUILD PAGE
      *----------------------------------------------------------------
       BUILD-PAGENO.
           PERFORM READ-GL1-FILE-NEXT.
           IF IO-FG NOT = 0 OR SUB > 9
              GO TO DISPLAY-PAGE.

           MOVE GL-BRANCH TO D-BR(SUB).
           MOVE GL-ACCOUNT TO D-ACCT(SUB).
           MOVE "-" TO D-DASH(SUB).
           MOVE GL-DESCRIPTION TO D-NAME(SUB).
           ADD 1 TO SUB.
           GO TO BUILD-PAGENO.

      *----------------------------------------------------------------
      *    DISPLAY PAGE
      *----------------------------------------------------------------
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
                    MOVE GL-BR-ACCOUNT TO PAGE-KEY(PAGE-COUNT)
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

      *****************************************************************
      *    SELECT FIELD FROM PAGES
      *****************************************************************
       SELECT-FIELD-FROM-DISPLAY.
           MOVE "F7 EXIT, SELECT CODE(RTN):" TO MESS.
           PERFORM SEND-LEGEND.

           PERFORM CALL-FLDSEL.

      *----------------------------------------------------------------
      * TEST UP ARROW:
      *----------------------------------------------------------------
           IF FLDSEL-STAT = "1U"
              IF CURRENT-PAGE = 1
                 PERFORM ALARM
                 GO TO SELECT-FIELD-FROM-DISPLAY
              ELSE
                 MOVE -1 TO ONE
                 MOVE LAST-FIELD TO FLDSEL-FIELD
                 GO TO START-BUILD-PAGENO.

      *----------------------------------------------------------------
      * TEST PAGE UP:
      *----------------------------------------------------------------
           IF FLDSEL-STAT = "1P"
              IF CURRENT-PAGE = 1
                 PERFORM ALARM
                 GO TO SELECT-FIELD-FROM-DISPLAY
              ELSE
                 MOVE -1 TO ONE
                 MOVE FIRST-FIELD TO FLDSEL-FIELD
                 GO TO START-BUILD-PAGENO.

      *----------------------------------------------------------------
      * TEST PAGE DOWN OR DOWN ARROW:
      *----------------------------------------------------------------
           IF FLDSEL-STAT = "1N" OR FLDSEL-STAT = "1D"
              IF CURRENT-PAGE NOT < PAGE-COUNT
                 PERFORM ALARM
                 GO TO SELECT-FIELD-FROM-DISPLAY
              ELSE
                 MOVE 1 TO ONE
                 MOVE FIRST-FIELD TO FLDSEL-FIELD
                 GO TO START-BUILD-PAGENO.

      *----------------------------------------------------------------
      * TEST HOME:
      *----------------------------------------------------------------
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

           MOVE FLDSEL-BR TO GLSCN2-BRANCH.
           MOVE FLDSEL-ACCT TO GLSCN2-ACCOUNT.

           GO TO END-ROUTINE.


       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/FLDSEL.CPY".
       COPY "LIBGB/DATER.CPY".

      *************************************************
       FORM-RESET SECTION.
      *************************************************
           DISPLAY GLSCN2-SCREEN UPON GLSCN2-WINDOW-HANDLE.
           MOVE GLSCN2-FORM-FIELDS TO WR-BUF.
           MOVE GLSCN2-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      *************************************************
       SETUP-LINK-INFO SECTION.
      *************************************************
           MOVE GLSCN2-HELP--FILE TO HELP-LINK-FILE.
           MOVE GLSCN2-HELP--DIR  TO HELP-LINK-DIR.

      *************************************************
       VDU-CONVERT-FIELD SECTION.
      *************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBWI/GLSCN2_EVA.CPY".
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
           PERFORM CLOSE-GL1-FILE.
       COPY "LIBGL/GLGLGS_SQL.CPY".
       COPY "LIBGL/GLGL1RN.CPY".

       COPY "LIBGB/FERRORS.CPY".

      *****************************************************************
       END-ROUTINE SECTION.
      *****************************************************************
           PERFORM CLOSE-FILES.
       EXIT-PROG.
       STOP-RUN.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY GLSCN2-SCREEN.
           DESTROY GLSCN2-WINDOW-HANDLE.
           EXIT PROGRAM.

