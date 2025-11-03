       IDENTIFICATION DIVISION.
       PROGRAM-ID.      FMSCAN.
      *****************************************************
      *  FORM REQUEST FILE WINDOW SCAN
      *
      * REV:
      *  GLB  020109 START SCAN WITH FMSCAN-FM-INITKEY FROM FMMAIN.C
      *  BAH 051004 INCREASED BUFFER SIZE FROM 300 TO 998.
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
           "@(#)WI/FMSCAN A30 07/08/14-09:56:33 10/01/14-09:55:57 >".
       COPY "LIBLP/LP01FM.CPY".
       COPY "LIBLP/LP01FM_SQL.CPY".
       COPY "LIBLP/LPWSFM.CPY".

       01  FMSCAN-LINES              PIC 99  VALUE 15.
       01  FMSCAN-COLUMNS            PIC 99  VALUE 27.
       01  FMSCAN-BEGIN-Y            PIC 99  VALUE 02.
       01  FMSCAN-BEGIN-X            PIC 99  VALUE 40.

       01  ERRCD                   PIC X      VALUE SPACES.
       01  ONE                     PIC S9     VALUE 1.
       01  SUB                     PIC 99.

       01  FLDSEL-FIELD            PIC X(4)   VALUE "010.".
       01  FLDSEL-STAT             PIC XX.
       01  FLDSEL-LIST.
           03  FILLER              PIC X(36)  VALUE
           "010 020 030 040 050 060 070 080 090.".
       01  FLDSEL-BUF.
           05  FLDSEL-KEY          PIC 9(4).
           05  FILLER              PIC XX.
           05  FLDSEL-NAME         PIC X(20).

       01  FIRST-FIELD             PIC X(4)  VALUE "010.".
       01  LAST-FIELD              PIC X(4)  VALUE "090.".

       01  DISPLAY-LIST.
           03  FILLER              PIC X(51)  VALUE
           "010 020 030 040 050 060 070 080 090 PGNO ENDMORE:F.".
       01  DISPLAY-BUF.
           03  D-LINE              OCCURS 9 TIMES.
               05  D-KEY           PIC 9(4).
               05  FILLER          PIC XX.
               05  D-NAME          PIC X(20).
               05  FILLER          PIC X.
               05  D-PRINT         PIC X.
           03  D-PAGENO            PIC ZZ9.
           03  D-END-MORE          PIC X(7).

       01  CURRENT-PAGE            PIC S999.
       01  PAGE-COUNT              PIC 999.
       01  MAX-PAGES               PIC 999    VALUE 998.
       01  PAGE-KEY-01.
           03  PAGE-KEY                OCCURS 998 TIMES.
               05  PAGE-INITKEY    PIC X(20).
               05  PAGE-FMNO       PIC 9(4).
               05  PAGE-PRINT      PIC X.


       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       01  FMSCAN-WINDOW-HANDLE PIC X(10).
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBWI/FMSCAN_DEF.CPY".
       COPY "LIBWI/FMSCAN_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/FLDSELW.CPY".

       COPY "LIBGB/GETFMW.CPY".
       COPY "LIBWI/FMSCANW.CPY".

       SCREEN SECTION.
       COPY "LIBWI/FMSCAN_SCN.CPY".

       PROCEDURE DIVISION
           USING FORM-PATHNAME FMSCAN-BUF EXIT-PATHNAME.

      *    ***************************
      *    *      MAIN PROGRAM MODULE
      *    ***************************
       MAIN-PROGRAM SECTION.

           PERFORM SQL-CONNECT.

       COPY "LIBGB/CHKSEC.CPY".

           MOVE " "      TO VDU-WIMENU-SELECTION.
           MOVE "XX"     TO FMSCAN-STATUS.
           MOVE "FMSCAN" TO VDU-FORM-NAME.

           DISPLAY FLOATING GRAPHICAL WINDOW
                   AT LINE FMSCAN-BEGIN-Y
                   AT COL  FMSCAN-BEGIN-X
                   SIZE    FMSCAN-COLUMNS + VDU-WIN-W-MOD
                   LINES   FMSCAN-LINES + VDU-WIN-H-MOD
           COLOR IS VDU-WCOLOR-SCAN,CONTROL VALUE IS VDU-WCTRL-SCAN
                   CELL SIZE IS LABEL FONT
                   BLANK SCREEN
                   HANDLE  IS FMSCAN-WINDOW-HANDLE.

           PERFORM FORM-RESET.

       BEGIN-BUILD-PAGENO.
           MOVE FIRST-FIELD TO FLDSEL-FIELD.
           MOVE 1 TO PAGE-COUNT ONE.
           MOVE 0 TO CURRENT-PAGE.

           PERFORM OPEN-FM1-FILE.

           MOVE 0 TO PAGE-FMNO(PAGE-COUNT).
           MOVE FMSCAN-FM-INITKEY TO PAGE-INITKEY(PAGE-COUNT).
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

           MOVE FM-PATH-OWNBR              TO FM-BRNO.
           MOVE PAGE-FMNO(CURRENT-PAGE)    TO FM-FMNO.
           MOVE PAGE-INITKEY(CURRENT-PAGE) TO FM-INITKEY.

           PERFORM START-FM1-FILE.

      *************************************
      *    BUILD PAGE
      *************************************
       BUILD-PAGENO.
           PERFORM READ-FM1-FILE-NEXT.
           IF IO-FG NOT = 0 OR SUB > 9 OR (FM-BRNO NOT = FM-PATH-OWNBR)
              GO TO DISPLAY-PAGE.

           IF FMSCAN-FM-INITKEY NOT = " "
              IF FMSCAN-FM-INITKEY NOT = FM-INITKEY
                 GO TO BUILD-PAGENO.
           IF FMSCAN-FM-FMNO NOT = " "
              IF FMSCAN-FM-FMNO NOT = FM-FMNO
                 GO TO BUILD-PAGENO.

           MOVE FM-FMNO TO D-KEY(SUB).
           MOVE FM-INITKEY TO D-NAME(SUB).
           MOVE FM-PRINT   TO D-PRINT(SUB).
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
                    MOVE FM-FMNO TO PAGE-FMNO(PAGE-COUNT)
                    MOVE FM-INITKEY TO PAGE-INITKEY(PAGE-COUNT)
                    MOVE FM-PRINT   TO PAGE-PRINT(PAGE-COUNT)
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

           MOVE FLDSEL-KEY  TO FMSCAN-KEY FMSCAN-FM-FMNO.
           MOVE FLDSEL-NAME TO FMSCAN-FM-INITKEY.

           MOVE "00" TO FMSCAN-STATUS.

           GO TO END-ROUTINE.

       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/FLDSEL.CPY".
       COPY "LIBGB/DATER.CPY".

      *************************************************
       FORM-RESET SECTION.
      *************************************************
           DISPLAY FMSCAN-SCREEN UPON FMSCAN-WINDOW-HANDLE.
           MOVE FMSCAN-FORM-FIELDS TO WR-BUF.
           MOVE FMSCAN-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      *************************************************
       SETUP-LINK-INFO SECTION.
      *************************************************
           MOVE FMSCAN-HELP--FILE TO HELP-LINK-FILE.
           MOVE FMSCAN-HELP--DIR  TO HELP-LINK-DIR.

      *************************************************
       VDU-CONVERT-FIELD SECTION.
      *************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBWI/FMSCAN_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ****************************************
       MISC-PARAGRAPHS SECTION.
      ****************************************
       COPY "LIBGB/ACCESS.CPY".
       SEND-LEGEND.
           MOVE "S  A600WSEL:D." TO VDU.
           MOVE MESS TO VDUBUF.
           PERFORM SCREENIO.


      ****************************************
       IO-ROUTINES SECTION.
      ****************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/DECLARE_SQL.CPY".
           PERFORM CLOSE-FM1-FILE.
       COPY "LIBLP/LPFMGS_SQL.CPY".
       COPY "LIBLP/LPFM1RN.CPY".
       COPY "LIBGB/FERRORS.CPY".

      ****************************************
       END-ROUTINE SECTION.
      ****************************************
           PERFORM CLOSE-FILES.
       EXIT-PROG.
       STOP-RUN.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY FMSCAN-SCREEN.
           DESTROY FMSCAN-WINDOW-HANDLE.
           EXIT PROGRAM.

