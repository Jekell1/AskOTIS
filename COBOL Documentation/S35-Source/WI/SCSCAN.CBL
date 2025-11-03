       IDENTIFICATION DIVISION.
       PROGRAM-ID.      SCSCAN.
      ***************************************************************
      *  SUB CLASS (STATE PARAMETER) WINDOW SCAN
      *  NOTE:
      *       SCSCAN-VERIFY ALLOWS THE SCAN TO BE FINITE
      *       PER THE IT'S VALUE.
      *          EX.
      *             SCSCAN-ORGST    = "FL"
      *             SCSCAN-SPRCLASS = 0
      *             SCSCAN-SUBCLASS = 0
      *             SCSCAN-LAWCODE  = 0
      *          WOULD RESULT IN A WINDOW SCAN FOR ALL
      *          SPR'S FOR THE STATE OF FLORIDA
      *       BECAUSE SUBCLASS = 0 IS VALID, TO IDENTIFY
      *       SCANNING OF A SPECIFIC SUBCLASS, 1 MUST BE
      *       ADDED TO THE SP-SUBCLASS VALUE TO BE SCANNED.
      * REV:
      *  JTG 112096 CHANGED TO DISPLAY SUBCLASS DESCRIPTION SP-DESC
      *  MJD 060397 ADDED SCSCAN-TRANS-DATE. ADDED TESTS TO TRAP VALID SPR.
      *             IF SP-EXPDATE NOT = 0 THEN TEST EXPDATE AGAINST TRANS-DATE.
      *             IF EP-EXPDATE = 0 THEN TEST EFFDATE AGAINST TRANS-DATE.
      * CS 160805 CHANGED SCSCAN-KEY TO SCSCAN-SP1-KEY
      * BAH 170421 ACTIVATE 8 DIGIT DATES, PD0006
      ***************************************************************
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
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)WI/SCSCAN.CBL S35 05/06/24-08:55:09 >".
       COPY "LIBLP/LP01SP.CPY".
       COPY "LIBLP/LP01SPA.CPY".
       COPY "LIBLP/LP01SPB.CPY".
       COPY "LIBLP/LP01SPC.CPY".
       COPY "LIBLP/LP01SP_SQL.CPY".
       COPY "LIBLP/LPWSSP.CPY".

       01  SCSCAN-LINES              PIC 99   VALUE 15.
       01  SCSCAN-COLUMNS            PIC 99   VALUE 25.
       01  SCSCAN-BEGIN-Y            PIC 99   VALUE 08.
       01  SCSCAN-BEGIN-X            PIC 99   VALUE 53.
       01  ERRCD                   PIC X VALUE " ".
       01  ONE                     PIC S9     VALUE 1.
       01  SUB                     PIC 99.

       01  FLDSEL-FIELD            PIC X(4)   VALUE "010.".
       01  FLDSEL-STAT             PIC XX.
       01  FLDSEL-LIST.
           03  FILLER              PIC X(36)  VALUE
           "010 020 030 040 050 060 070 080 090.".
       01  FLDSEL-BUF.
           03  FLDSEL-KEY.
               05  FLDSEL-SUBCLASS PIC 99.
           03  FILLER              PIC X(2).
           03  FLDSEL-DESC         PIC X(15).

       01  FIRST-FIELD             PIC X(4)  VALUE "010.".
       01  LAST-FIELD              PIC X(4)  VALUE "090.".

       01  DISPLAY-LIST.
           03  FILLER                  PIC X(51)  VALUE
           "010 020 030 040 050 060 070 080 090 PGNO ENDMORE:F.".
       01  DISPLAY-BUF.
           03  D-LINE                  OCCURS 9 TIMES.
               05  D-KEY.
                   07  D-SUBCLASS      PIC 99.
               05  FILLER              PIC X(2).
               05  D-DESC              PIC X(15).
           03  D-PAGENO                PIC ZZ9.
           03  D-END-MORE              PIC X(7).

       01  DISPLAY-KEY-BUF.
           03  D-KEY-LINE              OCCURS 100 TIMES.
               05  D-KEY-KEY.
                   07  D-KEY-ORGST     PIC XX.
                   07  D-KEY-SPRCLASS  PIC 99.
                   07  D-KEY-SUBCLASS  PIC 99.
                   07  D-KEY-LAWCODE   PIC 99.

       01  CURRENT-PAGE            PIC S999.
       01  PAGE-COUNT              PIC 999.
       01  MAX-PAGES               PIC 999    VALUE 2.
       01  PAGE-KEY-01.
           03  PAGE-KEY            OCCURS 2 TIMES
                                   PIC X(9).

       01  MORE-LIST.
           03  FILLER     PIC X(43)  VALUE
           "TITLE ORGST CLASS SUBCLASS LAWCODE EFF EXP ".
           03  FILLER     PIC X(33)  VALUE
           "SUBCLASSDESC NOTES.".
       01  MORE-BUF.
           03  FILLER         PIC X.
           03  M-TITLE        PIC X(15).
           03  FILLER         PIC X.
           03  M-ORGST        PIC XX.
           03  M-SPRCLASS     PIC 99.
           03  M-SUBCLASS     PIC 99.
           03  M-LAWCODE      PIC 99.
           03  M-EFFDATE      PIC 99/99/99.
           03  M-EXPDATE      PIC 99/99/99    BLANK ZERO.
           03  M-SUBCLASSDESC PIC X(15).
           03  M-NOTES        PIC X(60).

       COPY "LIBWI/PFSWINDOWSW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/FLDSELW.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBWI/SCSCAN_DEF.CPY".
       COPY "LIBWI/SCSCN2_DEF.CPY".
       COPY "LIBWI/SCSCAN_WKS.CPY".
       COPY "LIBWI/SCSCN2_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       01  SCSCAN-WINDOW-HANDLE    PIC X(10).

       COPY "LIBGB/GETFMW.CPY".
       COPY "LIBWI/SCSCANW.CPY".

       SCREEN SECTION.
       COPY "LIBWI/SCSCAN_SCN.CPY".
       COPY "LIBWI/SCSCN2_SCN.CPY".

       PROCEDURE DIVISION
           USING FORM-PATHNAME SCSCAN-BUF EXIT-PATHNAME.

      ***************************
      *      MAIN PROGRAM MODULE
      ****************************
       MAIN-PROGRAM SECTION.

           PERFORM SQL-CONNECT.

       COPY "LIBGB/CHKSEC.CPY".
      D    STOP MESS.
           MOVE "XX" TO SCSCAN-STATUS.
           MOVE " " TO VDU-WIMENU-SELECTION.
           MOVE "SCSCAN" TO VDU-FORM-NAME.

           DISPLAY FLOATING GRAPHICAL WINDOW
                   AT LINE SCSCAN-BEGIN-Y
                   AT COL  SCSCAN-BEGIN-X
                   SIZE    SCSCAN-COLUMNS + VDU-WIN-W-MOD
                   LINES   SCSCAN-LINES + VDU-WIN-H-MOD
           COLOR IS VDU-WCOLOR-SCAN,CONTROL VALUE IS VDU-WCTRL-SCAN
                   CELL SIZE IS LABEL FONT
                   BLANK SCREEN
                   HANDLE  IS SCSCAN-WINDOW-HANDLE.

           PERFORM FORM-RESET.

       BEGIN-BUILD-PAGENO.
           MOVE FIRST-FIELD TO FLDSEL-FIELD.
           MOVE 1 TO PAGE-COUNT ONE.
           MOVE 0 TO CURRENT-PAGE.
           PERFORM OPEN-SP1-FILE.
           MOVE " " TO SP-ORGST.
           MOVE 0 TO SP-SPRCLASS
                     SP-SUBCLASS
                     SP-LAWCODE.
           IF SCSCAN-ORGST NOT = " "
              MOVE SCSCAN-ORGST TO SP-ORGST
              IF SCSCAN-SPRCLASS NOT = 0
                 MOVE SCSCAN-SPRCLASS TO SP-SPRCLASS
                 IF SCSCAN-SUBCLASS NOT = 0
                    SUBTRACT 1 FROM SCSCAN-SUBCLASS
                                            GIVING SP-SUBCLASS
                    IF SCSCAN-LAWCODE NOT = 0
                       MOVE SCSCAN-LAWCODE TO SP-LAWCODE.
           MOVE SP1-KEY TO PAGE-KEY(PAGE-COUNT).
       START-BUILD-PAGENO.
           ADD ONE TO CURRENT-PAGE.
           IF CURRENT-PAGE < 1
              MOVE FIRST-FIELD TO FLDSEL-FIELD
              MOVE 1 TO CURRENT-PAGE.
           IF CURRENT-PAGE > PAGE-COUNT
              MOVE FIRST-FIELD TO FLDSEL-FIELD
              MOVE 1 TO CURRENT-PAGE.

           MOVE " " TO DISPLAY-BUF
                       DISPLAY-KEY-BUF.
           MOVE 1 TO SUB.
           MOVE PAGE-KEY(CURRENT-PAGE) TO SP1-KEY.
           PERFORM START-SP1-FILE.

       BUILD-PAGENO.
           PERFORM READ-SP1-FILE-NEXT.
           IF IO-FG NOT = 0 OR SUB > 9
              GO TO DISPLAY-PAGE.
           IF SCSCAN-ORGST NOT = " "
              IF SCSCAN-ORGST NOT = SP-ORGST
                 GO TO BUILD-PAGENO.
           IF SCSCAN-SPRCLASS NOT = 0
              IF SCSCAN-SPRCLASS NOT = SP-SPRCLASS
                 GO TO BUILD-PAGENO.
           IF SCSCAN-SUBCLASS NOT = 0
              IF (SCSCAN-SUBCLASS - 1) NOT = SP-SUBCLASS
                 GO TO BUILD-PAGENO.
           IF SCSCAN-LAWCODE NOT = 0
              IF SCSCAN-LAWCODE NOT = SP-LAWCODE
                 GO TO BUILD-PAGENO.
           IF SCSCAN-LAST-LAW = "Y"
              IF SP-EXPDATE NOT = 0
      * TEST THAT SPR HAS REALLY EXPIRED.
                 IF SP-EXPDATE NOT > SCSCAN-TRANS-DATE
                    GO TO BUILD-PAGENO
                 END-IF
              ELSE
      * TEST THAT SPR REALLY IS EFFECTIVE.
                 IF SP-EFFDATE > SCSCAN-TRANS-DATE
                    GO TO BUILD-PAGENO.

           MOVE " " TO D-LINE(SUB).
           MOVE SP-SUBCLASS TO D-SUBCLASS(SUB).
           MOVE SP-DESC TO D-DESC(SUB).

           MOVE " " TO D-KEY-LINE(SP-SUBCLASS + 1).
           MOVE SP-ORGST TO D-KEY-ORGST(SP-SUBCLASS + 1).
           MOVE SP-SPRCLASS TO D-KEY-SPRCLASS(SP-SUBCLASS + 1).
           MOVE SP-SUBCLASS TO D-KEY-SUBCLASS(SP-SUBCLASS + 1).
           MOVE SP-LAWCODE TO D-KEY-LAWCODE(SP-SUBCLASS + 1).
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
                    MOVE SP1-KEY TO PAGE-KEY(PAGE-COUNT)
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
           MOVE "F6-MORE INFO, F7-EXIT:" TO MESS.
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

           MOVE D-KEY-ORGST(FLDSEL-SUBCLASS + 1)
                                        TO SCSCAN-SP-ORGST.
           MOVE D-KEY-SPRCLASS(FLDSEL-SUBCLASS + 1)
                                        TO SCSCAN-SP-SPRCLASS.
           MOVE D-KEY-SUBCLASS(FLDSEL-SUBCLASS + 1)
                                        TO SCSCAN-SP-SUBCLASS.
           MOVE D-KEY-LAWCODE(FLDSEL-SUBCLASS + 1)
                                        TO SCSCAN-SP-LAWCODE.

           MOVE "00" TO SCSCAN-STATUS.

           IF FLDSEL-STAT = "1<"
              PERFORM MORE-INFO
              GO TO SELECT-FIELD-FROM-DISPLAY.

           GO TO END-ROUTINE.


      *************************************
       MORE-INFO SECTION.
      *************************************
           MOVE 8 TO WINDOW-LINES.
           MOVE 76 TO WINDOW-COLUMNS.
           MOVE 15 TO WINDOW-BEGIN-Y.
           MOVE 4 TO WINDOW-BEGIN-X.
           MOVE "WI/SCSCN2" TO WINDOW-FORM-NAM.
           MOVE "SCSCN2" TO VDU-FORM-NAME.

           PERFORM OPEN-WINDOW.
           DISPLAY SCSCN2-SCREEN.

           MOVE SCSCN2-FORM-FIELDS TO WR-BUF.
           MOVE SCSCN2-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

           MOVE SCSCAN-SP1-KEY TO SP1-KEY.
           PERFORM READ-SP1-FILE.
           IF IO-FG = 9
              MOVE "PROBLEM DISPLAYING MORE HELP" TO MESS
              PERFORM SEND-MESS
              GO TO MORE-INFO-CLOSE-WINDOW.

           MOVE SP-DESC     TO M-TITLE.
           MOVE SP-ORGST    TO M-ORGST.
           MOVE SP-SPRCLASS TO M-SPRCLASS.
           MOVE SP-SUBCLASS TO M-SUBCLASS.
           MOVE SP-LAWCODE  TO M-LAWCODE.
           MOVE SP-EFFDATE  TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO M-EFFDATE.
           MOVE SP-EXPDATE  TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO M-EXPDATE.
           MOVE SP-DESC     TO M-SUBCLASSDESC.
           MOVE SP-NOTES    TO M-NOTES.

           MOVE MORE-BUF TO WR-BUF.
           MOVE MORE-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

           MOVE "E  A010CONTINUE." TO VDU.
           PERFORM SCREENIO.

       MORE-INFO-CLOSE-WINDOW.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY SCSCN2-SCREEN.
           PERFORM CLOSE-WINDOW.
           MOVE "SCSCAN" TO VDU-FORM-NAME.
       MORE-INFO-EXIT.
           EXIT.

       COPY "LIBWI/PFSWINDOWS.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/FLDSEL.CPY".

      *************************************************
       FORM-RESET SECTION.
      *************************************************
           DISPLAY SCSCAN-SCREEN UPON SCSCAN-WINDOW-HANDLE.
           MOVE SCSCAN-FORM-FIELDS TO WR-BUF.
           MOVE SCSCAN-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.
       FORM-RESET-EXIT.
           EXIT.

      *************************************************
       SETUP-LINK-INFO SECTION.
      *************************************************
           MOVE SCSCAN-HELP--FILE TO HELP-LINK-FILE.
           MOVE SCSCAN-HELP--DIR  TO HELP-LINK-DIR.

      *************************************************
       VDU-CONVERT-FIELD SECTION.
      *************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBWI/SCSCAN_EVA.CPY".
              COPY "LIBWI/SCSCN2_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      *************************************
       MISC-PARAGRAPHS SECTION.
      *************************************
       COPY "LIBGB/ACCESS.CPY".
       SEND-LEGEND.
           MOVE "S  A600WSEL:D." TO VDU.
           MOVE MESS TO VDUBUF.
           PERFORM SCREENIO.

      ********************************
       IO-ROUTINES SECTION.
      ********************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/DECLARE_SQL.CPY".
           PERFORM CLOSE-SP1-FILE.
       COPY "LIBLP/LPSPGS_SQL.CPY".
       COPY "LIBLP/LPSP1RN.CPY".
       COPY "LIBGB/FERRORS.CPY".

      ********************************
       END-ROUTINE SECTION.
      ********************************
           PERFORM CLOSE-FILES.
       EXIT-PROG.
       STOP-RUN.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY SCSCAN-SCREEN.
           DESTROY SCSCAN-WINDOW-HANDLE.
           EXIT PROGRAM.
