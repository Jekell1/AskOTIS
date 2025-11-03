       IDENTIFICATION DIVISION.
       PROGRAM-ID.      SPSCAN.
      *****************************************************
      *  STATE PARAMETER FILE WINDOW SCAN
      *  NOTE:
      *       SPSCAN-VERIFY ALLOWS THE SCAN TO BE FINITE
      *       PER THE IT'S VALUE.
      *          EX.
      *             SPSCAN-ORGST    = "FL"
      *             SPSCAN-SPRCLASS = 0
      *             SPSCAN-SUBCLASS = 0
      *             SPSCAN-LAWCODE  = 0
      *          WOULD RESULT IN A WINDOW SCAN FOR ALL
      *          SPR'S FOR THE STATE OF FLORIDA
      *       BECOUSE SUBCLASS = 0 IS VALID, TO IDENTIFY
      *       SCANNING OF A SPECIFIC SUBCLASS, 1 MUST BE
      *       ADDED TO THE SP-SUBCLASS VALUE TO BE SCANNED.
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
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)WI/SPSCAN.CBL S34 05/24/21-13:06:44 >".
       COPY "LIBLP/LP01SP.CPY".
       COPY "LIBLP/LP01SPA.CPY".
       COPY "LIBLP/LP01SPB.CPY".
       COPY "LIBLP/LP01SPC.CPY".
       COPY "LIBLP/LP01SP_SQL.CPY".
       COPY "LIBLP/LPWSSP.CPY".
       COPY "LIBLP/LP01LC.CPY".
       COPY "LIBLP/LP01LC_SQL.CPY".
       COPY "LIBLP/LPWSLC.CPY".

       01  SPSCAN-LINES              PIC 99   VALUE 15.
       01  SPSCAN-COLUMNS            PIC 99   VALUE 54.
       01  SPSCAN-BEGIN-Y            PIC 99   VALUE 08.
       01  SPSCAN-BEGIN-X            PIC 99   VALUE 25.
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
               05  FLDSEL-ORGST    PIC XX.
               05  FILLER          PIC X.
               05  FLDSEL-SPRCLASS PIC 99.
               05  FILLER          PIC X.
               05  FLDSEL-SUBCLASS PIC 99.
               05  FILLER          PIC X.
               05  FLDSEL-LAWCODE  PIC 99.
           03  FILLER              PIC X(4).
           03  FLDSEL-DESC         PIC X(15).
           03  FILLER              PIC X(2).
           03  FLDSEL-EFFDATE      PIC 99/99/99.
           03  FILLER              PIC X(2).
           03  FLDSEL-EXPDATE      PIC 99/99/99.

       01  FIRST-FIELD             PIC X(4)  VALUE "010.".
       01  LAST-FIELD              PIC X(4)  VALUE "090.".

       01  DISPLAY-LIST.
           03  FILLER              PIC X(51)  VALUE
           "010 020 030 040 050 060 070 080 090 PGNO ENDMORE:F.".
       01  DISPLAY-BUF.
           03  D-LINE              OCCURS 9 TIMES.
               05  D-KEY.
                   07  D-ORGST     PIC XX.
                   07  FILLER      PIC X.
                   07  D-CLASS     PIC 99.
                   07  FILLER      PIC X.
                   07  D-SUBCLASS  PIC 99.
                   07  FILLER      PIC X.
                   07  D-LAWCODE   PIC 99.
               05  FILLER          PIC X(4).
               05  D-DESC          PIC X(15).
               05  FILLER          PIC X(2).
               05  D-EFFDATE       PIC 99/99/99.
               05  FILLER          PIC X(2).
               05  D-EXPDATE       PIC 99/99/99 BLANK ZERO.
           03  D-PAGENO            PIC ZZ9.
           03  D-END-MORE          PIC X(7).

       01  CURRENT-PAGE            PIC S999.
       01  PAGE-COUNT              PIC 999.
       01  MAX-PAGES               PIC 999    VALUE 10.
       01  PAGE-KEY-01.
           03  PAGE-KEY            OCCURS 10 TIMES
                                   PIC X(9).


       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       01  SPSCAN-WINDOW-HANDLE PIC X(10).
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBWI/SPSCAN_DEF.CPY".
       COPY "LIBWI/SPSCAN_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/FLDSELW.CPY".

       COPY "LIBGB/GETFMW.CPY".
       COPY "LIBWI/SPSCANW.CPY".

       SCREEN SECTION.
       COPY "LIBWI/SPSCAN_SCN.CPY".

       PROCEDURE DIVISION
           USING FORM-PATHNAME SPSCAN-BUF EXIT-PATHNAME.

      ****************************
      *      MAIN PROGRAM MODULE
      ****************************
       MAIN-PROGRAM SECTION.

           PERFORM SQL-CONNECT.

       COPY "LIBGB/CHKSEC.CPY".

      D    STOP MESS.
           MOVE "XX" TO SPSCAN-STATUS.

           MOVE " " TO VDU-WIMENU-SELECTION.
           MOVE "SPSCAN" TO VDU-FORM-NAME.

           DISPLAY FLOATING GRAPHICAL WINDOW
                   AT SCREEN LINE SPSCAN-BEGIN-Y
                   AT SCREEN COL  SPSCAN-BEGIN-X
                   CELL SIZE IS LABEL FONT
           COLOR IS VDU-WCOLOR-TCLP,CONTROL VALUE IS VDU-WCTRL-TCLP
                      SIZE    SPSCAN-COLUMNS + VDU-WIN-W-MOD
                      LINES   SPSCAN-LINES + VDU-WIN-H-MOD
                      BLANK SCREEN
                   HANDLE  IS SPSCAN-WINDOW-HANDLE.

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
           IF SPSCAN-ORGST NOT = " "
              MOVE SPSCAN-ORGST TO SP-ORGST
              IF SPSCAN-SPRCLASS NOT = 0
                 MOVE SPSCAN-SPRCLASS TO SP-SPRCLASS
                 IF SPSCAN-SUBCLASS NOT = 0
                    SUBTRACT 1 FROM SPSCAN-SUBCLASS
                                       GIVING SP-SUBCLASS
                    IF SPSCAN-LAWCODE NOT = 0
                       MOVE SPSCAN-LAWCODE TO SP-LAWCODE.
           MOVE SP1-KEY TO PAGE-KEY(PAGE-COUNT).
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
           MOVE PAGE-KEY(CURRENT-PAGE) TO SP1-KEY.
           PERFORM START-SP1-FILE.

      *************************************
      *    BUILD PAGE
      *************************************
       BUILD-PAGENO.
           PERFORM READ-SP1-FILE-NEXT.
           IF IO-FG NOT = 0 OR SUB > 9
              GO TO DISPLAY-PAGE.
           IF SPSCAN-ORGST NOT = " "
              IF SPSCAN-ORGST NOT = SP-ORGST
                 GO TO BUILD-PAGENO.
           IF SPSCAN-SPRCLASS NOT = 0
              IF SPSCAN-SPRCLASS NOT = SP-SPRCLASS
                 GO TO BUILD-PAGENO.
           IF SPSCAN-SUBCLASS NOT = 0
              IF (SPSCAN-SUBCLASS - 1) NOT = SP-SUBCLASS
                 GO TO BUILD-PAGENO.
           IF SPSCAN-LAWCODE NOT = 0
              IF SPSCAN-LAWCODE NOT = SP-LAWCODE
                 GO TO BUILD-PAGENO.
           IF SPSCAN-LAST-LAW = "Y"
              IF SP-EXPDATE NOT = 0
                 GO TO BUILD-PAGENO.
           IF SPSCAN-SALEFIN NOT = " "
              MOVE SP-SPRCLASS TO LC-CODE
              PERFORM GET-LC
              IF IO-FG = 9
                 GO TO IO-ERROR
              ELSE
                 IF SPSCAN-SALEFIN NOT = LC-SALEFIN
                    GO TO BUILD-PAGENO.

           MOVE " " TO D-LINE(SUB).
           MOVE SP-ORGST TO D-ORGST(SUB).
           MOVE SP-SPRCLASS TO D-CLASS(SUB).
           MOVE SP-SUBCLASS TO D-SUBCLASS(SUB).
           MOVE SP-LAWCODE TO D-LAWCODE(SUB).
           MOVE SP-DESC TO D-DESC(SUB).
           IF SPSCAN-LAST-LAW NOT = "Y"
              MOVE SP-EFFDATE TO DATE-YYYYMMDD
              PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
              MOVE DATE-MMDDYY TO D-EFFDATE(SUB)
              MOVE SP-EXPDATE TO DATE-YYYYMMDD
              PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
              MOVE DATE-MMDDYY TO D-EXPDATE(SUB).
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
           IF NOT (FLDSEL-STAT = "00")
              PERFORM ALARM
              GO TO SELECT-FIELD-FROM-DISPLAY.

           MOVE FLDSEL-ORGST    TO SPSCAN-SP-ORGST.
           MOVE FLDSEL-SPRCLASS TO SPSCAN-SP-SPRCLASS.
           MOVE FLDSEL-SUBCLASS TO SPSCAN-SP-SUBCLASS.
           MOVE FLDSEL-LAWCODE  TO SPSCAN-SP-LAWCODE.
           MOVE "00"            TO SPSCAN-STATUS.

           GO TO END-ROUTINE.

       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/FLDSEL.CPY".

      *************************************************
       FORM-RESET SECTION.
      *************************************************
           DISPLAY SPSCAN-SCREEN UPON SPSCAN-WINDOW-HANDLE.
           MOVE SPSCAN-FORM-FIELDS TO WR-BUF. 
           MOVE SPSCAN-FORM-LIST TO WR-LIST. 
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      *************************************************
       SETUP-LINK-INFO SECTION.
      *************************************************
           MOVE SPSCAN-HELP--FILE TO HELP-LINK-FILE.
           MOVE SPSCAN-HELP--DIR  TO HELP-LINK-DIR.

      *************************************************
       VDU-CONVERT-FIELD SECTION.
      *************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBWI/SPSCAN_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      **************************
       MISC-PARAGRAPHS SECTION.
      **************************
       COPY "LIBGB/ACCESS.CPY".
       SEND-LEGEND.
           MOVE "S  A600WSEL:D." TO VDU.
           MOVE MESS TO VDUBUF.
           PERFORM SCREENIO.
       GET-LC.
           PERFORM OPEN-LC1-FILE.
           PERFORM READ-LC1-FILE.
           PERFORM CLOSE-LC1-FILE.

      ******************************************
       IO-ROUTINES SECTION.
      ******************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/DECLARE_SQL.CPY".
           PERFORM CLOSE-SP1-FILE.
           PERFORM CLOSE-LC1-FILE.
       COPY "LIBLP/LPSPGS_SQL.CPY".
       COPY "LIBLP/LPLCGS_SQL.CPY".
       COPY "LIBLP/LPSP1RN.CPY".
       COPY "LIBLP/LPLC1RN.CPY".
       COPY "LIBGB/FERRORS.CPY".

      ******************************************
       END-ROUTINE SECTION.
      ******************************************
           PERFORM CLOSE-FILES.
       EXIT-PROG.
       STOP-RUN.
           DESTROY SPSCAN-WINDOW-HANDLE.
           EXIT PROGRAM.

