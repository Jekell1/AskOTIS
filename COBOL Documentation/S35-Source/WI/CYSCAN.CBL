       IDENTIFICATION DIVISION.
       PROGRAM-ID.      CYSCAN.
      **************************************************************************
      *     CITY SALES TAX CODES WINDOW SCAN
      *
      *REV:
      * JTG 020207 **NEW**
      * JTG 020201 ADDED "CT" COUNTY TAX CODE KENTUCKY AND PERCENTS
      *            ADDED "CY" CITY   TAX CODE KENTUCKY AND PERCENTS   WORLD #288
      * BLV 021206 FIXED PAGE KEY TO 2 X(3) INSTEAD OF X(2), COULDN'T PAGE DOWN;
      *            CHANGED MAX PAGES FROM 30 TO 200, COULDN'T SCAN ALL THE
      *            CITIES IN KENTUCY, WORLD PR# 288
      *  CS 110512 FIXED SCAN, WHEN WE ORIGINALLY DID, FORCED 'KY' TO STATE
      *            & DISPLAYED ALL CODES IN FILE, NOW THAT WE ALLOW A STATE
      *            IF CD-CT-STATE NOT = CTSCAN-STATE BE DONE REGENCY PR#3784
      *
      * KEC 2016.0328 {PD#00003} PARADATA <A30> [JAY CISZEWSKI]
      *         CHANGED CYSCAN WORKERS TO MATCH CD1-KEY FOR CY LAYOUT.
      *
      * KEC 2016.0406 {PD#00003} PARADATA <A30> [JAY CISZEWSKI]
      *         FIXED DISPLAY-BUF AND DISPLAY-LIST FOR ISSUES.
      *         FLDSEL-LIST FIELDS MUST BE FIRST/TOP WITHIN
      *         DISPLAY-BUF AND DISPLAY-LIST OTHERWISE THERE WILL
      *         ISSUES ON THE DISPLAY.
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
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)WI/CYSCAN.CBL S35 05/02/24-07:54:06 >".
       COPY "LIBLP/LP01CD.CPY".
       COPY "LIBLP/LP01CD_SQL.CPY".
       COPY "LIBLP/LPWSCD.CPY".

       01  CYSCAN-LINES               PIC 99      VALUE 17.
       01  CYSCAN-COLUMNS             PIC 99      VALUE 27.
       01  CYSCAN-BEGIN-Y             PIC 99      VALUE 04.
       01  CYSCAN-BEGIN-X             PIC 99      VALUE 10.
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
           05  FLDSEL-NAME         PIC X(18).

       01  FIRST-FIELD             PIC X(4)  VALUE "010.".
       01  LAST-FIELD              PIC X(4)  VALUE "090.".

       01  DISPLAY-LIST.
           03  FILLER PIC X(20)  VALUE "010 020 030 040 050 ".
           03  FILLER PIC X(16)  VALUE "060 070 080 090 ".
           03  FILLER PIC X(16)  VALUE "STATE STATEDESC ".
           03  FILLER PIC X(15)  VALUE "PGNO ENDMORE:F.".

       01  DISPLAY-BUF.
           03  D-LINE              OCCURS 9 TIMES.
               05  D-KEY           PIC X(3).
               05  FILLER          PIC XX.
               05  D-CITY          PIC X(18).
           03  D-STATE             PIC XX.
           03  D-STATE-DESC        PIC X(16).
           03  D-PAGENO            PIC ZZ9.
           03  D-END-MORE          PIC X(7).

       01  CURRENT-PAGE            PIC S999.
       01  PAGE-COUNT              PIC 999.
       01  MAX-PAGES               PIC 999    VALUE 200.
       01  PAGE-KEY-01             VALUE " ".
           03  PAGE-KEY            OCCURS 200
                                   PIC XXX.



       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/ACCESSW.CPY".

       COPY "LIBGB/DATERW.CPY".
       01  CYSCAN-WINDOW-HANDLE    PIC X(10).
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/FLDSELW.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBWI/CYSCAN_DEF.CPY".
       COPY "LIBWI/CYSCAN_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".

       COPY "LIBGB/GETFMW.CPY".
       COPY "LIBWI/CYSCANW.CPY".

       SCREEN SECTION.
       COPY "LIBWI/CYSCAN_SCN.CPY".

       PROCEDURE DIVISION
           USING FORM-PATHNAME CYSCAN-BUF EXIT-PATHNAME.

      ****************************
      *      MAIN PROGRAM MODULE
      ****************************
       MAIN-PROGRAM SECTION.

           PERFORM SQL-CONNECT.

       COPY "LIBGB/CHKSEC.CPY".

      D    STOP MESS.
           MOVE "XX" TO CYSCAN-STATUS.
           MOVE " " TO VDU-WIMENU-SELECTION.

           MOVE "CYSCAN" TO VDU-FORM-NAME.

           DISPLAY FLOATING GRAPHICAL WINDOW
                   AT LINE CYSCAN-BEGIN-Y
                   AT COL  CYSCAN-BEGIN-X
                   SIZE    CYSCAN-COLUMNS + VDU-WIN-W-MOD
                   LINES   CYSCAN-LINES + VDU-WIN-H-MOD
           COLOR IS VDU-WCOLOR-SCAN,CONTROL VALUE IS VDU-WCTRL-SCAN
                   CELL SIZE IS LABEL FONT
                   BLANK SCREEN
                   HANDLE  IS CYSCAN-WINDOW-HANDLE.

           PERFORM FORM-RESET.

       BEGIN-BUILD-PAGENO.
           MOVE FIRST-FIELD TO FLDSEL-FIELD.
           MOVE 1 TO PAGE-COUNT ONE.
           MOVE 0 TO CURRENT-PAGE.
           PERFORM OPEN-CD1-FILE.
           MOVE " " TO PAGE-KEY(PAGE-COUNT).

       START-BUILD-PAGENO.
           ADD ONE TO CURRENT-PAGE.
           IF CURRENT-PAGE < 1
              MOVE FIRST-FIELD TO FLDSEL-FIELD
              MOVE 1 TO CURRENT-PAGE.
           IF CURRENT-PAGE > PAGE-COUNT
              MOVE FIRST-FIELD TO FLDSEL-FIELD
              MOVE 1 TO CURRENT-PAGE.

           MOVE " "               TO DISPLAY-BUF.
           MOVE CYSCAN-KEY-STATE  TO D-STATE.
           MOVE CYSCAN-ST-DESC    TO D-STATE-DESC.
           MOVE 1 TO SUB.

           MOVE "CY"                   TO CD-TYPE.
           MOVE CYSCAN-KEY-STATE       TO CD-CY-STATE.
           MOVE PAGE-KEY(CURRENT-PAGE) TO CD-CY-CITY.
           MOVE SPACES                 TO CD-CY-FILLER.
           PERFORM START-CD1-FILE.

       BUILD-PAGENO.
           PERFORM READ-CD1-FILE-NEXT.
           IF IO-BAD
              GO TO DISPLAY-PAGE.

           IF CD-TYPE NOT = "CY" OR CD-CY-STATE NOT = CYSCAN-KEY-STATE
              MOVE 9 TO IO-FG
              GO TO DISPLAY-PAGE.

           IF SUB > 9
              GO TO DISPLAY-PAGE.

           MOVE CD-CY-CITY      TO D-KEY(SUB).
           MOVE CD-DESC         TO D-CITY(SUB).
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
                    MOVE CD-CY-CITY TO PAGE-KEY(PAGE-COUNT)
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

           MOVE FLDSEL-KEY TO CYSCAN-KEY-CITY.
           MOVE "00"       TO CYSCAN-STATUS.

           GO TO END-ROUTINE.


       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/FLDSEL.CPY".
       COPY "LIBGB/DATER.CPY".

      *************************************************
       FORM-RESET SECTION.
      *************************************************
           DISPLAY CYSCAN-SCREEN UPON CYSCAN-WINDOW-HANDLE.
           MOVE CYSCAN-FORM-FIELDS TO WR-BUF.
           MOVE CYSCAN-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      *************************************************
       SETUP-LINK-INFO SECTION.
      *************************************************
           MOVE CYSCAN-HELP--FILE TO HELP-LINK-FILE.
           MOVE CYSCAN-HELP--DIR  TO HELP-LINK-DIR.

      *************************************************
       VDU-CONVERT-FIELD SECTION.
      *************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBWI/CYSCAN_EVA.CPY".
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

      **************************
       IO-ROUTINES SECTION.
      **************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/DECLARE_SQL.CPY".
           PERFORM CLOSE-CD1-FILE.
       COPY "LIBLP/LPCDGS_SQL.CPY".
       COPY "LIBLP/LPCD1RN.CPY".
       COPY "LIBGB/FERRORS.CPY".

      **************************
       END-ROUTINE SECTION.
      **************************
           PERFORM CLOSE-FILES.
       EXIT-PROG.
       STOP-RUN.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY CYSCAN-SCREEN.
           DESTROY CYSCAN-WINDOW-HANDLE.
           EXIT PROGRAM.
