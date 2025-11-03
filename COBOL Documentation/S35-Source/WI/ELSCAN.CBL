       IDENTIFICATION DIVISION.
       PROGRAM-ID.      ELSCAN.
      ******************************************************************
      *
      *  EXTENDED LATE CHARGE CODE FILE WINDOW SCAN
      *       CALLED FROM : LP/LONIJ0, LONIN0, XONPC0, LONPC0, LONPB0
      *                     LP/ZONPC0, XONIN0, RESCHD, MN/DLMAN1,
      *                     MN/ELMAIN, MN/LNMAN3, MN/PDPG03
      *
      *=================================================================
      * REV:
      * JTG 050797 ADDED LOGIC FOR CD-ELUNITPER-CD
      *
      * KEC 2016.0329 {PD#00003} PARADATA <A30> [JAY CISZEWSKI]
      *         CHANGED ELSCAN WORKERS TO MATCH CD1-KEY FOR EL LAYOUT.
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
           "@(#)WI/ELSCAN.CBL A32 01/24/19-11:55:49 >".
       COPY "LIBLP/LP01CD.CPY".
       COPY "LIBLP/LP01CD_SQL.CPY".
       COPY "LIBLP/LPWSCD.CPY".

       01  ELSCAN-LINES            PIC 9(02)     VALUE 16.
       01  ELSCAN-COLUMNS          PIC 9(02)     VALUE 72.
       01  ELSCAN-BEGIN-Y          PIC 9(02)     VALUE 5.
       01  ELSCAN-BEGIN-X          PIC 9(02)     VALUE 3.
       01  ERRCD                   PIC X      VALUE SPACES.
       01  ONE                     PIC S9     VALUE 1.
       01  SUB                     PIC 99.

       01  FLDSEL-FIELD            PIC X(4)   VALUE "010.".
       01  FLDSEL-STAT             PIC XX.
       01  FLDSEL-LIST.
           03  FILLER              PIC X(36)  VALUE
           "010 020 030 040 050 060 070 080 090.".
       01  FLDSEL-BUF.
           05  FLDSEL-KEY          PIC 9(2).
           05  FILLER              PIC X(04).
           05  FLDSEL-NAME         PIC X(70).

       01  FIRST-FIELD             PIC X(4)  VALUE "010.".
       01  LAST-FIELD              PIC X(4)  VALUE "090.".

       01  DISPLAY-LIST.
           03  FILLER              PIC X(51)  VALUE
           "010 020 030 040 050 060 070 080 090 PGNO ENDMORE:F.".
       01  DISPLAY-BUF.
           03  D-LINE              OCCURS 9 TIMES.
               05  D-KEY           PIC X(4).
               05  FILLER          PIC XXX.
               05  D-ELFRMLA       PIC X.
               05  FILLER          PIC XXXXXX.
               05  D-ELTYPE        PIC X.
               05  FILLER          PIC XXX.
               05  D-ELRATE        PIC ZZZ.ZZZZ.
               05  FILLER          PIC XXX.
               05  D-ELMINCHG      PIC ZZ.ZZ.
               05  FILLER          PIC X.
               05  D-ELMAXAMT      PIC ZZZZ.ZZ.
               05  FILLER          PIC XXXXXX.
               05  D-ELGRACE       PIC Z9.
               05  FILLER          PIC XX.
               05  D-ELYRTYPE      PIC 999.
               05  FILLER          PIC XXX.
               05  D-ELDELFAC      PIC 999.
               05  FILLER          PIC XXXX.
               05  D-ELUNITPER-CD  PIC X.
           03  D-PAGENO            PIC ZZ9.
           03  D-END-MORE          PIC X(7).

       01  CURRENT-PAGE            PIC S999.
       01  PAGE-COUNT              PIC 999.
       01  MAX-PAGES               PIC 999    VALUE 50.
       01  PAGE-KEY-01.
           03  PAGE-KEY-WORK       OCCURS 50 TIMES.
               05  PAGE-KEY        PIC XXX.


       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       01  ELSCAN-WINDOW-HANDLE    PIC X(10).
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/FLDSELW.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBWI/ELSCAN_DEF.CPY".
       COPY "LIBWI/ELSCAN_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".

       COPY "LIBGB/GETFMW.CPY".
       COPY "LIBWI/ELSCANW.CPY".

       SCREEN SECTION.
       COPY "LIBWI/ELSCAN_SCN.CPY".

       PROCEDURE DIVISION
           USING FORM-PATHNAME ELSCAN-BUF EXIT-PATHNAME.

      ********************************
       MAIN-PROGRAM SECTION.
      ********************************

           PERFORM SQL-CONNECT.

       COPY "LIBGB/CHKSEC.CPY".

           MOVE "XX" TO ELSCAN-STATUS.
           MOVE " " TO VDU-WIMENU-SELECTION.

           MOVE "ELSCAN" TO VDU-FORM-NAME.

           DISPLAY FLOATING GRAPHICAL WINDOW
                   AT LINE ELSCAN-BEGIN-Y
                   AT COL  ELSCAN-BEGIN-X
                   SIZE    ELSCAN-COLUMNS + VDU-WIN-W-MOD
                   LINES   ELSCAN-LINES + VDU-WIN-H-MOD
           COLOR IS VDU-WCOLOR-SCAN,CONTROL VALUE IS VDU-WCTRL-SCAN
                   CELL SIZE IS LABEL FONT
                   BLANK SCREEN
                   HANDLE  IS ELSCAN-WINDOW-HANDLE.

           PERFORM FORM-RESET.

           MOVE "S  A300TITLE." TO VDU.
           MOVE " LATE CHARGE CODE SELECTION" TO VDUBUF.
           PERFORM SCREENIO.


       BEGIN-BUILD-PAGENO.
           MOVE FIRST-FIELD TO FLDSEL-FIELD.
           MOVE 1 TO PAGE-COUNT ONE.
           MOVE 0 TO CURRENT-PAGE.

           PERFORM OPEN-CD1-FILE.
           MOVE SPACES TO PAGE-KEY(PAGE-COUNT).
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
           MOVE ELSCAN-TYPE            TO CD-TYPE.
           MOVE PAGE-KEY(CURRENT-PAGE) TO CD-CODE.
           PERFORM START-CD1-FILE.

      *************************************
      *    BUILD PAGE
      *************************************
       BUILD-PAGENO.
           PERFORM READ-CD1-FILE-NEXT.
           IF IO-FG NOT = 0 OR SUB > 9
              GO TO DISPLAY-PAGE.

           IF CD-TYPE NOT = ELSCAN-TYPE
              MOVE 9 TO IO-FG
              GO TO DISPLAY-PAGE.

           MOVE CD-CODE         TO D-KEY(SUB).
           MOVE CD-ELFRMLA      TO D-ELFRMLA(SUB).
           MOVE CD-ELTYPE       TO D-ELTYPE(SUB).
           MOVE CD-ELRATE       TO D-ELRATE(SUB).
           MOVE CD-ELMINCHG     TO D-ELMINCHG(SUB).
           MOVE CD-ELMAXAMT     TO D-ELMAXAMT(SUB).
           MOVE CD-ELGRACE      TO D-ELGRACE(SUB).
           MOVE CD-ELYRTYPE     TO D-ELYRTYPE(SUB).
           MOVE CD-ELDELFAC     TO D-ELDELFAC(SUB).
           MOVE CD-ELUNITPER-CD TO D-ELUNITPER-CD(SUB).
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
                    MOVE CD-CODE TO PAGE-KEY(PAGE-COUNT)
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

           MOVE FLDSEL-KEY TO ELSCAN-CODE.
           MOVE "00"       TO ELSCAN-STATUS.

           GO TO END-ROUTINE.

       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/FLDSEL.CPY".

      *************************************************
       FORM-RESET SECTION.
      *************************************************
           DISPLAY ELSCAN-SCREEN UPON ELSCAN-WINDOW-HANDLE.
           MOVE ELSCAN-FORM-FIELDS TO WR-BUF.
           MOVE ELSCAN-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      *************************************************
       SETUP-LINK-INFO SECTION.
      *************************************************
           MOVE ELSCAN-HELP--FILE TO HELP-LINK-FILE.
           MOVE ELSCAN-HELP--DIR  TO HELP-LINK-DIR.

      *************************************************
       VDU-CONVERT-FIELD SECTION.
      *************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBWI/ELSCAN_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ******************************
       MISC-PARAGRAPHS SECTION.
      ******************************
       COPY "LIBGB/ACCESS.CPY".
       SEND-LEGEND.
           MOVE "S  A600WSEL:D." TO VDU.
           MOVE MESS TO VDUBUF.
           PERFORM SCREENIO.


      ******************************
       IO-ROUTINES SECTION.
      ******************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/DECLARE_SQL.CPY".
           PERFORM CLOSE-CD1-FILE.
       COPY "LIBLP/LPCDGS_SQL.CPY".
       COPY "LIBLP/LPCD1RN.CPY".

       COPY "LIBGB/FERRORS.CPY".
      ******************************
       END-ROUTINE SECTION.
      ******************************
           PERFORM CLOSE-FILES.
       EXIT-PROG.
       STOP-RUN.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY ELSCAN-SCREEN.
           DESTROY ELSCAN-WINDOW-HANDLE.
           EXIT PROGRAM.
