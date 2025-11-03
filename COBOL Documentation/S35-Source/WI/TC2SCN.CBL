       IDENTIFICATION DIVISION.
       PROGRAM-ID.      TC2SCN.
      *****************************************************
      *  TRANSACTION CODE REFERENCE CODE SCAN
      *
      * REV:      
      *     2009.0408 SCAN OF TRANSACTION CODE /ASSOCIATED REFNO CODES
      *                                              PR#2353 LENDMARK
      * KEC 2016.0321 DID NOT CHANGE PAGE-KEY TO MATCH CD-TC-KEY.  IT APPEARS
      *               THAT IT DOES REALLY USE PAGE-KEY IN THE NORMAL WAY A
      *               SCAN WINDOW PROGRAM WORKS.  THIS WORKS AS A ONE-PAGE
      *               SCAN WINDOW THAT READS ONE CD-REC TO DISPLAY.
      * BAH 2019.0122 SPLIT CDFILE
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
           "@(#)WI/TC2SCN.CBL S35 02/23/24-11:42:17 >".
       COPY "LIBLP/LP01CD.CPY".
       COPY "LIBLP/LP01CD_SQL.CPY".
       COPY "LIBLP/LPWSCD.CPY".

       01  TC2SCN-LINES              PIC 99  VALUE 17.
       01  TC2SCN-COLUMNS            PIC 99  VALUE 27.
       01  TC2SCN-BEGIN-Y            PIC 99  VALUE 03.
       01  TC2SCN-BEGIN-X            PIC 99  VALUE 40.

       01  ERRCD                   PIC X VALUE " ".
       01  ONE                     PIC S9     VALUE 1.
       01  SUB                     PIC 99.

       01  FLDSEL-FIELD            PIC X(4)   VALUE "010.".
       01  FILLER REDEFINES  FLDSEL-FIELD.
           03  FLDSEL-ELE          PIC 9(2).
           03  FILLER              PIC X(2).

       01  FLDSEL-STAT             PIC XX.
       01  FLDSEL-LIST.
           03  FILLER              PIC X(40)  VALUE
           "010 020 030 040 050 060 070 080 090 100.".
       01  FLDSEL-BUF.
           05  FLDSEL-KEY          PIC X(5).

       01  FIRST-FIELD             PIC X(4)  VALUE "010.".
       01  LAST-FIELD              PIC X(4)  VALUE "100.".

       01  DISPLAY-LIST.
           03  FILLER              PIC X(55)  VALUE
           "010 020 030 040 050 060 070 080 090 100 PGNO ENDMORE:F.".
       01  DISPLAY-BUF.
           03  D-LINE              OCCURS 10 TIMES.
               05  D-KEY           PIC X(5).
           03  D-PAGENO            PIC ZZ9.
           03  D-END-MORE          PIC X(7).

       01  CURRENT-PAGE            PIC S999.
       01  PAGE-COUNT              PIC 999.
       01  MAX-PAGES               PIC 999    VALUE 10.
       01  PAGE-KEY-01.
           03  PAGE-KEY                OCCURS 10 TIMES
                                       PIC 99.


       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       01  TC2SCN-WINDOW-HANDLE PIC X(10).
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBWI/TC2SCN_DEF.CPY".
       COPY "LIBWI/TC2SCN_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/FLDSELW.CPY".

       COPY "LIBGB/GETFMW.CPY".
       COPY "LIBWI/TC2SCNW.CPY".

       SCREEN SECTION.
       COPY "LIBWI/TC2SCN_SCN.CPY".

       PROCEDURE DIVISION
           USING FORM-PATHNAME TC2SCN-BUF EXIT-PATHNAME.

      ****************************
      *       MAIN PROGRAM MODULE
      ****************************
       MAIN-PROGRAM SECTION.

           PERFORM SQL-CONNECT.

       COPY "LIBGB/CHKSEC.CPY".
      D    STOP MESS.
           MOVE " " TO VDU-WIMENU-SELECTION.

           MOVE "TC2SCN" TO VDU-FORM-NAME.

           DISPLAY FLOATING GRAPHICAL WINDOW
                   AT LINE TC2SCN-BEGIN-Y
                   AT COL  TC2SCN-BEGIN-X
                   SIZE    TC2SCN-COLUMNS + VDU-WIN-W-MOD
                   LINES   TC2SCN-LINES + VDU-WIN-H-MOD
           COLOR IS VDU-WCOLOR-TCLP,CONTROL VALUE IS VDU-WCTRL-TCLP
                   CELL SIZE IS LABEL FONT
                   BLANK SCREEN
                   HANDLE  IS TC2SCN-WINDOW-HANDLE.

           PERFORM FORM-RESET.

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
           MOVE PAGE-KEY(CURRENT-PAGE) TO CD1-KEY.


      *************************************
      *    BUILD PAGE
      *************************************
       BUILD-PAGENO.

           MOVE "TC"        TO CD-TYPE.
           MOVE TC2SCN-CODE TO CD-CODE.
           PERFORM READ-CD1-FILE.
           IF IO-FG NOT = 0 OR SUB > 9
              GO TO DISPLAY-PAGE.

           MOVE ZEROES TO SUB.

       BUILD-PAGENO-LOOP.
           ADD 1 TO SUB.
           IF ( SUB > 10 )
              MOVE 9 TO IO-FG
              GO TO DISPLAY-PAGE.

           MOVE CD-TC-REFNO(SUB) TO D-KEY(SUB).

           IF ( D-KEY(SUB) = SPACES )
              MOVE "SPACE" TO D-KEY(SUB)
              CALL "C$TOLOWER" USING D-KEY(SUB), VALUE 5
           ELSE
           IF ( D-KEY(SUB) = "~~~~~" )
              MOVE SPACES TO D-KEY(SUB).

           GO TO BUILD-PAGENO-LOOP.

      *    ADD 1 TO SUB.
      *    GO TO BUILD-PAGENO.

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

           MOVE CD-TC-REFNO(FLDSEL-ELE) TO TC2SCN-REFNO.

           GO TO END-ROUTINE.

       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/FLDSEL.CPY".
       COPY "LIBGB/DATER.CPY".

      *************************************************
       FORM-RESET SECTION.
      *************************************************
           DISPLAY TC2SCN-SCREEN UPON TC2SCN-WINDOW-HANDLE.
           MOVE TC2SCN-FORM-FIELDS TO WR-BUF.
           MOVE TC2SCN-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      *************************************************
       SETUP-LINK-INFO SECTION.
      *************************************************
           MOVE TC2SCN-HELP--FILE TO HELP-LINK-FILE.
           MOVE TC2SCN-HELP--DIR  TO HELP-LINK-DIR.

      *************************************************
       VDU-CONVERT-FIELD SECTION.
      *************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBWI/TC2SCN_EVA.CPY".
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

      *********************************
       IO-ROUTINES SECTION.
      *********************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/DECLARE_SQL.CPY".
           PERFORM CLOSE-CD1-FILE.
       COPY "LIBLP/LPCDGS_SQL.CPY".
       COPY "LIBLP/LPCD1RN.CPY".
       COPY "LIBGB/FERRORS.CPY".

      *********************************
       END-ROUTINE SECTION.
      *********************************
           PERFORM CLOSE-FILES.
       EXIT-PROG.
       STOP-RUN.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY TC2SCN-SCREEN.
           DESTROY TC2SCN-WINDOW-HANDLE.
           EXIT PROGRAM.

