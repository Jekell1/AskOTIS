       IDENTIFICATION DIVISION.
       PROGRAM-ID.      PWSCAN.
      ******************************************************************
      *
      *  PASSWORD FILE SCAN
      *
      * REV:
      *               REGACC PR#188
      * KEC 2015.1201 FIXED TO NOT USE PWSCANW-DIR ANYMORE.
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
           "@(#)WI/PWSCAN.CBL A30 12/01/15-12:38:44 >".

       COPY "LIBGB/GB01PW.CPY".
       COPY "LIBGB/GB01PW_SQL.CPY".
       COPY "LIBGB/GBWSPW.CPY".

       01  PWSCAN-LINES          PIC 99  VALUE 17.
       01  PWSCAN-COLUMNS        PIC 99  VALUE 72.
       01  PWSCAN-BEGIN-Y        PIC 99  VALUE 6.
       01  PWSCAN-BEGIN-X        PIC 99  VALUE 7.

       01  ERRCD                    PIC X      VALUE SPACES.
       01  ONE                      PIC S9     VALUE 1.
       01  SUB                      PIC 99.

       01  HOLD-PW1-KEY.
           03  HOLD-PW-PGM          PIC X(10).
           03  HOLD-PW-SEQ          PIC 9(03).

       01  FLDSEL-STAT              PIC XX.
       01  FLDSEL-FIELD             PIC X(4)   VALUE "010.".
       01  FLDSEL-FIELD-NUM        REDEFINES FLDSEL-FIELD.
           03  FLDSEL-FIELD-N       PIC 99.
           03  FILLER               PIC XX.
       01  FLDSEL-LIST.
           03  FILLER               PIC X(36)  VALUE
           "010 020 030 040 050 060 070 080 090.".
       01  FLDSEL-BUF.
           03  FILLER              PIC X(02).  
           03  FILLER              PIC XX.
           03  FLDSEL-PGM          PIC X(10).
           03  FILLER              PIC X.
           03  FLDSEL-SEQ          PIC 9(03).
           03  FILLER              PIC X(02).
           03  FLDSEL-DESC         PIC X(30).

       01  FIRST-FIELD              PIC X(4)  VALUE "010.".
       01  LAST-FIELD               PIC X(4)  VALUE "090.".

       01  DISPLAY-LIST.
           03  FILLER               PIC X(51)  VALUE
           "010 020 030 040 050 060 070 080 090 PGNO ENDMORE:F.".
       01  DISPLAY-BUF.
           03  D-LINE               OCCURS 9 TIMES.
               05  D-DETAIL.
                   07  FILLER         PIC X(02).
                   07  FILLER         PIC XX.
                   07  D-PGM          PIC X(10).
                   07  FILLER         PIC X.
                   07  D-SEQ          PIC 9(03).
                   07  FILLER         PIC X(02).
                   07  D-DESC         PIC X(30).
                   07  FILLER         PIC X(2).
                   07  D-REQ          PIC X(1).
                   07  FILLER         PIC X(3).
                   07  D-PROFILE      PIC X.
                   07  FILLER         PIC X(4).
                   07  D-EXC          PIC X.
                   07  FILLER         PIC X(4).
                   07  D-ASK          PIC X.
                   07  FILLER         PIC X(2).

           03  D-PAGENO             PIC ZZ9.
           03  D-END-MORE           PIC X(7).

       01  CURRENT-PAGE             PIC S999.
       01  PAGE-COUNT               PIC 999.
       01  MAX-PAGES                PIC 999    VALUE 100.

       01  PAGE-KEY-TABLE.
           03  PAGE-KEY OCCURS 100.
               05  PAGE-KEY-PGM     PIC X(10).
               05  PAGE-KEY-SEQ     PIC 9(03).

       COPY "LIBGB/PHON400.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/FLDSELW.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBWI/PWSCAN_DEF.CPY".
       COPY "LIBWI/PWSCAN_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       01  PWSCAN-WINDOW-HANDLE    PIC X(10).

       COPY "LIBGB/GETFMW.CPY".
       COPY "LIBWI/PWSCANW.CPY".

       SCREEN SECTION.
       COPY "LIBWI/PWSCAN_SCN.CPY".

       PROCEDURE DIVISION
           USING FORM-PATHNAME PWSCAN-BUF EXIT-PATHNAME.

      **************************************
      *      MAIN PROGRAM MODULE
      **************************************
       MAIN-PROGRAM SECTION.

           PERFORM SQL-CONNECT.

       COPY "LIBGB/CHKSEC.CPY".

      D    STOP MESS.
           MOVE " "  TO VDU-WIMENU-SELECTION.
           MOVE "XX" TO PWSCAN-STATUS.

           MOVE "PWSCAN" TO VDU-FORM-NAME.

           DISPLAY FLOATING GRAPHICAL WINDOW
                   AT LINE PWSCAN-BEGIN-Y
                   AT COL  PWSCAN-BEGIN-X
                   SIZE    PWSCAN-COLUMNS + VDU-WIN-W-MOD
                   LINES   PWSCAN-LINES + VDU-WIN-H-MOD
           COLOR IS VDU-WCOLOR-SCAN,CONTROL VALUE IS VDU-WCTRL-SCAN
                   CELL SIZE IS LABEL FONT
                   BLANK SCREEN
                   HANDLE  IS PWSCAN-WINDOW-HANDLE.

           PERFORM FORM-RESET.

           MOVE PWSCAN-PW1-KEY TO HOLD-PW1-KEY.

       TEST-PGM.
           IF PWSCANW-PGM = SPACES
              GO TO ENTER-PGM
           ELSE
              GO TO BEGIN-THE-SCAN.

      *************************************
      *    ENTER PASSWORD PROGRAM
      *************************************
       ENTER-PGM.
           MOVE "ENTER PASSWORD PROGRAM" TO MESS.
           PERFORM SEND-LEGEND.

           MOVE "E  A100PGM." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>"
              GO TO END-ROUTINE.

           IF VDUSTATUS = "16"
              MOVE SPACES TO HOLD-PW-PGM
              MOVE "S  A000PGM." TO VDU
              MOVE "ALL PROGS " TO VDUBUF
              PERFORM SCREENIO
              GO TO BEGIN-THE-SCAN.

           IF VDUSTATUS NOT = "00"
              GO TO ENTER-PGM.

           IF VDUBUF = SPACES
              MOVE "S  A100PGM." TO VDU
              MOVE HOLD-PW-PGM TO VDUBUF
              PERFORM SCREENIO.
           MOVE VDUBUF TO HOLD-PW-PGM.

      *------------------------------------------------------------
      *  BEGIN PASSWORD SCAN
      *------------------------------------------------------------
       BEGIN-THE-SCAN.
           MOVE FIRST-FIELD TO FLDSEL-FIELD.
           MOVE 1 TO PAGE-COUNT ONE.
           MOVE 0 TO CURRENT-PAGE.
           PERFORM OPEN-PW1-FILE.

           MOVE HOLD-PW-PGM TO PAGE-KEY-PGM(1).
           MOVE 0         TO PAGE-KEY-SEQ(1).

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
           MOVE PAGE-KEY(CURRENT-PAGE) TO PW1-KEY.
           PERFORM START-PW1-FILE.

      *************************************
      *    BUILD PAGE
      *************************************
       BUILD-PAGENO.
           PERFORM READ-PW1-FILE-NEXT.
           IF IO-FG NOT = 0 OR SUB > 9
              GO TO DISPLAY-PAGE.

           IF HOLD-PW-PGM NOT = SPACES
              IF PW-PGM NOT = HOLD-PW-PGM
                 GO TO BUILD-PAGENO.

           MOVE PW-PGM          TO D-PGM(SUB).
           MOVE PW-SEQ          TO D-SEQ(SUB).
           MOVE PW-DESC         TO D-DESC(SUB).
           MOVE PW-PASSYN       TO D-REQ(SUB).
           MOVE PW-USER-PROFILE TO D-PROFILE(SUB).
           MOVE PW-EXCEPTION-FG TO D-EXC(SUB).
           MOVE PW-ASK-FG       TO D-ASK(SUB).

           ADD 1 TO SUB.
           GO TO BUILD-PAGENO.

      *************************************
      *    DISPLAY PAGE
      *************************************
       DISPLAY-PAGE.
           IF IO-FG NOT = 0
              IF SUB = 1
                 MOVE "NO RECORDS FOUND!" TO D-DESC(1)
                 MOVE DISPLAY-BUF TO WR-BUF
                 MOVE DISPLAY-LIST TO WR-LIST
                 PERFORM CALL-WRFORM
                 IF PWSCANW-PGM = SPACES
                    GO TO ENTER-PGM 
                 ELSE
                    GO TO END-ROUTINE.

           MOVE "MORE..." TO D-END-MORE.
           IF CURRENT-PAGE = PAGE-COUNT
              IF IO-FG = 0
                 IF PAGE-COUNT < MAX-PAGES
                    ADD 1 TO PAGE-COUNT
                    MOVE PW1-KEY TO PAGE-KEY(PAGE-COUNT)
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
           MOVE "F1-PGM, ^-UP, V-DOWN, F7-EXIT, SELECT PASSWORD"
              TO MESS.
           PERFORM SEND-LEGEND.

           PERFORM CALL-FLDSEL.
      * TEST UP ARROW:
           IF FLDSEL-STAT = "1U"
              IF PWSCANW-PGM = SPACES
                 GO TO ENTER-PGM.   

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

           IF FLDSEL-STAT = "10"
              IF PWSCANW-PGM = SPACES 
                 GO TO ENTER-PGM.    

           IF NOT (FLDSEL-STAT = "00")
              PERFORM ALARM
              GO TO SELECT-FIELD-FROM-DISPLAY.

           MOVE FLDSEL-PGM TO PWSCANW-PGM.
           MOVE FLDSEL-SEQ TO PWSCANW-SEQ.

           MOVE "00"       TO PWSCAN-STATUS.

           GO TO END-ROUTINE.

       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/FLDSEL.CPY".

      *************************************************
       FORM-RESET SECTION.
      *************************************************
           DISPLAY PWSCAN-SCREEN UPON PWSCAN-WINDOW-HANDLE.
           MOVE PWSCAN-FORM-FIELDS TO WR-BUF.
           MOVE PWSCAN-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      *************************************************
       SETUP-LINK-INFO SECTION.
      *************************************************
           MOVE PWSCAN-HELP--FILE TO HELP-LINK-FILE.
           MOVE PWSCAN-HELP--DIR  TO HELP-LINK-DIR.

      *************************************************
       VDU-CONVERT-FIELD SECTION.
      *************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBWI/PWSCAN_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ******************************
       MISC-PARAGRAPHS SECTION.
      ******************************
       COPY "LIBGB/PHONEIO.CPY".
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
           PERFORM CLOSE-PW1-FILE.
       COPY "LIBGB/GBPWGS_SQL.CPY".
       COPY "LIBGB/GBPW1RN.CPY".
       COPY "LIBGB/FERRORS.CPY".

      ******************************
       END-ROUTINE SECTION.
      ******************************
           PERFORM CLOSE-FILES.
       EXIT-PROG.
       STOP-RUN.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY PWSCAN-SCREEN.
           DESTROY PWSCAN-WINDOW-HANDLE.
           EXIT PROGRAM.
