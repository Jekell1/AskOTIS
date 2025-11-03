       IDENTIFICATION DIVISION.
       PROGRAM-ID.      ADONSC.
      **********************************************************
      *  WINDOW LOAN INSURANCE / FEE ADDON SCAN
      *
      * CALLED FROM ADONMN, WHICH IS CALLED FROM LONIN0 XONIN0
      *
      * REV:
      *  BAH 160125 SPLIT OUT LTFILE, PD0003
      *  BAH 170406 ACTIVATE 8 DIGIT DATES, PD0006
      *   CS 190104 GLOBAL LTIFILE
      *  BAH 190218 CHANGED LTI-INSTRM TO LTI-INSEFF
      **********************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       I-O-CONTROL.
       DATA DIVISION.
       FILE SECTION.


      ************************************
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)WI/ADONSC.CBL S35 04/22/24-08:07:44 >".
       COPY "LIBLP/LP01LTI.CPY".
       COPY "LIBLP/LP01LTI_SQL.CPY".
       COPY "LIBLP/LPWSLTI.CPY".

       01  ADONSC-LINES              PIC 99  VALUE 15.
       01  ADONSC-COLUMNS            PIC 99  VALUE 78.
       01  ADONSC-BEGIN-Y            PIC 99  VALUE 8.
       01  ADONSC-BEGIN-X            PIC 99  VALUE 1.
       01  ERRCD                   PIC X      VALUE SPACES.
       01  ONE                     PIC S9     VALUE 1.
       01  SUB                     PIC 99.
       01  INS-TABLE               PIC X(10)  VALUE "CLAHPPO1O2".
       01  INSTYPE                 REDEFINES INS-TABLE.
           03  INS-TYPE            OCCURS 5 TIMES
                                   PIC XX.
       01  FLDSEL-STAT             PIC XX.
       01  FLDSEL-FIELD            PIC X(4)   VALUE "010.".
       01  FLDSEL-LIST.
           03  FILLER              PIC X(36)  VALUE
           "010 020 030 040 050 060 070 080.".
       01  FLDSEL-BUF.
           05  FLDSEL-KEY          PIC 9(3).
           05  FILLER              PIC X(72).

       01  FIRST-FIELD             PIC X(4)  VALUE "010.".
       01  LAST-FIELD              PIC X(4)  VALUE "080.".

       01  DISPLAY-LIST.
           03  FILLER              PIC X(51)  VALUE
           "010 020 030 040 050 060 070 080 PGNO ENDMORE:F.".
       01  DISPLAY-BUF.
           03  D-LINE              OCCURS 8 TIMES.
               05  D-KEY           PIC 9(3).
               05  FILLER          PIC XX.
               05  D-TRCD          PIC XX.
               05  FILLER          PIC XXX.
               05  D-INS           PIC X(3).
               05  FILLER          PIC XX.
               05  D-INSCOMP       PIC Z9.
               05  FILLER          PIC XXX.
               05  D-INSOURS       PIC X.
               05  FILLER          PIC XX.
               05  D-INSEFF        PIC 99/99/99.
               05  FILLER          PIC XX.
               05  D-INSEXP        PIC 99/99/99.
               05  FILLER          PIC X.
               05  D-INSCOVR       PIC ZZZZZ9.99-.
               05  FILLER          PIC XX.
               05  D-INSPREM       PIC ZZZ9.99-.
               05  FILLER          PIC XX.
               05  D-INSCOMM       PIC ZZZ9.99-.
               05  FILLER          PIC XXX.
               05  D-INS-REBATE    PIC X.
           03  D-PAGENO            PIC ZZ9.
           03  D-END-MORE          PIC X(7).

       01  CURRENT-PAGE            PIC S999.
       01  PAGE-COUNT              PIC 999.
       01  MAX-PAGES               PIC 999    VALUE 5.
       01  PAGE-KEY-01.
           03  PAGE-KEY            OCCURS 5 TIMES
                                   PIC 9(3).

       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/FLDSELW.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBWI/ADONSC_DEF.CPY".
       COPY "LIBWI/ADONSC_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       01  ADONSC-WINDOW-HANDLE    PIC X(10).

       COPY "LIBGB/GETFMW.CPY".
       COPY "LIBWI/ADONSCW.CPY".

       SCREEN SECTION.
       COPY "LIBWI/ADONSC_SCN.CPY".

       PROCEDURE DIVISION
           USING FORM-PATHNAME ADONSC-BUF EXIT-PATHNAME.

      ********************************
       MAIN-PROGRAM SECTION.
      ********************************

           PERFORM SQL-CONNECT.

       COPY "LIBGB/CHKSEC.CPY".

           MOVE "XX" TO ADONSC-STATUS.
           MOVE " " TO VDU-WIMENU-SELECTION.

           MOVE "ADONSC" TO VDU-FORM-NAME.

           DISPLAY FLOATING GRAPHICAL WINDOW
                   AT LINE ADONSC-BEGIN-Y
                   AT COL  ADONSC-BEGIN-X
                   SIZE    ADONSC-COLUMNS + VDU-WIN-W-MOD
                   LINES   ADONSC-LINES + VDU-WIN-H-MOD
           COLOR IS VDU-WCOLOR-TCLP,CONTROL VALUE IS VDU-WCTRL-TCLP
                   CELL SIZE IS LABEL FONT
                   BLANK SCREEN
                   HANDLE  IS ADONSC-WINDOW-HANDLE.

           PERFORM FORM-RESET.

       ENTER-SCANKEY-CONT.
           MOVE FIRST-FIELD TO FLDSEL-FIELD.
           MOVE 1 TO PAGE-COUNT ONE.
           MOVE 0 TO CURRENT-PAGE.

           PERFORM OPEN-LTI1-FILE.

           MOVE 1 TO PAGE-KEY(PAGE-COUNT).
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
           MOVE LTI-PATH-OWNBR         TO LTI-BRNO.
           MOVE ADONSC-ACCTNO          TO LTI-ACCTNO.
           MOVE PAGE-KEY(CURRENT-PAGE) TO LTI-SEQNO.

           PERFORM START-LTI1-FILE.

      *************************************
      *    BUILD PAGE
      *************************************
       BUILD-PAGENO.
           PERFORM READ-LTI1-FILE-NEXT.
           IF IO-FG NOT = 0
              GO TO DISPLAY-PAGE.

           IF (LTI-BRNO NOT = LTI-PATH-OWNBR) OR
               (LTI-ACCTNO NOT = ADONSC-ACCTNO)
              MOVE 9 TO IO-FG
              GO TO DISPLAY-PAGE.

           IF SUB > 8
              GO TO DISPLAY-PAGE.

           MOVE LTI-SEQNO TO D-KEY(SUB).
           MOVE LTI-TRCD  TO D-TRCD(SUB).
           IF LTI-INSCOMP NOT = 0
              MOVE INS-TYPE(LTI-INS-TYPE) TO D-INS(SUB)
              MOVE LTI-INSCOMP TO D-INSCOMP(SUB)
              MOVE LTI-INSOURS TO D-INSOURS(SUB)

              MOVE LTI-INSEFF-DATE TO DATE-YYYYMMDD
              PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
              MOVE DATE-MMDDYY TO D-INSEFF(SUB)

              MOVE LTI-INSEXP-DATE TO DATE-YYYYMMDD
              PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
              MOVE DATE-MMDDYY TO D-INSEXP(SUB)

              MOVE LTI-INSPREM TO D-INSPREM(SUB)
              MOVE LTI-INSCOVR TO D-INSCOVR(SUB)
              MOVE LTI-INSCOMM TO D-INSCOMM(SUB)
              MOVE LTI-INS-REBATE TO D-INS-REBATE(SUB)
           ELSE
              MOVE "FEE" TO D-INS(SUB)
              MOVE LTI-INSFEE(1) TO D-INSPREM(SUB)
              MOVE LTI-INSFEE(2) TO D-INSCOMM(SUB).

           ADD 1 TO SUB.
           GO TO BUILD-PAGENO.

      *************************************
      *    DISPLAY PAGE
      *************************************
       DISPLAY-PAGE.
           IF IO-FG NOT = 0
              IF SUB = 1
                 MOVE "NO RECORDS FOUND!" TO MESS
                 PERFORM SEND-LEGEND
                 MOVE DISPLAY-BUF TO WR-BUF
                 MOVE DISPLAY-LIST TO WR-LIST
                 PERFORM CALL-WRFORM
                 PERFORM ALARM
                 MOVE "E  A010WSEL." TO VDU
                 PERFORM SCREENIO
                 GO TO END-ROUTINE.
           MOVE "MORE..." TO D-END-MORE.
           IF CURRENT-PAGE = PAGE-COUNT
              IF IO-FG = 0
                 IF PAGE-COUNT < MAX-PAGES
                    ADD 1 TO PAGE-COUNT
                    MOVE LTI-SEQNO TO PAGE-KEY(PAGE-COUNT)
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
           MOVE " F7-EXIT, SELECT AN ADDON RECORD (RTN):" TO MESS.
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

           MOVE FLDSEL-KEY TO ADONSC-KEY.
           MOVE "00"       TO ADONSC-STATUS.

           IF FLDSEL-STAT = "1<"
              PERFORM MORE-INFO
              GO TO SELECT-FIELD-FROM-DISPLAY.

           GO TO END-ROUTINE.

       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/FLDSEL.CPY".

      *************************************************
       FORM-RESET SECTION.
      *************************************************
           DISPLAY ADONSC-SCREEN UPON ADONSC-WINDOW-HANDLE.
           MOVE ADONSC-FORM-FIELDS TO WR-BUF.
           MOVE ADONSC-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      *************************************************
       SETUP-LINK-INFO SECTION.
      *************************************************
           MOVE ADONSC-HELP--FILE TO HELP-LINK-FILE.
           MOVE ADONSC-HELP--DIR  TO HELP-LINK-DIR.

      *************************************************
       VDU-CONVERT-FIELD SECTION.
      *************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBWI/ADONSC_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      *********************************
       MISC-ROUTINES SECTION.
      *********************************
       COPY "LIBGB/ACCESS.CPY".
       SEND-LEGEND.
           MOVE "S  A600WSEL:D." TO VDU.
           MOVE MESS TO VDUBUF.
           PERFORM SCREENIO.
       MORE-INFO.
           MOVE SPACES TO MESS.

      *********************************
       IO-ROUTINES SECTION.
      *********************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/DECLARE_SQL.CPY".
           PERFORM CLOSE-LTI1-FILE.
       COPY "LIBLP/LPLTIGS_SQL.CPY".
       COPY "LIBLP/LPLTI1RN.CPY".
       COPY "LIBGB/FERRORS.CPY".

      *********************************
       END-ROUTINE SECTION.
      *********************************
           PERFORM CLOSE-FILES.
       EXIT-PROG.
       STOP-RUN.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY ADONSC-SCREEN.
           DESTROY ADONSC-WINDOW-HANDLE.
           EXIT PROGRAM.
