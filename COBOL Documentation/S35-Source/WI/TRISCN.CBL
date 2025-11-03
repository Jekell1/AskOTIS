       IDENTIFICATION DIVISION.
       PROGRAM-ID.      TRISCN.
      *****************************************************************
      *
      *  SCAN TRIFILE - INSURANCE
      *
      *  CALLED FROM: TRIMN*
      *
      * REV.
      * BAH 2019.0507 DO NOT ENTER BRANCH, FORCE BRANCH YOU ARE IN. QA0017
      * BAH 20191007 REMOVED ACCESS-CALL TRIFILE
      * BAH 2025.0124 INVALID CHARACTER FOR CAST IF YOU RETURN THRU THE DAY
      *               MAKE SCAN-DAY 0 ACT LIKE F3 BY MOVING 99, AND
      *               VALIDATE MONTH BETWEEN 1 AND 12 S35Q-159
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
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)WI/TRISCN.CBL J35 08/17/23-14:16:45 >".

       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBLP/LP01TRI.CPY".
       COPY "LIBLP/LP01TRI_SQL.CPY".
       COPY "LIBLP/LPWSTRI.CPY".

       01  TRISCN-LINES          PIC 99  VALUE 17.
       01  TRISCN-COLUMNS        PIC 99  VALUE 80.
       01  TRISCN-BEGIN-Y        PIC 99  VALUE 3.
       01  TRISCN-BEGIN-X        PIC 99  VALUE 1.

       01  ERRCD                    PIC X    VALUE " ".
       01  ONE                      PIC S9   VALUE 1.
       01  SUB                      PIC 99.
       01  SCAN-BRANCH              PIC 9(4).
       01  SCAN-DAY                 PIC 99.

       01  FLDSEL-STAT              PIC XX.
       01  FLDSEL-FIELD             PIC X(4)   VALUE "010.".
       01  KEY-SUBSCRIPT REDEFINES FLDSEL-FIELD.
           03  KEY-SUB              PIC 99.
           03  FILLER               PIC XX.

       01  FLDSEL-LIST.
           03  FILLER               PIC X(36)  VALUE
           "010 020 030 040 050 060 070 080 090.".
       01  FLDSEL-BUF.
           03  FLDSEL-S.
               05  FLDSEL-S-SSNO      PIC X(11).
               05  FILLER             PIC X(38).
               05  FLDSEL-S-OWNBR     PIC 9(4).
               05  FILLER             PIC X(23).
           03  FLDSEL-N               REDEFINES FLDSEL-S.
               05  FLDSEL-N-NAME      PIC X(22).
               05  FILLER             PIC X.
               05  FLDSEL-N-SSNO      PIC X(11).
               05  FILLER             PIC X(15).
               05  FLDSEL-N-OWNBR     PIC 9(4).
               05  FILLER             PIC X(23).

       01  FIRST-FIELD              PIC X(4)  VALUE "010.".
       01  LAST-FIELD               PIC X(4)  VALUE "090.".

       01  DISPLAY-LIST.
           03  FILLER               PIC X(36)  VALUE
           "010 020 030 040 050 060 070 080 090 ".
           03  FILLER               PIC X(20)  VALUE
           "PGNO ENDMORE:F HEAD.".
       01  DISPLAY-BUF.
           03  D-LINE               OCCURS 9 TIMES.
               05  D-DATE           PIC 99/99/99.
               05  FILLER           PIC XX.
               05  D-CODE           PIC X.
               05  FILLER           PIC XX.
               05  DI-LINE.
                   07  DI-TYPE      PIC XXX.
                   07  FILLER       PIC X.
                   07  DI-PRODUCER  PIC 9(6).
                   07  FILLER       PIC XX.
                   07  DI-DRAFT     PIC 9(8).
                   07  FILLER       PIC X.
      * NAME STARTS IN 21
                   07  DI-NAME      PIC X(16).
                   07  FILLER       PIC X.
                   07  DI-TIME      PIC 99/99.
                   07  FILLER       PIC X.
                   07  DI-USERID    PIC X(08).
                   07  FILLER       PIC X.
                   07  DI-TRAMT     PIC ZZZZZ9.99-.

           03  D-PAGENO             PIC ZZ9.
           03  D-END-MORE           PIC X(7).
           03  D-HEAD               PIC X(76).


       01  TRI-HEAD.
           03  FILLER               PIC X(51) VALUE
               "  DATE  CODE TYP PRODUC    DRAFT  NAME ".
           03  FILLER               PIC X(24) VALUE
               "TIME   USERID     AMOUNT".

       01  ALL-HEAD.
           03  FILLER               PIC X(35) VALUE
               "  DATE  CODE                       ".
           03  FILLER               PIC X(41) VALUE
               "                                   AMOUNT".

       01  CURRENT-PAGE             PIC S999.
       01  PAGE-COUNT               PIC 999.
       01  MAX-PAGES                PIC 999    VALUE 250.

       01  PAGE-KEY-TABLE.
           03  PAGE-KEY             PIC X(17) OCCURS 250 TIMES.
       01  KEY-SELECT-TABLE.
           03  SELECT-KEY           PIC X(17) OCCURS 9 TIMES.

       COPY "LIBWI/TRSCAN_CMD.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/DATERW.CPY".
       01 TRISCN-WINDOW-HANDLE PIC X(10) VALUE SPACES.
       COPY "LIBGB/FLDSELW.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBWI/TRISCN_DEF.CPY".
       COPY "LIBWI/TRISCN_WKS.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/ACCESSW.CPY".

       COPY "LIBGB/GETFMW.CPY".
       COPY "LIBWI/TRISCNW.CPY".

       SCREEN SECTION.
       COPY "LIBWI/TRISCN_SCN.CPY".

       PROCEDURE DIVISION
            USING FORM-PATHNAME TRISCN-BUF EXIT-PATHNAME.

      *****************************************************************
      *
      *    M A I N    P R O G R A M
      *
      *****************************************************************
       MAIN-PROGRAM SECTION.

           PERFORM SQL-CONNECT.

      D    STOP MESS.

       COPY "LIBGB/CHKSEC.CPY".
           MOVE "TRISCN" TO VDU-FORM-NAME.
           MOVE FORM-PATHNAME TO FORM-PATH.

           DISPLAY FLOATING GRAPHICAL WINDOW
                    SCREEN LINE TRISCN-BEGIN-Y * VDU-WIN-L-OFFSET
                    SCREEN COL TRISCN-BEGIN-X * VDU-WIN-C-OFFSET
           COLOR IS VDU-WCOLOR-SCAN,CONTROL VALUE IS VDU-WCTRL-SCAN
                   CELL SIZE IS LABEL FONT
                   SIZE TRISCN-COLUMNS + VDU-WIN-W-MOD
                   LINES TRISCN-COLUMNS
                   BLANK SCREEN
                   HANDLE IS TRISCN-WINDOW-HANDLE.
           PERFORM FORM-RESET.

           MOVE " "      TO VDU-WIMENU-SELECTION.
           MOVE "XX"     TO TRISCN-STATUS.

           MOVE TRISCN-BRNO     TO SCAN-BRANCH BR-NO.
           PERFORM GET-BR.
           IF IO-FG NOT = 0
              GO TO ENTER-BRANCH.

           MOVE "SNNN040BRANCH."  TO VDU.
           MOVE SCAN-BRANCH     TO VDUNUM0.
           PERFORM SCREENIO.

           MOVE "SNNN040MMYY."  TO VDU.
           MOVE TRISCNW-MMYY    TO VDUNUM0.
           PERFORM SCREENIO.

           GO TO SKIP-ENTER-BRANCH.

       ENTER-BRANCH.

       ENTER-MONTH.
           MOVE LEGEND-02      TO MESS.
           PERFORM SEND-LEGEND.
           MOVE "ENNN040MMYY." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "1>"
              GO TO END-ROUTINE.

           IF VDUSTATUS NOT = "00"
              GO TO ENTER-MONTH.

           MOVE VDUNUM0 TO TRISCNW-MMYY.
           IF TRISCNW-MM < 1 OR > 12
              MOVE "INVALID MMYY, PLEASE RE-ENTER" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-MONTH.

       SKIP-ENTER-BRANCH.
           MOVE BR-MACHINE   TO TRI-ALL-MACHINE.
           MOVE BR-DATA-PATH TO TRI-ALL-DATA.
           PERFORM OPEN-TRI1-FILE.

           MOVE "S  A010CODE." TO VDU.
           MOVE "I"            TO VDUBUF.
           PERFORM SCREENIO.

       ENTER-DAY.
           MOVE LEGEND-04     TO MESS.
           PERFORM SEND-LEGEND.
           MOVE "ENNN020DAY." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "16"
              MOVE 99 TO SCAN-DAY
              GO TO BEGIN-SCAN.

           IF VDUSTATUS = "1>"
              GO TO END-ROUTINE.

           IF VDUSTATUS = "1U"
              GO TO ENTER-MONTH.

           IF VDUSTATUS NOT = "00"
              GO TO ENTER-DAY.

           MOVE VDUNUM0 TO SCAN-DAY.
           IF SCAN-DAY = 0
              MOVE 99 TO SCAN-DAY.

       BEGIN-SCAN.
           MOVE FIRST-FIELD TO FLDSEL-FIELD.
           MOVE 1           TO PAGE-COUNT ONE.
           MOVE 0           TO CURRENT-PAGE.
           MOVE 0           TO TI-KEY-DATE TI-BRNO.

           MOVE TRISCNW-MMYY TO DATE-MMYY.
           MOVE DATE-MMYY-MM TO DATE-MMDDYY-MM.
           MOVE DATE-MMYY-YY TO DATE-MMDDYY-YY.
           MOVE 1            TO DATE-MMDDYY-DD.
           IF SCAN-DAY NOT = 99
              MOVE SCAN-DAY TO DATE-MMDDYY-DD.
           PERFORM CONVERT-MMDDYY-TO-YYYYMMDD.
           MOVE DATE-YYYYMMDD TO TI-KEY-DATE.

           MOVE SCAN-BRANCH TO TI-BRNO.
           MOVE 0           TO TI-SEQ.
           MOVE TRI1-KEY    TO PAGE-KEY(1).

       START-BUILD-PAGENO.
           ADD ONE TO CURRENT-PAGE.

           IF CURRENT-PAGE < 1
              MOVE FIRST-FIELD TO FLDSEL-FIELD
              MOVE 1           TO CURRENT-PAGE.

           IF CURRENT-PAGE > PAGE-COUNT
              MOVE FIRST-FIELD TO FLDSEL-FIELD
              MOVE 1           TO CURRENT-PAGE.

           MOVE SPACES TO DISPLAY-BUF.
           MOVE 1      TO SUB.
           MOVE PAGE-KEY(CURRENT-PAGE) TO TRI1-KEY.

           IF TI-KEY-DATE = 0
              MOVE 19000101 TO TI-KEY-DATE.
           PERFORM START-TRI1-FILE-SCAN.

       BUILD-PAGENO.
           PERFORM READ-TRI1-FILE-NEXT.
           IF (IO-FG NOT = 0) OR (SUB > 9) OR
               (TI-BRNO NOT = TRI-PATH-OWNBR)
              GO TO DISPLAY-PAGE.

           MOVE TI-KEY-DATE TO DATE-YYYYMMDD.

           IF SCAN-DAY NOT = 99
              IF DATE-YYYYMMDD-DD NOT = SCAN-DAY
                 MOVE 9 TO IO-FG
                 GO TO DISPLAY-PAGE.

           IF DATE-YYYYMMDD-MM NOT = TRISCNW-MM OR
               DATE-YYYYMMDD-YY NOT = TRISCNW-YY
                 MOVE 9 TO IO-FG
                 GO TO DISPLAY-PAGE.

           MOVE TI-KEY-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY  TO D-DATE(SUB).
           MOVE "I"          TO D-CODE(SUB).
           MOVE TRI1-KEY     TO SELECT-KEY(SUB).

       TRI-DISPLAY.
           MOVE  TI-TYPE     TO DI-TYPE(SUB).
           MOVE  TI-PRODUCER TO DI-PRODUCER(SUB).
           MOVE  TI-DRAFT    TO DI-DRAFT(SUB).
           STRING TI-LNAME", ",TI-FNAME DELIMITED BY "   " INTO
                                DI-NAME(SUB).
      *  NUM-DATE 00HHMMSS
           MOVE  TI-POSTTIME TO NUM-DATE.
           MOVE NUM-MO TO NUM-DA.
           MOVE NUM-YR TO NUM-MO.
           MOVE 0 TO NUM-CCYY.
           MOVE NUM-DATE TO DI-TIME(SUB).
           INSPECT DI-TIME(SUB) REPLACING ALL "/" BY ":".

           MOVE  TI-USERID   TO DI-USERID(SUB).
           MOVE  TI-AMOUNT   TO DI-TRAMT(SUB).

           ADD 1 TO SUB.
           GO TO BUILD-PAGENO.

       DISPLAY-PAGE.
           IF IO-FG NOT = 0
              IF SUB = 1
                 MOVE "NO RECORDS FOUND!" TO MESS
                 PERFORM SEND-LEGEND
                 MOVE DISPLAY-BUF TO WR-BUF
                 MOVE DISPLAY-LIST TO WR-LIST
                 PERFORM CALL-WRFORM
                 GO TO ENTER-BRANCH.

           MOVE "MORE..." TO D-END-MORE.

           IF CURRENT-PAGE = PAGE-COUNT
              IF IO-FG = 0
                 IF PAGE-COUNT < MAX-PAGES
                    ADD 1 TO PAGE-COUNT
                    MOVE TRI1-KEY TO PAGE-KEY(PAGE-COUNT)
                 ELSE
                    MOVE "MAXIMUM PAGES REACHED!" TO MESS
                    PERFORM SEND-LEGEND
                    MOVE "OVRFLOW" TO D-END-MORE
              ELSE
                 MOVE "* END *" TO D-END-MORE.

           MOVE CURRENT-PAGE TO D-PAGENO.

           MOVE TRI-HEAD TO D-HEAD.

           MOVE DISPLAY-BUF TO WR-BUF.
           MOVE DISPLAY-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       SELECT-FIELD-FROM-DISPLAY.
           MOVE LEGEND-05 TO MESS.
           PERFORM SEND-LEGEND.
           PERFORM CALL-FLDSEL.
      * TEST UP ARROW:
           IF FLDSEL-STAT = "1U"
              IF CURRENT-PAGE = 1
                 PERFORM CALL-ALARM
                 GO TO SELECT-FIELD-FROM-DISPLAY
              ELSE
                 MOVE -1 TO ONE
                 MOVE LAST-FIELD TO FLDSEL-FIELD
                 GO TO START-BUILD-PAGENO.

      * TEST PAGE UP:
           IF FLDSEL-STAT = "1P"
              IF CURRENT-PAGE = 1
                 PERFORM CALL-ALARM
                 GO TO SELECT-FIELD-FROM-DISPLAY
              ELSE
                 MOVE -1 TO ONE
                 MOVE FIRST-FIELD TO FLDSEL-FIELD
                 GO TO START-BUILD-PAGENO.

      * TEST PAGE DOWN OR DOWN ARROW:
           IF FLDSEL-STAT = "1N" OR FLDSEL-STAT = "1D"
              IF CURRENT-PAGE NOT < PAGE-COUNT
                 PERFORM CALL-ALARM
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
              GO TO ENTER-DAY.
           IF NOT (FLDSEL-STAT = "00" OR "1<")
              PERFORM CALL-ALARM
              GO TO SELECT-FIELD-FROM-DISPLAY.

           IF (FLDSEL-STAT = "00") AND (TRISCN-RETURN = "Y")
              MOVE SELECT-KEY(KEY-SUB) TO TRISCN-TRI1-KEY
              MOVE SCAN-BRANCH         TO TRISCN-BRNO
              MOVE "00"                TO TRISCN-STATUS
              GO TO END-ROUTINE.

           GO TO SELECT-FIELD-FROM-DISPLAY.


      *********************************************************
       FORM-RESET SECTION.
      *********************************************************
           DISPLAY SPACES UPON TRISCN-WINDOW-HANDLE
                               LINE 0 COL 0 ERASE SCREEN.
           DISPLAY TRISCN-SCREEN UPON TRISCN-WINDOW-HANDLE.
           MOVE TRISCN-FORM-FIELDS TO WR-BUF.
           MOVE TRISCN-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM. 

       FORM-RESET-EXIT.
           EXIT.

      ********************************************************
       SETUP-LINK-INFO SECTION.
      ********************************************************
           MOVE TRISCN-HELP--FILE TO HELP-LINK-FILE.
           MOVE TRISCN-HELP--DIR TO HELP-LINK-DIR.

      ********************************************************
       VDU-CONVERT-FIELD SECTION.
      ********************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBWI/TRISCN_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

       COPY "LIBGB/FLDSEL.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/DATER.CPY".

      *****************************************************************
       MISC-PARAGRAPHS SECTION.
      *****************************************************************
       COPY "LIBGB/ACCESS.CPY".
       CALL-ALARM.
           DISPLAY OMITTED WITH BEEP.

       GET-BR.
           MOVE 0 TO IO-FG.
           PERFORM OPEN-BR-FILE.
           PERFORM READ-BR-FILE.
           PERFORM CLOSE-BR-FILE.

       SEND-LEGEND.
           MOVE "S  A600WSEL:D." TO VDU.
           MOVE MESS TO VDUBUF.
           PERFORM SCREENIO.

      *****************************************************************
       IO-ROUTINES SECTION.
      *****************************************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/DECLARE_SQL.CPY".
           PERFORM CLOSE-TRI1-FILE.
           PERFORM CLOSE-BR-FILE.
       COPY "LIBLP/LPTRIGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBLP/LPTRI1RN.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBGB/FERRORS.CPY".

      *****************************************************************
       END-ROUTINE SECTION.
      *****************************************************************
           PERFORM CLOSE-FILES.

       EXIT-PROG.
       STOP-RUN.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY TRISCN-SCREEN.
           DESTROY TRISCN-WINDOW-HANDLE.
           MOVE FORM-PATH TO FORM-PATHNAME.

           EXIT PROGRAM.
