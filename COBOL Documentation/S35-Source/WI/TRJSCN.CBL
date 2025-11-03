       IDENTIFICATION DIVISION.
       PROGRAM-ID.      TRJSCN.
      *****************************************************************
      *
      *  SCAN TRJFILE - BRANCH TRANSFER
      *
      *  CALLED FROM: TRJMN*
      *
      * REV.
      * BAH 2019.0508 DO NOT ENTER BRANCH, FORCE BRANCH YOU ARE IN. QA0017
      * BAH 20191007 REMOVED ACCESS-CALL TRJFILE
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
       77  SCCS-IDENTIFIER PIC X(37) VALUE
           "@(#)WI/TRJSCN J35 08/18/23-13:59:58 >".

      *****************************************************************
      *    W O R K E R S
      *****************************************************************
       COPY "LIBLP/LP01BW.CPY".
       COPY "LIBLP/LP01BW_SQL.CPY".
       COPY "LIBLP/LPWSBW.CPY".
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBLP/LP01TRJ.CPY".
       COPY "LIBLP/LP01TRJ_SQL.CPY".
       COPY "LIBLP/LPWSTRJ.CPY".

       01  NAME-WORKER              PIC X(34).
       01  FIRST-NAME               PIC X(16).

       01  TRJSCN-LINES          PIC 99  VALUE 17.
       01  TRJSCN-COLUMNS        PIC 99  VALUE 80.
       01  TRJSCN-BEGIN-Y        PIC 99  VALUE 3.
       01  TRJSCN-BEGIN-X        PIC 99  VALUE 1.

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
               05  DL-CLASS     PIC 99.
               05  DL-DASH1     PIC X.
               05  DL-NUMBER    PIC 9(6).
               05  DL-DASH2     PIC X.
               05  DL-SPCLASS   PIC 99.
               05  FILLER       PIC X(9).
               05  DL-NAME      PIC X(20).
               05  FILLER       PIC X.
               05  DL-SSNO      PIC 999/99/9999.
      * TRAMT STARTS IN 53
               05  DL-TRAMT     PIC ZZZZZ9.99-.
           03  D-PAGENO             PIC ZZ9.
           03  D-END-MORE           PIC X(7).
           03  D-HEAD               PIC X(76).


       01  TRJ-HEAD.
           03  FILLER               PIC X(39) VALUE
               "  DATE  CODE ACCTNO               NAME ".
           03  FILLER               PIC X(37) VALUE
               "                SOC SEC #      AMOUNT".
       01  ALL-HEAD.
           03  FILLER               PIC X(35) VALUE
               "  DATE  CODE                       ".
           03  FILLER               PIC X(41) VALUE
               "                                   AMOUNT".

       01  CURRENT-PAGE             PIC S999.
       01  PAGE-COUNT               PIC 999.
       01  MAX-PAGES                PIC 999    VALUE 250.

       01  PAGE-KEY-TABLE.
           03  PAGE-KEY             PIC X(28) OCCURS 250 TIMES.
       01  KEY-SELECT-TABLE.
           03  SELECT-KEY           PIC X(28) OCCURS 9 TIMES.

      *****************************************************************
       COPY "LIBWI/TRSCAN_CMD.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/DATERW.CPY".
       01 TRJSCN-WINDOW-HANDLE PIC X(10) VALUE SPACES.
       COPY "LIBGB/FLDSELW.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBWI/TRJSCN_DEF.CPY".
       COPY "LIBWI/TRJSCN_WKS.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/ACCESSW.CPY".

       COPY "LIBGB/GETFMW.CPY".
       COPY "LIBWI/TRJSCNW.CPY".

       SCREEN SECTION.
       COPY "LIBWI/TRJSCN_SCN.CPY".

       PROCEDURE DIVISION
            USING FORM-PATHNAME TRJSCN-BUF EXIT-PATHNAME.

      *****************************************************************
      *
      *    M A I N    P R O G R A M
      *
      *****************************************************************
       MAIN-PROGRAM SECTION.

           PERFORM SQL-CONNECT.

      D    STOP MESS.
       COPY "LIBGB/CHKSEC.CPY".
           MOVE "TRJSCN" TO VDU-FORM-NAME.
           MOVE FORM-PATHNAME TO FORM-PATH.

           DISPLAY FLOATING GRAPHICAL WINDOW
                    SCREEN LINE TRJSCN-BEGIN-Y * VDU-WIN-L-OFFSET
                    SCREEN COL TRJSCN-BEGIN-X * VDU-WIN-C-OFFSET
           COLOR IS VDU-WCOLOR-SCAN,CONTROL VALUE IS VDU-WCTRL-SCAN
                   CELL SIZE IS LABEL FONT
                   SIZE TRJSCN-COLUMNS + VDU-WIN-W-MOD
                   LINES TRJSCN-COLUMNS
                   BLANK SCREEN
                   HANDLE IS TRJSCN-WINDOW-HANDLE.
           PERFORM FORM-RESET.

           MOVE " "      TO VDU-WIMENU-SELECTION.
           MOVE "XX"     TO TRJSCN-STATUS.

           MOVE TRJSCN-BRNO     TO SCAN-BRANCH BR-NO.
           PERFORM GET-BR.
           IF IO-FG NOT = 0
              GO TO ENTER-BRANCH.

           MOVE "SNNN040BRANCH."  TO VDU.
           MOVE SCAN-BRANCH     TO VDUNUM0.
           PERFORM SCREENIO.

           MOVE "SNNN040MMYY."  TO VDU.
           MOVE TRJSCNW-MMYY    TO VDUNUM0.
           PERFORM SCREENIO.

           GO TO SKIP-ENTER-BRANCH.

       ENTER-BRANCH.
      *    MOVE LEGEND-01      TO MESS.
      *    PERFORM SEND-LEGEND.
      *    MOVE "ENNN040BRANCH." TO VDU.
      *    PERFORM SCREENIO.

      *    IF VDUSTATUS = "1>"
      *       GO TO END-ROUTINE.

      *    IF VDUSTATUS NOT = "00"
      *       GO TO ENTER-BRANCH.

      *    MOVE VDUNUM0 TO SCAN-BRANCH BR-NO.
      *    PERFORM GET-BR.
      *    IF IO-FG NOT = 0
      *       MOVE "INVALID BRANCH #, PLEASE RE-ENTER" TO MESS
      *       PERFORM SEND-MESS
      *       GO TO ENTER-BRANCH.

       ENTER-MONTH.
           MOVE LEGEND-02      TO MESS.
           PERFORM SEND-LEGEND.
           MOVE "ENNN040MMYY." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "1>"
              GO TO END-ROUTINE.

           IF VDUSTATUS NOT = "00"
              GO TO ENTER-MONTH.

           MOVE VDUNUM0 TO TRJSCNW-MMYY.
           IF TRJSCNW-MM < 1 OR > 12
              MOVE "INVALID MMYY, PLEASE RE-ENTER" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-MONTH.

       SKIP-ENTER-BRANCH.
           MOVE BR-MACHINE   TO TRJ-ALL-MACHINE.
           MOVE BR-DATA-PATH TO TRJ-ALL-DATA.
           PERFORM OPEN-TRJ1-FILE.

           MOVE "S  A010CODE." TO VDU.
           MOVE "J"            TO VDUBUF.
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
           MOVE 0           TO TJ-DATE TJ-BRNO TJ-CLASS TJ-ACCTNO
                               TJ-LAWCODE TJ-LINE.

           MOVE TRJSCNW-MMYY TO DATE-MMYY.
           MOVE DATE-MMYY-MM TO DATE-MMDDYY-MM.
           MOVE DATE-MMYY-YY TO DATE-MMDDYY-YY.
           MOVE 1            TO DATE-MMDDYY-DD.
           IF SCAN-DAY NOT = 99
              MOVE SCAN-DAY TO DATE-MMDDYY-DD.
           PERFORM CONVERT-MMDDYY-TO-YYYYMMDD.
           MOVE DATE-YYYYMMDD TO TJ-DATE.

           MOVE SCAN-BRANCH TO TJ-BRNO.
           MOVE TRJ1-KEY     TO PAGE-KEY(1).

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
           MOVE PAGE-KEY(CURRENT-PAGE) TO TRJ1-KEY.

           IF TJ-DATE = 0
              MOVE 19000101 TO TJ-DATE.

           PERFORM START-TRJ1-FILE-SCAN.

       BUILD-PAGENO.
           PERFORM READ-TRJ1-FILE-NEXT.
           IF IO-FG NOT = 0 OR SUB > 9 OR (TJ-BRNO NOT = TRJ-PATH-OWNBR)
              GO TO DISPLAY-PAGE.

           MOVE TJ-DATE TO DATE-YYYYMMDD.

           IF SCAN-DAY NOT = 99
              IF DATE-YYYYMMDD-DD NOT = SCAN-DAY
                 MOVE 9 TO IO-FG
                 GO TO DISPLAY-PAGE.

           IF DATE-YYYYMMDD-MM NOT = TRJSCNW-MM OR
               DATE-YYYYMMDD-YY NOT = TRJSCNW-YY
                 MOVE 9 TO IO-FG
                 GO TO DISPLAY-PAGE.

           MOVE TJ-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY  TO D-DATE(SUB).
           MOVE "J"          TO D-CODE(SUB).
           MOVE TRJ1-KEY     TO SELECT-KEY(SUB).

       TRJ-DISPLAY.
           MOVE TJ-CLASS   TO DL-CLASS(SUB).
           MOVE "-"        TO DL-DASH1(SUB) DL-DASH2(SUB).
           MOVE TJ-ACCTNO  TO DL-NUMBER(SUB).
           MOVE TJ-LAWCODE TO DL-SPCLASS(SUB).
           MOVE TJ-SSNO(1) TO BW-SSNO DL-SSNO(SUB).
           PERFORM FIND-BORROWER.
           INSPECT DL-SSNO(SUB) REPLACING ALL "/" BY "-".
           MOVE TJ-LNAMT   TO DL-TRAMT(SUB).

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
                    MOVE TRJ1-KEY TO PAGE-KEY(PAGE-COUNT)
                 ELSE
                    MOVE "MAXIMUM PAGES REACHED!" TO MESS
                    PERFORM SEND-LEGEND
                    MOVE "OVRFLOW" TO D-END-MORE
              ELSE
                 MOVE "* END *" TO D-END-MORE.

           MOVE CURRENT-PAGE TO D-PAGENO.

           MOVE TRJ-HEAD TO D-HEAD.

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

           IF (FLDSEL-STAT = "00") AND (TRJSCN-RETURN = "Y")
              MOVE SELECT-KEY(KEY-SUB) TO TRJSCN-TRJ1-KEY
              MOVE SCAN-BRANCH         TO TRJSCN-BRNO
              MOVE "00"                TO TRJSCN-STATUS
              GO TO END-ROUTINE.

           GO TO SELECT-FIELD-FROM-DISPLAY.

      *****************************************************************
       FIND-BORROWER SECTION.
      *****************************************************************
           PERFORM OPEN-BW1-FILE.
           PERFORM READ-BW1-FILE.

           IF IO-FG = 9
               MOVE "** NO BORROWER MASTER **" TO DL-NAME(SUB)
               GO TO END-FIND-BORROWER.

           MOVE BW-LNAME TO NAME-WORKER.
           INSPECT NAME-WORKER REPLACING FIRST "  " BY ",?".
           MOVE BW-FNAME TO FIRST-NAME.

           INSPECT NAME-WORKER REPLACING FIRST "                "
               BY FIRST-NAME.

           INSPECT NAME-WORKER REPLACING ALL "?" BY " ".
           MOVE NAME-WORKER TO DL-NAME(SUB).

       END-FIND-BORROWER.
           PERFORM CLOSE-BW1-FILE.

      *********************************************************
       FORM-RESET SECTION.
      *********************************************************
           DISPLAY SPACES UPON TRJSCN-WINDOW-HANDLE
                               LINE 0 COL 0 ERASE SCREEN.
           DISPLAY TRJSCN-SCREEN UPON TRJSCN-WINDOW-HANDLE.
           MOVE TRJSCN-FORM-FIELDS TO WR-BUF.
           MOVE TRJSCN-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      ********************************************************
       SETUP-LINK-INFO SECTION.
      ********************************************************
           MOVE TRJSCN-HELP--FILE TO HELP-LINK-FILE.
           MOVE TRJSCN-HELP--DIR TO HELP-LINK-DIR.

      ********************************************************
       VDU-CONVERT-FIELD SECTION.
      ********************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBWI/TRJSCN_EVA.CPY".
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
           PERFORM CLOSE-TRJ1-FILE.
           PERFORM CLOSE-BW1-FILE.
           PERFORM CLOSE-BR-FILE.
       COPY "LIBLP/LPTRJGS_SQL.CPY".
       COPY "LIBLP/LPBWGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBLP/LPTRJ1RN.CPY".
       COPY "LIBLP/LPBW1RN.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBGB/FERRORS.CPY".

      *****************************************************************
       END-ROUTINE SECTION.
      *****************************************************************
           PERFORM CLOSE-FILES.

       EXIT-PROG.

       STOP-RUN.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY TRJSCN-SCREEN.
           DESTROY TRJSCN-WINDOW-HANDLE.
           MOVE FORM-PATH TO FORM-PATHNAME.

           EXIT PROGRAM.
