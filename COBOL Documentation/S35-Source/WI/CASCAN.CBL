       IDENTIFICATION DIVISION.
       PROGRAM-ID.      CASCAN.
      *****************************************************************
      *
      *  SCAN CAFILE - CASH DRAWER FILE SCAN
      *  CALLED FROM: LP/LONPL0, MN/CAMAIN
      *
      * REV.
      *  062498 BAH CHANGED CASCANW-TILLNO TO ALPHA
      *  090199 GLF 1: CHANGED CASCANW - MOVED WINDOW DOWN 3 LINES
      *             2: CHANGED CASCAN.CMD - MADE SEPARATE LEGENDS FOR EACH
      *                SCAN TYPE
      *             3: CHANGED CASCAN - MADE SURE PROPER LEGEND DISPLAYS FOR
      *                EACH ENTRY FIELD ACCORDING TO SCAN TYPE
      *             4: ALLOW ENTRY OF ZERO DATE ONLY IF KEY IS F3-ALL
      *             PER JTG
      *
      * 160307 BAH EXPANDED CA-REC PD0003
      * 170313 BAH ACTIVATE 8 DIGIT DATES, PD0006
      * BAH 20191007 REMOVED ACCESS-CALL CAFILE
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


      *****************************************************************
      *
      *    W O R K I N G - S T O R A G E - S E C T I O N
      *
      *****************************************************************
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)WI/CASCAN.CBL S34 07/12/21-09:56:20 >".

      *****************************************************************
      *
      *    W O R K E R S
      *
      *****************************************************************
       COPY "LIBLP/LP01CA.CPY".
       COPY "LIBLP/LP01CA_SQL.CPY".
       COPY "LIBLP/LPWSCA.CPY".
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
      *-----------------------------------------------------------------

       01  CASCAN-LINES             PIC 99  VALUE 17.
       01  CASCAN-COLUMNS           PIC 99  VALUE 78.
       01  CASCAN-BEGIN-Y           PIC 99  VALUE 6.
       01  CASCAN-BEGIN-X           PIC 99  VALUE 2.
       01  ERRCD                    PIC X    VALUE SPACES.

       01  ONE                      PIC S9     VALUE 1.
       01  EDIT-DATE                PIC 99/99/99.
       01  SUB                      PIC 99.
       01  SCAN-BRANCH              PIC 9(4).
       01  SCAN-DATE-YYYYMMDD       PIC 9(8).
       01  SCAN-TILLNO              PIC 99.

      ******************************************************************
      *
      *    F L D S E L
      *
      ******************************************************************
       01  FLDSEL-STAT              PIC XX.
       01  FLDSEL-FIELD             PIC X(4)   VALUE "010.".

       01  FLDSEL-LIST.
           03  FILLER               PIC X(36)  VALUE
           "010 020 030 040 050 060 070 080 090.".

       01  FLDSEL-BUF.
           03  FLDSEL-S.
               05  FLDSEL-DATE        PIC 9(6).
               05  FILLER             PIC X(2).
               05  FLDSEL-DRAWER      PIC 9(2).
               05  FILLER             PIC X(2).
               05  FLDSEL-BEGBAL      PIC S9(7)V99.
               05  FILLER             PIC X(2).
               05  FLDSEL-ENDBAL      PIC S9(7)V99.
               05  FILLER             PIC X(2).
               05  FLDSEL-DESC        PIC X(30).
               05  FILLER             PIC X(2).

       01  FIRST-FIELD              PIC X(4)  VALUE "010.".
       01  LAST-FIELD               PIC X(4)  VALUE "090.".

      ******************************************************************
      *
      *    D I S P L A Y
      *
      ******************************************************************
       01  DISPLAY-LIST.
           03  FILLER               PIC X(36)  VALUE
           "010 020 030 040 050 060 070 080 090 ".
           03  FILLER               PIC X(15)  VALUE
           "PGNO ENDMORE:F.".

       01  DISPLAY-BUF.
           03  D-LINE               OCCURS 9 TIMES.
               05  D-DATE           PIC 99/99/99.
               05  FILLER           PIC X(2).
               05  D-DRAWER         PIC XX.
               05  FILLER           PIC X(2).
               05  D-BEGBAL         PIC Z,ZZZ,ZZ9.99-.
               05  FILLER           PIC X(2).
               05  D-ENDBAL         PIC Z,ZZZ,ZZ9.99-.
               05  FILLER           PIC X(2).
               05  D-DESC           PIC X(30).
               05  FILLER           PIC X(2).
           03  D-PAGENO             PIC ZZ9.
           03  D-END-MORE           PIC X(7).

       01  HEAD-LIST                PIC X(5)   VALUE
           "HEAD.".

       01  CA-HEAD.
           03  FILLER               PIC X(27) VALUE
               "  DATE  DRAWER   BEGINNING ".
           03  FILLER               PIC X(28) VALUE
               "        ENDING   DESCRIPTION".

       01  CURRENT-PAGE             PIC S999.
       01  PAGE-COUNT               PIC 999.
       01  MAX-PAGES                PIC 999    VALUE 50.
       01  PAGE-KEY-TABLE.
           03  PAGE-KEY             PIC X(14) OCCURS 50 TIMES.

       01  KEY-SELECT-TABLE.
           03  SELECT-KEY           PIC X(14) OCCURS 9 TIMES.

      ******************************************************************
      *
      *     C O P Y - L I B R A R Y- W O R K E R S
      *
      ******************************************************************
       COPY "LIBLP/LPWKPATH.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/PHON400.CPY".

       COPY "LIBWI/CASCAN_CMD.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/DATERW.CPY".

       01 CASCAN-WINDOW-HANDLE PIC X(10).
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/FLDSELW.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBWI/CASCAN_DEF.CPY".
       COPY "LIBWI/CASCAN_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".

       COPY "LIBGB/GETFMW.CPY".
       COPY "LIBWI/CASCANW.CPY".

       SCREEN SECTION.
       COPY "LIBWI/CASCAN_SCN.CPY".

       PROCEDURE DIVISION
           USING FORM-PATHNAME CASCAN-BUF EXIT-PATHNAME.

      *****************************************************************
      *
      *    M A I N    P R O G R A M
      *
      *****************************************************************
       MAIN-PROGRAM SECTION.

           PERFORM SQL-CONNECT.

       COPY "LIBGB/CHKSEC.CPY".
      D    STOP MESS.

           MOVE " "      TO VDU-WIMENU-SELECTION.
           MOVE "XX"     TO CASCAN-STATUS.

           MOVE "CASCAN" TO VDU-FORM-NAME.

           DISPLAY FLOATING GRAPHICAL WINDOW
                   AT LINE CASCAN-BEGIN-Y
                   AT COL  CASCAN-BEGIN-X
                   SIZE    CASCAN-COLUMNS + VDU-WIN-W-MOD
                   LINES   CASCAN-LINES + VDU-WIN-H-MOD
           COLOR IS VDU-WCOLOR-SCAN,CONTROL VALUE IS VDU-WCTRL-SCAN
                   CELL SIZE IS LABEL FONT
                   BLANK SCREEN
                   HANDLE  IS CASCAN-WINDOW-HANDLE.

           PERFORM FORM-RESET.

           IF CASCAN-CA-PATH = SPACES
              GO TO ENTER-BRANCH.

           MOVE CASCANW-BRNO    TO SCAN-BRANCH BR-NO.
           PERFORM GET-BR.
           IF IO-FG NOT = 0
              GO TO ENTER-BRANCH.

           MOVE "SNNN040BRNO."  TO VDU.
           MOVE SCAN-BRANCH     TO VDUNUM0.
           PERFORM SCREENIO.

           GO TO SKIP-ENTER-BRANCH.

      *----------------------------------------------------------------
      *    ENTER BRANCH
      *----------------------------------------------------------------
       ENTER-BRANCH.
           MOVE LEGEND-01      TO MESS.
           PERFORM SEND-LEGEND.
           MOVE "ENNN040BRNO." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "1>"
              GO TO END-ROUTINE.

           IF VDUSTATUS NOT = "00"
              GO TO ENTER-BRANCH.

           MOVE VDUNUM0 TO SCAN-BRANCH BR-NO.
           PERFORM GET-BR.
           IF IO-FG NOT = 0
              MOVE "INVALID BRANCH #, PLEASE RE-ENTER" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-BRANCH.

      *----------------------------------------------------------------
      *    SKIPPING POINT FROM MAIN-MODULE.
      *----------------------------------------------------------------
       SKIP-ENTER-BRANCH.

           MOVE EXT-FILPATH-BASE TO FILPATH-BASE.
           MOVE BR-MACHINE       TO FILPATH-MACHINE.
           MOVE BR-DATA-PATH     TO FILPATH-DATA.
           MOVE "B"              TO CA-PATH-OVERRIDE.

           PERFORM OPEN-CA1-FILE.

       ENTER-DATE.
      * IF IN LOAN PROCESSING, CASH DRAWER ENTRY, FORCES TRANSACTION DATE
           IF ( CASCAN-TYPE = "L" )
              MOVE "S  A080T-DATE."     TO VDU
              MOVE CASCAN-DATE TO DATE-YYYYMMDD
              PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
              MOVE DATE-MMDDYY          TO EDIT-DATE
              MOVE EDIT-DATE            TO VDUBUF
              PERFORM SCREENIO
              MOVE CASCAN-DATE TO SCAN-DATE-YYYYMMDD
              GO TO ENTER-DRAWER.

           IF ( CASCAN-TYPE = "M" )
              MOVE LEGEND-02-M    TO MESS
           ELSE
              MOVE LEGEND-02-S    TO MESS.
           PERFORM SEND-LEGEND.
           MOVE "ERNM060T-DATE." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "16"
              MOVE 99999999 TO SCAN-DATE-YYYYMMDD
              GO TO ENTER-DRAWER.

           IF VDUSTATUS = "1>"
              GO TO END-ROUTINE.

           IF VDUSTATUS = "1U"
              GO TO ENTER-DATE.

           IF VDUSTATUS NOT = "00"
              GO TO ENTER-DATE.

           IF VDUSTATUS NOT = "16"
              IF VDUNUM0 = 0
                 GO TO ENTER-DATE.

           MOVE VDU-DATE-YYYYMMDD TO SCAN-DATE-YYYYMMDD.

      *----------------------------------------------------------------
      *    ENTER DRAWER NUMBER
      *----------------------------------------------------------------
       ENTER-DRAWER.
           IF ( CASCAN-TYPE = "L" )
              MOVE LEGEND-03-L    TO MESS
           ELSE
              MOVE LEGEND-03-SM   TO MESS.
           PERFORM SEND-LEGEND.
           MOVE "EN N020DRAWER." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "16"
              MOVE 99 TO SCAN-TILLNO
              GO TO BEGIN-SCAN.

           IF VDUSTATUS = "1>"
              GO TO END-ROUTINE.

           IF VDUSTATUS = "1U"
              GO TO ENTER-DATE.

           IF VDUSTATUS NOT = "00"
              GO TO ENTER-DRAWER.


           MOVE VDUNUM0 TO SCAN-TILLNO.

      *----------------------------------------------------------------
      *    BEGINNING SCAN ON BRANCH #, TRANS. DATE, AND DRAWER #
      *----------------------------------------------------------------
       BEGIN-SCAN.
           MOVE FIRST-FIELD TO FLDSEL-FIELD.
           MOVE 1           TO PAGE-COUNT ONE.
           MOVE 0           TO CURRENT-PAGE.

           MOVE SCAN-BRANCH TO CA-BRNO.

           IF ( SCAN-DATE-YYYYMMDD NOT = 99999999 )
              MOVE SCAN-DATE-YYYYMMDD TO CA-DATE
           ELSE
              MOVE 19000101           TO CA-DATE.

           IF ( SCAN-TILLNO NOT = 99 )
              MOVE SCAN-TILLNO TO CA-TILLNO
           ELSE
              MOVE 0 TO CA-TILLNO.

           MOVE CA1-KEY     TO PAGE-KEY(1).

      *----------------------------------------------------------------
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
           MOVE PAGE-KEY(CURRENT-PAGE) TO CA1-KEY.

           MOVE CA-PATH-OWNBR TO CA-BRNO.

           PERFORM START-CA1-FILE.

      *----------------------------------------------------------------
      *    BUILD PAGE
      *----------------------------------------------------------------
       BUILD-PAGENO.
           PERFORM NEXT-VALID-CASH-FILE.
           IF IO-FG NOT = 0 OR SUB > 9
              GO TO DISPLAY-PAGE.

           MOVE CA1-KEY      TO SELECT-KEY(SUB).

      *----------------------------------------------------------------
       CA-DISPLAY.
           MOVE CA-DATE     TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO D-DATE(SUB).
           MOVE CA-TILLNO   TO D-DRAWER(SUB).
           MOVE CA-BEGBAL   TO D-BEGBAL(SUB).
           MOVE CA-ENDBAL   TO D-ENDBAL(SUB).
           MOVE CA-DESC     TO D-DESC(SUB).

       INCREMENT-SUB.
           ADD 1 TO SUB.
           GO TO BUILD-PAGENO.

      *----------------------------------------------------------------
      *    DISPLAY PAGE
      *----------------------------------------------------------------
       DISPLAY-PAGE.
           IF IO-FG NOT = 0
              IF SUB = 1
                 MOVE "NO RECORDS FOUND!" TO MESS
                 PERFORM SEND-LEGEND
                 MOVE DISPLAY-BUF TO WR-BUF
                 MOVE DISPLAY-LIST TO WR-LIST
                 PERFORM CALL-WRFORM
                 GO TO ENTER-DATE.

           MOVE "MORE..." TO D-END-MORE.

           IF CURRENT-PAGE = PAGE-COUNT
              IF IO-FG = 0
                 IF PAGE-COUNT < MAX-PAGES
                    ADD 1 TO PAGE-COUNT
                    MOVE CA1-KEY TO PAGE-KEY(PAGE-COUNT)
                 ELSE
                    MOVE "MAXIMUM PAGES REACHED!" TO MESS
                    PERFORM SEND-LEGEND
                    MOVE "OVRFLOW" TO D-END-MORE
              ELSE
                 MOVE "* END *" TO D-END-MORE.

           MOVE CURRENT-PAGE TO D-PAGENO.

      *- - - - - - - - - - - - - - - - - -
           MOVE CA-HEAD TO WR-BUF.
           MOVE HEAD-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.
      *- - - - - - - - - - - - - - - - - -

           MOVE DISPLAY-BUF TO WR-BUF.
           MOVE DISPLAY-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      *----------------------------------------------------------------
      *    SELECT FIELD FROM PAGES
      *----------------------------------------------------------------
       SELECT-FIELD-FROM-DISPLAY.
           IF ( CASCAN-TYPE = "L" )
              MOVE LEGEND-04-L  TO MESS
           ELSE
              MOVE LEGEND-04-SM TO MESS.
           PERFORM SEND-LEGEND.
           PERFORM CALL-FLDSEL.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      * TEST UP ARROW:
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF FLDSEL-STAT = "1U"
              IF CURRENT-PAGE = 1
                 PERFORM ALARM
                 GO TO SELECT-FIELD-FROM-DISPLAY
              ELSE
                 MOVE -1 TO ONE
                 MOVE LAST-FIELD TO FLDSEL-FIELD
                 GO TO START-BUILD-PAGENO.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      * TEST PAGE UP:
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF FLDSEL-STAT = "1P"
              IF CURRENT-PAGE = 1
                 PERFORM ALARM
                 GO TO SELECT-FIELD-FROM-DISPLAY
              ELSE
                 MOVE -1 TO ONE
                 MOVE FIRST-FIELD TO FLDSEL-FIELD
                 GO TO START-BUILD-PAGENO.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      * TEST PAGE DOWN OR DOWN ARROW:
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF FLDSEL-STAT = "1N" OR FLDSEL-STAT = "1D"
              IF CURRENT-PAGE NOT < PAGE-COUNT
                 PERFORM ALARM
                 GO TO SELECT-FIELD-FROM-DISPLAY
              ELSE
                 MOVE 1 TO ONE
                 MOVE FIRST-FIELD TO FLDSEL-FIELD
                 GO TO START-BUILD-PAGENO.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      * TEST HOME:
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF FLDSEL-STAT = "1H"
              MOVE 0 TO CURRENT-PAGE
              MOVE 1 TO ONE
              MOVE FIRST-FIELD TO FLDSEL-FIELD
              GO TO START-BUILD-PAGENO.

           IF FLDSEL-STAT = "1>"
              GO TO END-ROUTINE.
           IF FLDSEL-STAT = "10"
              GO TO ENTER-DATE.
           IF NOT (FLDSEL-STAT = "00" OR "1<")
              PERFORM ALARM
              GO TO SELECT-FIELD-FROM-DISPLAY.

           IF (FLDSEL-STAT = "00")
              MOVE SELECT-KEY(FSEL-CURR) TO CASCAN-CA-KEY
              MOVE SCAN-BRANCH         TO CASCANW-BRNO
              MOVE CA-ALL-PATH         TO CASCAN-CA-PATH
              MOVE "00"                TO CASCAN-STATUS
              GO TO END-ROUTINE.

           GO TO SELECT-FIELD-FROM-DISPLAY.

      *****************************************************************
       NEXT-VALID-CASH-FILE SECTION.
      *****************************************************************
       NEXT-VALID-CA.
           PERFORM READ-CA1-FILE-NEXT.
           IF ( IO-FG = 9 )
              GO TO EXIT-VALID-CA.

           IF ( CA-BRNO NOT = SCAN-BRANCH )
              MOVE 9 TO IO-FG
              GO TO EXIT-VALID-CA.

           IF ( SCAN-DATE-YYYYMMDD NOT = 99999999 )
              IF ( CA-DATE NOT = SCAN-DATE-YYYYMMDD )
                 MOVE 9 TO IO-FG
                 GO TO EXIT-VALID-CA.

           IF ( SCAN-TILLNO NOT = 99 )
              IF ( CA-TILLNO NOT = SCAN-TILLNO )
                 GO TO NEXT-VALID-CA.

       EXIT-VALID-CA.
           EXIT.

      *****************************************************************
      *
      *     C O P Y - L I B R A R Y - S E C T I O N S
      *
      *****************************************************************
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/FLDSEL.CPY".
       COPY "LIBGB/DATER.CPY".

      *************************************************
       FORM-RESET SECTION.
      *************************************************
           DISPLAY CASCAN-SCREEN UPON CASCAN-WINDOW-HANDLE.
           MOVE CASCAN-FORM-FIELDS TO WR-BUF.
           MOVE CASCAN-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      *************************************************
       SETUP-LINK-INFO SECTION.
      *************************************************
           MOVE CASCAN-HELP--FILE TO HELP-LINK-FILE.
           MOVE CASCAN-HELP--DIR  TO HELP-LINK-DIR.

      *************************************************
       VDU-CONVERT-FIELD SECTION.
      *************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBWI/CASCAN_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      *****************************************************************
       MISC-PARAGRAPHS SECTION.
      *****************************************************************
       COPY "LIBGB/ACCESS.CPY".

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
           PERFORM CLOSE-CA1-FILE.
           PERFORM CLOSE-BR-FILE.
       COPY "LIBLP/LPCAGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBLP/LPCA1RN.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBGB/FERRORS.CPY".

      *****************************************************************
       END-ROUTINE SECTION.
      *****************************************************************
           PERFORM CLOSE-FILES.

       EXIT-PROG.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY CASCAN-SCREEN.
           DESTROY CASCAN-WINDOW-HANDLE.

       STOP-RUN.
           EXIT PROGRAM.
