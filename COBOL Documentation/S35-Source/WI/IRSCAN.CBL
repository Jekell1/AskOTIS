       IDENTIFICATION DIVISION.
       PROGRAM-ID.    IRSCAN.
       AUTHOR.        MG.
       DATE-WRITTEN.  2002-06-20.
      ******************************************************************
      *
      * IDENTIFICATION:  WI/IRSCAN.C
      *
      * DESCRIPTION   :  INSURANCE RATE TABLE FILE SCAN
      *
      *-----------------------------------------------------------------
      * REV.
      * MG  020620 {DR#00008} PARADATA FINANCIAL <A15> [JOHN]
      *            ***NEW***
      *
      * MJD 131120 A30 CONVERSION COMPLETED
      * BAH 20190723 CHANGED IR-KEY FROM 99 TO 9(3), PT0147
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

      ******************************************************************
      *
      *     W O R K I N G - S T O R A G E - S E C T I O N
      *
      ******************************************************************
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(55) VALUE
           "@(#)WI/IRSCAN A30 11/20/13-09:38:17 10/01/14-09:56:05 >".

       COPY "LIBLP/LP01IR.CPY".
       COPY "LIBLP/LP01IR_SQL.CPY".
       COPY "LIBLP/LPWSIR.CPY".

       01  IRSCAN-LINES            PIC 9(02)     VALUE 17.
       01  IRSCAN-COLUMNS          PIC 9(02)     VALUE 49.
       01  IRSCAN-BEGIN-Y          PIC 9(02)     VALUE 8.
       01  IRSCAN-BEGIN-X          PIC 9(02)     VALUE 22.

       01  ERRCD                       PIC X         VALUE SPACES.
       01  LEGEND                      PIC X(45)     VALUE SPACES.
       01  LEGEND-LIST                 PIC X(09)     VALUE "LEGEND:S.".
       01  ONE                         PIC S9(1)     VALUE 1.
       01  SUB                         PIC 9(02)     VALUE ZEROES.

       01  PAGE-WORKERS.
           03  CURRENT-PAGE            PIC S9(3)     VALUE ZEROES.
           03  MAX-LINES               PIC 9(03)     VALUE 10.
           03  MAX-PAGES               PIC 9(03)     VALUE 10.
           03  PAGE-COUNT              PIC 9(03)     VALUE ZEROES.

      *
      *    D I S P L A Y - W O R K E R S
      *
       01  DISPLAY-BUF.
           03  D-LINE                               OCCURS 10 TIMES.
               05  D1-INS-RATE-CD                    PIC 9(03).
               05  FILLER                            PIC X(02).
               05  D1-DESCRIPTION                    PIC X(40).
           03  FILLER                                PIC X(55).

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       01  DISPLAY-BUF-MISC                REDEFINES DISPLAY-BUF.
           03  FILLER                                PIC X(450).
           03  D-END-MORE                            PIC X(007).
           03  D-PAGENO                              PIC ZZ9.
           03  D-HEADER                              PIC X(045).

      *-----------------------------------------------------------------
       01  DISPLAY-LIST.
           03  FILLER PIC X(20) VALUE "010 020 030 040 050 ".
           03  FILLER PIC X(20) VALUE "060 070 080 090 100 ".
           03  FILLER PIC X(24) VALUE "ENDMORE:F PGNO HEADER:S.".

      *
      *    F L D S E L - W O R K E R S
      *
       01  FLDSEL-BUF                  PIC X(45)     VALUE SPACES.

      *-----------------------------------------------------------------
       01  FLDSEL-FIELD                PIC X(04)     VALUE "010.".
       01  FLDSEL-FIELD-RDF            REDEFINES FLDSEL-FIELD.
           03  FLDSEL-FIELD-SUB        PIC 9(02).
           03  FILLER                  PIC X(02).

      *-----------------------------------------------------------------
       01  FLDSEL-LIST.
           03  FILLER PIC X(20) VALUE "010 020 030 040 050 ".
           03  FILLER PIC X(20) VALUE "060 070 080 090 100.".

      *-----------------------------------------------------------------
       01  FLDSEL-STAT                 PIC X(02)     VALUE SPACES.
           88  FLDSEL-F1-KEY                         VALUE "10".
           88  FLDSEL-F2-KEY                         VALUE "12".
           88  FLDSEL-F3-KEY                         VALUE "16".
           88  FLDSEL-F4-KEY                         VALUE "18".
           88  FLDSEL-F5-KEY                         VALUE "1:".
           88  FLDSEL-F6-KEY                         VALUE "1<".
           88  FLDSEL-F7-KEY                         VALUE "1>".
           88  FLDSEL-F8-KEY                         VALUE "1 ".
           88  FLDSEL-F9-KEY                         VALUE "1(".
           88  FLDSEL-F10-KEY                        VALUE "1)".
           88  FLDSEL-F11-KEY                        VALUE "1!".
           88  FLDSEL-F12-KEY                        VALUE "1#".
           88  FLDSEL-DOWN-KEY                       VALUE "1D".
           88  FLDSEL-ENTER-KEY                      VALUE "00".
           88  FLDSEL-HOME-KEY                       VALUE "1H".
           88  FLDSEL-LEFT-KEY                       VALUE "1L".
           88  FLDSEL-PAGE-UP-KEY                    VALUE "1P".
           88  FLDSEL-PAGE-DOWN-KEY                  VALUE "1N".
           88  FLDSEL-RIGHT-KEY                      VALUE "1R".
           88  FLDSEL-UP-KEY                         VALUE "1U".

      *-----------------------------------------------------------------
       01  FIRST-FIELD                 PIC X(04)     VALUE "010.".
       01  LAST-FIELD                  PIC X(04)     VALUE "100.".

      *
      *    H E A D E R - W O R K E R S
      *
       01  HEADER-STANDARD.
           03  FILLER PIC X(20) VALUE "CODE DESCRIPTION    ".
           03  FILLER PIC X(25) VALUE "                         ".

      *
      *    L E G E N D - W O R K E R S
      *
       01  LEGEND-SELECTION.
           03  FILLER PIC X(20) VALUE " F7-EXIT, 'ENTER' ON".
           03  FILLER PIC X(20) VALUE " SELECTED CODE:     ".

      *
      *    P A G E - K E Y - W O R K E R S
      *
       01  PAGE-KEY-WORKERS.
           03  PAGE-KEY                              OCCURS 10 TIMES.
               05  PAGE-KEY-IR-CODE                  PIC 9(03).

      *
      *    S E L E C T I O N - K E Y - W O R K E R S
      *
       01  SELECTION-KEY-WORKERS.
           03  SELECTION-KEY                         OCCURS 10 TIMES.
               05  SELECTION-KEY-IR-CODE             PIC 9(03).

      ******************************************************************
      *
      *    C O P Y - L I B R A R Y - W O R K E R S
      *
      ******************************************************************
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/FLDSELW.CPY".
       01 IRSCAN-WINDOW-HANDLE PIC X(10) VALUE SPACES.
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBWI/IRSCAN_DEF.CPY".
       COPY "LIBWI/IRSCAN_WKS.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/ACCESSW.CPY".

       COPY "LIBGB/GETFMW.CPY".
       COPY "LIBWI/IRSCANW.CPY".

       SCREEN SECTION.
       COPY "LIBWI/IRSCAN_SCN.CPY".
      ******************************************************************
      *
      *    P R O C E D U R E - D I V I S I O N
      *
      ******************************************************************
       PROCEDURE DIVISION
           USING FORM-PATHNAME IRSCAN-BUF EXIT-PATHNAME.

      ******************************************************************
      *
      *    M A I N - P R O G R A M
      *
      ******************************************************************
       MAIN-PROGRAM SECTION.

           PERFORM SQL-CONNECT.

           PERFORM INITIALIZATION.
           PERFORM PROCESS-SCAN.

       EXIT-MAIN-PROGRAM.
           GO TO END-PROGRAM.

      ******************************************************************
       INITIALIZATION SECTION.
      ******************************************************************

       COPY "LIBGB/CHKSEC.CPY".

           MOVE "IRSCAN" TO VDU-FORM-NAME.
           MOVE FORM-PATHNAME TO FORM-PATH.
           DISPLAY FLOATING GRAPHICAL WINDOW
                   SCREEN LINE 06 * VDU-WIN-L-OFFSET
                   SCREEN COL 22 * VDU-WIN-C-OFFSET
           COLOR IS VDU-WCOLOR-SCAN,CONTROL VALUE IS VDU-WCTRL-SCAN
                   CELL SIZE IS LABEL FONT
                   SIZE 49 + VDU-WIN-W-MOD
                   LINES 17 + VDU-WIN-H-MOD
                   BLANK SCREEN
                   HANDLE IS IRSCAN-WINDOW-HANDLE.

           PERFORM FORM-RESET.

           MOVE "XX"     TO IRSCAN-STATUS.
           MOVE SPACES   TO VDU-WIMENU-SELECTION.

           MOVE ZEROES TO IRSCAN-IR-KEY.

       EXIT-INITIALIZATION.
           EXIT.

      ******************************************************************
       PROCESS-SCAN SECTION.
      ******************************************************************

           MOVE ZEROES      TO CURRENT-PAGE.
           MOVE FIRST-FIELD TO FLDSEL-FIELD.
           MOVE 1           TO PAGE-COUNT ONE.
           PERFORM CLEAR-PAGE-KEY-TABLE.

           PERFORM OPEN-IR-FILE.

       START-BUILD-PAGENO.

           ADD ONE TO CURRENT-PAGE.

           IF ( CURRENT-PAGE < 1 )
              MOVE FIRST-FIELD TO FLDSEL-FIELD
              MOVE 1           TO CURRENT-PAGE.

           IF ( CURRENT-PAGE > PAGE-COUNT )
              MOVE FIRST-FIELD TO FLDSEL-FIELD
              MOVE 1           TO CURRENT-PAGE.

           PERFORM CLEAR-SELECTION-KEY-TABLE.

           MOVE SPACES   TO DISPLAY-BUF.
           MOVE 1        TO SUB.

           MOVE PAGE-KEY(CURRENT-PAGE) TO IR-KEY.
           PERFORM START-IR-FILE.

       BUILD-PAGENO.

           PERFORM READ-IR-FILE-NEXT.
           IF ( IO-BAD      )
              GO TO DISPLAY-PAGE.
           IF ( SUB > MAX-LINES )
              GO TO DISPLAY-PAGE.

       LOAD-SELECTION-TABLE.

           MOVE IR-KEY  TO SELECTION-KEY-IR-CODE(SUB).

           COMPUTE D1-INS-RATE-CD(SUB) = IR-KEY + 70.
           MOVE IR-DESC TO D1-DESCRIPTION(SUB).

       END-BUILD-PAGENO.

           ADD 1 TO SUB.
           GO TO BUILD-PAGENO.

       DISPLAY-PAGE.

           IF ( IO-BAD ) AND
              ( SUB = 1    )
              MOVE DISPLAY-BUF TO WR-BUF
              MOVE DISPLAY-LIST TO WR-LIST
              PERFORM CALL-WRFORM
              MOVE "NO RECORDS FOUND!" TO MESS
              PERFORM SEND-MESS
              GO TO EXIT-PROCESS-SCAN.

           MOVE "MORE..." TO D-END-MORE.

           IF ( CURRENT-PAGE = PAGE-COUNT )
              IF ( IO-GOOD )
                 IF ( PAGE-COUNT < MAX-PAGES )
                    ADD 1 TO PAGE-COUNT
                    MOVE IR-KEY     TO PAGE-KEY-IR-CODE   (PAGE-COUNT)
                 ELSE
                    MOVE "MAXIMUM PAGES REACHED!" TO LEGEND
                    PERFORM SEND-LEGEND
                    MOVE "OVRFLOW" TO D-END-MORE
              ELSE
                 MOVE "* END *" TO D-END-MORE.

           MOVE CURRENT-PAGE TO D-PAGENO.
           PERFORM SETUP-HEADER.
           MOVE DISPLAY-BUF TO WR-BUF.
           MOVE DISPLAY-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       SELECT-FIELD-FROM-DISPLAY.

           MOVE LEGEND-SELECTION TO LEGEND.
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

      * TEST PAGE DOWN OR DOWN ARROW
           IF FLDSEL-STAT = "1N" OR FLDSEL-STAT = "1D"
              IF CURRENT-PAGE NOT < PAGE-COUNT
                 PERFORM ALARM
                 GO TO SELECT-FIELD-FROM-DISPLAY
              ELSE
                 MOVE 1 TO ONE
                 MOVE FIRST-FIELD TO FLDSEL-FIELD
                 GO TO START-BUILD-PAGENO.

      * TEST HOME
           IF FLDSEL-STAT = "1H"
              MOVE 0           TO CURRENT-PAGE
              MOVE 1           TO ONE
              MOVE FIRST-FIELD TO FLDSEL-FIELD
              GO TO START-BUILD-PAGENO.

      * TEST F7
           IF FLDSEL-STAT = "1>"
              GO TO EXIT-PROCESS-SCAN.

           IF NOT (FLDSEL-STAT = "00" OR "1<")
              PERFORM ALARM
              GO TO SELECT-FIELD-FROM-DISPLAY.

           MOVE SELECTION-KEY-IR-CODE(FLDSEL-FIELD-SUB) TO
                                            IRSCAN-IR-KEY.
           MOVE "00" TO IRSCAN-STATUS.

       EXIT-PROCESS-SCAN.
           EXIT.

      **************************************************************************
       FORM-RESET SECTION.
      **************************************************************************
           DISPLAY SPACES UPON IRSCAN-WINDOW-HANDLE
                               LINE 0 COL 0 ERASE SCREEN.
           DISPLAY IRSCAN-SCREEN UPON IRSCAN-WINDOW-HANDLE.
           MOVE IRSCAN-FORM-FIELDS TO WR-BUF.
           MOVE IRSCAN-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      **************************************************************************
       SETUP-LINK-INFO SECTION.
      **************************************************************************
           MOVE IRSCAN-HELP--FILE TO HELP-LINK-FILE.
           MOVE IRSCAN-HELP--DIR TO HELP-LINK-DIR.

      **************************************************************************
       VDU-CONVERT-FIELD SECTION.
      **************************************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBWI/IRSCAN_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ******************************************************************
      *
      *    C O P Y - L I B R A R Y - S E C T I O N S
      *
      ******************************************************************
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/FLDSEL.CPY".
       COPY "LIBGB/DATER.CPY".

      ******************************************************************
       MISCELLANEOUS-PARAGRAPHS SECTION.
      ******************************************************************
       COPY "LIBGB/ACCESS.CPY".
       CLEAR-PAGE-KEY-TABLE.

           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > MAX-PAGES
                   MOVE ZEROES TO PAGE-KEY-IR-CODE   (SUB)
           END-PERFORM.

       CLEAR-SELECTION-KEY-TABLE.

           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > MAX-LINES
                   MOVE ZEROES TO SELECTION-KEY-IR-CODE   (SUB)
           END-PERFORM.

       SETUP-HEADER.

           MOVE HEADER-STANDARD TO D-HEADER.

       SEND-LEGEND.
           MOVE LEGEND TO WR-BUF.
           MOVE LEGEND-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      ******************************************************************
       IO-ROUTINES SECTION.
      ******************************************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/DECLARE_SQL.CPY".
           PERFORM CLOSE-IR-FILE.
       COPY "LIBLP/LPIRGS_SQL.CPY".
       COPY "LIBLP/LPIRRN.CPY".

       COPY "LIBGB/FERRORS.CPY".

      ******************************************************************
       END-PROGRAM SECTION.
      ******************************************************************

           PERFORM CLOSE-FILES.

       EXIT-PROG.

       STOP-RUN.
           DESTROY IRSCAN-WINDOW-HANDLE
           MOVE FORM-PATH TO FORM-PATHNAME.
           EXIT PROGRAM.
