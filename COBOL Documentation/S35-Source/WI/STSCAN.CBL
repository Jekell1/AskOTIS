       IDENTIFICATION DIVISION.
       PROGRAM-ID.    STSCAN.
       DATE-WRITTEN.  08-09-2005.
      ******************************************************************
      *
      * IDENTIFICATION:  WI/STSCAN.C
      *
      * DESCRIPTION   :  STATE SCAN
      *
      * REV.
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
      *    W O R K I N G - S T O R A G E - S E C T I O N
      *
      ******************************************************************
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(37) VALUE
           "@(#)WI/STSCAN A30 02/11/15-13:33:48 >".
       COPY "LIBLP/LP01ST.CPY".
       COPY "LIBLP/LP01ST_SQL.CPY".
       COPY "LIBLP/LPWSST.CPY".


       01  STSCAN-LINES            PIC 9(02)   VALUE 15.
       01  STSCAN-COLUMNS          PIC 9(02)   VALUE 27.
       01  STSCAN-BEGIN-Y          PIC 9(02)   VALUE 05.
       01  STSCAN-BEGIN-X          PIC 9(02)   VALUE 26.
       01  ERRCD                       PIC X VALUE " ".
       01  LEGEND.
           03  FILLER                  PIC X(25) VALUE
             " F1, SELECT CODE (RTN):  ".

       01  LEGEND-LIST                 PIC X(07)     VALUE "LEGEND.".

       01  ONE                     PIC S9(1)     VALUE ZEROES.
       01  SUB                     PIC 9(02)     VALUE ZEROES.


      ******************************************************************
      *
      *    D I S P L A Y - W O R K E R S
      *
      ******************************************************************
       78  MAX                                       VALUE 9.
           88  ELE-GOOD                              VALUE 01 THRU MAX.

      *=================================================================
       01  DISPLAY-BUF.
           03  D-LINE                  OCCURS MAX.
               05  D-ST-CODE           PIC X(02).
               05  FILLER              PIC X(5).
               05  D-ST-DESC           PIC X(16).

           03  D-PAGENO            PIC ZZ9.
           03  D-END-MORE          PIC X(07).

      *=================================================================
       01  DISPLAY-LIST.
           03  FILLER PIC X(20)  VALUE "010 020 030 040 050 ".
           03  FILLER PIC X(16)  VALUE "060 070 080 090 ".
           03  FILLER PIC X(15)  VALUE "PGNO ENDMORE:F.".

       01  STSCAN-WINDOW-HANDLE    PIC X(10).
      ******************************************************************
      *
      *    F L D S E L - W O R K E R S
      *
      ******************************************************************
       01  FLDSEL-BUF.
           05  FLDSEL-KEY          PIC X(02).
           05  FILLER              PIC X(5).
           05  FLDSEL-DESC         PIC X(16).

       01  FLDSEL-FIELD                PIC X(04)     VALUE "010.".

       01  FIRST-FIELD          PIC X(04)     VALUE "010.".
       01  LAST-FIELD           PIC X(04)     VALUE "090.".
       01  FLDSEL-FIELD-LAST    PIC X(04)     VALUE "090.".

      *-----------------------------------------------------------------
       01  FLDSEL-LIST.
           03  FILLER PIC X(20)  VALUE "010 020 030 040 050 ".
           03  FILLER PIC X(16)  VALUE "060 070 080 090.".

      *=================================================================
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

      ******************************************************************
      *
      *    P A G E - W O R K E R S
      *
      ******************************************************************
       77  PAGE-COUNT                  PIC 9(03)     VALUE ZEROES.
       77  CURRENT-PAGE                PIC S9(3)     VALUE ZEROES.
       78  MAX-PAGES                                  VALUE 50.
      *=================================================================
       01  PAGE-KEY-01.
           03  PAGE-KEY                              OCCURS MAX-PAGES
                                                     PIC XX.

      ******************************************************************
      *
      *     C O P Y - L I B R A R Y - W O R K E R S
      *
      ******************************************************************
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".

       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/FLDSELW.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBWI/STSCAN_DEF.CPY".
       COPY "LIBWI/STSCAN_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".

       COPY "LIBGB/GETFMW.CPY".
       COPY "LIBWI/STSCANW.CPY".

       SCREEN SECTION.
       COPY "LIBWI/STSCAN_SCN.CPY".

      ******************************************************************
      *
      *    P R O C E D U R E - D I V I S I O N
      *
      ******************************************************************
       PROCEDURE DIVISION
           USING FORM-PATHNAME STSCAN-BUF EXIT-PATHNAME.

      ******************************************************************
      *
      *    M A I N - P R O G R A M
      *
      ******************************************************************
       MAIN-PROGRAM SECTION.

           PERFORM SQL-CONNECT.

       COPY "LIBGB/CHKSEC.CPY".
      D    STOP MESS.
           MOVE "XX"                      TO STSCAN-STATUS.
           MOVE SPACES                    TO VDU-WIMENU-SELECTION.

           MOVE "STSCAN" TO VDU-FORM-NAME.

           DISPLAY FLOATING GRAPHICAL WINDOW
                   AT LINE STSCAN-BEGIN-Y
                   AT COL  STSCAN-BEGIN-X
                   SIZE    STSCAN-COLUMNS + VDU-WIN-W-MOD
                   LINES   STSCAN-LINES + VDU-WIN-H-MOD
           COLOR IS VDU-WCOLOR-SCAN,CONTROL VALUE IS VDU-WCTRL-SCAN
                   CELL SIZE IS LABEL FONT
                   BLANK SCREEN
                   HANDLE  IS STSCAN-WINDOW-HANDLE.

           PERFORM FORM-RESET.

           PERFORM SEND-LEGEND.

           PERFORM SCAN-MODULE.

           GO TO END-PROGRAM.

      ******************************************************************
       SCAN-MODULE SECTION.
      ******************************************************************

           MOVE FIRST-FIELD  TO FLDSEL-FIELD.
           MOVE 1 TO ONE PAGE-COUNT.
           MOVE 0 TO CURRENT-PAGE.

           PERFORM OPEN-ST1-FILE.
           MOVE 0 TO PAGE-KEY(PAGE-COUNT).

       START-BUILD-PAGENO.
           ADD ONE TO CURRENT-PAGE.

           IF ( CURRENT-PAGE < 1  )
              MOVE FIRST-FIELD TO FLDSEL-FIELD
              MOVE 1 TO CURRENT-PAGE.

           IF ( CURRENT-PAGE > PAGE-COUNT )
              MOVE FIRST-FIELD TO FLDSEL-FIELD
              MOVE 1 TO CURRENT-PAGE.

           MOVE SPACES TO DISPLAY-BUF.
           MOVE 1 TO SUB.

           MOVE PAGE-KEY(CURRENT-PAGE)     TO ST-CODE.
           PERFORM START-ST1-FILE.

       BUILD-PAGENO.
           PERFORM READ-ST1-FILE-NEXT.
           IF ( IO-BAD )  OR SUB > 9
              GO TO DISPLAY-PAGE.

           MOVE ST-CODE TO D-ST-CODE(SUB).
           MOVE ST-DESC TO D-ST-DESC(SUB).
           ADD 1 TO SUB.

           GO TO BUILD-PAGENO.

      *=================================================================
       DISPLAY-PAGE.

           IF ( IO-BAD       )
              IF SUB = 1
                 PERFORM SEND-DISPLAY
                 MOVE "NO RECORDS FOUND!"     TO MESS
                 PERFORM SEND-MESS
                 GO TO SCAN-MODULE-END.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "MORE..."                  TO D-END-MORE.

           IF ( CURRENT-PAGE = PAGE-COUNT )
              IF ( IO-GOOD             )
                 IF ( PAGE-COUNT < MAX-PAGES )
                    ADD 1 TO PAGE-COUNT
                    MOVE ST-CODE TO PAGE-KEY(PAGE-COUNT)
                 ELSE
                    MOVE "MAXIMUM PAGES REACHED!" TO MESS
                    PERFORM SEND-LEGEND
                    MOVE "OVRFLOW"         TO D-END-MORE
              ELSE
                 MOVE "* END *"            TO D-END-MORE.

           MOVE CURRENT-PAGE TO D-PAGENO.
           PERFORM SEND-DISPLAY.

      *=================================================================
       SELECT-FIELD-FROM-DISPLAY.

           MOVE "F7 EXIT, SELECT CODE(RTN):" TO LEGEND.
           PERFORM SEND-LEGEND.

           PERFORM CALL-FLDSEL.


      * UP ARROW
           IF ( FLDSEL-UP-KEY        )
              IF ( CURRENT-PAGE = 01 )
                 PERFORM ALARM
                 GO TO SELECT-FIELD-FROM-DISPLAY
              ELSE
                 MOVE -1 TO ONE
                 MOVE FLDSEL-FIELD-LAST TO FLDSEL-FIELD
                 GO TO START-BUILD-PAGENO.

      * TEST PAGE UP
           IF ( FLDSEL-PAGE-UP-KEY   )
              IF ( CURRENT-PAGE = 01 )
                 PERFORM ALARM
                 GO TO SELECT-FIELD-FROM-DISPLAY
              ELSE
                 MOVE -1 TO ONE
                 MOVE FIRST-FIELD TO FLDSEL-FIELD
                 GO TO START-BUILD-PAGENO.

           IF ( FLDSEL-PAGE-DOWN-KEY ) OR ( FLDSEL-DOWN-KEY )
              IF ( CURRENT-PAGE NOT < PAGE-COUNT )
                 PERFORM ALARM
                 GO TO SELECT-FIELD-FROM-DISPLAY
              ELSE
                 MOVE 1 TO ONE
                 MOVE FIRST-FIELD TO FLDSEL-FIELD
                 GO TO START-BUILD-PAGENO.

      * HOME
           IF ( FLDSEL-HOME-KEY      )
              MOVE 0 TO CURRENT-PAGE
              MOVE 1 TO ONE
              MOVE FIRST-FIELD TO FLDSEL-FIELD
              GO TO START-BUILD-PAGENO.

           IF ( FLDSEL-F7-KEY        )
              MOVE SPACES                     TO STSCAN-KEY
              GO TO SCAN-MODULE-END.

           IF NOT (FLDSEL-STAT = "00" OR "1<")
              PERFORM ALARM
              GO TO SELECT-FIELD-FROM-DISPLAY.

           MOVE FLDSEL-KEY TO STSCAN-KEY.
           MOVE "00"       TO STSCAN-STATUS.

           GO TO SCAN-MODULE-END.

       SCAN-MODULE-END.
           PERFORM CLOSE-ST1-FILE.


       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/FLDSEL.CPY".
       COPY "LIBGB/DATER.CPY".

      **************************************************************************
       FORM-RESET SECTION.
      **************************************************************************
           DISPLAY STSCAN-SCREEN UPON STSCAN-WINDOW-HANDLE.
           MOVE STSCAN-FORM-FIELDS TO WR-BUF.
           MOVE STSCAN-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      **************************************************************************
       SETUP-LINK-INFO SECTION.
      **************************************************************************
           MOVE STSCAN-HELP--FILE TO HELP-LINK-FILE.
           MOVE STSCAN-HELP--DIR  TO HELP-LINK-DIR.

      **************************************************************************
       VDU-CONVERT-FIELD SECTION.
      **************************************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBWI/STSCAN_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ******************************************************************
       MISC-PARAGRAPHS SECTION.
      ******************************************************************
      *-----------------------------------------------------------------
       COPY "LIBGB/ACCESS.CPY".
       SEND-DISPLAY.
           MOVE DISPLAY-BUF TO WR-BUF.
           MOVE DISPLAY-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      *-----------------------------------------------------------------
       SEND-LEGEND.
           MOVE LEGEND TO WR-BUF.
           MOVE LEGEND-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      ******************************************************************
       IO-ROUTINES SECTION.
      ******************************************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/DECLARE_SQL.CPY".
           PERFORM CLOSE-ST1-FILE.
       COPY "LIBLP/LPSTGS_SQL.CPY".
       COPY "LIBLP/LPST1RN.CPY".

       COPY "LIBGB/FERRORS.CPY".

      ******************************************************************
       END-PROGRAM SECTION.
      ******************************************************************

           PERFORM CLOSE-FILES.

       EXIT-PROG.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY STSCAN-SCREEN.
           DESTROY STSCAN-WINDOW-HANDLE.

       STOP-RUN.
           EXIT PROGRAM.
