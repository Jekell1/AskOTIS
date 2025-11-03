       IDENTIFICATION DIVISION.
       PROGRAM-ID.    TWISCN.
       AUTHOR.        MG.
       DATE-WRITTEN.  07-29-2005.
      ******************************************************************
      *
      * IDENTIFICATION:  WI/TWISCN.C
      *
      * DESCRIPTION   :  TRW INFORMATION FILE SCAN
      *                  CALLED IN METRO2 AND TWINFO
      *
      *=================================================================
      * REV.
      * MG  2005.0729 {PR#00000} PARADATA <A15> [BETH HILL]
      *               ***NEW***
      *
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.


      ******************************************************************
       I-O-CONTROL.
       DATA DIVISION.
       FILE SECTION.


      ******************************************************************
      *
      *    W O R K I N G - S T O R A G E - S E C T I O N
      *
      ******************************************************************
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)WI/TWISCN.CBL S35 03/07/24-09:15:35 >".

      ******************************************************************
      *
      *    F I L E - W O R K E R S
      *
      ******************************************************************
       COPY "LIBSP/SP01TRWINFO.CPY".
       COPY "LIBSP/SP01TRWINFO_SQL.CPY".
       COPY "LIBSP/SPWSTRWINFO.CPY".

      ******************************************************************
      *
      *    M I S C E L L A N E O U S - W O R K E R S
      *
      ******************************************************************

       01  TWISCN-LINES              PIC 99  VALUE 20.
       01  TWISCN-COLUMNS            PIC 99  VALUE 49.
       01  TWISCN-BEGIN-Y            PIC 99  VALUE 03.
       01  TWISCN-BEGIN-X            PIC 99  VALUE 20.

       01  ERRCD                       PIC X VALUE " ".
       01  LEGEND.
           03  FILLER PIC X(25) VALUE " F1, SELECT CODE (RTN):  ".

       01  LEGEND-LIST                 PIC X(07)     VALUE "LEGEND.".

       01  SUBSCRIPT-WORKERS.
           03  ONE                     PIC S9(1)     VALUE ZEROES.
           03  SUB                     PIC 9(02)     VALUE ZEROES.

       01  TITLE-BUF.
           03  FILLER PIC X(35) VALUE
                              "CREDIT BUREAU ID SCAN              ".

       01  TITLE-LIST                  PIC X(06)     VALUE "TITLE.".

       01  HEADER-BUF.
           05  FILLER           PIC X(45) VALUE
               "     E E T                                   ".
           05  FILLER           PIC X(45) VALUE
	       " ID  Q X U  REPORTER NAME                    ".
           05  FILLER           PIC X(45) VALUE
	       "---------------------------------------------".

       01  HEADER-LIST.
           03  FILLER PIC X(24)  VALUE "HEADER1 HEADER2 HEADER3 ".

      ******************************************************************
      *
      *    D I S P L A Y - W O R K E R S
      *
      ******************************************************************
       78  MAX                                       VALUE 10.
       77  ELE                         PIC 9(02)     VALUE ZEROES.
           88  ELE-GOOD                              VALUE 01 THRU MAX.

      *=================================================================
       01  DISPLAY-BUF.

           03  D-LINE              OCCURS MAX.
               05  D-ID                PIC 9(04).
               05  FILLER              PIC X(01).
               05  D-EQ                PIC X(01).
               05  FILLER              PIC X(01).
               05  D-EX                PIC X(01).
               05  FILLER              PIC X(01).
               05  D-TU                PIC X(01).
               05  FILLER              PIC X(02).
               05  D-NAME              PIC X(33).

           03  DISPLAY-INFO-BUF.
               05  D-PAGENO            PIC ZZ9.
               05  D-END-MORE          PIC X(07).

      *=================================================================
       01  DISPLAY-LIST.
           03  FILLER PIC X(20)  VALUE "010 020 030 040 050 ".
           03  FILLER PIC X(20)  VALUE "060 070 080 090 100 ".
           03  FILLER PIC X(15)  VALUE "PGNO ENDMORE:F.".

      ******************************************************************
      *
      *    F L D S E L - W O R K E R S
      *
      ******************************************************************
       01  FLDSEL-BUF                  PIC X(45)     VALUE SPACES.

       01  FLDSEL-FIELD                PIC X(04)     VALUE "010.".
       01  FILLER                      REDEFINES FLDSEL-FIELD.
           03  FLDSEL-ELE              PIC 9(02).
           03  FLDSEL-COL              PIC 9(01).
           03  FILLER                  PIC X(01).

       01  FIRST-FIELD          PIC X(04)     VALUE "010.".
       01  LAST-FIELD           PIC X(04)     VALUE "100.".

      *-----------------------------------------------------------------
       01  FLDSEL-LIST.
           03  FILLER PIC X(20)  VALUE "010 020 030 040 050 ".
           03  FILLER PIC X(20)  VALUE "060 070 080 090 100.".

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
       77  PAGE-CURRENT                PIC S9(3)     VALUE ZEROES.
       78  PAGE-MAX                                  VALUE 50.
      *=================================================================
       01  PAGE-KEY-WORKERS.
           03  PAGE-KEY                              OCCURS PAGE-MAX.
               05  PAGE-KEY-TRW-KEY.
                   07  PAGE-KEY-TRW-INFO-ID          PIC 9(04).

       01  PAGE-SEL-WORKERS.
           03  PAGE-SEL                              OCCURS PAGE-MAX.
               05  PAGE-SEL-TRW-KEY.
                   07  PAGE-SEL-TRW-INFO-ID          PIC 9(04).
               05  PAGE-SEL-TRW-EQ                   PIC X(01).
               05  PAGE-SEL-TRW-EX                   PIC X(01).
               05  PAGE-SEL-TRW-TU                   PIC X(01).
               05  PAGE-SEL-TRW-REP-NAME             PIC X(33).
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
       COPY "LIBWI/TWISCN_DEF.CPY".
       COPY "LIBWI/TWISCN_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       01  TWISCN-WINDOW-HANDLE    PIC X(10).

       COPY "LIBGB/GETFMW.CPY".
       COPY "LIBWI/TWISCNW.CPY".

       SCREEN SECTION.
       COPY "LIBWI/TWISCN_SCN.CPY".

      ******************************************************************
      *
      *    P R O C E D U R E - D I V I S I O N
      *
      ******************************************************************
       PROCEDURE DIVISION
           USING FORM-PATHNAME TWISCN-BUF EXIT-PATHNAME.

      ******************************************************************
      *
      *    M A I N - P R O G R A M
      *
      ******************************************************************
       MAIN-PROGRAM SECTION.

           PERFORM SQL-CONNECT.

           PERFORM INITIALIZATION.

           PERFORM SCAN-MODULE.

       MAIN-PROGRAM-EXIT.
           GO TO END-PROGRAM.

      ******************************************************************
       INITIALIZATION SECTION.
      ******************************************************************
       COPY "LIBGB/CHKSEC.CPY".

           MOVE " " TO VDU-WIMENU-SELECTION.

           MOVE "TWISCN" TO VDU-FORM-NAME.

           DISPLAY FLOATING GRAPHICAL WINDOW
                   AT LINE TWISCN-BEGIN-Y
                   AT COL  TWISCN-BEGIN-X
                   SIZE    TWISCN-COLUMNS + VDU-WIN-W-MOD
                   LINES   TWISCN-LINES + VDU-WIN-H-MOD
           COLOR IS VDU-WCOLOR-TCLP,CONTROL VALUE IS VDU-WCTRL-TCLP
                   CELL SIZE IS LABEL FONT
                   BLANK SCREEN
                   HANDLE  IS TWISCN-WINDOW-HANDLE.

           PERFORM FORM-RESET.

           PERFORM SEND-TITLE.
           PERFORM SEND-HEADER.

       INITIALIZATION-EXIT.
           EXIT.

      ******************************************************************
       SCAN-MODULE SECTION.
      ******************************************************************

           MOVE FIRST-FIELD         TO FLDSEL-FIELD.
           MOVE 01                         TO ONE.
           MOVE 01                         TO PAGE-COUNT.
           MOVE ZEROES                     TO PAGE-CURRENT.
           MOVE ZEROES                     TO PAGE-KEY-WORKERS.
           INITIALIZE PAGE-SEL-WORKERS.

           PERFORM OPEN-TRW-INFO-FILE.

       SCAN-PAGE-START.

           ADD ONE TO PAGE-CURRENT.

           IF ( PAGE-CURRENT < 1  )
              MOVE FIRST-FIELD      TO FLDSEL-FIELD
              MOVE 01                      TO PAGE-CURRENT.

           IF ( PAGE-CURRENT > PAGE-COUNT )
              MOVE FIRST-FIELD      TO FLDSEL-FIELD
              MOVE 01                      TO PAGE-CURRENT.

           MOVE SPACES                     TO DISPLAY-BUF.
           MOVE ZEROES                     TO ELE.

           MOVE PAGE-KEY(PAGE-CURRENT)     TO TRW-INFO-KEY.
           PERFORM START-TRW-INFO-FILE.

       SCAN-PAGE-BUILD.

           PERFORM READ-TRW-INFO-FILE-NEXT.
           IF ( IO-BAD )
              MOVE 9                       TO IO-FG
              GO TO SCAN-PAGE-DISPLAY.

           ADD  01                         TO ELE.

           IF NOT ( ELE-GOOD )
              GO TO SCAN-PAGE-DISPLAY.

           MOVE "N"                        TO D-EQ(ELE).
           MOVE "N"                        TO D-EX(ELE).
           MOVE "N"                        TO D-TU(ELE).

           IF (TRW-INFO-EQUIFAX-ID NOT = SPACES)
              MOVE "Y"                     TO D-EQ(ELE).
           IF (TRW-INFO-EXPERIAN-ID NOT = SPACES)
              MOVE "Y"                     TO D-EX(ELE).
           IF (TRW-INFO-TUC-ID NOT = SPACES)
              MOVE "Y"                     TO D-TU(ELE).

           MOVE TRW-INFO-ID                TO D-ID(ELE).
           MOVE TRW-INFO-REPORTER-NAME     TO D-NAME(ELE).

           MOVE TRW-INFO-ID               TO PAGE-SEL-TRW-INFO-ID(ELE).
           MOVE D-EQ(ELE)                 TO PAGE-SEL-TRW-EQ(ELE).
           MOVE D-EX(ELE)                 TO PAGE-SEL-TRW-EX(ELE).
           MOVE D-TU(ELE)                 TO PAGE-SEL-TRW-TU(ELE).
           MOVE TRW-INFO-REPORTER-NAME    TO PAGE-SEL-TRW-REP-NAME(ELE).

           GO TO SCAN-PAGE-BUILD.

       SCAN-PAGE-DISPLAY.

           IF ( IO-BAD       ) AND
              ( ELE = ZEROES )
              PERFORM SEND-DISPLAY
              MOVE "NO RECORDS FOUND!"     TO MESS
              PERFORM SEND-MESS
              GO TO SCAN-MODULE-END.

           MOVE "MORE..."                  TO D-END-MORE.

           IF ( PAGE-CURRENT = PAGE-COUNT )
              IF ( IO-GOOD )
                 IF ( PAGE-COUNT < PAGE-MAX )
                    ADD 01                 TO PAGE-COUNT
                    MOVE TRW-INFO-ID       TO PAGE-KEY(PAGE-COUNT)
                 ELSE
                    MOVE "MAXIMUM PAGES REACHED!" TO MESS
                    PERFORM SEND-LEGEND
                    MOVE "OVRFLOW"         TO D-END-MORE
              ELSE
                 MOVE "* END *"            TO D-END-MORE.

           MOVE PAGE-CURRENT               TO D-PAGENO.
           PERFORM SEND-DISPLAY.

       SCAN-PAGE-FLDSEL.

           MOVE "F7 EXIT, SELECT CODE(RTN):" TO LEGEND.
           PERFORM SEND-LEGEND.

           PERFORM CALL-FLDSEL.

           IF ( FLDSEL-HOME-KEY      ) GO TO SCAN-FLDSEL-HOME-KEY.

           IF ( FLDSEL-DOWN-KEY      ) GO TO SCAN-FLDSEL-DOWN-KEY.
           IF ( FLDSEL-UP-KEY        ) GO TO SCAN-FLDSEL-UP-KEY.

           IF ( FLDSEL-PAGE-DOWN-KEY ) GO TO SCAN-FLDSEL-DOWN-KEY.
           IF ( FLDSEL-PAGE-UP-KEY   ) GO TO SCAN-FLDSEL-UP-KEY.

           IF ( FLDSEL-F7-KEY        ) GO TO SCAN-FLDSEL-F7-KEY.
           IF ( FLDSEL-ENTER-KEY     ) GO TO SCAN-FLDSEL-ENTER-KEY.

           PERFORM ALARM.
           GO TO SCAN-PAGE-FLDSEL.

       SCAN-FLDSEL-DOWN-KEY.

           IF ( PAGE-CURRENT NOT < PAGE-COUNT )
              PERFORM ALARM
              GO TO SCAN-PAGE-FLDSEL.

           MOVE FIRST-FIELD         TO FLDSEL-FIELD.
           MOVE 01                         TO ONE.

           GO TO SCAN-PAGE-START.

       SCAN-FLDSEL-ENTER-KEY.

           MOVE PAGE-SEL-TRW-KEY   (FLDSEL-ELE)   TO TWISCN-TRW-KEY.

           GO TO SCAN-MODULE-END.

       SCAN-FLDSEL-F7-KEY.

           MOVE ZEROES                     TO TRW-INFO-KEY.

           GO TO SCAN-MODULE-END.

       SCAN-FLDSEL-HOME-KEY.

           MOVE FIRST-FIELD         TO FLDSEL-FIELD.
           MOVE 01                         TO ONE.
           MOVE ZEROES                     TO PAGE-CURRENT.

           GO TO SCAN-PAGE-START.

       SCAN-FLDSEL-UP-KEY.

           IF ( PAGE-CURRENT = 01 )
              PERFORM ALARM
              GO TO SCAN-PAGE-FLDSEL.

           IF ( FLDSEL-PAGE-UP-KEY )
              MOVE FIRST-FIELD      TO FLDSEL-FIELD
           ELSE
              MOVE LAST-FIELD       TO FLDSEL-FIELD.

           MOVE -1                         TO ONE.

           GO TO SCAN-PAGE-START.

       SCAN-MODULE-END.

           PERFORM CLOSE-TRW-INFO-FILE.

       SCAN-MODULE-EXIT.
           EXIT.

      ******************************************************************
      *
      *    C O P Y - L I B R A R Y - S E C T I O N S
      *
      ******************************************************************
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/FLDSEL.CPY".
       COPY "LIBGB/DATER.CPY".


      *************************************************
       FORM-RESET SECTION.
      *************************************************
           DISPLAY TWISCN-SCREEN UPON TWISCN-WINDOW-HANDLE.
           MOVE TWISCN-FORM-FIELDS TO WR-BUF.
           MOVE TWISCN-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      *************************************************
       SETUP-LINK-INFO SECTION.
      *************************************************
           MOVE TWISCN-HELP--FILE TO HELP-LINK-FILE.
           MOVE TWISCN-HELP--DIR  TO HELP-LINK-DIR.

      *************************************************
       VDU-CONVERT-FIELD SECTION.
      *************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBWI/TWISCN_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ******************************************************************
       MISC-PARAGRAPHS SECTION.
      ******************************************************************
       COPY "LIBGB/ACCESS.CPY".
      *-----------------------------------------------------------------
       SEND-DISPLAY.
           MOVE DISPLAY-BUF TO WR-BUF.
           MOVE DISPLAY-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.
      *-----------------------------------------------------------------
       SEND-LEGEND.
           MOVE LEGEND TO WR-BUF.
           MOVE LEGEND-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.
      *-----------------------------------------------------------------
       SEND-TITLE.
           CALL "C$JUSTIFY" USING                 TITLE-BUF "C".
           MOVE TITLE-BUF TO WR-BUF.
           MOVE TITLE-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.
      *-----------------------------------------------------------------
       SEND-HEADER.
           MOVE HEADER-BUF TO WR-BUF.
           MOVE HEADER-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      ******************************************************************
       IO-ROUTINES SECTION.
      ******************************************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/DECLARE_SQL.CPY".
       COPY "LIBSP/SPTRWINFOGS_SQL.CPY".
       COPY "LIBSP/SPTRWINFORN.CPY".

       COPY "LIBGB/FERRORS.CPY".

      ******************************************************************
       END-PROGRAM SECTION.
      ******************************************************************

           PERFORM CLOSE-FILES.

       EXIT-PROG.

       STOP-RUN.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY TWISCN-SCREEN.
           DESTROY TWISCN-WINDOW-HANDLE.
           EXIT PROGRAM.
