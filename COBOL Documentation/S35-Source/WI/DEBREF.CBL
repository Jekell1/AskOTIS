       IDENTIFICATION DIVISION.
       PROGRAM-ID.      DEBREF.
      ******************************************************************
      *
      *                   DEBIT REF CODE FILE WINDOW SCAN
      *        CD-TYPE = "BR" AND CD-BR-FILE-TYPE = "C"
      *
      * CALLED FROM LONPI0 AND DISBI0
      *
      * REV.
      *  2021.03.05 BAH NEW, TO SCAN FOR ONLY DEBIT REFCD'S, #1490
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
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)WI/DEBREF.CBL S35 08/28/23-14:33:21 >".

      ******************************************************************
      *    W O R K E R S
      ******************************************************************
       COPY "LIBLP/LP01CD.CPY".
       COPY "LIBLP/LP01CD_SQL.CPY".
       COPY "LIBLP/LPWSCD.CPY".

       01  DEBREF-LINES          PIC 99    VALUE 15.
       01  DEBREF-COLUMNS        PIC 99    VALUE 39.
       01  DEBREF-BEGIN-Y        PIC 99    VALUE 6.
       01  DEBREF-BEGIN-X        PIC 99    VALUE 41.

       01  ERRCD                   PIC X      VALUE SPACES.
       01  ONE                     PIC S9     VALUE 1.
       01  SUB                     PIC 99.

      ******************************************************************
      *
      *    F L D S E L
      *
      ******************************************************************
       01  FLDSEL-FIELD                        PIC X(04)   VALUE "010.".
       01  FILLER                    REDEFINES FLDSEL-FIELD.
           03  FLDSEL-ELE                      PIC 9(02).
           03  FILLER                          PIC X(02).

       01  FLDSEL-STAT                         PIC XX.
       01  FLDSEL-LIST.
           03  FILLER                          PIC X(36)  VALUE
           "010 020 030 040 050 060 070 080 090.".

      *-----------------------------------------------------------------
       01  FLDSEL-BUF                          PIC X(35).
       01  FLDSEL-CD1-BUF            REDEFINES FLDSEL-BUF.
           03  FLDSEL-CODE                     PIC X(08).
           03  FILLER                          PIC X(02).
           03  FLDSELD-DESC                    PIC X(25).
      *-----------------------------------------------------------------
       01  FIRST-FIELD                         PIC X(4)  VALUE "010.".
       01  LAST-FIELD                          PIC X(4)  VALUE "090.".

       01  DEBREF-WINDOW-HANDLE               PIC X(10).

      ******************************************************************
      *
      *    D I S P L A Y
      *
      ******************************************************************
       01  DISPLAY-LIST.
           03  FILLER                        PIC X(51)  VALUE
           "010 020 030 040 050 060 070 080 090 PGNO ENDMORE:F.".

      *-----------------------------------------------------------------
       01  DISPLAY-BUF.
           03  D-LINE                        OCCURS 9.
               05  D-CODE                    PIC X(8).
               05  FILLER                    PIC X(2).
               05  D-DESC                    PIC X(25).
           03  D-PAGENO                      PIC ZZ9.
           03  D-END-MORE                    PIC X(07).
      *-----------------------------------------------------------------
       01  CURRENT-PAGE                      PIC S999.
       01  PAGE-COUNT                        PIC 999.
       01  MAX-PAGES                         PIC 999    VALUE 50.

      ******************************************************************
      *
      *    P A G E - W O R K E R S
      *
      ******************************************************************
       01  PAGE-KEY-WORKERS.
           03  PAGE-KEY              OCCURS 50 PIC X(20).

      * LEAVING 20 SO I DONT HAVE TO CHANGE THE FORM
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       01  PAGE-KEY-WORKERS-CD1      REDEFINES PAGE-KEY-WORKERS.
           03  PAGE-KEY-CD1                    OCCURS 50.
               05  PAGE-KEY-TYPE               PIC X(2).
               05  PAGE-KEY-CODE               PIC X(8).
               05  PAGE-KEY-FILLER             PIC X(10).

      *-----------------------------------------------------------------
      *    SAVING CD1-KEY FOR EACH RECORD (9) DISPLAYED IN WINDOW
      *-----------------------------------------------------------------
       01  PAGE-SEL-WORKERS.
           03  PAGE-SEL-CD1-KEY      OCCURS 9  PIC X(20).

      ******************************************************************
      *
      *     C O P Y - L I B R A R Y - W O R K E R S
      *
      ******************************************************************

       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/FLDSELW.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBWI/DEBREF_DEF.CPY".
       COPY "LIBWI/DEBREF_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".

       COPY "LIBGB/GETFMW.CPY".
       COPY "LIBWI/DEBREFW.CPY".

       SCREEN SECTION.
       COPY "LIBWI/DEBREF_SCN.CPY".

      ******************************************************************
      *
      *    P R O C E D U R E - D I V I S I O N
      *
      ******************************************************************
       PROCEDURE DIVISION
           USING FORM-PATHNAME DEBREF-BUF EXIT-PATHNAME.

      ******************************************************************
       MAIN-PROGRAM SECTION.
      ******************************************************************

           PERFORM SQL-CONNECT.

       COPY "LIBGB/CHKSEC.CPY".
           MOVE " " TO VDU-WIMENU-SELECTION.

           MOVE "DEBREF" TO VDU-FORM-NAME.

           DISPLAY FLOATING GRAPHICAL WINDOW
                   AT  SCREEN LINE DEBREF-BEGIN-Y * VDU-WIN-L-OFFSET
                   AT  SCREEN COL  DEBREF-BEGIN-X * VDU-WIN-C-OFFSET
                   SIZE    DEBREF-COLUMNS + VDU-WIN-W-MOD
                   LINES   DEBREF-LINES + VDU-WIN-H-MOD
           COLOR IS VDU-WCOLOR-SCAN,CONTROL VALUE IS VDU-WCTRL-SCAN
                   CELL SIZE IS LABEL FONT
                   BLANK SCREEN
                   HANDLE  IS DEBREF-WINDOW-HANDLE.

           PERFORM FORM-RESET.

           MOVE "S  A300TITLE." TO VDU.
           MOVE " DEBIT REF CODE SELECTION" TO VDUBUF.
           PERFORM SCREENIO.

           MOVE FIRST-FIELD TO FLDSEL-FIELD.
           MOVE 1 TO PAGE-COUNT ONE.
           MOVE 0 TO CURRENT-PAGE.

           PERFORM OPEN-CD1-FILE.

           MOVE DEBREF-TYPE       TO PAGE-KEY-TYPE  (PAGE-COUNT).
           MOVE SPACES            TO PAGE-KEY-CODE  (PAGE-COUNT).
           MOVE SPACES            TO PAGE-KEY-FILLER(PAGE-COUNT).

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

           MOVE SPACES                 TO PAGE-SEL-WORKERS.
           MOVE PAGE-KEY(CURRENT-PAGE) TO CD1-KEY.
           PERFORM START-CD1-FILE.

       BUILD-PAGENO.

           PERFORM READ-CD1-FILE-NEXT.
           IF ( IO-BAD )
              GO TO DISPLAY-PAGE.

      * THIS SCAN ONLY INCLUDES "BR" AND FILE-TYPE "C" DEBIT CARDS
           IF ( CD-TYPE NOT = DEBREF-TYPE ) OR
                ( CD-BR-FILE-TYPE NOT = "C" )
              GO TO BUILD-PAGENO.

           IF ( SUB > 9 )
              GO TO DISPLAY-PAGE.

           MOVE CD1-KEY                    TO PAGE-SEL-CD1-KEY(SUB).

           MOVE CD-CODE                 TO D-CODE(SUB)
           MOVE CD-DESC                 TO D-DESC(SUB).

           ADD 1 TO SUB.
           GO TO BUILD-PAGENO.

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

           IF ( CURRENT-PAGE = PAGE-COUNT )
              IF ( IO-FG = 0 ) AND ( DEBREF-TYPE = CD-TYPE )
                 IF ( PAGE-COUNT < MAX-PAGES )
                    ADD 1 TO PAGE-COUNT
                    MOVE CD1-KEY TO PAGE-KEY(PAGE-COUNT)
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

       SELECT-FIELD-FROM-DISPLAY.

           MOVE "F7 EXIT, SELECT CODE(RTN):" TO MESS.
           PERFORM SEND-LEGEND.

           MOVE "L" TO FSEL-SEARCH-TYPE.
           MOVE 1 TO FSEL-SEARCH-COL-BEGIN.
           MOVE 1 TO FSEL-SEARCH-COL-END.
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

      * SEND THE SELECTED CD1-KEY BACK VIA DEBREF-CD1-KEY
      
           MOVE PAGE-SEL-CD1-KEY(FLDSEL-ELE) TO DEBREF-CD1-KEY.

           GO TO END-ROUTINE.

      ******************************************************************
      *
      *    C O P Y - L I B R A R Y - S E C T I O N S
      *
      ******************************************************************
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/FLDSEL.CPY".
       COPY "LIBGB/DATER.CPY".

      ******************************************************************
       FORM-RESET SECTION.
      ******************************************************************
           DISPLAY DEBREF-SCREEN UPON DEBREF-WINDOW-HANDLE.
           MOVE DEBREF-FORM-FIELDS TO WR-BUF.
           MOVE DEBREF-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      ******************************************************************
       SETUP-LINK-INFO SECTION.
      ******************************************************************
           MOVE DEBREF-HELP--FILE TO HELP-LINK-FILE.
           MOVE DEBREF-HELP--DIR  TO HELP-LINK-DIR.

      ******************************************************************
       VDU-CONVERT-FIELD SECTION.
      ******************************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBWI/DEBREF_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ******************************************************************
       MISC-PARAGRAPHS SECTION.
      ******************************************************************
       COPY "LIBGB/ACCESS.CPY".
       SEND-LEGEND.
           MOVE "S  A600WSEL:D." TO VDU.
           MOVE MESS TO VDUBUF.
           PERFORM SCREENIO.

      ******************************************************************
       IO-ROUTINES SECTION.
      ******************************************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/DECLARE_SQL.CPY".
           PERFORM CLOSE-CD1-FILE.
       COPY "LIBLP/LPCDGS_SQL.CPY".
       COPY "LIBLP/LPCD1RN.CPY".
       COPY "LIBGB/FERRORS.CPY".

      ******************************************************************
       END-ROUTINE SECTION.
      ******************************************************************
           PERFORM CLOSE-FILES.
       EXIT-PROG.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY DEBREF-SCREEN.
           DESTROY DEBREF-WINDOW-HANDLE.

       STOP-RUN.
           EXIT PROGRAM.
