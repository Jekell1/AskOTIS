       IDENTIFICATION DIVISION.
       PROGRAM-ID.      CDBSCN.
      ***********************************************************************
      *
      *                   CODE FILE WINDOW SCAN
      *                        "GC" AND "CN" ONLY
      *
      * REV.
      *  2019.0122 BAH NEW, SPLIT OUT CDFILE AND CDBFILE
      *  2024.0415 BAH REMOVED CDBSCN-GC-FEE-GIFTNET-ISO
      *  2025.0326 MB  PERFORMANCE TUNING.  MOVED VALUES FOR START. S35Q-183
      **********************************************************************
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
           "@(#)WI/CDBSCN.CBL S35 04/24/24-08:22:50 >".

      ******************************************************************
      *    W O R K E R S
      ******************************************************************
       COPY "LIBLP/LP01CDB.CPY".
       COPY "LIBLP/LP01CDB_SQL.CPY".
       COPY "LIBLP/LPWSCDB.CPY".

       01  CDBSCN-LINES             PIC 9(02)  VALUE 15.
       01  CDBSCN-COLUMNS           PIC 9(02)  VALUE 39.
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
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       01  FLDSEL-CN-BUF             REDEFINES FLDSEL-BUF.
           03  FLDSEL-BRANCH                   PIC 9(4).
           03  FILLER                          PIC X(1).
           03  FLDSEL-CODE                     PIC X(8).
           03  FILLER                          PIC X(1).
           03  FLDSEL-DESC                     PIC X(21).

      *-----------------------------------------------------------------
       01  FIRST-FIELD                         PIC X(4)  VALUE "010.".
       01  LAST-FIELD                          PIC X(4)  VALUE "090.".

       01  CDBSCN-WINDOW-HANDLE               PIC X(10).

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
           03  D-LINE                     OCCURS 9.
               05  D-BRANCH                  PIC 9(4).
               05  FILLER                    PIC X(1).
               05  D-CODE                    PIC X(8).
               05  FILLER                    PIC X(1).
               05  D-DESC                    PIC X(21).
           03  D-PAGENO                      PIC ZZ9.
           03  D-END-MORE                    PIC X(07).
       01  DISPLAY-GC-BUF          REDEFINES DISPLAY-BUF.
           03  D-GC-LINE                     OCCURS 9.
               05  D-GC-FEECODE              PIC X(2).
               05  FILLER                    PIC X(2).
               05  D-GC-DESC                 PIC X(31).
           03  FILLER                        PIC X(10).

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

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       01  PAGE-KEY-WORKERS-CDB1      REDEFINES PAGE-KEY-WORKERS.
           03  PAGE-KEY-CDB1                    OCCURS 50.
               05  PAGE-KEY-TYPE               PIC X(2).
               05  PAGE-KEY-BRANCH             PIC 9(4).
               05  PAGE-KEY-CODE               PIC X(8).

      *-----------------------------------------------------------------
      *    SAVING CD1-KEY FOR EACH RECORD (9) DISPLAYED IN WINDOW
      *-----------------------------------------------------------------
       01  PAGE-SEL-WORKERS.
           03  PAGE-SEL-CDB1-KEY      OCCURS 9  PIC X(20).

      ******************************************************************
      *
      *     C O P Y - L I B R A R Y - W O R K E R S
      *
      ******************************************************************

       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/FLDSELW.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBWI/CDBSCN_DEF.CPY".
       COPY "LIBWI/CDBSCN_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".

       COPY "LIBGB/GETFMW.CPY".
       COPY "LIBWI/CDBSCNW.CPY".

       SCREEN SECTION.
       COPY "LIBWI/CDBSCN_SCN.CPY".

      ******************************************************************
      *
      *    P R O C E D U R E - D I V I S I O N
      *
      ******************************************************************
       PROCEDURE DIVISION
           USING FORM-PATHNAME CDBSCN-BUF EXIT-PATHNAME.

      ******************************************************************
       MAIN-PROGRAM SECTION.
      ******************************************************************

           PERFORM SQL-CONNECT.

       COPY "LIBGB/CHKSEC.CPY".

           MOVE "XX" TO CDBSCN-STATUS.
           MOVE " " TO VDU-WIMENU-SELECTION.

           MOVE "CDBSCN" TO VDU-FORM-NAME.

           DISPLAY FLOATING GRAPHICAL WINDOW
                   AT  SCREEN LINE CDBSCN-BEGIN-Y * VDU-WIN-L-OFFSET
                   AT  SCREEN COL  CDBSCN-BEGIN-X * VDU-WIN-C-OFFSET
                   SIZE    CDBSCN-COLUMNS + VDU-WIN-W-MOD
                   LINES   CDBSCN-LINES + VDU-WIN-H-MOD
           COLOR IS VDU-WCOLOR-SCAN,CONTROL VALUE IS VDU-WCTRL-SCAN
                   CELL SIZE IS LABEL FONT
                   BLANK SCREEN
                   HANDLE  IS CDBSCN-WINDOW-HANDLE.

           PERFORM FORM-RESET.

           MOVE "S  A300TITLE." TO VDU.
           IF CDBSCN-TYPE = "CN"
              MOVE "     COLLECTOR SELECTION" TO VDUBUF
           ELSE
           IF CDBSCN-TYPE = "GC"
              MOVE "      FEE CODE SELECTION" TO VDUBUF.
           PERFORM SCREENIO.

       BEGIN-BUILD-PAGENO.
           MOVE FIRST-FIELD TO FLDSEL-FIELD.
           MOVE 1 TO PAGE-COUNT ONE.
           MOVE 0 TO CURRENT-PAGE.
           PERFORM OPEN-CDB1-FILE.

           MOVE CDBSCN-TYPE      TO PAGE-KEY-TYPE  (PAGE-COUNT).
           MOVE CDBSCN-BRANCH    TO PAGE-KEY-BRANCH(PAGE-COUNT).
           MOVE SPACES           TO PAGE-KEY-CODE  (PAGE-COUNT).

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

           MOVE CDBSCN-TYPE   TO QCDB1-WBEG-TYPE   QCDB1-WEND-TYPE.
           MOVE CDBSCN-BRANCH TO QCDB1-WBEG-BRANCH QCDB1-WEND-BRANCH.
           MOVE SPACES        TO QCDB1-WBEG-CODE.
           MOVE ALL "Z"       TO QCDB1-WEND-CODE.

           IF CURRENT-PAGE > 1
              MOVE PAGE-KEY(CURRENT-PAGE) TO CDB1-KEY
              MOVE CDB-CODE TO QCDB1-WBEG-CODE.

           PERFORM START-CDB1-FILE.

       BUILD-PAGENO.

           PERFORM READ-CDB1-FILE-NEXT.
           IF ( IO-BAD )
              GO TO DISPLAY-PAGE.

           IF ( CDB-TYPE NOT = CDBSCN-TYPE )
              MOVE 9 TO IO-FG
              GO TO DISPLAY-PAGE.

           IF ( CDB-BRANCH NOT = CDBSCN-BRANCH )
              MOVE 9 TO IO-FG
              GO TO DISPLAY-PAGE.

           IF ( SUB > 9 )
              GO TO DISPLAY-PAGE.

      * VALID 'CD' RECORD; LOAD RECORD TO DISPLAY TO SCREEN

           MOVE CDB1-KEY                    TO PAGE-SEL-CDB1-KEY(SUB).

           IF CDBSCN-TYPE = "GC"
              MOVE CDB-CODE              TO D-GC-FEECODE(SUB)
              MOVE CDB-DESC              TO D-GC-DESC   (SUB)
           ELSE
              MOVE CDB-BRANCH            TO D-BRANCH(SUB)
              MOVE CDB-CODE              TO D-CODE  (SUB)
              MOVE CDB-DESC              TO D-DESC  (SUB).

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

              IF ( IO-FG = 0 ) AND ( CDBSCN-TYPE = CDB-TYPE )
                 IF ( PAGE-COUNT < MAX-PAGES )
                    ADD 1 TO PAGE-COUNT
                    MOVE CDB1-KEY TO PAGE-KEY(PAGE-COUNT)
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

      * SEND THE SELECTED CD1-KEY BACK VIA CDBSCN-CD1-KEY
           MOVE PAGE-SEL-CDB1-KEY(FLDSEL-ELE) TO CDBSCN-CDB1-KEY.
           MOVE "00"                          TO CDBSCN-STATUS.

           GO TO END-ROUTINE.


      ******************************************************************
      *
      *    C O P Y - L I B R A R Y - S E C T I O N S
      *
      ******************************************************************
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/FLDSEL.CPY".
       COPY "LIBGB/DATER.CPY".

      *****************************************************
       FORM-RESET SECTION.
      *****************************************************
           DISPLAY CDBSCN-SCREEN UPON CDBSCN-WINDOW-HANDLE.
           MOVE CDBSCN-FORM-FIELDS TO WR-BUF.
           MOVE CDBSCN-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      *****************************************************
       SETUP-LINK-INFO SECTION.
      *****************************************************
           MOVE CDBSCN-HELP--FILE TO HELP-LINK-FILE.
           MOVE CDBSCN-HELP--DIR  TO HELP-LINK-DIR.

      *****************************************************
       VDU-CONVERT-FIELD SECTION.
      *****************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBWI/CDBSCN_EVA.CPY".
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
           PERFORM CLOSE-CDB1-FILE.
       COPY "LIBLP/LPCDBGS_SQL.CPY".
       COPY "LIBLP/LPCDB1RN.CPY".
       COPY "LIBGB/FERRORS.CPY".

      ******************************************************************
       END-ROUTINE SECTION.
      ******************************************************************
           PERFORM CLOSE-FILES.
       EXIT-PROG.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY CDBSCN-SCREEN.
           DESTROY CDBSCN-WINDOW-HANDLE.

       STOP-RUN.
           EXIT PROGRAM.
