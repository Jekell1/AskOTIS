       IDENTIFICATION DIVISION.
       PROGRAM-ID.      CQSCN3.
       DATE-WRITTEN.    040996.
      ******************************************************************
      *
      *              COLLECTOR QUEUE(3) FILE WINDOW SCAN
      *                CALLED FROM WI/CQSCAN
      *
      * DESCRIPTION: A LISTING OF AVAILABLE COLLECTORS THAT HAVE A 
      *              QUEUE FILE.
      *
      * REV:
      *  053196 BLL FIX LOOP IF NO RECORDS FOUND
      *   CS 141114 CHANGED VDU-WCOLOR-TCLP/VDU-WCTRL-TCLP TO VDU-WCOLOR-SCAN/
      *             VDU-WCTRL-SCAN  
      *   CS 190201 GLOBAL CQFILE
      *   CS 190301 BROKE OUT CQFILE TO 2 FILES. CQ WILL REMAIN AS THE DETAIL
      *             CQS IS THE SUMMARY/HEADER INFO
      * BAH 2024.0610 MOVE "1>" TO CQSCN3-STATUS OR CAUGHT IN A "NO RECORDS 
      *               FOUND LOOP" IN CQSCAN S35Q-61
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
           "@(#)WI/CQSCN3 S35 06/10/24-09:25:27 >".

      ******************************************************************
      *
      *    I F N - W O R K E R S
      *
      ******************************************************************
       COPY "LIBSP/SP01CQS.CPY".
       COPY "LIBSP/SP01CQS_SQL.CPY".
       COPY "LIBSP/SPWSCQS.CPY".
       COPY "LIBLP/LP01CDB.CPY".
       COPY "LIBLP/LP01CDB_SQL.CPY".
       COPY "LIBLP/LPWSCDB.CPY".

       01  CQSCN3-LINES               PIC 99  VALUE 17.
       01  CQSCN3-COLUMNS             PIC 99  VALUE 50.
       01  CQSCN3-BEGIN-Y             PIC 99  VALUE 6.
       01  CQSCN3-BEGIN-X             PIC 99  VALUE 14.

       01  ERRCD                   PIC X(1)   VALUE SPACES.
       01  ONE                     PIC S9     VALUE 1.
       01  SUB                     PIC 99.

      *---------------------------------------------------- LEGEND -----
       01  LEGEND-01               PIC X(46)  VALUE
           " F7-EXIT, SELECT COLLECTOR                   :".

      *=================================================================
      *
      *    F L D S E L
      *
      *=================================================================
       01  FLDSEL-STAT             PIC XX.
       01  FLDSEL-FIELD            PIC X(4)   VALUE "010.".

      *-----------------------------------------------------------------
       01  FLDSEL-LIST.
           03  FILLER              PIC X(36)  VALUE
           "010 020 030 040 050 060 070 080 090.".

      *-----------------------------------------------------------------
       01  FLDSEL-BUF.
           05  FLDSEL-COLLID        PIC X(3).
           05  FILLER               PIC X(1).
           05  FLDSEL-NAME          PIC X(25).
           05  FILLER               PIC X(1).
           05  FLDSEL-TOT-ACCTS     PIC ZZ,ZZ9.
           05  FILLER               PIC X(2).
           05  FLDSEL-TOT-ACCTS     PIC ZZ,ZZ9.

      *-----------------------------------------------------------------
       01  FIRST-FIELD             PIC X(4)  VALUE "010.".
       01  LAST-FIELD              PIC X(4)  VALUE "090.".

       01  CQSCN3-WINDOW-HANDLE    PIC X(10).
      *=================================================================
      *    D I S P L A Y
      *=================================================================
       01  DISPLAY-LIST.
           03  FILLER              PIC X(51)  VALUE
           "010 020 030 040 050 060 070 080 090 PGNO ENDMORE:F.".

      *-----------------------------------------------------------------
       01  DISPLAY-BUF.
           03  D-LINE              OCCURS 9 TIMES.
               05  D-COLLID        PIC X(3).
               05  FILLER          PIC X(1).
               05  D-NAME          PIC X(25).
               05  FILLER          PIC X(1).
               05  D-TOT-ACCTS     PIC ZZ,ZZ9.
               05  FILLER          PIC X(2).
               05  D-WORKED-ACCTS  PIC ZZ,ZZ9.
           03  D-PAGENO            PIC ZZ9.
           03  D-END-MORE          PIC X(7).

      *-----------------------------------------------------------------
       01  CURRENT-PAGE            PIC S999.
       01  PAGE-COUNT              PIC 999.
       01  MAX-PAGES               PIC 999    VALUE 100.
       01  PAGE-KEY-01.
           03  PAGE-KEY            OCCURS 100 TIMES.
               05  PAGE-COLLID     PIC X(3).

      ******************************************************************
      *
      *     C O P Y - L I B R A R Y - W O R K E R S
      *
      ******************************************************************
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/FLDSELW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBWI/CQSCN3_DEF.CPY".
       COPY "LIBWI/CQSCN3_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBLP/LPMDTEW.CPY".
       COPY "LIBLP/LPSTATW.CPY".

       COPY "LIBGB/GETFMW.CPY".
       COPY "LIBWI/CQSCN3W.CPY".

       SCREEN SECTION.
       COPY "LIBWI/CQSCN3_SCN.CPY".

      ******************************************************************
      *
      *     P R O C E D U R E - D I V I S I O N
      *
      ******************************************************************
       PROCEDURE DIVISION
           USING FORM-PATHNAME CQSCN3-BUF EXIT-PATHNAME.

      ******************************************************************
      *
      *    M A I N - P R O G R A M - M O D U L E
      *
      ******************************************************************
       MAIN-PROGRAM SECTION.

           PERFORM SQL-CONNECT.

       COPY "LIBGB/CHKSEC.CPY".

      D    STOP MESS.
           MOVE "XX"          TO CQSCN3-STATUS.

           MOVE " "           TO VDU-WIMENU-SELECTION.

           MOVE "CQSCN3" TO VDU-FORM-NAME.

           DISPLAY FLOATING GRAPHICAL WINDOW
                   AT LINE CQSCN3-BEGIN-Y
                   AT COL  CQSCN3-BEGIN-X
                   SIZE    CQSCN3-COLUMNS + VDU-WIN-W-MOD
                   LINES   CQSCN3-LINES + VDU-WIN-H-MOD
           COLOR IS VDU-WCOLOR-SCAN,CONTROL VALUE IS VDU-WCTRL-SCAN
                   CELL SIZE IS LABEL FONT
                   BLANK SCREEN
                   HANDLE  IS CQSCN3-WINDOW-HANDLE.

           PERFORM FORM-RESET.

           MOVE FIRST-FIELD TO FLDSEL-FIELD.
           MOVE 1 TO PAGE-COUNT ONE.
           MOVE 0 TO CURRENT-PAGE.

           PERFORM OPEN-CQS1-FILE.
           MOVE SPACES TO PAGE-COLLID(PAGE-COUNT).

       START-BUILD-PAGENO.

           ADD ONE TO CURRENT-PAGE.

           IF CURRENT-PAGE < 1
              MOVE FIRST-FIELD TO FLDSEL-FIELD
              MOVE 1 TO CURRENT-PAGE.

           IF CURRENT-PAGE > PAGE-COUNT
              MOVE FIRST-FIELD TO FLDSEL-FIELD
              MOVE 1 TO CURRENT-PAGE.

           MOVE SPACES                    TO DISPLAY-BUF.
           MOVE 1                         TO SUB.
           MOVE PAGE-COLLID(CURRENT-PAGE) TO CQS-COLLID.

           MOVE CQS-PATH-OWNBR TO CQS-BRNO.
 
           PERFORM START-CQS1-FILE.


       BUILD-PAGENO.

      * CHECK ON BRANCH MAY NEED TO TEST GLOBAL QUEUING FLAG

           PERFORM READ-CQS1-FILE-NEXT.
           IF ( IO-FG NOT = 0 ) OR (CQS-BRNO NOT = CQS-PATH-OWNBR)
               MOVE 9 TO IO-FG 
               GO TO DISPLAY-PAGE.

           IF ( SUB > 9 )
              GO TO DISPLAY-PAGE.

           MOVE CQS-COLLID          TO CDB-CODE.
           MOVE "CN"                TO CDB-TYPE.
           MOVE EXT-FILPATH-BRANCH  TO CDB-BRANCH.
           PERFORM GET-CDB1-FILE.
           IF ( IO-FG = 9 )
              MOVE "<NOT ON FILE>" TO D-NAME(SUB)
           ELSE
              MOVE CDB-DESC      TO D-NAME(SUB).

           MOVE CQS-COLLID TO D-COLLID(SUB).
           MOVE CQS-TOT-REC     TO D-TOT-ACCTS(SUB).
           COMPUTE D-WORKED-ACCTS(SUB) = CQS-TOT-REC - CQS-WORKED-REC.

           ADD 1 TO SUB.
           GO TO BUILD-PAGENO.

       DISPLAY-PAGE.

           IF ( IO-FG NOT = 0 )
              IF ( SUB = 1 )
                 MOVE "NO RECORDS FOUND!" TO MESS
                 PERFORM SEND-MESS
                 MOVE "1>" TO FLDSEL-STAT CQSCN3-STATUS
                 GO TO EXIT-MAIN-MODULE.

           MOVE "MORE..." TO D-END-MORE.

           IF CURRENT-PAGE = PAGE-COUNT
              IF IO-FG = 0
                 IF PAGE-COUNT < MAX-PAGES
                    ADD 1 TO PAGE-COUNT
                    MOVE CQS-COLLID TO PAGE-COLLID(PAGE-COUNT)
                 ELSE
                    MOVE "MAXIMUM PAGES REACHED!" TO MESS
                    PERFORM SEND-LEGEND
                    MOVE "OVRFLOW" TO D-END-MORE
              ELSE
                 MOVE "* END *" TO D-END-MORE.

           MOVE CURRENT-PAGE TO D-PAGENO.
           MOVE DISPLAY-LIST TO WR-LIST.
           MOVE DISPLAY-BUF TO WR-BUF.
           PERFORM CALL-WRFORM.

       SELECT-FIELD-FROM-DISPLAY.

           MOVE LEGEND-01 TO MESS.
           PERFORM SEND-LEGEND.

           PERFORM CALL-FLDSEL.

      * TEST UP ARROW:
           IF FLDSEL-STAT = "1U"
              IF CURRENT-PAGE = 1
                 GO TO SELECT-FIELD-FROM-DISPLAY
              ELSE
                 MOVE -1 TO ONE
                 MOVE LAST-FIELD TO FLDSEL-FIELD
                 GO TO START-BUILD-PAGENO.

      * TEST PAGE UP:
           IF FLDSEL-STAT = "1P"
              IF CURRENT-PAGE = 1
                 GO TO SELECT-FIELD-FROM-DISPLAY
              ELSE
                 MOVE -1 TO ONE
                 MOVE FIRST-FIELD TO FLDSEL-FIELD
                 GO TO START-BUILD-PAGENO.

      * TEST PAGE DOWN OR DOWN ARROW:
           IF FLDSEL-STAT = "1N" OR FLDSEL-STAT = "1D"
              IF CURRENT-PAGE NOT < PAGE-COUNT
                 GO TO SELECT-FIELD-FROM-DISPLAY
              ELSE
                 MOVE 1 TO ONE
                 MOVE FIRST-FIELD TO FLDSEL-FIELD
                 GO TO START-BUILD-PAGENO.

      * TEST HOME:
           IF ( FLDSEL-STAT = "1H" )
              MOVE 0 TO CURRENT-PAGE
              MOVE 1 TO ONE
              MOVE FIRST-FIELD TO FLDSEL-FIELD
              GO TO START-BUILD-PAGENO.

           IF ( FLDSEL-STAT = "1>" )
              MOVE "1>" TO CQSCN3-STATUS
              GO TO EXIT-MAIN-MODULE.

           IF NOT (FLDSEL-STAT = "00" OR "1<")
              GO TO SELECT-FIELD-FROM-DISPLAY.

           MOVE FLDSEL-COLLID TO CQSCN3-COLLID.
           MOVE "00"          TO CQSCN3-STATUS.

       EXIT-MAIN-MODULE.
           GO TO END-ROUTINE.

      **************************************************************************
       FORM-RESET SECTION.
      **************************************************************************
           DISPLAY CQSCN3-SCREEN UPON CQSCN3-WINDOW-HANDLE.
           MOVE CQSCN3-FORM-FIELDS TO WR-BUF.
           MOVE CQSCN3-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      **************************************************************************
       SETUP-LINK-INFO SECTION.
      **************************************************************************
           MOVE CQSCN3-HELP--FILE TO HELP-LINK-FILE.
           MOVE CQSCN3-HELP--DIR  TO HELP-LINK-DIR.

      **************************************************************************
       VDU-CONVERT-FIELD SECTION.
      **************************************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBWI/CQSCN3_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ******************************************************************
       MISC-PARAGRAPHS SECTION.
      ******************************************************************
      *-----------------------------------------------------------------
       COPY "LIBGB/ACCESS.CPY".
      *-----------------------------------------------------------------
       GET-CDB1-FILE.
           PERFORM OPEN-CDB1-FILE.
           PERFORM READ-CDB1-FILE.
           PERFORM CLOSE-CDB1-FILE.

      *-----------------------------------------------------------------
       SEND-LEGEND.
           MOVE "S  A460WSEL:D." TO VDU.
           MOVE MESS TO VDUBUF.
           PERFORM SCREENIO.

      ******************************************************************
      *
      *    C O P Y - L I B R A R Y - S E C T I O N S
      *
      ******************************************************************
       COPY "LIBGB/FLDSEL.CPY".
       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/SCREEN.CPY".

      ******************************************************************
       IO-ROUTINES SECTION.
      ******************************************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/DECLARE_SQL.CPY".
           PERFORM CLOSE-CQS1-FILE.
           PERFORM CLOSE-CDB1-FILE.
       COPY "LIBSP/SPCQSGS_SQL.CPY".
       COPY "LIBLP/LPCDBGS_SQL.CPY".
       COPY "LIBSP/SPCQS1RN.CPY".
       COPY "LIBLP/LPCDB1RN.CPY".
       COPY "LIBGB/FERRORS.CPY".

      ******************************************************************
       END-ROUTINE SECTION.
      ******************************************************************

           PERFORM CLOSE-FILES.

       EXIT-PROG.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY CQSCN3-SCREEN.
           DESTROY CQSCN3-WINDOW-HANDLE.
       STOP-RUN.
           EXIT PROGRAM.
