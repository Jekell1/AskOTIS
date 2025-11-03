       IDENTIFICATION  DIVISION.
       PROGRAM-ID.       GQSCN3.
       DATE-WRITTEN. 2000/11/30.
      ******************************************************************
      *
      *              GLOBAL COLLECTOR QUEUE(3) FILE WINDOW SCAN
      *
      * DESCRIPTION: A LISTING OF AVAILABLE GLOBAL COLLECTORS THAT
      *              HAVE A QUEUE FILE.
      *
      * REV:
      *  053196 BLL FIX LOOP IF NO RECORDS FOUND
      *  *** ****** ****************************************************
      *  MJD 001130 COPIED OVER FROM WI/CQSCN3 AND CONVERTED FOR GLOBAL
      *             (TFC PR#60)
      *  BLM 081009 IF I-AM-REGACC, ONLY DISPLAY QUEUES WHICH HAVE A
      *             GROUP ID MATCHING COLLECTORS GROUP ID, REGACC PR# 220
      *  BLM 081028 IF I-AM-REGACC, CALL CDSCAN TO SELECT QUEUE GROUP
      *             TO USE, REGACC PR# 220
      *
      * KEC 2016.0328 {PD#00003} PARADATA <A30> [JAY CISZEWSKI]
      *         CHANGED CDSCAN-WINDOW-BUF TO HAVE MATCHING CD1-KEY
      *         LAYOUTS (FOR EACH CODE TYPE; CD-TYPE).
      *         CDSCAN-CD1-KEY IS THE NEW (PRIMARY) WITH REDEFINES OF
      *         CDSCAN-??-KEY (NEW).
      *
      * BAH 190128 CDFILE BECAME CDBFILE FOR "CN" TYPE
      *  CS 190315 GQFILE IS NOW 2 SEPARATE FILES, GQFILE HOLDS THE DETAIL
      *            RECORDS, AND GQS HOLDS THE SUMMARY RECORDS.
      *            GQ-TYPE IS NOT NECESSARY BUT DONT REMOVE THE COMMENT, ITS
      *            MAKES IT EASIER TO KNOW WHICH TYPE OF RECORD YOU NEED
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
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)WI/GQSCN3.CBL S34 05/24/21-15:12:13 >".

      ******************************************************************
      *
      *    I F N - W O R K E R S
      *
      ******************************************************************
       COPY "LIBSP/SP01GQS.CPY".
       COPY "LIBSP/SP01GQS_SQL.CPY".
       COPY "LIBSP/SPWSGQS.CPY".
       COPY "LIBLP/LP01CDB.CPY".
       COPY "LIBLP/LP01CDB_SQL.CPY".
       COPY "LIBLP/LPWSCDB.CPY".

       01  GQSCN3-LINES               PIC 99  VALUE 17.
       01  GQSCN3-COLUMNS             PIC 99  VALUE 50.
       01  GQSCN3-BEGIN-Y             PIC 99  VALUE 6.
       01  GQSCN3-BEGIN-X             PIC 99  VALUE 14.

       01  ERRCD                   PIC X(1)   VALUE SPACES.
       01  ONE                     PIC S9     VALUE 1.
       01  SUB                     PIC 99.

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

      *=================================================================
      *
      *    D I S P L A Y
      *
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
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBLP/LPMDTEW.CPY".
       COPY "LIBLP/LPSTATW.CPY".

       01  GQSCN3-WINDOW-HANDLE    PIC X(10).
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/FLDSELW.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBWI/GQSCN3_DEF.CPY".
       COPY "LIBWI/GQSCN3_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".

       COPY "LIBGB/GETFMW.CPY".
       COPY "LIBWI/GQSCN3W.CPY".

       SCREEN SECTION.
       COPY "LIBWI/GQSCN3_SCN.CPY".

      ******************************************************************
      *
      *     P R O C E D U R E - D I V I S I O N
      *
      ******************************************************************
       PROCEDURE DIVISION
           USING FORM-PATHNAME GQSCN3-BUF EXIT-PATHNAME.

      ******************************************************************
      *
      *    M A I N - P R O G R A M - M O D U L E
      *
      ******************************************************************
       MAIN-PROGRAM SECTION.

           PERFORM SQL-CONNECT.

       COPY "LIBGB/CHKSEC.CPY".

      D    STOP MESS.
           MOVE "XX"          TO GQSCN3-STATUS.
           MOVE " "           TO VDU-WIMENU-SELECTION.

           MOVE "GQSCN3" TO VDU-FORM-NAME.

           DISPLAY FLOATING GRAPHICAL WINDOW
                   AT  SCREEN LINE GQSCN3-BEGIN-Y * VDU-WIN-L-OFFSET
                   AT  SCREEN COL  GQSCN3-BEGIN-X * VDU-WIN-C-OFFSET
                   SIZE    GQSCN3-COLUMNS + VDU-WIN-W-MOD
                   LINES   GQSCN3-LINES + VDU-WIN-H-MOD
           COLOR IS VDU-WCOLOR-TCLP,CONTROL VALUE IS VDU-WCTRL-TCLP
                   CELL SIZE IS LABEL FONT
                   BLANK SCREEN
                   HANDLE  IS GQSCN3-WINDOW-HANDLE.

           PERFORM FORM-RESET.

           MOVE FIRST-FIELD TO FLDSEL-FIELD.
           MOVE 1 TO PAGE-COUNT ONE.
           MOVE 0 TO CURRENT-PAGE.
           PERFORM OPEN-GQS1-FILE.
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
           MOVE PAGE-COLLID(CURRENT-PAGE) TO GQS-COLLID.
           PERFORM START-GQS1-FILE.

       BUILD-PAGENO.

           PERFORM READ-GQS1-FILE-NEXT.

           IF ( IO-FG NOT = 0 )
              GO TO DISPLAY-PAGE.

           IF ( SUB > 9 )
              GO TO DISPLAY-PAGE.

           MOVE "CN"           TO CDB-TYPE.
           MOVE GQSCN3-BRANCH  TO CDB-BRANCH.
           MOVE GQS-COLLID     TO CDB-CODE.
           PERFORM OPEN-CDB1-FILE.
           PERFORM READ-CDB1-FILE.
           PERFORM CLOSE-CDB1-FILE.
           IF ( IO-FG = 9 )
              MOVE "<NOT ON FILE>" TO D-NAME(SUB)
           ELSE
              MOVE CDB-DESC        TO D-NAME(SUB).

           MOVE GQS-COLLID      TO D-COLLID(SUB).
           MOVE GQS-TOT-REC     TO D-TOT-ACCTS(SUB).
           COMPUTE D-WORKED-ACCTS(SUB) = GQS-TOT-REC - GQS-WORKED-REC.

           ADD 1 TO SUB.
           GO TO BUILD-PAGENO.

       DISPLAY-PAGE.

           IF ( IO-FG NOT = 0 )
              IF ( SUB = 1 )
                 MOVE "NO RECORDS FOUND!" TO MESS
                 PERFORM SEND-MESS
                 MOVE "1>" TO FLDSEL-STAT
                 GO TO END-MAIN-MODULE.

           MOVE "MORE..." TO D-END-MORE.

           IF CURRENT-PAGE = PAGE-COUNT
              IF IO-FG = 0
                 IF PAGE-COUNT < MAX-PAGES
                    ADD 1 TO PAGE-COUNT
                    MOVE GQS-COLLID      TO PAGE-COLLID(PAGE-COUNT)
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

           MOVE LEGEND-01 TO MESS.
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
           IF ( FLDSEL-STAT = "1H" )
              MOVE 0 TO CURRENT-PAGE
              MOVE 1 TO ONE
              MOVE FIRST-FIELD TO FLDSEL-FIELD
              GO TO START-BUILD-PAGENO.

           IF ( FLDSEL-STAT = "1>" )
              MOVE "1>" TO GQSCN3-STATUS
              GO TO END-MAIN-MODULE.

           IF NOT (FLDSEL-STAT = "00" OR "1<")
              PERFORM ALARM
              GO TO SELECT-FIELD-FROM-DISPLAY.

           MOVE FLDSEL-COLLID TO GQSCN3-COLLID.
           MOVE "00"          TO GQSCN3-STATUS.

       END-MAIN-MODULE.

           GO TO END-ROUTINE.

      *************************************************
       FORM-RESET SECTION.
      *************************************************
           DISPLAY GQSCN3-SCREEN UPON GQSCN3-WINDOW-HANDLE.
           MOVE GQSCN3-FORM-FIELDS TO WR-BUF.
           MOVE GQSCN3-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      *************************************************
       SETUP-LINK-INFO SECTION.
      *************************************************
           MOVE GQSCN3-HELP--FILE TO HELP-LINK-FILE.
           MOVE GQSCN3-HELP--DIR  TO HELP-LINK-DIR.

      *************************************************
       VDU-CONVERT-FIELD SECTION.
      *************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBWI/GQSCN3_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ******************************************************************
       MISC-PARAGRAPHS SECTION.
      ******************************************************************
      *-----------------------------------------------------------------
       COPY "LIBGB/ACCESS.CPY".
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
       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/FLDSEL.CPY".

      ******************************************************************
       IO-ROUTINES SECTION.
      ******************************************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/DECLARE_SQL.CPY".
           PERFORM CLOSE-GQS1-FILE.
           PERFORM CLOSE-CDB1-FILE.
       COPY "LIBSP/SPGQSGS_SQL.CPY".
       COPY "LIBLP/LPCDBGS_SQL.CPY".
       COPY "LIBSP/SPGQS1RN.CPY".
       COPY "LIBLP/LPCDB1RN.CPY".
       COPY "LIBGB/FERRORS.CPY".

      ******************************************************************
       END-ROUTINE SECTION.
      ******************************************************************

           PERFORM CLOSE-FILES.

       EXIT-PROG.

       STOP-RUN.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY GQSCN3-SCREEN.
           DESTROY GQSCN3-WINDOW-HANDLE.
           EXIT PROGRAM.
