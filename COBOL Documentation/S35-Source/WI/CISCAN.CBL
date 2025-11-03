       IDENTIFICATION DIVISION.
       PROGRAM-ID.      CISCAN.
      ********************************************************************
      *  BORROWER / LOANS FILE WINDOW SCAN
      *  CALLED FROM:
      *             WI/MODBOR (F4-SCAN)
      *             WI/BWCREA (F4-SCAN)
      *  WORLD PR# 739
      *
      *  NOTE - THIS IS KIND OF A TRICKY SCAN.  COMES IN WITH AN SSNO
      *         FROM MODBOR OR BWCREA & READS 2 SECONDARY KEYS OF CIFILE.
      *         PRIMARY KEY OF CIFILE IS LOAN #, KEY 2 IS SSNO & KEY
      *         3 IS SSNO.  SO WE HAVE TO DO THE THING WHERE WE STORE THE
      *         PRIMARY KEY IN PAGE-KEY-TABLE & "CATCH UP" THE READ-NEXT
      *         AT THE TOP OF PAGE 2 - ?? SINCE THE KEYS HAVE DUPLICATES.
      *         ALSO, AFTER SSNO CHANGES READING CI2, I JUST START CI3 &
      *         START READING FOR THAT SSNO AS SECONDARY & DISPLAY BOTH
      *         PRIMARY & SECONDARY IN SAME SCAN.  IF USER SELECTS A
      *         LINE, SEND THE LOAN # & CALL CIINQ.
      *
      * KEC 2016.1003 {PD#00003} PARADATA <A30> [JAY CISZEWSKI]
      *         FIXED CI-DATE FIELDS TO BE IN CCYYMMDD FORMAT.
      * BAH 181009 GLOBAL CIFILE
      * BAH 20191007 REMOVED ACCESS-CALL CIFILE
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
      *    W O R K I N G - S T O R A G E     S E C T I O N
      *
      ******************************************************************
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)WI/CISCAN.CBL S35 02/22/24-12:49:03 >".

      ******************************************************************
      *
      *    P R O G R A M    W O R K E R S
      *
      ******************************************************************
       COPY "LIBLP/LP01CI.CPY".
       COPY "LIBLP/LP01CI_SQL.CPY".
       COPY "LIBLP/LPWSCI.CPY".

       01  CISCAN-LINES          PIC 99  VALUE 15.
       01  CISCAN-COLUMNS        PIC 99  VALUE 38.
       01  CISCAN-BEGIN-Y        PIC 99  VALUE 6.
       01  CISCAN-BEGIN-X        PIC 99  VALUE 19.

       01  ERRCD                    PIC X      VALUE SPACES.
       01  ONE                      PIC S9     VALUE 1.
       01  SUB                      PIC 99.
       01  SCAN-TYPE                PIC 9      VALUE 2.

      ******************************************************************
      *    F L D S E L
      ******************************************************************
       01  FLDSEL-STAT              PIC XX.
       01  FLDSEL-FIELD             PIC X(4)   VALUE "010.".
       01  FLDSEL-LIST.
           03  FILLER               PIC X(36)  VALUE
           "010 020 030 040 050 060 070 080 090.".
       01  FLDSEL-BUF.
           03  FLDSEL-S.
               07  FLDSEL-NUMBER    PIC 9(6).
               07  FILLER           PIC XX.
               07  FLDSEL-DATE      PIC 99/99/99.
               07  FILLER           PIC XXX.
               07  FLDSEL-TYPE      PIC X(15).

       01  FIRST-FIELD              PIC X(4)  VALUE "010.".
       01  LAST-FIELD               PIC X(4)  VALUE "090.".

      ******************************************************************
      *    D I S P L A Y
      ******************************************************************
       01  DISPLAY-LIST.
           03  FILLER               PIC X(51)  VALUE
           "010 020 030 040 050 060 070 080 090 PGNO ENDMORE:F.".
       01  DISPLAY-BUF.
           03  D-LINE               OCCURS 9 TIMES.
               05  D-S.
                   07  D-NUMBER     PIC 9(6).
                   07  FILLER       PIC XX.
                   07  D-DATE       PIC 99/99/99.
                   07  FILLER       PIC XXX.
                   07  D-TYPE       PIC X(15).
           03  D-PAGENO             PIC ZZ9.
           03  D-END-MORE           PIC X(7).

      *----------------------------------------------------------------
       01  CURRENT-PAGE             PIC S999.
       01  PAGE-COUNT               PIC 999.
       01  MAX-PAGES                PIC 999    VALUE 50.
       01  PAGE-KEY.
           03  FILLER               OCCURS 50 TIMES.
               05  PAGE-KEY-N-X.
                   07  PAGE-KEY-SSNO    PIC 9(9).
                   07  PAGE-KEY-NUMBER  PIC 9(6).
                   07  PAGE-KEY-TYPE    PIC 9.

      ******************************************************************
      *
      *     C O P Y    L I B R A R Y    W O R K E R S
      *
      ******************************************************************
       COPY "LIBCL/CLMENUW_EXT.CPY".

       COPY "LIBWI/CISCAN_CMD.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/DATERW.CPY".

       01  CISCAN-WINDOW-HANDLE    PIC X(10).
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/FLDSELW.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBWI/CISCAN_DEF.CPY".
       COPY "LIBWI/CISCAN_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".

       COPY "LIBGB/GETFMW.CPY".
       COPY "LIBWI/CISCANW.CPY".

       SCREEN SECTION.
       COPY "LIBWI/CISCAN_SCN.CPY".

       PROCEDURE DIVISION
           USING FORM-PATHNAME CISCAN-BUF EXIT-PATHNAME.

      ******************************************************************
      *
      *     M A I N     P R O G R A M
      *
      ******************************************************************
       MAIN-PROGRAM SECTION.

           PERFORM SQL-CONNECT.

       COPY "LIBGB/CHKSEC.CPY".

      D    STOP MESS.
           MOVE " " TO VDU-WIMENU-SELECTION.

           MOVE "CISCAN" TO VDU-FORM-NAME.

           DISPLAY FLOATING GRAPHICAL WINDOW
                   AT LINE CISCAN-BEGIN-Y
                   AT COL  CISCAN-BEGIN-X
                   SIZE    CISCAN-COLUMNS + VDU-WIN-W-MOD
                   LINES   CISCAN-LINES + VDU-WIN-H-MOD
           COLOR IS VDU-WCOLOR-SCAN,CONTROL VALUE IS VDU-WCTRL-SCAN
                   CELL SIZE IS LABEL FONT
                   BLANK SCREEN
                   HANDLE  IS CISCAN-WINDOW-HANDLE.

           PERFORM FORM-RESET.

       START-SCAN.
           MOVE FIRST-FIELD TO FLDSEL-FIELD.
           MOVE 1 TO PAGE-COUNT ONE.
           MOVE 0 TO CURRENT-PAGE.
           PERFORM OPEN-CI2-FILE.

           MOVE CISCAN-SSNO TO PAGE-KEY-SSNO(1).
           MOVE 2 TO PAGE-KEY-TYPE(1).
           MOVE 0 TO PAGE-KEY-NUMBER(1).

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

           IF PAGE-KEY-TYPE(CURRENT-PAGE) = 2
              MOVE 2 TO SCAN-TYPE
              MOVE PAGE-KEY-SSNO(CURRENT-PAGE) TO CI-SSNO
           ELSE
              MOVE 3 TO SCAN-TYPE
              MOVE PAGE-KEY-SSNO(CURRENT-PAGE) TO CI-SSNO-2.

           MOVE CI-PATH-OWNBR TO CI-BRNO.

           IF SCAN-TYPE = 2
              PERFORM START-CI2-FILE
           ELSE
              PERFORM START-CI3-FILE.

       BUILD-PAGENO.
           PERFORM READ-CUST-INFO.
           IF IO-FG NOT = 0 OR SUB > 9
              GO TO DISPLAY-PAGE.

           MOVE CI-ACCTNO           TO D-NUMBER(SUB).

           MOVE CI-DATE-UPDATED     TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY         TO D-DATE(SUB).

           IF SCAN-TYPE = 2
              MOVE "BORROWER #1"   TO D-TYPE(SUB)
           ELSE
              MOVE "BORROWER #2 " TO D-TYPE(SUB).

           ADD 1 TO SUB.
           GO TO BUILD-PAGENO.

       DISPLAY-PAGE.
           IF IO-FG NOT = 0
              IF SUB = 1
                 MOVE "NO CUSTOMER INFO FOR THIS BORROWER!" TO MESS
                 PERFORM SEND-MESS
                 MOVE DISPLAY-BUF TO WR-BUF
                 MOVE DISPLAY-LIST TO WR-LIST
                 PERFORM CALL-WRFORM
                 GO TO END-ROUTINE.

           MOVE "MORE..." TO D-END-MORE.

           IF CURRENT-PAGE = PAGE-COUNT
              IF IO-FG = 0
                 IF PAGE-COUNT < MAX-PAGES
                    ADD 1 TO PAGE-COUNT

                    MOVE CI-ACCTNO    TO PAGE-KEY-NUMBER(PAGE-COUNT)
                    IF SCAN-TYPE = 2
                       MOVE 2         TO PAGE-KEY-TYPE(PAGE-COUNT)
                       MOVE CI-SSNO   TO PAGE-KEY-SSNO(PAGE-COUNT)
                    ELSE
                       MOVE 3         TO PAGE-KEY-TYPE(PAGE-COUNT)
                       MOVE CI-SSNO-2 TO PAGE-KEY-SSNO(PAGE-COUNT)
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
           MOVE "F7-EXIT, ^, V, SELECT LOAN (RETURN):" TO MESS.
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
           IF FLDSEL-STAT = "1H"
              MOVE 0 TO CURRENT-PAGE
              MOVE 1 TO ONE
              MOVE FIRST-FIELD TO FLDSEL-FIELD
              GO TO START-BUILD-PAGENO.

           IF FLDSEL-STAT = "1>"
              GO TO END-ROUTINE.

           IF FLDSEL-STAT = "10"
              GO TO START-SCAN.

           IF FLDSEL-STAT NOT = "00"
              PERFORM ALARM
              GO TO SELECT-FIELD-FROM-DISPLAY.

           MOVE FLDSEL-NUMBER     TO EXT-WI-CL-ACCTNO.
           MOVE "WI/CIINQ" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH EXIT-PATHNAME.
           CANCEL FORM-PROGX.

           GO TO START-SCAN.


      ******************************************************************
       READ-CUST-INFO SECTION.
      ******************************************************************
           IF SCAN-TYPE = 2
              GO TO READ-CUST-INFO-2
           ELSE
              GO TO READ-CUST-INFO-3.
       READ-CUST-INFO-2.
           PERFORM READ-CI2-FILE-NEXT.
           IF (IO-FG NOT = 0) OR (CI-SSNO NOT = CISCAN-SSNO) OR
                               (CI-PATH-OWNBR NOT = CI-BRNO) 
              MOVE 9 TO IO-FG
              MOVE 3 TO SCAN-TYPE
              MOVE CISCAN-SSNO TO CI-SSNO-2
              MOVE CI-PATH-OWNBR TO CI-BRNO
              PERFORM START-CI3-FILE
              GO TO READ-CUST-INFO-3.

           IF SUB = 1 AND CURRENT-PAGE NOT = 1
              IF CI-ACCTNO NOT = PAGE-KEY-NUMBER(CURRENT-PAGE)
                 GO TO READ-CUST-INFO-2.

           GO TO EXIT-READ-CUST-INFO.
       READ-CUST-INFO-3.
           PERFORM READ-CI3-FILE-NEXT.
           IF (IO-FG NOT = 0) OR (CI-SSNO-2 NOT = CISCAN-SSNO) OR
                               (CI-PATH-OWNBR NOT = CI-BRNO) 
              MOVE 9 TO IO-FG
              GO TO EXIT-READ-CUST-INFO.

           IF SUB = 1 AND CURRENT-PAGE NOT = 1
              IF CI-ACCTNO NOT = PAGE-KEY-NUMBER(CURRENT-PAGE)
                 GO TO READ-CUST-INFO-3.

       EXIT-READ-CUST-INFO.
           EXIT.

      ******************************************************************
      *
      *    C O P Y    L I B R A R Y    W O R K E R S
      *
      ******************************************************************
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/FLDSEL.CPY".

      *************************************************
       FORM-RESET SECTION.
      *************************************************
           DISPLAY CISCAN-SCREEN UPON CISCAN-WINDOW-HANDLE.
           MOVE CISCAN-FORM-FIELDS TO WR-BUF.
           MOVE CISCAN-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      *************************************************
       SETUP-LINK-INFO SECTION.
      *************************************************
           MOVE CISCAN-HELP--FILE TO HELP-LINK-FILE.
           MOVE CISCAN-HELP--DIR  TO HELP-LINK-DIR.

      *************************************************
       VDU-CONVERT-FIELD SECTION.
      *************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBWI/CISCAN_EVA.CPY".
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
           PERFORM CLOSE-CI3-FILE.
           PERFORM CLOSE-CI2-FILE.
       COPY "LIBLP/LPCIGS_SQL.CPY".
       COPY "LIBLP/LPCI2RN.CPY".
       COPY "LIBLP/LPCI3RN.CPY".
       COPY "LIBGB/FERRORS.CPY".

      ******************************************************************
       END-ROUTINE SECTION.
      ******************************************************************
           PERFORM CLOSE-FILES.

       EXIT-PROG.

       STOP-RUN.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY CISCAN-SCREEN.
           DESTROY CISCAN-WINDOW-HANDLE.
           EXIT PROGRAM.
