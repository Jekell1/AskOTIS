       IDENTIFICATION DIVISION.
       PROGRAM-ID.      CJSCAN.
      ******************************************************************
      *  COLLECTOR JOURNAL WINDOW SCAN
      *       CALLED FROM MN/CJMAIN
      *
      * REV:
      *  GLB  030218 NEW; FROM CJSCAN
      *  MJD 131209 30 CONVERSION COMPLETED
      *  BAH 160602 CHANGED CJ-DATE FROM JULIAN TO YYYYMMDD
      *  BAH 170412 ACTIVATE 8 DIGIT DATES, PD0006
      *   CS 190118 GLOBAL CJFILE
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


       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)WI/CJSCAN.CBL S35 04/24/24-08:23:02 >".

       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBSP/SP01CJ.CPY".
       COPY "LIBSP/SP01CJ_SQL.CPY".
       COPY "LIBSP/SPWSCJ.CPY".

       01  CJSCAN-LINES          PIC 99  VALUE 17.
       01  CJSCAN-COLUMNS        PIC 99  VALUE 80.
       01  CJSCAN-BEGIN-Y        PIC 99  VALUE 6.
       01  CJSCAN-BEGIN-X        PIC 99  VALUE 1.

       01  ERRCD                    PIC X      VALUE SPACES.
       01  ONE                      PIC S9     VALUE 1.
       01  SUB                      PIC 99.
       01  FIRST-TIME               PIC X      VALUE "Y".

      *----------------------------------------------------------------
      * HOLD KEY  USED TO STORE CJ1-KEY WHILE CLEARING NEW RECORD.
      *----------------------------------------------------------------
       01  HOLD-KEY             PIC X(21).
       01  FILLER REDEFINES HOLD-KEY.
           03  HOLD-BRNO           PIC 9(4).
           03  HOLD-COLLID         PIC XXX.
           03  HOLD-DATE           PIC 9(8).
           03  HOLD-START-TIME     PIC 9(4).
           03  HOLD-SEQNO          PIC 99.

       01  FLDSEL-STAT              PIC XX.
       01  FLDSEL-FIELD             PIC X(4)   VALUE "010.".
       01  FLDSEL-FIELD-NUM        REDEFINES FLDSEL-FIELD.
           03  FLDSEL-FIELD-N       PIC 99.
           03  FILLER               PIC XX.
       01  FLDSEL-LIST.
           03  FILLER               PIC X(36)  VALUE
           "010 020 030 040 050 060 070 080 090.".
       01  FLDSEL-BUF.
           03  FLDSEL-T.
               05  FLDSEL-T-BRNO     PIC ZZZ9.
               05  FILLER            PIC XX.
               05  FLDSEL-T-COLLID   PIC XXX.
               05  FILLER            PIC X.
               05  FLDSEL-T-DATE     PIC 99/99/99.
               05  FILLER            PIC X.
               05  FLDSEL-T-START    PIC 9(4).
               05  FLDSEL-T-DASH1    PIC X.
               05  FLDSEL-T-END      PIC 9(4).
               05  FILLER            PIC XX.
               05  FLDSEL-T-ACCTNO   PIC Z(6).
               05  FLDSEL-T-ACTIVITY PIC X(3).
               05  FILLER            PIC XX.
               05  FLDSEL-T-RESULT   PIC X(3).
               05  FILLER            PIC XX.
               05  FLDSEL-T-ACTION   PIC X(4).
               05  FILLER            PIC XX.
               05  FLDSEL-T-CONSTAT  PIC X(3).
               05  FILLER            PIC XX.
               05  FLDSEL-T-RECSTAT  PIC X(3).
               05  FILLER            PIC XX.
               05  FLDSEL-T-PROMISE  PIC 99/99/99.
           03  FLDSEL-P            REDEFINES FLDSEL-T.
               05  FLDSEL-P-AREA     PIC X(70).

       01  FIRST-FIELD              PIC X(4)  VALUE "010.".
       01  LAST-FIELD               PIC X(4)  VALUE "090.".

      *01  TYPE-BUF.
      *    03  TYPE-SCAN            PIC X.
      *    03  TYPE-DESC            PIC X(7)  VALUE SPACES.
      *01  TYPE-LIST                PIC X(11) VALUE "TYPE ATYPE.".
      *01  PROMPT-LIST              PIC X(6)  VALUE "000:S.".
       01  PT-LIST                  PIC X(17) VALUE "TYPE ATYPE 000:S.".
       01  PT-BUF.
           03  TYPE-SCAN            PIC X.
           03  TYPE-DESC            PIC X(7)  VALUE SPACES.
           03  HEAD-BUF             PIC X(77).


       01  DISPLAY-LIST.
           03  FILLER               PIC X(51)  VALUE
           "010 020 030 040 050 060 070 080 090 PGNO ENDMORE:F.".
       01  DISPLAY-BUF.
           03  D-LINE               OCCURS 9 TIMES.
      *        TYPE 1  -  ALL DETAIL
               05  D-DETAIL.
                   07  D-T-BRNO          PIC ZZZ9.
                   07  FILLER            PIC X(3).
                   07  D-T-COLLID        PIC XXX.
                   07  FILLER            PIC X(3).
                   07  D-T-DATE          PIC 99/99/99.
                   07  FILLER            PIC XX.
                   07  D-T-START         PIC 9(4).
                   07  D-T-DASH1         PIC X.
                   07  D-T-END           PIC 9(4).
                   07  FILLER            PIC XX.
                   07  D-T-ACCTNO        PIC Z(6).
                   07  FILLER            PIC XX.
                   07  D-T-ACTIVITY      PIC X(3).
                   07  FILLER            PIC XX.
                   07  D-T-RESULT        PIC X(3).
                   07  FILLER            PIC XX.
                   07  D-T-ACTION        PIC X(4).
                   07  FILLER            PIC X(3).
                   07  D-T-CONSTAT       PIC X(3).
                   07  FILLER            PIC X(3).
                   07  D-T-RECSTAT       PIC X(3).
                   07  FILLER            PIC X.
                   07  D-T-PROMISE       PIC 99/99/99.

           03  D-PAGENO             PIC ZZ9.
           03  D-END-MORE           PIC X(7).

       01  NON-DISP-BUF.
           03  D-NDLINE               OCCURS 9 TIMES.
               05  D-KEY-BRNO       PIC 9(4).
               05  D-KEY-COLLID     PIC X(3).
               05  D-KEY-DATE       PIC 9(8).
               05  D-KEY-START-TIME PIC 9(4).
               05  D-KEY-SEQNO      PIC 9(2).


       01  CURRENT-PAGE             PIC S999.
       01  PAGE-COUNT               PIC 999.
       01  MAX-PAGES                PIC 999    VALUE 250.

       01  PAGE-KEY.
           03  FILLER                   OCCURS 250 TIMES.
               05  PAGE-KEY2            PIC X(30).
               05  PAGE-KEY1.
                   07  PAGE-KEY-BRNO         PIC 9(4).
                   07  PAGE-KEY-COLLID       PIC X(3).
                   07  PAGE-KEY-DATE         PIC 9(8).
                   07  PAGE-KEY-START-TIME   PIC 9(4).
                   07  PAGE-KEY-SEQNO        PIC 9(2).



       COPY "LIBWI/CJSCAN_CMD.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".

       01 CJSCAN-WINDOW-HANDLE PIC X(10) VALUE SPACES.
       COPY "LIBGB/FLDSELW.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBWI/CJSCAN_DEF.CPY".
       COPY "LIBWI/CJSCAN_WKS.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/ACCESSW.CPY".

       COPY "LIBGB/GETFMW.CPY".
       COPY "LIBWI/CJSCANW.CPY".

       SCREEN SECTION.
       COPY "LIBWI/CJSCAN_SCN.CPY".

       PROCEDURE DIVISION
            USING FORM-PATHNAME CJSCAN-BUF EXIT-PATHNAME.

      ******************************
       MAIN-PROGRAM SECTION.
      ******************************
      D    STOP MESS.
           PERFORM SQL-CONNECT.

       COPY "LIBGB/CHKSEC.CPY".
           MOVE "CJSCAN" TO VDU-FORM-NAME.
           MOVE FORM-PATHNAME TO FORM-PATH.
           DISPLAY FLOATING GRAPHICAL WINDOW
                    SCREEN LINE CJSCAN-BEGIN-Y * VDU-WIN-L-OFFSET
                    SCREEN COL CJSCAN-BEGIN-X * VDU-WIN-C-OFFSET
           COLOR IS VDU-WCOLOR-SCAN,CONTROL VALUE IS VDU-WCTRL-SCAN
                   CELL SIZE IS LABEL FONT
                   SIZE CJSCAN-COLUMNS + VDU-WIN-W-MOD
                   LINES CJSCAN-LINES + VDU-WIN-H-MOD
                   BLANK SCREEN
                   HANDLE IS CJSCAN-WINDOW-HANDLE.
            PERFORM FORM-RESET.

           MOVE " "  TO VDU-WIMENU-SELECTION.

           MOVE "XX" TO CJSCANW-STATUS.

           MOVE CJSCANW-CJ1-KEY TO HOLD-KEY.

           PERFORM SEND-HEADER-INFO.

           IF HOLD-BRNO = 0
              GO TO ENTER-BRNO
           ELSE
              GO TO CONTINUE-BRNO.

      *************************************
      *    ENTER BRANCH NO.
      *************************************
       ENTER-BRNO.
           MOVE LEGEND-02 TO MESS.
           PERFORM SEND-LEGEND.

           MOVE "ENNN040BRNO." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>"
              GO TO END-ROUTINE.
           IF VDUSTATUS NOT = "00"
              GO TO ENTER-BRNO.

           IF VDUNUM0 = 0
              MOVE "SN N040BRNO." TO VDU
              MOVE HOLD-BRNO TO VDUNUM0
              PERFORM SCREENIO.
           MOVE VDUNUM0 TO HOLD-BRNO.

       CONTINUE-BRNO.
           PERFORM OPEN-BR-FILE.
           MOVE HOLD-BRNO TO BR-NO.
           PERFORM READ-BR-FILE.
           PERFORM CLOSE-BR-FILE.
           IF IO-FG = 9
              MOVE "MISSING BRANCH...." TO BR-NAME.

           MOVE "S  A000BRNAME." TO VDU.
           MOVE BR-NAME TO VDUBUF.
           PERFORM SCREENIO.

      *************************************
      *    ENTER COLLECTOR ID.
      *************************************
       ENTER-COLLID.
           MOVE LEGEND-02 TO MESS.
           PERFORM SEND-LEGEND.

           MOVE "E  A000COLLID." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>"
              GO TO END-ROUTINE.
           IF VDUSTATUS = "1U"
              GO TO ENTER-BRNO.
           IF VDUSTATUS = "10"
              GO TO ENTER-BRNO.
           IF VDUSTATUS NOT = "00"
              GO TO ENTER-COLLID.

           IF VDUBUF = SPACES
              MOVE "S  A000COLLID." TO VDU
              MOVE HOLD-COLLID TO VDUBUF
              PERFORM SCREENIO.
           MOVE VDUBUF TO HOLD-COLLID.
           IF HOLD-COLLID = SPACES
              MOVE "S  A000COLLID." TO VDU
              MOVE "ALL" TO HOLD-COLLID VDUBUF
              PERFORM SCREENIO.

       CONTINUE-COLLID.

           IF HOLD-COLLID = SPACES
              MOVE 0 TO HOLD-DATE HOLD-START-TIME HOLD-SEQNO.



      *************************************
       ENTER-DATE.
      *************************************
           MOVE LEGEND-03B TO MESS.
           PERFORM SEND-LEGEND.

           MOVE "ERNM060DT1." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U" GO TO ENTER-COLLID.
           IF VDUSTATUS = "10" GO TO ENTER-BRNO.
           IF VDUSTATUS = "1>" GO TO END-ROUTINE.
           IF STAT NOT = "00" AND NOT = "1D" GO TO ENTER-DATE.

           MOVE VDU-DATE-YYYYMMDD TO NUM-DATE HOLD-DATE.

           IF FIRST-TIME = "Y"
              MOVE "N" TO FIRST-TIME
              GO TO BEGIN-THE-SCAN.

      *------------------------------------------------------------
      *  BEGIN COLLECTOR JOURNAL DETAIL PAGE
      *------------------------------------------------------------
       BEGIN-THE-SCAN.
           MOVE FIRST-FIELD TO FLDSEL-FIELD.
           MOVE 1 TO PAGE-COUNT ONE.
           MOVE 0 TO CURRENT-PAGE.

           MOVE HOLD-BRNO          TO PAGE-KEY-BRNO  (1).
           MOVE HOLD-COLLID        TO PAGE-KEY-COLLID (1).
           MOVE HOLD-DATE          TO PAGE-KEY-DATE  (1).
           MOVE HOLD-START-TIME    TO PAGE-KEY-START-TIME(1).
           MOVE HOLD-SEQNO         TO PAGE-KEY-SEQNO   (1).

       START-BUILD-PAGENO.
           ADD ONE TO CURRENT-PAGE.
           IF CURRENT-PAGE < 1
              MOVE FIRST-FIELD TO FLDSEL-FIELD
              MOVE 1 TO CURRENT-PAGE.
           IF CURRENT-PAGE > PAGE-COUNT
              MOVE FIRST-FIELD TO FLDSEL-FIELD
              MOVE 1 TO CURRENT-PAGE.

           PERFORM OPEN-CJ1-FILE.
           MOVE SPACES TO DISPLAY-BUF
                          NON-DISP-BUF.
           MOVE 1 TO SUB.

           MOVE PAGE-KEY1(CURRENT-PAGE)         TO CJ1-KEY.
           IF CJ-DATE = 0
              MOVE 19000101 TO CJ-DATE.

           PERFORM START-CJ1-FILE.

      *************************************
      *    BUILD PAGE
      *************************************
       BUILD-PAGENO.
           PERFORM READ-CJ1-FILE-NEXT.
           IF (IO-FG NOT = 0) OR (CJ-BRNO NOT = CJ-PATH-OWNBR) OR
               (SUB > 9)
              GO TO DISPLAY-PAGE.

           IF CJ-BRNO NOT = HOLD-BRNO
              MOVE 9 TO IO-FG
              GO TO DISPLAY-PAGE.

           IF HOLD-COLLID NOT = "ALL"
              IF CJ-COLLID NOT = HOLD-COLLID
                 MOVE 9 TO IO-FG
                 GO TO DISPLAY-PAGE.

           IF HOLD-DATE NOT = 0
              IF CJ-DATE < HOLD-DATE
                 GO TO BUILD-PAGENO.

      *------- DISPLAY ALL DETAIL ---------

           MOVE CJ-BRNO          TO D-T-BRNO(SUB).
           MOVE CJ-COLLID        TO D-T-COLLID(SUB).
           MOVE CJ-DATE          TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY      TO D-T-DATE(SUB).
           MOVE CJ-START-TIME    TO D-T-START(SUB).
           MOVE "-"              TO D-T-DASH1(SUB)
           MOVE CJ-END-TIME      TO D-T-END  (SUB).
           MOVE CJ-ACCTNO        TO D-T-ACCTNO(SUB).
           MOVE CJ-ACTIVITY-CD   TO D-T-ACTIVITY(SUB).
           MOVE CJ-RESULT-CD     TO D-T-RESULT  (SUB).
           MOVE CJ-ACTION-CD     TO D-T-ACTION  (SUB).
           MOVE CJ-CON-STAT      TO D-T-CONSTAT (SUB).
           MOVE CJ-REC-STAT      TO D-T-RECSTAT (SUB).
           MOVE CJ-PROMISE-DATE1 TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY      TO D-T-PROMISE (SUB).

           MOVE CJ-BRNO           TO D-KEY-BRNO(SUB).
           MOVE CJ-COLLID         TO D-KEY-COLLID(SUB).
           MOVE CJ-DATE           TO D-KEY-DATE(SUB).
           MOVE CJ-START-TIME     TO D-KEY-START-TIME(SUB).
           MOVE CJ-SEQNO          TO D-KEY-SEQNO(SUB).

           ADD 1 TO SUB.
           GO TO BUILD-PAGENO.

      *************************************
      *    DISPLAY PAGE
      *************************************
       DISPLAY-PAGE.
           IF IO-FG NOT = 0
              IF SUB = 1
                 MOVE "NO RECORDS FOUND!" TO D-DETAIL(1)
                 MOVE DISPLAY-BUF TO WR-BUF
                 MOVE DISPLAY-LIST TO WR-LIST
                 PERFORM CALL-WRFORM
                 GO TO ENTER-DATE.

           MOVE "MORE..." TO D-END-MORE.
           IF CURRENT-PAGE = PAGE-COUNT
              IF IO-FG = 0
                 IF PAGE-COUNT < MAX-PAGES
                    ADD 1 TO PAGE-COUNT
                    MOVE CJ1-KEY       TO PAGE-KEY1(PAGE-COUNT)
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

      *************************************
      *    SELECT FIELD FROM PAGES
      *************************************
       SELECT-FIELD-FROM-DISPLAY.
           MOVE LEGEND-04 TO MESS.
           PERFORM SEND-LEGEND.

           PERFORM CALL-FLDSEL.

      * TEST UP ARROW:
           IF FLDSEL-STAT = "1U"
              GO TO ENTER-BRNO.

      * TEST PAGE UP:
           IF FLDSEL-STAT = "1P"
              IF CURRENT-PAGE = 1
                 DISPLAY OMITTED WITH BEEP
                 GO TO SELECT-FIELD-FROM-DISPLAY
              ELSE
                 MOVE -1 TO ONE
                 MOVE FIRST-FIELD TO FLDSEL-FIELD
                 GO TO START-BUILD-PAGENO.

      * TEST PAGE DOWN OR DOWN ARROW:
           IF FLDSEL-STAT = "1N" OR FLDSEL-STAT = "1D"
              IF CURRENT-PAGE NOT < PAGE-COUNT
                 DISPLAY OMITTED WITH BEEP
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
              GO TO ENTER-BRNO.
           IF NOT (FLDSEL-STAT = "00")
              DISPLAY OMITTED WITH BEEP
              GO TO SELECT-FIELD-FROM-DISPLAY.

           MOVE  D-KEY-BRNO(FLDSEL-FIELD-N)     TO  CJSCANW-BRNO.
           MOVE  D-KEY-COLLID(FLDSEL-FIELD-N)   TO  CJSCANW-COLLID.
           MOVE  D-KEY-START-TIME(FLDSEL-FIELD-N)
                   TO CJSCANW-START-TIME.
           MOVE  D-KEY-DATE(FLDSEL-FIELD-N)      TO  CJSCANW-DATE.
           MOVE  D-KEY-SEQNO(FLDSEL-FIELD-N)     TO  CJSCANW-SEQNO.

           MOVE "00" TO CJSCANW-STATUS.

           GO TO END-ROUTINE.

      ***********************************************************
       FORM-RESET SECTION.
      ***********************************************************
           DISPLAY SPACES UPON CJSCAN-WINDOW-HANDLE
                               LINE 0 COL 0 ERASE SCREEN.
           DISPLAY CJSCAN-SCREEN UPON CJSCAN-WINDOW-HANDLE.
           MOVE CJSCAN-FORM-FIELDS TO WR-BUF.
           MOVE CJSCAN-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      ***********************************************************
       SETUP-LINK-INFO SECTION.
      ***********************************************************
           MOVE CJSCAN-HELP--FILE TO HELP-LINK-FILE.
           MOVE CJSCAN-HELP--DIR TO HELP-LINK-DIR.

      ***********************************************************
       VDU-CONVERT-FIELD SECTION.
      ***********************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBWI/CJSCAN_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/FLDSEL.CPY".

      **************************
       MISC-PARAGRAPHS SECTION.
      **************************
       COPY "LIBGB/ACCESS.CPY".
       SEND-LEGEND.
           MOVE "S  A600WSEL:D." TO VDU.
           MOVE MESS TO VDUBUF.
           PERFORM SCREENIO.

       SEND-HEADER-INFO.
           MOVE 1                 TO TYPE-SCAN.
           MOVE "BALS"            TO TYPE-DESC.
           MOVE BAL-PROMPT        TO HEAD-BUF.
           MOVE "DETAIL"          TO TYPE-DESC.
           MOVE DETAIL-PROMPT     TO HEAD-BUF.

           MOVE PT-BUF TO WR-BUF.
           MOVE PT-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

           IF HOLD-BRNO NOT = 0
              MOVE "SN N040BRNO." TO VDU
              MOVE HOLD-BRNO TO VDUNUM0
              PERFORM SCREENIO.
           IF HOLD-COLLID NOT = SPACES
              MOVE "S  A000COLLID." TO VDU
              MOVE HOLD-COLLID TO VDUBUF
              PERFORM SCREENIO.
           IF HOLD-DATE NOT = 0
              MOVE HOLD-DATE TO DATE-YYYYMMDD
              PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
              MOVE "S  A000DT1." TO VDU
              MOVE DATE-MMDDYY TO VDU-DATE-EDIT
              PERFORM SCREENIO.


      ****************************
       IO-ROUTINES SECTION.
      ****************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/DECLARE_SQL.CPY".
           PERFORM CLOSE-CJ1-FILE.
           PERFORM CLOSE-BR-FILE.
       COPY "LIBSP/SPCJGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBSP/SPCJ1RN.CPY".
       COPY "LIBGB/FERRORS.CPY".

      ****************************
       END-ROUTINE SECTION.
      ****************************
           PERFORM CLOSE-FILES.
       EXIT-PROG.
       STOP-RUN.
           DESTROY CJSCAN-WINDOW-HANDLE
           MOVE FORM-PATH TO FORM-PATHNAME.
           EXIT PROGRAM.
