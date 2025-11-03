       IDENTIFICATION DIVISION.
       PROGRAM-ID. GLINQ.
      *****************************************************************
      *
      *  DIR : GL
      *  DESC: GENERAL LEDGER TRANSACTION INQUIRY
      *
      *=================================================================
      * REV:
      * MJD 090597 EXPANDED FIELDS TO ACCOMODATE GT-AMT = S9(9)V99.
      * KEC 031798 FIXED COMPARING BEGINNING AND ENDING DATES TO
      *             GT-DATE1.  
      * KEC 041598 FIXED 'KEC 031798' TO CONVERT GT-DATE1 TO
      *             MMDDYY FORMAT FROM YYMMDD FORMAT. WAS CREATING
      *             A PROBLEM, AT LEAST THAT IS WHAT BARB SAID!!!
      * KEC 051198 FIXED 'KEC 041598' TO CONVERT BEG-DATE AND END-DATE
      *             TO MMDDYY FORMAT FROM YYMMDD FORMAT SO GET THE
      *             CORRECT RESULT FROM CMPDAT.
      * KEC 051298 REMOVED 'LIBGB/EDITOR(W)'; ADDED 'DATE-RANGE' FIELD.
      * BLV 051298 EXPANDED D-CURBAL (OPENING BAL); FIELD WAS EXPANDED
      *            BUT WORKING STORAGE WASN'T; MADE GARBAGE ON SCREEN
      * BLV 990818 FIX PROBLEM WITH TOTAL
      *  MG 000204 FIXED VDUSTATUS/IO-FG/STAT PER DEVELOPMENT MEETING.
      * KEC 2016.0104 {PD#00003} PARADATA <A30> [JAY CISZEWSKI]
      *         FIXED KEYS & RECS FOR GT-REC.
      * KEC 2016.1129 {PR#00000} PARADATA <A30> [JAY CISZEWSKI]
      *          CHANGED GT-DATE1 GT-DATE2 GT-DATE3 AS ACTIVE CCYYMMDD.
      * BLM 170404 REMOVED LOGIC FOR CHANGE OF CENTURY RESTARTS ON GT,
      *            NOT NEEDED NOW, REWROTE LIKE SCAN
      * RMZ 2024.0531 PERFORMANCE TUNING: MODIFIED START-GT3-FILE'S 
      *               WHERE CLAUSE TO REDUCE NUMBER OF ROWS BEING
      *               PROCESSED. - #S35Q-55
      *****************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       DATA DIVISION.
       FILE SECTION.

      *****************************************************************
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(36) VALUE
           "@(#)GL/GLINQ S35 09/20/22-15:15:44 >".
      *****************************************************************
       COPY "LIBGL/GL01GT.CPY".
       COPY "LIBGL/GL01GT_SQL.CPY".
       COPY "LIBGL/GLWSGT.CPY".
       COPY "LIBGL/GL01GL.CPY".
       COPY "LIBGL/GL01GL_SQL.CPY".
       COPY "LIBGL/GLWSGL.CPY".

       01  ERRCD                    PIC X          VALUE SPACES.

       01  BEG-DATE                 PIC 9(8).
       01  END-DATE                 PIC 9(8).

       01  DATE-RANGE               PIC X(2)       VALUE SPACES.
           88  DATE-RANGE-F3-ALL                   VALUE "F3".

       01  CURRENT-PAGE             PIC S999     VALUE 0.
       01  PAGE-COUNT               PIC 999      VALUE 0.
       01  COUNTER                  PIC 99       VALUE 0.
       01  INITIAL-BAL              PIC S9(11)V99 VALUE +0.
       01  WK-PERIOD                PIC 99.
       01  WK-CUR-BAL               PIC S9(11)V99 VALUE +0.
       01  GL-DISPLAY-LIST          PIC X(17) VALUE
           "DESC TYPE CURBAL.".
       01  GL-DISPLAY-BUF.
           03  D-DESC               PIC X(36).
           03  D-TYPE               PIC X.
           03  D-CURBAL             PIC ZZ,ZZZ,ZZZ,ZZ9.99-.
       01  MAX-PAGES                PIC 99 VALUE 14.
       01  PAGE-KEY-TABLE.
           03  PAGE-KEY            PIC X(27) OCCURS 400.
       01  TOTAL-TABLE             VALUE ZEROS.
           03  PAGE-TOTAL          PIC S9(11)V99 OCCURS 400.

      *************************************
      *       SCROLLING LINE WORKERS      *
      *************************************
       01  ONE                      PIC S9   VALUE 1.
       01  SUB     PIC 999.
       01  DISPLAY-LIST.
           03  FILLER              PIC X(28)  VALUE
           "L01 L02 L03 L04 L05 L06 L07 ".
           03  FILLER              PIC X(28)  VALUE
           "L08 L09 L10 L11 L12 L13 L14.".
       01  DISPLAY-BUF.
           03  D-LINE OCCURS 14.
               05  L-010            PIC 99/99/99.
               05  FILLER           PIC XX.
               05  L-020            PIC X.
               05  FILLER           PIC XX.
               05  L-030            PIC ZZ.
               05  FILLER           PIC XX.
               05  L-040            PIC 9.
               05  FILLER           PIC XXX.
               05  L-050            PIC X(14).
               05  FILLER           PIC X.
               05  L-060            PIC X(29).
               05  FILLER           PIC X.
               05  L-070            PIC ZZZZZZ,ZZZ.ZZ-.

       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       01  GLINQ-WINDOW-HANDLE  PIC X(10).
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBGL/GLINQ_DEF.CPY".
       COPY "LIBGL/GLINQ_WKS.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/HELPLINK.CPY".

       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBGL/GLINQ_SCN.CPY".

      *****************************************************************
      *
      *    P R O C E D U R E   D I V I S I O N
      *
      *****************************************************************
       PROCEDURE DIVISION
            USING FORM-PATHNAME EXIT-PATHNAME.

      *****************************************************************
       MAIN-PROGRAM SECTION.
      *****************************************************************
           PERFORM SQL-CONNECT.
      D    STOP MESS.
           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "GLINQ" TO VDU-FORM-NAME.

           DISPLAY FLOATING GRAPHICAL WINDOW
                   SCREEN LINE 01 * VDU-WIN-L-OFFSET
                   SCREEN COL 01 * VDU-WIN-C-OFFSET
           COLOR IS VDU-WCOLOR-TCLP,CONTROL VALUE IS VDU-WCTRL-TCLP
                   CELL SIZE IS LABEL FONT
                   SIZE 80 + VDU-WIN-W-MOD
                   LINES 24 + VDU-WIN-H-MOD
                   BLANK SCREEN
                   HANDLE IS GLINQ-WINDOW-HANDLE.

       COPY "LIBGB/CHKSEC.CPY".

           PERFORM FORM-RESET.

           PERFORM OPEN-GL1-FILE.
           PERFORM OPEN-GT3-FILE.

      *----------------------------------------------------------------
       ENTER-PERIOD.
           MOVE "EN N020PERIOD." TO VDU.
           PERFORM SCREENIO.
           IF F7-KEY
              GO TO END-PROGRAM.
           IF NOT ENTER-KEY
              GO TO ENTER-PERIOD.
           MOVE VDUNUM0 TO WK-PERIOD.
           IF WK-PERIOD > 12 OR WK-PERIOD < 1
              GO TO ENTER-PERIOD.

      *----------------------------------------------------------------
       ENTER-BRANCH.
           PERFORM FORM-RESET.
           MOVE "SNNN020PERIOD." TO VDU.
           MOVE WK-PERIOD TO VDUNUM0.
           PERFORM SCREENIO.
           MOVE "EN N040BRANCH." TO VDU.
           PERFORM SCREENIO.
           IF F7-KEY
              GO TO END-PROGRAM.
           IF UP-KEY
              GO TO ENTER-PERIOD.
           IF NOT ENTER-KEY
              GO TO ENTER-BRANCH.
           MOVE VDUNUM0 TO GL-BRANCH.

      *----------------------------------------------------------------
       ENTER-ACCT.
           MOVE "EN N070ACCT." TO VDU.
           PERFORM SCREENIO.
           IF UP-KEY
              GO TO ENTER-PERIOD.
           IF F7-KEY
              GO TO END-PROGRAM.
           IF NOT ENTER-KEY
              GO TO ENTER-ACCT.
           MOVE VDUNUM0 TO GL-ACCOUNT.


           PERFORM READ-GL1-FILE.
           IF IO-BAD
              MOVE "ACCOUNT NOT ON FILE; PLEASE RE-ENTER!" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-BRANCH.

           MOVE GL-DESCRIPTION TO D-DESC.
           MOVE GL-TYPE        TO D-TYPE.

           MOVE ZEROS TO TOTAL-TABLE.

           MOVE 0 TO INITIAL-BAL.
           MOVE 1 TO COUNTER.
           PERFORM SUMMING-UP UNTIL COUNTER = WK-PERIOD.

           IF GL-TYPE = "C"
              SUBTRACT GL-OPENBAL FROM INITIAL-BAL
           ELSE
               ADD GL-OPENBAL TO INITIAL-BAL.

           MOVE INITIAL-BAL TO WK-CUR-BAL D-CURBAL.

           MOVE GL-DISPLAY-LIST TO WR-LIST.
           MOVE GL-DISPLAY-BUF TO WR-BUF.
           PERFORM CALL-WRFORM.

      *----------------------------------------------------------------
       ENTER-BEGDATE.

           MOVE SPACES TO DATE-RANGE.

           MOVE "ER M060BEGDATE." TO VDU.
           PERFORM SCREENIO.
           IF F3-KEY GO TO ALL-DATES.
           IF UP-KEY GO TO ENTER-ACCT.
           IF F7-KEY GO TO END-PROGRAM.
           IF NOT ENTER-KEY
              GO TO ENTER-BEGDATE.
           MOVE VDU-DATE-YYYYMMDD TO BEG-DATE.
           IF BEG-DATE = 0
              MOVE 19000101 TO BEG-DATE.

      *----------------------------------------------------------------
       ENTER-ENDDATE.
           MOVE SPACES TO DATE-RANGE.
           MOVE "ER M060ENDDATE." TO VDU.
           PERFORM SCREENIO.
           IF UP-KEY GO TO ENTER-BEGDATE.
           IF F7-KEY GO TO END-PROGRAM.
           IF NOT ENTER-KEY
              GO TO ENTER-ENDDATE.
           MOVE VDU-DATE-YYYYMMDD TO END-DATE.
           IF BEG-DATE > END-DATE
              MOVE "INVALID DATE RANGE" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-BEGDATE.

           GO TO BEGIN-BUILD-PAGENO.

       ALL-DATES.
           MOVE "F3"       TO DATE-RANGE.
           MOVE 19000101   TO BEG-DATE.
           MOVE "S  A000BEGDATE." TO VDU.
           MOVE "00/00/00" TO VDUBUF.
           PERFORM SCREENIO.
           MOVE 99991231 TO END-DATE.
           MOVE "S  A000ENDDATE." TO VDU.
           MOVE "99/99/99" TO VDUBUF.
           PERFORM SCREENIO.

       BEGIN-BUILD-PAGENO.
           MOVE 1 TO PAGE-COUNT ONE.
           MOVE 0 TO CURRENT-PAGE.

           MOVE GL-BRANCH  TO GT-BRANCH.
           MOVE GL-ACCOUNT TO GT-ACCTNO.
           MOVE BEG-DATE   TO GT-DATE3.
           MOVE " "        TO GT-DRCRFG.
           MOVE 0          TO GT-LINE.

           MOVE GT3-KEY    TO PAGE-KEY(PAGE-COUNT).

       START-BUILD-PAGENO.
           ADD ONE TO CURRENT-PAGE.
           IF CURRENT-PAGE < 1
              MOVE 1 TO CURRENT-PAGE.
           IF CURRENT-PAGE > PAGE-COUNT
              MOVE 1 TO CURRENT-PAGE.
           MOVE SPACE TO DISPLAY-BUF.
           MOVE 1 TO SUB.
           MOVE PAGE-KEY(CURRENT-PAGE) TO GT3-KEY.

           MOVE GT-BRANCH                  TO QGT3-WBEG-BRANCH
                                              QGT3-WEND-BRANCH.

           IF ( GT-BRANCH = 9999 )
              MOVE GT-BRANCH               TO QGT3-WSBEG-BRANCH
           ELSE
              COMPUTE QGT3-WSBEG-BRANCH = GT-BRANCH + 1.

           MOVE GT-ACCTNO                  TO QGT3-WBEG-ACCTNO
                                              QGT3-WEND-ACCTNO.

           IF ( GT-ACCTNO = 9999 )
              MOVE GT-ACCTNO               TO QGT3-WSBEG-ACCTNO
           ELSE
              COMPUTE QGT3-WSBEG-ACCTNO = GT-ACCTNO + 1.

           MOVE GT-DATE3                   TO SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD        TO QGT3-WBEG-DATE3.

           MOVE END-DATE                   TO SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD        TO QGT3-WEND-DATE3.

           MOVE GT-DATE3                   TO NDTE-DATE.
           MOVE 1                          TO NDTE-HOLD.
           PERFORM INCREMENT-DAYS.
           MOVE NDTE-DATE                  TO SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD        TO QGT3-WSBEG-DATE3.

           MOVE GT-DRCRFG                  TO QGT3-WBEG-DRCRFG.
           MOVE ALL "Z"                    TO QGT3-WEND-DRCRFG.

           MOVE 01                         TO SQL-TEXT-SIZE
           MOVE GT-DRCRFG                  TO SQL-TEXT.
           PERFORM SQL-TEXT-INCREMENT.
           MOVE SQL-TEXT                   TO QGT3-WSBEG-DRCRFG.

           MOVE GT-LINE                    TO QGT3-WBEG-LINE.
           MOVE 999999                     TO QGT3-WEND-LINE.

           PERFORM START-GT3-FILE.

       BUILD-PAGENO.
           PERFORM READ-GT3-FILE-NEXT.
           IF IO-BAD OR SUB > 14
              GO TO DISPLAY-PAGE.

           IF ( GT-ACCTNO NOT = GL-ACCOUNT ) OR
              ( GT-BRANCH NOT = GL-BRANCH  )
              GO TO DISPLAY-PAGE.

           IF ( DATE-RANGE-F3-ALL )
              GO TO CONTINUE-BUILD-PAGE.

           IF GT-DATE1 < BEG-DATE OR GT-DATE1 > END-DATE
              GO TO BUILD-PAGENO.

       CONTINUE-BUILD-PAGE.
           IF ( SUB = 1 )
              MOVE GT3-KEY TO PAGE-KEY(PAGE-COUNT).

           MOVE GT-DATE1    TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO L-010(SUB).
           MOVE GT-FLAG     TO L-020(SUB).
           MOVE GT-PERIOD   TO L-030(SUB).
           MOVE GT-SOURCE   TO L-040(SUB).
           MOVE GT-REF      TO L-050(SUB).
           MOVE GT-EXPL     TO L-060(SUB).
           MOVE GT-AMT      TO L-070(SUB).
           ADD GT-AMT TO PAGE-TOTAL(CURRENT-PAGE).
           ADD 1 TO SUB.

           GO TO BUILD-PAGENO.

      *************************************
      *    DISPLAY PAGE
      *************************************
       DISPLAY-PAGE.
           IF IO-FG NOT = 0
              IF SUB = 1
                 MOVE DISPLAY-BUF TO WR-BUF
                 MOVE DISPLAY-LIST TO WR-LIST
                 PERFORM CALL-WRFORM
                 MOVE "NO RECORDS FOUND!" TO MESS
                 PERFORM SEND-MESS
                 GO TO END-PROGRAM.

      *    MOVE "MORE..." TO D-END-MORE.
           IF CURRENT-PAGE = PAGE-COUNT
              IF IO-FG = 0
                 IF PAGE-COUNT < MAX-PAGES
                    ADD 1 TO PAGE-COUNT
                    MOVE GT3-KEY TO PAGE-KEY(PAGE-COUNT)
                 ELSE
                    MOVE "MAXIMUM PAGES REACHED!" TO MESS
                    PERFORM SEND-MESS.

           MOVE DISPLAY-BUF  TO WR-BUF.
           MOVE DISPLAY-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

           PERFORM DISPLAY-BALANCE.

       READ-SEL.
           MOVE "R RA060SEL." TO VDU.
           PERFORM SCREENIO.
           IF F1-KEY
              GO TO ENTER-BRANCH.

      * TEST PAGE UP:
           IF UP-KEY OR PAGE-UP-KEY
              IF CURRENT-PAGE = 1
                 PERFORM ALARM
                 GO TO READ-SEL
              ELSE
                 MOVE -1 TO ONE
                 SUBTRACT PAGE-TOTAL(CURRENT-PAGE) FROM WK-CUR-BAL
                 SUBTRACT PAGE-TOTAL(CURRENT-PAGE - 1) FROM WK-CUR-BAL
                 MOVE 0 TO PAGE-TOTAL(CURRENT-PAGE - 1)
                 GO TO START-BUILD-PAGENO.

      * TEST PAGE DOWN OR DOWN ARROW:
           IF DOWN-KEY OR PAGE-DOWN-KEY
              IF CURRENT-PAGE NOT < PAGE-COUNT
                 PERFORM ALARM
                 GO TO READ-SEL
              ELSE
                 MOVE 1 TO ONE
                 MOVE 0 TO PAGE-TOTAL(CURRENT-PAGE + 1)
                 GO TO START-BUILD-PAGENO.

      * TEST HOME:
           IF HOME-KEY
              MOVE 0 TO CURRENT-PAGE
              MOVE 1 TO ONE
              GO TO START-BUILD-PAGENO.

           IF F7-KEY
              GO TO END-PROGRAM.

           IF NOT ENTER-KEY
              GO TO READ-SEL.

           GO TO READ-SEL.

      *****************************************************************
       FORM-RESET SECTION.
      *****************************************************************
           MOVE EXT-CONAME-CONAME TO GLINQ-CONAME.
           MOVE EXT-CONAME-SYSDATE TO GLINQ-SYSDATE.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DISPLAY GLINQ-SCREEN UPON GLINQ-WINDOW-HANDLE.
           MOVE GLINQ-FORM-FIELDS TO WR-BUF.
           MOVE GLINQ-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      *****************************************************************
       SETUP-LINK-INFO SECTION.
      *****************************************************************
           MOVE GLINQ-HELP--FILE TO HELP-LINK-FILE.
           MOVE GLINQ-HELP--DIR  TO HELP-LINK-DIR.

      **********************************************************
       VDU-CONVERT-FIELD SECTION.
      **********************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBGL/GLINQ_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/SCREEN.CPY".

      *****************************************************************
       MISC-ROUTINES SECTION.
      *****************************************************************
       COPY "LIBGB/ACCESS.CPY".
       SUMMING-UP.
           IF GL-TYPE = "C"
               SUBTRACT GL-CYBAL(COUNTER) FROM INITIAL-BAL
           ELSE
               ADD GL-CYBAL(COUNTER) TO INITIAL-BAL.
           ADD 1 TO COUNTER.
       DISPLAY-BALANCE.
           ADD PAGE-TOTAL(CURRENT-PAGE) TO WK-CUR-BAL.
           MOVE "S  S112BALANCE." TO VDU.
           MOVE WK-CUR-BAL TO VDUNUM2.
           PERFORM SCREENIO.

      *****************************************************************
       IO-ROUTINES SECTION.
      *****************************************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/DECLARE_SQL.CPY".
           PERFORM CLOSE-GT3-FILE.
           PERFORM CLOSE-GL1-FILE.
       COPY "LIBGL/GLGTGS_SQL.CPY".
       COPY "LIBGL/GLGLGS_SQL.CPY".
       COPY "LIBGL/GLGL1RN.CPY".
      *COPY "LIBGL/GLGT3RN.CPY".
  
      * COPYMEMBER: LIBGL/GLGT3RN
      ******************************************************************
      *
      * DESCRIPTION: IO ROUTINE FOR GT3-KEY
      *
      *=================================================================
      * REV:
      * KEC 2016.0104 {PD#00003} PARADATA <A30> [JAY CISZEWSKI]
      *         REMOVED GT3-KEY (A15) AND MAKE GT4-KEY (A15) TO
      *         NOW BE THE NEW GT3-KEY (A30).
      *
      ******************************************************************
       LOAD-GT3-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( GT-PATH-OVERRIDE = "Y" )
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( GT-PATH-OVERRIDE = SPACES )
              MOVE EXT-FILPATH-BASE        TO GT-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO GT-ALL-MACHINE
              MOVE GT-ALL-PATH             TO GT-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( GT-PATH-OVERRIDE = "B" )
              MOVE FILPATH-BASE            TO GT-ALL-BASE
              MOVE FILPATH-MACHINE         TO GT-ALL-MACHINE
              MOVE GT-ALL-PATH             TO GT-PATH.

      *-----------------------------------------------------------------
       OPEN-GT3-FILE.
           PERFORM LOAD-GT3-FILE.
           PERFORM OPEN-IT.
           MOVE GT-PATH-SQL TO E-FILE.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       START-GT3-FILE.
           PERFORM START-IT.
           MOVE GT-PATH-SQL TO E-FILE.
           MOVE GT3-KEY TO E-KEYX.
           IF ( GT3-CURSOR-STAT-GOOD )
              PERFORM CLOSE-GT3-FILE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *  DEFAULT THE WBEG AND WEND; TO GET A START TO WORK IN SQL LIKE
      *  IT DOES IN VISION FILES FOR MULTIPLE SCAN PAGES, NEED TO ADD
      *  ONE TO THE WHERE SCAN BEG (WSBEG).
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( QGT3-WEND-LINE = ZEROES    ) OR
              ( QGT3-WEND-LINE NOT NUMERIC )
              MOVE GT-BRANCH            TO QGT3-WBEG-BRANCH
              MOVE ALL "9"              TO QGT3-WEND-BRANCH
              MOVE GT-ACCTNO            TO QGT3-WBEG-ACCTNO
              MOVE ALL "9"              TO QGT3-WEND-ACCTNO

              MOVE GT-DATE3             TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD  TO QGT3-WBEG-DATE3
              MOVE EXT-JULIAN-END       TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD  TO QGT3-WEND-DATE3

              MOVE GT-DRCRFG            TO QGT3-WBEG-DRCRFG
              MOVE ALL "Z"              TO QGT3-WEND-DRCRFG
              MOVE GT-LINE              TO QGT3-WBEG-LINE
              MOVE ALL "9"              TO QGT3-WEND-LINE
      *-   -   -{ ADD ONE TO WHERE SCAN BEG FIELDS }
              IF ( GT-BRANCH = 9999 )
                 MOVE GT-BRANCH         TO QGT3-WSBEG-BRANCH
              ELSE
                 COMPUTE QGT3-WSBEG-BRANCH = GT-BRANCH + 1
              END-IF

              IF ( GT-ACCTNO = 99999999 )
                 MOVE GT-ACCTNO         TO QGT3-WSBEG-ACCTNO
              ELSE
                 COMPUTE QGT3-WSBEG-ACCTNO = GT-ACCTNO + 1
              END-IF

              MOVE GT-DATE3             TO NDTE-DATE
              MOVE 1                    TO NDTE-HOLD
              PERFORM INCREMENT-DAYS
              MOVE NDTE-DATE            TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD  TO QGT3-WSBEG-DATE3

              MOVE 01                   TO SQL-TEXT-SIZE
              MOVE GT-DRCRFG            TO SQL-TEXT
              PERFORM SQL-TEXT-INCREMENT
              MOVE SQL-TEXT             TO QGT3-WSBEG-DRCRFG.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           EXEC SQL
            DECLARE GT3_CURSOR CURSOR FOR
            SELECT GTFILE.GT_FLAG,
                   GTFILE.GT_REF,
                   CAST(GTFILE.GT_DATE1 AS VARCHAR(10)),
                   GTFILE.GT_SOURCE,
                   CAST(GTFILE.GT_DATE2 AS VARCHAR(10)),
                   GTFILE.GT_BRANCH,
                   GTFILE.GT_ACCTNO,
                   CAST(GTFILE.GT_DATE3 AS VARCHAR(10)),
                   GTFILE.GT_DRCRFG,
                   GTFILE.GT_LINE,
                   CAST(GTFILE.GT_LTOUCH_DATE AS VARCHAR(10)),
                   GTFILE.GT_AMT,
                   GTFILE.GT_EXPL,
                   GTFILE.GT_PERIOD,
                   GTFILE.GT_SMITH_SENT,
                   GTFILE.GT_POST_USERID,
                   CAST(GTFILE.GT_POST_DATE AS VARCHAR(10)),
                   GTFILE.GT_POST_TIME
            FROM DBO.GTFILE
            WHERE (GTFILE.GT_BRANCH BETWEEN :QGT3-WBEG-BRANCH
                                        AND :QGT3-WEND-BRANCH
              AND  GTFILE.GT_ACCTNO BETWEEN :QGT3-WBEG-ACCTNO
                                        AND :QGT3-WEND-ACCTNO
              AND  GTFILE.GT_DATE3  BETWEEN :QGT3-WBEG-DATE3 
                                        AND :QGT3-WEND-DATE3
              AND  GTFILE.GT_DRCRFG BETWEEN :QGT3-WBEG-DRCRFG
                                        AND :QGT3-WEND-DRCRFG
              AND  GTFILE.GT_LINE   BETWEEN :QGT3-WBEG-LINE 
                                        AND :QGT3-WEND-LINE) 
            ORDER BY GTFILE.GT_BRANCH,
                     GTFILE.GT_ACCTNO,
                     GTFILE.GT_DATE3,
                     GTFILE.GT_DRCRFG,
                     GTFILE.GT_LINE,
                     GTFILE.GT_SOURCE,
                     GTFILE.GT_DATE2
           END-EXEC.

           EXEC SQL
               OPEN GT3_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE SPACES TO QGT3-WBEG-DRCRFG
                          QGT3-WEND-DRCRFG
                          QGT3-WBEG-DATE3
                          QGT3-WEND-DATE3
                          QGT3-WSBEG-DRCRFG.

           MOVE ZEROES TO QGT3-WBEG-BRANCH
                          QGT3-WEND-BRANCH
                          QGT3-WBEG-ACCTNO
                          QGT3-WEND-ACCTNO
                          QGT3-WBEG-LINE
                          QGT3-WEND-LINE
                          QGT3-WSBEG-BRANCH
                          QGT3-WSBEG-ACCTNO.

           MOVE STAT---GOOD TO GT3-CURSOR-STAT.

      *-----------------------------------------------------------------
       READ-GT3-FILE.
           PERFORM READ-IT.
           MOVE GT-PATH-SQL TO E-FILE.
           MOVE GT3-KEY TO E-KEYX.
           INITIALIZE QGT-REC.

           MOVE GT-BRANCH           TO QGT-BRANCH.
           MOVE GT-ACCTNO           TO QGT-ACCTNO.
           MOVE GT-DATE3            TO SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO QGT-DATE3.
           MOVE GT-DRCRFG           TO QGT-DRCRFG.
           MOVE GT-LINE             TO QGT-LINE.

           EXEC SQL
            SELECT GTFILE.GT_FLAG,
                   GTFILE.GT_REF,
                   CAST(GTFILE.GT_DATE1 AS VARCHAR(10)),
                   GTFILE.GT_SOURCE,
                   CAST(GTFILE.GT_DATE2 AS VARCHAR(10)),
                   GTFILE.GT_BRANCH,
                   GTFILE.GT_ACCTNO,
                   CAST(GTFILE.GT_DATE3 AS VARCHAR(10)),
                   GTFILE.GT_DRCRFG,
                   GTFILE.GT_LINE,
                   CAST(GTFILE.GT_LTOUCH_DATE AS VARCHAR(10)),
                   GTFILE.GT_AMT,
                   GTFILE.GT_EXPL,
                   GTFILE.GT_PERIOD,
                   GTFILE.GT_SMITH_SENT,
                   GTFILE.GT_POST_USERID,
                   CAST(GTFILE.GT_POST_DATE AS VARCHAR(10)),
                   GTFILE.GT_POST_TIME
            INTO :QGT-REC
            FROM DBO.GTFILE
            WHERE GTFILE.GT_BRANCH = :QGT-BRANCH
              AND GTFILE.GT_ACCTNO = :QGT-ACCTNO
              AND GTFILE.GT_DATE3  = :QGT-DATE3
              AND GTFILE.GT_DRCRFG = :QGT-DRCRFG
              AND GTFILE.GT_LINE   = :QGT-LINE
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-GT3-FILE.

           IF ( IO-FG = 0 )
              PERFORM GET-GT-FIELDS.

      *-----------------------------------------------------------------
       READ-GT3-FILE-NEXT.
           PERFORM READ-IT.
           MOVE GT-PATH-SQL TO E-FILE.
           MOVE GT3-KEY TO E-KEYX.
           INITIALIZE QGT-REC.

           IF ( GT3-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH GT3_CURSOR INTO :QGT-REC
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-GT3-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-GT-FIELDS.

      *-----------------------------------------------------------------
       CLOSE-GT3-FILE.
           PERFORM CLOSE-IT.
           IF ( GT3-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE GT3_CURSOR
              END-EXEC
              MOVE STAT---BAD TO GT3-CURSOR-STAT.

      *----------------------------------------------------------------

       COPY "LIBGB/FERRORS.CPY".

      *----------------------------------------------------------------
       END-PROGRAM.
           PERFORM CLOSE-GT3-FILE.
           PERFORM CLOSE-GL1-FILE.

       EXIT-PROG.
           MOVE "GL/GLMENU" TO FORM-NAM.
           MOVE FORM-PATH TO FORM-PATHNAME.

       STOP-RUN.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY GLINQ-SCREEN.
           DESTROY GLINQ-WINDOW-HANDLE.
           EXIT PROGRAM.
