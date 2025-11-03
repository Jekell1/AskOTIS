       IDENTIFICATION DIVISION.
       PROGRAM-ID.      FXGLUP.
      **************************************************************************
      *               G/L MASTER FILE UPDATE REVERSAL
      * REMOVE AN UPDATE
      *
      *  NOTE: THIS PROGRAM GL/FXGLUP MUST EXIST IN THE SPECIAL PASSWORD
      *          FILE (PWFILE). THE PASSWORD CAN BE SET TO 'N', BUT IT
      *         HAS TO EXIST.
      *
      * MG  000203 FIXED VDUSTATUS/IO-FG/STAT PER DEVELOPMENT MEETING.
      * JTG 040105 CHANGED TO ENTER FISCAL PERIOD FIRST.
      *            CHANGED GL UPDATE TO TEST NEW GLOBAL PARAMETER FLAG
      *            GP-CLOSED-FISCAL-FG, TO DETERMINE IF GL FISCAL PERIOD
      *            HAS BEEN CLOSED. IF CLOSED STOP UPDATE.           WORLD #J040
      * BLF 070116 MAKE LOG ENTRY IN /USR/DATA/R1/LG/GL.YYMMDD TO TRACK
      *            WHEN THIS UPDATE WAS RUN, BY WHOM, & RANGES SELECTED,
      *            SUPPORT PR# 2780
      * BLF 080306 ADD SOURCE CODE PARAMETER, HONOR, PER JR
      * KEC 2016.1201 {PR#00000} PARADATA <A30> [JAY CISZEWSKI]
      *          CHANGED GT-DATE1 GT-DATE2 GT-DATE3 AS ACTIVE CCYYMMDD.
      * BLM 170329 ACTIVATE 8 DIGIT DATES, PD0006
      * RMZ 2024.0524 PERFORMANCE TUNING - ADDED STORED PROCEDURE S35Q-39
      * RMZ 2024.0806 ADDED LTOUCH-DATE TO UPDATE SCRIPTS. (S35Q-108)
      * RMZ 0225.0228 REMOVED GLFILE FROM THE STORED PROCEDURE BECAUSE
      *               THE TABLE IS BEING UPDATED IN THIS PROGRAM.(S35Q-167)
      *
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       DATA DIVISION.
       FILE SECTION.


       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(37) VALUE
           "@(#)GL/FXGLUP S35 05/28/24-10:13:56 >".
       COPY "LIBGL/GL01GT.CPY".
       COPY "LIBGL/GL01GT_SQL.CPY".
       COPY "LIBGL/GLWSGT.CPY".
       COPY "LIBGL/GL01GL.CPY".
       COPY "LIBGL/GL01GL_SQL.CPY".
       COPY "LIBGL/GLWSGL.CPY".

       01  GT1-CONN-OPEN-SW   PIC X        VALUE "N".

       01  ERRCD              PIC X         VALUE SPACES.
       01  ACCEPT-BUF         PIC X         VALUE SPACES.

       01  BEG-BRANCH         PIC 9(4) VALUE 0.
       01  END-BRANCH         PIC 9(4) VALUE 0.
       01  BEG-DATE           PIC 9(8) COMP-3 VALUE 0.
       01  END-DATE           PIC 9(8) COMP-3 VALUE 0.
       01  PERIOD-ALP.
           03  PERIOD         PIC 99        VALUE 0.
       01  IO-TYPE            PIC X(3)      VALUE SPACES.
       01  SOURCE-CODE        PIC 9.
       01  ALL-SRC-CODES REDEFINES SOURCE-CODE  PIC X.


       01  GT-AMT-WORKER      PIC S9(9)V99 COMP-3 VALUE 0.

       01  BAD-GL-LIST        PIC X(14)     VALUE
           "GLHEAD GLDATA.".
       01  BAD-GL-BUF.
           03  FILLER         PIC X(30)     VALUE
                             "SRC G/L NO. NO.   DATE   LINE ".
           03  GLDATA         VALUE SPACES.
               05  FILLER     PIC X.
               05  BAD-SOURCE PIC X.
               05  FILLER     PIC X(1).
               05  BAD-GLBR   PIC 9(4).
               05  FILLER     PIC X.
               05  BAD-GLACCT PIC 9(7).
               05  FILLER     PIC X.
               05  BAD-DATE   PIC 99/99/99.
               05  FILLER     PIC X(1).
               05  BAD-DRCRFG PIC X.
               05  FILLER     PIC X.
               05  BAD-LINE   PIC 9.
               05  FILLER     PIC X(1).

       01  LOG-UPDATE-MSG.
           03  FILLER         PIC X(6) VALUE " PER: ".
           03  LOG-PERIOD     PIC 99.
           03  FILLER         PIC XX   VALUE " ".
           03  LOG-BEG-DATE   PIC 9999/99/99.
           03  FILLER         PIC X    VALUE "-".
           03  LOG-END-DATE   PIC 9999/99/99.
           03  FILLER         PIC X(8) VALUE "  BR #: ".
           03  LOG-BEG-BR     PIC 9999.
           03  FILLER         PIC X    VALUE "-".
           03  LOG-END-BR     PIC 9999.
           03  FILLER         PIC X(6) VALUE " SRC: ".
           03  LOG-SRC        PIC X.

      *COPY "LIBWI/WMMESSW.CPY".
       COPY "LIBGB/PASSWDW.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/GPENVW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       01  FXGLUP-WINDOW-HANDLE  PIC X(10).
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBGL/FXGLUP_DEF.CPY".
       COPY "LIBGL/FXGLUP_WKS.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/GETFMW.CPY".

       SCREEN SECTION.
       COPY "LIBGL/FXGLUP_SCN.CPY".

       PROCEDURE DIVISION
           USING FORM-PATHNAME EXIT-PATHNAME.

      **************************************
       MAIN-PROGRAM SECTION.
      **************************************
           PERFORM SQL-CONNECT.

      D    STOP MESS.
           MOVE "FXGLUP" TO VDU-FORM-NAME.

           DISPLAY FLOATING GRAPHICAL WINDOW
                   SCREEN LINE 01 * VDU-WIN-L-OFFSET
                   SCREEN COL 01 * VDU-WIN-C-OFFSET
           COLOR IS VDU-WCOLOR-TCLP,CONTROL VALUE IS VDU-WCTRL-TCLP
                   CELL SIZE IS LABEL FONT
                   SIZE 80 + VDU-WIN-W-MOD
                   LINES 24 + VDU-WIN-H-MOD
                   BLANK SCREEN
                   HANDLE IS FXGLUP-WINDOW-HANDLE.

       COPY "LIBGB/CHKSEC.CPY".
           PERFORM FORM-RESET.

           PERFORM GET-GPENV.

      D    STOP MESS.
           PERFORM INITIALIZATION.
           IF IO-TYPE = "END"
              GO TO END-ROUTINE.

           PERFORM UPDATE-GL1-FILE.

           GO TO END-ROUTINE.

      *********************************************************
       INITIALIZATION SECTION.
      *********************************************************
           MOVE SPACES TO IO-TYPE.

           PERFORM SQL-OPEN-UPDATE-CONNECTION.

       REC-PERIOD.
           MOVE "ENNN020PERIOD." TO VDU.
           PERFORM SCREENIO.
           IF F1-KEY GO TO REC-PERIOD.
           IF F7-KEY GO TO ENDIT.
           IF NOT (ENTER-KEY OR DOWN-KEY)
              GO TO REC-PERIOD.
           MOVE VDUNUM0 TO PERIOD.
           IF PERIOD < 1 OR PERIOD > 12
              GO TO REC-PERIOD.

           MOVE GP-END-FISCAL-DATE(PERIOD) TO NDTE-DATE.
           MOVE 01                    TO NDTE-DD.
           MOVE NDTE-DATE             TO BEG-DATE.
           MOVE GP-END-FISCAL-DATE(PERIOD) TO END-DATE.

           MOVE "S  A080BEGDATE." TO VDU.
           MOVE BEG-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO VDU-DATE-EDIT.
           PERFORM SCREENIO.
           MOVE "S  A080ENDDATE." TO VDU.
           MOVE END-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO VDU-DATE-EDIT.
           PERFORM SCREENIO.

           IF GP-CLOSED-FISCAL-FG(PERIOD) = "Y"
              MOVE " " TO MESS
              STRING "GL FOR PERIOD "
                     PERIOD-ALP
                     " IS CLOSED"
                     DELIMITED BY SIZE INTO MESS
              PERFORM SEND-MESS
              MOVE "... NO FUTHER UPDATES ARE ALLOWED ..."
                                                 TO MESS
              PERFORM SEND-MESS
              GO TO REC-PERIOD.

           GO TO BR-01.
       DT-01.
           MOVE "ERNM060BEGDATE." TO VDU.
           PERFORM SCREENIO.
           IF F1-KEY GO TO REC-PERIOD.
           IF F3-KEY GO TO ALL-DT.
           IF UP-KEY GO TO REC-PERIOD.
           IF F7-KEY GO TO ENDIT.
           IF NOT (ENTER-KEY OR DOWN-KEY)
              GO TO DT-01.
           MOVE VDU-DATE-YYYYMMDD TO BEG-DATE.
       DT-02.
           MOVE "ERNM060ENDDATE." TO VDU.
           PERFORM SCREENIO.
           IF F1-KEY GO TO REC-PERIOD.
           IF UP-KEY GO TO DT-01.
           IF F7-KEY GO TO ENDIT.
           IF NOT (ENTER-KEY OR DOWN-KEY)
              GO TO DT-02.
           MOVE VDU-DATE-YYYYMMDD TO END-DATE.
           IF BEG-DATE > END-DATE
              MOVE "INVALID DATE RANGE" TO MESS
              PERFORM SEND-MESS
              GO TO DT-01.
           GO TO VERIFY-DATE-RANGE.
       ALL-DT.
           MOVE "S  A080BEGDATE." TO VDU.
           MOVE 0 TO BEG-DATE VDU-DATE-EDIT.
           PERFORM SCREENIO.
           MOVE "S  A080ENDDATE." TO VDU.
           MOVE 99999999 TO END-DATE VDU-DATE-EDIT.
           PERFORM SCREENIO.

       VERIFY-DATE-RANGE.
           MOVE GP-END-FISCAL-DATE(PERIOD) TO NDTE-DATE.
           MOVE 01                         TO NDTE-DD.
           IF BEG-DATE < NDTE-DATE
              PERFORM OUT-OF-PERIOD
              IF IO-FG NOT = 0
                 GO TO DT-01.
           IF END-DATE > GP-END-FISCAL-DATE(PERIOD)
              PERFORM OUT-OF-PERIOD
              IF IO-FG NOT = 0
                 GO TO DT-01.

       BR-01.
           MOVE "ENNN040BR1." TO VDU.
           PERFORM SCREENIO.
           IF F1-KEY GO TO REC-PERIOD.
           IF F3-KEY GO TO ALL-BR.
           IF F7-KEY GO TO ENDIT.
           IF UP-KEY GO TO DT-01.
           IF NOT (ENTER-KEY OR DOWN-KEY)
              GO TO BR-01.
           MOVE VDUNUM0 TO BEG-BRANCH.
       BR-02.
           MOVE "ENNN040BR2." TO VDU.
           PERFORM SCREENIO.
           IF F1-KEY GO TO REC-PERIOD.
           IF UP-KEY GO TO BR-01.
           IF F7-KEY GO TO ENDIT.
           IF NOT (ENTER-KEY OR DOWN-KEY)
              GO TO BR-02.
           MOVE VDUNUM0 TO END-BRANCH.
           IF BEG-BRANCH > END-BRANCH
              MOVE "INVALID BRANCH RANGE" TO MESS
              PERFORM SEND-MESS
              GO TO BR-01.
           GO TO ENTER-SOURCE-CODE.
       ALL-BR.
           MOVE "SNNN040BR1." TO VDU.
           MOVE 0 TO BEG-BRANCH VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN040BR2." TO VDU.
           MOVE 9999 TO END-BRANCH VDUNUM0.
           PERFORM SCREENIO.

       ENTER-SOURCE-CODE.
           MOVE "ENNN010SRC." TO VDU.
           PERFORM SCREENIO.
           IF F1-KEY GO TO REC-PERIOD.
           IF F3-KEY GO TO SEND-ALL-SRC.
           IF UP-KEY GO TO BR-02.
           IF F7-KEY GO TO ENDIT.
           IF NOT ENTER-KEY GO TO ENTER-SOURCE-CODE.
           MOVE VDUNUM0 TO SOURCE-CODE.
           GO TO ACCEPT-SCREEN-DATA.
       SEND-ALL-SRC.
            MOVE "S  A010SRC." TO VDU.
            MOVE "*" TO ALL-SRC-CODES VDUBUF.
            PERFORM SCREENIO.

       ACCEPT-SCREEN-DATA.
           MOVE "E  A010ACCEPT." TO VDU.
           PERFORM SCREENIO.
           IF F1-KEY GO TO REC-PERIOD.
           IF UP-KEY GO TO ENTER-SOURCE-CODE.
           IF F7-KEY GO TO ENDIT.
           IF NOT (ENTER-KEY OR DOWN-KEY)
              GO TO ACCEPT-SCREEN-DATA.
           MOVE VDUBUF TO ACCEPT-BUF.
           IF ACCEPT-BUF = "N"
              GO TO REC-PERIOD.
           IF ACCEPT-BUF NOT = "Y"
              GO TO ACCEPT-SCREEN-DATA.

           MOVE PERIOD      TO LOG-PERIOD.
           MOVE BEG-DATE    TO LOG-BEG-DATE.
           MOVE END-DATE    TO LOG-END-DATE.
           MOVE BEG-BRANCH  TO LOG-BEG-BR.
           MOVE END-BRANCH  TO LOG-END-BR.
           MOVE SOURCE-CODE TO LOG-SRC.
           PERFORM LOG-CHANGES.

           MOVE "S  A000MESS." TO VDU.
           MOVE "GL/TRANSACTION FILE REVERSAL IN PROGRESS"
               TO VDUBUF.
           PERFORM SCREENIO.

           PERFORM OPEN-GT1-FILE.
           PERFORM OPEN-GL1-FILE.

           GO TO EXIT-INITIALIZATION.
       ENDIT.
           MOVE "END" TO IO-TYPE.
       EXIT-INITIALIZATION.
           EXIT.

      **************************************
       UPDATE-GL1-FILE SECTION.
      **************************************

           IF ALL-SRC-CODES NOT = "*"
              MOVE SOURCE-CODE TO GT-SOURCE 
                                  QGT1-WBEG-SOURCE
                                  QGT1-WEND-SOURCE
           ELSE
              MOVE 0           TO GT-SOURCE
                                  QGT1-WBEG-SOURCE
              MOVE 9           TO QGT1-WEND-SOURCE.

           MOVE BEG-DATE       TO GT-DATE2 SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD  TO QGT1-WBEG-DATE2.
           MOVE END-DATE       TO  SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD  TO QGT1-WEND-DATE2.

           MOVE BEG-BRANCH     TO GT-BRANCH
                                  QGT1-WBEG-BRANCH.
           MOVE END-BRANCH     TO QGT1-WEND-BRANCH.

           MOVE 0              TO GT-ACCTNO GT-LINE
                                  QGT1-WBEG-ACCTNO
                                  QGT1-WBEG-LINE.
           MOVE 19000101       TO GT-DATE3 SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD  TO QGT1-WBEG-DATE3.

           MOVE EXT-JULIAN-END       TO SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD  TO QGT1-WEND-DATE3.

           MOVE ALL "9"              TO QGT1-WEND-LINE
                                        QGT1-WEND-ACCTNO.

           MOVE SPACES               TO GT-DRCRFG
                                        QGT1-WBEG-DRCRFG.
           MOVE ALL "Z"              TO QGT1-WEND-DRCRFG.

      *-   -   -{ ADD ONE TO WHERE SCAN BEG FIELDS }
           IF ( GT-SOURCE = 9 )
              MOVE GT-SOURCE         TO QGT1-WSBEG-SOURCE
           ELSE
              COMPUTE QGT1-WSBEG-SOURCE = GT-SOURCE + 1.

           MOVE GT-DATE2             TO NDTE-DATE.
           MOVE 1                    TO NDTE-HOLD.
           PERFORM INCREMENT-DAYS.
           MOVE NDTE-DATE            TO SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD  TO QGT1-WSBEG-DATE2.

           IF ( GT-BRANCH = 9999 )
              MOVE GT-BRANCH         TO QGT1-WSBEG-BRANCH
           ELSE
              COMPUTE QGT1-WSBEG-BRANCH = GT-BRANCH + 1.

           IF ( GT-ACCTNO = 99999999 )
              MOVE GT-ACCTNO         TO QGT1-WSBEG-ACCTNO
           ELSE
              COMPUTE QGT1-WSBEG-ACCTNO = GT-ACCTNO + 1.

           MOVE GT-DATE3             TO NDTE-DATE
           MOVE 1                    TO NDTE-HOLD
           PERFORM INCREMENT-DAYS
           MOVE NDTE-DATE            TO SQL-DATE-YYYYMMDD
           PERFORM SQL-SET-DATE
           MOVE SQL-DATE-YYYY-MM-DD  TO QGT1-WSBEG-DATE3.

           MOVE 01                   TO SQL-TEXT-SIZE
           MOVE GT-DRCRFG            TO SQL-TEXT.
           PERFORM SQL-TEXT-INCREMENT.
           MOVE SQL-TEXT             TO QGT1-WSBEG-DRCRFG.

           PERFORM START-GT1-FILE. *> SSP - GTFILE 
                 
       READ-GLTRANS.
      * READ ONLY "P" (UPDATED) RECORDS THAT FIT THE DATE RANGE
      * AND REMOVE FROM UPDATE
           PERFORM READ-GT1-FILE-NEXT. *> EMBEDDED - GTFILE 
           IF IO-BAD
              GO TO END-UPDATE-GL.

           IF GT-FLAG NOT = "P"
              GO TO READ-GLTRANS.

           IF GT-BRANCH < BEG-BRANCH OR GT-BRANCH > END-BRANCH
              GO TO READ-GLTRANS.

           IF PERIOD = 99
              GO TO CHECK-TRANS-DATE.

           IF GT-PERIOD NOT = PERIOD
              GO TO READ-GLTRANS.

           IF ALL-SRC-CODES NOT = "*"
              IF GT-SOURCE NOT = SOURCE-CODE
                 GO TO READ-GLTRANS.

       CHECK-TRANS-DATE.
      * VERIFY BETWEEN BEGINNING AND ENDING DATE
           IF GT-DATE1 < BEG-DATE OR GT-DATE1 > END-DATE
              GO TO READ-GLTRANS.

            MOVE GT-BRANCH TO GL-BRANCH.
            MOVE GT-ACCTNO TO GL-ACCOUNT.

       UPDATE-GLMASTER.
            PERFORM READ-GL1-FILE. *> EMBEDDED - GLFILE
            IF IO-BAD 
               MOVE GT-SOURCE   TO BAD-SOURCE
               MOVE GT-BRANCH   TO BAD-GLBR
               MOVE GT-ACCTNO   TO BAD-GLACCT
               MOVE GT-DATE1    TO DATE-YYYYMMDD
               PERFORM CONVERT-YYYYMMDD-TO-YYMMDD
               MOVE DATE-YYMMDD TO BAD-DATE
               MOVE GT-LINE     TO BAD-LINE
               MOVE GT-DRCRFG   TO BAD-DRCRFG
               MOVE BAD-GL-LIST TO WR-LIST
               MOVE BAD-GL-BUF  TO WR-BUF
               PERFORM CALL-WRFORM
               MOVE " INVALID G/L # IN TRANSACTION RECORD" TO MESS
               PERFORM SEND-MESS
               MOVE " RECORD NOT UPDATED" TO MESS
               PERFORM SEND-MESS
               MOVE BAD-GL-LIST TO WR-LIST
               MOVE SPACES TO WR-BUF
               PERFORM CALL-WRFORM
               GO TO READ-GLTRANS.

           MOVE GT-AMT TO GT-AMT-WORKER.
           IF GL-TYPE = "C"
              MULTIPLY GT-AMT-WORKER BY -1 GIVING GT-AMT-WORKER.
           SUBTRACT GT-AMT-WORKER FROM GL-CYBAL(GT-PERIOD).
       REWRITE-GLTRANS.
           MOVE " " TO GT-FLAG.
           MOVE 0 TO GT-PERIOD.
           PERFORM REWRITE-GT1-FILE. *> EMBEDDED - GTFILE
       REWRITE-GLMASTER.
           PERFORM REWRITE-GL1-FILE. *> EMBEDDED - GLFILE
           GO TO READ-GLTRANS.
       END-UPDATE-GL.
           EXIT.

      *****************************************************************
       OUT-OF-PERIOD SECTION.
      *****************************************************************

      * GET PASSWORD SINCE, POSTING DATES ARE OUTSIDE OF FISCAL PERIOD

           MOVE 0      TO IO-FG.
           MOVE 1      TO PASSWDW-SEQ.
           MOVE "TO UPDATE DATES OUT OF FISCAL PERIOD"
                       TO PASSWDW-PROMPT2.
           PERFORM VERIFY-PASSWORD.
           IF PASSWDW-STAT NOT = "OK"
              MOVE 9   TO IO-FG
              MOVE " " TO MESS
              STRING "SELECTED RANGE FOR UPDATE"
                     " IS OUTSIDE OF FISCAL PERIOD "
                     PERIOD-ALP
                     DELIMITED BY SIZE INTO MESS
              PERFORM SEND-MESS
              MOVE " " TO MESS
              MOVE " --- AND IS NOT ALLOWED ---"
                    TO MESS
              PERFORM SEND-MESS.

      *****************************************************************
       LOG-CHANGES SECTION.
      *****************************************************************
           MOVE "G"              TO ERRLOGW-ACTION.

           MOVE LOG-UPDATE-MSG   TO ERRLOGW-DETAIL.
           MOVE FORM-PATHNAME    TO ERRLOGW-PROGX.

           MOVE "CL/ERRLOG" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH ERRLOGW-AREA.
           CANCEL FORM-PROGX.

      *****************************************************************
       VERIFY-PASSWORD SECTION.
      *****************************************************************
           MOVE "EX"    TO PASSWDW-STAT.
           MOVE "FXGLUP" TO PASSWDW-PGM.
           MOVE "PLEASE ENTER THE PASSWORD " TO PASSWDW-PROMPT1.
           MOVE "CL/PASSWD" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH PASSWDW-BUF.
           CANCEL FORM-PROGX.

       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/GPENV.CPY".


      *****************************************************************
       FORM-RESET SECTION.
      *****************************************************************
           MOVE EXT-CONAME-CONAME TO FXGLUP-CONAME.
           MOVE EXT-CONAME-SYSDATE TO FXGLUP-SYSDATE.
      *    DISPLAY OMITTED ERASE SCREEN.
      * USING THE LINE 1 COL 1 WORKS MUCH MORE CONSISTENTLY!
      * THE OMITTED LINE WOULD NOT CLEAR ON A F1 FROM THE BOTTOM BUT WOULD
      * CLEAR WHEN PART WAY THRU A NEW RECORD. DIDN'T MAKE SENSE.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DISPLAY FXGLUP-SCREEN UPON FXGLUP-WINDOW-HANDLE.
           MOVE FXGLUP-FORM-FIELDS TO WR-BUF.
           MOVE FXGLUP-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      *****************************************************************
       SETUP-LINK-INFO SECTION.
      *****************************************************************
           MOVE FXGLUP-HELP--FILE TO HELP-LINK-FILE.
           MOVE FXGLUP-HELP--DIR  TO HELP-LINK-DIR.

      **********************************************************
       VDU-CONVERT-FIELD SECTION.
      **********************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBGL/FXGLUP_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      *****************************************
       MISC SECTION.
      *****************************************
       COPY "LIBGB/ACCESS.CPY".

      *****************************************
       IO-ROUTINES SECTION.
      *****************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/DECLARE_SQL.CPY".
           PERFORM CLOSE-GT1-FILE.
           PERFORM CLOSE-GT1-CONNECTION.
           PERFORM CLOSE-GL1-FILE.
           PERFORM SQL-CLOSE-UPDATE-CONNECTION.
       COPY "LIBGL/GLGTGS_SQL.CPY".
       COPY "LIBGL/GLGLGS_SQL.CPY".
      * COPY "LIBGL/GLGT1IN.CPY".
      * COPYMEMBER: LIBGL/GLGT1IN
      ******************************************************************
      * REV:
      * JKC 2023-1019 COMMENTED OUT START-GT1-FILE-GREATER; ONE PROGRAM
      *          (GL/DELDUP) ONLY USES IT; DUE TO THE COMPLEX GT1-KEY I
      *          DO NOT BELIEVE IT IS PROPERLY SETUP FOR IT TO WORK
      *          CORRECTLY; AFTER DISCUSSING WITH KEVIN MULLEN, DELDUP
      *          WAS ONLY USED ONCE BUT WOULD BE NICE TO HAVE IN CASE
      *          NEEDED FOR AN EMERGENCY; RON ZAMISKA WAS ABLE TO
      *          ACCOMPLISH WHAT DELDUP DOES WITHIN A SQL CODE; WE
      *          ARCHIVED DELDUP FROM USE SO THE USE OF
      *          START-GT1-FILE-GREATER IS NO LONGER NEEDED.
      ******************************************************************
       LOAD-GT1-FILE.
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
       OPEN-GT1-FILE.
           PERFORM LOAD-GT1-FILE.
           PERFORM OPEN-IT.
           MOVE GT-PATH-SQL TO E-FILE.

           IF GT1-CONN-OPEN-SW = "N" 
              MOVE "Y"         TO GT1-CONN-OPEN-SW
              EXEC SQL CONNECT TO :SQL-CONNECT-SERVER AS GT1_CONNECT 
                       USER :SQL-CONNECT-USERID USING :SQL-CONNECT-PSWD
              END-EXEC.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

           EXEC SQL SET CONNECTION C1 END-EXEC.

      *-----------------------------------------------------------------
       START-GT1-FILE. *> SSP - GTFILE 
           PERFORM START-IT.
           MOVE GT-PATH-SQL TO E-FILE.
           MOVE GT1-KEY TO E-KEYX.
           IF ( GT1-CURSOR-STAT-GOOD )
              PERFORM CLOSE-GT1-FILE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *  DEFAULT THE WBEG AND WEND; TO GET A START TO WORK IN SQL LIKE
      *  IT DOES IN VISION FILES FOR MULTIPLE SCAN PAGES, NEED TO ADD
      *  ONE TO THE WHERE SCAN BEG (WSBEG).
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( QGT1-WEND-LINE = ZEROES    ) OR
              ( QGT1-WEND-LINE NOT NUMERIC )
              MOVE GT-SOURCE            TO QGT1-WBEG-SOURCE
              MOVE 9                    TO QGT1-WEND-SOURCE

              MOVE GT-DATE2             TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD  TO QGT1-WBEG-DATE2
              MOVE GT-DATE3             TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD  TO QGT1-WBEG-DATE3

              MOVE EXT-JULIAN-END       TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD  TO QGT1-WEND-DATE2
                                           QGT1-WEND-DATE3

              MOVE GT-BRANCH            TO QGT1-WBEG-BRANCH
              MOVE ALL "9"              TO QGT1-WEND-BRANCH
              MOVE GT-ACCTNO            TO QGT1-WBEG-ACCTNO
              MOVE ALL "9"              TO QGT1-WEND-ACCTNO
              MOVE GT-DRCRFG            TO QGT1-WBEG-DRCRFG
              MOVE ALL "Z"              TO QGT1-WEND-DRCRFG
              MOVE GT-LINE              TO QGT1-WBEG-LINE
              MOVE ALL "9"              TO QGT1-WEND-LINE
      *-   -   -{ ADD ONE TO WHERE SCAN BEG FIELDS }
              IF ( GT-SOURCE = 9 )
                 MOVE GT-SOURCE         TO QGT1-WSBEG-SOURCE
              ELSE
                 COMPUTE QGT1-WSBEG-SOURCE = GT-SOURCE + 1
              END-IF

              MOVE GT-DATE2             TO NDTE-DATE
              MOVE 1                    TO NDTE-HOLD
              PERFORM INCREMENT-DAYS
              MOVE NDTE-DATE            TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD  TO QGT1-WSBEG-DATE2

              IF ( GT-BRANCH = 9999 )
                 MOVE GT-BRANCH         TO QGT1-WSBEG-BRANCH
              ELSE
                 COMPUTE QGT1-WSBEG-BRANCH = GT-BRANCH + 1
              END-IF

              IF ( GT-ACCTNO = 99999999 )
                 MOVE GT-ACCTNO         TO QGT1-WSBEG-ACCTNO
              ELSE
                 COMPUTE QGT1-WSBEG-ACCTNO = GT-ACCTNO + 1
              END-IF

              MOVE GT-DATE3             TO NDTE-DATE
              MOVE 1                    TO NDTE-HOLD
              PERFORM INCREMENT-DAYS
              MOVE NDTE-DATE            TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD  TO QGT1-WSBEG-DATE3

              MOVE 01                   TO SQL-TEXT-SIZE
              MOVE GT-DRCRFG            TO SQL-TEXT
              PERFORM SQL-TEXT-INCREMENT
              MOVE SQL-TEXT             TO QGT1-WSBEG-DRCRFG.

      *  STORED PROCEDURE:
      *
      *    (SELECT GTFILE.GT_FLAG,
      *            CAST(GTFILE.GT_DATE1 AS VARCHAR(10)) AS GT_DATE1, 
      *            GTFILE.GT_SOURCE,
      *            CAST(GTFILE.GT_DATE2 AS VARCHAR(10)) AS GT_DATE2,
      *            GTFILE.GT_BRANCH,
      *            GTFILE.GT_ACCTNO,
      *            CAST(GTFILE.GT_DATE3 AS VARCHAR(10)) AS GT_DATE3,
      *            GTFILE.GT_DRCRFG,
      *            GTFILE.GT_LINE,
      *            GTFILE.GT_AMT,
      *            GTFILE.GT_PERIOD
      *       FROM DBO.GTFILE GTFILE
      * 
      *      WHERE (GTFILE.GT_SOURCE BETWEEN @QGT1_WBEG_SOURCE
      *                                  AND @QGT1_WEND_SOURCE
      *        AND  GTFILE.GT_DATE2  BETWEEN @QGT1_WBEG_DATE2 
      *                                  AND @QGT1_WEND_DATE2 
      *        AND  GTFILE.GT_BRANCH BETWEEN @QGT1_WBEG_BRANCH
      *                                  AND @QGT1_WEND_BRANCH)
      *        AND  GTFILE.GT_FLAG         = 'P') TEMP1;

 
           EXEC SQL SET CONNECTION GT1_CONNECT END-EXEC.

           EXEC SQL
            DECLARE GT1_CURSOR CURSOR FOR
                :GT1-RETURN-CODE = EXEC SSP_FXGLUP_GTFILE_SELECT 
              ( :QGT1-WBEG-SOURCE,
                :QGT1-WEND-SOURCE,
                :QGT1-WBEG-DATE2, 
                :QGT1-WEND-DATE2, 
                :QGT1-WBEG-BRANCH,
                :QGT1-WEND-BRANCH )
           END-EXEC.

           EXEC SQL
               OPEN GT1_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE SPACES TO QGT1-WBEG-DRCRFG
                          QGT1-WEND-DRCRFG
                          QGT1-WBEG-DATE2
                          QGT1-WEND-DATE2
                          QGT1-WBEG-DATE3
                          QGT1-WEND-DATE3
                          QGT1-WSBEG-DATE2
                          QGT1-WSBEG-DATE3
                          QGT1-WSBEG-DRCRFG.

           MOVE ZEROES TO QGT1-WBEG-BRANCH
                          QGT1-WEND-BRANCH
                          QGT1-WBEG-ACCTNO
                          QGT1-WEND-ACCTNO
                          QGT1-WBEG-LINE
                          QGT1-WEND-LINE
                          QGT1-WBEG-SOURCE
                          QGT1-WEND-SOURCE
                          QGT1-WSBEG-SOURCE
                          QGT1-WSBEG-BRANCH
                          QGT1-WSBEG-ACCTNO.

           MOVE STAT---GOOD TO GT1-CURSOR-STAT.

           EXEC SQL SET CONNECTION C1 END-EXEC. 

      *-----------------------------------------------------------------
       READ-GT1-FILE.
           PERFORM READ-IT.
           MOVE GT-PATH-SQL TO E-FILE.
           MOVE GT1-KEY TO E-KEYX.
           INITIALIZE QGT-REC.

           MOVE GT-SOURCE           TO QGT-SOURCE.
           MOVE GT-DATE2            TO SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO QGT-DATE2.
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
            WHERE GTFILE.GT_SOURCE = :QGT-SOURCE
              AND GTFILE.GT_DATE2 = :QGT-DATE2
              AND GTFILE.GT_BRANCH = :QGT-BRANCH
              AND GTFILE.GT_ACCTNO = :QGT-ACCTNO
              AND GTFILE.GT_DATE3 = :QGT-DATE3
              AND GTFILE.GT_DRCRFG = :QGT-DRCRFG
              AND GTFILE.GT_LINE = :QGT-LINE
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-GT1-FILE.

           IF ( IO-FG = 0 )
              PERFORM GET-GT-FIELDS.

      *-----------------------------------------------------------------
       READ-GT1-FILE-NEXT. *> EMBEDDED - GTFILE
           PERFORM READ-IT.
           MOVE GT-PATH-SQL TO E-FILE.
           MOVE GT1-KEY TO E-KEYX.
           INITIALIZE QGT-REC.

           EXEC SQL SET CONNECTION GT1_CONNECT END-EXEC.

           IF ( GT1-CURSOR-STAT-GOOD )
              EXEC SQL
                FETCH GT1_CURSOR 
                  INTO 
                    :QGT-FLAG, 
                    :QGT-DATE1, 
                    :QGT-SOURCE,
                    :QGT-DATE2,
                    :QGT-BRANCH,
                    :QGT-ACCTNO,
                    :QGT-DATE3,
                    :QGT-DRCRFG,
                    :QGT-LINE,
                    :QGT-AMT,
                    :QGT-PERIOD 
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-GT1-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-GT-FIELDS
              PERFORM GET-GL-FIELDS.

          EXEC SQL SET CONNECTION C1 END-EXEC.

      *-----------------------------------------------------------------
       REWRITE-GT1-FILE. *> EMBEDDED - GTFILE

           ACCEPT GT-LTOUCH-DATE FROM CENTURY-DATE.
           PERFORM REWRITE-IT.
           MOVE GT-PATH-SQL TO E-FILE.
           MOVE GT1-KEY TO E-KEYX.
           PERFORM SET-GT-FIELDS.

           MOVE '2' TO SQL-UPDATE-CONN-USE-SW. *> USE UPDATE_CONNECT
           PERFORM SQL-SWAP-UPDATE-CONNECTION.

           EXEC SQL
            UPDATE DBO.GTFILE
             SET
              GTFILE.GT_FLAG         = :QGT-FLAG,
              GTFILE.GT_DATE1        = :QGT-DATE1,
              GTFILE.GT_SOURCE       = :QGT-SOURCE,
              GTFILE.GT_DATE2        = :QGT-DATE2,
              GTFILE.GT_BRANCH       = :QGT-BRANCH,
              GTFILE.GT_ACCTNO       = :QGT-ACCTNO,
              GTFILE.GT_DATE3        = :QGT-DATE3,
              GTFILE.GT_DRCRFG       = :QGT-DRCRFG,
              GTFILE.GT_LINE         = :QGT-LINE,
              GTFILE.GT_AMT          = :QGT-AMT,
              GTFILE.GT_PERIOD       = :QGT-PERIOD,
              GTFILE.GT_LTOUCH_DATE  = :QGT-LTOUCH-DATE
             WHERE GTFILE.GT_SOURCE  = :QGT-SOURCE
               AND GTFILE.GT_DATE2   = :QGT-DATE2
               AND GTFILE.GT_BRANCH  = :QGT-BRANCH
               AND GTFILE.GT_ACCTNO  = :QGT-ACCTNO
               AND GTFILE.GT_DATE3   = :QGT-DATE3
               AND GTFILE.GT_DRCRFG  = :QGT-DRCRFG
               AND GTFILE.GT_LINE    = :QGT-LINE
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO REWRITE-GT1-FILE.

      *    IF ( IO-FG = 0 )
      *       EXEC SQL COMMIT END-EXEC
      *       PERFORM SQL-IO-VALIDATION.

           MOVE '1' TO SQL-UPDATE-CONN-USE-SW. *> USE C1 CONNECT    
           PERFORM SQL-SWAP-UPDATE-CONNECTION.

      *-----------------------------------------------------------------
       CLOSE-GT1-FILE.

           PERFORM CLOSE-IT.

           IF ( GT1-CURSOR-STAT-GOOD )
              EXEC SQL SET CONNECTION GT1_CONNECT END-EXEC
              EXEC SQL
                   CLOSE GT1_CURSOR
              END-EXEC
              EXEC SQL SET CONNECTION C1 END-EXEC
              MOVE STAT---BAD TO GT1-CURSOR-STAT.
 
      *-----------------------------------------------------------------
       CLOSE-GT1-CONNECTION.

           IF GT1-CONN-OPEN-SW = "Y"
              EXEC SQL SET CONNECTION GT1_CONNECT END-EXEC
              EXEC SQL COMMIT END-EXEC
              EXEC SQL DISCONNECT GT1_CONNECT END-EXEC
              EXEC SQL SET CONNECTION C1 END-EXEC
              MOVE "N" TO GT1-CONN-OPEN-SW.

      *-----------------------------------------------------------------

      * COPY "LIBGL/GLGL1IN.CPY".
      * COPYMEMBER: LIBGL/GLGL1IN
       LOAD-GL1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( GL-PATH-OVERRIDE = "Y" )
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( GL-PATH-OVERRIDE = SPACES )
              MOVE EXT-FILPATH-BASE        TO GL-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO GL-ALL-MACHINE
              MOVE GL-ALL-PATH             TO GL-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( GL-PATH-OVERRIDE = "B" )
              MOVE FILPATH-BASE            TO GL-ALL-BASE
              MOVE FILPATH-MACHINE         TO GL-ALL-MACHINE
              MOVE GL-ALL-PATH             TO GL-PATH.

      *-----------------------------------------------------------------
       OPEN-GL1-FILE.
           PERFORM LOAD-GL1-FILE.
           PERFORM OPEN-IT.
           MOVE GL-PATH-SQL TO E-FILE.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       READ-GL1-FILE. *> EMBEDDED - GLFILE
           PERFORM READ-IT.
           MOVE GL-PATH-SQL TO E-FILE.
           MOVE GL1-KEY TO E-KEYX.
           INITIALIZE QGL-REC.

           MOVE GL-BR-ACCOUNT TO QGL-BR-ACCOUNT.

           EXEC SQL
            SELECT GLFILE.GL_BR_ACCOUNT,       
                   GLFILE.GL_CYBAL_1,       
                   GLFILE.GL_CYBAL_2,       
                   GLFILE.GL_CYBAL_3,       
                   GLFILE.GL_CYBAL_4,       
                   GLFILE.GL_CYBAL_5,       
                   GLFILE.GL_CYBAL_6,       
                   GLFILE.GL_CYBAL_7,       
                   GLFILE.GL_CYBAL_8,       
                   GLFILE.GL_CYBAL_9,       
                   GLFILE.GL_CYBAL_10,       
                   GLFILE.GL_CYBAL_11,       
                   GLFILE.GL_CYBAL_12,       
                   GLFILE.GL_TYPE,           
                   GLFILE.GL_LTOUCH_DATE    
              INTO  
                  :QGL-BR-ACCOUNT,
                  :QGL-CYBAL-1,
                  :QGL-CYBAL-2,
                  :QGL-CYBAL-3,
                  :QGL-CYBAL-4,
                  :QGL-CYBAL-5,
                  :QGL-CYBAL-6,
                  :QGL-CYBAL-7,
                  :QGL-CYBAL-8,
                  :QGL-CYBAL-9,
                  :QGL-CYBAL-10,
                  :QGL-CYBAL-11,
                  :QGL-CYBAL-12,
                  :QGL-TYPE,
                  :QGL-LTOUCH-DATE
              FROM DBO.GLFILE
             WHERE GLFILE.GL_BR_ACCOUNT  = :QGL-BR-ACCOUNT
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-GL1-FILE.

           IF ( IO-FG = 0 )
              PERFORM GET-GL-FIELDS.

      *-----------------------------------------------------------------
       REWRITE-GL1-FILE. *> EMBEDDED - GLFILE

           ACCEPT GL-LTOUCH-DATE FROM CENTURY-DATE.
           PERFORM REWRITE-IT.
           MOVE GL-PATH-SQL TO E-FILE.
           MOVE GL1-KEY TO E-KEYX.
           PERFORM SET-GL-FIELDS.

           MOVE '2' TO SQL-UPDATE-CONN-USE-SW. *> USE UPDATE_CONNECT
           PERFORM SQL-SWAP-UPDATE-CONNECTION.

           EXEC SQL
            UPDATE DBO.GLFILE
             SET
              GLFILE.GL_BR_ACCOUNT      = :QGL-BR-ACCOUNT,
              GLFILE.GL_CYBAL_1         = :QGL-CYBAL-1,
              GLFILE.GL_CYBAL_2         = :QGL-CYBAL-2,
              GLFILE.GL_CYBAL_3         = :QGL-CYBAL-3,
              GLFILE.GL_CYBAL_4         = :QGL-CYBAL-4,
              GLFILE.GL_CYBAL_5         = :QGL-CYBAL-5,
              GLFILE.GL_CYBAL_6         = :QGL-CYBAL-6,
              GLFILE.GL_CYBAL_7         = :QGL-CYBAL-7,
              GLFILE.GL_CYBAL_8         = :QGL-CYBAL-8,
              GLFILE.GL_CYBAL_9         = :QGL-CYBAL-9,
              GLFILE.GL_CYBAL_10        = :QGL-CYBAL-10,
              GLFILE.GL_CYBAL_11        = :QGL-CYBAL-11,
              GLFILE.GL_CYBAL_12        = :QGL-CYBAL-12,
              GLFILE.GL_TYPE            = :QGL-TYPE,
              GLFILE.GL_LTOUCH_DATE     = :QGL-LTOUCH-DATE
             WHERE GLFILE.GL_BR_ACCOUNT = :QGL-BR-ACCOUNT

           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO REWRITE-GL1-FILE.

      *    IF ( IO-FG = 0 )
      *       EXEC SQL COMMIT END-EXEC
      *       PERFORM SQL-IO-VALIDATION.

           MOVE '1' TO SQL-UPDATE-CONN-USE-SW. *> USE C1 CONNECT    
           PERFORM SQL-SWAP-UPDATE-CONNECTION.

      *-----------------------------------------------------------------
       CLOSE-GL1-FILE.

           PERFORM CLOSE-IT.

      *-----------------------------------------------------------------

       COPY "LIBGB/FERRORS.CPY".

      *****************************************
       END-ROUTINE SECTION.
      *****************************************
       END-RUN.
           PERFORM CLOSE-FILES.
       EXIT-PROG.
           MOVE "GL/GLMNMU" TO FORM-NAM.
           MOVE FORM-PATH TO FORM-PATHNAME.
       STOP-RUN.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY FXGLUP-SCREEN.
           DESTROY FXGLUP-WINDOW-HANDLE.
           EXIT PROGRAM.
