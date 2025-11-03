       IDENTIFICATION DIVISION.
       PROGRAM-ID.      GLUPD.
      **************************************************************************
      *                        (GL)
      *               G/L MASTER FILE UPDATE
      * REV:
      * BLV 082197 REVISED LIKE A90: READ & REWRITE GT2
      *            BASED ON DATE SO NOT READING ENTIRE FILE
      * BLV 000104 CHECK FOR GT-DATE1 < BEG-DATE, Y2K MEANS OLDER DATES
      *            WILL FOLLOW NEWER DATES IN READ-NEXT, SO GET OUT
      * MG  000204 FIXED VDUSTATUS/IO-FG/STAT PER DEVELOPMENT MEETING.
      * JTG 040105 CHANGED TO ENTER FISCAL PERIOD FIRST.
      *            CHANGED GL UPDATE TO TEST NEW GLOBAL PARAMETER FLAG
      *            GP-CLOSED-FISCAL-FG, TO DETERMINE IF GL FISCAL PERIOD
      *            HAS BEEN CLOSED. IF CLOSED STOP UPDATE.           WORLD #J040
      * BLF 070116 MAKE LOG ENTRY IN /USR/DATA/R1/LG/GL.YYMMDD TO TRACK
      *            WHEN THIS UPDATE WAS RUN, BY WHOM, & RANGES SELECTED,
      *            SUPPORT PR# 2780
      * KEC 2016.1129 {PR#00000} PARADATA <A30> [JAY CISZEWSKI]
      *          CHANGED GT-DATE1 GT-DATE2 GT-DATE3 AS ACTIVE CCYYMMDD.
      * BLM 170405 ACTIVATE 8 DIGIT DATES, PR0006
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
       77  SCCS-IDENTIFIER PIC X(40) VALUE
           "@(#)GL/GLUPD.CBL S34 06/10/21-14:55:54 >".
       COPY "LIBGL/GL01GT.CPY".
       COPY "LIBGL/GL01GT_SQL.CPY".
       COPY "LIBGL/GLWSGT.CPY".
       COPY "LIBGL/GL01GL.CPY".
       COPY "LIBGL/GL01GL_SQL.CPY".
       COPY "LIBGL/GLWSGL.CPY".
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".

       01  ERRCD              PIC X         VALUE SPACES.

       01  ACCEPT-BUF         PIC X         VALUE SPACES.

       01  MESS-LIST          PIC X(5)      VALUE
           "MESS.".

       01  BEG-BRANCH         PIC 9(4) VALUE 0.
       01  END-BRANCH         PIC 9(4) VALUE 0.
       01  BEG-DATE           PIC 9(8) VALUE 0.
       01  FILLER REDEFINES BEG-DATE.
           03  FILLER         PIC 9(6).
           03  BEG-DATE-DD    PIC 9(2).
       01  END-DATE           PIC 9(8) VALUE 0.
       01  PERIOD-ALP.
           03  PERIOD         PIC 99        VALUE 0.
       01  IO-TYPE            PIC X(3)      VALUE SPACES.

       01  GT-AMT-WORKER      PIC S9(9)V99 COMP.

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
           03  FILLER         PIC X(9) VALUE " PERIOD: ".
           03  LOG-PERIOD     PIC 99.
           03  FILLER         PIC XX   VALUE " ".
           03  LOG-BEG-DATE   PIC 99/99/99.
           03  FILLER         PIC X    VALUE "-".
           03  LOG-END-DATE   PIC 99/99/99.
           03  FILLER         PIC X(12) VALUE "  BRANCHES: ".
           03  LOG-BEG-BR     PIC Z999.
           03  FILLER         PIC X    VALUE "-".
           03  LOG-END-BR     PIC Z999.

       COPY "LIBWI/WIMESSW.CPY".
       COPY "LIBGB/PASSWDW.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/GPENVW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       01  GLUPD-WINDOW-HANDLE  PIC X(10).
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBGL/GLUPD_DEF.CPY".
       COPY "LIBGL/GLUPD_WKS.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/HELPLINK.CPY".

       COPY "LIBGB/GETFMW.CPY".

       SCREEN SECTION.
       COPY "LIBGL/GLUPD_SCN.CPY".

       PROCEDURE DIVISION
             USING FORM-PATHNAME EXIT-PATHNAME.

      ***********************************************
       MAIN-PROGRAM SECTION.
      ***********************************************
           PERFORM SQL-CONNECT.
           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "GLUPD" TO VDU-FORM-NAME.
           DISPLAY FLOATING GRAPHICAL WINDOW
                   SCREEN LINE 01 * VDU-WIN-L-OFFSET
                   SCREEN COL 01 * VDU-WIN-C-OFFSET
           COLOR IS VDU-WCOLOR-TCLP,CONTROL VALUE IS VDU-WCTRL-TCLP
                   CELL SIZE IS LABEL FONT
                   SIZE 80 + VDU-WIN-W-MOD
                   LINES 24 + VDU-WIN-H-MOD
                   BLANK SCREEN
                   HANDLE IS GLUPD-WINDOW-HANDLE.

       COPY "LIBGB/CHKSEC.CPY".

           PERFORM FORM-RESET.

           PERFORM OPEN-BR-FILE.
           MOVE EXT-CONAME-USER-LOC TO BR-NO.
           PERFORM READ-BR-FILE.
           PERFORM CLOSE-BR-FILE.
           IF IO-GOOD
              IF BR-POST-GL = "N"
                 MOVE "GENERAL LEDGER POSTING NOT ALLOWED" TO MESS
                 PERFORM SEND-MESS
                 GO TO END-ROUTINE.

           PERFORM GET-GPENV.

           PERFORM INITIALIZATION.
           IF IO-TYPE = "END"
              GO TO END-ROUTINE.

           PERFORM UPDATE-GL1-FILE.

           GO TO END-ROUTINE.

      *********************************************************
       INITIALIZATION SECTION.
      *********************************************************
           MOVE SPACES TO IO-TYPE.

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

           MOVE GP-END-FISCAL-DATE(PERIOD) TO BEG-DATE.
           MOVE 01                         TO BEG-DATE-DD.
           MOVE GP-END-FISCAL-DATE(PERIOD) TO END-DATE.

           MOVE "S  A080BEGDATE." TO VDU.
           MOVE BEG-DATE          TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY       TO VDU-DATE-EDIT.
           PERFORM SCREENIO.
           MOVE "S  A080ENDDATE." TO VDU.
           MOVE END-DATE          TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY       TO VDU-DATE-EDIT.
           PERFORM SCREENIO.

           IF GP-CLOSED-FISCAL-FG(PERIOD) = "Y"
              MOVE "WARNING!" TO WIMESS1
              STRING "   GL FOR PERIOD "
                     PERIOD-ALP
                     " IS CLOSED!"
                     DELIMITED BY SIZE INTO WIMESS2
              MOVE "   NO FUTHER UPDATES ARE ALLOWED."
                 TO WIMESS3
              MOVE 3 TO WIMESS-LINES
              PERFORM SEND-WIMESS
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
           MOVE 0 TO BEG-DATE.
           MOVE "00/00/00" TO VDUBUF.
           PERFORM SCREENIO.
           MOVE "S  A080ENDDATE." TO VDU.
           MOVE 99999999 TO END-DATE.
           MOVE "99/99/99" TO VDUBUF.
           PERFORM SCREENIO.

       VERIFY-DATE-RANGE.
           MOVE GP-END-FISCAL-DATE(PERIOD) TO DATE-YYYYMMDD.
           MOVE 1 TO DATE-YYYYMMDD-DD.
           IF BEG-DATE < DATE-YYYYMMDD
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
           GO TO ACCEPT-SCREEN-DATA.
       ALL-BR.
           MOVE "SNNN040BR1." TO VDU.
           MOVE 0 TO BEG-BRANCH VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN040BR2." TO VDU.
           MOVE 9999 TO END-BRANCH VDUNUM0.
           PERFORM SCREENIO.

       ACCEPT-SCREEN-DATA.
           MOVE "E  A010ACCEPT." TO VDU.
           PERFORM SCREENIO.
           IF F1-KEY GO TO REC-PERIOD.
           IF UP-KEY GO TO BR-01.
           IF F7-KEY GO TO ENDIT.
           IF NOT (ENTER-KEY OR DOWN-KEY)
              GO TO ACCEPT-SCREEN-DATA.
           MOVE VDUBUF TO ACCEPT-BUF.
           IF ACCEPT-BUF = "N"
              GO TO REC-PERIOD.
           IF ACCEPT-BUF NOT = "Y"
              GO TO ACCEPT-SCREEN-DATA.

           MOVE PERIOD      TO LOG-PERIOD.
           MOVE BEG-DATE    TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO LOG-BEG-DATE.
           MOVE END-DATE    TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO LOG-END-DATE.
           MOVE BEG-BRANCH  TO LOG-BEG-BR.
           MOVE END-BRANCH  TO LOG-END-BR.
           PERFORM LOG-CHANGES.

           MOVE " GL/TRANSACTION FILE UPDATE IN PROGRESS" TO MESS.
           MOVE MESS-LIST TO WR-LIST.
           MOVE MESS TO WR-BUF.
           PERFORM CALL-WRFORM.

           PERFORM OPEN-GT2-FILE.
           PERFORM OPEN-GL1-FILE.

           IF BEG-DATE = 0
              MOVE 19000101 TO BEG-DATE.
           IF END-DATE = 99999999
              MOVE 99991231 TO END-DATE.
           MOVE BEG-DATE TO GT-DATE1 SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO QGT2-WBEG-DATE1.

           MOVE END-DATE TO SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE
           MOVE SQL-DATE-YYYY-MM-DD TO QGT2-WEND-DATE1.

           PERFORM START-GT2-FILE.

           GO TO EXIT-INITIALIZATION.
       ENDIT.
           MOVE "END" TO IO-TYPE.
       EXIT-INITIALIZATION.
           EXIT.

      *************************************************
       UPDATE-GL1-FILE SECTION.
      *************************************************
       READ-GLTRANS.
           PERFORM READ-GT2-FILE-NEXT.
           IF IO-BAD
              GO TO END-UPDATE-GL.

      * IF GT1-DATE > END-DATE, WE ARE DONE
           IF GT-DATE1 > END-DATE
              GO TO END-UPDATE-GL.

           IF GT-FLAG = "P"
              GO TO READ-GLTRANS.

           IF GT-BRANCH < BEG-BRANCH OR GT-BRANCH > END-BRANCH
              GO TO READ-GLTRANS.

           MOVE GT-BRANCH TO GL-BRANCH.
           MOVE GT-ACCTNO TO GL-ACCOUNT.

       UPDATE-GLMASTER.
           PERFORM READ-GL1-FILE.
           IF IO-BAD
               MOVE GT-SOURCE TO BAD-SOURCE
               MOVE GT-BRANCH TO BAD-GLBR
               MOVE GT-ACCTNO TO BAD-GLACCT
               MOVE GT-DATE1  TO DATE-YYYYMMDD
               PERFORM CONVERT-YYYYMMDD-TO-YYMMDD
               MOVE DATE-YYMMDD TO BAD-DATE
               MOVE GT-LINE     TO BAD-LINE
               MOVE GT-DRCRFG   TO BAD-DRCRFG
               MOVE BAD-GL-LIST TO WR-LIST
               MOVE BAD-GL-BUF TO WR-BUF
               PERFORM CALL-WRFORM
               MOVE "INVALID G/L # IN TRANSACTION RECORD" TO MESS
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
           ADD GT-AMT-WORKER TO GL-CYBAL(PERIOD).
       REWRITE-GLTRANS.
           MOVE "P" TO GT-FLAG.
           MOVE PERIOD TO GT-PERIOD.
           PERFORM REWRITE-GT2-FILE.
       REWRITE-GLMASTER.
           PERFORM REWRITE-GL1-FILE.
           GO TO READ-GLTRANS.
       END-UPDATE-GL.
           EXIT.

      *****************************************************************
       OUT-OF-PERIOD SECTION.
      *****************************************************************

      * GET PASSWORD SINCE POSTING DATES ARE OUTSIDE OF FISCAL PERIOD

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

      ************************************************
       LOG-CHANGES SECTION.
      ***********************************************
      * CODE TO WRITE CHANGE FILE.
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
           MOVE "GLUPD" TO PASSWDW-PGM.
           MOVE "PLEASE ENTER THE PASSWORD " TO PASSWDW-PROMPT1.
           MOVE "CL/PASSWD" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH PASSWDW-BUF.
           CANCEL FORM-PROGX.

      **********************************************************
       FORM-RESET SECTION.
      **********************************************************
           MOVE EXT-CONAME-CONAME TO GLUPD-CONAME.
           MOVE EXT-CONAME-SYSDATE TO GLUPD-SYSDATE.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DISPLAY GLUPD-SCREEN UPON GLUPD-WINDOW-HANDLE.
           MOVE GLUPD-FORM-FIELDS TO WR-BUF.
           MOVE GLUPD-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      **********************************************************
       SETUP-LINK-INFO SECTION.
      **********************************************************
           MOVE GLUPD-HELP--FILE TO HELP-LINK-FILE.
           MOVE GLUPD-HELP--DIR  TO HELP-LINK-DIR.

      **********************************************************
       VDU-CONVERT-FIELD SECTION.
      **********************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBGL/GLUPD_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

       COPY "LIBWI/WIMESS.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/GPENV.CPY".

      *******************************************
       MISC-ROUTINES SECTION.
      *******************************************
       COPY "LIBGB/ACCESS.CPY".

      *******************************************
       IO-ROUTINES SECTION.
      *******************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/DECLARE_SQL.CPY".
           PERFORM CLOSE-GT2-FILE.
           PERFORM CLOSE-GL1-FILE.
           PERFORM CLOSE-BR-FILE.
       COPY "LIBGL/GLGTGS_SQL.CPY".
       COPY "LIBGL/GLGLGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGL/GLGT2IN.CPY".
       COPY "LIBGL/GLGL1IN.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBGB/FERRORS.CPY".

      *******************************************
       END-ROUTINE SECTION.
      *******************************************
       END-RUN.
           PERFORM CLOSE-FILES.
       EXIT-PROG.
           MOVE "GL/GLMENU" TO FORM-NAM.
           MOVE FORM-PATH TO FORM-PATHNAME.
       STOP-RUN.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY GLUPD-SCREEN.
           DESTROY GLUPD-WINDOW-HANDLE.
           EXIT PROGRAM.
