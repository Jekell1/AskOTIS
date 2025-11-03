       IDENTIFICATION DIVISION.
       PROGRAM-ID. ENDYER.
      **************************************************************
      *                   (GL)
      *
      *           END OF YEAR PROCEDURE
      *
      * REV:
      * BLV 990421 IF BRANCH HAS INVALID RETAINED EARNINGS ACCT#,
      *            DISPLAY BRANCH # WITH ERROR MESSAGE
      * GLB 990611 ADD TAG FILE FOR 1 OR ALL BRANCHES TO DOCUMENT
      *            THAT YEAR END HAS BEEN RUN, PR# 776
      *  MG 000201 FIXED VDUSTATUS/IO-FG/STAT PER DEVELOPMENT MEETING.
      * BLM 140508 GOT RID OF REMOVE-TAG ROUTINE & COPY OF RM_CMD
      *            COPY MEMBER, NOT USED
      * BLM 170328 ACTIVATE 8 DIGIT DATES, PD0006
      *  CS 190723 REMOVED USE OF EOY GL TAG AND CHANGE TO READ
      *            GLTAGS FILE FOR CHECKING PREVIOUSLY RUN AND CREATE
      *            OF TAG
      **************************************************************
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
           "@(#)GL/ENDYER S34 12/03/21-10:12:05 >".

       COPY "LIBGL/GL01GL.CPY".
       COPY "LIBGL/GL01GL_SQL.CPY".
       COPY "LIBGL/GLWSGL.CPY".
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBGL/GL01GLLOGS.CPY".
       COPY "LIBGL/GL01GLLOGS_SQL.CPY".
       COPY "LIBGL/GLWSGLLOGS.CPY".

       01  BAD-BR-KEY      PIC 9(4)     VALUE 0.

       77  PERIOD          PIC 99        VALUE 1.
       77  GL-OPENBAL-WK   PIC S9(10)V99 COMP-3 VALUE +0.

       01  YEAR-END-MMDDYY PIC 9(6).
       01  SEQ             PIC 9(4) VALUE 0.
       COPY "LIBGB/EOBUF_EXT.CPY".
           03  BEG-BRANCH-BUF  PIC 9(4).
           03  END-BRANCH-BUF  PIC 9(4).
           03  YEAR1-MMDDYY    PIC 9(6).
       01  VERSION-CONTROL     PIC X VALUE "A".

       01  BAD-GL-BUF.
           03  FILLER          PIC X(20)     VALUE
                               "INVALID G/L NUMBER: ".
           03  GL-DATA.
               05  BAD-GL-BRANCH  PIC 9(4).
               05  FILLER         PIC X.
               05  BAD-GL-ACCOUNT PIC 9(7).

       01  MESS-LIST       PIC X(5)      VALUE "MESS.".
       01  MESS-BUF        PIC X(50)     VALUE
            "*** GENERAL LEDGER CLEAR IN PROGRESS ***".
       01  MESS-BUF2       PIC X(50)      VALUE
            "* RETAINED EARNINGS UPDATE IN PROGRESS *".

       01  BR-RETAIN-WK    PIC S9(9)V99 COMP-3 VALUE 0.


       COPY "LIBWI/WIMESSW.CPY".
       COPY "LIBGB/GETPRW.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/SYSTEMW.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/GPENVW.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBGL/ENDYER_DEF.CPY".
       COPY "LIBGL/ENDYER_WKS.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBGL/ENDYER_SCN.CPY".

       PROCEDURE DIVISION
            USING FORM-PATHNAME EXIT-PATHNAME.

      ************************************************************
       MAIN-MODULE SECTION.
      ************************************************************
           PERFORM SQL-CONNECT.
      D    STOP MESS.
           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "ENDYER" TO VDU-FORM-NAME.

           PERFORM OPEN-GLLOGS1-FILE.

           PERFORM INITIALIZATION.
           IF EXT-EOBUF-ERRCD NOT = " "
              GO TO END-PROGRAM.

           PERFORM CREATE-TAG-FILE.
           IF STAT-BAD
              GO TO END-PROGRAM.

       READ-GLFILE-NEXT.
           PERFORM READ-GL1-FILE-NEXT.
           IF IO-BAD
              GO TO UPDATE-EQUITY.

           IF GL-BRANCH > END-BRANCH-BUF
              GO TO UPDATE-EQUITY.

           IF GL-BRANCH < BEG-BRANCH-BUF
              GO TO READ-GLFILE-NEXT.

           MOVE 0 TO GL-OPENBAL-WK.
           MOVE GL-OPENBAL TO GL-LYOPENBAL.

           PERFORM SET-PERIODS VARYING
              PERIOD FROM 1 BY 1 UNTIL PERIOD > 12.

           ADD GL-OPENBAL-WK TO GL-OPENBAL.
           IF GL-CLEAR = "Y"
              PERFORM BRANCH-UPDATE
              IF GL-BRANCH = BAD-BR-KEY
                 GO TO READ-GLFILE-NEXT.

           PERFORM REWRITE-GL1-FILE.

           GO TO READ-GLFILE-NEXT.


      **************************************************
       UPDATE-EQUITY.
           IF EXT-ROUTE-BUF = "00"
              MOVE MESS-LIST TO WR-LIST
              MOVE MESS-BUF2 TO WR-BUF
              PERFORM CALL-WRFORM.

      D    STOP MESS.
           PERFORM OPEN-GL1-FILE.
           PERFORM OPEN-BR-FILE.

           MOVE BEG-BRANCH-BUF TO BR-NO.
           PERFORM START-BR-FILE.
       READ-BRANCH-NEXT.
           PERFORM READ-BR-FILE-NEXT.
           IF IO-BAD
              GO TO END-PROGRAM.

           IF BR-NO > END-BRANCH-BUF
              GO TO END-PROGRAM.

           IF BR-NO < BEG-BRANCH-BUF
              GO TO READ-BRANCH-NEXT.

           MOVE BR-RETAIN TO GL-BR-ACCOUNT.
           PERFORM READ-GL1-FILE.
           IF IO-BAD
              MOVE "INVALID RETAINED EARNINGS GL ACCOUNT!" TO WIMESS1
              MOVE "BRANCH #: &&&&" TO WIMESS2
              INSPECT WIMESS2 REPLACING FIRST "&&&&" BY BR-NO
              MOVE GL-BRANCH TO BAD-GL-BRANCH
              MOVE GL-ACCOUNT TO BAD-GL-ACCOUNT
              MOVE BAD-GL-BUF TO WIMESS3
              MOVE 3 TO WIMESS-LINES
              PERFORM SEND-WIMESS
      *       UNLOCK GL-FILE
              GO TO READ-BRANCH-NEXT.

           MOVE BR-RETAIN-AMT TO BR-RETAIN-WK.
           IF GL-TYPE = "C"
              MULTIPLY -1 BY BR-RETAIN-WK.
           ADD BR-RETAIN-WK TO GL-OPENBAL.

           PERFORM REWRITE-GL1-FILE.
           MOVE 0 TO BR-RETAIN-AMT.
           PERFORM REWRITE-BR-FILE.

           GO TO READ-BRANCH-NEXT.

      **************************************************
       SET-PERIODS SECTION.
      **************************************************
           ADD  GL-CYBAL(PERIOD) TO GL-OPENBAL-WK.
           MOVE GL-CYBAL(PERIOD) TO GL-LYBAL(PERIOD).
           MOVE GL-CYBUD(PERIOD) TO GL-LYBUD(PERIOD).
           MOVE GL-NYBUD(PERIOD) TO GL-CYBUD(PERIOD).
           MOVE 0 TO GL-CYBAL(PERIOD) GL-NYBUD(PERIOD).

      **************************************************
       BRANCH-UPDATE SECTION.
      **************************************************
           IF GL-BRANCH = BR-NO
              GO TO DETERMINE-CR-DM.

           MOVE GL-BRANCH TO BR-NO.
           IF BR-NO = BAD-BR-KEY
              MOVE 0 TO BR-NO
              GO TO BRANCH-EXIT.

           PERFORM READ-BR-FILE.
           IF IO-BAD
              PERFORM SEND-BAD-GLFILE-MESS
              GO TO BRANCH-EXIT.

           MOVE 0 TO BR-RETAIN-AMT.
       DETERMINE-CR-DM.
           IF GL-TYPE = "C"
              MULTIPLY -1 BY GL-OPENBAL.

           ADD GL-OPENBAL TO BR-RETAIN-AMT.

           PERFORM REWRITE-BR-FILE.
           MOVE 0 TO GL-OPENBAL.
       BRANCH-EXIT.
           EXIT.

      **************************************************
       SEND-BAD-GLFILE-MESS SECTION.
      **************************************************
           MOVE "INVALID BRANCH# &&&&" TO MESS.
           INSPECT MESS REPLACING FIRST "&&&&" BY BR-NO.
           MOVE BR-NO TO BAD-BR-KEY.
           MOVE 0 TO BR-NO.

      ************************************************
       INITIALIZATION SECTION.
      ************************************************

           MOVE 0 TO Q-POS C-POS P-POS.
           MOVE "V" TO Q-BUF.
           MOVE "1" TO C-BUF.
           MOVE "00" TO P-BUF.

       COPY "LIBGB/EOINIT.CPY".

           IF EXT-ROUTE-BUF = "00"
              MOVE MESS-LIST TO WR-LIST
              MOVE MESS-BUF TO WR-BUF
              PERFORM CALL-WRFORM.

           PERFORM OPEN-GL1-FILE.
           PERFORM OPEN-BR-FILE.
           MOVE 0 TO BR-NO.
           PERFORM START-BR-FILE.
           
       VERIFY-BRANCH-ACCOUNTS.
           PERFORM READ-BR-FILE-NEXT.
           IF IO-GOOD
              MOVE BR-RETAIN TO GL-BR-ACCOUNT
              PERFORM READ-GL1-FILE
              IF IO-BAD
                 MOVE "INVALID RETAINED EARNINGS ACCT BRANCH &&&&"
                    TO MESS
                 INSPECT MESS REPLACING FIRST "&&&&" BY BR-NO
                 PERFORM SEND-MESS
                 GO TO ENDIT
              ELSE
                 GO TO VERIFY-BRANCH-ACCOUNTS.

           MOVE BEG-BRANCH-BUF TO GL-BRANCH
                                  QGL1-WBEG-BRANCH.
           MOVE END-BRANCH-BUF TO QGL1-WEND-BRANCH.
           MOVE 0              TO GL-ACCOUNT BR-NO
                                  QGL1-WBEG-ACCOUNT.
           MOVE ALL "9"        TO QGL1-WEND-ACCOUNT.

           PERFORM START-GL1-FILE.

           GO TO EXIT-INITIALIZE.
       ENDIT.
           MOVE "X" TO EXT-EOBUF-ERRCD.
       EXIT-INITIALIZE.
           EXIT.

      *************************************************
       EO-EXEC-AUDIT SECTION.
      *************************************************
           PERFORM FIND-DEFAULT-YEAR-END.
           PERFORM CHECK-PREVIOUSLY-RUN.
           IF STAT-BAD
              GO TO END-PROGRAM.

      *************************************************
       ENTER-PARAMETERS SECTION.
      *************************************************
           PERFORM FIND-DEFAULT-YEAR-END.
           MOVE "S  A080YEAR." TO VDU.
           MOVE YEAR-END-MMDDYY TO VDU-DATE-EDIT.
           PERFORM SCREENIO.

       REC-BEG-BRANCH.
           MOVE "ENNN040BEGBRN." TO VDU.
           PERFORM SCREENIO.
           IF F1-KEY GO TO EXIT-PARM.
           IF F7-KEY GO TO EXIT-PARM.
           IF F3-KEY GO TO ALL-BR.
           IF NOT ENTER-KEY GO TO REC-BEG-BRANCH.
           MOVE VDUNUM0 TO BEG-BRANCH-BUF.
       REC-END-BRANCH.
      *    MOVE "ENNN040ENDBRN." TO VDU.
      *    PERFORM SCREENIO.
      *    IF F1-KEY GO TO EXIT-PARM.
      *    IF UP-KEY GO TO REC-BEG-BRANCH.
      *    IF F7-KEY GO TO EXIT-PARM.
      *    IF NOT ENTER-KEY GO TO REC-END-BRANCH.
           MOVE "SN N040ENDBRN." TO VDU.
           MOVE BEG-BRANCH-BUF TO VDUNUM0 END-BRANCH-BUF.
           PERFORM SCREENIO.
           GO TO E-YEAR-END.
       ALL-BR.
           MOVE "SN N040BEGBRN." TO VDU.
           MOVE 0 TO BEG-BRANCH-BUF VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SN N040ENDBRN." TO VDU.
           MOVE 9999 TO END-BRANCH-BUF VDUNUM0.
           PERFORM SCREENIO.
      *-----------------------------------------------------------------
       E-YEAR-END.
           MOVE "ERNM060YEAR." TO VDU.
           PERFORM SCREENIO.
           IF UP-KEY GO TO REC-BEG-BRANCH.
           IF F1-KEY GO TO EXIT-PARM.
           IF F7-KEY GO TO EXIT-PARM.
           IF NOT (ENTER-KEY OR DOWN-KEY) GO TO E-YEAR-END.

           MOVE VDUNUM0 TO YEAR1-MMDDYY.
           IF YEAR1-MMDDYY = 0
              GO TO E-YEAR-END.
           IF YEAR1-MMDDYY NOT = YEAR-END-MMDDYY
              MOVE "GLOBAL FISCAL YEAR END CONFLICT, RE-ENTER YEAR END"
              TO MESS PERFORM SEND-MESS
              GO TO E-YEAR-END.

           PERFORM CHECK-PREVIOUSLY-RUN.
           IF STAT-BAD
              GO TO REC-BEG-BRANCH.

       EXIT-PARM.
           EXIT.

      *************************************************
       DISPLAY-PARAMETERS SECTION.
      *************************************************
           MOVE "SNNN040BEGBRN." TO VDU.
           MOVE BEG-BRANCH-BUF TO VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN040ENDBRN." TO VDU.
           MOVE END-BRANCH-BUF TO VDUNUM0.
           PERFORM SCREENIO.
           PERFORM FIND-DEFAULT-YEAR-END.
           MOVE "S  A080YEAR." TO VDU.
           MOVE YEAR-END-MMDDYY TO VDU-DATE-EDIT.
           PERFORM SCREENIO.


      *************************************************
       CHECK-PREVIOUSLY-RUN SECTION.
      *************************************************

      * THIS IS A CHECK TO SEE IF YEAR END HAS BEEN RUN FOR SAME
      * YEAR END,SAME BRANCH RANGE( 1 BRANCH OR ALL). SINCE THIS WILL
      * BE RUN FOR ALL BRANCHES AND ONLY ONCE A YEAR, NO HARM STARTING
      * AT BEGINNING OF FILE. IF THEY WERE TO DO A BRANCH AT A TIME, WHICH
      * SEEMS DUMB, WE NEED TO REVISIT START AT BEGINNING OF FILE.
           MOVE "00"            TO STAT.
           MOVE "ENDYER"        TO GLLOGS-ID.
           MOVE 19000101        TO GLLOGS-SYSTEM-DATE.
           MOVE 0               TO GLLOGS-SEQNO.
           PERFORM START-GLLOGS1-FILE.
        NEXT-GLLOGS1.
           PERFORM READ-GLLOGS1-FILE-NEXT.
           IF IO-BAD
              GO TO EXIT-CHECK-PREVIOUSLY-RUN.

           IF GLLOGS-ID NOT = "ENDYER"
              GO TO EXIT-CHECK-PREVIOUSLY-RUN.

           MOVE YEAR-END-MMDDYY TO DATE-MMDDYY.
           PERFORM CONVERT-MMDDYY-TO-YYYYMMDD.
           IF (GLLOGS-END-DATE NOT = DATE-YYYYMMDD)
              GO TO EXIT-CHECK-PREVIOUSLY-RUN.

      * A15 DID AN ACCESS CALL OF 9999 BRANCH FIRST, IF EXISTED, YOU
      * COULD NOT RUN AGAIN, IT WAS ALREADY RAN FOR ALL
      * SO IF YOU FIND ENDYER, YEAR-END-MMDDYY, 9999, YOU CAN NOT RUN AGAIN
           IF (GLLOGS-BRNO-RANGE = 9999)
              MOVE "END OF YEAR WAS RUN FOR ALL BRANCHES !" TO MESS
              PERFORM SEND-MESS
              MOVE "99" TO STAT
              GO TO EXIT-CHECK-PREVIOUSLY-RUN.

      * IN A15, IF YOU RAN FOR BRANCH 803 AND THEN TRIED TO RUN FOR
      * "ALL", IT WOULD LET YOU. SAME HERE.

      * YOU ARE ONLY HERE IF YOU READ AN ENDYER RECORD OTHER THAN "ALL"
      * BRANCHES

            IF (END-BRANCH-BUF NOT = 9999) AND
                (GLLOGS-BRNO-RANGE = END-BRANCH-BUF)
               MOVE "END OF YEAR WAS RUN FOR THIS BRANCH !" TO MESS
               PERFORM SEND-MESS
               MOVE "99" TO STAT
               GO TO EXIT-CHECK-PREVIOUSLY-RUN.

             
            GO TO NEXT-GLLOGS1.

       EXIT-CHECK-PREVIOUSLY-RUN.
           EXIT.

      ************************************************************
       CREATE-TAG-FILE SECTION.
      ************************************************************

      * CREATE TAG FILE TO PREVENT RE-UPDATE.

           PERFORM CLEAR-GLLOGS-FILE.

           MOVE "ENDYER"        TO GLLOGS-ID.
           ACCEPT GLLOGS-SYSTEM-DATE FROM CENTURY-DATE.
      * SEQ WAS FOUND IN PREVIOUSLY RUN SECTION, IF RAN FOR 1 BRANCH 
      * SEPARATE
           MOVE SEQ             TO GLLOGS-SEQNO.
           MOVE GP-BEG-FISCAL-DATE     TO GLLOGS-BEG-DATE. 
           MOVE GP-END-FISCAL-DATE(12) TO GLLOGS-END-DATE.
           MOVE END-BRANCH-BUF         TO GLLOGS-BRNO-RANGE.
           PERFORM GET-TIME.
           MOVE HH-MM-SS               TO GLLOGS-TIME
           MOVE 0                      TO GLLOGS-RECS-PURGED.
           MOVE "LOGNAME"              TO GETENV-BUF.
           PERFORM GETENV-CALL.
           MOVE GETENV-BUF             TO GLLOGS-USERID.
           PERFORM WRITE-GLLOGS1-FILE.
           IF IO-BAD
               MOVE "CANNOT CREATE LOG FILE" TO MESS
               PERFORM SEND-MESS 
               MOVE "99" TO STAT.

      ********************************************
       FIND-DEFAULT-YEAR-END SECTION.
      ********************************************
           PERFORM GET-GPENV.

           MOVE GP-END-FISCAL-DATE(12) TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO YEAR-END-MMDDYY.


      **********************************************************
       FORM-RESET SECTION.
      **********************************************************
           MOVE EXT-CONAME-CONAME TO ENDYER-CONAME.
           MOVE EXT-CONAME-SYSDATE TO ENDYER-SYSDATE.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DISPLAY ENDYER-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE ENDYER-FORM-FIELDS TO WR-BUF.
           MOVE ENDYER-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      **********************************************************
       SETUP-LINK-INFO SECTION.
      **********************************************************
           MOVE ENDYER-HELP--FILE TO HELP-LINK-FILE.
           MOVE ENDYER-HELP--DIR  TO HELP-LINK-DIR.

      **********************************************************
       VDU-CONVERT-FIELD SECTION.
      **********************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBGL/ENDYER_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/GPENV.CPY".
       COPY "LIBGL/GLGLLOGSCL.CPY". 
       COPY "LIBGB/TIMEIO.CPY".
       COPY "LIBWI/WIMESS.CPY".

      **********************************************************
       MISC-ROUTINES SECTION.
      **********************************************************
       COPY "LIBGB/GETPR.CPY".
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/SYSTEM.CPY".
       COPY "LIBGB/GETENV.CPY".

      ****************************************
       IO-ROUTINES SECTION.
      ****************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/DECLRP_SQL.CPY".
           PERFORM CLOSE-GLLOGS1-FILE.
           PERFORM CLOSE-GL1-FILE.
           PERFORM CLOSE-BR-FILE.
       COPY "LIBGL/GLGLLOGSGS_SQL.CPY".
       COPY "LIBGL/GLGLGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGL/GLGL1IN.CPY".
       COPY "LIBGB/GBBRIN.CPY".
       COPY "LIBGL/GLGLLOGS1IN.CPY".
       COPY "LIBGB/FERRORS.CPY".

      ****************************************
       END-PROGRAM SECTION.
      ****************************************
       END-RUN.
           PERFORM CLOSE-FILES.
       EXIT-PROG.
           IF EXT-ROUTE-BUF = "00"
              MOVE "GL/GLMENU" TO FORM-NAM
              MOVE FORM-PATH TO FORM-PATHNAME.
       STOP-RUN.
           IF EXT-ROUTE-BUF = "00" OR "40" OR "50"
              DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN
              DESTROY ENDYER-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.
           EXIT PROGRAM.
