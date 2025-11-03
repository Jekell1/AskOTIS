       IDENTIFICATION DIVISION.
       PROGRAM-ID.    RCOPEN.
       AUTHOR.        BLM.
       DATE-WRITTEN.  11-02-2009.
      ******************************************************************
      *
      *  IDENTIFICATION: SP/RCOPEN.C
      *
      *  DESCRIPTION   : FORCE OPEN DAYS
      *  TO BE RUN AT NIGHT IN BATCH BY REGACC, NEED BRANCHES OPEN
      *  FOR CERTAIN NIGHT JOBS.  PROGRAM WILL WRITE LOGS THAT CAN
      *  BE CHECKED FOR ERRORS BY EMAIL ROBOTS.
      *
      *-----------------------------------------------------------------
      * REV:
      * BLM 091102 ***NEW***
      * BLM 100326 FIX TO ONLY OPEN BRANCHES THAT ARE NOT CURRENTLY
      *            OPEN.  ADD BRANCH FLAG (BR-ALLOW-FORCE-OPEN-FG) &
      *            SKIP UNLESS FLAG = Y.  ADD LOGS FOR SUCCESSFUL OR
      *            FAILED OPENS IN /USR/DATA/R1/SP/AUTOTOPEN (AUTOOPEN
      *            IS IN CAPS, SUCCESSFUL OPENS IN LOG.YYMMDD, FAILS
      *            ARE IN ERR.YYMMDD (LOG & ERR ARE CAPS).  CONTENT, EG:
      *            LOG:
      *            10:05 AM BR # 1 03/26/10 BRANCH ALREADY OPEN, STATUS = A1
      *            10:05 AM BR #10 03/26/10 BRANCH OPENED, STATUS = A1:
      *                                                 SUCCESSFULLY OPENED
      *            ERROR LOG:
      *            10:05 AM BR # 2 03/26/10 BRANCH NOT OPENED, STATUS = D1:
      *                                                 EOD BATCH DONE
      * BLM 100427 IF BRANCH AUTO-OPEN FLAG IS ON, TEST FOR OTHER OPEN DAYS
      *            BEFORE FORCE OPENING SELECTED DAY, REGACC PR# 265
      * BLM 100511 CHANGE PRIOR OPEN TO GO TO ERROR LOG, FIX ERROR MSG IF
      *            AUTOOPEN DIRECTORY DOES NOT EXIST, REGACC PR# 265
      * BLM 100519 ADD CHMOD ON OPFILE AFTER OPENING IT, RUNNING THIS
      *            PROGRAM THRU EOCRON, BRANCHES COULD NOT GET INTO LOAN
      *            PROCESSING BECAUSE OF ACCES ON OPFILE, REGACC PR# 265
      * BLM 140509 FIX CHMOD ON OPEN FLAG TO BE 770 INSTEAD OF 777 IF
      *            I-AM-REGACC, EMBED CHMOD IN PROGRAM & GET RID OF
      *            RCOPEN.CMD, REGACC PR# 353
      *
      * KEC 2015.1124 {PD#00003} PARADATA <A30> [JAY CISZEWSKI]
      *         FIXED RC-TRANS-DATE FROM YYYMMDD TO CCYYMMDD.
      *
      *  20171013 BAH ACTIVATE 8 DIGIT DATES, PD0006
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "LIBLP/LPFSOP.CPY".
           SELECT OPENLOG-FILE ASSIGN OPENLOG-PATH
                             ORGANIZATION LINE SEQUENTIAL
                             ACCESS SEQUENTIAL
                             FILE STATUS FILE-STAT.

       DATA DIVISION.
       FILE SECTION.
       COPY "LIBLP/LPFDOP.CPY".
      **************************************************
      *  AUTO OPEN LOG FILE                            *
      **************************************************
       FD  OPENLOG-FILE
           LABEL RECORDS ARE STANDARD.
       01  OPENLOG-REC.
           03  OPENLOG-TIME     PIC X(8).
           03  FILLER           PIC X.
           03  OPENLOG-BRHEAD   PIC X(4) VALUE "BR# ".
           03  OPENLOG-BRNO     PIC Z(4).
           03  FILLER           PIC XX.
           03  OPENLOG-DATE     PIC 99/99/99 BLANK ZERO.
           03  FILLER           PIC XX.
           03  OPENLOG-MSG      PIC X(51).
           03  FILLER REDEFINES OPENLOG-MSG.
               05  OPENLOG-MSG1 PIC X(27).
               05  OPENLOG-MSG2 PIC X(24).


      ******************************************************************
      *
      *    W O R K I N G - S T O R A G E - S E C T I O N
      *
      ******************************************************************
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)SP/RCOPEN.CBL J35 12/21/23-09:46:17 >".

      *=================================================================
      *
      *    F I L E - W O R K E R S
      *
      *=================================================================
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBGB/GB01GR.CPY".
       COPY "LIBGB/GB01GR_SQL.CPY".
       COPY "LIBGB/GBWSGR.CPY".
       COPY "LIBGB/GB01RC.CPY".
       COPY "LIBGB/GB01RC_SQL.CPY".
       COPY "LIBGB/GBWSRC.CPY".
       COPY "LIBLP/LPEDPATH.CPY".

       01  OPENLOG-PATH.
           03  OPENLOG-PATH-BASE      PIC X(09).
           03  FILLER                 PIC X(01) VALUE "/".
           03  OPENLOG-PATH-MACHINE   PIC X(02).
           03  FILLER                 PIC X(13) VALUE "/SP/AUTOOPEN/".
           03  OPENLOG-PATH-NAME.
               05  OPENLOG-PATH-TYPE        PIC X(03).
               05  FILLER                   PIC X(01) VALUE ".".
               05  OPENLOG-PATH-DATE-YYMMDD PIC 9(06).

      *=================================================================
      *
      *    M I S C E L L A N E O U S - W O R K E R S
      *
      *=================================================================
       01  SAVE-OPENLOG-REC            PIC X(80)     VALUE SPACES.
       01  LEGEND                      PIC X(40)     VALUE SPACES.
       01  LEGEND-LIST                 PIC X(09)     VALUE "LEGEND:B.".
       01  RC-EXISTS                   PIC X         VALUE SPACES.
       01  RC-SUCCESSFUL               PIC X         VALUE SPACES.
       01  TODAYS-DATE                 PIC 9(8).
       01  DAY-IS-OPEN                 PIC X    VALUE SPACES.
       01  OPEN-TEST-FG                PIC X.
       01  PRIOR-OPEN-DATE             PIC 99/99/99.

       01  CHMOD-OPFILE-COMMAND.
           03  FILLER                      PIC X(6) VALUE "CHMOD ".
           03  CHMOD-OPFILE-ARGS           PIC 9(3) VALUE 777.
           03  FILLER                      PIC X    VALUE " ".
           03  CHMOD-OPFILE-PATH           PIC X(50).

      ******************************************************************
      *
      *    L I N E - W O R K E R S
      *
      ******************************************************************
       01  LN-FEED          PIC 99       VALUE 0.

      *  S C R E E N   -   F I E L D S
      *--------------------------------------------------------
       COPY "LIBGB/EOBUF_EXT.CPY".
           03  DATE-TO-OPEN            PIC 9(8).
           03  BEG-BRANCH              PIC 9(4).
           03  END-BRANCH              PIC 9(4).
           03  GROUP-OPTIONS.
               05  GROUP-FLAG          PIC X(01).
               05  ENT-GROUP           PIC X(04).

       01  VERSION-CONTROL         PIC X   VALUE "A".

      *=================================================================
      *
      *    C O P Y - L I B R A R Y - W O R K E R S
      *
      *=================================================================
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/BRSECUREW.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/GRWORKW.CPY".
       COPY "LIBGB/GETPRW.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBWI/GRSCANW.CPY".
       COPY "LIBGB/PASSWDW.CPY".
       78  RC-SEQ          VALUE 0.
       COPY "LIBGB/SYSTEMW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBSP/RCOPEN_DEF.CPY".
       COPY "LIBSP/RCOPEN_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

      *****************************************************************
      *
      *     P R O G R A M   C O N T R O L
      *
      *****************************************************************
       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBSP/RCOPEN_SCN.CPY".

      *----------------------------------------------------------------
       PROCEDURE DIVISION
           USING FORM-PATHNAME EXIT-PATHNAME.

       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
                 OP-FILE OPENLOG-FILE.
      *-----------------------------------------------------------------
       COPY "LIBGB/DECLRP.CPY".
           PERFORM CLOSE-RC2-FILE.
           PERFORM CLOSE-RC1-FILE.
           PERFORM CLOSE-GR1-FILE.
           PERFORM CLOSE-BR-FILE.
           CLOSE OP-FILE OPENLOG-FILE.
       COPY "LIBGB/DECLRP2.CPY".

      ******************************************************************
      *
      *    M A I N - P R O G R A M
      *
      ******************************************************************
       MAIN-PROGRAM SECTION.
           PERFORM SQL-CONNECT.
      D    STOP MESS.

           PERFORM INITIALIZATION.

           IF ( EXT-EOBUF-ERRCD NOT = SPACES )
              GO TO MAIN-PROGRAM-EXIT.

           PERFORM PRIMARY-DRIVER-MODULE.

       MAIN-PROGRAM-EXIT.
           GO TO END-PROGRAM.


      ******************************************************************
       INITIALIZATION SECTION.
      ******************************************************************
           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "RCOPEN"      TO FORM-PROG VDU-FORM-NAME.

           MOVE RCOPEN-QUEUE-POS TO Q-POS.
           MOVE RCOPEN-COPIES-POS TO C-POS.
           MOVE RCOPEN-PRU-POS TO P-POS.
           MOVE SPACES TO Q-BUF C-BUF P-BUF.
       COPY "LIBGB/EOINIT.CPY".

           IF ( EXT-EOBUF-ERRCD NOT = SPACES )
              GO TO INITIALIZATION-EXIT.

           IF EXT-ROUTE-BUF = "00"
              MOVE "PROGRAM IN PROGRESS...................." TO LEGEND
              PERFORM SEND-LEGEND.

       INITIALIZATION-EXIT.
           EXIT.

      ******************************************************************
       EO-EXEC-AUDIT SECTION.
      ******************************************************************
           MOVE EXT-EOBUF-DATE2 TO DATE-TO-OPEN.

      ******************************************************************
       ENTER-PARAMETERS SECTION.
      ******************************************************************
       ENTER-PARAMETERS-BEGIN.

           MOVE SPACES TO EXT-EOBUF-ERRCD.
           IF UP-KEY GO TO ENTER-RCDATE.

       DISPLAY-TODAYS-DATE.
           IF EXT-ROUTE-BUF = "40" OR "50"
               GO TO ENTER-BEG-BRANCH.

           MOVE EXT-CONAME-SYSDATE TO ALP-DATE.
           PERFORM CONVERT-ALPDATE-TO-YYYYMMDD.
           MOVE "S  8000RCDATE." TO VDU.
           MOVE DATE-YYYYMMDD TO DATE-TO-OPEN VDU-DATE-YYYYMMDD.
           PERFORM SCREENIO.

      *----------------------------------------------------
       ENTER-BEG-BRANCH.

           MOVE "E  A000BEG-BRANCH." TO VDU.
           PERFORM SCREENIO.

           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-BEGIN.
           IF (        F3-KEY ) GO TO ENTER-ALL-BRANCH.
           IF (        F6-KEY ) PERFORM SCAN-GR-FILE.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-END.
           IF ( NOT ENTER-KEY ) GO TO ENTER-BEG-BRANCH.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUBUF TO WGR-WORD ENT-GROUP.

           IF ( WGR-WORD-ZERO )
              GO TO BRANCH-IS-NUMERIC.

           MOVE ZEROES TO WGR-MIN-LEVEL.
           PERFORM CHECK-MEMBER.

           IF ( WGR-ERR > ZEROES )
              MOVE WGR-MSG(WGR-ERR) TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-BEG-BRANCH.

           IF ( WGR-TYPE = "A" )
              MOVE "S  A000END-BRANCH." TO VDU
              MOVE SPACES               TO VDUBUF
              PERFORM SCREENIO
              MOVE "Y"                  TO GROUP-FLAG
              MOVE ZEROES               TO BEG-BRANCH
              MOVE 9999                 TO END-BRANCH
              GO TO CHECK-BRANCH-SECURITY.

      *----------------------------------------------------------------
       BRANCH-IS-NUMERIC.
           MOVE "N"                  TO GROUP-FLAG.
           MOVE WGR-SRESULT          TO BEG-BRANCH.
           MOVE "SN N040BEG-BRANCH." TO VDU.
           MOVE BEG-BRANCH           TO VDUNUM0.
           PERFORM SCREENIO.

      *----------------------------------------------------------------
       ENTER-END-BRANCH.

           MOVE "ENNN040END-BRANCH." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-BEG-BRANCH.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-BEGIN.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-END.
           IF ( NOT ENTER-KEY ) GO TO ENTER-END-BRANCH.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUNUM0 TO END-BRANCH.

           IF ( BEG-BRANCH > END-BRANCH )
              MOVE "INVALID BRANCH RANGE" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-BEG-BRANCH.

           GO TO CHECK-BRANCH-SECURITY.

      *-----------------------------------------------------------------
       ENTER-ALL-BRANCH.

           MOVE "N"                  TO GROUP-FLAG.
           MOVE "SNNN040BEG-BRANCH." TO VDU.
           MOVE ZEROES               TO BEG-BRANCH VDUNUM0.
           PERFORM SCREENIO.

           MOVE "SNNN040END-BRANCH." TO VDU.
           MOVE 9999                 TO END-BRANCH VDUNUM0.
           PERFORM SCREENIO.

      *-----------------------------------------------------------------
       CHECK-BRANCH-SECURITY.

           PERFORM BR-RANGE-SECURITY.

           IF ( BR-SECURITY-OKAY = "N" )
              GO TO ENTER-BEG-BRANCH.

      *----------------------------------------------------
       ENTER-RCDATE.
           IF EXT-ROUTE-BUF = "40" OR "50"
               GO TO ENTER-PARAMETERS-EXIT.

           MOVE "ERNM060RCDATE." TO VDU.
           PERFORM SCREENIO.

           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-BEGIN.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-END.
           IF ( NOT ENTER-KEY ) GO TO ENTER-RCDATE.

           MOVE VDU-DATE-YYYYMMDD TO DATE-TO-OPEN.

           GO TO ENTER-PARAMETERS-EXIT.

       ENTER-PARAMETERS-END.

           MOVE "X" TO EXT-EOBUF-ERRCD.

       ENTER-PARAMETERS-EXIT.
           EXIT.

      ******************************************************************
       DISPLAY-PARAMETERS SECTION.
      ******************************************************************
           IF GROUP-FLAG = "Y"
              MOVE "S  A000BEG-BRANCH." TO VDU
              MOVE ENT-GROUP TO VDUBUF
              PERFORM SCREENIO
              MOVE "S  A000END-BRANCH." TO VDU
              MOVE SPACES TO VDUBUF
              PERFORM SCREENIO
           ELSE
              MOVE "SNNN040BEG-BRANCH." TO VDU
              MOVE BEG-BRANCH TO VDUNUM0
              PERFORM SCREENIO
              MOVE "SNNN040END-BRANCH." TO VDU
              MOVE END-BRANCH TO VDUNUM0
              PERFORM SCREENIO.

           MOVE "S  8080RCDATE." TO VDU.
           MOVE DATE-TO-OPEN     TO VDU-DATE-YYYYMMDD.
           PERFORM SCREENIO.

       DISPLAY-PARAMETERS-EXIT.
           EXIT.

      *****************************************************************
       SCAN-GR-FILE SECTION.
      *****************************************************************

           MOVE "WI/GRSCAN" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH GRSCAN-BUF EXIT-PATHNAME.
           CANCEL FORM-PROGX.
           IF GRSCAN-STATUS NOT = "00"
              MOVE "1<" TO VDUSTATUS
              GO TO SCAN-GR-FILE-EXIT.

           MOVE "S  A000BEG-BRANCH." TO VDU.
           MOVE GRSCAN-KEY           TO VDUBUF GR1-KEY.
           PERFORM SCREENIO.

           MOVE "00"       TO VDUSTATUS.
           MOVE GRSCAN-KEY TO VDUBUF.

       SCAN-GR-FILE-EXIT.
           EXIT.

      ******************************************************************
       PRIMARY-DRIVER-MODULE SECTION.
      ******************************************************************

           PERFORM OPEN-BR-FILE.
           PERFORM OPEN-GR1-FILE.
           PERFORM OPEN-RC1-FILE.

           IF ( GROUP-FLAG = "N" )
              MOVE BEG-BRANCH TO BR-NO
              PERFORM START-BR-FILE
              GO TO NEXT-BRANCH-RECORD.

           MOVE ENT-GROUP TO GR-ID.
           PERFORM SEARCH-GR-INIT.

      *=================================================================
      *    LOOPING TO GET NEXT GROUP OR BRANCH.
      *=================================================================
       NEXT-GROUP-RECORD.

           IF ( GROUP-FLAG = "N" )
              GO TO NEXT-BRANCH-RECORD.

           PERFORM SEARCH-GR-NEXT.

           IF ( WGR-SRESULT = ZEROES )
              GO TO PRIMARY-DRIVER-MODULE-END.

           MOVE WGR-SRESULT TO BR-NO.
           PERFORM READ-BR-FILE.
           IF ( IO-BAD )
              GO TO IO-ERROR.

           GO TO MOVE-BRANCH-PATH-INFO.

      *-----------------------------------------------------------------
       NEXT-BRANCH-RECORD.

           PERFORM READ-BR-FILE-NEXT.
           IF ( IO-BAD         ) OR
              ( BR-NO > END-BRANCH )
              GO TO PRIMARY-DRIVER-MODULE-END.

      *-----------------------------------------------------------------
       MOVE-BRANCH-PATH-INFO.

           PERFORM GET-SYSDATE-TIME.
           IF BR-ALLOW-FORCE-OPEN-FG NOT = "Y"
              MOVE "LOG" TO OPENLOG-PATH-TYPE
              MOVE "BRANCH SKIPPED, AUTO OPEN FLAG NOT Y" TO OPENLOG-MSG
              MOVE OPENLOG-REC TO SAVE-OPENLOG-REC
              PERFORM CREATE-OPENLOG
              GO TO NEXT-GROUP-RECORD.

           IF BR-AUTO-OPEN-BAL = "Y"
              PERFORM TEST-OPEN-DAYS
              IF OPEN-TEST-FG = "N"
                 MOVE "ERR" TO OPENLOG-PATH-TYPE
                 MOVE "BRANCH SKIPPED, PRIOR DATE OPEN: &&&&&&&&."
                    TO OPENLOG-MSG
                 INSPECT OPENLOG-MSG REPLACING FIRST "&&&&&&&&" BY
                                              PRIOR-OPEN-DATE
                 MOVE OPENLOG-REC TO SAVE-OPENLOG-REC
                 PERFORM CREATE-OPENLOG
                 GO TO NEXT-GROUP-RECORD.

           PERFORM PROCESS-RC-FILE.

           IF RC-SUCCESSFUL = "N"
              GO TO NEXT-GROUP-RECORD.

           MOVE "Y" TO OPEN-PATH-OVERRIDE.
           MOVE EXT-FILPATH-BASE           TO OPEN-PATH-BASE.
           MOVE BR-MACHINE                 TO OPEN-PATH-MACHINE.
           MOVE BR-NO                      TO OPEN-PATH-BRNO.
           MOVE DATE-TO-OPEN               TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY                TO OPEN-PATH-TRDATE.
           MOVE OPEN-PATH-FORMAT TO OPEN-PATH ACCESS-BUF.
           PERFORM ACCESS-CALL.
           IF STAT NOT = "00"
              PERFORM OPEN-OP-FILE-OUTPUT
              CALL "C$TOLOWER" USING CHMOD-OPFILE-COMMAND, VALUE 5
              MOVE 777                  TO CHMOD-OPFILE-ARGS
              MOVE OPEN-PATH            TO CHMOD-OPFILE-PATH
              MOVE CHMOD-OPFILE-COMMAND TO SYSTEM-BUF
              PERFORM SYSTEM-CALL.

           GO TO NEXT-GROUP-RECORD.

       PRIMARY-DRIVER-MODULE-END.

           PERFORM CLOSE-BR-FILE.
           PERFORM CLOSE-GR1-FILE.
           PERFORM CLOSE-RC1-FILE.

       PRIMARY-DRIVER-MODULE-EXIT.
           EXIT.

      ******************************************************************
       PROCESS-RC-FILE SECTION.
      ******************************************************************
           MOVE "N" TO RC-SUCCESSFUL.

           MOVE BR-NO        TO RC-BRNO.
           MOVE DATE-TO-OPEN TO RC-TRANS-DATE.

           PERFORM READ-RC1-FILE.

           IF IO-FG = 9
              MOVE "N" TO RC-EXISTS
           ELSE
              MOVE "Y" TO RC-EXISTS.

           IF RC-EXISTS = "N"
              PERFORM CLEAR-RC-FILE
              MOVE BR-NO        TO RC-BRNO
              MOVE DATE-TO-OPEN TO RC-TRANS-DATE
              MOVE "A1" TO RC-STATUS
              MOVE SPACES TO RC-ERROR-MSG
              MOVE TODAYS-DATE     TO RC-OPENED-DATE
              MOVE HH-MM-SS        TO RC-OPENED-TIME
              MOVE "AUTO OPEN"     TO RC-OP-USERID
              PERFORM WRITE-RC1-FILE
              IF IO-GOOD
                 MOVE "Y" TO RC-SUCCESSFUL
                 MOVE "LOG" TO OPENLOG-PATH-TYPE
                 MOVE "BRANCH OPENED, STATUS = A1:" TO OPENLOG-MSG1
                 MOVE " SUCCESSFULLY OPENED" TO OPENLOG-MSG2
                 MOVE OPENLOG-REC TO SAVE-OPENLOG-REC
                 PERFORM CREATE-OPENLOG
                 GO TO PROCESS-RC-FILE-EXIT
              ELSE
                 MOVE "ERR" TO OPENLOG-PATH-TYPE
                 MOVE "RC WRITE ERROR STAT: &&" TO OPENLOG-MSG2
                 INSPECT OPENLOG-MSG2 REPLACING FIRST "&&" BY FILE-STAT
                 MOVE OPENLOG-REC TO SAVE-OPENLOG-REC
                 PERFORM CREATE-OPENLOG
                 GO TO PROCESS-RC-FILE-EXIT.

           IF RC-STATUS = "A1"
              MOVE "LOG" TO OPENLOG-PATH-TYPE
              MOVE "BRANCH ALREADY OPEN, STATUS = A1" TO OPENLOG-MSG
              MOVE OPENLOG-REC TO SAVE-OPENLOG-REC
              PERFORM CREATE-OPENLOG
              GO TO PROCESS-RC-FILE-EXIT.

           MOVE "BRANCH NOT OPENED, STATUS = " TO OPENLOG-MSG1.
           IF RC-STATUS = "B1"
              MOVE " B1: UPDATE IN PROCESS" TO OPENLOG-MSG2
           ELSE
           IF RC-STATUS = "C1"
              MOVE " C1: DAY UPDATED" TO OPENLOG-MSG2
           ELSE
           IF RC-STATUS = "D1"
              MOVE " D1: EOD BATCH DONE" TO OPENLOG-MSG2
           ELSE
           IF RC-STATUS = "G1"
              MOVE " G1: ACCRUAL DONE " TO OPENLOG-MSG2
           ELSE
           IF RC-STATUS = "H1"
              MOVE " H1: EOM1 BATCH DONE" TO OPENLOG-MSG2
           ELSE
           IF RC-STATUS = "I1"
              MOVE " I1: EOM UPDATED" TO OPENLOG-MSG2
           ELSE
           IF RC-STATUS = "J1"
              MOVE " J1: EOM2 BATCH DONE" TO OPENLOG-MSG2
           ELSE
           IF RC-STATUS = "K1"
              MOVE " K1: BANK DEPOSIT" TO OPENLOG-MSG2
           ELSE
           IF RC-STATUS = "K2"
              MOVE " K2: BANK TRANSMISSION" TO OPENLOG-MSG2
           ELSE
           IF RC-STATUS = "M1"
              MOVE " M1: CHECK EXTRACT DONE" TO OPENLOG-MSG2.

           MOVE OPENLOG-REC TO SAVE-OPENLOG-REC.
           MOVE "ERR" TO OPENLOG-PATH-TYPE.
           PERFORM CREATE-OPENLOG.

       PROCESS-RC-FILE-EXIT.
           EXIT.

      ******************************************
       CREATE-OPENLOG SECTION.
      ******************************************

      * CREATE AUTO AUTO DAY LOGS

           MOVE EXT-FILPATH-BASE    TO OPENLOG-PATH-BASE.
           MOVE EXT-FILPATH-MACHINE TO OPENLOG-PATH-MACHINE.
           MOVE DATE-TO-OPEN        TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-YYMMDD.
           MOVE DATE-YYMMDD         TO OPENLOG-PATH-DATE-YYMMDD.

           MOVE OPENLOG-PATH TO ACCESS-BUF.
           PERFORM ACCESS-CALL.
           IF STAT-GOOD
              PERFORM OPEN-OPENLOG-FILE-EXTEND
           ELSE
              PERFORM OPEN-OPENLOG-FILE-OUTPUT.
           MOVE SAVE-OPENLOG-REC TO OPENLOG-REC.
           MOVE "BR #" TO OPENLOG-BRHEAD.
           MOVE BR-NO TO OPENLOG-BRNO.
           MOVE TODAYS-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO OPENLOG-DATE.
           PERFORM GET-TIME.
           MOVE TIME-EDIT TO OPENLOG-TIME.
           PERFORM WRITE-OPENLOG-FILE.
           PERFORM CLOSE-OPENLOG-FILE.

      ******************************************************************
       TEST-OPEN-DAYS SECTION.
      ******************************************************************
           MOVE " "       TO OPEN-TEST-FG.
           MOVE " "       TO DAY-IS-OPEN.
           PERFORM CLOSE-RC1-FILE.

           PERFORM OPEN-RC2-FILE.
           MOVE "A1"     TO RC-STATUS
                            QRC2-WBEG-STATUS
                            QRC2-WEND-STATUS.
           MOVE BR-NO    TO RC-BRNO
                            QRC2-WBEG-BRNO
                            QRC2-WEND-BRNO.
           MOVE 19000101 TO RC-TRANS-DATE SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO QRC2-WBEG-TRANS-DATE.
           MOVE EXT-JULIAN-END TO SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO QRC2-WEND-TRANS-DATE.

           PERFORM START-RC2-FILE.
       TEST-OPEN-NEXT-RC.
           PERFORM READ-RC2-FILE-NEXT.
           IF ( IO-FG NOT = 0 )
              GO TO EXIT-TEST-OPEN-DAYS.

           IF ( RC-BRNO NOT = BR-NO )
              GO TO EXIT-TEST-OPEN-DAYS.

           IF ( RC-STATUS NOT = "A1" )
              GO TO EXIT-TEST-OPEN-DAYS.

           IF RC-TRANS-DATE = DATE-TO-OPEN
              MOVE "Y" TO DAY-IS-OPEN
              GO TO TEST-OPEN-NEXT-RC.

      *  OTHER DAYS ARE OPEN, SET FLAG TO "N":
           MOVE "N" TO OPEN-TEST-FG.
           MOVE RC-TRANS-DATE      TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY        TO PRIOR-OPEN-DATE.

      *-----------------------------------------------------------------
       EXIT-TEST-OPEN-DAYS.
           PERFORM CLOSE-RC2-FILE.
           PERFORM OPEN-RC1-FILE.

      ******************************************************************
      *
      *    C O P Y - L I B R A R Y - S E C T I O N S
      *
      ******************************************************************
       COPY "LIBGB/BRSECURE.CPY".
       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/GBRCCL.CPY".
       COPY "LIBGB/GRSERCH.CPY".
       COPY "LIBGB/GRWORK.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/TIMEIO.CPY".


      *********************************************************
       FORM-RESET SECTION.
      *********************************************************
           MOVE EXT-CONAME-CONAME TO RCOPEN-CONAME.
           MOVE EXT-CONAME-SYSDATE TO RCOPEN-SYSDATE.
           DISPLAY RCOPEN-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE RCOPEN-FORM-FIELDS TO WR-BUF.
           MOVE RCOPEN-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.
       FORM-RESET-EXIT.
           EXIT.

      *********************************************************
       SETUP-LINK-INFO SECTION.
      *********************************************************
           MOVE RCOPEN-HELP--FILE TO HELP-LINK-FILE.
           MOVE RCOPEN-HELP--DIR  TO HELP-LINK-DIR.

      *********************************************************
       VDU-CONVERT-FIELD SECTION.
      *********************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBSP/RCOPEN_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ******************************************************************
       MISC-ROUTINES SECTION.
      ******************************************************************
       COPY "LIBGB/GETPR.CPY".
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/SYSTEM.CPY".
      *-----------------------------------------------------------------
       SEND-LEGEND.
           MOVE LEGEND TO WR-BUF.
           MOVE LEGEND-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.
      *-----------------------------------------------------------------
       GET-SYSDATE-TIME.
           MOVE EXT-CONAME-SYSDATE TO ALP-DATE.
           PERFORM CONVERT-ALPDATE-TO-YYYYMMDD.
           MOVE DATE-YYYYMMDD TO TODAYS-DATE.
           MOVE "Y"      TO TIME-MILITARY-FG.
           PERFORM GET-TIME.

      ******************************************************************
       IO-ROUTINE SECTION.
      ******************************************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/GBRCGS_SQL.CPY".
       COPY "LIBGB/GBGRGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBGB/GBGR1RN.CPY".
       COPY "LIBGB/GBRC1IN.CPY".
       COPY "LIBGB/GBRC2RN.CPY".
       COPY "LIBLP/LPOPIN.CPY".

       OPEN-OPENLOG-FILE-EXTEND.
           MOVE OPENLOG-PATH TO E-FILE.
           OPEN EXTEND OPENLOG-FILE.
           IF IO-FG = 8
              GO TO OPEN-OPENLOG-FILE-EXTEND.
           IF IO-FG = 7
              CLOSE OPENLOG-FILE
              GO TO OPEN-OPENLOG-FILE-EXTEND.

       OPEN-OPENLOG-FILE-OUTPUT.
           MOVE OPENLOG-PATH TO E-FILE.
           OPEN OUTPUT OPENLOG-FILE.
           IF IO-FG = 8
              GO TO OPEN-OPENLOG-FILE-OUTPUT.
           IF IO-FG = 7
              CLOSE OPENLOG-FILE
              GO TO OPEN-OPENLOG-FILE-OUTPUT.

       OPEN-OPENLOG-FILE.
           MOVE OPENLOG-PATH TO E-FILE.
           OPEN INPUT OPENLOG-FILE.
           IF IO-FG = 8
              GO TO OPEN-OPENLOG-FILE.
           IF IO-FG = 7
              CLOSE OPENLOG-FILE
              GO TO OPEN-OPENLOG-FILE.
       WRITE-OPENLOG-FILE.
           MOVE OPENLOG-PATH TO E-FILE.
           WRITE OPENLOG-REC.
       CLOSE-OPENLOG-FILE.
           MOVE OPENLOG-PATH TO E-FILE.
           CLOSE OPENLOG-FILE.

       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO EXT-EOBUF-ERRCD.

      ******************************************************************
       END-PROGRAM SECTION.
      ******************************************************************
       END-RUN.
           PERFORM CLOSE-FILES.

       EXIT-PROG.
           IF EXT-ROUTE-BUF = "00"
               MOVE EXIT-PATH  TO FORM-NAM
               MOVE FORM-PATH TO FORM-PATHNAME.

       STOP-RUN.
           IF EXT-ROUTE-BUF = "00" OR "40" OR "50"
              DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN
              DESTROY RCOPEN-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.
           EXIT PROGRAM.
