      * A30 CONVERSION COMPLETED
       IDENTIFICATION DIVISION.
       PROGRAM-ID.      CKSTAT.
      ******************************************************************
      *
      *                 (CL)
      *
      *  DESCRIPTION:   CHECK RECAP STATUS
      *           IN:   CKSTAT-ACTION, CKSTAT-HIGH-STATUS
      *                 CKSTAT-DATE1, CKSTAT-DATE2,
      *                 CKSTAT-BEG-BRANCH (OR GROUP), CKSTAT-END-BRANCH
      *
      *                 CKSTAT-ACTION OF "C" - CHECKS FOR ANY RC IN
      *                 DATE & BRANCH RANGE < CKSTAT-HIGH-STATUS;
      *                 RETURNS CKSTAT-RETURN-STAT OF "N" IF FOUND
      *
      *                 CKSTAT-ACTION OF "D" - CHECKS FOR OLDEST RC IN
      *                 DATE & BRANCH RANGE = CKSTAT-HIGH-STATUS
      *                 RETURNS CKSTAT-RETURN-DATE (999999) IF NONE
      *                 FOUND
      *
      *    CALLED BY:   SP/CHKREC, SP/ACHCRE
      * REV:
      *  MG  990928 FIXED VDUSTATUS/IO-FG/STAT PER DEVELOPMENT MEETING.
      *
      * KEC 2015.1120 {PD#00003} PARADATA <A30> [JAY CISZEWSKI]
      *         FIXED RC-TRANS-DATE FROM YYYMMDD TO CCYYMMDD.
      *
      * BAH 170323 ACTIVATE 8 DIGIT DATES, PD0006
      * BLM 180312 INCREASE LINK-BUF FROM 32 TO 38 TO ACCOMODATE LONGER
      *            DATES, CKSTAT-RETURN-DATE WAS TRUNCATED
      * BAH 20220826 HARD CODED START
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
           "@(#)CL/CKSTAT.CBL S34 05/24/21-15:32:27 >".
       COPY "LIBGB/GB01RC.CPY".
       COPY "LIBGB/GB01RC_SQL.CPY".
       COPY "LIBGB/GBWSRC.CPY".
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBGB/GB01GR.CPY".
       COPY "LIBGB/GB01GR_SQL.CPY".
       COPY "LIBGB/GBWSGR.CPY".
      *    **********************************
      *    *      PROGRAM WORKERS
      *    **********************************
       01  ERRCD             PIC X     VALUE SPACES.
       01  HOLD-BR-KEY       PIC 9999  VALUE 0.

       01  TEST-RC-TRANS-DATE                  PIC 9(08).

       01  MSEL-LIST         PIC X(8)       VALUE "MSEL:P.".
       01  IO-TYPE           PIC X(3).

       01  BEG-BRANCH      PIC 9(4).
       01  END-BRANCH      PIC 9(4).
       01  DATE1-YYYYMMDD  PIC 9(8).
       01  DATE2-YYYYMMDD  PIC 9(8).

      *----------------------------------GROUP
       01  GROUP-FLAG        PIC X.
       COPY "LIBGB/GRWORKW.CPY".
      *----------------------------------GROUP

       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/SYSTEMW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/PHON400.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBCL/CKSTATW.CPY".

       COPY "LIBGB/GETFMW.CPY".
       01  LINK-BUF         PIC X(38).
       PROCEDURE DIVISION
           USING FORM-PATHNAME LINK-BUF EXIT-PATHNAME.

      *    ***************************
      *    *      MAIN PROGRAM MODULE
      *    ***************************
       MAIN-PROGRAM SECTION.

           PERFORM SQL-CONNECT.

      D    STOP MESS.
           MOVE LINK-BUF TO CKSTAT-BUF.

           MOVE "CHECKING BRANCH STATUSES" TO MESS.
           PERFORM SEND-LEGEND.

           PERFORM SETUP-RANGES.

           IF CKSTAT-ACTION = "C"
              PERFORM CHECK-STATUSES
           ELSE
              PERFORM CHECK-DATES.

           GO TO END-ROUTINE.

      ***************************
       SETUP-RANGES SECTION.
      ***************************

      * SEND BRANCH
           IF CKSTAT-GROUP NUMERIC
              MOVE "N" TO GROUP-FLAG
              MOVE CKSTAT-BEG-BRANCH TO BEG-BRANCH
              MOVE CKSTAT-END-BRANCH TO END-BRANCH
              GO TO SEND-DATE1.

      * SEND GROUP
           MOVE CKSTAT-GROUP TO WGR-WORD.
           MOVE 0 TO WGR-MIN-LEVEL.
           PERFORM CHECK-MEMBER.
           IF WGR-ERR > 0 AND WGR-ERR NOT = 1
              MOVE WGR-MSG(WGR-ERR) TO MESS
              PERFORM SEND-MESS
              MOVE "CLR" TO IO-TYPE
              GO TO EXIT-SETUP-RANGES.

           MOVE WGR-WORD TO GR-ID.
           MOVE 0        TO BEG-BRANCH.
           MOVE 9999     TO END-BRANCH.
           MOVE "Y"      TO GROUP-FLAG.
           PERFORM OPEN-GR1-FILE.
           PERFORM SEARCH-GR-INIT.
           PERFORM CLOSE-GR1-FILE.

       SEND-DATE1.
           MOVE CKSTAT-DATE1 TO DATE1-YYYYMMDD.
           MOVE CKSTAT-DATE2 TO DATE2-YYYYMMDD.
           IF DATE1-YYYYMMDD = 0
              MOVE 19000101 TO DATE1-YYYYMMDD.
           IF DATE2-YYYYMMDD = 0
              MOVE 19000101 TO DATE2-YYYYMMDD.
           IF DATE2-YYYYMMDD = 99999999
              MOVE 99991231 TO DATE2-YYYYMMDD.

       EXIT-SETUP-RANGES.
           EXIT.

      ***************************
       CHECK-STATUSES SECTION.
      ***************************

           MOVE "Y" TO CKSTAT-RETURN-STAT.

           PERFORM OPEN-RC2-FILE.

           MOVE SPACES         TO RC-STATUS
                                  QRC2-WBEG-STATUS.
           MOVE ALL "Z"        TO QRC2-WEND-STATUS.
           MOVE BEG-BRANCH     TO RC-BRNO
                                  QRC2-WBEG-BRNO.
           MOVE END-BRANCH     TO QRC2-WEND-BRNO.
           MOVE DATE1-YYYYMMDD TO RC-TRANS-DATE
                                  SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO QRC2-WBEG-TRANS-DATE.

           MOVE DATE2-YYYYMMDD      TO SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO QRC2-WEND-TRANS-DATE.

           PERFORM START-RC2-FILE.

       NEXT-RC.
           PERFORM READ-RC2-FILE-NEXT.
           IF IO-BAD
              GO TO EXIT-CHECK-STATUSES.

      *------------------------------------GROUP
           IF GROUP-FLAG = "Y"
              MOVE RC-BRNO TO WGR-SRESULT
              PERFORM SEARCH-GR-VERIFY
              IF WGR-ERR NOT = 0
                 GO TO NEXT-RC
              ELSE
                 NEXT SENTENCE
           ELSE
              IF RC-BRNO < BEG-BRANCH OR > END-BRANCH
                 GO TO NEXT-RC.

      *------------------------------------GROUP

           IF RC-TRANS-DATE < DATE1-YYYYMMDD OR
               RC-TRANS-DATE > DATE2-YYYYMMDD
              GO TO NEXT-RC.

           IF RC-STATUS NOT < CKSTAT-HIGH-STATUS
              GO TO EXIT-CHECK-STATUSES.

           MOVE "N" TO CKSTAT-RETURN-STAT.

       EXIT-CHECK-STATUSES.
           EXIT.

      *************************************************
       CHECK-DATES SECTION.
      *************************************************
       START-RC-DATE.
           PERFORM OPEN-RC2-FILE.

           MOVE 99991231 TO TEST-RC-TRANS-DATE.

           MOVE CKSTAT-LOW-STATUS TO RC-STATUS
                                     QRC2-WBEG-STATUS
                                     QRC2-WEND-STATUS.
           MOVE BEG-BRANCH        TO RC-BRNO
                                     QRC2-WBEG-BRNO.
           MOVE END-BRANCH        TO QRC2-WEND-BRNO.
           MOVE DATE1-YYYYMMDD    TO RC-TRANS-DATE
                                     SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO QRC2-WBEG-TRANS-DATE.

           MOVE DATE2-YYYYMMDD        TO SQL-DATE-YYYYMMDD
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO QRC2-WEND-TRANS-DATE.

           PERFORM START-RC2-FILE.

       NEXT-RC-DATE.
           PERFORM READ-RC2-FILE-NEXT.
           IF IO-BAD
              GO TO EXIT-CHECK-DATES.

      *------------------------------------GROUP
           IF GROUP-FLAG = "Y"
              MOVE RC-BRNO TO WGR-SRESULT
              PERFORM SEARCH-GR-VERIFY
              IF WGR-ERR NOT = 0
                 GO TO NEXT-RC-DATE
              ELSE
                 NEXT SENTENCE
           ELSE
              IF RC-BRNO < BEG-BRANCH OR > END-BRANCH
                 GO TO NEXT-RC-DATE.
      *------------------------------------GROUP

           IF RC-TRANS-DATE < DATE1-YYYYMMDD OR
                RC-TRANS-DATE > DATE2-YYYYMMDD
              GO TO NEXT-RC-DATE.

           IF RC-STATUS > CKSTAT-HIGH-STATUS
              GO TO EXIT-CHECK-DATES.

           IF RC-TRANS-DATE < TEST-RC-TRANS-DATE
              MOVE RC-TRANS-DATE TO TEST-RC-TRANS-DATE.

           GO TO NEXT-RC-DATE.

       EXIT-CHECK-DATES.
           MOVE TEST-RC-TRANS-DATE  TO CKSTAT-RETURN-DATE.

      *--------------------------------------------
       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/GRWORK.CPY".
       COPY "LIBGB/GRSERCH.CPY".
       COPY "LIBGB/SCREEN.CPY".

      *************************
       MISC-PARAGRAPHS SECTION.
      *************************
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/SYSTEM.CPY".
       FORM-RESET.
       SETUP-LINK-INFO.
       VDU-CONVERT-FIELD.
       GET-BR.
           IF BR-NO NOT = HOLD-BR-KEY
              PERFORM OPEN-BR-FILE
              PERFORM READ-BR-FILE
              PERFORM CLOSE-BR-FILE
              IF IO-GOOD
                 MOVE BR-NO TO HOLD-BR-KEY.
       SEND-LEGEND.
           MOVE MESS TO WR-BUF.
           MOVE MSEL-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      *****************************************
       IO-ROUTINES SECTION.
      *****************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/DECLARE_SQL.CPY".
           PERFORM CLOSE-RC2-FILE.
           PERFORM CLOSE-GR1-FILE.
           PERFORM CLOSE-BR-FILE.
       COPY "LIBGB/GBRCGS_SQL.CPY".
       COPY "LIBGB/GBGRGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
      *****************************************
       COPY "LIBGB/GBRC2RN.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBGB/GBGR1RN.CPY".
       COPY "LIBGB/FERRORS.CPY".

      *****************************************
       END-ROUTINE SECTION.
      *****************************************
           PERFORM CLOSE-FILES.
       EXIT-PROG.
           MOVE CKSTAT-BUF TO LINK-BUF.
       STOP-RUN.
           EXIT PROGRAM.
