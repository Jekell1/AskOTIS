       IDENTIFICATION DIVISION.
       PROGRAM-ID.    DTLBAL.
       AUTHOR.        MJD.
       DATE-WRITTEN. 122195.
      ******************************************************************
      *
      *  DIR : GL
      *  DESC: G/L DETAIL TRIAL BALANCE REPORT
      *             REPORT LISTING BY BRANCH, ACCOUNT.
      *
      *=================================================================
      * REV:
      * MJD 010495 PROGRAM WAS PERFORMING PRU-OUT WITH EXT-ROUTE-BUF = "70"
      *            CORRECTED TO PRINT ONLY WITH EXT-ROUTE-BUF "00".
      * GLF 090497 REPLACED 'IO' ON GP-FILE WITH GET 'GPENV'
      * BLV 081498 DON'T CHECK ZERO BALANCE OPTION WHEN PRINTING BRANCH
      *            OR GRAND TOTALS
      * BLV 990427 REGENCY PL# 216 - FIXED PRINTING BLANK PAGE FOR EACH
      *            BRANCH IF ACCTS HAVE 0 OPEN & CLOSE & NO ACTITIVTY;
      *            ADDED DETAIL COUNTER BY LEVEL, BECAUSE 8/14 FIX WAS
      *            BECAUSE IF ALL ACCTS IN BRANCH HAVE ZERO OPENING BALS
      *            & DEBITS & CREDITS BOTH NETTED TO ZERO, DETAIL WAS
      *            NOT PRINTING
      * BLV 990831 FIX TO RUN IN BATCH PROCESSING BY GROUP
      * GLB 991028 FIX SO PRINT BY GROUP LOGIC INCLUDES BRANCH TOTALS
      *  MG 000201 FIXED VDUSTATUS/IO-FG/STAT PER DEVELOPMENT MEETING.
      * GLB 040429 IF PREVIOUS YEAR SELECTED, DISPLAY LAST YEAR FISCAL
      *            PERIOD DATE RANGE
      * BLM 090120 IF GP-I-AM-WORLD, ENTER OPTION TO PRINT SPANISH GL
      *            DESCRIPTIONS, WORLD PR# 616
      * BLM 090219 ONLY ASK SPANISH DESCRIPTION QUESTION IF MEXICO,
      *            WORLD PR# 616
      * BLM 090325 IF Y FOR SPANISH-FG TO PRINT SPANISH DESCRIPTIONS, ALSO
      *            PRINT SPANISH HEADINGS, WORLD PR# 616
      * BLM 090510 ADD A FEW MORE TRANSLATIONS TO SPANISH HEADINGS,
      *            WORLD PR# 616
      *  CS 120113 EXPANDED TOTALS WORKERS & PRINT ITEMS TO PRINT BILLIONS
      *                                               WRLD PR#783
      * BAH 130415 REMOVED AN OOPS LINE THAT MOVED "Y" TO MEXICO-FG
      *                                           MULLEN PL# 796
      * KEC 2016.0104 FIXED KEYS & RECS FOR GT-REC.
      *  CS 2016.0712 FIXED BRANCH HEADING PRINTING A BRANCH NUMBER  
      *               ON REPORT-TOTAL PAGE, CHANGED SPANISH-HEAD-PAGE ALSO. #943
      * KEC 2016.1129 CHANGED GT-DATE1 GT-DATE2 GT-DATE3 AS ACTIVE CCYYMMDD.
      *               CHANGED THE USE OF DATE1 AND DATE2 TO BE
      *               BEG-DATE-MMDDYY AND END-DATE-MMDDYY; AS WELL AS ADDED
      *               BEG-DATE-CCYYMMDD AND END-DATE-CCYYMMDD FOR EASE.
      * BLM 2017.0315 CHANGED FOR 8 DIGIT DATES, REVISE FISCAL LOGIC, PD0006
      * BAH 2020.0827 REMOVED RESET ON FIRST FIELD
      * BAH 2022.0923 HARD CODED STARTS
      * MB  20240603  PERFORMANCE TUNING FOR GTFILE S35Q-55
      *               EMBEDDED GLFILE/GTFILE ACCESS (START/READ NEXT) DUE TO 
      *               POOR PERFORMING SQL IN GLGT3RN START.
      *               GLFILE CURSOR CONTAINS GLFILE AND GTFILE DATA
      *               CORRECTED ISSUE - PROGRAM NOT PRINTING PAGE HEADERS
      *                FOR EVERY BRANCH BREAK
      *******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "LIBGB/GBFSPR.CPY".
       DATA DIVISION.
       FILE SECTION.
       COPY "LIBGB/GBFDPR.CPY".
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)GL/DTLBAL.CBL S35 10/17/22-10:32:09 >".
      *****************************************************************
      *  FILE PATH DEFINITIONS
      *****************************************************************
       COPY "LIBGB/GB01GR.CPY".
       COPY "LIBGB/GB01GR_SQL.CPY".
       COPY "LIBGB/GBWSGR.CPY".
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBGL/GL01GT.CPY".
       COPY "LIBGL/GL01GT_SQL.CPY".
       COPY "LIBGL/GLWSGT.CPY".
       COPY "LIBGL/GL01GL.CPY".
       COPY "LIBGL/GL01GL_SQL.CPY".
       COPY "LIBGL/GLWSGL.CPY".
       01  PR-IFN                      PIC X(11) VALUE "0C PRINTER".
       01  LN-WORKERS.
           03  LN-FEED                 PIC 99        VALUE 0.
           03  LN-COUNT                PIC 99        VALUE 61.
           03  LN-WK                   PIC 99        VALUE 0.
       01  PAGE-NUMBER                 PIC 9(6)      VALUE 0.

       01  GL1-CURSOR-STAT-V1          PIC X(2) VALUE SPACES.
           88  GL1-CURSOR-STAT-V1-GOOD          VALUE "00".
           88  GL1-CURSOR-STAT-V1-BAD           VALUE "  " THRU "//"
                                                      "01" THRU "~~".

       01  HEADUP-GL-ACCT              PIC X(3)    VALUE "NO ".
       01  FIRST-TIME                  PIC X(1)    VALUE "Y".
       01  RECORDS-FOUND               PIC X       VALUE " ".
           88  NO-RECORDS-FOUND                    VALUE "N".
       01  DETAIL-EXISTS               PIC X       VALUE "Y".
      *----------------------------------------------------------------
      * DETAIL-PRINTED : SET TO "Y" IF DETAIL HAS BEEN PRINTED.
      *                  ADDED BECAUSE IF FIRST-TIME WAS "Y" THEN
      *                  TOTALS WERE BEING PRINTED IN END-ROUTINE EVEN
      *                  THOUGH NO RECORDS WERE PRINTED.
      *----------------------------------------------------------------
       01  DETAIL-PRINTED              PIC X       VALUE "N".

       01  HOLD-LOC                    PIC 9(4)    VALUE 0.
       01  WORK-GL1-KEY.
           03  WORK-GL-BRANCH          PIC 9(4).
           03  WORK-GL-ACCOUNT         PIC 9(7).

       01  SUB                         PIC 99      VALUE 0.
       01  LEVEL                       PIC 9       VALUE 0.
       01  NEXT-LEVEL                  PIC 9       VALUE 0.
       01  COUNTER                     PIC 99      VALUE 0.

       01  TOTALS-TABLE.
           03  TOT-TABLE         OCCURS 3 TIMES.
               05  TOTAL-PROMPT        PIC X(15).
               05  INITIAL-BAL         PIC S9(10)V99.
               05  TOTAL-DEBIT         PIC S9(10)V99.
               05  TOTAL-CREDIT        PIC S9(10)V99.
               05  FINAL-BAL           PIC S9(10)V99.
               05  DETAIL-COUNT        PIC S9(9)V99.

       COPY "LIBGB/EOBUF_EXT.CPY".
           03  BEG-DATE                PIC 9(8).
           03  FILLER REDEFINES BEG-DATE.
               05  FILLER              PIC 9(6).
               05  BEG-DD              PIC 9(2).
           03  END-DATE                PIC 9(8).
           03  CURRENT-PERIOD          PIC 99.
           03  REPORT-OPTION           PIC X.
           03  BEG-BRANCH              PIC 9(4).
           03  END-BRANCH              PIC 9(4).
           03  BEG-ACCOUNT             PIC 9(7).
           03  END-ACCOUNT             PIC 9(7).
           03  ZERO-BALS               PIC X.
           03  GROUP-FLAG              PIC X.
           03  ENT-GROUP               PIC X(4).

       01  SV-BEG-BR              PIC 9(4)      VALUE 9999.
       01  SV-END-BR              PIC 9(4)      VALUE 9999.
       01  SV-GROUP-FLAG       PIC X     VALUE SPACES.
       01  SV-ENT-GROUP        PIC X(4)  VALUE SPACES.

       01  VERSION-CONTROL             PIC X VALUE "B".

       01  MESS-LIST                   PIC X(5) VALUE "MESS.".

       01  HEAD1.
           03  HEAD1-DATE              PIC X(8)  VALUE SPACES.
           03  FILLER                  PIC X     VALUE SPACES.
           03  HEAD1-TIME              PIC X(8)  VALUE SPACES.
           03  FILLER                  PIC X(30) VALUE SPACES.
           03  HEAD1-CONAME            PIC X(40).
           03  FILLER                  PIC X(33) VALUE SPACES.
           03  HEAD1-PAGE              PIC X(7)  VALUE "PAGE".
           03  HEAD1-PAGENO            PIC ZZZZ9.

       01  HEAD2.
           03  FILLER                  PIC X(10) VALUE "USERID: ".
           03  HEAD2-USERID            PIC X(10) VALUE SPACES.
           03  FILLER                  PIC X(28) VALUE SPACES.
           03  FILLER                  PIC X(27) VALUE
               "DETAIL TRIAL BALANCE REPORT".
           03  FILLER                  PIC X(1)  VALUE SPACES.
           03  HEAD2-BRANCH            PIC 9999.
           03  FILLER                  PIC X(1)  VALUE SPACES.
           03  HEAD2-BRANCHX           PIC X(20).
           03  FILLER                  PIC X(31) VALUE SPACES.

       01  HEAD3.
           03  HEAD3-ACCT-PROMPT       PIC X(18) VALUE
               "ACCOUNT NO RANGE: ".
           03  HEAD3-GROUP-ID          PIC X(5)  VALUE SPACES.
           03  HEAD3-BR1               PIC 9(4).
           03  FILLER                  PIC X(1)  VALUE "-".
           03  HEAD3-ACCT1             PIC 9(7).
           03  FILLER                  PIC X(1)  VALUE " ".
           03  HEAD3-BR2               PIC 9(4).
           03  FILLER                  PIC X(1)  VALUE "-".
           03  HEAD3-ACCT2             PIC 9(7).
           03  FILLER                  PIC X(9)  VALUE SPACES.
           03  HEAD3-PER-PROMPT        PIC X(9)  VALUE "PERIOD:".
           03  HEAD3-PERIOD            PIC Z9.
           03  FILLER                  PIC X(3)  VALUE "  (".
           03  HEAD3-DATE1             PIC 99/99/99.
           03  FILLER                  PIC X(3)  VALUE " - ".
           03  HEAD3-DATE2             PIC 99/99/99.
           03  FILLER                  PIC X     VALUE ")".

       01  HEAD4.
           03  FILLER                  PIC X(13) VALUE "BRN ACCT-NO".
           03  FILLER                  PIC X(34) VALUE
               "ACCOUNT DESCRIPTION".
           03  FILLER                  PIC X(25) VALUE "TYPE".
           03  FILLER                  PIC X(19) VALUE "OPENING".
           03  FILLER                  PIC X(24) VALUE "TRANSACTIONS".
           03  FILLER                  PIC X(7)  VALUE "CLOSING".

       01  HEAD5.
           03  FILLER                  PIC X(13) VALUE SPACES.
           03  FILLER                  PIC X(12) VALUE "TRANSACTION ".
           03  FILLER                  PIC X(22) VALUE "DESCRIPTION".
           03  FILLER                  PIC X(25) VALUE
               "REFERENCE    SOURCE".
           03  FILLER                  PIC X(16) VALUE "BALANCE".
           03  FILLER                  PIC X(27) VALUE
               "DEBIT        CREDIT".
           03  FILLER                  PIC X(16) VALUE
               "BALANCE     DATE".

       01  UNDER-LINE.
           03  FILLER                  PIC X(47) VALUE SPACES.
           03  FILLER                  PIC X(45) VALUE
               "---------------------------------------------".
           03  FILLER                  PIC X(40) VALUE
               "----------------------------------------".

       01  DETAIL-LINE                 PIC X(132) VALUE SPACES.

       01  DETAIL-LINE1 REDEFINES DETAIL-LINE.
           03  D-BRANCH                PIC 9(4).
           03  D-DASH                  PIC X.
           03  D-ACCT                  PIC 9(7).
           03  FILLER                  PIC X(2).
           03  D-DESCRIPTION           PIC X(32).
           03  GLMASTER-DETAIL.
               05  D-TYPE              PIC X(20).
               05  D-INITIAL-BAL       PIC Z(9)9.99-.
               05  FILLER              PIC X.
           03  GLTRANS-DETAIL REDEFINES GLMASTER-DETAIL.
               05  D-REFERENCE         PIC X(16).
               05  D-SOURCE            PIC X(2).
               05  D-FLAG              PIC X(2).
               05  D-PERIOD            PIC 99.
               05  FILLER              PIC X(13).
           03  D-DEBIT                 PIC Z(10).ZZ.
           03  FILLER                  PIC X.
           03  D-CREDIT                PIC Z(10).ZZ.
           03  FILLER                  PIC X.
           03  D-FINAL-BAL             PIC Z(9)9.99-.
           03  FILLER                  PIC X(1).
           03  D-DATE                  PIC 99/99/99.


       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBWI/GRSCANW.CPY".
       COPY "LIBGB/GRWORKW.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBGB/GETPRW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/GPENVW.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBGL/DTLBAL_DEF.CPY".
       COPY "LIBGL/DTLBAL_WKS.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".
       COPY "LIBGB/GETFMW.CPY".

       SCREEN SECTION.
       COPY "LIBGL/DTLBAL_SCN.CPY".

       PROCEDURE DIVISION
           USING FORM-PATHNAME EXIT-PATHNAME.
       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON PR-FILE.
       COPY "LIBGB/DECLRP.CPY".
      *     PERFORM CLOSE-GT3-FILE.
           PERFORM CLOSE-GL1-FILE.
           PERFORM CLOSE-GR1-FILE.
           PERFORM CLOSE-BR-FILE.
           CLOSE PR-FILE.
       COPY "LIBGB/DECLRP2.CPY".

      ***********************************************
       MAIN-PROGRAM SECTION.
      ***********************************************

           PERFORM SQL-CONNECT.

      D    STOP MESS.
           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "DTLBAL" TO VDU-FORM-NAME.

           PERFORM GET-GPENV.

           PERFORM INITIALIZATION.

           IF GROUP-FLAG = "N"
              GO TO NEXT-GL-MASTER.

           PERFORM OPEN-GR1-FILE.
           MOVE ENT-GROUP TO GR-ID.
           PERFORM SEARCH-GR-INIT.

       GROUP-NEXT-LOC.
           PERFORM SEARCH-GR-NEXT.
           IF WGR-SRESULT = 0
              GO TO END-ROUTINE.

      * THIS START IS FOR A GROUP ONLY, BRANCH RANGE START IS IN INIT
           MOVE WGR-SRESULT TO GL-BRANCH
                               QGL1-WBEG-BRANCH
                               QGL1-WEND-BRANCH.
           MOVE BEG-ACCOUNT TO GL-ACCOUNT
                               QGL1-WBEG-ACCOUNT.
           MOVE END-ACCOUNT TO QGL1-WEND-ACCOUNT.

      *     PERFORM START-GL1-FILE.

       NEXT-GL-MASTER.
           MOVE BEG-DATE      TO GT-DATE3 SQL-DATE-YYYYMMDD.
           IF BEG-DATE = 0
              MOVE 19000101   TO GT-DATE3 SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO QGT3-WBEG-DATE3.
           MOVE END-DATE      TO SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO QGT3-WEND-DATE3.

           IF EXT-ROUTE-BUF = "00"
              MOVE SPACE TO MESS
              STRING "REPORT IN PROGRESS.....", 
                        QGL1-WBEG-BRANCH INTO MESS
              MOVE MESS-LIST TO WR-LIST
              MOVE MESS TO WR-BUF
              PERFORM CALL-WRFORM
           END-IF.

           PERFORM START-GL1-FILE-V1.  *> EMBEDDED GL GT

           MOVE 0 TO WORK-GL1-KEY.

       NEXT-GL-MASTER-READ.
           PERFORM READ-GL1-FILE-NEXT-V1.  *> EMBEDDED GL GT

      * MUST PRINT ACCOUNT TOTALS AT END OF CURSOR (IF CURSOR HAD ROWS (WORK-GL1-KEY > 0) )

           IF GROUP-FLAG = "Y"
              IF (IO-BAD)            *> OR (GL-BRANCH NOT = WGR-SRESULT)
                 PERFORM CLOSE-GL1-FILE-V1
                 IF WORK-GL1-KEY > 0
                    PERFORM PRINT-ACCOUNT-TOTALS
                    PERFORM PRINT-BRANCH-TOTALS
                 END-IF
                 GO TO GROUP-NEXT-LOC.

      * FOR NON-GROUP REQUESTS END-ROUTINE PERFORMS BRANCH-BREAK AT END OF CURSOR
           IF GROUP-FLAG = "N"
              IF (IO-BAD)            *> OR (GL-BRANCH > END-BRANCH)
                 PERFORM CLOSE-GL1-FILE-V1
                 IF WORK-GL1-KEY > 0
                    PERFORM PRINT-ACCOUNT-TOTALS
                 END-IF
                 GO TO END-ROUTINE.

      *THIS CHECKED IN CURSOR
      *     IF GL-ACCOUNT < BEG-ACCOUNT OR GL-ACCOUNT > END-ACCOUNT
      *        GO TO NEXT-GL-MASTER.

      * GO TO READ-GLTRANS IF SAME BRANCH/ACCOUNT AS PREV
      * PRINT ACCOUNT TOTALS ON BRANCH/ACCOUNT BREAKS
           IF GL-BR-ACCOUNT = WORK-GL1-KEY
              GO TO READ-GLTRANS
           ELSE
              IF WORK-GL1-KEY > 0
                 PERFORM PRINT-ACCOUNT-TOTALS
              END-IF
              MOVE GL-BR-ACCOUNT TO WORK-GL1-KEY.

           MOVE "YES" TO HEADUP-GL-ACCT.

           IF FIRST-TIME = "Y"
              MOVE "N" TO FIRST-TIME
              MOVE GL-BRANCH TO HOLD-LOC.

      * TEST  BRANCH  BREAK
           IF GL-BRANCH NOT = HOLD-LOC
      *       GROUP REQUESTS BRANCH BREAK HANDLED AT END OF CURSOR
              IF GROUP-FLAG = "N"
                 PERFORM PRINT-BRANCH-TOTALS
              END-IF
              MOVE GL-BRANCH TO HOLD-LOC
              IF EXT-ROUTE-BUF = "00"
                 MOVE SPACE TO MESS
                 STRING "REPORT IN PROGRESS.....", GL-BRANCH INTO MESS
                 MOVE MESS-LIST TO WR-LIST
                 MOVE MESS TO WR-BUF
                 PERFORM CALL-WRFORM
              END-IF.
      *       MOVE EXT-CONAME-RPT-LINES TO LN-COUNT.  THIS MOVED TO PRINT-BRANCH-TOTALS MB 2024-06-03

      *    MOVE GL-BR-ACCOUNT TO WORK-GL1-KEY.
      *    MOVE GL-ACCOUNT    TO GT-ACCTNO
      *                          QGT3-WBEG-ACCTNO
      *                          QGT3-WEND-ACCTNO.
      *    MOVE GL-BRANCH     TO GT-BRANCH
      *                          QGT3-WBEG-BRANCH
      *                          QGT3-WEND-BRANCH.
      *    MOVE BEG-DATE      TO GT-DATE3 SQL-DATE-YYYYMMDD.
      *    IF BEG-DATE = 0
      *       MOVE 19000101   TO GT-DATE3 SQL-DATE-YYYYMMDD.
      *    PERFORM SQL-SET-DATE.
      *    MOVE SQL-DATE-YYYY-MM-DD TO QGT3-WBEG-DATE3.
      *    MOVE END-DATE      TO SQL-DATE-YYYYMMDD.
      *    PERFORM SQL-SET-DATE.
      *    MOVE SQL-DATE-YYYY-MM-DD TO QGT3-WEND-DATE3.
      *    MOVE SPACES        TO GT-DRCRFG
      *                          QGT3-WBEG-DRCRFG.
      *    MOVE ALL "Z"       TO QGT3-WEND-DRCRFG.
      *    MOVE 0             TO GT-LINE
      *                          QGT3-WBEG-LINE.
      *    MOVE ALL "9"       TO QGT3-WEND-LINE.
      * HAVE TO SET UP SOME INTERNAL WORKERS FOR NEW START ROUTINES
      *    COMPUTE QGT3-WSBEG-BRANCH = GT-BRANCH + 1.
      *    COMPUTE QGT3-WSBEG-ACCTNO = GT-ACCTNO + 1.
      *    MOVE GT-DATE3             TO NDTE-DATE.
      *    MOVE 1                    TO NDTE-HOLD.
      *    PERFORM INCREMENT-DAYS.
      *    MOVE NDTE-DATE            TO SQL-DATE-YYYYMMDD.
      *    PERFORM SQL-SET-DATE.
      *    MOVE SQL-DATE-YYYY-MM-DD  TO QGT3-WSBEG-DATE3.
      *
      *    MOVE 01                   TO SQL-TEXT-SIZE.
      *    MOVE GT-DRCRFG            TO SQL-TEXT.
      *    PERFORM SQL-TEXT-INCREMENT.
      *    MOVE SQL-TEXT             TO QGT3-WSBEG-DRCRFG.
      *
      *     PERFORM START-GT3-FILE.

      * GT DATA INCLUDED IN GL CURSOR
       READ-GLTRANS.
      *     PERFORM READ-GT3-FILE-NEXT.
      *     IF IO-BAD
      *        PERFORM PRINT-ACCOUNT-TOTALS
      *        GO TO NEXT-GL-MASTER.

      *** GT-BRANCH = 0 WHEN GTFILE ROW(S) NOT FOUND FOR GL BRANCH/ACCOUNT
           IF GT-BRANCH = 0
              PERFORM PRINT-ACCOUNT-TOTALS
              GO TO NEXT-GL-MASTER-READ.

      *     IF GL-BR-ACCOUNT NOT = WORK-GL1-KEY
      *        PERFORM PRINT-ACCOUNT-TOTALS
      *        MOVE GL-BR-ACCOUNT TO WORK-GL1-KEY
      *        GO TO NEXT-GL-MASTER.

      *THIS CHECKED IN CURSOR
      *     IF GT-DATE1 < BEG-DATE OR GT-DATE1 > END-DATE
      *        GO TO READ-GLTRANS.

           MOVE "Y" TO DETAIL-EXISTS.

           ADD 1 TO DETAIL-COUNT(1).

           PERFORM PRINT-GLTRANS-INFO.

      *     GO TO READ-GLTRANS.
           GO TO NEXT-GL-MASTER-READ.

       END-ROUTINE.

      * PRINT REPORT TOTAL ON NEW PAGE TO MATCH DETBAL.C

           IF FIRST-TIME = "Y" OR DETAIL-PRINTED = "N"
              MOVE "N" TO RECORDS-FOUND
           ELSE
              PERFORM PRINT-BRANCH-TOTALS
              MOVE EXT-CONAME-RPT-LINES TO LN-COUNT.
              MOVE 3 TO LEVEL
              MOVE "REPORT TOTAL  :" TO TOTAL-PROMPT(LEVEL)
              PERFORM TOTAL-PRINT-ROUTINE
              PERFORM WRITE-DETAIL-LINE
              MOVE " " TO EXT-EOBUF-ERRCD.

           GO TO END-PROGRAM.

      *****************************************************************
       INITIALIZATION SECTION.
      *****************************************************************
           MOVE DTLBAL-QUEUE-POS TO Q-POS.
           MOVE DTLBAL-COPIES-POS TO C-POS.
           MOVE DTLBAL-PRU-POS   TO P-POS.
           MOVE SPACES TO Q-BUF C-BUF P-BUF.
       COPY "LIBGB/EOINIT.CPY".

           IF EXT-ROUTE-BUF = "00"
              MOVE "REPORT IN PROGRESS ..." TO MESS
              MOVE MESS-LIST TO WR-LIST
              MOVE MESS TO WR-BUF
              PERFORM CALL-WRFORM.

           MOVE BEG-DATE    TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO HEAD3-DATE1.
           MOVE END-DATE    TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO HEAD3-DATE2.
      
           MOVE "N" TO DETAIL-PRINTED.

       ZERO-OUT-TOTAL-TABLE.
           PERFORM INITIALIZE-TOTALS VARYING LEVEL
              FROM 1 BY 1 UNTIL LEVEL > 3.

           IF EXT-EOBUF-GROUP-FG = "Y"
              MOVE BEG-BRANCH TO SV-BEG-BR
              MOVE END-BRANCH TO SV-END-BR
              MOVE GROUP-FLAG TO SV-GROUP-FLAG
              MOVE ENT-GROUP TO SV-ENT-GROUP
              MOVE EXT-EOBUF-BRNO TO BEG-BRANCH END-BRANCH
              MOVE "N" TO GROUP-FLAG
              MOVE EXT-EOBUF-GROUP-PRPATH TO PRU-FILE
           ELSE
              IF ( BEG-BRANCH = 0 AND END-BRANCH = 0 )
                 MOVE BEG-BRANCH TO SV-BEG-BR
                 MOVE END-BRANCH TO SV-END-BR
                 MOVE EXT-CONAME-USER-LOC TO BEG-BRANCH END-BRANCH.

      *     PERFORM OPEN-GT3-FILE.
           PERFORM OPEN-GL1-FILE.

           MOVE BEG-BRANCH  TO GL-BRANCH
                               QGL1-WBEG-BRANCH.
           MOVE END-BRANCH  TO QGL1-WEND-BRANCH.
           MOVE BEG-ACCOUNT TO GL-ACCOUNT
                               QGL1-WBEG-ACCOUNT.
           MOVE END-ACCOUNT TO QGL1-WEND-ACCOUNT.
      *     PERFORM START-GL1-FILE.

           PERFORM OPEN-PR-FILE.

      *****************************************************************
       EO-EXEC-AUDIT SECTION.
      *****************************************************************
           MOVE EXT-EOBUF-DATE1 TO BEG-DATE.
           MOVE EXT-EOBUF-DATE2 TO END-DATE.

           IF EXT-EOBUF-DATE2 < GP-BEG-FISCAL-DATE
              OR EXT-EOBUF-DATE2 > GP-END-FISCAL-DATE(12)
                 MOVE "UNABLE TO DETERMINE FISCAL PERIOD!" TO MESS
                 PERFORM SEND-MESS
                 GO TO END-PROGRAM.
           MOVE 0 TO CURRENT-PERIOD.
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 12
              IF CURRENT-PERIOD = 0
                 IF EXT-EOBUF-DATE2 <= GP-END-FISCAL-DATE(SUB)
                    MOVE SUB TO CURRENT-PERIOD
                 END-IF
              END-IF
           END-PERFORM.

      *****************************************************************
       ENTER-PARAMETERS SECTION.
      *****************************************************************
       ENTER-PARM.

       OPTION-01.
           MOVE "E  A010RPTOPT." TO VDU.
           PERFORM SCREENIO.
           IF F1-KEY GO TO OPTION-01.
           IF F7-KEY GO TO EXIT-PARM.
           IF NOT ENTER-KEY GO TO OPTION-01.
           MOVE VDUBUF TO REPORT-OPTION.
           IF REPORT-OPTION = "C" GO TO PERIOD-01.
           IF REPORT-OPTION = "P" GO TO PERIOD-01.
           GO TO OPTION-01.
       PERIOD-01.
           IF EXT-ROUTE-BUF = "40" OR "50"
              GO TO BR01.
           MOVE "ENNN020PERIOD." TO VDU.
           PERFORM SCREENIO.
           IF F1-KEY GO TO OPTION-01.
           IF UP-KEY GO TO OPTION-01.
           IF F7-KEY GO TO EXIT-PARM.
           IF NOT ENTER-KEY GO TO PERIOD-01.
           MOVE VDUNUM0 TO CURRENT-PERIOD.
           IF CURRENT-PERIOD < 1 OR CURRENT-PERIOD > 12
              GO TO PERIOD-01.
           MOVE GP-END-FISCAL-DATE(CURRENT-PERIOD) TO END-DATE.
           IF CURRENT-PERIOD = 1
              MOVE GP-BEG-FISCAL-DATE TO BEG-DATE
           ELSE
              MOVE GP-END-FISCAL-DATE(CURRENT-PERIOD) TO BEG-DATE
              MOVE 1 TO BEG-DD.
      *----------------------------------------------------------------
      *    SETUP LAST YEAR DATE IF PREVIOUS PERIOD:
      *----------------------------------------------------------------
       SETUP-LAST-YEAR.
           IF REPORT-OPTION = "P"
              MOVE BEG-DATE                   TO NDTE-DATE
              MOVE -1                         TO NDTE-HOLD
              PERFORM INCREMENT-YEARS
              MOVE NDTE-DATE                  TO BEG-DATE
              MOVE END-DATE                   TO NDTE-DATE
              MOVE -1                         TO NDTE-HOLD
              PERFORM INCREMENT-YEARS
              MOVE NDTE-DATE                  TO END-DATE.
      *----------------------------------------------------------------
       DISPLAY-DATES.
           MOVE "S  8000BEGDATE." TO VDU.
           MOVE BEG-DATE TO VDU-DATE-YYYYMMDD.
           PERFORM SCREENIO.
           MOVE "S  8000ENDDATE." TO VDU.
           MOVE END-DATE TO VDU-DATE-YYYYMMDD.
           PERFORM SCREENIO.
           GO TO BR01.
       DATE-01.
           IF EXT-ROUTE-BUF = "40" OR "50"
              GO TO BR01.
           MOVE "ERNM060BEGDATE." TO VDU.
           PERFORM SCREENIO.
           IF F1-KEY GO TO OPTION-01.
           IF UP-KEY GO TO PERIOD-01.
           IF F7-KEY GO TO EXIT-PARM.
           IF NOT ENTER-KEY GO TO DATE-01.
           IF VDUNUM0 = 0
              MOVE "S  A000BEGDATE." TO VDU
              MOVE BEG-DATE TO DATE-YYYYMMDD
              PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
              MOVE DATE-MMDDYY TO VDU-DATE-EDIT
              PERFORM SCREENIO
              MOVE BEG-DATE TO VDU-DATE-YYYYMMDD.
           MOVE VDU-DATE-YYYYMMDD TO BEG-DATE.
       DATE-02.
           IF EXT-ROUTE-BUF = "40" OR "50"
              GO TO BR01.
           MOVE "ERNM060ENDDATE." TO VDU.
           PERFORM SCREENIO.
           IF F1-KEY GO TO OPTION-01.
           IF UP-KEY GO TO DATE-01.
           IF F7-KEY GO TO EXIT-PARM.
           IF NOT ENTER-KEY GO TO DATE-02.
           IF VDUNUM0 = 0
              MOVE "S  A000ENDDATE." TO VDU
              MOVE END-DATE TO DATE-YYYYMMDD
              PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
              MOVE DATE-MMDDYY TO VDU-DATE-EDIT
              PERFORM SCREENIO
              MOVE END-DATE TO VDU-DATE-YYYYMMDD.
           MOVE VDU-DATE-YYYYMMDD TO END-DATE.
           IF BEG-DATE > END-DATE
              MOVE "INVALID DATE RANGE" TO MESS
              PERFORM SEND-MESS
              GO TO DATE-01.
       BR01.
           MOVE "E  A000BR1." TO VDU.
           PERFORM SCREENIO.
           IF F1-KEY GO TO OPTION-01.
           IF UP-KEY GO TO DATE-02.
           IF F3-KEY GO TO ALL-BR.
           IF F7-KEY GO TO EXIT-PARM.

           IF F6-KEY
              MOVE "WI/GRSCAN" TO FORM-NAM
              CALL FORM-PROGX USING FORM-PATH GRSCAN-BUF EXIT-PATHNAME
              CANCEL FORM-PROGX
              IF GRSCAN-STATUS NOT = "00"
                 GO TO BR01
              ELSE
                 MOVE "S  A000BR1." TO VDU
                 MOVE GRSCAN-KEY TO VDUBUF
                 PERFORM SCREENIO
                 MOVE GRSCAN-KEY TO VDUBUF
                 MOVE "00" TO VDUSTATUS.

           IF NOT ENTER-KEY GO TO BR01.

           MOVE VDUBUF TO WGR-WORD ENT-GROUP.
           IF WGR-WORD = SPACES
              GO TO BR01.
           MOVE 0 TO WGR-MIN-LEVEL.
           PERFORM CHECK-MEMBER.
           IF WGR-ERR > 0
              MOVE WGR-MSG(WGR-ERR) TO MESS
              PERFORM SEND-MESS
              GO TO BR01.
           IF WGR-TYPE = "A"
              MOVE "S  A000BR2." TO VDU
              MOVE SPACES TO VDUBUF
              PERFORM SCREENIO
              MOVE "Y" TO GROUP-FLAG
              MOVE 0 TO BEG-BRANCH
              MOVE 9999 TO END-BRANCH
              GO TO ACCT-01.
           MOVE "N" TO GROUP-FLAG.
           MOVE WGR-SRESULT TO BEG-BRANCH.

           MOVE "SN N040BR1." TO VDU.
           MOVE BEG-BRANCH TO VDUNUM0.
           PERFORM SCREENIO.
       BR02.
           MOVE "ENNN040BR2." TO VDU.
           PERFORM SCREENIO.
           IF F1-KEY GO TO OPTION-01.
           IF UP-KEY GO TO BR01.
           IF F3-KEY GO TO ALL-BR.
           IF F7-KEY GO TO EXIT-PARM.
           IF NOT ENTER-KEY GO TO BR02.
           MOVE VDUNUM0 TO END-BRANCH.
           IF BEG-BRANCH > END-BRANCH
              GO TO BR01.
           GO TO ACCT-01.
       ALL-BR.
           MOVE "N" TO GROUP-FLAG.
           MOVE "SN N040BR1." TO VDU.
           MOVE 0 TO BEG-BRANCH VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SN N040BR2." TO VDU.
           MOVE 9999 TO END-BRANCH VDUNUM0.
           PERFORM SCREENIO.
       ACCT-01.
           MOVE "ENNN070ACCT1." TO VDU.
           PERFORM SCREENIO.
           IF F1-KEY GO TO OPTION-01.
           IF UP-KEY GO TO BR02.
           IF F3-KEY GO TO ALL-ACCTS.
           IF F7-KEY GO TO EXIT-PARM.
           IF NOT ENTER-KEY GO TO ACCT-01.
           MOVE VDUNUM0 TO BEG-ACCOUNT.
       ACCT-02.
           MOVE "ENNN070ACCT2." TO VDU.
           PERFORM SCREENIO.
           IF F1-KEY GO TO OPTION-01.
           IF UP-KEY GO TO ACCT-01.
           IF F7-KEY GO TO EXIT-PARM.
           IF NOT ENTER-KEY GO TO ACCT-02.
           MOVE VDUNUM0 TO END-ACCOUNT.
           IF BEG-ACCOUNT > END-ACCOUNT
              GO TO ACCT-01.
           GO TO PRINT-ZERO-BALANCES.
       ALL-ACCTS.
           MOVE "SN N070ACCT1." TO VDU.
           MOVE 0 TO BEG-ACCOUNT VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SN N070ACCT2." TO VDU.
           MOVE 9999999 TO END-ACCOUNT VDUNUM0.
           PERFORM SCREENIO.

       PRINT-ZERO-BALANCES.
           MOVE "E  A010ZEROBAL." TO VDU.
           PERFORM SCREENIO.
           IF F1-KEY GO TO OPTION-01.
           IF UP-KEY GO TO ACCT-02.
           IF F7-KEY GO TO EXIT-PARM.
           IF NOT ENTER-KEY GO TO PRINT-ZERO-BALANCES.
           MOVE VDUBUF TO ZERO-BALS.
           IF ZERO-BALS NOT = "Y"
              IF ZERO-BALS NOT = "N"
                 GO TO PRINT-ZERO-BALANCES.

       EXIT-PARM.
           EXIT.

      *****************************************************************
       DISPLAY-PARAMETERS SECTION.
      *****************************************************************
           IF GROUP-FLAG = "Y"
              MOVE "S  A040BR1." TO VDU
              MOVE ENT-GROUP TO VDUBUF
              PERFORM SCREENIO
           ELSE
              MOVE "SNNN040BR1." TO VDU
              MOVE BEG-BRANCH TO VDUNUM0
              PERFORM SCREENIO
              MOVE "SNNN040BR2." TO VDU
              MOVE END-BRANCH TO VDUNUM0
              PERFORM SCREENIO.
      *----------------------------------------------------------------
           MOVE "SNNN070ACCT1." TO VDU.
           MOVE BEG-ACCOUNT TO VDUNUM0.
           PERFORM SCREENIO.
      *----------------------------------------------------------------
           MOVE "SNNN070ACCT2." TO VDU.
           MOVE END-ACCOUNT TO VDUNUM0.
           PERFORM SCREENIO.
      *----------------------------------------------------------------
           MOVE "S  A010ZEROBAL." TO VDU.
           MOVE ZERO-BALS TO VDUBUF.
           PERFORM SCREENIO.

      *****************************************************************
       INITIALIZE-TOTALS SECTION.
      *****************************************************************
           MOVE 0 TO INITIAL-BAL(LEVEL) TOTAL-CREDIT(LEVEL)
                     TOTAL-DEBIT(LEVEL) FINAL-BAL(LEVEL)
                     DETAIL-COUNT(LEVEL).

      *****************************************************************
       SUMMING-UP SECTION.
      *****************************************************************
           IF REPORT-OPTION = "C"
              IF GL-TYPE = "C"
                  SUBTRACT GL-CYBAL(COUNTER) FROM INITIAL-BAL(1)
              ELSE
                  ADD GL-CYBAL(COUNTER) TO INITIAL-BAL(1)
           ELSE
              IF GL-TYPE = "C"
                  SUBTRACT GL-LYBAL(COUNTER) FROM INITIAL-BAL(1)
              ELSE
                  ADD GL-LYBAL(COUNTER) TO INITIAL-BAL(1).
           ADD 1 TO COUNTER.

      *****************************************************************
       WRITE-DETAIL-LINE SECTION.
      *****************************************************************
           ADD LN-FEED LN-COUNT GIVING LN-WK.
           IF LN-WK GREATER THAN EXT-CONAME-RPT-LINES
              PERFORM HEAD-PAGE.
           MOVE DETAIL-LINE TO PR-REC.
           PERFORM WRITE-PR-REC.
           ADD LN-FEED TO LN-COUNT.
           MOVE 1 TO LN-FEED.
           MOVE SPACES TO DETAIL-LINE.

      *****************************************************************
       HEAD-PAGE SECTION.
      *****************************************************************
           ADD 1 TO PAGE-NUMBER.
           MOVE PAGE-NUMBER        TO HEAD1-PAGENO.
           PERFORM GET-TIME.
           MOVE TIME-EDIT          TO HEAD1-TIME.
           MOVE EXT-CONAME-SYSDATE TO HEAD1-DATE.
           MOVE EXT-CONAME-CONAME  TO HEAD1-CONAME.
           MOVE HEAD1              TO PR-REC.
           MOVE 99 TO LN-FEED.
           PERFORM WRITE-PR-REC.
      *----------------------------------------------------------------
           MOVE EXT-CONAME-USER-ID TO HEAD2-USERID.
           IF LEVEL NOT = 3
              PERFORM OPEN-BR-FILE 
              MOVE HOLD-LOC TO BR-NO
              PERFORM READ-BR-FILE 
              IF IO-BAD
                 MOVE SPACES TO BR-REC 
              END-IF
              MOVE HOLD-LOC TO HEAD2-BRANCH
              MOVE BR-NAME  TO HEAD2-BRANCHX
           ELSE
              MOVE SPACES TO HEAD2-BRANCH
                             HEAD2-BRANCHX.
           MOVE HEAD2 TO PR-REC.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-PR-REC.
      *----------------------------------------------------------------
           IF GROUP-FLAG = "Y"
              MOVE ENT-GROUP TO HEAD3-GROUP-ID.
           MOVE BEG-BRANCH TO HEAD3-BR1.
           MOVE END-BRANCH TO HEAD3-BR2.
           MOVE BEG-ACCOUNT TO HEAD3-ACCT1.
           MOVE END-ACCOUNT TO HEAD3-ACCT2.
           MOVE CURRENT-PERIOD   TO HEAD3-PERIOD.
           MOVE HEAD3 TO PR-REC.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-PR-REC.
      *----------------------------------------------------------------
           MOVE HEAD4 TO PR-REC.
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-PR-REC.
      *----------------------------------------------------------------
           MOVE HEAD5 TO PR-REC.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-PR-REC.
      *----------------------------------------------------------------
           MOVE 2 TO LN-FEED.
       HEADING-ROUTINE-EXIT.
           MOVE 2 TO LN-FEED.
           MOVE 5 TO LN-COUNT.

      *****************************************************************
       PRINT-GLMASTER-INFO SECTION.
      *****************************************************************
           IF HEADUP-GL-ACCT = "NO "
              GO TO EXIT-PRINT-GLMASTER.

           MOVE GL-BRANCH      TO D-BRANCH.
           MOVE "-"            TO D-DASH.
           MOVE GL-ACCOUNT     TO D-ACCT.
           MOVE GL-DESCRIPTION TO D-DESCRIPTION.
           MOVE GL-TYPE        TO D-TYPE.
           MOVE 1              TO COUNTER.
           MOVE 0              TO INITIAL-BAL(1).

      * CALCULATE OPENING AMOUNT
           PERFORM SUMMING-UP UNTIL COUNTER = CURRENT-PERIOD.
           IF REPORT-OPTION = "C"
              IF GL-TYPE = "C"
                 SUBTRACT GL-OPENBAL FROM INITIAL-BAL(1)
              ELSE
                  ADD GL-OPENBAL TO INITIAL-BAL(1)
           ELSE
              IF GL-TYPE = "C"
                 SUBTRACT GL-LYOPENBAL FROM INITIAL-BAL(1)
              ELSE
                  ADD GL-LYOPENBAL TO INITIAL-BAL(1).
           IF ZERO-BALS = "N"
              IF INITIAL-BAL(1) = 0
                 IF DETAIL-EXISTS = "N"
                    GO TO EXIT-PRINT-GLMASTER.

           MOVE "Y" TO DETAIL-PRINTED.
           MOVE INITIAL-BAL(1) TO D-INITIAL-BAL.
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.
       EXIT-PRINT-GLMASTER.
           MOVE "NO " TO HEADUP-GL-ACCT.

      *****************************************************************
       PRINT-GLTRANS-INFO SECTION.
      *****************************************************************

           PERFORM PRINT-GLMASTER-INFO.

           MOVE GT-REF    TO D-REFERENCE.
           MOVE GT-SOURCE TO D-SOURCE.
           MOVE GT-FLAG   TO D-FLAG.

           IF GT-FLAG NOT = SPACES
              MOVE GT-PERIOD TO D-PERIOD.

           MOVE GT-EXPL     TO D-DESCRIPTION.

           MOVE GT-DATE1    TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO D-DATE.

           IF GT-AMT > 0
               GO TO CALC-POSITIVE.

           MULTIPLY -1 BY GT-AMT.
           MOVE GT-AMT TO D-CREDIT.
           MOVE 0        TO D-DEBIT.
           ADD GT-AMT  TO TOTAL-CREDIT(1).
           GO TO PRINT-IT.

       CALC-POSITIVE.
           MOVE GT-AMT TO D-DEBIT.
           MOVE 0        TO D-CREDIT.
           ADD GT-AMT  TO TOTAL-DEBIT(1).
       PRINT-IT.
           MOVE "Y" TO DETAIL-PRINTED.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      *****************************************************************
       PRINT-ACCOUNT-TOTALS SECTION.
      *****************************************************************
           IF HEADUP-GL-ACCT = "YES"
              MOVE "N" TO DETAIL-EXISTS
           ELSE
              MOVE "Y" TO DETAIL-EXISTS.
           PERFORM PRINT-GLMASTER-INFO.
           IF ZERO-BALS = "N"
              IF INITIAL-BAL(1) = 0
                 IF DETAIL-EXISTS = "N"
                    GO TO EXIT-ACCOUNT-TOTALS.
           MOVE 1 TO LEVEL.
           COMPUTE FINAL-BAL(LEVEL) = INITIAL-BAL(LEVEL)
              + TOTAL-DEBIT(LEVEL) - TOTAL-CREDIT(LEVEL).
           MOVE "ACCOUNT TOTAL :" TO TOTAL-PROMPT(LEVEL).
           PERFORM TOTAL-PRINT-ROUTINE.
       EXIT-ACCOUNT-TOTALS.
           EXIT.

      *****************************************************************
       PRINT-BRANCH-TOTALS SECTION.
      *****************************************************************
           MOVE SPACES TO DETAIL-LINE.
           MOVE 2 TO LEVEL.
           MOVE "BRANCH TOTAL  :" TO TOTAL-PROMPT(LEVEL).
           PERFORM TOTAL-PRINT-ROUTINE.
      *     MOVE GL-BRANCH  TO HOLD-LOC.
      *     MOVE GL-BRANCH  TO WORK-GL-BRANCH.
      *     MOVE GL-ACCOUNT TO WORK-GL-ACCOUNT.

      * ADDED TO MAKE SURE HEADINGS OCCUR AFTER EVERY BRANCH BREAK.  MB 2024-06-03
           MOVE EXT-CONAME-RPT-LINES TO LN-COUNT.

       EXIT-RTOTAL.
           EXIT.

      *****************************************************************
      *           TOTALS PRINT ROUTINE:
      *           LEVEL OF 1 = ACCOUNT TOTALS
      *           LEVEL OF 2 = BRANCH TOTALS
      *           LEVEL OF 3 = REPORT TOTALS
      *****************************************************************
       TOTAL-PRINT-ROUTINE SECTION.
           IF ZERO-BALS = "N"
              IF LEVEL < 3
                 IF INITIAL-BAL(LEVEL) =  0 AND
                    TOTAL-DEBIT(LEVEL) =  0 AND
                    TOTAL-CREDIT(LEVEL) = 0 AND
                    FINAL-BAL(LEVEL) =    0 AND
                    DETAIL-COUNT(LEVEL) = 0
                       GO TO EXIT-TOTAL-PRINT.

           IF LEVEL > 1
              MOVE SPACES TO DETAIL-LINE.

           MOVE TOTAL-PROMPT(LEVEL) TO D-DESCRIPTION.
           MOVE INITIAL-BAL(LEVEL)  TO D-INITIAL-BAL.
           MOVE TOTAL-DEBIT(LEVEL)  TO D-DEBIT.
           MOVE TOTAL-CREDIT(LEVEL) TO D-CREDIT.
           MOVE FINAL-BAL(LEVEL)    TO D-FINAL-BAL.
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           IF LEVEL = 1
              MOVE UNDER-LINE TO PR-REC
              MOVE 1 TO LN-FEED
              PERFORM WRITE-PR-REC
              ADD LN-FEED TO LN-COUNT.

           IF LEVEL < 3
              ADD 1 LEVEL GIVING NEXT-LEVEL
              ADD INITIAL-BAL(LEVEL)  TO INITIAL-BAL(NEXT-LEVEL)
              ADD TOTAL-DEBIT(LEVEL)  TO TOTAL-DEBIT(NEXT-LEVEL)
              ADD TOTAL-CREDIT(LEVEL) TO TOTAL-CREDIT(NEXT-LEVEL)
              ADD FINAL-BAL(LEVEL)    TO FINAL-BAL(NEXT-LEVEL)
              ADD DETAIL-COUNT(LEVEL) TO DETAIL-COUNT(NEXT-LEVEL).

           PERFORM INITIALIZE-TOTALS.

       EXIT-TOTAL-PRINT.
           EXIT.

      *********************************************
       FORM-RESET SECTION.
      *********************************************
           MOVE EXT-CONAME-CONAME TO DTLBAL-CONAME.
           MOVE EXT-CONAME-SYSDATE TO DTLBAL-SYSDATE.
           DISPLAY DTLBAL-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE DTLBAL-FORM-FIELDS TO WR-BUF.
           MOVE DTLBAL-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      *********************************************
       SETUP-LINK-INFO SECTION.
      *********************************************
           MOVE DTLBAL-HELP--FILE TO HELP-LINK-FILE.
           MOVE DTLBAL-HELP--DIR  TO HELP-LINK-DIR.

      *********************************************
       VDU-CONVERT-FIELD SECTION.
      *********************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBGL/DTLBAL_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.


       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/GRWORK.CPY".
       COPY "LIBGB/GRSERCH.CPY".
       COPY "LIBGB/TIMEIO.CPY".
       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/GPENV.CPY".

      *******************************************
       MISC SECTION.
      *******************************************
       COPY "LIBGB/GETPR.CPY".
       COPY "LIBGB/ACCESS.CPY".

      *******************************************
       IO-ROUTINES SECTION.
      *******************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGL/GLGTGS_SQL.CPY".
       COPY "LIBGL/GLGLGS_SQL.CPY".
       COPY "LIBGB/GBGRGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBGB/GBGR1RN.CPY".

       COPY "LIBGL/GLGL1RN.CPY".
      *-----------------------------------------------------------------
       START-GL1-FILE-V1.    *> EMBEDDED GL GT
           PERFORM START-IT.
           MOVE GL-PATH-SQL TO E-FILE.
           MOVE GL1-KEY     TO E-KEYX.
           IF ( GL1-CURSOR-STAT-V1-GOOD )
              PERFORM CLOSE-GL1-FILE-V1.

           EXEC SQL
            DECLARE GL1_CURSOR_V1 CURSOR FOR
            SELECT GLFILE.GL_BR_ACCOUNT,
                   GLFILE.GL_DESCRIPTION,
                   GLFILE.GL_OPENBAL,
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
                   GLFILE.GL_LYBAL_1,
                   GLFILE.GL_LYBAL_2,
                   GLFILE.GL_LYBAL_3,
                   GLFILE.GL_LYBAL_4,
                   GLFILE.GL_LYBAL_5,
                   GLFILE.GL_LYBAL_6,
                   GLFILE.GL_LYBAL_7,
                   GLFILE.GL_LYBAL_8,
                   GLFILE.GL_LYBAL_9,
                   GLFILE.GL_LYBAL_10,
                   GLFILE.GL_LYBAL_11,
                   GLFILE.GL_LYBAL_12,
                   GLFILE.GL_TYPE,
                   GLFILE.GL_LYOPENBAL,
                   GTFILE.GT_SOURCE,
                   ISNULL(GTFILE.GT_BRANCH,0),
                   GTFILE.GT_ACCTNO,
                   CAST(GTFILE.GT_DATE3 AS VARCHAR(10)),
                   GTFILE.GT_DRCRFG,
                   GTFILE.GT_LINE,
                   GTFILE.GT_AMT,
                   GTFILE.GT_REF,
                   GTFILE.GT_FLAG,
                   GTFILE.GT_PERIOD,
                   GTFILE.GT_EXPL,
                   CAST(GTFILE.GT_DATE2 AS VARCHAR(10)),
                   CAST(GTFILE.GT_DATE1 AS VARCHAR(10))

            FROM DBO.GLFILE GLFILE

            LEFT OUTER JOIN DBO.GTFILE GTFILE
              ON  GTFILE.GT_BRANCH = 
                  SUBSTRING(CAST(FORMAT(GLFILE.GL_BR_ACCOUNT, 'D11') 
                  AS CHAR(11)),1,4)
              AND GTFILE.GT_ACCTNO = 
                  SUBSTRING(CAST(FORMAT(GLFILE.GL_BR_ACCOUNT, 'D11') 
                  AS CHAR(11)),5,7)
              AND GTFILE.GT_DATE3  BETWEEN :QGT3-WBEG-DATE3 
                                       AND :QGT3-WEND-DATE3 

            WHERE GLFILE.GL_BR_ACCOUNT BETWEEN :QGL1-WBEG-BR-ACCOUNT
                                           AND :QGL1-WEND-BR-ACCOUNT

            ORDER BY GLFILE.GL_BR_ACCOUNT
           END-EXEC.

           EXEC SQL
               OPEN GL1_CURSOR_V1
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE STAT---GOOD TO GL1-CURSOR-STAT-V1.

      *-----------------------------------------------------------------
       READ-GL1-FILE-NEXT-V1.    *> EMBEDDED GL GT
           PERFORM READ-IT.
           MOVE GL-PATH-SQL TO E-FILE.
           MOVE GL1-KEY     TO E-KEYX.
           INITIALIZE QGL-REC QGT-REC.

           IF ( GL1-CURSOR-STAT-V1-GOOD )
              EXEC SQL
                   FETCH GL1_CURSOR_V1 INTO 
                      :QGL-BR-ACCOUNT,
                      :QGL-DESCRIPTION,
                      :QGL-OPENBAL,
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
                      :QGL-LYBAL-1,
                      :QGL-LYBAL-2,
                      :QGL-LYBAL-3,
                      :QGL-LYBAL-4,
                      :QGL-LYBAL-5,
                      :QGL-LYBAL-6,
                      :QGL-LYBAL-7,
                      :QGL-LYBAL-8,
                      :QGL-LYBAL-9,
                      :QGL-LYBAL-10,
                      :QGL-LYBAL-11,
                      :QGL-LYBAL-12,
                      :QGL-TYPE,
                      :QGL-LYOPENBAL,
                      :QGT-SOURCE,
                      :QGT-BRANCH,
                      :QGT-ACCTNO,
                      :QGT-DATE3,
                      :QGT-DRCRFG,
                      :QGT-LINE,
                      :QGT-AMT,
                      :QGT-REF,
                      :QGT-FLAG,
                      :QGT-PERIOD,
                      :QGT-EXPL,
                      :QGT-DATE2,
                      :QGT-DATE1
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-GL1-FILE-NEXT-V1.

           IF ( IO-FG = 0 )
              PERFORM GET-GL-FIELDS
              PERFORM GET-GT-FIELDS.

      *-----------------------------------------------------------------
       CLOSE-GL1-FILE-V1.
           PERFORM CLOSE-IT.
           IF ( GL1-CURSOR-STAT-V1-GOOD )
              EXEC SQL
                   CLOSE GL1_CURSOR_V1
              END-EXEC
              MOVE STAT---BAD TO GL1-CURSOR-STAT-V1.
      *-----------------------------------------------------------------


       COPY "LIBGL/GLGT3RN.CPY".
       COPY "LIBGB/GBPRINT.CPY".
       COPY "LIBGB/FERRORS.CPY".
            MOVE "E" TO EXT-EOBUF-ERRCD.

      *******************************************
       END-PROGRAM SECTION.
      *******************************************
       END-PROG.
           PERFORM CLOSE-FILES.
           IF EXT-ROUTE-BUF = "00"
              PERFORM PRU-OUT.
           IF NO-RECORDS-FOUND
              MOVE "NO RECORDS FOUND FOR SELECTED RANGE" TO MESS
              PERFORM SEND-MESS.
       EXIT-PROG.

           IF SV-BEG-BR NOT = 9999
              IF EXT-EOBUF-GROUP-FG = "Y"
                 MOVE SV-BEG-BR TO BEG-BRANCH
                 MOVE SV-END-BR TO END-BRANCH
                 MOVE SV-GROUP-FLAG TO GROUP-FLAG
                 MOVE SV-ENT-GROUP TO ENT-GROUP
              ELSE
                 MOVE SV-BEG-BR TO BEG-BRANCH
                 MOVE SV-END-BR TO END-BRANCH.

           IF EXT-ROUTE-BUF = "00"
              MOVE EXIT-PATHNAME TO FORM-PATHNAME.
       STOP-RUN.
           IF EXT-ROUTE-BUF = "00" OR "40" OR "50"
              DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN 
              DESTROY DTLBAL-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.
           EXIT PROGRAM.
