       IDENTIFICATION DIVISION.
       PROGRAM-ID.    DTLBA2.
       AUTHOR.        MJD.
       DATE-WRITTEN. 122195.
      ******************************************************************
      *
      *  DIR : GL
      *  DESC: G/L DETAIL TRIAL BALANCE REPORT
      *             REPORT LISTING BY ACCOUNT, BRANCH
      *
      *=================================================================
      * REV:
      * MJD 010495 CHANGED TO PERFORM PRU-OUT WITH EXT-ROUTE-BUF = "00"
      *            ONLY.
      * MJD 011796 REMOVED BRANCH # AND NAME FROM HEADING.
      * BLL 022396 FIX READ-GT ROUTINES
      * MJD 020697 ADDED LOGIC TO START GT4-REC ON BR/ACC/DATE
      * BLV 082297 BROUGHT FROM A90; REPLACES DETBL2
      * GLF 090497 REPLACED 'IO' ON GP-FILE WITH GET 'GPENV'
      * GLB 990510 BUG, WK2-SOURCE NOT GETTING INITIALIZED FOR START
      * BLV 000103 WHEN READING GT NEXT, GO TO NEXT ACCT IF DATE IS
      *            < BEGINNING DATE; FOR EX. FILE DATES ARE YYMMDD, SO
      *            IF RUNNING FOR 1/1/00 TO 1/31/00, WE START WITH
      *            000101, BUT WILL RUN INTO RECS FROM 99, 98, ETC.
      *            NOTE - IF YOU SELECT DATE RANGE WHICH SPANS CENTURY
      *            IT WILL NOT WORK - WILL START WITH 99 FOR EXAMPLE
      *            & WILL NEVER HIT 00 - WOULD NEED SPECIAL LOGIC
      *  MG 000126 FIXED VDUSTATUS/IO-FG/STAT PER DEVELOPMENT MEETING.
      * BAH 031211 PIGGYBACK PRINT LEFT TERMINAL IN WEIRD MODE.
      *            OPEN-PR-FILE MUST BE BEFORE WORK FILES ARE CREATED
      *            ALSO REMOVED XFER/WRFORM AFTER OPEN-PR-FILE
      * GLB 040429 IF PREVIOUS YEAR SELECTED, DISPLAY LAST YEAR FISCAL
      *            PERIOD DATE RANGE
      * BLV 040430 FIXED GRFILE NOT OPEN, "ERROR READING GR"
      * BLM 080512 ADD OPTION TO SKIP DETAIL, WILL JUST PRINT ACCT &
      *            GRAND TOTALS, WORLD PR# 583
      * BLM 080516 ADD OPTION TO "CATCH UP" OPENING BALANCE WITH
      *            UNUPDATED GL DETAIL.  FOR EX, WORLD WANTS TO SEE
      *            1 DAYS ACTIVITY IN THE MIDDLE OF THE PERIOD AS IF
      *            THEY HAD RUN THE GL UPDATE EVERYDAY, SO READ
      *            UNUPDATED GT RECORDS FROM BEGINNING OF PERIOD TO
      *            THE DAY BEFORE BEGINNING DATE & ADD THESE TO
      *            OPENING BALANCE, WORLD PR# 583
      * BLM 090406 ADD OPTION TO PRINT SPANISH GL DESCRIPTIONS AND
      *            HEADINGS, WORLD PR# 616
      * BLM 090410 ADD A FEW MORE TRANSLATIONS TO SPANISH HEADINGS,
      *            WORLD PR# 616
      * BLM 100416 CHANGE "FECHE" TO "FECHA" ON MEXICAN VERSION,
      *            WORLD PR# 685
      * KEC 2016.0104 {PD#00003} PARADATA <A30> [JAY CISZEWSKI]
      *         FIXED KEYS & RECS FOR GT-REC.
      * KEC 2016.1201 {PR#00000} PARADATA <A30> [JAY CISZEWSKI]
      *          CHANGED GT-DATE1 GT-DATE2 GT-DATE3 AS ACTIVE CCYYMMDD.
      *          CHANGED THE USE OF DATE1 AND DATE2 TO BE
      *           BEG-DATE-MMDDYY AND END-DATE-MMDDYY; AS WELL AS ADDED
      *           BEG-DATE-CCYYMMDD AND END-DATE-CCYYMMDD FOR EASE.
      * BLM 170323 ACTIVATE 8 DIGIT DATES, PD0006
      * BAH 20200827 REMOVED RESET ON FIRST FIELD
      * MB  20240530 PERFORMANCE TUNING FOR GTFILE S35Q-55
      *               EMBEDDED GTFILE ACCESS (START/READ NEXT) DUE TO 
      *               POOR PERFORMING SQL IN GLGT3RN START.
      *       *** SO GLFILE IS NOT READ MULTIPLE TIMES IN THE PROGRAM, 
      *           GL-REC HAS BEEN ADDED TO THE WK FILE, ALONG WITH HARDCODED 
      *           SIZE. IF THE SIZE OF GLFILE TABLE INCREASE, THE 78 LEVEL ON 
      *           WK-FILE WILL ALSO NEED TO BE INCREASED. ***
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "LIBGB/GBFSPR.CPY".
       COPY "LIBGB/GBFSWKI.CPY".
           SELECT WK2-FILE ASSIGN WK2-PATH
                  ORGANIZATION INDEXED
                  ACCESS DYNAMIC
                  LOCK MODE AUTOMATIC WITH LOCK ON RECORD
                  RECORD KEY WK2-KEY
                  FILE STATUS FILE-STAT.
       DATA DIVISION.
       FILE SECTION.
       COPY "LIBGB/GBFDPR.CPY".
      ***********************************
      * SORTED GLFILE BY ACCOUNT, BRANCH
      ***********************************
       FD  WK-FILE
           LABEL RECORDS ARE STANDARD.

       78  WK-GL-SIZE                    VALUE 580. 

       01  WK-REC.
           03  WK-KEY.
               05  WK-GL-ACCOUNT          PIC 9(7).
               05  WK-GL-BRANCH           PIC 9(4).
               05  WK-GL-MTD-UNUPDATED    PIC S9(9)V99.
           03  WK-GL-REC                  PIC X(WK-GL-SIZE).

      ***********************************
      * SORTED GTFILE BY ACCOUNT, BRANCH
      ***********************************
       FD  WK2-FILE
           LABEL RECORDS ARE STANDARD.
       01  WK2-REC.
           03  WK2-KEY.
               05  WK2-GT-ACCTNO           PIC 9(8).
               05  WK2-GT-BRANCH           PIC 9(4).
               05  WK2-GT-SOURCE           PIC 9.
               05  WK2-GT-DATE             PIC 9(8).
               05  WK2-GT-DRCRFG           PIC X.
               05  WK2-GT-LINE             PIC 9(6).
           03  WK2-GT-REF                  PIC X(14).
           03  WK2-GT-FLAG                 PIC X.
           03  WK2-GT-PERIOD               PIC 99.
           03  WK2-GT-EXPL                 PIC X(32).
           03  WK2-GT-DATE2                PIC 9(8).
           03  WK2-GT-AMT                  PIC S9(9)V99.


       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)GL/DTLBA2.CBL S34 06/09/21-20:41:59 >".
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

       01  WK-IFN                      PIC X(2)  VALUE "WK".
       01  WK2-IFN                     PIC X(3)  VALUE "WK2".
       01  WK2-PATH                    PIC X(35).

       EXEC SQL BEGIN DECLARE SECTION END-EXEC.
       01  WS-SUM-AMT                  PIC S9(9)V99.
       EXEC SQL END DECLARE SECTION END-EXEC.

       01  GL1-CURSOR-STAT-V1         PIC X(2) VALUE SPACES.
           88  GL1-CURSOR-STAT-V1-GOOD         VALUE "00".
           88  GL1-CURSOR-STAT-V1-BAD          VALUE "  " THRU "//"
                                                     "01" THRU "~~".

       01  LN-WORKERS.
           03  LN-FEED                 PIC 99        VALUE 0.
           03  LN-COUNT                PIC 99        VALUE 61.
           03  LN-WK                   PIC 99        VALUE 0.
       01  PAGE-NUMBER                 PIC 9(6)       VALUE 0.

       01  HEADUP-GL-ACCT              PIC X(3)    VALUE "NO ".
       01  FIRST-TIME                  PIC X(1)    VALUE "Y".
       01  RECORDS-FOUND         PIC X       VALUE " ".
           88  NO-RECORDS-FOUND              VALUE "N".
       01  DETAIL-EXISTS               PIC X       VALUE "Y".

      *----------------------------------------------------------------
      * DETAIL-PRINTED : SET TO "Y" IF DETAIL HAS BEEN PRINTED.
      *                  ADDED BECAUSE IF FIRST-TIME WAS "Y" THEN
      *                  TOTALS WERE BEING PRINTED IN END-ROUTINE EVEN
      *                  THOUGH NO RECORDS WERE PRINTED.
      *----------------------------------------------------------------
       01  DETAIL-PRINTED              PIC X       VALUE "N".

       01  HOLD-LOC                    PIC 9(4)    VALUE 0.
       01  HOLD-ACCOUNT                PIC 9(7)    VALUE 0.
       01  WORK-GL1-KEY.
           03  WORK-GL-BRANCH          PIC 9(4).
           03  WORK-GL-ACCOUNT         PIC 9(7).

       01  SV-GL-ACCOUNT               PIC 9(7)    VALUE 0.
       01  SV-GL-DESCRIPTION           PIC X(36)   VALUE " ".
       01  SV-GL-TYPE                  PIC X       VALUE " ".
       01  SUB                         PIC 99      VALUE 0.
       01  LEVEL                       PIC 9       VALUE 0.
       01  NEXT-LEVEL                  PIC 9       VALUE 0.
       01  COUNTER                     PIC 99      VALUE 0.

       01  TOTALS-TABLE.
           03  TOT-TABLE         OCCURS 3 TIMES.
               05  TOTAL-PROMPT        PIC X(15).
               05  INITIAL-BAL         PIC S9(11)V99.
               05  TOTAL-DEBIT         PIC S9(11)V99.
               05  TOTAL-CREDIT        PIC S9(11)V99.
               05  FINAL-BAL           PIC S9(11)V99.

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
           03  HOLD-GROUP              PIC X(4).
           03  SKIP-DETAIL-FG          PIC X.
           03  CATCH-UP-OPENBAL-FG     PIC X.

       01  VERSION-CONTROL             PIC X VALUE "A".

       01  CATCHUP-DATE1               PIC 9(8).
       01  CATCHUP-DATE2               PIC 9(8).

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
           03  FILLER                  PIC X(57) VALUE SPACES.

       01  HEAD3.
           03  HEAD3-ACCT-PROMPT       PIC X(18) VALUE
               "ACCOUNT NO RANGE:".
           03  HEAD3-GROUP-ID          PIC X(5)  VALUE SPACES.
           03  HEAD3-ACCT1             PIC 9(7).
           03  FILLER                  PIC X(1)  VALUE "-".
           03  HEAD3-BR1               PIC 9(4).
           03  FILLER                  PIC X(1)  VALUE " ".
           03  HEAD3-ACCT2             PIC 9(7).
           03  FILLER                  PIC X(1)  VALUE "-".
           03  HEAD3-BR2               PIC 9(4).
           03  FILLER                  PIC X(8)  VALUE SPACES.
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
           03  FILLER                  PIC X(20) VALUE "TYPE".
           03  FILLER                  PIC X(22) VALUE "OPENING".
           03  FILLER                  PIC X(25) VALUE "TRANSACTIONS".
           03  FILLER                  PIC X(7)  VALUE "CLOSING".

       01  HEAD4B.
           03  FILLER                  PIC X(13) VALUE "BRN ACCT-NO".
           03  FILLER                  PIC X(34) VALUE
               "ACCOUNT DESCRIPTION".
           03  FILLER                  PIC X(20) VALUE "TYPE".
           03  FILLER                  PIC X(18) VALUE "OPENING".
           03  FILLER                  PIC X(29) VALUE
               "DEBIT          CREDIT".
           03  FILLER                  PIC X(7)  VALUE "CLOSING".

       01  HEAD5.
           03  FILLER                  PIC X(13) VALUE SPACES.
           03  FILLER                  PIC X(12) VALUE "TRANSACTION ".
           03  FILLER                  PIC X(17) VALUE "DESCRIPTION".
           03  FILLER                  PIC X(25) VALUE
               "REFERENCE    SOURCE".
           03  FILLER                  PIC X(16) VALUE "BALANCE".
           03  FILLER                  PIC X(26) VALUE
               "  DEBIT          CREDIT".
           03  FILLER                  PIC X(21) VALUE
               "     BALANCE     DATE".


       01  UNDER-LINE.
           03  FILLER                  PIC X(47) VALUE SPACES.
           03  FILLER                  PIC X(45) VALUE
               "---------------------------------------------".
           03  FILLER                  PIC X(40) VALUE
               "----------------------------------------".

       01  DETAIL-LINE                 PIC X(132) VALUE SPACES.

       01  DETAIL-LINE1 REDEFINES DETAIL-LINE.
           03  D-ACCT                  PIC 9(7).
           03  D-DASH                  PIC X.
           03  D-BRANCH                PIC 9(4).
           03  FILLER                  PIC X(2).
           03  D-DESCRIPTION           PIC X(28).
           03  GLMASTER-DETAIL.
               05  D-TYPE              PIC X(18).
               05  D-INITIAL-BAL       PIC Z(10)9.99-.
               05  FILLER              PIC X.
           03  GLTRANS-DETAIL REDEFINES GLMASTER-DETAIL.
               05  D-REFERENCE         PIC X(16).
               05  D-SOURCE            PIC X(2).
               05  D-FLAG              PIC X(2).
               05  D-PERIOD            PIC 99.
               05  FILLER              PIC X(2).
               05  D-POLL-SYSTEM       PIC X(6).
               05  FILLER              PIC X.
               05  D-POLL-FLAG         PIC X.
               05  FILLER              PIC X(2).
           03  D-DEBIT                 PIC Z(11).ZZCR.
           03  D-CREDIT                PIC Z(11).ZZDB.
           03  D-FINAL-BAL             PIC Z(10)9.99-.
           03  FILLER                  PIC X(1).
           03  D-DATE                  PIC 99/99/99.


       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/MKTEMPW_CMD.CPY".
       COPY "LIBWI/GRSCANW.CPY".
       COPY "LIBGB/GRWORKW.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBGB/GETPRW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/SYSTEMW.CPY".
       COPY "LIBGB/GPENVW.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBGL/DTLBA2_DEF.CPY".
       COPY "LIBGL/DTLBA2_WKS.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".
      
       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBGL/DTLBA2_SCN.CPY".

       PROCEDURE DIVISION
           USING FORM-PATHNAME EXIT-PATHNAME.
       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
               PR-FILE WK-FILE WK2-FILE.
       COPY "LIBGB/DECLRP.CPY".
           PERFORM CLOSE-GT3-FILE.
           PERFORM CLOSE-GT1-FILE.
           PERFORM CLOSE-GL1-FILE.
           PERFORM CLOSE-GR1-FILE.
           PERFORM CLOSE-BR-FILE.
           CLOSE
               PR-FILE WK-FILE WK2-FILE.
       COPY "LIBGB/DECLRP2.CPY".

      ******************************************************
       MAIN-PROGRAM SECTION.
      ******************************************************

           PERFORM SQL-CONNECT.

      D    STOP MESS.
           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "DTLBA2" TO VDU-FORM-NAME.

           PERFORM GET-GPENV.

           PERFORM INITIALIZATION.
           IF EXT-EOBUF-ERRCD NOT = " "
              GO TO END-PROG.

           IF EXT-ROUTE-BUF = "00"
              MOVE "REPORT IN PROGRESS........." TO MESS
              MOVE MESS-LIST TO WR-LIST
              MOVE MESS TO WR-BUF
              PERFORM CALL-WRFORM.

           PERFORM OPEN-WK-FILE.
           PERFORM OPEN-WK2-FILE.

       NEXT-GL-MASTER.
           PERFORM READ-WK-FILE-NEXT.
           IF IO-BAD
              GO TO END-ROUTINE.

           MOVE WK-GL-ACCOUNT TO GL-ACCOUNT.
           MOVE WK-GL-BRANCH  TO GL-BRANCH.
           MOVE WK-GL-REC     TO GL-REC.

      *     PERFORM READ-GL1-FILE.
      *     IF IO-BAD
      *        MOVE "MISSING GL RECORD SINCE SORT" TO MESS
      *        PERFORM SEND-MESS
      *        GO TO NEXT-GL-MASTER.

           MOVE "YES" TO HEADUP-GL-ACCT.
           IF FIRST-TIME = "Y"
              MOVE "N"            TO FIRST-TIME
              MOVE GL-BRANCH      TO HOLD-LOC
              MOVE GL-ACCOUNT     TO HOLD-ACCOUNT SV-GL-ACCOUNT
              MOVE GL-DESCRIPTION TO SV-GL-DESCRIPTION
              MOVE GL-TYPE        TO SV-GL-TYPE.
      *----------------------------------------------------------------
      *            TEST  BRANCH  TOTALS
      *----------------------------------------------------------------
           IF GL-ACCOUNT NOT = HOLD-ACCOUNT
              PERFORM PRINT-BRANCH-TOTALS.

       START-GLTRANS.
           MOVE GL-BR-ACCOUNT TO WORK-GL1-KEY.
           MOVE GL-ACCOUNT    TO WK2-GT-ACCTNO.
           MOVE GL-BRANCH     TO WK2-GT-BRANCH.
           MOVE " "           TO WK2-GT-SOURCE.
           MOVE BEG-DATE      TO WK2-GT-DATE.
           MOVE " "           TO WK2-GT-DRCRFG.
           MOVE 0             TO WK2-GT-LINE.
           PERFORM START-WK2-FILE.

       READ-GLTRANS.
           PERFORM READ-WK2-FILE-NEXT.
           IF IO-BAD
              PERFORM PRINT-ACCOUNT-TOTALS
              GO TO NEXT-GL-MASTER.

           IF NOT (WK2-GT-BRANCH = WORK-GL-BRANCH AND
                         WK2-GT-ACCTNO = WORK-GL-ACCOUNT)
              PERFORM PRINT-ACCOUNT-TOTALS
              GO TO NEXT-GL-MASTER.

           MOVE WK2-GT-SOURCE TO GT-SOURCE.
           MOVE WK2-GT-DATE   TO GT-DATE2.
           MOVE WK2-GT-ACCTNO TO GT-ACCTNO.
           MOVE WK2-GT-BRANCH TO GT-BRANCH.
           MOVE WK2-GT-DATE   TO GT-DATE3.
           MOVE WK2-GT-DRCRFG TO GT-DRCRFG.
           MOVE WK2-GT-LINE   TO GT-LINE.

           MOVE WK2-GT-REF    TO GT-REF.
           MOVE WK2-GT-FLAG   TO GT-FLAG.
           MOVE WK2-GT-PERIOD TO GT-PERIOD.
           MOVE WK2-GT-EXPL   TO GT-EXPL.
           MOVE WK2-GT-DATE2  TO GT-DATE2.
           MOVE WK2-GT-AMT    TO GT-AMT.

      *     PERFORM READ-GT1-FILE.
      *     IF IO-BAD
      *        MOVE "MISSING GT RECORD SINCE SORT..." TO MESS
      *        PERFORM SEND-MESS
      *        GO TO IO-ERROR.

           MOVE "Y" TO DETAIL-EXISTS.
           PERFORM PRINT-GLTRANS-INFO.
           GO TO READ-GLTRANS.

      *----------------------------------------------------------------
      * END ROUTINES
      *----------------------------------------------------------------
       END-ROUTINE.
           IF FIRST-TIME = "Y" OR DETAIL-PRINTED = "N"
              MOVE "N" TO RECORDS-FOUND
           ELSE
              PERFORM PRINT-BRANCH-TOTALS
              MOVE 3 TO LEVEL
              MOVE SPACES TO DETAIL-LINE
              MOVE "REPORT TOTAL  :" TO TOTAL-PROMPT(LEVEL)
              PERFORM TOTAL-PRINT-ROUTINE
              MOVE " " TO EXT-EOBUF-ERRCD.

           GO TO END-PROGRAM.

      *****************************************************************
       INITIALIZATION SECTION.
      *****************************************************************
           MOVE DTLBA2-QUEUE-POS TO Q-POS.
           MOVE DTLBA2-COPIES-POS TO C-POS.
           MOVE DTLBA2-PRU-POS   TO P-POS.
           MOVE SPACES TO Q-BUF C-BUF P-BUF.
       COPY "LIBGB/EOINIT.CPY".

           IF CATCH-UP-OPENBAL-FG NOT = "Y"
              GO TO SKIP-COMPUTE-CATCHUP-DATE.
           IF CURRENT-PERIOD = 1
              IF BEG-DATE = GP-BEG-FISCAL-DATE
                 MOVE "N" TO CATCH-UP-OPENBAL-FG
                 GO TO SKIP-COMPUTE-CATCHUP-DATE.

           IF CURRENT-PERIOD = 1
              MOVE GP-BEG-FISCAL-DATE TO CATCHUP-DATE1
           ELSE
              MOVE GP-END-FISCAL-DATE(CURRENT-PERIOD - 1) TO NDTE-DATE
              MOVE 1 TO NDTE-HOLD
              PERFORM INCREMENT-DAYS
              IF BEG-DATE = NDTE-DATE
                 MOVE "N" TO CATCH-UP-OPENBAL-FG
                 GO TO SKIP-COMPUTE-CATCHUP-DATE
              ELSE
                 MOVE NDTE-DATE TO CATCHUP-DATE1.

           MOVE END-DATE TO NDTE-DATE.
           MOVE -1 TO NDTE-HOLD.
           PERFORM INCREMENT-DAYS.
           MOVE NDTE-DATE TO CATCHUP-DATE2.

      *-----------------------------------------------------------------
       SKIP-COMPUTE-CATCHUP-DATE.

           MOVE BEG-DATE    TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO HEAD3-DATE1.

           MOVE END-DATE    TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO HEAD3-DATE2.

           MOVE "N" TO DETAIL-PRINTED.

           IF EXT-ROUTE-BUF = "00"
              MOVE "REPORT IN PROGRESS........." TO MESS
              MOVE MESS-LIST TO WR-LIST
              MOVE MESS TO WR-BUF
              PERFORM CALL-WRFORM.

           PERFORM OPEN-BR-FILE.
           PERFORM OPEN-GR1-FILE.
      * OPEN PRINTER BEFORE THE WORK FILES GET CREATED
           PERFORM OPEN-PR-FILE.

           PERFORM SORT-ROUTINE.
           IF EXT-EOBUF-ERRCD NOT = " "
              GO TO ENDIT.

           IF EXT-ROUTE-BUF = "00"
              MOVE SPACE                         TO MESS
              MOVE "REPORT IN PROGRESS........." TO MESS
              MOVE MESS-LIST TO WR-LIST
              MOVE MESS TO WR-BUF
              PERFORM CALL-WRFORM.

       ZERO-OUT-TOTAL-TABLE.
           PERFORM INITIALIZE-TOTALS VARYING LEVEL
              FROM 1 BY 1 UNTIL LEVEL > 3.

           PERFORM OPEN-GT3-FILE.
           PERFORM OPEN-GL1-FILE.
           PERFORM OPEN-WK-FILE.
           PERFORM OPEN-WK2-FILE.

           MOVE 0 TO WK-GL-BRANCH.
           MOVE 0 TO WK-GL-ACCOUNT.
           PERFORM START-WK-FILE.

           GO TO EXIT-INITIALIZE.
       ENDIT.
           MOVE "X" TO EXT-EOBUF-ERRCD.
       EXIT-INITIALIZE.
           EXIT.

      *****************************************************************
       EO-EXEC-AUDIT SECTION.
      *****************************************************************
       EA-000.
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
       EA-EXIT.
           EXIT.

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
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    SETUP LAST YEAR DATE IF PREVIOUS PERIOD:
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       SETUP-LAST-YEAR.
           IF REPORT-OPTION = "P"
              MOVE BEG-DATE  TO NDTE-DATE
              MOVE -1        TO NDTE-HOLD
              PERFORM INCREMENT-YEARS
              MOVE NDTE-DATE TO BEG-DATE
              MOVE END-DATE  TO NDTE-DATE
              MOVE -1        TO NDTE-HOLD
              PERFORM INCREMENT-YEARS
              MOVE NDTE-DATE TO END-DATE.

       DISPLAY-DATES.
           MOVE "S  A000BEGDATE." TO VDU.
           MOVE BEG-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO VDU-DATE-EDIT.
           PERFORM SCREENIO.
           MOVE "S  A000ENDDATE." TO VDU.
           MOVE END-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO VDU-DATE-EDIT.
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

           MOVE VDUBUF TO WGR-WORD HOLD-GROUP.
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

       SKIP-BRANCH-DETAIL.
           MOVE "E  A010SKIPDET." TO VDU.
           PERFORM SCREENIO.
           IF F1-KEY GO TO OPTION-01.
           IF UP-KEY GO TO PRINT-ZERO-BALANCES.
           IF F7-KEY GO TO EXIT-PARM.
           IF NOT ENTER-KEY GO TO SKIP-BRANCH-DETAIL.
           MOVE VDUBUF TO SKIP-DETAIL-FG.
           IF SKIP-DETAIL-FG NOT = "Y"
              IF SKIP-DETAIL-FG NOT = "N"
                 GO TO SKIP-BRANCH-DETAIL.
       CATCHUP-UNUPDATED-GTS.
           MOVE "E  A010CATCHUP." TO VDU.
           PERFORM SCREENIO.
           IF F1-KEY GO TO OPTION-01.
           IF UP-KEY GO TO SKIP-BRANCH-DETAIL.
           IF F7-KEY GO TO EXIT-PARM.
           IF NOT ENTER-KEY GO TO CATCHUP-UNUPDATED-GTS.
           MOVE VDUBUF TO CATCH-UP-OPENBAL-FG.
           IF CATCH-UP-OPENBAL-FG NOT = "Y"
              IF CATCH-UP-OPENBAL-FG NOT = "N"
                 GO TO CATCHUP-UNUPDATED-GTS.
       EXIT-PARM.
           EXIT.

      *****************************************************************
       DISPLAY-PARAMETERS SECTION.
      *****************************************************************
           IF GROUP-FLAG = "Y"
              MOVE "S  A040BR1." TO VDU
              MOVE HOLD-GROUP TO VDUBUF
              PERFORM SCREENIO
           ELSE
              MOVE "SNNN040BR1." TO VDU
              MOVE BEG-BRANCH TO VDUNUM0
              PERFORM SCREENIO
              MOVE "SNNN040BR2." TO VDU
              MOVE END-BRANCH TO VDUNUM0
              PERFORM SCREENIO.
      
           MOVE "SNNN070ACCT1." TO VDU.
           MOVE BEG-ACCOUNT TO VDUNUM0.
           PERFORM SCREENIO.
      
           MOVE "SNNN070ACCT2." TO VDU.
           MOVE END-ACCOUNT TO VDUNUM0.
           PERFORM SCREENIO.
      
           MOVE "S  A010ZEROBAL." TO VDU.
           MOVE ZERO-BALS TO VDUBUF.
           PERFORM SCREENIO.
      
           MOVE "S  A010SKIPDET." TO VDU.
           MOVE SKIP-DETAIL-FG TO VDUBUF.
           PERFORM SCREENIO.
      
           MOVE "S  A010CATCHUP." TO VDU.
           MOVE CATCH-UP-OPENBAL-FG TO VDUBUF.
           PERFORM SCREENIO.

      *****************************************************************
       INITIALIZE-TOTALS SECTION.
      *****************************************************************
           MOVE 0 TO INITIAL-BAL(LEVEL) TOTAL-CREDIT(LEVEL)
                     TOTAL-DEBIT(LEVEL)    FINAL-BAL(LEVEL).

      *****************************************************************
       SUMMING-UP SECTION.
      *****************************************************************

      * CALCULATE OPENING AMOUNT
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
           MOVE PAGE-NUMBER TO HEAD1-PAGENO.
           PERFORM GET-TIME.
           MOVE TIME-EDIT TO HEAD1-TIME.
           MOVE EXT-CONAME-SYSDATE TO HEAD1-DATE.
           MOVE EXT-CONAME-CONAME  TO HEAD1-CONAME.
           MOVE HEAD1 TO PR-REC.
           MOVE 99 TO LN-FEED.
           PERFORM WRITE-PR-REC.
      *----------------------------------------------------------------
           MOVE EXT-CONAME-USER-ID TO HEAD2-USERID.
           MOVE HOLD-LOC TO BR-NO.
           PERFORM READ-BR-FILE.
           IF IO-BAD MOVE SPACES TO BR-REC.
           MOVE HEAD2 TO PR-REC.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-PR-REC.
      *----------------------------------------------------------------
           MOVE "ACCOUNT RANGE:" TO HEAD3-ACCT-PROMPT.
           MOVE "PERIOD:"        TO HEAD3-PER-PROMPT.
           IF GROUP-FLAG = "Y"
              MOVE HOLD-GROUP TO HEAD3-GROUP-ID.
           MOVE BEG-BRANCH TO HEAD3-BR1.
           MOVE END-BRANCH TO HEAD3-BR2.
           MOVE BEG-ACCOUNT TO HEAD3-ACCT1.
           MOVE END-ACCOUNT TO HEAD3-ACCT2.
           MOVE CURRENT-PERIOD   TO HEAD3-PERIOD.
           MOVE HEAD3 TO PR-REC.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-PR-REC.
      *----------------------------------------------------------------
           IF SKIP-DETAIL-FG = "Y"
              MOVE HEAD4B TO PR-REC
              MOVE 2 TO LN-FEED
              PERFORM WRITE-PR-REC
           ELSE
              MOVE HEAD4 TO PR-REC
              MOVE 2 TO LN-FEED
              PERFORM WRITE-PR-REC
              MOVE HEAD5 TO PR-REC
              MOVE 1 TO LN-FEED
              PERFORM WRITE-PR-REC.
      
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
           IF CATCH-UP-OPENBAL-FG = "Y"
              ADD WK-GL-MTD-UNUPDATED TO INITIAL-BAL(1).
           IF ZERO-BALS = "N"
              IF INITIAL-BAL(1) = 0
                 IF DETAIL-EXISTS = "N"
                    GO TO EXIT-PRINT-GLMASTER.

           MOVE "Y" TO DETAIL-PRINTED.
           MOVE INITIAL-BAL(1) TO D-INITIAL-BAL.
           IF SKIP-DETAIL-FG = "Y"
              GO TO EXIT-PRINT-GLMASTER.
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

           MOVE GT-EXPL   TO D-DESCRIPTION.

           MOVE GT-DATE2                   TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY                TO D-DATE.
       DR-CR.
           IF GT-AMT > 0
               GO TO CALC-POSITIVE.

           MULTIPLY -1 BY GT-AMT.
           MOVE GT-AMT TO D-CREDIT.
           MOVE 0      TO D-DEBIT.
           ADD GT-AMT  TO TOTAL-CREDIT(1).
           GO TO PRINT-IT.
       CALC-POSITIVE.
           MOVE GT-AMT TO D-DEBIT.
           MOVE 0      TO D-CREDIT.
           ADD GT-AMT  TO TOTAL-DEBIT(1).
       PRINT-IT.
           MOVE "Y" TO DETAIL-PRINTED.
           MOVE 1 TO LN-FEED.
           IF SKIP-DETAIL-FG NOT = "Y"
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
           MOVE "BRANCH TOTAL  :" TO TOTAL-PROMPT(LEVEL).
           PERFORM TOTAL-PRINT-ROUTINE.
       EXIT-ACCOUNT-TOTALS.
           EXIT.

      *****************************************************************
       PRINT-BRANCH-TOTALS SECTION.
      *****************************************************************
           MOVE SPACES TO DETAIL-LINE.
           MOVE 2 TO LEVEL.
           IF SKIP-DETAIL-FG = "Y"
              MOVE SV-GL-ACCOUNT     TO D-ACCT
              MOVE SV-GL-DESCRIPTION TO D-DESCRIPTION
              MOVE SV-GL-TYPE        TO D-TYPE
           ELSE
              MOVE "ACCOUNT TOTAL :"  TO TOTAL-PROMPT(LEVEL).
           PERFORM TOTAL-PRINT-ROUTINE.
           MOVE GL-BRANCH  TO HOLD-LOC.
           MOVE GL-ACCOUNT TO HOLD-ACCOUNT.
           MOVE GL-BRANCH TO WORK-GL-BRANCH.
           MOVE GL-ACCOUNT TO WORK-GL-ACCOUNT SV-GL-ACCOUNT
           MOVE GL-DESCRIPTION TO SV-GL-DESCRIPTION.
           MOVE GL-TYPE TO SV-GL-TYPE.
       EXIT-RTOTAL.
           EXIT.

      *****************************************************************
      *      TOTALS PRINT ROUTINE:
      *           LEVEL OF 1 = BRANCH TOTALS
      *           LEVEL OF 2 = ACCOUNT TOTALS
      *           LEVEL OF 3 = REPORT TOTALS
      *****************************************************************
       TOTAL-PRINT-ROUTINE SECTION.
           IF ZERO-BALS = "N"
              IF INITIAL-BAL(LEVEL) = 0 AND
                 TOTAL-DEBIT(LEVEL) = 0 AND
                 TOTAL-CREDIT(LEVEL) = 0 AND
                 FINAL-BAL(LEVEL) = 0
                    GO TO EXIT-TOTAL-PRINT.

           IF SKIP-DETAIL-FG NOT = "Y"
              MOVE TOTAL-PROMPT(LEVEL) TO D-DESCRIPTION.
           IF SKIP-DETAIL-FG = "Y"
              IF LEVEL = 3
                 MOVE TOTAL-PROMPT(LEVEL) TO D-DESCRIPTION.
           MOVE INITIAL-BAL(LEVEL)  TO D-INITIAL-BAL.
           MOVE TOTAL-DEBIT(LEVEL)  TO D-DEBIT.
           MOVE TOTAL-CREDIT(LEVEL) TO D-CREDIT.
           MOVE FINAL-BAL(LEVEL)    TO D-FINAL-BAL.
           MOVE 2 TO LN-FEED.
           IF LEVEL > 1
              PERFORM WRITE-DETAIL-LINE
           ELSE
              IF SKIP-DETAIL-FG NOT = "Y"
                 PERFORM WRITE-DETAIL-LINE.
           IF LEVEL = 2
              MOVE UNDER-LINE TO PR-REC
              MOVE 1 TO LN-FEED
              PERFORM WRITE-PR-REC
              ADD LN-FEED TO LN-COUNT.

           IF LEVEL < 3
              ADD 1 LEVEL GIVING NEXT-LEVEL
              ADD INITIAL-BAL(LEVEL)  TO INITIAL-BAL(NEXT-LEVEL)
              ADD TOTAL-DEBIT(LEVEL)  TO TOTAL-DEBIT(NEXT-LEVEL)
              ADD TOTAL-CREDIT(LEVEL) TO TOTAL-CREDIT(NEXT-LEVEL)
              ADD FINAL-BAL(LEVEL)    TO FINAL-BAL(NEXT-LEVEL).
           PERFORM INITIALIZE-TOTALS.
       EXIT-TOTAL-PRINT.


      ******************************************************************
       SORT-ROUTINE SECTION.
      ******************************************************************

      * SORTS GL AND GT FILES INTO WK AND WK2 SORT KEY FILES.

      * CREATE WORK KEY FILE FOR GTFILE OUTPUT

           MOVE MKTEMP-DEFAULT TO MKTEMP-BUF.
           PERFORM MKTEMP-CALL.
           IF STAT-BAD
              MOVE "CANNOT MAKE SORT KEY FILE" TO MESS
              PERFORM SEND-MESS
              MOVE "X" TO EXT-EOBUF-ERRCD
              GO TO EXIT-SORT-ROUTINE.
           MOVE MKTEMP-BUF TO WK2-PATH.
           PERFORM OPEN-WK2-FILE-OUTPUT.

      * CREATE WORK KEY FILE FOR GLFILE OUTPUT
           MOVE MKTEMP-DEFAULT TO MKTEMP-BUF.
           PERFORM MKTEMP-CALL.
           IF STAT-BAD
              MOVE "CANNOT MAKE SORT KEY FILE" TO MESS
              PERFORM SEND-MESS
              MOVE "X" TO EXT-EOBUF-ERRCD
              GO TO EXIT-SORT-ROUTINE.
           PERFORM OPEN-WK-FILE-OUTPUT.

           PERFORM OPEN-GL1-FILE.
           PERFORM OPEN-GT3-FILE.

           IF GROUP-FLAG = "N"
              MOVE BEG-BRANCH TO BR-NO
              PERFORM START-BR-FILE
              GO TO NEXT-BRANCH-NO.

           MOVE HOLD-GROUP TO GR-ID.
           PERFORM SEARCH-GR-INIT.

      *-----------------------------------------------------------------
       NEXT-GROUP-LOC.
           IF GROUP-FLAG NOT = "Y"
              GO TO NEXT-BRANCH-NO.

           PERFORM SEARCH-GR-NEXT.

           IF WGR-SRESULT = 0
              GO TO SORT-OUT.

           MOVE WGR-SRESULT TO BR-NO.
           PERFORM READ-BR-FILE.
           IF IO-BAD
              GO TO IO-ERROR.

           GO TO MOVE-BRANCH-PATH-INFO.

       NEXT-BRANCH-NO.
           PERFORM READ-BR-FILE-NEXT.
           IF IO-BAD OR BR-NO > END-BRANCH
              GO TO SORT-OUT.

       MOVE-BRANCH-PATH-INFO.

           IF EXT-ROUTE-BUF = "00"
              MOVE SPACE TO MESS
              STRING "EXTRACTION IN PROGRESS.....", BR-NO INTO MESS
              MOVE MESS-LIST TO WR-LIST
              MOVE MESS TO WR-BUF
              PERFORM CALL-WRFORM.

           MOVE BR-NO       TO GL-BRANCH
                               QGL1-WBEG-BRANCH
                               QGL1-WEND-BRANCH.
           MOVE BEG-ACCOUNT TO GL-ACCOUNT
                               QGL1-WBEG-ACCOUNT.
           MOVE END-ACCOUNT TO QGL1-WEND-ACCOUNT.

      * GL CURSOR ALSO CONTAINS GT DATA
           MOVE BR-NO       TO QGT3-WBEG-BRANCH.
           MOVE BEG-DATE    TO GT-DATE3.
           IF BEG-DATE = 0
              MOVE 19000101 TO GT-DATE3.
           MOVE GT-DATE3             TO SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD  TO QGT3-WBEG-DATE3.
           MOVE END-DATE             TO SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD  TO QGT3-WEND-DATE3.

           PERFORM START-GL1-FILE-V1.  *> EMBEDDED GL GT

       SORT-GL-NEXT.
           PERFORM READ-GL1-FILE-NEXT-V1.   *> EMBEDDED GL GT
           IF IO-BAD
              PERFORM CLOSE-GL1-FILE-V1
              GO TO END-OF-SORT-GROUP.

      *THESE ALSO CHECKED IN CURSOR
           IF GL-BRANCH > BR-NO
              GO TO END-OF-SORT-GROUP.
           IF GL-ACCOUNT > END-ACCOUNT
              GO TO END-OF-SORT-GROUP.

      * WRITE WK FILE ONCE FOR EACH GL BRANCH/ACCTNO
           IF GL-BRANCH  NOT = WK-GL-BRANCH OR
              GL-ACCOUNT NOT = WK-GL-ACCOUNT
              MOVE GL-ACCOUNT TO WK-GL-ACCOUNT
              MOVE GL-BRANCH  TO WK-GL-BRANCH
              IF CATCH-UP-OPENBAL-FG = "Y"
                 PERFORM CATCH-UP-OPENBAL
              ELSE
                 MOVE 0 TO WK-GL-MTD-UNUPDATED
              END-IF
              MOVE GL-REC TO WK-GL-REC
              PERFORM WRITE-WK-FILE
              IF IO-BAD
                 GO TO IO-ERROR.

      *    MOVE GL-BRANCH   TO GT-BRANCH
      *                        QGT3-WBEG-BRANCH
      *                        QGT3-WEND-BRANCH.
      *    MOVE GL-ACCOUNT  TO GT-ACCTNO
      *                        QGT3-WBEG-ACCTNO
      *                        QGT3-WEND-ACCTNO.
      *    MOVE BEG-DATE    TO GT-DATE3.
      *    IF BEG-DATE = 0
      *       MOVE 19000101 TO GT-DATE3.
      *    MOVE GT-DATE3             TO SQL-DATE-YYYYMMDD.
      *    PERFORM SQL-SET-DATE.
      *    MOVE SQL-DATE-YYYY-MM-DD  TO QGT3-WBEG-DATE3.
      *    MOVE END-DATE             TO SQL-DATE-YYYYMMDD.
      *    PERFORM SQL-SET-DATE.
      *    MOVE SQL-DATE-YYYY-MM-DD  TO QGT3-WEND-DATE3.
      *
      *     MOVE SPACES      TO GT-DRCRFG
      *                         QGT3-WBEG-DRCRFG.
      *     MOVE ALL "Z"     TO QGT3-WEND-DRCRFG.
      *
      *     MOVE 0           TO GT-LINE
      *                         QGT3-WBEG-LINE.
      *     MOVE ALL "9"     TO QGT3-WEND-LINE.

      *-   -   -{ ADD ONE TO WHERE SCAN BEG FIELDS }
      *     IF ( GT-BRANCH = 9999 )
      *         MOVE GT-BRANCH         TO QGT3-WSBEG-BRANCH
      *     ELSE
      *        COMPUTE QGT3-WSBEG-BRANCH = GT-BRANCH + 1.
      *
      *     IF ( GT-ACCTNO = 99999999 )
      *         MOVE GT-ACCTNO         TO QGT3-WSBEG-ACCTNO
      *     ELSE
      *        COMPUTE QGT3-WSBEG-ACCTNO = GT-ACCTNO + 1.
      *
      *     MOVE GT-DATE3             TO NDTE-DATE
      *     MOVE 1                    TO NDTE-HOLD
      *     PERFORM INCREMENT-DAYS
      *     MOVE NDTE-DATE            TO SQL-DATE-YYYYMMDD
      *     PERFORM SQL-SET-DATE
      *     MOVE SQL-DATE-YYYY-MM-DD  TO QGT3-WSBEG-DATE3
      *
      *     MOVE 01                   TO SQL-TEXT-SIZE
      *     MOVE GT-DRCRFG            TO SQL-TEXT
      *     PERFORM SQL-TEXT-INCREMENT
      *     MOVE SQL-TEXT             TO QGT3-WSBEG-DRCRFG.
      *
      *     PERFORM START-GT3-FILE.

       SORT-GT-NEXT-NEW.
      * GT-BRANCH = 0 IF GT ROW(S) NOT FOUND
           IF GT-BRANCH > 0
              MOVE GT-SOURCE TO WK2-GT-SOURCE
              MOVE GT-ACCTNO TO WK2-GT-ACCTNO
              MOVE GT-BRANCH TO WK2-GT-BRANCH
              MOVE GT-DATE3  TO WK2-GT-DATE
              MOVE GT-DRCRFG TO WK2-GT-DRCRFG
              MOVE GT-LINE   TO WK2-GT-LINE
              MOVE GT-REF    TO WK2-GT-REF
              MOVE GT-FLAG   TO WK2-GT-FLAG
              MOVE GT-PERIOD TO WK2-GT-PERIOD
              MOVE GT-EXPL   TO WK2-GT-EXPL
              MOVE GT-DATE2  TO WK2-GT-DATE2
              MOVE GT-AMT    TO WK2-GT-AMT
              PERFORM WRITE-WK2-FILE
              IF IO-BAD
                 GO TO IO-ERROR.

           GO TO SORT-GL-NEXT.

      *SORT-GT-NEXT-OLD.
      *    PERFORM READ-GT3-FILE-NEXT.
      *    IF IO-BAD
      *       GO TO SORT-GL-NEXT.
      *
      *    IF GT-BRANCH > GL-BRANCH
      *       GO TO SORT-GL-NEXT.
      *    IF GT-ACCTNO > GL-ACCOUNT
      *       GO TO SORT-GL-NEXT.
      *    IF GT-DATE3 > END-DATE
      *       GO TO SORT-GL-NEXT.
      *
      *    MOVE GT-SOURCE TO WK2-GT-SOURCE.
      *    MOVE GT-ACCTNO TO WK2-GT-ACCTNO.
      *    MOVE GT-BRANCH TO WK2-GT-BRANCH.
      *    MOVE GT-DATE3  TO WK2-GT-DATE.
      *    MOVE GT-DRCRFG TO WK2-GT-DRCRFG.
      *    MOVE GT-LINE   TO WK2-GT-LINE.
      *
      *    PERFORM WRITE-WK2-FILE.
      *    IF IO-BAD
      *       GO TO IO-ERROR.
      *
      *    GO TO SORT-GT-NEXT.

       END-OF-SORT-GROUP.
           GO TO  NEXT-GROUP-LOC.

       SORT-OUT.
           PERFORM CLOSE-WK-FILE.
           PERFORM CLOSE-WK2-FILE.
      *     PERFORM CLOSE-GL1-FILE.
      *     PERFORM CLOSE-GT3-FILE.

       EXIT-SORT-ROUTINE.
           EXIT.

      ************************************************
       CATCH-UP-OPENBAL SECTION.
      ************************************************

      *  ACCUMULATE UNUPDATED GL DETAIL AMT FROM 1ST DAY OF PERIOD
      *  UNTIL SELECTED BEGINNING DATE

           MOVE 0 TO WK-GL-MTD-UNUPDATED
                     WS-SUM-AMT.

           MOVE GL-BRANCH   TO GT-BRANCH
                               QGT3-WBEG-BRANCH
                               QGT3-WEND-BRANCH.
           MOVE GL-ACCOUNT  TO GT-ACCTNO
                               QGT3-WBEG-ACCTNO
                               QGT3-WEND-ACCTNO.
           MOVE CATCHUP-DATE1 TO GT-DATE3 SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD  TO QGT3-WBEG-DATE3.

           MOVE CATCHUP-DATE2 TO SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD  TO QGT3-WEND-DATE3.

      * CHANGED TO SUM SQL MB 20240530
           PERFORM READ-GT3-FILE-SUM-AMT.


      *     MOVE SPACES      TO GT-DRCRFG
      *                         QGT3-WBEG-DRCRFG.
      *     MOVE ALL "Z"     TO QGT3-WEND-DRCRFG.
      *
      *     MOVE 0           TO GT-LINE
      *                         QGT3-WBEG-LINE.
      *     MOVE ALL "9"     TO QGT3-WEND-LINE.

      *-   -   -{ ADD ONE TO WHERE SCAN BEG FIELDS }
      *     IF ( GT-BRANCH = 9999 )
      *         MOVE GT-BRANCH         TO QGT3-WSBEG-BRANCH
      *     ELSE
      *        COMPUTE QGT3-WSBEG-BRANCH = GT-BRANCH + 1.
      *
      *     IF ( GT-ACCTNO = 99999999 )
      *         MOVE GT-ACCTNO         TO QGT3-WSBEG-ACCTNO
      *     ELSE
      *        COMPUTE QGT3-WSBEG-ACCTNO = GT-ACCTNO + 1.
      *  
      *     MOVE GT-DATE3             TO NDTE-DATE
      *     MOVE 1                    TO NDTE-HOLD
      *     PERFORM INCREMENT-DAYS
      *     MOVE NDTE-DATE            TO SQL-DATE-YYYYMMDD
      *     PERFORM SQL-SET-DATE
      *     MOVE SQL-DATE-YYYY-MM-DD  TO QGT3-WSBEG-DATE3
      *
      *     MOVE 01                   TO SQL-TEXT-SIZE
      *     MOVE GT-DRCRFG            TO SQL-TEXT
      *     PERFORM SQL-TEXT-INCREMENT
      *     MOVE SQL-TEXT             TO QGT3-WSBEG-DRCRFG.
      *
      *     PERFORM START-GT3-FILE.
      *
      *NEXT-CATCHUP-GT.
      *    PERFORM READ-GT3-FILE-NEXT.
      *    IF IO-BAD
      *       GO TO EXIT-CATCHUP-GT.
      *
      *    IF GT-BRANCH > GL-BRANCH
      *       GO TO EXIT-CATCHUP-GT.
      *    IF GT-ACCTNO > GL-ACCOUNT
      *       GO TO EXIT-CATCHUP-GT.
      *    IF GT-FLAG = "P"
      *       GO TO NEXT-CATCHUP-GT.
      *
      *    IF GT-DATE3 > CATCHUP-DATE2
      *       GO TO EXIT-CATCHUP-GT.
      *
      *    ADD GT-AMT TO WK-GL-MTD-UNUPDATED. 
      *
      *    GO TO NEXT-CATCHUP-GT.

       EXIT-CATCHUP-GT.
           EXIT.

      **********************************************************
       FORM-RESET SECTION.
      **********************************************************
           MOVE EXT-CONAME-CONAME TO DTLBA2-CONAME.
           MOVE EXT-CONAME-SYSDATE TO DTLBA2-SYSDATE.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DISPLAY DTLBA2-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE DTLBA2-FORM-FIELDS TO WR-BUF. 
           MOVE DTLBA2-FORM-LIST TO WR-LIST. 

       FORM-RESET-EXIT.
           EXIT.

      **********************************************************
       SETUP-LINK-INFO SECTION.
      **********************************************************
           MOVE DTLBA2-HELP--FILE TO HELP-LINK-FILE.
           MOVE DTLBA2-HELP--DIR  TO HELP-LINK-DIR.

      **********************************************************
       VDU-CONVERT-FIELD SECTION.
      **********************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBGL/DTLBA2_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/GRWORK.CPY".
       COPY "LIBGB/GRSERCH.CPY".
       COPY "LIBGB/TIMEIO.CPY".
       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/GPENV.CPY".

      **************************************
       MISC-ROUTINES SECTION.
      **************************************
       COPY "LIBGB/GETPR.CPY".
       COPY "LIBGB/SYSTEM.CPY".
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/RMFILE.CPY".
       COPY "LIBGB/MKTEMP.CPY".
       COPY "LIBGB/GETENV.CPY".

      **************************************
       IO-ROUTINES SECTION.
      **************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGL/GLGTGS_SQL.CPY".
       COPY "LIBGL/GLGLGS_SQL.CPY".
       COPY "LIBGB/GBGRGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBPRINT.CPY".
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
                   CAST(GTFILE.GT_DATE2 AS VARCHAR(10))

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
                      :QGT-DATE2
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

       COPY "LIBGL/GLGT1RN.CPY".
       COPY "LIBGL/GLGT3RN.CPY".
      *-----------------------------------------------------------------
       READ-GT3-FILE-SUM-AMT.
           PERFORM READ-IT.
           MOVE GT-PATH-SQL TO E-FILE.
           MOVE GT3-KEY TO E-KEYX.
           INITIALIZE QGT-REC.

           EXEC SQL
            SELECT SUM(GTFILE.GT_AMT)
            INTO :WS-SUM-AMT
            FROM DBO.GTFILE
            WHERE  GTFILE.GT_BRANCH = :QGT3-WBEG-BRANCH
              AND  GTFILE.GT_ACCTNO = :QGT3-WBEG-ACCTNO
              AND  GTFILE.GT_DATE3  BETWEEN :QGT3-WBEG-DATE3 
                                        AND :QGT3-WEND-DATE3 
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-GT3-FILE-SUM-AMT.
      *-----------------------------------------------------------------
 

       COPY "LIBGB/GBWKIN.CPY".
       COPY "LIBGB/GBWK2IN.CPY".
       COPY "LIBGB/FERRORS.CPY".
            MOVE "E" TO EXT-EOBUF-ERRCD.

      **************************************
       END-PROGRAM SECTION.
      **************************************
       END-PROG.

           PERFORM CLOSE-FILES.
           IF EXT-ROUTE-BUF = "00"
              PERFORM PRU-OUT.

           IF NO-RECORDS-FOUND
              MOVE "NO RECORDS FOUND FOR SELECTED RANGE" TO MESS
              PERFORM SEND-MESS.

           MOVE MKTEMP-BUF TO ACCESS-BUF.
           PERFORM REMOVE-WORKFILE.
           MOVE WK2-PATH TO ACCESS-BUF.
           PERFORM REMOVE-WORKFILE.
       EXIT-PROG.
           IF EXT-ROUTE-BUF = "00"
              MOVE EXIT-PATHNAME TO FORM-PATHNAME.
       STOP-RUN.
           IF EXT-ROUTE-BUF = "00" OR "40" OR "50"
              DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN
              DESTROY DTLBA2-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.
           EXIT PROGRAM.
