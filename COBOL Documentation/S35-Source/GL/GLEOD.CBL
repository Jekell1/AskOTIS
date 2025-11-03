       IDENTIFICATION DIVISION.
       PROGRAM-ID. GLEOD.
      ******************************************************************
      *
      *                   (GL)
      *       G/L TRANSACTION END-OF-DAY LISTING
      *       FROM TRANLS.  WILL PRINT LOAN PROCESSING (SOURCE 3) GL
      *       TRANSACTIONS WITH A BRANCH # IN THE REFERENCE FIELD
      *       WHICH MATCHES THE BRANCH RANGE, REGARDLESS OF THE
      *       GL BRANCH OF THE TRANSACTION.
      *
      *=================================================================
      * REV:
      * KEC 2015.0608 {PR#00000} PARADATA <A30> [JAY CISZEWSKI]
      *         REMOVED THE USE OF 'INDEXED BY' ON OCCURS
      *         FOR USE WITH MICROFOCUS.
      * KEC 2016.0105 {PD#00003} PARADATA <A30> [JAY CISZEWSKI]
      *         FIXED KEYS & RECS FOR GT-REC.
      * KEC 2016.1129 {PR#00000} PARADATA <A30> [JAY CISZEWSKI]
      *          CHANGED GT-DATE1 GT-DATE2 GT-DATE3 AS ACTIVE CCYYMMDD.
      * BLM 170329 ACTIVATE 8 DIGIT DATES, PD0006
      * SMR 2023.05.15 - CHANGED GBGR1IN TO GBGR1RN
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "LIBGB/GBFSPR.CPY".
       COPY "LIBGB/GBFSWKI.CPY".

       DATA DIVISION.
       FILE SECTION.
       COPY "LIBGB/GBFDPR.CPY".

      ******************************************************************
      *
      *    W K - F I L E
      *
      *=================================================================
      *    INDEXED WORK FILE
      ******************************************************************
       FD  WK-FILE
           LABEL RECORD IS STANDARD.
       01  WK-REC.
           03  WK-KEY.
               05  WK-COMMON-KEY               PIC X(35).
               05  WK-UNIQUE-KEY               PIC 9(6).
               05  WK-GT1-KEY.
                   07  WK-GT-SOURCE            PIC 9(01).
                   07  WK-GT-DATE2             PIC 9(08).
                   07  WK-GT3-KEY.
                       09  WK-GT-BRANCH        PIC 9(04).
                       09  WK-GT-ACCTNO        PIC 9(08).
                       09  WK-GT-DATE3         PIC 9(08).
                       09  WK-GT-DRCRFG        PIC X(01).
                       09  WK-GT-LINE          PIC 9(06).

      ******************************************************************
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(42) VALUE
           "@(#)./GL/GLEOD.CBL S35 05/15/23-08:42:08 >".

       COPY "LIBGL/GL01GT.CPY".
       COPY "LIBGL/GL01GT_SQL.CPY".
       COPY "LIBGL/GLWSGT.CPY".
       COPY "LIBGL/GL01GL.CPY".
       COPY "LIBGL/GL01GL_SQL.CPY".
       COPY "LIBGL/GLWSGL.CPY".
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBGB/GB01GR.CPY".
       COPY "LIBGB/GB01GR_SQL.CPY".
       COPY "LIBGB/GBWSGR.CPY".

       01  PR-IFN                      PIC X(3)      VALUE "PR".
       01  WK-IFN                      PIC X(3)      VALUE "WK".

       01  LN-WORKERS.
           03  LN-FEED        PIC 999   VALUE 0.
           03  LN-COUNT       PIC 999   VALUE 99.
           03  LN-WK          PIC 999   VALUE 0.
       01  PAGE-NUMBER        PIC 999   VALUE 0.

       01  WORK-COMMON-KEY.
           03  WORK-COMMON-AREA        PIC X(35).
           03  WORK-BRACCT-AREA REDEFINES WORK-COMMON-AREA.
               05  WORK-BRACCT-BRANCH  PIC 9(4).
               05  WORK-BRACCT-ACCTNO  PIC 9(7).
               05  WORK-BRACCT-DATE    PIC 9(8).
               05  WORK-BRACCT-LINE    PIC 9(6).
               05  FILLER              PIC X(10).
           03  WORK-DATE-AREA REDEFINES WORK-COMMON-AREA.
               05  WORK-DATE-DATE      PIC 9(8).
               05  WORK-DATE-BRANCH    PIC 9(4).
               05  WORK-DATE-ACCTNO    PIC 9(7).
               05  WORK-DATE-LINE      PIC 9(6).
               05  FILLER              PIC X(10).
           03  WORK-SOURCE-AREA REDEFINES WORK-COMMON-AREA.
               05  WORK-SOURCE         PIC 9.
               05  WORK-SOURCE-DATE    PIC 9(8).
               05  WORK-SOURCE-BRANCH  PIC 9(4).
               05  WORK-SOURCE-ACCTNO  PIC 9(7).
               05  WORK-SOURCE-LINE    PIC 9(6).
               05  FILLER              PIC X(9).
           03  WORK-REF-AREA REDEFINES WORK-COMMON-AREA.
               05  WORK-REF            PIC X(14).
               05  WORK-REF-DATE       PIC 9(8).
               05  WORK-REF-BRANCH     PIC 9(4).
               05  WORK-REF-ACCTNO     PIC 9(7).
               05  FILLER              PIC X(2).

       77  ACCUM-TOT-DEBIT    PIC S9(10)V99 COMP-3 VALUE +0.
       77  ACCUM-TOT-CREDIT   PIC S9(10)V99 COMP-3 VALUE +0.
       77  ACCUM-REF-DEBIT    PIC S9(10)V99 COMP-3 VALUE +0.
       77  ACCUM-REF-CREDIT   PIC S9(10)V99 COMP-3 VALUE +0.

       01  TEST-REF.
           03  FILLER         PIC X(10).
           03  TEST-REF-BRNO  PIC 9(4).
       01  HOLD-REF-COMPARE   PIC X(14).

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *  KEC 2015.0608 REMOVED 'INDEXED BY' FOR 'OCCURS'
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *01  REF-WORK.
      *    03  REF-WK-TAB     PIC X
      *                       OCCURS 14
      *                       INDEXED BY REFWK.
      *01  REF-TABLE.
      *    03  REF-TAB        PIC X
      *                       OCCURS 14
      *                       INDEXED BY REFTAB.
      *
      *01  REF-GT-TABLE.
      *    03  REF-GT-TAB     PIC X
      *                       OCCURS 14
      *                       INDEXED BY REFGTTAB.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

       01  HOLD-UNIQUE-KEY    PIC 9(6).
       01  HOLD-GT-DATE1      PIC 9(8).
       01  HOLD-BR-ACCOUNT.
           03  HOLD-BRANCH    PIC 9(4).
           03  FILLER         PIC X     VALUE "-".
           03  HOLD-ACCOUNT   PIC 9(7).

       01  HOLD-REF           PIC X(14) VALUE "ZZZZZZZZZZZZZZ".
       01  HOLD-SOURCE        PIC X     VALUE "9".

       01  FIRST-TIME         PIC X VALUE "Y".
       01  RECORDS-FOUND      PIC X VALUE " ".
           88  NO-RECORDS-FOUND     VALUE "N".

       01  MESS-LIST          PIC X(5) VALUE "MESS.".

       COPY "LIBGB/EOBUF_EXT.CPY".
           03  PRORDER                PIC 9.
           03  BEG-DATE               PIC 9(8).
           03  END-DATE               PIC 9(8).
           03  BEG-BR                 PIC 9(4).
           03  BEG-GLNO               PIC 9(7).
           03  END-BR                 PIC 9(4).
           03  END-GLNO               PIC 9(7).
           03  GROUP-FLAG             PIC X.
           03  ENT-GROUP              PIC X(4).
           03  POST-GT-FLAG           PIC X.

       01  VERSION-CONTROL    PIC X VALUE "A".

       01  SV-BEG-BR          PIC 9(4)   VALUE 9999.
       01  SV-END-BR          PIC 9(4)   VALUE 9999.
       01  SV-GROUP-FLAG      PIC X      VALUE SPACES.
       01  SV-ENT-GROUP       PIC X(4)   VALUE SPACES.

       01  HLINE-1.
           03  HD-DATE        PIC X(8).
           03  FILLER         PIC X(3)    VALUE SPACES.
           03  HD-TIME        PIC X(8)    VALUE SPACES.
           03  FILLER         PIC X(30)   VALUE SPACES.
           03  HD-CONAME      PIC X(40).
           03  FILLER         PIC X(34)   VALUE SPACES.
           03  FILLER         PIC X(5)    VALUE "PAGE ".
           03  HD-PAGE        PIC ZZ9.

       01  HLINE-2.
           03  FILLER         PIC X(9)    VALUE "USERID:".
           03  HD-USERID      PIC X(10)   VALUE SPACES.
           03  FILLER         PIC X(30)   VALUE SPACES.
           03  H-TITLE        PIC X(33)   VALUE
               "END-OF-DAY GL TRANSACTION LISTING".

       01  HLINE-1A.
           03  FILLER         PIC X(44) VALUE SPACES.
           03  FILLER         PIC X(17) VALUE
               "BEGINNING DATE:  ".
           03  HD-BEG-DATE    PIC 99/99/99.
           03  FILLER         PIC X(2)  VALUE SPACES.
           03  FILLER         PIC X(13) VALUE "ENDING DATE  ".
           03  HD-END-DATE    PIC 99/99/99.

           03  HD-SRC-LIT1    PIC X(9)  VALUE SPACES.
           03  HD-SRC-TYPE    PIC Z.
           03  HD-SRC-LIT2    PIC X(5)  VALUE SPACES.
       01  HLINE-2A.
           03  FILLER         PIC X(14) VALUE "SOURCE TYPES:".
           03  FILLER         PIC X(36) VALUE
           "1-MANUAL ENTRY  2-ACCOUNTS PAYABLE  ".
           03  FILLER         PIC X(36) VALUE
           "3-LOAN PROCESSING  5-NOTES PAYABLE  ".
       01  HLINE-3.
           03  FILLER         PIC X(11)   VALUE "REFERENCE  ".
           03  FILLER         PIC X(5)    VALUE "     ".
           03  FILLER         PIC X(12)   VALUE "GL ACCT # ".
           03  FILLER         PIC X(30)   VALUE
               "DESC          SRC UPD/PER ".
           03  FILLER         PIC X(13) VALUE "    DEBIT  ".
           03  FILLER         PIC X(14) VALUE "      CREDIT ".
           03  FILLER         PIC X(34)   VALUE
               "EXPLANATION".
           03  FILLER         PIC X(5)    VALUE " DATE".
           03  FILLER         PIC X(8)   VALUE "    LINE".

       01  DETAIL-LINE           VALUE SPACES.
           03  D-REFERENCE    PIC X(14).
           03  FILLER         PIC X.
           03  D-BRANCH       PIC ZZZ9.
           03  D-DASH         PIC X.
           03  D-ACCTNO       PIC 9999999.
           03  FILLER         PIC X.
           03  D-DESC         PIC X(15).
           03  FILLER         PIC X.
           03  D-SOURCE       PIC X.
           03  FILLER         PIC X(2).
           03  D-UPDATE       PIC X.
           03  FILLER         PIC X(2).
           03  D-PERIOD       PIC ZZ.
           03  FILLER         PIC X.
           03  D-DEBIT-AMT    PIC ZZZ,ZZZ,ZZ9.99-.
           03  FILLER         PIC X.
           03  D-CREDIT-AMT   PIC ZZZ,ZZZ,ZZ9.99-.
           03  FILLER         PIC X  .
           03  D-EXPL         PIC X(32).
           03  FILLER         PIC X.
           03  D-DATE         PIC 99/99/99.
           03  D-LINE-NO      PIC ZZZZZ9.

       01  RANGE-LINE REDEFINES DETAIL-LINE.
           03  RANGE-DESC         PIC X(33).
           03  RANGE-SEL1         PIC X(20).
           03  FILLER REDEFINES RANGE-SEL1.
               05  RANGE-DATE1    PIC 9999/99/99.
               05  FILLER         PIC X(10).
           03  FILLER             PIC X(5).
           03  RANGE-SEL2         PIC X(40).
           03  FILLER REDEFINES RANGE-SEL2.
               05  RANGE-DATE2    PIC 9999/99/99.
               05  FILLER         PIC X(30).
           03  FILLER             PIC X(34).

       01  TOTAL-LINE.
           03  FILLER         PIC X(25)   VALUE SPACES.
           03  TOT-TYPE       PIC X(14)   VALUE "         ".
           03  FILLER         PIC X(12)   VALUE "TOTAL:".
           03  TOT-DEBIT-AMT  PIC ZZZZZ,ZZZ,ZZ9.99-.
           03  TOT-CREDIT-AMT PIC ZZZZZ,ZZZ,ZZ9.99-.


       COPY "LIBWI/GRSCANW.CPY".
       COPY "LIBGB/SYSTEMW.CPY".
       COPY "LIBGB/GRWORKW.CPY".
       COPY "LIBGB/MKTEMPW_CMD.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/GETPRW.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBGL/GLEOD_DEF.CPY".
       COPY "LIBGL/GLEOD_WKS.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".
       COPY "LIBGB/GETFMW.CPY".

       SCREEN SECTION.
       COPY "LIBGL/GLEOD_SCN.CPY".

       PROCEDURE DIVISION
            USING FORM-PATHNAME EXIT-PATHNAME.
       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
              WK-FILE PR-FILE.
       COPY "LIBGB/DECLRP.CPY".
           PERFORM CLOSE-GT2-FILE.
           PERFORM CLOSE-GT1-FILE.
           PERFORM CLOSE-GL1-FILE.
           PERFORM CLOSE-GR1-FILE.
           PERFORM CLOSE-BR-FILE.
           PERFORM CLOSE-PR-FILE.
           CLOSE WK-FILE.
       COPY "LIBGB/DECLRP2.CPY".

      *********************************************
       MAIN-PROGRAM SECTION.
      *********************************************
           PERFORM SQL-CONNECT.
           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "GLEOD" TO VDU-FORM-NAME.

      D    STOP MESS.
           MOVE FORM-PATHNAME TO FORM-PATH.

           PERFORM INITIALIZATION.
           IF EXT-EOBUF-ERRCD NOT = " "
              GO TO END-PROGRAM.

           IF GROUP-FLAG = "Y"
              PERFORM OPEN-GR1-FILE
              MOVE ENT-GROUP TO GR-ID
              PERFORM SEARCH-GR-INIT.

           PERFORM CREATE-GT-SORT-DETAIL.

           PERFORM OPEN-GT1-FILE.
           PERFORM OPEN-WK-FILE.
           MOVE LOW-VALUES TO WK-KEY.
           PERFORM START-WK-FILE.

       NEXT-WK-RECORD.
           PERFORM READ-WK-FILE-NEXT.
           IF IO-BAD
              GO TO END-ROUTINE.

           MOVE WK-GT1-KEY TO GT1-KEY.
           PERFORM READ-GT1-FILE.
           IF IO-BAD
              GO TO IO-ERROR.

           IF FIRST-TIME = "Y"
              MOVE GT-SOURCE       TO HOLD-SOURCE
              MOVE GT-REF          TO HOLD-REF
              MOVE GT-DATE1        TO HOLD-GT-DATE1
              MOVE GT-BRANCH       TO HOLD-BRANCH
              MOVE GT-ACCTNO       TO HOLD-ACCOUNT.

           IF PRORDER = 1
              IF GT-SOURCE NOT = HOLD-SOURCE
                 PERFORM PRINT-SUB-TOTALS
                 MOVE GT-SOURCE     TO HOLD-SOURCE.

           IF PRORDER = 2
              IF GT-REF NOT = HOLD-REF
                 PERFORM PRINT-SUB-TOTALS
                 MOVE GT-REF       TO HOLD-REF.

           IF PRORDER = 3
              IF GT-DATE1 NOT = HOLD-GT-DATE1
                 PERFORM PRINT-SUB-TOTALS
                 MOVE GT-DATE1     TO HOLD-GT-DATE1.

           IF PRORDER = 4
              IF GT-BRANCH NOT = HOLD-BRANCH
                 OR GT-ACCTNO NOT = HOLD-ACCOUNT
                    PERFORM PRINT-SUB-TOTALS
                   MOVE GT-BRANCH  TO HOLD-BRANCH
                   MOVE GT-ACCTNO  TO HOLD-ACCOUNT.

           PERFORM PROCESS-GT-DETAIL.

           GO TO NEXT-WK-RECORD.

       END-ROUTINE.
           IF FIRST-TIME = "Y"
              MOVE "N" TO RECORDS-FOUND
              GO TO END-PROGRAM.

           PERFORM PRINT-SUB-TOTALS.

           PERFORM PRINT-GRAND-TOTALS.

           IF EXT-EOBUF-GROUP-FG NOT = "Y"
              PERFORM PRINT-RANGE-LINES.

           GO TO END-PROGRAM.


      *********************************************
       CREATE-GT-SORT-DETAIL SECTION.
      *********************************************
       NEXT-GT-RECORD.
           PERFORM READ-GT2-FILE-NEXT.
           IF IO-BAD
              GO TO EXIT-PROCESS-GT.

           IF GT-DATE1 > END-DATE
              GO TO EXIT-PROCESS-GT.

           PERFORM TEST-GT-FLAG.
           IF IO-FG = 9
              GO TO NEXT-GT-RECORD.

      * TEST G/L ACCOUNT NUMBER
           IF GT-ACCTNO < BEG-GLNO OR GT-ACCTNO > END-GLNO
              GO TO NEXT-GT-RECORD.

      * TEST BRANCH RANGE USING REF CODE (LP-YYMMDD-BBBB)
           MOVE GT-REF          TO TEST-REF.
           IF GROUP-FLAG = "Y"
              MOVE TEST-REF-BRNO TO WGR-SRESULT
              PERFORM SEARCH-GR-VERIFY
              IF WGR-ERR > 0
                 GO TO NEXT-GT-RECORD.
           IF GROUP-FLAG NOT = "Y"
              IF TEST-REF-BRNO < BEG-BR OR TEST-REF-BRNO > END-BR
                 GO TO NEXT-GT-RECORD.

      * ONLY INCLUDE SOURCE CODE 3 (LOAN PROCESSING) FOR EOD
           IF GT-SOURCE NOT = 3
              GO TO NEXT-GT-RECORD.

           PERFORM CREATE-WK-RECORD.

           GO TO NEXT-GT-RECORD.
       EXIT-PROCESS-GT.
           PERFORM CLOSE-GT2-FILE.


      *********************************************
       PROCESS-GT-DETAIL SECTION.
      *********************************************

           MOVE GT-REF             TO D-REFERENCE.
           MOVE GT-BRANCH          TO D-BRANCH.
           MOVE "-"                TO D-DASH.
           MOVE GT-ACCTNO          TO D-ACCTNO.
           MOVE GT-SOURCE          TO D-SOURCE.
           MOVE GT-FLAG            TO D-UPDATE.
           MOVE GT-PERIOD          TO D-PERIOD.

           MOVE GT-BRANCH          TO GL-BRANCH.
           MOVE GT-ACCTNO          TO GL-ACCOUNT.
           PERFORM READ-GL1-FILE.
           IF IO-FG = 0
              MOVE GL-DESCRIPTION  TO D-DESC
           ELSE
              MOVE "-NOT ON FILE " TO D-DESC.

           IF GT-AMT < 0
              ADD GT-AMT           TO ACCUM-TOT-CREDIT
              ADD GT-AMT           TO ACCUM-REF-CREDIT
              MOVE GT-AMT          TO D-CREDIT-AMT
           ELSE
              ADD GT-AMT           TO ACCUM-TOT-DEBIT
              ADD GT-AMT           TO ACCUM-REF-DEBIT
              MOVE GT-AMT          TO D-DEBIT-AMT.

           MOVE GT-LINE            TO D-LINE-NO.
           MOVE GT-EXPL            TO D-EXPL.

           MOVE GT-DATE1            TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY         TO D-DATE.

           PERFORM WRITE-DETAIL-LINE.

      *********************************************
       HEAD-PAGE SECTION.
      *********************************************
           MOVE "N" TO FIRST-TIME.
           ADD 1 TO PAGE-NUMBER.
           MOVE PAGE-NUMBER TO HD-PAGE.
           PERFORM GET-TIME.
           MOVE TIME-EDIT TO HD-TIME.

           MOVE HLINE-1 TO PR-REC.
           MOVE 99 TO LN-FEED.
           PERFORM WRITE-PR-REC.

           MOVE HLINE-2 TO PR-REC.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-PR-REC.

           MOVE HLINE-1A TO PR-REC.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-PR-REC.

           MOVE HLINE-2A TO PR-REC.
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-PR-REC.

           MOVE HLINE-3 TO PR-REC.
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-PR-REC.

           MOVE SPACES TO PR-REC.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-PR-REC.

           MOVE 8 TO LN-COUNT.
           MOVE 2 TO LN-FEED.


      *********************************************
       WRITE-DETAIL-LINE SECTION.
      *********************************************
           ADD LN-FEED LN-COUNT GIVING LN-WK.
           IF LN-WK > EXT-CONAME-RPT-LINES
              PERFORM HEAD-PAGE.
           MOVE DETAIL-LINE TO PR-REC.
           PERFORM WRITE-PR-REC.
           ADD LN-FEED TO LN-COUNT.
           MOVE 1 TO LN-FEED.
           MOVE SPACES TO DETAIL-LINE.

      *********************************************
       PRINT-SUB-TOTALS SECTION.
      *********************************************
           IF PRORDER = 1
              GO TO SUB-EXIT.

           MOVE ACCUM-REF-DEBIT  TO TOT-DEBIT-AMT.
           MOVE ACCUM-REF-CREDIT TO TOT-CREDIT-AMT.

           MOVE 0 TO ACCUM-REF-DEBIT ACCUM-REF-CREDIT.

           IF PRORDER = 1
              MOVE "   SOURCE"      TO TOT-TYPE.
           IF PRORDER = 2
              MOVE "REFERENCE"      TO TOT-TYPE.
           IF PRORDER = 3
              MOVE HOLD-GT-DATE1    TO DATE-YYYYMMDD
              PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
              MOVE DATE-MMDDYY      TO ALP-DATE-EDIT
              MOVE ALP-DATE-EDIT    TO TOT-TYPE.
           IF PRORDER = 4
              MOVE HOLD-BR-ACCOUNT  TO TOT-TYPE.

           ADD LN-FEED LN-COUNT GIVING LN-WK.
           IF LN-WK > EXT-CONAME-RPT-LINES
              PERFORM HEAD-PAGE.

           MOVE TOTAL-LINE TO PR-REC.
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-PR-REC.

           ADD LN-FEED TO LN-COUNT.

           MOVE SPACES TO PR-REC.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-PR-REC.
           ADD LN-FEED TO LN-COUNT.

       SUB-EXIT.
           EXIT.

      *********************************************
       PRINT-GRAND-TOTALS SECTION.
      *********************************************
           MOVE ACCUM-TOT-DEBIT  TO TOT-DEBIT-AMT.
           MOVE ACCUM-TOT-CREDIT TO TOT-CREDIT-AMT.

           MOVE 0 TO ACCUM-TOT-DEBIT ACCUM-TOT-CREDIT
                     ACCUM-REF-DEBIT ACCUM-REF-CREDIT.

           MOVE SPACES TO TOT-TYPE.

           ADD LN-FEED LN-COUNT GIVING LN-WK.
           IF LN-WK > EXT-CONAME-RPT-LINES
              PERFORM HEAD-PAGE.

           MOVE TOTAL-LINE TO PR-REC.
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-PR-REC.

      ****************************************
       INITIALIZATION SECTION.
      ****************************************
           MOVE GLEOD-QUEUE-POS TO Q-POS.
           MOVE GLEOD-COPIES-POS TO C-POS.
           MOVE GLEOD-PRU-POS   TO P-POS.
           MOVE SPACES TO Q-BUF C-BUF P-BUF.

       COPY "LIBGB/EOINIT.CPY".

           MOVE "EOD G/L TRANSACTION LISTING IN PROGRESS..." TO MESS.
           MOVE MESS-LIST TO WR-LIST.
           MOVE MESS TO WR-BUF.
           PERFORM CALL-WRFORM.

           MOVE BEG-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO HD-BEG-DATE.
           MOVE END-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO HD-END-DATE.

           MOVE EXT-CONAME-CONAME TO HD-CONAME.
           MOVE EXT-CONAME-SYSDATE TO HD-DATE.
           MOVE EXT-CONAME-USER-ID TO HD-USERID.

           MOVE 3 TO GT-SOURCE HD-SRC-TYPE.
           MOVE "  SOURCE " TO HD-SRC-LIT1.
           MOVE " ONLY" TO HD-SRC-LIT2.

           IF EXT-ROUTE-BUF = "70"
              MOVE EXT-EOBUF-MESS TO H-TITLE.

           IF EXT-EOBUF-GROUP-FG = "Y"
              MOVE BEG-BR TO SV-BEG-BR
              MOVE END-BR TO SV-END-BR
              MOVE GROUP-FLAG TO SV-GROUP-FLAG
              MOVE ENT-GROUP TO SV-ENT-GROUP
              MOVE EXT-EOBUF-GROUP-PRPATH TO PRU-FILE
              IF EXT-EOBUF-BRNO NUMERIC
                 MOVE EXT-EOBUF-BRNO TO BEG-BR END-BR
                 MOVE "N" TO GROUP-FLAG
              ELSE
                 MOVE EXT-EOBUF-GROUP TO ENT-GROUP
                 MOVE 0 TO BEG-BR
                 MOVE 9999 TO END-BR
                 MOVE "Y" TO GROUP-FLAG
              END-IF
           ELSE
              IF ( BEG-BR = 0 AND END-BR = 0 )
                 MOVE BEG-BR TO SV-BEG-BR
                 MOVE END-BR TO SV-END-BR
                 MOVE EXT-CONAME-USER-LOC TO BEG-BR END-BR.

           PERFORM OPEN-GL1-FILE.
           PERFORM OPEN-GT2-FILE.

           MOVE MKTEMP-DEFAULT TO MKTEMP-BUF.
           PERFORM MKTEMP-CALL.
           PERFORM OPEN-WK-FILE-OUTPUT.
           PERFORM CLOSE-WK-FILE.
           PERFORM OPEN-WK-FILE.

           PERFORM OPEN-PR-FILE.

           IF BEG-DATE = 0
              MOVE 19000101 TO BEG-DATE.
           IF END-DATE = 0
              MOVE 19000101 TO END-DATE.
           IF END-DATE = 99999999
              MOVE 99991231 TO END-DATE.

           MOVE SPACES TO GT1-KEY.
           MOVE 0 TO GT-DATE1 GT-SOURCE GT-DATE2 GT-BRANCH
                     GT-ACCTNO GT-DATE3 GT-LINE.

           MOVE BEG-DATE TO GT-DATE1 SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO QGT2-WBEG-DATE1.
           MOVE END-DATE TO SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO QGT2-WEND-DATE1.

           PERFORM START-GT2-FILE.

           GO TO EXIT-INITIALIZE.

       ENDIT.
           MOVE "X" TO EXT-EOBUF-ERRCD.
       EXIT-INITIALIZE.
           EXIT.

      ***********************************************
       EO-EXEC-AUDIT SECTION.
      ***********************************************
           MOVE EXT-EOBUF-DATE1 TO BEG-DATE.
           MOVE EXT-EOBUF-DATE2 TO END-DATE.

      ***********************************************
       ENTER-PARAMETERS SECTION.
      ***********************************************

           IF UP-KEY
              GO TO E-POST-FLAG.

       GET-PRORDER.
           MOVE "ENNN010PRORDER." TO VDU.
           PERFORM SCREENIO.
           IF F1-KEY GO TO EXIT-PARM.
           IF F7-KEY GO TO EXIT-PARM.
           IF NOT ENTER-KEY GO TO GET-PRORDER.
           MOVE VDUNUM0 TO PRORDER.
           IF PRORDER < 1 OR > 4
              GO TO GET-PRORDER.

       GET-BDT.
           IF EXT-ROUTE-BUF = "40" OR "50"
              GO TO GET-BEG-BR.
           MOVE "ERNM060BEGDATE." TO VDU
           PERFORM SCREENIO.
           IF F1-KEY GO TO EXIT-PARM.
           IF UP-KEY GO TO GET-PRORDER.
           IF F3-KEY GO TO ALLDT.
           IF F7-KEY GO TO EXIT-PARM.
           IF NOT ENTER-KEY GO TO GET-BDT.
           MOVE VDU-DATE-YYYYMMDD TO BEG-DATE.
       GET-EDT.
           MOVE "ERNM060ENDDATE." TO VDU
           PERFORM SCREENIO.
           IF F1-KEY GO TO EXIT-PARM.
           IF UP-KEY GO TO GET-BDT.
           IF F7-KEY GO TO EXIT-PARM.
           IF NOT ENTER-KEY GO TO GET-EDT.
           MOVE VDU-DATE-YYYYMMDD TO END-DATE.
           GO TO GET-BEG-BR.
       ALLDT.
           MOVE "S  A080BEGDATE." TO VDU.
           MOVE 0          TO BEG-DATE VDU-DATE-EDIT.
           PERFORM SCREENIO.
           MOVE "S  A080ENDDATE." TO VDU.
           MOVE 99999999 TO END-DATE VDU-DATE-EDIT.
           PERFORM SCREENIO.

       GET-BEG-BR.
           MOVE "E RA000BEGBR." TO VDU.
           PERFORM SCREENIO.
           IF F1-KEY GO TO EXIT-PARM.
           IF F3-KEY GO TO ALL-BRS.
           IF UP-KEY GO TO GET-EDT.
           IF F7-KEY GO TO EXIT-PARM.

           IF F6-KEY
              MOVE "WI/GRSCAN" TO FORM-NAM
              CALL FORM-PROGX USING FORM-PATH GRSCAN-BUF EXIT-PATHNAME
              CANCEL FORM-PROGX
              IF GRSCAN-STATUS NOT = "00"
                 GO TO GET-BEG-BR
              ELSE
                 MOVE "S  A000BEGBR." TO VDU
                 MOVE GRSCAN-KEY TO VDUBUF
                 PERFORM SCREENIO
                 MOVE GRSCAN-KEY TO VDUBUF
                 MOVE "00" TO VDUSTATUS.
           IF NOT ENTER-KEY
              GO TO GET-BEG-BR.

           MOVE VDUBUF TO WGR-WORD ENT-GROUP.

           IF WGR-WORD-ZERO
              MOVE 0 TO WGR-SRESULT
              GO TO BRANCH-IS-NUMERIC.

           MOVE 0 TO WGR-MIN-LEVEL.
           PERFORM CHECK-MEMBER.
           IF WGR-ERR > 0
              MOVE WGR-MSG(WGR-ERR) TO MESS
              PERFORM SEND-MESS
              GO TO GET-BEG-BR.
           IF WGR-TYPE = "A"
              MOVE "S  A000ENDBR." TO VDU
              MOVE SPACES TO VDUBUF
              PERFORM SCREENIO
              MOVE "S  A160DISPLAY-USER." TO VDU
              MOVE SPACES TO VDUBUF
              PERFORM SCREENIO
              MOVE "Y" TO GROUP-FLAG
              MOVE 0 TO BEG-BR
              MOVE 9999 TO END-BR
              GO TO GET-BEG-GLNO.

       BRANCH-IS-NUMERIC.
           MOVE "N" TO GROUP-FLAG.
           MOVE WGR-SRESULT TO BEG-BR.

           MOVE "SN N040BEGBR." TO VDU.
           MOVE BEG-BR TO VDUNUM0.
           PERFORM SCREENIO.

        GET-END-BR.
           MOVE "ENNN040ENDBR." TO VDU.
           PERFORM SCREENIO.
           IF F1-KEY GO TO EXIT-PARM.
           IF F3-KEY GO TO ALL-BRS.
           IF UP-KEY GO TO GET-BEG-BR.
           IF F7-KEY GO TO EXIT-PARM.
           IF NOT ENTER-KEY GO TO GET-END-BR.
           MOVE VDUNUM0 TO END-BR.
           IF BEG-BR > END-BR
              MOVE "INVALID BRANCH RANGE" TO MESS
              PERFORM SEND-MESS
              GO TO GET-BEG-BR.

           IF ( (BEG-BR = 0) AND (END-BR = 0) )
              MOVE "S  A160DISPLAY-USER." TO VDU
              MOVE "USER BRANCH ONLY" TO VDUBUF
              PERFORM SCREENIO
           ELSE
              MOVE "S  A160DISPLAY-USER." TO VDU
              MOVE SPACES TO VDUBUF
              PERFORM SCREENIO.

           GO TO GET-BEG-GLNO.
       ALL-BRS.
           MOVE "SNNN040BEGBR." TO VDU.
           MOVE 1 TO BEG-BR VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN040ENDBR." TO VDU.
           MOVE 9999 TO END-BR VDUNUM0.
           PERFORM SCREENIO.
           MOVE "S NA160DISPLAY-USER." TO VDU.
           MOVE SPACES TO VDUBUF.
           PERFORM SCREENIO.
           MOVE "N" TO GROUP-FLAG.

       GET-BEG-GLNO.
           MOVE "ENNN070BEGACT." TO VDU.
           PERFORM SCREENIO.
           IF F1-KEY GO TO EXIT-PARM.
           IF F3-KEY GO TO SEND-ALL-GLNO.
           IF UP-KEY GO TO GET-END-BR.
           IF F7-KEY GO TO EXIT-PARM.
           IF NOT ENTER-KEY GO TO GET-BEG-GLNO.
           MOVE VDUNUM0 TO BEG-GLNO.
       GET-END-GLNO.
           MOVE "ENNN070ENDACT." TO VDU.
           PERFORM SCREENIO.
           IF F1-KEY GO TO EXIT-PARM.
           IF UP-KEY GO TO GET-BEG-GLNO.
           IF F7-KEY GO TO EXIT-PARM.
           IF NOT ENTER-KEY GO TO GET-END-GLNO.
           MOVE VDUNUM0 TO END-GLNO.
           IF BEG-GLNO > END-GLNO
              MOVE "INVALID ACCOUNT RANGE" TO MESS
              PERFORM SEND-MESS
              GO TO GET-BEG-GLNO.
           GO TO E-POST-FLAG.

       SEND-ALL-GLNO.
           MOVE "SNNN070BEGACT." TO VDU.
           MOVE 1 TO BEG-GLNO VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN070ENDACT." TO VDU.
           MOVE 9999999 TO END-GLNO VDUNUM0.
           PERFORM SCREENIO.

       E-POST-FLAG.
           MOVE "E  A000GTPOST." TO VDU.
           PERFORM SCREENIO.
           IF F1-KEY GO TO EXIT-PARM.
           IF UP-KEY GO TO GET-END-GLNO.
           IF F7-KEY GO TO EXIT-PARM.
           IF NOT ENTER-KEY GO TO GET-END-GLNO.
           MOVE VDUBUF TO POST-GT-FLAG.
           IF POST-GT-FLAG = "S" GO TO EXIT-PARM.
           IF POST-GT-FLAG = "I" GO TO EXIT-PARM.
           IF POST-GT-FLAG = "O" GO TO EXIT-PARM.
           GO TO E-POST-FLAG.

       EXIT-PARM.
           EXIT.

      *********************************************
       DISPLAY-PARAMETERS SECTION.
      *********************************************
           MOVE "SNNN010PRORDER." TO VDU.
           MOVE PRORDER TO VDUNUM0.
           PERFORM SCREENIO.

           IF GROUP-FLAG = "Y"
              MOVE "S  A000BEGBR." TO VDU
              MOVE ENT-GROUP TO VDUBUF
              PERFORM SCREENIO
              MOVE "S  A000ENDBR." TO VDU
              MOVE SPACES TO VDUBUF
              PERFORM SCREENIO
           ELSE
              MOVE "SNNN040BEGBR." TO VDU
              MOVE BEG-BR TO VDUNUM0
              PERFORM SCREENIO
              MOVE "SNNN040ENDBR." TO VDU
              MOVE END-BR TO VDUNUM0
              PERFORM SCREENIO.

           IF ( (BEG-BR = 0) AND (END-BR = 0) )
              MOVE "S  A160DISPLAY-USER." TO VDU
              MOVE "USER BRANCH ONLY" TO VDUBUF
              PERFORM SCREENIO
           ELSE
              MOVE "S  A160DISPLAY-USER." TO VDU
              MOVE SPACES TO VDUBUF
              PERFORM SCREENIO.

           MOVE "SNNN070BEGACT." TO VDU.
           MOVE BEG-GLNO TO VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN070ENDACT." TO VDU.
           MOVE END-GLNO TO VDUNUM0.
           PERFORM SCREENIO.

      *********************************************
       CREATE-WK-RECORD SECTION.
      *********************************************

           MOVE 0 TO HOLD-UNIQUE-KEY.

       CREATE-NEW-WK.
           MOVE SPACES             TO WK-REC.
           PERFORM SETUP-WK-COMMON-KEY.
           MOVE HOLD-UNIQUE-KEY    TO WK-UNIQUE-KEY.
           MOVE GT1-KEY            TO WK-GT1-KEY.
           PERFORM WRITE-WK-FILE.
           IF IO-BAD
              IF E-CODE = "22"
                 ADD 1 TO HOLD-UNIQUE-KEY
                 GO TO CREATE-NEW-WK
              ELSE
                 GO TO IO-ERROR.

       EXIT-CREATE-WK-REC.
           EXIT.

      **************************************************
       SETUP-WK-COMMON-KEY SECTION.
      **************************************************
           MOVE SPACES TO WORK-COMMON-KEY.
           IF PRORDER = 1
              MOVE GT-SOURCE        TO WORK-SOURCE
              MOVE GT-DATE1         TO WORK-SOURCE-DATE
              MOVE GT-BRANCH        TO WORK-SOURCE-BRANCH
              MOVE GT-ACCTNO        TO WORK-SOURCE-ACCTNO
              MOVE GT-LINE          TO WORK-SOURCE-LINE.
           IF PRORDER = 2
              MOVE GT-REF           TO WORK-REF
              MOVE GT-DATE1         TO WORK-REF-DATE
              MOVE GT-BRANCH        TO WORK-REF-BRANCH
              MOVE GT-ACCTNO        TO WORK-REF-ACCTNO.
           IF PRORDER = 3
              MOVE GT-DATE1         TO WORK-DATE-DATE
              MOVE GT-BRANCH        TO WORK-DATE-BRANCH
              MOVE GT-ACCTNO        TO WORK-DATE-ACCTNO
              MOVE GT-LINE          TO WORK-DATE-LINE.
           IF PRORDER = 4
              MOVE GT-BRANCH        TO WORK-BRACCT-BRANCH
              MOVE GT-ACCTNO        TO WORK-BRACCT-ACCTNO
              MOVE GT-DATE1         TO WORK-BRACCT-DATE
              MOVE GT-LINE          TO WORK-BRACCT-LINE.

           MOVE WORK-COMMON-KEY TO WK-COMMON-KEY.
       EXIT-SETUP-WK-KEY.
           EXIT.

      ****************************************************
      *  TEST OPTION FLAG:
      *     "S" SKIP    IF GT-RECORD POSTED TO GL MASTER
      *     "I" INCLUDE IF GT-RECORD POSTED TO GL MASTER
      *     "O" ONLY    IF GT-RECORD POSTED TO GL MASTER
      ****************************************************
       TEST-GT-FLAG SECTION.
      **************************************************
           MOVE 0 TO IO-FG.
           IF POST-GT-FLAG = "S"
              IF GT-FLAG NOT = SPACES
                  MOVE 9 TO IO-FG.
           IF POST-GT-FLAG = "O"
              IF GT-FLAG = SPACES
                  MOVE 9 TO IO-FG.

      **************************************************
       PRINT-RANGE-LINES SECTION.
      **************************************************
           MOVE "PRINT REPORT ORDER BY          :"    TO RANGE-DESC.
           MOVE PRORDER                               TO RANGE-SEL1.
           IF PRORDER = 1
              MOVE "SOURCE/DATE/ACCOUNT#" TO RANGE-SEL2
           ELSE
           IF PRORDER = 2
              MOVE "REF/DATE/ACCOUNT#   " TO RANGE-SEL2
           ELSE
           IF PRORDER = 3
              MOVE "DATE/ACCOUNT#/LINE  " TO RANGE-SEL2
           ELSE
           IF PRORDER = 4
              MOVE "ACCOUNT#/DATE/LINE  " TO RANGE-SEL2.
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "BEG/ENDING DATE RANGE          :" TO RANGE-DESC.
           MOVE BEG-DATE                           TO RANGE-DATE1.
           MOVE END-DATE                           TO RANGE-DATE2.
           PERFORM WRITE-DETAIL-LINE.

           IF GROUP-FLAG = "Y"
              MOVE "GROUP SELECTED                 :" TO RANGE-DESC
              MOVE ENT-GROUP                          TO RANGE-SEL1
           ELSE
              MOVE "BEG/ENDING BRANCH RANGE        :" TO RANGE-DESC
              MOVE BEG-BR                             TO RANGE-SEL1
              MOVE END-BR                             TO RANGE-SEL2.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "BEG/ENDING ACCOUNT RANGE        :" TO RANGE-DESC.
           MOVE BEG-GLNO                            TO RANGE-SEL1.
           MOVE END-GLNO                            TO RANGE-SEL2.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "POSTED TO G/L FLAG             :" TO RANGE-DESC.
           MOVE POST-GT-FLAG                       TO RANGE-SEL1.
           IF POST-GT-FLAG = "S"
              MOVE "SKIP IF G/L MASTER POSTED    " TO RANGE-SEL2.
           IF POST-GT-FLAG = "I"
              MOVE "INCLUDE IF G/L MASTER POSTED " TO RANGE-SEL2.
           IF POST-GT-FLAG = "O"
              MOVE "ONLY IF G/L MASTER POSTED    " TO RANGE-SEL2.
           PERFORM WRITE-DETAIL-LINE.

       END-RANGE.
           EXIT.

      *********************************************
       FORM-RESET SECTION.
      *********************************************
           MOVE EXT-CONAME-CONAME TO GLEOD-CONAME.
           MOVE EXT-CONAME-SYSDATE TO GLEOD-SYSDATE.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DISPLAY GLEOD-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE GLEOD-FORM-FIELDS TO WR-BUF.
           MOVE GLEOD-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      *********************************************
       SETUP-LINK-INFO SECTION.
      *********************************************
           MOVE GLEOD-HELP--FILE TO HELP-LINK-FILE.
           MOVE GLEOD-HELP--DIR  TO HELP-LINK-DIR.

      **********************************************************
       VDU-CONVERT-FIELD SECTION.
      **********************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBGL/GLEOD_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.


       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/GRWORK.CPY".
       COPY "LIBGB/GRSERCH.CPY".
       COPY "LIBGB/TIMEIO.CPY".

      *********************************************
       MISC SECTION.
      *********************************************
       COPY "LIBGB/RMFILE.CPY".
       COPY "LIBGB/GETPR.CPY".
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/SYSTEM.CPY".
       COPY "LIBGB/MKTEMP.CPY".

      *********************************************
       IO-ROUTINES SECTION.
      *********************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGL/GLGTGS_SQL.CPY".
       COPY "LIBGL/GLGLGS_SQL.CPY".
       COPY "LIBGB/GBGRGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBPRINT.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBGB/GBWKIN.CPY".
       COPY "LIBGB/GBGR1RN.CPY".
       COPY "LIBGL/GLGT1RN.CPY".
       COPY "LIBGL/GLGT2RN.CPY".
       COPY "LIBGL/GLGL1RN.CPY".
       COPY "LIBGB/FERRORS.CPY".

      *********************************************
       END-PROGRAM SECTION.
      *********************************************
       END-RUN.
           PERFORM CLOSE-FILES.
           MOVE MKTEMP-BUF TO ACCESS-BUF.
           PERFORM REMOVE-WORKFILE.
           IF EXT-ROUTE-BUF = "00"
              PERFORM PRU-OUT.
           IF NO-RECORDS-FOUND
              MOVE "NO RECORDS FOUND FOR SELECTED RANGE" TO MESS
              PERFORM SEND-MESS.
       EXIT-PROG.

           IF SV-BEG-BR NOT = 9999
              IF EXT-EOBUF-GROUP-FG = "Y"
                 MOVE SV-BEG-BR TO BEG-BR
                 MOVE SV-END-BR TO END-BR
                 MOVE SV-GROUP-FLAG TO GROUP-FLAG
                 MOVE SV-ENT-GROUP TO ENT-GROUP
              ELSE
                 MOVE SV-BEG-BR TO BEG-BR
                 MOVE SV-END-BR TO END-BR.

           IF EXT-ROUTE-BUF = "00"
              MOVE "GL/GLRPMU" TO FORM-NAM
              MOVE FORM-PATH TO FORM-PATHNAME.
       STOP-RUN.
           IF EXT-ROUTE-BUF = "00" OR "40" OR "50"
              DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN
              DESTROY GLEOD-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.
           EXIT PROGRAM.
