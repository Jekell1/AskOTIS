       IDENTIFICATION DIVISION.
       PROGRAM-ID. TRANLS.
      ******************************************************************
      *
      *                   (GL)
      *       G/L TRANSACTION FILE LISTING
      *
      *=================================================================
      * REV:
      * 061395 BLL SHORTENED DETAIL LINE (WAS 133)
      * 060597 BLV MISSING FIELDS ON DISPLAY PARAMETERS
      * 060597 BLV WOULDN'T WORK IN BATCH-PUT REF COMPARE FLDS IN EOBUF
      * 090997 MJD CHANGED GT-AMT TO PIC S9(9)V99.
      *            ADJUSTED DETAIL LINE TO FIT, REDUCED EXPLANATION
      *            BY 3 BYTES.
      * 990215 BLV EXPAND EXPL TO 32 CHARACTERS, PRINT GL DESCRIPTION
      * 031104 GLB READ BY DATE, NOT REF OR SOURCE, TO SPEED UP
      *            PROCESSING LARGE FILE, ADD SKIP, INC, ONLY FOR POST
      *            FLAG, ADD GROUP OPTION FOR BRANCH GL RANGE, ADD
      *            RANGE PRINT
      * 031201 GLB BUG;ONLY CHECKS SOURCE CODE OPTION IF SOURCE ORDER ?
      * 031211 BLV BUG;WASN'T CHECKING GT DATE < BEG DATE & WAS FALLING
      *            THRU TO TRANSACTIONS PRIOR TO Y2K
      * 040303 BLV ALLOW BRANCH RANGE OF 0-0 FOR USER BRANCH, ADD LOGIC
      *            TO ALLOW RUNNING IN BATCH PROCESSING BY GROUP WITH
      *            LOCAL SPOOLING, WORLD PER JR
      * 060726 BLF FIX LOGIC FOR GROUP BATCH EXECUTION DRIVER
      * 060807 BLF FIX TO MOVE EXT-EOBUF-MESS TO H-TITLE PRINT, SO EO
      *            COMMENT WILL PRINT AS CUSTOM TITLE, WORLD PR# 484
      * 131118 BLM REMOVE "INDEXED BY" ON REF-TOT-TAB IN EOBUF, WAS
      *            GETTING "MISMATCHED EXTERNAL DATA ITEM" IN A30 IN
      *            BATCH BECAUSE EOMAIN, EOEXEC HAVE NO MATCHING INDEX.
      *            MOVED REF-TOT TO A WORKING STORAGE GUY THAT
      *            USES THE INDEXED BY, HOLD-REF-TOT.
      *
      * KEC 2015.0610 {PR#00000} PARADATA <A30> [JAY CISZEWSKI]
      *         REMOVED THE USE OF 'INDEXED BY' ON OCCURS
      *         FOR USE WITH MICROFOCUS.
      *
      * KEC 2016.0105 {PD#00003} PARADATA <A30> [JAY CISZEWSKI]
      *         FIXED KEYS & RECS FOR GT-REC.
      *
      * KEC 2016.1130 {PR#00000} PARADATA <A30> [JAY CISZEWSKI]
      *          CHANGED GT-DATE1 GT-DATE2 GT-DATE3 AS ACTIVE CCYYMMDD.
      * BLM 170313 FIX FOR 8 DIGIT DATES, PR# PD0006
      * BLM 180115 FIX E-POST-FLAG IN ENTER PARAMETERS
      * 20221025 CHANGED ~ TO Z FOR END-REF
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
      *   W K - F I L E
      *
      *=================================================================
      *   INDEXED WORK FILE
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
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)GL/TRANLS.CBL S34 06/10/21-11:39:40 >".

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

       01  ALL-REFERENCES     PIC X VALUE "N".
       01  BEG-QUESTION-MARKS PIC 99.
       01  REF-GT-COMPARE     PIC X(14).
       01  HOLD-REF-COMPARE   PIC X(14).

      *=================================================================
      *  KEC 2015.0610 REMOVED 'INDEXED BY' FOR 'OCCURS'
      *=================================================================
      *01  HOLD-REF-TOT.
      *    03  HOLD-REF-TOT-TAB PIC X OCCURS 14 INDEXED BY REFTOT.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       78  REFTOT-MAX                                VALUE 14.
       77  REFTOT                      PIC 9(02).
       01  HOLD-REF-TOT.
           03  HOLD-REF-TOT-TAB        PIC X(01)     OCCURS 14.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       78  REFWK-MAX                                 VALUE 14.
       77  REFWK                       PIC 9(02).
       01  REF-WORK.
           03  REF-WK-TAB              PIC X(01)
                                       OCCURS 14.
      *------------------------------->INDEXED BY REFWK.

       78  REFTAB-MAX                                VALUE 14.
       77  REFTAB                      PIC 9(02).
       01  REF-TABLE.
           03  REF-TAB                 PIC X(01)
                                       OCCURS 14.
      *------------------------------->INDEXED BY REFTAB.

       78  REFGTTAB-MAX                              VALUE 14.
       77  REFGTTAB                    PIC 9(02).
       01  REF-GT-TABLE.
           03  REF-GT-TAB              PIC X(01)
                                       OCCURS 14.
      *------------------------------->INDEXED BY REFGTTAB.

      *=================================================================
       01  HOLD-UNIQUE-KEY    PIC 9(6).
       01  HOLD-GT-DATE1      PIC 9(8).
       01  HOLD-BR-ACCOUNT.
           03  HOLD-BRANCH    PIC 9(4).
           03  HOLD-DASH      PIC X     VALUE "-".
           03  HOLD-ACCOUNT   PIC 9(7).

       01  HOLD-REF           PIC X(14) VALUE "ZZZZZZZZZZZZZZ".
       01  HOLD-SOURCE        PIC X     VALUE "9".

       01  FIRST-TIME         PIC X VALUE "Y".
       01  RECORDS-FOUND      PIC X VALUE " ".
           88  NO-RECORDS-FOUND     VALUE "N".

       01  MESS-LIST          PIC X(5) VALUE "MESS.".

       COPY "LIBGB/EOBUF_EXT.CPY".
           03  PRORDER                PIC 9.
           03  SOURCE-CODE            PIC 9.
           03  ALL-SRC-CODES  REDEFINES SOURCE-CODE        PIC X.
           03  BEG-REF                PIC X(14).
           03  END-REF                PIC X(14).
           03  REF-TOT                PIC X(14).
           03  BEG-DATE               PIC 9(8).
           03  END-DATE               PIC 9(8).
           03  BEG-BR                 PIC 9(4).
           03  BEG-GLNO               PIC 9(7).
           03  END-BR                 PIC 9(4).
           03  END-GLNO               PIC 9(7).
           03  BEG-REF-COMPARE        PIC X(14).
           03  END-REF-COMPARE        PIC X(14).
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
           03  FILLER         PIC X(32)   VALUE SPACES.
           03  H-TITLE        PIC X(30)   VALUE
               "    GL TRANSACTION JOURNAL    ".

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


       COPY "LIBWI/PFSWINDOWSW.CPY".
       COPY "LIBWI/GRSCANW.CPY".
       COPY "LIBWI/TSTINTW.CPY".
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
       COPY "LIBGL/TRANLS_DEF.CPY".
       COPY "LIBGL/TRANLS_WKS.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".
       COPY "LIBGB/GETFMW.CPY".

       SCREEN SECTION.
       COPY "LIBGL/TRANLS_SCN.CPY".
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

      *************************************************
       MAIN-PROGRAM SECTION.
      *************************************************
           PERFORM SQL-CONNECT.

      D    STOP MESS.
           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "TRANLS" TO VDU-FORM-NAME.

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
           PERFORM TRAP-INTERRUPT.

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
              MOVE GT-REF TO REF-TABLE
              PERFORM GET-REF-TOT-COMPARE
              MOVE REF-WORK TO REF-GT-COMPARE

              MOVE HOLD-REF TO REF-TABLE
              PERFORM GET-REF-TOT-COMPARE
              MOVE REF-WORK TO HOLD-REF-COMPARE
              IF REF-GT-COMPARE NOT = HOLD-REF-COMPARE
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


      *****************************************************
       CREATE-GT-SORT-DETAIL SECTION.
      *****************************************************
       NEXT-GT-RECORD.
           PERFORM TRAP-INTERRUPT.

           PERFORM READ-GT2-FILE-NEXT.
           IF IO-BAD
              GO TO EXIT-PROCESS-GT.

      * TEST DATE; GT-DATE1 STORED AS CCYYMMDD
           IF GT-DATE1 > END-DATE
              GO TO EXIT-PROCESS-GT.

      * TEST POSTED FLAG
           PERFORM TEST-GT-FLAG.
           IF IO-FG = 9
              GO TO NEXT-GT-RECORD.

      * TEST G/L ACCOUNT NUMBER
           IF GT-ACCTNO < BEG-GLNO OR GT-ACCTNO > END-GLNO
              GO TO NEXT-GT-RECORD.

      * TEST G/L BRANCH / GROUP
           IF GROUP-FLAG = "Y"
              MOVE GT-BRANCH TO WGR-SRESULT
              PERFORM SEARCH-GR-VERIFY
              IF WGR-ERR > 0
                 GO TO NEXT-GT-RECORD.
           IF GROUP-FLAG NOT = "Y"
              IF GT-BRANCH < BEG-BR OR GT-BRANCH > END-BR
                 GO TO NEXT-GT-RECORD.

      * TEST G/L REFERENCE
           IF ALL-REFERENCES = "N"
              MOVE BEG-REF         TO REF-TABLE
              MOVE GT-REF          TO REF-GT-TABLE
              PERFORM GET-GT-REF-TO-COMPARE
              IF REF-WORK < BEG-REF-COMPARE
                 GO TO NEXT-GT-RECORD
              ELSE
                 MOVE END-REF         TO REF-TABLE
                 MOVE GT-REF          TO REF-GT-TABLE
                 PERFORM GET-GT-REF-TO-COMPARE
                 IF REF-WORK > END-REF-COMPARE
                    GO TO NEXT-GT-RECORD.

      * TEST G/L SOURCE CODE OPTION
           IF ALL-SRC-CODES NOT = "*"
              IF GT-SOURCE NOT = SOURCE-CODE
                 GO TO NEXT-GT-RECORD.

           PERFORM CREATE-WK-RECORD.

           GO TO NEXT-GT-RECORD.
       EXIT-PROCESS-GT.
           PERFORM CLOSE-GT2-FILE.


      *****************************************************
       PROCESS-GT-DETAIL SECTION.
      *****************************************************

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

           MOVE GT-DATE1           TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY        TO D-DATE.

           PERFORM WRITE-DETAIL-LINE.

      *****************************************************
       HEAD-PAGE SECTION.
      *****************************************************
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


      *****************************************************
       WRITE-DETAIL-LINE SECTION.
      *****************************************************
           ADD LN-FEED LN-COUNT GIVING LN-WK.
           IF LN-WK > EXT-CONAME-RPT-LINES
              PERFORM HEAD-PAGE.
           MOVE DETAIL-LINE TO PR-REC.
           PERFORM WRITE-PR-REC.
           ADD LN-FEED TO LN-COUNT.
           MOVE 1 TO LN-FEED.
           MOVE SPACES TO DETAIL-LINE.

      *****************************************************
       PRINT-SUB-TOTALS SECTION.
      *****************************************************
           IF PRORDER = 1 AND ALL-SRC-CODES NOT = "*"
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

      *****************************************************
       PRINT-GRAND-TOTALS SECTION.
      *****************************************************
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

      *******************************************
       INITIALIZATION SECTION.
      *******************************************
           MOVE TRANLS-QUEUE-POS TO Q-POS.
           MOVE TRANLS-COPIES-POS TO C-POS.
           MOVE TRANLS-PRU-POS   TO P-POS.
           MOVE SPACES TO Q-BUF C-BUF P-BUF.
       COPY "LIBGB/EOINIT.CPY".

           IF EXT-ROUTE-BUF = "00"
              MOVE "G/L TRANSACTION LISTING IN PROGRESS......" TO MESS
              MOVE MESS-LIST TO WR-LIST
              MOVE MESS TO WR-BUF
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

           IF ALL-SRC-CODES = "*"
              MOVE 0 TO GT-SOURCE HD-SRC-TYPE
              MOVE SPACES TO HD-SRC-LIT1 HD-SRC-LIT2
           ELSE
              MOVE SOURCE-CODE TO GT-SOURCE HD-SRC-TYPE
              MOVE "  SOURCE " TO HD-SRC-LIT1
              MOVE " ONLY" TO HD-SRC-LIT2.

           IF EXT-ROUTE-BUF = "70"
              MOVE EXT-EOBUF-MESS TO H-TITLE.

           MOVE BEG-REF TO REF-TABLE.
           PERFORM COUNT-REF-QUESTION-MARKS.
           IF BEG-QUESTION-MARKS = 0
              MOVE BEG-REF TO GT-REF
           ELSE
              MOVE " " TO GT-REF.

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
           PERFORM SQL-SET-DATE
           MOVE SQL-DATE-YYYY-MM-DD TO QGT2-WBEG-DATE1.

           MOVE END-DATE TO SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE
           MOVE SQL-DATE-YYYY-MM-DD TO QGT2-WEND-DATE1.

           PERFORM START-GT2-FILE.

           PERFORM ENABLE-INTERRUPT.

           GO TO EXIT-INITIALIZE.

       ENDIT.
           MOVE "X" TO EXT-EOBUF-ERRCD.
       EXIT-INITIALIZE.
           EXIT.

      *****************************************
       EO-EXEC-AUDIT SECTION.
      *****************************************
           MOVE EXT-EOBUF-DATE1 TO BEG-DATE.
           MOVE EXT-EOBUF-DATE2 TO END-DATE.

      *****************************************
       ENTER-PARAMETERS SECTION.
      *****************************************

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

           IF PRORDER = 1
              MOVE "S  A000REFT." TO VDU
              MOVE " " TO REF-TOT VDUBUF
              PERFORM SCREENIO.

       GET-SINGLE-SRC.
      *    IF PRORDER = 2
      *       GO TO GET-BEG-REF.

           MOVE "ENNN010SSRC." TO VDU.
           PERFORM SCREENIO.
           IF F1-KEY GO TO EXIT-PARM.
           IF F3-KEY GO TO SEND-ALL-SRC.
           IF UP-KEY GO TO GET-PRORDER.
           IF F7-KEY GO TO EXIT-PARM.
           IF NOT ENTER-KEY GO TO GET-SINGLE-SRC.
           MOVE VDUNUM0 TO SOURCE-CODE.
           GO TO GET-BEG-REF.
       SEND-ALL-SRC.
            MOVE "S  A010SSRC." TO VDU.
            MOVE "*" TO ALL-SRC-CODES VDUBUF.
            PERFORM SCREENIO.

       GET-BEG-REF.
           MOVE "N" TO ALL-REFERENCES.
           MOVE "E  A000REF1." TO VDU.
           PERFORM SCREENIO.
           IF F1-KEY GO TO EXIT-PARM.
           IF F3-KEY GO TO SEND-ALL-REF.
           IF UP-KEY
              MOVE " " TO ALL-SRC-CODES
              MOVE "S  A000SSRC." TO VDU
              PERFORM SCREENIO
              GO TO GET-SINGLE-SRC.
           IF F7-KEY GO TO EXIT-PARM.
           IF NOT ENTER-KEY GO TO GET-BEG-REF.
           MOVE VDUBUF TO BEG-REF REF-TABLE.

           PERFORM GET-REF-SELECT-COMPARE.
           MOVE REF-WORK TO BEG-REF-COMPARE.
       GET-END-REF.
           MOVE "N" TO ALL-REFERENCES.
           MOVE "E  A000REF2." TO VDU.
           PERFORM SCREENIO.
           IF F1-KEY GO TO EXIT-PARM.
           IF UP-KEY GO TO GET-BEG-REF.
           IF F7-KEY GO TO EXIT-PARM.
           IF NOT ENTER-KEY GO TO GET-END-REF.
           MOVE VDUBUF TO END-REF REF-TABLE.

           PERFORM GET-REF-SELECT-COMPARE.
           MOVE REF-WORK TO END-REF-COMPARE.

           GO TO GET-REF-TOT.
       SEND-ALL-REF.
           MOVE "Y" TO ALL-REFERENCES.
           MOVE "S  A000REF1." TO VDU.
           MOVE " " TO BEG-REF
                       BEG-REF-COMPARE
                       VDUBUF.
           PERFORM SCREENIO.
           MOVE "S  A000REF2." TO VDU.
           MOVE ALL "Z"          TO END-REF.
           CALL "C$TOLOWER" USING END-REF, VALUE 14.
           MOVE END-REF TO END-REF-COMPARE VDUBUF.
           PERFORM SCREENIO.
       GET-REF-TOT.
           IF PRORDER = 1
              GO TO GET-BDT.

           MOVE "E  A000REFT." TO VDU.
           PERFORM SCREENIO.
           IF F1-KEY GO TO EXIT-PARM.
           IF F3-KEY GO TO SEND-ALL-REF-TOT.
           IF UP-KEY GO TO GET-END-REF.
           IF F7-KEY GO TO EXIT-PARM.
           IF NOT ENTER-KEY GO TO GET-REF-TOT.
           MOVE VDUBUF TO REF-TOT.
           PERFORM VERIFY-REF-TOT.
           IF IO-FG = 9
              MOVE "PLEASE ENTER ONLY 'T'S AND  '?'S" TO MESS
              PERFORM SEND-MESS
              GO TO GET-REF-TOT.
           GO TO GET-BDT.
       SEND-ALL-REF-TOT.
           MOVE "S  A000REFT." TO VDU.
           MOVE "TTTTTTTTTTTTTT" TO REF-TOT VDUBUF.
           PERFORM SCREENIO.

       GET-BDT.
           IF EXT-ROUTE-BUF = "40" OR "50"
              GO TO GET-BEG-BR.
           MOVE "ERNM060BEGDATE." TO VDU
           PERFORM SCREENIO.
           IF F1-KEY GO TO EXIT-PARM.
           IF UP-KEY GO TO GET-BEG-REF.
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
           MOVE "S  8080BEGDATE." TO VDU.
           MOVE 0          TO BEG-DATE VDU-DATE-YYYYMMDD.
           PERFORM SCREENIO.
           MOVE "S  8080ENDDATE." TO VDU.
           MOVE 99999999 TO END-DATE VDU-DATE-YYYYMMDD.
           PERFORM SCREENIO.

       GET-BEG-BR.
           MOVE "E RA000BEGBR." TO VDU.
           PERFORM SCREENIO.
           IF F1-KEY GO TO EXIT-PARM.
           IF F3-KEY GO TO ALL-BRS.
           IF UP-KEY GO TO GET-EDT.
           IF F7-KEY GO TO EXIT-PARM.

           IF F6-KEY
              MOVE GRSCAN-BUF TO SCAN-BUF
              PERFORM CALL-WINDOW-PROGRAM
              MOVE SCAN-BUF TO GRSCAN-BUF
              IF SCAN-STATUS NOT = "00"
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
           IF NOT ENTER-KEY GO TO E-POST-FLAG.
           MOVE VDUBUF TO POST-GT-FLAG.
           IF POST-GT-FLAG = "S" GO TO EXIT-PARM.
           IF POST-GT-FLAG = "I" GO TO EXIT-PARM.
           IF POST-GT-FLAG = "O" GO TO EXIT-PARM.
           GO TO E-POST-FLAG.

       EXIT-PARM.
           EXIT.

      **********************************************
       DISPLAY-PARAMETERS SECTION.
      **********************************************
           MOVE "SNNN010PRORDER." TO VDU.
           MOVE PRORDER TO VDUNUM0.
           PERFORM SCREENIO.

           MOVE "SNNN010SSRC." TO VDU.
           MOVE SOURCE-CODE TO VDUNUM0.
           PERFORM SCREENIO.

           MOVE "S  A000REF1." TO VDU.
           MOVE BEG-REF TO VDUBUF.
           PERFORM SCREENIO.
           MOVE "S  A000REF2." TO VDU.
           MOVE END-REF TO VDUBUF.
           PERFORM SCREENIO.
           MOVE "S  A000REFT." TO VDU.
           MOVE REF-TOT TO VDUBUF.
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

      ******************************************************************
       COUNT-REF-QUESTION-MARKS SECTION.
      ******************************************************************

           MOVE 0 TO BEG-QUESTION-MARKS.

           PERFORM VARYING REFTAB FROM 1 BY 1 UNTIL REFTAB > REFTAB-MAX

              IF ( REF-TAB(REFTAB) = "?" )
                 ADD 1 TO BEG-QUESTION-MARKS
              END-IF

           END-PERFORM.

       COUNT-REF-QUESTION-MARKS-EXIT.
           EXIT.

      ******************************************************************
      *    VERIFY REFERENCE TOTAL IS ALL 'T' OR '?'
      ******************************************************************
       VERIFY-REF-TOT SECTION.

           MOVE REF-TOT         TO HOLD-REF-TOT.
           MOVE 0               TO IO-FG.

           PERFORM VARYING REFTOT FROM 1 BY 1 UNTIL REFTOT > REFTOT-MAX
                   OR (IO-FG NOT = 0)

              IF ( NOT (HOLD-REF-TOT-TAB(REFTOT) = "T" OR "?"))
                 MOVE 9         TO IO-FG
              END-IF

           END-PERFORM.

       VERIFY-REF-TOT-EXIT.
           EXIT.

      ******************************************************************
       GET-REF-SELECT-COMPARE SECTION.
      ******************************************************************

           MOVE SPACES                     TO REF-WORK.
           MOVE ZEROES                     TO REFTAB
                                              REFWK.

           PERFORM VARYING REFTAB FROM 1 BY 1 UNTIL REFTAB > REFTAB-MAX

              IF ( REF-TAB(REFTAB) NOT = "?" )
                   ADD  01                 TO REFWK
                   MOVE REF-TAB(REFTAB)    TO REF-WK-TAB(REFWK)
              END-IF

           END-PERFORM.

       GET-REF-SELECT-COMPARE-EXIT.
           EXIT.

      ******************************************************************
       GET-GT-REF-TO-COMPARE SECTION.
      ******************************************************************

           MOVE SPACES                     TO REF-WORK.
           MOVE ZEROES                     TO REFTAB
                                              REFGTTAB
                                              REFWK.

           PERFORM VARYING REFTAB FROM 1 BY 1 UNTIL REFTAB > REFTAB-MAX

              IF ( REF-TAB(REFTAB) NOT = "?" )
                 MOVE REFTAB               TO REFGTTAB
                 ADD  01                   TO REFWK
                 MOVE REF-GT-TAB(REFGTTAB) TO REF-WK-TAB(REFWK)
              END-IF

           END-PERFORM.

       GET-GT-REF-TO-COMPARE-EXIT.
           EXIT.

      ******************************************************************
       GET-REF-TOT-COMPARE SECTION.
      ******************************************************************

           MOVE REF-TOT                    TO HOLD-REF-TOT.
           MOVE SPACES                     TO REF-WORK.
           MOVE ZEROES                     TO REFTAB
                                              REFTOT
                                              REFWK.

           PERFORM VARYING REFTOT FROM 1 BY 1 UNTIL REFTOT > REFTOT-MAX

              IF ( HOLD-REF-TOT-TAB(REFTOT) = "T" )
                   MOVE REFTOT             TO REFTAB
                   ADD  01                 TO REFWK
                   MOVE REF-TAB(REFTAB)    TO REF-WK-TAB(REFWK)
              END-IF

           END-PERFORM.

       GET-REF-TOT-COMPARE-EXIT.
           EXIT.

      **********************************************
       CREATE-WK-RECORD SECTION.
      **********************************************
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
           MOVE 0 TO IO-FG.
           IF POST-GT-FLAG = "S"
              IF GT-FLAG NOT = SPACES
                  MOVE 9 TO IO-FG.
           IF POST-GT-FLAG = "O"
              IF GT-FLAG = SPACES
                  MOVE 9 TO IO-FG.


      *****************************************************
       PRINT-RANGE-LINES SECTION.
      *****************************************************
           MOVE "PRINT REPORT ORDER BY          :"    TO RANGE-DESC.
           MOVE PRORDER                               TO RANGE-SEL1.
           IF PRORDER = 1 MOVE "SOURCE/DATE/ACCOUNT#" TO RANGE-SEL2.
           IF PRORDER = 2 MOVE "REF/DATE/ACCOUNT#   " TO RANGE-SEL2.
           IF PRORDER = 3 MOVE "DATE/ACCOUNT#/LINE  " TO RANGE-SEL2.
           IF PRORDER = 4 MOVE "ACCOUNT#/DATE/LINE  " TO RANGE-SEL2.
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "SOURCE CODE                    :" TO RANGE-DESC.
           MOVE SOURCE-CODE                        TO RANGE-SEL1.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "BEG/ENDING REFERENCE RANGE     :" TO RANGE-DESC.
           MOVE BEG-REF                            TO RANGE-SEL1.
           MOVE END-REF                            TO RANGE-SEL2.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "BEG/ENDING DATE RANGE          :" TO RANGE-DESC.
           MOVE BEG-DATE TO RANGE-DATE1.
           MOVE END-DATE TO RANGE-DATE2.
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

      **********************************************************
       FORM-RESET SECTION.
      **********************************************************
           MOVE EXT-CONAME-CONAME TO TRANLS-CONAME.
           MOVE EXT-CONAME-SYSDATE TO TRANLS-SYSDATE.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DISPLAY TRANLS-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE TRANLS-FORM-FIELDS TO WR-BUF.
           MOVE TRANLS-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      **********************************************************
       SETUP-LINK-INFO SECTION.
      **********************************************************
           MOVE TRANLS-HELP--FILE TO HELP-LINK-FILE.
           MOVE TRANLS-HELP--DIR  TO HELP-LINK-DIR.

      **********************************************************
       VDU-CONVERT-FIELD SECTION.
      **********************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBGL/TRANLS_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/GRWORK.CPY".
       COPY "LIBGB/GRSERCH.CPY".
       COPY "LIBWI/PFSWINDOWS.CPY".
       COPY "LIBGB/TIMEIO.CPY".

      ********************************************
       MISC SECTION.
      ********************************************
       COPY "LIBGB/RMFILE.CPY".
       COPY "LIBGB/GETPR.CPY".
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/SYSTEM.CPY".
       COPY "LIBGB/MKTEMP.CPY".

      ********************************************
       IO-ROUTINES SECTION.
      ********************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGL/GLGTGS_SQL.CPY".
       COPY "LIBGL/GLGLGS_SQL.CPY".
       COPY "LIBGB/GBGRGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBPRINT.CPY".

      * COPY "LIBGB/GBBRRN.CPY".
      * COPYMEMBER: LIBGB/GBBRRN
      * 20220923 BAH ADDED BR_VERITEC_LICENSE #1570
      * 20230419 BAH ADDED BR-TEST-PP-AUTO-INSCOVR #1594A
       LOAD-BR-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( BR-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( BR-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO BR-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO BR-ALL-MACHINE
              MOVE BR-ALL-PATH             TO BR-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( BR-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO BR-ALL-BASE
              MOVE FILPATH-MACHINE         TO BR-ALL-MACHINE
              MOVE BR-ALL-PATH             TO BR-PATH.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    CREATE PATH FOR FILE SELECT AND SQL TABLE NAME
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( EXT-IO-BRFILE NOT = "S" )
              MOVE BR-PATH-BEG             TO BR-SELECT-PATH-BEG
              MOVE BR-PATH-FILE            TO BR-SELECT-PATH-FILE
           ELSE
              MOVE BR-PATH-SQL             TO BR-SELECT-PATH.

      *-----------------------------------------------------------------
       OPEN-BR-FILE.
           PERFORM LOAD-BR-FILE.
           PERFORM OPEN-IT.
           MOVE BR-SELECT-PATH TO E-FILE.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       START-BR-FILE.
           PERFORM START-IT.
           MOVE BR-SELECT-PATH TO E-FILE.
           MOVE BR-NO TO E-KEYX.

           IF ( BR-CURSOR-STAT-GOOD )
              PERFORM CLOSE-BR-FILE.

           IF ( QBR-WEND-NO = ZEROES    ) OR
              ( QBR-WEND-NO NOT NUMERIC )
              MOVE BR-NO                TO QBR-WBEG-NO
              MOVE ALL "9"              TO QBR-WEND-NO.

           EXEC SQL
            DECLARE BR_CURSOR CURSOR FOR
            SELECT BRFILE.BR_NO,
                   ISNULL(CAST(BRFILE.BR_LTOUCH_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(BRFILE.BR_NAME,''),
                   ISNULL(BRFILE.BR_ADR1,''),
                   ISNULL(BRFILE.BR_ADR2,''),
                   ISNULL(BRFILE.BR_CITY,''),
                   ISNULL(BRFILE.BR_STATE,''),
                   ISNULL(BRFILE.BR_ZIP,''),
                   BRFILE.BR_PHONE,
                   BRFILE.BR_RETAIN,
                   BRFILE.BR_NEXT_CHECK,
                   BRFILE.BR_PRINTER_1,
                   BRFILE.BR_PRINTER_2,
                   BRFILE.BR_PRINTER_3,
                   BRFILE.BR_PRINTER_4,
                   BRFILE.BR_FEE_GL_BRNO,
                   ISNULL(BRFILE.BR_REMIT1,''),
                   ISNULL(BRFILE.BR_REMIT2,''),
                   ISNULL(BRFILE.BR_REMIT3,''),
                   ISNULL(BRFILE.BR_STMNT1,''),
                   ISNULL(BRFILE.BR_STMNT2,''),
                   BRFILE.BR_INS_PRODUCER_NUM,
                   ISNULL(BRFILE.BR_DLRCHK,''),
                   ISNULL(BRFILE.BR_FRONT_END_FG,''),
                   ISNULL(BRFILE.BR_EMLNRG_PHONE,''),
                   ISNULL(BRFILE.BR_POST_GL,''),
                   ISNULL(BRFILE.BR_ALLOTMNTS,''),
                   ISNULL(BRFILE.BR_EXTRACT_AUTO_UPD,''),
                   ISNULL(BRFILE.BR_PCNI_LEDGER,''),
                   ISNULL(BRFILE.BR_PRINTERFG_1,''),
                   ISNULL(BRFILE.BR_PRINTERFG_2,''),
                   ISNULL(BRFILE.BR_PRINTERFG_3,''),
                   ISNULL(BRFILE.BR_PRINTERFG_4,''),
                   ISNULL(BRFILE.BR_NEGDLRSV,''),
                   ISNULL(BRFILE.BR_CPCORP_ORDER,''),
                   ISNULL(BRFILE.BR_PRIO_MEMO_FLAG,''),
                   ISNULL(BRFILE.BR_PREBILL_PL_JD_OPT,''),
                   ISNULL(BRFILE.BR_LOAN_NUM_FLAG,''),
                   ISNULL(BRFILE.BR_DOWNPY_FLAG,''),
                   ISNULL(BRFILE.BR_AUTO_INS_EARN,''),
                   ISNULL(BRFILE.BR_OTH_INS_LEVELS,''),
                   ISNULL(BRFILE.BR_ALLOW_INTERBR,''),
                   ISNULL(BRFILE.BR_UNER_DLANRP,''),
                   ISNULL(BRFILE.BR_SECOND_PROMISE,''),
                   ISNULL(BRFILE.BR_AVAIL_CREDIT,''),
                   ISNULL(BRFILE.BR_RECP1,''),
                   ISNULL(BRFILE.BR_RECP2,''),
                   ISNULL(BRFILE.BR_INC_WAIVERS,''),
                   ISNULL(BRFILE.BR_INC_INS,''),
                   BRFILE.BR_BP_NEXT_BATCH,
                   ISNULL(BRFILE.BR_BP_NOPAYOFF,''),
                   ISNULL(BRFILE.BR_BP_ODDPAY,''),
                   ISNULL(BRFILE.BR_BP_LC,''),
                   ISNULL(BRFILE.BR_BP_NOPRIN,''),
                   ISNULL(BRFILE.BR_BP_FLASH,''),
                   ISNULL(BRFILE.BR_BP_ACTIONCD,''),
                   BRFILE.BR_BP_MAXCON,
                   BRFILE.BR_BP_MAXREC,
                   ISNULL(BRFILE.BR_BP_PL,''),
                   ISNULL(BRFILE.BR_BP_JD,''),
                   BRFILE.BR_BP_BRANCH,
                   ISNULL(BRFILE.BR_AGE_ERN,''),
                   ISNULL(BRFILE.BR_OTHER_BRANCH_GL,''),
                   ISNULL(BRFILE.BR_SALES_SMALL,''),
                   ISNULL(BRFILE.BR_NEW_COLL_INQUIRY,''),
                   ISNULL(BRFILE.BR_CHANGE_MEMO,''),
                   ISNULL(BRFILE.BR_INS_ADDON,''),
                   ISNULL(BRFILE.BR_TRAILER_REC,''),
                   ISNULL(BRFILE.BR_ALDEL_FLAG,''),
                   ISNULL(BRFILE.BR_IB_LCHG_MESS,''),
                   ISNULL(BRFILE.BR_COL_QUEUE_PAST_DUE,''),
                   BRFILE.BR_LI_DLCOMM_CMSI_PP_MAX,
                   ISNULL(BRFILE.BR_ADDON_FEECD_O2_1,''),
                   ISNULL(BRFILE.BR_ADDON_FEECD_O2_2,''),
                   ISNULL(BRFILE.BR_AUTO_CHECK,''),
                   ISNULL(BRFILE.BR_RECEIPT_PROMDATE,''),
                   ISNULL(BRFILE.BR_RECEIPT_INTPD,''),
                   ISNULL(BRFILE.BR_COLL_ID_SCAN,''),
                   ISNULL(BRFILE.BR_STOP_ON_COLL,''),
                   ISNULL(BRFILE.BR_DUEDATE_CHANGE_CD,''),
                   ISNULL(BRFILE.BR_CHECK_EOM_FG,''),
                   ISNULL(BRFILE.BR_VOLUME_DLANRP,''),
                   ISNULL(BRFILE.BR_PDTH_NXDUE,''),
                   ISNULL(BRFILE.BR_LC_CODE,''),
                   ISNULL(BRFILE.BR_VALIDATE_PRUS,''),
                   ISNULL(BRFILE.BR_AUTO_OPEN_BAL,''),
                   BRFILE.BR_OPEN_BAL_1,
                   BRFILE.BR_OPEN_BAL_2,
                   BRFILE.BR_OPEN_BAL_3,
                   BRFILE.BR_OPEN_BAL_4,
                   BRFILE.BR_OPEN_BAL_5,
                   BRFILE.BR_OPEN_BAL_6,
                   BRFILE.BR_OPEN_BAL_7,
                   BRFILE.BR_OPEN_BAL_8,
                   BRFILE.BR_OPEN_BAL_9,
                   BRFILE.BR_OPEN_BAL_10,
                   BRFILE.BR_OPEN_BAL_11,
                   BRFILE.BR_OPEN_BAL_12,
                   BRFILE.BR_OPEN_BAL_13,
                   BRFILE.BR_OPEN_BAL_14,
                   BRFILE.BR_OPEN_BAL_15,
                   BRFILE.BR_OPEN_BAL_16,
                   BRFILE.BR_OPEN_BAL_17,
                   BRFILE.BR_OPEN_BAL_18,
                   BRFILE.BR_OPEN_BAL_19,
                   BRFILE.BR_OPEN_BAL_20,
                   ISNULL(BRFILE.BR_TENDER_CD_1,''),
                   ISNULL(BRFILE.BR_TENDER_CD_2,''),
                   ISNULL(BRFILE.BR_TENDER_CD_3,''),
                   ISNULL(BRFILE.BR_DLPARV_ADJ_FG,''),
                   ISNULL(BRFILE.BR_OVERRIDE_BR_STATE,''),
                   BRFILE.BR_DEFAULT_SC_CLASS,
                   ISNULL(BRFILE.BR_RECDEL_NOT_REC,''),
                   ISNULL(BRFILE.BR_POST_AHEAD_FG,''),
                   ISNULL(BRFILE.BR_CAN_LOANS_FG,''),
                   ISNULL(BRFILE.BR_NON_MONTHLY_CAN_LOANS,''),
                   ISNULL(BRFILE.BR_BP_ALLOW_PMT_ACCT_OTHBAL,''),
                   ISNULL(BRFILE.BR_LBOX_ALLOW_PMT_ACCT_OTHBAL,''),
                   BRFILE.BR_REGION_NUMBER,
                   ISNULL(BRFILE.BR_OUR_FEE,''),
                   BRFILE.BR_REB_METHOD_1,
                   BRFILE.BR_REB_METHOD_2,
                   BRFILE.BR_REB_METHOD_3,
                   BRFILE.BR_REB_METHOD_4,
                   BRFILE.BR_REB_METHOD_5,
                   BRFILE.BR_REB_METHOD_6,
                   BRFILE.BR_REB_METHOD_7,
                   BRFILE.BR_REB_METHOD_8,
                   BRFILE.BR_REB_METHOD_9,
                   BRFILE.BR_REB_METHOD_10,
                   BRFILE.BR_REB_METHOD_11,
                   BRFILE.BR_REBDAYS_MONTH_1,
                   BRFILE.BR_REBDAYS_MONTH_2,
                   BRFILE.BR_REBDAYS_MONTH_3,
                   BRFILE.BR_REBDAYS_MONTH_4,
                   BRFILE.BR_REBDAYS_MONTH_5,
                   BRFILE.BR_REBDAYS_MONTH_6,
                   BRFILE.BR_REBDAYS_MONTH_7,
                   BRFILE.BR_REBDAYS_MONTH_8,
                   BRFILE.BR_REBDAYS_MONTH_9,
                   BRFILE.BR_REBDAYS_MONTH_10,
                   BRFILE.BR_REBDAYS_MONTH_11,
                   BRFILE.BR_ACCRUAL_MONTHS_1,
                   BRFILE.BR_ACCRUAL_MONTHS_2,
                   BRFILE.BR_ACCRUAL_MONTHS_3,
                   BRFILE.BR_ACCRUAL_MONTHS_4,
                   BRFILE.BR_ACCRUAL_MONTHS_5,
                   BRFILE.BR_ACCRUAL_MONTHS_6,
                   BRFILE.BR_ACCRUAL_MONTHS_7,
                   BRFILE.BR_ACCRUAL_MONTHS_8,
                   BRFILE.BR_ACCRUAL_MONTHS_9,
                   BRFILE.BR_ACCRUAL_MONTHS_10,
                   BRFILE.BR_ACCRUAL_MONTHS_11,
                   BRFILE.BR_GLOVERSHORT,
                   BRFILE.BR_GLBANK,
                   BRFILE.BR_GLONHAND,
                   BRFILE.BR_BANKBAL,
                   BRFILE.BR_RETAIN_AMT,
                   ISNULL(BRFILE.BR_LCARDFG,''),
                   ISNULL(BRFILE.BR_DATA_PATH,''),
                   ISNULL(BRFILE.BR_MACHINE,''),
                   ISNULL(BRFILE.BR_MANAGER,''),
                   ISNULL(BRFILE.BR_EOD_PROCEDURE,''),
                   ISNULL(BRFILE.BR_BP_PAYOFF_NONCASH,''),
                   ISNULL(BRFILE.BR_BP_PAYOFF_OV,''),
                   BRFILE.BR_LI_DLCOMM_CMSI_O3_MAX,
                   ISNULL(BRFILE.BR_LR_GROUP_COLLID,''),
                   ISNULL(BRFILE.BR_UNITPER_FG,''),
                   ISNULL(BRFILE.BR_UNITPER_Y_D_FG,''),
                   ISNULL(BRFILE.BR_NO_PROMPT_FOR_OTHBR,''),
                   ISNULL(BRFILE.BR_WORLD_BRANCH_TYPE,''),
                   ISNULL(BRFILE.BR_COL_QUEUE_BEHAVIOR_FG,''),
                   ISNULL(BRFILE.BR_CONVERT_AUTO_ACCTNO,''),
                   ISNULL(BRFILE.BR_TS_REPO_INS_FG,''),
                   ISNULL(BRFILE.BR_BOOK_SF_BR_ONLY,''),
                   ISNULL(BRFILE.BR_FORCE_VALID_CITY_COUNTY,''),
                   ISNULL(BRFILE.BR_SET_COLLCD_DL_MFGCD,''),
                   ISNULL(BRFILE.BR_DEFAULT_SF_UNITPER_CD,''),
                   ISNULL(BRFILE.BR_DEFAULT_SF_PURCD,''),
                   ISNULL(BRFILE.BR_DEFAULT_LN_UNITPER_CD,''),
                   ISNULL(BRFILE.BR_DEFAULT_LN_PURCD,''),
                   ISNULL(BRFILE.BR_TAP_OTHER_DL,''),
                   ISNULL(BRFILE.BR_AH_AND_CL,''),
                   ISNULL(BRFILE.BR_NO_CASH_PROCEEDS,''),
                   ISNULL(BRFILE.BR_RI_BEFORE_REPO,''),
                   ISNULL(BRFILE.BR_RI_BEFORE_P4P5,''),
                   ISNULL(BRFILE.BR_NOT_OURS_REFUND,''),
                   ISNULL(BRFILE.BR_SFDLO3_WINDOW_FG,''),
                   ISNULL(BRFILE.BR_CALC_INS_ADJ,''),
                   ISNULL(BRFILE.BR_AC_BEFORE_REPO,''),
                   ISNULL(BRFILE.BR_AC_BEFORE_P4P5,''),
                   BRFILE.BR_BANK_ROUTING_NO,
                   BRFILE.BR_BANK_CLEARING_GL,
                   BRFILE.BR_GLBANK_DEPOSIT,
                   ISNULL(BRFILE.BR_BANK_ACCTNO,''),
                   BRFILE.BR_DEFAULT_SC_SUBCLASS,
                   ISNULL(BRFILE.BR_NO_OUT_DLR_RSV,''),
                   ISNULL(BRFILE.BR_COLLECTOR_QUEUE,''),
                   ISNULL(BRFILE.BR_COLL_QUEUE_CON_REC,''),
                   ISNULL(BRFILE.BR_JE_ALLOCATION_SKIP,''),
                   ISNULL(BRFILE.BR_EXPAND_APP_INPUT,''),
                   ISNULL(BRFILE.BR_A50_CPCORP,''),
                   ISNULL(BRFILE.BR_INBR_BILNSC_FG,''),
                   ISNULL(BRFILE.BR_INBR_BRIEF_FG,''),
                   ISNULL(BRFILE.BR_LEDGER_JUST_INTCHG,''),
                   BRFILE.BR_BYINQ_START_PAGE,
                   BRFILE.BR_DSINQ_START_PAGE,
                   ISNULL(BRFILE.BR_APP_REQUIRED_FOR_INPUT,''),
                   ISNULL(BRFILE.BR_CREDIT_SCORING_FG,''),
                   ISNULL(BRFILE.BR_COL_QUEUE_LOW_TO_HI,''),
                   ISNULL(BRFILE.BR_CAN_GROUP_ID,''),
                   ISNULL(BRFILE.BR_AUTO_LNSTAT_WINDOW,''),
                   BRFILE.BR_TIME_ZONE_FACTOR,
                   ISNULL(BRFILE.BR_LONPF_OPT_DISPLAY,''),
                   ISNULL(BRFILE.BR_GIFTNET,''),
                   ISNULL(BRFILE.BR_GIFTNET_LEDGER_LINE,''),
                   BRFILE.BR_NO_FT_EMP,
                   BRFILE.BR_NO_PT_EMP,
                   ISNULL(BRFILE.BR_BALLOON_LOANS_FG,''),
                   ISNULL(BRFILE.BR_GL_TYPE_ALLOWED,''),
                   ISNULL(BRFILE.BR_GL_ENTRY_DEFAULT,''),
                   ISNULL(BRFILE.BR_MIDSTREAM_1STPAY_FG,''),
                   BRFILE.BR_NODEF_1ST_X_MONTHS,
                   ISNULL(BRFILE.BR_ADD_INS_FEECODE,''),
                   ISNULL(BRFILE.BR_ROAD_G_SGL_FEECODE,''),
                   ISNULL(BRFILE.BR_ROAD_G_FAM_FEECODE,''),
                   ISNULL(BRFILE.BR_NODEF_IF_ZERO_AMT,''),
                   ISNULL(BRFILE.BR_ASSESS_LATE_CHGS,''),
                   BRFILE.BR_NEXT_SOLICIT_NO,
                   ISNULL(BRFILE.BR_ADDON_FEECD_O3_1,''),
                   ISNULL(BRFILE.BR_ADDON_FEECD_O3_2,''),
                   ISNULL(BRFILE.BR_IMMEDIATE_RECEIPT,''),
                   ISNULL(BRFILE.BR_IMMEDIATE_DOC_PRINT,''),
                   ISNULL(BRFILE.BR_AUTO_PL_REF_AC,''),
                   BRFILE.BR_FAX_NUMBER,
                   ISNULL(BRFILE.BR_ALPHA_DRAWERS,''),
                   ISNULL(BRFILE.BR_LATECHG_ON_ALLOTMENTS,''),
                   ISNULL(BRFILE.BR_ALLOW_SYS_AS_EFFDATE,''),
                   BRFILE.BR_TAX_PREBOOK_CUST,
                   BRFILE.BR_TAX_PREBOOK_NONCUST,
                   ISNULL(BRFILE.BR_PASSWORD_DRAWER,''),
                   ISNULL(BRFILE.BR_ONESTEP_RN_FG,''),
                   ISNULL(BRFILE.BR_LI_LISCAN_FG,''),
                   ISNULL(BRFILE.BR_HIDE_END_BAL,''),
                   ISNULL(BRFILE.BR_LBOX_NOPAYOFF,''),
                   ISNULL(BRFILE.BR_LBOX_ODDPAY,''),
                   ISNULL(BRFILE.BR_LBOX_LC,''),
                   ISNULL(BRFILE.BR_LBOX_NOPRIN,''),
                   ISNULL(BRFILE.BR_LBOX_FLASH,''),
                   ISNULL(BRFILE.BR_LBOX_ACTIONCD,''),
                   BRFILE.BR_LBOX_MAXCON,
                   BRFILE.BR_LBOX_MAXREC,
                   ISNULL(BRFILE.BR_LBOX_PL,''),
                   ISNULL(BRFILE.BR_LBOX_JD,''),
                   ISNULL(BRFILE.BR_LBOX,''),
                   ISNULL(BRFILE.BR_LATECHG_ON_LBOX,''),
                   ISNULL(BRFILE.BR_ALLOW_OV_TRCD,''),
                   ISNULL(BRFILE.BR_GLOBAL_QUEUE,''),
                   ISNULL(BRFILE.BR_LBOX_PAYOFF_NONCASH,''),
                   ISNULL(BRFILE.BR_LBOX_PAYOFF_OV,''),
                   ISNULL(BRFILE.BR_COLLECT_MANAGER,''),
                   BRFILE.BR_COLLECT_PHONE,
                   BRFILE.BR_COLLECT_EXT,
                   ISNULL(BRFILE.BR_ALT_INTO_THIS_BRANCH,''),
                   ISNULL(BRFILE.BR_POFF_TITLE_WINDOW,''),
                   ISNULL(BRFILE.BR_COUPON_STATEMENT,''),
                   ISNULL(BRFILE.BR_SUPERVISOR,''),
                   ISNULL(BRFILE.BR_AREA_MANAGER,''),
                   BRFILE.BR_MIN_MONTHS_DEF,
                   ISNULL(BRFILE.BR_CC_FLAG,''),
                   ISNULL(BRFILE.BR_CC_ADR1,''),
                   ISNULL(BRFILE.BR_CC_CITY,''),
                   ISNULL(BRFILE.BR_CC_STATE,''),
                   ISNULL(BRFILE.BR_CC_ZIP,''),
                   BRFILE.BR_MIN_MONTHS_D2,
                   ISNULL(BRFILE.BR_SKIP_MIDSTREAM_TRW,''),
                   BRFILE.BR_GL_ESCROW_BALANCE,
                   BRFILE.BR_GL_ESCROW_PAYABLE,
                   ISNULL(BRFILE.BR_ALLOW_FORCE_OPEN_FG,''),
                   BRFILE.BR_ACH_BANK_NO,
                   ISNULL(BRFILE.BR_WIMENU_CHECK_WRITING,''),
                   ISNULL(BRFILE.BR_ADP_PAY_GROUP,''),
                   ISNULL(BRFILE.BR_ALLOW_SCANNED_CKS,''),
                   BRFILE.BR_CHECK_DEPOSIT_GL,
                   ISNULL(BRFILE.BR_OPTION_PST_LASER,''),
                   ISNULL(BRFILE.BR_USE_DEBIT_CARDS,''),
                   BRFILE.BR_GL_DEBIT_CARD_PROCEEDS,
                   BRFILE.BR_WKDAY_BANKID,
                   ISNULL(BRFILE.BR_PURGE_NOW_FLAG,''),
                   ISNULL(BRFILE.BR_EMAIL,''),
                   ISNULL(BRFILE.BR_MANAGER_EMAIL,''),
                   BRFILE.BR_VERITEC_LICENSE,
                   ISNULL(BRFILE.BR_TEST_PP_AUTO_INSCOVR,'')
            FROM DBO.BRFILE
            WHERE BRFILE.BR_NO >= :QBR-WBEG-NO
              AND BRFILE.BR_NO <= :QBR-WEND-NO
            ORDER BY BR_NO
           END-EXEC.

           EXEC SQL
               OPEN BR_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE ZEROES TO QBR-WBEG-NO
                          QBR-WEND-NO.

           MOVE STAT---GOOD TO BR-CURSOR-STAT.

      *-----------------------------------------------------------------
       READ-BR-FILE.
           PERFORM READ-IT.
           MOVE BR-SELECT-PATH TO E-FILE.
           MOVE BR-NO TO E-KEYX.

           MOVE BR-NO TO QBR-NO.

           EXEC SQL
            SELECT  BRFILE.BR_NO
              INTO :QBR-NO 
              FROM DBO.BRFILE
             WHERE BRFILE.BR_NO = :QBR-NO
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO READ-BR-FILE.

           IF ( IO-FG = 0 )
              PERFORM GET-BR-FIELDS.

      *-----------------------------------------------------------------
       READ-BR-FILE-NEXT.
           PERFORM READ-IT.
           MOVE BR-SELECT-PATH TO E-FILE.
           MOVE BR-NO TO E-KEYX.
           INITIALIZE QBR-REC.

           IF ( BR-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH BR_CURSOR INTO :QBR-REC
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO READ-BR-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-BR-FIELDS.

      *-----------------------------------------------------------------
       CLOSE-BR-FILE.
           PERFORM CLOSE-IT.
           IF ( BR-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE BR_CURSOR
              END-EXEC
              MOVE STAT---BAD TO BR-CURSOR-STAT.
 


       COPY "LIBGB/GBWKIN.CPY".
       COPY "LIBGB/GBGR1RN.CPY".
      * COPY "LIBGL/GLGT1RN.CPY".
      * COPYMEMBER: LIBGL/GLGT1R
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
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    CREATE PATH FOR FILE SELECT AND SQL TABLE NAME
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( EXT-IO-GTFILE NOT = "S" )
              MOVE GT-PATH-BEG             TO GT-SELECT-PATH-BEG
              MOVE GT-PATH-FILE            TO GT-SELECT-PATH-FILE
           ELSE
              MOVE GT-PATH-SQL             TO GT-SELECT-PATH.

      *-----------------------------------------------------------------
       OPEN-GT1-FILE.
           PERFORM LOAD-GT1-FILE.
           PERFORM OPEN-IT.
           MOVE GT-SELECT-PATH TO E-FILE.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       START-GT1-FILE.
           PERFORM START-IT.
           MOVE GT-SELECT-PATH TO E-FILE.
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

              MOVE JULIAN-END           TO SQL-DATE-YYYYMMDD
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

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           EXEC SQL
            DECLARE GT1_CURSOR CURSOR FOR
            SELECT ISNULL(GTFILE.GT_FLAG,''),
                   ISNULL(GTFILE.GT_REF,''),
                   ISNULL(CAST(GTFILE.GT_DATE1 AS VARCHAR(10)),'0'),
                   GTFILE.GT_SOURCE,
                   ISNULL(CAST(GTFILE.GT_DATE2 AS VARCHAR(10)),'0'),
                   GTFILE.GT_BRANCH,
                   GTFILE.GT_ACCTNO,
                   ISNULL(CAST(GTFILE.GT_DATE3 AS VARCHAR(10)),'0'),
                   ISNULL(GTFILE.GT_DRCRFG,''),
                   GTFILE.GT_LINE,
                   ISNULL(CAST(GTFILE.GT_LTOUCH_DATE
                                                  AS VARCHAR(10)),'0'),
                   GTFILE.GT_AMT,
                   ISNULL(GTFILE.GT_EXPL,''),
                   GTFILE.GT_PERIOD,
                   ISNULL(GTFILE.GT_SMITH_SENT,''),
                   ISNULL(GTFILE.GT_POST_USERID,''),
                   ISNULL(CAST(GTFILE.GT_POST_DATE
                                                  AS VARCHAR(10)),'0'),
                   GTFILE.GT_POST_TIME
            FROM DBO.GTFILE
            WHERE (( GTFILE.GT_SOURCE     >= :QGT1-WBEG-SOURCE
              AND    CAST(GTFILE.GT_DATE2 AS VARCHAR(10))
                                          >= :QGT1-WBEG-DATE2
              AND    GTFILE.GT_BRANCH     >= :QGT1-WBEG-BRANCH
              AND    GTFILE.GT_ACCTNO     >= :QGT1-WBEG-ACCTNO
              AND    CAST(GTFILE.GT_DATE3 AS VARCHAR(10))
                                          >= :QGT1-WBEG-DATE3
              AND    GTFILE.GT_DRCRFG     >= :QGT1-WBEG-DRCRFG
              AND    GTFILE.GT_LINE       >= :QGT1-WBEG-LINE )
               OR  ( GTFILE.GT_SOURCE     >= :QGT1-WBEG-SOURCE
              AND    CAST(GTFILE.GT_DATE2 AS VARCHAR(10))
                                          >= :QGT1-WBEG-DATE2
              AND    GTFILE.GT_BRANCH     >= :QGT1-WBEG-BRANCH
              AND    GTFILE.GT_ACCTNO     >= :QGT1-WBEG-ACCTNO
              AND    CAST(GTFILE.GT_DATE3 AS VARCHAR(10))
                                          >= :QGT1-WBEG-DATE3
              AND    GTFILE.GT_DRCRFG     >= :QGT1-WSBEG-DRCRFG
              AND    GTFILE.GT_LINE       >= 0 )
               OR  ( GTFILE.GT_SOURCE     >= :QGT1-WBEG-SOURCE
              AND    CAST(GTFILE.GT_DATE2 AS VARCHAR(10))
                                          >= :QGT1-WBEG-DATE2
              AND    GTFILE.GT_BRANCH     >= :QGT1-WBEG-BRANCH
              AND    GTFILE.GT_ACCTNO     >= :QGT1-WBEG-ACCTNO
              AND    CAST(GTFILE.GT_DATE3 AS VARCHAR(10))
                                          >= :QGT1-WSBEG-DATE3
              AND    GTFILE.GT_DRCRFG     >= ''
              AND    GTFILE.GT_LINE       >= 0 )
               OR  ( GTFILE.GT_SOURCE     >= :QGT1-WBEG-SOURCE
              AND    CAST(GTFILE.GT_DATE2 AS VARCHAR(10))
                                          >= :QGT1-WBEG-DATE2
              AND    GTFILE.GT_BRANCH     >= :QGT1-WBEG-BRANCH
              AND    GTFILE.GT_ACCTNO     >= :QGT1-WSBEG-ACCTNO
              AND    CAST(GTFILE.GT_DATE3 AS VARCHAR(10))
                                          >= '1900-01-01'
              AND    GTFILE.GT_DRCRFG     >= ''
              AND    GTFILE.GT_LINE       >= 0 )
               OR  ( GTFILE.GT_SOURCE     >= :QGT1-WBEG-SOURCE
              AND    CAST(GTFILE.GT_DATE2 AS VARCHAR(10))
                                          >= :QGT1-WBEG-DATE2
              AND    GTFILE.GT_BRANCH     >= :QGT1-WSBEG-BRANCH
              AND    GTFILE.GT_ACCTNO     >= 0
              AND    CAST(GTFILE.GT_DATE3 AS VARCHAR(10))
                                          >= '1900-01-01'
              AND    GTFILE.GT_DRCRFG     >= ''
              AND    GTFILE.GT_LINE       >= 0 )
               OR  ( GTFILE.GT_SOURCE     >= :QGT1-WBEG-SOURCE
              AND    CAST(GTFILE.GT_DATE2 AS VARCHAR(10))
                                          >= :QGT1-WSBEG-DATE2
              AND    GTFILE.GT_BRANCH     >= 0
              AND    GTFILE.GT_ACCTNO     >= 0
              AND    CAST(GTFILE.GT_DATE3 AS VARCHAR(10))
                                          >= '1900-01-01'
              AND    GTFILE.GT_DRCRFG     >= ''
              AND    GTFILE.GT_LINE       >= 0 )
               OR  ( GTFILE.GT_SOURCE     >= :QGT1-WSBEG-SOURCE
              AND    CAST(GTFILE.GT_DATE2 AS VARCHAR(10))
                                          >= '1900-01-01'
              AND    GTFILE.GT_BRANCH     >= 0
              AND    GTFILE.GT_ACCTNO     >= 0
              AND    CAST(GTFILE.GT_DATE3 AS VARCHAR(10))
                                          >= '1900-01-01'
              AND    GTFILE.GT_DRCRFG     >= ''
              AND    GTFILE.GT_LINE       >= 0 ))
              AND (  GTFILE.GT_SOURCE     <= :QGT1-WEND-SOURCE
              AND    CAST(GTFILE.GT_DATE2 AS VARCHAR(10))
                                          <= :QGT1-WEND-DATE2
              AND    GTFILE.GT_BRANCH     <= :QGT1-WEND-BRANCH
              AND    GTFILE.GT_ACCTNO     <= :QGT1-WEND-ACCTNO
              AND    CAST(GTFILE.GT_DATE3 AS VARCHAR(10))
                                          <= :QGT1-WEND-DATE3
              AND    GTFILE.GT_DRCRFG     <= :QGT1-WEND-DRCRFG
              AND    GTFILE.GT_LINE       <= :QGT1-WEND-LINE )
            ORDER BY GTFILE.GT_SOURCE,
                     GTFILE.GT_DATE2,
                     GTFILE.GT_BRANCH,
                     GTFILE.GT_ACCTNO,
                     GTFILE.GT_DATE3,
                     GTFILE.GT_DRCRFG,
                     GTFILE.GT_LINE
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

      *-----------------------------------------------------------------
       READ-GT1-FILE.
           PERFORM READ-IT.
           MOVE GT-SELECT-PATH TO E-FILE.
           MOVE GT1-KEY TO E-KEYX.

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
                   GTFILE.GT_AMT,
                   GTFILE.GT_EXPL,
                   GTFILE.GT_PERIOD
              INTO 
                  :QGT-FLAG,
                  :QGT-REF,
                  :QGT-DATE1,
                  :QGT-SOURCE,
                  :QGT-DATE2,
                  :QGT-BRANCH,
                  :QGT-ACCTNO,
                  :QGT-DATE3,
                  :QGT-DRCRFG,
                  :QGT-LINE,
                  :QGT-AMT,
                  :QGT-EXPL,
                  :QGT-PERIOD                
              FROM DBO.GTFILE
             WHERE GTFILE.GT_SOURCE = :QGT-SOURCE
               AND GTFILE.GT_DATE2  = :QGT-DATE2
               AND GTFILE.GT_BRANCH = :QGT-BRANCH
               AND GTFILE.GT_ACCTNO = :QGT-ACCTNO
               AND GTFILE.GT_DATE3  = :QGT-DATE3
               AND GTFILE.GT_DRCRFG = :QGT-DRCRFG
               AND GTFILE.GT_LINE   = :QGT-LINE
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-GT1-FILE.

           IF ( IO-FG = 0 )
              PERFORM GET-GT-FIELDS.

      *-----------------------------------------------------------------
       READ-GT1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE GT-SELECT-PATH TO E-FILE.
           MOVE GT1-KEY TO E-KEYX.
           INITIALIZE QGT-REC.

           IF ( GT1-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH GT1_CURSOR INTO :QGT-REC
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-GT1-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-GT-FIELDS.

      *-----------------------------------------------------------------
       CLOSE-GT1-FILE.
           PERFORM CLOSE-IT.
           IF ( GT1-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE GT1_CURSOR
              END-EXEC
              MOVE STAT---BAD TO GT1-CURSOR-STAT.



      * COPY "LIBGL/GLGT2RN.CPY".
      * COPYMEMBER: LIBGL/GLGT2RN
      *============================================================
       LOAD-GT2-FILE.
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
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    CREATE PATH FOR FILE SELECT AND SQL TABLE NAME
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( EXT-IO-GTFILE NOT = "S" )
              MOVE GT-PATH-BEG             TO GT-SELECT-PATH-BEG
              MOVE GT-PATH-FILE            TO GT-SELECT-PATH-FILE
           ELSE
              MOVE GT-PATH-SQL             TO GT-SELECT-PATH.

      *-----------------------------------------------------------------
       OPEN-GT2-FILE.
           PERFORM LOAD-GT2-FILE.
           PERFORM OPEN-IT.
           MOVE GT-SELECT-PATH TO E-FILE.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       START-GT2-FILE.
           PERFORM START-IT.
           MOVE GT-SELECT-PATH TO E-FILE.
           MOVE GT2-KEY TO E-KEYX.
           IF ( GT2-CURSOR-STAT-GOOD )
              PERFORM CLOSE-GT2-FILE.

           IF ( QGT2-WEND-DATE1 = SPACES )
              MOVE GT-DATE1            TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QGT2-WBEG-DATE1

              MOVE JULIAN-END          TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QGT2-WEND-DATE1.

           EXEC SQL
            DECLARE GT2_CURSOR CURSOR FOR
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
                   GTFILE.GT_AMT,
                   GTFILE.GT_EXPL,
                   GTFILE.GT_PERIOD
              FROM DBO.GTFILE
             WHERE GTFILE.GT_DATE1 >= :QGT2-WBEG-DATE1
               AND GTFILE.GT_DATE1 <= :QGT2-WEND-DATE1
             ORDER BY GTFILE.GT_DATE1,
                      GTFILE.GT_SOURCE,
                      GTFILE.GT_DATE2,
                      GTFILE.GT_BRANCH,
                      GTFILE.GT_ACCTNO,
                      GTFILE.GT_DATE3,
                      GTFILE.GT_DRCRFG,
                      GTFILE.GT_LINE
           END-EXEC.

           EXEC SQL
               OPEN GT2_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE SPACES TO QGT2-WBEG-DATE1
                          QGT2-WEND-DATE1.

           MOVE STAT---GOOD TO GT2-CURSOR-STAT.

      *-----------------------------------------------------------------
       READ-GT2-FILE.
           PERFORM READ-IT.
           MOVE GT-SELECT-PATH TO E-FILE.
           MOVE GT2-KEY TO E-KEYX.

           MOVE GT-DATE1            TO SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO QGT-DATE1.

           EXEC SQL
            SELECT ISNULL(GTFILE.GT_FLAG,''),
                   ISNULL(GTFILE.GT_REF,''),
                   ISNULL(CAST(GTFILE.GT_DATE1 AS VARCHAR(10)),'0'),
                   GTFILE.GT_SOURCE,
                   ISNULL(CAST(GTFILE.GT_DATE2 AS VARCHAR(10)),'0'),
                   GTFILE.GT_BRANCH,
                   GTFILE.GT_ACCTNO,
                   ISNULL(CAST(GTFILE.GT_DATE3 AS VARCHAR(10)),'0'),
                   ISNULL(GTFILE.GT_DRCRFG,''),
                   GTFILE.GT_LINE,
                   ISNULL(CAST(GTFILE.GT_LTOUCH_DATE
                                                  AS VARCHAR(10)),'0'),
                   GTFILE.GT_AMT,
                   ISNULL(GTFILE.GT_EXPL,''),
                   GTFILE.GT_PERIOD,
                   ISNULL(GTFILE.GT_SMITH_SENT,''),
                   ISNULL(GTFILE.GT_POST_USERID,''),
                   ISNULL(CAST(GTFILE.GT_POST_DATE
                                                  AS VARCHAR(10)),'0'),
                   GTFILE.GT_POST_TIME
            INTO :QGT-REC
            FROM DBO.GTFILE
            WHERE GTFILE.GT_DATE1 = :QGT-DATE1
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-GT2-FILE.

           IF ( IO-FG = 0 )
              PERFORM GET-GT-FIELDS.

      *-----------------------------------------------------------------
       READ-GT2-FILE-NEXT.
           PERFORM READ-IT.
           MOVE GT-SELECT-PATH TO E-FILE.
           MOVE GT2-KEY TO E-KEYX.
           INITIALIZE QGT-REC.

           IF ( GT2-CURSOR-STAT-GOOD )
              EXEC SQL
               FETCH GT2_CURSOR 
                INTO 
                     :QGT-FLAG,
                     :QGT-REF,
                     :QGT-DATE1,
                     :QGT-SOURCE,
                     :QGT-DATE2,
                     :QGT-BRANCH,
                     :QGT-ACCTNO,
                     :QGT-DATE3,
                     :QGT-DRCRFG,
                     :QGT-LINE,
                     :QGT-AMT,
                     :QGT-EXPL,
                     :QGT-PERIOD           
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-GT2-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-GT-FIELDS.

      *-----------------------------------------------------------------
       CLOSE-GT2-FILE.
           PERFORM CLOSE-IT.
           IF ( GT2-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE GT2_CURSOR
              END-EXEC
              MOVE STAT---BAD TO GT2-CURSOR-STAT.


	  
      * COPY "LIBGL/GLGL1RN.CPY".
      * COPYMEMBER: LIBGL/GLGL1RN
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
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    CREATE PATH FOR FILE SELECT AND SQL TABLE NAME
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( EXT-IO-GLFILE NOT = "S" )
              MOVE GL-PATH-BEG             TO GL-SELECT-PATH-BEG
              MOVE GL-PATH-FILE            TO GL-SELECT-PATH-FILE
           ELSE
              MOVE GL-PATH-SQL             TO GL-SELECT-PATH.

      *-----------------------------------------------------------------
       OPEN-GL1-FILE.
           PERFORM LOAD-GL1-FILE.
           PERFORM OPEN-IT.
           MOVE GL-SELECT-PATH TO E-FILE.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       START-GL1-FILE.
           PERFORM START-IT.
           MOVE GL-SELECT-PATH TO E-FILE.
           MOVE GL1-KEY TO E-KEYX.

           IF ( GL1-CURSOR-STAT-GOOD )
              PERFORM CLOSE-GL1-FILE.

           IF ( QGL1-WEND-ACCOUNT = ZEROES    ) OR
              ( QGL1-WEND-ACCOUNT NOT NUMERIC )
              MOVE GL-BRANCH        TO QGL1-WBEG-BRANCH
              MOVE GL-ACCOUNT       TO QGL1-WBEG-ACCOUNT
              MOVE ALL "9"          TO QGL1-WEND-BRANCH
                                       QGL1-WEND-ACCOUNT.

           EXEC SQL
            DECLARE GL1_CURSOR CURSOR FOR
            SELECT ISNULL(GLFILE.GL_FLAG,''),
                   GLFILE.GL_BR_ACCOUNT,
                   ISNULL(CAST(GLFILE.GL_LTOUCH_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(GLFILE.GL_DESCRIPTION,''),
                   GLFILE.GL_CONSOL,
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
                   GLFILE.GL_CYBUD_1,
                   GLFILE.GL_CYBUD_2,
                   GLFILE.GL_CYBUD_3,
                   GLFILE.GL_CYBUD_4,
                   GLFILE.GL_CYBUD_5,
                   GLFILE.GL_CYBUD_6,
                   GLFILE.GL_CYBUD_7,
                   GLFILE.GL_CYBUD_8,
                   GLFILE.GL_CYBUD_9,
                   GLFILE.GL_CYBUD_10,
                   GLFILE.GL_CYBUD_11,
                   GLFILE.GL_CYBUD_12,
                   GLFILE.GL_LYBUD_1,
                   GLFILE.GL_LYBUD_2,
                   GLFILE.GL_LYBUD_3,
                   GLFILE.GL_LYBUD_4,
                   GLFILE.GL_LYBUD_5,
                   GLFILE.GL_LYBUD_6,
                   GLFILE.GL_LYBUD_7,
                   GLFILE.GL_LYBUD_8,
                   GLFILE.GL_LYBUD_9,
                   GLFILE.GL_LYBUD_10,
                   GLFILE.GL_LYBUD_11,
                   GLFILE.GL_LYBUD_12,
                   GLFILE.GL_NYBUD_1,
                   GLFILE.GL_NYBUD_2,
                   GLFILE.GL_NYBUD_3,
                   GLFILE.GL_NYBUD_4,
                   GLFILE.GL_NYBUD_5,
                   GLFILE.GL_NYBUD_6,
                   GLFILE.GL_NYBUD_7,
                   GLFILE.GL_NYBUD_8,
                   GLFILE.GL_NYBUD_9,
                   GLFILE.GL_NYBUD_10,
                   GLFILE.GL_NYBUD_11,
                   GLFILE.GL_NYBUD_12,
                   ISNULL(GLFILE.GL_TYPE,''),
                   ISNULL(GLFILE.GL_CLEAR,''),
                   GLFILE.GL_NUMBER1,
                   GLFILE.GL_NUMBER2,
                   ISNULL(GLFILE.GL_FS_FLAG,''),
                   ISNULL(GLFILE.GL_REPORTS_1,''),
                   ISNULL(GLFILE.GL_REPORTS_2,''),
                   ISNULL(GLFILE.GL_REPORTS_3,''),
                   ISNULL(GLFILE.GL_REPORTS_4,''),
                   ISNULL(GLFILE.GL_REPORTS_5,''),
                   GLFILE.GL_LYOPENBAL,
                   ISNULL(GLFILE.GL_DLALLOC_GL,''),
                   ISNULL(GLFILE.GL_SPANISH_DESC,'')
            FROM DBO.GLFILE
            WHERE GLFILE.GL_BR_ACCOUNT >= :QGL1-WBEG-BR-ACCOUNT
              AND GLFILE.GL_BR_ACCOUNT <= :QGL1-WEND-BR-ACCOUNT
            ORDER BY GLFILE.GL_BR_ACCOUNT
           END-EXEC.

           EXEC SQL
               OPEN GL1_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE ZEROES TO QGL1-WBEG-BRANCH
                          QGL1-WBEG-ACCOUNT
                          QGL1-WEND-BRANCH
                          QGL1-WEND-ACCOUNT.

           MOVE STAT---GOOD TO GL1-CURSOR-STAT.

      *-----------------------------------------------------------------
       READ-GL1-FILE.
           PERFORM READ-IT.
           MOVE GL-SELECT-PATH TO E-FILE.
           MOVE GL1-KEY TO E-KEYX.

           MOVE GL-BR-ACCOUNT TO QGL-BR-ACCOUNT.

           EXEC SQL
            SELECT GLFILE.GL_BR_ACCOUNT,
                   GLFILE.GL_DESCRIPTION
              INTO 
                  :QGL-BR-ACCOUNT,
                  :QGL-DESCRIPTION
              FROM DBO.GLFILE
             WHERE GLFILE.GL_BR_ACCOUNT = :QGL-BR-ACCOUNT
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-GL1-FILE.

           IF ( IO-FG = 0 )
              PERFORM GET-GL-FIELDS.

      *-----------------------------------------------------------------
       READ-GL1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE GL-SELECT-PATH TO E-FILE.
           MOVE GL1-KEY TO E-KEYX.
           INITIALIZE QGL-REC.

           IF ( GL1-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH GL1_CURSOR INTO :QGL-REC
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-GL1-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-GL-FIELDS.

      *-----------------------------------------------------------------
       CLOSE-GL1-FILE.
           PERFORM CLOSE-IT.
           IF ( GL1-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE GL1_CURSOR
              END-EXEC
              MOVE STAT---BAD TO GL1-CURSOR-STAT.



       COPY "LIBGB/FERRORS.CPY".

      ********************************************
       END-PROGRAM SECTION.
      ********************************************
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
              MOVE EXIT-PATHNAME TO FORM-PATHNAME.
       STOP-RUN.
           IF EXT-ROUTE-BUF = "00" OR "40" OR "50"
              DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN
              DESTROY TRANLS-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.
           EXIT PROGRAM.
