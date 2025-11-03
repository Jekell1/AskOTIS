       IDENTIFICATION DIVISION.
       PROGRAM-ID.      GATXNF.
       DATE-WRITTEN.    12/14/04.
      ******************************************************************
      *    DESCRIPTION:  GEORGIA TAX REPORTS
      *                  FEE-CODE
      *                  POLICIES ISSUED, PREMIUMS CHARGED, PREMIUMS
      *                     REFUNDED, NET PREMIUMS
      *                  (TAKEN FROM NFMORP)
      ***********
      *** NOTE: THE PROGRAM RESULTS WILL NOT MATCH A15 DUE TO A BUG IN A15.
      *** NOTE: A15 TEST FOR SKIPPING BOOKED AND VOIDED SAME MONTH NOT WORKING
      *** NOTE: TESTS 1119 TO 112619 FOR ACCT 52388 BR 102. MISSING THE DAY
      ***********
      *
      * REV:
      *  BAH 041214 NEW FOR WORLD GA TAX REPORT FORM, WORLD PR# 364
      *  BAH 160125 SPLIT OUT LTFILE, PD0003
      *
      * KEC 2016.0315 {PD#00003} PARADATA <A30> [JAY CISZEWSKI]
      *         CHANGED CD-FEE???? TO CD-GC-????.
      *  BAH 170602 ACTIVATE 8 DIGIT DATES, PD0006
      *  BAH 181207 GLOBAL LNFILE
      *  BAH 190122 SPLIT CDFILE
      *  BAH 20190926 REMOVED ACCESS-CALL LNFILE, TRL, TRS
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "LIBGB/GBFSPR.CPY".

      * TOTALS FILE
           SELECT WK1-FILE ASSIGN WK1-PATH
                  ORGANIZATION INDEXED
                  ACCESS DYNAMIC
                  LOCK MODE AUTOMATIC WITH LOCK ON RECORD
                  RECORD KEY WK1-KEY
                  FILE STATUS FILE-STAT.
           SELECT WK2-FILE ASSIGN WK2-PATH
                  ORGANIZATION INDEXED
                  ACCESS DYNAMIC
                  LOCK MODE AUTOMATIC WITH LOCK ON RECORD
                  RECORD KEY WK2-KEY
                  FILE STATUS FILE-STAT.
       I-O-CONTROL.
       DATA DIVISION.
       FILE SECTION.

       COPY "LIBGB/GBFDPR.CPY".
      ******************************************************************
      *              SUMMARY TOTALS WORK RECORD
      *-----------------------------------------------------------------
      *    WK-FEEOURS-TOTALS(1) = NO
      *    WK-FEEOURS-TOTALS(2) = YES
      *
      ******************************************************************
       FD  WK1-FILE
           LABEL RECORDS ARE STANDARD.
       01  WK1-REC.
           03  WK1-KEY.
               05  WK-CODE         PIC 9.
               05  WK-ST.
                   07  WK-BRNO     PIC 9(4).
               05  WK-CLASS        PIC 9(3).
           03  WK-QTY              PIC S9(7)    COMP-3 OCCURS 2.
      * (1)-COVERAGE  (2)-NEW  (3)-REFUND (4)-PAYABLE(COMMISSION)
           03  WK-AMT              PIC S9(9)V99 COMP-3 OCCURS 5.
           03  WK-FEEOURS-TOTALS   OCCURS 2.
               05  WK-TOTAL-QTY    PIC S9(7)    COMP-3.
               05  WK-TOTAL-AMT    PIC S9(9)V99 COMP-3 OCCURS 5.
           03  WK-SALEFIN          PIC X(1).

      ******************************************************************
      *
      * SORTED LNFILE BY BRANCH, CLASS, ACCOUNT
      *
      ******************************************************************
       FD  WK2-FILE
           LABEL RECORDS ARE STANDARD.
       01  WK2-REC.
           03  WK2-KEY.
               05  WK2-BRANCH           PIC 9(4).
               05  WK2-CLASS            PIC 9(3).
               05  WK2-ACCTNO           PIC 9(8).
               05  WK2-DATE             PIC 9(8).
               05  WK2-CODE             PIC X.
               05  WK2-LAWCODE          PIC 99.
               05  WK2-LINE             PIC 9(3).

      ******************************************************************
      *
      *    W O R K I N G - S T O R A G E    S E C T I O N
      *
      ******************************************************************
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(37) VALUE
           "@(#)LP/GATXNF S34 10/01/21-08:32:14 >".

       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBGB/GB01GR.CPY".
       COPY "LIBGB/GB01GR_SQL.CPY".
       COPY "LIBGB/GBWSGR.CPY".
       COPY "LIBLP/LP01TRL.CPY".
       COPY "LIBLP/LP01TRL_SQL.CPY".
       COPY "LIBLP/LPWSTRL.CPY".
       COPY "LIBLP/LP01TRS.CPY".
       COPY "LIBLP/LP01TRS_SQL.CPY".
       COPY "LIBLP/LPWSTRS.CPY".
       COPY "LIBLP/LP01LN.CPY".
       COPY "LIBLP/LP01LN_SQL.CPY".
       COPY "LIBLP/LPWSLN.CPY".
       COPY "LIBLP/LP01LTF.CPY".
       COPY "LIBLP/LP01LTF_SQL.CPY".
       COPY "LIBLP/LPWSLTF.CPY".
       COPY "LIBLP/LP01BW.CPY".
       COPY "LIBLP/LP01BW_SQL.CPY".
       COPY "LIBLP/LPWSBW.CPY".
       COPY "LIBLP/LP01SP.CPY".
       COPY "LIBLP/LP01SPA.CPY".
       COPY "LIBLP/LP01SPB.CPY".
       COPY "LIBLP/LP01SPC.CPY".
       COPY "LIBLP/LP01SP_SQL.CPY".
       COPY "LIBLP/LPWSSP.CPY".
       COPY "LIBLP/LP01CDB.CPY".
       COPY "LIBLP/LP01CDB_SQL.CPY".
       COPY "LIBLP/LPWSCDB.CPY".
      *----------------------------------------------------------------
       01  WK1-IFN              PIC X(3)     VALUE "WK1".
       01  WK1-PATH             PIC X(35).
       01  WK2-PATH             PIC X(35).

      ******************************************************************
      *   LINE WORKERS     *
      ******************************************************************
       01  LN-WORKERS.
           03  LN-FEED          PIC 999      VALUE 0.
           03  LN-COUNT         PIC 999      VALUE 99.
           03  LN-WK            PIC 999      VALUE 0.
       01  PAGE-NUMBER          PIC 9(4)     VALUE 0.

       01  BR-PAGE-NUMBER       PIC 9(4) COMP-3 VALUE 0.

      ******************************************************************
      *   MISC WORKERS     *
      ******************************************************************
       01  SUB                  PIC 9(6)     COMP-3.
       01  SUB-LT               PIC 9(6)     COMP-3.
       01  FEECODE-PAYABLE      PIC S9(5)V99 COMP-3 VALUE 0.
       01  FIRST-TIME           PIC X        VALUE "Y".
       01  RECORDS-FOUND        PIC X        VALUE " ".
           88  NO-RECORDS-FOUND              VALUE "N".
       01  MESS-LIST            PIC X(5)     VALUE "MSEL.".
       01  HOLD-BR              PIC 9(4)     COMP-3 VALUE 0.
       01  HOLD-FEEOURS-N       PIC X(1)     VALUE SPACES.
       01  HOLD-FEEOURS-Y       PIC X(1)     VALUE SPACES.
       01  SAVE-WK1-KEY         PIC X(8).
       01  SAVE-CODE            PIC 9.
       01  SAVE-ST.
           03  SAVE-BR          PIC 9(4).
       01  LAST-ST              PIC XX.
       01  FIRST-TOT            PIC X.
       01  VALID-FEE            PIC X        VALUE " ".
       01  DASH-LINE            PIC X(132)   VALUE ALL "-".

       01  VOID-FLAG            PIC X        VALUE "N".
       01  INCLUDE-FLAG         PIC X        VALUE "N".
       01  INS-ACTIVITY         PIC X        VALUE "N".
       01  REC-EXISTS           PIC X        VALUE "N".
       01  TOTAL-FG             PIC X        VALUE "N".

       01  ONE-NF               PIC 9 VALUE 0.
       01  NON-FILING-N         PIC S9(7)V99 COMP-3 VALUE 0.
       01  NON-FILING-Y         PIC S9(7)V99 COMP-3 VALUE 0.
       01  NEW-COVERAGE         PIC S9(7)V99 COMP-3 VALUE 0.
       01  NEW-NF               PIC S9(7)V99 COMP-3 VALUE 0.
       01  HOLD-NF-REFUND       PIC S9(7)V99 COMP-3 VALUE 0.
       01  NF-REFUND            PIC S9(7)V99 COMP-3 VALUE 0.
       01  REFUND-DATE          PIC 9(8)     COMP-3 VALUE 0.
       01  RPT-END-DATE         PIC 9(8)     COMP-3 VALUE 0.
       01  BEGINNING-DATE       PIC 9(8)     COMP-3 VALUE 0.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    TOT 1 = NO;  2 = YES
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       01  LINE-TOTALS.
           03  TOTALS-ALL-YES   OCCURS 2 TIMES.
               05  TOT-QTY      PIC S9(7)    COMP-3 OCCURS 2.
               05  TOT-AMT      PIC S9(9)V99 COMP-3 OCCURS 5.

       01  OLD-ACCTNO           PIC 9(8).
       01  OLD-ACCTNOX REDEFINES OLD-ACCTNO.
           03  OLD-BR           PIC 9(4).
           03  OLD-AC           PIC 9(5).
           03  OLD-CD           PIC 9.
       01  CLASS-FG             PIC X(3)     VALUE "   ".
           88  CLASS-BEG                     VALUE "BEG".
           88  CLASS-END                     VALUE "END".

      ******************************************************************
      *     S C R E E N   F I E L D S   *
      ******************************************************************
       COPY "LIBGB/EOBUF_EXT.CPY".
           03  BEG-BRANCH      PIC 9(4).
           03  END-BRANCH      PIC 9(4).
           03  BEG-CLASS       PIC 9(3).
           03  END-CLASS       PIC 9(3).
           03  FEE-CODE        PIC XX.
           03  BEG-DATE.
               05  BEG-MONTH   PIC 9(2).
               05  BEG-YEAR    PIC 9(2).
           03  BDATE REDEFINES BEG-DATE PIC 9(4).
           03  END-DATE.
               05  END-MONTH   PIC 9(2).
               05  END-YEAR    PIC 9(2).
           03  EDATE REDEFINES END-DATE PIC 9(4).
           03  SUMMARY-ONLY    PIC X.
           03  GROUP-FLAG      PIC X(1).
           03  ENT-GROUP       PIC X(4).

       01  VERSION-CONTROL     PIC X    VALUE "A".

       01  DESC-LIST           PIC X(5)  VALUE "DESC.".
       01  DESC-BUF            PIC X(20) VALUE SPACES.

       01  SV-BEG-BR           PIC 9(4)  VALUE 9999.
       01  SV-END-BR           PIC 9(4)  VALUE 9999.
       01  SV-GROUP-FLAG       PIC X     VALUE SPACES.
       01  SV-ENT-GROUP        PIC X(4)  VALUE SPACES.

      ******************************************************************
      *     P R I N T   B U F F E R S   *
      ******************************************************************
       01  HEAD1.
           03  H-DATE           PIC X(8)     VALUE " ".
           03  FILLER           PIC XX       VALUE " ".
           03  H-TIME           PIC X(8)     VALUE " ".
           03  FILLER           PIC X(28)    VALUE " ".
           03  H-NAME           PIC X(40)    VALUE " ".
           03  FILLER           PIC X(29)    VALUE " ".
           03  FILLER           PIC X(8)     VALUE "COMPANY ".
           03  FILLER           PIC X(5)     VALUE "PAGE:".
           03  H-PAGE-NO        PIC ZZZ9.

       01  HEAD2.
           03  FILLER           PIC X(10)    VALUE "USER ID:".
           03  H-USERID         PIC X(10) VALUE " ".
           03  FILLER           PIC X(26)    VALUE " ".
           03  FILLER           PIC X(41) VALUE
           " FEE-CODE INSURANCE PREMIUM/CANCEL REPORT".

       01  HEAD3.
           03  FILLER           PIC X(10) VALUE "FEE CODE: ".
           03  H-CODE           PIC X(2).
           03  FILLER           PIC X     VALUE " ".
           03  H-CODENAME       PIC X(20) VALUE " ".
           03  FILLER           PIC X(23) VALUE " ".
           03  H-DATE1          PIC 99/99/99.
           03  FILLER           PIC X(6) VALUE " THRU ".
           03  H-DATE2          PIC 99/99/99.

       01  HEAD4                VALUE " ".
           03  H-PAGE-DESC      PIC X(46).
           03  H-STNO.
               05  H-BRNO       PIC Z(4).
           03  FILLER           PIC X.
           03  H-BRNAME         PIC X(20).
           03  FILLER           PIC X(45).
           03  H-PAGE-PROMPT    PIC X(12).
           03  H-BR-PAGE        PIC ZZZZ.

       01  HEAD5.
           03  FILLER           PIC X(105) VALUE SPACES.
           03  FILLER           PIC X(1)   VALUE "O".
           03  FILLER           PIC X(26)  VALUE SPACES.

       01  HEAD6.
           03  FILLER           PIC X(50) VALUE
           "ACCOUNT #        BORROWER             AGE      EFF".
           03  FILLER           PIC X(50) VALUE
           "ECT  TE   MATURD      TOTAL  REG PAY  PAYABLE     ".
           03  FILLER           PIC X(32) VALUE
           " INS U  DATE OF      INS    ORIG".

       01  HEAD7.
           03  FILLER           PIC X(50) VALUE
           "                 NAME                            D".
           03  FILLER           PIC X(50) VALUE
           "ATE  RM     DATE       NOTE   AMOUNT           PRE".
           03  FILLER           PIC X(32) VALUE
           "MIUM R   CANCEL   CANCEL  BRANCH".

       01  HEAD8.
           03  FILLER           PIC X(50) VALUE
           "TOT  **NEW ACCOUNTS**            NEW LOAN        N".
           03  FILLER           PIC X(50) VALUE
           "EW INS            INS        NET INS          PAYA".
           03  FILLER           PIC X(32) VALUE
           "BLE                             ".

       01  HEAD9.
           03  FILLER           PIC X(50) VALUE
           "CLS             TOTAL            COVERAGE       PR".
           03  FILLER           PIC X(50) VALUE
           "EMIUMS        REFUNDS         AMOUNT              ".
           03  FILLER           PIC X(32) VALUE
           "                                ".

      ******************************************************************
      *    D E T A I L - L I N E
      ******************************************************************
       01  DETAIL-LINE          PIC X(132) VALUE SPACES.

       01  DETAIL1 REDEFINES DETAIL-LINE.
           03  D-LOAN-NUM       PIC X(16).
           03  D-NUM REDEFINES D-LOAN-NUM.
               05  D-CLASS          PIC 99.
               05  FILLER           PIC X.
               05  D-NUMBER         PIC Z(6).
               05  D-H2             PIC X.
               05  D-CKDIGIT        PIC 9.
               05  FILLER           PIC X(5).
           03  FILLER           PIC X.
           03  D-NAME           PIC X(20).
           03  FILLER           PIC XX.
           03  D-AGE            PIC ZZ.
           03  FILLER           PIC X(4).
           03  D-LOANDATE       PIC 99/99/99.
           03  FILLER           PIC X.
           03  D-ORGTERM        PIC ZZ9.
           03  FILLER           PIC X.
           03  D-INSEXPR        PIC 99/99/99.
           03  FILLER           PIC X.
           03  D-COVRG          PIC ZZZ,ZZ9.99-.
           03  D-REGPYAMT       PIC Z(4)9.99.
           03  FILLER           PIC X.
           03  D-FEE-PAYABLE    PIC Z(4).99-   BLANK ZERO.
           03  FILLER           PIC X.
           03  D-INSPREM        PIC Z(4)9.99   BLANK ZERO.
           03  FILLER           PIC X.
           03  D-FEEOURS        PIC X(1).
           03  FILLER           PIC X.
           03  D-REFUND-DATE    PIC 99/99/99   BLANK ZERO.
           03  FILLER           PIC X.
           03  D-REFUND         PIC Z(4)9.99   BLANK ZERO.
           03  FILLER           PIC X(4).
           03  D-ORGBR          PIC 9(4).

       01  DETAIL2 REDEFINES DETAIL-LINE.
           03  FILLER           PIC X(3).
           03  D-QTY            PIC Z(5)9-          OCCURS 3.
           03  FILLER           PIC X(3).
           03  D-AMT            PIC ZZZ,ZZZ,ZZ9.99- OCCURS 7.

       01  RANGE-LINE REDEFINES DETAIL-LINE.
           03  FILLER           PIC X(20).
           03  RANGE-SEL        PIC X(112).

       01  ERROR-LINE REDEFINES DETAIL-LINE.
           03  D-STAR1          PIC X(20).
           03  FILLER           PIC X(2).
           03  D-ACCT-PROMPT    PIC X(10).
           03  D-ERR-ACCT       PIC 9(6).
           03  FILLER           PIC X(2).
           03  D-ERR-DATE       PIC 99/99/99.
           03  FILLER           PIC X(2).
           03  D-ERR-MESSG      PIC X(52).
           03  D-STAR2          PIC X(20).

      ******************************************************************
      *
      *     C O P Y    L I B R A R Y    W O R K E R S
      *
      ******************************************************************
       COPY "LIBWI/GRSCANW.CPY".
       COPY "LIBLP/LPSTATW.CPY".
       COPY "LIBLP/LPMDTEW.CPY".
       COPY "LIBWI/LCSCANW.CPY".
       COPY "LIBLP/LPAMTSW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBLP/LPBAGEW.CPY".
       COPY "LIBGB/GETPRW.CPY".
       COPY "LIBGB/MKTEMPW_CMD.CPY".
       COPY "LIBGB/SYSTEMW.CPY".
       COPY "LIBGB/GRWORKW.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/PASSWDW.CPY".
       COPY "LIBGB/BRSECUREW.CPY".
       COPY "LIBGB/DACCESSW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBLP/GATXNF_DEF.CPY".
       COPY "LIBLP/GATXNF_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

      *****************************************************************
      *
      *     P R O G R A M   C O N T R O L
      *
      *****************************************************************
       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBLP/GATXNF_SCN.CPY".

       PROCEDURE DIVISION
           USING FORM-PATHNAME EXIT-PATHNAME.

       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
               PR-FILE WK1-FILE WK2-FILE.
       COPY "LIBGB/DECLRP.CPY".
           PERFORM CLOSE-TRS1-FILE.
           PERFORM CLOSE-TRL1-FILE.
           PERFORM CLOSE-SP1-FILE.
           PERFORM CLOSE-LTF1-FILE.
           PERFORM CLOSE-LN1-FILE.
           PERFORM CLOSE-CDB1-FILE.
           PERFORM CLOSE-BW1-FILE.
           PERFORM CLOSE-GR1-FILE.
           PERFORM CLOSE-BR-FILE.
           PERFORM CLOSE-PR-FILE.
           CLOSE WK1-FILE WK2-FILE.
       COPY "LIBGB/DECLRP2.CPY".

      ******************************************************************
      *
      *    M A I N    M O D U L E
      *
      ******************************************************************
       MAIN-MODULE SECTION.

           PERFORM SQL-CONNECT.

      D    STOP MESS.
           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "GATXNF" TO VDU-FORM-NAME.

           PERFORM INITIALIZATION.

           IF EXT-EOBUF-ERRCD NOT = " "
              GO TO END-PROGRAM.

           IF GROUP-FLAG = "N"
              MOVE BEG-BRANCH TO BR-NO
              PERFORM START-BR-FILE
              GO TO NEXT-BRANCH-NO.

           MOVE ENT-GROUP TO GR-ID.
           PERFORM SEARCH-GR-INIT.
      *-----------------------------------------------------------------
       NEXT-GROUP-LOC.
           IF GROUP-FLAG NOT = "Y"
              GO TO NEXT-BRANCH-NO.

           PERFORM SEARCH-GR-NEXT.
           IF WGR-SRESULT = 0
              GO TO END-ROUTINE.

           MOVE WGR-SRESULT TO BR-NO.
           PERFORM READ-BR-FILE.
           IF IO-FG NOT = 0
              GO TO IO-ERROR.

           GO TO MOVE-BRANCH-PATH-INFO.

       NEXT-BRANCH-NO.
           PERFORM READ-BR-FILE-NEXT.
           IF IO-FG = 9 OR BR-NO > END-BRANCH
              GO TO END-ROUTINE.

       MOVE-BRANCH-PATH-INFO.

           MOVE EXT-FILPATH-BASE           TO FILPATH-BASE.
           MOVE BR-MACHINE                 TO FILPATH-MACHINE.
           MOVE BR-DATA-PATH               TO FILPATH-DATA.

           MOVE "B"                        TO BW-PATH-OVERRIDE
                                              LN-PATH-OVERRIDE
                                              LTF-PATH-OVERRIDE
                                              TRL-PATH-OVERRIDE
                                              TRS-PATH-OVERRIDE.

           PERFORM OPEN-LN1-FILE.
           PERFORM OPEN-BW1-FILE.
           PERFORM OPEN-LTF1-FILE.

           PERFORM SORT-TRL-FILE.
           PERFORM SORT-TRS-FILE.

           PERFORM CLOSE-WK2-FILE.
           PERFORM OPEN-WK2-FILE.

       NEXT-TR-REPORT.
           PERFORM READ-WK2-FILE-NEXT.
           IF IO-FG = 9
              GO TO END-OF-GROUP.

           IF WK2-CODE = "L"
              MOVE WK2-DATE    TO TL-DATE
              MOVE WK2-BRANCH  TO TL-BRNO
              MOVE WK2-CLASS   TO TL-CLASS
              MOVE WK2-ACCTNO  TO TL-ACCTNO
              MOVE WK2-LAWCODE TO TL-LAWCODE
              MOVE WK2-LINE    TO TL-LINE
              PERFORM READ-TRL1-FILE
           ELSE
              MOVE WK2-DATE    TO TS-DATE
              MOVE WK2-BRANCH  TO TS-BRNO
              MOVE WK2-CLASS   TO TS-CLASS
              MOVE WK2-ACCTNO  TO TS-ACCTNO
              MOVE WK2-LAWCODE TO TS-LAWCODE
              MOVE WK2-LINE    TO TS-LINE
              PERFORM READ-TRS1-FILE.
           IF IO-FG NOT = 0
              MOVE "TRANSACTION RECORD MISSING SINCE SORT" TO MESS
              PERFORM SEND-MESS
              GO TO IO-ERROR.

           IF WK2-CODE = "L"
              PERFORM CHECK-TRL-FEE-TABLE
           ELSE
              PERFORM CHECK-TRS-FEE-TABLE.

           MOVE WK2-BRANCH TO LN-OWNBR.
           MOVE WK2-ACCTNO TO LN-ACCTNO.
           PERFORM READ-LN1-FILE.
           IF IO-BAD
              MOVE "********************" TO D-STAR1 D-STAR2
              MOVE "ACCOUNT # " TO D-ACCT-PROMPT
              MOVE WK2-ACCTNO   TO D-ERR-ACCT
              MOVE WK2-DATE     TO DATE-YYYYMMDD
              PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
              MOVE DATE-MMDDYY  TO D-ERR-DATE
              MOVE "ACCOUNT NOT ON FILE!!!!!!!" TO D-ERR-MESSG
              MOVE 1 TO LN-FEED
              PERFORM WRITE-DETAIL-LINE
              GO TO NEXT-TR-REPORT.

           IF LN-POCD-INRESCISSION OR LN-POCD-RESCIND
              GO TO NEXT-TR-REPORT.

      * TEST IF LOAN BOOKED & VOIDED IN SELECTED DATE RANGE, THEN SKIP:
           IF LN-POCD = "**"
              IF LN-ENTDATE NOT < BEGINNING-DATE
                 IF LN-ENTDATE NOT > RPT-END-DATE
                    IF LN-POFFDATE NOT < BEGINNING-DATE
                       IF LN-POFFDATE NOT > RPT-END-DATE
                          GO TO NEXT-TR-REPORT.

      * AT THIS POINT ACCOUNT IS AT 1 OF 2 STATUSES:
      *    1. ENTERED WITHIN RANGE AND NOT DELETED.
      *    2. ENTERED PRIOR TO BEG-DATE BUT DELETED WITHIN RANGE.

           MOVE 0 TO HOLD-NF-REFUND NF-REFUND REFUND-DATE
                     NON-FILING-Y NON-FILING-N.
           MOVE SPACES TO HOLD-FEEOURS-N.

      * BECAUSE LN-FEEOURS COULD BE A SPACE OR A "Y"; MOVE "X" FOR
      * A CHECK TO SEE IF SOMETHING HAS BEEN MOVED INTO IT.
      
           MOVE "X" TO HOLD-FEEOURS-Y.
           MOVE 1 TO SUB.

       CHECK-NEXT-FEE.
           IF ( LN-FEECODE(SUB) = FEE-CODE )
              PERFORM SET-FEEOURS
              IF ( LN-FEEOURS(SUB) = "N" )
                 ADD LN-FEEAMT(SUB) TO NON-FILING-N HOLD-NF-REFUND
              ELSE
                 ADD LN-FEEAMT(SUB) TO NON-FILING-Y HOLD-NF-REFUND.

           IF SUB < 10
              ADD 1 TO SUB
              GO TO CHECK-NEXT-FEE.

           IF HOLD-FEEOURS-N = " " AND HOLD-FEEOURS-Y = "X"
              GO TO NEXT-TR-REPORT.

      * IF ACCOUNT WAS ENTERED IN PRIOR MONTH AND DELETED THIS MONTH,
      * SETUP REBATE, OTHERWISE SKIP IT:
      
           MOVE "N" TO INS-ACTIVITY.

           IF LN-POCD = "**"
              MOVE HOLD-NF-REFUND TO NF-REFUND
              MOVE LN-POFFDATE TO REFUND-DATE
              MOVE "Y" TO INS-ACTIVITY.

           IF FIRST-TIME = "Y"
              MOVE "N" TO FIRST-TIME
              MOVE "Y" TO RECORDS-FOUND
              MOVE LN-OWNBR TO HOLD-BR
              PERFORM BR-HEADER.

           MOVE LN-ORGST TO SP-ORGST.
           MOVE LN-SPRCLASS TO SP-SPRCLASS.
           MOVE LN-SUBCLASS TO SP-SUBCLASS.
           MOVE LN-LAWCODE TO SP-LAWCODE.

           IF SP1-KEY NOT = LPWSSP-SP1-KEY
              MOVE SP1-KEY TO LPWSSP-SP1-KEY
              PERFORM READ-SP1-FILE
              IF IO-FG = 9
                 GO TO IO-ERROR.

           PERFORM GET-LT-FEECODE.

           PERFORM KEY-DETAIL.

           GO TO NEXT-TR-REPORT.

       END-OF-GROUP.
           PERFORM CLOSE-TRL1-FILE.
           PERFORM CLOSE-TRS1-FILE.
           PERFORM CLOSE-LN1-FILE.
           PERFORM CLOSE-BW1-FILE.
           PERFORM CLOSE-LTF1-FILE.
           PERFORM CLOSE-WK2-FILE.

           PERFORM OPEN-WK2-FILE-OUTPUT.

           IF ( FIRST-TIME NOT = "Y" )
              PERFORM BR-TOTALS
              MOVE "Y" TO FIRST-TIME
              MOVE 0 TO PAGE-NUMBER
           ELSE
              MOVE 0 TO PAGE-NUMBER.

           GO TO NEXT-GROUP-LOC.

       END-ROUTINE.

           IF NO-RECORDS-FOUND
              GO TO END-PROGRAM.

           PERFORM GB-TOTALS.
           PERFORM RANGE-LINES.

           GO TO END-PROGRAM.

      ******************************************************************
       INITIALIZATION SECTION.
      ******************************************************************
           MOVE GATXNF-QUEUE-POS TO Q-POS.
           MOVE GATXNF-COPIES-POS TO C-POS.
           MOVE GATXNF-PRU-POS   TO P-POS.
           MOVE SPACES TO Q-BUF C-BUF P-BUF.

       COPY "LIBGB/EOINIT.CPY".

      * NEEDS TO BE HERE FOR RUNNING THRU BATCH
           PERFORM FIND-FEE-DESC.

           IF EXT-ROUTE-BUF = "00"
              MOVE "REPORT IN PROGRESS................" TO MESS
              PERFORM SEND-LEGEND.

      * CREATE TOTALS WORK FILE (WK1) & SORT WORK FILE (WK2)

           MOVE MKTEMP-DEFAULT TO MKTEMP-BUF.
           PERFORM MKTEMP-CALL.
           MOVE MKTEMP-BUF TO WK1-PATH.
           PERFORM OPEN-WK1-FILE-OUTPUT.
           PERFORM CLOSE-WK1-FILE.
           PERFORM OPEN-WK1-FILE.

           MOVE MKTEMP-DEFAULT TO MKTEMP-BUF.
           PERFORM MKTEMP-CALL.
           MOVE MKTEMP-BUF TO WK2-PATH.
           PERFORM OPEN-WK2-FILE-OUTPUT.

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

           PERFORM OPEN-BR-FILE.
           PERFORM OPEN-GR1-FILE.
           PERFORM OPEN-SP1-FILE.
           PERFORM OPEN-PR-FILE.

           MOVE EXT-CONAME-CONAME TO H-NAME.
           MOVE EXT-CONAME-SYSDATE TO H-DATE.
           MOVE EXT-CONAME-USER-ID TO H-USERID.
           MOVE FEE-CODE TO H-CODE.
           MOVE DESC-BUF TO H-CODENAME.

           MOVE BEG-MONTH   TO DATE-MMDDYY-MM.
           MOVE BEG-YEAR    TO DATE-MMDDYY-YY.
           MOVE 1           TO DATE-MMDDYY-DD.
           MOVE DATE-MMDDYY TO H-DATE1.
           PERFORM CONVERT-MMDDYY-TO-YYYYMMDD.
           MOVE DATE-YYYYMMDD TO BEGINNING-DATE.

           MOVE END-MONTH   TO DATE-MMDDYY-MM.
           MOVE END-YEAR    TO DATE-MMDDYY-YY.
           MOVE 50          TO DATE-MMDDYY-DD.
           PERFORM CONVERT-MMDDYY-TO-YYYYMMDD.
           MOVE DATE-YYYYMMDD TO NDTE-DATE.
           PERFORM NEW-DATE-CALCULATION.
           MOVE NDTE-DATE TO RPT-END-DATE DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO H-DATE2.

           PERFORM CLEAR-TOTALS.

           GO TO INITIALIZE-EXIT.

       INITIALIZE-ENDIT.
           MOVE "X" TO EXT-EOBUF-ERRCD.

       INITIALIZE-EXIT.
           EXIT.

      ******************************************************************
       EO-EXEC-AUDIT SECTION.
      ******************************************************************
           MOVE EXT-EOBUF-DATE1 TO NUM-DATE.
           MOVE NUM-MO TO BEG-MONTH.
           MOVE NUM-YR TO BEG-YEAR.
           MOVE EXT-EOBUF-DATE2 TO NUM-DATE.
           MOVE NUM-MO TO END-MONTH.
           MOVE NUM-YR TO END-YEAR.

      ******************************************************************
       ENTER-PARAMETERS SECTION.
      ******************************************************************
       ENTER-PARAM.

      * FOR RE-ENTRY ON F4 FROM ACCEPT.
      
           IF VDUSTATUS = "1U" GO TO SO-01.

       BR-01.
           MOVE "R  A000BR1." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "16" GO TO ALL-BR.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.

           IF VDUSTATUS = "1<"
              MOVE "WI/GRSCAN" TO FORM-NAM
              CALL FORM-PROGX USING FORM-PATH GRSCAN-BUF EXIT-PATHNAME
              CANCEL FORM-PROGX
              IF GRSCAN-STATUS NOT = "00"
                 GO TO BR-01
              ELSE
                 MOVE "S  A000BR1." TO VDU
                 MOVE GRSCAN-KEY TO VDUBUF GR1-KEY
                 PERFORM SCREENIO
                 MOVE "00" TO VDUSTATUS.

           IF NOT (VDUSTATUS = "00" OR "1D") GO TO BR-01.

           MOVE VDUBUF TO WGR-WORD ENT-GROUP.

           IF WGR-WORD-ZERO
              GO TO BRANCH-IS-NUMERIC.

           MOVE 0 TO WGR-MIN-LEVEL.
           PERFORM CHECK-MEMBER.

           IF WGR-ERR > 0
              MOVE WGR-MSG(WGR-ERR) TO MESS
              PERFORM SEND-MESS
              GO TO BR-01.

           IF WGR-TYPE = "A"
              MOVE "S  A000BR2." TO VDU
              MOVE SPACES TO VDUBUF
              PERFORM SCREENIO
              MOVE "Y" TO GROUP-FLAG
              MOVE 0 TO BEG-BRANCH
              MOVE 9999 TO END-BRANCH
              GO TO CHECK-BRANCH-SECURITY.

       BRANCH-IS-NUMERIC.
           MOVE "N" TO GROUP-FLAG.
           MOVE WGR-SRESULT TO BEG-BRANCH.
           MOVE "SN N040BR1." TO VDU.
           MOVE BEG-BRANCH TO VDUNUM0.
           PERFORM SCREENIO.

       BR-02.
           MOVE "ENNN040BR2." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "16" GO TO ALL-BR.
           IF VDUSTATUS = "1U" GO TO BR-01.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO BR-02.
           MOVE VDUNUM0 TO END-BRANCH.
           IF BEG-BRANCH > END-BRANCH
              PERFORM INVALID-RANGE
              GO TO BR-01.

           GO TO CHECK-BRANCH-SECURITY.

       ALL-BR.
           MOVE "SNNN040BR1." TO VDU.
           MOVE 0 TO BEG-BRANCH VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN040BR2." TO VDU.
           MOVE 9999 TO END-BRANCH VDUNUM0.
           PERFORM SCREENIO.
           MOVE "N" TO GROUP-FLAG.

       CHECK-BRANCH-SECURITY.
           PERFORM BR-RANGE-SECURITY.

           IF BR-SECURITY-OKAY = "N"
              GO TO BR-01.

       LC-01.
           MOVE "ENNN020LC1." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "16" GO TO ALL-LC.
           IF VDUSTATUS = "1U" GO TO BR-02.
           IF VDUSTATUS = "1<"
              MOVE "BEG" TO CLASS-FG
              PERFORM CLASS-SCAN
              IF IO-FG NOT = 0
              GO TO LC-01.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO LC-01.
           MOVE VDUNUM0 TO BEG-CLASS.

       LC-02.
           MOVE "ENNN020LC2." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "16" GO TO ALL-LC.
           IF VDUSTATUS = "1U" GO TO LC-01.
           IF VDUSTATUS = "1<"
              MOVE "END" TO CLASS-FG
              PERFORM CLASS-SCAN
              IF IO-FG NOT = 0
              GO TO LC-02.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO LC-02.
           MOVE VDUNUM0 TO END-CLASS.
           IF BEG-CLASS NOT > END-CLASS GO TO ENTER-BEG-DATE.
           PERFORM INVALID-RANGE.
           GO TO LC-01.

       ALL-LC.
           MOVE "SNNN020LC1." TO VDU.
           MOVE 1 TO BEG-CLASS VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN020LC2." TO VDU.
           MOVE 99 TO END-CLASS VDUNUM0.
           PERFORM SCREENIO.

       ENTER-BEG-DATE.
           IF EXT-ROUTE-BUF NOT = "00"
              GO TO FEE-01.

           MOVE "ENNN040BEGDATE." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO LC-02.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D"
              GO TO ENTER-BEG-DATE.
           MOVE VDUNUM0 TO BDATE.

           IF BEG-MONTH < 1 OR > 12
              MOVE "INVALID MONTH" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-BEG-DATE.

       ENTER-END-DATE.
           IF EXT-ROUTE-BUF NOT = "00"
              GO TO FEE-01.

           MOVE "ENNN040ENDDATE." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO ENTER-BEG-DATE.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D"
              GO TO ENTER-END-DATE.
           MOVE VDUNUM0 TO EDATE.

           IF END-MONTH < 1 OR > 12
              MOVE "INVALID MONTH" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-END-DATE.

           MOVE BEG-MONTH TO DATE-MMDDYY-MM.
           MOVE 1         TO DATE-MMDDYY-DD.
           MOVE BEG-YEAR  TO DATE-MMDDYY-YY.
           PERFORM CONVERT-MMDDYY-TO-YYYYMMDD.
           MOVE DATE-YYYYMMDD TO NUM-DATE.

           MOVE END-MONTH TO DATE-MMDDYY-MM.
           MOVE 1         TO DATE-MMDDYY-DD.
           MOVE END-YEAR  TO DATE-MMDDYY-YY.
           PERFORM CONVERT-MMDDYY-TO-YYYYMMDD.
           MOVE DATE-YYYYMMDD TO SYS-DATE.

           IF SYS-DATE < NUM-DATE
              MOVE "INVALID RANGE" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-END-DATE.
      
       FEE-01.
           MOVE SPACES TO WR-BUF.
           MOVE DESC-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.
           MOVE "E  A030FEE." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO ENTER-END-DATE.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO FEE-01.
           MOVE VDUBUF TO FEE-CODE.

           PERFORM FIND-FEE-DESC.
           IF VALID-FEE NOT = "Y"
              PERFORM INVALID-FEE
              GO TO FEE-01.

           MOVE DESC-BUF TO WR-BUF.
           MOVE DESC-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.
           GO TO SO-01.

       SO-01.
           MOVE "E  A010SO1." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO FEE-01.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO SO-01.
           MOVE VDUBUF TO SUMMARY-ONLY.
           IF NOT (SUMMARY-ONLY = "Y" OR "N") GO TO SO-01.

       EXIT-PARM.
           EXIT.

      ***************************
       FIND-FEE-DESC SECTION.
      ***************************
           MOVE "N" TO VALID-FEE.

           PERFORM OPEN-CDB1-FILE.

           MOVE "GC"     TO CDB-TYPE
                            QCDB1-WBEG-TYPE
                            QCDB1-WEND-TYPE.
           MOVE 0        TO CDB-BRANCH
                            QCDB1-WBEG-BRANCH.
           MOVE ALL "9"  TO QCDB1-WEND-BRANCH.
           MOVE FEE-CODE TO CDB-CODE
                            QCDB1-WBEG-CODE
                            QCDB1-WEND-CODE.

           PERFORM START-CDB1-FILE.
       FEE-NEXT-01.
           PERFORM READ-CDB1-FILE-NEXT.
           IF IO-FG = 9
              GO TO EXIT-FIND-FEE.

           IF CDB-TYPE NOT = "GC"
              GO TO EXIT-FIND-FEE.

           IF CDB-CODE NOT = FEE-CODE
              GO TO FEE-NEXT-01.

           MOVE CDB-DESC TO DESC-BUF.
           MOVE "Y" TO VALID-FEE.
       EXIT-FIND-FEE.
           PERFORM CLOSE-CDB1-FILE.


      ***************************
       CHECK-TRL-FEE-TABLE SECTION.
      ***************************
           MOVE "N" TO VOID-FLAG.
           MOVE 1 TO SUB.

       CHECK-NEXT-TRL-FEE.
           IF TL-FEECODE(SUB) = FEE-CODE
              IF TL-FEEAMT(SUB) < 0
                 MOVE "Y" TO VOID-FLAG.

           IF SUB < 10
              ADD 1 TO SUB
              GO TO CHECK-NEXT-TRL-FEE.

      ***************************
       CHECK-TRS-FEE-TABLE SECTION.
      ***************************
           MOVE "N" TO VOID-FLAG.
           MOVE 1 TO SUB.

       CHECK-NEXT-TRS-FEE.
           IF TS-FEECODE(SUB) = FEE-CODE
              IF TS-FEEAMT(SUB) < 0
                 MOVE "Y" TO VOID-FLAG.

           IF SUB < 10
              ADD 1 TO SUB
              GO TO CHECK-NEXT-TRS-FEE.

      ******************************************************************
       DISPLAY-PARAMETERS SECTION.
      ******************************************************************
           IF GROUP-FLAG = "Y"
              MOVE "S  A000BR1." TO VDU
              MOVE ENT-GROUP TO VDUBUF
              PERFORM SCREENIO
              MOVE "S  A000BR2." TO VDU
              MOVE SPACES TO VDUBUF
              PERFORM SCREENIO
           ELSE
              MOVE "SNNN040BR1." TO VDU
              MOVE BEG-BRANCH TO VDUNUM0
              PERFORM SCREENIO
              MOVE "SNNN040BR2." TO VDU
              MOVE END-BRANCH TO VDUNUM0
              PERFORM SCREENIO.
      
           MOVE "SNNN020LC1." TO VDU.
           MOVE BEG-CLASS TO VDUNUM0.
           PERFORM SCREENIO.
      
           MOVE "SNNN020LC2." TO VDU.
           MOVE END-CLASS TO VDUNUM0.
           PERFORM SCREENIO.
      
           MOVE "S  A040DATE1." TO VDU.
           MOVE BEG-DATE TO VDUNUM0.
           PERFORM SCREENIO.
      
           MOVE "S  A040DATE2." TO VDU.
           MOVE END-DATE TO VDUNUM0.
           PERFORM SCREENIO.
      
           MOVE "S  A030FEE." TO VDU.
           MOVE FEE-CODE TO VDUBUF.
           PERFORM SCREENIO.
      
           MOVE "S  A010SO1." TO VDU.
           MOVE SUMMARY-ONLY TO VDUBUF.
           PERFORM SCREENIO.

      **********************************
       KEY-DETAIL SECTION.
      **********************************
           PERFORM LOAN-CALCULATIONS.

           IF SUMMARY-ONLY = "Y"
              GO TO END-KEY-DETAIL.

      * SETUP PRIMARY INSURED INFO:
      
           IF LN-INSURED = "S"
              MOVE LN-SSNO(2) TO BW-SSNO
           ELSE
              MOVE LN-SSNO(1) TO BW-SSNO.
           PERFORM SETUP-INSURED.

           IF LN-INSURED = "S"
              MOVE LN-SSNO(1) TO BW-SSNO
              PERFORM READ-BW1-FILE.

           PERFORM LPSTAT-DISPLAY.
           MOVE LPS-DISPLAY-LNNO TO D-LOAN-NUM.

           MOVE LN-LOANDATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO D-LOANDATE.

      * SET INSURANCE TERM, EXPIRATION DATE AND COVERAGE:
      
           IF NON-FILING-Y NOT = 0 OR NON-FILING-N NOT = 0
              MOVE TOTAL-NOTE TO D-COVRG
              PERFORM MATURITY-DATE-CALCULATION
              MOVE MDTE-DATE TO DATE-YYYYMMDD
              PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
              MOVE DATE-MMDDYY TO D-INSEXPR
              MOVE LN-ORGTERM TO D-ORGTERM.
           MOVE LN-REGPYAMT TO D-REGPYAMT.

           MOVE FEECODE-PAYABLE TO D-FEE-PAYABLE.

           IF ( HOLD-FEEOURS-N = "N" )
              MOVE HOLD-FEEOURS-N TO D-FEEOURS
              MOVE NON-FILING-N   TO D-INSPREM
           ELSE
              MOVE HOLD-FEEOURS-Y TO D-FEEOURS
              MOVE NON-FILING-Y   TO D-INSPREM.

           IF VOID-FLAG = "Y"
              MOVE REFUND-DATE TO DATE-YYYYMMDD
              PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
              MOVE DATE-MMDDYY TO D-REFUND-DATE
              MOVE NF-REFUND TO D-REFUND.
           MOVE LN-ORGBR TO D-ORGBR.

           MOVE 1 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           IF ( HOLD-FEEOURS-N = "N" )
              IF ( HOLD-FEEOURS-Y NOT = "X" )
                 MOVE HOLD-FEEOURS-Y TO D-FEEOURS
                 MOVE NON-FILING-Y   TO D-INSPREM
                 MOVE 1 TO LN-FEED
                 PERFORM WRITE-DETAIL-LINE.

           IF LN-OLDACCTNO NOT = 0
              MOVE LN-OLDACCTNO TO OLD-ACCTNO
              MOVE OLD-AC TO D-NUMBER
              MOVE "-" TO D-H2
              MOVE OLD-CD TO D-CKDIGIT
              IF LN-INSURED NOT = "J"
                 PERFORM WRITE-DETAIL-LINE
                 GO TO END-KEY-DETAIL.

      * SETUP SECONDARY INSURED IF JOINT COVERAGE:
      
           IF LN-INSURED = "J"
              IF LN-SSNO(2) NOT = 0
                 MOVE LN-SSNO(2) TO BW-SSNO
                 PERFORM SETUP-INSURED
                 PERFORM WRITE-DETAIL-LINE.

       END-KEY-DETAIL.
           PERFORM CREATE-TOTAL-RECS.

      **************************
       SORT-TRL-FILE SECTION.
      **************************

           PERFORM OPEN-TRL1-FILE.

           MOVE BR-NO               TO TL-BRNO
                                       QTRL1-WBEG-BRNO
                                       QTRL1-WEND-BRNO.
           MOVE BEGINNING-DATE      TO TL-DATE SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO QTRL1-WBEG-DATE.
           MOVE RPT-END-DATE        TO  SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO QTRL1-WEND-DATE.
           MOVE 0                   TO TL-CLASS TL-ACCTNO
                                       TL-LAWCODE TL-LINE
                                       QTRL1-WBEG-CLASS
                                       QTRL1-WBEG-ACCTNO
                                       QTRL1-WBEG-LAWCODE
                                       QTRL1-WBEG-LINE.
           MOVE ALL "9"             TO QTRL1-WEND-CLASS
                                       QTRL1-WEND-ACCTNO
                                       QTRL1-WEND-LAWCODE
                                       QTRL1-WEND-LINE.
           PERFORM START-TRL1-FILE.

       SORT-TRL-NEXT.
           PERFORM READ-TRL1-FILE-NEXT.
           IF (IO-FG NOT = 0) OR (TRL-PATH-OWNBR NOT = TL-BRNO)
              GO TO SORT-TRL-EXIT.

           IF TL-DATE > RPT-END-DATE
              GO TO SORT-TRL-EXIT.

           IF TL-CLASS < BEG-CLASS OR TL-CLASS > END-CLASS
              GO TO SORT-TRL-NEXT.

           MOVE "N" TO INCLUDE-FLAG.

           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 10
              IF TL-FEECODE(SUB) = FEE-CODE
                 MOVE "Y" TO INCLUDE-FLAG
              END-IF
           END-PERFORM.

           IF INCLUDE-FLAG = "N"
              GO TO SORT-TRL-NEXT.

           MOVE TL-BRNO    TO WK2-BRANCH.
           MOVE "L"        TO WK2-CODE.
           MOVE TL-CLASS   TO WK2-CLASS.
           MOVE TL-ACCTNO  TO WK2-ACCTNO.
           MOVE TL-DATE    TO WK2-DATE.
           MOVE TL-LAWCODE TO WK2-LAWCODE.
           MOVE TL-LINE    TO WK2-LINE.
           PERFORM WRITE-WK2-FILE.
           IF IO-FG NOT = 0
              GO TO IO-ERROR.

           GO TO SORT-TRL-NEXT.

       SORT-TRL-EXIT.
           EXIT.

      **************************
       SORT-TRS-FILE SECTION.
      **************************

           PERFORM OPEN-TRS1-FILE.

           MOVE BR-NO               TO TS-BRNO
                                       QTRS1-WBEG-BRNO
                                       QTRS1-WEND-BRNO.
           MOVE BEGINNING-DATE      TO TS-DATE SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO QTRS1-WBEG-DATE.
           MOVE RPT-END-DATE        TO  SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO QTRS1-WEND-DATE.
           MOVE 0                   TO TS-CLASS TS-ACCTNO
                                       TS-LAWCODE TS-LINE
                                       QTRS1-WBEG-CLASS
                                       QTRS1-WBEG-ACCTNO
                                       QTRS1-WBEG-LAWCODE
                                       QTRS1-WBEG-LINE.
           MOVE ALL "9"             TO QTRS1-WEND-CLASS
                                       QTRS1-WEND-ACCTNO
                                       QTRS1-WEND-LAWCODE
                                       QTRS1-WEND-LINE.
           PERFORM START-TRS1-FILE.

       SORT-TRS-NEXT.
           PERFORM READ-TRS1-FILE-NEXT.
           IF (IO-FG NOT = 0) OR (TRS-PATH-OWNBR NOT = TS-BRNO)
              GO TO SORT-TRS-EXIT.

           IF TS-DATE > RPT-END-DATE
              GO TO SORT-TRS-EXIT.

           IF TS-CLASS < BEG-CLASS OR TS-CLASS > END-CLASS
              GO TO SORT-TRS-NEXT.

           MOVE "N" TO INCLUDE-FLAG.

           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 10
              IF TS-FEECODE(SUB) = FEE-CODE
                 MOVE "Y" TO INCLUDE-FLAG
              END-IF
           END-PERFORM.

           IF INCLUDE-FLAG = "N"
              GO TO SORT-TRS-NEXT.

           MOVE TS-BRNO    TO WK2-BRANCH.
           MOVE "S"        TO WK2-CODE.
           MOVE TS-CLASS   TO WK2-CLASS.
           MOVE TS-ACCTNO  TO WK2-ACCTNO.
           MOVE TS-DATE    TO WK2-DATE.
           MOVE TS-LAWCODE TO WK2-LAWCODE.
           MOVE TS-LINE    TO WK2-LINE.
           PERFORM WRITE-WK2-FILE.
           IF IO-FG NOT = 0
              GO TO IO-ERROR.

           GO TO SORT-TRS-NEXT.

       SORT-TRS-EXIT.
           EXIT.

      *************************
       GET-LT-FEECODE SECTION.
      *************************
           MOVE 0 TO FEECODE-PAYABLE.

           MOVE LN-OWNBR  TO LTF-BRNO.
           MOVE LN-ACCTNO TO LTF-ACCTNO.
           MOVE 1         TO LTF-SEQNO.
           PERFORM READ-LTF1-FILE.
           IF IO-FG NOT = 0
              GO TO EXIT-GET-LT-FEECODE.

      * WILL ONLY HAVE A PAYBLE AMOUNT IF FEEOURS WAS
      * "Y" IN THE LOAN UPDATE PROGRAM
           PERFORM VARYING SUB-LT FROM 1 BY 1 UNTIL
                           SUB-LT > 10
              IF ( LTF-FEECODE(SUB-LT) = FEE-CODE )
                  ADD LTF-FEECODE-PAYABLE(SUB-LT) TO
                                FEECODE-PAYABLE
              END-IF
           END-PERFORM.

       EXIT-GET-LT-FEECODE.
           EXIT.

      ******************************************************************
       CREATE-TOTAL-RECS SECTION.
      ******************************************************************
           MOVE 0 TO ONE-NF NEW-COVERAGE NEW-NF.

      * IF NOT ENTERED WITHIN THE DATE RANGE, DONT INCLUDE AS NEW PREMIUMS
      * OR IN THE TOTALS
           IF VOID-FLAG = "Y"
              MOVE 0 TO FEECODE-PAYABLE
              GO TO SET-WK1-KEYS.

           MOVE TOTAL-NOTE TO NEW-COVERAGE.
           IF ( HOLD-FEEOURS-N = "N" )
              MOVE NON-FILING-N TO NEW-NF
           ELSE
              IF ( HOLD-FEEOURS-Y  NOT = "X" )
                 MOVE NON-FILING-Y TO NEW-NF.
           IF NEW-NF NOT = 0
              MOVE 1 TO ONE-NF.

      * WK-CODE: 1-BR, 2-ST, 3-GB
      
       SET-WK1-KEYS.
           PERFORM INIT-WK1-KEY.
           MOVE 1 TO WK-CODE.
           MOVE LN-OWNBR TO WK-BRNO.
           PERFORM CREATE-WK.

           PERFORM INIT-WK1-KEY.
           MOVE 2 TO WK-CODE.
           MOVE LN-ORGST TO WK-ST.
           PERFORM CREATE-WK.

           PERFORM INIT-WK1-KEY.
           MOVE 3 TO WK-CODE.
           MOVE 0 TO WK-BRNO.
           PERFORM CREATE-WK.

       EXIT-CREATE-TOTAL-RECS.
           EXIT.

      ******************************************************************
       CREATE-WK SECTION.
      ******************************************************************
           MOVE WK1-KEY TO SAVE-WK1-KEY.
           MOVE "N" TO REC-EXISTS.

       CHECK-REC-EXISTS.
           PERFORM READ-WK1-FILE.
           IF IO-FG = 0
              GO TO UPDATE-WK1-REC.

           IF REC-EXISTS = "Y"
              GO TO IO-ERROR.

           MOVE " " TO WK1-REC.
           MOVE SAVE-WK1-KEY TO WK1-KEY.

           MOVE 0 TO WK-AMT(1) WK-AMT(2) WK-AMT(3) WK-AMT(4) WK-AMT(5)
                     WK-QTY(1) WK-QTY(2)
                     WK-TOTAL-QTY(1)   WK-TOTAL-QTY(2)
                     WK-TOTAL-AMT(1,1) WK-TOTAL-AMT(1,2)
                     WK-TOTAL-AMT(1,3) WK-TOTAL-AMT(1,4)
                     WK-TOTAL-AMT(1,5) WK-TOTAL-AMT(2,1)
                     WK-TOTAL-AMT(2,1) WK-TOTAL-AMT(2,2)
                     WK-TOTAL-AMT(2,3) WK-TOTAL-AMT(2,4)
                     WK-TOTAL-AMT(2,5).

           PERFORM WRITE-WK1-FILE.

           IF IO-FG = 9
              GO TO IO-ERROR.

           MOVE "Y" TO REC-EXISTS.
           GO TO CHECK-REC-EXISTS.

       UPDATE-WK1-REC.
           MOVE LN-SALEFIN   TO WK-SALEFIN.
           ADD  ONE-NF       TO WK-QTY(1).
           IF VOID-FLAG = "N"
              ADD  NEW-COVERAGE TO WK-AMT(1)
              ADD  NEW-NF       TO WK-AMT(2)
           ELSE
              ADD  NF-REFUND TO WK-AMT(3).
           ADD  FEECODE-PAYABLE TO WK-AMT(4).

           IF ( HOLD-FEEOURS-N = "N" )
              ADD ONE-NF       TO WK-TOTAL-QTY(1)
              IF VOID-FLAG = "N"
                 ADD NEW-COVERAGE TO WK-TOTAL-AMT(1,1)
                 ADD NEW-NF       TO WK-TOTAL-AMT(1,2)
              ELSE
                 ADD NF-REFUND    TO WK-TOTAL-AMT(1,3).

              IF ( HOLD-FEEOURS-Y NOT = "X" )
                 ADD ONE-NF          TO WK-TOTAL-QTY(2)
                 IF VOID-FLAG = "N"
                    ADD NEW-COVERAGE    TO WK-TOTAL-AMT(2,1)
                    ADD NEW-NF          TO WK-TOTAL-AMT(2,2)
                 ELSE
                    ADD NF-REFUND       TO WK-TOTAL-AMT(2,3)
                 END-IF
                 ADD FEECODE-PAYABLE TO WK-TOTAL-AMT(2,4).

           PERFORM REWRITE-WK1-FILE.

      ******************************************************************
       BR-TOTALS SECTION.
      ******************************************************************

           MOVE "B R A N C H   T O T A L S:" TO H-PAGE-DESC.
           MOVE 1 TO SAVE-CODE.
           MOVE HOLD-BR TO SAVE-BR.
           PERFORM PRINT-TOTALS.

           MOVE LN-OWNBR TO HOLD-BR.
           MOVE " " TO H-PAGE-DESC.
           PERFORM BR-HEADER.
           MOVE 0 TO BR-PAGE-NUMBER.

      ******************************************************************
       GB-TOTALS SECTION.
      ******************************************************************

           MOVE "S T A T E   T O T A L S:" TO H-PAGE-DESC
           MOVE 2 TO SAVE-CODE
           MOVE " " TO SAVE-ST
           PERFORM PRINT-TOTALS.

           MOVE "C O M P A N Y   T O T A L S:" TO H-PAGE-DESC.
           MOVE 0 TO H-BRNO.
           MOVE " " TO H-BRNAME.
           MOVE 3 TO SAVE-CODE.
           MOVE 0 TO SAVE-BR.
           PERFORM PRINT-TOTALS.

      ******************************************************************
       PRINT-TOTALS SECTION.
      ******************************************************************
      * TURNS ON/OFF TOTAL-HEADINGS.
           MOVE 99 TO LN-COUNT.
           MOVE "Y" TO TOTAL-FG.
           MOVE "Y" TO FIRST-TOT.
           MOVE " " TO WK1-KEY.
           MOVE SAVE-CODE TO WK-CODE.
           MOVE SAVE-ST TO WK-ST.
           MOVE 0 TO WK-CLASS.
           PERFORM START-WK1-FILE.

       NEXT-TOTAL-LINE.
           PERFORM READ-WK1-FILE-NEXT.
           IF IO-FG = 9
              GO TO EXIT-PRINT-TOTALS.

           IF WK-CODE NOT = SAVE-CODE
              GO TO EXIT-PRINT-TOTALS.

           IF SAVE-CODE NOT = 2
              GO TO SKIP-FIELD-2.

       LOOP-FIELD-2.

      * ST LOOP

           IF FIRST-TOT = "Y"
              MOVE "N" TO FIRST-TOT
              MOVE WK-ST TO LAST-ST H-STNO
              MOVE "- STATE SUMMARY -" TO H-BRNAME.

           IF WK-ST NOT = LAST-ST
              PERFORM TOTAL-LINE
              MOVE "Y" TO FIRST-TOT
              GO TO LOOP-FIELD-2.

           PERFORM MOVE-ACCUM-TOTALS.
           GO TO NEXT-TOTAL-LINE.

       SKIP-FIELD-2.

      * BR/IC LOOP
      
           IF WK-ST NOT = SAVE-ST
              GO TO EXIT-PRINT-TOTALS.

           PERFORM MOVE-ACCUM-TOTALS.
           GO TO NEXT-TOTAL-LINE.

       EXIT-PRINT-TOTALS.
           PERFORM TOTAL-LINE.
           MOVE "N" TO TOTAL-FG.

      ******************************************************************
       SETUP-INSURED SECTION.
      ******************************************************************
           PERFORM READ-BW1-FILE.
           IF IO-FG = 9
              MOVE "*** NOT FOUND ***" TO BW-FNAME
              MOVE SPACES TO BW-LNAME
              MOVE 0      TO BW-BDATE BW-REFDATE.

      * SET NAME OF PRIMARY INSURED:
      
           MOVE SPACES TO D-NAME.
           STRING BW-FNAME," ",BW-LNAME DELIMITED BY "  " INTO
               D-NAME.

      * SET AGE OF PRIMARY INSURED:
      
           MOVE BW-BDATE     TO BAGE-DATE.
           MOVE BW-REFDATE   TO BAGE-REFDATE.
           MOVE RPT-END-DATE TO BAGE-TODAY.
           PERFORM BAGE-CALCULATION.
           MOVE BAGE-AGE TO D-AGE.

       SETUP-INSURED-EXIT.
           EXIT.

      ******************************************************************
       WRITE-DETAIL-LINE SECTION.
      ******************************************************************
           ADD LN-FEED LN-COUNT GIVING LN-WK.

           IF LN-WK > EXT-CONAME-RPT-LINES
              PERFORM HEAD-PAGE.

           MOVE DETAIL-LINE TO PR-REC.
           PERFORM WRITE-PR-REC.
           ADD LN-FEED TO LN-COUNT.
           MOVE 1 TO LN-FEED.
           MOVE SPACES TO DETAIL-LINE.

      ******************************************************************
       HEAD-PAGE SECTION.
      ******************************************************************
           MOVE "N" TO FIRST-TIME.
           MOVE "Y" TO RECORDS-FOUND.
           ADD 1 TO PAGE-NUMBER.
           MOVE PAGE-NUMBER TO H-PAGE-NO.
           PERFORM GET-TIME.
           MOVE TIME-EDIT TO H-TIME.

           MOVE HEAD1 TO PR-REC.
           MOVE 99 TO LN-FEED.
           PERFORM WRITE-PR-REC.
      
           MOVE HEAD2 TO PR-REC.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-PR-REC.
      
           MOVE HEAD3 TO PR-REC.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-PR-REC.
      
           IF H-PAGE-DESC = " "
              ADD 1 TO BR-PAGE-NUMBER
              MOVE BR-PAGE-NUMBER TO H-BR-PAGE
              MOVE "BRANCH PAGE:" TO H-PAGE-PROMPT
           ELSE
              MOVE 0 TO H-BR-PAGE
              MOVE " " TO H-PAGE-PROMPT.
           MOVE HEAD4 TO PR-REC.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-PR-REC.
   
           IF TOTAL-FG = "N"
              MOVE HEAD5 TO PR-REC
           ELSE
              MOVE SPACES TO PR-REC.

           MOVE 2 TO LN-FEED.
           PERFORM WRITE-PR-REC.
      
           IF TOTAL-FG = "N"
              MOVE HEAD6 TO PR-REC
           ELSE
              MOVE HEAD8 TO PR-REC.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-PR-REC.
      
           IF TOTAL-FG = "N"
              MOVE HEAD7 TO PR-REC
           ELSE
              MOVE HEAD9 TO PR-REC.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-PR-REC.
      
           MOVE DASH-LINE TO PR-REC.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-PR-REC.

       END-HEAD-PAGE.
           MOVE 9 TO LN-COUNT.
           MOVE 2 TO LN-FEED.

      ******************************************************************
       RANGE-LINES SECTION.
      ******************************************************************
           MOVE 3 TO LN-FEED.
      
           IF GROUP-FLAG = "Y"
              MOVE "BRANCH GROUP" TO RANGE-LINE
              MOVE ENT-GROUP TO RANGE-SEL
              PERFORM WRITE-DETAIL-LINE
           ELSE
              MOVE "BEG BRANCH" TO RANGE-LINE
              MOVE BEG-BRANCH TO RANGE-SEL
              PERFORM WRITE-DETAIL-LINE
              MOVE "END BRANCH" TO RANGE-LINE
              MOVE END-BRANCH TO RANGE-SEL
              PERFORM WRITE-DETAIL-LINE.
      
           MOVE "BEG LOAN CLASS" TO RANGE-LINE.
           MOVE BEG-CLASS TO RANGE-SEL.
           PERFORM WRITE-DETAIL-LINE.
           MOVE "END LOAN CLASS" TO RANGE-LINE.
           MOVE END-CLASS TO RANGE-SEL.
           PERFORM WRITE-DETAIL-LINE.
      
           MOVE "BEGIN DATE" TO RANGE-LINE.
           MOVE BDATE TO RANGE-SEL.
           PERFORM WRITE-DETAIL-LINE.
           MOVE "END   DATE" TO RANGE-LINE.
           MOVE EDATE TO RANGE-SEL.
           PERFORM WRITE-DETAIL-LINE.
      
           MOVE "FEE CODE " TO RANGE-LINE.
           MOVE FEE-CODE TO RANGE-SEL.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "SUMMARY ONLY ?" TO RANGE-LINE.
           MOVE SUMMARY-ONLY TO RANGE-SEL.
           PERFORM WRITE-DETAIL-LINE.

      ******************************************************************
      *
      *   C O P Y    L I B R A R Y    S E C T I O N S
      *
      ******************************************************************
       COPY "LIBGB/DATER.CPY".
       COPY "LIBLP/LPAMTS.CPY".
       COPY "LIBLP/LPSTAT.CPY".
       COPY "LIBLP/LPMDTE.CPY".
       COPY "LIBGB/TIMEIO.CPY".
       COPY "LIBLP/LPBAGE.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/GRWORK.CPY".
       COPY "LIBGB/GRSERCH.CPY".
       COPY "LIBGB/BRSECURE.CPY".

      ******************************************************************
       MOVE-ACCUM-TOTALS SECTION.
      ******************************************************************

           PERFORM ADD-TO-TOTALS.

           MOVE WK-CLASS     TO D-CLASS.
           MOVE WK-QTY(1) TO D-QTY(3).

      * COVERAGE
           MOVE WK-AMT(1) TO D-AMT(1).
      * NEW PREMIUMS
           MOVE WK-AMT(2) TO D-AMT(2).
      * REFUNDS
           MOVE WK-AMT(3) TO D-AMT(3).
      * NET
           SUBTRACT WK-AMT(3) FROM WK-AMT(2) GIVING D-AMT(4).
      * PAYABLE
           MOVE WK-AMT(4) TO D-AMT(5).

           MOVE 2 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE 0 TO SUB.

       NEXT-TOTAL-QTY-ACCUM.
           ADD 1 TO SUB.

           IF ( SUB = 1 )
              MOVE "OUR FEE = NO" TO D-LOAN-NUM
           ELSE
              MOVE "OUR FEE = YES"  TO D-LOAN-NUM.

           IF ( WK-TOTAL-AMT(SUB,1) = ZEROES ) AND
              ( WK-TOTAL-AMT(SUB,2) = ZEROES ) AND
              ( WK-TOTAL-AMT(SUB,3) = ZEROES )
              MOVE ZEROES TO D-QTY(3)
           ELSE
              MOVE WK-TOTAL-QTY(SUB) TO D-QTY(3).

      * COVERAGE
           MOVE WK-TOTAL-AMT(SUB,1) TO D-AMT(1).
      * PREMIUMS
           MOVE WK-TOTAL-AMT(SUB,2) TO D-AMT(2).
      * REFUNDS
           MOVE WK-TOTAL-AMT(SUB,3) TO D-AMT(3).
      * NET
           SUBTRACT WK-TOTAL-AMT(SUB,3) FROM WK-TOTAL-AMT(SUB,2)
                    GIVING D-AMT(4).
      * PAYABLE
           MOVE WK-TOTAL-AMT(SUB,4) TO D-AMT(5).

           MOVE 1 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           IF ( SUB < 2 )
              GO TO NEXT-TOTAL-QTY-ACCUM.

      ****************************************************************
       CLASS-SCAN SECTION.
      ****************************************************************
           MOVE 0      TO LCSCAN-KEY.
           MOVE SPACES TO LCSCAN-VERIFY.
           MOVE "WI/LCSCAN" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH LCSCAN-BUF EXIT-PATHNAME.
           CANCEL FORM-PROGX.
           IF LCSCAN-STATUS NOT = "00" OR LCSCAN-KEY = 0
               MOVE 9 TO IO-FG
               GO TO EXIT-CLASS-SCAN.
           IF ( CLASS-FG = "BEG" )
              MOVE "SNNN020LC1." TO VDU
           ELSE
              MOVE "SNNN020LC2." TO VDU.
           MOVE LCSCAN-KEY TO VDUNUM0.
           PERFORM SCREENIO.
           MOVE LCSCAN-KEY TO VDUNUM0.
           MOVE 0 TO IO-FG.

       EXIT-CLASS-SCAN.
           EXIT.

      ******************************************************************
       ADD-TO-TOTALS SECTION.
      ******************************************************************
           ADD WK-TOTAL-QTY(1)   TO TOT-QTY(1,1).
           ADD WK-TOTAL-QTY(2)   TO TOT-QTY(2,1).
           ADD WK-TOTAL-AMT(1,1) TO TOT-AMT(1,1).
           ADD WK-TOTAL-AMT(1,2) TO TOT-AMT(1,2).
           ADD WK-TOTAL-AMT(1,3) TO TOT-AMT(1,3).
           ADD WK-TOTAL-AMT(1,4) TO TOT-AMT(1,4).
           ADD WK-TOTAL-AMT(1,5) TO TOT-AMT(1,5).
           ADD WK-TOTAL-AMT(2,1) TO TOT-AMT(2,1).
           ADD WK-TOTAL-AMT(2,2) TO TOT-AMT(2,2).
           ADD WK-TOTAL-AMT(2,3) TO TOT-AMT(2,3).
           ADD WK-TOTAL-AMT(2,4) TO TOT-AMT(2,4).
           ADD WK-TOTAL-AMT(2,5) TO TOT-AMT(2,5).

      **************************************************
       FORM-RESET SECTION.
      **************************************************
           MOVE EXT-CONAME-CONAME TO GATXNF-CONAME.
           MOVE EXT-CONAME-SYSDATE TO GATXNF-SYSDATE.
           DISPLAY GATXNF-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE GATXNF-FORM-FIELDS TO WR-BUF.
           MOVE GATXNF-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       SETUP-LINK-INFO SECTION.
      **************************************************
           MOVE GATXNF-HELP--FILE TO HELP-LINK-FILE.
           MOVE GATXNF-HELP--DIR TO HELP-LINK-DIR.

      **************************************************
       VDU-CONVERT-FIELD SECTION.
      **************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBLP/GATXNF_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ******************************************************************
       MISC-PARA SECTION.
      ******************************************************************
       COPY "LIBGB/GETPR.CPY".
       COPY "LIBGB/MKTEMP.CPY".
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/DACCESS.CPY".
       COPY "LIBGB/SYSTEM.CPY".
       COPY "LIBGB/RMFILE.CPY".
       SET-FEEOURS.
           IF ( LN-FEEOURS(SUB) = "N" )
              MOVE LN-FEEOURS(SUB) TO HOLD-FEEOURS-N
           ELSE
              MOVE LN-FEEOURS(SUB) TO HOLD-FEEOURS-Y
              IF ( LN-SALEFIN NOT = "Y" )
                 IF ( HOLD-FEEOURS-Y = SPACES )
                    MOVE "Y" TO HOLD-FEEOURS-Y.
       INVALID-FEE.
           MOVE "INVALID FEE CODE" TO MESS.
           PERFORM SEND-MESS.
       BR-HEADER.
           MOVE HOLD-BR TO BR-NO.
           PERFORM READ-BR-FILE.
           IF IO-FG = 9
              MOVE "* INVALID *" TO BR-NAME.
           MOVE HOLD-BR TO H-BRNO.
           MOVE BR-NAME TO H-BRNAME.
       INIT-WK1-KEY.
           MOVE " " TO WK1-KEY.
           MOVE LN-CLASS TO WK-CLASS.
       TOTAL-LINE.
           MOVE "ALL CLASSES" TO DETAIL-LINE.
           COMPUTE D-QTY(3) = TOT-QTY(1,1) + TOT-QTY(2,1).
      * COVERAGE
           COMPUTE D-AMT(1) = TOT-AMT(1,1) + TOT-AMT(2,1).
      * PREMIUMS
           COMPUTE D-AMT(2) = TOT-AMT(1,2) + TOT-AMT(2,2).
      * REFUNDS
           COMPUTE D-AMT(3) = TOT-AMT(1,3) + TOT-AMT(2,3).
      * NET
           COMPUTE D-AMT(4) = ( TOT-AMT(1,2) + TOT-AMT(2,2) ) -
                              ( TOT-AMT(1,3) + TOT-AMT(2,3) ).
      * PAYABLE
           COMPUTE D-AMT(5) = TOT-AMT(1,4) + TOT-AMT(2,4).

           MOVE 3 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "    ALL = NO" TO DETAIL-LINE.
           MOVE TOT-QTY(1,1) TO D-QTY(3).
      * COVERAGE
           MOVE TOT-AMT(1,1) TO D-AMT(1).
      * PREMIUMS
           MOVE TOT-AMT(1,2) TO D-AMT(2).
      * REFUNDS
           MOVE TOT-AMT(1,3) TO D-AMT(3).
      * NET
           COMPUTE D-AMT(4) = TOT-AMT(1,2) - TOT-AMT(1,3).
      * PAYABLE
           MOVE TOT-AMT(1,4) TO D-AMT(5).

           MOVE 1 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "    ALL = YES" TO DETAIL-LINE.
           MOVE TOT-QTY(2,1) TO D-QTY(3).
      * COVERAGE
           MOVE TOT-AMT(2,1) TO D-AMT(1).
      * PREMIUMS
           MOVE TOT-AMT(2,2) TO D-AMT(2).
      * REFUNDS
           MOVE TOT-AMT(2,3) TO D-AMT(3).
      * NET
           COMPUTE D-AMT(4) = TOT-AMT(2,2) - TOT-AMT(2,3).
      * PAYABLE
           MOVE TOT-AMT(2,4) TO D-AMT(5).

           MOVE 1 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           PERFORM CLEAR-TOTALS.
           MOVE 99 TO LN-COUNT.
       CLEAR-TOTALS.
           MOVE 0 TO
                  TOT-AMT(1,1) TOT-AMT(1,2) TOT-AMT(1,3) TOT-AMT(1,4)
                  TOT-AMT(1,5) TOT-AMT(2,1) TOT-AMT(2,2) TOT-AMT(2,3)
                  TOT-AMT(2,4) TOT-AMT(2,5)
                  TOT-QTY(1,1) TOT-QTY(1,2) TOT-QTY(2,1) TOT-QTY(2,2).
       INVALID-RANGE.
           MOVE "INVALID RANGE" TO MESS.
           PERFORM SEND-MESS.
       SEND-LEGEND.
           MOVE MESS TO WR-BUF.
           MOVE MESS-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      ******************************************************************
       IO-ROUTINES SECTION.
      ******************************************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBLP/LPTRSGS_SQL.CPY".
       COPY "LIBLP/LPTRLGS_SQL.CPY".
       COPY "LIBLP/LPSPGS_SQL.CPY".
       COPY "LIBLP/LPLTFGS_SQL.CPY".
       COPY "LIBLP/LPLNGS_SQL.CPY".
       COPY "LIBLP/LPCDBGS_SQL.CPY".
       COPY "LIBLP/LPBWGS_SQL.CPY".
       COPY "LIBGB/GBGRGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBPRINT.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBGB/GBGR1RN.CPY".
      * DO NOT ALLOW REWRITES ON LNFILE; (FIELDS MODIFIED).
       COPY "LIBLP/LPLN1RN.CPY".
       COPY "LIBLP/LPBW1RN.CPY".
       COPY "LIBLP/LPCDB1RN.CPY".
       COPY "LIBLP/LPSP1RN.CPY".
       COPY "LIBLP/LPLTF1RN.CPY".
       COPY "LIBGB/GBWK1IN.CPY".
       COPY "LIBGB/GBWK2IN.CPY".
       COPY "LIBLP/LPTRL1RN.CPY".
       COPY "LIBLP/LPTRS1RN.CPY".

       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO EXT-EOBUF-ERRCD.

      ******************************************************************
       END-PROGRAM SECTION.
      ******************************************************************
       END-RUN.
           PERFORM CLOSE-FILES.
           IF EXT-ROUTE-BUF = "00"
              PERFORM PRU-OUT.

           IF NO-RECORDS-FOUND
              IF EXT-ROUTE-BUF = "00" OR "70"
              MOVE "NO REPORT FOR SELECTED RANGE" TO MESS
              PERFORM SEND-MESS.

           MOVE WK1-PATH TO ACCESS-BUF.
           PERFORM REMOVE-WORKFILE.
           MOVE WK2-PATH TO ACCESS-BUF.
           PERFORM REMOVE-WORKFILE.

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
              MOVE "LP/GATXMU" TO FORM-NAM
              MOVE FORM-PATH TO FORM-PATHNAME.

       STOP-RUN.
           IF EXT-ROUTE-BUF = "00" OR "40" OR "50"
              DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN
              DESTROY GATXNF-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.
           EXIT PROGRAM.
