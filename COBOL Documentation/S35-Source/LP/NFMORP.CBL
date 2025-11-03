       IDENTIFICATION DIVISION.
       PROGRAM-ID.      NFMORP.
       DATE-WRITTEN.    011890.
      ******************************************************************
      *    DIR :  LP
      *    DESC:  MONTHLY FEE-CODE INSURANCE REPORT
      *           FEE CODE INPUTTED ON SCREEN
      *
      *
      * DRIVEN OFF TRFILE
      *    SITUATIONS REQUIRING REPORTING:
      *
      *    NEW PREMIUMS:
      *        1. LOAN RECORD WITH; LN-ENTDATE         = MMYY
      *                             LN-CONVERCD        = " "
      *                             LN-POCD        NOT = "**"
      *    REBATES AND CANCELLATIONS:
      *        CANCEL = DELETED LOAN NEXT MONTH
      *
      * USES GROUP LOGIC.
      * LOOPS THRU TRLFILE AND TRSFILE. WRITES WKFILE2
      * END OF BRANCH, LOOPS THRU WKFILE2, READS TR BY KEY, READS LNFILE,
      *     PRINTS DETAIL LINE, WRITE WKFILE FOR COMPANY TOTALS.
      * END OF PROGRAM, LOOP THRU WKFILE1 AND PRINT COMPANY TOTALS.
      *
      * REV:
      *  KEC 091695 ADDED LOCAL-SPOOLING LOGIC.
      *  KEC 091695 CHANGED MILITARY TIME TO AM/PM ON HEADER.
      *  JFF 100695 ADDED CLASS SCAN
      *  KEC 111695 ADDED LN-FEEOURS TO THE DETAIL-LINE.
      *             SEPERATE TOTALS FOR LN-FEEOURS.
      *  KEC 113095 FIXED LOCAL-SPOOL FOR IF A BRANCH HAS NO DATA.
      *  KEC 113095 FIXED LOCAL-SPOOL TO NOT PRINT THE RANGE-LINES.
      *  KEC 120195 COMMENTED OUT OPENS TO BW-FILE IN THE
      *             INITIALIZATION SECTION.
      *  KEC 121195 FIXED DETAIL-LINE TO PRINT "Y" IF FEEOURS IS A
      *             SPACE AND LN-SALEFIN NOT = "Y".
      *  KEC 121295 CHANGE THE TOTALS TO PRINT ALL THE YES'S AND NO'S.
      *  JFF 012496 ADDED LOGIC TO PRINT YES'S NAD NO'S IF IN
      *             THE SAME ACCOUNT.
      *  JTG 060496 CORRECTED BUG WITH DELETED ACCOUNTS BEING SKIPPED ALTOGETHER
      *  MJD 082896 ADDED LOGIC TO HANDLE EVERY VALID COMBINATION OF
      *             POCD, ENTDATE, AND POFFDATE: (SEE DESC IN CODE)
      *  MJD 121096 CHANGED TO PERFORM LPSTAT-DISPLAY AFTER
      *             READING BWFILE.
      *  BAH 121396 SKIP RESCISSION LOANS
      *  BAH 022597 IF JUST DELETED THIS MONTH, PREMIUM WAS GETTING ADDED
      *             INTO TOTALS, SHOULDNT BE !!
      *  BLV 060597 CHANGED FOR LOCAL SPOOL WITH NEW BATCH DRIVER - BREXEC
      *  BAH 100697 FEECODE DESCRIPTION WAS NOT PRINTING IN THE HEADING
      *             WHEN RAN FROM BATCH, PL# 147..ADDED FIND-FEE-DESC
      *             IN INITIALIZATION
      *  BAH 991103 WK-QTY WAS NOT SIGNED, IF THE FIRST ACCOUNT WAS A VOID,
      *             THE WK-QTY WAS 1 INSTEAD OF -1, CAUSING BAD TOTALS
      *             KEVIN, REGACC
      *  BAH 020724 BRANCH TRANSFERS WERE GETTING REPORTED IN BOTH BRANCHES
      *             BECAUSE THIS IS DRIVEN BY THE LOAN FILE !! CHANGED TO
      *             TEST LN-BR-TFR-FG AND ORGBR NOT = OWNBR, WORLD !
      *             SHOULD ONLY REPORT NEW LOANS FOR THE ORIGINATING BRANCH
      *  BAH 030108 PRINT NEW FEE CODE PAYABLE (COMMISSION) AMOUNT , WORLD
      *  BAH 040517 PUT STATE TOTALS BACK IN, THEY WERE COMMENTED OUT, WORLD
      *  BLF 041020 CHANGE TO DRIVE OFF OF TRFILE INSTEAD OF LNFILE.
      *             ADDED SORT ROUTINE, WILL PRINT BY CLASS BY ACCOUNT.
      *  BLF 041124 FIX TO TEST FOR FEE CODE IN SORT ROUTINE
      *  BLM 090622 ADD SOURCE CODE PARAMETERS, WORLD PR# 641
      *  BLM 101001 FIXED BUG FROM 090622.  WHEN I ADDED SOURCE CODE
      *             PARAMETER, SKIPPED CHECK ON SOURCE CODE IF SRCD =
      *             SPACES.  SOME CUSTOMER'S EOFILES HAD AN OLD VALUE FROM
      *             LOCAL-PRINT-FG, SO SRCD WAS = "N " & NO RECORDS WOULD
      *             PRINT.  CHANGED TO CHECK FOR "N ".
      *  BAH 160118 SPLIT OUT LTFILE, PD0003
      *
      * KEC 2016.0315 {PD#00003} PARADATA <A30> [JAY CISZEWSKI]
      *         CHANGED CD-FEE???? TO CD-GC-????.
      *  CS 160830 CHANGED SAVE-WK1-KEY FROM X(7) TO X(8)
      *            WK2-CLASS FROM 9(2) TO 9(3)
      *            WK-CLASS FROM 9(2) TO 9(3).
      *  BAH 170705 ACTIVATE 8 DIGIT DATES, PD0006
      *  BAH 181211 GLOBAL LNFILE
      *  BAH 190124 CHANGE CDFILE TO CDBFILE
      *  BAH 20190923 REMOVED ACCESS-CALL ON TRLFILE, TRSFILE
      *  MB  20230727 PERFORMANCE TUNING - ADDED STORED PROCEDURE, ADDED
      *               STATE PARAMETER ARRAY PROCESSING
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "LIBGB/GBFSPR.CPY".

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      * TOTALS FILE
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
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
           03  WK2-FEETBL               OCCURS 20 TIMES.
               05  WK2-FEECODE          PIC XX.
               05  WK2-FEEAMT           PIC S9(5)V99   COMP-3.



      ******************************************************************
      *
      *    W O R K I N G - S T O R A G E    S E C T I O N
      *
      ******************************************************************
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(37) VALUE
           "@(#)LP/NFMORP S35 02/23/24-11:48:52 >".

      ******************************************************************
      *
      *     F I L E   I F N
      *
      ******************************************************************
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

       01  TRL1-CONN-OPEN-SW     PIC X        VALUE "N".

       EXEC SQL BEGIN DECLARE SECTION END-EXEC.
       01  PASS-FEECODE                PIC XX.
       EXEC SQL END DECLARE SECTION END-EXEC.

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
           03  BEG-CLASS       PIC 99.
           03  END-CLASS       PIC 99.
           03  BEG-DATE        PIC 9(8).
           03  END-DATE        PIC 9(8).
           03  FEE-CODE        PIC XX.
           03  SUMMARY-ONLY    PIC X.
           03  KEY-PATH        PIC X(20).
           03  GROUP-FLAG      PIC X(1).
           03  ENT-GROUP       PIC X(4).
           03  SRCD            PIC X(2).

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
           03  FILLER           PIC X(36) VALUE " ".

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
       COPY "LIBLP/LPAMTSW.CPY".
       COPY "LIBWI/LCSCANW.CPY".
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
       COPY "LIBLP/NFMORP_DEF.CPY".
       COPY "LIBLP/NFMORP_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

       COPY "LIBLP/ARRAYSPW.CPY". 

      ******************************************************************
      *
      *     P R O G R A M   C O N T R O L
      *
      ******************************************************************
       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBLP/NFMORP_SCN.CPY".

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
           PERFORM CLOSE-TRL1-CONNECTION.
           CLOSE WK1-FILE WK2-FILE.

       COPY "LIBGB/DECLRP2.CPY".

      ******************************************************************
      *
      *    M A I N    M O D U L E
      *
      ******************************************************************
       MAIN-MODULE SECTION.

           PERFORM SQL-CONNECT.

           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "NFMORP" TO VDU-FORM-NAME.

           PERFORM INITIALIZATION.

           IF EXT-EOBUF-ERRCD NOT = " "
              GO TO END-PROGRAM.

           IF GROUP-FLAG = "N"
              MOVE BEG-BRANCH TO BR-NO
              PERFORM START-BR-FILE
              GO TO NEXT-BRANCH-NO.

           MOVE ENT-GROUP TO GR-ID.
           PERFORM SEARCH-GR-INIT.

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

           IF EXT-ROUTE-BUF = "00"
               MOVE SPACES TO WR-BUF
               STRING "REPORT IN PROGRESS...............", BR-NO
                      DELIMITED "  " INTO WR-BUF
               MOVE MESS-LIST TO WR-LIST
               PERFORM CALL-WRFORM
               END-IF.

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

               PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 20
                   MOVE WK2-FEECODE(SUB)   TO TL-FEECODE(SUB)
                   MOVE WK2-FEEAMT(SUB)    TO TL-FEEAMT(SUB)
               END-PERFORM

      *        PERFORM READ-TRL1-FILE
           ELSE
               MOVE WK2-DATE    TO TS-DATE
               MOVE WK2-BRANCH  TO TS-BRNO
               MOVE WK2-CLASS   TO TS-CLASS
               MOVE WK2-ACCTNO  TO TS-ACCTNO
               MOVE WK2-LAWCODE TO TS-LAWCODE
               MOVE WK2-LINE    TO TS-LINE
               PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 20
                   MOVE WK2-FEECODE(SUB)   TO TS-FEECODE(SUB)
                   MOVE WK2-FEEAMT(SUB)    TO TS-FEEAMT(SUB)
               END-PERFORM.
      *        PERFORM READ-TRS1-FILE.

      *     IF IO-FG NOT = 0
      *        MOVE "TRANSACTION RECORD MISSING SINCE SORT" TO MESS
      *        PERFORM SEND-MESS
      *        GO TO IO-ERROR.

           IF WK2-CODE = "L"
              PERFORM CHECK-TRL-FEE-TABLE
              MOVE TL-BRNO   TO LN-OWNBR
              MOVE TL-ACCTNO TO LN-ACCTNO
           ELSE
              PERFORM CHECK-TRS-FEE-TABLE
              MOVE TS-BRNO   TO LN-OWNBR
              MOVE TS-ACCTNO TO LN-ACCTNO.

           PERFORM READ-LN1-FILE.
           IF IO-BAD
              MOVE "********************" TO D-STAR1 D-STAR2
              MOVE "ACCOUNT # " TO D-ACCT-PROMPT
              MOVE LN-ACCTNO    TO D-ERR-ACCT
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
              IF LN-ENTDATE NOT < BEG-DATE
                 IF LN-ENTDATE NOT > END-DATE
                    IF LN-POFFDATE NOT < BEG-DATE
                       IF LN-POFFDATE NOT > END-DATE
                          GO TO NEXT-TR-REPORT.

      *    -------------------------------------------------------------
      *    AT THIS POINT ACCOUNT IS AT 1 OF 2 STATUSES:
      *       1. ENTERED WITHIN RANGE AND NOT DELETED.
      *       2. ENTERED PRIOR TO BEG-DATE BUT DELETED WITHIN RANGE.
      *    -------------------------------------------------------------

           MOVE 0 TO HOLD-NF-REFUND NF-REFUND REFUND-DATE
                     NON-FILING-Y NON-FILING-N.
           MOVE SPACES TO HOLD-FEEOURS-N.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    BECAUSE LN-FEEOURS COULD BE A SPACE OR A "Y"; MOVE "X" FOR
      *    A CHECK TO SEE IF SOMETHING HAS BEEN MOVED INTO IT.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "X" TO HOLD-FEEOURS-Y.
           MOVE 1 TO SUB.

      *-----------------------------------------------------------------
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

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      * IF ACCOUNT WAS ENTERED IN PRIOR MONTH AND DELETED THIS MONTH,
      * SETUP REBATE, OTHERWISE SKIP IT:
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "N" TO INS-ACTIVITY.

           IF LN-POCD = "**"
              MOVE HOLD-NF-REFUND TO NF-REFUND
              MOVE LN-POFFDATE    TO REFUND-DATE
              MOVE "Y" TO INS-ACTIVITY.

           IF FIRST-TIME = "Y"
              MOVE "N" TO FIRST-TIME
              MOVE LN-OWNBR TO HOLD-BR
              PERFORM BR-HEADER.

           MOVE LN-ORGST    TO SP-ORGST.
           MOVE LN-SPRCLASS TO SP-SPRCLASS.
           MOVE LN-SUBCLASS TO SP-SUBCLASS.
           MOVE LN-LAWCODE  TO SP-LAWCODE.

           PERFORM ARRAYSP-READ.
           IF NOT IO-GOOD
               STRING "MISSING SPR: ",LN-OWNBR,LN-CLASS,LN-ACCTNO
                               DELIMITED BY "  " INTO MESS
               PERFORM SEND-MESS
               GO TO IO-ERROR.
 
      *     IF SP1-KEY NOT = LPWSSP-SP1-KEY
      *        MOVE SP1-KEY TO LPWSSP-SP1-KEY
      *        PERFORM READ-SP1-FILE
      *        IF IO-FG = 9
      *           GO TO IO-ERROR.

           PERFORM GET-LT-FEECODE.

           PERFORM KEY-DETAIL.

           GO TO NEXT-TR-REPORT.

      *-----------------------------------------------------------------
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
           MOVE NFMORP-QUEUE-POS TO Q-POS.
           MOVE NFMORP-COPIES-POS TO C-POS.
           MOVE NFMORP-PRU-POS   TO P-POS.
           MOVE SPACES TO Q-BUF C-BUF P-BUF.

       COPY "LIBGB/EOINIT.CPY".

           MOVE "N"      TO RECORDS-FOUND.
           MOVE END-DATE TO RPT-END-DATE.

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
           PERFORM OPEN-CDB1-FILE.
           PERFORM OPEN-PR-FILE.

           MOVE EXT-CONAME-CONAME TO H-NAME.
           MOVE EXT-CONAME-SYSDATE TO H-DATE.
           MOVE EXT-CONAME-USER-ID TO H-USERID.
           MOVE FEE-CODE TO H-CODE.
           MOVE DESC-BUF TO H-CODENAME.

           PERFORM CLEAR-TOTALS.

           PERFORM ARRAYSP-LOAD.

           GO TO INITIALIZE-EXIT.

       INITIALIZE-ENDIT.
           MOVE "X" TO EXT-EOBUF-ERRCD.

       INITIALIZE-EXIT.
           EXIT.

      ******************************************************************
       EO-EXEC-AUDIT SECTION.
      ******************************************************************

           MOVE EXT-EOBUF-DATE1 TO BEG-DATE.
           MOVE EXT-EOBUF-DATE2 TO END-DATE.

      ******************************************************************
       ENTER-PARAMETERS SECTION.
      ******************************************************************
       ENTER-PARAM.

      * FOR RE-ENTRY ON F4 FROM ACCEPT.
           IF VDUSTATUS = "1U" GO TO SRCD-01.

      *-----------------------------------------------------------------
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

      *-----------------------------------------------------------------
       BRANCH-IS-NUMERIC.
           MOVE "N" TO GROUP-FLAG.
           MOVE WGR-SRESULT TO BEG-BRANCH.
           MOVE "SN N040BR1." TO VDU.
           MOVE BEG-BRANCH TO VDUNUM0.
           PERFORM SCREENIO.

      *-----------------------------------------------------------------
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

      *-----------------------------------------------------------------
       ALL-BR.
           MOVE "SNNN040BR1." TO VDU.
           MOVE 0 TO BEG-BRANCH VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN040BR2." TO VDU.
           MOVE 9999 TO END-BRANCH VDUNUM0.
           PERFORM SCREENIO.
           MOVE "N" TO GROUP-FLAG.

      *----------------------------------------------------------------
       CHECK-BRANCH-SECURITY.
           PERFORM BR-RANGE-SECURITY.

           IF BR-SECURITY-OKAY = "N"
              GO TO BR-01.

      *-----------------------------------------------------------------
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

      *----------------------------------------------------------------
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
           IF BEG-CLASS NOT > END-CLASS
              GO TO DATE-01.
           PERFORM INVALID-RANGE.
           GO TO LC-01.

      *----------------------------------------------------------------
       ALL-LC.
           MOVE "SNNN020LC1." TO VDU.
           MOVE 1 TO BEG-CLASS VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN020LC2." TO VDU.
           MOVE 99 TO END-CLASS VDUNUM0.
           PERFORM SCREENIO.

      *----------------------------------------------------------------
       DATE-01.
           IF EXT-ROUTE-BUF NOT = "00"
              GO TO FEE-01.
           MOVE "ERNM060DATE1." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "16" GO TO ALL-DATE.
           IF VDUSTATUS = "1U" GO TO LC-02.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO DATE-01.

           IF ( VDUNUM0 = 0 )
              GO TO DATE-01
           ELSE
              MOVE VDU-DATE-YYYYMMDD TO BEG-DATE.

      *----------------------------------------------------------------
       DATE-02.
           MOVE "ERNM060DATE2." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO DATE-01.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO DATE-02.

           IF ( VDUNUM0 = 0 )
              GO TO DATE-02
           ELSE
              MOVE VDU-DATE-YYYYMMDD TO END-DATE.

       CHECK-DATE-RANGE.
           MOVE BEG-DATE TO NUM-DATE.
           MOVE END-DATE TO SYS-DATE.
           IF (NUM-MO NOT = S-MM) OR (NUM-YR NOT = S-YY)
              MOVE "BEGINNING & ENDING DATES MUST BE IN SAME MONTH!"
                                                           TO MESS
                 PERFORM SEND-MESS
                 GO TO DATE-01.

           GO TO FEE-01.

      *----------------------------------------------------------------
       ALL-DATE.
           MOVE EXT-CONAME-SYSDATE TO ALP-DATE.
           PERFORM CONVERT-ALPDATE-TO-YYYYMMDD.
           MOVE 1 TO DATE-YYYYMMDD-DD.
           MOVE "S  8080DATE1." TO VDU.
           MOVE DATE-YYYYMMDD TO BEG-DATE VDU-DATE-YYYYMMDD.
           PERFORM SCREENIO.

           MOVE DATE-YYYYMMDD TO NDTE-DATE.
           MOVE 31            TO NDTE-DD.
           PERFORM NEW-DATE-CALCULATION.
           MOVE "S  8080DATE2." TO VDU.
           MOVE NDTE-DATE TO END-DATE VDU-DATE-YYYYMMDD.
           PERFORM SCREENIO.

      *----------------------------------------------------------------
       FEE-01.
           MOVE SPACES TO WR-BUF.
           MOVE DESC-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.
           MOVE "E  A030FEE." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO DATE-01.
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

       SRCD-01.
           MOVE "E  A010SRCD." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "16"
              MOVE "S  A020SRCD." TO VDU
              MOVE "**" TO VDUBUF SRCD
              PERFORM SCREENIO
              GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO SO-01.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO SRCD-01.
           MOVE VDUBUF TO SRCD.

       EXIT-PARM.
           EXIT.

      ***************************
       FIND-FEE-DESC SECTION.
      ***************************
           MOVE "N" TO VALID-FEE.

      *     PERFORM OPEN-CDB1-FILE.

           MOVE "GC"     TO CDB-TYPE.
           MOVE FEE-CODE TO CDB-CODE.

      *     PERFORM START-CDB1-FILE. 

       FEE-NEXT-01.
      *     PERFORM READ-CDB1-FILE-NEXT.
           PERFORM READ-CDB1-FILE.
           IF IO-FG = 9
              GO TO EXIT-FIND-FEE.

      *     IF CDB-TYPE NOT = "GC"
      *        GO TO EXIT-FIND-FEE.

      *     IF CDB-CODE NOT = FEE-CODE
      *        GO TO FEE-NEXT-01.

           MOVE CDB-DESC TO DESC-BUF.
           MOVE "Y" TO VALID-FEE.

       EXIT-FIND-FEE.
      *     PERFORM CLOSE-CDB1-FILE.

      ***************************
       TEST-TRL-FEE-TABLE SECTION.
      ***************************
           MOVE "N" TO INCLUDE-FLAG.
           MOVE 1 TO SUB.

       TEST-NEXT-TRL-FEE.
           IF TL-FEECODE(SUB) = FEE-CODE
              MOVE "Y" TO INCLUDE-FLAG
              GO TO EXIT-TEST-TRL-FEE-TABLE.

           IF SUB < 10
              ADD 1 TO SUB
              GO TO TEST-NEXT-TRL-FEE.

       EXIT-TEST-TRL-FEE-TABLE.
           EXIT.

      ***************************
       TEST-TRS-FEE-TABLE SECTION.
      ***************************
           MOVE "N" TO INCLUDE-FLAG.
           MOVE 1 TO SUB.

       TEST-NEXT-TRS-FEE.
           IF TS-FEECODE(SUB) = FEE-CODE
              MOVE "Y" TO INCLUDE-FLAG
              GO TO EXIT-TEST-TRS-FEE-TABLE.

           IF SUB < 10
              ADD 1 TO SUB
              GO TO TEST-NEXT-TRS-FEE.

       EXIT-TEST-TRS-FEE-TABLE.
           EXIT.

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
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "SNNN020LC1." TO VDU.
           MOVE BEG-CLASS TO VDUNUM0.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "SNNN020LC2." TO VDU.
           MOVE END-CLASS TO VDUNUM0.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "S  8080DATE1." TO VDU.
           MOVE BEG-DATE TO VDU-DATE-YYYYMMDD.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "S  8080DATE2." TO VDU.
           MOVE END-DATE TO VDU-DATE-YYYYMMDD.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "S  A030FEE." TO VDU.
           MOVE FEE-CODE TO VDUBUF.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "S  A010SO1." TO VDU.
           MOVE SUMMARY-ONLY TO VDUBUF.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "S  A010SRCD." TO VDU.
           MOVE SRCD TO VDUBUF.
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

      ******************************************************************
       SORT-TRL-FILE SECTION.
      ******************************************************************

           PERFORM OPEN-TRL1-FILE.

           MOVE BR-NO               TO TL-BRNO
                                       QTRL1-WBEG-BRNO
                                       QTRL1-WEND-BRNO.
           MOVE BEG-DATE            TO TL-DATE SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO QTRL1-WBEG-DATE.
           MOVE END-DATE            TO  SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO QTRL1-WEND-DATE.
           MOVE BEG-CLASS           TO TL-CLASS
                                       QTRL1-WBEG-CLASS.
           MOVE END-CLASS           TO QTRL1-WEND-CLASS.
           MOVE 0                   TO TL-ACCTNO TL-LAWCODE TL-LINE
                                       QTRL1-WBEG-ACCTNO
                                       QTRL1-WBEG-LAWCODE
                                       QTRL1-WBEG-LINE.
           MOVE ALL "9"             TO QTRL1-WEND-ACCTNO
                                       QTRL1-WEND-LAWCODE
                                       QTRL1-WEND-LINE.

           MOVE FEE-CODE            TO PASS-FEECODE.

           PERFORM START-TRL1-FILE.

       SORT-TRL-NEXT.
           PERFORM READ-TRL1-FILE-NEXT.
           IF (IO-FG NOT = 0) OR (TRL-PATH-OWNBR NOT = TL-BRNO)
              GO TO SORT-TRL-EXIT.

           IF TL-DATE > END-DATE
              GO TO SORT-TRL-EXIT.

           IF TL-CLASS < BEG-CLASS OR TL-CLASS > END-CLASS
              GO TO SORT-TRL-NEXT.

      * 100110 - CHANGED TO CHECK FOR SRCD OF "N " - BEFORE PARAMETER
      *          WAS ADDED, EOFILE HAD THIS VALUE FROM OLD LOCAL PRINT
      *          FLAG & NO RECORDS WOULD BE SELECTED.

           IF SRCD NOT = "  "
              IF SRCD NOT = "**"
                 IF SRCD NOT = "N "
                    IF TL-SRCD NOT = SRCD
                       GO TO SORT-TRL-NEXT.

           PERFORM TEST-TRL-FEE-TABLE.

           IF INCLUDE-FLAG = "N"
              GO TO SORT-TRL-NEXT.

           MOVE TL-DATE    TO WK2-DATE.
           MOVE TL-BRNO    TO WK2-BRANCH.
           MOVE "L"        TO WK2-CODE.
           MOVE TL-CLASS   TO WK2-CLASS.
           MOVE TL-ACCTNO  TO WK2-ACCTNO.
           MOVE TL-LAWCODE TO WK2-LAWCODE.
           MOVE TL-LINE    TO WK2-LINE.

           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 20
               MOVE TL-FEECODE(SUB)   TO WK2-FEECODE(SUB)
               MOVE TL-FEEAMT(SUB)    TO WK2-FEEAMT(SUB)
           END-PERFORM.

           PERFORM WRITE-WK2-FILE.
           IF IO-FG NOT = 0
              GO TO IO-ERROR.

           MOVE "Y" TO RECORDS-FOUND.

           GO TO SORT-TRL-NEXT.

       SORT-TRL-EXIT.
           EXIT.

      ******************************************************************
       SORT-TRS-FILE SECTION.
      ******************************************************************

           PERFORM OPEN-TRS1-FILE.

           MOVE BR-NO               TO TS-BRNO
                                       QTRS1-WBEG-BRNO
                                       QTRS1-WEND-BRNO.
           MOVE BEG-DATE            TO TS-DATE SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO QTRS1-WBEG-DATE.
           MOVE END-DATE            TO  SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO QTRS1-WEND-DATE.
           MOVE BEG-CLASS           TO TS-CLASS
                                       QTRS1-WBEG-CLASS.
           MOVE END-CLASS           TO QTRS1-WEND-CLASS.
           MOVE 0                   TO TS-ACCTNO TS-LAWCODE TS-LINE
                                       QTRS1-WBEG-ACCTNO
                                       QTRS1-WBEG-LAWCODE
                                       QTRS1-WBEG-LINE.
           MOVE ALL "9"             TO QTRS1-WEND-ACCTNO
                                       QTRS1-WEND-LAWCODE
                                       QTRS1-WEND-LINE.

           PERFORM START-TRS1-FILE.

       SORT-TRS-NEXT.
           PERFORM READ-TRS1-FILE-NEXT.
           IF (IO-FG NOT = 0) OR (TRS-PATH-OWNBR NOT = TS-BRNO)
              GO TO SORT-TRS-EXIT.

           IF TS-DATE > END-DATE
              GO TO SORT-TRS-EXIT.

           IF TS-CLASS < BEG-CLASS OR TS-CLASS > END-CLASS
              GO TO SORT-TRS-NEXT.

      * 100110 - CHANGED TO CHECK FOR SRCD OF "N " - BEFORE PARAMETER
      *          WAS ADDED, EOFILE HAD THIS VALUE FROM OLD LOCAL PRINT
      *          FLAG & NO RECORDS WOULD BE SELECTED.

           IF SRCD NOT = "  "
              IF SRCD NOT = "**"
                 IF SRCD NOT = "N "
                    IF TS-SRCD NOT = SRCD
                       GO TO SORT-TRS-NEXT.

           PERFORM TEST-TRS-FEE-TABLE.

           IF INCLUDE-FLAG = "N"
              GO TO SORT-TRS-NEXT.

           MOVE TS-DATE    TO WK2-DATE.
           MOVE TS-BRNO    TO WK2-BRANCH.
           MOVE "S"        TO WK2-CODE.
           MOVE TS-CLASS   TO WK2-CLASS.
           MOVE TS-ACCTNO  TO WK2-ACCTNO.
           MOVE TS-LAWCODE TO WK2-LAWCODE.
           MOVE TS-LINE    TO WK2-LINE.

           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 20
               MOVE TS-FEECODE(SUB)   TO WK2-FEECODE(SUB)
               MOVE TS-FEEAMT(SUB)    TO WK2-FEEAMT(SUB)
           END-PERFORM.

           PERFORM WRITE-WK2-FILE.
           IF IO-FG NOT = 0
              GO TO IO-ERROR.

           MOVE "Y" TO RECORDS-FOUND.

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
                  ADD LTF-FEECODE-PAYABLE(SUB-LT) TO FEECODE-PAYABLE
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

      *    IF ( HOLD-FEEOURS-N = "N" ) OR ( HOLD-FEEOURS-Y NOT = "X" )
      *       MOVE 1 TO ONE-NF.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    WK-CODE: 1-BR, 2-ST, 3-GB
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *----------------------------------------------------------------
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

      *----------------------------------------------------------------
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

      *----------------------------------------------------------------
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
           MOVE 99 TO LN-COUNT.
           MOVE "Y" TO TOTAL-FG.
           MOVE "Y" TO FIRST-TOT.
           MOVE " " TO WK1-KEY.
           MOVE SAVE-CODE TO WK-CODE.
           MOVE SAVE-ST TO WK-ST.
           MOVE 0 TO WK-CLASS.

           PERFORM START-WK1-FILE.

      *----------------------------------------------------------------
       NEXT-TOTAL-LINE.
           PERFORM READ-WK1-FILE-NEXT.
           IF IO-FG = 9
              GO TO EXIT-PRINT-TOTALS.

           IF WK-CODE NOT = SAVE-CODE
              GO TO EXIT-PRINT-TOTALS.

           IF SAVE-CODE NOT = 2
              GO TO SKIP-FIELD-2.

       LOOP-FIELD-2.

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
           STRING BW-FNAME," ",BW-LNAME DELIMITED BY "   "
               INTO D-NAME.

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

           IF LN-WK >  EXT-CONAME-RPT-LINES
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
           ADD 1 TO PAGE-NUMBER.
           MOVE PAGE-NUMBER TO H-PAGE-NO.
           PERFORM GET-TIME.
           MOVE TIME-EDIT TO H-TIME.

           MOVE HEAD1 TO PR-REC.
           MOVE 99 TO LN-FEED.
           PERFORM WRITE-PR-REC.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE HEAD2 TO PR-REC.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-PR-REC.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE HEAD3 TO PR-REC.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-PR-REC.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
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
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF TOTAL-FG = "N"
              MOVE HEAD5 TO PR-REC
           ELSE
              MOVE SPACES TO PR-REC.

           MOVE 2 TO LN-FEED.
           PERFORM WRITE-PR-REC.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF TOTAL-FG = "N"
              MOVE HEAD6 TO PR-REC
           ELSE
              MOVE HEAD8 TO PR-REC.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-PR-REC.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF TOTAL-FG = "N"
              MOVE HEAD7 TO PR-REC
           ELSE
              MOVE HEAD9 TO PR-REC.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-PR-REC.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
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
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
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
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "BEG LOAN CLASS" TO RANGE-LINE.
           MOVE BEG-CLASS TO RANGE-SEL.
           PERFORM WRITE-DETAIL-LINE.
           MOVE "END LOAN CLASS" TO RANGE-LINE.
           MOVE END-CLASS TO RANGE-SEL.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "BEGINNING DATE" TO RANGE-LINE.
           MOVE BEG-DATE TO RANGE-SEL.
           PERFORM WRITE-DETAIL-LINE.
           MOVE "ENDING    DATE" TO RANGE-LINE.
           MOVE END-DATE TO RANGE-SEL.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "FEE CODE " TO RANGE-LINE.
           MOVE FEE-CODE TO RANGE-SEL.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    MOVE "MONTHLY DETAIL ONLY ?" TO RANGE-LINE.
      *    MOVE DETAIL-MM-ONLY TO RANGE-SEL.
      *    PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "SUMMARY ONLY ?" TO RANGE-LINE.
           MOVE SUMMARY-ONLY TO RANGE-SEL.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "SOURCE CODE " TO RANGE-LINE.
           IF SRCD = "**"
              MOVE "ALL" TO RANGE-SEL
           ELSE
              MOVE SRCD TO RANGE-SEL.
           PERFORM WRITE-DETAIL-LINE.

      ******************************************************************
      *
      *   C O P Y    L I B R A R Y    S E C T I O N S
      *
      ******************************************************************
       COPY "LIBLP/ARRAYSP.CPY".
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

      *----------------------------------------------------------------
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

      *********************************************************
       FORM-RESET SECTION.
      *********************************************************
           MOVE EXT-CONAME-CONAME TO NFMORP-CONAME.
           MOVE EXT-CONAME-SYSDATE TO NFMORP-SYSDATE.
           DISPLAY NFMORP-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE NFMORP-FORM-FIELDS TO WR-BUF.
           MOVE NFMORP-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.
       FORM-RESET-EXIT.
           EXIT.

      *********************************************************
       SETUP-LINK-INFO SECTION.
      *********************************************************
           MOVE NFMORP-HELP--FILE TO HELP-LINK-FILE.
           MOVE NFMORP-HELP--DIR  TO HELP-LINK-DIR.

      *********************************************************
       VDU-CONVERT-FIELD SECTION.
      *********************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBLP/NFMORP_EVA.CPY".
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
      *-----------------------------------------------------------------
       SET-FEEOURS.
           IF ( LN-FEEOURS(SUB) = "N" )
              MOVE LN-FEEOURS(SUB) TO HOLD-FEEOURS-N
           ELSE
              MOVE LN-FEEOURS(SUB) TO HOLD-FEEOURS-Y
              IF ( LN-SALEFIN NOT = "Y" )
                 IF ( HOLD-FEEOURS-Y = SPACES )
                    MOVE "Y" TO HOLD-FEEOURS-Y.

      *-----------------------------------------------------------------
       INVALID-FEE.
           MOVE "INVALID FEE CODE" TO MESS.
           PERFORM SEND-MESS.

      *----------------------------------------------------------------
       BR-HEADER.
           MOVE HOLD-BR TO BR-NO.
           PERFORM READ-BR-FILE.
           IF IO-FG = 9
              MOVE "* INVALID *" TO BR-NAME.

           MOVE HOLD-BR TO H-BRNO.
           MOVE BR-NAME TO H-BRNAME.

      *----------------------------------------------------------------
       INIT-WK1-KEY.
           MOVE " " TO WK1-KEY.
           MOVE LN-CLASS TO WK-CLASS.

      *----------------------------------------------------------------
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

      *----------------------------------------------------------------
       CLEAR-TOTALS.
           MOVE 0 TO
                  TOT-AMT(1,1) TOT-AMT(1,2) TOT-AMT(1,3) TOT-AMT(1,4)
                  TOT-AMT(1,5) TOT-AMT(2,1) TOT-AMT(2,2) TOT-AMT(2,3)
                  TOT-AMT(2,4) TOT-AMT(2,5)
                  TOT-QTY(1,1) TOT-QTY(1,2) TOT-QTY(2,1) TOT-QTY(2,2).

      *----------------------------------------------------------------
       INVALID-RANGE.
           MOVE "INVALID RANGE" TO MESS.
           PERFORM SEND-MESS.

      *----------------------------------------------------------------
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
       COPY "LIBLP/LPLN1RN.CPY".
       COPY "LIBLP/LPTRS1RN.CPY".

      * COPY "LIBLP/LPBW1RN.CPY".
      * COPYMEMBER: LIBLP/LPBW1RN
       LOAD-BW1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( BW-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( BW-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO BW-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO BW-ALL-MACHINE
              MOVE EXT-FILPATH-DATA        TO BW-ALL-DATA
              MOVE BW-ALL-PATH             TO BW-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( BW-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO BW-ALL-BASE
              MOVE FILPATH-MACHINE         TO BW-ALL-MACHINE
              MOVE FILPATH-DATA            TO BW-ALL-DATA
              MOVE BW-ALL-PATH             TO BW-PATH.

      *-----------------------------------------------------------------
       OPEN-BW1-FILE.
           PERFORM LOAD-BW1-FILE.
           PERFORM OPEN-IT.
           MOVE BW-PATH-SQL TO E-FILE.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       READ-BW1-FILE.
           PERFORM READ-IT.
           MOVE BW-PATH-OWNBR  TO BW-OWNBR. 
           MOVE BW-PATH-SQL    TO E-FILE.
           MOVE BW1-KEY        TO E-KEYX.
           INITIALIZE QBW-REC.

           MOVE BW-OWNBR   TO QBW-OWNBR.
           MOVE BW-SSNO    TO QBW-SSNO.

           EXEC SQL
             SELECT BWFILE.BW_OWNBR,
                    BWFILE.BW_SSNO,
                    BWFILE.BW_LNAME,
                    BWFILE.BW_FNAME,
                    CAST(BWFILE.BW_REFDATE AS VARCHAR(10)),
                    CAST(BWFILE.BW_BDATE AS VARCHAR(10)),
                    BWFILE.BW_SOLICIT
               INTO 
                    :QBW-OWNBR,
                    :QBW-SSNO,
                    :QBW-LNAME,
                    :QBW-FNAME,
                    :QBW-REFDATE,
                    :QBW-BDATE,
                    :QBW-SOLICIT
            FROM DBO.BWFILE
            WHERE BWFILE.BW_OWNBR = :QBW-OWNBR
              AND BWFILE.BW_SSNO = :QBW-SSNO
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-BW1-FILE.

           IF ( IO-FG = 0 )
              PERFORM GET-BW-FIELDS.

      *-----------------------------------------------------------------
       CLOSE-BW1-FILE.
           PERFORM CLOSE-IT.


      * COPY "LIBLP/LPCDB1RN.CPY".
       LOAD-CDB1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( CDB-PATH-OVERRIDE = "Y" )
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( CDB-PATH-OVERRIDE = SPACES )
              MOVE EXT-FILPATH-BASE        TO CDB-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO CDB-ALL-MACHINE
              MOVE CDB-ALL-PATH             TO CDB-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( CDB-PATH-OVERRIDE = "B" )
              MOVE FILPATH-BASE            TO CDB-ALL-BASE
              MOVE FILPATH-MACHINE         TO CDB-ALL-MACHINE
              MOVE CDB-ALL-PATH             TO CDB-PATH.

      *-----------------------------------------------------------------
       OPEN-CDB1-FILE.
           PERFORM LOAD-CDB1-FILE.
           PERFORM OPEN-IT.
           MOVE CDB-PATH-SQL TO E-FILE.

      *-----------------------------------------------------------------
      * 5/25/2023 - DUE TO THE AMOUNT OF TIME TO IT TOOK TO PERFORM
      *     READ-CDB1-FILE-NEXT, THIS PARAGRAPH IS BEING USED INSTEAD.
      *     IT HAS BEEN MODIFIED TO FIND ANY OCCURANCE OF THE FEE-CODE,
      *     REGARDLESS OF THE BRANCH, TO VERIFY THE FEE-CODE ENTERED IS 
      *     VALID.
      *-----------------------------------------------------------------
       READ-CDB1-FILE.
           PERFORM READ-IT.
           MOVE CDB-PATH-SQL TO E-FILE.
           MOVE CDB1-KEY TO E-KEYX.

           MOVE CDB-TYPE    TO QCDB-TYPE. 
           MOVE CDB-CODE    TO QCDB-CODE.

           EXEC SQL
                SELECT CDBFILE.CDB_DESC
                 INTO  :QCDB-DESC
                FROM DBO.CDBFILE
                WHERE CDBFILE.CDB_TYPE   = :QCDB-TYPE
                  AND CDBFILE.CDB_CODE   = :QCDB-CODE 
                  AND CDBFILE.CDB_BRANCH =
                    (SELECT MIN(CDBFILE.CDB_BRANCH)
                     FROM DBO.CDBFILE
                     WHERE CDBFILE.CDB_TYPE   = :QCDB-TYPE
                       AND CDBFILE.CDB_CODE   = :QCDB-CODE) 
            END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-CDB1-FILE.

           IF ( IO-FG = 0 )
              PERFORM GET-CDB-FIELDS.

      *-----------------------------------------------------------------
       CLOSE-CDB1-FILE.
           PERFORM CLOSE-IT.



       COPY "LIBLP/LPSP1RN.CPY".
       COPY "LIBLP/LPLTF1RN.CPY".
       COPY "LIBGB/GBWK1IN.CPY".
       COPY "LIBGB/GBWK2IN.CPY".


      * COPY "LIBLP/LPTRL1RN.CPY".
      * COPYMEMBER: LIBLP/LPTR1RN
       LOAD-TRL1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( TRL-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( TRL-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO TRL-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO TRL-ALL-MACHINE
              MOVE EXT-FILPATH-DATA        TO TRL-ALL-DATA
              MOVE TRL-ALL-PATH             TO TRL-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( TRL-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO TRL-ALL-BASE
              MOVE FILPATH-MACHINE         TO TRL-ALL-MACHINE
              MOVE FILPATH-DATA            TO TRL-ALL-DATA
              MOVE TRL-ALL-PATH             TO TRL-PATH.

      *-----------------------------------------------------------------
       OPEN-TRL1-FILE.
           PERFORM LOAD-TRL1-FILE.
           PERFORM OPEN-IT.
           MOVE TRL-PATH-SQL TO E-FILE.

           IF TRL1-CONN-OPEN-SW = "N"
               EXEC SQL CONNECT TO :SQL-CONNECT-SERVER AS TRL1_CONNECT
                        USER :SQL-CONNECT-USERID USING :SQL-CONNECT-PSWD
               END-EXEC
               EXEC SQL SET CONNECTION C1 END-EXEC
               MOVE 'Y' TO TRL1-CONN-OPEN-SW.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       START-TRL1-FILE.    *> SSP - TRL
           PERFORM START-IT.
           MOVE TRL-PATH-OWNBR  TO TL-BRNO.
           MOVE TRL-PATH-SQL TO E-FILE.
           MOVE TRL1-KEY        TO E-KEYX.

           IF ( TRL1-CURSOR-STAT-GOOD )
              PERFORM CLOSE-TRL1-FILE.

      *     IF ( QTRL1-WEND-LINE = ZEROES    ) OR
      *        ( QTRL1-WEND-LINE NOT NUMERIC )
      *        MOVE TRL-PATH-OWNBR        TO QTRL1-WBEG-BRNO
      *        MOVE TRL-PATH-OWNBR        TO QTRL1-WEND-BRNO
      *        MOVE TL-DATE               TO SQL-DATE-YYYYMMDD
      *        PERFORM SQL-SET-DATE
      *        MOVE SQL-DATE-YYYY-MM-DD  TO QTRL1-WBEG-DATE
      *        MOVE JULIAN-END           TO SQL-DATE-YYYYMMDD
      *        PERFORM SQL-SET-DATE
      *        MOVE SQL-DATE-YYYY-MM-DD  TO QTRL1-WEND-DATE
      *        MOVE TL-CLASS             TO QTRL1-WBEG-CLASS
      *        MOVE TL-ACCTNO            TO QTRL1-WBEG-ACCTNO
      *        MOVE TL-LAWCODE           TO QTRL1-WBEG-LAWCODE
      *        MOVE TL-LINE              TO QTRL1-WBEG-LINE
      *        MOVE ALL "9"              TO QTRL1-WEND-CLASS
      *                                     QTRL1-WEND-ACCTNO
      *                                     QTRL1-WEND-LAWCODE
      *                                     QTRL1-WEND-LINE.

      * SELECT  TRLFILE.TL_BRNO,
      *    CAST(TRLFILE.TL_DATE AS VARCHAR(10)), 
      *    TRLFILE.TL_CLASS,
      *    TRLFILE.TL_ACCTNO,
      *    TRLFILE.TL_LAWCODE,
      *    TRLFILE.TL_LINE,
      *    TRLFILE.TL_SRCD, 
      *    TRLFILE.TL_FEECODE_1, 
      *    TRLFILE.TL_FEECODE_2, 
      *    TRLFILE.TL_FEECODE_3, 
      *    TRLFILE.TL_FEECODE_4, 
      *    TRLFILE.TL_FEECODE_5,
      *    TRLFILE.TL_FEECODE_6, 
      *    TRLFILE.TL_FEECODE_7,
      *    TRLFILE.TL_FEECODE_8, 
      *    TRLFILE.TL_FEECODE_9,
      *    TRLFILE.TL_FEECODE_10,
      *    TRLFILE.TL_FEECODE_11,
      *    TRLFILE.TL_FEECODE_12,
      *    TRLFILE.TL_FEECODE_13,
      *    TRLFILE.TL_FEECODE_14,
      *    TRLFILE.TL_FEECODE_15,
      *    TRLFILE.TL_FEECODE_16,
      *    TRLFILE.TL_FEECODE_17,
      *    TRLFILE.TL_FEECODE_18,
      *    TRLFILE.TL_FEECODE_19,
      *    TRLFILE.TL_FEECODE_20,
      *    TRLFILE.TL_FEEAMT_1,
      *    TRLFILE.TL_FEEAMT_2,
      *    TRLFILE.TL_FEEAMT_3,
      *    TRLFILE.TL_FEEAMT_4,
      *    TRLFILE.TL_FEEAMT_5,
      *    TRLFILE.TL_FEEAMT_6,
      *    TRLFILE.TL_FEEAMT_7,
      *    TRLFILE.TL_FEEAMT_8,
      *    TRLFILE.TL_FEEAMT_9,
      *    TRLFILE.TL_FEEAMT_10,
      *    TRLFILE.TL_FEEAMT_11,
      *    TRLFILE.TL_FEEAMT_12,
      *    TRLFILE.TL_FEEAMT_13,
      *    TRLFILE.TL_FEEAMT_14,
      *    TRLFILE.TL_FEEAMT_15,
      *    TRLFILE.TL_FEEAMT_16,
      *    TRLFILE.TL_FEEAMT_17,
      *    TRLFILE.TL_FEEAMT_18,
      *    TRLFILE.TL_FEEAMT_19,
      *    TRLFILE.TL_FEEAMT_20
      * FROM   DBO.TRLFILE TRLFILE
      * WHERE  TRLFILE.TL_BRNO   = @QTRL1_WBEG_BRNO
      *  AND  TRLFILE.TL_DATE  BETWEEN @QTRL1_WBEG_DATE  AND @QTRL1_WEND_DATE
      *  AND  TRLFILE.TL_CLASS BETWEEN @QTRL1_WBEG_CLASS AND @QTRL1_WEND_CLASS
      *  AND (TL_FEECODE_1  = @QTRL1_FEECODE OR TL_FEECODE_2 = @QTRL1_FEECODE OR TL_FEECODE_3 = @QTRL1_FEECODE OR
      *       TL_FEECODE_4  = @QTRL1_FEECODE OR TL_FEECODE_5 = @QTRL1_FEECODE OR TL_FEECODE_6 = @QTRL1_FEECODE OR 
      *       TL_FEECODE_7  = @QTRL1_FEECODE OR TL_FEECODE_8 = @QTRL1_FEECODE OR TL_FEECODE_9 = @QTRL1_FEECODE OR
      *       TL_FEECODE_10 = @QTRL1_FEECODE)
      *
      * ORDER BY TRLFILE.TL_CLASS,
      *    TRLFILE.TL_ACCTNO,
      *    TRLFILE.TL_DATE,
      *    TRLFILE.TL_LAWCODE,
      *    TRLFILE.TL_LINE

           EXEC SQL SET CONNECTION TRL1_CONNECT END-EXEC.

           EXEC SQL
            DECLARE TRL1_CURSOR CURSOR FOR
                :TRL1-RETURN-CODE = EXEC SSP_NFMORP_TRLFILE_SELECT
              (:QTRL1-WBEG-BRNO,  :QTRL1-WBEG-DATE,  :QTRL1-WEND-DATE,
               :QTRL1-WBEG-CLASS, :QTRL1-WEND-CLASS, :PASS-FEECODE)

           END-EXEC.

           EXEC SQL
               OPEN TRL1_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE ZEROES TO QTRL1-WBEG-BRNO
                          QTRL1-WBEG-CLASS
                          QTRL1-WEND-CLASS.
           MOVE SPACES TO QTRL1-WBEG-DATE
                          QTRL1-WEND-DATE.

           MOVE STAT---GOOD TO TRL1-CURSOR-STAT.

           EXEC SQL SET CONNECTION C1 END-EXEC.

      *-----------------------------------------------------------------
       READ-TRL1-FILE.   *> SSP - TRL
           PERFORM READ-IT.
           MOVE TRL-PATH-OWNBR  TO TL-BRNO. 
           MOVE TRL-PATH-SQL    TO E-FILE.
           MOVE TRL1-KEY        TO E-KEYX.

           MOVE TL-BRNO             TO QTL-BRNO.
           MOVE TL-DATE             TO SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO QTL-DATE.
           MOVE TL-CLASS            TO QTL-CLASS.
           MOVE TL-ACCTNO           TO QTL-ACCTNO.
           MOVE TL-LAWCODE          TO QTL-LAWCODE.
           MOVE TL-LINE             TO QTL-LINE.

           EXEC SQL
           SELECT TRLFILE.TL_BRNO,
                   CAST(TRLFILE.TL_DATE AS VARCHAR(10)),
                   TRLFILE.TL_CLASS,
                   TRLFILE.TL_ACCTNO,
                   TRLFILE.TL_LAWCODE,
                   TRLFILE.TL_LINE,
                   TRLFILE.TL_SRCD,
                   TRLFILE.TL_FEECODE_1,
                   TRLFILE.TL_FEECODE_2,
                   TRLFILE.TL_FEECODE_3,
                   TRLFILE.TL_FEECODE_4,
                   TRLFILE.TL_FEECODE_5,
                   TRLFILE.TL_FEECODE_6,
                   TRLFILE.TL_FEECODE_7,
                   TRLFILE.TL_FEECODE_8,
                   TRLFILE.TL_FEECODE_9,
                   TRLFILE.TL_FEECODE_10,
                   TRLFILE.TL_FEECODE_11,
                   TRLFILE.TL_FEECODE_12,
                   TRLFILE.TL_FEECODE_13,
                   TRLFILE.TL_FEECODE_14,
                   TRLFILE.TL_FEECODE_15,
                   TRLFILE.TL_FEECODE_16,
                   TRLFILE.TL_FEECODE_17,
                   TRLFILE.TL_FEECODE_18,
                   TRLFILE.TL_FEECODE_19,
                   TRLFILE.TL_FEECODE_20,
                   TRLFILE.TL_FEEAMT_1,
                   TRLFILE.TL_FEEAMT_2,
                   TRLFILE.TL_FEEAMT_3,
                   TRLFILE.TL_FEEAMT_4,
                   TRLFILE.TL_FEEAMT_5,
                   TRLFILE.TL_FEEAMT_6,
                   TRLFILE.TL_FEEAMT_7,
                   TRLFILE.TL_FEEAMT_8,
                   TRLFILE.TL_FEEAMT_9,
                   TRLFILE.TL_FEEAMT_10,
                   TRLFILE.TL_FEEAMT_11,
                   TRLFILE.TL_FEEAMT_12,
                   TRLFILE.TL_FEEAMT_13,
                   TRLFILE.TL_FEEAMT_14,
                   TRLFILE.TL_FEEAMT_15,
                   TRLFILE.TL_FEEAMT_16,
                   TRLFILE.TL_FEEAMT_17,
                   TRLFILE.TL_FEEAMT_18,
                   TRLFILE.TL_FEEAMT_19,
                   TRLFILE.TL_FEEAMT_20
              INTO 
                  :QTL-BRNO,
                  :QTL-DATE,
                  :QTL-CLASS,
                  :QTL-ACCTNO,
                  :QTL-LAWCODE,
                  :QTL-LINE,
                  :QTL-SRCD, 
                  :QTL-FEECODE-1, 
                  :QTL-FEECODE-2,  
                  :QTL-FEECODE-3,  
                  :QTL-FEECODE-4,  
                  :QTL-FEECODE-5,  
                  :QTL-FEECODE-6,  
                  :QTL-FEECODE-7,  
                  :QTL-FEECODE-8,  
                  :QTL-FEECODE-9,  
                  :QTL-FEECODE-10,  
                  :QTL-FEECODE-11,  
                  :QTL-FEECODE-12,  
                  :QTL-FEECODE-13,  
                  :QTL-FEECODE-14,  
                  :QTL-FEECODE-15,  
                  :QTL-FEECODE-16,  
                  :QTL-FEECODE-17,  
                  :QTL-FEECODE-18,  
                  :QTL-FEECODE-19,  
                  :QTL-FEECODE-20,  
                  :QTL-FEEAMT-1, 
                  :QTL-FEEAMT-2, 
                  :QTL-FEEAMT-3, 
                  :QTL-FEEAMT-4, 
                  :QTL-FEEAMT-5, 
                  :QTL-FEEAMT-6, 
                  :QTL-FEEAMT-7, 
                  :QTL-FEEAMT-8, 
                  :QTL-FEEAMT-9, 
                  :QTL-FEEAMT-10, 
                  :QTL-FEEAMT-11, 
                  :QTL-FEEAMT-12, 
                  :QTL-FEEAMT-13, 
                  :QTL-FEEAMT-14, 
                  :QTL-FEEAMT-15, 
                  :QTL-FEEAMT-16, 
                  :QTL-FEEAMT-17, 
                  :QTL-FEEAMT-18, 
                  :QTL-FEEAMT-19, 
                  :QTL-FEEAMT-20
            FROM DBO.TRLFILE
            WHERE TRLFILE.TL_BRNO    = :QTL-BRNO
              AND TRLFILE.TL_DATE    = :QTL-DATE
              AND TRLFILE.TL_CLASS   = :QTL-CLASS
              AND TRLFILE.TL_ACCTNO  = :QTL-ACCTNO
              AND TRLFILE.TL_LAWCODE = :QTL-LAWCODE
              AND TRLFILE.TL_LINE    = :QTL-LINE
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-TRL1-FILE.

           IF ( IO-FG = 0 )
              PERFORM GET-TRL-FIELDS.

      *-----------------------------------------------------------------
       READ-TRL1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE TRL-PATH-OWNBR  TO TL-BRNO.
           MOVE TRL-PATH-SQL    TO E-FILE.
           MOVE TRL1-KEY        TO E-KEYX.
           INITIALIZE QTRL-REC.

           EXEC SQL SET CONNECTION TRL1_CONNECT END-EXEC.

           IF ( TRL1-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH TRL1_CURSOR INTO 
                     :QTL-BRNO,
                     :QTL-DATE,
                     :QTL-CLASS,
                     :QTL-ACCTNO,
                     :QTL-LAWCODE,
                     :QTL-LINE,
                     :QTL-SRCD, 
                     :QTL-FEECODE-1, 
                     :QTL-FEECODE-2, 
                     :QTL-FEECODE-3, 
                     :QTL-FEECODE-4, 
                     :QTL-FEECODE-5, 
                     :QTL-FEECODE-6, 
                     :QTL-FEECODE-7, 
                     :QTL-FEECODE-8, 
                     :QTL-FEECODE-9, 
                     :QTL-FEECODE-10, 
                     :QTL-FEECODE-11, 
                     :QTL-FEECODE-12, 
                     :QTL-FEECODE-13, 
                     :QTL-FEECODE-14, 
                     :QTL-FEECODE-15, 
                     :QTL-FEECODE-16, 
                     :QTL-FEECODE-17, 
                     :QTL-FEECODE-18, 
                     :QTL-FEECODE-19, 
                     :QTL-FEECODE-20, 
                     :QTL-FEEAMT-1,
                     :QTL-FEEAMT-2,
                     :QTL-FEEAMT-3,
                     :QTL-FEEAMT-4,
                     :QTL-FEEAMT-5,
                     :QTL-FEEAMT-6,
                     :QTL-FEEAMT-7,
                     :QTL-FEEAMT-8,
                     :QTL-FEEAMT-9,
                     :QTL-FEEAMT-10,
                     :QTL-FEEAMT-11,
                     :QTL-FEEAMT-12,
                     :QTL-FEEAMT-13,
                     :QTL-FEEAMT-14,
                     :QTL-FEEAMT-15,
                     :QTL-FEEAMT-16,
                     :QTL-FEEAMT-17,
                     :QTL-FEEAMT-18,
                     :QTL-FEEAMT-19,
                     :QTL-FEEAMT-20 
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           EXEC SQL SET CONNECTION C1 END-EXEC.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-TRL1-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-TRL-FIELDS.

      *-----------------------------------------------------------------
       CLOSE-TRL1-FILE.
           PERFORM CLOSE-IT.
           IF ( TRL1-CURSOR-STAT-GOOD )
               EXEC SQL SET CONNECTION TRL1_CONNECT END-EXEC
               EXEC SQL CLOSE TRL1_CURSOR END-EXEC
               EXEC SQL SET CONNECTION C1 END-EXEC
               MOVE STAT---BAD TO TRL1-CURSOR-STAT.

      *-----------------------------------------------------------------
       CLOSE-TRL1-CONNECTION.
           IF TRL1-CONN-OPEN-SW = "Y"
               EXEC SQL SET CONNECTION TRL1_CONNECT END-EXEC
               EXEC SQL COMMIT END-EXEC
               EXEC SQL DISCONNECT TRL1_CONNECT END-EXEC
               EXEC SQL SET CONNECTION C1 END-EXEC
               MOVE "N" TO TRL1-CONN-OPEN-SW.
      *-----------------------------------------------------------------

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
              MOVE "LP/EMICMU" TO FORM-NAM
              MOVE FORM-PATH TO FORM-PATHNAME.

       STOP-RUN.
           IF EXT-ROUTE-BUF = "00" OR "40" OR "50"
              DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN
              DESTROY NFMORP-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.
           EXIT PROGRAM.
