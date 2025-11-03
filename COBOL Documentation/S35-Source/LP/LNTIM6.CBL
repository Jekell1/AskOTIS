       IDENTIFICATION DIVISION.
       PROGRAM-ID.   LNTIM6.
       DATE-WRITTEN. 071014
      ******************************************************************
      *                         (LP)
      *   REPORT LENGTH OF LOANS, EITHER NEW LOANS, FB'S,
      *      PB'S = OR > 10%, OR PB'S < 10%. 
      *      THIS REPORT IS A COMBINATION OF LNTIM4 (WHICH REPORTS
      *      NEW LOANS, FB'S OR PBS > 10%) AND LNTIM6 WHICH REPORTS
      *      PB'S > 10%.  BUT THIS REPORT KEEPS TOTALS FOR ALL
      *      TYPES, PLUS TOTAL BREAKDOWNS OF SKIPPED ACCTS: 
      *      TRANSFERRED FROM, TRANSFERRED TO, & CONVERTED.
      *      
      *      TRANSFERRED ACCTS ARE SKIPPED IN THESE REPORTS
      *      BECAUSE WE ARE READING BACK & FORWARD IN THE PB CHAIN,
      *      SO IT JUST GETS TOO COMPLICATED IF ACCT WAS TRANSFERRED
      *      THEN PB'ED IN ANOTHER BRANCH.
      *      
      *      THERE IS A TOTAL COLUMN ON THIS REPORT WHICH HOPEFULLY
      *      SHOULD MATCH THE OUTSTANDING # OF ACCTS IN STATS AS OF
      *      BEG DATE.  WHEN THERE ARE CONVERTED ACCTS, THE TOTAL
      *      MAY NOT MATCH STATS.
      *       
      *      LIKE LNTIM4 & LNTIM6, THIS REPORT FIND LOANS OPEN ON
      *      BEG-DATE & COMPUTES LENGTH OF LOAN FROM LN-ENTDATE
      *      TO EVENT DATE.  IF PL, ENDING (EVENT) DATE IS LN-PLDATE.
      *      IF PAYOFF (PO) EVENT DATE IS PAYOFF TRANS DATE (LN-PROMDATE2).
      *      IF PAYOFF WAS A PB, GO THRU CHAIN LOOKING FOR LOAN THAT
      *      HAS BEEN PL'ED OR PAIDOUT WITH PO OR PAID OUT WITH A
      *      PB = OR GT 10%.  IF NONE FOUND (IE LOAN IS OPEN), USE
      *      ENDING-DATE TO COMPUTE ELAPSED FOR LENGTH OF LOAN
      *
      *      ALSO, IN THE CASE OF PB'S < 10%, WILL SEARCH BACKWARDS
      *      IN THE PB CHAIN, IE READING PRIOR LOANS, TO FIND 1ST
      *      LOAN WHICH IS NOT < 10% & USES THAT LOANS ENTDATE FOR
      *      THE START START TO COMPUTE LOAN LENGTH.
      * 
      *      IMPORTANT***** BECAUSE WE ARE READING BACK & FORTH IN
      *      THE PB CHAIN, THERE ARE A LOT OF FIELDS THAT GET SAVED
      *      FROM THE ORIGINAL LOAN, OR THE FIRST OR LAST LOAN IN
      *      THE PB CHAIN, ETC.  SO, FOR EXAMPLE, YOU COULDN'T JUST
      *      GO IN & ADD A FIELD FROM THE LOAN RECORD TO THE DETAIL
      *      PRINT ROUTINE WITHOUT SAVING IT FROM THE READ OF
      *      THE ORIGINAL LOAN.
      *
      * NOTE: METHOD 1 WILL HAVE FEWER PB'S < 10, SO TOTAL LENGTH OF LOANS
      *       WILL BE SHORTER THAN WITH METHOD 2
      *
      *=================================================================
      * REV:
      * BLM 130701 NEW, WORLD PR#906
      * BLM 130711 FIXED PROBLEM WITH MULTIPLE PB'S < 10%, LIKE LNTIM6,
      *            ALSO CHANGED TO SKIP CONVERTED & TRANSFERED ACCOUNT
      *            WORLD PR# 906A
      * BLM 130924 ADD OPTION TO SELECT WHICH METHOD TO CALC WHETHER
      *            RENEWAL IS LESS THAN 10%. WORLD PR# 933
      * BLM 131016 REQUIRE SELECTION OF 10% CALC METHOD REGARDLESS OF
      *            LOAN TYPE REQUESTED, WORLD PR# 937
      * BLM 131017 FIX DISPLAY PARAMETERS, WAS MISSING A COUPLE,
      *             WORLD PR# 938
      * BLM 131018 FIX HEADING TO PRINT CALC METHOD REGARDLESS OF LOAN
      *            TYPE SELECTED, WORLD PR# 939.
      * BLM 131021 FOR METHOD 2 OF COMPUTING LOANS < 10%, ADD FEE 
      *            CODES 90, 92 & 95 TO PROCEEDS BEFORE DIVIDING BY 
      *            PRIOR-PAYOFF, WORLD PR# 939
      * BLM 131022 REMOVE ROUNDING FROM COMPUTATION OF LOANS < 10%, SO
      *            RESULT MUST BE AT LEAST 10.0 TO BE A > 10% RENEWAL
      *            WORLD PR# 939
      * BLM 141219 CLEAR LESS-THAN-10-FIRST-LN & LESS-THAN-10-LOAN-DATE
      *            UNLESS SRC 4 (PB'S < 10%), THEY WERE PRINTING INVALID
      *            INFO FOR OTHER SOURCE TYPES, WORLD PR# 984A
      * BLM 141219 IF CONVERTED & PAYOFF DATE & ALL BALANCES ARE 0, SKIP,
      *            WORLD PR# 984A
      * BLM 141222 IF LN-OWNBR NOT = LN-ORGBR, ACCT XFERED FROM ANOTHER
      *            BRANCH.  READ LM & SKIP IF ACCT TRANSFERED IN PRIOR
      *            TO BEG-DATE, OTHERWISE, PROCESS IT, WORLD PR# 984A
      * BLM 141223 IN FIND-PB-CHAIN ROUTINE, FIX TO READ-LN5, LN6, & LN7
      *            IF PB'ED LOAN NOT FOUND IN READ OF LN4, WORLD PR# 984A
      * BLM 141229 FIXED GET-PL-TRANS-DATE ROUTINE.  WHEN READING LP FOR
      *            PL POST DATE, IT WAS CORRECT FOR P2-P5 BUT NOT FOR
      *            PL, SO PL'S WOULD HAVE BEEN USING PLDATE FROM LNFILE,
      *            WORLD PR# 984A
      * BLM 151022 FIX PROBLEM WITH BT'S OF OLD PO'S.  PO'S FROM YEARS
      *            AGO WERE BT'ED IN RANGE OF REPORT AND APPEAR AS IF
      *            OPEN ON BEG-DATE.  IF POCD IS BT, READ LP & CHECK 
      *            FOR TRUE PO DATE ON PMT BEFORE BT, WORLD PR# 984B
      *
      * KEC 2017.0607 {PR#00000} PARADATA <A30> [JAY CISZEWSKI]
      *          ADDED 'LN' FILE PREFIX FOR WORKING STORAGE FIELDS.
      *
      * BAH 170608 ACTIVATE 8 DIGIT DATES, PD0006
      *
      * KEC 2017.0613 {PR#00000} PARADATA <A30> [JAY CISZEWSKI]
      *         ADDED LIBLP/TFRMEMOW; USED TO CREATE/CHECK MEMOS (BM/LM)
      *         
      * BAH 181128 GLOBAL LMFILE
      * BAH 181210 GLOBAL LNFILE
      * BAH 20190926 REMOVED ACCESS-CALL LNFILE
      * BAH 20211021 DONT NEED REPOSITIONING OF KEY FOR LN3-FILE-NEXT.
      *              ORIGINAL CURSOR HOLDS RECORDS, COMMENTED OUT
      *              START-LN3-FILE-GREATER
      * BAH 20220803 HARD CODED START
      * BAH 20230713 COMMENT OUT 3RD AND 4TH BORROWER TESTS, LN6 AND LN7
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "LIBGB/GBFSPR.CPY".

      *    SELECT ARCH-LN-FILE ASSIGN TO LN-ARCHIVE-PATH
      *           ORGANIZATION INDEXED
      *           ACCESS DYNAMIC
      *           LOCK MODE AUTOMATIC WITH LOCK ON RECORD
      *           RECORD KEY ARCH-LN1-KEY
      *           ALTERNATE RECORD KEY ARCH-LN3-KEY
      *                              = ARCH-LN-OWNBR
      *                                ARCH-LN-CLASS
      *                                ARCH-LN-ACCTNO
      *                                        WITH DUPLICATES
      *           ALTERNATE RECORD KEY ARCH-LN4-KEY WITH DUPLICATES
      *           ALTERNATE RECORD KEY ARCH-LN5-KEY WITH DUPLICATES
      *           ALTERNATE RECORD KEY ARCH-LN6-KEY WITH DUPLICATES
      *           ALTERNATE RECORD KEY ARCH-LN7-KEY WITH DUPLICATES
      *           FILE STATUS FILE-STAT.

       COPY "LIBGB/GBFSWKI.CPY".
       I-O-CONTROL.
       DATA DIVISION.
       FILE SECTION.
       COPY "LIBGB/GBFDPR.CPY".

      ********************************************************************
      *   ARCHIVED LOAN MASTER FILE   
      ********************************************************************
      *FD  ARCH-LN-FILE
      *    LABEL RECORDS ARE STANDARD.
      *COPY "LIBLP/LP01LN.CPY" REPLACING LEADING "LN" BY "ARCH-LN"
      *                              TRAILING "STFG" BY "ARCH-LN-STFG".

      ******************************************************************
      *  SUMMARY TOTALS WORK RECORD
      ******************************************************************
       FD  WK-FILE
           LABEL RECORDS ARE STANDARD.
       01  WK-REC.
           03  WK-KEY.
               05  WK-BR    PIC 9(4).
               05  WK-CLASS PIC 9(3).
               05  WK-SRC   PIC 9.
           03  WK-NOLOANS           PIC S9(9) COMP-3.
           03  WK-ELAPSED-MO        PIC S9(9) COMP-3.
           03  WK-ELAPSED-REM       PIC S9(9) COMP-3.

       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(43) VALUE
           "@(#)./LP/LNTIM6.CBL S35 02/20/24-14:37:48 >".
      ****************************
      *     F I L E   I F N      *
      ****************************
       COPY "LIBLP/LP01BW.CPY".
       COPY "LIBLP/LP01BW_SQL.CPY".
       COPY "LIBLP/LPWSBW.CPY".
       COPY "LIBLP/LP01LN.CPY".
       COPY "LIBLP/LP01LN_SQL.CPY".
       COPY "LIBLP/LPWSLN.CPY".
       COPY "LIBLP/LP01SP.CPY".
       COPY "LIBLP/LP01SPA.CPY".
       COPY "LIBLP/LP01SPB.CPY".
       COPY "LIBLP/LP01SPC.CPY".
       COPY "LIBLP/LP01SP_SQL.CPY".
       COPY "LIBLP/LPWSSP.CPY".
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBGB/GB01GR.CPY".
       COPY "LIBGB/GB01GR_SQL.CPY".
       COPY "LIBGB/GBWSGR.CPY".
       COPY "LIBLP/LP01LP.CPY".
       COPY "LIBLP/LP01LP_SQL.CPY".
       COPY "LIBLP/LPWSLP.CPY".
       COPY "LIBLP/LP01LM.CPY".
       COPY "LIBLP/LP01LM_SQL.CPY".
       COPY "LIBLP/LPWSLM.CPY".

       01  WK-IFN               PIC X(4)  VALUE "WK".
 
      *********************************************************
      *   DATA PATH FROM GETENV ON "FIL"
      *********************************************************
      *01  DATA-PATH                           PIC X(35).
      *01  DATA-PATH-FIL-BUFFER      REDEFINES DATA-PATH.
      *    03  DATA-PATH-DIR1                  PIC X(3).
      *    03  FILLER                          PIC X(32).

      *********************************************************
      *   LNFILE ARCHIVE PATH WORKERS
      *********************************************************
      *01  LN-ARCHIVE-PATH.
      *    03  FILLER                     PIC X     VALUE "/".
      *    03  LN-ARCHIVE-PATH1.
      *        05  LN-ARCHIVE-DIR1        PIC X(3).
      *        05  FILLER                 PIC X     VALUE "/".
      *        05  LN-ARCHIVE-DIRECTORY   PIC X(4).
      *        05  FILLER                 PIC X     VALUE "/".
      *        05  LN-ARCHIVE-MACHINE     PIC XX.
      *        05  FILLER                 PIC X     VALUE "/".
      *        05  LN-ARCHIVE-DATA        PIC X(6).
      *    03  FILLER                     PIC X(10) VALUE
      *        "/LP/LNFILE".

      *-----------------------------------------------------------------
      * LINE WORKERS
      *-----------------------------------------------------------------
       01  LN-WORKERS.
           03  LN-FEED          PIC 999      VALUE 0.
           03  LN-COUNT         PIC 999      VALUE 99.
           03  LN-WK            PIC 999      VALUE 0.
       01  PAGE-NUMBER          PIC 9(4)     VALUE 0.
 
       01  ER-FEED              PIC 9        VALUE 1.
 
      * SRC WILL INDICATE WHICH BUCKET GETS ADDED TO:
      *                         SRC 1 = NB'S
      *                         SRC 2 = FB'S
      *                         SRC 3 = PB'S  = OR > 10%
      *                         SRC 4 = PB'S  < 10%
      *                         SRC 5 = TRANSFER FROM
      *                         SRC 6 = TRANSFER TO
      *                         SRC 7 = CONVERTED
      *                         SRC 8 = TOTAL
      * SEL-SRC WILL INDICATE WHICH TYPE WAS SELECTED TO PRINT
       01  SRC                  PIC 9        VALUE 0.
       01  SEL-SRC              PIC 9        VALUE 0.
 
       01  WS-GTT-TOTAL-TABLE.
           03  WS-GTT-TABLE-GROUP-LEVEL     OCCURS 10 TIMES.
               05  WS-GTT-CLASS1-5      OCCURS 3 TIMES.
                   07  GTT-ELAPSED-MO   PIC S9(9)   COMP-3.
                   07  GTT-ELAPSED-REM  PIC S9(9)   COMP-3.
                   07  WS-GTT-SRC       OCCURS 8 TIMES.
                       09  GTT-NOLOANS  PIC S9(9)    COMP-3.

       01  SRC-DESC-TABLE.
           03  FILLER           PIC X(36) VALUE
           "  # NB'S   #FB'S  PB > 10% PB < 10% ".
           03  FILLER           PIC X(36) VALUE
           " XFER FR  XFER TO  CONVERT    TOTAL ".
       01  SRC-DESC REDEFINES SRC-DESC-TABLE PIC X(9) OCCURS 8.

       01  TEST-LN-PLCD.
           03  FILLER        PIC X.
           03  TEST-PLTYPE   PIC X.

       01  HOLD-PL-TRDATE       PIC 9(8).

       01  WS-HOLD-PROMPT       PIC X(7)  VALUE SPACES.

       01  LOAN-IS-LT-10-PERC     PIC X.
       01  LESS-THAN-10-FIRST-LN  PIC 9(8) VALUE 0.
       01  LESS-THAN-10-LAST-LN   PIC 9(8) VALUE 0.
       01  LESS-THAN-10-LOAN-DATE PIC 9(8) VALUE 0.
       01  SAVE-LN-ENTDATE        PIC 9(8).
       01  SAVE-LN-PLDATE         PIC 9(8).
       01  SAVE-LN-POFFDATE       PIC 9(8).
       01  SAVE-LN-SRCD         PIC X(2).
       01  SAVE-LN-PO-TRCD      PIC X(2).
       01  SAVE-LN-CLASS        PIC 9(3).
       01  SAVE-LN-SRC          PIC 9.
       01  EVENT-DATE           PIC 9(8). 
       01  HOLD-STAT            PIC X(22).
       01  HOLD-PB-LN-ACCTNO    PIC 9(8).

       01  HOLD-LN3-KEY.
           05  HOLD-LN-OWNBR   PIC 9(4).
           05  HOLD-LN-CLASS   PIC 9(3).
           05  HOLD-LN-ACCTNO  PIC 9(8).
       01  PRIOR-LOAN-TO-FIND           PIC 9(8).

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       01  HOLD-LN-SSNO                 PIC 9(9).

      *-----------------------------------------------------------------
      * MISC WORKERS
      *-----------------------------------------------------------------
       01  MSEL-LIST            PIC X(5)      VALUE "MSEL.".
       01  SUB                  PIC 9(2)  COMP.
       01  TOT-FEES             PIC 9(5)V99.

       01  HOLD-BRNO            PIC 9(4)  VALUE 0.
       01  HOLD-BRNAME          PIC X(20) VALUE SPACES.

       01  SAVE-WK-KEY.
           03  SAVE-WK-BR       PIC 9(4).
           03  SAVE-WK-CL       PIC 9(3).
           03  SAVE-WK-SRC      PIC 9.

       01  RECORDS-FOUND        PIC X     VALUE " ".
           88  NO-RECORDS-FOUND           VALUE "N".
           88  SKIP-RECORDS-FOUND         VALUE " ".

       01  TOT-ELAPSED1    PIC S9(9)V99  VALUE 0.
       01  TOT-ELAPSED5    PIC S9(9)V99  VALUE 0.
       01  TOT-ELAPSED     PIC S9(9)V99  VALUE 0.
       01  WORK-ELAPSED    PIC S9(9)V99  VALUE 0.
       01  WORK-AVG        PIC S9(9)V99  VALUE 0.

       01  WS-AMOUNT       PIC S9(9)V99  VALUE 0.
       01  WS-AMTFIN       PIC S9(9)V99  VALUE 0.
       01  PRIOR-PAYOFF    PIC S9(9)V99  VALUE 0.
       01  MEMO-BEG-DATE   PIC 9(8)      VALUE 0.
       01  XFERED-AFTER-BEGDATE-FG PIC X VALUE "N".
       01  BT-OLD-PO-FG    PIC X VALUE "N".

       01  EXITPROG             PIC X(9)     VALUE "LP/LPRPMU".

       01  FIRST-TIME           PIC X        VALUE "Y".
       01  MESS-LIST            PIC X(5)     VALUE "MESS.".
 
       01  LEGEND               PIC X(70).
       01  LEGEND-DTE-01.
           03  FILLER           PIC X(70)  VALUE
               " F7-EXIT, ^, V, ENTER BEGINNING LOAN DATE:".
       01  LEGEND-DTE-02.
           03  FILLER           PIC X(70)  VALUE
               " F7-EXIT, ^, V, ENTER ENDING LOAN DATE:".

      *-----------------------------------------------------------------
      *     S C R E E N   F I E L D S   *
      *-----------------------------------------------------------------
       COPY "LIBGB/EOBUF_EXT.CPY".
           03  BEG-BRANCH      PIC 9(4).
           03  END-BRANCH      PIC 9(4).
           03  LOAN-TYPE       PIC X.
           03  BEG-DATE        PIC 9(8).
           03  END-DATE        PIC 9(8).
           03  INCLUDE-DETAIL  PIC X.
           03  GROUP-FLAG      PIC X.
           03  ENT-GROUP       PIC X(4).
           03  GTT-ISO         PIC X.
           03  TOTAL-LEV-TABLE.
               05  TOTAL-LEVEL-TABLE PIC X OCCURS 10.
           03  CALC-METHOD     PIC 9.

       01  VERSION-CONTROL     PIC X VALUE "A".
 
       01  SV-BEG-BR           PIC 9999  VALUE 9999.
       01  SV-END-BR           PIC 9999  VALUE 9999.

      *-----------------------------------------------------------------
      *     P R I N T   B U F F E R S   *
      *-----------------------------------------------------------------
 
       01  HEAD1.
           03  H-DATE           PIC X(8)     VALUE " ".
           03  FILLER           PIC XX       VALUE " ".
           03  H-HR             PIC 99       VALUE 0.
           03  FILLER           PIC X        VALUE ":".
           03  H-MIN            PIC 99       VALUE 0.
           03  FILLER           PIC X(26)    VALUE " ".
           03  H-NAME           PIC X(50)    VALUE " ".
           03  FILLER           PIC X(32)    VALUE " ".
           03  FILLER           PIC X(5)     VALUE "PAGE ".
           03  H-PAGE-NO        PIC ZZZ9.
 
       01  HEAD2.
           03  FILLER           PIC X(10)    VALUE "USER ID:".
           03  H-USERID         PIC X(10).
           03  FILLER           PIC X(06)    VALUE " ".
           03  FILLER           PIC X(24) VALUE  
               "LENGTH OF LOANS REPORT: ".
           03  H-TYPE           PIC X(32).
           03  FILLER           PIC X(7)  VALUE
           " FROM: ".
           03  H-BEG-DATE       PIC 99/99/99.
           03  FILLER           PIC X(5)  VALUE
           " TO: ".
           03  H-END-DATE       PIC 99/99/99.
 
       01  HEAD3.
           03  FILLER           PIC X(44)  VALUE 
           "BRNO  ACCTNO        SRC   LNDATE   PO DATE ".        
           03  FILLER           PIC X(31)  VALUE
           "POCD  PLDATE    MO  REM     AVG".
           03  FILLER           PIC X(32)  VALUE
           "  STATUS                PB LOAN ".
           03  FILLER           PIC X(26)  VALUE
           "EVENT DT 1ST LN   1ST DATE".
       01  TOTAL-HEAD1.
           03  FILLER           PIC X(20) VALUE
           "BRANCH/GROUP". 
           03  FILLER           PIC X(15) VALUE SPACES.
           03  FILLER           PIC X(7)  VALUE
           "FROM:  ".
           03  TOT-BEG-DATE     PIC 99/99/99.
           03  FILLER           PIC X(6)  VALUE
           " TO:  ".
           03  TOT-END-DATE     PIC 99/99/99.
       01  TOTAL-HEAD2.
           03  FILLER           PIC X(31) VALUE SPACES.
           03  H-SEL-SRC        PIC X(13) VALUE SPACES.
           03  FILLER           PIC X(15) VALUE
           "    TOT ELAPSED ".
           03  FILLER           PIC X(10) VALUE
           "     AVG  ".
           03  H-SRC            PIC X(9) OCCURS 7.

       01  DETAIL-LINE          PIC X(132)  VALUE SPACES.
       01  DETAIL1 REDEFINES DETAIL-LINE.
           03  D-BRNO              PIC 9(4).
           03  FILLER              PIC X(2).
           03  D-LOAN-NUM          PIC X(13).
           03  FILLER              PIC X.
           03  D-SRCD              PIC X(2).
           03  D-SRCD-TYPE         PIC X.
           03  FILLER              PIC X(2).
           03  D-LOANDATE          PIC 99/99/99.
           03  FILLER              PIC X(2).
           03  D-POFFDATE          PIC 99/99/99 BLANK ZERO.
           03  FILLER              PIC X(2).
           03  D-POCD              PIC XX.
           03  FILLER              PIC X(2).
           03  D-PLDATE            PIC 99/99/99 BLANK ZERO.
           03  FILLER              PIC X.
           03  D-ELAPSED-MO        PIC ZZZ9.
           03  FILLER              PIC X.
           03  D-ELAPSED-REM       PIC ZZZ9.
           03  FILLER              PIC X.
           03  D-ELAPSED           PIC ZZZ9.99.
           03  FILLER              PIC X(2).
           03  D-STAT              PIC X(22).
           03  FILLER              PIC X(1).
           03  D-PB-NUMBER         PIC ZZ9999 BLANK ZERO.
           03  FILLER              PIC X.
           03  D-EVENT-DATE        PIC 99/99/99 BLANK ZERO.
           03  FILLER              PIC X.
           03  D-FIRST-LN          PIC ZZZ999   BLANK ZERO.
           03  FILLER              PIC XX.
           03  D-FIRST-DATE        PIC 99/99/99 BLANK ZERO.

       01  D-LINE2 REDEFINES DETAIL-LINE.
           03  G-GROUP.
               05  G-PROMPT1        PIC X(7).
               05  FILLER           PIC X.
               05  G-GRNO           PIC X(4).
               05  G-BRNO           REDEFINES G-GRNO
                                    PIC 9(4).
               05  FILLER           PIC X.
               05  G-GR-LEVEL       PIC 9(1).
               05  FILLER           PIC X(6).

           03  FILLER       PIC X(2).
           03  G-CLASS-PROMPT       PIC X(9).
           03  G-LOANS      PIC ZZZZ,ZZZ,ZZ9-.                      
           03  G-ELAPSED    PIC ZZZZ,ZZZ,ZZ9.99-.
           03  G-AVG        PIC ZZZZ9.99-.                      
           03  G-SKIPPED    PIC ZZZZ,ZZ9- OCCURS 7.

      *-----------------------------------------------------------------
       01  RANGE-LINE REDEFINES DETAIL-LINE.
           03  FILLER           PIC X(35).
           03  RANGE-SELECTION.
               04  R-NUM        PIC Z(9)9.

       COPY "LIBLP/EMSFRG_CMD.CPY".
       COPY "LIBWI/GRSCANW.CPY".
       COPY "LIBLP/LPSRCDW.CPY".
       COPY "LIBGB/GETPRW.CPY".
       COPY "LIBGB/GRWORKW.CPY".
       COPY "LIBWI/GRTOTLW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBGB/MKTEMPW_CMD.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/DACCESSW.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/SYSTEMW.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBLP/LNTIM6_DEF.CPY".
       COPY "LIBLP/LNTIM6_WKS.CPY".
       COPY "LIBLP/LPSTATW.CPY".
       COPY "LIBLP/LPMDTEW.CPY".
       COPY "LIBLP/LPAMTSW.CPY".
       COPY "LIBLP/TFRMEMOW.CPY".
       COPY "LIBGB/BRSECUREW.CPY".
       COPY "LIBGB/PASSWDW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".
       COPY "LIBGB/GETFMW.CPY".

       SCREEN SECTION.
       COPY "LIBLP/LNTIM6_SCN.CPY".

      ******************************************************************
      * P R O G R A M   C O N T R O L
      ******************************************************************
       PROCEDURE DIVISION
            USING FORM-PATHNAME.
       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
               PR-FILE WK-FILE.
       COPY "LIBGB/DECLRP.CPY".
           PERFORM CLOSE-SP1-FILE.
           PERFORM CLOSE-LP1-FILE.
      *    PERFORM CLOSE-LN7-FILE.
      *    PERFORM CLOSE-LN6-FILE.
           PERFORM CLOSE-LN5-FILE.
           PERFORM CLOSE-LN4-FILE.
           PERFORM CLOSE-LN3-FILE.
           PERFORM CLOSE-LN1-FILE.
           PERFORM CLOSE-LM1-FILE.
           PERFORM CLOSE-BW1-FILE.
           PERFORM CLOSE-GR1-FILE.
           PERFORM CLOSE-BR-FILE.
           PERFORM CLOSE-PR-FILE.
           CLOSE WK-FILE.
       COPY "LIBGB/DECLRP2.CPY".
 
      **************************************************
       MAIN-MODULE SECTION.
      **************************************************

           PERFORM SQL-CONNECT.

      D    STOP MESS.
           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "LNTIM6" TO FORM-PROG VDU-FORM-NAME.

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
           IF NOT IO-GOOD
              GO TO IO-ERROR.
           GO TO MOVE-BRANCH-PATH-INFO.
       NEXT-BRANCH-NO.
           PERFORM READ-BR-FILE-NEXT.
           IF IO-BAD OR BR-NO > END-BRANCH
              GO TO END-ROUTINE.

       MOVE-BRANCH-PATH-INFO.
           

           MOVE EXT-FILPATH-BASE           TO FILPATH-BASE.
           MOVE BR-MACHINE                 TO FILPATH-MACHINE.
           MOVE BR-DATA-PATH               TO FILPATH-DATA.

           MOVE "B"                        TO BW-PATH-OVERRIDE
                                              LN-PATH-OVERRIDE
                                              LP-PATH-OVERRIDE
                                              LM-PATH-OVERRIDE.
                                              

           PERFORM OPEN-BW1-FILE.
           PERFORM OPEN-LN3-FILE.
           PERFORM OPEN-LP1-FILE.
           PERFORM OPEN-LM1-FILE.


           MOVE BR-NO     TO LN-OWNBR
                             QLN3-WBEG-OWNBR
                             QLN3-WEND-OWNBR.
           MOVE 1         TO LN-CLASS
                             QLN3-WBEG-CLASS.
           MOVE 5         TO QLN3-WEND-CLASS.
           MOVE 0         TO LN-ACCTNO
                             QLN3-WBEG-ACCTNO.
           MOVE ALL "9"   TO QLN3-WEND-ACCTNO.

           PERFORM START-LN3-FILE. 

       NEXT-LN.
           PERFORM READ-LN3-FILE-NEXT.
           IF IO-BAD OR (LN-OWNBR NOT = LN-PATH-OWNBR)
              GO TO END-OF-GROUP.

      * '**', '*I' "*R' "*U'
           IF LN-POCD-NONLOAN
              GO TO NEXT-LN.

           IF LN-OWNBR > END-BRANCH
              GO TO END-OF-GROUP.

           IF LN-CLASS NOT = 1
              IF LN-CLASS NOT = 5
                 GO TO NEXT-LN.

           IF LN-ENTDATE > BEG-DATE
              GO TO NEXT-LN.

           IF LN-POFFDATE NOT = 0
              IF LN-PROMDATE2 NOT = 0
                 MOVE LN-PROMDATE2 TO SYS-DATE
              ELSE
                 MOVE LN-POFFDATE TO SYS-DATE
              END-IF
              IF SYS-DATE NOT > BEG-DATE
                 GO TO NEXT-LN.

           MOVE 0 TO HOLD-PL-TRDATE SAVE-LN-PLDATE.
           IF LN-PLDATE NOT = 0
              PERFORM GET-PL-TRANS-DATE
              IF HOLD-PL-TRDATE NOT > BEG-DATE
                 GO TO NEXT-LN.

           IF LN-CONVERCD NOT = " "
              IF LN-POFFDATE = 0
                 IF LN-CURBAL = 0
                    IF LN-INTBAL = 0
                       IF LN-LCBAL = 0
                          IF LN-OTHBAL = 0
                             GO TO NEXT-LN.
           MOVE 0 TO SRC.

           IF LN-OWNBR NOT = LN-ORGBR
              PERFORM CHECK-XFER-IN-DATE
              IF XFERED-AFTER-BEGDATE-FG = "Y"
                 GO TO NEXT-LN.

           IF LN-POCD = "BT"
              PERFORM CHECK-XFERED-OUT-DATE
              IF BT-OLD-PO-FG = "Y"
                 GO TO NEXT-LN
              ELSE
                 MOVE 5 TO SRC
                 GO TO READ-SPR.

           IF LN-CONVERCD NOT = " "
              MOVE 7 TO SRC
              GO TO READ-SPR.

           IF LN-SRCD = "FB"
              MOVE 2 TO SRC
              GO TO READ-SPR.

           IF LN-SRCD NOT = "PB"
              MOVE 1 TO SRC
              GO TO READ-SPR.

       READ-SPR.
           MOVE LN-ORGST     TO SP-ORGST.
           MOVE LN-SPRCLASS  TO SP-SPRCLASS.
           MOVE LN-SUBCLASS  TO SP-SUBCLASS.
           MOVE LN-LAWCODE   TO SP-LAWCODE.

           IF LPWSSP-SP1-KEY NOT = SP1-KEY
              MOVE SP1-KEY TO LPWSSP-SP1-KEY
              PERFORM READ-SP1-FILE
              IF IO-FG NOT = 0
                 GO TO NEXT-LN.

           IF (LN-SRCD = "PB" AND SRC < 5)
              PERFORM CHECK-LOAN-LT-10-PERC
              IF LOAN-IS-LT-10-PERC = "Y"
                 MOVE 4 TO SRC
              ELSE
                 MOVE 3 TO SRC.

           MOVE LN-SSNO(1) TO BW-SSNO.
           PERFORM READ-BW1-FILE.
           IF IO-FG NOT = 0
              MOVE SPACES TO BW-REC.

           PERFORM LPSTAT-DISPLAY.
           MOVE LN-ENTDATE TO SAVE-LN-ENTDATE.
           MOVE LN-SRCD    TO SAVE-LN-SRCD.
           MOVE LN-CLASS   TO SAVE-LN-CLASS.
           MOVE SRC        TO SAVE-LN-SRC.
           MOVE LN-PO-TRCD TO SAVE-LN-PO-TRCD.
           MOVE LN-POFFDATE TO SAVE-LN-POFFDATE.
           IF LN-POFFDATE NOT = 0
              IF LN-PROMDATE2 NOT = 0
                 MOVE LN-PROMDATE2 TO SAVE-LN-POFFDATE.
           IF LN-PLDATE NOT = 0
              MOVE HOLD-PL-TRDATE TO SAVE-LN-PLDATE.
           MOVE LN-OWNBR   TO HOLD-LN-OWNBR.
           MOVE LN-CLASS   TO HOLD-LN-CLASS.
           MOVE LN-ACCTNO  TO HOLD-LN-ACCTNO.
           MOVE LN-SSNO(1) TO HOLD-LN-SSNO.
           MOVE SPACES TO HOLD-STAT.
           MOVE 0 TO HOLD-PB-LN-ACCTNO.

           IF SRC NOT = 4
              MOVE 0 TO LESS-THAN-10-FIRST-LN
                        LESS-THAN-10-LOAN-DATE
              GO TO FIRST-NOT-FOUND.

           MOVE LN-ACCTNO   TO LESS-THAN-10-FIRST-LN
                               LESS-THAN-10-LAST-LN.
           MOVE LN-LOANDATE TO LESS-THAN-10-LOAN-DATE.

       CHECK-PRIOR-LOAN.
           MOVE LN-PRLNNO TO LN-ACCTNO.
           PERFORM READ-LN1-FILE.
           IF IO-BAD
              GO TO FIRST-NOT-FOUND.

           IF LN-SRCD NOT = "PB"
              GO TO FOUND-FIRST-LOAN.

           MOVE LN-ORGST     TO SP-ORGST.
           MOVE LN-SPRCLASS  TO SP-SPRCLASS.
           MOVE LN-SUBCLASS  TO SP-SUBCLASS.
           MOVE LN-LAWCODE   TO SP-LAWCODE.

           IF LPWSSP-SP1-KEY NOT = SP1-KEY
              MOVE SP1-KEY TO LPWSSP-SP1-KEY
              PERFORM READ-SP1-FILE
              IF IO-FG NOT = 0
                 GO TO NEXT-LN.

           PERFORM CHECK-LOAN-LT-10-PERC.
           IF LOAN-IS-LT-10-PERC = "N"
              GO TO FOUND-FIRST-LOAN.
           
           GO TO CHECK-PRIOR-LOAN.

       FOUND-FIRST-LOAN.
           MOVE LN-ACCTNO   TO LESS-THAN-10-FIRST-LN.
           MOVE LN-LOANDATE TO LESS-THAN-10-LOAN-DATE.

      * FIRST NOT FOUND SITUATION SHOULDN'T HAPPEN, THIS WOULD ONLY
      * BE IF PRIOR LN# OF PB IS INVALID, OR BAD SPR. IN THIS CASE,
      * I'M SKIPPING MOVING IN THE LESS-THAN-10-FIRST-LN & DATE,
      * IT WILL LEAVE IN LOAN & DATE FROM OPEN LOAN FOUND.
       FIRST-NOT-FOUND.

           IF SAVE-LN-PLDATE = 0
              IF SAVE-LN-POFFDATE = 0
                 MOVE "ORIG IS OPEN" TO HOLD-STAT
                 MOVE END-DATE TO EVENT-DATE
                 GO TO CONTINUE-PROCESSING.

           IF SAVE-LN-POFFDATE NOT = 0
              IF SAVE-LN-POFFDATE > END-DATE
                 MOVE "PD OUT AFTER END DATE" TO HOLD-STAT
                 MOVE END-DATE TO EVENT-DATE
                 GO TO CONTINUE-PROCESSING
              ELSE
                 IF LN-PO-TRCD NOT = "PB"
                    MOVE "PD OUT BEFORE END DATE" TO HOLD-STAT
                    MOVE SAVE-LN-POFFDATE TO EVENT-DATE
                    GO TO CONTINUE-PROCESSING
                 ELSE
                    PERFORM FIND-PB-CHAIN
                    GO TO CONTINUE-PROCESSING.

           IF SAVE-LN-PLDATE NOT = 0
              IF SAVE-LN-PLDATE NOT > END-DATE
                 MOVE SAVE-LN-PLDATE TO EVENT-DATE
                 MOVE "P&L BEFORE END DATE" TO HOLD-STAT
                 GO TO CONTINUE-PROCESSING
              ELSE
                 MOVE END-DATE TO EVENT-DATE
                 MOVE "P&L AFTER END DATE" TO HOLD-STAT
                 GO TO CONTINUE-PROCESSING.

      *-----------------------------------------------------------------
      * TEST FIRST TIME THROUGH
      *-----------------------------------------------------------------
       CONTINUE-PROCESSING.
           IF FIRST-TIME = "Y"
              MOVE BR-NO TO HOLD-BRNO
              MOVE BR-NAME TO HOLD-BRNAME.

      *-----------------------------------------------------------------
      * TEST BREAK IN BRANCH
      *-----------------------------------------------------------------
           IF BR-NO NOT = HOLD-BRNO
              MOVE BR-NO TO HOLD-BRNO
              MOVE BR-NAME TO HOLD-BRNAME.

           PERFORM LN-DETAIL.

      * DONT NEED REPOSITIONING OF KEY FOR LN3-FILE-NEXT. CURSOR HOLDS RECORDS
      *    MOVE HOLD-LN-OWNBR TO LN-OWNBR.
      *    MOVE HOLD-LN-CLASS TO LN-CLASS.
      *    MOVE HOLD-LN-ACCTNO TO LN-ACCTNO.

      *    PERFORM START-LN3-FILE-GREATER.

           GO TO NEXT-LN.

      *-----------------------------------------------------------------
       END-OF-GROUP.
           PERFORM CLOSE-LN3-FILE.
           PERFORM CLOSE-BW1-FILE.
           PERFORM CLOSE-LP1-FILE.
           PERFORM CLOSE-LM1-FILE.

           GO TO NEXT-GROUP-LOC.

      *-----------------------------------------------------------------
       END-ROUTINE.
           IF FIRST-TIME = "Y"
              GO TO END-PROGRAM.
           
           IF ( GTT-ISO NOT = "S" )
              MOVE 99 TO LN-COUNT
              PERFORM GROUP-TOTAL-TABLE
              PERFORM PRINT-RANGE-LINES
           ELSE
              PERFORM PRINT-RANGE-LINES.


           GO TO END-PROGRAM.
 
      ******************************************************************
       CHECK-LOAN-LT-10-PERC SECTION. 
      ******************************************************************

      * CHECK IF LOAN IS A PB FOR < 10%

           PERFORM LOAN-CALCULATIONS.

           COMPUTE WS-AMTFIN = TOTAL-NOTE - LN-INTCHG - LN-SERCHG.
           COMPUTE PRIOR-PAYOFF ROUNDED = LN-PRBAL + LN-PRINTDUE
                                       - LN-PRFINCHGRB - LN-PRINSRB.

      * METHOD 1 = AMOUNT FINANCED ON THE PB LOAN, SUBTRACT PRIOR LOAN PAYOFF,
      *            TAKE THAT AMT AND DIVIDE BY THE PRIOR LOAN PAYOFF
      *            & SEE IF THE RESULT IS < 10% (.10)

      * METHOD 2 = ADD PROCEEDS ON THE PB LOAN PLUS FEE CODES 90, 92 & 95,
      *            AND DIVIDE THAT BY THE PRIOR LOAN PAYOFF,
      *            & SEE IF THE RESULT IS < 10% (.10)

           IF CALC-METHOD = 2
              MOVE 0 TO TOT-FEES
              PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 10
                 IF LN-FEECODE(SUB) = "90"
                    ADD LN-FEEAMT(SUB) TO TOT-FEES
                 END-IF
                 IF LN-FEECODE(SUB) = "92"
                    ADD LN-FEEAMT(SUB) TO TOT-FEES
                 END-IF
                 IF LN-FEECODE(SUB) = "95"
                    ADD LN-FEEAMT(SUB) TO TOT-FEES
                 END-IF
              END-PERFORM.

           IF CALC-METHOD = 1
              COMPUTE WS-AMOUNT =
                           (WS-AMTFIN - PRIOR-PAYOFF) / PRIOR-PAYOFF
           ELSE
              COMPUTE WS-AMOUNT = 
                           (LN-PROCEEDS + TOT-FEES)  / PRIOR-PAYOFF.

           IF WS-AMOUNT < .10
              MOVE "Y" TO LOAN-IS-LT-10-PERC
           ELSE
              MOVE "N" TO LOAN-IS-LT-10-PERC.

      *************************************
       FIND-PB-CHAIN SECTION.
      *************************************
           MOVE HOLD-LN-ACCTNO TO PRIOR-LOAN-TO-FIND.
           MOVE 0 TO EVENT-DATE.
       START-BORROWER-LOANS.
           MOVE LN-PATH-OWNBR    TO LN-OWNBR
                                    QLN4-WBEG-OWNBR
                                    QLN4-WEND-OWNBR.
           MOVE HOLD-LN-SSNO     TO LN-SSNO(1)
                                    QLN4-WBEG-SSNO1
                                    QLN4-WEND-SSNO1.

           PERFORM START-LN4-FILE.
       NEXT-LN4.
           PERFORM READ-LN4-FILE-NEXT.
           IF IO-BAD OR (LN-OWNBR NOT = LN-PATH-OWNBR) OR
                          (LN-SSNO(1) NOT = HOLD-LN-SSNO)
              GO TO START-BORROWER-LOANS5.

           IF LN-POCD-NONLOAN
              GO TO NEXT-LN4.
      * IF THESE MATCH, WE FOUND THE PB RENEWAL LOAN
           IF LN-PRLNNO = PRIOR-LOAN-TO-FIND
              GO TO PB-CHAIN-FOUND-LOAN.
           GO TO NEXT-LN4.

       START-BORROWER-LOANS5.
           MOVE LN-PATH-OWNBR    TO LN-OWNBR
                                    QLN5-WBEG-OWNBR
                                    QLN5-WEND-OWNBR.
           MOVE HOLD-LN-SSNO     TO LN-SSNO(2)
                                    QLN5-WBEG-SSNO2
                                    QLN5-WEND-SSNO2.
           PERFORM START-LN5-FILE.
       NEXT-LN5.
           PERFORM READ-LN5-FILE-NEXT.
           IF IO-BAD OR (LN-OWNBR NOT = LN-PATH-OWNBR) OR
                          (LN-SSNO(2) NOT = HOLD-LN-SSNO)
      *       GO TO START-BORROWER-LOANS6.
              GO TO FIND-PB-CHAIN-EXIT.

           IF LN-POCD-NONLOAN
              GO TO NEXT-LN5.

      * IF THESE MATCH, WE FOUND THE PB RENEWAL LOAN
           IF LN-PRLNNO = PRIOR-LOAN-TO-FIND
              GO TO PB-CHAIN-FOUND-LOAN.

           GO TO NEXT-LN5.

       START-BORROWER-LOANS6.

      *    MOVE LN-PATH-OWNBR    TO LN-OWNBR
      *                             QLN6-WBEG-OWNBR
      *                             QLN6-WEND-OWNBR.
      *    MOVE HOLD-LN-SSNO     TO LN-SSNO(3)
      *                             QLN6-WBEG-SSNO3
      *                             QLN6-WEND-SSNO3.
      *    PERFORM START-LN6-FILE.
      *NEXT-LN6.
      *    PERFORM READ-LN6-FILE-NEXT.
      *    IF IO-BAD OR (LN-OWNBR NOT = LN-PATH-OWNBR) OR
      *                   (LN-SSNO(3) NOT = HOLD-LN-SSNO)
      *       GO TO START-BORROWER-LOANS7.

      *    IF LN-POCD-NONLOAN
      *       GO TO NEXT-LN6.
      * IF THESE MATCH, WE FOUND THE PB RENEWAL LOAN
      *    IF LN-PRLNNO = PRIOR-LOAN-TO-FIND
      *       GO TO PB-CHAIN-FOUND-LOAN.
      *    GO TO NEXT-LN6.

      *START-BORROWER-LOANS7.
      *    MOVE LN-PATH-OWNBR    TO LN-OWNBR
      *                             QLN7-WBEG-OWNBR
      *                             QLN7-WEND-OWNBR.
      *    MOVE HOLD-LN-SSNO     TO LN-SSNO(4)
      *                             QLN7-WBEG-SSNO4
      *                             QLN7-WEND-SSNO4.

      *    PERFORM START-LN7-FILE.
      *NEXT-LN7.
      *    PERFORM READ-LN7-FILE-NEXT.
      *    IF IO-BAD OR (LN-OWNBR NOT = LN-PATH-OWNBR) OR
      *                   (LN-SSNO(4) NOT = HOLD-LN-SSNO)
      *       GO TO FIND-PB-CHAIN-EXIT.

      *    IF LN-POCD-NONLOAN
      *       GO TO NEXT-LN7.
      * IF THESE MATCH, WE FOUND THE PB RENEWAL LOAN
      *    IF LN-PRLNNO = PRIOR-LOAN-TO-FIND
      *       GO TO PB-CHAIN-FOUND-LOAN.
      *    GO TO NEXT-LN7.

       PB-CHAIN-FOUND-LOAN.
           MOVE LN-ORGST     TO SP-ORGST.
           MOVE LN-SPRCLASS  TO SP-SPRCLASS.
           MOVE LN-SUBCLASS  TO SP-SUBCLASS.
           MOVE LN-LAWCODE   TO SP-LAWCODE.
           IF LPWSSP-SP1-KEY NOT = SP1-KEY
              MOVE SP1-KEY TO LPWSSP-SP1-KEY
              PERFORM READ-SP1-FILE.

           PERFORM CHECK-LOAN-LT-10-PERC.
           IF LOAN-IS-LT-10-PERC = "N"
              MOVE SAVE-LN-POFFDATE TO EVENT-DATE
              MOVE "PDOUT WITH PB > 10%" TO HOLD-STAT
              MOVE LN-ACCTNO TO HOLD-PB-LN-ACCTNO
              GO TO FINISH-DETAIL-LINE.

      * WE KNOW THIS PB WAS IN DATE RANGE, SINCE PB WAS B4 END-DATE
           IF LN-PLDATE = 0
              IF LN-POFFDATE = 0
                 MOVE "PB < 10% LOAN OPEN" TO HOLD-STAT
                 MOVE LN-ACCTNO TO HOLD-PB-LN-ACCTNO
                 MOVE END-DATE TO EVENT-DATE
                 GO TO FINISH-DETAIL-LINE.

      * IF CHARGED OFF BEFORE END-DATE, COUNT AS OPEN:
           IF LN-PLDATE NOT = 0
              PERFORM GET-PL-TRANS-DATE
              IF HOLD-PL-TRDATE NOT > END-DATE
                 MOVE "PB < 10% P&L B4 END DATE" TO HOLD-STAT
                 MOVE LN-ACCTNO TO HOLD-PB-LN-ACCTNO
                 MOVE HOLD-PL-TRDATE TO EVENT-DATE
                 GO TO FINISH-DETAIL-LINE
              ELSE
                 MOVE "PB < 10% P&L AFTER END DTE" TO HOLD-STAT
                 MOVE LN-ACCTNO TO HOLD-PB-LN-ACCTNO
                 MOVE END-DATE TO EVENT-DATE
                 GO TO FINISH-DETAIL-LINE.
    
      * IF PAID OFF AFTER END-DATE, COUNT AS OPEN:
           IF LN-POFFDATE NOT = 0
              IF LN-PROMDATE2 NOT = 0
                 MOVE LN-PROMDATE2 TO SYS-DATE
              ELSE
                 MOVE LN-POFFDATE TO SYS-DATE
              END-IF
              IF SYS-DATE > END-DATE
                 MOVE "PB<10% PDOUT AFTER END DTE" TO HOLD-STAT
                 MOVE LN-ACCTNO TO HOLD-PB-LN-ACCTNO
                 MOVE END-DATE TO EVENT-DATE
                 GO TO FINISH-DETAIL-LINE
              ELSE
                 IF LN-PO-TRCD NOT = "PB"
                    MOVE "PB <10% PDOUT B4 END DATE" TO HOLD-STAT
                    MOVE LN-ACCTNO TO HOLD-PB-LN-ACCTNO
                    MOVE SYS-DATE TO EVENT-DATE
                    GO TO FINISH-DETAIL-LINE
                 ELSE
                    MOVE LN-ACCTNO TO PRIOR-LOAN-TO-FIND
                    MOVE LN-POFFDATE TO SAVE-LN-POFFDATE
                    IF LN-PROMDATE2 NOT = 0
                       MOVE LN-PROMDATE2 TO SAVE-LN-POFFDATE
                    END-IF
                    GO TO START-BORROWER-LOANS.

           GO TO NEXT-LN4.

       FINISH-DETAIL-LINE.
       FIND-PB-CHAIN-EXIT.

      * IF NO LOAN FOUND WITH PB'ED LOAN AS PRIOR (THIS SHOULD NOT
      *    HAPPEN, BUT IT DOES), USE PAYOFF DATE OF PB'ED LOAN

           IF EVENT-DATE = 0
              MOVE "PB'ED LOAN NOT FOUND!" TO HOLD-STAT
              MOVE SAVE-LN-POFFDATE TO EVENT-DATE.


      ******************************************************************
      * IF ACCT WAS CHARGED OFF AFTER MONTH-END, PROCESS AS
      * NORMAL OPEN ACCT, OTHERWISE PROCESS AS P&L.  NOTE:
      * WILL HAVE TO READ LP TO GET TRANS DATE SINCE PL'S ARE
      * BACK-DATED, AT LEAST P2'S:
      ******************************************************************
       GET-PL-TRANS-DATE SECTION.
      ******************************************************************

           MOVE LN-PLDATE TO HOLD-PL-TRDATE.
           MOVE "PL" TO TEST-LN-PLCD.
           IF LN-PLTYPE = "2" OR "3" OR "4" OR "5"
              MOVE LN-PLTYPE TO TEST-PLTYPE.

           MOVE LP-PATH-OWNBR TO LP-BRNO
                                 QLP1-WBEG-BRNO
                                 QLP1-WEND-BRNO.
           MOVE LN-ACCTNO     TO LP-ACCTNO
                                 QLP1-WBEG-ACCTNO
                                 QLP1-WEND-ACCTNO.
           MOVE ALL "9"       TO LP-SEQNO
                                 QLP1-WEND-SEQNO.
           MOVE ALL "0"       TO QLP1-WBEG-SEQNO.

           PERFORM START-LP1-FILE-NOT-GREATER.
       NEXT-LP.
           PERFORM READ-LP1-FILE-PREVIOUS.
           IF IO-FG NOT = 0
              GO TO EXIT-GET-LP-INFO.
            
           IF (LP-BRNO NOT = LN-OWNBR) OR (LP-ACCTNO NOT = LN-ACCTNO)
              GO TO EXIT-GET-LP-INFO.

           IF LP-TRCD NOT = TEST-LN-PLCD
              GO TO NEXT-LP.

           IF LP-REV NOT = SPACES
              GO TO NEXT-LP.

           MOVE LP-TRDATE TO HOLD-PL-TRDATE.
       EXIT-GET-LP-INFO.
           EXIT.

      ******************************************************************
       CHECK-XFER-IN-DATE SECTION.
      ******************************************************************

      * IF LOAN WAS TRANSFERED TO THIS BRANCH AFTER BEGINNING DATE,
      * WE WILL SKIP IT, OTHERWISE, WILL PROCESS THRU REPORT

           MOVE "N" TO XFERED-AFTER-BEGDATE-FG.

           MOVE LM-PATH-OWNBR TO LM-BRNO
                                 QLM1-WBEG-BRNO
                                 QLM1-WEND-BRNO.
           MOVE LN-ACCTNO     TO LM-ACCTNO
                                 QLM1-WBEG-ACCTNO
                                 QLM1-WEND-ACCTNO.
           MOVE 0             TO LM-DATE
                                 QLM1-WBEG-DATE-REV.
           MOVE 0             TO LM-SEQ 
                                 QLM1-WBEG-SEQ.
           MOVE ALL "9"       TO QLM1-WEND-DATE-REV
                                 QLM1-WEND-SEQ.

           PERFORM START-LM1-FILE.
       CHECK-LM-NEXT.
           PERFORM READ-LM1-FILE-NEXT.
           IF IO-FG = 9 OR (LM-BRNO NOT = LM-PATH-OWNBR)
              GO TO EXIT-CHECK-LM-TRANSFER.

           IF LM-ACCTNO NOT = LN-ACCTNO
              GO TO EXIT-CHECK-LM-TRANSFER.
           IF LM-DATE >= MEMO-BEG-DATE
              GO TO EXIT-CHECK-LM-TRANSFER.
           IF LM-MEMO-TYPE NOT = "9"
              GO TO CHECK-LM-NEXT.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      * TFR TO BR #### FR BR ####, FOR $ZZZZZ9.99 ---> 12345678     [LM]
      * TFR FR BR #### TO BR ####, FOR $ZZZZZ9.99 <--- 12345678     [LM]
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE LM-MEMO                    TO TFRMEMO-MEMO.

           IF ( TFRMEMO-PRIMARY NOT = "TFR FR BR" ) AND
              ( TFRMEMO-BRNO2   NOT = BR-NO       )
              GO TO CHECK-LM-NEXT.

           MOVE "Y" TO XFERED-AFTER-BEGDATE-FG.

       EXIT-CHECK-LM-TRANSFER.
           EXIT.

      ******************************************************************
      * PROBLEM WHEN BRANCHES TRANSFERRED OLD PAID OUTS, E.G. PAID OUT
      * ACCT IN 2005, XFERED ACCT IN 2015.  XFER CHANGES POFFDATE, SO
      * ACCTS APPEAR TO BE OPEN AS OF BEG DATE WHEN THEY WERE REALLY
      * PAID OFF YEARS AGO.  LOOK FOR BT PMT, THEN READ PREV RECORD
      * (WHICH SHOULD BE ORIG PAYOFF) FOR DATE
      ******************************************************************
       CHECK-XFERED-OUT-DATE SECTION.
           MOVE "N" TO BT-OLD-PO-FG.

           MOVE LP-PATH-OWNBR TO LP-BRNO
                                 QLP1-WBEG-BRNO
                                 QLP1-WEND-BRNO.
           MOVE LN-ACCTNO     TO LP-ACCTNO
                                 QLP1-WBEG-ACCTNO
                                 QLP1-WEND-ACCTNO.
           MOVE ALL "9"       TO LP-SEQNO
                                 QLP1-WEND-SEQNO.
           MOVE ALL "0"       TO QLP1-WBEG-SEQNO.

           PERFORM START-LP1-FILE-NOT-GREATER.
       FIRST-LP-BT.
           PERFORM READ-LP1-FILE-PREVIOUS.
           IF IO-FG NOT = 0
              GO TO EXIT-CHECK-XFERED-OUT.

           IF (LP-BRNO NOT = LN-OWNBR) OR (LP-ACCTNO NOT = LN-ACCTNO)
              GO TO EXIT-CHECK-XFERED-OUT.

           IF LP-TRCD NOT = "BT"
              GO TO EXIT-CHECK-XFERED-OUT.
       PREV-LP-BT.
           PERFORM READ-LP1-FILE-PREVIOUS.
           IF IO-FG NOT = 0
              GO TO EXIT-CHECK-XFERED-OUT.
           IF LP-ACCTNO NOT = LN-ACCTNO
              GO TO EXIT-CHECK-XFERED-OUT.
           IF NOT
             (LP-TRCD = "PO" OR "RN" OR "SC" OR "PB" OR "RB" OR "RO")
                 GO TO EXIT-CHECK-XFERED-OUT.
      *CHECK FOR ZERO BAL, COULD BE PB BAL FWD PAYMENT
           IF LP-CURBAL NOT = 0
              GO TO EXIT-CHECK-XFERED-OUT.
           IF LP-TRDATE NOT > BEG-DATE
              MOVE "Y" TO BT-OLD-PO-FG.
       EXIT-CHECK-XFERED-OUT.
           EXIT.

      ******************************************************************
       INITIALIZATION SECTION.
      ******************************************************************
           MOVE LNTIM6-QUEUE-POS TO Q-POS.
           MOVE LNTIM6-COPIES-POS TO C-POS.
           MOVE LNTIM6-PRU-POS TO P-POS.
           MOVE SPACES TO Q-BUF C-BUF P-BUF.
       COPY "LIBGB/EOINIT.CPY".

           MOVE 0 TO PAGE-NUMBER.

       SET-HEAD.
           MOVE EXT-CONAME-CONAME TO H-NAME.
           MOVE EXT-CONAME-SYSDATE TO H-DATE.
           MOVE EXT-CONAME-USER-ID TO H-USERID.
           IF EXT-ROUTE-BUF = "00"
              MOVE "REPORT IN PROGRESS............" TO WR-BUF
              MOVE MESS-LIST TO WR-LIST
              PERFORM CALL-WRFORM.

           IF BEG-BRANCH = 0
              IF END-BRANCH = 0
                 MOVE BEG-BRANCH TO SV-BEG-BR
                 MOVE END-BRANCH TO SV-END-BR
                 MOVE EXT-CONAME-USER-LOC TO BEG-BRANCH
                                   END-BRANCH.
      
           MOVE MKTEMP-DEFAULT TO MKTEMP-BUF.
           PERFORM MKTEMP-CALL.
           IF STAT NOT = "00"
              MOVE "CANNOT MAKE SORT KEY FILE" TO MESS
              PERFORM SEND-MESS
              GO TO ENDIT.
           PERFORM OPEN-WK-FILE-OUTPUT.
           PERFORM CLOSE-WK-FILE.
           PERFORM OPEN-WK-FILE.
           PERFORM OPEN-PR-FILE.
           PERFORM OPEN-GR1-FILE.
           PERFORM OPEN-BR-FILE.
           PERFORM OPEN-SP1-FILE

           MOVE "N" TO RECORDS-FOUND.

           COMPUTE MEMO-BEG-DATE = 99999999 - BEG-DATE.

           IF LOAN-TYPE = "N"
              MOVE "NEW BORROWERS ONLY, METHOD &" TO H-TYPE
              MOVE 1 TO SEL-SRC
              MOVE " NO. OF NB'S " TO H-SEL-SRC
           ELSE IF LOAN-TYPE = "F"
              MOVE "FORMER BORROWERS ONLY, METHOD &" TO H-TYPE
              MOVE 2 TO SEL-SRC
              MOVE " NO. OF FB'S " TO H-SEL-SRC
           ELSE IF LOAN-TYPE = "P" 
              MOVE "PB'S > 10% ONLY, METHOD &" TO H-TYPE
              MOVE 3 TO SEL-SRC
              MOVE "# PB'S > 10% " TO H-SEL-SRC
           ELSE 
              MOVE "PB'S < 10% ONLY, METHOD &" TO H-TYPE
              MOVE 4 TO SEL-SRC
              MOVE "# PB'S < 10% " TO H-SEL-SRC.
           INSPECT H-TYPE REPLACING FIRST "&" BY CALC-METHOD.
           MOVE BEG-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO H-BEG-DATE.
           MOVE END-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO H-END-DATE.
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 7
              ADD 1 TO SRC
              IF SRC = SEL-SRC
                 ADD 1 TO SRC
              END-IF
              MOVE SRC-DESC(SRC) TO H-SRC(SUB)
           END-PERFORM.

      *
      *    PERFORM SETUP-LN-ARCHIVE-PATH.

           IF ( GROUP-FLAG = "Y" )
              MOVE LOW-VALUES TO WS-GTT-TOTAL-TABLE
              MOVE ENT-GROUP TO GR-ID
              PERFORM SEARCH-GR-INIT
              GO TO EXIT-INITIALIZE
           ELSE
              MOVE SPACES TO TOTAL-LEV-TABLE
              MOVE "Y"    TO TOTAL-LEVEL-TABLE(9).
       
           GO TO EXIT-INITIALIZE.

       ENDIT.
           MOVE "X" TO EXT-EOBUF-ERRCD.
       EXIT-INITIALIZE.
           EXIT.

      ******************************************************************
       EO-EXEC-AUDIT SECTION.
      ******************************************************************
           MOVE EXT-EOBUF-DATE1 TO BEG-DATE.    
           MOVE EXT-EOBUF-DATE2 TO END-DATE.

      ******************************************************************
       ENTER-PARAMETERS SECTION.
      ******************************************************************
       ENTER-PARM.
           IF VDUSTATUS = "1U"
              GO TO INCL-DETAIL.

      *-----------------------------------------------------------------
       BR-01.
           MOVE LEGEND-BR-01 TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "E  A000BR1." TO VDU.
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
              MOVE 0 TO WGR-SRESULT
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
           MOVE "PLEASE ENTER A GROUP" TO MESS
              PERFORM SEND-MESS
              GO TO BR-01.
           MOVE "N" TO GROUP-FLAG.
           MOVE WGR-SRESULT TO BEG-BRANCH.
           MOVE "SN N040BR1." TO VDU.
           MOVE BEG-BRANCH TO VDUNUM0.
           PERFORM SCREENIO.
      *-----------------------------------------------------------------
       BR-02.
           MOVE LEGEND-BR-02 TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "ENNN040BR2." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO BR-01.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO BR-02.
           MOVE VDUNUM0 TO END-BRANCH.
           IF BEG-BRANCH > END-BRANCH
              MOVE "INVALID BRANCH RANGE" TO MESS
              PERFORM SEND-MESS
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
      *----------------------------------------------------------------
       CHECK-BRANCH-SECURITY.
           PERFORM BR-RANGE-SECURITY.
 
           IF BR-SECURITY-OKAY = "N"
              GO TO BR-01.

       ENTER-TYPE.
           MOVE SPACES TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "E  A010TYPE." TO VDU.
           PERFORM SCREENIO.

           IF ( VDUSTATUS = "10" ) GO TO EXIT-PARM.
           IF ( VDUSTATUS = "1>" ) GO TO EXIT-PARM.
           IF ( VDUSTATUS = "1U" ) GO TO BR-01.
           IF NOT ( VDUSTATUS = "00" OR "1D" )
              GO TO ENTER-TYPE.

           IF NOT ( VDUBUF = "N" OR "F" OR "P" OR "L")
              GO TO ENTER-TYPE.

           MOVE VDUBUF TO LOAN-TYPE.

      *-----------------------------------------------------------------
       ENT-BEG-DATE.
           IF EXT-ROUTE-BUF NOT = "00" GO TO ENTER-CALC-METHOD.

           MOVE LEGEND-DTE-01 TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "ERNM060BEGDATE." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO ENTER-TYPE.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO ENT-BEG-DATE.
           MOVE VDU-DATE-YYYYMMDD TO BEG-DATE.    
      *-----------------------------------------------------------------
       ENT-END-DATE.
           IF EXT-ROUTE-BUF NOT = "00" GO TO ENTER-CALC-METHOD.
           MOVE LEGEND-DTE-02 TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "ERNM060ENDDATE." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO ENT-BEG-DATE.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO ENT-END-DATE.
           MOVE VDU-DATE-YYYYMMDD TO END-DATE.

           IF SYS-IS-LT 
              MOVE "END DATE CANNOT BE PRIOR TO BEGINNING DATE" TO MESS
              PERFORM SEND-MESS
              GO TO ENT-BEG-DATE.

       ENTER-CALC-METHOD.                  
           MOVE SPACES TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "ENNN010METHOD." TO VDU.
           PERFORM SCREENIO.

           IF ( VDUSTATUS = "10" ) GO TO EXIT-PARM.
           IF ( VDUSTATUS = "1>" ) GO TO EXIT-PARM.
           IF ( VDUSTATUS = "1U" )                    
              IF EXT-ROUTE-BUF NOT = "00"
                 GO TO BR-01
              ELSE 
                 GO TO ENT-END-DATE.
           IF NOT ( VDUSTATUS = "00" OR "1D" )
              GO TO ENTER-CALC-METHOD.                 

           IF NOT ( VDUNUM0 = 1 OR 2 )
              GO TO ENTER-CALC-METHOD.                    

           MOVE VDUNUM0 TO CALC-METHOD.

      *-----------------------------------------------------------------
       GTT-ISO-PARAGRAPH.

           IF ( GROUP-FLAG NOT = "Y" )
              MOVE SPACES TO TOTAL-LEV-TABLE
              MOVE "S  A010GTT-ISO." TO VDU
              MOVE "S" TO VDUBUF GTT-ISO
              PERFORM SCREENIO
              GO TO INCL-DETAIL.

           MOVE LEGEND-GTT-01 TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "E  A010GTT-ISO." TO VDU.
           PERFORM SCREENIO.
           IF ( VDUSTATUS = "10" ) GO TO EXIT-PARM.
           IF ( VDUSTATUS = "1>" ) GO TO EXIT-PARM.
           IF ( VDUSTATUS = "1U" )                    
              GO TO ENTER-CALC-METHOD.
       
           IF NOT ( VDUSTATUS = "00" OR "1D" )
              GO TO GTT-ISO-PARAGRAPH.

           IF NOT ( VDUBUF = "I" OR "O" )
              GO TO GTT-ISO-PARAGRAPH.

           MOVE VDUBUF TO GTT-ISO.

           IF ( GTT-ISO = "I" OR "O" )
              PERFORM GRTOTL-WINDOW
           ELSE
              MOVE SPACES TO TOTAL-LEV-TABLE.

       INCL-DETAIL.                  
           IF GTT-ISO = "O"
              MOVE "S   010DETAIL." TO VDU
              MOVE "N" TO INCLUDE-DETAIL VDUBUF
              PERFORM SCREENIO
              GO TO END-PARM.

           MOVE SPACES TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "E  A010DETAIL." TO VDU.
           PERFORM SCREENIO.

           IF ( VDUSTATUS = "10" ) GO TO EXIT-PARM.
           IF ( VDUSTATUS = "1>" ) GO TO EXIT-PARM.
           IF ( VDUSTATUS = "1U" ) GO TO ENT-END-DATE.
           IF NOT ( VDUSTATUS = "00" OR "1D" )
              GO TO INCL-DETAIL.                 

           IF NOT ( VDUBUF = "Y" OR "N" OR "A")
              GO TO INCL-DETAIL.                    

           MOVE VDUBUF TO INCLUDE-DETAIL.

      *-----------------------------------------------------------------
       END-PARM.
           MOVE LEGEND-ACCEPT TO LEGEND.
           PERFORM SEND-LEGEND.
       EXIT-PARM.
           EXIT.
 
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
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "S  A010TYPE." TO VDU.
           MOVE LOAN-TYPE           TO VDUBUF
           PERFORM SCREENIO.
      *    --------------------------------------
           MOVE "SNNN010METHOD."    TO VDU.
           MOVE CALC-METHOD         TO VDUNUM0.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "S  A010GTT-ISO."   TO VDU.
           MOVE GTT-ISO             TO VDUBUF.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "S  A010DETAIL."    TO VDU.
           MOVE INCLUDE-DETAIL      TO VDUBUF.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 
      ******************************************************************
       LN-DETAIL SECTION.
      ******************************************************************

           IF FIRST-TIME = "Y"
              MOVE EXT-CONAME-RPT-LINES TO LN-COUNT
              MOVE 1 TO LN-FEED
              MOVE "N" TO FIRST-TIME
              MOVE "Y" TO RECORDS-FOUND.

           MOVE BR-NO            TO D-BRNO.
           MOVE LPS-DISPLAY-LNNO TO D-LOAN-NUM.
           MOVE SAVE-LN-SRCD     TO D-SRCD.
           MOVE " " TO D-SRCD-TYPE.
           IF SAVE-LN-SRC = 3
              MOVE ">" TO D-SRCD-TYPE.
           IF SAVE-LN-SRC = 4
              MOVE "<" TO D-SRCD-TYPE.
           MOVE SAVE-LN-ENTDATE  TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY  TO D-LOANDATE.
           MOVE SAVE-LN-PO-TRCD  TO D-POCD.
           MOVE SAVE-LN-PLDATE  TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY  TO D-PLDATE.
           MOVE SAVE-LN-POFFDATE  TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY  TO D-POFFDATE.

           IF SRC = 4
              MOVE LESS-THAN-10-LOAN-DATE TO NUM-DATE
           ELSE
              MOVE SAVE-LN-ENTDATE TO NUM-DATE.
           MOVE EVENT-DATE TO SYS-DATE.
           PERFORM TIMALL.
           MOVE ELAPSED-MONTHS TO D-ELAPSED-MO.
           MOVE ELAPSED-REM TO D-ELAPSED-REM.
           COMPUTE WORK-ELAPSED ROUNDED = ELAPSED-MONTHS +
                                         (ELAPSED-REM / 30).
           MOVE WORK-ELAPSED TO D-ELAPSED.
           MOVE HOLD-STAT TO D-STAT.
           IF SRC = 5
              MOVE "TRANSFERED FROM "  TO D-STAT.
           IF SRC = 6
              MOVE "TRANSFERED TO "  TO D-STAT.
           IF SRC = 7
              MOVE "CONVERTED "  TO D-STAT.
           MOVE HOLD-PB-LN-ACCTNO TO D-PB-NUMBER.
           MOVE EVENT-DATE TO D-EVENT-DATE.
           MOVE LESS-THAN-10-FIRST-LN TO D-FIRST-LN.
           MOVE LESS-THAN-10-LOAN-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO D-FIRST-DATE.

           IF INCLUDE-DETAIL = "N"
              MOVE SPACES TO DETAIL-LINE
              GO TO END-LN-DETAIL.
           IF INCLUDE-DETAIL = "A"
              PERFORM WRITE-DETAIL-LINE 
              GO TO END-LN-DETAIL.
           IF INCLUDE-DETAIL = "Y"
              IF SAVE-LN-SRC = SEL-SRC
                 PERFORM WRITE-DETAIL-LINE.

       END-LN-DETAIL.
           PERFORM CREATE-WK.

       EXIT-LN-DETAIL.
           EXIT.

      **********************
       CREATE-WK SECTION.
      **********************
           MOVE BR-NO         TO SAVE-WK-BR.
           MOVE SAVE-LN-CLASS TO SAVE-WK-CL.
           MOVE SAVE-LN-SRC   TO SAVE-WK-SRC.

       SAVE-WORK-KEY.
           MOVE SAVE-WK-KEY TO WK-KEY.
           PERFORM READ-WK-FILE.
           IF IO-FG = 0
              GO TO CREATE-WK-UPDATE.

           MOVE " " TO WK-REC.
           MOVE SAVE-WK-KEY TO WK-KEY.

           MOVE 0 TO WK-NOLOANS
                     WK-ELAPSED-MO
                     WK-ELAPSED-REM.

           PERFORM WRITE-WK-FILE.
           IF IO-FG NOT = 0
              GO TO IO-ERROR.

           GO TO SAVE-WORK-KEY.

       CREATE-WK-UPDATE.
           ADD 1 TO WK-NOLOANS.
           ADD ELAPSED-MONTHS TO WK-ELAPSED-MO.
           ADD ELAPSED-REM TO WK-ELAPSED-REM.

       UPDATE-WK.
           PERFORM REWRITE-WK-FILE.

       EXIT-CREATE-WK.
           EXIT.


      ******************************************************************
       GTT-ADD-TOTALS SECTION.
      ******************************************************************
           IF WK-CLASS = 1
             ADD WK-NOLOANS TO GTT-NOLOANS(GTT-TOTAL-SUB 1 WK-SRC)
             ADD WK-NOLOANS TO GTT-NOLOANS(GTT-TOTAL-SUB 1 8)
             IF WK-SRC = SEL-SRC
                ADD WK-ELAPSED-MO TO GTT-ELAPSED-MO(GTT-TOTAL-SUB 1)
                ADD WK-ELAPSED-REM TO GTT-ELAPSED-REM(GTT-TOTAL-SUB 1)
              END-IF
           ELSE
             ADD WK-NOLOANS TO GTT-NOLOANS(GTT-TOTAL-SUB 2 WK-SRC)
             ADD WK-NOLOANS TO GTT-NOLOANS(GTT-TOTAL-SUB 2 8)
             IF WK-SRC = SEL-SRC
                ADD WK-ELAPSED-MO TO GTT-ELAPSED-MO(GTT-TOTAL-SUB 2)
                ADD WK-ELAPSED-REM TO GTT-ELAPSED-REM(GTT-TOTAL-SUB 2)
             END-IF.
           ADD WK-NOLOANS TO GTT-NOLOANS(GTT-TOTAL-SUB 3 WK-SRC).
           ADD WK-NOLOANS TO GTT-NOLOANS(GTT-TOTAL-SUB 3 8).
           IF WK-SRC = SEL-SRC
             ADD WK-ELAPSED-MO TO GTT-ELAPSED-MO(GTT-TOTAL-SUB 3)
             ADD WK-ELAPSED-REM TO GTT-ELAPSED-REM(GTT-TOTAL-SUB 3)
           END-IF.

      ***********************************
       GTT-COMPILE-GROUP-TOTALS SECTION.
      ***********************************
           MOVE SPACES      TO WK-KEY.
           MOVE WGR-SRESULT TO WK-BR.
           MOVE ZEROES      TO WK-CLASS WK-SRC.
           PERFORM START-WK-FILE.

        GTT-NEXT-WK1.
           PERFORM READ-WK-FILE-NEXT.
           IF ( IO-FG   NOT = 0           ) OR
              ( WK-BR NOT = WGR-SRESULT )
                 GO TO EXIT-GTT-COMPILE-GROUP-TOTALS.

           MOVE "N" TO GTT-FIRST-TIME.

       GTT-OK-TO-PROCESS.

           PERFORM GTT-CHECK-ADD-TOTALS.
           GO TO GTT-NEXT-WK1.

       EXIT-GTT-COMPILE-GROUP-TOTALS.
           EXIT.

      ************************************
       GTT-PRINT-GROUP-BR-TOTALS SECTION.
      ************************************
           MOVE WGR-SID(GTT-PREV-SUB) TO WGR-WORD.
           MOVE WGR-NUMB              TO BR-NO.
           PERFORM READ-BR-FILE.

           IF ( IO-FG NOT = 0 )
              MOVE "BRANCH #&&&& NOT ON FILE" TO BR-NAME
              INSPECT BR-NAME REPLACING FIRST "&&&&" BY BR-NO.

           MOVE SPACES    TO DETAIL-LINE.
           MOVE 9         TO GTT-TOTAL-SUB.
           MOVE "BRANCH:" TO G-PROMPT1 WS-HOLD-PROMPT.
           MOVE BR-NO     TO G-BRNO.

           PERFORM GTT-PRINT-TOTALS.

       EXIT-GTT-PRINT-GROUP-BR-TOTALS.
           EXIT.

      *********************************
       GTT-PRINT-GROUP-TOTALS SECTION.
      *********************************

           MOVE SPACES    TO DETAIL-LINE.
           MOVE "GROUP :" TO G-PROMPT1 WS-HOLD-PROMPT.
           MOVE GR-ID     TO G-GRNO.
           MOVE GR-LEVEL  TO G-GR-LEVEL.

           PERFORM GTT-PRINT-TOTALS.

       EXIT-GTT-PRINT-GROUP-TOTALS.
           EXIT.

      *****************************
       GTT-PRINT-TOTALS SECTION.
      *****************************

           MOVE "CLASS 1: " TO G-CLASS-PROMPT.
           MOVE GTT-NOLOANS(GTT-TOTAL-SUB, 1, SEL-SRC) TO G-LOANS.
           COMPUTE TOT-ELAPSED1 ROUNDED =
              (GTT-ELAPSED-MO(GTT-TOTAL-SUB, 1)) +
              (GTT-ELAPSED-REM(GTT-TOTAL-SUB, 1) / 30). 
           MOVE TOT-ELAPSED1 TO G-ELAPSED.
           COMPUTE WORK-AVG ROUNDED =
              TOT-ELAPSED1 /
              GTT-NOLOANS(GTT-TOTAL-SUB, 1, SEL-SRC).
           MOVE WORK-AVG TO G-AVG.
           MOVE 0 TO SRC.
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 7
              ADD 1 TO SRC
              IF SRC = SEL-SRC
                 ADD 1 TO SRC
              END-IF
              MOVE GTT-NOLOANS(GTT-TOTAL-SUB, 1, SRC) TO G-SKIPPED(SUB)
           END-PERFORM.
           MOVE 2                               TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "CLASS 5: " TO G-CLASS-PROMPT.
           MOVE GTT-NOLOANS(GTT-TOTAL-SUB, 2, SEL-SRC) TO G-LOANS.
           COMPUTE TOT-ELAPSED5 ROUNDED =
              (GTT-ELAPSED-MO(GTT-TOTAL-SUB, 2)) +
              (GTT-ELAPSED-REM(GTT-TOTAL-SUB, 2) / 30). 
           MOVE TOT-ELAPSED5 TO G-ELAPSED.
           COMPUTE WORK-AVG ROUNDED =
              TOT-ELAPSED5 /
              GTT-NOLOANS(GTT-TOTAL-SUB, 2, SEL-SRC).
           MOVE WORK-AVG TO G-AVG.
           MOVE 0 TO SRC.
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 7
              ADD 1 TO SRC
              IF SRC = SEL-SRC
                 ADD 1 TO SRC
              END-IF
              MOVE GTT-NOLOANS(GTT-TOTAL-SUB, 2, SRC) TO G-SKIPPED(SUB)
           END-PERFORM.
           MOVE 1                               TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "TOTAL  : " TO G-CLASS-PROMPT.
           MOVE GTT-NOLOANS(GTT-TOTAL-SUB, 3, SEL-SRC) TO G-LOANS.
      *    COMPUTE TOT-ELAPSED ROUNDED =
      *       (GTT-ELAPSED-MO(GTT-TOTAL-SUB, 3)) +
      *       (GTT-ELAPSED-REM(GTT-TOTAL-SUB, 3) / 30). 
           COMPUTE TOT-ELAPSED ROUNDED =
              TOT-ELAPSED1 + TOT-ELAPSED5.
           MOVE TOT-ELAPSED TO G-ELAPSED.
           COMPUTE WORK-AVG ROUNDED =
              TOT-ELAPSED /
              GTT-NOLOANS(GTT-TOTAL-SUB, 3, SEL-SRC).
           MOVE WORK-AVG TO G-AVG.
           MOVE 0 TO SRC.
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 7
              ADD 1 TO SRC
              IF SRC = SEL-SRC
                 ADD 1 TO SRC
              END-IF
              MOVE GTT-NOLOANS(GTT-TOTAL-SUB, 3, SRC) TO G-SKIPPED(SUB)
           END-PERFORM.
           MOVE 1                               TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           IF ( WS-HOLD-PROMPT = "GROUP :" )
              MOVE ALL "-" TO DETAIL-LINE
              PERFORM WRITE-DETAIL-LINE.

           MOVE LOW-VALUES TO WS-GTT-TABLE-GROUP-LEVEL(GTT-TOTAL-SUB).

       EXIT-GTT-PRINT-TOTALS.
           EXIT.

      ******************************************************************
       WRITE-DETAIL-LINE SECTION.
      ******************************************************************
           ADD LN-FEED LN-COUNT GIVING LN-WK.
           IF LN-WK GREATER THAN EXT-CONAME-RPT-LINES
              PERFORM HEAD-PAGE.
           MOVE DETAIL-LINE TO PR-REC.
           PERFORM WRITE-PR-REC.
           ADD LN-FEED TO LN-COUNT.
           MOVE 1 TO LN-FEED.
           MOVE SPACES TO DETAIL-LINE.
 

      ******************************************************************
       HEAD-PAGE SECTION.
      ******************************************************************
           ADD 1 TO PAGE-NUMBER.
           MOVE PAGE-NUMBER TO H-PAGE-NO.
           PERFORM GET-TIME.
           MOVE T-HH TO H-HR.
           MOVE T-MM TO H-MIN.
           MOVE HEAD1 TO PR-REC.
           MOVE 99 TO LN-FEED.
           PERFORM WRITE-PR-REC.
           MOVE HEAD2 TO PR-REC.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-PR-REC.
           IF GTT-HEADER-FG NOT = "Y"
              MOVE 2 TO LN-FEED
              MOVE HEAD3 TO PR-REC
              PERFORM WRITE-PR-REC
           ELSE
              MOVE 2 TO LN-FEED
              MOVE BEG-DATE TO DATE-YYYYMMDD
              PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
              MOVE DATE-MMDDYY TO TOT-BEG-DATE
              MOVE END-DATE TO DATE-YYYYMMDD
              PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
              MOVE DATE-MMDDYY TO TOT-END-DATE
              MOVE TOTAL-HEAD1 TO PR-REC
              PERFORM WRITE-PR-REC
              MOVE 1 TO LN-FEED
              MOVE TOTAL-HEAD2 TO PR-REC
              PERFORM WRITE-PR-REC.
           MOVE 4 TO LN-COUNT.
           MOVE 2 TO LN-FEED.
       END-HEAD-PAGE.
           EXIT.
 
      ******************************************************************
       PRINT-RANGE-LINES SECTION.
      ******************************************************************
           MOVE 4 TO LN-FEED.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF GROUP-FLAG = "Y"
              MOVE "BRANCH GROUP" TO RANGE-LINE
              MOVE ENT-GROUP TO RANGE-SELECTION
              PERFORM WRITE-DETAIL-LINE
           ELSE
              MOVE "BEG BRANCH" TO RANGE-LINE
              MOVE BEG-BRANCH TO R-NUM
              PERFORM WRITE-DETAIL-LINE
              MOVE "END BRANCH" TO RANGE-LINE
              MOVE END-BRANCH TO R-NUM
              PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "GROUP TOTALS" TO RANGE-LINE.
           MOVE GTT-ISO TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.

           MOVE 2 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.


       COPY "LIBLP/LPSTAT.CPY".
       COPY "LIBLP/LPMDTE.CPY".
       COPY "LIBLP/LPAMTS.CPY".
       COPY "LIBGB/TIMEIO.CPY".
       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/GRWORK.CPY".
       COPY "LIBGB/GRSERCH.CPY".
       COPY "LIBGB/GRTOTALS.CPY".
       COPY "LIBWI/GRTOTL.CPY".
       COPY "LIBGB/BRSECURE.CPY".

      ****************************************************
       FORM-RESET SECTION.
      ****************************************************
           MOVE EXT-CONAME-CONAME TO LNTIM6-CONAME.
           MOVE EXT-CONAME-SYSDATE TO LNTIM6-SYSDATE.
           DISPLAY LNTIM6-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE LNTIM6-FORM-FIELDS TO WR-BUF.
           MOVE LNTIM6-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      ****************************************************
       SETUP-LINK-INFO SECTION.
      ****************************************************
           MOVE LNTIM6-HELP--FILE TO HELP-LINK-FILE.
           MOVE LNTIM6-HELP--DIR  TO HELP-LINK-DIR.

      ****************************************************
       VDU-CONVERT-FIELD SECTION.
      ****************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBLP/LNTIM6_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ******************************************************************
       MISC-PARA SECTION.
      ******************************************************************
       COPY "LIBGB/GETPR.CPY".
       COPY "LIBGB/RMFILE.CPY".
       COPY "LIBGB/SYSTEM.CPY".
       COPY "LIBGB/MKTEMP.CPY".
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/DACCESS.CPY".
       COPY "LIBGB/GETENV.CPY".

       SEND-LEGEND.
           MOVE LEGEND TO WR-BUF.
           MOVE MSEL-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.
 
      ******************************************************
       IO-ROUTINES SECTION.
      ******************************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBLP/LPSPGS_SQL.CPY".
       COPY "LIBLP/LPLPGS_SQL.CPY".
       COPY "LIBLP/LPLNGS_SQL.CPY".
       COPY "LIBLP/LPLMGS_SQL.CPY".
       COPY "LIBLP/LPBWGS_SQL.CPY".
       COPY "LIBGB/GBGRGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBPRINT.CPY".
       COPY "LIBLP/LPBW1RN.CPY".
       COPY "LIBLP/LPSP1RN.CPY".
       COPY "LIBLP/LPLN3RN.CPY".
       COPY "LIBLP/LPLN4RN.CPY".
       COPY "LIBLP/LPLN5RN.CPY".
      *COPY "LIBLP/LPLN6RN.CPY".
      *COPY "LIBLP/LPLN7RN.CPY".
       COPY "LIBLP/LPLN1RN.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBGB/GBGR1RN.CPY".
       COPY "LIBGB/GBWKIN.CPY".
       COPY "LIBLP/LPLP1RN.CPY".
       COPY "LIBLP/LPLM1RN.CPY".
      **************************************************************
      *   ARCHIVED LNFILE IO
      **************************************************************
      *OPEN-ARCH-LN1-FILE.
      *    PERFORM OPEN-IT.
      *    MOVE LN-ARCHIVE-PATH TO E-FILE.
      *    OPEN INPUT ARCH-LN-FILE.
      *    IF IO-FG = 8
      *      GO TO OPEN-ARCH-LN1-FILE.
      *    IF IO-FG = 7
      *      CLOSE ARCH-LN-FILE
      *      GO TO OPEN-ARCH-LN1-FILE.
      *    UNLOCK ARCH-LN-FILE.
      *READ-ARCH-LN1-FILE.
      *    PERFORM READ-IT.
      *    MOVE LN-ARCHIVE-PATH TO E-FILE.
      *    MOVE ARCH-LN1-KEY TO E-KEYX.
      *    READ ARCH-LN-FILE.
      *    IF IO-FG = 8
      *      GO TO READ-ARCH-LN1-FILE.
      *CLOSE-ARCH-LN1-FILE.
      *    PERFORM CLOSE-IT.
      *    CLOSE ARCH-LN-FILE.
      **************************************************************
       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO EXT-EOBUF-ERRCD.
 
      ******************************************************************
       END-PROGRAM SECTION.
      ******************************************************************
       END-RUN.
           PERFORM CLOSE-FILES.

           MOVE MKTEMP-BUF TO ACCESS-BUF.
           PERFORM REMOVE-WORKFILE.

           IF ( SKIP-RECORDS-FOUND )
              GO TO EXIT-PROG.

           IF EXT-ROUTE-BUF = "00"
              PERFORM PRU-OUT.

           IF NO-RECORDS-FOUND
              MOVE "NO RECORDS FOUND FOR SELECTED RANGE" TO MESS
              PERFORM SEND-MESS.

       EXIT-PROG.

           IF SV-BEG-BR NOT = 9999
              MOVE SV-BEG-BR TO BEG-BRANCH
              MOVE SV-END-BR TO END-BRANCH.
           IF EXT-ROUTE-BUF = "00"
              MOVE EXITPROG TO FORM-NAM
              MOVE FORM-PATH TO FORM-PATHNAME.
       STOP-RUN.
           IF EXT-ROUTE-BUF = "00" OR "40" OR "50"
              DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN
              DESTROY LNTIM6-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.
           EXIT PROGRAM.
