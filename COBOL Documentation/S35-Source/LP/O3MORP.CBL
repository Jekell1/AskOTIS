       IDENTIFICATION DIVISION.
       PROGRAM-ID.      O3MORP.
       DATE-WRITTEN.    071095.
      ******************************************************************
      *    MONTHLY OTHER 3 INSURANCE
      *        PREMIUM & REFUND REPORT
      *
      *    SITUATIONS REQUIRING REPORTING:
      *
      *    NEW PREMIUMS:
      *        1. NEW LOANS IN THE TRFILE FOR (MMYY)
      *                             TR1-CODE = "L" OR "S"
      *                             TRL-INSOURS(6) = "Y"
      *                             TRS-INSOURS(6) = "Y"
      *
      *    REBATES OR CANCELLATIONS:
      *        1. REBATES/CANCELLATIONS/ADDONS IN THE TRFILE FOR (MMYY)
      *                             TR1-CODE = "P"
      *                             TP-INSOURS = "Y"
      *                             TP-TRCD = "R3" OR "C3"
      *                             TP-ADDON-TYPE = "O3"
      * REV:
      *  090695 BAH WAS NOT GETTING IC FILE FROM EACH FILE SET
      *  092995 BAH PRINT O3INS-DLPROC ON DETAIL LINE, AND CHANGED
      *             "CANCEL" TOTALS TO BE "DLR COMM" TOTALS.
      *  100695 JFF ADDED CLASS SCAN
      *  011696 KEC ADDED A DATE RANGE TO HAVE A BREAKDOWN ON THE
      *             TRANSACTION FILE.
      *  121096 MJD CHANGED TO PERFORM LPSTAT-DISPLAY AFTER
      *             READING BWFILE.
      *  021297 BLV FIX PRINTING INS CO'S OUT OF RANGE
      *  060497 JTG ADDED TEST FOR "R3" & "C3"
      *  BLV 060597 CHANGED FOR LOCAL SPOOL WITH NEW BATCH DRIVER - BREXEC
      *  GLF 090397 REMOVED ALL USE OF GLOBAL
      *  030598 BAH PRINT CANCELS AND ADJUSTMENTS ON TOTAL PAGES AGAIN...
      *  031798 BAH TOTAL COVERAGE NOT CORRECT IF VOIDS..
      *  BLV 042998 WAS DISPLAYING BEG-IC & END-IC FOR BEG & ENDING CLASS
      *  GLF 082499 ADD ARCHIVE PATH INFO. FOR TR FILES NOT FOUND IN DATA
      *             SET FOR SELECTED MONTH AND YEAR
      *  BLV 990826 FIX ARCHIVE PATH LOGIC
      *  BAH 991102 INS-TERM-CALCULATION WAS USING 5 NOT 6
      *  BAH 991103 WK-QTY WAS NOT SIGNED, IF THE FIRST ACCOUNT WAS A VOID,
      *             THE WK-QTY WAS 1 INSTEAD OF -1, CAUSING BAD TOTALS
      *             KEVIN, REGACC
      *  BAH 000621 ADDED THE "AND" TO THIS "IF" IN KEY-DETAIL SECTION
      *             IF LN-ACCTNO = WORK-ACCTNO AND (LN-OWNBR = WORK-OWNBR)
      *             REGENCY, KY0413 LAST ACCT # 500037, AND KY0414 FIRST
      *             ACCT # 500037 AND THE KEY-DETAIL WAS ADDING THESE 2
      *             ACCTS INFO TOGETHER, PL# 287
      *  BAH 010319 ADDED BRANCH SECURITY PASSWORD TESTING (BRSECURE)
      *  010813 BAH ADDED TEST IN THE READ-TR LOOP TO ONLY INCLUDE OWNING
      *             BRANCH RECORDS, ANOTHER BRANCH PAYOFF WITH THE SAME
      *             ACCTNO WAS BEING INCLUDED ON BOTH BRANCHES REPORT ! AJ
      *             OWNING BRANCHES DID MATCH, IT WAS THE WRONG ACCOUNT !!
      *             ONLY FIELD TO TEST IS TP-OTHBR-FG "G" = OWNER "C" = POSTER
      *  BAH 011113 ADDED TP-INSCOMP FOR ADDON/REVERSE/ADDON WITH DIFFERENT
      *             INSURANCE COMPANIES, REGACC PR# 1617
      *  BAH 020501 BUG FROM 010809 CHANGE, TESTED TP-OTHBR ON THE "L" AND "S"
      *             INSTEAD OF JUST "P" RECORDS, ACTUALLY CAUGHT A "C"
      *             IN THE TL-EFFDATE(3) FIELD FOR 04/30/02 STORED AS
      *             00 43 00 2F !! REGWAN PL# 365
      *  BAH 020722 TEST TP-INSOURS INSTEAD OF THE LN-INSOURS(1 OR 2), ON A
      *             REVERSAL OF AN ADDON, THE LOAN RECORD INSURANCE IS
      *             BLANK (KEVIN)!!
      *  BAH 030714 ADDED SUMMARY-ONLY OPTION OF "I" FOR INSURANCE TOTALS
      *              ONLY, NO BRANCH OR STATE, LENDMARK PR# 2113
      *  BAH 040123 WORLD "KY" INSURANCE TAX WAS PART OF THE REFUND
      *             BEING REPORTED, NEEDED TO BE SUBTRACTED OUT !!
      *                            ((TP-TRAMT - TP-INS-TAX-RB) * -1)
      *  BAH 041118 CHANGED TOTAL INSURANCE PAYABLE TO BE;
      *            (NEW PREM + ADDONS)  -  (ADJ + CANCEL + REF + DLCOMM
      *                                         + NET COMM). CORNER PL# 452
      *            THIS IS WHAT GOES TO GENERAL LEDGER, CINDY TESTED !
      *   CS 050224 ADDED NET NEW PREMIUMS = (NEW PREM + ADDONS) +
      *             (REFUNDS + CANCELS) + ADJUSTMENTS [WORLD PR#385]
      *  BAH 141015 STACK OVERFLOW, ABORT, SORT-TR HAD A GO TO OUTSIDE
      *             OF ITS OWN SECTION IF TRFILE DID NOT EXIST!
      *  BAH 161017 CHANGE COMMISSION HEADINGS TO PROCESS FEE FOR MAS
      *             ADDED HEAD5-MAS, HEAD6-MAS AND HEAD8-MAS, MAS #5186
      *  BAH 170707 ACTIVATE 8 DIGIT DATES, PD0006
      *  BAH 181211 GLOBAL LNFILE
      *  BAH 20190922 REMOVED ACCESS-CALL LNFILE, TRP, TRL TRS
      *  BAH 20200827 REMOVED RESET ON FIRST FIELD
      *  BLM 20210810 REMOVE TP-CODE, TP-POOLID NO LONGER IN KEY
      *  RMZ 20230807 PERFORMANCE TUNING - ADDED STORED PROCEDURE
      *  RMZ 20240116 CHANGED JULIAN-BEG(END,CC) TO EXT-JULIAN-BEG(END,CC)
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "LIBGB/GBFSPR.CPY".

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      * VALID LOANS WORK FILE
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       COPY "LIBGB/GBFSWKI.CPY".

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      * TOTALS FILE
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           SELECT WK1-FILE ASSIGN WK1-PATH
                  ORGANIZATION INDEXED
                  ACCESS DYNAMIC
                  LOCK MODE AUTOMATIC WITH LOCK ON RECORD
                  RECORD KEY WK1-KEY
                  FILE STATUS FILE-STAT.
       I-O-CONTROL.
       DATA DIVISION.
       FILE SECTION.

       COPY "LIBGB/GBFDPR.CPY".

      ******************************************************************
      *   SUMMARY TOTALS WORK RECORD
      ******************************************************************
       FD  WK1-FILE
           LABEL RECORDS ARE STANDARD.
       01  WK1-REC.
           03  WK1-KEY.
               05  WK-CODE     PIC 9.
               05  WK-IC       PIC 99.
               05  WK-ST.
                   07  WK-BRNO PIC 9(4).
               05  WK-CLASS    PIC 9(3).

           03  WK-QTY        PIC S9(7)    COMP-3 OCCURS 2.
           03  WK-AMT        PIC S9(9)V99 COMP-3 OCCURS 8.

      ******************************************************************
      *   WORK FILE FOR VALID LOANS
      ******************************************************************
       FD  WK-FILE
           LABEL RECORDS ARE STANDARD.
       01  WK-REC.
           03  WK-KEY.
               05  WORK-INSCOMP     PIC 99.
               05  WORK-OWNBR       PIC 9(4).
               05  WORK-CLASS       PIC 9(3).
               05  WORK-ACCTNO      PIC 9(8).
               05  WORK-DATE        PIC 9(8).
           03  WORK-NEW-O3      PIC S9(5)V99 COMP-3.
           03  WORK-ADJ-O3      PIC S9(5)V99 COMP-3.
           03  WORK-CAN-O3      PIC S9(5)V99 COMP-3.
           03  WORK-CAN-O3-DATE PIC 9(8)     COMP-3.
           03  WORK-COM-O3      PIC S9(5)V99 COMP-3.
           03  WORK-DLR-COM-O3  PIC S9(5)V99 COMP-3.
           03  WORK-ADDON-FG    PIC X.
           03  WORK-CANCEL-FG   PIC X.
      ******************************************************************
      *
      *    W O R K I N G - S T O R A G E    S E C T I O N
      *
      ******************************************************************
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)LP/O3MORP.CBL S35 12/22/23-11:38:35 >".

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
       COPY "LIBLP/LP01TRP.CPY".
       COPY "LIBLP/LP01TRP_SQL.CPY".
       COPY "LIBLP/LPWSTRP.CPY".
       COPY "LIBLP/LP01TRL.CPY".
       COPY "LIBLP/LP01TRL_SQL.CPY".
       COPY "LIBLP/LPWSTRL.CPY".
       COPY "LIBLP/LP01TRS.CPY".
       COPY "LIBLP/LP01TRS_SQL.CPY".
       COPY "LIBLP/LPWSTRS.CPY".
       COPY "LIBLP/LP01LN.CPY".
       COPY "LIBLP/LP01LN_SQL.CPY".
       COPY "LIBLP/LPWSLN.CPY".
       COPY "LIBLP/LP01IC.CPY".
       COPY "LIBLP/LP01IC_SQL.CPY".
       COPY "LIBLP/LPWSIC.CPY".
       COPY "LIBLP/LP01BW.CPY".
       COPY "LIBLP/LP01BW_SQL.CPY".
       COPY "LIBLP/LPWSBW.CPY".
       COPY "LIBLP/LP01SP.CPY".
       COPY "LIBLP/LP01SPA.CPY".
       COPY "LIBLP/LP01SPB.CPY".
       COPY "LIBLP/LP01SPC.CPY".
       COPY "LIBLP/LP01SP_SQL.CPY".
       COPY "LIBLP/LPWSSP.CPY".
      *----------------------------------------------------------------

       EXEC SQL BEGIN DECLARE SECTION END-EXEC.            
       01  QWS-BEG-IC           PIC 99.            
       01  QWS-END-IC           PIC 99.
       01  QWS-BEG-ST           PIC X(2).
       01  QWS-END-ST           PIC X(2).
       EXEC SQL END DECLARE SECTION END-EXEC.

       01  WK1-IFN              PIC X(3)     VALUE "WK1".
       01  WK-IFN               PIC X(2)     VALUE "WK".
       01  WK1-PATH             PIC X(35).

       01  TRP1-CONN-OPEN-SW    PIC X        VALUE "N".

      ******************************************************************
      *   LINE WORKERS
      ******************************************************************
       01  LN-WORKERS.
           03  LN-FEED          PIC 999      VALUE 0.
           03  LN-COUNT         PIC 999      VALUE 99.
           03  LN-WK            PIC 999      VALUE 0.
       01  PAGE-NUMBER          PIC 9(4)     VALUE 0.

       01  BR-PAGE-NUMBER       PIC 9(4) COMP-3 VALUE 0.

      ******************************************************************
      *   MISC WORKERS
      ******************************************************************
       01  SUB                  PIC 9(2)     COMP-3.
       01  FIRST-TIME           PIC X        VALUE "Y".
       01  RECORDS-FOUND        PIC X        VALUE " ".
           88  NO-RECORDS-FOUND              VALUE "N".
       01  MESS-LIST            PIC X(5)     VALUE "MSEL.".
       01  HOLD-IC              PIC 99       COMP-3 VALUE 0.
       01  HOLD-BR              PIC 9(4)     COMP-3 VALUE 0.
       01  HOLD-LN-BR           PIC 9(4)     COMP-3 VALUE 9999.
       01  SAVE-WK1-KEY         PIC X(10).
       01  SAVE-CODE            PIC 9.
       01  SAVE-ST.
           03  SAVE-BR          PIC 9(4).
       01  LAST-ST              PIC XX.
       01  FIRST-TOT            PIC X.
       01  DASH-LINE            PIC X(132)   VALUE ALL "-".

       01  INS-ACTIVITY         PIC X        VALUE "N".
       01  REC-EXISTS           PIC X        VALUE "N".
       01  TOTAL-FG             PIC X        VALUE "N".
       01  COMPANY-FG           PIC X        VALUE "N".

       01  ONE-O3               PIC S9       VALUE 0.
       01  NEW-COVERAGE         PIC S9(7)V99 COMP-3 VALUE 0.

       01  O3-ADDON-FG          PIC X.
       01  O3-CANCEL-FG         PIC X.
       01  O3-PREM              PIC S9(7)V99 COMP-3.
       01  O3-ADJ               PIC S9(7)V99 COMP-3.
       01  O3-CAN               PIC S9(7)V99 COMP-3.
       01  O3-CAN-DATE          PIC 9(8)     COMP-3.
       01  O3-COMM              PIC S9(7)V99 COMP-3.
       01  O3-DLR-COMM          PIC S9(7)V99 COMP-3.

       01  LINE-TOTALS.
           03  TOT-QTY          PIC S9(7)    COMP-3 OCCURS 2.
           03  TOT-AMT          PIC S9(9)V99 COMP-3 OCCURS 8.
       01  CLASS-FG             PIC X(3)           VALUE "   ".
           88  CLASS-BEG                           VALUE "BEG".
           88  CLASS-END                           VALUE "END".

       01  SCREEN-MESSAGES.
           03  ERROR-MONTH-MESS     PIC X(47)      VALUE
               "ENDING MONTH NOT EQUAL BEGINNING MONTH".
           03  ERROR-YEAR-MESS      PIC X(45)      VALUE
               "ENDING YEAR NOT EQUAL BEGINNING YEAR".
           03  ERROR-DATE-MESS      PIC X(37)      VALUE
               "DATE RANGE MUST BE IN ASCENDING ORDER".

      ******************************************************************
      *
      *     S C R E E N   F I E L D S
      *
      ******************************************************************
       COPY "LIBGB/EOBUF_EXT.CPY".
      *-----------------------------------------------------------------
           03  BEG-BRANCH            PIC 9(4).
           03  END-BRANCH            PIC 9(4).
           03  BEG-IC                PIC 99.
           03  END-IC                PIC 99.
           03  BEG-CLASS             PIC 9(3).
           03  END-CLASS             PIC 9(3).
           03  BEG-ST                PIC XX.
           03  END-ST                PIC XX.
           03  BEG-DATE              PIC 9(8).
           03  FILLER                REDEFINES BEG-DATE.
               05  BEG-DATE-CC       PIC 9(2).
               05  BEG-DATE-YY       PIC 9(2).
               05  BEG-DATE-MM       PIC 9(2).
               05  BEG-DATE-DD       PIC 9(2).
           03  END-DATE              PIC 9(8).
           03  FILLER                REDEFINES END-DATE.
               05  END-DATE-CC       PIC 9(2).
               05  END-DATE-YY       PIC 9(2).
               05  END-DATE-MM       PIC 9(2).
               05  END-DATE-DD       PIC 9(2).
           03  FILLER                REDEFINES END-DATE.
               05  END-DATE-MM-X     PIC X(2).
               05  END-DATE-DD-X     PIC X(2).
               05  END-DATE-YY-X     PIC X(2).
           03  SUMMARY-ONLY          PIC X.
           03  GROUP-FLAG            PIC X.
           03  ENT-GROUP             PIC X(4).
           03  KEY-PATH              PIC X(20).

       01  VERSION-CONTROL     PIC X    VALUE "B".

       01  SV-BEG-BR           PIC 9(4)    VALUE 9999.
       01  SV-END-BR           PIC 9(4)    VALUE 9999.
       01  SV-GROUP-FLAG       PIC X     VALUE SPACES.
       01  SV-ENT-GROUP        PIC X(4)  VALUE SPACES.

      ******************************************************************
      *
      *     P R I N T   B U F F E R S
      *
      ******************************************************************
       01  HEAD1.
           03  H-DATE           PIC X(8)     VALUE " ".
           03  FILLER           PIC XX       VALUE " ".
           03  H-HR             PIC 99       VALUE 0.
           03  FILLER           PIC X        VALUE ":".
           03  H-MIN            PIC 99       VALUE 0.
           03  FILLER           PIC X(31)    VALUE " ".
           03  H-NAME           PIC X(40)    VALUE " ".
           03  FILLER           PIC X(29)    VALUE " ".
           03  FILLER           PIC X(8)     VALUE "COMPANY ".
           03  FILLER           PIC X(5)     VALUE "PAGE:".
           03  H-PAGE-NO        PIC ZZZ9.

      *----------------------------------------------------------------
       01  HEAD2.
           03  FILLER           PIC X(10)    VALUE "USER ID:".
           03  H-USERID         PIC X(10) VALUE " ".
           03  FILLER           PIC X(26)    VALUE " ".
           03  FILLER           PIC X(41) VALUE
           "-OTHER 3 INSURANCE PREMIUM-REFUND REPORT-".

      *----------------------------------------------------------------
       01  HEAD3.
           03  H-INSCO          PIC X(10) VALUE "INS CO :".
           03  H-ICNO           PIC 99 VALUE 0.
           03  FILLER           PIC X VALUE " ".
           03  H-ICNAME         PIC X(20) VALUE " ".
           03  FILLER           PIC X(20) VALUE " ".
      *    03  FILLER           PIC X(13) VALUE "FOR MONTH OF ".
           03  FILLER           PIC X(4) VALUE "FOR".
           03  H-BEG-DATE       PIC 99/99/99.
           03  FILLER           PIC X(3) VALUE " - ".
           03  H-END-DATE       PIC 99/99/99.

      *----------------------------------------------------------------
       01  HEAD4                VALUE " ".
           03  H-PAGE-DESC      PIC X(46).
           03  H-STNO.
               05  H-BRNO       PIC Z(4).
           03  FILLER           PIC X.
           03  H-BRNAME         PIC X(20).
           03  FILLER           PIC X(45).
           03  H-PAGE-PROMPT    PIC X(12).
           03  H-BR-PAGE        PIC ZZZZ.

      *----------------------------------------------------------------
       01  HEAD5.
           03  FILLER           PIC X(51) VALUE
           "ACCOUNT #      BORROWER           AGE EFF-DATE TRM ".
           03  FILLER           PIC X(53) VALUE
           "EXP-DATE  COVERAGE    BENEFIT    PREMIUM  ADJUSTMNT  ".
           03  FILLER           PIC X(27) VALUE
           "  REFUND----DATE  COMMISSON".
       01  HEAD5-MAS.
           03  FILLER           PIC X(51) VALUE
           "ACCOUNT #      BORROWER           AGE EFF-DATE TRM ".
           03  FILLER           PIC X(53) VALUE
           "EXP-DATE  COVERAGE    BENEFIT    PREMIUM  ADJUSTMNT  ".
           03  FILLER           PIC X(27) VALUE
           "  REFUND----DATE   PROCESS ".
       01  HEAD6.
           03  FILLER           PIC X(51) VALUE SPACES.
           03  FILLER           PIC X(53) VALUE SPACES.
           03  FILLER           PIC X(27) VALUE
           "                  DLR COMSN".
       01  HEAD6-MAS.
           03  FILLER           PIC X(51) VALUE SPACES.
           03  FILLER           PIC X(53) VALUE SPACES.
           03  FILLER           PIC X(27) VALUE
           "                     FEE   ".
      *----------------------------------------------------------------
       01  HEAD7.
           03  FILLER           PIC X(48) VALUE
           "TOT  **NEW ACCOUNTS**              LOAN         ".
           03  FILLER           PIC X(57) VALUE
           "NEW LOAN         ADDON       ADJSTMNTS".
           03  FILLER           PIC X(26) VALUE
           "INS PAYABLE       NET PREM".

      *----------------------------------------------------------------
       01  HEAD8.
           03  FILLER           PIC X(48) VALUE
           "CLS                              COVERAGE".
           03  FILLER           PIC X(57) VALUE
           "PREMIUMS       PREMIUMS        CANCELS        REFUNDS".
           03  FILLER           PIC X(26) VALUE
           "DEALER COMM       NET COMM".
       01  HEAD8-MAS.
           03  FILLER           PIC X(48) VALUE
           "CLS                              COVERAGE".
           03  FILLER           PIC X(57) VALUE
           "PREMIUMS       PREMIUMS        CANCELS        REFUNDS".
           03  FILLER           PIC X(26) VALUE
           "PROCESS FEE       NET PFEE".

      *
      *    D E T A I L - L I N E
      *
      ******************************************************************
       01  DETAIL-LINE          PIC X(132) VALUE SPACES.

      *----------------------------------------------------------------
       01  DETAIL1 REDEFINES DETAIL-LINE.
           03  D-LOAN-NUM       PIC X(14).
           03  D-NUM REDEFINES D-LOAN-NUM.
               05  D-CLASS      PIC 99.
               05  FILLER       PIC X(12).
           03  FILLER           PIC X.
           03  D-NAME           PIC X(18).
           03  FILLER           PIC X.
           03  D-AGE            PIC ZZ9.
           03  FILLER           PIC X.
           03  D-EFFDATE        PIC 99/99/99   BLANK ZERO.
           03  FILLER           PIC X.
           03  D-TERM           PIC ZZ9        BLANK ZERO.
           03  FILLER           PIC X.
           03  D-EXPDATE        PIC 99/99/99   BLANK ZERO.
           03  FILLER           PIC X.
           03  D-COVRG          PIC Z(5)9.99-  BLANK ZERO.
           03  FILLER           PIC X.
           03  D-REGPYAMT       PIC Z(5)9.99-  BLANK ZERO.
           03  FILLER           PIC X.
           03  D-INSPREM        PIC Z(5)9.99-   BLANK ZERO.
           03  D-ADDTEXT        PIC X.
           03  D-INSADJ         PIC Z(5).99-   BLANK ZERO.
           03  FILLER           PIC X.
           03  D-INSREFUND      PIC Z(5).99-   BLANK ZERO.
           03  D-CNLTEXT        PIC X.
           03  FILLER           PIC X.
           03  D-REFUND-DATE    PIC 99/99/99   BLANK ZERO.
           03  FILLER           PIC X.
           03  D-INSCOMM        PIC Z(5).99-   BLANK ZERO.

      *----------------------------------------------------------------
       01  DETAIL2 REDEFINES DETAIL-LINE.
           03  FILLER           PIC X(9).
           03  D-QTY            PIC Z(5)9-.
           03  FILLER           PIC X(11).
           03  D-AMT            PIC ZZZ,ZZZ,ZZ9.99- OCCURS 7.

      *----------------------------------------------------------------
       01  RANGE-LINE REDEFINES DETAIL-LINE.
           03  FILLER           PIC X(20).
           03  RANGE-SEL        PIC X(112).

      ******************************************************************
      *
      *     C O P Y    L I B R A R Y    W O R K E R S
      *
      ******************************************************************
       COPY "LIBWI/GRSCANW.CPY".
       COPY "LIBLP/LPSTATW.CPY".
       COPY "LIBWI/LCSCANW.CPY".
       COPY "LIBLP/LPMDTEW.CPY".
       COPY "LIBLP/LPITRMW.CPY".
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
       COPY "LIBLP/O3MORP_DEF.CPY".
       COPY "LIBLP/O3MORP_WKS.CPY".
       COPY "LIBLP/ARRAYSPW.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

      ******************************************************************
      *
      *     P R O G R A M   C O N T R O L
      *
      ******************************************************************
       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBLP/O3MORP_SCN.CPY".

       PROCEDURE DIVISION
             USING FORM-PATHNAME EXIT-PATHNAME.
       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
               PR-FILE WK1-FILE WK-FILE.
       COPY "LIBGB/DECLRP.CPY".
           PERFORM CLOSE-TRS1-FILE.
           PERFORM CLOSE-TRP1-FILE.
           PERFORM CLOSE-TRP1-CONNECTION.
           PERFORM CLOSE-TRL1-FILE.
           PERFORM CLOSE-SP1-FILE.
           PERFORM CLOSE-LN1-FILE.
           PERFORM CLOSE-IC1-FILE.
           PERFORM CLOSE-BW1-FILE.
           PERFORM CLOSE-GR1-FILE.
           PERFORM CLOSE-BR-FILE.
           PERFORM CLOSE-PR-FILE.
           CLOSE WK1-FILE WK-FILE.
       COPY "LIBGB/DECLRP2.CPY".

      ******************************************************************
      *
      *     M A I N    M O D U L E
      *
      ******************************************************************
       MAIN-MODULE SECTION.

           PERFORM SQL-CONNECT.

           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "O3MORP" TO VDU-FORM-NAME.

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
              GO TO PROCESS-REPORT.

           MOVE WGR-SRESULT TO BR-NO.
           PERFORM READ-BR-FILE.
           IF IO-FG NOT = 0
              GO TO IO-ERROR.

           GO TO MOVE-BRANCH-PATH-INFO.

      *-----------------------------------------------------------------
       NEXT-BRANCH-NO.
           PERFORM READ-BR-FILE-NEXT.
           IF IO-FG = 9 OR BR-NO > END-BRANCH
              GO TO PROCESS-REPORT.

      *-----------------------------------------------------------------
       MOVE-BRANCH-PATH-INFO.

           IF EXT-ROUTE-BUF = "00" 
              MOVE SPACES TO WR-BUF 
              STRING "INSURANCE REPORT IN PROGRESS.......", BR-NO 
                     DELIMITED "  " INTO WR-BUF 
              MOVE MESS-LIST TO WR-LIST 
              PERFORM CALL-WRFORM. 

           MOVE EXT-FILPATH-BASE           TO FILPATH-BASE.
           MOVE BR-MACHINE                 TO FILPATH-MACHINE.
           MOVE BR-DATA-PATH               TO FILPATH-DATA.

           MOVE "B"                        TO BW-PATH-OVERRIDE
                                              IC-PATH-OVERRIDE
                                              LN-PATH-OVERRIDE
                                              TRP-PATH-OVERRIDE
                                              TRL-PATH-OVERRIDE
                                              TRS-PATH-OVERRIDE.

           PERFORM OPEN-LN1-FILE.

           PERFORM SORT-TRP-FILE.
           PERFORM SORT-TRL-FILE.
           PERFORM SORT-TRS-FILE.
                                                                                
       END-OF-GROUP.
           PERFORM CLOSE-LN1-FILE.

           GO TO NEXT-GROUP-LOC.

       PROCESS-REPORT.
           PERFORM CLOSE-WK-FILE.
           PERFORM OPEN-WK-FILE.

           PERFORM RUN-REPORT.

      *-----------------------------------------------------------------
       END-ROUTINE.
           IF FIRST-TIME = "Y"
              MOVE "N" TO RECORDS-FOUND
           ELSE
              MOVE "Y" TO COMPANY-FG
              PERFORM GB-TOTALS
              PERFORM RANGE-LINES.

           GO TO END-PROGRAM.

      ******************************************************************
       INITIALIZATION SECTION.
      ******************************************************************
           MOVE O3MORP-QUEUE-POS TO Q-POS.
           MOVE O3MORP-COPIES-POS TO C-POS.
           MOVE O3MORP-PRU-POS   TO P-POS.
           MOVE SPACES TO Q-BUF C-BUF P-BUF.

       COPY "LIBGB/EOINIT.CPY".

      * CREATE TOTALS WORK FILE
           MOVE MKTEMP-DEFAULT TO MKTEMP-BUF.
           PERFORM MKTEMP-CALL.
           MOVE MKTEMP-BUF TO WK1-PATH.
           PERFORM OPEN-WK1-FILE-OUTPUT.
           PERFORM CLOSE-WK1-FILE.

      * CREATE WORK FILE FOR VALID LOANS
           MOVE MKTEMP-DEFAULT TO MKTEMP-BUF.
           PERFORM MKTEMP-CALL.

           IF STAT NOT = "00"
              MOVE "CANNOT MAKE WORK FILE" TO MESS
              PERFORM SEND-MESS
              GO TO INITIALIZE-ENDIT.

           PERFORM OPEN-WK-FILE-OUTPUT.
           PERFORM CLOSE-WK-FILE.
           PERFORM OPEN-WK-FILE.

           IF EXT-ROUTE-BUF = "00"
              MOVE "INSURANCE REPORT IN PROGRESS......." TO MESS
              PERFORM SEND-LEGEND.

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

           PERFORM OPEN-WK1-FILE.
           PERFORM OPEN-BR-FILE.
           PERFORM OPEN-GR1-FILE.
      *    PERFORM OPEN-SP1-FILE.
           PERFORM ARRAYSP-LOAD.
           PERFORM OPEN-PR-FILE.

           MOVE EXT-CONAME-CONAME TO H-NAME.
           MOVE EXT-CONAME-SYSDATE TO H-DATE.
           MOVE EXT-CONAME-USER-ID TO H-USERID.

           MOVE BEG-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO H-BEG-DATE.
           MOVE END-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO H-END-DATE.

           PERFORM CLEAR-TOTALS.

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
           IF VDUSTATUS = "1U" GO TO SO-01.

      *-----------------------------------------------------------------
       BR-01.
           MOVE "E  A000BR1." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "16" GO TO ALL-BR.

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

      *-----------------------------------------------------------------
       CHECK-BRANCH-SECURITY.
           PERFORM BR-RANGE-SECURITY.

           IF BR-SECURITY-OKAY = "N"
              GO TO BR-01.

      *-----------------------------------------------------------------
       IC-01.
           IF ( (BEG-BRANCH = 0) AND (END-BRANCH = 0) )
              MOVE "S  A160DISPLAY-USER." TO VDU
              MOVE "USER BRANCH ONLY" TO VDUBUF
              PERFORM SCREENIO
           ELSE
              MOVE "S  A160DISPLAY-USER." TO VDU
              MOVE SPACES TO VDUBUF
              PERFORM SCREENIO.

           MOVE "ENNN020IC1." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "16" GO TO ALL-IC.
           IF VDUSTATUS = "1U" GO TO BR-02.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO IC-01.
           MOVE VDUNUM0 TO BEG-IC.

      *-----------------------------------------------------------------
       IC-02.
           MOVE "ENNN020IC2." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "16" GO TO ALL-IC.
           IF VDUSTATUS = "1U" GO TO IC-01.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO IC-02.
           MOVE VDUNUM0 TO END-IC.
           IF BEG-IC NOT > END-IC GO TO LC-01.
           PERFORM INVALID-RANGE.
           GO TO IC-01.

      *-----------------------------------------------------------------
       ALL-IC.
           MOVE "SNNN020IC1." TO VDU.
           MOVE 1 TO BEG-IC VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN020IC2." TO VDU.
           MOVE 99 TO END-IC VDUNUM0.
           PERFORM SCREENIO.

      *-----------------------------------------------------------------
       LC-01.
           MOVE "ENNN020LC1." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "16" GO TO ALL-LC.
           IF VDUSTATUS = "1U" GO TO IC-02.
           IF VDUSTATUS = "1<"
              MOVE "BEG" TO CLASS-FG
              PERFORM CLASS-SCAN
              IF IO-FG NOT = 0
              GO TO LC-01.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO IC-02.
           MOVE VDUNUM0 TO BEG-CLASS.

      *-----------------------------------------------------------------
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
           IF BEG-CLASS NOT > END-CLASS GO TO ST-01.
           PERFORM INVALID-RANGE.
           GO TO LC-01.

      *-----------------------------------------------------------------
       ALL-LC.
           MOVE "SNNN020LC1." TO VDU.
           MOVE 1 TO BEG-CLASS VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN020LC2." TO VDU.
           MOVE 99 TO END-CLASS VDUNUM0.
           PERFORM SCREENIO.

      *-----------------------------------------------------------------
       ST-01.
           MOVE "E  A020ST1." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "16" GO TO ALL-ST.
           IF VDUSTATUS = "1U" GO TO LC-02.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO ST-01.
           MOVE VDUBUF TO BEG-ST.

      *-----------------------------------------------------------------
       ST-02.
           MOVE "E  A020ST2." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "16" GO TO ALL-ST.
           IF VDUSTATUS = "1U" GO TO ST-01.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO ST-02.
           MOVE VDUBUF TO END-ST.
           IF BEG-ST NOT > END-ST GO TO DATE-01.
           PERFORM INVALID-RANGE.
           GO TO ST-01.

      *-----------------------------------------------------------------
       ALL-ST.
           MOVE "S  A020ST1." TO VDU.
           MOVE "  " TO BEG-ST VDUBUF.
           PERFORM SCREENIO.
           MOVE "S  A020ST2." TO VDU.
           MOVE ALL "Z" TO END-ST.
           CALL "C$TOLOWER" USING END-ST, VALUE 2.
           MOVE END-ST TO VDUBUF.
           PERFORM SCREENIO.

      *-----------------------------------------------------------------
       DATE-01.
           IF ( VDUSTATUS = "1U" ) AND ( EXT-ROUTE-BUF = "40" OR "50" )
              GO TO ST-02.

           IF ( EXT-ROUTE-BUF = "40" OR "50" )
              GO TO SO-01.

           MOVE "ERNM060BEG-DATE." TO VDU.
           PERFORM SCREENIO.
           IF ( VDUSTATUS = "1>" ) GO TO EXIT-PARM.
           IF ( VDUSTATUS = "10" ) GO TO EXIT-PARM.
           IF ( VDUSTATUS = "1U" ) GO TO ST-02.
           IF ( NOT (VDUSTATUS = "00" OR "1D") ) GO TO DATE-01.
           MOVE VDU-DATE-YYYYMMDD TO BEG-DATE.

           IF ( BEG-DATE = ZEROES )
              GO TO DATE-01.

      *-----------------------------------------------------------------
       DATE-02.
           IF ( VDUSTATUS = "1U" ) AND ( EXT-ROUTE-BUF = "40" OR "50" )
              GO TO ST-02.

           MOVE "ERNM060END-DATE." TO VDU.
           PERFORM SCREENIO.
           IF ( VDUSTATUS = "1>" ) GO TO EXIT-PARM.
           IF ( VDUSTATUS = "10" ) GO TO EXIT-PARM.
           IF ( VDUSTATUS = "1U" ) GO TO DATE-01.
           IF ( NOT (VDUSTATUS = "00" OR "1D") ) GO TO DATE-02.
           MOVE VDU-DATE-YYYYMMDD TO END-DATE.

           IF ( END-DATE = ZEROES )
              GO TO DATE-02.

           IF ( END-DATE-MM NOT = BEG-DATE-MM )
              MOVE ERROR-MONTH-MESS TO MESS
              PERFORM SEND-MESS
              GO TO DATE-02.

           IF ( END-DATE-YY NOT = BEG-DATE-YY )
              MOVE ERROR-YEAR-MESS TO MESS
              PERFORM SEND-MESS
              GO TO DATE-02.

           IF END-DATE < BEG-DATE
              MOVE ERROR-DATE-MESS TO MESS
              PERFORM SEND-MESS
              GO TO DATE-02.

      *-----------------------------------------------------------------
       SO-01.
           MOVE "E  A010SO1." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO DATE-02.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO SO-01.
           MOVE VDUBUF TO SUMMARY-ONLY.
           IF NOT (SUMMARY-ONLY = "Y" OR "N" OR "I") GO TO SO-01.

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
           MOVE "SNNN020IC1." TO VDU.
           MOVE BEG-IC TO VDUNUM0.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "SNNN020IC2." TO VDU.
           MOVE END-IC TO VDUNUM0.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "SNNN020LC1." TO VDU.
           MOVE BEG-CLASS TO VDUNUM0.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "SNNN020LC2." TO VDU.
           MOVE END-CLASS TO VDUNUM0.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "S  A020ST1." TO VDU.
           MOVE BEG-ST TO VDUBUF.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "S  A020ST2." TO VDU.
           MOVE END-ST TO VDUBUF.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "S  A010SO1." TO VDU.
           MOVE SUMMARY-ONLY TO VDUBUF.
           PERFORM SCREENIO.

      ******************************************************************
       RUN-REPORT SECTION.
      ******************************************************************

           MOVE 0 TO O3-PREM O3-ADJ O3-CAN O3-COMM O3-CAN-DATE
                     O3-DLR-COMM.
           MOVE SPACES TO O3-ADDON-FG O3-CANCEL-FG.

           PERFORM READ-WK-FILE-NEXT.
           IF IO-FG = 9
              GO TO END-RUN-REPORT.

           IF WORK-OWNBR NOT = HOLD-LN-BR
              MOVE WORK-OWNBR TO HOLD-LN-BR BR-NO
              PERFORM GET-LN-BW-PATH.

           MOVE WORK-OWNBR  TO LN-OWNBR.
           MOVE WORK-ACCTNO TO LN-ACCTNO.
           PERFORM READ-LN1-FILE. *> EMBEDDED - LNFILE
           IF IO-FG NOT = 0
              GO TO END-RUN-REPORT.

      *----------------------------------------------------------------
       KEY-FILE-LOOP.

           ADD WORK-NEW-O3 TO O3-PREM.

           IF WORK-NEW-O3 NOT = 0
              MOVE WORK-ADDON-FG TO O3-ADDON-FG.

           ADD WORK-ADJ-O3 TO O3-ADJ.
           ADD WORK-CAN-O3 TO O3-CAN.

           IF WORK-CAN-O3 NOT = 0
              MOVE WORK-CANCEL-FG TO O3-CANCEL-FG.

           ADD WORK-COM-O3 TO O3-COMM.
           ADD WORK-DLR-COM-O3 TO O3-DLR-COMM.

           IF WORK-CAN-O3-DATE NOT = 0
              MOVE WORK-CAN-O3-DATE TO O3-CAN-DATE.

           IF FIRST-TIME = "Y"
              MOVE "N" TO FIRST-TIME
              MOVE WORK-INSCOMP TO HOLD-IC
              MOVE LN-OWNBR TO HOLD-BR
      *------>MOVE WORK-OWNBR TO HOLD-BR
              PERFORM IC-HEADER
              PERFORM BR-HEADER.

           IF WORK-INSCOMP NOT = HOLD-IC
              PERFORM IC-TOTALS.

           IF LN-OWNBR NOT = HOLD-BR
              PERFORM BR-TOTALS.

           MOVE LN-ORGST    TO SP-ORGST.
           MOVE LN-SPRCLASS TO SP-SPRCLASS.
           MOVE LN-SUBCLASS TO SP-SUBCLASS.
           MOVE LN-LAWCODE  TO SP-LAWCODE.

           IF SP1-KEY NOT = LPWSSP-SP1-KEY
              MOVE SP1-KEY TO LPWSSP-SP1-KEY
      *       PERFORM READ-SP1-FILE
              PERFORM ARRAYSP-READ
              IF IO-FG = 9
                 GO TO IO-ERROR.

       NEXT-KEY-REPORT.
           PERFORM READ-WK-FILE-NEXT.
           IF IO-FG = 9
              MOVE 0 TO WORK-ACCTNO
              PERFORM KEY-DETAIL
              GO TO END-RUN-REPORT
           ELSE
              PERFORM KEY-DETAIL.

           IF WORK-OWNBR NOT = HOLD-LN-BR
              MOVE WORK-OWNBR TO HOLD-LN-BR BR-NO
              PERFORM GET-LN-BW-PATH.

           MOVE WORK-ACCTNO TO LN-ACCTNO.
           PERFORM READ-LN1-FILE. *> EMBEDDED - LNFILE
           IF IO-FG NOT = 0
              GO TO END-RUN-REPORT.

           GO TO KEY-FILE-LOOP.

       END-RUN-REPORT.
           EXIT.

      **********************************************
       SORT-TRP-FILE SECTION.
      **********************************************

           PERFORM OPEN-TRP1-FILE.


           MOVE BR-NO               TO TP-BRNO
                                       QTRP1-WBEG-BRNO
                                       QTRP1-WEND-BRNO.
           MOVE BEG-DATE            TO TP-DATE SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO QTRP1-WBEG-DATE.
           MOVE END-DATE            TO  SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO QTRP1-WEND-DATE.

           MOVE BEG-CLASS           TO TP-CLASS
                                       QTRP1-WBEG-CLASS.
           MOVE END-CLASS           TO QTRP1-WEND-CLASS.
           MOVE 0                   TO TP-ACCTNO TP-LINE
                                       QTRP1-WBEG-ACCTNO
                                       QTRP1-WBEG-LINE.
           MOVE ALL "9"             TO QTRP1-WEND-ACCTNO
                                       QTRP1-WEND-LINE.

           MOVE TP-DATE              TO NDTE-DATE.
           MOVE 1                    TO NDTE-HOLD.
           PERFORM INCREMENT-DAYS.
           MOVE NDTE-DATE            TO SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD  TO QTRP1-WSBEG-DATE.

           IF ( TP-CLASS = 999 )
              MOVE TP-CLASS          TO QTRP1-WSBEG-CLASS
           ELSE
              COMPUTE QTRP1-WSBEG-CLASS  = TP-CLASS + 1.

           IF ( TP-ACCTNO = 99999999 )
              MOVE TP-ACCTNO         TO QTRP1-WSBEG-ACCTNO
           ELSE
              COMPUTE QTRP1-WSBEG-ACCTNO = TP-ACCTNO + 1.

           IF ( TP-LINE = 999 )
              MOVE TP-LINE           TO QTRP1-WSBEG-LINE
           ELSE
              COMPUTE QTRP1-WSBEG-LINE   = TP-LINE + 1.  

           MOVE BEG-ST               TO QWS-BEG-ST.
           MOVE END-ST               TO QWS-END-ST.

           PERFORM START-TRP1-FILE. *> SSP - TRP,LN

       SORT-TRP-NEXT.
           PERFORM READ-TRP1-FILE-NEXT. *> EMBEDDED - TRP,LN
           IF (IO-FG NOT = 0) OR (TRP-PATH-OWNBR NOT = TP-BRNO)
              GO TO SORT-TRP-EXIT.

           IF ( TP-DATE > END-DATE )
              GO TO SORT-TRP-EXIT.

           IF TP-CLASS < BEG-CLASS OR TP-CLASS > END-CLASS
              GO TO SORT-TRP-NEXT.

           IF TP-ADDON-TYPE NOT = "O3"
              IF TP-TRCD NOT = "R3" AND NOT = "C3"
                 GO TO SORT-TRP-NEXT.

      *    MOVE TP-BRNO   TO LN-OWNBR.
      *    MOVE TP-ACCTNO TO LN-ACCTNO.
      *    PERFORM READ-LN1-FILE.
      *    IF IO-FG  NOT = 0
      *       GO TO SORT-TRP-NEXT.

           IF LN-ORGST < BEG-ST OR LN-ORGST > END-ST
              GO TO SORT-TRP-NEXT.

      *    IF LN-OWNBR NOT = TR1-BRNO    <<< THIS DOES NOT WORK ANYMORE !!
      * IF ANOTHER BRANCH DOES A PAYOFF, AND THE SAME LOAN # EXIST
      * IN BOTH BRANCHES, (NOT REALLY THE SAME LOAN!), IT WILL PRINT FOR
      * BOTH BRANCHES, SHOULD ONLY PRINT FOR THE OWNING BRANCH !!

      * ONLY REPORT FOR THE OWNING BRANCH ("G")
           IF TP-OTHBR-FG = "C"
              GO TO SORT-TRP-NEXT
      * THE "P" TRANSACTIONS ARE ONLY ADDONS AND REBATES/CANCELS
      * NEED TO SUBTRACT OUT THE INSURANCE TAX, BUT CHECK FOR
      * SPACES HERE SO WE CAN JUST SUBTRACT IT FURTHER DOWN
           ELSE
              IF TP-INS-TAX-RB = 2020202.02
                 MOVE 0 TO TP-INS-TAX-RB.

           MOVE 0 TO WORK-INSCOMP.
           IF TP-INSOURS = "Y"
              MOVE LN-INSCOMP(6) TO WORK-INSCOMP
              IF TP-INSCOMP NUMERIC AND TP-INSCOMP NOT = 0
                 MOVE TP-INSCOMP TO WORK-INSCOMP.

           IF WORK-INSCOMP = 0
              GO TO SORT-TRP-NEXT.

           IF WORK-INSCOMP < BEG-IC OR WORK-INSCOMP > END-IC
              GO TO SORT-TRP-NEXT.

           MOVE TP-DATE   TO WORK-DATE.
           MOVE TP-BRNO   TO WORK-OWNBR.
           MOVE TP-CLASS  TO WORK-CLASS.
           MOVE TP-ACCTNO TO WORK-ACCTNO.
           PERFORM READ-WK-FILE.
           IF IO-FG NOT = 0
              MOVE 0 TO WORK-NEW-O3 WORK-ADJ-O3 WORK-CAN-O3
                        WORK-CAN-O3-DATE WORK-COM-O3 WORK-DLR-COM-O3
              MOVE SPACES TO WORK-ADDON-FG WORK-CANCEL-FG.

           IF TP-ADDON-TYPE = "O3" AND
                    TP-ADDON-TEXT = "ADD"
              MOVE "A" TO WORK-ADDON-FG
              COMPUTE WORK-NEW-O3 = WORK-NEW-O3 +
                           (TP-APCUR - TP-GLAMT(1) - TP-GLAMT(2))
              COMPUTE WORK-COM-O3 = WORK-COM-O3 + TP-INSCOMM
           ELSE
           IF TP-ADDON-TYPE = "O3" AND
                    TP-ADDON-TEXT = "ADJ"
              COMPUTE WORK-ADJ-O3 = WORK-ADJ-O3 +
                           (TP-APCUR - TP-GLAMT(1) - TP-GLAMT(2))
              COMPUTE WORK-COM-O3 = WORK-COM-O3 + TP-INSCOMM
           ELSE
           IF TP-ADDON-TYPE = "O3" AND
                    TP-ADDON-TEXT = "CNL"
              MOVE "C" TO WORK-CANCEL-FG
              COMPUTE WORK-CAN-O3 = WORK-CAN-O3 +
                           (TP-APCUR - TP-GLAMT(1) - TP-GLAMT(2))
              MOVE TP-PAYDATE TO WORK-CAN-O3-DATE
              COMPUTE WORK-COM-O3 = WORK-COM-O3 + TP-INSCOMM
           ELSE
           IF TP-TRCD = "R3"
              COMPUTE WORK-CAN-O3 = WORK-CAN-O3 +
                                ((TP-TRAMT - TP-INS-TAX-RB) * -1)
              MOVE TP-PAYDATE TO WORK-CAN-O3-DATE
              COMPUTE WORK-COM-O3 = WORK-COM-O3 + (TP-INSCOMM * -1)
           ELSE
           IF TP-TRCD = "C3"
              MOVE "C" TO WORK-CANCEL-FG
              COMPUTE WORK-CAN-O3 = WORK-CAN-O3 +
                                ((TP-TRAMT - TP-INS-TAX-RB) * -1)
              MOVE TP-PAYDATE TO WORK-CAN-O3-DATE
              COMPUTE WORK-COM-O3 = WORK-COM-O3 + (TP-INSCOMM * -1).

           IF IO-FG = 0
              PERFORM REWRITE-WK-FILE
           ELSE
              PERFORM WRITE-WK-FILE.

           GO TO SORT-TRP-NEXT.

       SORT-TRP-EXIT.
           PERFORM CLOSE-TRP1-FILE.

      **********************************************
       SORT-TRL-FILE SECTION.
      **********************************************

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

           MOVE BEG-IC              TO QWS-BEG-IC.
           MOVE END-IC              TO QWS-END-IC.
           MOVE BEG-ST              TO QWS-BEG-ST.
           MOVE END-ST              TO QWS-END-ST.

           PERFORM START-TRL1-FILE. *> EMBEDDED - TRL,LN

       SORT-TRL-NEXT.
           PERFORM READ-TRL1-FILE-NEXT. *> EMBEDDED - TRL,LN
           IF (IO-FG NOT = 0) OR (TRL-PATH-OWNBR NOT = TL-BRNO)
              GO TO SORT-TRL-EXIT.

           IF ( TL-DATE > END-DATE )
              GO TO SORT-TRL-EXIT.

           IF TL-CLASS < BEG-CLASS OR TL-CLASS > END-CLASS
              GO TO SORT-TRL-NEXT.

           IF TL-INSCOMP(6) = 0
              GO TO SORT-TRL-NEXT
           ELSE
           IF TL-INSCOMP(6) < BEG-IC OR > END-IC
              GO TO SORT-TRL-NEXT
           ELSE
           IF TL-INSOURS(6) NOT = "Y"
              GO TO SORT-TRL-NEXT.

      *    MOVE TL-BRNO   TO LN-OWNBR.
      *    MOVE TL-ACCTNO TO LN-ACCTNO.
      *    PERFORM READ-LN1-FILE.
      *    IF IO-FG  NOT = 0
      *       GO TO SORT-TRL-NEXT.

           IF LN-ORGST < BEG-ST OR LN-ORGST > END-ST
              GO TO SORT-TRL-NEXT.

           MOVE TL-INSCOMP(6) TO WORK-INSCOMP.

           MOVE TL-DATE   TO WORK-DATE.
           MOVE TL-BRNO   TO WORK-OWNBR.
           MOVE TL-CLASS  TO WORK-CLASS.
           MOVE TL-ACCTNO TO WORK-ACCTNO.
           PERFORM READ-WK-FILE.
           IF IO-FG NOT = 0
              MOVE 0 TO WORK-NEW-O3 WORK-ADJ-O3 WORK-CAN-O3
                        WORK-CAN-O3-DATE WORK-COM-O3 WORK-DLR-COM-O3
              MOVE SPACES TO WORK-ADDON-FG WORK-CANCEL-FG.

           COMPUTE WORK-NEW-O3 = WORK-NEW-O3 + TL-INSPREM(6).
           COMPUTE WORK-COM-O3 = WORK-COM-O3 + TL-INSCOMM(6).


           IF IO-FG = 0
              PERFORM REWRITE-WK-FILE
           ELSE
              PERFORM WRITE-WK-FILE.

           GO TO SORT-TRL-NEXT.

       SORT-TRL-EXIT.
           PERFORM CLOSE-TRL1-FILE.

      **********************************************
       SORT-TRS-FILE SECTION.
      **********************************************

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

           MOVE BEG-IC              TO QWS-BEG-IC.
           MOVE END-IC              TO QWS-END-IC.
           MOVE BEG-ST              TO QWS-BEG-ST.
           MOVE END-ST              TO QWS-END-ST.

           PERFORM START-TRS1-FILE. *> EMBEDDED - TRS,LN

       SORT-TRS-NEXT.
           PERFORM READ-TRS1-FILE-NEXT. *> EMBEDDED - TRS,LN
           IF (IO-FG NOT = 0) OR (TRS-PATH-OWNBR NOT = TS-BRNO)
              GO TO SORT-TRS-EXIT.

           IF ( TS-DATE > END-DATE )
              GO TO SORT-TRS-EXIT.

           IF TS-CLASS < BEG-CLASS OR TS-CLASS > END-CLASS
              GO TO SORT-TRS-NEXT.

           IF TS-INSCOMP(6) = 0
              GO TO SORT-TRS-NEXT
           ELSE
           IF TS-INSCOMP(6) < BEG-IC OR > END-IC
              GO TO SORT-TRS-NEXT
           ELSE
           IF TS-INSOURS(6) NOT = "Y"
              GO TO SORT-TRS-NEXT.

      *    MOVE TS-BRNO   TO LN-OWNBR.
      *    MOVE TS-ACCTNO TO LN-ACCTNO.
      *    PERFORM READ-LN1-FILE.
      *    IF IO-FG  NOT = 0
      *       GO TO SORT-TRS-NEXT.

           IF LN-ORGST < BEG-ST OR LN-ORGST > END-ST
              GO TO SORT-TRS-NEXT.

           MOVE TS-INSCOMP(6) TO WORK-INSCOMP.

           MOVE TS-DATE   TO WORK-DATE.
           MOVE TS-BRNO   TO WORK-OWNBR.
           MOVE TS-CLASS  TO WORK-CLASS.
           MOVE TS-ACCTNO TO WORK-ACCTNO.
           PERFORM READ-WK-FILE.
           IF IO-FG NOT = 0
              MOVE 0 TO WORK-NEW-O3 WORK-ADJ-O3 WORK-CAN-O3
                        WORK-CAN-O3-DATE WORK-COM-O3 WORK-DLR-COM-O3
              MOVE SPACES TO WORK-ADDON-FG WORK-CANCEL-FG.

           COMPUTE WORK-NEW-O3 = WORK-NEW-O3 + TS-INSPREM(6)
           COMPUTE WORK-COM-O3 = WORK-COM-O3 + TS-INSCOMM(6)
           COMPUTE WORK-DLR-COM-O3 = WORK-DLR-COM-O3 +
                                     TS-O3INS-DLPROC.

           IF IO-FG = 0
              PERFORM REWRITE-WK-FILE
           ELSE
              PERFORM WRITE-WK-FILE.

           GO TO SORT-TRS-NEXT.

       SORT-TRS-EXIT.
           PERFORM CLOSE-TRS1-FILE.

      ******************************************************************
       KEY-DETAIL SECTION.
      ******************************************************************
           IF SUMMARY-ONLY = "Y" OR "I"
              GO TO END-KEY-DETAIL.

           IF (LN-ACCTNO = WORK-ACCTNO) AND (LN-OWNBR = WORK-OWNBR)
              GO TO EXIT-KEY-DETAIL.

      * SETUP PRIMARY INSURED INFO:

           IF LN-INSURED = "S"
              MOVE LN-SSNO(2) TO BW-SSNO
           ELSE
              MOVE LN-SSNO(1) TO BW-SSNO.

           PERFORM SETUP-INSURED.

           MOVE BAGE-AGE TO D-AGE.

           IF LN-INSURED = "S"
              MOVE LN-SSNO(1) TO BW-SSNO
              PERFORM READ-BW1-FILE. *> EMBEDDED - BWFILE

           PERFORM LPSTAT-DISPLAY.
           MOVE LPS-DISPLAY-LNNO TO D-LOAN-NUM.

           IF O3-PREM = 0 AND O3-ADJ = 0 AND O3-CAN = 0 AND
               O3-COMM = 0
              GO TO SKIP-O3.

           MOVE 6 TO ITRM-SUB.
           PERFORM INS-TERM-CALCULATION.
           MOVE ITRM-INSTERM TO D-TERM.

           MOVE LN-INSEFF-DATE(6) TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO D-EFFDATE.
           MOVE LN-INSEXP-DATE(6) TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO D-EXPDATE.
           MOVE LN-INSCOVR(6) TO D-COVRG D-REGPYAMT.

           MOVE O3-ADDON-FG TO D-ADDTEXT.
           MOVE O3-PREM TO D-INSPREM.
           MOVE O3-ADJ TO D-INSADJ.
           MOVE O3-CAN TO D-INSREFUND.
           MOVE O3-CANCEL-FG TO D-CNLTEXT.

           IF O3-CAN NOT = 0
              MOVE O3-CAN-DATE TO DATE-YYYYMMDD
              PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
              MOVE DATE-MMDDYY TO D-REFUND-DATE.

           MOVE O3-COMM TO D-INSCOMM.

       SKIP-O3.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           IF O3-DLR-COMM NOT = 0
              MOVE O3-DLR-COMM TO D-INSCOMM
              MOVE 1 TO LN-FEED
              PERFORM WRITE-DETAIL-LINE.

       END-KEY-DETAIL.
           PERFORM CREATE-TOTAL-RECS.
           MOVE 0 TO O3-PREM O3-ADJ O3-CAN O3-COMM O3-CAN-DATE
                     O3-DLR-COMM.
           MOVE SPACES TO O3-ADDON-FG O3-CANCEL-FG.

       EXIT-KEY-DETAIL.
           EXIT.

      ******************************************************************
       CREATE-TOTAL-RECS SECTION.
      ******************************************************************
           MOVE 0 TO ONE-O3.

           IF LN-INSCOMP(6) NOT = 0
              MOVE LN-INSCOVR(6) TO NEW-COVERAGE
              IF O3-PREM = 0
                 MOVE 0 TO NEW-COVERAGE
              ELSE
                 IF O3-PREM < 0
                    MULTIPLY -1 BY NEW-COVERAGE.

           IF O3-PREM > 0
              MOVE 1 TO ONE-O3
           ELSE
              IF O3-PREM < 0
                 MOVE -1 TO ONE-O3.

      *  WK-CODE: 1-BR, 2-ST, 3-IC, 4-GB

           PERFORM INIT-WK1-KEY.
           MOVE 1 TO WK-CODE.
           MOVE LN-OWNBR TO WK-BRNO.
           PERFORM CREATE-WK.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           PERFORM INIT-WK1-KEY.
           MOVE 2 TO WK-CODE.
           MOVE LN-ORGST TO WK-ST.
           PERFORM CREATE-WK.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           PERFORM INIT-WK1-KEY.
           MOVE 3 TO WK-CODE.
           PERFORM CREATE-WK.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           PERFORM INIT-WK1-KEY.
           MOVE 4 TO WK-CODE.
           MOVE 0 TO WK-IC.
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

           MOVE 0 TO
              WK-AMT(1) WK-AMT(2) WK-AMT(3) WK-AMT(8)
              WK-AMT(4) WK-AMT(5) WK-AMT(6) WK-AMT(7)
              WK-QTY(1) WK-QTY(2).

           PERFORM WRITE-WK1-FILE.

           IF IO-FG = 9
              GO TO IO-ERROR.

           MOVE "Y" TO REC-EXISTS.
           GO TO CHECK-REC-EXISTS.

      *----------------------------------------------------------------
       UPDATE-WK1-REC.
           ADD ONE-O3 TO WK-QTY(1).
           ADD NEW-COVERAGE TO WK-AMT(1).

           IF O3-ADDON-FG = "A"
              ADD O3-PREM TO WK-AMT(6)
           ELSE
              ADD O3-PREM TO WK-AMT(2).

           ADD O3-ADJ TO WK-AMT(3).

           IF O3-CANCEL-FG = "C"
              ADD O3-CAN TO WK-AMT(7)
           ELSE
              ADD O3-CAN TO WK-AMT(4).

           ADD O3-COMM TO WK-AMT(5).
           ADD O3-DLR-COMM TO WK-AMT(8).
           PERFORM REWRITE-WK1-FILE.

      ******************************************************************
       BR-TOTALS SECTION.
      ******************************************************************
           IF SUMMARY-ONLY = "I"
              GO TO EXIT-BR-TOTALS.

           MOVE "B R A N C H   T O T A L S:" TO H-PAGE-DESC.
           MOVE 1 TO SAVE-CODE.
           MOVE HOLD-BR TO SAVE-BR.
           PERFORM PRINT-TOTALS.

           MOVE LN-OWNBR TO HOLD-BR.
           MOVE " " TO H-PAGE-DESC.
           PERFORM BR-HEADER.
           MOVE 0 TO BR-PAGE-NUMBER.

       EXIT-BR-TOTALS.
           EXIT.

      ******************************************************************
       IC-TOTALS SECTION.
      ******************************************************************
           PERFORM BR-TOTALS.

           IF SUMMARY-ONLY NOT = "I"
              MOVE "S T A T E   T O T A L S:" TO H-PAGE-DESC
              MOVE 2 TO SAVE-CODE
              MOVE " " TO SAVE-ST
              PERFORM PRINT-TOTALS.

           MOVE "I N S U R E R   T O T A L S:" TO H-PAGE-DESC.
           MOVE 0 TO H-BRNO.
           MOVE " " TO H-BRNAME.
           MOVE 3 TO SAVE-CODE.
           MOVE " " TO SAVE-ST.
           PERFORM PRINT-TOTALS.

           MOVE WORK-INSCOMP TO HOLD-IC.
           MOVE " " TO H-PAGE-DESC.
           PERFORM BR-HEADER.
           PERFORM IC-HEADER.

      ******************************************************************
       GB-TOTALS SECTION.
      ******************************************************************
           PERFORM IC-TOTALS.
           MOVE "C O M P A N Y   T O T A L S:" TO H-PAGE-DESC.
           MOVE 0 TO H-BRNO.
           MOVE " " TO H-BRNAME.
           MOVE 4 TO SAVE-CODE.
           MOVE 0 TO HOLD-IC.
           MOVE " " TO SAVE-ST.
           PERFORM PRINT-TOTALS.

      ******************************************************************
       PRINT-TOTALS SECTION.
      ******************************************************************
           MOVE 99 TO LN-COUNT.
           MOVE "Y" TO TOTAL-FG.
           MOVE "Y" TO FIRST-TOT.
           MOVE " " TO WK1-KEY.
           MOVE SAVE-CODE TO WK-CODE.
           MOVE HOLD-IC TO WK-IC.
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

           IF WK-IC NOT = HOLD-IC
              GO TO EXIT-PRINT-TOTALS.

           IF SAVE-CODE NOT = 2
              GO TO SKIP-FIELD-2.

      *----------------------------------------------------------------
       LOOP-FIELD-2.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    ST LOOP
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
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

      *----------------------------------------------------------------
       SKIP-FIELD-2.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    BR/IC LOOP
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
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
           PERFORM READ-BW1-FILE. *> EMBEDDED - BWFILE

           IF IO-FG = 9
              MOVE "*** NOT FOUND ***" TO BW-FNAME
              MOVE SPACES TO BW-LNAME
              MOVE 0      TO BW-BDATE BW-REFDATE.

      * SET NAME OF PRIMARY INSURED:

           MOVE SPACES TO D-NAME.
           STRING BW-FNAME," ",BW-LNAME DELIMITED BY "   "
               INTO D-NAME.

      * SET AGE OF PRIMARY INSURED:

           MOVE BW-BDATE   TO BAGE-DATE.
           MOVE BW-REFDATE TO BAGE-REFDATE.
           MOVE END-DATE   TO BAGE-TODAY.
           PERFORM BAGE-CALCULATION.

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
           ADD 1 TO PAGE-NUMBER.
           MOVE PAGE-NUMBER TO H-PAGE-NO.
           PERFORM GET-TIME.
           MOVE T-HH TO H-HR.
           MOVE T-MM TO H-MIN.

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
              MOVE 2 TO LN-FEED
              PERFORM WRITE-PR-REC
              MOVE HEAD6 TO PR-REC
              MOVE 1 TO LN-FEED
              PERFORM WRITE-PR-REC
           ELSE
              MOVE HEAD7 TO PR-REC
              MOVE 2 TO LN-FEED
              PERFORM WRITE-PR-REC.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF TOTAL-FG = "N"
              NEXT SENTENCE
           ELSE
              MOVE HEAD8 TO PR-REC
              MOVE 1 TO LN-FEED
              PERFORM WRITE-PR-REC.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE DASH-LINE TO PR-REC.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-PR-REC.

      *----------------------------------------------------------------
       END-HEAD-PAGE.
           MOVE 8 TO LN-COUNT.
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
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "BEG INS CO" TO RANGE-LINE.
           MOVE BEG-IC TO RANGE-SEL.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "END INS CO" TO RANGE-LINE.
           MOVE END-IC TO RANGE-SEL.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "BEG ORIG STATE" TO RANGE-LINE.
           MOVE BEG-ST TO RANGE-SEL.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "END ORIG STATE" TO RANGE-LINE.
           MOVE END-ST TO RANGE-SEL.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "BEG LOAN CLASS" TO RANGE-LINE.
           MOVE BEG-CLASS TO RANGE-SEL.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "END LOAN CLASS" TO RANGE-LINE.
           MOVE END-CLASS TO RANGE-SEL.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "BEGINNING DATE" TO RANGE-LINE.
           MOVE BEG-DATE TO RANGE-SEL.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "ENDING    DATE" TO RANGE-LINE.
           MOVE END-DATE TO RANGE-SEL.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "SUMMARY ONLY ?" TO RANGE-LINE.
           MOVE SUMMARY-ONLY TO RANGE-SEL
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      ******************************************************************
      *
      *     C O P Y    L I B R A R Y    S E C T I O N
      *
      ******************************************************************
       COPY "LIBLP/LPSTAT.CPY".
       COPY "LIBLP/LPMDTE.CPY".
       COPY "LIBLP/ARRAYSP.CPY".
       COPY "LIBLP/LPITRM.CPY".
       COPY "LIBGB/TIMEIO.CPY".
       COPY "LIBGB/DATER.CPY".
       COPY "LIBLP/LPBAGE.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/GRWORK.CPY".
       COPY "LIBGB/GRSERCH.CPY".
       COPY "LIBGB/BRSECURE.CPY".

      ******************************************************************
       MOVE-ACCUM-TOTALS SECTION.
      ******************************************************************
           MOVE 8 TO SUB.

      *----------------------------------------------------------------
       NEXT-AMT-ACCUM.
           ADD WK-AMT(SUB) TO TOT-AMT(SUB).

           IF SUB > 1
              SUBTRACT 1 FROM SUB

           GO TO NEXT-AMT-ACCUM.

      *----------------------------------------------------------------
       NEXT-QTY-ACCUM.
           ADD WK-QTY(SUB) TO TOT-QTY(SUB).

           IF SUB < 2
              ADD 1 TO SUB
              GO TO NEXT-QTY-ACCUM.

      * ADJUSTMNTS
           MOVE WK-AMT(3) TO D-AMT(4).
      * PAYABLE = INSPREM - INSCOMM - DLRCOMM
      *    COMPUTE D-AMT(6) = WK-AMT(2) - WK-AMT(5) - WK-AMT(8).

      * 11/18/04
      * PAYABLE = (NEW PREM + ADDONS)  -
      *                (ADJ + CANCEL + REF + (NETCOM - DLRCOMM)

           COMPUTE D-AMT(6) = ( WK-AMT(2) + WK-AMT(6) ) -
      * 7 AND 4 ARE NEGATIVE NUMBERS
               ( WK-AMT(3) - WK-AMT(7) - WK-AMT(4) + WK-AMT(5) +
                                                       WK-AMT(8) ).
      *NET PREMIUMS
      * (NEW PREM + ADDONS) + (REFUNDS + CANCELS) - ADJUSTMENTS
           COMPUTE D-AMT(7) =
              (WK-AMT(2) + WK-AMT(6)) + (WK-AMT(4) + WK-AMT(7))
                                           + WK-AMT(3).

           MOVE 2 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE WK-CLASS TO D-CLASS.
           MOVE WK-QTY(1) TO D-QTY.
      * LOAN COVERAGE
           MOVE WK-AMT(1) TO D-AMT(1).
      * NEW LOAN PREMIUMS
           MOVE WK-AMT(2) TO D-AMT(2).
      * ADDON PREMIUMS
           MOVE WK-AMT(6) TO D-AMT(3).
      * CANCELS
           MOVE WK-AMT(7) TO D-AMT(4).
      * REFUNDS
           MOVE WK-AMT(4) TO D-AMT(5).
      * DEALER COMMISSION
           MOVE WK-AMT(8) TO D-AMT(6).
      * NET COMMISSION
           MOVE WK-AMT(5) TO D-AMT(7).
           PERFORM WRITE-DETAIL-LINE.

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

      *********************************************************
       FORM-RESET SECTION.
      *********************************************************
           MOVE EXT-CONAME-CONAME TO O3MORP-CONAME.
           MOVE EXT-CONAME-SYSDATE TO O3MORP-SYSDATE.
           DISPLAY O3MORP-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE O3MORP-FORM-FIELDS TO WR-BUF.
           MOVE O3MORP-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.
       FORM-RESET-EXIT.
           EXIT.

      *********************************************************
       SETUP-LINK-INFO SECTION.
      *********************************************************
           MOVE O3MORP-HELP--FILE TO HELP-LINK-FILE.
           MOVE O3MORP-HELP--DIR  TO HELP-LINK-DIR.

      *********************************************************
       VDU-CONVERT-FIELD SECTION.
      *********************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBLP/O3MORP_EVA.CPY".
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
      *----------------------------------------------------------------
       IC-HEADER.
           MOVE HOLD-IC TO IC-CODE.
           PERFORM READ-IC1-FILE. *> EMBEDDED - ICFILE

           IF IO-FG = 9
              MOVE "* INVALID *" TO IC-NAME.

           MOVE HOLD-IC TO H-ICNO.
           MOVE IC-NAME TO H-ICNAME.

           IF COMPANY-FG = "Y"
              MOVE SPACES TO H-INSCO H-ICNAME
              MOVE 0 TO H-ICNO.

      *----------------------------------------------------------------
       BR-HEADER.
           MOVE HOLD-BR TO BR-NO.
           PERFORM READ-BR-FILE.
           IF IO-FG = 9
              MOVE "* INVALID *" TO BR-NAME.

           MOVE HOLD-BR TO H-BRNO.
           MOVE BR-NAME TO H-BRNAME.

      *-----------------------------------------------------------------
       GET-LN-BW-PATH.
           PERFORM CLOSE-LN1-FILE.
           PERFORM CLOSE-BW1-FILE.
           PERFORM CLOSE-IC1-FILE.

           PERFORM READ-BR-FILE.
           IF IO-FG = 9
              GO TO IO-ERROR.

           MOVE EXT-FILPATH-BASE           TO FILPATH-BASE.
           MOVE BR-MACHINE                 TO FILPATH-MACHINE.
           MOVE BR-DATA-PATH               TO FILPATH-DATA.

           MOVE "B"                        TO BW-PATH-OVERRIDE
                                              IC-PATH-OVERRIDE
                                              LN-PATH-OVERRIDE.

           PERFORM OPEN-LN1-FILE.
           PERFORM OPEN-BW1-FILE.
           PERFORM OPEN-IC1-FILE.

      *----------------------------------------------------------------
       INIT-WK1-KEY.
           MOVE " " TO WK1-KEY.
           MOVE HOLD-IC TO WK-IC.
           MOVE LN-CLASS TO WK-CLASS.

      *----------------------------------------------------------------
       TOTAL-LINE.
      * ADJUSTMNTS
           MOVE TOT-AMT(3) TO D-AMT(4).
      * PAYABLE = INSPREM - INSCOMM - DLRCOMM
      *    COMPUTE D-AMT(6) = TOT-AMT(2) - TOT-AMT(5) - TOT-AMT(8).
      * 11/18/04
      * PAYABLE = (NEW PREM + ADDONS)  -
      *                (ADJ + CANCEL + REF + (NETCOM - DLRCOMM)
           COMPUTE D-AMT(6) = ( TOT-AMT(2) + TOT-AMT(6) ) -
      * 7 AND 4 ARE NEGATIVE NUMBERS
               ( TOT-AMT(3) - TOT-AMT(7) - TOT-AMT(4) + TOT-AMT(5) +
                                                       TOT-AMT(8) ).
      *NET PREMIUMS
      * (NEW PREM + ADDONS) + (REFUNDS + CANCELS) - ADJUSTMENTS
           COMPUTE D-AMT(7) =
              (TOT-AMT(2) + TOT-AMT(6)) + (TOT-AMT(4) + TOT-AMT(7))
                                                    + TOT-AMT(3).

           MOVE 3 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "ALL" TO DETAIL-LINE.
           MOVE TOT-QTY(1) TO D-QTY.

      * LOAN COVERAGE
           MOVE TOT-AMT(1) TO D-AMT(1).
      * NEW LOAN PREMIUMS
           MOVE TOT-AMT(2) TO D-AMT(2).
      * ADDON PREMIUMS
           MOVE TOT-AMT(6) TO D-AMT(3).
      * CANCELS
           MOVE TOT-AMT(7) TO D-AMT(4).
      * REFUNDS
           MOVE TOT-AMT(4) TO D-AMT(5).
      * DEALER COMMISSION
           MOVE TOT-AMT(8) TO D-AMT(6).
      * NET COMMISSION
           MOVE TOT-AMT(5) TO D-AMT(7).
           PERFORM WRITE-DETAIL-LINE.

           PERFORM CLEAR-TOTALS.
           MOVE 99 TO LN-COUNT.

      *----------------------------------------------------------------
       CLEAR-TOTALS.
           MOVE 0 TO
              TOT-AMT(1) TOT-AMT(2) TOT-AMT(3) TOT-AMT(4) TOT-AMT(5)
              TOT-AMT(6) TOT-AMT(7) TOT-AMT(8) TOT-QTY(1) TOT-QTY(2).

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
       COPY "LIBLP/LPTRPGS_SQL.CPY".
       COPY "LIBLP/LPTRLGS_SQL.CPY".
       COPY "LIBLP/LPSPGS_SQL.CPY".
       COPY "LIBLP/LPLNGS_SQL.CPY".
       COPY "LIBLP/LPICGS_SQL.CPY".
       COPY "LIBLP/LPBWGS_SQL.CPY".
       COPY "LIBGB/GBGRGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBPRINT.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBGB/GBGR1RN.CPY".

      * COPY "LIBLP/LPLN1RN.CPY".
      * COPYMEMBER: LIBLP/LPLN1RN
       LOAD-LN1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( LN-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( LN-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO LN-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO LN-ALL-MACHINE
              MOVE EXT-FILPATH-DATA        TO LN-ALL-DATA
              MOVE LN-ALL-PATH             TO LN-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( LN-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO LN-ALL-BASE
              MOVE FILPATH-MACHINE         TO LN-ALL-MACHINE
              MOVE FILPATH-DATA            TO LN-ALL-DATA
              MOVE LN-ALL-PATH             TO LN-PATH.

      *-----------------------------------------------------------------
       OPEN-LN1-FILE.
           PERFORM LOAD-LN1-FILE.
           PERFORM OPEN-IT.
           MOVE LN-PATH-SQL TO E-FILE.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       READ-LN1-FILE. *> EMBEDDED - LNFILE
           PERFORM READ-IT.
           MOVE LN-PATH-OWNBR  TO LN-OWNBR.
           MOVE LN-PATH-SQL    TO E-FILE.
           MOVE LN1-KEY        TO E-KEYX.

           MOVE LN-OWNBR  TO QLN-OWNBR.
           MOVE LN-ACCTNO TO QLN-ACCTNO.

           EXEC SQL
            SELECT LNFILE.LN_OWNBR,
                   LNFILE.LN_ACCTNO,
                   LNFILE.LN_CLASS,
                   LNFILE.LN_SUBCLASS,
                   LNFILE.LN_LAWCODE,
                   LNFILE.LN_SSNO1,
                   LNFILE.LN_MAKERCD1,  
                   LNFILE.LN_SSNO2,
                   LNFILE.LN_MAKERCD2, 
                   LNFILE.LN_SSNO3,
                   LNFILE.LN_MAKERCD3, 
                   LNFILE.LN_SSNO4,
                   LNFILE.LN_MAKERCD4, 
                   LNFILE.LN_ORGST, 
                   LNFILE.LN_SPRCLASS,
                   LNFILE.LN_STATFLAGS, 
                   CAST(LNFILE.LN_ORIG_1STPYDATE  AS VARCHAR(10)), 
                   CAST(LNFILE.LN_1STPYDATE       AS VARCHAR(10)), 
                   LNFILE.LN_ORGTERM,
                   LNFILE.LN_INSURED, 
                   LNFILE.LN_INSTYPES, 
                   LNFILE.LN_INSCOMP_1,
                   LNFILE.LN_INSCOMP_2,
                   LNFILE.LN_INSCOMP_3,
                   LNFILE.LN_INSCOMP_4,
                   LNFILE.LN_INSCOMP_5,
                   LNFILE.LN_INSCOMP_6,
                   LNFILE.LN_INSCOMP_7,
                   LNFILE.LN_INSCOMP_8,
                   LNFILE.LN_INSCOMP_9,
                   LNFILE.LN_INSCOMP_10,
                   CAST(LNFILE.LN_INSEFF_DATE_1   AS VARCHAR(10)), 
                   CAST(LNFILE.LN_INSEFF_DATE_2   AS VARCHAR(10)), 
                   CAST(LNFILE.LN_INSEFF_DATE_3   AS VARCHAR(10)), 
                   CAST(LNFILE.LN_INSEFF_DATE_4   AS VARCHAR(10)), 
                   CAST(LNFILE.LN_INSEFF_DATE_5   AS VARCHAR(10)), 
                   CAST(LNFILE.LN_INSEFF_DATE_6   AS VARCHAR(10)), 
                   CAST(LNFILE.LN_INSEFF_DATE_7   AS VARCHAR(10)), 
                   CAST(LNFILE.LN_INSEFF_DATE_8   AS VARCHAR(10)), 
                   CAST(LNFILE.LN_INSEFF_DATE_9   AS VARCHAR(10)), 
                   CAST(LNFILE.LN_INSEFF_DATE_10  AS VARCHAR(10)), 
                   CAST(LNFILE.LN_INSEXP_DATE_1   AS VARCHAR(10)), 
                   CAST(LNFILE.LN_INSEXP_DATE_2   AS VARCHAR(10)), 
                   CAST(LNFILE.LN_INSEXP_DATE_3   AS VARCHAR(10)), 
                   CAST(LNFILE.LN_INSEXP_DATE_4   AS VARCHAR(10)), 
                   CAST(LNFILE.LN_INSEXP_DATE_5   AS VARCHAR(10)), 
                   CAST(LNFILE.LN_INSEXP_DATE_6   AS VARCHAR(10)), 
                   CAST(LNFILE.LN_INSEXP_DATE_7   AS VARCHAR(10)), 
                   CAST(LNFILE.LN_INSEXP_DATE_8   AS VARCHAR(10)), 
                   CAST(LNFILE.LN_INSEXP_DATE_9   AS VARCHAR(10)), 
                   CAST(LNFILE.LN_INSEXP_DATE_10  AS VARCHAR(10)), 
                   LNFILE.LN_INSCOVR_1,
                   LNFILE.LN_INSCOVR_2,
                   LNFILE.LN_INSCOVR_3,
                   LNFILE.LN_INSCOVR_4,
                   LNFILE.LN_INSCOVR_5,
                   LNFILE.LN_INSCOVR_6,
                   LNFILE.LN_INSCOVR_7,
                   LNFILE.LN_INSCOVR_8,
                   LNFILE.LN_INSCOVR_9,
                   LNFILE.LN_INSCOVR_10,
                   LNFILE.LN_TOTNODEF,
                   LNFILE.LN_CL_REBATE, 
                   LNFILE.LN_AH_REBATE, 
                   LNFILE.LN_PP_REBATE, 
                   LNFILE.LN_INT_REBATE, 
                   LNFILE.LN_SC_REBATE,
                   LNFILE.LN_MF_REBATE,
                   LNFILE.LN_DF_REBATE,
                   LNFILE.LN_EX_REBATE,
                   LNFILE.LN_O1_REBATE,
                   LNFILE.LN_O2_REBATE,
                   LNFILE.LN_O3_REBATE,
                   LNFILE.LN_O4_REBATE,
                   LNFILE.LN_O5_REBATE,
                   LNFILE.LN_O6_REBATE,
                   LNFILE.LN_O7_REBATE,
                   LNFILE.LN_UN_O7_REB,
                   LNFILE.LN_UN_O8_REB,
                   LNFILE.LN_UN_O9_REB,
                   LNFILE.LN_UN_O10_REB, 
                   LNFILE.LN_SUM_ORGTERM,
                   LNFILE.LN_UNITPER_CD,
                   LNFILE.LN_UNITPER_FREQ
              INTO  
                  :QLN-OWNBR,
                  :QLN-ACCTNO,
                  :QLN-CLASS,
                  :QLN-SUBCLASS,
                  :QLN-LAWCODE,
                  :QLN-SSNO1,
                  :QLN-MAKERCD1,  
                  :QLN-SSNO2,
                  :QLN-MAKERCD2, 
                  :QLN-SSNO3,
                  :QLN-MAKERCD3, 
                  :QLN-SSNO4,
                  :QLN-MAKERCD4, 
                  :QLN-ORGST, 
                  :QLN-SPRCLASS,
                  :QLN-STATFLAGS, 
                  :QLN-ORIG-1STPYDATE, 
                  :QLN-1STPYDATE,      
                  :QLN-ORGTERM,
                  :QLN-INSURED, 
                  :QLN-INSTYPES, 
                  :QLN-INSCOMP-1,
                  :QLN-INSCOMP-2,
                  :QLN-INSCOMP-3,
                  :QLN-INSCOMP-4,
                  :QLN-INSCOMP-5,
                  :QLN-INSCOMP-6,
                  :QLN-INSCOMP-7,
                  :QLN-INSCOMP-8,
                  :QLN-INSCOMP-9,
                  :QLN-INSCOMP-10,
                  :QLN-INSEFF-DATE-1,  
                  :QLN-INSEFF-DATE-2,  
                  :QLN-INSEFF-DATE-3,  
                  :QLN-INSEFF-DATE-4,  
                  :QLN-INSEFF-DATE-5,  
                  :QLN-INSEFF-DATE-6,  
                  :QLN-INSEFF-DATE-7,  
                  :QLN-INSEFF-DATE-8,  
                  :QLN-INSEFF-DATE-9,  
                  :QLN-INSEFF-DATE-10, 
                  :QLN-INSEXP-DATE-1,  
                  :QLN-INSEXP-DATE-2,  
                  :QLN-INSEXP-DATE-3,  
                  :QLN-INSEXP-DATE-4,  
                  :QLN-INSEXP-DATE-5,  
                  :QLN-INSEXP-DATE-6,  
                  :QLN-INSEXP-DATE-7,  
                  :QLN-INSEXP-DATE-8,  
                  :QLN-INSEXP-DATE-9,  
                  :QLN-INSEXP-DATE-10, 
                  :QLN-INSCOVR-1,
                  :QLN-INSCOVR-2,
                  :QLN-INSCOVR-3,
                  :QLN-INSCOVR-4,
                  :QLN-INSCOVR-5,
                  :QLN-INSCOVR-6,
                  :QLN-INSCOVR-7,
                  :QLN-INSCOVR-8,
                  :QLN-INSCOVR-9,
                  :QLN-INSCOVR-10,
                  :QLN-TOTNODEF,
                  :QLN-CL-REBATE, 
                  :QLN-AH-REBATE, 
                  :QLN-PP-REBATE, 
                  :QLN-INT-REBATE, 
                  :QLN-SC-REBATE,
                  :QLN-MF-REBATE,
                  :QLN-DF-REBATE,
                  :QLN-EX-REBATE,
                  :QLN-O1-REBATE,
                  :QLN-O2-REBATE,
                  :QLN-O3-REBATE,
                  :QLN-O4-REBATE,
                  :QLN-O5-REBATE,
                  :QLN-O6-REBATE,
                  :QLN-O7-REBATE,
                  :QLN-UN-O7-REB,
                  :QLN-UN-O8-REB,
                  :QLN-UN-O9-REB,
                  :QLN-UN-O10-REB, 
                  :QLN-SUM-ORGTERM,
                  :QLN-UNITPER-CD,
                  :QLN-UNITPER-FREQ
            FROM DBO.LNFILE
            WHERE LNFILE.LN_OWNBR  = :QLN-OWNBR
              AND LNFILE.LN_ACCTNO = :QLN-ACCTNO
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-LN1-FILE.

           IF ( IO-FG = 0 )
              PERFORM GET-LN-FIELDS.

      *-----------------------------------------------------------------
       CLOSE-LN1-FILE.

           PERFORM CLOSE-IT.


      * COPY "LIBLP/LPTRP1RN.CPY".
      * COPYMEMBER: LIBLP/LPTRP1RN
      * 20220923 BAH ADDED TP-LCDUE *1570
       LOAD-TRP1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( TRP-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( TRP-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO TRP-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO TRP-ALL-MACHINE
              MOVE EXT-FILPATH-DATA        TO TRP-ALL-DATA
              MOVE TRP-ALL-PATH             TO TRP-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( TRP-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO TRP-ALL-BASE
              MOVE FILPATH-MACHINE         TO TRP-ALL-MACHINE
              MOVE FILPATH-DATA            TO TRP-ALL-DATA
              MOVE TRP-ALL-PATH             TO TRP-PATH.

      *-----------------------------------------------------------------
       OPEN-TRP1-FILE.
           PERFORM LOAD-TRP1-FILE.
           PERFORM OPEN-IT.
           MOVE TRP-PATH-SQL TO E-FILE.

           IF TRP1-CONN-OPEN-SW = "N"
              MOVE "Y" TO TRP1-CONN-OPEN-SW
              EXEC SQL CONNECT TO :SQL-CONNECT-SERVER AS TRP1_CONNECT
                       USER :SQL-CONNECT-USERID USING :SQL-CONNECT-PSWD
              END-EXEC.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

           EXEC SQL SET CONNECTION C1 END-EXEC.

      *-----------------------------------------------------------------
       START-TRP1-FILE. *> SSP - TRP,LN

      D    STOP MESS.
           PERFORM START-IT.
           MOVE TRP-PATH-OWNBR  TO TP-BRNO.
           MOVE TRP-PATH-SQL    TO E-FILE.
           MOVE TRP1-KEY        TO E-KEYX.

           IF ( TRP1-CURSOR-STAT-GOOD )
              PERFORM CLOSE-TRP1-FILE.

           IF ( QTRP1-WEND-LINE = ZEROES    ) OR
              ( QTRP1-WEND-LINE NOT NUMERIC )
              MOVE TRP-PATH-OWNBR        TO QTRP1-WBEG-BRNO
              MOVE TRP-PATH-OWNBR        TO QTRP1-WEND-BRNO
              MOVE TP-DATE               TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD  TO QTRP1-WBEG-DATE
              MOVE 99991231         TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD  TO QTRP1-WEND-DATE
              MOVE TP-CLASS             TO QTRP1-WBEG-CLASS
              MOVE TP-ACCTNO            TO QTRP1-WBEG-ACCTNO
              MOVE TP-LINE              TO QTRP1-WBEG-LINE
              MOVE ALL "9"              TO QTRP1-WEND-CLASS
                                           QTRP1-WEND-ACCTNO
                                           QTRP1-WEND-LINE
      *- - - -{ ADD ONE TO WHERE SCAN CONTINUE FIELDS
              MOVE TP-DATE              TO NDTE-DATE
              MOVE 1                    TO NDTE-HOLD
              PERFORM INCREMENT-DAYS
              MOVE NDTE-DATE            TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD  TO QTRP1-WSBEG-DATE

              IF ( TP-CLASS = 999 )
                 MOVE TP-CLASS          TO QTRP1-WSBEG-CLASS
              ELSE
                 COMPUTE QTRP1-WSBEG-CLASS  = TP-CLASS + 1
              END-IF

              IF ( TP-ACCTNO = 99999999 )
                 MOVE TP-ACCTNO         TO QTRP1-WSBEG-ACCTNO
              ELSE
                 COMPUTE QTRP1-WSBEG-ACCTNO = TP-ACCTNO + 1
              END-IF

              IF ( TP-LINE = 999 )
                 MOVE TP-LINE           TO QTRP1-WSBEG-LINE
              ELSE
                 COMPUTE QTRP1-WSBEG-LINE   = TP-LINE + 1.

      * STORED PROCEDURE SELECT STATEMENT: 
      *
      *  SELECT TRPFILE.TP_BRNO,
      *         CAST(TRPFILE.TP_DATE    AS VARCHAR(10)),
      *         TRPFILE.TP_CLASS,
      *         TRPFILE.TP_ACCTNO,
      *         TRPFILE.TP_LINE,
      *         TRPFILE.TP_TRCD,
      *         TRPFILE.TP_REFNO,
      *         CAST(TRPFILE.TP_PAYDATE AS VARCHAR(10)),
      *         TRPFILE.TP_TRAMT,
      *         TRPFILE.TP_APCUR,
      *         TRPFILE.TP_GLAMT_1,
      *         TRPFILE.TP_GLAMT_2,
      *         TRPFILE.TP_GLAMT_3,
      *         TRPFILE.TP_INSOURS,
      *         TRPFILE.TP_OTHBR_FG,
      *         TRPFILE.TP_INSCOMM,
      *         TRPFILE.TP_INSCOMP,
      *         TRPFILE.TP_INS_TAX_RB,
      *         TRPFILE.TP_ADDON_TYP,
      *         TRPFILE.TP_ADDON_TXT,
      *         LNFILE.LN_OWNBR,  
      *         LNFILE.LN_ACCTNO,  
      *         LNFILE.LN_CLASS,  
      *         LNFILE.LN_SUBCLASS, 
      *         LNFILE.LN_LAWCODE,  
      *         LNFILE.LN_SSNO1, 
      *         LNFILE.LN_MAKERCD1,
      *         LNFILE.LN_SSNO2, 
      *         LNFILE.LN_MAKERCD2,
      *         LNFILE.LN_SSNO3, 
      *         LNFILE.LN_MAKERCD3, 
      *         LNFILE.LN_SSNO4,  
      *         LNFILE.LN_MAKERCD4, 
      *         LNFILE.LN_ORGST, 
      *         LNFILE.LN_SPRCLASS,  
      *         LNFILE.LN_STATFLAGS,
      *         CAST(LNFILE.LN_ORIG_1STPYDATE  AS VARCHAR(10)), 
      *         CAST(LNFILE.LN_1STPYDATE       AS VARCHAR(10)), 
      *         LNFILE.LN_ORGTERM,  
      *         LNFILE.LN_INSURED, 
      *         LNFILE.LN_INSTYPES, 
      *         LNFILE.LN_INSCOMP_1,  
      *         LNFILE.LN_INSCOMP_2,  
      *         LNFILE.LN_INSCOMP_3,  
      *         LNFILE.LN_INSCOMP_4,  
      *         LNFILE.LN_INSCOMP_5,  
      *         LNFILE.LN_INSCOMP_6,  
      *         LNFILE.LN_INSCOMP_7,  
      *         LNFILE.LN_INSCOMP_8,  
      *         LNFILE.LN_INSCOMP_9,  
      *         LNFILE.LN_INSCOMP_10,  
      *         CAST(LNFILE.LN_INSEFF_DATE_1  AS VARCHAR(10)), 
      *         CAST(LNFILE.LN_INSEFF_DATE_2  AS VARCHAR(10)), 
      *         CAST(LNFILE.LN_INSEFF_DATE_3  AS VARCHAR(10)), 
      *         CAST(LNFILE.LN_INSEFF_DATE_4  AS VARCHAR(10)), 
      *         CAST(LNFILE.LN_INSEFF_DATE_5  AS VARCHAR(10)), 
      *         CAST(LNFILE.LN_INSEFF_DATE_6  AS VARCHAR(10)), 
      *         CAST(LNFILE.LN_INSEFF_DATE_7  AS VARCHAR(10)), 
      *         CAST(LNFILE.LN_INSEFF_DATE_8  AS VARCHAR(10)), 
      *         CAST(LNFILE.LN_INSEFF_DATE_9  AS VARCHAR(10)), 
      *         CAST(LNFILE.LN_INSEFF_DATE_10 AS VARCHAR(10)), 
      *         CAST(LNFILE.LN_INSEXP_DATE_1  AS VARCHAR(10)), 
      *         CAST(LNFILE.LN_INSEXP_DATE_2  AS VARCHAR(10)), 
      *         CAST(LNFILE.LN_INSEXP_DATE_3  AS VARCHAR(10)), 
      *         CAST(LNFILE.LN_INSEXP_DATE_4  AS VARCHAR(10)), 
      *         CAST(LNFILE.LN_INSEXP_DATE_5  AS VARCHAR(10)), 
      *         CAST(LNFILE.LN_INSEXP_DATE_6  AS VARCHAR(10)), 
      *         CAST(LNFILE.LN_INSEXP_DATE_7  AS VARCHAR(10)), 
      *         CAST(LNFILE.LN_INSEXP_DATE_8  AS VARCHAR(10)), 
      *         CAST(LNFILE.LN_INSEXP_DATE_9  AS VARCHAR(10)), 
      *         CAST(LNFILE.LN_INSEXP_DATE_10 AS VARCHAR(10)), 
      *         LNFILE.LN_INSCOVR_1,  
      *         LNFILE.LN_INSCOVR_2,  
      *         LNFILE.LN_INSCOVR_3,  
      *         LNFILE.LN_INSCOVR_4,  
      *         LNFILE.LN_INSCOVR_5,  
      *         LNFILE.LN_INSCOVR_6,  
      *         LNFILE.LN_INSCOVR_7,  
      *         LNFILE.LN_INSCOVR_8,  
      *         LNFILE.LN_INSCOVR_9,  
      *         LNFILE.LN_INSCOVR_10, 
      *         LNFILE.LN_TOTNODEF,  
      *         LNFILE.LN_CL_REBATE, 
      *         LNFILE.LN_AH_REBATE, 
      *         LNFILE.LN_PP_REBATE, 
      *         LNFILE.LN_INT_REBATE,
      *         LNFILE.LN_SC_REBATE, 
      *         LNFILE.LN_MF_REBATE, 
      *         LNFILE.LN_DF_REBATE, 
      *         LNFILE.LN_EX_REBATE, 
      *         LNFILE.LN_O1_REBATE, 
      *         LNFILE.LN_O2_REBATE, 
      *         LNFILE.LN_O3_REBATE, 
      *         LNFILE.LN_O4_REBATE, 
      *         LNFILE.LN_O5_REBATE, 
      *         LNFILE.LN_O6_REBATE, 
      *         LNFILE.LN_O7_REBATE, 
      *         LNFILE.LN_UN_O7_REB, 
      *         LNFILE.LN_UN_O8_REB, 
      *         LNFILE.LN_UN_O9_REB, 
      *         LNFILE.LN_UN_O10_REB, 
      *         LNFILE.LN_SUM_ORGTERM,  
      *         LNFILE.LN_UNITPER_CD, 
      *         LNFILE.LN_UNITPER_FREQ 
      *    FROM DBO.TRPFILE TRPFILE
      *      INNER JOIN
      *         DBO.LNFILE  LNFILE
      *         ON ( LNFILE.LN_OWNBR        = TRPFILE.TP_BRNO
      *        AND   LNFILE.LN_ACCTNO       = TRPFILE.TP_ACCTNO
      *        AND   LNFILE.LN_ORGST  BETWEEN @QWS_BEG_ST
      *                                   AND @QWS_END_ST )
      *      WHERE ( TRPFILE.TP_BRNO        = @QTRP1_WBEG_BRNO   
      *        AND   TRPFILE.TP_DATE  BETWEEN @QTRP1_WBEG_DATE
      *                                   AND @QTRP1_WEND_DATE  
      *        AND   TRPFILE.TP_CLASS BETWEEN @QTRP1_WBEG_CLASS
      *                                   AND @QTRP1_WEND_CLASS )
      *        AND ( TRPFILE.TP_ADDON_TYP   = 'O3'
      *         OR ( TRPFILE.TP_ADDON_TYP  != 'O3' 
      *        AND   TRPFILE.TP_TRCD       IN ('R3', 'C3'))) 
      *        AND ( TRPFILE.TP_OTHBR_FG   != 'C' ) 
      *   ORDER BY TRPFILE.TP_BRNO,
      *            TRPFILE.TP_DATE,
      *            TRPFILE.TP_CLASS,
      *            TRPFILE.TP_ACCTNO,
      *            TRPFILE.TP_LINE

           EXEC SQL SET CONNECTION TRP1_CONNECT END-EXEC.

           EXEC SQL
            DECLARE TRP1_CURSOR CURSOR FOR
                :TRP1-RETURN-CODE = EXEC SSP_O3MORP_TRPFILE_SELECT
              ( :QTRP1-WBEG-BRNO,   
                :QTRP1-WBEG-DATE,   
                :QTRP1-WEND-DATE,   
                :QTRP1-WBEG-CLASS,  
                :QTRP1-WEND-CLASS,
                :QWS-BEG-ST,
                :QWS-END-ST )
              
           END-EXEC.

           EXEC SQL
               OPEN TRP1_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE ZEROES TO QTRP1-WBEG-BRNO
                          QTRP1-WEND-BRNO
                          QTRP1-WBEG-CLASS
                          QTRP1-WEND-CLASS
                          QTRP1-WBEG-ACCTNO
                          QTRP1-WEND-ACCTNO
                          QTRP1-WBEG-LINE
                          QTRP1-WEND-LINE
                          QTRP1-WSBEG-CLASS
                          QTRP1-WSBEG-ACCTNO
                          QTRP1-WSBEG-LINE.
           MOVE SPACES TO QTRP1-WBEG-DATE
                          QTRP1-WEND-DATE
                          QTRP1-WSBEG-DATE.

           MOVE STAT---GOOD TO TRP1-CURSOR-STAT.

           EXEC SQL SET CONNECTION C1 END-EXEC.

      *-----------------------------------------------------------------
       READ-TRP1-FILE-NEXT. *> EMBEDDED - TRP,LN
           PERFORM READ-IT.
           MOVE TRP-PATH-OWNBR  TO TP-BRNO.
           MOVE TRP-PATH-SQL    TO E-FILE.
           MOVE TRP1-KEY        TO E-KEYX.
           INITIALIZE QTRP-REC.

           EXEC SQL SET CONNECTION TRP1_CONNECT END-EXEC.

           IF ( TRP1-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH TRP1_CURSOR INTO  
                      :QTP-BRNO,
                      :QTP-DATE,
                      :QTP-CLASS,
                      :QTP-ACCTNO,
                      :QTP-LINE,
                      :QTP-TRCD,
                      :QTP-REFNO,
                      :QTP-PAYDATE,
                      :QTP-TRAMT,
                      :QTP-APCUR,
                      :QTP-GLAMT-1,
                      :QTP-GLAMT-2,
                      :QTP-GLAMT-3,
                      :QTP-INSOURS,
                      :QTP-OTHBR-FG,
                      :QTP-INSCOMM,
                      :QTP-INSCOMP,
                      :QTP-INS-TAX-RB,
                      :QTP-ADDON-TYP,
                      :QTP-ADDON-TXT,
                      :QLN-OWNBR,  
                      :QLN-ACCTNO, 
                      :QLN-CLASS, 
                      :QLN-SUBCLASS, 
                      :QLN-LAWCODE, 
                      :QLN-SSNO1, 
                      :QLN-MAKERCD1,
                      :QLN-SSNO2, 
                      :QLN-MAKERCD2,
                      :QLN-SSNO3, 
                      :QLN-MAKERCD3,
                      :QLN-SSNO4, 
                      :QLN-MAKERCD4,
                      :QLN-ORGST,
                      :QLN-SPRCLASS, 
                      :QLN-STATFLAGS,
                      :QLN-ORIG-1STPYDATE,
                      :QLN-1STPYDATE,                      
                      :QLN-ORGTERM,
                      :QLN-INSURED,
                      :QLN-INSTYPES, 
                      :QLN-INSCOMP-1, 
                      :QLN-INSCOMP-2, 
                      :QLN-INSCOMP-3, 
                      :QLN-INSCOMP-4, 
                      :QLN-INSCOMP-5, 
                      :QLN-INSCOMP-6, 
                      :QLN-INSCOMP-7, 
                      :QLN-INSCOMP-8, 
                      :QLN-INSCOMP-9, 
                      :QLN-INSCOMP-10, 
                      :QLN-INSEFF-DATE-1,
                      :QLN-INSEFF-DATE-2,
                      :QLN-INSEFF-DATE-3,
                      :QLN-INSEFF-DATE-4,
                      :QLN-INSEFF-DATE-5,
                      :QLN-INSEFF-DATE-6,
                      :QLN-INSEFF-DATE-7,
                      :QLN-INSEFF-DATE-8,
                      :QLN-INSEFF-DATE-9,
                      :QLN-INSEFF-DATE-10,
                      :QLN-INSEXP-DATE-1,
                      :QLN-INSEXP-DATE-2,
                      :QLN-INSEXP-DATE-3,
                      :QLN-INSEXP-DATE-4,
                      :QLN-INSEXP-DATE-5,
                      :QLN-INSEXP-DATE-6,
                      :QLN-INSEXP-DATE-7,
                      :QLN-INSEXP-DATE-8,
                      :QLN-INSEXP-DATE-9,
                      :QLN-INSEXP-DATE-10,
                      :QLN-INSCOVR-1,  
                      :QLN-INSCOVR-2,  
                      :QLN-INSCOVR-3,  
                      :QLN-INSCOVR-4,  
                      :QLN-INSCOVR-5,  
                      :QLN-INSCOVR-6,  
                      :QLN-INSCOVR-7,  
                      :QLN-INSCOVR-8,  
                      :QLN-INSCOVR-9,  
                      :QLN-INSCOVR-10, 
                      :QLN-TOTNODEF,  
                      :QLN-CL-REBATE, 
                      :QLN-AH-REBATE, 
                      :QLN-PP-REBATE, 
                      :QLN-INT-REBATE, 
                      :QLN-SC-REBATE, 
                      :QLN-MF-REBATE, 
                      :QLN-DF-REBATE, 
                      :QLN-EX-REBATE, 
                      :QLN-O1-REBATE, 
                      :QLN-O2-REBATE, 
                      :QLN-O3-REBATE, 
                      :QLN-O4-REBATE, 
                      :QLN-O5-REBATE, 
                      :QLN-O6-REBATE, 
                      :QLN-O7-REBATE, 
                      :QLN-UN-O7-REB, 
                      :QLN-UN-O8-REB, 
                      :QLN-UN-O9-REB, 
                      :QLN-UN-O10-REB, 
                      :QLN-SUM-ORGTERM, 
                      :QLN-UNITPER-CD,
                      :QLN-UNITPER-FREQ 
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-TRP1-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-TRP-FIELDS.

           EXEC SQL SET CONNECTION C1 END-EXEC.

      *-----------------------------------------------------------------
       CLOSE-TRP1-FILE.

           PERFORM CLOSE-IT.
           IF (( TRP1-CURSOR-STAT-GOOD ) AND
               ( TRP1-CONN-OPEN-SW = "Y" ))
              EXEC SQL SET CONNECTION TRP1_CONNECT  END-EXEC
              EXEC SQL
                  CLOSE TRP1_CURSOR
              END-EXEC
              MOVE STAT---BAD TO TRP1-CURSOR-STAT  
              EXEC SQL SET CONNECTION C1 END-EXEC.

      *-----------------------------------------------------------------
       CLOSE-TRP1-CONNECTION.

           IF TRP1-CONN-OPEN-SW = "Y"
              EXEC SQL SET CONNECTION TRP1_CONNECT END-EXEC
              EXEC SQL COMMIT END-EXEC
              EXEC SQL DISCONNECT TRP1_CONNECT END-EXEC
              EXEC SQL SET CONNECTION C1 END-EXEC
              MOVE "N" TO TRP1-CONN-OPEN-SW.


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

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       START-TRL1-FILE. *> EMBEDDED - TRL,LN
           PERFORM START-IT.
           MOVE TRL-PATH-OWNBR  TO TL-BRNO.
           MOVE TRL-PATH-SQL    TO E-FILE.
           MOVE TRL1-KEY        TO E-KEYX.

           IF ( TRL1-CURSOR-STAT-GOOD )
              PERFORM CLOSE-TRL1-FILE.

           IF ( QTRL1-WEND-LINE = ZEROES    ) OR
              ( QTRL1-WEND-LINE NOT NUMERIC )
              MOVE TRL-PATH-OWNBR       TO QTRL1-WBEG-BRNO
              MOVE TRL-PATH-OWNBR       TO QTRL1-WEND-BRNO
              MOVE TL-DATE              TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD  TO QTRL1-WBEG-DATE
              MOVE EXT-JULIAN-END       TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD  TO QTRL1-WEND-DATE
              MOVE TL-CLASS             TO QTRL1-WBEG-CLASS
              MOVE TL-ACCTNO            TO QTRL1-WBEG-ACCTNO
              MOVE TL-LAWCODE           TO QTRL1-WBEG-LAWCODE
              MOVE TL-LINE              TO QTRL1-WBEG-LINE
              MOVE ALL "9"              TO QTRL1-WEND-CLASS
                                           QTRL1-WEND-ACCTNO
                                           QTRL1-WEND-LAWCODE
                                           QTRL1-WEND-LINE.

           EXEC SQL
            DECLARE TRL1_CURSOR CURSOR FOR
            SELECT TRLFILE.TL_BRNO,
                   CAST(TRLFILE.TL_DATE AS VARCHAR(10)),
                   TRLFILE.TL_CLASS,
                   TRLFILE.TL_ACCTNO,
                   TRLFILE.TL_LAWCODE,
                   TRLFILE.TL_LINE,
                   TRLFILE.TL_INSCOMP_1,
                   TRLFILE.TL_INSCOMP_2,
                   TRLFILE.TL_INSCOMP_3,
                   TRLFILE.TL_INSCOMP_4,
                   TRLFILE.TL_INSCOMP_5,
                   TRLFILE.TL_INSCOMP_6,
                   TRLFILE.TL_INSCOMP_7,
                   TRLFILE.TL_INSCOMP_8,
                   TRLFILE.TL_INSCOMP_9,
                   TRLFILE.TL_INSCOMP_10,
                   TRLFILE.TL_INSOURS_1, 
                   TRLFILE.TL_INSOURS_2, 
                   TRLFILE.TL_INSOURS_3, 
                   TRLFILE.TL_INSOURS_4, 
                   TRLFILE.TL_INSOURS_5, 
                   TRLFILE.TL_INSOURS_6, 
                   TRLFILE.TL_INSOURS_7, 
                   TRLFILE.TL_INSOURS_8, 
                   TRLFILE.TL_INSOURS_9, 
                   TRLFILE.TL_INSOURS_10,
                   TRLFILE.TL_INSPREM_1,
                   TRLFILE.TL_INSPREM_2,
                   TRLFILE.TL_INSPREM_3,
                   TRLFILE.TL_INSPREM_4,
                   TRLFILE.TL_INSPREM_5,
                   TRLFILE.TL_INSPREM_6,
                   TRLFILE.TL_INSPREM_7,
                   TRLFILE.TL_INSPREM_8,
                   TRLFILE.TL_INSPREM_9,
                   TRLFILE.TL_INSPREM_10,
                   TRLFILE.TL_INSCOMM_1,
                   TRLFILE.TL_INSCOMM_2,
                   TRLFILE.TL_INSCOMM_3,
                   TRLFILE.TL_INSCOMM_4,
                   TRLFILE.TL_INSCOMM_5,
                   TRLFILE.TL_INSCOMM_6,
                   TRLFILE.TL_INSCOMM_7,
                   TRLFILE.TL_INSCOMM_8,
                   TRLFILE.TL_INSCOMM_9,
                   TRLFILE.TL_INSCOMM_10,
                   LNFILE.LN_OWNBR,  
                   LNFILE.LN_ACCTNO,  
                   LNFILE.LN_CLASS,  
                   LNFILE.LN_SUBCLASS, 
                   LNFILE.LN_LAWCODE,  
                   LNFILE.LN_SSNO1, 
                   LNFILE.LN_MAKERCD1,
                   LNFILE.LN_SSNO2, 
                   LNFILE.LN_MAKERCD2,
                   LNFILE.LN_SSNO3, 
                   LNFILE.LN_MAKERCD3, 
                   LNFILE.LN_SSNO4,  
                   LNFILE.LN_MAKERCD4, 
                   LNFILE.LN_ORGST, 
                   LNFILE.LN_SPRCLASS,  
                   LNFILE.LN_STATFLAGS,
                   CAST(LNFILE.LN_ORIG_1STPYDATE  AS VARCHAR(10)), 
                   CAST(LNFILE.LN_1STPYDATE       AS VARCHAR(10)), 
                   LNFILE.LN_ORGTERM,  
                   LNFILE.LN_INSURED, 
                   LNFILE.LN_INSTYPES, 
                   LNFILE.LN_INSCOMP_1,  
                   LNFILE.LN_INSCOMP_2,  
                   LNFILE.LN_INSCOMP_3,  
                   LNFILE.LN_INSCOMP_4,  
                   LNFILE.LN_INSCOMP_5,  
                   LNFILE.LN_INSCOMP_6,  
                   LNFILE.LN_INSCOMP_7,  
                   LNFILE.LN_INSCOMP_8,  
                   LNFILE.LN_INSCOMP_9,  
                   LNFILE.LN_INSCOMP_10,  
                   CAST(LNFILE.LN_INSEFF_DATE_1  AS VARCHAR(10)), 
                   CAST(LNFILE.LN_INSEFF_DATE_2  AS VARCHAR(10)), 
                   CAST(LNFILE.LN_INSEFF_DATE_3  AS VARCHAR(10)), 
                   CAST(LNFILE.LN_INSEFF_DATE_4  AS VARCHAR(10)), 
                   CAST(LNFILE.LN_INSEFF_DATE_5  AS VARCHAR(10)), 
                   CAST(LNFILE.LN_INSEFF_DATE_6  AS VARCHAR(10)), 
                   CAST(LNFILE.LN_INSEFF_DATE_7  AS VARCHAR(10)), 
                   CAST(LNFILE.LN_INSEFF_DATE_8  AS VARCHAR(10)), 
                   CAST(LNFILE.LN_INSEFF_DATE_9  AS VARCHAR(10)), 
                   CAST(LNFILE.LN_INSEFF_DATE_10 AS VARCHAR(10)), 
                   CAST(LNFILE.LN_INSEXP_DATE_1  AS VARCHAR(10)), 
                   CAST(LNFILE.LN_INSEXP_DATE_2  AS VARCHAR(10)), 
                   CAST(LNFILE.LN_INSEXP_DATE_3  AS VARCHAR(10)), 
                   CAST(LNFILE.LN_INSEXP_DATE_4  AS VARCHAR(10)), 
                   CAST(LNFILE.LN_INSEXP_DATE_5  AS VARCHAR(10)), 
                   CAST(LNFILE.LN_INSEXP_DATE_6  AS VARCHAR(10)), 
                   CAST(LNFILE.LN_INSEXP_DATE_7  AS VARCHAR(10)), 
                   CAST(LNFILE.LN_INSEXP_DATE_8  AS VARCHAR(10)), 
                   CAST(LNFILE.LN_INSEXP_DATE_9  AS VARCHAR(10)), 
                   CAST(LNFILE.LN_INSEXP_DATE_10 AS VARCHAR(10)), 
                   LNFILE.LN_INSCOVR_1,  
                   LNFILE.LN_INSCOVR_2,  
                   LNFILE.LN_INSCOVR_3,  
                   LNFILE.LN_INSCOVR_4,  
                   LNFILE.LN_INSCOVR_5,  
                   LNFILE.LN_INSCOVR_6,  
                   LNFILE.LN_INSCOVR_7,  
                   LNFILE.LN_INSCOVR_8,  
                   LNFILE.LN_INSCOVR_9,  
                   LNFILE.LN_INSCOVR_10, 
                   LNFILE.LN_TOTNODEF,  
                   LNFILE.LN_CL_REBATE, 
                   LNFILE.LN_AH_REBATE, 
                   LNFILE.LN_PP_REBATE, 
                   LNFILE.LN_INT_REBATE,
                   LNFILE.LN_SC_REBATE, 
                   LNFILE.LN_MF_REBATE, 
                   LNFILE.LN_DF_REBATE, 
                   LNFILE.LN_EX_REBATE, 
                   LNFILE.LN_O1_REBATE, 
                   LNFILE.LN_O2_REBATE, 
                   LNFILE.LN_O3_REBATE, 
                   LNFILE.LN_O4_REBATE, 
                   LNFILE.LN_O5_REBATE, 
                   LNFILE.LN_O6_REBATE, 
                   LNFILE.LN_O7_REBATE, 
                   LNFILE.LN_UN_O7_REB, 
                   LNFILE.LN_UN_O8_REB, 
                   LNFILE.LN_UN_O9_REB, 
                   LNFILE.LN_UN_O10_REB, 
                   LNFILE.LN_SUM_ORGTERM,  
                   LNFILE.LN_UNITPER_CD, 
                   LNFILE.LN_UNITPER_FREQ 
              FROM DBO.TRLFILE TRLFILE  
             INNER JOIN
                DBO.LNFILE  LNFILE
                ON ( LNFILE.LN_OWNBR        = TRLFILE.TL_BRNO
               AND   LNFILE.LN_ACCTNO       = TRLFILE.TL_ACCTNO
               AND   LNFILE.LN_ORGST  BETWEEN :QWS-BEG-ST
                                          AND :QWS-END-ST )  
             WHERE ( TRLFILE.TL_BRNO        = :QTRL1-WBEG-BRNO
               AND   TRLFILE.TL_DATE  BETWEEN :QTRL1-WBEG-DATE
                                          AND :QTRL1-WEND-DATE
               AND   TRLFILE.TL_CLASS BETWEEN :QTRL1-WBEG-CLASS
                                          AND :QTRL1-WEND-CLASS )
               AND ( TRLFILE.TL_INSCOMP_6  <> 0 )
               AND ( TRLFILE.TL_INSCOMP_6 
                                      BETWEEN :QWS-BEG-IC
                                          AND :QWS-END-IC )
               AND ( TRLFILE.TL_INSOURS_6   = 'Y' )
             ORDER BY TRLFILE.TL_BRNO,
                      TRLFILE.TL_DATE,
                      TRLFILE.TL_CLASS,
                      TRLFILE.TL_ACCTNO,
                      TRLFILE.TL_LAWCODE,
                      TRLFILE.TL_LINE
           END-EXEC.

           EXEC SQL
               OPEN TRL1_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE ZEROES TO QTRL1-WBEG-BRNO
                          QTRL1-WEND-BRNO
                          QTRL1-WBEG-CLASS
                          QTRL1-WEND-CLASS
                          QTRL1-WBEG-ACCTNO
                          QTRL1-WEND-ACCTNO
                          QTRL1-WBEG-LAWCODE
                          QTRL1-WEND-LAWCODE
                          QTRL1-WBEG-LINE
                          QTRL1-WEND-LINE.
           MOVE SPACES TO QTRL1-WBEG-DATE
                          QTRL1-WEND-DATE.


           MOVE STAT---GOOD TO TRL1-CURSOR-STAT.

      *-----------------------------------------------------------------
       READ-TRL1-FILE-NEXT. *> EMBEDDED - TRL,LN
           PERFORM READ-IT.
           MOVE TRL-PATH-OWNBR  TO TL-BRNO.
           MOVE TRL-PATH-SQL    TO E-FILE.
           MOVE TRL1-KEY        TO E-KEYX.
           INITIALIZE QTRL-REC.
           INITIALIZE QLN-REC.

           IF ( TRL1-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH TRL1_CURSOR INTO  
                     :QTL-BRNO,
                     :QTL-DATE,
                     :QTL-CLASS,
                     :QTL-ACCTNO,
                     :QTL-LAWCODE,
                     :QTL-LINE,
                     :QTL-INSCOMP-1,
                     :QTL-INSCOMP-2,
                     :QTL-INSCOMP-3,
                     :QTL-INSCOMP-4,
                     :QTL-INSCOMP-5,
                     :QTL-INSCOMP-6,
                     :QTL-INSCOMP-7,
                     :QTL-INSCOMP-8,
                     :QTL-INSCOMP-9,
                     :QTL-INSCOMP-10,
                     :QTL-INSOURS-1, 
                     :QTL-INSOURS-2, 
                     :QTL-INSOURS-3, 
                     :QTL-INSOURS-4, 
                     :QTL-INSOURS-5, 
                     :QTL-INSOURS-6, 
                     :QTL-INSOURS-7, 
                     :QTL-INSOURS-8, 
                     :QTL-INSOURS-9, 
                     :QTL-INSOURS-10, 
                     :QTL-INSPREM-1,
                     :QTL-INSPREM-2,
                     :QTL-INSPREM-3,
                     :QTL-INSPREM-4,
                     :QTL-INSPREM-5,
                     :QTL-INSPREM-6,
                     :QTL-INSPREM-7,
                     :QTL-INSPREM-8,
                     :QTL-INSPREM-9,
                     :QTL-INSPREM-10,
                     :QTL-INSCOMM-1,
                     :QTL-INSCOMM-2,
                     :QTL-INSCOMM-3,
                     :QTL-INSCOMM-4,
                     :QTL-INSCOMM-5,
                     :QTL-INSCOMM-6,
                     :QTL-INSCOMM-7,
                     :QTL-INSCOMM-8,
                     :QTL-INSCOMM-9,
                     :QTL-INSCOMM-10,
                     :QLN-OWNBR,
                     :QLN-ACCTNO,
                     :QLN-CLASS,
                     :QLN-SUBCLASS,
                     :QLN-LAWCODE,
                     :QLN-SSNO1,
                     :QLN-MAKERCD1,
                     :QLN-SSNO2,
                     :QLN-MAKERCD2,
                     :QLN-SSNO3,
                     :QLN-MAKERCD3,
                     :QLN-SSNO4,
                     :QLN-MAKERCD4,
                     :QLN-ORGST,
                     :QLN-SPRCLASS,
                     :QLN-STATFLAGS,
                     :QLN-ORIG-1STPYDATE,
                     :QLN-1STPYDATE,
                     :QLN-ORGTERM,
                     :QLN-INSURED,
                     :QLN-INSTYPES,
                     :QLN-INSCOMP-1,
                     :QLN-INSCOMP-2,
                     :QLN-INSCOMP-3,
                     :QLN-INSCOMP-4,
                     :QLN-INSCOMP-5,
                     :QLN-INSCOMP-6,
                     :QLN-INSCOMP-7,
                     :QLN-INSCOMP-8,
                     :QLN-INSCOMP-9,
                     :QLN-INSCOMP-10,
                     :QLN-INSEFF-DATE-1,
                     :QLN-INSEFF-DATE-2,
                     :QLN-INSEFF-DATE-3,
                     :QLN-INSEFF-DATE-4,
                     :QLN-INSEFF-DATE-5,
                     :QLN-INSEFF-DATE-6,
                     :QLN-INSEFF-DATE-7,
                     :QLN-INSEFF-DATE-8,
                     :QLN-INSEFF-DATE-9,
                     :QLN-INSEFF-DATE-10,
                     :QLN-INSEXP-DATE-1,
                     :QLN-INSEXP-DATE-2,
                     :QLN-INSEXP-DATE-3,
                     :QLN-INSEXP-DATE-4,
                     :QLN-INSEXP-DATE-5,
                     :QLN-INSEXP-DATE-6,
                     :QLN-INSEXP-DATE-7,
                     :QLN-INSEXP-DATE-8,
                     :QLN-INSEXP-DATE-9,
                     :QLN-INSEXP-DATE-10,
                     :QLN-INSCOVR-1,
                     :QLN-INSCOVR-2,
                     :QLN-INSCOVR-3,
                     :QLN-INSCOVR-4,
                     :QLN-INSCOVR-5,
                     :QLN-INSCOVR-6,
                     :QLN-INSCOVR-7,
                     :QLN-INSCOVR-8,
                     :QLN-INSCOVR-9,
                     :QLN-INSCOVR-10,
                     :QLN-TOTNODEF,
                     :QLN-CL-REBATE,
                     :QLN-AH-REBATE,
                     :QLN-PP-REBATE,
                     :QLN-INT-REBATE, 
                     :QLN-SC-REBATE, 
                     :QLN-MF-REBATE, 
                     :QLN-DF-REBATE, 
                     :QLN-EX-REBATE, 
                     :QLN-O1-REBATE, 
                     :QLN-O2-REBATE, 
                     :QLN-O3-REBATE, 
                     :QLN-O4-REBATE, 
                     :QLN-O5-REBATE, 
                     :QLN-O6-REBATE, 
                     :QLN-O7-REBATE, 
                     :QLN-UN-O7-REB, 
                     :QLN-UN-O8-REB, 
                     :QLN-UN-O9-REB, 
                     :QLN-UN-O10-REB, 
                     :QLN-SUM-ORGTERM,
                     :QLN-UNITPER-CD,
                     :QLN-UNITPER-FREQ	
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-TRL1-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-TRL-FIELDS
              PERFORM GET-LN-FIELDS.

      *-----------------------------------------------------------------
       CLOSE-TRL1-FILE.

           PERFORM CLOSE-IT.
           IF ( TRL1-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE TRL1_CURSOR
              END-EXEC
              MOVE STAT---BAD TO TRL1-CURSOR-STAT. 


      * COPY "LIBLP/LPTRS1RN.CPY".
      * COPYMEMBER: LIBLP/LPTS1RN
       LOAD-TRS1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( TRS-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( TRS-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO TRS-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO TRS-ALL-MACHINE
              MOVE EXT-FILPATH-DATA        TO TRS-ALL-DATA
              MOVE TRS-ALL-PATH             TO TRS-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( TRS-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO TRS-ALL-BASE
              MOVE FILPATH-MACHINE         TO TRS-ALL-MACHINE
              MOVE FILPATH-DATA            TO TRS-ALL-DATA
              MOVE TRS-ALL-PATH             TO TRS-PATH.

      *-----------------------------------------------------------------
       OPEN-TRS1-FILE.
           PERFORM LOAD-TRS1-FILE.
           PERFORM OPEN-IT.
           MOVE TRS-PATH-SQL TO E-FILE.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       START-TRS1-FILE. *> EMBEDDED - TRS,LN
           PERFORM START-IT.
           MOVE TRS-PATH-OWNBR  TO TS-BRNO.
           MOVE TRS-PATH-SQL    TO E-FILE.
           MOVE TRS1-KEY        TO E-KEYX.

           IF ( TRS1-CURSOR-STAT-GOOD )
              PERFORM CLOSE-TRS1-FILE.

           IF ( QTRS1-WEND-LINE = ZEROES    ) OR
              ( QTRS1-WEND-LINE NOT NUMERIC )
              MOVE TRS-PATH-OWNBR       TO QTRS1-WBEG-BRNO
              MOVE TRS-PATH-OWNBR       TO QTRS1-WEND-BRNO
              MOVE TS-DATE              TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD  TO QTRS1-WBEG-DATE
              MOVE EXT-JULIAN-END       TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD  TO QTRS1-WEND-DATE
              MOVE TS-CLASS             TO QTRS1-WBEG-CLASS
              MOVE TS-ACCTNO            TO QTRS1-WBEG-ACCTNO
              MOVE TS-LAWCODE           TO QTRS1-WBEG-LAWCODE
              MOVE TS-LINE              TO QTRS1-WBEG-LINE
              MOVE ALL "9"              TO QTRS1-WEND-CLASS
                                           QTRS1-WEND-ACCTNO
                                           QTRS1-WEND-LAWCODE
                                           QTRS1-WEND-LINE.

           EXEC SQL		   
            DECLARE TRS1_CURSOR CURSOR FOR
            SELECT TRSFILE.TS_BRNO,
                   CAST(TRSFILE.TS_DATE AS VARCHAR(10)),
                   TRSFILE.TS_CLASS,
                   TRSFILE.TS_ACCTNO,
                   TRSFILE.TS_LAWCODE,
                   TRSFILE.TS_LINE,
                   TRSFILE.TS_INSCOMP_1,
                   TRSFILE.TS_INSCOMP_2,
                   TRSFILE.TS_INSCOMP_3,
                   TRSFILE.TS_INSCOMP_4,
                   TRSFILE.TS_INSCOMP_5,
                   TRSFILE.TS_INSCOMP_6,
                   TRSFILE.TS_INSCOMP_7,
                   TRSFILE.TS_INSCOMP_8,
                   TRSFILE.TS_INSCOMP_9,
                   TRSFILE.TS_INSCOMP_10,
                   TRSFILE.TS_INSOURS_1, 
                   TRSFILE.TS_INSOURS_2, 
                   TRSFILE.TS_INSOURS_3, 
                   TRSFILE.TS_INSOURS_4, 
                   TRSFILE.TS_INSOURS_5, 
                   TRSFILE.TS_INSOURS_6, 
                   TRSFILE.TS_INSOURS_7, 
                   TRSFILE.TS_INSOURS_8, 
                   TRSFILE.TS_INSOURS_9, 
                   TRSFILE.TS_INSOURS_10, 
                   TRSFILE.TS_INSPREM_1,
                   TRSFILE.TS_INSPREM_2,
                   TRSFILE.TS_INSPREM_3,
                   TRSFILE.TS_INSPREM_4,
                   TRSFILE.TS_INSPREM_5,
                   TRSFILE.TS_INSPREM_6,
                   TRSFILE.TS_INSPREM_7,
                   TRSFILE.TS_INSPREM_8,
                   TRSFILE.TS_INSPREM_9,
                   TRSFILE.TS_INSPREM_10,
                   TRSFILE.TS_INSCOMM_1,
                   TRSFILE.TS_INSCOMM_2,
                   TRSFILE.TS_INSCOMM_3,
                   TRSFILE.TS_INSCOMM_4,
                   TRSFILE.TS_INSCOMM_5,
                   TRSFILE.TS_INSCOMM_6,
                   TRSFILE.TS_INSCOMM_7,
                   TRSFILE.TS_INSCOMM_8,
                   TRSFILE.TS_INSCOMM_9,
                   TRSFILE.TS_INSCOMM_10,
                   TRSFILE.TS_O3INS_DLPROC,
                   LNFILE.LN_OWNBR,  
                   LNFILE.LN_ACCTNO,  
                   LNFILE.LN_CLASS,  
                   LNFILE.LN_SUBCLASS, 
                   LNFILE.LN_LAWCODE,  
                   LNFILE.LN_SSNO1, 
                   LNFILE.LN_MAKERCD1,
                   LNFILE.LN_SSNO2, 
                   LNFILE.LN_MAKERCD2,
                   LNFILE.LN_SSNO3, 
                   LNFILE.LN_MAKERCD3, 
                   LNFILE.LN_SSNO4,  
                   LNFILE.LN_MAKERCD4, 
                   LNFILE.LN_ORGST, 
                   LNFILE.LN_SPRCLASS,  
                   LNFILE.LN_STATFLAGS,
                   CAST(LNFILE.LN_ORIG_1STPYDATE  AS VARCHAR(10)), 
                   CAST(LNFILE.LN_1STPYDATE       AS VARCHAR(10)), 
                   LNFILE.LN_ORGTERM,  
                   LNFILE.LN_INSURED, 
                   LNFILE.LN_INSTYPES, 
                   LNFILE.LN_INSCOMP_1,  
                   LNFILE.LN_INSCOMP_2,  
                   LNFILE.LN_INSCOMP_3,  
                   LNFILE.LN_INSCOMP_4,  
                   LNFILE.LN_INSCOMP_5,  
                   LNFILE.LN_INSCOMP_6,  
                   LNFILE.LN_INSCOMP_7,  
                   LNFILE.LN_INSCOMP_8,  
                   LNFILE.LN_INSCOMP_9,  
                   LNFILE.LN_INSCOMP_10,  
                   CAST(LNFILE.LN_INSEFF_DATE_1  AS VARCHAR(10)), 
                   CAST(LNFILE.LN_INSEFF_DATE_2  AS VARCHAR(10)), 
                   CAST(LNFILE.LN_INSEFF_DATE_3  AS VARCHAR(10)), 
                   CAST(LNFILE.LN_INSEFF_DATE_4  AS VARCHAR(10)), 
                   CAST(LNFILE.LN_INSEFF_DATE_5  AS VARCHAR(10)), 
                   CAST(LNFILE.LN_INSEFF_DATE_6  AS VARCHAR(10)), 
                   CAST(LNFILE.LN_INSEFF_DATE_7  AS VARCHAR(10)), 
                   CAST(LNFILE.LN_INSEFF_DATE_8  AS VARCHAR(10)), 
                   CAST(LNFILE.LN_INSEFF_DATE_9  AS VARCHAR(10)), 
                   CAST(LNFILE.LN_INSEFF_DATE_10 AS VARCHAR(10)), 
                   CAST(LNFILE.LN_INSEXP_DATE_1  AS VARCHAR(10)), 
                   CAST(LNFILE.LN_INSEXP_DATE_2  AS VARCHAR(10)), 
                   CAST(LNFILE.LN_INSEXP_DATE_3  AS VARCHAR(10)), 
                   CAST(LNFILE.LN_INSEXP_DATE_4  AS VARCHAR(10)), 
                   CAST(LNFILE.LN_INSEXP_DATE_5  AS VARCHAR(10)), 
                   CAST(LNFILE.LN_INSEXP_DATE_6  AS VARCHAR(10)), 
                   CAST(LNFILE.LN_INSEXP_DATE_7  AS VARCHAR(10)), 
                   CAST(LNFILE.LN_INSEXP_DATE_8  AS VARCHAR(10)), 
                   CAST(LNFILE.LN_INSEXP_DATE_9  AS VARCHAR(10)), 
                   CAST(LNFILE.LN_INSEXP_DATE_10 AS VARCHAR(10)), 
                   LNFILE.LN_INSCOVR_1,  
                   LNFILE.LN_INSCOVR_2,  
                   LNFILE.LN_INSCOVR_3,  
                   LNFILE.LN_INSCOVR_4,  
                   LNFILE.LN_INSCOVR_5,  
                   LNFILE.LN_INSCOVR_6,  
                   LNFILE.LN_INSCOVR_7,  
                   LNFILE.LN_INSCOVR_8,  
                   LNFILE.LN_INSCOVR_9,  
                   LNFILE.LN_INSCOVR_10, 
                   LNFILE.LN_TOTNODEF,  
                   LNFILE.LN_CL_REBATE, 
                   LNFILE.LN_AH_REBATE, 
                   LNFILE.LN_PP_REBATE, 
                   LNFILE.LN_INT_REBATE,
                   LNFILE.LN_SC_REBATE, 
                   LNFILE.LN_MF_REBATE, 
                   LNFILE.LN_DF_REBATE, 
                   LNFILE.LN_EX_REBATE, 
                   LNFILE.LN_O1_REBATE, 
                   LNFILE.LN_O2_REBATE, 
                   LNFILE.LN_O3_REBATE, 
                   LNFILE.LN_O4_REBATE, 
                   LNFILE.LN_O5_REBATE, 
                   LNFILE.LN_O6_REBATE, 
                   LNFILE.LN_O7_REBATE, 
                   LNFILE.LN_UN_O7_REB, 
                   LNFILE.LN_UN_O8_REB, 
                   LNFILE.LN_UN_O9_REB, 
                   LNFILE.LN_UN_O10_REB, 
                   LNFILE.LN_SUM_ORGTERM,  
                   LNFILE.LN_UNITPER_CD, 
                   LNFILE.LN_UNITPER_FREQ 
              FROM DBO.TRSFILE TRSFILE  
             INNER JOIN
                DBO.LNFILE  LNFILE
                ON ( LNFILE.LN_OWNBR        = TRSFILE.TS_BRNO
               AND   LNFILE.LN_ACCTNO       = TRSFILE.TS_ACCTNO   
               AND   LNFILE.LN_ORGST  BETWEEN :QWS-BEG-ST
                                          AND :QWS-END-ST )
             WHERE ( TRSFILE.TS_BRNO        = :QTRS1-WBEG-BRNO
               AND   TRSFILE.TS_DATE  BETWEEN :QTRS1-WBEG-DATE
                                          AND :QTRS1-WEND-DATE
               AND   TRSFILE.TS_CLASS BETWEEN :QTRS1-WBEG-CLASS
                                          AND :QTRS1-WEND-CLASS )
               AND ( TRSFILE.TS_INSCOMP_6  <> 0 )
               AND ( TRSFILE.TS_INSCOMP_6 
                                      BETWEEN :QWS-BEG-IC
                                          AND :QWS-END-IC )
               AND ( TRSFILE.TS_INSOURS_6   = 'Y' )
             ORDER BY TRSFILE.TS_BRNO,
                      TRSFILE.TS_DATE,
                      TRSFILE.TS_CLASS,
                      TRSFILE.TS_ACCTNO,
                      TRSFILE.TS_LAWCODE,
                      TRSFILE.TS_LINE			  
           END-EXEC.

           EXEC SQL
               OPEN TRS1_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE ZEROES TO QTRS1-WBEG-BRNO
                          QTRS1-WEND-BRNO
                          QTRS1-WBEG-CLASS
                          QTRS1-WEND-CLASS
                          QTRS1-WBEG-ACCTNO
                          QTRS1-WEND-ACCTNO
                          QTRS1-WBEG-LAWCODE
                          QTRS1-WEND-LAWCODE
                          QTRS1-WBEG-LINE
                          QTRS1-WEND-LINE.
           MOVE SPACES TO QTRS1-WBEG-DATE
                          QTRS1-WEND-DATE.


           MOVE STAT---GOOD TO TRS1-CURSOR-STAT.

      *-----------------------------------------------------------------
       READ-TRS1-FILE-NEXT. *> EMBEDDED - TRS,LN
           PERFORM READ-IT.
           MOVE TRS-PATH-OWNBR  TO TS-BRNO.
           MOVE TRS-PATH-SQL    TO E-FILE.
           MOVE TRS1-KEY        TO E-KEYX.
           INITIALIZE QTRS-REC.
           INITIALIZE QLN-REC.

           IF ( TRS1-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH TRS1_CURSOR INTO  
                     :QTS-BRNO, 
                     :QTS-DATE,
                     :QTS-CLASS,
                     :QTS-ACCTNO,
                     :QTS-LAWCODE,
                     :QTS-LINE,
                     :QTS-INSCOMP-1,
                     :QTS-INSCOMP-2,
                     :QTS-INSCOMP-3,
                     :QTS-INSCOMP-4,
                     :QTS-INSCOMP-5,
                     :QTS-INSCOMP-6,
                     :QTS-INSCOMP-7,
                     :QTS-INSCOMP-8,
                     :QTS-INSCOMP-9,
                     :QTS-INSCOMP-10,
                     :QTS-INSOURS-1, 
                     :QTS-INSOURS-2, 
                     :QTS-INSOURS-3, 
                     :QTS-INSOURS-4, 
                     :QTS-INSOURS-5, 
                     :QTS-INSOURS-6, 
                     :QTS-INSOURS-7, 
                     :QTS-INSOURS-8, 
                     :QTS-INSOURS-9, 
                     :QTS-INSOURS-10, 
                     :QTS-INSPREM-1,
                     :QTS-INSPREM-2,
                     :QTS-INSPREM-3,
                     :QTS-INSPREM-4,
                     :QTS-INSPREM-5,
                     :QTS-INSPREM-6,
                     :QTS-INSPREM-7,
                     :QTS-INSPREM-8,
                     :QTS-INSPREM-9,
                     :QTS-INSPREM-10,
                     :QTS-INSCOMM-1,
                     :QTS-INSCOMM-2,
                     :QTS-INSCOMM-3,
                     :QTS-INSCOMM-4,
                     :QTS-INSCOMM-5,
                     :QTS-INSCOMM-6,
                     :QTS-INSCOMM-7,
                     :QTS-INSCOMM-8,
                     :QTS-INSCOMM-9,
                     :QTS-INSCOMM-10,
                     :QTS-O3INS-DLPROC,
                     :QLN-OWNBR,
                     :QLN-ACCTNO,
                     :QLN-CLASS,
                     :QLN-SUBCLASS,
                     :QLN-LAWCODE,
                     :QLN-SSNO1,
                     :QLN-MAKERCD1,
                     :QLN-SSNO2,
                     :QLN-MAKERCD2,
                     :QLN-SSNO3,
                     :QLN-MAKERCD3,
                     :QLN-SSNO4,
                     :QLN-MAKERCD4,
                     :QLN-ORGST,
                     :QLN-SPRCLASS,
                     :QLN-STATFLAGS,
                     :QLN-ORIG-1STPYDATE,
                     :QLN-1STPYDATE,
                     :QLN-ORGTERM,
                     :QLN-INSURED,
                     :QLN-INSTYPES,
                     :QLN-INSCOMP-1,
                     :QLN-INSCOMP-2,
                     :QLN-INSCOMP-3,
                     :QLN-INSCOMP-4,
                     :QLN-INSCOMP-5,
                     :QLN-INSCOMP-6,
                     :QLN-INSCOMP-7,
                     :QLN-INSCOMP-8,
                     :QLN-INSCOMP-9,
                     :QLN-INSCOMP-10,
                     :QLN-INSEFF-DATE-1,
                     :QLN-INSEFF-DATE-2,
                     :QLN-INSEFF-DATE-3,
                     :QLN-INSEFF-DATE-4,
                     :QLN-INSEFF-DATE-5,
                     :QLN-INSEFF-DATE-6,
                     :QLN-INSEFF-DATE-7,
                     :QLN-INSEFF-DATE-8,
                     :QLN-INSEFF-DATE-9,
                     :QLN-INSEFF-DATE-10,
                     :QLN-INSEXP-DATE-1,
                     :QLN-INSEXP-DATE-2,
                     :QLN-INSEXP-DATE-3,
                     :QLN-INSEXP-DATE-4,
                     :QLN-INSEXP-DATE-5,
                     :QLN-INSEXP-DATE-6,
                     :QLN-INSEXP-DATE-7,
                     :QLN-INSEXP-DATE-8,
                     :QLN-INSEXP-DATE-9,
                     :QLN-INSEXP-DATE-10,
                     :QLN-INSCOVR-1,
                     :QLN-INSCOVR-2,
                     :QLN-INSCOVR-3,
                     :QLN-INSCOVR-4,
                     :QLN-INSCOVR-5,
                     :QLN-INSCOVR-6,
                     :QLN-INSCOVR-7,
                     :QLN-INSCOVR-8,
                     :QLN-INSCOVR-9,
                     :QLN-INSCOVR-10,
                     :QLN-TOTNODEF,
                     :QLN-CL-REBATE, 
                     :QLN-AH-REBATE, 
                     :QLN-PP-REBATE, 
                     :QLN-INT-REBATE, 
                     :QLN-SC-REBATE, 
                     :QLN-MF-REBATE, 
                     :QLN-DF-REBATE, 
                     :QLN-EX-REBATE, 
                     :QLN-O1-REBATE, 
                     :QLN-O2-REBATE, 
                     :QLN-O3-REBATE, 
                     :QLN-O4-REBATE, 
                     :QLN-O5-REBATE, 
                     :QLN-O6-REBATE, 
                     :QLN-O7-REBATE, 
                     :QLN-UN-O7-REB, 
                     :QLN-UN-O8-REB, 
                     :QLN-UN-O9-REB, 
                     :QLN-UN-O10-REB,
                     :QLN-SUM-ORGTERM,
                     :QLN-UNITPER-CD,
                     :QLN-UNITPER-FREQ
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-TRS1-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-TRS-FIELDS
              PERFORM GET-LN-FIELDS.

      *-----------------------------------------------------------------
       CLOSE-TRS1-FILE.

           PERFORM CLOSE-IT.
           IF ( TRS1-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE TRS1_CURSOR
              END-EXEC
              MOVE STAT---BAD TO TRS1-CURSOR-STAT.	

      * COPY "LIBLP/LPIC1RN.CPY".
      * COPYMEMBER: LIBLP/LPIC1RN
       LOAD-IC1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( IC-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( IC-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO IC-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO IC-ALL-MACHINE
              MOVE EXT-FILPATH-DATA        TO IC-ALL-DATA
              MOVE IC-ALL-PATH             TO IC-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( IC-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO IC-ALL-BASE
              MOVE FILPATH-MACHINE         TO IC-ALL-MACHINE
              MOVE FILPATH-DATA            TO IC-ALL-DATA
              MOVE IC-ALL-PATH             TO IC-PATH.

      *-----------------------------------------------------------------
       OPEN-IC1-FILE.
           PERFORM LOAD-IC1-FILE.
           PERFORM OPEN-IT.
           MOVE IC-PATH-SQL TO E-FILE.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       READ-IC1-FILE. *> EMBEDDED - ICFILE
           PERFORM READ-IT.
           MOVE IC-PATH-OWNBR  TO IC-BRNO. 
           MOVE IC-PATH-SQL    TO E-FILE.
           MOVE IC1-KEY        TO E-KEYX.
           INITIALIZE QIC-REC.

           MOVE IC-BRNO   TO QIC-BRNO.
           MOVE IC-CODE   TO QIC-CODE.

           EXEC SQL
            SELECT ICFILE.IC_BRNO,
                   ICFILE.IC_CODE,
                   ISNULL(ICFILE.IC_NAME,'')
            INTO  
                   :QIC-BRNO,
                   :QIC-CODE,
                   :QIC-NAME
            FROM DBO.ICFILE
            WHERE ICFILE.IC_BRNO = :QIC-BRNO
              AND ICFILE.IC_CODE = :QIC-CODE
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-IC1-FILE.

           IF ( IO-FG = 0 )
              PERFORM GET-IC-FIELDS.

      *-----------------------------------------------------------------
       CLOSE-IC1-FILE.

           PERFORM CLOSE-IT.

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
       READ-BW1-FILE. *> EMBEDDED - BWFILE
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
                    ISNULL(BWFILE.BW_LNAME,''),
                    ISNULL(BWFILE.BW_FNAME,''),
                    ISNULL(CAST(BWFILE.BW_REFDATE AS VARCHAR(10)),'0'),
                    ISNULL(CAST(BWFILE.BW_BDATE AS VARCHAR(10)),'0'),
                    ISNULL(BWFILE.BW_SOLICIT,'')
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

       COPY "LIBLP/LPSP1RN.CPY".
       COPY "LIBGB/GBWK1IN.CPY".
       COPY "LIBGB/GBWKIN.CPY".

       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO EXT-EOBUF-ERRCD.

      ******************************************************************
       END-PROGRAM SECTION.
      ******************************************************************
       END-RUN.

           IF SV-BEG-BR NOT = 9999
              IF EXT-EOBUF-GROUP-FG = "Y"
                 MOVE SV-BEG-BR TO BEG-BRANCH
                 MOVE SV-END-BR TO END-BRANCH
                 MOVE SV-GROUP-FLAG TO GROUP-FLAG
                 MOVE SV-ENT-GROUP TO ENT-GROUP
              ELSE
                 MOVE SV-BEG-BR TO BEG-BRANCH
                 MOVE SV-END-BR TO END-BRANCH.

           PERFORM CLOSE-FILES.

           IF EXT-ROUTE-BUF = "00"
              PERFORM PRU-OUT.

           IF NO-RECORDS-FOUND
              MOVE "NO REPORT FOR SELECTED RANGE" TO MESS
              PERFORM SEND-MESS.

           MOVE MKTEMP-BUF TO ACCESS-BUF.
           PERFORM REMOVE-WORKFILE.
           MOVE WK1-PATH TO ACCESS-BUF.
           PERFORM REMOVE-WORKFILE.

      *----------------------------------------------------------------
       EXIT-PROG.

           IF EXT-ROUTE-BUF = "00"
              MOVE "LP/EMICMU" TO FORM-NAM
              MOVE FORM-PATH TO FORM-PATHNAME.

       STOP-RUN.
           IF EXT-ROUTE-BUF = "00" OR "40" OR "50"
              DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN
              DESTROY O3MORP-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.
           EXIT PROGRAM.
