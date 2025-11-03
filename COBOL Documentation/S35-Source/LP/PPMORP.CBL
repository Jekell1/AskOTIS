       IDENTIFICATION DIVISION.
       PROGRAM-ID.      PPMORP.
       DATE-WRITTEN.    041486.
      *****************************************************************
      *   MONTHLY PERSONAL PROPERTY INSURANCE
      *       PREMIUM & REFUND REPORT
      *
      *    SITUATIONS REQUIRING REPORTING:
      *
      *    NEW PREMIUMS:
      *        1. NEW LOANS IN THE TRFILE FOR (MMYY)
      *                             TR1-CODE = "L" OR "S"
      *                             TRL-INSOURS(3) = "Y"
      *                             TRS-INSOURS(3) = "Y"
      *
      *    REBATES OR CANCELLATIONS:
      *        1. REBATES/CANCELLATIONS/ADDONS IN THE TRFILE FOR (MMYY)
      *                             TR1-CODE = "P"
      *                             TP-INSOURS = "Y"
      *                             TP-TRCD = "RP" OR "CP"
      *                             TP-ADDON-TYPE = "PP"
      *
      * REV:
      *  090695 BAH WAS NOT GETTING IC FILE FROM EACH FILE SET
      *  100695 JFF ADDED CLASS SCAN
      *  100995 BLL GR-FILE NOT IN DECLARATIVES
      *  110295 MJD CHANGED READ OF LN TO GO TO SORT-TR-NEXT INSTEAD
      *             OF IO-ERROR IF FILE NOT FOUND.
      *  121096 MJD CHANGED TO PERFORM LPSTAT-DISPLAY AFTER
      *             READING BWFILE.
      *  021297 BLV FIX PRINTING INS CO'S OUT OF RANGE
      *  060497 BLV CHANGE TO WORK WITH NEW BATCH DRIVER (BREXEC)
      *  GLF 090397 REMOVED ALL USE OF GLOBAL
      *  BLV 042998 WAS DISPLAYING BEG-IC & END-IC FOR BEG & ENDING CLASS
      *  GLF 082499 ADD ARCHIVE PATH INFO. FOR TR FILES NOT FOUND IN DATA
      *             SET FOR SELECTED MONTH AND YEAR
      *  BLV 990826 FIX ARCHIVE PATH LOGIC
      *  BAH 991103 WK-QTY WAS NOT SIGNED, IF THE FIRST ACCOUNT WAS A VOID,
      *             THE WK-QTY WAS 1 INSTEAD OF -1, CAUSING BAD TOTALS
      *             KEVIN, REGACC
      *  BAH 000621 ADDED THE "AND" TO THIS "IF" IN KEY-DETAIL SECTION
      *             IF LN-ACCTNO = WORK-ACCTNO AND (LN-OWNBR = WORK-OWNBR)
      *             REGENCY, KY0413 LAST ACCT # 500037, AND KY0414 FIRST
      *             ACCT # 500037 AND THE KEY-DETAIL WAS ADDING THESE 2
      *             ACCTS INFO TOGETHER, PL# 287
      *  BAH 010319 ADDED BRANCH SECURITY PASSWORD TESTING (BRSECURE)
      *  010810 BAH ADDED TEST IN THE READ-TR LOOP TO ONLY INCLUDE OWNING
      *             BRANCH RECORDS, ANOTHER BRANCH PAYOFF WITH THE SAME
      *             ACCTNO WAS BEING INCLUDED ON BOTH BRANCHES REPORT ! AJ
      *             OWNING BRANCHES DID MATCH, IT WAS THE WRONG ACCOUNT !!
      *             ONLY FIELD TO TEST IS TP-OTHBR-FG "G" = OWNER "C" = POSTER
      *  BAH 011113 ADDED TP-INSCOMP FOR ADDON/REVERSE/ADDON WITH DIFFERENT
      *             INSURANCE COMPANIES, REGACC PR# 1617
      *  BAH 020501 BUG FROM 010810 CHANGE, TESTED TP-OTHBR ON THE "L" AND "S"
      *             INSTEAD OF JUST "P" RECORDS, ACTUALLY CAUGHT A "C"
      *             IN THE TL-EFFDATE(3) FIELD FOR 04/30/02 STORED AS
      *             00 43 00 2F !! REGWAN PL# 365
      *  BAH 020722 TEST TP-INSOURS INSTEAD OF THE LN-INSOURS(3), ON A
      *             REVERSAL OF AN ADDON, THE LOAN RECORD INSURANCE IS
      *             BLANK (KEVIN)!!
      *  BAH 030714 ADDED SUMMARY-ONLY OPTION OF "I" FOR INSURANCE TOTALS
      *              ONLY, NO BRANCH OR STATE, LENDMARK PR# 2113
      *  BAH 040123 WORLD "KY" INSURANCE TAX WAS PART OF THE REFUND
      *             BEING REPORTED, NEEDED TO BE SUBTRACTED OUT !!
      *                            ((TP-TRAMT - TP-INS-TAX-RB) * -1)
      *   CS 050224 ADDED NET NEW PREMIUMS = (NEW PREM + ADDONS) +
      *             (REFUNDS + CANCELS) + ADJUSTMENTS [WORLD PR#385]
      *  BAH 141015 STACK OVERFLOW, ABORT, SORT-TR HAD A GO TO OUTSIDE
      *             OF ITS OWN SECTION IF TRFILE DID NOT EXIST!
      *  BAH 170712 ACTIVATE 8 DIGIT DATES, PD0006
      *  BAH 181211 GLOBAL LNFILE
      *  BAH 20190919 REMOVED ACCESS-CALL ON LNFILE, TRPFILE, TRL & TRS
      *  BAH 20200827 REMOVED RESET ON FIRST FIELD
      *  MB  20230807 PERFORMANCE TUNING - ADDED STORED PROCEDURE
      *               ADDED STATE PARAMETER ARRAY PROCESSING
      *               LN-REC HAS BEEN ADDED TO THE WK FILE, ALONG WITH
      *               HARDCODED SIZE. IF THE SIZE OF LNFILE TABLE INCREASE,
      *               THE 78 LEVEL ON WK-FILE WILL ALSO NEED TO BE INCREASED.
      *****************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "LIBGB/GBFSPR.CPY".
      * VALID LOANS WORK FILE
       COPY "LIBGB/GBFSWKI.CPY".
      * TOTALS FILE
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

      *************************************
      *   SUMMARY TOTALS WORK RECORD      *
      *************************************
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
           03  WK-AMT        PIC S9(9)V99 COMP-3 OCCURS 7.

      *************************************
      *   WORK FILE FOR VALID LOANS
      *************************************
       78  WK-LN-SIZE                    VALUE 2800. 

       FD  WK-FILE
           LABEL RECORDS ARE STANDARD.
       01  WK-REC.
           03  WK-KEY.
               05  WORK-INSCOMP     PIC 99.
               05  WORK-OWNBR       PIC 9(4).
               05  WORK-CLASS       PIC 9(3).
               05  WORK-ACCTNO      PIC 9(8).
               05  WORK-DATE        PIC 9(8).
           03  WORK-NEW-PP      PIC S9(5)V99 COMP-3.
           03  WORK-ADJ-PP      PIC S9(5)V99 COMP-3.
           03  WORK-CAN-PP      PIC S9(5)V99 COMP-3.
           03  WORK-CAN-PP-DATE PIC 9(8).
           03  WORK-COM-PP      PIC S9(5)V99 COMP-3.
           03  WORK-ADDON-FG    PIC X.
           03  WORK-CANCEL-FG   PIC X.
           03  WORK-LN-REC      PIC X(WK-LN-SIZE).

       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)LP/PPMORP.CBL S35 02/20/24-15:28:32 >".

       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBGB/GB01GR.CPY".
       COPY "LIBGB/GB01GR_SQL.CPY".
       COPY "LIBGB/GBWSGR.CPY".
       COPY "LIBLP/LP01TRL.CPY".
       COPY "LIBLP/LP01TRL_SQL.CPY".
       COPY "LIBLP/LPWSTRL.CPY".
       COPY "LIBLP/LP01TRP.CPY".
       COPY "LIBLP/LP01TRP_SQL.CPY".
       COPY "LIBLP/LPWSTRP.CPY".
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

       01  TRP1-CONN-OPEN-SW    PIC X        VALUE 'N'.
       01  TRL1-CONN-OPEN-SW    PIC X        VALUE 'N'.

       01  WK1-IFN              PIC X(3)     VALUE "WK1".
       01  WK-IFN               PIC X(2)     VALUE "WK".
       01  WK1-PATH             PIC X(35).

      **********************
      *   LINE WORKERS     *
      **********************
       01  LN-WORKERS.
           03  LN-FEED          PIC 999      VALUE 0.
           03  LN-COUNT         PIC 999      VALUE 99.
           03  LN-WK            PIC 999      VALUE 0.
       01  PAGE-NUMBER          PIC 9(4)     VALUE 0.

       01  BR-PAGE-NUMBER       PIC 9(4) COMP-3 VALUE 0.

      **********************
      *   MISC WORKERS     *
      **********************
       01  SUB                  PIC 9(2)     COMP-3.
       01  FIRST-TIME           PIC X        VALUE "Y".
       01  RECORDS-FOUND        PIC X        VALUE " ".
           88  NO-RECORDS-FOUND              VALUE "N".
       01  MESS-LIST            PIC X(5)     VALUE "MSEL.".
       01  NAME-WK              PIC X(34).
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

       01  REC-EXISTS           PIC X        VALUE "N".
       01  TOTAL-FG             PIC X        VALUE "N".
       01  COMPANY-FG           PIC X        VALUE "N".

       01  ONE-PP               PIC S9       VALUE 0.
       01  NEW-COVERAGE         PIC S9(7)V99 COMP-3 VALUE 0.
       01  RPT-END-DATE         PIC 9(8)     COMP-3 VALUE 0.
       01  BEGINNING-DATE       PIC 9(8)     COMP-3 VALUE 0.

       01  PP-ADDON-FG          PIC X.
       01  PP-CANCEL-FG         PIC X.
       01  PP-PREM              PIC S9(7)V99 COMP-3.
       01  PP-ADJ               PIC S9(7)V99 COMP-3.
       01  PP-CAN               PIC S9(7)V99 COMP-3.
       01  PP-CAN-DATE          PIC 9(8)     COMP-3.
       01  PP-COMM              PIC S9(7)V99 COMP-3.

       01  LINE-TOTALS.
           03  TOT-QTY          PIC S9(7)    COMP-3 OCCURS 2.
           03  TOT-AMT          PIC S9(9)V99 COMP-3 OCCURS 7.
       01  CLASS-FG             PIC X(3)      VALUE "   ".
           88  CLASS-BEG                      VALUE "BEG".
           88  CLASS-END                      VALUE "END".
      ***********************************
      *     S C R E E N   F I E L D S   *
      ***********************************
       COPY "LIBGB/EOBUF_EXT.CPY".
           03  BEG-BRANCH      PIC 9(4).
           03  END-BRANCH      PIC 9(4).
           03  BEG-IC          PIC 99.
           03  END-IC          PIC 99.
           03  BEG-CLASS       PIC 99.
           03  END-CLASS       PIC 99.
           03  BEG-ST          PIC XX.
           03  END-ST          PIC XX.
           03  MMYY            PIC 9(4).
           03  MM-YY REDEFINES MMYY.
               05  MM          PIC 9(2).
               05  YY          PIC 9(2).
           03  SUMMARY-ONLY    PIC X.
           03  GROUP-FLAG      PIC X.
           03  ENT-GROUP       PIC X(4).

       01  VERSION-CONTROL     PIC X    VALUE "A".

       01  SV-BEG-BR           PIC 9999   VALUE 9999.
       01  SV-END-BR           PIC 9999   VALUE 9999.
       01  SV-GROUP-FLAG       PIC X     VALUE SPACES.
       01  SV-ENT-GROUP        PIC X(4)  VALUE SPACES.

      ***********************************
      *     P R I N T   B U F F E R S   *
      ***********************************
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

       01  HEAD2.
           03  FILLER           PIC X(10)    VALUE "USER ID:".
           03  H-USERID         PIC X(10) VALUE " ".
           03  FILLER           PIC X(26)    VALUE " ".
           03  FILLER           PIC X(41) VALUE
           "-PERSONAL PROPERTY PREMIUM-REFUND REPORT-".

       01  HEAD3.
           03  H-INSCO          PIC X(10) VALUE "INS CO :".
           03  H-ICNO           PIC 99 VALUE 0.
           03  FILLER           PIC X VALUE " ".
           03  H-ICNAME         PIC X(20) VALUE " ".
           03  FILLER           PIC X(23) VALUE " ".
           03  FILLER           PIC X(13) VALUE "FOR MONTH OF ".
           03  H-MMYY           PIC 99/99.

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
           03  FILLER           PIC X(51) VALUE
           "ACCOUNT #      BORROWER           AGE EFF-DATE TRM ".
           03  FILLER           PIC X(53) VALUE
           "EXP-DATE  COVERAGE    BENEFIT    PREMIUM  ADJUSTMNT  ".
           03  FILLER           PIC X(27) VALUE
           "  REFUND----DATE  COMMISSON".

       01  HEAD7.
           03  FILLER           PIC X(48) VALUE
           "TOT  **NEW ACCOUNTS**              LOAN        ".
           03  FILLER           PIC X(75) VALUE
           "NEW LOAN         ADDON".
           03  FILLER           PIC X(8) VALUE
           "NET PREM".

       01  HEAD8.
           03  FILLER           PIC X(50) VALUE
           "CLS                              COVERAGE       PR".
           03  FILLER           PIC X(59) VALUE
           "EMIUMS       PREMIUMS    ADJUSTMENTS        CANCELS".
           03  FILLER           PIC X(22) VALUE
           "REFUNDS       NET COMM".

       01  DETAIL-LINE          PIC X(132) VALUE SPACES.

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

       01  DETAIL2 REDEFINES DETAIL-LINE.
           03  FILLER           PIC X(3).
           03  D-QTY            PIC Z(5)9- OCCURS 3.
           03  FILLER           PIC X(3).
           03  D-AMT            PIC ZZZ,ZZZ,ZZ9.99- OCCURS 7.

       01  RANGE-LINE REDEFINES DETAIL-LINE.
           03  FILLER           PIC X(20).
           03  RANGE-SEL        PIC X(112).

       COPY "LIBWI/LCSCANW.CPY".
       COPY "LIBWI/GRSCANW.CPY".
       COPY "LIBLP/LPSTATW.CPY".
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
       COPY "LIBLP/PPMORP_DEF.CPY".
       COPY "LIBLP/PPMORP_WKS.CPY".
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
       COPY "LIBLP/PPMORP_SCN.CPY".

       PROCEDURE DIVISION
             USING FORM-PATHNAME EXIT-PATHNAME.

       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
               PR-FILE WK1-FILE WK-FILE.
       COPY "LIBGB/DECLRP.CPY".
           PERFORM CLOSE-TRS1-FILE.
           PERFORM CLOSE-TRP1-FILE.
           PERFORM CLOSE-TRL1-FILE.
           PERFORM CLOSE-SP1-FILE.
           PERFORM CLOSE-LN1-FILE.
           PERFORM CLOSE-IC1-FILE.
           PERFORM CLOSE-BW1-FILE.
           PERFORM CLOSE-GR1-FILE.
           PERFORM CLOSE-BR-FILE.
           PERFORM CLOSE-PR-FILE.
           PERFORM CLOSE-TRP1-CONNECTION.
           PERFORM CLOSE-TRL1-CONNECTION.
           CLOSE WK1-FILE WK-FILE.
       COPY "LIBGB/DECLRP2.CPY".

      ******************************************************
       MAIN-MODULE SECTION.
      ******************************************************
      D    STOP MESS.
           PERFORM SQL-CONNECT.

           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "PPMORP" TO VDU-FORM-NAME.

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

           MOVE EXT-FILPATH-BASE           TO FILPATH-BASE.
           MOVE BR-MACHINE                 TO FILPATH-MACHINE.
           MOVE BR-DATA-PATH               TO FILPATH-DATA.

           MOVE "B"                        TO BW-PATH-OVERRIDE
                                              IC-PATH-OVERRIDE
                                              LN-PATH-OVERRIDE
                                              TRP-PATH-OVERRIDE
                                              TRL-PATH-OVERRIDE
                                              TRS-PATH-OVERRIDE.

           IF EXT-ROUTE-BUF = "00"
              STRING "INSURANCE REPORT IN PROGRESS...." , BR-NO
                      DELIMITED "  " INTO MESS
              PERFORM SEND-LEGEND.

           PERFORM OPEN-LN1-FILE.

           PERFORM CREATE-WORK-FILE.

       END-OF-GROUP.
      *    PERFORM CLOSE-LN1-FILE.
           GO TO NEXT-GROUP-LOC.

       PROCESS-REPORT.
           PERFORM CLOSE-WK-FILE.
           PERFORM OPEN-WK-FILE.

           PERFORM RUN-REPORT.

       END-ROUTINE.
           IF FIRST-TIME = "Y"
              MOVE "N" TO RECORDS-FOUND
           ELSE
              MOVE "Y" TO COMPANY-FG
              PERFORM GB-TOTALS
              PERFORM RANGE-LINES.

           GO TO END-PROGRAM.

      *******************************************************
       INITIALIZATION SECTION.
      *******************************************************
           MOVE PPMORP-QUEUE-POS TO Q-POS.
           MOVE PPMORP-COPIES-POS TO C-POS.
           MOVE PPMORP-PRU-POS   TO P-POS.
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

       INITIALIZE-BEG-RPT.
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

      * RPT-END-DATE IS ALREADY SET, JUST NEED A BEGINNING FOR TR
           MOVE MM TO DATE-MMDDYY-MM.
           MOVE YY TO DATE-MMDDYY-YY.
           MOVE 01 TO DATE-MMDDYY-DD.
           PERFORM CONVERT-MMDDYY-TO-YYYYMMDD.
           MOVE DATE-YYYYMMDD TO BEGINNING-DATE.

           PERFORM OPEN-WK1-FILE.
           PERFORM OPEN-BR-FILE.
           PERFORM OPEN-GR1-FILE.
           PERFORM OPEN-SP1-FILE.
           PERFORM ARRAYSP-LOAD.
           PERFORM OPEN-PR-FILE.

           MOVE EXT-CONAME-CONAME TO H-NAME.
           MOVE EXT-CONAME-SYSDATE TO H-DATE.
           MOVE EXT-CONAME-USER-ID TO H-USERID.
           MOVE MMYY TO H-MMYY.

           PERFORM CLEAR-TOTALS.

      * CHECK IF ROWS EXIST IN TRSFILE.
      * IF ROWS EXIST, TRS1-CHECK-SW WILL = 'Y' ELSE TRS1-CHECK-SW = ' '
           PERFORM OPEN-TRS1-FILE.
           PERFORM CHECK-TRS-FILE.
           PERFORM CLOSE-TRS1-FILE.

           GO TO INITIALIZE-EXIT.

       INITIALIZE-ENDIT.
           MOVE "X" TO EXT-EOBUF-ERRCD.
       INITIALIZE-EXIT.
           EXIT.

      *******************************************************
       EO-EXEC-AUDIT SECTION.
      *******************************************************
           MOVE EXT-EOBUF-DATE2 TO NDTE-DATE RPT-END-DATE.
           MOVE NDTE-MM TO MM.
           MOVE NDTE-YY TO YY.

      *******************************************************
       ENTER-PARAMETERS SECTION.
      *******************************************************
       ENTER-PARAM.

      * FOR RE-ENTRY ON F4 FROM ACCEPT.
           IF VDUSTATUS = "1U" GO TO SO-01.
      *------------------------------------------------------
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

      *------------------------------------------------------
       BRANCH-IS-NUMERIC.
           MOVE "N" TO GROUP-FLAG.
           MOVE WGR-SRESULT TO BEG-BRANCH.
           MOVE "SN N040BR1." TO VDU.
           MOVE BEG-BRANCH TO VDUNUM0.
           PERFORM SCREENIO.
      *-----------------------------------------------------
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

      *------------------------------------------------------
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

      *-----------------------------------------------------
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
      *-----------------------------------------------------
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
      *-----------------------------------------------------
       ALL-IC.
           MOVE "SNNN020IC1." TO VDU.
           MOVE 1 TO BEG-IC VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN020IC2." TO VDU.
           MOVE 99 TO END-IC VDUNUM0.
           PERFORM SCREENIO.
      *-----------------------------------------------------
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
      *-----------------------------------------------------
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
      *----------------------------------------------------
       ALL-LC.
           MOVE "SNNN020LC1." TO VDU.
           MOVE 1 TO BEG-CLASS VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN020LC2." TO VDU.
           MOVE 99 TO END-CLASS VDUNUM0.
           PERFORM SCREENIO.
      *-----------------------------------------------------
       ST-01.
           MOVE "E  A020ST1." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "16" GO TO ALL-ST.
           IF VDUSTATUS = "1U" GO TO LC-02.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO ST-01.
           MOVE VDUBUF TO BEG-ST.
      *------------------------------------------------------
       ST-02.
           MOVE "E  A020ST2." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "16" GO TO ALL-ST.
           IF VDUSTATUS = "1U" GO TO ST-01.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO ST-02.
           MOVE VDUBUF TO END-ST.
           IF BEG-ST NOT > END-ST GO TO DT-01.
           PERFORM INVALID-RANGE.
           GO TO ST-01.
      *-------------------------------------------------------
       ALL-ST.
           MOVE "S  A020ST1." TO VDU.
           MOVE "  " TO BEG-ST VDUBUF.
           PERFORM SCREENIO.
           MOVE "S  A020ST2." TO VDU.
           MOVE ALL "Z" TO END-ST.
           CALL "C$TOLOWER" USING END-ST, VALUE 2.
           MOVE END-ST TO VDUBUF.
           PERFORM SCREENIO.
      *-------------------------------------------------------
       DT-01.
           IF EXT-ROUTE-BUF = "40" OR "50"
              GO TO SO-01.
           MOVE "ENNN040DT1." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO ST-02.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO DT-01.
           MOVE VDUNUM0 TO MMYY.
           IF MM < 1 OR MM > 12
              MOVE "INVALID MONTH" TO MESS
              PERFORM SEND-MESS
              GO TO DT-01.

           MOVE MM TO DATE-MMDDYY-MM.
           MOVE 33 TO DATE-MMDDYY-DD.
           MOVE YY TO DATE-MMDDYY-YY.
           PERFORM CONVERT-MMDDYY-TO-YYYYMMDD.
           MOVE DATE-YYYYMMDD TO NDTE-DATE.
           PERFORM NEW-DATE-CALCULATION.
           MOVE NDTE-DATE TO RPT-END-DATE.
      *-----------------------------------------------------
       SO-01.
           MOVE "E  A010SO1." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO DT-01.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO SO-01.
           MOVE VDUBUF TO SUMMARY-ONLY.
           IF NOT (SUMMARY-ONLY = "Y" OR "N" OR "I") GO TO SO-01.

       EXIT-PARM.
           EXIT.

      *************************************************
       DISPLAY-PARAMETERS SECTION.
      *************************************************
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
      *------------------------------------------------
           MOVE "SNNN020IC1." TO VDU.
           MOVE BEG-IC TO VDUNUM0.
           PERFORM SCREENIO.
      *------------------------------------------------
           MOVE "SNNN020IC2." TO VDU.
           MOVE END-IC TO VDUNUM0.
           PERFORM SCREENIO.
      *------------------------------------------------
           MOVE "SNNN020LC1." TO VDU.
           MOVE BEG-CLASS TO VDUNUM0.
           PERFORM SCREENIO.
      *------------------------------------------------
           MOVE "SNNN020LC2." TO VDU.
           MOVE END-CLASS TO VDUNUM0.
           PERFORM SCREENIO.
      *------------------------------------------------
           MOVE "S  A020ST1." TO VDU.
           MOVE BEG-ST TO VDUBUF.
           PERFORM SCREENIO.
      *------------------------------------------------
           MOVE "S  A020ST2." TO VDU.
           MOVE END-ST TO VDUBUF.
           PERFORM SCREENIO.
      *------------------------------------------------
           MOVE "S  A010SO1." TO VDU.
           MOVE SUMMARY-ONLY TO VDUBUF.
           PERFORM SCREENIO.

      *************************************************
       RUN-REPORT SECTION.
      *************************************************

           MOVE 0 TO PP-PREM PP-ADJ PP-CAN PP-COMM PP-CAN-DATE.
           MOVE SPACES TO PP-ADDON-FG PP-CANCEL-FG.

           PERFORM READ-WK-FILE-NEXT.
           IF IO-FG = 9
              GO TO END-RUN-REPORT.

           IF WORK-OWNBR NOT = HOLD-LN-BR
              MOVE WORK-OWNBR TO HOLD-LN-BR BR-NO
              PERFORM GET-LN-BW-PATH.

           MOVE WORK-LN-REC TO LN-REC.

      * LN-REC NOW HELD IN WORK FILE (08/2023)
      *     MOVE WORK-OWNBR  TO LN-OWNBR.
      *     MOVE WORK-ACCTNO TO LN-ACCTNO.
      *     PERFORM READ-LN1-FILE.
      *     IF IO-FG NOT = 0
      *        GO TO END-RUN-REPORT.

       KEY-FILE-LOOP.

           ADD WORK-NEW-PP TO PP-PREM.
           IF WORK-NEW-PP NOT = 0
              MOVE WORK-ADDON-FG TO PP-ADDON-FG.
           ADD WORK-ADJ-PP TO PP-ADJ.
           ADD WORK-CAN-PP TO PP-CAN.
           IF WORK-CAN-PP NOT = 0
              MOVE WORK-CANCEL-FG TO PP-CANCEL-FG.
           ADD WORK-COM-PP TO PP-COMM.
           IF WORK-CAN-PP-DATE NOT = 0
              MOVE WORK-CAN-PP-DATE TO PP-CAN-DATE.

           IF FIRST-TIME = "Y"
              MOVE "N" TO FIRST-TIME
              MOVE WORK-INSCOMP TO HOLD-IC
              MOVE LN-OWNBR TO HOLD-BR
      *       MOVE WORK-OWNBR TO HOLD-BR
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
      *        PERFORM READ-SP1-FILE
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

           MOVE WORK-LN-REC TO LN-REC.

      * LN-REC NOW HELD IN WORK FILE (08/2023)
      *     MOVE WORK-OWNBR  TO LN-OWNBR.
      *     MOVE WORK-ACCTNO TO LN-ACCTNO.
      *     PERFORM READ-LN1-FILE.
      *     IF IO-FG NOT = 0
      *        GO TO END-RUN-REPORT.

           GO TO KEY-FILE-LOOP.
       END-RUN-REPORT.

      *****************************************
       CREATE-WORK-FILE SECTION.
      *****************************************

           PERFORM SORT-TRP-FILE.
           PERFORM SORT-TRL-FILE.

      * TRSFILE CURRENTLY (10/2023) EMPTY.
      * INITIALIZE RAN PERFORM CHECK-TRS-FILE TO SET TRS1-CHECK-SW
           IF TRS1-CHECK-SW = "Y"
              PERFORM SORT-TRS-FILE.

       CREATE-WORK-FILE-EXIT.
           EXIT.

      ******************************************************************
       SORT-TRP-FILE SECTION.
      ******************************************************************

           PERFORM OPEN-TRP1-FILE.

           MOVE BR-NO               TO TP-BRNO
                                       QTRP1-WBEG-BRNO.
      *                                 QTRP1-WEND-BRNO.

           MOVE BEGINNING-DATE      TO TP-DATE SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO QTRP1-WBEG-DATE.
           MOVE RPT-END-DATE        TO  SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO QTRP1-WEND-DATE.

           MOVE BEG-CLASS           TO TP-CLASS
                                       QTRP1-WBEG-CLASS.
           MOVE END-CLASS           TO QTRP1-WEND-CLASS.

      *     MOVE 0                   TO TP-ACCTNO TP-LINE
      *                                 QTRP1-WBEG-ACCTNO
      *                                 QTRP1-WBEG-LINE.
      *     MOVE ALL "9"             TO QTRP1-WEND-ACCTNO
      *                                 QTRP1-WEND-LINE.

           PERFORM START-TRP1-FILE.

       SORT-TRP-NEXT. 
           PERFORM READ-TRP1-FILE-NEXT.
           IF (IO-FG NOT = 0)               *> OR (TRP-PATH-OWNBR NOT = TP-BRNO)
              GO TO SORT-TRP-EXIT.

      *** THIS CHECK ALSO IN STORED PROCEDURE ***
           IF TP-DATE > RPT-END-DATE
              GO TO SORT-TRP-EXIT.

      *** THIS CHECK ALSO IN STORED PROCEDURE ***
           IF TP-CLASS < BEG-CLASS OR TP-CLASS > END-CLASS
              GO TO SORT-TRP-NEXT.

      *** THIS CHECK ALSO IN STORED PROCEDURE ***
           IF TP-ADDON-TYPE NOT = "PP"
              IF TP-TRCD NOT = "RP" AND NOT = "CP"
                 GO TO SORT-TRP-NEXT.

      * LNFILE FIELDS RETURNED IN STORED PROCEDURE
      *     MOVE TP-BRNO   TO LN-OWNBR.
      *     MOVE TP-ACCTNO TO LN-ACCTNO.
      *     PERFORM READ-LN1-FILE.
      *     IF IO-FG NOT = 0
      *        GO TO SORT-TRP-NEXT.

           IF LN-ORGST < BEG-ST OR LN-ORGST > END-ST
              GO TO SORT-TRP-NEXT.

      *  IF LN-OWNBR NOT = TR1-BRNO    <<< THIS DOES NOT WORK ANYMORE !!
      * IF ANOTHER BRANCH DOES A PAYOFF, AND THE SAME LOAN # EXIST
      * IN BOTH BRANCHES, (NOT REALLY THE SAME LOAN!), IT WILL PRINT FOR
      * BOTH BRANCHES, SHOULD ONLY PRINT FOR THE OWNING BRANCH !!

      * ONLY REPORT FOR THE OWNING BRANCH ("G")
      *** THIS CHECK ALSO IN STORED PROCEDURE ***
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
              MOVE LN-INSCOMP(3) TO WORK-INSCOMP
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
              MOVE 0 TO WORK-NEW-PP WORK-ADJ-PP WORK-CAN-PP
                        WORK-CAN-PP-DATE WORK-COM-PP
              MOVE SPACES TO WORK-ADDON-FG WORK-CANCEL-FG.

      *    IF TP-CODE = "P"
              IF TP-ADDON-TYPE = "PP" AND
                       TP-ADDON-TEXT = "ADD"
                 MOVE "A" TO WORK-ADDON-FG
                 COMPUTE WORK-NEW-PP = WORK-NEW-PP + TP-APCUR
                 COMPUTE WORK-COM-PP = WORK-COM-PP + TP-INSCOMM
              ELSE
              IF TP-ADDON-TYPE = "PP" AND
                       TP-ADDON-TEXT = "ADJ"
                 COMPUTE WORK-ADJ-PP = WORK-ADJ-PP + TP-APCUR
                 COMPUTE WORK-COM-PP = WORK-COM-PP + TP-INSCOMM
              ELSE
              IF TP-ADDON-TYPE = "PP" AND
                       TP-ADDON-TEXT = "CNL"
                 MOVE "C" TO WORK-CANCEL-FG
                 COMPUTE WORK-CAN-PP = WORK-CAN-PP + TP-APCUR
                 MOVE TP-PAYDATE TO WORK-CAN-PP-DATE
                 COMPUTE WORK-COM-PP = WORK-COM-PP + TP-INSCOMM
              ELSE
              IF TP-TRCD = "RP"
                 COMPUTE WORK-CAN-PP = WORK-CAN-PP +
                                   ((TP-TRAMT - TP-INS-TAX-RB) * -1)
                 MOVE TP-PAYDATE TO WORK-CAN-PP-DATE
                 COMPUTE WORK-COM-PP = WORK-COM-PP + (TP-INSCOMM * -1)
              ELSE
              IF TP-TRCD = "CP"
                 MOVE "C" TO WORK-CANCEL-FG
                 COMPUTE WORK-CAN-PP = WORK-CAN-PP +
                                   ((TP-TRAMT - TP-INS-TAX-RB) * -1)
                 MOVE TP-PAYDATE TO WORK-CAN-PP-DATE
                 COMPUTE WORK-COM-PP = WORK-COM-PP + (TP-INSCOMM * -1).

           MOVE LN-REC TO WORK-LN-REC.

           IF IO-FG = 0
              PERFORM REWRITE-WK-FILE
           ELSE
              PERFORM WRITE-WK-FILE.

           GO TO SORT-TRP-NEXT.

       SORT-TRP-EXIT.
           PERFORM CLOSE-TRP1-FILE.

      ******************************************************************
       SORT-TRL-FILE SECTION.
      ******************************************************************

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

           MOVE BEG-CLASS           TO TL-CLASS
                                       QTRL1-WBEG-CLASS.
           MOVE END-CLASS           TO QTRL1-WEND-CLASS.

      *     MOVE 0                   TO TL-ACCTNO TL-LAWCODE TL-LINE
      *                                 QTRL1-WBEG-ACCTNO
      *                                 QTRL1-WBEG-LAWCODE
      *                                 QTRL1-WBEG-LINE.
      *     MOVE ALL "9"             TO QTRL1-WEND-ACCTNO
      *                                 QTRL1-WEND-LAWCODE
      *                                 QTRL1-WEND-LINE.

           PERFORM START-TRL1-FILE.

       SORT-TRL-NEXT.
           PERFORM READ-TRL1-FILE-NEXT.
           IF (IO-FG NOT = 0)               *> OR (TRL-PATH-OWNBR NOT = TL-BRNO)
              GO TO SORT-TRL-EXIT.

      * THIS ALSO CHECKED IN STORED PROCEDURE
           IF TL-DATE > RPT-END-DATE
              GO TO SORT-TRL-EXIT.

      * THIS ALSO CHECKED IN STORED PROCEDURE
           IF TL-CLASS < BEG-CLASS OR TL-CLASS > END-CLASS
              GO TO SORT-TRL-NEXT.

      * THIS ALSO CHECKED IN STORED PROCEDURE
           IF TL-INSCOMP(3) = 0
              GO TO SORT-TRL-NEXT
           ELSE
              IF TL-INSCOMP(3) < BEG-IC OR > END-IC
                 GO TO SORT-TRL-NEXT
              ELSE
      * THIS ALSO CHECKED IN STORED PROCEDURE
                 IF TL-INSOURS(3) NOT = "Y"
                    GO TO SORT-TRL-NEXT.

      * LNFILE FIELDS RETURNED IN STORED PROCEDURE
      *     MOVE TL-BRNO   TO LN-OWNBR.
      *     MOVE TL-ACCTNO TO LN-ACCTNO.
      *     PERFORM READ-LN1-FILE.
      *     IF IO-FG  NOT = 0
      *        GO TO SORT-TRL-NEXT.

           IF LN-ORGST < BEG-ST OR LN-ORGST > END-ST
              GO TO SORT-TRL-NEXT.

           MOVE TL-INSCOMP(3) TO WORK-INSCOMP.

           MOVE TL-DATE   TO WORK-DATE.
           MOVE TL-BRNO   TO WORK-OWNBR.
           MOVE TL-CLASS  TO WORK-CLASS.
           MOVE TL-ACCTNO TO WORK-ACCTNO.
           PERFORM READ-WK-FILE.
           IF IO-FG NOT = 0
              MOVE 0 TO WORK-NEW-PP WORK-ADJ-PP WORK-CAN-PP
                        WORK-CAN-PP-DATE WORK-COM-PP
              MOVE SPACES TO WORK-ADDON-FG WORK-CANCEL-FG.

           COMPUTE WORK-NEW-PP = WORK-NEW-PP + TL-INSPREM(3).
           COMPUTE WORK-COM-PP = WORK-COM-PP + TL-INSCOMM(3).

           MOVE LN-REC TO WORK-LN-REC.

           IF IO-FG = 0
              PERFORM REWRITE-WK-FILE
           ELSE
              PERFORM WRITE-WK-FILE.

           GO TO SORT-TRL-NEXT.

       SORT-TRL-EXIT.
           PERFORM CLOSE-TRL1-FILE.

      ******************************************************************
       SORT-TRS-FILE SECTION.
      ******************************************************************

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

           IF TS-DATE > RPT-END-DATE
              GO TO SORT-TRS-EXIT.

           IF TS-CLASS < BEG-CLASS OR TS-CLASS > END-CLASS
              GO TO SORT-TRS-NEXT.

           IF TS-INSCOMP(3) = 0
              GO TO SORT-TRS-NEXT
           ELSE
           IF TS-INSCOMP(3) < BEG-IC OR > END-IC
              GO TO SORT-TRS-NEXT
           ELSE
           IF TS-INSOURS(3) NOT = "Y"
              GO TO SORT-TRS-NEXT.

           MOVE TS-BRNO   TO LN-OWNBR.
           MOVE TS-ACCTNO TO LN-ACCTNO.
           PERFORM READ-LN1-FILE.
           IF IO-FG  NOT = 0
              GO TO SORT-TRS-NEXT.

           IF LN-ORGST < BEG-ST OR LN-ORGST > END-ST
              GO TO SORT-TRS-NEXT.

           MOVE TS-INSCOMP(3) TO WORK-INSCOMP.

           MOVE TS-DATE   TO WORK-DATE.
           MOVE TS-BRNO   TO WORK-OWNBR.
           MOVE TS-CLASS  TO WORK-CLASS.
           MOVE TS-ACCTNO TO WORK-ACCTNO.
           PERFORM READ-WK-FILE.
           IF IO-FG NOT = 0
              MOVE 0 TO WORK-NEW-PP WORK-ADJ-PP WORK-CAN-PP
                        WORK-CAN-PP-DATE WORK-COM-PP
              MOVE SPACES TO WORK-ADDON-FG WORK-CANCEL-FG.

           COMPUTE WORK-NEW-PP = WORK-NEW-PP + TS-INSPREM(3).
           COMPUTE WORK-COM-PP = WORK-COM-PP + TS-INSCOMM(3).

           MOVE LN-REC TO WORK-LN-REC.

           IF IO-FG = 0
              PERFORM REWRITE-WK-FILE
           ELSE
              PERFORM WRITE-WK-FILE.

           GO TO SORT-TRS-NEXT.

       SORT-TRS-EXIT.
           PERFORM CLOSE-TRS1-FILE.

      *********************************************
       KEY-DETAIL SECTION.
      *********************************************
           IF SUMMARY-ONLY = "Y" OR "I"
              GO TO END-KEY-DETAIL.

           IF (LN-ACCTNO = WORK-ACCTNO) AND (LN-OWNBR = WORK-OWNBR)
              GO TO EXIT-KEY-DETAIL.

      * SEUP PRIMARY INSURED INFO:
           IF LN-INSURED = "S"
              MOVE LN-SSNO(2) TO BW-SSNO
           ELSE
              MOVE LN-SSNO(1) TO BW-SSNO.
           PERFORM SETUP-INSURED.
           MOVE BAGE-AGE TO D-AGE.

           IF LN-INSURED = "S"
              MOVE LN-SSNO(1) TO BW-SSNO
              PERFORM READ-BW1-FILE.

           PERFORM LPSTAT-DISPLAY.
           MOVE LPS-DISPLAY-LNNO TO D-LOAN-NUM.

           IF PP-PREM = 0 AND PP-ADJ = 0 AND PP-CAN = 0 AND
               PP-COMM = 0 GO TO SKIP-PP.

           MOVE 3 TO ITRM-SUB.
           PERFORM INS-TERM-CALCULATION.
           MOVE ITRM-INSTERM TO D-TERM.

           MOVE LN-INSEFF-DATE(3) TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO D-EFFDATE.
           MOVE LN-INSEXP-DATE(3) TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO D-EXPDATE.
           MOVE LN-INSCOVR(3) TO D-COVRG D-REGPYAMT.

           MOVE PP-PREM TO D-INSPREM.
           MOVE PP-ADDON-FG TO D-ADDTEXT.
           MOVE PP-ADJ TO D-INSADJ.
           MOVE PP-CAN TO D-INSREFUND.
           MOVE PP-CANCEL-FG TO D-CNLTEXT.
           IF PP-CAN NOT = 0
              MOVE PP-CAN-DATE TO DATE-YYYYMMDD
              PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
              MOVE DATE-MMDDYY TO D-REFUND-DATE.
           MOVE PP-COMM TO D-INSCOMM.

       SKIP-PP.

           MOVE 1 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

       END-KEY-DETAIL.
           PERFORM CREATE-TOTAL-RECS.

           MOVE 0 TO PP-PREM PP-ADJ PP-CAN PP-COMM PP-CAN-DATE.
           MOVE SPACES TO PP-ADDON-FG PP-CANCEL-FG.

       EXIT-KEY-DETAIL.
           EXIT.

      *********************************************
       CREATE-TOTAL-RECS SECTION.
      *********************************************
           MOVE 0 TO ONE-PP.
           IF LN-INSCOMP(3) NOT = 0
              MOVE LN-INSCOVR(3) TO NEW-COVERAGE
              IF PP-PREM = 0
                 MOVE 0 TO NEW-COVERAGE
              ELSE
                 IF PP-PREM < 0
                    MULTIPLY -1 BY NEW-COVERAGE.
           IF PP-PREM > 0
              MOVE 1 TO ONE-PP
           ELSE
              IF PP-PREM < 0
                 MOVE -1 TO ONE-PP.

      *    WK-CODE: 1-BR, 2-ST, 3-IC, 4-GB

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
           PERFORM CREATE-WK.

           PERFORM INIT-WK1-KEY.
           MOVE 4 TO WK-CODE.
           MOVE 0 TO WK-IC.
           PERFORM CREATE-WK.
       EXIT-CREATE-TOTAL-RECS.
           EXIT.

      *********************************************
       CREATE-WK SECTION.
      *********************************************
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
              WK-AMT(1) WK-AMT(2) WK-AMT(3)
              WK-AMT(4) WK-AMT(5) WK-AMT(6) WK-AMT(7)
              WK-QTY(1) WK-QTY(2).
           PERFORM WRITE-WK1-FILE.
           IF IO-FG = 9
              GO TO IO-ERROR.
           MOVE "Y" TO REC-EXISTS.
           GO TO CHECK-REC-EXISTS.
       UPDATE-WK1-REC.
           ADD ONE-PP TO WK-QTY(1).
           ADD NEW-COVERAGE TO WK-AMT(1).
           IF PP-ADDON-FG = "A"
              ADD PP-PREM TO WK-AMT(6)
           ELSE
              ADD PP-PREM TO WK-AMT(2).
           ADD PP-ADJ TO WK-AMT(3).
           IF PP-CANCEL-FG = "C"
              ADD PP-CAN TO WK-AMT(7)
           ELSE
              ADD PP-CAN TO WK-AMT(4).
           ADD PP-COMM TO WK-AMT(5).
           PERFORM REWRITE-WK1-FILE.

      *********************************************
       BR-TOTALS SECTION.
      *********************************************
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

      *********************************************
       IC-TOTALS SECTION.
      *********************************************
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

      *********************************************
       GB-TOTALS SECTION.
      *********************************************
           PERFORM IC-TOTALS.
           MOVE "C O M P A N Y   T O T A L S:" TO H-PAGE-DESC.
           MOVE 0 TO H-BRNO.
           MOVE " " TO H-BRNAME.
           MOVE 4 TO SAVE-CODE.
           MOVE 0 TO HOLD-IC.
           MOVE " " TO SAVE-ST.
           PERFORM PRINT-TOTALS.

      *********************************************
       PRINT-TOTALS SECTION.
      *********************************************
           MOVE 99 TO LN-COUNT.
           MOVE "Y" TO TOTAL-FG.
           MOVE "Y" TO FIRST-TOT.
           MOVE " " TO WK1-KEY.
           MOVE SAVE-CODE TO WK-CODE.
           MOVE HOLD-IC TO WK-IC.
           MOVE SAVE-ST TO WK-ST.
           MOVE 0 TO WK-CLASS.
           PERFORM START-WK1-FILE.
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
       LOOP-FIELD-2.
      *    ST LOOP
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
      *    BR/IC LOOP
           IF WK-ST NOT = SAVE-ST
              GO TO EXIT-PRINT-TOTALS.
           PERFORM MOVE-ACCUM-TOTALS.
           GO TO NEXT-TOTAL-LINE.
       EXIT-PRINT-TOTALS.
           PERFORM TOTAL-LINE.
           MOVE "N" TO TOTAL-FG.

      *********************************************
       SETUP-INSURED SECTION.
      *********************************************
           PERFORM READ-BW1-FILE.
           IF IO-FG = 9
              MOVE "*** NOT FOUND ***" TO BW-FNAME
              MOVE SPACES TO BW-LNAME
              MOVE 0      TO BW-BDATE BW-REFDATE.

      * SET NAME OF PRIMARY INSURED:
           MOVE SPACES TO NAME-WK.
           STRING BW-FNAME," ",BW-LNAME DELIMITED BY "   "
               INTO NAME-WK.
           MOVE NAME-WK TO D-NAME.

      * SET AGE OF PRIMARY INSURED:
           MOVE BW-BDATE     TO BAGE-DATE.
           MOVE BW-REFDATE   TO BAGE-REFDATE.
           MOVE RPT-END-DATE TO BAGE-TODAY.
           PERFORM BAGE-CALCULATION.

       SETUP-INSURED-EXIT.
           EXIT.

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
       HEAD-PAGE SECTION.
      *********************************************
           MOVE "N" TO FIRST-TIME.
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
              MOVE 2 TO LN-FEED
              PERFORM WRITE-PR-REC
           ELSE
              MOVE HEAD7 TO PR-REC
              MOVE 2 TO LN-FEED
              PERFORM WRITE-PR-REC.

           IF TOTAL-FG = "N"
              NEXT SENTENCE
           ELSE
              MOVE HEAD8 TO PR-REC
              MOVE 1 TO LN-FEED
              PERFORM WRITE-PR-REC.

           MOVE DASH-LINE TO PR-REC.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-PR-REC.

       END-HEAD-PAGE.
           MOVE 8 TO LN-COUNT.
           MOVE 2 TO LN-FEED.

      *********************************************
       RANGE-LINES SECTION.
      *********************************************
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

           MOVE "BEG INS CO" TO RANGE-LINE.
           MOVE BEG-IC TO RANGE-SEL.
           PERFORM WRITE-DETAIL-LINE.
           MOVE "END INS CO" TO RANGE-LINE.
           MOVE END-IC TO RANGE-SEL.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "BEG ORIG STATE" TO RANGE-LINE.
           MOVE BEG-ST TO RANGE-SEL.
           PERFORM WRITE-DETAIL-LINE.
           MOVE "END ORIG STATE" TO RANGE-LINE.
           MOVE END-ST TO RANGE-SEL.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "BEG LOAN CLASS" TO RANGE-LINE.
           MOVE BEG-CLASS TO RANGE-SEL.
           PERFORM WRITE-DETAIL-LINE.
           MOVE "END LOAN CLASS" TO RANGE-LINE.
           MOVE END-CLASS TO RANGE-SEL.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "MONTH AND YEAR" TO RANGE-LINE.
           MOVE MMYY TO RANGE-SEL.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "SUMMARY ONLY ?" TO RANGE-LINE.
           MOVE SUMMARY-ONLY TO RANGE-SEL
           PERFORM WRITE-DETAIL-LINE.

       COPY "LIBLP/ARRAYSP.CPY".

       COPY "LIBGB/DATER.CPY".
       COPY "LIBLP/LPSTAT.CPY".
       COPY "LIBLP/LPMDTE.CPY".
       COPY "LIBLP/LPITRM.CPY".
       COPY "LIBGB/TIMEIO.CPY".
       COPY "LIBLP/LPBAGE.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/GRWORK.CPY".
       COPY "LIBGB/GRSERCH.CPY".
       COPY "LIBGB/BRSECURE.CPY".

      *********************************************
       MOVE-ACCUM-TOTALS SECTION.
      *********************************************
           MOVE 7 TO SUB.
       NEXT-AMT-ACCUM.
           ADD WK-AMT(SUB) TO TOT-AMT(SUB).
           IF SUB > 1
              SUBTRACT 1 FROM SUB
           GO TO NEXT-AMT-ACCUM.
       NEXT-QTY-ACCUM.
           ADD WK-QTY(SUB) TO TOT-QTY(SUB).
           IF SUB < 2
              ADD 1 TO SUB
              GO TO NEXT-QTY-ACCUM.
           MOVE WK-CLASS TO D-CLASS.
           MOVE WK-QTY(1) TO D-QTY(1).
           MOVE WK-AMT(1) TO D-AMT(1).
           MOVE WK-AMT(2) TO D-AMT(2).
           MOVE WK-AMT(6) TO D-AMT(3).
           MOVE WK-AMT(3) TO D-AMT(4).
           MOVE WK-AMT(7) TO D-AMT(5).
           MOVE WK-AMT(4) TO D-AMT(6).
           COMPUTE D-AMT(7) =
              (WK-AMT(2) + WK-AMT(6)) + (WK-AMT(4) + WK-AMT(7))
                                           + WK-AMT(3).
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE WK-AMT(5) TO D-AMT(7).
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      *********************************************
       CLASS-SCAN SECTION.
      *********************************************
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
           MOVE EXT-CONAME-CONAME TO PPMORP-CONAME.
           MOVE EXT-CONAME-SYSDATE TO PPMORP-SYSDATE.
           DISPLAY PPMORP-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE PPMORP-FORM-FIELDS TO WR-BUF.
           MOVE PPMORP-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      *********************************************************
       SETUP-LINK-INFO SECTION.
      *********************************************************
           MOVE PPMORP-HELP--FILE TO HELP-LINK-FILE.
           MOVE PPMORP-HELP--DIR  TO HELP-LINK-DIR.

      *********************************************************
       VDU-CONVERT-FIELD SECTION.
      *********************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBLP/PPMORP_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      *********************************************
       MISC-PARA SECTION.
      *********************************************
      *-----------------------------------------
       COPY "LIBGB/GETPR.CPY".
       COPY "LIBGB/MKTEMP.CPY".
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/DACCESS.CPY".
       COPY "LIBGB/SYSTEM.CPY".
       COPY "LIBGB/RMFILE.CPY".
      *-----------------------------------------
       IC-HEADER.
           MOVE HOLD-IC TO IC-CODE.
           PERFORM READ-IC1-FILE.
           IF IO-FG = 9
              MOVE "* INVALID *" TO IC-NAME.
           MOVE HOLD-IC TO H-ICNO.
           MOVE IC-NAME TO H-ICNAME.
           IF COMPANY-FG = "Y"
              MOVE SPACES TO H-INSCO H-ICNAME
              MOVE 0 TO H-ICNO.
      *-----------------------------------------
       BR-HEADER.
           MOVE HOLD-BR TO BR-NO.
           PERFORM READ-BR-FILE.
           IF IO-FG = 9
              MOVE "* INVALID *" TO BR-NAME.
           MOVE HOLD-BR TO H-BRNO.
           MOVE BR-NAME TO H-BRNAME.
      *-----------------------------------------
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
      *----------------------------------------
       INIT-WK1-KEY.
           MOVE " " TO WK1-KEY.
           MOVE HOLD-IC TO WK-IC.
           MOVE LN-CLASS TO WK-CLASS.
      *----------------------------------------
       TOTAL-LINE.
           MOVE "ALL" TO DETAIL-LINE.
           MOVE TOT-QTY(1) TO D-QTY(1).
           MOVE TOT-AMT(1) TO D-AMT(1).
           MOVE TOT-AMT(2) TO D-AMT(2).
           MOVE TOT-AMT(6) TO D-AMT(3).
           MOVE TOT-AMT(3) TO D-AMT(4).
           MOVE TOT-AMT(7) TO D-AMT(5).
           MOVE TOT-AMT(4) TO D-AMT(6).
      *NET PREMIUMS
      * (NEW PREM + ADDONS) + (REFUNDS + CANCELS) - ADJUSTMENTS
           COMPUTE D-AMT(7) =
              (TOT-AMT(2) + TOT-AMT(6)) + (TOT-AMT(4) + TOT-AMT(7))
                                                    + TOT-AMT(3).

           MOVE 3 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE TOT-AMT(5) TO D-AMT(7).
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           PERFORM CLEAR-TOTALS.
           MOVE 99 TO LN-COUNT.
      *---------------------------------------------------
       CLEAR-TOTALS.
           MOVE 0 TO
              TOT-AMT(1) TOT-AMT(2) TOT-AMT(3) TOT-AMT(4) TOT-AMT(5)
              TOT-AMT(6) TOT-AMT(7) TOT-QTY(1) TOT-QTY(2).
      *----------------------------------------------------
       INVALID-RANGE.
           MOVE "INVALID RANGE" TO MESS.
           PERFORM SEND-MESS.
      *-----------------------------------------------------
       SEND-LEGEND.
           MOVE MESS TO WR-BUF.
           MOVE MESS-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      *********************************************
       IO-ROUTINES SECTION.
      *********************************************
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

       COPY "LIBGB/GBGR1RN.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBLP/LPLN1RN.CPY".



      *COPY "LIBLP/LPTRP1RN.CPY".
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
               EXEC SQL CONNECT TO :SQL-CONNECT-SERVER AS TRP1_CONNECT
                        USER :SQL-CONNECT-USERID USING :SQL-CONNECT-PSWD
               END-EXEC
               EXEC SQL SET CONNECTION C1 END-EXEC
               MOVE 'Y' TO TRP1-CONN-OPEN-SW.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       START-TRP1-FILE.   *> SSP - TRP  LN
           PERFORM START-IT.
           MOVE TRP-PATH-OWNBR  TO TP-BRNO.
           MOVE TRP-PATH-SQL    TO E-FILE.
           MOVE TRP1-KEY        TO E-KEYX.

           IF ( TRP1-CURSOR-STAT-GOOD )
              PERFORM CLOSE-TRP1-FILE.

           EXEC SQL SET CONNECTION TRP1_CONNECT END-EXEC.

           EXEC SQL
            DECLARE TRP1_CURSOR CURSOR FOR
                :TRP1-RETURN-CODE =  
            EXEC SSP_PPMORP_TRPFILE_SELECT
              ( :QTRP1-WBEG-BRNO,
                :QTRP1-WBEG-DATE,   :QTRP1-WEND-DATE,
                :QTRP1-WBEG-CLASS,  :QTRP1-WEND-CLASS)
           END-EXEC.

      *     EXEC SQL
      *      DECLARE TRP1_CURSOR CURSOR FOR
      *        SELECT TRPFILE.TP_BRNO,
      *         CAST(TRPFILE.TP_DATE AS VARCHAR(10)), 
      *         TRPFILE.TP_CLASS,
      *         TRPFILE.TP_ACCTNO,
      *         TRPFILE.TP_LINE,
      *         TRPFILE.TP_TRCD, 
      *         TRPFILE.TP_REFNO, 
      *         CAST(TRPFILE.TP_PAYDATE AS VARCHAR(10)), 
      *         TRPFILE.TP_TRAMT,
      *         TRPFILE.TP_APCUR,
      *         TRPFILE.TP_INSOURS,
      *         TRPFILE.TP_OTHBR_FG,
      *         TRPFILE.TP_INSCOMM,
      *         TRPFILE.TP_INSCOMP,
      *         TRPFILE.TP_INS_TAX_RB,
      *         TRPFILE.TP_ADDON_TYP,
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
      *         CAST(LNFILE.LN_ORIG_1STPYDATE AS VARCHAR(10)),
      *         CAST(LNFILE.LN_1STPYDATE AS VARCHAR(10)),
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
      *         CAST(LNFILE.LN_INSEFF_DATE_1 AS VARCHAR(10)),
      *         CAST(LNFILE.LN_INSEFF_DATE_2 AS VARCHAR(10)),
      *         CAST(LNFILE.LN_INSEFF_DATE_3 AS VARCHAR(10)),
      *         CAST(LNFILE.LN_INSEFF_DATE_4 AS VARCHAR(10)),
      *         CAST(LNFILE.LN_INSEFF_DATE_5 AS VARCHAR(10)),
      *         CAST(LNFILE.LN_INSEFF_DATE_6 AS VARCHAR(10)),
      *         CAST(LNFILE.LN_INSEFF_DATE_7 AS VARCHAR(10)),
      *         CAST(LNFILE.LN_INSEFF_DATE_8 AS VARCHAR(10)),
      *         CAST(LNFILE.LN_INSEFF_DATE_9 AS VARCHAR(10)),
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
      *  FROM DBO.TRPFILE TRPFILE
      *  INNER JOIN DBO.LNFILE LNFILE
      *    ON LNFILE.LN_OWNBR = TRPFILE.TP_BRNO
      *   AND LNFILE.LN_ACCTNO = TRPFILE.TP_ACCTNO
      *  WHERE TRPFILE.TP_BRNO = @QTRP1_WBEG_BRNO
      *   AND TRPFILE.TP_DATE  BETWEEN @QTRP1_WBEG_DATE  AND @QTRP1_WEND_DATE
      *   AND TRPFILE.TP_CLASS BETWEEN @QTRP1_WBEG_CLASS AND @QTRP1_WEND_CLASS
      *   AND ( TRPFILE.TP_ADDON_TYP = 'PP'
      *       OR TRPFILE.TP_TRCD IN ('RP', 'CP')) 
      *  ORDER BY TRPFILE.TP_CLASS,
      *           TRPFILE.TP_ACCTNO,
      *           TRPFILE.TP_DATE,
      *           TRPFILE.TP_LINE
      *     END-EXEC.

           EXEC SQL
               OPEN TRP1_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE ZEROES TO QTRP1-WBEG-BRNO
                          QTRP1-WEND-BRNO
                          QTRP1-WBEG-CLASS
                          QTRP1-WEND-CLASS.

           MOVE SPACES TO QTRP1-WBEG-DATE
                          QTRP1-WEND-DATE
                          QTRP1-WSBEG-DATE.

           MOVE STAT---GOOD TO TRP1-CURSOR-STAT.

           EXEC SQL SET CONNECTION C1 END-EXEC.

      *-----------------------------------------------------------------
       READ-TRP1-FILE-NEXT.   *> SSP - TRP LN
           PERFORM READ-IT.
           MOVE TRP-PATH-OWNBR  TO TP-BRNO.
           MOVE TRP-PATH-SQL    TO E-FILE.
           MOVE TRP1-KEY        TO E-KEYX.
           INITIALIZE QTRP-REC QLN-REC.

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
                     :QTP-INSOURS, 
                     :QTP-OTHBR-FG, 
                     :QTP-INSCOMM,
                     :QTP-INSCOMP,
                     :QTP-INS-TAX-RB,
                     :QTP-ADDON-TYP,
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
              PERFORM GET-TRP-FIELDS
              PERFORM GET-LN-FIELDS.

           EXEC SQL SET CONNECTION C1 END-EXEC.

      *-----------------------------------------------------------------
       CLOSE-TRP1-FILE.

           PERFORM CLOSE-IT.

           IF ( TRP1-CURSOR-STAT-GOOD )
              EXEC SQL SET CONNECTION TRP1_CONNECT END-EXEC
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

           IF TRL1-CONN-OPEN-SW = "N" 
               EXEC SQL CONNECT TO :SQL-CONNECT-SERVER AS TRL1_CONNECT
                        USER :SQL-CONNECT-USERID USING :SQL-CONNECT-PSWD
               END-EXEC
               EXEC SQL SET CONNECTION C1 END-EXEC
               MOVE 'Y' TO TRL1-CONN-OPEN-SW.

      *-----------------------------------------------------------------
       START-TRL1-FILE.   *> SSP - TRL LN
           PERFORM START-IT.
           MOVE TRL-PATH-OWNBR  TO TL-BRNO.
           MOVE TRL-PATH-SQL    TO E-FILE.
           MOVE TRL1-KEY        TO E-KEYX.

           IF ( TRL1-CURSOR-STAT-GOOD )
              PERFORM CLOSE-TRL1-FILE.

      *  SELECT TRLFILE.TL_BRNO,
      *         CAST(TRLFILE.TL_DATE AS VARCHAR(10)),
      *         TRLFILE.TL_CLASS,
      *         TRLFILE.TL_ACCTNO,
      *         TRLFILE.TL_LAWCODE,
      *         TRLFILE.TL_LINE,
      *         TRLFILE.TL_INSCOMP_1,
      *         TRLFILE.TL_INSCOMP_2,
      *         TRLFILE.TL_INSCOMP_3,
      *         TRLFILE.TL_INSCOMP_4,
      *         TRLFILE.TL_INSCOMP_5,
      *         TRLFILE.TL_INSCOMP_6,
      *         TRLFILE.TL_INSCOMP_7,
      *         TRLFILE.TL_INSCOMP_8,
      *         TRLFILE.TL_INSCOMP_9,
      *         TRLFILE.TL_INSCOMP_10,
      *         TRLFILE.TL_INSOURS_1,
      *         TRLFILE.TL_INSOURS_2,
      *         TRLFILE.TL_INSOURS_3,
      *         TRLFILE.TL_INSOURS_4,
      *         TRLFILE.TL_INSOURS_5,
      *         TRLFILE.TL_INSOURS_6,
      *         TRLFILE.TL_INSOURS_7,
      *         TRLFILE.TL_INSOURS_8,
      *         TRLFILE.TL_INSOURS_9,
      *         TRLFILE.TL_INSOURS_10,
      *         TRLFILE.TL_INSPREM_1,
      *         TRLFILE.TL_INSPREM_2,
      *         TRLFILE.TL_INSPREM_3,
      *         TRLFILE.TL_INSPREM_4,
      *         TRLFILE.TL_INSPREM_5,
      *         TRLFILE.TL_INSPREM_6,
      *         TRLFILE.TL_INSPREM_7,
      *         TRLFILE.TL_INSPREM_8,
      *         TRLFILE.TL_INSPREM_9,
      *         TRLFILE.TL_INSPREM_10,
      *         TRLFILE.TL_INSCOMM_1,
      *         TRLFILE.TL_INSCOMM_2,
      *         TRLFILE.TL_INSCOMM_3,
      *         TRLFILE.TL_INSCOMM_4,
      *         TRLFILE.TL_INSCOMM_5,
      *         TRLFILE.TL_INSCOMM_6,
      *         TRLFILE.TL_INSCOMM_7,
      *         TRLFILE.TL_INSCOMM_8,
      *         TRLFILE.TL_INSCOMM_9,
      *         TRLFILE.TL_INSCOMM_10,
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
      *         CAST(LNFILE.LN_ORIG_1STPYDATE AS VARCHAR(10)),
      *         CAST(LNFILE.LN_1STPYDATE AS VARCHAR(10)),
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
      *         CAST(LNFILE.LN_INSEFF_DATE_1 AS VARCHAR(10)),
      *         CAST(LNFILE.LN_INSEFF_DATE_2 AS VARCHAR(10)),
      *         CAST(LNFILE.LN_INSEFF_DATE_3 AS VARCHAR(10)),
      *         CAST(LNFILE.LN_INSEFF_DATE_4 AS VARCHAR(10)),
      *         CAST(LNFILE.LN_INSEFF_DATE_5 AS VARCHAR(10)),
      *         CAST(LNFILE.LN_INSEFF_DATE_6 AS VARCHAR(10)),
      *         CAST(LNFILE.LN_INSEFF_DATE_7 AS VARCHAR(10)),
      *         CAST(LNFILE.LN_INSEFF_DATE_8 AS VARCHAR(10)),
      *         CAST(LNFILE.LN_INSEFF_DATE_9 AS VARCHAR(10)),
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
      *  FROM DBO.TRLFILE TRLFILE
      *  INNER JOIN DBO.LNFILE LNFILE
      *    ON LNFILE.LN_OWNBR = TRLFILE.TL_BRNO
      *   AND LNFILE.LN_ACCTNO = TRLFILE.TL_ACCTNO
      *  WHERE TRLFILE.TL_BRNO = @QTRL1_WBEG_BRNO
      *    AND TRLFILE.TL_DATE  BETWEEN @QTRL1_WBEG_DATE  AND @QTRL1_WEND_DATE
      *    AND TRLFILE.TL_CLASS BETWEEN @QTRL1_WBEG_CLASS AND @QTRL1_WEND_CLASS
      *    AND TRLFILE.TL_INSCOMP_3 != 0
      *    AND TRLFILE.TL_INSOURS_3 = 'Y'
      * ORDER BY TRLFILE.TL_DATE,
      *          TRLFILE.TL_CLASS,
      *          TRLFILE.TL_ACCTNO,
      *          TRLFILE.TL_LINE

           EXEC SQL SET CONNECTION TRL1_CONNECT END-EXEC.

           EXEC SQL
            DECLARE TRL1_CURSOR CURSOR FOR
                :TRL1-RETURN-CODE =  
            EXEC SSP_PPMORP_TRLFILE_SELECT
              ( :QTRL1-WBEG-BRNO,
                :QTRL1-WBEG-DATE, :QTRL1-WEND-DATE,
                :QTRL1-WBEG-CLASS, :QTRL1-WEND-CLASS)
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
       READ-TRL1-FILE-NEXT.   *> SSP - TRL LN
           PERFORM READ-IT.
           MOVE TRL-PATH-OWNBR  TO TL-BRNO.
           MOVE TRL-PATH-SQL    TO E-FILE.
           MOVE TRL1-KEY        TO E-KEYX.
           INITIALIZE QTRL-REC QLN-REC.

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

           EXEC SQL SET CONNECTION C1 END-EXEC.

      *-----------------------------------------------------------------
       CLOSE-TRL1-FILE.

           PERFORM CLOSE-IT.
           IF ( TRL1-CURSOR-STAT-GOOD )
              EXEC SQL SET CONNECTION TRL1_CONNECT END-EXEC
              EXEC SQL
                   CLOSE TRL1_CURSOR
              END-EXEC
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


       COPY "LIBLP/LPTRS1RN.CPY".
      *-----------------------------------------------------------------
       CHECK-TRS-FILE.
           PERFORM READ-IT.
           MOVE TRS-PATH-SQL TO E-FILE.
           MOVE SPACE TO E-KEYX.

           EXEC SQL
           SELECT CASE WHEN EXISTS 
                  (SELECT 1 FROM DBO.TRSFILE)
                  THEN 'Y' ELSE 'N'
                  END AS X
            INTO :TRS1-CHECK-SW
            FROM DBO.TRSFILE
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO CHECK-TRS-FILE.



       COPY "LIBLP/LPIC1RN.CPY".
       COPY "LIBLP/LPBW1RN.CPY".
       COPY "LIBLP/LPSP1RN.CPY".
       COPY "LIBGB/GBWK1IN.CPY".
       COPY "LIBGB/GBWKIN.CPY".

       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO EXT-EOBUF-ERRCD.

      *******************************************************
       END-PROGRAM SECTION.
      ********************************************************
       END-RUN.
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
              DESTROY PPMORP-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.
           EXIT PROGRAM.
