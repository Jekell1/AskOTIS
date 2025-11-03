       IDENTIFICATION DIVISION.
       PROGRAM-ID.      GATXO1.
       DATE-WRITTEN.    041486.
      ******************************************************************
      *    DESCRIPTION:  GEORGIA TAX REPORTS
      *                  OTHER 1 INSURANCE
      *                  POLICIES ISSUED, PREMIUMS CHARGED, PREMIUMS
      *                     REFUNDED, NET PREMIUMS
      *                  (TAKEN FROM O1MORP)
      *
      * REV:
      *  BAH 041210 NEW FOR WORLD GA TAX REPORT FORM, WORLD PR# 364
      *  BAH 170602 ACTIVATE 8 DIGIT DATES, PD0006
      *  BAH 181210 GLOBAL LNFILE
      *  BAH 20190924 REMOVED ACCESS-CALL LNFILE, TRP, TRL & TRS
      *  BAH 20200827 REMOVED RESET ON BR1 
      *  BLM 20210810 REMOVE TP-CODE, TP-POOLID NO LONGER IN KEY
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
           03  WK-AMT        PIC S9(9)V99 COMP-3 OCCURS 7.

      ******************************************************************
      *    WORK FILE FOR VALID LOANS
      ******************************************************************
       FD  WK-FILE
           LABEL RECORDS ARE STANDARD.
       01  WK-REC.
           03  WK-KEY.
               05  WORK-INSCOMP     PIC 99.
               05  WORK-OWNBR       PIC 9(4).
               05  WORK-CLASS       PIC 9(3).
               05  WORK-ACCTNO      PIC 9(8).

           03  WORK-NEW-O1      PIC S9(5)V99 COMP-3.
           03  WORK-ADJ-O1      PIC S9(5)V99 COMP-3.
           03  WORK-CAN-O1      PIC S9(5)V99 COMP-3.
           03  WORK-CAN-O1-DATE PIC 9(8).
           03  WORK-COM-O1      PIC S9(5)V99 COMP-3.
           03  WORK-ADDON-FG    PIC X.
           03  WORK-CANCEL-FG   PIC X.
      ******************************************************************
      *
      *    W O R K I N G - S T O R A G E    S E C T I O N
      *
      ******************************************************************
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)LP/GATXO1.CBL S34 08/11/21-11:20:47 >".

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
      *-----------------------------------------------------------------
       01  WK1-IFN              PIC X(3)     VALUE "WK1".
       01  WK-IFN               PIC X(2)     VALUE "WK".
       01  WK1-PATH             PIC X(35).

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

       01  REC-EXISTS           PIC X        VALUE "N".
       01  TOTAL-FG             PIC X        VALUE "N".
       01  COMPANY-FG           PIC X        VALUE "N".

       01  ONE-O1               PIC S9       VALUE 0.
       01  NEW-COVERAGE         PIC S9(7)V99 COMP-3 VALUE 0.
       01  RPT-END-DATE         PIC 9(8)     COMP-3 VALUE 0.
       01  BEGINNING-DATE       PIC 9(8)     COMP-3 VALUE 0.

       01  O1-ADDON-FG          PIC X.
       01  O1-CANCEL-FG         PIC X.
       01  O1-PREM              PIC S9(7)V99 COMP-3.
       01  O1-ADJ               PIC S9(7)V99 COMP-3.
       01  O1-CAN               PIC S9(7)V99 COMP-3.
       01  O1-CAN-DATE          PIC 9(8)     COMP-3.
       01  O1-COMM              PIC S9(7)V99 COMP-3.

       01  LINE-TOTALS.
           03  TOT-QTY          PIC S9(7)    COMP-3 OCCURS 2.
           03  TOT-AMT          PIC S9(9)V99 COMP-3 OCCURS 7.

       01  CLASS-FG             PIC X(3)         VALUE "   ".
           88  CLASS-BEG                         VALUE "BEG".
           88  CLASS-END                         VALUE "END".
      ******************************************************************
      *     S C R E E N   F I E L D S
      ******************************************************************
       COPY "LIBGB/EOBUF_EXT.CPY".
      *-----------------------------------------------------------------
           03  BEG-BRANCH      PIC 9(4).
           03  END-BRANCH      PIC 9(4).
           03  BEG-IC          PIC 99.
           03  END-IC          PIC 99.
           03  BEG-CLASS       PIC 9(3).
           03  END-CLASS       PIC 9(3).
           03  BEG-ST          PIC XX.
           03  END-ST          PIC XX.
           03  BEG-DATE.
               05  BEG-MONTH   PIC 9(2).
               05  BEG-YEAR    PIC 9(2).
           03  BDATE REDEFINES BEG-DATE PIC 9(4).
           03  END-DATE.
               05  END-MONTH   PIC 9(2).
               05  END-YEAR    PIC 9(2).
           03  EDATE REDEFINES END-DATE PIC 9(4).
           03  SUMMARY-ONLY    PIC X.
           03  GROUP-FLAG      PIC X.
           03  ENT-GROUP       PIC X(4).

       01  VERSION-CONTROL     PIC X    VALUE "A".

       01  SV-BEG-BR           PIC 9999   VALUE 9999.
       01  SV-END-BR           PIC 9999   VALUE 9999.
       01  SV-GROUP-FLAG       PIC X     VALUE SPACES.
       01  SV-ENT-GROUP        PIC X(4)  VALUE SPACES.

      ******************************************************************
      *     P R I N T   B U F F E R S
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

      *-----------------------------------------------------------------
       01  HEAD2.
           03  FILLER           PIC X(10)    VALUE "USER ID:".
           03  H-USERID         PIC X(10) VALUE " ".
           03  FILLER           PIC X(26)    VALUE " ".
           03  FILLER           PIC X(41) VALUE
           "-OTHER 1 INSURANCE PREMIUM-REFUND REPORT-".

      *-----------------------------------------------------------------
       01  HEAD3.
           03  H-INSCO          PIC X(10) VALUE "INS CO :".
           03  H-ICNO           PIC 99 VALUE 0.
           03  FILLER           PIC X VALUE " ".
           03  H-ICNAME         PIC X(20) VALUE " ".
           03  FILLER           PIC X(23) VALUE " ".
           03  H-DATE1          PIC 99/99/99.
           03  FILLER           PIC X(6) VALUE " THRU ".
           03  H-DATE2          PIC 99/99/99.

      *-----------------------------------------------------------------
       01  HEAD4                VALUE " ".
           03  H-PAGE-DESC      PIC X(46).
           03  H-STNO.
               05  H-BRNO       PIC Z(4).
           03  FILLER           PIC X.
           03  H-BRNAME         PIC X(20).
           03  FILLER           PIC X(45).
           03  H-PAGE-PROMPT    PIC X(12).
           03  H-BR-PAGE        PIC ZZZZ.

      *-----------------------------------------------------------------
       01  HEAD5.
           03  FILLER           PIC X(51) VALUE
           "ACCOUNT #      BORROWER           AGE EFF-DATE TRM ".
           03  FILLER           PIC X(53) VALUE
           "EXP-DATE  COVERAGE    BENEFIT    PREMIUM  ADJUSTMNT  ".
           03  FILLER           PIC X(27) VALUE
           "  REFUND----DATE  COMMISSON".

      *-----------------------------------------------------------------
       01  HEAD7.
           03  FILLER           PIC X(48) VALUE
           "TOT  **NEW ACCOUNTS**              LOAN        ".
           03  FILLER           PIC X(77) VALUE
           "NEW LOAN         ADDON".
           03  FILLER           PIC X(3) VALUE
           "NET".

      *-----------------------------------------------------------------
       01  HEAD8.
           03  FILLER           PIC X(50) VALUE
           "CLS                              COVERAGE       PR".
           03  FILLER           PIC X(59) VALUE
           "EMIUMS       PREMIUMS    ADJUSTMENTS        CANCELS".
           03  FILLER           PIC X(22) VALUE
           "REFUNDS     COMMISSION".

      ******************************************************************
      *    D E T A I L - L I N E
      ******************************************************************
       01  DETAIL-LINE          PIC X(132) VALUE SPACES.

      *-----------------------------------------------------------------
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

      *-----------------------------------------------------------------
       01  DETAIL2 REDEFINES DETAIL-LINE.
           03  FILLER           PIC X(3).
           03  D-QTY            PIC Z(5)9- OCCURS 3.
           03  FILLER           PIC X(3).
           03  D-AMT            PIC ZZZ,ZZZ,ZZ9.99- OCCURS 7.

      *-----------------------------------------------------------------
       01  RANGE-LINE REDEFINES DETAIL-LINE.
           03  FILLER           PIC X(20).
           03  RANGE-SEL        PIC X(112).
      ******************************************************************
      *
      *     C O P Y    L I B R A R Y     W O R K E R S
      *
      ******************************************************************
       COPY "LIBWI/GRSCANW.CPY".
       COPY "LIBLP/LPWKPATH.CPY".
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
       COPY "LIBLP/GATXO1_DEF.CPY".
       COPY "LIBLP/GATXO1_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

      *****************************************************************
      *
      *     P R O G R A M   C O N T R O L
      *
      *****************************************************************
       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBLP/GATXO1_SCN.CPY".

      *----------------------------------------------------------------
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
           MOVE "GATXO1" TO VDU-FORM-NAME.

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
           MOVE GATXO1-QUEUE-POS TO Q-POS.
           MOVE GATXO1-COPIES-POS TO C-POS.
           MOVE GATXO1-PRU-POS   TO P-POS.
           MOVE SPACES TO Q-BUF C-BUF P-BUF.

       COPY "LIBGB/EOINIT.CPY".

      * CREATE TOTALS WORK FILE
           MOVE MKTEMP-DEFAULT TO MKTEMP-BUF.
           PERFORM MKTEMP-CALL.
           MOVE MKTEMP-BUF TO WK1-PATH.
           PERFORM OPEN-WK1-FILE-OUTPUT.
           PERFORM CLOSE-WK1-FILE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      * CREATE WORK FILE FOR VALID LOANS
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
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
           PERFORM OPEN-SP1-FILE.
           PERFORM OPEN-PR-FILE.

           MOVE EXT-CONAME-CONAME TO H-NAME.
           MOVE EXT-CONAME-SYSDATE TO H-DATE.
           MOVE EXT-CONAME-USER-ID TO H-USERID.

           MOVE BEG-MONTH   TO DATE-MMDDYY-MM.
           MOVE BEG-YEAR    TO DATE-MMDDYY-YY.
           MOVE 1           TO DATE-MMDDYY-DD.
           MOVE DATE-MMDDYY TO H-DATE1.
           PERFORM CONVERT-MMDDYY-TO-YYYYMMDD.
           MOVE DATE-YYYYMMDD TO BEGINNING-DATE.

           MOVE END-MONTH TO DATE-MMDDYY-MM.
           MOVE END-YEAR  TO DATE-MMDDYY-YY.
           MOVE 50        TO DATE-MMDDYY-DD.
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
           IF BEG-ST NOT > END-ST GO TO ENTER-BEG-DATE.
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
       ENTER-BEG-DATE.
           IF EXT-ROUTE-BUF NOT = "00"
              GO TO SO-01.

           MOVE "ENNN040DT1." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO ST-02.
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
              GO TO SO-01.

           MOVE "ENNN040DT2." TO VDU.
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

      *-----------------------------------------------------------------
       SO-01.
           MOVE "E  A010SO1." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO ENTER-END-DATE.
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
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "SNNN020IC1." TO VDU.
           MOVE BEG-IC TO VDUNUM0.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "SNNN020IC2." TO VDU.
           MOVE END-IC TO VDUNUM0.
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
           MOVE "S  A020ST1." TO VDU.
           MOVE BEG-ST TO VDUBUF.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "S  A020ST2." TO VDU.
           MOVE END-ST TO VDUBUF.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "S  A010SO1." TO VDU.
           MOVE SUMMARY-ONLY TO VDUBUF.
           PERFORM SCREENIO.

      ******************************************************************
       RUN-REPORT SECTION.
      ******************************************************************

           MOVE 0 TO O1-PREM O1-ADJ O1-CAN O1-COMM O1-CAN-DATE.
           MOVE SPACES TO O1-ADDON-FG O1-CANCEL-FG.

           PERFORM READ-WK-FILE-NEXT.
           IF IO-FG = 9
              GO TO END-RUN-REPORT.

           IF WORK-OWNBR NOT = HOLD-LN-BR
              MOVE WORK-OWNBR TO HOLD-LN-BR BR-NO
              PERFORM GET-LN-BW-PATH.

           MOVE WORK-OWNBR  TO LN-OWNBR.
           MOVE WORK-ACCTNO TO LN-ACCTNO.
           PERFORM READ-LN1-FILE.
           IF IO-FG NOT = 0
              GO TO END-RUN-REPORT.

       KEY-FILE-LOOP.

           ADD WORK-NEW-O1 TO O1-PREM.

           IF WORK-NEW-O1 NOT = 0
              MOVE WORK-ADDON-FG TO O1-ADDON-FG.

           ADD WORK-ADJ-O1 TO O1-ADJ.
           ADD WORK-CAN-O1 TO O1-CAN.

           IF WORK-CAN-O1 NOT = 0
              MOVE WORK-CANCEL-FG TO O1-CANCEL-FG.

           ADD WORK-COM-O1 TO O1-COMM.

           IF WORK-CAN-O1-DATE NOT = 0
              MOVE WORK-CAN-O1-DATE TO O1-CAN-DATE.

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
              PERFORM READ-SP1-FILE
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

           MOVE WORK-OWNBR  TO LN-OWNBR.
           MOVE WORK-ACCTNO TO LN-ACCTNO.
           PERFORM READ-LN1-FILE.
           IF IO-FG NOT = 0
              GO TO END-RUN-REPORT.

           GO TO KEY-FILE-LOOP.

       END-RUN-REPORT.

      ******************************************************************
       SORT-TRP-FILE SECTION.
      ******************************************************************

           PERFORM OPEN-TRP1-FILE.

           MOVE BR-NO               TO TP-BRNO
                                       QTRP1-WBEG-BRNO
                                       QTRP1-WEND-BRNO.
           MOVE BEGINNING-DATE      TO TP-DATE SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO QTRP1-WBEG-DATE.
           MOVE RPT-END-DATE        TO  SQL-DATE-YYYYMMDD.
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

           PERFORM START-TRP1-FILE.

       SORT-TRP-NEXT.
           PERFORM READ-TRP1-FILE-NEXT.
           IF (IO-FG NOT = 0) OR (TRP-PATH-OWNBR NOT = TP-BRNO)
              GO TO SORT-TRP-EXIT.

           IF TP-DATE > RPT-END-DATE
              GO TO SORT-TRP-EXIT.

           IF TP-CLASS < BEG-CLASS OR TP-CLASS > END-CLASS
              GO TO SORT-TRP-NEXT.

           IF TP-ADDON-TYPE NOT = "O1"
              IF TP-TRCD NOT = "R1" AND NOT = "C1"
                 GO TO SORT-TRP-NEXT.

           MOVE TP-BRNO   TO LN-OWNBR.
           MOVE TP-ACCTNO TO LN-ACCTNO.
           PERFORM READ-LN1-FILE.
           IF IO-FG  NOT = 0
              GO TO SORT-TRP-NEXT.

           IF LN-ORGST < BEG-ST OR LN-ORGST > END-ST
              GO TO SORT-TRP-NEXT.

      *    IF LN-OWNBR NOT = TP-BRNO    <<< THIS DOES NOT WORK ANYMORE !!
      * IF ANOTHER BRANCH DOES A PAYOFF, AND THE SAME LOAN # EXIST
      * IN BOTH BRANCHES, (NOT REALLY THE SAME LOAN!), IT WILL PRINT FOR
      * BOTH BRANCHES, SHOULD ONLY PRINT FOR THE OWNING BRANCH !!

      * ONLY REPORT FOR THE OWNING BRANCH ("G")
      *    IF TP-CODE = "P"
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
              MOVE LN-INSCOMP(4) TO WORK-INSCOMP
              IF TP-INSCOMP NUMERIC AND TP-INSCOMP NOT = 0
                  MOVE TP-INSCOMP TO WORK-INSCOMP.

           IF WORK-INSCOMP = 0
              GO TO SORT-TRP-NEXT.

           IF WORK-INSCOMP < BEG-IC OR WORK-INSCOMP > END-IC
              GO TO SORT-TRP-NEXT.

           MOVE TP-BRNO   TO WORK-OWNBR.
           MOVE TP-CLASS  TO WORK-CLASS.
           MOVE TP-ACCTNO TO WORK-ACCTNO.
           PERFORM READ-WK-FILE.
           IF IO-FG NOT = 0
              MOVE 0 TO WORK-NEW-O1 WORK-ADJ-O1 WORK-CAN-O1
                        WORK-CAN-O1-DATE WORK-COM-O1
              MOVE SPACES TO WORK-ADDON-FG WORK-CANCEL-FG.

           IF TP-ADDON-TYPE = "O1" AND
                    TP-ADDON-TEXT = "ADD"
              MOVE "A" TO WORK-ADDON-FG
              COMPUTE WORK-NEW-O1 = WORK-NEW-O1 +
                           (TP-APCUR - TP-GLAMT(1) - TP-GLAMT(2))
              COMPUTE WORK-COM-O1 = WORK-COM-O1 + TP-INSCOMM
           ELSE
           IF TP-ADDON-TYPE = "O1" AND
                    TP-ADDON-TEXT = "ADJ"
              COMPUTE WORK-ADJ-O1 = WORK-ADJ-O1 +
                           (TP-APCUR - TP-GLAMT(1) - TP-GLAMT(2))
              COMPUTE WORK-COM-O1 = WORK-COM-O1 + TP-INSCOMM
           ELSE
           IF TP-ADDON-TYPE = "O1" AND
                    TP-ADDON-TEXT = "CNL"
              MOVE "C" TO WORK-CANCEL-FG
              COMPUTE WORK-CAN-O1 = WORK-CAN-O1 +
                           (TP-APCUR - TP-GLAMT(1) - TP-GLAMT(2))
              MOVE TP-PAYDATE TO WORK-CAN-O1-DATE
              COMPUTE WORK-COM-O1 = WORK-COM-O1 + TP-INSCOMM
           ELSE
           IF TP-TRCD = "R1"
              COMPUTE WORK-CAN-O1 = WORK-CAN-O1 +
                                ((TP-TRAMT - TP-INS-TAX-RB) * -1)
              MOVE TP-PAYDATE TO WORK-CAN-O1-DATE
              COMPUTE WORK-COM-O1 = WORK-COM-O1 + (TP-INSCOMM * -1)
           ELSE
           IF TP-TRCD = "C1"
              MOVE "C" TO WORK-CANCEL-FG
              COMPUTE WORK-CAN-O1 = WORK-CAN-O1 +
                                ((TP-TRAMT - TP-INS-TAX-RB) * -1)
              MOVE TP-PAYDATE TO WORK-CAN-O1-DATE
              COMPUTE WORK-COM-O1 = WORK-COM-O1 + (TP-INSCOMM * -1).

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
           MOVE 0                   TO TL-ACCTNO TL-LAWCODE TL-LINE
                                       QTRL1-WBEG-ACCTNO
                                       QTRL1-WBEG-LAWCODE
                                       QTRL1-WBEG-LINE.
           MOVE ALL "9"             TO QTRL1-WEND-ACCTNO
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


           IF TL-INSCOMP(4) = 0
              GO TO SORT-TRL-NEXT
           ELSE
           IF TL-INSCOMP(4) < BEG-IC OR > END-IC
              GO TO SORT-TRL-NEXT
           ELSE
           IF TL-INSOURS(4) NOT = "Y"
              GO TO SORT-TRL-NEXT.

           MOVE TL-BRNO   TO LN-OWNBR.
           MOVE TL-ACCTNO TO LN-ACCTNO.
           PERFORM READ-LN1-FILE.
           IF IO-FG  NOT = 0
              GO TO SORT-TRL-NEXT.

           IF LN-ORGST < BEG-ST OR LN-ORGST > END-ST
              GO TO SORT-TRL-NEXT.


           MOVE TL-INSCOMP(4) TO WORK-INSCOMP.

           MOVE TL-BRNO   TO WORK-OWNBR.
           MOVE TL-CLASS  TO WORK-CLASS.
           MOVE TL-ACCTNO TO WORK-ACCTNO.
           PERFORM READ-WK-FILE.
           IF IO-FG NOT = 0
              MOVE 0 TO WORK-NEW-O1 WORK-ADJ-O1 WORK-CAN-O1
                        WORK-CAN-O1-DATE WORK-COM-O1
              MOVE SPACES TO WORK-ADDON-FG WORK-CANCEL-FG.

           COMPUTE WORK-NEW-O1 = WORK-NEW-O1 + TL-INSPREM(4).
           COMPUTE WORK-COM-O1 = WORK-COM-O1 + TL-INSCOMM(4).

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

           IF TS-INSCOMP(4) = 0
              GO TO SORT-TRS-NEXT
           ELSE
           IF TS-INSCOMP(4) < BEG-IC OR > END-IC
              GO TO SORT-TRS-NEXT
           ELSE
           IF TS-INSOURS(4) NOT = "Y"
              GO TO SORT-TRS-NEXT.


           MOVE TS-BRNO   TO LN-OWNBR.
           MOVE TS-ACCTNO TO LN-ACCTNO.
           PERFORM READ-LN1-FILE.
           IF IO-FG  NOT = 0
              GO TO SORT-TRS-NEXT.

           IF LN-ORGST < BEG-ST OR LN-ORGST > END-ST
              GO TO SORT-TRS-NEXT.


           MOVE TS-INSCOMP(4) TO WORK-INSCOMP.

           MOVE TS-BRNO   TO WORK-OWNBR.
           MOVE TS-CLASS  TO WORK-CLASS.
           MOVE TS-ACCTNO TO WORK-ACCTNO.
           PERFORM READ-WK-FILE.
           IF IO-FG NOT = 0
              MOVE 0 TO WORK-NEW-O1 WORK-ADJ-O1 WORK-CAN-O1
                        WORK-CAN-O1-DATE WORK-COM-O1
              MOVE SPACES TO WORK-ADDON-FG WORK-CANCEL-FG.

           COMPUTE WORK-NEW-O1 = WORK-NEW-O1 + TS-INSPREM(4).
           COMPUTE WORK-COM-O1 = WORK-COM-O1 + TS-INSCOMM(4).

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

      *    -------------------------------------------------------------
      *    NEED TO HAVE BW-SOLICIT FOR LN-SSNO(1) IN LPSTAT-DISPLAY
      *    -------------------------------------------------------------
           IF LN-INSURED = "S"
              MOVE LN-SSNO(1) TO BW-SSNO
              PERFORM READ-BW1-FILE.

           PERFORM LPSTAT-DISPLAY.
           MOVE LPS-DISPLAY-LNNO TO D-LOAN-NUM.

           IF O1-PREM = 0 AND O1-ADJ = 0 AND O1-CAN = 0 AND
               O1-COMM = 0
              GO TO SKIP-O1.

           MOVE 4 TO ITRM-SUB.
           PERFORM INS-TERM-CALCULATION.
           MOVE ITRM-INSTERM TO D-TERM.

           MOVE LN-INSEFF-DATE(4) TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO D-EFFDATE.
           MOVE LN-INSEXP-DATE(4) TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO D-EXPDATE.
           MOVE LN-INSCOVR(4) TO D-COVRG D-REGPYAMT.

           MOVE O1-PREM TO D-INSPREM.
           MOVE O1-ADDON-FG TO D-ADDTEXT.
           MOVE O1-ADJ TO D-INSADJ.
           MOVE O1-CAN TO D-INSREFUND.
           MOVE O1-CANCEL-FG TO D-CNLTEXT.

           IF O1-CAN NOT = 0
              MOVE O1-CAN-DATE TO DATE-YYYYMMDD
              PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
              MOVE DATE-MMDDYY TO D-REFUND-DATE.

           MOVE O1-COMM TO D-INSCOMM.

      *-----------------------------------------------------------------
       SKIP-O1.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      *-----------------------------------------------------------------
       END-KEY-DETAIL.
           PERFORM CREATE-TOTAL-RECS.
           MOVE 0 TO O1-PREM O1-ADJ O1-CAN O1-COMM O1-CAN-DATE.
           MOVE SPACES TO O1-ADDON-FG O1-CANCEL-FG.

       EXIT-KEY-DETAIL.
           EXIT.

      ******************************************************************
       CREATE-TOTAL-RECS SECTION.
      ******************************************************************
           MOVE 0 TO ONE-O1.

           IF LN-INSCOMP(4) NOT = 0
              MOVE LN-INSCOVR(4) TO NEW-COVERAGE
              IF O1-PREM = 0
                 MOVE 0 TO NEW-COVERAGE
              ELSE
                 IF O1-PREM < 0
                    MULTIPLY -1 BY NEW-COVERAGE.

           IF O1-PREM > 0
              MOVE 1 TO ONE-O1
           ELSE
              IF O1-PREM < 0
                 MOVE -1 TO ONE-O1.

      *-----------------------------------------------------------------
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    WK-CODE: 1-BR, 2-ST, 3-IC, 4-GB
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
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

      ******************************************************************
       CREATE-WK SECTION.
      ******************************************************************
           MOVE WK1-KEY TO SAVE-WK1-KEY.
           MOVE "N" TO REC-EXISTS.

      *-----------------------------------------------------------------
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

      *-----------------------------------------------------------------
       UPDATE-WK1-REC.
           ADD ONE-O1 TO WK-QTY(1).
           ADD NEW-COVERAGE TO WK-AMT(1).

           IF O1-ADDON-FG = "A"
              ADD O1-PREM TO WK-AMT(6)
           ELSE
              ADD O1-PREM TO WK-AMT(2).

           ADD O1-ADJ TO WK-AMT(3).

           IF O1-CANCEL-FG = "C"
              ADD O1-CAN TO WK-AMT(7)
           ELSE
              ADD O1-CAN TO WK-AMT(4).

           ADD O1-COMM TO WK-AMT(5).
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

      * TURNS ON/OFF TOTAL-HEADINGS.
           MOVE 99 TO LN-COUNT.
           MOVE "Y" TO TOTAL-FG.
           MOVE "Y" TO FIRST-TOT.
           MOVE " " TO WK1-KEY.
           MOVE SAVE-CODE TO WK-CODE.
           MOVE HOLD-IC TO WK-IC.
           MOVE SAVE-ST TO WK-ST.
           MOVE 0 TO WK-CLASS.
           PERFORM START-WK1-FILE.

      *-----------------------------------------------------------------
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

      *-----------------------------------------------------------------
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

      *-----------------------------------------------------------------
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
           MOVE "END INS CO" TO RANGE-LINE.
           MOVE END-IC TO RANGE-SEL.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "BEG ORIG STATE" TO RANGE-LINE.
           MOVE BEG-ST TO RANGE-SEL.
           PERFORM WRITE-DETAIL-LINE.
           MOVE "END ORIG STATE" TO RANGE-LINE.
           MOVE END-ST TO RANGE-SEL.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "BEG LOAN CLASS" TO RANGE-LINE.
           MOVE BEG-CLASS TO RANGE-SEL.
           PERFORM WRITE-DETAIL-LINE.
           MOVE "END LOAN CLASS" TO RANGE-LINE.
           MOVE END-CLASS TO RANGE-SEL.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "BEGIN DATE" TO RANGE-LINE.
           MOVE BDATE TO RANGE-SEL.
           PERFORM WRITE-DETAIL-LINE.
           MOVE "END   DATE" TO RANGE-LINE.
           MOVE EDATE TO RANGE-SEL.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "SUMMARY ONLY ?" TO RANGE-LINE.
           MOVE SUMMARY-ONLY TO RANGE-SEL
           PERFORM WRITE-DETAIL-LINE.
           PERFORM WRITE-DETAIL-LINE.

      ******************************************************************
      *
      *     C O P Y    L I B R A R Y    S E C T I O N S
      *
      ******************************************************************
       COPY "LIBLP/LPSTAT.CPY".
       COPY "LIBLP/LPMDTE.CPY".
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
           MOVE WK-AMT(5) TO D-AMT(7).
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      *****************************************************************
       CLASS-SCAN SECTION.
      *****************************************************************
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

      **************************************************
       FORM-RESET SECTION.
      **************************************************
           MOVE EXT-CONAME-CONAME TO GATXO1-CONAME.
           MOVE EXT-CONAME-SYSDATE TO GATXO1-SYSDATE.
           DISPLAY GATXO1-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE GATXO1-FORM-FIELDS TO WR-BUF.
           MOVE GATXO1-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.


      **************************************************
       SETUP-LINK-INFO SECTION.
      **************************************************
           MOVE GATXO1-HELP--FILE TO HELP-LINK-FILE.
           MOVE GATXO1-HELP--DIR TO HELP-LINK-DIR.

      **************************************************
       VDU-CONVERT-FIELD SECTION.
      **************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBLP/GATXO1_EVA.CPY".
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

      *-----------------------------------------------------------------
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

      *-----------------------------------------------------------------
       INIT-WK1-KEY.
           MOVE " " TO WK1-KEY.
           MOVE HOLD-IC TO WK-IC.
           MOVE LN-CLASS TO WK-CLASS.

      *-----------------------------------------------------------------
       TOTAL-LINE.
           MOVE "ALL" TO DETAIL-LINE.
           MOVE TOT-QTY(1) TO D-QTY(1).
           MOVE TOT-AMT(1) TO D-AMT(1).
           MOVE TOT-AMT(2) TO D-AMT(2).
           MOVE TOT-AMT(6) TO D-AMT(3).
           MOVE TOT-AMT(3) TO D-AMT(4).
           MOVE TOT-AMT(7) TO D-AMT(5).
           MOVE TOT-AMT(4) TO D-AMT(6).
           MOVE TOT-AMT(5) TO D-AMT(7).
           MOVE 3 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.
           PERFORM CLEAR-TOTALS.
           MOVE 99 TO LN-COUNT.

      *-----------------------------------------------------------------
       CLEAR-TOTALS.
           MOVE 0 TO
              TOT-AMT(1) TOT-AMT(2) TOT-AMT(3) TOT-AMT(4) TOT-AMT(5)
              TOT-AMT(6) TOT-AMT(7) TOT-QTY(1) TOT-QTY(2).

      *-----------------------------------------------------------------
       INVALID-RANGE.
           MOVE "INVALID RANGE" TO MESS.
           PERFORM SEND-MESS.

      *-----------------------------------------------------------------
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
       COPY "LIBLP/LPLN1RN.CPY".
       COPY "LIBLP/LPTRP1RN.CPY".
       COPY "LIBLP/LPTRL1RN.CPY".
       COPY "LIBLP/LPTRS1RN.CPY".
       COPY "LIBLP/LPIC1RN.CPY".
       COPY "LIBLP/LPBW1RN.CPY".
       COPY "LIBLP/LPSP1RN.CPY".
       COPY "LIBGB/GBWK1IN.CPY".
       COPY "LIBGB/GBWKIN.CPY".

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
              MOVE "NO REPORT FOR SELECTED RANGE" TO MESS
              PERFORM SEND-MESS.

           MOVE MKTEMP-BUF TO ACCESS-BUF.
           PERFORM REMOVE-WORKFILE.
           MOVE WK1-PATH TO ACCESS-BUF.
           PERFORM REMOVE-WORKFILE.

      *-----------------------------------------------------------------
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
              DESTROY GATXO1-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.
           EXIT PROGRAM.
