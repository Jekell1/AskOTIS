       IDENTIFICATION DIVISION.
       PROGRAM-ID.      UNEXTR.
       DATE-WRITTEN.    06/11/2010.
      ******************************************************************
      *   DIR : SP
      *   DESC: UNEARNED INSURANCE PREMIUM EXTRACTION
      *
      *           CL,AH,PP,O1,O2,O3
      *
      * ASCII FILE:  /USR/DATA/R#/SP/UNEARN.MMYY     RSZ = 24
      *
      **************************************************************************
      * REVS:
      *  BAH 100611 NEW FOR REGMGM PR# 3613
      *   CS 160121 REMOVED BR-REB-METHOD-2, CL-LEVL IS NOW BR-REB-METHOD(7)
      *             O3 NOW BR-REB-METHOD(6)
      *   IN A30 
      *    CL(DEC)  -  1      O2       - 5     
      *    AH       -  2      O3       - 6     O5     -  9 
      *    PP       -  3      CL(LEV)  - 7     O6     - 10      
      *    O1       -  4      O4       - 8     O7     - 11
      *  BAH 160203 SPLIT OUT LTFILE, PD0003
      * MJD 171002 ACTIVATE 8 DIGIT DATES, PD0006
      * BAH 2018.07.02 ADDED ADDONSPRW AND SET ADDONSPR-SUB INSTEAD OF
      *                REB-SUB2. DONT SHARE REBATEW WORKERS ANYMORE!
      * BAH 181217 GLOBAL LNFILE
      * BAH 190116 GLOBAL LXFILE, SPLIT
      * BAH 20190308 CHANGED LP-ADDON TO LP-REFNO FOR THE MOVE
      * BAH 20191007 REMOVED ACCESS-CALL LNFILE
      * BAH 20200831 REMOVED RESET ON BR1 
      * BAH 20220803 HARD CODED START
      **************************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "LIBGB/GBFSPR.CPY".
       COPY "LIBGB/GBFSWKI.CPY".
       SELECT EXTRACT-FILE ASSIGN EXTRACT-PATH
              ORGANIZATION RELATIVE
              ACCESS DYNAMIC
              LOCK MODE AUTOMATIC WITH LOCK ON RECORD
              RELATIVE KEY EXTRACT-KEY
              FILE STATUS FILE-STAT.
       I-O-CONTROL.
       DATA DIVISION.
       FILE SECTION.

       COPY "LIBGB/GBFDPR.CPY".

      ******************************************************************
      *
      *   SUMMARY TOTALS WORK RECORD
      *
      ******************************************************************
       FD  WK-FILE
           LABEL RECORDS ARE STANDARD.
       01  WK-REC.
           03  WK-KEY.
               05  WK-BR     PIC 9(4).
               05  WK-CLASS  PIC 9(3).
           03  WK-LL-OR-DL   PIC X.
           03  WK-END        PIC S9(7)V99 COMP-3 OCCURS 6.


      * ASCII FILE:  /USR/DATA/R#/SP/UNEARN.MMYY     RSZ = 24
       FD  EXTRACT-FILE
           LABEL RECORDS ARE STANDARD.
       01  EXTRACT-REC.
           03  EXT-BRNO                 PIC 9(4).
           03  EXT-CLASS                PIC 99.
           03  EXT-INS-TYPE             PIC X(2).
           03  EXT-UNEARN-AMT           PIC -9(10).99.
           03  EXT-CR                   PIC X.
           03  EXT-LF                   PIC X.


      ******************************************************************
      *
      *    W O R K I N G - S T O R A G E   S E C T I O N
      *
      ******************************************************************
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)SP/UNEXTR.CBL S35 02/22/24-08:18:58 >".
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBGB/GB01GR.CPY".
       COPY "LIBGB/GB01GR_SQL.CPY".
       COPY "LIBGB/GBWSGR.CPY".
       COPY "LIBLP/LP01LN.CPY".
       COPY "LIBLP/LP01LN_SQL.CPY".
       COPY "LIBLP/LPWSLN.CPY".
       COPY "LIBLP/LP01LX.CPY".
       COPY "LIBLP/LP01LX_SQL.CPY".
       COPY "LIBLP/LPWSLX.CPY".
       COPY "LIBLP/LP01LTI.CPY".
       COPY "LIBLP/LP01LTI_SQL.CPY".
       COPY "LIBLP/LPWSLTI.CPY".
       COPY "LIBLP/LP01LTP.CPY".
       COPY "LIBLP/LP01LTP_SQL.CPY".
       COPY "LIBLP/LPWSLTP.CPY".
       COPY "LIBLP/LP01IC.CPY".
       COPY "LIBLP/LP01IC_SQL.CPY".
       COPY "LIBLP/LPWSIC.CPY".
       COPY "LIBLP/LP01SP.CPY".
       COPY "LIBLP/LP01SPA.CPY".
       COPY "LIBLP/LP01SPB.CPY".
       COPY "LIBLP/LP01SPC.CPY".
       COPY "LIBLP/LP01SP_SQL.CPY".
       COPY "LIBLP/LPWSSP.CPY".
       COPY "LIBLP/LP01LP.CPY".
       COPY "LIBLP/LP01LP_SQL.CPY".
       COPY "LIBLP/LPWSLP.CPY".
      *-----------------------------------------------------------------
       01  WK-IFN               PIC X(2)     VALUE "WK".
       01  EXTRACT-IFN          PIC X(7)     VALUE "EXTRACT".
       01  EXTRACT-KEY          PIC 9(8)     VALUE 0.
       01  WK-PATH              PIC X(35).

       01  EXTRACT-PATH.
           03  EXTRACT-PATH-BASE          PIC X(09).
           03  FILLER                     PIC X(01) VALUE "/".
           03  EXTRACT-PATH-MACHINE       PIC X(02).
           03  FILLER                     PIC X(11) VALUE "/SP/UNEARN.".
           03  EXTRACT-PATH-DATE          PIC 9(04).

       01  SAVE-WK-KEY          PIC X(7).
      ******************************************************************
      *
      *   LINE WORKERS
      *
      ******************************************************************
       01  LN-WORKERS.
           03  LN-FEED          PIC 999      VALUE 0.
           03  LN-COUNT         PIC 999      VALUE 99.
           03  LN-WK            PIC 999      VALUE 0.
       01  PAGE-NUMBER          PIC 9(4)     VALUE 0.

      ******************************************************************
      *
      *   MISC WORKERS
      *
      ******************************************************************
       01  TOT-WORKERS.
           03  TOT-LV               OCCURS 2.
               05  TOT-LC           OCCURS 101.
                   07  TOT-END      PIC S9(7)V99 COMP-3 OCCURS 6.

      * INS CO (1-99)
       01  IC                   PIC 9(6) COMP-3.

      * LOAN CLASS (1-101)
       01  LC                   PIC 9(6) COMP-3.

      * TOTAL LEVEL (1-ST 2-IC)
       01  LV                   PIC 9(6) COMP-3.

      * INS TYPE (1-CL 2-AH 3-PP 4-O1 5-O2 6-03)
       01  TP                   PIC 9(6) COMP-3.

       01  HOLD-OWNBR           PIC 9(4) VALUE 9999.

       01  FIRST-TIME           PIC X        VALUE "Y".
       01  TOTAL-TYPE           PIC X(6)     VALUE SPACES.
       01  MESS-LIST            PIC X(5)     VALUE "MSEL.".
       01  CLASS-FG            PIC X(3)  VALUE "   ".
           88  CLASS-BEG                 VALUE "BEG".
           88  CLASS-END                 VALUE "END".
       01  ACCRUAL-MONTHS       PIC 9(3)     OCCURS 6.
       01  REBDAYS-MONTH        PIC 9(2)     OCCURS 6.
       01  XUB                  PIC 9(6)     COMP-3.
       01  IDY                  PIC 9(6)     COMP-3.
       01  COMM-SUB             PIC 999      COMP-3.
       01  SAVE-IC-NAME         PIC X(20).
       01  WK-IC-COMMRATE       PIC S9(3)V99  COMP-3.
       01  WK-COMMISSION        PIC S9(7)V99 COMP-3.
       01  REC-EXISTS           PIC X        VALUE "N".
       01  PROCESS-ERROR.
           03  FILLER           PIC X(30)    VALUE
               "ERROR IN PROCESSING LOAN NO:  ".
           03  PROCESS-NUMBER   PIC 9(6).
       01  METHOD-4-FG          PIC X.
       01  METHOD-4-BEG-PAYDATE PIC 9(8)     COMP-3.
       01  METHOD-4-END-PAYDATE PIC 9(8)     COMP-3.
       01  METHOD-5-FG          PIC X.
       01  METHOD-5-BEG-PAYMNTD PIC 9(7)V99  COMP-3.
       01  METHOD-5-END-PAYMNTD PIC 9(7)V99  COMP-3.
       01  PAYMNTD              PIC 9(7)V99  COMP-3.
       01  SAVE-LN-TOTPAYMNTD   PIC 9(7)V99  COMP-3.
       01  HOLD-LP-ADDON.
           03  HOLD-LP-ADDON-TEXT  PIC X(3).
           03  HOLD-LP-ADDON-TYPE  PIC X(2).

      *-----------------------------------------------------------------
       01  UNRP-SUMW         PIC S9(5)      COMP-3.
       01  UNRP-SUMW1        PIC S9(5)      COMP-3.
       01  UNRP-WORKER       PIC S9(7)V9(8) COMP-3.

      *-----------------------------------------------------------------
       01  RV-CNT               PIC 99.
       01  RV-TABLE.
           03  RV-DATES         OCCURS 50 TIMES.
      * YYYYMMDD
               05  RV-DATE      PIC 9(8).
               05  FILLER       REDEFINES RV-DATE.
                   07  RV-YR    PIC 9999.
                   07  RV-MO    PIC 99.
                   07  RV-DA    PIC 99.

      *-----------------------------------------------------------------

       01  LAST-BR              PIC 9(4)     COMP-3 VALUE 0.

       01  UNEARNED-TAB.
           03  BEG-UNEARNED     PIC S9(7)V99 OCCURS 6.
           03  NEW-PREMIUMS     PIC S9(7)V99 OCCURS 6.
           03  NEW-REFUNDS      PIC S9(7)V99 OCCURS 6.
           03  END-UNEARNED     PIC S9(7)V99 OCCURS 6.
           03  INS-INEFFECT     PIC X        OCCURS 6.
           03  CHK-UNEARNED     PIC X        OCCURS 6.

       01  HOLD-LN-INSCODES     PIC XXX.
       01  HOLD-LN-CL-REBATE    PIC X.
       01  HOLD-LN-AH-REBATE    PIC X.
       01  HOLD-LN-PP-REBATE    PIC X.
       01  HOLD-LN-O1-REBATE    PIC X.
       01  HOLD-LN-O2-REBATE    PIC X.
       01  HOLD-LN-O3-REBATE    PIC X.
       01  HOLD-LN-INSTBL-01.
           03  HOLD-LN-INSTBL       OCCURS 6 TIMES
                                    PIC X(LN-INSTBL-SIZE).

      *    WS-NET-INCOME(1)   = CREDIT LIFE INS. (DECL)
      *    WS-NET-INCOME(2)   = ACCIDENT & HEALTH INS.
      *    WS-NET-INCOME(3)   = PERSONAL PROPERTY INS.
      *    WS-NET-INCOME(4)   = CREDIT LIFE INS. (LEVEL)
      *    WS-NET-INCOME-CALC = THIS IS TO PREVENT ADDING TWICE.
      
       01  WS-FIELDS.
           03  WS-NET-INCOME        PIC S9(7)V99     OCCURS 4 TIMES.
           03  WS-NET-INCOME-FLAG   PIC X(1)         VALUE "N".
           03  WS-NET-INCOME-CALC   PIC X(1)         VALUE "N".
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

      *-----------------------------------------------------------------
       01  HEAD2.
           03  FILLER           PIC X(10)    VALUE "USER ID:".
           03  H-USERID         PIC X(10).
           03  FILLER           PIC X(26)    VALUE " ".
           03  FILLER           PIC X(40)    VALUE
           "   UNEARNED INSURANCE PREMIUM EXTRACT  ".

      *-----------------------------------------------------------------
       01  HEAD3.
           03  FILLER           PIC X(56) VALUE " ".
           03  FILLER           PIC X(13) VALUE "FOR MONTH OF ".
           03  H-MMYY           PIC 99/99.

      *-----------------------------------------------------------------
       01  HEAD4.
           03  FILLER           PIC X(24) VALUE SPACES.
           03  FILLER           PIC X(19) VALUE "LIFE INSURANCE ".
           03  FILLER           PIC X(15) VALUE "A&H INSURANCE".
           03  FILLER           PIC X(20) VALUE "PERSONAL PROPERTY".
           03  FILLER           PIC X(18) VALUE "OTH1 INSURANCE".
           03  FILLER           PIC X(18) VALUE "OTH2 INSURANCE".
           03  FILLER           PIC X(16) VALUE "OTH3 INSURANCE".

      *-----------------------------------------------------------------
       01  HEAD5.
           03  FILLER           PIC X(24) VALUE
           "BRNO            CLASS   ".
           03  FILLER           PIC X(19) VALUE " ENDING UNEARN".
           03  FILLER           PIC X(15) VALUE "ENDING UNEARN".
           03  FILLER           PIC X(20) VALUE "  ENDING UNEARN".
           03  FILLER           PIC X(18) VALUE " ENDING UNEARN".
           03  FILLER           PIC X(18) VALUE " ENDING UNEARN".
           03  FILLER           PIC X(18) VALUE " ENDING UNEARN".

       01  DASH-LINE.
           03  FILLER           PIC X(21)    VALUE ALL "-".
           03  FILLER           PIC X        VALUE SPACES.
           03  FILLER           PIC X(17)    VALUE ALL "-".
           03  FILLER           PIC X        VALUE SPACES.
           03  FILLER           PIC X(17)    VALUE ALL "-".
           03  FILLER           PIC X        VALUE SPACES.
           03  FILLER           PIC X(17)    VALUE ALL "-".
           03  FILLER           PIC X        VALUE SPACES.
           03  FILLER           PIC X(17)    VALUE ALL "-".
           03  FILLER           PIC X        VALUE SPACES.
           03  FILLER           PIC X(17)    VALUE ALL "-".
           03  FILLER           PIC X        VALUE SPACES.
           03  FILLER           PIC X(17)    VALUE ALL "-".
      ******************************************************************
      *
      *    D E T A I L - L I N E
      *
      ******************************************************************
       01  DETAIL-LINE          PIC X(132) VALUE SPACES.

       01  DETAIL1 REDEFINES DETAIL-LINE.
           03  D-BRNO           PIC 9(4).
           03  FILLER           PIC X.
           03  D-BRNAME         PIC X(12).
           03  D-COLON          PIC X.
           03  FILLER           PIC X.
           03  D-CLASS          PIC 99.
           03  FILLER           PIC X.
           03  D-INS            OCCURS 6.
               05  FILLER       PIC X(4).
               05  D-END        PIC Z(6)9.99- BLANK ZERO.
               05  FILLER       PIC X(3).

      *-----------------------------------------------------------------
       01  RANGE-LINE REDEFINES DETAIL-LINE.
           03  FILLER           PIC X(25).
           03  RANGE-SEL        PIC X(107).

      ******************************************************************
      *
      *     C O P Y - L I B R A R Y - W O R K E R S
      *
      ******************************************************************
       COPY "LIBGB/EOBUF_EXT.CPY".
           03  BEG-BRANCH         PIC 9(4).
           03  END-BRANCH         PIC 9(4).
           03  BEG-IC             PIC 99.
           03  END-IC             PIC 99.
           03  BEG-CLASS          PIC 99.
           03  END-CLASS          PIC 99.
           03  BEG-ST             PIC XX.
           03  END-ST             PIC XX.
           03  MMYY               PIC 9(4).
           03  MM-YY              REDEFINES MMYY.
               05  MM             PIC 9(2).
               05  YY             PIC 9(2).
           03  ZBAL-ONLY          PIC X.
           03  REBATE-METHOD9.
               05  REBATE-METHOD   PIC 9 OCCURS 6.
      * REBATE-METHOD(1) FOR CREDIT LIFE
      * REBATE-METHOD(2) FOR A & H
      * REBATE-METHOD(3) FOR OTHER/FIRE
      * REBATE-METHOD(4) FOR OTHER 1 INS
      * REBATE-METHOD(5) FOR OTHER 2 INS
      * REBATE-METHOD(6) FOR OTHER 3 INS
           03  GROUP-FLAG          PIC X(1).
           03  ENT-GROUP           PIC X(4).
      *-----------------------------------------------------
           03  REPORT-ENDING-DATE  PIC 9(8) COMP-3.
           03  PRIOR-ENDING-DATE   PIC 9(8) COMP-3.
           03  PL                  PIC X.

       01  VERSION-CONTROL        PIC X    VALUE "B".
       01  SAVE-BRANCH-NUMBERS.
           03  SV-BEG-BR           PIC 9(4)  VALUE 9999.
           03  SV-END-BR           PIC 9(4)  VALUE 9999.
           03  SV-GROUP-FLAG       PIC X     VALUE SPACES.
           03  SV-ENT-GROUP        PIC X(4)  VALUE SPACES.

       COPY "LIBWI/GRSCANW.CPY".
       COPY "LIBLP/LPAMTSW.CPY".
       COPY "LIBWI/LCSCANW.CPY".
       COPY "LIBLP/ADDONSPRW.CPY".
       COPY "LIBGB/REBATEW.CPY".
       COPY "LIBLP/LPMDTEW.CPY".
       COPY "LIBLP/LPREGZW.CPY".
       COPY "LIBLP/LPAPRZW.CPY".
       COPY "LIBLP/LPAPRSW.CPY".
       COPY "LIBLP/SETCTBLW.CPY".
       COPY "LIBLP/LPUPERW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBLP/LPUPWKW.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBGB/GRWORKW.CPY".
       COPY "LIBGB/PASSWDW.CPY".
       COPY "LIBGB/BRSECUREW.CPY".
       COPY "LIBLP/LPITRMW.CPY".
       COPY "LIBGB/SPPDTHW.CPY".
       COPY "LIBGB/GETPRW.CPY".
       COPY "LIBGB/MKTEMPW_CMD.CPY".
       COPY "LIBGB/SYSTEMW.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/RBACTW.CPY".
       COPY "LIBGB/GPENVW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBSP/UNEXTR_DEF.CPY".
       COPY "LIBSP/UNEXTR_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

      *****************************************************************
      *
      *     P R O G R A M   C O N T R O L
      *
      *****************************************************************
       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBSP/UNEXTR_SCN.CPY".

      *----------------------------------------------------------------
       PROCEDURE DIVISION
           USING FORM-PATHNAME EXIT-PATHNAME.

       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
               PR-FILE WK-FILE EXTRACT-FILE.
       COPY "LIBGB/DECLRP.CPY".
           PERFORM CLOSE-SP1-FILE.
           PERFORM CLOSE-LX1-FILE.
           PERFORM CLOSE-LTP1-FILE.
           PERFORM CLOSE-LTI1-FILE.
           PERFORM CLOSE-LP1-FILE.
           PERFORM CLOSE-LN3-FILE.
           PERFORM CLOSE-IC1-FILE.
           PERFORM CLOSE-GR1-FILE.
           PERFORM CLOSE-BR-FILE.
           PERFORM CLOSE-PR-FILE.
           CLOSE
               WK-FILE EXTRACT-FILE.
       COPY "LIBGB/DECLRP2.CPY".

      ******************************************************************
       MAIN-MODULE SECTION.
      ******************************************************************
           PERFORM SQL-CONNECT.

      D    STOP MESS.
           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "UNEXTR" TO VDU-FORM-NAME.

           PERFORM GET-GPENV.

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
           IF IO-BAD
              GO TO IO-ERROR.

           GO TO MOVE-BRANCH-PATH-INFO.

       NEXT-BRANCH-NO.
           PERFORM READ-BR-FILE-NEXT.
           IF IO-BAD OR BR-NO > END-BRANCH
              GO TO END-ROUTINE.

       MOVE-BRANCH-PATH-INFO.
           MOVE EXT-FILPATH-BASE TO FILPATH-BASE.
           MOVE BR-MACHINE       TO FILPATH-MACHINE.
           MOVE BR-DATA-PATH     TO FILPATH-DATA.
           MOVE "B"        TO IC-PATH-OVERRIDE
                              LN-PATH-OVERRIDE
                              LP-PATH-OVERRIDE
                              LTI-PATH-OVERRIDE
                              LTP-PATH-OVERRIDE
                              LX-PATH-OVERRIDE.

           MOVE BR-REB-METHOD(1) TO REBATE-METHOD(1).
           MOVE BR-REB-METHOD(2) TO REBATE-METHOD(2).
           MOVE BR-REB-METHOD(3) TO REBATE-METHOD(3).
           MOVE BR-REB-METHOD(4) TO REBATE-METHOD(4).
           MOVE BR-REB-METHOD(5) TO REBATE-METHOD(5).
           MOVE BR-REB-METHOD(6) TO REBATE-METHOD(6).

           MOVE BR-REBDAYS-MONTH(1) TO REBDAYS-MONTH(1).
           MOVE BR-REBDAYS-MONTH(2) TO REBDAYS-MONTH(2).
           MOVE BR-REBDAYS-MONTH(3) TO REBDAYS-MONTH(3).
           MOVE BR-REBDAYS-MONTH(4) TO REBDAYS-MONTH(4).
           MOVE BR-REBDAYS-MONTH(5) TO REBDAYS-MONTH(5).
           MOVE BR-REBDAYS-MONTH(6) TO REBDAYS-MONTH(6).

           MOVE BR-ACCRUAL-MONTHS(1) TO ACCRUAL-MONTHS(1).
           MOVE BR-ACCRUAL-MONTHS(2) TO ACCRUAL-MONTHS(2).
           MOVE BR-ACCRUAL-MONTHS(3) TO ACCRUAL-MONTHS(3).
           MOVE BR-ACCRUAL-MONTHS(4) TO ACCRUAL-MONTHS(4).
           MOVE BR-ACCRUAL-MONTHS(5) TO ACCRUAL-MONTHS(5).
           MOVE BR-ACCRUAL-MONTHS(6) TO ACCRUAL-MONTHS(6).

           PERFORM OPEN-LP1-FILE.
           PERFORM OPEN-IC1-FILE.
           PERFORM OPEN-LN3-FILE.
           PERFORM OPEN-LX1-FILE.
           PERFORM OPEN-LTI1-FILE.

           MOVE BR-NO     TO LN-OWNBR
                             QLN3-WBEG-OWNBR
                             QLN3-WEND-OWNBR.
           MOVE BEG-CLASS TO LN-CLASS
                             QLN3-WBEG-CLASS.
           MOVE END-CLASS TO QLN3-WEND-CLASS.
           MOVE 0         TO LN-ACCTNO
                             QLN3-WBEG-ACCTNO.
           MOVE ALL "9"   TO QLN3-WEND-ACCTNO.

           PERFORM START-LN3-FILE.

       NEXT-KEY-REPORT.
           PERFORM READ-LN3-FILE-NEXT.
           IF IO-BAD OR (LN-OWNBR NOT = LN-PATH-OWNBR)
              GO TO END-OF-GROUP.

           IF LN-POCD-INRESCISSION OR LN-POCD-RESCIND
              GO TO NEXT-KEY-REPORT.

      *--------------------------------------------------------------
      * SKIP VOIDED LOANS THAT WERE NOT VOIDED THIS ACCOUNTING MONTH
      * UNWINDS SHOULD BE TREATED SAME AS VOIDS REGACC PL# 575
      *--------------------------------------------------------------
           IF LN-POCD-VOID OR LN-POCD-UNWIND
              MOVE LN-POFFDATE        TO NUM-DATE
              MOVE REPORT-ENDING-DATE TO SYS-DATE
              IF NOT (NUM-MO = S-MM AND NUM-YR = S-YY)
                 GO TO NEXT-KEY-REPORT.

      *--------------------------------------------------------------
      * SKIP VOIDED LOANS BOOKED & VOIDED IN THE SAME ACCOUNTING MONTH
      * UNWINDS SHOULD BE TREATED SAME AS VOIDS REGACC PL# 575
      *--------------------------------------------------------------
           IF LN-POCD-VOID OR LN-POCD-UNWIND
              MOVE LN-POFFDATE TO NUM-DATE
              MOVE LN-ENTDATE  TO SYS-DATE
              IF (NUM-MO = S-MM AND NUM-YR = S-YY)
                 GO TO NEXT-KEY-REPORT.

           IF LN-CLASS > END-CLASS
              GO TO END-OF-GROUP.

           IF LN-CLASS < BEG-CLASS 
              GO TO NEXT-KEY-REPORT.

           IF LN-ORGST < BEG-ST OR LN-ORGST > END-ST
              GO TO NEXT-KEY-REPORT.

           IF ZBAL-ONLY = "Y"
              IF LN-CURBAL NOT = 0
                 GO TO NEXT-KEY-REPORT.

           IF PL = "O"
              IF LN-PLDATE = 0
                 GO TO NEXT-KEY-REPORT.

           IF PL = "S"
              IF LN-PLDATE NOT = 0
                 GO TO NEXT-KEY-REPORT.

           IF LN-INSCOMP(1) < BEG-IC
              IF LN-INSCOMP(2) < BEG-IC
                 IF LN-INSCOMP(3) < BEG-IC
                    IF LN-INSCOMP(4) < BEG-IC
                       IF LN-INSCOMP(5) < BEG-IC
                          IF LN-INSCOMP(6) < BEG-IC
                    GO TO NEXT-KEY-REPORT.

           IF LN-INSCOMP(1) > END-IC
              IF LN-INSCOMP(2) > END-IC
                 IF LN-INSCOMP(3) > END-IC
                    IF LN-INSCOMP(4) > END-IC
                       IF LN-INSCOMP(5) > END-IC
                          IF LN-INSCOMP(6) > END-IC
                    GO TO NEXT-KEY-REPORT.

           MOVE LN-OWNBR TO HOLD-OWNBR.

           MOVE LN-ORGST    TO SP-ORGST.
           MOVE LN-SPRCLASS TO SP-SPRCLASS.
           MOVE LN-SUBCLASS TO SP-SUBCLASS.
           MOVE LN-LAWCODE  TO SP-LAWCODE.
           IF SP1-KEY NOT = LPWSSP-SP1-KEY
              MOVE SP1-KEY TO LPWSSP-SP1-KEY
              PERFORM READ-SP1-FILE
              IF IO-BAD
                 GO TO NEXT-KEY-REPORT.

           IF BR-AUTO-INS-EARN NOT = "Y"
              MOVE LN-INSPREM(1) TO LN-INSCOMM(1)
              MOVE LN-INSPREM(2) TO LN-INSCOMM(2)
              MOVE LN-INSPREM(3) TO LN-INSCOMM(3)
              MOVE LN-INSPREM(4) TO LN-INSCOMM(4)
              MOVE LN-INSPREM(5) TO LN-INSCOMM(5)
              MOVE LN-INSPREM(6) TO LN-INSCOMM(6).

      * IF THEY USE SEPARATE EARNINGS OPTIONS FOR LEVEL LIFE, MUST
      * TEST LEVEL OR DECREASING EACH TIME
      * IF THE NEW FORMULA IS 0, THEN ASSUME THE CUSTOMER DOES NOT
      * USE LEVEL, OR DOES NOT KNOW ABOUT IT, CONTINUE AS BEFORE CHANGE !
      * THE DEFAULTS ARE SET UP ABOVE.
           IF BR-REB-METHOD(7) NOT = 0
              IF LN-INSTYPES-LL
                 MOVE BR-REB-METHOD(7)     TO REBATE-METHOD(1)
                 MOVE BR-REBDAYS-MONTH(7)  TO REBDAYS-MONTH(1)
                 MOVE BR-ACCRUAL-MONTHS(7) TO ACCRUAL-MONTHS(1)
              ELSE
                 MOVE BR-REB-METHOD(1) TO REBATE-METHOD(1)
                 MOVE BR-REBDAYS-MONTH(1) TO REBDAYS-MONTH(1)
                 MOVE BR-ACCRUAL-MONTHS(1) TO ACCRUAL-MONTHS(1).

           MOVE ZEROS TO RV-CNT RV-TABLE.


           PERFORM CREATE-TOTAL-RECS.


           GO TO NEXT-KEY-REPORT.

       END-OF-GROUP.
           PERFORM CLOSE-LN3-FILE.
           PERFORM CLOSE-LP1-FILE.
           PERFORM CLOSE-IC1-FILE.
           PERFORM CLOSE-LX1-FILE.
           PERFORM CLOSE-LTI1-FILE.

           GO TO NEXT-GROUP-LOC.

       END-ROUTINE.
           IF FIRST-TIME = "Y"
              MOVE "NO REPORT FOR SELECTED RANGE" TO MESS
              PERFORM SEND-MESS
           ELSE
              PERFORM PRINT-REPORT.

           PERFORM RANGE-LINES.
           MOVE "Y" TO WS-NET-INCOME-FLAG.
           MOVE SPACES TO DETAIL-LINE.
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.
           GO TO END-PROGRAM.


      ******************************************************************
       INITIALIZATION SECTION.
      ******************************************************************
           MOVE UNEXTR-QUEUE-POS TO Q-POS.
           MOVE UNEXTR-COPIES-POS TO C-POS.
           MOVE UNEXTR-PRU-POS   TO P-POS.
           MOVE SPACES TO Q-BUF C-BUF P-BUF.

       COPY "LIBGB/EOINIT.CPY".

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

      * SET ENDING REPORT DATE
           IF EXT-ROUTE-BUF = "00"
              MOVE MM TO DATE-MMDDYY-MM
              MOVE 33 TO DATE-MMDDYY-DD
              MOVE YY TO DATE-MMDDYY-YY
              PERFORM CONVERT-MMDDYY-TO-YYYYMMDD
              MOVE DATE-YYYYMMDD TO NDTE-DATE
              PERFORM NEW-DATE-CALCULATION
              MOVE NDTE-DATE TO REPORT-ENDING-DATE
      * SET PRIOR MONTH ENDING DATE:
              MOVE 33 TO NDTE-DD
              MOVE -1 TO NDTE-HOLD
              PERFORM INCREMENT-MONTHS
              MOVE NDTE-DATE TO PRIOR-ENDING-DATE.

           PERFORM OPEN-BR-FILE.
           PERFORM OPEN-GR1-FILE.
           PERFORM OPEN-SP1-FILE.

           MOVE MKTEMP-DEFAULT TO MKTEMP-BUF.
           PERFORM MKTEMP-CALL.
           MOVE MKTEMP-BUF TO WK-PATH.
           PERFORM OPEN-WK-FILE-OUTPUT.
           PERFORM CLOSE-WK-FILE.
           PERFORM OPEN-WK-FILE.

           MOVE EXT-FILPATH-BASE    TO EXTRACT-PATH-BASE.
           MOVE EXT-FILPATH-MACHINE TO EXTRACT-PATH-MACHINE.
           MOVE MMYY                TO EXTRACT-PATH-DATE.
           PERFORM OPEN-EXTRACT-FILE-OUTPUT.
           PERFORM CLOSE-EXTRACT-FILE.
           PERFORM OPEN-EXTRACT-FILE.
           MOVE 0 TO EXTRACT-KEY.

           MOVE EXT-CONAME-CONAME TO H-NAME.
           MOVE EXT-CONAME-SYSDATE TO H-DATE.
           MOVE EXT-CONAME-USER-ID TO H-USERID.
           MOVE MMYY TO H-MMYY.

           IF EXT-ROUTE-BUF = "00"
              MOVE "INSURANCE UNEARN EXTRACT IN PROGRESS...." TO WR-BUF
              MOVE MESS-LIST TO WR-LIST
              PERFORM CALL-WRFORM.

           PERFORM OPEN-PR-FILE.
           GO TO EXIT-INITIALIZE.

       END-INITIALIZE.
           MOVE "X" TO EXT-EOBUF-ERRCD.

       EXIT-INITIALIZE.
           EXIT.

      ******************************************************************
       EO-EXEC-AUDIT SECTION.
      ******************************************************************

      * GET REPORT ENDING DATE:
           MOVE EXT-EOBUF-DATE2 TO NDTE-DATE.
           MOVE NDTE-MM TO MM.
           MOVE 33 TO NDTE-DD.
           MOVE NDTE-YY TO YY.
           PERFORM NEW-DATE-CALCULATION.
           MOVE NDTE-DATE TO REPORT-ENDING-DATE.

      * GET PRIOR MONTH ENDING DATE:
           MOVE 33 TO NDTE-DD.
           MOVE -1 TO NDTE-HOLD.
           PERFORM INCREMENT-MONTHS.
           MOVE NDTE-DATE TO PRIOR-ENDING-DATE.

      *******************************************************
       ENTER-PARAMETERS SECTION.
      *******************************************************
       ENTER-PARAM.

           IF VDUSTATUS = "1U" GO TO PL-01.

      *----------------------------------------------------------------
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

      *----------------------------------------------------------------
       IC-01.
           MOVE "ENNN020IC1." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "16" GO TO ALL-IC.
           IF VDUSTATUS = "1U" GO TO BR-02.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO IC-01.
           MOVE VDUNUM0 TO BEG-IC.

      *----------------------------------------------------------------
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

      *----------------------------------------------------------------
       ALL-IC.
           MOVE "SNNN020IC1." TO VDU.
           MOVE 1 TO BEG-IC VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN020IC2." TO VDU.
           MOVE 99 TO END-IC VDUNUM0.
           PERFORM SCREENIO.

      *----------------------------------------------------------------
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
           IF BEG-CLASS NOT > END-CLASS GO TO ST-01.
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
       ST-01.
           MOVE "E  A020ST1." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "16" GO TO ALL-ST.
           IF VDUSTATUS = "1U" GO TO LC-02.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO ST-01.
           MOVE VDUBUF TO BEG-ST.

      *----------------------------------------------------------------
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

      *----------------------------------------------------------------
       ALL-ST.
           MOVE "S  A020ST1." TO VDU.
           MOVE "  " TO BEG-ST VDUBUF.
           PERFORM SCREENIO.
           MOVE "S  A020ST2." TO VDU.
           MOVE ALL "Z" TO END-ST.
           CALL "C$TOLOWER" USING END-ST, VALUE 2.
           MOVE END-ST TO VDUBUF.
           PERFORM SCREENIO.

      *----------------------------------------------------------------
       DT-01.
           IF EXT-ROUTE-BUF = "40" OR "50"
              GO TO ZBAL-01.

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

      *----------------------------------------------------------------
       ZBAL-01.
           MOVE "E  A010ZBAL." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.

           IF VDUSTATUS = "1U"
              IF EXT-ROUTE-BUF = "40" OR "50"
                 GO TO ST-02
              ELSE
                 GO TO DT-01.

           IF NOT (VDUSTATUS = "00" OR "1D") GO TO ZBAL-01.
           MOVE VDUBUF TO ZBAL-ONLY.
           IF ZBAL-ONLY = "Y" OR "N" GO TO PL-01.
           IF ZBAL-ONLY NOT = "Y" OR "N" GO TO ZBAL-01.

      *----------------------------------------------------------------
       PL-01.
           MOVE "E  A010PL." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO ZBAL-01.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO PL-01.
           MOVE VDUBUF TO PL.
           IF PL NOT = "I" AND NOT = "S" AND NOT = "O"
              GO TO PL-01.

       EXIT-PARM.
           EXIT.

      *****************************************************
       DISPLAY-PARAMETERS SECTION.
      *****************************************************
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
      
           MOVE "SNNN020IC1." TO VDU.
           MOVE BEG-IC TO VDUNUM0.
           PERFORM SCREENIO.
      
           MOVE "SNNN020IC2." TO VDU.
           MOVE END-IC TO VDUNUM0.
           PERFORM SCREENIO.
      
           MOVE "SNNN020LC1." TO VDU.
           MOVE BEG-CLASS TO VDUNUM0.
           PERFORM SCREENIO.
      
           MOVE "SNNN020LC2." TO VDU.
           MOVE END-CLASS TO VDUNUM0.
           PERFORM SCREENIO.
      
           MOVE "S  A020ST1." TO VDU.
           MOVE BEG-ST TO VDUBUF.
           PERFORM SCREENIO.
      
           MOVE "S  A020ST2." TO VDU.
           MOVE END-ST TO VDUBUF.
           PERFORM SCREENIO.
      
           MOVE "S  A010ZBAL." TO VDU.
           MOVE ZBAL-ONLY TO VDUBUF.
           PERFORM SCREENIO.
      
           MOVE "S  A010PL." TO VDU.
           MOVE PL TO VDUBUF.
           PERFORM SCREENIO.
      

      ************************************************
       CREATE-TOTAL-RECS SECTION.
      ************************************************

           PERFORM CHECK-UNEARNED.

           MOVE 0 TO IC.

       CREATE-TOTAL-RECS-NEXT.
           ADD 1 TO IC.
           IF LN-INSCOMP(IC) < BEG-IC OR LN-INSCOMP(IC) > END-IC
              GO TO CREATE-TOTAL-RECS-CHECK-END.

      * EVEN IF NO UNEARNED REMAINS;
      * TEST TO SEE IF INSURANCE IS STILL INEFFECT.
      * IF NOT INEFFECT, STILL REPORT IF THERE
      * WAS A BEGINNING UNEARNED:

           IF END-UNEARNED(IC) = 0
              MOVE IC TO ITRM-SUB
              MOVE REPORT-ENDING-DATE TO ITRM-TODAY
              PERFORM INS-TERM-CALCULATION
              IF ITRM-INEFFECT-FG = "Y"
                 MOVE "Y" TO INS-INEFFECT(IC)
              ELSE
                 IF BEG-UNEARNED(IC) = 0
                    IF NEW-PREMIUMS(IC) = 0
                       IF NEW-REFUNDS(IC) = 0
                          GO TO CREATE-TOTAL-RECS-CHECK-END.

           MOVE LN-OWNBR TO WK-BR.
           MOVE LN-CLASS TO WK-CLASS.

           MOVE "Y" TO WS-NET-INCOME-CALC.
           PERFORM CREATE-WK.
           MOVE "N" TO WS-NET-INCOME-CALC.

           MOVE "N" TO FIRST-TIME.

       CREATE-TOTAL-RECS-CHECK-END.
           IF IC < 6
              GO TO CREATE-TOTAL-RECS-NEXT.

       EXIT-CREATE-TOTAL-RECS.
           EXIT.

      *********************************************
       CREATE-WK SECTION.
      *********************************************
           MOVE "N" TO REC-EXISTS.
           MOVE WK-KEY TO SAVE-WK-KEY.

       CHECK-REC-EXISTS.
           PERFORM READ-WK-FILE.
           IF IO-GOOD
              GO TO UPDATE-WK-REC.

           IF REC-EXISTS = "Y"
              GO TO IO-ERROR.

           MOVE " " TO WK-REC.
           MOVE SAVE-WK-KEY TO WK-KEY.
           MOVE 0 TO TP.

      *-----------------------------------------------------------------
       NEXT-WK-CLEAR.
           ADD 1 TO TP.
           MOVE 0 TO WK-END(TP).

           IF TP < 6
              GO TO NEXT-WK-CLEAR.

           PERFORM WRITE-WK-FILE.

           IF IO-BAD
              GO TO IO-ERROR.

           MOVE "Y" TO REC-EXISTS.
           GO TO CHECK-REC-EXISTS.

      *-----------------------------------------------------------------
       UPDATE-WK-REC.

           IF END-UNEARNED(IC) NOT = 0
              OR INS-INEFFECT(IC) = "Y"
              ADD END-UNEARNED(IC) TO WK-END(IC).

      * IF THEY USE SEPARATE EARNINGS OPTIONS FOR LEVEL LIFE, WANT
      * TO IDENTIFY ON THE DETAIL LINE
           IF BR-REB-METHOD(7) NOT = 0 AND
               ( BR-REB-METHOD(7) NOT = BR-REB-METHOD(1) )
              IF LN-INSTYPES-LL
                 MOVE "L" TO WK-LL-OR-DL
              ELSE
                 IF LN-INSTYPES-DL
                    MOVE "D" TO WK-LL-OR-DL.

           PERFORM REWRITE-WK-FILE.

      *-----------------------------------------------------------------
       EXIT-CREATE-WK.

           IF WS-NET-INCOME-CALC = "Y"
              IF (IC = 1 AND LN-INSTYPES-LL)
                    COMPUTE WS-NET-INCOME(4) = WS-NET-INCOME(4)
                         + BEG-UNEARNED(IC)
                         + NEW-PREMIUMS(IC)
                         + NEW-REFUNDS(IC)
                         - END-UNEARNED(IC)
              ELSE
                    COMPUTE WS-NET-INCOME(IC) = WS-NET-INCOME(IC)
                         + BEG-UNEARNED(IC)
                         + NEW-PREMIUMS(IC)
                         + NEW-REFUNDS(IC)
                         - END-UNEARNED(IC).

      ******************************************************************
       PRINT-REPORT SECTION.
      ******************************************************************

           MOVE "Y" TO FIRST-TIME.
           MOVE 2 TO LV.
           PERFORM CLEAR-TOTALS.
           CLOSE WK-FILE.
           PERFORM OPEN-WK-FILE.

      *-----------------------------------------------------------------
       NEXT-WK-READ.
           PERFORM READ-WK-FILE-NEXT.

           IF IO-BAD
              GO TO PRINT-REPORT-EXIT.

      *    UNLOCK WK-FILE.

           IF FIRST-TIME = "Y"
              MOVE "N" TO FIRST-TIME
              MOVE WK-BR TO LAST-BR
              PERFORM BR-HEADER.

           IF WK-BR NOT = LAST-BR
              PERFORM BR-TOTALS
              PERFORM BR-HEADER.

           MOVE WK-CLASS TO D-CLASS.

           MOVE 0 TO TP.

      *-----------------------------------------------------------------
       NEXT-WK-ACCUM.
           ADD 1 TO TP.

           ADD WK-END(TP) TO
              TOT-END(1 WK-CLASS TP) TOT-END(1 101 TP).

           MOVE WK-END(TP) TO D-END(TP).
           IF WK-END(TP) NOT = 0
              PERFORM CREATE-EXTRACT-RECORD.

           IF TP < 6
              GO TO NEXT-WK-ACCUM.

           PERFORM WRITE-DETAIL-LINE.

           GO TO NEXT-WK-READ.

       PRINT-REPORT-EXIT.
           EXIT.


      ******************************************************************
       BR-TOTALS SECTION.
      ******************************************************************
           MOVE 1 TO LV.
           MOVE "BRANCH" TO TOTAL-TYPE.
           PERFORM TOTAL-PRINT.
           MOVE 2 TO LN-FEED.
           MOVE WK-BR TO LAST-BR.


      ******************************************************************
       TOTAL-PRINT SECTION.
      ******************************************************************
           MOVE "TOTAL" TO DETAIL-LINE
           MOVE ":" TO D-COLON.

           IF TOT-END (LV 101 1) = 0
              IF TOT-END (LV 101 2) = 0
                 IF TOT-END (LV 101 3) = 0
                    IF TOT-END (LV 101 4) = 0
                       IF TOT-END (LV 101 5) = 0
                          IF TOT-END (LV 101 6) = 0
                             MOVE SPACES TO DETAIL-LINE
                             GO TO TOTAL-PRINT-EXIT.

           MOVE 0 TO TP.

      *-----------------------------------------------------------------
       NEXT-TOT-PRINT.
           ADD 1 TO TP.
           MOVE TOT-END(LV 101 TP) TO D-END(TP).

           MOVE 0 TO TOT-END(LV 101 TP).

           IF TP < 6
              GO TO NEXT-TOT-PRINT.

            PERFORM WRITE-DETAIL-LINE.

           GO TO TOTAL-PRINT-END.

       TOTAL-PRINT-END.

           IF TOTAL-TYPE = "STATE "
              MOVE DASH-LINE TO DETAIL-LINE
              MOVE 1 TO LN-FEED
              PERFORM WRITE-DETAIL-LINE.


       TOTAL-PRINT-EXIT.
           MOVE 2 TO LN-FEED.

      ******************************************************************
       CLEAR-TOTALS SECTION.
      ******************************************************************
           MOVE 0 TO TP.

      *-----------------------------------------------------------------
       CLEAR-NEXT-TYPE.
           ADD 1 TO TP.
           MOVE 0 TO TOT-END(1 1 TP).

           IF TP < 6
              GO TO CLEAR-NEXT-TYPE.

           MOVE 1 TO LC.

      *-----------------------------------------------------------------
       CLEAR-NEXT-CLASS.
           ADD 1 TO LC.
           MOVE TOT-LC(1 1) TO TOT-LC(1 LC).

           IF LC < 101
              GO TO CLEAR-NEXT-CLASS.

           IF LV = 2
              MOVE TOT-LV(1) TO TOT-LV(2).


      ******************************************************************
       CHECK-UNEARNED SECTION.
      ******************************************************************
           MOVE LN-INSCODES  TO HOLD-LN-INSCODES.
           MOVE LN-CL-REBATE TO HOLD-LN-CL-REBATE.
           MOVE LN-AH-REBATE TO HOLD-LN-AH-REBATE.
           MOVE LN-PP-REBATE TO HOLD-LN-PP-REBATE.
           MOVE LN-O1-REBATE TO HOLD-LN-O1-REBATE.
           MOVE LN-O2-REBATE TO HOLD-LN-O2-REBATE.
           MOVE LN-O3-REBATE TO HOLD-LN-O3-REBATE.

      * THIS CLEAR IS NECESSARY FOR TEST IN COMPUTE-BEG-UNEARNED:
      
           MOVE " " TO HOLD-LP-ADDON.
           MOVE " " TO METHOD-4-FG
                       METHOD-5-FG.

           MOVE 0 TO XUB.

       CHECK-UNEARNED-NEXT.
           ADD 1 TO XUB.

      * CLEAR WORKERS:
           MOVE 0 TO BEG-UNEARNED(XUB) END-UNEARNED(XUB)
                     NEW-PREMIUMS(XUB) NEW-REFUNDS(XUB).
           MOVE " " TO CHK-UNEARNED(XUB) INS-INEFFECT(XUB).


      * SAVE INSURANCE TABLE FOR COMPUTE OF ENDING UNEARNED:
      
           MOVE LN-INSTBL(XUB) TO HOLD-LN-INSTBL(XUB).

      * FOR INSURANCE THAT HAS NOT BEEN REBATED,
      *     DIRECTLY COMPUTE BEG & END UNEARNED:
      
           IF LN-INSCOMP(XUB) = 0      OR
              LN-INSCOMP(XUB) < BEG-IC OR
              LN-INSCOMP(XUB) > END-IC OR
              LN-INSEFF-DATE(XUB) = 0       OR
              LN-INSOURS(XUB) NOT = "Y"
              MOVE "N" TO CHK-UNEARNED(XUB)
              GO TO CHECK-UNEARNED-SKIP.

      * LN-REBATE TABLE, O1 = (9), O2 = (10) AND O3 = (11)
           MOVE XUB TO IDY.
           IF XUB > 3
              ADD 5 XUB GIVING IDY.

           IF LN-REBATE(IDY) = " " AND LN-ADDON-FG = " "
              MOVE "N" TO CHK-UNEARNED(XUB)
              PERFORM COMPUTE-BEG-END-UNEARNED.

       CHECK-UNEARNED-SKIP.
           IF XUB < 6
              GO TO CHECK-UNEARNED-NEXT.

      * IF ALL CHK-UNEARNED FLAGS = 'N',
      *    THAN IT IS NOT NECESSARY TO SEARCH FOR ADDONS,
      *    ADJUSTMENTS, CANCELLATIONS OR REBATES:
      
           IF CHK-UNEARNED(1) = "N" AND
              CHK-UNEARNED(2) = "N" AND
              CHK-UNEARNED(3) = "N" AND
              CHK-UNEARNED(4) = "N" AND
              CHK-UNEARNED(5) = "N" AND
              CHK-UNEARNED(6) = "N"
              GO TO EXIT-CHECK-UNEARNED.

      * REBATE OR CANCELLATION HAS OCCURRED;
      * IF IT OCCURRED THIS MONTH, FIND BEGINNING INS
      *    UNEARNED (ENDING BALANCE IS 0):

           MOVE LP-PATH-OWNBR TO LP-BRNO
                                 QLP1-WBEG-BRNO
                                 QLP1-WEND-BRNO.
           MOVE LN-ACCTNO     TO LP-ACCTNO
                                 QLP1-WBEG-ACCTNO
                                 QLP1-WEND-ACCTNO.
           MOVE 99999         TO LP-SEQNO
                                 QLP1-WEND-SEQNO.
           MOVE ALL "0"       TO QLP1-WBEG-SEQNO.
           PERFORM START-LP1-FILE-NOT-GREATER.

      *-----------------------------------------------------------------
       CHECK-UNEARNED-TRANS.
           PERFORM READ-LP1-FILE-PREVIOUS.
           IF IO-BAD
              GO TO END-CHECK-UNEARNED.

           IF LP-BRNO NOT = LN-OWNBR OR LP-ACCTNO NOT = LN-ACCTNO
              GO TO END-CHECK-UNEARNED.

           IF LP-REV = "Y" OR LP-TRCD = "RV"
              GO TO CHECK-UNEARNED-TRANS.

           IF LP-SSNO = 999999999
              GO TO CHECK-UNEARNED-TRANS.

           MOVE LP-REFNO TO HOLD-LP-ADDON.

      * TEST FOR ACTIVITY THIS MONTH AND
      * SKIP TRANSACTIONS THAT DID NOT OCCUR THIS MONTH
      
           MOVE LP-TRDATE         TO NUM-DATE.
           MOVE PRIOR-ENDING-DATE TO SYS-DATE.
           PERFORM TIM365.
           IF ELAPSED-DAYS NOT < 0
              GO TO END-CHECK-UNEARNED.

      * RESTORE LN-INSPREM AND LN-INSCOMM TO END OF MONTH AMT
      * TO ALLOW FOR CORRECT REBATING
      
           IF LP-TRCD = "CC"
              MOVE 1 TO XUB
              PERFORM RESTORE-PREM-COMM
              GO TO CHECK-UNEARNED-TRANS
           ELSE
              IF LP-TRCD = "CA"
                 MOVE 2 TO XUB
                 PERFORM RESTORE-PREM-COMM
                 GO TO CHECK-UNEARNED-TRANS
              ELSE
                 IF LP-TRCD = "CP"
                    MOVE 3 TO XUB
                    PERFORM RESTORE-PREM-COMM
                    GO TO CHECK-UNEARNED-TRANS
                 ELSE
                    IF LP-TRCD = "C1"
                       MOVE 4 TO XUB
                       PERFORM RESTORE-PREM-COMM
                       GO TO CHECK-UNEARNED-TRANS
                    ELSE
                       IF LP-TRCD = "C2"
                          MOVE 5 TO XUB
                          PERFORM RESTORE-PREM-COMM
                          GO TO CHECK-UNEARNED-TRANS
                       ELSE
                          IF LP-TRCD = "C3"
                             MOVE 6 TO XUB
                             PERFORM RESTORE-PREM-COMM
                             GO TO CHECK-UNEARNED-TRANS.


           IF LP-TRCD = "RC"
              MOVE 1 TO XUB
              PERFORM RESTORE-REBATES
              GO TO CHECK-UNEARNED-TRANS
           ELSE
              IF LP-TRCD = "RA"
                 MOVE 2 TO XUB
                 PERFORM RESTORE-REBATES
                 GO TO CHECK-UNEARNED-TRANS
              ELSE
                 IF LP-TRCD = "RP"
                    MOVE 3 TO XUB
                    PERFORM RESTORE-REBATES
                    GO TO CHECK-UNEARNED-TRANS
                 ELSE
                    IF LP-TRCD = "R1"
                       MOVE 4 TO XUB
                       PERFORM RESTORE-REBATES
                       GO TO CHECK-UNEARNED-TRANS
                    ELSE
                       IF LP-TRCD = "R2"
                          MOVE 5 TO XUB
                          PERFORM RESTORE-REBATES
                          GO TO CHECK-UNEARNED-TRANS
                       ELSE
                          IF LP-TRCD = "R3"
                             MOVE 6 TO XUB
                             PERFORM RESTORE-REBATES
                             GO TO CHECK-UNEARNED-TRANS.

           IF LP-TRCD = "RT" OR "ET" OR "LP" OR "NP" OR "ST"
              IF HOLD-LP-ADDON-TYPE = "CL" OR "AH" OR "PP" OR "O1"
                                            OR "O2" OR "O3"
                 PERFORM RESTORE-PREM-COMM-ADDONS
                 GO TO CHECK-UNEARNED-TRANS.

           GO TO CHECK-UNEARNED-TRANS.

      *-----------------------------------------------------------------
       END-CHECK-UNEARNED.
           MOVE 0 TO XUB.

      *-----------------------------------------------------------------
       CHECK-UNEARNED-BEG-ONLY.
           ADD 1 TO XUB.

           IF CHK-UNEARNED(XUB) = " "
              PERFORM COMPUTE-BEG-END-UNEARNED.

           IF XUB < 6
              GO TO CHECK-UNEARNED-BEG-ONLY.

       EXIT-CHECK-UNEARNED.
           EXIT.

      ******************************************************************
       RESTORE-REBATES SECTION.
      ******************************************************************
           MOVE XUB TO IDY.
           IF XUB > 3
              ADD 5 XUB GIVING IDY.

           IF LN-INSPREM(XUB) = 0
              PERFORM GET-INS-COMMISSION
              IF WK-IC-COMMRATE >= 0
                 COMPUTE WK-COMMISSION ROUNDED =
                    LP-TRAMT * WK-IC-COMMRATE / 100
              ELSE
                 COMPUTE WK-COMMISSION = WK-IC-COMMRATE * -1
              END-IF
           ELSE
              COMPUTE WK-COMMISSION ROUNDED =
                    LP-TRAMT * LN-INSCOMM(XUB)
                               / LN-INSPREM(XUB).

           SUBTRACT WK-COMMISSION FROM NEW-REFUNDS(XUB).
           MOVE " " TO LN-REBATE(IDY).

      ******************************************************************
       RESTORE-PREM-COMM SECTION.
      ******************************************************************
           MOVE XUB TO IDY.
           IF XUB > 3
              ADD 5 XUB GIVING IDY.

           IF LN-INSPREM(XUB) = 0
              PERFORM GET-INS-COMMISSION
              IF WK-IC-COMMRATE >= 0
                 COMPUTE WK-COMMISSION ROUNDED =
                    LP-TRAMT * WK-IC-COMMRATE / 100
              ELSE
                 COMPUTE WK-COMMISSION = WK-IC-COMMRATE * -1
              END-IF
           ELSE
              COMPUTE WK-COMMISSION ROUNDED =
                    LP-TRAMT * LN-INSCOMM(XUB)
                               / (LN-INSPREM(XUB) + LN-INSCOMM(XUB)).

           SUBTRACT WK-COMMISSION FROM NEW-REFUNDS(XUB).
           MOVE " " TO LN-REBATE(IDY).
           ADD WK-COMMISSION TO LN-INSCOMM(XUB).
           ADD LP-TRAMT TO LN-INSPREM(XUB).

      ******************************************************************
       RESTORE-PREM-COMM-ADDONS SECTION.
      ******************************************************************
           MOVE LN-OWNBR    TO LTI-BRNO.
           MOVE LN-ACCTNO   TO LTI-ACCTNO.
           MOVE LP-LT-SEQNO TO LTI-SEQNO.
           PERFORM READ-LTI1-FILE.
           IF IO-BAD
              GO TO IO-ERROR.

           IF BR-AUTO-INS-EARN NOT = "Y"
              MOVE LTI-INSPREM TO LTI-INSCOMM.

           IF HOLD-LP-ADDON-TYPE = "CL"
              MOVE 1 TO XUB
           ELSE
              IF HOLD-LP-ADDON-TYPE = "AH"
                 MOVE 2 TO XUB
              ELSE
                 IF HOLD-LP-ADDON-TYPE = "PP"
                    MOVE 3 TO XUB
                 ELSE
                    IF HOLD-LP-ADDON-TYPE = "O1"
                       MOVE 4 TO XUB
                    ELSE
                       IF HOLD-LP-ADDON-TYPE = "O2"
                          MOVE 5 TO XUB
                       ELSE
                          IF HOLD-LP-ADDON-TYPE = "O3"
                             MOVE 6 TO XUB.

           MOVE XUB TO IDY.
           IF XUB > 3
              ADD 5 XUB GIVING IDY.

           IF HOLD-LP-ADDON-TEXT = "ADJ"
              SUBTRACT LTI-INSPREM FROM LN-INSPREM(XUB)
              SUBTRACT LTI-INSCOMM FROM LN-INSCOMM(XUB)
              ADD LTI-INSCOMM TO NEW-PREMIUMS(XUB)
              GO TO RESTORE-PREM-COMM-ADDONS-EXIT.

           IF HOLD-LP-ADDON-TEXT = "CNL"
              SUBTRACT LTI-INSPREM FROM LN-INSPREM(XUB)
              SUBTRACT LTI-INSCOMM FROM LN-INSCOMM(XUB)
              ADD  LTI-INSCOMM TO NEW-REFUNDS(XUB)
              MOVE " " TO LN-REBATE(IDY)
              GO TO RESTORE-PREM-COMM-ADDONS-EXIT.

           IF HOLD-LP-ADDON-TEXT = "ADD"
              ADD LTI-INSCOMM TO NEW-PREMIUMS(XUB)
              MOVE LN-OWNBR  TO LX-BRNO
              MOVE LN-ACCTNO TO LX-ACCTNO
              MOVE LP-SEQNO  TO LX-SEQNO
              PERFORM READ-LX1-FILE
              IF IO-GOOD AND (LXA-CODE = "A")
                 MOVE LXA-LN-INSCOMP TO LN-INSCOMP(XUB)
                 MOVE LXA-LN-INSOURS TO LN-INSOURS(XUB)
                 MOVE LXA-LN-INSEFF-DATE TO LN-INSEFF-DATE(XUB)
                 MOVE LXA-LN-INSEXP-DATE TO LN-INSEXP-DATE(XUB)
                 MOVE LXA-LN-INSPREM TO LN-INSPREM(XUB)
                 MOVE LXA-LN-INSCOVR TO LN-INSCOVR(XUB)
                 MOVE LXA-LN-INSCOMM TO LN-INSCOMM(XUB)
                 MOVE LXA-LN-INSCODES TO LN-INSCODES
                 IF XUB = 1
                    MOVE LXA-LN-INS-REBATE TO LN-CL-REBATE
                 ELSE
                 IF XUB = 2
                    MOVE LXA-LN-INS-REBATE TO LN-AH-REBATE
                 ELSE
                 IF XUB = 3
                    MOVE LXA-LN-INS-REBATE TO LN-PP-REBATE
                 ELSE
                 IF XUB = 4
                    MOVE LXA-LN-INS-REBATE TO LN-O1-REBATE
                 ELSE
                 IF XUB = 5
                    MOVE LXA-LN-INS-REBATE TO LN-O2-REBATE
                 ELSE
                 IF XUB = 6
                    MOVE LXA-LN-INS-REBATE TO LN-O3-REBATE.

       RESTORE-PREM-COMM-ADDONS-EXIT.
           EXIT.

      ******************************************************************
       GET-INS-COMMISSION SECTION.
      ******************************************************************
           MOVE IC-NAME TO SAVE-IC-NAME.

           IF BR-AUTO-INS-EARN NOT = "Y"
              MOVE 100.00 TO WK-IC-COMMRATE
              GO TO GET-INS-COMMISSION-EXIT.

           MOVE LN-INSCOMP(XUB) TO IC-CODE.
           PERFORM READ-IC1-FILE.
           IF IO-BAD
              MOVE 0 TO WK-IC-COMMRATE
              GO TO GET-INS-COMMISSION-EXIT.

           IF XUB = 1
              IF LN-INSURED = "J"
                 MOVE 2 TO COMM-SUB
              ELSE
                 MOVE 1 TO COMM-SUB.

           IF XUB = 1
              IF LN-INSTYPES-LL
                 ADD 2 TO COMM-SUB.

           IF XUB = 2
              MOVE 5 TO COMM-SUB
           ELSE
           IF XUB = 3
              IF LN-INSTYPES-PP2
                 MOVE 7 TO COMM-SUB
              ELSE
                 MOVE 6 TO COMM-SUB.

           IF XUB > 3
              ADD 4 XUB GIVING COMM-SUB.

           MOVE IC-COMMRATE(COMM-SUB) TO WK-IC-COMMRATE.

       GET-INS-COMMISSION-EXIT.
           MOVE SAVE-IC-NAME TO IC-NAME.

      ******************************************************************
       COMPUTE-BEG-UNEARNED SECTION.
      ******************************************************************
           IF REBATE-METHOD(XUB) = 4
              IF METHOD-4-FG = " "
                 PERFORM FIND-METHOD-4-DATES
                 MOVE "Y" TO METHOD-4-FG.

           IF REBATE-METHOD(XUB) = 5
              IF METHOD-5-FG = " "
                 PERFORM FIND-METHOD-5-DATES
                 MOVE "Y" TO METHOD-5-FG.

           MOVE XUB TO IDY.
           IF XUB > 3
              ADD 5 XUB GIVING IDY.

      * TEST FOR NEW PREMIUM AND NEW VOIDED PREMIUM:
      
           IF ( LN-CONVERCD = " "           ) AND
              ( LN-INSEFF-DATE(XUB) = LN-INTDATE )
              IF HOLD-LP-ADDON-TEXT NOT = "ADD"
                 MOVE LN-POFFDATE TO NUM-DATE
                 MOVE LN-ENTDATE TO SYS-DATE
      * TEST IF ACCOUNT PAIDOFF (NOT VOIDED) THE SAME MONTH AS BOOKED
      *   AND LOAN WAS BOOKED THIS ACCOUNTING PERIOD THEN ADD TO NEW PREMIUMS
                 IF (NUM-MO = S-MM AND NUM-YR = S-YY) AND
                    (LN-POCD-POFF) AND (NOT LN-POCD-TRANSFERRED)
                     MOVE PRIOR-ENDING-DATE TO NUM-DATE
                     MOVE LN-ENTDATE TO SYS-DATE
                     IF LN-ENTDATE > PRIOR-ENDING-DATE
                        ADD LN-INSCOMM(XUB) TO NEW-PREMIUMS(XUB)
                     END-IF
                 ELSE
      * TEST IF ACCOUNT WAS NOT PAIDOFF OR VOIDED THE SAME MONTH AS BOOKED
                 IF NOT (NUM-MO = S-MM AND NUM-YR = S-YY)
      * TEST FOR ACCOUNT VOIDED BUT NOT IN THE SAME MONTH AS BOOKED
      * THAN SUBTRACT FROM NEW PREMIUMS
                   IF LN-POCD-VOID OR LN-POCD-UNWIND
                      SUBTRACT LN-INSCOMM(XUB) FROM NEW-PREMIUMS(XUB)
                   ELSE
      * TEST IF ACCOUNT WAS NOT PAIDOFF OR VOIDED THE SAME MONTH
      * AS BOOKED AND LOAN WAS BOOKED THIS ACCOUNTING PERIOD
      * THEN ADD TO NEW PREMIUMS
                      IF LN-ENTDATE > PRIOR-ENDING-DATE
                         ADD LN-INSCOMM(XUB) TO NEW-PREMIUMS(XUB).

      * TEST FOR REBATE DATE PRIOR TO ENTRY-DATE:

           IF REBATE-METHOD(XUB) = 4
              MOVE METHOD-4-BEG-PAYDATE TO REB-PAYDATE
           ELSE
              IF REBATE-METHOD(XUB) = 5
                 MOVE METHOD-5-BEG-PAYMNTD TO PAYMNTD
              ELSE
                 MOVE PRIOR-ENDING-DATE TO REB-PAYDATE.

           IF LN-CONVERCD = " "
              MOVE PRIOR-ENDING-DATE TO NUM-DATE
              MOVE LN-ENTDATE        TO SYS-DATE
              PERFORM TIM365
           ELSE
              MOVE -1 TO ELAPSED-DAYS.

           IF ELAPSED-DAYS NOT > 0
              IF LN-INSPREM(XUB) NOT = 0
                 IF LN-REBATE(IDY) = " "
                    IF (REBATE-METHOD(XUB) = 4 OR 5)
                                   AND (LN-DATE-PAID-LAST = 0)
                       ADD LN-INSCOMM(XUB) TO BEG-UNEARNED(XUB)
                    ELSE
                       PERFORM UNEARNED-VIA-REBATE
                       COMPUTE BEG-UNEARNED(XUB) ROUNDED =
                          BEG-UNEARNED(XUB)
                            + REB-REBATE * LN-INSCOMM(XUB)
                                / LN-INSPREM(XUB).

      ******************************************************************
       COMPUTE-BEG-END-UNEARNED SECTION.
      ******************************************************************
           PERFORM COMPUTE-BEG-UNEARNED.

           IF REBATE-METHOD(XUB) = 4
              MOVE METHOD-4-END-PAYDATE TO REB-PAYDATE
           ELSE
           IF REBATE-METHOD(XUB) = 5
              MOVE METHOD-5-END-PAYMNTD TO PAYMNTD
           ELSE
              MOVE REPORT-ENDING-DATE TO REB-PAYDATE.

           MOVE XUB TO IDY.
           IF XUB > 3
              ADD 5 XUB GIVING IDY.

           MOVE HOLD-LN-INSCODES TO LN-INSCODES.

           IF XUB = 1
              MOVE HOLD-LN-CL-REBATE TO LN-CL-REBATE
           ELSE
           IF XUB = 2
              MOVE HOLD-LN-AH-REBATE TO LN-AH-REBATE
           ELSE
              IF XUB = 3
                 MOVE HOLD-LN-PP-REBATE TO LN-PP-REBATE
              ELSE
              IF XUB = 4
                 MOVE HOLD-LN-O1-REBATE TO LN-O1-REBATE
              ELSE
              IF XUB = 5
                 MOVE HOLD-LN-O2-REBATE TO LN-O2-REBATE
              ELSE
              IF XUB = 6
                 MOVE HOLD-LN-O3-REBATE TO LN-O3-REBATE.

           MOVE HOLD-LN-INSTBL(XUB) TO LN-INSTBL(XUB).

           IF NOT LN-POCD-VOID
            IF NOT LN-POCD-UNWIND
              IF LN-INSPREM(XUB) NOT = 0
                 IF LN-REBATE(IDY) = " "
                    IF (REBATE-METHOD(XUB) = 4 OR 5) AND
                       (LN-DATE-PAID-LAST = 0)
                       ADD LN-INSCOMM(XUB) TO END-UNEARNED(XUB)
                    ELSE
                       PERFORM UNEARNED-VIA-REBATE
                       COMPUTE END-UNEARNED(XUB) ROUNDED =
                         END-UNEARNED(XUB) + REB-REBATE *
                            LN-INSCOMM(XUB) / LN-INSPREM(XUB).

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
      
           MOVE HEAD2 TO PR-REC.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-PR-REC.
      
           MOVE HEAD3 TO PR-REC.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-PR-REC.
      
           IF ( WS-NET-INCOME-FLAG = "Y" )
              GO TO HEAD-NET-INCOME.

           MOVE HEAD4 TO PR-REC.
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-PR-REC.
           MOVE HEAD5 TO PR-REC
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-PR-REC.
      
           MOVE DASH-LINE TO PR-REC.
           PERFORM WRITE-PR-REC.

       END-HEAD-PAGE.
           MOVE 9 TO LN-COUNT.
           MOVE 2 TO LN-FEED.
           GO TO EXIT-HEAD-PAGE.

       HEAD-NET-INCOME.
           MOVE 6 TO LN-COUNT.
           MOVE 2 TO LN-FEED.

       EXIT-HEAD-PAGE.
           EXIT.

      *******************************************************
       RANGE-LINES SECTION.
      *******************************************************
           MOVE 3 TO LN-FEED.
           MOVE "# OF EXTRACT RECS:" TO RANGE-LINE.
           MOVE EXTRACT-KEY TO RANGE-SEL
           PERFORM WRITE-DETAIL-LINE.
           MOVE 2 TO LN-FEED.
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
      
           MOVE "ZERO BALANCE ONLY ?" TO RANGE-LINE.
           MOVE ZBAL-ONLY TO RANGE-SEL.
           PERFORM WRITE-DETAIL-LINE.
      
           MOVE "INCLUDE P&L ACCOUNTS ?" TO RANGE-LINE.
           MOVE PL TO RANGE-SEL.
           PERFORM WRITE-DETAIL-LINE.
      

      ******************************************************************
       UNEARNED-VIA-REBATE SECTION.
      ******************************************************************
           MOVE 0 TO REB-REBATE.

           IF LN-INSURED = "J"
              MOVE 2 TO REB-SUB
           ELSE
              MOVE 1 TO REB-SUB.

           IF LN-INSTYPES-LL
              ADD 2 TO REB-SUB.

           IF XUB = 2
              MOVE 5 TO REB-SUB
           ELSE
           IF XUB = 3
              MOVE 6 TO REB-SUB
           ELSE
           IF XUB = 4
              MOVE 12 TO REB-SUB
           ELSE
           IF XUB = 5
              MOVE 13 TO REB-SUB
           ELSE
           IF XUB = 6
              MOVE 14 TO REB-SUB.

           IF ACCRUAL-MONTHS(XUB) = 0
              MOVE XUB TO ITRM-SUB
              MOVE 0 TO ITRM-TODAY
              PERFORM INS-TERM-CALCULATION
              MOVE ITRM-INSTERM TO REB-ORGTERM
           ELSE
              MOVE ACCRUAL-MONTHS(XUB) TO REB-ORGTERM.

           MOVE LN-INSPREM(XUB) TO REB-TOTCHG.

      * 5 = RULE OF 78THS PRINCIPLE REDUCTION:
      
           IF REBATE-METHOD(XUB) = 5
              PERFORM METHOD-5-CALCULATION
              GO TO END-UNEARNED-VIA-REBATE.

      * 7 = ACTUARIAL 52:
      
           IF REBATE-METHOD(XUB) = 7
              PERFORM METHOD-7-CALCULATION
              GO TO END-UNEARNED-VIA-REBATE.

           IF LN-ADDON-FG = "Y"
              PERFORM SET-ADDON-INS.

      * 1 OR 4 OR 6 = RULE OF 78THS FIXED TO THE FOLLOWING FORMULATION:
      
           IF REBATE-METHOD(XUB) = 1 OR 4 OR 6
              MOVE 1 TO SP-RBFRMLA(REB-SUB)
              MOVE "B" TO SP-RBDTE(REB-SUB 1)
                          SP-RBDTE(REB-SUB 2)
              MOVE 360 TO SP-RBYRTYPE(REB-SUB 1)
                          SP-RBYRTYPE(REB-SUB 2)
              MOVE 0 TO SP-RBMINRET(REB-SUB)
              MOVE 0 TO SP-RBNOMON(REB-SUB)
              MOVE 0 TO SP-RBDEDCHG(REB-SUB)
              MOVE 0 TO SP-RBRECAST(REB-SUB)
              MOVE 0 TO SP-RBEARLY(REB-SUB)
              MOVE 0 TO SP-RBDEFER(REB-SUB)
              MOVE 100 TO SP-RBPCT(REB-SUB)
              MOVE 0 TO SP-RBREDUC(REB-SUB)
              MOVE 0 TO SP-RBMIN(REB-SUB)
              MOVE 0 TO SP-RBSPOPT1(REB-SUB)
              MOVE 0 TO SP-RBSPOPT2(REB-SUB)
              MOVE 0 TO SP-RBMANAGER(REB-SUB)
              MOVE REBDAYS-MONTH(XUB) TO SP-RBDAYS(REB-SUB 1)
                                            SP-RBDAYS(REB-SUB 2).


      * 2 = PRO RATA FIXED TO THE FOLLOWING FORMULATION:
      
           IF REBATE-METHOD(XUB) = 2
              MOVE 5 TO SP-RBFRMLA(REB-SUB)
              MOVE "B" TO SP-RBDTE(REB-SUB 1)
                          SP-RBDTE(REB-SUB 2)
              MOVE 360 TO SP-RBYRTYPE(REB-SUB 1)
                          SP-RBYRTYPE(REB-SUB 2)
              MOVE 0 TO SP-RBMINRET(REB-SUB)
              MOVE 0 TO SP-RBNOMON(REB-SUB)
              MOVE 0 TO SP-RBDEDCHG(REB-SUB)
              MOVE 0 TO SP-RBRECAST(REB-SUB)
              MOVE 0 TO SP-RBEARLY(REB-SUB)
              MOVE 0 TO SP-RBDEFER(REB-SUB)
              MOVE 100 TO SP-RBPCT(REB-SUB)
              MOVE 0 TO SP-RBREDUC(REB-SUB)
              MOVE 0 TO SP-RBMIN(REB-SUB)
              MOVE 0 TO SP-RBSPOPT1(REB-SUB)
              MOVE 0 TO SP-RBSPOPT2(REB-SUB)
              MOVE 0 TO SP-RBMANAGER(REB-SUB)
              MOVE REBDAYS-MONTH(XUB) TO SP-RBDAYS(REB-SUB 1)
                                            SP-RBDAYS(REB-SUB 2).

      * 3 = REFUND STANDARD METHOD:
      * USE STATE PARAMETERS FOR DETAIL TO REBATE.
      

      * SECURITY FINANCE, EARN 25% UPFRONT:
           IF REBATE-METHOD(XUB) = 9
              COMPUTE REB-TOTCHG ROUNDED = REB-TOTCHG * .75.

           IF REBATE-METHOD(XUB) = 3
              MOVE XUB TO REB-SUB2 ADDONSPR-SUB
              PERFORM FIND-ADDON-SPR
              PERFORM REBATE
              PERFORM FIND-ADDON-SPR-RESTORE
              IF BR-FORCE-VALID-CITY-COUNTY = "Y"
                 SUBTRACT INS-TAX-RB FROM REB-REBATE
              END-IF
           ELSE
              PERFORM REBATE
              IF BR-FORCE-VALID-CITY-COUNTY = "Y"
                 SUBTRACT INS-TAX-RB FROM REB-REBATE.

      * 6 = 78THS / PRORATA AVERAGE METHOD:
      
           IF REBATE-METHOD(XUB) = 6
              COMPUTE REB-REBATE ROUNDED =
                 REB-REBATE / 2 * (1 + (REB-ORGTERM + 1)
                                          / (REB-REMTERM + 1)
                                  ).

       END-UNEARNED-VIA-REBATE.
           EXIT.

      ******************************************************************
      *    METHOD 5 CALCULATION
      *    RE: PRINCIPLE REDUCTION
      ******************************************************************
       METHOD-5-CALCULATION SECTION.

      * DETERMINE NO. OF WHOLE AND FRACTIONAL PAYMENTS:
           MOVE LN-TOTPAYMNTD TO SAVE-LN-TOTPAYMNTD.
           MOVE PAYMNTD TO LN-TOTPAYMNTD.
           MOVE LN-ORIG-1STPYDATE TO PDTH-DATE-WORK.
           PERFORM PAID-THRU-CALCULATION.
           MOVE SAVE-LN-TOTPAYMNTD TO LN-TOTPAYMNTD.

           COMPUTE UNRP-WORKER =
              (REB-ORGTERM * (REB-ORGTERM + 1)) / 2.

           IF UNRP-WORKER NOT = 0
              COMPUTE UNRP-WORKER ROUNDED =
                  REB-TOTCHG  / UNRP-WORKER.

      * SETUP SUM OF WHOLE REMAINING PAYMENTS:
           COMPUTE UNRP-SUMW =
              ((REB-ORGTERM - PDTH-PAYS) *
                    (REB-ORGTERM - PDTH-PAYS + 1)) / 2.

      * SETUP SUM OF WHOLE REMAINING PAYMENTS PLUS 1:
           COMPUTE UNRP-SUMW1 =
              ((REB-ORGTERM - PDTH-PAYS - 1) *
                    (REB-ORGTERM - PDTH-PAYS)) / 2.

      * COMPUTE UNEARNED THRU THIS PAYMENT:
           COMPUTE REB-REBATE ROUNDED = UNRP-WORKER *
              (UNRP-SUMW + PDTH-REMAIN * (UNRP-SUMW1 - UNRP-SUMW)).

       METHOD-5-CALCULATION-EXIT.
           EXIT.

      ****************************************
      *    METHOD 7 CALCULATION
      *    RE: ACTUARIAL ACCRUAL
      ****************************************
       METHOD-7-CALCULATION SECTION.
           MOVE REB-PAYDATE TO REGZ-DATE.
           MOVE LN-LOANDATE TO REGZ-LOANDATE.
           MOVE LN-INTDATE TO REGZ-INTDATE.
           MOVE LN-ORIG-1STPYDATE TO REGZ-1STPYDATE.
           MOVE LN-ORGTERM TO REGZ-ORGTERM.
           MOVE LN-1STPYAMT TO REGZ-1STPYAMT.
           MOVE LN-REGPYAMT TO REGZ-REGPYAMT.
           MOVE LN-LASTPYAMT TO REGZ-LASTPYAMT.
           MOVE LN-FINCHG TO REGZ-FINCHG.
           MOVE LN-APRATE TO REGZ-APRATE.
           MOVE "A" TO REGZ-DISFRMLA.
           MOVE "N" TO REGZ-INCLUDE-DEF-FG.
           MOVE LN-TOTNODEF TO REGZ-TOTNODEF.

           PERFORM REGZ-UNEARNED-CALC.

           COMPUTE REB-REBATE ROUNDED =
               REGZ-UNEARNED * LN-INSPREM(XUB) / LN-FINCHG.

       METHOD-7-CALCULATION-EXIT.
           EXIT.

      ******************************************************************
       SET-ADDON-INS SECTION.
      ******************************************************************
           MOVE LN-INSEFF-DATE(XUB) TO REB-LN-LOANDATE
                                  REB-LN-PURDATE
                                  REB-LN-INTDATE
                                  REB-LN-INSEFF
                                  NDTE-DATE.
           MOVE LN-INSEXP-DATE(XUB) TO REB-LN-INSEXP.

           MOVE 1 TO NDTE-HOLD.
           MOVE LN-UNITPER-CD TO DATER-UNITPER-CD.
           MOVE LN-UNITPER-FREQ TO DATER-UNITPER-FREQ.
           PERFORM INCREMENT-UNITPER.
           MOVE NDTE-DATE TO REB-LN-ORIG-1STPYDATE.
           MOVE 0 TO REB-LN-TOTNODEF.
           MOVE LN-INSCOVR(XUB) TO REB-LN-INSCOVR.
           MOVE "N" TO REB-LN-SALEFIN.
           MOVE "Y" TO REB-ADDON-FG.

      ******************************************************************
      *
      *    FIND METHOD 4 (CASH METHOD RE: INTEREST) REBATE
      *
      ******************************************************************
       FIND-METHOD-4-DATES SECTION.
           MOVE LN-ENTDATE TO METHOD-4-BEG-PAYDATE
                              METHOD-4-END-PAYDATE.

           MOVE LP-PATH-OWNBR TO LP-BRNO
                                 QLP1-WBEG-BRNO
                                 QLP1-WEND-BRNO.
           MOVE LN-ACCTNO     TO LP-ACCTNO
                                 QLP1-WBEG-ACCTNO
                                 QLP1-WEND-ACCTNO.
           MOVE 99999         TO LP-SEQNO
                                 QLP1-WEND-SEQNO.
           MOVE ALL "0"       TO QLP1-WBEG-SEQNO.
           PERFORM START-LP1-FILE-NOT-GREATER.

      *-----------------------------------------------------------------
       FIND-METHOD-4-DATES-NEXT.
           PERFORM READ-LP1-FILE-PREVIOUS.
           IF IO-BAD
              GO TO FIND-METHOD-4-DATES-END.

           IF LP-BRNO NOT = LN-OWNBR OR LP-ACCTNO NOT = LN-ACCTNO
              GO TO FIND-METHOD-4-DATES-END.

           IF LP-SSNO = 999999999
              GO TO FIND-METHOD-4-DATES-NEXT.

           IF LP-TRCD = "RV"
              IF RV-CNT < 50
                 ADD 1 TO RV-CNT
                 MOVE LP-TRDATE TO NUM-DATE
                 MOVE NUM-MO TO RV-MO(RV-CNT)
                 MOVE NUM-YR TO RV-YR(RV-CNT)
                 GO TO FIND-METHOD-4-DATES-NEXT
              ELSE
                 MOVE LN-ACCTNO TO PROCESS-NUMBER
                 MOVE PROCESS-ERROR TO MESS
                 PERFORM SEND-MESS
                 GO TO FIND-METHOD-4-DATES-END.

           IF LP-REV NOT = "Y"
              IF RV-CNT = 0
                 GO TO FIND-METHOD-4-DATES-NO-RV.

           IF RV-CNT = 0
              MOVE LN-ACCTNO TO PROCESS-NUMBER
              MOVE PROCESS-ERROR TO MESS
              PERFORM SEND-MESS
              GO TO FIND-METHOD-4-DATES-END.

           IF NOT (LP-TRCD = "PY" OR "PR" OR "PC" OR "PP"
                                   OR "PA" OR "PE" OR "SS"
                                    OR "PO" OR "PB" OR "SC"
                                     OR "RN" OR "RB" OR "RO"
                                      OR "AH" OR "PZ" OR "PN"
                                       OR "IN")
              SUBTRACT 1 FROM RV-CNT
              GO TO FIND-METHOD-4-DATES-NEXT.

           MOVE LP-TRDATE TO NUM-DATE.

           IF NUM-MO = RV-MO(RV-CNT)
              IF NUM-YR = RV-YR(RV-CNT)
                 SUBTRACT 1 FROM RV-CNT
                 GO TO FIND-METHOD-4-DATES-NEXT.

           SUBTRACT 1 FROM RV-CNT.

           IF METHOD-4-BEG-PAYDATE NOT = LN-ENTDATE
              GO TO FIND-METHOD-4-DATES-NEXT.

           IF PRIOR-ENDING-DATE NOT < LP-PAYDATE
              MOVE LP-PAYDATE TO METHOD-4-BEG-PAYDATE.

           GO TO FIND-METHOD-4-DATES-NEXT.

      *-----------------------------------------------------------------
      * TEST FOR ACTIVITY THIS MONTH:
      *-----------------------------------------------------------------
       FIND-METHOD-4-DATES-NO-RV.
           IF NOT (LP-TRCD = "PY" OR "PR" OR "PC" OR "PP"
                                   OR "PA" OR "PE" OR "SS"
                                    OR "BF" OR "PO" OR "PB"
                                     OR "SC" OR "RN" OR "RB"
                                      OR "RO" OR "AH" OR "PZ"
                                       OR "PN" OR "IN")
              GO TO FIND-METHOD-4-DATES-NEXT.

           IF METHOD-4-END-PAYDATE = LN-ENTDATE
              MOVE LP-PAYDATE TO METHOD-4-END-PAYDATE.

           IF PRIOR-ENDING-DATE < LP-PAYDATE
              GO TO FIND-METHOD-4-DATES-NEXT.

           MOVE LP-PAYDATE TO METHOD-4-BEG-PAYDATE.

       FIND-METHOD-4-DATES-END.
           EXIT.

      ******************************************************************
      *
      *    FIND METHOD 5 (CASH METHOD RE: INTEREST) REBATE
      *
      ******************************************************************
       FIND-METHOD-5-DATES SECTION.
           MOVE LN-TOTPAYMNTD TO METHOD-5-BEG-PAYMNTD
                                 METHOD-5-END-PAYMNTD.
           IF SP-CONTRFRMLA = "A"
              SUBTRACT LN-TOTLCHG FROM METHOD-5-BEG-PAYMNTD
                                       METHOD-5-END-PAYMNTD.

           MOVE LP-PATH-OWNBR TO LP-BRNO
                                 QLP1-WBEG-BRNO
                                 QLP1-WEND-BRNO.
           MOVE LN-ACCTNO     TO LP-ACCTNO
                                 QLP1-WBEG-ACCTNO
                                 QLP1-WEND-ACCTNO.
           MOVE 99999         TO LP-SEQNO
                                 QLP1-WEND-SEQNO.
           MOVE ALL "0"       TO QLP1-WBEG-SEQNO.
           PERFORM START-LP1-FILE-NOT-GREATER.

      *-----------------------------------------------------------------
       FIND-METHOD-5-DATES-NEXT.
           PERFORM READ-LP1-FILE-PREVIOUS.
           IF IO-BAD
              GO TO FIND-METHOD-5-DATES-END.

           IF LP-BRNO NOT = LN-OWNBR OR LP-ACCTNO NOT = LN-ACCTNO
              GO TO FIND-METHOD-5-DATES-END.

           IF LP-SSNO = 999999999
              GO TO FIND-METHOD-5-DATES-NEXT.

           IF LP-TRCD = "RV"
              IF RV-CNT < 50
                 ADD 1 TO RV-CNT
                 MOVE LP-TRDATE TO RV-DATE(RV-CNT)
                 GO TO FIND-METHOD-5-DATES-NEXT
              ELSE
                 MOVE LN-ACCTNO TO PROCESS-NUMBER
                 MOVE PROCESS-ERROR TO MESS
                 PERFORM SEND-MESS
                 GO TO FIND-METHOD-5-DATES-END.

           IF LP-REV NOT = "Y"
              IF RV-CNT = 0
                 GO TO FIND-METHOD-5-DATES-NO-RV.

           IF RV-CNT = 0
              MOVE LN-ACCTNO TO PROCESS-NUMBER
              MOVE PROCESS-ERROR TO MESS
              PERFORM SEND-MESS
              GO TO FIND-METHOD-5-DATES-END.

           IF NOT (LP-TRCD = "PY" OR "PR" OR "PC" OR "PP"
                                   OR "PA" OR "PE" OR "SS"
                                    OR "PO" OR "PB" OR "SC"
                                     OR "RN" OR "RB" OR "RO"
                                      OR "AH" OR "PZ" OR "PN"
                                       OR "IN")
              SUBTRACT 1 FROM RV-CNT
              GO TO FIND-METHOD-5-DATES-NEXT.

           MOVE LP-TRDATE TO NUM-DATE.

           IF NUM-MO = RV-MO(RV-CNT)
              IF NUM-YR = RV-YR(RV-CNT)
                 SUBTRACT 1 FROM RV-CNT
                 GO TO FIND-METHOD-5-DATES-NEXT.

           IF RV-DATE(RV-CNT) > PRIOR-ENDING-DATE
              ADD LP-APCUR TO METHOD-5-BEG-PAYMNTD
              ADD LP-APINT TO METHOD-5-BEG-PAYMNTD.

           SUBTRACT 1 FROM RV-CNT.
           GO TO FIND-METHOD-5-DATES-NEXT.

      *-----------------------------------------------------------------
      * TEST FOR ACTIVITY THIS MONTH:
      *-----------------------------------------------------------------
       FIND-METHOD-5-DATES-NO-RV.
           IF NOT (LP-TRCD = "PY" OR "PR" OR "PC" OR "PP"
                                   OR "PA" OR "PE" OR "SS"
                                    OR "PO" OR "PB" OR "SC"
                                     OR "RN" OR "RB" OR "RO"
                                      OR "AH" OR "PN" OR "PZ"
                                       OR "IN")
              GO TO FIND-METHOD-5-DATES-NEXT.

           IF LP-TRDATE > PRIOR-ENDING-DATE
              SUBTRACT LP-APCUR FROM METHOD-5-BEG-PAYMNTD
              SUBTRACT LP-APINT FROM METHOD-5-BEG-PAYMNTD
              GO TO FIND-METHOD-5-DATES-NEXT.

       FIND-METHOD-5-DATES-END.
           EXIT.

      ******************************
       CREATE-EXTRACT-RECORD SECTION.
      ******************************

           MOVE SPACES TO EXTRACT-REC.

           MOVE LAST-BR    TO EXT-BRNO.
           MOVE WK-CLASS   TO EXT-CLASS.
           MOVE WK-END(TP) TO EXT-UNEARN-AMT.

           IF TP = 1
              MOVE "CL" TO EXT-INS-TYPE
           ELSE
           IF TP = 2
              MOVE "AH" TO EXT-INS-TYPE
           ELSE
           IF TP = 3
              MOVE "PP" TO EXT-INS-TYPE
           ELSE
           IF TP = 4
              MOVE "O1" TO EXT-INS-TYPE
           ELSE
           IF TP = 5
              MOVE "O2" TO EXT-INS-TYPE
           ELSE
           IF TP = 6
              MOVE "O3" TO EXT-INS-TYPE.

           MOVE X"0D" TO EXT-CR.
           MOVE X"0A" TO EXT-LF.
           ADD 1 TO EXTRACT-KEY.
           PERFORM WRITE-EXTRACT-FILE.
           IF IO-BAD
              GO TO IO-ERROR.

       EXIT-CREATE-EXTRACT-RECORD.
           EXIT.


      *************************************
       CLASS-SCAN SECTION.
      *************************************
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

       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBLP/LPUPWK.CPY".
       COPY "LIBLP/LPAMTS.CPY".
       COPY "LIBGB/REBATE.CPY".
       COPY "LIBLP/LPMDTE.CPY".
       COPY "LIBLP/LPREGZ.CPY".
       COPY "LIBLP/LPAPRZ.CPY".
       COPY "LIBLP/LPUPER.CPY".
       COPY "LIBLP/LPAPRS.CPY".
       COPY "LIBLP/SETCTBL.CPY".
       COPY "LIBGB/TIMEIO.CPY".
       COPY "LIBGB/GRSERCH.CPY".
       COPY "LIBGB/GRWORK.CPY".
       COPY "LIBGB/BRSECURE.CPY".
       COPY "LIBLP/LPITRM.CPY".
       COPY "LIBGB/SPPDTH.CPY".
       COPY "LIBGB/RBACT.CPY".
       COPY "LIBLP/ADDONSPR.CPY".
       COPY "LIBGB/GPENV.CPY".

      ************************************************
       FORM-RESET SECTION.
      ************************************************
           MOVE EXT-CONAME-CONAME TO UNEXTR-CONAME.
           MOVE EXT-CONAME-SYSDATE TO UNEXTR-SYSDATE.
           DISPLAY UNEXTR-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE UNEXTR-FORM-FIELDS TO WR-BUF.
           MOVE UNEXTR-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.
       FORM-RESET-EXIT.
           EXIT.


      ************************************************
       SETUP-LINK-INFO SECTION.
      ************************************************
           MOVE UNEXTR-HELP--FILE TO HELP-LINK-FILE.
           MOVE UNEXTR-HELP--DIR TO HELP-LINK-DIR.

      ************************************************
       VDU-CONVERT-FIELD SECTION.
      ************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBSP/UNEXTR_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      *****************************************
       MISC-PARA SECTION.
      *****************************************
       COPY "LIBGB/GETPR.CPY".
       COPY "LIBGB/MKTEMP.CPY".
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/SYSTEM.CPY".
       COPY "LIBGB/RMFILE.CPY".
       COPY "LIBGB/GETENV.CPY".
      *-----------------------------------------------------------------
       INVALID-RANGE.
           MOVE "INVALID RANGE ENTERED" TO MESS.
           PERFORM SEND-MESS.
      *-----------------------------------------------------------------
       BR-HEADER.
           MOVE LAST-BR TO BR-NO.
           PERFORM GET-BR.
           MOVE LAST-BR TO D-BRNO.
           MOVE BR-NAME TO D-BRNAME.
           MOVE ":" TO D-COLON.
      *-----------------------------------------------------------------
       GET-BR.
           PERFORM READ-BR-FILE.
           IF IO-BAD
              MOVE "* INVALID *" TO BR-NAME.

      **********************************************
       IO-ROUTINES SECTION.
      **********************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBLP/LPSPGS_SQL.CPY".
       COPY "LIBLP/LPLXGS_SQL.CPY".
       COPY "LIBLP/LPLTPGS_SQL.CPY".
       COPY "LIBLP/LPLTIGS_SQL.CPY".
       COPY "LIBLP/LPLPGS_SQL.CPY".
       COPY "LIBLP/LPLNGS_SQL.CPY".
       COPY "LIBLP/LPICGS_SQL.CPY".
       COPY "LIBGB/GBGRGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBPRINT.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBGB/GBGR1RN.CPY".
       COPY "LIBLP/LPLN3RN.CPY".
       COPY "LIBLP/LPIC1RN.CPY".
       COPY "LIBLP/LPLP1RN.CPY".
       COPY "LIBLP/LPLX1RN.CPY".
       COPY "LIBLP/LPLTI1RN.CPY".
       COPY "LIBLP/LPLTP1RN.CPY".
       COPY "LIBLP/LPSP1RN.CPY".
       COPY "LIBGB/GBWKIN.CPY".

       OPEN-EXTRACT-FILE-OUTPUT.
           PERFORM OPEN-IT.
           MOVE EXTRACT-IFN TO E-FILE.
           OPEN OUTPUT EXTRACT-FILE.
           IF IO-FG = 8
              GO TO OPEN-EXTRACT-FILE-OUTPUT.
           IF IO-FG = 7
              CLOSE EXTRACT-FILE
              GO TO OPEN-EXTRACT-FILE-OUTPUT.
      *    UNLOCK EXTRACT-FILE.
       OPEN-EXTRACT-FILE.
           PERFORM OPEN-IT.
           MOVE EXTRACT-IFN TO E-FILE.
           OPEN I-O EXTRACT-FILE.
           IF IO-FG = 8
              GO TO OPEN-EXTRACT-FILE.
           IF IO-FG = 7
              CLOSE EXTRACT-FILE
              GO TO OPEN-EXTRACT-FILE.
      *    UNLOCK EXTRACT-FILE.
       WRITE-EXTRACT-FILE.
           PERFORM WRITE-IT.
           MOVE EXTRACT-IFN TO E-FILE.
           MOVE EXTRACT-KEY TO E-KEYX.
           WRITE EXTRACT-REC.
           IF IO-FG = 8
              GO TO WRITE-EXTRACT-FILE.
      *    UNLOCK EXTRACT-FILE.
       CLOSE-EXTRACT-FILE.
           PERFORM CLOSE-IT.
           CLOSE EXTRACT-FILE.

       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO EXT-EOBUF-ERRCD.


      ********************************************
       END-PROGRAM SECTION.
      ********************************************
       END-RUN.
           PERFORM CLOSE-FILES.

           IF EXT-ROUTE-BUF = "00"
              PERFORM PRU-OUT.

           MOVE WK-PATH    TO ACCESS-BUF.
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
              MOVE "SP/EXTRMU" TO FORM-NAM
              MOVE FORM-PATH TO FORM-PATHNAME.
       STOP-RUN.
           IF EXT-ROUTE-BUF = "00" OR "40" OR "50"
              DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN
              DESTROY UNEXTR-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.
           EXIT PROGRAM.
