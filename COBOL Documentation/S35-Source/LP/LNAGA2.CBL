       IDENTIFICATION DIVISION.
       PROGRAM-ID.      LNAGA2.
       DATE-WRITTEN.  06/20/2022.
      ******************************************************************
      *    DIR :  LP
      *    DESC:  LOAN AGED TRIAL BALANCE IN BRANCH, DEALER ORDER.
      *           FOR A SPECIFIC ACTION CODE 2 (SECURITIZATION POOL)
      *
      **********   IF AN ACCOUNT IS BOTH A REPO AND A PL, WILL PRINT AS
      **********   P&L TOTALS, AND NOT AS REPO TOTALS
      *
      * REV:
      *  BAH 20220620 COPIED FROM LNAGAC AND CHANGED ACTION TO ACTION2
      *************************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "LIBGB/GBFSPR.CPY".
       COPY "LIBGB/GBFSWKI.CPY".
       I-O-CONTROL.
       DATA DIVISION.
       FILE SECTION.
       COPY "LIBGB/GBFDPR.CPY".
       FD  WK-FILE
           LABEL RECORDS ARE STANDARD.
       01  WK-REC.
           03  WK-KEY.
               05  WK-BRNO               PIC 9(4).
               05  WK-DEALER             PIC 9(4)V99.
               05  WK-AGE                PIC S9(5).
               05  WK-BAL                PIC 9(6)V99.
               05  WK-ACCTNO             PIC 9(8).

      ******************************************************************
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)LP/LNAGA2.CBL S35 10/31/22-10:57:10 >".

      ******************************************************************
      *     F I L E   S P E C S  *
      ******************************************************************
       COPY "LIBLP/LP01LN.CPY".
       COPY "LIBLP/LP01LN_SQL.CPY".
       COPY "LIBLP/LPWSLN.CPY".
       COPY "LIBLP/LP01BW.CPY".
       COPY "LIBLP/LP01BW_SQL.CPY".
       COPY "LIBLP/LPWSBW.CPY".
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBLP/LP01LC.CPY".
       COPY "LIBLP/LP01LC_SQL.CPY".
       COPY "LIBLP/LPWSLC.CPY".
       COPY "LIBLP/LP01SP.CPY".
       COPY "LIBLP/LP01SPA.CPY".
       COPY "LIBLP/LP01SPB.CPY".
       COPY "LIBLP/LP01SPC.CPY".
       COPY "LIBLP/LP01SP_SQL.CPY".
       COPY "LIBLP/LPWSSP.CPY".
       COPY "LIBLP/LP01DL.CPY".
       COPY "LIBLP/LP01DL_SQL.CPY".
       COPY "LIBLP/LPWSDL.CPY".
       COPY "LIBLP/LP01LTP.CPY".
       COPY "LIBLP/LP01LTP_SQL.CPY".
       COPY "LIBLP/LPWSLTP.CPY".
       COPY "LIBGB/GB01GR.CPY".
       COPY "LIBGB/GB01GR_SQL.CPY".
       COPY "LIBGB/GBWSGR.CPY".
      *----------------------------------------------------------------
       01  PR-IFN        PIC XX       VALUE "PR".
       01  WK-IFN        PIC XX       VALUE "WK".


      ******************************************************************
      *   LINE WORKERS     *
      ******************************************************************
       01  LN-WORKERS.
           03  LN-FEED          PIC 99       VALUE 0.
           03  LN-COUNT         PIC 999      VALUE 99.
       01  PAGE-NUMBER          PIC 9(4)     VALUE 0.

      ******************************************************************
      *   MISC WORKERS     *
      ******************************************************************
       01  FIRST-TIME           PIC X        VALUE "Y".
       01  NEW-BRANCH           PIC X        VALUE "Y".
       01  MORE-THAN-1-DEALER   PIC X        VALUE "N".

       01  RECORDS-FOUND        PIC X        VALUE " ".
           88  NO-RECORDS-FOUND              VALUE "N".

       01  ACTION-WK.
           03  ACTION-CHAR      PIC X        OCCURS 4.
       01  ACTION-SUB           PIC 9.
       01  ACTION-MATCH         PIC X.
       01  SUMMARY-FG           PIC X        VALUE "N".
       01  MESS-LIST            PIC X(5)     VALUE "MESS.".
       01  NAME-WORKER          PIC X(34).
       01  FIRST-NAME           PIC X(16).
       01  HOLD-BRNO            PIC 9(4)     VALUE 0.
       01  BAL-WK               PIC 9(7)     VALUE 0 COMP-3.
       01  WORK-DEALER          PIC 9(4).
       01  HOLD-DEALER          PIC 9999V99  VALUE 0.
       01  HOLD-DEALER-2        PIC 9999     VALUE 0.
       01  LAST-LC-CODE         PIC 9(3)     VALUE 999.
       01  TRANS-DATE           PIC 9(8)     COMP-3 VALUE 0.

       01  UNEARNED             PIC S9(7)V99 VALUE 0 COMP-3.
       01  EARNED               PIC S9(7)V99 VALUE 0 COMP-3.
       01  INTEREST             PIC S9(7)V99 VALUE 0 COMP-3.
       01  RS-DEALER            PIC 9(4)V99.
       01  RS-DL                REDEFINES RS-DEALER.
           03  FILLER           PIC 9(4).
           03  FILLER           PIC 99.
       01  HOLD-LPS-NUM.
           03  H-LPS-CLASS      PIC XXX.
           03  H-LPS-NUM        PIC X(13).

       01 HOLD-BALANCES.
          03  HOLD-CURBAL       PIC S9(9)V99.
          03  HOLD-OTHBAL       PIC S9(7)V99.
       01 CLASS-FG              PIC X(3)        VALUE "   ".
          88  CLASS-BEG                         VALUE "BEG".
          88  CLASS-END                         VALUE "END".
       01 DEALER-FG             PIC X(4)        VALUE "    ".
          88  DEALER-BEG                        VALUE "DBEG".
          88  DEALER-END                        VALUE "DEND".
      ******************************************************************
      *               T O T A L S
      ******************************************************************
      *
      *    TOTAL-TBL:
      *       LEV(1) =  BY LOAN DEALER
      *          (2) =  BY BRANCH
      *          (3) =  BY COMPANY
      *
      *-----------------------------------------------------------------
      *    T-AGETBL :
      *         TYP(1)       (2)             (3)         (4)        (5)
      *        CONTRACTUAL/RECENCY       PAID-AHEAD      REPO      P & L
      *    -------------------------------------------------------------
      * AGE(1) =   30 DAYS            |  PAID-AHEAD  |  ONHAND  |  PL1
      *    (2) =   60 DAYS            |  N/A         |  SOLD    |  PL2
      *    (3) =   90 DAYS            |  N/A         |  N/A     |  PL3
      *    (4) =  120 DAYS            |  N/A         |  N/A     |  PL4
      *    (5) =  150 DAYS            |  N/A         |  N/A     |  PL5
      *    (6) =  180 DAYS            |  N/A         |  N/A     |  N/A
      *    (7) =  210 + DAYS          |  N/A         |  N/A     |  N/A
      *    (8) =  <M> MISSPAYS        |  N/A         |  N/A     |  N/A
      *    (9) =  <P> POTENTIAL 30'S  |  N/A         |  N/A     |  N/A
      *    (10) =  CURRENT ACCOUNTS   |  N/A         |  N/A     |  N/A
      *
      *-----------------------------------------------------------------
      *    T-TYPETBL:
      *       TYP(1) = CONTRACTUAL
      *          (2) = RECENCY
      *          (3) = PAID AHEAD
      *                CONTRACTUALLY CURRENT AND
      *                  DELINQ-RECENCY
      *          (4) = REPO
      *          (5) = P & L
      *
      ******************************************************************
       01  TOTAL-TABLE            VALUE ZEROS.
           03  TOTAL-TBL          OCCURS 3 TIMES.
               05  T-AGETBL       OCCURS 10 TIMES.
                   07  T-TYPETBL  OCCURS 5 TIMES.
                       09  T-NO   PIC S9(7).
                       09  T-BAL  PIC S9(9)V99.
                       09  T-INT  PIC S9(7)V99.
                       09  T-OTH  PIC S9(9)V99.
                       09  T-UNE  PIC S9(9)V99.
                       09  T-ERN  PIC S9(9)V99.
                       09  T-IER  PIC S9(7)V99.
       01  TT-TOTALS.
           03  TT-TYPETBL         OCCURS 5 TIMES.
               05  TT-NO          PIC S9(7)    COMP-3.
               05  TT-BAL         PIC S9(9)V99 COMP-3.
               05  TT-INT         PIC S9(7)V99 COMP-3.
               05  TT-OTH         PIC S9(9)V99 COMP-3.
               05  TT-UNE         PIC S9(9)V99 COMP-3.
               05  TT-ERN         PIC S9(9)V99 COMP-3.
               05  TT-IER         PIC S9(7)V99 COMP-3.

      * ----------------------------------------------------------------
      * SUB TOTALS    1 - DELINQUENCY
      *               2 - SLOW
      * ----------------------------------------------------------------
       01  SUB-TT-TOTALS.
           03  SUB-TT-TYPETBL     OCCURS 2 TIMES.
               05  SUB-TT-NO      PIC S9(7)    COMP-3.
               05  SUB-TT-BAL     PIC S9(9)V99 COMP-3.
               05  SUB-TT-INT     PIC S9(7)V99 COMP-3.
               05  SUB-TT-OTH     PIC S9(9)V99 COMP-3.
               05  SUB-TT-UNE     PIC S9(9)V99 COMP-3.
               05  SUB-TT-ERN     PIC S9(9)V99 COMP-3.
               05  SUB-TT-IER     PIC S9(7)V99 COMP-3.

       01  LEV                    PIC 9(6) COMP-3.
       01  AGE                    PIC 9(6) COMP-3.
           88  AGE-CONTRACTUAL             VALUE 1 THRU 10.
           88  AGE-RECENCY                 VALUE 1 THRU 10.
           88  AGE-PAID-AHEAD              VALUE 1.
           88  AGE-REPO                    VALUE 1 THRU 2.
           88  AGE-PL                      VALUE 1 THRU 5.

       01  TYP                    PIC 9(6) COMP-3.
           88  TYP-CONTRACTUAL             VALUE 1.
           88  TYP-RECENCY                 VALUE 2.
           88  TYP-PAID-AHEAD              VALUE 3.
           88  TYP-REPO                    VALUE 4.
           88  TYP-PL                      VALUE 5.

       01  SUB                    PIC 9(6) COMP-3.
       01  SUB1                   PIC 9(6) COMP-3.
       01  SUB2                   PIC 9(6) COMP-3.

       01  DD-TOTAL-TABLE.
           03  DD-TOT-TBL         OCCURS 32 TIMES.
               05  DD-NO          PIC S9(7)    COMP-3.
               05  DD-BAL         PIC S9(9)V99 COMP-3.
               05  DD-INT         PIC S9(7)V99 COMP-3.
               05  DD-OTH         PIC S9(9)V99 COMP-3.
               05  DD-UNE         PIC S9(9)V99 COMP-3.

       01  NM-TOTAL-TABLE.
           03  NM-TOT-TBL         OCCURS 27 TIMES.
               05  NM-NO          PIC S9(7)    COMP-3.
               05  NM-BAL         PIC S9(9)V99 COMP-3.
               05  NM-INT         PIC S9(7)V99 COMP-3.
               05  NM-OTH         PIC S9(9)V99 COMP-3.
               05  NM-UNE         PIC S9(9)V99 COMP-3.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    BINARY VALUE OF A LESS ONE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

       01  NM-A                   PIC 9(4) COMP VALUE 64.
       01  NM-B                   PIC 9(4) COMP VALUE 0.
       01  NM-BX REDEFINES NM-B.
           03  FILLER             PIC X.
           03  NM-TEST            PIC X.
       01  NM-SUB                 PIC 9(4) COMP.
       01  NM-PRINT               PIC X.

       01  RPSUB                  PIC 9(6) COMP.

       01  PLSUB                  PIC 9(6) COMP.

       01  AGE-DESC-TABLE.
           03  FILLER             PIC X(12) VALUE "    30 DAYS:".
           03  FILLER             PIC X(12) VALUE "    60 DAYS:".
           03  FILLER             PIC X(12) VALUE "    90 DAYS:".
           03  FILLER             PIC X(12) VALUE "   120 DAYS:".
           03  FILLER             PIC X(12) VALUE "   150 DAYS:".
           03  FILLER             PIC X(12) VALUE "   180 DAYS:".
           03  FILLER             PIC X(12) VALUE "   210+DAYS:".
           03  FILLER             PIC X(12) VALUE "   MIS-PAYS:".
           03  FILLER             PIC X(12) VALUE "   POT 30'S:".
           03  FILLER             PIC X(12) VALUE "   CURRENT :".
       01  AGE-DESC-TBL           REDEFINES AGE-DESC-TABLE.
           03  AGE-DESC           OCCURS 10 TIMES
                                  PIC X(12).
      *----------------------------------------------------------------
      *    REPO DESCRIPTION TABLE
      *----------------------------------------------------------------
       01  REPO-DESC-TABLE.
           03  FILLER             PIC X(12) VALUE "     ONHAND:".
           03  FILLER             PIC X(12) VALUE "       SOLD:".

       01  REPO-DESC-TBL          REDEFINES REPO-DESC-TABLE.
           03  REPO-DESC          OCCURS 2 TIMES
                                  PIC X(12).

      *----------------------------------------------------------------
      *    P & L DESCRIPTION TABLE
      *----------------------------------------------------------------
       01  PL-DESC-TABLE.
           03  FILLER             PIC X(12) VALUE "         P1:".
           03  FILLER             PIC X(12) VALUE "         P2:".
           03  FILLER             PIC X(12) VALUE "         P3:".
           03  FILLER             PIC X(12) VALUE "         P4:".
           03  FILLER             PIC X(12) VALUE "         P5:".

       01  PL-DESC-TBL           REDEFINES PL-DESC-TABLE.
           03  PL-DESC           OCCURS 5 TIMES
                                 PIC X(12).

      ******************************************************************
      *     S C R E E N   F I E L D S   *
      ******************************************************************
       COPY "LIBGB/EOBUF_EXT.CPY".
           03  AGE-DATE        PIC 9(8).
           03  BEG-BRANCH      PIC 9(4).
           03  END-BRANCH      PIC 9(4).
           03  BEG-CLASS       PIC 9(3).
           03  END-CLASS       PIC 9(3).
           03  BEG-DEALER      PIC 9(4)V99.
           03  END-DEALER      PIC 9(4)V99.
           03  ACTIONX.
               05  ACTION-CD   PIC X OCCURS 4.
           03  BEG-AGE         PIC 9(4).
           03  END-AGE         PIC 9(4).
           03  CPOT            PIC X.
           03  CMISS           PIC X.
           03  BEG-BAL         PIC 9(7).
           03  END-BAL         PIC 9(7).
           03  BEG-RAGE        PIC 9(4).
           03  END-RAGE        PIC 9(4).
           03  RPOT            PIC X.
           03  OWNED           PIC X.
           03  PL              PIC X.
           03  PLACTIVE        PIC X.
           03  RC              PIC X.
           03  JD              PIC X.
           03  REPO            PIC X.
           03  BANK            PIC X.
           03  LEGL            PIC X.
           03  DEBT            PIC X.
           03  ZBAL            PIC X.
           03  BRFLAG          PIC X.
           03  ALPHA-SUMMARY   PIC X.
           03  SUM-ONLY        PIC X.
           03  BEG-LNAMT       PIC 9(6).
           03  END-LNAMT       PIC 9(6).
           03  PLTYPE          PIC X.
           03  BRULE           PIC X.
           03  GROUP-FLAG      PIC X.
           03  ENT-GROUP       PIC X(4).
           03  LOCAL-PRINT-FG  PIC X.

       01  VERSION-CONTROL     PIC X VALUE "C".

       01  SV-BEG-BR           PIC 9(4) VALUE 9999.
       01  SV-END-BR           PIC 9(4) VALUE 9999.
       01  SV-GROUP-FLAG       PIC X     VALUE SPACES.
       01  SV-ENT-GROUP        PIC X(4)  VALUE SPACES.

      ******************************************************************
      *     P R I N T   B U F F E R S   *
      ******************************************************************
       01  HEAD1.
           03  H-DATE           PIC X(8)     VALUE " ".
           03  FILLER           PIC XX       VALUE " ".
           03  H-HR             PIC 99       VALUE 0.
           03  FILLER           PIC X        VALUE ":".
           03  H-MIN            PIC 99       VALUE 0.
           03  FILLER           PIC X(31)    VALUE " ".
           03  H-NAME           PIC X(40)    VALUE " ".
           03  FILLER           PIC X(37)    VALUE " ".
           03  FILLER           PIC X(5)     VALUE "PAGE ".
           03  H-PAGE-NO        PIC ZZZ9.

       01  HEAD2.
           03  FILLER           PIC X(10)    VALUE "USER ID :".
           03  H-USERID         PIC X(10).
           03  FILLER           PIC X(26)    VALUE " ".
           03  FILLER           PIC X(30)    VALUE
           "AGED TRIAL BALANCE     AS OF: ".
           03  H-AGEDATE        PIC X(10).
           03  FILLER           PIC X(3).

       01  HEAD3.
           03  FILLER      PIC X(9)     VALUE "BRANCH  :".
           03  FILLER      PIC XX       VALUE " ".
           03  H-BRNO      PIC 9(4).
           03  H-BRNOX     REDEFINES H-BRNO PIC X(4).
           03  FILLER      PIC X        VALUE " ".
           03  H-BRDESC    PIC X(20).
           03  FILLER      PIC X(21)    VALUE " ".
           03  FILLER      PIC X(15)    VALUE "ACTION CODE 2: ".
           03  H-ACTION    PIC X(4).
           03  FILLER      PIC X(35)    VALUE " ".
           03  FILLER      PIC X(20)    VALUE
           "99.99.99 - LOAN DATE".

       01  HEAD4.
           03  FILLER      PIC X(9)     VALUE "DEALER  :".
           03  FILLER      PIC XX       VALUE " ".
           03  H-DEALER      PIC 9999.99.
           03  H-DEALERX REDEFINES H-DEALER.
               05  H-DEALER-2 PIC 9999.
               05  FILLER     PIC XXX.
           03  FILLER      PIC X        VALUE " ".
           03  H-DEALERNM    PIC X(35).

       01  HEAD5.
           03  FILLER      PIC X(48)    VALUE " ".
           03  HEAD5XX     PIC X(46)    VALUE
               "LAST  REC-    RECENCY  UNEARNED  CONTRACTUAL".
           03  HEAD5X      REDEFINES HEAD5XX.
               05  FILLER  PIC X(23).
               05  HEAD5T  PIC X(8).
               05  FILLER  PIC X(15).
           03  FILLER      PIC X(38)    VALUE
               "    PAID   -------  BALANCES  -------".

       01  HEAD6.
           03  FILLER      PIC X(39)    VALUE
               "ACCOUNT NO.  DUE  BORROWER".
           03  HEAD6XX     PIC X(45)    VALUE
               "DEALER PAYMENT ENCY    PROFILE   CHARGES    P".
           03  FILLER      REDEFINES HEAD6XX.
               05  FILLER  PIC X(33).
               05  HEAD6T  PIC X(7).
               05  FILLER  PIC X(5).
           03  FILLER      PIC X(48)    VALUE
               "ROFILE   AGE  THRU  OTH-DUE   INT-DUE  PRINCIPAL".

      ******************************************************************
      *    D E T A I L    L I N E
      ******************************************************************
       01  DETAIL-LINE          PIC X(132) VALUE SPACES.

       01  D-LINE1 REDEFINES DETAIL-LINE.
           03  D-LOAN-NUM  PIC X(13).
           03  FILLER      PIC X.
           03  D-DUEDAY    PIC 99.
           03  FILLER      PIC XX.
           03  D-NAME      PIC X(19).
           03  FILLER      PIC X.
           03  D-DLNO      PIC ZZZ9.99.
           03  FILLER      PIC X.
           03  D-PAYDATE   PIC X(08).
           03  FILLER      REDEFINES D-PAYDATE.
               05  FILLER  PIC XX.
               05  D-DASH1 PIC X.
               05  FILLER  PIC XX.
               05  D-DASH2 PIC X.
               05  FILLER  PIC XX.
           03  D-RECENCYX.
               05  D-RECENCY   PIC ZZZZ.
           03  FILLER      PIC X.
           03  D-PYRECPRO  PIC 9(12).
           03  D-XPYRECPRO REDEFINES D-PYRECPRO PIC X(12).
           03  D-UNEARNED  PIC ZZZZ9.99-.
           03  D-PROFILE   PIC 9(12).
           03  D-XPROFILE  REDEFINES D-PROFILE PIC X(12).
           03  D-CONTUALX.
               05  D-CONTUAL   PIC ZZZZ.
           03  D-PDTHRU    PIC ZZ9999.
           03  D-OTHBAL    PIC ZZZZZ9.99-.
           03  D-INTBAL    PIC ZZZZZ9.99-.
           03  D-CURBAL    PIC ZZZZZZ9.99.

       01  SUMMARY-LINE1 REDEFINES DETAIL-LINE.
           03  S-SUMNAME        PIC X(20).
       01  SUMMARY-LINE2 REDEFINES DETAIL-LINE.
           03  FILLER           PIC X(46).
           03  S-AGETYPE        PIC X(80).
       01  SUMMARY-LINE3 REDEFINES DETAIL-LINE.
           03  S-NAME           PIC X(12).
           03  FILLER           REDEFINES S-NAME.
               05  FILLER       PIC X(9).
               05  S-LASTINITIAL.
                   07  S-NAMEDUE    PIC 99.
               05  S-NAMECOLON  PIC X.
           03  S-NO             PIC ZZZZZZZ9-.
           03  S-NO-P           PIC ZZ9.99-.
           03  S-OTH            PIC ZZZZZZZZ9.99-.
           03  S-OTH-P          PIC ZZ9.99-.
           03  S-INT            PIC ZZZZZZZZ9.99-.
           03  S-INT-P          PIC ZZ9.99-.
           03  S-BAL            PIC ZZZZZZZZZZ9.99-.
           03  S-BAL-P          PIC ZZ9.99-.
           03  S-UNE            PIC ZZZZZZZZZ9.99-.
           03  S-UNE-P          PIC ZZ9.99-.
           03  S-ERN            PIC ZZZZZZZZZ9.99-.
           03  S-ERN-P          PIC ZZ9.99-.
      *     03  S-IER            PIC ZZZZZZ9.99-.
      *     03  S-IER-P          PIC ZZ9.99-.

       01  RANGE-LN REDEFINES DETAIL-LINE.
           03  RANGE-LINE       PIC X(20).
           03  RANGE-SELECTION  PIC X(112).

       01  SUMMARY1.
           03  FILLER           PIC X(15)  VALUE SPACES.
           03  FILLER           PIC X(57)  VALUE
           "   NO----%        OTH DUE----%        INT DUE----%".
           03  FILLER           PIC X(20)  VALUE
           "   BAL DUE----%".
           03  SUMMARY1X        PIC X(37)  VALUE
           "   UNEARNED----%          EARNED----%".

      ******************************************************************
      *
      *     C O P Y    L I B R A R Y   W O R K E R S
      *
      ******************************************************************
       COPY "LIBWI/GRSCANW.CPY".
       COPY "LIBGB/GRWORKW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBWI/LCSCANW.CPY".
       COPY "LIBWI/CDSCANW.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBWI/DLSCANW.CPY".
       COPY "LIBGB/AGEINGW.CPY".
       COPY "LIBLP/LPPDUEW.CPY".
       COPY "LIBLP/LPUPWKW.CPY".
       COPY "LIBGB/SPPDTHW.CPY".
       COPY "LIBLP/LPEARNW.CPY".
       COPY "LIBLP/LPIBPCW.CPY".
       COPY "LIBLP/LPCEPPW.CPY".
       COPY "LIBLP/LPSTATW.CPY".
       COPY "LIBLP/LPMDTEW.CPY".
       COPY "LIBLP/LPXDTEW.CPY".
       COPY "LIBGB/EOSPOOLW.CPY".
       COPY "LIBGB/MKTEMPW_CMD.CPY".
       COPY "LIBGB/SYSTEMW.CPY".
       COPY "LIBGB/GPENVW.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/GETPRW.CPY".
       COPY "LIBGB/DACCESSW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBLP/LNAGA2_DEF.CPY".
       COPY "LIBLP/LNAGA2_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

      *****************************************************************
      *
      *     P R O G R A M   C O N T R O L
      *
      *****************************************************************
       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBLP/LNAGA2_SCN.CPY".

      *----------------------------------------------------------------
       PROCEDURE DIVISION
           USING FORM-PATHNAME EXIT-PATHNAME.
       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
               PR-FILE WK-FILE.
       COPY "LIBGB/DECLRP.CPY".
           PERFORM CLOSE-SP1-FILE.
           PERFORM CLOSE-LTP1-FILE.
           PERFORM CLOSE-LN3-FILE.
           PERFORM CLOSE-LN1-FILE.
           PERFORM CLOSE-LC1-FILE.
           PERFORM CLOSE-DL1-FILE.
           PERFORM CLOSE-BW1-FILE.
           PERFORM CLOSE-GR1-FILE.
           PERFORM CLOSE-BR-FILE.
           PERFORM CLOSE-PR-FILE.
           CLOSE WK-FILE.
       COPY "LIBGB/DECLRP2.CPY".

      ******************************************************************
      *
      *    M A I N    P R O G R A M
      *
      ******************************************************************
       MAIN-MODULE SECTION.

           PERFORM SQL-CONNECT.

           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "LNAGA2" TO FORM-PROG VDU-FORM-NAME.

           PERFORM GET-GPENV.

           PERFORM INITIALIZATION.

           IF EXT-EOBUF-ERRCD NOT = " "
              GO TO END-PROGRAM.

           MOVE "N" TO RECORDS-FOUND.

           IF GROUP-FLAG = "N"
              MOVE BEG-BRANCH TO BR-NO
              PERFORM START-BR-FILE
              GO TO NEXT-BRANCH-NO.

           MOVE ENT-GROUP TO GR-ID.
           PERFORM SEARCH-GR-INIT.

      *----------------------------------------------------------------
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

      *----------------------------------------------------------------
       NEXT-BRANCH-NO.
           PERFORM READ-BR-FILE-NEXT.

           IF IO-FG = 9 OR BR-NO > END-BRANCH
              GO TO END-ROUTINE.

      *----------------------------------------------------------------
       MOVE-BRANCH-PATH-INFO.

           IF LOCAL-PRINT-FG = "Y"
              PERFORM CLOSE-PR-FILE
              PERFORM CREATE-SPOOL-DIR
              IF STAT NOT = "00"
                 GO TO NEXT-GROUP-LOC.

           MOVE "Y" TO NEW-BRANCH.

           MOVE EXT-FILPATH-BASE           TO FILPATH-BASE.
           MOVE BR-MACHINE                 TO FILPATH-MACHINE.
           MOVE BR-DATA-PATH               TO FILPATH-DATA.

           MOVE "B"                        TO BW-PATH-OVERRIDE
                                              DL-PATH-OVERRIDE
                                              LN-PATH-OVERRIDE
                                              LTP-PATH-OVERRIDE.

           PERFORM OPEN-BW1-FILE.
           PERFORM OPEN-LN3-FILE.
           PERFORM OPEN-WK-FILE.

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

      *----------------------------------------------------------------
       NEXT-LN.
           PERFORM READ-LN3-FILE-NEXT.
           IF IO-BAD OR (LN-OWNBR NOT = LN-PATH-OWNBR)
              GO TO PROCESS-WK-FILE.

           IF LN-OWNBR > END-BRANCH
              GO TO PROCESS-WK-FILE.

           IF ( LN-POCD-NONLOAN )
              GO TO NEXT-LN.

           IF LN-CLASS < BEG-CLASS OR LN-CLASS > END-CLASS
                 GO TO NEXT-LN.

           IF LN-DLNO < BEG-DEALER OR LN-DLNO > END-DEALER
              GO TO NEXT-LN.

      * NEED FOR LOANS THAT ARE 999999.??
           IF BEG-LNAMT = 0 AND END-LNAMT = 999999
              NEXT SENTENCE
           ELSE
              IF LN-SUM-LNAMT < BEG-LNAMT OR LN-SUM-LNAMT > END-LNAMT
                 GO TO NEXT-LN.

           MOVE LN-CLASS TO LC-CODE.
           IF LC-CODE NOT = LAST-LC-CODE
              MOVE LC-CODE TO LAST-LC-CODE
              PERFORM READ-LC1-FILE
              IF IO-FG NOT = 0
                 GO TO IO-ERROR.

           IF OWNED = "O"
              IF LC-SERVACCT = "Y"
                 GO TO NEXT-LN.

           IF OWNED = "S"
              IF LC-SERVACCT = "N"
                 GO TO NEXT-LN.

           IF PL = "O"
              IF LN-PLDATE = 0
                 GO TO NEXT-LN.

           IF PL = "S"
              IF LN-PLDATE NOT = 0
                 GO TO NEXT-LN.

           IF LN-PLDATE NOT = 0
              IF PLACTIVE = "A"
                 IF LN-PLCD NOT = "P"
                    GO TO NEXT-LN.

           IF LN-PLDATE NOT = 0
              IF PLACTIVE = "I"
                 IF LN-PLCD NOT = "I"
                    IF LN-PLCD NOT = "W"
                       GO TO NEXT-LN.

           IF LN-PLDATE NOT = 0
              IF PLTYPE NOT = "A"
                 IF LN-PLTYPE NOT = PLTYPE
                    GO TO NEXT-LN.

           IF LN-SALEFIN NOT = "Y"
              GO TO CONT-LN.

           IF RC = "F"
              IF LN-DLRECOURSE NOT = "F"
                 GO TO NEXT-LN.

           IF RC = "P"
              IF LN-DLRECOURSE NOT = "P"
                 GO TO NEXT-LN.

           IF RC = "N"
              IF LN-DLRECOURSE NOT = "N"
                 GO TO NEXT-LN.


      *----------------------------------------------------------------
       CONT-LN.
           IF JD = "O"
              IF LN-JDDATE = 0
                 GO TO NEXT-LN.

           IF JD = "S"
              IF LN-JDDATE NOT = 0
                 GO TO NEXT-LN.

           IF REPO = "O"
              IF NOT (LN-REPOCD = "X" OR "S")
                 GO TO NEXT-LN.

           IF REPO = "S"
              IF LN-REPOCD = "X" OR "S"
                 GO TO NEXT-LN.

           IF BANK = "O"
              IF LN-BNKRPTDATE = 0
                 GO TO NEXT-LN.

           IF BANK = "S"
              IF LN-BNKRPTDATE NOT = 0
                 GO TO NEXT-LN.

           IF LEGL = "O"
              IF LN-LEGALDATE = 0
                 GO TO NEXT-LN.

           IF LEGL = "S"
              IF LN-LEGALDATE NOT = 0
                 GO TO NEXT-LN.

           IF DEBT = "O"
              IF LN-DEBTAIDATE = 0
                 GO TO NEXT-LN.

           IF DEBT = "S"
              IF LN-DEBTAIDATE NOT = 0
                 GO TO NEXT-LN.

           IF ZBAL = "O"
              IF LN-CURBAL NOT = 0 OR
                  LN-INTBAL NOT = 0 OR
                   LN-LCBAL NOT = 0 OR
                    LN-OTHBAL NOT = 0
                 GO TO NEXT-LN.

           IF ZBAL = "S"
              IF LN-CURBAL = 0
                 IF LN-INTBAL = 0
                 IF LN-LCBAL = 0
                    IF LN-OTHBAL = 0
                       GO TO NEXT-LN.

           MOVE LN-ACTIONCD2 TO ACTION-WK.
           PERFORM ACTION-CD-CHECK.
           IF ACTION-MATCH NOT = "Y"
              GO TO NEXT-LN.

      * NEED FOR LOANS THAT ARE 999999.??
           IF BEG-BAL = 0 AND END-BAL = 9999999
              NEXT SENTENCE
           ELSE
              COMPUTE BAL-WK ROUNDED =
                      LN-CURBAL + LN-INTBAL + LN-LCBAL + LN-OTHBAL
              IF BAL-WK < BEG-BAL OR BAL-WK > END-BAL
                 GO TO NEXT-LN.

           PERFORM COMPUTE-AGEING-CAT.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      * TEST CONTRACTUAL:
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF CONTRACTUAL < BEG-AGE OR CONTRACTUAL > END-AGE
              GO TO NEXT-LN.


      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      * TEST CONTRACTUAL: POTENTIALS AND MISS PAYS.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

           IF CPOT = "S"
              IF CPOT30 = "Y"
                 GO TO NEXT-LN
              END-IF
           ELSE
              IF CPOT = "O"
                 IF CPOT30 NOT = "Y"
                    GO TO NEXT-LN.

           IF CMISS = "S"
              IF CPOT30 = "M"
                 GO TO NEXT-LN
              END-IF
           ELSE
              IF CMISS = "O"
                 IF CPOT30 NOT = "M"
                    GO TO NEXT-LN.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      * TEST RECENCY:
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF RECDEL < BEG-RAGE OR RECDEL > END-RAGE
              GO TO NEXT-LN.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      * TEST RECENCY: POTENTIALS
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF RPOT = "S"
              IF RDPOT30 NOT = " "
                  GO TO NEXT-LN
              END-IF
           ELSE
              IF RPOT = "O"
                 IF RDPOT30 = " "
                    GO TO NEXT-LN.

           MOVE LN-OWNBR  TO WK-BRNO.
           MOVE LN-DLNO   TO WK-DEALER.
           SUBTRACT CONTRACTUAL FROM 99999 GIVING WK-AGE.
           SUBTRACT LN-CURBAL FROM 999999.99 GIVING WK-BAL.
           MOVE LN-ACCTNO TO WK-ACCTNO.
           PERFORM WRITE-WK-FILE.
           IF IO-FG = 9
              GO TO IO-ERROR.

           GO TO NEXT-LN.


      *----------------------------------------------------------------
       PROCESS-WK-FILE.
           MOVE " " TO WK-KEY.
           MOVE 0 TO WK-BRNO WK-DEALER WK-AGE WK-BAL WK-ACCTNO.
           PERFORM START-WK-FILE.

      *----------------------------------------------------------------
       PROCESS-WK-FILE-PROCESS.
           PERFORM READ-WK-FILE-NEXT.

           IF IO-FG = 9
              GO TO END-OF-GROUP.

           MOVE WK-BRNO   TO LN-OWNBR.
           MOVE WK-ACCTNO TO LN-ACCTNO.
           PERFORM READ-LN1-FILE.
           IF IO-FG = 9
              GO TO PROCESS-WK-FILE-PROCESS.

           IF FIRST-TIME = "Y"
              MOVE LN-OWNBR TO HOLD-BRNO
              MOVE LN-DLNO TO HOLD-DEALER HOLD-DEALER-2
              PERFORM SET-DEALER-HEADINGS
              PERFORM SET-BRANCH-HEADINGS.

           IF LN-OWNBR NOT = HOLD-BRNO
              PERFORM DEALER-TOTALS
              PERFORM BRANCH-TOTALS
              MOVE LN-OWNBR TO HOLD-BRNO
              MOVE LN-DLNO TO HOLD-DEALER HOLD-DEALER-2
              PERFORM SET-DEALER-HEADINGS
              PERFORM SET-BRANCH-HEADINGS.

           MOVE LN-DLNO TO WORK-DEALER.

           IF (LN-DLNO NOT = HOLD-DEALER AND BRFLAG = "B") OR
              (WORK-DEALER NOT = HOLD-DEALER-2 AND BRFLAG = "D" )
              MOVE "Y"     TO MORE-THAN-1-DEALER
              PERFORM DEALER-TOTALS
              MOVE LN-DLNO TO HOLD-DEALER HOLD-DEALER-2
              PERFORM SET-DEALER-HEADINGS.

           PERFORM COMPUTE-AGEING-CAT.
           PERFORM DETAIL-LINE-PROCESS.
           GO TO PROCESS-WK-FILE-PROCESS.

      *----------------------------------------------------------------
       END-OF-GROUP.
           PERFORM CLOSE-LN3-FILE.
           PERFORM CLOSE-BW1-FILE.
           PERFORM OPEN-WK-FILE-OUTPUT.
           PERFORM CLOSE-WK-FILE.

           IF LOCAL-PRINT-FG = "Y"
              PERFORM DEALER-TOTALS
              PERFORM BRANCH-TOTALS
              MOVE 0 TO PAGE-NUMBER
              MOVE "Y" TO FIRST-TIME
              MOVE "N" TO MORE-THAN-1-DEALER.

           GO TO NEXT-GROUP-LOC.
      *----------------------------------------------------------------
       END-ROUTINE.
           IF NO-RECORDS-FOUND
              GO TO END-PROGRAM.

           IF LOCAL-PRINT-FG = "N"
              PERFORM DEALER-TOTALS
              PERFORM BRANCH-TOTALS
              PERFORM COMPANY-TOTALS
              PERFORM PRINT-RANGE-LINES.

           GO TO END-PROGRAM.

      ******************************************************************
       ACTION-CD-CHECK SECTION.
      ******************************************************************
           MOVE "Y" TO ACTION-MATCH.
           MOVE 0 TO ACTION-SUB.

       ACTION-NEXT-CHAR.
           ADD 1 TO ACTION-SUB.

           IF ACTION-CHAR(ACTION-SUB) NOT = ACTION-CD(ACTION-SUB) AND
              ACTION-CD(ACTION-SUB) NOT = "?"
              MOVE "N" TO ACTION-MATCH
              GO TO ACTION-CD-CHECK-EXIT.

           IF ACTION-SUB < 4
              GO TO ACTION-NEXT-CHAR.

       ACTION-CD-CHECK-EXIT.
           EXIT.

      ******************************************************************
       COMPUTE-AGEING-CAT SECTION.
      ******************************************************************

           PERFORM COMPUTE-RECENCY.
           MOVE LN-ORGST    TO SP-ORGST.
           MOVE LN-SPRCLASS TO SP-SPRCLASS.
           MOVE LN-SUBCLASS TO SP-SUBCLASS.
           MOVE LN-LAWCODE  TO SP-LAWCODE.
           IF LPWSSP-SP1-KEY NOT = SP1-KEY
              MOVE SP1-KEY TO LPWSSP-SP1-KEY
              PERFORM READ-SP1-FILE
              IF IO-FG NOT = 0
                 GO TO IO-ERROR.

           IF BRULE = "T"
              MOVE SP-TRWBANKRULE TO SP-BANKRULE
              MOVE SP-TRWCONTPDTH TO SP-CONTPDTH
              MOVE SP-TRWBANKUNITPER-CD TO SP-BANKUNITPER-CD.

           PERFORM COMPUTE-CONTRACTUAL.
           PERFORM COMPUTE-PAID-AHEAD.
           PERFORM COMPUTE-REPO.
           PERFORM COMPUTE-PL.

      ******************************************************************
       INITIALIZATION SECTION.
      ******************************************************************
           MOVE LNAGA2-QUEUE-POS TO Q-POS.
           MOVE LNAGA2-COPIES-POS TO C-POS.
           MOVE LNAGA2-PRU-POS   TO P-POS.
           MOVE SPACES TO Q-BUF C-BUF P-BUF.

       COPY "LIBGB/EOINIT.CPY".

           MOVE SPACES TO RECORDS-FOUND.
           MOVE AGE-DATE TO AGEING-DATE TRANS-DATE DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-ALPDATE.
           MOVE ALP-DATE TO H-AGEDATE.

           PERFORM ZERO-TT-TOTALS.
           PERFORM ZERO-DD-TOTALS.
           PERFORM ZERO-NM-TOTALS.

       SET-HEAD.
           MOVE EXT-CONAME-CONAME TO H-NAME.
           MOVE EXT-CONAME-SYSDATE TO H-DATE.
           MOVE EXT-CONAME-USER-ID TO H-USERID.

           IF EXT-ROUTE-BUF = "00"
              MOVE "REPORT IN PROGRESS................." TO WR-BUF
              MOVE MESS-LIST TO WR-LIST
              PERFORM CALL-WRFORM.

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

           IF LOCAL-PRINT-FG NOT = "Y"
              PERFORM OPEN-PR-FILE.
           PERFORM OPEN-BR-FILE.
           PERFORM OPEN-GR1-FILE.
           PERFORM OPEN-SP1-FILE.
           PERFORM OPEN-LC1-FILE.

           MOVE MKTEMP-DEFAULT TO MKTEMP-BUF.
           PERFORM MKTEMP-CALL.
           PERFORM OPEN-WK-FILE-OUTPUT.
           PERFORM CLOSE-WK-FILE.
           PERFORM OPEN-WK-FILE.

           GO TO EXIT-INITIALIZE.

       ENDIT.
           MOVE "X" TO EXT-EOBUF-ERRCD.

       EXIT-INITIALIZE.
           EXIT.

      ******************************************************************
       EO-EXEC-AUDIT SECTION.
      ******************************************************************
           MOVE SPACES TO H-DEALERX.
           MOVE EXT-EOBUF-DATE2 TO AGE-DATE.

      ******************************************************************
       ENTER-PARAMETERS SECTION.
      ******************************************************************
       ENTER-PARM.

      * FOR RE-ENTRY ON F4 FROM ACCEPT.
           IF VDUSTATUS = "1U" GO TO SUMYN.

       AGEDT.

      * BATCH WILL USE DATE RAN

           IF EXT-ROUTE-BUF = "40" OR "50"
              GO TO BR-01.

           MOVE "ERNM060AGEDATE." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO AGEDT.

           MOVE VDU-DATE-YYYYMMDD TO AGE-DATE.

           IF AGE-DATE = ZERO
              MOVE EXT-CONAME-SYSDATE TO ALP-DATE
              PERFORM CONVERT-ALPDATE-TO-YYYYMMDD
              MOVE "S  8000AGEDATE." TO VDU
              MOVE DATE-YYYYMMDD TO AGE-DATE VDU-DATE-YYYYMMDD
              PERFORM SCREENIO.

      *----------------------------------------------------------------
       BR-01.
           MOVE "E  A000BR1." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "16" GO TO ALL-BR.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO AGEDT.

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
              GO TO CL-01.

      *----------------------------------------------------------------
       BRANCH-IS-NUMERIC.
           MOVE "N"           TO GROUP-FLAG.
           MOVE WGR-SRESULT   TO BEG-BRANCH.
           MOVE "SN N040BR1." TO VDU.
           MOVE BEG-BRANCH    TO VDUNUM0.
           PERFORM SCREENIO.

      *----------------------------------------------------------------
       BR-02.
           MOVE "ENNN040BR2." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO BR-01.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO BR-02.

           MOVE VDUNUM0 TO END-BRANCH.

           IF BEG-BRANCH > END-BRANCH
              PERFORM INVALID-RANGE
              GO TO BR-01.

           GO TO CL-01.

      *----------------------------------------------------------------
       ALL-BR.
           MOVE "SNNN040BR1." TO VDU.
           MOVE 0 TO BEG-BRANCH VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN040BR2." TO VDU.
           MOVE 9999 TO END-BRANCH VDUNUM0.
           PERFORM SCREENIO.
           MOVE "N"           TO GROUP-FLAG.

      *----------------------------------------------------------------
       CL-01.
           MOVE "ENNN020CL1." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "16" GO TO ALL-CL.
           IF VDUSTATUS = "1U" GO TO BR-02.
           IF VDUSTATUS = "1<"
              MOVE "BEG" TO CLASS-FG
              PERFORM CLASS-SCAN
              IF IO-FG NOT = 0
              GO TO CL-01.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO CL-01.

           MOVE VDUNUM0 TO BEG-CLASS.

      *----------------------------------------------------------------
       CL-02.
           MOVE "ENNN020CL2." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO CL-01.
           IF VDUSTATUS = "1<"
              MOVE "END" TO CLASS-FG
              PERFORM CLASS-SCAN
              IF IO-FG NOT = 0
              GO TO CL-02.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO CL-02.

           MOVE VDUNUM0 TO END-CLASS.

           IF BEG-CLASS > END-CLASS
              PERFORM INVALID-RANGE
              GO TO CL-01.

           GO TO DL-01.

      *----------------------------------------------------------------
       ALL-CL.
           MOVE "SNNN020CL1." TO VDU.
           MOVE 0 TO BEG-CLASS VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN020CL2." TO VDU.
           MOVE 99 TO END-CLASS VDUNUM0.
           PERFORM SCREENIO.

      *----------------------------------------------------------------
       DL-01.
           MOVE "ENNN042DL1." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "16" GO TO ALL-DL.
           IF VDUSTATUS = "1U" GO TO CL-02.
           IF VDUSTATUS = "1<"
              MOVE "DBEG" TO DEALER-FG
              PERFORM DEALER-SCAN
              IF IO-FG NOT = 0
              GO TO DL-01.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO DL-01.

           MOVE VDUNUM2 TO BEG-DEALER.

      *----------------------------------------------------------------
       DL-02.
           MOVE "ENNN042DL2." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO DL-01.
           IF VDUSTATUS = "1<"
              MOVE "DEND" TO DEALER-FG
              PERFORM DEALER-SCAN
              IF IO-FG NOT = 0
              GO TO DL-02.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO DL-02.

           MOVE VDUNUM2 TO END-DEALER.

           IF BEG-DEALER > END-DEALER
              PERFORM INVALID-RANGE
              GO TO DL-01.

           GO TO ENTER-ACTION-CODE.

      *----------------------------------------------------------------
       ALL-DL.
           MOVE "SNNN042DL1." TO VDU.
           MOVE 0 TO BEG-DEALER VDUNUM2.
           PERFORM SCREENIO.
           MOVE "SNNN042DL2." TO VDU.
           MOVE 9999.99 TO END-DEALER VDUNUM2.
           PERFORM SCREENIO.

      *----------------------------------------------------------------
       ENTER-ACTION-CODE.
           MOVE "E  A040ACTION." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "16" GO TO ALL-AC.
           IF VDUSTATUS = "1U" GO TO DL-01.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "1<" 
              MOVE "A2" TO CDSCAN-TYPE
              PERFORM CODE-SCAN
              IF IO-FG NOT = 0
                 GO TO ENTER-ACTION-CODE.

           IF VDUSTATUS NOT = "00" AND NOT = "1D"
              GO TO ENTER-ACTION-CODE.

           MOVE VDUBUF TO ACTIONX H-ACTION.
           GO TO AGE-01.

       ALL-AC.
           MOVE "S  A040ACTION." TO VDU.
           MOVE "????" TO VDUBUF ACTIONX H-ACTION.
           PERFORM SCREENIO.

       AGE-01.
           MOVE "ENNN040AGE1." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "16" GO TO ALLAGE.
           IF VDUSTATUS = "1U" GO TO ENTER-ACTION-CODE.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO AGE-01.

           MOVE VDUNUM0 TO BEG-AGE.

      *----------------------------------------------------------------
       AGE-02.
           MOVE "ENNN040AGE2." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO AGE-01.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO AGE-02.

           MOVE VDUNUM0 TO END-AGE.

           IF BEG-AGE > END-AGE
              PERFORM INVALID-RANGE
              GO TO AGE-01.

           IF NOT ( BEG-AGE < 30)
               MOVE "SNNA010C-POT." TO VDU
               MOVE " " TO VDUBUF
               PERFORM SCREENIO
               MOVE "SNNA010C-MISS." TO VDU
               MOVE " " TO VDUBUF
               PERFORM SCREENIO
               GO TO RAGE01.
           GO TO CPOTYN.

      *----------------------------------------------------------------
       ALLAGE.
           MOVE "SNNN040AGE1." TO VDU.
           MOVE 0 TO BEG-AGE VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN040AGE2." TO VDU.
           MOVE 9999 TO END-AGE VDUNUM0.
           PERFORM SCREENIO.

      *----------------------------------------------------------------
       CPOTYN.
           MOVE "E  A010C-POT." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO AGE-02.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO CPOTYN.

           MOVE VDUBUF TO CPOT.
           IF CPOT = "S"
                 MOVE "SNNA010C-MISS." TO VDU
                 MOVE "S" TO VDUBUF
                 PERFORM SCREENIO
                 GO TO RAGE01.
           IF CPOT NOT = "I" AND NOT = "S" AND NOT = "O"
              GO TO CPOTYN.

      *----------------------------------------------------------------
       CMISSYN.
           MOVE "E  A010C-MISS." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO CPOTYN.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO CMISSYN.

           MOVE VDUBUF TO CMISS.

           IF CMISS NOT = "I" AND NOT = "S" AND NOT = "O"
              GO TO CMISSYN.

      *----------------------------------------------------------------
       RAGE01.
           MOVE "ENNN040RAGE1." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "16" GO TO ALRAGE.
           IF VDUSTATUS = "1U"
                IF NOT (BEG-AGE < 30)
                   GO TO AGE-02
                ELSE
                IF CPOT = "S"
                    GO TO CPOTYN
                ELSE
                    GO TO CMISSYN.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO RAGE01.

           MOVE VDUNUM0 TO BEG-RAGE.

      *----------------------------------------------------------------
       RAGE02.
           MOVE "ENNN040RAGE2." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO RAGE01.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO RAGE02.

           MOVE VDUNUM0 TO END-RAGE.

           IF BEG-RAGE > END-RAGE
              PERFORM INVALID-RANGE
              GO TO RAGE01.

           IF NOT (BEG-RAGE < 30)
               MOVE "SNNA010R-POT." TO VDU
               MOVE " " TO VDUBUF
               PERFORM SCREENIO
               GO TO BAL01.
           GO TO RPOTYN.

      *----------------------------------------------------------------
       ALRAGE.
           MOVE "SNNN040RAGE1." TO VDU.
           MOVE 0 TO BEG-RAGE VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN040RAGE2." TO VDU.
           MOVE 9999 TO END-RAGE VDUNUM0.
           PERFORM SCREENIO.

      *----------------------------------------------------------------
       RPOTYN.
           MOVE "E  A010R-POT." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO RAGE02.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO RPOTYN.

           MOVE VDUBUF TO RPOT.

           IF RPOT NOT = "I" AND NOT = "S" AND NOT = "O"
              GO TO RPOTYN.

      *----------------------------------------------------------------
       BAL01.
           MOVE "ENNN070BB1." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "16" GO TO ALLBAL.
           IF VDUSTATUS = "1U"
                IF NOT (BEG-RAGE < 30)
                   GO TO RAGE02
                ELSE
                   GO TO RPOTYN.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO BAL01.

           MOVE VDUNUM0 TO BEG-BAL.

      *----------------------------------------------------------------
       BAL02.
           MOVE "ENNN070BB2." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO BAL01.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO BAL02.

           MOVE VDUNUM0 TO END-BAL.

           IF BEG-BAL > END-BAL
              PERFORM INVALID-RANGE
              GO TO BAL01.

           GO TO AMT01.

      *----------------------------------------------------------------
       ALLBAL.
           MOVE "SNNN070BB1." TO VDU.
           MOVE 0 TO BEG-BAL VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN070BB2." TO VDU.
           MOVE 9999999 TO END-BAL VDUNUM0.
           PERFORM SCREENIO.

      *----------------------------------------------------------------
       AMT01.
           MOVE "ENNN060LA1." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "16" GO TO ALLAMT.
           IF VDUSTATUS = "1U" GO TO BAL02.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO AMT01.

           MOVE VDUNUM0 TO BEG-LNAMT.

      *----------------------------------------------------------------
       AMT02.
           MOVE "ENNN060LA2." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO AMT01.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO AMT02.

           MOVE VDUNUM0 TO END-LNAMT.

           IF BEG-LNAMT > END-LNAMT
              PERFORM INVALID-RANGE
              GO TO AMT01.

           GO TO TYPEYN.

      *----------------------------------------------------------------
       ALLAMT.
           MOVE "SNNN060LA1." TO VDU.
           MOVE 0 TO BEG-LNAMT VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN060LA2." TO VDU.
           MOVE 999999 TO END-LNAMT VDUNUM0.
           PERFORM SCREENIO.

      *----------------------------------------------------------------
       TYPEYN.
           MOVE "E  A010TYPE." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO AMT02.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO TYPEYN.

           MOVE VDUBUF TO OWNED.

           IF OWNED NOT = "O" AND NOT = "S" AND NOT = "B"
              GO TO TYPEYN.

      *----------------------------------------------------------------
       PLYN.
           MOVE "E  A010PL." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO TYPEYN.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO PLYN.

           MOVE VDUBUF TO PL.

           IF PL = "I" OR "O"
              GO TO PLTYPEYN.

           IF PL NOT = "S" GO TO PLYN.

           MOVE "S  A010ACTIVE." TO VDU.
           MOVE "B" TO PLACTIVE VDUBUF.
           PERFORM SCREENIO.
           GO TO BANKRULE.

      *----------------------------------------------------------------
       PLTYPEYN.
           MOVE "E  A010PLTYPE." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO PLYN.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO PLTYPEYN.

           MOVE VDUBUF TO PLTYPE.

           IF NOT (PLTYPE = "A" OR "P" OR "2" OR "3" OR "4" OR "5")
              GO TO PLTYPEYN.

      *----------------------------------------------------------------
       ACTVYN.
           MOVE "E  A010ACTIVE." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO PLTYPEYN.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO ACTVYN.

           MOVE VDUBUF TO PLACTIVE.

           IF PLACTIVE NOT = "I" AND NOT = "A" AND NOT = "B"
              GO TO ACTVYN.

      *----------------------------------------------------------------
       BANKRULE.
           MOVE "E  A010BRULE." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO ACTVYN.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO BANKRULE.
           MOVE VDUBUF TO BRULE.
           IF BRULE = "I" GO TO RECRSE.
           IF BRULE NOT = "T" GO TO BANKRULE.
      *----------------------------------------------------------------
       RECRSE.
           MOVE "E  A010RC." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO BANKRULE.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO RECRSE.

           MOVE VDUBUF TO RC.

           IF RC NOT = "F"
              AND NOT = "P"
               AND NOT = "N"
                AND NOT = "A"
                    GO TO RECRSE.

      *----------------------------------------------------------------
       JDYN.
           MOVE "E  A010JD." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO RECRSE.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO JDYN.

           MOVE VDUBUF TO JD.

           IF JD NOT = "I" AND NOT = "S" AND NOT = "O"
              GO TO JDYN.

      *----------------------------------------------------------------
       REPOYN.
           MOVE "E  A010RP." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO JDYN.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO REPOYN.

           MOVE VDUBUF TO REPO.

           IF REPO NOT = "I" AND NOT = "S" AND NOT = "O"
              GO TO REPOYN.

      *----------------------------------------------------------------
       BANKYN.
           MOVE "E  A010BK." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO REPOYN.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO BANKYN.

           MOVE VDUBUF TO BANK.

           IF BANK NOT = "I" AND NOT = "S" AND NOT = "O"
              GO TO BANKYN.

      *----------------------------------------------------------------
       LEGLYN.
           MOVE "E  A010LG." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO BANKYN.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO LEGLYN.

           MOVE VDUBUF TO LEGL.

           IF LEGL NOT = "I" AND NOT = "S" AND NOT = "O"
              GO TO LEGLYN.

      *----------------------------------------------------------------
       DEBTYN.
           MOVE "E  A010DA." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO LEGLYN.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO DEBTYN.

           MOVE VDUBUF TO DEBT.

           IF DEBT NOT = "I" AND NOT = "S" AND NOT = "O"
              GO TO DEBTYN.

      *----------------------------------------------------------------
       ZBALYN.
           MOVE "E  A010ZBAL." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO DEBTYN.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO ZBALYN.

           MOVE VDUBUF TO ZBAL.

           IF ZBAL = "I" GO TO BRFLAG-SWITCH.
           IF ZBAL = "S" GO TO BRFLAG-SWITCH.
           IF ZBAL NOT = "O" GO TO ZBALYN.

      *----------------------------------------------------------------
       BRFLAG-SWITCH.
           MOVE "E  A010BRFLAG." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO ZBALYN.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO BRFLAG-SWITCH.

           MOVE VDUBUF TO BRFLAG.

           IF BRFLAG NOT = "D" AND NOT = "B"
                GO TO BRFLAG-SWITCH.

      *----------------------------------------------------------------
       ALPYN.
           MOVE "E  A010ALPSUM." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO BRFLAG-SWITCH.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO ALPYN.

           MOVE VDUBUF TO ALPHA-SUMMARY.

           IF ALPHA-SUMMARY = "Y" GO TO SUMYN.
           IF ALPHA-SUMMARY NOT = "N" GO TO ALPYN.

      *----------------------------------------------------------------
       SUMYN.
           MOVE "E  A010SUMONLY." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO ALPYN.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO SUMYN.

           MOVE VDUBUF TO SUM-ONLY.

           IF SUM-ONLY = "Y" GO TO LOCAL-PRINT.
           IF SUM-ONLY NOT = "N" GO TO SUMYN.

      *----------------------------------------------------------------
       LOCAL-PRINT.
           IF EXT-ROUTE-BUF NOT = "40"
              IF EXT-ROUTE-BUF NOT = "50"
                 MOVE "N" TO LOCAL-PRINT-FG
                 GO TO EXIT-PARM.

           IF BEG-BRANCH = END-BRANCH
              MOVE "N" TO LOCAL-PRINT-FG
              GO TO EXIT-PARM.

           IF GROUP-FLAG = "Y"
              MOVE "N" TO LOCAL-PRINT-FG
              GO TO EXIT-PARM.
      *-----------------------------------------------------------------
       ASK-LOCAL.
           MOVE "SPOOL TO LOCAL DIRECTORY? (Y/N)" TO WR-BUF.
           MOVE MESS-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.
           MOVE "S  A240SPOOL-PMPT:P." TO VDU.
           MOVE "SPOOL TO LOCAL    (Y/N):"    TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "E  A010LOCAL-SPOOL." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO SUMYN.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO ASK-LOCAL.

           IF VDUBUF NOT = "Y"
              IF VDUBUF NOT = "N"
                 GO TO ASK-LOCAL.

           MOVE VDUBUF TO LOCAL-PRINT-FG.

       EXIT-PARM.
           EXIT.

      ******************************************************************
       DISPLAY-PARAMETERS SECTION.
      ******************************************************************

           IF GROUP-FLAG = "Y"
              MOVE "S  A000BR1." TO VDU
              MOVE ENT-GROUP     TO VDUBUF
              PERFORM SCREENIO
              MOVE "S  A000BR2." TO VDU
              MOVE SPACES        TO VDUBUF
              PERFORM SCREENIO
           ELSE
              MOVE "SNNN040BR1." TO VDU
              MOVE BEG-BRANCH    TO VDUNUM0
              PERFORM SCREENIO
              MOVE "SNNN040BR2." TO VDU
              MOVE END-BRANCH    TO VDUNUM0
              PERFORM SCREENIO.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "SNNN040BR1." TO VDU.
           MOVE BEG-BRANCH TO VDUNUM0.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "SNNN040BR2." TO VDU.
           MOVE END-BRANCH TO VDUNUM0.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "SN N020CL1." TO VDU.
           MOVE BEG-CLASS TO VDUNUM0.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "SN N020CL2." TO VDU.
           MOVE END-CLASS TO VDUNUM0.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "SN N042DL1." TO VDU.
           MOVE BEG-DEALER TO VDUNUM2.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "SN N042DL2." TO VDU.
           MOVE END-DEALER TO VDUNUM2.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "S  A040ACTION." TO VDU.
           MOVE ACTIONX TO VDUBUF.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "SN N040AGE1." TO VDU.
           MOVE BEG-AGE TO VDUNUM0.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "SN N040AGE2." TO VDU.
           MOVE END-AGE TO VDUNUM0.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "SN A010C-POT." TO VDU.
           MOVE CPOT TO VDUBUF.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "SN A010C-MISS." TO VDU.
           MOVE CMISS TO VDUBUF.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "SN N040RAGE1." TO VDU.
           MOVE BEG-RAGE TO VDUNUM0.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "SN N040RAGE2." TO VDU.
           MOVE END-RAGE TO VDUNUM0.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "SN A010R-POT." TO VDU.
           MOVE RPOT TO VDUBUF.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "SN N070BB1." TO VDU.
           MOVE BEG-BAL TO VDUNUM0.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "SN N070BB2." TO VDU.
           MOVE END-BAL TO VDUNUM0.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "SN N060LA1." TO VDU.
           MOVE BEG-LNAMT TO VDUNUM0.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "SN N060LA2." TO VDU.
           MOVE END-LNAMT TO VDUNUM0.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "S  A010TYPE." TO VDU.
           MOVE OWNED TO VDUBUF.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "S  A010PL." TO VDU.
           MOVE PL TO VDUBUF.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "S  A010PLTYPE." TO VDU.
           MOVE PLTYPE TO VDUBUF.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "S  A010ACTIVE." TO VDU.
           MOVE PLACTIVE TO VDUBUF.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "S  A010BRULE." TO VDU.
           MOVE BRULE TO VDUBUF.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "S  A010RC." TO VDU.
           MOVE RC TO VDUBUF.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "S  A010JD." TO VDU.
           MOVE JD TO VDUBUF.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "S  A010RP." TO VDU.
           MOVE REPO TO VDUBUF.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "S  A010BK." TO VDU.
           MOVE BANK TO VDUBUF.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "S  A010LG." TO VDU.
           MOVE LEGL TO VDUBUF.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "S  A010DA." TO VDU.
           MOVE DEBT TO VDUBUF.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "S  A010ZBAL." TO VDU.
           MOVE ZBAL TO VDUBUF.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "S  A010BRFLAG." TO VDU.
           MOVE BRFLAG TO VDUBUF.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "S  A010ALPSUM." TO VDU.
           MOVE ALPHA-SUMMARY TO VDUBUF.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "S  A010SUMONLY." TO VDU.
           MOVE SUM-ONLY TO VDUBUF.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "S  A240SPOOL-PMPT:P." TO VDU.
           MOVE "SPOOL TO LOCAL    (Y/N):"    TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "S  A010LOCAL-SPOOL." TO VDU.
           MOVE LOCAL-PRINT-FG        TO VDUBUF.
           PERFORM SCREENIO.

      ******************************************************************
       DETAIL-LINE-PROCESS SECTION.
      ******************************************************************

           IF FIRST-TIME = "Y"
              MOVE EXT-CONAME-RPT-LINES TO LN-COUNT
              MOVE 1 TO LN-FEED
              MOVE "N" TO FIRST-TIME
              MOVE "Y" TO RECORDS-FOUND.

           PERFORM PROCESS-BORROWER.
           PERFORM LPSTAT-DISPLAY.
           MOVE LPS-DISPLAY-LNNO TO HOLD-LPS-NUM.
           MOVE H-LPS-NUM TO D-LOAN-NUM.
           MOVE LN-1STPYDATE TO NUM-DATE.
           MOVE NUM-DA TO D-DUEDAY.
           MOVE LN-DLNO TO D-DLNO.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - -{ PAYDATE  }
           IF ( LN-DATE-PAID-LAST NOT = ZEROES )
              MOVE LN-DATE-PAID-LAST TO DATE-YYYYMMDD
              PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
              MOVE DATE-MMDDYY TO ALP-DATE-EDIT
           ELSE
              MOVE LN-LOANDATE TO DATE-YYYYMMDD
              PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
              MOVE DATE-MMDDYY TO ALP-DATE-EDIT.

           MOVE LN-UNITPER-CD   TO DATER-UNITPER-CD.
           MOVE LN-UNITPER-FREQ TO DATER-UNITPER-FREQ.
           PERFORM UNITPER-MASK.
           MOVE ALP-DATE-EDIT   TO D-PAYDATE.

           IF ( LN-DATE-PAID-LAST = ZEROES )
              MOVE "." TO D-DASH1
              IF ( D-DASH2 = "/" )
                 MOVE "." TO D-DASH2.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF RDPOT30 = "Y"
              MOVE "<P>" TO D-RECENCYX
           ELSE
           IF RDPOT30 = "M"
              MOVE "<M>" TO D-RECENCYX
           ELSE
           IF RDPOTCHGOFF = "Y"
              MOVE "<PC>" TO D-RECENCYX
           ELSE
           IF RDPOTCHGOFF = "M"
              MOVE "<PL>" TO D-RECENCYX
           ELSE
              MOVE RECDEL TO D-RECENCY.

           MOVE LN-RECPROFILE(1) TO D-PYRECPRO.
           INSPECT D-XPYRECPRO REPLACING ALL "9" BY "-".

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      * DETERMINE ACCOUNT STATUS (IB-PC):
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

           MOVE 0 TO UNEARNED EARNED INTEREST.
           MOVE 0 TO HOLD-CURBAL HOLD-OTHBAL.
           MOVE TRANS-DATE TO IBPC-DATE.
           PERFORM IBPC-TEST.

           PERFORM COMPUTE-EARNINGS.
           MOVE LN-TOTERN TO EARNED.
           MOVE EARN-TOT-UNEARN TO UNEARNED.

           IF BR-AGE-ERN = "T" OR SPACE
              MOVE LN-TOTERN TO D-UNEARNED
           ELSE
              PERFORM COMPUTE-EARNINGS
              MOVE EARN-TOT-UNEARN TO D-UNEARNED.

           MOVE LN-CONPROFILE(1) TO D-PROFILE.
           INSPECT D-XPROFILE REPLACING ALL "9" BY "-".
           IF CPOT30 = "Y"
              MOVE "<P>" TO D-CONTUALX
           ELSE
           IF CPOT30 = "M"
              MOVE "<M>" TO D-CONTUALX
           ELSE
           IF CPOTCHGOFF = "Y"
              MOVE "<PC>" TO D-CONTUALX
           ELSE
           IF CPOTCHGOFF = "M"
              MOVE "<PL>" TO D-CONTUALX
           ELSE
              MOVE CONTRACTUAL TO D-CONTUAL.

           MOVE PDTH-DATE TO D-PDTHRU.

           ADD LN-CURBAL TO HOLD-CURBAL.
           SUBTRACT LN-OT2BAL FROM HOLD-CURBAL.
           ADD LN-OTHBAL LN-OT2BAL TO HOLD-OTHBAL.

           IF HOLD-OTHBAL NOT = 0
              MOVE HOLD-OTHBAL TO D-OTHBAL.

           IF LN-INTBAL NOT = 0 OR LN-LCBAL NOT = 0
              ADD LN-INTBAL LN-LCBAL GIVING D-INTBAL.

           IF HOLD-CURBAL NOT = 0
              MOVE HOLD-CURBAL TO D-CURBAL.

           PERFORM UPDATE-BY-DUEDAY.

           IF ALPHA-SUMMARY = "Y"
              PERFORM UPDATE-BY-ALPHA.

           MOVE 1 TO LEV.
           PERFORM DETAIL-LN-ACCTBL.
           MOVE 2 TO LEV.
           PERFORM DETAIL-LN-ACCTBL.
           MOVE 3 TO LEV.
           PERFORM DETAIL-LN-ACCTBL.

           IF SUM-ONLY NOT = "Y"
              PERFORM WRITE-DETAIL-LINE
           ELSE
              MOVE SPACES TO DETAIL-LINE.

           GO TO END-DETAIL-LN-PROCESS.


      *----------------------------------------------------------------
       DETAIL-LN-ACCTBL.
      *----------------------------------------------------------------
      *    ADDING TO TOTALS: CONTRACTUAL
      *      ...SEE LNAGED.C FOR EXPLAINATION OF IF
      *----------------------------------------------------------------
           IF ( PL = "O" ) OR ( LN-PLCD = " " )
              ADD 1 TO T-NO(LEV CSUB 1)
              ADD HOLD-OTHBAL TO T-OTH(LEV CSUB 1)
              ADD LN-INTBAL TO T-INT(LEV CSUB 1)
              ADD LN-LCBAL TO T-INT(LEV CSUB 1)
              ADD HOLD-CURBAL TO T-BAL(LEV CSUB 1)
              ADD UNEARNED  TO T-UNE(LEV CSUB 1)
              ADD INTEREST  TO T-IER(LEV CSUB 1)
              ADD EARNED TO T-ERN(LEV CSUB 1)
      *----------------------------------------------------------------
      *    ADDING TO TOTALS: RECENCY
      *----------------------------------------------------------------
              ADD 1 TO T-NO(LEV RDSUB 2)
              ADD HOLD-OTHBAL TO T-OTH(LEV RDSUB 2)
              ADD LN-INTBAL TO T-INT(LEV RDSUB 2)
              ADD LN-LCBAL TO T-INT(LEV RDSUB 2)
              ADD HOLD-CURBAL TO T-BAL(LEV RDSUB 2)
              ADD UNEARNED  TO T-UNE(LEV RDSUB 2)
              ADD INTEREST  TO T-IER(LEV RDSUB 2)
              ADD EARNED TO T-ERN(LEV RDSUB 2)

      *----------------------------------------------------------------
      *    ADDING TO TOTALS: PAID AHEAD
      *----------------------------------------------------------------
              IF (PSUB > 0) AND
                 (SP-BANKRULE NOT = "D" AND SP-BANKRULE NOT = "F")
                 ADD 1 TO T-NO(LEV PSUB 3)
                 ADD HOLD-OTHBAL TO T-OTH(LEV PSUB 3)
                 ADD LN-INTBAL TO T-INT(LEV PSUB 3)
                 ADD LN-LCBAL TO T-INT(LEV PSUB 3)
                 ADD HOLD-CURBAL TO T-BAL(LEV PSUB 3)
                 ADD UNEARNED  TO T-UNE(LEV PSUB 3)
                 ADD INTEREST  TO T-IER(LEV PSUB 3)
                 ADD EARNED TO T-ERN(LEV PSUB 3).

      *----------------------------------------------------------------
      *    ADDING TO TOTALS: REPO
      *----------------------------------------------------------------
           IF ( LN-IS-REPO-AR ) AND
              ( LN-PLCD = SPACES )
              ADD 1 TO T-NO(LEV RPSUB 4)
              ADD HOLD-OTHBAL TO T-OTH(LEV RPSUB 4)
              ADD LN-INTBAL TO T-INT(LEV RPSUB 4)
              ADD LN-LCBAL TO T-INT(LEV RPSUB 4)
              ADD HOLD-CURBAL TO T-BAL(LEV RPSUB 4)
              ADD UNEARNED  TO T-UNE(LEV RPSUB 4)
              ADD INTEREST  TO T-IER(LEV RPSUB 4)
              ADD EARNED TO T-ERN(LEV RPSUB 4).

      *----------------------------------------------------------------
      *    ADDING TO TOTALS: P & L
      *----------------------------------------------------------------
           IF ( LN-PLCD NOT = SPACES )
              ADD 1 TO T-NO(LEV PLSUB 5)
              ADD HOLD-OTHBAL TO T-OTH(LEV PLSUB 5)
              ADD LN-INTBAL TO T-INT(LEV PLSUB 5)
              ADD LN-LCBAL TO T-INT(LEV PLSUB 5)
              ADD HOLD-CURBAL TO T-BAL(LEV PLSUB 5)
              ADD UNEARNED  TO T-UNE(LEV PLSUB 5)
              ADD INTEREST  TO T-IER(LEV PLSUB 5)
              ADD EARNED TO T-ERN(LEV PLSUB 5).

      *----------------------------------------------------------------
       UPDATE-BY-DUEDAY.
           MOVE LN-1STPYDATE TO NUM-DATE.
           MOVE NUM-DA TO D-DUEDAY.

           IF NUM-DA < 1 OR NUM-DA > 31
              MOVE 32 TO NUM-DA.

           ADD 1 TO DD-NO(NUM-DA).
           ADD HOLD-OTHBAL TO DD-OTH(NUM-DA).
           ADD LN-INTBAL TO DD-INT(NUM-DA).
           ADD LN-LCBAL TO DD-INT(NUM-DA).
           ADD HOLD-CURBAL TO DD-BAL(NUM-DA).
           ADD LN-REGPYAMT TO DD-UNE(NUM-DA).

      *----------------------------------------------------------------
       UPDATE-BY-ALPHA.
           MOVE BW-LNAME TO NM-TEST.

           IF NM-TEST < "A" OR NM-TEST > "Z"
              MOVE 27 TO NM-SUB
           ELSE
              SUBTRACT NM-A FROM NM-B GIVING NM-SUB.

           ADD 1 TO NM-NO(NM-SUB).
           ADD HOLD-OTHBAL TO NM-OTH(NM-SUB).
           ADD LN-INTBAL TO NM-INT(NM-SUB).
           ADD LN-LCBAL TO NM-INT(NM-SUB).
           ADD HOLD-CURBAL TO NM-BAL(NM-SUB).
           ADD LN-REGPYAMT TO NM-UNE(NM-SUB).

       END-DETAIL-LN-PROCESS.
           EXIT.

      ******************************************************************
       PROCESS-BORROWER SECTION.
      ******************************************************************

           MOVE LN-SSNO(1) TO BW-SSNO.
           PERFORM READ-BW1-FILE.
           IF IO-FG NOT = 0
               MOVE "**NOT ON FILE**" TO D-NAME
               GO TO END-PROCESS-BORROWER.

           MOVE BW-LNAME TO NAME-WORKER.
           MOVE BW-FNAME TO FIRST-NAME.
           INSPECT NAME-WORKER REPLACING FIRST "  " BY ",?".
           INSPECT NAME-WORKER REPLACING FIRST "                "
                   BY FIRST-NAME.
           INSPECT NAME-WORKER REPLACING ALL "?" BY " ".
           MOVE NAME-WORKER TO D-NAME.

       END-PROCESS-BORROWER.
           EXIT.

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
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE HEAD2 TO PR-REC.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-PR-REC.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE HEAD3 TO PR-REC.
           PERFORM WRITE-PR-REC.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE HEAD4 TO PR-REC.
           PERFORM WRITE-PR-REC.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

           IF SUMMARY-FG = "N"
              MOVE HEAD5 TO PR-REC
              PERFORM WRITE-PR-REC
              MOVE HEAD6 TO PR-REC
              PERFORM WRITE-PR-REC
              MOVE 6 TO LN-COUNT
              MOVE 2 TO LN-FEED
            ELSE
              MOVE 4 TO LN-COUNT
              MOVE 2 TO LN-FEED.

       END-HEAD-PAGE.
           EXIT.

      ******************************************************************
       WRITE-DETAIL-LINE SECTION.
      ******************************************************************
           ADD LN-FEED TO LN-COUNT.

           IF LN-COUNT > EXT-CONAME-RPT-LINES
              PERFORM HEAD-PAGE.

           MOVE DETAIL-LINE TO PR-REC.
           PERFORM WRITE-PR-REC.
           MOVE 1 TO LN-FEED.
           MOVE SPACES TO DETAIL-LINE.

      ******************************************************************
       PRINT-RANGE-LINES SECTION.
      ******************************************************************

           MOVE "Y" TO SUMMARY-FG.
           MOVE EXT-CONAME-RPT-LINES TO LN-COUNT.
           MOVE 4 TO LN-FEED.
           MOVE "BEGINNING BRANCH   :" TO RANGE-LINE.
           MOVE BEG-BRANCH             TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "ENDING             :" TO RANGE-LINE.
           MOVE END-BRANCH             TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "BEGINNING CLASS    :" TO RANGE-LINE.
           MOVE BEG-CLASS              TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "ENDING             :" TO RANGE-LINE.
           MOVE END-CLASS             TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "BEGINNING DEALER   :" TO RANGE-LINE.
           MOVE BEG-DEALER TO RS-DEALER.
           MOVE RS-DL                  TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "ENDING             :" TO RANGE-LINE.
           MOVE END-DEALER TO RS-DEALER.
           MOVE RS-DL                  TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "ACTION CODE 2      :" TO RANGE-LINE.
           MOVE ACTION-CD (1) TO ACTION-CHAR (1).
           MOVE ACTION-CD (2) TO ACTION-CHAR (2).
           MOVE ACTION-CD (3) TO ACTION-CHAR (3).
           MOVE ACTION-CD (4) TO ACTION-CHAR (4).
           MOVE ACTION-WK              TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "CONTRACTUAL AGE    :" TO RANGE-LINE.
           MOVE BEG-AGE                TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "ENDING             :" TO RANGE-LINE.
           MOVE END-AGE TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "POTENTIALS         :" TO RANGE-LINE.
           MOVE CPOT                   TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "MISS PAYS          :" TO RANGE-LINE.
           MOVE CMISS                  TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "RECENCY AGE        :" TO RANGE-LINE.
           MOVE BEG-RAGE               TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "ENDING             :" TO RANGE-LINE.
           MOVE END-RAGE               TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "POTENTIALS         :" TO RANGE-LINE.
           MOVE RPOT                   TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "BEGINNING BALANCE  :" TO RANGE-LINE.
           MOVE BEG-BAL                TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "ENDING             :" TO RANGE-LINE.
           MOVE END-BAL                TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "BEGINNING LOAN AMT :" TO RANGE-LINE.
           MOVE BEG-LNAMT              TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "ENDING             :" TO RANGE-LINE.
           MOVE END-LNAMT              TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "OWNED,SERVICE,BOTH :" TO RANGE-LINE.
           MOVE OWNED                  TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "P & L              :" TO RANGE-LINE.
           MOVE PL                     TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "P & L TYPE         :" TO RANGE-LINE.
           MOVE PLTYPE                 TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "ACTIVE             :" TO RANGE-LINE.
           MOVE PLACTIVE               TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "BANKRULE    (I/T)  :" TO RANGE-LINE.
           MOVE BRULE                  TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "RECOURSE           :" TO RANGE-LINE.
           MOVE RC                     TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "JUDGEMENTS         :" TO RANGE-LINE.
           MOVE JD                     TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "REPO               :" TO RANGE-LINE.
           MOVE REPO                   TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "BANKRUPT           :" TO RANGE-LINE.
           MOVE BANK                   TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "LEGAL              :" TO RANGE-LINE.
           MOVE LEGL                   TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "DEBT-AID           :" TO RANGE-LINE.
           MOVE DEBT                   TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "ZERO BALANCES      :" TO RANGE-LINE.
           MOVE ZBAL                   TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF BRFLAG = "D"
              MOVE "BY DEALER NUMBER" TO RANGE-LINE
           ELSE
              MOVE "BY DEALER BRANCH" TO RANGE-LINE.
           MOVE " " TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.

      ******************************************************************
       SET-BRANCH-HEADINGS SECTION.
      ******************************************************************

           MOVE HOLD-BRNO TO BR-NO H-BRNO.
           MOVE BR-NAME   TO H-BRDESC.

           IF BR-AGE-ERN = "T" OR SPACE
              MOVE "TOT EARN" TO HEAD5T
              MOVE "TO-DATE" TO HEAD6T
            ELSE
              MOVE "UNEARNED" TO HEAD5T
              MOVE "CHARGES" TO HEAD6T.

      ******************************************************************
       SET-DEALER-HEADINGS SECTION.
      ******************************************************************

           IF BRFLAG = "B"
              MOVE HOLD-DEALER TO H-DEALER DL-DLNO
           ELSE
              MOVE HOLD-DEALER TO H-DEALER-2
              MOVE H-DEALER-2 TO DL-DLNO.

           PERFORM OPEN-DL1-FILE.
           MOVE DL-PATH-OWNBR TO DL-BRNO.
           
           PERFORM START-DL1-FILE.
           PERFORM READ-DL1-FILE-NEXT.
           IF IO-FG = 9 OR (DL-BRNO NOT = DL-PATH-OWNBR)
              MOVE "UNKNOWN DEALER" TO H-DEALERNM
           ELSE
              MOVE DL-NAME TO H-DEALERNM.

           PERFORM CLOSE-DL1-FILE.
           MOVE "N" TO SUMMARY-FG.

      ******************************************************************
       DEALER-TOTALS SECTION.
      ******************************************************************

           IF MORE-THAN-1-DEALER = "Y"
              MOVE "Y"             TO SUMMARY-FG
              MOVE EXT-CONAME-RPT-LINES    TO LN-COUNT
              MOVE 1               TO LN-FEED
              MOVE "DLR   TOTALS:" TO S-SUMNAME
              PERFORM WRITE-DETAIL-LINE
              MOVE 1               TO LEV
              PERFORM PRINT-AGEING-SUMMARY.

           MOVE ZEROS TO TOTAL-TBL(1).
           MOVE "N" TO SUMMARY-FG.
           MOVE EXT-CONAME-RPT-LINES TO LN-COUNT.
           MOVE 1 TO LN-FEED.

      ******************************************************************
       BRANCH-TOTALS SECTION.
      ******************************************************************

           MOVE "Y" TO SUMMARY-FG.
           MOVE SPACES TO H-DEALERX.
           MOVE "**** ALL DEALERS ****" TO H-DEALERNM.
           MOVE EXT-CONAME-RPT-LINES TO LN-COUNT.
           MOVE 1 TO LN-FEED.
           MOVE "BRANCH TOTALS:" TO S-SUMNAME.
           PERFORM WRITE-DETAIL-LINE.
           MOVE 2 TO LEV.
           PERFORM PRINT-AGEING-SUMMARY.

           MOVE ZEROS TO TOTAL-TBL(2).
           MOVE "N" TO SUMMARY-FG.
           MOVE EXT-CONAME-RPT-LINES TO LN-COUNT.
           MOVE 1 TO LN-FEED.

      ******************************************************************
       COMPANY-TOTALS SECTION.
      ******************************************************************

           MOVE "Y" TO SUMMARY-FG.
           MOVE SPACES TO H-BRNOX.
           MOVE "**** ALL BRANCHES ****" TO H-BRDESC.
           MOVE EXT-CONAME-RPT-LINES TO LN-COUNT.
           MOVE 1 TO LN-FEED.
           MOVE "COMPANY TOTALS:" TO S-SUMNAME.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE 3 TO LEV.
           PERFORM PRINT-AGEING-SUMMARY.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

           MOVE "   PMT DUE----%" TO SUMMARY1X.
           PERFORM RESET-TOT-PAGE.
           MOVE "********* BY PAYMENT DUE DATE *********"
                TO S-AGETYPE.
           MOVE "N" TO NM-PRINT.
           MOVE 32 TO NM-SUB.
           PERFORM PRINT-BYDUEDATE-SUMMARY.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

           IF ALPHA-SUMMARY = "Y"
              PERFORM RESET-TOT-PAGE
              PERFORM MOVE-NM-TO-DD
              MOVE "****** A L P H A   S U M M A R Y ******"
                 TO S-AGETYPE
              MOVE "Y" TO NM-PRINT
              MOVE 27 TO NM-SUB
              PERFORM PRINT-BYDUEDATE-SUMMARY.

      ******************************************************************
       PRINT-AGEING-SUMMARY SECTION.
      ******************************************************************

           PERFORM ZERO-TT-TOTALS.
           PERFORM ACCUM-AGEING-TOTALS
                   VARYING AGE FROM 1 BY 1 UNTIL AGE > 10
                   AFTER TYP FROM 1 BY 1 UNTIL TYP > 5.

           PERFORM CONTRACT-PRINT.

           IF ( REPO NOT = "S" ) AND
              ( PL   NOT = "O" )
              PERFORM REPO-PRINT.

           IF (  PL = "O" ) OR
              (( PL = "I" ) AND ( REPO NOT = "O" ))
              PERFORM PL-PRINT.

       EXIT-PRINT-AGEING-SUMMARY.
           EXIT.

      ***********************************************
       CONTRACT-PRINT SECTION.
      ***********************************************
           MOVE 1 TO TYP.
           MOVE "********* CONTRACTUAL DELINQUENCY *********"
                                                       TO S-AGETYPE.
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE SUMMARY1 TO DETAIL-LINE.
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE 2 TO LN-FEED.
           PERFORM CLEAR-SUM-SUB-TOTALS.
           PERFORM VARYING AGE FROM 1 BY 1 UNTIL AGE > 10
              PERFORM PRINT-DETAIL-SUM
              IF TYP = 1 OR 2
                 PERFORM ACCUM-SUM-SUB-TOTALS
              END-IF
              IF TYP = 1 OR 3
                 IF AGE = 7 OR 9
                    PERFORM PRINT-SUM-SUB-TOTALS
                 END-IF
              END-IF
           END-PERFORM.
           MOVE 2 TO LN-FEED.
           PERFORM PRINT-SUM-TOTALS.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "*********** RECENCY DELINQUENCY ***********"
                                                      TO S-AGETYPE.
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE SUMMARY1 TO DETAIL-LINE.
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE 2 TO LN-FEED.
           PERFORM CLEAR-SUM-SUB-TOTALS.
           PERFORM VARYING AGE FROM 1 BY 1 UNTIL AGE > 10
                     AFTER TYP FROM 2 BY 1 UNTIL TYP > 3
              PERFORM PRINT-DETAIL-SUM
              IF TYP = 1 OR 2
                 PERFORM ACCUM-SUM-SUB-TOTALS
              END-IF
              IF TYP = 1 OR 3
                 IF AGE = 7 OR 9
                    PERFORM PRINT-SUM-SUB-TOTALS
                 END-IF
              END-IF
           END-PERFORM.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE 2 TO TYP.
           MOVE 2 TO LN-FEED.
           PERFORM PRINT-SUM-TOTALS.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE 3 TO TYP.
           MOVE 1 TO LN-FEED.
           PERFORM PRINT-SUM-TOTALS.

       END-CONTRACT-PRINT.
           EXIT.

      ***********************************************
       REPO-PRINT SECTION.
      ***********************************************
           MOVE "  ************* REPOSSESSION ************"
                          TO S-AGETYPE.
           MOVE 2         TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.
           MOVE SUMMARY1  TO DETAIL-LINE.
           MOVE 2         TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.
           MOVE 2         TO LN-FEED.

           PERFORM CLEAR-SUM-SUB-TOTALS.
           MOVE 4         TO TYP.
           PERFORM VARYING AGE FROM 1 BY 1 UNTIL AGE > 2
                   PERFORM PRINT-DETAIL-SUM
           END-PERFORM.

           MOVE 4         TO TYP.
           MOVE 2         TO LN-FEED.
           PERFORM PRINT-SUM-TOTALS.
           MOVE 1         TO LN-FEED.

       END-REPO-PRINT.
           EXIT.

      ***********************************************
       PL-PRINT SECTION.
      ***********************************************
           MOVE "  **************** P & L ****************"
                          TO S-AGETYPE.
           MOVE 2         TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.
           MOVE SUMMARY1  TO DETAIL-LINE.
           MOVE 2         TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.
           MOVE 2         TO LN-FEED.

           PERFORM CLEAR-SUM-SUB-TOTALS.
           MOVE 5         TO TYP.
           PERFORM VARYING AGE FROM 1 BY 1 UNTIL AGE > 5
                   PERFORM PRINT-DETAIL-SUM
           END-PERFORM.

           MOVE 5         TO TYP.
           MOVE 2         TO LN-FEED.
           PERFORM PRINT-SUM-TOTALS.
           MOVE 1         TO LN-FEED.

       END-PL-PRINT.
           EXIT.

      ******************************************************************
       PRINT-DETAIL-SUM SECTION.
      ******************************************************************

           IF T-NO(LEV AGE TYP) = 0
              IF T-BAL(LEV AGE TYP) = 0
                 IF T-INT(LEV AGE TYP) = 0
                    IF T-OTH(LEV AGE TYP) = 0
                       IF T-UNE(LEV AGE TYP) = 0
                          IF T-ERN(LEV AGE TYP) = 0
                             IF T-IER(LEV AGE TYP) = 0
                                GO TO END-PRINT-DETAIL-SUM.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF TYP = 3
              MOVE "  PAID-AHEAD" TO S-NAME
           ELSE
           IF TYP = 4
              MOVE REPO-DESC(AGE) TO S-NAME
           ELSE
           IF TYP = 5
              MOVE PL-DESC(AGE) TO S-NAME
           ELSE
              MOVE AGE-DESC(AGE) TO S-NAME.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF T-NO(LEV AGE TYP) NOT = 0
              MOVE T-NO(LEV AGE TYP) TO S-NO
              IF TYP NOT = 3
                 IF TT-NO(TYP) NOT = 0
                    COMPUTE S-NO-P ROUNDED =
                        ( T-NO(LEV AGE TYP) / TT-NO(TYP) ) * 100
                 ELSE
                    MOVE 0 TO S-NO-P.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF T-BAL(LEV AGE TYP) NOT = 0
              MOVE T-BAL(LEV AGE TYP) TO S-BAL
              IF TYP NOT = 3
                 IF TT-BAL(TYP) NOT = 0
                    COMPUTE S-BAL-P ROUNDED =
                        ( T-BAL(LEV AGE TYP) / TT-BAL(TYP) ) * 100
                 ELSE
                    MOVE 0 TO S-BAL-P.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF T-INT(LEV AGE TYP) NOT = 0
              MOVE T-INT(LEV AGE TYP) TO S-INT
              IF TYP NOT = 3
                 IF TT-INT(TYP) NOT = 0
                    COMPUTE S-INT-P ROUNDED =
                        ( T-INT(LEV AGE TYP) / TT-INT(TYP) ) * 100
                 ELSE
                    MOVE 0 TO S-INT-P.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF T-OTH(LEV AGE TYP) NOT = 0
              MOVE T-OTH(LEV AGE TYP) TO S-OTH
              IF TYP NOT = 3
                 IF TT-OTH(TYP) NOT = 0
                    COMPUTE S-OTH-P ROUNDED =
                        ( T-OTH(LEV AGE TYP) / TT-OTH(TYP) ) * 100
                 ELSE
                    MOVE 0 TO S-OTH-P.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF T-UNE(LEV AGE TYP) NOT = 0
              MOVE T-UNE(LEV AGE TYP) TO S-UNE
              IF TYP NOT = 3
                 IF TT-UNE(TYP) NOT = 0
                    COMPUTE S-UNE-P ROUNDED =
                        ( T-UNE(LEV AGE TYP) / TT-UNE(TYP) ) * 100
                 ELSE
                    MOVE 0 TO S-UNE-P.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF T-ERN(LEV AGE TYP) NOT = 0
              MOVE T-ERN(LEV AGE TYP) TO S-ERN
              IF TYP NOT = 3
                 IF TT-ERN(TYP) NOT = 0
                    COMPUTE S-ERN-P ROUNDED =
                        ( T-ERN(LEV AGE TYP) / TT-ERN(TYP) ) * 100
                 ELSE
                    MOVE 0 TO S-ERN-P.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *     IF T-IER(LEV AGE TYP) NOT = 0
      *        MOVE T-IER(LEV AGE TYP) TO S-IER
      *        IF TYP NOT = 3
      *           IF TT-IER(TYP) NOT = 0
      *              COMPUTE S-IER-P ROUNDED =
      *                  ( T-IER(LEV AGE TYP) / TT-IER(TYP) ) * 100
      *           ELSE
      *              MOVE 0 TO S-IER-P.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           PERFORM WRITE-DETAIL-LINE.

       END-PRINT-DETAIL-SUM.
           EXIT.

      ******************************************************************
      *  PRINT SUB TOTALS FOR DELINQUENCY (30,60,90,120,150,180,210+)
      *                       SLOW  (ABOVE DELINQUENCY, <M>, <P>)
      ******************************************************************
       PRINT-SUM-SUB-TOTALS SECTION.
           IF NOT ( AGE = 7 OR 9 )
              GO TO EXIT-PRINT-SUM-SUB-TOTALS.

      * DO NOT PRINT DELINQ TOTALS IF THERE ARE NO DELINQ ACCTS
           IF AGE = 7
              IF SUB-TT-NO(1)  = 0 AND SUB-TT-BAL(1) = 0 AND
                 SUB-TT-INT(1) = 0 AND SUB-TT-OTH(1) = 0 AND
                 SUB-TT-UNE(1) = 0 AND SUB-TT-ERN(1) = 0 AND
                 SUB-TT-IER(1) = 0
                    GO TO EXIT-PRINT-SUM-SUB-TOTALS
              ELSE
                 MOVE 1 TO SUB1
                 MOVE "   TOTAL DEL" TO S-NAME.

      * DO NOT PRINT SLOW TOTALS IF THERE ARE NO <M> OR <P> ACCTS
      *       (IE. SLOW TOTALS = DELINQ TOTALS)
           IF AGE = 9
              IF SUB-TT-TYPETBL(2) = SUB-TT-TYPETBL(1)
                    GO TO EXIT-PRINT-SUM-SUB-TOTALS
              ELSE
                 MOVE 2 TO SUB1
                 MOVE "  TOTAL SLOW" TO S-NAME.

      * WHEN WE GET HERE TYP = 1 (CONTRACTUAL) OR 3 (PAID-AHEAD)
      * BUT WE REALLY WANT TO TOTAL TYP = 1 (CONTRACTUAL)
      * OR TYP = 2 (RECENCY)
           IF TYP = 3
              MOVE 2 TO SUB2
           ELSE
              MOVE 1 TO SUB2.

           IF SUB-TT-NO(SUB1) NOT = 0
              MOVE SUB-TT-NO(SUB1)  TO S-NO
              COMPUTE S-NO-P ROUNDED =
                      ( SUB-TT-NO(SUB1) / TT-NO(SUB2) ) * 100.

           IF SUB-TT-BAL(SUB1) NOT = 0
              MOVE SUB-TT-BAL(SUB1) TO S-BAL
              COMPUTE S-BAL-P ROUNDED =
                      ( SUB-TT-BAL(SUB1) / TT-BAL(SUB2) ) * 100.

           IF SUB-TT-INT(SUB1) NOT = 0
              MOVE SUB-TT-INT(SUB1) TO S-INT
              COMPUTE S-INT-P ROUNDED =
                      ( SUB-TT-INT(SUB1) / TT-INT(SUB2) ) * 100.

           IF SUB-TT-OTH(SUB1) NOT = 0
              MOVE SUB-TT-OTH(SUB1) TO S-OTH
              COMPUTE S-OTH-P ROUNDED =
                      ( SUB-TT-OTH(SUB1) / TT-OTH(SUB2) ) * 100.

           IF SUB-TT-UNE(SUB1) NOT = 0
              MOVE SUB-TT-UNE(SUB1) TO S-UNE
              COMPUTE S-UNE-P ROUNDED =
                      ( SUB-TT-UNE(SUB1) / TT-UNE(SUB2) ) * 100.

           IF SUB-TT-ERN(SUB1) NOT = 0
              MOVE SUB-TT-ERN(SUB1) TO S-ERN
              COMPUTE S-ERN-P ROUNDED =
                      ( SUB-TT-ERN(SUB1) / TT-ERN(SUB2) ) * 100.

           PERFORM WRITE-DETAIL-LINE.

      * SLOW TOTALS NEED TO INCLUDE DELINQUENCY
      * ONLY CLEAR AFTER SLOW TOTALS ARE PRINTED
           IF AGE = 9
              PERFORM CLEAR-SUM-SUB-TOTALS.

       EXIT-PRINT-SUM-SUB-TOTALS.
           EXIT.

      ****************************************************
       CLEAR-SUM-SUB-TOTALS SECTION.
      ****************************************************

      * CLEAR DEL TOTALS
           MOVE 0 TO SUB-TT-NO(1)  SUB-TT-BAL(1) SUB-TT-INT(1)
                     SUB-TT-OTH(1) SUB-TT-UNE(1) SUB-TT-ERN(1)
                     SUB-TT-IER(1).

      * CLEAR SLOW TOTALS
           MOVE 0 TO SUB-TT-NO(2)  SUB-TT-BAL(2) SUB-TT-INT(2)
                     SUB-TT-OTH(2) SUB-TT-UNE(2) SUB-TT-ERN(2)
                     SUB-TT-IER(2).

       EXIT-CLEAR-SUM-SUB-TOTALS.
           EXIT.

      *************************************************
       ACCUM-SUM-SUB-TOTALS SECTION.
      *************************************************

      * INCREMENT SLOW TOTALS (30/60/90/120/150/180/210+/<M>/<P>)
           ADD  T-NO(LEV AGE TYP) TO SUB-TT-NO(2).
           ADD T-BAL(LEV AGE TYP) TO SUB-TT-BAL(2).
           ADD T-INT(LEV AGE TYP) TO SUB-TT-INT(2).
           ADD T-OTH(LEV AGE TYP) TO SUB-TT-OTH(2).
           ADD T-UNE(LEV AGE TYP) TO SUB-TT-UNE(2).
           ADD T-ERN(LEV AGE TYP) TO SUB-TT-ERN(2).
           ADD T-IER(LEV AGE TYP) TO SUB-TT-IER(2).

      * INCREMENT DEL TOTALS (30/60/90/120/150/180/210+)
           IF AGE < 8
              ADD  T-NO(LEV AGE TYP) TO SUB-TT-NO(1)
              ADD T-BAL(LEV AGE TYP) TO SUB-TT-BAL(1)
              ADD T-INT(LEV AGE TYP) TO SUB-TT-INT(1)
              ADD T-OTH(LEV AGE TYP) TO SUB-TT-OTH(1)
              ADD T-UNE(LEV AGE TYP) TO SUB-TT-UNE(1)
              ADD T-ERN(LEV AGE TYP) TO SUB-TT-ERN(1)
              ADD T-IER(LEV AGE TYP) TO SUB-TT-IER(1).

       EXIT-ACCUM-SUM-SUB-TOTALS.
           EXIT.

      ******************************************************************
       PRINT-BYDUEDATE-SUMMARY SECTION.
      ******************************************************************

           PERFORM ZERO-TT-TOTALS.
           PERFORM ACCUM-BYDUEDATE-TOTALS
                   VARYING LEV FROM 1 BY 1 UNTIL LEV > NM-SUB.
           MOVE 1 TO LEV.
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.
           MOVE SUMMARY1 TO DETAIL-LINE.
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.
           MOVE 2 TO LN-FEED.
           PERFORM PRINT-BYDUEDATE-SUM VARYING LEV
                   FROM 1 BY 1 UNTIL LEV > NM-SUB.
           MOVE 2 TO LN-FEED.
           PERFORM PRINT-DUE-TOTALS.

      ******************************************************************
       PRINT-BYDUEDATE-SUM SECTION.
      ******************************************************************

           IF DD-NO(LEV) = 0
              IF DD-BAL(LEV) = 0
                 IF DD-INT(LEV) = 0
                    IF DD-OTH(LEV) = 0
                       IF DD-UNE(LEV) = 0
                          GO TO END-PRINT-BYDUEDATE-SUM.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF NM-PRINT = "N"
              MOVE LEV TO NUM-DA
              MOVE NUM-DA TO S-NAMEDUE
           ELSE
              ADD LEV NM-A GIVING NM-B
              MOVE NM-TEST TO S-LASTINITIAL.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE ":" TO S-NAMECOLON.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

           IF DD-NO(LEV) NOT = 0
              MOVE DD-NO(LEV) TO S-NO
              IF TT-NO(1) NOT = 0
                 COMPUTE S-NO-P ROUNDED =
                     ( DD-NO(LEV) / TT-NO(1) ) * 100
              ELSE
                 MOVE 0 TO S-NO-P.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF DD-BAL(LEV) NOT = 0
              MOVE DD-BAL(LEV) TO S-BAL
              IF TT-BAL(1) NOT = 0
                 COMPUTE S-BAL-P ROUNDED =
                     ( DD-BAL(LEV) / TT-BAL(1) ) * 100
              ELSE
                 MOVE 0 TO S-BAL-P.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF DD-INT(LEV) NOT = 0
              MOVE DD-INT(LEV) TO S-INT
              IF TT-INT(1) NOT = 0
                 COMPUTE S-INT-P ROUNDED =
                     ( DD-INT(LEV) / TT-INT(1) ) * 100
              ELSE
                 MOVE 0 TO S-INT-P.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF DD-OTH(LEV) NOT = 0
              MOVE DD-OTH(LEV) TO S-OTH
              IF TT-OTH(1) NOT = 0
                 COMPUTE S-OTH-P ROUNDED =
                     ( DD-OTH(LEV) / TT-OTH(1) ) * 100
              ELSE
                 MOVE 0 TO S-OTH-P.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    ACTUALLY HOLDS REGPYAMT SUMMATION
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

           IF DD-UNE(LEV) NOT = 0
              MOVE DD-UNE(LEV) TO S-UNE
              IF TT-UNE(1) NOT = 0
                 COMPUTE S-UNE-P ROUNDED =
                     ( DD-UNE(LEV) / TT-UNE(1) ) * 100
              ELSE
                 MOVE 0 TO S-UNE-P.

           PERFORM WRITE-DETAIL-LINE.

       END-PRINT-BYDUEDATE-SUM.
           EXIT.

      ******************************************************************
       ZERO-TT-TOTALS SECTION.
      ******************************************************************
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 5
              MOVE 0 TO TT-NO(SUB) TT-BAL(SUB) TT-INT(SUB)
                        TT-OTH(SUB) TT-UNE(SUB) TT-ERN(SUB)
                        TT-IER(SUB)
           END-PERFORM.

       END-ZERO-TT-TOTALS.
           EXIT.

      ******************************************************************
       ZERO-DD-TOTALS SECTION.
      ******************************************************************

           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 32
              MOVE 0 TO DD-NO(SUB) DD-BAL(SUB) DD-INT(SUB)
                        DD-OTH(SUB) DD-UNE(SUB)
           END-PERFORM.

       END-ZERO-DD-TOTALS.
           EXIT.

      ******************************************************************
       ZERO-NM-TOTALS SECTION.
      ******************************************************************
           MOVE 1 TO SUB.

           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 27
              MOVE 0 TO NM-NO(SUB) NM-BAL(SUB) NM-INT(SUB)
                        NM-OTH(SUB) NM-UNE(SUB)
           END-PERFORM.

       END-ZERO-NM-TOTALS.
           EXIT.

      ******************************************************************
       MOVE-NM-TO-DD SECTION.
      ******************************************************************
           MOVE 1 TO SUB.

           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 27
              MOVE NM-NO(SUB) TO DD-NO(SUB)
              MOVE NM-BAL(SUB) TO DD-BAL(SUB)
              MOVE NM-INT(SUB) TO DD-INT(SUB)
              MOVE NM-OTH(SUB) TO DD-OTH(SUB)
              MOVE NM-UNE(SUB) TO DD-UNE(SUB)
           END-PERFORM.

       END-MOVE-NM-TOTALS.
           EXIT.

      *****************************************************************
       CODE-SCAN SECTION.
      *****************************************************************

           MOVE SPACES TO CDSCAN-CODE.

           MOVE "WI/CDSCAN" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH CDSCAN-BUF EXIT-PATHNAME.
           CANCEL FORM-PROGX.
           IF CDSCAN-STATUS NOT = "00" OR CDSCAN-CODE = SPACES
              MOVE 9 TO IO-FG
              GO TO EXIT-CODE-SCAN.

           MOVE "S  A040ACTION." TO VDU.
           MOVE CDSCAN-CODE TO VDUBUF.
           PERFORM SCREENIO.
           MOVE CDSCAN-CODE TO VDUBUF.
           MOVE 0 TO IO-FG.

       EXIT-CODE-SCAN.
           EXIT.

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
              MOVE "SNNN020CL1." TO VDU
           ELSE
              MOVE "SNNN020CL2." TO VDU.
           MOVE LCSCAN-KEY TO VDUNUM0.
           PERFORM SCREENIO.
           MOVE LCSCAN-KEY TO VDUNUM0.
           MOVE 0 TO IO-FG.

       EXIT-CLASS-SCAN.
           EXIT.

      **********************************************************
       DEALER-SCAN SECTION.
      **********************************************************
           MOVE 0 TO DLSCAN-NUM.
           MOVE "WI/DLSCAN " TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH DLSCAN-BUF EXIT-PATHNAME.
           CANCEL FORM-PROGX.
           IF DLSCAN-STATUS NOT = "00" OR DLSCAN-NUM = " "
               MOVE 9 TO IO-FG
               GO TO EXIT-DEALER-SCAN.

           IF ( DEALER-FG = "DBEG" )
               MOVE "SNNN042DL1." TO VDU
           ELSE
               MOVE "SNNN042DL2." TO VDU.
           MOVE DLSCAN-DLNO TO VDUNUM2.
           PERFORM SCREENIO.
           MOVE DLSCAN-DLNO TO VDUNUM2.
           MOVE 0 TO IO-FG.

       EXIT-DEALER-SCAN.
           EXIT.
      ******************************************************************
      *
      *     C O P Y    L I B R A R Y    S E C T I O N S
      *
      ******************************************************************
       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/TIMEIO.CPY".
       COPY "LIBGB/AGEING.CPY".
       COPY "LIBLP/LPPDUE.CPY".
       COPY "LIBLP/LPUPWK.CPY".
       COPY "LIBGB/SPPDTH.CPY".
       COPY "LIBLP/LPCEPP.CPY".
       COPY "LIBLP/LPEARN.CPY".
       COPY "LIBLP/LPIBPC.CPY".
       COPY "LIBLP/LPSTAT.CPY".
       COPY "LIBLP/LPMDTE.CPY".
       COPY "LIBLP/LPXDTE.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/GRWORK.CPY".
       COPY "LIBGB/GRSERCH.CPY".
       COPY "LIBGB/EOSPOOL.CPY".
       COPY "LIBGB/GPENV.CPY".

      **************************************************
       FORM-RESET SECTION.
      **************************************************
           MOVE EXT-CONAME-CONAME TO LNAGA2-CONAME.
           MOVE EXT-CONAME-SYSDATE TO LNAGA2-SYSDATE.
           DISPLAY LNAGA2-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE LNAGA2-FORM-FIELDS TO WR-BUF.
           MOVE LNAGA2-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      **************************************************
       SETUP-LINK-INFO SECTION.
      **************************************************
           MOVE LNAGA2-HELP--FILE TO HELP-LINK-FILE.
           MOVE LNAGA2-HELP--DIR TO HELP-LINK-DIR.

      **************************************************
       VDU-CONVERT-FIELD SECTION.
      **************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBLP/LNAGA2_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ******************************************************************
       MISC-ROUTINES SECTION.
      ******************************************************************
       COPY "LIBGB/GETPR.CPY".
       COPY "LIBGB/MKTEMP.CPY".
       COPY "LIBGB/SYSTEM.CPY".
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/DACCESS.CPY".
       COPY "LIBGB/RMFILE.CPY".

      *----------------------------------------------------------------
       COMPUTE-REPO.

            IF ( LN-REPO-ONHAND )
               MOVE 1 TO RPSUB
            ELSE
            IF ( LN-REPO-SOLD   )
               MOVE 2 TO RPSUB
            ELSE
               MOVE ZEROES TO RPSUB.

      *----------------------------------------------------------------
       COMPUTE-PL.

            MOVE ZEROES TO PLSUB.

            IF ( LN-PLCD NOT = SPACES )
               IF ( LN-PLTYPE = "2" )
                  MOVE 2 TO PLSUB
               ELSE
               IF ( LN-PLTYPE = "3" )
                  MOVE 3 TO PLSUB
               ELSE
               IF ( LN-PLTYPE = "4" )
                  MOVE 4 TO PLSUB
               ELSE
               IF ( LN-PLTYPE = "5" )
                  MOVE 5 TO PLSUB
               ELSE
                  MOVE 1 TO PLSUB.

      *----------------------------------------------------------------
       ACCUM-AGEING-TOTALS.
           ADD T-NO(LEV AGE TYP) TO TT-NO(TYP).
           ADD T-BAL(LEV AGE TYP) TO TT-BAL(TYP).
           ADD T-INT(LEV AGE TYP) TO TT-INT(TYP).
           ADD T-OTH(LEV AGE TYP) TO TT-OTH(TYP).
           ADD T-UNE(LEV AGE TYP) TO TT-UNE(TYP).
           ADD T-ERN(LEV AGE TYP) TO TT-ERN(TYP).
           ADD T-IER(LEV AGE TYP) TO TT-IER(TYP).

      *----------------------------------------------------------------
       INVALID-RANGE.
           MOVE "INVALID RANGE" TO MESS.
           PERFORM SEND-MESS.

      *----------------------------------------------------------------
       PRINT-DUE-TOTALS.
           MOVE "      TOTAL:" TO S-NAME.
           MOVE TT-NO(1) TO S-NO.
           MOVE TT-BAL(1) TO S-BAL.
           MOVE TT-INT(1) TO S-INT.
           MOVE TT-OTH(1) TO S-OTH.
           MOVE TT-UNE(1) TO S-UNE.
           PERFORM WRITE-DETAIL-LINE.

      *----------------------------------------------------------------
       ACCUM-BYDUEDATE-TOTALS.
           ADD DD-NO(LEV) TO TT-NO(1).
           ADD DD-BAL(LEV) TO TT-BAL(1).
           ADD DD-INT(LEV) TO TT-INT(1).
           ADD DD-OTH(LEV) TO TT-OTH(1).
           ADD DD-UNE(LEV) TO TT-UNE(1).

      *----------------------------------------------------------------
       PRINT-SUM-TOTALS.
           IF TYP = 3
              MOVE "  PAID-AHEAD" TO S-NAME
           ELSE
           IF TYP = 4
              MOVE " TOTAL REPO:" TO S-NAME
           ELSE
           IF TYP = 5
              MOVE "  TOTAL P&L:" TO S-NAME
           ELSE
              MOVE "      TOTAL:" TO S-NAME.

           MOVE TT-NO(TYP) TO S-NO.
           MOVE TT-BAL(TYP) TO S-BAL.
           MOVE TT-INT(TYP) TO S-INT.
           MOVE TT-OTH(TYP) TO S-OTH.
           MOVE TT-UNE(TYP) TO S-UNE.
           MOVE TT-ERN(TYP) TO S-ERN.
      *     MOVE TT-IER(TYP) TO S-IER.
           PERFORM WRITE-DETAIL-LINE.

      *----------------------------------------------------------------
       RESET-TOT-PAGE.
           MOVE ZEROS TO TOTAL-TBL(3).
           MOVE "Y" TO SUMMARY-FG.
           MOVE EXT-CONAME-RPT-LINES TO LN-COUNT.
           MOVE 1 TO LN-FEED.

      ******************************************************************
       IO-ROUTINE SECTION.
      ******************************************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBLP/LPSPGS_SQL.CPY".
       COPY "LIBLP/LPLTPGS_SQL.CPY".
       COPY "LIBLP/LPLNGS_SQL.CPY".
       COPY "LIBLP/LPLCGS_SQL.CPY".
       COPY "LIBLP/LPDLGS_SQL.CPY".
       COPY "LIBLP/LPBWGS_SQL.CPY".
       COPY "LIBGB/GBGRGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBGB/GBGR1RN.CPY".
       COPY "LIBGB/GBPRINT.CPY".
       COPY "LIBLP/LPLN1RN.CPY".
       COPY "LIBLP/LPLN3RN.CPY".
       COPY "LIBLP/LPBW1RN.CPY".
       COPY "LIBLP/LPLC1RN.CPY".
       COPY "LIBLP/LPSP1RN.CPY".
       COPY "LIBLP/LPDL1RN.CPY".
       COPY "LIBLP/LPLTP1RN.CPY".
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

           MOVE MKTEMP-BUF TO ACCESS-BUF.
           PERFORM REMOVE-WORKFILE.

           IF NO-RECORDS-FOUND
              MOVE "NO RECORDS FOUND FOR SELECTED RANGE" TO MESS
              PERFORM SEND-MESS.

      *----------------------------------------------------------------
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
              MOVE "SP/SCMENU" TO FORM-NAM
              MOVE FORM-PATH TO FORM-PATHNAME.

      *----------------------------------------------------------------
       STOP-RUN.
           IF EXT-ROUTE-BUF = "00" OR "40" OR "50"
              DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN
              DESTROY LNAGA2-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.
           EXIT PROGRAM.
