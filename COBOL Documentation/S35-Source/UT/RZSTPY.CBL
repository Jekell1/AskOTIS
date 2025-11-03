       IDENTIFICATION DIVISION.
       PROGRAM-ID.      LASTPY.
       DATE-WRITTEN.    071701.
      ******************************************************************
      *                      (LP)
      *    DESCRIPTION:  DELINQUENT AT LAST PAYMENT REPORT, WORLD PR# 270
      *
      *                  USES GROUP LOGIC.
      *                  LOOPS THRU LNFILE KEY 3 AND WRITES WKFILE
      *                  COMPUTES PAID-THRU-CALCULATION, TIMALL (ELAPSED-DAYS)
      *                  COMPUTES CONTRIACTUAL AND RECENCY
      *                  END OF BRANCH, PRINTS MULTIPLE TOTALS BASED UPON FLAGS
      *
      *     NOTE:
      *      REPORT ORDER       "1" OR "Y" = ACCOUNT NUMBER
      *                         "2" OR "N" = AGEING
      *                         "3"        = ALPHA (LNAME,FNAME)
      *                         "4"        = RENEWABLE 1ST, THEN HIGHEST
      *                                      CASH AVAILABLE TO LOWEST
      *                                      CURRENTLY ONLY AVAILABLE
      *                                      IF I-AM-WORLD
      *                          NOTE- 4 USED TO BE ALPHA FOR ALL BRANCHES
      *                                BUT THIS WENT AWAY IN A60
      *                         "5"        = RECENCY AGEING (A50)
      *                         "6"        = AGEING, ALPHA(LNAME,FNAME)
      * REV:
      *  BLV 011120 ADD FLAG TO PRINT ONLY ACCOUNTS WHOSE LAST PMT WAS WITHIN
      *             7 DAYS OF THEIR NEXT DUE DATE, WORLD PR# 284
      *  BLV 011126 FIX TEST OF NEXT DUE FLAG, WORLD PR# 284
      *  BAH 020115 CHANGED TO EXIT TO LPR2MU, SECOND REPORT MENU
      *  BLV 020712 FIXED BLOWING OUT WITH MEMORY ACCESS ERROR; BAD SUBSCRIPT
      *             BECAUSE COMPUTE-RECENCY WASN'T GETTING PERFORMED (NOTE,
      *             THIS REPORT WORKED FOR WORLD BECAUSE BANKRULE D COMPUTES
      *             RECENCY WHEN IT PERFORMS CONTRACTUAL)
      *  JTG 030630 ACCOUNTS BYPASS LATE CHARGE ASSESS ROUTINE WHEN
      *             SP-LCFRMLA = ' ' BUT ACCOUNT IS SETUP FOR DEALER LATE
      *             CHARGE FORMULA                               MIDATL #J097
      *  BAH 040225 REMOVED D-BRNO FROM DETAIL LINE, ONLY HAD ROOM FOR 2 BYTES
      *             THE BRNO IS IN THE PAGE HEADING ANYWAYS
      *  BLF 041004 CHANGED TO SEND CLASS RANGE TO SUMMBY WHEN IT'S CALLED, SO
      *             IT WILL RETURN TOTALS FOR COMPUTING DELINQUENCY PERCENTS
      *             BASED ON CLASS RANGE, NOT TOTAL PORTFOLIO, ACE PR# 2374
      *  BAH 050118 ADDED CHECK OF BW-CONFIDENTIAL, PR# 2343
      *  BAH 160125 SPLIT OUT LTFILE, PD0003
      *
      * KEC 2016.0218 {PD#00003} PARADATA <A30> [JAY CISZEWSKI]
      *         FIXED KEYS & RECS FOR FD-REC.
      *         CHANGED FMNO/FORMNO/NEXTFM/SETFM FROM 3 TO 4 BYTES.
      * KEC 2017.0522 {PR#00000} PARADATA <A30> [JAY CISZEWSKI]
      *          CHANGED THE USE OF PDTH-WK-MO/DA/YR
      *                       TO BE PDTH-WK-MM/DD/YY
      * BAH 170605 ACTIVATE 8 DIGIT DATES, PD0006
      * BAH 181128 GLOBAL LMFILE, L2G
      * BAH 181210 GLOBAL LNFILE, L2G
      * KEC 190219 A32 FFR - REMOVED REDEFINE FOR LN-LC-CODEX; UNUSED
      * BAH 20190926 REMOVED ACCESS-CALL LNFILE
      * BAH 20200827 REMOVED RESET ON BR1 
      * BAH 20220818 REMOVED GLOBRD, READ GB-FILE INSTEAD
      * BAH 20220823 HARD CODED START
      ******************************************************************
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
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       FD  WK-FILE
           LABEL RECORDS ARE STANDARD.
       01  WK-REC.
           03  WK-KEY.
               05  WK-BRNO           PIC 9(4).
               05  WK-COLCTID        PIC X(3).
               05  WK-ACTION-CD      PIC X(4).
               05  WK-AGE            PIC S9(5).
               05  WK-BALDUE         PIC S9(9)V99.
               05  WK-NAME           PIC X(33).
               05  WK-ACCTNO         PIC 9(8).
           03  WK-CSUB               PIC S9(6)  COMP.
           03  WK-CPOT30             PIC X.
           03  WK-RDSUB              PIC S9(6)  COMP.
           03  WK-RDPOT30            PIC X.
           03  WK-PSUB               PIC S9(6)  COMP.
      ******************************************************************
      *
      *     W O R K I N G - S T O R A G E   S E C T I O N
      *
      ******************************************************************
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)LP/LASTPY.CBL S35 06/16/23-09:36:23 >".

      ******************************************************************
      *
      *     F I L E   S P E C S
      *
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
       COPY "LIBGB/GB01GB.CPY".
       COPY "LIBGB/GB01GB_SQL.CPY".
       COPY "LIBGB/GBWSGB.CPY".
       COPY "LIBGB/GB01GR.CPY".
       COPY "LIBGB/GB01GR_SQL.CPY".
       COPY "LIBGB/GBWSGR.CPY".
       COPY "LIBLP/LP01SP.CPY".
       COPY "LIBLP/LP01SPA.CPY".
       COPY "LIBLP/LP01SPB.CPY".
       COPY "LIBLP/LP01SPC.CPY".
       COPY "LIBLP/LP01SP_SQL.CPY".
       COPY "LIBLP/LPWSSP.CPY".
       COPY "LIBLP/LP01LP.CPY".
       COPY "LIBLP/LP01LP_SQL.CPY".
       COPY "LIBLP/LPWSLP.CPY".
       COPY "LIBLP/LP01LTI.CPY".
       COPY "LIBLP/LP01LTI_SQL.CPY".
       COPY "LIBLP/LPWSLTI.CPY".
       COPY "LIBLP/LP01LTP.CPY".
       COPY "LIBLP/LP01LTP_SQL.CPY".
       COPY "LIBLP/LPWSLTP.CPY".
       COPY "LIBLP/LP01LM.CPY".
       COPY "LIBLP/LP01LM_SQL.CPY".
       COPY "LIBLP/LPWSLM.CPY".
       COPY "LIBLP/LP01NO.CPY".
       COPY "LIBLP/LP01CRWKBY.CPY".
      *----------------------------------------------------------------
       01  WK-IFN        PIC XX       VALUE "WK".

      ******************************************************************
      *
      *   LINE WORKERS
      *
      ******************************************************************
       01  LN-WORKERS.
           03  LN-FEED          PIC 99       VALUE 0.
           03  LN-COUNT         PIC 99       VALUE 61.
           03  LN-WK            PIC 99       VALUE 0.
       01  PAGE-NUMBER          PIC 9(4)     VALUE 0.

       01  TOTAL-FG             PIC X        VALUE "N".
       01  CLASS-FG             PIC X(3)     VALUE "   ".
           88  CLASS-BEG                     VALUE "BEG".
           88  CLASS-END                     VALUE "END".

      ******************************************************************
      *
      *     S C R E E N   F I E L D S
      *
      ******************************************************************
       COPY "LIBGB/EOBUF_EXT.CPY".
      *-----------------------------------------------------------------
           03  AGE-DATE               PIC 9(8).
           03  BEG-BRANCH             PIC 9(4).
           03  END-BRANCH             PIC 9(4).
           03  BEG-CLASS              PIC 9(3).
           03  END-CLASS              PIC 9(3).
           03  BRSEQ                  PIC X.
           03  REPORT-TYPE            PIC X.
           03  DEL-FLAG               PIC X.
           03  COLLECTOR-FG           PIC X.
           03  GROUP-FLAG             PIC X.
           03  ENT-GROUP              PIC X(4).
           03  TOTALS-FG              PIC X.
               88  TOTALS-FG-VALID    VALUE  "Y" "N" "T" "R".
               88  TOTALS-YES         VALUE  "Y" "T".
               88  RANGES-YES         VALUE  "Y" "R".
           03  NXT-DUE-FLAG           PIC X.

       01  VERSION-CONTROL     PIC X    VALUE "A".

       01  SV-BEG-BR           PIC 9999 VALUE 9999.
       01  SV-END-BR           PIC 9999 VALUE 9999.
       01  SV-GROUP-FLAG       PIC X     VALUE SPACES.
       01  SV-ENT-GROUP        PIC X(4)  VALUE SPACES.
      *-----------------------------------------------------------------

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
           03  FILLER           PIC X(37)    VALUE " ".
           03  FILLER           PIC X(5)     VALUE "PAGE ".
           03  H-PAGE-NO        PIC ZZZ9.

      *-----------------------------------------------------------------
       01  HEAD2.
           03  FILLER           PIC X(10)    VALUE "USER ID:".
           03  H-USERID         PIC X(10).
           03  FILLER           PIC X(26)    VALUE " ".
           03  H-TITLE          PIC X(35)    VALUE
           " DELINQUENT AT LAST PAYMENT REPORT ".
           03  FILLER           PIC X(6)     VALUE " FOR: ".
           03  H-AGEDATE        PIC X(10).

      *-----------------------------------------------------------------
       01  HEAD3.
           03  FILLER      PIC X(10)    VALUE "BRANCH   :".
           03  FILLER      PIC XX       VALUE " ".
           03  H-BRNO      PIC X(4).
           03  FILLER      PIC X        VALUE " ".
           03  H-BRDESC    PIC X(20).

      *-----------------------------------------------------------------
       01  HEAD4X.
           03  FILLER      PIC X(10)    VALUE "COLLECTOR:".
           03  FILLER      PIC XX       VALUE " ".
           03  H-COLCTID   PIC X(3).
           03  FILLER      PIC X        VALUE " ".
           03  H-COLDESC   PIC X(20).

      *-----------------------------------------------------------------
       01  HEAD5.
           03  FILLER      PIC X(52)    VALUE " ".
           03  FILLER      PIC X(47)    VALUE
           "LOAN-AMT 1ST-PMNT    PROFILES   PDTH PROMISED ".
           03  FILLER      PIC X(20)    VALUE SPACES.
           03  FILLER      PIC X(13)  VALUE "BALANCE   CON".

      *-----------------------------------------------------------------
       01  HEAD6.
           03  FILLER      PIC X(53)    VALUE
           "ACCOUNT #        LOANDATE  DEALER BR SC CC  PC TRM   ".
           03  FILLER      PIC X(46)    VALUE
           " LC-AMT MATURITY               LCPD FORMDATE  ".
           03  FILLER      PIC X(34)    VALUE
           "LAST-DUE  NEXT-DUE TODAY-DUE  REC".

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    COMPOSITION OF THIS LINE SHOULD MATCH LONPF1
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

      *-----------------------------------------------------------------
       01  HEAD7.
           03  FILLER      PIC X(50)    VALUE
           "-DATE--CD-REF-----AMOUNT--OTHER--INT/LC-----PRIN--".
           03  FILLER      PIC X(30)    VALUE
           "PDTH--OTHER--INT/LC-----PRIN--".

      * SINGLE LINE HEADINGS
       01  HEAD8.
           03  FILLER     PIC X(44)    VALUE
           "ACCOUNT #       DUE  NAME                   ".
           03  HEAD8-PROMPT PIC X(9)   VALUE "DEALER   ".
           03  FILLER     PIC X(30)    VALUE
           "LOAN      CURRENT    LAST     ".
           03  FILLER     PIC X(49)    VALUE
           "PROM      AMOUNT      TODAY CONFID PDTH  CON  REC".
       01  HEAD9.
           03  FILLER     PIC X(41)    VALUE
           "                DAY                      ".
           03  HEAD9-PROMPT PIC X(12)  VALUE "   NUMBER   ".
           03  FILLER     PIC X(41)    VALUE
           "DATE      BALANCE   PAYDATE   DATE       ".
           03  FILLER     PIC X(22)    VALUE
           "PAID        DUE  ALLOT".

      **********************
      *   MISC WORKERS     *
      **********************
       01  SUB                  PIC 9(6)     COMP VALUE 0.
       01  HOLD-TODAY-DATE      PIC 9(8)     COMP.
       01  LN-ACCUM-BAL         PIC S9(7)V99 COMP VALUE 0.
       01  FIRST-TIME           PIC X        VALUE "Y".
       01  NEW-BRANCH           PIC X        VALUE "N".
       01  RECORDS-FOUND        PIC X        VALUE " ".
           88  NO-RECORDS-FOUND              VALUE "N".
       01  MESS-LIST            PIC X(5)     VALUE "MESS.".
       01  WK-DISPLAY-MESS.
           03  FILLER           PIC X(22)    VALUE
           "BUILDING WORK FILE....".
           03  DISPLAY-MESS-BR  PIC 9(4).
           03  FILLER           PIC X(10).
       01  RUN-DISPLAY-MESS.
           03  FILLER           PIC X(24)    VALUE
           "REPORT IN PROGRESS......".
           03  RUN-MESS-BR      PIC 9(4).
           03  FILLER           PIC X(10).
       01  NAME-WORKER          PIC X(34).
       01  FIRST-NAME           PIC X(16).
       01  DASH-LINE            PIC X(132)    VALUE ALL "-".

       01  SAVE-CO-PHONE        PIC 9(10) COMP VALUE 0.
       01  SAVE-CO-EMPPHONE     PIC 9(10) COMP VALUE 0.
       01  SAVE-BW-FIELDS.
           03  SAVE-BW-ADR1     PIC X(24).
           03  SAVE-BW-ADR2     PIC X(24).
           03  SAVE-BW-CITY     PIC X(15).
           03  SAVE-BW-STATE    PIC XX.
           03  SAVE-BW-ZIP      PIC X(10).
           03  SAVE-BW-PHONE    PIC 9(10)     COMP.
           03  SAVE-BW-LOANS    PIC 99        COMP.
           03  SAVE-BW-EMPLOYER PIC X(24).
           03  SAVE-BW-EMPPHONE PIC 9(10)     COMP.
           03  SAVE-BW-EMPLPOS  PIC X(24).
           03  SAVE-BW-EMPEXT   PIC 9(4).

       01  LOAN-MEMO-BUF.
           03  LOAN-DATE-MEMO   OCCURS 4.
      * LEAVE THIS 6 FOR PRINTING, ULESS YOU CHANGE DETAIL LINE
               07  LOAN-MEMO-DT PIC 9(6)      COMP.
               07  LOAN-MEMO    PIC X(60).
       01  MEMO-SUB             PIC 9.

       01  LOAN-PMT-BUF.
           03  LOAN-PMT         PIC X(80)     OCCURS 4.
       01  PMT-SUB              PIC 9.

       01  PN-PHONE             PIC 9(10).
       01  PN-PHONEX REDEFINES PN-PHONE.
           03  PN-123           PIC 9(3).
           03  PN-456           PIC 9(3).
           03  PN-7890          PIC 9(4).
       01  PA-PHONE.
           03  FILLER           PIC X  VALUE "(".
           03  PA-123           PIC 9(3).
           03  FILLER           PIC XX VALUE ") ".
           03  PA-456           PIC 9(3).
           03  FILLER           PIC X  VALUE "-".
           03  PA-7890          PIC 9(4).

       01  HOLD-BRNO     PIC 9(4)  VALUE 0.
       01  HOLD-GROUP    PIC X(4)  VALUE SPACES.

       01  OLD-COLCTID-CD       PIC X(3)     VALUE "+++".
       01  PROFILE-X.
           03  PROFILE-9        PIC 9(12).
       01  NUM-BW               PIC 9(9).
       01  NUM-BW-WK            REDEFINES NUM-BW.
           03  NUM-BW1          PIC 999.
           03  NUM-BW2          PIC 99.
           03  NUM-BW3          PIC 9999.
       01  ALP-BW.
           03  ALP-BW1          PIC 999.
           03  ALP-HYPHEN1      PIC X.
           03  ALP-BW2          PIC 99.
           03  ALP-HYPHEN2      PIC X.
           03  ALP-BW3          PIC 9999.

       01  RENEWABLE-FG         PIC X.
       01  WK-RENEWABLE-PAYS    PIC 9(4).
       01  WK-RENEWABLE-DAYS    PIC 9(4).

      *********************************************
      *               T O T A L S                 *
      *********************************************
      *                                           *
      *      CLASS-TBL:                           *
      *        LTYP(1) = DIRECT LOAN              *
      *            (2) = SALES FINANCE            *
      *                                           *
      *      TOTAL-TBL:                           *
      *         LEV(1) = BY COLLECTOR             *
      *            (2) = BY COMPANY (GRAND TOTALS)*
      *            (3) = BY BRANCH                *
      *                                           *
      *      T-AGETBL:                            *
      *         AGE(1)  =    30 DAYS OLD          *
      *            (2)  =    60 DAYS OLD          *
      *            (3)  =    90 DAYS OLD          *
      *            (4)  =   120 DAYS OLD          *
      *            (5)  =   150 DAYS OLD          *
      *            (6)  =   180 DAYS OLD          *
      *            (7)  =   210 +                 *
      *            (8)  =   <M> MISPAYS           *
      *            (9)  =   <P> POTS              *
      *           (10)  =   CURRENT               *
      *                                           *
      *      T-TYPETBL:                           *
      *         TYP(1)  = CONTRACTUAL             *
      *            (2)  = RECENCY                 *
      *            (3)  = PAID AHEAD              *
      *                   CONTRACTUALLY CURRENT   *
      *                    AND DELINQ-RECENCY     *
      *                                           *
      *********************************************
       01  TOTAL-TABLE             VALUE ZEROS.
           03  CLASS-TBL           OCCURS 2 TIMES.
              05  TOTAL-TBL           OCCURS 3 TIMES.
                  07  T-AGETBL        OCCURS 10 TIMES.
                      09  T-TYPETBL   OCCURS 3 TIMES.
                          11  T-NO    PIC S9(5)     COMP.
                          11  T-BAL   PIC S9(9)V99  COMP.
                          11  T-INT   PIC S9(7)V99  COMP.
                          11  T-OTH   PIC S9(7)V99  COMP.
                          11  T-TDUE  PIC S9(9)V99  COMP.
                          11  T-UNE   PIC S9(7)V99  COMP.
                          11  T-ERN   PIC S9(7)V99  COMP.
                          11  T-IER   PIC S9(7)V99  COMP.

       01  TT-TOTALS              VALUE ZEROS.
           03  TT-CLASS           OCCURS 2 TIMES.
               05  TT-TYPETBL         OCCURS 3 TIMES.
                   07  TT-NO          PIC S9(5)     COMP.
                   07  TT-BAL         PIC S9(9)V99  COMP.
                   07  TT-INT         PIC S9(7)V99  COMP.
                   07  TT-OTH         PIC S9(7)V99  COMP.
                   07  TT-TDUE        PIC S9(9)V99  COMP.
                   07  TT-UNE         PIC S9(7)V99  COMP.
                   07  TT-ERN         PIC S9(7)V99  COMP.
                   07  TT-IER         PIC S9(7)V99  COMP.

      * PORTFOLIO-TOTALS:
      *         LEVEL (1) = DIRECT LOAN TOTALS.
      *               (2) = SALES FINANCE TOTALS.
       01  PORTFOLIO-TOTALS  OCCURS 2 TIMES.
           03  PORT-NO        PIC S9(8)      COMP.
           03  PORT-AMT       PIC S9(13)V99  COMP.

      * ACCUMULATED PORTFOLIO TOTALS USED IN GRAND TOTALS.
      *
       01  ACCUM-DL-NO            PIC S9(8)     COMP.
       01  ACCUM-DL-AMT           PIC S9(13)V99 COMP.
       01  ACCUM-SF-NO            PIC S9(8)     COMP.
       01  ACCUM-SF-AMT           PIC S9(13)V99 COMP.

       01  LTYP                   PIC 9(6)  COMP.
       01  LEV                    PIC 9(6)  COMP.
       01  AGE                    PIC 9(6)  COMP.
       01  P-AGE                  PIC 9(6)  COMP.
       01  M-AGE                  PIC 9(6)  COMP.
       01  TYP                    PIC 9(6)  COMP.

       01  AGE-DESC-TABLE.
           03  FILLER             PIC X(12) VALUE "    30 DAYS:".
           03  FILLER             PIC X(12) VALUE "    60 DAYS:".
           03  FILLER             PIC X(12) VALUE "    90 DAYS:".
           03  FILLER             PIC X(12) VALUE "   120 DAYS:".
           03  FILLER             PIC X(12) VALUE "   150 DAYS:".
           03  FILLER             PIC X(12) VALUE "   180 DAYS:".
           03  FILLER             PIC X(12) VALUE "   210 +   :".
           03  FILLER             PIC X(12) VALUE "   MIS-PAYS:".
           03  FILLER             PIC X(12) VALUE "   POT 30'S:".
           03  FILLER             PIC X(12) VALUE "   CURRENT :".
       01  AGE-DESC-TBL           REDEFINES AGE-DESC-TABLE.
           03  AGE-DESC           OCCURS 10 TIMES
                                  PIC X(12).

       01  DETAIL-LINE          PIC X(132) VALUE SPACES.

       01  D-LINE1 REDEFINES DETAIL-LINE.
           03  D-LOAN-NUM  PIC X(16).
           03  FILLER      PIC X.
           03  D-LOANDATE  PIC 99/99/99.
           03  FILLER      PIC X.
           03  D-DLNO      PIC 9(4).99     BLANK ZERO.
           03  FILLER      PIC X(4).
           03  D-SRCD      PIC XX.
           03  FILLER      PIC X.
           03  D-COLLCD    PIC XX.
           03  FILLER      PIC X.
           03  D-PURCD     PIC XXX.
           03  FILLER      PIC X.
           03  D-ORGTERM   PIC ZZ9.
           03  FILLER      PIC X.
           03  D-LNAMT     PIC Z(5)9.99   BLANK ZERO.
           03  FILLER      PIC X.
           03  D-1STPYDATE PIC 99/99/99    BLANK ZERO.
           03  FILLER      PIC X.
           03  FILLER      PIC X.
           03  D-CONPROF   PIC X(12).
           03  FILLER      PIC X.
           03  D-PDTH      PIC 9999.
           03  FILLER      PIC X.
           03  D-PROMISED  PIC 99/99/99    BLANK ZERO.
           03  FILLER      PIC XX.
           03  D-LASTDATE  PIC 99/99/99    BLANK ZERO.
           03  FILLER      PIC XX.
           03  D-NEXTDATE  PIC 99/99/99   BLANK ZERO.
           03  FILLER      PIC X.
           03  D-BALANCE   PIC Z(5)9.99-  BLANK ZERO.
           03  D-CONX.
               05  D-CON   PIC 9999.
           03  FILLER      PIC XXX.

       01  D-LINE2 REDEFINES DETAIL-LINE.
           03  FILLER          PIC X.
           03  D-MAKERCD       PIC X.
           03  FILLER          PIC X.
           03  D-SSNO          PIC X(11).
           03  FILLER          PIC X.
           03  D-NAME          PIC X(27).
           03  FILLER          PIC X(11).
           03  D-LCAMT         PIC Z(3)9.99    BLANK ZERO.
           03  FILLER          PIC X.
           03  D-MATURDATE     PIC 99/99/99    BLANK ZERO.
           03  FILLER          PIC X.
           03  FILLER          PIC X.
           03  D-RECPROF       PIC X(12).
           03  FILLER          PIC X.
           03  D-LCPD.
               05  D-LCPD-MO   PIC 99.
               05  D-LCPD-DAYR PIC 99.
           03  FILLER          PIC X.
           03  D-FORMDATE      PIC 99/99/99    BLANK ZERO.
           03  FILLER          PIC X.
           03  D-LASTAMT       PIC Z(5)9.99-   BLANK ZERO.
           03  D-NEXTAMT       PIC Z(5)9.99-   BLANK ZERO.
           03  D-TODAYDUE      PIC Z(5)9.99-   BLANK ZERO.
           03  D-RECX.
               05  D-REC       PIC ZZZ9.

       01  D-LINE2A REDEFINES DETAIL-LINE.
           03  D-COMAKER-PHONE PIC X(11).
           03  D-CO-WORK   PIC X.
           03  D-CO-PHONE1 PIC X(14).
           03  FILLER      PIC X.
           03  D-CO-HOME   PIC X.
           03  D-CO-PHONE2 PIC X(14).

       01  D-LINE3 REDEFINES DETAIL-LINE.
           03  FILLER      PIC X(42).
           03  D-MSTAG     PIC X(11).
           03  FILLER      PIC X.
           03  D-WORK      PIC X.
           03  D-PHONE1    PIC X(14).
           03  FILLER      PIC X.
           03  D-WORKEXT   PIC X.
           03  D-PHONE1EXT PIC Z(4).
           03  FILLER      PIC XX.
           03  D-HOME      PIC X.
           03  D-PHONE2    PIC X(14).
           03  FILLER      PIC X(2).
           03  D-N         PIC X.
           03  D-FORMNO    PIC 9999.
           03  FILLER      PIC X.
           03  D-LDUELC    PIC Z(3)9.99  BLANK ZERO.
           03  FILLER      PIC XX.
           03  D-NXDUELC    PIC Z(4)9.99  BLANK ZERO.
           03  FILLER      PIC XX.
           03  D-TDUELC   PIC Z(4)9.99  BLANK ZERO.
           03  FILLER      PIC X.

       01  D-LINE4 REDEFINES DETAIL-LINE.
           03  FILLER      PIC X(15).
           03  D-ADDR      PIC X(30).
           03  FILLER      PIC X.
           03  D-EMPLOYER.
               05  D-LOANSX    PIC X(8).
               05  D-LOAN-NO.
                   07  FILLER  PIC X.
                   07  D-LOANS PIC Z9.
                   07  FILLER  PIC X(5).
               05  D-PMTX REDEFINES D-LOAN-NO.
                   07  D-PMT   PIC Z(4)9.99   BLANK ZERO.
           03  FILLER          PIC X.
           03  D-MEMOX.
               05  D-MEMO-DT   PIC 9(6).
               05  FILLER      PIC X.
               05  D-MEMO      PIC X(60).
           03  FILLER          PIC X(2).

       01  D-LINE5 REDEFINES DETAIL-LINE.
           03  D-SLOAN-NUM    PIC X(16).
           03  FILLER         PIC X.
           03  D-SDUEDAY      PIC 99.
           03  FILLER         PIC XX.
           03  D-SNAME        PIC X(20).
           03  FILLER         PIC X.
           03  D-SDLNO        PIC ZZZZ9.99     BLANK ZERO.
           03  FILLER         PIC X.
           03  D-SLNDATE      PIC 99/99/99     BLANK ZERO.
           03  FILLER         PIC X.
           03  D-SBALANCE     PIC Z(6)9.99-    BLANK ZERO.
           03  FILLER         PIC X.
           03  D-SLASTDATE    PIC 99/99/99     BLANK ZERO.
           03  FILLER         PIC X.
           03  D-SPROMDATE    PIC 99/99/99     BLANK ZERO.
           03  D-STOTAMTPD    PIC Z(6)9.99-    BLANK ZERO.
           03  FILLER         PIC X.
           03  D-STODAYDUE    PIC Z(5)9.99-    BLANK ZERO.
           03  FILLER         PIC XX.
           03  D-SFLAG        PIC X(3).
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           03  FILLER         PIC X(2).
           03  D-SPDTH        PIC 9(4).
           03  FILLER         PIC X(1).
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           03  D-SCONX.
               05  D-SCON     PIC ZZZ9         BLANK ZERO.
           03  FILLER         PIC X.
           03  D-SRECX.
               05  D-SREC     PIC ZZZ9         BLANK ZERO.


       01  PMT-LINE REDEFINES DETAIL-LINE.
           03  FILLER      PIC X(52).
      *    COMPOSITION OF THIS LINE SHOULD MATCH LONPF1
           03  D-PMT-LINE.
               05  L-010      PIC 9(6).
               05  FILLER     PIC X.
               05  L-020      PIC XX.
               05  FILLER     PIC X.
               05  L-030      PIC X(5).
               05  L-040      PIC Z(5).99- BLANK ZERO.
               05  L-040RATE  REDEFINES L-040
                              PIC ZZZZ.ZZZ-.
               05  L-050      PIC Z(4).99- BLANK ZERO.
               05  L-060      PIC Z(4).99- BLANK ZERO.
               05  L-070      PIC Z(5).99- BLANK ZERO.
               05  L-080-MO   PIC 99.
               05  L-080-DAYR PIC 99.
               05  L-090      PIC Z(5).99- BLANK ZERO.
               05  L-100      PIC Z(4).99- BLANK ZERO.
               05  L-110      PIC Z(5)9.99.
               05  L-120      PIC X.

       01  SUMMARY-LINE1 REDEFINES DETAIL-LINE.
           03  S-SUMNAME           PIC X(20).
           03  FILLER              PIC X(112).
       01  SUMMARY-LINE2 REDEFINES DETAIL-LINE.
           03  FILLER              PIC X(53).
           03  S-AGETYPE           PIC X(79).
       01  SUMMARY-LINE2B REDEFINES DETAIL-LINE.
           03  FILLER           PIC X(38).
           03  S-DESC           PIC X(12).
           03  FILLER           PIC X(44).
           03  S-DESC2          PIC X(14).
           03  FILLER           PIC X(7).
       01  SUMMARY-LINE3 REDEFINES DETAIL-LINE.
           03  S-NAME              PIC X(12).
           03  SUMMARY-LINE3X      OCCURS 2 TIMES.
               05  FILLER              PIC X(8).
               05  S-NO                PIC ZZZZ9-        BLANK ZERO.
               05  S-NO-P              PIC ZZ9.99-       BLANK ZERO.
               05  S-BAL               PIC ZZZZZZZ9.99-  BLANK ZERO.
               05  S-BAL-P             PIC ZZ9.99-       BLANK ZERO.
               05  S-TDUE              PIC ZZZZZZZ9.99-  BLANK ZERO.
               05  S-TDUE-P            PIC ZZ9.99-       BLANK ZERO.

       01  RANGE-LN REDEFINES DETAIL-LINE.
           03  RANGE-LINE        PIC X(20).
           03  RANGE-SELECTION   PIC X(112).

       01  SUMMARY1.
           03  FILLER           PIC X(23)  VALUE SPACES.
           03  FILLER           PIC X(45)  VALUE
           "NO----%       BAL DUE----%      TDAY DUE----%".
           03  FILLER           PIC X(14).
           03  FILLER           PIC X(45)  VALUE
           "NO----%       BAL DUE----%      TDAY DUE----%".
           03  FILLER           PIC X(7).

       COPY "LIBGB/PASSWDW.CPY".
       COPY "LIBWI/PFSWINDOWSW.CPY".
       COPY "LIBWI/TSTINTW.CPY".
       COPY "LIBWI/GRSCANW.CPY".
       COPY "LIBWI/LCSCANW.CPY".
       COPY "LIBWI/DLSCANW.CPY".

       COPY "LIBLP/LPAPRSW.CPY".
       COPY "LIBLP/LPITRMW.CPY".
       COPY "LIBLP/LPAPRZW.CPY".
       COPY "LIBLP/LPUPERW.CPY".
       COPY "LIBGB/GRWORKW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBLP/SETCTBLW.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBGB/SPPDTHW.CPY".
       COPY "LIBGB/BRSECUREW.CPY".
       COPY "LIBLP/LPAMTSW.CPY".
       COPY "LIBGB/AGEINGW.CPY".
       COPY "LIBLP/LPUPWKW.CPY".
       COPY "LIBLP/LPAVCRW.CPY".
       COPY "LIBLP/LPLCASW.CPY".
       COPY "LIBLP/LPLCHGW.CPY".
       COPY "LIBLP/LPSCHDW.CPY".
       COPY "LIBLP/LPLCAPW.CPY".
       COPY "LIBLP/LPMDTEW.CPY".
       COPY "LIBLP/LPXDTEW.CPY".
       COPY "LIBLP/LPCEPPW.CPY".
       COPY "LIBLP/LPPOFFW.CPY".
       COPY "LIBLP/ADDONSPRW.CPY".
       COPY "LIBCL/CRNOR2W.CPY".
       COPY "LIBLP/DEFPOLW.CPY".
       COPY "LIBLP/LPRATEW.CPY".
       COPY "LIBLP/LPIBPCW.CPY".
       COPY "LIBLP/LPPDUEW.CPY".
       COPY "LIBLP/LPCDELW.CPY".
       COPY "LIBGB/REBATEW.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBLP/LPSTATW.CPY".
       COPY "LIBGB/MKTEMPW_CMD.CPY".
       COPY "LIBGB/GETPRW.CPY".
       COPY "LIBCL/SUMMBYW.CPY".
       COPY "LIBGB/RBACTW.CPY".
       COPY "LIBGB/GPENVW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBLP/LASTPY_DEF.CPY".
       COPY "LIBLP/LASTPY_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

      *****************************************************************
      *
      *     P R O G R A M   C O N T R O L
      *
      *****************************************************************
       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBLP/LASTPY_SCN.CPY".

       PROCEDURE DIVISION
           USING FORM-PATHNAME EXIT-PATHNAME.
       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
               PR-FILE WK-FILE.
       COPY "LIBGB/DECLRP.CPY".
           PERFORM CLOSE-SP1-FILE.
           PERFORM CLOSE-LTP1-FILE.
           PERFORM CLOSE-LTI1-FILE.
           PERFORM CLOSE-LP1-FILE.
           PERFORM CLOSE-LN3-FILE.
           PERFORM CLOSE-LN1-FILE.
           PERFORM CLOSE-LM1-FILE.
           PERFORM CLOSE-BW1-FILE.
           PERFORM CLOSE-GR1-FILE.
           PERFORM CLOSE-BR-FILE.
           PERFORM CLOSE-GB-FILE.
           PERFORM CLOSE-PR-FILE.
           CLOSE WK-FILE.
       COPY "LIBGB/DECLRP2.CPY".

      ******************************************************************
      *
      *    M A I N   M O D U L E
      *
      ******************************************************************
       MAIN-MODULE SECTION.

           PERFORM SQL-CONNECT.

      D    STOP MESS.
           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "LASTPY" TO FORM-PROG VDU-FORM-NAME.

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
                                              LM-PATH-OVERRIDE
                                              LN-PATH-OVERRIDE
                                              LP-PATH-OVERRIDE
                                              LTI-PATH-OVERRIDE
                                              LTP-PATH-OVERRIDE
                                              GB-PATH-OVERRIDE.

      * NEED FOR LCAS ROUTINES (REPLACES GLOBRD)
           PERFORM OPEN-GB-FILE.
           MOVE GB-PATH-OWNBR TO GB-BRNO.
           MOVE 1             TO GB-NO.
           PERFORM READ-GB-FILE.
           PERFORM CLOSE-GB-FILE.
           IF IO-BAD
              GO TO END-OF-GROUP.

           PERFORM OPEN-WK-FILE-OUTPUT.
           PERFORM CLOSE-WK-FILE.
           PERFORM OPEN-WK-FILE.
           PERFORM OPEN-BW1-FILE.
           PERFORM OPEN-LN1-FILE.

           IF EXT-ROUTE-BUF = "00"
              MOVE BR-NO TO DISPLAY-MESS-BR
              MOVE WK-DISPLAY-MESS TO MESS
              PERFORM SEND-LEGEND.

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
           IF (IO-BAD) OR (LN-OWNBR NOT = LN-PATH-OWNBR)
              GO TO PROCESS-WK-FILE.

           PERFORM NPR-TRAP-INTERRUPT.

           IF LN-CLASS > END-CLASS
              GO TO PROCESS-WK-FILE.

      * SKIP PAID OUTS, P&L'S
           IF NOT LN-POCD-OPEN
              GO TO NEXT-LN.

           IF LN-PLDATE NOT = 0
              GO TO NEXT-LN.

      * ONLY WANT ACCOUNTS WHOSE LAST PAYMENT WAS A FULL PAYMENT
           IF LN-AMT-PAID-LAST < LN-REGPYAMT
              GO TO NEXT-LN.

           IF LN-CLASS < BEG-CLASS
              GO TO NEXT-LN.

      * GET STATE-PARAMETER-RECORD:
            PERFORM GET-SP.

      * COMPUTE PAID THRU:
           MOVE LN-1STPYDATE TO PDTH-DATE-WORK.
           PERFORM PAID-THRU-CALCULATION.

      * IF USER SELECTED TO PRINT ONLY ACCOUNTS WHOSE LAST PAYMENT WAS
      * WITH 7 DAYS OF THEIR NEXT DUE DATE, CHECK THIS PARAMETER:
           IF NXT-DUE-FLAG = "Y"
              MOVE PDTH-DATE-FULL    TO NUM-DATE
              MOVE LN-DATE-PAID-LAST TO SYS-DATE
              MOVE "D" TO DATER-UNITPER-CD
              PERFORM TIMALL
              IF ELAPSED-DAYS < 24 OR ELAPSED-DAYS > 30
                 GO TO NEXT-LN
              ELSE
                 GO TO CONTINUE-CREATE-WORK.

      * IF PDTHRU MO/YR GREATER THAN OR = LAST PAY DATE, SKIP
           MOVE PDTH-DATE-FULL    TO SYS-DATE.
           MOVE LN-DATE-PAID-LAST TO NUM-DATE.
           MOVE 1                 TO S-DD NUM-DA.
           IF SYS-DATE NOT < NUM-DATE
              GO TO NEXT-LN.

      * IF PDTHRU NOT AT LEAST A MONTH OLDER THAN LAST PAY DATE, SKIP
           MOVE PDTH-DATE-FULL    TO NUM-DATE.
           MOVE LN-DATE-PAID-LAST TO SYS-DATE.
           PERFORM TIMALL.
           IF ELAPSED-MONTHS < 1
              GO TO NEXT-LN.

       CONTINUE-CREATE-WORK.

      * COMPUTE CONTRACTUAL:
           MOVE HOLD-TODAY-DATE TO AGEING-DATE.
           PERFORM COMPUTE-CONTRACTUAL.
           PERFORM COMPUTE-RECENCY.

      * FILL IN WORK RECORD FIELDS

           MOVE SPACES   TO WK-KEY.
           MOVE LN-OWNBR TO WK-BRNO.
           IF COLLECTOR-FG = "Y"
              MOVE LN-COLLECTOR TO WK-COLCTID
           ELSE
              MOVE SPACES TO WK-COLCTID.

           MOVE SPACES TO WK-ACTION-CD.

           IF BRSEQ = "Y" OR "1"
              MOVE 0 TO WK-AGE WK-BALDUE
              MOVE " " TO WK-NAME.

           IF BRSEQ = "N" OR "2" OR " "
              SUBTRACT CONTRACTUAL FROM 99999 GIVING WK-AGE
              SUBTRACT LN-ACCUM-BAL FROM 9999999.99 GIVING WK-BALDUE
              MOVE " " TO WK-NAME.

           IF BRSEQ = "3" OR "6"
              MOVE 0 TO WK-AGE WK-BALDUE
              MOVE LN-SSNO(1) TO BW-SSNO
              PERFORM READ-BW1-FILE
              IF IO-FG NOT = 0
                 MOVE "ZZ" TO WK-NAME
              ELSE
                 STRING BW-LNAME "," BW-FNAME
                    DELIMITED BY SIZE
                       INTO WK-NAME.

           IF BRSEQ = "4"
              PERFORM CHECK-RENEWABILITY
              IF RENEWABLE-FG = "Y"
                 MOVE 0 TO WK-AGE
              ELSE
                 MOVE 99999 TO WK-AGE
              END-IF
              PERFORM GET-AVAILABLE-CREDIT
              SUBTRACT AVCR-AVAILCREDIT FROM 9999999.99 GIVING WK-BALDUE
              MOVE " " TO WK-NAME.

           IF BRSEQ = "5"
              SUBTRACT RECENCY FROM 99999 GIVING WK-AGE
              SUBTRACT LN-ACCUM-BAL FROM 9999999.99 GIVING WK-BALDUE
              MOVE " " TO WK-NAME.

           IF BRSEQ = "6"
              SUBTRACT CONTRACTUAL FROM 99999 GIVING WK-AGE.

           MOVE LN-ACCTNO TO WK-ACCTNO.
           PERFORM COMPUTE-PAID-AHEAD.
           MOVE CSUB TO WK-CSUB.
           MOVE CPOT30 TO WK-CPOT30.
           MOVE RSUB TO WK-RDSUB.
           MOVE RDPOT30 TO WK-RDPOT30.
           MOVE PSUB TO WK-PSUB.
           PERFORM WRITE-WK-FILE.
           IF IO-FG = 9
              GO TO IO-ERROR.

           GO TO NEXT-LN.

      *----------------------------------------------------------------
       PROCESS-WK-FILE.

           PERFORM DISABLE-INTERRUPT.
           PERFORM ENABLE-INTERRUPT.

           IF EXT-ROUTE-BUF = "00"
              MOVE BR-NO TO RUN-MESS-BR
              MOVE RUN-DISPLAY-MESS TO MESS
              PERFORM SEND-LEGEND.

           PERFORM CLOSE-LN1-FILE.
           PERFORM OPEN-LN1-FILE.
           PERFORM CLOSE-WK-FILE.
           PERFORM OPEN-WK-FILE.
           MOVE " " TO WK-KEY.
           MOVE 0 TO WK-BRNO WK-AGE WK-BALDUE WK-ACCTNO
                     WK-CSUB WK-RDSUB WK-PSUB.

           PERFORM START-WK-FILE.

      *----------------------------------------------------------------
       PROCESS-WK-FILE-PROCESS.
           PERFORM READ-WK-FILE-NEXT.
           IF IO-FG = 9
              IF FIRST-TIME NOT = "Y"
                 IF COLLECTOR-FG = "Y"
                    IF TOTALS-YES
                       PERFORM COLLECTOR-TOTALS
                    END-IF
                 END-IF
                 IF TOTALS-YES
                    PERFORM BRANCH-TOTALS
                 END-IF
                 MOVE WK-COLCTID TO OLD-COLCTID-CD
                 PERFORM SET-COLLECTOR-HEADINGS
                 PERFORM SET-BRANCH-HEADINGS
                 ADD 1 EXT-CONAME-RPT-LINES GIVING LN-COUNT
              END-IF
              GO TO END-OF-GROUP.

           UNLOCK WK-FILE.

           PERFORM TRAP-INTERRUPT.

           IF FIRST-TIME = "Y"
              MOVE WK-COLCTID TO OLD-COLCTID-CD
              PERFORM SET-COLLECTOR-HEADINGS
              PERFORM SET-BRANCH-HEADINGS.

      * TEST-COLLECTOR-BREAK.
           IF WK-COLCTID NOT = OLD-COLCTID-CD
              IF TOTALS-YES
                 PERFORM COLLECTOR-TOTALS
              END-IF
              MOVE WK-COLCTID TO OLD-COLCTID-CD
              PERFORM SET-COLLECTOR-HEADINGS
              ADD 1 EXT-CONAME-RPT-LINES GIVING LN-COUNT.

           MOVE WK-ACCTNO TO LN-ACCTNO.
           PERFORM READ-LN1-FILE.
           IF IO-FG = 9
              GO TO PROCESS-WK-FILE-PROCESS.

            PERFORM GET-SP.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      * SINGLE LINE AND SUMMARY WILL BOTH PERFORM THE SAME BUT
      * IF SUMMARY IT WONT PERFORM WRITE-DETAIL
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

           MOVE LN-ACCTNO       TO CRNOR2-ACCTNO.
           MOVE HOLD-TODAY-DATE TO CRNOR2-SYSDATE.
           MOVE "CL/CRNOR2"     TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH CRNOR2-LINK NO-REC
                                 POFF-WORKERS LN-REC.
           CANCEL FORM-PROGX.

           IF REPORT-TYPE = "L" OR "S"
              PERFORM SINGLE-LINE-DETAIL-PROCESS
           ELSE
              PERFORM DETAIL-LINE-PROCESS.

           MOVE "N" TO FIRST-TIME.
           MOVE "Y" TO NEW-BRANCH.
           MOVE "Y" TO RECORDS-FOUND.
           GO TO PROCESS-WK-FILE-PROCESS.

      *----------------------------------------------------------------
       END-OF-GROUP.
           PERFORM CLOSE-BW1-FILE.
           PERFORM CLOSE-LN3-FILE.

           MOVE "Y" TO FIRST-TIME.

           MOVE 1 TO LEV.
           PERFORM ZERO-TOTAL-TBL.
           MOVE 3 TO LEV.
           PERFORM ZERO-TOTAL-TBL.

           GO TO NEXT-GROUP-LOC.

      *----------------------------------------------------------------
       END-ROUTINE.

           IF NO-RECORDS-FOUND
              GO TO END-PROGRAM.

           IF TOTALS-YES
               IF WK-COLCTID NOT = OLD-COLCTID-CD
                  PERFORM COLLECTOR-TOTALS.

           IF ( BEG-BRANCH NOT = END-BRANCH )
                      OR ( EXT-EOBUF-GROUP-FG = "Y" )
              IF TOTALS-YES
                 PERFORM GRAND-TOTALS
              END-IF
           END-IF
           IF RANGES-YES
              PERFORM HEAD-PAGE
              PERFORM PRINT-RANGE-LINES.

           GO TO END-PROGRAM.

      ******************************************************************
       INITIALIZATION SECTION.
      ******************************************************************
           MOVE LASTPY-QUEUE-POS TO Q-POS.
           MOVE LASTPY-COPIES-POS TO C-POS.
           MOVE LASTPY-PRU-POS   TO P-POS.
           MOVE SPACES TO Q-BUF C-BUF P-BUF.

       COPY "LIBGB/EOINIT.CPY".

           MOVE AGE-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-ALPDATE.
           MOVE ALP-DATE TO H-AGEDATE.
           MOVE EXT-CONAME-CONAME TO H-NAME.
           MOVE EXT-CONAME-SYSDATE TO H-DATE.
           MOVE EXT-CONAME-USER-ID TO H-USERID.

           IF EXT-ROUTE-BUF = "70"
              MOVE EXT-EOBUF-MESS TO H-TITLE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
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
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

           MOVE AGE-DATE TO HOLD-TODAY-DATE AGEING-DATE.
           MOVE " " TO EXT-EOBUF-ERRCD.

           MOVE "CASH"      TO HEAD8-PROMPT.
           MOVE "AVAILABLE" TO HEAD9-PROMPT.
           MOVE "N" TO TOTAL-FG.
           MOVE 1 TO LEV.
           PERFORM ZERO-TOTAL-TBL.
           MOVE 2 TO LEV.
           PERFORM ZERO-TOTAL-TBL.
           PERFORM ZERO-TT-TOTALS.
           MOVE 3 TO LEV.
           PERFORM ZERO-TOTAL-TBL.

      * INITIALIZE ACCUMULATED PORTFOLIO TOTALS
           MOVE 0 TO ACCUM-SF-AMT ACCUM-SF-NO
                     ACCUM-DL-AMT ACCUM-DL-NO.

           PERFORM OPEN-BR-FILE.
           PERFORM OPEN-GR1-FILE.
      * MUST LEAVE SP1-FILE OPEN BECAUSE ADDONSPR READS IT
           PERFORM OPEN-SP1-FILE.

           IF EXT-ROUTE-BUF = "00"
              MOVE "REPORT IN PROGRESS...................." TO WR-BUF
              MOVE MESS-LIST TO WR-LIST
              PERFORM CALL-WRFORM.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      * NEED TO USE MKTEMP HERE FIRST BECAUSE WE NEED THE WORK
      * FILE PATH IN IT LAST FOR THE REMOVE
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

           PERFORM OPEN-PR-FILE.

           PERFORM ENABLE-INTERRUPT.

           MOVE MKTEMP-DEFAULT TO MKTEMP-BUF.
           PERFORM MKTEMP-CALL.
           PERFORM OPEN-WK-FILE-OUTPUT.
           PERFORM CLOSE-WK-FILE.

           GO TO EXIT-INITIALIZE.

       ENDIT.
           MOVE "X" TO EXT-EOBUF-ERRCD.

       EXIT-INITIALIZE.
           EXIT.

      ******************************************************************
       EO-EXEC-AUDIT SECTION.
      ******************************************************************
           MOVE EXT-EOBUF-DATE2 TO AGE-DATE.

           IF DEL-FLAG NOT = "Y"
              GO TO EXIT-EO-EXEC-AUDIT.

      * SETUP NEXT MONTH DATE (WORLD ONLY, FOR CORRECT EOM AGEING):
           MOVE EXT-EOBUF-DATE2 TO NDTE-DATE.
           MOVE 1 TO NDTE-HOLD.
           MOVE 1 TO NDTE-DD.
           PERFORM INCREMENT-MONTHS.
           MOVE NDTE-DATE TO AGE-DATE.

       EXIT-EO-EXEC-AUDIT.
           EXIT.

      ******************************************************************
       ENTER-PARAMETERS SECTION.
      ******************************************************************
       ENTER-PARM.

           IF EXT-CONAME-USER-LOC NOT NUMERIC
              MOVE "INVALID USER" TO MESS
              PERFORM SEND-MESS
              GO TO ENDIT.

           IF VDUSTATUS = "1U"
              GO TO SUMYN.

           IF EXT-ROUTE-BUF = "00"
              MOVE EXT-CONAME-USER-LOC TO BR-NO
              PERFORM OPEN-BR-FILE
              PERFORM READ-BR-FILE
              PERFORM CLOSE-BR-FILE
              IF IO-FG NOT = 0
                 GO TO IO-ERROR
              END-IF
              MOVE BR-CPCORP-ORDER TO BRSEQ
              MOVE "S  A010PRSEQ." TO VDU
              MOVE BRSEQ TO VDUBUF
              PERFORM SCREENIO.

      *----------------------------------------------------------------
       AGEDT.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      * BATCH WILL USE DATE RAN
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

           IF EXT-ROUTE-BUF = "40" OR "50"
              GO TO ENTER-DEL-FLAG.

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
              MOVE DATE-YYYYMMDD TO VDU-DATE-YYYYMMDD AGE-DATE
              PERFORM SCREENIO.

      *----------------------------------------------------------------
       ENTER-DEL-FLAG.
           MOVE "S  A160DELPROMPT:P." TO VDU.
           MOVE "BEG MO DELINQ?: "    TO VDUBUF.
           PERFORM SCREENIO.
      * DEFAUT FLAG TO 'N'
           MOVE "S  A010DELFLAG." TO VDU.
           IF DEL-FLAG = "Y" OR "N"
              MOVE DEL-FLAG TO VDUBUF
           ELSE
              MOVE "N" TO VDUBUF.
           PERFORM SCREENIO.
           MOVE "E  A010DELFLAG." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U"
              IF NOT (EXT-ROUTE-BUF = "40" OR "50")
                 GO TO AGEDT.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO ENTER-DEL-FLAG.

           MOVE VDUBUF TO DEL-FLAG.

           IF DEL-FLAG NOT = "Y" AND NOT = "N"
              GO TO ENTER-DEL-FLAG.

           IF DEL-FLAG = "N"
              GO TO BR-01.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      * SETUP NEXT MONTH DATE:
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE AGE-DATE TO NDTE-DATE.
           MOVE 1        TO NDTE-DD.
           MOVE 1        TO NDTE-HOLD.
           PERFORM INCREMENT-MONTHS.
           MOVE NDTE-DATE TO AGE-DATE.

      *----------------------------------------------------------------
       BR-01.
           MOVE "E  A000BR1." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "16" GO TO ALL-BR.
           IF VDUSTATUS = "1U" GO TO AGEDT.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.

           IF VDUSTATUS = "1<"
              MOVE GRSCAN-BUF TO SCAN-BUF
              PERFORM CALL-WINDOW-PROGRAM
              MOVE SCAN-BUF TO GRSCAN-BUF
              IF SCAN-STATUS NOT = "00"
                 GO TO BR-01
              ELSE
                 MOVE "S NA000BR1." TO VDU
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
              MOVE "S  A160DISPLAY-USER." TO VDU
              MOVE SPACES TO VDUBUF
              PERFORM SCREENIO
              MOVE "Y" TO GROUP-FLAG
              MOVE 0 TO BEG-BRANCH
              MOVE 9999 TO END-BRANCH
              GO TO CHECK-BRANCH-SECURITY.

      *----------------------------------------------------------------
       BRANCH-IS-NUMERIC.
           MOVE "N" TO GROUP-FLAG.
           MOVE WGR-SRESULT TO BEG-BRANCH.
           MOVE "SN N040BR1." TO VDU.
           MOVE BEG-BRANCH TO VDUNUM0.
           PERFORM SCREENIO.

      *----------------------------------------------------------------
       BR-02.
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

           IF ( (BEG-BRANCH = 0) AND (END-BRANCH = 0) )
              MOVE "S  A160DISPLAY-USER." TO VDU
              MOVE "USER BRANCH ONLY" TO VDUBUF
              PERFORM SCREENIO
           ELSE
              MOVE "S  A160DISPLAY-USER." TO VDU
              MOVE SPACES TO VDUBUF
              PERFORM SCREENIO.

           GO TO CHECK-BRANCH-SECURITY.

      *----------------------------------------------------------------
       ALL-BR.
           MOVE "SNNN040BR1." TO VDU.
           MOVE 0 TO BEG-BRANCH VDUNUM0.
           PERFORM SCREENIO.

           MOVE "SNNN040BR2." TO VDU.
           MOVE 9999 TO END-BRANCH VDUNUM0.
           PERFORM SCREENIO.

           MOVE "S NA160DISPLAY-USER." TO VDU.
           MOVE SPACES TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "N" TO GROUP-FLAG.

      *----------------------------------------------------------------
       CHECK-BRANCH-SECURITY.
           PERFORM BR-RANGE-SECURITY.

           IF BR-SECURITY-OKAY = "N"
              GO TO BR-01.

      *----------------------------------------------------------------
       CL-01.
           MOVE "ENNN020CL1." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "10" GO TO BR-01.
           IF VDUSTATUS = "16" GO TO ALL-CL.
           IF VDUSTATUS = "1U" GO TO BR-02.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "1<"
              MOVE "BEG" TO CLASS-FG
              PERFORM CLASS-SCAN
              IF IO-FG NOT = 0
              GO TO CL-01.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO CL-01.

           MOVE VDUNUM0 TO BEG-CLASS.

      *----------------------------------------------------------------
       CL-02.
           MOVE "ENNN020CL2." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "10" GO TO BR-01.
           IF VDUSTATUS = "1U" GO TO CL-01.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "1<"
              MOVE "END" TO CLASS-FG
              PERFORM CLASS-SCAN
              IF IO-FG NOT = 0
              GO TO CL-02.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO CL-02.

           MOVE VDUNUM0 TO END-CLASS.

           IF BEG-CLASS > END-CLASS
              PERFORM INVALID-RANGE
              GO TO CL-01.

           GO TO PRINTSEQ.

      *----------------------------------------------------------------
       ALL-CL.
           MOVE "SNNN020CL1." TO VDU.
           MOVE 0 TO BEG-CLASS VDUNUM0.
           PERFORM SCREENIO.

           MOVE "SNNN020CL2." TO VDU.
           MOVE 99 TO END-CLASS VDUNUM0.
           PERFORM SCREENIO.

      *----------------------------------------------------------------
       PRINTSEQ.
           MOVE "E  A010PRSEQ." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "10" GO TO BR-01.
           IF VDUSTATUS = "1U" GO TO CL-02.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO PRINTSEQ.

           IF VDUBUF NOT = "1" AND
              VDUBUF NOT = "2" AND
              VDUBUF NOT = "3" AND
              VDUBUF NOT = "4" AND
              VDUBUF NOT = "5" AND
              VDUBUF NOT = "6"
                 MOVE "ENTER A VALUE FROM 1 TO 6 " TO MESS
                 PERFORM SEND-MESS
                 GO TO PRINTSEQ
           ELSE
              MOVE VDUBUF TO BRSEQ
              GO TO NXT-DUE-FG-YN.

           IF VDUBUF NOT = "1" AND
              VDUBUF NOT = "2" AND
              VDUBUF NOT = "3" AND
              VDUBUF NOT = "5" AND
              VDUBUF NOT = "6"
                 MOVE "ENTER 1, 2, 3, 5, OR 6 ONLY " TO MESS
                 PERFORM SEND-MESS
                 GO TO PRINTSEQ
           ELSE
                 MOVE VDUBUF TO BRSEQ.

      *----------------------------------------------------------------
       NXT-DUE-FG-YN.
           MOVE "E  A010NXTDUEFG." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "10" GO TO BR-01.
           IF VDUSTATUS = "1U" GO TO PRINTSEQ.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D"
              GO TO NXT-DUE-FG-YN.

           IF VDUBUF NOT = "Y" AND
              VDUBUF NOT = "N"
                 MOVE "ENTER Y OR N ONLY " TO MESS
                 PERFORM SEND-MESS
                 GO TO NXT-DUE-FG-YN
           ELSE
                 MOVE VDUBUF TO NXT-DUE-FLAG.
      *----------------------------------------------------------------
       COLLECTOR-YN.
           MOVE "E  A010COLL." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "10" GO TO BR-01.
           IF VDUSTATUS = "1U" GO TO NXT-DUE-FG-YN.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO COLLECTOR-YN.

           IF VDUBUF NOT = "Y" AND
              VDUBUF NOT = "N"
                 MOVE "ENTER Y OR N ONLY " TO MESS
                 PERFORM SEND-MESS
                 GO TO COLLECTOR-YN
           ELSE
                 MOVE VDUBUF TO COLLECTOR-FG.
      *----------------------------------------------------------------
       TOTALS-YN.
           MOVE "E  A010TOT." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "10" GO TO BR-01.
           IF VDUSTATUS = "1U" GO TO COLLECTOR-YN.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO TOTALS-YN.

           IF NOT (VDUBUF = "Y" OR "N" OR "T" OR "R" )
                 MOVE "ENTER Y, N, T OR R ONLY " TO MESS
                 PERFORM SEND-MESS
                 GO TO TOTALS-YN
           ELSE
                 MOVE VDUBUF TO TOTALS-FG.

      *----------------------------------------------------------------
       SUMYN.
           MOVE "E  A010SUMONLY." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "10" GO TO BR-01.
           IF VDUSTATUS = "1U" GO TO TOTALS-YN.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO SUMYN.

           MOVE VDUBUF TO REPORT-TYPE.

           IF REPORT-TYPE NOT = "D" AND
                           NOT = "B" AND
                            NOT = "L" AND
                             NOT = "S"
              GO TO SUMYN.

      * IF USERS CHOOSES TO SUPPRESS ALL TOTALS
      *    DO NOT ALLOW THEN TO SELECT SUMMARY ONLY.
           IF NOT TOTALS-YES
              IF REPORT-TYPE = "S"
                 GO TO SUMYN.
       EXIT-PARM.
           EXIT.

      ******************************************************************
       DISPLAY-PARAMETERS SECTION.
      ******************************************************************
           MOVE "S  A160DELPROMPT:P." TO VDU.
           MOVE "BEG MO DELINQ?: "    TO VDUBUF.
           PERFORM SCREENIO..
           MOVE "S  A010DELFLAG." TO VDU.
           MOVE DEL-FLAG TO VDUBUF.
           PERFORM SCREENIO.

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
           IF ( (BEG-BRANCH = 0) AND (END-BRANCH = 0) )
              MOVE "S  A160DISPLAY-USER." TO VDU
              MOVE "USER BRANCH ONLY" TO VDUBUF
              PERFORM SCREENIO
           ELSE
              MOVE "S  A160DISPLAY-USER." TO VDU
              MOVE SPACES TO VDUBUF
              PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "SNNN020CL1." TO VDU.
           MOVE BEG-CLASS TO VDUNUM0.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "SNNN020CL2." TO VDU.
           MOVE END-CLASS TO VDUNUM0.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "S  A010PRSEQ." TO VDU.
           MOVE BRSEQ TO VDUBUF.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "S  A010NXTDUEFG." TO VDU.
           MOVE NXT-DUE-FLAG TO VDUBUF.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "S  A010COLL." TO VDU.
           MOVE COLLECTOR-FG TO VDUBUF.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "S  A010TOT." TO VDU.
           MOVE TOTALS-FG TO VDUBUF.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "S  A010SUMONLY." TO VDU.
           MOVE REPORT-TYPE TO VDUBUF.
           PERFORM SCREENIO.

      ******************************************************************
       DETAIL-LINE-PROCESS SECTION.
      ******************************************************************

           MOVE DASH-LINE TO DETAIL-LINE.
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE LN-SSNO(1) TO BW-SSNO.
           PERFORM READ-BW1-FILE.

           PERFORM LPSTAT-DISPLAY.
           MOVE LPS-DISPLAY-LNNO TO D-LOAN-NUM.
           MOVE LN-LOANDATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO D-LOANDATE.
           MOVE LN-DLNO TO D-DLNO.
           MOVE LN-SRCD TO D-SRCD.
           MOVE LN-COLLCD TO D-COLLCD.
           MOVE LN-PURCD TO D-PURCD.
           MOVE LN-SUM-ORGTERM TO D-ORGTERM.
           MOVE LN-SUM-LNAMT TO D-LNAMT.
           MOVE LN-1STPYDATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO D-1STPYDATE.

           IF CRNOR2-CPOT30 = "Y"
              MOVE "<P>" TO D-CONX
           ELSE
           IF CRNOR2-CPOT30 = "M"
              MOVE "<M>" TO D-CONX
           ELSE
           IF CRNOR2-CPOTCHGOFF = "Y"
              MOVE "<PC>" TO D-CONX
           ELSE
           IF CRNOR2-CPOTCHGOFF = "M"
              MOVE "<PL>" TO D-CONX
           ELSE
              MOVE NO-TRUCONTR TO D-CON.

           MOVE LN-CONPROFILE(1) TO PROFILE-9.
           PERFORM PROFILE-DASH.
           MOVE PROFILE-X TO D-CONPROF.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      * CANT TRUST PDTH-DATE
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

           MOVE NO-PDTH-DATE TO PDTH-DATE-WORK.
           MOVE PDTH-WK-MM   TO PDTH-MM.
           IF LN-UNITPER-CD-DAYS
              MOVE PDTH-WK-DD TO PDTH-YY
           ELSE
              MOVE PDTH-WK-YY TO PDTH-YY.
           MOVE PDTH-DATE TO D-PDTH.
           MOVE LN-PROMDATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO D-PROMISED.

           COMPUTE D-BALANCE = LN-CURBAL + LN-OTHBAL + LN-INTBAL
                               + LN-LCBAL.

      *-----------------------------------------------------------------
       DETAIL-GET-TODAYDUE.
           MOVE NO-LDUE-DATE  TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY   TO D-LASTDATE.
           MOVE NO-NXDUE-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY   TO D-NEXTDATE.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE " " TO SAVE-BW-FIELDS.
           MOVE 0 TO SAVE-BW-PHONE SAVE-BW-EMPPHONE SAVE-BW-LOANS.
           MOVE 0 TO SAVE-CO-PHONE SAVE-CO-EMPPHONE SAVE-BW-EMPEXT.
           MOVE 1 TO SUB.
           PERFORM PROCESS-BORROWER.

           IF IO-FG = 0
              MOVE BW-ADR1 TO SAVE-BW-ADR1
              MOVE BW-ADR2 TO SAVE-BW-ADR2
              MOVE BW-CITY TO SAVE-BW-CITY
              MOVE BW-STATE TO SAVE-BW-STATE
              MOVE BW-ZIP TO SAVE-BW-ZIP
              MOVE BW-PHONE TO SAVE-BW-PHONE
              MOVE BW-EMPLOYER TO SAVE-BW-EMPLOYER
              MOVE BW-EMPPHONE TO SAVE-BW-EMPPHONE
              MOVE BW-EMPLPOS TO SAVE-BW-EMPLPOS
              MOVE BW-EMPEXT TO SAVE-BW-EMPEXT
              MOVE BW-LOANS TO SAVE-BW-LOANS.

           MOVE 0 TO LCHG-LATE.
           MOVE NO-LATE-CHG TO D-LCAMT.

           PERFORM MATURITY-DATE-CALCULATION.
           IF LN-TOTNOALDEL NOT = 0
              MOVE MDTE-DATE       TO NDTE-DATE
              MOVE LN-TOTNOALDEL   TO NDTE-HOLD
              MOVE LN-UNITPER-CD   TO DATER-UNITPER-CD
              MOVE LN-UNITPER-FREQ TO DATER-UNITPER-FREQ
              PERFORM INCREMENT-UNITPER
              MOVE NDTE-DATE TO MDTE-DATE.

           MOVE MDTE-DATE   TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO D-MATURDATE.

           IF BR-RECDEL-NOT-REC = "Y"
              IF CRNOR2-RDPOT30 = "Y"
                 MOVE "<P>" TO D-RECX
              ELSE
              IF CRNOR2-RDPOT30 = "M"
                 MOVE "<M>" TO D-RECX
              ELSE
              IF CRNOR2-RDPOTCHGOFF = "Y"
                 MOVE "<PC>" TO D-RECX
              ELSE
              IF CRNOR2-RDPOTCHGOFF = "M"
                 MOVE "<PL>" TO D-RECX
              ELSE
                 MOVE NO-RECDEL TO D-REC
           ELSE
              MOVE NO-RECENCY TO D-REC.

           MOVE LN-RECPROFILE(1) TO PROFILE-9.
           PERFORM PROFILE-DASH.
           MOVE PROFILE-X TO D-RECPROF.

J029       IF ( SP-LCFRMLA NOT = " "            ) OR
J029  * FFR   ( NOT (LN-LC-CODEX = "00" OR " ") )               *> FFR
J029          ( LN-LC-CODE NOT = ZEROES         )               *> FFR
              MOVE LN-LCPDTH-DATE TO NUM-DATE
              MOVE NUM-MO         TO D-LCPD-MO
              IF LN-UNITPER-CD-DAYS
                 MOVE NUM-DA TO D-LCPD-DAYR
              ELSE
                 MOVE NUM-YR TO D-LCPD-DAYR.

           MOVE LN-LASTCFORMDATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO D-FORMDATE.
           IF GP-NO-LC-IN-ARREARAGE
              MOVE NO-TDUECONTR TO D-TODAYDUE
              MOVE NO-LDUECONTR TO D-LASTAMT
              MOVE NO-NXDUECONTR TO D-NEXTAMT
           ELSE
              MOVE NO-TDUEAMT TO D-TODAYDUE
              MOVE NO-LDUEAMT TO D-LASTAMT
              MOVE NO-NXDUEAMT TO D-NEXTAMT.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           IF LN-SSNO(2) = 0
              MOVE BW-SPSSNO TO LN-SSNO(2)
              MOVE "S" TO LN-MAKERCD(2).

           IF LN-CONFIDENTCD = "Y" OR BW-CONFIDENTIAL = "Y"
              IF LN-ALLOTCD = "Y"
                 MOVE "*ALOT/CONF*" TO D-MSTAG
              ELSE
                MOVE "*CONFIDENT*" TO D-MSTAG.

           IF LN-CONFIDENTCD NOT = "Y"
              IF BW-CONFIDENTIAL NOT = "Y"
                 IF LN-ALLOTCD = "Y"
                   MOVE "*ALLT PLAN*" TO D-MSTAG.

           MOVE 2 TO SUB.
           PERFORM PROCESS-BORROWER.

           IF IO-FG = 0
              IF LN-MAKERCD(SUB) = "J" OR "E"
                 MOVE BW-PHONE TO SAVE-CO-PHONE
                 MOVE BW-EMPPHONE TO SAVE-CO-EMPPHONE.

           IF D-MSTAG = SPACES
              MOVE "MKR PHONE:" TO D-MSTAG.

           IF SAVE-BW-EMPPHONE NOT = 0
              MOVE "W" TO D-WORK
              MOVE SAVE-BW-EMPPHONE TO PN-PHONE
              PERFORM SEND-PHONE
              MOVE PA-PHONE TO D-PHONE1
              IF SAVE-BW-EMPEXT NOT = 0
                 MOVE "X" TO D-WORKEXT
                 MOVE SAVE-BW-EMPEXT TO D-PHONE1EXT.

           IF SAVE-BW-PHONE NOT = 0
              MOVE "H" TO D-HOME
              MOVE SAVE-BW-PHONE TO PN-PHONE
              PERFORM SEND-PHONE
              MOVE PA-PHONE TO D-PHONE2.

           IF LN-LASTCFORMDATE NOT = 0
              MOVE "#" TO D-N
              MOVE LN-LASTCFORMNO TO D-FORMNO.

           MOVE NO-LDUELC TO D-LDUELC.
           MOVE NO-TDUELC TO D-TDUELC.
           MOVE NO-NXDUELC TO D-NXDUELC.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           IF REPORT-TYPE = "B"
              GO TO DETAIL-LN-ACCTOT.

           MOVE 3 TO SUB.
           PERFORM PROCESS-BORROWER.

           IF D-NAME = SPACES AND (LN-MAKERCD(2) = "J" OR "E")
              MOVE "CO PHONE: " TO D-COMAKER-PHONE
              MOVE "W" TO D-CO-WORK
              MOVE SAVE-CO-EMPPHONE TO PN-PHONE
              PERFORM SEND-PHONE
              MOVE PA-PHONE TO D-CO-PHONE1
              MOVE "H" TO D-CO-HOME
              MOVE SAVE-CO-PHONE TO PN-PHONE
              PERFORM SEND-PHONE
              MOVE PA-PHONE TO D-CO-PHONE2.

           MOVE "EMPLOYER:" TO D-EMPLOYER.
           PERFORM GET-LOAN-MEMOS.
           MOVE "-----MEMO-----" TO D-MEMOX.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE 4 TO SUB.
           PERFORM PROCESS-BORROWER.
           MOVE SAVE-BW-EMPLOYER TO D-EMPLOYER.

           IF LOAN-MEMO(1) NOT = SPACES
              MOVE LOAN-MEMO(1) TO D-MEMO
              MOVE LOAN-MEMO-DT(1) TO D-MEMO-DT.

           MOVE 1 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE SAVE-BW-ADR1 TO D-ADDR.
           MOVE SAVE-BW-EMPLPOS TO D-EMPLOYER.

           IF LOAN-MEMO(2) NOT = SPACES
              MOVE LOAN-MEMO-DT(2) TO D-MEMO-DT
              MOVE LOAN-MEMO(2) TO D-MEMO.

           PERFORM CONDITIONAL-PRINT.

           MOVE SAVE-BW-CITY TO NAME-WORKER.
           INSPECT NAME-WORKER REPLACING FIRST "      " BY ", ++ +".
           INSPECT NAME-WORKER REPLACING FIRST "++" BY SAVE-BW-STATE.

           INSPECT NAME-WORKER REPLACING FIRST "+         "
              BY SAVE-BW-ZIP.

           IF SAVE-BW-ADR2 NOT = " "
              MOVE SAVE-BW-ADR2 TO D-ADDR
           ELSE
              MOVE NAME-WORKER TO D-ADDR.

           MOVE "# LOANS:" TO D-LOANSX.
           MOVE SAVE-BW-LOANS TO D-LOANS.

           IF LOAN-MEMO(3) NOT = SPACES
              MOVE LOAN-MEMO(3) TO D-MEMO
              MOVE LOAN-MEMO-DT(3) TO D-MEMO-DT.

           PERFORM CONDITIONAL-PRINT.

           IF SAVE-BW-ADR2 NOT = " "
              MOVE NAME-WORKER TO D-ADDR.

           MOVE "REG PMT:" TO D-LOANSX.
           MOVE LN-REGPYAMT TO D-PMT.

           IF LOAN-MEMO(4) NOT = SPACES
              MOVE LOAN-MEMO(4) TO D-MEMO
              MOVE LOAN-MEMO-DT(4) TO D-MEMO-DT.

           PERFORM CONDITIONAL-PRINT.

           PERFORM GET-LOAN-PMTS.

           IF LOAN-PMT(1) NOT = " "
              MOVE HEAD7 TO D-PMT-LINE
              MOVE 1 TO LN-FEED
              PERFORM WRITE-DETAIL-LINE.

           MOVE LOAN-PMT(1) TO D-PMT-LINE.
           PERFORM CONDITIONAL-PRINT.

           MOVE LOAN-PMT(2) TO D-PMT-LINE.
           PERFORM CONDITIONAL-PRINT.

           MOVE LOAN-PMT(3) TO D-PMT-LINE.
           PERFORM CONDITIONAL-PRINT.

           MOVE LOAN-PMT(4) TO D-PMT-LINE.
           PERFORM CONDITIONAL-PRINT.

      *----------------------------------------------------------------
       DETAIL-LN-ACCTOT.

           MOVE 1 TO LEV.
           PERFORM DETAIL-LN-ACCTBL.
           MOVE 2 TO LEV.
           PERFORM DETAIL-LN-ACCTBL.
           MOVE 3 TO LEV.
           PERFORM DETAIL-LN-ACCTBL.

           GO TO END-DETAIL-LINE-PROCESS.

      *-----------------------------------------------------------------
       DETAIL-LN-ACCTBL.
           IF LN-SALEFIN = "Y"
              MOVE 2 TO LTYP
           ELSE
              MOVE 1 TO LTYP.

           ADD 1 TO T-NO(LTYP LEV WK-CSUB 1).

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      * T-OTH(LTYP LEV WK-CSUB 1) = LN-OTHBAL + LN-OT2BAL.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

           ADD LN-OTHBAL TO T-OTH(LTYP LEV WK-CSUB 1).
           ADD LN-OT2BAL TO T-OTH(LTYP LEV WK-CSUB 1).
           ADD LN-INTBAL TO T-INT(LTYP LEV WK-CSUB 1).
           ADD LN-LCBAL TO T-INT(LTYP LEV WK-CSUB 1).

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      * T-BAL(LTYP LEV WK-CSUB 1) = LN-CURBAL - LN-OT2BAL
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

           ADD LN-CURBAL TO T-BAL(LTYP LEV WK-CSUB 1).
           SUBTRACT LN-OT2BAL FROM T-BAL(LTYP LEV WK-CSUB 1).
           IF GP-NO-LC-IN-ARREARAGE
              ADD NO-TDUECONTR TO T-TDUE(LTYP LEV WK-CSUB 1)
           ELSE
              ADD NO-TDUEAMT TO T-TDUE(LTYP LEV WK-CSUB 1).
           ADD 1 TO T-NO(LTYP LEV WK-RDSUB 2).

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      * T-OTH(LTYP LEV WK-PSUB 2) = LN-OTHBAL + LN-OT2BAL.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

           ADD LN-OTHBAL TO T-OTH(LTYP LEV WK-RDSUB 2).
           ADD LN-OT2BAL TO T-OTH(LTYP LEV WK-RDSUB 2).
           ADD LN-INTBAL TO T-INT(LTYP LEV WK-RDSUB 2).
           ADD LN-LCBAL TO T-INT(LTYP LEV WK-RDSUB 2).

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      * T-BAL(LTYP LEV WK-RDSUB 2) = LN-CURBAL - LN-OT2BAL
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

           ADD LN-CURBAL TO T-BAL(LTYP LEV WK-RDSUB 2).
           SUBTRACT LN-OT2BAL FROM T-BAL(LTYP LEV WK-RDSUB 2).
           IF GP-NO-LC-IN-ARREARAGE
              ADD NO-TDUECONTR TO T-TDUE(LTYP LEV WK-RDSUB 2)
           ELSE
              ADD NO-TDUEAMT TO T-TDUE(LTYP LEV WK-RDSUB 2).

           IF WK-PSUB > 0
              ADD 1 TO T-NO(LTYP LEV WK-PSUB 3)

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      * T-OTH(LTYP LEV WK-PSUB 3) = LN-OTHBAL + LN-OT2BAL
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

              ADD LN-OTHBAL TO T-OTH(LTYP LEV WK-PSUB 3)
              ADD LN-OT2BAL TO T-OTH(LTYP LEV WK-PSUB 3)
              ADD LN-INTBAL TO T-INT(LTYP LEV WK-PSUB 3)
              ADD LN-LCBAL TO T-INT(LTYP LEV WK-PSUB 3)

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      * T-BAL(LTYP LEV WK-PSUB 3) = LN-CURBAL - LN-OT2BAL
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

              ADD LN-CURBAL TO T-BAL(LTYP LEV WK-PSUB 3)
              SUBTRACT LN-OT2BAL FROM T-BAL(LTYP LEV WK-RDSUB 3)
              IF GP-NO-LC-IN-ARREARAGE
                 ADD NO-TDUECONTR TO T-TDUE(LTYP LEV WK-PSUB 3)
              ELSE
                 ADD NO-TDUEAMT TO T-TDUE(LTYP LEV WK-PSUB 3).

       END-DETAIL-LINE-PROCESS.
           EXIT.

      ******************************************************************
       SINGLE-LINE-DETAIL-PROCESS SECTION.
      ******************************************************************

           PERFORM LOAN-CALCULATIONS.
           MOVE 1 TO SUB.
           PERFORM PROCESS-BORROWER.

           PERFORM LPSTAT-DISPLAY.
           MOVE LPS-DISPLAY-LNNO TO D-SLOAN-NUM.
           MOVE LN-1STPYDA TO D-SDUEDAY.
           PERFORM COMPUTE-RECENCY.

           IF BR-RECDEL-NOT-REC = "Y"
              IF CRNOR2-RDPOT30 = "Y"
                 MOVE "<P>" TO D-SRECX
              ELSE
              IF CRNOR2-RDPOT30 = "M"
                 MOVE "<M>" TO D-SRECX
              ELSE
              IF CRNOR2-RDPOTCHGOFF = "Y"
                 MOVE "<PC>" TO D-SRECX
              ELSE
              IF CRNOR2-RDPOTCHGOFF = "M"
                 MOVE "<PL>" TO D-SRECX
              ELSE
                 MOVE NO-RECDEL TO D-SREC
           ELSE
              MOVE NO-RECENCY TO D-SREC.

           IF CRNOR2-CPOT30 = "Y"
              MOVE "<P>" TO D-SCONX
           ELSE
           IF CRNOR2-CPOT30 = "M"
              MOVE "<M>" TO D-SCONX
           ELSE
           IF CRNOR2-CPOTCHGOFF = "Y"
              MOVE "<PC>" TO D-SCONX
           ELSE
           IF CRNOR2-CPOTCHGOFF = "M"
              MOVE "<PL>" TO D-SCONX
           ELSE
              MOVE NO-TRUCONTR TO D-SCON.

           MOVE LN-DATE-PAID-LAST TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO D-SLASTDATE.
           MOVE LN-LOANDATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO D-SLNDATE.
           MOVE LN-PROMDATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO D-SPROMDATE.

           COMPUTE D-SBALANCE = LN-CURBAL + LN-OTHBAL + LN-INTBAL
                                + LN-LCBAL.

           IF GP-NO-LC-IN-ARREARAGE
              MOVE NO-TDUECONTR TO D-STODAYDUE
           ELSE
              MOVE NO-TDUEAMT TO D-STODAYDUE.

      *????MOVE TOTAL-NOTE TO D-STOTNOTE.

           IF LN-CONFIDENTCD = "Y" OR BW-CONFIDENTIAL = "Y"
              IF LN-ALLOTCD = "Y"
                 MOVE "C A" TO D-SFLAG
              ELSE
                 MOVE "C" TO D-SFLAG.

           IF LN-CONFIDENTCD NOT = "Y"
              IF BW-CONFIDENTIAL NOT = "Y"
                 IF LN-ALLOTCD = "Y"
                    MOVE "A" TO D-SFLAG.

           MOVE NO-PDTH-DATE TO PDTH-DATE-WORK.
           MOVE PDTH-WK-MM TO PDTH-MM.
           IF LN-UNITPER-CD-DAYS
              MOVE PDTH-WK-DD TO PDTH-YY
           ELSE
              MOVE PDTH-WK-YY TO PDTH-YY.
           MOVE PDTH-DATE TO D-SPDTH.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *?????????????????????????????????????????????????????
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           PERFORM CHECK-RENEWABILITY
           IF RENEWABLE-FG = "Y"
              PERFORM GET-AVAILABLE-CREDIT
              MOVE AVCR-AVAILCREDIT TO D-SDLNO
           ELSE
              MOVE 0 TO D-SDLNO.
           MOVE LN-AMT-PAID-LAST TO D-STOTAMTPD.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      * BOTH SUMMARY AND SINGLE LINE PERFORM THIS ROUTINE
      * IF SUMMARY ONLY, DONT WRITE THE LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

           IF REPORT-TYPE = "L"
              PERFORM WRITE-DETAIL-LINE
           ELSE
              MOVE SPACES TO DETAIL-LINE.

           MOVE 1 TO LEV.
           PERFORM DETAIL-LN-ACCTBL.
           MOVE 2 TO LEV.
           PERFORM DETAIL-LN-ACCTBL.
           MOVE 3 TO LEV.
           PERFORM DETAIL-LN-ACCTBL.

       END-SINGLE-DETAIL-LINE-PROCESS.
           EXIT.

      ******************************************************************
       PROCESS-BORROWER SECTION.
      ******************************************************************
           IF REPORT-TYPE NOT = "L"
              MOVE SPACES TO D-NAME
           ELSE
              MOVE SPACES TO D-SNAME.

           MOVE LN-SSNO(SUB) TO BW-SSNO.

           IF BW-SSNO = 0
              GO TO END-PROCESS-BORROWER.

           IF REPORT-TYPE NOT = "L"
              MOVE LN-MAKERCD(SUB) TO D-MAKERCD.

           MOVE BW-SSNO TO NUM-BW.
           PERFORM NUMBW-TO-ALPBW.

           IF REPORT-TYPE NOT = "L"
              MOVE ALP-BW TO D-SSNO.

           IF (REPORT-TYPE = "L"  OR "S") OR SUB NOT = 1
              PERFORM READ-BW1-FILE.

           IF IO-FG NOT = 0
               IF REPORT-TYPE = "L"
                  MOVE "**NOT ON FILE**" TO D-SNAME
               ELSE
                  MOVE "**NOT ON FILE**" TO D-NAME.

           IF IO-FG NOT = 0
              MOVE 0 TO BW-SPSSNO
              GO TO END-PROCESS-BORROWER.

           MOVE BW-LNAME TO NAME-WORKER.
           MOVE BW-FNAME TO FIRST-NAME.
           INSPECT NAME-WORKER REPLACING FIRST "  " BY ",?".

           INSPECT NAME-WORKER REPLACING FIRST "?               "
               BY FIRST-NAME.

           IF REPORT-TYPE = "L"
              MOVE NAME-WORKER TO D-SNAME
           ELSE
              MOVE NAME-WORKER TO D-NAME.

       END-PROCESS-BORROWER.
           EXIT.

      ******************************************************************
       GET-LOAN-MEMOS SECTION.
      ******************************************************************

           PERFORM OPEN-LM1-FILE.

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

           MOVE " " TO LOAN-MEMO-BUF.
           MOVE 1 TO MEMO-SUB.

       NEXT-MEMO.
           PERFORM READ-LM1-FILE-NEXT.
           IF IO-FG = 9 OR (LM-BRNO NOT = LM-PATH-OWNBR)
              GO TO EXIT-GET-LOAN-MEMOS.

           IF LM-ACCTNO NOT = LN-ACCTNO
              GO TO EXIT-GET-LOAN-MEMOS.

           MOVE LM-MEMO TO LOAN-MEMO (MEMO-SUB).
           SUBTRACT LM-DATE FROM 99999999 GIVING DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO LOAN-MEMO-DT (MEMO-SUB).

           IF MEMO-SUB < 4
              ADD 1 TO MEMO-SUB
              GO TO NEXT-MEMO.

       EXIT-GET-LOAN-MEMOS.
           PERFORM CLOSE-LM1-FILE.

      ******************************************************************
       GET-LOAN-PMTS SECTION.
      ******************************************************************
           PERFORM OPEN-LP1-FILE

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
           MOVE " " TO LOAN-PMT-BUF.
           MOVE 1 TO PMT-SUB.

       NEXT-PMT.
           PERFORM READ-LP1-FILE-PREVIOUS.
           IF IO-FG = 9
              GO TO EXIT-GET-LOAN-PMTS.

           IF (LP-BRNO NOT = LN-OWNBR) OR (LP-ACCTNO NOT = LN-ACCTNO)
              GO TO EXIT-GET-LOAN-PMTS.

           MOVE " " TO PMT-LINE.
           MOVE LP-PAYDATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO L-010.
           MOVE LP-TRCD TO L-020.
           MOVE LP-REFNO TO L-030.
           IF LP-TRCD = "JD"  OR "AC" OR "IR"
              MOVE LP-NEWSTAT-RATE TO L-040RATE
           ELSE
              MOVE LP-TRAMT TO L-040.

           IF LP-TRCD = "AP"
              MOVE LP-APINTOWE TO L-050
              ADD LP-INTDUE LP-LCPARTIALS GIVING L-060
              COMPUTE L-070 = 0 - LP-APINTOWE
                                - LP-INTDUE
                                - LP-LCPARTIALS
           ELSE
              MOVE LP-APOTH TO L-050
              ADD LP-APINT LP-APLC GIVING L-060
              MOVE LP-APCUR TO L-070.

           IF (LP-TRCD = "RV" OR LP-REV NOT = " ") AND
               NOT (LP-TRCD = "RV" AND LP-REVSEQNO = 0)
              MOVE "*" TO L-120
           ELSE
              MOVE LP-PDTH-DATE TO NUM-DATE
              MOVE NUM-MO TO L-080-MO
              IF LN-UNITPER-CD-DAYS
                 MOVE NUM-DA TO L-080-DAYR
              ELSE
                 MOVE NUM-YR TO L-080-DAYR
              END-IF
              COMPUTE L-090 = LP-OTHBAL + LP-OT2BAL
              ADD LP-LCBAL LP-INTBAL GIVING L-100
              MOVE LP-CURBAL TO L-110.

           MOVE LOAN-PMT(3) TO LOAN-PMT(4).
           MOVE LOAN-PMT(2) TO LOAN-PMT(3).
           MOVE LOAN-PMT(1) TO LOAN-PMT(2).
           MOVE D-PMT-LINE TO LOAN-PMT(1).

           IF PMT-SUB < 4
              ADD 1 TO PMT-SUB
              GO TO NEXT-PMT.

       EXIT-GET-LOAN-PMTS.
           PERFORM CLOSE-LP1-FILE.

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
       PRINT-RANGE-LINES SECTION.
      ******************************************************************
           MOVE 4 TO LN-FEED.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF GROUP-FLAG = "Y"
              MOVE "BEGINNING BRANCH   :" TO RANGE-LINE
              MOVE ENT-GROUP              TO RANGE-SELECTION
              PERFORM WRITE-DETAIL-LINE
              MOVE "ENDING BRANCH      :" TO RANGE-LINE
              MOVE SPACES                 TO RANGE-SELECTION
              PERFORM WRITE-DETAIL-LINE
           ELSE
              MOVE "BEGINNING BRANCH   :" TO RANGE-LINE
              MOVE BEG-BRANCH             TO RANGE-SELECTION
              PERFORM WRITE-DETAIL-LINE
              MOVE "ENDING BRANCH      :" TO RANGE-LINE
              MOVE END-BRANCH             TO RANGE-SELECTION
              PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "BEGINNING CLASS    :" TO RANGE-LINE.
           MOVE BEG-CLASS              TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "ENDING CLASS       :" TO RANGE-LINE.
           MOVE END-CLASS              TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "PRINT SEQUENCE     :" TO RANGE-LINE.
           MOVE BRSEQ                  TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "7 DAYS FROM NXT DUE:" TO RANGE-LINE.
           MOVE NXT-DUE-FLAG           TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "COLLECTOR BREAK    :" TO RANGE-LINE.
           MOVE COLLECTOR-FG           TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "TOTALS / RANGES    :" TO RANGE-LINE.
           MOVE TOTALS-FG              TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "REPORT TYPE        :" TO RANGE-LINE.
           MOVE REPORT-TYPE           TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.

      ******************************************************************
       PRINT-COLLECTION-SUMMARY SECTION.
      ******************************************************************
           PERFORM ZERO-TT-TOTALS.
           PERFORM GET-PORTFOLIO-TOTALS.

           PERFORM ACCUM-AGEING-TOTALS
              VARYING LTYP FROM 1 BY 1 UNTIL LTYP > 2
                AFTER AGE FROM 1 BY 1 UNTIL AGE > 10
                AFTER TYP FROM 1 BY 1 UNTIL TYP > 3.

           MOVE 1 TO TYP.
           MOVE "********* CONTRACTUAL DELINQUENCY *********"
                                  TO S-AGETYPE.
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "DIRECT  LOAN"  TO S-DESC.
           MOVE "SALES  FINANCE"  TO S-DESC2.
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE SUMMARY1 TO DETAIL-LINE.
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE 2 TO LN-FEED.
           PERFORM PRINT-DETAIL-SUM
                  VARYING AGE FROM 1 BY 1 UNTIL AGE > 10.
           MOVE 2 TO LN-FEED.
           PERFORM PRINT-SUM-TOTALS.
           MOVE "  ********* RECENCY DELINQUENCY *********"
                                TO S-AGETYPE.
           MOVE 4 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "DIRECT  LOAN"  TO S-DESC.
           MOVE "SALES  FINANCE"  TO S-DESC2.
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE SUMMARY1 TO DETAIL-LINE.
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE 2 TO LN-FEED.
           PERFORM PRINT-DETAIL-SUM
                 VARYING AGE FROM 1 BY 1 UNTIL AGE > 10
                AFTER TYP FROM 2 BY 1 UNTIL TYP > 3.
           MOVE 2 TO TYP.
           MOVE 2 TO LN-FEED.
           PERFORM PRINT-SUM-TOTALS.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE 3 TO TYP.
           MOVE 1 TO LN-FEED.
           PERFORM PRINT-SUM-TOTALS.

      ******************************************************************
       ZERO-TT-TOTALS SECTION.
      ******************************************************************
           MOVE 1 TO TYP.

       NEXT-TT-TOTAL.
           MOVE 0 TO TT-NO(1 TYP) TT-NO(2 TYP).
           MOVE 0 TO TT-BAL(1 TYP) TT-BAL(2 TYP).
           MOVE 0 TO TT-INT(1 TYP) TT-INT(2 TYP).
           MOVE 0 TO TT-OTH(1 TYP) TT-OTH(2 TYP).
           MOVE 0 TO TT-TDUE(1 TYP) TT-TDUE(2 TYP).
           MOVE 0 TO TT-UNE(1 TYP) TT-UNE(2 TYP).
           MOVE 0 TO TT-ERN(1 TYP) TT-ERN(2 TYP).
           MOVE 0 TO TT-IER(1 TYP) TT-IER(2 TYP).

           IF TYP < 3
              ADD 1 TO TYP
              GO TO NEXT-TT-TOTAL.

      ******************************************************************
       ZERO-TOTAL-TBL SECTION.
      ******************************************************************
           MOVE 1 TO AGE TYP.

       NEXT-TOTAL-TBL.
           MOVE 0 TO T-NO(1 LEV AGE TYP) T-NO(2 LEV AGE TYP).
           MOVE 0 TO T-BAL(1 LEV AGE TYP) T-BAL(2 LEV AGE TYP).
           MOVE 0 TO T-INT(1 LEV AGE TYP) T-INT(2 LEV AGE TYP).
           MOVE 0 TO T-OTH(1 LEV AGE TYP) T-OTH(2 LEV AGE TYP).
           MOVE 0 TO T-TDUE(1 LEV AGE TYP) T-TDUE(2 LEV AGE TYP).
           MOVE 0 TO T-UNE(1 LEV AGE TYP) T-UNE(2 LEV AGE TYP).
           MOVE 0 TO T-ERN(1 LEV AGE TYP) T-ERN(2 LEV AGE TYP).
           MOVE 0 TO T-IER(1 LEV AGE TYP) T-IER(2 LEV AGE TYP).

           IF TYP < 3
              ADD 1 TO TYP
              GO TO NEXT-TOTAL-TBL.

           IF AGE < 10
              ADD 1 TO AGE
              MOVE 1 TO TYP
              GO TO NEXT-TOTAL-TBL.

      ******************************************************************
       ACCUM-AGEING-TOTALS SECTION.
      ******************************************************************
           ADD T-NO(LTYP LEV AGE TYP) TO TT-NO(LTYP TYP).
           ADD T-BAL(LTYP LEV AGE TYP) TO TT-BAL(LTYP TYP).
           ADD T-INT(LTYP LEV AGE TYP) TO TT-INT(LTYP TYP).
           ADD T-OTH(LTYP LEV AGE TYP) TO TT-OTH(LTYP TYP).
           ADD T-TDUE(LTYP LEV AGE TYP) TO TT-TDUE(LTYP TYP).
           ADD T-UNE(LTYP LEV AGE TYP) TO TT-UNE(LTYP TYP).
           ADD T-ERN(LTYP LEV AGE TYP) TO TT-ERN(LTYP TYP).
           ADD T-IER(LTYP LEV AGE TYP) TO TT-IER(LTYP TYP).

      ******************************************************************
       GET-PORTFOLIO-TOTALS SECTION.
      ******************************************************************

      * IF GRAND TOTALS THEN MOVE ACCUMULATED TOTALS TO PORTFOLIO TOTALS
           IF TOTAL-FG = "Y"
              MOVE ACCUM-DL-AMT TO  PORT-AMT(1)
              MOVE ACCUM-DL-NO  TO  PORT-NO(1)
              MOVE ACCUM-SF-AMT TO  PORT-AMT(2)
              MOVE ACCUM-SF-NO  TO  PORT-NO(2)
              GO TO EXIT-PORTFOLIO-TOTALS.

           IF GROUP-FLAG = "Y"
              IF ENT-GROUP = HOLD-GROUP
                 GO TO EXIT-PORTFOLIO-TOTALS
              END-IF
              MOVE ENT-GROUP TO HOLD-GROUP SUMMBY-BRNO
           ELSE
              IF BR-NO = HOLD-BRNO
                 GO TO EXIT-PORTFOLIO-TOTALS
              END-IF
              MOVE BR-NO TO HOLD-BRNO SUMMBY-BRNO.

           MOVE AGE-DATE TO SUMMBY-DATE.
      * TYPE 1 : REC + O2.
           MOVE 1 TO SUMMBY-TYPE.
           MOVE BEG-CLASS TO SUMMBY-BEG-CLASS.
           MOVE END-CLASS TO SUMMBY-END-CLASS.
           MOVE "CL/SUMMBY" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH SUMMBY-LINK CRWKBY-REC.
           CANCEL FORM-PROGX.

           COMPUTE PORT-AMT(2) =
               CRWKBY-AMT(2) + CRWKBY-AMT(3)
           COMPUTE PORT-NO(2) =
               CRWKBY-NO(2) + CRWKBY-NO(3).

            MOVE CRWKBY-AMT(1) TO PORT-AMT(1).
            MOVE CRWKBY-NO(1)  TO PORT-NO(1).

      * IF NEW GROUP OR NEW BRANCH THEN ACCUMULATE TOTALS.
           ADD PORT-AMT(1) TO ACCUM-DL-AMT.
           ADD PORT-NO(1)  TO ACCUM-DL-NO.
           ADD PORT-AMT(2) TO ACCUM-SF-AMT.
           ADD PORT-NO(2)  TO ACCUM-SF-NO.

       EXIT-PORTFOLIO-TOTALS.
           EXIT.

      ******************************************************************
       PRINT-DETAIL-SUM SECTION.
      ******************************************************************
           IF T-NO(1 LEV AGE TYP) = 0 AND
              T-NO(2 LEV AGE TYP) = 0
              IF T-BAL(1 LEV AGE TYP) = 0 AND
                 T-BAL(2 LEV AGE TYP) = 0
                 IF T-TDUE(1 LEV AGE TYP) = 0 AND
                    T-TDUE(2 LEV AGE TYP) = 0
                               GO TO END-PRINT-DETAIL-SUM.

           IF TYP = 3
              MOVE " PAID-AHEAD:" TO S-NAME
           ELSE
              MOVE AGE-DESC(AGE) TO S-NAME.

           COMPUTE P-AGE = AGE + 1.
           COMPUTE M-AGE = AGE - 1.

           PERFORM PRINT-DET-SUM
                    VARYING LTYP FROM 1 BY 1 UNTIL LTYP > 2.

           GO TO PRINT-DET-SUM-EXIT.

       PRINT-DET-SUM.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *--NUMBER----------
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF T-NO(LTYP LEV AGE TYP) NOT = 0
              MOVE T-NO(LTYP LEV AGE TYP) TO S-NO(LTYP)
              IF TYP NOT = 3
                 IF PORT-NO(LTYP) NOT = 0
                    COMPUTE S-NO-P(LTYP) ROUNDED =
                        (T-NO(LTYP LEV AGE TYP) /
                                     PORT-NO(LTYP)) * 100
                 ELSE
                    MOVE 0 TO S-NO-P(LTYP).

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *--BALANCE-DUE------
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

           IF T-BAL(LTYP LEV AGE TYP) NOT = 0
              MOVE T-BAL(LTYP LEV AGE TYP) TO S-BAL(LTYP)
              IF TYP NOT = 3
                 IF PORT-AMT(LTYP) NOT = 0
                    COMPUTE S-BAL-P(LTYP) ROUNDED =
                        (T-BAL(LTYP LEV AGE TYP) /
                                 PORT-AMT(LTYP)) * 100
                 ELSE
                    MOVE 0 TO S-BAL-P(LTYP).

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *---TODAY DUE-----------
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

           IF T-TDUE(LTYP LEV AGE TYP) NOT = 0
              MOVE T-TDUE(LTYP LEV AGE TYP) TO S-TDUE(LTYP)
              IF TYP NOT = 3
                 IF PORT-AMT(LTYP) NOT = 0
                    COMPUTE S-TDUE-P(LTYP) ROUNDED =
                        (T-TDUE(LTYP LEV AGE TYP) /
                                   PORT-AMT(LTYP)) * 100
                 ELSE
                    MOVE 0 TO S-TDUE-P(LTYP).

       PRINT-DET-SUM-EXIT.
              PERFORM WRITE-DETAIL-LINE.

       END-PRINT-DETAIL-SUM.
           EXIT.

      ******************************************************************
       PRINT-SUM-TOTALS SECTION.
      ******************************************************************
           IF TYP = 3
              MOVE " PAID-AHEAD:" TO S-NAME
           ELSE
              MOVE "      TOTAL:" TO S-NAME.

           MOVE TT-NO(1 TYP) TO S-NO(1).
           MOVE TT-BAL(1 TYP) TO S-BAL(1).
           MOVE TT-TDUE(1 TYP) TO S-TDUE(1).
           MOVE TT-NO(2 TYP) TO S-NO(2).
           MOVE TT-BAL(2 TYP) TO S-BAL(2).
           MOVE TT-TDUE(2 TYP) TO S-TDUE(2).
           PERFORM WRITE-DETAIL-LINE.

           IF TYP NOT = 3
              IF TT-NO(1 TYP) NOT = 0
                 IF TT-BAL(1 TYP) NOT = 0
                     MOVE "GROUP TOTAL:" TO S-NAME
                     MOVE PORT-NO(1) TO S-NO(1)
                     MOVE PORT-AMT(1) TO S-BAL(1)
                 END-IF
              END-IF
              IF TT-NO(2 TYP) NOT = 0
                 IF TT-BAL(2 TYP) NOT = 0
                     MOVE "GROUP TOTAL:" TO S-NAME
                     MOVE PORT-NO(2) TO S-NO(2)
                     MOVE PORT-AMT(2) TO S-BAL(2)
                 END-IF
              END-IF
              PERFORM WRITE-DETAIL-LINE.

      ******************************************************************
       HEAD-PAGE SECTION.
      ******************************************************************

           ADD 1 TO PAGE-NUMBER.
           MOVE PAGE-NUMBER TO H-PAGE-NO.
           PERFORM GET-TIME.
           MOVE T-HH TO H-HR.
           MOVE T-MM TO H-MIN.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
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
           MOVE LN-COLLECTOR TO H-COLCTID.

      *    IF S-SUMNAME = "GRAND TOTALS:"
           IF TOTAL-FG = "Y"
              MOVE SPACES TO H-COLCTID.

           IF (COLLECTOR-FG = "Y")
              MOVE HEAD4X TO PR-REC
              MOVE 1 TO LN-FEED
              PERFORM WRITE-PR-REC.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF (S-SUMNAME = "GRAND TOTALS:") OR
              (S-SUMNAME = "COLLECTOR TOTALS:") OR
              (S-SUMNAME = "BRANCH    TOTALS:")
              GO TO END-HEAD-PAGE.

           IF REPORT-TYPE = "L"
              MOVE HEAD8 TO PR-REC
              MOVE 2 TO LN-FEED
              PERFORM WRITE-PR-REC
              MOVE HEAD9 TO PR-REC
              MOVE 1 TO LN-FEED
              PERFORM WRITE-PR-REC
           ELSE
              MOVE HEAD5 TO PR-REC
              MOVE 2 TO LN-FEED
              PERFORM WRITE-PR-REC

              MOVE HEAD6 TO PR-REC
              MOVE 1 TO LN-FEED
              PERFORM WRITE-PR-REC.

       END-HEAD-PAGE.
           MOVE 8 TO LN-COUNT.
           MOVE 2 TO LN-FEED.


      ******************************************************************
       COLLECTOR-TOTALS SECTION.
      ******************************************************************

           IF ( TOTAL-TBL(1,1) = TOTAL-TBL(1,3) ) AND
              ( TOTAL-TBL(2,1) = TOTAL-TBL(2,3) )
              GO TO COLLECTOR-TOTALS-END.

           MOVE EXT-CONAME-RPT-LINES        TO LN-COUNT.
           MOVE 1                   TO LN-FEED.
           MOVE "COLLECTOR TOTALS:" TO S-SUMNAME.
           PERFORM WRITE-DETAIL-LINE.

           MOVE 1                   TO LEV.
           PERFORM PRINT-COLLECTION-SUMMARY.

       COLLECTOR-TOTALS-END.

           MOVE 1            TO LEV.
           PERFORM ZERO-TOTAL-TBL.
           MOVE EXT-CONAME-RPT-LINES TO LN-COUNT.
           MOVE 1            TO LN-FEED.

       COLLECTOR-TOTALS-EXIT.
           EXIT.

      ******************************************************************
       BRANCH-TOTALS SECTION.
      ******************************************************************
           MOVE EXT-CONAME-RPT-LINES TO LN-COUNT.
           MOVE 1 TO LN-FEED.
           MOVE "BRANCH    TOTALS:" TO S-SUMNAME.
           PERFORM WRITE-DETAIL-LINE.
           MOVE 3 TO LEV.
           PERFORM PRINT-COLLECTION-SUMMARY.

       END-BRANCH-TOTALS.
           MOVE 3 TO LEV.
           PERFORM ZERO-TOTAL-TBL.
           MOVE EXT-CONAME-RPT-LINES TO LN-COUNT.
           MOVE 1 TO LN-FEED.

      ******************************************************************
       GRAND-TOTALS SECTION.
      ******************************************************************
           MOVE "Y" TO TOTAL-FG.
           MOVE SPACES TO H-BRNO.
           MOVE "** ALL BRANCHES **" TO H-BRDESC.
           MOVE "** ALL COLLECTORS **" TO H-COLDESC.
           MOVE EXT-CONAME-RPT-LINES TO LN-COUNT.
           MOVE 1 TO LN-FEED.
           MOVE "GRAND TOTALS:" TO S-SUMNAME.
           PERFORM WRITE-DETAIL-LINE.
           MOVE 2 TO LEV.
           PERFORM PRINT-COLLECTION-SUMMARY.

       END-GRAND-TOTALS.
           EXIT.

      *************************************************************
       CLASS-SCAN SECTION.
      *************************************************************
           MOVE 0 TO LCSCAN-KEY.
           MOVE " " TO LCSCAN-VERIFY.
           MOVE LCSCAN-BUF TO SCAN-BUF.
           PERFORM CALL-WINDOW-PROGRAM.
           MOVE SCAN-BUF TO LCSCAN-BUF.
           IF SCAN-STATUS NOT = "00" OR LCSCAN-KEY = 0
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

      ******************************************************************
       CHECK-RENEWABILITY SECTION.
      ******************************************************************
           MOVE "Y" TO RENEWABLE-FG.
           IF SP-RENEWABLE-PAYS = 0
              IF SP-RENEWABLE-DAYS = 0
                 MOVE "N" TO RENEWABLE-FG
                 GO TO EXIT-CHECK-RENEWABILITY.
           MOVE LN-1STPYDATE TO PDTH-DATE-WORK.
           PERFORM PAID-THRU-CALCULATION.
           IF SP-RENEWABLE-PAYS = -1
              COMPUTE WK-RENEWABLE-PAYS = LN-ORGTERM / 3
              IF PDTH-PAYMENTS < WK-RENEWABLE-PAYS
                 MOVE "N" TO RENEWABLE-FG
                 GO TO EXIT-CHECK-RENEWABILITY
              END-IF
           ELSE
           IF PDTH-PAYMENTS < SP-RENEWABLE-PAYS
              MOVE "N" TO RENEWABLE-FG
              GO TO EXIT-CHECK-RENEWABILITY.

           MOVE LN-LOANDATE     TO NUM-DATE.
           MOVE HOLD-TODAY-DATE TO SYS-DATE.
           PERFORM TIM.
           IF SP-RENEWABLE-DAYS = -1
              COMPUTE WK-RENEWABLE-DAYS = (LN-ORGTERM / 3) * 30 + 1
              IF ELAPSED-DAYS < WK-RENEWABLE-DAYS
                 MOVE "N" TO RENEWABLE-FG
                 GO TO EXIT-CHECK-RENEWABILITY
              END-IF
           ELSE
           IF ELAPSED-DAYS < SP-RENEWABLE-DAYS
              MOVE "N" TO RENEWABLE-FG.
       EXIT-CHECK-RENEWABILITY.
           EXIT.

      ******************************************************************
       GET-AVAILABLE-CREDIT SECTION.
      ******************************************************************

           IF BR-AVAIL-CREDIT = "Y"
                     AND (LN-CREDITLIM NOT = 0)
                      AND (LN-BNKRPTDATE = 0)
              MOVE HOLD-TODAY-DATE TO AVCR-PAYDATE
              MOVE LN-MAKERCD(1) TO AVCR-MAKERCD
              PERFORM COMPUTE-AVAILABLE-CREDIT
           ELSE
           IF BR-AVAIL-CREDIT = "S"
                     AND (LN-CREDITLIM NOT = 0)
                      AND (LN-BNKRPTDATE = 0)
              MOVE HOLD-TODAY-DATE TO AVCR-PAYDATE
              MOVE LN-MAKERCD(1)   TO AVCR-MAKERCD
              PERFORM COMPUTE-AVAILABLE-CREDIT
           ELSE
              MOVE 0 TO AVCR-AVAILCREDIT.

      ******************************************************************
      *
      *     C O P Y    L I B R A R Y    S E C T I O N S
      *
      ******************************************************************
       COPY "LIBGB/GPENV.CPY".
       COPY "LIBLP/LPITRM.CPY".
       COPY "LIBLP/LPAPRS.CPY".
       COPY "LIBLP/LPAPRZ.CPY".
       COPY "LIBLP/LPUPER.CPY".
       COPY "LIBGB/DATER.CPY".
       COPY "LIBLP/SETCTBL.CPY".
       COPY "LIBLP/LPSTAT.CPY".
       COPY "LIBGB/SPPDTH.CPY".
       COPY "LIBLP/LPSCHD.CPY".
       COPY "LIBLP/LPLCAS.CPY".
       COPY "LIBLP/LPLCHG.CPY".
       COPY "LIBLP/LPLCAP.CPY".
       COPY "LIBLP/LPMDTE.CPY".
       COPY "LIBLP/LPXDTE.CPY".
       COPY "LIBLP/LPPOFF.CPY".
       COPY "LIBLP/LPPOF2.CPY".
       COPY "LIBLP/LPIBPC.CPY".
       COPY "LIBLP/LPPDUE.CPY".
       COPY "LIBGB/RBACT.CPY".
       COPY "LIBLP/LPCDEL.CPY".
       COPY "LIBLP/LPAVCR.CPY".
       COPY "LIBLP/LPTDEL.CPY".
       COPY "LIBLP/DEFPOL.CPY".
       COPY "LIBLP/LPRATE.CPY".
       COPY "LIBGB/REBATE.CPY".
       COPY "LIBLP/LPAMTS.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/AGEING.CPY".
       COPY "LIBLP/LPUPWK.CPY".
       COPY "LIBGB/TIMEIO.CPY".
       COPY "LIBLP/LPCEPP.CPY".
       COPY "LIBWI/PFSWINDOWS.CPY".
       COPY "LIBGB/GRWORK.CPY".
       COPY "LIBGB/GRSERCH.CPY".
       COPY "LIBGB/BRSECURE.CPY".
       COPY "LIBLP/ADDONSPR.CPY".

      **************************************************
       FORM-RESET SECTION.
      **************************************************
           MOVE EXT-CONAME-CONAME TO LASTPY-CONAME.
           MOVE EXT-CONAME-SYSDATE TO LASTPY-SYSDATE.
           DISPLAY LASTPY-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE LASTPY-FORM-FIELDS TO WR-BUF.
           MOVE LASTPY-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      **************************************************
       SETUP-LINK-INFO SECTION.
      **************************************************
           MOVE LASTPY-HELP--FILE TO HELP-LINK-FILE.
           MOVE LASTPY-HELP--DIR TO HELP-LINK-DIR.

      **************************************************
       VDU-CONVERT-FIELD SECTION.
      **************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBLP/LASTPY_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ******************************************************************
       MISC-PARA SECTION.
      ******************************************************************
       COPY "LIBGB/GETPR.CPY".
       COPY "LIBGB/MKTEMP.CPY".
       COPY "LIBGB/RMFILE.CPY".
       COPY "LIBGB/ACCESS.CPY".
       SET-BRANCH-HEADINGS.
           MOVE BR-NO TO H-BRNO.
           MOVE BR-NAME TO H-BRDESC.
       SET-COLLECTOR-HEADINGS.
           MOVE OLD-COLCTID-CD TO H-COLCTID.
           MOVE SPACES TO H-COLDESC.
       SEND-LEGEND.
           MOVE MESS TO WR-BUF.
           MOVE MESS-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.
       INVALID-RANGE.
           MOVE "INVALID RANGE" TO MESS.
           PERFORM SEND-MESS.
       PROFILE-DASH.
           INSPECT PROFILE-X REPLACING ALL "9" BY "-".
       NUMBW-TO-ALPBW.
           MOVE NUM-BW1 TO ALP-BW1.
           MOVE NUM-BW2 TO ALP-BW2.
           MOVE NUM-BW3 TO ALP-BW3.
           MOVE "-" TO ALP-HYPHEN1 ALP-HYPHEN2.
       SEND-PHONE.
           MOVE PN-123 TO PA-123.
           MOVE PN-456 TO PA-456.
           MOVE PN-7890 TO PA-7890.
       CONDITIONAL-PRINT.
           IF DETAIL-LINE NOT = " "
              MOVE 1 TO LN-FEED
              PERFORM WRITE-DETAIL-LINE.
       GET-SP.
           MOVE LN-ORGST    TO SP-ORGST.
           MOVE LN-SPRCLASS TO SP-SPRCLASS.
           MOVE LN-SUBCLASS TO SP-SUBCLASS.
           MOVE LN-LAWCODE  TO SP-LAWCODE.
           IF LPWSSP-SP1-KEY NOT = SP1-KEY
              MOVE SP1-KEY TO LPWSSP-SP1-KEY
              PERFORM READ-SP1-FILE
              IF IO-FG NOT = 0
                 GO TO IO-ERROR.

      ******************************************************************
       IO-ROUTINE SECTION.
      ******************************************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBLP/LPSPGS_SQL.CPY".
       COPY "LIBLP/LPLTPGS_SQL.CPY".
       COPY "LIBLP/LPLTIGS_SQL.CPY".
       COPY "LIBLP/LPLPGS_SQL.CPY".
       COPY "LIBLP/LPLNGS_SQL.CPY".
       COPY "LIBLP/LPLMGS_SQL.CPY".
       COPY "LIBLP/LPBWGS_SQL.CPY".
       COPY "LIBGB/GBGRGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBGBGS_SQL.CPY".
       COPY "LIBGB/GBPRINT.CPY".
       COPY "LIBLP/LPBW1RN.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBGB/GBGBRN.CPY".
       COPY "LIBGB/GBGR1RN.CPY".
       COPY "LIBLP/LPSP1RN.CPY".
       COPY "LIBLP/LPLP1RN.CPY".
       COPY "LIBLP/LPLM1RN.CPY".
       COPY "LIBGB/GBWKIN.CPY".
       COPY "LIBLP/LPLN1RN.CPY".
       COPY "LIBLP/LPLTP1RN.CPY".
       COPY "LIBLP/LPLTI1RN.CPY".
       COPY "LIBLP/LPLN3RN.CPY".
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
              MOVE "NO RECORDS FOUND FOR SELECTED RANGE" TO MESS
              PERFORM SEND-MESS.

           MOVE MKTEMP-BUF TO ACCESS-BUF.
           PERFORM REMOVE-WORKFILE.

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
              MOVE EXIT-PATH TO FORM-NAM
              MOVE FORM-PATH TO FORM-PATHNAME.

      *----------------------------------------------------------------
       STOP-RUN.
           IF EXT-ROUTE-BUF = "00" OR "40" OR "50"
              DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN
              DESTROY LASTPY-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.
           EXIT PROGRAM.
