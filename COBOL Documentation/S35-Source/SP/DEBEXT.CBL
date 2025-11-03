       IDENTIFICATION DIVISION.
       PROGRAM-ID.   DEBEXT.
       DATE-WRITTEN. 12/28/2015.
      ******************************************************************
      *
      *                      (LP)
      *    DESCRIPTION:  MONTHLY TRANSACTION REFNO EXTRACT/REPORT
      *                  TOOK FROM EMTRRG, ADDED EXTRACT FILE
      *                  WORLD WANTED EXTRACT FOR "DEBIT" PAYMENTS WHICH
      *                  HAVE "DEBIT" IN THE REFNO
      *
      * FIXED LENGTH ASCII 
      * CREATE USR/EXTRACT/ACT/TR(REFNO)   RSZ 37   INCLUDES CR,LF
      *
      *=================================================================
      * REV:
      *  151228 BAH COPIED EMTRRG TO DEBEXT, WORLD PR# 1128
      *  160108 BAH ONLY INCLUDE POSTING BRANCH IN THE EXTRACT FILE, ONLY
      *             INCLUDE CERTAIN TRCD'S AND REMOVE CLASS TOTALS, PR#1128A
      * KEC 2016.1028 CHANGED SAVE-TR1-KEY AND TEST-TRP1-KEY TO HAVE A
      *               PIC X(27) FOR QUICKER VERIFICATION.
      * BAH 2016.1103 CHANGE TILLNO TO 9(2)
      * BAH 170829 ACTIVATE 8 DIGIT DATES, PD0006
      * BAH 20191002 REMOVED ACCESS-CALL TRPFILE
      * BLM 20210810 REMOVE TP-CODE, TP-POOLID NO LONGER IN KEY
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "LIBGB/GBFSPR.CPY".
       COPY "LIBGB/GBFSWKI.CPY".
           SELECT WK2-FILE ASSIGN WK2-PATH
                  ORGANIZATION INDEXED
                  ACCESS DYNAMIC
                  LOCK MODE AUTOMATIC WITH LOCK ON RECORD
                  RECORD KEY WK2-KEY
                  FILE STATUS FILE-STAT.
           SELECT DEBIT-FILE ASSIGN TO DEBIT-PATH
                           ORGANIZATION RELATIVE
                           ACCESS DYNAMIC
                           LOCK MODE AUTOMATIC WITH LOCK ON RECORD
                           RELATIVE KEY DEBIT-KEY
                           FILE STATUS FILE-STAT.

       I-O-CONTROL.
       DATA DIVISION.
       FILE SECTION.
       COPY "LIBGB/GBFDPR.CPY".
       FD  WK-FILE
           LABEL RECORDS ARE STANDARD.
       01  WK-REC.
           03  WK-KEY.
               05  WK-CODE  PIC X.
               05  WK-BRNO  PIC 9(4).
               05  WK-CLASS PIC 9(3).
               05  WK-TRCD  PIC XX.
           03  WK-NO        PIC S9(7)  COMP-3.
           03  WK-TRAMT     PIC S9(9)V99  COMP-3.
           03  WK-INTDUE    PIC S9(9)V99  COMP-3.
           03  WK-APOTH     PIC S9(9)V99  COMP-3.
           03  WK-APINT     PIC S9(9)V99  COMP-3.
           03  WK-APLC      PIC S9(9)V99  COMP-3.
           03  WK-APCUR     PIC S9(9)V99  COMP-3.
           03  WK-CHECKS    PIC S9(9)V99  COMP-3.
           03  WK-EARNINGS  PIC S9(9)V99  COMP-3.
           03  WK-DLPROC    PIC S9(9)V99  COMP-3.
           03  WK-APESCROW  PIC S9(9)V99  COMP-3.
       01  WK-GL-REC.
           03  WK-GL1-KEY.
               05  FILLER   PIC X.
               05  WK-GLNO  PIC 9(11)     COMP-3.
               05  FILLER   PIC XX.
           03  WK-GLAMT     PIC S9(7)V99  COMP-3.
           03  FILLER       PIC X(53).

      *****************************************************************
      *  INDEXED TRFILE BY BRANCH, CLASS, ACCOUNT
      *****************************************************************
       FD  WK2-FILE
           LABEL RECORDS ARE STANDARD.
       01  WK2-REC.
           03  WK2-KEY.
               05  WK2-BRANCH            PIC 9(4).
               05  WK2-CLASS             PIC 9(3).
               05  WK2-ACCT              PIC 9(8).
               05  WK2-DATE              PIC 9(8).
      *        05  WK2-CODE              PIC X.
               05  WK2-LINE              PIC 9(3).


      **************************************************
      *  DEBIT EXTRACT FILE                 RSZ 37
      **************************************************
       FD  DEBIT-FILE
           LABEL RECORDS ARE STANDARD.
       01  DEBIT-REC.
           03  DEB-POSTING-BRNO            PIC 9999.
           03  DEB-PAYMENT-DATE            PIC 9(8).
           03  DEB-REFNO                   PIC X(5).
           03  DEB-OWNBR                   PIC 9(4).
           03  DEB-NUMBER                  PIC 9(6).
           03  DEB-TRAMT                   PIC 9999.99-.
           03  DEB-CR                      PIC X.
           03  DEB-LF                      PIC X.

      *****************************************************************
      *
      *    W O R K I N G - S T O R A G E    S E C T I O N
      *
      *****************************************************************
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)SP/DEBEXT.CBL S35 03/28/24-08:43:56 >".

       COPY "LIBLP/LP01TRP.CPY".
       COPY "LIBLP/LP01TRP_SQL.CPY".
       COPY "LIBLP/LPWSTRP.CPY".
       COPY "LIBLP/LP01BW.CPY".
       COPY "LIBLP/LP01BW_SQL.CPY".
       COPY "LIBLP/LPWSBW.CPY".
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBGB/GB01GR.CPY".
       COPY "LIBGB/GB01GR_SQL.CPY".
       COPY "LIBGB/GBWSGR.CPY".
       COPY "LIBLP/LP01LN.CPY".
       COPY "LIBLP/LP01LN_SQL.CPY".
       COPY "LIBLP/LPWSLN.CPY".
       COPY "LIBLP/LP01SP.CPY".
       COPY "LIBLP/LP01SPA.CPY".
       COPY "LIBLP/LP01SPB.CPY".
       COPY "LIBLP/LP01SPC.CPY".
       COPY "LIBLP/LP01SP_SQL.CPY".
       COPY "LIBLP/LPWSSP.CPY".
      *-----------------------------------------------------------------
       01  PR-IFN               PIC X(2)     VALUE "PR".
       01  WK2-IFN              PIC X(3)     VALUE "WK2".
       01  WK-IFN               PIC X(2)     VALUE "WK".
       01  WK2-PATH             PIC X(35).
       01  DEBIT-IFN            PIC X(5)     VALUE "DEBIT".
       01  DEBIT-KEY            PIC 9(9)     VALUE 0.
       01  DEBIT-PATH.
           03  DEBIT-PATH-FIXED     PIC X(13) VALUE "/USR/EXTRACT/".
           03  DEBIT-PATH-NAME      PIC X(12) VALUE SPACES.

 
      *****************************************************************
      *
      *   LINE WORKERS
      *
      *****************************************************************
       01  LN-WORKERS.
           03  LN-FEED          PIC 999      VALUE 0.
           03  LN-COUNT         PIC 999      VALUE 99.
           03  LN-WK            PIC 999      VALUE 0.
       01  PAGE-NUMBER          PIC 9(4)     VALUE 0.
 
       01  ER-FEED              PIC 9        VALUE 1.
 
      *****************************************************************
      *
      *   MISC WORKERS     
      *
      *****************************************************************
       01  HOLD-EARNINGS        PIC S9(7)V99  COMP-3 VALUE 0.
       01  HOLD-TP-OTHBR-FG     PIC X                VALUE "?".
       01  HOLD-TP-ACCTNO       PIC 9(8)      COMP-3 VALUE 99999999.
       01  HOLD-TP-SSNO         PIC 9(10)     COMP-3 VALUE 9999999999.

       01  RECORD-PL-STATUS     PIC X(1)      VALUE SPACES.
           88  RECORD-IN-PL-NO                VALUE "N".
           88  RECORD-IN-PL-YES               VALUE "Y".

       01  SUB                  PIC 9(2)      COMP-3.
      *--> SUB1 AND SUB2 USED FOR CLEARING PRINTING TOTAL TABLE.
       01  SUB1                 PIC 9(2)      COMP-3.
       01  SUB2                 PIC 9(2)      COMP-3.
       01  LEV                  PIC 99        COMP-3.
       01  HOLD-BRNO            PIC 9(4)    VALUE 0.
       01  HOLD-CLASS           PIC 9(3)    VALUE 0.
       01  RECORDS-FOUND        PIC X       VALUE " ".
           88  NO-RECORDS-FOUND             VALUE "N".
       01  SUMMARY-FG           PIC X       VALUE "N".
       01  NAME-WORKER          PIC X(34).
       01  FIRST-NAME           PIC X(16).

       01  FIRST-TIME           PIC X        VALUE "Y".
       01  MSEL-LIST            PIC X(5)     VALUE "MSEL.".
       01  MESS-LIST            PIC X(5)     VALUE "MESS.".
       01  SAVE-TP-REC-FG       PIC X VALUE SPACES.

       01  TEST-TRP1-KEY        PIC X(26)    VALUE LOW-VALUES.
       01  FILLER               REDEFINES TEST-TRP1-KEY.
           03  TEST-TRP1-KEY-MAIN.
               05  TEST-TRP1-BRNO             PIC 9(4).
               05  TEST-TRP1-DATE             PIC 9(8).
               05  TEST-TRP1-CLASS            PIC 9(3).
               05  TEST-TRP1-ACCT             PIC 9(8).
           03  TEST-TRP1-LINE                 PIC 9(3).

       01  SAVE-TRP1-KEY         PIC X(26)    VALUE LOW-VALUES.
       01  FILLER               REDEFINES SAVE-TRP1-KEY.
           03  SAVE-TRP1-KEY-MAIN.
               05  SAVE-TRP1-BRNO             PIC 9(4).
               05  SAVE-TRP1-DATE             PIC 9(8).
               05  SAVE-TRP1-CLASS            PIC 9(3).
               05  SAVE-TRP1-ACCT             PIC 9(8).
           03  SAVE-TRP1-LINE                 PIC 9(3). 

       01  CLASS-FG                          PIC X(3)   VALUE "   ".
           88  CLASS-BEG                                VALUE "BEG".
           88  CLASS-END                                VALUE "END".  

      *-----------------------------------------------------------------
       01  MESSAGE-01-LOC-PRT   PIC X(43)    VALUE
           "CANNOT DO LOCAL SPOOLING WITH GROUP TOTALS".

      ******************************************************************
      *
      *    W S - G T T - T A B L E
      *
      ******************************************************************
       01  WS-GTT-TOTAL-TABLE.
           03  WS-GTT-TABLE-GROUP-LEVEL                 OCCURS 10 TIMES.
               05  WS-GTT-NO              PIC S9(7)     COMP.
               05  WS-GTT-TRAMT           PIC S9(9)V99  COMP.
               05  WS-GTT-INTDUE          PIC S9(9)V99  COMP.
               05  WS-GTT-APOTH           PIC S9(9)V99  COMP.
               05  WS-GTT-APINT           PIC S9(9)V99  COMP.
               05  WS-GTT-APLC            PIC S9(9)V99  COMP.
               05  WS-GTT-APCUR           PIC S9(9)V99  COMP.
               05  WS-GTT-CHECKS          PIC S9(9)V99  COMP.
               05  WS-GTT-EARNINGS        PIC S9(9)V99  COMP.
               05  WS-GTT-DLPROC          PIC S9(9)V99  COMP.
               05  WS-GTT-APESCROW        PIC S9(9)V99  COMP.

      *-----------------------------------------------------------------
       01  WS-HOLD-PROMPT       PIC X(7)  VALUE SPACES.

      *****************************************************************
      *
      *  TYPE   LEV1 = POSTING FROM THIS BRANCH
      *            2 = POSTING FROM OTHER BRANCHES
      *
      *  TOTALS LEV1 = CLASS TOTALS
      *            2 = BRANCH TOTALS
      *            3 = GRAND TOTALS
      *
      *****************************************************************
       01  TOTALS-TAB   VALUE ZEROS.             
           03  TYPE-TBL        OCCURS 2.
               05  TOTAL-TBL       OCCURS 3.
                   07  T-TRAMT      PIC S9(9)V99.         
                   07  T-INTDUE     PIC S9(9)V99.         
                   07  T-APOTH      PIC S9(9)V99.          
                   07  T-APINT      PIC S9(9)V99.          
                   07  T-APLC       PIC S9(9)V99.
                   07  T-APCUR     PIC S9(9)V99.          
                   07  T-CHECKS     PIC S9(9)V99.          
                   07  T-EARNINGS   PIC S9(9)V99.
                   07  T-DLPROC     PIC S9(7)V99.
                   07  T-APESCROW   PIC S9(9)V99.
 
      *-----------------------------------------------------------------
       01  LEGEND               PIC X(70).

      *****************************************************************
      *
      *     S C R E E N   F I E L D S
      *
      *****************************************************************
       COPY "LIBGB/EOBUF_EXT.CPY".
           03  BEG-BRANCH                     PIC 9(4).
           03  END-BRANCH                     PIC 9(4).
           03  BEG-CLASS                      PIC 9(3).
           03  END-CLASS                      PIC 9(3).
           03  TILLNO                         PIC 9(2).
           03  END-DATE.
               05  END-MONTH                  PIC 9(2).
               05  END-YEAR                   PIC 9(2).
           03  EDATE                REDEFINES END-DATE
                                              PIC 9(4).

           03  DAY-OF-MONTH                   PIC 9(2).
           03  GTT-ISO                        PIC X(1).
           03  SUM-ONLY                       PIC X(1).
           03  LOCAL-PRINT-FG                 PIC X(1).
           03  GROUP-FLAG                     PIC X(1).
           03  ENT-GROUP                      PIC X(4).
           03  TOTAL-LEV-TABLE.
               05 TOTAL-LEVEL-TABLE           PIC X(1)     OCCURS 10.
           03  REVERSALS-ONLY                 PIC X(1).

           03  PL-ISO                         PIC X(1).
               88  PL-ISO-INCLUDE                 VALUE "I".
               88  PL-ISO-SKIP                    VALUE "S".
               88  PL-ISO-ONLY                    VALUE "O".

           03  PL-TYPE                        PIC X(1).
               88  PL-TYPE-ALL                    VALUE "A".
               88  PL-TYPE-PP                     VALUE "P".
               88  PL-TYPE-P2                     VALUE "2".
               88  PL-TYPE-P3                     VALUE "3".
               88  PL-TYPE-P4                     VALUE "4".
               88  PL-TYPE-P5                     VALUE "5".

           03  PL-ACTIVE                      PIC X(1).
               88  PL-ACTIVE-ACTIVE               VALUE "A".
               88  PL-ACTIVE-INACTIVE             VALUE "I".
               88  PL-ACTIVE-BOTH                 VALUE "B".

           03  INT-BR-ISO                     PIC X(1).
               88  INT-BR-ISO-INCLUDE             VALUE "I".
               88  INT-BR-ISO-SKIP                VALUE "S".
               88  INT-BR-ISO-ONLY                VALUE "O".
           
           03  TRAN-CODE                      PIC XX.
           03  REF-CODE                       PIC X(5).
           03  EXTRACT-FLAG                   PIC X.

       01  VERSION-CONTROL                    PIC X(1)     VALUE "A".

      *-----------------------------------------------------------------
       01  SV-BEG-BR                          PIC 9(4)     VALUE 9999.
       01  SV-END-BR                          PIC 9(4)     VALUE 9999.
       01  SV-GROUP-FLAG                      PIC X(1)     VALUE SPACES.
       01  SV-ENT-GROUP                       PIC X(4)     VALUE SPACES.
       01  SV-LOCAL-PRINT-FG                  PIC X(1)     VALUE SPACES.
       01  RPT-END-DATE                       PIC 9(8)     VALUE 0.
       01  BEGINNING-DATE                     PIC 9(8)     VALUE 0.
 
      *****************************************************************
      *
      *     P R I N T   B U F F E R S
      *
      *****************************************************************
       01  HEAD1.
           03  H-DATE           PIC X(8)     VALUE " ".
           03  FILLER           PIC XX       VALUE " ".
           03  H-TIME           PIC X(8)     VALUE " ".
           03  FILLER           PIC X(28)    VALUE " ".
           03  H-NAME           PIC X(40)    VALUE " ".
           03  FILLER           PIC X(37)    VALUE " ".
           03  FILLER           PIC X(5)     VALUE "PAGE ".
           03  H-PAGE-NO        PIC ZZZ9.
 
      *-----------------------------------------------------------------
       01  HEAD2.
           03  FILLER           PIC X(10)    VALUE "USER ID:".
           03  H-USERID         PIC X(10).
           03  FILLER           PIC X(30)    VALUE " ".
           03  H-TITLE          PIC X(43) VALUE
               "INTERBRANCH DEBIT CARD PAY REPORT/EXTRACT".
 
      *-----------------------------------------------------------------
       01  HEAD3.
           03  FILLER           PIC X(10) VALUE "BRANCH :".
           03  H-BRNO           PIC 9(4).
           03  FILLER           PIC X VALUE " ".
           03  H-BRDESC         PIC X(20).
           03  FILLER           PIC X(46) VALUE " ".
 
      *-----------------------------------------------------------------
       01  HEAD5.
           03  FILLER           PIC X(54)      VALUE
           "ACCTNO                  IB".
           03  FILLER           PIC X(37)    VALUE
           "-------- APPLIED ---------           ".
           03  FILLER           PIC X(41)    VALUE
           "     --- BALANCES --------         G/L   ".
 
      *-----------------------------------------------------------------
       01  HEAD6.
           03  FILLER           PIC X(54)     VALUE
           "  TL   DL PROC   EARNED PC PAYDATE  CD REFNO   AMOUNT ".
           03  FILLER           PIC X(37)     VALUE
           "INTEREST LATE CHG     PRIN  PDTH LCPD".
           03  FILLER           PIC X(41)     VALUE
           "     OTH      INT     PRIN      ACCT-AMT ".
 
      *-----------------------------------------------------------------
       01  HEAD6-ESCROW.
           03  FILLER           PIC X(54)     VALUE
           "       DL PROC   EARNED PC PAYDATE  CD REFNO   AMOUNT ".
           03  FILLER           PIC X(37)     VALUE
           "  INT+LC   ESCROW     PRIN  PDTH LCPD".
           03  FILLER           PIC X(41)     VALUE
           "     OTH      INT     PRIN L    ACCT-AMT ".
 
      *-----------------------------------------------------------------
       01  SUM-HEAD.
           03  SUM-HEAD-PROMPT  PIC X(10).
           03  FILLER           PIC X(29)    VALUE
           "  NO       TR-AMT   NSF CHKS ".
           03  FILLER           PIC X(45)    VALUE
           "   APP-OTH    APP-INT      APP-LC   APP-ESCR ".
           03  FILLER           PIC X(48)    VALUE
           "     APP-PRIN  REB CHECKS   EARNINGS    DL-PROC ".
 
      *-----------------------------------------------------------------
       01  GL-HEAD.
           03  FILLER           PIC X(17)    VALUE "G/L DIST  SUMMARY".
 
      *-----------------------------------------------------------------
       01  GTT-HEAD1.
           03  FILLER            PIC X(57)  VALUE SPACES.
           03  FILLER            PIC X(18)  VALUE "GROUP TOTALS TABLE".
           03  FILLER            PIC X(57)  VALUE SPACES.

      *****************************************************************
      *
      *    D E T A I L - L I N E
      *
      *****************************************************************
       01  DETAIL-LINE          VALUE SPACES.
           03  FILLER           PIC X(116).
           03  D-BRXX           PIC X(12).
           03  D-BRX            PIC 9(4).
 
      *-----------------------------------------------------------------
       01  D-LINE1 REDEFINES DETAIL-LINE.
           03  D-DISPLAY-LNNO   PIC X(16).
           03  FILLER           PIC X(03).
           03  D-NAME           PIC X(26).
           03  FILLER           PIC X.
           03  D-MAKER          PIC X.
           03  FILLER           PIC XX.
           03  D-SSNO           PIC X(11).
           03  FILLER           PIC X(2).
           03  D-DLNO           PIC ZZZZ.ZZ.
 
      *-----------------------------------------------------------------
       01  D-LINE2 REDEFINES DETAIL-LINE.
           03  FILLER           PIC X(2).
           03  D-TILL           PIC Z9.
           03  FILLER           PIC X(2).
           03  D-DLPROC         PIC ZZZZZ.ZZ-.
           03  D-EARNINGS       PIC ZZZZZ.ZZ-.
           03  FILLER           PIC X.
           03  D-IBPC           PIC X.
           03  FILLER           PIC X.
           03  D-DATE           PIC 99/99/99.
           03  FILLER           PIC X.
           03  D-TRCD           PIC XX.
           03  D-REV            PIC X.
           03  D-REFNO          PIC X(5).
           03  FILLER           PIC X.
           03  D-TRAMT          PIC ZZZZZ.ZZ-.
           03  D-APINT          PIC ZZZZZ.ZZ-.
           03  D-AP-LC-ESC      PIC ZZZZZ.ZZ-.
           03  D-APCUR          PIC ZZZZZ.ZZ-.
           03  FILLER           PIC X.
           03  D-PDTHRU-MO      PIC 99.
           03  D-PDTHRU-DAYR    PIC 99.
           03  FILLER           PIC X.
           03  D-REST-OF-LINE.
               05  D-LCPD-MO        PIC 99.
               05  D-LCPD-DAYR      PIC 99.
               05  D-OTHBAL         PIC ZZZZZ.ZZ-.
               05  D-INTBAL         PIC ZZZZZ.ZZ-.
               05  D-PRINBAL        PIC ZZZZ9.99-.
           03  D-REST-OF-LINE2 REDEFINES D-REST-OF-LINE.
               05  FILLER            PIC X(3).
               05  D2-OTHBAL         PIC ZZZZZ.ZZ-.
               05  D2-INTBAL         PIC ZZZZZ.ZZ-.
               05  D2-PRINBAL        PIC ZZZZZ9.99-.
           03  FILLER           PIC X(2).
           03  D-GLNO           PIC ZZZ9/9999999.
           03  D-GLAMT          REDEFINES D-GLNO
                                PIC ZZZZZZZZ.ZZ-.
           03  D-PAIDOFF REDEFINES D-GLNO PIC X(12).
 
      *-----------------------------------------------------------------
       01  D-LINE2A REDEFINES DETAIL-LINE.
           03  FILLER             PIC X(9).
           03  D-TAX-INSURANCE-TAX-REFUND.             
               05  FILLER         PIC X.
               05  D-TAX-REFUND-T PIC X.
               05  D-TAX-REFUND-INSTYPE PIC X(2).
           03  D-TAX-REFUND-AMOUNT  PIC ZZZZZZZ.ZZ-.
           03  FILLER             PIC X(108). 

      *-----------------------------------------------------------------
       01  D-LINE3 REDEFINES DETAIL-LINE.
           03  FILLER           PIC X(10).
           03  D-DLTAG1         PIC X(10).
           03  D-DLADJ1         PIC ZZZZZ.ZZ-.
           03  FILLER           PIC X(3).
           03  D-DLTAG2         PIC X(10).
           03  D-DLADJ2         PIC ZZZZZ.ZZ-.
           03  FILLER           PIC X(3).
           03  D-DLTAG3         PIC X(10).
           03  D-DLADJ3         PIC ZZZZZ.ZZ-.
           03  FILLER           PIC X(3).
           03  D-DLTAG4         PIC X(11).
           03  D-PVHELD         PIC ZZZZZ.ZZ-.
           03  FILLER           PIC X(3).
           03  D-DLTAG5         PIC X(11).
           03  D-PVPAID         PIC ZZZZZ.ZZ-.

      *-----------------------------------------------------------------
       01  D-LINE4 REDEFINES DETAIL-LINE.
           03  D2-GRNO          PIC X(4).
           03  D2-BRNO      REDEFINES D2-GRNO
                                PIC 9(4).
           03  FILLER           PIC XX.
           03  DS-TRCD          PIC XX.
           03  DS-NO            PIC ZZZZZZ.
           03  DS-TRAMT         PIC ZZZZZZZZZZ.ZZ-.
           03  DS-INTDUE        PIC ZZZZZZZ.ZZ-.
           03  DS-APOTH         PIC ZZZZZZZ.ZZ-.
           03  DS-APINT         PIC ZZZZZZZ.ZZ-.
           03  DS-APLC          PIC ZZZZZZZZ.ZZ-.
           03  DS-APESCROW      PIC ZZZZZZZ.ZZ-.
           03  DS-APCUR         PIC ZZZZZZZZZZ.ZZ-.
           03  DS-CHECKS        PIC ZZZZZZZZ.ZZ-.
           03  DS-EARNINGS      PIC ZZZZZZZ.ZZ-.
           03  DS-DLPROC        PIC ZZZZZZZ.ZZ-.
 
      *-----------------------------------------------------------------
       01  D-LINE5 REDEFINES DETAIL-LINE.
           03  DS-GLNO          PIC ZZZ9/9999999.
           03  FILLER           PIC XX.
           03  DS-GLAMT         PIC Z,ZZZ,ZZ9.99-.

      *-----------------------------------------------------------------
       01  RANGE-LINE REDEFINES DETAIL-LINE.
           03  FILLER           PIC X(35).
           03  RANGE-SELECTION.
               04  R-NUM        PIC Z(5)9.
               04  R-NUM-REDEF REDEFINES R-NUM.
                   05 R-CHAR    PIC X(6).

       01  D-SUMMARY REDEFINES DETAIL-LINE.
           03  FILLER           PIC X(14).
           03  SUM-TRAMT         PIC ZZZZZZZZZZ.ZZ-.
           03  SUM-INTDUE        PIC ZZZZZZZ.ZZ-.
           03  SUM-APOTH         PIC ZZZZZZZ.ZZ-.
           03  SUM-APINT         PIC ZZZZZZZ.ZZ-.
           03  SUM-APLC          PIC ZZZZZZZZ.ZZ-.
           03  SUM-APESCROW      PIC ZZZZZZZ.ZZ-.
           03  SUM-APCUR         PIC ZZZZZZZZZZ.ZZ-.
           03  SUM-CHECKS        PIC ZZZZZZZZ.ZZ-.
           03  SUM-EARNINGS      PIC ZZZZZZZ.ZZ-.
           03  SUM-DLPROC        PIC ZZZZZZZ.ZZ-.
      *****************************************************************
      *
      *     C O P Y   L I B R A R Y   S E C T I O N
      *
      *****************************************************************
       COPY "LIBLP/EMSFRG_CMD.CPY".
       COPY "LIBWI/GRSCANW.CPY".
       COPY "LIBGB/GRWORKW.CPY".
       COPY "LIBWI/GRTOTLW.CPY".
       COPY "LIBWI/LCSCANW.CPY".
       COPY "LIBGB/PASSWDW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBGB/MKTEMPW_CMD.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/DACCESSW.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/GPENVW.CPY".
       COPY "LIBLP/LPWKPATH.CPY".
       COPY "LIBLP/LPMDTEW.CPY".
       COPY "LIBLP/LPSTATW.CPY".
       COPY "LIBGB/BRSECUREW.CPY".
       COPY "LIBGB/SYSTEMW.CPY".
       COPY "LIBGB/GETPRW.CPY".
       COPY "LIBGB/EOSPOOLW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBSP/DEBEXT_DEF.CPY".
       COPY "LIBSP/DEBEXT_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

      *****************************************************************
      *
      *     P R O G R A M   C O N T R O L
      *
      *****************************************************************
       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBSP/DEBEXT_SCN.CPY".

       PROCEDURE DIVISION
           USING FORM-PATHNAME EXIT-PATHNAME.
       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
               PR-FILE WK-FILE WK2-FILE DEBIT-FILE.
       COPY "LIBGB/DECLRP.CPY".
           PERFORM CLOSE-TRP1-FILE.
           PERFORM CLOSE-SP1-FILE.
           PERFORM CLOSE-LN1-FILE.
           PERFORM CLOSE-BW1-FILE.
           PERFORM CLOSE-GR1-FILE.
           PERFORM CLOSE-BR-FILE.
           PERFORM CLOSE-PR-FILE.
           CLOSE
               WK-FILE WK2-FILE DEBIT-FILE.
       COPY "LIBGB/DECLRP2.CPY".

      ******************************************************************
       MAIN-MODULE SECTION.
      ******************************************************************
           PERFORM SQL-CONNECT.

           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "DEBEXT" TO FORM-PROG VDU-FORM-NAME.

           PERFORM GET-GPENV.

      D    STOP MESS.
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

      *-----------------------------------------------------------------
       NEXT-BRANCH-NO.
           PERFORM READ-BR-FILE-NEXT.
           IF IO-FG = 9 OR BR-NO > END-BRANCH
              GO TO END-ROUTINE.

      *-----------------------------------------------------------------
       MOVE-BRANCH-PATH-INFO.
           IF LOCAL-PRINT-FG = "Y"
              PERFORM CLOSE-PR-FILE
              PERFORM CREATE-SPOOL-DIR
              IF STAT NOT = "00"
                 GO TO NEXT-GROUP-LOC.

           MOVE EXT-FILPATH-BASE        TO FILPATH-BASE.
           MOVE BR-MACHINE              TO FILPATH-MACHINE.
           MOVE BR-DATA-PATH            TO FILPATH-DATA.

           MOVE "B"                     TO LN-PATH-OVERRIDE
                                           BW-PATH-OVERRIDE
                                           TRP-PATH-OVERRIDE.

           PERFORM OPEN-LN1-FILE.
           PERFORM OPEN-TRP1-FILE.
           PERFORM OPEN-BW1-FILE.

           PERFORM SORT-ROUTINE.

           IF EXT-EOBUF-ERRCD = "X"
              GO TO NEXT-GROUP-LOC.

           PERFORM OPEN-WK2-FILE.

      *-----------------------------------------------------------------
       NEXT-TR-REPORT.
           PERFORM READ-WK2-FILE-NEXT.
           IF IO-FG = 9
              GO TO END-OF-GROUP.

           MOVE WK2-BRANCH TO TP-BRNO.
           MOVE WK2-DATE   TO TP-DATE.
           MOVE WK2-CLASS  TO TP-CLASS.
           MOVE WK2-ACCT   TO TP-ACCTNO.
           MOVE WK2-LINE   TO TP-LINE.

           PERFORM READ-TRP1-FILE.
           IF IO-FG = 9
              MOVE "TRANSACTION RECORD MISSING SINCE SORT" TO MESS
              PERFORM SEND-MESS
              GO TO IO-ERROR.

           IF FIRST-TIME = "Y"
              MOVE TP-BRNO TO HOLD-BRNO
              MOVE TP-CLASS TO HOLD-CLASS
              PERFORM SET-BRANCH-HEADINGS.

           IF TP-BRNO NOT = HOLD-BRNO
              PERFORM CLASS-TOTALS
              PERFORM BRANCH-TOTALS
              MOVE TP-BRNO TO HOLD-BRNO
              MOVE TP-CLASS TO HOLD-CLASS
              PERFORM SET-BRANCH-HEADINGS
              IF GTT-ISO NOT = "O"
                 IF SUM-ONLY = "N"
                    PERFORM HEAD-PAGE.

           IF TP-CLASS NOT = HOLD-CLASS
              PERFORM CLASS-TOTALS
              MOVE TP-CLASS TO HOLD-CLASS.
   
           PERFORM DETAIL-LINE-PROCESS.

      * ONLY CREATE THE EXTRACT RECORD FOR THE POSTING BRANCH ("C")
      * DO NOT WANT 2 EXTRACT RECORDS
      * ONLY CREATE FOR SPECIFIC TRANSACTION CODES, DIDNT WANT REBATES, ETC.
           IF EXTRACT-FLAG = "Y"
              IF TP-OTHBR-FG = "C"
                 IF TP-TRCD = "PY" OR "PA" OR "PE" OR "PO"
                                 OR "DF" OR "D2" OR "D3"
                                  OR "D4" OR "D5" OR "D6"
                                   OR "D7" OR "D8" OR "D9"
                                     OR "SS" OR "Z2" OR "IN"
                                      OR "OV"
                 PERFORM CREATE-DEBIT-RECORD.

           MOVE 1 TO LEV.
           PERFORM CREATE-WK.
           PERFORM BRANCH-WK-TOTALS-BY-TRCD.

           GO TO NEXT-TR-REPORT.

      *-----------------------------------------------------------------
       END-OF-GROUP.
           PERFORM CLOSE-TRP1-FILE.
           PERFORM CLOSE-BW1-FILE.
           PERFORM CLOSE-LN1-FILE.
           PERFORM CLOSE-WK2-FILE.
           PERFORM OPEN-WK2-FILE-OUTPUT.

           IF ( LOCAL-PRINT-FG = "Y" )
              IF ( FIRST-TIME NOT = "Y" )
                 PERFORM CLASS-TOTALS
                 PERFORM BRANCH-TOTALS
                 MOVE "Y" TO FIRST-TIME
                 MOVE 0 TO PAGE-NUMBER
              ELSE
                 MOVE 0 TO PAGE-NUMBER.

           GO TO NEXT-GROUP-LOC.

      *-----------------------------------------------------------------
       END-ROUTINE.
           IF NO-RECORDS-FOUND
              GO TO END-PROGRAM.

           IF ( LOCAL-PRINT-FG = "N" )
              PERFORM CLASS-TOTALS
              PERFORM BRANCH-TOTALS
              PERFORM GRAND-TOTALS
              IF ( GTT-ISO NOT = "S" )
                 MOVE 99 TO LN-COUNT
                 PERFORM GROUP-TOTAL-TABLE 
                 PERFORM PRINT-RANGE-LINES
                 PERFORM GL-SUMMARY
              ELSE
                 PERFORM PRINT-RANGE-LINES
                 PERFORM GL-SUMMARY.

           GO TO END-PROGRAM.

      ******************************************************************
       INITIALIZATION SECTION.
      ******************************************************************
           MOVE DEBEXT-QUEUE-POS TO Q-POS.
           MOVE DEBEXT-COPIES-POS TO C-POS.
           MOVE DEBEXT-PRU-POS   TO P-POS.
           MOVE SPACES TO Q-BUF C-BUF P-BUF.
       COPY "LIBGB/EOINIT.CPY".

           MOVE SPACES TO RECORDS-FOUND.

           PERFORM CLEAR-T-TOTALS
                         VARYING SUB1 FROM 1 BY 1 UNTIL SUB1 > 2
                           AFTER SUB2 FROM 1 BY 1 UNTIL SUB2 > 3.
                                                   
           MOVE 0 TO PAGE-NUMBER.

      * /USR/EXTRACT/ACT/TR(XXXXX)
           CALL "C$TOLOWER" USING DEBIT-PATH-FIXED, VALUE 13.
           MOVE SPACES TO DEBIT-PATH-NAME.
           IF REF-CODE = "*****"
              MOVE "ACT/TRALL" TO DEBIT-PATH-NAME
           ELSE
              STRING "ACT/TR",REF-CODE DELIMITED BY " " INTO
                        DEBIT-PATH-NAME.
           
           PERFORM OPEN-DEBIT-FILE-OUTPUT.
           PERFORM CLOSE-DEBIT-FILE.
           PERFORM OPEN-DEBIT-FILE.

      * CREATE INDEXED KEY FILE FOR TRFILE OUTPUT
           MOVE MKTEMP-DEFAULT TO MKTEMP-BUF.
           PERFORM MKTEMP-CALL.
           IF STAT NOT = "00"
              MOVE "CANNOT MAKE WK2 SORT KEY FILE" TO MESS
              PERFORM SEND-MESS
              GO TO ENDIT.

           MOVE MKTEMP-BUF TO WK2-PATH.
           PERFORM OPEN-WK2-FILE-OUTPUT.

           MOVE MKTEMP-DEFAULT TO MKTEMP-BUF.
           PERFORM MKTEMP-CALL.
           IF STAT NOT = "00"
              MOVE "CANNOT MAKE WK SORT KEY FILE" TO MESS
              PERFORM SEND-MESS
              GO TO ENDIT.

           PERFORM OPEN-BR-FILE.
           PERFORM OPEN-GR1-FILE.
           PERFORM OPEN-SP1-FILE.

           PERFORM OPEN-WK-FILE-OUTPUT.
           PERFORM CLOSE-WK-FILE.
           PERFORM OPEN-WK-FILE.

           IF EXT-EOBUF-GROUP-FG = "Y"
              MOVE BEG-BRANCH TO SV-BEG-BR
              MOVE END-BRANCH TO SV-END-BR
              MOVE GROUP-FLAG TO SV-GROUP-FLAG
              MOVE ENT-GROUP TO SV-ENT-GROUP
              MOVE LOCAL-PRINT-FG TO SV-LOCAL-PRINT-FG
              MOVE EXT-EOBUF-BRNO TO BEG-BRANCH END-BRANCH
              MOVE "N" TO GROUP-FLAG LOCAL-PRINT-FG
              MOVE EXT-EOBUF-GROUP-PRPATH TO PRU-FILE
           ELSE
              IF ( BEG-BRANCH = 0 AND END-BRANCH = 0 )
                 MOVE BEG-BRANCH TO SV-BEG-BR
                 MOVE END-BRANCH TO SV-END-BR
                 MOVE EXT-CONAME-USER-LOC TO BEG-BRANCH
                                             END-BRANCH.

      * RPT-END-DATE IS ALREADY SET, JUST NEED A BEGINNING FOR TR
           MOVE END-MONTH TO DATE-MMDDYY-MM.
           MOVE END-YEAR  TO DATE-MMDDYY-YY.
           MOVE 01        TO DATE-MMDDYY-DD.
           PERFORM CONVERT-MMDDYY-TO-YYYYMMDD.
           MOVE DATE-YYYYMMDD TO BEGINNING-DATE.

           MOVE END-MONTH TO DATE-MMDDYY-MM.
           MOVE END-YEAR  TO DATE-MMDDYY-YY.
           MOVE 50        TO DATE-MMDDYY-DD.
           PERFORM CONVERT-MMDDYY-TO-YYYYMMDD.
           MOVE DATE-YYYYMMDD TO NDTE-DATE.
           PERFORM NEW-DATE-CALCULATION.
           MOVE NDTE-DATE     TO RPT-END-DATE.

           IF EXT-ROUTE-BUF = "00"
              MOVE "INTERBRANCH DEBIT REPORT/EXTRACT IN PROGRESS...."
                             TO WR-BUF
              MOVE MESS-LIST TO WR-LIST
              PERFORM CALL-WRFORM.

           IF ( LOCAL-PRINT-FG NOT = "Y" )
              PERFORM OPEN-PR-FILE.

           MOVE EXT-CONAME-CONAME TO H-NAME.
           MOVE EXT-CONAME-SYSDATE TO H-DATE.
           MOVE EXT-CONAME-USER-ID TO H-USERID.
           IF EXT-ROUTE-BUF = "70"
              IF EXT-EOBUF-MESS NOT = SPACES
                 MOVE EXT-EOBUF-MESS TO H-TITLE.

           IF ( GROUP-FLAG = "Y" )
              MOVE LOW-VALUES TO WS-GTT-TOTAL-TABLE
              MOVE ENT-GROUP TO GR-ID
              PERFORM SEARCH-GR-INIT
              GO TO EXIT-INITIALIZE
           ELSE
              MOVE SPACES TO TOTAL-LEV-TABLE
              MOVE "Y" TO TOTAL-LEVEL-TABLE(9).

           GO TO EXIT-INITIALIZE.

       ENDIT.
           MOVE "X" TO EXT-EOBUF-ERRCD.

       EXIT-INITIALIZE.
           EXIT.

      ******************************************************************
       EO-EXEC-AUDIT SECTION.
      ******************************************************************
           MOVE EXT-EOBUF-DATE2 TO SYS-DATE.
           MOVE S-MM TO END-MONTH.
           MOVE S-YY TO END-YEAR.

           IF EXT-EOBUF-DATE1 NOT = EXT-EOBUF-DATE2
              MOVE 99 TO DAY-OF-MONTH
           ELSE
              MOVE S-DD TO DAY-OF-MONTH.
        
      ******************************************************************
       ENTER-PARAMETERS SECTION.
      ******************************************************************
       ENTER-PARM.
           MOVE "S  A000REFCD." TO VDU.
           MOVE "DEBIT" TO VDUBUF.
           PERFORM SCREENIO.
           
           IF VDUSTATUS = "1U"
              IF ( GTT-ISO = "O" )
                 GO TO GTT-ISO-PARAGRAPH
              ELSE
                 GO TO SUMYN.

      *-----------------------------------------------------------------
       BR-01.
           MOVE LEGEND-BR-01 TO LEGEND.
           PERFORM SEND-LEGEND.
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

      *-----------------------------------------------------------------
       BRANCH-IS-NUMERIC.
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
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO BR-02.
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

      *-----------------------------------------------------------------
       ALL-BR.
           MOVE "SNNN040BR1." TO VDU.
           MOVE 0 TO BEG-BRANCH VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN040BR2." TO VDU.
           MOVE 9999 TO END-BRANCH VDUNUM0.
           PERFORM SCREENIO.
           MOVE "N" TO GROUP-FLAG.
           MOVE "S  A160DISPLAY-USER." TO VDU.
           MOVE SPACES TO VDUBUF.
           PERFORM SCREENIO.

      *-----------------------------------------------------------------
       CHECK-BRANCH-SECURITY.
           PERFORM BR-RANGE-SECURITY.
           IF BR-SECURITY-OKAY = "N"
              GO TO BR-01.

      *-----------------------------------------------------------------
       CL-01.
           MOVE LEGEND-CL-01 TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "ENNN020CL1." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
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

      *-----------------------------------------------------------------
       CL-02.
           MOVE LEGEND-CL-02 TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "ENNN020CL2." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
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
              MOVE "INVALID CLASS RANGE" TO MESS
              PERFORM SEND-MESS
              GO TO CL-01.

           GO TO DRAWER-NO.

      *-----------------------------------------------------------------
       ALL-CL.
           MOVE "SNNN020CL1." TO VDU.
           MOVE 0 TO BEG-CLASS VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN020CL2." TO VDU.
           MOVE 99 TO END-CLASS VDUNUM0.
           PERFORM SCREENIO.

      *-----------------------------------------------------------------
       DRAWER-NO.
           MOVE LEGEND-DRAWER-NO TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "EN N020DRAWER." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "16" GO TO ALL-DRAWERS.
           IF VDUSTATUS = "1U" GO TO CL-02.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO DRAWER-NO.
           MOVE VDUNUM0 TO TILLNO.
           IF TILLNO = 0 OR 99
              GO TO DRAWER-NO.
           GO TO DTE.

      *-----------------------------------------------------------------
       ALL-DRAWERS.
           MOVE "SN N020DRAWER." TO VDU.
           MOVE 99 TO TILLNO VDUNUM0.
           PERFORM SCREENIO.

      *-----------------------------------------------------------------
       DTE.
           IF EXT-ROUTE-BUF NOT = "00"
              GO TO DA-01.

           MOVE LEGEND-DTE TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "ENNN040ENDDATE." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO DRAWER-NO.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO DTE.
           MOVE VDUNUM0 TO EDATE.

           IF END-MONTH < 1 OR > 12
              MOVE "INVALID MONTH" TO MESS
              PERFORM SEND-MESS
              GO TO DTE.

           MOVE END-MONTH TO DATE-MMDDYY-MM.
           MOVE END-YEAR  TO DATE-MMDDYY-YY.
           MOVE 50        TO DATE-MMDDYY-DD.
           PERFORM CONVERT-MMDDYY-TO-YYYYMMDD.
           MOVE DATE-YYYYMMDD TO NDTE-DATE.
           PERFORM NEW-DATE-CALCULATION.
           MOVE NDTE-DATE     TO NUM-DATE.
           PERFORM TSTDAT.
           IF NUM-DATE = 0
              MOVE "INVALID DATE" TO MESS
              PERFORM SEND-MESS
              GO TO DTE.
           MOVE NUM-DATE TO RPT-END-DATE.

      *-----------------------------------------------------------------
       DA-01.
           MOVE LEGEND-DA-01 TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "ENNN020DA1." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.

           IF VDUSTATUS = "16" 
              MOVE "S  A020DA1." TO VDU
              MOVE "**" TO VDUBUF
              PERFORM SCREENIO
              MOVE 99 TO DAY-OF-MONTH
              GO TO ENTER-REVERSALS-ONLY.

           IF ( UP-KEY )
              IF ( EXT-ROUTE-BUF = "00" )
                 GO TO DTE
              ELSE
                 GO TO DRAWER-NO.

           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO DTE.
           MOVE VDUNUM0 TO DAY-OF-MONTH.

           IF DAY-OF-MONTH > 31  OR
              (DAY-OF-MONTH > 30 AND 
              (END-MONTH = 2 OR 4 OR 6 OR 9 OR 11)) OR
              (DAY-OF-MONTH > 29 AND END-MONTH = 2)
              MOVE "DAY IS PAST LAST DAY OF MONTH" TO MESS
              PERFORM SEND-MESS
              GO TO DA-01.

      *-----------------------------------------------------------------
       ENTER-REVERSALS-ONLY.

           MOVE SPACES TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "E  A010REV-ONLY." TO VDU.
           PERFORM SCREENIO.

           IF (                    UP-KEY ) GO TO DA-01.
           IF (                    F1-KEY ) GO TO EXIT-PARM.
           IF (                    F7-KEY ) GO TO EXIT-PARM.
           IF NOT ( DOWN-KEY OR ENTER-KEY ) GO TO ENTER-REVERSALS-ONLY.

           IF NOT ( VDUBUF = "Y" OR "N" )
              GO TO ENTER-REVERSALS-ONLY
           ELSE
              MOVE VDUBUF TO REVERSALS-ONLY.

      *-----------------------------------------------------------------
       ENTER-PL-ISO.

           MOVE "E  A010PL." TO VDU.
           PERFORM SCREENIO.

           IF (                    UP-KEY ) GO TO ENTER-REVERSALS-ONLY.
           IF (                    F1-KEY ) GO TO EXIT-PARM.
           IF (                    F7-KEY ) GO TO EXIT-PARM.
           IF NOT ( DOWN-KEY OR ENTER-KEY ) GO TO ENTER-PL-ISO.

           MOVE VDUBUF TO PL-ISO.

           IF ( PL-ISO-INCLUDE ) OR
              ( PL-ISO-ONLY    )
              GO TO ENTER-PL-TYPE.

           IF ( PL-ISO-SKIP    )
              MOVE "S  A010PLTYPE." TO VDU
              MOVE "A"              TO VDUBUF PL-TYPE
              PERFORM SCREENIO
              MOVE "S  A010ACTIVE." TO VDU
              MOVE "B"              TO VDUBUF PL-ACTIVE
              PERFORM SCREENIO
              GO TO ENTER-INT-BR-ISO.

           GO TO ENTER-PL-ISO.

      *----------------------------------------------------------------
       ENTER-PL-TYPE.

           MOVE "E  A010PLTYPE." TO VDU.
           PERFORM SCREENIO.

           IF (                    UP-KEY ) GO TO ENTER-PL-ISO.
           IF (                    F1-KEY ) GO TO EXIT-PARM.
           IF (                    F7-KEY ) GO TO EXIT-PARM.
           IF NOT ( DOWN-KEY OR ENTER-KEY ) GO TO ENTER-PL-TYPE.

           MOVE VDUBUF TO PL-TYPE.

           IF ( NOT PL-TYPE-ALL ) AND
              ( NOT PL-TYPE-PP  ) AND
              ( NOT PL-TYPE-P2  ) AND
              ( NOT PL-TYPE-P3  ) AND
              ( NOT PL-TYPE-P4  ) AND
              ( NOT PL-TYPE-P5  )
              GO TO ENTER-PL-TYPE.

      *----------------------------------------------------------------
       ENTER-PL-ACTIVE.

           MOVE "E  A010ACTIVE." TO VDU.
           PERFORM SCREENIO.

           IF (                    UP-KEY ) GO TO ENTER-PL-TYPE.
           IF (                    F1-KEY ) GO TO EXIT-PARM.
           IF (                    F7-KEY ) GO TO EXIT-PARM.
           IF NOT ( DOWN-KEY OR ENTER-KEY ) GO TO ENTER-PL-ACTIVE.

           MOVE VDUBUF TO PL-ACTIVE.

           IF ( NOT PL-ACTIVE-INACTIVE ) AND
              ( NOT PL-ACTIVE-ACTIVE   ) AND
              ( NOT PL-ACTIVE-BOTH     )
              GO TO ENTER-PL-ACTIVE.

      *-----------------------------------------------------------------
       ENTER-INT-BR-ISO.

           MOVE "E  A010INT-BR." TO VDU.
           PERFORM SCREENIO.

           IF ( UP-KEY )
              IF ( PL-ISO-SKIP )
                 GO TO ENTER-PL-ISO
              ELSE
                 GO TO ENTER-PL-ACTIVE.

           IF (                    F1-KEY ) GO TO EXIT-PARM.
           IF (                    F7-KEY ) GO TO EXIT-PARM.
           IF NOT ( DOWN-KEY OR ENTER-KEY ) GO TO ENTER-INT-BR-ISO.

           MOVE VDUBUF TO INT-BR-ISO.

           IF ( INT-BR-ISO-INCLUDE ) OR
              ( INT-BR-ISO-SKIP    ) OR
              ( INT-BR-ISO-ONLY    )
              GO TO ENTER-TRAN-CODE.

           GO TO ENTER-INT-BR-ISO.

      *-----------------------------------------------------------------
       ENTER-TRAN-CODE.

           MOVE "E  A020TRCD." TO VDU.
           PERFORM SCREENIO.

           IF (                    UP-KEY ) GO TO ENTER-INT-BR-ISO.
           IF (                    F1-KEY ) GO TO EXIT-PARM.
           IF (                    F3-KEY ) GO TO ALL-TRAN-CODES. 
           IF (                    F7-KEY ) GO TO EXIT-PARM.
           IF NOT ( DOWN-KEY OR ENTER-KEY ) GO TO ENTER-TRAN-CODE.

           MOVE VDUBUF TO TRAN-CODE.
           GO TO ENTER-REF-CODE.            

       ALL-TRAN-CODES.
           MOVE "S  A000TRCD." TO VDU.
           MOVE "**"           TO VDUBUF TRAN-CODE.
           PERFORM SCREENIO.

       ENTER-REF-CODE.

           MOVE "E  A050REFCD." TO VDU.
           PERFORM SCREENIO.

           IF (                    UP-KEY ) GO TO ENTER-TRAN-CODE.     
           IF (                    F1-KEY ) GO TO EXIT-PARM.
           IF (                    F3-KEY ) GO TO ALL-REF-CODES. 
           IF (                    F7-KEY ) GO TO EXIT-PARM.
           IF NOT ( DOWN-KEY OR ENTER-KEY ) GO TO ENTER-REF-CODE.

           MOVE VDUBUF TO REF-CODE.
           GO TO GTT-ISO-PARAGRAPH.

       ALL-REF-CODES.
           MOVE "S  A000REFCD." TO VDU.
           MOVE "*****"           TO VDUBUF REF-CODE.           
           PERFORM SCREENIO.


      *-----------------------------------------------------------------
       GTT-ISO-PARAGRAPH.

           IF ( GROUP-FLAG NOT = "Y" )
              MOVE SPACES TO TOTAL-LEV-TABLE
              MOVE "S  A010GTT-ISO." TO VDU
              MOVE "S" TO VDUBUF GTT-ISO
              PERFORM SCREENIO
              GO TO SUMYN.

           MOVE LEGEND-GTT-01 TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "E  A010GTT-ISO." TO VDU.
           PERFORM SCREENIO.

           IF ( UP-KEY ) GO TO ENTER-REF-CODE.
           IF ( VDUSTATUS = "10" ) GO TO EXIT-PARM.
           IF ( VDUSTATUS = "1>" ) GO TO EXIT-PARM.
           IF NOT ( VDUSTATUS = "00" OR "1D" )
              GO TO GTT-ISO-PARAGRAPH.

           IF NOT ( VDUBUF = "I" OR "S" OR "O" )
              GO TO GTT-ISO-PARAGRAPH.

           MOVE VDUBUF TO GTT-ISO.

           IF ( GTT-ISO = "I" OR "O" )
              PERFORM GRTOTL-WINDOW
           ELSE
              MOVE SPACES TO TOTAL-LEV-TABLE.
 
      *----------------------------------------------------------------
       SUMYN.

           IF ( GTT-ISO = "O" )
              MOVE "S  A010SUMONLY." TO VDU
              MOVE "Y" TO SUM-ONLY VDUBUF
              PERFORM SCREENIO
              GO TO ENTER-EXTRACT-FLAG.

           MOVE "E  A010SUMONLY." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.

           IF ( UP-KEY )
              IF ( GROUP-FLAG = "Y" )
                 GO TO GTT-ISO-PARAGRAPH
              ELSE
                 GO TO ENTER-REF-CODE.

           IF NOT ( VDUSTATUS = "00" OR "1D" )
              GO TO SUMYN.

           MOVE VDUBUF TO SUM-ONLY.
           IF NOT ( SUM-ONLY = "Y" OR "N" )
              GO TO SUMYN.

       ENTER-EXTRACT-FLAG.
           MOVE "E  A010EXTRACT." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF ( UP-KEY ) GO TO ENTER-REF-CODE.
           IF NOT ( VDUSTATUS = "00" OR "1D" )
              GO TO ENTER-EXTRACT-FLAG.

           MOVE VDUBUF TO EXTRACT-FLAG.
           IF NOT ( EXTRACT-FLAG = "Y" OR "N" )
              GO TO ENTER-EXTRACT-FLAG.
      *-----------------------------------------------------------------
       LOCAL-PRINT.
           IF EXT-ROUTE-BUF NOT = "40"
              IF EXT-ROUTE-BUF NOT = "50"
                 MOVE "N" TO LOCAL-PRINT-FG
                 GO TO END-PARM.

           IF BEG-BRANCH = END-BRANCH
              MOVE "N" TO LOCAL-PRINT-FG
              GO TO END-PARM.

      *-----------------------------------------------------------------
       ASK-LOCAL.
           MOVE "S  A170SPOOL-PROMPT." TO VDU.
           MOVE "SPOOL TO LOCAL  :"    TO VDUBUF.
           PERFORM SCREENIO.
           MOVE "SPOOL TO LOCAL DIRECTORY? (Y/N)" TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "S  A050YES-NO." TO VDU.
           MOVE "(Y/N)"          TO VDUBUF.
           PERFORM SCREENIO.
           
           MOVE "E  A010LOCAL-SPOOL." TO VDU.
           PERFORM SCREENIO.  
           IF VDUSTATUS = "10" GO TO EXIT-PARM.

           IF VDUSTATUS = "1U"
              IF GROUP-FLAG = "Y"
                 IF GTT-ISO = "O"
                    GO TO GTT-ISO-PARAGRAPH
                 ELSE
                    GO TO SUMYN
              ELSE
                  GO TO SUMYN.

           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO ASK-LOCAL.  
           

           IF VDUBUF NOT = "Y"
              IF VDUBUF NOT = "N"
                 GO TO ASK-LOCAL.

           MOVE VDUBUF TO LOCAL-PRINT-FG.

           IF ( GTT-ISO NOT = "S" )
              IF ( LOCAL-PRINT-FG = "Y" )
                 MOVE MESSAGE-01-LOC-PRT TO MESS
                 PERFORM SEND-MESS
                 GO TO ASK-LOCAL.
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
              MOVE ENT-GROUP TO VDUBUF GTT-SV-ENT-GROUP
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
           IF ( (BEG-BRANCH = 0) AND (END-BRANCH = 0) )
              MOVE "S  A160DISPLAY-USER." TO VDU
              MOVE "USER BRANCH ONLY" TO VDUBUF
              PERFORM SCREENIO
           ELSE
              MOVE "S  A160DISPLAY-USER." TO VDU
              MOVE SPACES TO VDUBUF
              PERFORM SCREENIO.
           MOVE "SNNN020CL1." TO VDU.
           MOVE BEG-CLASS TO VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN020CL2." TO VDU.
           MOVE END-CLASS TO VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SN N020DRAWER." TO VDU.
           MOVE TILLNO TO VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN020DA1." TO VDU.
           MOVE DAY-OF-MONTH TO VDUNUM0.
           PERFORM SCREENIO.
           MOVE "S  A010REV-ONLY." TO VDU.
           MOVE REVERSALS-ONLY     TO VDUBUF.
           PERFORM SCREENIO.
           MOVE "S  A010PL." TO VDU.
           MOVE PL-ISO       TO VDUBUF.
           PERFORM SCREENIO.
           MOVE "S  A010PLTYPE." TO VDU.
           MOVE PL-TYPE          TO VDUBUF.
           PERFORM SCREENIO.
           MOVE "S  A010ACTIVE." TO VDU.
           MOVE PL-ACTIVE        TO VDUBUF.
           PERFORM SCREENIO.
           MOVE "S  A010INT-BR." TO VDU.
           MOVE INT-BR-ISO       TO VDUBUF.
           PERFORM SCREENIO.
           MOVE "S  A020TRCD." TO VDU.
           MOVE TRAN-CODE      TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "S  A050REFCD." TO VDU.
           MOVE REF-CODE       TO VDUBUF.
           PERFORM SCREENIO.
 
           MOVE "S  A010GTT-ISO." TO VDU.
           MOVE GTT-ISO           TO VDUBUF.
           PERFORM SCREENIO.
           MOVE "S  A010SUMONLY." TO VDU.
           MOVE SUM-ONLY TO VDUBUF.
           PERFORM SCREENIO.
           MOVE "S  A010EXTRACT." TO VDU.
           MOVE EXTRACT-FLAG      TO VDUBUF.
           PERFORM SCREENIO.
           MOVE "S  A170SPOOL-PROMPT." TO VDU.
           MOVE "SPOOL TO LOCAL  :"    TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "S  A050YES-NO." TO VDU.
           MOVE "(Y/N)"          TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "S  A010LOCAL-SPOOL." TO VDU.    
           MOVE LOCAL-PRINT-FG        TO VDUBUF.
           PERFORM SCREENIO.

      ******************************************************************
       SORT-ROUTINE SECTION.
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
              GO TO EXIT-SORT-ROUTINE.

           IF TP-DATE > RPT-END-DATE
              GO TO EXIT-SORT-ROUTINE.

           IF TP-CLASS < BEG-CLASS OR TP-CLASS > END-CLASS
              GO TO SORT-TRP-NEXT.

           IF TILLNO NOT = 99
              IF TP-TILLNO NOT = TILLNO
                 GO TO SORT-TRP-NEXT.

           IF TRAN-CODE = "  " OR  "**"
              IF REF-CODE = "  " OR "*****"
                 GO TO SKIP-REFNO.       
           
           IF TRAN-CODE NOT = "  " AND NOT = "**"
              IF TP-TRCD NOT = TRAN-CODE
                 GO TO SORT-TRP-NEXT.
       
           IF REF-CODE NOT = "     " AND NOT = "*****"
              IF TP-REFNO NOT = REF-CODE
                 GO TO SORT-TRP-NEXT.

       SKIP-REFNO.   
           IF (REVERSALS-ONLY = "Y")    
              IF TP-REV NOT = "Y" 
                 GO TO SORT-TRP-NEXT.

           PERFORM CHECK-TR-REC-IN-PL.

           IF ( PL-ISO-SKIP      ) AND
              ( RECORD-IN-PL-YES )
              GO TO SORT-TRP-NEXT.

           IF ( PL-ISO-ONLY     ) AND
              ( RECORD-IN-PL-NO )
              GO TO SORT-TRP-NEXT.

           IF ( RECORD-IN-PL-NO )
              GO TO SORT-TR-VERIFY-CONTINUE.

           IF ( PL-ACTIVE-ACTIVE  ) AND
              ( TP-PLCD NOT = "P" )
              GO TO SORT-TRP-NEXT.

           IF ( PL-ACTIVE-INACTIVE ) AND
              ( TP-PLCD NOT = "I"  ) AND
              ( TP-PLCD NOT = "W"  )
              GO TO SORT-TRP-NEXT.

           IF ( NOT PL-TYPE-ALL         ) AND
              ( TP-PLTYPE NOT = PL-TYPE )
              GO TO SORT-TRP-NEXT.

      *-----------------------------------------------------------------
       SORT-TR-VERIFY-CONTINUE.

           IF ( INT-BR-ISO-SKIP              ) AND
              ( TP-BRNO NOT = TP-POSTING-BR )
              GO TO SORT-TRP-NEXT.

           IF ( INT-BR-ISO-ONLY          ) AND
              ( TP-BRNO = TP-POSTING-BR )
              GO TO SORT-TRP-NEXT.

           MOVE TRP1-KEY TO TEST-TRP1-KEY.

           IF TEST-TRP1-KEY-MAIN = SAVE-TRP1-KEY-MAIN
              AND SAVE-TP-REC-FG = "Y"
              GO TO SAVE-THIS-ACCT.

           IF TEST-TRP1-KEY-MAIN NOT = SAVE-TRP1-KEY-MAIN
              MOVE SPACE TO SAVE-TP-REC-FG
              MOVE TRP1-KEY TO SAVE-TRP1-KEY.

      *-----------------------------------------------------------------
       SAVE-THIS-ACCT.
           MOVE TP-DATE   TO WK2-DATE.
           MOVE TP-BRNO   TO WK2-BRANCH.
      *    MOVE TP-CODE   TO WK2-CODE.
           MOVE TP-CLASS  TO WK2-CLASS.
           MOVE TP-ACCTNO TO WK2-ACCT.
           MOVE TP-LINE   TO WK2-LINE.

           PERFORM WRITE-WK2-FILE.
           IF IO-FG NOT = 0
              GO TO IO-ERROR.

           GO TO SORT-TRP-NEXT.

       EXIT-SORT-ROUTINE.
           PERFORM CLOSE-WK2-FILE.


      ******************************************************************
       DETAIL-LINE-PROCESS SECTION.
      ******************************************************************
           IF FIRST-TIME = "Y"
              MOVE EXT-CONAME-RPT-LINES TO LN-COUNT
              MOVE 1 TO LN-FEED
              MOVE "N" TO FIRST-TIME
              MOVE "Y" TO RECORDS-FOUND.

           PERFORM GET-DISPLAY-LNNO.
           MOVE LPS-DISPLAY-LNNO TO D-DISPLAY-LNNO.

           PERFORM FIND-BORROWER.
           MOVE TP-MAKER TO D-MAKER.
           MOVE SPACES   TO D-SSNO.
           MOVE TP-DLNO  TO D-DLNO.

           IF HOLD-TP-ACCTNO = TP-ACCTNO AND
               HOLD-TP-OTHBR-FG = TP-OTHBR-FG AND
                HOLD-TP-SSNO = TP-SSNO
              MOVE SPACES TO DETAIL-LINE
           ELSE
              MOVE 1 TO LN-FEED
              MOVE TP-ACCTNO TO HOLD-TP-ACCTNO
              MOVE TP-OTHBR-FG TO HOLD-TP-OTHBR-FG
              MOVE TP-SSNO TO HOLD-TP-SSNO
              IF SUM-ONLY = "N"
                 IF TP-OTHBR-FG = "G"
                    MOVE "POSTED BY :" TO D-BRXX
                    MOVE TP-POSTING-BR TO D-BRX
                    PERFORM WRITE-DETAIL-LINE
                 ELSE
                    IF TP-OTHBR-FG = "C"
                       MOVE "POSTED FOR:" TO D-BRXX
                       MOVE TP-POSTING-BR TO D-BRX
                       PERFORM WRITE-DETAIL-LINE
                    ELSE
                       PERFORM WRITE-DETAIL-LINE.

           MOVE TP-PAYDATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO D-DATE.
           MOVE TP-IBPC TO D-IBPC.
           COMPUTE HOLD-EARNINGS = TP-EARNINGS(1) + TP-EARNINGS(2) +
                                   TP-EARNINGS(3) + TP-EARNINGS(4) +
                                   TP-EARNINGS(5) + TP-EARNINGS(6).

           MOVE HOLD-EARNINGS TO D-EARNINGS.
           MOVE TP-TILLNO TO D-TILL.
           MOVE TP-DLPROC TO D-DLPROC.
           MOVE TP-TRCD TO D-TRCD.

           IF TP-REV = "Y"
              MOVE "*" TO D-REV.

           MOVE TP-REFNO TO D-REFNO.

           IF TP-TRCD = "OT"
              MOVE TP-INTDUE TO TP-TRAMT
              MOVE 0 TO TP-INTDUE.

           IF TP-APPLIED-TO-ESCROW = 20202.02
              MOVE 0 TO TP-APPLIED-TO-ESCROW.

           IF TP-TRCD = "WL" OR "WI"
              ADD TP-APINT TP-APLC GIVING D-TRAMT
           ELSE
              MOVE TP-TRAMT TO D-TRAMT
              IF GP-ESCROW-FG = "Y"
                 ADD TP-APINT TP-APLC GIVING D-APINT
                 MOVE TP-APPLIED-TO-ESCROW TO D-AP-LC-ESC
              ELSE
                 MOVE TP-APINT TO D-APINT
                 MOVE TP-APLC TO D-AP-LC-ESC
              END-IF
              IF ( (TP-TRCD = "ST" OR "RT" OR "ET" OR "LP" OR "NP") OR
                   (TP-TRCD = "RV" AND
                   (TP-ADDON-TEXT = "ADD" OR "ADJ" OR "CNL") ) )
                 PERFORM SETUP-D-TRAMT
                 MOVE TP-APINT TO D-APINT.

           MOVE TP-APCUR TO D-APCUR.

      *WORLD PR#288 REFUND OF INSURANCE TAX
           IF TP-TRCD = "RC" OR "RA" OR "RP" OR "R1" OR "R2" OR "R3"
              IF TP-INS-TAX-RB NOT = ZEROES     AND  
                 TP-INS-TAX-RB NOT = 2020202.02 AND
                 TP-INS-TAX-RB NOT = 0202020.20
                 MOVE "T" TO D-TAX-REFUND-T 
                 MOVE TP-TRCD TO D-TAX-REFUND-INSTYPE 
                 MOVE TP-INS-TAX-RB TO D-TAX-REFUND-AMOUNT.

           IF ( (TP-TRCD = "ST" OR "RT" OR "ET" OR "LP" OR "NP") OR
                 (TP-TRCD = "RV" AND
                  (TP-ADDON-TEXT = "ADD" OR "ADJ" OR "CNL") ) )
              COMPUTE D-APCUR = TP-APCUR * -1
              IF TP-IBPC = "P"
                 COMPUTE D-APCUR = TP-TRAMT * -1.

           MOVE TP-PDTH-DATE TO NUM-DATE.
           MOVE NUM-MO TO D-PDTHRU-MO.
           IF TP-UNITPER-CD-DAYS
              MOVE NUM-DA TO D-PDTHRU-DAYR
           ELSE
              MOVE NUM-YR TO D-PDTHRU-DAYR.

      * IF PRINCIPAL BALANCE TOO BIG, DONT PRINT LCPDTH, AND
      * USE BIGGER PRINT FIELDS.
           IF TP-CURBAL > 99999.99
              ADD TP-OTHBAL TP-OT2BAL GIVING D2-OTHBAL
              ADD TP-INTBAL TP-LCBAL GIVING D2-INTBAL
              MOVE TP-CURBAL TO D2-PRINBAL
           ELSE
              MOVE TP-LCPDTH-DATE TO NUM-DATE
              MOVE NUM-MO TO D-LCPD-MO
              IF TP-UNITPER-CD-DAYS
                 MOVE NUM-DA TO D-LCPD-DAYR
              ELSE
                 MOVE NUM-YR TO D-LCPD-DAYR
              END-IF
              ADD TP-OTHBAL TP-OT2BAL GIVING D-OTHBAL
              ADD TP-INTBAL TP-LCBAL GIVING D-INTBAL
              MOVE TP-CURBAL TO D-PRINBAL.


           IF SUM-ONLY = "N"
            IF TP-CURBAL = 0
               IF NOT (TP-TRCD = "RC" OR "RA" OR "RP"
                               OR "R1" OR "R2" OR "R3")
                  MOVE "**PAIDOFF**" TO D-PAIDOFF
                  PERFORM WRITE-DETAIL-LINE.

           IF TP-GLAMT(1) NOT = 0
              MOVE TP-GLNO(1) TO D-GLNO
              INSPECT D-GLNO REPLACING ALL "/" BY "-".

           IF SUM-ONLY = "N"
              PERFORM WRITE-DETAIL-LINE.

           IF SUM-ONLY = "N"
            IF TP-GLAMT(1) NOT = 0 OR
               TP-DLRVCHGS-ADJ(1) NOT = 0 OR 
                TP-DLRVCHGS-ADJ(2) NOT = 0 OR
                 TP-DLRVCHGS-ADJ(3) NOT = 0 OR
                  TP-DLPARVHELD-ADJ NOT = 0 OR
                   TP-DLPARVPAID-ADJ NOT = 0
              PERFORM SETUP-DLRVCHGS-ADJ
              MOVE TP-GLAMT(1) TO D-GLAMT
              PERFORM WRITE-DETAIL-LINE.

           IF SUM-ONLY = "N"
            IF TP-GLAMT(2) NOT = 0
              MOVE TP-GLNO(2) TO D-GLNO
              INSPECT D-GLNO REPLACING ALL "/" BY "-"
              PERFORM WRITE-DETAIL-LINE
              MOVE TP-GLAMT(2) TO D-GLAMT
              PERFORM WRITE-DETAIL-LINE.

           IF SUM-ONLY = "N"
            IF TP-GLAMT(3) NOT = 0
              MOVE TP-GLNO(3) TO D-GLNO
              INSPECT D-GLNO REPLACING ALL "/" BY "-"
              PERFORM WRITE-DETAIL-LINE
              MOVE TP-GLAMT(3) TO D-GLAMT
              PERFORM WRITE-DETAIL-LINE.

           IF SUM-ONLY = "Y"
              MOVE SPACES TO DETAIL-LINE.
 
      ******************************************************************
       CREATE-DEBIT-RECORD SECTION.
      ******************************************************************

           MOVE SPACES TO DEBIT-REC.

      * WE ARE ONLY EXTRACTING FOR THE POSTING BRANCH SO THESE FIELDS
      * ARE A LITTLE BACKWARDS.
      * IF BRANCH 2 POSTS FOR A BRANCH 1 ACCOUNT;
      *   TRFILE FOR BRANCH 2 CONTAINS;
      *      21. POSTING BR = 1
      *      BRANCH IN THE KEY = 2
      *      OTHER BR FLAG = "C"

      * THE BRANCH IN THE KEY IS BRANCH THAT POSTED IN THE TR RECORD FOR THE
      *  POSTING BRANCH, SEE MY NOTE ABOVE
           MOVE TP-BRNO       TO DEB-POSTING-BRNO.

           MOVE TP-PAYDATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYYYY.
           MOVE DATE-MMDDYYYY TO DEB-PAYMENT-DATE.
           MOVE TP-REFNO      TO DEB-REFNO.

      * TP-POSTING-BR IS REALLY THE OWNING BRANCH IN THE TR RECORD FOR THE
      * POSTING BRANCH, SEE MY NOTE ABOVE
           MOVE TP-POSTING-BR TO DEB-OWNBR.

           MOVE TP-ACCTNO     TO DEB-NUMBER.
           MOVE TP-TRAMT      TO DEB-TRAMT.

           MOVE X"0D" TO DEB-CR.
           MOVE X"0A" TO DEB-LF.

           ADD 1 TO DEBIT-KEY.
           PERFORM WRITE-DEBIT-FILE.
           IF IO-BAD
              GO TO IO-ERROR.


      ******************************************************************
       CREATE-WK SECTION.
      ******************************************************************
       CREATE-WK-REC.
           PERFORM SETUP-WK-KEY.
           PERFORM READ-WK-FILE.

           IF IO-FG = 0
              GO TO CREATE-WK-UPDATE.

           PERFORM SETUP-WK-KEY.
           MOVE 0 TO WK-NO.

           MOVE 0 TO WK-TRAMT WK-INTDUE WK-APOTH WK-APINT WK-APLC
                     WK-APCUR WK-CHECKS WK-EARNINGS WK-DLPROC
                     WK-APESCROW.

           PERFORM WRITE-WK-FILE.

           IF IO-FG NOT = 0
              GO TO IO-ERROR.

           GO TO CREATE-WK-REC.

      *-----------------------------------------------------------------
       CREATE-WK-UPDATE.
           IF TP-TRAMT > 0
              ADD 1 TO WK-NO
           ELSE
              IF TP-TRAMT < 0
                 SUBTRACT 1 FROM WK-NO
              ELSE
      * TP-TRAMT = 0,   "AC" "JD" "PL" "SP"
                 IF TP-REV = "Y"
                    SUBTRACT 1 FROM WK-NO
                 ELSE
                    ADD 1 TO WK-NO.

      *-----------------------------------------------------------------
      * TP-OTHBR-FG --> " " : POSTING BRANCH = OWNING BRANCH.
      *                 "C" : POSTED FOR ANOTHER BRANCH.
      *                 "G" : POSTED BY ANOTHER BRANCH.
      *-----------------------------------------------------------------

           IF TP-OTHBR-FG = " " OR "C"
              IF TP-REFNO = "NSF  "
                 ADD TP-TRAMT TO WK-INTDUE T-INTDUE(1 LEV)
              ELSE
              IF TP-TRCD = "PY" OR "PA" OR "PE" OR "PO"
                                 OR "DF" OR "D2" OR "D3"
                                  OR "D4" OR "D5" OR "D6"
                                   OR "D7" OR "D8" OR "D9"
                                    OR "PL" OR "P2" OR "P3"
                                     OR "P4" OR "P5" OR "TS"
                                      OR "TP" OR "PR" OR "PC"
                                       OR "PP" OR "SS" OR "Z2"
                                        OR "AH" OR "PN" OR "PZ"
                                         OR "IN" OR "OV" OR "ED"
                                          OR "EP" OR "ES"
                 ADD TP-TRAMT TO WK-TRAMT T-TRAMT(1 LEV).

           IF TP-OTHBR-FG = " " OR "G"
              ADD TP-APOTH TO WK-APOTH T-APOTH(1 LEV)
              ADD TP-APOT2 TO WK-APOTH T-APOTH(1 LEV)
              ADD HOLD-EARNINGS TO WK-EARNINGS T-EARNINGS(1 LEV)
              ADD TP-DLPROC TO WK-DLPROC T-DLPROC(1 LEV)
              ADD TP-APLC TO WK-APLC
              IF TP-APPLIED-TO-ESCROW NOT = 20202.02
                 ADD TP-APPLIED-TO-ESCROW TO WK-APESCROW
                                             T-APESCROW(1 LEV)
              END-IF
              PERFORM TEST-ADD-APLC
              IF ( (TP-TRCD = "ST" OR "RT" OR "ET" OR "LP" OR "NP") OR
                   (TP-TRCD = "RV" AND
                     (TP-ADDON-TEXT = "ADD" OR "ADJ" OR "CNL") ) )
                 IF TP-IBPC = "I"
                    SUBTRACT TP-APCUR FROM WK-APCUR
                    SUBTRACT TP-APCUR FROM T-APCUR(1 LEV)
                 ELSE
                    SUBTRACT TP-TRAMT FROM WK-APCUR
                    SUBTRACT TP-TRAMT FROM T-APCUR(1 LEV)
              ELSE
                 ADD TP-APCUR TO WK-APCUR T-APCUR(1 LEV)
                 ADD TP-APINT TO WK-APINT
                 IF TP-TRCD NOT = "WI"
                    ADD TP-APINT TO T-APINT(1 LEV).

      * REVERSALS OF "OV" DO NOT HAVE "CHECK" IN THE REF BUT THE ORIG
      * "OV" DOES, SO THEY DONT WASH....DO NOT WANT "CHECK" IN THE REF
      * SO THIS WAS MY SOLUTION!! MIDA 428
           IF TP-REFNO = "CHECK" OR
                 (TP-TRAMT < 0 AND TP-TRCD = "OV")
              ADD TP-TRAMT TO WK-CHECKS T-CHECKS(1 LEV).

           PERFORM REWRITE-WK-FILE.
           IF TP-BRNO NOT = TP-POSTING-BR
              PERFORM OTHER-BRANCH.


       EXIT-CREATE-WK.
           MOVE 0 TO SUB.
           PERFORM OUTPUT-GL 3 TIMES.

      ****************************************
       BRANCH-WK-TOTALS-BY-TRCD SECTION.
      ****************************************
       BRANCH-CREATE-WK-REC.
           MOVE SPACES TO WK-REC.
           MOVE "L" TO WK-CODE.
           MOVE HOLD-BRNO TO WK-BRNO.
           MOVE 0 TO WK-CLASS.
           MOVE TP-TRCD TO WK-TRCD.
           PERFORM READ-WK-FILE.
           IF IO-FG = 0
              GO TO BRANCH-CREATE-WK-UPDATE.

           MOVE SPACES TO WK-REC.
           MOVE "L" TO WK-CODE.
           MOVE HOLD-BRNO TO WK-BRNO
           MOVE 0 TO WK-CLASS.
           MOVE TP-TRCD TO WK-TRCD.
           MOVE 0 TO WK-NO.

           MOVE 0 TO WK-TRAMT WK-INTDUE WK-APOTH WK-APINT WK-APLC
                     WK-APCUR WK-CHECKS WK-EARNINGS WK-DLPROC
                     WK-APESCROW.

           PERFORM WRITE-WK-FILE.
           IF IO-FG NOT = 0
              GO TO IO-ERROR.

           GO TO BRANCH-CREATE-WK-REC.

      *-----------------------------------------------------------------
       BRANCH-CREATE-WK-UPDATE.
           IF TP-TRAMT > 0
              ADD 1 TO WK-NO
           ELSE
              IF TP-TRAMT < 0
                 SUBTRACT 1 FROM WK-NO
              ELSE
      * TP-TRAMT = 0,   "AC" "JD" "PL" "SP"
                 IF TP-REV = "Y"
                    SUBTRACT 1 FROM WK-NO
                 ELSE
                    ADD 1 TO WK-NO.

      *-----------------------------------------------------------------
      * TP-OTHBR-FG --> " " : POSTING BRANCH = OWNING BRANCH.
      *                 "C" : POSTED FOR ANOTHER BRANCH.
      *                 "G" : POSTED BY ANOTHER BRANCH.
      *-----------------------------------------------------------------

           IF TP-OTHBR-FG = " " OR "C"
              IF TP-REFNO = "NSF  "
                 ADD TP-TRAMT TO WK-INTDUE
              ELSE
              IF TP-TRCD = "PY" OR "PA" OR "PE" OR "PO"
                                 OR "DF" OR "D2" OR "D3"
                                  OR "D4" OR "D5" OR "D6"
                                   OR "D7" OR "D8" OR "D9"
                                    OR "PL" OR "P2" OR "P3"
                                     OR "P4" OR "P5" OR "TS"
                                      OR "TP" OR "PR" OR "PC"
                                       OR "PP" OR "SS" OR "Z2"
                                        OR "AH" OR "PN" OR "PZ"
                                         OR "IN" OR "OV" OR "ED"
                                          OR "EP" OR "ES"
                 ADD TP-TRAMT TO WK-TRAMT.

           IF TP-OTHBR-FG = " " OR "G"
              ADD TP-APOTH TO WK-APOTH
              ADD TP-APOT2 TO WK-APOTH
              ADD HOLD-EARNINGS TO WK-EARNINGS
              ADD TP-DLPROC TO WK-DLPROC 
              ADD TP-APLC TO WK-APLC
              IF TP-APPLIED-TO-ESCROW NOT = 20202.02
                 ADD TP-APPLIED-TO-ESCROW TO WK-APESCROW
              END-IF
              IF ( (TP-TRCD = "ST" OR "RT" OR "ET" OR "LP" OR "NP") OR
                   (TP-TRCD = "RV" AND
                     (TP-ADDON-TEXT = "ADD" OR "ADJ" OR "CNL") ) )
                 IF TP-IBPC = "I"
                    SUBTRACT TP-APCUR FROM WK-APCUR
                 ELSE
                    SUBTRACT TP-TRAMT FROM WK-APCUR
              ELSE
                 ADD TP-APCUR TO WK-APCUR 
                 ADD TP-APINT TO WK-APINT.

           IF TP-REFNO = "CHECK" OR
                 (TP-TRAMT < 0 AND TP-TRCD = "OV")
              ADD TP-TRAMT TO WK-CHECKS.

           PERFORM REWRITE-WK-FILE.


      ******************************************************************
       OUTPUT-GL SECTION.
      ******************************************************************
           ADD 1 TO SUB.

           IF TP-GLAMT(SUB) = 0
              GO TO END-OUTPUT-GL.

      * ONLY GIVE THE GL AMT TO THE BRANCH THAT IT WAS MADE FOR
           IF TP-OTHBR-FG NOT = " " AND NOT = "G"
              GO TO END-OUTPUT-GL.

           PERFORM SETUP-WK-GL1-KEY.
           PERFORM READ-WK-FILE.

           IF IO-FG = 0
              GO TO ACCUM-GL-WK.

           PERFORM SETUP-WK-GL1-KEY.
           MOVE TP-GLAMT(SUB) TO WK-GLAMT.
           PERFORM WRITE-WK-FILE.

           IF IO-FG = 9
              GO TO IO-ERROR.

           GO TO END-OUTPUT-GL.

       ACCUM-GL-WK.
           ADD TP-GLAMT(SUB) TO WK-GLAMT.
           PERFORM REWRITE-WK-FILE.

       END-OUTPUT-GL.
           EXIT.

      ******************************************************************
       FIND-BORROWER SECTION.
      ******************************************************************
           IF ( TP-OTHBR-FG = "C" )
              MOVE "INTER-OFFICE PAYMENT" TO D-NAME
              GO TO END-FIND-BORROWER.

           MOVE TP-SSNO TO BW-SSNO.
           PERFORM READ-BW1-FILE.
           IF IO-FG = 9
               MOVE SPACES TO BW-REC
               MOVE "** NO BORROWER MASTER **" TO D-NAME
               GO TO END-FIND-BORROWER.

           MOVE BW-LNAME TO NAME-WORKER.
           INSPECT NAME-WORKER REPLACING FIRST "  " BY ",?".
           MOVE BW-FNAME TO FIRST-NAME.

           INSPECT NAME-WORKER REPLACING FIRST "                "
               BY FIRST-NAME.

           INSPECT NAME-WORKER REPLACING ALL "?" BY " ".
           MOVE NAME-WORKER TO D-NAME.

       END-FIND-BORROWER.
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
       SET-BRANCH-HEADINGS SECTION.
      ******************************************************************
           MOVE HOLD-BRNO TO H-BRNO.
           MOVE BR-NAME TO H-BRDESC.

      ******************************************************************
       CLASS-TOTALS SECTION.
      ******************************************************************

           IF ( GTT-ISO = "O" )
              GO TO END-CLASS-TOTALS.

           MOVE "Y" TO SUMMARY-FG.

      * DO NOT PRINT ANY TOTALS, BUT NEED TO DO THE ADDING SO BRANCH
      * TOTALS WORK

       END-CLASS-TOTALS.
           MOVE 2 TO LEV.
           MOVE 1 TO SUB.
           PERFORM UPDATE-TABLE2 VARYING SUB1 FROM 1 BY 1
                                                  UNTIL SUB1 > 2.
           MOVE 1 TO SUB2.
           PERFORM CLEAR-T-TOTALS
                         VARYING SUB1 FROM 1 BY 1 UNTIL SUB1 > 2
           MOVE "N" TO SUMMARY-FG.

      ******************************************************************
       BRANCH-TOTALS SECTION.
      ******************************************************************
           IF ( GTT-ISO = "O" )
              GO TO END-BRANCH-TOTALS.

           MOVE "BRANCH TOTALS:" TO DETAIL-LINE.
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE SPACES TO WK-KEY.
           MOVE "L" TO WK-CODE.
           MOVE HOLD-BRNO TO WK-BRNO.
           MOVE 0 TO WK-CLASS.
           PERFORM START-WK-FILE.

      *-----------------------------------------------------------------
       PRINT-BRANCH-NEXT-CODE.
           PERFORM READ-WK-FILE-NEXT.

           IF IO-FG = 9 OR WK-CODE NOT = "L" OR
                WK-BRNO NOT = HOLD-BRNO OR WK-CLASS NOT = 0
              GO TO FINISH-BRANCH-TOTALS.

           MOVE WK-TRCD TO DS-TRCD.
           MOVE WK-NO TO DS-NO.
           MOVE WK-TRAMT TO DS-TRAMT.
           MOVE WK-INTDUE TO DS-INTDUE.
           MOVE WK-APOTH TO DS-APOTH.
           MOVE WK-APINT TO DS-APINT.
           MOVE WK-APLC TO DS-APLC.
           MOVE WK-APCUR TO DS-APCUR.
           MOVE WK-CHECKS TO DS-CHECKS.
           MOVE WK-EARNINGS TO DS-EARNINGS.
           MOVE WK-DLPROC TO DS-DLPROC.
           PERFORM WRITE-DETAIL-LINE.
           MOVE 1 TO LN-FEED.

           GO TO PRINT-BRANCH-NEXT-CODE.

       FINISH-BRANCH-TOTALS.
           MOVE "Y" TO SUMMARY-FG.
           MOVE 2 TO LEV.     
           MOVE 1 TO SUB1.
           MOVE 3 TO LN-FEED.
           MOVE "BR TOTALS:" TO DETAIL-LINE.
           PERFORM PRINT-SUMMARY. 
           MOVE 2 TO LEV.     
           MOVE 2 TO SUB1.
           MOVE 2 TO LN-FEED.
           MOVE "POSTINGS FROM OTHER BRANCHES:" TO DETAIL-LINE.
           PERFORM WRITE-DETAIL-LINE.
           PERFORM PRINT-SUMMARY. 

      *-----------------------------------------------------------------
       END-BRANCH-TOTALS.
           MOVE 3 TO LEV.
           MOVE 2 TO SUB.
           PERFORM UPDATE-TABLE2 VARYING SUB1 FROM 1 BY 1 
                                                   UNTIL SUB1 > 2.
           MOVE 2 TO SUB2.
           PERFORM CLEAR-T-TOTALS
                          VARYING SUB1 FROM 1 BY 1 UNTIL SUB1 > 2.
           MOVE "N" TO SUMMARY-FG.

      ******************************************************************
       GRAND-TOTALS SECTION.
      ******************************************************************
           IF ( GTT-ISO = "O" )
              GO TO END-GRAND-TOTALS.

           MOVE "Y" TO SUMMARY-FG.
           MOVE 3 TO LEV.
           MOVE 1 TO SUB1.
           MOVE 3 TO LN-FEED.
           MOVE "GRAND TOTALS:" TO DETAIL-LINE.
           PERFORM PRINT-SUMMARY.
           MOVE 3 TO LEV.     
           MOVE 2 TO SUB1.
           MOVE 2 TO LN-FEED.
           MOVE "TOTAL OTH BR:" TO DETAIL-LINE.
           PERFORM PRINT-SUMMARY. 

      *-----------------------------------------------------------------
       END-GRAND-TOTALS.
           MOVE 3 TO SUB2.
           PERFORM CLEAR-T-TOTALS
                         VARYING SUB1 FROM 1 BY 1 UNTIL SUB1 > 2.
           MOVE "Y" TO SUMMARY-FG.

      ******************************************************************
       GL-SUMMARY SECTION.
      ******************************************************************
           MOVE GL-HEAD TO DETAIL-LINE.
           MOVE 4 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.
           MOVE LOW-VALUE TO WK-KEY.
           MOVE "G" TO WK-CODE.
           PERFORM START-WK-FILE.

       PRINT-NEXT-GL.
           PERFORM READ-WK-FILE-NEXT.
           IF IO-FG NOT = 0
              GO TO EXIT-GL-SUMMARY.

           IF WK-CODE NOT = "G"
              GO TO PRINT-NEXT-GL.

           MOVE WK-GLNO TO DS-GLNO.
           INSPECT DS-GLNO REPLACING ALL "/" BY "-".
           MOVE WK-GLAMT TO DS-GLAMT.
           PERFORM WRITE-DETAIL-LINE.
           GO TO PRINT-NEXT-GL.

       EXIT-GL-SUMMARY.
           EXIT.

      ******************************************************************
       HEAD-PAGE SECTION.
      ******************************************************************
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
           PERFORM WRITE-PR-REC.
           IF ( GTT-HEADER-FG = "Y" )
              MOVE GTT-HEAD1 TO PR-REC
              MOVE 2 TO LN-FEED
              PERFORM WRITE-PR-REC
              MOVE "BR#/GROUP " TO SUM-HEAD-PROMPT
              MOVE SUM-HEAD TO PR-REC
              MOVE 1 TO LN-FEED
              PERFORM WRITE-PR-REC
              MOVE ALL "-" TO PR-REC
              PERFORM WRITE-PR-REC
              MOVE 10 TO LN-COUNT
              MOVE 1 TO LN-FEED
           ELSE
           IF SUMMARY-FG = "N"
              MOVE HEAD5 TO PR-REC
              MOVE 2 TO LN-FEED
              PERFORM WRITE-PR-REC
              IF GP-ESCROW-FG = "Y"
                 MOVE HEAD6-ESCROW TO PR-REC
              ELSE
                 MOVE HEAD6 TO PR-REC
              END-IF
              MOVE 1 TO LN-FEED
              PERFORM WRITE-PR-REC
              MOVE 6 TO LN-COUNT
              MOVE 2 TO LN-FEED
           ELSE
              MOVE 4 TO LN-COUNT
              MOVE 2 TO LN-FEED.

       END-HEAD-PAGE.
           EXIT.

      ******************************************************************
       PRINT-RANGE-LINES SECTION.
      ******************************************************************
           MOVE 4 TO LN-FEED.
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
           MOVE "BEG CLASS" TO RANGE-LINE.
           MOVE BEG-CLASS TO R-NUM.
           PERFORM WRITE-DETAIL-LINE.
           MOVE "END CLASS" TO RANGE-LINE.
           MOVE END-CLASS TO R-NUM.
           PERFORM WRITE-DETAIL-LINE.
           MOVE "MONTH-END DATE" TO RANGE-LINE.
           MOVE EDATE TO R-NUM.
           PERFORM WRITE-DETAIL-LINE.
           MOVE "DAY OF MONTH" TO RANGE-LINE.
           MOVE DAY-OF-MONTH TO R-NUM.
           PERFORM WRITE-DETAIL-LINE.
           MOVE "P & L" TO RANGE-LINE.
           MOVE PL-ISO  TO R-CHAR.
           PERFORM WRITE-DETAIL-LINE.
           MOVE "  TYPE" TO RANGE-LINE.
           MOVE PL-TYPE  TO R-CHAR.
           PERFORM WRITE-DETAIL-LINE.
           MOVE "    (IN)ACTIVE" TO RANGE-LINE.
           MOVE PL-ACTIVE        TO R-CHAR.
           PERFORM WRITE-DETAIL-LINE.
           MOVE "INTER BRANCH PAYMENTS     (I,S,O): " TO RANGE-LINE.
           MOVE INT-BR-ISO  TO R-CHAR.
           PERFORM WRITE-DETAIL-LINE.
           MOVE "REVERSALS ONLY" TO RANGE-LINE.
           MOVE REVERSALS-ONLY   TO R-CHAR.
           PERFORM WRITE-DETAIL-LINE.
           MOVE "TRANSACTION CODE" TO RANGE-LINE.
           MOVE TRAN-CODE        TO R-CHAR.
           PERFORM WRITE-DETAIL-LINE.
           MOVE "REF NO"           TO RANGE-LINE.
           MOVE REF-CODE           TO R-CHAR.
           PERFORM WRITE-DETAIL-LINE.
           MOVE "INCLUDE EXTRACT" TO RANGE-LINE.
           MOVE EXTRACT-FLAG  TO R-CHAR.
           PERFORM WRITE-DETAIL-LINE.
           MOVE "GROUP TOTALS" TO RANGE-LINE.
           MOVE GTT-ISO        TO R-CHAR.
           PERFORM WRITE-DETAIL-LINE.

      ***************************************************************
       CLASS-SCAN SECTION.
      ***************************************************************
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

      *****************************************************************
      * PROCESS TRANSACTIONS MADE TO THIS BRANCHES ACCOUNTS BY 
      *   OTHER BRANCHES.
      *
      * TP-OTHBR-FG --> " " : POSTING BRANCH = OWNING BRANCH.
      *                 "C" : POSTED FOR ANOTHER BRANCH.
      *                 "G" : POSTED BY ANOTHER BRANCH.
      *****************************************************************
       OTHER-BRANCH SECTION.
           IF TP-OTHBR-FG = " " OR "G"
              IF TP-REFNO = "NSF  "
                 ADD TP-TRAMT TO T-INTDUE(2 LEV)
              ELSE
              IF TP-TRCD = "PY" OR "PA" OR "PE" OR "PO"
                                 OR "DF" OR "D2" OR "D3"
                                  OR "D4" OR "D5" OR "D6"
                                   OR "D7" OR "D8" OR "D9"
                                    OR "PL" OR "P2" OR "P3"
                                     OR "P4" OR "P5" OR "TS"
                                      OR "TP" OR "PR" OR "PC"
                                       OR "PP" OR "SS" OR "Z2"
                                        OR "AH" OR "PN" OR "PZ"
                                         OR "IN" OR "ED" OR "EP"
                                          OR "ES"
                 ADD TP-TRAMT TO T-TRAMT(2 LEV).

           IF TP-OTHBR-FG = " " OR "G"
              ADD TP-APOTH TO T-APOTH(2 LEV)
              ADD TP-APOT2 TO T-APOTH(2 LEV)
              ADD HOLD-EARNINGS TO T-EARNINGS(2 LEV)
              ADD TP-DLPROC TO T-DLPROC(2 LEV)
              IF TP-APPLIED-TO-ESCROW NOT = 20202.02
                 ADD TP-APPLIED-TO-ESCROW TO T-APESCROW(2 LEV)
              END-IF
              IF TP-TRCD NOT = "WL"
                 ADD TP-APLC TO T-APLC(2 LEV)
              END-IF
              IF ( (TP-TRCD = "ST" OR "RT" OR "ET" OR "LP" OR "NP") OR
                   (TP-TRCD = "RV" AND
                     (TP-ADDON-TEXT = "ADD" OR "ADJ" OR "CNL") ) )
                 IF TP-IBPC = "I"
                    SUBTRACT TP-APCUR FROM T-APCUR(2 LEV)
                 ELSE
                    SUBTRACT TP-TRAMT FROM T-APCUR(2 LEV)
              ELSE
                 ADD TP-APCUR TO T-APCUR(2 LEV)
                 IF TP-TRCD NOT = "WI"
                    ADD TP-APINT TO T-APINT(2 LEV).

           IF TP-REFNO = "CHECK"
              ADD TP-TRAMT TO T-CHECKS(2 LEV).
       EXIT-OTHER-BRANCH.
           EXIT.
 
      *****************************************************************
      * CLEAR TOTAL TABLE
      * 
      *  SUB1 --> 1 = OWNING BRANCH IS POSTING BRANCH
      *           2 = OWNING BRANCH IS NOT THE POSTING BRANCH
      *               POSTED FROM AMOTHER BRANCH.
      *
      *  SUB  --> 1 = CLASS TOTALS.
      *           2 = BRANCH TOTALS.
      *           3 = GRAND TOTALS.
      *
      *****************************************************************
       CLEAR-T-TOTALS SECTION.
           MOVE 0 TO T-TRAMT  (SUB1 SUB2)   T-INTDUE   (SUB1 SUB2)
                     T-APOTH  (SUB1 SUB2)   T-APINT    (SUB1 SUB2)
                     T-APLC   (SUB1 SUB2)   T-APCUR   (SUB1 SUB2)
                     T-CHECKS (SUB1 SUB2)   T-EARNINGS (SUB1 SUB2)
                     T-DLPROC (SUB1 SUB2)   T-APESCROW (SUB1 SUB2).

      ******************************************************************
       GTT-ADD-TOTALS SECTION.
      ******************************************************************

           ADD WK-NO          TO  WS-GTT-NO        ( GTT-TOTAL-SUB ).
           ADD WK-TRAMT       TO  WS-GTT-TRAMT     ( GTT-TOTAL-SUB ).
           ADD WK-INTDUE      TO  WS-GTT-INTDUE    ( GTT-TOTAL-SUB ).
           ADD WK-APOTH       TO  WS-GTT-APOTH     ( GTT-TOTAL-SUB ).
           ADD WK-APINT       TO  WS-GTT-APINT     ( GTT-TOTAL-SUB ).
           ADD WK-APLC        TO  WS-GTT-APLC      ( GTT-TOTAL-SUB ).
           ADD WK-APCUR       TO  WS-GTT-APCUR     ( GTT-TOTAL-SUB ).
           ADD WK-CHECKS      TO  WS-GTT-CHECKS    ( GTT-TOTAL-SUB ).
           ADD WK-EARNINGS    TO  WS-GTT-EARNINGS  ( GTT-TOTAL-SUB ).
           ADD WK-DLPROC      TO  WS-GTT-DLPROC    ( GTT-TOTAL-SUB ).  
           ADD WK-APESCROW    TO  WS-GTT-APESCROW  ( GTT-TOTAL-SUB ).  

       EXIT-GTT-ADD-TOTALS.
           EXIT.

      ******************************************************************
       GTT-COMPILE-GROUP-TOTALS SECTION.
      ******************************************************************
           MOVE SPACES      TO WK-KEY.
           MOVE "L"         TO WK-CODE.
           MOVE WGR-SRESULT TO WK-BRNO.
           MOVE ZEROES      TO WK-CLASS.
           PERFORM START-WK-FILE.

       GTT-NEXT-WK1.

           PERFORM READ-WK-FILE-NEXT.
           IF ( IO-FG   NOT = 0           ) OR
              ( WK-CODE NOT = "L"         ) OR
              ( WK-BRNO NOT = WGR-SRESULT ) OR
      * CREATED NEW BRANCH TOTALS , CLASS = 0
              ( WK-CLASS NOT = 0 )
                 GO TO EXIT-GTT-COMPILE-GROUP-TOTALS.

           MOVE "N" TO GTT-FIRST-TIME.

       GTT-OK-TO-PROCESS.

           PERFORM GTT-CHECK-ADD-TOTALS.
           GO TO GTT-NEXT-WK1.

       EXIT-GTT-COMPILE-GROUP-TOTALS.
           EXIT.

      ******************************************************************
       GTT-PRINT-GROUP-BR-TOTALS SECTION.
      ******************************************************************

           MOVE WGR-SID(GTT-PREV-SUB) TO WGR-WORD.
           MOVE WGR-NUMB              TO BR-NO.
           PERFORM READ-BR-FILE.

           IF ( IO-FG NOT = 0 )
              MOVE "BRANCH #&&&& NOT ON FILE" TO BR-NAME
              INSPECT BR-NAME REPLACING FIRST "&&&&" BY BR-NO.

           MOVE SPACES    TO DETAIL-LINE.
           MOVE 9         TO GTT-TOTAL-SUB.
           MOVE "BR#:"    TO WS-HOLD-PROMPT.
           MOVE BR-NO     TO D2-BRNO.

           PERFORM GTT-PRINT-TOTALS.

       EXIT-GTT-PRINT-GROUP-BR-TOTALS.
           EXIT.

      ******************************************************************
       GTT-PRINT-GROUP-TOTALS SECTION.
      ******************************************************************

           MOVE SPACES    TO DETAIL-LINE.
           MOVE "GRP:"    TO WS-HOLD-PROMPT.
           MOVE GR-ID     TO D2-GRNO.

           PERFORM GTT-PRINT-TOTALS.

       EXIT-GTT-PRINT-GROUP-TOTALS.
           EXIT.

      ******************************************************************
       GTT-PRINT-TOTALS SECTION.
      ******************************************************************
           MOVE WS-GTT-NO        (GTT-TOTAL-SUB) TO DS-NO.
           MOVE WS-GTT-TRAMT     (GTT-TOTAL-SUB) TO DS-TRAMT.
           MOVE WS-GTT-INTDUE    (GTT-TOTAL-SUB) TO DS-INTDUE.     
           MOVE WS-GTT-APOTH     (GTT-TOTAL-SUB) TO DS-APOTH.    
           MOVE WS-GTT-APINT     (GTT-TOTAL-SUB) TO DS-APINT.     
           MOVE WS-GTT-APLC      (GTT-TOTAL-SUB) TO DS-APLC.      
           MOVE WS-GTT-APCUR    (GTT-TOTAL-SUB) TO DS-APCUR.   
           MOVE WS-GTT-CHECKS    (GTT-TOTAL-SUB) TO DS-CHECKS.   
           MOVE WS-GTT-EARNINGS  (GTT-TOTAL-SUB) TO DS-EARNINGS.
           MOVE WS-GTT-DLPROC    (GTT-TOTAL-SUB) TO DS-DLPROC.
           MOVE WS-GTT-APESCROW  (GTT-TOTAL-SUB) TO DS-APESCROW.

           MOVE 1 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           IF ( WS-HOLD-PROMPT = "GRP:" )
              MOVE ALL "-" TO DETAIL-LINE
              PERFORM WRITE-DETAIL-LINE.

      * CLEAR LEVEL IN TABLE  
           MOVE LOW-VALUES TO WS-GTT-TABLE-GROUP-LEVEL(GTT-TOTAL-SUB).

       EXIT-GTT-PRINT-TOTALS.
           EXIT.


      ******************************************************************
      *    OUT:  LPS-DISPLAY-LNNO.
      ******************************************************************
       GET-DISPLAY-LNNO SECTION.
      * LPS-DISPLAY-LNNO IS SET FIRST SO THAT IF THERE IS A ERROR
      * IT WILL BE ALREADY DISPLAYING FOR A ERROR, OTHERWISE IT WILL
      * RESET LPS-DISPLAY-LNNO IN 'PERFORM LPSTAT-DISPLAY.'
           MOVE TP-CLASS  TO LPS-CLASS.
           MOVE "-"       TO LPS-DASH.
           MOVE TP-ACCTNO TO LPS-NUMBER.
           MOVE "{N/A}"   TO LPS-7STATUS.

      * WAS DISPLAYING THE WRONG BRANCHES STATUS FLAGS ON THE ACCOUNT
      * IF THE ACCOUNT HAPPENS TO EXIST IN THE POSTING BRANCHES FILE !!
           IF ( TP-OTHBR-FG = "C" )
              GO TO GET-DISPLAY-LNNO-EXIT.

           MOVE TP-BRNO   TO LN-OWNBR.
           MOVE TP-ACCTNO TO LN-ACCTNO.
           PERFORM READ-LN1-FILE.
           IF ( IO-BAD )
              GO TO GET-DISPLAY-LNNO-EXIT.

           MOVE LN-ORGST    TO SP-ORGST.
           MOVE LN-SPRCLASS TO SP-SPRCLASS.
           MOVE LN-SUBCLASS TO SP-SUBCLASS.
           MOVE LN-LAWCODE  TO SP-LAWCODE.
           PERFORM READ-SP1-FILE.
           IF ( IO-BAD )
              GO TO GET-DISPLAY-LNNO-EXIT.

           PERFORM LPSTAT-DISPLAY.

       GET-DISPLAY-LNNO-EXIT.
           EXIT.

      ******************************************************************
      *
      *     C O P Y - L I B R A R Y - S E C T I O N S
      *
      ******************************************************************
       COPY "LIBWI/GRTOTL.CPY".
       COPY "LIBGB/TIMEIO.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/GPENV.CPY".
       COPY "LIBGB/GRWORK.CPY".
       COPY "LIBGB/GRSERCH.CPY".
       COPY "LIBGB/BRSECURE.CPY".
       COPY "LIBGB/EOSPOOL.CPY".
       COPY "LIBGB/GRTOTALS.CPY".
       COPY "LIBLP/LPMDTE.CPY".
       COPY "LIBLP/LPSTAT.CPY".

      ************************************************                          
       FORM-RESET SECTION.                                                      
      ************************************************                          
           MOVE EXT-CONAME-CONAME TO DEBEXT-CONAME.                             
           MOVE EXT-CONAME-SYSDATE TO DEBEXT-SYSDATE.                           
           DISPLAY DEBEXT-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.                  
           MOVE DEBEXT-FORM-FIELDS TO WR-BUF.
           MOVE DEBEXT-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.
                                                                                
       FORM-RESET-EXIT.                                                         
           EXIT.                                                                
                                                                                
      ************************************************                          
       SETUP-LINK-INFO SECTION.                                                 
      ************************************************                          
           MOVE DEBEXT-HELP--FILE TO HELP-LINK-FILE.                            
           MOVE DEBEXT-HELP--DIR TO HELP-LINK-DIR.                              
                                                                                
      ************************************************                          
       VDU-CONVERT-FIELD SECTION.                                               
      ************************************************                          
           MOVE "00" TO STAT.       
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBSP/DEBEXT_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ******************************************************************
       MISC-PARA SECTION.
      ******************************************************************
       COPY "LIBGB/GETPR.CPY".
       COPY "LIBGB/SYSTEM.CPY".
       COPY "LIBGB/MKTEMP.CPY".
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/DACCESS.CPY".
       COPY "LIBGB/RMFILE.CPY".
      *-----------------------------------------------------------------
       CHECK-TR-REC-IN-PL.

           MOVE "Y" TO RECORD-PL-STATUS.

           IF ( TP-TRCD = "PL" OR "P2" OR "P3" OR "P4" OR "P5" )
              MOVE "N" TO RECORD-PL-STATUS.

           IF ( TP-PLCD = SPACES )
              MOVE "N" TO RECORD-PL-STATUS.

      *-----------------------------------------------------------------
       SETUP-DLRVCHGS-ADJ.
           IF TP-DLRVCHGS-ADJ(1) NOT = 0 
              MOVE "RSV ADJ 1: " TO D-DLTAG1
              MOVE TP-DLRVCHGS-ADJ(1) TO D-DLADJ1.
 
           IF TP-DLRVCHGS-ADJ(2) NOT = 0
              MOVE "RSV ADJ 2: " TO D-DLTAG2
              MOVE TP-DLRVCHGS-ADJ(2) TO D-DLADJ2.
 
           IF TP-DLRVCHGS-ADJ(3) NOT = 0
              MOVE "RSV ADJ 3: " TO D-DLTAG3
              MOVE TP-DLRVCHGS-ADJ(3) TO D-DLADJ3.
           
           IF TP-DLPARVHELD-ADJ NOT = 0
              MOVE "PVHELD ADJ: " TO D-DLTAG4
              MOVE TP-DLPARVHELD-ADJ TO D-PVHELD.

           IF TP-DLPARVPAID-ADJ NOT = 0
              MOVE "PVPAID ADJ: " TO D-DLTAG5
              MOVE TP-DLPARVPAID-ADJ TO D-PVPAID.
 
      *-----------------------------------------------------------------
       TEST-ADD-APLC.
           IF TP-TRCD NOT = "WL"
              ADD TP-APLC TO T-APLC(1 LEV).
 
      *-----------------------------------------------------------------
       SEND-LEGEND.
           MOVE LEGEND TO WR-BUF.
           MOVE MSEL-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.
 
      *-----------------------------------------------------------------
       SETUP-WK-KEY.
           MOVE SPACES TO WK-REC.
           MOVE "L" TO WK-CODE.
           MOVE TP-BRNO TO WK-BRNO.
           MOVE TP-CLASS TO WK-CLASS.
           MOVE TP-TRCD TO WK-TRCD.
 
      *-----------------------------------------------------------------
       SETUP-WK-GL1-KEY.
           MOVE SPACES TO WK-REC.
           MOVE "G" TO WK-CODE.
           MOVE TP-GLNO(SUB) TO WK-GLNO.
 
      *-----------------------------------------------------------------
       UPDATE-TABLE2.
           ADD T-TRAMT(SUB1 SUB) TO T-TRAMT(SUB1 LEV).
           ADD T-INTDUE(SUB1 SUB) TO T-INTDUE(SUB1 LEV).
           ADD T-APOTH(SUB1 SUB) TO T-APOTH(SUB1 LEV).
           ADD T-APINT(SUB1 SUB) TO T-APINT(SUB1 LEV).
           ADD T-APLC(SUB1 SUB) TO T-APLC(SUB1 LEV).
           ADD T-APCUR(SUB1 SUB) TO T-APCUR(SUB1 LEV).
           ADD T-CHECKS(SUB1 SUB) TO T-CHECKS(SUB1 LEV).
           ADD T-EARNINGS(SUB1 SUB) TO T-EARNINGS(SUB1 LEV).
           ADD T-DLPROC(SUB1 SUB) TO T-DLPROC(SUB1 LEV).
           ADD T-APESCROW(SUB1 SUB) TO T-APESCROW(SUB1 LEV).
 
      *-----------------------------------------------------------------
       TYPE-TOTALS.
           MOVE 1 TO LEV.
           MOVE 1 TO SUB1.
           MOVE 3 TO LN-FEED.
           MOVE "CLASS TOTALS:" TO DETAIL-LINE.
           PERFORM PRINT-SUMMARY.
 
      *-----------------------------------------------------------------
       PRINT-SUMMARY.
           MOVE T-TRAMT(SUB1 LEV) TO SUM-TRAMT.
           MOVE T-INTDUE(SUB1 LEV) TO SUM-INTDUE.
           MOVE T-APOTH(SUB1 LEV) TO SUM-APOTH.
           MOVE T-APINT(SUB1 LEV) TO SUM-APINT.
           MOVE T-APLC(SUB1 LEV) TO SUM-APLC.
           MOVE T-APCUR(SUB1 LEV) TO SUM-APCUR.
           MOVE T-CHECKS(SUB1 LEV) TO SUM-CHECKS.
           MOVE T-EARNINGS(SUB1 LEV) TO SUM-EARNINGS.
           MOVE T-DLPROC(SUB1 LEV) TO SUM-DLPROC.
           MOVE T-APESCROW(SUB1 LEV) TO SUM-APESCROW.
           PERFORM WRITE-DETAIL-LINE.
 
      *-----------------------------------------------------------------
       SETUP-D-TRAMT.
           IF TP-IBPC = "I"
              COMPUTE D-TRAMT = TP-TRAMT * -1
           ELSE
              COMPUTE D-TRAMT = TP-APCUR * -1.

      ******************************************************************
       IO-ROUTINES SECTION.
      ******************************************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBLP/LPTRPGS_SQL.CPY".
       COPY "LIBLP/LPSPGS_SQL.CPY".
       COPY "LIBLP/LPLNGS_SQL.CPY".
       COPY "LIBLP/LPBWGS_SQL.CPY".
       COPY "LIBGB/GBGRGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBPRINT.CPY".
       COPY "LIBLP/LPBW1RN.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBGB/GBGR1RN.CPY".
       COPY "LIBLP/LPTRP1RN.CPY".
       COPY "LIBLP/LPLN1RN.CPY".
       COPY "LIBLP/LPSP1RN.CPY".
       COPY "LIBGB/GBWKIN.CPY".
       COPY "LIBGB/GBWK2IN.CPY".
      *-----------------------------------------------------------------

       OPEN-DEBIT-FILE.
           PERFORM OPEN-IT.
           MOVE DEBIT-IFN TO E-FILE.
           OPEN I-O DEBIT-FILE.
           IF IO-FG = 8
              GO TO OPEN-DEBIT-FILE.
           IF IO-FG = 7
              CLOSE DEBIT-FILE
              GO TO OPEN-DEBIT-FILE.
       OPEN-DEBIT-FILE-OUTPUT.
           PERFORM OPEN-IT.
           MOVE DEBIT-IFN TO E-FILE.
           OPEN OUTPUT DEBIT-FILE.
           IF IO-FG = 8
              GO TO OPEN-DEBIT-FILE-OUTPUT.
           IF IO-FG = 7
              CLOSE DEBIT-FILE
              GO TO OPEN-DEBIT-FILE-OUTPUT.
       WRITE-DEBIT-FILE.
           PERFORM WRITE-IT.
           MOVE DEBIT-IFN TO E-FILE.
           MOVE DEBIT-KEY TO E-KEYX.
           WRITE DEBIT-REC.
           IF IO-FG = 8
              GO TO WRITE-DEBIT-FILE.
       CLOSE-DEBIT-FILE.
           PERFORM CLOSE-IT.
           CLOSE DEBIT-FILE.
 
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
           MOVE WK2-PATH TO ACCESS-BUF.
           PERFORM REMOVE-WORKFILE.
 
       EXIT-PROG.

           IF SV-BEG-BR NOT = 9999
              IF EXT-EOBUF-GROUP-FG = "Y"
                 MOVE SV-BEG-BR TO BEG-BRANCH
                 MOVE SV-END-BR TO END-BRANCH
                 MOVE SV-GROUP-FLAG TO GROUP-FLAG
                 MOVE SV-ENT-GROUP TO ENT-GROUP
                 MOVE SV-LOCAL-PRINT-FG TO LOCAL-PRINT-FG
              ELSE
                 MOVE SV-BEG-BR TO BEG-BRANCH
                 MOVE SV-END-BR TO END-BRANCH.
 
           IF EXT-ROUTE-BUF = "00"
              MOVE "SP/EXTRMU" TO FORM-NAM
              MOVE FORM-PATH TO FORM-PATHNAME.
 
       STOP-RUN.
           IF EXT-ROUTE-BUF = "00" OR "40" OR "50"
              DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN
              DESTROY DEBEXT-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.
           EXIT PROGRAM.
