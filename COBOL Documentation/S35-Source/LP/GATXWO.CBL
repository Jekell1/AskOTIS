       IDENTIFICATION DIVISION.
       PROGRAM-ID.   GATXWO.
       DATE-WRITTEN.  12/08/04.
      ******************************************************************
      *
      *                      (LP)
      *    DESCRIPTION:  GEORGIA TAX REPORTS
      *                  RECOVERED LOANS (PREVIOUSLY CHARGED OFF)
      *                  ALL TRANSACTIONS ON "PL"'S
      *                  (TAKEN FROM EMWORG)
      *
      * REV:
      *  BAH 041208 NEW FOR WORLD GA TAX REPORT FORM, WORLD PR# 364
      *  BAH 050110 SKIP "WI", JIMR
      *  BAH 2016.1103 CHANGE TILLNO TO 9(2)
      *  BAH 170602 ACTIVATE 8 DIGIT DATES, PD0006
      *  BAH 190424 CHANGED DS-CLASS FROM XX TO 99, WAS TRUNCATING CLASS
      *                                                         #QA0008
      *  BAH 20190926 REMOVED ACCESS-CALL LNFILE, TRP
      *  BLM 20210810 REMOVE TP-CODE, TP-POOLID NO LONGER IN KEY
      *  BAH 2024.0819 ONLY PRINT LAST 4 OF SSNO #1673 S35Q-118
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
       I-O-CONTROL.
       DATA DIVISION.
       FILE SECTION.
       COPY "LIBGB/GBFDPR.CPY".
       FD  WK-FILE
           LABEL RECORDS ARE STANDARD.
      *-------------------------------------------------
       01  WK-REC.
           03  WK-KEY.
               05  WK-CODE  PIC X.
               05  WK-BRNO  PIC 9(4).
               05  WK-CLASS PIC 9(3).
               05  WK-TRCD  PIC XX.
           03  WK-NO        PIC S9(7)  COMP-3.
           03  WK-TRAMT     PIC S9(9)V99  COMP-3.
           03  WK-APOTH     PIC S9(9)V99  COMP-3.
           03  WK-APINT     PIC S9(9)V99  COMP-3.
           03  WK-APLC      PIC S9(9)V99  COMP-3.
           03  WK-APCUR    PIC S9(9)V99  COMP-3.
           03  WK-EARNINGS  PIC S9(9)V99  COMP-3.
           03  WK-DLPROC    PIC S9(9)V99  COMP-3.
       01  WK-GL-REC.
           03  WK-GL1-KEY.
               05  FILLER   PIC X(7).
               05  WK-GLNO  PIC 9(11)     COMP-3.
           03  WK-GLAMT     PIC S9(7)V99  COMP-3.
           03  FILLER       PIC X(41).

      ******************************************************************
      *  INDEXED TRFILE BY BRANCH, CLASS, ACCOUNT
      ******************************************************************
       FD  WK2-FILE
           LABEL RECORDS ARE STANDARD.
       01  WK2-REC.
           03  WK2-KEY.
               05  WK2-BRANCH            PIC 9(4).
               05  WK2-CLASS             PIC 9(3).
               05  WK2-ACCOUNT           PIC 9(8).
               05  WK2-DATE              PIC 9(8).
      *        05  WK2-CODE              PIC X.
               05  WK2-LINE              PIC 9(3).

      ******************************************************************
      *
      *     W O R K I N G - S T O R A G E    S E C T I O N
      *
      ******************************************************************
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(43) VALUE
           "@(#)./LP/GATXWO.CBL S34 08/11/21-11:29:44 >".

      ******************************************************************
      *     F I L E   I F N      *
      ******************************************************************
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
       COPY "LIBLP/LP01LC.CPY".
       COPY "LIBLP/LP01LC_SQL.CPY".
       COPY "LIBLP/LPWSLC.CPY".
       COPY "LIBLP/LP01LN.CPY".
       COPY "LIBLP/LP01LN_SQL.CPY".
       COPY "LIBLP/LPWSLN.CPY".
       COPY "LIBLP/LP01SP.CPY".
       COPY "LIBLP/LP01SPA.CPY".
       COPY "LIBLP/LP01SPB.CPY".
       COPY "LIBLP/LP01SPC.CPY".
       COPY "LIBLP/LP01SP_SQL.CPY".
       COPY "LIBLP/LPWSSP.CPY".
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       01  WK2-IFN              PIC X(3)     VALUE "WK2".
       01  WK-IFN               PIC X(2)     VALUE "WK".
       01  WK2-PATH             PIC X(35).


      ******************************************************************
      *   LINE WORKERS     *
      ******************************************************************
       01  LN-WORKERS.
           03  LN-FEED          PIC 999      VALUE 0.
           03  LN-COUNT         PIC 999      VALUE 99.
           03  LN-WK            PIC 999      VALUE 0.
       01  PAGE-NUMBER          PIC 9(4)     VALUE 0.

       01  ER-FEED              PIC 9        VALUE 1.

      ******************************************************************
      *   MISC WORKERS     *
      ******************************************************************
       01  HOLD-EARNINGS        PIC S9(7)V99  COMP-3 VALUE 0.
       01  HOLD-TP-OTHBR-FG     PIC X                VALUE "?".
       01  HOLD-TP-ACCTNO       PIC 9(8)      COMP-3 VALUE 99999999.
       01  HOLD-TP-SSNO         PIC 9(10)     COMP-3 VALUE 9999999999.
       01  SUB                  PIC 9(6)      COMP-3.
       01  LEV                  PIC 99        COMP-3.
       01  LEV-PL               PIC 99        COMP-3.
       01  LEV-NEXT-1           PIC 99        COMP-3.
       01  HOLD-BRNO            PIC 9(4)  VALUE 0.
       01  HOLD-CLASS           PIC 9(3)  VALUE 0.
       01  RECORDS-FOUND        PIC X     VALUE " ".
           88  NO-RECORDS-FOUND           VALUE "N".
       01  SUMMARY-FG           PIC X     VALUE "N".
       01  GRAND-SUMMARY     PIC X     VALUE "N".
       01  NAME-WORKER          PIC X(34).
       01  FIRST-NAME           PIC X(16).

       01  FIRST-TIME           PIC X        VALUE "Y".
       01  MESS-LIST            PIC X(5)     VALUE "MESS.".
       01  MSEL-LIST            PIC X(5)     VALUE "MSEL.".

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
               05  WS-GTT-APOTH           PIC S9(9)V99  COMP.
               05  WS-GTT-APINT           PIC S9(9)V99  COMP.
               05  WS-GTT-APLC            PIC S9(9)V99  COMP.
               05  WS-GTT-APCUR          PIC S9(9)V99  COMP.
               05  WS-GTT-EARNINGS        PIC S9(9)V99  COMP.
               05  WS-GTT-DLPROC          PIC S9(9)V99  COMP.

      *-----------------------------------------------------------------
       01  WS-HOLD-PROMPT       PIC X(7)  VALUE SPACES.

      ******************************************************************
      *  TOTALS LEV1 = CLASS TOTALS    | LEV-PL = 1 --- P
      *            2 = BRANCH TOTALS   | LEV-PL = 2 --- 2
      *            3 = GRAND TOTALS    | LEV-PL = 3 --- 3
      *                                | LEV-PL = 4 --- 4
      *                                | LEV-PL = 5 --- 5
      *                                | LEV-PL = 6 --- TOTAL
      ******************************************************************
       01  TOTALS-TAB   VALUE ZEROS.
           03  TOTAL-TBL       OCCURS 3.
               05  TOTAL-TBL2  OCCURS 6.
                   07  T-NO         PIC S9(7)    COMP-3.
                   07  T-TRAMT      PIC S9(9)V99 COMP-3.
                   07  T-APOTH      PIC S9(9)V99 COMP-3.
                   07  T-APINT      PIC S9(9)V99 COMP-3.
                   07  T-APLC       PIC S9(9)V99 COMP-3.
                   07  T-APCUR      PIC S9(9)V99 COMP-3.
                   07  T-EARNINGS   PIC S9(9)V99 COMP-3.
                   07  T-DLPROC     PIC S9(7)V99 COMP-3.
      *         RECOVERIES
                   07  T-R-NO       PIC S9(7)    COMP-3.
                   07  T-R-TRAMT    PIC S9(9)V99 COMP-3.
                   07  T-R-APOTH    PIC S9(9)V99 COMP-3.
                   07  T-R-APINT    PIC S9(9)V99 COMP-3.
                   07  T-R-APLC     PIC S9(9)V99 COMP-3.
                   07  T-R-APCUR    PIC S9(9)V99 COMP-3.
                   07  T-R-EARNINGS PIC S9(9)V99 COMP-3.
                   07  T-R-DLPROC   PIC S9(7)V99 COMP-3.
       01  PTYPE.
           03  PLTYPE               PIC X(6) VALUE
               "P23456".
           03  PL-TYPE REDEFINES PLTYPE
                  OCCURS 6 TIMES    PIC X(1).

       01  CLASS-FG             PIC X(3)       VALUE "   ".
           88  CLASS-BEG                       VALUE "BEG".
           88  CLASS-END                       VALUE "END".
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       COPY "LIBLP/EMSFRG_CMD.CPY".
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       01  LEGEND               PIC X(70).

      ******************************************************************
      *     S C R E E N   F I E L D S   *
      ******************************************************************
       COPY "LIBGB/EOBUF_EXT.CPY".
           03  BEG-BRANCH      PIC 9(4).
           03  END-BRANCH      PIC 9(4).
           03  BEG-CLASS       PIC 99.
           03  END-CLASS       PIC 99.
           03  BEG-DATE.
               05  BEG-MONTH   PIC 99.
               05  BEG-YEAR    PIC 99.
           03  BDATE REDEFINES BEG-DATE PIC 9999.
           03  END-DATE.
               05  END-MONTH   PIC 99.
               05  END-YEAR    PIC 99.
           03  EDATE REDEFINES END-DATE PIC 9999.
           03  GTT-ISO         PIC X(1).
           03  SUM-ONLY        PIC X.
           03  GROUP-FLAG      PIC X.
           03  ENT-GROUP       PIC X(4).
           03  LOCAL-PRINT-FG  PIC X(1).
           03  TOTAL-LEV-TABLE.
               05  TOTAL-LEVEL-TABLE  PIC X(1)  OCCURS 10.

       01  VERSION-CONTROL     PIC X VALUE "D".
       01  BEGINNING-DATE       PIC 9(8).
       01  ENDING-DATE          PIC 9(8).

       01  SV-BEG-BR           PIC 9(4)        VALUE 9999.
       01  SV-END-BR           PIC 9(4)        VALUE 9999.
       01  SV-GROUP-FLAG       PIC X     VALUE SPACES.
       01  SV-ENT-GROUP        PIC X(4)  VALUE SPACES.
       01  SV-LOCAL-PRINT-FG   PIC X     VALUE SPACES.

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
           03  FILLER           PIC X(10)    VALUE "USER ID:".
           03  H-USERID         PIC X(10).
           03  FILLER           PIC X(30)    VALUE " ".
           03  FILLER           PIC X(38) VALUE
               "    WRITE-OFF ACTIVITY REGISTER".

       01  HEAD3.
           03  FILLER           PIC X(10) VALUE "BRANCH :".
           03  H-BRNO           PIC 9(4).
           03  FILLER           PIC X VALUE " ".
           03  H-BRDESC         PIC X(20).
           03  FILLER           PIC X(20) VALUE " ".
           03  H-DATE1          PIC 99/99/99.
           03  FILLER           PIC X(6) VALUE " THRU ".
           03  H-DATE2          PIC 99/99/99.

       01  HEAD4.
           03  FILLER           PIC X(10) VALUE "CLASS  :".
           03  H-LCNO           PIC 99.
           03  FILLER           PIC X VALUE " ".
           03  H-LCDESC         PIC X(20).

       01  HEAD5.
           03  FILLER           PIC X(18)    VALUE "ACCTNO".
           03  FILLER           PIC X(28)    VALUE
              "DEALER          IB".
           03  FILLER           PIC X(52)    VALUE
           "TR                      - APPLIED -".
           03  FILLER           PIC X(34)    VALUE
           "---- BALANCES -----          G/L".

       01  HEAD6.
           03  FILLER           PIC X(11)      VALUE
           "    TL".
           03  FILLER           PIC X(54)     VALUE
           "RATE   PROC'D   EARNED PC PAY-DATE CD REFNO   AMOUNT".
           03  FILLER           PIC X(45)     VALUE
           "OT/INT/LC   PRIN  PDTH LCPD     OTH      INT".
           03  FILLER           PIC X(18)     VALUE
           "PRIN      ACCT-AMT".

       01  CLASS-HEAD.
           03  FILLER           PIC X(55)    VALUE
           "          TRANS CODE   NUMBER     AMOUNT    APP-OTH    ".
           03  FILLER           PIC X(48)    VALUE
           "     APP-INT      APP-LC      APP-PRIN  EARNINGS".
           03  FILLER           PIC X(11)    VALUE
           "    DL-PROC".
       01  GRAND-HEAD.
           03  FILLER           PIC X(55)    VALUE
           "CLASS     TRANS CODE   NUMBER     AMOUNT    APP-OTH    ".
           03  FILLER           PIC X(48)    VALUE
           "     APP-INT      APP-LC      APP-PRIN  EARNINGS".
           03  FILLER           PIC X(11)    VALUE
           "    DL-PROC".

      *-----------------------------------------------------------------
       01  GTT-HEAD1.
           03  FILLER            PIC X(57)  VALUE SPACES.
           03  FILLER            PIC X(18)  VALUE "GROUP TOTALS TABLE".
           03  FILLER            PIC X(57)  VALUE SPACES.

      *-----------------------------------------------------------------
       01  GTT-HEAD2.
           03  FILLER            PIC X(24)  VALUE
               "BRANCH/GROUP   GR       ".
           03  FILLER            PIC X(9)   VALUE "NUMBER   ".
           03  FILLER            PIC X(11)  VALUE "TRANS      ".
           03  FILLER            PIC X(16)  VALUE "APPLIED         ".
           03  FILLER            PIC X(12)  VALUE "APPLIED     ".
           03  FILLER            PIC X(12)  VALUE "APPLIED     ".
           03  FILLER            PIC X(14)  VALUE "APPLIED       ".
           03  FILLER            PIC X(12)  VALUE "EARNINGS    ".
           03  FILLER            PIC X(11)  VALUE "DEALER     ".

      *-----------------------------------------------------------------
       01  GTT-HEAD3.
           03  FILLER            PIC X(14)  VALUE SPACES.
           03  FILLER            PIC X(10)  VALUE "LEV       ".
           03  FILLER            PIC X(7)   VALUE "       ".
           03  FILLER            PIC X(11)  VALUE "  AMOUNT   ".
           03  FILLER            PIC X(16)  VALUE "  OTHER         ".
           03  FILLER            PIC X(12)  VALUE "  INTEREST  ".
           03  FILLER            PIC X(12)  VALUE "  LT-CHG    ".
           03  FILLER            PIC X(20)  VALUE "  PRINCIPAL        ".
           03  FILLER            PIC X(16)  VALUE "        PROCEDES".

       01  GL-HEAD.
           03  FILLER           PIC X(17)    VALUE "G/L DIST  SUMMARY".

       01  DETAIL-LINE          VALUE SPACES.
           03  FILLER           PIC X(118).
           03  D-BRXX           PIC X(12).
           03  D-BRX            PIC 9(4).

       01  D-LINE1 REDEFINES DETAIL-LINE.

           03  D-DISPLAY-LNNO   PIC X(16).
           03  FILLER           PIC X(03).
           03  D-NAME           PIC X(26).
           03  FILLER           PIC X.
           03  D-MAKER          PIC X.
           03  FILLER           PIC XX.
           03  D-SSNO5          PIC X(7).
           03  D-SSNO           PIC 9999.
           03  FILLER           PIC X(5).
           03  D-DLNO           PIC 9999.99.

       01  D-LINE2 REDEFINES DETAIL-LINE.
           03  FILLER           PIC X(4).
           03  D-TILL           PIC Z9.
           03  FILLER           PIC X(9).
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
           03  D-APCUR          PIC ZZZZZ.ZZ-.
           03  FILLER           PIC X.
           03  D-PDTHRU-MO      PIC 99.
           03  D-PDTHRU-DAYR    PIC 99.
           03  FILLER           PIC X.
           03  D-LCPD-MO        PIC 99.
           03  D-LCPD-DAYR      PIC 99.
           03  D-OTHBAL         PIC ZZZZZ.ZZ-.
           03  D-INTBAL         PIC ZZZZZ.ZZ-.
           03  D-PRINBAL        PIC ZZZZ9.99-.
           03  FILLER           PIC X(2).
           03  D-GLNO           PIC ZZZ9/9999999.
           03  D-GLAMT          REDEFINES D-GLNO
                                PIC ZZZZZZZZ.ZZ-.
           03  D-PAIDOFF        REDEFINES D-GLNO
                                PIC X(12).

       01  D-LINE4 REDEFINES DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           03  D2-GROUP             PIC X(19).
           03  D2-GROUP-1           REDEFINES D2-GROUP.
               05  DS-CLASS         PIC 99.
               05  FILLER           PIC X(17).
           03  D2-GROUP-2           REDEFINES D2-GROUP.
               05  D2-PROMPT1       PIC X(7).
               05  FILLER           PIC X(2).
               05  D2-GRNO          PIC X(4).
               05  D2-BRNO          REDEFINES D2-GRNO
                                    PIC 9(4).
               05  FILLER           PIC X(3).
               05  D2-GR-LEVEL      PIC 9(1).
               05  FILLER           PIC X(2).
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           03  DS-TRCD          PIC XX.
           03  FILLER           PIC XX.
           03  DS-NO            PIC ZZZ-.
           03  FILLER           PIC X.
           03  DS-TRAMT         PIC ZZZZZZZZZ.ZZ-.
           03  DS-APOTH         PIC ZZZZZZZZZ.ZZ-.
           03  DS-APINT         PIC ZZZZZZZZZ.ZZ-.
           03  DS-APLC          PIC ZZZZZZZZZ.ZZ-.
           03  DS-APCUR        PIC ZZZZZZZZZ.ZZ-.
           03  DS-EARNINGS      PIC ZZZZZZZZZ.ZZ-.
           03  DS-DLPROC        PIC ZZZZZZZZZ.ZZ-.


       01  D-LINE5 REDEFINES DETAIL-LINE.
           03  DS-GLNO          PIC ZZZ9/9999999.
           03  FILLER           PIC XX.
           03  DS-GLAMT         PIC ZZZZ9.99-.

      *-----------------------------------------------------------------
       01  RANGE-LINE REDEFINES DETAIL-LINE.
           03  FILLER           PIC X(35).
           03  RANGE-SELECTION.
               04  R-NUM        PIC Z(5)9.
               04  R-NUM-REDEF  REDEFINES R-NUM.
                   05 R-CHAR    PIC X(6).
      
       COPY "LIBWI/GRSCANW.CPY".
       COPY "LIBGB/GRWORKW.CPY".
       COPY "LIBWI/LCSCANW.CPY".
       COPY "LIBWI/GRTOTLW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBGB/MKTEMPW_CMD.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/DACCESSW.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBLP/LPMDTEW.CPY".
       COPY "LIBLP/LPSTATW.CPY".
       COPY "LIBGB/PASSWDW.CPY".
       COPY "LIBGB/BRSECUREW.CPY".
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       COPY "LIBGB/SYSTEMW.CPY".
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       COPY "LIBGB/EOSPOOLW.CPY".
       COPY "LIBGB/GETPRW.CPY".

       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBLP/GATXWO_DEF.CPY".
       COPY "LIBLP/GATXWO_WKS.CPY".

      *****************************************************************
      *
      *     P R O G R A M   C O N T R O L
      *
      *****************************************************************
       COPY "LIBGB/GETFMW.CPY".

       SCREEN SECTION.
       COPY "LIBLP/GATXWO_SCN.CPY".

       PROCEDURE DIVISION
            USING FORM-PATHNAME EXIT-PATHNAME.
       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
               PR-FILE WK-FILE WK2-FILE.
       COPY "LIBGB/DECLRP.CPY".
           PERFORM CLOSE-TRP1-FILE.
           PERFORM CLOSE-SP1-FILE.
           PERFORM CLOSE-LN1-FILE.
           PERFORM CLOSE-LC1-FILE.
           PERFORM CLOSE-BW1-FILE.
           PERFORM CLOSE-GR1-FILE.
           PERFORM CLOSE-BR-FILE.
           PERFORM CLOSE-PR-FILE.
           CLOSE WK-FILE WK2-FILE.
       COPY "LIBGB/DECLRP2.CPY".

      *****************************************************************
      *
      *     M A I N   P R O G R A M
      *
      *****************************************************************
       MAIN-MODULE SECTION.

           PERFORM SQL-CONNECT.

      D    STOP MESS.
           MOVE FORM-PATHNAME  TO FORM-PATH.
           MOVE "GATXWO"       TO FORM-PROG VDU-FORM-NAME.

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

           MOVE EXT-FILPATH-BASE           TO FILPATH-BASE.
           MOVE BR-MACHINE                 TO FILPATH-MACHINE.
           MOVE BR-DATA-PATH               TO FILPATH-DATA.

           MOVE "B"                        TO BW-PATH-OVERRIDE
                                              LN-PATH-OVERRIDE
                                              TRP-PATH-OVERRIDE.

           PERFORM OPEN-BW1-FILE.
           PERFORM OPEN-LN1-FILE.
           PERFORM OPEN-TRP1-FILE.

           PERFORM SORT-ROUTINE.

           PERFORM OPEN-WK2-FILE.

      *----------------------------------------------------------------
      * AT THIS POINT, ALL TRFILES HAVE BEEN READ
      *----------------------------------------------------------------
       NEXT-TR-REPORT.
           PERFORM READ-WK2-FILE-NEXT.
           IF IO-FG = 9
              GO TO END-OF-GROUP.

           MOVE WK2-DATE    TO TP-DATE.
           MOVE WK2-BRANCH  TO TP-BRNO.
           MOVE WK2-CLASS   TO TP-CLASS.
           MOVE WK2-ACCOUNT TO TP-ACCTNO.
           MOVE WK2-LINE    TO TP-LINE.

           PERFORM READ-TRP1-FILE.
           IF IO-FG = 9
              MOVE "TRANSACTION RECORD MISSING SINCE SORT" TO MESS
              PERFORM SEND-MESS
              GO TO IO-ERROR.

           IF FIRST-TIME = "Y"
              MOVE TP-BRNO  TO HOLD-BRNO
              MOVE TP-CLASS TO HOLD-CLASS
              PERFORM SET-CLASS-HEADINGS
              PERFORM SET-BRANCH-HEADINGS.

           IF TP-BRNO NOT = HOLD-BRNO
              PERFORM CLASS-TOTALS
      *------ PERFORM GL-SUMMARY
              PERFORM BRANCH-TOTALS
              MOVE TP-BRNO TO HOLD-BRNO
              MOVE TP-CLASS TO HOLD-CLASS
              PERFORM SET-CLASS-HEADINGS
              PERFORM SET-BRANCH-HEADINGS
              IF ( GTT-ISO NOT = "O" )
                 IF SUM-ONLY = "N"
                    PERFORM HEAD-PAGE.

           IF TP-CLASS NOT = HOLD-CLASS
              PERFORM CLASS-TOTALS
      *-------PERFORM GL-SUMMARY
              MOVE TP-CLASS TO HOLD-CLASS
              PERFORM SET-CLASS-HEADINGS
              IF ( GTT-ISO NOT = "O" )
                 IF SUM-ONLY = "N"
                    PERFORM HEAD-PAGE.

           PERFORM DETAIL-LINE-PROCESS.

           MOVE 1 TO LEV.
           PERFORM CREATE-WK.

           GO TO NEXT-TR-REPORT.

      *----------------------------------------------------------------
       END-OF-GROUP.
           PERFORM CLOSE-TRP1-FILE.
           PERFORM CLOSE-BW1-FILE.
           PERFORM CLOSE-LN1-FILE.
           PERFORM CLOSE-WK2-FILE.
           PERFORM OPEN-WK2-FILE-OUTPUT.

           IF ( LOCAL-PRINT-FG = "Y" )
              IF ( FIRST-TIME NOT = "Y" )
                 PERFORM CLASS-TOTALS
      *----------PERFORM GL-SUMMARY
                 PERFORM BRANCH-TOTALS
                 MOVE "Y" TO FIRST-TIME
                 MOVE 0 TO PAGE-NUMBER
              ELSE
                 MOVE 0 TO PAGE-NUMBER.

           GO TO NEXT-GROUP-LOC.

      *----------------------------------------------------------------
       END-ROUTINE.
           IF NO-RECORDS-FOUND
              GO TO END-PROGRAM.

           IF LOCAL-PRINT-FG = "N"
              PERFORM CLASS-TOTALS
      *-------PERFORM GL-SUMMARY
              PERFORM BRANCH-TOTALS
              MOVE "Y" TO GRAND-SUMMARY
              PERFORM GRAND-TOTALS
      *-------PERFORM GL-SUMMARY
              IF ( GTT-ISO NOT = "S" )
                 MOVE 99 TO LN-COUNT
                 PERFORM GROUP-TOTAL-TABLE
                 PERFORM PRINT-RANGE-LINES
                 PERFORM GL-SUMMARY
              ELSE
                 PERFORM PRINT-RANGE-LINES
                 PERFORM GL-SUMMARY.

           GO TO END-PROGRAM.

      *****************************************************************
       INITIALIZATION SECTION.
      *****************************************************************

           MOVE GATXWO-QUEUE-POS TO Q-POS.
           MOVE GATXWO-COPIES-POS TO C-POS.
           MOVE GATXWO-PRU-POS   TO P-POS.
           MOVE SPACES TO Q-BUF C-BUF P-BUF.

       COPY "LIBGB/EOINIT.CPY".

           MOVE SPACES TO RECORDS-FOUND.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      * CREATE INDEXED KEY FILE FOR TRFILE OUTPUT
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE MKTEMP-DEFAULT TO MKTEMP-BUF.
           PERFORM MKTEMP-CALL.
           IF STAT NOT = "00"
              MOVE "CANNOT MAKE WK2 SORT KEY FILE" TO MESS
              PERFORM SEND-MESS
              GO TO ENDIT.

           MOVE MKTEMP-BUF TO WK2-PATH.
           PERFORM OPEN-WK2-FILE-OUTPUT.

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
                 MOVE EXT-CONAME-USER-LOC TO BEG-BRANCH END-BRANCH.

           IF EXT-ROUTE-BUF = "00"
              MOVE "REPORT IN PROGRESS............" TO WR-BUF
              MOVE MESS-LIST TO WR-LIST
              PERFORM CALL-WRFORM.

      * SET DATE RANGE FOR TESTING TR FILES
           MOVE BEG-MONTH TO DATE-MMDDYY-MM.
           MOVE BEG-YEAR  TO DATE-MMDDYY-YY.
           MOVE 01        TO DATE-MMDDYY-DD.
           PERFORM CONVERT-MMDDYY-TO-YYYYMMDD.
           MOVE DATE-YYYYMMDD TO BEGINNING-DATE.

           MOVE END-MONTH TO DATE-MMDDYY-MM.
           MOVE END-YEAR  TO DATE-MMDDYY-YY.
           MOVE 31        TO DATE-MMDDYY-DD.
           PERFORM CONVERT-MMDDYY-TO-YYYYMMDD.
           MOVE DATE-YYYYMMDD TO NDTE-DATE.
           PERFORM NEW-DATE-CALCULATION.
           MOVE NDTE-DATE TO ENDING-DATE.

           IF ( LOCAL-PRINT-FG NOT = "Y" )
              PERFORM OPEN-PR-FILE.

           PERFORM OPEN-BR-FILE.
           PERFORM OPEN-GR1-FILE.
           PERFORM OPEN-SP1-FILE.
           PERFORM OPEN-LC1-FILE.

           MOVE 1 TO SUB.
           PERFORM ZERO-TOTAL-TBL
              VARYING LEV-PL FROM 1 BY 1
                 UNTIL LEV-PL > 6.
           MOVE 2 TO SUB.
           PERFORM ZERO-TOTAL-TBL
              VARYING LEV-PL FROM 1 BY 1
                 UNTIL LEV-PL > 6.
           MOVE 3 TO SUB.
           PERFORM ZERO-TOTAL-TBL
              VARYING LEV-PL FROM 1 BY 1
                 UNTIL LEV-PL > 6.

           MOVE 0 TO PAGE-NUMBER.

           MOVE MKTEMP-DEFAULT TO MKTEMP-BUF.
           PERFORM MKTEMP-CALL.
           PERFORM OPEN-WK-FILE-OUTPUT.
           PERFORM CLOSE-WK-FILE.
           PERFORM OPEN-WK-FILE.

      *----------------------------------------------------------------
       SET-HEAD.
           MOVE EXT-CONAME-CONAME TO H-NAME.
           MOVE EXT-CONAME-SYSDATE TO H-DATE.
           MOVE EXT-CONAME-USER-ID TO H-USERID.

           MOVE BEG-MONTH   TO DATE-MMDDYY-MM.
           MOVE BEG-YEAR    TO DATE-MMDDYY-YY.
           MOVE 1           TO DATE-MMDDYY-DD.
           MOVE DATE-MMDDYY TO H-DATE1.

           MOVE END-MONTH TO DATE-MMDDYY-MM.
           MOVE END-YEAR  TO DATE-MMDDYY-YY.
           MOVE 50        TO DATE-MMDDYY-DD.
           PERFORM CONVERT-MMDDYY-TO-YYYYMMDD.
           MOVE DATE-YYYYMMDD TO NDTE-DATE.
           PERFORM NEW-DATE-CALCULATION.
           MOVE NDTE-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO H-DATE2.

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

      *****************************************************************
       EO-EXEC-AUDIT SECTION.
      *****************************************************************
           MOVE EXT-EOBUF-DATE1 TO NUM-DATE.
           MOVE NUM-MO TO BEG-MONTH.
           MOVE NUM-YR TO BEG-YEAR.
           MOVE EXT-EOBUF-DATE2 TO NUM-DATE.
           MOVE NUM-MO TO END-MONTH.
           MOVE NUM-YR TO END-YEAR.

      *****************************************************************
       ENTER-PARAMETERS SECTION.
      *****************************************************************
       ENTER-PARM.
           IF VDUSTATUS = "1U"
              IF ( GTT-ISO = "O" )
                 GO TO GTT-ISO-PARAGRAPH
              ELSE
                 GO TO SUMYN.

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

       BRANCH-IS-NUMERIC.
           MOVE "N" TO GROUP-FLAG.
           MOVE WGR-SRESULT TO BEG-BRANCH.
           MOVE "SN N040BR1." TO VDU.
           MOVE BEG-BRANCH TO VDUNUM0.
           PERFORM SCREENIO.

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

       ALL-BR.
           MOVE "SNNN040BR1." TO VDU.
           MOVE 0 TO BEG-BRANCH VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN040BR2." TO VDU.
           MOVE 9999 TO END-BRANCH VDUNUM0.
           PERFORM SCREENIO.
           MOVE "S  A160DISPLAY-USER." TO VDU.
           MOVE SPACES TO VDUBUF.
           PERFORM SCREENIO.
           MOVE "N" TO GROUP-FLAG.

       CHECK-BRANCH-SECURITY.
           PERFORM BR-RANGE-SECURITY.
           IF BR-SECURITY-OKAY = "N"
              GO TO BR-01.

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
           GO TO ENTER-BEG-DATE.

       ALL-CL.
           MOVE "SNNN020CL1." TO VDU.
           MOVE 0 TO BEG-CLASS VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN020CL2." TO VDU.
           MOVE 99 TO END-CLASS VDUNUM0.
           PERFORM SCREENIO.

       ENTER-BEG-DATE.
           IF EXT-ROUTE-BUF NOT = "00"
              GO TO GTT-ISO-PARAGRAPH.

           MOVE SPACES TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "ENNN040BEGDATE." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO CL-02.
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
              GO TO GTT-ISO-PARAGRAPH.

           MOVE SPACES TO LEGEND.
           PERFORM SEND-LEGEND.
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

           IF ( VDUSTATUS = "10" ) GO TO EXIT-PARM.
           IF ( VDUSTATUS = "1>" ) GO TO EXIT-PARM.
           IF ( VDUSTATUS = "1U" ) GO TO CL-02.
           IF NOT ( VDUSTATUS = "00" OR "1D" )
              GO TO GTT-ISO-PARAGRAPH.

           IF NOT ( VDUBUF = "I" OR "S" OR "O" )
              GO TO GTT-ISO-PARAGRAPH.

           MOVE VDUBUF TO GTT-ISO.

           IF ( GTT-ISO = "I" OR "O" )
              PERFORM GRTOTL-WINDOW
           ELSE
              MOVE SPACES TO TOTAL-LEV-TABLE.

       SUMYN.
           IF ( GTT-ISO = "O" )
              MOVE "S  A010SUMONLY." TO VDU
              MOVE "Y" TO SUM-ONLY VDUBUF
              PERFORM SCREENIO
              GO TO LOCAL-PRINT.

           MOVE "E  A010SUMONLY." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.

           IF ( VDUSTATUS = "1U" )
              IF  ( GROUP-FLAG = "Y" )
                  GO TO GTT-ISO-PARAGRAPH.

           IF VDUSTATUS = "1U"
              IF EXT-ROUTE-BUF = "00"
                 GO TO ENTER-END-DATE
              ELSE
                 GO TO CL-02.
           IF VDUSTATUS NOT = "00" AND NOT = "1D"
              GO TO SUMYN.

           MOVE VDUBUF TO SUM-ONLY.
           IF SUM-ONLY NOT = "N" AND NOT = "Y" GO TO SUMYN.

       LOCAL-PRINT.
           IF EXT-ROUTE-BUF NOT = "40"
              IF EXT-ROUTE-BUF NOT = "50"
                 MOVE "N" TO LOCAL-PRINT-FG
                 GO TO END-PARM.

           IF BEG-BRANCH = END-BRANCH
              MOVE "N" TO LOCAL-PRINT-FG
              GO TO END-PARM.

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
              IF ( GROUP-FLAG = "Y" )
                 IF ( GTT-ISO = "O" )
                    GO TO GTT-ISO-PARAGRAPH
                 ELSE
                    GO TO SUMYN
              ELSE
                 GO TO SUMYN.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO ASK-LOCAL.

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

      *****************************************************************
       DISPLAY-PARAMETERS SECTION.
      *****************************************************************
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
           MOVE "S  A010GTT-ISO." TO VDU.
           MOVE GTT-ISO           TO VDUBUF.
           PERFORM SCREENIO.
           MOVE "S  A010SUMONLY." TO VDU.
           MOVE SUM-ONLY          TO VDUBUF.
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

      *****************************************************************
       SORT-ROUTINE SECTION.
      *****************************************************************

           MOVE BR-NO               TO TP-BRNO
                                       QTRP1-WBEG-BRNO
                                       QTRP1-WEND-BRNO.
           MOVE BEGINNING-DATE      TO TP-DATE SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO QTRP1-WBEG-DATE.
           MOVE ENDING-DATE         TO  SQL-DATE-YYYYMMDD.
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

       SORT-TR-NEXT.
           PERFORM READ-TRP1-FILE-NEXT.
           IF (IO-FG NOT = 0) OR (TRP-PATH-OWNBR NOT = TP-BRNO)
              GO TO SORT-ROUTINE-EXIT.

           IF TP-DATE > ENDING-DATE
              GO TO SORT-ROUTINE-EXIT.

           IF TP-CLASS < BEG-CLASS OR TP-CLASS > END-CLASS
              GO TO SORT-TR-NEXT.

      *  ONLY INCLUDE RECOVERIES POSTED TO THIS BRANCH
           IF TP-OTHBR-FG = "C"
              GO TO SORT-TR-NEXT.

           IF TP-TRCD = "PL" OR "P2" OR "P3" OR "P4" OR "P5" OR "WI"
              GO TO SORT-TR-NEXT.

           IF TP-PLCD  = SPACES
              GO TO SORT-TR-NEXT.

           MOVE TP-DATE   TO WK2-DATE.
           MOVE TP-BRNO   TO WK2-BRANCH.
      *    MOVE TP-CODE   TO WK2-CODE.
           MOVE TP-CLASS  TO WK2-CLASS.
           MOVE TP-ACCTNO TO WK2-ACCOUNT.
           MOVE TP-LINE   TO WK2-LINE.

           PERFORM WRITE-WK2-FILE.
           IF IO-FG NOT = 0
              GO TO IO-ERROR.

           GO TO SORT-TR-NEXT.

       SORT-ROUTINE-EXIT.
           PERFORM CLOSE-WK2-FILE.


      *****************************************************************
       DETAIL-LINE-PROCESS SECTION.
      *****************************************************************
           IF FIRST-TIME = "Y"
              MOVE EXT-CONAME-RPT-LINES TO LN-COUNT
              MOVE 1 TO LN-FEED
              MOVE "Y" TO RECORDS-FOUND
              MOVE "N" TO FIRST-TIME.

      *- - - - - - - - - - - - - - - - - - - -{ GET ACCTNO WITH STATUS }
           PERFORM GET-DISPLAY-LNNO.
           MOVE LPS-DISPLAY-LNNO TO D-DISPLAY-LNNO.

           PERFORM FIND-BORROWER.
           MOVE TP-MAKER  TO D-MAKER.
           MOVE "XXX-XX-" TO D-SSNO5.
           MOVE TP-SSNO   TO D-SSNO.
           MOVE TP-DLNO   TO D-DLNO.

           IF HOLD-TP-ACCTNO = TP-ACCTNO AND
               HOLD-TP-OTHBR-FG = TP-OTHBR-FG AND
                HOLD-TP-SSNO = TP-SSNO
              MOVE SPACES TO DETAIL-LINE
           ELSE
              MOVE 1 TO LN-FEED
              MOVE TP-ACCTNO   TO HOLD-TP-ACCTNO
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
           MOVE 0 TO HOLD-EARNINGS.

           IF TP-SERVACCT = "N"
              COMPUTE HOLD-EARNINGS = TP-EARNINGS(1) + TP-EARNINGS(2) +
                                      TP-EARNINGS(3) + TP-EARNINGS(4) +
                                      TP-EARNINGS(5) + TP-EARNINGS(6)
            ELSE
               IF TP-TRCD NOT = "AP"
                  COMPUTE HOLD-EARNINGS = TP-APCUR + TP-APINT +
                                          TP-APLC - TP-DLPROC.

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

           IF ( TP-TRCD NOT = "JD" )
              MOVE TP-TRAMT TO D-TRAMT.

           ADD TP-APOTH TP-APINT TP-APLC GIVING D-APINT.
           MOVE TP-APCUR TO D-APCUR.

           MOVE TP-PDTH-DATE TO NUM-DATE.
           MOVE NUM-MO TO D-PDTHRU-MO.
           IF TP-UNITPER-CD-DAYS
              MOVE NUM-DA TO D-PDTHRU-DAYR
           ELSE
              MOVE NUM-YR TO D-PDTHRU-DAYR.

           MOVE TP-LCPDTH-DATE TO NUM-DATE.
           MOVE NUM-MO TO D-LCPD-MO.
           IF TP-UNITPER-CD-DAYS
              MOVE NUM-DA TO D-LCPD-DAYR
           ELSE
              MOVE NUM-YR TO D-LCPD-DAYR.

           MOVE TP-OTHBAL TO D-OTHBAL.
           ADD TP-INTBAL TP-LCBAL GIVING D-INTBAL.
           MOVE TP-CURBAL TO D-PRINBAL.

           IF SUM-ONLY = "N"
            IF TP-CURBAL = 0
              MOVE "**PAIDOFF**" TO D-PAIDOFF
              PERFORM WRITE-DETAIL-LINE.

           IF TP-GLAMT(1) NOT = 0
              MOVE TP-GLNO(1) TO D-GLNO
              INSPECT D-GLNO REPLACING ALL "/" BY "-".

           IF SUM-ONLY = "N"
              PERFORM WRITE-DETAIL-LINE.

           IF SUM-ONLY = "N"
            IF TP-GLAMT(1) NOT = 0
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

      *****************************************************************
       CREATE-WK SECTION.
      *****************************************************************
           PERFORM SETUP-WK-KEY.
           PERFORM READ-WK-FILE.

           IF IO-FG = 0
              GO TO CREATE-WK-UPDATE.

           PERFORM SETUP-WK-KEY.
           MOVE 0 TO WK-NO.
           MOVE 0 TO WK-TRAMT WK-APOTH WK-APINT WK-APLC
                     WK-APCUR WK-EARNINGS WK-DLPROC.

       CREATE-WK-UPDATE.
           IF TP-REV NOT = "Y"
              ADD 1 TO WK-NO
           ELSE
              SUBTRACT 1 FROM WK-NO.

           IF ( TP-OTHBR-FG = " " OR "G" ) AND
              ( TP-TRCD NOT = "JD"       )
                 ADD TP-TRAMT TO WK-TRAMT.

           IF TP-OTHBR-FG = " " OR "G"
              ADD TP-APOTH      TO WK-APOTH
              ADD TP-APINT      TO WK-APINT
              ADD TP-APLC       TO WK-APLC
              ADD TP-APCUR     TO WK-APCUR
              ADD HOLD-EARNINGS TO WK-EARNINGS
              ADD TP-DLPROC     TO WK-DLPROC.

           PERFORM WRITE-WK-FILE.

           IF IO-FG NOT = 0
              PERFORM REWRITE-WK-FILE.

           MOVE 0 TO SUB.
           PERFORM OUTPUT-GL 3 TIMES.
           PERFORM CREATE-TOTAL-WK.

      *****************************************************************
       CREATE-TOTAL-WK SECTION.
      *****************************************************************
           PERFORM SETUP-SUM-KEY.
           PERFORM READ-WK-FILE.

           IF IO-FG = 0
              GO TO CREATE-SUM-UPDATE.

           PERFORM SETUP-SUM-KEY.
           MOVE 0 TO WK-NO.
           MOVE 0 TO WK-TRAMT WK-APOTH WK-APINT WK-APLC
                     WK-APCUR WK-EARNINGS WK-DLPROC.

       CREATE-SUM-UPDATE.
           IF TP-REV NOT = "Y"
              ADD 1 TO WK-NO
           ELSE
              SUBTRACT 1 FROM WK-NO.

           IF ( TP-OTHBR-FG = " " OR "G" ) AND
              ( TP-TRCD NOT = "JD"       )
                 ADD TP-TRAMT TO WK-TRAMT.

           IF TP-OTHBR-FG = " " OR "G"
              ADD TP-APOTH TO WK-APOTH
              ADD TP-APINT TO WK-APINT
              ADD TP-APLC TO WK-APLC
              ADD TP-APCUR TO WK-APCUR
              ADD HOLD-EARNINGS TO WK-EARNINGS
              ADD TP-DLPROC TO WK-DLPROC.

           PERFORM WRITE-WK-FILE.

           IF IO-FG NOT = 0
              PERFORM REWRITE-WK-FILE.

           PERFORM ADD-TO-LEV-PL
                   VARYING LEV FROM 1 BY 1 UNTIL LEV > 3.

      ******************************************************************
      *
      *    ADD TO LEVEL PL SECTION.
      *
      * NOTE: IF TP-PLTYPE NOT = 2,3,4,5 THEN DEFAULT TO P
      ******************************************************************
       ADD-TO-LEV-PL SECTION.

           MOVE 0 TO LEV-PL.

           IF (TP-PLTYPE = "2")
               MOVE 2 TO LEV-PL
           ELSE
           IF (TP-PLTYPE = "3")
               MOVE 3 TO LEV-PL
           ELSE
           IF (TP-PLTYPE = "4")
               MOVE 4 TO LEV-PL
           ELSE
           IF (TP-PLTYPE = "5")
               MOVE 5 TO LEV-PL
           ELSE
               MOVE 1 TO LEV-PL.

           IF TP-REV NOT = "Y"
              ADD 1 TO T-NO(LEV LEV-PL)
                       T-NO(LEV 6)
              IF NOT (TP-TRCD = "PI" OR "OT")
                 ADD 1 TO T-R-NO(LEV LEV-PL)
                          T-R-NO(LEV 6)
              END-IF
           ELSE
              SUBTRACT 1 FROM T-NO(LEV LEV-PL)
                              T-NO(LEV 6)
              IF NOT (TP-TRCD = "PI" OR "OT")
                 SUBTRACT 1 FROM T-R-NO(LEV LEV-PL)
                                 T-R-NO(LEV 6).

           IF ( TP-OTHBR-FG = " " OR "G" ) AND
              ( TP-TRCD NOT = "JD"       )
              ADD TP-TRAMT TO T-TRAMT(LEV,LEV-PL)
                              T-TRAMT(LEV,6)
              IF NOT (TP-TRCD = "PI" OR "OT")
                 ADD TP-TRAMT TO T-R-TRAMT(LEV,LEV-PL)
                                 T-R-TRAMT(LEV,6).

           IF ( TP-OTHBR-FG = " " OR "G" )
              ADD TP-APOTH      TO T-APOTH   (LEV,LEV-PL)
                                   T-APOTH   (LEV,6)
              ADD TP-APINT      TO T-APINT   (LEV,LEV-PL)
                                   T-APINT   (LEV,6)
              ADD TP-APLC       TO T-APLC    (LEV,LEV-PL)
                                   T-APLC    (LEV,6)
              ADD TP-APCUR      TO T-APCUR  (LEV,LEV-PL)
                                   T-APCUR  (LEV,6)
              ADD HOLD-EARNINGS TO T-EARNINGS(LEV,LEV-PL)
                                   T-EARNINGS(LEV,6)
              ADD TP-DLPROC     TO T-DLPROC  (LEV,LEV-PL)
                                   T-DLPROC  (LEV,6)
              IF NOT (TP-TRCD = "PI" OR "OT")
                 ADD TP-APOTH      TO T-R-APOTH   (LEV,LEV-PL)
                                      T-R-APOTH   (LEV,6)
                 ADD TP-APINT      TO T-R-APINT   (LEV,LEV-PL)
                                      T-R-APINT   (LEV,6)
                 ADD TP-APLC       TO T-R-APLC    (LEV,LEV-PL)
                                      T-R-APLC    (LEV,6)
                 ADD TP-APCUR      TO T-R-APCUR  (LEV,LEV-PL)
                                      T-R-APCUR  (LEV,6)
                 ADD HOLD-EARNINGS TO T-R-EARNINGS(LEV,LEV-PL)
                                      T-R-EARNINGS(LEV,6)
                 ADD TP-DLPROC     TO T-R-DLPROC  (LEV,LEV-PL)
                                      T-R-DLPROC  (LEV,6).
       EXIT-ADD-TO-LEV-PL.
           EXIT.

      *****************************************************************
       OUTPUT-GL SECTION.
      *****************************************************************
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

      *****************************************************************
       FIND-BORROWER SECTION.
      *****************************************************************
           MOVE TP-SSNO TO BW-SSNO.
           PERFORM READ-BW1-FILE.
           IF IO-FG = 9
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

      *****************************************************************
       WRITE-DETAIL-LINE SECTION.
      *****************************************************************
           ADD LN-FEED LN-COUNT GIVING LN-WK.

           IF LN-WK GREATER THAN EXT-CONAME-RPT-LINES
              PERFORM HEAD-PAGE.

           MOVE DETAIL-LINE TO PR-REC.
           PERFORM WRITE-PR-REC.
           ADD LN-FEED TO LN-COUNT.
           MOVE 1 TO LN-FEED.
           MOVE SPACES TO DETAIL-LINE.


      *****************************************************************
       SET-CLASS-HEADINGS SECTION.
      *****************************************************************
           MOVE HOLD-CLASS TO H-LCNO LC-CODE.
           PERFORM READ-LC1-FILE.
           IF IO-FG NOT = 0
              MOVE "INVALID" TO LC-DESC.

           MOVE LC-DESC TO H-LCDESC.
           MOVE "N" TO SUMMARY-FG.

      *****************************************************************
      *
      *     C L A S S    T O T A L S
      *
      *****************************************************************
       CLASS-TOTALS SECTION.
           IF ( GTT-ISO = "O" )
              GO TO END-CLASS-TOTALS.

           MOVE "Y" TO SUMMARY-FG.
           MOVE EXT-CONAME-RPT-LINES TO LN-COUNT.
           PERFORM WRITE-DETAIL-LINE.
           MOVE CLASS-HEAD TO PR-REC.
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-PR-REC.
           MOVE SPACES TO WK-KEY.
           MOVE "L" TO WK-CODE.
           MOVE HOLD-BRNO TO WK-BRNO.
           MOVE HOLD-CLASS TO WK-CLASS.
           PERFORM START-WK-FILE.

       PRINT-NEXT-CODE.
           PERFORM READ-WK-FILE-NEXT.
           IF IO-FG = 9 OR WK-CODE NOT = "L" OR
              WK-CLASS NOT = HOLD-CLASS
              PERFORM TYPE-TOTALS
              GO TO END-CLASS-TOTALS.

           MOVE WK-TRCD      TO DS-TRCD.
           MOVE WK-NO        TO DS-NO.
           MOVE WK-TRAMT     TO DS-TRAMT.
           MOVE WK-APOTH     TO DS-APOTH.
           MOVE WK-APINT     TO DS-APINT.
           MOVE WK-APLC      TO DS-APLC.
           MOVE WK-APCUR    TO DS-APCUR.
           MOVE WK-EARNINGS  TO DS-EARNINGS.
           MOVE WK-DLPROC    TO DS-DLPROC.
           PERFORM WRITE-DETAIL-LINE.
           MOVE 1            TO LN-FEED.

           GO TO PRINT-NEXT-CODE.

       END-CLASS-TOTALS.

           MOVE 1 TO SUB LEV.
           IF ( GTT-ISO NOT = "O" )
               PERFORM PRINT-RECOVERIES.

           MOVE 1 TO SUB.
           PERFORM ZERO-TOTAL-TBL
                   VARYING LEV-PL FROM 1 BY 1 UNTIL LEV-PL > 6.

           MOVE "N" TO SUMMARY-FG.

           IF ( GTT-ISO NOT = "O" )
              MOVE ALL "-" TO DETAIL-LINE
              PERFORM WRITE-DETAIL-LINE.


      *****************************************************************
      *
      *     BRANCH TOTALS.
      *
      *****************************************************************
       BRANCH-TOTALS SECTION.
           IF ( GTT-ISO = "O" )
              GO TO END-BRANCH-TOTALS.

           MOVE "Y" TO SUMMARY-FG.
           MOVE 0 TO HOLD-CLASS.
           MOVE 2 TO LEV.
           MOVE "BRANCH TOTALS:" TO DETAIL-LINE.
           PERFORM PRINT-SUMMARY.

       END-BRANCH-TOTALS.

           MOVE 2 TO SUB LEV.
           IF ( GTT-ISO NOT = "O" )
               PERFORM PRINT-RECOVERIES
               MOVE ALL "-" TO DETAIL-LINE
               PERFORM WRITE-DETAIL-LINE.

           MOVE 3 TO LEV.
           MOVE 2 TO SUB.
           PERFORM ZERO-TOTAL-TBL
              VARYING LEV-PL FROM 1 BY 1
                 UNTIL LEV-PL > 6.

           MOVE "N" TO SUMMARY-FG.

      *****************************************************************
      *
      *     G R A N D    T O T A L S
      *
      *****************************************************************
       GRAND-TOTALS SECTION.
           IF ( GTT-ISO = "O" )
              GO TO EXIT-PRINT-GRAND-TOTALS.

           MOVE "Y" TO SUMMARY-FG.
           MOVE EXT-CONAME-RPT-LINES TO LN-COUNT.
           PERFORM WRITE-DETAIL-LINE.
           MOVE GRAND-HEAD TO PR-REC.
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-PR-REC.
           MOVE SPACES TO WK-KEY.
           MOVE "S" TO WK-CODE.
           PERFORM START-WK-FILE.

       PRINT-NEXT-SUM.
           PERFORM READ-WK-FILE-NEXT.
           IF IO-FG NOT = 0 OR WK-CODE NOT = "S"
              GO TO PRINT-GRAND-CD.

           MOVE WK-CLASS TO DS-CLASS.
           MOVE WK-TRCD TO DS-TRCD.
           MOVE WK-NO TO DS-NO.
           MOVE WK-TRAMT TO DS-TRAMT.
           MOVE WK-APOTH TO DS-APOTH.
           MOVE WK-APINT TO DS-APINT.
           MOVE WK-APLC TO DS-APLC.
           MOVE WK-APCUR TO DS-APCUR.
           MOVE WK-EARNINGS TO DS-EARNINGS.
           MOVE WK-DLPROC TO DS-DLPROC.
           PERFORM WRITE-DETAIL-LINE.
           GO TO PRINT-NEXT-SUM.

       PRINT-GRAND-CD.
           MOVE "Y" TO SUMMARY-FG.
           MOVE 3 TO LEV.
           MOVE "GRAND TOTALS :" TO DETAIL-LINE.
           PERFORM PRINT-SUMMARY.

           PERFORM PRINT-RECOVERIES.

           MOVE ALL "-" TO DETAIL-LINE.
           PERFORM WRITE-DETAIL-LINE.

      *--------------------------------------------------------------
       EXIT-PRINT-GRAND-TOTALS.
           EXIT.

      *****************************************************************
      *
      *     G L    D I S T R I B U T I O N
      *
      *****************************************************************
       GL-SUMMARY SECTION.
           MOVE GL-HEAD TO DETAIL-LINE.
           MOVE 4 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.
           MOVE SPACES TO WK-KEY.
           MOVE 0 TO WK-BRNO
                     WK-CLASS
                     WK-GLNO
                     WK-GLAMT.
           MOVE "G" TO WK-CODE.

           IF GRAND-SUMMARY NOT = "Y"
              MOVE HOLD-BRNO TO WK-BRNO
              MOVE HOLD-CLASS TO WK-CLASS.

           PERFORM START-WK-FILE.

      *----------------------------------------------------------------
       PRINT-NEXT-GL.
           PERFORM READ-WK-FILE-NEXT.
           IF IO-FG NOT = 0 OR WK-CODE NOT = "G"
              GO TO END-GL-SUMMARY.

           IF GRAND-SUMMARY NOT = "Y"
              IF WK-BRNO NOT = HOLD-BRNO
                 OR WK-CLASS NOT = HOLD-CLASS
                 GO TO PRINT-NEXT-GL.

           IF WK-GLAMT = 0
              GO TO PRINT-NEXT-GL.

           MOVE WK-GLNO TO DS-GLNO.
           INSPECT DS-GLNO REPLACING ALL "/" BY "-".
           MOVE WK-GLAMT TO DS-GLAMT.
           PERFORM WRITE-DETAIL-LINE.
           GO TO PRINT-NEXT-GL.

      *----------------------------------------------------------------
       END-GL-SUMMARY.

           IF LEV = 2
              MOVE ALL "-" TO DETAIL-LINE
              PERFORM WRITE-DETAIL-LINE.

      *****************************************************************
       HEAD-PAGE SECTION.
      *****************************************************************
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

           IF GRAND-SUMMARY = "N"
              MOVE HEAD3 TO PR-REC
              PERFORM WRITE-PR-REC
              MOVE HEAD4 TO PR-REC
              PERFORM WRITE-PR-REC
           ELSE
              MOVE 00 TO H-BRNO
              MOVE "ALL BRANCHES" TO H-BRDESC
              MOVE HEAD3 TO PR-REC
              PERFORM WRITE-PR-REC.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( GTT-HEADER-FG = "Y" )
              MOVE GTT-HEAD1 TO PR-REC
              MOVE 2 TO LN-FEED
              PERFORM WRITE-PR-REC
              MOVE GTT-HEAD2 TO PR-REC
              MOVE 2 TO LN-FEED
              PERFORM WRITE-PR-REC
              MOVE GTT-HEAD3 TO PR-REC
              MOVE 1 TO LN-FEED
              PERFORM WRITE-PR-REC
              MOVE ALL "-" TO PR-REC
              PERFORM WRITE-PR-REC
              MOVE 10 TO LN-COUNT
              MOVE 1 TO LN-FEED
           ELSE
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF SUMMARY-FG = "N"
              MOVE HEAD5 TO PR-REC
              MOVE 2 TO LN-FEED
              PERFORM WRITE-PR-REC
              MOVE HEAD6 TO PR-REC
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
           MOVE "BEG CLASS" TO RANGE-LINE.
           MOVE BEG-CLASS TO R-NUM.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "END CLASS" TO RANGE-LINE.
           MOVE END-CLASS TO R-NUM.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "BEGIN DATE" TO RANGE-LINE.
           MOVE BDATE TO R-NUM.
           PERFORM WRITE-DETAIL-LINE.
           MOVE "END   DATE" TO RANGE-LINE.
           MOVE EDATE TO R-NUM.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "GROUP TOTALS" TO RANGE-LINE.
           MOVE GTT-ISO        TO R-CHAR.
           PERFORM WRITE-DETAIL-LINE.

      ************************************************************
       CLASS-SCAN SECTION.
      ************************************************************
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

      ******************************************************************
       GTT-ADD-TOTALS SECTION.
      ******************************************************************

           ADD WK-NO          TO  WS-GTT-NO        ( GTT-TOTAL-SUB ).
           ADD WK-TRAMT       TO  WS-GTT-TRAMT     ( GTT-TOTAL-SUB ).
           ADD WK-APOTH       TO  WS-GTT-APOTH     ( GTT-TOTAL-SUB ).
           ADD WK-APINT       TO  WS-GTT-APINT     ( GTT-TOTAL-SUB ).
           ADD WK-APLC        TO  WS-GTT-APLC      ( GTT-TOTAL-SUB ).
           ADD WK-APCUR      TO  WS-GTT-APCUR    ( GTT-TOTAL-SUB ).
           ADD WK-EARNINGS    TO  WS-GTT-EARNINGS  ( GTT-TOTAL-SUB ).
           ADD WK-DLPROC      TO  WS-GTT-DLPROC    ( GTT-TOTAL-SUB ).

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
              ( WK-BRNO NOT = WGR-SRESULT )
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
           MOVE "BRANCH:" TO D2-PROMPT1 WS-HOLD-PROMPT.
           MOVE BR-NO     TO D2-BRNO.


           PERFORM GTT-PRINT-TOTALS.

       EXIT-GTT-PRINT-GROUP-BR-TOTALS.
           EXIT.

      ******************************************************************
       GTT-PRINT-GROUP-TOTALS SECTION.
      ******************************************************************

           MOVE SPACES    TO DETAIL-LINE.
           MOVE "GROUP :" TO D2-PROMPT1 WS-HOLD-PROMPT.
           MOVE GR-ID     TO D2-GRNO.
           MOVE GR-LEVEL  TO D2-GR-LEVEL.

           PERFORM GTT-PRINT-TOTALS.

       EXIT-GTT-PRINT-GROUP-TOTALS.
           EXIT.

      ******************************************************************
       GTT-PRINT-TOTALS SECTION.
      ******************************************************************
           MOVE WS-GTT-NO        (GTT-TOTAL-SUB) TO DS-NO.
           MOVE WS-GTT-TRAMT     (GTT-TOTAL-SUB) TO DS-TRAMT.
           MOVE WS-GTT-APOTH     (GTT-TOTAL-SUB) TO DS-APOTH.
           MOVE WS-GTT-APINT     (GTT-TOTAL-SUB) TO DS-APINT.
           MOVE WS-GTT-APLC      (GTT-TOTAL-SUB) TO DS-APLC.
           MOVE WS-GTT-APCUR    (GTT-TOTAL-SUB) TO DS-APCUR.
           MOVE WS-GTT-EARNINGS  (GTT-TOTAL-SUB) TO DS-EARNINGS.
           MOVE WS-GTT-DLPROC    (GTT-TOTAL-SUB) TO DS-DLPROC.

      *- - - - - - - - - - - - - - - - - - PRINT DETAIL AFTER LOAD - - -
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           IF ( WS-HOLD-PROMPT = "GROUP :" )
              MOVE ALL "-" TO DETAIL-LINE
              PERFORM WRITE-DETAIL-LINE.

      *- - - - - - - - - - - - - - - - - - - CLEAR LEVEL IN TABLE  - - -
           MOVE LOW-VALUES TO WS-GTT-TABLE-GROUP-LEVEL(GTT-TOTAL-SUB).

       EXIT-GTT-PRINT-TOTALS.
           EXIT.

      ******************************************************************
       GET-DISPLAY-LNNO SECTION.
      ******************************************************************

      * LPS-DISPLAY-LNNO IS SET FIRST SO THAT IF THERE IS A ERROR
      * IT WILL BE ALREADY DISPLAYING FOR A ERROR, OTHERWISE IT WILL
      * RESET LPS-DISPLAY-LNNO IN 'PERFORM LPSTAT-DISPLAY.'

           MOVE TP-CLASS  TO LPS-CLASS.
           MOVE "-"       TO LPS-DASH.
           MOVE TP-ACCTNO TO LPS-NUMBER.
           MOVE "{N/A}"   TO LPS-7STATUS.

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


      *********************************************
       FORM-RESET SECTION.
      *********************************************
           MOVE EXT-CONAME-CONAME TO GATXWO-CONAME.
           MOVE EXT-CONAME-SYSDATE TO GATXWO-SYSDATE.
           DISPLAY GATXWO-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE GATXWO-FORM-FIELDS TO WR-BUF.
           MOVE GATXWO-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.


      *********************************************
       SETUP-LINK-INFO SECTION.
      *********************************************
           MOVE GATXWO-HELP--FILE TO HELP-LINK-FILE.
           MOVE GATXWO-HELP--DIR  TO HELP-LINK-DIR.

      *********************************************
       VDU-CONVERT-FIELD SECTION.
      *********************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBLP/GATXWO_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      *****************************************************************
      *
      *     C O P Y - L I B R A R Y - S E C T I O N S
      *
      *****************************************************************
       COPY "LIBGB/TIMEIO.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/GRWORK.CPY".
       COPY "LIBGB/GRSERCH.CPY".
       COPY "LIBGB/GRTOTALS.CPY".
       COPY "LIBWI/GRTOTL.CPY".
       COPY "LIBGB/BRSECURE.CPY".
       COPY "LIBGB/EOSPOOL.CPY".
       COPY "LIBLP/LPMDTE.CPY".
       COPY "LIBLP/LPSTAT.CPY".

      *****************************************************************
       MISC-PARA SECTION.
      *****************************************************************
       COPY "LIBGB/GETPR.CPY".
       COPY "LIBGB/RMFILE.CPY".
       COPY "LIBGB/SYSTEM.CPY".
       COPY "LIBGB/MKTEMP.CPY".
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/DACCESS.CPY".
       SET-BRANCH-HEADINGS.
           MOVE HOLD-BRNO TO H-BRNO.
           MOVE BR-NAME TO H-BRDESC.
      *-----------------------------------------------------------------
       SEND-LEGEND.
           MOVE LEGEND TO WR-BUF.
           MOVE MSEL-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      *-----------------------------------------------------------------
       SETUP-SUM-KEY.
           MOVE SPACE TO WK-REC.
           MOVE "S" TO WK-CODE.
           MOVE 0 TO WK-BRNO.
           MOVE TP-CLASS TO WK-CLASS.
           MOVE TP-TRCD TO WK-TRCD.

      *-----------------------------------------------------------------
       SETUP-WK-KEY.
           MOVE SPACES TO WK-REC.
           MOVE "L" TO WK-CODE.
           MOVE TP-BRNO TO WK-BRNO.
           MOVE TP-CLASS TO WK-CLASS.
           MOVE TP-TRCD TO WK-TRCD.

      *-----------------------------------------------------------------
       SETUP-WK-GL1-KEY.
           MOVE SPACE TO WK-REC.
           MOVE "G" TO WK-CODE.
           MOVE TP-BRNO TO WK-BRNO.
           MOVE TP-CLASS TO WK-CLASS.
           MOVE TP-GLNO(SUB) TO WK-GLNO.

      *-----------------------------------------------------------------
       ZERO-TOTAL-TBL.
           MOVE ZEROS TO T-NO(SUB,LEV-PL).
           MOVE ZEROS TO T-TRAMT(SUB,LEV-PL).
           MOVE ZEROS TO T-APOTH(SUB,LEV-PL).
           MOVE ZEROS TO T-APINT(SUB,LEV-PL).
           MOVE ZEROS TO T-APLC(SUB,LEV-PL).
           MOVE ZEROS TO T-APCUR(SUB,LEV-PL).
           MOVE ZEROS TO T-EARNINGS(SUB,LEV-PL).
           MOVE ZEROS TO T-DLPROC(SUB,LEV-PL).

           MOVE ZEROS TO T-R-NO(SUB,LEV-PL).
           MOVE ZEROS TO T-R-TRAMT(SUB,LEV-PL).
           MOVE ZEROS TO T-R-APOTH(SUB,LEV-PL).
           MOVE ZEROS TO T-R-APINT(SUB,LEV-PL).
           MOVE ZEROS TO T-R-APLC(SUB,LEV-PL).
           MOVE ZEROS TO T-R-APCUR(SUB,LEV-PL).
           MOVE ZEROS TO T-R-EARNINGS(SUB,LEV-PL).
           MOVE ZEROS TO T-R-DLPROC(SUB,LEV-PL).

      *-----------------------------------------------------------------
       UPDATE-TABLE2.
           ADD T-NO(SUB,LEV-PL)       TO T-NO(LEV-NEXT-1,LEV-PL).
           ADD T-TRAMT(SUB,LEV-PL)    TO T-TRAMT(LEV-NEXT-1,LEV-PL).
           ADD T-APOTH(SUB,LEV-PL)    TO T-APOTH(LEV-NEXT-1,LEV-PL).
           ADD T-APINT(SUB,LEV-PL)    TO T-APINT(LEV-NEXT-1,LEV-PL).
           ADD T-APLC(SUB,LEV-PL)     TO T-APLC(LEV-NEXT-1,LEV-PL).
           ADD T-APCUR(SUB,LEV-PL)   TO T-APCUR(LEV-NEXT-1,LEV-PL).
           ADD T-EARNINGS(SUB,LEV-PL) TO T-EARNINGS(LEV-NEXT-1,LEV-PL).
           ADD T-DLPROC(SUB,LEV-PL)   TO T-DLPROC(LEV-NEXT-1,LEV-PL).

           ADD T-R-NO(SUB,LEV-PL)       TO T-R-NO(LEV-NEXT-1,LEV-PL).
           ADD T-R-TRAMT(SUB,LEV-PL)    TO T-R-TRAMT(LEV-NEXT-1,LEV-PL).
           ADD T-R-APOTH(SUB,LEV-PL)    TO T-R-APOTH(LEV-NEXT-1,LEV-PL).
           ADD T-R-APINT(SUB,LEV-PL)    TO T-R-APINT(LEV-NEXT-1,LEV-PL).
           ADD T-R-APLC(SUB,LEV-PL)     TO T-R-APLC(LEV-NEXT-1,LEV-PL).
           ADD T-R-APCUR(SUB,LEV-PL)    TO T-R-APCUR(LEV-NEXT-1,LEV-PL).
           ADD T-R-EARNINGS(SUB,LEV-PL)
                                     TO T-R-EARNINGS(LEV-NEXT-1,LEV-PL).
           ADD T-R-DLPROC(SUB,LEV-PL)  TO T-R-DLPROC(LEV-NEXT-1,LEV-PL).

      *-----------------------------------------------------------------
       TYPE-TOTALS.
           MOVE 1 TO LEV.
           MOVE "CLASS  TOTALS:" TO DETAIL-LINE.
           PERFORM PRINT-SUMMARY.

      *-----------------------------------------------------------------
       PRINT-RECOVERIES.
           MOVE "RECOVERIES:" TO DETAIL-LINE.
           PERFORM  WRITE-DETAIL-LINE.
           PERFORM PRINT-LEVEL-PL
                  VARYING LEV-PL FROM 1 BY 1 UNTIL LEV-PL > 6.
      *-----------------------------------------------------------------
       PRINT-LEVEL-PL.
           IF PL-TYPE(LEV-PL) = "P" MOVE "PL" TO DS-TRCD
           ELSE
           IF PL-TYPE(LEV-PL) = "2" MOVE "P2" TO DS-TRCD
           ELSE
           IF PL-TYPE(LEV-PL) = "3" MOVE "P3" TO DS-TRCD
           ELSE
           IF PL-TYPE(LEV-PL) = "4" MOVE "P4" TO DS-TRCD
           ELSE
           IF PL-TYPE(LEV-PL) = "5" MOVE "P5" TO DS-TRCD
           ELSE
           IF PL-TYPE(LEV-PL) = "6" MOVE "TOTAL" TO DETAIL-LINE.

           MOVE T-R-NO(LEV,LEV-PL)       TO DS-NO.
           MOVE T-R-TRAMT(LEV,LEV-PL)    TO DS-TRAMT.
           MOVE T-R-APOTH(LEV,LEV-PL)    TO DS-APOTH.
           MOVE T-R-APINT(LEV,LEV-PL)    TO DS-APINT.
           MOVE T-R-APLC(LEV,LEV-PL)     TO DS-APLC.
           MOVE T-R-APCUR(LEV,LEV-PL)    TO DS-APCUR.
           MOVE T-R-EARNINGS(LEV,LEV-PL) TO DS-EARNINGS.
           MOVE T-R-DLPROC(LEV,LEV-PL)   TO DS-DLPROC.
           PERFORM WRITE-DETAIL-LINE.

      *-----------------------------------------------------------------
       PRINT-SUMMARY.
           MOVE T-NO(LEV,6) TO DS-NO.
           MOVE T-TRAMT(LEV,6) TO DS-TRAMT.
           MOVE T-APOTH(LEV,6) TO DS-APOTH.
           MOVE T-APINT(LEV,6) TO DS-APINT.
           MOVE T-APLC(LEV,6) TO DS-APLC.
           MOVE T-APCUR(LEV,6) TO DS-APCUR.
           MOVE T-EARNINGS(LEV,6) TO DS-EARNINGS.
           MOVE T-DLPROC(LEV,6) TO DS-DLPROC.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.
           MOVE ALL " " TO DETAIL-LINE.
           PERFORM WRITE-DETAIL-LINE.

      *****************************************************************
       IO-ROUTINES SECTION.
      *****************************************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBLP/LPTRPGS_SQL.CPY".
       COPY "LIBLP/LPSPGS_SQL.CPY".
       COPY "LIBLP/LPLNGS_SQL.CPY".
       COPY "LIBLP/LPLCGS_SQL.CPY".
       COPY "LIBLP/LPBWGS_SQL.CPY".
       COPY "LIBGB/GBGRGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBPRINT.CPY".
       COPY "LIBLP/LPBW1RN.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBGB/GBGR1RN.CPY".
       COPY "LIBLP/LPTRP1RN.CPY".
       COPY "LIBLP/LPLC1RN.CPY".
       COPY "LIBLP/LPLN1RN.CPY".
       COPY "LIBLP/LPSP1RN.CPY".
       COPY "LIBGB/GBWKIN.CPY".
       COPY "LIBGB/GBWK2IN.CPY".
      *----------------------------------------------------------------
       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO EXT-EOBUF-ERRCD.

      *****************************************************************
       END-PROGRAM SECTION.
      *****************************************************************
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

      *----------------------------------------------------------------
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
              MOVE EXIT-PATH TO FORM-NAM
              MOVE FORM-PATH TO FORM-PATHNAME.

      *----------------------------------------------------------------
       STOP-RUN.
           IF EXT-ROUTE-BUF = "00" OR "40" OR "50"
              DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN
              DESTROY GATXWO-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.
           EXIT PROGRAM.
