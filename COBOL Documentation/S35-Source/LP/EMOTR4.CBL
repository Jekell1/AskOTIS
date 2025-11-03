       IDENTIFICATION DIVISION.
       PROGRAM-ID.   EMOTR4.
       DATE-WRITTEN. 08/09/2022.
      ******************************************************************
      *                      (LP)
      *    DESCRIPTION:  MONTHLY OTHER CHARGES REGISTER
      *                   BY ACTIONCD2 (SECURITIZATION)
      * REV:
      *  20220809 BAH #1565 SECURITIZATION GROUP #3
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
               05  FILLER   PIC X.
               05  WK-GLNO  PIC 9(11)     COMP-3.
               05  FILLER   PIC XXX.
           03  WK-GLAMT     PIC S9(7)V99  COMP-3.
           03  FILLER       PIC X(41).

      ********************************************
      *  SORTED TRFILE BY BRANCH, CLASS, ACCOUNT
      ********************************************
       FD  WK2-FILE
           LABEL RECORDS ARE STANDARD.
       01  WK2-REC.
           03  WK2-KEY.
               05  WK2-BRANCH            PIC 9(4).
               05  WK2-CLASS             PIC 9(3).
               05  WK2-ACCTNO            PIC 9(8).
               05  WK2-DATE              PIC 9(8).
               05  WK2-LINE              PIC 9(3).

       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)LP/EMOTR4.CBL S35 08/09/22-14:51:45 >".
      ****************************
      *     F I L E   I F N      *
      ****************************
       COPY "LIBLP/LP01TRP.CPY".
       COPY "LIBLP/LP01TRP_SQL.CPY".
       COPY "LIBLP/LPWSTRP.CPY".
       COPY "LIBLP/LP01BW.CPY".
       COPY "LIBLP/LP01BW_SQL.CPY".
       COPY "LIBLP/LPWSBW.CPY".
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
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
       COPY "LIBGB/GB01GR.CPY".
       COPY "LIBGB/GB01GR_SQL.CPY".
       COPY "LIBGB/GBWSGR.CPY".
       01  WK2-IFN              PIC X(3)     VALUE "WK2".
       01  WK-IFN               PIC X(2)     VALUE "WK".
       01  WK2-PATH             PIC X(35).


      **********************
      *   LINE WORKERS     *
      **********************
       01  LN-WORKERS.
           03  LN-FEED          PIC 999      VALUE 0.
           03  LN-COUNT         PIC 999      VALUE 99.
           03  LN-WK            PIC 999      VALUE 0.
       01  PAGE-NUMBER          PIC 9(4)     VALUE 0.

       01  ER-FEED              PIC 9        VALUE 1.

      **********************
      *   MISC WORKERS     *
      **********************
       01  HOLD-EARNINGS        PIC S9(7)V99  COMP-3 VALUE 0.
       01  HOLD-TP-OTHBR-FG     PIC X                VALUE "?".
       01  HOLD-TP-ACCTNO       PIC 9(8)      COMP-3 VALUE 99999999.
       01  HOLD-TP-SSNO         PIC 9(10)     COMP-3 VALUE 9999999999.
       01  SUB                  PIC 9(6)      COMP-3.
       01  LEV                  PIC 99        COMP-3.
       01  HOLD-BRNO            PIC 9(4)      VALUE 0.
       01  HOLD-CLASS           PIC 9(3)      VALUE 0.
       01  RECORDS-FOUND        PIC X     VALUE " ".
           88  NO-RECORDS-FOUND           VALUE "N".
       01  SUMMARY-FG           PIC X     VALUE "N".
       01  GRAND-SUMMARY        PIC X     VALUE "N".
       01  NAME-WORKER          PIC X(34).
       01  FIRST-NAME           PIC X(16).

       01  FIRST-TIME           PIC X        VALUE "Y".
       01  MSEL-LIST            PIC X(5)     VALUE "MSEL.".
       01  MESS-LIST            PIC X(5)     VALUE "MESS.".

      *-----------------------------------------------------------------
       01  MESSAGE-01-LOC-PRT   PIC X(43)    VALUE
           "CANNOT DO LOCAL SPOOLING WITH GROUP TOTALS".

       78  GTT-TYPE-MAX                                 VALUE 2.
       77  GTT-TYPE-SUB                       PIC 9(2)  VALUE 0.
           88  GTT-TYPE-SUB-OT                          VALUE 1.
           88  GTT-TYPE-SUB-AP                          VALUE 2.
           88  GTT-TYPE-SUB-VALID          VALUE 1 THRU GTT-TYPE-MAX.
      ******************************************************************
      *
      *    W S - G T T - T A B L E
      *
      ******************************************************************
       01  WS-GTT-TOTAL-TABLE.
           03  WS-GTT-TABLE-GROUP-LEVEL                 OCCURS 10 TIMES.
      *-------------------------------------------------------------
      *  WS-GTT-TYPE-TABLE
      *      OT = 1
      *      AP = 2
      *-------------------------------------------------------------
               05  WS-GTT-TYPE-TABLE               OCCURS GTT-TYPE-MAX.
                   07  WS-GTT-NO              PIC S9(7)     COMP.
                   07  WS-GTT-TRAMT           PIC S9(9)V99  COMP.
                   07  WS-GTT-APOTH           PIC S9(9)V99  COMP.
                   07  WS-GTT-APINT           PIC S9(9)V99  COMP.
                   07  WS-GTT-APLC            PIC S9(9)V99  COMP.
                   07  WS-GTT-APCUR          PIC S9(9)V99  COMP.
                   07  WS-GTT-EARNINGS        PIC S9(9)V99  COMP.
                   07  WS-GTT-DLPROC          PIC S9(9)V99  COMP.

      *-----------------------------------------------------------------
       01  WS-HOLD-PROMPT       PIC X(7)  VALUE SPACES.


      *  TOTALS LEV1 = CLASS TOTALS
      *            2 = BRANCH TOTALS
      *            3 = GRAND TOTALS
       01  TOTALS-TAB   VALUE ZEROS.
           03  TOTAL-TBL       OCCURS 3.
               05  T-TRAMT      PIC S9(9)V99 COMP-3.
               05  T-APOTH      PIC S9(9)V99 COMP-3.
               05  T-APINT      PIC S9(9)V99 COMP-3.
               05  T-APLC       PIC S9(9)V99 COMP-3.
               05  T-APCUR     PIC S9(9)V99 COMP-3.
               05  T-EARNINGS   PIC S9(9)V99 COMP-3.
               05  T-DLPROC     PIC S9(7)V99 COMP-3.

       01  CLASS-FG             PIC X(3)       VALUE "   ".
           88  CLASS-BEG                       VALUE "BEG".
           88  CLASS-END                       VALUE "END".

       01  LEGEND               PIC X(70).

      ***********************************
      *     S C R E E N   F I E L D S   *
      ***********************************
       COPY "LIBGB/EOBUF_EXT.CPY".
           03  BEG-BRANCH      PIC 9(4).
           03  END-BRANCH      PIC 9(4).
           03  BEG-CLASS       PIC 9(3).
           03  END-CLASS       PIC 9(3).
           03  END-DATE.
               05  END-MONTH   PIC 99.
               05  END-YEAR    PIC 99.
           03  EDATE REDEFINES END-DATE PIC 9999.
           03  DAY-OF-MONTH    PIC 99.

           03  GTT-ISO         PIC X.
           03  SUM-ONLY        PIC X.
           03  GROUP-FLAG      PIC X.
           03  ENT-GROUP       PIC X(4).
           03  LOCAL-PRINT-FG  PIC X(1).
           03  TOTAL-LEV-TABLE.
               05  TOTAL-LEVEL-TABLE PIC X(1)   OCCURS 10.
           03  REVERSALS-ONLY  PIC X.
           03  ACTION-CODE2    PIC X(4).

       01  VERSION-CONTROL     PIC X VALUE "D".
       01  BEGINNING-DATE       PIC 9(8).
       01  ENDING-DATE          PIC 9(8).

       01  SV-BEG-BR           PIC 9(4)       VALUE 9999.
       01  SV-END-BR           PIC 9(4)       VALUE 9999.
       01  SV-GROUP-FLAG       PIC X     VALUE SPACES.
       01  SV-ENT-GROUP        PIC X(4)  VALUE SPACES.
       01  SV-LOCAL-PRINT-FG   PIC X     VALUE SPACES.

      ***********************************
      *     P R I N T   B U F F E R S   *
      ***********************************

       01  HEAD1.
           03  H-DATE           PIC X(8)     VALUE " ".
           03  FILLER           PIC XX       VALUE " ".
           03  H-TIME           PIC X(8)     VALUE " ".
           03  FILLER           PIC X(28)    VALUE " ".
           03  H-NAME           PIC X(40)    VALUE " ".
           03  FILLER           PIC X(37)    VALUE " ".
           03  FILLER           PIC X(5)     VALUE "PAGE ".
           03  H-PAGE-NO        PIC ZZZ9.

       01  HEAD2.
           03  FILLER           PIC X(10)    VALUE "USER ID:".
           03  H-USERID         PIC X(10).
           03  FILLER           PIC X(20)    VALUE " ".
           03  FILLER           PIC X(50) VALUE
               "MONTHLY OTHER CHARGES REGISTER (SECURITIZATION)".

       01  HEAD3.
           03  FILLER           PIC X(10) VALUE "BRANCH :".
           03  H-BRNO           PIC 9(4).
           03  FILLER           PIC X VALUE " ".
           03  H-BRDESC         PIC X(20).
           03  FILLER           PIC X(46) VALUE " ".

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
           "       PROC'D   EARNED PC PAY-DATE CD REFNO   AMOUNT".
           03  FILLER           PIC X(45)     VALUE
           "OT/INT/LC   PRIN  PDTH LCPD     OTH      INT".
           03  FILLER           PIC X(18)     VALUE
           "PRIN      ACCT-AMT".

       01  CLASS-HEAD.
           03  FILLER           PIC X(55)    VALUE
           "          TRANS CODE   NUMBER     AMOUNT    APP-OTH    ".
           03  FILLER           PIC X(41)    VALUE
           "APP-INT    APP-LC      APP-PRIN  EARNINGS".
           03  FILLER           PIC X(11)    VALUE
           "    DL-PROC".
       01  GRAND-HEAD.
           03  FILLER           PIC X(55)    VALUE
           "CLASS     TRANS CODE   NUMBER     AMOUNT    APP-OTH    ".
           03  FILLER           PIC X(41)    VALUE
           "APP-INT    APP-LC      APP-PRIN  EARNINGS".
           03  FILLER           PIC X(11)    VALUE
           "    DL-PROC".

       01  GL-HEAD.
           03  FILLER           PIC X(17)    VALUE "G/L DIST  SUMMARY".

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
           03  FILLER            PIC X(11)  VALUE "APPLIED    ".
           03  FILLER            PIC X(11)  VALUE "APPLIED    ".
           03  FILLER            PIC X(12)  VALUE "APPLIED     ".
           03  FILLER            PIC X(10)  VALUE "APPLIED   ".
           03  FILLER            PIC X(12)  VALUE "EARNINGS    ".
           03  FILLER            PIC X(11)  VALUE "DEALER     ".

      *-----------------------------------------------------------------
       01  GTT-HEAD3.
           03  FILLER            PIC X(14)  VALUE SPACES.
           03  FILLER            PIC X(10)  VALUE "LEV       ".
           03  FILLER            PIC X(9)   VALUE "         ".
           03  FILLER            PIC X(11)  VALUE "AMOUNT     ".
           03  FILLER            PIC X(11)  VALUE "OTHER      ".
           03  FILLER            PIC X(11)  VALUE "INTEREST   ".
           03  FILLER            PIC X(12)  VALUE "LT-CHG      ".
           03  FILLER            PIC X(17)  VALUE "PRINCIPAL        ".
           03  FILLER            PIC X(15)  VALUE "     PROCEEDS  ".

       01  DETAIL-LINE          VALUE SPACES.
           03  FILLER           PIC X(116).
           03  D-BRXX           PIC X(12).
           03  D-BRX            PIC 9(4).

       01  D-LINE1 REDEFINES DETAIL-LINE.
           03  D-DISPLAY-LNNO   PIC X(16).
           03  FILLER           PIC X(03).
           03  D-NAME           PIC X(26).
           03  FILLER           PIC X.
           03  D-MAKER          PIC X.
           03  FILLER           PIC XX.
           03  D-SSNO           PIC 999/99/9999 BLANK ZERO.
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
           03  DS-TRCD          PIC XX.
           03  FILLER           PIC X(5).
           03  DS-NO            PIC ZZZ.
           03  FILLER           PIC X.
           03  DS-TRAMT         PIC ZZZZZZZ.ZZ-.
           03  FILLER           PIC X.
           03  DS-APOTH         PIC ZZZZZZ.ZZ-.
           03  FILLER           PIC X.
           03  DS-APINT         PIC ZZZZZZ.ZZ-.
           03  FILLER           PIC X.
           03  DS-APLC          PIC ZZZZZZ.ZZ-.
           03  FILLER           PIC X.
           03  DS-APCUR        PIC ZZZZZZZ.ZZ-.
           03  FILLER           PIC X.
           03  DS-EARNINGS      PIC ZZZZZZ.ZZ-.
           03  FILLER           PIC X.
           03  DS-DLPROC        PIC ZZZZZZ.ZZ-.

       01  D-LINE5 REDEFINES DETAIL-LINE.
           03  DS-GLNO          PIC ZZZ9/9999999.
           03  FILLER           PIC XX.
           03  DS-GLAMT         PIC ZZZZ9.99-.

      *-----------------------------------------------------------------
       01  RANGE-LINE REDEFINES DETAIL-LINE.
           03  FILLER           PIC X(35).
           03  RANGE-SELECTION.
               04  R-NUM        PIC Z(5)9.
               04  R-NUM-REDEF REDEFINES R-NUM.
                   05  R-CHAR   PIC X(6).
      *----------------------------------------------------------------

       COPY "LIBGB/PASSWDW.CPY".
       COPY "LIBWI/LCSCANW.CPY".
       COPY "LIBWI/CDSCANW.CPY".
       COPY "LIBLP/EMSFRG_CMD.CPY".
       COPY "LIBWI/GRSCANW.CPY".
       COPY "LIBGB/GRWORKW.CPY".
       COPY "LIBWI/GRTOTLW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBGB/MKTEMPW_CMD.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBLP/LPMDTEW.CPY".
       COPY "LIBLP/LPSTATW.CPY".
       COPY "LIBGB/BRSECUREW.CPY".
       COPY "LIBGB/SYSTEMW.CPY".
       COPY "LIBGB/EOSPOOLW.CPY".
       COPY "LIBGB/GETPRW.CPY".
       COPY "LIBGB/DACCESSW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBLP/EMOTR4_DEF.CPY".
       COPY "LIBLP/EMOTR4_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

      ******************************************************************
      *
      *     P R O G R A M   C O N T R O L
      *
      ******************************************************************
       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBLP/EMOTR4_SCN.CPY".

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

      ***************************************************
       MAIN-MODULE SECTION.
      ***************************************************

           PERFORM SQL-CONNECT.

           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "EMOTR4" TO FORM-PROG VDU-FORM-NAME.

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

           PERFORM OPEN-TRP1-FILE.
           PERFORM OPEN-BW1-FILE.
           PERFORM OPEN-LN1-FILE.

           PERFORM SORT-ROUTINE.

           IF EXT-EOBUF-ERRCD = "X"
              GO TO NEXT-GROUP-LOC.

           PERFORM OPEN-WK2-FILE.

      *----------------------------------------------------------------
       NEXT-TR-REPORT.
           PERFORM READ-WK2-FILE-NEXT.
           IF IO-FG = 9
              GO TO END-OF-GROUP.

           MOVE WK2-BRANCH  TO TP-BRNO.
           MOVE WK2-DATE    TO TP-DATE.
           MOVE WK2-CLASS   TO TP-CLASS.
           MOVE WK2-ACCTNO  TO TP-ACCTNO.
           MOVE WK2-LINE    TO TP-LINE.

           PERFORM READ-TRP1-FILE.
           IF IO-FG = 9
              MOVE "TRANSACTION RECORD MISSING SINCE SORT" TO MESS
              PERFORM SEND-MESS
              GO TO IO-ERROR.

           IF FIRST-TIME = "Y"
              MOVE TP-BRNO TO HOLD-BRNO
              MOVE TP-CLASS TO HOLD-CLASS
              PERFORM SET-CLASS-HEADINGS
              PERFORM SET-BRANCH-HEADINGS.
           IF TP-BRNO NOT = HOLD-BRNO
              PERFORM CLASS-TOTALS
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
                 PERFORM BRANCH-TOTALS
                 PERFORM GRAND-TOTALS
                 MOVE "Y" TO FIRST-TIME
                 MOVE 0 TO PAGE-NUMBER
              ELSE
                 MOVE 0 TO PAGE-NUMBER.

           GO TO NEXT-GROUP-LOC.

       END-ROUTINE.
           IF NO-RECORDS-FOUND
              GO TO END-PROGRAM.

           IF LOCAL-PRINT-FG = "N"
              PERFORM CLASS-TOTALS
              PERFORM BRANCH-TOTALS
              MOVE "Y" TO GRAND-SUMMARY
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

      ***********************************
       INITIALIZATION SECTION.
      ***********************************
           MOVE EMOTR4-QUEUE-POS TO Q-POS.
           MOVE EMOTR4-COPIES-POS TO C-POS.
           MOVE EMOTR4-PRU-POS   TO P-POS.
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
           MOVE END-MONTH TO DATE-MMDDYY-MM.
           MOVE END-YEAR  TO DATE-MMDDYY-YY.
           IF DAY-OF-MONTH = 99
              MOVE 1      TO DATE-MMDDYY-DD
           ELSE
              MOVE DAY-OF-MONTH TO DATE-MMDDYY-DD.
           PERFORM CONVERT-MMDDYY-TO-YYYYMMDD.
           MOVE DATE-YYYYMMDD TO BEGINNING-DATE.

           MOVE END-MONTH TO DATE-MMDDYY-MM.
           MOVE END-YEAR  TO DATE-MMDDYY-YY.
           IF DAY-OF-MONTH = 99
              MOVE 31      TO DATE-MMDDYY-DD
           ELSE
              MOVE DAY-OF-MONTH TO DATE-MMDDYY-DD.
           PERFORM CONVERT-MMDDYY-TO-YYYYMMDD.
           MOVE DATE-YYYYMMDD TO NDTE-DATE.
           PERFORM NEW-DATE-CALCULATION.
           MOVE NDTE-DATE TO ENDING-DATE.

           IF LOCAL-PRINT-FG NOT = "Y"
              PERFORM OPEN-PR-FILE.

           PERFORM OPEN-BR-FILE.
           PERFORM OPEN-GR1-FILE.
           PERFORM OPEN-SP1-FILE.
           PERFORM OPEN-LC1-FILE.

           MOVE 1 TO SUB.
           PERFORM ZERO-TOTAL-TBL.
           MOVE 2 TO SUB.
           PERFORM ZERO-TOTAL-TBL.
           MOVE 3 TO SUB.
           PERFORM ZERO-TOTAL-TBL.
           MOVE 0 TO PAGE-NUMBER.

           PERFORM OPEN-WK2-FILE.
           MOVE MKTEMP-DEFAULT TO MKTEMP-BUF.
           PERFORM MKTEMP-CALL.
           PERFORM OPEN-WK-FILE-OUTPUT.
           PERFORM CLOSE-WK-FILE.
           PERFORM OPEN-WK-FILE.

       SET-HEAD.
           MOVE EXT-CONAME-CONAME TO H-NAME.
           MOVE EXT-CONAME-SYSDATE TO H-DATE.
           MOVE EXT-CONAME-USER-ID TO H-USERID.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( GROUP-FLAG = "Y" )
              MOVE  LOW-VALUES TO WS-GTT-TOTAL-TABLE
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

      **************************************************
       EO-EXEC-AUDIT SECTION.
      **************************************************
           MOVE EXT-EOBUF-DATE2 TO SYS-DATE.
           MOVE S-MM TO END-MONTH.
           MOVE S-YY TO END-YEAR.

           IF EXT-EOBUF-DATE1 NOT = EXT-EOBUF-DATE2
              MOVE 99 TO DAY-OF-MONTH
           ELSE
      * IF USING 12/29/93 TO 12/29/93 FOR A DAILY BATCH, MUST SET
      * DAY-OF-MONTH TO S-DD
              MOVE S-DD TO DAY-OF-MONTH.

      **********************************************************
       ENTER-PARAMETERS SECTION.
      **********************************************************
       ENTER-PARM.

           IF VDUSTATUS = "1U"
              IF ( GTT-ISO = "O" )
                 GO TO GTT-ISO-PARAGRAPH
              ELSE
                 GO TO SUMYN.

      * DEFAULT TO USER BRANCH
      *    MOVE "SNNN040BR1." TO VDU.
      *    MOVE 0 TO BEG-BRANCH VDUNUM0.
      *    PERFORM SCREENIO.
      *    MOVE "SNNN040BR2." TO VDU.
      *    MOVE 0 TO END-BRANCH VDUNUM0.
      *--->PERFORM SCREENIO.

      *----------------------------------------------------------------
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


      *----------------------------------------------------------------
       BRANCH-IS-NUMERIC.
           MOVE "N" TO GROUP-FLAG.
           MOVE WGR-SRESULT TO BEG-BRANCH.
           MOVE "SN N040BR1." TO VDU.
           MOVE BEG-BRANCH TO VDUNUM0.
           PERFORM SCREENIO.
      *--------------------------------------------------------------
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
      *----------------------------------------------------
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
      *-----------------------------------------------------
       CHECK-BRANCH-SECURITY.
           PERFORM BR-RANGE-SECURITY.
           IF BR-SECURITY-OKAY = "N"
              GO TO BR-01.
      *------------------------------------------------------
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
      *--------------------------------------------------------
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
           GO TO DTE.
      *---------------------------------------------------
       ALL-CL.
           MOVE "SNNN020CL1." TO VDU.
           MOVE 0 TO BEG-CLASS VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN020CL2." TO VDU.
           MOVE 99 TO END-CLASS VDUNUM0.
           PERFORM SCREENIO.
      *----------------------------------------------------
       DTE.
           IF EXT-ROUTE-BUF NOT = "00"
              GO TO DA-01.
           MOVE LEGEND-DTE TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "ENNN040ENDDATE." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO CL-02.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO DTE.
           MOVE VDUNUM0 TO EDATE.
           IF END-MONTH < 1 OR > 12
              MOVE "INVALID MONTH" TO MESS
              PERFORM SEND-MESS
              GO TO DTE.

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

           IF VDUSTATUS = "1U"
              IF EXT-ROUTE-BUF = "00"
                 GO TO DTE
              ELSE
                 GO TO CL-02.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO DTE.
           IF VDUNUM0 = 0 GO TO DA-01.
           MOVE VDUNUM0 TO DAY-OF-MONTH.
           IF DAY-OF-MONTH > 31  OR
              (DAY-OF-MONTH > 30 AND
              (END-MONTH = 2 OR 4 OR 6 OR 9 OR 11)) OR
              (DAY-OF-MONTH > 29 AND END-MONTH = 2)
              MOVE "DAY IS PAST LAST DAY OF MONTH" TO MESS
              PERFORM SEND-MESS
              GO TO DA-01.

      *----------------------------------------------------------------
       ENTER-REVERSALS-ONLY.
           MOVE LEGEND-REVERSAL-ONLY TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "E  A010REV-ONLY." TO VDU.
           PERFORM SCREENIO.

           IF UP-KEY GO TO  DA-01.
           IF F7-KEY GO TO EXIT-PARM.
           IF F1-KEY GO TO EXIT-PARM.

           IF NOT (DOWN-KEY OR ENTER-KEY) GO TO ENTER-REVERSALS-ONLY.

           IF NOT (VDUBUF = "Y" OR "N")
              GO TO ENTER-REVERSALS-ONLY.

           MOVE VDUBUF TO REVERSALS-ONLY.

       ENTER-ACTIONCD2.

           MOVE "E  A040ACTION." TO VDU.
           PERFORM SCREENIO.
           IF ( UP-KEY ) GO TO ENTER-REVERSALS-ONLY.
           IF ( F1-KEY ) GO TO EXIT-PARM.
           IF ( F6-KEY )
              MOVE "A2" TO CDSCAN-TYPE
              PERFORM CODE-SCAN
              IF IO-FG NOT = 0
                 GO TO ENTER-ACTIONCD2.
           IF (                    F7-KEY ) GO TO EXIT-PARM.
           IF NOT ( DOWN-KEY OR ENTER-KEY ) GO TO ENTER-ACTIONCD2.

           MOVE VDUBUF TO ACTION-CODE2.

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

           IF ( VDUSTATUS = "10" ) GO TO EXIT-PARM.
           IF ( VDUSTATUS = "1>" ) GO TO EXIT-PARM.
           IF ( VDUSTATUS = "1U" ) GO TO ENTER-ACTIONCD2.
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
              GO TO LOCAL-PRINT.

           MOVE "E  A010SUMONLY." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U"
              IF ( GROUP-FLAG = "Y" )
                 GO TO GTT-ISO-PARAGRAPH.

           IF VDUSTATUS = "1U"
               GO TO ENTER-ACTIONCD2.

           IF VDUSTATUS NOT = "00" AND NOT = "1D"
              GO TO SUMYN.
           MOVE VDUBUF TO SUM-ONLY.
           IF SUM-ONLY NOT = "Y" AND NOT = "N"
              GO TO SUMYN.

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
           MOVE "SPOOL TO LOCAL DIRECTORY? (Y/N)" TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "S  A170SPOOL-PROMPT." TO VDU.
           MOVE "SPOOL TO LOCAL  :"    TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "S  A050YES-NO." TO VDU.
           MOVE "(Y/N)"          TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "E  A010LOCAL-SPOOL." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U"
              IF ( GROUP-FLAG = "Y" )
                 IF ( GTT-ISO = "O" )
                    GO TO GTT-ISO-PARAGRAPH
                 ELSE
                    GO TO SUMYN
              ELSE
                  GO TO SUMYN.
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

      **********************************************************
       DISPLAY-PARAMETERS SECTION.
      **********************************************************
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
           MOVE "SNNN020DA1." TO VDU.
           MOVE DAY-OF-MONTH TO VDUNUM0.
           PERFORM SCREENIO.
           MOVE "S  A010REV-ONLY." TO VDU.
           MOVE REVERSALS-ONLY TO VDUBUF.
           PERFORM SCREENIO.
           MOVE "S  A040ACTION." TO VDU.
           MOVE ACTION-CODE2 TO VDUBUF.
           PERFORM SCREENIO.
           MOVE "S  A010GTT-ISO." TO VDU.
           MOVE GTT-ISO TO VDUBUF.
           PERFORM SCREENIO.
           MOVE "S  A010SUMONLY." TO VDU.
           MOVE SUM-ONLY TO VDUBUF.
           PERFORM SCREENIO.
      *--------------------------------------------------------
           MOVE "S  A170SPOOL-PROMPT." TO VDU.
           MOVE "SPOOL TO LOCAL  :"    TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "S  A050YES-NO." TO VDU.
           MOVE "(Y/N)"          TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "S  A010LOCAL-SPOOL." TO VDU.
           MOVE LOCAL-PRINT-FG        TO VDUBUF.
           PERFORM SCREENIO.

      ***************************************
       SORT-ROUTINE SECTION.
      ***************************************

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
              GO TO EXIT-SORT-ROUTINE.

           IF TP-DATE > ENDING-DATE
              GO TO EXIT-SORT-ROUTINE.

           IF TP-TRCD NOT = "OT" AND NOT = "AP" AND NOT = "O2"
                    AND NOT = "OP"
              GO TO SORT-TR-NEXT.

           IF TP-CLASS < BEG-CLASS OR TP-CLASS > END-CLASS
              GO TO SORT-TR-NEXT.

           IF REVERSALS-ONLY = "Y"
              IF TP-REV NOT = "Y"
                 GO TO SORT-TR-NEXT.

           IF TP-ACTIONCD2 NOT = ACTION-CODE2
              GO TO SORT-TR-NEXT.

           MOVE TP-DATE    TO WK2-DATE.
           MOVE TP-BRNO   TO WK2-BRANCH.
           MOVE TP-CLASS  TO WK2-CLASS.
           MOVE TP-ACCTNO TO WK2-ACCTNO.
           MOVE TP-LINE   TO WK2-LINE.

           PERFORM WRITE-WK2-FILE.
           IF IO-FG NOT = 0
              GO TO IO-ERROR.

           GO TO SORT-TR-NEXT.
       EXIT-SORT-ROUTINE.
           PERFORM CLOSE-WK2-FILE.

      ***************************************
       DETAIL-LINE-PROCESS SECTION.
      ***************************************
           IF FIRST-TIME = "Y"
              MOVE EXT-CONAME-RPT-LINES TO LN-COUNT
              MOVE 1 TO LN-FEED
              MOVE "Y" TO RECORDS-FOUND
              MOVE "N" TO FIRST-TIME.

      *- - - - - - - - - - - - - - - - - - - -{ GET ACCTNO WITH STATUS }
           PERFORM GET-DISPLAY-LNNO.
           MOVE LPS-DISPLAY-LNNO TO D-DISPLAY-LNNO.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           PERFORM FIND-BORROWER.
           MOVE TP-MAKER TO D-MAKER.
           MOVE 0        TO D-SSNO.
           MOVE TP-DLNO  TO D-DLNO.

           IF HOLD-TP-ACCTNO = TP-ACCTNO AND
               HOLD-TP-OTHBR-FG = TP-OTHBR-FG AND
                HOLD-TP-SSNO = TP-SSNO
              MOVE SPACES TO DETAIL-LINE
            ELSE
              MOVE 1 TO LN-FEED
              MOVE TP-ACCTNO    TO HOLD-TP-ACCTNO
              MOVE TP-OTHBR-FG  TO HOLD-TP-OTHBR-FG
              MOVE TP-SSNO      TO HOLD-TP-SSNO
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
           IF TP-TRCD = "OT" OR "O2"
              MOVE TP-INTDUE TO TP-TRAMT
              MOVE 0 TO TP-INTDUE.
           MOVE TP-TRAMT TO D-TRAMT.
           ADD TP-APOTH TP-APINT TP-APLC GIVING D-APINT.
      * "OP" DOESNT STORE THE AMOUNT ANYWHERE EXCEPT APCUR
           IF TP-TRCD = "OP"
              COMPUTE D-APINT = TP-APCUR * -1.
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

      *************************************************
       CREATE-WK SECTION.
      *************************************************
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
           IF TP-OTHBR-FG = " " OR "C"
              IF TP-TRCD = "OP"
                 COMPUTE WK-TRAMT = WK-TRAMT + (TP-APCUR * -1)
                 COMPUTE T-TRAMT(LEV) = T-TRAMT(LEV) + (TP-APCUR * -1)
              ELSE
              IF TP-TRCD = "OT" OR "O2"
                 ADD TP-TRAMT TO WK-TRAMT T-TRAMT(LEV)
              ELSE
                 ADD TP-APINT TP-APOTH TP-APLC TO WK-TRAMT T-TRAMT(LEV).

           IF TP-OTHBR-FG = " " OR "G"
              ADD TP-APOTH TO WK-APOTH T-APOTH(LEV)
              ADD TP-APINT TO WK-APINT T-APINT(LEV)
              ADD TP-APLC TO WK-APLC T-APLC(LEV)
              ADD TP-APCUR TO WK-APCUR T-APCUR(LEV)
              ADD HOLD-EARNINGS TO WK-EARNINGS T-EARNINGS(LEV)
              ADD TP-DLPROC TO WK-DLPROC T-DLPROC(LEV).

           IF TP-OTHBR-FG = " " OR "G"
              IF TP-TRCD = "OP"
                 COMPUTE WK-APOTH = WK-APOTH + (TP-APCUR * -1)
                 COMPUTE T-APOTH(LEV) = T-APOTH(LEV) + (TP-APCUR * -1).

           PERFORM WRITE-WK-FILE.
           IF IO-FG NOT = 0
              PERFORM REWRITE-WK-FILE.

           MOVE 0 TO SUB.
           PERFORM OUTPUT-GL 3 TIMES.
           PERFORM CREATE-TOTAL-WK.

      *******************************************
       CREATE-TOTAL-WK SECTION.
      *******************************************
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

           IF TP-OTHBR-FG = " " OR "C"
              IF TP-TRCD = "OP"
                 COMPUTE WK-TRAMT = WK-TRAMT + (TP-APCUR * -1)
              ELSE
                 IF TP-TRCD = "OT" OR "O2"
                    ADD TP-TRAMT TO WK-TRAMT
                 ELSE
                    ADD TP-APINT TP-APOTH TP-APLC TO WK-TRAMT.

           IF TP-OTHBR-FG = " " OR "G"
              ADD TP-APOTH TO WK-APOTH
              ADD TP-APINT TO WK-APINT
              ADD TP-APLC TO WK-APLC
              ADD TP-APCUR TO WK-APCUR
              ADD HOLD-EARNINGS TO WK-EARNINGS
              ADD TP-DLPROC TO WK-DLPROC
              IF TP-TRCD = "OP"
                 COMPUTE WK-APOTH = WK-APOTH + (TP-APCUR * -1).

           PERFORM WRITE-WK-FILE.
           IF IO-FG NOT = 0
              PERFORM REWRITE-WK-FILE.

      ***********************************
       OUTPUT-GL SECTION.
      ***********************************
           ADD 1 TO SUB.
           IF TP-GLAMT(SUB) = 0
              GO TO END-OUTPUT-GL.
      * ONLY GIVE THE GL AMT TO THE BRANCH THAT IT WAS
      *   MADE FOR
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

      ***********************************
       FIND-BORROWER SECTION.
      ***********************************
           IF TP-OTHBR-FG = "C"
              MOVE "INTER-OFFICE PAYMENT" TO D-NAME
              GO TO END-FIND-BORROWER.

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

      *******************************
       WRITE-DETAIL-LINE SECTION.
      *******************************
           ADD LN-FEED LN-COUNT GIVING LN-WK.
           IF LN-WK GREATER THAN EXT-CONAME-RPT-LINES
              PERFORM HEAD-PAGE.
           MOVE DETAIL-LINE TO PR-REC.
           PERFORM WRITE-PR-REC.
           ADD LN-FEED TO LN-COUNT.
           MOVE 1 TO LN-FEED.
           MOVE SPACES TO DETAIL-LINE.

      ***********************************
       SET-CLASS-HEADINGS SECTION.
      ***********************************
           MOVE HOLD-CLASS TO H-LCNO LC-CODE.
           PERFORM READ-LC1-FILE.
           IF IO-FG NOT = 0
              MOVE "INVALID" TO LC-DESC.
           MOVE LC-DESC TO H-LCDESC.
           MOVE "N" TO SUMMARY-FG.

      *********************************
       CLASS-TOTALS SECTION.
      *********************************

           IF ( GTT-ISO = "O" )
              GO TO END-CLASS-TOTALS.

           MOVE "Y" TO SUMMARY-FG.
           MOVE EXT-CONAME-RPT-LINES TO LN-COUNT.
           PERFORM WRITE-DETAIL-LINE.
           MOVE CLASS-HEAD TO PR-REC.
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-PR-REC.
           MOVE SPACE TO WK-KEY.
           MOVE "L" TO WK-CODE.
           MOVE HOLD-BRNO TO WK-BRNO.
           MOVE HOLD-CLASS TO WK-CLASS.
           PERFORM START-WK-FILE.
       PRINT-NEXT-CODE.
           PERFORM READ-WK-FILE-NEXT.

           IF IO-FG = 9 OR WK-CODE NOT = "L" OR
              WK-CLASS NOT = HOLD-CLASS OR WK-BRNO NOT = HOLD-BRNO
              PERFORM TYPE-TOTALS
              GO TO END-CLASS-TOTALS.

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
           MOVE 1 TO LN-FEED.

           GO TO PRINT-NEXT-CODE.
       END-CLASS-TOTALS.
           MOVE 2 TO LEV.
           MOVE 1 TO SUB.
           PERFORM UPDATE-TABLE2.
           MOVE 1 TO SUB.
           PERFORM ZERO-TOTAL-TBL.
           MOVE "N" TO SUMMARY-FG.

      *******************************
       BRANCH-TOTALS SECTION.
      *******************************

           IF ( GTT-ISO = "O" )
              GO TO END-BRANCH-TOTALS.

           MOVE "Y" TO SUMMARY-FG.
           MOVE 0 TO HOLD-CLASS.
           MOVE 2 TO LEV.
           MOVE "BRANCH TOTALS:" TO DETAIL-LINE.
           PERFORM PRINT-SUMMARY.
       END-BRANCH-TOTALS.
           MOVE 3 TO LEV.
           MOVE 2 TO SUB.
           PERFORM UPDATE-TABLE2.
           MOVE 2 TO SUB.
           PERFORM ZERO-TOTAL-TBL.
           MOVE "N" TO SUMMARY-FG.

      ********************************
       GRAND-TOTALS SECTION.
      ********************************

           IF ( GTT-ISO = "O" )
              GO TO END-GRAND-SUMMARY-EXIT.

           MOVE "Y" TO SUMMARY-FG.
           MOVE EXT-CONAME-RPT-LINES TO LN-COUNT.
           PERFORM WRITE-DETAIL-LINE.
           MOVE GRAND-HEAD TO PR-REC.
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-PR-REC.
           MOVE SPACE TO WK-KEY.
           MOVE "S" TO WK-CODE.
           PERFORM START-WK-FILE.
       PRINT-NEXT-SUM.
           PERFORM READ-WK-FILE-NEXT.
           IF IO-FG NOT = 0 OR
             WK-CODE NOT = "S"
              GO TO END-GRAND-SUMMARY.
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
       END-GRAND-SUMMARY.
           MOVE "Y" TO SUMMARY-FG.
           MOVE 3 TO LEV.
           MOVE "GRAND TOTALS :" TO DETAIL-LINE.
           PERFORM PRINT-SUMMARY.
       END-GRAND-SUMMARY-EXIT.
           EXIT.

      ********************************
       GL-SUMMARY SECTION.
      ********************************
           MOVE GL-HEAD TO DETAIL-LINE.
           MOVE 4 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE SPACE TO WK-REC.
           MOVE "G" TO WK-CODE.
           MOVE 0 TO WK-GLNO.
           PERFORM START-WK-FILE.
      *----------------------------------------
       PRINT-NEXT-GL.
           PERFORM READ-WK-FILE-NEXT.
           IF IO-FG NOT = 0 OR WK-CODE NOT = "G"
              GO TO END-GL-SUMMARY.
           IF GRAND-SUMMARY NOT = "Y"
              IF WK-BRNO NOT = HOLD-BRNO
                 GO TO PRINT-NEXT-GL.
           IF WK-GLAMT = 0
              GO TO PRINT-NEXT-GL.
           MOVE WK-GLNO TO DS-GLNO.
           INSPECT DS-GLNO REPLACING ALL "/" BY "-".
           MOVE WK-GLAMT TO DS-GLAMT.
           PERFORM WRITE-DETAIL-LINE.
           GO TO PRINT-NEXT-GL.
       END-GL-SUMMARY.

      *******************************
       HEAD-PAGE SECTION.
      *******************************
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
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
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
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
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
           MOVE "MONTH-END DATE" TO RANGE-LINE.
           MOVE EDATE TO R-NUM.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "DAY OF MONTH" TO RANGE-LINE.
           MOVE DAY-OF-MONTH TO R-NUM.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "REVERSALS ONLY" TO RANGE-LINE.
           MOVE REVERSALS-ONLY   TO R-CHAR.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "ACTION CODE2" TO RANGE-LINE.
           MOVE ACTION-CODE2   TO R-CHAR.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "GROUP TOTALS" TO RANGE-LINE.
           MOVE GTT-ISO        TO R-CHAR.
           PERFORM WRITE-DETAIL-LINE.

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

      ******************************************************************
       GTT-ADD-TOTALS SECTION.
      ******************************************************************

           IF  WK-TRCD = "OT" OR "O2"
               MOVE 1 TO GTT-TYPE-SUB
           ELSE
               MOVE 2 TO GTT-TYPE-SUB.
           ADD WK-NO    TO  WS-GTT-NO    (GTT-TOTAL-SUB, GTT-TYPE-SUB).
           ADD WK-TRAMT TO  WS-GTT-TRAMT (GTT-TOTAL-SUB, GTT-TYPE-SUB).
           ADD WK-APOTH TO  WS-GTT-APOTH (GTT-TOTAL-SUB, GTT-TYPE-SUB).
           ADD WK-APINT TO  WS-GTT-APINT (GTT-TOTAL-SUB, GTT-TYPE-SUB).
           ADD WK-APLC  TO  WS-GTT-APLC  (GTT-TOTAL-SUB, GTT-TYPE-SUB).
           ADD WK-APCUR TO  WS-GTT-APCUR (GTT-TOTAL-SUB, GTT-TYPE-SUB).
           ADD WK-EARNINGS TO
                            WS-GTT-EARNINGS(GTT-TOTAL-SUB,GTT-TYPE-SUB).
           ADD WK-DLPROC TO WS-GTT-DLPROC(GTT-TOTAL-SUB, GTT-TYPE-SUB).

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

           MOVE ZEROS TO GTT-TYPE-SUB.
       GTT-PRT-TOTALS-NEXT.
           ADD 1 TO GTT-TYPE-SUB.

           IF GTT-TYPE-SUB > GTT-TYPE-MAX
              GO TO END-GTT-PRINT-TOTALS.

           IF GTT-TYPE-SUB = 1
               MOVE "OT"   TO DS-TRCD
           ELSE
               MOVE "AP"   TO DS-TRCD.

           MOVE WS-GTT-NO(GTT-TOTAL-SUB,GTT-TYPE-SUB) TO DS-NO.
           MOVE WS-GTT-TRAMT(GTT-TOTAL-SUB,GTT-TYPE-SUB) TO DS-TRAMT.
           MOVE WS-GTT-APOTH(GTT-TOTAL-SUB,GTT-TYPE-SUB) TO DS-APOTH.
           MOVE WS-GTT-APINT(GTT-TOTAL-SUB,GTT-TYPE-SUB) TO DS-APINT.
           MOVE WS-GTT-APLC(GTT-TOTAL-SUB,GTT-TYPE-SUB)  TO DS-APLC.
           MOVE WS-GTT-APCUR(GTT-TOTAL-SUB,GTT-TYPE-SUB) TO DS-APCUR.
           MOVE WS-GTT-EARNINGS(GTT-TOTAL-SUB,GTT-TYPE-SUB)
                                                      TO DS-EARNINGS.
           MOVE WS-GTT-DLPROC(GTT-TOTAL-SUB,GTT-TYPE-SUB) TO DS-DLPROC.

      *- - - - - - - - - - - - - - - - - - PRINT DETAIL AFTER LOAD - - -
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      *    IF ( WS-HOLD-PROMPT = "GROUP :" )
      *       MOVE ALL "-" TO DETAIL-LINE
      *       PERFORM WRITE-DETAIL-LINE.

           GO TO GTT-PRT-TOTALS-NEXT.

       END-GTT-PRINT-TOTALS.

           IF ( WS-HOLD-PROMPT = "GROUP :" )
              MOVE ALL "-" TO DETAIL-LINE
              MOVE 1 TO LN-FEED
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

           MOVE TP-BRNO  TO LN-OWNBR.
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

       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/TIMEIO.CPY".
       COPY "LIBGB/GRWORK.CPY".
       COPY "LIBGB/GRSERCH.CPY".
       COPY "LIBGB/BRSECURE.CPY".
       COPY "LIBGB/EOSPOOL.CPY".
       COPY "LIBGB/GRTOTALS.CPY".
       COPY "LIBWI/GRTOTL.CPY".
       COPY "LIBLP/LPMDTE.CPY".
       COPY "LIBLP/LPSTAT.CPY".

      *********************************************************
       FORM-RESET SECTION.
      *********************************************************
           MOVE EXT-CONAME-CONAME TO EMOTR4-CONAME.
           MOVE EXT-CONAME-SYSDATE TO EMOTR4-SYSDATE.
           DISPLAY EMOTR4-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE EMOTR4-FORM-FIELDS TO WR-BUF.
           MOVE EMOTR4-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      *********************************************************
       SETUP-LINK-INFO SECTION.
      *********************************************************
           MOVE EMOTR4-HELP--FILE TO HELP-LINK-FILE.
           MOVE EMOTR4-HELP--DIR  TO HELP-LINK-DIR.

      *********************************************************
       VDU-CONVERT-FIELD SECTION.
      *********************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBLP/EMOTR4_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      **************************************
       MISC-PARA SECTION.
      **************************************
       COPY "LIBGB/GETPR.CPY".
       COPY "LIBGB/RMFILE.CPY".
       COPY "LIBGB/SYSTEM.CPY".
       COPY "LIBGB/MKTEMP.CPY".
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/DACCESS.CPY".
       SET-BRANCH-HEADINGS.
           MOVE HOLD-BRNO TO H-BRNO.
           MOVE BR-NAME   TO H-BRDESC.

      *-------------------------------------------------------
       SEND-LEGEND.
           MOVE LEGEND TO WR-BUF.
           MOVE MSEL-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.
      *-------------------------------------------------------
       ZERO-TOTAL-TBL.
           MOVE 0 TO T-TRAMT(SUB) T-APOTH(SUB) T-APINT(SUB) T-APLC(SUB)
                     T-APCUR(SUB) T-EARNINGS(SUB) T-DLPROC(SUB).
      *-------------------------------------------------------
       SETUP-SUM-KEY.
           MOVE SPACES TO WK-REC.
           MOVE "S"    TO WK-CODE.
           MOVE 0      TO WK-BRNO.
           MOVE TP-CLASS TO WK-CLASS.
           MOVE TP-TRCD TO WK-TRCD.
      *--------------------------------------------------------
       SETUP-WK-KEY.
           MOVE SPACES   TO WK-REC.
           MOVE "L"      TO WK-CODE.
           MOVE TP-BRNO  TO WK-BRNO.
           MOVE TP-CLASS TO WK-CLASS.
           MOVE TP-TRCD  TO WK-TRCD.
      *--------------------------------------------------------
       SETUP-WK-GL1-KEY.
           MOVE SPACES       TO WK-REC.
           MOVE "G"          TO WK-CODE.
           MOVE TP-GLNO(SUB) TO WK-GLNO.
      *--------------------------------------------------------
       UPDATE-TABLE2.
           ADD T-TRAMT(SUB) TO T-TRAMT(LEV).
           ADD T-APOTH(SUB) TO T-APOTH(LEV).
           ADD T-APINT(SUB) TO T-APINT(LEV).
           ADD T-APLC(SUB) TO T-APLC(LEV).
           ADD T-APCUR(SUB) TO T-APCUR(LEV).
           ADD T-EARNINGS(SUB) TO T-EARNINGS(LEV).
           ADD T-DLPROC(SUB) TO T-DLPROC(LEV).
      *-------------------------------------------------------
       TYPE-TOTALS.
           MOVE 1 TO LEV.
           MOVE "CLASS  TOTALS:" TO DETAIL-LINE.
           PERFORM PRINT-SUMMARY.
      *-------------------------------------------------------
       PRINT-SUMMARY.
           MOVE 0 TO DS-NO.
           MOVE T-TRAMT(LEV) TO DS-TRAMT.
           MOVE T-APOTH(LEV) TO DS-APOTH.
           MOVE T-APINT(LEV) TO DS-APINT.
           MOVE T-APLC(LEV) TO DS-APLC.
           MOVE T-APCUR(LEV) TO DS-APCUR.
           MOVE T-EARNINGS(LEV) TO DS-EARNINGS.
           MOVE T-DLPROC(LEV) TO DS-DLPROC.
           MOVE 3 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      *********************************************************
       IO-ROUTINES SECTION.
      *********************************************************
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
      *-------------------------------------------------
       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO EXT-EOBUF-ERRCD.

      ***************************************************
       END-PROGRAM SECTION.
      ***************************************************

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
              MOVE EXIT-PATH TO FORM-NAM
              MOVE FORM-PATH TO FORM-PATHNAME.

       STOP-RUN.
           IF EXT-ROUTE-BUF = "00" OR "40" OR "50"
              DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN
              DESTROY EMOTR4-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.
           EXIT PROGRAM.
