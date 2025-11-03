       IDENTIFICATION DIVISION.
       PROGRAM-ID.   EMPLR4.
       DATE-WRITTEN. 08/10/2022.
      ******************************************************************
      *
      *                      (LP)
      *    DESCRIPTION:  WRITE OFF REGISTER BY ACTIONCD2
      *                     SECURITIZATION
      *
      * REV:
      *  BAH 20220810 #1565
      **************************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "LIBGB/GBFSPR.CPY".
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           SELECT WK-FILE ASSIGN WK-PATH
                  ORGANIZATION INDEXED
                  ACCESS DYNAMIC
                  LOCK MODE AUTOMATIC WITH LOCK ON RECORD
                  RECORD KEY WK-KEY
                  FILE STATUS FILE-STAT.
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
      *-----------------------------------------------------------------
      * WK-CODE:   "L"  DETAIL LINE
      *            "G"  GL-SUMMARY
      *-----------------------------------------------------------------
       FD  WK-FILE
           LABEL RECORDS ARE STANDARD.
       01  WK-REC.
           03  WK-KEY.
               05  WK-CODE   PIC X.
               05  WK-BRNO   PIC 9(4).
               05  WK-CLASS  PIC 9(3).
               05  WK-TRCD   PIC XX.
           03  WK-NO         PIC S9(7)  COMP-3.
           03  WK-TRAMT      PIC S9(9)V99  COMP-3.
           03  WK-UNEARN-INT PIC S9(9)V99  COMP-3.
           03  WK-ADDON-CHG  PIC S9(9)V99  COMP-3.
           03  WK-SRVS-CHG   PIC S9(9)V99  COMP-3.
           03  WK-MAINT-FEE  PIC S9(9)V99  COMP-3.
           03  WK-DLR-DISC   PIC S9(9)V99  COMP-3.
           03  WK-OTHER      PIC S9(9)V99  COMP-3.
           03  WK-NET-LOSS   PIC S9(9)V99  COMP-3.
           03  WK-RES-AMT    PIC S9(9)V99  COMP-3.
           03  WK-OT2BAL     PIC S9(9)V99  COMP-3.

      ******************************************************************
      *  INDEXED TRFILE BY BRANCH, CLASS, ACCOUNT
      ******************************************************************
       FD  WK2-FILE
           LABEL RECORDS ARE STANDARD.
       01  WK2-REC.
           03  WK2-KEY.
               05  WK2-BRANCH            PIC 9(4).
               05  WK2-CLASS             PIC 9(3).
               05  WK2-ACCTNO            PIC 9(8).
               05  WK2-DATE              PIC 9(8).
               05  WK2-LINE              PIC 9(3).

      ******************************************************************
      *
      *    W O R K I N G - S T O R A G E - S E C T I O N
      *
      ******************************************************************
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)LP/EMPLR4.CBL S35 08/10/22-09:40:45 >".
       COPY "LIBLP/LP01TRP.CPY".
       COPY "LIBLP/LP01TRP_SQL.CPY".
       COPY "LIBLP/LPWSTRP.CPY".
       COPY "LIBLP/LP01BW.CPY".
       COPY "LIBLP/LP01BW_SQL.CPY".
       COPY "LIBLP/LPWSBW.CPY".
       COPY "LIBLP/LP01LN.CPY".
       COPY "LIBLP/LP01LN_SQL.CPY".
       COPY "LIBLP/LPWSLN.CPY".
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBLP/LP01LC.CPY".
       COPY "LIBLP/LP01LC_SQL.CPY".
       COPY "LIBLP/LPWSLC.CPY".
       COPY "LIBLP/LP01LP.CPY".
       COPY "LIBLP/LP01LP_SQL.CPY".
       COPY "LIBLP/LPWSLP.CPY".
       COPY "LIBLP/LP01SP.CPY".
       COPY "LIBLP/LP01SPA.CPY".
       COPY "LIBLP/LP01SPB.CPY".
       COPY "LIBLP/LP01SPC.CPY".
       COPY "LIBLP/LP01SP_SQL.CPY".
       COPY "LIBLP/LPWSSP.CPY".
       COPY "LIBGB/GB01GR.CPY".
       COPY "LIBGB/GB01GR_SQL.CPY".
       COPY "LIBGB/GBWSGR.CPY".
      *----------------------------------------------------------------
       01  PR-IFN               PIC X(2)     VALUE "PR".
       01  WK2-IFN              PIC X(3)     VALUE "WK2".
       01  WK-IFN               PIC X(2)     VALUE "WK".

       01  WK-PATH              PIC X(35).
       01  WK2-PATH             PIC X(35).

      ******************************************************************
      *
      *    L I N E - W O R K E R S
      *
      ******************************************************************
       01  LN-WORKERS.
           03  LN-FEED          PIC 999      VALUE 0.
           03  LN-COUNT         PIC 999      VALUE 99.
           03  LN-WK            PIC 999      VALUE 0.
       01  PAGE-NUMBER          PIC 9(4)     VALUE 0.

       01  ER-FEED              PIC 9        VALUE 1.

      ******************************************************************
      *
      *    M I S C - W O R K E R S
      *
      ******************************************************************
       01  HOLD-TP-OTHBR-FG     PIC X                VALUE "?".
       01  HOLD-TP-ACCTNO       PIC 9(8)      COMP-3 VALUE 99999999.
       01  HOLD-TP-SSNO         PIC 9(10)     COMP-3 VALUE 9999999999.
       01  SUB                  PIC 9(2)      COMP-3.
       01  SUB1                 PIC 9(1)      COMP-3.
       01  SUB-1                PIC 9(2)      COMP-3.
       01  SUB-1-MAX            PIC 9(2)      COMP-3.
       01  SUB-2                PIC 9(2)      COMP-3.
       01  SUB-2-MAX            PIC 9(2)      COMP-3.
       01  SUB-3                PIC 9(2)      COMP-3.
       01  SUB-4                PIC 9(2)      COMP-3.
       01  LEV                  PIC 99        COMP-3.
       01  LEV-PL               PIC 99        COMP-3.
       01  HOLD-BRNO            PIC 9(4)  VALUE 0.
       01  HOLD-CLASS           PIC 9(3)  VALUE 0.
       01  RECORDS-FOUND        PIC X     VALUE " ".
           88  NO-RECORDS-FOUND           VALUE "N".
       01  SUMMARY-FG           PIC X     VALUE "N".
       01  NAME-WORKER          PIC X(34).
       01  FIRST-NAME           PIC X(16).
       01  SAME-MONTH-FG        PIC X     VALUE "N".

       01  FIRST-TIME           PIC X        VALUE "Y".
       01  MESS-LIST            PIC X(5)     VALUE "MESS.".
       01  MSEL-LIST            PIC X(5)     VALUE "MSEL.".
       01  CLASS-FG             PIC X(3)     VALUE "   ".
           88  CLASS-BEG                     VALUE "BEG".
           88  CLASS-END                     VALUE "END".

      *-----------------------------------------------------------------
       01  MESSAGE-01-LOC-PRT   PIC X(43)    VALUE
           "CANNOT DO LOCAL SPOOLING WITH GROUP TOTALS".

      ******************************************************************
       01  WS-GTT-TOTAL-TABLE.
           03  WS-GTT-TABLE-GROUP-LEVEL                 OCCURS 10 TIMES.
               05  WS-GTT-NO              PIC S9(7)     COMP.
               05  WS-GTT-TRAMT           PIC S9(9)V99  COMP.
               05  WS-GTT-UNEARN-INT      PIC S9(9)V99  COMP.
               05  WS-GTT-ADDON-CHG       PIC S9(9)V99  COMP.
               05  WS-GTT-SRVS-CHG        PIC S9(9)V99  COMP.
               05  WS-GTT-MAINT-FEE       PIC S9(9)V99  COMP.
               05  WS-GTT-DLR-DISC        PIC S9(9)V99  COMP.
               05  WS-GTT-OTHER           PIC S9(9)V99  COMP.
               05  WS-GTT-NET-LOSS        PIC S9(9)V99  COMP.
               05  WS-GTT-RES-AMT         PIC S9(9)V99  COMP.
               05  WS-GTT-OT2BAL          PIC S9(9)V99  COMP.

      *-----------------------------------------------------------------
       01  WS-HOLD-PROMPT       PIC X(7)  VALUE SPACES.

       01  WS-PROMPT-1.
           03  WS-PROMPT-1X     PIC X(1)  OCCURS 19 TIMES VALUE SPACES.

       01  WS-PROMPT-2.
           03  WS-PROMPT-2X     PIC X(1)  OCCURS 19 TIMES VALUE SPACES.

      ******************************************************************
      *  TOTALS LEV1 = CLASS TOTALS  |   LEV-PL = 1 --- PL
      *            2 = BRANCH TOTALS |   LEV-PL = 2 --- P2
      *            3 = GRAND TOTALS  |   LEV-PL = 3 --- P3
      *                              |   LEV-PL = 4 --- P4
      *                              |   LEV-PL = 5 --- P5
      *                              |   LEV-PL = 6 --- PX
      *                              |   LEV-PL = 7 TOTALS
      ******************************************************************
       01  TOTALS-TAB   VALUE ZEROS.
           03  TOTAL-TBL       OCCURS 3.
               05  TOTAL-TBL2      OCCURS 7.
                   07  T-TRAMT      PIC S9(9)V99.
                   07  T-UNEARN-INT PIC S9(9)V99.
                   07  T-ADDON-CHG  PIC S9(9)V99.
                   07  T-SRVS-CHG   PIC S9(9)V99.
                   07  T-MAINT-FEE  PIC S9(9)V99.
                   07  T-DLR-DISC   PIC S9(9)V99.
                   07  T-OTHER      PIC S9(9)V99.
                   07  T-NET-LOSS   PIC S9(9)V99.
                   07  T-RES-AMT    PIC S9(7)V99.
                   07  T-OT2BAL     PIC S9(7)V99.

      *-----------------------------------------------------------------
       01  PCODE.
           03  PLCODES         PIC X(12) VALUE
                   "PLP2P3P4P5PX".
           03  PL-CODE REDEFINES PLCODES
                    OCCURS 6 TIMES PIC X(2).

       01  LEGEND               PIC X(70).

       01  HOLD-NET-LOSS        PIC S9(9)V99      COMP-3.
       01  TRUE-NET-LOSS        PIC S9(9)V99      COMP-3.


      ******************************************************************
      *
      *    S C R E E N   F I E L D S
      *
      ******************************************************************
       COPY "LIBGB/EOBUF_EXT.CPY".
      *-----------------------------------------------------------------
           03  BEG-BRANCH              PIC 9(4).
           03  END-BRANCH              PIC 9(4).
           03  BEG-CLASS               PIC 9(3).
           03  END-CLASS               PIC 9(3).
           03  END-DATE.
               05  END-MONTH           PIC 99.
               05  END-YEAR            PIC 99.
           03  EDATE                   REDEFINES END-DATE PIC 9999.
           03  DAY-OF-MONTH            PIC 99.
           03  GTT-ISO                 PIC X(1).
           03  INCL-DETAIL             PIC X.
           03  GROUP-FLAG              PIC X.
           03  ENT-GROUP               PIC X(4).
           03  LOCAL-PRINT-FG          PIC X(1).
           03  TOTAL-LEV-TABLE.
               05  TOTAL-LEVEL-TABLE   PIC X(1)   OCCURS 10.
           03  REVERSALS-ONLY          PIC X(1).
           03  PL-TYPE                 PIC X(1).
           03  SIGNATURE-LINE          PIC X(1).
           03  ACTION-CODE2            PIC X(4).

       01  VERSION-CONTROL             PIC X      VALUE "D".

       01  BEGINNING-DATE              PIC 9(8).
       01  ENDING-DATE                 PIC 9(8).
       01  SV-BEG-BR                   PIC 9(4)   VALUE 9999.
       01  SV-END-BR                   PIC 9(4)   VALUE 9999.
       01  SV-GROUP-FLAG       PIC X     VALUE SPACES.
       01  SV-ENT-GROUP        PIC X(4)  VALUE SPACES.
       01  SV-LOCAL-PRINT-FG   PIC X     VALUE SPACES.
      *-----------------------------------------------------------------

      ******************************************************************
      *
      *     P R I N T - B U F F E R S
      *
      ******************************************************************
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
           03  FILLER           PIC X(32)    VALUE " ".
           03  FILLER           PIC X(41) VALUE
               "WRITE-OFF REGISTER (SECURITIZATION)".

      *-----------------------------------------------------------------
       01  HEAD3.
           03  FILLER           PIC X(10) VALUE "BRANCH :".
           03  H-BRNO           PIC 9(4).
           03  H-BRNOX          REDEFINES H-BRNO PIC X(4).
           03  FILLER           PIC X VALUE " ".
           03  H-BRDESC         PIC X(20).
           03  FILLER           PIC X(46) VALUE " ".

      *-----------------------------------------------------------------
       01  HEAD4.
           03  FILLER           PIC X(10) VALUE "CLASS  :".
           03  H-LCNO           PIC Z99.
           03  H-LCNOX          REDEFINES H-LCNO PIC X(3).
           03  FILLER           PIC X VALUE " ".
           03  H-LCDESC         PIC X(20).

      *-----------------------------------------------------------------
       01  HEAD5.
           03  FILLER           PIC X(7)     VALUE "ACCTNO ".
           03  FILLER           PIC X(6)     VALUE "      ".
           03  FILLER           PIC X(11)    VALUE "           ".
           03  FILLER           PIC X(11)    VALUE "           ".
           03  FILLER           PIC X(11)    VALUE "  UNEARNED ".
           03  FILLER           PIC X(11)    VALUE "      SSNO ".
           03  FILLER           PIC X(11)    VALUE "    REASON ".
           03  FILLER           PIC X(11)    VALUE "  LOANDATE ".
           03  FILLER           PIC X(11)    VALUE "  DLR DISC ".
           03  FILLER           PIC X(15)    VALUE "               ".
           03  FILLER           PIC X(14)    VALUE " AMOUNT FROM  ".
           03  FILLER           PIC X(9)     VALUE "   DLNO  ".
           03  FILLER           PIC X(3)     VALUE "RSV".

      *-----------------------------------------------------------------
       01  HEAD6.
           03  FILLER            PIC X(7).
           03  FILLER            PIC X(6)     VALUE "TRCD  ".
           03  FILLER            PIC X(11)    VALUE " TRDATE    ".
           03  FILLER            PIC X(11)    VALUE "     TRAMT ".
           03  FILLER            PIC X(11)    VALUE "  INTEREST ".
           03  FILLER            PIC X(11)    VALUE "     ADDON ".
           03  FILLER            PIC X(11)    VALUE "    SERCHG ".
           03  FILLER            PIC X(11)    VALUE "  MAINTFEE ".
           03  FILLER            PIC X(11)    VALUE "     OTHER ".
           03  FILLER            PIC X(15)    VALUE "     NET LOSS  ".
           03  FILLER            PIC X(14)    VALUE " RESERVE      ".
           03  FILLER            PIC X(9)     VALUE " OT2BAL  ".
           03  FILLER            PIC X(3)     VALUE "NUM".

      *-----------------------------------------------------------------
       01  SUM-HEAD.
           03  FILLER            PIC X(20)  VALUE
           "CLASS     &&&&&&    ".
           03  FILLER            PIC X(4)   VALUE "TR  ".
           03  FILLER            PIC X(9)   VALUE "NO       ".
           03  FILLER            PIC X(11)  VALUE "TRANS      ".
           03  FILLER            PIC X(15)  VALUE "UNEARNED       ".
           03  FILLER            PIC X(12)  VALUE "ADDON       ".
           03  FILLER            PIC X(15)  VALUE "SERVICE        ".
           03  FILLER            PIC X(10)  VALUE "MAINT     ".
           03  FILLER            PIC X(13)  VALUE "DLR DISC     ".
           03  FILLER            PIC X(10)  VALUE "NET LOSS  ".
           03  FILLER            PIC X(11)  VALUE "AMT FRM RSV".

      *-----------------------------------------------------------------
       01  SUM-HEAD2.
           03  FILLER            PIC X(20)  VALUE SPACES.
           03  FILLER            PIC X(4)   VALUE "CD  ".
           03  FILLER            PIC X(9)   VALUE "         ".
           03  FILLER            PIC X(11)  VALUE "AMOUNT     ".
           03  FILLER            PIC X(15)  VALUE "INTEREST       ".
           03  FILLER            PIC X(12)  VALUE "CHARGE      ".
           03  FILLER            PIC X(15)  VALUE "CHARGE         ".
           03  FILLER            PIC X(10)  VALUE "FEE       ".
           03  FILLER            PIC X(13)  VALUE "OTHER        ".
           03  FILLER            PIC X(10)  VALUE "          ".
           03  FILLER            PIC X(11)  VALUE "OT2BAL     ".

      *-----------------------------------------------------------------
       01  GTT-HEAD1.
           03  FILLER            PIC X(57)  VALUE SPACES.
           03  FILLER            PIC X(18)  VALUE "GROUP TOTALS TABLE".
           03  FILLER            PIC X(57)  VALUE SPACES.

      *-----------------------------------------------------------------
       01  GTT-HEAD2.
           03  FILLER            PIC X(24)  VALUE
               "BRANCH/GROUP   GR       ".
           03  FILLER            PIC X(9)   VALUE "NO       ".
           03  FILLER            PIC X(11)  VALUE "TRANS      ".
           03  FILLER            PIC X(15)  VALUE "UNEARNED       ".
           03  FILLER            PIC X(12)  VALUE "ADDON       ".
           03  FILLER            PIC X(15)  VALUE "SERVICE        ".
           03  FILLER            PIC X(10)  VALUE "MAINT     ".
           03  FILLER            PIC X(13)  VALUE "DLR DISC     ".
           03  FILLER            PIC X(10)  VALUE "NET LOSS  ".
           03  FILLER            PIC X(11)  VALUE "AMT FRM RSV".

      *-----------------------------------------------------------------
       01  GTT-HEAD3.
           03  FILLER            PIC X(14)  VALUE SPACES.
           03  FILLER            PIC X(10)  VALUE "LEV       ".
           03  FILLER            PIC X(9)   VALUE "         ".
           03  FILLER            PIC X(11)  VALUE "AMOUNT     ".
           03  FILLER            PIC X(15)  VALUE "INTEREST       ".
           03  FILLER            PIC X(12)  VALUE "CHARGE      ".
           03  FILLER            PIC X(15)  VALUE "CHARGE         ".
           03  FILLER            PIC X(10)  VALUE "FEE       ".
           03  FILLER            PIC X(13)  VALUE "OTHER        ".
           03  FILLER            PIC X(10)  VALUE "          ".
           03  FILLER            PIC X(11)  VALUE "OT2BAL     ".

      ******************************************************************
      *
      *    D E T A I L - L I N E
      *
      ******************************************************************
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
           03  D-SSNO           PIC 999/99/9999 BLANK ZERO.
           03  FILLER           PIC X(5).
           03  D-REASON         PIC XX.
           03  FILLER           PIC X(3).
           03  D-LOANDATE       PIC 99/99/99.
           03  FILLER           PIC X(50).
           03  D-ACTIONCD2      PIC X(4).

       01  D-LINE2 REDEFINES DETAIL-LINE.
           03  D2-GROUP             PIC X(19).
           03  D2-GROUP-1           REDEFINES D2-GROUP.
               05  DS-CLASS         PIC Z99.
               05  FILLER           PIC X.
               05  DS-DESC          PIC X(15).
           03  D2-GROUP-2           REDEFINES D2-GROUP.
               05  D2-PROMPT1       PIC X(7).
               05  FILLER           PIC X(2).
               05  D2-GRNO          PIC X(4).
               05  D2-BRNO          REDEFINES D2-GRNO
                                    PIC 9(4).
               05  FILLER           PIC X(3).
               05  D2-GR-LEVEL      PIC 9(1).
               05  FILLER           PIC X(2).
           03  FILLER           PIC X.
           03  DS-TRCD          PIC XX.
           03  FILLER           PIC X.
           03  DS-NO            PIC ZZZ.
           03  FILLER           PIC X.
           03  DS-TRAMT         PIC ZZZZZZZZZ.ZZ-.
           03  DS-UNEARN-INT    PIC ZZZZZZZZZ.ZZ-.
           03  DS-ADDON-CHG     PIC ZZZZZZZZZ.ZZ-.
           03  DS-SRVS-CHG      PIC ZZZZZZZZZ.ZZ-.
           03  DS-MAINT-FEE     PIC ZZZZZZZZZ.ZZ-.
           03  DS-DLR-DISC      PIC ZZZZZZZZZ.ZZ-.
           03  DS-NET-LOSS      PIC ZZZZZZZZZ.ZZ-.
           03  DS-RES-AMT       PIC ZZZZZZZZZ.ZZ-.

      *-----------------------------------------------------------------
       01  D-LINE3 REDEFINES DETAIL-LINE.
           03  FILLER           PIC X(8).
           03  D-TR-CODE        PIC XX.
           03  D-REV            PIC X.
           03  FILLER           PIC X(2).
           03  D-DATE           PIC 99/99/99.
           03  FILLER           PIC X(3).
           03  D-TRANS-AMT      PIC ZZZZZZ9.99-.
           03  D-CHARGES        PIC ZZZZZZZ.ZZ- OCCURS 5.
           03  FILLER           PIC X.
           03  D-NET-LOSS       PIC ZZZZZZZZZ.ZZ-.
           03  FILLER           PIC X.
           03  D-RES-AMT        PIC ZZZZZZZZ.ZZ-.
           03  FILLER           PIC X.
           03  D-DLNO           PIC ZZZZZ.ZZ-.
           03  FILLER           PIC X.
           03  D-REFNO          PIC X(5).

      *-----------------------------------------------------------------
       01  RANGE-LINE REDEFINES DETAIL-LINE.
           03  FILLER           PIC X(35).
           03  RANGE-SELECTION.
               04  R-NUM        PIC Z(5)9.
               04  R-NUM-REDEF  REDEFINES R-NUM.
                   05  R-CHAR   PIC X(6).
      ******************************************************************
      *
      *     C O P Y - L I B R A R Y - W O R K E R S
      *
      ******************************************************************
       COPY "LIBWI/GRSCANW.CPY".
       COPY "LIBGB/PASSWDW.CPY".
       COPY "LIBLP/EMPLR4_CMD.CPY".
       COPY "LIBWI/LCSCANW.CPY".
       COPY "LIBWI/CDSCANW.CPY".
       COPY "LIBGB/GRWORKW.CPY".
       COPY "LIBWI/GRTOTLW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBGB/MKTEMPW_CMD.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBLP/LPMDTEW.CPY".
       COPY "LIBLP/LPSTATW.CPY".
       COPY "LIBGB/BRSECUREW.CPY".
       COPY "LIBGB/GPENVW.CPY".
       COPY "LIBGB/SYSTEMW.CPY".
       COPY "LIBGB/EOSPOOLW.CPY".
       COPY "LIBGB/GETPRW.CPY".
       COPY "LIBGB/DACCESSW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBLP/EMPLR4_DEF.CPY".
       COPY "LIBLP/EMPLR4_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

      ******************************************************************
      *
      *     P R O G R A M   C O N T R O L
      *
      ******************************************************************
       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBLP/EMPLR4_SCN.CPY".

       PROCEDURE DIVISION
             USING FORM-PATHNAME EXIT-PATHNAME.

       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
               PR-FILE WK-FILE WK2-FILE.
       COPY "LIBGB/DECLRP.CPY".
           PERFORM CLOSE-TRP1-FILE.
           PERFORM CLOSE-SP1-FILE.
           PERFORM CLOSE-LP1-FILE.
           PERFORM CLOSE-LN1-FILE.
           PERFORM CLOSE-LC1-FILE.
           PERFORM CLOSE-BW1-FILE.
           PERFORM CLOSE-GR1-FILE.
           PERFORM CLOSE-BR-FILE.
           PERFORM CLOSE-PR-FILE.
           CLOSE WK-FILE WK2-FILE.
       COPY "LIBGB/DECLRP2.CPY".

      ******************************************************************
      *
      *    M A I N - M O D U L E
      *
      ******************************************************************
       MAIN-MODULE SECTION.

           PERFORM SQL-CONNECT.

           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "EMPLR4" TO FORM-PROG VDU-FORM-NAME.

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

           MOVE EXT-FILPATH-BASE           TO FILPATH-BASE.
           MOVE BR-MACHINE                 TO FILPATH-MACHINE.
           MOVE BR-DATA-PATH               TO FILPATH-DATA.

           MOVE "B"                        TO BW-PATH-OVERRIDE
                                              LN-PATH-OVERRIDE
                                              LP-PATH-OVERRIDE
                                              TRP-PATH-OVERRIDE.


           PERFORM OPEN-TRP1-FILE.
           PERFORM OPEN-BW1-FILE.
           PERFORM OPEN-LN1-FILE.

           PERFORM SORT-ROUTINE.

           IF EXT-EOBUF-ERRCD = "X"
              GO TO NEXT-GROUP-LOC.

           PERFORM OPEN-WK2-FILE.

       NEXT-TR-REPORT.

           PERFORM READ-WK2-FILE-NEXT.
           IF IO-FG = 9
              GO TO END-OF-GROUP.

           MOVE WK2-BRANCH TO TP-BRNO.
           MOVE WK2-DATE   TO TP-DATE.
           MOVE WK2-CLASS  TO TP-CLASS.
           MOVE WK2-ACCTNO TO TP-ACCTNO.
           MOVE WK2-LINE   TO TP-LINE.
           PERFORM READ-TRP1-FILE.
           IF ( IO-FG = 9 )
              MOVE "TRANSACTION RECORD MISSING SINCE SORT" TO MESS
              PERFORM SEND-MESS
              GO TO IO-ERROR.

           IF ( FIRST-TIME = "Y" )
              MOVE TP-BRNO  TO HOLD-BRNO
              MOVE TP-CLASS TO HOLD-CLASS
              PERFORM SET-CLASS-HEADINGS
              PERFORM SET-BRANCH-HEADINGS.

           IF ( TP-BRNO NOT = HOLD-BRNO )
              PERFORM CLASS-TOTALS
              PERFORM BRANCH-TOTALS
              MOVE TP-BRNO  TO HOLD-BRNO
              MOVE TP-CLASS TO HOLD-CLASS
              PERFORM SET-CLASS-HEADINGS
              PERFORM SET-BRANCH-HEADINGS
              IF ( GTT-ISO NOT = "O" )
                 IF ( INCL-DETAIL = "Y" )
                    PERFORM HEAD-PAGE.

           IF ( TP-CLASS NOT = HOLD-CLASS )
              PERFORM CLASS-TOTALS
              MOVE TP-CLASS TO HOLD-CLASS
              PERFORM SET-CLASS-HEADINGS
              IF ( GTT-ISO NOT = "O" )
                 IF ( INCL-DETAIL = "Y" )
                    PERFORM HEAD-PAGE.

           MOVE TP-BRNO   TO LN-OWNBR.
           MOVE TP-ACCTNO TO LN-ACCTNO.
           PERFORM READ-LN1-FILE.
           IF IO-FG NOT = 0
              MOVE SPACES TO LN-ACTIONCD2 LN-PLREASON
              MOVE 0 TO LN-LOANDATE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    IF  GTT-ISO = "O", ALLOW TO GO INTO DETAIL-LINE-PROCESS.
      *    SETTING GTT-ISO TO "O" AUTOMATICALLY SETS INCL-DETAIL TO "N".
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
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
                 MOVE "Y" TO FIRST-TIME
                 MOVE 0 TO PAGE-NUMBER
              ELSE
                 MOVE 0 TO PAGE-NUMBER.

           GO TO NEXT-GROUP-LOC.

      *=================================================================
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
              ELSE
                 PERFORM PRINT-RANGE-LINES.

           GO TO END-PROGRAM.

      ******************************************************************
       INITIALIZATION SECTION.
      ******************************************************************
           MOVE EMPLR4-QUEUE-POS TO Q-POS.
           MOVE EMPLR4-COPIES-POS TO C-POS.
           MOVE EMPLR4-PRU-POS   TO P-POS.
           MOVE SPACES TO Q-BUF C-BUF P-BUF.

       COPY "LIBGB/EOINIT.CPY".

           MOVE SPACES TO RECORDS-FOUND.

           MOVE MKTEMP-DEFAULT TO MKTEMP-BUF.
           PERFORM MKTEMP-CALL.
           MOVE MKTEMP-BUF     TO WK-PATH.
           PERFORM OPEN-WK-FILE-OUTPUT.
           PERFORM CLOSE-WK-FILE.
           PERFORM OPEN-WK-FILE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      * CREATE INDEXED KEY FILE FOR TRFILE OUTPUT
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE MKTEMP-DEFAULT TO MKTEMP-BUF.
           PERFORM MKTEMP-CALL.
           IF ( STAT NOT = "00" )
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

           IF ( LOCAL-PRINT-FG NOT = "Y" )
              PERFORM OPEN-PR-FILE.

           PERFORM OPEN-BR-FILE.
           PERFORM OPEN-GR1-FILE.
           PERFORM OPEN-SP1-FILE.
           PERFORM OPEN-LC1-FILE.

           MOVE ZEROS TO TOTAL-TBL(1) TOTAL-TBL(2) TOTAL-TBL(3).
           MOVE 0 TO PAGE-NUMBER.

           PERFORM OPEN-WK2-FILE.
           MOVE MKTEMP-DEFAULT TO MKTEMP-BUF.
           PERFORM MKTEMP-CALL.
           PERFORM OPEN-WK-FILE-OUTPUT.
           PERFORM CLOSE-WK-FILE.
           PERFORM OPEN-WK-FILE.

           MOVE EXT-CONAME-CONAME TO H-NAME.
           MOVE EXT-CONAME-SYSDATE TO H-DATE.
           MOVE EXT-CONAME-USER-ID TO H-USERID.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
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
           MOVE EXT-EOBUF-DATE2 TO SYS-DATE.
           MOVE S-MM TO END-MONTH.
           MOVE S-YY TO END-YEAR.
           IF EXT-EOBUF-DATE1 NOT = EXT-EOBUF-DATE2
              MOVE 99 TO DAY-OF-MONTH
           ELSE
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      * IF USING 12/29/93 TO 12/29/93 FOR A DAILY BATCH, MUST SET
      * DAY-OF-MONTH TO S-DD
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
              MOVE S-DD TO DAY-OF-MONTH.

      ******************************************************************
       ENTER-PARAMETERS SECTION.
      ******************************************************************
       ENTER-PARM.

           IF ( VDUSTATUS = "1U" )
              IF ( GTT-ISO = "O" )
                 GO TO GTT-ISO-PARAGRAPH
              ELSE
                 GO TO ENTER-PL-TYPE.

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

      *----------------------------------------------------------------
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

      *----------------------------------------------------------------
       ALL-BR.

           MOVE "SNNN040BR1." TO VDU.
           MOVE 0 TO BEG-BRANCH VDUNUM0.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "SNNN040BR2." TO VDU.
           MOVE 9999 TO END-BRANCH VDUNUM0.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "S  A160DISPLAY-USER." TO VDU.
           MOVE SPACES TO VDUBUF.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "N" TO GROUP-FLAG.

      *-----------------------------------------------------------------
       CHECK-BRANCH-SECURITY.

           PERFORM BR-RANGE-SECURITY.

           IF BR-SECURITY-OKAY = "N"
              GO TO BR-01.

      *----------------------------------------------------------------
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

      *----------------------------------------------------------------
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

      *----------------------------------------------------------------
       ALL-CL.

           MOVE "SNNN020CL1." TO VDU.
           MOVE 0 TO BEG-CLASS VDUNUM0.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "SNNN020CL2." TO VDU.
           MOVE 99 TO END-CLASS VDUNUM0.
           PERFORM SCREENIO.

      *----------------------------------------------------------------
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

      *----------------------------------------------------------------
       DA-01.

           MOVE LEGEND-DA-01 TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "ENNN020DA1." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.

           IF ( VDUSTATUS = "16" )
              MOVE "S  A020DA1." TO VDU
              MOVE "**" TO VDUBUF
              PERFORM SCREENIO
              MOVE 99 TO DAY-OF-MONTH
              GO TO GTT-ISO-PARAGRAPH.

           IF ( VDUSTATUS = "1U" )
              IF ( EXT-ROUTE-BUF = "00" )
                 GO TO DTE
              ELSE
                 GO TO CL-02.

           IF ( VDUSTATUS = "1>" ) GO TO EXIT-PARM.
           IF ( VDUSTATUS NOT = "00" AND NOT = "1D" ) GO TO DTE.
           IF ( VDUNUM0 = 0 ) GO TO DA-01.
           MOVE VDUNUM0 TO DAY-OF-MONTH.

           IF DAY-OF-MONTH > 31  OR
              (DAY-OF-MONTH > 30 AND
              (END-MONTH = 2 OR 4 OR 6 OR 9 OR 11)) OR
              (DAY-OF-MONTH > 29 AND END-MONTH = 2)
              MOVE "DAY IS PAST LAST DAY OF MONTH" TO MESS
              PERFORM SEND-MESS
              GO TO DA-01.

      *-----------------------------------------------------------------
       GTT-ISO-PARAGRAPH.

           IF ( GROUP-FLAG NOT = "Y" )
              MOVE SPACES TO TOTAL-LEV-TABLE
              MOVE "S  A010GTT-ISO." TO VDU
              MOVE "S" TO VDUBUF GTT-ISO
              PERFORM SCREENIO
              GO TO INCLDETAIL.

           MOVE LEGEND-GTT-01 TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "E  A010GTT-ISO." TO VDU.
           PERFORM SCREENIO.

           IF ( VDUSTATUS = "10" ) GO TO EXIT-PARM.
           IF ( VDUSTATUS = "1>" ) GO TO EXIT-PARM.
           IF ( VDUSTATUS = "1U" ) GO TO DA-01.
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
       INCLDETAIL.

           IF ( GTT-ISO = "O" )
              MOVE "S  A010INC-DTL." TO VDU
              MOVE "N" TO INCL-DETAIL VDUBUF
              PERFORM SCREENIO
              GO TO ENTER-REVERSALS-ONLY.

           MOVE LEGEND-INCLDETAIL TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "E  A010INC-DTL." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.

           IF ( VDUSTATUS = "1U" )
              IF ( GROUP-FLAG = "Y" )
                 GO TO GTT-ISO-PARAGRAPH.

           IF ( VDUSTATUS = "1U" )
              IF ( EXT-ROUTE-BUF = "00" )
                 IF ( DAY-OF-MONTH = 99 )
                    MOVE "SNNN020DA1." TO VDU
                    MOVE 99 TO VDUNUM0
                    PERFORM SCREENIO
                    GO TO DTE.

           IF ( VDUSTATUS = "1U" )
              IF ( EXT-ROUTE-BUF NOT = "00" )
                 GO TO CL-02
              ELSE
                 GO TO DTE.

           IF NOT (VDUSTATUS = "00" OR "1D") GO TO INCLDETAIL.
           MOVE VDUBUF TO INCL-DETAIL.

           IF NOT (INCL-DETAIL = "Y" OR "N")
              GO TO INCLDETAIL.

      *-----------------------------------------------------------------
       ENTER-REVERSALS-ONLY.

           MOVE SPACES TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "E  A010REV-ONLY." TO VDU.
           PERFORM SCREENIO.

           IF (                    UP-KEY ) GO TO INCLDETAIL.
           IF (                    F1-KEY ) GO TO EXIT-PARM.
           IF (                    F7-KEY ) GO TO EXIT-PARM.
           IF NOT ( DOWN-KEY OR ENTER-KEY ) GO TO ENTER-REVERSALS-ONLY.

           IF NOT ( VDUBUF = "Y" OR "N" )
              GO TO ENTER-REVERSALS-ONLY
           ELSE
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


       ENTER-PL-TYPE.

           MOVE SPACES TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "E  A010PLTYPE." TO VDU.
           PERFORM SCREENIO.

           IF (                    UP-KEY ) GO TO ENTER-ACTIONCD2.
           IF (                    F1-KEY ) GO TO EXIT-PARM.
           IF (                    F7-KEY ) GO TO EXIT-PARM.
           IF NOT ( DOWN-KEY OR ENTER-KEY ) GO TO ENTER-PL-TYPE.

           MOVE VDUBUF TO PL-TYPE.

           IF NOT (PL-TYPE = "A" OR "P" OR "2" OR "3" OR "4" OR "5")
              GO TO ENTER-PL-TYPE.

       ENTER-SIGNATURE.

           MOVE SPACES TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "E  A010SIGN." TO VDU.
           PERFORM SCREENIO.

           IF (                    UP-KEY ) GO TO ENTER-PL-TYPE.
           IF (                    F1-KEY ) GO TO EXIT-PARM.
           IF (                    F7-KEY ) GO TO EXIT-PARM.
           IF NOT ( DOWN-KEY OR ENTER-KEY ) GO TO ENTER-SIGNATURE.

           MOVE VDUBUF TO SIGNATURE-LINE.

           IF NOT (SIGNATURE-LINE = "Y" OR "N")
              GO TO ENTER-SIGNATURE.

      *----------------------------------------------------------------
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

           IF ( VDUSTATUS = "1U" )
              IF ( GROUP-FLAG = "Y" )
                 IF ( GTT-ISO = "O" )
                    GO TO GTT-ISO-PARAGRAPH
                 ELSE
                    GO TO ENTER-SIGNATURE
              ELSE
                 GO TO ENTER-SIGNATURE.

           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO ASK-LOCAL.

           IF ( VDUBUF NOT = "Y" )
              IF ( VDUBUF NOT = "N" )
                 GO TO ASK-LOCAL.

           MOVE VDUBUF TO LOCAL-PRINT-FG.

           IF ( GTT-ISO NOT = "S" )
              IF ( LOCAL-PRINT-FG = "Y" )
                 MOVE MESSAGE-01-LOC-PRT TO MESS
                 PERFORM SEND-MESS
                 GO TO ASK-LOCAL.

      *=================================================================
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
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( (BEG-BRANCH = 0) AND (END-BRANCH = 0) )
              MOVE "S  A160DISPLAY-USER." TO VDU
              MOVE "USER BRANCH ONLY" TO VDUBUF
              PERFORM SCREENIO
           ELSE
              MOVE "S  A160DISPLAY-USER." TO VDU
              MOVE SPACES TO VDUBUF
              PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "SNNN020CL1." TO VDU.
           MOVE BEG-CLASS TO VDUNUM0.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "SNNN020CL2." TO VDU.
           MOVE END-CLASS TO VDUNUM0.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "SNNN020DA1." TO VDU.
           MOVE DAY-OF-MONTH TO VDUNUM0.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "S  A010INC-DTL." TO VDU.
           MOVE INCL-DETAIL TO VDUBUF.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "S  A010REV-ONLY." TO VDU.
           MOVE REVERSALS-ONLY     TO VDUBUF.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "S  A040ACTION." TO VDU.
           MOVE ACTION-CODE2 TO VDUBUF.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "S  A010PLTYPE." TO VDU.
           MOVE PL-TYPE     TO VDUBUF.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "S  A010SIGN." TO VDU.
           MOVE SIGNATURE-LINE TO VDUBUF.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "S  A010GTT-ISO." TO VDU.
           MOVE GTT-ISO           TO VDUBUF.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "S  A170SPOOL-PROMPT." TO VDU.
           MOVE "SPOOL TO LOCAL  :"    TO VDUBUF.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "S  A050YES-NO." TO VDU.
           MOVE "(Y/N)"          TO VDUBUF.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "S  A010LOCAL-SPOOL." TO VDU.
           MOVE LOCAL-PRINT-FG        TO VDUBUF.
           PERFORM SCREENIO.

      ******************************************************************
       SORT-ROUTINE SECTION.
      ******************************************************************
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

           IF TP-CLASS < BEG-CLASS OR TP-CLASS > END-CLASS
              GO TO SORT-TR-NEXT.

           IF NOT (TP-TRCD = "PL" OR "P2" OR "P3" OR "P4" OR "P5"
                              OR "PX")
              GO TO SORT-TR-NEXT.

           IF TP-ACTIONCD2 NOT = ACTION-CODE2
              GO TO SORT-TR-NEXT.

           IF REVERSALS-ONLY = "Y"
              IF TP-REV NOT = "Y"
                 GO TO SORT-TR-NEXT.

           IF REVERSALS-ONLY = "Y"
              PERFORM CHECK-REVERSED-SAME-MONTH
              IF SAME-MONTH-FG = "Y"
                 GO TO SORT-TR-NEXT.

           IF PL-TYPE = SPACES
              MOVE "A" TO PL-TYPE.

           IF PL-TYPE NOT = "A"
              IF PL-TYPE NOT = "P"
      * 2 - 5
                 IF PL-TYPE NOT = TP-TRCD2
                    GO TO SORT-TR-NEXT
      * P (PL)
                 ELSE
                    NEXT SENTENCE
              ELSE
                 IF TP-TRCD2 NOT = "L"
                    GO TO SORT-TR-NEXT.

           MOVE TP-DATE   TO WK2-DATE.
           MOVE TP-BRNO   TO WK2-BRANCH.
      *    MOVE TP-CODE   TO WK2-CODE.
           MOVE TP-CLASS  TO WK2-CLASS.
           MOVE TP-ACCTNO TO WK2-ACCTNO.
           MOVE TP-LINE   TO WK2-LINE.

           PERFORM WRITE-WK2-FILE.
           IF IO-FG NOT = 0
              GO TO IO-ERROR.

           GO TO SORT-TR-NEXT.

       EXIT-SORT-ROUTINE.
           PERFORM CLOSE-WK2-FILE.

      ******************************************************************
       DETAIL-LINE-PROCESS SECTION.
      ******************************************************************

           IF ( FIRST-TIME = "Y" )
              MOVE EXT-CONAME-RPT-LINES TO LN-COUNT
              MOVE 1 TO LN-FEED
              MOVE "Y" TO RECORDS-FOUND
              MOVE "N" TO FIRST-TIME.

           PERFORM GET-DISPLAY-LNNO.
           MOVE LPS-DISPLAY-LNNO TO D-DISPLAY-LNNO.

           PERFORM FIND-BORROWER.
           MOVE TP-MAKER    TO D-MAKER.
           MOVE 0           TO D-SSNO.
           MOVE LN-PLREASON TO D-REASON.
           MOVE LN-LOANDATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO D-LOANDATE.
           MOVE LN-DLNO     TO D-DLNO.

           MOVE LN-ACTIONCD2 TO D-ACTIONCD2.

           IF HOLD-TP-ACCTNO = TP-ACCTNO AND
               HOLD-TP-OTHBR-FG = TP-OTHBR-FG AND
                HOLD-TP-SSNO = TP-SSNO
              MOVE SPACES TO DETAIL-LINE
            ELSE
              MOVE 1 TO LN-FEED
              MOVE TP-ACCTNO TO HOLD-TP-ACCTNO
              MOVE TP-OTHBR-FG TO HOLD-TP-OTHBR-FG
              MOVE TP-SSNO TO HOLD-TP-SSNO
              IF INCL-DETAIL = "Y"
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

           MOVE TP-TRCD TO D-TR-CODE.
           MOVE TP-PAYDATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO D-DATE.
           MOVE TP-TRAMT TO D-TRANS-AMT.
           MOVE 0 TO HOLD-NET-LOSS
                     TRUE-NET-LOSS.

           PERFORM VARYING SUB1 FROM 1 BY 1 UNTIL SUB1 > 5
              MOVE TP-WORKAMT(SUB1) TO D-CHARGES(SUB1)
              IF SUB1 NOT = 4
                 ADD TP-WORKAMT(SUB1) TO HOLD-NET-LOSS
              END-IF
              IF NOT ( GP-INCL-OTH-IN-EARNED = "R") AND SUB1 = 4
                 ADD TP-WORKAMT(4) TO HOLD-NET-LOSS
              END-IF
           END-PERFORM.

           IF TP-REV = "Y"
              MOVE "*" TO D-REV
              COMPUTE TP-OT2BAL = TP-OT2BAL * -1.
           SUBTRACT HOLD-NET-LOSS FROM TP-TRAMT GIVING TRUE-NET-LOSS.


           IF TP-TRCD = "P4" OR TP-TRCD = "P5"
              MOVE TP-REFNO TO D-REFNO
              MOVE TRUE-NET-LOSS TO D-RES-AMT
           ELSE
              MOVE TRUE-NET-LOSS TO D-NET-LOSS.

           MOVE TP-OT2BAL TO D-DLNO.

           IF INCL-DETAIL = "Y"
              PERFORM WRITE-DETAIL-LINE
              IF TP-WORKAMT(6) NOT = ZERO
                 MOVE TP-WORKAMT(6) TO D-CHARGES(5)
                 PERFORM WRITE-DETAIL-LINE
              END-IF
           ELSE
              MOVE SPACES TO DETAIL-LINE.

       EXIT-DETAIL-LINE-PROCESS.
           EXIT.

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
           MOVE 0 TO WK-TRAMT WK-UNEARN-INT WK-ADDON-CHG
                     WK-SRVS-CHG WK-MAINT-FEE
                     WK-DLR-DISC WK-NET-LOSS WK-RES-AMT
                     WK-OTHER WK-OT2BAL.

           PERFORM WRITE-WK-FILE.

           IF IO-FG NOT = 0
              GO TO IO-ERROR.

           GO TO CREATE-WK-REC.

      *-----------------------------------------------------------------
       CREATE-WK-UPDATE.

           IF TP-TRAMT NOT < 0
              ADD 1 TO WK-NO
           ELSE
              SUBTRACT 1 FROM WK-NO.

           IF TP-OTHBR-FG = " " OR "C"
             IF TP-TRCD = "PL" OR "P2" OR "P3" OR "P4" OR "P5" OR "PX"
                 ADD TP-TRAMT TO T-TRAMT(LEV,7)
                 ADD TP-TRAMT TO WK-TRAMT.

           IF TP-OTHBR-FG = " " OR "G"
                 ADD TP-WORKAMT(1) TO T-UNEARN-INT(LEV,7)
                 ADD TP-WORKAMT(2) TO T-ADDON-CHG(LEV,7)
                 ADD TP-WORKAMT(3) TO T-SRVS-CHG(LEV,7)
                 ADD TP-WORKAMT(4) TO T-MAINT-FEE(LEV,7)
                 ADD TP-WORKAMT(5) TO T-DLR-DISC(LEV,7)
                 ADD TP-WORKAMT(6) TO T-OTHER(LEV,7)
                 ADD TP-OT2BAL TO T-OT2BAL(LEV,7)
                 IF TP-TRCD = "P4" OR TP-TRCD = "P5"
                    ADD TRUE-NET-LOSS TO T-RES-AMT(LEV,7)
                 ELSE
                   ADD TRUE-NET-LOSS TO T-NET-LOSS(LEV,7)
                 END-IF
              ADD TP-WORKAMT(1) TO WK-UNEARN-INT
              ADD TP-WORKAMT(2) TO WK-ADDON-CHG
              ADD TP-WORKAMT(3) TO WK-SRVS-CHG
              ADD TP-WORKAMT(4) TO WK-MAINT-FEE
              ADD TP-WORKAMT(5) TO WK-DLR-DISC
              ADD TP-WORKAMT(6) TO WK-OTHER
              ADD TP-OT2BAL TO WK-OT2BAL
              IF TP-TRCD = "P4" OR TP-TRCD = "P5"
                 ADD TRUE-NET-LOSS TO WK-RES-AMT
              ELSE
                ADD TRUE-NET-LOSS TO WK-NET-LOSS.

           PERFORM REWRITE-WK-FILE.

       EXIT-CREATE-WK.
           MOVE 0 TO SUB
           PERFORM ADD-TO-LEV-PL.

      ******************************************************************
       ADD-TO-LEV-PL SECTION.
      ******************************************************************

           MOVE 0 TO LEV-PL.

           IF (TP-TRCD = "PL")
               MOVE 1 TO LEV-PL
           ELSE
           IF (TP-TRCD = "P2")
               MOVE 2 TO LEV-PL
           ELSE
           IF (TP-TRCD = "P3")
               MOVE 3 TO LEV-PL
           ELSE
           IF (TP-TRCD = "P4")
               MOVE 4 TO LEV-PL
           ELSE
           IF (TP-TRCD = "P5")
               MOVE 5 TO LEV-PL
           ELSE
           IF (TP-TRCD = "PX")
               MOVE 6 TO LEV-PL
           ELSE
               GO TO EXIT-ADD-TO-LEV-PL.

           IF (TP-OTHBR-FG = " " OR "C")
               ADD TP-TRAMT TO WK-TRAMT T-TRAMT(LEV,LEV-PL).

           IF TP-OTHBR-FG = " " OR "G"
              ADD TP-WORKAMT(1) TO WK-UNEARN-INT
                                   T-UNEARN-INT(LEV,LEV-PL)
              ADD TP-WORKAMT(2) TO WK-ADDON-CHG T-ADDON-CHG(LEV,LEV-PL)
              ADD TP-WORKAMT(3) TO WK-SRVS-CHG T-SRVS-CHG(LEV,LEV-PL)
              ADD TP-WORKAMT(4) TO WK-MAINT-FEE T-MAINT-FEE(LEV,LEV-PL)
              ADD TP-WORKAMT(5) TO WK-DLR-DISC T-DLR-DISC(LEV,LEV-PL)
              ADD TP-WORKAMT(6) TO WK-OTHER T-OTHER(LEV,LEV-PL)
              ADD TP-OT2BAL      TO WK-OT2BAL T-OT2BAL(LEV,LEV-PL)
              IF TP-TRCD = "P4" OR TP-TRCD = "P5"
                 ADD TRUE-NET-LOSS TO WK-RES-AMT T-RES-AMT(LEV,LEV-PL)
              ELSE
                ADD TRUE-NET-LOSS TO WK-NET-LOSS T-NET-LOSS(LEV,LEV-PL).

       EXIT-ADD-TO-LEV-PL.
           EXIT.

      ******************************************************************
       FIND-BORROWER SECTION.
      ******************************************************************

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

      *-----------------------------------------------------------------
       END-FIND-BORROWER.
           EXIT.

      ******************************************************************
      *    CHECK WHETHER P&L POSTED & REVERSED IN SAME MONTH (WORLD)
      ******************************************************************
       CHECK-REVERSED-SAME-MONTH SECTION.
           PERFORM OPEN-LP1-FILE.
           MOVE "N" TO SAME-MONTH-FG.

           MOVE TP-BRNO       TO LP-BRNO
                                 QLP1-WBEG-BRNO
                                 QLP1-WEND-BRNO.
           MOVE TP-ACCTNO     TO LP-ACCTNO
                                 QLP1-WBEG-ACCTNO
                                 QLP1-WEND-ACCTNO.
           MOVE ALL "9"       TO LP-SEQNO
                                 QLP1-WEND-SEQNO.
           MOVE ALL "0"       TO QLP1-WBEG-SEQNO.

           PERFORM START-LP1-FILE-NOT-GREATER.
       GET-NEXT-PAYMENT.
           PERFORM READ-LP1-FILE-PREVIOUS.
           IF IO-FG NOT = 0 OR
               LP-BRNO NOT = TP-BRNO OR
                LP-ACCTNO NOT = TP-ACCTNO
              GO TO END-CHECK-REVERSED.

           IF NOT (LP-TRCD = "PL" OR "P2" OR "P3" OR "P4" OR "P5"
                              OR "PX")
              GO TO GET-NEXT-PAYMENT.

      * IF PAYMENT DATE IS GREATER THAN TRANSACTION PAYMENT DATE, READ
      * PREVIOUS AGAIN TO SEE IF THERE IS ONE IN THE SAME MONTH
           IF LP-TRDATE > TP-DATE
              GO TO GET-NEXT-PAYMENT.

           MOVE TP-DATE   TO DATE-YYYYMMDD.
           MOVE LP-TRDATE TO NUM-DATE.

           IF (DATE-YYYYMMDD-MM = NUM-MO) AND
               (DATE-YYYYMMDD-YY = NUM-YR)
                 MOVE "Y" TO SAME-MONTH-FG.

       END-CHECK-REVERSED.
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
       SET-CLASS-HEADINGS SECTION.
      ******************************************************************

           MOVE HOLD-CLASS TO H-LCNO LC-CODE.
           PERFORM READ-LC1-FILE.
           IF IO-FG NOT = 0
              MOVE "INVALID" TO LC-DESC.

           MOVE LC-DESC TO H-LCDESC.
           MOVE "N" TO SUMMARY-FG.

      ******************************************************************
       CLASS-TOTALS SECTION.
      ******************************************************************

           IF ( GTT-ISO = "O" )
              GO TO END-CLASS-TOTALS.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "Y" TO SUMMARY-FG.
           MOVE EXT-CONAME-RPT-LINES TO LN-COUNT.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           INSPECT SUM-HEAD REPLACING FIRST "&&&&&&" BY SPACES.
           MOVE SUM-HEAD TO PR-REC.
           PERFORM WRITE-PR-REC.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE SUM-HEAD2 TO PR-REC.
           PERFORM WRITE-PR-REC.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE 2 TO LN-FEED.
           MOVE SPACES TO WK-KEY.
           MOVE "L" TO WK-CODE.
           MOVE HOLD-BRNO TO WK-BRNO.
           MOVE HOLD-CLASS TO WK-CLASS.
           PERFORM START-WK-FILE.

      *-----------------------------------------------------------------
       PRINT-NEXT-CODE.
           PERFORM READ-WK-FILE-NEXT.
           IF IO-FG = 9 OR WK-CODE NOT = "L" OR
              WK-CLASS NOT = HOLD-CLASS OR WK-BRNO NOT = HOLD-BRNO
                 PERFORM TYPE-TOTALS
                 GO TO END-CLASS-TOTALS.

           MOVE WK-CLASS TO DS-CLASS LC-CODE.
           PERFORM READ-LC1-FILE.
           IF IO-FG NOT = 0
              MOVE "INVALID CLASS" TO LC-DESC.

           MOVE LC-DESC TO DS-DESC.
           MOVE WK-TRCD TO DS-TRCD.
           MOVE WK-NO TO DS-NO.
           MOVE WK-TRAMT TO DS-TRAMT.
           MOVE WK-UNEARN-INT TO DS-UNEARN-INT.
           MOVE WK-ADDON-CHG TO DS-ADDON-CHG.
           MOVE WK-SRVS-CHG TO DS-SRVS-CHG.
           MOVE WK-MAINT-FEE TO DS-MAINT-FEE.
           MOVE WK-DLR-DISC TO DS-DLR-DISC.
           MOVE WK-NET-LOSS TO DS-NET-LOSS.
           MOVE WK-RES-AMT TO DS-RES-AMT.
           PERFORM WRITE-DETAIL-LINE.
           MOVE 1 TO LN-FEED.

           IF (WK-OTHER NOT = ZERO) OR
               (WK-OT2BAL NOT = ZERO)
              MOVE WK-OTHER TO DS-DLR-DISC
              MOVE WK-OT2BAL TO DS-RES-AMT
              PERFORM WRITE-DETAIL-LINE
              MOVE 1 TO LN-FEED.

           GO TO PRINT-NEXT-CODE.

      *-----------------------------------------------------------------
       END-CLASS-TOTALS.

           MOVE 2 TO LEV.
           MOVE 1 TO SUB.
           PERFORM UPDATE-TABLE2
                 VARYING LEV-PL FROM 1 BY 1
                    UNTIL LEV-PL > 6.

           PERFORM ADD-TOTALS
              VARYING LEV-PL FROM 1 BY 1
                UNTIL LEV-PL > 6.

           MOVE ZEROS TO TOTAL-TBL(1).
           MOVE "N" TO SUMMARY-FG.

      ******************************************************************
       BRANCH-TOTALS SECTION.
      ******************************************************************
           IF ( GTT-ISO = "O" )
              PERFORM ADD-TOTALS
                      VARYING LEV-PL FROM 1 BY 1
                      UNTIL LEV-PL > 6
              GO TO END-BRANCH-TOTALS.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "Y" TO SUMMARY-FG.

           MOVE 0 TO HOLD-CLASS.
           MOVE 2 TO LEV.
           MOVE 2 TO LN-FEED.
           PERFORM PRINT-LEVEL-PL
              VARYING LEV-PL FROM 1 BY 1
                  UNTIL LEV-PL > 6.

           PERFORM ADD-TOTALS
               VARYING LEV-PL FROM 1 BY 1
                    UNTIL LEV-PL > 6.

           MOVE "BRANCH TOTALS:" TO DETAIL-LINE.
           PERFORM PRINT-SUMMARY.

      *-----------------------------------------------------------------
       END-BRANCH-TOTALS.

           MOVE 3 TO LEV.
           MOVE 2 TO SUB.
           PERFORM UPDATE-TABLE2
                VARYING LEV-PL FROM 1 BY 1
                    UNTIL LEV-PL > 6.

           PERFORM ADD-TOTALS
                VARYING LEV-PL FROM 1 BY 1
                    UNTIL LEV-PL > 6.

           MOVE ZEROS TO TOTAL-TBL(2).
           MOVE "N" TO SUMMARY-FG.

      ******************************************************************
       GRAND-TOTALS SECTION.
      ******************************************************************

           IF ( GTT-ISO = "O" )
              GO TO END-GRAND-TOTALS.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "Y" TO SUMMARY-FG.

           MOVE 3 TO LEV.
           MOVE 2 TO LN-FEED.
           PERFORM PRINT-LEVEL-PL
              VARYING LEV-PL FROM 1 BY 1
                 UNTIL LEV-PL > 6.

           MOVE "GRAND TOTALS :" TO DETAIL-LINE.
           PERFORM PRINT-SUMMARY.

      *-----------------------------------------------------------------
       END-GRAND-TOTALS.

           MOVE ZEROS TO TOTAL-TBL(3).
           MOVE "Y" TO SUMMARY-FG.


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
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE HEAD2 TO PR-REC.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-PR-REC.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE HEAD3 TO PR-REC.
           PERFORM WRITE-PR-REC.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE HEAD4 TO PR-REC.
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
           IF ( SUMMARY-FG = "N" )
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
           MOVE 3 TO LN-FEED.

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
           MOVE "GROUP TOTALS" TO RANGE-LINE.
           MOVE GTT-ISO        TO R-CHAR.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "INCLUDE DETAIL" TO RANGE-LINE.
           MOVE INCL-DETAIL      TO R-CHAR.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "REVERSALS ONLY" TO RANGE-LINE.
           MOVE REVERSALS-ONLY   TO R-CHAR.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "ACTION CODE2" TO RANGE-LINE.
           MOVE ACTION-CODE2   TO R-CHAR.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "P&L TYPE" TO RANGE-LINE.
           MOVE PL-TYPE   TO R-CHAR.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "SIGNATURE LINE" TO RANGE-LINE.
           MOVE SIGNATURE-LINE   TO R-CHAR.
           PERFORM WRITE-DETAIL-LINE.

           MOVE 2 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           IF SIGNATURE-LINE = "Y"
              PERFORM PRINT-SIGNATURE-LINES.

       EXIT-PRINT-RANGE-LINES.
           EXIT.

      ************************************************
       PRINT-SIGNATURE-LINES SECTION.
      ************************************************
      * START A NEW PAGE
           MOVE SPACES TO DETAIL-LINE.
           MOVE 99 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE EMPLR4-SIGNATURE-LINE1 TO DETAIL-LINE.
           MOVE 5 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE EMPLR4-SIGNATURE-LINE2 TO DETAIL-LINE.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE EMPLR4-SIGNATURE-LINE3 TO DETAIL-LINE.
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE EMPLR4-SIGNATURE-LINE4 TO DETAIL-LINE.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE 2 TO LN-FEED.
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
           ADD WK-NO          TO  WS-GTT-NO        ( GTT-TOTAL-SUB ).
           ADD WK-TRAMT       TO  WS-GTT-TRAMT     ( GTT-TOTAL-SUB ).
           ADD WK-UNEARN-INT  TO  WS-GTT-UNEARN-INT( GTT-TOTAL-SUB ).
           ADD WK-ADDON-CHG   TO  WS-GTT-ADDON-CHG ( GTT-TOTAL-SUB ).
           ADD WK-SRVS-CHG    TO  WS-GTT-SRVS-CHG  ( GTT-TOTAL-SUB ).
           ADD WK-MAINT-FEE   TO  WS-GTT-MAINT-FEE ( GTT-TOTAL-SUB ).
           ADD WK-DLR-DISC    TO  WS-GTT-DLR-DISC  ( GTT-TOTAL-SUB ).
           ADD WK-OTHER       TO  WS-GTT-OTHER     ( GTT-TOTAL-SUB ).
           ADD WK-NET-LOSS    TO  WS-GTT-NET-LOSS  ( GTT-TOTAL-SUB ).
           ADD WK-RES-AMT     TO  WS-GTT-RES-AMT   ( GTT-TOTAL-SUB ).
           ADD WK-OT2BAL       TO  WS-GTT-OT2BAL     ( GTT-TOTAL-SUB ).

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

       GTT-NEXT-WK.

           PERFORM READ-WK-FILE-NEXT.
           IF ( IO-FG   NOT = 0           ) OR
              ( WK-CODE NOT = "L"         ) OR
              ( WK-BRNO NOT = WGR-SRESULT )
                 GO TO EXIT-GTT-COMPILE-GROUP-TOTALS.

           MOVE "N" TO GTT-FIRST-TIME.

       GTT-OK-TO-PROCESS.

           PERFORM GTT-CHECK-ADD-TOTALS.
           GO TO GTT-NEXT-WK.

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

       GTT-PRT-TOTALS-NEXT.
           MOVE WS-GTT-NO        (GTT-TOTAL-SUB) TO DS-NO.
           MOVE WS-GTT-TRAMT     (GTT-TOTAL-SUB) TO DS-TRAMT.
           MOVE WS-GTT-UNEARN-INT(GTT-TOTAL-SUB) TO DS-UNEARN-INT.
           MOVE WS-GTT-ADDON-CHG (GTT-TOTAL-SUB) TO DS-ADDON-CHG.
           MOVE WS-GTT-SRVS-CHG  (GTT-TOTAL-SUB) TO DS-SRVS-CHG.
           MOVE WS-GTT-MAINT-FEE (GTT-TOTAL-SUB) TO DS-MAINT-FEE.
           MOVE WS-GTT-DLR-DISC  (GTT-TOTAL-SUB) TO DS-DLR-DISC.
           MOVE WS-GTT-NET-LOSS  (GTT-TOTAL-SUB) TO DS-NET-LOSS.
           MOVE WS-GTT-RES-AMT   (GTT-TOTAL-SUB) TO DS-RES-AMT.

      *- - - - - - - - - - - - - - - - - - PRINT DETAIL AFTER LOAD - - -
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           IF (WS-GTT-OTHER(GTT-TOTAL-SUB) NOT = ZERO) OR
               (WS-GTT-OT2BAL(GTT-TOTAL-SUB) NOT = ZERO)
              MOVE WS-GTT-OTHER     (GTT-TOTAL-SUB) TO DS-DLR-DISC
              MOVE WS-GTT-OT2BAL     (GTT-TOTAL-SUB) TO DS-RES-AMT
              MOVE 1 TO LN-FEED
              PERFORM WRITE-DETAIL-LINE.

           IF ( WS-HOLD-PROMPT = "GROUP :" )
              MOVE ALL "-" TO DETAIL-LINE
              PERFORM WRITE-DETAIL-LINE.

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

      ******************************************************************
      *
      *    C O P Y - L I B R A R Y - S E C T I O N S
      *
      ******************************************************************
       COPY "LIBGB/TIMEIO.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/GPENV.CPY".
       COPY "LIBGB/GRWORK.CPY".
       COPY "LIBGB/GRSERCH.CPY".
       COPY "LIBGB/GRTOTALS.CPY".
       COPY "LIBGB/BRSECURE.CPY".
       COPY "LIBGB/EOSPOOL.CPY".
       COPY "LIBLP/LPMDTE.CPY".
       COPY "LIBLP/LPSTAT.CPY".
       COPY "LIBWI/GRTOTL.CPY".

      *********************************************************
       FORM-RESET SECTION.
      *********************************************************
           MOVE EXT-CONAME-CONAME TO EMPLR4-CONAME.
           MOVE EXT-CONAME-SYSDATE TO EMPLR4-SYSDATE.
           DISPLAY EMPLR4-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE EMPLR4-FORM-FIELDS TO WR-BUF.
           MOVE EMPLR4-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      *********************************************************
       SETUP-LINK-INFO SECTION.
      *********************************************************
           MOVE EMPLR4-HELP--FILE TO HELP-LINK-FILE.
           MOVE EMPLR4-HELP--DIR  TO HELP-LINK-DIR.

      *********************************************************
       VDU-CONVERT-FIELD SECTION.
      *********************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBLP/EMPLR4_EVA.CPY".
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
       SET-BRANCH-HEADINGS.
           MOVE HOLD-BRNO TO H-BRNO.
           MOVE BR-NAME TO H-BRDESC.
      *-----------------------------------------------------------------
       SEND-LEGEND.
           MOVE LEGEND TO WR-BUF.
           MOVE MSEL-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      *-----------------------------------------------------------------
       SETUP-WK-KEY.
           MOVE SPACES   TO WK-REC.
           MOVE "L"      TO WK-CODE.
           MOVE TP-BRNO  TO WK-BRNO.
           MOVE TP-CLASS TO WK-CLASS.
           MOVE TP-TRCD  TO WK-TRCD.

      *-----------------------------------------------------------------
       UPDATE-TABLE2.
           ADD T-TRAMT(SUB,LEV-PL) TO T-TRAMT(LEV,LEV-PL).
           ADD T-UNEARN-INT(SUB,LEV-PL) TO T-UNEARN-INT(LEV,LEV-PL).
           ADD T-ADDON-CHG(SUB,LEV-PL) TO T-ADDON-CHG(LEV,LEV-PL).
           ADD T-SRVS-CHG(SUB,LEV-PL) TO T-SRVS-CHG(LEV,LEV-PL).
           ADD T-MAINT-FEE(SUB,LEV-PL) TO T-MAINT-FEE(LEV,LEV-PL).
           ADD T-DLR-DISC(SUB,LEV-PL) TO T-DLR-DISC(LEV,LEV-PL).
           ADD T-OTHER(SUB,LEV-PL) TO T-OTHER(LEV,LEV-PL).
           ADD T-NET-LOSS(SUB,LEV-PL) TO T-NET-LOSS(LEV,LEV-PL).
           ADD T-RES-AMT(SUB,LEV-PL) TO T-RES-AMT(LEV,LEV-PL).
           ADD T-OT2BAL(SUB,LEV-PL) TO T-OT2BAL(LEV,LEV-PL).

      *-----------------------------------------------------------------
       TYPE-TOTALS.
           MOVE 1 TO LEV.
           MOVE "CLASS  TOTALS:" TO DETAIL-LINE.
           PERFORM PRINT-SUMMARY.

      *-----------------------------------------------------------------
       PRINT-LEVEL-PL.
           MOVE PL-CODE(LEV-PL)          TO DS-TRCD.
           MOVE T-TRAMT(LEV,LEV-PL)      TO DS-TRAMT.
           MOVE T-UNEARN-INT(LEV,LEV-PL) TO DS-UNEARN-INT.
           MOVE T-ADDON-CHG(LEV,LEV-PL)  TO DS-ADDON-CHG.
           MOVE T-SRVS-CHG(LEV,LEV-PL)   TO DS-SRVS-CHG.
           MOVE T-MAINT-FEE(LEV,LEV-PL)  TO DS-MAINT-FEE.
           MOVE T-DLR-DISC(LEV,LEV-PL)   TO DS-DLR-DISC.
           MOVE T-NET-LOSS(LEV,LEV-PL)   TO DS-NET-LOSS.
           MOVE T-RES-AMT(LEV,LEV-PL)    TO DS-RES-AMT.
           PERFORM WRITE-DETAIL-LINE.

           IF (T-OTHER(LEV,LEV-PL) NOT = ZERO) OR
               (T-OT2BAL(LEV,LEV-PL) NOT = ZERO)
              MOVE T-OTHER(LEV,LEV-PL) TO DS-DLR-DISC
              MOVE T-OT2BAL(LEV,LEV-PL) TO DS-RES-AMT
              PERFORM WRITE-DETAIL-LINE.

      *-----------------------------------------------------------------
       PRINT-SUMMARY.
      *CS
           MOVE 0                   TO DS-NO.
           MOVE T-TRAMT(LEV,7)      TO DS-TRAMT.
           MOVE T-UNEARN-INT(LEV,7) TO DS-UNEARN-INT.
           MOVE T-ADDON-CHG(LEV,7)  TO DS-ADDON-CHG.
           MOVE T-SRVS-CHG(LEV,7)   TO DS-SRVS-CHG.
           MOVE T-MAINT-FEE(LEV,7)  TO DS-MAINT-FEE.
           MOVE T-DLR-DISC(LEV,7)   TO DS-DLR-DISC.
           MOVE T-NET-LOSS(LEV,7)   TO DS-NET-LOSS.
           MOVE T-RES-AMT(LEV,7)    TO DS-RES-AMT.
           IF LEV = 1 
              MOVE 3 TO LN-FEED
           ELSE
              MOVE 2 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           IF (T-OTHER(LEV,7) NOT = ZERO) OR
               (T-OT2BAL(LEV,7) NOT = ZERO)
              MOVE T-OTHER(LEV,7) TO DS-DLR-DISC
              MOVE T-OT2BAL(LEV,7) TO DS-RES-AMT
              MOVE 1 TO LN-FEED
              PERFORM WRITE-DETAIL-LINE.

           IF LEV = 1 OR 2
              MOVE ALL "-" TO DETAIL-LINE
              PERFORM WRITE-DETAIL-LINE.

      *-----------------------------------------------------------------
       ADD-TOTALS.
           ADD T-TRAMT(SUB,LEV-PL) TO T-TRAMT(LEV,7).
           ADD T-UNEARN-INT(SUB,LEV-PL) TO T-UNEARN-INT(LEV,7).
           ADD T-ADDON-CHG(SUB,LEV-PL) TO T-ADDON-CHG(LEV,7).
           ADD T-SRVS-CHG(SUB,LEV-PL) TO T-SRVS-CHG(LEV,7).
           ADD T-MAINT-FEE(SUB,LEV-PL) TO T-SRVS-CHG(LEV,7).
           ADD T-DLR-DISC(SUB,LEV-PL) TO T-DLR-DISC(LEV,7).
           ADD T-OTHER(SUB,LEV-PL) TO T-OTHER(LEV,7).
           ADD T-NET-LOSS(SUB,LEV-PL) TO T-NET-LOSS(LEV,7).
           ADD T-RES-AMT(SUB,LEV-PL) TO T-RES-AMT(LEV,7).
           ADD T-OT2BAL(SUB,LEV-PL) TO T-OT2BAL(LEV,7).

      ******************************************************************
       IO-ROUTINES SECTION.
      ******************************************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBLP/LPTRPGS_SQL.CPY".
       COPY "LIBLP/LPSPGS_SQL.CPY".
       COPY "LIBLP/LPLPGS_SQL.CPY".
       COPY "LIBLP/LPLNGS_SQL.CPY".
       COPY "LIBLP/LPLCGS_SQL.CPY".
       COPY "LIBLP/LPBWGS_SQL.CPY".
       COPY "LIBGB/GBGRGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBPRINT.CPY".
       COPY "LIBLP/LPBW1RN.CPY".
       COPY "LIBLP/LPLN1RN.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBGB/GBGR1RN.CPY".
       COPY "LIBLP/LPTRP1RN.CPY".
       COPY "LIBLP/LPLC1RN.CPY".
       COPY "LIBLP/LPLP1RN.CPY".
       COPY "LIBLP/LPSP1RN.CPY".
       COPY "LIBGB/GBWKIN.CPY".
       COPY "LIBGB/GBWK2IN.CPY".
      *-----------------------------------------------------------------
       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO EXT-EOBUF-ERRCD.

      ******************************************************************
       END-PROGRAM SECTION.
      ******************************************************************
           PERFORM CLOSE-FILES.

           IF EXT-ROUTE-BUF = "00"
              PERFORM PRU-OUT.

           IF NO-RECORDS-FOUND
              MOVE "NO RECORDS FOUND FOR SELECTED RANGE" TO MESS
              PERFORM SEND-MESS.

           MOVE WK-PATH TO ACCESS-BUF.
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

           IF EXT-ROUTE-BUF  = "00"
              MOVE EXIT-PATH TO FORM-NAM
              MOVE FORM-PATH TO FORM-PATHNAME.

       STOP-RUN.
           IF EXT-ROUTE-BUF = "00" OR "40" OR "50"
              DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN
              DESTROY EMPLR4-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.
           EXIT PROGRAM.
