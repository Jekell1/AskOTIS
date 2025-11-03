       IDENTIFICATION DIVISION.
       PROGRAM-ID.   EMCASH.
       DATE-WRITTEN. 080995.
      *****************************************************************
      *                      (LP)
      *    DESCRIPTION:  MONTHLY CASH ONLY TRANSACTION REGISTER
      *
      * REV:
      * MJD ADDED INITIALIZE OF HOLD-TP-TRCD.
      * JFF 030496 ADDED "SUMMARY ONLY" TO FORM & PARAMETER SECTION.
      * MJD 071896 ADDED WEEKLY UNIT PERIOD LOGIC FOR 6 DIGIT PDTH DATES
      * JTG 060497 ADDED TEST FOR "R3" & "C3"
      * GLF 022098 DELETED BR-SECURITY-OK AND ADDED LIBGB/BRSECUREW.
      *  BAH 030698 CHANGED WK2-LINE TO PIC 9(3) FROM 99 BECAUSE TP-LINE
      *             PIC 99 COMP ACTUALLY STORES A 3 DIGIT NUMBER !.
      *  BAH 062598 CHANGED TP-TILL PIC 9 TO TP-TILLNO PIC X
      *  GLF 082798 CHANGED REPORT HEADING FROM "MONTHLY TRANSACTION REGISTER"
      *             TO "MONTHLY CASH TRANSACTION REGISTER".
      *  GLF 043099 WAS MOVING SPACES TO DETAIL-LINE ON SUMMARY ONLY IN
      *             WRONG SPOT CAUSING LINES TO BE MISSING, IS NOW FIXED
      *  BAH 990618 ADDED "ST"
      *  GLF 081999 ADD ARCHIVE PATH INFO. FOR TR FILES NOT FOUND IN DATA
      *             SET FOR SELECTED MONTH AND YEAR
      *  BLV 990826 FIX ARCHIVE PATH LOGIC
      *  KEC 000728 {PR#01273} PARADATA FINANCIAL [JUDY]
      *              REPLACED THE PART IN THE DETAIL-LINE IN DISPLAYING
      *              THE ACCTNO WITH SOME STATUS FLAGS WITH STANDARD
      *              LAYOUT FROM LIBLP/LPSTAT (LPS-DISPLAY-LNNO).
      *
      *  KEC 001002 {PR#00000} MERCURY FINANCE [CINDY]
      *              CHANGED WK2-LINE FROM 9(3) TO 9(2) COMP JUST LIKE
      *              TP-LINE; BETH ORIGINALLY MADE THE CHANGE BUT HAD
      *              GOT CAUGHT IN LP/EMTRRG SO CHANGED TO 9(2) COMP.
      *  BAH 021014 ADDED NEW TRANSACTION CODE OF "IN" INSURANCE LOSS, EXACTLY
      *             LIKE "PE", REGACC PR# 0007
      *  BAH 070703 DO NOT PRINT SSNO FOR WORLD, WORLD PR# 536
      *
      * KEC 2016.1026 {PD#00003} PARADATA <A30> [JAY CISZEWSKI]
      *         CHANGED SAVE-TR1-KEY AND TEST-TR1-KEY TO HAVE A
      *         PIC X(27) FOR QUICKER VERIFICATION.
      *
      * BAH 2016.1103 CHANGE TILLNO TO 9(2)
      * BAH 170526 ACTIVATE 8 DIGIT DATES, PD0006
      * BAH 2019.0524 CLASS TRUNCATED IN CLASS TOTALS, CHANGED DS-CLASS
      *               FROM PIC XX TO PIC 99. QA#0030
      * BAH 20190924 REMOVED ACCESS-CALL TRPFILE
      * BAH 20200827 REMOVED RESET ON FIRST FIELD
      * BLM 20210810 REMOVE TP-CODE, TP-POOLID NO LONGER IN KEY
      ****************************************************************
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
           03  WK-INTDUE    PIC S9(9)V99  COMP-3.
           03  WK-APOTH     PIC S9(9)V99  COMP-3.
           03  WK-APINT     PIC S9(9)V99  COMP-3.
           03  WK-APLC      PIC S9(9)V99  COMP-3.
           03  WK-APCUR    PIC S9(9)V99  COMP-3.
           03  WK-CHECKS    PIC S9(9)V99  COMP-3.
           03  WK-EARNINGS  PIC S9(9)V99  COMP-3.
           03  WK-DLPROC    PIC S9(9)V99  COMP-3.
       01  WK-GL-REC.
           03  WK-GL1-KEY.
               05  FILLER   PIC X.
               05  WK-GLNO  PIC 9(11)     COMP-3.
               05  FILLER   PIC XX.
           03  WK-GLAMT     PIC S9(7)V99  COMP-3.
           03  FILLER       PIC X(53).

      ********************************************
      *  INDEXED TRFILE BY BRANCH, CLASS, ACCOUNT
      ********************************************
       FD  WK2-FILE
           LABEL RECORDS ARE STANDARD.
       01  WK2-REC.
           03  WK2-KEY.
               05  WK2-BRANCH            PIC 9(4).
               05  WK2-CLASS             PIC 9(3).
               05  WK2-ACCTNO            PIC 9(8).
               05  WK2-DATE              PIC 9(8).
      *        05  WK2-CODE              PIC X.
               05  WK2-LINE              PIC 9(3).

       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(43) VALUE
           "@(#)./LP/EMCASH.CBL S34 08/10/21-20:48:08 >".
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
       01  PR-IFN               PIC X(2)     VALUE "PR".
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
       01  HOLD-TP-TRCD         PIC XX        VALUE SPACES.
           88  CASH-POSTING                   VALUE "DF" "D2" "D3" "D4"
                                                    "D5" "D6" "D7" "D8"
                                                    "D9" "PY" "PE" "PO"
                                                    "Z2" "AH" "IN".

       01  SUB                  PIC 9(2)      COMP-3.
       01  LEV                  PIC 99        COMP-3.
       01  HOLD-BRNO            PIC 9(4)  VALUE 0.
       01  HOLD-CLASS           PIC 9(3)  VALUE 0.
       01  RECORDS-FOUND        PIC X     VALUE " ".
           88  NO-RECORDS-FOUND           VALUE "N".
       01  SUMMARY-FG           PIC X     VALUE "N".
       01  NAME-WORKER          PIC X(34).
       01  FIRST-NAME           PIC X(16).

       01  FIRST-TIME           PIC X        VALUE "Y".
       01  MSEL-LIST            PIC X(5)     VALUE "MSEL.".
       01  MESS-LIST            PIC X(5)     VALUE "MESS.".
       01  SAVE-TP-REC-FG       PIC X VALUE SPACES.

       01  TEST-TRP1-KEY         PIC X(32)    VALUE SPACES.
       01  FILLER               REDEFINES TEST-TRP1-KEY.
           03  TEST-TRP1-KEY-MAIN.
               05  TEST-TRP1-BRNO             PIC 9(4).
               05  TEST-TRP1-DATE             PIC 9(8).
               05  TEST-TRP1-CLASS            PIC 9(3).
               05  TEST-TRP1-ACCTNO           PIC 9(8).
           03  TEST-TRP1-LINE                 PIC 9(3).

       01  SAVE-TRP1-KEY         PIC X(32)    VALUE SPACES.
       01  FILLER               REDEFINES SAVE-TRP1-KEY.
           03  SAVE-TRP1-KEY-MAIN.
               05  SAVE-TRP1-DATE             PIC 9(8).
               05  SAVE-TRP1-BRNO             PIC 9(4).
      *        05  SAVE-TRP1-CODE             PIC X.
      *        05  SAVE-TRP1-POOLID           PIC 9(5).
               05  SAVE-TRP1-CLASS            PIC 9(3).
               05  SAVE-TRP1-ACCTNO           PIC 9(8).
           03  SAVE-TRP1-LINE                 PIC 9(3).

      *  TOTALS LEV1 = CLASS TOTALS
      *            2 = BRANCH TOTALS
      *            3 = GRAND TOTALS
       01  TOTALS-TAB   VALUE ZEROS.
           03  TOTAL-TBL       OCCURS 3.
               05  T-TRAMT      PIC S9(9)V99.
               05  T-INTDUE     PIC S9(9)V99.
               05  T-APOTH      PIC S9(9)V99.
               05  T-APINT      PIC S9(9)V99.
               05  T-APLC       PIC S9(9)V99.
               05  T-APCUR     PIC S9(9)V99.
               05  T-CHECKS     PIC S9(9)V99.
               05  T-EARNINGS   PIC S9(9)V99.
               05  T-DLPROC     PIC S9(7)V99.

       01  LEGEND               PIC X(70).
      ***********************************
      *     S C R E E N   F I E L D S   *
      ***********************************
       COPY "LIBGB/EOBUF_EXT.CPY".
           03  BEG-BRANCH      PIC 9(4).
           03  END-BRANCH      PIC 9(4).
           03  BEG-CLASS       PIC 99.
           03  END-CLASS       PIC 99.
           03  END-DATE.
               05  END-MONTH   PIC 99.
               05  END-YEAR    PIC 99.
           03  EDATE REDEFINES END-DATE PIC 9999.
           03  DAY-OF-MONTH    PIC 99.
           03  SUM-ONLY        PIC X.
           03  GROUP-FLAG      PIC X.
           03  ENT-GROUP       PIC X(4).

       01  VERSION-CONTROL     PIC X VALUE "B".
       01  BEGINNING-DATE       PIC 9(8).
       01  ENDING-DATE          PIC 9(8).

       01  SV-BEG-BR           PIC 9999  VALUE 9999.
       01  SV-END-BR           PIC 9999  VALUE 9999.

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
           03  FILLER           PIC X(37)    VALUE " ".
           03  FILLER           PIC X(5)     VALUE "PAGE ".
           03  H-PAGE-NO        PIC ZZZ9.

       01  HEAD2.
           03  FILLER           PIC X(10)    VALUE "USER ID:".
           03  H-USERID         PIC X(10).
           03  FILLER           PIC X(36)    VALUE " ".
           03  FILLER           PIC X(41) VALUE
               "MONTHLY CASH TRANSACTION REGISTER".

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
           "TR          TRANS       - APPLIED -".
           03  FILLER           PIC X(34)    VALUE
           "---- BALANCES -----          G/L".

       01  HEAD6.
           03  FILLER           PIC X(11)      VALUE
           "    TL".
           03  FILLER           PIC X(58)     VALUE
           "       PROC'D   EARNED PC PAY-DATE CD REFNO   AMOUNT".
           03  FILLER           PIC X(45)     VALUE
           "INT     PRIN  PDTH LCPD     OTH      INT".
           03  FILLER           PIC X(18)     VALUE
           "PRIN      ACCT-AMT".

       01  SUM-HEAD.
           03  FILLER           PIC X(55)    VALUE
           "CLASS               TRCD   NO     TR-AMT  NSF CHECKS   ".
           03  FILLER           PIC X(55)    VALUE
           "APP-OTH    APP-INT     APP-LC    APP-PRIN  REB CHECKS  ".
           03  FILLER           PIC X(20)    VALUE
           " EARNINGS    DL-PROC".

       01  GL-HEAD.
           03  FILLER           PIC X(17)    VALUE "G/L DIST  SUMMARY".

       01  DETAIL-LINE          VALUE SPACES.
           03  FILLER           PIC X(118).
           03  D-BRXX           PIC X(12).
           03  D-BRX            PIC 9(4).


       01  D-LINE1 REDEFINES DETAIL-LINE.
           03  D-DISPLAY-LNNO   PIC X(17).
           03  FILLER           PIC XX.
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
           03  FILLER           PIC X(11).
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

       01  D-LINE3 REDEFINES DETAIL-LINE.
           03  FILLER           PIC X(14).
           03  D-DLTAG1         PIC X(10).
           03  D-DLADJ1         PIC ZZZZZ.ZZ-.
           03  FILLER           PIC X(3).
           03  D-DLTAG2         PIC X(10).
           03  D-DLADJ2         PIC ZZZZZ.ZZ-.
           03  FILLER           PIC X(3).
           03  D-DLTAG3         PIC X(10).
           03  D-DLADJ3         PIC ZZZZZ.ZZ-.

       01  D-LINE4 REDEFINES DETAIL-LINE.
           03  DS-CLASS         PIC 99.
           03  FILLER           PIC X.
           03  DS-DESC          PIC X(16).
           03  FILLER           PIC XX.
           03  DS-TRCD          PIC XX.
           03  FILLER           PIC XXX.
           03  DS-NO            PIC ZZZ.
           03  FILLER           PIC X.
           03  DS-TRAMT         PIC ZZZZZZZ.ZZ-.
           03  FILLER           PIC X.
           03  DS-INTDUE        PIC ZZZZZZ.ZZ-.
           03  FILLER           PIC X.
           03  DS-APOTH         PIC ZZZZZZ.ZZ-.
           03  FILLER           PIC X.
           03  DS-APINT         PIC ZZZZZZ.ZZ-.
           03  FILLER           PIC X.
           03  DS-APLC          PIC ZZZZZZ.ZZ-.
           03  FILLER           PIC X.
           03  DS-APCUR        PIC ZZZZZZZ.ZZ-.
           03  FILLER           PIC X.
           03  DS-CHECKS        PIC ZZZZZZZ.ZZ-.
           03  FILLER           PIC X.
           03  DS-EARNINGS      PIC ZZZZZZ.ZZ-.
           03  FILLER           PIC X.
           03  DS-DLPROC        PIC ZZZZZZ.ZZ-.

       01  D-LINE5 REDEFINES DETAIL-LINE.
           03  DS-GLNO          PIC ZZZ9/9999999.
           03  FILLER           PIC XX.
           03  DS-GLAMT         PIC ZZZZ9.99-.

       COPY "LIBLP/EMSFRG_CMD.CPY".
       COPY "LIBWI/GRSCANW.CPY".
       COPY "LIBGB/GRWORKW.CPY".
       COPY "LIBGB/PASSWDW.CPY".
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
       COPY "LIBGB/GETPRW.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBLP/EMCASH_DEF.CPY".
       COPY "LIBLP/EMCASH_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

      ******************************************************************
      *
      *     P R O G R A M   C O N T R O L
      *
      ******************************************************************
       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBLP/EMCASH_SCN.CPY".

      *-----------------------------------------------------------------
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
           CLOSE
               WK-FILE WK2-FILE.
       COPY "LIBGB/DECLRP2.CPY".

      ************************************************
       MAIN-MODULE SECTION.
      ************************************************

           PERFORM SQL-CONNECT.

      D    STOP MESS.
           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "EMCASH" TO FORM-PROG VDU-FORM-NAME.

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

      * TEST IF CASH TRANSACTIONS ARE BEING USED
           IF ((BR-TENDER-CD(1) = SPACES AND BR-TENDER-CD(2) = SPACES
               AND BR-TENDER-CD(3) = SPACES) OR
               (BR-TENDER-CD(1) = "XXXXX" AND BR-TENDER-CD(2) = "XXXXX"
                    AND BR-TENDER-CD(3) = "XXXXX"))
              GO TO NEXT-GROUP-LOC.

       MOVE-BRANCH-PATH-INFO.
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

       NEXT-TR-REPORT.
           PERFORM READ-WK2-FILE-NEXT.
           IF IO-FG = 9
              GO TO END-OF-GROUP.

           MOVE WK2-DATE   TO TP-DATE.
           MOVE WK2-BRANCH TO TP-BRNO.
           MOVE WK2-CLASS  TO TP-CLASS.
           MOVE WK2-ACCTNO TO TP-ACCTNO.
           MOVE WK2-LINE   TO TP-LINE.
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
              PERFORM HEAD-PAGE.
           IF TP-CLASS NOT = HOLD-CLASS
              PERFORM CLASS-TOTALS
              MOVE TP-CLASS TO HOLD-CLASS
              PERFORM SET-CLASS-HEADINGS
              PERFORM HEAD-PAGE.

           PERFORM DETAIL-LINE-PROCESS.

           MOVE 1 TO LEV.
           PERFORM CREATE-WK.

           GO TO NEXT-TR-REPORT.

       END-OF-GROUP.
           PERFORM CLOSE-TRP1-FILE.
           PERFORM CLOSE-BW1-FILE.
           PERFORM CLOSE-LN1-FILE.
           PERFORM CLOSE-WK2-FILE.
           PERFORM OPEN-WK2-FILE-OUTPUT.
           GO TO NEXT-GROUP-LOC.

       END-ROUTINE.
           IF FIRST-TIME = "Y"
              MOVE "N" TO RECORDS-FOUND
              GO TO END-PROGRAM.
           PERFORM CLASS-TOTALS.
           PERFORM BRANCH-TOTALS.
           PERFORM GRAND-TOTALS.
           PERFORM GL-SUMMARY.
           GO TO END-PROGRAM.

      ***********************************
       INITIALIZATION SECTION.
      ***********************************
           MOVE EMCASH-QUEUE-POS TO Q-POS.
           MOVE EMCASH-COPIES-POS TO C-POS.
           MOVE EMCASH-PRU-POS   TO P-POS.
           MOVE SPACES TO Q-BUF C-BUF P-BUF.
       COPY "LIBGB/EOINIT.CPY".

           MOVE ZEROS TO TOTAL-TBL(1) TOTAL-TBL(2) TOTAL-TBL(3).
           MOVE 0 TO PAGE-NUMBER.

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
           PERFORM OPEN-WK-FILE-OUTPUT.
           PERFORM CLOSE-WK-FILE.
           PERFORM OPEN-WK-FILE.

           MOVE EXT-CONAME-CONAME TO H-NAME.
           MOVE EXT-CONAME-SYSDATE TO H-DATE.
           MOVE EXT-CONAME-USER-ID TO H-USERID.

           IF BEG-BRANCH = 0
              IF END-BRANCH = 0
                 MOVE BEG-BRANCH TO SV-BEG-BR
                 MOVE END-BRANCH TO SV-END-BR
                 MOVE EXT-CONAME-USER-LOC TO BEG-BRANCH
                                             END-BRANCH.
           IF EXT-ROUTE-BUF = "00"
              MOVE "TRANSACTION REGISTER IN PROGRESS........." TO WR-BUF
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

           PERFORM OPEN-PR-FILE.
           PERFORM OPEN-BR-FILE.
           PERFORM OPEN-SP1-FILE.
           PERFORM OPEN-LC1-FILE.
           PERFORM OPEN-GR1-FILE.

           GO TO EXIT-INITIALIZE.
       ENDIT.
           MOVE "X" TO EXT-EOBUF-ERRCD.
       EXIT-INITIALIZE.
           EXIT.

      ********************************************
       EO-EXEC-AUDIT SECTION.
      ********************************************
           MOVE EXT-EOBUF-DATE2 TO SYS-DATE.
           MOVE S-MM TO END-MONTH.
           MOVE S-YY TO END-YEAR.
           IF EXT-EOBUF-DATE1 NOT = EXT-EOBUF-DATE2
              MOVE 99 TO DAY-OF-MONTH
           ELSE
              MOVE S-DD TO DAY-OF-MONTH.

      ********************************************
       ENTER-PARAMETERS SECTION.
      ********************************************
       ENTER-PARM.

           IF VDUSTATUS = "1U"
              GO TO DA-01.

      * DEFAULT TO USER BRANCH
           IF EXT-ROUTE-BUF NOT = "00"
              MOVE "SNNN040BR1." TO VDU
              MOVE 0 TO BEG-BRANCH VDUNUM0
              PERFORM SCREENIO
              MOVE "SNNN040BR2." TO VDU
              MOVE 0 TO END-BRANCH VDUNUM0
              PERFORM SCREENIO.

       BR-01.
           MOVE LEGEND-BR-01 TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "E  A000BR1." TO VDU.
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
           MOVE "N" TO GROUP-FLAG.
           MOVE "S  A160DISPLAY-USER." TO VDU.
           MOVE SPACES TO VDUBUF.
           PERFORM SCREENIO.

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
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO CL-02.
           MOVE VDUNUM0 TO END-CLASS.
           IF BEG-CLASS > END-CLASS
              MOVE "INVALID CLASS RANGE" TO MESS
              PERFORM SEND-MESS
              GO TO CL-01.
           GO TO DTE.
       ALL-CL.
           MOVE "SNNN020CL1." TO VDU.
           MOVE 0 TO BEG-CLASS VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN020CL2." TO VDU.
           MOVE 99 TO END-CLASS VDUNUM0.
           PERFORM SCREENIO.
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

       DA-01.
           MOVE "ENNN020DA1." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "16"
              MOVE "S  A020DA1." TO VDU
              MOVE "**" TO VDUBUF
              PERFORM SCREENIO
              MOVE 99 TO DAY-OF-MONTH
              GO TO SUMYN.
           IF VDUSTATUS = "1U"
              IF EXT-ROUTE-BUF NOT = "00"
                 GO TO CL-02
              ELSE
                 GO TO DTE.
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
      *---------------------------------------------------------------
       SUMYN.
           MOVE "E  A010SUMONLY." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U"
              IF EXT-ROUTE-BUF = "00"
                 IF ( DAY-OF-MONTH = 99 )
                    MOVE "SNNN020DA1." TO VDU
                    MOVE 99 TO VDUNUM0
                    PERFORM SCREENIO
                    GO TO DTE.
           IF VDUSTATUS = "1U"
              IF EXT-ROUTE-BUF NOT = "00"
                 GO TO CL-02
              ELSE
                 GO TO DTE.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO SUMYN.
           MOVE VDUBUF TO SUM-ONLY.
           IF SUM-ONLY NOT = "N" AND NOT = "Y" GO TO SUMYN.

       END-PARM.
           MOVE LEGEND-ACCEPT TO LEGEND.
           PERFORM SEND-LEGEND.

       EXIT-PARM.
           EXIT.

      *******************************************************
       DISPLAY-PARAMETERS SECTION.
      *******************************************************

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
           MOVE "S  A010SUMONLY." TO VDU.
           MOVE SUM-ONLY TO VDUBUF.
           PERFORM SCREENIO.

      *******************************************************
       SORT-ROUTINE SECTION.
      *******************************************************

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

           MOVE TP-TRCD TO HOLD-TP-TRCD.
      * ONLY FIND CASH TRANSACTIONS
           IF NOT CASH-POSTING OR
               NOT (TP-REFNO = BR-TENDER-CD(1) OR BR-TENDER-CD(2)
                            OR BR-TENDER-CD(3))
              GO TO SORT-TR-NEXT.

           MOVE TRP1-KEY TO TEST-TRP1-KEY.
           IF TEST-TRP1-KEY-MAIN = SAVE-TRP1-KEY-MAIN
              AND SAVE-TP-REC-FG = "Y"
              GO TO SAVE-THIS-ACCT.

           IF TEST-TRP1-KEY-MAIN NOT = SAVE-TRP1-KEY-MAIN
              MOVE SPACE TO SAVE-TP-REC-FG
              MOVE TRP1-KEY TO SAVE-TRP1-KEY.

       SAVE-THIS-ACCT.
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


      *******************************************************
       DETAIL-LINE-PROCESS SECTION.
      *******************************************************
           IF FIRST-TIME = "Y"
              MOVE EXT-CONAME-RPT-LINES TO LN-COUNT
              MOVE 1 TO LN-FEED
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
              MOVE TP-ACCTNO TO HOLD-TP-ACCTNO
              MOVE TP-OTHBR-FG TO HOLD-TP-OTHBR-FG
              MOVE TP-SSNO TO HOLD-TP-SSNO
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
           MOVE TP-IBPC     TO D-IBPC.
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
           IF TP-TRCD = "WL" OR "WI"
              ADD TP-APINT TP-APLC GIVING D-TRAMT
           ELSE
              MOVE TP-TRAMT TO D-TRAMT
              ADD TP-APINT TP-APLC GIVING D-APINT
              IF ( (TP-TRCD = "RT" OR "ET" OR "LP" OR "NP" OR "ST") OR
                   (TP-TRCD = "RV" AND
                   (TP-ADDON-TEXT = "ADD" OR "ADJ" OR "CNL") ) )
                 PERFORM SETUP-D-TRAMT
                 MOVE TP-APINT TO D-APINT.

           MOVE TP-APCUR TO D-APCUR.
           IF ( (TP-TRCD = "RT" OR "ET" OR "LP" OR "NP" OR "ST") OR
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
           MOVE TP-LCPDTH-DATE TO NUM-DATE.
           MOVE NUM-MO TO D-LCPD-MO.
           IF TP-UNITPER-CD-DAYS
              MOVE NUM-DA TO D-LCPD-DAYR
           ELSE
              MOVE NUM-YR TO D-LCPD-DAYR.
           COMPUTE D-OTHBAL = TP-OTHBAL + TP-OT2BAL.
           ADD TP-INTBAL TP-LCBAL GIVING D-INTBAL.
           MOVE TP-CURBAL TO D-PRINBAL.

           IF TP-CURBAL = 0
              IF NOT (TP-TRCD = "RC" OR "RA" OR "RP"
                              OR "R1" OR "R2" OR "R3")
                 MOVE "**PAIDOFF**" TO D-PAIDOFF
                 PERFORM WRITE-DETAIL-LINE.
           IF TP-GLAMT(1) NOT = 0
              MOVE TP-GLNO(1) TO D-GLNO
              INSPECT D-GLNO REPLACING ALL "/" BY "-".
           PERFORM WRITE-DETAIL-LINE.

           IF TP-GLAMT(1) NOT = 0 OR
               TP-DLRVCHGS-ADJ(1) NOT = 0 OR
                TP-DLRVCHGS-ADJ(2) NOT = 0 OR
                 TP-DLRVCHGS-ADJ(3) NOT = 0
              PERFORM SETUP-DLRVCHGS-ADJ
              MOVE TP-GLAMT(1) TO D-GLAMT
              PERFORM WRITE-DETAIL-LINE.
           IF TP-GLAMT(2) NOT = 0
              MOVE TP-GLNO(2) TO D-GLNO
              INSPECT D-GLNO REPLACING ALL "/" BY "-"
              PERFORM WRITE-DETAIL-LINE
              MOVE TP-GLAMT(2) TO D-GLAMT
              PERFORM WRITE-DETAIL-LINE.
           IF TP-GLAMT(3) NOT = 0
              MOVE TP-GLNO(3) TO D-GLNO
              INSPECT D-GLNO REPLACING ALL "/" BY "-"
              PERFORM WRITE-DETAIL-LINE
              MOVE TP-GLAMT(3) TO D-GLAMT
              PERFORM WRITE-DETAIL-LINE.

           IF SUM-ONLY = "Y"
              MOVE SPACES TO DETAIL-LINE.

      ********************************************
       CREATE-WK SECTION.
      ********************************************
       CREATE-WK-REC.
           PERFORM SETUP-WK-KEY.
           PERFORM READ-WK-FILE.
           IF IO-FG = 0
              GO TO CREATE-WK-UPDATE.

           PERFORM SETUP-WK-KEY.
           MOVE 0 TO WK-NO.
           MOVE 0 TO WK-TRAMT WK-INTDUE WK-APOTH WK-APINT WK-APLC
                     WK-APCUR WK-CHECKS WK-EARNINGS WK-DLPROC.
           PERFORM WRITE-WK-FILE.
           IF IO-FG NOT = 0
              GO TO IO-ERROR.

           GO TO CREATE-WK-REC.
       CREATE-WK-UPDATE.
           IF TP-TRAMT NOT < 0
              ADD 1 TO WK-NO
           ELSE
              SUBTRACT 1 FROM WK-NO.
           IF TP-OTHBR-FG = " " OR "C"
              IF TP-REFNO = "NSF  "
                 ADD TP-TRAMT TO WK-INTDUE T-INTDUE(LEV)
              ELSE
              IF TP-TRCD = "PY" OR "PA" OR "PE" OR "PO" OR "IN"
                                 OR "DF" OR "D2" OR "D3"
                                  OR "D4" OR "D5" OR "D6"
                                   OR "D7" OR "D8" OR "D9"
                                    OR "PL" OR "TS" OR "TP"
                                     OR "PR" OR "PC" OR "PP"
                                      OR "SS" OR "Z2" OR "AH"
                 ADD TP-TRAMT TO WK-TRAMT T-TRAMT(LEV).
           IF TP-OTHBR-FG = " " OR "G"
              ADD TP-APOTH TO WK-APOTH T-APOTH(LEV)
              ADD TP-APOT2 TO WK-APOTH T-APOTH(LEV)
              ADD HOLD-EARNINGS TO WK-EARNINGS T-EARNINGS(LEV)
              ADD TP-DLPROC TO WK-DLPROC T-DLPROC(LEV)
              ADD TP-APLC TO WK-APLC
              PERFORM TEST-ADD-APLC
              IF ( (TP-TRCD = "RT" OR "ET" OR "LP" OR "NP" OR "ST") OR
                   (TP-TRCD = "RV" AND
                     (TP-ADDON-TEXT = "ADD" OR "ADJ" OR "CNL") ) )
                 IF TP-IBPC = "I"
                    SUBTRACT TP-APCUR FROM WK-APCUR
                    SUBTRACT TP-APCUR FROM T-APCUR(LEV)
                 ELSE
                    SUBTRACT TP-TRAMT FROM WK-APCUR
                    SUBTRACT TP-TRAMT FROM T-APCUR(LEV)
              ELSE
                 ADD TP-APCUR TO WK-APCUR T-APCUR(LEV)
                 ADD TP-APINT TO WK-APINT
                 IF TP-TRCD NOT = "WI"
                    ADD TP-APINT TO T-APINT(LEV).

           IF TP-REFNO = "CHECK"
              ADD TP-TRAMT TO WK-CHECKS T-CHECKS(LEV).

           PERFORM REWRITE-WK-FILE.

       EXIT-CREATE-WK.
           MOVE 0 TO SUB.
           PERFORM OUTPUT-GL 3 TIMES.

      ***********************************
       OUTPUT-GL SECTION.
      ***********************************
           ADD 1 TO SUB.
           IF TP-GLAMT(SUB) = 0
              GO TO END-OUTPUT-GL.
      * ONLY GIVE THE GL AMT TO THE BRANCH THAT IT WAS
      * MADE FOR
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

      ***************************
       FIND-BORROWER SECTION.
      ***************************
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

      ****************************
       WRITE-DETAIL-LINE SECTION.
      ****************************
           ADD LN-FEED LN-COUNT GIVING LN-WK.
           IF LN-WK GREATER THAN EXT-CONAME-RPT-LINES
              PERFORM HEAD-PAGE.
           MOVE DETAIL-LINE TO PR-REC.
           PERFORM WRITE-PR-REC.
           ADD LN-FEED TO LN-COUNT.
           MOVE 1 TO LN-FEED.
           MOVE SPACES TO DETAIL-LINE.

      ********************************
       SET-BRANCH-HEADINGS SECTION.
      *******************************
           MOVE HOLD-BRNO TO H-BRNO.
           MOVE BR-NAME TO H-BRDESC.

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
           MOVE "Y" TO SUMMARY-FG.
           MOVE EXT-CONAME-RPT-LINES TO LN-COUNT.
           PERFORM WRITE-DETAIL-LINE.
           MOVE SUM-HEAD TO PR-REC.
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

           MOVE WK-CLASS TO DS-CLASS LC-CODE.
           PERFORM READ-LC1-FILE.
           IF IO-FG NOT = 0
              MOVE "INVALID CLASS" TO LC-DESC.
           MOVE LC-DESC TO DS-DESC.

           MOVE WK-TRCD     TO DS-TRCD.
           MOVE WK-NO       TO DS-NO.
           MOVE WK-TRAMT    TO DS-TRAMT.
           MOVE WK-INTDUE   TO DS-INTDUE.
           MOVE WK-APOTH    TO DS-APOTH.
           MOVE WK-APINT    TO DS-APINT.
           MOVE WK-APLC     TO DS-APLC.
           MOVE WK-APCUR    TO DS-APCUR.
           MOVE WK-CHECKS   TO DS-CHECKS.
           MOVE WK-EARNINGS TO DS-EARNINGS.
           MOVE WK-DLPROC   TO DS-DLPROC.
           PERFORM WRITE-DETAIL-LINE.
           MOVE 1 TO LN-FEED.

           GO TO PRINT-NEXT-CODE.
       END-CLASS-TOTALS.
           MOVE 2 TO LEV.
           MOVE 1 TO SUB.
           PERFORM UPDATE-TABLE2.
           MOVE ZEROS TO TOTAL-TBL(1).
           MOVE "N" TO SUMMARY-FG.

      *******************************
       BRANCH-TOTALS SECTION.
      *******************************
           MOVE "Y" TO SUMMARY-FG.
           MOVE 0 TO HOLD-CLASS.
           MOVE 2 TO LEV.
           MOVE "BRANCH TOTALS:" TO DETAIL-LINE.
           PERFORM PRINT-SUMMARY.
       END-BRANCH-TOTALS.
           MOVE 3 TO LEV.
           MOVE 2 TO SUB.
           PERFORM UPDATE-TABLE2.
           MOVE ZEROS TO TOTAL-TBL(2).
           MOVE "N" TO SUMMARY-FG.

      ********************************
       GRAND-TOTALS SECTION.
      ********************************
           MOVE "Y" TO SUMMARY-FG.
           MOVE 3 TO LEV.
           MOVE "GRAND TOTALS :" TO DETAIL-LINE.
           PERFORM PRINT-SUMMARY.
       END-GRAND-TOTALS.
           MOVE ZEROS TO TOTAL-TBL(3).
           MOVE "Y" TO SUMMARY-FG.

      ********************************
       GL-SUMMARY SECTION.
      ********************************
           MOVE GL-HEAD TO DETAIL-LINE.
           MOVE 4 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE SPACES TO WK-KEY.
           MOVE "G"    TO WK-CODE.
           MOVE 0      TO WK-BRNO WK-CLASS.
           PERFORM START-WK-FILE.
       PRINT-NEXT-GL.
           PERFORM READ-WK-FILE-NEXT.
           IF IO-FG NOT = 0
              GO TO END-GL-SUMMARY.
           IF WK-CODE NOT = "G"
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
           MOVE T-HH TO H-HR.
           MOVE T-MM TO H-MIN.
           MOVE HEAD1 TO PR-REC.
           MOVE 99 TO LN-FEED.
           PERFORM WRITE-PR-REC.
           MOVE HEAD2 TO PR-REC.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-PR-REC.
           MOVE HEAD3 TO PR-REC.
           PERFORM WRITE-PR-REC.
           MOVE HEAD4 TO PR-REC.
           PERFORM WRITE-PR-REC.
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

       COPY "LIBGB/TIMEIO.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/GRWORK.CPY".
       COPY "LIBGB/GRSERCH.CPY".
       COPY "LIBGB/BRSECURE.CPY".
       COPY "LIBLP/LPMDTE.CPY".
       COPY "LIBLP/LPSTAT.CPY".

      ************************************************
       FORM-RESET SECTION.
      ************************************************
           MOVE EXT-CONAME-CONAME TO EMCASH-CONAME.
           MOVE EXT-CONAME-SYSDATE TO EMCASH-SYSDATE.
           DISPLAY EMCASH-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE EMCASH-FORM-FIELDS TO WR-BUF.
           MOVE EMCASH-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.


      ************************************************
       SETUP-LINK-INFO SECTION.
      ************************************************
           MOVE EMCASH-HELP--FILE TO HELP-LINK-FILE.
           MOVE EMCASH-HELP--DIR TO HELP-LINK-DIR.

      ************************************************
       VDU-CONVERT-FIELD SECTION.
      ************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBLP/EMCASH_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      **************************************
       MISC-PARA SECTION.
      **************************************
       COPY "LIBGB/GETPR.CPY".
       COPY "LIBGB/RMFILE.CPY".
       COPY "LIBGB/MKTEMP.CPY".
       COPY "LIBGB/ACCESS.CPY".
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
       TEST-ADD-APLC.
           IF TP-TRCD NOT = "WL"
              ADD TP-APLC TO T-APLC(LEV).
       SEND-LEGEND.
           MOVE LEGEND TO WR-BUF.
           MOVE MSEL-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.
       SETUP-WK-KEY.
           MOVE SPACES TO WK-REC.
           MOVE "L" TO WK-CODE.
           MOVE TP-BRNO TO WK-BRNO.
           MOVE TP-CLASS TO WK-CLASS.
           MOVE TP-TRCD TO WK-TRCD.
       SETUP-WK-GL1-KEY.
           MOVE SPACES TO WK-REC.
           MOVE "G" TO WK-CODE.
           MOVE TP-GLNO(SUB) TO WK-GLNO.
       UPDATE-TABLE2.
           ADD T-TRAMT(SUB) TO T-TRAMT(LEV).
           ADD T-INTDUE(SUB) TO T-INTDUE(LEV).
           ADD T-APOTH(SUB) TO T-APOTH(LEV).
           ADD T-APINT(SUB) TO T-APINT(LEV).
           ADD T-APLC(SUB) TO T-APLC(LEV).
           ADD T-APCUR(SUB) TO T-APCUR(LEV).
           ADD T-CHECKS(SUB) TO T-CHECKS(LEV).
           ADD T-EARNINGS(SUB) TO T-EARNINGS(LEV).
           ADD T-DLPROC(SUB) TO T-DLPROC(LEV).
       TYPE-TOTALS.
           MOVE 1 TO LEV.
           MOVE "CLASS  TOTALS:" TO DETAIL-LINE.
           PERFORM PRINT-SUMMARY.
       PRINT-SUMMARY.
           MOVE 0 TO DS-NO.
           MOVE T-TRAMT(LEV) TO DS-TRAMT.
           MOVE T-INTDUE(LEV) TO DS-INTDUE.
           MOVE T-APOTH(LEV) TO DS-APOTH.
           MOVE T-APINT(LEV) TO DS-APINT.
           MOVE T-APLC(LEV) TO DS-APLC.
           MOVE T-APCUR(LEV) TO DS-APCUR.
           MOVE T-CHECKS(LEV) TO DS-CHECKS.
           MOVE T-EARNINGS(LEV) TO DS-EARNINGS.
           MOVE T-DLPROC(LEV) TO DS-DLPROC.
           MOVE 3 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.
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
       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO EXT-EOBUF-ERRCD.

      **************************************
       END-PROGRAM SECTION.
      **************************************
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
              MOVE SV-BEG-BR TO BEG-BRANCH
              MOVE SV-END-BR TO END-BRANCH.

           IF EXT-ROUTE-BUF = "00"
              MOVE EXIT-PATH TO FORM-NAM
              MOVE FORM-PATH TO FORM-PATHNAME.
       STOP-RUN.
           IF EXT-ROUTE-BUF = "00" OR "40" OR "50"
              DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN
              DESTROY EMCASH-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.
           EXIT PROGRAM.
