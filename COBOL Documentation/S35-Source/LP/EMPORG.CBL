       IDENTIFICATION DIVISION.
       PROGRAM-ID.   EMPORG.
       DATE-WRITTEN. 042084
      ******************************************************************
      *                      (LP)
      *    DESCRIPTION:  MONTHLY PAID OUT REGISTER
      *
      * REV:
      *  042995 SLC ADDED PLTYPE LOGIC
      *  061295 BLL FIX WORK FILE & KEY REDEFINITIONS
      *  091595 KEC ADDED LOCAL-SPOOLING LOGIC.
      *  091595 KEC CHANGED MILITARY TIME TO AM/PM ON HEADER.
      *  091595 KEC ADDED CHECK FOR VALID BRANCH # FOR CLASS TOTALS.
      *  BAH 092895 RECORDS-FOUND INITIALIZED TO "N" INSTEAD OF " "
      *  100695 JFF ADDED CLASS SCAN
      *  101295 KEC COMMENTED OUT DEFAULT BRANCH SETTING IN INITIZATION.
      *  111395 JFF CHANGED FORM TO INCLUDE LOCAL-SPOOLING QUESTION.
      *  111595 KEC MADE CHANGES FOR GEORGIA 4% / 8% SPLIT.
      *             REFLECTS WORLD A90 VERSION.
      *  112995 BLL FIX TO LOCAL-SPOOL FOR IF BRANCH HAS NO DATA.
      *  121395 JFF CHANGED "IF LOCAL-PRINT-FG = N" TO
      *             "IF LOCAL-PRINT-FG NOT = Y" IN INITIALIZATION.
      *  030596 JFF ADDED "SUMMARY ONLY" TO FORM & PARAMETER SECTION.
      *  031196 JFF ADDED PRINT-RANGE-LINES SECTION.
      *  052096 JFF ADDED 'GROUP TOTALS' LOGIC.
      *  MJD 071896 ADDED WEEKLY UNIT PERIOD LOGIC FOR 6 DIGIT PDTH DATES
      *  JTG 060497 ADDED TEST FOR "R3" & "C3"
      *  BLV 060597 CHANGED FOR LOCAL SPOOL WITH NEW BATCH DRIVER - BREXEC
      *  JTG 091897 ADDED TEST FOR TRCD = 'AH', WORLD A90 ALSO ADDED
      *             TEST ON 'P2', 'P3', 'P4', 'P5'
      *  GLF 022098 DELETED BR-SECURITY-OK AND ADDED LIBGB/BRSECUREW.
      *  BAH 030698 CHANGED WK2-LINE TO PIC 9(3) FROM 99 BECAUSE TP-LINE
      *             PIC 99 COMP ACTUALLY STORES A 3 DIGIT NUMBER !.
      *   CS 042898 ADDED "RB" AND "RO" RENEWAL CODES
      *   CS 050898 CHANGED DETAIL DS-NO TO 4 DIGITS
      *  BAH 062598 CHANGED TP-TILL PIC 9 TO TP-TILLNO PIC X
      *  GLF 033099 ADD TRCD'S "PN" AND "PZ" FOR TFC
      *  GLF 043099 WAS MOVING SPACES TO DETAIL-LINE ON SUMMARY ONLY IN
      *             WRONG SPOT CAUSING LINES TO BE MISSING, IS NOW FIXED
      *  GLF 081999 ADD ARCHIVE PATH INFO. FOR TR FILES NOT FOUND IN DATA
      *             SET FOR SELECTED MONTH AND YEAR
      *  BLV 990826 FIX ARCHIVE PATH LOGIC
      *  BAH 990831 INCLUDE ALL ACCOUNTS REDUCED TO ZERO BALANCE, REGARDLESS
      *             OF TRANSACTION TYPE, ADDED "PA,PE,SS,PR,PP,PC", TFC PR# 46
      *  KEC 000731 {PR#01273} PARADATA FINANCIAL [JUDY]
      *              REPLACED THE PART IN THE DETAIL-LINE IN DISPLAYING
      *              THE ACCTNO WITH SOME STATUS FLAGS WITH STANDARD
      *              LAYOUT FROM LIBLP/LPSTAT (LPS-DISPLAY-LNNO).
      *
      *  KEC 001002 {PR#00000} MERCURY FINANCE [CINDY]
      *              CHANGED WK2-LINE FROM 9(3) TO 9(2) COMP JUST LIKE
      *              TP-LINE; BETH ORIGINALLY MADE THE CHANGE BUT HAD
      *              GOT CAUGHT IN LP/EMTRRG SO CHANGED TO 9(2) COMP.
      *  BAH 010810 THE "POSTED FOR" BRANCH NO. WAS TRUNCATING LAST 2
      *              DIGITS...FOUND MYSELF
      *  CS  010816 ADDED OPTION FOR "OV" TRANS (I,S,O) AND OPTION TO
      *             PRINT BORROWER ADDRESS.
      *  CS  010817 RE:TOTALS. IF OV'S ONLY INCLUDE TR-AMT IN TOTALS, OTHERWISE
      *             DONT INCLUDE IN TOTALS. PER JIM R.
      *  CS  011002 IF PRINT-BW-INFO WAS "N" WAS NOT PRINTING CORRECT INFO
      *  CS  020716 ADD PRINT OF INSURANCE TAX REFUNDS IF NOT ZERO[WORLD PR#288]
      *  CS  020717 CHANGED TEST ON TP-INSURANCE-TAX-REBATE TO TEST NOT = ZERO
      *             OR 2020202.02
      *  BAH 021008 FIXED RECORDS-FOUND TO NOT GIVE "NO RECORDS FOUND"
      *             ON A F7 EXIT
      *  BAH 021014 ADDED NEW TRANSACTION CODE OF "IN" INSURANCE LOSS, EXACTLY
      *             LIKE "PE", REGACC PR# 0007
      *  CS  040608 CHANGED 2020202.02 TEST...FOR TP-INS-TAX-RB.
      *  BAH 040903 REPLACE BR-NAME IN HEADING WITH BR-REGION-NUMBER
      *             FOR GP-I-AM-REGACC, REGACC PR# 77
      *  BAH 041202 REMOVED "RE", USED TO BE EXTENSION CHARGE REBATE
      *             NOW ITS A REPO EXPENSE ADDON CODE...REGACC PL# 453
      *  BAH 070703 DO NOT PRINT SSNO FOR WORLD, WORLD PR# 536
      *  BAH 090731 ADDED LOANDATE, LNAMT, AND AMTFIN FOR GP-I-AM-WORLD AND
      *             BR-STATE = "AL" ONLY, WORLD PR# 646
      *  BAH 090114 TEST GP-HIDE-FIRST-5-SSNO, REGMGM PR# 3785
      *  BAH 130205 ADDED LOANDATE, LNAMT, AND AMTFIN FOR GP-I-AM-THRIFT
      *             ADDED THEM TO THE WORLD #646 (090731), THRIFT PR# 4204
      *  BAH 140514 STATE OF "MS" AND WORLD MUST HAVE ORIG REBATE AMOUNTS
      *             ADDED TO DETAIL LINES AND TOTALS, WORLD PR# 995
      * BAH 2016.1103 CHANGE TILLNO TO 9(2)
      *  CS 161111 CHANGED WK-GL1-KEY AND WK-PO-KEY TO 10, CHANGED
      *            FILLER OF X(2)-WK-GL1-KEY TO X(3), WK-FILLER PIC X(6) TO
      *            PIC X(7)
      * BAH 2016.1129 MOVED D-TILL TO BEGINNING OF LINE TO FIT 9(2)
      * BAH 170530 ACTIVATE 8 DIGIT DATES, PD0006
      *
      * KEC 2017.0530 {PR#00000} PARADATA <A30> [JAY CISZEWSKI]
      *          ADDED 'LN' FILE PREFIX FOR WORKING STORAGE FIELDS.
      *
      * BAH 181207 GLOBAL LNFILE
      * BAH 190122 SPLIT CDFILE
      * BAH 2019.0515 DS-CLASS WAS PIC XX AND CLASS OF 9(3) WAS TRUNCATING
      *               MAKE DS-POCD PIC X(2) AND CLASS REDEFINE AS 9(2).
      *                                                            QA0012
      * BAH 20190922 REMOVED ACCESS-CALL TRPFILE
      * BLM 20210810 REMOVE TP-CODE, TP-POOLID NO LONGER IN KEY
      *  MB  20230909 PERFORMANCE TUNING - ADDED STORED PROCEDURE 
      *               REMOVED USE OF WK2 FILE 
      *               ADDED STATE PARAMETER ARRAY PROCESSING
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
           03  WK-EARNINGS  PIC S9(9)V99  COMP-3.
           03  WK-DLPROC    PIC S9(9)V99  COMP-3.
           03  WK-ORIG      PIC S9(9)V99  COMP-3.

       01  WK-GL-REC.
           03  WK-GL1-KEY.
               05  FILLER   PIC X.
               05  WK-GLNO  PIC 9(11)     COMP-3.
               05  FILLER   PIC X(3).
           03  WK-GLAMT     PIC S9(7)V99  COMP-3.
           03  FILLER       PIC X(55).
       01  WK-PO-REC.
           03  WK-PO-KEY.
               05  FILLER   PIC X.
               05  WK-PO-POCD  PIC XX.
               05  FILLER   PIC X(7).
           03  WK-PO-NO        PIC S9(7)  COMP-3.
           03  WK-PO-TRAMT     PIC S9(9)V99  COMP-3.
           03  WK-PO-INTDUE    PIC S9(9)V99  COMP-3.
           03  WK-PO-APOTH     PIC S9(9)V99  COMP-3.
           03  WK-PO-APINT     PIC S9(9)V99  COMP-3.
           03  WK-PO-APLC      PIC S9(9)V99  COMP-3.
           03  WK-PO-APCUR     PIC S9(9)V99  COMP-3.
           03  WK-PO-EARNINGS  PIC S9(9)V99  COMP-3.
           03  WK-PO-DLPROC    PIC S9(9)V99  COMP-3.
           03  FILLER       PIC X(8).

       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)LP/EMPORG.CBL S35 11/22/23-08:08:52 >".
      ****************************
      *     F I L E   I F N      *
      ****************************
       COPY "LIBLP/LP01TRP.CPY".
       COPY "LIBLP/LP01TRP_SQL.CPY".
       COPY "LIBLP/LPWSTRP.CPY".
       COPY "LIBLP/LP01BW.CPY".
       COPY "LIBLP/LP01BW_SQL.CPY".
       COPY "LIBLP/LPWSBW.CPY".
       COPY "LIBLP/LP01CD.CPY".
       COPY "LIBLP/LP01CD_SQL.CPY".
       COPY "LIBLP/LPWSCD.CPY".
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

       COPY "LIBGB/GRSSPW.CPY".
       COPY "LIBGB/GRBRLSW.CPY".

      *----------------------------------------------------------------

       01  CREATE-SPOOL-FILE-ERR-SW    PIC X           VALUE 'N'.
       01  TRP1-CONN-OPEN-SW           PIC X           VALUE "N".
       01  LOOP-PROC-SUB               PIC 9(9)        VALUE 0.
       01  WS-LAST-PROC-BRANCH         PIC 9(4)        VALUE 0.
 
      *01  WK2-IFN              PIC X(3)     VALUE "WK2".
       01  WK-IFN               PIC X(2)     VALUE "WK".
      *01  WK2-PATH             PIC X(35).

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
       01  MSEL-LIST            PIC X(5)      VALUE "MSEL.".
       01  HOLD-EARNINGS        PIC S9(7)V99  COMP-3 VALUE 0.
       01  HOLD-TP-OTHBR-FG     PIC X                VALUE "?".
       01  HOLD-TP-ACCTNO       PIC 9(8)      COMP-3 VALUE 99999999.
       01  HOLD-TP-SSNO         PIC 9(10)     COMP-3 VALUE 9999999999.
       01  SUB                  PIC 9(2)      COMP-3.
       01  LEV                  PIC 99        COMP-3.
       01  GA-TRCD              PIC X(4).
       01  HOLD-BRNO            PIC 9(4)  VALUE 0.
       01  HOLD-CLASS           PIC 9(3)  VALUE 0.
       01  HOLD-STATE           PIC X(2)  VALUE SPACES.

       01  HOLD-MS-STATE        PIC XX    VALUE SPACES.
       01  LINE-TO-PRINT        PIC 9 VALUE 0.
       01  HOLD-BW-SSNO         PIC 9(9) VALUE ZEROS.
       01  HOLD-LN1-KEY         VALUE ZEROS.
           03  HOLD-LN-OWNBR    PIC 9(4).
           03  HOLD-LN-ACCTNO   PIC 9(8).

       01  RECORDS-FOUND        PIC X     VALUE " ".
           88  NO-RECORDS-FOUND           VALUE "N".
       01  SUMMARY-FG           PIC X     VALUE "N".
       01  NAME-WORKER          PIC X(34).
       01  FIRST-NAME           PIC X(16).

       01  FIRST-TIME           PIC X        VALUE "Y".
       01  MESS-LIST            PIC X(5)     VALUE "MESS.".
       01  C-S-Z                PIC X(66).
       01  STATE                PIC XX.
       01  ZIP                  PIC X(10).

       01 FIRST-TIME-PRINT-ADDR PIC X         VALUE "N".

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
               05  WS-GTT-EARNINGS        PIC S9(9)V99  COMP.
               05  WS-GTT-DLPROC          PIC S9(7)V99  COMP.
               05  WS-GTT-ORIG            PIC S9(7)V99  COMP.

      *-----------------------------------------------------------------
       01  WS-HOLD-PROMPT       PIC X(7)  VALUE SPACES.

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
               05  T-APCUR      PIC S9(9)V99.
               05  T-EARNINGS   PIC S9(9)V99.
               05  T-DLPROC     PIC S9(7)V99.
               05  T-ORIG       PIC S9(7)V99.

       01  LEGEND               PIC X(70).
       01  CLASS-FG             PIC X(3)      VALUE "   ".
           88  CLASS-BEG                      VALUE "BEG".
           88  CLASS-END                      VALUE "END".

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
               05  TOTAL-LEVEL-TABLE  PIC X(1)  OCCURS 10.
           03  OV-ISO          PIC X.
               88 OV-ISO-INCLUDE                 VALUE "I".
               88 OV-ISO-SKIP                    VALUE "S".
               88 OV-ISO-ONLY                    VALUE "O".
           03  PRINT-BW-INFO   PIC X.


       01  VERSION-CONTROL     PIC X VALUE "C".
       01  BEGINNING-DATE       PIC 9(8).
       01  ENDING-DATE          PIC 9(8).

       01  SV-BEG-BR           PIC 9(4)           VALUE 9999.
       01  SV-END-BR           PIC 9(4)           VALUE 9999.
       01  SV-GROUP-FLAG       PIC X     VALUE SPACES.
       01  SV-ENT-GROUP        PIC X(4)  VALUE SPACES.
       01  SV-LOCAL-PRINT-FG   PIC X     VALUE SPACES.

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
           03  FILLER           PIC X(41)    VALUE
               "PAID OUT REGISTER".

       01  HEAD3.
           03  FILLER           PIC X(10) VALUE "BRANCH :".
           03  H-BRNO           PIC 9(4).
           03  FILLER           PIC X VALUE " ".
           03  H-BRDESC         PIC X(20).
           03  FILLER           PIC X(23) VALUE " ".
           03  FILLER           PIC X(41) VALUE "REFUND REGISTER".

       01  HEAD4.
           03  FILLER           PIC X(10) VALUE "CLASS  :".
           03  H-LCNO           PIC 99.
           03  FILLER           PIC X VALUE " ".
           03  H-LCDESC         PIC X(20).

       01  HEAD5.
           03  FILLER           PIC X(16)    VALUE "ACCTNO".
           03  FILLER           PIC X(28)    VALUE
              "DEALER          IB".
           03  FILLER           PIC X(52)    VALUE
           "TR           TRANS       - APPLIED -".
           03  FILLER           PIC X(36)    VALUE
           " ---- BALANCES -----          G/L".

       01  HEAD6.
           03  FILLER           PIC X(11)      VALUE
           "    TL".
           03  FILLER           PIC X(56)     VALUE
           "     PROC'D   EARNED PC PAY-DATE CD  REFNO   AMOUNT".
           03  FILLER           PIC X(45)     VALUE
           " INT     PRIN  PDTH LCPD     OTH      INT".
           03  FILLER           PIC X(20)     VALUE
           " PRIN      ACCT-AMT".

       01  HEAD7.
           03  FILLER           PIC X(11)      VALUE SPACES.
           03  FILLER           PIC X(56)     VALUE
           "              ORIG AMT".

      *-----------------------------------------------------------------
       01  GTT-HEAD1.
           03  FILLER            PIC X(57)  VALUE SPACES.
           03  FILLER            PIC X(18)  VALUE "GROUP TOTALS TABLE".
           03  FILLER            PIC X(57)  VALUE SPACES.

      *-----------------------------------------------------------------
       01  GTT-HEAD2.
           03  FILLER            PIC X(24)  VALUE
               "BRANCH/GROUP   GR       ".
           03  FILLER            PIC X(9)   VALUE "   NO    ".
           03  FILLER            PIC X(10)  VALUE "TRANS     ".
           03  FILLER            PIC X(13)  VALUE "INTEREST     ".
           03  FILLER            PIC X(11)  VALUE "APPLIED    ".
           03  FILLER            PIC X(11)  VALUE "APPLIED    ".
           03  FILLER            PIC X(10)  VALUE " APPLIED  ".
           03  FILLER            PIC X(14)  VALUE " APPLIED      ".
           03  FILLER            PIC X(10)  VALUE "EARNINGS  ".
           03  GTT-HEAD2-MS      PIC X(11)  VALUE "DEALER     ".

      *-----------------------------------------------------------------
       01  GTT-HEAD3.
           03  FILLER            PIC X(14)  VALUE SPACES.
           03  FILLER            PIC X(10)  VALUE "LEV       ".
           03  FILLER            PIC X(9)   VALUE "         ".
           03  FILLER            PIC X(10)  VALUE "AMOUNT    ".
           03  FILLER            PIC X(13)  VALUE "DUE          ".
           03  FILLER            PIC X(11)  VALUE "OTHER      ".
           03  FILLER            PIC X(12)  VALUE "INTEREST    ".
           03  FILLER            PIC X(10)  VALUE "LT-CHG    ".
           03  FILLER            PIC X(13)  VALUE "PRINCIPAL    ".
           03  FILLER            PIC X(9)   VALUE "         ".
           03  GTT-HEAD3-MS      PIC X(11)  VALUE " PROCEEDS  ".

       01  SUM-HEAD.
           03  FILLER           PIC X(55)    VALUE
           "CLASS               TRCD   NO     TR-AMT    INT-DUE    ".
           03  FILLER           PIC X(52)    VALUE
           "APP-OTH    APP-INT     APP-LC    APP-PRIN   EARNINGS".
           03  SUM-MS-HEAD      PIC X(11)    VALUE
           "    DL-PROC".

       01  GL-HEAD.
           03  FILLER           PIC X(17)    VALUE "G/L DIST  SUMMARY".

       01  POCD-HEAD.
           03  FILLER           PIC X(55)    VALUE
           "PAYOFF CODE                NO     TR-AMT    INT-DUE    ".
           03  FILLER           PIC X(52)    VALUE
           "APP-OTH    APP-INT     APP-LC    APP-PRIN   EARNINGS".
           03  FILLER           PIC X(11)    VALUE
           "    DL-PROC".

       01  DETAIL-LINE          VALUE SPACES.
           03  FILLER           PIC X(87).
           03  D-AMTFIN-TEXT    PIC X(12).
           03  FILLER           PIC X.
           03  D-AMTFIN         PIC Z(6).99 BLANK ZERO.
           03  FILLER           PIC X(7).
           03  D-BRXX           PIC X(12).
           03  D-BRX            PIC 9(4).

       01  D-LINE1 REDEFINES DETAIL-LINE.
           03  D-DISPLAY-LNNO   PIC X(16).
           03  FILLER           PIC X(03).
           03  D-NAME           PIC X(24).
           03  FILLER           PIC X.
           03  D-MAKER          PIC X.
           03  FILLER           PIC XX.
           03  D-SSNO           PIC 999/99/9999 BLANK ZERO.
           03  FILLER           PIC X(3).
           03  D-DLNO           PIC 9999.99.
           03  FILLER           PIC X(4).
           03  D-LOANDATE-TEXT  PIC X(8).
           03  FILLER           PIC X.
           03  D-LOANDATE       PIC 99/99/99 BLANK ZERO.
           03  FILLER           PIC XX.
           03  D-LNAMT-TEXT     PIC X(8).
           03  FILLER           PIC X.
           03  D-LNAMT          PIC Z(6).99 BLANK ZERO.
      * CANT USE ANYMORE OF THIS LINE BECAUSE THE "POSTED BY" IS HERE

       01  D-LINE1-B REDEFINES DETAIL-LINE.
           03  FILLER           PIC X(19).
           03  D-BW-ADR         PIC X(66).

       01  D-LINE2 REDEFINES DETAIL-LINE.
           03  FILLER           PIC X(4).
           03  D-TILL           PIC Z9.
           03  FILLER           PIC X(7).
           03  D-DLPROC         PIC ZZZZZ.ZZ-.
           03  D-MS-TAG         PIC X(9) REDEFINES D-DLPROC.
           03  D-EARNINGS       PIC ZZZZZ.ZZ-.
           03  FILLER           PIC X.
           03  D-IBPC           PIC X.
           03  FILLER           PIC X.
           03  D-DATE           PIC 99/99/99.
           03  FILLER           PIC X.
           03  D-NORMAL-TRCD.
               05  FILLER       PIC X.
               05  D-TRCD       PIC XX.
               05  FILLER       PIC X.
           03  D-GA-TRCD        REDEFINES D-NORMAL-TRCD.
               05  D-TRCD-GA    PIC X(4).
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
           03  FILLER             PIC X(7).
           03  D-TAX-INSURANCE-TAX-REFUND.
               05  FILLER         PIC X.
               05  D-TAX-REFUND-T PIC X.
               05  D-TAX-REFUND-INSTYPE PIC X(2).
           03  FILLER             PIC X(121).

       01  D-LINE4 REDEFINES DETAIL-LINE.
           03  D2-GROUP             PIC X(19).
           03  D2-GROUP-1           REDEFINES D2-GROUP.
               05  DS-POCD          PIC XX.
               05  DS-CLASS REDEFINES DS-POCD PIC 99.
               05  FILLER           PIC X.
               05  DS-DESC          PIC X(16).
           03  D2-GROUP-2           REDEFINES D2-GROUP.
               05  D2-PROMPT1       PIC X(7).
               05  FILLER           PIC X(2).
               05  D2-GRNO          PIC X(4).
               05  D2-BRNO          REDEFINES D2-GRNO
                                    PIC 9(4).
               05  FILLER           PIC X(3).
               05  D2-GR-LEVEL      PIC 9(1).
               05  FILLER           PIC X(2).
           03  FILLER           PIC X(1).
           03  DS-NORMAL-TRCD.
               05  FILLER       PIC XX.
               05  DS-TRCD      PIC XX.
           03  DS-GA-TRCD       REDEFINES DS-NORMAL-TRCD.
               05  DS-TRCD-GA   PIC XXXX.
      *    03  FILLER           PIC XX.
      *    03  DS-NO            PIC ZZZ.
           03  FILLER           PIC X.
           03  DS-NO            PIC ZZZZ.

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
           03  DS-APCUR         PIC ZZZZZZZ.ZZ-.
           03  FILLER           PIC X.
           03  DS-EARNINGS      PIC ZZZZZZ.ZZ-.
           03  FILLER           PIC X.
           03  DS-DLPROC        PIC ZZZZZZ.ZZ-.
           03  DS-MS-TAG        PIC X(10).

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
      *-----------------------------------------------------------------

       COPY "LIBGB/PASSWDW.CPY".
       COPY "LIBLP/EMSFRG_CMD.CPY".
       COPY "LIBWI/GRSCANW.CPY".
       COPY "LIBGB/GRWORKW.CPY".
       COPY "LIBWI/GRTOTLW.CPY".
       COPY "LIBWI/LCSCANW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBGB/MKTEMPW_CMD.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/DACCESSW.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBLP/LPMDTEW.CPY".
       COPY "LIBLP/LPSTATW.CPY".
       COPY "LIBLP/LPAMTSW.CPY".
       COPY "LIBGB/BRSECUREW.CPY".
       COPY "LIBGB/SYSTEMW.CPY".
       COPY "LIBGB/EOSPOOLW.CPY".
       COPY "LIBGB/GETPRW.CPY".

       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBLP/EMPORG_DEF.CPY".
       COPY "LIBLP/EMPORG_WKS.CPY".
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
       COPY "LIBLP/EMPORG_SCN.CPY".

      *-----------------------------------------------------------------
       PROCEDURE DIVISION
            USING FORM-PATHNAME EXIT-PATHNAME.
       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
               PR-FILE WK-FILE.  
       COPY "LIBGB/DECLRP.CPY".
           PERFORM CLOSE-TRP1-FILE.
           PERFORM CLOSE-SP1-FILE.
      *     PERFORM CLOSE-LN1-FILE.
           PERFORM CLOSE-LC1-FILE.
           PERFORM CLOSE-CD1-FILE.
      *     PERFORM CLOSE-BW1-FILE.
           PERFORM CLOSE-GR1-FILE.
           PERFORM CLOSE-BR-FILE.
           PERFORM CLOSE-PR-FILE.
           PERFORM CLOSE-TRP1-CONNECTION.
           CLOSE WK-FILE.
       COPY "LIBGB/DECLRP2.CPY".

      ******************************************************************
      *
      *    M A I N - M O D U L E
      *
      ******************************************************************
       MAIN-MODULE SECTION.

           PERFORM SQL-CONNECT.

           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "EMPORG" TO FORM-PROG VDU-FORM-NAME.

           PERFORM INITIALIZATION.
           IF ( EXT-EOBUF-ERRCD NOT = " " )
              GO TO END-PROGRAM.

           MOVE "N" TO RECORDS-FOUND.
           MOVE SPACES TO HOLD-MS-STATE.

      *     IF ( GROUP-FLAG = "N" )
      *        MOVE BEG-BRANCH TO BR-NO
      *        PERFORM START-BR-FILE
      *        GO TO NEXT-BRANCH-NO.

      *     MOVE ENT-GROUP TO GR-ID.
      *     PERFORM SEARCH-GR-INIT.

           MOVE EXT-FILPATH-BASE TO FILPATH-BASE.
           MOVE "B"      TO BW-PATH-OVERRIDE
                            LN-PATH-OVERRIDE
                            TRP-PATH-OVERRIDE.

           PERFORM OPEN-TRP1-FILE.

      * LOAD BRANCHES TO PROCESS USING STORED PROCEDURE
      *  SSP_TCLP_BRANCH_GROUP_LOGIC_SELECT INTO BGLS-BRANCH-TBL
           IF GROUP-FLAG = "Y"
               MOVE ENT-GROUP TO BGLS-PASS-BEG-BRGRP
               MOVE SPACES    TO BGLS-PASS-END-BRGRP
           ELSE
               MOVE BEG-BRANCH                TO BRANCHLIST-WORK-BRNO
               MOVE BRANCHLIST-WORK-BRNO-TEXT TO BGLS-PASS-BEG-BRGRP
               MOVE END-BRANCH                TO BRANCHLIST-WORK-BRNO
               MOVE BRANCHLIST-WORK-BRNO-TEXT TO BGLS-PASS-END-BRGRP.

           PERFORM LOAD-BGLS-BRANCH-LIST.

      * SET BRANCHLIST-PGM-PROC-MAX TO NUMBER OF BRANCHES TO PROCESS PER
      * STORED PROCEDURE CURSOR
           MOVE 800 TO BRANCHLIST-PGM-PROC-MAX.

           PERFORM VARYING LOOP-PROC-SUB 
            FROM 1 BY 1 UNTIL LOOP-PROC-SUB > BGLS-BRANCHES-RETR-CTR
              ADD 1 TO BRANCHLIST-PROC-BRANCH-CTR
                       BRANCHLIST-SUB
              MOVE BGLS-BRANCH-BRNO(LOOP-PROC-SUB) TO 
                                    BRANCHLIST-BRNO(BRANCHLIST-SUB)
              IF BRANCHLIST-SUB > 1
                 MOVE ","   TO BRANCHLIST-COMMA(BRANCHLIST-SUB)
              END-IF 

              IF BRANCHLIST-PROC-BRANCH-CTR = BRANCHLIST-PGM-PROC-MAX 
               OR LOOP-PROC-SUB = BGLS-BRANCHES-RETR-CTR
                 MOVE BRANCHLIST-TBL TO PASS-BRANCHLIST
                 PERFORM OPEN-TRP-CURSOR
                 MOVE 'N' TO CREATE-SPOOL-FILE-ERR-SW
                 PERFORM NEXT-TR-REPORT
                 PERFORM CLOSE-TRP1-FILE
                 INITIALIZE BRANCHLIST-TBL BRANCHLIST-SUB
                            BRANCHLIST-PROC-BRANCH-CTR
                            PASS-BRANCHLIST
              END-IF
           END-PERFORM.

           GO TO END-ROUTINE.

      *-----------------------------------------------------------------
      *NEXT-GROUP-LOC.
      *    IF ( GROUP-FLAG NOT = "Y" )
      *       GO TO NEXT-BRANCH-NO.
      *
      *    PERFORM SEARCH-GR-NEXT.
      *
      *    IF ( WGR-SRESULT = 0 )
      *       GO TO END-ROUTINE.
      *
      *    MOVE WGR-SRESULT TO BR-NO.
      *    PERFORM READ-BR-FILE.
      *    IF ( IO-FG NOT = 0 )
      *       GO TO IO-ERROR.
      *
      *    GO TO MOVE-BRANCH-PATH-INFO.
      *
      *-----------------------------------------------------------------
      *NEXT-BRANCH-NO.
      *    PERFORM READ-BR-FILE-NEXT.
      *    IF ( IO-FG = 9 ) OR ( BR-NO > END-BRANCH )
      *       GO TO END-ROUTINE.
      *
      *-----------------------------------------------------------------
       MOVE-BRANCH-PATH-INFO.
           IF ( LOCAL-PRINT-FG = "Y" )
             PERFORM CLOSE-PR-FILE
             PERFORM CREATE-SPOOL-DIR
             IF ( STAT NOT = "00" )
                 MOVE 'Y' TO CREATE-SPOOL-FILE-ERR-SW
                 EXIT PARAGRAPH.
      *          GO TO NEXT-GROUP-LOC.
      
           MOVE EXT-FILPATH-BASE           TO FILPATH-BASE.
           MOVE BR-MACHINE                 TO FILPATH-MACHINE.
           MOVE BR-DATA-PATH               TO FILPATH-DATA.
      
           MOVE "B"                        TO BW-PATH-OVERRIDE
                                             LN-PATH-OVERRIDE
                                             TRP-PATH-OVERRIDE.
      
           IF HOLD-MS-STATE = SPACES
              MOVE BR-STATE TO HOLD-MS-STATE.
      
           PERFORM OPEN-TRP1-FILE.
      *     PERFORM OPEN-BW1-FILE.
      *     PERFORM OPEN-LN1-FILE.

      *    PERFORM SORT-ROUTINE.
      *
      *    IF ( EXT-EOBUF-ERRCD = "X" )
      *       GO TO NEXT-GROUP-LOC.
      *
      *    PERFORM OPEN-WK2-FILE.

      *-----------------------------------------------------------------
       NEXT-TR-REPORT.
      *     PERFORM READ-WK2-FILE-NEXT.
      *     IF ( IO-FG = 9 )
      *        GO TO END-OF-GROUP.

           PERFORM READ-TRP1-FILE-NEXT.   *> EMBEDDED - TRP BW BR LN
           IF IO-FG NOT = 0
              IF WS-LAST-PROC-BRANCH NOT = 0 
                  PERFORM END-OF-GROUP
              END-IF
              EXIT PARAGRAPH.

      * IF LAST BRANCH BREAK ERROR'D CREATING SPPOL DIRECTORY THEN MUST BYPASS ALL
      *   TRP RECORDS FOR BEANCH AND THEN RESET WS-LAST-PROC-BRANCH.
           IF CREATE-SPOOL-FILE-ERR-SW = 'Y' 
              IF  TP-BRNO = WS-LAST-PROC-BRANCH
                 GO TO NEXT-TR-REPORT
              ELSE 
                 MOVE 0 TO WS-LAST-PROC-BRANCH.

      **************************************************
      * THE FOLLOWING CHECKS COPIED FROM SORT-ROUTINE  *
      **************************************************
      *08/15/01 PR# 256  OPTIONS ON "OV"
           IF TP-TRCD NOT = "OV"
              IF OV-ISO-ONLY
                 GO TO NEXT-TR-REPORT.
      
           IF TP-TRCD = "OV"
             IF OV-ISO-SKIP
               GO TO NEXT-TR-REPORT.

      * NEW CODE FOR PR# 46 TFC  INCLUDE ALL CODES THAT TAKE ACCOUNT
      * TO ZERO BALANCE
      *
      *08/14/01 ADDED "OV" TO CODES THAT TAKE ACCOUNT TO ZERO BALANCE.
      ****
      **>>  THIS CHECK ALSO IN STORED PROCEDURE <<**
           IF NOT (TP-TRCD = "PO" OR "PB" OR "SC" OR "RN" OR "RB"
                          OR "RO" OR "RC" OR "RA" OR "RP" OR "R1"
                           OR "R2" OR "R3" OR "RI" OR "RS" OR "RM"
                            OR "RD" OR "PA" OR "PE" OR "SS"
                             OR "PR" OR "PP" OR "PC" OR "OV" OR "IN")
             GO TO NEXT-TR-REPORT.
  
      * DONT PRINT A MANUAL REBATE ON THE PAYOFF REGISTER
           IF TP-TRCD = "RC" OR "RA" OR "RP" OR "R1" OR "R2" OR "R3"
                             OR "RI" OR "RS" OR "RM" OR "RD"
             IF TP-MANUAL-RB = "Y"
                GO TO NEXT-TR-REPORT.

      * WENT TO 0 BALANCE VIA THESE CODES:
      **>>  THIS CHECK ALSO IN STORED PROCEDURE <<**
           IF TP-TRCD = "PA" OR "PE" OR "SS" OR "PR" OR "PP" OR "PC"
                             OR "IN"
             IF TP-CURBAL = 0
                 NEXT SENTENCE
      * REVERSED FROM 0 BALANCE
             ELSE
                 IF (TP-APCUR * -1) = TP-CURBAL
                    NEXT SENTENCE
                 ELSE
                    GO TO NEXT-TR-REPORT.

      *     MOVE WK2-DATE   TO TP-DATE.
      *     MOVE WK2-BRANCH TO TP-BRNO.
      **    MOVE WK2-CODE   TO TP-CODE.
      **    MOVE 0          TO TP-POOLID.
      *     MOVE WK2-CLASS  TO TP-CLASS.
      *     MOVE WK2-ACCTNO TO TP-ACCTNO.
      *     MOVE WK2-LINE   TO TP-LINE.
      *     PERFORM READ-TRP1-FILE.
      *     IF ( IO-FG = 9 )
      *        MOVE "TRANSACTION RECORD MISSING SINCE SORT" TO MESS
      *        PERFORM SEND-MESS
      *        GO TO IO-ERROR.

           IF TP-BRNO NOT = WS-LAST-PROC-BRANCH
               IF WS-LAST-PROC-BRANCH NOT = 0
                   PERFORM END-OF-GROUP
               END-IF
      * MUST BYPASS BRANCH IF UNABLE TO CREATE SPOOL DIR IN
      * MOVE-BRANCH-PATH-INFO
               MOVE 'N' TO CREATE-SPOOL-FILE-ERR-SW
               PERFORM MOVE-BRANCH-PATH-INFO
               IF CREATE-SPOOL-FILE-ERR-SW = 'Y'  
                   MOVE TP-BRNO TO WS-LAST-PROC-BRANCH
                   GO TO NEXT-TR-REPORT
               END-IF
               IF EXT-ROUTE-BUF = "00"
                   MOVE SPACES TO WR-BUF
                   STRING "REPORT IN PROGRESS...............", TP-BRNO
                          DELIMITED "  " INTO WR-BUF
                   MOVE MESS-LIST TO WR-LIST
                   PERFORM CALL-WRFORM
               END-IF
               MOVE TP-BRNO TO WS-LAST-PROC-BRANCH.

           IF ( FIRST-TIME = "Y" )
              MOVE BR-STATE  TO HOLD-STATE
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
              MOVE BR-STATE TO HOLD-STATE
              IF ( GTT-ISO NOT = "O" )
                 IF SUM-ONLY = "N"
                    PERFORM HEAD-PAGE.

           IF ( TP-CLASS NOT = HOLD-CLASS )
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

      *-----------------------------------------------------------------
       END-OF-GROUP.
      *     PERFORM CLOSE-TRP1-FILE.
      *     PERFORM CLOSE-BW1-FILE.
      *     PERFORM CLOSE-LN1-FILE.
      *     PERFORM CLOSE-WK2-FILE.
      *     PERFORM OPEN-WK2-FILE-OUTPUT.

           IF ( LOCAL-PRINT-FG = "Y" )
              IF ( FIRST-TIME NOT = "Y" )
                 PERFORM CLASS-TOTALS
                 PERFORM BRANCH-TOTALS
                 PERFORM GRAND-TOTALS
                 PERFORM POCD-SUMMARY
                 PERFORM GL-SUMMARY
                 MOVE "Y" TO FIRST-TIME
                 MOVE 0 TO PAGE-NUMBER
              ELSE
                 MOVE 0 TO PAGE-NUMBER.

      *     GO TO NEXT-GROUP-LOC.

      *----------------------------------------------------------------
       END-ROUTINE.

           IF ( NO-RECORDS-FOUND )
              GO TO END-PROGRAM.

           IF EXT-ROUTE-BUF = "00"
              MOVE "PAYOFF REGISTER IN PROGRESS............" TO WR-BUF
              MOVE MESS-LIST TO WR-LIST
              PERFORM CALL-WRFORM.

      *--->PERFORM CLASS-TOTALS.
      *--->PERFORM BRANCH-TOTALS.

           IF ( LOCAL-PRINT-FG = "N" )
              PERFORM CLASS-TOTALS
              PERFORM BRANCH-TOTALS
              PERFORM GRAND-TOTALS
              PERFORM POCD-SUMMARY
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
           MOVE EMPORG-QUEUE-POS TO Q-POS.
           MOVE EMPORG-COPIES-POS TO C-POS.
           MOVE EMPORG-PRU-POS   TO P-POS.
           MOVE SPACES TO Q-BUF C-BUF P-BUF.
       COPY "LIBGB/EOINIT.CPY".

           MOVE SPACES TO RECORDS-FOUND.

      * CREATE INDEXED KEY FILE FOR TRFILE OUTPUT
      *     MOVE MKTEMP-DEFAULT TO MKTEMP-BUF.
      *     PERFORM MKTEMP-CALL.
      *     IF STAT NOT = "00"
      *        MOVE "CANNOT MAKE WK2 SORT KEY FILE" TO MESS
      *        PERFORM SEND-MESS
      *        GO TO ENDIT.

      *     MOVE MKTEMP-BUF TO WK2-PATH.
      *     PERFORM OPEN-WK2-FILE-OUTPUT.

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
              MOVE "PAYOFF REGISTER IN PROGRESS............" TO WR-BUF
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
           PERFORM OPEN-CD1-FILE.
           PERFORM OPEN-LC1-FILE.

           MOVE ZEROS TO TOTAL-TBL(1) TOTAL-TBL(2) TOTAL-TBL(3).
           MOVE 0 TO PAGE-NUMBER.

      *     PERFORM OPEN-WK2-FILE.
           PERFORM OPEN-BR-FILE.
           PERFORM OPEN-GR1-FILE.
           MOVE MKTEMP-DEFAULT TO MKTEMP-BUF.
           PERFORM MKTEMP-CALL.
           PERFORM OPEN-WK-FILE-OUTPUT.
           PERFORM CLOSE-WK-FILE.
           PERFORM OPEN-WK-FILE.

           MOVE EXT-CONAME-CONAME TO H-NAME.
           MOVE EXT-CONAME-SYSDATE TO H-DATE.
           MOVE EXT-CONAME-USER-ID TO H-USERID.

           PERFORM ARRAYSP-LOAD.

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

      **************************************************
       EO-EXEC-AUDIT SECTION.
      **************************************************
           MOVE EXT-EOBUF-DATE2 TO SYS-DATE.
           MOVE S-MM TO END-MONTH.
           MOVE S-YY TO END-YEAR.

           IF EXT-EOBUF-DATE1 NOT = EXT-EOBUF-DATE2
              MOVE 99 TO DAY-OF-MONTH
           ELSE
              MOVE S-DD TO DAY-OF-MONTH.

      ******************************************************
       ENTER-PARAMETERS SECTION.
      ******************************************************
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
      *---------------------------------------------------
       BRANCH-IS-NUMERIC.
           MOVE "N" TO GROUP-FLAG.
           MOVE WGR-SRESULT TO BEG-BRANCH.
           MOVE "SN N040BR1." TO VDU.
           MOVE BEG-BRANCH TO VDUNUM0.
           PERFORM SCREENIO.
      *---------------------------------------------------
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
      *-----------------------------------------------------
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
      *-----------------------------------------------------
       CHECK-BRANCH-SECURITY.
           PERFORM BR-RANGE-SECURITY.
           IF BR-SECURITY-OKAY = "N"
              GO TO BR-01.
      *-----------------------------------------------------
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
      *-------------------------------------------------------
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
      *--------------------------------------------------------
       ALL-CL.
           MOVE "SNNN020CL1." TO VDU.
           MOVE 0 TO BEG-CLASS VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN020CL2." TO VDU.
           MOVE 99 TO END-CLASS VDUNUM0.
           PERFORM SCREENIO.
      *-------------------------------------------------------
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
              GO TO OVERPAYMENTS-ISO.

           IF VDUSTATUS = "1U"
              IF EXT-ROUTE-BUF NOT = "00"
                 GO TO CL-02
              ELSE
                 GO TO DTE.
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

      *---------------------------------------------------------------
       OVERPAYMENTS-ISO.
           MOVE "E  A010OV-ISO." TO VDU.
           PERFORM SCREENIO.

           IF F7-KEY GO TO EXIT-PARM.
           IF F1-KEY GO TO EXIT-PARM.
           IF UP-KEY GO TO DA-01.

           IF NOT(DOWN-KEY OR ENTER-KEY)
              GO TO OVERPAYMENTS-ISO.

           MOVE VDUBUF TO OV-ISO.

           IF NOT (OV-ISO-INCLUDE OR
                   OV-ISO-SKIP    OR
                   OV-ISO-ONLY )
                   GO TO OVERPAYMENTS-ISO.


      *----------------------------------------------------------
       PRINT-BORROWER-INFO.
           MOVE "E  A010BW-INFO." TO VDU.
           PERFORM SCREENIO.

           IF F7-KEY GO TO EXIT-PARM.
           IF F1-KEY GO TO EXIT-PARM.
           IF UP-KEY GO TO OVERPAYMENTS-ISO.

           IF NOT (DOWN-KEY OR ENTER-KEY)
              GO TO PRINT-BORROWER-INFO.

           MOVE VDUBUF TO PRINT-BW-INFO.

           IF NOT (PRINT-BW-INFO = "Y" OR "N")
              GO TO PRINT-BORROWER-INFO.

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
           IF ( VDUSTATUS = "1U" ) GO TO PRINT-BORROWER-INFO.
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
              IF EXT-ROUTE-BUF = "00"
                 IF ( DAY-OF-MONTH = 99 )
                    MOVE "SNNN020DA1." TO VDU
                    PERFORM SCREENIO
                    GO TO PRINT-BORROWER-INFO.

           IF VDUSTATUS = "1U"
              IF EXT-ROUTE-BUF NOT = "00"
                 GO TO PRINT-BORROWER-INFO.
      *       ELSE
      *          GO TO PRINT-BORROWER-INFO.

           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO SUMYN.
           MOVE VDUBUF TO SUM-ONLY.
           IF SUM-ONLY NOT = "N" AND NOT = "Y" GO TO SUMYN.

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

       END-PARM.
           MOVE LEGEND-ACCEPT TO LEGEND.
           PERFORM SEND-LEGEND.

       EXIT-PARM.
           EXIT.

      **********************************************
       DISPLAY-PARAMETERS SECTION.
      **********************************************

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
      *----------------------------------------------------------
           MOVE "S  A010OV-ISO." TO VDU.
           MOVE OV-ISO           TO VDUBUF.
           PERFORM SCREENIO.

      *----------------------------------------------------------
           MOVE "S  A010BW-INFO." TO VDU.
           MOVE PRINT-BW-INFO     TO VDUBUF.
           PERFORM SCREENIO.
      *----------------------------------------------------------
           MOVE "S  A010GTT-ISO." TO VDU.
           MOVE GTT-ISO           TO VDUBUF.
           PERFORM SCREENIO.
      *--------------------------------------------------------
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

      ******************************************************************
      * SORT-ROUTINE SECTION.
      ******************************************************************
      *
      *    MOVE BR-NO               TO TP-BRNO
      *                                QTRP1-WBEG-BRNO
      *                                QTRP1-WEND-BRNO.
      *    MOVE BEGINNING-DATE      TO TP-DATE SQL-DATE-YYYYMMDD.
      *    PERFORM SQL-SET-DATE.
      *    MOVE SQL-DATE-YYYY-MM-DD TO QTRP1-WBEG-DATE.
      *    MOVE ENDING-DATE         TO  SQL-DATE-YYYYMMDD.
      *    PERFORM SQL-SET-DATE.
      *    MOVE SQL-DATE-YYYY-MM-DD TO QTRP1-WEND-DATE.
      *
      *    MOVE BEG-CLASS           TO TP-CLASS
      *                                QTRP1-WBEG-CLASS.
      *    MOVE END-CLASS           TO QTRP1-WEND-CLASS.
      *    MOVE 0                   TO TP-ACCTNO TP-LINE
      *                                QTRP1-WBEG-ACCTNO
      *                                QTRP1-WBEG-LINE.
      *    MOVE ALL "9"             TO QTRP1-WEND-ACCTNO
      *                                QTRP1-WEND-LINE.
      *
      *    MOVE TP-DATE              TO NDTE-DATE.
      *    MOVE 1                    TO NDTE-HOLD.
      *    PERFORM INCREMENT-DAYS.
      *    MOVE NDTE-DATE            TO SQL-DATE-YYYYMMDD.
      *    PERFORM SQL-SET-DATE.
      *    MOVE SQL-DATE-YYYY-MM-DD  TO QTRP1-WSBEG-DATE.
      *
      *    IF ( TP-CLASS = 999 )
      *       MOVE TP-CLASS          TO QTRP1-WSBEG-CLASS
      *    ELSE
      *       COMPUTE QTRP1-WSBEG-CLASS  = TP-CLASS + 1.
      *
      *    IF ( TP-ACCTNO = 99999999 )
      *       MOVE TP-ACCTNO         TO QTRP1-WSBEG-ACCTNO
      *    ELSE
      *       COMPUTE QTRP1-WSBEG-ACCTNO = TP-ACCTNO + 1.
      *
      *    IF ( TP-LINE = 999 )
      *       MOVE TP-LINE           TO QTRP1-WSBEG-LINE
      *    ELSE
      *       COMPUTE QTRP1-WSBEG-LINE   = TP-LINE + 1.
      *
      *    PERFORM START-TRP1-FILE.
      *
      *SORT-TR-NEXT.
      *    PERFORM READ-TRP1-FILE-NEXT.
      *    IF (IO-FG NOT = 0) OR (TRP-PATH-OWNBR NOT = TP-BRNO)
      *       GO TO EXIT-SORT-ROUTINE.
      *
      *
      *    IF TP-DATE > ENDING-DATE
      *       GO TO EXIT-SORT-ROUTINE.
      *
      *    IF TP-CLASS < BEG-CLASS OR TP-CLASS > END-CLASS
      *       GO TO SORT-TR-NEXT.
      *
      *08/15/01 PR# 256  OPTIONS ON "OV"
      *    IF TP-TRCD NOT = "OV"
      *       IF OV-ISO-ONLY
      *           GO TO SORT-TR-NEXT.
      *
      *    IF TP-TRCD = "OV"
      *       IF OV-ISO-SKIP
      *         GO TO SORT-TR-NEXT.
      *
      * NEW CODE FOR PR# 46 TFC  INCLUDE ALL CODES THAT TAKE ACCOUNT
      * TO ZERO BALANCE
      *
      *08/14/01 ADDED "OV" TO CODES THAT TAKE ACCOUNT TO ZERO BALANCE.
      ****
      *    IF NOT (TP-TRCD = "PO" OR "PB" OR "SC" OR "RN" OR "RB"
      *                    OR "RO" OR "RC" OR "RA" OR "RP" OR "R1"
      *                     OR "R2" OR "R3" OR "RI" OR "RS" OR "RM"
      *                      OR "RD" OR "PA" OR "PE" OR "SS"
      *                       OR "PR" OR "PP" OR "PC" OR "OV" OR "IN")
      *       GO TO SORT-TR-NEXT.
      *
      * DONT PRINT A MANUAL REBATE ON THE PAYOFF REGISTER
      *    IF TP-TRCD = "RC" OR "RA" OR "RP" OR "R1" OR "R2" OR "R3"
      *                       OR "RI" OR "RS" OR "RM" OR "RD"
      *       IF TP-MANUAL-RB = "Y"
      *          GO TO SORT-TR-NEXT.
      *
      * WENT TO 0 BALANCE VIA THESE CODES:
      *    IF TP-TRCD = "PA" OR "PE" OR "SS" OR "PR" OR "PP" OR "PC"
      *                       OR "IN"
      *       IF TP-CURBAL = 0
      *           NEXT SENTENCE
      * REVERSED FROM 0 BALANCE
      *       ELSE
      *           IF (TP-APCUR * -1) = TP-CURBAL
      *              NEXT SENTENCE
      *           ELSE
      *              GO TO SORT-TR-NEXT.
      *
      *
      *    MOVE TP-DATE     TO WK2-DATE.
      *    MOVE TP-BRNO    TO WK2-BRANCH.
      *    MOVE TP-CODE    TO WK2-CODE.
      *    MOVE TP-CLASS   TO WK2-CLASS.
      *    MOVE TP-ACCTNO  TO WK2-ACCTNO.
      *    MOVE TP-LINE    TO WK2-LINE.
      *
      *    PERFORM WRITE-WK2-FILE.
      *    IF IO-FG NOT = 0
      *       GO TO IO-ERROR.
      *
      *    GO TO SORT-TR-NEXT.
      *
      *EXIT-SORT-ROUTINE.
      *    PERFORM CLOSE-WK2-FILE.
      *
      ******************************************************************
       DETAIL-LINE-PROCESS SECTION.
      ******************************************************************

           IF FIRST-TIME = "Y"
              MOVE EXT-CONAME-RPT-LINES TO LN-COUNT
              MOVE 1 TO LN-FEED
              MOVE "Y" TO RECORDS-FOUND
              MOVE "N" TO FIRST-TIME.

      * GET ACCTNO WITH STATUS 
           PERFORM GET-DISPLAY-LNNO.
           MOVE LPS-DISPLAY-LNNO TO D-DISPLAY-LNNO.

           PERFORM FIND-BORROWER.
           MOVE TP-MAKER TO D-MAKER.

           MOVE 0 TO D-SSNO.

           MOVE TP-DLNO TO D-DLNO.

           IF BR-STATE = "AL"
              MOVE "LOANDATE" TO D-LOANDATE-TEXT
              MOVE LN-LOANDATE TO DATE-YYYYMMDD
              PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
              MOVE DATE-MMDDYY TO D-LOANDATE
              MOVE "LOAN AMT" TO D-LNAMT-TEXT
              MOVE LN-LNAMT TO D-LNAMT.
 
           IF HOLD-TP-ACCTNO = TP-ACCTNO AND
               HOLD-TP-OTHBR-FG = TP-OTHBR-FG AND
                HOLD-TP-SSNO = TP-SSNO
              ADD 1 TO LINE-TO-PRINT
              MOVE SPACES TO DETAIL-LINE
            ELSE
              MOVE 1 TO LN-FEED LINE-TO-PRINT
              MOVE TP-ACCTNO TO HOLD-TP-ACCTNO
              MOVE TP-OTHBR-FG TO HOLD-TP-OTHBR-FG
              MOVE TP-SSNO TO HOLD-TP-SSNO
              IF PRINT-BW-INFO = "Y"
                 IF SUM-ONLY = "N"
                    MOVE "Y" TO FIRST-TIME-PRINT-ADDR  
                 END-IF
              END-IF 
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


      * ADDRESS, CITY, STATE,ZIP
           IF PRINT-BW-INFO  = "Y"
              IF FIRST-TIME-PRINT-ADDR = "Y"
                 PERFORM GET-BW-ADDRESS-INFO
                 MOVE 1 TO LN-FEED
                 PERFORM WRITE-DETAIL-LINE
                 MOVE "N" TO FIRST-TIME-PRINT-ADDR.

      * NEXT LINE

           MOVE 0 TO HOLD-EARNINGS.
           IF TP-SERVACCT = "N"
           COMPUTE HOLD-EARNINGS = TP-EARNINGS(1) + TP-EARNINGS(2) +
                                   TP-EARNINGS(3) + TP-EARNINGS(4) +
                                   TP-EARNINGS(5) + TP-EARNINGS(6)
            ELSE
              IF TP-TRCD NOT = "RC" AND NOT = "CC" AND
                          NOT = "RA" AND NOT = "CA" AND
                           NOT = "RO" AND NOT = "CO" AND
                            NOT = "RI" AND NOT = "CI" AND
                             NOT = "RS" AND NOT = "CS" AND
                              NOT = "RM" AND NOT = "CM" AND
                               NOT = "RD" AND NOT = "CD" AND
                                 NOT = "AP" AND NOT = "OV"
                     COMPUTE HOLD-EARNINGS = TP-APCUR + TP-APINT +
                                             TP-APLC - TP-DLPROC.
           MOVE HOLD-EARNINGS TO D-EARNINGS.


      * WORLD PR#995 PRINT EARNINGS ON LINE BY ITSELF AND PRINT
      *              ORIG AMOUNTS FOR EACH REFUND LINE
           IF HOLD-MS-STATE = "MS"
      * PRINT CURRENT LINE WITH JUST D-EARNINGS, START NEXT LINE
      * LINE-TO-PRINT WILL BE > 1 AFTER FIRST TP FOR EACH ACCOUNT GETS
      * PRINTED
               IF LINE-TO-PRINT = 1 AND SUM-ONLY = "N"
                  PERFORM WRITE-DETAIL-LINE
               END-IF
               PERFORM FIND-ORIGINAL-AMOUNTS.

           MOVE TP-PAYDATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO D-DATE.
           MOVE TP-IBPC TO D-IBPC.
         
      * DO NOT PRINT ANYTHING OVERTOP OF ORIG AMOUNTS FROM PR# 995 ABOVE
           IF NOT HOLD-MS-STATE = "MS"
              MOVE TP-DLPROC TO D-DLPROC
      *WORLD PR#288 REFUND OF INSURANCE TAX
              IF TP-TRCD = "RC" OR "RA" OR "RP" OR "R1" OR "R2" OR "R3"
                 IF TP-INS-TAX-RB NOT = ZERO AND
                    TP-INS-TAX-RB NOT = 2020202.02 AND
                    TP-INS-TAX-RB NOT = 0202020.20 
                    MOVE "T" TO D-TAX-REFUND-T 
                    MOVE TP-TRCD TO D-TAX-REFUND-INSTYPE 
                    MOVE TP-INS-TAX-RB TO D-DLPROC.


           MOVE TP-TILLNO TO D-TILL.

           IF ( HOLD-STATE NOT = "GA" )
              MOVE TP-TRCD TO D-TRCD
           ELSE
              MOVE TP-TRCD TO GA-TRCD
              PERFORM SET-GA-TRCD
              MOVE GA-TRCD TO D-TRCD-GA.

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
              ADD TP-APINT TP-APLC GIVING D-APINT.
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
                 IF TP-TRCD NOT = "RC" AND
                             NOT = "RA" AND
                              NOT = "RO" AND
                               NOT = "OV"
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

       END-DETAIL-LINE-PROCESS.
           EXIT.

      ************************************************
       FIND-ORIGINAL-AMOUNTS SECTION.
      ************************************************

      * LOAN AND SPR ARE READ FROM THE FIRST TP TO DISPLAY LOAN #
           PERFORM LOAN-CALCULATIONS.

           IF TP-TRCD = "RC"
              MOVE "LIFE" TO D-MS-TAG
              MOVE LN-INSPREM(1) TO D-EARNINGS
           ELSE
           IF TP-TRCD = "RA"
              MOVE "A&H " TO D-MS-TAG
              MOVE LN-INSPREM(2) TO D-EARNINGS
           ELSE
           IF TP-TRCD = "RP"
              MOVE "PROPERTY" TO D-MS-TAG
              MOVE LN-INSPREM(3) TO D-EARNINGS
           ELSE
           IF TP-TRCD = "R1"
              MOVE "OTHER 1" TO D-MS-TAG
              MOVE LN-INSPREM(4) TO D-EARNINGS
           ELSE
           IF TP-TRCD = "R2"
              MOVE "OTHER 2" TO D-MS-TAG
              MOVE LN-INSPREM(5) TO D-EARNINGS
           ELSE
           IF TP-TRCD = "R3"
              MOVE "OTHER 3" TO D-MS-TAG
              MOVE LN-INSPREM(6) TO D-EARNINGS
           ELSE
           IF TP-TRCD = "RS"
              MOVE "SERV CHG" TO D-MS-TAG
              MOVE LN-SERCHG TO D-EARNINGS
           ELSE
           IF TP-TRCD = "RI"
              MOVE "INTEREST" TO D-MS-TAG
              MOVE LN-INTCHG TO D-EARNINGS
           ELSE
              MOVE "LOAN" TO D-MS-TAG
              MOVE TOTAL-NOTE TO D-EARNINGS.

      ************************************************
       ADD-ORIGINAL-AMOUNTS SECTION.
      ************************************************

           IF TP-TRCD = "RC"
              ADD LN-INSPREM(1) TO WK-ORIG T-ORIG(LEV)
           ELSE
           IF TP-TRCD = "RA"
              ADD LN-INSPREM(2) TO WK-ORIG T-ORIG(LEV)
           ELSE
           IF TP-TRCD = "RP"
              ADD LN-INSPREM(3) TO WK-ORIG T-ORIG(LEV)
           ELSE
           IF TP-TRCD = "R1"
              ADD LN-INSPREM(4) TO WK-ORIG T-ORIG(LEV)
           ELSE
           IF TP-TRCD = "R2"
              ADD LN-INSPREM(5) TO WK-ORIG T-ORIG(LEV)
           ELSE
           IF TP-TRCD = "R3"
              ADD LN-INSPREM(6) TO WK-ORIG T-ORIG(LEV)
           ELSE
           IF TP-TRCD = "RS"
              ADD LN-SERCHG TO WK-ORIG T-ORIG(LEV)
           ELSE
           IF TP-TRCD = "RI"
              ADD LN-INTCHG TO WK-ORIG T-ORIG(LEV)
           ELSE
              ADD TOTAL-NOTE TO WK-ORIG T-ORIG(LEV).

       EXIT-ADD-ORIGINAL.
           EXIT.

      ************************************************
       PRINT-ORIGINAL-AMOUNT SECTION.
      ************************************************

           MOVE WK-ORIG TO DS-DLPROC.
           IF WK-TRCD = "RC"
              MOVE "LIFE" TO DS-MS-TAG
           ELSE
           IF WK-TRCD = "RA"
              MOVE "A&H " TO DS-MS-TAG
           ELSE
           IF WK-TRCD = "RP"
              MOVE "PROPERTY" TO DS-MS-TAG
           ELSE
           IF WK-TRCD = "R1"
              MOVE "OTHER 1" TO DS-MS-TAG
           ELSE
           IF WK-TRCD = "R2"
              MOVE "OTHER 2" TO DS-MS-TAG
           ELSE
           IF WK-TRCD = "R3"
              MOVE "OTHER 3" TO DS-MS-TAG
           ELSE
           IF WK-TRCD = "RS"
              MOVE "SERV CHG" TO DS-MS-TAG
           ELSE
           IF WK-TRCD = "RI"
              MOVE "INTEREST" TO DS-MS-TAG
           ELSE
              MOVE "LOAN" TO DS-MS-TAG.


      *******************************************************
       CREATE-WK SECTION.
      *******************************************************
       CREATE-WK-REC.
           PERFORM SETUP-WK-KEY.
           PERFORM READ-WK-FILE.
           IF IO-FG = 0
              GO TO CREATE-WK-UPDATE.
           PERFORM SETUP-WK-KEY.
           MOVE 0 TO WK-NO.
           MOVE 0 TO WK-TRAMT WK-INTDUE WK-APOTH WK-APINT WK-APLC
                     WK-APCUR WK-EARNINGS WK-DLPROC WK-ORIG.
           PERFORM WRITE-WK-FILE.
           IF IO-FG NOT = 0
              GO TO IO-ERROR.
           GO TO CREATE-WK-REC.
      *-------------------------------------------------------
       CREATE-WK-UPDATE.
           IF TP-TRAMT NOT < 0
              ADD 1 TO WK-NO
           ELSE
              SUBTRACT 1 FROM WK-NO.
           IF TP-OTHBR-FG = " " OR "C"
              IF TP-TRCD = "PY" OR "PO"
                                 OR "DF" OR "D2" OR "D3"
                                  OR "D4" OR "D5" OR "D6"
                                   OR "D7" OR "D8" OR "D9"
                                    OR "PR" OR "PC" OR "PP"
                                     OR "PL" OR "P2" OR "P3"
                                      OR "P4" OR "P5" OR "RP"
                                       OR "TS" OR "TP" OR "PA"
                                        OR "PE" OR "SS" OR "AH"
                                         OR "PN" OR "PZ" OR "IN"
                           OR (TP-TRCD = "OV" AND  OV-ISO-ONLY)

                 ADD TP-TRAMT TO WK-TRAMT T-TRAMT(LEV).

           IF TP-OTHBR-FG = " " OR "G"
              PERFORM ADD-ORIGINAL-AMOUNTS
              ADD TP-APOTH TO WK-APOTH T-APOTH(LEV)
              ADD TP-APINT TO WK-APINT T-APINT(LEV)
              ADD TP-APLC TO WK-APLC T-APLC(LEV)
              ADD TP-APCUR TO WK-APCUR T-APCUR(LEV)
              ADD HOLD-EARNINGS TO WK-EARNINGS T-EARNINGS(LEV)
              ADD TP-DLPROC TO WK-DLPROC T-DLPROC(LEV)
              IF TP-TRCD NOT = "JD"
                       AND NOT = "PI"
                 ADD TP-INTDUE TO WK-INTDUE T-INTDUE(LEV).

           PERFORM REWRITE-WK-FILE.
       EXIT-CREATE-WK.
           MOVE 0 TO SUB.
           PERFORM OUTPUT-GL 3 TIMES.
           IF TP-POCD NOT = "  "
              IF TP-OTHBR-FG = " " OR "G"
                   PERFORM CREATE-POCD-SUMMARY.

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

      **************************************************
       CREATE-POCD-SUMMARY SECTION.
      **************************************************
       CREATE-POCD-REC.
           PERFORM SETUP-WK-PO-KEY.
           PERFORM READ-WK-FILE.
           IF IO-FG = 0
              GO TO CREATE-PO-UPDATE.
           PERFORM SETUP-WK-PO-KEY.
           MOVE 0 TO WK-PO-NO.
           MOVE 0 TO WK-PO-TRAMT WK-PO-INTDUE WK-PO-APOTH WK-PO-APINT
                     WK-PO-APLC WK-PO-APCUR WK-PO-EARNINGS WK-PO-DLPROC.

           PERFORM WRITE-WK-FILE.

           IF IO-FG NOT = 0
              GO TO IO-ERROR.

           GO TO CREATE-POCD-REC.

       CREATE-PO-UPDATE.
           IF TP-TRAMT NOT < 0
              ADD 1 TO WK-PO-NO
           ELSE
              SUBTRACT 1 FROM WK-PO-NO.
           IF TP-OTHBR-FG = " " OR "C"
              IF TP-TRCD = "PY" OR "PO"
                                 OR "DF" OR "D2" OR "D3"
                                  OR "D4" OR "D5" OR "D6"
                                   OR "D7" OR "D8" OR "D9"
                                    OR "PR" OR "PC" OR "PP"
                                     OR "PL" OR "P2" OR "P3"
                                      OR "P4" OR "P5" OR "RP"
                                       OR "TS" OR "TP" OR "PA"
                                        OR "PE" OR "SS" OR "AH"
                                         OR "PN" OR "PZ" OR "IN"
                 ADD TP-TRAMT TO WK-PO-TRAMT.
           IF TP-OTHBR-FG = " " OR "G"
              ADD TP-APOTH TO WK-PO-APOTH
              ADD TP-APINT TO WK-PO-APINT
              ADD TP-APLC TO WK-PO-APLC
              ADD TP-APCUR TO WK-PO-APCUR
              ADD HOLD-EARNINGS TO WK-PO-EARNINGS
              ADD TP-DLPROC TO WK-PO-DLPROC
              IF TP-TRCD NOT = "JD"
                       AND NOT = "PI"
                 ADD TP-INTDUE TO WK-PO-INTDUE.


           PERFORM REWRITE-WK-FILE.

      ***************************
       FIND-BORROWER SECTION.
      ***************************
           IF TP-OTHBR-FG = "C"
              MOVE "INTER-OFFICE PAYMENT" TO D-NAME
              GO TO END-FIND-BORROWER.

      *     MOVE TP-SSNO TO HOLD-BW-SSNO.
      *     IF HOLD-BW-SSNO NOT = BW-SSNO
      *        MOVE HOLD-BW-SSNO TO BW-SSNO
      *      PERFORM READ-BW1-FILE.
      *      IF IO-FG = 9
      *            MOVE "** NO BORROWER MASTER **" TO D-NAME
      *            GO TO END-FIND-BORROWER.
 
      *  BW DATA RETRIEVE IN STORED PROCEDURE.  BW-NAME WILL = SPACES IF ROW NOT FOUND
           IF BW-FNAME = SPACES
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

      ***************************************
       GET-BW-ADDRESS-INFO SECTION.
      ***************************************
           MOVE BW-CITY TO C-S-Z.
           MOVE BW-STATE TO STATE.
           MOVE BW-ZIP TO ZIP.
           PERFORM CAT-CSZ.
           IF BW-ADR2 NOT = SPACES
              STRING BW-ADR1, " ", BW-ADR2, " ", DELIMITED BY "  "
                INTO D-BW-ADR
           ELSE
              MOVE BW-ADR1 TO D-BW-ADR.

           STRING D-BW-ADR, "   ", C-S-Z DELIMITED BY "     "
              INTO D-BW-ADR.

           IF BR-STATE = "AL"
              PERFORM LOAN-CALCULATIONS
              MOVE "AMT FINANCED" TO D-AMTFIN-TEXT
              MOVE DISCLOSED-FINAMT TO D-AMTFIN.


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
           IF HOLD-MS-STATE = "MS"
              MOVE "   ORIGINAL" TO SUM-MS-HEAD.
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

           IF ( HOLD-STATE NOT = "GA" )
              MOVE WK-TRCD TO DS-TRCD
           ELSE
              MOVE WK-TRCD TO GA-TRCD
              PERFORM SET-GA-TRCD
              MOVE GA-TRCD TO DS-TRCD-GA.

           MOVE WK-NO TO DS-NO.
           MOVE WK-TRAMT TO DS-TRAMT.
           MOVE WK-INTDUE TO DS-INTDUE.
           MOVE WK-APOTH TO DS-APOTH.
           MOVE WK-APINT TO DS-APINT.
           MOVE WK-APLC TO DS-APLC.
           MOVE WK-APCUR TO DS-APCUR.
           MOVE WK-EARNINGS TO DS-EARNINGS.
           IF HOLD-MS-STATE = "MS"
              PERFORM PRINT-ORIGINAL-AMOUNT
           ELSE
              MOVE WK-DLPROC TO DS-DLPROC.
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
           MOVE ZEROS TO TOTAL-TBL(2).
           MOVE "N" TO SUMMARY-FG.

      ********************************
       GRAND-TOTALS SECTION.
      ********************************

           IF ( GTT-ISO = "O" )
              GO TO END-GRAND-TOTALS.

           MOVE "Y" TO SUMMARY-FG.
           MOVE 3 TO LEV.
           MOVE "GRAND TOTALS :" TO DETAIL-LINE.
           PERFORM PRINT-SUMMARY.
       END-GRAND-TOTALS.
           MOVE ZEROS TO TOTAL-TBL(3).

           IF ( GTT-ISO NOT = "O" )
               MOVE "Y" TO SUMMARY-FG.

      ********************************
       GL-SUMMARY SECTION.
      ********************************
           MOVE GL-HEAD TO DETAIL-LINE.
           MOVE 4 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.
           MOVE LOW-VALUE TO WK-KEY.
           MOVE "G" TO WK-CODE.
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

      ********************************
       POCD-SUMMARY SECTION.
      ********************************
           IF ( GTT-ISO = "O" )
              GO TO END-POCD-SUMMARY.

           MOVE POCD-HEAD TO DETAIL-LINE.
           MOVE 4 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.
           MOVE 2 TO LN-FEED.
           MOVE LOW-VALUE TO WK-KEY.
           MOVE "P" TO WK-CODE.
           PERFORM START-WK-FILE.
       PRINT-NEXT-POCD.
           PERFORM READ-WK-FILE-NEXT.
           IF IO-FG NOT = 0
              GO TO END-POCD-SUMMARY.
           IF WK-CODE NOT = "P"
              GO TO PRINT-NEXT-POCD.

           MOVE "PO"       TO CD-TYPE.
           MOVE WK-PO-POCD TO CD-CODE DS-POCD.
           PERFORM READ-CD1-FILE.   *> EMBEDDED  CD
           IF IO-FG NOT = 0
              MOVE "INVALID " TO DS-DESC
           ELSE
              MOVE CD-DESC TO DS-DESC.
           MOVE WK-PO-NO TO DS-NO.
           MOVE WK-PO-TRAMT TO DS-TRAMT.
           MOVE WK-PO-INTDUE TO DS-INTDUE.
           MOVE WK-PO-APOTH TO DS-APOTH.
           MOVE WK-PO-APINT TO DS-APINT.
           MOVE WK-PO-APLC TO DS-APLC.
           MOVE WK-PO-APCUR TO DS-APCUR.
           MOVE WK-PO-EARNINGS TO DS-EARNINGS.
           MOVE WK-PO-DLPROC TO DS-DLPROC.
           PERFORM WRITE-DETAIL-LINE.
           GO TO PRINT-NEXT-POCD.
       END-POCD-SUMMARY.

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
      
           IF ( GTT-HEADER-FG = "Y" )
              MOVE GTT-HEAD1 TO PR-REC
              MOVE 2 TO LN-FEED
              PERFORM WRITE-PR-REC
              IF HOLD-MS-STATE = "MS"
                  MOVE "ORIGINAL   " TO GTT-HEAD2-MS
                  MOVE " AMOUNTS   " TO GTT-HEAD3-MS
              END-IF
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
           IF SUMMARY-FG = "N"
              MOVE HEAD5 TO PR-REC
              MOVE 2 TO LN-FEED
              PERFORM WRITE-PR-REC
              MOVE HEAD6 TO PR-REC
              MOVE 1 TO LN-FEED
              PERFORM WRITE-PR-REC
              IF HOLD-MS-STATE = "MS"
                 MOVE HEAD7 TO PR-REC
                 MOVE 1 TO LN-FEED
                 PERFORM WRITE-PR-REC
              END-IF
              MOVE 7 TO LN-COUNT
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
      
           MOVE "OV TRANSACTIONS" TO RANGE-LINE.
           MOVE OV-ISO               TO R-CHAR.
           PERFORM WRITE-DETAIL-LINE.
      
           MOVE "PRINT BORROWER INFO" TO RANGE-LINE.
           MOVE PRINT-BW-INFO TO R-CHAR.
           PERFORM WRITE-DETAIL-LINE.
      
           MOVE "GROUP TOTALS" TO RANGE-LINE.
           MOVE GTT-ISO        TO R-CHAR.
           PERFORM WRITE-DETAIL-LINE.

      **************************************************************
       CLASS-SCAN SECTION.
      **************************************************************
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
           ADD WK-INTDUE      TO  WS-GTT-INTDUE    ( GTT-TOTAL-SUB ).
           ADD WK-APOTH       TO  WS-GTT-APOTH     ( GTT-TOTAL-SUB ).
           ADD WK-APINT       TO  WS-GTT-APINT     ( GTT-TOTAL-SUB ).
           ADD WK-APLC        TO  WS-GTT-APLC      ( GTT-TOTAL-SUB ).
           ADD WK-APCUR       TO  WS-GTT-APCUR     ( GTT-TOTAL-SUB ).
           ADD WK-EARNINGS    TO  WS-GTT-EARNINGS  ( GTT-TOTAL-SUB ).
           ADD WK-DLPROC      TO  WS-GTT-DLPROC    ( GTT-TOTAL-SUB ).
           ADD WK-ORIG        TO  WS-GTT-ORIG      ( GTT-TOTAL-SUB ).

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
           MOVE WS-GTT-INTDUE    (GTT-TOTAL-SUB) TO DS-INTDUE.
           MOVE WS-GTT-APOTH     (GTT-TOTAL-SUB) TO DS-APOTH.
           MOVE WS-GTT-APINT     (GTT-TOTAL-SUB) TO DS-APINT.
           MOVE WS-GTT-APLC      (GTT-TOTAL-SUB) TO DS-APLC.
           MOVE WS-GTT-APCUR     (GTT-TOTAL-SUB) TO DS-APCUR.
           MOVE WS-GTT-EARNINGS  (GTT-TOTAL-SUB) TO DS-EARNINGS.
           IF HOLD-MS-STATE = "MS"
              MOVE WS-GTT-ORIG   (GTT-TOTAL-SUB) TO DS-DLPROC
           ELSE
              MOVE WS-GTT-DLPROC (GTT-TOTAL-SUB) TO DS-DLPROC. 

      * PRINT DETAIL AFTER LOAD 

           MOVE 1 TO LN-FEED.
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

           MOVE TP-BRNO   TO HOLD-LN-OWNBR.
           MOVE TP-ACCTNO TO HOLD-LN-ACCTNO.
      * LN-OWNBR WILL = 0 IF STORED PROCEDURE DOES NOT FIND LN REC
      * WITH JOIN FROM TPR
           IF LN-OWNBR = 0
              GO TO GET-DISPLAY-LNNO-EXIT.

      *     IF HOLD-LN1-KEY NOT = LN1-KEY
      *        MOVE HOLD-LN1-KEY TO LN1-KEY
      *        PERFORM READ-LN1-FILE
      *        IF ( IO-BAD )
      *           GO TO GET-DISPLAY-LNNO-EXIT.

      * INTER BRANCH PAYMENTS DO NOT EVER GET A SUCCESSFUL READ OF THE
      * LOAN FILE, AS WELL AS, IF IT IS THE 1ST READ OF THE LNFILE THE
      * LN-REC WILL BE SPACES.
      * IF IT IS UNSUCCESSFUL IN READING THE LNFILE AND/OR LN-REC IS
      * SPACES IT WILL RESULT IN 'MAJOR' SQL IO ERROR WINDOW BEING
      * DISPLAY DUE TO HAVING SPACES WITHIN NUMERIC SP1-KEY FIELDS.
      * THEREFORE, WE NEED TO EXIT PRIOR TO READING THE SPFILE OTHERWISE
      * THE DATABASE THROWS UP & STOPS EXECUTION (IN A15 IT WOULD
      * NOT STOP EXECUTION AND WOULD CONTINUE ONWARDS)
           IF LN-ORGST = SPACES
              GO TO GET-DISPLAY-LNNO-EXIT.

           MOVE LN-ORGST     TO SP-ORGST.
           MOVE LN-SPRCLASS  TO SP-SPRCLASS.
           MOVE LN-SUBCLASS  TO SP-SUBCLASS.
           MOVE LN-LAWCODE   TO SP-LAWCODE.
      *     IF LPWSSP-SP1-KEY NOT = SP1-KEY
      *        MOVE SP1-KEY TO LPWSSP-SP1-KEY
      *        PERFORM READ-SP1-FILE
      *        IF ( IO-BAD )
      *           GO TO GET-DISPLAY-LNNO-EXIT.

           MOVE SP1-KEY TO LPWSSP-SP1-KEY.
           PERFORM ARRAYSP-READ.
           IF ( IO-BAD )
             GO TO GET-DISPLAY-LNNO-EXIT.

           PERFORM LPSTAT-DISPLAY.

       GET-DISPLAY-LNNO-EXIT.
           EXIT.

      ******************************************************************
       OPEN-TRP-CURSOR SECTION.
      ******************************************************************

           MOVE BEGINNING-DATE      TO TP-DATE SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO QTRP1-WBEG-DATE.

           MOVE ENDING-DATE         TO  SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO QTRP1-WEND-DATE.

           MOVE BEG-CLASS           TO TP-CLASS
                                       QTRP1-WBEG-CLASS.
           MOVE END-CLASS           TO QTRP1-WEND-CLASS.

           PERFORM START-TRP1-FILE.   *> SSP - TRP BW BR LN

       COPY "LIBWI/GRTOTL.CPY".
       COPY "LIBGB/TIMEIO.CPY".
       COPY "LIBGB/GRWORK.CPY".
       COPY "LIBGB/GRSERCH.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/BRSECURE.CPY".
       COPY "LIBGB/EOSPOOL.CPY".
       COPY "LIBGB/GRTOTALS.CPY".

       COPY "LIBLP/LPMDTE.CPY".
       COPY "LIBLP/LPSTAT.CPY".
       COPY "LIBLP/LPAMTS.CPY".
       COPY "LIBLP/ARRAYSP.CPY".
	   
      ************************************
       FORM-RESET SECTION.
      ************************************
           MOVE EXT-CONAME-CONAME TO EMPORG-CONAME.
           MOVE EXT-CONAME-SYSDATE TO EMPORG-SYSDATE.
           DISPLAY EMPORG-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE EMPORG-FORM-FIELDS TO WR-BUF.
           MOVE EMPORG-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      ************************************
       SETUP-LINK-INFO SECTION.
      ************************************
           MOVE EMPORG-HELP--FILE TO HELP-LINK-FILE.
           MOVE EMPORG-HELP--DIR TO HELP-LINK-DIR.

      ************************************
       VDU-CONVERT-FIELD SECTION.
      ************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBLP/EMPORG_EVA.CPY".
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
           MOVE BR-NAME TO H-BRDESC.
      *-----------------------------------------------------------------
       SET-GA-TRCD.
           IF GA-TRCD = "RC" OR "CC"
              MOVE "LIFE" TO GA-TRCD
           ELSE
           IF GA-TRCD = "RA" OR "CA"
              MOVE "A&H" TO GA-TRCD
           ELSE
           IF GA-TRCD = "RP" OR "CP"
              MOVE "PROP" TO GA-TRCD
           ELSE
           IF GA-TRCD = "R1" OR "C1"
              MOVE "AUTO" TO GA-TRCD
           ELSE
           IF GA-TRCD = "R2" OR "C2"
              MOVE "OTH2" TO GA-TRCD
           ELSE
           IF GA-TRCD = "R3" OR "C3"
              MOVE "OTH3" TO GA-TRCD
           ELSE
           IF GA-TRCD = "RI" OR "CI"
              MOVE "INT" TO GA-TRCD
           ELSE
           IF GA-TRCD = "RS" OR "CS"
              MOVE "8-4%" TO GA-TRCD
           ELSE
           IF GA-TRCD = "RM" OR "CM"
              MOVE "MFEE" TO GA-TRCD.

      *---------------------------------------------------------------
       CAT-CSZ.
           INSPECT C-S-Z REPLACING FIRST "      " BY ", ++ +".
           INSPECT C-S-Z REPLACING FIRST "++" BY STATE.
           INSPECT C-S-Z REPLACING FIRST "+         " BY ZIP.

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

      *-----------------------------------------------------------
       SETUP-WK-GL1-KEY.
           MOVE SPACES TO WK-REC.
           MOVE "G" TO WK-CODE.
           MOVE TP-GLNO(SUB) TO WK-GLNO.
      *-----------------------------------------------------------
       SETUP-WK-PO-KEY.
           MOVE SPACES TO WK-REC.
           MOVE "P" TO WK-CODE.
           MOVE TP-POCD TO WK-PO-POCD.
      *-----------------------------------------------------------
       UPDATE-TABLE2.
           ADD T-TRAMT(SUB) TO T-TRAMT(LEV).
           ADD T-INTDUE(SUB) TO T-INTDUE(LEV).
           ADD T-APOTH(SUB) TO T-APOTH(LEV).
           ADD T-APINT(SUB) TO T-APINT(LEV).
           ADD T-APLC(SUB) TO T-APLC(LEV).
           ADD T-APCUR(SUB) TO T-APCUR(LEV).
           ADD T-EARNINGS(SUB) TO T-EARNINGS(LEV).
           ADD T-DLPROC(SUB) TO T-DLPROC(LEV).
           ADD T-ORIG(SUB) TO T-ORIG(LEV).
      *-----------------------------------------------------------
       TYPE-TOTALS.
           MOVE 1 TO LEV.
           MOVE "CLASS  TOTALS:" TO DETAIL-LINE.
           PERFORM PRINT-SUMMARY.
      *-----------------------------------------------------------
       PRINT-SUMMARY.
           MOVE 0 TO DS-NO.
           MOVE T-TRAMT(LEV) TO DS-TRAMT.
           MOVE T-INTDUE(LEV) TO DS-INTDUE.
           MOVE T-APOTH(LEV) TO DS-APOTH.
           MOVE T-APINT(LEV) TO DS-APINT.
           MOVE T-APLC(LEV) TO DS-APLC.
           MOVE T-APCUR(LEV) TO DS-APCUR.
           MOVE T-EARNINGS(LEV) TO DS-EARNINGS.
           IF HOLD-MS-STATE = "MS"
              MOVE T-ORIG(LEV) TO DS-DLPROC
           ELSE
              MOVE T-DLPROC(LEV) TO DS-DLPROC.
           MOVE 3 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      **********************************************************
       IO-ROUTINES SECTION.
      **********************************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBLP/LPTRPGS_SQL.CPY".
       COPY "LIBLP/LPSPGS_SQL.CPY".
       COPY "LIBLP/LPLNGS_SQL.CPY".
       COPY "LIBLP/LPLCGS_SQL.CPY".
       COPY "LIBLP/LPCDGS_SQL.CPY".
       COPY "LIBLP/LPBWGS_SQL.CPY".
       COPY "LIBGB/GBGRGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBPRINT.CPY".

       COPY "LIBGB/GBGR1RN.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBLP/LPLC1RN.CPY".
       COPY "LIBLP/LPSP1RN.CPY".
      * COPY "LIBLP/LPBW1RN.CPY".
      * COPY "LIBLP/LPLN1RN.CPY".

      * COPY "LIBLP/LPCD1RN.CPY".
      * COPYMEMBER: LIBLP/LPCD1RN
       LOAD-CD1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( CD-PATH-OVERRIDE = "Y" )
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( CD-PATH-OVERRIDE = SPACES )
              MOVE EXT-FILPATH-BASE        TO CD-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO CD-ALL-MACHINE
              MOVE CD-ALL-PATH             TO CD-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( CD-PATH-OVERRIDE = "B" )
              MOVE FILPATH-BASE            TO CD-ALL-BASE
              MOVE FILPATH-MACHINE         TO CD-ALL-MACHINE
              MOVE CD-ALL-PATH             TO CD-PATH.

      *-----------------------------------------------------------------
       OPEN-CD1-FILE.
           PERFORM LOAD-CD1-FILE.
           PERFORM OPEN-IT.
           MOVE CD-PATH-SQL TO E-FILE.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       READ-CD1-FILE.   *> EMBEDDED  CD
           PERFORM READ-IT.
           MOVE CD-PATH-SQL TO E-FILE.
           MOVE CD1-KEY TO E-KEYX.
           INITIALIZE QCD-REC.

           MOVE CD-TYPE    TO QCD-TYPE.
           MOVE CD-CODE    TO QCD-CODE.

           EXEC SQL
            SELECT CDFILE.CD_TYPE, 
                   CDFILE.CD_CODE, 
                   CDFILE.CD_DESC
            INTO :QCD-TYPE, 
                 :QCD-CODE, 
                 :QCD-DESC
            FROM DBO.CDFILE
            WHERE CDFILE.CD_TYPE = :QCD-TYPE
              AND CDFILE.CD_CODE = :QCD-CODE
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-CD1-FILE.

           IF ( IO-FG = 0 )
              PERFORM GET-CD-FIELDS.

      *-----------------------------------------------------------------
       CLOSE-CD1-FILE.
           PERFORM CLOSE-IT.


      * COPY "LIBLP/LPTRP1RN.CPY".
      * COPY "LIBLP/LPTRP1RN.CPY".
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
              MOVE TRP-ALL-PATH            TO TRP-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( TRP-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO TRP-ALL-BASE
              MOVE FILPATH-MACHINE         TO TRP-ALL-MACHINE
              MOVE FILPATH-DATA            TO TRP-ALL-DATA
              MOVE TRP-ALL-PATH            TO TRP-PATH.

      *-----------------------------------------------------------------
       OPEN-TRP1-FILE.
           PERFORM LOAD-TRP1-FILE.
           PERFORM OPEN-IT.
           MOVE TRP-PATH-SQL TO E-FILE.

           IF TRP1-CONN-OPEN-SW = "N" 
               EXEC SQL CONNECT TO :SQL-CONNECT-SERVER AS TRP1_CONNECT
                    USER :SQL-CONNECT-USERID USING :SQL-CONNECT-PSWD
               END-EXEC
               MOVE "Y" TO TRP1-CONN-OPEN-SW
               EXEC SQL SET CONNECTION C1 END-EXEC.

      *-----------------------------------------------------------------
       START-TRP1-FILE.   *> SSP - TRP BW BR LN
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
                 EXEC SSP_EMPORG_TRPFILE_SELECT
                  ( :PASS-BRANCHLIST,
                    :QTRP1-WBEG-DATE,  :QTRP1-WEND-DATE,
                    :QTRP1-WBEG-CLASS, :QTRP1-WEND-CLASS) 
                END-EXEC.

      *     EXEC SQL
      *      DECLARE TRP1_CURSOR CURSOR FOR
      *     SELECT TRPFILE.TP_BRNO,
      *            CAST(TRPFILE.TP_DATE AS VARCHAR(10)),
      *            TRPFILE.TP_CLASS,
      *            TRPFILE.TP_ACCTNO,
      *            TRPFILE.TP_LINE,
      *            TRPFILE.TP_SSNO,
      *            TRPFILE.TP_MAKER, 
      *            TRPFILE.TP_IBPC, 
      *            TRPFILE.TP_TRCD, 
      *            TRPFILE.TP_REFNO, 
      *            CAST(TRPFILE.TP_PAYDATE AS VARCHAR(10)), 
      *            TRPFILE.TP_TRAMT,
      *            TRPFILE.TP_INTDUE,
      *            TRPFILE.TP_APOTH,
      *            TRPFILE.TP_APINT,
      *            TRPFILE.TP_APLC,
      *            TRPFILE.TP_APCUR,
      *            TRPFILE.TP_GLNO_1,
      *            TRPFILE.TP_GLNO_2,
      *            TRPFILE.TP_GLNO_3,
      *            TRPFILE.TP_GLAMT_1,
      *            TRPFILE.TP_GLAMT_2,
      *            TRPFILE.TP_GLAMT_3,
      *            TRPFILE.TP_EARNINGS_1,
      *            TRPFILE.TP_EARNINGS_2,
      *            TRPFILE.TP_EARNINGS_3,
      *            TRPFILE.TP_EARNINGS_4,
      *            TRPFILE.TP_EARNINGS_5,
      *            TRPFILE.TP_EARNINGS_6,
      *            TRPFILE.TP_EARNINGS_7,
      *            TRPFILE.TP_EARNINGS_8,
      *            TRPFILE.TP_EARNINGS_9,
      *            TRPFILE.TP_EARNINGS_10,
      *            TRPFILE.TP_OTHBAL,
      *            TRPFILE.TP_INTBAL,
      *            TRPFILE.TP_LCBAL,
      *            TRPFILE.TP_CURBAL,
      *            CAST(TRPFILE.TP_PDTH_DATE AS VARCHAR(10)), 
      *            TRPFILE.TP_DLNO,
      *            TRPFILE.TP_DLPROC,
      *            TRPFILE.TP_SERVACCT, 
      *            TRPFILE.TP_TILLNO,
      *            CAST(TRPFILE.TP_LCPDTH_DATE AS VARCHAR(10)), 
      *            TRPFILE.TP_POSTING_BR,
      *            TRPFILE.TP_OTHBR_FG, 
      *            TRPFILE.TP_REV, 
      *            TRPFILE.TP_POCD, 
      *            TRPFILE.TP_MANUAL_RB, 
      *            TRPFILE.TP_UNITPER_CD, 
      *            TRPFILE.TP_INS_TAX_RB,
      *            ISNULL(BWFILE.BW_OWNBR,0),
      *            ISNULL(BWFILE.BW_SSNO,0),
      *            ISNULL(BWFILE.BW_LNAME,''),
      *            ISNULL(BWFILE.BW_FNAME,''),
      *            ISNULL(BWFILE.BW_ADR1,''),
      *            ISNULL(BWFILE.BW_ADR2,''),
      *            ISNULL(BWFILE.BW_CITY,''),
      *            ISNULL(BWFILE.BW_STATE,''),
      *            ISNULL(BWFILE.BW_ZIP,''),
      *            ISNULL(BWFILE.BW_SOLICIT,''),
      *            BRFILE.BR_NO,
      *            BRFILE.BR_NAME, 
      *            BRFILE.BR_STATE,
      *            BRFILE.BR_LOAN_NUM_FLAG, 
      *            BRFILE.BR_DATA_PATH, 
      *            BRFILE.BR_MACHINE, 
      *            BRFILE.BR_FORCE_VALID_CITY_COUNTY, 
      *            ISNULL(LNFILE.LN_OWNBR,0),
      *            LNFILE.LN_ACCTNO,
      *            LNFILE.LN_CLASS,
      *            LNFILE.LN_SUBCLASS,
      *            LNFILE.LN_LAWCODE,
      *            LNFILE.LN_ORGST,
      *            LNFILE.LN_SPRCLASS,
      *            LNFILE.LN_LOANTYPE,
      *            LNFILE.LN_STATFLAGS,
      *            CAST(LNFILE.LN_LOANDATE AS VARCHAR(10)),
      *            CAST(LNFILE.LN_ORIG_1STPYDATE AS VARCHAR(10)),
      *            CAST(LNFILE.LN_1STPYDATE AS VARCHAR(10)),
      *            LNFILE.LN_LNAMT,
      *            LNFILE.LN_INTCHG,
      *            LNFILE.LN_SERCHG,
      *            LNFILE.LN_EXTCHG,
      *            LNFILE.LN_MAINTFEE,
      *            LNFILE.LN_ORGTERM,
      *            LNFILE.LN_1STPYAMT,
      *            LNFILE.LN_NOREGPY,
      *            LNFILE.LN_REGPYAMT,
      *            LNFILE.LN_LASTPYAMT,
      *            LNFILE.LN_FEECODE_1,
      *            LNFILE.LN_FEECODE_2,
      *            LNFILE.LN_FEECODE_3,
      *            LNFILE.LN_FEECODE_4,
      *            LNFILE.LN_FEECODE_5,
      *            LNFILE.LN_FEECODE_6,
      *            LNFILE.LN_FEECODE_7,
      *            LNFILE.LN_FEECODE_8,
      *            LNFILE.LN_FEECODE_9,
      *            LNFILE.LN_FEECODE_10,
      *            LNFILE.LN_FEECODE_11,
      *            LNFILE.LN_FEECODE_12,
      *            LNFILE.LN_FEECODE_13,
      *            LNFILE.LN_FEECODE_14,
      *            LNFILE.LN_FEECODE_15,
      *            LNFILE.LN_FEECODE_16,
      *            LNFILE.LN_FEECODE_17,
      *            LNFILE.LN_FEECODE_18,
      *            LNFILE.LN_FEECODE_19,
      *            LNFILE.LN_FEECODE_20,
      *            LNFILE.LN_FEEAMT_1,
      *            LNFILE.LN_FEEAMT_2,
      *            LNFILE.LN_FEEAMT_3,
      *            LNFILE.LN_FEEAMT_4,
      *            LNFILE.LN_FEEAMT_5,
      *            LNFILE.LN_FEEAMT_6,
      *            LNFILE.LN_FEEAMT_7,
      *            LNFILE.LN_FEEAMT_8,
      *            LNFILE.LN_FEEAMT_9,
      *            LNFILE.LN_FEEAMT_10,
      *            LNFILE.LN_FEEAMT_11,
      *            LNFILE.LN_FEEAMT_12,
      *            LNFILE.LN_FEEAMT_13,
      *            LNFILE.LN_FEEAMT_14,
      *            LNFILE.LN_FEEAMT_15,
      *            LNFILE.LN_FEEAMT_16,
      *            LNFILE.LN_FEEAMT_17,
      *            LNFILE.LN_FEEAMT_18,
      *            LNFILE.LN_FEEAMT_19,
      *            LNFILE.LN_FEEAMT_20,
      *            LNFILE.LN_FEEOURS_1,
      *            LNFILE.LN_FEEOURS_2,
      *            LNFILE.LN_FEEOURS_3,
      *            LNFILE.LN_FEEOURS_4,
      *            LNFILE.LN_FEEOURS_5,
      *            LNFILE.LN_FEEOURS_6,
      *            LNFILE.LN_FEEOURS_7,
      *            LNFILE.LN_FEEOURS_8,
      *            LNFILE.LN_FEEOURS_9,
      *            LNFILE.LN_FEEOURS_10,
      *            LNFILE.LN_FEEOURS_11,
      *            LNFILE.LN_FEEOURS_12,
      *            LNFILE.LN_FEEOURS_13,
      *            LNFILE.LN_FEEOURS_14,
      *            LNFILE.LN_FEEOURS_15,
      *            LNFILE.LN_FEEOURS_16,
      *            LNFILE.LN_FEEOURS_17,
      *            LNFILE.LN_FEEOURS_18,
      *            LNFILE.LN_FEEOURS_19,
      *            LNFILE.LN_FEEOURS_20,
      *            LNFILE.LN_FEEFINCHG_1,
      *            LNFILE.LN_FEEFINCHG_2,
      *            LNFILE.LN_FEEFINCHG_3,
      *            LNFILE.LN_FEEFINCHG_4,
      *            LNFILE.LN_FEEFINCHG_5,
      *            LNFILE.LN_FEEFINCHG_6,
      *            LNFILE.LN_FEEFINCHG_7,
      *            LNFILE.LN_FEEFINCHG_8,
      *            LNFILE.LN_FEEFINCHG_9,
      *            LNFILE.LN_FEEFINCHG_10,
      *            LNFILE.LN_FEEFINCHG_11,
      *            LNFILE.LN_FEEFINCHG_12,
      *            LNFILE.LN_FEEFINCHG_13,
      *            LNFILE.LN_FEEFINCHG_14,
      *            LNFILE.LN_FEEFINCHG_15,
      *            LNFILE.LN_FEEFINCHG_16,
      *            LNFILE.LN_FEEFINCHG_17,
      *            LNFILE.LN_FEEFINCHG_18,
      *            LNFILE.LN_FEEFINCHG_19,
      *            LNFILE.LN_FEEFINCHG_20,
      *            LNFILE.LN_INSOURS_1,
      *            LNFILE.LN_INSOURS_2,
      *            LNFILE.LN_INSOURS_3,
      *            LNFILE.LN_INSOURS_4,
      *            LNFILE.LN_INSOURS_5,
      *            LNFILE.LN_INSOURS_6,
      *            LNFILE.LN_INSOURS_7,
      *            LNFILE.LN_INSOURS_8,
      *            LNFILE.LN_INSOURS_9,
      *            LNFILE.LN_INSOURS_10,
      *            LNFILE.LN_INSPREM_1,
      *            LNFILE.LN_INSPREM_2,
      *            LNFILE.LN_INSPREM_3,
      *            LNFILE.LN_INSPREM_4,
      *            LNFILE.LN_INSPREM_5,
      *            LNFILE.LN_INSPREM_6,
      *            LNFILE.LN_INSPREM_7,
      *            LNFILE.LN_INSPREM_8,
      *            LNFILE.LN_INSPREM_9,
      *            LNFILE.LN_INSPREM_10,
      *            LNFILE.LN_INSFINCHG_1,
      *            LNFILE.LN_INSFINCHG_2,
      *            LNFILE.LN_INSFINCHG_3,
      *            LNFILE.LN_INSFINCHG_4,
      *            LNFILE.LN_INSFINCHG_5,
      *            LNFILE.LN_INSFINCHG_6,
      *            LNFILE.LN_INSFINCHG_7,
      *            LNFILE.LN_INSFINCHG_8,
      *            LNFILE.LN_INSFINCHG_9,
      *            LNFILE.LN_INSFINCHG_10,
      *            LNFILE.LN_ANTICERN_1,
      *            LNFILE.LN_ANTICERN_2,
      *            LNFILE.LN_ANTICERN_3,
      *            LNFILE.LN_ANTICERN_4,
      *            LNFILE.LN_ANTICERN_5,
      *            LNFILE.LN_ANTICERN_6,
      *            LNFILE.LN_ANTICERN_7,
      *            LNFILE.LN_ANTICERN_8,
      *            LNFILE.LN_ANTICERN_9,
      *            LNFILE.LN_ANTICERN_10,
      *            LNFILE.LN_TOTNODEF,
      *            LNFILE.LN_CREDITLIM,
      *            LNFILE.LN_SUM_ORGTERM,
      *            LNFILE.LN_UNITPER_CD,
      *            LNFILE.LN_UNITPER_FREQ
      *
      *    FROM #TEMP1_BRANCH_GROUP_TBL T1
      *    INNER JOIN DBO.BRFILE BRFILE
      *      ON BRFILE.BR_NO = T1.BRNO
      *    INNER JOIN DBO.TRPFILE TRPFILE
      *      ON TRPFILE.TP_BRNO = BRFILE.BR_NO
      *    LEFT OUTER JOIN DBO.BWFILE BWFILE
      *      ON BWFILE.BW_OWNBR = TRPFILE.TP_BRNO 
      *     AND BWFILE.BW_SSNO = TRPFILE.TP_SSNO 
      *       WHERE TRPFILE.TP_DATE  BETWEEN @QTRP1_WBEG_DATE  AND @QTRP1_WEND_DATE
      *         AND TRPFILE.TP_CLASS BETWEEN @QTRP1_WBEG_CLASS AND @QTRP1_WEND_CLASS
      *    LEFT OUTER JOIN DBO.LNFILE LNFILE
      *      ON LNFILE.LN_OWNBR = TRPFILE.TP_BRNO
      *     AND LNFILE.LN_ACCTNO = TRPFILE.TP_ACCTNO
      *
      *   WHERE TRPFILE.TP_DATE   BETWEEN @QTRP1_WBEG_DATE  AND @QTRP1_WEND_DATE
      *    AND TRPFILE.TP_CLASS BETWEEN @QTRP1_WBEG_CLASS AND @QTRP1_WEND_CLASS
      *    AND TRPFILE.TP_TRCD IN ('PO','PB','SC','RN','RB','RO','RC','RA','RP',
      *                     'R1','R2','R3','RI','RS','RM','RD','PA','PE','SS',
      *                     'PR','PP','PC','OV','IN')
      *    AND (TRPFILE.TP_TRCD NOT IN ('PA','PE','SS','PR','PP','PC','IN') OR TRPFILE.TP_CURBAL = 0
      *        OR (TRPFILE.TP_APCUR * -1) = TRPFILE.TP_CURBAL)
      *
      *      ORDER BY T1.SEQ,
      *               TRPFILE.TP_BRNO,
      *               TRPFILE.TP_CLASS,
      *               TRPFILE.TP_ACCTNO,
      *               TRPFILE.TP_DATE,
      *               TRPFILE.TP_LINE
      *     END-EXEC.

           EXEC SQL
               OPEN TRP1_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE ZEROES TO QTRP1-WBEG-CLASS
                          QTRP1-WEND-CLASS
           MOVE SPACES TO QTRP1-WBEG-DATE
                          QTRP1-WEND-DATE.

           MOVE STAT---GOOD TO TRP1-CURSOR-STAT.

           EXEC SQL SET CONNECTION C1 END-EXEC.

      *-----------------------------------------------------------------
       READ-TRP1-FILE-NEXT.   *> EMBEDDED - TRP BW BR LN
           PERFORM READ-IT.
           MOVE TRP-PATH-OWNBR  TO TP-BRNO.
           MOVE TRP-PATH-SQL    TO E-FILE.
           MOVE TRP1-KEY        TO E-KEYX.
           INITIALIZE QTRP-REC QBW-REC QBR-REC QLN-REC.

           EXEC SQL SET CONNECTION TRP1_CONNECT END-EXEC.

           IF ( TRP1-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH TRP1_CURSOR INTO 
                   :QTP-BRNO,
                   :QTP-DATE,
                   :QTP-CLASS,
                   :QTP-ACCTNO,
                   :QTP-LINE,
                   :QTP-SSNO,
                   :QTP-MAKER, 
                   :QTP-IBPC, 
                   :QTP-TRCD, 
                   :QTP-REFNO, 
                   :QTP-PAYDATE,
                   :QTP-TRAMT,
                   :QTP-INTDUE,
                   :QTP-APOTH,
                   :QTP-APINT,
                   :QTP-APLC,
                   :QTP-APCUR,
                   :QTP-GLNO-1,
                   :QTP-GLNO-2,
                   :QTP-GLNO-3,
                   :QTP-GLAMT-1,
                   :QTP-GLAMT-2,
                   :QTP-GLAMT-3,
                   :QTP-EARNINGS-1,
                   :QTP-EARNINGS-2,
                   :QTP-EARNINGS-3,
                   :QTP-EARNINGS-4,
                   :QTP-EARNINGS-5,
                   :QTP-EARNINGS-6,
                   :QTP-EARNINGS-7,
                   :QTP-EARNINGS-8,
                   :QTP-EARNINGS-9,
                   :QTP-EARNINGS-10,
                   :QTP-OTHBAL,
                   :QTP-INTBAL,
                   :QTP-LCBAL,
                   :QTP-CURBAL,
                   :QTP-PDTH-DATE,
                   :QTP-DLNO,
                   :QTP-DLPROC,
                   :QTP-SERVACCT, 
                   :QTP-TILLNO,
                   :QTP-LCPDTH-DATE,
                   :QTP-POSTING-BR,
                   :QTP-OTHBR-FG, 
                   :QTP-REV, 
                   :QTP-POCD, 
                   :QTP-MANUAL-RB, 
                   :QTP-UNITPER-CD, 
                   :QTP-INS-TAX-RB,
                   :QBW-OWNBR, 
                   :QBW-SSNO, 
                   :QBW-LNAME, 
                   :QBW-FNAME, 
                   :QBW-ADR1, 
                   :QBW-ADR2, 
                   :QBW-CITY, 
                   :QBW-STATE, 
                   :QBW-ZIP, 
                   :QBW-SOLICIT, 
                   :QBR-NO,
                   :QBR-NAME, 
                   :QBR-STATE,
                   :QBR-LOAN-NUM-FLAG, 
                   :QBR-DATA-PATH, 
                   :QBR-MACHINE, 
                   :QBR-FORCE-VALID-CITY-COUNTY,
                   :QLN-OWNBR,
                   :QLN-ACCTNO,
                   :QLN-CLASS,
                   :QLN-SUBCLASS,
                   :QLN-LAWCODE,
                   :QLN-ORGST,
                   :QLN-SPRCLASS,
                   :QLN-LOANTYPE,
                   :QLN-STATFLAGS,
                   :QLN-LOANDATE ,
                   :QLN-ORIG-1STPYDATE ,
                   :QLN-1STPYDATE ,
                   :QLN-LNAMT,
                   :QLN-INTCHG,
                   :QLN-SERCHG,
                   :QLN-EXTCHG,
                   :QLN-MAINTFEE,
                   :QLN-ORGTERM,
                   :QLN-1STPYAMT,
                   :QLN-NOREGPY,
                   :QLN-REGPYAMT,
                   :QLN-LASTPYAMT,
                   :QLN-FEECODE-1,
                   :QLN-FEECODE-2,
                   :QLN-FEECODE-3,
                   :QLN-FEECODE-4,
                   :QLN-FEECODE-5,
                   :QLN-FEECODE-6,
                   :QLN-FEECODE-7,
                   :QLN-FEECODE-8,
                   :QLN-FEECODE-9,
                   :QLN-FEECODE-10,
                   :QLN-FEECODE-11,
                   :QLN-FEECODE-12,
                   :QLN-FEECODE-13,
                   :QLN-FEECODE-14,
                   :QLN-FEECODE-15,
                   :QLN-FEECODE-16,
                   :QLN-FEECODE-17,
                   :QLN-FEECODE-18,
                   :QLN-FEECODE-19,
                   :QLN-FEECODE-20,
                   :QLN-FEEAMT-1,
                   :QLN-FEEAMT-2,
                   :QLN-FEEAMT-3,
                   :QLN-FEEAMT-4,
                   :QLN-FEEAMT-5,
                   :QLN-FEEAMT-6,
                   :QLN-FEEAMT-7,
                   :QLN-FEEAMT-8,
                   :QLN-FEEAMT-9,
                   :QLN-FEEAMT-10,
                   :QLN-FEEAMT-11,
                   :QLN-FEEAMT-12,
                   :QLN-FEEAMT-13,
                   :QLN-FEEAMT-14,
                   :QLN-FEEAMT-15,
                   :QLN-FEEAMT-16,
                   :QLN-FEEAMT-17,
                   :QLN-FEEAMT-18,
                   :QLN-FEEAMT-19,
                   :QLN-FEEAMT-20,
                   :QLN-FEEOURS-1,
                   :QLN-FEEOURS-2,
                   :QLN-FEEOURS-3,
                   :QLN-FEEOURS-4,
                   :QLN-FEEOURS-5,
                   :QLN-FEEOURS-6,
                   :QLN-FEEOURS-7,
                   :QLN-FEEOURS-8,
                   :QLN-FEEOURS-9,
                   :QLN-FEEOURS-10,
                   :QLN-FEEOURS-11,
                   :QLN-FEEOURS-12,
                   :QLN-FEEOURS-13,
                   :QLN-FEEOURS-14,
                   :QLN-FEEOURS-15,
                   :QLN-FEEOURS-16,
                   :QLN-FEEOURS-17,
                   :QLN-FEEOURS-18,
                   :QLN-FEEOURS-19,
                   :QLN-FEEOURS-20,
                   :QLN-FEEFINCHG-1,
                   :QLN-FEEFINCHG-2,
                   :QLN-FEEFINCHG-3,
                   :QLN-FEEFINCHG-4,
                   :QLN-FEEFINCHG-5,
                   :QLN-FEEFINCHG-6,
                   :QLN-FEEFINCHG-7,
                   :QLN-FEEFINCHG-8,
                   :QLN-FEEFINCHG-9,
                   :QLN-FEEFINCHG-10,
                   :QLN-FEEFINCHG-11,
                   :QLN-FEEFINCHG-12,
                   :QLN-FEEFINCHG-13,
                   :QLN-FEEFINCHG-14,
                   :QLN-FEEFINCHG-15,
                   :QLN-FEEFINCHG-16,
                   :QLN-FEEFINCHG-17,
                   :QLN-FEEFINCHG-18,
                   :QLN-FEEFINCHG-19,
                   :QLN-FEEFINCHG-20,
                   :QLN-INSOURS-1,
                   :QLN-INSOURS-2,
                   :QLN-INSOURS-3,
                   :QLN-INSOURS-4,
                   :QLN-INSOURS-5,
                   :QLN-INSOURS-6,
                   :QLN-INSOURS-7,
                   :QLN-INSOURS-8,
                   :QLN-INSOURS-9,
                   :QLN-INSOURS-10,
                   :QLN-INSPREM-1,
                   :QLN-INSPREM-2,
                   :QLN-INSPREM-3,
                   :QLN-INSPREM-4,
                   :QLN-INSPREM-5,
                   :QLN-INSPREM-6,
                   :QLN-INSPREM-7,
                   :QLN-INSPREM-8,
                   :QLN-INSPREM-9,
                   :QLN-INSPREM-10,
                   :QLN-INSFINCHG-1,
                   :QLN-INSFINCHG-2,
                   :QLN-INSFINCHG-3,
                   :QLN-INSFINCHG-4,
                   :QLN-INSFINCHG-5,
                   :QLN-INSFINCHG-6,
                   :QLN-INSFINCHG-7,
                   :QLN-INSFINCHG-8,
                   :QLN-INSFINCHG-9,
                   :QLN-INSFINCHG-10,
                   :QLN-ANTICERN-1,
                   :QLN-ANTICERN-2,
                   :QLN-ANTICERN-3,
                   :QLN-ANTICERN-4,
                   :QLN-ANTICERN-5,
                   :QLN-ANTICERN-6,
                   :QLN-ANTICERN-7,
                   :QLN-ANTICERN-8,
                   :QLN-ANTICERN-9,
                   :QLN-ANTICERN-10,
                   :QLN-TOTNODEF,
                   :QLN-CREDITLIM,
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
              PERFORM GET-BW-FIELDS
              PERFORM GET-BR-FIELDS
      * QLN-OWNBR WILL = 0 IF LN REC NOT FOUND IN QUERY
              IF QLN-OWNBR= 0 
                  INITIALIZE LN-REC
              ELSE
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


       COPY "LIBGB/GRBRLS_SQL.CPY".
       COPY "LIBGB/GBWKIN.CPY".
      * COPY "LIBGB/GBWK2IN.CPY".
      *--------------------------------------------------------
       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO EXT-EOBUF-ERRCD.

      *********************************************************
       END-PROGRAM SECTION.
      **********************************************************
       END-RUN.
           PERFORM CLOSE-FILES.

           IF EXT-ROUTE-BUF = "00"
              PERFORM PRU-OUT.

           IF NO-RECORDS-FOUND
              MOVE "NO RECORDS FOUND FOR SELECTED RANGE" TO MESS
              PERFORM SEND-MESS.

           MOVE MKTEMP-BUF TO ACCESS-BUF.
           PERFORM REMOVE-WORKFILE.
      *    MOVE WK2-PATH TO ACCESS-BUF.
      *    PERFORM REMOVE-WORKFILE.

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
              DESTROY EMPORG-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.
           EXIT PROGRAM.
