       IDENTIFICATION DIVISION.
       PROGRAM-ID.   EMTRR2.
       AUTHOR.          MJD.
       DATE-WRITTEN. 081596.
      *****************************************************************
      *
      *                      (LP)
      *    DESCRIPTION:  CASH DRAWER TRANSACTION JOURNAL
      *
      * REV:
      *  MJD 081596 COPIED EMTRRG.C TO EMTRR2.C (NEW PROGRAM)
      *  MJD 081596 ADDED OPTION TO DISPLAY IN POSTED TIME ORDER. IF
      *             TIME ORDER IS SELECTED, THEN THE DATE AND TIME OF
      *             TRANSACTION WILL BE DISPLAYED.
      *  MJD 081696 REMOVED DATE BRAEKS AND TOTALS.
      *  BLV 100896 FIX GL1 KEY TO MATCH WK-KEY (REDEFINED RECORD)
      *  JTG 060497 ADDED TEST FOR "R3"
      *  BLV 061197 CHANGED FOR LOCAL SPOOL WITH NEW BATCH DRIVER - BREXEC
      *  KEC 091797 FIXED THE GROUP-TOTAL-TABLE (GTT) ROUTINE SO TO
      *              CALCULATE THE CORRECT TOTALS.
      *              REVISED THE WK-KEY TO WK-GTT-KEY.    PL#0144
      *  GLF 022098 DELETED BR-SECURITY-OK AND ADDED LIBGB/BRSECUREW.
      *  BAH 030698 CHANGED WK2-LINE TO PIC 9(3) FROM 99 BECAUSE TP-LINE
      *             PIC 99 COMP ACTUALLY STORES A 3 DIGIT NUMBER !.
      *  BAH 062598 CHANGED TP-TILL PIC 9 TO TP-TILLNO PIC X
      *  GLF 043099 WAS MOVING SPACES TO DETAIL-LINE ON SUMMARY ONLY IN
      *             WRONG SPOT CAUSING LINES TO BE MISSING, IS NOW FIXED
      *  BAH 990618 ADDED "ST"
      *  GLF 081999 ADD ARCHIVE PATH INFO. FOR TR FILES NOT FOUND IN DATA
      *             SET FOR SELECTED MONTH AND YEAR
      *  BLV 990826 FIX ARCHIVE PATH LOGIC
      *  KEC 001002 {PR#00000} MERCURY FINANCE [CINDY]
      *              CHANGED WK2-LINE FROM 9(3) TO 9(2) COMP JUST LIKE
      *              TP-LINE; BETH ORIGINALLY MADE THE CHANGE BUT HAD
      *              GOT CAUGHT IN LP/EMTRRG SO CHANGED TO 9(2) COMP.
      *  CS  020501  ADDED OPTION FOR REFCODE, INTERBRANCH-ISO, OPTION TO SKIP
      *              PRINTING OF TOTALS AT TILL/ BRANCH & PRODUCE ONLY GRAND
      *              TOTAL & A GRAND TOTAL BY TRAN CODE. [PR#1779 REGACC]
      *  CS  020502  WHEN RUNNING FOR GROUP, TILL TOTALS & BRANCH TOTALS WERE
      *              DOUBLED(BOTH "L" & "T" RECORDS WERE UPDATING TOTALS TABLE
      *  CS  020502  FIXED UP-KEY TO USER-ID.
      *  CS  020502  FIXED DISPLAY WHEN RUNNING IN BATCH
      *  CS  020502  ADDED TEST IF DRAWER-TOTALS-SPACE, INTERBR-SPACE & REFNO
      *              = SPACES, SKIP REFERENCE CHECK. ALSO CHANGED TEST ON
      *              DRAWER-TOTALS-YES TO BE DRAWER-TOTALS-YES OR SPACE SO
      *              PAGE 2 PARAMETERS WOULD NOT NEED TO BE ENTERED IN BATCH.
      * BAH 020508 FIXED REDEFS OF 01 WK-REC'S CAUSING WRITE ERROR 22 ON WK
      * CS  020624  CORRECTED BUG IN RECAP-TRCD-TOTALS SECTION, WAS PRINTING
      *             "R" CODES AS WELL AS "T"(GROUP TOTALS) CODES WHEN RUNNING
      *             FOR GROUP RANGE
      * CS  020801 ADD OPTION TO SKIP PAGE BREAK ON CHANGE OF TILL OR BRANCH
      *            (SKIP-PAGE-BREAKS-ON-SUMMARY) [PR#1839 REGACC]
      * CS  020806 REVISED BEHAVIOR IF UP-KEY FROM ACCEPT OR QUESTION
      *            ON SKIPPING BREAKS. WAS BLANKING QUESTION & ANSWER
      * CS  020807 FIXED THE LEGEND TO MATCH FIELD [SKIP PAGE BREAK ON SUMMARY]
      * CS  020827 ABOVE CHANGE(020801) CAUSED THE TOTALS NOT TO PRINT FOR OTHER
      *            BRANCH PAYMENTS.
      * CS  020828 PER KEVIN, IF DRAWER TOTALS = N (REGIONAL) CHANGE WORDING
      *            ON INTERBRANCH PAYMENTS TO BE "TO" & BY"
      * BAH 021004 PRINT TOTAL COUNT (NO OF TRANSACTIONS) FOR GRAND TOTALS
      *            AND ALLOW CASH DRAWER OF 0 FOR ALLOTMENTS, ADDED A F3-FLAG
      *            SO EXISTING BATCHES WONT BE EMPTY LOOKING FOR 0...
      *            REGACC PL# 375
      *  BAH 021008 FIXED RECORDS-FOUND TO NOT GIVE "NO RECORDS FOUND"
      *             ON A F7 EXIT
      *  BAH 021014 ADDED NEW TRANSACTION CODE OF "IN" INSURANCE LOSS, EXACTLY
      *             LIKE "PE", REGACC PR# 0007
      *  CS  021107 REGACC DIDNT LIKE TO-BY SO CHANGED TO JUST SAY "BR" SO
      *             INTERBR PMTS ARE IDENTIFIED
      *  CS  021217 TOTAL COUNTS (DS-NO) WERE LIMITED TO 999 CHANGED TO 9(6)
      *             [KEVIN PER REGIONAL]
      *  BAH 031024 INCLUDE "OV" IN TRAMT TOTALS, IT INCREASES THE CASH
      *             DRAWER...MIDA PL# 423
      *  BAH 031125 REVERSALS OF "OV" DO NOT HAVE "CHECK" IN THE REF BUT
      *             THE ORIG "OV" DOES, SO THEY DONT WASH OUT OF THE
      *             "REB CHECKS" TOTAL COLUMN.., MIDA PL# 428
      *  BLM 080812 IF F3 NOT SELECTED FOR ALL REFNO'S, REVERSALS DON'T
      *             PRINT (REFNO'S DON'T CARRY OVER TO REVERSAL TR'S).
      *             SO READ LP TO GET REFNO IF TR IS A REVERSAL & ALL
      *             REFNO'S NOT SELECTED.  FIRSTLINE, PR# 3242
      *  BLM 100305 ADD ESCROW TRANSACTION TYPES (ED, EP, ES) TO
      *             TRAMT TOTALS, REGWAN PR# 3506
      *  BAH 140527 "OV" DONE WITH A BATCH REFNO HAVE "G" IN OTHBR-FG
      *             AND WERE NOT ADDING TRAMT INTO TOTALS, SO WE ADDED
      *             A HARD CODED TEST IN HERE INSTEAD OF MESSING WITH
      *             LONPF2 SETTING THE "G" (TOO SCARED), PL# 848 MIDATL
      *
      * KEC 2016.1025 {PD#00003} PARADATA <A30> [JAY CISZEWSKI]
      *         CHANGED SAVE-TRP1-KEY AND TEST-TRP1-KEY TO HAVE A
      *         PIC X(27) FOR QUICKER VERIFICATION.
      *
      * KEC 2017.0103 {PR#00000} PARADATA <A30> [JAY CISZEWSKI]
      *          CHANGED DS- FIELDS TO BE D4- & D5- FIELDS.
      *          THE DS FIELDS WERE CONFUSING WHEN SEARCHING BECAUSE
      *          THEY WERE NOT FIELDS FROM THE DS FILE BUT WERE
      *          DETAIL LINE FIELDS.
      *
      *  BAH 170530 ACTIVATE 8 DIGIT DATES, PD0006
      * BAH 20190919 REMOVED ACCESS-CALL ON TRPFILE
      * BAH 20200423 WK-GTT-KEY WAS LARGER THAN WK-KEY, REMOVED 2 FILLER,
      *              CAUSING A WRITE-ERROR 22, WK-GL1-KEY WAS SHORT 2 BYTES
      * BLM 20210810 REMOVE TP-CODE, TP-POOLID NO LONGER IN KEY
      * BAH 2023.1005 CHANGED SCREEN FIELD USERID TO MUST Y TO ALLOW
      *               LOWERCASE
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

      *=================================================================
      *
      *    W K - F I L E
      *
      *=================================================================
       FD  WK-FILE
           LABEL RECORDS ARE STANDARD.
       01  WK-REC.
           03  WK-KEY.
               05  WK-CODE    PIC X.
               05  WK-TILLNO  PIC 9(3).
               05  WK-DATE    PIC 9(8).
               05  WK-BRNO    PIC 9(5).
               05  WK-CLASS   PIC 9(3).
               05  WK-TRCD    PIC XX.
           03  WK-NO          PIC S9(7)  COMP-3.
           03  WK-TRAMT       PIC S9(9)V99  COMP-3.
           03  WK-INTDUE      PIC S9(9)V99  COMP-3.
           03  WK-APOTH       PIC S9(9)V99  COMP-3.
           03  WK-APINT       PIC S9(9)V99  COMP-3.
           03  WK-APLC        PIC S9(9)V99  COMP-3.
           03  WK-APCUR       PIC S9(9)V99  COMP-3.
           03  WK-CHECKS      PIC S9(9)V99  COMP-3.
           03  WK-EARNINGS    PIC S9(9)V99  COMP-3.
           03  WK-DLPROC      PIC S9(9)V99  COMP-3.

      *-----------------------------------------------------------------
      *    WORK RECORD FOR GROUP TOTALS
      *-----------------------------------------------------------------
       01  WK-GTT-REC.
           03  WK-GTT-KEY.
               05  WK-GTT-CODE              PIC X.
               05  WK-GTT-BRNO              PIC 9(4).
               05  WK-GTT-TILLNO            PIC 99.
               05  WK-GTT-DATE              PIC 9(8).
               05  WK-GTT-CLASS             PIC 9(3).
               05  WK-GTT-TRCD              PIC XX.
               05  FILLER                   PIC X(2).
           03  FILLER                       PIC X(58).

      *-----------------------------------------------------------------
       01  WK-GL-REC.
           03  WK-GL1-KEY.
               05  FILLER     PIC X.
               05  WK-GLNO    PIC 9(11)     COMP-3.
               05  FILLER     PIC X(15).
           03  WK-GLAMT       PIC S9(7)V99  COMP-3.
           03  FILLER         PIC X(53).

      *****************************************************************
      *  INDEXED TRFILE BY DATE, TIME, BRANCH, CLASS, ACCOUNT
      *****************************************************************
       FD  WK2-FILE
           LABEL RECORDS ARE STANDARD.
       01  WK2-REC.
           03  WK2-KEY.
               05  WK2-TILLNO            PIC 99.
               05  WK2-POSTDATE          PIC 9(8).
               05  WK2-POSTTIME          PIC 9(6).
               05  WK2-USERID            PIC X(8).
               05  WK2-BRANCH            PIC 9(4).
               05  WK2-CLASS             PIC 9(3).
               05  WK2-ACCTNO            PIC 9(8).
               05  WK2-DATE              PIC 9(8).
      *        05  WK2-CODE              PIC X.
               05  WK2-LINE              PIC 9(3).

      *****************************************************************
      *
      *    W O R K I N G - S T O R A G E    S E C T I O N
      *
      *****************************************************************
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(43) VALUE
           "@(#)./LP/EMTRR2.CBL S34 08/11/21-12:20:01 >".

      *****************************************************************
      *
      *     F I L E   I F N
      *
      *****************************************************************
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
       COPY "LIBLP/LP01LP.CPY".
       COPY "LIBLP/LP01LP_SQL.CPY".
       COPY "LIBLP/LPWSLP.CPY".
      *-----------------------------------------------------------------
       01  PR-IFN               PIC X(2)     VALUE "PR".
       01  WK2-IFN              PIC X(3)     VALUE "WK2".
       01  WK-IFN               PIC X(2)     VALUE "WK".
       01  WK2-PATH             PIC X(35).

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
       01  SKIP-BREAK-ANSWER-BUF     PIC X(01) VALUE SPACES.
       01  SKIP-BREAK-ANSWER-LIST    PIC X(12) VALUE "SKIP-BREAKS.".
       01  SKIP-BREAK-PROMPT-BUF     PIC X(27) VALUE SPACES.
       01  SKIP-BREAK-PROMPT-LIST    PIC X(13) VALUE "BREAK-PROMPT.".

       01  HOLD-WK-CODE         PIC X.
       01  HOLD-EARNINGS        PIC S9(7)V99  COMP-3 VALUE 0.
       01  HOLD-POSTTIME        PIC X(6).
       01  HOLD-POSTTIMEX REDEFINES HOLD-POSTTIME.
           03  HOLD-HR-MIN      PIC X(4).
           03  FILLER           PIC X(2).
       01  SUB                  PIC 9(2)      COMP-3.
      *--> SUB1 AND SUB2 USED FOR CLEARING PRINTING TOTAL TABLE.
       01  SUB1                 PIC 9(2)      COMP-3.
       01  SUB2                 PIC 9(2)      COMP-3.
       01  REFNO-SUB            PIC 9         COMP-3.
       01  LEV                  PIC 99        COMP-3.
       01  HOLD-BRNO            PIC 9(4)  VALUE 0.
       01  HOLD-CLASS           PIC 9(3)  VALUE 0.
       01  HOLD-DATE            PIC 9(8)  VALUE 0.
       01  HOLD-TILLNO          PIC 99    VALUE 0.
       01  RECORDS-FOUND        PIC X     VALUE " ".
           88  NO-RECORDS-FOUND           VALUE "N".
       01  SUMMARY-FG           PIC X     VALUE "N".
       01  PRINT-STATUS         PIC X(6).
       01  NAME-WORKER          PIC X(34).
       01  FIRST-NAME           PIC X(16).

       01  FIRST-TIME           PIC X        VALUE "Y".
       01  MSEL-LIST            PIC X(5)     VALUE "MSEL.".
       01  MESS-LIST            PIC X(5)     VALUE "MESS.".
       01  SAVE-TP-REC-FG       PIC X VALUE SPACES.

       01  TEST-TRP1-KEY         PIC X(26)    VALUE LOW-VALUES.
       01  FILLER               REDEFINES TEST-TRP1-KEY.
           03  TEST-TRP1-KEY-MAIN.
               05  TEST-TRP1-BRNO             PIC 9(4).
               05  TEST-TRP1-DATE             PIC 9(8).
               05  TEST-TRP1-CLASS            PIC 9(3).
               05  TEST-TRP1-ACCTNO           PIC 9(8).
           03  TEST-TRP1-LINE                 PIC 9(3).

       01  SAVE-TRP1-KEY         PIC X(26)    VALUE LOW-VALUES.
       01  FILLER               REDEFINES SAVE-TRP1-KEY.
           03  SAVE-TRP1-KEY-MAIN.
               05  SAVE-TRP1-DATE             PIC 9(8).
               05  SAVE-TRP1-BRNO             PIC 9(4).
               05  SAVE-TRP1-CLASS            PIC 9(3).
               05  SAVE-TRP1-ACCTNO           PIC 9(8).
           03  SAVE-TRP1-LINE                 PIC 9(3).

       01  CLASS-FG                          PIC X(3)   VALUE "   ".
           88  CLASS-BEG                                VALUE "BEG".
           88  CLASS-END                                VALUE "END".

       01  REFNO-MATCH                       PIC X.
           88 REFNO-MATCH-NO                 VALUE "N".
           88 REFNO-MATCH-YES                VALUE "Y".

       01  TEST-TP-REFNO.
           03  TEST-TP-REFNO-CHAR            PIC X  OCCURS 5 TIMES.
       01  TEST-REFNO.
           03  TEST-REFNO-CHAR               PIC X  OCCURS 5 TIMES.

      *-----------------------------------------------------------------
       01  MESSAGE-01-LOC-PRT   PIC X(43)    VALUE
           "CANNOT DO LOCAL SPOOLING WITH GROUP TOTALS".

       01  UPDATE-WORKERS.
           03  UPDATE-COUNT       PIC S9(9)V99  COMP-3.
           03  UPDATE-TRAMT       PIC S9(9)V99  COMP-3.
           03  UPDATE-INTDUE      PIC S9(9)V99  COMP-3.
           03  UPDATE-APOTH       PIC S9(9)V99  COMP-3.
           03  UPDATE-APINT       PIC S9(9)V99  COMP-3.
           03  UPDATE-APLC        PIC S9(9)V99  COMP-3.
           03  UPDATE-APCUR       PIC S9(9)V99  COMP-3.
           03  UPDATE-CHECKS      PIC S9(9)V99  COMP-3.
           03  UPDATE-EARNINGS    PIC S9(9)V99  COMP-3.
           03  UPDATE-DLPROC      PIC S9(9)V99  COMP-3.

       01  UPDATE-TYPE            PIC X.
       01  S1                     PIC 9 VALUE ZERO.
       01  S2                     PIC 9 VALUE ZERO.


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

      *-----------------------------------------------------------------
       01  WS-HOLD-PROMPT       PIC X(7)  VALUE SPACES.

      *****************************************************************
      *
      *  TYPE   LEV1 = POSTING FROM THIS BRANCH
      *            2 = POSTING FROM OTHER BRANCHES
      *
      *  TOTALS LEV1 = DATE TOTALS
      *            2 = DRAWER TOTALS
      *            3 = BRANCH TOTALS
      *            4 = GRAND TOTALS
      *
      *****************************************************************
       01  TOTALS-TAB   VALUE ZEROS.
           03  TYPE-TBL        OCCURS 2.
               05  TOTAL-TBL       OCCURS 4.
                   07  T-COUNT      PIC S9(9).
                   07  T-TRAMT      PIC S9(9)V99.
                   07  T-INTDUE     PIC S9(9)V99.
                   07  T-APOTH      PIC S9(9)V99.
                   07  T-APINT      PIC S9(9)V99.
                   07  T-APLC       PIC S9(9)V99.
                   07  T-APCUR      PIC S9(9)V99.
                   07  T-CHECKS     PIC S9(9)V99.

       01  LEGEND               PIC X(70).

      *****************************************************************
      *
      *     S C R E E N   F I E L D S
      *
      *****************************************************************
       COPY "LIBGB/EOBUF_EXT.CPY".
      *-----------------------------------------------------------------
           03  BEG-BRANCH       PIC 9(4).
           03  END-BRANCH       PIC 9(4).
           03  BEG-CLASS        PIC 9(3).
           03  END-CLASS        PIC 9(3).
           03  EDATE            PIC 9(8).
           03  POST-USERID      PIC X(8).
           03  GTT-ISO          PIC X.
           03  SUM-ONLY         PIC X.
           03  GROUP-FLAG       PIC X.
           03  ENT-GROUP        PIC X(4).
           03  LOCAL-PRINT-FG   PIC X(1).
           03  TILLNO           PIC 99.
           03  TOTAL-LEV-TABLE.
               05 TOTAL-LEVEL-TABLE PIC X(1) OCCURS 10.

           03  REF-NO           PIC X(5).
           03  INTERBR-ISO      PIC X.
               88  INTERBR-INCLUDE VALUE "I".
               88  INTERBR-ONLY    VALUE "O".
               88  INTERBR-SKIP    VALUE "S".
               88  INTERBR-SPACE   VALUE " ".
               88  INTERBR-VALID   VALUE "I", "S", "O" " ".

           03  DRAWER-TOTALS        PIC X.
               88  DRAWER-TOTALS-YES    VALUE "Y".
               88  DRAWER-TOTALS-NO     VALUE "N".
               88  DRAWER-TOTALS-SPACE  VALUE " ".
               88  DRAWER-TOTALS-VALID  VALUE "Y", "N", " ".

           03  SKIP-PAGE-BREAKS-ON-SUMMARY          PIC X.
           03  F3-ALL-DRAWERS       PIC X.

       01  VERSION-CONTROL     PIC X VALUE "D".

       01  SV-BEG-BR           PIC 9999  VALUE 9999.
       01  SV-END-BR           PIC 9999  VALUE 9999.
       01  SV-GROUP-FLAG       PIC X     VALUE SPACES.
       01  SV-ENT-GROUP        PIC X(4)  VALUE SPACES.
       01  SV-LOCAL-PRINT-FG   PIC X     VALUE SPACES.

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
           03  FILLER           PIC X(36)    VALUE " ".
           03  FILLER           PIC X(41) VALUE
               "CASH DRAWER TRANSACTION JOURNAL".

      *-----------------------------------------------------------------
       01  HEAD3.
           03  FILLER           PIC X(10) VALUE "BRANCH :".
           03  H-BRNO           PIC 9(4).
           03  FILLER           PIC X VALUE " ".
           03  H-BRDESC         PIC X(20).
           03  FILLER           PIC X(46) VALUE " ".

      *-----------------------------------------------------------------
       01  HEAD4.
           03  FILLER           PIC X(10) VALUE "DRAWER :".
           03  H4-TILLNO        PIC 99.
      *-----------------------------------------------------------------
       01  HEAD4B.
           03  FILLER           PIC X(20) VALUE
                                    "TRANSACTION DATE :  ".
           03  H4-DATE          PIC 99/99/99.

      *-----------------------------------------------------------------
       01  HEAD5.
           03  FILLER           PIC X(32)    VALUE SPACES.
           03  FILLER           PIC X(16)    VALUE SPACES.
           03  FILLER           PIC X(43)    VALUE
           "         TR        TRANS    - APPLIED -    ".
           03  FILLER           PIC X(37)    VALUE
           "     ---- BALANCES -----        G/L".

       01  HEAD5A.
           03  FILLER           PIC X(32)    VALUE SPACES.
           03  FILLER           PIC X(16)    VALUE SPACES.
           03  FILLER           PIC X(43)    VALUE
           "         TR        TRANS    - APPLIED -    ".
           03  FILLER           PIC X(37)    VALUE
           "     ---- BALANCES -----           ".
      *-----------------------------------------------------------------
       01  HEAD6.
           03  FILLER           PIC X(32)      VALUE
           "TIME  USERID   ACCTNO:   NAME   ".
           03  FILLER           PIC X(16)      VALUE SPACES.
           03  FILLER           PIC X(43)      VALUE
           "PAYDATE  CD REFNO  AMOUNT   INT     PRIN   ".
           03  FILLER           PIC X(37)      VALUE
           "    OTH      INT     PRIN    ACCT-AMT".

       01  HEAD6A.
           03  FILLER           PIC X(32)      VALUE
           "TIME  USERID   ACCTNO:   NAME   ".
           03  FILLER           PIC X(16)      VALUE SPACES.
           03  FILLER           PIC X(43)      VALUE
           "PAYDATE  CD REFNO  AMOUNT   INT     PRIN   ".
           03  FILLER           PIC X(41)      VALUE
           "    OTH      INT     PRIN     BRNO-[TILL]".

      *-----------------------------------------------------------------
       01  SUM-HEAD.
           03  FILLER           PIC X(55)    VALUE
           "DATE               TRCD    NO     TR-AMT  NSF CHECKS   ".
           03  FILLER           PIC X(55)    VALUE
           "APP-OTH    APP-INT     APP-LC    APP-PRIN  REB CHECKS  ".
      *--->03  FILLER           PIC X(20)    VALUE
      *--->" EARNINGS    DL-PROC".

      *-----------------------------------------------------------------
       01  SUM-HEAD-SKIP-PAGE-BREAK.
           03  FILLER           PIC X(55)    VALUE
           "DATE       BR  TILL TRCD   NO     TR-AMT  NSF CHECKS   ".
           03  FILLER           PIC X(55)    VALUE
           "APP-OTH    APP-INT     APP-LC    APP-PRIN  REB CHECKS  ".


      *-----------------------------------------------------------------
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
           03  FILLER            PIC X(9)   VALUE "  NO     ".
           03  FILLER            PIC X(11)  VALUE "TRANS      ".
           03  FILLER            PIC X(12)  VALUE "NSF         ".
           03  FILLER            PIC X(12)  VALUE "APPL        ".
           03  FILLER            PIC X(11)  VALUE "APPL      ".
           03  FILLER            PIC X(11)  VALUE "APPL       ".
           03  FILLER            PIC X(12)  VALUE "APPL        ".
           03  FILLER            PIC X(10)  VALUE "REBATE    ".
           03  FILLER            PIC X(11)  VALUE "EARNINGS   ".
           03  FILLER            PIC X(11)  VALUE "DEALER     ".

      *-----------------------------------------------------------------
       01  GTT-HEAD3.
           03  FILLER            PIC X(14)  VALUE SPACES.
           03  FILLER            PIC X(10)  VALUE "LEV       ".
           03  FILLER            PIC X(9)   VALUE "         ".
           03  FILLER            PIC X(11)  VALUE "AMOUNT     ".
           03  FILLER            PIC X(10)  VALUE "CHECKS    ".
           03  FILLER            PIC X(14)  VALUE "  OTHER       ".
           03  FILLER            PIC X(11)  VALUE "INT        ".
           03  FILLER            PIC X(11)  VALUE "LT-CHG     ".
           03  FILLER            PIC X(12)  VALUE "PRIN        ".
           03  FILLER            PIC X(10)  VALUE "CHECKS    ".
           03  FILLER            PIC X(11)  VALUE "           ".
           03  FILLER            PIC X(11)  VALUE "PROCEEDS   ".

      *****************************************************************
      *
      *    D E T A I L - L I N E
      *
      *****************************************************************
       01  DETAIL-LINE          VALUE SPACES.
           03  FILLER           PIC X(116).
           03  D-BRXX           PIC X(12).
           03  D-BRX            PIC 9(4).

       01  D-BRANCH-TILL    REDEFINES DETAIL-LINE.
           03  FILLER           PIC X(120).
           03 D-INTERBR         PIC X(2).
           03 D-BRNO            PIC 9(4).
           03 FILLER            PIC X(2).
           03 D-BRACKET1        PIC X.
           03 D-TILLNO          PIC 9(2).
           03 D-BRACKET2        PIC X.

      *-----------------------------------------------------------------
       01  D-LINE1 REDEFINES DETAIL-LINE.
           03  D-POSTTIME       PIC ZZ/ZZ.
           03  FILLER           PIC X(1).
           03  D-POST-ID        PIC X(8).
           03  FILLER           PIC X(1).
           03  D-CLASS          PIC 99.
           03  D-DASH1          PIC X.
           03  D-NUMBER         PIC 9(6).
           03  FILLER           PIC X.
           03  D-NAME           PIC X(22).
           03  FILLER           PIC X.
           03  D-DATE           PIC 99/99/99.
           03  FILLER           PIC X.
           03  D-TRCD           PIC XX.
           03  D-REV            PIC X.
           03  D-REFNO          PIC X(5).
           03  D-TRAMT          PIC ZZZZZ.ZZ-.
           03  D-APINT          PIC ZZZZZ.ZZ-.
           03  D-APCUR          PIC ZZZZZ.ZZ-.
           03  FILLER           PIC X.
           03  D-OTHBAL         PIC ZZZZZ.ZZ-.
           03  D-INTBAL         PIC ZZZZZ.ZZ-.
           03  D-PRINBAL        PIC ZZZZ9.99-.



           03  D-GLNO           PIC ZZZ9/9999999.
           03  D-GLAMT          REDEFINES D-GLNO
                                PIC ZZZZZZZZ.ZZ-.
           03  D-PAIDOFF        REDEFINES D-GLNO
                                PIC X(12).

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
           03  D4-GROUP             PIC X(19).
           03  D4-GROUP-1A          REDEFINES D4-GROUP.
               05  D4-1A-DATE       PIC Z9/99/99.
               05  FILLER           PIC X.
               05  D4-1A-BRNO       PIC ZZZ9.
               05  FILLER           PIC X(2).
               05  D4-1A-TILLNO     PIC Z9.
               05  FILLER           PIC X(2).
           03  D4-GROUP-2           REDEFINES D4-GROUP.
               05  D4-2-PROMPT1     PIC X(7).
               05  FILLER           PIC X(2).
               05  D4-2-GRNO        PIC X(4).
               05  D4-2-BRNO        REDEFINES D4-2-GRNO
                                    PIC 9(4).
               05  FILLER           PIC X(3).
               05  D4-2-GR-LEVEL    PIC 9(1).
               05  FILLER           PIC X(2).
           03  FILLER           PIC X.
           03  D4-TRCD          PIC XX.
           03  FILLER           PIC X.
           03  D4-NO            PIC ZZZZZ9.
           03  FILLER           PIC X.
           03  D4-TRAMT         PIC ZZZZZZZ.ZZ-.
           03  FILLER           PIC X.
           03  D4-INTDUE        PIC ZZZZZZ.ZZ-.
           03  FILLER           PIC X.
           03  D4-APOTH         PIC ZZZZZZ.ZZ-.
           03  FILLER           PIC X.
           03  D4-APINT         PIC ZZZZZZ.ZZ-.
           03  FILLER           PIC X.
           03  D4-APLC          PIC ZZZZZZ.ZZ-.
           03  FILLER           PIC X.
           03  D4-APCUR         PIC ZZZZZZZ.ZZ-.
           03  FILLER           PIC X.
           03  D4-CHECKS        PIC ZZZZZZZ.ZZ-.
           03  FILLER           PIC X.
           03  D4-EARNINGS      PIC ZZZZZZ.ZZ-.
           03  FILLER           PIC X.
           03  D4-DLPROC        PIC ZZZZZZ.ZZ-.

      *-----------------------------------------------------------------
       01  D-LINE5 REDEFINES DETAIL-LINE.
           03  D5-GLNO          PIC ZZZ9/9999999.
           03  FILLER           PIC XX.
           03  D5-GLAMT         PIC ZZZZ9.99-.

      *-----------------------------------------------------------------
       01  RANGE-LINE REDEFINES DETAIL-LINE.
           03  FILLER           PIC X(35).
           03  RANGE-SELECTION.
               04  R-NUM        PIC Z(7)9.
               04  R-NUM-REDEF REDEFINES R-NUM.
                   05 R-CHAR    PIC X(8).

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
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/BRSECUREW.CPY".
       COPY "LIBGB/SYSTEMW.CPY".
       COPY "LIBGB/GETPRW.CPY".
       COPY "LIBGB/EOSPOOLW.CPY".
       COPY "LIBGB/DACCESSW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBLP/EMTRR2_DEF.CPY".
       COPY "LIBLP/EMTRR2_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

      ******************************************************************
      *
      *     P R O G R A M   C O N T R O L
      *
      ******************************************************************
       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBLP/EMTRR2_SCN.CPY".

       PROCEDURE DIVISION
             USING FORM-PATHNAME EXIT-PATHNAME.
       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
               PR-FILE WK-FILE WK2-FILE.
       COPY "LIBGB/DECLRP.CPY".
           PERFORM CLOSE-TRP1-FILE.
           PERFORM CLOSE-LP1-FILE.
           PERFORM CLOSE-BW1-FILE.
           PERFORM CLOSE-GR1-FILE.
           PERFORM CLOSE-BR-FILE.
           PERFORM CLOSE-PR-FILE.
           CLOSE WK-FILE WK2-FILE.
       COPY "LIBGB/DECLRP2.CPY".

      ******************************************************************
      *
      *    M A I N    M O D U L E
      *
      ******************************************************************
       MAIN-MODULE SECTION.

           PERFORM SQL-CONNECT.

           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "EMTRR2" TO FORM-PROG VDU-FORM-NAME.


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

           MOVE EXT-FILPATH-BASE           TO FILPATH-BASE.
           MOVE BR-MACHINE                 TO FILPATH-MACHINE.
           MOVE BR-DATA-PATH               TO FILPATH-DATA.

           MOVE "B"                        TO BW-PATH-OVERRIDE
                                              LP-PATH-OVERRIDE
                                              TRP-PATH-OVERRIDE.

           PERFORM OPEN-TRP1-FILE.
           PERFORM OPEN-BW1-FILE.
           IF REF-NO NOT = "?????"
              PERFORM OPEN-LP1-FILE.

           PERFORM SORT-ROUTINE.

           IF EXT-EOBUF-ERRCD = "X"
              GO TO NEXT-GROUP-LOC.

           PERFORM OPEN-WK2-FILE.

      *-----------------------------------------------------------------
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
              MOVE TP-BRNO     TO HOLD-BRNO
              MOVE TP-TILLNO   TO HOLD-TILLNO
              MOVE TP-POSTDATE TO HOLD-DATE
              PERFORM SET-DATE-HEADINGS
              PERFORM SET-TILL-HEADINGS
              PERFORM SET-BRANCH-HEADINGS.

           IF TP-BRNO NOT = HOLD-BRNO
              PERFORM TILL-TOTALS
              PERFORM BRANCH-TOTALS
              MOVE TP-BRNO     TO HOLD-BRNO
              MOVE TP-TILLNO   TO HOLD-TILLNO
              MOVE TP-POSTDATE TO HOLD-DATE
              PERFORM SET-DATE-HEADINGS
              PERFORM SET-TILL-HEADINGS
              PERFORM SET-BRANCH-HEADINGS
              IF GTT-ISO NOT = "O"
                 IF DRAWER-TOTALS-YES OR DRAWER-TOTALS-SPACE
                   IF SUM-ONLY = "N"
                    PERFORM HEAD-PAGE.

           IF TP-TILLNO NOT = HOLD-TILLNO
              PERFORM TILL-TOTALS
              MOVE TP-TILLNO   TO HOLD-TILLNO
              MOVE TP-POSTDATE TO HOLD-DATE
              PERFORM SET-DATE-HEADINGS
              PERFORM SET-TILL-HEADINGS
              IF GTT-ISO NOT = "O"
                 IF DRAWER-TOTALS-YES OR DRAWER-TOTALS-SPACE
                   IF SUM-ONLY = "N"
                     PERFORM HEAD-PAGE.

           PERFORM DETAIL-LINE-PROCESS.
           PERFORM UPDATE-TOTALS-MODULE.

           GO TO NEXT-TR-REPORT.

       END-OF-GROUP.
           PERFORM CLOSE-TRP1-FILE.
           PERFORM CLOSE-BW1-FILE.
           PERFORM CLOSE-WK2-FILE.
           PERFORM OPEN-WK2-FILE-OUTPUT.

           IF ( LOCAL-PRINT-FG = "Y" )
              IF ( FIRST-TIME NOT = "Y" )
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
              PERFORM TILL-TOTALS
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
           MOVE EMTRR2-QUEUE-POS TO Q-POS.
           MOVE EMTRR2-COPIES-POS TO C-POS.
           MOVE EMTRR2-PRU-POS   TO P-POS.
           MOVE SPACES TO Q-BUF C-BUF P-BUF.

       COPY "LIBGB/EOINIT.CPY".

           MOVE SPACES TO RECORDS-FOUND.

           PERFORM CLEAR-T-TOTALS
                         VARYING SUB1 FROM 1 BY 1 UNTIL SUB1 > 2
                           AFTER SUB2 FROM 1 BY 1 UNTIL SUB2 > 4.

           MOVE 0 TO PAGE-NUMBER.


      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      * CREATE INDEXED KEY FILE FOR TRFILE OUTPUT
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

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
                 MOVE EXT-CONAME-USER-LOC TO BEG-BRANCH END-BRANCH.

           IF EXT-ROUTE-BUF = "00"
              MOVE "TRANSACTION REGISTER IN PROGRESS....." TO WR-BUF
              MOVE MESS-LIST TO WR-LIST
              PERFORM CALL-WRFORM.

           IF ( LOCAL-PRINT-FG NOT = "Y" )
              PERFORM OPEN-PR-FILE.

           MOVE EXT-CONAME-CONAME TO H-NAME.
           MOVE EXT-CONAME-SYSDATE TO H-DATE.
           MOVE EXT-CONAME-USER-ID TO H-USERID.

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
           MOVE EXT-EOBUF-DATE2 TO SYS-DATE EDATE.

      ******************************************************************
       ENTER-PARAMETERS SECTION.
      ******************************************************************
       ENTER-PARM.
           IF VDUSTATUS = "1U"
              IF ( GTT-ISO = "O" )
                 GO TO GTT-ISO-PARAGRAPH
              ELSE
                 IF NOT (DRAWER-TOTALS-YES AND SUM-ONLY = "Y")
                    MOVE SPACES TO SKIP-BREAK-ANSWER-BUF
                                   SKIP-BREAK-PROMPT-BUF
                    PERFORM SEND-SKIP-BREAK
                    GO TO SUMMARY-SKIP-BREAK-ON-TILL
                 ELSE
                    MOVE "SKIP PAGE BREAK ON SUMMARY:" TO
                                              SKIP-BREAK-PROMPT-BUF
                    MOVE SKIP-PAGE-BREAKS-ON-SUMMARY  TO
                                              SKIP-BREAK-ANSWER-BUF
                    PERFORM SEND-SKIP-BREAK
                    GO TO SUMMARY-SKIP-BREAK-ON-TILL.


      *    DEFAULT REPORT TO TODAYS DATE (TRANS-DATE NOT USED).
           IF EXT-ROUTE-BUF = "00"
              MOVE "S  A000ENDDATE." TO VDU
              MOVE EXT-CONAME-SYSDATE TO VDUBUF
              PERFORM SCREENIO.

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
           MOVE "N" TO F3-ALL-DRAWERS.
           IF TILLNO = 0                     
              GO TO DRAWER-NO.
           GO TO REFNO.

      *-----------------------------------------------------------------
       ALL-DRAWERS.
           MOVE "Y" TO F3-ALL-DRAWERS.
           MOVE "SN N020DRAWER." TO VDU. 
           MOVE 99 TO TILLNO VDUNUM0.
           PERFORM SCREENIO.
      *-----------------------------------------------------------------
       REFNO.
           MOVE LEGEND-REFNO TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "E  A050REFNO." TO VDU.
           PERFORM SCREENIO.
           IF F3-KEY GO TO ALL-REFNO.
           IF UP-KEY GO TO DRAWER-NO.
           IF F1-KEY GO TO EXIT-PARM.
           IF F7-KEY GO TO EXIT-PARM.

           IF NOT ENTER-KEY AND NOT DOWN-KEY GO TO REFNO.

           MOVE VDUBUF  TO REF-NO.

           GO TO DTE.
      *-----------------------------------------------------------------
       ALL-REFNO.
           MOVE "S  A050REFNO." TO VDU.
           MOVE "?????"  TO VDUBUF.
           MOVE "?????"  TO REF-NO.
           PERFORM SCREENIO.

      *-----------------------------------------------------------------
       DTE.
           IF EXT-ROUTE-BUF NOT = "00"
              GO TO USER-ID.

           MOVE LEGEND-TRANS-DTE TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "ERNM060ENDDATE." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO REFNO.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO DTE.
           MOVE VDU-DATE-YYYYMMDD TO EDATE.
           IF EDATE = 0
              GO TO DTE.

      *-----------------------------------------------------------------
       USER-ID.
           MOVE LEGEND-INCLUDE-USER TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "E  A080USER-ID." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO DTE.
           IF VDUSTATUS = "16" GO TO ALL-USER-ID.
           IF NOT ( VDUSTATUS = "00" OR "1D" )
              GO TO USER-ID.
           MOVE VDUBUF TO POST-USERID.
           IF POST-USERID = "********"
              GO TO USER-ID.
           GO TO INTERBRANCH-ISO.

       ALL-USER-ID.
              MOVE "S  A080USER-ID." TO VDU.
              MOVE "********" TO VDUBUF.
              PERFORM SCREENIO.
              MOVE SPACES TO POST-USERID.

      *-----------------------------------------------------------------
       INTERBRANCH-ISO.
           MOVE LEGEND-INTERBRANCH TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "E  A010INTERBR-ISO." TO VDU.
           PERFORM SCREENIO.

           IF UP-KEY GO TO USER-ID.
           IF F1-KEY GO TO EXIT-PARM.
           IF F7-KEY GO TO EXIT-PARM.
           IF NOT DOWN-KEY AND NOT ENTER-KEY
              GO TO INTERBRANCH-ISO.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

           MOVE VDUBUF TO INTERBR-ISO.

           IF NOT INTERBR-VALID
              GO TO INTERBRANCH-ISO.

      *-----------------------------------------------------------------
       PRINT-DRAWER-TOTALS-YN.
           MOVE LEGEND-DRAWER-TOTALS TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "E  A010DRAWER-TOTAL." TO VDU.
           PERFORM SCREENIO.

           IF UP-KEY GO TO INTERBRANCH-ISO.
           IF F1-KEY GO TO EXIT-PARM.
           IF F7-KEY GO TO EXIT-PARM.
           IF NOT DOWN-KEY AND NOT ENTER-KEY
              GO TO PRINT-DRAWER-TOTALS-YN.

           MOVE VDUBUF   TO DRAWER-TOTALS.
           IF DRAWER-TOTALS NOT = "Y" AND NOT = "N"
              GO TO PRINT-DRAWER-TOTALS-YN.

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
           IF ( VDUSTATUS = "1U" ) GO TO PRINT-DRAWER-TOTALS-YN.
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

           MOVE LEGEND-SUMMARY-ONLY TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "E  A010SUMONLY." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U"
              IF ( GROUP-FLAG = "Y" )
                 GO TO GTT-ISO-PARAGRAPH.

           IF VDUSTATUS = "1U"
              IF EXT-ROUTE-BUF = "00"
                 GO TO PRINT-DRAWER-TOTALS-YN.
           IF VDUSTATUS = "1U"
              IF EXT-ROUTE-BUF NOT = "00"
                 GO TO DRAWER-NO.

           IF NOT ( VDUSTATUS = "00" OR "1D" )
              GO TO SUMYN.
           MOVE VDUBUF TO SUM-ONLY.

           IF NOT ( SUM-ONLY = "Y" OR "N" )
              GO TO SUMYN.

      *-----------------------------------------------------------------
       SUMMARY-SKIP-BREAK-ON-TILL.
           MOVE LEGEND-SKIP-PAGE-BREAKS TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "SKIP PAGE BREAK ON SUMMARY:" TO SKIP-BREAK-PROMPT-BUF.
           MOVE SKIP-PAGE-BREAKS-ON-SUMMARY   TO SKIP-BREAK-ANSWER-BUF.

           IF NOT (DRAWER-TOTALS-YES AND SUM-ONLY = "Y")
              MOVE SPACES TO SKIP-BREAK-PROMPT-BUF
              MOVE SPACE  TO SKIP-BREAK-ANSWER-BUF
              MOVE "N"    TO SKIP-PAGE-BREAKS-ON-SUMMARY.

           PERFORM SEND-SKIP-BREAK.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF NOT (DRAWER-TOTALS-YES AND SUM-ONLY = "Y")
              IF UP-KEY
                 GO TO SUMYN
               ELSE
                 GO TO LOCAL-PRINT.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "E  A010SKIP-BREAKS."     TO VDU.
           PERFORM SCREENIO.

           IF UP-KEY GO TO SUMYN.
           IF F7-KEY GO TO EXIT-PARM.
           IF NOT(DOWN-KEY OR ENTER-KEY)
              GO TO SUMMARY-SKIP-BREAK-ON-TILL.

           MOVE VDUBUF TO SKIP-PAGE-BREAKS-ON-SUMMARY.

           IF NOT(SKIP-PAGE-BREAKS-ON-SUMMARY = "Y" OR "N")
              GO TO SUMMARY-SKIP-BREAK-ON-TILL.

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
                   GO TO SUMMARY-SKIP-BREAK-ON-TILL
              ELSE
                 GO TO SUMMARY-SKIP-BREAK-ON-TILL.

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
      *-----------------------------------------------------------------
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

      * 'Y' OR SPACE
           IF F3-ALL-DRAWERS = "Y" OR
              (F3-ALL-DRAWERS = " " AND TILLNO = 99 )
              MOVE "SN N020DRAWER." TO VDU
              MOVE 99  TO VDUNUM0
              PERFORM SCREENIO
           ELSE
              MOVE "SN N020DRAWER." TO VDU
              MOVE TILLNO TO VDUNUM0
              PERFORM SCREENIO.

           MOVE "S  A050REFNO." TO VDU.
           MOVE REF-NO          TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "S  A080USER-ID." TO VDU.
           IF POST-USERID = SPACES
              MOVE "********" TO VDUBUF
           ELSE
              MOVE POST-USERID TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "S  A010INTERBR-ISO." TO VDU.
           MOVE INTERBR-ISO     TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "S  A010DRAWER-TOTAL." TO VDU.
           MOVE DRAWER-TOTALS   TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "S  A010GTT-ISO." TO VDU.
           MOVE GTT-ISO           TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "S  A010SUMONLY." TO VDU.
           MOVE SUM-ONLY TO VDUBUF.
           PERFORM SCREENIO.

           IF SUM-ONLY = "Y"
              MOVE "S  A270BREAK-PROMPT." TO VDU
              MOVE "SKIP PAGE BREAKS ON SUMMARY" TO VDUBUF
              PERFORM SCREENIO
              MOVE "S  A010SKIP-BREAKS." TO VDU
              MOVE SKIP-PAGE-BREAKS-ON-SUMMARY TO VDUBUF
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

           MOVE BR-NO               TO TP-BRNO
                                       QTRP1-WBEG-BRNO
                                       QTRP1-WEND-BRNO.
           MOVE EDATE               TO TP-DATE SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO QTRP1-WBEG-DATE
                                       QTRP1-WEND-DATE.

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

           IF TP-DATE NOT = EDATE
              GO TO EXIT-SORT-ROUTINE.

           IF TP-CLASS < BEG-CLASS OR TP-CLASS > END-CLASS
              GO TO SORT-TR-NEXT.

           IF (F3-ALL-DRAWERS = "N") OR
              (F3-ALL-DRAWERS = " " AND TILLNO NOT = 99 )
              IF TP-TILLNO NOT = TILLNO
                 GO TO SORT-TR-NEXT.

           IF (REF-NO = "?????")
              GO TO SKIP-REFERENCE-CHECK.

      *NOT BEEN TO PAGE 2 YET IN BATCH & 3 NEW FIELDS ARE SPACES.
           IF (REF-NO = SPACES)
             IF (INTERBR-SPACE)
              IF (DRAWER-TOTALS-SPACE)
                GO TO SKIP-REFERENCE-CHECK.

           PERFORM TEST-REFERENCE.
           IF REFNO-MATCH-NO
              GO TO SORT-TR-NEXT.

       SKIP-REFERENCE-CHECK.
           IF POST-USERID NOT = SPACES
              IF TP-USERID NOT = POST-USERID
                 GO TO SORT-TR-NEXT.

           IF INTERBR-SKIP
              IF TP-BRNO NOT = TP-POSTING-BR
                 GO TO SORT-TR-NEXT.

           IF INTERBR-ONLY
              IF TP-BRNO = TP-POSTING-BR
                 GO TO SORT-TR-NEXT.

           MOVE TRP1-KEY TO TEST-TRP1-KEY.

           IF TEST-TRP1-KEY-MAIN = SAVE-TRP1-KEY-MAIN AND
                                      SAVE-TP-REC-FG = "Y"
              GO TO SAVE-THIS-ACCT.

           IF TEST-TRP1-KEY-MAIN NOT = SAVE-TRP1-KEY-MAIN
              MOVE SPACE TO SAVE-TP-REC-FG
              MOVE TRP1-KEY TO SAVE-TRP1-KEY.

       SAVE-THIS-ACCT.
           MOVE POST-USERID TO WK2-USERID.
           MOVE TP-TILLNO   TO WK2-TILLNO.
           MOVE TP-POSTDATE TO WK2-POSTDATE.
           MOVE TP-POSTTIME TO WK2-POSTTIME.
           MOVE TP-DATE     TO WK2-DATE.
           MOVE TP-BRNO     TO WK2-BRANCH.
      *    MOVE TP-CODE     TO WK2-CODE.
           MOVE TP-CLASS    TO WK2-CLASS.
           MOVE TP-ACCTNO   TO WK2-ACCTNO.
           MOVE TP-LINE     TO WK2-LINE.

           PERFORM WRITE-WK2-FILE.
           IF IO-FG NOT = 0
              GO TO IO-ERROR.

           GO TO SORT-TR-NEXT.

       EXIT-SORT-ROUTINE.
           PERFORM CLOSE-WK2-FILE.


      ********************************************
       TEST-REFERENCE SECTION.
      ********************************************
           IF TP-REV = "Y"
              MOVE TP-BRNO     TO LP-BRNO
              MOVE TP-ACCTNO   TO LP-ACCTNO
              MOVE TP-REVSEQNO TO LP-SEQNO
              PERFORM READ-LP1-FILE
              IF IO-GOOD
                 MOVE LP-REFNO TO TEST-TP-REFNO
              ELSE
                 GO TO TEST-REFERENCE-LOOP-EXIT
              END-IF
           ELSE
              MOVE TP-REFNO    TO TEST-TP-REFNO.
           MOVE REF-NO       TO TEST-REFNO.
           MOVE "N"         TO REFNO-MATCH.
           MOVE 0 TO REFNO-SUB.
       TEST-REFERENCE-LOOP-NEXT.
           ADD 1 TO REFNO-SUB.
           IF REFNO-SUB > 5
              GO TO TEST-REFERENCE-END.
           IF TEST-REFNO-CHAR(REFNO-SUB) = "?"
              GO TO TEST-REFERENCE-LOOP-NEXT.
           IF TEST-TP-REFNO-CHAR(REFNO-SUB) NOT =
                                       TEST-REFNO-CHAR(REFNO-SUB)
              GO TO TEST-REFERENCE-LOOP-EXIT.
           GO TO TEST-REFERENCE-LOOP-NEXT.
       TEST-REFERENCE-END.
           MOVE "Y" TO REFNO-MATCH.

       TEST-REFERENCE-LOOP-EXIT.
           EXIT.

      ******************************************************************
       DETAIL-LINE-PROCESS SECTION.
      ******************************************************************
           IF FIRST-TIME = "Y"
              MOVE EXT-CONAME-RPT-LINES TO LN-COUNT
              MOVE 1 TO LN-FEED
              MOVE "N" TO FIRST-TIME
              MOVE "Y" TO RECORDS-FOUND.

           MOVE TP-POSTTIME TO HOLD-POSTTIME.
           MOVE HOLD-HR-MIN TO D-POSTTIME.
           INSPECT D-POSTTIME REPLACING FIRST "/" BY ":".
           MOVE TP-USERID TO D-POST-ID.

           MOVE TP-CLASS  TO D-CLASS.
           MOVE "-"       TO D-DASH1.
           MOVE TP-ACCTNO TO D-NUMBER.

           PERFORM FIND-BORROWER.

           IF SUM-ONLY = "N"  AND DRAWER-TOTALS NOT = "N"
              IF TP-OTHBR-FG = "G"
                 MOVE "POSTED BY :" TO D-BRXX
                 MOVE TP-POSTING-BR TO D-BRX
              ELSE
                 IF TP-OTHBR-FG = "C"
                    MOVE "POSTED FOR:" TO D-BRXX
                    MOVE TP-POSTING-BR TO D-BRX.

      *REGIONAL OPTION TO SKIP PRINTING OF DRAWER TOTALS
           IF SUM-ONLY = "N" AND DRAWER-TOTALS = "N"
              IF TP-OTHBR-FG = "G"
                 MOVE "BR"    TO D-INTERBR
                 MOVE TP-POSTING-BR     TO D-BRNO
              ELSE
              IF TP-OTHBR-FG = "C"
                 MOVE "BR" TO D-INTERBR
                 MOVE TP-POSTING-BR     TO D-BRNO
              ELSE
                 MOVE SPACES TO D-INTERBR
                 MOVE TP-BRNO           TO D-BRNO.

           MOVE TP-PAYDATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO D-DATE.

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

           COMPUTE D-OTHBAL = TP-OTHBAL + TP-OT2BAL.
           ADD TP-INTBAL TP-LCBAL GIVING D-INTBAL.
           MOVE TP-CURBAL TO D-PRINBAL.

           IF SUM-ONLY = "N"
            IF TP-CURBAL = 0
               IF NOT (TP-TRCD = "RC" OR "RA" OR "RP"
                               OR "R1" OR "R2" OR "R3")
                  MOVE "**PAIDOFF**" TO D-PAIDOFF
                  PERFORM WRITE-DETAIL-LINE.

           IF SUM-ONLY = "N"
              IF DRAWER-TOTALS-NO
                MOVE "["     TO D-BRACKET1
                MOVE "]"     TO D-BRACKET2
                MOVE TP-TILLNO TO D-TILLNO
                PERFORM WRITE-DETAIL-LINE.

           IF TP-GLAMT(1) NOT = 0
              MOVE TP-GLNO(1) TO D-GLNO
              INSPECT D-GLNO REPLACING ALL "/" BY "-".

           IF SUM-ONLY = "N"
              IF DETAIL-LINE NOT = SPACES
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
      *    UPDATE-TYPE - USED TO KNOW WHAT TYPE OF NUMBERS/CALCULATIONS
      *                  ARE STORED IN THE 'UPDATE-WORKERS'.
      *
      *                  "C" = CURRENT BRANCH
      *                  "O" = OTHER BRANCH (INTER-BRANCH)
      *
      ******************************************************************
       UPDATE-TOTALS-MODULE SECTION.
           MOVE "C" TO UPDATE-TYPE.
           PERFORM LOAD-UPDATE-WORKERS.
           MOVE 1 TO S1.
           MOVE 2 TO S2.
           PERFORM UPDATE-T-TOTALS.

           MOVE "L" TO HOLD-WK-CODE.
           PERFORM UPDATE-WK-FILE.

           MOVE "R" TO HOLD-WK-CODE.
           PERFORM UPDATE-WK-FILE.

           IF ( GTT-ISO NOT = "S" )
              MOVE "T" TO HOLD-WK-CODE
              PERFORM UPDATE-WK-FILE.

           IF ( TP-BRNO NOT = TP-POSTING-BR )
              MOVE "O" TO UPDATE-TYPE
              PERFORM LOAD-UPDATE-WORKERS
              MOVE 2 TO S1
              MOVE 2 TO S2
              PERFORM UPDATE-T-TOTALS.

           PERFORM UPDATE-GL-FILE.

       UPDATE-TOTALS-MODULE-EXIT.
           EXIT.

      ******************************************************************
       LOAD-UPDATE-WORKERS SECTION.
      ******************************************************************

           MOVE ZEROES TO UPDATE-COUNT
                          UPDATE-TRAMT
                          UPDATE-INTDUE
                          UPDATE-APOTH
                          UPDATE-APINT
                          UPDATE-APLC
                          UPDATE-APCUR
                          UPDATE-CHECKS
                          UPDATE-EARNINGS
                          UPDATE-DLPROC.

      * O = OTHER BRANCH TOTALS ARE IN THE WORKERS
           IF ( UPDATE-TYPE = "O" )
              GO TO LOAD-UPDATE-OTHER-BR.

      *-----------------------------------------------------------------
       LOAD-UPDATE-CURRENT.

           IF ( TP-TRAMT NOT < 0 )
              ADD 1        TO UPDATE-COUNT
           ELSE
              SUBTRACT 1 FROM UPDATE-COUNT.

           IF TP-OTHBR-FG = " " OR "C"
              IF TP-REFNO = "NSF  "
                 ADD TP-TRAMT TO UPDATE-INTDUE
              ELSE
              IF TP-TRCD = "PY" OR "PA" OR "PE" OR "PO"
                                 OR "DF" OR "D2" OR "D3"
                                  OR "D4" OR "D5" OR "D6"
                                   OR "D7" OR "D8" OR "D9"
                                    OR "PL" OR "TS" OR "TP"
                                     OR "PR" OR "PC" OR "PP"
                                      OR "SS" OR "Z2" OR "AH"
                                       OR "IN" OR "OV" OR "ED"
                                        OR "EP" OR "ES"
                 ADD TP-TRAMT TO UPDATE-TRAMT.

           IF TP-OTHBR-FG = " " OR "G"
              ADD TP-APOTH TO UPDATE-APOTH
              ADD TP-APOT2 TO UPDATE-APOTH
              ADD HOLD-EARNINGS TO UPDATE-EARNINGS
              ADD TP-DLPROC TO UPDATE-DLPROC
              PERFORM TEST-ADD-APLC
              IF ( (TP-TRCD = "RT" OR "ET" OR "LP" OR "NP" OR "ST") OR
                   (TP-TRCD = "RV" AND
                     (TP-ADDON-TEXT = "ADD" OR "ADJ" OR "CNL") ) )
                 IF TP-IBPC = "I"
                    SUBTRACT TP-APCUR FROM UPDATE-APCUR
                 ELSE
                    SUBTRACT TP-TRAMT FROM UPDATE-APCUR
              ELSE
                 ADD TP-APCUR TO UPDATE-APCUR
                 IF TP-TRCD  NOT = "WI"
                   ADD TP-APINT TO UPDATE-APINT.

           IF TP-REFNO = "CHECK" OR
                 (TP-TRAMT < 0 AND TP-TRCD = "OV")
              ADD TP-TRAMT TO UPDATE-CHECKS.

            GO TO LOAD-UPDATE-WORKERS-EXIT.

       LOAD-UPDATE-OTHER-BR.

           IF ( TP-TRAMT NOT < 0 )
              ADD 1        TO UPDATE-COUNT
           ELSE
              SUBTRACT 1 FROM UPDATE-COUNT.

           IF TP-OTHBR-FG = " " OR "G"
              IF TP-REFNO = "NSF  "
                 ADD TP-TRAMT TO UPDATE-INTDUE
              ELSE
              IF TP-TRCD = "PY" OR "PA" OR "PE" OR "PO"
                                 OR "DF" OR "D2" OR "D3"
                                  OR "D4" OR "D5" OR "D6"
                                   OR "D7" OR "D8" OR "D9"
                                    OR "PL" OR "TS" OR "TP"
                                     OR "PR" OR "PC" OR "PP"
                                      OR "SS" OR "Z2" OR "AH"
                                       OR "IN" OR "ED" OR "EP"
                                        OR "ES"
                 ADD TP-TRAMT TO UPDATE-TRAMT.

           IF TP-OTHBR-FG = " " OR "G"
              ADD TP-APOTH TO UPDATE-APOTH
              ADD TP-APOT2 TO UPDATE-APOTH
              PERFORM TEST-ADD-APLC
              IF ( (TP-TRCD = "RT" OR "ET" OR "LP" OR "NP" OR "ST") OR
                   (TP-TRCD = "RV" AND
                     (TP-ADDON-TEXT = "ADD" OR "ADJ" OR "CNL") ) )
                 IF TP-IBPC = "I"
                    SUBTRACT TP-APCUR FROM UPDATE-APCUR
                 ELSE
                    SUBTRACT TP-TRAMT FROM UPDATE-APCUR
              ELSE
                 ADD TP-APCUR TO UPDATE-APCUR
                 IF TP-TRCD NOT = "WI"
                    ADD TP-APINT TO UPDATE-APINT.

           IF TP-REFNO = "CHECK" OR
                 (TP-TRAMT < 0 AND TP-TRCD = "OV")
              ADD TP-TRAMT TO UPDATE-CHECKS.

       LOAD-UPDATE-WORKERS-EXIT.
           EXIT.

      ******************************************************************
       UPDATE-T-TOTALS SECTION.
      ******************************************************************

           ADD UPDATE-COUNT    TO T-COUNT   (S1 S2).
           ADD UPDATE-TRAMT    TO T-TRAMT   (S1 S2).
           ADD UPDATE-INTDUE   TO T-INTDUE  (S1 S2).
           ADD UPDATE-APOTH    TO T-APOTH   (S1 S2).
           ADD UPDATE-APINT    TO T-APINT   (S1 S2).
           ADD UPDATE-APLC     TO T-APLC    (S1 S2).
           ADD UPDATE-APCUR    TO T-APCUR   (S1 S2).
           ADD UPDATE-CHECKS   TO T-CHECKS  (S1 S2).

       UPDATE-T-TOTALS-EXIT.
           EXIT.

      ******************************************************************
       UPDATE-GL-FILE SECTION.
      ******************************************************************

           MOVE ZEROES TO SUB.

       UPDATE-GL-FILE-LOOP.

           ADD 1 TO SUB.

           IF ( SUB > 3 )
              GO TO UPDATE-GL-FILE-EXIT.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF TP-GLAMT(SUB) = 0
              GO TO UPDATE-GL-FILE-LOOP.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      * ONLY GIVE THE GL AMT TO THE BRANCH THAT IT WAS MADE FOR
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF TP-OTHBR-FG NOT = " " AND NOT = "G"
              GO TO UPDATE-GL-FILE-LOOP.

           PERFORM SETUP-WK-GL1-KEY.
           PERFORM READ-WK-FILE.

           IF ( IO-GOOD )
              ADD TP-GLAMT(SUB) TO WK-GLAMT
              PERFORM REWRITE-WK-FILE
              GO TO UPDATE-GL-FILE-LOOP.

           PERFORM SETUP-WK-GL1-KEY.
           MOVE TP-GLAMT(SUB) TO WK-GLAMT.
           PERFORM WRITE-WK-FILE.

           IF ( IO-BAD )
              GO TO IO-ERROR.

           GO TO UPDATE-GL-FILE-LOOP.

       UPDATE-GL-FILE-EXIT.
           EXIT.

      ******************************************************************
       UPDATE-WK-FILE SECTION.
      ******************************************************************

           PERFORM SETUP-WK-KEY.
           PERFORM READ-WK-FILE.

           IF ( IO-GOOD )
              GO TO UPDATE-WK-REWRITE.

      *- - - - - - - - - - - - - - - - - - - -{ CREATE NEW 'WK'; WRITE }
           PERFORM SETUP-WK-KEY.
           MOVE 0 TO WK-NO.

           MOVE 0 TO WK-TRAMT WK-INTDUE WK-APOTH WK-APINT WK-APLC
                     WK-APCUR WK-CHECKS WK-EARNINGS WK-DLPROC.

           PERFORM WRITE-WK-FILE.

           IF IO-FG NOT = 0
              GO TO IO-ERROR.

      *-----------------------------------------------------------------
       UPDATE-WK-REWRITE.

           ADD UPDATE-COUNT    TO WK-NO.
           ADD UPDATE-TRAMT    TO WK-TRAMT.
           ADD UPDATE-INTDUE   TO WK-INTDUE.
           ADD UPDATE-APOTH    TO WK-APOTH.
           ADD UPDATE-APINT    TO WK-APINT.
           ADD UPDATE-APLC     TO WK-APLC.
           ADD UPDATE-APCUR    TO WK-APCUR.
           ADD UPDATE-CHECKS   TO WK-CHECKS.
           ADD UPDATE-EARNINGS TO WK-EARNINGS.
           ADD UPDATE-DLPROC   TO WK-DLPROC.

           PERFORM REWRITE-WK-FILE.

       UPDATE-WK-FILE-EXIT.
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
       SET-DATE-HEADINGS SECTION.
      ******************************************************************
           MOVE EDATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO H4-DATE.
           MOVE "N" TO SUMMARY-FG.

      ******************************************************************
       TILL-TOTALS SECTION.
      ******************************************************************

           MOVE "TILL  " TO PRINT-STATUS.
           MOVE "Y"      TO SUMMARY-FG.

           IF ( GTT-ISO = "O" )
              GO TO END-TILL-TOTALS.

           IF DRAWER-TOTALS-NO
              GO TO END-TILL-TOTALS.

           IF ( SKIP-PAGE-BREAKS-ON-SUMMARY NOT = "Y" )
              MOVE EXT-CONAME-RPT-LINES TO LN-COUNT
              PERFORM WRITE-DETAIL-LINE
              MOVE SUM-HEAD     TO PR-REC
              MOVE 2            TO LN-FEED
              PERFORM WRITE-PR-REC
              ADD  2            TO LN-COUNT
              GO TO TILL-TOTALS-START-WK.

      *- - - - - - - - - - - - - - - - - - - - - - - -{ SKIP PAGE BREAK}
           COMPUTE LN-WK = LN-COUNT + 4.

           IF ( LN-WK > EXT-CONAME-RPT-LINES )
              PERFORM HEAD-PAGE
              GO TO TILL-TOTALS-START-WK.

           MOVE SPACES TO DETAIL-LINE.
           MOVE 1      TO LN-FEED
           PERFORM WRITE-DETAIL-LINE.

           MOVE SUM-HEAD-SKIP-PAGE-BREAK TO DETAIL-LINE.
           MOVE 2                        TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE 2 TO LN-FEED.

      *-----------------------------------------------------------------
       TILL-TOTALS-START-WK.

           MOVE SPACES TO WK-KEY.
           MOVE "L" TO WK-CODE.
           MOVE HOLD-TILLNO TO WK-TILLNO.
           MOVE HOLD-DATE   TO WK-DATE.
           MOVE HOLD-BRNO   TO WK-BRNO.
           MOVE HOLD-CLASS  TO WK-CLASS.
           PERFORM START-WK-FILE.

      *-----------------------------------------------------------------
       PRINT-NEXT-CODE2.
           PERFORM READ-WK-FILE-NEXT.
           IF (IO-FG = 9) OR
               (WK-CODE NOT = "L") OR
                (WK-TILLNO NOT = HOLD-TILLNO) OR
                 (WK-DATE NOT = HOLD-DATE) OR
                  (WK-BRNO NOT = HOLD-BRNO)
                      PERFORM TYPE2-TOTALS
                      GO TO END-TILL-TOTALS.

           IF SKIP-PAGE-BREAKS-ON-SUMMARY = "Y"
              MOVE WK-BRNO   TO D4-1A-BRNO
              MOVE WK-TILLNO TO D4-1A-TILLNO.

           MOVE HOLD-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO D4-1A-DATE.
           MOVE WK-TRCD TO D4-TRCD.
           MOVE WK-NO TO D4-NO.
           MOVE WK-TRAMT TO D4-TRAMT.
           MOVE WK-INTDUE TO D4-INTDUE.
           MOVE WK-APOTH TO D4-APOTH.
           MOVE WK-APINT TO D4-APINT.
           MOVE WK-APLC TO D4-APLC.
           MOVE WK-APCUR TO D4-APCUR.
           MOVE WK-CHECKS TO D4-CHECKS.
           MOVE WK-EARNINGS TO D4-EARNINGS.
           MOVE WK-DLPROC TO D4-DLPROC.
           PERFORM WRITE-DETAIL-LINE.
           MOVE 1 TO LN-FEED.

           GO TO PRINT-NEXT-CODE2.

       END-TILL-TOTALS.
           MOVE 3 TO LEV.
           MOVE 2 TO SUB.
           PERFORM UPDATE-TABLE2 VARYING SUB1 FROM 1 BY 1
                                                  UNTIL SUB1 > 2.
           MOVE 2 TO SUB2.
           PERFORM CLEAR-T-TOTALS
                         VARYING SUB1 FROM 1 BY 1 UNTIL SUB1 > 2

           MOVE "N"      TO SUMMARY-FG.
           MOVE "DETAIL" TO PRINT-STATUS.

      ******************************************************************
       BRANCH-TOTALS SECTION.
      ******************************************************************
           IF ( GTT-ISO = "O" )
              GO TO END-BRANCH-TOTALS.

           IF (DRAWER-TOTALS-NO)
              GO TO END-BRANCH-TOTALS.

           MOVE "Y" TO SUMMARY-FG.
           MOVE 0 TO HOLD-CLASS.
           MOVE 3 TO LEV.
           MOVE 1 TO SUB1.
           MOVE 3 TO LN-FEED.
           MOVE "BRANCH TOTALS:" TO DETAIL-LINE.
           PERFORM PRINT-SUMMARY.
           MOVE 0 TO HOLD-CLASS.
           MOVE 3 TO LEV.
           MOVE 2 TO SUB1.
           MOVE 2 TO LN-FEED.
           MOVE "POSTINGS FROM" TO DETAIL-LINE.
           PERFORM WRITE-DETAIL-LINE.
           MOVE "OTHER BRANCHES:" TO DETAIL-LINE.
           PERFORM PRINT-SUMMARY.

       END-BRANCH-TOTALS.
           MOVE 4 TO LEV.
           MOVE 3 TO SUB.
           PERFORM UPDATE-TABLE2 VARYING SUB1 FROM 1 BY 1
                                                   UNTIL SUB1 > 2.
           MOVE 3 TO SUB2.
           PERFORM CLEAR-T-TOTALS
                          VARYING SUB1 FROM 1 BY 1 UNTIL SUB1 > 2.
           MOVE "N" TO SUMMARY-FG.

      ******************************************************************
       GRAND-TOTALS SECTION.
      ******************************************************************
           IF ( GTT-ISO = "O" )
              GO TO END-GRAND-TOTALS.

           MOVE "Y" TO SUMMARY-FG.

           IF DRAWER-TOTALS-YES OR DRAWER-TOTALS-SPACE
              GO TO GRAND-TOTALS-SUMMARY.

           MOVE EXT-CONAME-RPT-LINES TO LN-COUNT.
           PERFORM WRITE-DETAIL-LINE.
           MOVE SUM-HEAD TO PR-REC.
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-PR-REC.
           PERFORM RECAP-TRCD-TOTALS.

       GRAND-TOTALS-SUMMARY.
           MOVE 4 TO LEV.
           MOVE 1 TO SUB1.
           MOVE 3 TO LN-FEED.
           MOVE "GRAND TOTALS :" TO DETAIL-LINE.
           PERFORM PRINT-SUMMARY.
           MOVE 0 TO HOLD-CLASS.
           MOVE 4 TO LEV.
           MOVE 2 TO SUB1.
           MOVE 2 TO LN-FEED.
           MOVE "TOTAL POSTINGS" TO DETAIL-LINE.
           PERFORM WRITE-DETAIL-LINE.
           MOVE "FROM OTHER BRANCHES:" TO DETAIL-LINE.
           PERFORM PRINT-SUMMARY.

       END-GRAND-TOTALS.
           MOVE 4 TO SUB2.
           PERFORM CLEAR-T-TOTALS
                         VARYING SUB1 FROM 1 BY 1 UNTIL SUB1 > 2.
           MOVE "Y" TO SUMMARY-FG.

      ********************************************
       RECAP-TRCD-TOTALS SECTION.
      ********************************************
           MOVE SPACES   TO WK-KEY.
           MOVE "R"      TO WK-CODE.
           MOVE 999      TO WK-TILLNO.
           MOVE 99991231 TO WK-DATE.
           MOVE 99999    TO WK-BRNO.
           MOVE 999      TO WK-CLASS.

           PERFORM START-WK-FILE.
      *---------------------------------------------------------
       PRINT-NEXT-RECAP-CODE.
           PERFORM READ-WK-FILE-NEXT.
           IF (IO-FG = 9) OR
               (WK-CODE NOT = "R") OR
                (WK-TILLNO NOT = 999) OR
                 (WK-DATE NOT = 99991231) OR
                  (WK-BRNO NOT = 99999) OR
                   (WK-CLASS NOT = 999)
              GO TO RECAP-TRCD-TOTALS-EXIT.

           MOVE TP-POSTDATE  TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY  TO D4-1A-DATE.
           MOVE WK-TRCD TO D4-TRCD.
           MOVE WK-NO TO D4-NO.
           MOVE WK-TRAMT TO D4-TRAMT.
           MOVE WK-INTDUE TO D4-INTDUE.
           MOVE WK-APOTH TO D4-APOTH.
           MOVE WK-APINT TO D4-APINT.
           MOVE WK-APLC TO D4-APLC.
           MOVE WK-APCUR TO D4-APCUR.
           MOVE WK-CHECKS TO D4-CHECKS.
           PERFORM WRITE-DETAIL-LINE.
           MOVE 1 TO LN-FEED.

           GO TO  PRINT-NEXT-RECAP-CODE.
       RECAP-TRCD-TOTALS-EXIT.
           EXIT.

      ******************************************************************
       GL-SUMMARY SECTION.
      ******************************************************************
           MOVE GL-HEAD TO DETAIL-LINE.
           MOVE 4 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.
           MOVE LOW-VALUE TO WK-KEY.
           MOVE "G" TO WK-CODE.
           PERFORM START-WK-FILE.

      *-----------------------------------------------------------------
       PRINT-NEXT-GL.
           PERFORM READ-WK-FILE-NEXT.
           IF IO-FG NOT = 0
              GO TO END-GL-SUMMARY.

           IF WK-CODE NOT = "G"
              GO TO PRINT-NEXT-GL.

           MOVE WK-GLNO TO D5-GLNO.
           INSPECT D5-GLNO REPLACING ALL "/" BY "-".
           MOVE WK-GLAMT TO D5-GLAMT.
           PERFORM WRITE-DETAIL-LINE.
           GO TO PRINT-NEXT-GL.

       END-GL-SUMMARY.
           MOVE SPACES TO DETAIL-LINE.
           PERFORM WRITE-DETAIL-LINE.

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

           IF  DRAWER-TOTALS-YES OR DRAWER-TOTALS-SPACE
              IF SKIP-PAGE-BREAKS-ON-SUMMARY NOT = "Y"
                 PERFORM GET-BRANCH-HEADER
                 MOVE HEAD4 TO PR-REC
                 PERFORM WRITE-PR-REC
                 GO TO HEAD-PAGE-CONTINUE.

      *CS    IF (DRAWER-TOTALS-YES) OR (DRAWER-TOTALS-SPACE)
      *CS       IF  SKIP-PAGE-BREAKS-ON-SUMMARY NOT = "Y"
      *CS       MOVE HEAD4 TO PR-REC
      *CS       PERFORM WRITE-PR-REC
      *CS       GO TO HEAD-PAGE-CONTINUE.

           MOVE HEAD4B TO PR-REC.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-PR-REC.


       HEAD-PAGE-CONTINUE.
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
              MOVE 11 TO LN-COUNT
              MOVE 1 TO LN-FEED
           ELSE
           IF SUMMARY-FG = "N"
              IF DRAWER-TOTALS-NO
                 MOVE HEAD5A TO PR-REC
              ELSE
                 MOVE HEAD5  TO PR-REC
              END-IF
              MOVE 2 TO LN-FEED
              PERFORM WRITE-PR-REC
              IF DRAWER-TOTALS-NO
                 MOVE HEAD6A TO PR-REC
              ELSE
                 MOVE HEAD6 TO PR-REC
              END-IF
              MOVE 1 TO LN-FEED
              PERFORM WRITE-PR-REC
              MOVE 7 TO LN-COUNT
              MOVE 2 TO LN-FEED
           ELSE
              MOVE 5 TO LN-COUNT
              MOVE 2 TO LN-FEED.

           IF ( PRINT-STATUS = "TILL  " )
              IF ( SKIP-PAGE-BREAKS-ON-SUMMARY = "Y" )
                 MOVE SUM-HEAD-SKIP-PAGE-BREAK TO PR-REC
      *CS     ELSE
      *CS        MOVE SUM-HEAD                 TO PR-REC
      * CS    END-IF
              MOVE 2 TO LN-FEED
              PERFORM WRITE-PR-REC
              ADD  2 TO LN-COUNT
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

           MOVE "CASH DRAWER"   TO RANGE-LINE.
           MOVE TILLNO          TO R-NUM.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "REFNO"         TO RANGE-LINE.
           MOVE  REF-NO         TO R-CHAR.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "TRANSACTION DATE" TO RANGE-LINE.
           MOVE EDATE TO R-NUM.
           PERFORM WRITE-DETAIL-LINE.
           MOVE "USERID     " TO RANGE-LINE.
           IF POST-USERID = SPACES
              MOVE "********"  TO R-CHAR
           ELSE
              MOVE POST-USERID TO R-CHAR.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "INTER BRANCH" TO RANGE-LINE.
           MOVE INTERBR-ISO    TO R-CHAR.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "PRINT DRAWER TOTALS" TO RANGE-LINE.
           MOVE DRAWER-TOTALS         TO R-CHAR.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "GROUP TOTALS" TO RANGE-LINE.
           MOVE GTT-ISO        TO R-CHAR.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "SUMMARY ONLY"      TO RANGE-LINE.
           MOVE SUM-ONLY       TO R-CHAR.
           PERFORM WRITE-DETAIL-LINE.

           IF SUM-ONLY = "Y"
              MOVE "SKIP PAGE BREAK" TO RANGE-LINE
              MOVE SKIP-PAGE-BREAKS-ON-SUMMARY  TO R-CHAR
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
      * CLEAR TOTAL TABLE
      *
      *  SUB1 --> 1 = OWNING BRANCH IS POSTING BRANCH
      *           2 = OWNING BRANCH IS NOT THE POSTING BRANCH
      *               POSTED FROM AMOTHER BRANCH.
      *
      *  SUB  --> 1 = CLASS TOTALS.
      *           2 = DRAWER TOTALS.
      *           3 = BRANCH TOTALS.
      *           4 = GRAND TOTALS.
      *
      *****************************************************************
       CLEAR-T-TOTALS SECTION.
           MOVE 0 TO T-TRAMT  (SUB1 SUB2)   T-INTDUE   (SUB1 SUB2)
                     T-APOTH  (SUB1 SUB2)   T-APINT    (SUB1 SUB2)
                     T-APLC   (SUB1 SUB2)   T-APCUR    (SUB1 SUB2)
                     T-CHECKS (SUB1 SUB2)   T-COUNT    (SUB1 SUB2).

      ******************************************************************
       GTT-ADD-TOTALS SECTION.
      ******************************************************************
           ADD WK-NO          TO  WS-GTT-NO        ( GTT-TOTAL-SUB ).
           ADD WK-TRAMT       TO  WS-GTT-TRAMT     ( GTT-TOTAL-SUB ).
           ADD WK-INTDUE      TO  WS-GTT-INTDUE    ( GTT-TOTAL-SUB ).
           ADD WK-APOTH       TO  WS-GTT-APOTH     ( GTT-TOTAL-SUB ).
           ADD WK-APINT       TO  WS-GTT-APINT     ( GTT-TOTAL-SUB ).
           ADD WK-APLC        TO  WS-GTT-APLC      ( GTT-TOTAL-SUB ).
           ADD WK-APCUR      TO  WS-GTT-APCUR    ( GTT-TOTAL-SUB ).
           ADD WK-CHECKS      TO  WS-GTT-CHECKS    ( GTT-TOTAL-SUB ).
           ADD WK-EARNINGS    TO  WS-GTT-EARNINGS  ( GTT-TOTAL-SUB ).
           ADD WK-DLPROC      TO  WS-GTT-DLPROC    ( GTT-TOTAL-SUB ).

       EXIT-GTT-ADD-TOTALS.
           EXIT.

      ******************************************************************
       GTT-COMPILE-GROUP-TOTALS SECTION.
      ******************************************************************

           MOVE SPACES      TO WK-GTT-KEY.
           MOVE "T"         TO WK-GTT-CODE.
           MOVE WGR-SRESULT TO WK-GTT-BRNO.
           MOVE ZEROES      TO WK-GTT-CLASS WK-GTT-DATE.
           PERFORM START-WK-FILE.

       GTT-NEXT-WK1.

           PERFORM READ-WK-FILE-NEXT.
           IF (IO-FG NOT = 0) OR
              (WK-GTT-CODE NOT = "T") OR
               (WK-GTT-BRNO NOT = WGR-SRESULT)
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
           MOVE "BRANCH:" TO D4-2-PROMPT1 WS-HOLD-PROMPT.
           MOVE BR-NO     TO D4-2-BRNO.
           PERFORM GTT-PRINT-TOTALS.

       EXIT-GTT-PRINT-GROUP-BR-TOTALS.
           EXIT.

      ******************************************************************
       GTT-PRINT-GROUP-TOTALS SECTION.
      ******************************************************************
           MOVE SPACES    TO DETAIL-LINE.
           MOVE "GROUP :" TO D4-2-PROMPT1 WS-HOLD-PROMPT.
           MOVE GR-ID     TO D4-2-GRNO.
           MOVE GR-LEVEL  TO D4-2-GR-LEVEL.
           PERFORM GTT-PRINT-TOTALS.

       EXIT-GTT-PRINT-GROUP-TOTALS.
           EXIT.

      ******************************************************************
       GTT-PRINT-TOTALS SECTION.
      ******************************************************************
           MOVE WS-GTT-NO        (GTT-TOTAL-SUB) TO D4-NO.
           MOVE WS-GTT-TRAMT     (GTT-TOTAL-SUB) TO D4-TRAMT.
           MOVE WS-GTT-INTDUE    (GTT-TOTAL-SUB) TO D4-INTDUE.
           MOVE WS-GTT-APOTH     (GTT-TOTAL-SUB) TO D4-APOTH.
           MOVE WS-GTT-APINT     (GTT-TOTAL-SUB) TO D4-APINT.
           MOVE WS-GTT-APLC      (GTT-TOTAL-SUB) TO D4-APLC.
           MOVE WS-GTT-APCUR    (GTT-TOTAL-SUB) TO D4-APCUR.
           MOVE WS-GTT-CHECKS    (GTT-TOTAL-SUB) TO D4-CHECKS.
           MOVE WS-GTT-EARNINGS  (GTT-TOTAL-SUB) TO D4-EARNINGS.
           MOVE WS-GTT-DLPROC    (GTT-TOTAL-SUB) TO D4-DLPROC.
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
      *
      *     C O P Y - L I B R A R Y - S E C T I O N S
      *
      ******************************************************************
       COPY "LIBWI/GRTOTL.CPY".
       COPY "LIBGB/TIMEIO.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/GRWORK.CPY".
       COPY "LIBGB/GRSERCH.CPY".
       COPY "LIBGB/BRSECURE.CPY".
       COPY "LIBGB/EOSPOOL.CPY".
       COPY "LIBGB/GRTOTALS.CPY".

      *********************************************************
       FORM-RESET SECTION.
      *********************************************************
           MOVE EXT-CONAME-CONAME TO EMTRR2-CONAME.
           MOVE EXT-CONAME-SYSDATE TO EMTRR2-SYSDATE.
           DISPLAY EMTRR2-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE EMTRR2-FORM-FIELDS TO WR-BUF.
           MOVE EMTRR2-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      *********************************************************
       SETUP-LINK-INFO SECTION.
      *********************************************************
           MOVE EMTRR2-HELP--FILE TO HELP-LINK-FILE.
           MOVE EMTRR2-HELP--DIR  TO HELP-LINK-DIR.

      *********************************************************
       VDU-CONVERT-FIELD SECTION.
      *********************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBLP/EMTRR2_EVA.CPY".
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
           MOVE HOLD-BRNO TO H-BRNO
           MOVE BR-NAME TO H-BRDESC.
      *-----------------------------------------------------------------
       SET-TILL-HEADINGS.
           MOVE HOLD-TILLNO TO H4-TILLNO.
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
      *----------------------------------------------------------------
       GET-BRANCH-HEADER.
              MOVE HEAD3 TO PR-REC.
              PERFORM WRITE-PR-REC.
      *-----------------------------------------------------------------
       TEST-ADD-APLC.
           IF TP-TRCD NOT = "WL"
              ADD TP-APLC TO UPDATE-APLC.

      *-----------------------------------------------------------------
       SEND-LEGEND.
           MOVE LEGEND TO WR-BUF.
           MOVE MSEL-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      *-----------------------------------------------------------------
       SETUP-WK-KEY.
           IF ( HOLD-WK-CODE = "L" )
              MOVE SPACES      TO WK-REC
              MOVE "L"         TO WK-CODE
              MOVE TP-TILLNO   TO WK-TILLNO
              MOVE TP-POSTDATE TO WK-DATE
              MOVE TP-BRNO     TO WK-BRNO
              IF SKIP-PAGE-BREAKS-ON-SUMMARY = "Y"
                 MOVE 999      TO WK-CLASS
              ELSE
                 MOVE TP-CLASS    TO WK-CLASS
              END-IF
              MOVE TP-TRCD     TO WK-TRCD
           ELSE
           IF ( HOLD-WK-CODE = "R" )
              MOVE SPACES      TO WK-REC
              MOVE "R"         TO WK-CODE
              MOVE 999         TO WK-TILLNO
              MOVE 99991231    TO WK-DATE
              MOVE 99999       TO WK-BRNO
              MOVE 999         TO WK-CLASS
              MOVE TP-TRCD     TO WK-TRCD
           ELSE
           IF ( HOLD-WK-CODE = "T" )
              MOVE SPACES      TO WK-REC
              MOVE "T"         TO WK-GTT-CODE
              MOVE TP-BRNO     TO WK-GTT-BRNO
              MOVE TP-TILLNO   TO WK-GTT-TILLNO
              MOVE TP-POSTDATE TO WK-GTT-DATE
              MOVE TP-CLASS    TO WK-GTT-CLASS
              MOVE TP-TRCD     TO WK-GTT-TRCD.


      *-----------------------------------------------------------------
       SETUP-WK-GL1-KEY.
           MOVE SPACES TO WK-REC.
           MOVE "G" TO WK-CODE.
           MOVE TP-GLNO(SUB) TO WK-GLNO.

      *-----------------------------------------------------------------
       UPDATE-TABLE2.
           ADD T-COUNT(SUB1 SUB) TO T-COUNT(SUB1 LEV).
           ADD T-TRAMT(SUB1 SUB) TO T-TRAMT(SUB1 LEV).
           ADD T-INTDUE(SUB1 SUB) TO T-INTDUE(SUB1 LEV).
           ADD T-APOTH(SUB1 SUB) TO T-APOTH(SUB1 LEV).
           ADD T-APINT(SUB1 SUB) TO T-APINT(SUB1 LEV).
           ADD T-APLC(SUB1 SUB) TO T-APLC(SUB1 LEV).
           ADD T-APCUR(SUB1 SUB) TO T-APCUR(SUB1 LEV).
           ADD T-CHECKS(SUB1 SUB) TO T-CHECKS(SUB1 LEV).

      *-----------------------------------------------------------------
       TYPE-TOTALS.
           MOVE 1 TO LEV.
           MOVE 1 TO SUB1.
           MOVE 3 TO LN-FEED.
           MOVE "DATE   TOTALS:" TO DETAIL-LINE.
           PERFORM PRINT-SUMMARY.

      *-----------------------------------------------------------------
       TYPE2-TOTALS.
           MOVE 2 TO LEV.
           MOVE 1 TO SUB1.
           MOVE 3 TO LN-FEED.
           MOVE "DRAWER TOTALS:" TO DETAIL-LINE.
           PERFORM PRINT-SUMMARY.

      *-----------------------------------------------------------------
       PRINT-SUMMARY.
           MOVE T-COUNT(SUB1 LEV) TO D4-NO.
           MOVE T-TRAMT(SUB1 LEV) TO D4-TRAMT.
           MOVE T-INTDUE(SUB1 LEV) TO D4-INTDUE.
           MOVE T-APOTH(SUB1 LEV) TO D4-APOTH.
           MOVE T-APINT(SUB1 LEV) TO D4-APINT.
           MOVE T-APLC(SUB1 LEV) TO D4-APLC.
           MOVE T-APCUR(SUB1 LEV) TO D4-APCUR.
           MOVE T-CHECKS(SUB1 LEV) TO D4-CHECKS.
           PERFORM WRITE-DETAIL-LINE.

      *-----------------------------------------------------------------
       SETUP-D-TRAMT.
           IF TP-IBPC = "I"
              COMPUTE D-TRAMT = TP-TRAMT * -1
           ELSE
              COMPUTE D-TRAMT = TP-APCUR * -1.


      *-----------------------------------------------------------------
       SEND-SKIP-BREAK.

           MOVE SKIP-BREAK-ANSWER-BUF TO WR-BUF.
           MOVE SKIP-BREAK-ANSWER-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

           MOVE SKIP-BREAK-PROMPT-BUF TO WR-BUF.
           MOVE SKIP-BREAK-PROMPT-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      ******************************************************************
       IO-ROUTINES SECTION.
      ******************************************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBLP/LPTRPGS_SQL.CPY".
       COPY "LIBLP/LPLPGS_SQL.CPY".
       COPY "LIBLP/LPBWGS_SQL.CPY".
       COPY "LIBGB/GBGRGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBPRINT.CPY".
       COPY "LIBLP/LPBW1RN.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBGB/GBGR1RN.CPY".
       COPY "LIBLP/LPTRP1RN.CPY".
       COPY "LIBLP/LPLP1RN.CPY".
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
              DESTROY EMTRR2-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.
           EXIT PROGRAM.
