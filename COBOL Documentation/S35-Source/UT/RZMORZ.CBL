       IDENTIFICATION DIVISION.
       PROGRAM-ID.      ICMORP.
       DATE-WRITTEN.    012985.
      *****************************************************************
      *                      (LP)
      *    DESCRIPTION:  MONTHLY CL/AH INS PREMIUM-REFUND REPORT
      *
      *                  USES GROUP LOGIC.
      *                  LOOPS THRU TRPFILE, TRLFILE AND TRSFILE AND READS
      *                  LNFILE1 TO TEST STATE. WRITES WKFILE
      *                  END OF BRANCH, LOOPS THRU WKFILE, READS LNFILE1, AND
      *                  PRINTS DETAIL LINE, WRITE WKFILE1 FOR COMPANY TOTALS.
      *                  END OF PROGRAM, LOOP THRU WKFILE1, PRINT COMPANY TOTALS
      *
      * * * NOTE * * * * * *
      * TO GET "STATE" AND "INSUROR" TOTALS, USE MUST USE F3-ALL BRANCHES
      * * * NOTE * * * * * *
      *
      *    SITUATIONS REQUIRING REPORTING:
      *
      *    NEW PREMIUMS:
      *        1. NEW LOANS IN THE TRFILE FOR (MMYY)
      *                             TR1-CODE = "L" OR "S"
      *                             TRL-INSOURS(1 OR 2) = "Y"
      *                             TRS-INSOURS(1 OR 2) = "Y"
      *
      *    REBATES OR CANCELLATIONS:
      *        1. REBATES/CANCELLATIONS/ADDONS IN THE TRFILE FOR (MMYY)
      *                             TR1-CODE = "P"
      *                             TP-INSOURS = "Y"
      *                             TP-TRCD = "RC" OR "RA" OR "CC" OR "CA"
      *                             TP-ADDON-TYPE = "CL" OR "AH"
      *
      * REV:
      *  090695 BAH WAS NOT GETTING IC FILE FROM EACH FILE SET
      *  100495 JFF ADDED CLASS SCAN
      *  121096 MJD CHANGED TO PERFORM LPSTAT-DISPLAY AFTER READING BWFILE.
      *  020497 BLV FIX PRINTING INS CO'S OUT OF RANGE
      *  BLV 060597 CHANGED FOR LOCAL SPOOL WITH NEW BATCH DRIVER - BREXEC
      *  GLF 090297 REMOVED ALL USE OF GLOBAL
      *  JTG 091897 ADDED TEST FOR TRCD = 'AH', WORLD A90
      *  011598 BAH ADDED LOGIC TO BREAK OUT TOTALS FOR SDL, JDL, SLL, AND
      *             JLL FROM A48. PR# 519 MESSY...MESSY..MESSY !!
      *             WILL ALWAYS PRINT BRANCH AND COMPANY TOTALS....SORRY
      *  BLV 042998 WAS DISPLAYING BEG-IC & END-IC FOR BEG & ENDING CLASS
      *  BAH 990719 NEEDS TO TEST SP-INSMAXBENIF(5) FOR A&H REGPYAMT
      *             REMOVE TEST ON SP-INSCHGABL TO SUBTRACT MAINTFEE,
      *             ALWAYS SUBTRACT MAINTFEE, REGENCY PR# 55
      *  GLF 082499 ADD ARCHIVE PATH INFO. FOR TR FILES NOT FOUND IN DATA
      *             SET FOR SELECTED MONTH AND YEAR
      *  BLV 990826 FIX ARCHIVE PATH LOGIC
      *  BAH 990908 PROBLEM WITH CL AND AH WRITTEN THRU 2 DIFFERENT COMPANIES
      *             NEEDED TO CREATE 2 SEPARATE WORK RECORDS, REGMGM, JR
      *  BAH 991103 WK-QTY WAS NOT SIGNED, IF THE FIRST ACCOUNT WAS A VOID,
      *             THE WK-QTY WAS 1 INSTEAD OF -1, CAUSING BAD TOTALS
      *             KEVIN, REGACC
      *  BAH 000314 AMERICAN THRIFT NEEDS TO RUN IF INSOURS = "N", ADDED
      *             GP-I-AM-AMERICAN, PR# 793
      *  BAH 000621 ADDED THE "AND" TO THIS "IF" IN KEY-DETAIL SECTION
      *             IF LN-ACCTNO = WORK-ACCTNO AND (LN-OWNBR = WORK-OWNBR)
      *             REGENCY, KY0413 LAST ACCT # 500037, AND KY0414 FIRST
      *             ACCT # 500037 AND THE KEY-DETAIL WAS ADDING THESE 2
      *             ACCTS INFO TOGETHER, PL# 287
      *  BAH 010319 ADDED BRANCH SECURITY PASSWORD TESTING (BRSECURE)
      *  010809 BAH ADDED TEST IN THE READ-TR LOOP TO ONLY INCLUDE OWNING
      *             BRANCH RECORDS, ANOTHER BRANCH PAYOFF WITH THE SAME
      *             ACCTNO WAS BEING INCLUDED ON BOTH BRANCHES REPORT ! AJ
      *  BAH 011113 ADDED TP-INSCOMP FOR ADDON/REVERSE/ADDON WITH DIFFERENT
      *             INSURANCE COMPANIES, REGACC PR# 1617
      *  BAH 020501 BUG FROM 010809 CHANGE, TESTED TP-OTHBR ON THE "L" AND "S"
      *             INSTEAD OF JUST "P" RECORDS, ACTUALLY CAUGHT A "C"
      *             IN THE TL-EFFDATE(3) FIELD FOR 04/30/02 STORED AS
      *             00 43 00 2F !! REGWAN PL#365
      *  BAH 020722 TEST TP-INSOURS INSTEAD OF THE LN-INSOURS(1 OR 2), ON A
      *             REVERSAL OF AN ADDON, THE LOAN RECORD INSURANCE IS
      *             BLANK (KEVIN)!!
      *  BAH 030714 ADDED SUMMARY-ONLY OPTION OF "I" FOR INSURANCE TOTALS
      *              ONLY, NO BRANCH OR STATE, LENDMARK PR# 2113
      *  BAH 040123 WORLD "KY" INSURANCE TAX WAS PART OF THE REFUND
      *             BEING REPORTED, NEEDED TO BE SUBTRACTED OUT !!
      *                            ((TP-TRAMT - TP-INS-TAX-RB) * -1)
      *  BAH 040611 DONT PRINT RANGE LINES UNLESS RECORDS ARE FOUND...JOSH
      *  BAH 041201 INSURANCE COMPANY RANGE TEST DIDNT WORK....EXAMPLE:
      *             NO LIFE, AND A&H COMPANY WAS OUT OF RANGE, ACCOUNT WAS
      *             INCLUDED IN THE REPORT, WORLD..
      *   CS 050225 ADD NET PREMUIMS (WORLD PR#385)
      *  BAH 051111 ADD TOTAL COUNT OF REFUNDS BELOW TOTAL COUNT OF NEW
      *             PREMIUMS, FRANEK PR# 2642
      *   CS 060808 WHEN INS CO. CHANGED, PART OF THE DETAIL LINE (FROM TOTALS)
      *             WAS CARRYING DOWN TO THE ACCOUNT DETAIL FOR THE NEXT INS CO.
      *             CHANGED THE 'IF' ON PRINT TO EITHER PRINT OR MOVE SPACES
      *                                                     COLTN PR#548
      * BAH 170605 ACTIVATE 8 DIGIT DATES, PD0006
      * BAH 181210 GLOBAL LNFILE, L2G
      * BAH 20190925 REMOVED ACCESS-CALL LNFILE, TRPFILE, TRLFILE, TRS
      * BAH 20200827 REMOVED RESET ON FIRST FIELD
      * BLM 20210810 REMOVE TP-CODE, TP-POOLID NO LONGER IN KEY
      * RMZ 20230725 PERFORMANCE TUNING - ADDED STORED PROCEDURE
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "LIBGB/GBFSPR.CPY".
      * VALID LOANS FILE
       COPY "LIBGB/GBFSWKI.CPY".
      * TOTALS FILE
           SELECT WK1-FILE ASSIGN WK1-PATH
                  ORGANIZATION INDEXED
                  ACCESS DYNAMIC
                  LOCK MODE AUTOMATIC WITH LOCK ON RECORD
                  RECORD KEY WK1-KEY
                  FILE STATUS FILE-STAT.
       I-O-CONTROL.
       DATA DIVISION.
       FILE SECTION.

       COPY "LIBGB/GBFDPR.CPY".

      ******************************************************************
      *   SUMMARY TOTALS WORK RECORD
      ******************************************************************
       FD  WK1-FILE
           LABEL RECORDS ARE STANDARD.
       01  WK1-REC.
           03  WK1-KEY.
               05  WK-CODE     PIC 9.
               05  WK-IC       PIC 99.
               05  WK-ST.
                   07  WK-BRNO PIC 9(4).
               05  WK-CLASS    PIC 9(3).
      *--------------------------------------------------
      * WORK WORKERS: 1 - SDL - SINGLE DECLINING LIFE
      *               2 - JDL - JOINT DECLINING LIFE
      *               3 - SLL - SINGLE LEVEL LIFE
      *               4 - JLL - JOINT LEVEL LIFE
      * QTY (1) NEW CL (2) NEW AH  (3) REF CL  (4) REF AH
      *--------------------------------------------------
           03  WK1-TOT-TABLE.
               05  WK-WORKERS   OCCURS 4.
                   07  WK-QTY    PIC S9(7)    COMP-3 OCCURS 4.
                   07  WK-AMT    PIC S9(9)V99 COMP-3 OCCURS 13.

      ******************************************************************
      *   WORK FILE FOR VALID LOANS
      ******************************************************************
       FD  WK-FILE
           LABEL RECORDS ARE STANDARD.
       01  WK-REC.
           03  WK-KEY.
               05  WORK-INSCOMP     PIC 99.
               05  WORK-OWNBR       PIC 9(4).
               05  WORK-CLASS       PIC 9(3).
               05  WORK-ACCTNO      PIC 9(8).
               05  WORK-DATE        PIC 9(8).
      *--------------------------------------------------
      * WORK WORKERS: 1 - SDL - SINGLE DECLINING LIFE
      *               2 - JDL - JOINT DECLINING LIFE
      *               3 - SLL - SINGLE LEVEL LIFE
      *               4 - JLL - JOINT LEVEL LIFE
      *--------------------------------------------------
           03  WORK-WORKERS  OCCURS 4.
               05  WORK-NEW-CL      PIC S9(5)V99 COMP-3.
               05  WORK-ADJ-CL      PIC S9(5)V99 COMP-3.
               05  WORK-CAN-CL      PIC S9(5)V99 COMP-3.
               05  WORK-COM-CL      PIC S9(5)V99 COMP-3.
               05  WORK-NEW-AH      PIC S9(5)V99 COMP-3.
               05  WORK-ADJ-AH      PIC S9(5)V99 COMP-3.
               05  WORK-CAN-AH      PIC S9(5)V99 COMP-3.
               05  WORK-COM-AH      PIC S9(5)V99 COMP-3.
           03  WORK-CAN-CL-DATE     PIC 9(8) COMP-3.
           03  WORK-CAN-AH-DATE     PIC 9(8) COMP-3.
           03  WORK-ADDON-FG-CL     PIC X.
           03  WORK-CANCEL-FG-CL    PIC X.
           03  WORK-ADDON-FG-AH     PIC X.
           03  WORK-CANCEL-FG-AH    PIC X.
      ******************************************************************
      *
      *     W O R K I N G - S T O R A G E    S E C T I O N
      *
      ******************************************************************
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)LP/ICMORP.CBL S34 08/10/21-20:55:23 >".

       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBGB/GB01GR.CPY".
       COPY "LIBGB/GB01GR_SQL.CPY".
       COPY "LIBGB/GBWSGR.CPY".
       COPY "LIBLP/LP01TRL.CPY".
       COPY "LIBLP/LP01TRL_SQL.CPY".
       COPY "LIBLP/LPWSTRL.CPY".
       COPY "LIBLP/LP01TRP.CPY".
       COPY "LIBLP/LP01TRP_SQL.CPY".
       COPY "LIBLP/LPWSTRP.CPY".
       COPY "LIBLP/LP01TRS.CPY".
       COPY "LIBLP/LP01TRS_SQL.CPY".
       COPY "LIBLP/LPWSTRS.CPY".
       COPY "LIBLP/LP01LN.CPY".
       COPY "LIBLP/LP01LN_SQL.CPY".
       COPY "LIBLP/LPWSLN.CPY".
       COPY "LIBLP/LP01IC.CPY".
       COPY "LIBLP/LP01IC_SQL.CPY".
       COPY "LIBLP/LPWSIC.CPY".
       COPY "LIBLP/LP01BW.CPY".
       COPY "LIBLP/LP01BW_SQL.CPY".
       COPY "LIBLP/LPWSBW.CPY".
       COPY "LIBLP/LP01SP.CPY".
       COPY "LIBLP/LP01SPA.CPY".
       COPY "LIBLP/LP01SPB.CPY".
       COPY "LIBLP/LP01SPC.CPY".
       COPY "LIBLP/LP01SP_SQL.CPY".
       COPY "LIBLP/LPWSSP.CPY".
      *-----------------------------------------------------------------
       01  WK1-IFN              PIC X(3)     VALUE "WK1".
       01  WK-IFN               PIC X(2)     VALUE "WK".
       01  WK1-PATH             PIC X(35).

       01  TRP1-CONN-OPEN-SW    PIC X        VALUE "N".
       01  TRL1-CONN-OPEN-SW    PIC X        VALUE "N".

      ******************************************************************
      *   LINE WORKERS
      ******************************************************************
       01  LN-WORKERS.
           03  LN-FEED          PIC 999      VALUE 0.
           03  LN-COUNT         PIC 999      VALUE 99.
           03  LN-WK            PIC 999      VALUE 0.
       01  PAGE-NUMBER          PIC 9(4)     VALUE 0.

       01  BR-PAGE-NUMBER       PIC 9(4) COMP-3 VALUE 0.

      ******************************************************************
      *   MISC WORKERS
      ******************************************************************
       01  SUB                  PIC 9(2)     COMP-3.
      *    --------------------------------------------------------
      *    ADDED SUBSCRIPTS FOR BREAKDOWN OF SDL, JDL, SLL, AND JLL.
      *    --------------------------------------------------------
       01  SUB1                 PIC 9(2)     COMP-3.
       01  SUB2                 PIC 9(2)     COMP-3.
       01  SUB3                 PIC 9(2)     COMP-3.
       01  T-LEV                PIC 9(2)     COMP-3.
      *    --------------------------------------------------------
       01  FIRST-TIME           PIC X        VALUE "Y".
       01  RECORDS-FOUND        PIC X        VALUE " ".
           88  NO-RECORDS-FOUND              VALUE "N".
       01  MESS-LIST            PIC X(5)     VALUE "MSEL.".
       01  HOLD-IC              PIC 99       COMP-3 VALUE 0.
       01  HOLD-BR              PIC 9(4)     COMP-3 VALUE 0.
       01  HOLD-LN-BR           PIC 9(4)     COMP-3 VALUE 9999.
       01  SAVE-WK1-KEY         PIC X(10).
       01  SAVE-CODE            PIC 9.
       01  SAVE-ST.
           03  SAVE-BR          PIC 9(4).
       01  LAST-ST              PIC XX.
       01  FIRST-TOT            PIC X.
       01  DASH-LINE            PIC X(132)   VALUE ALL "-".

       01  REC-EXISTS           PIC X        VALUE "N".
       01  TOTAL-FG             PIC X        VALUE "N".
       01  COMPANY-FG           PIC X        VALUE "N".

       01  ONE-CL               PIC S9 VALUE 0.
       01  ONE-AH               PIC S9 VALUE 0.
       01  REF-CL               PIC S9 VALUE 0.
       01  REF-AH               PIC S9 VALUE 0.
       01  NEW-COVERAGE         PIC S9(7)V99 COMP-3 VALUE 0.
       01  RPT-END-DATE         PIC 9(8)     COMP-3 VALUE 0.
       01  BEGINNING-DATE       PIC 9(8)     COMP-3 VALUE 0.

       01  LIFE-ADDON-FG        PIC X.
       01  LIFE-CANCEL-FG       PIC X.
       01  LIFE-PREM            PIC S9(7)V99 COMP-3.
       01  LIFE-ADJ             PIC S9(7)V99 COMP-3.
       01  LIFE-CAN             PIC S9(7)V99 COMP-3.
       01  LIFE-CAN-DATE        PIC 9(8)     COMP-3.
       01  LIFE-COMM            PIC S9(7)V99 COMP-3.
       01  AH-ADDON-FG          PIC X.
       01  AH-CANCEL-FG         PIC X.
       01  AH-PREM              PIC S9(7)V99 COMP-3.
       01  AH-ADJ               PIC S9(7)V99 COMP-3.
       01  AH-CAN               PIC S9(7)V99 COMP-3.
       01  AH-CAN-DATE          PIC 9(8)     COMP-3.
       01  AH-COMM              PIC S9(7)V99 COMP-3.

       01  LINE-TOTALS.
           03  TOT-QTY          PIC S9(7)    COMP-3 OCCURS 4.
           03  TOT-AMT          PIC S9(9)V99 COMP-3 OCCURS 13.
       01  TOT-WK-WORKERS.
           03  TOT-WK-QTY        PIC S9(7)    COMP-3 OCCURS 4.
           03  TOT-WK-AMT        PIC S9(9)V99 COMP-3 OCCURS 13.

       01  CLASS-FG             PIC X(3)    VALUE "   ".
           88  CLASS-BEG                    VALUE "BEG".
           88  CLASS-END                    VALUE "END".
      ******************************************************************
      *     S C R E E N   F I E L D S
      ******************************************************************
      *-----------------------------------------------------------------
       COPY "LIBGB/EOBUF_EXT.CPY".
      *-----------------------------------------------------------------
           03  BEG-BRANCH      PIC 9(4).
           03  END-BRANCH      PIC 9(4).
           03  BEG-IC          PIC 99.
           03  END-IC          PIC 99.
           03  BEG-CLASS       PIC 99.
           03  END-CLASS       PIC 99.
           03  BEG-ST          PIC XX.
           03  END-ST          PIC XX.
           03  MMYY            PIC 9(4).
           03  MM-YY REDEFINES MMYY.
               05  MM          PIC 9(2).
               05  YY          PIC 9(2).
           03  SUMMARY-ONLY    PIC X.
           03  GROUP-FLAG      PIC X.
           03  ENT-GROUP       PIC X(4).

       01  VERSION-CONTROL     PIC X      VALUE "A".

       01  SV-BEG-BR           PIC 9999   VALUE 9999.
       01  SV-END-BR           PIC 9999   VALUE 9999.
       01  SV-GROUP-FLAG       PIC X     VALUE SPACES.
       01  SV-ENT-GROUP        PIC X(4)  VALUE SPACES.

      *----------------------------------------------------------------
      * TOTAL TABLE
      *  TOTAL LEV-TABLE:  1 - SDL
      *                    2 - JDL
      *                    3 - SLL
      *                    4 - JLL
      * QTY  (1) NEW CL (2) NEW AH (3) REF CL (4) REF AH
      *----------------------------------------------------------------
       01  TOTAL-TABLE.
           03  TOTAL-LEV-TABLE OCCURS 4.
               05  T-QTY           PIC S9(7)    OCCURS 4.
               05  T-AMT           PIC S9(9)V99 OCCURS 13.

      ******************************************************************
      *     P R I N T   B U F F E R S
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
           03  FILLER           PIC X(40) VALUE
           " - - LIFE/A&H PREMIUM-REFUND REPORT - - ".

      *-----------------------------------------------------------------
       01  HEAD3.
           03  H-INSCO          PIC X(10) VALUE "INS CO :".
           03  H-ICNO           PIC ZZ.
           03  FILLER           PIC X VALUE " ".
           03  H-ICNAME         PIC X(20) VALUE " ".
           03  FILLER           PIC X(23) VALUE " ".
           03  FILLER           PIC X(13) VALUE "FOR MONTH OF ".
           03  H-MMYY           PIC 99/99.

      *-----------------------------------------------------------------
       01  HEAD4.
           03  H-PAGE-DESC      PIC X(46) VALUE " ".
           03  H-STNO                 VALUE " ".
               05  H-BRNO       PIC Z(4).
           03  FILLER           PIC X VALUE " ".
           03  H-BRNAME         PIC X(20) VALUE " ".
           03  FILLER           PIC X(45) VALUE " ".
           03  H-PAGE-PROMPT    PIC X(12).
           03  H-BR-PAGE        PIC ZZZZ.

      *-----------------------------------------------------------------
       01  HEAD5.
           03  FILLER           PIC X(51) VALUE
           "ACCOUNT #      BORROWER           AGE LIFE EFF TRM ".
           03  FILLER           PIC X(53) VALUE
           "LIFE EXP LIFE COVR  LIFE BNFT  LIFE PREM   LIFE ADJ  ".
           03  FILLER           PIC X(27) VALUE
           "LIFE REF----DATE  LIFE COMM".

      *-----------------------------------------------------------------
       01  HEAD6.
           03  FILLER           PIC X(51) VALUE
           "                                  WHO A&H EFF".
           03  FILLER           PIC X(53) VALUE
           " A&H EXP  A&H COVR   A&H BNFT   A&H PREM    A&H ADJ  ".
           03  FILLER           PIC X(27) VALUE
           " A&H REF----DATE   A&H COMM".

      *-----------------------------------------------------------------
       01  HEAD7.
           03  FILLER           PIC X(50) VALUE
           "TOT  ** T O T A L S *            NEW LOAN      LIF".
           03  FILLER           PIC X(51) VALUE
           "E PREM     LIFE ADDON       LIFE ADJ     LIFE CANCL".
           03  FILLER           PIC X(30) VALUE
           "       LIFE REF      LIFE COMM".

      *-----------------------------------------------------------------
       01  HEAD8.
           03  FILLER           PIC X(50) VALUE
           "CLS  LIFE   A&H   TOT            COVERAGE       A&".
           03  FILLER           PIC X(51) VALUE
           "H PREM      A&H ADDON        A&H ADJ      A&H CANCL".
           03  FILLER           PIC X(30) VALUE
           "        A&H REF       A&H COMM".
      *-----------------------------------------------------------------
       01  HEAD9.
           03  FILLER           PIC X(101) VALUE SPACES.
           03  FILLER           PIC X(30) VALUE
           "     NET CL PRM     NET AH PRM".

      ******************************************************************
      *    D E T A I L - L I N E
      ******************************************************************
       01  DETAIL-LINE          PIC X(132) VALUE SPACES.

      *-----------------------------------------------------------------
       01  DETAIL1 REDEFINES DETAIL-LINE.
           03  D-LOAN-NUM       PIC X(14).
           03  D-NUM REDEFINES D-LOAN-NUM.
               05  D-CLASS      PIC 99.
               05  FILLER       PIC X(12).
           03  FILLER           PIC X.
           03  D-NAME           PIC X(18).
           03  FILLER           PIC X.
           03  D-AGE            PIC ZZ9.
           03  D-XX REDEFINES D-AGE.
               05  D-LEVEL      PIC X.
               05  FILLER       PIC X.
               05  D-WHO        PIC X.
           03  FILLER           PIC X.
           03  D-EFFDATE        PIC 99/99/99   BLANK ZERO.
           03  FILLER           PIC X.
           03  D-TERM           PIC ZZ9        BLANK ZERO.
           03  FILLER           PIC X.
           03  D-EXPDATE        PIC 99/99/99   BLANK ZERO.
           03  FILLER           PIC X.
           03  D-COVRG          PIC Z(5)9.99-  BLANK ZERO.
           03  FILLER           PIC X.
           03  D-REGPYAMT       PIC Z(5)9.99-  BLANK ZERO.
           03  FILLER           PIC X.
           03  D-INSPREM        PIC Z(5)9.99-   BLANK ZERO.
           03  D-ADDTEXT        PIC X.
           03  D-INSADJ         PIC Z(5).99-   BLANK ZERO.
           03  FILLER           PIC X.
           03  D-INSREFUND      PIC Z(5).99-   BLANK ZERO.
           03  D-CNLTEXT        PIC X.
           03  FILLER           PIC X.
           03  D-REFUND-DATE    PIC 99/99/99   BLANK ZERO.
           03  FILLER           PIC X.
           03  D-INSCOMM        PIC Z(5).99-   BLANK ZERO.

      *-----------------------------------------------------------------
       01  DETAIL2 REDEFINES DETAIL-LINE.
           03  FILLER           PIC X(3).
           03  D-QTY            PIC Z(5)9- OCCURS 3.
           03  D-TAG            PIC X(3).
           03  D-AMT            PIC ZZZ,ZZZ,ZZ9.99- OCCURS 7.

      *-----------------------------------------------------------------
       01  RANGE-LINE REDEFINES DETAIL-LINE.
           03  FILLER           PIC X(20).
           03  RANGE-SEL        PIC X(112).
      ******************************************************************
      *
      *      C O P Y    L I B R A R Y    W O R K E R S
      *
      ******************************************************************
       COPY "LIBWI/PFSWINDOWSW.CPY".
       COPY "LIBWI/TSTINTW.CPY".
       COPY "LIBWI/GRSCANW.CPY".
       COPY "LIBWI/LCSCANW.CPY".
       COPY "LIBLP/LPSTATW.CPY".
       COPY "LIBLP/LPMDTEW.CPY".
       COPY "LIBLP/LPITRMW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBLP/LPBAGEW.CPY".
       COPY "LIBGB/GETPRW.CPY".
       COPY "LIBGB/PASSWDW.CPY".
       COPY "LIBGB/BRSECUREW.CPY".
       COPY "LIBGB/MKTEMPW_CMD.CPY".
       COPY "LIBGB/SYSTEMW.CPY".
       COPY "LIBGB/GRWORKW.CPY".
       COPY "LIBGB/ACCESSW.CPY".

       COPY "LIBGB/DACCESSW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBLP/ICMORP_DEF.CPY".
       COPY "LIBLP/ICMORP_WKS.CPY".
       COPY "LIBLP/ARRAYSPW.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

      ******************************************************************
      *
      *     P R O G R A M   C O N T R O L
      *
      ******************************************************************
       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBLP/ICMORP_SCN.CPY".

       PROCEDURE DIVISION
             USING FORM-PATHNAME EXIT-PATHNAME.

       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
               PR-FILE WK1-FILE WK-FILE.
       COPY "LIBGB/DECLRP.CPY".
           PERFORM CLOSE-TRS1-FILE.
           PERFORM CLOSE-TRP1-FILE.
           PERFORM CLOSE-TRP1-CONNECTION.
           PERFORM CLOSE-TRL1-FILE.
           PERFORM CLOSE-TRL1-CONNECTION.
      *     PERFORM CLOSE-SP1-FILE.
           PERFORM CLOSE-LN1-FILE.
           PERFORM CLOSE-IC1-FILE.
           PERFORM CLOSE-BW1-FILE.
           PERFORM CLOSE-GR1-FILE.
           PERFORM CLOSE-BR-FILE.
           PERFORM CLOSE-PR-FILE.
           CLOSE WK1-FILE WK-FILE.
       COPY "LIBGB/DECLRP2.CPY".

      ******************************************************************
      *
      *     M A I N    M O D U L E
      *
      ******************************************************************
       MAIN-MODULE SECTION.

           PERFORM SQL-CONNECT.

           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "ICMORP" TO VDU-FORM-NAME.

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
              GO TO PROCESS-REPORT.

           MOVE WGR-SRESULT TO BR-NO.
           PERFORM READ-BR-FILE.
           IF IO-FG NOT = 0
              GO TO IO-ERROR.

           GO TO MOVE-BRANCH-PATH-INFO.

       NEXT-BRANCH-NO.
           PERFORM READ-BR-FILE-NEXT.
           IF IO-FG = 9 OR BR-NO > END-BRANCH
              GO TO PROCESS-REPORT.

       MOVE-BRANCH-PATH-INFO.

           IF EXT-ROUTE-BUF = "00"
              MOVE SPACES TO WR-BUF
              STRING "REPORT IN PROGRESS...............", BR-NO
                     DELIMITED "  " INTO WR-BUF
              MOVE MESS-LIST TO WR-LIST
              PERFORM CALL-WRFORM.

           MOVE EXT-FILPATH-BASE           TO FILPATH-BASE.
           MOVE BR-MACHINE                 TO FILPATH-MACHINE.
           MOVE BR-DATA-PATH               TO FILPATH-DATA.

           MOVE "B"                        TO BW-PATH-OVERRIDE
                                              IC-PATH-OVERRIDE
                                              LN-PATH-OVERRIDE
                                              TRP-PATH-OVERRIDE
                                              TRL-PATH-OVERRIDE
                                              TRS-PATH-OVERRIDE.

           PERFORM OPEN-LN1-FILE.

           PERFORM SORT-TRP-FILE.
           PERFORM SORT-TRL-FILE.
           PERFORM SORT-TRS-FILE.

       END-OF-GROUP.
           PERFORM CLOSE-LN1-FILE.
           GO TO NEXT-GROUP-LOC.

       PROCESS-REPORT.
           PERFORM CLOSE-WK-FILE.
           PERFORM OPEN-WK-FILE.

           PERFORM RUN-REPORT.

      *-----------------------------------------------------------------
       END-ROUTINE.
           IF FIRST-TIME = "Y"
              MOVE "N" TO RECORDS-FOUND
           ELSE
              MOVE "Y" TO COMPANY-FG
              PERFORM GB-TOTALS
              PERFORM RANGE-LINES.

           GO TO END-PROGRAM.

      ******************************************************************
       INITIALIZATION SECTION.
      ******************************************************************
           MOVE ICMORP-QUEUE-POS TO Q-POS.
           MOVE ICMORP-COPIES-POS TO C-POS.
           MOVE ICMORP-PRU-POS   TO P-POS.
           MOVE SPACES TO Q-BUF C-BUF P-BUF.

       COPY "LIBGB/EOINIT.CPY".

      * CREATE TOTALS WORK FILE
           MOVE MKTEMP-DEFAULT TO MKTEMP-BUF.
           PERFORM MKTEMP-CALL.
           MOVE MKTEMP-BUF TO WK1-PATH.
           PERFORM OPEN-WK1-FILE-OUTPUT.
           PERFORM CLOSE-WK1-FILE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      * CREATE WORK FILE FOR VALID LOANS
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

           MOVE MKTEMP-DEFAULT TO MKTEMP-BUF.
           PERFORM MKTEMP-CALL.

           IF STAT NOT = "00"
              MOVE "CANNOT MAKE WORK FILE" TO MESS
              PERFORM SEND-MESS
              GO TO INITIALIZE-ENDIT.

           PERFORM OPEN-WK-FILE-OUTPUT.
           PERFORM CLOSE-WK-FILE.
           PERFORM OPEN-WK-FILE.

      *-----------------------------------------------------------------
       INITIALIZE-BEG-RPT.
      *    IF EXT-ROUTE-BUF = "00"
      *       MOVE "INSURANCE REPORT IN PROGRESS......." TO MESS
      *       PERFORM SEND-LEGEND.

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

      * RPT-END-DATE IS ALREADY SET, JUST NEED A BEGINNING FOR TR
           MOVE MM TO DATE-MMDDYY-MM.
           MOVE YY TO DATE-MMDDYY-YY.
           MOVE 01 TO DATE-MMDDYY-DD.
           PERFORM CONVERT-MMDDYY-TO-YYYYMMDD.
           MOVE DATE-YYYYMMDD TO BEGINNING-DATE.

           PERFORM OPEN-WK1-FILE.
           PERFORM OPEN-BR-FILE.
           PERFORM OPEN-GR1-FILE.
      *     PERFORM OPEN-SP1-FILE.
           PERFORM ARRAYSP-LOAD.
           PERFORM OPEN-PR-FILE.
           PERFORM ENABLE-INTERRUPT.

           MOVE EXT-CONAME-CONAME TO H-NAME.
           MOVE EXT-CONAME-SYSDATE TO H-DATE.
           MOVE EXT-CONAME-USER-ID TO H-USERID.
           MOVE MMYY TO H-MMYY.

           PERFORM CLEAR-TOTALS.

           GO TO INITIALIZE-EXIT.

      *-----------------------------------------------------------------
       INITIALIZE-ENDIT.
           MOVE "X" TO EXT-EOBUF-ERRCD.

       INITIALIZE-EXIT.
           EXIT.

      ******************************************************************
       EO-EXEC-AUDIT SECTION.
      ******************************************************************
           MOVE EXT-EOBUF-DATE2 TO NDTE-DATE RPT-END-DATE.
           MOVE NDTE-MM TO MM.
           MOVE NDTE-YY TO YY.

      ******************************************************************
       ENTER-PARAMETERS SECTION.
      ******************************************************************
       ENTER-PARAM.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      * FOR RE-ENTRY ON F4 FROM ACCEPT.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

           IF VDUSTATUS = "1U" GO TO SO-01.

      *-----------------------------------------------------------------
       BR-01.
           MOVE "E  A000BR1." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "16" GO TO ALL-BR.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.

           IF VDUSTATUS = "1<"
              MOVE GRSCAN-BUF TO SCAN-BUF
              PERFORM CALL-WINDOW-PROGRAM
              MOVE SCAN-BUF TO GRSCAN-BUF
              IF SCAN-STATUS NOT = "00"
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

      *-----------------------------------------------------------------
       IC-01.
           IF ( (BEG-BRANCH = 0) AND (END-BRANCH = 0) )
              MOVE "S  A160DISPLAY-USER." TO VDU
              MOVE "USER BRANCH ONLY" TO VDUBUF
              PERFORM SCREENIO
           ELSE
              MOVE "S  A160DISPLAY-USER." TO VDU
              MOVE SPACES TO VDUBUF
              PERFORM SCREENIO.

           MOVE "ENNN020IC1." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "16" GO TO ALL-IC.
           IF VDUSTATUS = "1U" GO TO BR-02.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO IC-01.
           MOVE VDUNUM0 TO BEG-IC.

      *-----------------------------------------------------------------
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

      *-----------------------------------------------------------------
       ALL-IC.
           MOVE "SNNN020IC1." TO VDU.
           MOVE 1 TO BEG-IC VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN020IC2." TO VDU.
           MOVE 99 TO END-IC VDUNUM0.
           PERFORM SCREENIO.

      *-----------------------------------------------------------------
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

      *-----------------------------------------------------------------
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

      *-----------------------------------------------------------------
       ALL-LC.
           MOVE "SNNN020LC1." TO VDU.
           MOVE 1 TO BEG-CLASS VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN020LC2." TO VDU.
           MOVE 99 TO END-CLASS VDUNUM0.
           PERFORM SCREENIO.

      *-----------------------------------------------------------------
       ST-01.
           MOVE "E  A020ST1." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "16" GO TO ALL-ST.
           IF VDUSTATUS = "1U" GO TO LC-02.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO ST-01.
           MOVE VDUBUF TO BEG-ST.

      *-----------------------------------------------------------------
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

      *-----------------------------------------------------------------
       ALL-ST.
           MOVE "S  A020ST1." TO VDU.
           MOVE "  " TO BEG-ST VDUBUF.
           PERFORM SCREENIO.
           MOVE "S  A020ST2." TO VDU.
           MOVE ALL "Z" TO END-ST.
           CALL "C$TOLOWER" USING END-ST, VALUE 2.
           MOVE END-ST TO VDUBUF.
           PERFORM SCREENIO.

      *-----------------------------------------------------------------
       DT-01.
           IF EXT-ROUTE-BUF = "40" OR "50"
              GO TO SO-01.

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

           MOVE MM TO DATE-MMDDYY-MM.
           MOVE 33 TO DATE-MMDDYY-DD.
           MOVE YY TO DATE-MMDDYY-YY.
           PERFORM CONVERT-MMDDYY-TO-YYYYMMDD.
           MOVE DATE-YYYYMMDD TO NDTE-DATE.
           PERFORM NEW-DATE-CALCULATION.
           MOVE NDTE-DATE TO RPT-END-DATE.

      *-----------------------------------------------------------------
       SO-01.
           MOVE "E  A010SO1." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO DT-01.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO SO-01.
           MOVE VDUBUF TO SUMMARY-ONLY.
           IF NOT (SUMMARY-ONLY = "Y" OR "N" OR "I") GO TO SO-01.

       EXIT-PARM.
           EXIT.

      ******************************************************************
       DISPLAY-PARAMETERS SECTION.
      ******************************************************************
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
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "SNNN020IC1." TO VDU.
           MOVE BEG-IC TO VDUNUM0.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "SNNN020IC2." TO VDU.
           MOVE END-IC TO VDUNUM0.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "SNNN020LC1." TO VDU.
           MOVE BEG-CLASS TO VDUNUM0.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "SNNN020LC2." TO VDU.
           MOVE END-CLASS TO VDUNUM0.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "S  A020ST1." TO VDU.
           MOVE BEG-ST TO VDUBUF.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "S  A020ST2." TO VDU.
           MOVE END-ST TO VDUBUF.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "S  A010SO1." TO VDU.
           MOVE SUMMARY-ONLY TO VDUBUF.
           PERFORM SCREENIO.

      ******************************************************************
       RUN-REPORT SECTION.
      ******************************************************************
           MOVE 0 TO LIFE-PREM LIFE-ADJ LIFE-CAN
                     LIFE-COMM LIFE-CAN-DATE.

           MOVE 0 TO AH-PREM AH-ADJ AH-CAN AH-COMM AH-CAN-DATE.
           MOVE SPACES TO LIFE-ADDON-FG LIFE-CANCEL-FG.
           MOVE SPACES TO AH-ADDON-FG AH-CANCEL-FG.

           PERFORM READ-WK-FILE-NEXT.
           IF IO-FG = 9
              GO TO END-RUN-REPORT.

           IF WORK-OWNBR NOT = HOLD-LN-BR
              MOVE WORK-OWNBR TO HOLD-LN-BR BR-NO
              PERFORM GET-LN-BW-PATH.

           MOVE WORK-OWNBR  TO LN-OWNBR.
           MOVE WORK-ACCTNO TO LN-ACCTNO.
           PERFORM READ-LN1-FILE.
           IF IO-FG NOT = 0
              GO TO END-RUN-REPORT.

      *-----------------------------------------------------------------
       KEY-FILE-LOOP.
           IF (LN-INSURED = "M" OR "S") AND LN-INSTYPES-DL
              MOVE 1 TO SUB1
           ELSE
           IF (LN-INSURED = "M" OR "S") AND LN-INSTYPES-LL
              MOVE 3 TO SUB1
           ELSE
           IF LN-INSURED = "J" AND LN-INSTYPES-DL
              MOVE 2 TO SUB1
           ELSE
           IF LN-INSURED = "J" AND LN-INSTYPES-LL
              MOVE 4 TO SUB1
           ELSE
      * NEEDED A DEFAULT FOR A&H ONLY !!!!!!! 4/8/97
              MOVE 1 TO SUB1.

           ADD WORK-NEW-CL(SUB1) TO LIFE-PREM.
           IF WORK-NEW-CL(SUB1) NOT = 0
              MOVE WORK-ADDON-FG-CL TO LIFE-ADDON-FG.

           ADD WORK-ADJ-CL(SUB1) TO LIFE-ADJ.
           ADD WORK-CAN-CL(SUB1) TO LIFE-CAN.
           IF WORK-CAN-CL(SUB1) NOT = 0
              MOVE WORK-CANCEL-FG-CL TO LIFE-CANCEL-FG.

           ADD WORK-COM-CL(SUB1) TO LIFE-COMM.

           IF WORK-CAN-CL-DATE NOT = 0
              MOVE WORK-CAN-CL-DATE TO LIFE-CAN-DATE.

           ADD WORK-NEW-AH(SUB1) TO AH-PREM.
           IF WORK-NEW-AH(SUB1) NOT = 0
              MOVE WORK-ADDON-FG-AH TO AH-ADDON-FG.

           ADD WORK-ADJ-AH(SUB1) TO AH-ADJ.
           ADD WORK-CAN-AH(SUB1) TO AH-CAN.
           IF WORK-CAN-AH(SUB1) NOT = 0
              MOVE WORK-CANCEL-FG-AH TO AH-CANCEL-FG.

           ADD WORK-COM-AH(SUB1) TO AH-COMM.

           IF WORK-CAN-AH-DATE NOT = 0
              MOVE WORK-CAN-AH-DATE TO AH-CAN-DATE.

           IF FIRST-TIME = "Y"
              MOVE "N" TO FIRST-TIME
              MOVE WORK-INSCOMP TO HOLD-IC
              MOVE LN-OWNBR TO HOLD-BR
              PERFORM IC-HEADER
              PERFORM BR-HEADER.

           IF WORK-INSCOMP NOT = HOLD-IC
              PERFORM IC-TOTALS.

           IF LN-OWNBR NOT = HOLD-BR
              PERFORM BR-TOTALS.

           MOVE LN-ORGST    TO SP-ORGST.
           MOVE LN-SPRCLASS TO SP-SPRCLASS.
           MOVE LN-SUBCLASS TO SP-SUBCLASS.
           MOVE LN-LAWCODE  TO SP-LAWCODE.
           IF SP1-KEY NOT = LPWSSP-SP1-KEY
              MOVE SP1-KEY TO LPWSSP-SP1-KEY
      *        PERFORM READ-SP1-FILE
              PERFORM ARRAYSP-READ
              IF IO-FG = 9
                 GO TO IO-ERROR.

      *-----------------------------------------------------------------
       NEXT-KEY-REPORT.
           PERFORM TRAP-INTERRUPT.
           PERFORM READ-WK-FILE-NEXT.
           IF IO-FG = 9
              MOVE 0 TO WORK-ACCTNO
              PERFORM KEY-DETAIL
              GO TO END-RUN-REPORT
           ELSE
              PERFORM KEY-DETAIL.

           IF WORK-OWNBR NOT = HOLD-LN-BR
              MOVE WORK-OWNBR TO HOLD-LN-BR BR-NO
              PERFORM GET-LN-BW-PATH.

           MOVE WORK-OWNBR  TO LN-OWNBR.
           MOVE WORK-ACCTNO TO LN-ACCTNO.
           PERFORM READ-LN1-FILE.
           IF IO-FG NOT = 0
              GO TO END-RUN-REPORT.

           GO TO KEY-FILE-LOOP.

       END-RUN-REPORT.
           EXIT.

      ******************************************************************
       SORT-TRP-FILE SECTION.
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
              GO TO SORT-TRP-EXIT.

           PERFORM NPR-TRAP-INTERRUPT.

           IF TP-DATE > RPT-END-DATE
              GO TO SORT-TRP-EXIT.

           IF TP-CLASS < BEG-CLASS OR TP-CLASS > END-CLASS
              GO TO SORT-TRP-NEXT.

           IF NOT (TP-ADDON-TYPE = "CL" OR "AH")
              IF NOT (TP-TRCD = "RC" OR "RA" OR "CC" OR "CA")
                 GO TO SORT-TRP-NEXT.

      *    MOVE TP-BRNO   TO LN-OWNBR.
      *    MOVE TP-ACCTNO TO LN-ACCTNO.
      *    PERFORM READ-LN1-FILE.
      *    IF IO-FG NOT = 0
      *       GO TO SORT-TRP-NEXT.

           IF LN-ORGST < BEG-ST OR LN-ORGST > END-ST
              GO TO SORT-TRP-NEXT.

      * THE FOLLOWING IF WAS MISSING, PUT IT BACK IN 8/9/01 BAH
      *  PPMORP AND O1MORP STILL HAD IT !!
      * IF ANOTHER BRANCH DOES A PAYOFF, AND THE SAME LOAN # EXIST
      * IN BOTH BRANCHES, (NOT REALLY THE SAME LOAN!), IT WILL PRINT FOR
      * BOTH BRANCHES, SHOULD ONLY PRINT FOR THE OWNING BRANCH !!

      * ONLY REPORT FOR THE OWNING BRANCH ("G")
      *    IF TP-CODE = "P"
              IF TP-OTHBR-FG = "C"
                 GO TO SORT-TRP-NEXT
      * THE "P" TRANSACTIONS ARE ONLY ADDONS AND REBATES/CANCELS
      * NEED TO SUBTRACT OUT THE INSURANCE TAX, BUT CHECK FOR
      * SPACES HERE SO WE CAN JUST SUBTRACT IT FURTHER DOWN
              ELSE
                 IF TP-INS-TAX-RB = 2020202.02
                    MOVE 0 TO TP-INS-TAX-RB.

           MOVE 0 TO WORK-INSCOMP.
           IF (TP-ADDON-TYPE = "CL") OR TP-TRCD = "RC"
                                     OR TP-TRCD = "CC"
              IF TP-INSOURS = "Y" 
                 MOVE LN-INSCOMP(1) TO WORK-INSCOMP
                 IF TP-INSCOMP NUMERIC AND TP-INSCOMP NOT = 0
                    MOVE TP-INSCOMP TO WORK-INSCOMP
                 END-IF
              END-IF
           ELSE
              IF (TP-ADDON-TYPE = "AH") OR TP-TRCD = "RA"
                                        OR TP-TRCD = "CA"
                 IF TP-INSOURS = "Y" 
                    MOVE LN-INSCOMP(2) TO WORK-INSCOMP
                    IF TP-INSCOMP NUMERIC AND TP-INSCOMP NOT = 0
                       MOVE TP-INSCOMP TO WORK-INSCOMP.

           IF WORK-INSCOMP = 0
              GO TO SORT-TRP-NEXT.

           IF WORK-INSCOMP < BEG-IC OR WORK-INSCOMP > END-IC
              GO TO SORT-TRP-NEXT.

           MOVE TP-DATE   TO WORK-DATE.
           MOVE TP-BRNO   TO WORK-OWNBR.
           MOVE TP-CLASS  TO WORK-CLASS.
           MOVE TP-ACCTNO TO WORK-ACCTNO.
           PERFORM READ-WK-FILE.
           IF IO-FG NOT = 0
              PERFORM CLEAR-WK-FILE.

           IF (LN-INSURED = "M" OR "S") AND LN-INSTYPES-DL
              MOVE 1 TO SUB1
           ELSE
           IF (LN-INSURED = "M" OR "S") AND LN-INSTYPES-LL
              MOVE 3 TO SUB1
           ELSE
           IF LN-INSURED = "J" AND LN-INSTYPES-DL
              MOVE 2 TO SUB1
           ELSE
           IF LN-INSURED = "J" AND LN-INSTYPES-LL
              MOVE 4 TO SUB1
           ELSE
      * NEEDED A DEFAULT FOR JUST A&H !!!!! 4/8/97 BAH
              MOVE 1 TO SUB1.

              IF TP-ADDON-TYPE = "CL" AND
                 TP-ADDON-TEXT = "ADD"
                 MOVE "A" TO WORK-ADDON-FG-CL
                 COMPUTE WORK-NEW-CL(SUB1) =
                              WORK-NEW-CL(SUB1) + TP-APCUR
                 COMPUTE WORK-COM-CL(SUB1) =
                              WORK-COM-CL(SUB1) + TP-INSCOMM
              ELSE
              IF TP-ADDON-TYPE = "AH" AND
                 TP-ADDON-TEXT = "ADD"
                 MOVE "A" TO WORK-ADDON-FG-AH
                 COMPUTE WORK-NEW-AH(SUB1) =
                              WORK-NEW-AH(SUB1) + TP-APCUR
                 COMPUTE WORK-COM-AH(SUB1) =
                              WORK-COM-AH(SUB1) + TP-INSCOMM
              ELSE
              IF TP-ADDON-TYPE = "CL" AND
                 TP-ADDON-TEXT = "ADJ"
                 COMPUTE WORK-ADJ-CL(SUB1) =
                              WORK-ADJ-CL(SUB1) + TP-APCUR
                 COMPUTE WORK-COM-CL(SUB1) =
                              WORK-COM-CL(SUB1) + TP-INSCOMM
              ELSE
              IF TP-ADDON-TYPE = "AH" AND
                 TP-ADDON-TEXT = "ADJ"
                 COMPUTE WORK-ADJ-AH(SUB1) =
                              WORK-ADJ-AH(SUB1) + TP-APCUR
                 COMPUTE WORK-COM-AH(SUB1) =
                              WORK-COM-AH(SUB1) + TP-INSCOMM
              ELSE
              IF TP-ADDON-TYPE = "CL" AND
                 TP-ADDON-TEXT = "CNL"
                 MOVE "C" TO WORK-CANCEL-FG-CL
                 COMPUTE WORK-CAN-CL(SUB1) =
                              WORK-CAN-CL(SUB1) + TP-APCUR
                 MOVE TP-PAYDATE TO WORK-CAN-CL-DATE
                 COMPUTE WORK-COM-CL(SUB1) =
                              WORK-COM-CL(SUB1) + TP-INSCOMM
              ELSE
              IF TP-ADDON-TYPE = "AH" AND
                 TP-ADDON-TEXT = "CNL"
                 MOVE "C" TO WORK-CANCEL-FG-AH
                 COMPUTE WORK-CAN-AH(SUB1) =
                              WORK-CAN-AH(SUB1) + TP-APCUR
                 MOVE TP-PAYDATE TO WORK-CAN-AH-DATE
                 COMPUTE WORK-COM-AH(SUB1) =
                              WORK-COM-AH(SUB1) + TP-INSCOMM
              ELSE
              IF TP-TRCD = "RC"
                 COMPUTE WORK-CAN-CL(SUB1) =
                              WORK-CAN-CL(SUB1) +
                                   ((TP-TRAMT - TP-INS-TAX-RB) * -1)
                 MOVE TP-PAYDATE TO WORK-CAN-CL-DATE
                 COMPUTE WORK-COM-CL(SUB1) =
                            WORK-COM-CL(SUB1) + (TP-INSCOMM * -1)
              ELSE
              IF TP-TRCD = "RA"
                 COMPUTE WORK-CAN-AH(SUB1) =
                              WORK-CAN-AH(SUB1) +
                                   ((TP-TRAMT - TP-INS-TAX-RB) * -1)
                 MOVE TP-PAYDATE TO WORK-CAN-AH-DATE
                 COMPUTE WORK-COM-AH(SUB1) =
                            WORK-COM-AH(SUB1) + (TP-INSCOMM * -1)
              ELSE
              IF TP-TRCD = "CC"
                 MOVE "C" TO WORK-CANCEL-FG-CL
                 COMPUTE WORK-CAN-CL(SUB1) =
                              WORK-CAN-CL(SUB1) +
                                   ((TP-TRAMT - TP-INS-TAX-RB) * -1)
                 MOVE TP-PAYDATE TO WORK-CAN-CL-DATE
                 COMPUTE WORK-COM-CL(SUB1) =
                            WORK-COM-CL(SUB1) + (TP-INSCOMM * -1)
              ELSE
              IF TP-TRCD = "CA"
                 MOVE "C" TO WORK-CANCEL-FG-AH
                 COMPUTE WORK-CAN-AH(SUB1) =
                           WORK-CAN-AH(SUB1) +
                                   ((TP-TRAMT - TP-INS-TAX-RB) * -1)
                 MOVE TP-PAYDATE TO WORK-CAN-AH-DATE
                 COMPUTE WORK-COM-AH(SUB1) =
                         WORK-COM-AH(SUB1) + (TP-INSCOMM * -1).

           IF IO-FG = 0
              PERFORM REWRITE-WK-FILE
           ELSE
              PERFORM WRITE-WK-FILE.

           GO TO SORT-TRP-NEXT.

       SORT-TRP-EXIT.
           PERFORM CLOSE-TRP1-FILE.

      ******************************************************************
       SORT-TRL-FILE SECTION.
      ******************************************************************

           PERFORM OPEN-TRL1-FILE.

           MOVE BR-NO               TO TL-BRNO
                                       QTRL1-WBEG-BRNO
                                       QTRL1-WEND-BRNO.
           MOVE BEGINNING-DATE      TO TL-DATE SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO QTRL1-WBEG-DATE.
           MOVE RPT-END-DATE        TO  SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO QTRL1-WEND-DATE.
           MOVE BEG-CLASS           TO TL-CLASS
                                       QTRL1-WBEG-CLASS.
           MOVE END-CLASS           TO QTRL1-WEND-CLASS.
           MOVE 0                   TO TL-ACCTNO TL-LAWCODE TL-LINE
                                       QTRL1-WBEG-ACCTNO
                                       QTRL1-WBEG-LAWCODE
                                       QTRL1-WBEG-LINE.
           MOVE ALL "9"             TO QTRL1-WEND-ACCTNO
                                       QTRL1-WEND-LAWCODE
                                       QTRL1-WEND-LINE.
           PERFORM START-TRL1-FILE.

       SORT-TRL-NEXT.
           PERFORM READ-TRL1-FILE-NEXT.
           IF (IO-FG NOT = 0) OR (TRL-PATH-OWNBR NOT = TL-BRNO)
              GO TO SORT-TRL-EXIT.

           PERFORM NPR-TRAP-INTERRUPT.

           IF TL-DATE > RPT-END-DATE
              GO TO SORT-TRL-EXIT.

           IF TL-CLASS < BEG-CLASS OR TL-CLASS > END-CLASS
              GO TO SORT-TRL-NEXT.


           IF TL-INSCOMP(1) = 0 AND TL-INSCOMP(2) = 0
              GO TO SORT-TRL-NEXT
           ELSE
           IF TL-INSCOMP(1) < BEG-IC OR TL-INSCOMP(1) > END-IC
              IF TL-INSCOMP(2) < BEG-IC OR TL-INSCOMP(2) > END-IC
                 GO TO SORT-TRL-NEXT.

           IF TL-INSOURS(1) NOT = "Y" AND TL-INSOURS(2) NOT = "Y"
              GO TO SORT-TRL-NEXT.

      *    MOVE TL-BRNO   TO LN-OWNBR.
      *    MOVE TL-ACCTNO TO LN-ACCTNO.
      *    PERFORM READ-LN1-FILE.
      *    IF IO-FG NOT = 0
      *       GO TO SORT-TRL-NEXT.

           IF LN-ORGST < BEG-ST OR LN-ORGST > END-ST
              GO TO SORT-TRL-NEXT.

           MOVE 0 TO WORK-INSCOMP.

      * MAJOR HEADACHES WHEN LIFE AND A&H ARE DIFFERENT COMPANIES, THEY
      * WERE SHARING 1 WORK RECORD WITH THE LIFE INS COMPANY...WRONG !!
              IF TL-INSCOMP(1) NOT = 0 AND (TL-INSOURS(1) = "Y")
                 MOVE TL-INSCOMP(1) TO WORK-INSCOMP
      * IF CREDIT LIFE COMPANY DOES NOT MATCH RANGE, THEN IT MUST BE A&H
      * CREATE WORK RECORD, AND GO TO NEXT
                 IF WORK-INSCOMP < BEG-IC OR WORK-INSCOMP > END-IC
                    MOVE TL-INSCOMP(2) TO WORK-INSCOMP
                    PERFORM CREATE-WORK-TRL-RECORD
                    GO TO SORT-TRL-NEXT
                 ELSE
      * CREDIT LIFE COMPANY FITS RANGE, SO CREATE WORK, THEN YOU HAVE TO
      * TEST A&H
                    PERFORM CREATE-WORK-TRL-RECORD
      * IF CREDIT LIFE COMPANY SAME AS A&H THEN YOU ARE DONE WITH WORK RECORD
                    IF TL-INSCOMP(1) = TL-INSCOMP(2)
                       GO TO SORT-TRL-NEXT
                    ELSE
      * IF COMPANIES ARE DIFFERENT, YOU MUST CREATE A&H
                       MOVE TL-INSCOMP(2) TO WORK-INSCOMP
                       IF WORK-INSCOMP < BEG-IC OR
                                            WORK-INSCOMP > END-IC
                          GO TO SORT-TRL-NEXT
                       ELSE
                          PERFORM CREATE-WORK-TRL-RECORD
                          GO TO SORT-TRL-NEXT
                       END-IF
                    END-IF
                 END-IF
      * IF NO CREDIT LIFE OR NOT OURS, TEST CREATING JUST A&H
              ELSE
                 IF TL-INSCOMP(2) NOT = 0 AND (TL-INSOURS(2) = "Y")
                    MOVE TL-INSCOMP(2) TO WORK-INSCOMP
                    PERFORM CREATE-WORK-TRL-RECORD
                    GO TO SORT-TRL-NEXT.

           GO TO SORT-TRL-NEXT.

       SORT-TRL-EXIT.
           PERFORM CLOSE-TRL1-FILE.

      ******************************************************************
       SORT-TRS-FILE SECTION.
      ******************************************************************

           PERFORM OPEN-TRS1-FILE.

           MOVE BR-NO               TO TS-BRNO
                                       QTRS1-WBEG-BRNO
                                       QTRS1-WEND-BRNO.
           MOVE BEGINNING-DATE      TO TS-DATE SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO QTRS1-WBEG-DATE.
           MOVE RPT-END-DATE        TO  SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO QTRS1-WEND-DATE.
           MOVE BEG-CLASS           TO TS-CLASS
                                       QTRS1-WBEG-CLASS.
           MOVE END-CLASS           TO QTRS1-WEND-CLASS.
           MOVE 0                   TO TS-ACCTNO TS-LAWCODE TS-LINE
                                       QTRS1-WBEG-ACCTNO
                                       QTRS1-WBEG-LAWCODE
                                       QTRS1-WBEG-LINE.
           MOVE ALL "9"             TO QTRS1-WEND-ACCTNO
                                       QTRS1-WEND-LAWCODE
                                       QTRS1-WEND-LINE.
           PERFORM START-TRS1-FILE.

       SORT-TRS-NEXT.
           PERFORM READ-TRS1-FILE-NEXT.
           IF (IO-FG NOT = 0) OR (TRS-PATH-OWNBR NOT = TS-BRNO)
              GO TO SORT-TRS-EXIT.

           PERFORM NPR-TRAP-INTERRUPT.

           IF TS-CLASS < BEG-CLASS OR TS-CLASS > END-CLASS
              GO TO SORT-TRS-NEXT.

           IF TS-INSCOMP(1) = 0 AND TS-INSCOMP(2) = 0
              GO TO SORT-TRS-NEXT
           ELSE
           IF TS-INSCOMP(1) < BEG-IC OR TS-INSCOMP(1) > END-IC
              IF TS-INSCOMP(2) < BEG-IC OR TS-INSCOMP(2) > END-IC
                 GO TO SORT-TRS-NEXT.

      * ALREADY SKIPPED INVALID COMPANIES OR NO INSURANCE
           IF TS-INSOURS(1) NOT = "Y" AND TS-INSOURS(2) NOT = "Y"
              GO TO SORT-TRS-NEXT.

      *    MOVE TS-BRNO   TO LN-OWNBR.
      *    MOVE TS-ACCTNO TO LN-ACCTNO.
      *    PERFORM READ-LN1-FILE.
      *    IF IO-FG NOT = 0
      *       GO TO SORT-TRS-NEXT.

           IF LN-ORGST < BEG-ST OR LN-ORGST > END-ST
              GO TO SORT-TRS-NEXT.

           MOVE 0 TO WORK-INSCOMP.

      * MAJOR HEADACHES WHEN LIFE AND A&H ARE DIFFERENT COMPANIES, THEY
      * WERE SHARING 1 WORK RECORD WITH THE LIFE INS COMPANY...WRONG !!
              IF TS-INSCOMP(1) NOT = 0 AND (TS-INSOURS(1) = "Y")
                 MOVE TS-INSCOMP(1) TO WORK-INSCOMP
      * IF CREDIT LIFE COMPANY DOES NOT MATCH RANGE, THEN IT MUST BE A&H
      * CREATE WORK RECORD, AND GO TO NEXT
                 IF WORK-INSCOMP < BEG-IC OR WORK-INSCOMP > END-IC
                    MOVE TS-INSCOMP(2) TO WORK-INSCOMP
                    PERFORM CREATE-WORK-TRS-RECORD
                    GO TO SORT-TRS-NEXT
                 ELSE
      * CREDIT LIFE COMPANY FITS RANGE, SO CREATE WORK, THEN YOU HAVE TO
      * TEST A&H
                    PERFORM CREATE-WORK-TRS-RECORD
      * IF CREDIT LIFE COMPANY SAME AS A&H THEN YOU ARE DONE WITH WORK RECORD
                    IF TS-INSCOMP(1) = TS-INSCOMP(2)
                       GO TO SORT-TRS-NEXT
                    ELSE
      * IF COMPANIES ARE DIFFERENT, YOU MUST CREATE A&H
                       MOVE TS-INSCOMP(2) TO WORK-INSCOMP
                       IF WORK-INSCOMP < BEG-IC OR
                                            WORK-INSCOMP > END-IC
                          GO TO SORT-TRS-NEXT
                       ELSE
                          PERFORM CREATE-WORK-TRS-RECORD
                          GO TO SORT-TRS-NEXT
                       END-IF
                    END-IF
                 END-IF
      * IF NO CREDIT LIFE OR NOT OURS, TEST CREATING JUST A&H
              ELSE
                 IF TS-INSCOMP(2) NOT = 0 AND (TS-INSOURS(2) = "Y")
                    MOVE TS-INSCOMP(2) TO WORK-INSCOMP
                    PERFORM CREATE-WORK-TRS-RECORD
                    GO TO SORT-TRS-NEXT.

           GO TO SORT-TRS-NEXT.

       SORT-TRS-EXIT.
           PERFORM CLOSE-TRS1-FILE.

      ***********************************************
       CREATE-WORK-TRL-RECORD SECTION.
      ***********************************************

           IF WORK-INSCOMP = 0
              GO TO CREATE-WORK-TRL-RECORD-EXIT.

           MOVE TL-DATE   TO WORK-DATE.
           MOVE TL-BRNO   TO WORK-OWNBR.
           MOVE TL-CLASS  TO WORK-CLASS.
           MOVE TL-ACCTNO TO WORK-ACCTNO.
           PERFORM READ-WK-FILE.
           IF IO-FG NOT = 0
              PERFORM CLEAR-WK-FILE.

           IF (LN-INSURED = "M" OR "S") AND LN-INSTYPES-DL
              MOVE 1 TO SUB1
           ELSE
           IF (LN-INSURED = "M" OR "S") AND LN-INSTYPES-LL
              MOVE 3 TO SUB1
           ELSE
           IF LN-INSURED = "J" AND LN-INSTYPES-DL
              MOVE 2 TO SUB1
           ELSE
           IF LN-INSURED = "J" AND LN-INSTYPES-LL
              MOVE 4 TO SUB1
           ELSE
      * NEEDED A DEFAULT FOR JUST A&H !!!!! 4/8/97 BAH
              MOVE 1 TO SUB1.

           IF TL-INSOURS(1) = "Y" AND TL-INSCOMP(1) = WORK-INSCOMP
              COMPUTE WORK-NEW-CL(SUB1) = WORK-NEW-CL(SUB1)
                                        + TL-INSPREM(1)
              COMPUTE WORK-COM-CL(SUB1) = WORK-COM-CL(SUB1)
                                        + TL-INSCOMM(1)
           END-IF
           IF TL-INSOURS(2) = "Y" AND TL-INSCOMP(2) = WORK-INSCOMP
              COMPUTE WORK-NEW-AH(SUB1) = WORK-NEW-AH(SUB1)
                                        + TL-INSPREM(2)
              COMPUTE WORK-COM-AH(SUB1) = WORK-COM-AH(SUB1)
                                        + TL-INSCOMM(2).

           IF IO-FG = 0
              PERFORM REWRITE-WK-FILE
           ELSE
              PERFORM WRITE-WK-FILE.

       CREATE-WORK-TRL-RECORD-EXIT.
           EXIT.

      *****************************************************
       CREATE-WORK-TRS-RECORD SECTION.
      *****************************************************

           IF WORK-INSCOMP = 0
              GO TO CREATE-WORK-TRS-RECORD-EXIT.

           MOVE TS-DATE   TO WORK-DATE.
           MOVE TS-BRNO   TO WORK-OWNBR.
           MOVE TS-CLASS  TO WORK-CLASS.
           MOVE TS-ACCTNO TO WORK-ACCTNO.
           PERFORM READ-WK-FILE.
           IF IO-FG NOT = 0
              PERFORM CLEAR-WK-FILE.

           IF (LN-INSURED = "M" OR "S") AND LN-INSTYPES-DL
              MOVE 1 TO SUB1
           ELSE
           IF (LN-INSURED = "M" OR "S") AND LN-INSTYPES-LL
              MOVE 3 TO SUB1
           ELSE
           IF LN-INSURED = "J" AND LN-INSTYPES-DL
              MOVE 2 TO SUB1
           ELSE
           IF LN-INSURED = "J" AND LN-INSTYPES-LL
              MOVE 4 TO SUB1
           ELSE
      * NEEDED A DEFAULT FOR JUST A&H !!!!! 4/8/97 BAH
              MOVE 1 TO SUB1.

              IF TS-INSOURS(1) = "Y" AND TS-INSCOMP(1) = WORK-INSCOMP
                 COMPUTE WORK-NEW-CL(SUB1) = WORK-NEW-CL(SUB1)
                                           + TS-INSPREM(1)
                 COMPUTE WORK-COM-CL(SUB1) = WORK-COM-CL(SUB1)
                                           + TS-INSCOMM(1)
              END-IF
              IF TS-INSOURS(2) = "Y" AND TS-INSCOMP(2) = WORK-INSCOMP
                 COMPUTE WORK-NEW-AH(SUB1) = WORK-NEW-AH(SUB1)
                                           + TS-INSPREM(2)
                 COMPUTE WORK-COM-AH(SUB1) = WORK-COM-AH(SUB1)
                                           + TS-INSCOMM(2).

           IF IO-FG = 0
              PERFORM REWRITE-WK-FILE
           ELSE
              PERFORM WRITE-WK-FILE.

       CREATE-WORK-TRS-RECORD-EXIT.
           EXIT.

      ******************************************************************
       KEY-DETAIL SECTION.
      ******************************************************************

           IF SUMMARY-ONLY = "Y" OR "I"
              GO TO END-KEY-DETAIL.

      * IF ACCTNO IS THE SAME, DONT REPRINT LOAN INFO....
      * BUT YOU HAVE TO TEST IF COMPANY IS THE SAME, BECAUSE THEY NEED
      * TO BE TOTALED SEPARATELY.....
      * YOU ALSO NEED TO TEST THAT THE BRANCH IS THE SAME, REGENCY HAD
      * THE LAST ACCT IN 413 THE SAME AS THE FIRST ACCT IN 414, AND
      * IT COMBINED THEM BOTH INTO BRANCH 414 !!!!
           IF LN-ACCTNO = WORK-ACCTNO AND (LN-OWNBR = WORK-OWNBR)
              IF WORK-INSCOMP = HOLD-IC
                 GO TO EXIT-KEY-DETAIL.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      * SETUP PRIMARY INSURED INFO:
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF LN-INSURED = "S"
              MOVE LN-SSNO(2) TO BW-SSNO
           ELSE
              MOVE LN-SSNO(1) TO BW-SSNO.

           PERFORM SETUP-INSURED.

           IF LN-INSURED = "S"
              MOVE LN-SSNO(1) TO BW-SSNO
              PERFORM READ-BW1-FILE.

           PERFORM LPSTAT-DISPLAY.
           MOVE LPS-DISPLAY-LNNO TO D-LOAN-NUM.

           MOVE BAGE-AGE TO D-AGE.

           IF LIFE-PREM = 0 AND LIFE-ADJ = 0 AND LIFE-CAN = 0 AND
               LIFE-COMM = 0 GO TO SKIP-LIFE.

           MOVE 1 TO ITRM-SUB.
           PERFORM INS-TERM-CALCULATION.
           MOVE ITRM-INSTERM TO D-TERM.

           MOVE LN-INSEFF-DATE(1) TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO D-EFFDATE.
           MOVE LN-INSEXP-DATE(1) TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO D-EXPDATE.
           MOVE LN-INSCOVR(1) TO D-COVRG D-REGPYAMT.

           MOVE LIFE-PREM TO D-INSPREM.
           MOVE LIFE-ADDON-FG TO D-ADDTEXT.
           MOVE LIFE-ADJ TO D-INSADJ.
           MOVE LIFE-CAN TO D-INSREFUND.
           MOVE LIFE-CANCEL-FG TO D-CNLTEXT.

           IF LIFE-CAN NOT = 0
              MOVE LIFE-CAN-DATE TO DATE-YYYYMMDD
              PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
              MOVE DATE-MMDDYY TO D-REFUND-DATE.

           MOVE LIFE-COMM TO D-INSCOMM.

      *-----------------------------------------------------------------
       SKIP-LIFE.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      * SETUP SECONDARY INSURED IF JOINT COVERAGE:
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

           IF LN-INSURED = "J"
              IF LN-SSNO(2) NOT = 0
                 MOVE LN-SSNO(2) TO BW-SSNO
                 PERFORM SETUP-INSURED.

           IF LN-INSTYPES-LL
              MOVE "L" TO D-LEVEL.

           MOVE LN-INSURED TO D-WHO.

           IF AH-PREM = 0 AND AH-ADJ = 0 AND AH-CAN = 0 AND
               AH-COMM = 0 GO TO SKIP-AH.

           IF LN-INSTYPES-LL
              IF LN-INSURED = "J"
                  MOVE 4 TO SUB
               ELSE
                  MOVE 3 TO SUB
           ELSE
               IF LN-INSURED = "J"
                  MOVE 2 TO SUB
               ELSE
                  MOVE 1 TO SUB.

           IF (LN-REGPYAMT - LN-MAINTFEE) > SP-INSMAXBENIF(5)
              MOVE SP-INSMAXBENIF(5) TO D-REGPYAMT
           ELSE
              COMPUTE D-REGPYAMT = LN-REGPYAMT - LN-MAINTFEE.

           MOVE 2 TO ITRM-SUB.
           PERFORM INS-TERM-CALCULATION.
           MOVE ITRM-INSTERM TO D-TERM.

           MOVE LN-INSEFF-DATE(2) TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO D-EFFDATE.
           MOVE LN-INSEXP-DATE(2) TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO D-EXPDATE.
           MOVE LN-INSCOVR(2) TO D-COVRG.

           MOVE AH-PREM TO D-INSPREM.
           MOVE AH-ADDON-FG TO D-ADDTEXT.
           MOVE AH-ADJ TO D-INSADJ.
           MOVE AH-CAN TO D-INSREFUND.
           MOVE AH-CANCEL-FG TO D-CNLTEXT.

           IF AH-CAN NOT = 0
              MOVE AH-CAN-DATE TO DATE-YYYYMMDD
              PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
              MOVE DATE-MMDDYY TO D-REFUND-DATE.

           MOVE AH-COMM TO D-INSCOMM.

      *-----------------------------------------------------------------
       SKIP-AH.
           PERFORM WRITE-DETAIL-LINE.

      *-----------------------------------------------------------------
       END-KEY-DETAIL.
           PERFORM CREATE-TOTAL-RECS.

           MOVE 0 TO LIFE-PREM LIFE-ADJ LIFE-CAN
                     LIFE-COMM LIFE-CAN-DATE.

           MOVE 0 TO AH-PREM AH-ADJ AH-CAN AH-COMM AH-CAN-DATE.
           MOVE SPACES TO LIFE-ADDON-FG LIFE-CANCEL-FG.
           MOVE SPACES TO AH-ADDON-FG AH-CANCEL-FG.

       EXIT-KEY-DETAIL.
           EXIT.

      ******************************************************************
       CREATE-TOTAL-RECS SECTION.
      ******************************************************************
           MOVE 0 TO ONE-CL ONE-AH.
           MOVE 0 TO REF-CL REF-AH.

           IF LN-INSCOMP(1) NOT = 0
              MOVE LN-INSCOVR(1) TO NEW-COVERAGE
              IF LIFE-PREM = 0
                 MOVE 0 TO NEW-COVERAGE
              ELSE
                 IF LIFE-PREM < 0
                    MULTIPLY -1 BY NEW-COVERAGE
                 ELSE
                    NEXT SENTENCE
           ELSE
              IF LN-INSCOMP(2) NOT = 0
                 MOVE LN-INSCOVR(2) TO NEW-COVERAGE
                 IF AH-PREM = 0
                    MOVE 0 TO NEW-COVERAGE
                 ELSE
                    IF AH-PREM < 0
                       MULTIPLY -1 BY NEW-COVERAGE.

      *--->IF LIFE-PREM NOT = 0 OR LIFE-CAN NOT = 0 OR LIFE-ADJ NOT = 0
      *       MOVE 1 TO ONE-CL.
      *    IF AH-PREM NOT = 0 OR AH-CAN NOT = 0 OR AH-ADJ NOT = 0
      *------>MOVE 1 TO ONE-AH.

           IF LIFE-PREM > 0
              MOVE 1 TO ONE-CL
           ELSE
              IF LIFE-PREM < 0
                 MOVE -1 TO ONE-CL.

      * REFUNDS ARE NEGATIVE
           IF LIFE-CAN > 0
              MOVE -1 TO REF-CL
           ELSE
              IF LIFE-CAN < 0
                 MOVE 1 TO REF-CL.

           IF AH-PREM > 0
              MOVE 1 TO ONE-AH
           ELSE
              IF AH-PREM < 0
                 MOVE -1 TO ONE-AH.

      * REFUNDS ARE NEGATIVE
           IF AH-CAN > 0
              MOVE -1 TO REF-AH
           ELSE
              IF AH-CAN < 0
                 MOVE 1 TO REF-AH.

      *-----------------------------------------------------------------
      *    WK-CODE: 1-BR, 2-ST, 3-IC, 4-GB
      *-----------------------------------------------------------------
           PERFORM INIT-WK1-KEY.
           MOVE 1 TO WK-CODE.
           MOVE LN-OWNBR TO WK-BRNO.
           PERFORM CREATE-WK.

           PERFORM INIT-WK1-KEY.
           MOVE 2 TO WK-CODE.
           MOVE LN-ORGST TO WK-ST.
           PERFORM CREATE-WK.

           PERFORM INIT-WK1-KEY.
           MOVE 3 TO WK-CODE.
           PERFORM CREATE-WK.

           PERFORM INIT-WK1-KEY.
           MOVE 4 TO WK-CODE.
           MOVE 0 TO WK-IC.
           PERFORM CREATE-WK.

       EXIT-CREATE-TOTAL-RECS.
           EXIT.

      ******************************************************************
       CREATE-WK SECTION.
      ******************************************************************
           MOVE WK1-KEY TO SAVE-WK1-KEY.
           MOVE "N" TO REC-EXISTS.

      *-----------------------------------------------------------------
       CHECK-REC-EXISTS.
           PERFORM READ-WK1-FILE.
           IF IO-FG = 0
              GO TO UPDATE-WK1-REC.

           IF REC-EXISTS = "Y"
              GO TO IO-ERROR.

           MOVE " " TO WK1-REC.
           MOVE SAVE-WK1-KEY TO WK1-KEY.

           PERFORM VARYING SUB2 FROM 1 BY 1 UNTIL SUB2 > 4
               MOVE 0 TO WK-AMT(SUB2 1) WK-AMT(SUB2 2) WK-AMT(SUB2 3)
                         WK-AMT(SUB2 4) WK-AMT(SUB2 5) WK-AMT(SUB2 6)
                         WK-AMT(SUB2 7) WK-AMT(SUB2 8) WK-AMT(SUB2 9)
                         WK-AMT(SUB2 10) WK-AMT(SUB2 11) WK-AMT(SUB2 12)
                         WK-AMT(SUB2 13) WK-QTY(SUB2 1) WK-QTY(SUB2 2)
                                         WK-QTY(SUB2 3) WK-QTY(SUB2 4)
           END-PERFORM.

           PERFORM WRITE-WK1-FILE.
           IF IO-FG = 9
              GO TO IO-ERROR.

           MOVE "Y" TO REC-EXISTS.
           GO TO CHECK-REC-EXISTS.

      *-----------------------------------------------------------------
       UPDATE-WK1-REC.
           ADD ONE-CL TO WK-QTY(SUB1 1).
           ADD ONE-AH TO WK-QTY(SUB1 2).
           ADD REF-CL TO WK-QTY(SUB1 3).
           ADD REF-AH TO WK-QTY(SUB1 4).

           ADD NEW-COVERAGE TO WK-AMT(SUB1 1).

           IF LIFE-ADDON-FG = "A"
              ADD LIFE-PREM TO WK-AMT(SUB1 10)
           ELSE
              ADD LIFE-PREM TO WK-AMT(SUB1 2).

           ADD LIFE-ADJ TO WK-AMT(SUB1 3).

           IF LIFE-CANCEL-FG = "C"
              ADD LIFE-CAN TO WK-AMT(SUB1 11)
           ELSE
              ADD LIFE-CAN TO WK-AMT(SUB1 4).

           ADD LIFE-COMM TO WK-AMT(SUB1 5).

           IF AH-ADDON-FG = "A"
              ADD AH-PREM TO WK-AMT(SUB1 12)
           ELSE
              ADD AH-PREM TO WK-AMT(SUB1 6).

           ADD AH-ADJ TO WK-AMT(SUB1 7).

           IF AH-CANCEL-FG = "C"
              ADD AH-CAN TO WK-AMT(SUB1 13)
           ELSE
              ADD AH-CAN TO WK-AMT(SUB1 8).

           ADD AH-COMM TO WK-AMT(SUB1 9).
           PERFORM REWRITE-WK1-FILE.

      ******************************************************************
       BR-TOTALS SECTION.
      ******************************************************************
           IF SUMMARY-ONLY = "I"
              GO TO EXIT-BR-TOTALS.

           MOVE "B R A N C H   T O T A L S:" TO H-PAGE-DESC.
           MOVE 1 TO SAVE-CODE.
           MOVE HOLD-BR TO SAVE-BR.
           PERFORM PRINT-TOTALS.

           MOVE LN-OWNBR TO HOLD-BR.
           MOVE " " TO H-PAGE-DESC.
           PERFORM BR-HEADER.
           MOVE 0 TO BR-PAGE-NUMBER.
       EXIT-BR-TOTALS.
           EXIT.

      ******************************************************************
       IC-TOTALS SECTION.
      ******************************************************************
           PERFORM BR-TOTALS.

           IF SUMMARY-ONLY NOT = "I"
              MOVE "S T A T E   T O T A L S:" TO H-PAGE-DESC
              MOVE 2 TO SAVE-CODE
              MOVE " " TO SAVE-ST
              PERFORM PRINT-TOTALS.

           MOVE "I N S U R E R   T O T A L S:" TO H-PAGE-DESC.
           MOVE 0 TO H-BRNO.
           MOVE " " TO H-BRNAME.
           MOVE 3 TO SAVE-CODE.
           MOVE " " TO SAVE-ST.
           PERFORM PRINT-TOTALS.

           MOVE WORK-INSCOMP TO HOLD-IC.
           MOVE " " TO H-PAGE-DESC.
           PERFORM BR-HEADER.
           PERFORM IC-HEADER.

      ******************************************************************
       GB-TOTALS SECTION.
      ******************************************************************
           PERFORM IC-TOTALS.
           MOVE "C O M P A N Y   T O T A L S:" TO H-PAGE-DESC.
           MOVE 0 TO H-BRNO.
           MOVE " " TO H-BRNAME.
           MOVE 4 TO SAVE-CODE.
           MOVE 0 TO HOLD-IC.
           MOVE " " TO SAVE-ST.
           PERFORM PRINT-TOTALS.

      ******************************************************************
       PRINT-TOTALS SECTION.
      ******************************************************************
           MOVE 99 TO LN-COUNT.
           MOVE "Y" TO TOTAL-FG.
           MOVE "Y" TO FIRST-TOT.

           PERFORM CLEAR-T-TOTALS.

           MOVE " " TO WK1-KEY.
           MOVE SAVE-CODE TO WK-CODE.
           MOVE HOLD-IC TO WK-IC.
           MOVE SAVE-ST TO WK-ST.
           MOVE 0 TO WK-CLASS.
           PERFORM START-WK1-FILE.

      *-----------------------------------------------------------------
       NEXT-TOTAL-LINE.
           PERFORM READ-WK1-FILE-NEXT.
           IF IO-FG = 9
              GO TO EXIT-PRINT-TOTALS.

           IF WK-CODE NOT = SAVE-CODE
              GO TO EXIT-PRINT-TOTALS.

           IF WK-IC NOT = HOLD-IC
              GO TO EXIT-PRINT-TOTALS.

           IF SAVE-CODE NOT = 2
              GO TO SKIP-FIELD-2.

      *-----------------------------------------------------------------
       LOOP-FIELD-2.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    ST LOOP
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

           IF FIRST-TOT = "Y"
              MOVE "N" TO FIRST-TOT
              MOVE WK-ST TO LAST-ST H-STNO
              MOVE "- STATE SUMMARY -" TO H-BRNAME.

           IF WK-ST NOT = LAST-ST
              PERFORM TOTAL-LINE
              MOVE "Y" TO FIRST-TOT
              GO TO LOOP-FIELD-2.

           PERFORM MOVE-ACCUM-TOTALS.
           GO TO NEXT-TOTAL-LINE.

      *-----------------------------------------------------------------
       SKIP-FIELD-2.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    BR/IC LOOP
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

           IF WK-ST NOT = SAVE-ST
              GO TO EXIT-PRINT-TOTALS.

           PERFORM MOVE-ACCUM-TOTALS.
           GO TO NEXT-TOTAL-LINE.

       EXIT-PRINT-TOTALS.
           PERFORM TOTAL-LINE.
           MOVE "N" TO TOTAL-FG.

      ******************************************************************
       SETUP-INSURED SECTION.
      ******************************************************************
           PERFORM READ-BW1-FILE.
           IF IO-FG = 9
              MOVE "*** NOT FOUND ***" TO BW-FNAME
              MOVE SPACES TO BW-LNAME
              MOVE 0      TO BW-BDATE BW-REFDATE.

      * SET NAME OF PRIMARY INSURED:
           MOVE SPACES TO D-NAME.
           STRING BW-FNAME," ",BW-LNAME DELIMITED BY "  "
                 INTO D-NAME.

      * SET AGE OF PRIMARY INSURED:
           MOVE BW-BDATE     TO BAGE-DATE.
           MOVE BW-REFDATE   TO BAGE-REFDATE.
           MOVE RPT-END-DATE TO BAGE-TODAY.
           PERFORM BAGE-CALCULATION.

       SETUP-INSURED-EXIT.
           EXIT.

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
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE HEAD2 TO PR-REC.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-PR-REC.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE HEAD3 TO PR-REC.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-PR-REC.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF H-PAGE-DESC = " "
              ADD 1 TO BR-PAGE-NUMBER
              MOVE BR-PAGE-NUMBER TO H-BR-PAGE
              MOVE "BRANCH PAGE:" TO H-PAGE-PROMPT
           ELSE
              MOVE 0 TO H-BR-PAGE
              MOVE " " TO H-PAGE-PROMPT.

           MOVE HEAD4 TO PR-REC.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-PR-REC.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF TOTAL-FG = "N"
              MOVE HEAD5 TO PR-REC
              MOVE 2 TO LN-FEED
              PERFORM WRITE-PR-REC
           ELSE
              MOVE HEAD7 TO PR-REC
              MOVE 2 TO LN-FEED
              PERFORM WRITE-PR-REC.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF TOTAL-FG = "N"
              MOVE HEAD6 TO PR-REC
           ELSE
              MOVE HEAD8 TO PR-REC.

           MOVE 1 TO LN-FEED.
           PERFORM WRITE-PR-REC.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF TOTAL-FG NOT = "N"
              MOVE HEAD9 TO PR-REC
              MOVE 1 TO LN-FEED
              PERFORM WRITE-PR-REC.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE DASH-LINE TO PR-REC.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-PR-REC.

       END-HEAD-PAGE.
           MOVE 8 TO LN-COUNT.
           MOVE 2 TO LN-FEED.

      ******************************************************************
       RANGE-LINES SECTION.
      ******************************************************************

           MOVE 3 TO LN-FEED.
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
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "BEG INS CO" TO RANGE-LINE.
           MOVE BEG-IC TO RANGE-SEL.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "END INS CO" TO RANGE-LINE.
           MOVE END-IC TO RANGE-SEL.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "BEG ORIG STATE" TO RANGE-LINE.
           MOVE BEG-ST TO RANGE-SEL.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "END ORIG STATE" TO RANGE-LINE.
           MOVE END-ST TO RANGE-SEL.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "BEG LOAN CLASS" TO RANGE-LINE.
           MOVE BEG-CLASS TO RANGE-SEL.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "END LOAN CLASS" TO RANGE-LINE.
           MOVE END-CLASS TO RANGE-SEL.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "MONTH AND YEAR" TO RANGE-LINE.
           MOVE MMYY TO RANGE-SEL.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "SUMMARY ONLY ?" TO RANGE-LINE.
           MOVE SUMMARY-ONLY TO RANGE-SEL.
           PERFORM WRITE-DETAIL-LINE.

      *-----------------------------------------------------------------
       SKIP-RANGE-PRINT.
           EXIT.
      ******************************************************************
      *
      *     C O P Y    L I B R A R Y     S E C T I O N
      *
      ******************************************************************
       COPY "LIBGB/DATER.CPY".
       COPY "LIBWI/PFSWINDOWS.CPY".
       COPY "LIBLP/LPSTAT.CPY".
       COPY "LIBLP/LPMDTE.CPY".
       COPY "LIBLP/LPITRM.CPY".
       COPY "LIBLP/ARRAYSP.CPY".
       COPY "LIBGB/TIMEIO.CPY".
       COPY "LIBLP/LPBAGE.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/GRWORK.CPY".
       COPY "LIBGB/GRSERCH.CPY".
       COPY "LIBGB/BRSECURE.CPY".

      ******************************************************************
       MOVE-ACCUM-TOTALS SECTION.
      ******************************************************************
           PERFORM CLEAR-TOT-WK.
           PERFORM ACCUM-TOT-WK.
           MOVE 13 TO SUB.
       NEXT-AMT-ACCUM.
           ADD TOT-WK-AMT(SUB) TO TOT-AMT(SUB).
           IF SUB > 1
              SUBTRACT 1 FROM SUB
              GO TO NEXT-AMT-ACCUM.

       NEXT-QTY-ACCUM.
           ADD TOT-WK-QTY(SUB) TO TOT-QTY(SUB).
           IF SUB < 4
              ADD 1 TO SUB
              GO TO NEXT-QTY-ACCUM.

           MOVE SPACES TO DETAIL-LINE.
           MOVE WK-CLASS TO D-CLASS.
           MOVE TOT-WK-QTY(1) TO D-QTY(1).
           MOVE TOT-WK-QTY(2) TO D-QTY(2).
           COMPUTE D-QTY(3) = TOT-WK-QTY(1) + TOT-WK-QTY(2).
           MOVE "NEW" TO D-TAG.
           MOVE TOT-WK-AMT(1) TO D-AMT(1).
           MOVE TOT-WK-AMT(2) TO D-AMT(2).
           MOVE TOT-WK-AMT(10) TO D-AMT(3).
           MOVE TOT-WK-AMT(3) TO D-AMT(4).
           MOVE TOT-WK-AMT(11) TO D-AMT(5).
           MOVE TOT-WK-AMT(4) TO D-AMT(6).
           MOVE TOT-WK-AMT(5) TO D-AMT(7).
           MOVE 2 TO LN-FEED.

           IF BEG-BRANCH NOT = END-BRANCH
              PERFORM WRITE-DETAIL-LINE
           ELSE
      * ALWAYS PRINT BRANCH TOTALS AND COMPANY (FOR MULTIPLE INS. COS)
           IF SAVE-CODE = 1 OR 4
              PERFORM WRITE-DETAIL-LINE
           ELSE
              MOVE SPACES TO DETAIL-LINE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE SPACES TO DETAIL-LINE.

      * TOTAL # REFUNDS PR# 2642
           MOVE TOT-WK-QTY(3) TO D-QTY(1).
           MOVE TOT-WK-QTY(4) TO D-QTY(2).
           COMPUTE D-QTY(3) = TOT-WK-QTY(3) + TOT-WK-QTY(4).
           MOVE "REF" TO D-TAG.

           MOVE TOT-WK-AMT(6) TO D-AMT(2).
           MOVE TOT-WK-AMT(12) TO D-AMT(3).
           MOVE TOT-WK-AMT(7) TO D-AMT(4).
           MOVE TOT-WK-AMT(13) TO D-AMT(5).
           MOVE TOT-WK-AMT(8) TO D-AMT(6).
           MOVE TOT-WK-AMT(9) TO D-AMT(7).

           IF BEG-BRANCH NOT = END-BRANCH
              PERFORM WRITE-DETAIL-LINE
           ELSE
      * ALWAYS PRINT BRANCH TOTALS AND COMPANY (FOR MULTIPLE INS. COS)
           IF SAVE-CODE = 1 OR 4
              PERFORM WRITE-DETAIL-LINE
           ELSE
              MOVE SPACES TO DETAIL-LINE.


      *NET C/L PREMIUMS
           COMPUTE D-AMT(6) =
              (TOT-WK-AMT(2) + TOT-WK-AMT(10)) +
               (TOT-WK-AMT(11) + TOT-WK-AMT(4)) + TOT-WK-AMT(3).

      *NET AH PREMIUMS
           COMPUTE D-AMT(7) =
              (TOT-WK-AMT(6) + TOT-WK-AMT(12)) +
               (TOT-WK-AMT(13) + TOT-WK-AMT(8)) + TOT-WK-AMT(7).

           IF BEG-BRANCH NOT = END-BRANCH
              PERFORM WRITE-DETAIL-LINE
           ELSE
      * ALWAYS PRINT BRANCH TOTALS AND COMPANY (FOR MULTIPLE INS. COS)
           IF SAVE-CODE = 1 OR 4
              PERFORM WRITE-DETAIL-LINE
           ELSE
              MOVE SPACES TO DETAIL-LINE.

      *  ACCUM TOTALS FOR SDL, JDL, SLL, JLL BREAKDOWN.
           PERFORM ACCUM-T-TOTALS.

      ****************************************
       T-TOTAL-LINE SECTION.
      ****************************************

      * PRINTS BREAKDOWN OF SDL, JDL, SLL, AND JLL.
      *    IF TOTALS ARE ZERO THAN DO NOT PRINT.
      *    T-QTY ARE NEW LOANS, CL AND AH
      *    T-AMT ARE CL ADJ, CANCELS, REFUNDS
      *     AND      AH ADJ, CANCELS, REFUNDS

           IF T-QTY(T-LEV 1) = 0 AND T-QTY(T-LEV 2) = 0 AND
               T-AMT(T-LEV 3) = 0 AND T-AMT(T-LEV 4) = 0 AND
                T-AMT(T-LEV 11) = 0 AND T-AMT(T-LEV 7) = 0 AND
                 T-AMT(T-LEV 8) = 0 AND T-AMT(T-LEV 13) = 0
              GO TO END-T-TOTAL-LINE.

      *    PRINT DASH LINE TO SEPARATE FROM "ALL" TOTALS.

           IF T-LEV = 1
              MOVE ALL "-" TO DETAIL-LINE.

           IF BEG-BRANCH NOT = END-BRANCH
              PERFORM WRITE-DETAIL-LINE
           ELSE
      * ALWAYS PRINT BRANCH TOTALS AND COMPANY (FOR MULTIPLE INS. COS)
           IF SAVE-CODE = 1 OR 4
              PERFORM WRITE-DETAIL-LINE
           ELSE
              MOVE SPACES TO DETAIL-LINE.
      * SETUP ROW HEADERS.
           IF T-LEV = 1
              MOVE "SDL" TO DETAIL-LINE
           ELSE
           IF T-LEV = 2
              MOVE "JDL" TO DETAIL-LINE
           ELSE
           IF T-LEV = 3
              MOVE "SLL" TO DETAIL-LINE
           ELSE
              MOVE "JLL" TO DETAIL-LINE.

      * PRINT CREDIT LIFE TOTALS.
           MOVE T-QTY(T-LEV 1) TO D-QTY(1).
           MOVE T-QTY(T-LEV 2) TO D-QTY(2).
           ADD T-QTY(T-LEV 1) T-QTY(T-LEV 2) GIVING D-QTY(3).
           MOVE "NEW" TO D-TAG.
           MOVE T-AMT(T-LEV 1) TO D-AMT(1).
           MOVE T-AMT(T-LEV 2) TO D-AMT(2).
           MOVE T-AMT(T-LEV 10) TO D-AMT(3).
           MOVE T-AMT(T-LEV 3) TO D-AMT(4).
           MOVE T-AMT(T-LEV 11) TO D-AMT(5).
           MOVE T-AMT(T-LEV 4) TO D-AMT(6).
           MOVE T-AMT(T-LEV 5) TO D-AMT(7).
           MOVE 2 TO LN-FEED.
           IF BEG-BRANCH NOT = END-BRANCH
              PERFORM WRITE-DETAIL-LINE
           ELSE
      * ALWAYS PRINT BRANCH TOTALS AND COMPANY (FOR MULTIPLE INS. COS)
           IF SAVE-CODE = 1 OR 4
              PERFORM WRITE-DETAIL-LINE
           ELSE
              MOVE SPACES TO DETAIL-LINE.

      * PRINT ACCIDENT AND HEALTH TOTALS.
      * PRINT # REFUNDS, PR# 2642
           MOVE T-QTY(T-LEV 3) TO D-QTY(1).
           MOVE T-QTY(T-LEV 4) TO D-QTY(2).
           ADD T-QTY(T-LEV 3) T-QTY(T-LEV 4) GIVING D-QTY(3).
           MOVE "REF" TO D-TAG.
           MOVE T-AMT(T-LEV 6) TO D-AMT(2).
           MOVE T-AMT(T-LEV 12) TO D-AMT(3).
           MOVE T-AMT(T-LEV 7) TO D-AMT(4).
           MOVE T-AMT(T-LEV 13) TO D-AMT(5).
           MOVE T-AMT(T-LEV 8) TO D-AMT(6).
           MOVE T-AMT(T-LEV 9) TO D-AMT(7).
           IF BEG-BRANCH NOT = END-BRANCH
              PERFORM WRITE-DETAIL-LINE
           ELSE
      * ALWAYS PRINT BRANCH TOTALS AND COMPANY (FOR MULTIPLE INS. COS)
           IF SAVE-CODE = 1 OR 4
              PERFORM WRITE-DETAIL-LINE
           ELSE
              MOVE SPACES TO DETAIL-LINE.

           COMPUTE D-AMT(6) =
               (T-AMT(T-LEV 2) + T-AMT(T-LEV 10)) +
                 (T-AMT(T-LEV 11) + T-AMT(T-LEV 4)) +
                   T-AMT(T-LEV 3).

           COMPUTE D-AMT(7) =
               (T-AMT(T-LEV 6) + T-AMT(T-LEV 12)) +
                 (T-AMT(T-LEV 13) + T-AMT(T-LEV 8)) +
                   T-AMT(T-LEV 7).

           IF BEG-BRANCH NOT = END-BRANCH
              PERFORM WRITE-DETAIL-LINE
           ELSE
      * ALWAYS PRINT BRANCH TOTALS AND COMPANY (FOR MULTIPLE INS. COS)
           IF SAVE-CODE = 1 OR 4
              PERFORM WRITE-DETAIL-LINE
           ELSE
              MOVE SPACES TO DETAIL-LINE.


       END-T-TOTAL-LINE.
           IF T-LEV = 4
              MOVE 99 TO LN-COUNT.
       EXIT-T-TOTAL-LINE.
           EXIT.

      ***********************************************************
       CLASS-SCAN SECTION.
      ***********************************************************
           MOVE 0 TO LCSCAN-KEY.
           MOVE " " TO LCSCAN-VERIFY.
           MOVE LCSCAN-BUF TO SCAN-BUF.
           PERFORM CALL-WINDOW-PROGRAM.
           MOVE SCAN-BUF TO LCSCAN-BUF.
           IF SCAN-STATUS NOT = "00"
            OR LCSCAN-KEY = 0
               MOVE 9 TO IO-FG
               GO TO EXIT-CLASS-SCAN.
           IF ( CLASS-FG= "BEG" )
              MOVE "SNNN020LC1." TO VDU
           ELSE
              MOVE "SNNN020LC2." TO VDU.
           MOVE LCSCAN-KEY TO VDUNUM0.
           PERFORM SCREENIO.
           MOVE LCSCAN-KEY TO VDUNUM0.
           MOVE 0 TO IO-FG.

       EXIT-CLASS-SCAN.
           EXIT.

      *********************************************************
       FORM-RESET SECTION.
      *********************************************************
           MOVE EXT-CONAME-CONAME TO ICMORP-CONAME.
           MOVE EXT-CONAME-SYSDATE TO ICMORP-SYSDATE.
           DISPLAY ICMORP-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE ICMORP-FORM-FIELDS TO WR-BUF.
           MOVE ICMORP-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.
       FORM-RESET-EXIT.
           EXIT.

      *********************************************************
       SETUP-LINK-INFO SECTION.
      *********************************************************
           MOVE ICMORP-HELP--FILE TO HELP-LINK-FILE.
           MOVE ICMORP-HELP--DIR  TO HELP-LINK-DIR.

      *********************************************************
       VDU-CONVERT-FIELD SECTION.
      *********************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBLP/ICMORP_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ******************************************************************
       MISC-PARA SECTION.
      ******************************************************************
       COPY "LIBGB/GETPR.CPY".
       COPY "LIBGB/MKTEMP.CPY".
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/DACCESS.CPY".
       COPY "LIBGB/SYSTEM.CPY".
       COPY "LIBGB/RMFILE.CPY".
       CLEAR-WK-FILE.
              PERFORM VARYING SUB1 FROM 1 BY 1 UNTIL SUB1 > 4
                  MOVE 0 TO WORK-NEW-CL(SUB1) WORK-ADJ-CL(SUB1)
                            WORK-CAN-CL(SUB1) WORK-COM-CL(SUB1)
                            WORK-NEW-AH(SUB1) WORK-ADJ-AH(SUB1)
                            WORK-CAN-AH(SUB1) WORK-COM-AH(SUB1)
              END-PERFORM
              MOVE 0 TO WORK-CAN-AH-DATE WORK-CAN-CL-DATE
              MOVE SPACES TO WORK-ADDON-FG-CL WORK-CANCEL-FG-CL
                             WORK-ADDON-FG-AH WORK-CANCEL-FG-AH.
      *-----------------------------------------------------------------
       CLEAR-T-TOTALS.
           PERFORM VARYING SUB2 FROM 1 BY 1 UNTIL SUB2 > 4
                     AFTER SUB3 FROM 1 BY 1 UNTIL SUB3 > 13
              IF SUB3 < 5
                MOVE 0 TO T-QTY(SUB2 SUB3)
              END-IF
              MOVE 0 TO T-AMT(SUB2 SUB3)
           END-PERFORM.
      *-----------------------------------------------------------------
       ACCUM-T-TOTALS.
           PERFORM VARYING SUB2 FROM 1 BY 1 UNTIL SUB2 > 4
                     AFTER SUB3 FROM 1 BY 1 UNTIL SUB3 > 13
              IF SUB3 < 5
                ADD WK-QTY(SUB2 SUB3) TO T-QTY(SUB2 SUB3)
              END-IF
              ADD WK-AMT(SUB2 SUB3) TO T-AMT(SUB2 SUB3)
           END-PERFORM.
       CLEAR-TOT-WK.
      *    --------------------------------------------------------
      *    CLEAR TOT-WK-XXX TOTALS.
      *    --------------------------------------------------------
           PERFORM VARYING SUB3 FROM 1 BY 1 UNTIL SUB3 > 13
              IF SUB3 < 5
                MOVE 0 TO TOT-WK-QTY(SUB3)
              END-IF
              MOVE 0 TO TOT-WK-AMT(SUB3)
           END-PERFORM.
      *    ------------------------------------------------------------
      *    ACCUMULATE TOT-WK-XXX TOTALS: SDL + JDL + SLL + JLL
      *    ------------------------------------------------------------
       ACCUM-TOT-WK.
           PERFORM VARYING SUB2 FROM 1 BY 1 UNTIL SUB2 > 4
                     AFTER SUB3 FROM 1 BY 1 UNTIL SUB3 > 13
              IF SUB3 < 5
                ADD WK-QTY(SUB2 SUB3) TO TOT-WK-QTY(SUB3)
              END-IF
              ADD WK-AMT(SUB2 SUB3) TO TOT-WK-AMT(SUB3)
           END-PERFORM.
       IC-HEADER.
           MOVE HOLD-IC TO IC-CODE.
           PERFORM READ-IC1-FILE.

           IF IO-FG = 9
              MOVE "* INVALID *" TO IC-NAME.

           MOVE HOLD-IC TO H-ICNO.
           MOVE IC-NAME TO H-ICNAME.

           IF COMPANY-FG = "Y"
              MOVE SPACES TO H-INSCO H-ICNAME
              MOVE 0 TO H-ICNO.

      *-----------------------------------------------------------------
       BR-HEADER.
           MOVE HOLD-BR TO BR-NO.
           PERFORM READ-BR-FILE.
           IF IO-FG = 9
              MOVE "* INVALID *" TO BR-NAME.

           MOVE HOLD-BR TO H-BRNO.
           MOVE BR-NAME TO H-BRNAME.

      *-----------------------------------------------------------------
       GET-LN-BW-PATH.
           PERFORM CLOSE-LN1-FILE.
           PERFORM CLOSE-BW1-FILE.
           PERFORM CLOSE-IC1-FILE.

           PERFORM READ-BR-FILE.
           IF IO-FG = 9
              GO TO IO-ERROR.

           MOVE EXT-FILPATH-BASE           TO FILPATH-BASE.
           MOVE BR-MACHINE                 TO FILPATH-MACHINE.
           MOVE BR-DATA-PATH               TO FILPATH-DATA.

           MOVE "B"                        TO BW-PATH-OVERRIDE
                                              IC-PATH-OVERRIDE
                                              LN-PATH-OVERRIDE.

           PERFORM OPEN-LN1-FILE.
           PERFORM OPEN-BW1-FILE.
           PERFORM OPEN-IC1-FILE.

      *-----------------------------------------------------------------
       INIT-WK1-KEY.
           MOVE " " TO WK1-KEY.
           MOVE HOLD-IC TO WK-IC.
           MOVE LN-CLASS TO WK-CLASS.

      *-----------------------------------------------------------------
       TOTAL-LINE.
           MOVE "ALL" TO DETAIL-LINE.
           MOVE TOT-QTY(1) TO D-QTY(1).
           MOVE TOT-QTY(2) TO D-QTY(2).
           ADD TOT-QTY(1) TOT-QTY(2) GIVING D-QTY(3).
           MOVE "NEW" TO D-TAG.
           MOVE TOT-AMT(1) TO D-AMT(1).
           MOVE TOT-AMT(2) TO D-AMT(2).
           MOVE TOT-AMT(10) TO D-AMT(3).
           MOVE TOT-AMT(3) TO D-AMT(4).
           MOVE TOT-AMT(11) TO D-AMT(5).
           MOVE TOT-AMT(4) TO D-AMT(6).
           MOVE TOT-AMT(5) TO D-AMT(7).
           MOVE 3 TO LN-FEED.

           IF BEG-BRANCH NOT = END-BRANCH
              PERFORM WRITE-DETAIL-LINE
           ELSE
      * ALWAYS PRINT BRANCH TOTALS AND COMPANY (FOR MULTIPLE INS. COS)
           IF SAVE-CODE = 1 OR 4
              PERFORM WRITE-DETAIL-LINE
           ELSE
              MOVE SPACES TO DETAIL-LINE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE TOT-QTY(3) TO D-QTY(1).
           MOVE TOT-QTY(4) TO D-QTY(2).
           ADD TOT-QTY(3) TOT-QTY(4) GIVING D-QTY(3).
           MOVE "REF" TO D-TAG.
           MOVE TOT-AMT(6) TO D-AMT(2).
           MOVE TOT-AMT(12) TO D-AMT(3).
           MOVE TOT-AMT(7) TO D-AMT(4).
           MOVE TOT-AMT(13) TO D-AMT(5).
           MOVE TOT-AMT(8) TO D-AMT(6).
           MOVE TOT-AMT(9) TO D-AMT(7).

           IF BEG-BRANCH NOT = END-BRANCH
              PERFORM WRITE-DETAIL-LINE
           ELSE
      * ALWAYS PRINT BRANCH TOTALS AND COMPANY (FOR MULTIPLE INS. COS)
           IF SAVE-CODE = 1 OR 4
              PERFORM WRITE-DETAIL-LINE
           ELSE
              MOVE SPACES TO DETAIL-LINE.

      *NET C/L AH PREMIUMS
           COMPUTE D-AMT(6) =
              (TOT-AMT(2) + TOT-AMT(10)) +
               (TOT-AMT(11) + TOT-AMT(4)) +
                             TOT-AMT(3).

           COMPUTE D-AMT(7) =
              (TOT-AMT(6) + TOT-AMT(12)) +
               (TOT-AMT(13) + TOT-AMT(8)) +
                             TOT-AMT(7).
           IF BEG-BRANCH NOT = END-BRANCH
              PERFORM WRITE-DETAIL-LINE
           ELSE
      * ALWAYS PRINT BRANCH TOTALS AND COMPANY (FOR MULTIPLE INS. COS)
           IF SAVE-CODE = 1 OR 4
              PERFORM WRITE-DETAIL-LINE
           ELSE
              MOVE SPACES TO DETAIL-LINE.


           PERFORM T-TOTAL-LINE
             VARYING T-LEV FROM 1 BY 1 UNTIL T-LEV > 4.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           PERFORM CLEAR-TOTALS.
           PERFORM CLEAR-T-TOTALS.
      *    MOVE 99 TO LN-COUNT.

      *-----------------------------------------------------------------
       CLEAR-TOTALS.
           MOVE 0 TO
              TOT-AMT(1) TOT-AMT(2) TOT-AMT(3) TOT-AMT(4) TOT-AMT(5)
              TOT-AMT(6) TOT-AMT(7) TOT-AMT(8) TOT-AMT(9)
              TOT-AMT(10) TOT-AMT(11) TOT-AMT(12) TOT-AMT(13)
              TOT-QTY(1) TOT-QTY(2) TOT-QTY(3) TOT-QTY(4).

      *-----------------------------------------------------------------
       INVALID-RANGE.
           MOVE "INVALID RANGE" TO MESS.
           PERFORM SEND-MESS.

      *-----------------------------------------------------------------
       SEND-LEGEND.
           MOVE MESS TO WR-BUF.
           MOVE MESS-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      ******************************************************************
       IO-ROUTINES SECTION.
      ******************************************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBLP/LPTRSGS_SQL.CPY".
       COPY "LIBLP/LPTRPGS_SQL.CPY".
       COPY "LIBLP/LPTRLGS_SQL.CPY".
       COPY "LIBLP/LPSPGS_SQL.CPY".
       COPY "LIBLP/LPLNGS_SQL.CPY".
       COPY "LIBLP/LPICGS_SQL.CPY".
       COPY "LIBLP/LPBWGS_SQL.CPY".
       COPY "LIBGB/GBGRGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBPRINT.CPY".

      * COPY "LIBGB/GBBRRN.CPY".
      * COPYMEMBER: LIBGB/GBBRRN
      * 20220923 BAH ADDED BR_VERITEC_LICENSE #1570
      * 20230419 BAH ADDED BR-TEST-PP-AUTO-INSCOVR #1594A
      * 20230713 BAH ADDED BR-USE-NEW-WFARGO-ACCT #1616
       LOAD-BR-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( BR-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( BR-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO BR-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO BR-ALL-MACHINE
              MOVE BR-ALL-PATH             TO BR-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( BR-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO BR-ALL-BASE
              MOVE FILPATH-MACHINE         TO BR-ALL-MACHINE
              MOVE BR-ALL-PATH             TO BR-PATH.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    CREATE PATH FOR FILE SELECT AND SQL TABLE NAME
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( EXT-IO-BRFILE NOT = "S" )
              MOVE BR-PATH-BEG             TO BR-SELECT-PATH-BEG
              MOVE BR-PATH-FILE            TO BR-SELECT-PATH-FILE
           ELSE
              MOVE BR-PATH-SQL             TO BR-SELECT-PATH.

      *-----------------------------------------------------------------
       OPEN-BR-FILE.
           PERFORM LOAD-BR-FILE.
           PERFORM OPEN-IT.
           MOVE BR-SELECT-PATH TO E-FILE.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       START-BR-FILE.
           PERFORM START-IT.
           MOVE BR-SELECT-PATH TO E-FILE.
           MOVE BR-NO TO E-KEYX.

           IF ( BR-CURSOR-STAT-GOOD )
              PERFORM CLOSE-BR-FILE.

           IF ( QBR-WEND-NO = ZEROES    ) OR
              ( QBR-WEND-NO NOT NUMERIC )
              MOVE BR-NO                TO QBR-WBEG-NO
              MOVE ALL "9"              TO QBR-WEND-NO.

           EXEC SQL
            DECLARE BR_CURSOR CURSOR FOR
            SELECT BRFILE.BR_NO,
                   BRFILE.BR_NAME,
                   BRFILE.BR_LOAN_NUM_FLAG,
                   BRFILE.BR_DATA_PATH,
                   BRFILE.BR_MACHINE 
              FROM DBO.BRFILE
             WHERE BRFILE.BR_NO >= :QBR-WBEG-NO
               AND BRFILE.BR_NO <= :QBR-WEND-NO
             ORDER BY BR_NO
           END-EXEC.

           EXEC SQL
               OPEN BR_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE ZEROES TO QBR-WBEG-NO
                          QBR-WEND-NO.

           MOVE STAT---GOOD TO BR-CURSOR-STAT.

      *-----------------------------------------------------------------
       READ-BR-FILE.
           PERFORM READ-IT.
           MOVE BR-SELECT-PATH TO E-FILE.
           MOVE BR-NO TO E-KEYX.
           INITIALIZE QBR-REC.

           MOVE BR-NO TO QBR-NO.

           EXEC SQL
            SELECT BRFILE.BR_NO,
                   BRFILE.BR_NAME,
                   BRFILE.BR_LOAN_NUM_FLAG,
                   BRFILE.BR_DATA_PATH,
                   BRFILE.BR_MACHINE 
              INTO  
                  :QBR-NO,
                  :QBR-NAME,
                  :QBR-LOAN-NUM-FLAG,
                  :QBR-DATA-PATH,
                  :QBR-MACHINE
              FROM DBO.BRFILE
             WHERE BRFILE.BR_NO = :QBR-NO
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO READ-BR-FILE.

           IF ( IO-FG = 0 )
              PERFORM GET-BR-FIELDS.

      *-----------------------------------------------------------------
       READ-BR-FILE-NEXT.
           PERFORM READ-IT.
           MOVE BR-SELECT-PATH TO E-FILE.
           MOVE BR-NO TO E-KEYX.
           INITIALIZE QBR-REC.

           IF ( BR-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH BR_CURSOR 
                    INTO  
                      :QBR-NO,
                      :QBR-NAME,
                      :QBR-LOAN-NUM-FLAG,
                      :QBR-DATA-PATH,
                      :QBR-MACHINE
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO READ-BR-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-BR-FIELDS.

      *-----------------------------------------------------------------
       CLOSE-BR-FILE.
           PERFORM CLOSE-IT.
           IF ( BR-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE BR_CURSOR
              END-EXEC
              MOVE STAT---BAD TO BR-CURSOR-STAT.


       COPY "LIBGB/GBGR1RN.CPY".


      * COPY "LIBLP/LPLN1RN.CPY".
      * COPYMEMBER: LIBLP/LPLN1RN
       LOAD-LN1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( LN-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( LN-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO LN-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO LN-ALL-MACHINE
              MOVE EXT-FILPATH-DATA        TO LN-ALL-DATA
              MOVE LN-ALL-PATH             TO LN-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( LN-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO LN-ALL-BASE
              MOVE FILPATH-MACHINE         TO LN-ALL-MACHINE
              MOVE FILPATH-DATA            TO LN-ALL-DATA
              MOVE LN-ALL-PATH             TO LN-PATH.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    CREATE PATH FOR FILE SELECT AND SQL TABLE NAME
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( EXT-IO-LNFILE NOT = "S" )
              MOVE LN-PATH-BEG             TO LN-SELECT-PATH-BEG
              MOVE LN-PATH-FILE            TO LN-SELECT-PATH-FILE
           ELSE
              MOVE LN-PATH-SQL             TO LN-SELECT-PATH.

      *-----------------------------------------------------------------
       OPEN-LN1-FILE.
           PERFORM LOAD-LN1-FILE.
           PERFORM OPEN-IT.
           MOVE LN-SELECT-PATH TO E-FILE.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       READ-LN1-FILE.
           PERFORM READ-IT.
           MOVE LN-PATH-OWNBR  TO LN-OWNBR.
           MOVE LN-SELECT-PATH TO E-FILE.
           MOVE LN1-KEY        TO E-KEYX.

           MOVE LN-OWNBR  TO QLN-OWNBR.
           MOVE LN-ACCTNO TO QLN-ACCTNO.

           EXEC SQL
            SELECT LNFILE.LN_OWNBR,
                   LNFILE.LN_ACCTNO,
                   LNFILE.LN_CLASS,
                   LNFILE.LN_SUBCLASS,
                   LNFILE.LN_LAWCODE,
                   LNFILE.LN_SSNO1,
                   LNFILE.LN_MAKERCD1, 
                   LNFILE.LN_SSNO2,
                   LNFILE.LN_MAKERCD2, 
                   LNFILE.LN_SSNO3,
                   LNFILE.LN_MAKERCD3, 
                   LNFILE.LN_SSNO4,
                   LNFILE.LN_MAKERCD4, 
                   LNFILE.LN_ORGST, 
                   LNFILE.LN_SPRCLASS,
                   LNFILE.LN_STATFLAGS, 
                   CAST(LNFILE.LN_ORIG_1STPYDATE  AS VARCHAR(10)), 
                   CAST(LNFILE.LN_1STPYDATE       AS VARCHAR(10)), 
                   LNFILE.LN_MAINTFEE,
                   LNFILE.LN_ORGTERM,
                   LNFILE.LN_REGPYAMT,
                   LNFILE.LN_INSURED, 
                   LNFILE.LN_INSTYPES, 
                   LNFILE.LN_INSCOMP_1,
                   LNFILE.LN_INSCOMP_2,
                   LNFILE.LN_INSCOMP_3,
                   LNFILE.LN_INSCOMP_4,
                   LNFILE.LN_INSCOMP_5,
                   LNFILE.LN_INSCOMP_6,
                   LNFILE.LN_INSCOMP_7,
                   LNFILE.LN_INSCOMP_8,
                   LNFILE.LN_INSCOMP_9,
                   LNFILE.LN_INSCOMP_10,
                   CAST(LNFILE.LN_INSEFF_DATE_1   AS VARCHAR(10)), 
                   CAST(LNFILE.LN_INSEFF_DATE_2   AS VARCHAR(10)), 
                   CAST(LNFILE.LN_INSEFF_DATE_3   AS VARCHAR(10)), 
                   CAST(LNFILE.LN_INSEFF_DATE_4   AS VARCHAR(10)), 
                   CAST(LNFILE.LN_INSEFF_DATE_5   AS VARCHAR(10)), 
                   CAST(LNFILE.LN_INSEFF_DATE_6   AS VARCHAR(10)), 
                   CAST(LNFILE.LN_INSEFF_DATE_7   AS VARCHAR(10)), 
                   CAST(LNFILE.LN_INSEFF_DATE_8   AS VARCHAR(10)), 
                   CAST(LNFILE.LN_INSEFF_DATE_9   AS VARCHAR(10)),
                   CAST(LNFILE.LN_INSEFF_DATE_10  AS VARCHAR(10)), 
                   CAST(LNFILE.LN_INSEXP_DATE_1   AS VARCHAR(10)), 
                   CAST(LNFILE.LN_INSEXP_DATE_2   AS VARCHAR(10)), 
                   CAST(LNFILE.LN_INSEXP_DATE_3   AS VARCHAR(10)), 
                   CAST(LNFILE.LN_INSEXP_DATE_4   AS VARCHAR(10)), 
                   CAST(LNFILE.LN_INSEXP_DATE_5   AS VARCHAR(10)), 
                   CAST(LNFILE.LN_INSEXP_DATE_6   AS VARCHAR(10)), 
                   CAST(LNFILE.LN_INSEXP_DATE_7   AS VARCHAR(10)), 
                   CAST(LNFILE.LN_INSEXP_DATE_8   AS VARCHAR(10)), 
                   CAST(LNFILE.LN_INSEXP_DATE_9   AS VARCHAR(10)), 
                   CAST(LNFILE.LN_INSEXP_DATE_10  AS VARCHAR(10)), 
                   LNFILE.LN_INSCOVR_1,
                   LNFILE.LN_INSCOVR_2,
                   LNFILE.LN_INSCOVR_3,
                   LNFILE.LN_INSCOVR_4,
                   LNFILE.LN_INSCOVR_5,
                   LNFILE.LN_INSCOVR_6,
                   LNFILE.LN_INSCOVR_7,
                   LNFILE.LN_INSCOVR_8,
                   LNFILE.LN_INSCOVR_9,
                   LNFILE.LN_INSCOVR_10,
                   LNFILE.LN_TOTNODEF,
                   LNFILE.LN_CL_REBATE, 
                   LNFILE.LN_AH_REBATE, 
                   LNFILE.LN_PP_REBATE, 
                   LNFILE.LN_INT_REBATE, 
                   LNFILE.LN_SC_REBATE, 
                   LNFILE.LN_MF_REBATE, 
                   LNFILE.LN_DF_REBATE, 
                   LNFILE.LN_EX_REBATE, 
                   LNFILE.LN_O1_REBATE, 
                   LNFILE.LN_O2_REBATE, 
                   LNFILE.LN_O3_REBATE, 
                   LNFILE.LN_O4_REBATE, 
                   LNFILE.LN_O5_REBATE, 
                   LNFILE.LN_O6_REBATE, 
                   LNFILE.LN_O7_REBATE, 
                   LNFILE.LN_UN_O7_REB, 
                   LNFILE.LN_UN_O8_REB, 
                   LNFILE.LN_UN_O9_REB, 
                   LNFILE.LN_UN_O10_REB, 
                   LNFILE.LN_SUM_ORGTERM,
                   LNFILE.LN_UNITPER_CD,
                   LNFILE.LN_UNITPER_FREQ
              INTO  
                  :QLN-OWNBR,
                  :QLN-ACCTNO,
                  :QLN-CLASS,
                  :QLN-SUBCLASS,
                  :QLN-LAWCODE,
                  :QLN-SSNO1,
                  :QLN-MAKERCD1,
                  :QLN-SSNO2,
                  :QLN-MAKERCD2,
                  :QLN-SSNO3,
                  :QLN-MAKERCD3,
                  :QLN-SSNO4,
                  :QLN-MAKERCD4,
                  :QLN-ORGST,
                  :QLN-SPRCLASS,
                  :QLN-STATFLAGS,
                  :QLN-ORIG-1STPYDATE,
                  :QLN-1STPYDATE,
                  :QLN-MAINTFEE,
                  :QLN-ORGTERM,
                  :QLN-REGPYAMT,
                  :QLN-INSURED,
                  :QLN-INSTYPES,
                  :QLN-INSCOMP-1,
                  :QLN-INSCOMP-2,
                  :QLN-INSCOMP-3,
                  :QLN-INSCOMP-4,
                  :QLN-INSCOMP-5,
                  :QLN-INSCOMP-6,
                  :QLN-INSCOMP-7,
                  :QLN-INSCOMP-8,
                  :QLN-INSCOMP-9,
                  :QLN-INSCOMP-10,
                  :QLN-INSEFF-DATE-1,
                  :QLN-INSEFF-DATE-2,
                  :QLN-INSEFF-DATE-3,
                  :QLN-INSEFF-DATE-4,
                  :QLN-INSEFF-DATE-5,
                  :QLN-INSEFF-DATE-6,
                  :QLN-INSEFF-DATE-7,
                  :QLN-INSEFF-DATE-8,
                  :QLN-INSEFF-DATE-9,
                  :QLN-INSEFF-DATE-10,
                  :QLN-INSEXP-DATE-1,
                  :QLN-INSEXP-DATE-2,
                  :QLN-INSEXP-DATE-3,
                  :QLN-INSEXP-DATE-4,
                  :QLN-INSEXP-DATE-5,
                  :QLN-INSEXP-DATE-6,
                  :QLN-INSEXP-DATE-7,
                  :QLN-INSEXP-DATE-8,
                  :QLN-INSEXP-DATE-9,
                  :QLN-INSEXP-DATE-10,
                  :QLN-INSCOVR-1,
                  :QLN-INSCOVR-2,
                  :QLN-INSCOVR-3,
                  :QLN-INSCOVR-4,
                  :QLN-INSCOVR-5,
                  :QLN-INSCOVR-6,
                  :QLN-INSCOVR-7,
                  :QLN-INSCOVR-8,
                  :QLN-INSCOVR-9,
                  :QLN-INSCOVR-10,
                  :QLN-TOTNODEF,
                  :QLN-CL-REBATE,
                  :QLN-AH-REBATE,
                  :QLN-PP-REBATE,
                  :QLN-INT-REBATE,
                  :QLN-SC-REBATE,
                  :QLN-MF-REBATE,
                  :QLN-DF-REBATE,
                  :QLN-EX-REBATE,
                  :QLN-O1-REBATE,
                  :QLN-O2-REBATE,
                  :QLN-O3-REBATE,
                  :QLN-O4-REBATE,
                  :QLN-O5-REBATE,
                  :QLN-O6-REBATE,
                  :QLN-O7-REBATE,
                  :QLN-UN-O7-REB,
                  :QLN-UN-O8-REB,
                  :QLN-UN-O9-REB,
                  :QLN-UN-O10-REB,
                  :QLN-SUM-ORGTERM,
                  :QLN-UNITPER-CD,
                  :QLN-UNITPER-FREQ
            FROM DBO.LNFILE
            WHERE LNFILE.LN_OWNBR  = :QLN-OWNBR
              AND LNFILE.LN_ACCTNO = :QLN-ACCTNO
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-LN1-FILE.

           IF ( IO-FG = 0 )
              PERFORM GET-LN-FIELDS.

      *-----------------------------------------------------------------
       CLOSE-LN1-FILE.

           PERFORM CLOSE-IT.
 

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
              MOVE TRP-ALL-PATH             TO TRP-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( TRP-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO TRP-ALL-BASE
              MOVE FILPATH-MACHINE         TO TRP-ALL-MACHINE
              MOVE FILPATH-DATA            TO TRP-ALL-DATA
              MOVE TRP-ALL-PATH             TO TRP-PATH.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    CREATE PATH FOR FILE SELECT AND SQL TABLE NAME
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( EXT-IO-TRPFILE NOT = "S" )
              MOVE TRP-PATH-BEG             TO TRP-SELECT-PATH-BEG
              MOVE TRP-PATH-FILE            TO TRP-SELECT-PATH-FILE
           ELSE
              MOVE TRP-PATH-SQL             TO TRP-SELECT-PATH.

      *-----------------------------------------------------------------
       OPEN-TRP1-FILE.
           PERFORM LOAD-TRP1-FILE.
           PERFORM OPEN-IT.
           MOVE TRP-SELECT-PATH TO E-FILE.

           IF TRP1-CONN-OPEN-SW = "N"
              MOVE "Y" TO TRP1-CONN-OPEN-SW
              EXEC SQL CONNECT TO :SQL-CONNECT-SERVER AS TRP1_CONNECT
                      USER :SQL-CONNECT-USERID USING :SQL-CONNECT-PSWD
              END-EXEC.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

           EXEC SQL SET CONNECTION C1 END-EXEC.

      *-----------------------------------------------------------------
       START-TRP1-FILE.
           PERFORM START-IT.
           MOVE TRP-PATH-OWNBR  TO TP-BRNO.
           MOVE TRP-SELECT-PATH TO E-FILE.
           MOVE TRP1-KEY        TO E-KEYX.

           IF ( TRP1-CURSOR-STAT-GOOD )
              PERFORM CLOSE-TRP1-FILE.

           IF ( QTRP1-WEND-LINE = ZEROES    ) OR
              ( QTRP1-WEND-LINE NOT NUMERIC )
              MOVE TRP-PATH-OWNBR        TO QTRP1-WBEG-BRNO
              MOVE TRP-PATH-OWNBR        TO QTRP1-WEND-BRNO
              MOVE TP-DATE               TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD  TO QTRP1-WBEG-DATE
              MOVE 99991231         TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD  TO QTRP1-WEND-DATE
              MOVE TP-CLASS             TO QTRP1-WBEG-CLASS
              MOVE TP-ACCTNO            TO QTRP1-WBEG-ACCTNO
              MOVE TP-LINE              TO QTRP1-WBEG-LINE
              MOVE ALL "9"              TO QTRP1-WEND-CLASS
                                           QTRP1-WEND-ACCTNO
                                           QTRP1-WEND-LINE
      *- - - -{ ADD ONE TO WHERE SCAN CONTINUE FIELDS
              MOVE TP-DATE              TO NDTE-DATE
              MOVE 1                    TO NDTE-HOLD
              PERFORM INCREMENT-DAYS
              MOVE NDTE-DATE            TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD  TO QTRP1-WSBEG-DATE

              IF ( TP-CLASS = 999 )
                 MOVE TP-CLASS          TO QTRP1-WSBEG-CLASS
              ELSE
                 COMPUTE QTRP1-WSBEG-CLASS  = TP-CLASS + 1
              END-IF

              IF ( TP-ACCTNO = 99999999 )
                 MOVE TP-ACCTNO         TO QTRP1-WSBEG-ACCTNO
              ELSE
                 COMPUTE QTRP1-WSBEG-ACCTNO = TP-ACCTNO + 1
              END-IF

              IF ( TP-LINE = 999 )
                 MOVE TP-LINE           TO QTRP1-WSBEG-LINE
              ELSE
                 COMPUTE QTRP1-WSBEG-LINE   = TP-LINE + 1.

      * STORED PROCEDURE SELECT STATEMENT: 
      *
      *     SELECT TRPFILE.TP_BRNO,
      *            CAST(TRPFILE.TP_DATE          AS VARCHAR(10)), 
      *            TRPFILE.TP_CLASS,
      *            TRPFILE.TP_ACCTNO,
      *            TRPFILE.TP_LINE,
      *            TRPFILE.TP_TRCD, 
      *            TRPFILE.TP_REFNO, 
      *            CAST(TRPFILE.TP_PAYDATE       AS VARCHAR(10)), 
      *            TRPFILE.TP_TRAMT,
      *            TRPFILE.TP_APCUR,
      *            TRPFILE.TP_INSOURS, 
      *            TRPFILE.TP_OTHBR_FG,
      *            TRPFILE.TP_INSCOMM,
      *            TRPFILE.TP_INSCOMP,
      *            TRPFILE.TP_INS_TAX_RB,
      *            TRPFILE.TP_ADDON_TYP,
      *            LNFILE.LN_OWNBR,
      *            LNFILE.LN_ACCTNO,
      *            LNFILE.LN_CLASS,
      *            LNFILE.LN_SUBCLASS,
      *            LNFILE.LN_LAWCODE,
      *            LNFILE.LN_SSNO1,
      *            LNFILE.LN_MAKERCD1, 
      *            LNFILE.LN_SSNO2,
      *            LNFILE.LN_MAKERCD2, 
      *            LNFILE.LN_SSNO3,
      *            LNFILE.LN_MAKERCD3, 
      *            LNFILE.LN_SSNO4,
      *            LNFILE.LN_MAKERCD4, 
      *            LNFILE.LN_ORGST, 
      *            LNFILE.LN_SPRCLASS,
      *            LNFILE.LN_STATFLAGS, 
      *            CAST(LNFILE.LN_ORIG_1STPYDATE  AS VARCHAR(10)), 
      *            CAST(LNFILE.LN_1STPYDATE       AS VARCHAR(10)), 
      *            LNFILE.LN_MAINTFEE,
      *            LNFILE.LN_ORGTERM,
      *            LNFILE.LN_REGPYAMT,
      *            LNFILE.LN_INSURED, 
      *            LNFILE.LN_INSTYPES, 
      *            LNFILE.LN_INSCOMP_1,
      *            LNFILE.LN_INSCOMP_2,
      *            LNFILE.LN_INSCOMP_3,
      *            LNFILE.LN_INSCOMP_4,
      *            LNFILE.LN_INSCOMP_5,
      *            LNFILE.LN_INSCOMP_6,
      *            LNFILE.LN_INSCOMP_7,
      *            LNFILE.LN_INSCOMP_8,
      *            LNFILE.LN_INSCOMP_9,
      *            LNFILE.LN_INSCOMP_10,
      *            CAST(LNFILE.LN_INSEFF_DATE_1   AS VARCHAR(10)), 
      *            CAST(LNFILE.LN_INSEFF_DATE_2   AS VARCHAR(10)), 
      *            CAST(LNFILE.LN_INSEFF_DATE_3   AS VARCHAR(10)), 
      *            CAST(LNFILE.LN_INSEFF_DATE_4   AS VARCHAR(10)), 
      *            CAST(LNFILE.LN_INSEFF_DATE_5   AS VARCHAR(10)), 
      *            CAST(LNFILE.LN_INSEFF_DATE_6   AS VARCHAR(10)), 
      *            CAST(LNFILE.LN_INSEFF_DATE_7   AS VARCHAR(10)), 
      *            CAST(LNFILE.LN_INSEFF_DATE_8   AS VARCHAR(10)), 
      *            CAST(LNFILE.LN_INSEFF_DATE_9   AS VARCHAR(10)),
      *            CAST(LNFILE.LN_INSEFF_DATE_10  AS VARCHAR(10)), 
      *            CAST(LNFILE.LN_INSEXP_DATE_1   AS VARCHAR(10)), 
      *            CAST(LNFILE.LN_INSEXP_DATE_2   AS VARCHAR(10)), 
      *            CAST(LNFILE.LN_INSEXP_DATE_3   AS VARCHAR(10)), 
      *            CAST(LNFILE.LN_INSEXP_DATE_4   AS VARCHAR(10)), 
      *            CAST(LNFILE.LN_INSEXP_DATE_5   AS VARCHAR(10)), 
      *            CAST(LNFILE.LN_INSEXP_DATE_6   AS VARCHAR(10)), 
      *            CAST(LNFILE.LN_INSEXP_DATE_7   AS VARCHAR(10)), 
      *            CAST(LNFILE.LN_INSEXP_DATE_8   AS VARCHAR(10)), 
      *            CAST(LNFILE.LN_INSEXP_DATE_9   AS VARCHAR(10)), 
      *            CAST(LNFILE.LN_INSEXP_DATE_10  AS VARCHAR(10)), 
      *            LNFILE.LN_INSCOVR_1,
      *            LNFILE.LN_INSCOVR_2,
      *            LNFILE.LN_INSCOVR_3,
      *            LNFILE.LN_INSCOVR_4,
      *            LNFILE.LN_INSCOVR_5,
      *            LNFILE.LN_INSCOVR_6,
      *            LNFILE.LN_INSCOVR_7,
      *            LNFILE.LN_INSCOVR_8,
      *            LNFILE.LN_INSCOVR_9,
      *            LNFILE.LN_INSCOVR_10,
      *            LNFILE.LN_TOTNODEF,
      *            LNFILE.LN_CL_REBATE, 
      *            LNFILE.LN_AH_REBATE, 
      *            LNFILE.LN_PP_REBATE, 
      *            LNFILE.LN_INT_REBATE, 
      *            LNFILE.LN_SC_REBATE, 
      *            LNFILE.LN_MF_REBATE, 
      *            LNFILE.LN_DF_REBATE, 
      *            LNFILE.LN_EX_REBATE, 
      *            LNFILE.LN_O1_REBATE, 
      *            LNFILE.LN_O2_REBATE, 
      *            LNFILE.LN_O3_REBATE, 
      *            LNFILE.LN_O4_REBATE, 
      *            LNFILE.LN_O5_REBATE, 
      *            LNFILE.LN_O6_REBATE, 
      *            LNFILE.LN_O7_REBATE, 
      *            LNFILE.LN_UN_O7_REB, 
      *            LNFILE.LN_UN_O8_REB, 
      *            LNFILE.LN_UN_O9_REB, 
      *            LNFILE.LN_UN_O10_REB, 
      *            LNFILE.LN_SUM_ORGTERM,
      *            LNFILE.LN_UNITPER_CD,
      *            LNFILE.LN_UNITPER_FREQ
      *       FROM DBO.TRPFILE TRPFILE
      *      INNER JOIN
      *            DBO.LNFILE  LNFILE
      *         ON ( LNFILE.LN_OWNBR        = TRPFILE.TP_BRNO
      *        AND   LNFILE.LN_ACCTNO       = TRPFILE.TP_ACCTNO )
      *      WHERE   TRPFILE.TP_BRNO        = @QTRP1_WBEG_BRNO   
      *        AND   TRPFILE.TP_DATE  BETWEEN @QTRP1_WBEG_DATE
      *                                   AND @QTRP1_WEND_DATE  
      *        AND   TRPFILE.TP_CLASS BETWEEN @QTRP1_WBEG_CLASS
      *                                   AND @QTRP1_WEND_CLASS 
      *      ORDER BY TRPFILE.TP_BRNO,
      *               TRPFILE.TP_DATE,
      *               TRPFILE.TP_CLASS,
      *               TRPFILE.TP_ACCTNO,
      *               TRPFILE.TP_LINE

           EXEC SQL SET CONNECTION TRP1_CONNECT END-EXEC.

           EXEC SQL
            DECLARE TRP1_CURSOR CURSOR FOR
                :TRP1-RETURN-CODE = EXEC SP_ICMORP_TRPFILE_SELECT
              ( :QTRP1-WBEG-BRNO,   
                :QTRP1-WBEG-DATE,   
                :QTRP1-WEND-DATE,   
                :QTRP1-WBEG-CLASS,  
                :QTRP1-WEND-CLASS ) 
           END-EXEC.
  
           EXEC SQL
               OPEN TRP1_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE ZEROES TO QTRP1-WBEG-BRNO
                          QTRP1-WEND-BRNO
                          QTRP1-WBEG-CLASS
                          QTRP1-WEND-CLASS
                          QTRP1-WBEG-ACCTNO
                          QTRP1-WEND-ACCTNO
                          QTRP1-WBEG-LINE
                          QTRP1-WEND-LINE
                          QTRP1-WSBEG-CLASS
                          QTRP1-WSBEG-ACCTNO
                          QTRP1-WSBEG-LINE.
           MOVE SPACES TO QTRP1-WBEG-DATE
                          QTRP1-WEND-DATE
                          QTRP1-WSBEG-DATE.

           MOVE STAT---GOOD TO TRP1-CURSOR-STAT.

           EXEC SQL SET CONNECTION C1 END-EXEC.

      *-----------------------------------------------------------------
       READ-TRP1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE TRP-PATH-OWNBR  TO TP-BRNO.
           MOVE TRP-SELECT-PATH TO E-FILE.
           MOVE TRP1-KEY        TO E-KEYX.
           INITIALIZE QTRP-REC.
           INITIALIZE QLN-REC.

           EXEC SQL SET CONNECTION TRP1_CONNECT END-EXEC.

           IF ( TRP1-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH TRP1_CURSOR INTO 
                     :QTP-BRNO,
                     :QTP-DATE,
                     :QTP-CLASS,
                     :QTP-ACCTNO,
                     :QTP-LINE,
                     :QTP-TRCD,
                     :QTP-REFNO,
                     :QTP-PAYDATE,
                     :QTP-TRAMT,
                     :QTP-APCUR,
                     :QTP-INSOURS,
                     :QTP-OTHBR-FG,
                     :QTP-INSCOMM,
                     :QTP-INSCOMP,
                     :QTP-INS-TAX-RB,
                     :QTP-ADDON-TYP,
                     :QLN-OWNBR,
                     :QLN-ACCTNO,
                     :QLN-CLASS,
                     :QLN-SUBCLASS,
                     :QLN-LAWCODE,
                     :QLN-SSNO1,
                     :QLN-MAKERCD1,
                     :QLN-SSNO2,
                     :QLN-MAKERCD2,
                     :QLN-SSNO3,
                     :QLN-MAKERCD3,
                     :QLN-SSNO4,
                     :QLN-MAKERCD4,
                     :QLN-ORGST,
                     :QLN-SPRCLASS,
                     :QLN-STATFLAGS,
                     :QLN-ORIG-1STPYDATE,
                     :QLN-1STPYDATE,
                     :QLN-MAINTFEE,
                     :QLN-ORGTERM,
                     :QLN-REGPYAMT,
                     :QLN-INSURED,
                     :QLN-INSTYPES,
                     :QLN-INSCOMP-1,
                     :QLN-INSCOMP-2,
                     :QLN-INSCOMP-3,
                     :QLN-INSCOMP-4,
                     :QLN-INSCOMP-5,
                     :QLN-INSCOMP-6,
                     :QLN-INSCOMP-7,
                     :QLN-INSCOMP-8,
                     :QLN-INSCOMP-9,
                     :QLN-INSCOMP-10,
                     :QLN-INSEFF-DATE-1,
                     :QLN-INSEFF-DATE-2,
                     :QLN-INSEFF-DATE-3,
                     :QLN-INSEFF-DATE-4,
                     :QLN-INSEFF-DATE-5,
                     :QLN-INSEFF-DATE-6,
                     :QLN-INSEFF-DATE-7,
                     :QLN-INSEFF-DATE-8,
                     :QLN-INSEFF-DATE-9,
                     :QLN-INSEFF-DATE-10,
                     :QLN-INSEXP-DATE-1,
                     :QLN-INSEXP-DATE-2,
                     :QLN-INSEXP-DATE-3,
                     :QLN-INSEXP-DATE-4,
                     :QLN-INSEXP-DATE-5,
                     :QLN-INSEXP-DATE-6,
                     :QLN-INSEXP-DATE-7,
                     :QLN-INSEXP-DATE-8,
                     :QLN-INSEXP-DATE-9,
                     :QLN-INSEXP-DATE-10,
                     :QLN-INSCOVR-1,
                     :QLN-INSCOVR-2,
                     :QLN-INSCOVR-3,
                     :QLN-INSCOVR-4,
                     :QLN-INSCOVR-5,
                     :QLN-INSCOVR-6,
                     :QLN-INSCOVR-7,
                     :QLN-INSCOVR-8,
                     :QLN-INSCOVR-9,
                     :QLN-INSCOVR-10,
                     :QLN-TOTNODEF,
                     :QLN-CL-REBATE,
                     :QLN-AH-REBATE,
                     :QLN-PP-REBATE,
                     :QLN-INT-REBATE,
                     :QLN-SC-REBATE,
                     :QLN-MF-REBATE,
                     :QLN-DF-REBATE,
                     :QLN-EX-REBATE,
                     :QLN-O1-REBATE,
                     :QLN-O2-REBATE,
                     :QLN-O3-REBATE,
                     :QLN-O4-REBATE,
                     :QLN-O5-REBATE,
                     :QLN-O6-REBATE,
                     :QLN-O7-REBATE,
                     :QLN-UN-O7-REB,
                     :QLN-UN-O8-REB,
                     :QLN-UN-O9-REB,
                     :QLN-UN-O10-REB,
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
              PERFORM GET-LN-FIELDS.

           EXEC SQL SET CONNECTION C1 END-EXEC.

      *-----------------------------------------------------------------
       CLOSE-TRP1-FILE.

           PERFORM CLOSE-IT.
           IF (( TRP1-CURSOR-STAT-GOOD ) AND
               ( TRP1-CONN-OPEN-SW = "Y" ))
              EXEC SQL SET CONNECTION TRP1_CONNECT  END-EXEC
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


      * COPY "LIBLP/LPTRL1RN.CPY".
      * COPYMEMBER: LIBLP/LPTR1RN
       LOAD-TRL1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( TRL-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( TRL-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO TRL-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO TRL-ALL-MACHINE
              MOVE EXT-FILPATH-DATA        TO TRL-ALL-DATA
              MOVE TRL-ALL-PATH             TO TRL-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( TRL-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO TRL-ALL-BASE
              MOVE FILPATH-MACHINE         TO TRL-ALL-MACHINE
              MOVE FILPATH-DATA            TO TRL-ALL-DATA
              MOVE TRL-ALL-PATH             TO TRL-PATH.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    CREATE PATH FOR FILE SELECT AND SQL TABLE NAME
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( EXT-IO-TRLFILE NOT = "S" )
              MOVE TRL-PATH-BEG             TO TRL-SELECT-PATH-BEG
              MOVE TRL-PATH-FILE            TO TRL-SELECT-PATH-FILE
           ELSE
              MOVE TRL-PATH-SQL             TO TRL-SELECT-PATH.

      *-----------------------------------------------------------------
       OPEN-TRL1-FILE.
           PERFORM LOAD-TRL1-FILE.
           PERFORM OPEN-IT.
           MOVE TRL-SELECT-PATH TO E-FILE.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       START-TRL1-FILE.
           PERFORM START-IT.
           MOVE TRL-PATH-OWNBR  TO TL-BRNO.
           MOVE TRL-SELECT-PATH TO E-FILE.
           MOVE TRL1-KEY        TO E-KEYX.

           IF ( TRL1-CURSOR-STAT-GOOD )
              PERFORM CLOSE-TRL1-FILE.

           IF ( QTRL1-WEND-LINE = ZEROES    ) OR
              ( QTRL1-WEND-LINE NOT NUMERIC )
              MOVE TRL-PATH-OWNBR        TO QTRL1-WBEG-BRNO
              MOVE TRL-PATH-OWNBR        TO QTRL1-WEND-BRNO
              MOVE TL-DATE               TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD  TO QTRL1-WBEG-DATE
              MOVE JULIAN-END           TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD  TO QTRL1-WEND-DATE
              MOVE TL-CLASS             TO QTRL1-WBEG-CLASS
              MOVE TL-ACCTNO            TO QTRL1-WBEG-ACCTNO
              MOVE TL-LAWCODE           TO QTRL1-WBEG-LAWCODE
              MOVE TL-LINE              TO QTRL1-WBEG-LINE
              MOVE ALL "9"              TO QTRL1-WEND-CLASS
                                           QTRL1-WEND-ACCTNO
                                           QTRL1-WEND-LAWCODE
                                           QTRL1-WEND-LINE.

           EXEC SQL
            DECLARE TRL1_CURSOR CURSOR FOR
              SELECT TRLFILE.TL_BRNO,
                     CAST(TRLFILE.TL_DATE        AS VARCHAR(10)),
                     TRLFILE.TL_CLASS,
                     TRLFILE.TL_ACCTNO,
                     TRLFILE.TL_LAWCODE,
                     TRLFILE.TL_LINE,
                     TRLFILE.TL_INSCOMP_1,
                     TRLFILE.TL_INSCOMP_2,
                     TRLFILE.TL_INSCOMP_3,
                     TRLFILE.TL_INSCOMP_4,
                     TRLFILE.TL_INSCOMP_5,
                     TRLFILE.TL_INSCOMP_6,
                     TRLFILE.TL_INSCOMP_7,
                     TRLFILE.TL_INSCOMP_8,
                     TRLFILE.TL_INSCOMP_9,
                     TRLFILE.TL_INSCOMP_10,
                     TRLFILE.TL_INSOURS_1, 
                     TRLFILE.TL_INSOURS_2, 
                     TRLFILE.TL_INSOURS_3, 
                     TRLFILE.TL_INSOURS_4, 
                     TRLFILE.TL_INSOURS_5, 
                     TRLFILE.TL_INSOURS_6, 
                     TRLFILE.TL_INSOURS_7, 
                     TRLFILE.TL_INSOURS_8, 
                     TRLFILE.TL_INSOURS_9, 
                     TRLFILE.TL_INSOURS_10, 
                     TRLFILE.TL_INSPREM_1,
                     TRLFILE.TL_INSPREM_2,
                     TRLFILE.TL_INSPREM_3,
                     TRLFILE.TL_INSPREM_4,
                     TRLFILE.TL_INSPREM_5,
                     TRLFILE.TL_INSPREM_6,
                     TRLFILE.TL_INSPREM_7,
                     TRLFILE.TL_INSPREM_8,
                     TRLFILE.TL_INSPREM_9,
                     TRLFILE.TL_INSPREM_10,
                     TRLFILE.TL_INSCOMM_1,
                     TRLFILE.TL_INSCOMM_2,
                     TRLFILE.TL_INSCOMM_3,
                     TRLFILE.TL_INSCOMM_4,
                     TRLFILE.TL_INSCOMM_5,
                     TRLFILE.TL_INSCOMM_6,
                     TRLFILE.TL_INSCOMM_7,
                     TRLFILE.TL_INSCOMM_8,
                     TRLFILE.TL_INSCOMM_9,
                     TRLFILE.TL_INSCOMM_10,
                     LNFILE.LN_OWNBR,
                     LNFILE.LN_ACCTNO,
                     LNFILE.LN_CLASS,
                     LNFILE.LN_SUBCLASS,
                     LNFILE.LN_LAWCODE,
                     LNFILE.LN_SSNO1,
                     LNFILE.LN_MAKERCD1, 
                     LNFILE.LN_SSNO2,
                     LNFILE.LN_MAKERCD2, 
                     LNFILE.LN_SSNO3,
                     LNFILE.LN_MAKERCD3, 
                     LNFILE.LN_SSNO4,
                     LNFILE.LN_MAKERCD4, 
                     LNFILE.LN_ORGST, 
                     LNFILE.LN_SPRCLASS,
                     LNFILE.LN_STATFLAGS, 
                     CAST(LNFILE.LN_ORIG_1STPYDATE AS VARCHAR(10)),
                     CAST(LNFILE.LN_1STPYDATE      AS VARCHAR(10)),
                     LNFILE.LN_MAINTFEE,
                     LNFILE.LN_ORGTERM,
                     LNFILE.LN_REGPYAMT,
                     LNFILE.LN_INSURED, 
                     LNFILE.LN_INSTYPES, 
                     LNFILE.LN_INSCOMP_1,
                     LNFILE.LN_INSCOMP_2,
                     LNFILE.LN_INSCOMP_3,
                     LNFILE.LN_INSCOMP_4,
                     LNFILE.LN_INSCOMP_5,
                     LNFILE.LN_INSCOMP_6,
                     LNFILE.LN_INSCOMP_7,
                     LNFILE.LN_INSCOMP_8,
                     LNFILE.LN_INSCOMP_9,
                     LNFILE.LN_INSCOMP_10,
                     CAST(LNFILE.LN_INSEFF_DATE_1  AS VARCHAR(10)), 
                     CAST(LNFILE.LN_INSEFF_DATE_2  AS VARCHAR(10)), 
                     CAST(LNFILE.LN_INSEFF_DATE_3  AS VARCHAR(10)), 
                     CAST(LNFILE.LN_INSEFF_DATE_4  AS VARCHAR(10)), 
                     CAST(LNFILE.LN_INSEFF_DATE_5  AS VARCHAR(10)), 
                     CAST(LNFILE.LN_INSEFF_DATE_6  AS VARCHAR(10)), 
                     CAST(LNFILE.LN_INSEFF_DATE_7  AS VARCHAR(10)), 
                     CAST(LNFILE.LN_INSEFF_DATE_8  AS VARCHAR(10)), 
                     CAST(LNFILE.LN_INSEFF_DATE_9  AS VARCHAR(10)), 
                     CAST(LNFILE.LN_INSEFF_DATE_10 AS VARCHAR(10)), 
                     CAST(LNFILE.LN_INSEXP_DATE_1  AS VARCHAR(10)), 
                     CAST(LNFILE.LN_INSEXP_DATE_2  AS VARCHAR(10)), 
                     CAST(LNFILE.LN_INSEXP_DATE_3  AS VARCHAR(10)), 
                     CAST(LNFILE.LN_INSEXP_DATE_4  AS VARCHAR(10)), 
                     CAST(LNFILE.LN_INSEXP_DATE_5  AS VARCHAR(10)), 
                     CAST(LNFILE.LN_INSEXP_DATE_6  AS VARCHAR(10)), 
                     CAST(LNFILE.LN_INSEXP_DATE_7  AS VARCHAR(10)), 
                     CAST(LNFILE.LN_INSEXP_DATE_8  AS VARCHAR(10)), 
                     CAST(LNFILE.LN_INSEXP_DATE_9  AS VARCHAR(10)), 
                     CAST(LNFILE.LN_INSEXP_DATE_10 AS VARCHAR(10)), 
                     LNFILE.LN_INSCOVR_1,
                     LNFILE.LN_INSCOVR_2,
                     LNFILE.LN_INSCOVR_3,
                     LNFILE.LN_INSCOVR_4,
                     LNFILE.LN_INSCOVR_5,
                     LNFILE.LN_INSCOVR_6,
                     LNFILE.LN_INSCOVR_7,
                     LNFILE.LN_INSCOVR_8,
                     LNFILE.LN_INSCOVR_9,
                     LNFILE.LN_INSCOVR_10,
                     LNFILE.LN_TOTNODEF,
                     LNFILE.LN_CL_REBATE, 
                     LNFILE.LN_AH_REBATE, 
                     LNFILE.LN_PP_REBATE, 
                     LNFILE.LN_INT_REBATE, 
                     LNFILE.LN_SC_REBATE, 
                     LNFILE.LN_MF_REBATE, 
                     LNFILE.LN_DF_REBATE, 
                     LNFILE.LN_EX_REBATE, 
                     LNFILE.LN_O1_REBATE, 
                     LNFILE.LN_O2_REBATE, 
                     LNFILE.LN_O3_REBATE, 
                     LNFILE.LN_O4_REBATE, 
                     LNFILE.LN_O5_REBATE, 
                     LNFILE.LN_O6_REBATE, 
                     LNFILE.LN_O7_REBATE, 
                     LNFILE.LN_UN_O7_REB, 
                     LNFILE.LN_UN_O8_REB, 
                     LNFILE.LN_UN_O9_REB, 
                     LNFILE.LN_UN_O10_REB,
                     LNFILE.LN_SUM_ORGTERM,
                     LNFILE.LN_UNITPER_CD, 
                     LNFILE.LN_UNITPER_FREQ
              FROM DBO.TRLFILE TRLFILE  
             INNER JOIN
                   DBO.LNFILE  LNFILE
                ON ( LNFILE.LN_OWNBR        = TRLFILE.TL_BRNO
               AND   LNFILE.LN_ACCTNO       = TRLFILE.TL_ACCTNO )
             WHERE   TRLFILE.TL_BRNO        = :QTRL1-WBEG-BRNO   
               AND   TRLFILE.TL_DATE  BETWEEN :QTRL1-WBEG-DATE
                                          AND :QTRL1-WEND-DATE  
               AND   TRLFILE.TL_CLASS BETWEEN :QTRL1-WBEG-CLASS
                                          AND :QTRL1-WEND-CLASS 
             ORDER BY TRLFILE.TL_BRNO,
                      TRLFILE.TL_DATE,
                      TRLFILE.TL_CLASS,
                      TRLFILE.TL_ACCTNO,
                      TRLFILE.TL_LAWCODE,
                      TRLFILE.TL_LINE
           END-EXEC.

           EXEC SQL
               OPEN TRL1_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE ZEROES TO QTRL1-WBEG-BRNO
                          QTRL1-WEND-BRNO
                          QTRL1-WBEG-CLASS
                          QTRL1-WEND-CLASS
                          QTRL1-WBEG-ACCTNO
                          QTRL1-WEND-ACCTNO
                          QTRL1-WBEG-LAWCODE
                          QTRL1-WEND-LAWCODE
                          QTRL1-WBEG-LINE
                          QTRL1-WEND-LINE.
           MOVE SPACES TO QTRL1-WBEG-DATE
                          QTRL1-WEND-DATE.


           MOVE STAT---GOOD TO TRL1-CURSOR-STAT.

      *-----------------------------------------------------------------
       READ-TRL1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE TRL-PATH-OWNBR  TO TL-BRNO.
           MOVE TRL-SELECT-PATH TO E-FILE.
           MOVE TRL1-KEY        TO E-KEYX.
           INITIALIZE QTRL-REC.
           INITIALIZE QLN-REC.

           IF ( TRL1-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH TRL1_CURSOR INTO  
                     :QTL-BRNO, 
                     :QTL-DATE, 
                     :QTL-CLASS, 
                     :QTL-ACCTNO, 
                     :QTL-LAWCODE, 
                     :QTL-LINE, 
                     :QTL-INSCOMP-1, 
                     :QTL-INSCOMP-2, 
                     :QTL-INSCOMP-3, 
                     :QTL-INSCOMP-4, 
                     :QTL-INSCOMP-5, 
                     :QTL-INSCOMP-6, 
                     :QTL-INSCOMP-7, 
                     :QTL-INSCOMP-8, 
                     :QTL-INSCOMP-9, 
                     :QTL-INSCOMP-10, 
                     :QTL-INSOURS-1, 
                     :QTL-INSOURS-2, 
                     :QTL-INSOURS-3, 
                     :QTL-INSOURS-4, 
                     :QTL-INSOURS-5, 
                     :QTL-INSOURS-6, 
                     :QTL-INSOURS-7, 
                     :QTL-INSOURS-8, 
                     :QTL-INSOURS-9, 
                     :QTL-INSOURS-10,  
                     :QTL-INSPREM-1, 
                     :QTL-INSPREM-2, 
                     :QTL-INSPREM-3, 
                     :QTL-INSPREM-4, 
                     :QTL-INSPREM-5, 
                     :QTL-INSPREM-6, 
                     :QTL-INSPREM-7, 
                     :QTL-INSPREM-8, 
                     :QTL-INSPREM-9, 
                     :QTL-INSPREM-10, 
                     :QTL-INSCOMM-1, 
                     :QTL-INSCOMM-2, 
                     :QTL-INSCOMM-3, 
                     :QTL-INSCOMM-4, 
                     :QTL-INSCOMM-5, 
                     :QTL-INSCOMM-6, 
                     :QTL-INSCOMM-7, 
                     :QTL-INSCOMM-8, 
                     :QTL-INSCOMM-9, 
                     :QTL-INSCOMM-10,
                     :QLN-OWNBR,
                     :QLN-ACCTNO,
                     :QLN-CLASS,
                     :QLN-SUBCLASS,
                     :QLN-LAWCODE,
                     :QLN-SSNO1,
                     :QLN-MAKERCD1,
                     :QLN-SSNO2,
                     :QLN-MAKERCD2,
                     :QLN-SSNO3,
                     :QLN-MAKERCD3,
                     :QLN-SSNO4,
                     :QLN-MAKERCD4,
                     :QLN-ORGST,
                     :QLN-SPRCLASS,
                     :QLN-STATFLAGS,
                     :QLN-ORIG-1STPYDATE,
                     :QLN-1STPYDATE,
                     :QLN-MAINTFEE,
                     :QLN-ORGTERM,
                     :QLN-REGPYAMT,
                     :QLN-INSURED,
                     :QLN-INSTYPES,
                     :QLN-INSCOMP-1,
                     :QLN-INSCOMP-2,
                     :QLN-INSCOMP-3,
                     :QLN-INSCOMP-4,
                     :QLN-INSCOMP-5,
                     :QLN-INSCOMP-6,
                     :QLN-INSCOMP-7,
                     :QLN-INSCOMP-8,
                     :QLN-INSCOMP-9,
                     :QLN-INSCOMP-10,
                     :QLN-INSEFF-DATE-1,
                     :QLN-INSEFF-DATE-2,
                     :QLN-INSEFF-DATE-3,
                     :QLN-INSEFF-DATE-4,
                     :QLN-INSEFF-DATE-5,
                     :QLN-INSEFF-DATE-6,
                     :QLN-INSEFF-DATE-7,
                     :QLN-INSEFF-DATE-8,
                     :QLN-INSEFF-DATE-9,
                     :QLN-INSEFF-DATE-10,
                     :QLN-INSEXP-DATE-1,
                     :QLN-INSEXP-DATE-2,
                     :QLN-INSEXP-DATE-3,
                     :QLN-INSEXP-DATE-4,
                     :QLN-INSEXP-DATE-5,
                     :QLN-INSEXP-DATE-6,
                     :QLN-INSEXP-DATE-7,
                     :QLN-INSEXP-DATE-8,
                     :QLN-INSEXP-DATE-9,
                     :QLN-INSEXP-DATE-10,
                     :QLN-INSCOVR-1,
                     :QLN-INSCOVR-2,
                     :QLN-INSCOVR-3,
                     :QLN-INSCOVR-4,
                     :QLN-INSCOVR-5,
                     :QLN-INSCOVR-6,
                     :QLN-INSCOVR-7,
                     :QLN-INSCOVR-8,
                     :QLN-INSCOVR-9,
                     :QLN-INSCOVR-10,
                     :QLN-TOTNODEF,
                     :QLN-CL-REBATE,
                     :QLN-AH-REBATE,
                     :QLN-PP-REBATE,
                     :QLN-INT-REBATE,
                     :QLN-SC-REBATE,
                     :QLN-MF-REBATE,
                     :QLN-DF-REBATE,
                     :QLN-EX-REBATE,
                     :QLN-O1-REBATE,
                     :QLN-O2-REBATE,
                     :QLN-O3-REBATE,
                     :QLN-O4-REBATE,
                     :QLN-O5-REBATE,
                     :QLN-O6-REBATE,
                     :QLN-O7-REBATE,
                     :QLN-UN-O7-REB,
                     :QLN-UN-O8-REB,
                     :QLN-UN-O9-REB,
                     :QLN-UN-O10-REB,
                     :QLN-SUM-ORGTERM,
                     :QLN-UNITPER-CD,
                     :QLN-UNITPER-FREQ
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-TRL1-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-TRL-FIELDS
              PERFORM GET-LN-FIELDS.

      *-----------------------------------------------------------------
       CLOSE-TRL1-FILE.

           PERFORM CLOSE-IT.
           IF ( TRL1-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE TRL1_CURSOR
              END-EXEC
              MOVE STAT---BAD TO TRL1-CURSOR-STAT.


      * COPY "LIBLP/LPTRS1RN.CPY".
      * COPYMEMBER: LIBLP/LPTR1RN
       LOAD-TRS1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( TRS-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( TRS-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO TRS-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO TRS-ALL-MACHINE
              MOVE EXT-FILPATH-DATA        TO TRS-ALL-DATA
              MOVE TRS-ALL-PATH             TO TRS-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( TRS-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO TRS-ALL-BASE
              MOVE FILPATH-MACHINE         TO TRS-ALL-MACHINE
              MOVE FILPATH-DATA            TO TRS-ALL-DATA
              MOVE TRS-ALL-PATH             TO TRS-PATH.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    CREATE PATH FOR FILE SELECT AND SQL TABLE NAME
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( EXT-IO-TRSFILE NOT = "S" )
              MOVE TRS-PATH-BEG             TO TRS-SELECT-PATH-BEG
              MOVE TRS-PATH-FILE            TO TRS-SELECT-PATH-FILE
           ELSE
              MOVE TRS-PATH-SQL             TO TRS-SELECT-PATH.

      *-----------------------------------------------------------------
       OPEN-TRS1-FILE.
           PERFORM LOAD-TRS1-FILE.
           PERFORM OPEN-IT.
           MOVE TRS-SELECT-PATH TO E-FILE.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       START-TRS1-FILE.
           PERFORM START-IT.
           MOVE TRS-PATH-OWNBR  TO TS-BRNO.
           MOVE TRS-SELECT-PATH TO E-FILE.
           MOVE TRS1-KEY        TO E-KEYX.

           IF ( TRS1-CURSOR-STAT-GOOD )
              PERFORM CLOSE-TRS1-FILE.

           IF ( QTRS1-WEND-LINE = ZEROES    ) OR
              ( QTRS1-WEND-LINE NOT NUMERIC )
              MOVE TRS-PATH-OWNBR        TO QTRS1-WBEG-BRNO
              MOVE TRS-PATH-OWNBR        TO QTRS1-WEND-BRNO
              MOVE TS-DATE               TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD  TO QTRS1-WBEG-DATE
              MOVE JULIAN-END           TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD  TO QTRS1-WEND-DATE
              MOVE TS-CLASS             TO QTRS1-WBEG-CLASS
              MOVE TS-ACCTNO            TO QTRS1-WBEG-ACCTNO
              MOVE TS-LAWCODE           TO QTRS1-WBEG-LAWCODE
              MOVE TS-LINE              TO QTRS1-WBEG-LINE
              MOVE ALL "9"              TO QTRS1-WEND-CLASS
                                           QTRS1-WEND-ACCTNO
                                           QTRS1-WEND-LAWCODE
                                           QTRS1-WEND-LINE.

           EXEC SQL		   
            DECLARE TRS1_CURSOR CURSOR FOR
              SELECT TRSFILE.TS_BRNO,
                     CAST(TRSFILE.TS_DATE       AS VARCHAR(10)),
                     TRSFILE.TS_CLASS,
                     TRSFILE.TS_ACCTNO,
                     TRSFILE.TS_LAWCODE,
                     TRSFILE.TS_LINE,
                     TRSFILE.TS_INSCOMP_1,
                     TRSFILE.TS_INSCOMP_2,
                     TRSFILE.TS_INSCOMP_3,
                     TRSFILE.TS_INSCOMP_4,
                     TRSFILE.TS_INSCOMP_5,
                     TRSFILE.TS_INSCOMP_6,
                     TRSFILE.TS_INSCOMP_7,
                     TRSFILE.TS_INSCOMP_8,
                     TRSFILE.TS_INSCOMP_9,
                     TRSFILE.TS_INSCOMP_10,
                     TRSFILE.TS_INSOURS_1, 
                     TRSFILE.TS_INSOURS_2, 
                     TRSFILE.TS_INSOURS_3, 
                     TRSFILE.TS_INSOURS_4, 
                     TRSFILE.TS_INSOURS_5, 
                     TRSFILE.TS_INSOURS_6, 
                     TRSFILE.TS_INSOURS_7, 
                     TRSFILE.TS_INSOURS_8, 
                     TRSFILE.TS_INSOURS_9, 
                     TRSFILE.TS_INSOURS_10, 
                     TRSFILE.TS_INSPREM_1,
                     TRSFILE.TS_INSPREM_2,
                     TRSFILE.TS_INSPREM_3,
                     TRSFILE.TS_INSPREM_4,
                     TRSFILE.TS_INSPREM_5,
                     TRSFILE.TS_INSPREM_6,
                     TRSFILE.TS_INSPREM_7,
                     TRSFILE.TS_INSPREM_8,
                     TRSFILE.TS_INSPREM_9,
                     TRSFILE.TS_INSPREM_10,
                     TRSFILE.TS_INSCOMM_1,
                     TRSFILE.TS_INSCOMM_2,
                     TRSFILE.TS_INSCOMM_3,
                     TRSFILE.TS_INSCOMM_4,
                     TRSFILE.TS_INSCOMM_5,
                     TRSFILE.TS_INSCOMM_6,
                     TRSFILE.TS_INSCOMM_7,
                     TRSFILE.TS_INSCOMM_8,
                     TRSFILE.TS_INSCOMM_9,
                     TRSFILE.TS_INSCOMM_10,
                     LNFILE.LN_OWNBR,
                     LNFILE.LN_ACCTNO,
                     LNFILE.LN_CLASS,
                     LNFILE.LN_SUBCLASS,
                     LNFILE.LN_LAWCODE,
                     LNFILE.LN_SSNO1,
                     LNFILE.LN_MAKERCD1,
                     LNFILE.LN_SSNO2,
                     LNFILE.LN_MAKERCD2,
                     LNFILE.LN_SSNO3,
                     LNFILE.LN_MAKERCD3, 
                     LNFILE.LN_SSNO4,
                     LNFILE.LN_MAKERCD4, 
                     LNFILE.LN_ORGST, 
                     LNFILE.LN_SPRCLASS,
                     LNFILE.LN_STATFLAGS, 
                     CAST(LNFILE.LN_ORIG_1STPYDATE AS VARCHAR(10)),
                     CAST(LNFILE.LN_1STPYDATE      AS VARCHAR(10)),
                     LNFILE.LN_MAINTFEE,
                     LNFILE.LN_ORGTERM,
                     LNFILE.LN_REGPYAMT,
                     ISNULL(LNFILE.LN_INSURED,''),
                     ISNULL(LNFILE.LN_INSTYPES,''),
                     LNFILE.LN_INSCOMP_1,
                     LNFILE.LN_INSCOMP_2,
                     LNFILE.LN_INSCOMP_3,
                     LNFILE.LN_INSCOMP_4,
                     LNFILE.LN_INSCOMP_5,
                     LNFILE.LN_INSCOMP_6,
                     LNFILE.LN_INSCOMP_7,
                     LNFILE.LN_INSCOMP_8,
                     LNFILE.LN_INSCOMP_9,
                     LNFILE.LN_INSCOMP_10,
                     CAST(LNFILE.LN_INSEFF_DATE_1  AS VARCHAR(10)),
                     CAST(LNFILE.LN_INSEFF_DATE_2  AS VARCHAR(10)),
                     CAST(LNFILE.LN_INSEFF_DATE_3  AS VARCHAR(10)),
                     CAST(LNFILE.LN_INSEFF_DATE_4  AS VARCHAR(10)),
                     CAST(LNFILE.LN_INSEFF_DATE_5  AS VARCHAR(10)),
                     CAST(LNFILE.LN_INSEFF_DATE_6  AS VARCHAR(10)),
                     CAST(LNFILE.LN_INSEFF_DATE_7  AS VARCHAR(10)),
                     CAST(LNFILE.LN_INSEFF_DATE_8  AS VARCHAR(10)),
                     CAST(LNFILE.LN_INSEFF_DATE_9  AS VARCHAR(10)),
                     CAST(LNFILE.LN_INSEFF_DATE_10 AS VARCHAR(10)),
                     CAST(LNFILE.LN_INSEXP_DATE_1  AS VARCHAR(10)),
                     CAST(LNFILE.LN_INSEXP_DATE_2  AS VARCHAR(10)),
                     CAST(LNFILE.LN_INSEXP_DATE_3  AS VARCHAR(10)),
                     CAST(LNFILE.LN_INSEXP_DATE_4  AS VARCHAR(10)),
                     CAST(LNFILE.LN_INSEXP_DATE_5  AS VARCHAR(10)),
                     CAST(LNFILE.LN_INSEXP_DATE_6  AS VARCHAR(10)),
                     CAST(LNFILE.LN_INSEXP_DATE_7  AS VARCHAR(10)),
                     CAST(LNFILE.LN_INSEXP_DATE_8  AS VARCHAR(10)),
                     CAST(LNFILE.LN_INSEXP_DATE_9  AS VARCHAR(10)),
                     CAST(LNFILE.LN_INSEXP_DATE_10 AS VARCHAR(10)),
                     LNFILE.LN_INSCOVR_1,
                     LNFILE.LN_INSCOVR_2,
                     LNFILE.LN_INSCOVR_3,
                     LNFILE.LN_INSCOVR_4,
                     LNFILE.LN_INSCOVR_5,
                     LNFILE.LN_INSCOVR_6,
                     LNFILE.LN_INSCOVR_7,
                     LNFILE.LN_INSCOVR_8,
                     LNFILE.LN_INSCOVR_9,
                     LNFILE.LN_INSCOVR_10,
                     LNFILE.LN_TOTNODEF,
                     LNFILE.LN_CL_REBATE, 
                     LNFILE.LN_AH_REBATE, 
                     LNFILE.LN_PP_REBATE, 
                     LNFILE.LN_INT_REBATE, 
                     LNFILE.LN_SC_REBATE, 
                     LNFILE.LN_MF_REBATE, 
                     LNFILE.LN_DF_REBATE, 
                     LNFILE.LN_EX_REBATE, 
                     LNFILE.LN_O1_REBATE, 
                     LNFILE.LN_O2_REBATE, 
                     LNFILE.LN_O3_REBATE, 
                     LNFILE.LN_O4_REBATE, 
                     LNFILE.LN_O5_REBATE, 
                     LNFILE.LN_O6_REBATE, 
                     LNFILE.LN_O7_REBATE, 
                     LNFILE.LN_UN_O7_REB, 
                     LNFILE.LN_UN_O8_REB, 
                     LNFILE.LN_UN_O9_REB, 
                     LNFILE.LN_UN_O10_REB, 
                     LNFILE.LN_SUM_ORGTERM,
                     LNFILE.LN_UNITPER_CD, 
                     LNFILE.LN_UNITPER_FREQ
              FROM DBO.TRSFILE TRSFILE 
             INNER JOIN
                   DBO.LNFILE  LNFILE
                ON ( LNFILE.LN_OWNBR        = TRSFILE.TS_BRNO
               AND   LNFILE.LN_ACCTNO       = TRSFILE.TS_ACCTNO )
             WHERE   TRSFILE.TS_BRNO        = :QTRS1-WBEG-BRNO   
               AND   TRSFILE.TS_DATE  BETWEEN :QTRS1-WBEG-DATE
                                          AND :QTRS1-WEND-DATE  
               AND   TRSFILE.TS_CLASS BETWEEN :QTRS1-WBEG-CLASS
                                          AND :QTRS1-WEND-CLASS
             ORDER BY TRSFILE.TS_BRNO,
                      TRSFILE.TS_DATE,
                      TRSFILE.TS_CLASS,
                      TRSFILE.TS_ACCTNO,
                      TRSFILE.TS_LAWCODE,
                      TRSFILE.TS_LINE			  
           END-EXEC.

           EXEC SQL
               OPEN TRS1_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE ZEROES TO QTRS1-WBEG-BRNO
                          QTRS1-WEND-BRNO
                          QTRS1-WBEG-CLASS
                          QTRS1-WEND-CLASS
                          QTRS1-WBEG-ACCTNO
                          QTRS1-WEND-ACCTNO
                          QTRS1-WBEG-LAWCODE
                          QTRS1-WEND-LAWCODE
                          QTRS1-WBEG-LINE
                          QTRS1-WEND-LINE.
           MOVE SPACES TO QTRS1-WBEG-DATE
                          QTRS1-WEND-DATE.


           MOVE STAT---GOOD TO TRS1-CURSOR-STAT.

      *-----------------------------------------------------------------
       READ-TRS1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE TRS-PATH-OWNBR  TO TS-BRNO.
           MOVE TRS-SELECT-PATH TO E-FILE.
           MOVE TRS1-KEY        TO E-KEYX.
           INITIALIZE QTRS-REC.
           INITIALIZE QLN-REC.

           IF ( TRS1-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH TRS1_CURSOR INTO  
                     :QTS-BRNO,
                     :QTS-DATE,
                     :QTS-CLASS,
                     :QTS-ACCTNO,
                     :QTS-LAWCODE,
                     :QTS-LINE,
                     :QTS-INSCOMP-1,
                     :QTS-INSCOMP-2,
                     :QTS-INSCOMP-3,
                     :QTS-INSCOMP-4,
                     :QTS-INSCOMP-5,
                     :QTS-INSCOMP-6,
                     :QTS-INSCOMP-7,
                     :QTS-INSCOMP-8,
                     :QTS-INSCOMP-9,
                     :QTS-INSCOMP-10,
                     :QTS-INSOURS-1, 
                     :QTS-INSOURS-2, 
                     :QTS-INSOURS-3, 
                     :QTS-INSOURS-4, 
                     :QTS-INSOURS-5, 
                     :QTS-INSOURS-6, 
                     :QTS-INSOURS-7, 
                     :QTS-INSOURS-8, 
                     :QTS-INSOURS-9, 
                     :QTS-INSOURS-10, 
                     :QTS-INSPREM-1,
                     :QTS-INSPREM-2,
                     :QTS-INSPREM-3,
                     :QTS-INSPREM-4,
                     :QTS-INSPREM-5,
                     :QTS-INSPREM-6,
                     :QTS-INSPREM-7,
                     :QTS-INSPREM-8,
                     :QTS-INSPREM-9,
                     :QTS-INSPREM-10,
                     :QTS-INSCOMM-1,
                     :QTS-INSCOMM-2,
                     :QTS-INSCOMM-3,
                     :QTS-INSCOMM-4,
                     :QTS-INSCOMM-5,
                     :QTS-INSCOMM-6,
                     :QTS-INSCOMM-7,
                     :QTS-INSCOMM-8,
                     :QTS-INSCOMM-9,
                     :QTS-INSCOMM-10,
                     :QLN-OWNBR,
                     :QLN-ACCTNO,
                     :QLN-CLASS,
                     :QLN-SUBCLASS,
                     :QLN-LAWCODE,
                     :QLN-SSNO1,
                     :QLN-MAKERCD1,
                     :QLN-SSNO2,
                     :QLN-MAKERCD2,
                     :QLN-SSNO3,
                     :QLN-MAKERCD3,
                     :QLN-SSNO4,
                     :QLN-MAKERCD4,
                     :QLN-ORGST,
                     :QLN-SPRCLASS,
                     :QLN-STATFLAGS,
                     :QLN-ORIG-1STPYDATE,
                     :QLN-1STPYDATE,
                     :QLN-MAINTFEE,
                     :QLN-ORGTERM,
                     :QLN-REGPYAMT,
                     :QLN-INSURED,
                     :QLN-INSTYPES,
                     :QLN-INSCOMP-1,
                     :QLN-INSCOMP-2,
                     :QLN-INSCOMP-3,
                     :QLN-INSCOMP-4,
                     :QLN-INSCOMP-5,
                     :QLN-INSCOMP-6,
                     :QLN-INSCOMP-7,
                     :QLN-INSCOMP-8,
                     :QLN-INSCOMP-9,
                     :QLN-INSCOMP-10,
                     :QLN-INSEFF-DATE-1,
                     :QLN-INSEFF-DATE-2,
                     :QLN-INSEFF-DATE-3,
                     :QLN-INSEFF-DATE-4,
                     :QLN-INSEFF-DATE-5,
                     :QLN-INSEFF-DATE-6,
                     :QLN-INSEFF-DATE-7,
                     :QLN-INSEFF-DATE-8,
                     :QLN-INSEFF-DATE-9,
                     :QLN-INSEFF-DATE-10,
                     :QLN-INSEXP-DATE-1,
                     :QLN-INSEXP-DATE-2,
                     :QLN-INSEXP-DATE-3,
                     :QLN-INSEXP-DATE-4,
                     :QLN-INSEXP-DATE-5,
                     :QLN-INSEXP-DATE-6,
                     :QLN-INSEXP-DATE-7,
                     :QLN-INSEXP-DATE-8,
                     :QLN-INSEXP-DATE-9,
                     :QLN-INSEXP-DATE-10,
                     :QLN-INSCOVR-1,
                     :QLN-INSCOVR-2,
                     :QLN-INSCOVR-3,
                     :QLN-INSCOVR-4,
                     :QLN-INSCOVR-5,
                     :QLN-INSCOVR-6,
                     :QLN-INSCOVR-7,
                     :QLN-INSCOVR-8,
                     :QLN-INSCOVR-9,
                     :QLN-INSCOVR-10,
                     :QLN-TOTNODEF,
                     :QLN-CL-REBATE,
                     :QLN-AH-REBATE,
                     :QLN-PP-REBATE,
                     :QLN-INT-REBATE,
                     :QLN-SC-REBATE,
                     :QLN-MF-REBATE,
                     :QLN-DF-REBATE,
                     :QLN-EX-REBATE,
                     :QLN-O1-REBATE,
                     :QLN-O2-REBATE,
                     :QLN-O3-REBATE,
                     :QLN-O4-REBATE,
                     :QLN-O5-REBATE,
                     :QLN-O6-REBATE,
                     :QLN-O7-REBATE,
                     :QLN-UN-O7-REB,
                     :QLN-UN-O8-REB,
                     :QLN-UN-O9-REB,
                     :QLN-UN-O10-REB,
                     :QLN-SUM-ORGTERM,
                     :QLN-UNITPER-CD,
                     :QLN-UNITPER-FREQ
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-TRS1-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-TRS-FIELDS
              PERFORM GET-LN-FIELDS.

      *-----------------------------------------------------------------
       CLOSE-TRS1-FILE.

           PERFORM CLOSE-IT.
           IF ( TRS1-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE TRS1_CURSOR
              END-EXEC
              MOVE STAT---BAD TO TRS1-CURSOR-STAT.

  
      * COPY "LIBLP/LPIC1RN.CPY".
      * COPYMEMBER: LIBLP/LPIC1RN
       LOAD-IC1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( IC-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( IC-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO IC-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO IC-ALL-MACHINE
              MOVE EXT-FILPATH-DATA        TO IC-ALL-DATA
              MOVE IC-ALL-PATH             TO IC-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( IC-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO IC-ALL-BASE
              MOVE FILPATH-MACHINE         TO IC-ALL-MACHINE
              MOVE FILPATH-DATA            TO IC-ALL-DATA
              MOVE IC-ALL-PATH             TO IC-PATH.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    CREATE PATH FOR FILE SELECT AND SQL TABLE NAME
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( EXT-IO-ICFILE NOT = "S" )
              MOVE IC-PATH-BEG             TO IC-SELECT-PATH-BEG
              MOVE IC-PATH-FILE            TO IC-SELECT-PATH-FILE
           ELSE
              MOVE IC-PATH-SQL             TO IC-SELECT-PATH.

      *-----------------------------------------------------------------
       OPEN-IC1-FILE.
           PERFORM LOAD-IC1-FILE.
           PERFORM OPEN-IT.
           MOVE IC-SELECT-PATH TO E-FILE.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       READ-IC1-FILE.
           PERFORM READ-IT.
           MOVE IC-PATH-OWNBR  TO IC-BRNO. 
           MOVE IC-SELECT-PATH TO E-FILE.
           MOVE IC1-KEY        TO E-KEYX.
           INITIALIZE QIC-REC.

           MOVE IC-BRNO   TO QIC-BRNO.
           MOVE IC-CODE   TO QIC-CODE.

           EXEC SQL
            SELECT ICFILE.IC_BRNO,
                   ICFILE.IC_CODE,
                   ISNULL(ICFILE.IC_NAME,'')
            INTO  
			       :QIC-BRNO,
                   :QIC-CODE,
                   :QIC-NAME
            FROM DBO.ICFILE
            WHERE ICFILE.IC_BRNO = :QIC-BRNO
              AND ICFILE.IC_CODE = :QIC-CODE
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-IC1-FILE.

           IF ( IO-FG = 0 )
              PERFORM GET-IC-FIELDS.

      *-----------------------------------------------------------------
       CLOSE-IC1-FILE.

           PERFORM CLOSE-IT.
 

      * COPY "LIBLP/LPBW1RN.CPY".
      * COPYMEMBER: LIBLP/LPBW1RN
       LOAD-BW1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( BW-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( BW-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO BW-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO BW-ALL-MACHINE
              MOVE EXT-FILPATH-DATA        TO BW-ALL-DATA
              MOVE BW-ALL-PATH             TO BW-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( BW-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO BW-ALL-BASE
              MOVE FILPATH-MACHINE         TO BW-ALL-MACHINE
              MOVE FILPATH-DATA            TO BW-ALL-DATA
              MOVE BW-ALL-PATH             TO BW-PATH.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    CREATE PATH FOR FILE SELECT AND SQL TABLE NAME
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( EXT-IO-BWFILE NOT = "S" )
              MOVE BW-PATH-BEG             TO BW-SELECT-PATH-BEG
              MOVE BW-PATH-FILE            TO BW-SELECT-PATH-FILE
           ELSE
              MOVE BW-PATH-SQL             TO BW-SELECT-PATH.

      *-----------------------------------------------------------------
       OPEN-BW1-FILE.
           PERFORM LOAD-BW1-FILE.
           PERFORM OPEN-IT.
           MOVE BW-SELECT-PATH TO E-FILE.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       READ-BW1-FILE.
           PERFORM READ-IT.
           MOVE BW-PATH-OWNBR  TO BW-OWNBR. 
           MOVE BW-SELECT-PATH TO E-FILE.
           MOVE BW1-KEY        TO E-KEYX.
           INITIALIZE QBW-REC.

           MOVE BW-OWNBR   TO QBW-OWNBR.
           MOVE BW-SSNO    TO QBW-SSNO.

           EXEC SQL
             SELECT BWFILE.BW_OWNBR,
                    BWFILE.BW_SSNO,
                    ISNULL(BWFILE.BW_LNAME,''),
                    ISNULL(BWFILE.BW_FNAME,''),
                    ISNULL(CAST(BWFILE.BW_REFDATE AS VARCHAR(10)),'0'),
                    ISNULL(CAST(BWFILE.BW_BDATE AS VARCHAR(10)),'0'),
                    ISNULL(BWFILE.BW_SOLICIT,'')
               INTO 
                    :QBW-OWNBR,
                    :QBW-SSNO,
                    :QBW-LNAME,
                    :QBW-FNAME,
                    :QBW-REFDATE,
                    :QBW-BDATE,
                    :QBW-SOLICIT
            FROM DBO.BWFILE
            WHERE BWFILE.BW_OWNBR = :QBW-OWNBR
              AND BWFILE.BW_SSNO = :QBW-SSNO
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-BW1-FILE.

           IF ( IO-FG = 0 )
              PERFORM GET-BW-FIELDS.

      *-----------------------------------------------------------------
       CLOSE-BW1-FILE.

           PERFORM CLOSE-IT.
 

       COPY "LIBLP/LPSP1RN.CPY".
       COPY "LIBGB/GBWK1IN.CPY".
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

           IF NO-RECORDS-FOUND
              MOVE "NO REPORT FOR SELECTED RANGE" TO MESS
              PERFORM SEND-MESS.

           MOVE MKTEMP-BUF TO ACCESS-BUF.
           PERFORM REMOVE-WORKFILE.
           MOVE WK1-PATH TO ACCESS-BUF.
           PERFORM REMOVE-WORKFILE.

      *-----------------------------------------------------------------
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
              MOVE "LP/EMICMU" TO FORM-NAM
              MOVE FORM-PATH TO FORM-PATHNAME.

       STOP-RUN.
           IF EXT-ROUTE-BUF = "00" OR "40" OR "50"
              DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN
              DESTROY ICMORP-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.
           EXIT PROGRAM.
