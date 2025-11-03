       IDENTIFICATION DIVISION.
       PROGRAM-ID.   EMLNRG.
       DATE-WRITTEN. 042084
      ******************************************************************
      *                      (LP)
      *    DESCRIPTION:  DIRECT LOAN REGISTER
      *
      * REV:
      *  KEC 091595 ADDED LOCAL-SPOOLING LOGIC.
      *  KEC 091595 CHANGE MILITARY TIME TO AM/PM ON HEADER.
      *  KEC 091595 ADDED CHECK FOR VALID BRANCH # FOR CLASS TOTALS.
      *  BAH 092895 RECORDS-FOUND INITIALIZED TO "N" INSTEAD OF " "
      *  JFF 100695 ADDED CLASS SCAN
      *  KEC 101295 COMMENTED OUT DEFAULT BRANCH SETTING IN INITIZATION.
      *  KEC 111595 MADE CHANGES FOR GEORGIA 4% / 8% SPLIT.
      *             REFLECTS WORLD A90 VERSION.
      *  KEC 113095 FIXED LOCAL-SPOOL FOR IF A BRANCH HAS NO DATA.
      *  JFF 121395 CHANGED "IF LOCAL-PRINT-FG = N" TO
      *             "IF LOCAL-PRINT-FG NOT = Y" IN INITIALIZATION.
      *  JFF 030496 ADDED "INCLUDE DETAIL" TO FORM & PARAMETER SECTION.
      *  JFF 031196 ADDED PRINT-RANGE-LINES ROUTINE.
      *  JFF 061296 ADDED GROUP TOTALS LOGIC.
      *  BLV 030497 ADD BORROWER CREDIT SCORE
      *  BLV 060597 CHANGED FOR LOCAL SPOOL WITH NEW BATCH DRIVER - BREXEC
      *  BAH 111097 OTHER 3 INSURANCE WAS NOT PRINTING, PL# 159
      *  BAH 010898 OTHER 3 INSURANCE WAS NOT TOTALING
      *  GLF 022098 DELETED BR-SECURITY-OK AND ADDED LIBGB/BRSECUREW.
      *  MJD 040198 CHANGED PRINTING OF GRAND TOTALS TO PRINT VERTICALLY
      *             INSTEAD OF HORIZONTALLY.
      *  BLV 041798 WORLD - PRINT TILL # UNDER PROCEEDS ON 3RD DETAIL
      *             LINE IF PROCASH NOT = 0
      *  BAH 062598 TL-TILLNO IS NOW ALPHA
      *  JTG 101398 ADDED SET OF LCSCAN-REVOLVING = 'N', REGENCY REVOL
      *  GLF 081899 ADD ARCHIVE PATH INFO. FOR TR FILES NOT FOUND IN DATA
      *             SET FOR SELECTED MONTH AND YEAR
      *  BLV 990826 FIX ARCHIVE PATH LOGIC
      *  MJD 000124 FIXED BUG (I THINK) COLLATERAL WAS PRINTING TWICE
      *             FOR EACH ACCOUNT, ONCE AS THE SECOND LINE OF DETAIL
      *             AND ALSO AS THE LAST LINE OF DETAIL (PER ACCOUNT)
      *             DO NOT LET ACCOUNTS SPAN PAGES.
      *             INSTEAD OF PRINTING EACH DETAIL LINE AS IT IS BUILT,
      *             I MOVE IT TO A HOLD BUFFER.  AT THE END OF THE
      *             TR-DETAIL1 ROUTINE I COUNT THE LINES IN THE BUFFER
      *             TO DETERMINE IF THE ACCOUNT WILL FIT ON THE CURRENT
      *             PAGE OF IF IT SHOULD BEGIN THE FOLLOWING PAGE.
      *             IF ANY NEW DETAIL LINE PRINTS ARE ADDED THEN THE
      *             TABLE WILL HAVE TO BE EXPANDED ACCORDINGLY.
      *  MJD 000524 CHANGED THE WAY TOTALS PRINT. INSTEAD OF PRINTING
      *             WITHIN ROWS WITH HEADERS ON THE TOP, THEY NOW PRINT
      *             IN 5 COLUMNS WITH THE HEADER PRINTED NEXT TO THE NUMBER
      *             THIS WAS DONE BECAUSE NUMBERS WERE GETTING TRUNCATED AND
      *             IT WAS EASIER TO READ THEN TO ADD A FOURTH LINE OF
      *             TOTALS AND HEADERS.  (SUPPORT PL#214)
      *  CS  010219 IF GP-I-AM-WORLD BREAK OUT THE NONFILE FEES (FEECODE 90)
      *             SEPARATELY IN THE GRAND TOTALS, WORLD PR# 253
      * KEC 010605 {PR#01504} PARADATA FINANCIAL [SUPPORT]
      *            ADDED "REN PRIN:" (RENEWED PRINCIPAL) TO THE REPORT
      *            (DETAIL & SUMMARY TOTALS).
      *  BLV 011203 PRINT POSTING TIME & USERID UNDER PRIOR LN#, WORLD PR# 285
      *  BAH 021008 FIXED RECORDS-FOUND TO NOT GIVE "NO RECORDS FOUND"
      *             ON A F7 EXIT
      *  BAH 070703 DO NOT PRINT SSNO FOR WORLD, WORLD PR# 536
      *  BAH 090304 CLEAR OUT BW-REC IF RECORD NOT FOUND AND 0 OUT THE
      *             NUMERIC FIELDS, FINMGM PL# 650
      *  BAH 090114 TEST GP-HIDE-FIRST-5-SSNO, REGMGM PR# 3785
      *  BAH 130801 ADDED PRINT OF TL-POINTS, WORLD PR# 898
      *  BAH 160118 SPLIT OUT LTFILE, PD0003
      *  KEC 2016.0315 {PD#00003} PARADATA <A30> [JAY CISZEWSKI]
      *         CHANGED CD-FEE???? TO CD-GC-????.
      *  BLM 160323 ADDED CREDIT SCORE TO REPORT, CREDIT SCORE RANGE, &
      *             OPTION TO EXTRACT TAB-DELIMITED FILE, WORLD PR# 1145
      *  BAH 2016.1103 CHANGE TILLNO TO 9(2)
      *  BAH 170530 ACTIVATE 8 DIGIT DATES, PD0006
      *  BAH 180219 ADDED PRINT OF LTD-DEBIT-CARD-NUMBER, WORLD PR#1306
      *  BAH 181207 GLOBAL LNFILE
      *  BAH 20190625 REMOVED LCSCAN-REVOLVING, PT#0153
      *  BAH 20190920 REMOVED ACCESS-CALL ON LNFILE TRLFILE
      *  RMZ 20230908 PERFORMANCE TUNING - ADDED STORED PROCEDURE
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "LIBGB/GBFSPR.CPY".
       COPY "LIBGB/GBFSWKI.CPY".
      * SORT IS NOW IN STORED PROCEDURE
      *    SELECT WK2-FILE ASSIGN WK2-PATH
      *           ORGANIZATION INDEXED
      *           ACCESS DYNAMIC
      *           LOCK MODE AUTOMATIC WITH LOCK ON RECORD
      *           RECORD KEY WK2-KEY
      *           FILE STATUS FILE-STAT.
          SELECT EMLNRG-FILE ASSIGN OUTPUT EMLNRG-PATH
              ORGANIZATION LINE SEQUENTIAL
              LOCK MODE AUTOMATIC WITH LOCK ON RECORD
              FILE STATUS FILE-STAT.
       I-O-CONTROL.
       DATA DIVISION.
       FILE SECTION.
       COPY "LIBGB/GBFDPR.CPY".

      ******************************************************************
      *
      *  SUMMARY TOTALS WORK RECORD
      *
      ******************************************************************
       FD  WK-FILE
           LABEL RECORDS ARE STANDARD.
       01  WK-REC.
           03  WK-KEY.
               05  WK-BR    PIC 9(4).
               05  WK-CLASS PIC 9(3).
               05  WK-CODE  PIC X(3).
           03  WK-NOLOANS         PIC S9(7)     COMP-3.
           03  WK-LNAMT           PIC S9(9)V99  COMP-3.
           03  WK-INTCHG          PIC S9(9)V99  COMP-3.
           03  WK-SERCHG          PIC S9(7)V99  COMP-3.
           03  WK-EXTCHG          PIC S9(7)V99  COMP-3.
           03  WK-MFEE            PIC S9(7)V99  COMP-3.
           03  WK-POINTS          PIC S9(7)V99  COMP-3.
           03  WK-STATE           PIC S9(7)V99  COMP-3.
           03  WK-EARNINGS        PIC S9(9)V99  COMP-3.
           03  WK-DL              PIC S9(7)V99  COMP-3.
           03  WK-LL              PIC S9(7)V99  COMP-3.
           03  WK-AH              PIC S9(7)V99  COMP-3.
           03  WK-PP              PIC S9(7)V99  COMP-3.
           03  WK-O1              PIC S9(7)V99  COMP-3.
           03  WK-O2              PIC S9(7)V99  COMP-3.
           03  WK-O3              PIC S9(7)V99  COMP-3.
           03  WK-FEES            PIC S9(7)V99  COMP-3.
           03  WK-WRLD-NONFILE-90 PIC S9(7)V99  COMP-3.
           03  WK-PROCEEDS        PIC S9(7)V99  COMP-3.
           03  WK-PROCASH         PIC S9(7)V99  COMP-3.
           03  WK-RENEW-PRIN      PIC S9(7)V99  COMP-3.

      ******************************************************************
      * 2024 NOW IN STORED PROCEDURE
      *
      * SORTED LNFILE BY BRANCH, CLASS, ACCOUNT
      *
      ******************************************************************
      *FD  WK2-FILE
      *    LABEL RECORDS ARE STANDARD.
      *01  WK2-REC.
      *    03  WK2-KEY.
      *        05  WK2-BRANCH           PIC 9(4).
      *        05  WK2-CLASS            PIC 9(3).
      *        05  WK2-ACCTNO           PIC 9(8).
      *        05  WK2-DATE             PIC 9(8).
      *        05  WK2-LAWCODE          PIC 99.
      *        05  WK2-LINE             PIC 9(3).

      *****************************************************************
      *    EMLNRG EXTRACT FILE
      *****************************************************************
       FD  EMLNRG-FILE
           RECORD IS VARYING IN SIZE FROM 10 TO 500 CHARACTERS
           LABEL RECORDS ARE STANDARD.
       01  EMLNRG-REC.
           03  EMLNRG-BYTE             PIC X OCCURS 500 TIMES.

      ******************************************************************
      ******************************************************************
      *
      *    W O R K I N G - S T O R A G E    S E C T I O N
      *
      ******************************************************************
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)LP/EMLNRG.CBL S35 02/20/24-13:18:29 >".

      ******************************************************************
      *
      *     F I L E   I F N
      *
      ******************************************************************
       COPY "LIBLP/LP01TRL.CPY".
       COPY "LIBLP/LP01TRL_SQL.CPY".
       COPY "LIBLP/LPWSTRL.CPY".
       COPY "LIBLP/LP01CDB.CPY".
       COPY "LIBLP/LP01CDB_SQL.CPY".
       COPY "LIBLP/LPWSCDB.CPY".
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
       COPY "LIBGB/GB01GR.CPY".
       COPY "LIBGB/GB01GR_SQL.CPY".
       COPY "LIBGB/GBWSGR.CPY".
       COPY "LIBLP/LP01LTC.CPY".
       COPY "LIBLP/LP01LTC_SQL.CPY".
       COPY "LIBLP/LPWSLTC.CPY".
       COPY "LIBLP/LP01LTD.CPY".
       COPY "LIBLP/LP01LTD_SQL.CPY".
       COPY "LIBLP/LPWSLTD.CPY".
      *-----------------------------------------------------------------
       EXEC SQL BEGIN DECLARE SECTION END-EXEC.
       01  QWS-BW-OWNBR1        PIC 9(4)     VALUE 0.      
       01  QWS-BW-SSNO1         PIC 9(9)     VALUE 0.
       01  QWS-BW-LNAME1        PIC X(35)    VALUE SPACES.
       01  QWS-BW-FNAME1        PIC X(35)    VALUE SPACES.
       01  QWS-BW-ADR11         PIC X(30)    VALUE SPACES.
       01  QWS-BW-ADR21         PIC X(30)    VALUE SPACES.
       01  QWS-BW-CITY1         PIC X(30)    VALUE SPACES.
       01  QWS-BW-STATE1        PIC X(2)     VALUE SPACES.
       01  QWS-BW-ZIP1          PIC X(10)    VALUE SPACES.
       01  QWS-BW-PHONE1        PIC 9(10)    VALUE 0.
       01  QWS-BW-EMPPHONE1     PIC 9(10)    VALUE 0.
       01  QWS-BW-EMPEXT1       PIC 9(4)     VALUE 0.
       01  QWS-BW-CREDIT-SCORE1 PIC 9(4)     VALUE 0.
       01  QWS-BW-OWNBR2        PIC 9(4)     VALUE 0.
       01  QWS-BW-SSNO2         PIC 9(9)     VALUE 0.
       01  QWS-BW-LNAME2        PIC X(35)    VALUE SPACES.
       01  QWS-BW-FNAME2        PIC X(35)    VALUE SPACES.
       01  QWS-BW-ADR12         PIC X(30)    VALUE SPACES.
       01  QWS-BW-ADR22         PIC X(30)    VALUE SPACES.
       01  QWS-BW-CITY2         PIC X(30)    VALUE SPACES.
       01  QWS-BW-STATE2        PIC X(2)     VALUE SPACES.
       01  QWS-BW-ZIP2          PIC X(10)    VALUE SPACES.
       01  QWS-BW-PHONE2        PIC 9(10)    VALUE 0.
       01  QWS-BW-EMPPHONE2     PIC 9(10)    VALUE 0.
       01  QWS-BW-EMPEXT2       PIC 9(4)     VALUE 0.
       01  QWS-BW-CREDIT-SCORE2 PIC 9(4)     VALUE 0.
       01  QWS-CDB-TYPE1        PIC X(2)     VALUE SPACES.
       01  QWS-CDB-BRANCH1      PIC 9(4)     VALUE 0.
       01  QWS-CDB-CODE1        PIC X(8)     VALUE SPACES.
       01  QWS-CDB-DESC1        PIC X(35)    VALUE SPACES.
       01  QWS-CDB-GC-GLFEENO1  PIC 9(7)     VALUE 0.
       01  QWS-CDB-TYPE2        PIC X(2)     VALUE SPACES.
       01  QWS-CDB-BRANCH2      PIC 9(4)     VALUE 0.
       01  QWS-CDB-CODE2        PIC X(8)     VALUE SPACES.
       01  QWS-CDB-DESC2        PIC X(35)    VALUE SPACES.
       01  QWS-CDB-GC-GLFEENO2  PIC 9(7)     VALUE 0.
       01  QWS-CDB-TYPE3        PIC X(2)     VALUE SPACES.
       01  QWS-CDB-BRANCH3      PIC 9(4)     VALUE 0.
       01  QWS-CDB-CODE3        PIC X(8)     VALUE SPACES.
       01  QWS-CDB-DESC3        PIC X(35)    VALUE SPACES.
       01  QWS-CDB-GC-GLFEENO3  PIC 9(7)     VALUE 0.
       01  QWS-CDB-TYPE4        PIC X(2)     VALUE SPACES.
       01  QWS-CDB-BRANCH4      PIC 9(4)     VALUE 0.
       01  QWS-CDB-CODE4        PIC X(8)     VALUE SPACES.
       01  QWS-CDB-DESC4        PIC X(35)    VALUE SPACES.
       01  QWS-CDB-GC-GLFEENO4  PIC 9(7)     VALUE 0.
       01  QWS-CDB-TYPE5        PIC X(2)     VALUE SPACES.
       01  QWS-CDB-BRANCH5      PIC 9(4)     VALUE 0.
       01  QWS-CDB-CODE5        PIC X(8)     VALUE SPACES.
       01  QWS-CDB-DESC5        PIC X(35)    VALUE SPACES.
       01  QWS-CDB-GC-GLFEENO5  PIC 9(7)     VALUE 0.
       01  QWS-CDB-TYPE6        PIC X(2)     VALUE SPACES.
       01  QWS-CDB-BRANCH6      PIC 9(4)     VALUE 0.
       01  QWS-CDB-CODE6        PIC X(8)     VALUE SPACES.
       01  QWS-CDB-DESC6        PIC X(35)    VALUE SPACES.
       01  QWS-CDB-GC-GLFEENO6  PIC 9(7)     VALUE 0.
       01  QWS-CDB-TYPE7        PIC X(2)     VALUE SPACES.
       01  QWS-CDB-BRANCH7      PIC 9(4)     VALUE 0.
       01  QWS-CDB-CODE7        PIC X(8)     VALUE SPACES.
       01  QWS-CDB-DESC7        PIC X(35)    VALUE SPACES.
       01  QWS-CDB-GC-GLFEENO7  PIC 9(7)     VALUE 0.
       01  QWS-CDB-TYPE8        PIC X(2)     VALUE SPACES.
       01  QWS-CDB-BRANCH8      PIC 9(4)     VALUE 0.
       01  QWS-CDB-CODE8        PIC X(8)     VALUE SPACES.
       01  QWS-CDB-DESC8        PIC X(35)    VALUE SPACES.
       01  QWS-CDB-GC-GLFEENO8  PIC 9(7)     VALUE 0.
       01  QWS-CDB-TYPE9        PIC X(2)     VALUE SPACES.
       01  QWS-CDB-BRANCH9      PIC 9(4)     VALUE 0.
       01  QWS-CDB-CODE9        PIC X(8)     VALUE SPACES.
       01  QWS-CDB-DESC9        PIC X(35)    VALUE SPACES.
       01  QWS-CDB-GC-GLFEENO9  PIC 9(7)     VALUE 0.
       01  QWS-CDB-TYPE10       PIC X(2)     VALUE SPACES.
       01  QWS-CDB-BRANCH10     PIC 9(4)     VALUE 0.
       01  QWS-CDB-CODE10       PIC X(8)     VALUE SPACES.
       01  QWS-CDB-DESC10       PIC X(35)    VALUE SPACES.
       01  QWS-CDB-GC-GLFEENO10 PIC 9(7)     VALUE 0.
       EXEC SQL END DECLARE SECTION END-EXEC.

       01  TRL1-CONN-OPEN-SW    PIC X        VALUE "N".


       01  PR-IFN               PIC X(2)     VALUE "PR".
       01  WK-IFN               PIC X(2)     VALUE "WK".
      *01  WK2-IFN              PIC X(3)     VALUE "WK2".

      *01  WK2-PATH             PIC X(35).
       01  EMLNRG-PATH.
               03  EMLNRG-PATH1.
               05  EMLNRG-PATH1A PIC X(12) VALUE "/USR/EXTRACT".
               05  FILLER        PIC X(5) VALUE "/ACT/".
           03  EMLNRG-PATH2      PIC X(6) VALUE "EMLNRG".

      ******************************************************************
      *
      *   LINE WORKERS
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
      *   MISC WORKERS
      *
      ******************************************************************
       01  MSEL-LIST            PIC X(5)      VALUE "MSEL.".
       01  SUB                  PIC 9(2)  COMP-3.
       01  I-SUB                PIC 9(6)  COMP-3.
       01  LEV                  PIC 9(2)  COMP-3.

       01  HOLD-BRNO            PIC 9(4)  VALUE 0.
       01  HOLD-CLASS           PIC 9(3)  VALUE 0.
       01  HOLD-STATE           PIC X(2)  VALUE SPACES.

       01  RECORDS-FOUND        PIC X     VALUE " ".
           88  NO-RECORDS-FOUND           VALUE "N".
       01  C-S-Z           PIC X(66).
       01  STATE           PIC XX.
       01  ZIP             PIC X(10).

       01  SAVE-WK-KEY.
           03  SAVE-WK-BR       PIC 9(4).
           03  SAVE-WK-CL       PIC 9(3).
           03  SAVE-WK-CODE     PIC XXX.

       01  PN-PHONE             PIC 9(10).
       01  PH-PHONEX REDEFINES PN-PHONE.
           03  PN-123           PIC 9(3).
           03  PN-456           PIC 9(3).
           03  PN-7890          PIC 9(4).
       01  PA-PHONE.
           03  FILLER           PIC X VALUE "(".
           03  PA-123           PIC 9(3).
           03  FILLER           PIC XX VALUE ") ".
           03  PA-456           PIC 9(3).
           03  FILLER           PIC X VALUE "-".
           03  PA-7890          PIC 9(4).

       01  SUMMARY-FG           PIC X     VALUE "N".

       01  FIRST-BORROWER-SW    PIC X        VALUE "N".
       01  FIRST-TIME           PIC X        VALUE "Y".
       01  MESS-LIST            PIC X(5)     VALUE "MESS.".
       01  CLASS-FG             PIC X(3)     VALUE "   ".
           88  CLASS-BEG                     VALUE "BEG".
           88  CLASS-END                     VALUE "END".
       01  FEE-SUB              PIC 9(2)     COMP-3.
      *-----------------------------------------------------------------
       01  MESSAGE-01-LOC-PRT   PIC X(43)    VALUE
           "CANNOT DO LOCAL SPOOLING WITH GROUP TOTALS".
       01  EXTRACT-FIELDS.
           03  EXT-BRNO         PIC 9(4).
           03  EXT-NUMBER       PIC 9(6).
           03  EXT-LOANDATE     PIC 9999/99/99.
           03  EXT-DLNO         PIC ----9.99.
           03  EXT-APR          PIC ---9.999.
           03  EXT-SRCD         PIC XX.
           03  EXT-COLLCD       PIC XX.
           03  EXT-TERM         PIC --9.
           03  EXT-LNAMT        PIC -------9.99.
           03  EXT-SERCHG       PIC -----9.99.
           03  EXT-FINCHG       PIC -------9.99.
           03  EXT-LIFE         PIC -----9.99.
           03  EXT-AH           PIC -----9.99.
           03  EXT-PP           PIC -----9.99.
           03  EXT-O1           PIC -----9.99.
           03  EXT-O2           PIC -----9.99.
           03  EXT-O3           PIC -----9.99.
           03  EXT-PRLNNO       PIC 9(6).
           03  EXT-PRBAL        PIC -------9.99.
           03  EXT-INT-REBATE   PIC -----9.99.
           03  EXT-FEECODE1     PIC XX.
           03  EXT-FEECODE2     PIC XX.
           03  EXT-FEECODE3     PIC XX.
           03  EXT-PROCEEDS     PIC -------9.99.
           03  EXT-CRSCORE      PIC ---9.

      ******************************************************************
      *
      *    W S - G T T - T A B L E
      *
      ******************************************************************
       01  WS-GTT-TOTAL-TABLE.
           03  WS-GTT-TABLE-GROUP-LEVEL                 OCCURS 10 TIMES.
               05  WS-GTT-NOLOANS         PIC S9(7)     COMP.
               05  WS-GTT-LNAMT           PIC S9(9)V99  COMP.
               05  WS-GTT-INTCHG          PIC S9(9)V99  COMP.
               05  WS-GTT-SERCHG          PIC S9(9)V99  COMP.
               05  WS-GTT-EXTCHG          PIC S9(7)V99  COMP.
               05  WS-GTT-MFEE            PIC S9(7)V99  COMP.
               05  WS-GTT-POINTS          PIC S9(7)V99  COMP.
               05  WS-GTT-STATE           PIC S9(7)V99  COMP.
               05  WS-GTT-EARNINGS        PIC S9(9)V99  COMP.
               05  WS-GTT-DL              PIC S9(7)V99  COMP.
               05  WS-GTT-LL              PIC S9(7)V99  COMP.
               05  WS-GTT-AH              PIC S9(7)V99  COMP.
               05  WS-GTT-PP              PIC S9(7)V99  COMP.
               05  WS-GTT-O1              PIC S9(7)V99  COMP.
               05  WS-GTT-O2              PIC S9(7)V99  COMP.
               05  WS-GTT-O3              PIC S9(7)V99  COMP.
               05  WS-GTT-FEES            PIC S9(7)V99  COMP.
               05  WS-GTT-WRLD-NONFILE-90 PIC S9(7)V99  COMP.
               05  WS-GTT-PROCEEDS        PIC S9(9)V99  COMP.
               05  WS-GTT-PROCASH         PIC S9(9)V99  COMP.
               05  WS-GTT-RENEW-PRIN      PIC S9(9)V99  COMP.

      *-----------------------------------------------------------------
       01  WS-HOLD-PROMPT       PIC X(7)  VALUE SPACES.

      ******************************************************************
      *  TOTALS LEV1 = CLASS TOTALS
      *            2 = BRANCH TOTALS
      *            3 = GRAND TOTALS
      ******************************************************************
       01  TOTALS-TAB.
           03  TOTAL-TBL       OCCURS 3.
               05  T-NOLOANS         PIC S9(7)     COMP-3.
               05  T-LNAMT           PIC S9(9)V99  COMP-3.
               05  T-INTCHG          PIC S9(9)V99  COMP-3.
               05  T-SERCHG          PIC S9(9)V99  COMP-3.
               05  T-EXTCHG          PIC S9(7)V99  COMP-3.
               05  T-MFEE            PIC S9(7)V99  COMP-3.
               05  T-POINTS          PIC S9(7)V99  COMP-3.
               05  T-STATE           PIC S9(7)V99  COMP-3.
               05  T-EARNINGS        PIC S9(9)V99  COMP-3.
               05  T-DL              PIC S9(7)V99  COMP-3.
               05  T-LL              PIC S9(7)V99  COMP-3.
               05  T-AH              PIC S9(7)V99  COMP-3.
               05  T-PP              PIC S9(7)V99  COMP-3.
               05  T-O1              PIC S9(7)V99  COMP-3.
               05  T-O2              PIC S9(7)V99  COMP-3.
               05  T-O3              PIC S9(7)V99  COMP-3.
               05  T-FEES            PIC S9(7)V99  COMP-3.
               05  T-WRLD-NONFILE-90 PIC S9(7)V99 COMP-3.
               05  T-PROCEEDS        PIC S9(9)V99  COMP-3.
               05  T-PROCASH         PIC S9(9)V99  COMP-3.
               05  T-RENEW-PRIN      PIC S9(9)V99  COMP-3.

       01  TOT-EXTRACTED             PIC S9(7)     COMP-3 VALUE 0.


       01  LEGEND               PIC X(70).

      ******************************************************************
      *
      *     S C R E E N   F I E L D S
      *
      ******************************************************************
       COPY "LIBGB/EOBUF_EXT.CPY".
      *-----------------------------------------------------------------
           03  BEG-BRANCH      PIC 9(4).
           03  END-BRANCH      PIC 9(4).
           03  BEG-CLASS       PIC 9(3).
           03  END-CLASS       PIC 9(3).
           03  END-DATE.
               05  END-MONTH   PIC 99.
               05  END-YEAR    PIC 99.
           03  EDATE REDEFINES END-DATE PIC 9999.
           03  DAY-OF-MONTH    PIC 99.
           03  GTT-ISO         PIC X(1).
           03  INCL-DETAIL     PIC X.
           03  GROUP-FLAG      PIC X.
           03  ENT-GROUP       PIC X(4).
           03  LOCAL-PRINT-FG  PIC X.
           03  TOTAL-LEV-TABLE.
               05 TOTAL-LEVEL-TABLE PIC X(1)  OCCURS 10.
           03  PRCOL-FG        PIC X.
           03  BEG-SCORE       PIC 9(3).
           03  END-SCORE       PIC 9(3).
           03  EXTRACT-FG      PIC X.


       01  VERSION-CONTROL     PIC X VALUE "E".
       01  BEGINNING-DATE       PIC 9(8).
       01  ENDING-DATE          PIC 9(8).

       01  SV-BEG-BR           PIC 9999  VALUE 9999.
       01  SV-END-BR           PIC 9999  VALUE 9999.
       01  SV-GROUP-FLAG       PIC X     VALUE SPACES.
       01  SV-ENT-GROUP        PIC X(4)  VALUE SPACES.
       01  SV-LOCAL-PRINT-FG   PIC X     VALUE SPACES.

      ******************************************************************
      *
      *     P R I N T   B U F F E R S
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
           03  FILLER           PIC X(31)    VALUE " ".
           03  FILLER           PIC X(38) VALUE
               "     DIRECT LOAN REGISTER".

      *-----------------------------------------------------------------
       01  HEAD3.
           03  FILLER           PIC X(10) VALUE "BRANCH :".
           03  H-BRNO           PIC 9(4).
           03  FILLER           PIC X VALUE " ".
           03  H-BRDESC         PIC X(20).
           03  FILLER           PIC X(46) VALUE " ".

      *-----------------------------------------------------------------
       01  HEAD4.
           03  FILLER           PIC X(10) VALUE "CLASS  :".
           03  H-LCNO           PIC 99.
           03  FILLER           PIC X VALUE " ".
           03  H-LCDESC         PIC X(20).

      *-----------------------------------------------------------------
       01  HEAD5.
           03  FILLER           PIC X(39)  VALUE
           "ACCOUNT NO.  LOANDATE  DEALER  APR  SC".
           03  FILLER           PIC X(40)  VALUE
           "CC  TRM  LOAN-AMT   FIN-CHG   INSURANCES".
           03  FILLER           PIC X(53)  VALUE
           " PRIOR-# PRIOR-BAL INT-REB ----- FEES ----- PROCEEDS ".

      *-----------------------------------------------------------------
       01  HEAD6.
           03  FILLER           PIC X(35)  VALUE SPACES.
           03  H-CRED-SCORE     PIC X(3)   VALUE SPACES.
           03  FILLER           PIC X(12)  VALUE
           " PC SCORE   ".
           03  FILLER           PIC X(58)  VALUE
           "SERCHG  EARNINGS              POSTED  INSUR-REB INT-DUE".
           03  FILLER           PIC X(24)  VALUE
           "GL-NO  AMOUNT  PRO-CASH".

      *-----------------------------------------------------------------
       01  HEAD6A.
           03  FILLER           PIC X(35)  VALUE SPACES.
           03  H-CRED-SCORE2    PIC X(3)   VALUE SPACES.
           03  FILLER           PIC X(20)  VALUE
           " PC SCORE  CLOSING  ".
           03  FILLER           PIC X(50)  VALUE
           "EARNINGS              POSTED  INSUR-REB INT-DUE   ".
           03  FILLER           PIC X(24)  VALUE
           "GL-NO  AMOUNT  PRO-CASH".

      *-----------------------------------------------------------------
       01  HEAD7.
           03  FILLER            PIC X(48) VALUE " ".
           03  FILLER            PIC X(18) VALUE "  SERCHG  8%SERCHG".

      *-----------------------------------------------------------------
       01  GTT-HEAD1.
           03  FILLER            PIC X(16)  VALUE
               "              GR".
           03  FILLER            PIC X(41)  VALUE SPACES.
           03  FILLER            PIC X(18)  VALUE "GROUP TOTALS TABLE".
           03  FILLER            PIC X(57)  VALUE SPACES.

      *-----------------------------------------------------------------
       01  GTT-HEAD2.
           03  FILLER            PIC X(17)  VALUE
               "BRANCH/GROUP  LEV".
           03  FILLER            PIC X(115) VALUE SPACES.

      ******************************************************************
      *
      *    D E T A I L - L I N E
      *
      ******************************************************************
      *----------------------------------------------------------------
      * DETAIL LINE TABLE - HOLDS DETAIL LINES UNTIL END OF DETAIL
      *  ROUTINE AND THEN PRINTS THEM ON THE CURRENT OR NEXT PAGE
      *  DEPENDING ON SPACE. IT PREVENTS AN ACCOUNT FROM SPANNING A PAGE
      *----------------------------------------------------------------
       01  DETAIL-LINE-TBL.
           03  DTL-LINE         PIC X(132) OCCURS 12.
       01  DTL-SUB              PIC 9(02).

       01  DETAIL-LINE          PIC X(132)  VALUE SPACES.

      *-----------------------------------------------------------------
       01  DETAIL1 REDEFINES DETAIL-LINE.
           03  D-CLASS          PIC 99.
           03  D-H1             PIC X.
           03  D-NUMBER         PIC 9(6).
           03  D-H2             PIC X.
           03  D-SPCLASS        PIC 99.
           03  FILLER           PIC X.
           03  D-LNDTE          PIC 99/99/99  BLANK ZERO.
           03  FILLER           PIC X.
           03  D-DLNO           PIC Z(4).99  BLANK ZERO.
           03  FILLER           PIC X.
           03  D-APR            PIC Z9.99    BLANK ZERO.
           03  FILLER           PIC X.
           03  D-SRCD           PIC XX.
           03  FILLER           PIC X.
           03  D-COLCD          PIC XXX.
           03  FILLER           PIC X.
           03  D-TERM           PIC ZZ9        BLANK ZERO.
           03  FILLER           PIC X.
           03  D-LNAMT          PIC Z(5)9.99-  BLANK ZERO.
           03  D-FINCHG         PIC Z(5)9.99-  BLANK ZERO.
           03  D-INSPREM        PIC Z(4)9.99-  BLANK ZERO.
           03  D-INSFLAG        PIC X.
           03  D-INSTYPE        PIC X.
           03  D-INSOURS        PIC X.
           03  FILLER           PIC X.
           03  D-USERID.
               05  D-PRLNNO     PIC 9(6)   BLANK ZERO.
               05  FILLER       PIC XX.
           03  D-TIME REDEFINES D-USERID.
               05  D-HH         PIC 99.
               05  D-MM         PIC 99.
               05  D-AMPM       PIC XX.
               05  FILLER       PIC X.
           03  D-PRBAL          PIC Z(5)9.99- BLANK ZERO.
           03  D-INTRBS         PIC Z(3)9.99- BLANK ZERO.
           03  D-FEEGLNO        PIC X(7).
           03  FILLER           PIC X.
           03  D-FEEAMT         PIC Z(3)9.99-  BLANK ZERO.
           03  D-PROCEEDS       PIC Z(5)9.99-  BLANK ZERO.
           03  D-PROCEEDSX REDEFINES D-PROCEEDS.
               05 FILLER        PIC X(7).
               05 D-TILLNO      PIC Z9.
               05 FILLER        PIC X.
           03  D-DEBIT-CARD REDEFINES D-PROCEEDS PIC Z(10).

       01  D-LINE2 REDEFINES DETAIL-LINE.
           03  G-GROUP              PIC X(14).
           03  G-GROUP-1 REDEFINES G-GROUP.
               05  G-TOTNAME        PIC X(14).
               05  G-CODENAME REDEFINES G-TOTNAME.
                   07  FILLER       PIC X(10).
                   07  G-DCODE      PIC XXX.
                   07  G-COLON      PIC X.
           03  G-GROUP-2 REDEFINES G-GROUP.
               05  G-PROMPT1        PIC X(7).
               05  FILLER           PIC X.
               05  G-GRNO           PIC X(4).
               05  G-BRNO           REDEFINES G-GRNO
                                    PIC 9(4).
               05  FILLER           PIC X.
               05  G-GR-LEVEL       PIC 9(1).
           03  FILLER           PIC X(01).
           03  G-HEADING1       PIC X(12).
           03  G-AMOUNT-1       PIC ZZZZZZZZ9.99-  BLANK WHEN ZERO.
           03  FILLER REDEFINES G-AMOUNT-1.
               05  FILLER       PIC X(03).
               05  G-LOANS      PIC ZZZZZ9-    BLANK WHEN ZERO.
               05  FILLER       PIC X(03).
           03  FILLER           PIC X(1).
           03  G-HEADING2       PIC X(09).
           03  G-AMOUNT-2       PIC ZZZZZZZZ9.99-  BLANK WHEN ZERO.
           03  FILLER           PIC X(1).
           03  G-HEADING3       PIC X(09).
           03  G-AMOUNT-3       PIC ZZZZZZZZ9.99-  BLANK WHEN ZERO.
           03  FILLER           PIC X(1).
           03  G-HEADING4       PIC X(09).
           03  G-AMOUNT-4       PIC ZZZZZZZZ9.99-  BLANK WHEN ZERO.
           03  FILLER           PIC X(1).
           03  G-HEADING5       PIC X(09).
           03  G-AMOUNT-5       PIC ZZZZZZZZ9.99-  BLANK WHEN ZERO.

      *-----------------------------------------------------------------
       01  DETAIL2 REDEFINES DETAIL-LINE.
           03  FILLER           PIC X.
           03  D-MAKERCD        PIC X.
           03  FILLER           PIC X.
           03  D-BW-SSNO        PIC X(11).
           03  FILLER           PIC X.
           03  D-BW-NAME.
               05  FILLER       PIC X(19).
               03  D-CRED-SCORE PIC ZZZ9.

      *-----------------------------------------------------------------
       01  DETAIL3 REDEFINES DETAIL-LINE.
           03  FILLER           PIC X(2).
           03  D-BW-ADDR        PIC X(66).
           03  D-XX REDEFINES D-BW-ADDR.
               05  D-PHONE-TEXT PIC X(7).
               05  D-PHONE      PIC X(14).
               05  D-WORK-TEXT  PIC X(7).
               05  D-WORK       PIC X(14).
               05  FILLER       PIC X.
               05  D-EXT-TEXT   PIC X.
               05  D-EXT        PIC Z(4).
               05  FILLER       PIC X(18).

      *-----------------------------------------------------------------
      * DETAIL LINE FOR OUTPUT OF AUTOMOTIVE COLLATERAL
      *-----------------------------------------------------------------
       01  D-LINE1  REDEFINES DETAIL-LINE.
           03  FILLER         PIC X.
           03  FILLER         PIC X(17).
           03  D-FIL-MAKE     PIC X(7).
           03  D-MAKE         PIC X(15).
           03  FILLER         PIC XX.
           03  D-FIL-MODEL    PIC X(8).
           03  D-MODEL        PIC X(15).
           03  FILLER         PIC XX.
           03  D-FIL-YEAR     PIC X(7).
           03  D-YEAR         PIC X(4).
           03  FILLER         PIC XX.
           03  D-FIL-ID       PIC X(8).
           03  D-ID           PIC X(20).
           03  FILLER         PIC X(24).

      *-----------------------------------------------------------------
       01  RANGE-LINE REDEFINES DETAIL-LINE.
           03  FILLER           PIC X(35).
           03  RANGE-SELECTION.
               04  R-NUM        PIC Z(5)9.
               04  R-NUM-REDEF REDEFINES R-NUM.
                   05  R-CHAR   PIC X(6).

      ******************************************************************
      *
      *     C O P Y    L I B R A R Y     W O R K E R S
      *
      ******************************************************************
       COPY "LIBGB/PASSWDW.CPY".
       COPY "LIBLP/EMSFRG_CMD.CPY".
       COPY "LIBWI/GRSCANW.CPY".
       COPY "LIBGB/GETPRW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBWI/LCSCANW.CPY".
       COPY "LIBWI/GRTOTLW.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBGB/MKTEMPW_CMD.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/DACCESSW.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/SYSTEMW.CPY".
       COPY "LIBGB/BRSECUREW.CPY".
       COPY "LIBGB/GRWORKW.CPY".
       COPY "LIBGB/EOSPOOLW.CPY".

       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBLP/EMLNRG_DEF.CPY".
       COPY "LIBLP/EMLNRG_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

      ******************************************************************
      *
      *     P R O G R A M   C O N T R O L
      *
      ******************************************************************
       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBLP/EMLNRG_SCN.CPY".

       PROCEDURE DIVISION
            USING FORM-PATHNAME EXIT-PATHNAME.
       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
      *        PR-FILE WK-FILE WK2-FILE EMLNRG-FILE.
               PR-FILE WK-FILE EMLNRG-FILE.
       COPY "LIBGB/DECLRP.CPY".
           PERFORM CLOSE-TRL1-FILE.
           PERFORM CLOSE-TRL1-CONNECTION.
           PERFORM CLOSE-LTD1-FILE.
           PERFORM CLOSE-LTC1-FILE.
           PERFORM CLOSE-LN1-FILE.
           PERFORM CLOSE-LC1-FILE.
           PERFORM CLOSE-CDB1-FILE.
           PERFORM CLOSE-BW1-FILE.
           PERFORM CLOSE-GR1-FILE.
           PERFORM CLOSE-BR-FILE.
           PERFORM CLOSE-PR-FILE.
           CLOSE
      *        WK-FILE WK2-FILE EMLNRG-FILE.
               WK-FILE EMLNRG-FILE.
       COPY "LIBGB/DECLRP2.CPY".

      ******************************************************************
      *
      *    M A I N - M O D U L E
      *
      ******************************************************************
       MAIN-MODULE SECTION.

           PERFORM SQL-CONNECT.

      D    STOP MESS.
           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "EMLNRG" TO FORM-PROG VDU-FORM-NAME.

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
           IF LOCAL-PRINT-FG = "Y"
              PERFORM CLOSE-PR-FILE
              PERFORM CREATE-SPOOL-DIR
              IF STAT NOT = "00"
                 GO TO NEXT-GROUP-LOC.

           IF EXT-ROUTE-BUF = "00"
              MOVE SPACES TO WR-BUF
              STRING "REPORT IN PROGRESS............", BR-NO
                     DELIMITED "  " INTO WR-BUF
              MOVE MESS-LIST TO WR-LIST
              PERFORM CALL-WRFORM.

           MOVE EXT-FILPATH-BASE           TO FILPATH-BASE.
           MOVE BR-MACHINE                 TO FILPATH-MACHINE.
           MOVE BR-DATA-PATH               TO FILPATH-DATA.

           MOVE "B"                        TO BW-PATH-OVERRIDE
                                              LN-PATH-OVERRIDE
                                              LTC-PATH-OVERRIDE
                                              LTD-PATH-OVERRIDE
                                              TRL-PATH-OVERRIDE.

           PERFORM OPEN-LN1-FILE.
           PERFORM OPEN-LTC1-FILE.
           PERFORM OPEN-LTD1-FILE.
           PERFORM OPEN-TRL1-FILE.
           PERFORM OPEN-BW1-FILE.

      *    PERFORM SORT-ROUTINE.

           MOVE BR-NO               TO TL-BRNO
                                       QTRL1-WBEG-BRNO
                                       QTRL1-WEND-BRNO.
           MOVE BEGINNING-DATE      TO TL-DATE SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO QTRL1-WBEG-DATE.
           MOVE ENDING-DATE         TO  SQL-DATE-YYYYMMDD.
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
           PERFORM START-TRL1-FILE. *> SSP - TRL,LN,LTC,LTD,BW,CDB

           IF EXT-EOBUF-ERRCD = "X"
              GO TO NEXT-GROUP-LOC.

      *    PERFORM OPEN-WK2-FILE.

      *----------------------------------------------------------------
       NEXT-TR-REPORT.
      *    PERFORM READ-WK2-FILE-NEXT.
      *    IF IO-FG = 9
      *       GO TO END-OF-GROUP.

      *    MOVE WK2-BRANCH  TO TL-BRNO.
      *    MOVE WK2-DATE    TO TL-DATE.
      *    MOVE WK2-CLASS   TO TL-CLASS.
      *    MOVE WK2-ACCTNO  TO TL-ACCTNO.
      *    MOVE WK2-LAWCODE TO TL-LAWCODE.
      *    MOVE WK2-LINE    TO TL-LINE.

      *    PERFORM READ-TRL1-FILE.
      *    IF IO-FG NOT = 0
      *       MOVE "TRANSACTION RECORD MISSING SINCE SORT" TO MESS
      *       PERFORM SEND-MESS
      *       GO TO IO-ERROR.

           PERFORM READ-TRL1-FILE-NEXT. *> EMBEDDED - TRL,LN,LTC,LTD,BW,CDB
           IF (IO-FG NOT = 0) OR (TRL-PATH-OWNBR NOT = TL-BRNO)
              GO TO END-OF-GROUP.

           IF TL-DATE > ENDING-DATE
              GO TO NEXT-TR-REPORT.

           IF TL-BRNO > END-BRANCH
              GO TO NEXT-TR-REPORT.

           IF TL-BRNO < BEG-BRANCH
              GO TO NEXT-TR-REPORT.

           IF TL-CLASS < BEG-CLASS OR TL-CLASS > END-CLASS
              GO TO NEXT-TR-REPORT.
  
           MOVE TL-BRNO   TO LN-OWNBR.
           MOVE TL-ACCTNO TO LN-ACCTNO.
      *    PERFORM READ-LN1-FILE.
      *    IF IO-FG NOT = 0
      *       MOVE 0 TO LN-APRATE.
           
           IF (BEG-SCORE NUMERIC AND END-SCORE NUMERIC)
              IF LN-CRSCORE < BEG-SCORE OR LN-CRSCORE > END-SCORE
                 GO TO NEXT-TR-REPORT.

           IF FIRST-TIME = "Y"
              MOVE BR-STATE TO HOLD-STATE
              MOVE TL-BRNO  TO HOLD-BRNO
              MOVE TL-CLASS TO HOLD-CLASS
              PERFORM SET-CLASS-HEADINGS
              PERFORM SET-BRANCH-HEADINGS.

           IF TL-BRNO NOT = HOLD-BRNO
              PERFORM CLASS-TOTALS
              PERFORM BRANCH-TOTALS
              MOVE TL-BRNO  TO HOLD-BRNO
              MOVE TL-CLASS TO HOLD-CLASS
              MOVE BR-STATE TO HOLD-STATE
              PERFORM SET-CLASS-HEADINGS
              PERFORM SET-BRANCH-HEADINGS
              IF ( GTT-ISO NOT = "O" )
                 IF INCL-DETAIL = "Y"
                    PERFORM HEAD-PAGE.

           IF TL-CLASS NOT = HOLD-CLASS
              PERFORM CLASS-TOTALS
              MOVE TL-CLASS TO HOLD-CLASS
              PERFORM SET-CLASS-HEADINGS
              IF ( GTT-ISO NOT = "O" )
                 IF INCL-DETAIL = "Y"
                    PERFORM HEAD-PAGE.

           PERFORM TR-DETAIL1.

           IF EXTRACT-FG = "Y"
              IF TL-LNAMT > 0
                 PERFORM CREATE-EXTRACT.

           MOVE 1 TO LEV.
           PERFORM UPDATE-TABLE.
           PERFORM CREATE-WK.

           GO TO NEXT-TR-REPORT.

       END-OF-GROUP.
           PERFORM CLOSE-TRL1-FILE.
           PERFORM CLOSE-BW1-FILE.
           PERFORM CLOSE-LN1-FILE.
           PERFORM CLOSE-LTC1-FILE.
           PERFORM CLOSE-LTD1-FILE.
      *    PERFORM CLOSE-WK2-FILE.
      *    PERFORM OPEN-WK2-FILE-OUTPUT.

           IF ( LOCAL-PRINT-FG = "Y" )
              IF ( FIRST-TIME NOT = "Y" )
                 PERFORM CLASS-TOTALS
                 PERFORM BRANCH-TOTALS
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
           MOVE EMLNRG-QUEUE-POS TO Q-POS.
           MOVE EMLNRG-COPIES-POS TO C-POS.
           MOVE EMLNRG-PRU-POS   TO P-POS.
           MOVE SPACES TO Q-BUF C-BUF P-BUF.
       COPY "LIBGB/EOINIT.CPY".

           MOVE SPACES TO RECORDS-FOUND.

           MOVE 1 TO SUB.

       INIT-NEXT-TOTAL.
           PERFORM CLEAR-TOTAL-TBL.

           IF SUB < 3
              ADD 1 TO SUB
              GO TO INIT-NEXT-TOTAL.

           MOVE 0 TO PAGE-NUMBER.

      * CREATE INDEX KEY FILE FOR TRFILE OUTPUT - 2024 NOW IN STORED PROCEDURE

      *    MOVE MKTEMP-DEFAULT TO MKTEMP-BUF.
      *    PERFORM MKTEMP-CALL.
      *    IF STAT NOT = "00"
      *       MOVE "CANNOT MAKE SORT KEY FILE" TO MESS
      *       PERFORM SEND-MESS
      *       GO TO ENDIT.

      *    MOVE MKTEMP-BUF TO WK2-PATH.
      *    PERFORM OPEN-WK2-FILE-OUTPUT.

           MOVE MKTEMP-DEFAULT TO MKTEMP-BUF.
           PERFORM MKTEMP-CALL.
           IF STAT NOT = "00"
              MOVE "CANNOT MAKE SORT KEY FILE" TO MESS
              PERFORM SEND-MESS
              GO TO ENDIT.

           IF EXTRACT-FG = "Y"
              CALL "C$TOLOWER" USING EMLNRG-PATH1A, VALUE 12
              CALL "C$TOLOWER" USING EMLNRG-PATH2, VALUE 6
              MOVE EMLNRG-PATH1 TO ACCESS-BUF
              PERFORM ACCESS-CALL
              IF STAT-BAD
                 CALL "C$MAKEDIR" USING ACCESS-BUF GIVING STAT-99
              END-IF
              PERFORM OPEN-EMLNRG-FILE-OUTPUT.

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

           PERFORM OPEN-WK-FILE-OUTPUT.
           PERFORM CLOSE-WK-FILE.
           PERFORM OPEN-WK-FILE.

      *-----------------------------------------------------------------
       SET-HEAD.
           MOVE EXT-CONAME-CONAME TO H-NAME.
           CALL "C$JUSTIFY" USING H-NAME "C".
           MOVE EXT-CONAME-SYSDATE TO H-DATE.
           MOVE EXT-CONAME-USER-ID TO H-USERID.

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
              MOVE S-DD TO DAY-OF-MONTH.

      ******************************************************************
       ENTER-PARAMETERS SECTION.
      ******************************************************************
       ENTER-PARM.

           IF VDUSTATUS = "1U"
              GO TO EXTRACT-FILE-YN.

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
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "SNNN040BR2." TO VDU.
           MOVE 9999 TO END-BRANCH VDUNUM0.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
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

           GO TO DTE.

      *-----------------------------------------------------------------
       ALL-CL.
           MOVE "SNNN020CL1." TO VDU.
           MOVE 0 TO BEG-CLASS VDUNUM0.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "SNNN020CL2." TO VDU.
           MOVE 99 TO END-CLASS VDUNUM0.
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
              GO TO GTT-ISO-PARAGRAPH.

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

      *-----------------------------------------------------------------
       INCLDETAIL.

           IF ( GTT-ISO = "O" )
              MOVE "S  A010INCLDETAIL." TO VDU
              MOVE "N" TO INCL-DETAIL VDUBUF
              PERFORM SCREENIO
              MOVE "S  A010PRCOL." TO VDU
              MOVE "N" TO PRCOL-FG VDUBUF
              PERFORM SCREENIO
              GO TO LOCAL-PRINT.

           MOVE LEGEND-INCLDETAIL TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "E  A010INCLDETAIL." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.

           IF VDUSTATUS = "1U"
              IF ( GROUP-FLAG = "Y" )
                 GO TO GTT-ISO-PARAGRAPH.

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

           IF VDUSTATUS NOT = "00" AND "1D" GO TO INCLDETAIL.
           MOVE VDUBUF TO INCL-DETAIL.

           IF INCL-DETAIL NOT = "N" AND NOT = "Y"
              GO TO INCLDETAIL.

      *-----------------------------------------------------------------
       PRINTCOL.
           IF ( INCL-DETAIL = "N" )
              MOVE "S  A010PRCOL." TO VDU
              MOVE "N" TO PRCOL-FG VDUBUF
              PERFORM SCREENIO
              GO TO LOCAL-PRINT.

           MOVE LEGEND-PRINTCOL-01 TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "E  A010PRCOL." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO INCLDETAIL.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO PRINTCOL.

           IF VDUBUF NOT = "Y" AND
              VDUBUF NOT = "N"
                 MOVE "ENTER 'Y' OR 'N' ONLY " TO MESS
                 PERFORM SEND-MESS
                 GO TO PRINTCOL
           ELSE
                 MOVE VDUBUF TO PRCOL-FG.

      *-----------------------------------------------------------------
       SCORE-01.
           MOVE "ENTER BEGINNING CREDIT SCORE" TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "ENNN030SCORE1." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "16" GO TO ALL-SCORES.
           IF VDUSTATUS = "1U" GO TO PRINTCOL.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO SCORE-01.
           MOVE VDUNUM0 TO BEG-SCORE.
      *-----------------------------------------------------------------
       SCORE-02.
           MOVE "ENTER ENDING CREDIT SCORE" TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "ENNN030SCORE2." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO SCORE-01.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO SCORE-02.
           MOVE VDUNUM0 TO END-SCORE.
           IF BEG-SCORE > END-SCORE
              MOVE "INVALID SCORE RANGE" TO MESS
              PERFORM SEND-MESS
              GO TO SCORE-01.
           GO TO EXTRACT-FILE-YN.
      *-----------------------------------------------------------------
       ALL-SCORES.
           MOVE "SNNN030SCORE1." TO VDU.
           MOVE 0 TO BEG-SCORE VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN030SCORE2." TO VDU.
           MOVE 999 TO END-SCORE VDUNUM0.
           PERFORM SCREENIO.
      *-----------------------------------------------------------------
       EXTRACT-FILE-YN.
           MOVE LEGEND-CREATE-FILE TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "E  A010EXTRACT." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO SCORE-02.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D"
              GO TO EXTRACT-FILE-YN.
           MOVE VDUBUF TO EXTRACT-FG.
           IF EXTRACT-FG NOT = "Y"
              IF EXTRACT-FG NOT = "N"
                 MOVE "PLEASE ENTER 'Y' OR 'N'." TO MESS
                 PERFORM SEND-MESS
                 GO TO EXTRACT-FILE-YN.
           GO TO LOCAL-PRINT.
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
           MOVE LEGEND-LOCAL-SPOOL TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "S  A170SPOOL-PROMPT." TO VDU.
           MOVE "SPOOL TO LOCAL  :"     TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "S  A050YES-NO." TO VDU.
           MOVE "(Y/N)"           TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "E  A010LOCAL-SPOOL." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U"
              IF ( GROUP-FLAG = "Y" )
                 IF ( GTT-ISO = "O" )
                    GO TO GTT-ISO-PARAGRAPH
                 ELSE
                    GO TO PRINTCOL
              ELSE
                  GO TO PRINTCOL.
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
           MOVE "S  A010GTT-ISO." TO VDU.
           MOVE GTT-ISO           TO VDUBUF.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "S  A010INCLDETAIL." TO VDU.
           MOVE INCL-DETAIL TO VDUBUF.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "S  A010PRCOL." TO VDU.
           MOVE PRCOL-FG TO VDUBUF.
           PERFORM SCREENIO.
           PERFORM SCREENIO.                                                    
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -        
           MOVE "SNNN020DA1." TO VDU.                                           
           MOVE DAY-OF-MONTH TO VDUNUM0.                                        
           PERFORM SCREENIO.                                                    
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -        
           MOVE "S  A010GTT-ISO." TO VDU.                                       
           MOVE GTT-ISO           TO VDUBUF.                                    
           PERFORM SCREENIO.                                                    
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -        
           MOVE "S  A010INCLDETAIL." TO VDU.                                    
           MOVE INCL-DETAIL TO VDUBUF.                                          
           PERFORM SCREENIO.                                                    
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -        
           MOVE "S  A010PRCOL." TO VDU.                                         
           MOVE PRCOL-FG TO VDUBUF.                                             
           PERFORM SCREENIO.                                                    
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -        
           MOVE "SNNN030SCORE1." TO VDU.
           MOVE BEG-SCORE TO VDUNUM0.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "SNNN030SCORE2." TO VDU.
           MOVE END-SCORE TO VDUNUM0.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "S  A010EXTRACT." TO VDU.
           MOVE EXTRACT-FG        TO VDUBUF.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

           MOVE "S  A170SPOOL-PROMPT." TO VDU.                                  
           MOVE "SPOOL TO LOCAL  :"     TO VDUBUF.                              
           PERFORM SCREENIO.                                                    
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "S  A170SPOOL-PROMPT." TO VDU.
           MOVE "SPOOL TO LOCAL  :"     TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "S  A050YES-NO." TO VDU.
           MOVE "(Y/N)"           TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "S  A010LOCAL-SPOOL." TO VDU.
           MOVE LOCAL-PRINT-FG        TO VDUBUF.
           PERFORM SCREENIO.

      ******************************************************************
      * SORT-ROUTINE SECTION.
      ******************************************************************

      *    MOVE BR-NO               TO TL-BRNO
      *                                QTRL1-WBEG-BRNO
      *                                QTRL1-WEND-BRNO.
      *    MOVE BEGINNING-DATE      TO TL-DATE SQL-DATE-YYYYMMDD.
      *    PERFORM SQL-SET-DATE.
      *    MOVE SQL-DATE-YYYY-MM-DD TO QTRL1-WBEG-DATE.
      *    MOVE ENDING-DATE         TO  SQL-DATE-YYYYMMDD.
      *    PERFORM SQL-SET-DATE.
      *    MOVE SQL-DATE-YYYY-MM-DD TO QTRL1-WEND-DATE.
      *    MOVE BEG-CLASS           TO TL-CLASS
      *                                QTRL1-WBEG-CLASS.
      *    MOVE END-CLASS           TO QTRL1-WEND-CLASS.
      *    MOVE 0                   TO TL-ACCTNO TL-LAWCODE TL-LINE
      *                                QTRL1-WBEG-ACCTNO
      *                                QTRL1-WBEG-LAWCODE
      *                                QTRL1-WBEG-LINE.
      *    MOVE ALL "9"             TO QTRL1-WEND-ACCTNO
      *                                QTRL1-WEND-LAWCODE
      *                                QTRL1-WEND-LINE.
      *    PERFORM START-TRL1-FILE.

      *SORT-TR-NEXT.
      *    PERFORM READ-TRL1-FILE-NEXT.
      *    IF (IO-FG NOT = 0) OR (TRL-PATH-OWNBR NOT = TL-BRNO)
      *       GO TO EXIT-SORT-ROUTINE.

      *    IF TL-DATE > ENDING-DATE
      *       GO TO EXIT-SORT-ROUTINE.

      *    IF TL-BRNO > END-BRANCH
      *       GO TO EXIT-SORT-ROUTINE.

      *    IF TL-BRNO < BEG-BRANCH
      *       GO TO SORT-TR-NEXT.

      *    IF TL-CLASS < BEG-CLASS OR TL-CLASS > END-CLASS
      *       GO TO SORT-TR-NEXT.


      *    MOVE TL-DATE    TO WK2-DATE.
      *    MOVE TL-BRNO    TO WK2-BRANCH.
      *    MOVE TL-CLASS   TO WK2-CLASS.
      *    MOVE TL-ACCTNO  TO WK2-ACCTNO.
      *    MOVE TL-LAWCODE TO WK2-LAWCODE.
      *    MOVE TL-LINE TO WK2-LINE.

      *    PERFORM WRITE-WK2-FILE.
      *    IF IO-FG NOT = 0
      *       GO TO IO-ERROR.

      *    GO TO SORT-TR-NEXT.

      *EXIT-SORT-ROUTINE.
      *    PERFORM CLOSE-WK2-FILE.

      ******************************************************************
       TR-DETAIL1 SECTION.
      ******************************************************************
           IF FIRST-TIME = "Y"
              MOVE "N" TO FIRST-TIME
              MOVE EXT-CONAME-RPT-LINES TO LN-COUNT
              MOVE 1 TO LN-FEED
              MOVE "Y" TO RECORDS-FOUND.

           PERFORM VARYING DTL-SUB FROM 1 BY 1 UNTIL DTL-SUB > 12
              MOVE SPACES TO DTL-LINE(DTL-SUB)
           END-PERFORM.

           MOVE 0 TO DTL-SUB.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      * LINE1
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

           MOVE TL-CLASS TO D-CLASS.
           MOVE "-" TO D-H1.
           MOVE TL-ACCTNO TO D-NUMBER.
           MOVE "-" TO D-H2.
           MOVE TL-LAWCODE TO D-SPCLASS.
           MOVE TL-LOANDATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO D-LNDTE.
           MOVE TL-DLNO TO D-DLNO.

           COMPUTE D-APR ROUNDED = LN-APRATE * 1.
           MOVE TL-SRCD TO D-SRCD.
           MOVE TL-COLLCD TO D-COLCD.
           MOVE TL-ORGTERM TO D-TERM.
           MOVE TL-LNAMT TO D-LNAMT.
           MOVE TL-FINCHG TO D-FINCHG.


           MOVE 1 TO I-SUB.
           PERFORM SELECT-INSURANCE.
           IF I-SUB < 7
              MOVE TL-INSPREM(I-SUB) TO D-INSPREM
              MOVE TL-INSOURS(I-SUB) TO D-INSOURS.

           MOVE TL-PRLNNO            TO D-PRLNNO.
           MOVE TL-PRBAL             TO D-PRBAL.
           MOVE TL-PRFINCHGRB        TO D-INTRBS.
           MOVE TL-FEECODE(1)        TO CDB-CODE.
           MOVE QWS-CDB-TYPE1        TO CDB-TYPE.
           MOVE QWS-CDB-BRANCH1      TO CDB-BRANCH.
           MOVE QWS-CDB-CODE1        TO CDB-CODE.
           MOVE QWS-CDB-DESC1        TO CDB-DESC.
           MOVE QWS-CDB-GC-GLFEENO1  TO CDB-GC-GLFEENO.
           PERFORM GET-CODE.
           MOVE TL-FEEAMT(1)         TO D-FEEAMT.
           MOVE TL-PROCEEDS          TO D-PROCEEDS.

           IF INCL-DETAIL = "Y"
              ADD 1 TO DTL-SUB
              MOVE DETAIL-LINE TO DTL-LINE(DTL-SUB)
              MOVE SPACES TO DETAIL-LINE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      * LINE2
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

           MOVE TL-MAKERCD(1) TO D-MAKERCD.
           MOVE TL-SSNO(1) TO BW-SSNO.
           MOVE QWS-BW-OWNBR1         TO BW-OWNBR.       
           MOVE QWS-BW-SSNO1          TO BW-SSNO.       
           MOVE QWS-BW-LNAME1         TO BW-LNAME.       
           MOVE QWS-BW-FNAME1         TO BW-FNAME.       
           MOVE QWS-BW-ADR11          TO BW-ADR1.        
           MOVE QWS-BW-ADR21          TO BW-ADR2.        
           MOVE QWS-BW-CITY1          TO BW-CITY.        
           MOVE QWS-BW-STATE1         TO BW-STATE.       
           MOVE QWS-BW-ZIP1           TO BW-ZIP.         
           MOVE QWS-BW-PHONE1         TO BW-PHONE.       
           MOVE QWS-BW-EMPPHONE1      TO BW-EMPPHONE.    
           MOVE QWS-BW-EMPEXT1        TO BW-EMPEXT.      
           MOVE QWS-BW-CREDIT-SCORE1  TO BW-CREDIT-SCORE.
           MOVE "Y"                   TO FIRST-BORROWER-SW.
           PERFORM MOVE-BORROWER.
           MOVE "N"                   TO FIRST-BORROWER-SW.
           MOVE TL-PURCD TO D-COLCD.
           MOVE LN-CRSCORE TO D-TERM.

           IF ( HOLD-STATE NOT = "GA" )
              MOVE TL-SERCHG TO D-LNAMT
           ELSE
              IF NOT (TL-POINTS = 20.20 OR 02.02)
                 MOVE TL-POINTS TO D-LNAMT.

           COMPUTE D-FINCHG = TL-EARNINGS(1) + TL-EARNINGS(2) +
                              TL-EARNINGS(3) + TL-EARNINGS(4) +
                              TL-EARNINGS(5) + TL-EARNINGS(6).

           MOVE TL-POSTTIME TO HH-MM-SS.
           IF H-AND-M NOT GREATER THAN 1159
              MOVE "AM" TO D-AMPM
           ELSE
              MOVE "PM" TO D-AMPM.
           IF H-AND-M IS NOT LESS THAN 1300
              SUBTRACT 1200 FROM H-AND-M.
           IF CUR-HH = 0
              MOVE 12 TO CUR-HH.
           MOVE CUR-HH TO D-HH.
           MOVE CUR-MM TO D-MM.
           ADD 1 TO I-SUB.

           IF I-SUB < 7
              PERFORM SELECT-INSURANCE
              IF I-SUB < 7
                 MOVE TL-INSPREM(I-SUB) TO D-INSPREM
                 MOVE TL-INSOURS(I-SUB) TO D-INSOURS.

           MOVE TL-PRINSRB           TO D-PRBAL.
           MOVE TL-PRINTDUE          TO D-INTRBS.
           MOVE TL-FEECODE(2)        TO CDB-CODE.
           MOVE QWS-CDB-TYPE2        TO CDB-TYPE.
           MOVE QWS-CDB-BRANCH2      TO CDB-BRANCH.
           MOVE QWS-CDB-CODE2        TO CDB-CODE.
           MOVE QWS-CDB-DESC2        TO CDB-DESC.
           MOVE QWS-CDB-GC-GLFEENO2  TO CDB-GC-GLFEENO.
           PERFORM GET-CODE.
           MOVE TL-FEEAMT(2)         TO D-FEEAMT.
           MOVE TL-PROCASH           TO D-PROCEEDS.

           IF INCL-DETAIL = "Y"
              ADD 1 TO DTL-SUB
              MOVE DETAIL-LINE TO DTL-LINE(DTL-SUB)
              MOVE SPACES TO DETAIL-LINE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      * LINE3
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

           MOVE BW-CITY TO C-S-Z.
           MOVE BW-STATE TO STATE.
           MOVE BW-ZIP TO ZIP.
           PERFORM CAT-CSZ.

           IF BW-ADR2 NOT = SPACES
              STRING BW-ADR1,", ",BW-ADR2 DELIMITED BY "   "
                 INTO D-BW-ADDR
           ELSE
              MOVE BW-ADR1 TO D-BW-ADDR.

           STRING D-BW-ADDR,"   ",C-S-Z DELIMITED BY "     "
               INTO D-BW-ADDR.

           MOVE TL-USERID TO D-USERID.

           ADD 1 TO I-SUB.
           IF I-SUB < 7
              PERFORM SELECT-INSURANCE
              IF I-SUB < 7
                 MOVE TL-INSPREM(I-SUB) TO D-INSPREM
                 MOVE TL-INSOURS(I-SUB) TO D-INSOURS.

      *-   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -
      * GA WANTS 4% 8% SPLIT
      *-   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -
           IF ( HOLD-STATE = "GA" )
              MOVE TL-SERCHG TO D-LNAMT
              MOVE TL-STATE-WORKER TO D-FINCHG.

           MOVE TL-FEECODE(3)        TO CDB-CODE.
           MOVE QWS-CDB-TYPE3        TO CDB-TYPE.
           MOVE QWS-CDB-BRANCH3      TO CDB-BRANCH.
           MOVE QWS-CDB-CODE3        TO CDB-CODE.
           MOVE QWS-CDB-DESC3        TO CDB-DESC.
           MOVE QWS-CDB-GC-GLFEENO3  TO CDB-GC-GLFEENO.
           PERFORM GET-CODE.
           MOVE TL-FEEAMT(3)         TO D-FEEAMT.

           IF TL-PROCASH NOT = 0
              MOVE TL-TILLNO TO D-TILLNO
           ELSE
              IF TL-DEBIT-CARD-AMT NOT = 0
                 PERFORM PRINT-DEBIT-CARD-NUMBER.

           IF INCL-DETAIL = "Y"
              IF DETAIL-LINE NOT = SPACES
                 ADD 1 TO DTL-SUB
                 MOVE DETAIL-LINE TO DTL-LINE(DTL-SUB)
                 MOVE SPACES TO DETAIL-LINE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      * LINE4
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

           IF BR-EMLNRG-PHONE NOT = "Y"
              MOVE TL-MAKERCD(2) TO D-MAKERCD
              MOVE TL-SSNO(2) TO BW-SSNO
              MOVE QWS-BW-OWNBR2         TO BW-OWNBR      
              MOVE QWS-BW-SSNO2          TO BW-SSNO       
              MOVE QWS-BW-LNAME2         TO BW-LNAME       
              MOVE QWS-BW-FNAME2         TO BW-FNAME       
              MOVE QWS-BW-ADR12          TO BW-ADR1        
              MOVE QWS-BW-ADR22          TO BW-ADR2        
              MOVE QWS-BW-CITY2          TO BW-CITY        
              MOVE QWS-BW-STATE2         TO BW-STATE       
              MOVE QWS-BW-ZIP2           TO BW-ZIP         
              MOVE QWS-BW-PHONE2         TO BW-PHONE       
              MOVE QWS-BW-EMPPHONE2      TO BW-EMPPHONE    
              MOVE QWS-BW-EMPEXT2        TO BW-EMPEXT      
              MOVE QWS-BW-CREDIT-SCORE2  TO BW-CREDIT-SCORE
              PERFORM MOVE-BORROWER
           ELSE
              MOVE BW-PHONE TO PN-PHONE
              MOVE PN-123 TO PA-123
              MOVE PN-456 TO PA-456
              MOVE PN-7890 TO PA-7890
              MOVE "PHONE:" TO D-PHONE-TEXT
              MOVE PA-PHONE TO D-PHONE
              MOVE BW-EMPPHONE TO PN-PHONE
              MOVE PN-123 TO PA-123
              MOVE PN-456 TO PA-456
              MOVE PN-7890 TO PA-7890
              MOVE " WORK: " TO D-WORK-TEXT
              IF PN-PHONE NOT = 0
                 MOVE PA-PHONE TO D-WORK
                 IF BW-EMPEXT NOT = 0
                    MOVE "X" TO D-EXT-TEXT
                    MOVE BW-EMPEXT TO D-EXT.

           ADD 1 TO I-SUB.
           IF I-SUB < 7
              PERFORM SELECT-INSURANCE
              IF I-SUB < 7
                 MOVE TL-INSPREM(I-SUB) TO D-INSPREM
                 MOVE TL-INSOURS(I-SUB) TO D-INSOURS.

           MOVE TL-FEECODE(4)        TO CDB-CODE.
           MOVE QWS-CDB-TYPE4        TO CDB-TYPE.
           MOVE QWS-CDB-BRANCH4      TO CDB-BRANCH.
           MOVE QWS-CDB-CODE4        TO CDB-CODE.
           MOVE QWS-CDB-DESC4        TO CDB-DESC.
           MOVE QWS-CDB-GC-GLFEENO4  TO CDB-GC-GLFEENO.
           PERFORM GET-CODE.
           MOVE TL-FEEAMT(4)         TO D-FEEAMT.

      * IF PROCASH IS 0, THEN DEBIT CARD PRINTED ABOVE, OTHERWISE
      * PRINT IT HERE
           IF TL-PROCASH NOT = 0
              IF TL-DEBIT-CARD-AMT NOT = 0
                 PERFORM PRINT-DEBIT-CARD-NUMBER.

           IF INCL-DETAIL = "Y"
              IF DETAIL-LINE NOT = SPACES
                 ADD 1 TO DTL-SUB
                 MOVE DETAIL-LINE TO DTL-LINE(DTL-SUB)
                 MOVE SPACES TO DETAIL-LINE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      * LINE5
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF BR-EMLNRG-PHONE = "Y"
              MOVE TL-MAKERCD(2) TO D-MAKERCD
              MOVE TL-SSNO(2)    TO BW-SSNO
              MOVE QWS-BW-OWNBR2         TO BW-OWNBR      
              MOVE QWS-BW-SSNO2          TO BW-SSNO       
              MOVE QWS-BW-LNAME2         TO BW-LNAME       
              MOVE QWS-BW-FNAME2         TO BW-FNAME       
              MOVE QWS-BW-ADR12          TO BW-ADR1        
              MOVE QWS-BW-ADR22          TO BW-ADR2        
              MOVE QWS-BW-CITY2          TO BW-CITY        
              MOVE QWS-BW-STATE2         TO BW-STATE       
              MOVE QWS-BW-ZIP2           TO BW-ZIP         
              MOVE QWS-BW-PHONE2         TO BW-PHONE       
              MOVE QWS-BW-EMPPHONE2      TO BW-EMPPHONE    
              MOVE QWS-BW-EMPEXT2        TO BW-EMPEXT      
              MOVE QWS-BW-CREDIT-SCORE2  TO BW-CREDIT-SCORE
              PERFORM MOVE-BORROWER
           ELSE
              MOVE TL-MAKERCD(3) TO D-MAKERCD
      *       MOVE TL-SSNO(3)    TO BW-SSNO
              MOVE 0             TO BW-SSNO
              PERFORM MOVE-BORROWER.

           ADD 1 TO I-SUB.
           IF I-SUB < 7
              PERFORM SELECT-INSURANCE
              IF I-SUB < 7
                 MOVE TL-INSPREM(I-SUB) TO D-INSPREM
                 MOVE TL-INSOURS(I-SUB) TO D-INSOURS.

           MOVE TL-FEECODE(5)        TO CDB-CODE.
           MOVE QWS-CDB-TYPE5        TO CDB-TYPE.
           MOVE QWS-CDB-BRANCH5      TO CDB-BRANCH.
           MOVE QWS-CDB-CODE5        TO CDB-CODE.
           MOVE QWS-CDB-DESC5        TO CDB-DESC.
           MOVE QWS-CDB-GC-GLFEENO5  TO CDB-GC-GLFEENO.
           PERFORM GET-CODE.
           MOVE TL-FEEAMT(5)         TO D-FEEAMT.

           IF INCL-DETAIL = "Y"
              IF DETAIL-LINE NOT = SPACES
                 ADD 1 TO DTL-SUB
                 MOVE DETAIL-LINE TO DTL-LINE(DTL-SUB)
                 MOVE SPACES TO DETAIL-LINE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      * LINE6
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF BR-EMLNRG-PHONE = "Y"
              MOVE TL-MAKERCD(3) TO D-MAKERCD
      *       MOVE TL-SSNO(3)    TO BW-SSNO
              MOVE 0             TO BW-SSNO
              PERFORM MOVE-BORROWER
           ELSE
              MOVE TL-MAKERCD(4) TO D-MAKERCD
      *       MOVE TL-SSNO(4)    TO BW-SSNO
              MOVE 0             TO BW-SSNO
              PERFORM MOVE-BORROWER.

           ADD 1 TO I-SUB.
           IF I-SUB < 7
              PERFORM SELECT-INSURANCE
              IF I-SUB < 7
                 MOVE TL-INSPREM(I-SUB) TO D-INSPREM
                 MOVE TL-INSOURS(I-SUB) TO D-INSOURS.

           MOVE TL-FEECODE(6)        TO CDB-CODE.
           MOVE QWS-CDB-TYPE6        TO CDB-TYPE.
           MOVE QWS-CDB-BRANCH6      TO CDB-BRANCH.
           MOVE QWS-CDB-CODE6        TO CDB-CODE.
           MOVE QWS-CDB-DESC6        TO CDB-DESC.
           MOVE QWS-CDB-GC-GLFEENO6  TO CDB-GC-GLFEENO.
           PERFORM GET-CODE.
           MOVE TL-FEEAMT(6)         TO D-FEEAMT.

           IF INCL-DETAIL = "Y"
              IF DETAIL-LINE NOT = SPACES
                 ADD 1 TO DTL-SUB
                 MOVE DETAIL-LINE TO DTL-LINE(DTL-SUB)
                 MOVE SPACES TO DETAIL-LINE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      * LINE7
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

           IF BR-EMLNRG-PHONE = "Y"
              MOVE TL-MAKERCD(4) TO D-MAKERCD
      *       MOVE TL-SSNO(4) TO BW-SSNO
              MOVE 0          TO BW-SSNO
              PERFORM MOVE-BORROWER.

           MOVE TL-FEECODE(7)        TO CDB-CODE.
           MOVE QWS-CDB-TYPE7        TO CDB-TYPE.
           MOVE QWS-CDB-BRANCH7      TO CDB-BRANCH.
           MOVE QWS-CDB-CODE7        TO CDB-CODE.
           MOVE QWS-CDB-DESC7        TO CDB-DESC.
           MOVE QWS-CDB-GC-GLFEENO7  TO CDB-GC-GLFEENO.
           PERFORM GET-CODE.
           MOVE TL-FEEAMT(7)         TO D-FEEAMT.

           IF INCL-DETAIL = "Y"
              IF DETAIL-LINE NOT = SPACES
                 ADD 1 TO DTL-SUB
                 MOVE DETAIL-LINE TO DTL-LINE(DTL-SUB)
                 MOVE SPACES TO DETAIL-LINE.

           MOVE TL-FEECODE(8)        TO CDB-CODE.
           MOVE QWS-CDB-TYPE8        TO CDB-TYPE.
           MOVE QWS-CDB-BRANCH8      TO CDB-BRANCH.
           MOVE QWS-CDB-CODE8        TO CDB-CODE.
           MOVE QWS-CDB-DESC8        TO CDB-DESC.
           MOVE QWS-CDB-GC-GLFEENO8  TO CDB-GC-GLFEENO.
           PERFORM GET-CODE.
           MOVE TL-FEEAMT(8)         TO D-FEEAMT.

           IF INCL-DETAIL = "Y"
              IF DETAIL-LINE NOT = SPACES
                 ADD 1 TO DTL-SUB
                 MOVE DETAIL-LINE TO DTL-LINE(DTL-SUB)
                 MOVE SPACES TO DETAIL-LINE.

           MOVE TL-FEECODE(9)        TO CDB-CODE.
           MOVE QWS-CDB-TYPE9        TO CDB-TYPE.
           MOVE QWS-CDB-BRANCH9      TO CDB-BRANCH.
           MOVE QWS-CDB-CODE9        TO CDB-CODE.
           MOVE QWS-CDB-DESC9        TO CDB-DESC.
           MOVE QWS-CDB-GC-GLFEENO9  TO CDB-GC-GLFEENO.
           PERFORM GET-CODE.
           MOVE TL-FEEAMT(9)         TO D-FEEAMT.

           IF INCL-DETAIL = "Y"
              IF DETAIL-LINE NOT = SPACES
                 ADD 1 TO DTL-SUB
                 MOVE DETAIL-LINE TO DTL-LINE(DTL-SUB)
                 MOVE SPACES TO DETAIL-LINE.

           MOVE TL-FEECODE(10)       TO CDB-CODE.
           MOVE QWS-CDB-TYPE10       TO CDB-TYPE.             
           MOVE QWS-CDB-BRANCH10     TO CDB-BRANCH.         
           MOVE QWS-CDB-CODE10       TO CDB-CODE.            
           MOVE QWS-CDB-DESC10       TO CDB-DESC.            
           MOVE QWS-CDB-GC-GLFEENO10 TO CDB-GC-GLFEENO.    
           PERFORM GET-CODE.
           MOVE TL-FEEAMT(10)        TO D-FEEAMT.

           IF INCL-DETAIL = "Y"
              IF DETAIL-LINE NOT = SPACES
                 ADD 1 TO DTL-SUB
                 MOVE DETAIL-LINE TO DTL-LINE(DTL-SUB)
                 MOVE SPACES TO DETAIL-LINE.

           IF INCL-DETAIL = "Y"
              PERFORM PRINT-LT-REC-COL.

      *    -------------------------------------------------------------
      *    IF DETAIL LINES FOR ACCOUNT WILL EXCEED PAGE END,
      *       THEN ADVANCE TO NEXT PAGE TO PRINT FIRST LINE
      *       ( NOTE: NEED THE +1 BECAUSE WE ARE SKIPPING A LINE
      *               BEFORE PRINTING FIRST LINE FOR EACH ACCOUNT
      *               [I.E. MOVE 2 TO LN-FEED])
      *    -------------------------------------------------------------
           IF (DTL-SUB + LN-COUNT + 1) > EXT-CONAME-RPT-LINES
              MOVE EXT-CONAME-RPT-LINES TO LN-COUNT.

      *    -------------------------------------------------------------
      *    PRINT ALL DETAIL-LINES FOR THE ACCOUNT.
      *    -------------------------------------------------------------
           MOVE 2 TO LN-FEED.
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > DTL-SUB
              MOVE DTL-LINE(SUB) TO DETAIL-LINE
              PERFORM WRITE-DETAIL-LINE
           END-PERFORM.
           MOVE SPACES TO DETAIL-LINE.

       END-TR1-DETAIL.
           EXIT.

      ******************************************************************
       CREATE-EXTRACT SECTION.
      ******************************************************************
           MOVE BR-NO          TO EXT-BRNO.
           MOVE TL-ACCTNO      TO EXT-NUMBER.
           MOVE TL-LOANDATE    TO EXT-LOANDATE.
           MOVE TL-DLNO        TO EXT-DLNO.
           MOVE TL-APR         TO EXT-APR.
           MOVE TL-SRCD        TO EXT-SRCD.
           MOVE TL-COLLCD      TO EXT-COLLCD.
           MOVE TL-ORGTERM     TO EXT-TERM.
           MOVE TL-LNAMT       TO EXT-LNAMT.
           MOVE TL-SERCHG      TO EXT-SERCHG.
           MOVE TL-FINCHG      TO EXT-FINCHG.
           MOVE TL-INSPREM(1)  TO EXT-LIFE.
           MOVE TL-INSPREM(2)  TO EXT-AH.
           MOVE TL-INSPREM(3)  TO EXT-PP.
           MOVE TL-INSPREM(4)  TO EXT-O1.
           MOVE TL-INSPREM(5)  TO EXT-O2.
           MOVE TL-INSPREM(6)  TO EXT-O3.
           MOVE TL-PRLNNO      TO EXT-PRLNNO.
           MOVE TL-PRBAL       TO EXT-PRBAL.
           MOVE TL-PRFINCHGRB  TO EXT-INT-REBATE.
           MOVE TL-FEECODE(1)  TO EXT-FEECODE1.
           MOVE TL-FEECODE(2)  TO EXT-FEECODE2.
           MOVE TL-FEECODE(3)  TO EXT-FEECODE3.
           MOVE TL-PROCEEDS    TO EXT-PROCEEDS.
           MOVE LN-CRSCORE     TO EXT-CRSCORE.
           CALL "C$JUSTIFY" USING EXT-DLNO, "L".
           CALL "C$JUSTIFY" USING EXT-APR, "L".
           CALL "C$JUSTIFY" USING EXT-TERM, "L".
           CALL "C$JUSTIFY" USING EXT-LNAMT, "L".
           CALL "C$JUSTIFY" USING EXT-SERCHG, "L".
           CALL "C$JUSTIFY" USING EXT-FINCHG, "L".
           CALL "C$JUSTIFY" USING EXT-LIFE, "L".
           CALL "C$JUSTIFY" USING EXT-AH, "L".
           CALL "C$JUSTIFY" USING EXT-PP, "L".
           CALL "C$JUSTIFY" USING EXT-O1, "L".
           CALL "C$JUSTIFY" USING EXT-O2, "L".
           CALL "C$JUSTIFY" USING EXT-O3, "L".
           CALL "C$JUSTIFY" USING EXT-PRBAL, "L".
           CALL "C$JUSTIFY" USING EXT-INT-REBATE, "L".
           CALL "C$JUSTIFY" USING EXT-PROCEEDS, "L".
           CALL "C$JUSTIFY" USING EXT-CRSCORE, "L".

           MOVE SPACES TO EMLNRG-REC.
           STRING 
                  EXT-BRNO, X'09',
                  EXT-NUMBER, X'09',
                  EXT-LOANDATE, X'09',
                  EXT-DLNO, X'09',
                  EXT-APR, X'09',
                  EXT-SRCD, X'09',
                  EXT-COLLCD, X'09',
                  EXT-TERM, X'09',
                  EXT-LNAMT, X'09',
                  EXT-SERCHG, X'09',
                  EXT-FINCHG, X'09',
                  EXT-LIFE, X'09',
                  EXT-AH, X'09',
                  EXT-PP, X'09',
                  EXT-O1, X'09',
                  EXT-O2, X'09',
                  EXT-O3, X'09',
                  EXT-PRLNNO, X'09',
                  EXT-PRBAL, X'09',
                  EXT-INT-REBATE, X'09',
                  EXT-FEECODE1, X'09',
                  EXT-FEECODE2, X'09',
                  EXT-FEECODE3, X'09',
                  EXT-PROCEEDS, X'09',
                  EXT-CRSCORE, 
               DELIMITED BY " " INTO EMLNRG-REC.
           PERFORM WRITE-EMLNRG-FILE.
           IF IO-BAD
              GO TO IO-ERROR.
           ADD 1 TO TOT-EXTRACTED.

      ******************************************************************
       CREATE-WK SECTION.
      ******************************************************************
           MOVE TL-SRCD  TO SAVE-WK-CODE.
           MOVE TL-BRNO  TO SAVE-WK-BR.
           MOVE TL-CLASS TO SAVE-WK-CL.

       SAVE-WORK-KEY.
           MOVE SAVE-WK-KEY TO WK-KEY.
           PERFORM READ-WK-FILE.

           IF IO-FG = 0
              GO TO CREATE-WK-UPDATE.

           MOVE " " TO WK-REC.
           MOVE SAVE-WK-KEY TO WK-KEY.

           MOVE 0 TO WK-NOLOANS WK-LNAMT WK-EARNINGS WK-INTCHG
                     WK-SERCHG WK-EXTCHG WK-MFEE WK-POINTS WK-STATE
                     WK-DL WK-LL WK-AH WK-PP WK-O1 WK-O2 WK-O3 WK-FEES
                     WK-WRLD-NONFILE-90 WK-PROCEEDS WK-PROCASH
                     WK-RENEW-PRIN.

           PERFORM WRITE-WK-FILE.

           IF IO-FG NOT = 0
              GO TO IO-ERROR.

           GO TO SAVE-WORK-KEY.

       CREATE-WK-UPDATE.
           IF TL-LNAMT < 0
              SUBTRACT 1 FROM WK-NOLOANS
           ELSE
              ADD 1 TO WK-NOLOANS.

           ADD TL-LNAMT TO WK-LNAMT.
           ADD TL-INTCHG TO WK-INTCHG.
           ADD TL-SERCHG TO WK-SERCHG.
           ADD TL-EXTCHG TO WK-EXTCHG.

           COMPUTE WK-MFEE ROUNDED = WK-MFEE
                                   + (TL-MAINTFEE * TL-ORGTERM).

           IF BR-STATE = "GA"
              ADD TL-STATE-WORKER TO WK-STATE
              IF NOT (TL-POINTS = 20.20 OR 02.02)
                 ADD TL-POINTS TO WK-POINTS.

           COMPUTE WK-EARNINGS = WK-EARNINGS +
                                 TL-EARNINGS(1) + TL-EARNINGS(2) +
                                 TL-EARNINGS(3) + TL-EARNINGS(4) +
                                 TL-EARNINGS(5) + TL-EARNINGS(6).

           IF TL-INSTYPES-DL
              IF TL-INSOURS(1) = "Y"
                 ADD TL-INSPREM(1) TO WK-DL.

           IF NOT TL-INSTYPES-DL
              IF TL-INSOURS(1) = "Y"
                 ADD TL-INSPREM(1) TO WK-LL.

           IF TL-INSOURS(2) = "Y"
              ADD TL-INSPREM(2) TO WK-AH.

           IF TL-INSOURS(3) = "Y"
              ADD TL-INSPREM(3) TO WK-PP.

           IF TL-INSOURS(4) = "Y"
              ADD TL-INSPREM(4) TO WK-O1.

           IF TL-INSOURS(5) = "Y"
              ADD TL-INSPREM(5) TO WK-O2.

           IF TL-INSOURS(6) = "Y"
              ADD TL-INSPREM(6) TO WK-O3.

           PERFORM VARYING FEE-SUB FROM 1 BY 1 UNTIL FEE-SUB > 10
              IF TL-FEECODE(FEE-SUB) = 90
                 ADD TL-FEEAMT(FEE-SUB) TO WK-WRLD-NONFILE-90
              END-IF
           END-PERFORM.


           ADD TL-FEEAMT(1) TL-FEEAMT(2) TL-FEEAMT(3)
               TL-FEEAMT(4) TL-FEEAMT(5) TL-FEEAMT(6)
               TL-FEEAMT(7) TL-FEEAMT(8) TL-FEEAMT(9)
               TL-FEEAMT(10) TO WK-FEES.

           ADD TL-PROCEEDS TO WK-PROCEEDS.
           ADD TL-PROCASH TO WK-PROCASH.

           COMPUTE WK-RENEW-PRIN = WK-RENEW-PRIN
                                 + TL-PRBAL
                                 + TL-PRINTDUE
                                 - TL-PRFINCHGRB
                                 - TL-PRINSRB.

           PERFORM REWRITE-WK-FILE.

       EXIT-CREATE-WK.
           EXIT.

      ******************************************************************
       MOVE-BORROWER SECTION.
      ******************************************************************

           IF BW-SSNO = 0 AND
              FIRST-BORROWER-SW = "Y"
              GO TO CONTINUE-MOVE-BORROWER.

           IF BW-SSNO = 0 
              GO TO EXIT-MOVE-BORROWER.


       CONTINUE-MOVE-BORROWER.

           MOVE SPACES TO D-BW-SSNO.

      *    PERFORM READ-BW1-FILE.
      *    IF IO-FG = 9
           IF BW-LNAME = SPACES
              MOVE SPACES TO BW-REC
              MOVE "* * NOT FOUND * *" TO BW-LNAME
              MOVE 0 TO BW-CREDIT-SCORE BW-PHONE BW-EMPPHONE
                        BW-EMPEXT.

           MOVE SPACES TO D-BW-NAME.
           STRING BW-LNAME," ",BW-FNAME DELIMITED BY "   "
                 INTO D-BW-NAME.

           IF BR-CREDIT-SCORING-FG = "Y"
              MOVE BW-CREDIT-SCORE TO D-CRED-SCORE.

       EXIT-MOVE-BORROWER.
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
       SET-CLASS-HEADINGS SECTION.
      ******************************************************************
           MOVE HOLD-CLASS TO H-LCNO LC-CODE.
           PERFORM OPEN-LC1-FILE.
           PERFORM READ-LC1-FILE. *> EMBEDDED - LCFILE

           IF IO-FG NOT = 0
              MOVE "INVALID" TO LC-DESC.

           PERFORM CLOSE-LC1-FILE.
           MOVE LC-DESC TO H-LCDESC.
           MOVE "N" TO SUMMARY-FG.

      ******************************************************************
       CLASS-TOTALS SECTION.
      ******************************************************************

           IF ( GTT-ISO = "O" )
              GO TO END-CLASS-TOTALS.

           MOVE "Y" TO SUMMARY-FG.
           MOVE EXT-CONAME-RPT-LINES TO LN-COUNT.
           PERFORM WRITE-DETAIL-LINE.
      *--- IF INCL-DETAIL = "Y"
      *       PERFORM WRITE-DETAIL-LINE
      *    ELSE
      *       MOVE SPACES TO DETAIL-LINE
      *       IF LN-COUNT = EXT-CONAME-RPT-LINES
      *          PERFORM HEAD-PAGE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE HOLD-BRNO TO WK-BR.
           MOVE HOLD-CLASS TO WK-CLASS.
           MOVE " " TO WK-CODE.
           PERFORM START-WK-FILE.

      *-----------------------------------------------------------------
       PRINT-NEXT-CODE.
           PERFORM READ-WK-FILE-NEXT.

           IF IO-FG = 9
              PERFORM TYPE-TOTALS
              GO TO END-CLASS-TOTALS.

           IF WK-CLASS NOT = HOLD-CLASS
              PERFORM TYPE-TOTALS
              GO TO END-CLASS-TOTALS.

           IF WK-BR NOT = HOLD-BRNO
              PERFORM TYPE-TOTALS
              GO TO END-CLASS-TOTALS.

           MOVE WK-CODE     TO G-DCODE.
           MOVE ":"         TO G-COLON.

      * LINE 1
           MOVE "LOANS   :" TO G-HEADING1.
           MOVE WK-NOLOANS  TO G-LOANS.
           MOVE "SRVCHRG :" TO G-HEADING2.
           MOVE WK-SERCHG   TO G-AMOUNT-2.
           MOVE "OUR-DL  :" TO G-HEADING3.
           MOVE WK-DL       TO G-AMOUNT-3.
           MOVE "OUR-O1  :" TO G-HEADING4.
           MOVE WK-O1       TO G-AMOUNT-4.
           MOVE "EARNINGS:" TO G-HEADING5.
           MOVE WK-EARNINGS TO G-AMOUNT-5.
           MOVE 2           TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      * LINE 2
           MOVE "LOAN AMT:" TO G-HEADING1.
           MOVE WK-LNAMT    TO G-AMOUNT-1.
           MOVE "EXTCHG  :" TO G-HEADING2.
           MOVE WK-EXTCHG   TO G-AMOUNT-2.
           MOVE "OUR-LL  :" TO G-HEADING3.
           MOVE WK-LL       TO G-AMOUNT-3.
           MOVE "OUR-O2  :" TO G-HEADING4.
           MOVE WK-O2       TO G-AMOUNT-4.
           MOVE "PROCASH :" TO G-HEADING5.
           MOVE WK-PROCASH  TO G-AMOUNT-5.
           PERFORM WRITE-DETAIL-LINE.


      * LINE 3
           MOVE "INTCHG  :" TO G-HEADING1.
           MOVE WK-INTCHG   TO G-AMOUNT-1.
           MOVE "MAINTFEE:" TO G-HEADING2.
           MOVE WK-MFEE     TO G-AMOUNT-2.
           MOVE "OUR-AH  :" TO G-HEADING3.
           MOVE WK-AH       TO G-AMOUNT-3.
           MOVE "OUR-O3  :" TO G-HEADING4.
           MOVE WK-O3       TO G-AMOUNT-4.
           MOVE "PROCEEDS:" TO G-HEADING5.
           MOVE WK-PROCEEDS TO G-AMOUNT-5.
           PERFORM WRITE-DETAIL-LINE.

      * LINE 4
      *- - - 8% SERVICE  - - - - - - - - - -
           IF ( HOLD-STATE = "GA" )
              MOVE "8% SRVCH:" TO G-HEADING1
              MOVE WK-STATE    TO G-AMOUNT-1
              MOVE "CLOSING :" TO G-HEADING2
              MOVE WK-POINTS   TO G-AMOUNT-2.
           MOVE "OUR-PP  :"    TO G-HEADING3.
           MOVE WK-PP          TO G-AMOUNT-3.
           MOVE "OUR FEES:"    TO G-HEADING4.
           MOVE WK-FEES        TO G-AMOUNT-4.
           MOVE "REN PRIN:"    TO G-HEADING5.
           MOVE WK-RENEW-PRIN  TO G-AMOUNT-5.
           PERFORM WRITE-DETAIL-LINE.

           GO TO PRINT-NEXT-CODE.

      *-----------------------------------------------------------------
       END-CLASS-TOTALS.
           MOVE 2 TO LEV.
           MOVE 1 TO SUB.
           ADD T-NOLOANS(1) TO T-NOLOANS(2).
           PERFORM UPDATE-TABLE2.
           MOVE 1 TO SUB.
           PERFORM CLEAR-TOTAL-TBL.
           MOVE "N" TO SUMMARY-FG.

      ******************************************************************
       BRANCH-TOTALS SECTION.
      ******************************************************************

           IF ( GTT-ISO = "O" )
              GO TO END-BRANCH-TOTALS.

           MOVE "Y" TO SUMMARY-FG.
           MOVE 0 TO HOLD-CLASS.
           MOVE 2 TO LEV.
           MOVE "BRANCH TOTALS:" TO G-TOTNAME.
           PERFORM PRINT-SUMMARY.

      *-----------------------------------------------------------------
       END-BRANCH-TOTALS.
           MOVE 3 TO LEV.
           MOVE 2 TO SUB.
           ADD T-NOLOANS(2) TO T-NOLOANS(3).
           PERFORM UPDATE-TABLE2.
           MOVE 2 TO SUB.
           PERFORM CLEAR-TOTAL-TBL.
           MOVE "N" TO SUMMARY-FG.

      ******************************************************************
       GRAND-TOTALS SECTION.
      ******************************************************************

           IF ( GTT-ISO = "O" )
              GO TO END-GRAND-TOTALS.

           MOVE "Y" TO SUMMARY-FG.
           MOVE 3 TO LEV.
      *--->MOVE "GRAND  TOTALS:" TO G-TOTNAME.
      *--->PERFORM PRINT-SUMMARY.
           PERFORM PRINT-GRAND-TOTALS.

      *-----------------------------------------------------------------
       END-GRAND-TOTALS.
           MOVE 3 TO SUB.
           PERFORM CLEAR-TOTAL-TBL.
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

           IF BR-CREDIT-SCORING-FG = "Y"
              MOVE "CRD " TO H-CRED-SCORE H-CRED-SCORE2
           ELSE
              MOVE SPACES TO H-CRED-SCORE H-CRED-SCORE2.

           IF ( GTT-HEADER-FG = "Y" )
              MOVE GTT-HEAD1 TO PR-REC
              MOVE 2 TO LN-FEED
              PERFORM WRITE-PR-REC
              MOVE GTT-HEAD2 TO PR-REC
              MOVE 1 TO LN-FEED
              PERFORM WRITE-PR-REC
              MOVE ALL "-" TO PR-REC
              PERFORM WRITE-PR-REC
              MOVE 8 TO LN-COUNT
              MOVE 1 TO LN-FEED
           ELSE
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF SUMMARY-FG = "N"
              MOVE HEAD5 TO PR-REC
              MOVE 2 TO LN-FEED
              PERFORM WRITE-PR-REC
              IF ( HOLD-STATE = "GA" )
                 MOVE HEAD6A TO PR-REC
                 MOVE 1 TO LN-FEED
                 PERFORM WRITE-PR-REC
                 MOVE HEAD7 TO PR-REC
                 MOVE 1 TO LN-FEED
                 PERFORM WRITE-PR-REC
                 MOVE 10 TO LN-COUNT
                 MOVE 2  TO LN-FEED
              ELSE
                 MOVE HEAD6 TO PR-REC
                 MOVE 1 TO LN-FEED
                 PERFORM WRITE-PR-REC
                 MOVE 9 TO LN-COUNT
                 MOVE 2 TO LN-FEED
           ELSE
              MOVE 6 TO LN-COUNT
              MOVE 1 TO LN-FEED.

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
           MOVE "INCLUDE DETAIL" TO RANGE-LINE.
           MOVE INCL-DETAIL TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "BEG SCORE" TO RANGE-LINE.
           MOVE BEG-SCORE TO R-NUM.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "END SCORE" TO RANGE-LINE.
           MOVE END-SCORE TO R-NUM.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "EXTRACT FILE" TO RANGE-LINE.
           MOVE EXTRACT-FG TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "PRINT COLLATERAL" TO RANGE-LINE.
           MOVE PRCOL-FG TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "GROUP TOTALS" TO RANGE-LINE.
           MOVE GTT-ISO        TO R-CHAR.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF EXTRACT-FG = "Y"
              MOVE "TOTAL RECORDS EXTRACTED" TO RANGE-LINE
              MOVE TOT-EXTRACTED TO R-NUM
              PERFORM WRITE-DETAIL-LINE.


      *****************************
       SELECT-INSURANCE SECTION.
      *****************************
       SELECT-IT.

           IF I-SUB > 6
              GO TO EXIT-SELECT-INSURANCE.

           IF TL-INSPREM(I-SUB) = 0
              ADD 1 TO I-SUB
              GO TO SELECT-IT.

           IF I-SUB = 1
              PERFORM GET-CREDIT-LIFE-TAG
           ELSE
           IF I-SUB = 2
              MOVE "A" TO D-INSTYPE
           ELSE
           IF I-SUB = 3
              MOVE "P" TO D-INSTYPE
           ELSE
           IF I-SUB = 4
              MOVE "1" TO D-INSTYPE
           ELSE
           IF I-SUB = 5
              MOVE "2" TO D-INSTYPE
           ELSE
           IF I-SUB = 6
              MOVE "3" TO D-INSTYPE.

       EXIT-SELECT-INSURANCE.
           EXIT.

      *****************************************************************
       CLASS-SCAN SECTION.
      *****************************************************************
           MOVE 0      TO LCSCAN-KEY.
           MOVE SPACES TO LCSCAN-VERIFY.
           MOVE "N"    TO LCSCAN-SALEFIN.
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
           ADD WK-NOLOANS     TO  WS-GTT-NOLOANS   ( GTT-TOTAL-SUB ).
           ADD WK-LNAMT       TO  WS-GTT-LNAMT     ( GTT-TOTAL-SUB ).
           ADD WK-INTCHG      TO  WS-GTT-INTCHG    ( GTT-TOTAL-SUB ).
           ADD WK-SERCHG      TO  WS-GTT-SERCHG    ( GTT-TOTAL-SUB ).
           ADD WK-EXTCHG      TO  WS-GTT-EXTCHG    ( GTT-TOTAL-SUB ).
           ADD WK-MFEE        TO  WS-GTT-MFEE      ( GTT-TOTAL-SUB ).
           ADD WK-POINTS      TO  WS-GTT-POINTS    ( GTT-TOTAL-SUB ).
           ADD WK-STATE       TO  WS-GTT-STATE     ( GTT-TOTAL-SUB ).
           ADD WK-EARNINGS    TO  WS-GTT-EARNINGS  ( GTT-TOTAL-SUB ).
           ADD WK-DL          TO  WS-GTT-DL        ( GTT-TOTAL-SUB ).
           ADD WK-LL          TO  WS-GTT-LL        ( GTT-TOTAL-SUB ).
           ADD WK-AH          TO  WS-GTT-AH        ( GTT-TOTAL-SUB ).
           ADD WK-PP          TO  WS-GTT-PP        ( GTT-TOTAL-SUB ).
           ADD WK-O1          TO  WS-GTT-O1        ( GTT-TOTAL-SUB ).
           ADD WK-O2          TO  WS-GTT-O2        ( GTT-TOTAL-SUB ).
           ADD WK-O3          TO  WS-GTT-O3        ( GTT-TOTAL-SUB ).
           ADD WK-FEES        TO  WS-GTT-FEES      ( GTT-TOTAL-SUB ).
           ADD WK-WRLD-NONFILE-90
                         TO  WS-GTT-WRLD-NONFILE-90( GTT-TOTAL-SUB ).
           ADD WK-PROCEEDS    TO  WS-GTT-PROCEEDS  ( GTT-TOTAL-SUB ).
           ADD WK-PROCASH     TO  WS-GTT-PROCASH   ( GTT-TOTAL-SUB ).
           ADD WK-RENEW-PRIN  TO  WS-GTT-RENEW-PRIN( GTT-TOTAL-SUB ).

       EXIT-GTT-ADD-TOTALS.
           EXIT.

      ******************************************************************
       GTT-COMPILE-GROUP-TOTALS SECTION.
      ******************************************************************
           MOVE SPACES      TO WK-KEY.
           MOVE SPACES      TO WK-CODE.
           MOVE WGR-SRESULT TO WK-BR.
           MOVE ZEROES      TO WK-CLASS.
           PERFORM START-WK-FILE.

       GTT-NEXT-WK1.

           PERFORM READ-WK-FILE-NEXT.

           IF ( IO-FG   NOT = 0           ) OR
              ( WK-BR NOT = WGR-SRESULT )
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
           MOVE "BRANCH:" TO G-PROMPT1 WS-HOLD-PROMPT.
           MOVE BR-NO     TO G-BRNO.

           PERFORM GTT-PRINT-TOTALS.

       EXIT-GTT-PRINT-GROUP-BR-TOTALS.
           EXIT.

      ******************************************************************
       GTT-PRINT-GROUP-TOTALS SECTION.
      ******************************************************************

           MOVE SPACES    TO DETAIL-LINE.
           MOVE "GROUP :" TO G-PROMPT1 WS-HOLD-PROMPT.
           MOVE GR-ID     TO G-GRNO.
           MOVE GR-LEVEL  TO G-GR-LEVEL.

           PERFORM GTT-PRINT-TOTALS.

       EXIT-GTT-PRINT-GROUP-TOTALS.
           EXIT.

      ******************************************************************
       GTT-PRINT-TOTALS SECTION.
      ******************************************************************
      * LINE 1
           MOVE "LOANS   :"                     TO G-HEADING1.
           MOVE WS-GTT-NOLOANS  (GTT-TOTAL-SUB) TO G-LOANS.
           MOVE "SRVCHRG :"                     TO G-HEADING2.
           MOVE WS-GTT-SERCHG   (GTT-TOTAL-SUB) TO G-AMOUNT-2.
           MOVE "OUR-DL  :"                     TO G-HEADING3.
           MOVE WS-GTT-DL       (GTT-TOTAL-SUB) TO G-AMOUNT-3.
           MOVE "OUR-O1  :"                     TO G-HEADING4.
           MOVE WS-GTT-O1       (GTT-TOTAL-SUB) TO G-AMOUNT-4.
           MOVE "EARNINGS:"                     TO G-HEADING5.
           MOVE WS-GTT-EARNINGS (GTT-TOTAL-SUB) TO G-AMOUNT-5.
           MOVE 2                               TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      * LINE 2
           MOVE "LOAN AMT:"                     TO G-HEADING1.
           MOVE WS-GTT-LNAMT    (GTT-TOTAL-SUB) TO G-AMOUNT-1.
           MOVE "EXTCHG  :"                     TO G-HEADING2.
           MOVE WS-GTT-EXTCHG   (GTT-TOTAL-SUB) TO G-AMOUNT-2.
           MOVE "OUR-LL  :"                     TO G-HEADING3.
           MOVE WS-GTT-LL       (GTT-TOTAL-SUB) TO G-AMOUNT-3.
           MOVE "OUR-O2  :"                     TO G-HEADING4.
           MOVE WS-GTT-O2       (GTT-TOTAL-SUB) TO G-AMOUNT-4.
           MOVE "PROCASH :"                     TO G-HEADING5.
           MOVE WS-GTT-PROCASH  (GTT-TOTAL-SUB) TO G-AMOUNT-5.
           PERFORM WRITE-DETAIL-LINE.

      * LINE 3
           MOVE "INTCHG  :"                     TO G-HEADING1.
           MOVE WS-GTT-INTCHG   (GTT-TOTAL-SUB) TO G-AMOUNT-1.
           MOVE "MAINTFEE:"                     TO G-HEADING2.
           MOVE WS-GTT-MFEE     (GTT-TOTAL-SUB) TO G-AMOUNT-2.
           MOVE "OUR-AH  :"                     TO G-HEADING3.
           MOVE WS-GTT-AH       (GTT-TOTAL-SUB) TO G-AMOUNT-3.
           MOVE "OUR-O3  :"                     TO G-HEADING4.
           MOVE WS-GTT-O3       (GTT-TOTAL-SUB) TO G-AMOUNT-4.
           MOVE "PROCEEDS:"                     TO G-HEADING5.
           MOVE WS-GTT-PROCEEDS (GTT-TOTAL-SUB) TO G-AMOUNT-5.
           PERFORM WRITE-DETAIL-LINE.

      * LINE 4
      *- - - 8% SERVICE  - - - - - - - - - -
           IF ( HOLD-STATE = "GA" )
              MOVE "8% SRVCH:"                     TO G-HEADING1
              MOVE WS-GTT-STATE    (GTT-TOTAL-SUB) TO G-AMOUNT-1
              MOVE "CLOSING :"                     TO G-HEADING2
              MOVE WS-GTT-POINTS   (GTT-TOTAL-SUB) TO G-AMOUNT-2.

           MOVE "OUR-PP  :"                        TO G-HEADING3.
           MOVE WS-GTT-PP          (GTT-TOTAL-SUB) TO G-AMOUNT-3.
           MOVE "OUR FEES:"                        TO G-HEADING4.
           MOVE WS-GTT-FEES        (GTT-TOTAL-SUB) TO G-AMOUNT-4.
           MOVE "REN PRIN:"                        TO G-HEADING5.
           MOVE WS-GTT-RENEW-PRIN  (GTT-TOTAL-SUB) TO G-AMOUNT-5.
           PERFORM WRITE-DETAIL-LINE.

           IF ( WS-HOLD-PROMPT = "GROUP :" )
              MOVE ALL "-" TO DETAIL-LINE
              PERFORM WRITE-DETAIL-LINE.

      * CLEAR LEVEL IN TABLE

           MOVE LOW-VALUES TO WS-GTT-TABLE-GROUP-LEVEL(GTT-TOTAL-SUB).

       EXIT-GTT-PRINT-TOTALS.
           EXIT.


      ******************************************************************
      *
      *     C O P Y - L I B R A R Y - S E C T I O N S
      *
      ******************************************************************
       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/TIMEIO.CPY".
       COPY "LIBGB/GRSERCH.CPY".
       COPY "LIBGB/GRTOTALS.CPY".
       COPY "LIBWI/GRTOTL.CPY".
       COPY "LIBGB/GRWORK.CPY".
       COPY "LIBGB/BRSECURE.CPY".
       COPY "LIBGB/EOSPOOL.CPY".

      **************************************************************************
       FORM-RESET SECTION.
      **************************************************************************
           MOVE EXT-CONAME-CONAME TO EMLNRG-CONAME.
           MOVE EXT-CONAME-SYSDATE TO EMLNRG-SYSDATE.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DISPLAY EMLNRG-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE EMLNRG-FORM-FIELDS TO WR-BUF.
           MOVE EMLNRG-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      **************************************************************************
       SETUP-LINK-INFO SECTION.
      **************************************************************************
           MOVE EMLNRG-HELP--FILE TO HELP-LINK-FILE.
           MOVE EMLNRG-HELP--DIR TO HELP-LINK-DIR.

      **************************************************************************
       VDU-CONVERT-FIELD SECTION.
      **************************************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBLP/EMLNRG_EVA.CPY".
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
      *-----------------------------------------------------------------
       SET-BRANCH-HEADINGS.
           MOVE HOLD-BRNO TO H-BRNO.
           MOVE BR-NAME TO H-BRDESC.
      *-----------------------------------------------------------------
       PRINT-DEBIT-CARD-NUMBER.
           MOVE LN-OWNBR  TO LTD-BRNO.
           MOVE LN-ACCTNO TO LTD-ACCTNO.
           MOVE 1         TO LTD-SEQNO.
      *    PERFORM READ-LTD1-FILE.
      *    IF IO-FG = 0
           IF LTD-DEBIT-CARD-NUMBER NOT = 0
              MOVE LTD-DEBIT-CARD-NUMBER TO D-DEBIT-CARD.
       SEND-LEGEND.
           MOVE LEGEND TO WR-BUF.
           MOVE MSEL-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      *-----------------------------------------------------------------
       CLEAR-TOTAL-TBL.
           MOVE 0 TO T-NOLOANS(SUB) T-LNAMT(SUB) T-INTCHG(SUB)
                     T-SERCHG(SUB) T-EXTCHG(SUB) T-MFEE(SUB)
                     T-POINTS(SUB) T-STATE(SUB) T-EARNINGS(SUB)
                     T-DL(SUB) T-LL(SUB)
                     T-AH(SUB) T-PP(SUB) T-O1(SUB) T-O2(SUB) T-FEES(SUB)
                     T-WRLD-NONFILE-90(SUB) T-PROCEEDS(SUB)
                     T-PROCASH(SUB) T-O3(SUB) T-RENEW-PRIN(SUB).

      *-----------------------------------------------------------------
       CAT-CSZ.
           INSPECT C-S-Z REPLACING FIRST "      " BY ", ++ +".
           INSPECT C-S-Z REPLACING FIRST "++" BY STATE.
           INSPECT C-S-Z REPLACING FIRST "+         " BY ZIP.

      *-----------------------------------------------------------------
       GET-CODE.
           MOVE "GC"    TO CDB-TYPE.
           MOVE TL-BRNO TO CDB-BRANCH.
      *--> SET BEFORE      CDB-CODE.
      *    PERFORM OPEN-CDB1-FILE.
      *    PERFORM READ-CDB1-FILE.
      *    PERFORM CLOSE-CDB1-FILE.
      *    IF ( IO-FG NOT = 0 )

           IF CDB-DESC = SPACES
              MOVE 0 TO CDB-GC-GLFEENO
              MOVE SPACES TO CDB-DESC.

           MOVE CDB-DESC TO D-FEEGLNO.

      *-----------------------------------------------------------------
       UPDATE-TABLE.
           IF TL-LNAMT < 0
              SUBTRACT 1 FROM T-NOLOANS(1)
           ELSE
              ADD 1 TO T-NOLOANS(1).

           ADD TL-LNAMT TO T-LNAMT(LEV).
           ADD TL-INTCHG TO T-INTCHG(LEV).
           ADD TL-SERCHG TO T-SERCHG(LEV).
           ADD TL-EXTCHG TO T-EXTCHG(LEV).
           COMPUTE T-MFEE(LEV) ROUNDED = T-MFEE(LEV)
                                       + (TL-MAINTFEE * TL-ORGTERM).

           IF HOLD-STATE = "GA"
              ADD TL-STATE-WORKER TO T-STATE(LEV)
              IF NOT (TL-POINTS = 20.20 OR 02.02)
                 ADD TL-POINTS TO T-POINTS(LEV).

           COMPUTE T-EARNINGS(LEV) = T-EARNINGS(LEV) +
                                     TL-EARNINGS(1) + TL-EARNINGS(2) +
                                     TL-EARNINGS(3) + TL-EARNINGS(4) +
                                     TL-EARNINGS(5) + TL-EARNINGS(6).

           IF TL-INSTYPES-DL
              IF TL-INSOURS(1) = "Y"
                 ADD TL-INSPREM(1) TO T-DL(LEV).

           IF NOT TL-INSTYPES-DL
              IF TL-INSOURS(1) = "Y"
                 ADD TL-INSPREM(1) TO T-LL(LEV).

           IF TL-INSOURS(2) = "Y"
              ADD TL-INSPREM(2) TO T-AH(LEV).

           IF TL-INSOURS(3) = "Y"
              ADD TL-INSPREM(3) TO T-PP(LEV).

           IF TL-INSOURS(4) = "Y"
              ADD TL-INSPREM(4) TO T-O1(LEV).

           IF TL-INSOURS(5) = "Y"
              ADD TL-INSPREM(5) TO T-O2(LEV).

           IF TL-INSOURS(6) = "Y"
              ADD TL-INSPREM(6) TO T-O3(LEV).

           PERFORM VARYING FEE-SUB FROM 1 BY 1 UNTIL FEE-SUB > 10
              IF TL-FEECODE(FEE-SUB) = 90
                 ADD TL-FEEAMT(FEE-SUB) TO T-WRLD-NONFILE-90(LEV)
              END-IF
           END-PERFORM.

           ADD TL-FEEAMT(1) TL-FEEAMT(2) TL-FEEAMT(3)
               TL-FEEAMT(4) TL-FEEAMT(5) TL-FEEAMT(6)
               TL-FEEAMT(7) TL-FEEAMT(8) TL-FEEAMT(9)
               TL-FEEAMT(10) TO T-FEES(LEV).

           ADD TL-PROCEEDS TO T-PROCEEDS(LEV).
           ADD TL-PROCASH TO T-PROCASH(LEV).
           COMPUTE T-RENEW-PRIN(LEV) = T-RENEW-PRIN(LEV)
                                     + TL-PRBAL
                                     + TL-PRINTDUE
                                     - TL-PRFINCHGRB
                                     - TL-PRINSRB.

      *-----------------------------------------------------------------
       UPDATE-TABLE2.
           ADD T-LNAMT(SUB) TO T-LNAMT(LEV).
           ADD T-INTCHG(SUB) TO T-INTCHG(LEV).
           ADD T-SERCHG(SUB) TO T-SERCHG(LEV).
           ADD T-EXTCHG(SUB) TO T-EXTCHG(LEV).
           ADD T-MFEE(SUB) TO T-MFEE(LEV).
           ADD T-POINTS(SUB) TO T-POINTS(LEV).
           ADD T-STATE(SUB) TO T-STATE(LEV).
           ADD T-EARNINGS(SUB) TO T-EARNINGS(LEV).
           ADD T-LL(SUB) TO T-LL(LEV).
           ADD T-DL(SUB) TO T-DL(LEV).
           ADD T-AH(SUB) TO T-AH(LEV).
           ADD T-PP(SUB) TO T-PP(LEV).
           ADD T-O1(SUB) TO T-O1(LEV).
           ADD T-O2(SUB) TO T-O2(LEV).
           ADD T-O3(SUB) TO T-O3(LEV).
           ADD T-FEES(SUB) TO T-FEES(LEV).
           ADD T-WRLD-NONFILE-90(SUB) TO T-WRLD-NONFILE-90(LEV).
           ADD T-PROCEEDS(SUB) TO T-PROCEEDS(LEV).
           ADD T-PROCASH(SUB) TO T-PROCASH(LEV).
           ADD T-RENEW-PRIN(SUB) TO T-RENEW-PRIN(LEV).

      *-----------------------------------------------------------------
       TYPE-TOTALS.
           MOVE 1 TO LEV.
           MOVE "CLASS  TOTALS:" TO G-TOTNAME.
           PERFORM PRINT-SUMMARY.

      *-----------------------------------------------------------------
       PRINT-SUMMARY.
      * LINE 1
           MOVE "LOANS   :"      TO G-HEADING1.
           MOVE T-NOLOANS  (LEV) TO G-LOANS.
           MOVE "SRVCHRG :"      TO G-HEADING2.
           MOVE T-SERCHG   (LEV) TO G-AMOUNT-2.
           MOVE "OUR-DL  :"      TO G-HEADING3.
           MOVE T-DL       (LEV) TO G-AMOUNT-3.
           MOVE "OUR-O1  :"      TO G-HEADING4.
           MOVE T-O1       (LEV) TO G-AMOUNT-4.
           MOVE "EARNINGS:"      TO G-HEADING5.
           MOVE T-EARNINGS (LEV) TO G-AMOUNT-5.
           MOVE 2                TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      * LINE 2
           MOVE "LOAN AMT:"      TO G-HEADING1.
           MOVE T-LNAMT    (LEV) TO G-AMOUNT-1.
           MOVE "EXTCHG  :"      TO G-HEADING2.
           MOVE T-EXTCHG   (LEV) TO G-AMOUNT-2.
           MOVE "OUR-LL  :"      TO G-HEADING3.
           MOVE T-LL       (LEV) TO G-AMOUNT-3.
           MOVE "OUR-O2  :"      TO G-HEADING4.
           MOVE T-O2       (LEV) TO G-AMOUNT-4.
           MOVE "PROCASH :"      TO G-HEADING5.
           MOVE T-PROCASH  (LEV) TO G-AMOUNT-5.
           PERFORM WRITE-DETAIL-LINE.

      * LINE 3
           MOVE "INTCHG  :"      TO G-HEADING1.
           MOVE T-INTCHG   (LEV) TO G-AMOUNT-1.
           MOVE "MAINTFEE:"      TO G-HEADING2.
           MOVE T-MFEE     (LEV) TO G-AMOUNT-2.
           MOVE "OUR-AH  :"      TO G-HEADING3.
           MOVE T-AH       (LEV) TO G-AMOUNT-3.
           MOVE "OUR-O3  :"      TO G-HEADING4.
           MOVE T-O3       (LEV) TO G-AMOUNT-4.
           MOVE "PROCEEDS:"      TO G-HEADING5.
           MOVE T-PROCEEDS (LEV) TO G-AMOUNT-5.
           PERFORM WRITE-DETAIL-LINE.

      * LINE 4
      *- - - 8% SERVICE  - - - - - - - - - -
           IF ( HOLD-STATE = "GA" )
              MOVE "8% SRVCH:"      TO G-HEADING1
              MOVE T-STATE    (LEV) TO G-AMOUNT-1
              MOVE "CLOSING :"      TO G-HEADING2
              MOVE T-POINTS   (LEV) TO G-AMOUNT-2.

           MOVE "OUR-PP  :"         TO G-HEADING3.
           MOVE T-PP          (LEV) TO G-AMOUNT-3.
           MOVE "OUR FEES:"         TO G-HEADING4.
           MOVE T-FEES        (LEV) TO G-AMOUNT-4.
           MOVE "REN PRIN:"         TO G-HEADING5.
           MOVE T-RENEW-PRIN  (LEV) TO G-AMOUNT-5.
           PERFORM WRITE-DETAIL-LINE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE 1 TO LN-FEED.

      *-----------------------------------------------------------------
       PRINT-GRAND-TOTALS.
           MOVE "GRAND  TOTALS:" TO G-TOTNAME.
      * LINE 1
           MOVE "LOANS   :"      TO G-HEADING1.
           MOVE T-NOLOANS  (LEV) TO G-LOANS.
           MOVE "SRVCHRG :"      TO G-HEADING2.
           MOVE T-SERCHG   (LEV) TO G-AMOUNT-2.
           MOVE "OUR-DL  :"      TO G-HEADING3.
           MOVE T-DL       (LEV) TO G-AMOUNT-3.
           MOVE "OUR-O1  :"      TO G-HEADING4.
           MOVE T-O1       (LEV) TO G-AMOUNT-4.
           MOVE "EARNINGS:"      TO G-HEADING5.
           MOVE T-EARNINGS (LEV) TO G-AMOUNT-5.
           MOVE 2                TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      * LINE 2
           MOVE "LOAN AMT:"      TO G-HEADING1.
           MOVE T-LNAMT    (LEV) TO G-AMOUNT-1.
           MOVE "EXTCHG  :"      TO G-HEADING2.
           MOVE T-EXTCHG   (LEV) TO G-AMOUNT-2.
           MOVE "OUR-LL  :"      TO G-HEADING3.
           MOVE T-LL       (LEV) TO G-AMOUNT-3.
           MOVE "OUR-O2  :"      TO G-HEADING4.
           MOVE T-O2       (LEV) TO G-AMOUNT-4.
           MOVE "PROCASH :"      TO G-HEADING5.
           MOVE T-PROCASH  (LEV) TO G-AMOUNT-5.
           PERFORM WRITE-DETAIL-LINE.

      * LINE 3
           MOVE "INTCHG  :"      TO G-HEADING1.
           MOVE T-INTCHG   (LEV) TO G-AMOUNT-1.
           MOVE "MAINTFEE:"      TO G-HEADING2.
           MOVE T-MFEE     (LEV) TO G-AMOUNT-2.
           MOVE "OUR-AH  :"      TO G-HEADING3.
           MOVE T-AH       (LEV) TO G-AMOUNT-3.
           MOVE "OUR-O3  :"      TO G-HEADING4.
           MOVE T-O3       (LEV) TO G-AMOUNT-4.
           MOVE "PROCEEDS:"      TO G-HEADING5.
           MOVE T-PROCEEDS (LEV) TO G-AMOUNT-5.
           PERFORM WRITE-DETAIL-LINE.

      * LINE 4
      *- - - 8% SERVICE  - - - - - - - - - -
           IF ( HOLD-STATE = "GA" )
              MOVE "8% SRVCH:"      TO G-HEADING1
              MOVE T-STATE    (LEV) TO G-AMOUNT-1
              MOVE "CLOSING :"      TO G-HEADING2
              MOVE T-POINTS   (LEV) TO G-AMOUNT-2.

           MOVE "OUR-PP  :"         TO G-HEADING3.
           MOVE T-PP          (LEV) TO G-AMOUNT-3.
           MOVE "OUR FEES:"         TO G-HEADING4.
           SUBTRACT T-WRLD-NONFILE-90(LEV) FROM T-FEES(LEV)
              GIVING                   G-AMOUNT-4.
           MOVE "REN PRIN:"         TO G-HEADING5.
           MOVE T-RENEW-PRIN  (LEV) TO G-AMOUNT-5.
           PERFORM WRITE-DETAIL-LINE.

      *  LINE 5
           MOVE "NONFILE :"            TO G-HEADING4.
           MOVE T-WRLD-NONFILE-90(LEV) TO G-AMOUNT-4.
           PERFORM WRITE-DETAIL-LINE.

      *-----------------------------------------------------------------
       GET-CREDIT-LIFE-TAG.
           IF TL-INSTYPES-DL
              MOVE "D" TO D-INSTYPE
              IF TL-INSURED = "M" OR "S"
                 MOVE "S" TO D-INSFLAG
              ELSE
                 MOVE "J" TO D-INSFLAG
           ELSE
              MOVE "L" TO D-INSTYPE
              IF TL-INSURED = "M" OR "S"
                 MOVE "S" TO D-INSFLAG
              ELSE
                 MOVE "J" TO D-INSFLAG.

      *-----------------------------------------------------------------
       PRINT-LT-REC-COL.
           IF PRCOL-FG = "Y"
              MOVE LN-OWNBR  TO LTC-BRNO
              MOVE LN-ACCTNO TO LTC-ACCTNO
              MOVE 1         TO LTC-SEQNO
      *       PERFORM READ-LTC1-FILE
      *       IF IO-FG = 0
	      IF LTC-MAKE(1)  NOT   = SPACES OR
                 LTC-MODEL(1) NOT   = SPACES OR
                 LTC-YEAR(1)  NOT   = SPACES OR
                 LTC-VIN(1)   NOT   = SPACES
                 MOVE "MAKE : "    TO D-FIL-MAKE
                 MOVE "MODEL : "   TO D-FIL-MODEL
                 MOVE "YEAR : "    TO D-FIL-YEAR
                 MOVE "VIN # : "   TO D-FIL-ID
                 MOVE LTC-MAKE(1)  TO D-MAKE
                 MOVE LTC-MODEL(1) TO D-MODEL
                 MOVE LTC-YEAR(1)  TO D-YEAR
                 MOVE LTC-VIN(1)   TO D-ID
                 ADD 1 TO DTL-SUB
                 MOVE DETAIL-LINE  TO DTL-LINE(DTL-SUB)
                 MOVE SPACES       TO DETAIL-LINE
              ELSE
                 MOVE SPACES       TO LTC-AUTO(2).

           IF PRCOL-FG = "Y"
              IF LTC-MAKE(2)  NOT   = SPACES OR 
                 LTC-MODEL(2) NOT   = SPACES OR 
                 LTC-YEAR(2)  NOT   = SPACES OR
                 LTC-VIN(2)   NOT   = SPACES
                 MOVE "MAKE : "    TO D-FIL-MAKE
                 MOVE "MODEL : "   TO D-FIL-MODEL
                 MOVE "YEAR : "    TO D-FIL-YEAR
                 MOVE "VIN # : "   TO D-FIL-ID
                 MOVE LTC-MAKE(2)  TO D-MAKE
                 MOVE LTC-MODEL(2) TO D-MODEL
                 MOVE LTC-YEAR(2)  TO D-YEAR
                 MOVE LTC-VIN(2)   TO D-ID
                 ADD 1             TO DTL-SUB
                 MOVE DETAIL-LINE  TO DTL-LINE(DTL-SUB)
                 MOVE SPACES       TO DETAIL-LINE.

      ******************************************************************
       IO-ROUTINES SECTION.
      ******************************************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBLP/LPTRLGS_SQL.CPY".
       COPY "LIBLP/LPLTDGS_SQL.CPY".
       COPY "LIBLP/LPLTCGS_SQL.CPY".
       COPY "LIBLP/LPLNGS_SQL.CPY".
       COPY "LIBLP/LPLCGS_SQL.CPY".
       COPY "LIBLP/LPCDBGS_SQL.CPY".
       COPY "LIBLP/LPBWGS_SQL.CPY".
       COPY "LIBGB/GBGRGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBPRINT.CPY".
       COPY "LIBLP/LPCDB1RN.CPY".
       COPY "LIBLP/LPLN1RN.CPY".
       COPY "LIBLP/LPBW1RN.CPY".
       COPY "LIBGB/GBGR1RN.CPY".
       COPY "LIBGB/GBBRRN.CPY".
  

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

      *-----------------------------------------------------------------
       OPEN-TRL1-FILE.
           PERFORM LOAD-TRL1-FILE.
           PERFORM OPEN-IT.
           MOVE TRL-PATH-SQL TO E-FILE.

           IF TRL1-CONN-OPEN-SW = "N"
               EXEC SQL CONNECT TO :SQL-CONNECT-SERVER AS TRL1_CONNECT
                        USER :SQL-CONNECT-USERID USING :SQL-CONNECT-PSWD
               END-EXEC
               EXEC SQL SET CONNECTION C1 END-EXEC
               MOVE 'Y' TO TRL1-CONN-OPEN-SW.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       START-TRL1-FILE. *> SSP - TRL,LN,LTC,LTD,BW,CDB
           PERFORM START-IT.
           MOVE TRL-PATH-OWNBR  TO TL-BRNO.
           MOVE TRL-PATH-SQL    TO E-FILE.
           MOVE TRL1-KEY        TO E-KEYX.

           IF ( TRL1-CURSOR-STAT-GOOD )
              PERFORM CLOSE-TRL1-FILE.

      *     IF ( QTRL1-WEND-LINE = ZEROES    ) OR
      *        ( QTRL1-WEND-LINE NOT NUMERIC )
      *        MOVE TRL-PATH-OWNBR        TO QTRL1-WBEG-BRNO
      *        MOVE TRL-PATH-OWNBR        TO QTRL1-WEND-BRNO
      *        MOVE TL-DATE               TO SQL-DATE-YYYYMMDD
      *        PERFORM SQL-SET-DATE
      *        MOVE SQL-DATE-YYYY-MM-DD  TO QTRL1-WBEG-DATE
      *        MOVE JULIAN-END           TO SQL-DATE-YYYYMMDD
      *        PERFORM SQL-SET-DATE
      *        MOVE SQL-DATE-YYYY-MM-DD  TO QTRL1-WEND-DATE
      *        MOVE TL-CLASS             TO QTRL1-WBEG-CLASS
      *        MOVE TL-ACCTNO            TO QTRL1-WBEG-ACCTNO
      *        MOVE TL-LAWCODE           TO QTRL1-WBEG-LAWCODE
      *        MOVE TL-LINE              TO QTRL1-WBEG-LINE
      *        MOVE ALL "9"              TO QTRL1-WEND-CLASS
      *                                     QTRL1-WEND-ACCTNO
      *                                     QTRL1-WEND-LAWCODE
      *                                     QTRL1-WEND-LINE.

      * STORED PROCEDURE:         
      *                                          
      *   SELECT TRLFILE.TL_BRNO,
      *          CAST(TRLFILE.TL_DATE AS VARCHAR(10)), 
      *          TRLFILE.TL_CLASS,
      *          TRLFILE.TL_ACCTNO,
      *          TRLFILE.TL_LAWCODE,
      *          TRLFILE.TL_LINE,
      *          TRLFILE.TL_SSNO_1,
      *          TRLFILE.TL_SSNO_2,
      *          TRLFILE.TL_SSNO_3,
      *          TRLFILE.TL_SSNO_4,
      *          TRLFILE.TL_MAKERCD_1, 
      *          TRLFILE.TL_MAKERCD_2, 
      *          TRLFILE.TL_MAKERCD_3, 
      *          TRLFILE.TL_MAKERCD_4, 
      *          TRLFILE.TL_PURCD,  
      *          TRLFILE.TL_SRCD,  
      *          TRLFILE.TL_COLLCD, 
      *          CAST(TRLFILE.TL_LOANDATE AS VARCHAR(10)), 
      *          TRLFILE.TL_PROCEEDS,
      *          TRLFILE.TL_LNAMT,
      *          TRLFILE.TL_FINCHG,
      *          TRLFILE.TL_INTCHG,
      *          TRLFILE.TL_SERCHG,
      *          TRLFILE.TL_EXTCHG,
      *          TRLFILE.TL_MAINTFEE,
      *          TRLFILE.TL_ORGTERM,
      *          TRLFILE.TL_PROCASH,
      *          TRLFILE.TL_TILLNO,
      *          TRLFILE.TL_FEECODE_1, 
      *          TRLFILE.TL_FEECODE_2, 
      *          TRLFILE.TL_FEECODE_3, 
      *          TRLFILE.TL_FEECODE_4, 
      *          TRLFILE.TL_FEECODE_5, 
      *          TRLFILE.TL_FEECODE_6, 
      *          TRLFILE.TL_FEECODE_7, 
      *          TRLFILE.TL_FEECODE_8, 
      *          TRLFILE.TL_FEECODE_9, 
      *          TRLFILE.TL_FEECODE_10, 
      *          TRLFILE.TL_FEEAMT_1,
      *          TRLFILE.TL_FEEAMT_2,
      *          TRLFILE.TL_FEEAMT_3,
      *          TRLFILE.TL_FEEAMT_4,
      *          TRLFILE.TL_FEEAMT_5,
      *          TRLFILE.TL_FEEAMT_6,
      *          TRLFILE.TL_FEEAMT_7,
      *          TRLFILE.TL_FEEAMT_8,
      *          TRLFILE.TL_FEEAMT_9,
      *          TRLFILE.TL_FEEAMT_10,
      *          TRLFILE.TL_INSURED,
      *          TRLFILE.TL_INSTYPES,
      *          TRLFILE.TL_INSOURS_1, 
      *          TRLFILE.TL_INSOURS_2, 
      *          TRLFILE.TL_INSOURS_3, 
      *          TRLFILE.TL_INSOURS_4, 
      *          TRLFILE.TL_INSOURS_5, 
      *          TRLFILE.TL_INSOURS_6, 
      *          TRLFILE.TL_INSOURS_7, 
      *          TRLFILE.TL_INSOURS_8, 
      *          TRLFILE.TL_INSOURS_9, 
      *          TRLFILE.TL_INSOURS_10, 
      *          TRLFILE.TL_INSPREM_1,
      *          TRLFILE.TL_INSPREM_2,
      *          TRLFILE.TL_INSPREM_3,
      *          TRLFILE.TL_INSPREM_4,
      *          TRLFILE.TL_INSPREM_5,
      *          TRLFILE.TL_INSPREM_6,
      *          TRLFILE.TL_INSPREM_7,
      *          TRLFILE.TL_INSPREM_8,
      *          TRLFILE.TL_INSPREM_9,
      *          TRLFILE.TL_INSPREM_10,
      *          TRLFILE.TL_DLNO,
      *          TRLFILE.TL_PRLNNO,
      *          TRLFILE.TL_PRBAL,
      *          TRLFILE.TL_PRINTDUE,
      *          TRLFILE.TL_PRFINCHGRB,
      *          TRLFILE.TL_PRINSRB,
      *          TRLFILE.TL_STATE_WORKER,
      *          TRLFILE.TL_USERID,
      *          TRLFILE.TL_POSTTIME,
      *          TRLFILE.TL_APR,
      *          TRLFILE.TL_POINTS,
      *          TRLFILE.TL_DEBIT_CARD_AMT,
      *          TRLFILE.TL_EARNINGS_1,          
      *          TRLFILE.TL_EARNINGS_2,          
      *          TRLFILE.TL_EARNINGS_3,          
      *          TRLFILE.TL_EARNINGS_4,          
      *          TRLFILE.TL_EARNINGS_5,          
      *          TRLFILE.TL_EARNINGS_6,  
      *          ISNULL(LNFILE.LN_OWNBR, ' '),
      *          ISNULL(LNFILE.LN_ACCTNO, 0),
      *          ISNULL(LNFILE.LN_APRATE, 0),
      *          ISNULL(LNFILE.LN_CRSCORE, 0),
      *          ISNULL(LTCFILE.LTC_BRNO, 0),
      *          ISNULL(LTCFILE.LTC_ACCTNO, 0),
      *          ISNULL(LTCFILE.LTC_SEQNO, 0),
      *          ISNULL(LTCFILE.LTC_DESC_1,''),
      *          ISNULL(LTCFILE.LTC_MAKE_1,''),
      *          ISNULL(LTCFILE.LTC_MODEL_1,''),
      *          ISNULL(LTCFILE.LTC_YEAR_1,''),
      *          ISNULL(LTCFILE.LTC_STYLE_1,''),
      *          ISNULL(LTCFILE.LTC_CLASS_1,''),
      *          ISNULL(LTCFILE.LTC_VIN_1,''),
      *          ISNULL(LTCFILE.LTC_TITLE_NO_1,''),
      *          ISNULL(LTCFILE.LTC_MILEAGE_1, 0),
      *          ISNULL(LTCFILE.LTC_VALUE_1, 0),
      *          ISNULL(LTCFILE.LTC_SELLPR_1, 0),
      *          ISNULL(LTCFILE.LTC_MISC_AMT_1, 0),
      *          ISNULL(LTCFILE.LTC_SENT_FLAG_1,''),
      *          ISNULL(CAST(LTCFILE.LTC_VIN_CHG_DATE_1
      *                                         AS VARCHAR(10)),'0'),
      *          ISNULL(LTCFILE.LTC_DESC_2,''),
      *          ISNULL(LTCFILE.LTC_MAKE_2,''),
      *          ISNULL(LTCFILE.LTC_MODEL_2,''),
      *          ISNULL(LTCFILE.LTC_YEAR_2,''),
      *          ISNULL(LTCFILE.LTC_STYLE_2,''),
      *          ISNULL(LTCFILE.LTC_CLASS_2,''),
      *          ISNULL(LTCFILE.LTC_VIN_2,''),
      *          ISNULL(LTCFILE.LTC_TITLE_NO_2,''),
      *          ISNULL(LTCFILE.LTC_MILEAGE_2, 0),
      *          ISNULL(LTCFILE.LTC_VALUE_2, 0),
      *          ISNULL(LTCFILE.LTC_SELLPR_2, 0),
      *          ISNULL(LTCFILE.LTC_MISC_AMT_2, 0),
      *          ISNULL(LTCFILE.LTC_SENT_FLAG_2,''),
      *          ISNULL(CAST(LTCFILE.LTC_VIN_CHG_DATE_2
      *                                         AS VARCHAR(10)),'0'),
      *          ISNULL(LTDFILE.LTD_BRNO, 0),
      *          ISNULL(LTDFILE.LTD_ACCTNO, 0),
      *          ISNULL(LTDFILE.LTD_SEQNO, 0),
      *          ISNULL(LTDFILE.LTD_DEBIT_CARD_NUMBER, 0),
      *          ISNULL(BWFILE1.BW_OWNBR, ' '),
      *          ISNULL(BWFILE1.BW_SSNO, 0),
      *          ISNULL(BWFILE1.BW_LNAME,''),
      *          ISNULL(BWFILE1.BW_FNAME,''),
      *          ISNULL(BWFILE1.BW_ADR1,''),
      *          ISNULL(BWFILE1.BW_ADR2,''),
      *          ISNULL(BWFILE1.BW_CITY,''),
      *          ISNULL(BWFILE1.BW_STATE,''),
      *          ISNULL(BWFILE1.BW_ZIP,''),
      *          ISNULL(BWFILE1.BW_PHONE, 0),
      *          ISNULL(BWFILE1.BW_EMPPHONE, 0),
      *          ISNULL(BWFILE1.BW_EMPEXT, 0),
      *          ISNULL(BWFILE1.BW_CREDIT_SCORE, 0),
      *          ISNULL(BWFILE2.BW_OWNBR, ' '),
      *          ISNULL(BWFILE2.BW_SSNO, 0),
      *          ISNULL(BWFILE2.BW_LNAME,''),
      *          ISNULL(BWFILE2.BW_FNAME,''),
      *          ISNULL(BWFILE2.BW_ADR1,''),
      *          ISNULL(BWFILE2.BW_ADR2,''),
      *          ISNULL(BWFILE2.BW_CITY,''),
      *          ISNULL(BWFILE2.BW_STATE,''),
      *          ISNULL(BWFILE2.BW_ZIP,''),
      *          ISNULL(BWFILE2.BW_PHONE, 0),
      *          ISNULL(BWFILE2.BW_EMPPHONE, 0),
      *          ISNULL(BWFILE2.BW_EMPEXT, 0),
      *          ISNULL(BWFILE2.BW_CREDIT_SCORE, 0),
      *          ISNULL(CDBFILE1.CDB_TYPE,''),
      *          ISNULL(CDBFILE1.CDB_BRANCH, 0),
      *          ISNULL(CDBFILE1.CDB_CODE,''),
      *          ISNULL(CDBFILE1.CDB_DESC,''),
      *          ISNULL(CDBFILE1.CDB_GC_GLFEENO, 0),
      *          ISNULL(CDBFILE2.CDB_TYPE,''),
      *          ISNULL(CDBFILE2.CDB_BRANCH, 0),
      *          ISNULL(CDBFILE2.CDB_CODE,''),
      *          ISNULL(CDBFILE2.CDB_DESC,''),
      *          ISNULL(CDBFILE2.CDB_GC_GLFEENO, 0),
      *          ISNULL(CDBFILE3.CDB_TYPE,''),
      *          ISNULL(CDBFILE3.CDB_BRANCH, 0),
      *          ISNULL(CDBFILE3.CDB_CODE,''),
      *          ISNULL(CDBFILE3.CDB_DESC,''),
      *          ISNULL(CDBFILE3.CDB_GC_GLFEENO, 0),
      *          ISNULL(CDBFILE4.CDB_TYPE,''),
      *          ISNULL(CDBFILE4.CDB_BRANCH, 0),
      *          ISNULL(CDBFILE4.CDB_CODE,''),
      *          ISNULL(CDBFILE4.CDB_DESC,''),
      *          ISNULL(CDBFILE4.CDB_GC_GLFEENO, 0),
      *          ISNULL(CDBFILE5.CDB_TYPE,''),
      *          ISNULL(CDBFILE5.CDB_BRANCH, 0),
      *          ISNULL(CDBFILE5.CDB_CODE,''),
      *          ISNULL(CDBFILE5.CDB_DESC,''),
      *          ISNULL(CDBFILE5.CDB_GC_GLFEENO, 0),
      *          ISNULL(CDBFILE6.CDB_TYPE,''),
      *          ISNULL(CDBFILE6.CDB_BRANCH, 0),
      *          ISNULL(CDBFILE6.CDB_CODE,''),
      *          ISNULL(CDBFILE6.CDB_DESC,''),
      *          ISNULL(CDBFILE6.CDB_GC_GLFEENO, 0),
      *          ISNULL(CDBFILE7.CDB_TYPE,''),
      *          ISNULL(CDBFILE7.CDB_BRANCH, 0),
      *          ISNULL(CDBFILE7.CDB_CODE,''),
      *          ISNULL(CDBFILE7.CDB_DESC,''),
      *          ISNULL(CDBFILE7.CDB_GC_GLFEENO, 0),
      *          ISNULL(CDBFILE8.CDB_TYPE,''),
      *          ISNULL(CDBFILE8.CDB_BRANCH, 0),
      *          ISNULL(CDBFILE8.CDB_CODE,''),
      *          ISNULL(CDBFILE8.CDB_DESC,''),
      *          ISNULL(CDBFILE8.CDB_GC_GLFEENO, 0),
      *          ISNULL(CDBFILE9.CDB_TYPE,''),
      *          ISNULL(CDBFILE9.CDB_BRANCH, 0),
      *          ISNULL(CDBFILE9.CDB_CODE,''),
      *          ISNULL(CDBFILE9.CDB_DESC,''),
      *          ISNULL(CDBFILE9.CDB_GC_GLFEENO, 0),
      *          ISNULL(CDBFILE10.CDB_TYPE,''),
      *          ISNULL(CDBFILE10.CDB_BRANCH, 0),
      *          ISNULL(CDBFILE10.CDB_CODE,''),
      *          ISNULL(CDBFILE10.CDB_DESC,''),
      *          ISNULL(CDBFILE10.CDB_GC_GLFEENO, 0)
      *
      *    FROM  DBO.TRLFILE TRLFILE
      *
      *    LEFT OUTER JOIN
      *          DBO.LNFILE  LNFILE 
      *      ON (TRLFILE.TL_BRNO        = LNFILE.LN_OWNBR
      *     AND  TRLFILE.TL_ACCTNO      = LNFILE.LN_ACCTNO)
      *
      *    LEFT OUTER JOIN
      *          DBO.LTCFILE LTCFILE 
      *      ON (LNFILE.LN_OWNBR        = LTCFILE.LTC_BRNO
      *     AND  LNFILE.LN_ACCTNO       = LTCFILE.LTC_ACCTNO
      *     AND  LTCFILE.LTC_SEQNO      = 1) 
      *
      *    LEFT OUTER JOIN
      *          DBO.LTDFILE LTDFILE 
      *      ON (LNFILE.LN_OWNBR        = LTDFILE.LTD_BRNO
      *     AND  LNFILE.LN_ACCTNO       = LTDFILE.LTD_ACCTNO
      *     AND  LTDFILE.LTD_SEQNO      = 1) 
      *
      *    LEFT OUTER JOIN
      *          DBO.BWFILE  BWFILE1 
      *      ON (TRLFILE.TL_BRNO        = BWFILE1.BW_OWNBR
      *     AND  TRLFILE.TL_SSNO_1      = BWFILE1.BW_SSNO)
      *
      *    LEFT OUTER JOIN
      *          DBO.BWFILE  BWFILE2 
      *      ON (TRLFILE.TL_BRNO        = BWFILE2.BW_OWNBR
      *     AND  TRLFILE.TL_SSNO_2      = BWFILE2.BW_SSNO)
      *
      *    LEFT OUTER JOIN
      *          DBO.CDBFILE CDBFILE1
      *      ON (TRLFILE.TL_BRNO        = CDBFILE1.CDB_BRANCH
      *     AND  'GC'                   = CDBFILE1.CDB_TYPE
      *     AND  TRLFILE.TL_FEECODE_1   = CDBFILE1.CDB_CODE) 
      *
      *    LEFT OUTER JOIN
      *          DBO.CDBFILE CDBFILE2
      *      ON (TRLFILE.TL_BRNO        = CDBFILE2.CDB_BRANCH
      *     AND  'GC'                   = CDBFILE2.CDB_TYPE
      *     AND  TRLFILE.TL_FEECODE_2   = CDBFILE2.CDB_CODE) 
      *
      *    LEFT OUTER JOIN
      *          DBO.CDBFILE CDBFILE3
      *      ON (TRLFILE.TL_BRNO        = CDBFILE3.CDB_BRANCH
      *     AND  'GC'                   = CDBFILE3.CDB_TYPE
      *     AND  TRLFILE.TL_FEECODE_3   = CDBFILE3.CDB_CODE) 
      *
      *    LEFT OUTER JOIN
      *          DBO.CDBFILE CDBFILE4
      *      ON (TRLFILE.TL_BRNO        = CDBFILE4.CDB_BRANCH
      *     AND  'GC'                   = CDBFILE4.CDB_TYPE
      *     AND  TRLFILE.TL_FEECODE_4   = CDBFILE4.CDB_CODE) 
      *
      *    LEFT OUTER JOIN
      *          DBO.CDBFILE CDBFILE5
      *      ON (TRLFILE.TL_BRNO        = CDBFILE5.CDB_BRANCH
      *     AND  'GC'                   = CDBFILE5.CDB_TYPE
      *     AND  TRLFILE.TL_FEECODE_5   = CDBFILE5.CDB_CODE) 
      *
      *    LEFT OUTER JOIN
      *          DBO.CDBFILE CDBFILE6
      *      ON (TRLFILE.TL_BRNO        = CDBFILE6.CDB_BRANCH
      *     AND  'GC'                   = CDBFILE6.CDB_TYPE
      *     AND  TRLFILE.TL_FEECODE_6   = CDBFILE6.CDB_CODE) 
      *
      *    LEFT OUTER JOIN
      *          DBO.CDBFILE CDBFILE7
      *      ON (TRLFILE.TL_BRNO        = CDBFILE7.CDB_BRANCH
      *     AND  'GC'                   = CDBFILE7.CDB_TYPE
      *     AND  TRLFILE.TL_FEECODE_7   = CDBFILE7.CDB_CODE) 
      *
      *    LEFT OUTER JOIN
      *          DBO.CDBFILE CDBFILE8
      *      ON (TRLFILE.TL_BRNO        = CDBFILE8.CDB_BRANCH
      *     AND  'GC'                   = CDBFILE8.CDB_TYPE
      *     AND  TRLFILE.TL_FEECODE_8   = CDBFILE8.CDB_CODE) 
      *
      *    LEFT OUTER JOIN
      *          DBO.CDBFILE CDBFILE9
      *      ON (TRLFILE.TL_BRNO        = CDBFILE9.CDB_BRANCH
      *     AND  'GC'                   = CDBFILE9.CDB_TYPE
      *     AND  TRLFILE.TL_FEECODE_9   = CDBFILE9.CDB_CODE) 
      *
      *    LEFT OUTER JOIN
      *          DBO.CDBFILE CDBFILE10
      *      ON (TRLFILE.TL_BRNO        = CDBFILE10.CDB_BRANCH
      *     AND  'GC'                   = CDBFILE10.CDB_TYPE
      *     AND  TRLFILE.TL_FEECODE_10  = CDBFILE10.CDB_CODE) 
      *
      *   WHERE  TRLFILE.TL_BRNO        = @QTRL1_WBEG_BRNO
      *     AND  TRLFILE.TL_DATE  BETWEEN @QTRL1_WBEG_DATE  AND @QTRL1_WEND_DATE
      *     AND  TRLFILE.TL_CLASS BETWEEN @QTRL1_WBEG_CLASS AND @QTRL1_WEND_CLASS
      * 
      *   ORDER BY TRLFILE.TL_BRNO,
      *            TRLFILE.TL_CLASS,
      *            TRLFILE.TL_ACCTNO,
      *            TRLFILE.TL_DATE,
      *            TRLFILE.TL_LAWCODE,
      *            TRLFILE.TL_LINE

           EXEC SQL SET CONNECTION TRL1_CONNECT END-EXEC.

           EXEC SQL
            DECLARE TRL1_CURSOR CURSOR FOR
               :TRL1-RETURN-CODE = EXEC SSP_EMLNRG_TRLFILE_SELECT
              (:QTRL1-WBEG-BRNO,  
               :QTRL1-WBEG-DATE,  
               :QTRL1-WEND-DATE,
               :QTRL1-WBEG-CLASS, 
               :QTRL1-WEND-CLASS)
           END-EXEC.

           EXEC SQL
               OPEN TRL1_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE ZEROES TO QTRL1-WBEG-BRNO
                          QTRL1-WBEG-CLASS
                          QTRL1-WEND-CLASS.
           MOVE SPACES TO QTRL1-WBEG-DATE
                          QTRL1-WEND-DATE.

           MOVE STAT---GOOD TO TRL1-CURSOR-STAT.

           EXEC SQL SET CONNECTION C1 END-EXEC.

      *-----------------------------------------------------------------
       READ-TRL1-FILE-NEXT. *> EMBEDDED - TRL,LN,LTC,LTD,BW,CDB
           PERFORM READ-IT.
           MOVE TRL-PATH-OWNBR  TO TL-BRNO.
           MOVE TRL-PATH-SQL    TO E-FILE.
           MOVE TRL1-KEY        TO E-KEYX.
           INITIALIZE QTRL-REC.
           INITIALIZE QLN-REC.
           INITIALIZE QLTC-REC.
           INITIALIZE QLTD-REC.
           INITIALIZE QBW-REC.
           INITIALIZE QCDB-REC.

           IF ( TRL1-CURSOR-STAT-GOOD )
              EXEC SQL SET CONNECTION TRL1_CONNECT END-EXEC
              EXEC SQL
                   FETCH TRL1_CURSOR INTO 
                     :QTL-BRNO,
                     :QTL-DATE, 
                     :QTL-CLASS,
                     :QTL-ACCTNO,
                     :QTL-LAWCODE,
                     :QTL-LINE,
                     :QTL-SSNO-1,
                     :QTL-SSNO-2,
                     :QTL-SSNO-3,
                     :QTL-SSNO-4,
                     :QTL-MAKERCD-1, 
                     :QTL-MAKERCD-2, 
                     :QTL-MAKERCD-3, 
                     :QTL-MAKERCD-4, 
                     :QTL-PURCD,  
                     :QTL-SRCD,  
                     :QTL-COLLCD, 
                     :QTL-LOANDATE, 
                     :QTL-PROCEEDS,
                     :QTL-LNAMT,
                     :QTL-FINCHG,
                     :QTL-INTCHG,
                     :QTL-SERCHG,
                     :QTL-EXTCHG,
                     :QTL-MAINTFEE,
                     :QTL-ORGTERM,
                     :QTL-PROCASH,
                     :QTL-TILLNO,
                     :QTL-FEECODE-1, 
                     :QTL-FEECODE-2, 
                     :QTL-FEECODE-3, 
                     :QTL-FEECODE-4, 
                     :QTL-FEECODE-5, 
                     :QTL-FEECODE-6, 
                     :QTL-FEECODE-7, 
                     :QTL-FEECODE-8, 
                     :QTL-FEECODE-9, 
                     :QTL-FEECODE-10, 
                     :QTL-FEEAMT-1,
                     :QTL-FEEAMT-2,
                     :QTL-FEEAMT-3,
                     :QTL-FEEAMT-4,
                     :QTL-FEEAMT-5,
                     :QTL-FEEAMT-6,
                     :QTL-FEEAMT-7,
                     :QTL-FEEAMT-8,
                     :QTL-FEEAMT-9,
                     :QTL-FEEAMT-10,
                     :QTL-INSURED,
                     :QTL-INSTYPES,
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
                     :QTL-DLNO,
                     :QTL-PRLNNO,
                     :QTL-PRBAL,
                     :QTL-PRINTDUE,
                     :QTL-PRFINCHGRB,
                     :QTL-PRINSRB,
                     :QTL-STATE-WORKER,
                     :QTL-USERID,
                     :QTL-POSTTIME,
                     :QTL-APR,
                     :QTL-POINTS,
                     :QTL-DEBIT-CARD-AMT,
                     :QTL-EARNINGS-1,       
                     :QTL-EARNINGS-2,       
                     :QTL-EARNINGS-3,         
                     :QTL-EARNINGS-4,        
                     :QTL-EARNINGS-5,        
                     :QTL-EARNINGS-6,        
                     :QLN-OWNBR,  
                     :QLN-ACCTNO,  
                     :QLN-APRATE,  
                     :QLN-CRSCORE, 
                     :QLTC-BRNO, 
                     :QLTC-ACCTNO,
                     :QLTC-SEQNO,  
                     :QLTC-DESC-1, 
                     :QLTC-MAKE-1, 
                     :QLTC-MODEL-1, 
                     :QLTC-YEAR-1, 
                     :QLTC-STYLE-1, 
                     :QLTC-CLASS-1, 
                     :QLTC-VIN-1, 
                     :QLTC-TITLE-NO-1, 
                     :QLTC-MILEAGE-1,  
                     :QLTC-VALUE-1,  
                     :QLTC-SELLPR-1, 
                     :QLTC-MISC-AMT-1,
                     :QLTC-SENT-FLAG-1,
                     :QLTC-VIN-CHG-DATE-1,
                     :QLTC-DESC-2,
                     :QLTC-MAKE-2,
                     :QLTC-MODEL-2,
                     :QLTC-YEAR-2,
                     :QLTC-STYLE-2,
                     :QLTC-CLASS-2,
                     :QLTC-VIN-2,
                     :QLTC-TITLE-NO-2,
                     :QLTC-MILEAGE-2, 
                     :QLTC-VALUE-2, 
                     :QLTC-SELLPR-2, 
                     :QLTC-MISC-AMT-2, 
                     :QLTC-SENT-FLAG-2,
                     :QLTC-VIN-CHG-DATE-2,
                     :QLTD-BRNO, 
                     :QLTD-ACCTNO, 
                     :QLTD-SEQNO, 
                     :QLTD-DEBIT-CARD-NUMBER,  
                     :QWS-BW-OWNBR1,         
                     :QWS-BW-SSNO1,          
                     :QWS-BW-LNAME1,         
                     :QWS-BW-FNAME1,         
                     :QWS-BW-ADR11,          
                     :QWS-BW-ADR21,          
                     :QWS-BW-CITY1,          
                     :QWS-BW-STATE1,         
                     :QWS-BW-ZIP1,           
                     :QWS-BW-PHONE1,         
                     :QWS-BW-EMPPHONE1,      
                     :QWS-BW-EMPEXT1,        
                     :QWS-BW-CREDIT-SCORE1,  
                     :QWS-BW-OWNBR2,         
                     :QWS-BW-SSNO2,          
                     :QWS-BW-LNAME2,         
                     :QWS-BW-FNAME2,         
                     :QWS-BW-ADR12,          
                     :QWS-BW-ADR22,          
                     :QWS-BW-CITY2,          
                     :QWS-BW-STATE2,         
                     :QWS-BW-ZIP2,           
                     :QWS-BW-PHONE2,         
                     :QWS-BW-EMPPHONE2,      
                     :QWS-BW-EMPEXT2,        
                     :QWS-BW-CREDIT-SCORE2,  
                     :QWS-CDB-TYPE1,         
                     :QWS-CDB-BRANCH1,       
                     :QWS-CDB-CODE1,         
                     :QWS-CDB-DESC1,         
                     :QWS-CDB-GC-GLFEENO1,   
                     :QWS-CDB-TYPE2,         
                     :QWS-CDB-BRANCH2,       
                     :QWS-CDB-CODE2,         
                     :QWS-CDB-DESC2,         
                     :QWS-CDB-GC-GLFEENO2,   
                     :QWS-CDB-TYPE3,         
                     :QWS-CDB-BRANCH3,       
                     :QWS-CDB-CODE3,         
                     :QWS-CDB-DESC3,         
                     :QWS-CDB-GC-GLFEENO3,   
                     :QWS-CDB-TYPE4,         
                     :QWS-CDB-BRANCH4,       
                     :QWS-CDB-CODE4,         
                     :QWS-CDB-DESC4,         
                     :QWS-CDB-GC-GLFEENO4,   
                     :QWS-CDB-TYPE5,         
                     :QWS-CDB-BRANCH5,       
                     :QWS-CDB-CODE5,         
                     :QWS-CDB-DESC5,         
                     :QWS-CDB-GC-GLFEENO5,   
                     :QWS-CDB-TYPE6,         
                     :QWS-CDB-BRANCH6,       
                     :QWS-CDB-CODE6,         
                     :QWS-CDB-DESC6,         
                     :QWS-CDB-GC-GLFEENO6,   
                     :QWS-CDB-TYPE7,         
                     :QWS-CDB-BRANCH7,       
                     :QWS-CDB-CODE7,         
                     :QWS-CDB-DESC7,         
                     :QWS-CDB-GC-GLFEENO7,   
                     :QWS-CDB-TYPE8,         
                     :QWS-CDB-BRANCH8,       
                     :QWS-CDB-CODE8,         
                     :QWS-CDB-DESC8,         
                     :QWS-CDB-GC-GLFEENO8,   
                     :QWS-CDB-TYPE9,         
                     :QWS-CDB-BRANCH9,       
                     :QWS-CDB-CODE9,         
                     :QWS-CDB-DESC9,         
                     :QWS-CDB-GC-GLFEENO9,   
                     :QWS-CDB-TYPE10,        
                     :QWS-CDB-BRANCH10,      
                     :QWS-CDB-CODE10,        
                     :QWS-CDB-DESC10,        
                     :QWS-CDB-GC-GLFEENO10  
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           EXEC SQL SET CONNECTION C1 END-EXEC.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-TRL1-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-TRL-FIELDS 
              PERFORM GET-LN-FIELDS 
              PERFORM GET-LTC-FIELDS 
              PERFORM GET-LTD-FIELDS.

      *-----------------------------------------------------------------
       CLOSE-TRL1-FILE.
           PERFORM CLOSE-IT.
           IF ( TRL1-CURSOR-STAT-GOOD )
               EXEC SQL SET CONNECTION TRL1_CONNECT END-EXEC
               EXEC SQL CLOSE TRL1_CURSOR END-EXEC
               EXEC SQL SET CONNECTION C1 END-EXEC
               MOVE STAT---BAD TO TRL1-CURSOR-STAT.

      *-----------------------------------------------------------------
       CLOSE-TRL1-CONNECTION.
           IF TRL1-CONN-OPEN-SW = "Y"
               EXEC SQL SET CONNECTION TRL1_CONNECT END-EXEC
               EXEC SQL COMMIT END-EXEC
               EXEC SQL DISCONNECT TRL1_CONNECT END-EXEC
               EXEC SQL SET CONNECTION C1 END-EXEC
               MOVE "N" TO TRL1-CONN-OPEN-SW.

      * COPY "LIBLP/LPLC1RN.CPY".
      * COPYMEMBER: LIBLP/LPLC1RN
      *-----------------------------------------------------------------
      *    STILL NEED TO LOAD THE FILE PATH WORKERS
      *-----------------------------------------------------------------
       LOAD-LC1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( LC-PATH-OVERRIDE = "Y" )
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( LC-PATH-OVERRIDE = SPACES )
              MOVE EXT-FILPATH-BASE        TO LC-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO LC-ALL-MACHINE
              MOVE LC-ALL-PATH             TO LC-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( LC-PATH-OVERRIDE = "B" )
              MOVE FILPATH-BASE            TO LC-ALL-BASE
              MOVE FILPATH-MACHINE         TO LC-ALL-MACHINE
              MOVE LC-ALL-PATH             TO LC-PATH.

      *-----------------------------------------------------------------
       OPEN-LC1-FILE.
           PERFORM LOAD-LC1-FILE.
           PERFORM OPEN-IT.
           MOVE LC-PATH-SQL TO E-FILE.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       READ-LC1-FILE. *> EMBEDDED - LCFILE
           PERFORM READ-IT.
           MOVE LC-PATH-SQL TO E-FILE.
           MOVE LC1-KEY TO E-KEYX.
           INITIALIZE QLC-REC.

           MOVE LC-CODE TO QLC-CODE.

           EXEC SQL
            SELECT LCFILE.LC_CODE,
                   LCFILE.LC_DESC
              INTO 
                  :QLC-CODE, 
                  :QLC-DESC
              FROM DBO.LCFILE
             WHERE LCFILE.LC_CODE = :QLC-CODE
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO READ-LC1-FILE.

           IF ( IO-FG = 0 )
              PERFORM GET-LC-FIELDS.

      *-----------------------------------------------------------------
       CLOSE-LC1-FILE.

           PERFORM CLOSE-IT.
 
 
       COPY "LIBLP/LPLTC1RN.CPY".
       COPY "LIBLP/LPLTD1RN.CPY".
       COPY "LIBGB/GBWKIN.CPY".
      *COPY "LIBGB/GBWK2IN.CPY".

      *-----------------------------------------------------------------
       OPEN-EMLNRG-FILE-OUTPUT.
           PERFORM OPEN-IT.
           MOVE EMLNRG-PATH TO E-FILE.
           OPEN OUTPUT EMLNRG-FILE.
           IF ( IO-FG = 8 )
              GO TO OPEN-EMLNRG-FILE-OUTPUT.
           IF ( IO-FG = 7 )
              CLOSE EMLNRG-FILE
              GO TO OPEN-EMLNRG-FILE-OUTPUT.

      *-----------------------------------------------------------------
       WRITE-EMLNRG-FILE.
           PERFORM WRITE-IT.
           MOVE EMLNRG-PATH TO E-FILE.
           WRITE EMLNRG-REC.
           IF ( IO-FG = 8 )
              GO TO WRITE-EMLNRG-FILE.
      *-----------------------------------------------------------------
       CLOSE-EMLNRG-FILE.
           PERFORM CLOSE-IT.
           CLOSE EMLNRG-FILE.
      *-----------------------------------------------------------------
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
      *    MOVE WK2-PATH TO ACCESS-BUF.
      *    PERFORM REMOVE-WORKFILE.

      *-----------------------------------------------------------------
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
              DESTROY EMLNRG-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.
           EXIT PROGRAM.

