       IDENTIFICATION DIVISION.
       PROGRAM-ID.      BYINQ.
       DATE-WRITTEN.    062294.
      ******************************************************************
      *                 (LP)
      *            BRANCH STATISTICS FILE INQUIRY
      *
      * NOTE:
      *     REAL TIME STATS WITH TY RECORDS INCLUDED:
      *        TO GET TY'S INCLUDED, YOU MUST HAVE A BY FOR THE CURRENT
      *        MONTH ALREADY OUT THERE, AND CAN ONLY HAVE THE CURRENT
      *        DAY OPEN, TESTED BY READING RC RECORDS
      * PASSWORDS:
      *       CREATE PASSWORD FOR THE BRANCH LEVEL (LP/STATS/9), THAT KEEPS
      *       USERS FROM LOOKING AT OTHER BRANCH'S STATS.  THE PASSWORDS
      *       FOR OTHER LEVELS ARE ONLY IF YOU HAVE USER LOGINS FOR
      *       SUPERVISORS OR VP'S THAT HAVE GROUPS THAT CORRESPOND TO THE
      *       LAST 3 CHARACTERS OF THEIR LOGINS.  IF THEY HAVE SOME USERS
      *       THAT NEED TO SEE OTHER BRANCHES IN STATS, THEY CAN EITHER KNOW
      *       THE PASSWORD, OR MAYBE THEIR LOGINS ARE LOWER LETTERS, OR YOU
      *       CAN USE PASSWORD EXCEPTIONS
      * WORLD SMALL LOAN CLASSES 1, 2, 3, 4 AND 22
      *
      * REV:
      *  MJD 060995 ADDED REPOSSESSIONS PAGE 4.
      *  MJD 062195 ADDED 'O2' FIELDS. BROKE P1 DOWN INTO TWO
      *             PAGES TO MAKE FIELDS FIT P1:AMT COLLECTED,
      *             P5: AMT REBATED. ADDED P6 CHARGES
      *             COLLECTED/WAIVED.
      *  BLL 062295 ADDING SECURITY CHECKS
      *  MJD 081195 ADDED GP-FILE INFO FOR GP-THE-ONLY-SF-SRCD
      *             ADDED LOGIC FOR GP-THE-ONLY-SF-SRCD.
      *  BLL 090695 DON'T DISPLAY PASTDUE COLUMNS ON PAGE 3
      *  BLL 092895 DON'T INCLUDE SERVICE CLASSES UNLESS 1
      *             CLASS SELECTED & IT'S A SERVICE CLASS
      *  MJD 100395 FIXED INCORRECT PERCENTAGES ON PAGE 3
      *             (% CHANGE FROM BEGIN MONTH)
      *  MJD 110795 CHANGED TOTAL ON PAGE 3 (DELINQ 1-5) TO
      *             INCLUDE 30, 60, 90+, <M>&<P>, AND CURRENT
      *             ONLY. TOTAL LINE HAS ALSO BEEN MOVED TO
      *             TO FOLLOW CURRENT AND PRECEED NB.
      *             COMMENTED OUT DISPLAY OF NB-PL ON SCREEN 3
      *  MJD 111695 REMOVED ALL REFERENCE TO BY/TY-UNITS(X,Y,8-18)
      *             BY/TY-OUTBAL(X,Y,8-18), BY/TY-PASTDUE(X,Y,8-18).
      *  JTG 111795 CHANGED TO TEST ACCESS ON TY-FILE
      *  KEC 121195 ON PAGE 2 & 3, FIXED THE AMOUNT FIELDS FOR DISPLAYS
      *             AND CALCULATIONS WERE INCREASED TO HANDLE THE
      *             BILLION DOLLAR MARK.
      *  KEC 121195 ON PAGE 3 (F2 THRU F6), COMMENTED OUT PASTDUE PRINT,
      *             AND INSERTED COMMAS IN THE AMOUNT FIELDS.
      *  MJD 012396 REMOVED BY-PLA-RECV AND TY-PLA-RECV.
      *  MJD 012996 PAGE 1 - REMOVED PLA RECOVERED.
      *             PAGE 2 - REMOVED PLA AND PLI. ADDED F6 TOGGLE FOR O2
      *                      (REC W/ O2, REC W/O O2, O2 ONLY, INTEREST).
      *             PAGE 3 - CHANGED P&L TO DISPLAY PL,P2,P3,P5 ONLY.
      *  MJD 013096 PAGE 4 - CHANGED PLA/PLI TO P4 ACT/P4 INACT.
      *                      ADDED F6 TOGGLE FOR O2
      *                      (REC W/ O2, REC W/O O2, O2 ONLY, INTEREST).
      *  MJD 013196 PAGE 7 - NEW PAGE. BREAKDOWN OF PL, P2, P3, P4, AND P5'S.
      *                      INCLUDES F6 TOGGLE FOR O2 AMOUNTS.
      *  MJD 013196 PAGE 8 - NEW PAGE. BREAKDOWN ON P&L RECOVERIES.
      *             PAGE 9 - NEW PAGE. BREAKDOWN OF DEALER RESERVES.
      *  MJD 020196 PAGE 1 - ADDED LOANS SOLD/TRANS/PURCH.
      *  MJD 021496 PAGE 3 - SUM PL ACTIVE AND INACTIVE INSTEAD OF PL235 ACT.
      *             PAGE 5 - DO NOT DISPLAY BY-EXREB/TY-EXREB.
      *             PAGE 7 - ADDED F7 TOGGLE FOR INACTIVE:
      *                       IT NET : ACT + INACT OR BOTH ARE DISPLAYED.
      *                       IF VOL : ACTIVE OR BOTH ARE DISPLAYED.
      *             PAGE 9 - REMOVED DISPLAY OF " BALANCES " WHEN
      *                       DISPLAYING VOLUME.
      *  MJD 021696 ADDED OPTION OF SNAPSHOT. STATS ARE READ IN ONCE AND
      *                JUST RECOMPUTE FOR EACH PAGE INSTEAD OF REREADING STATS
      *  MJD 022796 CORRECTED INCORRECT GO TO'S IN SUM-FIELDS-2-BY-DIFF
      *              SECTION, CAUSING INCOMPLETE TOTALS.
      *  MJD 022996 ADDED TOTALS TO PAGE 9 DEALER RESERVES.
      *  BLL 040596 FIX 2 MISSING PERIODS
      *  MJD 041296 INCREASED HBY-SUMWGTT* FIELDS TO PIC 9(15) TO PREVENT
      *             OVERFLOW.
      *  MJD 043096 ADDED OPTION "A" FOR "AFP CLASSES ONLY". OPTION INCLUDES
      *             ONLY THOSE CLASSES IDENTIFIED AS "AFP" IN LC-FILE.
      *             "S" SALES FINANCE OPTION EXCLUDES AFP CLASSES FROM TOTALS.
      *             ADDED LOGIC TO START TYFILE AT END OF CURRENT MONTH
      *  BLV 092396 READ RC TO GET FIRST OPEN DATE FOR EACH BRANCH:
      *             IN GET-TODAYS-TOTALS, ONLY ATTEMPT 1 READ FOR
      *             EACH BRANCH/CLASS ON TY FOR THAT OPEN DATE
      *  MJD 012897 ADDED BR-BYINQ-START-PAGR TO DETERMINE ON WHAT
      *             PAGE TO START THE INQUIRY
      *  KEC 031797 ADDED PAGE 10 (FOR REVISION OF BY, DS TY FILES):
      *                   ADDED   NEW APPLICATION STATISTICS;
      *                   REMOVED OLD APPLICATION STATISTICS.
      *  JTG 032497 ALLOWED F9 FROM ANY SCREEN
      *             REVISED PAGE 10 APPLICATIONS FOR TUD
      *  BLV 051597 REVISE PAGE 10; ADD APS MADE; DISPLAY OUTSTANDING,
      *             NOT GAIN/LOSS
      *  MJD 070197 ADDED BY-DLDISC AND TY-DLDISC.
      *  GLF 090497 REPLACED 'IO' ON GP-FILE WITH GET 'GPENV'
      *  MJD 100897 CHANGED RECORD SIZE TO 2253. EXPANDED ACTIVITY AND REPO
      *             COUNTERS TO 9(5) AND AMOUNTS TO 9(8)V99.
      *  MJD 101597 EXTRA PERIODS "." WERE EMBEDDED WITHIN ONE OF THE 'IF'
      *             STATEMENTS IN SETUP-PAGE3-SUMMARY-TOT SECTION.
      *  JTG 120197 CHANGED TO SKIP READ OF BY WHEN TRAP-A-DATE-TY-FG = 'Y'
      *  JTG 121297 CHANGED TO DISPLAY INCREASE REMAIN FOR WRLD AND NOT NFR
      *  BLV 123197 ADD CLASS OPTION "W" FOR WORLD CLASS BUYING CLUB
      *  BLV 010798 SET CONTRACTUAL MISPAYS INTO RECENCY IF WORLD
      *  BLV 010998 IF WORLD, DON'T DISPLAY WCBC CLASSES IF NORMAL INQUIRY;
      *             DISLPAY ONLY WCBC CLASSES IF INQUIRING FROM WCBC MENU
      *  MJD 012697 AS PER JIM R., FORCE WORLD TO ENTER CLASS MANUALLY.
      *             CUT OFF WCBC ACCESS FROM  INTERRUPT.
      *  MJD 030598 ADDED PAGE 11. DISPLAYS INS # ELIG/WROTE/NOT WROTE
      *             AND AMT WROTE
      *  MJD 031198 EXPANDED XX-SUMWGTTM XX-SUMWGTTA
      *  KEC 032398 FIXED ADDING TO TOTALS FOR APPS; WAS ADDING TWICE TO
      *              'APPROVED' STATUS AND NOT ADDING TO 'TURNDOWN'
      *              STATUS.
      *  KEC 032498 USED THE EMPTY BUCKETS 10 & 11 IN
      *              '[B/T]Y-PERIOD-TBL' & '[B/T]Y-APP-PERIOD-TBL' FOR
      *              NEW SOURCE CODES 'RB' & 'RO'.
      *  MJD 051998 REMOVED XY-PASTDUE. CHANGED XY-EXREB TO BE XY-PART-CHGOFF
      *  JTG 060998 ADDED LOGIC TO DISPLAY BANKRUPT DELINQUENCY BUCKETS
      *  KEC 061298 ADDED [BY/TY]-PART-CHGOFF TO PAGE 7 & STILL IS
      *              ON PAGE 1.
      *  CS  111398  IF GP-I-AM-MERC FORCE INSURANCE ELIGIBLE EQUAL TO
      *              VOLUME UNITS
      *  JTG 111798 CORRECTED PROMPTS FOR NON VOLUME STATS APPS PAGE 10
      *  BLV 990818 CHANGE PAGE 4 TO DISPLAY SLOTS 4 & 5 OF REPO TABLE
      *             INSTEAD OF P4 ACTIVE & INACTIVE, TFC PR# 20
      *             GREATER SUBURBAN PR# 809
      *  GLF 990923 REPLACED IO-FG NUMBERS WITH IO-FG 88 LEVEL PER DEVELOPMENT
      *             MEETING (082599)
      *  MJD 000103 FIXED PAGE 10 ELM AND ELY CHANGE FROM CURRENT.
      *  BLV 000228 BR-ALLCL(1 BRANCH ALL CLASSES) WAS ADDING 1 TO BY-CLASS,
      *             STARTING BY & READING NEXT; CAUSED ENDLESS LOOP IF BY
      *             WITH CLASS 99 EXISTS.  CHANGED TO JUST GO TO READ-NEXT
      *  BLV 000228 MOVE DATE TO BY-KEY IN PARAGRAPH "START-FILE"; REAL-TIME
      *             INQUIRY COULD LOSE ALL DATA IF READ-NEXT HAD PUT ANOTHER
      *             DATE IN KEY & RE-STARTED WITH WRONG DATE
      *  MJD 000327 ADDED PAGE 12 - UNEARNED INCOME (MERC PR#223)
      *  BLV 000627 ADD CLASS OPTIONS OF "O" (OTHER, CLASS 5-20) & "F" (CLASS
      *             30 & 40) FOR WORLD, HAD TO CHANGE 88 FOR ONE-CLASS FROM
      *             "O" TO "1", WORLD PR#231
      *  BLV 000802 FIXED 1998 CHANGE FOR MERC WHICH FORCES VOLUME TO INS-
      *             ELIGIBLE, WAS USING MTD ALWAYS INSTEAD OF MTD OR YTD
      *  MJD ADDED LOCK BOX TOTALS TO PAGE 6 (MFC PR#237)
      *  MJD 001106 FIXED BUG IN LOCK BOX TOTALS, BY/TY-LOCKBOX NEVER
      *             MOVED TO HBY/HTY-LOCKBOX.
      *  BLV 011226 CHANGED CLASS OPTION OF "A" (USED TO BE AFP) TO BE CLASS 1
      *             & CLASS 2 LOANS FOR WORLD, CLASS 2 WILL BE MO IB LOANS
      *  BLV 020308 ADDED CLASS OPTION OF "M": LC-SMALLOAN = "Y", AND "R":
      *             LC-SMALLOAN = "N" & LC-SALEFIN = "N", LENDMARK PR# 1755
      *  BLV 020513 ADDED LOGIC FOR WORLD (PR #292) TO CHECK FOR FILES FROM
      *             OTHER MACHINE WHICH ARE PREPPED TO /USR/PFSI SEVERAL
      *             TIMES A DAY, SO STATS ARE MORE ACCURATE
      *  JTG 021029 CHANGED CALCULATION OF INCREASE REMAINING FOR WORLD
      *  BLV 021120 CHANGE ABOVE WAS SUBTRACTING P2'S FROM SUM-DAILY-PL-AMT
      *             WHICH DISPLAY ON PAGE 3, ALSO SUBTRACT P2'S FROM
      *             SUM-DAILY-PL-NO IF I-AM-WORLD
      *  CS  021212 ADDED TOTALS #ELIGIBLE, NO WRITTEN, NOT WRITTEN, AMOUNT
      *             WRITTEN [WORLD PR# 309] PAGE 11
      * BLV 030211 CHANGES FOR NEW CLASSES (ROYAL PURCHASE), INCLUDE CLASS
      *            3 IN CLASS SELECTION "A", INCLUDE CLASS 21 IN "O",
      *            WORLD PR# 314
      * BAH 030225 ADDED TOTALS ON PAGE 8, PL, P2, P3 ETC..WORLD PR# 317
      * JTG 030318 ADDED GP-NO-SC-USE-DM -NO SALESFIN USE DIRECT MAIL WORLD #307
      * BLV 030401 INCREASED DATA-NO (NUMBER OF ACCTS ON PAGE 2) TO DISPLAY
      *            MILLIONS, ALSO INCREASED WORKER (SUM-NO), WORLD
      * JTG 030421 ADDED GP-NO-SC-USE-DM -NO SALESFIN USE DIRECT MAIL WORLD #307
      *            TESTED TO DETERMINE IF TO INCLUDE 'DM' IN DAILY PAGE 3
      *            'NB' AND NOT 'PB' VOLUME NUMBERS
      * BLV 040628 FIX NB, FB, PB PERCENT TO OUTSTANDING; THEY WEREN'T BEING
      *            MULTIPLIED BY 100, SO USUALLY SHOW AS 0
      * BLF 040928 FIX SUM-FIELDS-12-TY TO NOT ADD TY UNEARNED AMTS TO LAST
      *            YEAR VOLUME
      * BLF 060320 INCREASE SUM-DAILY-PB TO HANDLE 6 DIGITS, REDEFINE DISPLAY
      *            FIELD (DAILY-PB-NO) TO DISPLAY 6 DIGITS IF POSITIVE, ALSO
      *            TOOK A COMMA OUT OF $ VOLUME SO THE TERM WOULD'T RUN INTO
      *            IT IF VOLUME WAS OVER 99 MILLION, WORLD
      * BLF 060525 IF I-AM-WORLD, ADD PERCENTS TO F2 & F5 FLAVORS ON
      *            PAGE 3, WORLD PR# 471
      * BLF 060906 FIX SOME PROBLEMS WITH LAST CHANGE (PR#471), F2 & F5
      *            PERCENTS WERE NOT GETTING CLEARED WHEN YOU HIT F3, F4
      * BLF 060906 ADDED CHECKS ON DIVIDING BY 0 IN SETUP3-PER SECTION - PAGE
      *            3, SECOND F4 FOR PERCENTS WAS GIVING HUGE NUMBER IF WE
      *            DIVIDED BY 0
      * BLF 060907 ADDED AVG TERM, WORLD PR# 489
      * BLF 070118 IF PAGE 3 VOLUME < 100 MILLION, DISPLAY AS 123,456,789
      *            OTHERWISE DISPLAY AS 123456,789.  ALSO, EXPANDED
      *            SUM-DAILY-VOL TO 10 DIGITS!  REGENCY PL# 571
      * BLM 090130 INCREASE DATA-NO-PL IN DISPLAY-BUF7 TO HANDLE 7 DIGITS
      *            FOR NUMBER OF PL'S, ALSO INCREASED SUM-NO-PL, PER JIM
      * BLM 090426 FIX DISPLAY OF AVG TERM ON PAGE 2, WAS DISPLAYING GOOFY
      *            NUMBER IF VOLUME INSTEAD OF GAIN/LOSS.  ADDED FIELD,
      *            SUM-NO-OUTS TO DIVIDE INTO SUM WEIGHTED AMT.
      * BLM 090803 ADD TOTALS TO PAGE 7 (P&L'S), REGMGM PR# 3418
      * BLM 110203 IF GP-I-AM-BAY-COUNTRY & USERID STARTS WITH "P", START
      *            INQUIRY ON PAGE1 & DON'T ALLOW OTHER PAGES, BAYC PR# 3724
      * MJD 130906 ADDED PERFORM LOAD BY1/TY1/RC2 FILES.    ACCESS CALLS WERE
      *            FAILING BECAUSE THEY WERE PERFORMED PRIOR TO OPENING
      *            THE FILES SO THE PATHS WERE NOT SET PROPERLY.
      * BAH 150515 REMOVED _CMD FROM XBYPATH.CPY AND PERFORMED C$LOWER
      * BAH 150516 REMOVED ALL MOVES TO EXT-CONAME-USER-GROUP, USE WGR-WORD, DO
      *            NOT WANT TO SET AN EXTERNAL VARIABLE IN HERE!
      * KEC 2015.1120 {PD#00003} PARADATA <A30> [JAY CISZEWSKI]
      *         FIXED RC-TRANS-DATE FROM YYYMMDD TO CCYYMMDD.
      * BLM 160722 ADD PAGE OF RATING SHEET INFO, PAGE 13, ONLY IF 
      *            GP-I-AM-WORLD, WORLD PR# 1160
      *
      * KEC 2016.0929 {PD#00003} PARADATA <A30> [JAY CISZEWSKI]
      *         CHANGED BY-DATE TO BY-DATE-CCYYMM FOR CONSISTENCY.
      *  CS 161207 IF GP-I-AM-WDT, ON THE PAGE 3 SUMMARY ONLY, SUM-DAILY-NB
      *            DOES NOT INCLUDE HBY-NO(1,2,9) (SOURCE "OT"), INSTEAD ADD
      *            THE SOURCE OT NUMBER,AMOUNTS TO "PB", RECALCULATE OUTSTANDING
      *            PERCENTAGE FOR NB/PB & AVG FOR PB/NB  WDT PR#5126
      * 180727 CS  WORLD NEEDS THE TOP LINE OF DAILY OPERATION STATISTICS
      *            (BALANCE  NO   AVG  BALANCE NO) TO ALLOW AN OPTION THAT
      *            TOGGLES BETWEEN ACTUAL AND RECENCY < 60 OPTION. RECENCY < 60
      *            WILL DISPLAY THE ABOVE ITEMS FROM THE RECENCY COLUMN AND 
      *            INCLUDE ONLY 30,MISSPAYS,POTS,CURRENT.  THE TOGGLE IS F11,
      *            IF YOU MOVE TO ANOTHER PAGE, AND COME BACK TO DAILY, IT
      *            DEFAULTS THE ORIGINAL. IF GP-I-AM-WORLD  PR#1326
      * 180727 CS  CHANGED THE TOPLINE DISPLAY REVERSE VIDEO TO APPLY
      *            ONLY TO OUTSTANDING,NO,GAIN/LOSS/NUMBER, ALSO ONLY DISPLY
      *            INCREASE REMAIN IN FLASH/REVERSE IF POSITIVE
      * 181017 BAH REMOVING OTHER-MACHINE-FG LOGIC. XTYFILE IN /USR/PFSI CAN'T
      *            BE USED IN TY-PATH SUCCESSFULLY IN A32.
      * BAH 181017 GLOBAL TYFILE
      * BAH 181018 GLOBAL RCFILE
      * BAH 181026 GLOBAL BYFILE
      * KEC 190213 REVISED FOR SPLITTING THE BYFILE.
      *            REMOVED LOAN APP FIELDS IN BY & TY.
      *            REMOVED PAGE 10 WITH LOAN APPS
      *            MOVE PAGES BACKWARDS; 11 TO 10, 12 TO 11, 13 TO 12.
      * CS 190808 IS DISPLAY-FIELDS-2, MISSING PERIOD AFTER 
      *           MOVE O2-INT-TYPEL(ACTIVITY) TO WR-BUF   QA0074
      *   IF PAGE2-TODAY
      *       IF PAGE2-REC OR PAGE2-NO-O2
      *          MOVE ACT-TYPET(ACTIVITY) TO WR-BUF
      *       ELSE
      *          MOVE O2-INT-TYPET(ACTIVITY) TO WR-BUF
      *       END-IF
      *    ELSE
      *       IF PAGE2-REC OR PAGE2-NO-O2
      *          MOVE ACT-TYPEL(ACTIVITY) TO WR-BUF
      *       ELSE
      *          MOVE O2-INT-TYPEL(ACTIVITY) TO WR-BUF. <--MISSING PERIOD
      *    MOVE ACT-FNAMEL TO WR-LIST.
      *    PERFORM CALL-WRFORM.
      *    PAGE 11 DISPLAY DID NOT CHANGE HEADERS WHEN PRESSING F3-TODAY
      *    CHANGED PAGE11-PROMPT TO INCLUDE 'TODAY', IN DISPLAY-FIELDS-11
      *    CHANGED  PAGE11-PROMPT(ACTIVITY) TO PAGE11-PROMPT(PAGE11-TYPE)
      *                                                 QA0078
      * BAH 20190924 REMOVED ACCESS-CALL BYAFILE, BYB, RCFILE, TYFILE
      * CS  20191003 FIXED WORLD-MSEL LEGENDS FOR PAGE 3, PAGE 10 AND
      *              PAGE 12
      * 2020.0528 BAH ADDED CLASSES 4 AND 22 TO WORLD SMALL LOANS, #1434
      * 2020.0611 BAH FIXED CLASS DESC FOR GROUP "A", #1434A
      * 2020.1012 CS  ADD BREAKOUT FOR IB LOANS, ENTRY AT CLASS "I" #1466
      * 2023.0620 BAH PAGE 3 F7 TOGGLE, SWITCH IN THE CUR NUMBERS WHERE
      *                THE DEL NUMBERS CURRENTLY ARE DISPLAYED #1612
      * 2024.0619 RMZ PERFORMANCE TUNING: ADDED STORED PROCEDURES #S35Q-69
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       I-O-CONTROL.
       DATA DIVISION.
       FILE SECTION.


       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(36) VALUE
           "@(#)UT/UTINQ S35 06/20/24-07:20:02 >".
      *    **********************************
      *    *      PROGRAM WORKERS
      *    **********************************
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBLP/LP01LC.CPY".
       COPY "LIBLP/LP01LC_SQL.CPY".
       COPY "LIBLP/LPWSLC.CPY".
       COPY "LIBLP/LP01BY.CPY".
       COPY "LIBLP/LP01BYA.CPY".
       COPY "LIBLP/LP01BYB.CPY".
       COPY "LIBLP/LP01BY_SQL.CPY".
       COPY "LIBLP/LPWSBY.CPY".
       COPY "LIBLP/LP01TY.CPY".
       COPY "LIBLP/LP01TY_SQL.CPY".
       COPY "LIBLP/LPWSTY.CPY".
       COPY "LIBGB/GB01GR.CPY".
       COPY "LIBGB/GB01GR_SQL.CPY".
       COPY "LIBGB/GBWSGR.CPY".
       COPY "LIBGB/GB01RC.CPY".
       COPY "LIBGB/GB01RC_SQL.CPY".
       COPY "LIBGB/GBWSRC.CPY".
       COPY "LIBSP/SP01WR.CPY".
       COPY "LIBSP/SP01WR_SQL.CPY".
       COPY "LIBSP/SPWSWR.CPY".

       EXEC SQL BEGIN DECLARE SECTION END-EXEC.            
       01  WS-RC2-ROW-COUNT    PIC 9(9) VALUE 0.
       01  WS-WR1-ROW-COUNT    PIC 9(9) VALUE 0.
       EXEC SQL END DECLARE SECTION END-EXEC.

       01  BY1-CONN-OPEN-SW    PIC X VALUE "N".
       01  WR1-CONN-OPEN-SW    PIC X VALUE "N".

       01  WORLD-DAILY-RECENCY PIC X VALUE " ".

       01  ERRCD           PIC X     VALUE SPACES.
       01  FIRST-TIME      PIC 9     VALUE 0.
       01  FIRST-READ      PIC X     VALUE "Y".
       01  IO-TYPE         PIC X(3)  VALUE " ".
       01  ACTION          PIC X(6)  VALUE " ".
       88  ACTION-P1-9               VALUE "    P1"
                                           "    P2"
                                           "    P3"
                                           "    P4"
                                           "    P5"
                                           "    P6"
                                           "    P7"
                                           "    P8"
                                           "    P9".
       88  ACTION-P10-12             VALUE "   P10"
                                           "   P11"
                                           "   P12".
       01  FILLER REDEFINES ACTION.
           03  FILLER      PIC X(5).
           03  ACTION-P1   PIC 9.
       01  FILLER REDEFINES ACTION.
           03  FILLER      PIC X(4).
           03  ACTION-P2   PIC 99.

       01  REM             PIC X(5)  VALUE X"72656D2D3E".

       01  TRAP-NO-TY-FG       PIC X     VALUE " ".
       01  TRAP-A-DATE-TY-FG   PIC X     VALUE " ".
       01  TY-TRAP-MSG.
           03  FILLER      PIC X(11) VALUE
               "** TY TRAP(".
           03  TY-TRAP-DATE PIC 9(6) VALUE 0.
           03  FILLER      PIC X(2)  VALUE ") ".
           03  TY-TRAP-UPD PIC X.
       01  READ-WRFILE-FG  PIC X     VALUE "N".

       01  LC-NAME         PIC X(20).
       01  FORM-BUFX.
           03  FORM-1       PIC X(6) VALUE "BYINQ1".
           03  FORM-2       PIC X(6) VALUE "BYINQ2".
           03  FORM-3       PIC X(6) VALUE "BYINQ3".
           03  FORM-4       PIC X(6) VALUE "BYINQ4".
           03  FORM-5       PIC X(6) VALUE "BYINQ5".
           03  FORM-6       PIC X(6) VALUE "BYINQ6".
           03  FORM-7       PIC X(6) VALUE "BYINQ7".
           03  FORM-8       PIC X(6) VALUE "BYINQ8".
           03  FORM-9       PIC X(6) VALUE "BYINQ9".
           03  FORM-10      PIC X(6) VALUE "BYINQA".
           03  FORM-11      PIC X(6) VALUE "BYINQB".
           03  FORM-12      PIC X(6) VALUE "BYINQC".
       01  FILLER REDEFINES FORM-BUFX.
           03  FORM-BUFFER  PIC X(6) OCCURS 12 TIMES.

       01  HOLD-FORM-NAM   PIC X(6) VALUE "BYINQ3".

       01  PAGE-FLAG       PIC 9(2) VALUE 3.
           88  PAGE1       VALUE 01.
           88  PAGE2       VALUE 02.
           88  PAGE3       VALUE 03.
           88  PAGE4       VALUE 04.
           88  PAGE5       VALUE 05.
           88  PAGE6       VALUE 06.
           88  PAGE7       VALUE 07.
           88  PAGE8       VALUE 08.
           88  PAGE9       VALUE 09.
           88  PAGE10      VALUE 10.
           88  PAGE11      VALUE 11.
           88  PAGE12      VALUE 12.

       01  GROUP-FLAG      PIC X.
       01  ENT-GROUP       PIC X(4).
       01  ENT-GROUPX REDEFINES ENT-GROUP.
           03  ENT-GROUP-1ST-3 PIC X(3).
           03  FILLER      PIC X.
       01  SAVE-LEVEL      PIC 9.
       01  HOLD-GR-NAME PIC X(20).

       01  ALL-BRANCHES-FLAG        PIC X    VALUE "N".
           88  NOT-ALL-BRANCHES              VALUE "N".
           88  ALL-BRANCHES                  VALUE "Y".
       01  ALL-CLASSES-FLAG         PIC X    VALUE "N".
           88  ONE-CLASS                     VALUE "1".
           88  ALL-CLASSES                   VALUE "Y".
           88  SF-CLASSES                    VALUE "S".
           88  LN-CLASSES                    VALUE "L".
           88  WORLD-SMALL-CLASSES           VALUE "A".
           88  WCBC-CLASSES                  VALUE "W".
           88  WORLD-OTHER-CLASSES           VALUE "O".
           88  WORLD-SF-CLASSES              VALUE "F".
           88  SMALL-LN-CLASSES              VALUE "M".
           88  NON-SMALL-LN-CLASSES          VALUE "R".
           88  IB-LN-CLASSES                 VALUE "I".


       01  YYES                     PIC X    VALUE "Y".
       01  NNO                      PIC X    VALUE "N".
       01  STORE-BRNO               PIC 9(4) VALUE 0.
       01  STORE-CLASS              PIC 99   VALUE 00.
       01  ENTER-CLASS-WK           PIC 99.
       01  ENTER-CLASS-ALPHA       REDEFINES ENTER-CLASS-WK.
           03  CLASS-WK1            PIC X.
           03  CLASS-WK2            PIC X.
       01  CORRECT-CLASS            PIC X    VALUE "N".
       01  STORE-DATE-CCYYMM        PIC 9(6).
       01  FILLER                  REDEFINES STORE-DATE-CCYYMM.
           03  STORE-DATE-CCYYMM-CC PIC 9(2).
           03  STORE-DATE-CCYYMM-YY PIC 9(2).
           03  STORE-DATE-CCYYMM-MM PIC 9(2).
       01  CASE-BUF                 PIC X(4).
       01  FIRST-OPEN-DATE          PIC 9(8) VALUE 0.
       01  PERCENT-WK-2             PIC S999V99 VALUE 0.

       01  TODAYS-DATE.
           03  TODAY-MM           PIC 99.
           03  FILLER             PIC X.
           03  TODAY-DD           PIC 99.
           03  FILLER             PIC X.
           03  TODAY-YY           PIC 99.

       01  DISPLAY-DATE             PIC 9(4).
       01  DISPLAY-DATEX            REDEFINES DISPLAY-DATE.
           03  DISPLAY-MO           PIC 99.
           03  DISPLAY-YR           PIC 99.
       01  DATE-LIST                PIC X(5)  VALUE "MMYY.".
      **************************************************
      *    SUMMARY-TYPES:
      *
      *    BR-CL        0 = ONE BRANCH   / ONE CLASS
      *
      *    BR-ALLCL     1 = ONE BRANCH   / ALL CLASSES
      *    ALLBR-CL     2 = ALL BRANCHES / ONE CLASS
      *    ALLBR-ALLCL  3 = ALL BRANCHES / ALL CLASSES
      *    RESOLVED     1 OR 2 OR 3
      **************************************************
       01  SUMMARY-TYPE        PIC 9 VALUE 0.
           88  BR-CL           VALUE 0.
           88  BR-ALLCL        VALUE 1.
           88  ALLBR-CL        VALUE 2.
           88  ALLBR-ALLCL     VALUE 3.
           88  RESOLVED        VALUE 1 2 3.

      *----------------------------------------------------------------
      * READ TYPE: PASSED IN FROM MENU  1 - SNAPSHOT (DEFAULT)
      *                                 2 - REALTIME
      * READ-ONCE-FG
      *   SET TO "Y" AFTER ALL STAT RECORDS HAVE BEEN READ IN SNAPSHOT
      *     MODE.
      *----------------------------------------------------------------
       01  READ-TYPE           PIC 9 VALUE 1.
           88  READ-ONCE             VALUE 1.
           88  READ-EVERY            VALUE 2.
       01  READ-TYPEA REDEFINES READ-TYPE
                               PIC X.
       01  READ-ONCE-FG     PIC X VALUE "N".


       01  SUB             PIC 9(6) COMP VALUE 1.
       01  SUB1            PIC 9(6) COMP VALUE 1.
       01  SUB2            PIC 9(6) COMP VALUE 1.
       01  SUB4            PIC 9(6) COMP VALUE 1.
       01  BYSUB1            PIC 9(6) COMP VALUE 1.
       01  BYSUB2            PIC 9(6) COMP VALUE 1.
       01  BYSUB3            PIC 9(6) COMP VALUE 1.
       01  SUBX            PIC 9(6) COMP VALUE 1.
       01  SUBY            PIC 9(6) COMP VALUE 1.
       01  PERIOD          PIC 9(6) COMP VALUE 1.
       01  ACTSUB          PIC 9(6) COMP VALUE 1.
       01  TYPSUB          PIC 9(6) COMP VALUE 1.
       01  RECCON          PIC 9(6) COMP VALUE 1.
       01  AGESUB          PIC 9(6) COMP VALUE 1.
       01  MTDYTD          PIC 9(6) COMP VALUE 1.
       01  PLSUB           PIC 9(6) COMP VALUE 1.
       01  REPOSUB         PIC 9(6) COMP VALUE 1.
       01  INSUR           PIC 9(6) COMP VALUE 1.
       01  DLRV            PIC 9(6) COMP VALUE 1.

       01  WORK1           PIC S9(9)V99 VALUE ZERO.

       01  WORLD-MSEL-PROMPTS.
           03  FILLER PIC X(67) VALUE
           "F1-CLEAR, F2-CURRENT, F3-TODAY, P2 - P12:".
           03  FILLER PIC X(67) VALUE
           "F1-CLEAR, F2-CURRENT, F3-TODAY, P1,P3 - P12:".
      *PAGE 3
           03  FILLER PIC X(45) VALUE
           "F2-CURRENT,F3-TODAY,F4-DIFF BEGMO,F5-BEG/END,".
           03  FILLER PIC X(22) VALUE
           "F6-BNKRP, P1-2, P4-12:".
      *PAGE 4
           03  FILLER PIC X(67) VALUE
           "F1-CLEAR, F2-CURRENT, F3-TODAY, P1 - P3,P5 - P12:".
      *PAGE 5
           03  FILLER PIC X(67) VALUE
           "F1-CLEAR, F2-CURRENT, F3-TODAY, P1 - P4,P6 - P12:".
           03  FILLER PIC X(67) VALUE
           "F1-CLEAR, F2-CURRENT, F3-TODAY, P1- P5, P7 - P12:".

           03  FILLER PIC X(67) VALUE
           "F1-CLEAR, F2-CURRENT, F3-TODAY, P1 - P6, P8 - P12:".
           03  FILLER PIC X(35) VALUE
           "F1-CLEAR, F2-CURRENT MTD, F3-TODAY,".
           03  FILLER PIC X(32) VALUE
           "F4-CURRENT YTD,P1-P7,P9-P12:".
           03  FILLER PIC X(67) VALUE
           "F1-CLEAR, F2-CURRENT, F3-TODAY, P1 - P8, P10 - P12:".
      *PAGE 10
           03  FILLER PIC X(67) VALUE
           "F1-CLEAR, P1 - P9, P11-P12: ".
      *PAGE 11
           03  FILLER PIC X(40) VALUE
               "F1-CLEAR, F2-CURRENT, F3-TODAY, F4-NET, ".
           03  FILLER PIC X(27) VALUE
               "F5-VOL, P1 - P10, P12:".
           03  FILLER PIC X(67) VALUE
           "F1-CLEAR, P1 - P11:".


       01  WORLD-MSEL-TABLE REDEFINES WORLD-MSEL-PROMPTS
                                     PIC X(67) OCCURS 12.

       01  MSEL-LIST    PIC X(7)    VALUE "MSEL:P.".

      *----------------------------------------------------------------
      * SCREEN PROMPTS PAGE 1
      *----------------------------------------------------------------
       01  PAGE1-TYPE     PIC 9 VALUE 1.
           88  PAGE1-TOTAL      VALUE 1.
           88  PAGE1-TODAY      VALUE 2.
       01  PAGE1-TYPE1-DESC.
           03  FILLER     PIC X(13) VALUE
           "***  MTD  ***".
           03  FILLER     PIC X(13) VALUE
           "***  YTD  ***".
       01  PAGE1-TYPE2-DESC.
           03  FILLER     PIC X(13) VALUE
           "**  TODAY  **".
           03  FILLER     PIC X(13) VALUE
           "             ".
       01  PAGE1-DESC-LIST  PIC X(16) VALUE "TYPE1:P TYPE2:P.".

      *----------------------------------------------------------------
      * SCREEN PROMPTS PAGE 2
      *----------------------------------------------------------------
       01  PAGE2-TYPE     PIC 9 VALUE 1.
           88  PAGE2-TOTAL      VALUE 1.
           88  PAGE2-TODAY      VALUE 2.
       01  PAGE2-O2-TYPE  PIC 9 VALUE 1.
           88  PAGE2-REC        VALUE 1.
           88  PAGE2-NO-O2      VALUE 2.
           88  PAGE2-O2         VALUE 3.
           88  PAGE2-INT        VALUE 4.

       01  O2-TYPE-TABLE.
           03  FILLER     PIC X(27) VALUE
           "-------- ACCOUNTS ---------".
           03  FILLER     PIC X(27) VALUE
           "-------- ACCOUNTS ---------".
           03  FILLER     PIC X(27) VALUE
           "                           ".
           03  FILLER     PIC X(27) VALUE
           "                           ".
           03  FILLER     PIC X(45) VALUE
           "------------ RECEIVABLES WITH O2 ------------".
           03  FILLER     PIC X(45) VALUE
           "------------ RECEIVABLES LESS O2 ------------".
           03  FILLER     PIC X(45) VALUE
           "------------------ O2 ONLY ------------------".
           03  FILLER     PIC X(45) VALUE
           "----------------- INTEREST ------------------".
       01  O2-TYPES      REDEFINES O2-TYPE-TABLE.
           03  O2-ACCT    PIC X(27) OCCURS 4 TIMES.
           03  O2-REC     PIC X(45) OCCURS 4 TIMES.
       01  O2-FNAME      PIC X(11)  VALUE "O2_TYPE1:P.".
       01  O2-FNAMEL     PIC X(11)  VALUE "O2_TYPE2:P.".

       01  O2-INT-TYPE-TABLE.
           03  FILLER     PIC X(41) VALUE SPACES.
           03  FILLER     PIC X(39) VALUE
           "  $REC             MTD             YTD".
           03  FILLER     PIC X(41) VALUE SPACES.
           03  FILLER     PIC X(39) VALUE
           "   MTD             YTD             LYR ".
           03  FILLER     PIC X(41) VALUE SPACES.
           03  FILLER     PIC X(39) VALUE
           " TODAY                                 ".
           03  FILLER     PIC X(41) VALUE SPACES.
           03  FILLER     PIC X(39) VALUE
           " TODAY                                 ".
       01  O2-INT-TYPES      REDEFINES O2-INT-TYPE-TABLE.
           03  O2-INT-TYPEL  PIC X(80) OCCURS 2 TIMES.
           03  O2-INT-TYPET  PIC X(80) OCCURS 2 TIMES.

       01  ACT-TYPE-TABLE-MTD-YTD.
           03  FILLER     PIC X(21) VALUE
           "***      MTD      ***".
           03  FILLER     PIC X(21) VALUE
           "***      YTD      ***".
       01  ACT-TYPES-MTD-YTD  REDEFINES ACT-TYPE-TABLE-MTD-YTD.
           03  ACT-TYPE-MTD-YTD   PIC X(21) OCCURS 2 TIMES.

       01  ACT-TYPE-TABLE.
           03  FILLER     PIC X(21) VALUE
           "*** NET GAIN/LOSS ***".
           03  FILLER     PIC X(21) VALUE
           " *** NEW VOLUME ***  ".
           03  FILLER     PIC X(41) VALUE
           "          NO      MTD      YTD           ".
           03  FILLER     PIC X(39) VALUE
           "  $REC             MTD             YTD ".
           03  FILLER     PIC X(41) VALUE
           "         MTD      YTD      LYR ".
           03  FILLER     PIC X(39) VALUE
           "   MTD             YTD             LYR ".
           03  FILLER     PIC X(41) VALUE
           "      TODAY                              ".
           03  FILLER     PIC X(39) VALUE
           " TODAY                                 ".
           03  FILLER     PIC X(41) VALUE
           "      TODAY                              ".
           03  FILLER     PIC X(39) VALUE
           " TODAY                                 ".
       01  ACT-TYPES      REDEFINES ACT-TYPE-TABLE.
           03  ACT-TYPE   PIC X(21) OCCURS 2 TIMES.
           03  ACT-TYPEL  PIC X(80) OCCURS 2 TIMES.
           03  ACT-TYPET  PIC X(80) OCCURS 2 TIMES.
       01  ACT-FNAME      PIC X(9)  VALUE "ACT_TYPE.".
       01  ACT-FNAME2     PIC X(12) VALUE "ACT_TYPE2:P.".
       01  ACT-FNAMEL     PIC X(11) VALUE "ACT_LINE:P.".

      *----------------------------------------------------------------
      * SCREEN PROMPTS PAGE 4
      *----------------------------------------------------------------
       01  O2-INT-TYPE-TABLE4.
           03  FILLER     PIC X(41) VALUE SPACES.
           03  FILLER     PIC X(39) VALUE
           "    $REC            MTD            YTD ".
           03  FILLER     PIC X(41) VALUE SPACES.
           03  FILLER     PIC X(39) VALUE
           "     MTD            YTD            LYR ".
           03  FILLER     PIC X(41) VALUE SPACES.
           03  FILLER     PIC X(39) VALUE
           "   TODAY                               ".
           03  FILLER     PIC X(41) VALUE SPACES.
           03  FILLER     PIC X(39) VALUE
           " TODAY                                 ".
       01  O2-INT-TYPES4      REDEFINES O2-INT-TYPE-TABLE4.
           03  O2-INT-TYPE4L  PIC X(80) OCCURS 2 TIMES.
           03  O2-INT-TYPE4T  PIC X(80) OCCURS 2 TIMES.

       01  ACT4-TYPE-TABLE.
           03  FILLER     PIC X(21) VALUE
           "*** NET GAIN/LOSS ***".
           03  FILLER     PIC X(21) VALUE
           " *** NEW VOLUME ***  ".
           03  FILLER     PIC X(41) VALUE
           "             NO.     MTD     YTD         ".
           03  FILLER     PIC X(39) VALUE
           "    $REC            MTD            YTD ".
           03  FILLER     PIC X(41) VALUE
           "             MTD     YTD    LYR          ".
           03  FILLER     PIC X(39) VALUE
           "     MTD            YTD            LYR ".
           03  FILLER     PIC X(41) VALUE
           "           TODAY                         ".
           03  FILLER     PIC X(39) VALUE
           "   TODAY                               ".
           03  FILLER     PIC X(41) VALUE
           "           TODAY                         ".
           03  FILLER     PIC X(39) VALUE
           "   TODAY                               ".
       01  ACT4-TYPES      REDEFINES ACT4-TYPE-TABLE.
           03  ACT4-TYPE   PIC X(21) OCCURS 2 TIMES.
           03  ACT4-TYPEL  PIC X(80) OCCURS 2 TIMES.
           03  ACT4-TYPET  PIC X(80) OCCURS 2 TIMES.
       01  PAGE4-TYPE     PIC 9 VALUE 1.
           88  PAGE4-TOTAL      VALUE 1.
           88  PAGE4-TODAY      VALUE 2.
       01  PAGE4-O2-TYPE  PIC 9 VALUE 1.
           88  PAGE4-REC        VALUE 1.
           88  PAGE4-NO-O2      VALUE 2.
           88  PAGE4-O2         VALUE 3.
           88  PAGE4-INT        VALUE 4.

      *----------------------------------------------------------------
      * SCREEN PROMPTS PAGE 5
      *----------------------------------------------------------------
       01  PAGE5-TYPE     PIC 9 VALUE 1.
           88  PAGE5-TOTAL      VALUE 1.
           88  PAGE5-TODAY      VALUE 2.
       01  PAGE5-TYPE1-DESC.
           03  FILLER     PIC X(13) VALUE
           "***  MTD  ***".
           03  FILLER     PIC X(13) VALUE
           "***  YTD  ***".
       01  PAGE5-TYPE2-DESC.
           03  FILLER     PIC X(13) VALUE
           "**  TODAY  **".
           03  FILLER     PIC X(13) VALUE
           "             ".
       01  PAGE5-DESC-LIST  PIC X(16) VALUE "TYPE1:P TYPE2:P.".

      *----------------------------------------------------------------
      * SCREEN PROMPTS PAGE 6
      *----------------------------------------------------------------
       01  PAGE6-TYPE     PIC 9 VALUE 1.
           88  PAGE6-TOTAL      VALUE 1.
           88  PAGE6-TODAY      VALUE 2.
       01  PAGE6-TYPE1-DESC.
           03  FILLER     PIC X(13) VALUE
           "***  MTD  ***".
           03  FILLER     PIC X(13) VALUE
           "***  YTD  ***".
       01  PAGE6-TYPE2-DESC.
           03  FILLER     PIC X(13) VALUE
           "**  TODAY  **".
           03  FILLER     PIC X(13) VALUE
           "             ".
       01  PAGE6-DESC-LIST  PIC X(16) VALUE "TYPE1:P TYPE2:P.".

      *----------------------------------------------------------------
      * SCREEN PROMPTS PAGE 7
      *----------------------------------------------------------------
       01  ACT7-TYPE-TABLE.
           03  FILLER     PIC X(21) VALUE
           "*** NET GAIN/LOSS ***".
           03  FILLER     PIC X(21) VALUE
           " *** NEW VOLUME ***  ".
           03  FILLER     PIC X(41) VALUE
           "            NO.     MTD     YTD          ".
           03  FILLER     PIC X(39) VALUE
           "  $REC             MTD             YTD ".
           03  FILLER     PIC X(41) VALUE
           "            MTD     YTD    LYR          ".
           03  FILLER     PIC X(39) VALUE
           "   MTD              YTD            LYR ".
           03  FILLER     PIC X(40) VALUE
           "            TODAY                    ".
           03  FILLER     PIC X(40) VALUE
           "  TODAY                                 ".
           03  FILLER     PIC X(40) VALUE
           "            TODAY                    ".
           03  FILLER     PIC X(40) VALUE
           "  TODAY                                 ".
       01  ACT7-TYPES      REDEFINES ACT7-TYPE-TABLE.
           03  ACT7-TYPE   PIC X(21) OCCURS 2 TIMES.
           03  ACT7-TYPEL  PIC X(80) OCCURS 2 TIMES.
           03  ACT7-TYPET  PIC X(80) OCCURS 2 TIMES.

       01  O2-INT-TYPE-TABLE7.
           03  FILLER     PIC X(41) VALUE SPACES.
           03  FILLER     PIC X(39) VALUE
           "  $REC             MTD             YTD ".
           03  FILLER     PIC X(41) VALUE SPACES.
           03  FILLER     PIC X(39) VALUE
           "   MTD             YTD             LYR ".
           03  FILLER     PIC X(41) VALUE SPACES.
           03  FILLER     PIC X(39) VALUE
           " TODAY                                 ".
           03  FILLER     PIC X(41) VALUE SPACES.
           03  FILLER     PIC X(39) VALUE
           " TODAY                                 ".
       01  O2-INT-TYPES7      REDEFINES O2-INT-TYPE-TABLE7.
           03  O2-INT-TYPE7L  PIC X(80) OCCURS 2 TIMES.
           03  O2-INT-TYPE7T  PIC X(80) OCCURS 2 TIMES.

      *----------------------------------------------------------------
      *  "I"'S ARE DISPLAY IF DISPLAYING SEPARATE ACTIVE AND INACTIVE
      *  " "'S ARE DISPLAY IF SHOWING TOTALS
      *  NOTE: BASED ON PAGE7-TOT-TYPE AS SUBSCRIPT
      *----------------------------------------------------------------
       01  ACT-INACT-TABLE7.
           03  FILLER     PIC X(5) VALUE "     ".
           03  FILLER     PIC X(5) VALUE "IIIII".
       01  ACT-INACT-TYPE-TBL7 REDEFINES ACT-INACT-TABLE7.
           03  I-BUF7     PIC X(5)   OCCURS 2 TIMES.

       01  I-LIST7        PIC X(30)  VALUE
           "I01:S I02:S I03:S I04:S I05:S.".

      *----------------------------------------------------------------
      * PAGE7-TOT-PL - IF NEW VOLUME THEN P&L ACTIVE WILL BE DISPLAYED
      *                IF NET GAIN/LOSS THEN A TOTAL OF P&L ACTIVE +
      *                  P&L INACTIVE WILL BE DISPLAYED
      * PAGE7-SEP-PL - P&L ACTIVE AND P7L INACTIVE WILL BE DISPLAYED
      *                  SEPARATELY
      * NOTE: VOLUME CANNOT BE DISPLAYED AS ACTIVE + INACTIVE BECAUSE
      *        AN ACCOUNT THAT WAS PUT INTO PLA AND THEN INTO PLI IN THE
      *        SAME MONTH WILL BE COUNTED TWICE.
      * DEFAULT TO TOTAL ON ENTRY.
      *----------------------------------------------------------------
       01  PAGE7-TOT-TYPE PIC 9 VALUE 1.
           88  PAGE7-TOT-PL     VALUE 1.
           88  PAGE7-SEP-PL     VALUE 2.

      *----------------------------------------------------------------
      *  PAGE7-ACTIVE - PL-ACTIVITY TABLE ACTIVE BUCKETS 1, 3, 5, 7, 9
      *  PAGE7-INACT  - PL-ACTIVITY TABLE INACTIVE BUCKETS 2, 4, 6, 8, 10
      *----------------------------------------------------------------
       01  PAGE7-A-I-TYPE PIC 99 VALUE 1.
           88  PAGE7-ACTIVE      VALUE 1 3 5 7 9.
           88  PAGE7-INACT       VALUE 2 4 6 8 10.

       01  PAGE7-TYPE     PIC 9 VALUE 1.
           88  PAGE7-TOTAL      VALUE 1.
           88  PAGE7-TODAY      VALUE 2.

      * DEFAULT TO NEW VOLUME (P7-ACTIVITY) ON ENTRY
       01  PAGE7-O2-TYPE  PIC 9 VALUE 1.
           88  PAGE7-REC        VALUE 1.
           88  PAGE7-NO-O2      VALUE 2.
           88  PAGE7-O2         VALUE 3.
           88  PAGE7-INT        VALUE 4.

      *----------------------------------------------------------------
      * P7-ACTIVITY - PAGE 7 ACTIVITY : DEFAULT TO NEW VOLUME ON ENTRY.
      *----------------------------------------------------------------
       01  P7-ACTIVITY    PIC 9 VALUE 2.
           88  P7-NET-GAIN-LOSS    VALUE 1.
           88  P7-NEW-VOLUME       VALUE 2.
      *----------------------------------------------------------------
      * SCREEN PROMPTS PAGE 8
      *----------------------------------------------------------------
       01  PAGE8-TYPE     PIC 9 VALUE 1.
           88  PAGE8-MTD        VALUE 1.
           88  PAGE8-YTD        VALUE 2.
           88  PAGE8-TODAY      VALUE 3.

       01  PAGE8-TYPE-DESCRIPTION.
           03  FILLER     PIC X(38) VALUE
           "---------------- MTD -----------------".
           03  FILLER     PIC X(38) VALUE
           "---------------- YTD -----------------".
           03  FILLER     PIC X(38) VALUE
           "-------------- CURRENT ---------------".
       01  PAGE8-TYPE-DESCX REDEFINES PAGE8-TYPE-DESCRIPTION.
           03  PAGE8-TYPE-DESC    OCCURS 3
                                  PIC X(38).

       01  PAGE8-DESC-LIST  PIC X(8) VALUE "TYPE1:P.".

      *----------------------------------------------------------------
      * SCREEN PROMPTS PAGE 9
      *----------------------------------------------------------------
       01  ACT9-TYPE-TABLE.
           03  FILLER     PIC X(21) VALUE
           "*** NET GAIN/LOSS ***".
           03  FILLER     PIC X(21) VALUE
           " *** NEW VOLUME ***  ".

           03  FILLER     PIC X(10) VALUE
           " BALANCES ".
           03  FILLER     PIC X(10) VALUE
           "----------".

           03  FILLER     PIC X(41) VALUE SPACES.
           03  FILLER     PIC X(39) VALUE
           " AMT              MTD              YTD ".
           03  FILLER     PIC X(41) VALUE SPACES.
           03  FILLER     PIC X(39) VALUE
           " MTD              YTD              LYR ".
           03  FILLER     PIC X(40) VALUE SPACES.
           03  FILLER     PIC X(40) VALUE
           "TODAY                                   ".
           03  FILLER     PIC X(40) VALUE SPACES.
           03  FILLER     PIC X(40) VALUE
           "TODAY                                   ".
       01  ACT9-TYPES      REDEFINES ACT9-TYPE-TABLE.
           03  ACT9-TYPE   PIC X(21) OCCURS 2 TIMES.
           03  ACT9-TYPE2  PIC X(10) OCCURS 2 TIMES.
           03  ACT9-TYPEL  PIC X(80) OCCURS 2 TIMES.
           03  ACT9-TYPET  PIC X(80) OCCURS 2 TIMES.

       01  PAGE9-TYPE     PIC 9 VALUE 1.
           88  PAGE9-TOTAL      VALUE 1.
           88  PAGE9-TODAY      VALUE 2.

       01  PAGE10-TYPE                 PIC 9       VALUE 1.
           88  PAGE10-TOTAL                        VALUE 1.
           88  PAGE10-TODAY                        VALUE 2.

      *----------------------------------------------------------------
      * SCREEN PROMPTS COMMON
      *----------------------------------------------------------------
       01  ACTIVITY       PIC 9 VALUE 1.
           88  NET-GAIN-LOSS    VALUE 1.
           88  NEW-VOLUME       VALUE 2.

      *----------------------------------------------------------------
      * SCREEN DISPLAYS FOR INQUIRY TYPE:
      *  INDEXED BY READ-TYPE.
      *   1 - SNAPSHOT STATISTICS (NORMAL PRINT).
      *   2 - REALTIME STATISTICS (REVERSE PRINT).
      *----------------------------------------------------------------
       01  DISP-INQ-TYPES.
           03 FILLER            PIC X(29) VALUE
           "SNAPSHOT OPERATING STATISTICS".
           03 FILLER            PIC X(29) VALUE
           "REALTIME OPERATING STATISTICS".
       01  DISP-INQ-TYPES-TBL  REDEFINES DISP-INQ-TYPES.
           03 INQUIRY-TYPE      PIC X(29) OCCURS 2.
       01  DISP-INQ-LISTS.
           03  FILLER           PIC X(12) VALUE "INQ_TYPE:S.".
           03  FILLER           PIC X(12) VALUE "INQ_TYPE:S.".
       01  DISP-INQ-LISTS-TBL  REDEFINES DISP-INQ-LISTS.
           03 INQUIRY-LIST      PIC X(12) OCCURS 2.

      *----------------------------------------------------------------
      * SCREEN PROMPTS PAGE 11 
      *----------------------------------------------------------------
       01  PAGE11-TYPE     PIC 9 VALUE 1.
           88  PAGE11-TOTAL      VALUE 1.
           88  PAGE11-TODAY      VALUE 2.
       01  PAGE11-PROMPTS-TABLE.
           03  FILLER      PIC X(19)    VALUE "           CURRENT ".
           03  FILLER      PIC X(19)    VALUE "GAIN-FOR-THE-MONTH ".
           03  FILLER      PIC X(19)    VALUE " GAIN-FOR-THE-YEAR ".
           03  FILLER      PIC X(19)    VALUE "     MONTH-TO-DATE ".
           03  FILLER      PIC X(19)    VALUE "      YEAR-TO-DATE ".
           03  FILLER      PIC X(19)    VALUE "         LAST-YEAR ".
           03  FILLER      PIC X(19)    VALUE "             TODAY ".
           03  FILLER      PIC X(19)    VALUE SPACES.
           03  FILLER      PIC X(19)    VALUE SPACES.
           03  FILLER      PIC X(19)    VALUE  SPACES.
           03  FILLER      PIC X(19)    VALUE SPACES.
           03  FILLER      PIC X(19)    VALUE SPACES.



       01  PAGE11-PROMPT-TBL REDEFINES PAGE11-PROMPTS-TABLE.
      *    03  PAGE11-PROMPT   PIC X(57) OCCURS 2.
           03  PAGE11-PROMPT   PIC X(114) OCCURS 2.
       01  PAGE11-PMPT-LIST.
           03  FILLER      PIC X(30) VALUE
           "PROMPT1:P PROMPT2:P PROMPT3:P.".

      **************************************************
      *    SCREEN DISPLAY BUFFER
      **************************************************
      *=================================================================
      *
      *    S C R E E N - 1 - F I E L D S
      *
      *  DATA-STP-TABLE1 :    1 - LOANS SOLD
      *                       2 - LOANS TRANSFERRED
      *                       3 - LOANS PURCHASED
      *=================================================================
       01  DISPLAY-BUF1    VALUE " ".
           03  DATA-TYPE-TABLE1   OCCURS 9 TIMES.
               05  DATA-AMTS1     OCCURS 2 TIMES.
                   07  DATA-AMT1     PIC Z,ZZZ,ZZZ,ZZ9.99- BLANK ZERO.
           03  DATA-STP-TABLE1    OCCURS 3.
               05  DATA-NOS-STP   OCCURS 2.
                   07  DATA-NO-STP   PIC Z,ZZZ,ZZ9- BLANK ZERO.
               05  DATA-AMTS-STP  OCCURS 2.
                   07  DATA-AMT-STP  PIC Z,ZZZ,ZZZ,ZZ9.99- BLANK ZERO.

       01  DISPLAY-LIST1.
           03  FILLER     PIC X(32) VALUE
               "010 020 030 040 050 060 070 080 ".
           03  FILLER     PIC X(50) VALUE
               "090 100 110 120 130 140 141 142 140A 140B 150 160 ".
           03  FILLER     PIC X(40) VALUE
               "170 180 190 200 210 220 230 240 250 260.".

      **************************************************
      *    PAGE 1 TOTALS
      *
      *  PAGE1-STP-TOTALS :   1 - LOANS SOLD
      *                       2 - LOANS TRANSFERRED
      *                       3 - LOANS PURCHASED
      **************************************************
       01  PAGE-1-TOTALS     VALUE ZERO.
           03  PAGE-1-TABLE  OCCURS 9 TIMES.
               05  SUM1      OCCURS 2 TIMES
                             PIC S9(10)V99.
           03  PAGE-1-STP-TABLE OCCURS 3 TIMES.
               05  SUM1-NO   OCCURS 2 TIMES
                             PIC S9(7).
               05  SUM1-AMT  OCCURS 2 TIMES
                             PIC S9(10)V99.

      *=================================================================
      *
      *    S C R E E N - 2 - F I E L D S
      *
      *=================================================================
       01  DISPLAY-BUF2          VALUE " ".
           03  DATA-TYPE-TABLE   OCCURS 12 TIMES.
               05  DATA-NOS      OCCURS 3 TIMES.
                   07  DATA-NO   PIC ZZZZ,ZZ9-           BLANK ZERO.
               05  FILLER        PIC X.
               05  DATA-AMTS     OCCURS 3 TIMES.
                   07  DATA-AMT  PIC ZZZZ,ZZZ,ZZ9.99-    BLANK ZERO.
           03  DATA-130          PIC Z,ZZZ,ZZ9.99-       BLANK ZERO.
           03  DATA-140          PIC ZZZ,ZZZ,ZZZ,ZZ9.99- BLANK ZERO.
           03  DATA-150          PIC ZZZ9.99-            BLANK ZERO.

      *-----------------------------------------------------------------
       01  DISPLAY-LIST2.
           03  FILLER       PIC X(40) VALUE
           "010 020 030 040 050 060 070 080 090 100 ".
           03  FILLER       PIC X(20) VALUE
           "110 120 130 140 150.".

      *=================================================================
      *
      *    P A G E - 2 - T O T A L S
      *
      *=================================================================
       01  PAGE-2-TOTALS.
           03  SUM-TABLE        OCCURS 12 TIMES.
               05  SUM-NOS      OCCURS 3 TIMES.
                   07  SUM-NO   PIC S9(7).
               05  SUM-AMTS     OCCURS 3 TIMES.
                   07  SUM-AMT  PIC S9(10)V99.
           03  SUM-WGTTM        PIC S9(15)V99   VALUE ZERO.
           03  SUM-WGTAV        PIC S9(15)V99   VALUE ZERO.
           03  SUM-WGTTA        PIC S9(15)V99   VALUE ZERO.
           03  SUM-NO-OUTS      PIC S9(9)       VALUE ZERO.

      *=================================================================
      *
      *    SCREEN 3 FIELDS
      *
      *=================================================================
       01  PROMPT-12345.
           03  FILLER       PIC X(45)   VALUE
           "       UNITS            RECENCY             U".
           03  FILLER       PIC X(35)   VALUE
           "NITS        CONTRACTUAL            ".

      *-----------------------------------------------------------------
       01  DEL-TYPE-TABLE.
           03  FILLER     PIC X(34) VALUE
           " ****  CURRENT INFORMATION  ****  ".
           03  FILLER     PIC X(34) VALUE
           "****  BEGINNING OF THE MONTH  ****".
           03  FILLER     PIC X(34) VALUE
           "   ****  END OF LAST MONTH ****   ".
           03  FILLER     PIC X(34) VALUE
           " ****  TODAYS INFORMATION   ****  ".
           03  FILLER     PIC X(34) VALUE
           "***  CHANGE FROM BEGIN MONTH  *** ".
           03  FILLER     PIC X(34) VALUE
           "**  % CHANGE FROM BEGIN MONTH  ** ".
           03  FILLER     PIC X(34) VALUE
           "**  DAILY OPERATING STATISTICS  **".

      *-----------------------------------------------------------------
       01  DEL-TYPES REDEFINES DEL-TYPE-TABLE.
           03  DEL-TYPE   PIC X(34)
                          OCCURS 7 TIMES.
      *-----------------------------------------------------------------
       01  DEL-MSEL-TABLE.
           03  FILLER       PIC X(40)   VALUE
           "F3-TODAY,F4-DIFF BEGMO,F5-BEG/END,F6-BAN".
           03  FILLER       PIC X(27)   VALUE
           "KRUPT,F7-DAILY,P1-2, P4-12:".
           03  FILLER       PIC X(40)   VALUE
           "F2-CURRENT,F3-TODAY,F4-DIFF BEGMO,F6-BAN".
           03  FILLER       PIC X(27)   VALUE
           "KRUPT,F7-DAILY,P1-2, P4-12:".
           03  FILLER       PIC X(40)   VALUE
           "F2-CURRENT,F3-TODAY,F4-DIFF BEGMO,F5-BEG".
           03  FILLER       PIC X(27)   VALUE
           "/END,F7-DAILY, P1-2, P4-12:".
           03  FILLER       PIC X(40)   VALUE
           "F2-CURRENT,F4-DIFF BEGMO,F5-BEG/END,F6-B".
           03  FILLER       PIC X(27)   VALUE
           "ANKRUPT,F7-DAILY,P1-2,P4-12".
           03  FILLER       PIC X(40)   VALUE
           "F4,F2-CURRENT,F3-TODAY,F5-BEG/END,F6-BAN".
           03  FILLER       PIC X(27)   VALUE
           "KRUPT,F7-DAILY,P1-2, P4-12:".
           03  FILLER       PIC X(40)   VALUE
           "F2-CURRENT,F3-TODAY,F4,F5-BEG/END,F6-BAN".
           03  FILLER       PIC X(27)   VALUE
           "KRUPT,F7-DAILY,P1-2, P4-12:".
           03  FILLER       PIC X(40)   VALUE
           "F7,F2-CURRENT,F3-TODAY,F4-DIFF BEGMO,F5-".
           03  FILLER       PIC X(27)   VALUE
           "BEG/END,F6-BNKRP,P1-2,P4-12".
       01  DEL-MSEL-TBL     REDEFINES DEL-MSEL-TABLE.
           03  DEL-MSEL     OCCURS 7 TIMES
                            PIC X(67).
       01  WORLD-DEL-MSEL-TABLE.
           03  FILLER       PIC X(40)   VALUE
           "F3-TODAY,F4-DIFF BEGMO,F5-BEG/END,F6-BAN".
           03  FILLER       PIC X(27)   VALUE
           "KRUPT,F7-DAILY,P1-2, P4-12:".
           03  FILLER       PIC X(40)   VALUE
           "F2-CURRENT,F3-TODAY,F4-DIFF BEGMO,F6-BAN".
      *PAGE 3
           03  FILLER       PIC X(27)   VALUE
           "KRUPT,F7-DAILY,P1-2, P4-12:".

           03  FILLER       PIC X(40)   VALUE
           "F2-CURRENT,F3-TODAY,F4-DIFF BEGMO,F5-BEG".
           03  FILLER       PIC X(27)   VALUE
           "/END,F7-DAILY, P1-2, P4-12:".
           03  FILLER       PIC X(40)   VALUE
           "F2-CURRENT,F4-DIFF BEGMO,F5-BEG/END,F6-B".
           03  FILLER       PIC X(27)   VALUE
           "ANKRUPT,F7-DAILY,P1-2,P4-12".
           03  FILLER       PIC X(40)   VALUE
           "F4,F2-CURRENT,F3-TODAY,F5-BEG/END,F6-BAN".
           03  FILLER       PIC X(27)   VALUE
           "KRUPT,F7-DAILY,P1-2, P4-12:".
           03  FILLER       PIC X(40)   VALUE
           "F2-CURRENT,F3-TODAY,F4,F5-BEG/END,F6-BAN".
           03  FILLER       PIC X(27)   VALUE
           "KRUPT,F7-DAILY,P1-2, P4-12:".
           03  FILLER       PIC X(40)   VALUE
           "F7,F2-CURRENT,F3-TODAY,F4-DIFF BEGMO,F5-".
           03  FILLER       PIC X(27)   VALUE
           "BEG/END,F6-BNKRP,P1-2,P4-12".
       01  WORLD-DEL-MSEL-TBL REDEFINES WORLD-DEL-MSEL-TABLE.
           03  WORLD-DEL-MSEL OCCURS 7 TIMES
                            PIC X(67).
       01  DEL-MSEL-LIST    PIC X(7)    VALUE "MSEL:P.".

       01  DEL-RECENCY    PIC X(11)  VALUE "DEL_TYPE:F.".  
       01  DEL-FNAME      PIC X(9)   VALUE "DEL_TYPE.".
       01  DEL-BUFFER     PIC X(80) VALUE " ".
       01  DELINQ         PIC 9 VALUE 7.
       01  DELTYP-COD     PIC 9 VALUE ZERO.
       01  DIFF-PER       PIC 9 VALUE ZERO.
           88  DIFF       VALUE 0.
           88  PER        VALUE 1.
       01  BNKRPT-DEL     PIC 9 VALUE 1.

      *-----------------------------------------------------------------
       01  DISPLAY-BUF3.
           03  DEL-PROMPT1                   PIC X(80).
           03  DEL-TABLE                     OCCURS 18.
               05  DEL-LN-PROMPT             PIC X(5).
               05  DEL-LN                    PIC X(75).
               05  FILLER                    REDEFINES DEL-LN.
                   07  DEL-REC-CON           OCCURS 2.
                       09  DEL-UNITS         PIC Z(6)9-     BLANK ZERO.
                       09  DEL-T-UNITS       REDEFINES DEL-UNITS
                                             PIC Z(6)9-.
                       09  FILLER            REDEFINES DEL-UNITS.
                           11 FILLER         PIC X(7).
                           11 DEL-UNITS-SIGN PIC X.
                       09  DEL-UNITS-P       REDEFINES DEL-UNITS
                                             PIC Z(4).99-    BLANK ZERO.
                       09  DEL-T-UNITS-P     REDEFINES DEL-UNITS
                                             PIC Z(4).99-.

                       09  FILLER            PIC X(2).

                       09  DEL-OUTBAL        PIC Z,ZZZ,ZZZ,ZZ9.99-
                                                 BLANK ZERO.
                       09  DEL-T-OUTBAL      REDEFINES DEL-OUTBAL
                                             PIC Z,ZZZ,ZZZ,ZZ9.99-.
                       09  FILLER            REDEFINES DEL-OUTBAL.
                           11 FILLER         PIC X(16).
                           11 DEL-OUTBAL-SIGN PIC X.

                       09  FILLER            PIC X.
                       09  DEL-PERCENT       PIC ZZ9.99-     BLANK ZERO.
                       09  FILLER            PIC X(2).

      *-----------------------------------------------------------------
       01  DISPLAY-LIST3.
           03  FILLER       PIC X(46) VALUE
           "P1:S 010:S 011 012 013 020:S 021 021B 022 023 ".
           03  FILLER       PIC X(20) VALUE
           "030:S 031 040:S 041 ".
           03  FILLER       PIC X(40) VALUE
           "050:S 051 060:S 061 070:S 071 080 081:S ".
           03  FILLER       PIC X(40) VALUE
           "090:S 091 100:S 101 110 111:S 120:S 121 ".
           03  FILLER       PIC X(40) VALUE
           "130 131:S 140:S 141 150:S 151 160:S 161 ".
           03  FILLER       PIC X(22) VALUE
           "170:S 171:S 180:S 181.".

      *=================================================================
      *
      *    P A G E - 3 - T O T A L S
      *
      *=================================================================
       01  DAILY-TYPE          PIC 9       VALUE 0.
           88  DAILY-TOP       VALUE 0.
           88  DAILY-BOTTOM    VALUE 1.

      *-----------------------------------------------------------------
       01  PAGE-3-TOTALS                   VALUE ZEROS.
           03  SUM-DEL-TABLE               OCCURS 10.
               05  SUM-DEL-REC-CON         OCCURS 2.
                   07  SUM-DEL-UNITS       PIC S9(6).
                   07  SUM-DEL-OUTBAL      PIC S9(10)V99.

      *-----------------------------------------------------------------
       01  PAGE-3-TOT-TABLE                VALUE ZEROS.
           03  TOT-DEL-TABLE               OCCURS 2.
               05  TOT-DEL-REC-CON         OCCURS 2.
                   07  TOT-DEL-UNITS       PIC S9(6)V9(7).
                   07  TOT-DEL-OUTBAL      PIC S9(10)V9(7).

      *-----------------------------------------------------------------
      *  USED TO COMPUTE DEL, SLOW AND TOT PERCENTAGES ON PAGE3,
      *     PERCENT OF CHANGE FROM BEGINNING OF MONTH.
      *     COMPUTED:
      *
      *  HOLD-TOT-DEL-UNITS (1 RECCON) =
      *   SUM-DEL-UNITS(AGESUB RECCON) - SUM-REL-DEL-UNITS(AGESUB RECCON).
      *
      *   HOLD-TOT-DEL-UNITS (1 RECCON) =
      *    HOLD-OT-DEL-UNITS (2 RECCON) + SUM-REL-DEL-UNITS (SUB3 RECCON).
      *
      *                             HOLD-TOT-DEL-UNITS(1 RECCON)
      *   TOT-DEL-UNITS(1 RECCON) = ---------------------------- * 100.
      *                             HOLD-TOT-DEL-UNITS(2 RECCON)
      *
      *     (SAME FORMAT FOR OUTBAL.)
      *
      *  HOLD-TOT-DEL-TABLE:    LEVEL 1 : CHANGE FROM BEGIN OF MONTH.
      *                               2 : BEGINNING OF MONTH.
      *
      *  HOLD-TOT-DEL-REC-CON:        1 : RECENCY.
      *                               2 : CONTRACTUAL.
      *-----------------------------------------------------------------
       01  HOLD-PAGE-3-TOT-TABLE            VALUE ZEROS.
           03  HOLD-TOT-DEL-TABLE           OCCURS 2.
               05  HOLD-TOT-DEL-REC-CON     OCCURS 2.
                   07  HOLD-TOT-DEL-UNITS   PIC S9(6)V9(7).
                   07  HOLD-TOT-DEL-OUTBAL  PIC S9(10)V9(7).

      *-----------------------------------------------------------------
       01  PAGE-3-REL-TOTALS               VALUE ZEROS.
           03  SUM-REL-DEL-TABLE           OCCURS 10.
               05  SUM-REL-DEL-REC-CON     OCCURS 2.
                   07  SUM-REL-DEL-UNITS   PIC S9(6).
                   07  SUM-REL-DEL-OUTBAL  PIC S9(10)V99.

      *-----------------------------------------------------------------
       01  PAGE-3-DAILY-TOTALS             VALUE ZEROS.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *        (1) 30 DAYS
      *        (2) 60 DAYS
      *        (3) 90 +
      *        (4) <M>
      *        (5) <P>
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           03  SUM-DAILY-TABLE             OCCURS 5.
               05  SUM-DAILY-REC-CON       OCCURS 2.
                   07  SUM-DAILY-UNITS     PIC S9(6).
                   07  SUM-DAILY-OUTBAL    PIC S9(10)V99.
           03  SUM-DAILY-NO                PIC S9(9).
           03  SUM-DAILY-AMT               PIC S9(10)V99.
           03  SUM-DAILY-VOLNO             PIC S9(9).
           03  SUM-DAILY-VOL               PIC S9(10)V99.
           03  SUM-DAILY-SUMWGTTM          PIC S9(12).
           03  SUM-DAILY-SUMWGTAV          PIC S9(14)V99.
           03  SUM-DAILY-SUMWGTTA          PIC S9(14)V99.
           03  SUM-DAILY-NB                PIC S99999.
           03  SUM-DAILY-FB                PIC S99999.
           03  SUM-DAILY-PB                PIC S999999.
           03  SUM-DAILY-NB-AMT            PIC S9(9)V99.
           03  SUM-DAILY-FB-AMT            PIC S9(9)V99.
           03  SUM-DAILY-PB-AMT            PIC S99(9)V99.
           03  SUM-DAILY-PL-AMT            PIC S9(11)V99.
           03  SUM-DAILY-PL-NO             PIC S999999.
           03  SUM-DAILY-OUTBAL-RECENCY60  PIC S9(11)V99.          
           03  SUM-DAILY-REL-OUTBAL-RECENCY60     PIC S9(11)V99. 
           03  SUM-DAILY-REL-UNITS-RECENCY60      PIC S9(11).           
           03  SUM-DAILY-UNITS-RECENCY60  PIC S9(11).        

      *-----------------------------------------------------------------
       01  PAGE-3-DAILY-TOT-TABLE          VALUE ZEROS.
           03  TOT-DAILY-TABLE             OCCURS 2.
               05  TOT-DAILY-REC-CON       OCCURS 2.
                   07  TOT-DAILY-UNITS     PIC S9(6)V9(7).
                   07  TOT-DAILY-P-UNITS   PIC S9(4)V9(2).
                   07  TOT-DAILY-OUTBAL    PIC S9(10)V9(7).
                   07  TOT-DAILY-P-OUTBAL  PIC S9(4)V9(2).

      *-----------------------------------------------------------------
       01  PAGE-3-REL-DAILY-TOTALS           VALUE ZEROS.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *        (1) 30 DAYS
      *        (2) 60 DAYS
      *        (3) 90 +
      *        (4) <M>
      *        (5) <P>
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           03  SUM-REL-DAILY-TABLE           OCCURS 5.
               05  SUM-REL-DAILY-REC-CON     OCCURS 2.
                   07  SUM-REL-DAILY-UNITS   PIC S9(6).
                   07  SUM-REL-DAILY-OUTBAL  PIC S9(10)V99.
           03  SUM-REL-DAILY-NO              PIC S9(9).
           03  SUM-REL-DAILY-AMT             PIC S9(10)V99.

      *-----------------------------------------------------------------
       01  PAGE-3-REL-DAILY-TOT-TABLE         VALUE ZEROS.
           03  TOT-REL-DAILY-TABLE            OCCURS 2.
               05  TOT-REL-DAILY-REC-CON      OCCURS 2.
                   07  TOT-REL-DAILY-UNITS    PIC S9(6)V9(7).
                   07  TOT-REL-DAILY-P-UNITS  PIC S9(4)V9(2).
                   07  TOT-REL-DAILY-OUTBAL   PIC S9(10)V9(7).
                   07  TOT-REL-DAILY-P-OUTBAL PIC S9(4)V9(2).

      *-----------------------------------------------------------------
       01  UNITS-DIFF                      PIC S9(6)      VALUE ZERO.
       01  OUTBAL-DIFF                     PIC S9(10)V99   VALUE ZERO.

       01  UNITS-PER                       PIC S9(6)V9(7) VALUE ZERO.
       01  OUTBAL-PER                      PIC S9(10)V9(7) VALUE ZERO.

      *=================================================================
      *
      *    PAGE 3 DAILY
      *
      *=================================================================
       01  DAILY-BUF3.
           03  DAILY-PROMPT1             PIC X(80).
           03  FILLER.
               05  DAILY-PROMPT2         PIC X(67).
               05  DAILY-INC-REMAIN-P    PIC X(12).
               05  FILLER                PIC X.
           03  DAILY-OUTBALS-FILLER.
               05  DAILY-OSBAL           PIC ZZZZ,ZZZ,ZZZ-.
               05  DAILY-OSNO            PIC ZZZZ,ZZZ-.
               05  DAILY-OSAVG           PIC ZZZZZ.

               05  DAILY-G-L-BAL         PIC ZZZ,ZZZ,ZZZ-.
               05  DAILY-G-L-NO          PIC ZZZZ,ZZZ-.

      * MTD P&L (SUBTRACTS OUT P2S)
               05  DAILY-AMT-PL          PIC ZZZZZZ,ZZZ-.
               05  DAILY-NO-PL           PIC ZZZZZZ-.
               05  FILLER                PIC X.
      * INCREASE REMAINING
               05  DAILY-INC-REMAIN      PIC ZZZ,ZZZ,ZZZ-.
               05  DAILY-NFR-90          REDEFINES DAILY-INC-REMAIN
                                         PIC ZZZ,ZZZ,ZZZ-.
               05  FILLER                PIC X.
           03  DAILY-PROMPT3             PIC X(80).
           03  DAILY-PROMPT4             PIC X(80).
           03  DAILY-VOLS-FILLER.
               05  DAILY-VOL-AVG-LN      PIC ZZZZ,ZZZ.
               05  DAILY-VOL-AVG-TM      PIC ZZZZ.
               05  DAILY-VOL-BAL         PIC ZZZ,ZZZ,ZZZ.
               05  DAILY-VOL-BAL-BIG REDEFINES DAILY-VOL-BAL
                                         PIC ZZZZZZZ,ZZZ.

               05  DAILY-NB-NO           PIC ZZZZZZ-.
               05  DAILY-NB-PER          PIC ZZZ.Z-.

               05  DAILY-FB-NO           PIC ZZZZZZ-.
               05  DAILY-RN-PER          PIC ZZZ.Z-.

               05  DAILY-PB-NO           PIC ZZZZZZ-.
               05  DAILY-PB-NO6          REDEFINES DAILY-PB-NO
                                         PIC ZZZZZZZ.
               05  DAILY-PB-PER          PIC ZZZ.Z-.

               05  DAILY-NB-AVG          PIC ZZZZZ-.
               05  DAILY-FB-AVG          PIC ZZZZZ-.
               05  DAILY-PB-AVG          PIC ZZZZZ-.
           03  DAILY-PROMPT5             PIC X(80).
           03  DAILY-TABLE               OCCURS 12.
               05  DAILY-DELTBL.
                   07  DAILY-LN-PROMPT   PIC X(5).
                   07  DAILY-LN          PIC X(75).
                   07  DAILY-DELINQ-FILLER   REDEFINES DAILY-LN.
                       09  DAILY-REC-CON     OCCURS 2.
                           11  DAILY-UNITS   PIC Z(6)9-  BLANK ZERO.
                           11  DAILY-T-UNITS REDEFINES DAILY-UNITS
                                             PIC Z(6)9-.
                           11  FILLER        REDEFINES DAILY-UNITS.
                               13  DAILY-UNITS-X    PIC X(7).
                               13  DAILY-UNITS-SIGN PIC X.
                           11  FILLER         PIC X.
                           11  DAILY-P-UNITS  PIC ZZ9.9- BLANK ZERO.
                           11  FILLER         REDEFINES DAILY-P-UNITS.
                               13  DAILY-P-UNITS-X    PIC X(5).
                               13  DAILY-P-UNITS-SIGN PIC X.
                           11  FILLER         PIC X.

                           11  DAILY-OUTBAL   PIC Z,ZZZ,ZZZ,ZZZ-
                                                         BLANK ZERO.
                           11  DAILY-T-OUTBAL REDEFINES DAILY-OUTBAL
                                              PIC Z,ZZZ,ZZZ,ZZ9-.
                           11  FILLER         REDEFINES DAILY-OUTBAL.
                               13  DAILY-OUTBAL-X    PIC X(13).
                               13  DAILY-OUTBAL-SIGN PIC X.
                           11  FILLER         PIC X.
                           11  DAILY-P-OUTBAL PIC ZZ9.9- BLANK ZERO.
                           11  FILLER         REDEFINES DAILY-P-OUTBAL.
                               13  DAILY-P-OUTBAL-X    PIC X(5).
                               13  DAILY-P-OUTBAL-SIGN PIC X.

      *-----------------------------------------------------------------
       01  DAILY-LIST3-TOP.
           03  FILLER              PIC X(5)  VALUE
           "P1:P ".
           03  FILLER              PIC X(12) VALUE
           "010:P 011:P ".
           03  DAILY-LIST3-NFR60-T PIC X(7)  VALUE
           "012:RF ".
           03  FILLER              PIC X(17) VALUE
           "013 020 021 021B ".
           03  DAILY-LIST3-NFR90-T PIC X(7)  VALUE
           "022:RF ".
           03  FILLER       PIC X(28) VALUE
           "023 030:P 031:P 040:P 041:P ".
           03  FILLER       PIC X(40) VALUE
           "050 051 060:P 061:P 070:P 071 080:P 081 ".
           03  FILLER       PIC X(40) VALUE
           "090:P 091 100:P 101 110:P 111 120:P 121 ".
           03  FILLER       PIC X(40) VALUE
           "130:P 131 140:P 141 150:P 151 160 161:P ".
           03  FILLER       PIC X(24) VALUE
           "170 171:P 180:P 181.".

       01  DAILY-LIST3-TOP-WORLD-RECENCY.
           03  FILLER              PIC X(5)  VALUE
           "P1:P ".
           03  FILLER              PIC X(12) VALUE
           "010:P 011:P ".
           03  DAILY-LIST3-NFR60-T-W PIC X(7)  VALUE
           "012:RF ".
           03  FILLER              PIC X(21) VALUE
           "013 020:R 021:R 021B ".
           03  DAILY-LIST3-NFR90-T-W PIC X(7)  VALUE
           "022:RF ".
           03  FILLER       PIC X(28) VALUE
           "023 030:P 031:P 040:P 041:P ".
           03  FILLER       PIC X(40) VALUE
           "050 051 060:P 061:P 070:P 071 080:P 081 ".
           03  FILLER       PIC X(40) VALUE
           "090:P 091 100:P 101 110:P 111 120:P 121 ".
           03  FILLER       PIC X(40) VALUE
           "130:P 131 140:P 141 150:P 151 160 161:P ".
           03  FILLER       PIC X(24) VALUE
           "170 171:P 180:P 181.".

      *-----------------------------------------------------------------
       01  DAILY-LIST3-BOTTOM.
           03  FILLER              PIC X(5)  VALUE
           "P1:P ".
           03  FILLER              PIC X(12) VALUE
           "010:P 011:P ".
           03  DAILY-LIST3-NFR60-B PIC X(7)  VALUE
           "012:RF ".
           03  FILLER              PIC X(17) VALUE
           "013 020 021 021B ".
           03  DAILY-LIST3-NFR90-B PIC X(7)  VALUE
           "022:RF ".
           03  FILLER       PIC X(28) VALUE
           "023 030:P 031:P 040:P 041:P ".
           03  FILLER       PIC X(40) VALUE
           "050 051 060:P 061:P 070 071:P 080 081:P ".
           03  FILLER       PIC X(40) VALUE
           "090:P 091 100:P 101 110:P 111 120 121:P ".
           03  FILLER       PIC X(40) VALUE
           "130:P 131 140:P 141 150:P 151 160 161:P ".
           03  FILLER       PIC X(24) VALUE
           "170 171:P 180 181:P.".

       01  DAILY-LIST3-BOT-WORLD-RECENCY.
           03  FILLER              PIC X(5)  VALUE
           "P1:P ".
           03  FILLER              PIC X(12) VALUE
           "010:P 011:P ".
           03  DAILY-LIST3-NFR60-B-W PIC X(7)  VALUE
           "012:RF ".
           03  FILLER              PIC X(21) VALUE
           "013 020:R 021:R 021B ".
           03  DAILY-LIST3-NFR90-B-W PIC X(7)  VALUE
           "022:RF ".
           03  FILLER       PIC X(28) VALUE
           "023 030:P 031:P 040:P 041:P ".
           03  FILLER       PIC X(40) VALUE
           "050 051 060:P 061:P 070 071:P 080 081:P ".
           03  FILLER       PIC X(40) VALUE
           "090:P 091 100:P 101 110:P 111 120 121:P ".
           03  FILLER       PIC X(40) VALUE
           "130:P 131 140:P 141 150:P 151 160 161:P ".
           03  FILLER       PIC X(24) VALUE
           "170 171:P 180 181:P.".

      *=================================================================
      *
      *    SCREEN 4 FIELDS
      *
      *=================================================================
       01  DISPLAY-BUF4               VALUE " ".
           03  DATA-TYPE-TABLE        OCCURS 5 TIMES.
               05  DATA-NOS-REPO      OCCURS 3 TIMES.
                   07  DATA-NO-REPO   PIC ZZZ,ZZ9-        BLANK ZERO.
               05  FILLER             PIC XX.
               05  DATA-AMTS-REPO     OCCURS 3 TIMES.
                   07  FILLER         PIC X.
                   07  DATA-AMT-REPO  PIC ZZ,ZZZ,ZZ9.99-  BLANK ZERO.

       01  DISPLAY-LIST4.
           03  FILLER                 PIC X(20) VALUE
           "010 020 030 040 050.".

      *=================================================================
      *
      *    SCREEN 4 TOTALS
      *
      *=================================================================
       01  PAGE-4-TOTALS.
           03  SUM-TABLE        OCCURS 5 TIMES.
               05  SUM-NOS-REPO      OCCURS 3 TIMES.
                   07  SUM-NO-REPO   PIC S9(6).
               05  SUM-AMTS-REPO     OCCURS 3 TIMES.
                   07  SUM-AMT-REPO  PIC S9(8)V99.

      *=================================================================
      *
      *    SCREEN 5 FIELDS
      *
      *=================================================================
       01  DISPLAY-BUF5    VALUE " ".
           03  DATA-TYPE-TABLE5  OCCURS 18 TIMES.
               05  DATA-AMTS5    OCCURS 2 TIMES.
                   07  DATA-AMT5 PIC ZZ,ZZZ,ZZ9.99- BLANK ZERO.

       01  DISPLAY-LIST5.
           03  FILLER     PIC X(32) VALUE
               "010 020 030 040 050 060 070 080 ".
           03  FILLER     PIC X(32) VALUE
               "090 100 110 120 130 140 150 160 ".
           03  FILLER     PIC X(32) VALUE
               "170 180 190 200 210 220 230 240 ".
           03  FILLER     PIC X(32) VALUE
               "250 260 270 280 290 300 310 320 ".
           03  FILLER     PIC X(16) VALUE
               "330 340 350 360.".

      *=================================================================
      *
      *    SCREEN 5 TOTALS
      *
      *=================================================================
       01  PAGE-5-TOTALS     VALUE ZERO.
           03  PAGE-5-TABLE  OCCURS 18 TIMES.
               05  SUM5      OCCURS 2 TIMES
                             PIC S9(8)V99.

      *=================================================================
      *
      *    SCREEN 6 FIELDS
      *
      *=================================================================
       01  DISPLAY-BUF6    VALUE " ".
           03  DATA-TYPE-TABLE6  OCCURS 2 TIMES.
               05  DATA-AMT6X OCCURS 2 TIMES.
                   07  DATA-AMT6 PIC ZZ,ZZZ,ZZ9.99- BLANK ZERO.
           03  DATA-TYPE-TABLE6A  OCCURS 10 TIMES.
               05  DATA-AMTS6A    OCCURS 2 TIMES.
                   07  DATA-AMT6A PIC ZZ,ZZZ,ZZ9- BLANK ZERO.
           03  DATA-TYPE-TABLE6B  OCCURS 4 TIMES.
               05  DATA-AMTS6B    OCCURS 2 TIMES.
                   07  DATA-AMT6B PIC ZZ,ZZZ,ZZ9.99- BLANK ZERO.

       01  DISPLAY-LIST6.
           03  FILLER     PIC X(32) VALUE
               "010 020 030 040 050 060 070 080 ".
           03  FILLER     PIC X(32) VALUE
               "090 100 110 120 130 140 150 160 ".
           03  FILLER     PIC X(32) VALUE
               "170 180 190 200 210 220 230 240 ".
           03  FILLER     PIC X(32) VALUE
               "250 260 270 280 290 300 310 320.".

      *=================================================================
      *
      *    SCREEN 6 TOTALS
      *
      *=================================================================
       01  PAGE-6-TOTALS     VALUE ZERO.
           03  PAGE-6-TABLE  OCCURS 2 TIMES.
               05  SUM6      OCCURS 2 TIMES
                             PIC S9(8)V99.
           03  PAGE-6-TABLEA  OCCURS 10 TIMES.
               05  SUM6A      OCCURS 2 TIMES
                             PIC S9(8).
           03  PAGE-6-TABLEB  OCCURS 4 TIMES.
               05  SUM6B      OCCURS 2 TIMES
                             PIC S9(8)V99.

      *=================================================================
      *
      *    SCREEN 7 FIELDS
      *
      *=================================================================
       01  DISPLAY-BUF7                VALUE " ".
           03  DATA-TYPE-TABLE         OCCURS 11 TIMES.
               05  FILLER              PIC XXXXX.
               05  DATA-NOS-PL         OCCURS 3 TIMES.
                   07  DATA-NO-PL      PIC ZZZZZZ9-          BLANK ZERO.
               05  DATA-AMTS-PL        OCCURS 3 TIMES.
                   07  DATA-AMT-PL     PIC ZZZZ,ZZZ,ZZ9.99-  BLANK ZERO.
           03  DATA-PART-CHGOFF-MTD    PIC ZZ,ZZZ,ZZ9.99-.
           03  DATA-PART-CHGOFF-YTD    PIC ZZ,ZZZ,ZZ9.99-.

       01  DISPLAY-LIST7.
           03  FILLER                  PIC X(44) VALUE
               "010 020 030 040 050 060 070 080 090 100 TOT ".
           03  FILLER                  PIC X(08) VALUE
               "110 111.".

      *=================================================================
      *
      *    SCREEN 7 TOTALS
      *
      *=================================================================
       01  PAGE-7-TOTALS.
           03  SUM-TABLE        OCCURS 11 TIMES.
               05  SUM-NOS-PL      OCCURS 3 TIMES.
                   07  SUM-NO-PL   PIC S9(7).
               05  SUM-AMTS-PL     OCCURS 3 TIMES.
                   07  SUM-AMT-PL  PIC S9(10)V99.

           03  SUM-PART-CHGOFF-MTD PIC S9(10)V99.
           03  SUM-PART-CHGOFF-YTD PIC S9(10)V99.

      *=================================================================
      *
      *    S C R E E N - 8 - F I E L D S
      *
      *=================================================================
       01  DISPLAY-BUF8    VALUE " ".
           03  DATA-TYPE-TABLE8  OCCURS 5 TIMES.
               05  DATA-PL-PRINCOL   PIC Z,ZZZ,ZZZ,ZZ9.99- BLANK ZERO.
               05  DATA-PL-INTCOL    PIC Z,ZZZ,ZZZ,ZZ9.99- BLANK ZERO.
               05  DATA-PL-LATECOL   PIC Z,ZZZ,ZZZ,ZZ9.99- BLANK ZERO.
               05  DATA-PL-O2COL     PIC Z,ZZZ,ZZZ,ZZ9.99- BLANK ZERO.
           03  DATA-PL-TOTAL1        PIC Z,ZZZ,ZZZ,ZZ9.99- BLANK ZERO.
           03  DATA-PL-TOTAL2        PIC Z,ZZZ,ZZZ,ZZ9.99- BLANK ZERO.

       01  DISPLAY-LIST8.
           03  FILLER     PIC X(32) VALUE
               "010 020 030 040 050 060 070 080 ".
           03  FILLER     PIC X(32) VALUE
               "090 100 110 120 130 140 150 160 ".
           03  FILLER     PIC X(26) VALUE
               "170 180 190 200 TOT1 TOT2.".

      **************************************************
      *    PAGE 8 TOTALS
      **************************************************
       01  PAGE-8-TOTALS     VALUE ZERO.
           03  PAGE-8-TABLE  OCCURS 5 TIMES.
               05  SUM-PL-PRINCOL      PIC S9(10)V99.
               05  SUM-PL-INTCOL       PIC S9(10)V99.
               05  SUM-PL-LATECOL      PIC S9(10)V99.
               05  SUM-PL-O2COL        PIC S9(10)V99.

       01  PAGE-8-TOTAL1               PIC S9(10)V99 COMP-3.
       01  PAGE-8-TOTAL2               PIC S9(10)V99 COMP-3.
      *=================================================================
      *
      *    SCREEN - 9 -  FIELDS
      *
      *=================================================================
       01  DISPLAY-BUF9               VALUE " ".
           03  DATA-TYPE-TABLE        OCCURS 3 TIMES.
               05  DATA-DLRVPRINX   OCCURS 3 TIMES.
                   07  DATA-DLRVPRIN  PIC Z,ZZZ,ZZZ,ZZ9.99- BLANK ZERO.
               05  DATA-DLRVCHGSX   OCCURS 3 TIMES.
                   07  DATA-DLRVCHGS  PIC Z,ZZZ,ZZZ,ZZ9.99- BLANK ZERO.
               05  DATA-DLPARVHELD    PIC Z,ZZZ,ZZZ,ZZ9.99- BLANK ZERO.
               05  DATA-DLTOTAL       PIC ZZZZZ,ZZZ,ZZ9.99- BLANK ZERO.
               05  DATA-DLDISC        PIC ZZZZZ,ZZZ,ZZ9.99- BLANK ZERO.

       01  DISPLAY-LIST9.
           03  FILLER       PIC X(20) VALUE
           "010 040 070 080 090 ".
           03  FILLER       PIC X(20) VALUE
           "020 050 071 081 091 ".
           03  FILLER       PIC X(20) VALUE
           "030 060 072 082 092.".

      *=================================================================
      *
      *    SCREEN 9 TOTALS
      *
      *=================================================================
       01  PAGE-9-TOTALS.
           03  SUM-TABLE        OCCURS 3 TIMES.
               05  SUM-DLRVPRINX   OCCURS 3 TIMES.
                   07  SUM-DLRVPRIN   PIC S9(10)V99.
               05  SUM-DLRVCHGSX   OCCURS 3 TIMES.
                   07  SUM-DLRVCHGS   PIC S9(10)V99.
               05  SUM-DLPARVHELD     PIC S9(10)V99.
               05  SUM-DLTOTAL        PIC S9(11)V99.
               05  SUM-DLDISC         PIC S9(11)V99.

      *=================================================================
      *
      *    S C R E E N - 1 0 - F I E L D S 
      *
      *=================================================================
       01  DISPLAY-BUF10          VALUE " ".
           03  DATA-TYPE-TABLE10   OCCURS 14 TIMES.
                   07  DATA-INS-NO-ELIG       PIC ZZ,ZZZ,ZZ9-.
                   07  DATA-INS-NO-WROTE      PIC ZZ,ZZZ,ZZ9-.
                   07  DATA-INS-NO-NOTWROTE   PIC ZZ,ZZZ,ZZ9-.
                   07  DATA-INS-AMT-WROTE     PIC Z,ZZZ,ZZZ,ZZ9.99-.

      *-----------------------------------------------------------------
       01  DISPLAY-LIST10.
           03  FILLER       PIC X(32) VALUE
           "010 011 012 013 020 021 022 023 ".
           03  FILLER       PIC X(32) VALUE
           "030 031 032 033 040 041 042 043 ".
           03  FILLER       PIC X(32) VALUE
           "050 051 052 053 060 061 062 063 ".
           03  FILLER       PIC X(32) VALUE
           "070 071 072 073 080 081 082 083 ".
           03  FILLER       PIC X(32) VALUE
           "090 091 092 093 100 101 102 103 ".
           03  FILLER       PIC X(32) VALUE
           "110 111 112 113 120 121 122 123 ".
           03  FILLER       PIC X(32) VALUE
           "130 131 132 133 140 141 142 143.".

      *=================================================================
      *
      *    P A G E - 1 0 - T O T A L S 
      *
      *=================================================================
       01  PAGE-10-TOTALS.
               05  SUM-INS-TABLE    OCCURS 14 TIMES.
                   07  SUM-INS-NO-ELIG       PIC S9(8).
                   07  SUM-INS-NO-WROTE      PIC S9(8).
                   07  SUM-INS-NO-NOTWROTE   PIC S9(8).
                   07  SUM-INS-AMT-WROTE     PIC S9(10)V99.

      *=================================================================
      *
      *    SCREEN DISPLAY BUFFER ----- PAGE 11 
      *
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    GNLS = GAIN/LOSS     NVOL = NEW VOLUME
      *=================================================================
       01  DISPLAY-BUF11.
           03  DATA-PERIOD-TABLE          OCCURS 3.
               05  DATA-TYPE-TABLE        OCCURS 10.
                   07  DISP-UNEARNED-AMT  PIC ZZZ,ZZZ,ZZZ,ZZZ.ZZ-.

       01  DISPLAY-LIST11.
           03  FILLER                     PIC X(40)     VALUE
               "010 020 030 040 050 060 070 080 090 100 ".
           03  FILLER                     PIC X(40)     VALUE
               "011 021 031 041 051 061 071 081 091 101 ".
           03  FILLER                     PIC X(40)     VALUE
               "012 022 032 042 052 062 072 082 092 102.".

      *=================================================================
      *
      *    P A G E - 1 1 - T O T A L S 
      *
      *=================================================================
       01  PAGE-11-TOTALS.
           03  SUM-11-PER-TBL            OCCURS 3.
               05  SUM-11-ACT-TBL            OCCURS 2.
                   07  SUM-11-TYP-TBL            OCCURS 10.
                       09  SUM-UNEARNED-AMT PIC S9(12)V99.

      *=================================================================
      *
      *    PAGE 12 -  DISPLAY FIELDS
      *
      *=================================================================
       01  DISPLAY-BUF12           VALUE " ".
           03  DATA-ELM-OUTS-BALANCE   PIC ZZZZZ,ZZZ,ZZZ.99- BLANK ZERO.
           03  DATA-ELY-OUTS-BALANCE   PIC ZZZZZ,ZZZ,ZZZ.99- BLANK ZERO.
           03  DATA-TABLE-12       OCCURS 2.
               05  DATA-PROFIT         PIC ZZZZZ,ZZZ,ZZZ.99- BLANK ZERO.
               05  DATA-PROFIT-PCT     PIC ZZZ.99-           BLANK ZERO.
               05  DATA-INCOME         PIC ZZZZZ,ZZZ,ZZZ.99- BLANK ZERO.
               05  DATA-INCOME-PCT     PIC ZZZ.99-           BLANK ZERO.
               05  DATA-INT-INCOME     PIC ZZZZZ,ZZZ,ZZZ.99- BLANK ZERO.
               05  DATA-INT-INCOME-PCT PIC ZZZ.99-           BLANK ZERO.
               05  DATA-INS-INCOME     PIC ZZZZZ,ZZZ,ZZZ.99- BLANK ZERO.
               05  DATA-INS-INCOME-PCT PIC ZZZ.99-           BLANK ZERO.
               05  DATA-EXPENSE        PIC ZZZZZ,ZZZ,ZZZ.99- BLANK ZERO.
               05  DATA-EXP-PER-LOAN   PIC Z,ZZZ,ZZZ.99-     BLANK ZERO.
               05  DATA-EXP-TO-OUTS    PIC ZZZ.99-           BLANK ZERO.
               05  DATA-EXP-TO-INCOME  PIC ZZZ.99-           BLANK ZERO.
               05  DATA-PERSONNEL      PIC ZZZZZ,ZZZ,ZZZ.99- BLANK ZERO.
               05  DATA-PERS-TO-INCOME PIC ZZZ.99-           BLANK ZERO.
               05  DATA-MGR-BONUS      PIC ZZZZZ,ZZZ,ZZZ.99- BLANK ZERO.
               05  DATA-BSR-BONUS      PIC ZZZZZ,ZZZ,ZZZ.99- BLANK ZERO.

       01  DISPLAY-LIST12.
           03  FILLER       PIC X(14) VALUE
           "ELMBAL ELYBAL ".
           03  FILLER       PIC X(32) VALUE
           "010 020 030 040 050 060 070 080 ".
           03  FILLER       PIC X(32) VALUE
           "090 100 110 120 130 140 150 160 ".
           03  FILLER       PIC X(32) VALUE
           "011 021 031 041 051 061 071 081 ".
           03  FILLER       PIC X(32) VALUE
           "091 101 111 121 131 141 151 161.".

      *=================================================================
      *
      *    P A G E   1 2  TOTALS 
      *
      *=================================================================
       01  PAGE-12-TOTALS.
           03  SUM-TABLE-12             OCCURS 2.
               05  SUM-PROFIT           PIC S9(13)V99.
               05  SUM-INCOME           PIC S9(13)V99.
               05  SUM-INT-INCOME       PIC S9(13)V99.
               05  SUM-INS-INCOME       PIC S9(13)V99.
               05  SUM-EXPENSE          PIC S9(13)V99.
               05  SUM-PERSONNEL        PIC S9(13)V99.
               05  SUM-NO-LOANS         PIC S9(13).
           03  SUM-ELM-OUTS-BALANCE     PIC S9(13)V99.
           03  SUM-ELY-OUTS-BALANCE     PIC S9(13)V99.
           03  SUM-YTD-PCT-PROFIT       PIC S9(3)V99.
           03  SUM-YTD-PCT-INCOME       PIC S9(3)V99.
           03  SUM-YTD-PCT-INT-INC      PIC S9(3)V99.
           03  SUM-YTD-PCT-INS-INC      PIC S9(3)V99.
           03  SUM-YTD-PCT-EXPENSE      PIC S9(3)V99.
           03  SUM-MGR-BONUS            PIC S9(13)V99.
           03  SUM-BSR-BONUS            PIC S9(13)V99.

      *----------------------------------------------------------------
      * HOLD BY-REC: CONTAINS ACCUMULATED NUMBERS FROM BY-RECORDS READ
      *              IF RUNNING IN SNAPSHOT MODE THIS RECORD IS BUILT ONCE
      *                 AND THEN USED UNTIL A NEW RANGE IS ENTERED.
      *              IF RUNNING IN REAL TIME THIS RECORD IS BUILT EACH
      *                 TIME A NEW ACTIVITY STARTS (NEW PAGE, FUNCTION
      *                 KEY, ETC.
      *----------------------------------------------------------------
      *    PERIOD TABLE:  ____________________________________________________
      *                   1 - CURRENT        [BYCUR] |  1 - MTD        [BYMTD]
      *                   2 - END LAST MONTH [BYELM] |  2 - YTD        [BYYTD]
      *                   3 - END LAST YEAR  [BYELY] |  3 - LYR        [BYLYR]
      *                                              |
      *    ACTIVITY TBL:  1 - NET GAIN/LOSS  [BYNET] |  2 - NEW VOLUME [BYVOL]
      *                   ____________________________________________________
      *    TYPE TABLE  :  1 - NB                7 - RN
      *                   2 - RF                8 - SC
      *                   3 - FB                9 - OT
      *                   4 - PB               10 - RB
      *                   5 - PB DELINQ        11 - RO
      *                   6 - PB INCR          12 - TOTAL OF ALL LEVELS
      *    -------------------------------------------------------------
       01  HBY-REC.
               05  HBY-PERIOD-TBL              OCCURS 3.
                   07  HBY-ACTIVITY-TBL        OCCURS 2.
                       09  HBY-TYPE-TBL        OCCURS 12.
                           11  HBY-NO          PIC S9(9)    COMP.
                           11  HBY-AMT         PIC S9(10)V99 COMP.
                           11  HBY-O2          PIC S9(10)V99 COMP.
                           11  HBY-INT         PIC S9(10)V99 COMP.
      *----------------------------------------------------------------------
      *    REPO TABLE:
      *    PERIOD TABLE:  ____________________________________________________
      *                   1 - CURRENT        [BYCUR] |  1 - MTD        [BYMTD]
      *                   2 - END LAST MONTH [BYELM] |  2 - YTD        [BYYTD]
      *                   3 - END LAST YEAR  [BYELY] |  3 - LYR        [BYLYR]
      *
      *    ACTIVITY TBL:  1 - NET GAIN / LOSS
      *                   2 - NEW VOLUME
      *
      *    TYPE TABLE  :  1 - REPO ON HAND
      *                   2 - REPO REDEEMED
      *                   3 - REPO SOLD LIVE RECEIVABLE
      *                   4 - N/A
      *                   5 - N/A
      *
      *----------------------------------------------------------------------
               05  HBY-REPO-PERIOD-TBL         OCCURS 3.
                   07  HBY-REPO-ACTIVITY-TBL   OCCURS 2.
                       09  HBY-REPO-TYPE-TBL   OCCURS 5.
                           11  HBY-NO-REPO     PIC S9(9)    COMP.
                           11  HBY-AMT-REPO    PIC S9(10)V99 COMP.
                           11  HBY-O2-REPO     PIC S9(10)V99 COMP.
                           11  HBY-INT-REPO    PIC S9(10)V99 COMP.

      *----------------------------------------------------------------------
      * PL PERIOD TABLE: ----------------------------------------------------
      *                  1 - CURRENT        [BYCUR] | 1 - MTD        [BYMTD]
      *                  2 - END LAST MONTH [BYELM] | 2 - YTD        [BYYTD]
      *                  3 - END LAST YEAR  [BYELY] | 3 - LYR        [BYLYR]
      *                                             |
      * PL ACTIVITY TBL:  1 - NET GAIN/LOSS [BYNET] | 2 - NEW VOLUME [BYVOL]
      *                   ---------------------------------------------------
      * PL TYPE TABLE  :  1 - PL ACTIVE         2 - PL INACTIVE
      *                   3 - P1 ACTIVE         4 - P1 INACTIVE
      *                   5 - P3 ACTIVE         6 - P3 INACTIVE
      *                   7 - P4 ACTIVE         8 - P4 INACTIVE
      *                   9 - P5 ACTIVE        10 - P5 INACTIVE
      *            NOTE: HBY-AMT-PL INCLUDES HBY-02-PL
      *----------------------------------------------------------------------
               05  HBY-PL-PERIOD-TBL              OCCURS 3.
                   07  HBY-PL-ACTIVITY-TBL        OCCURS 2.
                       09  HBY-PL-TYPE-TBL        OCCURS 10.
                           11  HBY-NO-PL          PIC S9(9)    COMP.
                           11  HBY-AMT-PL         PIC S9(10)V99 COMP.
                           11  HBY-O2-PL          PIC S9(10)V99 COMP.
                           11  HBY-INT-PL         PIC S9(10)V99 COMP.

      *----------------------------------------------------------------------
      * DLR PERIOD TABLE: ----------------------------------------------------
      *                   1 - CURRENT        [BYCUR] | 1 - MTD        [BYMTD]
      *                   2 - END LAST MONTH [BYELM] | 2 - YTD        [BYYTD]
      *                   3 - END LAST YEAR  [BYELY] | 3 - LYR        [BYLYR]
      *                                              |
      * DLR ACTIVITY TBL: 1 - NET GAIN/LOSS [BYNET]  | 2 - NEW VOLUME [BYVOL]
      *
      *----------------------------------------------------------------------
               05  HBY-DLR-PERIOD-TBL              OCCURS 3.
                   07  HBY-DLR-ACTIVITY-TBL        OCCURS 2.
                       09  HBY-DLRVPRIN   OCCURS 3
                                              PIC S9(10)V99 COMP.
                       09  HBY-DLRVCHGS   OCCURS 3
                                              PIC S9(10)V99 COMP.
                       09  HBY-DLPARVHELD     PIC S9(10)V99 COMP.

      *    -------------------------------------------------
      *    SUBSCRIPTED FOR MTD AND YTD FIGURE
      *        1- MTD                     [BYMTD]
      *        2- YTD                     [BYYTD]
      *
      *    NOTE: HBY-PL-PRINCOL INCLUDES HBY-PL-O2COL
      *    -------------------------------------------------
               05  HBY-MTD-YTD                 OCCURS 2.

      *            -----------------------------------------
      *                HBY-PL-RECOVERED    1 - PL RECOVERIES
      *                                   2 - P2 RECOVERIES
      *                                   3 - P3 RECOVERIES
      *                                   4 - P4 RECOVERIES
      *                                   5 - P5 RECOVERIES
      *            -----------------------------------------
                   07  HBY-PL-RECOVERED OCCURS 5.
                       09  HBY-PL-PRINCOL      PIC S9(10)V99 COMP.
                       09  HBY-PL-INTCOL       PIC S9(10)V99 COMP.
                       09  HBY-PL-LATECOL      PIC S9(10)V99 COMP.
                       09  HBY-PL-O2COL        PIC S9(10)V99 COMP.

      *----------------------------------------------------------------
      * SOLD  : LOANS SOLD/TRANSFERRED VIA BULKTR.C
      * TRANS : TRANSFERRED TO A GIVEN BRANCH VIA BULKTR.C
      * PURCH : MIDSTREAM PURCHASE FROM OUTSIDE SOURCE.
      *----------------------------------------------------------------
                   07  HBY-AMT-SOLD            PIC S9(10)V99 COMP.
                   07  HBY-AMT-TRANS           PIC S9(10)V99 COMP.
                   07  HBY-AMT-PURCH           PIC S9(10)V99 COMP.

                   07  HBY-NO-SOLD             PIC S9(7)    COMP.
                   07  HBY-NO-TRANS            PIC S9(7)    COMP.
                   07  HBY-NO-PURCH            PIC S9(7)    COMP.

                   07  HBY-OTHCOL              PIC S9(10)V99 COMP.
                   07  HBY-OT2COL              PIC S9(10)V99 COMP.
                   07  HBY-INTCOL              PIC S9(10)V99 COMP.
                   07  HBY-LATECOL             PIC S9(10)V99 COMP.
                   07  HBY-PRINCOL             PIC S9(10)V99 COMP.
                   07  HBY-DEFCOL              PIC S9(10)V99 COMP.
                   07  HBY-RNPRIN              PIC S9(10)V99 COMP.
                   07  HBY-INTREB              PIC S9(10)V99 COMP.
                   07  HBY-CLREB1              PIC S9(10)V99 COMP.
                   07  HBY-CLREB2              PIC S9(10)V99 COMP.
                   07  HBY-CLREB3              PIC S9(10)V99 COMP.
                   07  HBY-CLREB4              PIC S9(10)V99 COMP.
                   07  HBY-AHREB               PIC S9(10)V99 COMP.
                   07  HBY-PPREB               PIC S9(10)V99 COMP.
                   07  HBY-O1REB               PIC S9(10)V99 COMP.
                   07  HBY-O2REB               PIC S9(10)V99 COMP.
                   07  HBY-O3REB               PIC S9(10)V99 COMP.
                   07  HBY-O4REB               PIC S9(10)V99 COMP.
                   07  HBY-O5REB               PIC S9(10)V99 COMP.
                   07  HBY-O6REB               PIC S9(10)V99 COMP.
                   07  HBY-O7REB               PIC S9(10)V99 COMP.
                   07  HBY-SCREB               PIC S9(10)V99 COMP.
                   07  HBY-MFREB               PIC S9(10)V99 COMP.
                   07  HBY-DFREB               PIC S9(10)V99 COMP.
                   07  HBY-PART-CHGOFF               PIC S9(10)V99 COMP.
                   07  HBY-ALLOTMENTS          PIC S9(10)V99 COMP.
                   07  HBY-LOCKBOX             PIC S9(10)V99 COMP.
                   07  HBY-INS-TYPE            OCCURS 10.
                       09  HBY-INS-NO-ELIG     PIC S9(8)    COMP.
                       09  HBY-INS-NO-WROTE    PIC S9(8)    COMP.
                       09  HBY-INS-NO-NOTWROTE PIC S9(8)    COMP.
                       09  HBY-INS-AMT-WROTE   PIC S9(10)V99 COMP.
                   07  HBY-FEE-TYPE            OCCURS 4.
                       09  HBY-FEE-NO-ELIG     PIC S9(8)    COMP.
                       09  HBY-FEE-NO-WROTE    PIC S9(8)    COMP.
                       09  HBY-FEE-NO-NOTWROTE PIC S9(8)    COMP.
                       09  HBY-FEE-AMT-WROTE   PIC S9(10)V99 COMP.
                   07  HBY-OTH-NO-COL          PIC S9(8)    COMP.
                   07  HBY-OT2-NO-COL          PIC S9(8)    COMP.
                   07  HBY-INT-NO-COL          PIC S9(8)    COMP.
                   07  HBY-LATE-NO-COL         PIC S9(8)    COMP.

                   07  HBY-OTH-NO-WAIVED       PIC S9(8)    COMP.
                   07  HBY-OT2-NO-WAIVED       PIC S9(8)    COMP.
                   07  HBY-INT-NO-WAIVED       PIC S9(8)    COMP.
                   07  HBY-LATE-NO-WAIVED      PIC S9(8)    COMP.

                   07  HBY-OTH-AMT-WAIVED      PIC S9(10)V99 COMP.
                   07  HBY-OT2-AMT-WAIVED      PIC S9(10)V99 COMP.
                   07  HBY-INT-AMT-WAIVED      PIC S9(10)V99 COMP.
                   07  HBY-LATE-AMT-WAIVED     PIC S9(10)V99 COMP.
      *    -------------------------------------------------
      *    HBY-NOW-BMO-LME
      *        1- NOW   (CURRENT)         [BYNOW]
      *        2- BEGINNING OF MONTH      [BYBMO]
      *        3- LAST MONTH END          [BYLME]
      *
      *    HBY-REC-CON
      *        1- RECENCY                 [BYREC]
      *        2- CONTRACTUAL             [BYCON]
      *
      *    HBY-DEL-BUCKET
      *        1- 30
      *        2- 60
      *        3- 90
      *        4- 120
      *        5- 150
      *        6- 180
      *        7- 210+
      *        8- <M>
      *        9- <P>
      *       10- CURRENT
      *
      *    -------------------------------------------------
               05  HBY-NOW-BMO-LME          OCCURS 3.
                   07  HBY-REC-CON          OCCURS 2.
                       09  HBY-DEL-BUCKET   OCCURS 10.
      *                DELINQUENT UNITS:
                           11  HBY-UNITS    PIC S9(9)    COMP.
      *                OUTSTANDING BALANCE:
                           11  HBY-OUTBAL   PIC S9(11)V99 COMP.
      *    ------------------------------------------------------------
      *    NOTE:  THESE FIELDS ARE BANKRUPT, NON P&L, NON REPO ACCOUNTS
      *
      *    HBY-NOW-BMO-LME-BNKRPT
      *        1- NOW   (CURRENT)         [BYNOW]
      *        2- BEGINNING OF MONTH      [BYBMO]
      *        3- LAST MONTH END          [BYLME]
      *
      *    HBY-REC-CON-BNKRPT
      *        1- RECENCY                 [BYREC]
      *        2- CONTRACTUAL             [BYCON]
      *
      *    HBY-DEL-BUCKET-BNKRPT
      *        1- 30
      *        2- 60
      *        3- 90
      *        4- 120
      *        5- 150
      *        6- 180
      *        7- 210+
      *        8- <M>
      *        9- <P>
      *       10- CURRENT
      *
      *    -------------------------------------------------
               05  HBY-NOW-BMO-LME-BNKRPT          OCCURS 3.
                   07  HBY-REC-CON-BNKRPT          OCCURS 2.
                       09  HBY-DEL-BUCKET-BNKRPT   OCCURS 10.
      *                DELINQUENT UNITS:
                           11  HBY-UNITS-BNKRPT   PIC S9(9)    COMP.
      *                OUTSTANDING BALANCE:
                           11  HBY-OUTBAL-BNKRPT  PIC S9(11)V99 COMP.
      *    -------------------------------------------------
               05  HBY-SUMWGTTM                PIC S9(15)    COMP.
               05  HBY-SUMWGTAV                PIC S9(15)V99 COMP.
               05  HBY-SUMWGTTA                PIC S9(15)V99 COMP.
               05  HBY-IB-ACCRUED              PIC S9(10)V99 COMP.

      *-------------------- APPLICATIONS -------------------------------
      *    PERIOD TABLE:  ____________________________________________________
      *                   1 - CURRENT        [BYCUR] |  1 - MTD        [BYMTD]
      *                   2 - END LAST MONTH [BYELM] |  2 - YTD        [BYYTD]
      *                   3 - END LAST YEAR  [BYELY] |  3 - LYR        [BYLYR]
      *                                              |
      *    ACTIVITY TBL:  1 - NET GAIN/LOSS  [BYNET] |  2 - NEW VOLUME [BYVOL]
      *                   ____________________________________________________
      *    TYPE TABLE  :  1 - NB                7 - RN
      *                   2 - RF                8 - SC
      *                   3 - FB                9 - OT
      *                   4 - PB               10 - RB
      *                   5 - PB DELINQ        11 - RO
      *                   6 - PB INCR          12 - TOTAL OF ALL LEVELS
      *
      *    NOTE: STATISTICS FOR:
      *         APPS MADE IS THE ACTUAL LOAN VOLUME CARRIED UNDER HBY-NO
      *         APPS NOT TAKEN IS NOT EASILY MAINTAINED
      *
      *-----------------------------------------------------------------

      *-----------------------------------------------------------------
      *    DLDISC-VOL-TBL: 1 - MTD                  NEW VOLUMES
      *                    2 - YTD
      *                    3 - LYR
      *
      *-----------------------------------------------------------------
           03  HBY-DLDISC-VOL-TBL                  OCCURS 3.
               05 HBY-DLDISC                       PIC S9(9)V99 COMP.
      *    ------------------------------------------------------
      *    UNEARNED PERIOD TABLE:  ___________________________________________
      *                   1 - CURRENT                |  1 - MTD
      *                   2 - END LAST MONTH         |  2 - YTD
      *                   3 - END LAST YEAR          |  3 - LYR
      *                                              |
      *    UNEARNED ACT:  1 - NET GAIN/LOSS          |  2 - NEW VOLUME
      *                   ____________________________________________________
      *    UNEARNED TABLE: 1 - INTEREST
      *                    2 - ADDON
      *                    3 - SERVICE CHARGE
      *                    4 - MAINTENANCE FEES
      *                    5 - DEALER DISCOUNT
      *                    6 - OTHER
      *    ------------------------------------------------------
           03  HBY-UNEARNED-PERIOD                OCCURS 3.
               05  HBY-UNEARNED-ACTIVITY          OCCURS 2.
                   07  HBY-UNEARNED-TYPE          OCCURS 10.
                       09  HBY-UNEARNED-AMT       PIC S9(11)V99 COMP.

           03  HBY-PROCEEDS    OCCURS 2           PIC S9(11)V99 COMP.

      *----------------------------------------------------------------
      * HOLD TY-REC: CONTAINS ACCUMULATED NUMBERS FROM TY-RECORDS READ
      *              IF RUNNING IN SNAPSHOT MODE THIS RECORD IS BUILT ONCE
      *                 AND THEN USED UNTIL A NEW RANGE IS ENTERED.
      *              IF RUNNING IN REAL TIME THIS RECORD IS BUILT EACH
      *                 TIME A NEW ACTIVITY STARTS (NEW PAGE, FUNCTION
      *                 KEY, ETC.
      *----------------------------------------------------------------
      *
      *    ACTIVITY TABLE: 1 - NET GAIN / LOSS
      *                    2 - NEW VOLUME
      *
      *    TYPE TABLE  :  1 - NB           7 - RN
      *                   2 - RF           8 - SC
      *                   3 - FB           9 - OT
      *                   4 - PB          10 - RB
      *                   5 - PB DELINQ   11 - RO
      *                   6 - PB INCR     12 - TOTAL OF ALL LEVELS
      *
      *----------------------------------------------------------------
       01 HTY-REC.
           03  HTY-ACTIVITY-TBL        OCCURS 2.
               05  HTY-TYPE-TBL        OCCURS 12.
                   07  HTY-NO          PIC S9(9)    COMP.
                   07  HTY-AMT         PIC S9(10)V99 COMP.
                   07  HTY-O2          PIC S9(10)V99 COMP.
                   07  HTY-INT         PIC S9(10)V99 COMP.
      *----------------------------------------------------------------
      *    REPO TABLE:
      *
      *    ACTIVITY TABLE:  1 - NET GAIN / LOSS
      *                     2 - NEW VOLUME
      *
      *    TYPE TABLE    :  1 - REPO ON HAND
      *                     2 - REPO REDEEMED
      *                     3 - REPO SOLD LIVE RECEIVABLE
      *                     4 - N/A
      *                     5 - N/A
      *
      *----------------------------------------------------------------
           03  HTY-REPO-ACTIVITY-TBL   OCCURS 2.
               05  HTY-REPO-TYPE-TBL   OCCURS 5.
                   07  HTY-NO-REPO     PIC S9(9)    COMP.
                   07  HTY-AMT-REPO    PIC S9(10)V99 COMP.
                   07  HTY-O2-REPO     PIC S9(10)V99 COMP.
                   07  HTY-INT-REPO    PIC S9(10)V99 COMP.
      *----------------------------------------------------------------------
      * PL ACTIVITY TBL:  1 - NET GAIN/LOSS [BYNET] | 2 - NEW VOLUME [BYVOL]
      *                   ---------------------------------------------------
      * PL TYPE TABLE  :  1 - PL ACTIVE         2 - PL INACTIVE
      *                   3 - P2 ACTIVE         4 - P2 INACTIVE
      *                   5 - P3 ACTIVE         6 - P3 INACTIVE
      *                   7 - P4 ACTIVE         8 - P4 INACTIVE
      *                   9 - P5 ACTIVE        10 - P5 INACTIVE
      *----------------------------------------------------------------------
           03  HTY-PL-ACTIVITY-TBL        OCCURS 2.
               05  HTY-PL-TYPE-TBL        OCCURS 10.
                   07  HTY-NO-PL          PIC S9(9)    COMP.
                   07  HTY-AMT-PL         PIC S9(10)V99 COMP.
                   07  HTY-O2-PL          PIC S9(10)V99 COMP.
                   07  HTY-INT-PL         PIC S9(10)V99 COMP.

      *----------------------------------------------------------------------
      *
      * DLR ACTIVITY TBL:  1 - NET GAIN/LOSS [BYNET] | 2 - NEW VOLUME [BYVOL]
      *
      *----------------------------------------------------------------------
           03  HTY-DLR-ACTIVITY-TBL        OCCURS 2.
               05  HTY-DLRVPRIN   OCCURS 3
                                      PIC S9(10)V99 COMP.
               05  HTY-DLRVCHGS   OCCURS 3
                                      PIC S9(10)V99 COMP.
               05  HTY-DLPARVHELD      PIC S9(10)V99 COMP.

      *      -----------------------------------------------
      *                HTY-PL-RECOVERED    1 - PL RECOVERIES
      *                                   2 - P2 RECOVERIES
      *                                   3 - P3 RECOVERIES
      *                                   4 - P4 RECOVERIES
      *                                   5 - P5 RECOVERIES
      *      -----------------------------------------------
           03  HTY-PL-RECOVERED OCCURS 5.
               05  HTY-PL-PRINCOL      PIC S9(10)V99 COMP.
               05  HTY-PL-INTCOL       PIC S9(10)V99 COMP.
               05  HTY-PL-LATECOL      PIC S9(10)V99 COMP.
               05  HTY-PL-O2COL        PIC S9(10)V99 COMP.

      *----------------------------------------------------------------
      * SOLD  : LOANS SOLD/TRANSFERRED VIA BULKTR.C
      * TRANS : TRANSFERRED TO A GIVEN BRANCH VIA BULKTR.C
      * PURCH : MIDSTREAM PURCHASE FROM OUTSIDE SOURCE.
      *----------------------------------------------------------------
           03  HTY-AMT-SOLD            PIC S9(10)V99 COMP.
           03  HTY-AMT-TRANS           PIC S9(10)V99 COMP.
           03  HTY-AMT-PURCH           PIC S9(10)V99 COMP.

           03  HTY-NO-SOLD             PIC S9(9)    COMP.
           03  HTY-NO-TRANS            PIC S9(9)    COMP.
           03  HTY-NO-PURCH            PIC S9(9)    COMP.

           03  HTY-OTHCOL              PIC S9(10)V99 COMP.
           03  HTY-OT2COL              PIC S9(10)V99 COMP.
           03  HTY-INTCOL              PIC S9(10)V99 COMP.
           03  HTY-LATECOL             PIC S9(10)V99 COMP.
           03  HTY-PRINCOL             PIC S9(10)V99 COMP.
           03  HTY-DEFCOL              PIC S9(10)V99 COMP.
           03  HTY-RNPRIN              PIC S9(10)V99 COMP.
           03  HTY-INTREB              PIC S9(10)V99 COMP.
           03  HTY-CLREB1              PIC S9(10)V99 COMP.
           03  HTY-CLREB2              PIC S9(10)V99 COMP.
           03  HTY-CLREB3              PIC S9(10)V99 COMP.
           03  HTY-CLREB4              PIC S9(10)V99 COMP.
           03  HTY-AHREB               PIC S9(10)V99 COMP.
           03  HTY-PPREB               PIC S9(10)V99 COMP.
           03  HTY-O1REB               PIC S9(10)V99 COMP.
           03  HTY-O2REB               PIC S9(10)V99 COMP.
           03  HTY-O3REB               PIC S9(10)V99 COMP.
           03  HTY-O4REB               PIC S9(10)V99 COMP.
           03  HTY-O5REB               PIC S9(10)V99 COMP.
           03  HTY-O6REB               PIC S9(10)V99 COMP.
           03  HTY-O7REB               PIC S9(10)V99 COMP.
           03  HTY-SCREB               PIC S9(10)V99 COMP.
           03  HTY-MFREB               PIC S9(10)V99 COMP.
           03  HTY-DFREB               PIC S9(10)V99 COMP.
           03  HTY-PART-CHGOFF               PIC S9(10)V99 COMP.
      *CS  03  HTY-INS-TYPE            OCCURS 10.
           03  HTY-INS-TYPE            OCCURS 11.
               05  HTY-INS-NO-ELIG     PIC S9(9)    COMP.
               05  HTY-INS-NO-WROTE    PIC S9(9)    COMP.
               05  HTY-INS-NO-NOTWROTE PIC S9(9)    COMP.
               05  HTY-INS-AMT-WROTE   PIC S9(10)V99 COMP.

      *-------------------------------------------------------
      *    *NO-COL FIELDS REFLECT THE TOTAL NUMBER OF TIME
      *                   THEY WERE COLLECTED, EVEN THOUGH
      *                   THEIR COUNTER FIELDS ARE BROKEN
      *                   UP FOR NON P&L AND P&L
      *-------------------------------------------------------
           03  HTY-OTH-NO-COL          PIC S9(9)    COMP.
           03  HTY-OT2-NO-COL          PIC S9(9)    COMP.
           03  HTY-INT-NO-COL          PIC S9(9)    COMP.
           03  HTY-LATE-NO-COL         PIC S9(9)    COMP.

           03  HTY-OTH-NO-WAIVED       PIC S9(9)    COMP.
           03  HTY-OT2-NO-WAIVED       PIC S9(9)    COMP.
           03  HTY-INT-NO-WAIVED       PIC S9(9)    COMP.
           03  HTY-LATE-NO-WAIVED      PIC S9(9)    COMP.

           03  HTY-OTH-AMT-WAIVED      PIC S9(10)V99 COMP.
           03  HTY-OT2-AMT-WAIVED      PIC S9(10)V99 COMP.
           03  HTY-INT-AMT-WAIVED      PIC S9(10)V99 COMP.
           03  HTY-LATE-AMT-WAIVED     PIC S9(10)V99 COMP.
      *---------------------------------------------------------------
      * (LOAN, DEALER, MISC, OVERPYMNT, NSF CKS, WITHDRLS, INT DEP, FUTURE)
           03  HTY-CKWRITTEN           OCCURS 8
                                      PIC S9(10)V99 COMP.
      *---------------------------------------------------------------
      *    NOTE: THESE FIELDS ARE NON P&L, NON REPO ACCOUNTS
      *    -------------------------------------------------
      *    HTY-REC-CON
      *        1- RECENCY
      *        2- CONTRACTUAL
      *
      *    HTY-DEL-BUCKET
      *        1- 30
      *        2- 60
      *        3- 90
      *        4- 120
      *        5- 150
      *        6- 180
      *        7- 210+
      *        8- <M>
      *        9- <P>
      *       10- CURRENT
      *
      *----------------------------------------------------------------
           03  HTY-REC-CON                 OCCURS 2.
               05  HTY-DEL-BUCKET          OCCURS 10.
      *        DELINQUENT UNITS:
                   07  HTY-UNITS           PIC S9(9)    COMP.
      *        OUTSTANDING BALANCE:
                   07  HTY-OUTBAL          PIC S9(11)V99 COMP.
      *---------------------------------------------------------------
      *    NOTE: THESE FIELDS ARE BANKRUPT, NON P&L, NON REPO ACCOUNTS
      *    -----------------------------------------------------------
      *    HTY-REC-CON-BNKRPT
      *        1- RECENCY
      *        2- CONTRACTUAL
      *
      *    HTY-DEL-BUCKET-BNKRPT
      *        1- 30
      *        2- 60
      *        3- 90
      *        4- 120
      *        5- 150
      *        6- 180
      *        7- 210+
      *        8- <M>
      *        9- <P>
      *       10- CURRENT
      *
      *----------------------------------------------------------------
           03  HTY-REC-CONBNKRPT              OCCURS 2.
               05  HTY-DEL-BUCKET-BNKRPT      OCCURS 10.
      *        DELINQUENT UNITS:
                   07  HTY-UNITS-BNKRPT       PIC S9(9)    COMP.
      *        OUTSTANDING BALANCE:
                   07  HTY-OUTBAL-BNKRPT      PIC S9(11)V99 COMP.
      *----------------------------------------------------------------
           03  HTY-SUMWGTTM                PIC S9(9)    COMP.
           03  HTY-SUMWGTAV                PIC S9(11)V99 COMP.
           03  HTY-SUMWGTTA                PIC S9(11)V99 COMP.
           03  HTY-ALLOTMENTS              PIC S9(9)V99 COMP.
           03  HTY-LOCKBOX                 PIC S9(10)V99 COMP.

           03 HTY-DLDISC                   PIC S9(10)V99 COMP.
      *----------------------------------------------------------------
      *    UNEARNED TABLE:
      *    UNEARNED ACT:  1 - NET GAIN/LOSS          |  2 - NEW VOLUME
      *                   ____________________________________________________
      *    UNEARNED TABLE: 1 - INTEREST
      *                    2 - ADDON
      *                    3 - SERVICE CHARGE
      *                    4 - MAINTENANCE FEES
      *                    5 - DEALER DISCOUNT
      *                    6 - OTHER
      *    ------------------------------------------------------
           03  HTY-UNEARNED-ACTIVITY          OCCURS 2.
               05  HTY-UNEARNED-TYPE          OCCURS 10.
                   07  HTY-UNEARNED-AMT       PIC S9(11)V99 COMP.

           03  HTY-PROCEEDS                   PIC S9(11)V99 COMP.

       COPY "LIBGB/GRWORKW.CPY".
       COPY "LIBLP/BYINQ_CMD.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBLP/LPSETTYW.CPY".
       COPY "LIBGB/SYSTEMW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/GPENVW.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBLP/BYINQ1_DEF.CPY".
       COPY "LIBLP/BYINQ2_DEF.CPY".
       COPY "LIBLP/BYINQ3_DEF.CPY".
       COPY "LIBLP/BYINQ4_DEF.CPY".
       COPY "LIBLP/BYINQ5_DEF.CPY".
       COPY "LIBLP/BYINQ6_DEF.CPY".
       COPY "LIBLP/BYINQ7_DEF.CPY".
       COPY "LIBLP/BYINQ8_DEF.CPY".
       COPY "LIBLP/BYINQ9_DEF.CPY".
       COPY "LIBLP/BYINQA_DEF.CPY".
       COPY "LIBLP/BYINQB_DEF.CPY".
       COPY "LIBLP/BYINQC_DEF.CPY".
       COPY "LIBLP/BYINQ1_WKS.CPY".
       COPY "LIBLP/BYINQ2_WKS.CPY".
       COPY "LIBLP/BYINQ3_WKS.CPY".
       COPY "LIBLP/BYINQ4_WKS.CPY".
       COPY "LIBLP/BYINQ5_WKS.CPY".
       COPY "LIBLP/BYINQ6_WKS.CPY".
       COPY "LIBLP/BYINQ7_WKS.CPY".
       COPY "LIBLP/BYINQ8_WKS.CPY".
       COPY "LIBLP/BYINQ9_WKS.CPY".
       COPY "LIBLP/BYINQA_WKS.CPY".
       COPY "LIBLP/BYINQB_WKS.CPY".
       COPY "LIBLP/BYINQC_WKS.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBWI/LCSCANW.CPY".
       COPY "LIBGB/PASSWDW.CPY".
       COPY "LIBWI/GRSCANW.CPY".

       COPY "LIBGB/GETFMW.CPY".
       01 FORM-LINK-AREA    PIC X(2000).
       SCREEN SECTION.
       COPY "LIBLP/BYINQ1_SCN.CPY".
       COPY "LIBLP/BYINQ2_SCN.CPY".
       COPY "LIBLP/BYINQ3_SCN.CPY".
       COPY "LIBLP/BYINQ4_SCN.CPY".
       COPY "LIBLP/BYINQ5_SCN.CPY".
       COPY "LIBLP/BYINQ6_SCN.CPY".
       COPY "LIBLP/BYINQ7_SCN.CPY".
       COPY "LIBLP/BYINQ8_SCN.CPY".
       COPY "LIBLP/BYINQ9_SCN.CPY".
       COPY "LIBLP/BYINQA_SCN.CPY".
       COPY "LIBLP/BYINQB_SCN.CPY".
       COPY "LIBLP/BYINQC_SCN.CPY".
       PROCEDURE DIVISION
             USING FORM-PATHNAME EXIT-PATHNAME.

      *****************************************************************
      *
      *           MAIN PROGRAM MODULE
      *
      *****************************************************************
       MAIN-PROGRAM SECTION.

           PERFORM SQL-CONNECT.

      D    STOP MESS.

       COPY "LIBGB/CHKSEC.CPY".

           PERFORM GET-GPENV.

      * REMOVING OTHER-MACHINE-FG LOGIC. XTYFILE IN /USR/PFSI CAN NOT
      * BE USED IN TY-PATH SUCCESSFULLY IN A32.

           MOVE DAILY-PROMPTS-I-AM-WORLD TO DAILY-PROMPTS-I-AM.

           MOVE EXT-CONAME-USER-LOC TO BR-NO.
           PERFORM OPEN-BR-FILE.
           PERFORM READ-BR-FILE.
           PERFORM CLOSE-BR-FILE.
           IF NOT IO-GOOD
              MOVE 3 TO BR-BYINQ-START-PAGE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE EXT-FILPATH-BASE           TO FILPATH-BASE.
           MOVE BR-MACHINE                 TO FILPATH-MACHINE.
           MOVE BR-DATA-PATH               TO FILPATH-DATA.

           MOVE "B"                        TO BY-PATH-OVERRIDE
                                              RC-PATH-OVERRIDE
                                              TY-PATH-OVERRIDE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE BR-BYINQ-START-PAGE TO PAGE-FLAG.
           MOVE FORM-BUFFER(PAGE-FLAG) TO VDU-FORM-NAME.

           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE HOLD-FORM-NAM TO FORM-NAM.
           MOVE FORM-PATH TO FORM-PATHNAME.

      *----------------------------------------------------------------
      * COUNT THE NUMBER OF ROWS IN RCFILE THAT HAVE AN RC_STATUS =     
      * 'A1', CURRENTLY THERE ARE NONE. IF THE COUNT IS 0, DO NOT    
      * PERFORM ANY FETCHING OF ROWS IN LATER PROCESSING. 
      *----------------------------------------------------------------

           PERFORM OPEN-RC2-FILE.
           PERFORM READ-RC2-FILE-FIRST-CALL.
           PERFORM CLOSE-RC2-FILE.

      *----------------------------------------------------------------
      * GET TYPE OF RUN: 1 -  SNAPSHOT,  2 - REALTIME.
      *                  IF VALUE UNAVAILABLE DEFAULT TO SNAPSHOT.
      *----------------------------------------------------------------
           IF EXT-MENU-OPTION-FG = "2"
              MOVE 2 TO READ-TYPE
           ELSE
              MOVE 1 TO READ-TYPE.

           PERFORM FORM-RESET.

       MAIN-MODULE.

      * DISPLAY TYPE OF RUN: SNAPSHOT OR REALTIME.
           MOVE INQUIRY-TYPE(READ-TYPE) TO WR-BUF.
           MOVE INQUIRY-LIST(READ-TYPE) TO WR-LIST.
           PERFORM CALL-WRFORM.

           IF FIRST-TIME = 0
              MOVE 1 TO FIRST-TIME
              MOVE EXT-CONAME-SYSDATE TO TODAYS-DATE
           ELSE
              PERFORM FORM-RESET
              IF ( PAGE2 )
                 MOVE "S  A020SRCD:S." TO VDU
                 IF GP-THE-ONLY-SF-SRCD NOT = " "
                    MOVE GP-THE-ONLY-SF-SRCD TO VDUBUF
                 ELSE
                    MOVE "RF" TO VDUBUF
                 END-IF
                 PERFORM SCREENIO

                 MOVE "S  A020SCDM:S." TO VDU
                 IF GP-NO-SC-USE-DM = "Y"
                    MOVE "DM" TO VDUBUF
                 ELSE
                    MOVE "SC" TO VDUBUF
                 END-IF
                 PERFORM SCREENIO.

           PERFORM REC-BY-MODULE.
           IF IO-TYPE = "END"
              GO TO END-ROUTINE.

           PERFORM ENTRY-MODULE.

           GO TO MAIN-MODULE.

      ******************************************************************
      *
      *    R E C - B Y - M O D U L E
      *
      *-----------------------------------------------------------------
      *    RECEIVE BYFILE MODULE
      ******************************************************************
       REC-BY-MODULE SECTION.
      *-----------------------------------------------------------------

       REC-BY-MODULE-NO.
           MOVE NNO      TO ALL-BRANCHES-FLAG.
           MOVE "1"      TO ALL-CLASSES-FLAG.
           MOVE ACTIVITY TO SUB2.
           MOVE 0        TO SUMMARY-TYPE.
           MOVE " "      TO IO-TYPE.

           PERFORM GET-TODAYS-DATE.

           GO TO SETUP-BY-DATE.
       ENTER-DATE.
           MOVE "EN N040MMYY." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>"
              MOVE "END" TO IO-TYPE
              GO TO END-REC-BY-MODULE.
           IF VDUSTATUS NOT = "00"
              GO TO ENTER-DATE.
           MOVE VDUNUM0 TO DISPLAY-DATE.
           IF DISPLAY-MO < 1 OR DISPLAY-MO > 12
              GO TO ENTER-DATE.

       SETUP-BY-DATE.
           MOVE DISPLAY-MO TO DATE-MMDDYY-MM.
           MOVE 1          TO DATE-MMDDYY-DD.
           MOVE DISPLAY-YR TO DATE-MMDDYY-YY.
           PERFORM CONVERT-MMDDYY-TO-YYYYMM.
           MOVE DATE-YYYYMM TO BY-DATE-CCYYMM STORE-DATE-CCYYMM.

           PERFORM SET-WRFILE-FLAG.

       ENTER-BRANCH.
           MOVE "E  A040BRANCH." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>"
              MOVE "END" TO IO-TYPE
              GO TO END-REC-BY-MODULE.
      *    IF VDUSTATUS = "16"
      *       PERFORM VERIFY-PASSWORD
      *       IF PASSWDW-STAT NOT = "OK"
      *          MOVE "ALL BRANCHES NOT ALLOWED WITHOUT PASSWORD"
      *                                            TO MESS
      *          PERFORM SEND-MESS
      *          GO TO REC-BY-MODULE-NO
      *       ELSE
      *          MOVE YYES TO ALL-BRANCHES-FLAG
      *          MOVE 01 TO BR-NO
      *                     BY-BRNO
      *          MOVE "ALL BRANCHES  " TO BR-NAME
      *          PERFORM SEND-BR-NAME
      *          GO TO GET-CLASS.
           IF VDUSTATUS = "1U"
              GO TO ENTER-DATE.
           IF VDUSTATUS = "1<"
              MOVE "WI/GRSCAN" TO FORM-NAM
              CALL FORM-PROGX USING FORM-PATH GRSCAN-BUF EXIT-PATHNAME
              CANCEL FORM-PROGX
              IF GRSCAN-STATUS NOT = "00"
                 GO TO ENTER-BRANCH
              ELSE
                 MOVE "S  A000BRANCH." TO VDU
                 MOVE GRSCAN-KEY TO VDUBUF
                 PERFORM SCREENIO
                 GO TO READ-GROUP-FILE.
           IF VDUSTATUS NOT = "00"
              GO TO ENTER-BRANCH.

       READ-GROUP-FILE.
           MOVE VDUBUF TO WGR-WORD
                          ENT-GROUP.
           IF WGR-WORD = SPACES
              GO TO ENTER-BRANCH.

           MOVE 0 TO WGR-MIN-LEVEL.
           PERFORM CHECK-MEMBER.
           IF WGR-ERR > 0
                   AND WGR-ERR NOT = 1
              MOVE WGR-MSG(WGR-ERR) TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-BRANCH.
           MOVE WGR-LEVEL TO SAVE-LEVEL.

           PERFORM OPEN-BR-FILE.

           IF WGR-TYPE NOT = "A"
              GO TO BRANCH-NUMBER-ENTERED.

           MOVE WGR-WORD TO ENT-GROUP.
           MOVE GR-NAME TO BR-NAME HOLD-GR-NAME.
           MOVE EXT-CONAME-USER-GROUP TO CASE-BUF.
           CALL "C$TOUPPER" USING CASE-BUF, VALUE 4.
           MOVE CASE-BUF TO WGR-WORD.
           IF WGR-WORD = ENT-GROUP-1ST-3
              GO TO GROUP-OKAY.
           PERFORM CHECK-MEMBER.
           IF WGR-ERR > 0
              GO TO PASSWORD-REQUIRED.

           PERFORM OPEN-GR1-FILE.
           MOVE WGR-WORD TO GR-ID.
           PERFORM SEARCH-GR-INIT.
           MOVE ENT-GROUP TO WGR-TEST-GROUP.
           PERFORM VERIFY-GROUP-MEMBER.
           IF WGR-ERR > 0
              GO TO PASSWORD-REQUIRED.
           GO TO GROUP-OKAY.

       PASSWORD-REQUIRED.
           PERFORM VERIFY-PASSWORD.
           IF PASSWDW-STAT NOT = "OK"
              MOVE
           "VIEWING THIS GROUP IS NOT ALLOWED WITHOUT PASSWORD!!!"
                                                TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-BRANCH.

       GROUP-OKAY.
           MOVE "Y" TO GROUP-FLAG.
           MOVE 0   TO STORE-BRNO.
           PERFORM SEND-BR-NAME.

           GO TO GET-CLASS.

       BRANCH-NUMBER-ENTERED.
           MOVE "N" TO GROUP-FLAG.
           MOVE WGR-SRESULT TO STORE-BRNO
                               BR-NO
                               BY-BRNO.
           MOVE "SNNN040BRANCH." TO VDU.
           MOVE STORE-BRNO TO VDUNUM0.
           PERFORM SCREENIO.
           IF STORE-BRNO = 0
              GO TO ENTER-BRANCH.

           PERFORM READ-BR-FILE.
           IF IO-BAD
              MOVE "INVALID BRANCH" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-BRANCH.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE EXT-FILPATH-BASE           TO FILPATH-BASE.
           MOVE BR-MACHINE                 TO FILPATH-MACHINE.
           MOVE BR-DATA-PATH               TO FILPATH-DATA.

           MOVE "B"                        TO BY-PATH-OVERRIDE
                                              RC-PATH-OVERRIDE
                                              TY-PATH-OVERRIDE.

           IF WS-RC2-ROW-COUNT > 0
              PERFORM GET-FIRST-OPEN-DATE
           ELSE
              MOVE 0 TO FIRST-OPEN-DATE.

           PERFORM SEND-BR-NAME.
           IF STORE-BRNO = EXT-CONAME-USER-LOC
              GO TO BRANCH-OKAY.

           MOVE EXT-CONAME-USER-GROUP TO CASE-BUF.
           CALL "C$TOUPPER" USING CASE-BUF, VALUE 4.
           MOVE CASE-BUF TO WGR-WORD.
           PERFORM CHECK-MEMBER.
           IF WGR-ERR > 0
              GO TO PASSWORD-REQUIRED-BR.

           PERFORM OPEN-GR1-FILE.
           MOVE WGR-WORD TO GR-ID.
           PERFORM SEARCH-GR-INIT.
           MOVE STORE-BRNO TO WGR-SRESULT.
           PERFORM SEARCH-GR-VERIFY.
           IF WGR-ERR > 0
              GO TO PASSWORD-REQUIRED-BR.

           GO TO BRANCH-OKAY.

       PASSWORD-REQUIRED-BR.
           PERFORM VERIFY-PASSWORD.
           IF PASSWDW-STAT NOT = "OK"
              MOVE
           "VIEWING THIS BRANCH IS NOT ALLOWED WITHOUT PASSWORD!!!"
                                                TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-BRANCH.

       BRANCH-OKAY.
           PERFORM OPEN-BY1-FILE.

       GET-CLASS.
           MOVE "S  A020LCX." TO VDU.
           PERFORM SCREENIO.

      * TEMPORARY NO INPUT OF CLASS OPTION:
      *       MOVE YYES TO ALL-CLASSES-FLAG.
      *       MOVE 01 TO BY-CLASS.
      *       MOVE "S  A020LCX." TO VDU.
      *       MOVE "ALL CLASSES" TO VDUBUF.
      *       PERFORM SCREENIO.
      *       GO TO TEST-ALL.

           MOVE "E  A020LC." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U" OR "10"
              GO TO ENTER-BRANCH.
           IF VDUSTATUS = "1>"
              MOVE "END" TO IO-TYPE
              GO TO END-REC-BY-MODULE.
           IF VDUSTATUS = "16"
              MOVE YYES          TO ALL-CLASSES-FLAG
              MOVE 01            TO BY-CLASS
              MOVE "S  A020LCX." TO VDU
              MOVE "ALL CLASSES" TO VDUBUF
              PERFORM SCREENIO
              GO TO TEST-ALL.
           IF VDUSTATUS = "1<"
              PERFORM CLASS-SCAN
              IF NOT IO-GOOD
                 GO TO GET-CLASS.
           IF VDUSTATUS NOT = "00"
              GO TO GET-CLASS.
           MOVE VDUBUF TO ENTER-CLASS-ALPHA.
           IF ENTER-CLASS-ALPHA NUMERIC
              GO TO ONE-CLASS-ENTERED.
           IF CLASS-WK1 = "S"
              MOVE "S" TO ALL-CLASSES-FLAG
              MOVE "S  A020LCX." TO VDU
              MOVE "SALES FINANCE" TO VDUBUF
              PERFORM SCREENIO
              GO TO TEST-ALL.
           IF CLASS-WK1 = "L"
              MOVE "L" TO ALL-CLASSES-FLAG
              MOVE "S  A020LCX." TO VDU
              MOVE "DIRECT LOANS" TO VDUBUF
              PERFORM SCREENIO
              GO TO TEST-ALL.
           IF CLASS-WK1 = "A"
              MOVE "A" TO ALL-CLASSES-FLAG
              MOVE "S  A020LCX." TO VDU
              MOVE "CLASS 1-4 & 22 LOANS" TO VDUBUF
              PERFORM SCREENIO
              GO TO TEST-ALL.
           IF CLASS-WK1 = "W"
              MOVE "W" TO ALL-CLASSES-FLAG
              MOVE "S  A020LCX." TO VDU
              MOVE "WCBC LOANS" TO VDUBUF
              PERFORM SCREENIO
              GO TO TEST-ALL.
           IF CLASS-WK1 = "M"
              MOVE "M" TO ALL-CLASSES-FLAG
              MOVE "S  A020LCX." TO VDU
              MOVE "SMALL LOANS" TO VDUBUF
              PERFORM SCREENIO
              GO TO TEST-ALL.
           IF CLASS-WK1 = "R"
              MOVE "R" TO ALL-CLASSES-FLAG
              MOVE "S  A020LCX." TO VDU
              MOVE "REAL ESTATE LOANS" TO VDUBUF
              PERFORM SCREENIO
              GO TO TEST-ALL.
           IF CLASS-WK1 = "I"
              MOVE "I" TO ALL-CLASSES-FLAG
              MOVE "S  A020LCX." TO VDU
              MOVE "I.B. LOANS       " TO VDUBUF
              PERFORM SCREENIO
              GO TO TEST-ALL.             

           IF CLASS-WK1 = "O"
              MOVE "O" TO ALL-CLASSES-FLAG
              MOVE "S  A020LCX." TO VDU
              MOVE "CLASS 5 - 21 LOANS" TO VDUBUF
              PERFORM SCREENIO
              GO TO TEST-ALL.

           IF CLASS-WK1 = "F"
              MOVE "F" TO ALL-CLASSES-FLAG
              MOVE "S  A020LCX." TO VDU
              MOVE "WCBC & CLASS 30" TO VDUBUF
              PERFORM SCREENIO
              GO TO TEST-ALL.

           MOVE CLASS-WK1 TO CLASS-WK2.
           MOVE 0 TO CLASS-WK1.
           IF ENTER-CLASS-ALPHA NUMERIC
              MOVE "SN N020LC." TO VDU
              MOVE ENTER-CLASS-WK TO VDUNUM0
              PERFORM SCREENIO
              GO TO ONE-CLASS-ENTERED.
           GO TO GET-CLASS.

       ONE-CLASS-ENTERED.
           MOVE ENTER-CLASS-WK TO BY-CLASS
                                  STORE-CLASS
                                  LC-CODE.
           PERFORM OPEN-LC1-FILE.
           PERFORM READ-LC1-FILE.
           PERFORM CLOSE-LC1-FILE.
           IF IO-BAD
              MOVE "INVALID CLASS" TO MESS
              PERFORM SEND-MESS
              GO TO GET-CLASS.
           MOVE "S  A020LCX." TO VDU.
           MOVE LC-DESC TO VDUBUF.
           PERFORM SCREENIO.
           MOVE "1" TO ALL-CLASSES-FLAG.

       TEST-ALL.
           IF GROUP-FLAG = "Y"
              PERFORM OPEN-GR1-FILE
              MOVE ENT-GROUP TO GR-ID
              PERFORM SEARCH-GR-INIT.

           PERFORM SUMMARY-MODULE.

           IF PAGE-FLAG = 12   
              IF READ-WRFILE-FG NOT = "Y"
                 MOVE
                 "RATING SHEET UPDATE NOT COMPLETED FOR THIS MONTH"
                     TO MESS
                 PERFORM SEND-MESS
                 GO TO ENTER-DATE
              ELSE
                 PERFORM PROCESS-WRFILE.

           PERFORM DISPLAY-PAGE.
       END-REC-BY-MODULE.
           EXIT.

      **************************************************
      *    SUMMARY-TYPES:
      *
      *    BR-CL        0 = ONE BRANCH   / ONE CLASS
      *    BR-ALLCL     1 = ONE BRANCH   / ALL CLASSES
      *    ALLBR-CL     2 = ALL BRANCHES / ONE CLASS
      *    ALLBR-ALLCL  3 = ALL BRANCHES / ALL CLASSES
      *    RESOLVED     1 OR 2 OR 3
      *
      *  READ TYPES:
      *
      *    READ-ONCE    1 - SNAPSHOT MODE
      *    READ-EVERY   2 - REALTIME MODE
      *
      *    READ-ONCE-FG "Y/N" SET AT END OF READ SO NO ADDITIONAL READ ARE
      *                  MADE (SNAPSHOT MODE).
      **************************************************
       SUMMARY-MODULE SECTION.
           IF READ-ONCE
              IF READ-ONCE-FG = "Y"
                 IF GROUP-FLAG = "Y"
                    MOVE HOLD-GR-NAME TO BR-NAME
                 END-IF
                 GO TO EXIT-SUMMARY-MODULE.
           IF RESOLVED
              GO TO INC-NEXT-RECORD-KEY.

           MOVE "CALCULATING SUMMARY INFO - STANDBY" TO MESS.
           PERFORM CALL-TMMSG.

      *----------------------------------------------------------------
      * CLEAR HOLD BY/TY FILES.
      *----------------------------------------------------------------
           PERFORM CLEAR-HBY-FILE.
           PERFORM CLEAR-HTY-FILE.

           IF GROUP-FLAG = "Y"
              GO TO INC-NEXT-RECORD-KEY.

           IF NOT-ALL-BRANCHES
                   AND NOT ONE-CLASS
              MOVE 1 TO SUMMARY-TYPE
           ELSE
           IF ALL-BRANCHES
                   AND ONE-CLASS
              MOVE 2 TO SUMMARY-TYPE
           ELSE
           IF ALL-BRANCHES
                   AND NOT ONE-CLASS
              MOVE 3 TO SUMMARY-TYPE
              GO TO READ-NEXT-REC.

           IF ALL-BRANCHES
              MOVE 1 TO BY-BRNO
           ELSE
              MOVE STORE-BRNO TO BY-BRNO.
           IF ONE-CLASS
              MOVE STORE-CLASS TO BY-CLASS
           ELSE
              MOVE 1 TO BY-CLASS.

           IF NOT ALL-BRANCHES
                   AND ONE-CLASS
              PERFORM READ-BY1-FILE
              GO TO TEST-TYPE.

           GO TO START-FILE.

       INC-NEXT-RECORD-KEY.
           IF GROUP-FLAG NOT = "Y"
              GO TO SKIP-GROUP-LOGIC.
           IF FIRST-READ = "N"
              GO TO NEXT-GROUP-BY.
           MOVE "N" TO FIRST-READ.
       NEXT-GROUP-LOC.
           PERFORM SEARCH-GR-NEXT.
           IF WGR-SRESULT = 0
              GO TO EXIT-SUMMARY-MODULE.

           MOVE STORE-DATE-CCYYMM TO BY-DATE-CCYYMM.
           MOVE WGR-SRESULT       TO BY-BRNO BR-NO.
           PERFORM READ-BR-FILE.
           IF IO-BAD
              GO TO NEXT-GROUP-LOC.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE EXT-FILPATH-BASE           TO FILPATH-BASE.
           MOVE BR-MACHINE                 TO FILPATH-MACHINE.
           MOVE BR-DATA-PATH               TO FILPATH-DATA.

           MOVE "B"                        TO BY-PATH-OVERRIDE
                                              RC-PATH-OVERRIDE
                                              TY-PATH-OVERRIDE.

           IF WS-RC2-ROW-COUNT > 0
              PERFORM GET-FIRST-OPEN-DATE
           ELSE
              MOVE 0 TO FIRST-OPEN-DATE.

           PERFORM OPEN-BY1-FILE.
           IF NOT ONE-CLASS
              MOVE 1           TO BY-CLASS
           ELSE
              MOVE STORE-CLASS TO BY-CLASS.

           MOVE BY-DATE-CCYYMM TO QBY1-WBEG-DATE-CCYYMM
                                  QBY1-WEND-DATE-CCYYMM.
           MOVE BY-BRNO        TO QBY1-WBEG-BRNO
                                  QBY1-WEND-BRNO.
           MOVE BY-CLASS       TO QBY1-WBEG-CLASS.
           MOVE ALL "9"        TO QBY1-WEND-CLASS.
           
           PERFORM START-BY1-FILE. *> SSP - BYAFILE  
       NEXT-GROUP-BY.
           PERFORM READ-BY1-FILE-NEXT. *> EMBEDDED - BYAFILE, BYBFILE
           IF (IO-BAD) OR (BY-BRNO NOT = WGR-SRESULT)
              PERFORM CLOSE-BY1-FILE
              GO TO NEXT-GROUP-LOC.

           IF BY-DATE-CCYYMM NOT = STORE-DATE-CCYYMM
              PERFORM CLOSE-BY1-FILE
              GO TO NEXT-GROUP-LOC.

           IF ONE-CLASS
              IF BY-CLASS NOT = STORE-CLASS
                 PERFORM CLOSE-BY1-FILE
                 GO TO NEXT-GROUP-LOC
              ELSE
                 GO TO ACCUM-BY-TOTALS.

           PERFORM TEST-CLASS.
           IF CORRECT-CLASS = "N"
              GO TO NEXT-GROUP-BY.

           GO TO ACCUM-BY-TOTALS.

       SKIP-GROUP-LOGIC.
           MOVE STORE-DATE-CCYYMM TO BY-DATE-CCYYMM.
           IF BR-ALLCL
              GO TO READ-NEXT-REC.
           IF ALLBR-CL
              ADD 1 TO BY-BRNO.
       START-FILE.
           MOVE STORE-DATE-CCYYMM TO BY-DATE-CCYYMM
                                     QBY1-WBEG-DATE-CCYYMM
                                     QBY1-WEND-DATE-CCYYMM.
           MOVE BY-BRNO        TO QBY1-WBEG-BRNO.
           MOVE ALL "9"        TO QBY1-WEND-BRNO.
           MOVE BY-CLASS       TO QBY1-WBEG-CLASS.
           MOVE ALL "9"        TO QBY1-WEND-CLASS.

           PERFORM START-BY1-FILE. *> SSP - BYAFILE
       READ-NEXT-REC.
           PERFORM READ-BY1-FILE-NEXT. *> EMBEDDED - BYAFILE, BYBFILE
       TEST-TYPE.
           IF IO-BAD
              IF E-CODE NOT = "00"
                 GO TO EXIT-SUMMARY-MODULE.

           IF BY-DATE-CCYYMM NOT = STORE-DATE-CCYYMM
              GO TO EXIT-SUMMARY-MODULE.

           IF BR-ALLCL
              IF BY-BRNO > STORE-BRNO
                 GO TO EXIT-SUMMARY-MODULE.

           IF ONE-CLASS
              IF BY-CLASS NOT = STORE-CLASS
                 GO TO READ-NEXT-REC
              ELSE
                 GO TO ACCUM-BY-TOTALS.

           PERFORM TEST-CLASS.
           IF CORRECT-CLASS = "N"
              GO TO READ-NEXT-REC.

       ACCUM-BY-TOTALS.
      *----------------------------------------------------------------
      *  DON'T GET TODAY'S TOTALS WHEN WHEN NOT INQUIRING
      *  ON THE CURRENT SYSTEM MONTH:
      *----------------------------------------------------------------
           IF (TODAY-MM = STORE-DATE-CCYYMM-MM
                    AND TODAY-YY = STORE-DATE-CCYYMM-YY)
                  OR TRAP-A-DATE-TY-FG = "Y"
              PERFORM GET-TODAYS-TOTALS
           ELSE
              PERFORM CLEAR-TY-FILE.

      *----------------------------------------------------------------
      * ACCUMULATE BY/TY HOLD FILES.
      *----------------------------------------------------------------
           IF TRAP-A-DATE-TY-FG NOT = "Y"
              PERFORM ADD-BY-TO-HBY.
           PERFORM ADD-TY-TO-HTY.

           IF GROUP-FLAG = "Y"
              GO TO NEXT-GROUP-BY.
           IF BR-ALLCL OR ALLBR-CL
              GO TO INC-NEXT-RECORD-KEY.
           IF ALLBR-ALLCL
              GO TO READ-NEXT-REC.

       EXIT-SUMMARY-MODULE.
           MOVE ZEROS TO PAGE-1-TOTALS
                         PAGE-2-TOTALS
                         PAGE-3-TOTALS
                         PAGE-3-REL-TOTALS
                         PAGE-3-DAILY-TOTALS
                         PAGE-3-REL-DAILY-TOTALS
                         PAGE-4-TOTALS
                         PAGE-5-TOTALS
                         PAGE-6-TOTALS
                         PAGE-7-TOTALS
                         PAGE-8-TOTALS
                         PAGE-9-TOTALS
                         PAGE-10-TOTALS
                         PAGE-11-TOTALS
                         PAGE-12-TOTALS.   
           IF PAGE1
              IF PAGE1-TOTAL
                 PERFORM SUM-FIELDS-1-BY
                        VARYING MTDYTD FROM 1 BY 1
                                   UNTIL MTDYTD > 2
                 PERFORM SUM-FIELDS-1-TY
                     VARYING MTDYTD FROM 1 BY 1
                                   UNTIL MTDYTD > 2
                 GO TO SUMMARY-MODULE-TEST-ALL
              ELSE
                 PERFORM SUM-FIELDS-1-TY
                     VARYING MTDYTD FROM 1 BY 1
                                   UNTIL MTDYTD > 1
                 GO TO SUMMARY-MODULE-TEST-ALL.

           IF PAGE2
              MOVE ACTIVITY TO ACTSUB
              IF PAGE2-TOTAL
                 PERFORM SUM-FIELDS-2-BY
                     VARYING PERIOD FROM 1 BY 1 UNTIL PERIOD > 3
                         AFTER TYPSUB FROM 1 BY 1 UNTIL TYPSUB > 11
                 ADD HBY-SUMWGTTM TO SUM-WGTTM
                 ADD HBY-SUMWGTAV TO SUM-WGTAV
                 ADD HBY-SUMWGTTA TO SUM-WGTTA
                 PERFORM SUM-FIELDS-2-TY
                     VARYING TYPSUB FROM 1 BY 1 UNTIL TYPSUB > 11
                 ADD HTY-SUMWGTTM TO SUM-WGTTM
                 ADD HTY-SUMWGTAV TO SUM-WGTAV
                 ADD HTY-SUMWGTTA TO SUM-WGTTA
                 GO TO SUMMARY-MODULE-TEST-ALL
              ELSE
                 PERFORM SUM-FIELDS-2-TY
                     VARYING TYPSUB FROM 1 BY 1 UNTIL TYPSUB > 11
                 ADD HTY-SUMWGTTM TO SUM-WGTTM
                 ADD HTY-SUMWGTAV TO SUM-WGTAV
                 ADD HTY-SUMWGTTA TO SUM-WGTTA
                 GO TO SUMMARY-MODULE-TEST-ALL.

           IF PAGE3
              PERFORM SETUP-PAGE3-SUMMARY-TOT
              GO TO SUMMARY-MODULE-TEST-ALL.

           IF PAGE4
              MOVE ACTIVITY TO ACTSUB
              IF PAGE4-TOTAL
                 PERFORM SUM-FIELDS-4-BY
                     VARYING PERIOD FROM 1 BY 1 UNTIL PERIOD > 3
                         AFTER REPOSUB FROM 1 BY 1 UNTIL REPOSUB > 5
                 PERFORM SUM-FIELDS-4-TY
                     VARYING REPOSUB FROM 1 BY 1 UNTIL REPOSUB > 5
                 GO TO SUMMARY-MODULE-TEST-ALL
              ELSE
                 PERFORM SUM-FIELDS-4-TY
                     VARYING REPOSUB FROM 1 BY 1 UNTIL REPOSUB > 5
                 GO TO SUMMARY-MODULE-TEST-ALL.

           IF PAGE5
              IF PAGE5-TOTAL
                 PERFORM SUM-FIELDS-5-BY
                        VARYING MTDYTD FROM 1 BY 1
                                   UNTIL MTDYTD > 2
                 PERFORM SUM-FIELDS-5-TY
                     VARYING MTDYTD FROM 1 BY 1
                                   UNTIL MTDYTD > 2
                 GO TO SUMMARY-MODULE-TEST-ALL
              ELSE
                 PERFORM SUM-FIELDS-5-TY
                     VARYING MTDYTD FROM 1 BY 1
                                   UNTIL MTDYTD > 1
                 GO TO SUMMARY-MODULE-TEST-ALL.

           IF PAGE6
              IF PAGE6-TOTAL
                 PERFORM SUM-FIELDS-6-BY
                        VARYING MTDYTD FROM 1 BY 1
                                   UNTIL MTDYTD > 2
                 PERFORM SUM-FIELDS-6-TY
                     VARYING MTDYTD FROM 1 BY 1
                                   UNTIL MTDYTD > 2
                 GO TO SUMMARY-MODULE-TEST-ALL
              ELSE
                 PERFORM SUM-FIELDS-6-TY
                     VARYING MTDYTD FROM 1 BY 1
                                   UNTIL MTDYTD > 1
                 GO TO SUMMARY-MODULE-TEST-ALL.

           IF PAGE7
              MOVE P7-ACTIVITY TO ACTSUB
              IF PAGE7-TOTAL
                 PERFORM SUM-FIELDS-7-BY
                     VARYING PERIOD FROM 1 BY 1 UNTIL PERIOD > 3
                         AFTER PLSUB FROM 1 BY 1 UNTIL PLSUB > 10
                 PERFORM SUM-FIELDS-7-TY
                     VARYING PLSUB FROM 1 BY 1 UNTIL PLSUB > 10
                 GO TO SUMMARY-MODULE-TEST-ALL
              ELSE
                 PERFORM SUM-FIELDS-7-TY
                     VARYING PLSUB FROM 1 BY 1 UNTIL PLSUB > 10
                 GO TO SUMMARY-MODULE-TEST-ALL.

           IF PAGE8
              IF PAGE8-MTD OR PAGE8-YTD
                 MOVE PAGE8-TYPE TO MTDYTD
                 PERFORM SUM-FIELDS-8-BY
                        VARYING PLSUB FROM 1 BY 1 UNTIL PLSUB > 5
                 PERFORM SUM-FIELDS-8-TY
                        VARYING PLSUB FROM 1 BY 1 UNTIL PLSUB > 5
                 GO TO SUMMARY-MODULE-TEST-ALL
              ELSE
                 PERFORM SUM-FIELDS-8-TY
                     VARYING PLSUB FROM 1 BY 1 UNTIL PLSUB > 5
                 GO TO SUMMARY-MODULE-TEST-ALL.

           IF PAGE9
              MOVE ACTIVITY TO ACTSUB
              IF PAGE9-TOTAL
                 PERFORM SUM-FIELDS-9-BY
                     VARYING PERIOD FROM 1 BY 1 UNTIL PERIOD > 3
                       AFTER DLRV FROM 1 BY 1 UNTIL DLRV > 3
                 PERFORM SUM-FIELDS-9-TY
                     VARYING DLRV FROM 1 BY 1 UNTIL DLRV > 3
                 GO TO SUMMARY-MODULE-TEST-ALL
              ELSE
                 PERFORM SUM-FIELDS-9-TY
                     VARYING DLRV FROM 1 BY 1 UNTIL DLRV > 3
                 GO TO SUMMARY-MODULE-TEST-ALL.


           IF PAGE10   
              MOVE ACTIVITY TO ACTSUB
              IF PAGE10-TOTAL
                 PERFORM SUM-FIELDS-10-BY
                     VARYING TYPSUB FROM 1 BY 1 UNTIL TYPSUB > 13

                 PERFORM SUM-FIELDS-10-TY
                     VARYING TYPSUB FROM 1 BY 1 UNTIL TYPSUB > 10
                 GO TO SUMMARY-MODULE-TEST-ALL
              ELSE
                 PERFORM SUM-FIELDS-10-TY
                     VARYING TYPSUB FROM 1 BY 1 UNTIL TYPSUB > 10
                 GO TO SUMMARY-MODULE-TEST-ALL.

           IF PAGE11
              IF PAGE11-TOTAL
                 PERFORM SUM-FIELDS-11-BY
                 PERFORM SUM-FIELDS-11-TY
                 GO TO SUMMARY-MODULE-TEST-ALL
              ELSE
                 PERFORM SUM-FIELDS-11-TY
                 GO TO SUMMARY-MODULE-TEST-ALL.

       SUMMARY-MODULE-TEST-ALL.
           MOVE 0 TO SUMMARY-TYPE.
           MOVE "Y" TO FIRST-READ.
           MOVE "Y" TO READ-ONCE-FG.
           MOVE 1 TO WGR-SDEPTH
                     WGR-SCURR(1).

      *******************************************
      *    SETUP PAGE 3 SUMMARY TOTALS
      *******************************************
       SETUP-PAGE3-SUMMARY-TOT SECTION.
           IF DELINQ = 1 OR 2 OR 3
              MOVE DELINQ TO PERIOD
              PERFORM SUM-FIELDS-3-BY
                  VARYING AGESUB FROM 1 BY 1
                                       UNTIL AGESUB > 10
                      AFTER RECCON FROM 1 BY 1
                                       UNTIL RECCON > 2
              IF DELINQ = 1
                 PERFORM SUM-FIELDS-3-TY
                     VARYING AGESUB FROM 1 BY 1
                                          UNTIL AGESUB > 10
                         AFTER RECCON FROM 1 BY 1
                                          UNTIL RECCON > 2
                 GO TO SETUP-PAGE3-SUMMARY-TOT-EXIT
              ELSE
                 GO TO SETUP-PAGE3-SUMMARY-TOT-EXIT.

           IF DELINQ = 4
              PERFORM SUM-FIELDS-3-TY
                  VARYING AGESUB FROM 1 BY 1
                                       UNTIL AGESUB > 10
                      AFTER RECCON FROM 1 BY 1
                                       UNTIL RECCON > 2
              GO TO SETUP-PAGE3-SUMMARY-TOT-EXIT.

           IF DELINQ = 5
              MOVE 1 TO PERIOD
              PERFORM SUM-FIELDS-3-BY
                  VARYING AGESUB FROM 1 BY 1
                                       UNTIL AGESUB > 10
                      AFTER RECCON FROM 1 BY 1
                                       UNTIL RECCON > 2
              PERFORM SUM-FIELDS-3-TY
                  VARYING AGESUB FROM 1 BY 1
                                       UNTIL AGESUB > 10
                      AFTER RECCON FROM 1 BY 1
                                       UNTIL RECCON > 2
              MOVE 2 TO PERIOD
              PERFORM SUM-FIELDS-3-BY-REL
                  VARYING AGESUB FROM 1 BY 1
                                       UNTIL AGESUB > 10
                      AFTER RECCON FROM 1 BY 1
                                       UNTIL RECCON > 2
              GO TO SETUP-PAGE3-SUMMARY-TOT-EXIT.

           IF DELINQ NOT = 7
              GO TO SKIP-DELINQ-7.

      **** BEGIN IF DELINQ = 7 ****
           ADD HBY-NO(1 1 1)
               HBY-NO(1 1 2)
               HBY-NO(1 1 3)
               HBY-NO(1 1 4)
               HBY-NO(1 1 5)
               HBY-NO(1 1 6)
               HBY-NO(1 1 7)
               HBY-NO(1 1 8)
               HBY-NO(1 1 9)
               HBY-NO(1 1 10)
               HBY-NO(1 1 11) TO SUM-DAILY-NO.

      *CURRENT,INC RECENCY 30S,MISSPAYS,POTS, CURRENT
      *ADD FROM THE DELINQUENCY BUCKETS
      *CURRENT,GAIN/LOSS,TYPE USED FOR F11 TOGGLE
           ADD HBY-UNITS(1,1,1)
               HBY-UNITS(1,1,8)
               HBY-UNITS(1,1,9)
               HBY-UNITS(1,1,10) TO SUM-DAILY-UNITS-RECENCY60.

           ADD HBY-AMT(1 1 1)
               HBY-AMT(1 1 2)
               HBY-AMT(1 1 3)
               HBY-AMT(1 1 4)
               HBY-AMT(1 1 5)
               HBY-AMT(1 1 6)
               HBY-AMT(1 1 7)
               HBY-AMT(1 1 8)
               HBY-AMT(1 1 9)
               HBY-AMT(1 1 10)
               HBY-AMT(1 1 11) TO SUM-DAILY-AMT.
           ADD HBY-OUTBAL(1,1,1)
               HBY-OUTBAL(1,1,8)
               HBY-OUTBAL(1,1,9)
               HBY-OUTBAL(1,1,10) TO SUM-DAILY-OUTBAL-RECENCY60.

           ADD HBY-NO(1 2 1)
               HBY-NO(1 2 2)
               HBY-NO(1 2 3)
               HBY-NO(1 2 4)
               HBY-NO(1 2 5)
               HBY-NO(1 2 6)
               HBY-NO(1 2 7)
               HBY-NO(1 2 8)
               HBY-NO(1 2 9)
               HBY-NO(1 2 10)
               HBY-NO(1 2 11) TO SUM-DAILY-VOLNO.
           ADD HBY-AMT(1 2 1)
               HBY-AMT(1 2 2)
               HBY-AMT(1 2 3)
               HBY-AMT(1 2 4)
               HBY-AMT(1 2 5)
               HBY-AMT(1 2 6)
               HBY-AMT(1 2 7)
               HBY-AMT(1 2 8)
               HBY-AMT(1 2 9)
               HBY-AMT(1 2 10)
               HBY-AMT(1 2 11) TO SUM-DAILY-VOL.
      * ACCUMULATE PL, P2, P3, P5 TOTALS FOR PAGE 3.
               ADD HBY-NO-PL(1 2 1) HBY-NO-PL(1 2 3)
                   HBY-NO-PL(1 2 5) HBY-NO-PL(1 2 7)
                   HBY-NO-PL(1 2 9) TO SUM-DAILY-PL-NO.
               ADD HBY-AMT-PL(1 2 1) HBY-AMT-PL(1 2 3)
                   HBY-AMT-PL(1 2 5) HBY-AMT-PL(1 2 7)
                   HBY-AMT-PL(1 2 9) TO SUM-DAILY-PL-AMT.
      * SUBTRACT OUT P2'S
           SUBTRACT HBY-NO-PL(1 2 3) FROM SUM-DAILY-PL-NO.
           SUBTRACT HBY-AMT-PL(1 2 3) FROM SUM-DAILY-PL-AMT.

           ADD HBY-SUMWGTTM TO SUM-DAILY-SUMWGTTM.
           ADD HBY-SUMWGTAV TO SUM-DAILY-SUMWGTAV.
           ADD HBY-SUMWGTTA TO SUM-DAILY-SUMWGTTA.
           ADD HBY-NO(1 2 1)
               HBY-NO(1 2 2)
               HBY-NO(1 2 9) TO SUM-DAILY-NB.

           ADD HBY-NO(1 2 3) TO SUM-DAILY-FB.
           ADD HBY-NO(1 2 4)
               HBY-NO(1 2 5)
               HBY-NO(1 2 6)
               HBY-NO(1 2 7)
               HBY-NO(1 2 8)
               HBY-NO(1 2 10)
               HBY-NO(1 2 11) TO SUM-DAILY-PB.
           ADD HBY-AMT(1 2 1)
               HBY-AMT(1 2 2)
               HBY-AMT(1 2 9) TO SUM-DAILY-NB-AMT. 

           ADD HBY-AMT(1 2 3) TO SUM-DAILY-FB-AMT.
           ADD HBY-AMT(1 2 4)
               HBY-AMT(1 2 5)
               HBY-AMT(1 2 6)
               HBY-AMT(1 2 7)
               HBY-AMT(1 2 8)
               HBY-AMT(1 2 10)
               HBY-AMT(1 2 11) TO SUM-DAILY-PB-AMT.
      *-------------------------------------------------------------------------
      * JTG 030421 ADDED GP-NO-SC-USE-DM -NO SALESFIN USE DIRECT MAIL WORLD #307
      *            TESTED TO DETERMINE IF TO INCLUDE 'DM' IN DAILY PAGE 3
      *            'NB' AND NOT 'PB' VOLUME NUMBERS
      *-------------------------------------------------------------------------
           IF GP-NO-SC-USE-DM = "Y"
              ADD HBY-NO(1 2 8)       TO SUM-DAILY-NB
              ADD HBY-AMT(1 2 8)      TO SUM-DAILY-NB-AMT
              SUBTRACT HBY-NO(1 2 8)  FROM SUM-DAILY-PB
              SUBTRACT HBY-AMT(1 2 8) FROM SUM-DAILY-PB-AMT.
      *-------------------------------------------------------------------------

           MOVE 1 TO PERIOD.
           PERFORM SUM-FIELDS-3-DAILY-BY
               VARYING AGESUB FROM 1 BY 1 UNTIL AGESUB > 9
                 AFTER RECCON FROM 1 BY 1 UNTIL RECCON > 2.

           ADD HTY-NO(1 1)
               HTY-NO(1 2)
               HTY-NO(1 3)
               HTY-NO(1 4)
               HTY-NO(1 5)
               HTY-NO(1 6)
               HTY-NO(1 7)
               HTY-NO(1 8)
               HTY-NO(1 9)
               HTY-NO(1 10)
               HTY-NO(1 11) TO SUM-DAILY-NO.
           ADD HTY-UNITS(1,1)
               HTY-UNITS(1,8)
               HTY-UNITS(1,9)
               HTY-UNITS(1,10) TO SUM-DAILY-UNITS-RECENCY60.

           ADD HTY-AMT(1 1)
               HTY-AMT(1 2)
               HTY-AMT(1 3)
               HTY-AMT(1 4)
               HTY-AMT(1 5)
               HTY-AMT(1 6)
               HTY-AMT(1 7)
               HTY-AMT(1 8)
               HTY-AMT(1 9)
               HTY-AMT(1 10)
               HTY-AMT(1 11) TO SUM-DAILY-AMT.
           ADD HTY-OUTBAL(1,1)
               HTY-OUTBAL(1,8)
               HTY-OUTBAL(1,9)
               HTY-OUTBAL(1,10) TO SUM-DAILY-OUTBAL-RECENCY60.  

           ADD HTY-NO(2 1)
               HTY-NO(2 2)
               HTY-NO(2 3)
               HTY-NO(2 4)
               HTY-NO(2 5)
               HTY-NO(2 6)
               HTY-NO(2 7)
               HTY-NO(2 8)
               HTY-NO(2 9)
               HTY-NO(2 10)
               HTY-NO(2 11) TO SUM-DAILY-VOLNO.
           ADD HTY-AMT(2 1)
               HTY-AMT(2 2)
               HTY-AMT(2 3)
               HTY-AMT(2 4)
               HTY-AMT(2 5)
               HTY-AMT(2 6)
               HTY-AMT(2 7)
               HTY-AMT(2 8)
               HTY-AMT(2 9)
               HTY-AMT(2 10)
               HTY-AMT(2 11) TO SUM-DAILY-VOL.
      * ACCUMULATE PL, P2, P3, P4, P5 ACTIVE TOTALS
               ADD HTY-NO-PL(2 1) HTY-NO-PL(2 3) HTY-NO-PL(2 5)
                   HTY-NO-PL(2 7) HTY-NO-PL(2 9) TO SUM-DAILY-PL-NO.
               ADD HTY-AMT-PL(2 1) HTY-AMT-PL(2 3) HTY-AMT-PL(2 5)
                   HTY-AMT-PL(2 7) HTY-AMT-PL(2 9)
                       TO SUM-DAILY-PL-AMT.
      * SUBTRACT OUT P2'S
           SUBTRACT HTY-NO-PL(2 3)  FROM SUM-DAILY-PL-NO.
           SUBTRACT HTY-AMT-PL(2 3) FROM SUM-DAILY-PL-AMT.

           ADD HTY-SUMWGTTM TO SUM-DAILY-SUMWGTTM.
           ADD HTY-SUMWGTAV TO SUM-DAILY-SUMWGTAV.
           ADD HTY-SUMWGTTA TO SUM-DAILY-SUMWGTTA.
           ADD HTY-NO(2 1)
               HTY-NO(2 2) 
               HTY-NO(2 9) TO SUM-DAILY-NB.

           ADD HTY-NO(2 3) TO SUM-DAILY-FB.
           ADD HTY-NO(2 4)
               HTY-NO(2 5)
               HTY-NO(2 6)
               HTY-NO(2 7)
               HTY-NO(2 8)
               HTY-NO(2 10)
               HTY-NO(2 11) TO SUM-DAILY-PB.

           ADD HTY-AMT(2 1)
               HTY-AMT(2 2) 
               HTY-AMT(2 9) TO SUM-DAILY-NB-AMT.

           ADD HTY-AMT(2 3) TO SUM-DAILY-FB-AMT.
           ADD HTY-AMT(2 4)
               HTY-AMT(2 5)
               HTY-AMT(2 6)
               HTY-AMT(2 7)
               HTY-AMT(2 8)
               HTY-AMT(2 10)
               HTY-AMT(2 11) TO SUM-DAILY-PB-AMT.
      *-------------------------------------------------------------------------
      * JTG 030421 ADDED GP-NO-SC-USE-DM -NO SALESFIN USE DIRECT MAIL WORLD #307
      *            TESTED TO DETERMINE IF TO INCLUDE 'DM' IN DAILY PAGE 3
      *            'NB' AND NOT 'PB' VOLUME NUMBERS
      *-------------------------------------------------------------------------
           IF GP-NO-SC-USE-DM = "Y"
              ADD HTY-NO(2 8)       TO SUM-DAILY-NB
              ADD HTY-AMT(2 8)      TO SUM-DAILY-NB-AMT
              SUBTRACT HTY-NO(2 8)  FROM SUM-DAILY-PB
              SUBTRACT HTY-AMT(2 8) FROM SUM-DAILY-PB-AMT.
      *-------------------------------------------------------------------------

           PERFORM SUM-FIELDS-3-DAILY-TY
               VARYING AGESUB FROM 1 BY 1 UNTIL AGESUB > 9
                 AFTER RECCON FROM 1 BY 1 UNTIL RECCON > 2.

           ADD HBY-NO(2 1 1)
               HBY-NO(2 1 2)
               HBY-NO(2 1 3)
               HBY-NO(2 1 4)
               HBY-NO(2 1 5)
               HBY-NO(2 1 6)
               HBY-NO(2 1 7)
               HBY-NO(2 1 8)
               HBY-NO(2 1 9)
               HBY-NO(2 1 10)
               HBY-NO(2 1 11) TO SUM-REL-DAILY-NO.

           ADD HBY-UNITS(3 1 1)
               HBY-UNITS(3 1 8)
               HBY-UNITS(3 1 9)
               HBY-UNITS(3,1,10) TO SUM-DAILY-REL-UNITS-RECENCY60.

           ADD HBY-AMT(2 1 1)
               HBY-AMT(2 1 2)
               HBY-AMT(2 1 3)
               HBY-AMT(2 1 4)
               HBY-AMT(2 1 5)
               HBY-AMT(2 1 6)
               HBY-AMT(2 1 7)
               HBY-AMT(2 1 8)
               HBY-AMT(2 1 9)
               HBY-AMT(2 1 10)
               HBY-AMT(2 1 11) TO SUM-REL-DAILY-AMT.

           ADD HBY-OUTBAL(3,1,1)
               HBY-OUTBAL(3,1,8)
               HBY-OUTBAL(3,1,9)
               HBY-OUTBAL(3,1,10) TO SUM-DAILY-REL-OUTBAL-RECENCY60.

           MOVE 2 TO SUB4.
           PERFORM SUM-FIELDS-3-DAILY-BY-REL
               VARYING AGESUB FROM 1 BY 1 UNTIL AGESUB > 9
                 AFTER RECCON FROM 1 BY 1 UNTIL RECCON > 2.
           GO TO SETUP-PAGE3-SUMMARY-TOT-EXIT.
      **** END IF DELINQ = 7 ****
       SKIP-DELINQ-7.

       SETUP-PAGE3-SUMMARY-TOT-EXIT.
           EXIT.

      *******************************************
      *    SUM FIELDS FROM BRANCH ANALYSYS PAGE 1
      *******************************************
       SUM-FIELDS-1-BY SECTION.
           ADD HBY-OTHCOL(MTDYTD)    TO SUM1(1 MTDYTD).
           ADD HBY-OT2COL(MTDYTD)    TO SUM1(2 MTDYTD).
           ADD HBY-INTCOL(MTDYTD)    TO SUM1(3 MTDYTD).
           ADD HBY-LATECOL(MTDYTD)   TO SUM1(4 MTDYTD).
           ADD HBY-PRINCOL(MTDYTD)   TO SUM1(5 MTDYTD).
           ADD HBY-DEFCOL(MTDYTD)    TO SUM1(6 MTDYTD).
           ADD HBY-RNPRIN(MTDYTD)    TO SUM1(7 MTDYTD).
           ADD HBY-PART-CHGOFF(MTDYTD) TO SUM1(8 MTDYTD).
           ADD HBY-PROCEEDS(MTDYTD)  TO SUM1(9 MTDYTD).
           ADD HBY-NO-SOLD(MTDYTD)   TO SUM1-NO(1 MTDYTD).
           ADD HBY-NO-TRANS(MTDYTD)  TO SUM1-NO(2 MTDYTD).
           ADD HBY-NO-PURCH(MTDYTD)  TO SUM1-NO(3 MTDYTD).
           ADD HBY-AMT-SOLD(MTDYTD)  TO SUM1-AMT(1 MTDYTD).
           ADD HBY-AMT-TRANS(MTDYTD) TO SUM1-AMT(2 MTDYTD).
           ADD HBY-AMT-PURCH(MTDYTD) TO SUM1-AMT(3 MTDYTD).

       SUM-FIELDS-1-BY-EXIT.
           EXIT.

       SUM-FIELDS-1-TY SECTION.
           ADD HTY-OTHCOL    TO SUM1(1 MTDYTD).
           ADD HTY-OT2COL    TO SUM1(2 MTDYTD).
           ADD HTY-INTCOL    TO SUM1(3 MTDYTD).
           ADD HTY-LATECOL   TO SUM1(4 MTDYTD).
           ADD HTY-PRINCOL   TO SUM1(5 MTDYTD).
           ADD HTY-DEFCOL    TO SUM1(6 MTDYTD).
           ADD HTY-RNPRIN    TO SUM1(7 MTDYTD).
           ADD HTY-PART-CHGOFF TO SUM1(8 MTDYTD).
           ADD HTY-PROCEEDS  TO SUM1(9 MTDYTD).
           ADD HTY-NO-SOLD   TO SUM1-NO(1 MTDYTD).
           ADD HTY-NO-TRANS  TO SUM1-NO(2 MTDYTD).
           ADD HTY-NO-PURCH  TO SUM1-NO(3 MTDYTD).
           ADD HTY-AMT-SOLD  TO SUM1-AMT(1 MTDYTD).
           ADD HTY-AMT-TRANS TO SUM1-AMT(2 MTDYTD).
           ADD HTY-AMT-PURCH TO SUM1-AMT(3 MTDYTD).

       SUM-FIELDS-1-TY-EXIT.
           EXIT.

      ********************************************************
      *    SUM FIELDS BRANCH ANALYSIS PAGE 2
      ********************************************************
      *  PERIOD:
      *    PERIOD TABLE: 1 - CURRENT           1 - MTD
      *                  2 - LAST MONTH        2 - YTD
      *                  3 - LAST YEAR         3 - LYR
      *
      *  ACTSUB:
      *    ACTIVITY TBL: 1 - NET GAIN / LOSS   2 - NEW VOLUME
      *
      *  TYPSUB:
      *    TYPE TABLE  : 1 - NB      7 - RN
      *                  2 - RF      8 - SC
      *                  3 - FB      9 - OT
      *                  4 - PB     10 - RB
      *                  5 - PBD    11 - RO
      *                  6 - PBI    12 - TOTAL OF ALL LEVELS
      *
      ********************************************************
       SUM-FIELDS-2-BY SECTION.
           MOVE TYPSUB TO SUB4.
           IF (ACTSUB = 1 AND PERIOD = 1) OR ACTSUB = 2
              PERFORM SUM-FIELDS-2-BY-FIXED
              MOVE 12 TO SUB4
              PERFORM SUM-FIELDS-2-BY-FIXED.
           IF ACTSUB = 1
              IF PERIOD = 2 OR 3
              PERFORM SUM-FIELDS-2-BY-DIFF
              MOVE 12 TO SUB4
              PERFORM SUM-FIELDS-2-BY-DIFF.

       EXIT-SUM-FIELDS-2-BY.
           EXIT.

      *----------------------------------------------------------------
      * SUM FIELDS 2 BY FIXED SECTION:
      *-------------------------------
      * NO OF ACCOUNTS DISPLAYED ONLY WITH PAGE2-REC AND PAGE2-NO-O2.
      *  IF PAGE2-REC   - DISPLAY RECEIVABLES WITH O2 ADDED IN.
      *  IF PAGE2-NO-O2 - DISPLAY RECEIVABLES WITH O2 SUBTRACTED OUT.
      *  IF PAGE2-O2    - SHOW O2 AMOUNTS ONLY (NO RECEIVABLES).
      *  IF PAGE2-INT   - SHOW INTEREST CHARGES ONLY.
      *----------------------------------------------------------------
       SUM-FIELDS-2-BY-FIXED SECTION.
           IF (PERIOD = 1 AND SUB4 = 12)
              ADD HBY-NO(PERIOD 1 TYPSUB) TO SUM-NO-OUTS.
           IF PAGE2-REC
              ADD HBY-NO(PERIOD ACTSUB TYPSUB) TO SUM-NO(SUB4 PERIOD)
              ADD HBY-AMT(PERIOD ACTSUB TYPSUB) TO SUM-AMT(SUB4 PERIOD)
              GO TO EXIT-SUM-FIELDS-2-BY-FIXED.
           IF PAGE2-NO-O2
              ADD HBY-NO(PERIOD ACTSUB TYPSUB) TO SUM-NO(SUB4 PERIOD)
              ADD HBY-AMT(PERIOD ACTSUB TYPSUB) TO SUM-AMT(SUB4 PERIOD)
              SUBTRACT HBY-O2(PERIOD ACTSUB TYPSUB)
                  FROM SUM-AMT(SUB4 PERIOD)
              GO TO EXIT-SUM-FIELDS-2-BY-FIXED.
           IF PAGE2-O2
              MOVE 0 TO SUM-NO(SUB4 PERIOD)
              ADD HBY-O2(PERIOD ACTSUB TYPSUB) TO SUM-AMT(SUB4 PERIOD)
              GO TO EXIT-SUM-FIELDS-2-BY-FIXED.
           IF PAGE2-INT
              MOVE 0 TO SUM-NO(SUB4 PERIOD)
              ADD HBY-INT(PERIOD ACTSUB TYPSUB) TO SUM-AMT(SUB4 PERIOD)
              GO TO EXIT-SUM-FIELDS-2-BY-FIXED.
       EXIT-SUM-FIELDS-2-BY-FIXED.
           EXIT.

      *----------------------------------------------------------------
      * SUM FIELDS 2 BY DIFF SECTION:
      *------------------------------
      * DISPLAYS DIFFERENCE BETWEEN CURRENT AND ELM, ELY.
      *    NO OF ACCOUNTS DISPLAYED ONLY WITH PAGE2-REC AND PAGE2-NO-O2.
      *  IF PAGE2-REC   - DISPLAY RECEIVABLES WITH O2 ADDED IN.
      *                      (CUR) - (ELM)   [O2 INCLUDED]
      *  IF PAGE2-NO-O2 - DISPLAY RECEIVABLES WITH O2 SUBTRACTED OUT.
      *                      (CUR - 02) - (ELM - O2)
      *  IF PAGE2-O2    - SHOW O2 AMOUNTS ONLY (NO RECEIVABLES).
      *                      (CUR 02) - (ELM O2)
      *  IF PAGE2-INT   - SHOW INTEREST CHARGES ONLY.
      *                      (CUR INT) - (ELM INT)
      *----------------------------------------------------------------
       SUM-FIELDS-2-BY-DIFF SECTION.
           IF (PERIOD = 1 AND SUB4 = 12)
              ADD HBY-NO(PERIOD 1 TYPSUB) TO SUM-NO-OUTS.
           IF PAGE2-REC
              ADD HBY-NO(1 ACTSUB TYPSUB) TO SUM-NO(SUB4 PERIOD)
              SUBTRACT HBY-NO(PERIOD ACTSUB TYPSUB)
                  FROM SUM-NO(SUB4 PERIOD)
              ADD HBY-AMT(1 ACTSUB TYPSUB) TO SUM-AMT(SUB4 PERIOD)
              SUBTRACT HBY-AMT(PERIOD ACTSUB TYPSUB)
                  FROM SUM-AMT(SUB4 PERIOD)
              GO TO EXIT-SUM-FIELDS-2-BY-DIFF.
           IF PAGE2-NO-O2
              ADD HBY-NO(1 ACTSUB TYPSUB) TO SUM-NO(SUB4 PERIOD)
              SUBTRACT HBY-NO(PERIOD ACTSUB TYPSUB)
                  FROM SUM-NO(SUB4 PERIOD)
              ADD HBY-AMT(1 ACTSUB TYPSUB) TO SUM-AMT(SUB4 PERIOD)
              SUBTRACT HBY-O2(1 ACTSUB TYPSUB)
                  FROM SUM-AMT(SUB4 PERIOD)
              SUBTRACT HBY-AMT(PERIOD ACTSUB TYPSUB)
                  FROM SUM-AMT(SUB4 PERIOD)
              ADD HBY-O2(PERIOD ACTSUB TYPSUB) TO SUM-AMT(SUB4 PERIOD)
              GO TO EXIT-SUM-FIELDS-2-BY-DIFF.
           IF PAGE2-O2
              MOVE 0 TO SUM-NO(SUB4 PERIOD)
              ADD HBY-O2(1 ACTSUB TYPSUB) TO SUM-AMT(SUB4 PERIOD)
              SUBTRACT HBY-O2(PERIOD ACTSUB TYPSUB)
                  FROM SUM-AMT(SUB4 PERIOD)
              GO TO EXIT-SUM-FIELDS-2-BY-DIFF.
           IF PAGE2-INT
              MOVE 0 TO SUM-NO(SUB4 PERIOD)
              ADD HBY-INT(1 ACTSUB TYPSUB) TO SUM-AMT(SUB4 PERIOD)
              SUBTRACT HBY-INT(PERIOD ACTSUB TYPSUB)
                  FROM SUM-AMT(SUB4 PERIOD)
              GO TO EXIT-SUM-FIELDS-2-BY-DIFF.
       EXIT-SUM-FIELDS-2-BY-DIFF.
           EXIT.

      ********************************************************
      *    SUM FIELDS TODAYS TOTALS PAGE 2
      ********************************************************
      *
      *  ACTSUB:
      *    ACTIVITY TBL: 1 - NET FIXED / LOSS   2 - NEW VOLUME
      *
      *  TYPSUB:
      *    TYPE TABLE  : 1 - NB     7 - RN
      *                  2 - RF     8 - SC
      *                  3 - FB     9 - OT
      *                  4 - PB    10 - RB
      *                  5 - PBD   11 - RO
      *                  6 - PBI   12 - TOTAL OF ALL LEVELS
      ********************************************************
       SUM-FIELDS-2-TY SECTION.
           MOVE TYPSUB TO SUB4.
           PERFORM SUM-FIELDS-2-TY-FIXED.
           MOVE 12 TO SUB4.
           PERFORM SUM-FIELDS-2-TY-FIXED.

       EXIT-SUM-FIELDS-2-TY.
           EXIT.

      *----------------------------------------------------------------
      * SUM FIELDS 2 TY FIXED SECTION:
      *-------------------------------
      * DISPLAYS DIFFERENCE BETWEEN CURRENT AND ELM, ELY.
      *    NO OF ACCOUNTS DISPLAYED ONLY WITH PAGE2-REC AND PAGE2-NO-02.
      *  IF PAGE2-TODAY - INCREMENT ONLY CURRENT TOTALS (SKIP MTD AND YTD).
      *  IF PAGE2-TOTAL - INCREMENT CUR, MTD, YTD.
      *
      *  IF PAGE2-REC   - INCREMENT RECEIVABLES WITH O2 ADDED IN.
      *                      (CUR) - (ELM)   [O2 INCLUDED]
      *  IF PAGE2-NO-O2 - INCREMENT RECEIVABLES WITH O2 SUBTRACTED OUT.
      *                      (CUR - 02) - (ELM - O2)
      *  IF PAGE2-O2    - INCREMENT O2 AMOUNTS ONLY (NO RECEIVABLES).
      *                      (CUR 02) - (ELM O2)
      *  IF PAGE2-INT   - INCREMENT INTEREST CHARGES ONLY.
      *                      (CUR INT) - (ELM INT)
      *
      *  NOTE: IF ACTIVITY IS GAIN-LOSS, ADDS TO ALL PERIODS, BUT
      *        IF ACTIVITY IS VOLUME, JUST ADD TO MTD & YTD, NOT LAST YTD
      *----------------------------------------------------------------
       SUM-FIELDS-2-TY-FIXED SECTION.
           IF PAGE2-TODAY
              GO TO PAGE-2-DAILY.
           IF PAGE2-REC
              ADD HTY-NO(ACTSUB TYPSUB) TO SUM-NO(SUB4 1)
              ADD HTY-AMT(ACTSUB TYPSUB) TO SUM-AMT(SUB4 1)
              ADD HTY-NO(ACTSUB TYPSUB) TO SUM-NO(SUB4 2)
              ADD HTY-AMT(ACTSUB TYPSUB) TO SUM-AMT(SUB4 2)
              IF ACTSUB = 1
                 ADD HTY-NO(ACTSUB TYPSUB) TO SUM-NO(SUB4 3)
                 ADD HTY-AMT(ACTSUB TYPSUB) TO SUM-AMT(SUB4 3).
           IF PAGE2-NO-O2
              ADD HTY-NO(ACTSUB TYPSUB) TO SUM-NO(SUB4 1)
              ADD HTY-AMT(ACTSUB TYPSUB) TO SUM-AMT(SUB4 1)
              SUBTRACT HTY-O2(ACTSUB TYPSUB) FROM SUM-AMT(SUB4 1)
              ADD HTY-AMT(ACTSUB TYPSUB) TO SUM-AMT(SUB4 2)
              ADD HTY-NO(ACTSUB TYPSUB) TO SUM-NO(SUB4 2)
              SUBTRACT HTY-O2(ACTSUB TYPSUB) FROM SUM-AMT(SUB4 2)
              IF ACTSUB = 1
                 ADD HTY-NO(ACTSUB TYPSUB) TO SUM-NO(SUB4 3)
                 ADD HTY-AMT(ACTSUB TYPSUB) TO SUM-AMT(SUB4 3)
                 SUBTRACT HTY-O2(ACTSUB TYPSUB) FROM SUM-AMT(SUB4 3).
           IF PAGE2-O2
              ADD HTY-O2(ACTSUB TYPSUB) TO SUM-AMT(SUB4 1)
              ADD HTY-O2(ACTSUB TYPSUB) TO SUM-AMT(SUB4 2)
              IF ACTSUB = 1
                 ADD HTY-O2(ACTSUB TYPSUB) TO SUM-AMT(SUB4 3).
           IF PAGE2-INT
              ADD HTY-INT(ACTSUB TYPSUB) TO SUM-AMT(SUB4 1)
              ADD HTY-INT(ACTSUB TYPSUB) TO SUM-AMT(SUB4 2)
              IF ACTSUB = 1
                 ADD HTY-INT(ACTSUB TYPSUB) TO SUM-AMT(SUB4 3).

           GO TO EXIT-SUM-FIELDS-2-TY-FIXED.

         PAGE-2-DAILY.
           IF PAGE2-REC
              ADD HTY-NO(ACTSUB TYPSUB) TO SUM-NO(SUB4 1)
              ADD HTY-AMT(ACTSUB TYPSUB) TO SUM-AMT(SUB4 1).
           IF PAGE2-NO-O2
              ADD HTY-NO(ACTSUB TYPSUB) TO SUM-NO(SUB4 1)
              ADD HTY-AMT(ACTSUB TYPSUB) TO SUM-AMT(SUB4 1)
              SUBTRACT HTY-O2(ACTSUB TYPSUB) FROM SUM-AMT(SUB4 1).
           IF PAGE2-O2
              ADD HTY-O2(ACTSUB TYPSUB) TO SUM-AMT(SUB4 1).
           IF PAGE2-INT
              ADD HTY-INT(ACTSUB TYPSUB) TO SUM-AMT(SUB4 1).
       EXIT-SUM-FIELDS-2-TY-FIXED.
           EXIT.

      *******************************************
      *    SUM FIELDS PAGE 3
      *******************************************
       SUM-FIELDS-3-BY SECTION.

      * INCLUDE BANKRUPT DEL:
           IF BNKRPT-DEL = 1
              ADD HBY-UNITS(PERIOD RECCON AGESUB)
                                  TO SUM-DEL-UNITS(AGESUB RECCON)
              ADD HBY-OUTBAL(PERIOD RECCON AGESUB)
                                  TO SUM-DEL-OUTBAL(AGESUB RECCON)
           ELSE
      * SKIP BANKRUPT DEL:
           IF BNKRPT-DEL = 2
              ADD HBY-UNITS(PERIOD RECCON AGESUB)
                                  TO SUM-DEL-UNITS(AGESUB RECCON)
              ADD HBY-OUTBAL(PERIOD RECCON AGESUB)
                                  TO SUM-DEL-OUTBAL(AGESUB RECCON)
              SUBTRACT HBY-UNITS-BNKRPT(PERIOD RECCON AGESUB)
                                  FROM SUM-DEL-UNITS(AGESUB RECCON)
              SUBTRACT HBY-OUTBAL-BNKRPT(PERIOD RECCON AGESUB)
                                  FROM SUM-DEL-OUTBAL(AGESUB RECCON)
           ELSE
      * ONLY BANKRUPT DEL:
           IF BNKRPT-DEL = 3
              ADD HBY-UNITS-BNKRPT(PERIOD RECCON AGESUB)
                                  TO SUM-DEL-UNITS(AGESUB RECCON)
              ADD HBY-OUTBAL-BNKRPT(PERIOD RECCON AGESUB)
                                  TO SUM-DEL-OUTBAL(AGESUB RECCON).

       SUM-FIELDS-3-TY SECTION.

      * INCLUDE BANKRUPT DEL:
           IF BNKRPT-DEL = 1
              ADD HTY-UNITS(RECCON AGESUB)
                                  TO SUM-DEL-UNITS(AGESUB RECCON)
              ADD HTY-OUTBAL(RECCON AGESUB)
                                  TO SUM-DEL-OUTBAL(AGESUB RECCON)
           ELSE
      * SKIP BANKRUPT DEL:
           IF BNKRPT-DEL = 2
              ADD HTY-UNITS(RECCON AGESUB)
                                  TO SUM-DEL-UNITS(AGESUB RECCON)
              ADD HTY-OUTBAL(RECCON AGESUB)
                                  TO SUM-DEL-OUTBAL(AGESUB RECCON)
              SUBTRACT HTY-UNITS-BNKRPT(RECCON AGESUB)
                                  FROM SUM-DEL-UNITS(AGESUB RECCON)
              SUBTRACT HTY-OUTBAL-BNKRPT(RECCON AGESUB)
                                  FROM SUM-DEL-OUTBAL(AGESUB RECCON)
           ELSE
      * ONLY BANKRUPT DEL:
           IF BNKRPT-DEL = 3
              ADD HTY-UNITS-BNKRPT(RECCON AGESUB)
                                  TO SUM-DEL-UNITS(AGESUB RECCON)
              ADD HTY-OUTBAL-BNKRPT(RECCON AGESUB)
                                  TO SUM-DEL-OUTBAL(AGESUB RECCON).

       SUM-FIELDS-3-BY-REL SECTION.

      * INCLUDE BANKRUPT DEL:
           IF BNKRPT-DEL = 1
              ADD HBY-UNITS(PERIOD RECCON AGESUB)
                                TO SUM-REL-DEL-UNITS(AGESUB RECCON)
              ADD HBY-OUTBAL(PERIOD RECCON AGESUB)
                                TO SUM-REL-DEL-OUTBAL(AGESUB RECCON)
           ELSE
      * SKIP BANKRUPT DEL:
           IF BNKRPT-DEL = 2
              ADD HBY-UNITS(PERIOD RECCON AGESUB)
                                TO SUM-REL-DEL-UNITS(AGESUB RECCON)
              ADD HBY-OUTBAL(PERIOD RECCON AGESUB)
                                TO SUM-REL-DEL-OUTBAL(AGESUB RECCON)
              SUBTRACT HBY-UNITS-BNKRPT(PERIOD RECCON AGESUB)
                                FROM SUM-REL-DEL-UNITS(AGESUB RECCON)
              SUBTRACT HBY-OUTBAL-BNKRPT(PERIOD RECCON AGESUB)
                                FROM SUM-REL-DEL-OUTBAL(AGESUB RECCON)
           ELSE
      * ONLY BANKRUPT DEL:
           IF BNKRPT-DEL = 3
              ADD HBY-UNITS-BNKRPT(PERIOD RECCON AGESUB)
                                TO SUM-REL-DEL-UNITS(AGESUB RECCON)
              ADD HBY-OUTBAL-BNKRPT(PERIOD RECCON AGESUB)
                                TO SUM-REL-DEL-OUTBAL(AGESUB RECCON).

      *******************************************
      *    SUM FIELDS DAILY PAGE 3
      *******************************************
       SUM-FIELDS-3-DAILY-BY SECTION.
           IF AGESUB > 2 AND < 8
              MOVE 3 TO SUBX
           ELSE
              IF AGESUB = 8
                 MOVE 4 TO SUBX
              ELSE
                 IF AGESUB = 9
                    MOVE 5 TO SUBX
                 ELSE
                    MOVE AGESUB TO SUBX.

      * INCLUDE BANKRUPT DEL:
           IF BNKRPT-DEL = 1
              ADD HBY-UNITS(PERIOD RECCON AGESUB)
                               TO SUM-DAILY-UNITS(SUBX RECCON)
              ADD HBY-OUTBAL(PERIOD RECCON AGESUB)
                               TO SUM-DAILY-OUTBAL(SUBX RECCON)
           ELSE
      * SKIP BANKRUPT DEL:
           IF BNKRPT-DEL = 2
              ADD HBY-UNITS(PERIOD RECCON AGESUB)
                               TO SUM-DAILY-UNITS(SUBX RECCON)
              ADD HBY-OUTBAL(PERIOD RECCON AGESUB)
                               TO SUM-DAILY-OUTBAL(SUBX RECCON)
              SUBTRACT HBY-UNITS-BNKRPT(PERIOD RECCON AGESUB)
                               FROM SUM-DAILY-UNITS(SUBX RECCON)
              SUBTRACT HBY-OUTBAL-BNKRPT(PERIOD RECCON AGESUB)
                               FROM SUM-DAILY-OUTBAL(SUBX RECCON)
           ELSE
      * ONLY BANKRUPT DEL:
           IF BNKRPT-DEL = 3
              ADD HBY-UNITS-BNKRPT(PERIOD RECCON AGESUB)
                               TO SUM-DAILY-UNITS(SUBX RECCON)
              ADD HBY-OUTBAL-BNKRPT(PERIOD RECCON AGESUB)
                               TO SUM-DAILY-OUTBAL(SUBX RECCON).

      * SET CONTRACTUAL MISPAYS INTO RECENCY IF WORLD:
           IF RECCON = 2
              IF AGESUB = 8
      * INCLUDE BANKRUPT DEL:
                 IF BNKRPT-DEL = 1
                    ADD HBY-UNITS(PERIOD RECCON AGESUB)
                                   TO SUM-DAILY-UNITS(SUBX 1)
                    ADD HBY-OUTBAL(PERIOD RECCON AGESUB)
                                   TO SUM-DAILY-OUTBAL(SUBX 1)
                 ELSE
      * SKIP BANKRUPT DEL:
                    IF BNKRPT-DEL = 2
                       ADD HBY-UNITS(PERIOD RECCON AGESUB)
                                      TO SUM-DAILY-UNITS(SUBX 1)
                       ADD HBY-OUTBAL(PERIOD RECCON AGESUB)
                                      TO SUM-DAILY-OUTBAL(SUBX 1)
                       SUBTRACT HBY-UNITS-BNKRPT(PERIOD RECCON AGESUB)
                                      FROM SUM-DAILY-UNITS(SUBX 1)
                       SUBTRACT HBY-OUTBAL-BNKRPT(PERIOD RECCON AGESUB)
                                      FROM SUM-DAILY-OUTBAL(SUBX 1)
                    ELSE
      * ONLY BANKRUPT DEL:
                    IF BNKRPT-DEL = 3
                       ADD HBY-UNITS-BNKRPT(PERIOD RECCON AGESUB)
                                      TO SUM-DAILY-UNITS(SUBX 1)
                       ADD HBY-OUTBAL-BNKRPT(PERIOD RECCON AGESUB)
                                      TO SUM-DAILY-OUTBAL(SUBX 1).

       SUM-FIELDS-3-DAILY-TY SECTION.
           IF AGESUB > 2 AND < 8
              MOVE 3 TO SUBX
           ELSE
              IF AGESUB = 8
                 MOVE 4 TO SUBX
              ELSE
                 IF AGESUB = 9
                    MOVE 5 TO SUBX
                 ELSE
                    MOVE AGESUB TO SUBX.

      * INCLUDE BANKRUPT DEL:
           IF BNKRPT-DEL = 1
              ADD HTY-UNITS(RECCON AGESUB)
                               TO SUM-DAILY-UNITS(SUBX RECCON)
              ADD HTY-OUTBAL(RECCON AGESUB)
                               TO SUM-DAILY-OUTBAL(SUBX RECCON)
           ELSE
      * SKIP BANKRUPT DEL:
           IF BNKRPT-DEL = 2
              ADD HTY-UNITS(RECCON AGESUB)
                               TO SUM-DAILY-UNITS(SUBX RECCON)
              ADD HTY-OUTBAL(RECCON AGESUB)
                               TO SUM-DAILY-OUTBAL(SUBX RECCON)
              SUBTRACT HTY-UNITS-BNKRPT(RECCON AGESUB)
                               FROM SUM-DAILY-UNITS(SUBX RECCON)
              SUBTRACT HTY-OUTBAL-BNKRPT(RECCON AGESUB)
                               FROM SUM-DAILY-OUTBAL(SUBX RECCON)
           ELSE
      * ONLY BANKRUPT DEL:
           IF BNKRPT-DEL = 3
              ADD HTY-UNITS-BNKRPT(RECCON AGESUB)
                               TO SUM-DAILY-UNITS(SUBX RECCON)
              ADD HTY-OUTBAL-BNKRPT(RECCON AGESUB)
                               TO SUM-DAILY-OUTBAL(SUBX RECCON).

      * SET CONTRACTUAL MISPAYS INTO RECENCY IF WORLD:
           IF RECCON = 2
              IF AGESUB = 8
      * INCLUDE BANKRUPT DEL:
                 IF BNKRPT-DEL = 1
                    ADD HTY-UNITS(RECCON AGESUB)
                                  TO SUM-DAILY-UNITS(SUBX 1)
                    ADD HTY-OUTBAL(RECCON AGESUB)
                                  TO SUM-DAILY-OUTBAL(SUBX 1)
                 ELSE
      * SKIP BANKRUPT DEL:
                    IF BNKRPT-DEL = 2
                       ADD HTY-UNITS(RECCON AGESUB)
                                     TO SUM-DAILY-UNITS(SUBX 1)
                       ADD HTY-OUTBAL(RECCON AGESUB)
                                     TO SUM-DAILY-OUTBAL(SUBX 1)
                       SUBTRACT HTY-UNITS-BNKRPT(RECCON AGESUB)
                                     FROM SUM-DAILY-UNITS(SUBX 1)
                       SUBTRACT HTY-OUTBAL-BNKRPT(RECCON AGESUB)
                                     FROM SUM-DAILY-OUTBAL(SUBX 1)
                    ELSE
      * ONLY BANKRUPT DEL:
                    IF BNKRPT-DEL = 3
                       ADD HTY-UNITS-BNKRPT(RECCON AGESUB)
                                     TO SUM-DAILY-UNITS(SUBX 1)
                       ADD HTY-OUTBAL-BNKRPT(RECCON AGESUB)
                                     TO SUM-DAILY-OUTBAL(SUBX 1).


       SUM-FIELDS-3-DAILY-BY-REL SECTION.
           IF AGESUB > 2 AND < 8
              MOVE 3 TO SUBX
           ELSE
              IF AGESUB = 8
                 MOVE 4 TO SUBX
              ELSE
                 IF AGESUB = 9
                    MOVE 5 TO SUBX
                 ELSE
                    MOVE AGESUB TO SUBX.

      * INCLUDE BANKRUPT DEL:
           IF BNKRPT-DEL = 1
              ADD HBY-UNITS(SUB4 RECCON AGESUB)
                             TO SUM-REL-DAILY-UNITS(SUBX RECCON)
              ADD HBY-OUTBAL(SUB4 RECCON AGESUB)
                             TO SUM-REL-DAILY-OUTBAL(SUBX RECCON)
           ELSE
      * SKIP BANKRUPT DEL:
           IF BNKRPT-DEL = 2
              ADD HBY-UNITS(SUB4 RECCON AGESUB)
                             TO SUM-REL-DAILY-UNITS(SUBX RECCON)
              ADD HBY-OUTBAL(SUB4 RECCON AGESUB)
                             TO SUM-REL-DAILY-OUTBAL(SUBX RECCON)
              SUBTRACT HBY-UNITS-BNKRPT(SUB4 RECCON AGESUB)
                             FROM SUM-REL-DAILY-UNITS(SUBX RECCON)
              SUBTRACT HBY-OUTBAL-BNKRPT(SUB4 RECCON AGESUB)
                             FROM SUM-REL-DAILY-OUTBAL(SUBX RECCON)
           ELSE
      * ONLY BANKRUPT DEL:
           IF BNKRPT-DEL = 3
              ADD HBY-UNITS-BNKRPT(SUB4 RECCON AGESUB)
                             TO SUM-REL-DAILY-UNITS(SUBX RECCON)
              ADD HBY-OUTBAL-BNKRPT(SUB4 RECCON AGESUB)
                             TO SUM-REL-DAILY-OUTBAL(SUBX RECCON).

      * SET CONTRACTUAL MISPAYS INTO RECENCY IF WORLD:
           IF RECCON = 2
              IF AGESUB = 8
      * INCLUDE BANKRUPT DEL:
                 IF BNKRPT-DEL = 1
                    ADD HBY-UNITS(SUB4 RECCON AGESUB)
                              TO SUM-REL-DAILY-UNITS(SUBX 1)
                    ADD HBY-OUTBAL(SUB4 RECCON AGESUB)
                             TO SUM-REL-DAILY-OUTBAL(SUBX 1)
                 ELSE
      * SKIP BANKRUPT DEL:
                    IF BNKRPT-DEL = 2
                       ADD HBY-UNITS(SUB4 RECCON AGESUB)
                                 TO SUM-REL-DAILY-UNITS(SUBX 1)
                       ADD HBY-OUTBAL(SUB4 RECCON AGESUB)
                                TO SUM-REL-DAILY-OUTBAL(SUBX 1)
                       SUBTRACT HBY-UNITS-BNKRPT(SUB4 RECCON AGESUB)
                                 FROM SUM-REL-DAILY-UNITS(SUBX 1)
                       SUBTRACT HBY-OUTBAL-BNKRPT(SUB4 RECCON AGESUB)
                                FROM SUM-REL-DAILY-OUTBAL(SUBX 1)
                    ELSE
      * ONLY BANKRUPT DEL:
                    IF BNKRPT-DEL = 3
                       ADD HBY-UNITS-BNKRPT(SUB4 RECCON AGESUB)
                                 TO SUM-REL-DAILY-UNITS(SUBX 1)
                       ADD HBY-OUTBAL-BNKRPT(SUB4 RECCON AGESUB)
                                TO SUM-REL-DAILY-OUTBAL(SUBX 1).

      *****************************************************************
      *    SUM FIELDS BRANCH ANALYSIS PAGE 4
      *****************************************************************
      *  PERIOD:
      *    PERIOD TABLE: 1 - CURRENT           1 - MTD
      *                  2 - LAST MONTH        2 - YTD
      *                  3 - LAST YEAR         3 - LYR
      *  ACTSUB:
      *    ACTIVITY TBL: 1 - NET GAIN / LOSS   2 - NEW VOLUME
      *
      *  REPOSUB:
      *    TYPE TABLE  : 1 - REPO ON HAND
      *                  2 - REPO REDEEMED
      *                  3 - REPO SOLD LIVE RECEIVABLE
      *                  4 - REPO ON HAND - P&L RECEIVABLE
      *                  5 - REPO SOLD - P&L RECEIVABLE
      *****************************************************************
       SUM-FIELDS-4-BY SECTION.
           IF (ACTSUB = 1 AND PERIOD = 1) OR ACTSUB = 2
              PERFORM SUM-FIELDS-4-BY-FIXED.
           IF ACTSUB = 1
              IF PERIOD = 2 OR 3
                 PERFORM SUM-FIELDS-4-BY-DIFF.
       EXIT-SUM-FIELDS-4-BY.
           EXIT.

      *----------------------------------------------------------------
      * SUM FIELDS 4 BY FIXED SECTION:
      *-------------------------------
      * NO OF ACCOUNTS DISPLAYED ONLY WITH PAGE4-REC AND PAGE4-NO-O2.
      *
      *  IF PAGE4-REC   - INCREMENT RECEIVABLES WITH O2 ADDED IN.
      *  IF PAGE4-NO-O2 - INCREMENT RECEIVABLES WITH O2 SUBTRACTED OUT.
      *  IF PAGE4-O2    - INCREMENT O2 AMOUNTS ONLY (NO RECEIVABLES).
      *  IF PAGE4-INT   - INCREMENT INTEREST CHARGES ONLY.
      *----------------------------------------------------------------
       SUM-FIELDS-4-BY-FIXED SECTION.
           IF PAGE4-REC
              ADD HBY-NO-REPO(PERIOD ACTSUB REPOSUB) TO
                  SUM-NO-REPO(REPOSUB PERIOD)
              ADD HBY-AMT-REPO(PERIOD ACTSUB REPOSUB) TO
                  SUM-AMT-REPO(REPOSUB PERIOD)
              GO TO EXIT-SUM-FIELDS-4-BY-FIXED.
           IF PAGE4-NO-O2
              ADD HBY-NO-REPO(PERIOD ACTSUB REPOSUB) TO
                  SUM-NO-REPO(REPOSUB PERIOD)
              ADD HBY-AMT-REPO(PERIOD ACTSUB REPOSUB) TO
                  SUM-AMT-REPO(REPOSUB PERIOD)
              SUBTRACT HBY-O2-REPO(PERIOD ACTSUB REPOSUB) FROM
                       SUM-AMT-REPO(REPOSUB PERIOD)
              GO TO EXIT-SUM-FIELDS-4-BY-FIXED.
           IF PAGE4-O2
              MOVE 0 TO SUM-NO-REPO(REPOSUB PERIOD)
              ADD HBY-O2-REPO(PERIOD ACTSUB REPOSUB) TO
                  SUM-AMT-REPO(REPOSUB PERIOD)
              GO TO EXIT-SUM-FIELDS-4-BY-FIXED.
           IF PAGE4-INT
              MOVE 0 TO SUM-NO-REPO(REPOSUB PERIOD)
              ADD HBY-INT-REPO(PERIOD ACTSUB REPOSUB) TO
                  SUM-AMT-REPO(REPOSUB PERIOD)
              GO TO EXIT-SUM-FIELDS-4-BY-FIXED.
       EXIT-SUM-FIELDS-4-BY-FIXED.
           EXIT.

      *----------------------------------------------------------------
      * SUM FIELDS 4 BY DIFF SECTION:
      *------------------------------
      * DISPLAYS DIFFERENCE BETWEEN CURRENT AND ELM, ELY.
      *    NO OF ACCOUNTS DISPLAYED ONLY WITH PAGE4-REC AND PAGE4-NO-O2
      *
      *  IF PAGE4-REC   - INCREMENT RECEIVABLES WITH O2 ADDED IN.
      *                      (CUR) - (ELM)   [O2 INCLUDED]
      *  IF PAGE4-NO-O2 - INCREMENT RECEIVABLES WITH O2 SUBTRACTED OUT.
      *                      (CUR - 02) - (ELM - O2)
      *  IF PAGE4-O2    - INCREMENT O2 AMOUNTS ONLY (NO RECEIVABLES).
      *                      (CUR 02) - (ELM O2)
      *  IF PAGE4-INT   - INCREMENT INTEREST CHARGES ONLY.
      *                      (CUR INT) - (ELM INT)
      *----------------------------------------------------------------
       SUM-FIELDS-4-BY-DIFF SECTION.
           IF PAGE4-REC
              ADD HBY-NO-REPO(1 ACTSUB REPOSUB) TO
                  SUM-NO-REPO(REPOSUB PERIOD)
              SUBTRACT HBY-NO-REPO(PERIOD ACTSUB REPOSUB) FROM
                       SUM-NO-REPO(REPOSUB PERIOD)
              ADD HBY-AMT-REPO(1 ACTSUB REPOSUB) TO
                  SUM-AMT-REPO(REPOSUB PERIOD)
              SUBTRACT HBY-AMT-REPO(PERIOD ACTSUB REPOSUB) FROM
                       SUM-AMT-REPO(REPOSUB PERIOD)
              GO TO EXIT-SUM-FIELDS-4-BY-DIFF.
           IF PAGE4-NO-O2
              ADD HBY-NO-REPO(1 ACTSUB REPOSUB) TO
                  SUM-NO-REPO(REPOSUB PERIOD)
              SUBTRACT HBY-NO-REPO(PERIOD ACTSUB REPOSUB) FROM
                       SUM-NO-REPO(REPOSUB PERIOD)
              ADD HBY-AMT-REPO(1 ACTSUB REPOSUB) TO
                  SUM-AMT-REPO(REPOSUB PERIOD)
              SUBTRACT HBY-O2-REPO(1 ACTSUB REPOSUB) FROM
                       SUM-AMT-REPO(REPOSUB PERIOD)
              SUBTRACT HBY-AMT-REPO(PERIOD ACTSUB REPOSUB) FROM
                       SUM-AMT-REPO(REPOSUB PERIOD)
              ADD HBY-O2-REPO(PERIOD ACTSUB REPOSUB) TO
                  SUM-AMT-REPO(REPOSUB PERIOD)
              GO TO EXIT-SUM-FIELDS-4-BY-DIFF.
           IF PAGE4-O2
              MOVE 0 TO SUM-NO-REPO(REPOSUB PERIOD)
              ADD HBY-O2-REPO(1 ACTSUB REPOSUB) TO
                  SUM-AMT-REPO(REPOSUB PERIOD)
              SUBTRACT HBY-O2-REPO(PERIOD ACTSUB REPOSUB) FROM
                       SUM-AMT-REPO(REPOSUB PERIOD)
              GO TO EXIT-SUM-FIELDS-4-BY-DIFF.
           IF PAGE4-INT
              MOVE 0 TO SUM-NO-REPO(REPOSUB PERIOD)
              ADD HBY-INT-REPO(1 ACTSUB REPOSUB) TO
                  SUM-AMT-REPO(REPOSUB PERIOD)
              SUBTRACT HBY-INT-REPO(PERIOD ACTSUB REPOSUB) FROM
                       SUM-AMT-REPO(REPOSUB PERIOD)
              GO TO EXIT-SUM-FIELDS-4-BY-DIFF.
       EXIT-SUM-FIELDS-4-BY-DIFF.
              EXIT.

      *****************************************************************
      *    SUM FIELDS TODAYS TOTALS PAGE 4
      *****************************************************************
      *
      *  ACTSUB:
      *    ACTIVITY TBL: 1 - NET FIXED / LOSS   2 - NEW VOLUME
      *
      *  REPOSUB:
      *    TYPE TABLE  : 1 - REPO ON HAND
      *                  2 - REPO REDEEMED
      *                  3 - REPO SOLD LIVE RECEIVABLE
      *                  4 - REPO ON HAND P&L RECEIVABLE
      *                  5 - REPO SOLD P&L RECEIVABLE
      *****************************************************************
       SUM-FIELDS-4-TY SECTION.
           IF ACTSUB = 1 OR ACTSUB = 2
              PERFORM SUM-FIELDS-4-TY-FIXED.

       EXIT-SUM-FIELDS-4-TY.
           EXIT.

      *----------------------------------------------------------------
      * SUM FIELDS 4 TY FIXED SECTION:
      *-------------------------------
      * DISPLAYS DIFFERENCE BETWEEN CURRENT AND ELM, ELY.
      *    NO OF ACCOUNTS DISPLAYED ONLY WITH PAGE2-REC AND PAGE2-NO-O2.
      *  IF PAGE4-TODAY - INCREMENT ONLY CURRENT TOTALS (SKIP MTD AND YTD).
      *  IF PAGE4-TOTAL - INCREMENT CUR, MTD, YTD.
      *
      *  IF PAGE4-REC   - INCREMENT RECEIVABLES WITH O2 ADDED IN.
      *  IF PAGE4-NO-O2 - INCREMENT RECEIVABLES WITH O2 SUBTRACTED OUT.
      *  IF PAGE4-O2    - INCREMENT O2 AMOUNTS ONLY (NO RECEIVABLES).
      *  IF PAGE4-INT   - INCREMENT INTEREST CHARGES ONLY.
      *----------------------------------------------------------------
       SUM-FIELDS-4-TY-FIXED SECTION.
           IF PAGE4-TODAY
              GO TO PAGE-4-DAILY.

           IF PAGE4-REC
              ADD HTY-NO-REPO(ACTSUB REPOSUB) TO
                  SUM-NO-REPO(REPOSUB 1)
              ADD HTY-AMT-REPO(ACTSUB REPOSUB) TO
                  SUM-AMT-REPO(REPOSUB 1)
              ADD HTY-NO-REPO(ACTSUB REPOSUB) TO
                  SUM-NO-REPO(REPOSUB 2)
              ADD HTY-AMT-REPO(ACTSUB REPOSUB) TO
                  SUM-AMT-REPO(REPOSUB 2)
              IF ACTSUB = 1
                 ADD HTY-NO-REPO(ACTSUB REPOSUB) TO
                     SUM-NO-REPO(REPOSUB 3)
                 ADD HTY-AMT-REPO(ACTSUB REPOSUB) TO
                     SUM-AMT-REPO(REPOSUB 3)
              ELSE
                 GO TO EXIT-SUM-FIELDS-4-TY-FIXED.
           IF PAGE4-NO-O2
              ADD HTY-NO-REPO(ACTSUB REPOSUB) TO
                  SUM-NO-REPO(REPOSUB 1)
              ADD HTY-AMT-REPO(ACTSUB REPOSUB) TO
                  SUM-AMT-REPO(REPOSUB 1)
              SUBTRACT HTY-O2-REPO(ACTSUB REPOSUB) FROM
                       SUM-AMT-REPO(REPOSUB 1)
              ADD HTY-NO-REPO(ACTSUB REPOSUB) TO
                  SUM-NO-REPO(REPOSUB 2)
              ADD HTY-AMT-REPO(ACTSUB REPOSUB) TO
                  SUM-AMT-REPO(REPOSUB 2)
              SUBTRACT HTY-O2-REPO(ACTSUB REPOSUB) FROM
                       SUM-AMT-REPO(REPOSUB 2)
              IF ACTSUB = 1
                 ADD HTY-NO-REPO(ACTSUB REPOSUB) TO
                     SUM-NO-REPO(REPOSUB 3)
                 ADD HTY-AMT-REPO(ACTSUB REPOSUB) TO
                     SUM-AMT-REPO(REPOSUB 3)
                 SUBTRACT HTY-O2-REPO(ACTSUB REPOSUB) FROM
                          SUM-AMT-REPO(REPOSUB 3)
              ELSE
                 GO TO EXIT-SUM-FIELDS-4-TY-FIXED.
           IF PAGE4-O2
              ADD HTY-O2-REPO(ACTSUB REPOSUB) TO
                  SUM-AMT-REPO(REPOSUB 1)
              ADD HTY-O2-REPO(ACTSUB REPOSUB) TO
                  SUM-AMT-REPO(REPOSUB 2)
              IF ACTSUB = 1
                 ADD HTY-O2-REPO(ACTSUB REPOSUB) TO
                     SUM-AMT-REPO(REPOSUB 3)
              ELSE
                 GO TO EXIT-SUM-FIELDS-4-TY-FIXED.
           IF PAGE4-INT
              ADD HTY-INT-REPO(ACTSUB REPOSUB) TO
                  SUM-AMT-REPO(REPOSUB 1)
              ADD HTY-INT-REPO(ACTSUB REPOSUB) TO
                  SUM-AMT-REPO(REPOSUB 2)
              IF ACTSUB = 1
                 ADD HTY-INT-REPO(ACTSUB REPOSUB) TO
                     SUM-AMT-REPO(REPOSUB 3)
              ELSE
                 GO TO EXIT-SUM-FIELDS-4-TY-FIXED.

           GO TO EXIT-SUM-FIELDS-4-TY-FIXED.
       PAGE-4-DAILY.
              IF PAGE4-REC
                 ADD HTY-NO-REPO(ACTSUB REPOSUB) TO
                     SUM-NO-REPO(REPOSUB 1)
                 ADD HTY-AMT-REPO(ACTSUB REPOSUB) TO
                     SUM-AMT-REPO(REPOSUB 1)
                 GO TO EXIT-SUM-FIELDS-4-TY-FIXED.
              IF PAGE4-NO-O2
                 ADD HTY-NO-REPO(ACTSUB REPOSUB) TO
                     SUM-NO-REPO(REPOSUB 1)
                 ADD HTY-AMT-REPO(ACTSUB REPOSUB) TO
                     SUM-AMT-REPO(REPOSUB 1)
                 SUBTRACT HTY-O2-REPO(ACTSUB REPOSUB) FROM
                          SUM-AMT-REPO(REPOSUB 1)
                 GO TO EXIT-SUM-FIELDS-4-TY-FIXED.
              IF PAGE4-O2
                 ADD HTY-O2-REPO(ACTSUB REPOSUB) TO
                     SUM-AMT-REPO(REPOSUB 1)
                 GO TO EXIT-SUM-FIELDS-4-TY-FIXED.
              IF PAGE4-INT
                 ADD HTY-INT-REPO(ACTSUB REPOSUB) TO
                     SUM-AMT-REPO(REPOSUB 1)
                 GO TO EXIT-SUM-FIELDS-4-TY-FIXED.
       EXIT-SUM-FIELDS-4-TY-FIXED.
           EXIT.

      *******************************************
      *    SUM FIELDS FROM BRANCH ANALYSYS PAGE 5
      *******************************************
       SUM-FIELDS-5-BY SECTION.
           ADD HBY-INTREB(MTDYTD) TO SUM5(1 MTDYTD).
           ADD HBY-CLREB1(MTDYTD) TO SUM5(2 MTDYTD).
           ADD HBY-CLREB2(MTDYTD) TO SUM5(3 MTDYTD).
           ADD HBY-CLREB3(MTDYTD) TO SUM5(4 MTDYTD).
           ADD HBY-CLREB4(MTDYTD) TO SUM5(5 MTDYTD).
           ADD HBY-AHREB(MTDYTD)  TO SUM5(6 MTDYTD).
           ADD HBY-PPREB(MTDYTD)  TO SUM5(7 MTDYTD).
           ADD HBY-O1REB(MTDYTD)  TO SUM5(8 MTDYTD).
           ADD HBY-O2REB(MTDYTD)  TO SUM5(9 MTDYTD).
           ADD HBY-O3REB(MTDYTD)  TO SUM5(10 MTDYTD).
           ADD HBY-O4REB(MTDYTD)  TO SUM5(11 MTDYTD).
           ADD HBY-O5REB(MTDYTD)  TO SUM5(12 MTDYTD).
           ADD HBY-O5REB(MTDYTD)  TO SUM5(13 MTDYTD).
           ADD HBY-O7REB(MTDYTD)  TO SUM5(14 MTDYTD).
           ADD HBY-SCREB(MTDYTD)  TO SUM5(15 MTDYTD).
           ADD HBY-MFREB(MTDYTD)  TO SUM5(16 MTDYTD).
           ADD HBY-DFREB(MTDYTD)  TO SUM5(17 MTDYTD).
      *     ADD HBY-PART-CHGOFF(MTDYTD)  TO SUM5(18 MTDYTD).

       SUM-FIELDS-5-BY-EXIT.
           EXIT.

       SUM-FIELDS-5-TY SECTION.
           ADD HTY-INTREB TO SUM5(1 MTDYTD).
           ADD HTY-CLREB1 TO SUM5(2 MTDYTD).
           ADD HTY-CLREB2 TO SUM5(3 MTDYTD).
           ADD HTY-CLREB3 TO SUM5(4 MTDYTD).
           ADD HTY-CLREB4 TO SUM5(5 MTDYTD).
           ADD HTY-AHREB  TO SUM5(6 MTDYTD).
           ADD HTY-PPREB  TO SUM5(7 MTDYTD).
           ADD HTY-O1REB  TO SUM5(8 MTDYTD).
           ADD HTY-O2REB  TO SUM5(9 MTDYTD).
           ADD HTY-O3REB  TO SUM5(10 MTDYTD).
           ADD HTY-O4REB  TO SUM5(11 MTDYTD).
           ADD HTY-O5REB  TO SUM5(12 MTDYTD).
           ADD HTY-O6REB  TO SUM5(13 MTDYTD).
           ADD HTY-O7REB  TO SUM5(14 MTDYTD).
           ADD HTY-SCREB  TO SUM5(15 MTDYTD).
           ADD HTY-MFREB  TO SUM5(16 MTDYTD).
           ADD HTY-DFREB  TO SUM5(17 MTDYTD).
      *     ADD HTY-PART-CHGOFF  TO SUM5(18 MTDYTD).

       SUM-FIELDS-5-TY-EXIT.
           EXIT.


      *******************************************
      *    SUM FIELDS FROM BRANCH ANALYSYS PAGE 6
      *******************************************
       SUM-FIELDS-6-BY SECTION.
           ADD HBY-ALLOTMENTS(MTDYTD)      TO SUM6(1 MTDYTD).
           ADD HBY-LOCKBOX(MTDYTD)         TO SUM6(2 MTDYTD).
           ADD HBY-OTH-NO-COL(MTDYTD)      TO SUM6A(3 MTDYTD).
           ADD HBY-OT2-NO-COL(MTDYTD)      TO SUM6A(4 MTDYTD).
           ADD HBY-INT-NO-COL(MTDYTD)      TO SUM6A(5 MTDYTD).
           ADD HBY-LATE-NO-COL(MTDYTD)     TO SUM6A(6 MTDYTD).
           ADD HBY-OTH-NO-WAIVED(MTDYTD)   TO SUM6A(7 MTDYTD).
           ADD HBY-OT2-NO-WAIVED(MTDYTD)   TO SUM6A(8 MTDYTD).
           ADD HBY-INT-NO-WAIVED(MTDYTD)   TO SUM6A(9 MTDYTD).
           ADD HBY-LATE-NO-WAIVED(MTDYTD)  TO SUM6A(10 MTDYTD).
           ADD HBY-OTH-AMT-WAIVED(MTDYTD)  TO SUM6B(1 MTDYTD).
           ADD HBY-OT2-AMT-WAIVED(MTDYTD)  TO SUM6B(2 MTDYTD).
           ADD HBY-INT-AMT-WAIVED(MTDYTD)  TO SUM6B(3 MTDYTD).
           ADD HBY-LATE-AMT-WAIVED(MTDYTD) TO SUM6B(4 MTDYTD).

       SUM-FIELDS-6-BY-EXIT.
           EXIT.

       SUM-FIELDS-6-TY SECTION.
           ADD HTY-ALLOTMENTS      TO SUM6(1 MTDYTD).
           ADD HTY-LOCKBOX         TO SUM6(2 MTDYTD).
           ADD HTY-OTH-NO-COL      TO SUM6A(3 MTDYTD).
           ADD HTY-OT2-NO-COL      TO SUM6A(4 MTDYTD).
           ADD HTY-INT-NO-COL      TO SUM6A(5 MTDYTD).
           ADD HTY-LATE-NO-COL     TO SUM6A(6 MTDYTD).
           ADD HTY-OTH-NO-WAIVED   TO SUM6A(7 MTDYTD).
           ADD HTY-OT2-NO-WAIVED   TO SUM6A(8 MTDYTD).
           ADD HTY-INT-NO-WAIVED   TO SUM6A(9 MTDYTD).
           ADD HTY-LATE-NO-WAIVED  TO SUM6A(10 MTDYTD).
           ADD HTY-OTH-AMT-WAIVED  TO SUM6B(1 MTDYTD).
           ADD HTY-OT2-AMT-WAIVED  TO SUM6B(2 MTDYTD).
           ADD HTY-INT-AMT-WAIVED  TO SUM6B(3 MTDYTD).
           ADD HTY-LATE-AMT-WAIVED TO SUM6B(4 MTDYTD).

       SUM-FIELDS-5-TY-EXIT.
           EXIT.

      *****************************************************************
      *    SUM FIELDS BRANCH ANALYSIS PAGE 7
      *****************************************************************
      *  PERIOD:
      *    PERIOD TABLE: 1 - CURRENT           1 - MTD
      *                  2 - LAST MONTH        2 - YTD
      *                  3 - LAST YEAR         3 - LYR
      *
      *  ACTSUB:
      *    ACTIVITY TBL: 1 - NET GAIN / LOSS   2 - NEW VOLUME
      *
      *  PLSUB:
      *    TYPE TABLE  : 1 - PL ACTIVE   |    2 - PL INACTIVE
      *                  3 - P2 ACTIVE   |    4 - P2 INACTIVE
      *                  5 - P3 ACTIVE   |    6 - P3 INACTIVE
      *                  7 - P4 ACTIVE   |    8 - P4 INACTIVE
      *                  9 - P5 ACTIVE   |   10 - P5 INACTIVE
      *                 11 - TOTAL
      *****************************************************************
       SUM-FIELDS-7-BY SECTION.

           MOVE PLSUB TO SUB4 PAGE7-A-I-TYPE.
           IF (ACTSUB = 1 AND PERIOD = 1) OR ACTSUB = 2
                PERFORM SUM-FIELDS-7-BY-FIXED.
           IF ACTSUB = 1
              IF PERIOD = 2 OR 3
                 PERFORM SUM-FIELDS-7-BY-DIFF.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    THIS IS FOR ADDING TO PARTIAL CHARGEOFFS ONCE!!!
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( PERIOD = 1 ) AND ( PLSUB = 1 )
              PERFORM SUM-FIELDS-7-BY-PRT-CHGOFF.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       EXIT-SUM-FIELDS-7-BY.
           EXIT.

      *----------------------------------------------------------------
      * SUM FIELDS 7 BY FIXED SECTION:
      *-------------------------------
      * NO OF ACCOUNTS DISPLAYED ONLY WITH  PAGE7-REC
      *        AND PAGE7-NO-O2.
      *
      *  IF PAGE7-REC   - INCREMENT P&L WITH O2 ADDED IN.
      *  IF PAGE7-NO-O2 - INCREMENT P&L WITH O2 SUBTRACTED OUT.
      *  IF PAGE7-O2    - INCREMENT O2 AMOUNTS ONLY (NO RECEIVABLES).
      *  IF PAGE7-INT   - INCREMENT INTEREST CHARGES ONLY.
      *
      *  PAGE7-SEP-PL - SHOW SEPARATE ACTIVE AND INACTIVE TOTALS.
      *  PAGE7-TOT-PL - IF NET GAIN/LOSS THEN DISPLAY ACTIVE + INACTIVE.
      *                 IF NEW VOLUME THEN DISPLAY ACTIVE ONLY.
      *
      *  PAGE7-ACTIVE - ACTIVE P&L (BUCKETS 1 3 5 7 9).
      *  PAGE7-INACT  - INACTIVE P&L ( BUCKETS 2 4 6 8 10).
      *----------------------------------------------------------------
       SUM-FIELDS-7-BY-FIXED SECTION.
           IF PAGE7-REC
              IF PAGE7-SEP-PL OR (PAGE7-TOT-PL AND PAGE7-ACTIVE)
                 ADD HBY-NO-PL(PERIOD ACTSUB PLSUB)
                                         TO SUM-NO-PL(SUB4 PERIOD)
                 ADD HBY-AMT-PL(PERIOD ACTSUB PLSUB)
                                         TO SUM-AMT-PL(SUB4 PERIOD)
              ELSE
                 IF P7-NET-GAIN-LOSS AND PAGE7-TOT-PL AND PAGE7-INACT
                    ADD HBY-NO-PL(PERIOD ACTSUB PLSUB)
                               TO SUM-NO-PL(SUB4 - 1 PERIOD)
                    ADD HBY-AMT-PL(PERIOD ACTSUB PLSUB)
                               TO SUM-AMT-PL(SUB4 - 1 PERIOD)
                 END-IF
              END-IF
              GO TO EXIT-SUM-FIELDS-7-BY-FIXED.
      *----------------------------------------------------------------
           IF PAGE7-NO-O2
              IF PAGE7-SEP-PL OR (PAGE7-TOT-PL AND PAGE7-ACTIVE)
                 ADD HBY-NO-PL(PERIOD ACTSUB PLSUB)
                                  TO SUM-NO-PL(SUB4 PERIOD)
                 ADD HBY-AMT-PL(PERIOD ACTSUB PLSUB)
                                  TO SUM-AMT-PL(SUB4 PERIOD)
                 SUBTRACT HBY-O2-PL(PERIOD ACTSUB PLSUB)
                                  FROM SUM-AMT-PL(SUB4 PERIOD)
              ELSE
                 IF P7-NET-GAIN-LOSS AND PAGE7-TOT-PL AND PAGE7-INACT
                    ADD HBY-NO-PL(PERIOD ACTSUB PLSUB)
                                     TO SUM-NO-PL(SUB4 - 1 PERIOD)
                    ADD HBY-AMT-PL(PERIOD ACTSUB PLSUB)
                                     TO SUM-AMT-PL(SUB4 - 1 PERIOD)
                    SUBTRACT HBY-O2-PL(PERIOD ACTSUB PLSUB)
                                     FROM SUM-AMT-PL(SUB4 - 1 PERIOD)
                 END-IF
              END-IF
              GO TO EXIT-SUM-FIELDS-7-BY-FIXED.
      *----------------------------------------------------------------
           IF PAGE7-O2
              IF PAGE7-SEP-PL OR (PAGE7-TOT-PL AND PAGE7-ACTIVE)
                 MOVE 0 TO SUM-NO-PL(SUB4 PERIOD)
                 ADD HBY-O2-PL(PERIOD ACTSUB PLSUB)
                                       TO SUM-AMT-PL(SUB4 PERIOD)
              ELSE
                 IF P7-NET-GAIN-LOSS AND PAGE7-TOT-PL AND PAGE7-INACT
                    MOVE 0 TO SUM-NO-PL(SUB4 - 1  PERIOD)
                    ADD HBY-O2-PL(PERIOD ACTSUB PLSUB)
                                  TO SUM-AMT-PL(SUB4 - 1  PERIOD)
                 END-IF
              END-IF
              GO TO EXIT-SUM-FIELDS-7-BY-FIXED.
      *----------------------------------------------------------------
           IF PAGE7-INT
              IF PAGE7-SEP-PL OR (PAGE7-TOT-PL AND PAGE7-ACTIVE)
                 MOVE 0 TO SUM-NO-PL(SUB4 PERIOD)
                 ADD HBY-INT-PL(PERIOD ACTSUB PLSUB)
                                      TO SUM-AMT-PL(SUB4 PERIOD)
              ELSE
                 IF P7-NET-GAIN-LOSS AND PAGE7-TOT-PL AND PAGE7-INACT
                    MOVE 0 TO SUM-NO-PL(SUB4 - 1  PERIOD)
                    ADD HBY-INT-PL(PERIOD ACTSUB PLSUB)
                                  TO SUM-AMT-PL(SUB4 - 1  PERIOD)
                 END-IF
              END-IF
              GO TO EXIT-SUM-FIELDS-7-BY-FIXED.
       EXIT-SUM-FIELDS-7-BY-FIXED.
           EXIT.

      *----------------------------------------------------------------
      * SUM FIELDS 7 TY DIFF SECTION:
      *------------------------------
      * DISPLAYS DIFFERENCE BETWEEN CURRENT AND ELM, ELY.
      *    NO OF ACCOUNTS DISPLAYED ONLY WITH PAGE7-REC,
      *       AND PAGE7-NO-O2.
      *
      *  IF PAGE7-REC   - INCREMENT P&L WITH O2 ADDED IN.
      *                      (CUR) - (MTD)  [O2 INCLUDED]
      *  IF PAGE7-NO-O2 - INCREMENT P&L WITH O2 SUBTRACTED OUT.
      *                      (CUR - O2) - (MTD - O2)
      *  IF PAGE7-O2    - INCREMENT O2 AMOUNTS ONLY (NO RECEIVABLES).
      *                      (CUR O2) - (MTD O2)
      *  IF PAGE7-INT   - INCREMENT INTEREST CHARGES ONLY.
      *                      (CUR INT) - (MTD INT)
      *
      *  PAGE7-SEP-PL - SHOW SEPARATE ACTIVE AND INACTIVE TOTALS.
      *  PAGE7-TOT-PL - IF NET GAIN/LOSS THEN DISPLAY ACTIVE + INACTIVE.
      *                 IF NEW VOLUME THEN DISPLAY ACTIVE ONLY.
      *
      *  PAGE7-ACTIVE - ACTIVE P&L (BUCKETS 1 3 5 7 9).
      *  PAGE7-INACT  - INACTIVE P&L ( BUCKETS 2 4 6 8 10).
      *----------------------------------------------------------------
       SUM-FIELDS-7-BY-DIFF SECTION.
           IF PAGE7-REC
              IF PAGE7-SEP-PL OR (PAGE7-TOT-PL AND PAGE7-ACTIVE)
                 ADD HBY-NO-PL(1 ACTSUB PLSUB) TO
                     SUM-NO-PL(SUB4 PERIOD)
                 SUBTRACT HBY-NO-PL(PERIOD ACTSUB PLSUB) FROM
                          SUM-NO-PL(SUB4 PERIOD)
                 ADD HBY-AMT-PL(1 ACTSUB PLSUB) TO
                     SUM-AMT-PL(SUB4 PERIOD)
                 SUBTRACT HBY-AMT-PL(PERIOD ACTSUB PLSUB) FROM
                          SUM-AMT-PL(SUB4 PERIOD)
              ELSE
                 IF P7-NET-GAIN-LOSS AND PAGE7-TOT-PL AND PAGE7-INACT
                    ADD HBY-NO-PL(1 ACTSUB PLSUB) TO
                        SUM-NO-PL(SUB4 - 1 PERIOD)
                    SUBTRACT HBY-NO-PL(PERIOD ACTSUB PLSUB) FROM
                             SUM-NO-PL(SUB4 - 1 PERIOD)
                    ADD HBY-AMT-PL(1 ACTSUB PLSUB) TO
                        SUM-AMT-PL(SUB4 - 1 PERIOD)
                    SUBTRACT HBY-AMT-PL(PERIOD ACTSUB PLSUB) FROM
                             SUM-AMT-PL(SUB4 - 1 PERIOD)
                 END-IF
              END-IF
              GO TO EXIT-SUM-FIELDS-7-BY-DIFF.
      *----------------------------------------------------------------
           IF PAGE7-NO-O2
              IF PAGE7-SEP-PL OR (PAGE7-TOT-PL AND PAGE7-ACTIVE)
                 ADD HBY-NO-PL(1 ACTSUB PLSUB) TO
                     SUM-NO-PL(SUB4 PERIOD)
                 SUBTRACT HBY-NO-PL(PERIOD ACTSUB PLSUB) FROM
                          SUM-NO-PL(SUB4 PERIOD)
                 ADD HBY-AMT-PL(1 ACTSUB PLSUB) TO
                     SUM-AMT-PL(SUB4 PERIOD)
                 SUBTRACT HBY-O2-PL(1 ACTSUB PLSUB) FROM
                          SUM-AMT-PL(SUB4 PERIOD)
                 SUBTRACT HBY-AMT-PL(PERIOD ACTSUB PLSUB) FROM
                          SUM-AMT-PL(SUB4 PERIOD)
                 ADD HBY-O2-PL(PERIOD ACTSUB PLSUB) TO
                     SUM-AMT-PL(SUB4 PERIOD)
              ELSE
                 IF P7-NET-GAIN-LOSS AND PAGE7-TOT-PL AND PAGE7-INACT
                    ADD HBY-NO-PL(1 ACTSUB PLSUB) TO
                        SUM-NO-PL(SUB4 - 1 PERIOD)
                    SUBTRACT HBY-NO-PL(PERIOD ACTSUB PLSUB) FROM
                             SUM-NO-PL(SUB4 - 1 PERIOD)
                    ADD HBY-AMT-PL(1 ACTSUB PLSUB) TO
                        SUM-AMT-PL(SUB4 - 1 PERIOD)
                    SUBTRACT HBY-O2-PL(1 ACTSUB PLSUB) FROM
                             SUM-AMT-PL(SUB4 - 1 PERIOD)
                    SUBTRACT HBY-AMT-PL(PERIOD ACTSUB PLSUB) FROM
                             SUM-AMT-PL(SUB4 - 1 PERIOD)
                    ADD HBY-O2-PL(PERIOD ACTSUB PLSUB) TO
                        SUM-AMT-PL(SUB4 - 1 PERIOD)
                 END-IF
              END-IF
              GO TO EXIT-SUM-FIELDS-7-BY-DIFF.
      *----------------------------------------------------------------
           IF PAGE7-O2
              IF PAGE7-SEP-PL OR (PAGE7-TOT-PL AND PAGE7-ACTIVE)
                 MOVE 0 TO SUM-NO-PL(SUB4 PERIOD)
                 ADD HBY-O2-PL(1 ACTSUB PLSUB) TO
                     SUM-AMT-PL(SUB4 PERIOD)
                 SUBTRACT HBY-O2-PL(PERIOD ACTSUB PLSUB) FROM
                          SUM-AMT-PL(SUB4 PERIOD)
              ELSE
                 IF P7-NET-GAIN-LOSS AND PAGE7-TOT-PL AND PAGE7-INACT
                    MOVE 0 TO SUM-NO-PL(SUB4 - 1 PERIOD)
                    ADD HBY-O2-PL(1 ACTSUB PLSUB) TO
                        SUM-AMT-PL(SUB4 - 1 PERIOD)
                    SUBTRACT HBY-O2-PL(PERIOD ACTSUB PLSUB) FROM
                             SUM-AMT-PL(SUB4 - 1 PERIOD)
                 END-IF
              END-IF
              GO TO EXIT-SUM-FIELDS-7-BY-DIFF.
      *----------------------------------------------------------------
           IF PAGE7-INT
              IF PAGE7-SEP-PL OR (PAGE7-TOT-PL AND PAGE7-ACTIVE)
                 MOVE 0 TO SUM-NO-PL(SUB4 PERIOD)
                 ADD HBY-INT-PL(1 ACTSUB PLSUB) TO
                     SUM-AMT-PL(SUB4 PERIOD)
                 SUBTRACT HBY-INT-PL(PERIOD ACTSUB PLSUB) FROM
                          SUM-AMT-PL(SUB4 PERIOD)
              ELSE
                 IF P7-NET-GAIN-LOSS AND PAGE7-TOT-PL AND PAGE7-INACT
                    MOVE 0 TO SUM-NO-PL(SUB4 - 1 PERIOD)
                    ADD HBY-INT-PL(1 ACTSUB PLSUB) TO
                        SUM-AMT-PL(SUB4 - 1 PERIOD)
                    SUBTRACT HBY-INT-PL(PERIOD ACTSUB PLSUB) FROM
                             SUM-AMT-PL(SUB4 - 1 PERIOD)
                 END-IF
              END-IF
              GO TO EXIT-SUM-FIELDS-7-BY-DIFF.
       EXIT-SUM-FIELDS-7-BY-DIFF.
              EXIT.

      *=================================================================
      *
      * SUM FIELDS 7 BY PARTIAL CHARGEOFF SECTION:
      *-------------------------------------------
      *
      *    DISPLAYS PARTIAL CHARGEOFF FOR MTD & YTD.
      *
      *=================================================================
       SUM-FIELDS-7-BY-PRT-CHGOFF SECTION.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

           ADD HBY-PART-CHGOFF(1) TO SUM-PART-CHGOFF-MTD.
           ADD HBY-PART-CHGOFF(2) TO SUM-PART-CHGOFF-YTD.

      *-----------------------------------------------------------------
       EXIT-SUM-FIELDS-7-BY-P-COF.
           EXIT.

      *****************************************************************
      *    SUM FIELDS TODAYS TOTALS PAGE 7
      *****************************************************************
      *
      *  ACTSUB:
      *    ACTIVITY TBL: 1 - NET FIXED / LOSS   2 - NEW VOLUME
      *
      *  PLSUB:
      *    TYPE TABLE  : 1 - PL ACTIVE   |    2 - PL INACTIVE
      *                  3 - P2 ACTIVE   |    4 - P2 INACTIVE
      *                  5 - P3 ACTIVE   |    6 - P3 INACTIVE
      *                  7 - P4 ACTIVE   |    8 - P4 INACTIVE
      *                  9 - P5 ACTIVE   |   10 - P5 INACTIVE
      *****************************************************************
       SUM-FIELDS-7-TY SECTION.
           MOVE PLSUB TO SUB4 PAGE7-A-I-TYPE.
           IF ACTSUB = 1 OR ACTSUB = 2
              PERFORM SUM-FIELDS-7-TY-FIXED.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    THIS IS FOR ADDING TO PARTIAL CHARGEOFFS ONCE!!!
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( PLSUB = 1 )
              PERFORM SUM-FIELDS-7-TY-PRT-CHGOFF.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       EXIT-SUM-FIELDS-7-TY.
           EXIT.

      *----------------------------------------------------------------
      * SUM FIELDS 7 TY FIXED SECTION:
      *-------------------------------
      * DISPLAYS DIFFERENCE BETWEEN CURRENT AND ELM, ELY.
      *    NO OF ACCOUNTS DISPLAYED ONLY WITH PAGE7-REC,
      *         AND PAGE7-NO-O2.
      *
      *  IF PAGE7-TODAY - INCREMENT ONLY CURRENT TOTALS (SKIP MTD AND YTD).
      *  IF PAGE7-TOTAL - INCREMENT CUR, MTD, YTD.
      *
      *  IF PAGE7-REC   - INCREMENT P&L WITH O2 ADDED IN.
      *  IF PAGE7-NO-O2 - INCREMENT P&L WITH O2 SUBTRACTED OUT.
      *  IF PAGE7-O2    - INCREMENT O2 AMOUNTS ONLY (NO RECEIVABLES).
      *  IF PAGE7-INT   - INCREMENT INTEREST CHARGES ONLY.
      *
      *  PAGE7-SEP-PL - SHOW SEPARATE ACTIVE AND INACTIVE TOTALS.
      *  PAGE7-TOT-PL - IF NET GAIN/LOSS THEN DISPLAY ACTIVE + INACTIVE.
      *                 IF NEW VOLUME THEN DISPLAY ACTIVE ONLY.
      *
      *  PAGE7-ACTIVE - ACTIVE P&L (BUCKETS 1 3 5 7 9).
      *  PAGE7-INACT  - INACTIVE P&L ( BUCKETS 2 4 6 8 10).
      *----------------------------------------------------------------
       SUM-FIELDS-7-TY-FIXED SECTION.
           IF PAGE7-TODAY
              GO TO PAGE-7-DAILY.

           IF PAGE7-REC
              IF PAGE7-SEP-PL OR (PAGE7-TOT-PL AND PAGE7-ACTIVE)
                 ADD HTY-NO-PL(ACTSUB PLSUB) TO SUM-NO-PL(SUB4 1)
                 ADD HTY-AMT-PL(ACTSUB PLSUB) TO SUM-AMT-PL(SUB4 1)
                 ADD HTY-NO-PL(ACTSUB PLSUB) TO SUM-NO-PL(SUB4 2)
                 ADD HTY-AMT-PL(ACTSUB PLSUB) TO SUM-AMT-PL(SUB4 2)
                 IF ACTSUB = 1
                    ADD HTY-NO-PL(ACTSUB PLSUB) TO SUM-NO-PL(SUB4 3)
                    ADD HTY-AMT-PL(ACTSUB PLSUB) TO SUM-AMT-PL(SUB4 3)
                 END-IF
              ELSE
                 IF P7-NET-GAIN-LOSS AND PAGE7-TOT-PL AND PAGE7-INACT
                    ADD HTY-NO-PL(ACTSUB PLSUB) TO
                        SUM-NO-PL(SUB4 - 1 1)
                    ADD HTY-AMT-PL(ACTSUB PLSUB) TO
                        SUM-AMT-PL(SUB4 - 1 1)
                    ADD HTY-NO-PL(ACTSUB PLSUB) TO
                        SUM-NO-PL(SUB4 - 1 2)
                    ADD HTY-AMT-PL(ACTSUB PLSUB) TO
                        SUM-AMT-PL(SUB4 - 1 2)
                    IF ACTSUB = 1
                       ADD HTY-NO-PL(ACTSUB PLSUB) TO
                           SUM-NO-PL(SUB4 - 1 3)
                       ADD HTY-AMT-PL(ACTSUB PLSUB) TO
                           SUM-AMT-PL(SUB4 - 1 3)
                    END-IF
                 END-IF
              END-IF
              GO TO EXIT-SUM-FIELDS-7-TY-FIXED.
      *----------------------------------------------------------------
           IF PAGE7-NO-O2
              IF PAGE7-SEP-PL OR ( PAGE7-TOT-PL AND PAGE7-ACTIVE)
                 ADD HTY-NO-PL(ACTSUB PLSUB) TO SUM-NO-PL(SUB4 1)
                 ADD HTY-AMT-PL(ACTSUB PLSUB) TO SUM-AMT-PL(SUB4 1)
                 SUBTRACT HTY-O2-PL(ACTSUB PLSUB)
                                       FROM SUM-AMT-PL(SUB4 1)
                 ADD HTY-NO-PL(ACTSUB PLSUB) TO SUM-NO-PL(SUB4 2)
                 ADD HTY-AMT-PL(ACTSUB PLSUB) TO SUM-AMT-PL(SUB4 2)
                 SUBTRACT HTY-O2-PL(ACTSUB PLSUB)
                                       FROM SUM-AMT-PL(SUB4 2)
                 IF ACTSUB = 1
                    ADD HTY-NO-PL(ACTSUB PLSUB) TO SUM-NO-PL(SUB4 3)
                    ADD HTY-AMT-PL(ACTSUB PLSUB) TO SUM-AMT-PL(SUB4 3)
                    SUBTRACT HTY-O2-PL(ACTSUB PLSUB)
                                          FROM SUM-AMT-PL(SUB4 3)
                 END-IF
              ELSE
                 IF P7-NET-GAIN-LOSS AND PAGE7-TOT-PL AND PAGE7-INACT
                    ADD HTY-NO-PL(ACTSUB PLSUB) TO
                        SUM-NO-PL(SUB4 - 1 1)
                    ADD HTY-AMT-PL(ACTSUB PLSUB) TO
                        SUM-AMT-PL(SUB4 - 1 1)
                    SUBTRACT HTY-O2-PL(ACTSUB PLSUB) FROM
                             SUM-AMT-PL(SUB4 - 1 1)
                    ADD HTY-NO-PL(ACTSUB PLSUB) TO
                        SUM-NO-PL(SUB4 - 1 2)
                    ADD HTY-AMT-PL(ACTSUB PLSUB) TO
                        SUM-AMT-PL(SUB4 - 1 2)
                    SUBTRACT HTY-O2-PL(ACTSUB PLSUB) FROM
                             SUM-AMT-PL(SUB4 - 1 2)
                    IF ACTSUB = 1
                       ADD HTY-NO-PL(ACTSUB PLSUB) TO
                           SUM-NO-PL(SUB4 - 1 3)
                       ADD HTY-AMT-PL(ACTSUB PLSUB) TO
                           SUM-AMT-PL(SUB4 - 1 3)
                       SUBTRACT HTY-O2-PL(ACTSUB PLSUB) FROM
                                SUM-AMT-PL(SUB4 - 1 3)
                    END-IF
                 END-IF
              END-IF
              GO TO EXIT-SUM-FIELDS-7-TY-FIXED.
      *----------------------------------------------------------------
           IF PAGE7-O2
              IF PAGE7-SEP-PL OR (PAGE7-TOT-PL AND PAGE7-ACTIVE)
                 ADD HTY-O2-PL(ACTSUB PLSUB) TO SUM-AMT-PL(SUB4 1)
                 ADD HTY-O2-PL(ACTSUB PLSUB) TO SUM-AMT-PL(SUB4 2)
                 IF ACTSUB = 1
                    ADD HTY-O2-PL(ACTSUB PLSUB) TO SUM-AMT-PL(SUB4 3)
                 END-IF
              ELSE
                 IF P7-NET-GAIN-LOSS AND PAGE7-TOT-PL AND PAGE7-INACT
                    ADD HTY-O2-PL(ACTSUB PLSUB) TO
                        SUM-AMT-PL(SUB4 - 1 1)
                    ADD HTY-O2-PL(ACTSUB PLSUB) TO
                        SUM-AMT-PL(SUB4 - 1 2)
                    IF ACTSUB = 1
                       ADD HTY-O2-PL(ACTSUB PLSUB) TO
                           SUM-AMT-PL(SUB4 - 1 3)
                    END-IF
                 END-IF
              END-IF
              GO TO EXIT-SUM-FIELDS-7-TY-FIXED.
      *----------------------------------------------------------------
           IF PAGE7-INT
              IF PAGE7-SEP-PL OR (PAGE7-TOT-PL AND PAGE7-ACTIVE)
                 ADD HTY-INT-PL(ACTSUB PLSUB) TO SUM-AMT-PL(SUB4 1)
                 ADD HTY-INT-PL(ACTSUB PLSUB) TO SUM-AMT-PL(SUB4 2)
                 IF ACTSUB = 1
                    ADD HTY-INT-PL(ACTSUB PLSUB) TO SUM-AMT-PL(SUB4 3)
                 END-IF
              ELSE
                 IF P7-NET-GAIN-LOSS AND PAGE7-TOT-PL AND PAGE7-INACT
                    ADD HTY-INT-PL(ACTSUB PLSUB)
                                      TO SUM-AMT-PL(SUB4 - 1 1)
                    ADD HTY-INT-PL(ACTSUB PLSUB)
                                      TO SUM-AMT-PL(SUB4 - 1 2)
                    IF ACTSUB = 1
                       ADD HTY-INT-PL(ACTSUB PLSUB)
                                      TO SUM-AMT-PL(SUB4 - 1 3)
                    END-IF
                 END-IF
              END-IF
              GO TO EXIT-SUM-FIELDS-7-TY-FIXED.

      *----------------------------------------------------------------
       PAGE-7-DAILY.
           IF PAGE7-REC
              IF PAGE7-SEP-PL OR (PAGE7-TOT-PL AND PAGE7-ACTIVE)
                 ADD HTY-NO-PL(ACTSUB PLSUB) TO SUM-NO-PL(SUB4 1)
                 ADD HTY-AMT-PL(ACTSUB PLSUB) TO SUM-AMT-PL(SUB4 1)
              ELSE
                 IF P7-NET-GAIN-LOSS AND PAGE7-TOT-PL AND PAGE7-INACT
                    ADD HTY-NO-PL(ACTSUB PLSUB) TO
                        SUM-NO-PL(SUB4 - 1 1)
                    ADD HTY-AMT-PL(ACTSUB PLSUB) TO
                        SUM-AMT-PL(SUB4 - 1 1)
                 END-IF
              END-IF
              GO TO EXIT-SUM-FIELDS-7-TY-FIXED.
      *----------------------------------------------------------------
           IF PAGE7-NO-O2
              IF PAGE7-SEP-PL OR ( PAGE7-TOT-PL AND PAGE7-ACTIVE)
                 ADD HTY-NO-PL(ACTSUB PLSUB) TO SUM-NO-PL(SUB4 1)
                 ADD HTY-AMT-PL(ACTSUB PLSUB) TO SUM-AMT-PL(SUB4 1)
                 SUBTRACT HTY-O2-PL(ACTSUB PLSUB)
                                       FROM SUM-AMT-PL(SUB4 1)
              ELSE
                 IF P7-NET-GAIN-LOSS AND PAGE7-TOT-PL AND PAGE7-INACT
                    ADD HTY-NO-PL(ACTSUB PLSUB) TO
                        SUM-NO-PL(SUB4 - 1 1)
                    ADD HTY-AMT-PL(ACTSUB PLSUB) TO
                        SUM-AMT-PL(SUB4 - 1 1)
                    SUBTRACT HTY-O2-PL(ACTSUB PLSUB)
                                          FROM SUM-AMT-PL(SUB4 - 1 1)
                 END-IF
              END-IF
              GO TO EXIT-SUM-FIELDS-7-TY-FIXED.
      *----------------------------------------------------------------
           IF PAGE7-O2
              IF PAGE7-SEP-PL OR (PAGE7-TOT-PL AND PAGE7-ACTIVE)
                 ADD HTY-O2-PL(ACTSUB PLSUB) TO SUM-AMT-PL(SUB4 1)
              ELSE
                 IF P7-NET-GAIN-LOSS AND PAGE7-TOT-PL AND PAGE7-INACT
                    ADD HTY-O2-PL(ACTSUB PLSUB) TO
                        SUM-AMT-PL(SUB4 -  1 1)
                 END-IF
              END-IF
              GO TO EXIT-SUM-FIELDS-7-TY-FIXED.
      *----------------------------------------------------------------
           IF PAGE7-INT
              IF PAGE7-SEP-PL OR (PAGE7-TOT-PL AND PAGE7-ACTIVE)
                 ADD HTY-INT-PL(ACTSUB PLSUB) TO SUM-AMT-PL(SUB4 1)
              ELSE
                 IF P7-NET-GAIN-LOSS AND PAGE7-TOT-PL AND PAGE7-INACT
                    ADD HTY-INT-PL(ACTSUB PLSUB) TO
                        SUM-AMT-PL(SUB4 - 1 1)
                 END-IF
              END-IF
              GO TO EXIT-SUM-FIELDS-7-TY-FIXED.
       EXIT-SUM-FIELDS-7-TY-FIXED.
           EXIT.

      *=================================================================
      *
      * SUM FIELDS 7 TY PARTIAL CHARGEOFF SECTION:
      *-------------------------------------------
      *
      *    DISPLAYS PARTIAL CHARGEOFF FOR MTD & YTD.
      *
      *=================================================================
       SUM-FIELDS-7-TY-PRT-CHGOFF SECTION.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

           ADD HTY-PART-CHGOFF TO SUM-PART-CHGOFF-MTD.
           ADD HTY-PART-CHGOFF TO SUM-PART-CHGOFF-YTD.

      *-----------------------------------------------------------------
       EXIT-SUM-FIELDS-7-TY-P-COF.
           EXIT.

      *****************************************************************
      *    SUM FIELDS PAGE 8
      *  MTDYTD: ACTIVITY MTD / YTD
      *  PLSUB : PL TYPE 1 - PL
      *               2 - P2
      *               3 - P3
      *               4 - P4
      *               5 - P5
      *****************************************************************
       SUM-FIELDS-8-BY SECTION.
           ADD HBY-PL-PRINCOL(MTDYTD PLSUB) TO SUM-PL-PRINCOL(PLSUB).
           ADD HBY-PL-INTCOL(MTDYTD PLSUB)  TO SUM-PL-INTCOL(PLSUB).
           ADD HBY-PL-LATECOL(MTDYTD PLSUB) TO SUM-PL-LATECOL(PLSUB).
           ADD HBY-PL-O2COL(MTDYTD PLSUB)   TO SUM-PL-O2COL(PLSUB).

       SUM-FIELDS-8-BY-EXIT.
           EXIT.

       SUM-FIELDS-8-TY SECTION.
           ADD HTY-PL-PRINCOL(PLSUB) TO SUM-PL-PRINCOL(PLSUB).
           ADD HTY-PL-INTCOL(PLSUB)  TO SUM-PL-INTCOL(PLSUB).
           ADD HTY-PL-LATECOL(PLSUB) TO SUM-PL-LATECOL(PLSUB).
           ADD HTY-PL-O2COL(PLSUB)   TO SUM-PL-O2COL(PLSUB).
       SUM-FIELDS-8-TY-EXIT.
           EXIT.

      *****************************************************************
      *    SUM FIELDS BY-REC PAGE 9
      *
      *  IF (CURRENT AND NET GAIN/LOSS) OR (NEW VOLUME)
      *       PERFORM SUM FIELDS FIXED.
      *  IF (NET GAIN/LOSS) AND (MTD OR LYR)
      *       PERFORM SUM FIELDS DIFF (SHOW DIFFERENCE FROM CURRENT)
      *
      *  SUB1 = PERIOD
      *            1 -  CUR/MTD
      *            2 -  ELM/YTD
      *            3 -  ELY/LYR
      *  SUB2 = ACTIVITY
      *            1 - NET GAIN/LOSS
      *            2 - VOLUME
      *  SUB3 = RESERVE #
      *            VARIES FROM 1-3
      *****************************************************************
       SUM-FIELDS-9-BY SECTION.
           IF (ACTSUB = 1 AND PERIOD = 1) OR ACTSUB = 2
              PERFORM SUM-FIELDS-9-BY-FIXED.
           IF ACTSUB = 1
              IF PERIOD = 2 OR 3
                 PERFORM SUM-FIELDS-9-BY-DIFF.
       EXIT-SUM-FIELDS-9-BY.
           EXIT.

       SUM-FIELDS-9-BY-FIXED SECTION.
      * DO THIS IF (CURREN TNET GAIN) OR VOLUME
           ADD HBY-DLRVPRIN(PERIOD ACTSUB DLRV) TO
               SUM-DLRVPRIN(DLRV PERIOD)
               SUM-DLTOTAL(PERIOD).
           ADD HBY-DLRVCHGS(PERIOD ACTSUB DLRV) TO
               SUM-DLRVCHGS(DLRV PERIOD)
               SUM-DLTOTAL(PERIOD).
           IF ACTSUB = 2 AND DLRV = 1
      *       ONLY UPDATE DLDISC VOLUME
              ADD HBY-DLDISC(PERIOD) TO SUM-DLDISC(PERIOD).

           IF DLRV = 1
              ADD HBY-DLPARVHELD(PERIOD ACTSUB) TO
                  SUM-DLPARVHELD(PERIOD)
                  SUM-DLTOTAL(PERIOD).
       EXIT-SUM-FIELDS-9-BY-FIXED.
           EXIT.

       SUM-FIELDS-9-BY-DIFF SECTION.
      * DO THIS IF NET GAIN AND (ELM OR ELY)
           ADD HBY-DLRVPRIN(1 ACTSUB DLRV) TO SUM-DLRVPRIN(DLRV PERIOD)
                                              SUM-DLTOTAL(PERIOD).
           SUBTRACT HBY-DLRVPRIN(PERIOD ACTSUB DLRV) FROM
                                              SUM-DLRVPRIN(DLRV PERIOD)
                                              SUM-DLTOTAL(PERIOD).
           ADD HBY-DLRVCHGS(1 ACTSUB DLRV) TO SUM-DLRVCHGS(DLRV PERIOD)
                                              SUM-DLTOTAL(PERIOD).
           SUBTRACT HBY-DLRVCHGS(PERIOD ACTSUB DLRV) FROM
                                              SUM-DLRVCHGS(DLRV PERIOD)
                                              SUM-DLTOTAL(PERIOD).
           IF DLRV = 1
              ADD HBY-DLPARVHELD(1 ACTSUB) TO SUM-DLPARVHELD(PERIOD)
                                              SUM-DLTOTAL(PERIOD)
              SUBTRACT HBY-DLPARVHELD(PERIOD ACTSUB) FROM
                                              SUM-DLPARVHELD(PERIOD)
                                              SUM-DLTOTAL(PERIOD).
       EXIT-SUM-FIELDS-9-BY-DIFF.
           EXIT.

      *****************************************************************
      *    SUM FIELDS TY-REC PAGE 9
      *
      *  IF ( NET GAIN/LOSS) OR (NEW VOLUME)
      *       PERFORM SUM FIELDS TY FIXED.
      *****************************************************************
       SUM-FIELDS-9-TY SECTION.
           IF ACTSUB = 1 OR ACTSUB = 2
              PERFORM SUM-FIELDS-9-TY-FIXED.

       EXIT-SUM-FIELDS-9-TY-FIXED.
           EXIT.

       SUM-FIELDS-9-TY-FIXED SECTION.
           IF PAGE9-TODAY
              GO TO PAGE-9-DAILY.
           ADD HTY-DLRVPRIN(ACTSUB DLRV)    TO SUM-DLRVPRIN(DLRV 1)
                                               SUM-DLTOTAL(1).
           ADD HTY-DLRVCHGS(ACTSUB DLRV)    TO SUM-DLRVCHGS(DLRV 1)
                                               SUM-DLTOTAL(1).
           ADD HTY-DLRVPRIN(ACTSUB DLRV)    TO SUM-DLRVPRIN(DLRV 2)
                                               SUM-DLTOTAL(2).
           ADD HTY-DLRVCHGS(ACTSUB DLRV)    TO SUM-DLRVCHGS(DLRV 2)
                                               SUM-DLTOTAL(2).
           IF DLRV = 1
              ADD HTY-DLPARVHELD(ACTSUB)    TO SUM-DLPARVHELD(1)
                                               SUM-DLTOTAL(1)
              ADD HTY-DLPARVHELD(ACTSUB)    TO SUM-DLPARVHELD(2)
                                               SUM-DLTOTAL(2)
              IF ACTSUB = 2
      *       VOLUME
              ADD HTY-DLDISC                TO SUM-DLDISC(1)
                                               SUM-DLDISC(2).
           IF ACTSUB = 1
      *       NET GAIN
              ADD HTY-DLRVPRIN(ACTSUB DLRV) TO SUM-DLRVPRIN(DLRV 3)
                                               SUM-DLTOTAL(3)
              ADD HTY-DLRVCHGS(ACTSUB DLRV) TO SUM-DLRVCHGS(DLRV 3)
                                               SUM-DLTOTAL(3)
              IF DLRV = 1
                 ADD HTY-DLPARVHELD(ACTSUB) TO SUM-DLPARVHELD(3)
                                               SUM-DLTOTAL(3).
           GO TO EXIT-SUM-FIELDS-9-TY.

       PAGE-9-DAILY.
           ADD HTY-DLRVPRIN(ACTSUB DLRV)    TO SUM-DLRVPRIN(DLRV 1)
                                               SUM-DLTOTAL(1).
           ADD HTY-DLRVCHGS(ACTSUB DLRV)    TO SUM-DLRVCHGS(DLRV 1)
                                               SUM-DLTOTAL(1).
           IF DLRV = 1
              ADD HTY-DLPARVHELD(ACTSUB)    TO SUM-DLPARVHELD(1)
                                               SUM-DLTOTAL(1)
              IF ACTSUB = 2
                 ADD HTY-DLDISC             TO SUM-DLDISC(1).

       EXIT-SUM-FIELDS-9-TY.
           EXIT.


      ********************************************************
      *    SUM FIELDS BRANCH ANALYSIS PAGE 10 
      ********************************************************
      *  PERIOD:
      *    PERIOD TABLE: 1 - MTD
      *                  2 - YTD
      *
      *  TYPSUB:
      *    TYPE TABLE  : 1 - CL     6 - O3
      *                  2 - AH     7 - O4
      *                  3 - PP     8 - O5
      *                  4 - O1     9 - AD&D
      *                  5 - 02    10 - ROAD GUARD
      ********************************************************
       SUM-FIELDS-10-BY SECTION. 
           IF TYPSUB < 11
              ADD HBY-INS-NO-ELIG(ACTSUB TYPSUB) TO
                             SUM-INS-NO-ELIG(TYPSUB)
                             SUM-INS-NO-ELIG(14)
              ADD HBY-INS-NO-WROTE(ACTSUB TYPSUB) TO
                             SUM-INS-NO-WROTE(TYPSUB)
                             SUM-INS-NO-WROTE(14)
              ADD HBY-INS-NO-NOTWROTE(ACTSUB TYPSUB) TO
                             SUM-INS-NO-NOTWROTE(TYPSUB)
                             SUM-INS-NO-NOTWROTE(14)
              ADD HBY-INS-AMT-WROTE(ACTSUB TYPSUB) TO
                             SUM-INS-AMT-WROTE(TYPSUB)
                             SUM-INS-AMT-WROTE(14)
           ELSE
              ADD HBY-FEE-NO-ELIG(ACTSUB TYPSUB - 10) TO
                             SUM-INS-NO-ELIG(TYPSUB)
                             SUM-INS-NO-ELIG(14)
              ADD HBY-FEE-NO-WROTE(ACTSUB TYPSUB - 10) TO
                             SUM-INS-NO-WROTE(TYPSUB)
                             SUM-INS-NO-WROTE(14)
              ADD HBY-FEE-NO-NOTWROTE(ACTSUB TYPSUB - 10) TO
                             SUM-INS-NO-NOTWROTE(TYPSUB)
                             SUM-INS-NO-NOTWROTE(14)
              ADD HBY-FEE-AMT-WROTE(ACTSUB TYPSUB - 10) TO
                             SUM-INS-AMT-WROTE(TYPSUB)
                             SUM-INS-AMT-WROTE(14).

       SUM-FIELDS-10-BY-EXIT.
           EXIT.


      ********************************************************
      *    SUM FIELDS TODAYS TOTALS PAGE 11
      ********************************************************
      *
      *  TYPSUB:
      *    TYPE TABLE  : 1 - CL     6 - O3
      *                  2 - AH     7 - O4
      *                  3 - PP     8 - O5
      *                  4 - O1     9 - AD&D
      *                  5 - 02    10 - ROAD
      ********************************************************
       SUM-FIELDS-10-TY SECTION.
           ADD HTY-INS-NO-ELIG(TYPSUB) TO SUM-INS-NO-ELIG(TYPSUB)
                                          SUM-INS-NO-ELIG(11).

           ADD HTY-INS-NO-WROTE(TYPSUB) TO SUM-INS-NO-WROTE(TYPSUB)
                                           SUM-INS-NO-WROTE(11).

           ADD HTY-INS-NO-NOTWROTE(TYPSUB) TO
                                         SUM-INS-NO-NOTWROTE(TYPSUB)
                                         SUM-INS-NO-NOTWROTE(11).

           ADD HTY-INS-AMT-WROTE(TYPSUB) TO SUM-INS-AMT-WROTE(TYPSUB)
                                            SUM-INS-AMT-WROTE(11).
       EXIT-SUM-FIELDS-10-TY.
           EXIT.

      ********************************************************
      *    SUM FIELDS PAGE 11 - BRANCH TOTALS
      ********************************************************
      *    PERIOD  :      CUR | MTD
      *                   ELM | YTD
      *                   ELY | LYR
      *
      *    ACTIVITY: NET GAIN | VOLUME
      *
      *    TYPE    :  1. INTEREST
      *               2. ADDON
      *               3. SERVICE CHARGE
      *               4. MAINTENANCE FEE
      *               5. DEALER DISCOUNT
      *               6. OTHER
      ********************************************************
       SUM-FIELDS-11-BY SECTION.
           PERFORM VARYING PERIOD FROM 1 BY 1 UNTIL PERIOD > 3
                     AFTER ACTSUB FROM 1 BY 1 UNTIL ACTSUB > 2
                     AFTER TYPSUB FROM 1 BY 1 UNTIL TYPSUB > 10
              IF (PERIOD = 1 AND ACTSUB = 1) OR ACTSUB = 2
                 ADD HBY-UNEARNED-AMT(PERIOD ACTSUB TYPSUB) TO
                     SUM-UNEARNED-AMT(PERIOD ACTSUB TYPSUB)
              END-IF
              IF ACTSUB = 1 AND (PERIOD = 2 OR 3)
                 ADD HBY-UNEARNED-AMT(1 ACTSUB TYPSUB) TO
                     SUM-UNEARNED-AMT(PERIOD ACTSUB TYPSUB)
                 SUBTRACT HBY-UNEARNED-AMT(PERIOD ACTSUB TYPSUB) FROM
                          SUM-UNEARNED-AMT(PERIOD ACTSUB TYPSUB)
              END-IF
           END-PERFORM.

       EXIT-SUM-FIELDS-11-BY.
           EXIT.

      ********************************************************
      *    SUM FIELDS PAGE 11 - TODAYS TOTALS 
      ********************************************************
      *    ACTIVITY: NET GAIN | VOLUME
      *
      *    TYPE    :  1. INTEREST
      *               2. ADDON
      *               3. SERVICE CHARGE
      *               4. MAINTENANCE FEE
      *               5. DEALER DISCOUNT
      *               6. OTHER
      ********************************************************
       SUM-FIELDS-11-TY SECTION.
           PERFORM VARYING ACTSUB FROM 1 BY 1 UNTIL ACTSUB > 2
                     AFTER TYPSUB FROM 1 BY 1 UNTIL TYPSUB > 10
              ADD HTY-UNEARNED-AMT(ACTSUB TYPSUB) TO
                         SUM-UNEARNED-AMT(1 ACTSUB TYPSUB)
              IF PAGE11-TOTAL
                 ADD HTY-UNEARNED-AMT(ACTSUB TYPSUB) TO
                     SUM-UNEARNED-AMT(2 ACTSUB TYPSUB)
                 IF ACTSUB = 1
                    ADD HTY-UNEARNED-AMT(ACTSUB TYPSUB) TO
                        SUM-UNEARNED-AMT(3 ACTSUB TYPSUB)
                 END-IF
              END-IF
           END-PERFORM.

       EXIT-SUM-FIELDS-11-TY.
           EXIT.

      *************************************
      *    GET FIRST OPEN DATE FOR BRANCH
      *************************************
       GET-FIRST-OPEN-DATE SECTION.
           MOVE 0 TO FIRST-OPEN-DATE.

           PERFORM OPEN-RC2-FILE.

           MOVE "A1"     TO RC-STATUS
                            QRC2-WBEG-STATUS
                            QRC2-WEND-STATUS.
           MOVE BR-NO    TO RC-BRNO
                            QRC2-WBEG-BRNO
                            QRC2-WEND-BRNO.
           
           MOVE 19000101 TO RC-TRANS-DATE SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD  TO QRC2-WBEG-TRANS-DATE.

           MOVE EXT-JULIAN-END       TO SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD  TO QRC2-WEND-TRANS-DATE.

           PERFORM START-RC2-FILE.
           PERFORM READ-RC2-FILE-NEXT.
           IF IO-FG = 0 AND RC-BRNO = BR-NO
              IF RC-STATUS = "A1"
                 MOVE RC-TRANS-DATE TO FIRST-OPEN-DATE.

           PERFORM CLOSE-RC2-FILE.
       EXIT-GET-FIRST-OPEN.
           EXIT.

      ********************************************************
      * GET FIRST OPEN DATE FOR BRANCH IN /USR/PFSI (WORLD)
      * NOTE, DON'T HAVE RC TO FIND FIRST OPEN DATE SO
      * WE WILL GET IT HERE
      ********************************************************
      *OTHER-MACHINE-GET-FIRST-OPEN SECTION.
      *    MOVE 0 TO FIRST-OPEN-DATE.

      *    PERFORM OPEN-TY1-FILE.
      *    MOVE BR-NO TO TY-BRNO.
      *    MOVE 0     TO TY-DATE TY-CLASS.
      *    PERFORM START-TY1-FILE.
      *    PERFORM READ-TY1-FILE-NEXT.
      *    IF IO-GOOD
      *       IF TY-BRNO = BR-NO
      *          MOVE TY-DATE TO FIRST-OPEN-DATE.

      *    PERFORM CLOSE-TY1-FILE.
      *EXIT-OTHER-MACH-GET-FIRST-OPEN.
      *    EXIT.

      *************************************
      *    GET TODAYS TOTALS
      *************************************
       GET-TODAYS-TOTALS SECTION.

           IF TRAP-NO-TY-FG = "Y"
              PERFORM CLEAR-TY-FILE
              GO TO GET-TODAYS-TOTALS-EXIT.

           MOVE "B"                     TO TY-PATH-OVERRIDE.

           IF TRAP-A-DATE-TY-FG = "Y"
              MOVE BY-BRNO      TO TY-BRNO
              MOVE BY-CLASS     TO TY-CLASS
              MOVE TY-TRAP-DATE TO DATE-MMDDYY
              PERFORM CONVERT-MMDDYY-TO-YYYYMMDD
              MOVE DATE-YYYYMMDD TO TY-DATE
              PERFORM OPEN-TY1-FILE
              PERFORM READ-TY1-FILE
              PERFORM CLOSE-TY1-FILE
              IF IO-BAD
                 PERFORM CLEAR-TY-FILE
                 GO TO GET-TODAYS-TOTALS-EXIT
              ELSE
                 PERFORM SEND-BR-NAME
                 GO TO GET-TODAYS-TOTALS-EXIT.

           IF FIRST-OPEN-DATE = 0
              PERFORM CLEAR-TY-FILE
              GO TO GET-TODAYS-TOTALS-EXIT.

           PERFORM OPEN-TY1-FILE.
           MOVE BY-BRNO         TO TY-BRNO.
           MOVE FIRST-OPEN-DATE TO TY-DATE.
           MOVE BY-CLASS        TO TY-CLASS.
           PERFORM READ-TY1-FILE.
           IF IO-BAD OR TY-BRFG = "Y"
              PERFORM CLEAR-TY-FILE.

           PERFORM CLOSE-TY1-FILE.

       GET-TODAYS-TOTALS-EXIT.
           EXIT.

      *************************************
      *    ENTER PAGE SELECTION
      *************************************
       ENTRY-MODULE SECTION.
       ENTER-SELECTION.

           MOVE "R RA060SEL." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              MOVE "N" TO READ-ONCE-FG
              GO TO END-ENTRY-MODULE.

      *******************************************************
      *   TEST FOR SET UP OF TRAP FOR ANY DAY'S TY-REC:
      *
      *******************************************************
      *   - TO ACTIVATE TRAP FOR ANY DAY'S TY-REC;
      *         ONLY ALLOWED IN REAL TIME MODE!!!!
      *           1. PRESS F9
      *           2. ENTER 3535 (HIT RETURN)
      *           3. ENTER TODAYS TOTAL DATE IN QUESTION
      *   - TO CHANGE TRAP DATE;
      *         WHILE TRAP IS ACTIVATED AND IN TODAYS MODE
      *           1. PRESS F9
      *           2. ENTER NEW TRAP DATE
      *   - TO INCREMENT TRAP DATE;
      *         WHILE TRAP IS ACTIVATED AND IN TODAYS MODE
      *           1. PRESS F9
      *           2. PRESS F9 TO INCREMENT TRAP DATE BY 1 DAY
      *   - TO STOP TRAP;
      *         WHILE IN TODAYS MODE, PAGE 3 ONLY,
      *           1. PRESS F9
      *           3. ENTER 0 (HIT RETURN)
      *
      *******************************************************

      *******************************************************
      *   TEST FOR SET UP OF TRAP FOR SKIPPING TODAYS TY-REC:
      *
      *******************************************************
      *   - TO ACTIVATE SKIP TODAYS TOTALS TRAP;
      *         ONLY ALLOWED IN REAL TIME MODE!!!!
      *           1. PRESS F9
      *           2. ENTER 3636 (HIT RETURN)
      *           3. TYPE 'SKIP'
      *   - TO STOP SKIP TODAYS TOTALS TRAP;
      *           1. PRESS F9
      *           2. ENTER 3636 (HIT RETURN)
      *           3. TYPE 'SKIP'
      *******************************************************
      *    IF GROUP-FLAG NOT = "Y"
      *JTG    IF DELINQ = 4
              IF READ-EVERY
                 IF VDUSTATUS = "1("
                    PERFORM ALARM
                    MOVE "ENNN060SEL." TO VDU
                    PERFORM SCREENIO
                    IF VDUNUM0 = 3636
                       MOVE "E  A060SEL." TO VDU
                       PERFORM SCREENIO
                       MOVE VDUBUF TO ACTION
                       IF ACTION = "  SKIP"
                          MOVE "Y" TO TRAP-NO-TY-FG
                          MOVE " " TO TRAP-A-DATE-TY-FG
                          MOVE "TODAYS TOTALS WILL BE SKIPPED!"
                                                        TO MESS
                          PERFORM SEND-MESS
                          PERFORM SEND-BR-NAME
                          GO TO ACTIVITY-OPTION
                       ELSE
                          MOVE " " TO TRAP-NO-TY-FG
                          MOVE "TODAYS TOTALS WILL BE INCLUDED!"
                                                        TO MESS
                          PERFORM SEND-MESS
                          PERFORM SEND-BR-NAME
                          GO TO ACTIVITY-OPTION
                    ELSE
                       IF TRAP-A-DATE-TY-FG = " "
                          IF VDUNUM0 = 3535
                             MOVE "Y" TO TRAP-A-DATE-TY-FG
                             MOVE " " TO TRAP-NO-TY-FG
                             MOVE "TODAYS TOTALS TRAP TURNED ON!"
                                                           TO MESS
                             PERFORM SEND-MESS
                             MOVE TODAY-MM TO DATE-MMDDYY-MM
                             MOVE TODAY-DD TO DATE-MMDDYY-DD
                             MOVE TODAY-YY TO DATE-MMDDYY-YY
                             MOVE DATE-MMDDYY TO TY-TRAP-DATE
                             PERFORM SEND-BR-NAME
                             GO TO ACTIVITY-OPTION
                          ELSE
                             GO TO ENTER-SELECTION
                       ELSE
                          IF VDUSTATUS = "1("
                             MOVE TY-TRAP-DATE TO DATE-MMDDYY
                             PERFORM CONVERT-MMDDYY-TO-YYYYMMDD
                             MOVE DATE-YYYYMMDD TO NDTE-DATE
                             MOVE 1 TO NDTE-HOLD
                             PERFORM INCREMENT-DAYS
                             MOVE NDTE-DATE TO DATE-YYYYMMDD
                             PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
                             MOVE DATE-MMDDYY TO TY-TRAP-DATE
                             MOVE "Y" TO TRAP-A-DATE-TY-FG
                             MOVE " " TO TRAP-NO-TY-FG
                             MOVE "TODAYS TOTALS TRAP DATE INCREMENTED!"
                                                              TO MESS
                             PERFORM SEND-MESS
                             PERFORM SEND-BR-NAME
                             GO TO ACTIVITY-OPTION
                          ELSE
                             IF VDUNUM0 NOT = 0
                                MOVE VDUNUM0 TO TY-TRAP-DATE
                                MOVE "Y" TO TRAP-A-DATE-TY-FG
                                MOVE " " TO TRAP-NO-TY-FG
                                MOVE "TODAYS TOTALS TRAP DATE SET!"
                                                              TO MESS
                                PERFORM SEND-MESS
                                PERFORM SEND-BR-NAME
                                GO TO ACTIVITY-OPTION
                             ELSE
                                MOVE " " TO TRAP-A-DATE-TY-FG
                                MOVE "TODAYS TOTALS TRAP TURNED OFF!"
                                                              TO MESS
                                PERFORM SEND-MESS
                                PERFORM SEND-BR-NAME
                                GO TO ACTIVITY-OPTION.

           IF PAGE1
              IF VDUSTATUS = "12"
                 MOVE 1 TO PAGE1-TYPE
                 GO TO ACTIVITY-OPTION
              ELSE
                 IF VDUSTATUS = "16"
                    MOVE 2 TO PAGE1-TYPE
                    GO TO ACTIVITY-OPTION.

           IF PAGE2
              IF VDUSTATUS = "18"
                 MOVE 1 TO ACTIVITY
                 GO TO ACTIVITY-OPTION
              ELSE
                 IF VDUSTATUS = "1:"
                    MOVE 2 TO ACTIVITY
                    GO TO ACTIVITY-OPTION
                 ELSE
                    IF VDUSTATUS = "12"
                       MOVE 1 TO PAGE2-TYPE
                       GO TO ACTIVITY-OPTION
                    ELSE
                       IF VDUSTATUS = "16"
                          MOVE 2 TO PAGE2-TYPE
                          GO TO ACTIVITY-OPTION
                       ELSE
                          IF VDUSTATUS = "1<"
                             IF PAGE2-O2
                                MOVE 1 TO PAGE2-O2-TYPE
                              ELSE
                                ADD 1 TO PAGE2-O2-TYPE
                             END-IF
                             GO TO ACTIVITY-OPTION.

           IF PAGE3

      * CURRENT:
              IF VDUSTATUS = "12"
                 MOVE 1 TO DELINQ
                 GO TO DELINQ-OPTION
              ELSE
      * TODAY:
              IF VDUSTATUS = "16"
                 MOVE 4 TO DELINQ
                 GO TO DELINQ-OPTION
              ELSE
      * CHANGE SINCE BEGINNING OF MONTH:
              IF VDUSTATUS = "18"
                 IF DELINQ NOT = 5
                    MOVE 5 TO DELINQ
                    MOVE 0 TO DIFF-PER
                    GO TO DELINQ-OPTION
                 ELSE
                    IF DIFF-PER = 0
                       MOVE 1 TO DIFF-PER
                       GO TO DELINQ-OPTION
                    ELSE
                       MOVE 0 TO DIFF-PER
                       GO TO DELINQ-OPTION
              ELSE
      * TOGGLE BEG OF MONTH & END OF LAST MONTH:
              IF VDUSTATUS = "1:"
                 IF DELINQ = 2
                    MOVE 3 TO DELINQ
                 ELSE
                    MOVE 2 TO DELINQ
                 END-IF
                 GO TO DELINQ-OPTION
              ELSE
      * TOGGLE DELINQUENCY TO (1)INCLUDE (2)SKIP (3)ONLY BANKRUPT:
              IF VDUSTATUS = "1<"
                 IF BNKRPT-DEL = 3
                    MOVE 0 TO BNKRPT-DEL
                 END-IF
                 ADD 1 TO BNKRPT-DEL
                 GO TO DELINQ-OPTION
              ELSE
      * DAILY OPERATING STATISTICS:
              IF VDUSTATUS = "1>"
                 IF DELINQ NOT = 7
                    MOVE 7 TO DELINQ
                    MOVE 0 TO DAILY-TYPE
                    GO TO DELINQ-OPTION
                 ELSE
                    IF DAILY-TYPE = 0
                       MOVE 1 TO DAILY-TYPE
                       GO TO DELINQ-OPTION
                    ELSE
                       MOVE 0 TO DAILY-TYPE
                       GO TO DELINQ-OPTION 
              ELSE
              IF VDUSTATUS = "1!"
                  IF DELINQ = 7
                     IF WORLD-DAILY-RECENCY = "W"
                       MOVE " " TO WORLD-DAILY-RECENCY
                       GO TO DELINQ-OPTION
                     ELSE
    
                   MOVE "W" TO WORLD-DAILY-RECENCY
                       GO TO DELINQ-OPTION.

           IF PAGE4
              IF VDUSTATUS = "18"
                 MOVE 1 TO ACTIVITY
                 GO TO ACTIVITY-OPTION
              ELSE
                 IF VDUSTATUS = "1:"
                    MOVE 2 TO ACTIVITY
                    GO TO ACTIVITY-OPTION
                 ELSE
                    IF VDUSTATUS = "12"
                       MOVE 1 TO PAGE4-TYPE
                       GO TO ACTIVITY-OPTION
                    ELSE
                       IF VDUSTATUS = "16"
                          MOVE 2 TO PAGE4-TYPE
                          GO TO ACTIVITY-OPTION
                       ELSE
                          IF VDUSTATUS = "1<"
                             IF PAGE4-O2
                                MOVE 1 TO PAGE4-O2-TYPE
                             ELSE
                                ADD 1 TO PAGE4-O2-TYPE
                             END-IF
                             GO TO ACTIVITY-OPTION.

           IF PAGE5
              IF VDUSTATUS = "12"
                 MOVE 1 TO PAGE5-TYPE
                 GO TO ACTIVITY-OPTION
              ELSE
                 IF VDUSTATUS = "16"
                    MOVE 2 TO PAGE5-TYPE
                    GO TO ACTIVITY-OPTION.


           IF PAGE6
              IF VDUSTATUS = "12"
                 MOVE 1 TO PAGE6-TYPE
                 GO TO ACTIVITY-OPTION
              ELSE
                 IF VDUSTATUS = "16"
                    MOVE 2 TO PAGE6-TYPE
                    GO TO ACTIVITY-OPTION.

      *----------------------------------------------------------------------
      * DO NOT DISPLAY TOTAL P&L WITH NEW VOLUME
      *  IF VIEWING TOTAL P&L WHEN SWITCHING TO NEW VOL, THE SCREEN RESET TO
      *      RECEIVABLES WITH O2.
      *  IF VIEWING NEW VOLUME AND USING TOGGLE F6, TOTAL P&L IS SKIPPED.
      *----------------------------------------------------------------------
           IF PAGE7
              IF VDUSTATUS = "18"
                 MOVE 1 TO P7-ACTIVITY
                 GO TO ACTIVITY-OPTION
              ELSE
                 IF VDUSTATUS = "1:"
                    MOVE 2 TO P7-ACTIVITY
                    GO TO ACTIVITY-OPTION
                 ELSE
                    IF VDUSTATUS = "12"
                       MOVE 1 TO PAGE7-TYPE
                       GO TO ACTIVITY-OPTION
                    ELSE
                       IF VDUSTATUS = "16"
                          MOVE 2 TO PAGE7-TYPE
                          GO TO ACTIVITY-OPTION
                       ELSE
                          IF VDUSTATUS = "1<"
                             IF PAGE7-O2
                                MOVE 1 TO PAGE7-O2-TYPE
                             ELSE
                                ADD 1 TO PAGE7-O2-TYPE
                             END-IF
                             GO TO ACTIVITY-OPTION
                          ELSE
                             IF VDUSTATUS = "1>"
                                IF PAGE7-TOT-PL
                                   MOVE 2 TO PAGE7-TOT-TYPE
                                ELSE
                                   MOVE 1 TO PAGE7-TOT-TYPE
                                END-IF
                                GO TO ACTIVITY-OPTION.


           IF PAGE8
              IF VDUSTATUS = "12"
                 MOVE 1 TO PAGE8-TYPE
                 GO TO ACTIVITY-OPTION
              ELSE
                 IF VDUSTATUS = "16"
                    MOVE 3 TO PAGE8-TYPE
                    GO TO ACTIVITY-OPTION
                 ELSE
                    IF VDUSTATUS = "18"
                       MOVE 2 TO PAGE8-TYPE
                       GO TO ACTIVITY-OPTION.

           IF PAGE9
              IF VDUSTATUS = "12"
                 MOVE 1 TO PAGE9-TYPE
                 GO TO ACTIVITY-OPTION
              ELSE
                 IF VDUSTATUS = "16"
                    MOVE 2 TO PAGE9-TYPE
                    GO TO ACTIVITY-OPTION
                 ELSE
                    IF VDUSTATUS = "18"
                       MOVE 1 TO ACTIVITY
                       GO TO ACTIVITY-OPTION
                    ELSE
                       IF VDUSTATUS = "1:"
                          MOVE 2 TO ACTIVITY
                          GO TO ACTIVITY-OPTION.

           IF PAGE10
              IF VDUSTATUS = "18"
                 MOVE 1 TO ACTIVITY
                 GO TO ACTIVITY-OPTION
              ELSE
                 IF VDUSTATUS = "1:"
                    MOVE 2 TO ACTIVITY
                    GO TO ACTIVITY-OPTION.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *  IF ON PAGE 11, JUST RE-DISPLAY SCREEN, PERIOD WILL CHANGE THERE
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
              IF ( PAGE11 ) 
                 IF ( F2-KEY )
                    MOVE 1 TO PAGE11-TYPE
                    GO TO ACTIVITY-OPTION
                 ELSE
                 IF ( F3-KEY )
                    MOVE 2 TO PAGE11-TYPE
                    GO TO ACTIVITY-OPTION
                 ELSE
                    IF VDUSTATUS = "18"
                       MOVE 1 TO ACTIVITY
                       GO TO ACTIVITY-OPTION
                    ELSE
                       IF VDUSTATUS = "1:"
                          MOVE 2 TO ACTIVITY
                          GO TO ACTIVITY-OPTION.

           IF VDUSTATUS NOT = "00"
              GO TO ENTER-SELECTION.

           MOVE VDUBUF TO ACTION.
              
           IF ACTION = "   P12" 
              IF READ-WRFILE-FG NOT = "Y"
                 MOVE "RATING SHEET UPDATE NOT COMPLETED FOR THIS MONTH"
                    TO MESS
                 PERFORM SEND-MESS
                 GO TO ENTER-SELECTION
              END-IF
              MOVE " " TO PASSWDW-STAT 
              MOVE "BYINQ" TO PASSWDW-PGM 
              MOVE 1 TO PASSWDW-SEQ 
              MOVE "PLEASE ENTER THE PASSWORD," TO PASSWDW-PROMPT1 
              MOVE "TO VIEW PAGE 12 (RATING SHEET):" TO PASSWDW-PROMPT2
              MOVE "CL/PASSWD" TO FORM-NAM 
              CALL FORM-PROGX USING FORM-PATH PASSWDW-BUF
              CANCEL FORM-PROGX
              IF PASSWDW-STAT NOT = "OK"
                 GO TO ENTER-SELECTION
              ELSE
                 PERFORM FORM-DESTROY 
                 MOVE 12 TO PAGE-FLAG
                 MOVE FORM-12 TO VDU-FORM-NAME
                 MOVE FORM-PATH TO FORM-PATHNAME
                 PERFORM FORM-RESET
                 PERFORM PROCESS-WRFILE
                 IF GROUP-FLAG = "Y"
                    MOVE "S  A000BRANCH." TO VDU
                    MOVE ENT-GROUP TO VDUBUF
                 ELSE
                    MOVE "SN N040BRANCH." TO VDU
                    IF ALL-BRANCHES
                       MOVE "ALL BRANCHES   " TO BR-NAME
                    ELSE
                       MOVE STORE-BRNO TO VDUNUM0
                                          BR-NO
                       PERFORM GET-BR
                    END-IF
                 END-IF
                 PERFORM SCREENIO
                 PERFORM SEND-BR-NAME
                 PERFORM DISPLAY-CLASS
                 PERFORM DISPLAY-PAGE
                 GO TO ENTER-SELECTION.

           IF PAGE1
              IF ACTION = "    P2" OR "    P3" OR "    P4" OR
                          "    P5" OR "    P6" OR "    P7" OR
                          "    P8" OR "    P9" OR "   P10" OR
                          "   P11" OR "   P12"
                 PERFORM SWITCH-FORM
                 GO TO ENTER-SELECTION.
           IF PAGE2
              IF ACTION = "    P1" OR "    P3" OR "    P4" OR
                          "    P5" OR "    P6" OR "    P7" OR
                          "    P8" OR "    P9" OR "   P10" OR
                          "   P11" OR "   P12"
                 PERFORM SWITCH-FORM
                 GO TO ENTER-SELECTION.
           IF PAGE3
              IF ACTION = "    P1" OR "    P2" OR "    P4" OR
                          "    P5" OR "    P6" OR "    P7" OR
                          "    P8" OR "    P9" OR "   P10" OR
                          "   P11" OR "   P12"
                 PERFORM SWITCH-FORM
                 GO TO ENTER-SELECTION.
           IF PAGE4
              IF ACTION = "    P1" OR "    P2" OR "    P3" OR
                          "    P5" OR "    P6" OR "    P7" OR
                          "    P8" OR "    P9" OR "   P10" OR
                          "   P11" OR "   P12"
                 PERFORM SWITCH-FORM
                 GO TO ENTER-SELECTION.
           IF PAGE5
              IF ACTION = "    P1" OR "    P2" OR "    P3" OR
                          "    P4" OR "    P6" OR "    P7" OR
                          "    P8" OR "    P9" OR "   P10" OR
                          "   P11" OR "   P12"
                 PERFORM SWITCH-FORM
                 GO TO ENTER-SELECTION.
           IF PAGE6
              IF ACTION = "    P1" OR "    P2" OR "    P3" OR
                          "    P4" OR "    P5" OR "    P7" OR
                          "    P8" OR "    P9" OR "   P10" OR
                          "   P11" OR "   P12"
                 PERFORM SWITCH-FORM
                 GO TO ENTER-SELECTION.
           IF PAGE7
              IF ACTION = "    P1" OR "    P2" OR "    P3" OR
                          "    P4" OR "    P5" OR "    P6" OR
                          "    P8" OR "    P9" OR "   P10" OR
                          "   P11" OR "   P12"
                 PERFORM SWITCH-FORM
                 GO TO ENTER-SELECTION.
           IF PAGE8
              IF ACTION = "    P1" OR "    P2" OR "    P3" OR
                          "    P4" OR "    P5" OR "    P6" OR
                          "    P7" OR "    P9" OR "   P10" OR
                          "   P11" OR "   P12"
                 PERFORM SWITCH-FORM
                 GO TO ENTER-SELECTION.
           IF PAGE9
              IF ACTION = "    P1" OR "    P2" OR "    P3" OR
                          "    P4" OR "    P5" OR "    P6" OR
                          "    P7" OR "    P8" OR "   P10" OR
                          "   P11" OR "   P12"
                 PERFORM SWITCH-FORM
                 GO TO ENTER-SELECTION.

           IF PAGE10
              IF ACTION = "    P1" OR "    P2" OR "    P3" OR
                          "    P4" OR "    P5" OR "    P6" OR
                          "    P7" OR "    P8" OR "    P9" OR
                          "   P11" OR "   P12"
                 PERFORM SWITCH-FORM
                 GO TO ENTER-SELECTION.

           IF PAGE11
              IF ACTION = "    P1" OR "    P2" OR "    P3" OR
                          "    P4" OR "    P5" OR "    P6" OR
                          "    P7" OR "    P8" OR "    P9" OR
                          "   P10" OR "   P12"
                 PERFORM SWITCH-FORM
                 GO TO ENTER-SELECTION.

           IF PAGE12
              IF ACTION = "    P1" OR "    P2" OR "    P3" OR
                          "    P4" OR "    P5" OR "    P6" OR
                          "    P7" OR "    P8" OR "    P9" OR
                          "   P10" OR "   P11"
                 PERFORM SWITCH-FORM
                 GO TO ENTER-SELECTION.

           GO TO ENTER-SELECTION.

       ACTIVITY-OPTION.
           PERFORM SUMMARY-MODULE.
           PERFORM DISPLAY-PAGE.
           GO TO ENTER-SELECTION.

       DELINQ-OPTION.
           MOVE DELINQ TO SUB2.
           PERFORM SUMMARY-MODULE.
           PERFORM DISPLAY-PAGE.
           IF DELINQ NOT = 7
              MOVE SPACES TO WORLD-DAILY-RECENCY.
           GO TO ENTER-SELECTION.

       SWITCH-FORM.
           PERFORM FORM-DESTROY.
           IF ACTION-P1-9
               MOVE FORM-BUFFER(ACTION-P1) TO VDU-FORM-NAME
                             MOVE ACTION-P1 TO PAGE-FLAG
           ELSE IF ACTION-P10-12
               MOVE FORM-BUFFER(ACTION-P2) TO VDU-FORM-NAME
                             MOVE ACTION-P2 TO PAGE-FLAG.

      *    EVALUATE ACTION
      *        WHEN "   P12" MOVE FORM-12 TO VDU-FORM-NAME
      *                      MOVE 12 TO PAGE-FLAG
      *        WHEN "   P11" MOVE FORM-11 TO VDU-FORM-NAME
      *                      MOVE 11 TO PAGE-FLAG
      *        WHEN "   P10" MOVE FORM-10 TO VDU-FORM-NAME
      *                      MOVE 10 TO PAGE-FLAG
      *        WHEN "    P9" MOVE FORM-9 TO VDU-FORM-NAME
      *                      MOVE 9 TO PAGE-FLAG
      *        WHEN "    P8" MOVE FORM-8 TO VDU-FORM-NAME
      *                      MOVE 8 TO PAGE-FLAG
      *        WHEN "    P7" MOVE FORM-7 TO VDU-FORM-NAME
      *                      MOVE 7 TO PAGE-FLAG
      *        WHEN "    P6" MOVE FORM-6 TO VDU-FORM-NAME
      *                      MOVE 6 TO PAGE-FLAG
      *        WHEN "    P5" MOVE FORM-5 TO VDU-FORM-NAME
      *                      MOVE 5 TO PAGE-FLAG
      *        WHEN "    P4" MOVE FORM-4 TO VDU-FORM-NAME
      *                      MOVE 4 TO PAGE-FLAG
      *        WHEN "    P3" MOVE FORM-3 TO VDU-FORM-NAME
      *                      MOVE 3 TO PAGE-FLAG
      *        WHEN "    P2" MOVE FORM-2 TO VDU-FORM-NAME
      *                      MOVE 2 TO PAGE-FLAG
      *        WHEN "    P1" MOVE FORM-1 TO VDU-FORM-NAME
      *                      MOVE 1 TO PAGE-FLAG
      *    END-EVALUATE.

           PERFORM SUMMARY-MODULE.
           MOVE FORM-PATH TO FORM-PATHNAME.

           PERFORM FORM-RESET.

           MOVE INQUIRY-TYPE(READ-TYPE) TO WR-BUF.
           MOVE INQUIRY-LIST(READ-TYPE) TO WR-LIST.
           PERFORM CALL-WRFORM.
           IF PAGE2
              MOVE "S  A020SRCD:S." TO VDU
              IF GP-THE-ONLY-SF-SRCD NOT = " "
                 MOVE GP-THE-ONLY-SF-SRCD TO VDUBUF
              ELSE
                 MOVE "RF" TO VDUBUF
              END-IF
              PERFORM SCREENIO

              MOVE "S  A020SCDM:S." TO VDU
              IF GP-NO-SC-USE-DM = "Y"
                 MOVE "DM" TO VDUBUF
              ELSE
                 MOVE "SC" TO VDUBUF
              END-IF
              PERFORM SCREENIO.

           IF GROUP-FLAG = "Y"
              MOVE "S  A000BRANCH." TO VDU
              MOVE ENT-GROUP TO VDUBUF
           ELSE
              MOVE "SN N040BRANCH." TO VDU
              IF ALL-BRANCHES
                 MOVE "ALL BRANCHES   " TO BR-NAME
              ELSE
                 MOVE STORE-BRNO TO VDUNUM0
                                    BR-NO
                 PERFORM GET-BR.
           PERFORM SCREENIO.
           PERFORM SEND-BR-NAME.
           PERFORM DISPLAY-CLASS.
           PERFORM DISPLAY-PAGE.

       END-ENTRY-MODULE.
           EXIT.

      *****************************************************************
      *    DISPLAY CLASS
      *****************************************************************
       DISPLAY-CLASS SECTION.
           MOVE "SN N020LC." TO VDU.
           IF ALL-CLASSES
              MOVE 0 TO VDUNUM0
              MOVE "ALL CLASSES    " TO LC-NAME
           ELSE
              IF ONE-CLASS
                 MOVE " " TO LC-NAME
                 MOVE STORE-CLASS TO VDUNUM0
              ELSE
              IF SF-CLASSES
                 MOVE "SALES FINANCE  " TO LC-NAME
              ELSE
              IF LN-CLASSES
                 MOVE "DIRECT LOANS   " TO LC-NAME
              ELSE
              IF SMALL-LN-CLASSES
                 MOVE "SMALL LOANS    " TO LC-NAME
              ELSE
              IF NON-SMALL-LN-CLASSES
                 MOVE "REAL ESTATE LOANS" TO LC-NAME
              ELSE
              IF WORLD-SMALL-CLASSES
                 MOVE "CLASS 1-4 & 22   " TO LC-NAME
              ELSE
              IF IB-LN-CLASSES
                 MOVE "IB CLASS"      TO LC-NAME
              ELSE
              IF WCBC-CLASSES
                 MOVE "WCBC LOANS     " TO LC-NAME
              ELSE
              IF WORLD-OTHER-CLASSES
                 MOVE "CLASS 5-21 LOANS" TO LC-NAME
              ELSE
              IF WORLD-SF-CLASSES
                 MOVE "CLASS 30 & 40 LOANS" TO LC-NAME.
           PERFORM SCREENIO.
           MOVE "S  A000LCX." TO VDU.
           MOVE LC-NAME TO VDUBUF.
           PERFORM SCREENIO.

      *****************************************************************
      *    DISPLAY PAGE
      *****************************************************************
       DISPLAY-PAGE SECTION.

           MOVE DISPLAY-DATE TO WR-BUF.
           MOVE DATE-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

           MOVE WORLD-MSEL-TABLE(PAGE-FLAG) TO WR-BUF
           MOVE MSEL-LIST TO WR-LIST
           PERFORM CALL-WRFORM.

           IF PAGE1 PERFORM DISPLAY-FIELDS-1
           ELSE IF PAGE2 PERFORM DISPLAY-FIELDS-2
           ELSE IF PAGE3
                MOVE WORLD-DEL-MSEL(DELINQ) TO WR-BUF
                MOVE DEL-MSEL-LIST TO WR-LIST
                PERFORM CALL-WRFORM
                PERFORM DISPLAY-FIELDS-3
           ELSE IF PAGE4 PERFORM DISPLAY-FIELDS-4
           ELSE IF PAGE5 PERFORM DISPLAY-FIELDS-5
           ELSE IF PAGE6 PERFORM DISPLAY-FIELDS-6
           ELSE IF PAGE7 PERFORM DISPLAY-FIELDS-7
           ELSE IF PAGE8 PERFORM DISPLAY-FIELDS-8
           ELSE IF PAGE9 PERFORM DISPLAY-FIELDS-9
           ELSE IF PAGE10 PERFORM DISPLAY-FIELDS-10
           ELSE IF PAGE11 PERFORM DISPLAY-FIELDS-11
           ELSE IF PAGE12 PERFORM DISPLAY-FIELDS-12. 

       END-DISPLAY-PAGE.
           EXIT.

      *****************************************************************
      *    DISPLAY FIELDS PAGE 1
      *****************************************************************
       DISPLAY-FIELDS-1 SECTION.
           MOVE " " TO DISPLAY-BUF1
                       MESS.
           PERFORM CALL-TMMSG.
           IF PAGE1-TYPE = 1
              MOVE PAGE1-TYPE1-DESC TO WR-BUF
           ELSE
              MOVE PAGE1-TYPE2-DESC TO WR-BUF.
           MOVE PAGE1-DESC-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

           PERFORM MOVE-1-TOTALS
               VARYING SUB1 FROM 1 BY 1 UNTIL SUB1 > 9
                 AFTER MTDYTD FROM 1 BY 1 UNTIL MTDYTD > 2.
           MOVE DISPLAY-BUF1 TO WR-BUF.
           MOVE DISPLAY-LIST1 TO WR-LIST.
           PERFORM CALL-WRFORM.
       DISPLAY-FIELDS-EXIT-1.
           EXIT.

      ******************************************
       MOVE-1-TOTALS SECTION.
      ******************************************
           MOVE SUM1(SUB1 MTDYTD) TO DATA-AMT1(SUB1 MTDYTD).
           IF SUB1 < 4
              MOVE SUM1-NO(SUB1 MTDYTD) TO DATA-NO-STP(SUB1 MTDYTD)
              MOVE SUM1-AMT(SUB1 MTDYTD) TO DATA-AMT-STP(SUB1 MTDYTD).
       EXIT-MOVE-1.
           EXIT.

      *****************************************************************
      *    DISPLAY FIELDS PAGE 2
      *****************************************************************
       DISPLAY-FIELDS-2 SECTION.
           MOVE ACT-TYPE(ACTIVITY)  TO WR-BUF.
           MOVE ACT-FNAME TO WR-LIST.
           PERFORM CALL-WRFORM.
           IF PAGE2-TODAY
              IF PAGE2-REC OR PAGE2-NO-O2
                 MOVE ACT-TYPET(ACTIVITY) TO WR-BUF
              ELSE
                 MOVE O2-INT-TYPET(ACTIVITY) TO WR-BUF
              END-IF
           ELSE
              IF PAGE2-REC OR PAGE2-NO-O2
                 MOVE ACT-TYPEL(ACTIVITY) TO WR-BUF
              ELSE
                 MOVE O2-INT-TYPEL(ACTIVITY) TO WR-BUF.

           MOVE ACT-FNAMEL TO WR-LIST.
           PERFORM CALL-WRFORM.
      * DISPLAY O2 INFO
           MOVE O2-ACCT(PAGE2-O2-TYPE) TO WR-BUF.
           MOVE O2-FNAME TO WR-LIST.
           PERFORM CALL-WRFORM.

           MOVE O2-REC(PAGE2-O2-TYPE) TO WR-BUF.
           MOVE O2-FNAMEL TO WR-LIST.
           PERFORM CALL-WRFORM.

           PERFORM MOVE-2-TOTALS.
           MOVE DISPLAY-BUF2 TO WR-BUF.
           MOVE DISPLAY-LIST2 TO WR-LIST.
           PERFORM CALL-WRFORM.
           MOVE " " TO MESS.
           PERFORM CALL-TMMSG.

       DISPLAY-FIELDS-EXIT-2.
           EXIT.

      ******************************************
       MOVE-2-TOTALS SECTION.
      ******************************************
           PERFORM SETUP-2T
               VARYING TYPSUB FROM 1 BY 1 UNTIL TYPSUB > 12
                   AFTER PERIOD FROM 1 BY 1 UNTIL PERIOD > 3.
      * COMPUTE WEIGHTED AVERAGE TERM
           MOVE 0 TO VDUNUM2.
           IF SUM-WGTAV NOT = 0
              DIVIDE SUM-WGTTA BY SUM-WGTAV
                          GIVING VDUNUM2 ROUNDED.
           MOVE VDUNUM2 TO DATA-130.
      * COMPUTE WEIGHTED AVERAGE LOAN AMOUNT
           MOVE 0 TO VDUNUM2.
           IF SUM-WGTTM NOT = 0
              DIVIDE SUM-WGTTA BY SUM-WGTTM
                          GIVING VDUNUM2 ROUNDED.
           MOVE VDUNUM2 TO DATA-140.
      * COMPUTE AVERAGE LOAN TERM
           MOVE 0 TO VDUNUM2.
      *    IF SUM-NO(12,1) NOT = 0
           IF SUM-NO-OUTS NOT = 0
              DIVIDE SUM-WGTTM BY SUM-NO-OUTS
                          GIVING VDUNUM2 ROUNDED.
           MOVE VDUNUM2 TO DATA-150.
           GO TO EXIT-MOVE-2.
       SETUP-2T.
           MOVE SUM-NO(TYPSUB PERIOD) TO DATA-NO(TYPSUB PERIOD).
           MOVE SUM-AMT(TYPSUB PERIOD) TO DATA-AMT(TYPSUB PERIOD).
       SETUPTX.
           EXIT.

       EXIT-MOVE-2.
           EXIT.

      *****************************************************************
      *    DISPLAY FIELDS    PAGE 3
      *****************************************************************
       DISPLAY-FIELDS-3 SECTION.
           PERFORM NEW-DEL-DELINQ.

           IF DELINQ = 1 OR 2 OR 3 OR 4
              MOVE ZEROS TO PAGE-3-TOT-TABLE
              MOVE 1 TO SUBX
              PERFORM SETUP3-AMOUNT
                   VARYING AGESUB FROM 1 BY 1
                            UNTIL AGESUB > 10
                     AFTER RECCON FROM 1 BY 1
                              UNTIL RECCON > 2
              PERFORM SETUP-PAGE3-PERCENTS
                 VARYING RECCON FROM 1 BY 1 UNTIL RECCON > 2.

           IF DELINQ = 5
              MOVE ZEROS TO PAGE-3-TOT-TABLE
              MOVE ZEROS TO HOLD-PAGE-3-TOT-TABLE
              MOVE 1 TO SUBX
              IF DIFF
                 PERFORM SETUP3-DIFF
                      VARYING AGESUB FROM 1 BY 1
                               UNTIL AGESUB > 10
                        AFTER RECCON FROM 1 BY 1
                                 UNTIL RECCON > 2
              ELSE
                 PERFORM SETUP3-PER
                      VARYING AGESUB FROM 1 BY 1
                               UNTIL AGESUB > 10
                        AFTER RECCON FROM 1 BY 1
                                 UNTIL RECCON > 2.

           IF DELINQ = 7
              PERFORM SETUP3-DAILY-LOAN-STATS
              MOVE ZEROS TO PAGE-3-DAILY-TOT-TABLE
                            PAGE-3-REL-DAILY-TOT-TABLE
              MOVE 1 TO SUBX
              PERFORM SETUP3-DAILY-DELINQ-STATS
                   VARYING AGESUB FROM 1 BY 1
                            UNTIL AGESUB > 5
                       AFTER RECCON FROM 1 BY 1
                              UNTIL RECCON > 2
              PERFORM NEW-PAGE3-BOTTOM-FOR-CLINT.

           IF DELINQ = 7
              IF BNKRPT-DEL = 1
                 MOVE "B.I." TO DAILY-LN-PROMPT(12)
              END-IF
              IF BNKRPT-DEL = 2
                 MOVE "B.S." TO DAILY-LN-PROMPT(12)
              END-IF
              IF BNKRPT-DEL = 3
                 MOVE "B.O." TO DAILY-LN-PROMPT(12)
              END-IF

              IF DAILY-TOP AND WORLD-DAILY-RECENCY = "W"
                 MOVE DAILY-BUF3 TO WR-BUF
                 MOVE DAILY-LIST3-TOP-WORLD-RECENCY TO WR-LIST
                 PERFORM CALL-WRFORM
              ELSE
              IF DAILY-TOP AND WORLD-DAILY-RECENCY = " "
                 MOVE DAILY-BUF3 TO WR-BUF
                 MOVE DAILY-LIST3-TOP TO WR-LIST
                 PERFORM CALL-WRFORM
              ELSE
              IF DAILY-BOTTOM AND WORLD-DAILY-RECENCY = "W"
                 MOVE DAILY-BUF3 TO WR-BUF
                 MOVE DAILY-LIST3-BOT-WORLD-RECENCY TO WR-LIST
                 PERFORM CALL-WRFORM
              ELSE
                 MOVE DAILY-BUF3 TO WR-BUF
                 MOVE DAILY-LIST3-BOTTOM TO WR-LIST
                 PERFORM CALL-WRFORM
           ELSE
              IF BNKRPT-DEL = 1
                 MOVE "                              BANKRUPT INCLUDED"
                                                   TO DEL-TABLE(17)
                 MOVE "B.I." TO DEL-TABLE(18)
              END-IF
              IF BNKRPT-DEL = 2
                 MOVE "                              BANKRUPT SKIPPED"
                                                   TO DEL-TABLE(17)
                 MOVE "B.S." TO DEL-TABLE(18)
              END-IF
              IF BNKRPT-DEL = 3
                 MOVE "                                BANKRUPT ONLY"
                                                   TO DEL-TABLE(17)
                 MOVE "B.O." TO DEL-TABLE(18)
              END-IF
              MOVE DISPLAY-BUF3 TO WR-BUF
              MOVE DISPLAY-LIST3 TO WR-LIST
              PERFORM CALL-WRFORM.

           MOVE " " TO MESS.
           PERFORM CALL-TMMSG.

       DISPLAY-FIELDS-EXIT-3.
           EXIT.

      **********************************************
      *    SETUP AMOUNTS
      *
      *   BY TABLE  AGESUB    SUBX    DISPLAY
      *   --------  ------    ----    -------
      *    30         1        1        30
      *    60         2        2        60
      *    90         3        3        90
      *   120         4        4       120
      *   150         5        5       150
      *   180         6        6       180
      *   210         7        7       210
      *                        8       DEL
      *   <M>         8        9       <M>
      *   <P>         9       10       <P>
      *                       11      SLOW
      *   CURRENT    10       12       CUR
      *                       13     TOTAL
      **********************************************
       SETUP3-AMOUNT SECTION.

      * ADD TO TOTAL DELINQUENCY:

           ADD SUM-DEL-UNITS(AGESUB RECCON)
                               TO TOT-DEL-UNITS(1 RECCON).
           ADD SUM-DEL-OUTBAL(AGESUB RECCON)
                               TO TOT-DEL-OUTBAL(1 RECCON).

      * TEST FOR 90+, IF SO ADDED IN 150+ FOR DISPLAY ONLY:
      *B   IF AGESUB = 3
      *B      ADD 1 AGESUB GIVING SUB
      *B      COMPUTE DEL-UNITS(SUBX RECCON) =
      *B               SUM-DEL-UNITS(AGESUB RECCON)
      *B                    + SUM-DEL-UNITS(SUB RECCON)
      *B      COMPUTE DEL-OUTBAL(SUBX RECCON) =
      *B               SUM-DEL-OUTBAL(AGESUB RECCON)
      *B                    + SUM-DEL-OUTBAL(SUB RECCON)
      *B   ELSE
      * TEST FOR <P>, IF SO ADDED IN <M> FOR DISPLAY ONLY:
      *B   IF AGESUB = 6
      *B      SUBTRACT 1 FROM AGESUB GIVING SUB
      *B      COMPUTE DEL-UNITS(SUBX RECCON) =
      *B               SUM-DEL-UNITS(AGESUB RECCON)
      *B                    + SUM-DEL-UNITS(SUB RECCON)
      *B      COMPUTE DEL-OUTBAL(SUBX RECCON) =
      *B               SUM-DEL-OUTBAL(AGESUB RECCON)
      *B                    + SUM-DEL-OUTBAL(SUB RECCON)
      *B   ELSE
              MOVE SUM-DEL-UNITS(AGESUB RECCON)
                                  TO DEL-UNITS(SUBX RECCON)
              MOVE SUM-DEL-OUTBAL(AGESUB RECCON)
                                  TO DEL-OUTBAL(SUBX RECCON).

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      * WILL NOT DISPLAY NB, SF, FB, PB, RN, SC, PO, OR PL
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF SUBX > 13
              MOVE 0 TO DEL-UNITS(SUBX RECCON)
                        DEL-OUTBAL(SUBX RECCON).

      *----------------------------------------------------------------
      *  AGESUB =  7 (210+) DISPLAY DEL TOTALS  ->  SUBX = 8
      *         =  9 (POTS) DISPLAY SLOW TOTALS ->  SUBX = 11
      *         = 10 (CUR)  DISPLAY TOT TOTALS  ->  SUBX = 13
      *----------------------------------------------------------------
           IF AGESUB = 7 OR 9 OR 10
              ADD 1 SUBX GIVING SUBY
              MOVE TOT-DEL-UNITS(1 RECCON)
                               TO DEL-T-UNITS(SUBY RECCON)
              MOVE TOT-DEL-OUTBAL(1 RECCON)
                                TO DEL-T-OUTBAL(SUBY RECCON)
              PERFORM SETUP3-SIGN
              IF RECCON = 2
                 ADD 1 TO SUBX.

           IF RECCON = 2
              ADD 1 TO SUBX.

       SETUP3-AMOUNT-EXIT.
           EXIT.

      **********************************************
       SETUP-PAGE3-PERCENTS SECTION.
      **********************************************
           PERFORM VARYING AGESUB FROM 1 BY 1 UNTIL AGESUB > 2
              COMPUTE PERCENT-WK-2 ROUNDED =
                 SUM-DEL-OUTBAL(AGESUB RECCON) /
                 TOT-DEL-OUTBAL(1 RECCON) * 100
                 IF (DELINQ = 4 OR 5)
                    MOVE 0 TO PERCENT-WK-2
                 END-IF
                 MOVE PERCENT-WK-2 TO DEL-PERCENT(AGESUB RECCON)
           END-PERFORM.

           COMPUTE PERCENT-WK-2 ROUNDED =
             (SUM-DEL-OUTBAL(3 RECCON) + SUM-DEL-OUTBAL(4 RECCON) +
              SUM-DEL-OUTBAL(5 RECCON) + SUM-DEL-OUTBAL(6 RECCON) +
              SUM-DEL-OUTBAL(7 RECCON)) /
              TOT-DEL-OUTBAL(1 RECCON) * 100
              IF (DELINQ = 4 OR 5)
                 MOVE 0 TO PERCENT-WK-2
              END-IF
              MOVE PERCENT-WK-2 TO DEL-PERCENT(3 RECCON).

           COMPUTE PERCENT-WK-2 ROUNDED =
              (TOT-DEL-OUTBAL(1 RECCON) - SUM-DEL-OUTBAL(8 RECCON)
                                        - SUM-DEL-OUTBAL(9 RECCON)
                                        - SUM-DEL-OUTBAL(10 RECCON))
              / TOT-DEL-OUTBAL(1 RECCON) * 100
           IF (DELINQ = 4 OR 5)
              MOVE 0 TO PERCENT-WK-2
           END-IF
           MOVE PERCENT-WK-2 TO DEL-PERCENT(8 RECCON).

           PERFORM VARYING AGESUB FROM 8 BY 1 UNTIL AGESUB > 9
              COMPUTE PERCENT-WK-2 ROUNDED =
                 SUM-DEL-OUTBAL(AGESUB RECCON) /
                 TOT-DEL-OUTBAL(1 RECCON) * 100
              ADD 1 AGESUB GIVING SUBX
              IF (DELINQ = 4 OR 5)
                 MOVE 0 TO PERCENT-WK-2
              END-IF
              MOVE PERCENT-WK-2 TO DEL-PERCENT(SUBX RECCON)
           END-PERFORM.

           COMPUTE PERCENT-WK-2 ROUNDED =
              (TOT-DEL-OUTBAL(1 RECCON) - SUM-DEL-OUTBAL(10 RECCON))
              / TOT-DEL-OUTBAL(1 RECCON) * 100
           IF (DELINQ = 4 OR 5)
              MOVE 0 TO PERCENT-WK-2
           END-IF
           MOVE PERCENT-WK-2 TO DEL-PERCENT(11 RECCON).
       EXIT-SETUP-PAGE3-PERCENTS.
           EXIT.

      **********************************************
      *    SETUP DIFFERENCE TO RELATIVE
      *
      *   BY TABLE  AGESUB    SUBX    DISPLAY
      *   --------  ------    ----    -------
      *    30         1        1        30
      *    60         2        2        60
      *    90         3        3        90
      *   120         4        4       120
      *   150         5        5       150
      *   180         6        6       180
      *   210         7        7       210
      *                        8       DEL
      *   <M>         8        9       <M>
      *   <P>         9       10       <P>
      *                       11      SLOW
      *   CURRENT    10       12       CUR
      *                       13     TOTAL
      **********************************************
       SETUP3-DIFF SECTION.

           COMPUTE UNITS-DIFF ROUNDED =
                 SUM-DEL-UNITS(AGESUB RECCON)
               - SUM-REL-DEL-UNITS(AGESUB RECCON).

      * ADD TO TOTAL:

           ADD UNITS-DIFF TO TOT-DEL-UNITS(1 RECCON).

      * TEST FOR 90+, IF SO ADDED IN 150+ FOR DISPLAY ONLY:
      *B   IF AGESUB = 3
      *B      ADD 1 TO AGESUB GIVING SUB
      *B      COMPUTE UNITS-DIFF ROUNDED =
      *B          SUM-DEL-UNITS(AGESUB RECCON)
      *B               + SUM-DEL-UNITS(SUB RECCON)
      *B                - SUM-REL-DEL-UNITS(AGESUB RECCON)
      *B                 - SUM-REL-DEL-UNITS(SUB RECCON)
      *B   ELSE
      * TEST FOR <P>, IF SO ADDED IN <M> FOR DISPLAY ONLY:
      *B   IF AGESUB = 6
      *B      SUBTRACT 1 FROM AGESUB GIVING SUB
      *B      COMPUTE UNITS-DIFF ROUNDED =
      *B          SUM-DEL-UNITS(AGESUB RECCON)
      *B               + SUM-DEL-UNITS(SUB RECCON)
      *B                - SUM-REL-DEL-UNITS(AGESUB RECCON)
      *B                 - SUM-REL-DEL-UNITS(SUB RECCON).

           MOVE UNITS-DIFF TO DEL-UNITS(SUBX RECCON).

           COMPUTE OUTBAL-DIFF ROUNDED =
                 SUM-DEL-OUTBAL(AGESUB RECCON)
               - SUM-REL-DEL-OUTBAL(AGESUB RECCON).

      * ADD TO TOTAL:

           ADD OUTBAL-DIFF TO TOT-DEL-OUTBAL(1 RECCON).

      * TEST FOR 90+, IF SO ADDED IN 150+ FOR DISPLAY ONLY:
      *B   IF AGESUB = 3
      *B      ADD 1 TO AGESUB GIVING SUB
      *B      COMPUTE OUTBAL-DIFF ROUNDED =
      *B          SUM-DEL-OUTBAL(AGESUB RECCON)
      *B               + SUM-DEL-OUTBAL(SUB RECCON)
      *B                - SUM-REL-DEL-OUTBAL(AGESUB RECCON)
      *B                 - SUM-REL-DEL-OUTBAL(SUB RECCON)
      *B   ELSE
      * TEST FOR <P>, IF SO ADDED IN <M> FOR DISPLAY ONLY:
      *B   IF AGESUB = 6
      *B      SUBTRACT 1 FROM AGESUB GIVING SUB
      *B      COMPUTE OUTBAL-DIFF ROUNDED =
      *B          SUM-DEL-OUTBAL(AGESUB RECCON)
      *B               + SUM-DEL-OUTBAL(SUB RECCON)
      *B                - SUM-REL-DEL-OUTBAL(AGESUB RECCON)
      *B                 - SUM-REL-DEL-OUTBAL(SUB RECCON).

           MOVE OUTBAL-DIFF TO DEL-OUTBAL(SUBX RECCON).

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      * WILL NOT DISPLAY NB, SF, FB, PB, RN, SC, PO, OR PL
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF SUBX > 13
              MOVE 0 TO DEL-UNITS(SUBX RECCON)
                        DEL-OUTBAL(SUBX RECCON).

      *----------------------------------------------------------------
      *  AGESUB =  7 (210+) DISPLAY DEL TOTALS  ->  SUBX = 8
      *         =  9 (POTS) DISPLAY SLOW TOTALS ->  SUBX = 11
      *         = 10 (CUR)  DISPLAY TOT TOTALS  ->  SUBX = 13
      *----------------------------------------------------------------
           IF AGESUB = 7 OR 9 OR 10
              ADD 1 SUBX GIVING SUBY
              MOVE TOT-DEL-UNITS(1 RECCON)
                               TO DEL-T-UNITS(SUBY RECCON)
              MOVE TOT-DEL-OUTBAL(1 RECCON)
                                TO DEL-T-OUTBAL(SUBY RECCON)
              PERFORM SETUP3-SIGN
              IF RECCON = 2
                 ADD 1 TO SUBX.

           IF RECCON = 2
              ADD 1 TO SUBX.

           MOVE 0 TO DEL-PERCENT(AGESUB RECCON).
           MOVE 0 TO DEL-PERCENT(11 RECCON).
       SETUP3-DIFF-EXIT.
           EXIT.

      **********************************************
      *    SETUP PERCENT TO RELATIVE
      *
      *   BY TABLE  AGESUB    SUBX    DISPLAY
      *   --------  ------    ----    -------
      *    30         1        1        30
      *    60         2        2        60
      *    90         3        3        90
      *   120         4        4       120
      *   150         5        5       150
      *   180         6        6       180
      *   210         7        7       210
      *                        8       DEL
      *   <M>         8        9       <M>
      *   <P>         9       10       <P>
      *                       11      SLOW
      *   CURRENT    10       12       CUR
      *                       13     TOTAL
      **********************************************
       SETUP3-PER SECTION.

           IF SUM-REL-DEL-UNITS(AGESUB RECCON) NOT = 0
              COMPUTE UNITS-PER ROUNDED =
                  (SUM-DEL-UNITS(AGESUB RECCON)
                 - SUM-REL-DEL-UNITS(AGESUB RECCON))
                 / SUM-REL-DEL-UNITS(AGESUB RECCON) * 100
           ELSE
              MOVE 0 TO UNITS-PER.


      * ADD TO TOTALS:

           ADD SUM-DEL-UNITS(AGESUB RECCON) TO
                  HOLD-TOT-DEL-UNITS(1 RECCON).
           SUBTRACT SUM-REL-DEL-UNITS(AGESUB RECCON) FROM
                  HOLD-TOT-DEL-UNITS(1 RECCON).
           ADD SUM-REL-DEL-UNITS(AGESUB RECCON) TO
                  HOLD-TOT-DEL-UNITS(2 RECCON).

      *     ADD UNITS-PER TO TOT-DEL-UNITS(1 RECCON).
      * TEST FOR 90+, IF SO ADDED IN 150+ FOR DISPLAY ONLY:
      *B   IF AGESUB = 3
      *B      ADD 1 TO AGESUB GIVING SUB
      *B      COMPUTE UNITS-PER ROUNDED =
      *B          (SUM-DEL-UNITS(AGESUB RECCON)
      *B               + SUM-DEL-UNITS(SUB RECCON)
      *B                - SUM-REL-DEL-UNITS(AGESUB RECCON)
      *B                 - SUM-REL-DEL-UNITS(SUB RECCON))
      *B          / (SUM-REL-DEL-UNITS(AGESUB RECCON) +
      *B             SUM-REL-DEL-UNITS(SUB RECCON)) * 100
      *B   ELSE
      * TEST FOR <P>, IF SO ADDED IN <M> FOR DISPLAY ONLY:
      *B   IF AGESUB = 6
      *B      SUBTRACT 1 FROM AGESUB GIVING SUB
      *B      COMPUTE UNITS-PER ROUNDED =
      *B          (SUM-DEL-UNITS(AGESUB RECCON)
      *B               + SUM-DEL-UNITS(SUB RECCON)
      *B                - SUM-REL-DEL-UNITS(AGESUB RECCON)
      *B                 - SUM-REL-DEL-UNITS(SUB RECCON))
      *B          / (SUM-REL-DEL-UNITS(AGESUB RECCON) +
      *B             SUM-REL-DEL-UNITS(SUB RECCON)) * 100.

      *    IF UNITS-PER NOT = 0
              MOVE UNITS-PER TO DEL-UNITS-P(SUBX RECCON)
              IF DEL-UNITS-SIGN(SUBX RECCON) = " "
                 MOVE "+" TO DEL-UNITS-SIGN(SUBX RECCON).

           IF SUM-REL-DEL-OUTBAL(AGESUB RECCON) NOT = 0
              COMPUTE OUTBAL-PER ROUNDED =
                  (SUM-DEL-OUTBAL(AGESUB RECCON)
                 - SUM-REL-DEL-OUTBAL(AGESUB RECCON))
                 / SUM-REL-DEL-OUTBAL(AGESUB RECCON) * 100
           ELSE
              MOVE 0 TO OUTBAL-PER.

      * ADD TO TOTALS:

           ADD SUM-DEL-OUTBAL(AGESUB RECCON) TO
                  HOLD-TOT-DEL-OUTBAL(1 RECCON).
           SUBTRACT SUM-REL-DEL-OUTBAL(AGESUB RECCON) FROM
                  HOLD-TOT-DEL-OUTBAL(1 RECCON).
           ADD SUM-REL-DEL-OUTBAL(AGESUB RECCON) TO
                  HOLD-TOT-DEL-OUTBAL(2 RECCON).

      *     ADD OUTBAL-PER TO TOT-DEL-OUTBAL(1 RECCON).
      * TEST FOR 90+, IF SO ADDED IN 150+ FOR DISPLAY ONLY:
      *B   IF AGESUB = 3
      *B      ADD 1 TO AGESUB GIVING SUB
      *B      COMPUTE OUTBAL-PER ROUNDED =
      *B          (SUM-DEL-OUTBAL(AGESUB RECCON)
      *B               + SUM-DEL-OUTBAL(SUB RECCON)
      *B                - SUM-REL-DEL-OUTBAL(AGESUB RECCON)
      *B                 - SUM-REL-DEL-OUTBAL(SUB RECCON))
      *B          / (SUM-REL-DEL-OUTBAL(AGESUB RECCON) +
      *B             SUM-REL-DEL-OUTBAL(SUB RECCON)) * 100
      *B   ELSE
      * TEST FOR <P>, IF SO ADDED IN <M> FOR DISPLAY ONLY:
      *B   IF AGESUB = 6
      *B      SUBTRACT 1 FROM AGESUB GIVING SUB
      *B      COMPUTE OUTBAL-PER ROUNDED =
      *B          (SUM-DEL-OUTBAL(AGESUB RECCON)
      *B               + SUM-DEL-OUTBAL(SUB RECCON)
      *B                - SUM-REL-DEL-OUTBAL(AGESUB RECCON)
      *B                 - SUM-REL-DEL-OUTBAL(SUB RECCON))
      *B          / (SUM-REL-DEL-OUTBAL(AGESUB RECCON)  +
      *B             SUM-REL-DEL-OUTBAL(SUB RECCON)) * 100.
      *    IF OUTBAL-PER NOT = 0
              MOVE OUTBAL-PER TO DEL-OUTBAL(SUBX RECCON)
              IF DEL-OUTBAL-SIGN(SUBX RECCON) = " "
                 MOVE "+" TO DEL-OUTBAL-SIGN(SUBX RECCON).

      * ADD TO TOTALS:

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      * WILL NOT DISPLAY NB, SF, FB, PB, RN, SC, PO, OR PL
      *  PERCENTAGES.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF SUBX > 13
              MOVE 0 TO DEL-UNITS-P(SUBX RECCON)
                        DEL-OUTBAL(SUBX RECCON).

      *----------------------------------------------------------------
      *  AGESUB =  7 (210+) DISPLAY DEL TOTALS  ->  SUBX = 8
      *         =  9 (POTS) DISPLAY SLOW TOTALS ->  SUBX = 11
      *         = 10 (CUR)  DISPLAY TOT TOTALS  ->  SUBX = 13
      *----------------------------------------------------------------
           IF AGESUB = 7 OR 9 OR 10
              ADD 1 SUBX GIVING SUBY
              IF HOLD-TOT-DEL-UNITS(2 RECCON) NOT = 0
                 COMPUTE TOT-DEL-UNITS(1 RECCON) =
                         HOLD-TOT-DEL-UNITS(1 RECCON) /
                              HOLD-TOT-DEL-UNITS(2 RECCON) * 100
              ELSE
                 MOVE 0 TO TOT-DEL-UNITS(1 RECCON)
              END-IF
              MOVE TOT-DEL-UNITS(1 RECCON)
                               TO DEL-T-UNITS-P(SUBY RECCON)

              IF HOLD-TOT-DEL-OUTBAL(2 RECCON) NOT = 0
                 COMPUTE TOT-DEL-OUTBAL(1 RECCON) =
                         HOLD-TOT-DEL-OUTBAL(1 RECCON) /
                              HOLD-TOT-DEL-OUTBAL(2 RECCON) * 100
              ELSE
                MOVE 0 TO TOT-DEL-OUTBAL(1 RECCON)
              END-IF
              MOVE TOT-DEL-OUTBAL(1 RECCON)
                                TO DEL-T-OUTBAL(SUBY RECCON)

              PERFORM SETUP3-SIGN-PER
              IF RECCON = 2
                 ADD 1 TO SUBX.

           IF RECCON = 2
              ADD 1 TO SUBX.

           MOVE 0 TO DEL-PERCENT(AGESUB RECCON).
           MOVE 0 TO DEL-PERCENT(11 RECCON).

       SETUP3-PER-EXIT.
           EXIT.

      **********************************************
      *    SETUP DAILY OPERATING LOAN STATISTICS
      **********************************************
       SETUP3-DAILY-LOAN-STATS SECTION.
           MOVE SUM-DAILY-AMT TO DAILY-OSBAL.
           IF WORLD-DAILY-RECENCY = "W"
              MOVE SUM-DAILY-OUTBAL-RECENCY60 TO DAILY-OSBAL.

           MOVE SUM-DAILY-NO TO DAILY-OSNO.
           IF WORLD-DAILY-RECENCY = "W"
              MOVE SUM-DAILY-UNITS-RECENCY60 TO DAILY-OSNO.


           COMPUTE DAILY-OSAVG ROUNDED =
                SUM-DAILY-AMT / SUM-DAILY-NO.

           IF WORLD-DAILY-RECENCY = "W"
              COMPUTE DAILY-OSAVG ROUNDED =
                SUM-DAILY-OUTBAL-RECENCY60 /
                   SUM-DAILY-UNITS-RECENCY60.


           COMPUTE DAILY-G-L-BAL ROUNDED =
                SUM-DAILY-AMT - SUM-REL-DAILY-AMT.
           COMPUTE DAILY-G-L-NO ROUNDED =
                SUM-DAILY-NO - SUM-REL-DAILY-NO.

           IF WORLD-DAILY-RECENCY = "W"
              COMPUTE DAILY-G-L-BAL ROUNDED =
                  SUM-DAILY-OUTBAL-RECENCY60 - 
                       SUM-DAILY-REL-OUTBAL-RECENCY60
              COMPUTE DAILY-G-L-NO ROUNDED = 
                  SUM-DAILY-UNITS-RECENCY60 -
                       SUM-DAILY-REL-UNITS-RECENCY60.

           MOVE SUM-DAILY-PL-AMT TO DAILY-AMT-PL.
           MOVE SUM-DAILY-PL-NO TO DAILY-NO-PL.

           PERFORM SET-INCREASE-REMAIN.

      * SET VOLUME MTD:
           IF SUM-DAILY-VOL > 999999999
              MOVE SUM-DAILY-VOL TO DAILY-VOL-BAL-BIG
           ELSE
              MOVE SUM-DAILY-VOL TO DAILY-VOL-BAL.

      * AVG LOAN AMOUNT
           MOVE 0 TO DAILY-VOL-AVG-LN.
           IF SUM-DAILY-SUMWGTTM NOT = 0
              DIVIDE SUM-DAILY-SUMWGTTA BY SUM-DAILY-SUMWGTTM
                          GIVING DAILY-VOL-AVG-LN ROUNDED.
      * AVG TERM
           MOVE 0 TO DAILY-VOL-AVG-TM.
           IF SUM-DAILY-SUMWGTAV NOT = 0
              DIVIDE SUM-DAILY-SUMWGTTA BY SUM-DAILY-SUMWGTAV
                          GIVING DAILY-VOL-AVG-TM ROUNDED.

      * NEW LOANS
           MOVE SUM-DAILY-NB TO DAILY-NB-NO.
           IF SUM-DAILY-NO NOT = 0
              COMPUTE DAILY-NB-PER ROUNDED =
                   SUM-DAILY-NB / SUM-DAILY-NO * 100.

           MOVE SUM-DAILY-FB TO DAILY-FB-NO.
           IF SUM-DAILY-NO NOT = 0
              COMPUTE DAILY-RN-PER ROUNDED =
                   SUM-DAILY-FB / SUM-DAILY-NO * 100.

           IF SUM-DAILY-PB > 0
              MOVE SUM-DAILY-PB TO DAILY-PB-NO6
           ELSE
              MOVE SUM-DAILY-PB TO DAILY-PB-NO.
           IF SUM-DAILY-NO NOT = 0
              COMPUTE DAILY-PB-PER ROUNDED =
                   SUM-DAILY-PB / SUM-DAILY-NO * 100.

           IF SUM-DAILY-NB NOT = 0
              COMPUTE DAILY-NB-AVG ROUNDED =
                   SUM-DAILY-NB-AMT / SUM-DAILY-NB.

           IF SUM-DAILY-FB NOT = 0
              COMPUTE DAILY-FB-AVG ROUNDED =
                   SUM-DAILY-FB-AMT / SUM-DAILY-FB.

           IF SUM-DAILY-PB NOT = 0
              COMPUTE DAILY-PB-AVG ROUNDED =
                   SUM-DAILY-PB-AMT / SUM-DAILY-PB.

      ************************************************
       SET-INCREASE-REMAIN SECTION.
      ************************************************
           MOVE " REMAINING" TO DAILY-INC-REMAIN-P.

           IF WORLD-DAILY-RECENCY = "W"
              MOVE "012:S" TO DAILY-LIST3-NFR60-T-W
                              DAILY-LIST3-NFR60-B-W
           ELSE
              MOVE "012:S" TO DAILY-LIST3-NFR60-T
                           DAILY-LIST3-NFR60-B.

           COMPUTE VDUNUM0 ROUNDED =
              SUM-DAILY-OUTBAL(2 1)
                  - (SUM-REL-DAILY-OUTBAL(3 1) - SUM-DAILY-OUTBAL(3 1))
                   + SUM-DAILY-PL-AMT.
           MOVE VDUNUM0 TO DAILY-INC-REMAIN.

           IF WORLD-DAILY-RECENCY = "W"
              IF VDUNUM0 > 0
                  MOVE "022:RF" TO DAILY-LIST3-NFR90-T-W
                               DAILY-LIST3-NFR90-B-W
              ELSE
                  MOVE "022" TO DAILY-LIST3-NFR90-T-W
                            DAILY-LIST3-NFR90-B-W 
           ELSE
           IF VDUNUM0 > 0
              MOVE "022:RF" TO DAILY-LIST3-NFR90-T
                               DAILY-LIST3-NFR90-B
           ELSE
              MOVE "022" TO DAILY-LIST3-NFR90-T
                            DAILY-LIST3-NFR90-B.


      **********************************************
      *    SETUP DAILY OPERATING DELINQ STATISTICS
      **********************************************
       SETUP3-DAILY-DELINQ-STATS SECTION.
           IF DAILY-TOP AND AGESUB > 3
              GO TO SETUP3-DAILY-DELINQ-STATS-EXIT.

      * ADD REL:
           ADD SUM-REL-DAILY-UNITS(AGESUB RECCON)
                                  TO TOT-REL-DAILY-UNITS(1 RECCON).
           COMPUTE UNITS-PER ROUNDED =
                     SUM-REL-DAILY-UNITS(AGESUB RECCON)
                                          / SUM-REL-DAILY-NO * 100.
           ADD UNITS-PER TO TOT-REL-DAILY-P-UNITS(1 RECCON).
           ADD SUM-REL-DAILY-OUTBAL(AGESUB RECCON)
                                  TO TOT-REL-DAILY-OUTBAL(1 RECCON).
           COMPUTE OUTBAL-PER ROUNDED =
                     SUM-REL-DAILY-OUTBAL(AGESUB RECCON)
                                         / SUM-REL-DAILY-AMT * 100.
           ADD OUTBAL-PER TO TOT-REL-DAILY-P-OUTBAL(1 RECCON).

      * MOVE REL:
           IF NOT (DAILY-BOTTOM AND (AGESUB = 1 OR 2 OR 3))
              MOVE SUM-REL-DAILY-UNITS(AGESUB RECCON)
                                   TO DAILY-UNITS(SUBX RECCON)
              MOVE UNITS-PER TO DAILY-P-UNITS(SUBX RECCON)

              ADD 0.50 SUM-REL-DAILY-OUTBAL(AGESUB RECCON)
                                 GIVING DAILY-OUTBAL(SUBX RECCON)
              MOVE OUTBAL-PER TO DAILY-P-OUTBAL(SUBX RECCON)
              MOVE SUBX TO SUBY
              PERFORM SETUP3-DAILY-SIGN
              ADD 1 TO SUBX.
      * ADD:
           ADD SUM-DAILY-UNITS(AGESUB RECCON)
                                  TO TOT-DAILY-UNITS(1 RECCON).
           COMPUTE UNITS-PER ROUNDED =
               SUM-DAILY-UNITS(AGESUB RECCON)
                          / SUM-DAILY-NO * 100.
           ADD UNITS-PER TO TOT-DAILY-P-UNITS(1 RECCON).
           ADD SUM-DAILY-OUTBAL(AGESUB RECCON)
                                  TO TOT-DAILY-OUTBAL(1 RECCON).
           COMPUTE OUTBAL-PER ROUNDED =
               SUM-DAILY-OUTBAL(AGESUB RECCON)
                          / SUM-DAILY-AMT * 100.
           ADD OUTBAL-PER TO TOT-DAILY-P-OUTBAL(1 RECCON).

      * MOVE:
           IF NOT (DAILY-BOTTOM AND (AGESUB = 1 OR 2 OR 3))
              MOVE SUM-DAILY-UNITS(AGESUB RECCON)
                                   TO DAILY-UNITS(SUBX RECCON)
              MOVE UNITS-PER TO DAILY-P-UNITS(SUBX RECCON)
              ADD 0.50 SUM-DAILY-OUTBAL(AGESUB RECCON)
                                 GIVING DAILY-OUTBAL(SUBX RECCON)
              MOVE OUTBAL-PER TO DAILY-P-OUTBAL(SUBX RECCON).

           IF (AGESUB = 3 AND DAILY-TOP)
                   OR (AGESUB = 5 AND DAILY-BOTTOM)
              ADD 2 SUBX GIVING SUBY
           ELSE
              IF (AGESUB = 3 AND DAILY-BOTTOM)
                 MOVE SUBX TO SUBY.

           IF (AGESUB = 3)
                   OR (AGESUB = 5 AND DAILY-BOTTOM)
              MOVE TOT-REL-DAILY-UNITS(1 RECCON)
                               TO DAILY-T-UNITS(SUBY RECCON)
              MOVE TOT-REL-DAILY-P-UNITS(1 RECCON)
                               TO DAILY-P-UNITS(SUBY RECCON)
              ADD 0.50 TOT-REL-DAILY-OUTBAL(1 RECCON)
                              GIVING DAILY-T-OUTBAL(SUBY RECCON)
              MOVE TOT-REL-DAILY-P-OUTBAL(1 RECCON)
                               TO DAILY-P-OUTBAL(SUBY RECCON)
              PERFORM SETUP3-DAILY-SIGN

              ADD 1 TO SUBY
              MOVE TOT-DAILY-UNITS(1 RECCON)
                               TO DAILY-T-UNITS(SUBY RECCON)
              MOVE TOT-DAILY-P-UNITS(1 RECCON)
                               TO DAILY-P-UNITS(SUBY RECCON)
              ADD 0.50 TOT-DAILY-OUTBAL(1 RECCON)
                                GIVING DAILY-T-OUTBAL(SUBY RECCON)
              MOVE TOT-DAILY-P-OUTBAL(1 RECCON)
                               TO DAILY-P-OUTBAL(SUBY RECCON)
              IF RECCON = 2
                 ADD 2 TO SUBX.

           IF NOT (DAILY-BOTTOM AND (AGESUB = 1 OR 2 OR 3))
              IF RECCON = 2
                 ADD 2 TO SUBX
              ELSE
                 SUBTRACT 1 FROM SUBX
           ELSE
              IF (DAILY-BOTTOM AND AGESUB = 3)
                 IF RECCON = 2
                    ADD 1 TO SUBX.

       SETUP3-DAILY-DELINQ-STATS-EXIT.
           EXIT.

      ************************************************
       NEW-PAGE3-BOTTOM-FOR-CLINT SECTION.
      ************************************************

           IF NOT DAILY-BOTTOM 
              GO TO EXIT-NEW-PAGE3-BOTTOM-FOR-CLINT.

           MOVE "CUR:" TO DAILY-LN-PROMPT(10).
      * UNITS FROM CUR: BEGINNING OF MONTH
           MOVE HBY-UNITS(2,1,10) TO DAILY-UNITS(10,1).
      * TOT: FROM BEGINNING OF MONTH
           COMPUTE UNITS-PER ROUNDED = HBY-UNITS(2,1,10)
                                    / (HBY-UNITS(2,1,1) +
                                       HBY-UNITS(2,1,2) +
                                       HBY-UNITS(2,1,3) +
                                       HBY-UNITS(2,1,4) +
                                       HBY-UNITS(2,1,5) +
                                       HBY-UNITS(2,1,6) +
                                       HBY-UNITS(2,1,7) +
                                       HBY-UNITS(2,1,8) +
                                       HBY-UNITS(2,1,9) +
                                       HBY-UNITS(2,1,10)) * 100.
           MOVE UNITS-PER TO DAILY-P-UNITS(10,1).
      * RECENCY FROM CUR: BEGINNING OF MONTH
           MOVE HBY-OUTBAL(2,1,10) TO DAILY-OUTBAL(10,1).
      * TOT: FROM BEGINNING OF MONTH
           COMPUTE OUTBAL-PER ROUNDED = HBY-OUTBAL(2,1,10)
                                    / (HBY-OUTBAL(2,1,1) +
                                       HBY-OUTBAL(2,1,2) +
                                       HBY-OUTBAL(2,1,3) +
                                       HBY-OUTBAL(2,1,4) +
                                       HBY-OUTBAL(2,1,5) +
                                       HBY-OUTBAL(2,1,6) +
                                       HBY-OUTBAL(2,1,7) +
                                       HBY-OUTBAL(2,1,8) +
                                       HBY-OUTBAL(2,1,9) +
                                       HBY-OUTBAL(2,1,10)) * 100.
           MOVE OUTBAL-PER TO DAILY-P-OUTBAL(10,1).
      * CON UNITS FROM CUR: BEGINNING OF MONTH
           MOVE HBY-UNITS(2,2,10) TO DAILY-UNITS(10,2).
      * TOT: FROM BEGINNING OF MONTH
           COMPUTE UNITS-PER ROUNDED = HBY-UNITS(2,2,10)
                                    / (HBY-UNITS(2,2,1) +
                                       HBY-UNITS(2,2,2) +
                                       HBY-UNITS(2,2,3) +
                                       HBY-UNITS(2,2,4) +
                                       HBY-UNITS(2,2,5) +
                                       HBY-UNITS(2,2,6) +
                                       HBY-UNITS(2,2,7) +
                                       HBY-UNITS(2,2,8) +
                                       HBY-UNITS(2,2,9) +
                                       HBY-UNITS(2,2,10)) * 100.
           MOVE UNITS-PER TO DAILY-P-UNITS(10,2).
      * CUR: CONTRACTUAL FROM BEGINNING OF MONTH
           MOVE HBY-OUTBAL(2,2,10) TO DAILY-OUTBAL(10,2).
      * TOT: FROM BEGINNING OF MONTH CONTRACTUAL
           COMPUTE OUTBAL-PER ROUNDED = HBY-OUTBAL(2,2,10)
                                    / (HBY-OUTBAL(2,2,1) +
                                       HBY-OUTBAL(2,2,2) +
                                       HBY-OUTBAL(2,2,3) +
                                       HBY-OUTBAL(2,2,4) +
                                       HBY-OUTBAL(2,2,5) +
                                       HBY-OUTBAL(2,2,6) +
                                       HBY-OUTBAL(2,2,7) +
                                       HBY-OUTBAL(2,2,8) +
                                       HBY-OUTBAL(2,2,9) +
                                       HBY-OUTBAL(2,2,10)) * 100.
           MOVE OUTBAL-PER TO DAILY-P-OUTBAL(10,2).

      * CUR: REM-> , NUMBERS ARE FROM PAGE 3 F2 CURRENT
           MOVE REM TO DAILY-LN-PROMPT(11).

      *CUR:REM> CONTRACTUAL
      * UNITS FROM F2-CURRENT, INCLUDING TYFILE

           COMPUTE DAILY-UNITS(11,1) = 
                    HBY-UNITS(1,1,10) + HTY-UNITS(1,10).
      
           COMPUTE UNITS-PER ROUNDED = 
                    ( HBY-UNITS(1,1,10) + HTY-UNITS(1,10) )
                                    / (HBY-UNITS(1,1,1) +
                                       HBY-UNITS(1,1,2) +
                                       HBY-UNITS(1,1,3) +
                                       HBY-UNITS(1,1,4) +
                                       HBY-UNITS(1,1,5) +
                                       HBY-UNITS(1,1,6) +
                                       HBY-UNITS(1,1,7) +
                                       HBY-UNITS(1,1,8) +
                                       HBY-UNITS(1,1,9) +
                                       HBY-UNITS(1,1,10)+
                                       HTY-UNITS(1,1) +
                                       HTY-UNITS(1,2) +
                                       HTY-UNITS(1,3) +
                                       HTY-UNITS(1,4) +
                                       HTY-UNITS(1,5) +
                                       HTY-UNITS(1,6) +
                                       HTY-UNITS(1,7) +
                                       HTY-UNITS(1,8) +
                                       HTY-UNITS(1,9) +
                                       HTY-UNITS(1,10) ) * 100.
           MOVE UNITS-PER TO DAILY-P-UNITS(11,1).
           
           COMPUTE DAILY-OUTBAL(11,1) = 
                    HBY-OUTBAL(1,1,10) + HTY-OUTBAL(1,10).

           COMPUTE OUTBAL-PER ROUNDED = 
                    ( HBY-OUTBAL(1,1,10) + HTY-OUTBAL(1,10) )
                                    / (HBY-OUTBAL(1,1,1) +
                                       HBY-OUTBAL(1,1,2) +
                                       HBY-OUTBAL(1,1,3) +
                                       HBY-OUTBAL(1,1,4) +
                                       HBY-OUTBAL(1,1,5) +
                                       HBY-OUTBAL(1,1,6) +
                                       HBY-OUTBAL(1,1,7) +
                                       HBY-OUTBAL(1,1,8) +
                                       HBY-OUTBAL(1,1,9) +
                                       HBY-OUTBAL(1,1,10)+
                                       HTY-OUTBAL(1,1) +
                                       HTY-OUTBAL(1,2) +
                                       HTY-OUTBAL(1,3) +
                                       HTY-OUTBAL(1,4) +
                                       HTY-OUTBAL(1,5) +
                                       HTY-OUTBAL(1,6) +
                                       HTY-OUTBAL(1,7) +
                                       HTY-OUTBAL(1,8) +
                                       HTY-OUTBAL(1,9) +
                                       HTY-OUTBAL(1,10))
                                        * 100.
           MOVE OUTBAL-PER TO DAILY-P-OUTBAL(11,1).

      *CUR:REM> CONTRACTUAL
           COMPUTE DAILY-UNITS(11,2) = 
                    HBY-UNITS(1,2,10) + HTY-UNITS(2,10).
           COMPUTE UNITS-PER ROUNDED = 
                    ( HBY-UNITS(1,2,10) + HTY-UNITS(2,10) )
                                    / (HBY-UNITS(1,2,1) +
                                       HBY-UNITS(1,2,2) +
                                       HBY-UNITS(1,2,3) +
                                       HBY-UNITS(1,2,4) +
                                       HBY-UNITS(1,2,5) +
                                       HBY-UNITS(1,2,6) +
                                       HBY-UNITS(1,2,7) +
                                       HBY-UNITS(1,2,8) +
                                       HBY-UNITS(1,2,9) +
                                       HBY-UNITS(1,2,10)+
                                       HTY-UNITS(2,1) +
                                       HTY-UNITS(2,2) +
                                       HTY-UNITS(2,3) +
                                       HTY-UNITS(2,4) +
                                       HTY-UNITS(2,5) +
                                       HTY-UNITS(2,6) +
                                       HTY-UNITS(2,7) +
                                       HTY-UNITS(2,8) +
                                       HTY-UNITS(2,9) +
                                       HTY-UNITS(2,10) ) * 100.
           MOVE UNITS-PER TO DAILY-P-UNITS(11,2).

           COMPUTE DAILY-OUTBAL(11,2) = 
                    HBY-OUTBAL(1,2,10) + HTY-OUTBAL(2,10).

           COMPUTE OUTBAL-PER ROUNDED = 
                    ( HBY-OUTBAL(1,2,10) + HTY-OUTBAL(2,10) )
                                    / (HBY-OUTBAL(1,2,1) +
                                       HBY-OUTBAL(1,2,2) +
                                       HBY-OUTBAL(1,2,3) +
                                       HBY-OUTBAL(1,2,4) +
                                       HBY-OUTBAL(1,2,5) +
                                       HBY-OUTBAL(1,2,6) +
                                       HBY-OUTBAL(1,2,7) +
                                       HBY-OUTBAL(1,2,8) +
                                       HBY-OUTBAL(1,2,9) +
                                       HBY-OUTBAL(1,2,10)+
                                       HTY-OUTBAL(2,1) +
                                       HTY-OUTBAL(2,2) +
                                       HTY-OUTBAL(2,3) +
                                       HTY-OUTBAL(2,4) +
                                       HTY-OUTBAL(2,5) +
                                       HTY-OUTBAL(2,6) +
                                       HTY-OUTBAL(2,7) +
                                       HTY-OUTBAL(2,8) +
                                       HTY-OUTBAL(2,9) +
                                       HTY-OUTBAL(2,10))
                                        * 100.
           MOVE OUTBAL-PER TO DAILY-P-OUTBAL(11,2).
           
       EXIT-NEW-PAGE3-BOTTOM-FOR-CLINT.
           EXIT.


      *******************************************
       SETUP3-SIGN SECTION.
      *******************************************
           IF DEL-UNITS-SIGN(SUBY RECCON) = " "
              MOVE "*" TO DEL-UNITS-SIGN(SUBY RECCON).
           IF DEL-OUTBAL-SIGN(SUBY RECCON) = " "
              MOVE "*" TO DEL-OUTBAL-SIGN(SUBY RECCON).

      *******************************************
       SETUP3-SIGN-PER SECTION.
      *******************************************
           IF DEL-UNITS-SIGN(SUBY RECCON) = " "
              MOVE "%" TO DEL-UNITS-SIGN(SUBY RECCON).
           IF DEL-OUTBAL-SIGN(SUBY RECCON) = " "
              MOVE "%" TO DEL-OUTBAL-SIGN(SUBY RECCON).

       SETUP3-DAILY-SIGN SECTION.
           IF DAILY-UNITS-X(SUBY RECCON) NOT = " "
              IF DAILY-UNITS-SIGN(SUBY RECCON) = " "
                 MOVE "*" TO DAILY-UNITS-SIGN(SUBY RECCON).
           IF DAILY-P-UNITS-X(SUBY RECCON) NOT = " "
              IF DAILY-P-UNITS-SIGN(SUBY RECCON) = " "
                 MOVE "%" TO DAILY-P-UNITS-SIGN(SUBY RECCON).
           IF DAILY-OUTBAL-X(SUBY RECCON) NOT = " "
              IF DAILY-OUTBAL-SIGN(SUBY RECCON) = " "
                 MOVE "*" TO DAILY-OUTBAL-SIGN(SUBY RECCON).
           IF DAILY-P-OUTBAL-X(SUBY RECCON) NOT = " "
              IF DAILY-P-OUTBAL-SIGN(SUBY RECCON) = " "
                 MOVE "%" TO DAILY-P-OUTBAL-SIGN(SUBY RECCON).

      ***********************************
      *    NEW DELINQ PAGE 3
      ***********************************
       NEW-DEL-DELINQ SECTION.
           MOVE DELINQ TO DELTYP-COD.
           IF DELINQ NOT = 5
              MOVE DEL-TYPE(DELINQ) TO DEL-BUFFER
           ELSE
              IF DIFF-PER = 0
                 MOVE DEL-TYPE(5) TO DEL-BUFFER
              ELSE
                 MOVE DEL-TYPE(6) TO DEL-BUFFER.
           MOVE DEL-BUFFER TO WR-BUF.
           MOVE DEL-FNAME TO WR-LIST.
           PERFORM CALL-WRFORM.

           MOVE " " TO MESS.
           PERFORM CALL-TMMSG.

           IF DELINQ = 1 OR 2 OR 3 OR 4 OR 5
              MOVE PROMPT-12345 TO DEL-PROMPT1
              MOVE " 30:" TO DEL-LN-PROMPT(1)
              MOVE " 60:" TO DEL-LN-PROMPT(2)
              MOVE " 90:" TO DEL-LN-PROMPT(3)
              MOVE "120:" TO DEL-LN-PROMPT(4)
              MOVE "150:" TO DEL-LN-PROMPT(5)
              MOVE "180:" TO DEL-LN-PROMPT(6)
              MOVE "210:" TO DEL-LN-PROMPT(7)
              MOVE "DEL:" TO DEL-LN-PROMPT(8)
              MOVE "<M>:" TO DEL-LN-PROMPT(9)
              MOVE "<P>:" TO DEL-LN-PROMPT(10)
              MOVE "SLOW" TO DEL-LN-PROMPT(11)
              MOVE "CUR:" TO DEL-LN-PROMPT(12)
              MOVE "TOT:" TO DEL-LN-PROMPT(13)
              GO TO NEW-DEL-DELINQ-EXIT.

           IF DELINQ = 7
              MOVE " " TO DAILY-BUF3
              MOVE DAILY-PROMPTS-NO(1) TO DAILY-PROMPT1
              MOVE DAILY-PROMPTS-NO(2) TO DAILY-PROMPT2

              MOVE DAILY-PROMPTS-NO(3) TO DAILY-PROMPT3
              MOVE DAILY-PROMPTS-NO(4) TO DAILY-PROMPT4

              MOVE DAILY-PROMPTS-NO(5) TO DAILY-PROMPT5
              IF DAILY-TOP
                 MOVE "30 :" TO DAILY-LN-PROMPT(1)
                 MOVE REM TO DAILY-LN-PROMPT(2)
                 MOVE "60 :" TO DAILY-LN-PROMPT(4)
                 MOVE REM TO DAILY-LN-PROMPT(5)
                 MOVE "90+:" TO DAILY-LN-PROMPT(7)
                 MOVE REM TO DAILY-LN-PROMPT(8)
                 MOVE "DEL:" TO DAILY-LN-PROMPT(10)
                 MOVE REM TO DAILY-LN-PROMPT(11)
                 GO TO NEW-DEL-DELINQ-EXIT
              ELSE
                 MOVE "DEL:" TO DAILY-LN-PROMPT(1)
                 MOVE REM TO DAILY-LN-PROMPT(2)
                 MOVE "<M>:" TO DAILY-LN-PROMPT(4)
                 MOVE REM TO DAILY-LN-PROMPT(5)
                 MOVE "<P>:" TO DAILY-LN-PROMPT(7)
                 MOVE REM TO DAILY-LN-PROMPT(8)
                 MOVE "SLOW" TO DAILY-LN-PROMPT(10)
                 MOVE REM TO DAILY-LN-PROMPT(11)
                 GO TO NEW-DEL-DELINQ-EXIT.

       NEW-DEL-DELINQ-EXIT.
           EXIT.

      *****************************************************************
      *    DISPLAY FIELDS PAGE 4
      *****************************************************************
       DISPLAY-FIELDS-4 SECTION.
           MOVE ACT4-TYPE(ACTIVITY) TO WR-BUF.
           MOVE ACT-FNAME TO WR-LIST.
           PERFORM CALL-WRFORM.
           IF PAGE4-TODAY
              IF PAGE4-REC OR PAGE4-NO-O2
                 MOVE ACT4-TYPET(ACTIVITY) TO WR-BUF
              ELSE
                 MOVE O2-INT-TYPE4T(ACTIVITY) TO WR-BUF
              END-IF
           ELSE
              IF PAGE4-REC OR PAGE4-NO-O2
                 MOVE ACT4-TYPEL(ACTIVITY) TO WR-BUF
              ELSE
                 MOVE O2-INT-TYPE4L(ACTIVITY) TO WR-BUF.

           MOVE ACT-FNAMEL TO WR-LIST.
           PERFORM CALL-WRFORM.
      * DISPLAY O2 INFO
           MOVE O2-ACCT(PAGE4-O2-TYPE) TO WR-BUF.
           MOVE O2-FNAME TO WR-LIST.
           PERFORM CALL-WRFORM.

           MOVE O2-REC(PAGE4-O2-TYPE) TO WR-BUF.
           MOVE O2-FNAMEL TO WR-LIST.
           PERFORM CALL-WRFORM.

           PERFORM MOVE-4-TOTALS.
           MOVE DISPLAY-BUF4 TO WR-BUF.
           MOVE DISPLAY-LIST4 TO WR-LIST.
           PERFORM CALL-WRFORM.
           MOVE " " TO MESS.
           PERFORM CALL-TMMSG.

       DISPLAY-FIELDS-EXIT-4.
           EXIT.

      *----------------------------------------------------------------
      *    MOVE TOTALS PAGE 4
      *----------------------------------------------------------------
       MOVE-4-TOTALS SECTION.

           PERFORM VARYING REPOSUB FROM 1 BY 1 UNTIL REPOSUB > 5
                      AFTER PERIOD FROM 1 BY 1 UNTIL PERIOD > 3
              IF (REPOSUB = 2 AND PERIOD = 1 AND NET-GAIN-LOSS)
                                         AND (NOT (PAGE4-TODAY))
                 MOVE 0 TO DATA-NO-REPO(REPOSUB PERIOD)
                           DATA-AMT-REPO(REPOSUB PERIOD)
              ELSE
                 MOVE SUM-NO-REPO(REPOSUB PERIOD)
                                    TO DATA-NO-REPO(REPOSUB PERIOD)
                 MOVE SUM-AMT-REPO(REPOSUB PERIOD)
                                    TO DATA-AMT-REPO(REPOSUB PERIOD)
              END-IF
           END-PERFORM.

      * COMPUTE WEIGHTED AVERAGE TERM
           MOVE 0 TO VDUNUM2.
      * COMPUTE WEIGHTED AVERAGE LOAN AMOUNT
           MOVE 0 TO VDUNUM2.

       EXIT-MOVE-4.
           EXIT.

      *****************************************************************
      *    DISPLAY FIELDS PAGE 5
      *****************************************************************
       DISPLAY-FIELDS-5 SECTION.
           MOVE " " TO DISPLAY-BUF5
                       MESS.
           PERFORM CALL-TMMSG.
           IF PAGE5-TYPE = 1
              MOVE PAGE5-TYPE1-DESC TO WR-BUF
           ELSE
              MOVE PAGE5-TYPE2-DESC TO WR-BUF.
           MOVE PAGE5-DESC-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

           PERFORM MOVE-5-TOTALS
               VARYING SUB1 FROM 1 BY 1
                              UNTIL SUB1 > 18
                   AFTER MTDYTD FROM 1 BY 1
                                UNTIL MTDYTD > 2.
           MOVE DISPLAY-BUF5 TO WR-BUF.
           MOVE DISPLAY-LIST5 TO WR-LIST.
           PERFORM CALL-WRFORM.

       DISPLAY-FIELDS-EXIT-5.
           EXIT.

      *----------------------------------------------------------------
      *    MOVE TOTALS PAGE 5
      *----------------------------------------------------------------
       MOVE-5-TOTALS SECTION.
           MOVE SUM5(SUB1 MTDYTD) TO DATA-AMT5(SUB1 MTDYTD).

       EXIT-MOVE-5.
           EXIT.


      *****************************************************************
      *    DISPLAY FIELDS PAGE 6
      *****************************************************************
       DISPLAY-FIELDS-6 SECTION.
           MOVE " " TO DISPLAY-BUF6
                       MESS.
           PERFORM CALL-TMMSG.
           IF PAGE6-TYPE = 1
              MOVE PAGE6-TYPE1-DESC TO WR-BUF
           ELSE
              MOVE PAGE6-TYPE2-DESC TO WR-BUF.
           MOVE PAGE6-DESC-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

           PERFORM MOVE-6-TOTALS.
           PERFORM MOVE-6A-TOTALS
               VARYING SUB1 FROM 1 BY 1
                              UNTIL SUB1 > 10
                   AFTER MTDYTD FROM 1 BY 1
                                UNTIL MTDYTD > 2.
           PERFORM MOVE-6B-TOTALS
               VARYING SUB1 FROM 1 BY 1
                              UNTIL SUB1 > 4
                   AFTER MTDYTD FROM 1 BY 1
                                UNTIL MTDYTD > 2.
           MOVE DISPLAY-BUF6 TO WR-BUF.
           MOVE DISPLAY-LIST6 TO WR-LIST.
           PERFORM CALL-WRFORM.

       DISPLAY-FIELDS-EXIT-6.
           EXIT.

      *----------------------------------------------------------------
      *    MOVE TOTALS PAGE 6
      *----------------------------------------------------------------
       MOVE-6-TOTALS SECTION.
           MOVE SUM6(1 1) TO DATA-AMT6(1 1).
           MOVE SUM6(1 2) TO DATA-AMT6(1 2).
           MOVE SUM6(2 1) TO DATA-AMT6(2 1).
           MOVE SUM6(2 2) TO DATA-AMT6(2 2).
       EXIT-MOVE-6.
           EXIT.

       MOVE-6A-TOTALS SECTION.
           MOVE SUM6A(SUB1 MTDYTD) TO DATA-AMT6A(SUB1 MTDYTD).
       EXIT-MOVE-6A.
           EXIT.

       MOVE-6B-TOTALS SECTION.
           MOVE SUM6B(SUB1 MTDYTD) TO DATA-AMT6B(SUB1 MTDYTD).
       EXIT-MOVE-6B.
           EXIT.

      *****************************************************************
      *    DISPLAY FIELDS PAGE 7
      *****************************************************************
       DISPLAY-FIELDS-7 SECTION.
           MOVE I-BUF7(PAGE7-TOT-TYPE) TO WR-BUF.
           MOVE I-LIST7 TO WR-LIST.
           PERFORM CALL-WRFORM.

           MOVE ACT7-TYPE(P7-ACTIVITY) TO WR-BUF.
           MOVE ACT-FNAME TO WR-LIST.
           PERFORM CALL-WRFORM.
           IF PAGE7-TODAY
              IF PAGE7-REC OR PAGE7-NO-O2
                 MOVE ACT7-TYPET(P7-ACTIVITY) TO WR-BUF
              ELSE
                 MOVE O2-INT-TYPE7T(P7-ACTIVITY) TO WR-BUF
              END-IF
           ELSE
              IF PAGE7-REC OR PAGE7-NO-O2
                 MOVE ACT7-TYPEL(P7-ACTIVITY) TO WR-BUF
              ELSE
                 MOVE O2-INT-TYPE7L(P7-ACTIVITY) TO WR-BUF.

           MOVE ACT-FNAMEL TO WR-LIST.
           PERFORM CALL-WRFORM.

      * DISPLAY O2 INFO
           MOVE O2-ACCT(PAGE7-O2-TYPE) TO WR-BUF.
           MOVE O2-FNAME TO WR-LIST.
           PERFORM CALL-WRFORM.
           MOVE O2-REC(PAGE7-O2-TYPE) TO WR-BUF.
           MOVE O2-FNAMEL TO WR-LIST.
           PERFORM CALL-WRFORM.

           PERFORM MOVE-7-TOTALS.
           MOVE DISPLAY-BUF7 TO WR-BUF.
           MOVE DISPLAY-LIST7 TO WR-LIST.
           PERFORM CALL-WRFORM.
           MOVE " " TO MESS.
           PERFORM CALL-TMMSG.

       DISPLAY-FIELDS-EXIT-7.
           EXIT.

      *----------------------------------------------------------------
      *    MOVE TOTALS PAGE 7
      *----------------------------------------------------------------
       MOVE-7-TOTALS SECTION.

           PERFORM VARYING PLSUB FROM 1 BY 1 UNTIL PLSUB > 10
                      AFTER PERIOD FROM 1 BY 1 UNTIL PERIOD > 3
              ADD SUM-NO-PL(PLSUB PERIOD)
                                    TO SUM-NO-PL(11 PERIOD)
              ADD SUM-AMT-PL(PLSUB PERIOD)
                                    TO SUM-AMT-PL(11 PERIOD)
           END-PERFORM.

           PERFORM VARYING PLSUB FROM 1 BY 1 UNTIL PLSUB > 11
                      AFTER PERIOD FROM 1 BY 1 UNTIL PERIOD > 3
              MOVE SUM-NO-PL(PLSUB PERIOD)
                                    TO DATA-NO-PL(PLSUB PERIOD)
              MOVE SUM-AMT-PL(PLSUB PERIOD)
                                    TO DATA-AMT-PL(PLSUB PERIOD)
           END-PERFORM.

           MOVE SUM-PART-CHGOFF-MTD TO DATA-PART-CHGOFF-MTD.
           MOVE SUM-PART-CHGOFF-YTD TO DATA-PART-CHGOFF-YTD.

      * COMPUTE WEIGHTED AVERAGE TERM
           MOVE 0 TO VDUNUM2.
      * COMPUTE WEIGHTED AVERAGE LOAN AMOUNT
           MOVE 0 TO VDUNUM2.

       EXIT-MOVE-7.
           EXIT.

      *****************************************************************
      *    DISPLAY FIELDS PAGE 8
      *****************************************************************
       DISPLAY-FIELDS-8 SECTION.
           MOVE " " TO DISPLAY-BUF8
                       MESS.
           PERFORM CALL-TMMSG.
           MOVE PAGE8-TYPE-DESC(PAGE8-TYPE) TO WR-BUF.
           MOVE PAGE8-DESC-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

           MOVE 0 TO PAGE-8-TOTAL1 PAGE-8-TOTAL2.
           PERFORM MOVE-8-TOTALS
               VARYING PLSUB FROM 1 BY 1 UNTIL PLSUB > 5.
           MOVE PAGE-8-TOTAL1 TO DATA-PL-TOTAL1.
           MOVE PAGE-8-TOTAL2 TO DATA-PL-TOTAL2.
           MOVE DISPLAY-BUF8 TO WR-BUF.
           MOVE DISPLAY-LIST8 TO WR-LIST.
           PERFORM CALL-WRFORM.

       DISPLAY-FIELDS-EXIT-8.
           EXIT.

      *----------------------------------------------------------------
      *    MOVE TOTALS PAGE 8
      *----------------------------------------------------------------
       MOVE-8-TOTALS SECTION.
           MOVE SUM-PL-PRINCOL(PLSUB) TO DATA-PL-PRINCOL(PLSUB).
           MOVE SUM-PL-INTCOL(PLSUB)  TO DATA-PL-INTCOL(PLSUB).
           MOVE SUM-PL-LATECOL(PLSUB) TO DATA-PL-LATECOL(PLSUB).
           MOVE SUM-PL-O2COL(PLSUB)   TO DATA-PL-O2COL(PLSUB).

           ADD SUM-PL-PRINCOL(PLSUB)  TO PAGE-8-TOTAL1.
           ADD SUM-PL-INTCOL(PLSUB)   TO PAGE-8-TOTAL2.
           ADD SUM-PL-LATECOL(PLSUB)  TO PAGE-8-TOTAL1.
           ADD SUM-PL-O2COL(PLSUB)    TO PAGE-8-TOTAL2.
       EXIT-MOVE-8.
           EXIT.

      *****************************************************************
      *    DISPLAY FIELDS PAGE 9
      *****************************************************************
       DISPLAY-FIELDS-9 SECTION.
           MOVE " " TO DISPLAY-BUF9
                       MESS.
           MOVE ACT9-TYPE(ACTIVITY) TO WR-BUF.
           MOVE ACT-FNAME TO WR-LIST.
           PERFORM CALL-WRFORM.

           MOVE ACT9-TYPE2(ACTIVITY) TO WR-BUF.
           MOVE ACT-FNAME2 TO WR-LIST.
           PERFORM CALL-WRFORM.

           PERFORM CALL-TMMSG.
           IF PAGE9-TODAY
              MOVE ACT9-TYPET(ACTIVITY) TO WR-BUF
           ELSE
              MOVE ACT9-TYPEL(ACTIVITY) TO WR-BUF.

           MOVE ACT-FNAMEL TO WR-LIST.
           PERFORM CALL-WRFORM.

           PERFORM MOVE-9-TOTALS
               VARYING DLRV FROM 1 BY 1 UNTIL DLRV > 3
                 AFTER PERIOD FROM 1 BY 1 UNTIL PERIOD > 3.
           MOVE DISPLAY-BUF9 TO WR-BUF.
           MOVE DISPLAY-LIST9 TO WR-LIST.
           PERFORM CALL-WRFORM.

       DISPLAY-FIELDS-EXIT-9.
           EXIT.

      *----------------------------------------------------------------
      *    MOVE TOTALS PAGE 9
      *----------------------------------------------------------------
       MOVE-9-TOTALS SECTION.
           MOVE SUM-DLRVPRIN(DLRV PERIOD) TO DATA-DLRVPRIN(DLRV PERIOD).
           MOVE SUM-DLRVCHGS(DLRV PERIOD)  TO DATA-DLRVCHGS(DLRV PERIOD).
           IF PERIOD = 1
              MOVE SUM-DLPARVHELD(DLRV) TO DATA-DLPARVHELD(DLRV)
              MOVE SUM-DLTOTAL(DLRV) TO DATA-DLTOTAL(DLRV)
              MOVE SUM-DLDISC(DLRV) TO DATA-DLDISC(DLRV).
       EXIT-MOVE-9.
           EXIT.



      *****************************************************************
      *    DISPLAY FIELDS PAGE 10                   WAS PAGE 11
      *****************************************************************
       DISPLAY-FIELDS-10 SECTION.                  
           MOVE ACT-TYPE-MTD-YTD(ACTIVITY) TO WR-BUF.
           MOVE ACT-FNAME TO WR-LIST.
           PERFORM CALL-WRFORM.

           PERFORM MOVE-10-TOTALS
               VARYING SUB4   FROM 1 BY 1 UNTIL SUB4   > 14.

           MOVE DISPLAY-BUF10 TO WR-BUF.
           MOVE DISPLAY-LIST10 TO WR-LIST.
           PERFORM CALL-WRFORM.
           MOVE " " TO MESS.
           PERFORM CALL-TMMSG.

       DISPLAY-FIELDS-EXIT-10.
           EXIT.

      *----------------------------------------------------------------
      *    MOVE TOTALS PAGE 10                       WAS PAGE 11
      *----------------------------------------------------------------
       MOVE-10-TOTALS SECTION.                      
           MOVE SUM-INS-NO-ELIG(SUB4)   TO
                                  DATA-INS-NO-ELIG(SUB4).
           MOVE SUM-INS-NO-WROTE(SUB4)   TO
                                  DATA-INS-NO-WROTE(SUB4).
           MOVE SUM-INS-NO-NOTWROTE(SUB4) TO
                                  DATA-INS-NO-NOTWROTE(SUB4).
           MOVE SUM-INS-AMT-WROTE(SUB4) TO
                                  DATA-INS-AMT-WROTE(SUB4).
       EXIT-MOVE-10-TOTALS.
           EXIT.

      *****************************************************************
      *    DISPLAY FIELDS PAGE 11                    WAS PAGE 12
      *****************************************************************
       DISPLAY-FIELDS-11 SECTION.           
           MOVE ACT-TYPE(ACTIVITY)  TO WR-BUF.
           MOVE ACT-FNAME TO WR-LIST.
           PERFORM CALL-WRFORM.
           MOVE PAGE11-PROMPT(PAGE11-TYPE) TO WR-BUF.
           MOVE PAGE11-PMPT-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

           PERFORM VARYING PERIOD FROM 1 BY 1 UNTIL PERIOD > 3
                     AFTER TYPSUB FROM 1 BY 1 UNTIL TYPSUB > 10
              MOVE SUM-UNEARNED-AMT (PERIOD ACTIVITY TYPSUB) TO
                   DISP-UNEARNED-AMT(PERIOD TYPSUB)
           END-PERFORM.

           MOVE DISPLAY-BUF11 TO WR-BUF.
           MOVE DISPLAY-LIST11 TO WR-LIST.
           PERFORM CALL-WRFORM.
           MOVE " " TO MESS.
           PERFORM CALL-TMMSG.

       DISPLAY-FIELDS-EXIT-11.
           EXIT.

      *****************************************************************
      *    DISPLAY FIELDS PAGE 12                   WAS PAGE 13
      *****************************************************************
       DISPLAY-FIELDS-12 SECTION.       
           MOVE SUM-ELM-OUTS-BALANCE        TO DATA-ELM-OUTS-BALANCE.
           MOVE SUM-ELY-OUTS-BALANCE        TO DATA-ELY-OUTS-BALANCE.
           PERFORM VARYING PERIOD FROM 1 BY 1 UNTIL PERIOD > 2
              MOVE SUM-PROFIT(PERIOD)       TO DATA-PROFIT(PERIOD)
              MOVE SUM-INCOME(PERIOD)       TO DATA-INCOME(PERIOD)
              MOVE SUM-INT-INCOME(PERIOD)   TO DATA-INT-INCOME(PERIOD)
              MOVE SUM-INS-INCOME(PERIOD)   TO DATA-INS-INCOME(PERIOD)
              MOVE SUM-EXPENSE(PERIOD)      TO DATA-EXPENSE(PERIOD)
              MOVE SUM-PERSONNEL(PERIOD)    TO DATA-PERSONNEL(PERIOD)
              IF SUM-NO-LOANS(PERIOD) NOT = 0
                 COMPUTE WORK1 ROUNDED =
                    SUM-EXPENSE(PERIOD) / SUM-NO-LOANS(PERIOD)
              ELSE
                 MOVE 0 TO WORK1
              END-IF
              MOVE WORK1                 TO DATA-EXP-PER-LOAN(PERIOD)
              IF SUM-INCOME(PERIOD) NOT = 0
                 COMPUTE WORK1 ROUNDED =
                    SUM-EXPENSE(PERIOD) / SUM-INCOME(PERIOD) * 100
              ELSE
                 MOVE 0 TO WORK1
              END-IF
              MOVE WORK1                 TO DATA-EXP-TO-INCOME(PERIOD)
              IF SUM-INCOME(PERIOD) NOT = 0
                 COMPUTE WORK1 ROUNDED =
                    SUM-PERSONNEL(PERIOD) / SUM-INCOME(PERIOD) * 100
              ELSE
                 MOVE 0 TO WORK1
              END-IF
              MOVE WORK1                 TO DATA-PERS-TO-INCOME(PERIOD)
           END-PERFORM.

           MOVE SUM-MGR-BONUS            TO DATA-MGR-BONUS(1)
           MOVE SUM-BSR-BONUS            TO DATA-BSR-BONUS(1)

           IF SUM-ELM-OUTS-BALANCE NOT = 0
              COMPUTE PERCENT-WK-2 ROUNDED =
                 SUM-PROFIT(1) / SUM-ELM-OUTS-BALANCE * 100
           ELSE
              MOVE 0 TO PERCENT-WK-2.
           MOVE PERCENT-WK-2                TO DATA-PROFIT-PCT(1).
           IF GROUP-FLAG = "N"
              MOVE SUM-YTD-PCT-PROFIT TO PERCENT-WK-2
           ELSE
              IF SUM-ELY-OUTS-BALANCE NOT = 0
                 COMPUTE PERCENT-WK-2 ROUNDED =
                    SUM-PROFIT(2) / SUM-ELY-OUTS-BALANCE * 100
              ELSE
                 MOVE 0 TO PERCENT-WK-2.
           MOVE PERCENT-WK-2                TO DATA-PROFIT-PCT(2).

           IF SUM-ELM-OUTS-BALANCE NOT = 0
              COMPUTE PERCENT-WK-2 ROUNDED =
                 SUM-INCOME(1) / SUM-ELM-OUTS-BALANCE * 100
           ELSE
              MOVE 0 TO PERCENT-WK-2.
           MOVE PERCENT-WK-2                TO DATA-INCOME-PCT(1).
           IF GROUP-FLAG = "N"
              MOVE SUM-YTD-PCT-INCOME TO PERCENT-WK-2
           ELSE
              IF SUM-ELY-OUTS-BALANCE NOT = 0
                 COMPUTE PERCENT-WK-2 ROUNDED =
                    SUM-INCOME(2) / SUM-ELY-OUTS-BALANCE * 100
              ELSE
                 MOVE 0 TO PERCENT-WK-2.
           MOVE PERCENT-WK-2                TO DATA-INCOME-PCT(2).

           IF SUM-ELM-OUTS-BALANCE NOT = 0
              COMPUTE PERCENT-WK-2 ROUNDED =
                 SUM-INT-INCOME(1) / SUM-ELM-OUTS-BALANCE * 100
           ELSE
              MOVE 0 TO PERCENT-WK-2.
           MOVE PERCENT-WK-2                TO DATA-INT-INCOME-PCT(1).
           IF GROUP-FLAG = "N"
              MOVE SUM-YTD-PCT-INT-INC TO PERCENT-WK-2
           ELSE
              IF SUM-ELY-OUTS-BALANCE NOT = 0
                 COMPUTE PERCENT-WK-2 ROUNDED =
                    SUM-INT-INCOME(2) / SUM-ELY-OUTS-BALANCE * 100
              ELSE
                 MOVE 0 TO PERCENT-WK-2.
           MOVE PERCENT-WK-2                TO DATA-INT-INCOME-PCT(2).

           IF SUM-ELM-OUTS-BALANCE NOT = 0
              COMPUTE PERCENT-WK-2 ROUNDED =
                 SUM-INS-INCOME(1) / SUM-ELM-OUTS-BALANCE * 100
           ELSE
              MOVE 0 TO PERCENT-WK-2.
           MOVE PERCENT-WK-2                TO DATA-INS-INCOME-PCT(1).
           IF GROUP-FLAG = "N"
              MOVE SUM-YTD-PCT-INS-INC TO PERCENT-WK-2
           ELSE
              IF SUM-ELY-OUTS-BALANCE NOT = 0
                 COMPUTE PERCENT-WK-2 ROUNDED =
                    SUM-INS-INCOME(2) / SUM-ELY-OUTS-BALANCE * 100
              ELSE
                 MOVE 0 TO PERCENT-WK-2.
           MOVE PERCENT-WK-2                TO DATA-INS-INCOME-PCT(2).

           IF SUM-ELM-OUTS-BALANCE NOT = 0
              COMPUTE PERCENT-WK-2 ROUNDED =
                 SUM-EXPENSE(1) / SUM-ELM-OUTS-BALANCE * 100
           ELSE
              MOVE 0 TO PERCENT-WK-2.
           MOVE PERCENT-WK-2                TO DATA-EXP-TO-OUTS(1).
           IF GROUP-FLAG = "N"
              MOVE SUM-YTD-PCT-EXPENSE      TO PERCENT-WK-2
           ELSE
              IF SUM-ELY-OUTS-BALANCE NOT = 0
                 COMPUTE PERCENT-WK-2 ROUNDED =
                    SUM-EXPENSE(2) / SUM-ELY-OUTS-BALANCE * 100
              ELSE
                 MOVE 0 TO PERCENT-WK-2.
           MOVE PERCENT-WK-2                TO DATA-EXP-TO-OUTS(2).

           MOVE DISPLAY-BUF12 TO WR-BUF.
           MOVE DISPLAY-LIST12 TO WR-LIST.
           PERFORM CALL-WRFORM.
           MOVE " " TO MESS.
           PERFORM CALL-TMMSG.

       DISPLAY-FIELDS-EXIT-12.
           EXIT.

      *********************************
      *    CLASS SCAN
      *********************************
       CLASS-SCAN SECTION.
           MOVE 0      TO LCSCAN-KEY.
           MOVE SPACES TO LCSCAN-VERIFY.
           MOVE "WI/LCSCAN" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH LCSCAN-BUF EXIT-PATHNAME.
           CANCEL FORM-PROGX.
           IF LCSCAN-STATUS NOT = "00" OR LCSCAN-KEY = 0
              MOVE 9 TO IO-FG
              GO TO EXIT-CLASS-SCAN.

           MOVE "S  A020LC." TO VDU.
           MOVE LCSCAN-KEY TO VDUBUF.
           PERFORM SCREENIO.
           MOVE 0 TO IO-FG.

       EXIT-CLASS-SCAN.
           EXIT.

      ***************************************************
      *TEST CLASS IF SALES FINANCE OR LOAN CLASSES SELECTED
      *  NOTE - THIS ROUTINE NOT PERFORMED IF ONE-CLASS
      *  OTHERWISE, FLAG WILL MAKE US SKIP SERVICE ACCTS
      ***************************************************
       TEST-CLASS SECTION.
       READ-CLASS.
           MOVE "N" TO CORRECT-CLASS.
           MOVE BY-CLASS TO LC-CODE.
           PERFORM OPEN-LC1-FILE.
           PERFORM READ-LC1-FILE.
           PERFORM CLOSE-LC1-FILE.
           IF IO-BAD
              GO TO EXIT-TEST-CLASS.
           IF SF-CLASSES
              IF LC-SALEFIN = "Y"
                 IF LC-AFP-CLASS NOT = "Y"
                    MOVE "Y" TO CORRECT-CLASS.
      * CLASS 1, 2, 3, 4 AND 22
           IF WORLD-SMALL-CLASSES
              IF (LC-CODE < 5 OR LC-CODE = 22)
                 MOVE "Y" TO CORRECT-CLASS.
           IF LN-CLASSES
              IF LC-SALEFIN = "N" AND LC-GIFTNET NOT = "Y"
                 MOVE "Y" TO CORRECT-CLASS.
           IF SMALL-LN-CLASSES
              IF LC-SMALLOAN = "Y"
                 MOVE "Y" TO CORRECT-CLASS.
           IF NON-SMALL-LN-CLASSES
              IF LC-SMALLOAN = "N" AND LC-SALEFIN = "N"
                 MOVE "Y" TO CORRECT-CLASS.
           IF IB-LN-CLASSES
              IF LC-LOANTYPE = "I" AND LC-SALEFIN NOT = "Y"
                 MOVE "Y" TO CORRECT-CLASS.

           IF WCBC-CLASSES
              IF LC-GIFTNET = "Y"
                 MOVE "Y" TO CORRECT-CLASS.
      * CLASSES 5 - 21
           IF WORLD-OTHER-CLASSES
              IF LC-CODE > 4
                 IF LC-CODE < 22
                    MOVE "Y" TO CORRECT-CLASS.
           IF WORLD-SF-CLASSES
              IF LC-GIFTNET = "Y" OR LC-CODE = 30
                 MOVE "Y" TO CORRECT-CLASS.
           IF ALL-CLASSES
              MOVE "Y" TO CORRECT-CLASS.
           IF LC-SERVACCT = "Y"
              MOVE "N" TO CORRECT-CLASS.
       EXIT-TEST-CLASS.
           EXIT.

      ***************************************************
      *   ENTER PASSWORD IF MODIFYING/ADDING/DELETING
      ***************************************************
       VERIFY-PASSWORD SECTION.
           MOVE " " TO PASSWDW-STAT.
           MOVE "STATS" TO PASSWDW-PGM.
           MOVE SAVE-LEVEL TO PASSWDW-SEQ.
           MOVE "PLEASE ENTER THE PASSWORD," TO PASSWDW-PROMPT1.
           MOVE "TO VIEW THESE BRANCHES:" TO PASSWDW-PROMPT2.
           MOVE "CL/PASSWD" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH PASSWDW-BUF.
           CANCEL FORM-PROGX.

      *----------------------------------------------------------------
      *  ADD BY TO HBY SETION
      *
      *  ACCUMULATES BY RECORDS INTO A HOLD RECORD HBY-REC.
      *----------------------------------------------------------------
       ADD-BY-TO-HBY SECTION.
           PERFORM ADD-BY-PERIOD-TABLES
               VARYING PERIOD FROM 1 BY 1 UNTIL PERIOD > 3
                 AFTER ACTSUB FROM 1 BY 1 UNTIL ACTSUB > 2
                 AFTER TYPSUB FROM 1 BY 1 UNTIL TYPSUB > 11.

           PERFORM ADD-BY-MTD-YTD-TABLE
              VARYING MTDYTD FROM 1 BY 1 UNTIL MTDYTD > 2.

           PERFORM VARYING PERIOD FROM 1 BY 1 UNTIL PERIOD > 3
                     AFTER ACTSUB FROM 1 BY 1 UNTIL ACTSUB > 2
                     AFTER TYPSUB FROM 1 BY 1 UNTIL TYPSUB > 10
              ADD BY-UNEARNED-AMT(PERIOD ACTSUB TYPSUB) TO
                              HBY-UNEARNED-AMT(PERIOD ACTSUB TYPSUB)
           END-PERFORM.

           ADD BY-PROCEEDS(1) TO HBY-PROCEEDS(1).
           ADD BY-PROCEEDS(2) TO HBY-PROCEEDS(2).

           ADD BY-SUMWGTTM TO  HBY-SUMWGTTM
           ADD BY-SUMWGTAV TO  HBY-SUMWGTAV
           ADD BY-SUMWGTTA TO  HBY-SUMWGTTA
           ADD BY-IB-ACCRUED TO  HBY-IB-ACCRUED

           GO TO EXIT-ADD-BY-TO-HBY.

      *----------------------------------------------------------------
      *    ACCUMULATE ACTIVITY.
      *----------------------------------------------------------------
       ADD-BY-PERIOD-TABLES.
           ADD BY-NO(PERIOD ACTSUB TYPSUB) TO
              HBY-NO(PERIOD ACTSUB TYPSUB).
           ADD BY-AMT(PERIOD ACTSUB TYPSUB) TO
              HBY-AMT(PERIOD ACTSUB TYPSUB).
           ADD BY-O2(PERIOD ACTSUB TYPSUB) TO
              HBY-O2(PERIOD ACTSUB TYPSUB).
           ADD BY-INT(PERIOD ACTSUB TYPSUB) TO
              HBY-INT(PERIOD ACTSUB TYPSUB).

      *- - - - - - - - - - - - - - - - - - - - ADD-BY-APP-PERIOD-TABLES
      *    ADD  BY-NO-PENDING(PERIOD ACTSUB TYPSUB) TO
      *        HBY-NO-PENDING(PERIOD ACTSUB TYPSUB).

      *    ADD  BY-NO-COND-APP(PERIOD ACTSUB TYPSUB) TO
      *        HBY-NO-COND-APP(PERIOD ACTSUB TYPSUB).

      *    ADD  BY-NO-APPROVED(PERIOD ACTSUB TYPSUB) TO
      *        HBY-NO-APPROVED(PERIOD ACTSUB TYPSUB).

      *    ADD  BY-NO-MADE(PERIOD ACTSUB TYPSUB) TO
      *        HBY-NO-MADE(PERIOD ACTSUB TYPSUB).

      *    ADD  BY-NO-TURNDOWN(PERIOD ACTSUB TYPSUB) TO
      *        HBY-NO-TURNDOWN(PERIOD ACTSUB TYPSUB).

      *    ADD  BY-NO-TURNUSDOWN(PERIOD ACTSUB TYPSUB) TO
      *        HBY-NO-TURNUSDOWN(PERIOD ACTSUB TYPSUB).

      *----------------------------------------------------------------
      *    ACCUMULATE P&L AND DELINQUENCY ACTIVITY.
      *----------------------------------------------------------------
           IF TYPSUB < 11
              ADD BY-NO-PL(PERIOD ACTSUB TYPSUB) TO
                 HBY-NO-PL(PERIOD ACTSUB TYPSUB)
              ADD BY-AMT-PL(PERIOD ACTSUB TYPSUB) TO
                 HBY-AMT-PL(PERIOD ACTSUB TYPSUB)
              ADD BY-O2-PL(PERIOD ACTSUB TYPSUB) TO
                 HBY-O2-PL(PERIOD ACTSUB TYPSUB)
              ADD BY-INT-PL(PERIOD ACTSUB TYPSUB) TO
                 HBY-INT-PL(PERIOD ACTSUB TYPSUB)
      * ACCUM DELINQ:
              ADD BY-UNITS(PERIOD ACTSUB TYPSUB) TO
                 HBY-UNITS(PERIOD ACTSUB TYPSUB)
              ADD BY-OUTBAL(PERIOD ACTSUB TYPSUB) TO
                 HBY-OUTBAL(PERIOD ACTSUB TYPSUB)
      * ACCUM BANKRUPT DELINQ:
              ADD BY-UNITS-BNKRPT(PERIOD ACTSUB TYPSUB) TO
                 HBY-UNITS-BNKRPT(PERIOD ACTSUB TYPSUB)
              ADD BY-OUTBAL-BNKRPT(PERIOD ACTSUB TYPSUB) TO
                 HBY-OUTBAL-BNKRPT(PERIOD ACTSUB TYPSUB).
      *----------------------------------------------------------------
      *    ACCUMULATE REPO ACTIVITY.
      *----------------------------------------------------------------
           IF TYPSUB < 6
              ADD BY-NO-REPO(PERIOD ACTSUB TYPSUB) TO
                 HBY-NO-REPO(PERIOD ACTSUB TYPSUB)
              ADD BY-AMT-REPO(PERIOD ACTSUB TYPSUB) TO
                 HBY-AMT-REPO(PERIOD ACTSUB TYPSUB)
              ADD BY-O2-REPO(PERIOD ACTSUB TYPSUB) TO
                 HBY-O2-REPO(PERIOD ACTSUB TYPSUB)
              ADD BY-INT-REPO(PERIOD ACTSUB TYPSUB) TO
                 HBY-INT-REPO(PERIOD ACTSUB TYPSUB).
      *----------------------------------------------------------------
      *    ACCUMULATE DEALER RESERVES.
      *----------------------------------------------------------------
           IF TYPSUB < 4
              ADD BY-DLRVPRIN(PERIOD ACTSUB TYPSUB) TO
                 HBY-DLRVPRIN(PERIOD ACTSUB TYPSUB)
              ADD BY-DLRVCHGS(PERIOD ACTSUB TYPSUB) TO
                 HBY-DLRVCHGS(PERIOD ACTSUB TYPSUB)
              IF TYPSUB = 1
                 ADD BY-DLPARVHELD(PERIOD ACTSUB) TO
                    HBY-DLPARVHELD(PERIOD ACTSUB)
                 IF ACTSUB = 2
                    ADD BY-DLDISC(PERIOD) TO HBY-DLDISC(PERIOD).

      *-----------------------------------------------------------------
       ADD-BY-MTD-YTD-TABLE.
           PERFORM VARYING PLSUB FROM 1 BY 1 UNTIL PLSUB > 5
              ADD BY-PL-PRINCOL(MTDYTD PLSUB)
                               TO  HBY-PL-PRINCOL(MTDYTD PLSUB)
              ADD BY-PL-INTCOL(MTDYTD PLSUB)
                               TO  HBY-PL-INTCOL(MTDYTD PLSUB)
              ADD BY-PL-LATECOL(MTDYTD PLSUB)
                               TO  HBY-PL-LATECOL(MTDYTD PLSUB)
              ADD BY-PL-O2COL(MTDYTD PLSUB)
                               TO  HBY-PL-O2COL(MTDYTD PLSUB)
           END-PERFORM.

              ADD BY-AMT-SOLD(MTDYTD)  TO  HBY-AMT-SOLD(MTDYTD).
              ADD BY-AMT-TRANS(MTDYTD) TO  HBY-AMT-TRANS(MTDYTD).
              ADD BY-AMT-PURCH(MTDYTD) TO  HBY-AMT-PURCH(MTDYTD).

              ADD BY-NO-SOLD(MTDYTD)  TO  HBY-NO-SOLD(MTDYTD).
              ADD BY-NO-TRANS(MTDYTD) TO  HBY-NO-TRANS(MTDYTD).
              ADD BY-NO-PURCH(MTDYTD) TO  HBY-NO-PURCH(MTDYTD).

              ADD BY-OTHCOL(MTDYTD)  TO HBY-OTHCOL(MTDYTD).
              ADD BY-OT2COL(MTDYTD)  TO HBY-OT2COL(MTDYTD).
              ADD BY-INTCOL(MTDYTD)  TO HBY-INTCOL(MTDYTD).
              ADD BY-LATECOL(MTDYTD) TO HBY-LATECOL(MTDYTD).
              ADD BY-PRINCOL(MTDYTD) TO HBY-PRINCOL(MTDYTD).
              ADD BY-DEFCOL(MTDYTD)  TO HBY-DEFCOL(MTDYTD).
              ADD BY-RNPRIN(MTDYTD)  TO HBY-RNPRIN(MTDYTD).
              ADD BY-INTREB(MTDYTD)  TO HBY-INTREB(MTDYTD).
              ADD BY-CLREB1(MTDYTD)  TO HBY-CLREB1(MTDYTD).
              ADD BY-CLREB2(MTDYTD)  TO HBY-CLREB2(MTDYTD).
              ADD BY-CLREB3(MTDYTD)  TO HBY-CLREB3(MTDYTD).
              ADD BY-CLREB4(MTDYTD)  TO HBY-CLREB4(MTDYTD).
              ADD BY-AHREB(MTDYTD)   TO HBY-AHREB(MTDYTD).
              ADD BY-PPREB(MTDYTD)   TO HBY-PPREB(MTDYTD).
              ADD BY-O1REB(MTDYTD)   TO HBY-O1REB(MTDYTD).
              ADD BY-O2REB(MTDYTD)   TO HBY-O2REB(MTDYTD).
              ADD BY-O3REB(MTDYTD)   TO HBY-O3REB(MTDYTD).
              ADD BY-O4REB(MTDYTD)   TO HBY-O4REB(MTDYTD).
              ADD BY-O5REB(MTDYTD)   TO HBY-O5REB(MTDYTD).
              ADD BY-O6REB(MTDYTD)   TO HBY-O6REB(MTDYTD).
              ADD BY-O7REB(MTDYTD)   TO HBY-O7REB(MTDYTD).
              ADD BY-SCREB(MTDYTD)   TO HBY-SCREB(MTDYTD).
              ADD BY-MFREB(MTDYTD)   TO HBY-MFREB(MTDYTD).
              ADD BY-DFREB(MTDYTD)   TO HBY-DFREB(MTDYTD).
              ADD BY-PART-CHGOFF(MTDYTD) TO HBY-PART-CHGOFF(MTDYTD).
              ADD BY-ALLOTMENTS(MTDYTD)  TO HBY-ALLOTMENTS(MTDYTD).
              ADD BY-LOCKBOX(MTDYTD)     TO HBY-LOCKBOX(MTDYTD).

              PERFORM VARYING INSUR FROM 1 BY 1 UNTIL INSUR > 10
                 ADD BY-INS-NO-ELIG(MTDYTD INSUR)
                                   TO HBY-INS-NO-ELIG(MTDYTD INSUR)
                 ADD BY-INS-NO-WROTE(MTDYTD INSUR)
                                   TO HBY-INS-NO-WROTE(MTDYTD INSUR)
                 ADD BY-INS-NO-NOTWROTE(MTDYTD INSUR)
                                   TO HBY-INS-NO-NOTWROTE(MTDYTD INSUR)
                 ADD BY-INS-AMT-WROTE(MTDYTD INSUR)
                                   TO HBY-INS-AMT-WROTE(MTDYTD INSUR)
              END-PERFORM.

              PERFORM VARYING INSUR FROM 1 BY 1 UNTIL INSUR > 3
                 ADD BY-FEE-NO-ELIG(MTDYTD INSUR)
                                   TO HBY-FEE-NO-ELIG(MTDYTD INSUR)
                 ADD BY-FEE-NO-WROTE(MTDYTD INSUR)
                                   TO HBY-FEE-NO-WROTE(MTDYTD INSUR)
                 ADD BY-FEE-NO-NOTWROTE(MTDYTD INSUR)
                                   TO HBY-FEE-NO-NOTWROTE(MTDYTD INSUR)
                 ADD BY-FEE-AMT-WROTE(MTDYTD INSUR)
                                   TO HBY-FEE-AMT-WROTE(MTDYTD INSUR)
              END-PERFORM.

              ADD BY-OTH-NO-COL(MTDYTD)  TO HBY-OTH-NO-COL(MTDYTD).
              ADD BY-OT2-NO-COL(MTDYTD)  TO HBY-OT2-NO-COL(MTDYTD).
              ADD BY-INT-NO-COL(MTDYTD)  TO HBY-INT-NO-COL(MTDYTD).
              ADD BY-LATE-NO-COL(MTDYTD) TO HBY-LATE-NO-COL(MTDYTD).

              ADD BY-OTH-NO-WAIVED(MTDYTD)
                                         TO HBY-OTH-NO-WAIVED(MTDYTD).
              ADD BY-OT2-NO-WAIVED(MTDYTD)
                                         TO HBY-OT2-NO-WAIVED(MTDYTD).
              ADD BY-INT-NO-WAIVED(MTDYTD)
                                         TO HBY-INT-NO-WAIVED(MTDYTD).
              ADD BY-LATE-NO-WAIVED(MTDYTD)
                                         TO HBY-LATE-NO-WAIVED(MTDYTD).

              ADD BY-OTH-AMT-WAIVED(MTDYTD)
                                         TO HBY-OTH-AMT-WAIVED(MTDYTD).
              ADD BY-OT2-AMT-WAIVED(MTDYTD)
                                         TO HBY-OT2-AMT-WAIVED(MTDYTD).
              ADD BY-INT-AMT-WAIVED(MTDYTD)
                                         TO HBY-INT-AMT-WAIVED(MTDYTD).
              ADD BY-LATE-AMT-WAIVED(MTDYTD)
                                         TO HBY-LATE-AMT-WAIVED(MTDYTD).
      *----------------------------------------------------------------
       EXIT-ADD-BY-TO-HBY.
              EXIT.

      *----------------------------------------------------------------
      * ADD TY TO HTY SECTION
      *  ACCUMULATES TY RECORDS INTO A HOLD TY-REC (HTY-REC).
      *----------------------------------------------------------------
       ADD-TY-TO-HTY SECTION.
           PERFORM ADD-TY-ACTIVITY-TABLES
              VARYING SUB1 FROM 1 BY 1 UNTIL SUB1 > 2
                AFTER SUB2 FROM 1 BY 1 UNTIL SUB2 > 11.

           PERFORM VARYING ACTSUB FROM 1 BY 1 UNTIL ACTSUB > 2
                     AFTER TYPSUB FROM 1 BY 1 UNTIL TYPSUB > 10
              ADD TY-UNEARNED-AMT(ACTSUB TYPSUB) TO
                               HTY-UNEARNED-AMT(ACTSUB TYPSUB)
           END-PERFORM.

           ADD TY-PROCEEDS TO HTY-PROCEEDS.

           ADD TY-AMT-SOLD TO HTY-AMT-SOLD.
           ADD TY-AMT-TRANS TO HTY-AMT-TRANS.
           ADD TY-AMT-PURCH TO HTY-AMT-PURCH.

           ADD TY-NO-SOLD TO HTY-NO-SOLD.
           ADD TY-NO-TRANS TO HTY-NO-TRANS.
           ADD TY-NO-PURCH TO HTY-NO-PURCH.

           ADD TY-OTHCOL TO HTY-OTHCOL.
           ADD TY-OT2COL TO HTY-OT2COL.
           ADD TY-INTCOL TO HTY-INTCOL.
           ADD TY-LATECOL TO HTY-LATECOL.
           ADD TY-PRINCOL TO HTY-PRINCOL.
           ADD TY-DEFCOL TO HTY-DEFCOL.
           ADD TY-RNPRIN TO HTY-RNPRIN.
           ADD TY-INTREB TO HTY-INTREB.
           ADD TY-CLREB1 TO HTY-CLREB1.
           ADD TY-CLREB2 TO HTY-CLREB2.
           ADD TY-CLREB3 TO HTY-CLREB3.
           ADD TY-CLREB4 TO HTY-CLREB4.
           ADD TY-AHREB TO HTY-AHREB.
           ADD TY-PPREB TO HTY-PPREB.
           ADD TY-O1REB TO HTY-O1REB.
           ADD TY-O2REB TO HTY-O2REB.
           ADD TY-O3REB TO HTY-O3REB.
           ADD TY-O4REB TO HTY-O4REB.
           ADD TY-O5REB TO HTY-O5REB.
           ADD TY-O6REB TO HTY-O6REB.
           ADD TY-O7REB TO HTY-O7REB.
           ADD TY-SCREB TO HTY-SCREB.
           ADD TY-MFREB TO HTY-MFREB.
           ADD TY-DFREB TO HTY-DFREB.
           ADD TY-PART-CHGOFF TO HTY-PART-CHGOFF.
           PERFORM VARYING SUB1 FROM 1 BY 1 UNTIL SUB1 > 10
              ADD TY-INS-NO-ELIG(SUB1) TO HTY-INS-NO-ELIG(SUB1)
              ADD TY-INS-NO-WROTE(SUB1) TO HTY-INS-NO-WROTE(SUB1)
              ADD TY-INS-NO-NOTWROTE(SUB1) TO HTY-INS-NO-NOTWROTE(SUB1)
              ADD TY-INS-AMT-WROTE(SUB1) TO HTY-INS-AMT-WROTE(SUB1)
           END-PERFORM.

           ADD TY-OTH-NO-COL TO HTY-OTH-NO-COL.
           ADD TY-OT2-NO-COL TO HTY-OT2-NO-COL.
           ADD TY-INT-NO-COL TO HTY-INT-NO-COL.
           ADD TY-LATE-NO-COL TO HTY-LATE-NO-COL.

           ADD TY-OTH-NO-WAIVED TO HTY-OTH-NO-WAIVED.
           ADD TY-OT2-NO-WAIVED TO HTY-OT2-NO-WAIVED.
           ADD TY-INT-NO-WAIVED TO HTY-INT-NO-WAIVED.
           ADD TY-LATE-NO-WAIVED TO HTY-LATE-NO-WAIVED.

           ADD TY-OTH-AMT-WAIVED TO HTY-OTH-AMT-WAIVED.
           ADD TY-OT2-AMT-WAIVED TO HTY-OT2-AMT-WAIVED.
           ADD TY-INT-AMT-WAIVED TO HTY-INT-AMT-WAIVED.
           ADD TY-LATE-AMT-WAIVED TO HTY-LATE-AMT-WAIVED.

           PERFORM VARYING SUB1 FROM 1 BY 1 UNTIL SUB1 > 8
              ADD TY-CKWRITTEN(SUB1) TO HTY-CKWRITTEN(SUB1)
           END-PERFORM.

           ADD TY-SUMWGTTM TO HTY-SUMWGTTM.
           ADD TY-SUMWGTAV TO HTY-SUMWGTAV.
           ADD TY-SUMWGTTA TO HTY-SUMWGTTA.
           ADD TY-ALLOTMENTS TO HTY-ALLOTMENTS.
           ADD TY-LOCKBOX    TO HTY-LOCKBOX.
           ADD TY-DLDISC   TO HTY-DLDISC.

           GO TO EXIT-ADD-TY-TO-HTY.

      *---------------------------------------------------------------
       ADD-TY-ACTIVITY-TABLES.
           ADD TY-NO(SUB1 SUB2) TO  HTY-NO(SUB1 SUB2).
           ADD TY-AMT(SUB1 SUB2) TO  HTY-AMT(SUB1 SUB2).
           ADD TY-O2(SUB1 SUB2) TO  HTY-O2(SUB1 SUB2).
           ADD TY-INT(SUB1 SUB2) TO  HTY-INT(SUB1 SUB2).

      *- - - - - - - - - - - - - - - - - - - ADD-BY-APP-ACTIVITY-TABLES
      *    ADD TY-NO-PENDING(SUB1 SUB2)  TO HTY-NO-PENDING(SUB1 SUB2).
      *    ADD TY-NO-COND-APP(SUB1 SUB2) TO HTY-NO-COND-APP(SUB1 SUB2).
      *    ADD TY-NO-APPROVED(SUB1 SUB2) TO HTY-NO-APPROVED(SUB1 SUB2).
      *    ADD TY-NO-MADE    (SUB1 SUB2) TO HTY-NO-MADE(SUB1 SUB2).
      *    ADD TY-NO-TURNDOWN(SUB1 SUB2) TO HTY-NO-TURNDOWN(SUB1 SUB2).
      *    ADD TY-NO-TURNUSDOWN(SUB1 SUB2) TO
      *        HTY-NO-TURNUSDOWN(SUB1 SUB2).

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF SUB2 < 11
              ADD TY-NO-PL(SUB1 SUB2) TO HTY-NO-PL(SUB1 SUB2)
              ADD TY-AMT-PL(SUB1 SUB2) TO HTY-AMT-PL(SUB1 SUB2)
              ADD TY-O2-PL(SUB1 SUB2) TO HTY-O2-PL(SUB1 SUB2)
              ADD TY-INT-PL(SUB1 SUB2) TO HTY-INT-PL(SUB1 SUB2)

              ADD TY-UNITS(SUB1 SUB2) TO HTY-UNITS(SUB1 SUB2)
              ADD TY-OUTBAL(SUB1 SUB2) TO HTY-OUTBAL(SUB1 SUB2)

              ADD TY-UNITS-BNKRPT(SUB1 SUB2) TO
                  HTY-UNITS-BNKRPT(SUB1 SUB2)
              ADD TY-OUTBAL-BNKRPT(SUB1 SUB2) TO
                  HTY-OUTBAL-BNKRPT(SUB1 SUB2).
           IF SUB2 < 6
              ADD TY-NO-REPO(SUB1 SUB2) TO HTY-NO-REPO(SUB1 SUB2)
              ADD TY-AMT-REPO(SUB1 SUB2) TO HTY-AMT-REPO(SUB1 SUB2)
              ADD TY-O2-REPO(SUB1 SUB2) TO HTY-O2-REPO(SUB1 SUB2)
              ADD TY-INT-REPO(SUB1 SUB2) TO HTY-INT-REPO(SUB1 SUB2).
           IF SUB2 < 4
              ADD TY-DLRVPRIN(SUB1 SUB2) TO HTY-DLRVPRIN(SUB1 SUB2)
              ADD TY-DLRVCHGS(SUB1 SUB2) TO HTY-DLRVCHGS(SUB1 SUB2)
              IF SUB2 = 1
                 ADD TY-DLPARVHELD(SUB1) TO HTY-DLPARVHELD(SUB1).
           IF SUB1 = 1
              IF SUB2 < 6
                 ADD TY-PL-PRINCOL(SUB2) TO  HTY-PL-PRINCOL(SUB2)
                 ADD TY-PL-INTCOL(SUB2) TO  HTY-PL-INTCOL(SUB2)
                 ADD TY-PL-LATECOL(SUB2) TO  HTY-PL-LATECOL(SUB2)
                 ADD TY-PL-O2COL(SUB2) TO  HTY-PL-O2COL(SUB2).

      *=================================================================
       EXIT-ADD-TY-TO-HTY.
           EXIT.

      ***********************************************************************
      *    HOLD BRANCH STATISTICS FILE CLEAR ROUTINE
      ***********************************************************************
       CLEAR-HBY-FILE SECTION.
           MOVE " " TO HBY-REC.

      * HBY-MTD-YTD-LMO-LYR:

           PERFORM CLEAR-HBY-PL-RECOVERY
                     VARYING BYSUB1 FROM 1 BY 1 UNTIL BYSUB1 > 5.

           MOVE 0 TO HBY-NO-SOLD(1) HBY-AMT-SOLD(1)
                     HBY-NO-TRANS(1) HBY-AMT-TRANS(1)
                     HBY-NO-PURCH(1) HBY-AMT-PURCH(1)
                     HBY-OTHCOL(1)   HBY-OT2COL(1)
                     HBY-INTCOL(1)   HBY-LATECOL(1)  HBY-PRINCOL(1)
                     HBY-DEFCOL(1)   HBY-RNPRIN(1)   HBY-INTREB(1)
                     HBY-CLREB1(1)   HBY-CLREB2(1)   HBY-CLREB3(1)
                     HBY-CLREB4(1)   HBY-AHREB(1)    HBY-PPREB(1)
                     HBY-O1REB(1)    HBY-O2REB(1)    HBY-O3REB(1)
                     HBY-O4REB(1)    HBY-O5REB(1)
                     HBY-O6REB(1)    HBY-O7REB(1)    HBY-SCREB(1)
                     HBY-MFREB(1)    HBY-DFREB(1)    HBY-PART-CHGOFF(1)
                     HBY-ALLOTMENTS(1) HBY-LOCKBOX(1).

           MOVE 0 TO HBY-INS-NO-ELIG(1 1)
                     HBY-INS-NO-WROTE(1 1)
                     HBY-INS-NO-NOTWROTE(1 1)
                     HBY-INS-AMT-WROTE(1 1).

           MOVE HBY-INS-TYPE(1 1) TO HBY-INS-TYPE(1 2).
           MOVE HBY-INS-TYPE(1 1) TO HBY-INS-TYPE(1 3).
           MOVE HBY-INS-TYPE(1 1) TO HBY-INS-TYPE(1 4).
           MOVE HBY-INS-TYPE(1 1) TO HBY-INS-TYPE(1 5).
           MOVE HBY-INS-TYPE(1 1) TO HBY-INS-TYPE(1 6).
           MOVE HBY-INS-TYPE(1 1) TO HBY-INS-TYPE(1 7).
           MOVE HBY-INS-TYPE(1 1) TO HBY-INS-TYPE(1 8).
           MOVE HBY-INS-TYPE(1 1) TO HBY-INS-TYPE(1 9).
           MOVE HBY-INS-TYPE(1 1) TO HBY-INS-TYPE(1 10).

           MOVE 0 TO HBY-FEE-NO-ELIG(1 1)
                     HBY-FEE-NO-WROTE(1 1)
                     HBY-FEE-NO-NOTWROTE(1 1)
                     HBY-FEE-AMT-WROTE(1 1).

           MOVE HBY-FEE-TYPE(1 1) TO HBY-FEE-TYPE(1 2).
           MOVE HBY-FEE-TYPE(1 1) TO HBY-FEE-TYPE(1 3).
           MOVE HBY-FEE-TYPE(1 1) TO HBY-FEE-TYPE(1 4).

           MOVE 0 TO HBY-OTH-NO-COL(1)
                     HBY-OT2-NO-COL(1)
                     HBY-INT-NO-COL(1)
                     HBY-LATE-NO-COL(1).

           MOVE 0 TO HBY-OTH-NO-WAIVED(1)
                     HBY-OT2-NO-WAIVED(1)
                     HBY-INT-NO-WAIVED(1)
                     HBY-LATE-NO-WAIVED(1).

           MOVE 0 TO HBY-OTH-AMT-WAIVED(1)
                     HBY-OT2-AMT-WAIVED(1)
                     HBY-INT-AMT-WAIVED(1)
                     HBY-LATE-AMT-WAIVED(1).

           MOVE HBY-MTD-YTD(1) TO HBY-MTD-YTD(2).

      * HBY-PERIOD-TBL:
           PERFORM CLEAR-HBY-PERIOD-TBL VARYING
               BYSUB3 FROM 1 BY 1 UNTIL BYSUB3 > 12
                  AFTER BYSUB2 FROM 1 BY 1 UNTIL BYSUB2 > 2
                    AFTER BYSUB1 FROM 1 BY 1 UNTIL BYSUB1 > 3.

      * HBY-DEL-BUCKETS:
           PERFORM CLEAR-HBY-DEL-BUCKETS VARYING
               BYSUB3 FROM 1 BY 1 UNTIL BYSUB3 > 10
                  AFTER BYSUB2 FROM 1 BY 1 UNTIL BYSUB2 > 2
                    AFTER BYSUB1 FROM 1 BY 1 UNTIL BYSUB1 > 3.

      * HBY-REPO-ACTIVITY-TBL:
           PERFORM CLEAR-HBY-REPO-ACTIVITY-TBL VARYING
               BYSUB3 FROM 1 BY 1 UNTIL BYSUB3 > 5
                  AFTER BYSUB2 FROM 1 BY 1 UNTIL BYSUB2 > 2
                    AFTER BYSUB1 FROM 1 BY 1 UNTIL BYSUB1 > 3.

      * HBY-PL-ACTIVITY-TBL:
           PERFORM CLEAR-HBY-PL-ACTIVITY-TBL VARYING
               BYSUB3 FROM 1 BY 1 UNTIL BYSUB3 > 10
                  AFTER BYSUB2 FROM 1 BY 1 UNTIL BYSUB2 > 2
                    AFTER BYSUB1 FROM 1 BY 1 UNTIL BYSUB1 > 3.

      * HBY-DLR-ACTIVITY-TBL:
           PERFORM CLEAR-HBY-DLR-ACTIVITY-TBL VARYING
               BYSUB3 FROM 1 BY 1 UNTIL BYSUB3 > 3
                  AFTER BYSUB2 FROM 1 BY 1 UNTIL BYSUB2 > 2
                    AFTER BYSUB1 FROM 1 BY 1 UNTIL BYSUB1 > 3.

      * HBY-DLR-ACTIVITY-TBL:
           PERFORM VARYING BYSUB1 FROM 1 BY 1 UNTIL BYSUB1 > 3
                     AFTER BYSUB2 FROM 1 BY 1 UNTIL BYSUB2 > 2
                     AFTER BYSUB3 FROM 1 BY 1 UNTIL BYSUB3 > 10
              MOVE 0 TO HBY-UNEARNED-AMT(BYSUB1 BYSUB2 BYSUB3)
           END-PERFORM.

      * MISC:
           MOVE 0 TO HBY-SUMWGTTM
                     HBY-SUMWGTAV
                     HBY-SUMWGTTA.

           MOVE 0 TO HBY-IB-ACCRUED
                     HBY-PROCEEDS(1)
                     HBY-PROCEEDS(2).

           GO TO CLEAR-HBY-FILE-EXIT.


       CLEAR-HBY-PL-RECOVERY.
           MOVE 0 TO HBY-PL-PRINCOL(1 BYSUB1)
                     HBY-PL-INTCOL(1 BYSUB1)
                     HBY-PL-LATECOL(1 BYSUB1)
                     HBY-PL-O2COL(1 BYSUB1).

      *-----------------------------------------------------------------
       CLEAR-HBY-PERIOD-TBL.
           MOVE 0 TO HBY-NO(BYSUB1 BYSUB2 BYSUB3)
                     HBY-AMT(BYSUB1 BYSUB2 BYSUB3)
                     HBY-O2(BYSUB1 BYSUB2 BYSUB3)
                     HBY-INT(BYSUB1 BYSUB2 BYSUB3).

      *- - - - - - - - - - - - - - - - - - - CLEAR-BY-APP-PERIOD-TABLES
      *    MOVE 0 TO HBY-NO-PENDING (BYSUB1 BYSUB2 BYSUB3)
      *              HBY-NO-COND-APP(BYSUB1 BYSUB2 BYSUB3)
      *              HBY-NO-APPROVED(BYSUB1 BYSUB2 BYSUB3)
      *              HBY-NO-MADE    (BYSUB1 BYSUB2 BYSUB3)
      *              HBY-NO-TURNDOWN(BYSUB1 BYSUB2 BYSUB3)
      *              HBY-NO-TURNUSDOWN(BYSUB1 BYSUB2 BYSUB3).

      *-----------------------------------------------------------------
       CLEAR-HBY-DEL-BUCKETS.
           MOVE 0 TO HBY-UNITS(BYSUB1 BYSUB2 BYSUB3)
                     HBY-OUTBAL(BYSUB1 BYSUB2 BYSUB3).
           MOVE 0 TO HBY-UNITS-BNKRPT(BYSUB1 BYSUB2 BYSUB3)
                     HBY-OUTBAL-BNKRPT(BYSUB1 BYSUB2 BYSUB3).

       CLEAR-HBY-REPO-ACTIVITY-TBL.
           MOVE 0 TO HBY-NO-REPO(BYSUB1 BYSUB2 BYSUB3)
                     HBY-AMT-REPO(BYSUB1 BYSUB2 BYSUB3)
                     HBY-O2-REPO(BYSUB1 BYSUB2 BYSUB3)
                     HBY-INT-REPO(BYSUB1 BYSUB2 BYSUB3).

       CLEAR-HBY-PL-ACTIVITY-TBL.
           MOVE 0 TO HBY-NO-PL(BYSUB1 BYSUB2 BYSUB3)
                     HBY-AMT-PL(BYSUB1 BYSUB2 BYSUB3)
                     HBY-O2-PL(BYSUB1 BYSUB2 BYSUB3)
                     HBY-INT-PL(BYSUB1 BYSUB2 BYSUB3).

       CLEAR-HBY-DLR-ACTIVITY-TBL.
           MOVE 0 TO HBY-DLRVPRIN(BYSUB1 BYSUB2 BYSUB3)
                     HBY-DLRVCHGS(BYSUB1 BYSUB2 BYSUB3)
                     HBY-DLPARVHELD(BYSUB1 BYSUB2)
                     HBY-DLDISC(BYSUB1).

       CLEAR-HBY-FILE-EXIT.
           EXIT.
      ***********************************************************************
      *    HOLD TODAYS TOTALS FILE CLEAR ROUTINE
      ***********************************************************************
       CLEAR-HTY-FILE SECTION.
           MOVE " " TO HTY-REC.

      * HTY-CKWRITTEN:
           MOVE 0 TO HTY-CKWRITTEN(1) HTY-CKWRITTEN(2)
                     HTY-CKWRITTEN(3) HTY-CKWRITTEN(4)
                     HTY-CKWRITTEN(5) HTY-CKWRITTEN(6)
                     HTY-CKWRITTEN(7) HTY-CKWRITTEN(8).

      * TY TODAYS NO'S:

           PERFORM CLEAR-HTY-PL-RECOVERY
                   VARYING TYSUB1 FROM 1 BY 1 UNTIL TYSUB1 > 5.

           MOVE 0 TO HTY-NO-SOLD  HTY-AMT-SOLD
                     HTY-NO-TRANS HTY-AMT-TRANS
                     HTY-NO-PURCH HTY-AMT-PURCH
                     HTY-OTHCOL   HTY-OT2COL
                     HTY-INTCOL   HTY-LATECOL  HTY-PRINCOL
                     HTY-DEFCOL   HTY-INTREB   HTY-CLREB1
                     HTY-CLREB2   HTY-CLREB3   HTY-CLREB4
                     HTY-AHREB    HTY-PPREB    HTY-SCREB
                     HTY-O1REB    HTY-O2REB
                     HTY-O3REB    HTY-O4REB    HTY-O5REB
                     HTY-O6REB    HTY-O7REB    HTY-RNPRIN
                     HTY-MFREB    HTY-DFREB    HTY-PART-CHGOFF
                     HTY-ALLOTMENTS HTY-LOCKBOX.

           MOVE 0 TO HTY-INS-NO-ELIG(1)
                     HTY-INS-NO-WROTE(1)
                     HTY-INS-NO-NOTWROTE(1)
                     HTY-INS-AMT-WROTE(1).
           MOVE HTY-INS-TYPE(1) TO HTY-INS-TYPE(2)
                                  HTY-INS-TYPE(3)
                                  HTY-INS-TYPE(4)
                                  HTY-INS-TYPE(5)
                                  HTY-INS-TYPE(6)
                                  HTY-INS-TYPE(7)
                                  HTY-INS-TYPE(8)
                                  HTY-INS-TYPE(9)
                                  HTY-INS-TYPE(10).

           MOVE 0 TO HTY-OTH-NO-COL
                     HTY-OT2-NO-COL
                     HTY-INT-NO-COL
                     HTY-LATE-NO-COL.

           MOVE 0 TO HTY-OTH-NO-WAIVED
                     HTY-OT2-NO-WAIVED
                     HTY-INT-NO-WAIVED
                     HTY-LATE-NO-WAIVED.

           MOVE 0 TO HTY-OTH-AMT-WAIVED
                     HTY-OT2-AMT-WAIVED
                     HTY-INT-AMT-WAIVED
                     HTY-LATE-AMT-WAIVED.

      * HTY-ACTIVITY-TBL:
           PERFORM CLEAR-HTY-ACTIVITY-TBL VARYING
              TYSUB2 FROM 1 BY 1 UNTIL TYSUB2 > 12
                AFTER TYSUB1 FROM 1 BY 1 UNTIL TYSUB1 > 2.

      * HTY-DEL-BUCKETS:
           PERFORM CLEAR-HTY-DEL-BUCKETS VARYING
              TYSUB2 FROM 1 BY 1 UNTIL TYSUB2 > 10
                AFTER TYSUB1 FROM 1 BY 1 UNTIL TYSUB1 > 2.

      * HTY-REPO-ACTIVITY-TBL:
           PERFORM CLEAR-HTY-REPO-ACTIVITY-TBL VARYING
               TYSUB2 FROM 1 BY 1 UNTIL TYSUB2 > 5
                  AFTER TYSUB1 FROM 1 BY 1 UNTIL TYSUB1 > 2.

      * HTY-PL-ACTIVITY-TBL:
           PERFORM CLEAR-HTY-PL-ACTIVITY-TBL VARYING
               TYSUB2 FROM 1 BY 1 UNTIL TYSUB2 > 10
                  AFTER TYSUB1 FROM 1 BY 1 UNTIL TYSUB1 > 2.

      * HTY-DLR-ACTIVITY-TBL:
           PERFORM CLEAR-HTY-DLR-ACTIVITY-TBL VARYING
               TYSUB2 FROM 1 BY 1 UNTIL TYSUB2 > 3
                  AFTER TYSUB1 FROM 1 BY 1 UNTIL TYSUB1 > 2.

      * HTY-UNEARNED-AMT
           PERFORM VARYING TYSUB1 FROM 1 BY 1 UNTIL TYSUB1 > 2
                     AFTER TYSUB2 FROM 1 BY 1 UNTIL TYSUB2 > 10
              MOVE 0 TO HTY-UNEARNED-AMT(TYSUB1 TYSUB2)
           END-PERFORM.



      * TY SUMWGT:
           MOVE 0 TO HTY-SUMWGTTM
                     HTY-SUMWGTAV
                     HTY-SUMWGTTA.

      * TY DLDISC::
           MOVE 0 TO HTY-DLDISC
                     HTY-PROCEEDS.

           GO TO CLEAR-HTY-FILE-EXIT.
      * ----------------------------------------------------

       CLEAR-HTY-PL-RECOVERY.
              MOVE 0 TO HTY-PL-PRINCOL(TYSUB1)
                        HTY-PL-INTCOL(TYSUB1)
                        HTY-PL-LATECOL(TYSUB1)
                        HTY-PL-O2COL(TYSUB1).

       CLEAR-HTY-ACTIVITY-TBL.
           MOVE 0 TO HTY-NO(TYSUB1 TYSUB2)
                     HTY-AMT(TYSUB1 TYSUB2)
                     HTY-O2(TYSUB1 TYSUB2)
                     HTY-INT(TYSUB1 TYSUB2).
      *- - - - - - - - - - - - - - - - - - CLEAR-TY-APP-ACTIVITY-TABLES
      *    MOVE 0 TO HTY-NO-PENDING(TYSUB1 TYSUB2)
      *              HTY-NO-COND-APP(TYSUB1 TYSUB2)
      *              HTY-NO-APPROVED(TYSUB1 TYSUB2)
      *              HTY-NO-MADE    (TYSUB1 TYSUB2)
      *              HTY-NO-TURNDOWN(TYSUB1 TYSUB2)
      *              HTY-NO-TURNUSDOWN(TYSUB1 TYSUB2).

       CLEAR-HTY-DEL-BUCKETS.
           MOVE 0 TO HTY-UNITS(TYSUB1 TYSUB2)
                     HTY-OUTBAL(TYSUB1 TYSUB2).
           MOVE 0 TO HTY-UNITS-BNKRPT(TYSUB1 TYSUB2)
                     HTY-OUTBAL-BNKRPT(TYSUB1 TYSUB2).

       CLEAR-HTY-REPO-ACTIVITY-TBL.
           MOVE 0 TO HTY-NO-REPO(TYSUB1 TYSUB2)
                     HTY-AMT-REPO(TYSUB1 TYSUB2)
                     HTY-O2-REPO(TYSUB1 TYSUB2)
                     HTY-INT-REPO(TYSUB1 TYSUB2).

       CLEAR-HTY-PL-ACTIVITY-TBL.
           MOVE 0 TO HTY-NO-PL(TYSUB1 TYSUB2)
                     HTY-AMT-PL(TYSUB1 TYSUB2)
                     HTY-O2-PL(TYSUB1 TYSUB2)
                     HTY-INT-PL(TYSUB1 TYSUB2).

       CLEAR-HTY-DLR-ACTIVITY-TBL.
           MOVE 0 TO HTY-DLRVPRIN(TYSUB1 TYSUB2)
                     HTY-DLRVCHGS(TYSUB1 TYSUB2)
                     HTY-DLPARVHELD(TYSUB1).

       CLEAR-HTY-FILE-EXIT.
           EXIT.

      ***********************************************************************
      *  IF WORLD, SEE IF WRFILE UPDATED FOR MONTH & IF USER IS AUTHORIZED
      *  & SET FLAG TO READ RATING SHEET FILE & SET UP PAGE 13
      ***********************************************************************
       SET-WRFILE-FLAG SECTION.
           MOVE "N" TO READ-WRFILE-FG.
           PERFORM OPEN-WR1-FILE.

           MOVE STORE-DATE-CCYYMM-MM TO DATE-MMDDYY-MM.
           MOVE STORE-DATE-CCYYMM-YY TO DATE-MMDDYY-YY.
           PERFORM CONVERT-MMDDYY-TO-YYYYMM.

           MOVE DATE-YYYYMM TO WR-YYYYMM
                               QWR1-WBEG-YYYYMM
                               QWR1-WEND-YYYYMM.
           MOVE 0           TO WR-BRNO
                               QWR1-WBEG-BRNO.
           MOVE ALL "9"     TO QWR1-WEND-BRNO.

      *    PERFORM START-WR1-FILE.
      *    PERFORM READ-WR1-FILE-NEXT.
 
           PERFORM READ-WR1-FILE-FIRST-CALL. *> EMBEDDED - WRFILE
           PERFORM CLOSE-WR1-FILE.
      *    IF IO-GOOD
      *       IF WR-YYYYMM = DATE-YYYYMM
      *          MOVE "Y" TO READ-WRFILE-FG.

            IF WS-WR1-ROW-COUNT = 0
                 MOVE "N" TO READ-WRFILE-FG 
            ELSE
                 MOVE "Y" TO READ-WRFILE-FG.

       SET-WRFILE-FLAG-EXIT.
           EXIT.

      ***********************************************************************
      *  FOR PAGE 12, READ WRFILE & ADD TOTALS 
      ***********************************************************************
       PROCESS-WRFILE SECTION.
           MOVE ZEROS TO PAGE-12-TOTALS.
           PERFORM OPEN-WR1-FILE.

           MOVE STORE-DATE-CCYYMM-MM TO DATE-MMDDYY-MM.
           MOVE STORE-DATE-CCYYMM-YY TO DATE-MMDDYY-YY.
           PERFORM CONVERT-MMDDYY-TO-YYYYMM.
           MOVE DATE-YYYYMM TO WR-YYYYMM
                               QWR1-WBEG-YYYYMM
                               QWR1-WEND-YYYYMM.
           IF GROUP-FLAG = "N"
              MOVE STORE-BRNO  TO WR-BRNO
              PERFORM READ-WR1-FILE *> EMBEDDED - WRFILE
              IF IO-BAD
                 GO TO PROCESS-WRFILE-EXIT
              ELSE
                 PERFORM ADD-WR-TOTALS
                 GO TO PROCESS-WRFILE-EXIT.

           MOVE 1       TO WR-BRNO
                           QWR1-WBEG-BRNO.
           MOVE ALL "9" TO QWR1-WEND-BRNO.

           PERFORM START-WR1-FILE. *> SSP - WRFILE
       NEXT-WR.
           PERFORM READ-WR1-FILE-NEXT. *> EMBEDDED - WRFILE
           IF IO-BAD
              GO TO PROCESS-WRFILE-EXIT.

           IF WR-YYYYMM NOT = DATE-YYYYMM
              GO TO PROCESS-WRFILE-EXIT.

           MOVE WR-BRNO TO WGR-SRESULT.
           PERFORM SEARCH-GR-VERIFY.
           IF WGR-ERR > 0
              GO TO NEXT-WR.
           PERFORM ADD-WR-TOTALS.
           GO TO NEXT-WR.
       PROCESS-WRFILE-EXIT.
           PERFORM CLOSE-WR1-FILE.

      ***********************************************************************
      *  FOR PAGE 12, ADD WRFILE TOTALS
      ***********************************************************************
       ADD-WR-TOTALS SECTION.
           ADD WR-ELM-OUTS-BALANCE              TO SUM-ELM-OUTS-BALANCE.
           ADD WR-ELY-OUTS-BALANCE              TO SUM-ELY-OUTS-BALANCE.
           ADD WR-MGR-BONUS                     TO SUM-MGR-BONUS.
           ADD WR-BSR-BONUS(1)                  TO SUM-BSR-BONUS.
           COMPUTE SUM-PROFIT(1) =
                   SUM-PROFIT(1)
                 + WR-PRETAX-PROFIT(1)
                 + WR-INTEREST-ALLOCATION(1)
                 + WR-SUPERVISION(1)
                 + WR-GEN-RESERVE-ADJ(1)
                 + WR-INTANGIBLE-AMORT(1)
                 + WR-ROBBERIES(1).
           COMPUTE SUM-PROFIT(2) =
                   SUM-PROFIT(2)
                 + WR-PRETAX-PROFIT(2)
                 + WR-INTEREST-ALLOCATION(2)
                 + WR-SUPERVISION(2)
                 + WR-GEN-RESERVE-ADJ(2)
                 + WR-INTANGIBLE-AMORT(2)
                 + WR-ROBBERIES(2).
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 2
              ADD WR-RATING-SHEET-INCOME(SUB)   TO SUM-INCOME(SUB)
              ADD WR-INT-FEE-INCOME(SUB)        TO SUM-INT-INCOME(SUB)
              ADD WR-INSURANCE-INCOME(SUB)      TO SUM-INS-INCOME(SUB)
              ADD WR-RATING-SHEET-EXPENSES(SUB) TO SUM-EXPENSE(SUB)
              ADD WR-PERSONNEL-EXPENSE(SUB)     TO SUM-PERSONNEL(SUB)
              ADD WR-NO-NB(SUB) 
                  WR-NO-FB(SUB)
                  WR-NO-PB(SUB)                 TO SUM-NO-LOANS(SUB)
              END-PERFORM.
           IF GROUP-FLAG = "N"
              MOVE WR-YTD-PCT-RATE-SHT-PROFIT   TO SUM-YTD-PCT-PROFIT
              MOVE WR-YTD-PCT-RS-INC-LEDGER     TO SUM-YTD-PCT-INCOME
              MOVE WR-YTD-PCT-INT-FEE-LEDGER    TO SUM-YTD-PCT-INT-INC
              MOVE WR-YTD-PCT-INS-INC-LEDGER    TO SUM-YTD-PCT-INS-INC
              MOVE WR-YTD-PCT-EXPENSE-LEDGER    TO SUM-YTD-PCT-EXPENSE.

      **************************************************************************
      * DESTROY FORM (SCREEN). PERFORMED PRIOR TO OPENING UP A NEW FORM
      **************************************************************************
       FORM-DESTROY SECTION.
           EVALUATE PAGE-FLAG
              WHEN  1 DESTROY BYINQ1-SCREEN
              WHEN  2 DESTROY BYINQ2-SCREEN
              WHEN  4 DESTROY BYINQ4-SCREEN
              WHEN  5 DESTROY BYINQ5-SCREEN
              WHEN  6 DESTROY BYINQ6-SCREEN
              WHEN  7 DESTROY BYINQ7-SCREEN
              WHEN  8 DESTROY BYINQ8-SCREEN
              WHEN  9 DESTROY BYINQ9-SCREEN
              WHEN 10 DESTROY BYINQA-SCREEN
              WHEN 11 DESTROY BYINQB-SCREEN
              WHEN 12 DESTROY BYINQC-SCREEN
              WHEN OTHER DESTROY BYINQ3-SCREEN
           END-EVALUATE.

      **************************************************************************
      * REQUIRED SECTION: CALLED BY SCREENIO:
      *
      * RESET FORM (SCREEN) TO ITS INITIAL STATE (LIKE FRESET).
      **************************************************************************
       FORM-RESET SECTION.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           EVALUATE PAGE-FLAG
              WHEN 1 DISPLAY BYINQ1-SCREEN
                     MOVE EXT-CONAME-SYSDATE TO BYINQ1-SYSDATE
                     MOVE EXT-CONAME-CONAME  TO BYINQ1-CONAME
                     MOVE BYINQ1-FORM-FIELDS TO WR-BUF 
                     MOVE BYINQ1-FORM-LIST   TO WR-LIST 
              WHEN 2 DISPLAY BYINQ2-SCREEN
                     MOVE EXT-CONAME-SYSDATE TO BYINQ2-SYSDATE
                     MOVE BYINQ2-FORM-FIELDS TO WR-BUF 
                     MOVE BYINQ2-FORM-LIST   TO WR-LIST 
              WHEN 4 DISPLAY BYINQ4-SCREEN
                     MOVE EXT-CONAME-SYSDATE TO BYINQ4-SYSDATE
                     MOVE BYINQ4-FORM-FIELDS TO WR-BUF 
                     MOVE BYINQ4-FORM-LIST   TO WR-LIST 
              WHEN 5 DISPLAY BYINQ5-SCREEN
                     MOVE EXT-CONAME-SYSDATE TO BYINQ5-SYSDATE
                     MOVE EXT-CONAME-CONAME  TO BYINQ5-CONAME
                     MOVE BYINQ5-FORM-FIELDS TO WR-BUF 
                     MOVE BYINQ5-FORM-LIST   TO WR-LIST 
              WHEN 6 DISPLAY BYINQ6-SCREEN
                     MOVE EXT-CONAME-SYSDATE TO BYINQ6-SYSDATE
                     MOVE EXT-CONAME-CONAME  TO BYINQ6-CONAME
                     MOVE BYINQ6-FORM-FIELDS TO WR-BUF 
                     MOVE BYINQ6-FORM-LIST   TO WR-LIST 
              WHEN 7 DISPLAY BYINQ7-SCREEN
                     MOVE EXT-CONAME-SYSDATE TO BYINQ7-SYSDATE
                     MOVE BYINQ7-FORM-FIELDS TO WR-BUF 
                     MOVE BYINQ7-FORM-LIST   TO WR-LIST 
              WHEN 8 DISPLAY BYINQ8-SCREEN
                     MOVE EXT-CONAME-SYSDATE TO BYINQ8-SYSDATE
                     MOVE EXT-CONAME-CONAME  TO BYINQ8-CONAME
                     MOVE BYINQ8-FORM-FIELDS TO WR-BUF 
                     MOVE BYINQ8-FORM-LIST   TO WR-LIST 
              WHEN 9 DISPLAY BYINQ9-SCREEN
                     MOVE BYINQ9-FORM-FIELDS TO WR-BUF 
                     MOVE BYINQ9-FORM-LIST   TO WR-LIST 
              WHEN 10 DISPLAY BYINQA-SCREEN
                     MOVE EXT-CONAME-SYSDATE TO BYINQA-SYSDATE
                     MOVE BYINQA-FORM-FIELDS TO WR-BUF 
                     MOVE BYINQA-FORM-LIST   TO WR-LIST 
              WHEN 11 DISPLAY BYINQB-SCREEN
                     MOVE EXT-CONAME-SYSDATE TO BYINQB-SYSDATE
                     MOVE BYINQB-FORM-FIELDS TO WR-BUF 
                     MOVE BYINQB-FORM-LIST   TO WR-LIST 
              WHEN 12 DISPLAY BYINQC-SCREEN
                     MOVE EXT-CONAME-SYSDATE TO BYINQC-SYSDATE
                     MOVE BYINQC-FORM-FIELDS TO WR-BUF 
                     MOVE BYINQC-FORM-LIST   TO WR-LIST 
              WHEN OTHER DISPLAY BYINQ3-SCREEN
                     MOVE EXT-CONAME-SYSDATE TO BYINQ3-SYSDATE
                     MOVE EXT-CONAME-CONAME  TO BYINQ3-CONAME
                     MOVE BYINQ3-FORM-FIELDS TO WR-BUF 
                     MOVE BYINQ3-FORM-LIST   TO WR-LIST 
           END-EVALUATE.
           
           PERFORM CALL-WRFORM.
           
       FORM-RESET-EXIT.
           EXIT.

      **************************************************************************
      * REQUIRED SECTION: CALLED BY SCREENIO:
      *
      * SETUP CORRECT HELP FIELDS
      **************************************************************************
       SETUP-LINK-INFO SECTION.
           EVALUATE PAGE-FLAG
                WHEN  1 MOVE BYINQ1-HELP--FILE TO HELP-LINK-FILE
                        MOVE BYINQ1-HELP--DIR  TO HELP-LINK-DIR
                WHEN  2 MOVE BYINQ2-HELP--FILE TO HELP-LINK-FILE
                        MOVE BYINQ2-HELP--DIR  TO HELP-LINK-DIR
                WHEN  4 MOVE BYINQ4-HELP--FILE TO HELP-LINK-FILE
                        MOVE BYINQ4-HELP--DIR  TO HELP-LINK-DIR
                WHEN  5 MOVE BYINQ5-HELP--FILE TO HELP-LINK-FILE
                        MOVE BYINQ5-HELP--DIR  TO HELP-LINK-DIR
                WHEN  6 MOVE BYINQ6-HELP--FILE TO HELP-LINK-FILE
                        MOVE BYINQ6-HELP--DIR  TO HELP-LINK-DIR
                WHEN  7 MOVE BYINQ7-HELP--FILE TO HELP-LINK-FILE
                        MOVE BYINQ7-HELP--DIR  TO HELP-LINK-DIR
                WHEN  8 MOVE BYINQ8-HELP--FILE TO HELP-LINK-FILE
                        MOVE BYINQ8-HELP--DIR  TO HELP-LINK-DIR
                WHEN  9 MOVE BYINQ9-HELP--FILE TO HELP-LINK-FILE
                        MOVE BYINQ9-HELP--DIR  TO HELP-LINK-DIR
                WHEN 10 MOVE BYINQA-HELP--FILE TO HELP-LINK-FILE
                        MOVE BYINQA-HELP--DIR  TO HELP-LINK-DIR
                WHEN 11 MOVE BYINQB-HELP--FILE TO HELP-LINK-FILE
                        MOVE BYINQB-HELP--DIR  TO HELP-LINK-DIR
                WHEN 12 MOVE BYINQC-HELP--FILE TO HELP-LINK-FILE
                        MOVE BYINQC-HELP--DIR  TO HELP-LINK-DIR
                WHEN OTHER MOVE BYINQ3-HELP--FILE TO HELP-LINK-FILE
                           MOVE BYINQ3-HELP--DIR  TO HELP-LINK-DIR
           END-EVALUATE.


       VDU-CONVERT-FIELD SECTION.
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBLP/BYINQ1_EVA.CPY".
              COPY "LIBLP/BYINQ2_EVA.CPY".
              COPY "LIBLP/BYINQ3_EVA.CPY".
              COPY "LIBLP/BYINQ4_EVA.CPY".
              COPY "LIBLP/BYINQ5_EVA.CPY".
              COPY "LIBLP/BYINQ6_EVA.CPY".
              COPY "LIBLP/BYINQ7_EVA.CPY".
              COPY "LIBLP/BYINQ8_EVA.CPY".
              COPY "LIBLP/BYINQ9_EVA.CPY".
              COPY "LIBLP/BYINQA_EVA.CPY".
              COPY "LIBLP/BYINQB_EVA.CPY".
              COPY "LIBLP/BYINQC_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

       COPY "LIBLP/LPTYCL.CPY".
       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/GRWORK.CPY".
       COPY "LIBGB/GRSERCH.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/GPENV.CPY".

      ******************************************
      *    MISC SECTION
      ******************************************
       MISC SECTION.
       COPY "LIBGB/SYSTEM.CPY".
       GET-TODAYS-DATE.
           MOVE TODAY-MM TO DISPLAY-MO.
           MOVE TODAY-YY TO DISPLAY-YR.
           MOVE DISPLAY-DATE TO WR-BUF.
           MOVE DATE-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.
       GET-BR.
           PERFORM READ-BR-FILE.

           MOVE EXT-FILPATH-BASE           TO FILPATH-BASE.
           MOVE BR-MACHINE                 TO FILPATH-MACHINE.
           MOVE BR-DATA-PATH               TO FILPATH-DATA.

           MOVE "B"                        TO BY-PATH-OVERRIDE
                                              RC-PATH-OVERRIDE
                                              TY-PATH-OVERRIDE.
       SEND-BR-NAME.
           MOVE "S  A000BRX." TO VDU.
           IF TRAP-NO-TY-FG = "Y"
              MOVE "TODAY TOTALS SKIPPED" TO VDUBUF
           ELSE
              IF TRAP-A-DATE-TY-FG = "Y"
                 MOVE TY-BRFG TO TY-TRAP-UPD
                 MOVE TY-TRAP-MSG TO VDUBUF
              ELSE
                 IF GROUP-FLAG = "Y"
                    MOVE HOLD-GR-NAME TO VDUBUF
                 ELSE
                    MOVE BR-NAME TO VDUBUF.
           PERFORM SCREENIO.

      *************************************
       IO-ROUTINES SECTION.
      *************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/DECLARE_SQL.CPY".
           PERFORM CLOSE-WR1-FILE.
           PERFORM CLOSE-WR1-CONNECTION.
           PERFORM CLOSE-TY1-FILE.
           PERFORM CLOSE-LC1-FILE.
           PERFORM CLOSE-BY1-FILE.
           PERFORM CLOSE-BY1-CONNECTION.
           PERFORM CLOSE-RC2-FILE.
           PERFORM CLOSE-GR1-FILE.
           PERFORM CLOSE-BR-FILE.
       COPY "LIBSP/SPWRGS_SQL.CPY".
       COPY "LIBLP/LPTYGS_SQL.CPY".
       COPY "LIBLP/LPLCGS_SQL.CPY".
       COPY "LIBLP/LPBYGS_SQL.CPY".
       COPY "LIBGB/GBRCGS_SQL.CPY".
       COPY "LIBGB/GBGRGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBLP/LPLC1RN.CPY".

      * COPY "LIBLP/LPBY1RN.CPY".
      * COPYMEMBER: LIBLP/LPBY1RN
      ******************************************************************
      *
      * DESCRIPTION: BRANCH STATISTICS FILE - IO ROUTINES - A THRU B
      *
      * FILE KEY   : #1 = DATE-CCYYMM, BRNO, CLASS
      *
      * FILE PATH  : /USR/DATA/R#/LP/BYAFILE
      *              /USR/DATA/R#/LP/BYBFILE
      *
      * ATTENTION  : BY-PATH-OVERRIDE CONTROLS ALL FILES - A THRU B
      *              ^^^^^^^^^^^^^^^^
      *              THERE IS NO BYA-PATH-OVERRIDE OR BYB-PATH-OVERRIDE
      *
      *=================================================================
      * REV:
      * KEC 2019-0205 ***NEW*** TO A32; MODIFIED FROM A30
      * KEC 2021-0524 ADDED ACUSQL VERSION
      ******************************************************************
      *-----------------------------------------------------------------
       SET-ALL-TO-BY1.
           MOVE BYA1-KEY   TO BY1-KEY.
           MOVE BYA-AREA-A TO BY-AREA-A.
           MOVE BYB-AREA-B TO BY-AREA-B.

      *-----------------------------------------------------------------
       SET-BY1-TO-ALL.
           MOVE BY1-KEY    TO BYA1-KEY BYB1-KEY.
           MOVE BY-AREA-A  TO BYA-AREA-A.
           MOVE BY-AREA-B  TO BYB-AREA-B.

      *-----------------------------------------------------------------
       LOAD-BY1-FILE.
           PERFORM LOAD-BYA1-FILE.
           PERFORM LOAD-BYB1-FILE.

      *-----------------------------------------------------------------
       OPEN-BY1-FILE.
           PERFORM OPEN-BYA1-FILE.
           IF ( IO-FG = 0 )
              PERFORM OPEN-BYB1-FILE
              IF ( IO-FG = 0 )
                 MOVE " " TO IO-MULTI-FG
              ELSE
                 MOVE "B" TO IO-MULTI-FG
           ELSE
              MOVE "A" TO IO-MULTI-FG.

      *-----------------------------------------------------------------
       START-BY1-FILE.
           PERFORM SET-BY1-TO-ALL.
           PERFORM START-BYA1-FILE.
           IF ( IO-FG NOT = 0 )
              MOVE "A" TO IO-MULTI-FG.
             
      *-----------------------------------------------------------------
       READ-BY1-FILE.
           PERFORM SET-BY1-TO-ALL.
           PERFORM READ-BYA1-FILE.
           IF ( IO-FG = 0 )
              PERFORM READ-BYB1-FILE
              IF ( IO-FG = 0 )
                 PERFORM SET-ALL-TO-BY1
               ELSE
                 MOVE "B" TO IO-MULTI-FG
           ELSE
              MOVE "A" TO IO-MULTI-FG.
             
      *-----------------------------------------------------------------
       READ-BY1-FILE-NEXT.
           PERFORM SET-BY1-TO-ALL.
           PERFORM READ-BYA1-FILE-NEXT.
           IF ( IO-FG = 0 )
              MOVE BYA1-KEY TO BYB1-KEY
              PERFORM READ-BYB1-FILE
              IF ( IO-FG = 0 )
                 PERFORM SET-ALL-TO-BY1
               ELSE
                 MOVE "B" TO IO-MULTI-FG
           ELSE
              MOVE "A" TO IO-MULTI-FG.
             
      *-----------------------------------------------------------------
       CLOSE-BY1-FILE.
           PERFORM CLOSE-BYA1-FILE.
           PERFORM CLOSE-BYB1-FILE.

      *=================================================================
       LOAD-BYA1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( BY-PATH-OVERRIDE = "Y" )
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( BY-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO BYA-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO BYA-ALL-MACHINE
              MOVE BYA-ALL-PATH            TO BYA-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( BY-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO BYA-ALL-BASE
              MOVE FILPATH-MACHINE         TO BYA-ALL-MACHINE
              MOVE BYA-ALL-PATH            TO BYA-PATH.

      *-----------------------------------------------------------------
       OPEN-BYA1-FILE.
           PERFORM LOAD-BYA1-FILE.
           PERFORM OPEN-IT.
           MOVE BYA-PATH-SQL TO E-FILE.

           IF BY1-CONN-OPEN-SW = "N" 
              MOVE "Y"         TO BY1-CONN-OPEN-SW
              EXEC SQL CONNECT TO :SQL-CONNECT-SERVER AS BY1_CONNECT 
                       USER :SQL-CONNECT-USERID USING :SQL-CONNECT-PSWD
              END-EXEC.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

           EXEC SQL SET CONNECTION C1 END-EXEC.

      *-----------------------------------------------------------------
       START-BYA1-FILE. *> SSP - BYAFILE
           PERFORM START-IT.
           MOVE BYA-PATH-SQL TO E-FILE.
           MOVE BYA1-KEY TO E-KEYX.

           IF ( BYA1-CURSOR-STAT-GOOD )
              PERFORM CLOSE-BYA1-FILE.

           IF ( QBY1-WEND-DATE-CCYYMM = ZEROES ) OR
              ( QBY1-WEND-DATE-CCYYMM NOT NUMERIC )
              MOVE BY-DATE-CCYYMM       TO QBY1-WBEG-DATE-CCYYMM
              MOVE ALL "9"              TO QBY1-WEND-DATE-CCYYMM 
              MOVE BY-BRNO              TO QBY1-WBEG-BRNO
              MOVE ALL "9"              TO QBY1-WEND-BRNO
              MOVE BY-CLASS             TO QBY1-WBEG-CLASS
              MOVE ALL "9"              TO QBY1-WEND-CLASS.

           EXEC SQL SET CONNECTION BY1_CONNECT END-EXEC.

           EXEC SQL
            DECLARE BYA1_CURSOR CURSOR FOR
                :BY1-RETURN-CODE = EXEC SSP_BYINQ_BYAFILE_SELECT 
              ( :QBY1-WBEG-DATE-CCYYMM,
                :QBY1-WEND-DATE-CCYYMM,
                :QBY1-WBEG-BRNO,
                :QBY1-WEND-BRNO, 
                :QBY1-WBEG-CLASS,      
                :QBY1-WEND-CLASS )     
           END-EXEC.

           EXEC SQL
               OPEN BYA1_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE ZEROES TO QBY1-WBEG-DATE-CCYYMM
                          QBY1-WEND-DATE-CCYYMM
                          QBY1-WBEG-BRNO
                          QBY1-WEND-BRNO
                          QBY1-WBEG-CLASS
                          QBY1-WEND-CLASS.

           MOVE STAT---GOOD TO BYA1-CURSOR-STAT.

           EXEC SQL SET CONNECTION C1 END-EXEC.

      *-----------------------------------------------------------------
       READ-BYA1-FILE.
           PERFORM READ-IT.
           MOVE BYA-PATH-SQL TO E-FILE.
           MOVE BYA1-KEY TO E-KEYX.
           INITIALIZE QBYA-REC.

           MOVE BYA-DATE-CCYYMM      TO QBYA-DATE-CCYYMM.
           MOVE BYA-BRNO             TO QBYA-BRNO.
           MOVE BYA-CLASS            TO QBYA-CLASS.

           EXEC SQL
            SELECT BYAFILE.BYA_DATE_CCYYMM,
                   BYAFILE.BYA_BRNO,
                   BYAFILE.BYA_CLASS,
                   CAST(BYAFILE.BYA_LTOUCH_DATE
                                                  AS VARCHAR(10)),
                   BYAFILE.BYA_NO_1_1_1,
                   BYAFILE.BYA_AMT_1_1_1,
                   BYAFILE.BYA_O2_1_1_1,
                   BYAFILE.BYA_INT_1_1_1,
                   BYAFILE.BYA_NO_1_1_2,
                   BYAFILE.BYA_AMT_1_1_2,
                   BYAFILE.BYA_O2_1_1_2,
                   BYAFILE.BYA_INT_1_1_2,
                   BYAFILE.BYA_NO_1_1_3,
                   BYAFILE.BYA_AMT_1_1_3,
                   BYAFILE.BYA_O2_1_1_3,
                   BYAFILE.BYA_INT_1_1_3,
                   BYAFILE.BYA_NO_1_1_4,
                   BYAFILE.BYA_AMT_1_1_4,
                   BYAFILE.BYA_O2_1_1_4,
                   BYAFILE.BYA_INT_1_1_4,
                   BYAFILE.BYA_NO_1_1_5,
                   BYAFILE.BYA_AMT_1_1_5,
                   BYAFILE.BYA_O2_1_1_5,
                   BYAFILE.BYA_INT_1_1_5,
                   BYAFILE.BYA_NO_1_1_6,
                   BYAFILE.BYA_AMT_1_1_6,
                   BYAFILE.BYA_O2_1_1_6,
                   BYAFILE.BYA_INT_1_1_6,
                   BYAFILE.BYA_NO_1_1_7,
                   BYAFILE.BYA_AMT_1_1_7,
                   BYAFILE.BYA_O2_1_1_7,
                   BYAFILE.BYA_INT_1_1_7,
                   BYAFILE.BYA_NO_1_1_8,
                   BYAFILE.BYA_AMT_1_1_8,
                   BYAFILE.BYA_O2_1_1_8,
                   BYAFILE.BYA_INT_1_1_8,
                   BYAFILE.BYA_NO_1_1_9,
                   BYAFILE.BYA_AMT_1_1_9,
                   BYAFILE.BYA_O2_1_1_9,
                   BYAFILE.BYA_INT_1_1_9,
                   BYAFILE.BYA_NO_1_1_10,
                   BYAFILE.BYA_AMT_1_1_10,
                   BYAFILE.BYA_O2_1_1_10,
                   BYAFILE.BYA_INT_1_1_10,
                   BYAFILE.BYA_NO_1_1_11,
                   BYAFILE.BYA_AMT_1_1_11,
                   BYAFILE.BYA_O2_1_1_11,
                   BYAFILE.BYA_INT_1_1_11,
                   BYAFILE.BYA_NO_1_2_1,
                   BYAFILE.BYA_AMT_1_2_1,
                   BYAFILE.BYA_O2_1_2_1,
                   BYAFILE.BYA_INT_1_2_1,
                   BYAFILE.BYA_NO_1_2_2,
                   BYAFILE.BYA_AMT_1_2_2,
                   BYAFILE.BYA_O2_1_2_2,
                   BYAFILE.BYA_INT_1_2_2,
                   BYAFILE.BYA_NO_1_2_3,
                   BYAFILE.BYA_AMT_1_2_3,
                   BYAFILE.BYA_O2_1_2_3,
                   BYAFILE.BYA_INT_1_2_3,
                   BYAFILE.BYA_NO_1_2_4,
                   BYAFILE.BYA_AMT_1_2_4,
                   BYAFILE.BYA_O2_1_2_4,
                   BYAFILE.BYA_INT_1_2_4,
                   BYAFILE.BYA_NO_1_2_5,
                   BYAFILE.BYA_AMT_1_2_5,
                   BYAFILE.BYA_O2_1_2_5,
                   BYAFILE.BYA_INT_1_2_5,
                   BYAFILE.BYA_NO_1_2_6,
                   BYAFILE.BYA_AMT_1_2_6,
                   BYAFILE.BYA_O2_1_2_6,
                   BYAFILE.BYA_INT_1_2_6,
                   BYAFILE.BYA_NO_1_2_7,
                   BYAFILE.BYA_AMT_1_2_7,
                   BYAFILE.BYA_O2_1_2_7,
                   BYAFILE.BYA_INT_1_2_7,
                   BYAFILE.BYA_NO_1_2_8,
                   BYAFILE.BYA_AMT_1_2_8,
                   BYAFILE.BYA_O2_1_2_8,
                   BYAFILE.BYA_INT_1_2_8,
                   BYAFILE.BYA_NO_1_2_9,
                   BYAFILE.BYA_AMT_1_2_9,
                   BYAFILE.BYA_O2_1_2_9,
                   BYAFILE.BYA_INT_1_2_9,
                   BYAFILE.BYA_NO_1_2_10,
                   BYAFILE.BYA_AMT_1_2_10,
                   BYAFILE.BYA_O2_1_2_10,
                   BYAFILE.BYA_INT_1_2_10,
                   BYAFILE.BYA_NO_1_2_11,
                   BYAFILE.BYA_AMT_1_2_11,
                   BYAFILE.BYA_O2_1_2_11,
                   BYAFILE.BYA_INT_1_2_11,
                   BYAFILE.BYA_NO_2_1_1,
                   BYAFILE.BYA_AMT_2_1_1,
                   BYAFILE.BYA_O2_2_1_1,
                   BYAFILE.BYA_INT_2_1_1,
                   BYAFILE.BYA_NO_2_1_2,
                   BYAFILE.BYA_AMT_2_1_2,
                   BYAFILE.BYA_O2_2_1_2,
                   BYAFILE.BYA_INT_2_1_2,
                   BYAFILE.BYA_NO_2_1_3,
                   BYAFILE.BYA_AMT_2_1_3,
                   BYAFILE.BYA_O2_2_1_3,
                   BYAFILE.BYA_INT_2_1_3,
                   BYAFILE.BYA_NO_2_1_4,
                   BYAFILE.BYA_AMT_2_1_4,
                   BYAFILE.BYA_O2_2_1_4,
                   BYAFILE.BYA_INT_2_1_4,
                   BYAFILE.BYA_NO_2_1_5,
                   BYAFILE.BYA_AMT_2_1_5,
                   BYAFILE.BYA_O2_2_1_5,
                   BYAFILE.BYA_INT_2_1_5,
                   BYAFILE.BYA_NO_2_1_6,
                   BYAFILE.BYA_AMT_2_1_6,
                   BYAFILE.BYA_O2_2_1_6,
                   BYAFILE.BYA_INT_2_1_6,
                   BYAFILE.BYA_NO_2_1_7,
                   BYAFILE.BYA_AMT_2_1_7,
                   BYAFILE.BYA_O2_2_1_7,
                   BYAFILE.BYA_INT_2_1_7,
                   BYAFILE.BYA_NO_2_1_8,
                   BYAFILE.BYA_AMT_2_1_8,
                   BYAFILE.BYA_O2_2_1_8,
                   BYAFILE.BYA_INT_2_1_8,
                   BYAFILE.BYA_NO_2_1_9,
                   BYAFILE.BYA_AMT_2_1_9,
                   BYAFILE.BYA_O2_2_1_9,
                   BYAFILE.BYA_INT_2_1_9,
                   BYAFILE.BYA_NO_2_1_10,
                   BYAFILE.BYA_AMT_2_1_10,
                   BYAFILE.BYA_O2_2_1_10,
                   BYAFILE.BYA_INT_2_1_10,
                   BYAFILE.BYA_NO_2_1_11,
                   BYAFILE.BYA_AMT_2_1_11,
                   BYAFILE.BYA_O2_2_1_11,
                   BYAFILE.BYA_INT_2_1_11,
                   BYAFILE.BYA_NO_2_2_1,
                   BYAFILE.BYA_AMT_2_2_1,
                   BYAFILE.BYA_O2_2_2_1,
                   BYAFILE.BYA_INT_2_2_1,
                   BYAFILE.BYA_NO_2_2_2,
                   BYAFILE.BYA_AMT_2_2_2,
                   BYAFILE.BYA_O2_2_2_2,
                   BYAFILE.BYA_INT_2_2_2,
                   BYAFILE.BYA_NO_2_2_3,
                   BYAFILE.BYA_AMT_2_2_3,
                   BYAFILE.BYA_O2_2_2_3,
                   BYAFILE.BYA_INT_2_2_3,
                   BYAFILE.BYA_NO_2_2_4,
                   BYAFILE.BYA_AMT_2_2_4,
                   BYAFILE.BYA_O2_2_2_4,
                   BYAFILE.BYA_INT_2_2_4,
                   BYAFILE.BYA_NO_2_2_5,
                   BYAFILE.BYA_AMT_2_2_5,
                   BYAFILE.BYA_O2_2_2_5,
                   BYAFILE.BYA_INT_2_2_5,
                   BYAFILE.BYA_NO_2_2_6,
                   BYAFILE.BYA_AMT_2_2_6,
                   BYAFILE.BYA_O2_2_2_6,
                   BYAFILE.BYA_INT_2_2_6,
                   BYAFILE.BYA_NO_2_2_7,
                   BYAFILE.BYA_AMT_2_2_7,
                   BYAFILE.BYA_O2_2_2_7,
                   BYAFILE.BYA_INT_2_2_7,
                   BYAFILE.BYA_NO_2_2_8,
                   BYAFILE.BYA_AMT_2_2_8,
                   BYAFILE.BYA_O2_2_2_8,
                   BYAFILE.BYA_INT_2_2_8,
                   BYAFILE.BYA_NO_2_2_9,
                   BYAFILE.BYA_AMT_2_2_9,
                   BYAFILE.BYA_O2_2_2_9,
                   BYAFILE.BYA_INT_2_2_9,
                   BYAFILE.BYA_NO_2_2_10,
                   BYAFILE.BYA_AMT_2_2_10,
                   BYAFILE.BYA_O2_2_2_10,
                   BYAFILE.BYA_INT_2_2_10,
                   BYAFILE.BYA_NO_2_2_11,
                   BYAFILE.BYA_AMT_2_2_11,
                   BYAFILE.BYA_O2_2_2_11,
                   BYAFILE.BYA_INT_2_2_11,
                   BYAFILE.BYA_NO_3_1_1,
                   BYAFILE.BYA_AMT_3_1_1,
                   BYAFILE.BYA_O2_3_1_1,
                   BYAFILE.BYA_INT_3_1_1,
                   BYAFILE.BYA_NO_3_1_2,
                   BYAFILE.BYA_AMT_3_1_2,
                   BYAFILE.BYA_O2_3_1_2,
                   BYAFILE.BYA_INT_3_1_2,
                   BYAFILE.BYA_NO_3_1_3,
                   BYAFILE.BYA_AMT_3_1_3,
                   BYAFILE.BYA_O2_3_1_3,
                   BYAFILE.BYA_INT_3_1_3,
                   BYAFILE.BYA_NO_3_1_4,
                   BYAFILE.BYA_AMT_3_1_4,
                   BYAFILE.BYA_O2_3_1_4,
                   BYAFILE.BYA_INT_3_1_4,
                   BYAFILE.BYA_NO_3_1_5,
                   BYAFILE.BYA_AMT_3_1_5,
                   BYAFILE.BYA_O2_3_1_5,
                   BYAFILE.BYA_INT_3_1_5,
                   BYAFILE.BYA_NO_3_1_6,
                   BYAFILE.BYA_AMT_3_1_6,
                   BYAFILE.BYA_O2_3_1_6,
                   BYAFILE.BYA_INT_3_1_6,
                   BYAFILE.BYA_NO_3_1_7,
                   BYAFILE.BYA_AMT_3_1_7,
                   BYAFILE.BYA_O2_3_1_7,
                   BYAFILE.BYA_INT_3_1_7,
                   BYAFILE.BYA_NO_3_1_8,
                   BYAFILE.BYA_AMT_3_1_8,
                   BYAFILE.BYA_O2_3_1_8,
                   BYAFILE.BYA_INT_3_1_8,
                   BYAFILE.BYA_NO_3_1_9,
                   BYAFILE.BYA_AMT_3_1_9,
                   BYAFILE.BYA_O2_3_1_9,
                   BYAFILE.BYA_INT_3_1_9,
                   BYAFILE.BYA_NO_3_1_10,
                   BYAFILE.BYA_AMT_3_1_10,
                   BYAFILE.BYA_O2_3_1_10,
                   BYAFILE.BYA_INT_3_1_10,
                   BYAFILE.BYA_NO_3_1_11,
                   BYAFILE.BYA_AMT_3_1_11,
                   BYAFILE.BYA_O2_3_1_11,
                   BYAFILE.BYA_INT_3_1_11,
                   BYAFILE.BYA_NO_3_2_1,
                   BYAFILE.BYA_AMT_3_2_1,
                   BYAFILE.BYA_O2_3_2_1,
                   BYAFILE.BYA_INT_3_2_1,
                   BYAFILE.BYA_NO_3_2_2,
                   BYAFILE.BYA_AMT_3_2_2,
                   BYAFILE.BYA_O2_3_2_2,
                   BYAFILE.BYA_INT_3_2_2,
                   BYAFILE.BYA_NO_3_2_3,
                   BYAFILE.BYA_AMT_3_2_3,
                   BYAFILE.BYA_O2_3_2_3,
                   BYAFILE.BYA_INT_3_2_3,
                   BYAFILE.BYA_NO_3_2_4,
                   BYAFILE.BYA_AMT_3_2_4,
                   BYAFILE.BYA_O2_3_2_4,
                   BYAFILE.BYA_INT_3_2_4,
                   BYAFILE.BYA_NO_3_2_5,
                   BYAFILE.BYA_AMT_3_2_5,
                   BYAFILE.BYA_O2_3_2_5,
                   BYAFILE.BYA_INT_3_2_5,
                   BYAFILE.BYA_NO_3_2_6,
                   BYAFILE.BYA_AMT_3_2_6,
                   BYAFILE.BYA_O2_3_2_6,
                   BYAFILE.BYA_INT_3_2_6,
                   BYAFILE.BYA_NO_3_2_7,
                   BYAFILE.BYA_AMT_3_2_7,
                   BYAFILE.BYA_O2_3_2_7,
                   BYAFILE.BYA_INT_3_2_7,
                   BYAFILE.BYA_NO_3_2_8,
                   BYAFILE.BYA_AMT_3_2_8,
                   BYAFILE.BYA_O2_3_2_8,
                   BYAFILE.BYA_INT_3_2_8,
                   BYAFILE.BYA_NO_3_2_9,
                   BYAFILE.BYA_AMT_3_2_9,
                   BYAFILE.BYA_O2_3_2_9,
                   BYAFILE.BYA_INT_3_2_9,
                   BYAFILE.BYA_NO_3_2_10,
                   BYAFILE.BYA_AMT_3_2_10,
                   BYAFILE.BYA_O2_3_2_10,
                   BYAFILE.BYA_INT_3_2_10,
                   BYAFILE.BYA_NO_3_2_11,
                   BYAFILE.BYA_AMT_3_2_11,
                   BYAFILE.BYA_O2_3_2_11,
                   BYAFILE.BYA_INT_3_2_11,
                   BYAFILE.BYA_NO_REPO_1_1_1,
                   BYAFILE.BYA_AMT_REPO_1_1_1,
                   BYAFILE.BYA_O2_REPO_1_1_1,
                   BYAFILE.BYA_INT_REPO_1_1_1,
                   BYAFILE.BYA_NO_REPO_1_1_2,
                   BYAFILE.BYA_AMT_REPO_1_1_2,
                   BYAFILE.BYA_O2_REPO_1_1_2,
                   BYAFILE.BYA_INT_REPO_1_1_2,
                   BYAFILE.BYA_NO_REPO_1_1_3,
                   BYAFILE.BYA_AMT_REPO_1_1_3,
                   BYAFILE.BYA_O2_REPO_1_1_3,
                   BYAFILE.BYA_INT_REPO_1_1_3,
                   BYAFILE.BYA_NO_REPO_1_1_4,
                   BYAFILE.BYA_AMT_REPO_1_1_4,
                   BYAFILE.BYA_O2_REPO_1_1_4,
                   BYAFILE.BYA_INT_REPO_1_1_4,
                   BYAFILE.BYA_NO_REPO_1_1_5,
                   BYAFILE.BYA_AMT_REPO_1_1_5,
                   BYAFILE.BYA_O2_REPO_1_1_5,
                   BYAFILE.BYA_INT_REPO_1_1_5,
                   BYAFILE.BYA_NO_REPO_1_2_1,
                   BYAFILE.BYA_AMT_REPO_1_2_1,
                   BYAFILE.BYA_O2_REPO_1_2_1,
                   BYAFILE.BYA_INT_REPO_1_2_1,
                   BYAFILE.BYA_NO_REPO_1_2_2,
                   BYAFILE.BYA_AMT_REPO_1_2_2,
                   BYAFILE.BYA_O2_REPO_1_2_2,
                   BYAFILE.BYA_INT_REPO_1_2_2,
                   BYAFILE.BYA_NO_REPO_1_2_3,
                   BYAFILE.BYA_AMT_REPO_1_2_3,
                   BYAFILE.BYA_O2_REPO_1_2_3,
                   BYAFILE.BYA_INT_REPO_1_2_3,
                   BYAFILE.BYA_NO_REPO_1_2_4,
                   BYAFILE.BYA_AMT_REPO_1_2_4,
                   BYAFILE.BYA_O2_REPO_1_2_4,
                   BYAFILE.BYA_INT_REPO_1_2_4,
                   BYAFILE.BYA_NO_REPO_1_2_5,
                   BYAFILE.BYA_AMT_REPO_1_2_5,
                   BYAFILE.BYA_O2_REPO_1_2_5,
                   BYAFILE.BYA_INT_REPO_1_2_5,
                   BYAFILE.BYA_NO_REPO_2_1_1,
                   BYAFILE.BYA_AMT_REPO_2_1_1,
                   BYAFILE.BYA_O2_REPO_2_1_1,
                   BYAFILE.BYA_INT_REPO_2_1_1,
                   BYAFILE.BYA_NO_REPO_2_1_2,
                   BYAFILE.BYA_AMT_REPO_2_1_2,
                   BYAFILE.BYA_O2_REPO_2_1_2,
                   BYAFILE.BYA_INT_REPO_2_1_2,
                   BYAFILE.BYA_NO_REPO_2_1_3,
                   BYAFILE.BYA_AMT_REPO_2_1_3,
                   BYAFILE.BYA_O2_REPO_2_1_3,
                   BYAFILE.BYA_INT_REPO_2_1_3,
                   BYAFILE.BYA_NO_REPO_2_1_4,
                   BYAFILE.BYA_AMT_REPO_2_1_4,
                   BYAFILE.BYA_O2_REPO_2_1_4,
                   BYAFILE.BYA_INT_REPO_2_1_4,
                   BYAFILE.BYA_NO_REPO_2_1_5,
                   BYAFILE.BYA_AMT_REPO_2_1_5,
                   BYAFILE.BYA_O2_REPO_2_1_5,
                   BYAFILE.BYA_INT_REPO_2_1_5,
                   BYAFILE.BYA_NO_REPO_2_2_1,
                   BYAFILE.BYA_AMT_REPO_2_2_1,
                   BYAFILE.BYA_O2_REPO_2_2_1,
                   BYAFILE.BYA_INT_REPO_2_2_1,
                   BYAFILE.BYA_NO_REPO_2_2_2,
                   BYAFILE.BYA_AMT_REPO_2_2_2,
                   BYAFILE.BYA_O2_REPO_2_2_2,
                   BYAFILE.BYA_INT_REPO_2_2_2,
                   BYAFILE.BYA_NO_REPO_2_2_3,
                   BYAFILE.BYA_AMT_REPO_2_2_3,
                   BYAFILE.BYA_O2_REPO_2_2_3,
                   BYAFILE.BYA_INT_REPO_2_2_3,
                   BYAFILE.BYA_NO_REPO_2_2_4,
                   BYAFILE.BYA_AMT_REPO_2_2_4,
                   BYAFILE.BYA_O2_REPO_2_2_4,
                   BYAFILE.BYA_INT_REPO_2_2_4,
                   BYAFILE.BYA_NO_REPO_2_2_5,
                   BYAFILE.BYA_AMT_REPO_2_2_5,
                   BYAFILE.BYA_O2_REPO_2_2_5,
                   BYAFILE.BYA_INT_REPO_2_2_5,
                   BYAFILE.BYA_NO_REPO_3_1_1,
                   BYAFILE.BYA_AMT_REPO_3_1_1,
                   BYAFILE.BYA_O2_REPO_3_1_1,
                   BYAFILE.BYA_INT_REPO_3_1_1,
                   BYAFILE.BYA_NO_REPO_3_1_2,
                   BYAFILE.BYA_AMT_REPO_3_1_2,
                   BYAFILE.BYA_O2_REPO_3_1_2,
                   BYAFILE.BYA_INT_REPO_3_1_2,
                   BYAFILE.BYA_NO_REPO_3_1_3,
                   BYAFILE.BYA_AMT_REPO_3_1_3,
                   BYAFILE.BYA_O2_REPO_3_1_3,
                   BYAFILE.BYA_INT_REPO_3_1_3,
                   BYAFILE.BYA_NO_REPO_3_1_4,
                   BYAFILE.BYA_AMT_REPO_3_1_4,
                   BYAFILE.BYA_O2_REPO_3_1_4,
                   BYAFILE.BYA_INT_REPO_3_1_4,
                   BYAFILE.BYA_NO_REPO_3_1_5,
                   BYAFILE.BYA_AMT_REPO_3_1_5,
                   BYAFILE.BYA_O2_REPO_3_1_5,
                   BYAFILE.BYA_INT_REPO_3_1_5,
                   BYAFILE.BYA_NO_REPO_3_2_1,
                   BYAFILE.BYA_AMT_REPO_3_2_1,
                   BYAFILE.BYA_O2_REPO_3_2_1,
                   BYAFILE.BYA_INT_REPO_3_2_1,
                   BYAFILE.BYA_NO_REPO_3_2_2,
                   BYAFILE.BYA_AMT_REPO_3_2_2,
                   BYAFILE.BYA_O2_REPO_3_2_2,
                   BYAFILE.BYA_INT_REPO_3_2_2,
                   BYAFILE.BYA_NO_REPO_3_2_3,
                   BYAFILE.BYA_AMT_REPO_3_2_3,
                   BYAFILE.BYA_O2_REPO_3_2_3,
                   BYAFILE.BYA_INT_REPO_3_2_3,
                   BYAFILE.BYA_NO_REPO_3_2_4,
                   BYAFILE.BYA_AMT_REPO_3_2_4,
                   BYAFILE.BYA_O2_REPO_3_2_4,
                   BYAFILE.BYA_INT_REPO_3_2_4,
                   BYAFILE.BYA_NO_REPO_3_2_5,
                   BYAFILE.BYA_AMT_REPO_3_2_5,
                   BYAFILE.BYA_O2_REPO_3_2_5,
                   BYAFILE.BYA_INT_REPO_3_2_5,
                   BYAFILE.BYA_NO_PL_1_1_1,
                   BYAFILE.BYA_AMT_PL_1_1_1,
                   BYAFILE.BYA_O2_PL_1_1_1,
                   BYAFILE.BYA_INT_PL_1_1_1,
                   BYAFILE.BYA_NO_PL_1_1_2,
                   BYAFILE.BYA_AMT_PL_1_1_2,
                   BYAFILE.BYA_O2_PL_1_1_2,
                   BYAFILE.BYA_INT_PL_1_1_2,
                   BYAFILE.BYA_NO_PL_1_1_3,
                   BYAFILE.BYA_AMT_PL_1_1_3,
                   BYAFILE.BYA_O2_PL_1_1_3,
                   BYAFILE.BYA_INT_PL_1_1_3,
                   BYAFILE.BYA_NO_PL_1_1_4,
                   BYAFILE.BYA_AMT_PL_1_1_4,
                   BYAFILE.BYA_O2_PL_1_1_4,
                   BYAFILE.BYA_INT_PL_1_1_4,
                   BYAFILE.BYA_NO_PL_1_1_5,
                   BYAFILE.BYA_AMT_PL_1_1_5,
                   BYAFILE.BYA_O2_PL_1_1_5,
                   BYAFILE.BYA_INT_PL_1_1_5,
                   BYAFILE.BYA_NO_PL_1_1_6,
                   BYAFILE.BYA_AMT_PL_1_1_6,
                   BYAFILE.BYA_O2_PL_1_1_6,
                   BYAFILE.BYA_INT_PL_1_1_6,
                   BYAFILE.BYA_NO_PL_1_1_7,
                   BYAFILE.BYA_AMT_PL_1_1_7,
                   BYAFILE.BYA_O2_PL_1_1_7,
                   BYAFILE.BYA_INT_PL_1_1_7,
                   BYAFILE.BYA_NO_PL_1_1_8,
                   BYAFILE.BYA_AMT_PL_1_1_8,
                   BYAFILE.BYA_O2_PL_1_1_8,
                   BYAFILE.BYA_INT_PL_1_1_8,
                   BYAFILE.BYA_NO_PL_1_1_9,
                   BYAFILE.BYA_AMT_PL_1_1_9,
                   BYAFILE.BYA_O2_PL_1_1_9,
                   BYAFILE.BYA_INT_PL_1_1_9,
                   BYAFILE.BYA_NO_PL_1_1_10,
                   BYAFILE.BYA_AMT_PL_1_1_10,
                   BYAFILE.BYA_O2_PL_1_1_10,
                   BYAFILE.BYA_INT_PL_1_1_10,
                   BYAFILE.BYA_NO_PL_1_2_1,
                   BYAFILE.BYA_AMT_PL_1_2_1,
                   BYAFILE.BYA_O2_PL_1_2_1,
                   BYAFILE.BYA_INT_PL_1_2_1,
                   BYAFILE.BYA_NO_PL_1_2_2,
                   BYAFILE.BYA_AMT_PL_1_2_2,
                   BYAFILE.BYA_O2_PL_1_2_2,
                   BYAFILE.BYA_INT_PL_1_2_2,
                   BYAFILE.BYA_NO_PL_1_2_3,
                   BYAFILE.BYA_AMT_PL_1_2_3,
                   BYAFILE.BYA_O2_PL_1_2_3,
                   BYAFILE.BYA_INT_PL_1_2_3,
                   BYAFILE.BYA_NO_PL_1_2_4,
                   BYAFILE.BYA_AMT_PL_1_2_4,
                   BYAFILE.BYA_O2_PL_1_2_4,
                   BYAFILE.BYA_INT_PL_1_2_4,
                   BYAFILE.BYA_NO_PL_1_2_5,
                   BYAFILE.BYA_AMT_PL_1_2_5,
                   BYAFILE.BYA_O2_PL_1_2_5,
                   BYAFILE.BYA_INT_PL_1_2_5,
                   BYAFILE.BYA_NO_PL_1_2_6,
                   BYAFILE.BYA_AMT_PL_1_2_6,
                   BYAFILE.BYA_O2_PL_1_2_6,
                   BYAFILE.BYA_INT_PL_1_2_6,
                   BYAFILE.BYA_NO_PL_1_2_7,
                   BYAFILE.BYA_AMT_PL_1_2_7,
                   BYAFILE.BYA_O2_PL_1_2_7,
                   BYAFILE.BYA_INT_PL_1_2_7,
                   BYAFILE.BYA_NO_PL_1_2_8,
                   BYAFILE.BYA_AMT_PL_1_2_8,
                   BYAFILE.BYA_O2_PL_1_2_8,
                   BYAFILE.BYA_INT_PL_1_2_8,
                   BYAFILE.BYA_NO_PL_1_2_9,
                   BYAFILE.BYA_AMT_PL_1_2_9,
                   BYAFILE.BYA_O2_PL_1_2_9,
                   BYAFILE.BYA_INT_PL_1_2_9,
                   BYAFILE.BYA_NO_PL_1_2_10,
                   BYAFILE.BYA_AMT_PL_1_2_10,
                   BYAFILE.BYA_O2_PL_1_2_10,
                   BYAFILE.BYA_INT_PL_1_2_10,
                   BYAFILE.BYA_NO_PL_2_1_1,
                   BYAFILE.BYA_AMT_PL_2_1_1,
                   BYAFILE.BYA_O2_PL_2_1_1,
                   BYAFILE.BYA_INT_PL_2_1_1,
                   BYAFILE.BYA_NO_PL_2_1_2,
                   BYAFILE.BYA_AMT_PL_2_1_2,
                   BYAFILE.BYA_O2_PL_2_1_2,
                   BYAFILE.BYA_INT_PL_2_1_2,
                   BYAFILE.BYA_NO_PL_2_1_3,
                   BYAFILE.BYA_AMT_PL_2_1_3,
                   BYAFILE.BYA_O2_PL_2_1_3,
                   BYAFILE.BYA_INT_PL_2_1_3,
                   BYAFILE.BYA_NO_PL_2_1_4,
                   BYAFILE.BYA_AMT_PL_2_1_4,
                   BYAFILE.BYA_O2_PL_2_1_4,
                   BYAFILE.BYA_INT_PL_2_1_4,
                   BYAFILE.BYA_NO_PL_2_1_5,
                   BYAFILE.BYA_AMT_PL_2_1_5,
                   BYAFILE.BYA_O2_PL_2_1_5,
                   BYAFILE.BYA_INT_PL_2_1_5,
                   BYAFILE.BYA_NO_PL_2_1_6,
                   BYAFILE.BYA_AMT_PL_2_1_6,
                   BYAFILE.BYA_O2_PL_2_1_6,
                   BYAFILE.BYA_INT_PL_2_1_6,
                   BYAFILE.BYA_NO_PL_2_1_7,
                   BYAFILE.BYA_AMT_PL_2_1_7,
                   BYAFILE.BYA_O2_PL_2_1_7,
                   BYAFILE.BYA_INT_PL_2_1_7,
                   BYAFILE.BYA_NO_PL_2_1_8,
                   BYAFILE.BYA_AMT_PL_2_1_8,
                   BYAFILE.BYA_O2_PL_2_1_8,
                   BYAFILE.BYA_INT_PL_2_1_8,
                   BYAFILE.BYA_NO_PL_2_1_9,
                   BYAFILE.BYA_AMT_PL_2_1_9,
                   BYAFILE.BYA_O2_PL_2_1_9,
                   BYAFILE.BYA_INT_PL_2_1_9,
                   BYAFILE.BYA_NO_PL_2_1_10,
                   BYAFILE.BYA_AMT_PL_2_1_10,
                   BYAFILE.BYA_O2_PL_2_1_10,
                   BYAFILE.BYA_INT_PL_2_1_10,
                   BYAFILE.BYA_NO_PL_2_2_1,
                   BYAFILE.BYA_AMT_PL_2_2_1,
                   BYAFILE.BYA_O2_PL_2_2_1,
                   BYAFILE.BYA_INT_PL_2_2_1,
                   BYAFILE.BYA_NO_PL_2_2_2,
                   BYAFILE.BYA_AMT_PL_2_2_2,
                   BYAFILE.BYA_O2_PL_2_2_2,
                   BYAFILE.BYA_INT_PL_2_2_2,
                   BYAFILE.BYA_NO_PL_2_2_3,
                   BYAFILE.BYA_AMT_PL_2_2_3,
                   BYAFILE.BYA_O2_PL_2_2_3,
                   BYAFILE.BYA_INT_PL_2_2_3,
                   BYAFILE.BYA_NO_PL_2_2_4,
                   BYAFILE.BYA_AMT_PL_2_2_4,
                   BYAFILE.BYA_O2_PL_2_2_4,
                   BYAFILE.BYA_INT_PL_2_2_4,
                   BYAFILE.BYA_NO_PL_2_2_5,
                   BYAFILE.BYA_AMT_PL_2_2_5,
                   BYAFILE.BYA_O2_PL_2_2_5,
                   BYAFILE.BYA_INT_PL_2_2_5,
                   BYAFILE.BYA_NO_PL_2_2_6,
                   BYAFILE.BYA_AMT_PL_2_2_6,
                   BYAFILE.BYA_O2_PL_2_2_6,
                   BYAFILE.BYA_INT_PL_2_2_6,
                   BYAFILE.BYA_NO_PL_2_2_7,
                   BYAFILE.BYA_AMT_PL_2_2_7,
                   BYAFILE.BYA_O2_PL_2_2_7,
                   BYAFILE.BYA_INT_PL_2_2_7,
                   BYAFILE.BYA_NO_PL_2_2_8,
                   BYAFILE.BYA_AMT_PL_2_2_8,
                   BYAFILE.BYA_O2_PL_2_2_8,
                   BYAFILE.BYA_INT_PL_2_2_8,
                   BYAFILE.BYA_NO_PL_2_2_9,
                   BYAFILE.BYA_AMT_PL_2_2_9,
                   BYAFILE.BYA_O2_PL_2_2_9,
                   BYAFILE.BYA_INT_PL_2_2_9,
                   BYAFILE.BYA_NO_PL_2_2_10,
                   BYAFILE.BYA_AMT_PL_2_2_10,
                   BYAFILE.BYA_O2_PL_2_2_10,
                   BYAFILE.BYA_INT_PL_2_2_10,
                   BYAFILE.BYA_NO_PL_3_1_1,
                   BYAFILE.BYA_AMT_PL_3_1_1,
                   BYAFILE.BYA_O2_PL_3_1_1,
                   BYAFILE.BYA_INT_PL_3_1_1,
                   BYAFILE.BYA_NO_PL_3_1_2,
                   BYAFILE.BYA_AMT_PL_3_1_2,
                   BYAFILE.BYA_O2_PL_3_1_2,
                   BYAFILE.BYA_INT_PL_3_1_2,
                   BYAFILE.BYA_NO_PL_3_1_3,
                   BYAFILE.BYA_AMT_PL_3_1_3,
                   BYAFILE.BYA_O2_PL_3_1_3,
                   BYAFILE.BYA_INT_PL_3_1_3,
                   BYAFILE.BYA_NO_PL_3_1_4,
                   BYAFILE.BYA_AMT_PL_3_1_4,
                   BYAFILE.BYA_O2_PL_3_1_4,
                   BYAFILE.BYA_INT_PL_3_1_4,
                   BYAFILE.BYA_NO_PL_3_1_5,
                   BYAFILE.BYA_AMT_PL_3_1_5,
                   BYAFILE.BYA_O2_PL_3_1_5,
                   BYAFILE.BYA_INT_PL_3_1_5,
                   BYAFILE.BYA_NO_PL_3_1_6,
                   BYAFILE.BYA_AMT_PL_3_1_6,
                   BYAFILE.BYA_O2_PL_3_1_6,
                   BYAFILE.BYA_INT_PL_3_1_6,
                   BYAFILE.BYA_NO_PL_3_1_7,
                   BYAFILE.BYA_AMT_PL_3_1_7,
                   BYAFILE.BYA_O2_PL_3_1_7,
                   BYAFILE.BYA_INT_PL_3_1_7,
                   BYAFILE.BYA_NO_PL_3_1_8,
                   BYAFILE.BYA_AMT_PL_3_1_8,
                   BYAFILE.BYA_O2_PL_3_1_8,
                   BYAFILE.BYA_INT_PL_3_1_8,
                   BYAFILE.BYA_NO_PL_3_1_9,
                   BYAFILE.BYA_AMT_PL_3_1_9,
                   BYAFILE.BYA_O2_PL_3_1_9,
                   BYAFILE.BYA_INT_PL_3_1_9,
                   BYAFILE.BYA_NO_PL_3_1_10,
                   BYAFILE.BYA_AMT_PL_3_1_10,
                   BYAFILE.BYA_O2_PL_3_1_10,
                   BYAFILE.BYA_INT_PL_3_1_10,
                   BYAFILE.BYA_NO_PL_3_2_1,
                   BYAFILE.BYA_AMT_PL_3_2_1,
                   BYAFILE.BYA_O2_PL_3_2_1,
                   BYAFILE.BYA_INT_PL_3_2_1,
                   BYAFILE.BYA_NO_PL_3_2_2,
                   BYAFILE.BYA_AMT_PL_3_2_2,
                   BYAFILE.BYA_O2_PL_3_2_2,
                   BYAFILE.BYA_INT_PL_3_2_2,
                   BYAFILE.BYA_NO_PL_3_2_3,
                   BYAFILE.BYA_AMT_PL_3_2_3,
                   BYAFILE.BYA_O2_PL_3_2_3,
                   BYAFILE.BYA_INT_PL_3_2_3,
                   BYAFILE.BYA_NO_PL_3_2_4,
                   BYAFILE.BYA_AMT_PL_3_2_4,
                   BYAFILE.BYA_O2_PL_3_2_4,
                   BYAFILE.BYA_INT_PL_3_2_4,
                   BYAFILE.BYA_NO_PL_3_2_5,
                   BYAFILE.BYA_AMT_PL_3_2_5,
                   BYAFILE.BYA_O2_PL_3_2_5,
                   BYAFILE.BYA_INT_PL_3_2_5,
                   BYAFILE.BYA_NO_PL_3_2_6,
                   BYAFILE.BYA_AMT_PL_3_2_6,
                   BYAFILE.BYA_O2_PL_3_2_6,
                   BYAFILE.BYA_INT_PL_3_2_6,
                   BYAFILE.BYA_NO_PL_3_2_7,
                   BYAFILE.BYA_AMT_PL_3_2_7,
                   BYAFILE.BYA_O2_PL_3_2_7,
                   BYAFILE.BYA_INT_PL_3_2_7,
                   BYAFILE.BYA_NO_PL_3_2_8,
                   BYAFILE.BYA_AMT_PL_3_2_8,
                   BYAFILE.BYA_O2_PL_3_2_8,
                   BYAFILE.BYA_INT_PL_3_2_8,
                   BYAFILE.BYA_NO_PL_3_2_9,
                   BYAFILE.BYA_AMT_PL_3_2_9,
                   BYAFILE.BYA_O2_PL_3_2_9,
                   BYAFILE.BYA_INT_PL_3_2_9,
                   BYAFILE.BYA_NO_PL_3_2_10,
                   BYAFILE.BYA_AMT_PL_3_2_10,
                   BYAFILE.BYA_O2_PL_3_2_10,
                   BYAFILE.BYA_INT_PL_3_2_10,
                   BYAFILE.BYA_DLRVPRIN_1_1_1,
                   BYAFILE.BYA_DLRVPRIN_1_1_2,
                   BYAFILE.BYA_DLRVPRIN_1_1_3,
                   BYAFILE.BYA_DLRVCHGS_1_1_1,
                   BYAFILE.BYA_DLRVCHGS_1_1_2,
                   BYAFILE.BYA_DLRVCHGS_1_1_3,
                   BYAFILE.BYA_DLPARVHELD_1_1,
                   BYAFILE.BYA_DLRVPRIN_1_2_1,
                   BYAFILE.BYA_DLRVPRIN_1_2_2,
                   BYAFILE.BYA_DLRVPRIN_1_2_3,
                   BYAFILE.BYA_DLRVCHGS_1_2_1,
                   BYAFILE.BYA_DLRVCHGS_1_2_2,
                   BYAFILE.BYA_DLRVCHGS_1_2_3,
                   BYAFILE.BYA_DLPARVHELD_1_2,
                   BYAFILE.BYA_DLRVPRIN_2_1_1,
                   BYAFILE.BYA_DLRVPRIN_2_1_2,
                   BYAFILE.BYA_DLRVPRIN_2_1_3,
                   BYAFILE.BYA_DLRVCHGS_2_1_1,
                   BYAFILE.BYA_DLRVCHGS_2_1_2,
                   BYAFILE.BYA_DLRVCHGS_2_1_3,
                   BYAFILE.BYA_DLPARVHELD_2_1,
                   BYAFILE.BYA_DLRVPRIN_2_2_1,
                   BYAFILE.BYA_DLRVPRIN_2_2_2,
                   BYAFILE.BYA_DLRVPRIN_2_2_3,
                   BYAFILE.BYA_DLRVCHGS_2_2_1,
                   BYAFILE.BYA_DLRVCHGS_2_2_2,
                   BYAFILE.BYA_DLRVCHGS_2_2_3,
                   BYAFILE.BYA_DLPARVHELD_2_2,
                   BYAFILE.BYA_DLRVPRIN_3_1_1,
                   BYAFILE.BYA_DLRVPRIN_3_1_2,
                   BYAFILE.BYA_DLRVPRIN_3_1_3,
                   BYAFILE.BYA_DLRVCHGS_3_1_1,
                   BYAFILE.BYA_DLRVCHGS_3_1_2,
                   BYAFILE.BYA_DLRVCHGS_3_1_3,
                   BYAFILE.BYA_DLPARVHELD_3_1,
                   BYAFILE.BYA_DLRVPRIN_3_2_1,
                   BYAFILE.BYA_DLRVPRIN_3_2_2,
                   BYAFILE.BYA_DLRVPRIN_3_2_3,
                   BYAFILE.BYA_DLRVCHGS_3_2_1,
                   BYAFILE.BYA_DLRVCHGS_3_2_2,
                   BYAFILE.BYA_DLRVCHGS_3_2_3,
                   BYAFILE.BYA_DLPARVHELD_3_2
            INTO :QBYA-REC
            FROM DBO.BYAFILE
            WHERE BYAFILE.BYA_DATE_CCYYMM = :QBYA-DATE-CCYYMM
              AND BYAFILE.BYA_BRNO = :QBYA-BRNO 
              AND BYAFILE.BYA_CLASS = :QBYA-CLASS
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-BYA1-FILE.

           IF ( IO-FG = 0 )
              PERFORM GET-BYA-FIELDS.

      *-----------------------------------------------------------------
       READ-BYA1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE BYA-PATH-SQL TO E-FILE.
           MOVE BYA1-KEY TO E-KEYX.
           INITIALIZE QBYA-REC.

           EXEC SQL SET CONNECTION BY1_CONNECT END-EXEC.

           IF ( BYA1-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH BYA1_CURSOR INTO :QBYA-REC
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-BYA1-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-BYA-FIELDS.

           EXEC SQL SET CONNECTION C1 END-EXEC.

      *-----------------------------------------------------------------
       CLOSE-BYA1-FILE.
           PERFORM CLOSE-IT.
           IF ( BYA1-CURSOR-STAT-GOOD )
              EXEC SQL SET CONNECTION BY1_CONNECT END-EXEC
              EXEC SQL
                   CLOSE BYA1_CURSOR
              END-EXEC
              EXEC SQL SET CONNECTION C1 END-EXEC
              MOVE STAT---BAD TO BYA1-CURSOR-STAT.

      *=================================================================
       LOAD-BYB1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( BY-PATH-OVERRIDE = "Y" )
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( BY-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO BYB-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO BYB-ALL-MACHINE
              MOVE BYB-ALL-PATH            TO BYB-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( BY-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO BYB-ALL-BASE
              MOVE FILPATH-MACHINE         TO BYB-ALL-MACHINE
              MOVE BYB-ALL-PATH            TO BYB-PATH.

      *-----------------------------------------------------------------
       OPEN-BYB1-FILE.
           PERFORM LOAD-BYB1-FILE.
           PERFORM OPEN-IT.
           MOVE BYB-PATH-SQL TO E-FILE.

      *-----------------------------------------------------------------
       READ-BYB1-FILE.
           PERFORM READ-IT.
           MOVE BYB-PATH-SQL TO E-FILE.
           MOVE BYB1-KEY TO E-KEYX.
           INITIALIZE QBYB-REC.

           MOVE BYB-DATE-CCYYMM      TO QBYB-DATE-CCYYMM.
           MOVE BYB-BRNO             TO QBYB-BRNO.
           MOVE BYB-CLASS            TO QBYB-CLASS.

           EXEC SQL
            SELECT BYBFILE.BYB_DATE_CCYYMM,
                   BYBFILE.BYB_BRNO,
                   BYBFILE.BYB_CLASS,
                   BYBFILE.BYB_PL_PRINCOL_1_1,
                   BYBFILE.BYB_PL_INTCOL_1_1,
                   BYBFILE.BYB_PL_LATECOL_1_1,
                   BYBFILE.BYB_PL_O2COL_1_1,
                   BYBFILE.BYB_PL_PRINCOL_1_2,
                   BYBFILE.BYB_PL_INTCOL_1_2,
                   BYBFILE.BYB_PL_LATECOL_1_2,
                   BYBFILE.BYB_PL_O2COL_1_2,
                   BYBFILE.BYB_PL_PRINCOL_1_3,
                   BYBFILE.BYB_PL_INTCOL_1_3,
                   BYBFILE.BYB_PL_LATECOL_1_3,
                   BYBFILE.BYB_PL_O2COL_1_3,
                   BYBFILE.BYB_PL_PRINCOL_1_4,
                   BYBFILE.BYB_PL_INTCOL_1_4,
                   BYBFILE.BYB_PL_LATECOL_1_4,
                   BYBFILE.BYB_PL_O2COL_1_4,
                   BYBFILE.BYB_PL_PRINCOL_1_5,
                   BYBFILE.BYB_PL_INTCOL_1_5,
                   BYBFILE.BYB_PL_LATECOL_1_5,
                   BYBFILE.BYB_PL_O2COL_1_5,
                   BYBFILE.BYB_AMT_SOLD_1,
                   BYBFILE.BYB_AMT_TRANS_1,
                   BYBFILE.BYB_AMT_PURCH_1,
                   BYBFILE.BYB_NO_SOLD_1,
                   BYBFILE.BYB_NO_TRANS_1,
                   BYBFILE.BYB_NO_PURCH_1,
                   BYBFILE.BYB_OTHCOL_1,
                   BYBFILE.BYB_OT2COL_1,
                   BYBFILE.BYB_INTCOL_1,
                   BYBFILE.BYB_LATECOL_1,
                   BYBFILE.BYB_PRINCOL_1,
                   BYBFILE.BYB_DEFCOL_1,
                   BYBFILE.BYB_RNPRIN_1,
                   BYBFILE.BYB_INTREB_1,
                   BYBFILE.BYB_CLREB1_1,
                   BYBFILE.BYB_CLREB2_1,
                   BYBFILE.BYB_CLREB3_1,
                   BYBFILE.BYB_CLREB4_1,
                   BYBFILE.BYB_AHREB_1,
                   BYBFILE.BYB_PPREB_1,
                   BYBFILE.BYB_O1REB_1,
                   BYBFILE.BYB_O2REB_1,
                   BYBFILE.BYB_O3REB_1,
                   BYBFILE.BYB_O4REB_1,
                   BYBFILE.BYB_O5REB_1,
                   BYBFILE.BYB_O6REB_1,
                   BYBFILE.BYB_O7REB_1,
                   BYBFILE.BYB_SCREB_1,
                   BYBFILE.BYB_MFREB_1,
                   BYBFILE.BYB_DFREB_1,
                   BYBFILE.BYB_UN7_REB_1,
                   BYBFILE.BYB_UN8_REB_1,
                   BYBFILE.BYB_UN9_REB_1,
                   BYBFILE.BYB_UN10_REB_1,
                   BYBFILE.BYB_PART_CHGOFF_1,
                   BYBFILE.BYB_ALLOTMENTS_1,
                   BYBFILE.BYB_INS_NO_ELIG_1_1,
                   BYBFILE.BYB_INS_NO_WROTE_1_1,
                   BYBFILE.BYB_INS_NO_NOTWROTE_1_1,
                   BYBFILE.BYB_INS_AMT_WROTE_1_1,
                   BYBFILE.BYB_INS_NO_ELIG_1_2,
                   BYBFILE.BYB_INS_NO_WROTE_1_2,
                   BYBFILE.BYB_INS_NO_NOTWROTE_1_2,
                   BYBFILE.BYB_INS_AMT_WROTE_1_2,
                   BYBFILE.BYB_INS_NO_ELIG_1_3,
                   BYBFILE.BYB_INS_NO_WROTE_1_3,
                   BYBFILE.BYB_INS_NO_NOTWROTE_1_3,
                   BYBFILE.BYB_INS_AMT_WROTE_1_3,
                   BYBFILE.BYB_INS_NO_ELIG_1_4,
                   BYBFILE.BYB_INS_NO_WROTE_1_4,
                   BYBFILE.BYB_INS_NO_NOTWROTE_1_4,
                   BYBFILE.BYB_INS_AMT_WROTE_1_4,
                   BYBFILE.BYB_INS_NO_ELIG_1_5,
                   BYBFILE.BYB_INS_NO_WROTE_1_5,
                   BYBFILE.BYB_INS_NO_NOTWROTE_1_5,
                   BYBFILE.BYB_INS_AMT_WROTE_1_5,
                   BYBFILE.BYB_INS_NO_ELIG_1_6,
                   BYBFILE.BYB_INS_NO_WROTE_1_6,
                   BYBFILE.BYB_INS_NO_NOTWROTE_1_6,
                   BYBFILE.BYB_INS_AMT_WROTE_1_6,
                   BYBFILE.BYB_INS_NO_ELIG_1_7,
                   BYBFILE.BYB_INS_NO_WROTE_1_7,
                   BYBFILE.BYB_INS_NO_NOTWROTE_1_7,
                   BYBFILE.BYB_INS_AMT_WROTE_1_7,
                   BYBFILE.BYB_INS_NO_ELIG_1_8,
                   BYBFILE.BYB_INS_NO_WROTE_1_8,
                   BYBFILE.BYB_INS_NO_NOTWROTE_1_8,
                   BYBFILE.BYB_INS_AMT_WROTE_1_8,
                   BYBFILE.BYB_INS_NO_ELIG_1_9,
                   BYBFILE.BYB_INS_NO_WROTE_1_9,
                   BYBFILE.BYB_INS_NO_NOTWROTE_1_9,
                   BYBFILE.BYB_INS_AMT_WROTE_1_9,
                   BYBFILE.BYB_INS_NO_ELIG_1_10,
                   BYBFILE.BYB_INS_NO_WROTE_1_10,
                   BYBFILE.BYB_INS_NO_NOTWROTE_1_10,
                   BYBFILE.BYB_INS_AMT_WROTE_1_10,
                   BYBFILE.BYB_FEE_NO_ELIG_1_1,
                   BYBFILE.BYB_FEE_NO_WROTE_1_1,
                   BYBFILE.BYB_FEE_NO_NOTWROTE_1_1,
                   BYBFILE.BYB_FEE_AMT_WROTE_1_1,
                   BYBFILE.BYB_FEE_NO_ELIG_1_2,
                   BYBFILE.BYB_FEE_NO_WROTE_1_2,
                   BYBFILE.BYB_FEE_NO_NOTWROTE_1_2,
                   BYBFILE.BYB_FEE_AMT_WROTE_1_2,
                   BYBFILE.BYB_FEE_NO_ELIG_1_3,
                   BYBFILE.BYB_FEE_NO_WROTE_1_3,
                   BYBFILE.BYB_FEE_NO_NOTWROTE_1_3,
                   BYBFILE.BYB_FEE_AMT_WROTE_1_3,
                   BYBFILE.BYB_OTH_NO_COL_1,
                   BYBFILE.BYB_OT2_NO_COL_1,
                   BYBFILE.BYB_INT_NO_COL_1,
                   BYBFILE.BYB_LATE_NO_COL_1,
                   BYBFILE.BYB_OTH_NO_WAIVED_1,
                   BYBFILE.BYB_OT2_NO_WAIVED_1,
                   BYBFILE.BYB_INT_NO_WAIVED_1,
                   BYBFILE.BYB_LATE_NO_WAIVED_1,
                   BYBFILE.BYB_OTH_AMT_WAIVED_1,
                   BYBFILE.BYB_OT2_AMT_WAIVED_1,
                   BYBFILE.BYB_INT_AMT_WAIVED_1,
                   BYBFILE.BYB_LATE_AMT_WAIVED_1,
                   BYBFILE.BYB_PL_PRINCOL_2_1,
                   BYBFILE.BYB_PL_INTCOL_2_1,
                   BYBFILE.BYB_PL_LATECOL_2_1,
                   BYBFILE.BYB_PL_O2COL_2_1,
                   BYBFILE.BYB_PL_PRINCOL_2_2,
                   BYBFILE.BYB_PL_INTCOL_2_2,
                   BYBFILE.BYB_PL_LATECOL_2_2,
                   BYBFILE.BYB_PL_O2COL_2_2,
                   BYBFILE.BYB_PL_PRINCOL_2_3,
                   BYBFILE.BYB_PL_INTCOL_2_3,
                   BYBFILE.BYB_PL_LATECOL_2_3,
                   BYBFILE.BYB_PL_O2COL_2_3,
                   BYBFILE.BYB_PL_PRINCOL_2_4,
                   BYBFILE.BYB_PL_INTCOL_2_4,
                   BYBFILE.BYB_PL_LATECOL_2_4,
                   BYBFILE.BYB_PL_O2COL_2_4,
                   BYBFILE.BYB_PL_PRINCOL_2_5,
                   BYBFILE.BYB_PL_INTCOL_2_5,
                   BYBFILE.BYB_PL_LATECOL_2_5,
                   BYBFILE.BYB_PL_O2COL_2_5,
                   BYBFILE.BYB_AMT_SOLD_2,
                   BYBFILE.BYB_AMT_TRANS_2,
                   BYBFILE.BYB_AMT_PURCH_2,
                   BYBFILE.BYB_NO_SOLD_2,
                   BYBFILE.BYB_NO_TRANS_2,
                   BYBFILE.BYB_NO_PURCH_2,
                   BYBFILE.BYB_OTHCOL_2,
                   BYBFILE.BYB_OT2COL_2,
                   BYBFILE.BYB_INTCOL_2,
                   BYBFILE.BYB_LATECOL_2,
                   BYBFILE.BYB_PRINCOL_2,
                   BYBFILE.BYB_DEFCOL_2,
                   BYBFILE.BYB_RNPRIN_2,
                   BYBFILE.BYB_INTREB_2,
                   BYBFILE.BYB_CLREB1_2,
                   BYBFILE.BYB_CLREB2_2,
                   BYBFILE.BYB_CLREB3_2,
                   BYBFILE.BYB_CLREB4_2,
                   BYBFILE.BYB_AHREB_2,
                   BYBFILE.BYB_PPREB_2,
                   BYBFILE.BYB_O1REB_2,
                   BYBFILE.BYB_O2REB_2,
                   BYBFILE.BYB_O3REB_2,
                   BYBFILE.BYB_O4REB_2,
                   BYBFILE.BYB_O5REB_2,
                   BYBFILE.BYB_O6REB_2,
                   BYBFILE.BYB_O7REB_2,
                   BYBFILE.BYB_SCREB_2,
                   BYBFILE.BYB_MFREB_2,
                   BYBFILE.BYB_DFREB_2,
                   BYBFILE.BYB_UN7_REB_2,
                   BYBFILE.BYB_UN8_REB_2,
                   BYBFILE.BYB_UN9_REB_2,
                   BYBFILE.BYB_UN10_REB_2,
                   BYBFILE.BYB_PART_CHGOFF_2,
                   BYBFILE.BYB_ALLOTMENTS_2,
                   BYBFILE.BYB_INS_NO_ELIG_2_1,
                   BYBFILE.BYB_INS_NO_WROTE_2_1,
                   BYBFILE.BYB_INS_NO_NOTWROTE_2_1,
                   BYBFILE.BYB_INS_AMT_WROTE_2_1,
                   BYBFILE.BYB_INS_NO_ELIG_2_2,
                   BYBFILE.BYB_INS_NO_WROTE_2_2,
                   BYBFILE.BYB_INS_NO_NOTWROTE_2_2,
                   BYBFILE.BYB_INS_AMT_WROTE_2_2,
                   BYBFILE.BYB_INS_NO_ELIG_2_3,
                   BYBFILE.BYB_INS_NO_WROTE_2_3,
                   BYBFILE.BYB_INS_NO_NOTWROTE_2_3,
                   BYBFILE.BYB_INS_AMT_WROTE_2_3,
                   BYBFILE.BYB_INS_NO_ELIG_2_4,
                   BYBFILE.BYB_INS_NO_WROTE_2_4,
                   BYBFILE.BYB_INS_NO_NOTWROTE_2_4,
                   BYBFILE.BYB_INS_AMT_WROTE_2_4,
                   BYBFILE.BYB_INS_NO_ELIG_2_5,
                   BYBFILE.BYB_INS_NO_WROTE_2_5,
                   BYBFILE.BYB_INS_NO_NOTWROTE_2_5,
                   BYBFILE.BYB_INS_AMT_WROTE_2_5,
                   BYBFILE.BYB_INS_NO_ELIG_2_6,
                   BYBFILE.BYB_INS_NO_WROTE_2_6,
                   BYBFILE.BYB_INS_NO_NOTWROTE_2_6,
                   BYBFILE.BYB_INS_AMT_WROTE_2_6,
                   BYBFILE.BYB_INS_NO_ELIG_2_7,
                   BYBFILE.BYB_INS_NO_WROTE_2_7,
                   BYBFILE.BYB_INS_NO_NOTWROTE_2_7,
                   BYBFILE.BYB_INS_AMT_WROTE_2_7,
                   BYBFILE.BYB_INS_NO_ELIG_2_8,
                   BYBFILE.BYB_INS_NO_WROTE_2_8,
                   BYBFILE.BYB_INS_NO_NOTWROTE_2_8,
                   BYBFILE.BYB_INS_AMT_WROTE_2_8,
                   BYBFILE.BYB_INS_NO_ELIG_2_9,
                   BYBFILE.BYB_INS_NO_WROTE_2_9,
                   BYBFILE.BYB_INS_NO_NOTWROTE_2_9,
                   BYBFILE.BYB_INS_AMT_WROTE_2_9,
                   BYBFILE.BYB_INS_NO_ELIG_2_10,
                   BYBFILE.BYB_INS_NO_WROTE_2_10,
                   BYBFILE.BYB_INS_NO_NOTWROTE_2_10,
                   BYBFILE.BYB_INS_AMT_WROTE_2_10,
                   BYBFILE.BYB_FEE_NO_ELIG_2_1,
                   BYBFILE.BYB_FEE_NO_WROTE_2_1,
                   BYBFILE.BYB_FEE_NO_NOTWROTE_2_1,
                   BYBFILE.BYB_FEE_AMT_WROTE_2_1,
                   BYBFILE.BYB_FEE_NO_ELIG_2_2,
                   BYBFILE.BYB_FEE_NO_WROTE_2_2,
                   BYBFILE.BYB_FEE_NO_NOTWROTE_2_2,
                   BYBFILE.BYB_FEE_AMT_WROTE_2_2,
                   BYBFILE.BYB_FEE_NO_ELIG_2_3,
                   BYBFILE.BYB_FEE_NO_WROTE_2_3,
                   BYBFILE.BYB_FEE_NO_NOTWROTE_2_3,
                   BYBFILE.BYB_FEE_AMT_WROTE_2_3,
                   BYBFILE.BYB_OTH_NO_COL_2,
                   BYBFILE.BYB_OT2_NO_COL_2,
                   BYBFILE.BYB_INT_NO_COL_2,
                   BYBFILE.BYB_LATE_NO_COL_2,
                   BYBFILE.BYB_OTH_NO_WAIVED_2,
                   BYBFILE.BYB_OT2_NO_WAIVED_2,
                   BYBFILE.BYB_INT_NO_WAIVED_2,
                   BYBFILE.BYB_LATE_NO_WAIVED_2,
                   BYBFILE.BYB_OTH_AMT_WAIVED_2,
                   BYBFILE.BYB_OT2_AMT_WAIVED_2,
                   BYBFILE.BYB_INT_AMT_WAIVED_2,
                   BYBFILE.BYB_LATE_AMT_WAIVED_2,
                   BYBFILE.BYB_UNITS_1_1_1,
                   BYBFILE.BYB_OUTBAL_1_1_1,
                   BYBFILE.BYB_UNITS_1_1_2,
                   BYBFILE.BYB_OUTBAL_1_1_2,
                   BYBFILE.BYB_UNITS_1_1_3,
                   BYBFILE.BYB_OUTBAL_1_1_3,
                   BYBFILE.BYB_UNITS_1_1_4,
                   BYBFILE.BYB_OUTBAL_1_1_4,
                   BYBFILE.BYB_UNITS_1_1_5,
                   BYBFILE.BYB_OUTBAL_1_1_5,
                   BYBFILE.BYB_UNITS_1_1_6,
                   BYBFILE.BYB_OUTBAL_1_1_6,
                   BYBFILE.BYB_UNITS_1_1_7,
                   BYBFILE.BYB_OUTBAL_1_1_7,
                   BYBFILE.BYB_UNITS_1_1_8,
                   BYBFILE.BYB_OUTBAL_1_1_8,
                   BYBFILE.BYB_UNITS_1_1_9,
                   BYBFILE.BYB_OUTBAL_1_1_9,
                   BYBFILE.BYB_UNITS_1_1_10,
                   BYBFILE.BYB_OUTBAL_1_1_10,
                   BYBFILE.BYB_UNITS_1_2_1,
                   BYBFILE.BYB_OUTBAL_1_2_1,
                   BYBFILE.BYB_UNITS_1_2_2,
                   BYBFILE.BYB_OUTBAL_1_2_2,
                   BYBFILE.BYB_UNITS_1_2_3,
                   BYBFILE.BYB_OUTBAL_1_2_3,
                   BYBFILE.BYB_UNITS_1_2_4,
                   BYBFILE.BYB_OUTBAL_1_2_4,
                   BYBFILE.BYB_UNITS_1_2_5,
                   BYBFILE.BYB_OUTBAL_1_2_5,
                   BYBFILE.BYB_UNITS_1_2_6,
                   BYBFILE.BYB_OUTBAL_1_2_6,
                   BYBFILE.BYB_UNITS_1_2_7,
                   BYBFILE.BYB_OUTBAL_1_2_7,
                   BYBFILE.BYB_UNITS_1_2_8,
                   BYBFILE.BYB_OUTBAL_1_2_8,
                   BYBFILE.BYB_UNITS_1_2_9,
                   BYBFILE.BYB_OUTBAL_1_2_9,
                   BYBFILE.BYB_UNITS_1_2_10,
                   BYBFILE.BYB_OUTBAL_1_2_10,
                   BYBFILE.BYB_UNITS_2_1_1,
                   BYBFILE.BYB_OUTBAL_2_1_1,
                   BYBFILE.BYB_UNITS_2_1_2,
                   BYBFILE.BYB_OUTBAL_2_1_2,
                   BYBFILE.BYB_UNITS_2_1_3,
                   BYBFILE.BYB_OUTBAL_2_1_3,
                   BYBFILE.BYB_UNITS_2_1_4,
                   BYBFILE.BYB_OUTBAL_2_1_4,
                   BYBFILE.BYB_UNITS_2_1_5,
                   BYBFILE.BYB_OUTBAL_2_1_5,
                   BYBFILE.BYB_UNITS_2_1_6,
                   BYBFILE.BYB_OUTBAL_2_1_6,
                   BYBFILE.BYB_UNITS_2_1_7,
                   BYBFILE.BYB_OUTBAL_2_1_7,
                   BYBFILE.BYB_UNITS_2_1_8,
                   BYBFILE.BYB_OUTBAL_2_1_8,
                   BYBFILE.BYB_UNITS_2_1_9,
                   BYBFILE.BYB_OUTBAL_2_1_9,
                   BYBFILE.BYB_UNITS_2_1_10,
                   BYBFILE.BYB_OUTBAL_2_1_10,
                   BYBFILE.BYB_UNITS_2_2_1,
                   BYBFILE.BYB_OUTBAL_2_2_1,
                   BYBFILE.BYB_UNITS_2_2_2,
                   BYBFILE.BYB_OUTBAL_2_2_2,
                   BYBFILE.BYB_UNITS_2_2_3,
                   BYBFILE.BYB_OUTBAL_2_2_3,
                   BYBFILE.BYB_UNITS_2_2_4,
                   BYBFILE.BYB_OUTBAL_2_2_4,
                   BYBFILE.BYB_UNITS_2_2_5,
                   BYBFILE.BYB_OUTBAL_2_2_5,
                   BYBFILE.BYB_UNITS_2_2_6,
                   BYBFILE.BYB_OUTBAL_2_2_6,
                   BYBFILE.BYB_UNITS_2_2_7,
                   BYBFILE.BYB_OUTBAL_2_2_7,
                   BYBFILE.BYB_UNITS_2_2_8,
                   BYBFILE.BYB_OUTBAL_2_2_8,
                   BYBFILE.BYB_UNITS_2_2_9,
                   BYBFILE.BYB_OUTBAL_2_2_9,
                   BYBFILE.BYB_UNITS_2_2_10,
                   BYBFILE.BYB_OUTBAL_2_2_10,
                   BYBFILE.BYB_UNITS_3_1_1,
                   BYBFILE.BYB_OUTBAL_3_1_1,
                   BYBFILE.BYB_UNITS_3_1_2,
                   BYBFILE.BYB_OUTBAL_3_1_2,
                   BYBFILE.BYB_UNITS_3_1_3,
                   BYBFILE.BYB_OUTBAL_3_1_3,
                   BYBFILE.BYB_UNITS_3_1_4,
                   BYBFILE.BYB_OUTBAL_3_1_4,
                   BYBFILE.BYB_UNITS_3_1_5,
                   BYBFILE.BYB_OUTBAL_3_1_5,
                   BYBFILE.BYB_UNITS_3_1_6,
                   BYBFILE.BYB_OUTBAL_3_1_6,
                   BYBFILE.BYB_UNITS_3_1_7,
                   BYBFILE.BYB_OUTBAL_3_1_7,
                   BYBFILE.BYB_UNITS_3_1_8,
                   BYBFILE.BYB_OUTBAL_3_1_8,
                   BYBFILE.BYB_UNITS_3_1_9,
                   BYBFILE.BYB_OUTBAL_3_1_9,
                   BYBFILE.BYB_UNITS_3_1_10,
                   BYBFILE.BYB_OUTBAL_3_1_10,
                   BYBFILE.BYB_UNITS_3_2_1,
                   BYBFILE.BYB_OUTBAL_3_2_1,
                   BYBFILE.BYB_UNITS_3_2_2,
                   BYBFILE.BYB_OUTBAL_3_2_2,
                   BYBFILE.BYB_UNITS_3_2_3,
                   BYBFILE.BYB_OUTBAL_3_2_3,
                   BYBFILE.BYB_UNITS_3_2_4,
                   BYBFILE.BYB_OUTBAL_3_2_4,
                   BYBFILE.BYB_UNITS_3_2_5,
                   BYBFILE.BYB_OUTBAL_3_2_5,
                   BYBFILE.BYB_UNITS_3_2_6,
                   BYBFILE.BYB_OUTBAL_3_2_6,
                   BYBFILE.BYB_UNITS_3_2_7,
                   BYBFILE.BYB_OUTBAL_3_2_7,
                   BYBFILE.BYB_UNITS_3_2_8,
                   BYBFILE.BYB_OUTBAL_3_2_8,
                   BYBFILE.BYB_UNITS_3_2_9,
                   BYBFILE.BYB_OUTBAL_3_2_9,
                   BYBFILE.BYB_UNITS_3_2_10,
                   BYBFILE.BYB_OUTBAL_3_2_10,
                   BYBFILE.BYB_UNITS_BNKRPT_1_1_1,
                   BYBFILE.BYB_OUTBAL_BNKRPT_1_1_1,
                   BYBFILE.BYB_UNITS_BNKRPT_1_1_2,
                   BYBFILE.BYB_OUTBAL_BNKRPT_1_1_2,
                   BYBFILE.BYB_UNITS_BNKRPT_1_1_3,
                   BYBFILE.BYB_OUTBAL_BNKRPT_1_1_3,
                   BYBFILE.BYB_UNITS_BNKRPT_1_1_4,
                   BYBFILE.BYB_OUTBAL_BNKRPT_1_1_4,
                   BYBFILE.BYB_UNITS_BNKRPT_1_1_5,
                   BYBFILE.BYB_OUTBAL_BNKRPT_1_1_5,
                   BYBFILE.BYB_UNITS_BNKRPT_1_1_6,
                   BYBFILE.BYB_OUTBAL_BNKRPT_1_1_6,
                   BYBFILE.BYB_UNITS_BNKRPT_1_1_7,
                   BYBFILE.BYB_OUTBAL_BNKRPT_1_1_7,
                   BYBFILE.BYB_UNITS_BNKRPT_1_1_8,
                   BYBFILE.BYB_OUTBAL_BNKRPT_1_1_8,
                   BYBFILE.BYB_UNITS_BNKRPT_1_1_9,
                   BYBFILE.BYB_OUTBAL_BNKRPT_1_1_9,
                   BYBFILE.BYB_UNITS_BNKRPT_1_1_10,
                   BYBFILE.BYB_OUTBAL_BNKRPT_1_1_10,
                   BYBFILE.BYB_UNITS_BNKRPT_1_2_1,
                   BYBFILE.BYB_OUTBAL_BNKRPT_1_2_1,
                   BYBFILE.BYB_UNITS_BNKRPT_1_2_2,
                   BYBFILE.BYB_OUTBAL_BNKRPT_1_2_2,
                   BYBFILE.BYB_UNITS_BNKRPT_1_2_3,
                   BYBFILE.BYB_OUTBAL_BNKRPT_1_2_3,
                   BYBFILE.BYB_UNITS_BNKRPT_1_2_4,
                   BYBFILE.BYB_OUTBAL_BNKRPT_1_2_4,
                   BYBFILE.BYB_UNITS_BNKRPT_1_2_5,
                   BYBFILE.BYB_OUTBAL_BNKRPT_1_2_5,
                   BYBFILE.BYB_UNITS_BNKRPT_1_2_6,
                   BYBFILE.BYB_OUTBAL_BNKRPT_1_2_6,
                   BYBFILE.BYB_UNITS_BNKRPT_1_2_7,
                   BYBFILE.BYB_OUTBAL_BNKRPT_1_2_7,
                   BYBFILE.BYB_UNITS_BNKRPT_1_2_8,
                   BYBFILE.BYB_OUTBAL_BNKRPT_1_2_8,
                   BYBFILE.BYB_UNITS_BNKRPT_1_2_9,
                   BYBFILE.BYB_OUTBAL_BNKRPT_1_2_9,
                   BYBFILE.BYB_UNITS_BNKRPT_1_2_10,
                   BYBFILE.BYB_OUTBAL_BNKRPT_1_2_10,
                   BYBFILE.BYB_UNITS_BNKRPT_2_1_1,
                   BYBFILE.BYB_OUTBAL_BNKRPT_2_1_1,
                   BYBFILE.BYB_UNITS_BNKRPT_2_1_2,
                   BYBFILE.BYB_OUTBAL_BNKRPT_2_1_2,
                   BYBFILE.BYB_UNITS_BNKRPT_2_1_3,
                   BYBFILE.BYB_OUTBAL_BNKRPT_2_1_3,
                   BYBFILE.BYB_UNITS_BNKRPT_2_1_4,
                   BYBFILE.BYB_OUTBAL_BNKRPT_2_1_4,
                   BYBFILE.BYB_UNITS_BNKRPT_2_1_5,
                   BYBFILE.BYB_OUTBAL_BNKRPT_2_1_5,
                   BYBFILE.BYB_UNITS_BNKRPT_2_1_6,
                   BYBFILE.BYB_OUTBAL_BNKRPT_2_1_6,
                   BYBFILE.BYB_UNITS_BNKRPT_2_1_7,
                   BYBFILE.BYB_OUTBAL_BNKRPT_2_1_7,
                   BYBFILE.BYB_UNITS_BNKRPT_2_1_8,
                   BYBFILE.BYB_OUTBAL_BNKRPT_2_1_8,
                   BYBFILE.BYB_UNITS_BNKRPT_2_1_9,
                   BYBFILE.BYB_OUTBAL_BNKRPT_2_1_9,
                   BYBFILE.BYB_UNITS_BNKRPT_2_1_10,
                   BYBFILE.BYB_OUTBAL_BNKRPT_2_1_10,
                   BYBFILE.BYB_UNITS_BNKRPT_2_2_1,
                   BYBFILE.BYB_OUTBAL_BNKRPT_2_2_1,
                   BYBFILE.BYB_UNITS_BNKRPT_2_2_2,
                   BYBFILE.BYB_OUTBAL_BNKRPT_2_2_2,
                   BYBFILE.BYB_UNITS_BNKRPT_2_2_3,
                   BYBFILE.BYB_OUTBAL_BNKRPT_2_2_3,
                   BYBFILE.BYB_UNITS_BNKRPT_2_2_4,
                   BYBFILE.BYB_OUTBAL_BNKRPT_2_2_4,
                   BYBFILE.BYB_UNITS_BNKRPT_2_2_5,
                   BYBFILE.BYB_OUTBAL_BNKRPT_2_2_5,
                   BYBFILE.BYB_UNITS_BNKRPT_2_2_6,
                   BYBFILE.BYB_OUTBAL_BNKRPT_2_2_6,
                   BYBFILE.BYB_UNITS_BNKRPT_2_2_7,
                   BYBFILE.BYB_OUTBAL_BNKRPT_2_2_7,
                   BYBFILE.BYB_UNITS_BNKRPT_2_2_8,
                   BYBFILE.BYB_OUTBAL_BNKRPT_2_2_8,
                   BYBFILE.BYB_UNITS_BNKRPT_2_2_9,
                   BYBFILE.BYB_OUTBAL_BNKRPT_2_2_9,
                   BYBFILE.BYB_UNITS_BNKRPT_2_2_10,
                   BYBFILE.BYB_OUTBAL_BNKRPT_2_2_10,
                   BYBFILE.BYB_UNITS_BNKRPT_3_1_1,
                   BYBFILE.BYB_OUTBAL_BNKRPT_3_1_1,
                   BYBFILE.BYB_UNITS_BNKRPT_3_1_2,
                   BYBFILE.BYB_OUTBAL_BNKRPT_3_1_2,
                   BYBFILE.BYB_UNITS_BNKRPT_3_1_3,
                   BYBFILE.BYB_OUTBAL_BNKRPT_3_1_3,
                   BYBFILE.BYB_UNITS_BNKRPT_3_1_4,
                   BYBFILE.BYB_OUTBAL_BNKRPT_3_1_4,
                   BYBFILE.BYB_UNITS_BNKRPT_3_1_5,
                   BYBFILE.BYB_OUTBAL_BNKRPT_3_1_5,
                   BYBFILE.BYB_UNITS_BNKRPT_3_1_6,
                   BYBFILE.BYB_OUTBAL_BNKRPT_3_1_6,
                   BYBFILE.BYB_UNITS_BNKRPT_3_1_7,
                   BYBFILE.BYB_OUTBAL_BNKRPT_3_1_7,
                   BYBFILE.BYB_UNITS_BNKRPT_3_1_8,
                   BYBFILE.BYB_OUTBAL_BNKRPT_3_1_8,
                   BYBFILE.BYB_UNITS_BNKRPT_3_1_9,
                   BYBFILE.BYB_OUTBAL_BNKRPT_3_1_9,
                   BYBFILE.BYB_UNITS_BNKRPT_3_1_10,
                   BYBFILE.BYB_OUTBAL_BNKRPT_3_1_10,
                   BYBFILE.BYB_UNITS_BNKRPT_3_2_1,
                   BYBFILE.BYB_OUTBAL_BNKRPT_3_2_1,
                   BYBFILE.BYB_UNITS_BNKRPT_3_2_2,
                   BYBFILE.BYB_OUTBAL_BNKRPT_3_2_2,
                   BYBFILE.BYB_UNITS_BNKRPT_3_2_3,
                   BYBFILE.BYB_OUTBAL_BNKRPT_3_2_3,
                   BYBFILE.BYB_UNITS_BNKRPT_3_2_4,
                   BYBFILE.BYB_OUTBAL_BNKRPT_3_2_4,
                   BYBFILE.BYB_UNITS_BNKRPT_3_2_5,
                   BYBFILE.BYB_OUTBAL_BNKRPT_3_2_5,
                   BYBFILE.BYB_UNITS_BNKRPT_3_2_6,
                   BYBFILE.BYB_OUTBAL_BNKRPT_3_2_6,
                   BYBFILE.BYB_UNITS_BNKRPT_3_2_7,
                   BYBFILE.BYB_OUTBAL_BNKRPT_3_2_7,
                   BYBFILE.BYB_UNITS_BNKRPT_3_2_8,
                   BYBFILE.BYB_OUTBAL_BNKRPT_3_2_8,
                   BYBFILE.BYB_UNITS_BNKRPT_3_2_9,
                   BYBFILE.BYB_OUTBAL_BNKRPT_3_2_9,
                   BYBFILE.BYB_UNITS_BNKRPT_3_2_10,
                   BYBFILE.BYB_OUTBAL_BNKRPT_3_2_10,
                   BYBFILE.BYB_SUMWGTTM,
                   BYBFILE.BYB_SUMWGTAV,
                   BYBFILE.BYB_SUMWGTTA,
                   BYBFILE.BYB_IB_ACCRUED,
                   BYBFILE.BYB_DLDISC_1,
                   BYBFILE.BYB_DLDISC_2,
                   BYBFILE.BYB_DLDISC_3,
                   BYBFILE.BYB_STD_POS,
                   BYBFILE.BYB_30_DAY_MOVED,
                   BYBFILE.BYB_PROJ_INCREASE,
                   BYBFILE.BYB_PROJ_PROFIT,
                   BYBFILE.BYB_PROCEEDS_1,
                   BYBFILE.BYB_PROCEEDS_2,
                   BYBFILE.BYB_LOCKBOX_1,
                   BYBFILE.BYB_LOCKBOX_2,
                   BYBFILE.BYB_UNEARNED_AMT_1_1_1,
                   BYBFILE.BYB_UNEARNED_AMT_1_1_2,
                   BYBFILE.BYB_UNEARNED_AMT_1_1_3,
                   BYBFILE.BYB_UNEARNED_AMT_1_1_4,
                   BYBFILE.BYB_UNEARNED_AMT_1_1_5,
                   BYBFILE.BYB_UNEARNED_AMT_1_1_6,
                   BYBFILE.BYB_UNEARNED_AMT_1_1_7,
                   BYBFILE.BYB_UNEARNED_AMT_1_1_8,
                   BYBFILE.BYB_UNEARNED_AMT_1_1_9,
                   BYBFILE.BYB_UNEARNED_AMT_1_1_10,
                   BYBFILE.BYB_UNEARNED_AMT_1_2_1,
                   BYBFILE.BYB_UNEARNED_AMT_1_2_2,
                   BYBFILE.BYB_UNEARNED_AMT_1_2_3,
                   BYBFILE.BYB_UNEARNED_AMT_1_2_4,
                   BYBFILE.BYB_UNEARNED_AMT_1_2_5,
                   BYBFILE.BYB_UNEARNED_AMT_1_2_6,
                   BYBFILE.BYB_UNEARNED_AMT_1_2_7,
                   BYBFILE.BYB_UNEARNED_AMT_1_2_8,
                   BYBFILE.BYB_UNEARNED_AMT_1_2_9,
                   BYBFILE.BYB_UNEARNED_AMT_1_2_10,
                   BYBFILE.BYB_UNEARNED_AMT_2_1_1,
                   BYBFILE.BYB_UNEARNED_AMT_2_1_2,
                   BYBFILE.BYB_UNEARNED_AMT_2_1_3,
                   BYBFILE.BYB_UNEARNED_AMT_2_1_4,
                   BYBFILE.BYB_UNEARNED_AMT_2_1_5,
                   BYBFILE.BYB_UNEARNED_AMT_2_1_6,
                   BYBFILE.BYB_UNEARNED_AMT_2_1_7,
                   BYBFILE.BYB_UNEARNED_AMT_2_1_8,
                   BYBFILE.BYB_UNEARNED_AMT_2_1_9,
                   BYBFILE.BYB_UNEARNED_AMT_2_1_10,
                   BYBFILE.BYB_UNEARNED_AMT_2_2_1,
                   BYBFILE.BYB_UNEARNED_AMT_2_2_2,
                   BYBFILE.BYB_UNEARNED_AMT_2_2_3,
                   BYBFILE.BYB_UNEARNED_AMT_2_2_4,
                   BYBFILE.BYB_UNEARNED_AMT_2_2_5,
                   BYBFILE.BYB_UNEARNED_AMT_2_2_6,
                   BYBFILE.BYB_UNEARNED_AMT_2_2_7,
                   BYBFILE.BYB_UNEARNED_AMT_2_2_8,
                   BYBFILE.BYB_UNEARNED_AMT_2_2_9,
                   BYBFILE.BYB_UNEARNED_AMT_2_2_10,
                   BYBFILE.BYB_UNEARNED_AMT_3_1_1,
                   BYBFILE.BYB_UNEARNED_AMT_3_1_2,
                   BYBFILE.BYB_UNEARNED_AMT_3_1_3,
                   BYBFILE.BYB_UNEARNED_AMT_3_1_4,
                   BYBFILE.BYB_UNEARNED_AMT_3_1_5,
                   BYBFILE.BYB_UNEARNED_AMT_3_1_6,
                   BYBFILE.BYB_UNEARNED_AMT_3_1_7,
                   BYBFILE.BYB_UNEARNED_AMT_3_1_8,
                   BYBFILE.BYB_UNEARNED_AMT_3_1_9,
                   BYBFILE.BYB_UNEARNED_AMT_3_1_10,
                   BYBFILE.BYB_UNEARNED_AMT_3_2_1,
                   BYBFILE.BYB_UNEARNED_AMT_3_2_2,
                   BYBFILE.BYB_UNEARNED_AMT_3_2_3,
                   BYBFILE.BYB_UNEARNED_AMT_3_2_4,
                   BYBFILE.BYB_UNEARNED_AMT_3_2_5,
                   BYBFILE.BYB_UNEARNED_AMT_3_2_6,
                   BYBFILE.BYB_UNEARNED_AMT_3_2_7,
                   BYBFILE.BYB_UNEARNED_AMT_3_2_8,
                   BYBFILE.BYB_UNEARNED_AMT_3_2_9,
                   BYBFILE.BYB_UNEARNED_AMT_3_2_10
            INTO :QBYB-REC
            FROM DBO.BYBFILE
            WHERE BYBFILE.BYB_DATE_CCYYMM = :QBYB-DATE-CCYYMM
              AND BYBFILE.BYB_BRNO = :QBYB-BRNO
              AND BYBFILE.BYB_CLASS = :QBYB-CLASS
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-BYB1-FILE.

           IF ( IO-FG = 0 )
              PERFORM GET-BYB-FIELDS.

      *-----------------------------------------------------------------
       CLOSE-BYB1-FILE.
           PERFORM CLOSE-IT.

      *-----------------------------------------------------------------
       CLOSE-BY1-CONNECTION.

           IF BY1-CONN-OPEN-SW = "Y"
               EXEC SQL SET CONNECTION BY1_CONNECT END-EXEC
               EXEC SQL COMMIT END-EXEC
               EXEC SQL DISCONNECT BY1_CONNECT END-EXEC
               EXEC SQL SET CONNECTION C1 END-EXEC
               MOVE "N" TO BY1-CONN-OPEN-SW.

      *-----------------------------------------------------------------


       COPY "LIBLP/LPTY1RN.CPY".
       COPY "LIBGB/GBGR1RN.CPY".
       COPY "LIBGB/GBRC2RN.CPY".
      *-----------------------------------------------------------------
       READ-RC2-FILE-FIRST-CALL.
           PERFORM READ-IT.
           MOVE RC-PATH-SQL TO E-FILE.
           MOVE RC2-KEY TO E-KEYX.
      *    INITIALIZE QRC-REC.

           EXEC SQL
            SELECT COUNT(*)
              INTO :WS-RC2-ROW-COUNT
              FROM DBO.RCFILE
             WHERE RC_STATUS = 'A1' 
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-RC2-FILE-FIRST-CALL.

      *-----------------------------------------------------------------

      * COPY "LIBSP/SPWR1RN.CPY".
      * COPYMEMBER: LIBSP/SPWR1RN
       LOAD-WR1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( WR-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( WR-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO WR-PATH-BASE
              MOVE EXT-FILPATH-MACHINE     TO WR-PATH-MACHINE
              MOVE WR-PATH-FORMAT          TO WR-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( WR-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO WR-PATH-BASE
              MOVE FILPATH-MACHINE         TO WR-PATH-MACHINE
              MOVE WR-PATH-FORMAT          TO WR-PATH.

      *-----------------------------------------------------------------
       OPEN-WR1-FILE.
           PERFORM LOAD-WR1-FILE.
           PERFORM OPEN-IT.
           MOVE WR-PATH-SQL TO E-FILE.

           IF WR1-CONN-OPEN-SW = "N" 
              MOVE "Y"         TO WR1-CONN-OPEN-SW
              EXEC SQL CONNECT TO :SQL-CONNECT-SERVER AS WR1_CONNECT 
                       USER :SQL-CONNECT-USERID USING :SQL-CONNECT-PSWD
              END-EXEC.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

           EXEC SQL SET CONNECTION C1 END-EXEC.

      *-----------------------------------------------------------------
       START-WR1-FILE. *> SSP - WRFILE
           PERFORM START-IT.
           MOVE WR-PATH-SQL TO E-FILE.
           MOVE WR1-KEY TO E-KEYX.

           IF ( WR1-CURSOR-STAT-GOOD )
              PERFORM CLOSE-WR1-FILE.

           IF ( QWR1-WEND-YYYYMM = ZEROES    ) OR
              ( QWR1-WEND-YYYYMM NOT NUMERIC )
              MOVE WR-YYYYMM            TO QWR1-WBEG-YYYYMM
              MOVE ALL "9"              TO QWR1-WEND-YYYYMM
              MOVE WR-BRNO              TO QWR1-WBEG-BRNO
              MOVE ALL "9"              TO QWR1-WEND-BRNO.

      *  STORED PROCEDURE:
      *
      *  SELECT WRFILE.WR_YYYYMM,
      *         WRFILE.WR_BRNO,
      *         WRFILE.WR_ELM_OUTS_BALANCE,
      *         WRFILE.WR_ELY_OUTS_BALANCE,
      *         WRFILE.WR_NO_NB_1,
      *         WRFILE.WR_NO_NB_2,
      *         WRFILE.WR_NO_FB_1,
      *         WRFILE.WR_NO_FB_2,
      *         WRFILE.WR_NO_PB_1,
      *         WRFILE.WR_NO_PB_2,
      *         WRFILE.WR_BSR_BONUS_1,
      *         WRFILE.WR_BSR_BONUS_2,
      *         WRFILE.WR_INTEREST_ALLOCATION_1,
      *         WRFILE.WR_INTEREST_ALLOCATION_2,
      *         WRFILE.WR_GEN_RESERVE_ADJ_1,
      *         WRFILE.WR_GEN_RESERVE_ADJ_2,
      *         WRFILE.WR_INTANGIBLE_AMORT_1,
      *         WRFILE.WR_INTANGIBLE_AMORT_2,
      *         WRFILE.WR_ROBBERIES_1,
      *         WRFILE.WR_ROBBERIES_2,
      *         WRFILE.WR_RATING_SHEET_INCOME_1,
      *         WRFILE.WR_RATING_SHEET_INCOME_2,
      *         WRFILE.WR_INT_FEE_INCOME_1,
      *         WRFILE.WR_INT_FEE_INCOME_2,
      *         WRFILE.WR_INSURANCE_INCOME_1,
      *         WRFILE.WR_INSURANCE_INCOME_2,
      *         WRFILE.WR_RATING_SHEET_EXPENSES_1,
      *         WRFILE.WR_RATING_SHEET_EXPENSES_2,
      *         WRFILE.WR_PERSONNEL_EXPENSE_1,
      *         WRFILE.WR_PERSONNEL_EXPENSE_2,
      *         WRFILE.WR_YTD_PCT_RATE_SHT_PROFIT,
      *         WRFILE.WR_YTD_PCT_RS_INC_LEDGER,
      *         WRFILE.WR_YTD_PCT_INT_FEE_LEDGER,
      *         WRFILE.WR_YTD_PCT_INS_INC_LEDGER,
      *         WRFILE.WR_YTD_PCT_EXPENSE_LEDGER,
      *         WRFILE.WR_MGR_BONUS,
      *         WRFILE.WR_PRETAX_PROFIT_1,
      *         WRFILE.WR_PRETAX_PROFIT_2,
      *         WRFILE.WR_SUPERVISION_1,
      *         WRFILE.WR_SUPERVISION_2
      *  FROM DBO.WRFILE
      *  WHERE WRFILE.WR_YYYYMM =  @QWR1_WBEG_YYYYMM
      *  ORDER BY WRFILE.WR_YYYYMM,
      *           WRFILE.WR_BRNO;

           EXEC SQL SET CONNECTION WR1_CONNECT END-EXEC.

           EXEC SQL
            DECLARE WR1_CURSOR CURSOR FOR
                :WR1-RETURN-CODE = EXEC SSP_BYINQ_WRFILE_SELECT 
              ( :QWR1-WBEG-YYYYMM )
           END-EXEC.

           EXEC SQL
               OPEN WR1_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE ZEROES TO QWR1-WBEG-YYYYMM
                          QWR1-WEND-YYYYMM
                          QWR1-WEND-BRNO
                          QWR1-WEND-BRNO.

           MOVE STAT---GOOD TO WR1-CURSOR-STAT.

           EXEC SQL SET CONNECTION C1 END-EXEC.

      *-----------------------------------------------------------------
       READ-WR1-FILE. *> EMBEDDED - WRFILE
           PERFORM READ-IT.
           MOVE WR-PATH-SQL TO E-FILE.
           MOVE WR1-KEY TO E-KEYX.
           INITIALIZE QWR-REC.

           MOVE WR-YYYYMM   TO QWR-YYYYMM.
           MOVE WR-BRNO     TO QWR-BRNO.

           EXEC SQL
            SELECT WRFILE.WR_YYYYMM,
                   WRFILE.WR_BRNO,
                   WRFILE.WR_ELM_OUTS_BALANCE,
                   WRFILE.WR_ELY_OUTS_BALANCE,
                   WRFILE.WR_NO_NB_1,
                   WRFILE.WR_NO_NB_2,
                   WRFILE.WR_NO_FB_1,
                   WRFILE.WR_NO_FB_2,
                   WRFILE.WR_NO_PB_1,
                   WRFILE.WR_NO_PB_2,
                   WRFILE.WR_BSR_BONUS_1,
                   WRFILE.WR_BSR_BONUS_2,
                   WRFILE.WR_INTEREST_ALLOCATION_1,
                   WRFILE.WR_INTEREST_ALLOCATION_2,
                   WRFILE.WR_GEN_RESERVE_ADJ_1,
                   WRFILE.WR_GEN_RESERVE_ADJ_2,
                   WRFILE.WR_INTANGIBLE_AMORT_1,
                   WRFILE.WR_INTANGIBLE_AMORT_2,
                   WRFILE.WR_ROBBERIES_1,
                   WRFILE.WR_ROBBERIES_2,
                   WRFILE.WR_RATING_SHEET_INCOME_1,
                   WRFILE.WR_RATING_SHEET_INCOME_2,
                   WRFILE.WR_INT_FEE_INCOME_1,
                   WRFILE.WR_INT_FEE_INCOME_2,
                   WRFILE.WR_INSURANCE_INCOME_1,
                   WRFILE.WR_INSURANCE_INCOME_2,
                   WRFILE.WR_RATING_SHEET_EXPENSES_1,
                   WRFILE.WR_RATING_SHEET_EXPENSES_2,
                   WRFILE.WR_PERSONNEL_EXPENSE_1,
                   WRFILE.WR_PERSONNEL_EXPENSE_2,
                   WRFILE.WR_YTD_PCT_RATE_SHT_PROFIT,
                   WRFILE.WR_YTD_PCT_RS_INC_LEDGER,
                   WRFILE.WR_YTD_PCT_INT_FEE_LEDGER,
                   WRFILE.WR_YTD_PCT_INS_INC_LEDGER,
                   WRFILE.WR_YTD_PCT_EXPENSE_LEDGER,
                   WRFILE.WR_MGR_BONUS,
                   WRFILE.WR_PRETAX_PROFIT_1,
                   WRFILE.WR_PRETAX_PROFIT_2,
                   WRFILE.WR_SUPERVISION_1,
                   WRFILE.WR_SUPERVISION_2
             INTO 
                   :QWR-YYYYMM,
                   :QWR-BRNO,
                   :QWR-ELM-OUTS-BALANCE,
                   :QWR-ELY-OUTS-BALANCE,
                   :QWR-NO-NB-1,
                   :QWR-NO-NB-2,
                   :QWR-NO-FB-1,
                   :QWR-NO-FB-2,
                   :QWR-NO-PB-1,
                   :QWR-NO-PB-2,
                   :QWR-BSR-BONUS-1,
                   :QWR-BSR-BONUS-2,
                   :QWR-INTEREST-ALLOCATION-1,
                   :QWR-INTEREST-ALLOCATION-2,
                   :QWR-GEN-RESERVE-ADJ-1,
                   :QWR-GEN-RESERVE-ADJ-2,
                   :QWR-INTANGIBLE-AMORT-1,
                   :QWR-INTANGIBLE-AMORT-2,
                   :QWR-ROBBERIES-1,
                   :QWR-ROBBERIES-2,
                   :QWR-RATING-SHEET-INCOME-1,
                   :QWR-RATING-SHEET-INCOME-2,
                   :QWR-INT-FEE-INCOME-1,
                   :QWR-INT-FEE-INCOME-2,
                   :QWR-INSURANCE-INCOME-1,
                   :QWR-INSURANCE-INCOME-2,
                   :QWR-RATING-SHEET-EXPENSES-1,
                   :QWR-RATING-SHEET-EXPENSES-2,
                   :QWR-PERSONNEL-EXPENSE-1,
                   :QWR-PERSONNEL-EXPENSE-2,
                   :QWR-YTD-PCT-RATE-SHT-PROFIT,
                   :QWR-YTD-PCT-RS-INC-LEDGER,
                   :QWR-YTD-PCT-INT-FEE-LEDGER,
                   :QWR-YTD-PCT-INS-INC-LEDGER,
                   :QWR-YTD-PCT-EXPENSE-LEDGER,
                   :QWR-MGR-BONUS,
                   :QWR-PRETAX-PROFIT-1,
                   :QWR-PRETAX-PROFIT-2,
                   :QWR-SUPERVISION-1,
                   :QWR-SUPERVISION-2
             FROM DBO.WRFILE
            WHERE WRFILE.WR_YYYYMM = :QWR-YYYYMM
              AND WRFILE.WR_BRNO = :QWR-BRNO
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-WR1-FILE.

           IF ( IO-FG = 0 )
              PERFORM GET-WR-FIELDS.

      *-----------------------------------------------------------------
       READ-WR1-FILE-NEXT. *> EMBEDDED - WRFILE
           PERFORM READ-IT.
           MOVE WR-PATH-SQL TO E-FILE.
           MOVE WR1-KEY TO E-KEYX.
           INITIALIZE QWR-REC.

           EXEC SQL SET CONNECTION WR1_CONNECT END-EXEC.

           IF ( WR1-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH WR1_CURSOR 
              INTO 
                   :QWR-YYYYMM,
                   :QWR-BRNO,
                   :QWR-ELM-OUTS-BALANCE,
                   :QWR-ELY-OUTS-BALANCE,
                   :QWR-NO-NB-1,
                   :QWR-NO-NB-2,
                   :QWR-NO-FB-1,
                   :QWR-NO-FB-2,
                   :QWR-NO-PB-1,
                   :QWR-NO-PB-2,
                   :QWR-BSR-BONUS-1,
                   :QWR-BSR-BONUS-2,
                   :QWR-INTEREST-ALLOCATION-1,
                   :QWR-INTEREST-ALLOCATION-2,
                   :QWR-GEN-RESERVE-ADJ-1,
                   :QWR-GEN-RESERVE-ADJ-2,
                   :QWR-INTANGIBLE-AMORT-1,
                   :QWR-INTANGIBLE-AMORT-2,
                   :QWR-ROBBERIES-1,
                   :QWR-ROBBERIES-2,
                   :QWR-RATING-SHEET-INCOME-1,
                   :QWR-RATING-SHEET-INCOME-2,
                   :QWR-INT-FEE-INCOME-1,
                   :QWR-INT-FEE-INCOME-2,
                   :QWR-INSURANCE-INCOME-1,
                   :QWR-INSURANCE-INCOME-2,
                   :QWR-RATING-SHEET-EXPENSES-1,
                   :QWR-RATING-SHEET-EXPENSES-2,
                   :QWR-PERSONNEL-EXPENSE-1,
                   :QWR-PERSONNEL-EXPENSE-2,
                   :QWR-YTD-PCT-RATE-SHT-PROFIT,
                   :QWR-YTD-PCT-RS-INC-LEDGER,
                   :QWR-YTD-PCT-INT-FEE-LEDGER,
                   :QWR-YTD-PCT-INS-INC-LEDGER,
                   :QWR-YTD-PCT-EXPENSE-LEDGER,
                   :QWR-MGR-BONUS,
                   :QWR-PRETAX-PROFIT-1,
                   :QWR-PRETAX-PROFIT-2,
                   :QWR-SUPERVISION-1,
                   :QWR-SUPERVISION-2
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-WR1-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-WR-FIELDS.

           EXEC SQL SET CONNECTION C1 END-EXEC.

      *-----------------------------------------------------------------
       CLOSE-WR1-FILE.
           PERFORM CLOSE-IT.
           IF ( WR1-CURSOR-STAT-GOOD )
              EXEC SQL SET CONNECTION WR1_CONNECT END-EXEC
              EXEC SQL
                   CLOSE WR1_CURSOR
              END-EXEC
              EXEC SQL SET CONNECTION C1 END-EXEC
              MOVE STAT---BAD TO WR1-CURSOR-STAT.

      *-----------------------------------------------------------------
       READ-WR1-FILE-FIRST-CALL.
           PERFORM READ-IT.
           MOVE WR-PATH-SQL TO E-FILE.
           MOVE WR1-KEY TO E-KEYX.
      *    INITIALIZE QWR-REC.

           EXEC SQL
            SELECT COUNT(*)
              INTO :WS-WR1-ROW-COUNT
              FROM DBO.WRFILE
             WHERE WRFILE.WR_YYYYMM = :QWR1-WBEG-YYYYMM
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-WR1-FILE-FIRST-CALL.

      *-----------------------------------------------------------------
       CLOSE-WR1-CONNECTION.

           IF WR1-CONN-OPEN-SW = "Y"
              EXEC SQL SET CONNECTION WR1_CONNECT END-EXEC
              EXEC SQL COMMIT END-EXEC
              EXEC SQL DISCONNECT WR1_CONNECT END-EXEC
              EXEC SQL SET CONNECTION C1 END-EXEC
              MOVE "N" TO WR1-CONN-OPEN-SW.

      *-----------------------------------------------------------------

       COPY "LIBGB/FERRORS.CPY".
       END-ROUTINE SECTION.
       END-RUN.
           PERFORM CLOSE-FILES.
       EXIT-PROG.
           MOVE "LP/LPIQMU" TO FORM-NAM.
           MOVE FORM-PATH TO FORM-PATHNAME.
       STOP-RUN.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN  
           PERFORM FORM-DESTROY.
           EXIT PROGRAM.
