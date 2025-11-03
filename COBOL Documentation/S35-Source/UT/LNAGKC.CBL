      ******************************************************************
      ******************************************************************
      ******************************************************************
      * THIS IS A COPY OF LP/LNAGED.CBL FOR TESTING THE NEW SQL-COL.SH
      * SCRIPT THAT GETS THE SQL COLUMNS USED IN THE PROGRAM SO THAT 
      * HARDCODED IO ROUTINES WITH LIMITED COLUMNS ARE USED TO
      * IMPROVE PERFORMANCE.
      ******************************************************************
      ******************************************************************
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.      LNAGKC.
       DATE-WRITTEN. 02-10-84
      ******************************************************************
      *    DIR :  LP
      *    DESC:  LOAN AGED TRIAL BALANCE
      *
      **********   IF AN ACCOUNT IS BOTH A REPO AND A PL, WILL PRINT AS
      **********   P&L TOTALS, AND NOT AS REPO TOTALS
      *
      *=================================================================
      * REV:
      * 042995 SLC ADDED PLTYPE LOGIC.
      * 071295 MJD CHANGED TOTALS,AND DET LINE SO THAT:
      *             CURRENT BALANCE = LN-CURBAL - LN-OT2BAL,
      *             OTHER BALANCE   = LN-OTHBAL + LN-OT2BAL.
      * 073195 MJD ADDED INCLUDE/SKIP/ONLY OPTIONS FOR POTS
      *             AND MISS PAYS.
      * MJD 092995 REMOVED INTEREST FROM SUMMARY LINE AND INCREASED
      *             OTHER FIELDS BY 3 SIG DIGITS.
      * JFF 100695 ADDED CLASS SCAN AND DEALER SCAN
      * MJD 010896 CORRECTED TEST OF LN-PLTYPE. IF "A" FOR ALL WAS ENTERED
      *             THEN THE PROGRAM WAS INCORRECTLY TESTING LN-PLTYPE
      *             FOR AN "A" INSTEAD OF FOR ALL TYPES.
      * BAH 012296 EXPANDED T-NO, TT-NO, DD-NO AND NM-NO TO PIC S9(7)
      * BAH 012296 EXPANDED T-OTH, TT-OTH, DD-OTH AND NM-OTH TO PIC S9(9)V99
      * BAH 012296 EXPANDED T-UNE, TT-UNE, DD-UNE AND NM-UNE TO PIC S9(9)V99
      * BAH 012296 EXPANDED T-ERN, TT-ERN, DD-ERN AND NM-ERN TO PIC S9(9)V99
      * MJD 031396 ADDED OPTION TO PRINT 0 UNEARNED (I,S,O).
      * MJD 051096 ADDED LOCAL SPOOLING LOGIC.
      * JFF 053196 FIXED CLASS TOTALS TO PRINT ALL CLASSES BEFORE
      *            BRANCH SUMMARY.
      * JTG 070196 MADE 0000-9999 AGEING RANGE SKIP THE TEST RE: ACCOUNTS
      *            BEING 10000+ DAYS RECENCY
      * MJD 071696 ADDED LIBLP/LPUPWK FOR CHANGES IN LIBLP/AGEING
      * BAH 080296 ADDED BANKRULE OPTION (A50)
      * JTG 081496 CHANGED TO USE LN-SPRCLASS FOR SPR READ
      * KEC 111496 CHANGED LN-POCD FOR RESCISSION.
      * MJD 121096 CHANGED TO PERFORM LPSTAT-DISPLAY AFTER READING BWFILE.
      * JTG 121696 CHANGED TO DISPLAY "W","B","S" WEEKLY/BIWEEKLY
      * BAH 121896 IF ALL LNAMTS AND BALANCES, DONT TEST RANGE, BECAUSE
      *            LNAMTS > 999999.00 WERE NOT GETTING INCLUDED. ADV,PR#421
      * BLV 060497 CHANGED TO WORK WITH NEW BATCH DRIVER - BREXEC; ALSO -
      *            ADDED USERID BRANCH FOR RANGE OF 0 - 0
      * MJD 082297 CHANGED LN-REC/CONPROFILE TO LN-REC/CONPROFILE(1)
      * GLF 090297 REMOVED ALL USE OF GLOBAL
      * BLV 091597 CHANGED PROMPTS FROM 270+ TO 210+; FIX FOR NEW AGEING
      * MJD 091897 ADDED SUB TOTALS FOR DELINQUENT AND SLOW
      * BAH 110797 ADDED DATE-TYPE FLAG FOR THE DATE RANGE ENTERED TO
      *            BE EITHER ENTRY DATE OR LOAN DATE, PR# 545
      * BAH 111497 BROKE OUT LN-OT2BAL INTO SEPARATE TOTALS,  PL# 149
      * BLV 031198 ADD NEXT MO AGEING OPTION FOR WORLD; IF DEL-FLAG
      *            ENTERED AS 'Y', BUMP AGE-DATE TO NEXT MONTH
      * BAH 032098 DEFAULT RECORDS-FOUND TO "N", SO TOTALS DONT PRINT IF
      *            NO RECORDS ARE FOUND, IT WAS A " "
      * JTG 033198 SUPRESSED PAID AHEAD PRINTOUT IF SP-BANKRULE = 'D', WORLD
      * GLF 040298 ADD PRINT REPO TOTALS AND PL TOTALS IF THEY ARE NOT "S"
      * JTG 040598 CHANGED VERSION CONTROL TO 'H', RE: BLV 031198 ADD NEXT
      *            MO AGEING OPTION FOR WORLD; IF DEL-FLAG
      * BLV 040598 ADDED PASSWD, BRSECURE FOR WORLD
      * GLF 041498 REVERSE CHANGE 'BAH 032098' BUT MOVE 'N' TO
      *            RECORDS-FOUND AFTER THE INITIALIZATION SECTION SO
      *            TO AVOID THE 'NO RECORDS FOUND' MESSAGE WHILE IN
      *            BATCH MAINTENANCE
      * JTG 041598 FIXED TESTS ON SKIP, INCLUDE, ONLY FOR POTS &
      *            MISPAYS - THEY DID NOT WORK
      * GLF 041798 WILL PRINT REPOS ONLY, PL ONLY, CONT/REC ONLY, ALL, ETC..
      * MJD 042198 REMOVED DISPLAY OF ACCOUNT NUMBER DURING RUN.
      *            DEFAULT DEL-FLAG TO "N"
      * BAH 042398 IF RUNNING FOR 1 BRANCH, MORE THAN 1 CLASS, SUMMARY,
      *            THE LAST CLASS TOTALS WERE NOT PRINTING BECAUSE OF THE
      *            MOVE "N" TO MORE-THAN-1-FLAG, MOVED WHERE IT WAS !!
      * MJD 042498 ADDED SCREEN OPTION TO PRINT DUE DATE SUMMARY
      *            ADDED SCREEN OPTION TO SUPPRESS RANGE LINES, CHANGED
      *            VERSION-CONTROL TO "I".
      * MJD 042898 CHANGED TO ONLY RESET MORE-THAN-1-CLASS FLAG AFTER
      *            BRANCH BREAK. IF IT IS NOT DONE HERE, THEN TOTALS
      *            FOR THE LAST CLASS PROCESSED WILL NOT PRINT.
      * JTG 042998 ADDED LOGIC TO PRINT A COMPANY CLASS SUMMARY, WHEN
      *            REPORT SPANS MORE THAN 1 BRANCH AND MORE THAN 1 CLASS
      * JTG 052898 ADDED PRINT OF DELINQUENCY WHEN REPORT IS FOR:
      *                     REPO 'ONLY' OR P&L 'ONLY'
      * BLV 070798 NOT PRINTING CLASS BREAKOUTS ON GRAND TOTALS IF ONLY 1
      *            CLASS IN LAST BRANCH; ADDED MORE-THAN-1-CLASS-ON-REPORT
      * BLV 070798 IF RUNNING IN BATCH, USE EO-BUF-MESS FOR TITLE
      * BLV 071798 IF RUNNING IN BATCH, CHECK FOR DEL-FLAG NOT = Y
      *            INSTEAD OF DEL-FLAG = N (EO-EXEC)
      * BLV 073198 FIXED SKIPPING DUEDATE SUMMARY QUESTION; HAD TO UP ARROW
      * JTG 121698 DISPLAYED 'R' VIA UNITPER-MASK LOGIC FOR REVOLVING ACCOUNT
      * GLF 990226 ADDED I,S,O OF REVOLVING ACCOUNTS
      * JTG 990317 ADDED TEST IF NOT GP-I-AM-MERC TO INCLUDE REPO ACCOUNTS
      *            IN CONTRACTUAL AND RECENCY TOTALS
      * MJD 990324 ADDED LOGIC FOR NON-MONTHLY TOTALS
      *            IF COMPANY = TFC AND LOAN IS NON-MONTHLY
      * BAH 990415 WAS NOT REMOVING WORK FILE
      * BLV 990512 CHECK FOR SKIP, INCLUDE, ONLY REVOLVING WAS CHECKING
      *            LC-REVOLVING INSTEAD OF LN-REVOLVING - IF YOU SAID
      *            SKIP YOU GOT NO ACCOUNTS!!
      * GLB 990712 ADDITIONAL TYPE OPTION FOR DATE RANGE, B=BORROWER
      *            REFERENCE DATE
      * GLB 990712 PRINT "MISSING BORROWER" WHEN BW-REC INVALID
      * GLF 990830 REMOVED PRINT OF JULIAN (H-JULIAN) FROM HEADER PER JTG
      * MJD 990902 ADDED ANOTHER OPTION TO TOTALS ONLY. ID "G" IS ENTERRED,
      *            THEN ONLY GRAND TOTALS ARE PRINTED. BRANCH/CLASS TOTALS
      *            ARE SKIPPED. "C" - ONLY COMPANY CLASS TOTALS ARE PRINTED.
      *            "A" - ONLY COMPANY TOTALS ARE PRINTED ( WORLD PR#208 -
      *            THIS IS SLIGHTLY DIFFERENT THAN REQUEST DESCRIBES, BUT
      *            THIS WAS THE EASIEST WAY JR AND I COULD COME UP WITH AS
      *            ADDING GROUP TOTALS WOULD BE A MAJOR UNDERTAKING.)
      * MJD 990924 ADDED TEST FOR MULLEN. IF GP-I-AM-MULL THEN
      *            PRINT CLASS TOTALS ALL THE TIME. (MULL PR#239)
      * BAH 991012 DEFAULT REPO TO "I" ON THE SCREEN FOR MULLEN PR# 971
      *
      * KEC 2000.0501 {PR#00000} MERCURY FINANCE; CINDY
      *                AS OF A PROBLEM IN THE BATCH WHEN DOING THE
      *                'A10-TO-A15' CONVERSION FOR THEM BECAUSE OF THE
      *                REVOLVING CHECK (LN-REVOLVING IS SPACES):
      *                   WAS: ( LN-REVOLVING     = "N" )
      *                        ( LN-REVOLVING NOT = "N" )
      *                   NEW: ( LN-REVOLVING     = "Y" )
      *                        ( LN-REVOLVING NOT = "Y" )
      *  BLV 000503 IN ENTRY OF DEL-FLAG PARAMETER, IF NOT GP-I-AM-WORLD
      *       MOVE "N" TO DEL-FLAG; UPGRADES MAY HAVE Y IN BATCH PARAMETERS:
      *       REVOL FLAG WAS ADDED IN THE MIDDLE OF EOBUF
      * KEC 000530 {PR#J009} PARADATA FINANCIAL [JOHN GENTILE]
      *             CHANGED '03  D-PAYDATE   PIC 99/99/99.'
      *             TO      '03  D-PAYDATE   PIC X(08).'
      * BAH 020114 UNDISC TOTALS WERE TRUNCATING, PERCENTAGES WRONG, REGACC
      * BLV 030502 FIX APR TO PRINT 3 DIGITS INSTEAD OF 2, CASHCO
      * BAH 040903 REPLACE BR-NAME IN HEADING WITH BR-REGION-NUMBER
      *             FOR GP-I-AM-REGACC, REGACC PR# 77
      * BLF 051025 IF GP-INCLUDE-O2-IN-BAL = "Y", DON'T SUBTRACT O2 FROM
      *            CURBAL, PRINT CURBAL INCLUDING OTHER 2 ON DETAIL LINE &
      *            FOR BALANCE DUE COLUMN IN TOTALS, PR# 2623
      * BAH 101108 PRINT THE CLASS RANGE FOR BRANCH TOTALS, WHEN REPORTS GET
      *            SEPARATED FROM THE RANGES, YOU DONT KNOW WHICH TOTALS THESE
      *            WERE FOR. HCREDIT PR# 3681
      * BLM 110224 OPTION IF I-AM-WORLD TO ENTER "F" FOR DUE DATE SUMMARY &
      *            GENERATE ASCII FILE WITH DUE DATE TOTALS BY BRANCH. FILE
      *            NAME IS /USR/TMP/DUE_MMDDYY_GGGG (GGGG=GROUP OR ENDING
      *            BRANCH.  PASSWORD FOR OPTION LP/LNAGKC/0.  WORLD PR# 745
      *  CS 110823 DONT ADD TO PAID AHEAD IF SP-BANKRULE = 'F', ALREADY DID THIS
      *            IF SP-BANKRULE = "D"  WORLD PR#752
      * BAH 120112 EXPAND UNEARNED TO PRINT 999999.99, WORLD
      * MJD 130118 ADDED EXT-EOBUF-WINDOW-HANDLE (LIBGB/EOBUF.EXT) TO DISPLAY
      *            AND DESTROY AT END OF PROGRAM.
      * BAH 130118 GP-I-AM-MULL WIPING OUT REPO FLAG SET IN BATCH, PL# 788
      *  CS 130924 EXPANDED WK-NUM-BAL,WK-BAL,T-NM-BAL,T-BAL,TT-NM-BAL,SUB-TT
      *            -BA,DD-BAL,NM-BAL & THE UNE TOTALS TABLES WORLD PR#932
      *  CS 130924 REMOVE THE SIGN FROM PERCENT WORLD PR#932
      * MJD 140430 CHANGED TO PERFORM CALL-WRFORM TO DISPLAY INITIAL-VALUES
      * MJD 140509 ADDED DISPLAY SPACES TO EXIT PROGRAM.
      * BLM 141021 CHANGED LOGIC FOR NEW FILPATH LOAD ROUTINES
      * BAH 160125 SPLIT OUT LTFILE, PD0003
      *  CS 160830 CHANGED WK-CLASS, HOLD-CLASS FROM PIC 9(2) TO 9(3)
      * BLM 170208 CHANGED AGE-DATE, BEG-DATE, END-DATE FROM MMDDYY TO
      *            YYYYMMDD, PR# PD0006
      *  CS 170317 CHANGED DETAIL-LINE LAST PAYMENT TO MM/DD/YY
      * BAH 170606 ACTIVATE 8 DIGIT DATES, PD0006
      *
      * KEC 2017.0606 {PR#00000} PARADATA <A30> [JAY CISZEWSKI]
      *          ADDED 'LN' FILE PREFIX FOR WORKING STORAGE FIELDS.
      *          ADDED 'SP' FILE PREFIX FOR WORKING STORAGE FIELDS.
      * BAH 181210 GLOBAL LNFILE, L2G
      * BAH 20190625 REMOVED LN-REVOLVING, PT0153
      * BAH 20190924 REMOVED ACCESS-CALL LNFILE
      * BAH 20200623 ADDED A4GL-WHERE-CONSTRAINT
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "LIBGB/GBFSPR.CPY".
       COPY "LIBGB/GBFSWKI.CPY".
           SELECT DUE-FILE ASSIGN TO DUE-PATH
                  ORGANIZATION RELATIVE
                  ACCESS MODE DYNAMIC
                  LOCK MODE AUTOMATIC WITH LOCK ON RECORD
                  RELATIVE KEY DUE-KEY
                  FILE STATUS FILE-STAT.
       I-O-CONTROL.
       DATA DIVISION.
       FILE SECTION.
       COPY "LIBGB/GBFDPR.CPY".
       FD  WK-FILE
           LABEL RECORDS ARE STANDARD.
       01  WK-REC.
           03  WK-KEY.
               05  WK-CLASS           PIC 9(3).
      *-----------------------------------------------------------------
      *
      *    COMPANY CLASS TOTAL RECORD
      *
      *    WK-TOTAL-TBL   MAPS WORKING STORAGE TOTAL-TBL(1) CLASS TOTALS
      *-----------------------------------------------------------------
      *
      *    WK-NM-TBL            OCCURS 31 TIMES.
      *    (01) =  CURRENT NONMON
      *    (02) =   1  WEEKS
      *     ....
      *    (31) =  30+ WEEKS
      *
      *    WK-AGETBL :
      *         TYP(1)       (2)             (3)         (4)        (5)
      *        CONTRACTUAL/RECENCY       PAID-AHEAD      REPO      P & L
      *    -------------------------------------------------------------
      * AGE(1)  =   30 DAYS           |  PAID-AHEAD  |  ONHAND  |  PL1
      *    (2)  =   60 DAYS           |  N/A         |  SOLD    |  PL2
      *    (3)  =   90 DAYS           |  N/A         |  N/A     |  PL3
      *    (4)  =  120 DAYS           |  N/A         |  N/A     |  PL4
      *    (5)  =  150 DAYS           |  N/A         |  N/A     |  PL5
      *    (6)  =  180 DAYS           |  N/A         |  N/A     |  N/A
      *    (7)  =  210 + DAYS         |  N/A         |  N/A     |  N/A
      *    (8)  =  <M> MISSPAYS       |  N/A         |  N/A     |  N/A
      *    (9)  =  <P> POTENTIAL 30'S |  N/A         |  N/A     |  N/A
      *    (10) =  CURRENT ACCOUNTS   |  N/A         |  N/A     |  N/A
      *-----------------------------------------------------------------
      *    WK-TYPETBL:
      *       TYP(1) = CONTRACTUAL
      *          (2) = RECENCY
      *          (3) = PAID AHEAD
      *                CONTRACTUALLY CURRENT AND
      *                  DELINQ-RECENCY
      *          (4) = REPO
      *          (5) = P & L
      *
      ******************************************************************
           03  WK-TOTAL-TBL.
               05  WK-NM-TBL            OCCURS 31 TIMES.
                   07  WK-NM-NO          PIC S9(7).
                   07  WK-NM-BAL         PIC S9(10)V99.
                   07  WK-NM-INT         PIC S9(7)V99.
                   07  WK-NM-OTH         PIC S9(9)V99.
                   07  WK-NM-OTH2        PIC S9(9)V99.
                   07  WK-NM-UNEARN      PIC S9(10)V99.
                   07  WK-NM-UNDISC      PIC S9(9)V99.
               05  WK-AGETBL            OCCURS 10 TIMES.
                   07  WK-TYPETBL       OCCURS 5  TIMES.
                       09  WK-NO         PIC S9(7).
                       09  WK-BAL        PIC S9(10)V99.
                       09  WK-INT        PIC S9(7)V99.
                       09  WK-OTH        PIC S9(9)V99.
                       09  WK-OTH2       PIC S9(9)V99.
                       09  WK-UNEARN     PIC S9(10)V99.
                       09  WK-UNDISC     PIC S9(9)V99.

       FD  DUE-FILE
           LABEL RECORDS ARE STANDARD.
       01  DUE-REC.
           03  DUE-BRNO                  PIC 9(4).
           03  DUE-DAY                   PIC 99.
           03  DUE-NO-LOANS              PIC 9(6).
           03  DUE-CR                    PIC X.
           03  DUE-LF                    PIC X.

      ******************************************************************
      *
      *    W O R K I N G - S T O R A G E    S E C T I O N
      *
      ******************************************************************
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)UT/LNAGKC.CBL S35 11/24/23-09:14:17 >".

      ******************************************************************
      *
      *     F I L E   S P E C S  *
      *
      ******************************************************************
       COPY "LIBGB/GB01GR.CPY".
       COPY "LIBGB/GB01GR_SQL.CPY".
       COPY "LIBGB/GBWSGR.CPY".
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
       COPY "LIBLP/LP01SP.CPY".
       COPY "LIBLP/LP01SPA.CPY".
       COPY "LIBLP/LP01SPB.CPY".
       COPY "LIBLP/LP01SPC.CPY".
       COPY "LIBLP/LP01SP_SQL.CPY".
       COPY "LIBLP/LPWSSP.CPY".
       COPY "LIBLP/LP01LTP.CPY".
       COPY "LIBLP/LP01LTP_SQL.CPY".
       COPY "LIBLP/LPWSLTP.CPY".

      *****ASCII DUE DATE FILE, /USR/TMP/DUE_GGGG_MMDDYY
      *    GGGG = GROUP ID OR ENDING BR#, DATE = SYSTEM DATE
       01  DUE-PATH.
           03  FILLER          PIC X(9).
           03  DUE-PATH-DUE    PIC X(4) VALUE "DUE_".
           03  DUE-PATH-MMDDYY PIC 9(6).
           03  DUE-PATH-UNDER  PIC X    VALUE "_".
           03  DUE-PATH-GROUP  PIC X(4).
           03  DUE-PATH-BRNO   REDEFINES DUE-PATH-GROUP
                               PIC 9(4).
      *----------------------------------------------------------------

       01  DUE-KEY       PIC 9(6)     VALUE 0.
       01  WK-IFN        PIC XX       VALUE "WK".

      ******************************************************************
      *
      *    L I N E    W O R K E R S
      *
      ******************************************************************
       01  LN-FEED          PIC 99    VALUE 0.
       01  LN-COUNT         PIC 999   VALUE 99.

       01  PAGE-NUMBER          PIC 9(4)  VALUE 0.

      ******************************************************************
      *
      *     M I S C E L L E N O U S     W O R K E R S
      *
      ******************************************************************
       01  FIRST-TIME           PIC X        VALUE "Y".

      * MORE-THAN-1-CLASS FLAG:
      * THIS FLAG IS AN ATTEMPT TO SUPPRESS CLASS TOTALS IF A BRANCH
      * HAS ONLY ONE CLASS.  (CLASS TOTALS = BRANCH TOTALS FOR ONE CLASS)
      * STARTING VALUE = "N"
      * IF CLASS BREAK IS FOUND THAN SET TO "Y"
      * ONLY RESET BACK TO "N" AFTER CLASS / BRANCH TOTALS ARE PRINTED
      * FOR EACH BRANCH.
       01  MORE-THAN-1-CLASS           PIC X        VALUE "N".
       01  MORE-THAN-1-CLASS-ON-REPORT PIC X        VALUE "N".

       01  RECORDS-FOUND        PIC X        VALUE SPACES.
           88  NO-RECORDS-FOUND              VALUE "N".

       01  SUMMARY-FG           PIC X        VALUE "N".
       01  INVALID-BW-FG        PIC X.

       01  MESS-LIST            PIC X(5)     VALUE "MESS.".
       01  NAME-WORKER          PIC X(34).
       01  FIRST-NAME           PIC X(16).

       01  BAL-WK               PIC 9(7)     COMP-3.
       01  TRANS-DATE           PIC 9(8)     COMP-3 VALUE 0.
       01  UNEARNED             PIC S9(7)V99 COMP-3 VALUE 0.
       01  UNDISCOUNT           PIC S9(7)V99 COMP-3 VALUE 0.

       01  HOLD-LN-OWNBR        PIC 9(4)     VALUE 0.
       01  HOLD-LN-CLASS        PIC 9(3)     VALUE 0.

       01  HOLD-LPS-NUM.
           03  H-LPS-CLASS      PIC XXX.
           03  H-LPS-NUM        PIC X(13).

       01 HOLD-BALANCES.
          03  HOLD-CURBAL       PIC S9(9)V99.

      ******************************************************************
      *
      *               T O T A L S
      *
      ******************************************************************
      *  NOTE:
      *    WK-TOTAL-TBL   MAPS WORKING STORAGE TOTAL-TBL(1) CLASS TOTALS
      *
      *    TOTAL-TBL:
      *       LEV(1) =  BY LOAN CLASS
      *          (2) =  BY BRANCH
      *          (3) =  BY COMPANY
      *
      *-----------------------------------------------------------------
      **(NON-MONTHLY ACCOUNTS)**
      *    (01) =  CURRENT NONMON
      *    (02) =   1  WEEKS
      *     ....
      *    (31) =  30+ WEEKS
      *-----------------------------------------------------------------
      *    T-AGETBL :
      *         TYP(1)       (2)             (3)         (4)        (5)
      *        CONTRACTUAL/RECENCY       PAID-AHEAD      REPO      P & L
      *    -------------------------------------------------------------
      **(MONTHLY ACCOUNTS)**
      * AGE(1)  =   30 DAYS           |  PAID-AHEAD  |  ONHAND  |  PL1
      *    (2)  =   60 DAYS           |  N/A         |  SOLD    |  PL2
      *    (3)  =   90 DAYS           |  N/A         |  N/A     |  PL3
      *    (4)  =  120 DAYS           |  N/A         |  N/A     |  PL4
      *    (5)  =  150 DAYS           |  N/A         |  N/A     |  PL5
      *    (6)  =  180 DAYS           |  N/A         |  N/A     |  N/A
      *    (7)  =  210 + DAYS         |  N/A         |  N/A     |  N/A
      *    (8)  =  <M> MISSPAYS       |  N/A         |  N/A     |  N/A
      *    (9)  =  <P> POTENTIAL 30'S |  N/A         |  N/A     |  N/A
      *    (10) =  CURRENT ACCOUNTS   |  N/A         |  N/A     |  N/A
      *-----------------------------------------------------------------
      *    T-TYPETBL:
      *       TYP(1) = CONTRACTUAL
      *          (2) = RECENCY
      *          (3) = PAID AHEAD
      *                CONTRACTUALLY CURRENT AND
      *                  DELINQ-RECENCY
      *          (4) = REPO
      *          (5) = P & L
      *
      ******************************************************************
       01  TOTAL-TABLE            VALUE ZEROS.
           03  TOTAL-TBL          OCCURS 3  TIMES.
               05  T-NM-TBL       OCCURS 31 TIMES.
                   07  T-NM-NO       PIC S9(7).
                   07  T-NM-BAL      PIC S9(10)V99.
                   07  T-NM-INT      PIC S9(7)V99.
                   07  T-NM-OTH      PIC S9(9)V99.
                   07  T-NM-OTH2     PIC S9(9)V99.
                   07  T-NM-UNEARN   PIC S9(10)V99.
                   07  T-NM-UNDISC   PIC S9(9)V99.
               05  T-AGETBL       OCCURS 10 TIMES.
                   07  T-TYPETBL  OCCURS 5  TIMES.
                       09  T-NO      PIC S9(7).
                       09  T-BAL     PIC S9(10)V99.
                       09  T-INT     PIC S9(7)V99.
                       09  T-OTH     PIC S9(9)V99.
                       09  T-OTH2    PIC S9(9)V99.
                       09  T-UNEARN  PIC S9(10)V99.
                       09  T-UNDISC  PIC S9(9)V99.
       01  TT-TOTALS.
           03  TT-NM-TBL.
               05  TT-NM-NO       PIC S9(7).
               05  TT-NM-BAL      PIC S9(10)V99.
               05  TT-NM-INT      PIC S9(7)V99.
               05  TT-NM-OTH      PIC S9(9)V99.
               05  TT-NM-OTH2     PIC S9(9)V99.
               05  TT-NM-UNEARN   PIC S9(10)V99.
               05  TT-NM-UNDISC   PIC S9(9)V99.
           03  TT-TYPETBL         OCCURS 5 TIMES.
               05  TT-NO          PIC S9(7)    COMP-3.
               05  TT-BAL         PIC S9(10)V99 COMP-3.
               05  TT-INT         PIC S9(7)V99 COMP-3.
               05  TT-OTH         PIC S9(9)V99 COMP-3.
               05  TT-OTH2        PIC S9(9)V99 COMP-3.
               05  TT-UNEARN      PIC S9(10)V99 COMP-3.
               05  TT-UNDISC      PIC S9(9)V99 COMP-3.

      * ----------------------------------------------------------------
      * SUB TOTALS    1 - DELINQUENCY
      *               2 - SLOW
      * ----------------------------------------------------------------
       01  SUB-TT-TOTALS.
           03  SUB-TT-TYPETBL     OCCURS 2 TIMES.
               05  SUB-TT-NO      PIC S9(7)    COMP-3.
               05  SUB-TT-BAL     PIC S9(10)V99 COMP-3.
               05  SUB-TT-INT     PIC S9(7)V99 COMP-3.
               05  SUB-TT-OTH     PIC S9(9)V99 COMP-3.
               05  SUB-TT-OTH2    PIC S9(9)V99 COMP-3.
               05  SUB-TT-UNEARN  PIC S9(10)V99 COMP-3.
               05  SUB-TT-UNDISC  PIC S9(9)V99 COMP-3.

       01  LEV                    PIC 9(6) COMP-3.
       01  AGE                    PIC 9(6) COMP-3.
           88  AGE-CONTRACTUAL             VALUE 1 THRU 10.
           88  AGE-RECENCY                 VALUE 1 THRU 10.
           88  AGE-PAID-AHEAD              VALUE 1.
           88  AGE-REPO                    VALUE 1 THRU 2.
           88  AGE-PL                      VALUE 1 THRU 5.

       01  TYP                    PIC 9(6) COMP-3.
           88  TYP-CONTRACTUAL             VALUE 1.
           88  TYP-RECENCY                 VALUE 2.
           88  TYP-PAID-AHEAD              VALUE 3.
           88  TYP-REPO                    VALUE 4.
           88  TYP-PL                      VALUE 5.

       01  SUB                    PIC 9(6) COMP-3.
       01  SUB1                   PIC 9(6) COMP-3.
       01  SUB2                   PIC 9(6) COMP-3.

       01  DD-TOTAL-TABLE.
           03  DD-TOT-TBL         OCCURS 32 TIMES.
               05  DD-NO          PIC S9(7)    COMP-3.
               05  DD-BAL         PIC S9(10)V99 COMP-3.
               05  DD-INT         PIC S9(7)V99 COMP-3.
               05  DD-OTH         PIC S9(9)V99 COMP-3.
               05  DD-OTH2        PIC S9(9)V99 COMP-3.
               05  DD-PAY         PIC S9(9)V99 COMP-3.

       01  NM-TOTAL-TABLE.
           03  NM-TOT-TBL         OCCURS 27 TIMES.
               05  NM-NO          PIC S9(7)    COMP-3.
               05  NM-BAL         PIC S9(10)V99 COMP-3.
               05  NM-INT         PIC S9(7)V99 COMP-3.
               05  NM-OTH         PIC S9(9)V99 COMP-3.
               05  NM-OTH2        PIC S9(9)V99 COMP-3.
               05  NM-PAY         PIC S9(9)V99 COMP-3.

      *----------------------------------------------------------------
      *    BINARY VALUE OF A LESS ONE.
      *----------------------------------------------------------------
       01  NAME-A                 PIC 9(4) COMP VALUE 64.
       01  NAME-B                 PIC 9(4) COMP VALUE 0.
       01  NAME-BX REDEFINES NAME-B.
           03  FILLER             PIC X.
           03  NAME-TEST          PIC X.

       01  NAME-SUB               PIC 9(4) COMP-3.
       01  NAME-PRINT             PIC X.

       01  RPSUB                 PIC 9(6) COMP.

       01  PLSUB                 PIC 9(6) COMP.

      *----------------------------------------------------------------
      *    AGEING DESCRIPTION TABLE
      *----------------------------------------------------------------
       01  AGE-DESC-TABLE.
           03  FILLER             PIC X(12) VALUE "    30 DAYS:".
           03  FILLER             PIC X(12) VALUE "    60 DAYS:".
           03  FILLER             PIC X(12) VALUE "    90 DAYS:".
           03  FILLER             PIC X(12) VALUE "   120 DAYS:".
           03  FILLER             PIC X(12) VALUE "   150 DAYS:".
           03  FILLER             PIC X(12) VALUE "   180 DAYS:".
           03  FILLER             PIC X(12) VALUE "   210+DAYS:".
           03  FILLER             PIC X(12) VALUE "   MIS-PAYS:".
           03  FILLER             PIC X(12) VALUE "   POT 30'S:".
           03  FILLER             PIC X(12) VALUE "   CURRENT :".

       01  AGE-DESC-TBL           REDEFINES AGE-DESC-TABLE.
           03  AGE-DESC           OCCURS 33 TIMES
                                  PIC X(12).


      *----------------------------------------------------------------
      *    REPO DESCRIPTION TABLE
      *----------------------------------------------------------------
       01  REPO-DESC-TABLE.
           03  FILLER             PIC X(12) VALUE "     ONHAND:".
           03  FILLER             PIC X(12) VALUE "       SOLD:".

       01  REPO-DESC-TBL          REDEFINES REPO-DESC-TABLE.
           03  REPO-DESC          OCCURS 2 TIMES
                                  PIC X(12).

      *----------------------------------------------------------------
      *    P & L DESCRIPTION TABLE
      *----------------------------------------------------------------
       01  PL-DESC-TABLE.
           03  FILLER             PIC X(12) VALUE "         P1:".
           03  FILLER             PIC X(12) VALUE "         P2:".
           03  FILLER             PIC X(12) VALUE "         P3:".
           03  FILLER             PIC X(12) VALUE "         P4:".
           03  FILLER             PIC X(12) VALUE "         P5:".

       01  PL-DESC-TBL           REDEFINES PL-DESC-TABLE.
           03  PL-DESC           OCCURS 5 TIMES
                                 PIC X(12).

       01  CLASS-FG               PIC X(3)    VALUE "   ".
           88  CLASS-BEG                      VALUE "BEG".
           88  CLASS-END                      VALUE "END".
       01  DEALER-FG              PIC X(5)    VALUE "     ".
           88  DEALER-BEG                     VALUE "DLBEG".
           88  DEALER-END                     VALUE "DLEND".

      **DUE DATE TOTALS, 1= BRANCH, 2= GRAND, FOR ASCII FILE FOR WORLD
       01  DUE-DATE-TOTALS.
           03  DUE-DATE-BR-GRAND   OCCURS 2  TIMES.
               05  DUE-DATE-DAY    OCCURS 32 TIMES.
                   07  DUE-DATE-NO PIC 9(6).
      ******************************************************************
      *
      *     S C R E E N   F I E L D S
      *
      ******************************************************************
       COPY "LIBGB/EOBUF_EXT.CPY".
           03  AGE-DATE        PIC 9(8).
           03  BEG-BRANCH      PIC 9(4).
           03  END-BRANCH      PIC 9(4).
           03  BEG-CLASS       PIC 9(3).
           03  END-CLASS       PIC 9(3).
           03  BEG-DEALER      PIC 9(4)V99.
           03  END-DEALER      PIC 9(4)V99.
           03  BEG-AGE         PIC 9(4).
           03  END-AGE         PIC 9(4).
           03  CPOT            PIC X.
           03  CMISS           PIC X.
           03  BEG-BAL         PIC 9(7).
           03  END-BAL         PIC 9(7).
           03  BEG-RAGE        PIC 9(4).
           03  END-RAGE        PIC 9(4).
           03  RPOT            PIC X.
           03  OWNED           PIC X.
           03  PL              PIC X.
           03  PLACTIVE        PIC X.
           03  JD              PIC X.
           03  REPO            PIC X.
           03  LEGAL           PIC X.
           03  DEBT            PIC X.
           03  BANK            PIC X.
           03  RC              PIC X.
           03  ZBAL            PIC X.
           03  ALPHA-SUMMARY   PIC X.
           03  SUM-ONLY        PIC X.
           88  SUM-ONLY-VALID  VALUE "Y" "N" "G" "A" "C".
           03  BEG-LNAMT       PIC 9(6).
           03  END-LNAMT       PIC 9(6).
           03  PLTYPE          PIC X.
           03  GROUP-FLAG      PIC X.
           03  ENT-GROUP       PIC X(4).
           03  UEARN           PIC X.
      * THIS WAS REVOLVING FLAG, IF WE REMOVE IT, THEY HAVE REBUILD BATCHES
           03  FILLER          PIC X.
           03  LOCAL-PRINT-FG  PIC X.
           03  BRULE           PIC X.
           03  BEG-DATE        PIC 9(8).
           03  END-DATE        PIC 9(8).
           03  DATE-TYPE       PIC X.
           03  DEL-FLAG        PIC X.
           03  DUEDATE-SUMMARY PIC X.
           03  PRINT-RANGES    PIC X.

       01  VERSION-CONTROL     PIC X VALUE "I".

       01  SV-BEG-BR           PIC 9(4)   VALUE 9999.
       01  SV-END-BR           PIC 9(4)   VALUE 9999.
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
           03  FILLER           PIC X(26)    VALUE " ".
           03  H-TITLE            PIC X(30)    VALUE
           "AGED TRIAL BALANCE     AS OF: ".
           03  H-AGEDATE        PIC X(10).
           03  FILLER           PIC X(3).

       01  HEAD3.
           03  FILLER      PIC X(8)     VALUE "BRANCH :".
           03  FILLER      PIC XX       VALUE " ".
           03  H-BRNO      PIC 9(4).
           03  H-BRNOX     REDEFINES H-BRNO PIC X(4).
           03  FILLER      PIC X        VALUE " ".
           03  H-BRDESC    PIC X(20).
           03  FILLER      PIC X(77)    VALUE " ".
           03  FILLER      PIC X(20)    VALUE
           "99.99.99 - LOAN DATE".

       01  HEAD4.
           03  FILLER      PIC X(8)     VALUE "CLASS  :".
           03  FILLER      PIC XX       VALUE "  ".
           03  H-LCNO      PIC Z99.
           03  H-LCNOX     REDEFINES H-LCNO PIC XXX.
           03  FILLER      PIC X        VALUE " ".
           03  H-LCDESC    PIC X(20).
           03  FILLER      PIC XX       VALUE "  ".
           03  H-LCBEG     PIC XXX.
           03  H-LCDASH    PIC XXX       VALUE "   ".
           03  H-LCEND     PIC XXX.

       01  HEAD5.
           03  FILLER      PIC X(44)    VALUE " ".
           03  FILLER      PIC X(50)    VALUE
               "LAST  REC-    RECENCY   UNEARNED  CONTRACTUAL     ".
           03  FILLER      PIC X(37)    VALUE
               "PAID        ------  BALANCES  -------".

       01  HEAD6.
           03  FILLER      PIC X(35)    VALUE
               "ACCOUNT NO.  DUE BORROWER          ".
           03  FILLER      PIC X(46)    VALUE
               "DEALER PAYMENT ENCY    PROFILE    CHARGES    P".
           03  FILLER      PIC X(41)    VALUE
               "ROFILE   AGE THRU   APR OT-DUE INT/LC-DUE".
           03  HEAD6-PRIN  PIC X(10)    VALUE
               " PRINCIPAL".

      ******************************************************************
      *
      *    D E T A I L   L I N E
      *
      ******************************************************************
       01  DETAIL-LINE          PIC X(132) VALUE SPACES.

       01  D-LINE1 REDEFINES DETAIL-LINE.
           03  D-LOAN-NUM  PIC X(13).
           03  FILLER      PIC X.
           03  D-DUEDAY    PIC 99.
           03  D-BW-BAD    PIC X.
           03  D-NAME      PIC X(16).
           03  FILLER      PIC X.
           03  D-DLNO      PIC ZZZ9.99.
           03  FILLER      PIC X.
           03  D-PAYDATE   PIC X(08).
           03  FILLER      REDEFINES D-PAYDATE.
               05  FILLER  PIC XX.
               05  D-DASH1 PIC X.
               05  FILLER  PIC XX.
               05  D-DASH2 PIC X.
               05  FILLER  PIC XX.
           03  D-RECENCYX.
               05  D-RECENCY   PIC ZZZZ.
           03  FILLER      PIC X.
           03  D-PYRECPRO  PIC 9(12).
           03  D-XPYRECPRO REDEFINES D-PYRECPRO PIC X(12).
           03  D-UNEARNED  PIC ZZZZZ9.99-.
           03  D-PROFILE   PIC 9(12).
           03  D-XPROFILE  REDEFINES D-PROFILE PIC X(12).
           03  D-CONTUALX.
               05  D-CONTUAL   PIC ZZZZ.
           03  FILLER      PIC X.
           03  D-PDTHRU    PIC 9999.
           03  FILLER      PIC X.
           03  D-APR       PIC ZZ9.99.
           03  D-OTHBAL    PIC ZZZZ9.99-.
           03  D-INTBAL    PIC ZZZZ9.99-.
           03  D-CURBAL    PIC ZZZZZ9.99.

       01  SUMMARY-LINE1 REDEFINES DETAIL-LINE.
           03  S-SUMNAME        PIC X(20).
       01  SUMMARY-LINE2 REDEFINES DETAIL-LINE.
           03  FILLER           PIC X(46).
           03  S-AGETYPE        PIC X(80).
       01  SUMMARY-LINE3 REDEFINES DETAIL-LINE.
           03  S-NAME           PIC X(12).
           03  FILLER           REDEFINES S-NAME.
               05  FILLER       PIC X(9).
               05  S-LASTINITIAL.
                   07  S-NAMEDUE    PIC 99.
               05  S-NAMECOLON  PIC X.
           03  S-NO             PIC ZZZZZZZ9-.
           03  S-NO-P           PIC ZZ9.99-.
           03  S-OTH            PIC Z(6)9.99-.
           03  S-OTH2           PIC Z(7)9.99-.
           03  S-INT            PIC Z(8)9.99-.
           03  S-INT-P          PIC ZZ9.99.
           03  FILLER           PIC X.
           03  S-BAL            PIC Z(10)9.99-.
           03  S-BAL-P          PIC ZZ9.99.
           03  FILLER           PIC X.
           03  S-UNEARN         PIC Z(9)9.99-.
           03  S-UNEARN-P       PIC ZZ9.99.
           03  FILLER           PIC X.
           03  S-UNDISC         PIC Z(7)9.99-.
           03  S-UNDISC-P       PIC ZZ9.99.

       01  RANGE-LN REDEFINES DETAIL-LINE.
           03  RANGE-LINE       PIC X(20).
           03  RANGE-SELECTION  PIC X(112).
           03  RANGE-DEALER REDEFINES RANGE-SELECTION PIC Z(4).99.

       01  SUMMARY1.
           03  FILLER           PIC X(15)  VALUE SPACES.
           03  FILLER           PIC X(53)  VALUE
           "   NO----%      OTH DUE    OTH2 DUE   INT/LC DUE----%".
           03  FILLER           PIC X(42)  VALUE
           "      BALANCE DUE----%       UNEARNED----%".
           03  FILLER           PIC X(18) VALUE
           "     UNE DISC----%".

       01  SUMMARY2.
           03  FILLER           PIC X(15)  VALUE SPACES.
           03  FILLER           PIC X(53)  VALUE
           "   NO----%      OTH DUE    OTH2 DUE   INT/LC DUE----%".
           03  FILLER           PIC X(42)  VALUE
           "      BALANCE DUE----%        PMT DUE----%".


      ******************************************************************
      *
      *     C O P Y    L I B R A R Y    W O R K E R S
      *
      ******************************************************************
       COPY "LIBWI/PFSWINDOWSW.CPY".
       COPY "LIBWI/TSTINTW.CPY".
       COPY "LIBWI/GRSCANW.CPY".
       COPY "LIBWI/LCSCANW.CPY".
       COPY "LIBWI/DLSCANW.CPY".
       COPY "LIBGB/GRWORKW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/DACCESSW.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBGB/AGEINGW.CPY".
       COPY "LIBLP/LPPDUEW.CPY".
       COPY "LIBLP/LPUPWKW.CPY".
       COPY "LIBGB/SPPDTHW.CPY".
       COPY "LIBLP/LPEARNW.CPY".
       COPY "LIBLP/LPIBPCW.CPY".
       COPY "LIBLP/LPCEPPW.CPY".
       COPY "LIBLP/LPSTATW.CPY".
       COPY "LIBLP/LPMDTEW.CPY".
       COPY "LIBLP/LPXDTEW.CPY".
       COPY "LIBGB/EOSPOOLW.CPY".
       COPY "LIBGB/PASSWDW.CPY".
       COPY "LIBGB/BRSECUREW.CPY".
       COPY "LIBGB/MKTEMPW_CMD.CPY".
       COPY "LIBGB/SYSTEMW.CPY".
       COPY "LIBGB/GETPRW.CPY".
       COPY "LIBGB/GPENVW.CPY".

       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBUT/LNAGKC_DEF.CPY".
       COPY "LIBUT/LNAGKC_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".


      ******************************************************************
      *
      *     P R O G R A M   C O N T R O L
      *
      ******************************************************************
       COPY "LIBGB/GETFMW.CPY".

       SCREEN SECTION.
       COPY "LIBUT/LNAGKC_SCN.CPY".

       PROCEDURE DIVISION
           USING FORM-PATHNAME EXIT-PATHNAME.
       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
                 WK-FILE DUE-FILE PR-FILE.
       COPY "LIBGB/DECLRP.CPY".
            PERFORM CLOSE-BR-FILE.
            PERFORM CLOSE-GR1-FILE.
            PERFORM CLOSE-SP1-FILE.
            PERFORM CLOSE-LC1-FILE.
            PERFORM CLOSE-BW1-FILE.
            PERFORM CLOSE-LN3-FILE.
            PERFORM CLOSE-PR-FILE.
           CLOSE WK-FILE DUE-FILE.
       COPY "LIBGB/DECLRP2.CPY".

      ******************************************************************
      *
      *     M A I N    M O D U L E
      *
      ******************************************************************
       MAIN-MODULE SECTION.
      D    STOP MESS.
           PERFORM SQL-CONNECT.
           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "LNAGKC" TO VDU-FORM-NAME.

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


*LIO*      MOVE EXT-FILPATH-BASE           TO FILPATH-BASE.
*LIO*      MOVE BR-MACHINE                 TO FILPATH-MACHINE.
*LIO*      MOVE BR-DATA-PATH               TO FILPATH-DATA.

*LIO*      MOVE "B"                        TO BW-PATH-OVERRIDE
*LIO*                                         LN-PATH-OVERRIDE
*LIO*                                         LTP-PATH-OVERRIDE.

      * 4/23/98 BAH, THIS WAS COMMENTED OUT, BUT THIS IS WHERE IT NEEDS
      * TO BE, THE OTHER ONE, IN END-OF-GROUP, WAS CAUSING THE LAST CLASS
      * TOTALS TO NOT PRINT IF YOU ONLY HAD 1 BRANCH, 2 OR MORE CLASSES !!
      * 042898 MJD, THIS DOES NOT BELONG HERE. IF FLAG IS SET HERE, YOU
      * WILL NEVER SEE TOTALS FOR LAST CLASS IN EACH BRANCH. IT SHOULD NOT
      * BE RESET TO 'N' UNTIL AFTER CLASS BRANCH TOTALS HAVE BEEN PRINTED FOR
      * PREVIOS BRANCH.
      *        MOVE "N"       TO MORE-THAN-1-CLASS.

      *    PERFORM OPEN-BW1-FILE.
      *    PERFORM OPEN-LN3-FILE.

           PERFORM LOAD-BW1-FILE.
           PERFORM LOAD-LN3-FILE.

           MOVE BR-NO     TO LN-OWNBR.
           MOVE BEG-CLASS TO LN-CLASS.
           MOVE 0         TO LN-ACCTNO.

           PERFORM START-LN3-FILE.

      *----------------------------------------------------------------
       NEXT-LN.
           PERFORM READ-LN3-FILE-NEXT.
*L2G       IF IO-BAD OR (LN-OWNBR NOT = LN-PATH-OWNBR)
              GO TO END-OF-GROUP.

           IF LN-OWNBR > END-BRANCH
              GO TO END-OF-GROUP.

           PERFORM TRAP-INTERRUPT.

           IF LN-CLASS < BEG-CLASS OR LN-CLASS > END-CLASS
              GO TO NEXT-LN.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *  POCD - **  IS A DELETED CONTRACT
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

           IF ( LN-POCD-NONLOAN )
              GO TO NEXT-LN.

           IF LN-DLNO < BEG-DEALER OR LN-DLNO > END-DEALER
              GO TO NEXT-LN.

      *  GLB  07/12/99 MOVED UP HERE SO BW-DATE WOULD BE AVAILABLE
           PERFORM PROCESS-BORROWER.

      * VERIFY BETWEEN BEGINNING AND ENDING DATES

           MOVE LN-LOANDATE TO SYS-DATE.
           IF DATE-TYPE = "E"
              MOVE LN-ENTDATE TO SYS-DATE.
           IF DATE-TYPE = "B"
              MOVE BW-REFDATE TO SYS-DATE
              IF BEG-DATE = 0
      * DONT TEST DATE RANGE AT ALL IF BW-REFDATE IS 0 AND BEG-DATE IS
      * 0, YOU ALREADY MATCH AND ENDDATE WILL BOMB OUT WITH A 0
                 IF (BW-REFDATE = 0)
                    GO TO TEST-LNAMT
                 ELSE
                    GO TO TEST-ENDDATE.

           IF SYS-DATE < BEG-DATE
               GO TO NEXT-LN.

       TEST-ENDDATE.
           MOVE LN-LOANDATE TO SYS-DATE.
           IF DATE-TYPE = "E"
              MOVE LN-ENTDATE TO SYS-DATE.
           IF DATE-TYPE = "B"
              MOVE BW-REFDATE TO SYS-DATE.

            IF SYS-DATE > END-DATE
               GO TO NEXT-LN.


       TEST-LNAMT.
      * NEED FOR LOANS THAT ARE 999999.??
           IF BEG-LNAMT = 0 AND END-LNAMT = 999999
              NEXT SENTENCE
           ELSE
              IF LN-SUM-LNAMT < BEG-LNAMT OR LN-SUM-LNAMT > END-LNAMT
                 GO TO NEXT-LN.

           IF PL = "O"
              IF LN-PLDATE = 0
                 GO TO NEXT-LN.

           IF PL = "S"
              IF LN-PLDATE NOT = 0
                 GO TO NEXT-LN.

           IF LN-PLDATE NOT = 0
              IF PLACTIVE = "A"
                 IF LN-PLCD NOT = "P"
                    GO TO NEXT-LN.

           IF LN-PLDATE NOT = 0
              IF PLACTIVE = "I"
                 IF LN-PLCD NOT = "I"
                    IF LN-PLCD NOT = "W"
                       GO TO NEXT-LN.

           IF LN-PLDATE NOT = 0
              IF PLTYPE NOT = "A"
                 IF LN-PLTYPE NOT = PLTYPE
                    GO TO NEXT-LN.

           IF LN-SALEFIN NOT = "Y"
              GO TO CONT-LN.

           IF RC = "F"
              IF LN-DLRECOURSE NOT = "F"
                 GO TO NEXT-LN.

           IF RC = "P"
              IF LN-DLRECOURSE NOT = "P"
                 GO TO NEXT-LN.

           IF RC = "N"
              IF LN-DLRECOURSE NOT = "N"
                 GO TO NEXT-LN.


      *----------------------------------------------------------------
       CONT-LN.
           IF OWNED = "O"
              IF LN-SERVACCT = "Y"
                 GO TO NEXT-LN.

           IF OWNED = "S"
              IF LN-SERVACCT = "N"
                 GO TO NEXT-LN.

           IF JD = "O"
              IF LN-JDDATE = 0
                 GO TO NEXT-LN.

           IF JD = "S"
              IF LN-JDDATE NOT = 0
                 GO TO NEXT-LN.

           IF REPO = "O"
              IF NOT (LN-REPOCD = "X" OR "S")
                 GO TO NEXT-LN.

           IF REPO = "S"
              IF LN-REPOCD = "X" OR "S"
                 GO TO NEXT-LN.

           IF BANK = "O"
              IF LN-BNKRPTDATE = 0
                 GO TO NEXT-LN.

           IF BANK = "S"
              IF LN-BNKRPTDATE NOT = 0
                 GO TO NEXT-LN.

           IF LEGAL = "O"
              IF LN-LEGALDATE = 0
                 GO TO NEXT-LN.

           IF LEGAL = "S"
              IF LN-LEGALDATE NOT = 0
                 GO TO NEXT-LN.

           IF DEBT = "O"
              IF LN-DEBTAIDATE = 0
                 GO TO NEXT-LN.

           IF DEBT = "S"
              IF LN-DEBTAIDATE NOT = 0
                 GO TO NEXT-LN.

           IF ZBAL = "O"
              IF ( LN-CURBAL NOT = 0 ) OR
                 ( LN-INTBAL NOT = 0 ) OR
                 ( LN-LCBAL  NOT = 0 ) OR
                 ( LN-OTHBAL NOT = 0 )
                     GO TO NEXT-LN.

           IF ZBAL = "S"
              IF LN-CURBAL = 0
                 IF LN-INTBAL = 0
                    IF LN-LCBAL = 0
                       IF LN-OTHBAL = 0
                          GO TO NEXT-LN.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      * NEED FOR LOANS THAT ARE 999999.??
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF BEG-BAL = 0 AND END-BAL = 9999999
              NEXT SENTENCE
           ELSE
              COMPUTE BAL-WK ROUNDED =
                      LN-CURBAL + LN-INTBAL + LN-LCBAL + LN-OTHBAL
              IF ( BAL-WK < BEG-BAL ) OR ( BAL-WK > END-BAL )
                 GO TO NEXT-LN.

           MOVE LN-ORGST     TO SP-ORGST.
           MOVE LN-SPRCLASS  TO SP-SPRCLASS.
           MOVE LN-SUBCLASS  TO SP-SUBCLASS.
           MOVE LN-LAWCODE   TO SP-LAWCODE.

           IF LPWSSP-SP1-KEY NOT = SP1-KEY
              MOVE SP1-KEY TO LPWSSP-SP1-KEY
              PERFORM READ-SP1-FILE
              IF IO-FG NOT = 0
                 STRING "MISSING SPR: ",LN-OWNBR,LN-CLASS,LN-ACCTNO
                               DELIMITED BY "  " INTO MESS
                 PERFORM SEND-MESS
                 GO TO IO-ERROR.

           IF BRULE = "T"
              MOVE SP-TRWBANKRULE TO SP-BANKRULE
              MOVE SP-TRWCONTPDTH TO SP-CONTPDTH
              MOVE SP-TRWBANKUNITPER-CD TO SP-BANKUNITPER-CD.

           PERFORM COMPUTE-RECENCY.
           PERFORM COMPUTE-CONTRACTUAL.
           PERFORM COMPUTE-PAID-AHEAD.
           PERFORM COMPUTE-REPO.
           PERFORM COMPUTE-PL.

           IF NOT (BEG-AGE = 0 AND END-AGE = 9999)
              IF CONTRACTUAL < BEG-AGE OR CONTRACTUAL > END-AGE
                 GO TO NEXT-LN.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      * TEST CONTRACTUAL: POTENTIALS AND MISS PAYS.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

           IF CPOT = "S"
              IF CPOT30 = "Y"
                 GO TO NEXT-LN
              END-IF
           ELSE
              IF CPOT = "O"
                 IF CPOT30 NOT = "Y"
                    GO TO NEXT-LN.

           IF CMISS = "S"
              IF CPOT30 = "M"
                 GO TO NEXT-LN
              END-IF
           ELSE
              IF CMISS = "O"
                 IF CPOT30 NOT = "M"
                    GO TO NEXT-LN.


           IF NOT (BEG-RAGE = 0 AND END-RAGE = 9999)
              IF RECDEL < BEG-RAGE OR RECDEL > END-RAGE
                 GO TO NEXT-LN.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      * TEST RECENCY: POTENTIALS
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF RPOT = "S"
              IF RDPOT30 NOT = " "
                  GO TO NEXT-LN
              END-IF
           ELSE
              IF RPOT = "O"
                 IF RDPOT30 = " "
                    GO TO NEXT-LN.

           PERFORM COMPUTE-EARNINGS.
           MOVE EARN-TOT-UNEARN TO UNEARNED.

      * TEST FOR UNEARNED PRINT OPTION
           IF UEARN = "O"
              IF UNEARNED NOT = 0
                 GO TO NEXT-LN.

           IF UEARN = "S"
              IF UNEARNED = 0
                 GO TO NEXT-LN.

      * UNEARNED TOTALS WILL BE LESS UNEARNED DISCOUNT
           SUBTRACT EARN-UNEARN(5) FROM EARN-TOT-UNEARN
               GIVING UNEARNED.
           MOVE EARN-UNEARN(5) TO UNDISCOUNT.

           IF FIRST-TIME = "Y"
              MOVE LN-OWNBR TO HOLD-LN-OWNBR
              MOVE LN-CLASS TO HOLD-LN-CLASS
              MOVE 0        TO LC-CODE
              PERFORM SET-CLASS-HEADINGS
              PERFORM SET-BRANCH-HEADINGS.

           IF LN-OWNBR NOT = HOLD-LN-OWNBR
              PERFORM CLASS-TOTALS
              PERFORM BRANCH-TOTALS
              MOVE "N"   TO MORE-THAN-1-CLASS
              MOVE LN-OWNBR TO HOLD-LN-OWNBR
              MOVE LN-CLASS TO HOLD-LN-CLASS
              PERFORM SET-CLASS-HEADINGS
              PERFORM SET-BRANCH-HEADINGS.

           IF LN-CLASS NOT = HOLD-LN-CLASS
              MOVE "Y" TO MORE-THAN-1-CLASS
              MOVE "Y" TO MORE-THAN-1-CLASS-ON-REPORT
              PERFORM CLASS-TOTALS
              MOVE LN-CLASS TO HOLD-LN-CLASS
              PERFORM SET-CLASS-HEADINGS.

           PERFORM DETAIL-LINE-PROCESS.

           GO TO NEXT-LN.

      *----------------------------------------------------------------
       END-OF-GROUP.
      *    PERFORM CLOSE-LN3-FILE.
      *    PERFORM CLOSE-BW1-FILE.

           IF LOCAL-PRINT-FG = "Y"
              IF FIRST-TIME NOT = "Y"
                 PERFORM CLASS-TOTALS
                 PERFORM BRANCH-TOTALS
                 MOVE "N" TO MORE-THAN-1-CLASS
                 MOVE "Y" TO FIRST-TIME
                 MOVE 0 TO PAGE-NUMBER
              ELSE
                 MOVE 0 TO PAGE-NUMBER.

           IF DUEDATE-SUMMARY = "F"
              MOVE 1 TO LEV
              PERFORM UPDATE-DUE-DATE-FILE
              MOVE ZEROS TO DUE-DATE-BR-GRAND(1).

      * BAH 4/23/98 COMMENTED OUT, NEEDS TO STAY "Y" UNTIL YOU READ
      * THE NEXT BRANCH. IF YOU HIT END OF FILE, YOUR LAST CLASS TOTALS
      * DIDN'T PRINT !!
      * MJD 042898 DO NOT RESET HERE. MUST STAY "Y" UNTIL LAST
      * CLASS IS TOTALD OUT FOR THE BRANCH.
      *    MOVE "N" TO MORE-THAN-1-CLASS.

           GO TO NEXT-GROUP-LOC.
      *----------------------------------------------------------------
       END-ROUTINE.
           IF NO-RECORDS-FOUND
              GO TO END-PROGRAM.

           IF LOCAL-PRINT-FG = "N"
              PERFORM CLASS-TOTALS
              PERFORM BRANCH-TOTALS
              PERFORM COMPANY-TOTALS
              IF PRINT-RANGES = "Y"
              PERFORM PRINT-RANGE-LINES.
           GO TO END-PROGRAM.

      ******************************************************************
       INITIALIZATION SECTION.
      ******************************************************************
           MOVE LNAGKC-QUEUE-POS TO Q-POS.
           MOVE LNAGKC-COPIES-POS TO C-POS.
           MOVE LNAGKC-PRU-POS   TO P-POS.
           MOVE SPACES TO Q-BUF C-BUF P-BUF.
       COPY "LIBGB/EOINIT.CPY".

           MOVE SPACES      TO RECORDS-FOUND.

           MOVE AGE-DATE    TO AGEING-DATE TRANS-DATE DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-ALPDATE.
           MOVE ALP-DATE    TO H-AGEDATE.

           PERFORM ZERO-TT-TOTALS.
           PERFORM ZERO-DD-TOTALS.
           PERFORM ZERO-NM-TOTALS.

      *----------------------------------------------------------------
       SET-HEAD.
           MOVE EXT-CONAME-CONAME  TO H-NAME.
           MOVE EXT-CONAME-SYSDATE TO H-DATE.
           MOVE EXT-CONAME-USER-ID TO H-USERID.
           IF EXT-ROUTE-BUF = "70"
              MOVE EXT-EOBUF-MESS TO H-TITLE.

      * BECAUSE WE USED TO DO XFER AND WRFORM AND NOT CHECK STAT IF MESS
      * DIDNT EXIST, DO NOT WANT IT TO DISPLAY FOR BATCH
           IF EXT-ROUTE-BUF = "00"
              MOVE "REPORT IN PROGRESS...................." TO WR-BUF
              MOVE MESS-LIST TO WR-LIST
              PERFORM CALL-WRFORM.

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

           IF LOCAL-PRINT-FG NOT = "Y"
              PERFORM OPEN-PR-FILE.
           PERFORM OPEN-BR-FILE.
           PERFORM OPEN-GR1-FILE.
           PERFORM OPEN-SP1-FILE.
           PERFORM OPEN-LC1-FILE.

           PERFORM OPEN-BW1-FILE.
           PERFORM OPEN-LN3-FILE.

           MOVE MKTEMP-DEFAULT TO MKTEMP-BUF.
           PERFORM MKTEMP-CALL.
           PERFORM OPEN-WK-FILE-OUTPUT.
           PERFORM CLOSE-WK-FILE.
           PERFORM OPEN-WK-FILE.

           IF DUEDATE-SUMMARY = "F"
              MOVE MKTEMP-DEFAULT TO DUE-PATH
              MOVE "DUE_"         TO DUE-PATH-DUE
              IF GROUP-FLAG = "Y"
                 MOVE ENT-GROUP   TO DUE-PATH-GROUP
              ELSE
                 MOVE END-BRANCH  TO DUE-PATH-BRNO
              END-IF
              MOVE "_"            TO DUE-PATH-UNDER
              MOVE EXT-CONAME-SYSDATE TO ALP-DATE
              PERFORM CONVERT-ALPDATE-TO-MMDDYY
              MOVE DATE-MMDDYY    TO DUE-PATH-MMDDYY
              PERFORM OPEN-DUE-FILE-OUTPUT
              PERFORM CLOSE-DUE-FILE
              PERFORM OPEN-DUE-FILE.

           MOVE "N" TO MORE-THAN-1-CLASS
                       MORE-THAN-1-CLASS-ON-REPORT.

           PERFORM ENABLE-INTERRUPT.

      ******************************************************************
       EO-EXEC-AUDIT SECTION.
      ******************************************************************
           MOVE EXT-EOBUF-DATE2 TO AGE-DATE.
           MOVE JULIAN-BEG TO BEG-DATE.
           MOVE JULIAN-END TO END-DATE.

           IF DEL-FLAG NOT = "Y"
              GO TO EXIT-EO-EXEC-AUDIT.

      * SETUP NEXT MONTH DATE (WORLD ONLY, FOR CORRECT EOM AGEING):
           MOVE EXT-EOBUF-DATE2 TO NDTE-DATE.
           MOVE 1               TO NDTE-DD.
           MOVE 1 TO NDTE-HOLD.
           PERFORM INCREMENT-MONTHS.
           MOVE NDTE-DATE TO AGE-DATE.

       EXIT-EO-EXEC-AUDIT.
           EXIT.

      ******************************************************************
       ENTER-PARAMETERS SECTION.
      ******************************************************************
           MOVE "L" TO DATE-TYPE.

      *----------------------------------------------------------------
       ENTER-PARM.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      * FOR RE-ENTRY ON F4 FROM ACCEPT.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

           IF VDUSTATUS = "1U" GO TO SUMYN.

      *----------------------------------------------------------------
       AGEDT.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      * BATCH WILL USE DATE RAN
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

           IF EXT-ROUTE-BUF = "40" OR "50"
              GO TO BR-01.

           MOVE "ERNM060AGEDATE." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO AGEDT.

           MOVE VDU-DATE-YYYYMMDD TO AGE-DATE.

           IF AGE-DATE = ZERO
              MOVE EXT-CONAME-SYSDATE TO ALP-DATE
              PERFORM CONVERT-ALPDATE-TO-YYYYMMDD
              MOVE "S  8000AGEDATE." TO VDU
              MOVE DATE-YYYYMMDD TO VDU-DATE-YYYYMMDD AGE-DATE
              PERFORM SCREENIO.

           GO TO BEGDATE-01.

       DATE-TYPE.
           MOVE "E  A010DATETYPE." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO AGEDT.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO DATE-TYPE.
           MOVE VDUBUF TO DATE-TYPE.
           IF DATE-TYPE NOT = "E" AND NOT = "L" AND NOT = "B"
              MOVE "'E'=ENTRYDATE 'L'=LOANDATE 'B'=BORROWER REFDATE"
              TO MESS
              PERFORM SEND-MESS
              GO TO DATE-TYPE.


      *---------------------------------------------------------------
       BEGDATE-01.
           IF EXT-ROUTE-BUF = "40" OR "50"
              GO TO BR-01.

           MOVE "ERNM060DATE1." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO DATE-TYPE.
           IF VDUSTATUS = "16" GO TO ALL-DATE.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO BEGDATE-01.

           MOVE VDU-DATE-YYYYMMDD TO BEG-DATE.
      *-----------------------------------------------------------
       ENDDATE-02.

           IF EXT-ROUTE-BUF = "40" OR "50"
              GO TO BR-01.

           MOVE "ERNM060DATE2." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO BEGDATE-01.
           IF NOT (VDUSTATUS = "00" OR "LD") GO TO ENDDATE-02.

           MOVE VDU-DATE-YYYYMMDD TO END-DATE.

      * MAKE SURE THE DATE RANGE IS VALID.

           IF NOT (DATE-TYPE = "B" AND BEG-DATE = 0)
              IF END-DATE < BEG-DATE
                 MOVE "INVALID DATE RANGE" TO MESS
                 PERFORM SEND-MESS
                 GO TO BEGDATE-01.

           GO TO BR-01.

      *----------------------------------------------------------
       ALL-DATE.

           MOVE "S  8080DATE1." TO VDU.
           MOVE JULIAN-BEG TO BEG-DATE VDU-DATE-YYYYMMDD.
           PERFORM SCREENIO.
           MOVE "S  8080DATE2."   TO VDU.
           MOVE JULIAN-END TO END-DATE VDU-DATE-YYYYMMDD.
           PERFORM SCREENIO.

      *----------------------------------------------------------------
       BR-01.
           MOVE "E  A000BR1." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "16" GO TO ALL-BR.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO ENDDATE-02.

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

      *----------------------------------------------------------------
       BRANCH-IS-NUMERIC.
           MOVE "N"           TO GROUP-FLAG.
           MOVE WGR-SRESULT   TO BEG-BRANCH.
           MOVE "SN N040BR1." TO VDU.
           MOVE BEG-BRANCH    TO VDUNUM0.
           PERFORM SCREENIO.

      *----------------------------------------------------------------
       BR-02.
           MOVE "ENNN040BR2." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
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
           MOVE 0             TO BEG-BRANCH VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN040BR2." TO VDU.
           MOVE 9999          TO END-BRANCH VDUNUM0.
           PERFORM SCREENIO.
           MOVE "N"           TO GROUP-FLAG.

      *-----------------------------------------------------------------
       CHECK-BRANCH-SECURITY.
           PERFORM BR-RANGE-SECURITY.

           IF BR-SECURITY-OKAY = "N"
              GO TO BR-01.

      *----------------------------------------------------------------
       CL-01.
           MOVE "ENNN020CL1." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "16" GO TO ALL-CL.
           IF VDUSTATUS = "1U" GO TO BR-02.
           IF VDUSTATUS = "1<"
              MOVE "BEG" TO CLASS-FG
              PERFORM CLASS-SCAN
              IF IO-FG NOT = 0
              GO TO CL-01.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO CL-01.

           MOVE VDUNUM0 TO BEG-CLASS.

      *----------------------------------------------------------------
       CL-02.
           MOVE "ENNN020CL2." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO CL-01.
           IF VDUSTATUS = "1<"
              MOVE "END" TO CLASS-FG
              PERFORM CLASS-SCAN
              IF IO-FG NOT = 0
              GO TO CL-02.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO CL-02.

           MOVE VDUNUM0 TO END-CLASS.

           IF BEG-CLASS > END-CLASS
              PERFORM INVALID-RANGE
              GO TO CL-01.

           GO TO DL-01.

      *----------------------------------------------------------------
       ALL-CL.
           MOVE "SNNN020CL1." TO VDU.
           MOVE 0             TO BEG-CLASS VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN020CL2." TO VDU.
           MOVE 99            TO END-CLASS VDUNUM0.
           PERFORM SCREENIO.

      *----------------------------------------------------------------
       DL-01.
           MOVE "ENNN042DL1." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "16" GO TO ALL-DL.
           IF VDUSTATUS = "1U" GO TO CL-02.
           IF VDUSTATUS = "1<"
              MOVE "DLBEG" TO DEALER-FG
              PERFORM DEALER-SCAN
              IF IO-FG NOT = 0
              GO TO DL-01.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO DL-01.

           MOVE VDUNUM2 TO BEG-DEALER.

      *----------------------------------------------------------------
       DL-02.
           MOVE "ENNN042DL2." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO DL-01.
           IF VDUSTATUS = "1<"
              MOVE "DLEND" TO DEALER-FG
              PERFORM DEALER-SCAN
              IF IO-FG NOT = 0
              GO TO DL-02.

           IF NOT (VDUSTATUS = "00" OR "1D") GO TO DL-02.

           MOVE VDUNUM2 TO END-DEALER.

           IF BEG-DEALER > END-DEALER
              PERFORM INVALID-RANGE
              GO TO DL-01.

           GO TO AGE-01.

      *----------------------------------------------------------------
       ALL-DL.
           MOVE "SNNN042DL1." TO VDU.
           MOVE 0             TO BEG-DEALER VDUNUM2.
           PERFORM SCREENIO.
           MOVE "SNNN042DL2." TO VDU.
           MOVE 9999.99       TO END-DEALER VDUNUM2.
           PERFORM SCREENIO.

      *----------------------------------------------------------------
       AGE-01.
           MOVE "ENNN040AGE1." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "16" GO TO ALLAGE.
           IF VDUSTATUS = "1U" GO TO DL-02.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO AGE-01.

           MOVE VDUNUM0 TO BEG-AGE.

      *----------------------------------------------------------------
       AGE-02.
           MOVE "ENNN040AGE2." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO AGE-01.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO AGE-02.

           MOVE VDUNUM0 TO END-AGE.

           IF BEG-AGE > END-AGE
              PERFORM INVALID-RANGE
              GO TO AGE-01.

           IF NOT ( BEG-AGE < 30)
               MOVE "SNNA010C-POT." TO VDU
               MOVE " " TO VDUBUF
               PERFORM SCREENIO
               MOVE "SNNA010C-MISS." TO VDU
               MOVE " " TO VDUBUF
               PERFORM SCREENIO
               GO TO RAGE01.
           GO TO CPOTYN.

      *----------------------------------------------------------------
       ALLAGE.
           MOVE "SNNN040AGE1." TO VDU.
           MOVE 0              TO BEG-AGE VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN040AGE2." TO VDU.
           MOVE 9999           TO END-AGE VDUNUM0.
           PERFORM SCREENIO.

      *----------------------------------------------------------------
       CPOTYN.
           MOVE "E  A010C-POT." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO AGE-02.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO CPOTYN.

           MOVE VDUBUF TO CPOT.
           IF CPOT = "S"
                 MOVE "SNNA010C-MISS." TO VDU
                 MOVE "S" TO VDUBUF
                 PERFORM SCREENIO
                 GO TO RAGE01.
           IF CPOT NOT = "I" AND NOT = "S" AND NOT = "O"
              GO TO CPOTYN.

      *----------------------------------------------------------------
       CMISSYN.
           MOVE "E  A010C-MISS." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO CPOTYN.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO CMISSYN.

           MOVE VDUBUF TO CMISS.

           IF CMISS NOT = "I" AND NOT = "S" AND NOT = "O"
              GO TO CMISSYN.

      *----------------------------------------------------------------
       RAGE01.
           MOVE "ENNN040RAGE1." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "16" GO TO ALRAGE.
           IF VDUSTATUS = "1U"
                IF NOT (BEG-AGE < 30)
                   GO TO AGE-02
                ELSE
                IF CPOT = "S"
                    GO TO CPOTYN
                ELSE
                    GO TO CMISSYN.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO RAGE01.

           MOVE VDUNUM0 TO BEG-RAGE.

      *----------------------------------------------------------------
       RAGE02.
           MOVE "ENNN040RAGE2." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO RAGE01.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO RAGE02.

           MOVE VDUNUM0 TO END-RAGE.

           IF BEG-RAGE > END-RAGE
              PERFORM INVALID-RANGE
              GO TO RAGE01.

           IF NOT (BEG-RAGE < 30)
               MOVE "SNNA010R-POT." TO VDU
               MOVE " " TO VDUBUF
               PERFORM SCREENIO
               GO TO BAL01.
           GO TO RPOTYN.

      *----------------------------------------------------------------
       ALRAGE.
           MOVE "SNNN040RAGE1." TO VDU.
           MOVE 0               TO BEG-RAGE VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN040RAGE2." TO VDU.
           MOVE 9999            TO END-RAGE VDUNUM0.
           PERFORM SCREENIO.

      *----------------------------------------------------------------
       RPOTYN.
           MOVE "E  A010R-POT." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO RAGE02.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO RPOTYN.

           MOVE VDUBUF TO RPOT.

           IF RPOT NOT = "I" AND NOT = "S" AND NOT = "O"
              GO TO RPOTYN.

      *----------------------------------------------------------------
       BAL01.
           MOVE "ENNN070BB1." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "16" GO TO ALLBAL.
           IF VDUSTATUS = "1U"
                IF NOT (BEG-RAGE < 30)
                   GO TO RAGE02
                ELSE
                   GO TO RPOTYN.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO BAL01.

           MOVE VDUNUM0 TO BEG-BAL.

      *----------------------------------------------------------------
       BAL02.
           MOVE "ENNN070BB2." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO BAL01.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO BAL02.

           MOVE VDUNUM0 TO END-BAL.

           IF BEG-BAL > END-BAL
              PERFORM INVALID-RANGE
              GO TO BAL01.

           GO TO AMT01.

      *----------------------------------------------------------------
       ALLBAL.
           MOVE "SNNN070BB1." TO VDU.
           MOVE 0             TO BEG-BAL VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN070BB2." TO VDU.
           MOVE 9999999       TO END-BAL VDUNUM0.
           PERFORM SCREENIO.

      *----------------------------------------------------------------
       AMT01.
           MOVE "ENNN060LA1." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "16" GO TO ALLAMT.
           IF VDUSTATUS = "1U" GO TO BAL02.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO AMT01.

           MOVE VDUNUM0 TO BEG-LNAMT.

      *----------------------------------------------------------------
       AMT02.
           MOVE "ENNN060LA2." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO AMT01.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO AMT02.

           MOVE VDUNUM0 TO END-LNAMT.

           IF BEG-LNAMT > END-LNAMT
              PERFORM INVALID-RANGE
              GO TO AMT01.

           GO TO TYPEYN.

      *----------------------------------------------------------------
       ALLAMT.
           MOVE "SNNN060LA1." TO VDU.
           MOVE 0             TO BEG-LNAMT VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN060LA2." TO VDU.
           MOVE 999999        TO END-LNAMT VDUNUM0.
           PERFORM SCREENIO.

      *----------------------------------------------------------------
       TYPEYN.
           MOVE "E  A010TYPE." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO AMT02.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO TYPEYN.

           MOVE VDUBUF TO OWNED.

           IF OWNED NOT = "O" AND NOT = "S" AND NOT = "B"
              GO TO TYPEYN.

      *----------------------------------------------------------------
       PLYN.
           MOVE "E  A010PL." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO TYPEYN.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO PLYN.

           MOVE VDUBUF TO PL.

           IF PL = "I" OR "O"
              GO TO PLTYPEYN.

           IF PL NOT = "S" GO TO PLYN.

           MOVE "S  A010ACTIVE." TO VDU.
           MOVE "B"              TO PLACTIVE VDUBUF.
           PERFORM SCREENIO.
           GO TO BANKRULE.

      *----------------------------------------------------------------
       PLTYPEYN.
           MOVE "E  A010PLTYPE." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO PLYN.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO PLTYPEYN.

           MOVE VDUBUF TO PLTYPE.

           IF NOT (PLTYPE = "A" OR "P" OR "2" OR "3" OR "4" OR "5")
              GO TO PLTYPEYN.

      *----------------------------------------------------------------
       ACTVYN.
           MOVE "E  A010ACTIVE." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO PLTYPEYN.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO ACTVYN.

           MOVE VDUBUF TO PLACTIVE.

           IF PLACTIVE NOT = "I" AND NOT = "A" AND NOT = "B"
              GO TO ACTVYN.

       BANKRULE.
           MOVE "E  A010BRULE." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO ACTVYN.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO BANKRULE.
           MOVE VDUBUF TO BRULE.
           IF BRULE = "I" GO TO RECRSE.
           IF BRULE NOT = "T" GO TO BANKRULE.

      *----------------------------------------------------------------
       RECRSE.
           MOVE "E  A010RC." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO BANKRULE.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO RECRSE.

           MOVE VDUBUF TO RC.

           IF RC NOT = "F"
              AND NOT = "P"
               AND NOT = "N"
                AND NOT = "A"
                    GO TO RECRSE.

      *----------------------------------------------------------------
       JDYN.
           MOVE "E  A010JD." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO RECRSE.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO JDYN.

           MOVE VDUBUF TO JD.

           IF JD NOT = "I" AND NOT = "S" AND NOT = "O"
              GO TO JDYN.

      *----------------------------------------------------------------
       REPOYN.
           MOVE "E  A010RP." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO JDYN.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO REPOYN.

           MOVE VDUBUF TO REPO.

           IF REPO NOT = "I" AND NOT = "S" AND NOT = "O"
              GO TO REPOYN.

      *----------------------------------------------------------------
       BANKYN.
           MOVE "E  A010BK." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO REPOYN.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO BANKYN.

           MOVE VDUBUF TO BANK.

           IF BANK NOT = "I" AND NOT = "S" AND NOT = "O"
              GO TO BANKYN.

      *----------------------------------------------------------------
       LEGLYN.
           MOVE "E  A010LG." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO BANKYN.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO LEGLYN.

           MOVE VDUBUF TO LEGAL.

           IF LEGAL NOT = "I" AND NOT = "S" AND NOT = "O"
              GO TO LEGLYN.

      *----------------------------------------------------------------
       DEBTYN.
           MOVE "E  A010DA." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO LEGLYN.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO DEBTYN.

           MOVE VDUBUF TO DEBT.

           IF DEBT NOT = "I" AND NOT = "S" AND NOT = "O"
              GO TO DEBTYN.

      *----------------------------------------------------------------
       ZBALYN.
           MOVE "E  A010ZBAL." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO DEBTYN.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO ZBALYN.

           MOVE VDUBUF TO ZBAL.

           IF ZBAL = "I"     GO TO UEARNYN.
           IF ZBAL = "S"     GO TO UEARNYN.
           IF ZBAL NOT = "O" GO TO ZBALYN.

      *----------------------------------------------------------------
       UEARNYN.
           MOVE "E  A010UEARN." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO ZBALYN.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO UEARNYN.

           MOVE VDUBUF TO UEARN.

           IF UEARN = "I"     GO TO ENTER-DEL-FLAG.
           IF UEARN = "S"     GO TO ENTER-DEL-FLAG.
           IF UEARN NOT = "O" GO TO UEARNYN.

      *----------------------------------------------------------------
       ENTER-DEL-FLAG.
           MOVE "S  A200DELPROMPT:P." TO VDU.
           MOVE "BEG MO DELINQENCY?: "    TO VDUBUF.
           PERFORM SCREENIO.
      * DEFAUT FLAG TO 'N'
           MOVE "S  A010DELFLAG." TO VDU.
           IF DEL-FLAG = "Y" OR "N"
              MOVE DEL-FLAG TO VDUBUF
           ELSE
              MOVE "N" TO VDUBUF.
           PERFORM SCREENIO.
           MOVE "E  A010DELFLAG." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO UEARNYN.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO ENTER-DEL-FLAG.

           MOVE VDUBUF TO DEL-FLAG.

           IF DEL-FLAG NOT = "Y" AND NOT = "N"
              GO TO ENTER-DEL-FLAG.

           IF DEL-FLAG = "N"
              GO TO DUEYN.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      * SETUP NEXT MONTH DATE:
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE AGE-DATE TO NDTE-DATE.
           MOVE 1        TO NDTE-DD.
           MOVE 1        TO NDTE-HOLD.
           PERFORM INCREMENT-MONTHS.
           MOVE NDTE-DATE TO AGE-DATE.
      *-----------------------------------------------------------------
       DUEYN.
           MOVE "E  A010DUESUM." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO ENTER-DEL-FLAG.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO DUEYN.

           MOVE VDUBUF TO DUEDATE-SUMMARY.

           IF NOT (DUEDATE-SUMMARY = "N" OR "Y" OR "F")
              GO TO DUEYN.

           IF DUEDATE-SUMMARY = "F"
               MOVE 0  TO PASSWDW-SEQ
               MOVE "CREATE AN ASCII DUE DATE FILE"
                  TO PASSWDW-PROMPT2
               PERFORM VERIFY-PASSWORD
               IF PASSWDW-STAT NOT = "OK"
                  GO TO DUEYN.

      *----------------------------------------------------------------
       ALPYN.
           MOVE "E  A010ALPSUM." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO DUEYN.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO ALPYN.

           MOVE VDUBUF TO ALPHA-SUMMARY.

           IF ALPHA-SUMMARY = "Y"     GO TO SUMYN.
           IF ALPHA-SUMMARY NOT = "N" GO TO ALPYN.

      *----------------------------------------------------------------
       SUMYN.
           MOVE "E  A010SUMONLY." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO ALPYN.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO SUMYN.

           MOVE VDUBUF TO SUM-ONLY.

           IF NOT SUM-ONLY-VALID
              MOVE "INVALID ENTRY, USE F8-HELP FOR MORE INFORMATION"
                                                              TO MESS
              PERFORM SEND-MESS
              GO TO SUMYN.

      *----------------------------------------------------------------
       RANGES-YN.
           MOVE "E  A010RANGES." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO SUMYN.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO RANGES-YN.

           MOVE VDUBUF TO PRINT-RANGES.

           IF NOT (PRINT-RANGES = "Y" OR "N")
              GO TO RANGES-YN.

      *----------------------------------------------------------------
       LOCAL-PRINT.
           IF EXT-ROUTE-BUF NOT = "40"
              IF EXT-ROUTE-BUF NOT = "50"
                 MOVE "N" TO LOCAL-PRINT-FG
                 GO TO EXIT-PARM.

           IF BEG-BRANCH = END-BRANCH
              MOVE "N" TO LOCAL-PRINT-FG
              GO TO EXIT-PARM.

      *    IF GROUP-FLAG = "Y"
      *       MOVE "N" TO LOCAL-PRINT-FG
      *       GO TO EXIT-PARM.
      *-----------------------------------------------------------------
       ASK-LOCAL.
           MOVE "SPOOL TO LOCAL DIRECTORY? (Y/N)" TO MESS.
           MOVE MESS-LIST TO WR-LIST.
           MOVE MESS TO WR-BUF.
           PERFORM CALL-WRFORM.
           MOVE "S  A200SPOOL-PMPT:P." TO VDU.
           MOVE "LOCAL SPOOL   (Y/N):"    TO VDUBUF.
           PERFORM SCREENIO.

      *     MOVE "S  A050YES-NO." TO VDU.
      *     MOVE "(Y/N)"          TO VDUBUF.
      *     PERFORM SCREENIO.

           MOVE "E  A010LOCAL-SPOOL." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U"
              GO TO RANGES-YN.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO ASK-LOCAL.

           IF VDUBUF NOT = "Y"
              IF VDUBUF NOT = "N"
                 GO TO ASK-LOCAL.

           MOVE VDUBUF TO LOCAL-PRINT-FG.

       EXIT-PARM.
           EXIT.

      ******************************************************************
       DISPLAY-PARAMETERS SECTION.
      ******************************************************************

           IF GROUP-FLAG = "Y"
              MOVE "S  A000BR1." TO VDU
              MOVE ENT-GROUP     TO VDUBUF
              PERFORM SCREENIO
              MOVE "S  A000BR2." TO VDU
              MOVE SPACES        TO VDUBUF
              PERFORM SCREENIO
           ELSE
              MOVE "SNNN040BR1." TO VDU
              MOVE BEG-BRANCH    TO VDUNUM0
              PERFORM SCREENIO
              MOVE "SNNN040BR2." TO VDU
              MOVE END-BRANCH    TO VDUNUM0
              PERFORM SCREENIO.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    MOVE "SNNN040BR1." TO VDU.
      *    MOVE BEG-BRANCH    TO VDUNUM0.
      *    PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    MOVE "SNNN040BR2." TO VDU.
      *    MOVE END-BRANCH    TO VDUNUM0.
      *    PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "SN N020CL1." TO VDU.
           MOVE BEG-CLASS     TO VDUNUM0.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "SN N020CL2." TO VDU.
           MOVE END-CLASS     TO VDUNUM0.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "SN N042DL1." TO VDU.
           MOVE BEG-DEALER    TO VDUNUM2.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "SN N042DL2." TO VDU.
           MOVE END-DEALER    TO VDUNUM2.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "SN N040AGE1." TO VDU.
           MOVE BEG-AGE        TO VDUNUM0.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "SN N040AGE2." TO VDU.
           MOVE END-AGE        TO VDUNUM0.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "SN A010C-POT." TO VDU.
           MOVE CPOT            TO VDUBUF.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "SN A010C-MISS." TO VDU.
           MOVE CMISS            TO VDUBUF.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "SN N040RAGE1." TO VDU.
           MOVE BEG-RAGE        TO VDUNUM0.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "SN N040RAGE2." TO VDU.
           MOVE END-RAGE        TO VDUNUM0.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "SN A010R-POT." TO VDU.
           MOVE RPOT            TO VDUBUF.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "SN N070BB1." TO VDU.
           MOVE BEG-BAL       TO VDUNUM0.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "SN N070BB2." TO VDU.
           MOVE END-BAL       TO VDUNUM0.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "SN N060LA1." TO VDU.
           MOVE BEG-LNAMT     TO VDUNUM0.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "SN N060LA2." TO VDU.
           MOVE END-LNAMT     TO VDUNUM0.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "S  A010TYPE." TO VDU.
           MOVE OWNED          TO VDUBUF.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "S  A010PL." TO VDU.
           MOVE PL           TO VDUBUF.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "S  A010PLTYPE." TO VDU.
           MOVE PLTYPE           TO VDUBUF.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "S  A010ACTIVE." TO VDU.
           MOVE PLACTIVE         TO VDUBUF.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "S  A010BRULE." TO VDU.
           MOVE BRULE TO VDUBUF.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "S  A010RC." TO VDU.
           MOVE RC           TO VDUBUF.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "S  A010JD." TO VDU.
           MOVE JD           TO VDUBUF.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "S  A010RP." TO VDU.
           MOVE REPO         TO VDUBUF.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "S  A010BK." TO VDU.
           MOVE BANK         TO VDUBUF.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "S  A010LG." TO VDU.
           MOVE LEGAL        TO VDUBUF.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "S  A010DA." TO VDU.
           MOVE DEBT         TO VDUBUF.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "S  A010ZBAL." TO VDU.
           MOVE ZBAL           TO VDUBUF.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "S  A010UEARN." TO VDU.
           MOVE UEARN           TO VDUBUF.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "S  A200DELPROMPT:P." TO VDU
           MOVE "BEG MO DELINQENCY?: "    TO VDUBUF
           PERFORM SCREENIO
           MOVE "S  A010DELFLAG." TO VDU
           MOVE DEL-FLAG TO VDUBUF.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "S  A010ALPSUM." TO VDU.
           MOVE ALPHA-SUMMARY    TO VDUBUF.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "S  A010DUESUM." TO VDU.
           MOVE DUEDATE-SUMMARY  TO VDUBUF.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "S  A010SUMONLY." TO VDU.
           MOVE SUM-ONLY          TO VDUBUF.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "S  A010RANGES." TO VDU.
           MOVE PRINT-RANGES     TO VDUBUF.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "S  A200SPOOL-PMPT:P." TO VDU.
           MOVE "LOCAL SPOOL   (Y/N):"    TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "S  A010LOCAL-SPOOL." TO VDU.
           MOVE LOCAL-PRINT-FG        TO VDUBUF.
           PERFORM SCREENIO.

      ******************************************************************
       DETAIL-LINE-PROCESS SECTION.
      ******************************************************************

           IF FIRST-TIME = "Y"
              MOVE EXT-CONAME-RPT-LINES TO LN-COUNT
              MOVE 1 TO LN-FEED
              MOVE "N" TO FIRST-TIME
              MOVE "Y" TO RECORDS-FOUND.

           IF INVALID-BW-FG = "Y"
              MOVE "?" TO D-BW-BAD
              MOVE "MISSING BORROWER" TO D-NAME
           ELSE
              MOVE NAME-WORKER TO D-NAME.
           PERFORM LPSTAT-DISPLAY.
           MOVE LPS-DISPLAY-LNNO TO HOLD-LPS-NUM.
           MOVE H-LPS-NUM        TO D-LOAN-NUM.
           MOVE LN-1STPYDA       TO D-DUEDAY.
           MOVE LN-DLNO          TO D-DLNO.

           IF LN-DATE-PAID-LAST NOT = 0
              MOVE LN-DATE-PAID-LAST TO DATE-YYYYMMDD
              PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
              MOVE DATE-MMDDYY TO ALP-DATE-EDIT
           ELSE
              MOVE LN-LOANDATE TO DATE-YYYYMMDD 
              PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
              MOVE DATE-MMDDYY  TO ALP-DATE-EDIT.

           MOVE LN-UNITPER-CD TO DATER-UNITPER-CD.
           MOVE LN-UNITPER-FREQ TO DATER-UNITPER-FREQ.
           PERFORM UNITPER-MASK.
           MOVE ALP-DATE-EDIT TO D-PAYDATE.
           IF LN-DATE-PAID-LAST = 0
              MOVE "." TO D-DASH1
              IF D-DASH2 = "/"
                 MOVE "." TO D-DASH2.

           IF RDPOT30 = "Y"
              MOVE "<P>" TO D-RECENCYX
           ELSE
           IF RDPOT30 = "M"
              MOVE "<M>" TO D-RECENCYX
           ELSE
           IF RDPOTCHGOFF = "Y"
              MOVE "<PC>" TO D-RECENCYX
           ELSE
           IF RDPOTCHGOFF = "M"
              MOVE "<PL>" TO D-RECENCYX
           ELSE
              MOVE RECDEL TO D-RECENCY.

           MOVE LN-RECPROFILE(1) TO D-PYRECPRO.
           INSPECT D-XPYRECPRO REPLACING ALL "9" BY "-".

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      * DETERMINE ACCOUNT STATUS (IB-PC):
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

           MOVE 0  TO HOLD-CURBAL.
           MOVE TRANS-DATE TO IBPC-DATE.
           PERFORM IBPC-TEST.

      *    IF BR-AGE-ERN = "T" OR SPACE
      *       MOVE LN-TOTERN       TO D-UNEARNED
      *    ELSE
              MOVE EARN-TOT-UNEARN TO D-UNEARNED.

           MOVE LN-CONPROFILE(1) TO D-PROFILE.
           INSPECT D-XPROFILE REPLACING ALL "9" BY "-".

           IF CPOT30 = "Y"
              MOVE "<P>" TO D-CONTUALX
           ELSE
           IF CPOT30 = "M"
              MOVE "<M>" TO D-CONTUALX
           ELSE
           IF CPOTCHGOFF = "Y"
              MOVE "<PC>" TO D-CONTUALX
           ELSE
           IF CPOTCHGOFF = "M"
              MOVE "<PL>" TO D-CONTUALX
           ELSE
              MOVE CONTRACTUAL TO D-CONTUAL.

           MOVE PDTH-DATE     TO D-PDTHRU.
           COMPUTE D-APR ROUNDED = LN-APRATE * 1.

           ADD LN-CURBAL TO HOLD-CURBAL.
           IF GP-INCLUDE-O2-IN-BAL = "Y"
              MOVE " PRIN + O2" TO HEAD6-PRIN
           ELSE
              MOVE " PRINCIPAL" TO HEAD6-PRIN
              SUBTRACT LN-OT2BAL FROM HOLD-CURBAL.

           IF LN-OTHBAL NOT = 0 OR LN-OT2BAL NOT = 0
              COMPUTE D-OTHBAL = LN-OTHBAL + LN-OT2BAL.

           IF LN-INTBAL NOT = 0 OR LN-LCBAL NOT = 0
              ADD LN-INTBAL LN-LCBAL GIVING D-INTBAL.

           IF HOLD-CURBAL NOT = 0
              MOVE HOLD-CURBAL TO D-CURBAL.

      *----------------------------------------------------------------
       DETAIL-LN-ACCTOT.
           PERFORM UPDATE-BY-DUEDAY.

           IF ALPHA-SUMMARY = "Y"
              PERFORM UPDATE-BY-ALPHA.

           MOVE 1 TO LEV.
           PERFORM DETAIL-LN-ACCTBL.
           MOVE 2 TO LEV.
           PERFORM DETAIL-LN-ACCTBL.
           MOVE 3 TO LEV.
           PERFORM DETAIL-LN-ACCTBL.

           IF NOT (SUM-ONLY = "Y" OR "G" OR "A" OR "C")
              PERFORM WRITE-DETAIL-LINE
           ELSE
              MOVE SPACES TO DETAIL-LINE.

           GO TO END-DETAIL-LN-PROCESS.

      *----------------------------------------------------------------
       DETAIL-LN-ACCTBL.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    ADDING TO TOTALS: CONTRACTUAL
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( PL = "O" ) OR ( LN-PLCD = " " )
              ADD 1                   TO T-NO    (LEV CSUB 1)
              ADD LN-OTHBAL           TO T-OTH   (LEV CSUB 1)
              ADD LN-OT2BAL           TO T-OTH2  (LEV CSUB 1)
              ADD LN-INTBAL LN-LCBAL  TO T-INT   (LEV CSUB 1)
              ADD HOLD-CURBAL         TO T-BAL   (LEV CSUB 1)
              ADD UNEARNED            TO T-UNEARN(LEV CSUB 1)
              ADD UNDISCOUNT          TO T-UNDISC(LEV CSUB 1)
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    ADDING TO TOTALS: RECENCY
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
              ADD 1                   TO T-NO    (LEV RDSUB 2)
              ADD LN-OTHBAL           TO T-OTH   (LEV RDSUB 2)
              ADD LN-OT2BAL           TO T-OTH2  (LEV RDSUB 2)
              ADD LN-INTBAL LN-LCBAL  TO T-INT   (LEV RDSUB 2)
              ADD HOLD-CURBAL         TO T-BAL   (LEV RDSUB 2)
              ADD UNEARNED            TO T-UNEARN(LEV RDSUB 2)
              ADD UNDISCOUNT          TO T-UNDISC(LEV RDSUB 2)

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    ADDING TO TOTALS: PAID AHEAD
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
              IF ( PSUB > ZEROES         ) AND
                 ( SP-BANKRULE NOT = "D" AND SP-BANKRULE NOT = "F")
                 ADD 1                  TO T-NO    (LEV PSUB 3)
                 ADD LN-OTHBAL          TO T-OTH   (LEV PSUB 3)
                 ADD LN-OT2BAL          TO T-OTH2  (LEV PSUB 3)
                 ADD LN-INTBAL LN-LCBAL TO T-INT   (LEV PSUB 3)
                 ADD HOLD-CURBAL        TO T-BAL   (LEV PSUB 3)
                 ADD UNEARNED           TO T-UNEARN(LEV PSUB 3)
                 ADD UNDISCOUNT         TO T-UNDISC(LEV PSUB 3).

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    ADDING TO TOTALS: REPO
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( LN-IS-REPO-AR ) AND
              ( LN-PLCD = SPACES  )
              ADD 1                   TO T-NO    (LEV RPSUB 4)
              ADD LN-OTHBAL           TO T-OTH   (LEV RPSUB 4)
              ADD LN-OT2BAL           TO T-OTH2  (LEV RPSUB 4)
              ADD LN-INTBAL LN-LCBAL  TO T-INT   (LEV RPSUB 4)
              ADD HOLD-CURBAL         TO T-BAL   (LEV RPSUB 4)
              ADD UNEARNED            TO T-UNEARN(LEV RPSUB 4)
              ADD UNDISCOUNT          TO T-UNDISC(LEV RPSUB 4).

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    ADDING TO TOTALS: P & L
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
              IF ( LN-PLCD NOT = SPACES)
                 ADD 1                   TO T-NO    (LEV PLSUB 5)
                 ADD LN-OTHBAL           TO T-OTH   (LEV PLSUB 5)
                 ADD LN-OT2BAL           TO T-OTH2  (LEV PLSUB 5)
                 ADD LN-INTBAL LN-LCBAL  TO T-INT   (LEV PLSUB 5)
                 ADD HOLD-CURBAL         TO T-BAL   (LEV PLSUB 5)
                 ADD UNEARNED            TO T-UNEARN(LEV PLSUB 5)
                 ADD UNDISCOUNT          TO T-UNDISC(LEV PLSUB 5).

      *----------------------------------------------------------------
       UPDATE-BY-DUEDAY.
           MOVE LN-1STPYDATE      TO NUM-DATE.
           MOVE NUM-DA            TO D-DUEDAY.

           IF NUM-DA < 1 OR NUM-DA > 31
              MOVE 32             TO NUM-DA.

           ADD 1                  TO DD-NO(NUM-DA).
           ADD LN-OTHBAL          TO DD-OTH(NUM-DA).
           ADD LN-OT2BAL          TO DD-OTH2(NUM-DA).
           ADD LN-INTBAL LN-LCBAL TO DD-INT(NUM-DA).
           ADD HOLD-CURBAL        TO DD-BAL(NUM-DA).
           ADD LN-REGPYAMT        TO DD-PAY(NUM-DA).

           IF DUEDATE-SUMMARY = "F"
              ADD 1               TO DUE-DATE-NO(1, NUM-DA)
                                     DUE-DATE-NO(2, NUM-DA).

      *----------------------------------------------------------------
       UPDATE-BY-ALPHA.
           MOVE BW-LNAME           TO NAME-TEST.

           IF NAME-TEST < "A" OR NAME-TEST > "Z"
              MOVE 27              TO NAME-SUB
           ELSE
              SUBTRACT NAME-A FROM NAME-B GIVING NAME-SUB.

           ADD 1                   TO NM-NO(NAME-SUB).
           ADD LN-OTHBAL           TO NM-OTH(NAME-SUB).
           ADD LN-OT2BAL           TO NM-OTH2(NAME-SUB).
           ADD LN-INTBAL LN-LCBAL  TO NM-INT(NAME-SUB).
           ADD HOLD-CURBAL         TO NM-BAL(NAME-SUB).
           ADD LN-REGPYAMT         TO NM-PAY(NAME-SUB).

       END-DETAIL-LN-PROCESS.
           EXIT.

      ******************************************************************
       PROCESS-BORROWER SECTION.
      ******************************************************************

           MOVE SPACES TO INVALID-BW-FG.
           MOVE LN-SSNO(1) TO BW-SSNO.
           PERFORM READ-BW1-FILE.
           IF IO-FG NOT = 0
               MOVE BEG-DATE TO BW-REFDATE
               MOVE "Y" TO INVALID-BW-FG
               GO TO END-PROCESS-BORROWER.

           MOVE BW-LNAME TO NAME-WORKER.
           MOVE BW-FNAME TO FIRST-NAME.
           INSPECT NAME-WORKER REPLACING FIRST "  " BY ",?".
           INSPECT NAME-WORKER REPLACING FIRST "                "
                   BY FIRST-NAME.
           INSPECT NAME-WORKER REPLACING ALL "?" BY " ".

       END-PROCESS-BORROWER.
           EXIT.

      ******************************************************************
       HEAD-PAGE SECTION.
      ******************************************************************

           ADD 1            TO PAGE-NUMBER.
           MOVE PAGE-NUMBER TO H-PAGE-NO.
           PERFORM GET-TIME.
           MOVE T-HH        TO H-HR.
           MOVE T-MM        TO H-MIN.
           MOVE HEAD1       TO PR-REC.
           MOVE 99          TO LN-FEED.
           PERFORM WRITE-PR-REC.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE HEAD2       TO PR-REC.
           MOVE 1           TO LN-FEED.
           PERFORM WRITE-PR-REC.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE HEAD3        TO PR-REC.
           PERFORM WRITE-PR-REC.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE HEAD4       TO PR-REC.
           PERFORM WRITE-PR-REC.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF SUMMARY-FG = "N"
              MOVE HEAD5  TO PR-REC
              PERFORM WRITE-PR-REC
              MOVE HEAD6  TO PR-REC
              PERFORM WRITE-PR-REC
              MOVE 7      TO LN-COUNT
              MOVE 2      TO LN-FEED
            ELSE
              MOVE 5      TO LN-COUNT
              MOVE 2      TO LN-FEED.

       END-HEAD-PAGE.
           EXIT.

      ******************************************************************
       WRITE-DETAIL-LINE SECTION.
      ******************************************************************
           ADD LN-FEED       TO LN-COUNT.

           IF LN-COUNT > EXT-CONAME-RPT-LINES
              PERFORM HEAD-PAGE.

           MOVE DETAIL-LINE  TO PR-REC.
           PERFORM WRITE-PR-REC.
           MOVE 1            TO LN-FEED.
           MOVE SPACES       TO DETAIL-LINE.

      ******************************************************************
       PRINT-RANGE-LINES SECTION.
      ******************************************************************

           MOVE "Y"                    TO SUMMARY-FG.
           MOVE EXT-CONAME-RPT-LINES   TO LN-COUNT.
           MOVE 4                      TO LN-FEED.
      *-------------------------------------------------------------
           IF DATE-TYPE = "L"
              MOVE "BEG LOAN DATE:" TO RANGE-LINE
              MOVE  BEG-DATE         TO RANGE-SELECTION
              PERFORM WRITE-DETAIL-LINE
              MOVE "END LOAN DATE:" TO RANGE-LINE
              MOVE  END-DATE         TO RANGE-SELECTION
              PERFORM WRITE-DETAIL-LINE
           ELSE
           IF DATE-TYPE = "E"
              MOVE "BEG ENTRY DATE:" TO RANGE-LINE
              MOVE  BEG-DATE         TO RANGE-SELECTION
              PERFORM WRITE-DETAIL-LINE
              MOVE "END ENTRY DATE:" TO RANGE-LINE
              MOVE  END-DATE         TO RANGE-SELECTION
              PERFORM WRITE-DETAIL-LINE
           ELSE
           IF DATE-TYPE = "B"
              MOVE "BEG BW REFDATE:" TO RANGE-LINE
              MOVE  BEG-DATE         TO RANGE-SELECTION
              PERFORM WRITE-DETAIL-LINE
              MOVE "END BW REFDATE:" TO RANGE-LINE
              MOVE  END-DATE         TO RANGE-SELECTION
              PERFORM WRITE-DETAIL-LINE.

      *--------------------------------------------------------------
           IF GROUP-FLAG = "Y"
              MOVE "BEGINNING BRANCH   :" TO RANGE-LINE
              MOVE ENT-GROUP              TO RANGE-SELECTION
              PERFORM WRITE-DETAIL-LINE
              MOVE "ENDING             :" TO RANGE-LINE
              MOVE SPACES                 TO RANGE-SELECTION
           ELSE
              MOVE "BEGINNING BRANCH   :" TO RANGE-LINE
              MOVE BEG-BRANCH             TO RANGE-SELECTION
              PERFORM WRITE-DETAIL-LINE
              MOVE "ENDING             :" TO RANGE-LINE
              MOVE END-BRANCH             TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "BEGINNING CLASS    :" TO RANGE-LINE.
           MOVE BEG-CLASS              TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "ENDING             :" TO RANGE-LINE.
           MOVE END-CLASS              TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "BEGINNING DEALER   :" TO RANGE-LINE.
           MOVE BEG-DEALER             TO RANGE-DEALER.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "ENDING             :" TO RANGE-LINE.
           MOVE END-DEALER             TO RANGE-DEALER.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "CONTRACTUAL AGE    :" TO RANGE-LINE.
           MOVE BEG-AGE                TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "ENDING             :" TO RANGE-LINE.
           MOVE END-AGE                TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "POTENTIALS         :" TO RANGE-LINE.
           MOVE CPOT                   TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "MISS PAYS          :" TO RANGE-LINE.
           MOVE CMISS                  TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "RECENCY AGE        :" TO RANGE-LINE.
           MOVE BEG-RAGE               TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "ENDING             :" TO RANGE-LINE.
           MOVE END-RAGE               TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "POTENTIALS          :" TO RANGE-LINE.
           MOVE RPOT                    TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "BEGINNING BALANCE  :" TO RANGE-LINE.
           MOVE BEG-BAL                TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "ENDING             :" TO RANGE-LINE.
           MOVE END-BAL                TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "BEGINNING LOAN AMT :" TO RANGE-LINE.
           MOVE BEG-LNAMT              TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "ENDING             :" TO RANGE-LINE.
           MOVE END-LNAMT              TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "OWNED,SERVICE,BOTH :" TO RANGE-LINE.
           MOVE OWNED                  TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "P & L              :" TO RANGE-LINE.
           MOVE PL                     TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "P & L TYPE         :" TO RANGE-LINE.
           MOVE PLTYPE                 TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "ACTIVE             :" TO RANGE-LINE.
           MOVE PLACTIVE               TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "BANK RULE  (I/T)   :" TO RANGE-LINE.
           MOVE BRULE                  TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "RECOURSE           :" TO RANGE-LINE.
           MOVE RC                     TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "JUDGEMENTS         :" TO RANGE-LINE.
           MOVE JD                     TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "REPO               :" TO RANGE-LINE.
           MOVE REPO                   TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "BANKRUPT           :" TO RANGE-LINE.
           MOVE BANK                   TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "LEGAL              :" TO RANGE-LINE.
           MOVE LEGAL                  TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "DEBT-AID           :" TO RANGE-LINE.
           MOVE DEBT                   TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "ZERO BALANCES      :" TO RANGE-LINE.
           MOVE ZBAL                   TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "ZERO UNEARNED      :" TO RANGE-LINE.
           MOVE UEARN                  TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "DUEDATE SUMMARY    :" TO RANGE-LINE.
           MOVE DUEDATE-SUMMARY        TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "ALPHA SUMMARY      :" TO RANGE-LINE.
           MOVE ALPHA-SUMMARY          TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "SUMMARY ONLY       :" TO RANGE-LINE.
           MOVE SUM-ONLY               TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "PRINT RANGES       :" TO RANGE-LINE.
           MOVE PRINT-RANGES           TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.


      ******************************************************************
       CLASS-TOTALS SECTION.
      ******************************************************************
           IF NOT (SUM-ONLY = "G" OR "A" OR "C")
              IF MORE-THAN-1-CLASS = "Y"
                 MOVE "Y"             TO SUMMARY-FG
                 MOVE EXT-CONAME-RPT-LINES TO LN-COUNT
                 MOVE 1               TO LN-FEED
                 MOVE "CLASS TOTALS:" TO S-SUMNAME
                 PERFORM WRITE-DETAIL-LINE
                 MOVE 1               TO LEV
                 PERFORM PRINT-AGEING-SUMMARY.

           PERFORM UPDATE-WK-FILE.
           MOVE ZEROS            TO TOTAL-TBL(1).
           MOVE "N"              TO SUMMARY-FG.
           MOVE EXT-CONAME-RPT-LINES TO LN-COUNT.
           MOVE 1                TO LN-FEED.

      ******************************************************************
       UPDATE-WK-FILE SECTION.
      ******************************************************************
           MOVE HOLD-LN-CLASS TO WK-CLASS.
           PERFORM READ-WK-FILE.
           IF IO-FG = 9
              MOVE HOLD-LN-CLASS TO WK-CLASS
              MOVE ZEROS TO WK-TOTAL-TBL
              PERFORM WRITE-WK-FILE.

           PERFORM VARYING SUB2 FROM 1 BY 1 UNTIL SUB2 > 5
                      AFTER SUB1 FROM 1 BY 1 UNTIL SUB1 > 10
               ADD T-NO    (1 SUB1 SUB2) TO WK-NO    (SUB1 SUB2)
               ADD T-BAL   (1 SUB1 SUB2) TO WK-BAL   (SUB1 SUB2)
               ADD T-INT   (1 SUB1 SUB2) TO WK-INT   (SUB1 SUB2)
               ADD T-OTH   (1 SUB1 SUB2) TO WK-OTH   (SUB1 SUB2)
               ADD T-OTH2  (1 SUB1 SUB2) TO WK-OTH2  (SUB1 SUB2)
               ADD T-UNEARN(1 SUB1 SUB2) TO WK-UNEARN(SUB1 SUB2)
               ADD T-UNDISC(1 SUB1 SUB2) TO WK-UNDISC(SUB1 SUB2)
           END-PERFORM.

           PERFORM REWRITE-WK-FILE.

      ******************************************************************
       BRANCH-TOTALS SECTION.
      ******************************************************************

           IF NOT (SUM-ONLY = "G" OR "A" OR "C")
              MOVE "Y"                      TO SUMMARY-FG
              MOVE SPACES                   TO H-LCNOX
              MOVE "**** ALL CLASSES ****"  TO H-LCDESC
              IF NOT (BEG-CLASS = 0 AND END-CLASS = 99)
                 MOVE BEG-CLASS TO H-LCBEG
                 MOVE " - " TO H-LCDASH
                 MOVE END-CLASS TO H-LCEND
              END-IF
              MOVE EXT-CONAME-RPT-LINES     TO LN-COUNT
              MOVE 1                        TO LN-FEED
              MOVE "BRANCH TOTALS:"         TO S-SUMNAME
              PERFORM WRITE-DETAIL-LINE
              MOVE 2                        TO LEV
              PERFORM PRINT-AGEING-SUMMARY.

           MOVE ZEROS                    TO TOTAL-TBL(2).
           MOVE "N"                      TO SUMMARY-FG.
           MOVE EXT-CONAME-RPT-LINES     TO LN-COUNT.
           MOVE 1                        TO LN-FEED.

      ******************************************************************
       UPDATE-DUE-DATE-FILE SECTION.
      ******************************************************************
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 32
              IF DUE-DATE-NO(LEV, SUB) > 0
                 IF LEV = 1
                    MOVE BR-NO              TO DUE-BRNO
                 ELSE
                    MOVE 9999               TO DUE-BRNO
                 END-IF
                 MOVE SUB                   TO DUE-DAY
                 MOVE DUE-DATE-NO(LEV, SUB) TO DUE-NO-LOANS
                 MOVE X"0D"                 TO DUE-CR
                 MOVE X"0A"                 TO DUE-LF
                 ADD 1 TO DUE-KEY
                 PERFORM WRITE-DUE-FILE
                 IF IO-BAD
                    GO TO IO-ERROR
                 END-IF
              END-IF
           END-PERFORM.

      ******************************************************************
       COMPANY-TOTALS SECTION.
      ******************************************************************

           IF BEG-BRANCH = END-BRANCH
              GO TO SKIP-COMPANY-TOTALS.

           IF MORE-THAN-1-CLASS-ON-REPORT = "N"
              GO TO SKIP-COMPANY-CLASS-SUMMARY.

      * TEST TO SKIP CLASS / BRANCH SUMMARY
           IF SUM-ONLY = "A"
              GO TO SKIP-COMPANY-CLASS-SUMMARY.

           MOVE 0 TO WK-CLASS.
           PERFORM START-WK-FILE.
       COMPANY-TOTALS-CLASS-NEXT.
           PERFORM READ-WK-FILE-NEXT.
           IF IO-FG = 9
              GO TO SKIP-COMPANY-CLASS-SUMMARY.

           MOVE WK-CLASS TO LC-CODE
                            H-LCNO.
           PERFORM READ-LC1-FILE.
           IF IO-FG NOT = 0
              MOVE "INVALID" TO LC-DESC.
           MOVE LC-DESC TO H-LCDESC.
           MOVE SPACES TO H-LCBEG H-LCDASH H-LCEND.

           MOVE "Y"             TO SUMMARY-FG.
           MOVE EXT-CONAME-RPT-LINES TO LN-COUNT.
           MOVE 1               TO LN-FEED.
           MOVE SPACES                    TO H-BRNOX.
           MOVE "**** ALL BRANCHES ****"  TO H-BRDESC.
           MOVE "COMPANY CLASS TOTALS:" TO S-SUMNAME.
           PERFORM WRITE-DETAIL-LINE.

           MOVE WK-TOTAL-TBL TO TOTAL-TBL(1).
           MOVE 1               TO LEV.
           PERFORM PRINT-AGEING-SUMMARY.
           GO TO COMPANY-TOTALS-CLASS-NEXT.


       SKIP-COMPANY-CLASS-SUMMARY.
      *    -------------------------------------------------------------
      *    TEST TO SKIP BRANCH/CLASS GRAND TOTALS
      *    -------------------------------------------------------------
           IF SUM-ONLY = "C"
              GO TO SKIP-COMPANY-TOTALS.

           MOVE "Y"                       TO SUMMARY-FG.
           MOVE SPACES                    TO H-LCNOX.
           MOVE "**** ALL CLASSES ****"   TO H-LCDESC.
           IF NOT (BEG-CLASS = 0 AND END-CLASS = 99)
              MOVE BEG-CLASS TO H-LCBEG
              MOVE " - " TO H-LCDASH
              MOVE END-CLASS TO H-LCEND.
           MOVE SPACES                    TO H-BRNOX.
           MOVE "**** ALL BRANCHES ****"  TO H-BRDESC.
           MOVE EXT-CONAME-RPT-LINES      TO LN-COUNT.
           MOVE 1                         TO LN-FEED.
           MOVE "COMPANY TOTALS:"         TO S-SUMNAME.
           PERFORM WRITE-DETAIL-LINE.
           MOVE 3                         TO LEV.
           PERFORM PRINT-AGEING-SUMMARY.

       SKIP-COMPANY-TOTALS.
           IF DUEDATE-SUMMARY = "Y" OR "F"
              PERFORM RESET-TOT-PAGE
              MOVE "Y"                       TO SUMMARY-FG
              MOVE SPACES                    TO H-LCNOX
              MOVE "**** ALL CLASSES ****"   TO H-LCDESC
              IF NOT (BEG-CLASS = 0 AND END-CLASS = 99)
                 MOVE BEG-CLASS TO H-LCBEG
                 MOVE " - " TO H-LCDASH
                 MOVE END-CLASS TO H-LCEND
              END-IF
              MOVE SPACES                    TO H-BRNOX
              MOVE "**** ALL BRANCHES ****"  TO H-BRDESC
              MOVE EXT-CONAME-RPT-LINES      TO LN-COUNT
              MOVE 1                         TO LN-FEED
              MOVE "********* BY PAYMENT DUE DATE *********"
                                             TO S-AGETYPE
              MOVE "N"                       TO NAME-PRINT
              MOVE 32                        TO NAME-SUB
              PERFORM PRINT-BYDUEDATE-SUMMARY.

           IF ALPHA-SUMMARY = "Y"
              PERFORM RESET-TOT-PAGE
              PERFORM MOVE-NM-TO-DD
              MOVE "Y"                       TO SUMMARY-FG
              MOVE SPACES                    TO H-LCNOX
              MOVE "**** ALL CLASSES ****"   TO H-LCDESC
              IF NOT (BEG-CLASS = 0 AND END-CLASS = 99)
                 MOVE BEG-CLASS TO H-LCBEG
                 MOVE " - " TO H-LCDASH
                 MOVE END-CLASS TO H-LCEND
              END-IF
              MOVE SPACES                    TO H-BRNOX
              MOVE "**** ALL BRANCHES ****"  TO H-BRDESC
              MOVE EXT-CONAME-RPT-LINES      TO LN-COUNT
              MOVE 1                         TO LN-FEED
              MOVE "****** A L P H A   S U M M A R Y ******"
                                          TO S-AGETYPE
              MOVE "Y"                    TO NAME-PRINT
              MOVE 27                     TO NAME-SUB
              PERFORM PRINT-BYDUEDATE-SUMMARY.

           IF DUEDATE-SUMMARY = "F"
              IF BEG-BRANCH NOT = END-BRANCH
                 MOVE 2 TO LEV
                 PERFORM UPDATE-DUE-DATE-FILE.

      ******************************************************************
       PRINT-AGEING-SUMMARY SECTION.
      ******************************************************************

           PERFORM ZERO-TT-TOTALS.

           PERFORM ACCUM-AGEING-TOTALS-NM
                   VARYING AGE FROM 1 BY 1 UNTIL AGE > 31.
           PERFORM ACCUM-AGEING-TOTALS
                   VARYING AGE FROM 1 BY 1 UNTIL AGE > 10
                     AFTER TYP FROM 1 BY 1 UNTIL TYP > 5.


           PERFORM CONTRACT-PRINT.

           IF ( REPO NOT = "S" ) AND
              ( PL   NOT = "O" )
              PERFORM REPO-PRINT.

           IF (  PL = "O" ) OR
              (( PL = "I" ) AND ( REPO NOT = "O" ))
              PERFORM PL-PRINT.

       EXIT-PRINT-AGEING-SUMMARY.
           EXIT.

      ******************************************************************
       CONTRACT-PRINT SECTION.
      ******************************************************************
           MOVE 1         TO TYP.
           MOVE "********* CONTRACTUAL DELINQUENCY *********"
                          TO S-AGETYPE.
           MOVE 1         TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.
           MOVE SUMMARY1  TO DETAIL-LINE.
           MOVE 2         TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.
           MOVE 2         TO LN-FEED.
           PERFORM CLEAR-SUM-SUB-TOTALS.
           PERFORM VARYING AGE FROM 1 BY 1 UNTIL AGE > 10
              PERFORM PRINT-DETAIL-SUM
              IF TYP = 1 OR 2
                 PERFORM ACCUM-SUM-SUB-TOTALS
              END-IF
              IF TYP = 1 OR 3
                 IF AGE = 7 OR 9
                    PERFORM PRINT-SUM-SUB-TOTALS
                 END-IF
              END-IF
           END-PERFORM.
           MOVE 2         TO LN-FEED.
           PERFORM PRINT-SUM-TOTALS.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    FOR RECENCY DELINQUENCY PRINTING
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "  ********* RECENCY DELINQUENCY *********"
                          TO S-AGETYPE.
           MOVE 2         TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.
           MOVE SUMMARY1  TO DETAIL-LINE.
           MOVE 2         TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.
           MOVE 2         TO LN-FEED.

           PERFORM CLEAR-SUM-SUB-TOTALS.
           PERFORM VARYING AGE FROM 1 BY 1 UNTIL AGE > 10
                     AFTER TYP FROM 2 BY 1 UNTIL TYP > 3
              PERFORM PRINT-DETAIL-SUM
              IF TYP = 1 OR 2
                 PERFORM ACCUM-SUM-SUB-TOTALS
              END-IF
              IF TYP = 1 OR 3
                 IF AGE = 7 OR 9
                    PERFORM PRINT-SUM-SUB-TOTALS
                 END-IF
              END-IF
           END-PERFORM.

           MOVE 2         TO TYP.
           MOVE 2         TO LN-FEED.
           PERFORM PRINT-SUM-TOTALS.
           MOVE 3         TO TYP.
           MOVE 1         TO LN-FEED.
           PERFORM PRINT-SUM-TOTALS.

       END-CONTRACT-PRINT.
           EXIT.

      ******************************************************************
       REPO-PRINT SECTION.
      ******************************************************************
           MOVE "  ************* REPOSSESSION ************"
                          TO S-AGETYPE.
           MOVE 2         TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.
           MOVE SUMMARY1  TO DETAIL-LINE.
           MOVE 2         TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.
           MOVE 2         TO LN-FEED.

           PERFORM CLEAR-SUM-SUB-TOTALS.
           MOVE 4         TO TYP.
           PERFORM VARYING AGE FROM 1 BY 1 UNTIL AGE > 2
                   PERFORM PRINT-DETAIL-SUM
           END-PERFORM.

           MOVE 4         TO TYP.
           MOVE 2         TO LN-FEED.
           PERFORM PRINT-SUM-TOTALS.
           MOVE 1         TO LN-FEED.

       END-REPO-PRINT.
           EXIT.

      ******************************************************************
       PL-PRINT SECTION.
      ******************************************************************
           MOVE "  **************** P & L ****************"
                          TO S-AGETYPE.
           MOVE 2         TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.
           MOVE SUMMARY1  TO DETAIL-LINE.
           MOVE 2         TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.
           MOVE 2         TO LN-FEED.

           PERFORM CLEAR-SUM-SUB-TOTALS.
           MOVE 5         TO TYP.
           PERFORM VARYING AGE FROM 1 BY 1 UNTIL AGE > 5
                   PERFORM PRINT-DETAIL-SUM
           END-PERFORM.

           MOVE 5         TO TYP.
           MOVE 2         TO LN-FEED.
           PERFORM PRINT-SUM-TOTALS.
           MOVE 1         TO LN-FEED.

       END-PL-PRINT.
           EXIT.

      ******************************************************************
       PRINT-DETAIL-SUM SECTION.
      ******************************************************************

           IF T-NO(LEV AGE TYP) = 0
              IF T-BAL(LEV AGE TYP) = 0
                 IF T-INT(LEV AGE TYP) = 0
                    IF T-OTH(LEV AGE TYP) = 0
                      IF T-OTH2(LEV AGE TYP) = 0
                       IF T-UNEARN(LEV AGE TYP) = 0
                           IF T-UNDISC(LEV AGE TYP) = 0
                              GO TO END-PRINT-DETAIL-SUM.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF TYP = 3
              MOVE "  PAID-AHEAD" TO S-NAME
           ELSE
           IF TYP = 4
              MOVE REPO-DESC(AGE) TO S-NAME
           ELSE
           IF TYP = 5
              MOVE PL-DESC(AGE) TO S-NAME
           ELSE
              MOVE AGE-DESC(AGE)  TO S-NAME.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF T-NO(LEV AGE TYP) NOT = 0
              MOVE T-NO(LEV AGE TYP) TO S-NO
              IF TYP NOT = 3
                 IF TT-NO(TYP) NOT = 0
                    COMPUTE S-NO-P ROUNDED =
                            ( T-NO(LEV AGE TYP) / TT-NO(TYP) ) * 100
                 ELSE
                    MOVE 0 TO S-NO-P.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF T-BAL(LEV AGE TYP) NOT = 0
              MOVE T-BAL(LEV AGE TYP) TO S-BAL
              IF TYP NOT = 3
                 IF TT-BAL(TYP) NOT = 0
                    COMPUTE S-BAL-P ROUNDED =
                            ( T-BAL(LEV AGE TYP) / TT-BAL(TYP) ) * 100
                 ELSE
                    MOVE 0 TO S-BAL-P.

      * INT/LC DUE  %
           IF T-INT(LEV AGE TYP) NOT = 0
              MOVE T-INT(LEV AGE TYP) TO S-INT
              IF TYP NOT = 3
                 IF TT-INT(TYP) NOT = 0
                    COMPUTE S-INT-P ROUNDED =
                            ( T-INT(LEV AGE TYP) / TT-INT(TYP) ) * 100
                 ELSE
                    MOVE 0 TO S-INT-P.

      * OTHER DUE
           IF T-OTH(LEV AGE TYP) NOT = 0
              MOVE T-OTH(LEV AGE TYP) TO S-OTH.

      * OTHER2 DUE
           IF T-OTH2(LEV AGE TYP) NOT = 0
              MOVE T-OTH2(LEV AGE TYP) TO S-OTH2.

      * UNEARNED   %
           IF T-UNEARN(LEV AGE TYP) NOT = 0
              MOVE T-UNEARN(LEV AGE TYP) TO S-UNEARN
              IF TYP NOT = 3
                 IF TT-UNEARN(TYP) NOT = 0
                    COMPUTE S-UNEARN-P ROUNDED =
                     ( T-UNEARN(LEV AGE TYP) / TT-UNEARN(TYP) ) * 100
                 ELSE
                    MOVE 0 TO S-UNEARN-P.

      * UNEARNED DISC  %
           IF T-UNDISC(LEV AGE TYP) NOT = 0
              MOVE T-UNDISC(LEV AGE TYP) TO S-UNDISC
              IF TYP NOT = 3
                 IF TT-UNDISC(TYP) NOT = 0
                    COMPUTE S-UNDISC-P ROUNDED =
                       (T-UNDISC(LEV AGE TYP) / TT-UNDISC(TYP)) * 100
                 ELSE
                    MOVE 0 TO S-UNDISC-P.

           PERFORM WRITE-DETAIL-LINE.

       END-PRINT-DETAIL-SUM.
           EXIT.

      ******************************************************************
      *  PRINT SUB TOTALS FOR DELINQUENCY (30,60,90,120,150,180,210+)
      *                       SLOW  (ABOVE DELINQUENCY, <M>, <P>)
      ******************************************************************
       PRINT-SUM-SUB-TOTALS SECTION.
      ******************************************************************
           IF NOT ( AGE = 7 OR 9 )
              GO TO EXIT-PRINT-SUM-SUB-TOTALS.

      * DO NOT PRINT DELINQ TOTALS IF THERE ARE NO DELINQ ACCTS
           IF AGE = 7
              IF SUB-TT-NO(1)  = 0 AND SUB-TT-BAL(1) = 0 AND
                 SUB-TT-INT(1) = 0 AND SUB-TT-OTH(1) = 0 AND
                 SUB-TT-UNEARN(1) = 0 AND SUB-TT-INT(1) = 0 AND
                 SUB-TT-UNDISC(1) = 0 AND SUB-TT-OTH2(1) = 0
                    GO TO EXIT-PRINT-SUM-SUB-TOTALS
              ELSE
                 MOVE 1 TO SUB1
                 MOVE "   TOTAL DEL" TO S-NAME.

      * DO NOT PRINT SLOW TOTALS IF THERE ARE NO <M> OR <P> ACCTS
      *    (IE. SLOW TOTALS = DELINQ TOTALS)
           IF AGE = 9
              IF SUB-TT-TYPETBL(2) = SUB-TT-TYPETBL(1)
                    GO TO EXIT-PRINT-SUM-SUB-TOTALS
              ELSE
                 MOVE 2 TO SUB1
                 MOVE "  TOTAL SLOW" TO S-NAME.

      * WHEN WE GET HERE TYP = 1 (CONTRACTUAL) OR 3 (PAID-AHEAD)
      * BUT WE REALLY WANT TO TOTAL TYP = 1 (CONTRACTUAL)
      * OR TYP = 2 (RECENCY)
           IF TYP = 3
              MOVE 2 TO SUB2
           ELSE
              MOVE 1 TO SUB2.

      
           IF SUB-TT-NO(SUB1) NOT = 0
              MOVE SUB-TT-NO(SUB1)  TO S-NO
              COMPUTE S-NO-P ROUNDED =
                      ( SUB-TT-NO(SUB1) / TT-NO(SUB2) ) * 100.
      
           IF SUB-TT-BAL(SUB1) NOT = 0
              MOVE SUB-TT-BAL(SUB1) TO S-BAL
              COMPUTE S-BAL-P ROUNDED =
                      ( SUB-TT-BAL(SUB1) / TT-BAL(SUB2) ) * 100.
     
           IF SUB-TT-INT(SUB1) NOT = 0
              MOVE SUB-TT-INT(SUB1) TO S-INT
              COMPUTE S-INT-P ROUNDED =
                      ( SUB-TT-INT(SUB1) / TT-INT(SUB2) ) * 100.
    
           IF SUB-TT-OTH(SUB1) NOT = 0
              MOVE SUB-TT-OTH(SUB1) TO S-OTH.
           IF SUB-TT-OTH2(SUB1) NOT = 0
              MOVE SUB-TT-OTH2(SUB1) TO S-OTH2.
   
           IF SUB-TT-UNEARN(SUB1) NOT = 0
              MOVE SUB-TT-UNEARN(SUB1) TO S-UNEARN
              COMPUTE S-UNEARN-P ROUNDED =
                ( SUB-TT-UNEARN(SUB1) / TT-UNEARN(SUB2) ) * 100.
  
           IF SUB-TT-UNDISC(SUB1) NOT = 0
              MOVE SUB-TT-UNDISC(SUB1) TO S-UNDISC
              COMPUTE S-UNDISC-P ROUNDED =
                (SUB-TT-UNDISC(SUB1) / TT-UNDISC(SUB2)) * 100.

           PERFORM WRITE-DETAIL-LINE.

      * SLOW TOTALS NEED TO INCLUDE DELINQUENCY
      *   ONLY CLEAR AFTER SLOW TOTALS ARE PRINTED
           IF AGE = 9
              PERFORM CLEAR-SUM-SUB-TOTALS.

       EXIT-PRINT-SUM-SUB-TOTALS.
           EXIT.

      *********************************************
       CLEAR-SUM-SUB-TOTALS SECTION.
      *********************************************

      * CLEAR DEL TOTALS
           MOVE 0 TO SUB-TT-NO(1)  SUB-TT-BAL(1) SUB-TT-INT(1)
                     SUB-TT-OTH(1) SUB-TT-UNEARN(1)  SUB-TT-OTH2(1)
                     SUB-TT-UNDISC(1).

      * CLEAR SLOW TOTALS
           MOVE 0 TO SUB-TT-NO(2)  SUB-TT-BAL(2) SUB-TT-INT(2)
                     SUB-TT-OTH(2) SUB-TT-UNEARN(2) SUB-TT-OTH2(2)
                     SUB-TT-UNDISC(2).

       EXIT-CLEAR-SUM-SUB-TOTALS.
           EXIT.

      *************************************************
       ACCUM-SUM-SUB-TOTALS SECTION.
      *************************************************

      * INCREMENT SLOW TOTALS (30/60/90/120/150/180/210+/<M>/<P>)
           ADD  T-NO(LEV AGE TYP) TO SUB-TT-NO(2).
           ADD T-BAL(LEV AGE TYP) TO SUB-TT-BAL(2).
           ADD T-INT(LEV AGE TYP) TO SUB-TT-INT(2).
           ADD T-OTH(LEV AGE TYP) TO SUB-TT-OTH(2).
           ADD T-OTH2(LEV AGE TYP) TO SUB-TT-OTH2(2).
           ADD T-UNEARN(LEV AGE TYP) TO SUB-TT-UNEARN(2).
           ADD T-UNDISC(LEV AGE TYP) TO SUB-TT-UNDISC(2).

      * INCREMENT DEL TOTALS (30/60/90/120/150/180/210+)
           IF AGE < 8
              ADD  T-NO(LEV AGE TYP) TO SUB-TT-NO(1)
              ADD T-BAL(LEV AGE TYP) TO SUB-TT-BAL(1)
              ADD T-INT(LEV AGE TYP) TO SUB-TT-INT(1)
              ADD T-OTH(LEV AGE TYP) TO SUB-TT-OTH(1)
              ADD T-OTH2(LEV AGE TYP) TO SUB-TT-OTH2(1)
              ADD T-UNEARN(LEV AGE TYP) TO SUB-TT-UNEARN(1)
              ADD T-UNDISC(LEV AGE TYP) TO SUB-TT-UNDISC(1).

       EXIT-ACCUM-SUM-SUB-TOTALS.
           EXIT.

      ******************************************************************
       PRINT-BYDUEDATE-SUMMARY SECTION.
      ******************************************************************

           PERFORM ZERO-TT-TOTALS.
           PERFORM ACCUM-BYDUEDATE-TOTALS
                   VARYING LEV FROM 1 BY 1 UNTIL LEV > NAME-SUB.
           MOVE 1         TO LEV.
           MOVE 2         TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.
           MOVE SUMMARY2  TO DETAIL-LINE.
           MOVE 2         TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.
           MOVE 2         TO LN-FEED.
           PERFORM PRINT-BYDUEDATE-SUM VARYING LEV
                   FROM 1 BY 1 UNTIL LEV > NAME-SUB.
           MOVE 2         TO LN-FEED.
           PERFORM PRINT-DUE-TOTALS.

      ******************************************************************
       PRINT-BYDUEDATE-SUM SECTION.
      ******************************************************************

           IF DD-NO(LEV) = 0
              IF DD-BAL(LEV) = 0
                 IF DD-INT(LEV) = 0
                    IF DD-OTH(LEV) = 0
                      IF DD-OTH2(LEV) = 0
                        IF DD-PAY(LEV) = 0
                          GO TO END-PRINT-BYDUEDATE-SUM.

           IF NAME-PRINT = "N"
              MOVE LEV    TO NUM-DA
              MOVE NUM-DA TO S-NAMEDUE
           ELSE
              ADD LEV NAME-A GIVING NAME-B
              MOVE NAME-TEST TO S-LASTINITIAL.

           MOVE ":" TO S-NAMECOLON.

           IF DD-NO(LEV) NOT = 0
              MOVE DD-NO(LEV) TO S-NO
              IF TT-NO(1) NOT = 0
                 COMPUTE S-NO-P ROUNDED =
                                ( DD-NO(LEV) / TT-NO(1) ) * 100
              ELSE
                 MOVE 0 TO S-NO-P.

            IF DD-BAL(LEV) NOT = 0
              MOVE DD-BAL(LEV) TO S-BAL
              IF TT-BAL(1) NOT = 0
                 COMPUTE S-BAL-P ROUNDED =
                                 ( DD-BAL(LEV) / TT-BAL(1) ) * 100
              ELSE
                 MOVE 0 TO S-BAL-P.

           IF DD-INT(LEV) NOT = 0
              MOVE DD-INT(LEV) TO S-INT
              IF TT-INT(1) NOT = 0
                 COMPUTE S-INT-P ROUNDED =
                                 ( DD-INT(LEV) / TT-INT(1) ) * 100
              ELSE
                 MOVE 0 TO S-INT-P.

           IF DD-OTH(LEV) NOT = 0
              MOVE DD-OTH(LEV) TO S-OTH.

           IF DD-OTH2(LEV) NOT = 0
              MOVE DD-OTH2(LEV) TO S-OTH2.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    ACTUALLY HOLDS REGPYAMT SUMMATION
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

           IF DD-PAY(LEV) NOT = 0
              MOVE DD-PAY(LEV) TO S-UNEARN
              IF TT-UNEARN(1) NOT = 0
                 COMPUTE S-UNEARN-P ROUNDED =
                           ( DD-PAY(LEV) / TT-UNEARN(1) ) * 100
              ELSE
                 MOVE 0 TO S-UNEARN-P.

           PERFORM WRITE-DETAIL-LINE.

       END-PRINT-BYDUEDATE-SUM.
           EXIT.

      ******************************************************************
       ZERO-TT-TOTALS SECTION.
      ******************************************************************

           MOVE 0 TO TT-NM-NO  TT-NM-BAL  TT-NM-INT
                     TT-NM-OTH TT-NM-OTH2 TT-NM-UNEARN
                     TT-NM-UNDISC.

           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 5
              MOVE 0 TO TT-NO(SUB) TT-BAL(SUB) TT-INT(SUB)
                        TT-OTH(SUB) TT-OTH2(SUB) TT-UNEARN(SUB)
                        TT-UNDISC(SUB)
           END-PERFORM.

       END-ZERO-TT-TOTALS.
           EXIT.

      ******************************************************************
       ZERO-DD-TOTALS SECTION.
      ******************************************************************

           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 32
              MOVE 0 TO DD-NO(SUB) DD-BAL(SUB) DD-INT(SUB)
                        DD-OTH(SUB) DD-OTH2(SUB) DD-PAY(SUB)
           END-PERFORM.
       END-ZERO-DD-TOTALS.
           EXIT.

      ******************************************************************
       ZERO-NM-TOTALS SECTION.
      ******************************************************************

           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 27
              MOVE 0 TO NM-NO(SUB) NM-BAL(SUB) NM-INT(SUB)
                        NM-OTH(SUB) NM-OTH2(SUB) NM-PAY(SUB)
           END-PERFORM.

       END-ZERO-NM-TOTALS.
           EXIT.

      ******************************************************************
       MOVE-NM-TO-DD SECTION.
      ******************************************************************

           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 27
              MOVE NM-NO(SUB)  TO DD-NO(SUB)
              MOVE NM-BAL(SUB) TO DD-BAL(SUB)
              MOVE NM-INT(SUB) TO DD-INT(SUB)
              MOVE NM-OTH(SUB) TO DD-OTH(SUB)
              MOVE NM-OTH2(SUB) TO DD-OTH2(SUB)
              MOVE NM-PAY(SUB) TO DD-PAY(SUB)
           END-PERFORM.

       END-MOVE-NM-TOTALS.
           EXIT.

      *****************************************************************
       CLASS-SCAN SECTION.
      *****************************************************************
           MOVE 0 TO LCSCAN-KEY.
           MOVE " " TO LCSCAN-VERIFY.
           MOVE LCSCAN-BUF TO SCAN-BUF.
           PERFORM CALL-WINDOW-PROGRAM.
           MOVE SCAN-BUF TO LCSCAN-BUF.
           IF SCAN-STATUS NOT = "00"
            OR LCSCAN-KEY = 0
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

      ***************************************************************
       DEALER-SCAN SECTION.
      ***************************************************************
           MOVE 0 TO DLSCAN-NUM.
           MOVE DLSCAN-BUF TO SCAN-BUF.
           PERFORM CALL-WINDOW-PROGRAM.
           MOVE SCAN-BUF TO DLSCAN-BUF.
           IF SCAN-STATUS NOT = "00"
            OR DLSCAN-NUM = " "
               MOVE 9 TO IO-FG
               GO TO EXIT-DEALER-SCAN.
           IF ( DEALER-FG = "DLBEG" )
              MOVE "SN N042DL1." TO VDU
           ELSE
              MOVE "SN N042DL2." TO VDU.
           MOVE DLSCAN-DLNO TO VDUNUM2.
           PERFORM SCREENIO.
           MOVE DLSCAN-DLNO TO VDUNUM2.
           MOVE 0 TO IO-FG.

       EXIT-DEALER-SCAN.
           EXIT.

      ***************************************************************
       VERIFY-PASSWORD SECTION.
      ***************************************************************
           MOVE " " TO PASSWDW-STAT.
           MOVE "LNAGKC" TO PASSWDW-PGM.
           MOVE "PLEASE ENTER PASSWORD TO " TO PASSWDW-PROMPT1.
           MOVE "CL/PASSWD"  TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH PASSWDW-BUF.
           CANCEL FORM-PROGX.


      *********************************************
       FORM-RESET SECTION.
      *********************************************
           MOVE EXT-CONAME-CONAME TO LNAGKC-CONAME.
           MOVE EXT-CONAME-SYSDATE TO LNAGKC-SYSDATE.
           DISPLAY LNAGKC-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE LNAGKC-FORM-FIELDS TO WR-BUF.
           MOVE LNAGKC-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.


      *********************************************
       SETUP-LINK-INFO SECTION.
      *********************************************
           MOVE LNAGKC-HELP--FILE TO HELP-LINK-FILE.
           MOVE LNAGKC-HELP--DIR  TO HELP-LINK-DIR.

      *********************************************
       VDU-CONVERT-FIELD SECTION.
      *********************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBUT/LNAGKC_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ******************************************************************
      *
      *     C O P Y    L I B R A R Y    M E M B E R S
      *
      ******************************************************************
       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/TIMEIO.CPY".
       COPY "LIBLP/LPEARN.CPY".
       COPY "LIBGB/AGEING.CPY".
       COPY "LIBLP/LPPDUE.CPY".
       COPY "LIBLP/LPUPWK.CPY".
       COPY "LIBGB/SPPDTH.CPY".
       COPY "LIBLP/LPCEPP.CPY".
       COPY "LIBLP/LPIBPC.CPY".
       COPY "LIBLP/LPSTAT.CPY".
       COPY "LIBLP/LPMDTE.CPY".
       COPY "LIBLP/LPXDTE.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBWI/PFSWINDOWS.CPY".
       COPY "LIBGB/GRWORK.CPY".
       COPY "LIBGB/GRSERCH.CPY".
       COPY "LIBGB/BRSECURE.CPY".
       COPY "LIBGB/EOSPOOL.CPY".
       COPY "LIBGB/GPENV.CPY".

      ******************************************************************
       MISC-ROUTINES SECTION.
      ******************************************************************
       COPY "LIBGB/MKTEMP.CPY".
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/DACCESS.CPY".
       COPY "LIBGB/SYSTEM.CPY".
       COPY "LIBGB/GETPR.CPY".
       COPY "LIBGB/RMFILE.CPY".
       SET-BRANCH-HEADINGS.
           MOVE BR-NAME   TO H-BRDESC.
           MOVE HOLD-LN-OWNBR TO BR-NO H-BRNO.
      *----------------------------------------------------------------
       COMPUTE-REPO.

            IF ( LN-REPO-ONHAND )
               MOVE 1 TO RPSUB
            ELSE
            IF ( LN-REPO-SOLD   )
               MOVE 2 TO RPSUB
            ELSE
               MOVE ZEROES TO RPSUB.

      *----------------------------------------------------------------
       COMPUTE-PL.

            MOVE ZEROES TO PLSUB.

            IF ( LN-PLCD NOT = SPACES )
               IF ( LN-PLTYPE = "2" )
                  MOVE 2 TO PLSUB
               ELSE
               IF ( LN-PLTYPE = "3" )
                  MOVE 3 TO PLSUB
               ELSE
               IF ( LN-PLTYPE = "4" )
                  MOVE 4 TO PLSUB
               ELSE
               IF ( LN-PLTYPE = "5" )
                  MOVE 5 TO PLSUB
               ELSE
                  MOVE 1 TO PLSUB.

      *----------------------------------------------------------------
       ACCUM-BYDUEDATE-TOTALS.
           ADD DD-NO(LEV)  TO TT-NO(1).
           ADD DD-BAL(LEV) TO TT-BAL(1).
           ADD DD-INT(LEV) TO TT-INT(1).
           ADD DD-OTH(LEV) TO TT-OTH(1).
           ADD DD-OTH2(LEV) TO TT-OTH2(1).
           ADD DD-PAY(LEV) TO TT-UNEARN(1).

      *----------------------------------------------------------------
       INVALID-RANGE.
           MOVE "INVALID RANGE" TO MESS.
           PERFORM SEND-MESS.

      *----------------------------------------------------------------
       PRINT-DUE-TOTALS.
           MOVE "      TOTAL:" TO S-NAME.
           MOVE TT-NO(1)       TO S-NO.
           MOVE TT-BAL(1)      TO S-BAL.
           MOVE TT-INT(1)      TO S-INT.
           MOVE TT-OTH(1)      TO S-OTH.
           MOVE TT-OTH2(1)     TO S-OTH2.
           MOVE TT-UNEARN(1)   TO S-UNEARN.
           PERFORM WRITE-DETAIL-LINE.

      *----------------------------------------------------------------
       PRINT-SUM-TOTALS.
           IF TYP = 3
              MOVE "  PAID-AHEAD" TO S-NAME
           ELSE
           IF TYP = 4
              MOVE " TOTAL REPO:" TO S-NAME
           ELSE
           IF TYP = 5
              MOVE "  TOTAL P&L:" TO S-NAME
           ELSE
              MOVE "      TOTAL:" TO S-NAME.

           MOVE TT-NO(TYP)  TO S-NO.
           MOVE TT-BAL(TYP) TO S-BAL.
           MOVE TT-INT(TYP) TO S-INT.
           MOVE TT-OTH(TYP) TO S-OTH.
           MOVE TT-OTH2(TYP) TO S-OTH2.
           MOVE TT-UNEARN(TYP) TO S-UNEARN.
           MOVE TT-UNDISC(TYP) TO S-UNDISC.
           PERFORM WRITE-DETAIL-LINE.

      *----------------------------------------------------------------
       ACCUM-AGEING-TOTALS.
           ADD T-NO(LEV AGE TYP)  TO TT-NO(TYP).
           ADD T-BAL(LEV AGE TYP) TO TT-BAL(TYP).
           ADD T-INT(LEV AGE TYP) TO TT-INT(TYP).
           ADD T-OTH(LEV AGE TYP) TO TT-OTH(TYP).
           ADD T-OTH2(LEV AGE TYP) TO TT-OTH2(TYP).
           ADD T-UNEARN(LEV AGE TYP) TO TT-UNEARN(TYP).
           ADD T-UNDISC(LEV AGE TYP) TO TT-UNDISC(TYP).
      *----------------------------------------------------------------
       ACCUM-AGEING-TOTALS-NM.
           ADD T-NM-NO(LEV AGE)     TO TT-NM-NO.
           ADD T-NM-BAL(LEV AGE)    TO TT-NM-BAL.
           ADD T-NM-INT(LEV AGE)    TO TT-NM-INT.
           ADD T-NM-OTH(LEV AGE)    TO TT-NM-OTH.
           ADD T-NM-OTH2(LEV AGE)   TO TT-NM-OTH2.
           ADD T-NM-UNEARN(LEV AGE) TO TT-NM-UNEARN.
           ADD T-NM-UNDISC(LEV AGE) TO TT-NM-UNDISC.
      *----------------------------------------------------------------
       SET-CLASS-HEADINGS.
           MOVE HOLD-LN-CLASS TO H-LCNO.

           IF HOLD-LN-CLASS NOT = LC-CODE
              MOVE HOLD-LN-CLASS TO LC-CODE
              PERFORM READ-LC1-FILE
              IF IO-FG NOT = 0
                 MOVE "INVALID" TO LC-DESC.

           MOVE LC-DESC TO H-LCDESC.
           MOVE SPACES TO H-LCBEG H-LCDASH H-LCEND.
           MOVE "N"     TO SUMMARY-FG.

      *----------------------------------------------------------------
       RESET-TOT-PAGE.
           MOVE ZEROS         TO TOTAL-TBL(3).
           MOVE "Y"           TO SUMMARY-FG.
           MOVE EXT-CONAME-RPT-LINES  TO LN-COUNT.
           MOVE 1             TO LN-FEED.

      ******************************************************************
       IO-ROUTINE SECTION.
      ******************************************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBLP/LPSPGS_SQL.CPY".
       COPY "LIBLP/LPLTPGS_SQL.CPY".
       COPY "LIBLP/LPLNGS_SQL.CPY".
       COPY "LIBLP/LPLCGS_SQL.CPY".
       COPY "LIBLP/LPBWGS_SQL.CPY".
       COPY "LIBGB/GBGRGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
      *COPY "LIBGB/GBBRRN.CPY".
      * COPYMEMBER: LIBGB/GBBRRN
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

      *-----------------------------------------------------------------
       OPEN-BR-FILE.
           PERFORM LOAD-BR-FILE.
           PERFORM OPEN-IT.
           MOVE BR-PATH-SQL TO E-FILE.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       START-BR-FILE.
           PERFORM START-IT.
           MOVE BR-PATH-SQL TO E-FILE.
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
                   ISNULL(BRFILE.BR_NAME,''),
                   ISNULL(BRFILE.BR_LOAN_NUM_FLAG,''),
                   ISNULL(BRFILE.BR_DATA_PATH,''),
                   ISNULL(BRFILE.BR_MACHINE,'')
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
           MOVE BR-PATH-SQL TO E-FILE.
           MOVE BR-NO TO E-KEYX.

           MOVE BR-NO TO QBR-NO.

           EXEC SQL
            SELECT BRFILE.BR_NO,
                   ISNULL(BRFILE.BR_NAME,''),
                   ISNULL(BRFILE.BR_LOAN_NUM_FLAG,''),
                   ISNULL(BRFILE.BR_DATA_PATH,''),
                   ISNULL(BRFILE.BR_MACHINE,'')
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
           MOVE BR-PATH-SQL TO E-FILE.
           MOVE BR-NO TO E-KEYX.
           INITIALIZE QBR-REC.

           IF ( BR-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH BR_CURSOR INTO
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
           IF ( BR-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE BR_CURSOR
              END-EXEC
              MOVE STAT---BAD TO BR-CURSOR-STAT.
 
       COPY "LIBGB/GBGR1RN.CPY".
       COPY "LIBGB/GBPRINT.CPY".
      *COPY "LIBLP/LPLN3RN.CPY".
      * COPYMEMBER: LIBLP/LPLN3RN
      *-----------------------------------------------------------------
      *    STILL NEED TO LOAD THE FILE PATH WORKERS
      *-----------------------------------------------------------------
       LOAD-LN3-FILE.
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

      *-----------------------------------------------------------------
       OPEN-LN3-FILE.
           PERFORM LOAD-LN3-FILE.
           PERFORM OPEN-IT.
           MOVE LN-PATH-SQL TO E-FILE.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       START-LN3-FILE.
           PERFORM START-IT.
           MOVE LN-PATH-OWNBR  TO LN-OWNBR.
           MOVE LN-PATH-SQL    TO E-FILE.
           MOVE SPACES         TO E-KEYX.
           STRING LN-OWNBR, LN-CLASS,LN-ACCTNO
              DELIMITED BY " " INTO E-KEYX.

           IF ( LN3-CURSOR-STAT-GOOD )
              PERFORM CLOSE-LN3-FILE.

           IF ( QLN3-WEND-ACCTNO = ZEROES    ) OR
              ( QLN3-WEND-ACCTNO NOT NUMERIC )
              MOVE LN-OWNBR             TO QLN3-WBEG-OWNBR
              MOVE LN-OWNBR             TO QLN3-WEND-OWNBR
              MOVE LN-CLASS             TO QLN3-WBEG-CLASS
              MOVE ALL "9"              TO QLN3-WEND-CLASS
              MOVE LN-ACCTNO            TO QLN3-WBEG-ACCTNO
              MOVE ALL "9"              TO QLN3-WEND-ACCTNO.

           EXEC SQL
            DECLARE LN3_CURSOR CURSOR FOR
            SELECT LNFILE.LN_OWNBR,
                   LNFILE.LN_ACCTNO,
                   LNFILE.LN_CLASS,
                   LNFILE.LN_SUBCLASS,
                   LNFILE.LN_LAWCODE,
                   ISNULL(LNFILE.LN_MAKERCD4,''),
                   LNFILE.LN_SSNO4,
                   ISNULL(LNFILE.LN_MAKERCD3,''),
                   LNFILE.LN_SSNO3,
                   ISNULL(LNFILE.LN_MAKERCD2,''),
                   LNFILE.LN_SSNO2,
                   ISNULL(LNFILE.LN_MAKERCD1,''),
                   LNFILE.LN_SSNO1,
                   ISNULL(LNFILE.LN_ORGST,''),
                   LNFILE.LN_SPRCLASS,
                   ISNULL(LNFILE.LN_POCD,''),
                   ISNULL(LNFILE.LN_PLCD,''),
                   ISNULL(LNFILE.LN_PLTYPE,''),
                   ISNULL(LNFILE.LN_STATUSFG,''),
                   ISNULL(LNFILE.LN_SALEFIN,''),
                   ISNULL(LNFILE.LN_SERVACCT,''),
                   ISNULL(LNFILE.LN_LOANTYPE,''),
                   ISNULL(LNFILE.LN_PAY_SCHLD_FG,''),
                   ISNULL(LNFILE.LN_STATFLAGS,''),
                   ISNULL(CAST(LNFILE.LN_ENTDATE AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_LOANDATE AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_INTDATE AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_ORIG_1STPYDATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_1STPYDATE AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_JDDATE AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_PLDATE AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_ACCELDATE AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_REDUDATE AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_LEGALDATE AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_BNKRPTDATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_DEBTAIDATE
                                                  AS VARCHAR(10)),'0'),
                   LNFILE.LN_APRATE,
                   LNFILE.LN_JDRATE,
                   LNFILE.LN_ORGTERM,
                   LNFILE.LN_1STPYAMT,
                   LNFILE.LN_NOREGPY,
                   LNFILE.LN_REGPYAMT,
                   LNFILE.LN_LASTPYAMT,
                   LNFILE.LN_PY_YYYYMM_RECDEL,
                   LNFILE.LN_MTD_PAYS_RECDEL,
                   LNFILE.LN_DLNO,
                   ISNULL(LNFILE.LN_DLRECOURSE,''),
                   LNFILE.LN_CURBAL,
                   LNFILE.LN_INTBAL,
                   LNFILE.LN_LCBAL,
                   LNFILE.LN_OTHBAL,
                   LNFILE.LN_OT2BAL,
                   LNFILE.LN_ANTICERN_1,
                   LNFILE.LN_ANTICERN_2,
                   LNFILE.LN_ANTICERN_3,
                   LNFILE.LN_ANTICERN_4,
                   LNFILE.LN_ANTICERN_5,
                   LNFILE.LN_ANTICERN_6,
                   LNFILE.LN_ANTICERN_7,
                   LNFILE.LN_ANTICERN_8,
                   LNFILE.LN_ANTICERN_9,
                   LNFILE.LN_ANTICERN_10,
                   LNFILE.LN_ANTICADJ_1,
                   LNFILE.LN_ANTICADJ_2,
                   LNFILE.LN_ANTICADJ_3,
                   LNFILE.LN_ANTICADJ_4,
                   LNFILE.LN_ANTICADJ_5,
                   LNFILE.LN_ANTICADJ_6,
                   LNFILE.LN_ANTICADJ_7,
                   LNFILE.LN_ANTICADJ_8,
                   LNFILE.LN_ANTICADJ_9,
                   LNFILE.LN_ANTICADJ_10,
                   LNFILE.LN_UFERN_1,
                   LNFILE.LN_UFERN_2,
                   LNFILE.LN_UFERN_3,
                   LNFILE.LN_UFERN_4,
                   LNFILE.LN_UFERN_5,
                   LNFILE.LN_UFERN_6,
                   LNFILE.LN_UFERN_7,
                   LNFILE.LN_UFERN_8,
                   LNFILE.LN_UFERN_9,
                   LNFILE.LN_UFERN_10,
                   LNFILE.LN_ACCERN_1,
                   LNFILE.LN_ACCERN_2,
                   LNFILE.LN_ACCERN_3,
                   LNFILE.LN_ACCERN_4,
                   LNFILE.LN_ACCERN_5,
                   LNFILE.LN_ACCERN_6,
                   LNFILE.LN_ACCERN_7,
                   LNFILE.LN_ACCERN_8,
                   LNFILE.LN_ACCERN_9,
                   LNFILE.LN_ACCERN_10,
                   LNFILE.LN_TOTPAYMNTD,
                   LNFILE.LN_TOTNODEF,
                   LNFILE.LN_TOTNOALDEL,
                   ISNULL(LNFILE.LN_UN_O10_REB,''),
                   ISNULL(LNFILE.LN_UN_O9_REB,''),
                   ISNULL(LNFILE.LN_UN_O8_REB,''),
                   ISNULL(LNFILE.LN_UN_O7_REB,''),
                   ISNULL(LNFILE.LN_O7_REBATE,''),
                   ISNULL(LNFILE.LN_O6_REBATE,''),
                   ISNULL(LNFILE.LN_O5_REBATE,''),
                   ISNULL(LNFILE.LN_O4_REBATE,''),
                   ISNULL(LNFILE.LN_O3_REBATE,''),
                   ISNULL(LNFILE.LN_O2_REBATE,''),
                   ISNULL(LNFILE.LN_O1_REBATE,''),
                   ISNULL(LNFILE.LN_EX_REBATE,''),
                   ISNULL(LNFILE.LN_DF_REBATE,''),
                   ISNULL(LNFILE.LN_MF_REBATE,''),
                   ISNULL(LNFILE.LN_SC_REBATE,''),
                   ISNULL(LNFILE.LN_INT_REBATE,''),
                   ISNULL(LNFILE.LN_PP_REBATE,''),
                   ISNULL(LNFILE.LN_AH_REBATE,''),
                   ISNULL(LNFILE.LN_CL_REBATE,''),
                   ISNULL(CAST(LNFILE.LN_DATE_PAID_LAST
                                                  AS VARCHAR(10)),'0'),
                   LNFILE.LN_SUM_LNAMT,
                   LNFILE.LN_SUM_ORGTERM,
                   ISNULL(LNFILE.LN_UNITPER_CD,''),
                   LNFILE.LN_UNITPER_FREQ,
                   ISNULL(LNFILE.LN_REPOCD,''),
                   LNFILE.LN_CONPROFILE_1,
                   LNFILE.LN_CONPROFILE_2,
                   LNFILE.LN_CONPROFILE_3,
                   LNFILE.LN_CONPROFILE_4,
                   LNFILE.LN_CONPROFILE_5,
                   LNFILE.LN_RECPROFILE_1,
                   LNFILE.LN_RECPROFILE_2,
                   LNFILE.LN_RECPROFILE_3,
                   LNFILE.LN_RECPROFILE_4,
                   LNFILE.LN_RECPROFILE_5
            FROM DBO.LNFILE
            WHERE LNFILE.LN_OWNBR >= :QLN3-WBEG-OWNBR
              AND LNFILE.LN_OWNBR <= :QLN3-WEND-OWNBR
              AND LNFILE.LN_CLASS >= :QLN3-WBEG-CLASS
              AND LNFILE.LN_CLASS <= :QLN3-WEND-CLASS
              AND LNFILE.LN_ACCTNO >= :QLN3-WBEG-ACCTNO
              AND LNFILE.LN_ACCTNO <= :QLN3-WEND-ACCTNO 
            ORDER BY LNFILE.LN_OWNBR,
                     LNFILE.LN_CLASS,
                     LNFILE.LN_ACCTNO
           END-EXEC.

           EXEC SQL
               OPEN LN3_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE ZEROES TO QLN3-WBEG-OWNBR
                          QLN3-WEND-OWNBR
                          QLN3-WBEG-CLASS
                          QLN3-WEND-CLASS
                          QLN3-WBEG-ACCTNO
                          QLN3-WEND-ACCTNO.

           MOVE STAT---GOOD TO LN3-CURSOR-STAT.

      *-----------------------------------------------------------------
      *START-LN3-FILE-GREATER.
      *    PERFORM START-IT.
      *    MOVE LN-PATH-OWNBR  TO LN-OWNBR.
      *    MOVE LN-PATH-SQL    TO E-FILE.
      *    MOVE SPACES         TO E-KEYX.
      *    STRING LN-OWNBR, LN-CLASS,LN-ACCTNO
      *       DELIMITED BY " " INTO E-KEYX.

      *    IF ( LN3-GR-CURSOR-STAT-GOOD )
      *       PERFORM CLOSE-LN3-FILE.

      *    IF ( QLN3-WEND-ACCTNO = ZEROES    ) OR
      *       ( QLN3-WEND-ACCTNO NOT NUMERIC )
      *       MOVE LN-OWNBR             TO QLN3-WBEG-OWNBR
      *       MOVE LN-OWNBR             TO QLN3-WEND-OWNBR
      *       MOVE LN-CLASS             TO QLN3-WBEG-CLASS
      *       MOVE ALL "9"              TO QLN3-WEND-CLASS
      *       MOVE LN-ACCTNO            TO QLN3-WBEG-ACCTNO
      *       MOVE ALL "9"              TO QLN3-WEND-ACCTNO.

      *    EXEC SQL
      *     DECLARE LN3_GR_CURSOR CURSOR FOR
      *     SELECT LNFILE.LN_OWNBR,
      *            LNFILE.LN_ACCTNO,
      *            LNFILE.LN_CLASS,
      *            LNFILE.LN_SUBCLASS,
      *            LNFILE.LN_LAWCODE,
      *            LNFILE.LN_SSNO1,
      *            ISNULL(LNFILE.LN_MAKERCD1,''),
      *            LNFILE.LN_SSNO2,
      *            ISNULL(LNFILE.LN_MAKERCD2,''),
      *            LNFILE.LN_SSNO3,
      *            ISNULL(LNFILE.LN_MAKERCD3,''),
      *            LNFILE.LN_SSNO4,
      *            ISNULL(LNFILE.LN_MAKERCD4,''),
      *            ISNULL(LNFILE.LN_ORGST,''),
      *            LNFILE.LN_ORGBR,
      *            ISNULL(LNFILE.LN_OWNST,''),
      *            LNFILE.LN_SPRCLASS,
      *            ISNULL(LNFILE.LN_CONVERCD,''),
      *            ISNULL(LNFILE.LN_MIDSTREAM,''),
      *            ISNULL(LNFILE.LN_PURCD,''),
      *            ISNULL(LNFILE.LN_SRCD,''),
      *            ISNULL(LNFILE.LN_COLLCD,''),
      *            ISNULL(LNFILE.LN_COLLID,''),
      *            ISNULL(LNFILE.LN_POCD,''),
      *            ISNULL(LNFILE.LN_PLCD,''),
      *            ISNULL(LNFILE.LN_PLTYPE,''),
      *            ISNULL(LNFILE.LN_CONFIDENTCD,''),
      *            ISNULL(LNFILE.LN_ALLOTCD,''),
      *            ISNULL(LNFILE.LN_RECASTCD,''),
      *            ISNULL(LNFILE.LN_SKIP_TRW,''),
      *            ISNULL(LNFILE.LN_STATUSFG,''),
      *            ISNULL(LNFILE.LN_SMALLOAN,''),
      *            ISNULL(LNFILE.LN_SALEFIN,''),
      *            ISNULL(LNFILE.LN_SERVACCT,''),
      *            ISNULL(LNFILE.LN_LOANTYPE,''),
      *            ISNULL(LNFILE.LN_ADDON_FG,''),
      *            ISNULL(LNFILE.LN_PAY_SCHLD_FG,''),
      *            LNFILE.LN_LC_CODE,
      *            ISNULL(LNFILE.LN_CAN_LOAN_NO,''),
      *            ISNULL(LNFILE.LN_STATFLAGS,''),
      *            ISNULL(CAST(LNFILE.LN_ENTDATE AS VARCHAR(10)),'0'),
      *            ISNULL(CAST(LNFILE.LN_LOANDATE AS VARCHAR(10)),'0'),
      *            ISNULL(CAST(LNFILE.LN_PURDATE AS VARCHAR(10)),'0'),
      *            ISNULL(CAST(LNFILE.LN_INTDATE AS VARCHAR(10)),'0'),
      *            ISNULL(CAST(LNFILE.LN_CASHDATE AS VARCHAR(10)),'0'),
      *            ISNULL(CAST(LNFILE.LN_FILEDATE AS VARCHAR(10)),'0'),
      *            ISNULL(CAST(LNFILE.LN_RESCISSION_DATE
      *                                           AS VARCHAR(10)),'0'),
      *            ISNULL(CAST(LNFILE.LN_PART_RECOURSE_DATE
      *                                           AS VARCHAR(10)),'0'),
      *            ISNULL(CAST(LNFILE.LN_ORIG_1STPYDATE
      *                                           AS VARCHAR(10)),'0'),
      *            ISNULL(CAST(LNFILE.LN_1STPYDATE
      *                                           AS VARCHAR(10)),'0'),
      *            ISNULL(CAST(LNFILE.LN_JDDATE AS VARCHAR(10)),'0'),
      *            ISNULL(CAST(LNFILE.LN_PLDATE AS VARCHAR(10)),'0'),
      *            ISNULL(CAST(LNFILE.LN_PL_TRDATE
      *                                           AS VARCHAR(10)),'0'),
      *            ISNULL(CAST(LNFILE.LN_ACCELDATE
      *                                           AS VARCHAR(10)),'0'),
      *            ISNULL(CAST(LNFILE.LN_REDUDATE AS VARCHAR(10)),'0'),
      *            ISNULL(CAST(LNFILE.LN_POFFDATE AS VARCHAR(10)),'0'),
      *            ISNULL(CAST(LNFILE.LN_PO_TRDATE
      *                                           AS VARCHAR(10)),'0'),
      *            ISNULL(CAST(LNFILE.LN_LEGALDATE
      *                                           AS VARCHAR(10)),'0'),
      *            ISNULL(CAST(LNFILE.LN_BNKRPTDATE
      *                                           AS VARCHAR(10)),'0'),
      *            ISNULL(CAST(LNFILE.LN_BNKRPT_DISMISS_DATE
      *                                           AS VARCHAR(10)),'0'),
      *            ISNULL(CAST(LNFILE.LN_BNKRPT_DISCHARGE_DATE
      *                                           AS VARCHAR(10)),'0'),
      *            ISNULL(CAST(LNFILE.LN_BNKRPT_REAFFIRM_DATE
      *                                           AS VARCHAR(10)),'0'),
      *            ISNULL(CAST(LNFILE.LN_DEBTAIDATE
      *                                           AS VARCHAR(10)),'0'),
      *            ISNULL(CAST(LNFILE.LN_REPODATE AS VARCHAR(10)),'0'),
      *            ISNULL(CAST(LNFILE.LN_REDEEMDATE
      *                                           AS VARCHAR(10)),'0'),
      *            ISNULL(CAST(LNFILE.LN_SOLDDATE AS VARCHAR(10)),'0'),
      *            ISNULL(CAST(LNFILE.LN_INS_CONFIRM_DATE
      *                                           AS VARCHAR(10)),'0'),
      *            ISNULL(CAST(LNFILE.LN_PRIOR_RN_FOR10_DATE
      *                                           AS VARCHAR(10)),'0'),
      *            ISNULL(CAST(LNFILE.LN_LAST_RN_FOR10_DATE
      *                                           AS VARCHAR(10)),'0'),
      *            ISNULL(CAST(LNFILE.LN_TRW_DOFD_DATE
      *                                           AS VARCHAR(10)),'0'),
      *            ISNULL(CAST(LNFILE.LN_LAST_PYPA_DATE
      *                                           AS VARCHAR(10)),'0'),
      *            ISNULL(CAST(LNFILE.LN_BT_TRDATE
      *                                           AS VARCHAR(10)),'0'),
      *            ISNULL(CAST(LNFILE.LN_METRO2_LAST_PMT_DATE
      *                                           AS VARCHAR(10)),'0'),
      *            ISNULL(CAST(LNFILE.LN_FOLLOWUP_DATE
      *                                           AS VARCHAR(10)),'0'),
      *            ISNULL(CAST(LNFILE.LN_LAST_ACCRUAL_DATE
      *                                           AS VARCHAR(10)),'0'),
      *            LNFILE.LN_APRATE,
      *            LNFILE.LN_EFFRATE,
      *            LNFILE.LN_SMPRATE,
      *            LNFILE.LN_YLDRATE,
      *            LNFILE.LN_OVRDRATE,
      *            LNFILE.LN_ENDORATE,
      *            LNFILE.LN_JDRATE,
      *            LNFILE.LN_ACCRATE,
      *            LNFILE.LN_REDURATE,
      *            LNFILE.LN_CONRATE,
      *            LNFILE.LN_SERVPER,
      *            LNFILE.LN_SELLPR,
      *            LNFILE.LN_DOWNPY,
      *            LNFILE.LN_TRADEIN,
      *            LNFILE.LN_LNAMT,
      *            LNFILE.LN_FINCHG,
      *            LNFILE.LN_INTCHG,
      *            LNFILE.LN_SERCHG,
      *            LNFILE.LN_EXTCHG,
      *            LNFILE.LN_MAINTFEE,
      *            LNFILE.LN_POINTS,
      *            LNFILE.LN_PROCEEDS,
      *            LNFILE.LN_ORGTERM,
      *            LNFILE.LN_1STPYAMT,
      *            LNFILE.LN_NOREGPY,
      *            LNFILE.LN_REGPYAMT,
      *            LNFILE.LN_LASTPYAMT,
      *            LNFILE.LN_PROCASH,
      *            LNFILE.LN_TILLNO,
      *            ISNULL(LNFILE.LN_STATE_FLAG,''),
      *            LNFILE.LN_STATE_WORKER,
      *            LNFILE.LN_STATE_WORKER2,
      *            ISNULL(LNFILE.LN_FEECODE_1,''),
      *            LNFILE.LN_FEEAMT_1,
      *            ISNULL(LNFILE.LN_FEEOURS_1,''),
      *            ISNULL(LNFILE.LN_FEEFINCHG_1,''),
      *            ISNULL(LNFILE.LN_FEECODE_2,''),
      *            LNFILE.LN_FEEAMT_2,
      *            ISNULL(LNFILE.LN_FEEOURS_2,''),
      *            ISNULL(LNFILE.LN_FEEFINCHG_2,''),
      *            ISNULL(LNFILE.LN_FEECODE_3,''),
      *            LNFILE.LN_FEEAMT_3,
      *            ISNULL(LNFILE.LN_FEEOURS_3,''),
      *            ISNULL(LNFILE.LN_FEEFINCHG_3,''),
      *            ISNULL(LNFILE.LN_FEECODE_4,''),
      *            LNFILE.LN_FEEAMT_4,
      *            ISNULL(LNFILE.LN_FEEOURS_4,''),
      *            ISNULL(LNFILE.LN_FEEFINCHG_4,''),
      *            ISNULL(LNFILE.LN_FEECODE_5,''),
      *            LNFILE.LN_FEEAMT_5,
      *            ISNULL(LNFILE.LN_FEEOURS_5,''),
      *            ISNULL(LNFILE.LN_FEEFINCHG_5,''),
      *            ISNULL(LNFILE.LN_FEECODE_6,''),
      *            LNFILE.LN_FEEAMT_6,
      *            ISNULL(LNFILE.LN_FEEOURS_6,''),
      *            ISNULL(LNFILE.LN_FEEFINCHG_6,''),
      *            ISNULL(LNFILE.LN_FEECODE_7,''),
      *            LNFILE.LN_FEEAMT_7,
      *            ISNULL(LNFILE.LN_FEEOURS_7,''),
      *            ISNULL(LNFILE.LN_FEEFINCHG_7,''),
      *            ISNULL(LNFILE.LN_FEECODE_8,''),
      *            LNFILE.LN_FEEAMT_8,
      *            ISNULL(LNFILE.LN_FEEOURS_8,''),
      *            ISNULL(LNFILE.LN_FEEFINCHG_8,''),
      *            ISNULL(LNFILE.LN_FEECODE_9,''),
      *            LNFILE.LN_FEEAMT_9,
      *            ISNULL(LNFILE.LN_FEEOURS_9,''),
      *            ISNULL(LNFILE.LN_FEEFINCHG_9,''),
      *            ISNULL(LNFILE.LN_FEECODE_10,''),
      *            LNFILE.LN_FEEAMT_10,
      *            ISNULL(LNFILE.LN_FEEOURS_10,''),
      *            ISNULL(LNFILE.LN_FEEFINCHG_10,''),
      *            ISNULL(LNFILE.LN_FEECODE_11,''),
      *            LNFILE.LN_FEEAMT_11,
      *            ISNULL(LNFILE.LN_FEEOURS_11,''),
      *            ISNULL(LNFILE.LN_FEEFINCHG_11,''),
      *            ISNULL(LNFILE.LN_FEECODE_12,''),
      *            LNFILE.LN_FEEAMT_12,
      *            ISNULL(LNFILE.LN_FEEOURS_12,''),
      *            ISNULL(LNFILE.LN_FEEFINCHG_12,''),
      *            ISNULL(LNFILE.LN_FEECODE_13,''),
      *            LNFILE.LN_FEEAMT_13,
      *            ISNULL(LNFILE.LN_FEEOURS_13,''),
      *            ISNULL(LNFILE.LN_FEEFINCHG_13,''),
      *            ISNULL(LNFILE.LN_FEECODE_14,''),
      *            LNFILE.LN_FEEAMT_14,
      *            ISNULL(LNFILE.LN_FEEOURS_14,''),
      *            ISNULL(LNFILE.LN_FEEFINCHG_14,''),
      *            ISNULL(LNFILE.LN_FEECODE_15,''),
      *            LNFILE.LN_FEEAMT_15,
      *            ISNULL(LNFILE.LN_FEEOURS_15,''),
      *            ISNULL(LNFILE.LN_FEEFINCHG_15,''),
      *            ISNULL(LNFILE.LN_FEECODE_16,''),
      *            LNFILE.LN_FEEAMT_16,
      *            ISNULL(LNFILE.LN_FEEOURS_16,''),
      *            ISNULL(LNFILE.LN_FEEFINCHG_16,''),
      *            ISNULL(LNFILE.LN_FEECODE_17,''),
      *            LNFILE.LN_FEEAMT_17,
      *            ISNULL(LNFILE.LN_FEEOURS_17,''),
      *            ISNULL(LNFILE.LN_FEEFINCHG_17,''),
      *            ISNULL(LNFILE.LN_FEECODE_18,''),
      *            LNFILE.LN_FEEAMT_18,
      *            ISNULL(LNFILE.LN_FEEOURS_18,''),
      *            ISNULL(LNFILE.LN_FEEFINCHG_18,''),
      *            ISNULL(LNFILE.LN_FEECODE_19,''),
      *            LNFILE.LN_FEEAMT_19,
      *            ISNULL(LNFILE.LN_FEEOURS_19,''),
      *            ISNULL(LNFILE.LN_FEEFINCHG_19,''),
      *            ISNULL(LNFILE.LN_FEECODE_20,''),
      *            LNFILE.LN_FEEAMT_20,
      *            ISNULL(LNFILE.LN_FEEOURS_20,''),
      *            ISNULL(LNFILE.LN_FEEFINCHG_20,''),
      *            ISNULL(LNFILE.LN_INSURED,''),
      *            ISNULL(LNFILE.LN_INSTYPES,''),
      *            ISNULL(LNFILE.LN_INSELIGIBLE,''),
      *            LNFILE.LN_INSCOMP_1,
      *            ISNULL(LNFILE.LN_INSOURS_1,''),
      *            ISNULL(CAST(LNFILE.LN_INSEFF_DATE_1
      *                                           AS VARCHAR(10)),'0'),
      *            ISNULL(CAST(LNFILE.LN_INSEXP_DATE_1
      *                                           AS VARCHAR(10)),'0'),
      *            LNFILE.LN_INSPREM_1,
      *            LNFILE.LN_INSCOVR_1,
      *            LNFILE.LN_INSCOMM_1,
      *            LNFILE.LN_INSANTICERN_1,
      *            LNFILE.LN_INSANTICADJ_1,
      *            LNFILE.LN_INSUFERN_1,
      *            LNFILE.LN_INSACCERN_1,
      *            LNFILE.LN_INSEARN_ORDER_1,
      *            ISNULL(LNFILE.LN_INSFINCHG_1,''),
      *            LNFILE.LN_INSCOMP_2,
      *            ISNULL(LNFILE.LN_INSOURS_2,''),
      *            ISNULL(CAST(LNFILE.LN_INSEFF_DATE_2
      *                                           AS VARCHAR(10)),'0'),
      *            ISNULL(CAST(LNFILE.LN_INSEXP_DATE_2
      *                                           AS VARCHAR(10)),'0'),
      *            LNFILE.LN_INSPREM_2,
      *            LNFILE.LN_INSCOVR_2,
      *            LNFILE.LN_INSCOMM_2,
      *            LNFILE.LN_INSANTICERN_2,
      *            LNFILE.LN_INSANTICADJ_2,
      *            LNFILE.LN_INSUFERN_2,
      *            LNFILE.LN_INSACCERN_2,
      *            LNFILE.LN_INSEARN_ORDER_2,
      *            ISNULL(LNFILE.LN_INSFINCHG_2,''),
      *            LNFILE.LN_INSCOMP_3,
      *            ISNULL(LNFILE.LN_INSOURS_3,''),
      *            ISNULL(CAST(LNFILE.LN_INSEFF_DATE_3
      *                                           AS VARCHAR(10)),'0'),
      *            ISNULL(CAST(LNFILE.LN_INSEXP_DATE_3
      *                                           AS VARCHAR(10)),'0'),
      *            LNFILE.LN_INSPREM_3,
      *            LNFILE.LN_INSCOVR_3,
      *            LNFILE.LN_INSCOMM_3,
      *            LNFILE.LN_INSANTICERN_3,
      *            LNFILE.LN_INSANTICADJ_3,
      *            LNFILE.LN_INSUFERN_3,
      *            LNFILE.LN_INSACCERN_3,
      *            LNFILE.LN_INSEARN_ORDER_3,
      *            ISNULL(LNFILE.LN_INSFINCHG_3,''),
      *            LNFILE.LN_INSCOMP_4,
      *            ISNULL(LNFILE.LN_INSOURS_4,''),
      *            ISNULL(CAST(LNFILE.LN_INSEFF_DATE_4
      *                                           AS VARCHAR(10)),'0'),
      *            ISNULL(CAST(LNFILE.LN_INSEXP_DATE_4
      *                                           AS VARCHAR(10)),'0'),
      *            LNFILE.LN_INSPREM_4,
      *            LNFILE.LN_INSCOVR_4,
      *            LNFILE.LN_INSCOMM_4,
      *            LNFILE.LN_INSANTICERN_4,
      *            LNFILE.LN_INSANTICADJ_4,
      *            LNFILE.LN_INSUFERN_4,
      *            LNFILE.LN_INSACCERN_4,
      *            LNFILE.LN_INSEARN_ORDER_4,
      *            ISNULL(LNFILE.LN_INSFINCHG_4,''),
      *            LNFILE.LN_INSCOMP_5,
      *            ISNULL(LNFILE.LN_INSOURS_5,''),
      *            ISNULL(CAST(LNFILE.LN_INSEFF_DATE_5
      *                                           AS VARCHAR(10)),'0'),
      *            ISNULL(CAST(LNFILE.LN_INSEXP_DATE_5
      *                                           AS VARCHAR(10)),'0'),
      *            LNFILE.LN_INSPREM_5,
      *            LNFILE.LN_INSCOVR_5,
      *            LNFILE.LN_INSCOMM_5,
      *            LNFILE.LN_INSANTICERN_5,
      *            LNFILE.LN_INSANTICADJ_5,
      *            LNFILE.LN_INSUFERN_5,
      *            LNFILE.LN_INSACCERN_5,
      *            LNFILE.LN_INSEARN_ORDER_5,
      *            ISNULL(LNFILE.LN_INSFINCHG_5,''),
      *            LNFILE.LN_INSCOMP_6,
      *            ISNULL(LNFILE.LN_INSOURS_6,''),
      *            ISNULL(CAST(LNFILE.LN_INSEFF_DATE_6
      *                                           AS VARCHAR(10)),'0'),
      *            ISNULL(CAST(LNFILE.LN_INSEXP_DATE_6
      *                                           AS VARCHAR(10)),'0'),
      *            LNFILE.LN_INSPREM_6,
      *            LNFILE.LN_INSCOVR_6,
      *            LNFILE.LN_INSCOMM_6,
      *            LNFILE.LN_INSANTICERN_6,
      *            LNFILE.LN_INSANTICADJ_6,
      *            LNFILE.LN_INSUFERN_6,
      *            LNFILE.LN_INSACCERN_6,
      *            LNFILE.LN_INSEARN_ORDER_6,
      *            ISNULL(LNFILE.LN_INSFINCHG_6,''),
      *            LNFILE.LN_INSCOMP_7,
      *            ISNULL(LNFILE.LN_INSOURS_7,''),
      *            ISNULL(CAST(LNFILE.LN_INSEFF_DATE_7
      *                                           AS VARCHAR(10)),'0'),
      *            ISNULL(CAST(LNFILE.LN_INSEXP_DATE_7
      *                                           AS VARCHAR(10)),'0'),
      *            LNFILE.LN_INSPREM_7,
      *            LNFILE.LN_INSCOVR_7,
      *            LNFILE.LN_INSCOMM_7,
      *            LNFILE.LN_INSANTICERN_7,
      *            LNFILE.LN_INSANTICADJ_7,
      *            LNFILE.LN_INSUFERN_7,
      *            LNFILE.LN_INSACCERN_7,
      *            LNFILE.LN_INSEARN_ORDER_7,
      *            ISNULL(LNFILE.LN_INSFINCHG_7,''),
      *            LNFILE.LN_INSCOMP_8,
      *            ISNULL(LNFILE.LN_INSOURS_8,''),
      *            ISNULL(CAST(LNFILE.LN_INSEFF_DATE_8
      *                                           AS VARCHAR(10)),'0'),
      *            ISNULL(CAST(LNFILE.LN_INSEXP_DATE_8
      *                                           AS VARCHAR(10)),'0'),
      *            LNFILE.LN_INSPREM_8,
      *            LNFILE.LN_INSCOVR_8,
      *            LNFILE.LN_INSCOMM_8,
      *            LNFILE.LN_INSANTICERN_8,
      *            LNFILE.LN_INSANTICADJ_8,
      *            LNFILE.LN_INSUFERN_8,
      *            LNFILE.LN_INSACCERN_8,
      *            LNFILE.LN_INSEARN_ORDER_8,
      *            ISNULL(LNFILE.LN_INSFINCHG_8,''),
      *            LNFILE.LN_INSCOMP_9,
      *            ISNULL(LNFILE.LN_INSOURS_9,''),
      *            ISNULL(CAST(LNFILE.LN_INSEFF_DATE_9
      *                                           AS VARCHAR(10)),'0'),
      *            ISNULL(CAST(LNFILE.LN_INSEXP_DATE_9
      *                                           AS VARCHAR(10)),'0'),
      *            LNFILE.LN_INSPREM_9,
      *            LNFILE.LN_INSCOVR_9,
      *            LNFILE.LN_INSCOMM_9,
      *            LNFILE.LN_INSANTICERN_9,
      *            LNFILE.LN_INSANTICADJ_9,
      *            LNFILE.LN_INSUFERN_9,
      *            LNFILE.LN_INSACCERN_9,
      *            LNFILE.LN_INSEARN_ORDER_9,
      *            ISNULL(LNFILE.LN_INSFINCHG_9,''),
      *            LNFILE.LN_INSCOMP_10,
      *            ISNULL(LNFILE.LN_INSOURS_10,''),
      *            ISNULL(CAST(LNFILE.LN_INSEFF_DATE_10
      *                                           AS VARCHAR(10)),'0'),
      *            ISNULL(CAST(LNFILE.LN_INSEXP_DATE_10
      *                                           AS VARCHAR(10)),'0'),
      *            LNFILE.LN_INSPREM_10,
      *            LNFILE.LN_INSCOVR_10,
      *            LNFILE.LN_INSCOMM_10,
      *            LNFILE.LN_INSANTICERN_10,
      *            LNFILE.LN_INSANTICADJ_10,
      *            LNFILE.LN_INSUFERN_10,
      *            LNFILE.LN_INSACCERN_10,
      *            LNFILE.LN_INSEARN_ORDER_10,
      *            ISNULL(LNFILE.LN_INSFINCHG_10,''),
      *            LNFILE.LN_PY_YYYYMM_RECDEL,
      *            LNFILE.LN_MTD_PAYS_RECDEL,
      *            ISNULL(LNFILE.LN_GARNISH_EXTR,''),
      *            ISNULL(LNFILE.LN_SHIP_SAME_FG,''),
      *            LNFILE.LN_AD_D_INS_COVR,
      *            LNFILE.LN_TOT_PARTIAL_CHGOFF,
      *            ISNULL(LNFILE.LN_LPO,''),
      *            ISNULL(LNFILE.LN_REP_CD,''),
      *            LNFILE.LN_EMP_ID,
      *            ISNULL(LNFILE.LN_PROCESS_CD,''),
      *            LNFILE.LN_WCBC_CREDITLIM,
      *            ISNULL(LNFILE.LN_FOLLOWUP_CD,''),
      *            ISNULL(LNFILE.LN_MIP_FG,''),
      *            LNFILE.LN_MIP_MTD_CLAHPAID,
      *            ISNULL(CAST(LNFILE.LN_MIP_CLPDTH_DATE
      *                                           AS VARCHAR(10)),'0'),
      *            LNFILE.LN_MIP_TOT_CLPAID,
      *            ISNULL(CAST(LNFILE.LN_MIP_AHPDTH_DATE
      *                                           AS VARCHAR(10)),'0'),
      *            LNFILE.LN_MIP_TOT_AHPAID,
      *            LNFILE.LN_IL_GROSS_INCOME,
      *            LNFILE.LN_IL_GROSS_INCOME2,
      *            ISNULL(LNFILE.LN_RESCISSION,''),
      *            ISNULL(LNFILE.LN_INTERIM_TRW,''),
      *            LNFILE.LN_DLNO,
      *            LNFILE.LN_DLPOOL,
      *            ISNULL(LNFILE.LN_DLRECOURSE,''),
      *            LNFILE.LN_DLPARTRCAMT,
      *            LNFILE.LN_ORIGPURAMT,
      *            LNFILE.LN_PURBAL,
      *            LNFILE.LN_DLRVPRIN_1,
      *            LNFILE.LN_DLRVPRIN_2,
      *            LNFILE.LN_DLRVPRIN_3,
      *            LNFILE.LN_DLRVCHGS_1,
      *            LNFILE.LN_DLRVCHGS_2,
      *            LNFILE.LN_DLRVCHGS_3,
      *            LNFILE.LN_DLPARVHELD,
      *            LNFILE.LN_DLPARVPAID,
      *            LNFILE.LN_DLDISC,
      *            ISNULL(CAST(LNFILE.LN_LCPDTH_DATE
      *                                           AS VARCHAR(10)),'0'),
      *            ISNULL(CAST(LNFILE.LN_INTPDTH_DATE
      *                                           AS VARCHAR(10)),'0'),
      *            LNFILE.LN_CURBAL,
      *            LNFILE.LN_INTBAL,
      *            LNFILE.LN_LCBAL,
      *            LNFILE.LN_OTHBAL,
      *            LNFILE.LN_OT2BAL,
      *            LNFILE.LN_LCPAID,
      *            LNFILE.LN_LCPARTIALS,
      *            LNFILE.LN_STMT_COUNTER,
      *            LNFILE.LN_PRIOR_STMT_COUNTER,
      *            ISNULL(LNFILE.LN_FROZEN_REASON,''),
      *            ISNULL(LNFILE.LN_COUPON_STATEMENT,''),
      *            LNFILE.LN_MONTHEND_BAL,
      *            LNFILE.LN_ANTICTERM,
      *            LNFILE.LN_ANTICERN_1,
      *            LNFILE.LN_ANTICADJ_1,
      *            LNFILE.LN_UFERN_1,
      *            LNFILE.LN_ACCERN_1,
      *            LNFILE.LN_EARN_ORDER_1,
      *            LNFILE.LN_ANTICERN_2,
      *            LNFILE.LN_ANTICADJ_2,
      *            LNFILE.LN_UFERN_2,
      *            LNFILE.LN_ACCERN_2,
      *            LNFILE.LN_EARN_ORDER_2,
      *            LNFILE.LN_ANTICERN_3,
      *            LNFILE.LN_ANTICADJ_3,
      *            LNFILE.LN_UFERN_3,
      *            LNFILE.LN_ACCERN_3,
      *            LNFILE.LN_EARN_ORDER_3,
      *            LNFILE.LN_ANTICERN_4,
      *            LNFILE.LN_ANTICADJ_4,
      *            LNFILE.LN_UFERN_4,
      *            LNFILE.LN_ACCERN_4,
      *            LNFILE.LN_EARN_ORDER_4,
      *            LNFILE.LN_ANTICERN_5,
      *            LNFILE.LN_ANTICADJ_5,
      *            LNFILE.LN_UFERN_5,
      *            LNFILE.LN_ACCERN_5,
      *            LNFILE.LN_EARN_ORDER_5,
      *            LNFILE.LN_ANTICERN_6,
      *            LNFILE.LN_ANTICADJ_6,
      *            LNFILE.LN_UFERN_6,
      *            LNFILE.LN_ACCERN_6,
      *            LNFILE.LN_EARN_ORDER_6,
      *            LNFILE.LN_ANTICERN_7,
      *            LNFILE.LN_ANTICADJ_7,
      *            LNFILE.LN_UFERN_7,
      *            LNFILE.LN_ACCERN_7,
      *            LNFILE.LN_EARN_ORDER_7,
      *            LNFILE.LN_ANTICERN_8,
      *            LNFILE.LN_ANTICADJ_8,
      *            LNFILE.LN_UFERN_8,
      *            LNFILE.LN_ACCERN_8,
      *            LNFILE.LN_EARN_ORDER_8,
      *            LNFILE.LN_ANTICERN_9,
      *            LNFILE.LN_ANTICADJ_9,
      *            LNFILE.LN_UFERN_9,
      *            LNFILE.LN_ACCERN_9,
      *            LNFILE.LN_EARN_ORDER_9,
      *            LNFILE.LN_ANTICERN_10,
      *            LNFILE.LN_ANTICADJ_10,
      *            LNFILE.LN_UFERN_10,
      *            LNFILE.LN_ACCERN_10,
      *            LNFILE.LN_EARN_ORDER_10,
      *            LNFILE.LN_LMO_PRIOR_INTEREST,
      *            LNFILE.LN_PRIOR_UNEARNED,
      *            ISNULL(LNFILE.LN_ERNDELQ_STOP,''),
      *            LNFILE.LN_NONACRERN,
      *            LNFILE.LN_PRIOR_NONACRERN,
      *            ISNULL(CAST(LNFILE.LN_ERN_IB_PDTH_DATE
      *                                           AS VARCHAR(10)),'0'),
      *            LNFILE.LN_ERN_IB_PRINBAL,
      *            LNFILE.LN_ERN_IB_INTOWE,
      *            LNFILE.LN_TOTERN,
      *            LNFILE.LN_TOTOTH,
      *            LNFILE.LN_TOTINT,
      *            LNFILE.LN_TOTLCHG,
      *            LNFILE.LN_TOTDEF,
      *            LNFILE.LN_TOTSERACCT,
      *            LNFILE.LN_LCWAIVERS,
      *            LNFILE.LN_INTWAIVERS,
      *            LNFILE.LN_TOTPAYMNTD,
      *            LNFILE.LN_TOTEXCPAYMNTD,
      *            LNFILE.LN_TOTNODEF,
      *            LNFILE.LN_YTDNODEF,
      *            LNFILE.LN_TOTNODEFACT,
      *            LNFILE.LN_YTDNODEFACT,
      *            LNFILE.LN_TOTNOALDEL,
      *            LNFILE.LN_YTDNOALDEL,
      *            LNFILE.LN_YTDINTPD,
      *            LNFILE.LN_LYRINTPD,
      *            LNFILE.LN_TOTINTPD,
      *            LNFILE.LN_RECASTINT,
      *            LNFILE.LN_RECASTPRIN,
      *            ISNULL(LNFILE.LN_CL_REBATE,''),
      *            ISNULL(LNFILE.LN_AH_REBATE,''),
      *            ISNULL(LNFILE.LN_PP_REBATE,''),
      *            ISNULL(LNFILE.LN_INT_REBATE,''),
      *            ISNULL(LNFILE.LN_SC_REBATE,''),
      *            ISNULL(LNFILE.LN_MF_REBATE,''),
      *            ISNULL(LNFILE.LN_DF_REBATE,''),
      *            ISNULL(LNFILE.LN_EX_REBATE,''),
      *            ISNULL(LNFILE.LN_O1_REBATE,''),
      *            ISNULL(LNFILE.LN_O2_REBATE,''),
      *            ISNULL(LNFILE.LN_O3_REBATE,''),
      *            ISNULL(LNFILE.LN_O4_REBATE,''),
      *            ISNULL(LNFILE.LN_O5_REBATE,''),
      *            ISNULL(LNFILE.LN_O6_REBATE,''),
      *            ISNULL(LNFILE.LN_O7_REBATE,''),
      *            ISNULL(LNFILE.LN_UN_O7_REB,''),
      *            ISNULL(LNFILE.LN_UN_O8_REB,''),
      *            ISNULL(LNFILE.LN_UN_O9_REB,''),
      *            ISNULL(LNFILE.LN_UN_O10_REB,''),
      *            ISNULL(LNFILE.LN_PLREASON,''),
      *            ISNULL(LNFILE.LN_COLLECTOR,''),
      *            ISNULL(LNFILE.LN_ACTIONCD,''),
      *            ISNULL(LNFILE.LN_ACTIONCD2,''),
      *            ISNULL(LNFILE.LN_ATCODE,''),
      *            ISNULL(LNFILE.LN_RECODE,''),
      *            ISNULL(LNFILE.LN_ESCROW_FG,''),
      *            LNFILE.LN_PROMDAYS_PASTDUE,
      *            ISNULL(LNFILE.LN_PROMID,''),
      *            ISNULL(CAST(LNFILE.LN_PROMDATE AS VARCHAR(10)),'0'),
      *            LNFILE.LN_PROMAMT,
      *            ISNULL(CAST(LNFILE.LN_PROMDATE2
      *                                           AS VARCHAR(10)),'0'),
      *            ISNULL(CAST(LNFILE.LN_DATE_PAID_LAST
      *                                           AS VARCHAR(10)),'0'),
      *            LNFILE.LN_AMT_PAID_LAST,
      *            LNFILE.LN_LASTCFORMNO,
      *            ISNULL(CAST(LNFILE.LN_LASTCFORMDATE
      *                                           AS VARCHAR(10)),'0'),
      *            LNFILE.LN_DELCNT,
      *            LNFILE.LN_DELCNT2,
      *            LNFILE.LN_CREDITLIM,
      *            LNFILE.LN_CRSCORE,
      *            LNFILE.LN_LASTSFORMNO,
      *            ISNULL(CAST(LNFILE.LN_LASTSFORMDATE
      *                                           AS VARCHAR(10)),'0'),
      *            LNFILE.LN_SOLCNT,
      *            ISNULL(LNFILE.LN_MESSAGE,''),
      *            LNFILE.LN_OLDACCTNO,
      *            LNFILE.LN_PRLNNO,
      *            LNFILE.LN_PRBAL,
      *            LNFILE.LN_PRINTDUE,
      *            LNFILE.LN_PRFINCHGRB,
      *            LNFILE.LN_PRINSRB,
      *            ISNULL(LNFILE.LN_PRPOCD,''),
      *            ISNULL(CAST(LNFILE.LN_PRPODATE AS VARCHAR(10)),'0'),
      *            LNFILE.LN_PRDELINQAMT,
      *            LNFILE.LN_PRLNAMT,
      *            LNFILE.LN_PRPDUE_DAYS,
      *            LNFILE.LN_PRDLDISC_UNEARN,
      *            LNFILE.LN_PRCONPROFILE,
      *            LNFILE.LN_SUM_ORIGPURAMT,
      *            LNFILE.LN_SUM_LNAMT,
      *            LNFILE.LN_SUM_FINCHG,
      *            LNFILE.LN_SUM_INTCHG,
      *            LNFILE.LN_SUM_ORGTERM,
      *            LNFILE.LN_SUM_REGPYAMT,
      *            LNFILE.LN_SUM_EXTCHG,
      *            LNFILE.LN_SUM_SERCHG,
      *            LNFILE.LN_SUM_MAINTFEE,
      *            ISNULL(LNFILE.LN_UNITPER_CD,''),
      *            LNFILE.LN_UNITPER_FREQ,
      *            LNFILE.LN_DAYS_IN_UNITPER,
      *            LNFILE.LN_UNITPER_PER_YEAR,
      *            ISNULL(LNFILE.LN_REPOCD,''),
      *            ISNULL(LNFILE.LN_BR_TFR_FG,''),
      *            LNFILE.LN_O3INS_DLPROC,
      *            LNFILE.LN_INSPREM_DLADJ,
      *            LNFILE.LN_PLAMT,
      *            LNFILE.LN_MTD_PRIN,
      *            LNFILE.LN_MTD_LC,
      *            LNFILE.LN_MTD_INT,
      *            LNFILE.LN_MTD_OT,
      *            LNFILE.LN_MTD_EX,
      *            LNFILE.LN_BNKRPT_CHAPTER,
      *            ISNULL(LNFILE.LN_BMO_CON_STATUS,''),
      *            ISNULL(LNFILE.LN_BMO_REC_STATUS,''),
      *            LNFILE.LN_APP_NUMBER,
      *            ISNULL(LNFILE.LN_GIFTNET,''),
      *            LNFILE.LN_GIFTNET_OUR_PO,
      *            ISNULL(LNFILE.LN_PROC_DISBURSED,''),
      *            ISNULL(LNFILE.LN_NOODDPY,''),
      *            LNFILE.LN_CONPROFILE_1,
      *            LNFILE.LN_CONPROFILE_2,
      *            LNFILE.LN_CONPROFILE_3,
      *            LNFILE.LN_CONPROFILE_4,
      *            LNFILE.LN_CONPROFILE_5,
      *            LNFILE.LN_RECPROFILE_1,
      *            LNFILE.LN_RECPROFILE_2,
      *            LNFILE.LN_RECPROFILE_3,
      *            LNFILE.LN_RECPROFILE_4,
      *            LNFILE.LN_RECPROFILE_5,
      *            ISNULL(LNFILE.LN_TRW_STATUS,''),
      *            ISNULL(LNFILE.LN_ORIGIN,''),
      *            ISNULL(LNFILE.LN_PO_TRCD,''),
      *            LNFILE.LN_NEXIS_CRSCORE,
      *            ISNULL(LNFILE.LN_TRW_B_CONSUMER_IND,''),
      *            ISNULL(LNFILE.LN_PARVHELD_PAID_FG,''),
      *            ISNULL(LNFILE.LN_ACCT_FROZEN,''),
      *            ISNULL(LNFILE.LN_TRW_SPECIAL_CODE,''),
      *            ISNULL(LNFILE.LN_CREATION_STATUS,''),
      *            LNFILE.LN_TOTNOLCHG,
      *            ISNULL(LNFILE.LN_LBOX,''),
      *            LNFILE.LN_EXTRA_PRIN_LFP,
      *            LNFILE.LN_REMAIN_INT_SINCE_LFP,
      *            LNFILE.LN_REMAIN_PRIN_SINCE_LFP,
      *            ISNULL(LNFILE.LN_INTERIM_COUPON,''),
      *            LNFILE.LN_DLCOMM_O3_CMSI,
      *            LNFILE.LN_DLCOMM_PP_CMSI,
      *            ISNULL(LNFILE.LN_TRW_J_CONSUMER_IND,''),
      *            ISNULL(LNFILE.LN_ACH,''),
      *            ISNULL(LNFILE.LN_1099_C_FG,''),
      *            ISNULL(LNFILE.LN_TRW_COMPLIANCE_CODE,''),
      *            ISNULL(LNFILE.LN_UNWIND_REASON_CODE,''),
      *            ISNULL(LNFILE.LN_PBI_PBD_FG,''),
      *            ISNULL(LNFILE.LN_OLD_ACCT_FORMAT_FG,''),
      *            ISNULL(LNFILE.LN_APPROVED_BY_USER,''),
      *            ISNULL(LNFILE.LN_APPROVED_BY_CODE,''),
      *            ISNULL(CAST(LNFILE.LN_APPROVED_BY_DATE
      *                                           AS VARCHAR(10)),'0'),
      *            LNFILE.LN_APPROVED_BY_TIME,
      *            ISNULL(CAST(LNFILE.LN_LTOUCH_DATE
      *                                           AS VARCHAR(10)),'0'),
      *            LNFILE.LN_LTOUCH_TIME
      *     FROM DBO.LNFILE
      *     WHERE LNFILE.LN_OWNBR = :QLN3-WBEG-OWNBR
      *       AND LNFILE.LN_CLASS > :QLN3-WBEG-CLASS
      *       AND LNFILE.LN_CLASS <= :QLN3-WEND-CLASS
      *       AND LNFILE.LN_ACCTNO > :QLN3-WBEG-ACCTNO
      *       AND LNFILE.LN_ACCTNO <= :QLN3-WEND-ACCTNO 
      *     ORDER BY LNFILE.LN_OWNBR,
      *              LNFILE.LN_CLASS,
      *              LNFILE.LN_ACCTNO
      *    END-EXEC.

      *    EXEC SQL
      *        OPEN LN3_GR_CURSOR
      *    END-EXEC.

      *    PERFORM SQL-IO-VALIDATION.

      *    MOVE ZEROES TO QLN3-WBEG-OWNBR
      *                   QLN3-WEND-OWNBR
      *                   QLN3-WBEG-CLASS
      *                   QLN3-WEND-CLASS
      *                   QLN3-WBEG-ACCTNO
      *                   QLN3-WEND-ACCTNO.

      *    MOVE STAT---GOOD TO LN3-GR-CURSOR-STAT.

      *-----------------------------------------------------------------
       READ-LN3-FILE.
           PERFORM READ-IT.
           MOVE LN-PATH-OWNBR  TO LN-OWNBR.
           MOVE LN-PATH-SQL    TO E-FILE.
           MOVE SPACES         TO E-KEYX.
           STRING LN-OWNBR, LN-CLASS,LN-ACCTNO
              DELIMITED BY " " INTO E-KEYX.

           MOVE LN-OWNBR  TO QLN-OWNBR.
           MOVE LN-CLASS  TO QLN-CLASS.
           MOVE LN-ACCTNO TO QLN-ACCTNO.

           EXEC SQL
            SELECT LNFILE.LN_OWNBR,
                   LNFILE.LN_ACCTNO,
                   LNFILE.LN_CLASS,
                   LNFILE.LN_SUBCLASS,
                   LNFILE.LN_LAWCODE,
                   ISNULL(LNFILE.LN_MAKERCD4,''),
                   LNFILE.LN_SSNO4,
                   ISNULL(LNFILE.LN_MAKERCD3,''),
                   LNFILE.LN_SSNO3,
                   ISNULL(LNFILE.LN_MAKERCD2,''),
                   LNFILE.LN_SSNO2,
                   ISNULL(LNFILE.LN_MAKERCD1,''),
                   LNFILE.LN_SSNO1,
                   ISNULL(LNFILE.LN_ORGST,''),
                   LNFILE.LN_SPRCLASS,
                   ISNULL(LNFILE.LN_POCD,''),
                   ISNULL(LNFILE.LN_PLCD,''),
                   ISNULL(LNFILE.LN_PLTYPE,''),
                   ISNULL(LNFILE.LN_STATUSFG,''),
                   ISNULL(LNFILE.LN_SALEFIN,''),
                   ISNULL(LNFILE.LN_SERVACCT,''),
                   ISNULL(LNFILE.LN_LOANTYPE,''),
                   ISNULL(LNFILE.LN_PAY_SCHLD_FG,''),
                   ISNULL(LNFILE.LN_STATFLAGS,''),
                   ISNULL(CAST(LNFILE.LN_ENTDATE AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_LOANDATE AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_INTDATE AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_ORIG_1STPYDATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_1STPYDATE AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_JDDATE AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_PLDATE AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_ACCELDATE AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_REDUDATE AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_LEGALDATE AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_BNKRPTDATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_DEBTAIDATE
                                                  AS VARCHAR(10)),'0'),
                   LNFILE.LN_APRATE,
                   LNFILE.LN_JDRATE,
                   LNFILE.LN_ORGTERM,
                   LNFILE.LN_1STPYAMT,
                   LNFILE.LN_NOREGPY,
                   LNFILE.LN_REGPYAMT,
                   LNFILE.LN_LASTPYAMT,
                   LNFILE.LN_PY_YYYYMM_RECDEL,
                   LNFILE.LN_MTD_PAYS_RECDEL,
                   LNFILE.LN_DLNO,
                   ISNULL(LNFILE.LN_DLRECOURSE,''),
                   LNFILE.LN_CURBAL,
                   LNFILE.LN_INTBAL,
                   LNFILE.LN_LCBAL,
                   LNFILE.LN_OTHBAL,
                   LNFILE.LN_OT2BAL,
                   LNFILE.LN_ANTICERN_1,
                   LNFILE.LN_ANTICERN_2,
                   LNFILE.LN_ANTICERN_3,
                   LNFILE.LN_ANTICERN_4,
                   LNFILE.LN_ANTICERN_5,
                   LNFILE.LN_ANTICERN_6,
                   LNFILE.LN_ANTICERN_7,
                   LNFILE.LN_ANTICERN_8,
                   LNFILE.LN_ANTICERN_9,
                   LNFILE.LN_ANTICERN_10,
                   LNFILE.LN_ANTICADJ_1,
                   LNFILE.LN_ANTICADJ_2,
                   LNFILE.LN_ANTICADJ_3,
                   LNFILE.LN_ANTICADJ_4,
                   LNFILE.LN_ANTICADJ_5,
                   LNFILE.LN_ANTICADJ_6,
                   LNFILE.LN_ANTICADJ_7,
                   LNFILE.LN_ANTICADJ_8,
                   LNFILE.LN_ANTICADJ_9,
                   LNFILE.LN_ANTICADJ_10,
                   LNFILE.LN_UFERN_1,
                   LNFILE.LN_UFERN_2,
                   LNFILE.LN_UFERN_3,
                   LNFILE.LN_UFERN_4,
                   LNFILE.LN_UFERN_5,
                   LNFILE.LN_UFERN_6,
                   LNFILE.LN_UFERN_7,
                   LNFILE.LN_UFERN_8,
                   LNFILE.LN_UFERN_9,
                   LNFILE.LN_UFERN_10,
                   LNFILE.LN_ACCERN_1,
                   LNFILE.LN_ACCERN_2,
                   LNFILE.LN_ACCERN_3,
                   LNFILE.LN_ACCERN_4,
                   LNFILE.LN_ACCERN_5,
                   LNFILE.LN_ACCERN_6,
                   LNFILE.LN_ACCERN_7,
                   LNFILE.LN_ACCERN_8,
                   LNFILE.LN_ACCERN_9,
                   LNFILE.LN_ACCERN_10,
                   LNFILE.LN_TOTPAYMNTD,
                   LNFILE.LN_TOTNODEF,
                   LNFILE.LN_TOTNOALDEL,
                   ISNULL(LNFILE.LN_UN_O10_REB,''),
                   ISNULL(LNFILE.LN_UN_O9_REB,''),
                   ISNULL(LNFILE.LN_UN_O8_REB,''),
                   ISNULL(LNFILE.LN_UN_O7_REB,''),
                   ISNULL(LNFILE.LN_O7_REBATE,''),
                   ISNULL(LNFILE.LN_O6_REBATE,''),
                   ISNULL(LNFILE.LN_O5_REBATE,''),
                   ISNULL(LNFILE.LN_O4_REBATE,''),
                   ISNULL(LNFILE.LN_O3_REBATE,''),
                   ISNULL(LNFILE.LN_O2_REBATE,''),
                   ISNULL(LNFILE.LN_O1_REBATE,''),
                   ISNULL(LNFILE.LN_EX_REBATE,''),
                   ISNULL(LNFILE.LN_DF_REBATE,''),
                   ISNULL(LNFILE.LN_MF_REBATE,''),
                   ISNULL(LNFILE.LN_SC_REBATE,''),
                   ISNULL(LNFILE.LN_INT_REBATE,''),
                   ISNULL(LNFILE.LN_PP_REBATE,''),
                   ISNULL(LNFILE.LN_AH_REBATE,''),
                   ISNULL(LNFILE.LN_CL_REBATE,''),
                   ISNULL(CAST(LNFILE.LN_DATE_PAID_LAST
                                                  AS VARCHAR(10)),'0'),
                   LNFILE.LN_SUM_LNAMT,
                   LNFILE.LN_SUM_ORGTERM,
                   ISNULL(LNFILE.LN_UNITPER_CD,''),
                   LNFILE.LN_UNITPER_FREQ,
                   ISNULL(LNFILE.LN_REPOCD,''),
                   LNFILE.LN_CONPROFILE_1,
                   LNFILE.LN_CONPROFILE_2,
                   LNFILE.LN_CONPROFILE_3,
                   LNFILE.LN_CONPROFILE_4,
                   LNFILE.LN_CONPROFILE_5,
                   LNFILE.LN_RECPROFILE_1,
                   LNFILE.LN_RECPROFILE_2,
                   LNFILE.LN_RECPROFILE_3,
                   LNFILE.LN_RECPROFILE_4,
                   LNFILE.LN_RECPROFILE_5
            INTO
                   :QLN-OWNBR,
                   :QLN-ACCTNO,
                   :QLN-CLASS,
                   :QLN-SUBCLASS,
                   :QLN-LAWCODE,
                   :QLN-MAKERCD4,
                   :QLN-SSNO4,
                   :QLN-MAKERCD3,
                   :QLN-SSNO3,
                   :QLN-MAKERCD2,
                   :QLN-SSNO2,
                   :QLN-MAKERCD1,
                   :QLN-SSNO1,
                   :QLN-ORGST,
                   :QLN-SPRCLASS,
                   :QLN-POCD,
                   :QLN-PLCD,
                   :QLN-PLTYPE,
                   :QLN-STATUSFG,
                   :QLN-SALEFIN,
                   :QLN-SERVACCT,
                   :QLN-LOANTYPE,
                   :QLN-PAY-SCHLD-FG,
                   :QLN-STATFLAGS,
                   :QLN-ENTDATE,
                   :QLN-LOANDATE,
                   :QLN-INTDATE,
                   :QLN-ORIG-1STPYDATE,
                   :QLN-1STPYDATE,
                   :QLN-JDDATE,
                   :QLN-PLDATE,
                   :QLN-ACCELDATE,
                   :QLN-REDUDATE,
                   :QLN-LEGALDATE,
                   :QLN-BNKRPTDATE,
                   :QLN-DEBTAIDATE,
                   :QLN-APRATE,
                   :QLN-JDRATE,
                   :QLN-ORGTERM,
                   :QLN-1STPYAMT,
                   :QLN-NOREGPY,
                   :QLN-REGPYAMT,
                   :QLN-LASTPYAMT,
                   :QLN-PY-YYYYMM-RECDEL,
                   :QLN-MTD-PAYS-RECDEL,
                   :QLN-DLNO,
                   :QLN-DLRECOURSE,
                   :QLN-CURBAL,
                   :QLN-INTBAL,
                   :QLN-LCBAL,
                   :QLN-OTHBAL,
                   :QLN-OT2BAL,
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
                   :QLN-ANTICADJ-1,
                   :QLN-ANTICADJ-2,
                   :QLN-ANTICADJ-3,
                   :QLN-ANTICADJ-4,
                   :QLN-ANTICADJ-5,
                   :QLN-ANTICADJ-6,
                   :QLN-ANTICADJ-7,
                   :QLN-ANTICADJ-8,
                   :QLN-ANTICADJ-9,
                   :QLN-ANTICADJ-10,
                   :QLN-UFERN-1,
                   :QLN-UFERN-2,
                   :QLN-UFERN-3,
                   :QLN-UFERN-4,
                   :QLN-UFERN-5,
                   :QLN-UFERN-6,
                   :QLN-UFERN-7,
                   :QLN-UFERN-8,
                   :QLN-UFERN-9,
                   :QLN-UFERN-10,
                   :QLN-ACCERN-1,
                   :QLN-ACCERN-2,
                   :QLN-ACCERN-3,
                   :QLN-ACCERN-4,
                   :QLN-ACCERN-5,
                   :QLN-ACCERN-6,
                   :QLN-ACCERN-7,
                   :QLN-ACCERN-8,
                   :QLN-ACCERN-9,
                   :QLN-ACCERN-10,
                   :QLN-TOTPAYMNTD,
                   :QLN-TOTNODEF,
                   :QLN-TOTNOALDEL,
                   :QLN-UN-O10-REB,
                   :QLN-UN-O9-REB,
                   :QLN-UN-O8-REB,
                   :QLN-UN-O7-REB,
                   :QLN-O7-REBATE,
                   :QLN-O6-REBATE,
                   :QLN-O5-REBATE,
                   :QLN-O4-REBATE,
                   :QLN-O3-REBATE,
                   :QLN-O2-REBATE,
                   :QLN-O1-REBATE,
                   :QLN-EX-REBATE,
                   :QLN-DF-REBATE,
                   :QLN-MF-REBATE,
                   :QLN-SC-REBATE,
                   :QLN-INT-REBATE,
                   :QLN-PP-REBATE,
                   :QLN-AH-REBATE,
                   :QLN-CL-REBATE,
                   :QLN-DATE-PAID-LAST,
                   :QLN-SUM-LNAMT,
                   :QLN-SUM-ORGTERM,
                   :QLN-UNITPER-CD,
                   :QLN-UNITPER-FREQ,
                   :QLN-REPOCD,
                   :QLN-CONPROFILE-1,
                   :QLN-CONPROFILE-2,
                   :QLN-CONPROFILE-3,
                   :QLN-CONPROFILE-4,
                   :QLN-CONPROFILE-5,
                   :QLN-RECPROFILE-1,
                   :QLN-RECPROFILE-2,
                   :QLN-RECPROFILE-3,
                   :QLN-RECPROFILE-4,
                   :QLN-RECPROFILE-5
            FROM DBO.LNFILE
            WHERE LNFILE.LN_OWNBR  = :QLN-OWNBR
              AND LNFILE.LN_CLASS  = :QLN-CLASS
              AND LNFILE.LN_ACCTNO = :QLN-ACCTNO
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-LN3-FILE.

           IF ( IO-FG = 0 )
              PERFORM GET-LN-FIELDS.

      *-----------------------------------------------------------------
       READ-LN3-FILE-NEXT.
           PERFORM READ-IT.
           MOVE LN-PATH-OWNBR  TO LN-OWNBR.
           MOVE LN-PATH-SQL    TO E-FILE.
           MOVE SPACES         TO E-KEYX.
           STRING LN-OWNBR, LN-CLASS,LN-ACCTNO
              DELIMITED BY " " INTO E-KEYX.
           INITIALIZE QLN-REC.

           IF ( LN3-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH LN3_CURSOR INTO
                   :QLN-OWNBR,
                   :QLN-ACCTNO,
                   :QLN-CLASS,
                   :QLN-SUBCLASS,
                   :QLN-LAWCODE,
                   :QLN-MAKERCD4,
                   :QLN-SSNO4,
                   :QLN-MAKERCD3,
                   :QLN-SSNO3,
                   :QLN-MAKERCD2,
                   :QLN-SSNO2,
                   :QLN-MAKERCD1,
                   :QLN-SSNO1,
                   :QLN-ORGST,
                   :QLN-SPRCLASS,
                   :QLN-POCD,
                   :QLN-PLCD,
                   :QLN-PLTYPE,
                   :QLN-STATUSFG,
                   :QLN-SALEFIN,
                   :QLN-SERVACCT,
                   :QLN-LOANTYPE,
                   :QLN-PAY-SCHLD-FG,
                   :QLN-STATFLAGS,
                   :QLN-ENTDATE,
                   :QLN-LOANDATE,
                   :QLN-INTDATE,
                   :QLN-ORIG-1STPYDATE,
                   :QLN-1STPYDATE,
                   :QLN-JDDATE,
                   :QLN-PLDATE,
                   :QLN-ACCELDATE,
                   :QLN-REDUDATE,
                   :QLN-LEGALDATE,
                   :QLN-BNKRPTDATE,
                   :QLN-DEBTAIDATE,
                   :QLN-APRATE,
                   :QLN-JDRATE,
                   :QLN-ORGTERM,
                   :QLN-1STPYAMT,
                   :QLN-NOREGPY,
                   :QLN-REGPYAMT,
                   :QLN-LASTPYAMT,
                   :QLN-PY-YYYYMM-RECDEL,
                   :QLN-MTD-PAYS-RECDEL,
                   :QLN-DLNO,
                   :QLN-DLRECOURSE,
                   :QLN-CURBAL,
                   :QLN-INTBAL,
                   :QLN-LCBAL,
                   :QLN-OTHBAL,
                   :QLN-OT2BAL,
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
                   :QLN-ANTICADJ-1,
                   :QLN-ANTICADJ-2,
                   :QLN-ANTICADJ-3,
                   :QLN-ANTICADJ-4,
                   :QLN-ANTICADJ-5,
                   :QLN-ANTICADJ-6,
                   :QLN-ANTICADJ-7,
                   :QLN-ANTICADJ-8,
                   :QLN-ANTICADJ-9,
                   :QLN-ANTICADJ-10,
                   :QLN-UFERN-1,
                   :QLN-UFERN-2,
                   :QLN-UFERN-3,
                   :QLN-UFERN-4,
                   :QLN-UFERN-5,
                   :QLN-UFERN-6,
                   :QLN-UFERN-7,
                   :QLN-UFERN-8,
                   :QLN-UFERN-9,
                   :QLN-UFERN-10,
                   :QLN-ACCERN-1,
                   :QLN-ACCERN-2,
                   :QLN-ACCERN-3,
                   :QLN-ACCERN-4,
                   :QLN-ACCERN-5,
                   :QLN-ACCERN-6,
                   :QLN-ACCERN-7,
                   :QLN-ACCERN-8,
                   :QLN-ACCERN-9,
                   :QLN-ACCERN-10,
                   :QLN-TOTPAYMNTD,
                   :QLN-TOTNODEF,
                   :QLN-TOTNOALDEL,
                   :QLN-UN-O10-REB,
                   :QLN-UN-O9-REB,
                   :QLN-UN-O8-REB,
                   :QLN-UN-O7-REB,
                   :QLN-O7-REBATE,
                   :QLN-O6-REBATE,
                   :QLN-O5-REBATE,
                   :QLN-O4-REBATE,
                   :QLN-O3-REBATE,
                   :QLN-O2-REBATE,
                   :QLN-O1-REBATE,
                   :QLN-EX-REBATE,
                   :QLN-DF-REBATE,
                   :QLN-MF-REBATE,
                   :QLN-SC-REBATE,
                   :QLN-INT-REBATE,
                   :QLN-PP-REBATE,
                   :QLN-AH-REBATE,
                   :QLN-CL-REBATE,
                   :QLN-DATE-PAID-LAST,
                   :QLN-SUM-LNAMT,
                   :QLN-SUM-ORGTERM,
                   :QLN-UNITPER-CD,
                   :QLN-UNITPER-FREQ,
                   :QLN-REPOCD,
                   :QLN-CONPROFILE-1,
                   :QLN-CONPROFILE-2,
                   :QLN-CONPROFILE-3,
                   :QLN-CONPROFILE-4,
                   :QLN-CONPROFILE-5,
                   :QLN-RECPROFILE-1,
                   :QLN-RECPROFILE-2,
                   :QLN-RECPROFILE-3,
                   :QLN-RECPROFILE-4,
                   :QLN-RECPROFILE-5
              END-EXEC
      *    ELSE
      *    IF ( LN3-GR-CURSOR-STAT-GOOD )
      *       EXEC SQL
      *            FETCH LN3_GR_CURSOR INTO :QLN-REC
      *       END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-LN3-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-LN-FIELDS.

      *-----------------------------------------------------------------
       CLOSE-LN3-FILE.

           IF ( LN3-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE LN3_CURSOR
              END-EXEC
              MOVE STAT---BAD TO LN3-CURSOR-STAT.

      *    IF ( LN3-GR-CURSOR-STAT-GOOD )
      *       EXEC SQL
      *            CLOSE LN3_GR_CURSOR
      *       END-EXEC
      *       MOVE STAT---BAD TO LN3-GR-CURSOR-STAT.

      *COPY "LIBLP/LPBW1RN.CPY".
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

      *-----------------------------------------------------------------
       OPEN-BW1-FILE.
           PERFORM LOAD-BW1-FILE.
           PERFORM OPEN-IT.
           MOVE BW-PATH-SQL TO E-FILE.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       START-BW1-FILE.
           PERFORM START-IT.
           MOVE BW-PATH-OWNBR  TO BW-OWNBR.
           MOVE BW-PATH-SQL    TO E-FILE.
           MOVE BW1-KEY        TO E-KEYX.

           IF ( BW1-CURSOR-STAT-GOOD )
              PERFORM CLOSE-BW1-FILE.

           IF ( QBW1-WEND-SSNO = ZEROES    ) OR
              ( QBW1-WEND-SSNO NOT NUMERIC )
              MOVE BW-OWNBR             TO QBW1-WBEG-OWNBR
                                           QBW1-WEND-OWNBR
              MOVE BW-SSNO              TO QBW1-WBEG-SSNO
              MOVE ALL "9"              TO QBW1-WEND-SSNO.

           EXEC SQL
            DECLARE BW1_CURSOR CURSOR FOR
            SELECT BWFILE.BW_SSNO,
                   ISNULL(BWFILE.BW_LNAME,''),
                   ISNULL(BWFILE.BW_FNAME,''),
                   ISNULL(CAST(BWFILE.BW_REFDATE AS VARCHAR(10)),'0'),
                   ISNULL(BWFILE.BW_SOLICIT,'')
            FROM DBO.BWFILE
            WHERE BWFILE.BW_OWNBR >= :QBW1-WBEG-OWNBR
              AND BWFILE.BW_OWNBR <= :QBW1-WEND-OWNBR
              AND BWFILE.BW_SSNO >= :QBW1-WBEG-SSNO
              AND BWFILE.BW_SSNO <= :QBW1-WEND-SSNO
              ORDER BY BWFILE.BW_OWNBR,
                       BWFILE.BW_SSNO
           END-EXEC.

           EXEC SQL
               OPEN BW1_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE ZEROES TO QBW1-WBEG-OWNBR 
                          QBW1-WEND-OWNBR
                          QBW1-WBEG-SSNO
                          QBW1-WEND-SSNO.

           MOVE STAT---GOOD TO BW1-CURSOR-STAT.

      *-----------------------------------------------------------------
       READ-BW1-FILE.
           PERFORM READ-IT.
           MOVE BW-PATH-OWNBR  TO BW-OWNBR. 
           MOVE BW-PATH-SQL    TO E-FILE.
           MOVE BW1-KEY        TO E-KEYX.

           MOVE BW-OWNBR   TO QBW-OWNBR.
           MOVE BW-SSNO    TO QBW-SSNO.

           EXEC SQL
            SELECT BWFILE.BW_SSNO,
                   ISNULL(BWFILE.BW_LNAME,''),
                   ISNULL(BWFILE.BW_FNAME,''),
                   ISNULL(CAST(BWFILE.BW_REFDATE AS VARCHAR(10)),'0'),
                   ISNULL(BWFILE.BW_SOLICIT,'')
            INTO
                   :QBW-SSNO,
                   :QBW-LNAME,
                   :QBW-FNAME,
                   :QBW-REFDATE,
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
       READ-BW1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE BW-PATH-OWNBR  TO BW-OWNBR.
           MOVE BW-PATH-SQL    TO E-FILE.
           MOVE BW1-KEY        TO E-KEYX.
           INITIALIZE QBW-REC.

           IF ( BW1-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH BW1_CURSOR INTO
                   :QBW-SSNO,
                   :QBW-LNAME,
                   :QBW-FNAME,
                   :QBW-REFDATE,
                   :QBW-SOLICIT
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-BW1-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-BW-FIELDS.

      *-----------------------------------------------------------------
       CLOSE-BW1-FILE.

           IF ( BW1-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE BW1_CURSOR
              END-EXEC
              MOVE STAT---BAD TO BW1-CURSOR-STAT.

      *COPY "LIBLP/LPLC1RN.CPY".
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
       START-LC1-FILE.
           PERFORM START-IT.
           MOVE LC-PATH-SQL TO E-FILE.
           MOVE LC1-KEY TO E-KEYX.

           IF ( LC1-CURSOR-STAT-GOOD )
              PERFORM CLOSE-LC1-FILE.

           IF ( QLC1-WEND-CODE = ZEROES    ) OR
              ( QLC1-WEND-CODE NOT NUMERIC )
              MOVE LC-CODE              TO QLC1-WBEG-CODE
              MOVE ALL "9"              TO QLC1-WEND-CODE.

           EXEC SQL
            DECLARE LC1_CURSOR CURSOR FOR
            SELECT LCFILE.LC_CODE,
                   ISNULL(LCFILE.LC_DESC,'')
            FROM DBO.LCFILE
            WHERE LCFILE.LC_CODE >= :QLC1-WBEG-CODE
              AND LCFILE.LC_CODE <= :QLC1-WEND-CODE
            ORDER BY LCFILE.LC_CODE
           END-EXEC.

           EXEC SQL
               OPEN LC1_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE ZEROES TO QLC1-WBEG-CODE 
                          QLC1-WEND-CODE.

           MOVE STAT---GOOD TO LC1-CURSOR-STAT.

      *-----------------------------------------------------------------
       START-LC1-FILE-GREATER.
           PERFORM START-IT.
           MOVE LC-PATH-SQL TO E-FILE.
           MOVE LC1-KEY TO E-KEYX.

           IF ( LC1-GR-CURSOR-STAT-GOOD )
              PERFORM CLOSE-LC1-FILE.

           IF ( QLC1-WEND-CODE = ZEROES    ) OR
              ( QLC1-WEND-CODE NOT NUMERIC )
              MOVE LC-CODE              TO QLC1-WBEG-CODE
              MOVE ALL "9"              TO QLC1-WEND-CODE.

           EXEC SQL
            DECLARE LC1_GR_CURSOR CURSOR FOR
            SELECT LCFILE.LC_CODE,
                   ISNULL(LCFILE.LC_DESC,'')
            FROM DBO.LCFILE
            WHERE LCFILE.LC_CODE >  :QLC1-WBEG-CODE
              AND LCFILE.LC_CODE <= :QLC1-WEND-CODE
            ORDER BY LCFILE.LC_CODE
           END-EXEC.

           EXEC SQL
               OPEN LC1_GR_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE ZEROES TO QLC1-WBEG-CODE 
                          QLC1-WEND-CODE.

           MOVE STAT---GOOD TO LC1-GR-CURSOR-STAT.

      *-----------------------------------------------------------------
       READ-LC1-FILE.
           PERFORM READ-IT.
           MOVE LC-PATH-SQL TO E-FILE.
           MOVE LC1-KEY TO E-KEYX.

           MOVE LC-CODE TO QLC-CODE.

           EXEC SQL
            SELECT LCFILE.LC_CODE,
                   ISNULL(LCFILE.LC_DESC,'')
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
      *    STILL LOADING FILE WORKERS LIKE E-WORKERS
      *-----------------------------------------------------------------
       READ-LC1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE LC-PATH-SQL TO E-FILE.
           MOVE LC1-KEY TO E-KEYX.
           INITIALIZE QLC-REC.

           IF ( LC1-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH LC1_CURSOR INTO
                   :QLC-CODE,
                   :QLC-DESC
              END-EXEC
           ELSE
           IF ( LC1-GR-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH LC1_GR_CURSOR INTO
                   :QLC-CODE,
                   :QLC-DESC
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO READ-LC1-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-LC-FIELDS.

      *-----------------------------------------------------------------
       CLOSE-LC1-FILE.

           IF ( LC1-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE LC1_CURSOR
              END-EXEC
              MOVE STAT---BAD TO LC1-CURSOR-STAT.

           IF ( LC1-GR-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE LC1_GR_CURSOR
              END-EXEC
              MOVE STAT---BAD TO LC1-GR-CURSOR-STAT.


      *COPY "LIBLP/LPSP1RN.CPY".
      * COPYMEMBER: LIBLP/LPSP1RN
      ******************************************************************
       SET-ALL-TO-SP1.
           MOVE SPA1-KEY   TO SP1-KEY.
           MOVE SPA-AREA-A TO SP-AREA-A.
           MOVE SPB-AREA-B TO SP-AREA-B.
           MOVE SPC-AREA-C TO SP-AREA-C.

       SET-SP1-TO-ALL.
           MOVE SP1-KEY   TO SPA1-KEY.
           MOVE SP-AREA-A TO SPA-AREA-A.
           MOVE SP1-KEY   TO SPB1-KEY.
           MOVE SP-AREA-B TO SPB-AREA-B.
           MOVE SP1-KEY   TO SPC1-KEY.
           MOVE SP-AREA-C TO SPC-AREA-C.

       LOAD-SP1-FILE.
           PERFORM LOAD-SPA1-FILE.
           PERFORM LOAD-SPB1-FILE.
           PERFORM LOAD-SPC1-FILE.

      *-----------------------------------------------------------------
       OPEN-SP1-FILE.
           PERFORM OPEN-SPA1-FILE.
           IF ( IO-FG = 0 )
               PERFORM OPEN-SPB1-FILE
               IF ( IO-FG = 0 )
                  PERFORM OPEN-SPC1-FILE
                  IF ( IO-FG = 0 )
                     MOVE " " TO IO-MULTI-FG
                  ELSE
                     MOVE "C" TO IO-MULTI-FG
               ELSE
                  MOVE "B" TO IO-MULTI-FG
            ELSE
               MOVE "A" TO IO-MULTI-FG.

      *-----------------------------------------------------------------
       START-SP1-FILE.
           PERFORM SET-SP1-TO-ALL.
           PERFORM START-SPA1-FILE.
           IF ( IO-FG NOT = 0 )
              MOVE "A" TO IO-MULTI-FG.
             
      *-----------------------------------------------------------------
       READ-SP1-FILE.
           PERFORM SET-SP1-TO-ALL.
           PERFORM READ-SPA1-FILE.
      *    IF ( IO-FG = 0 )
      *       PERFORM READ-SPB1-FILE
      *       IF ( IO-FG = 0 )
      *          PERFORM READ-SPC1-FILE
      *          IF ( IO-FG = 0 )
      *             PERFORM SET-ALL-TO-SP1
      *          ELSE
      *             MOVE "C" TO IO-MULTI-FG
      *       ELSE
      *          MOVE "B" TO IO-MULTI-FG
      *    ELSE
      *       MOVE "A" TO IO-MULTI-FG.

      *-----------------------------------------------------------------
       READ-SP1-FILE-NEXT.
           PERFORM SET-SP1-TO-ALL.
           PERFORM READ-SPA1-FILE-NEXT.
           IF ( IO-FG = 0 )
              MOVE SPA1-KEY TO SPB1-KEY
              PERFORM READ-SPB1-FILE
              IF ( IO-FG = 0 )
                 MOVE SPA1-KEY TO SPC1-KEY
                 PERFORM READ-SPC1-FILE
                 IF ( IO-FG = 0 )
                    PERFORM SET-ALL-TO-SP1
                 ELSE
                    MOVE "C" TO IO-MULTI-FG
              ELSE
                 MOVE "B" TO IO-MULTI-FG
           ELSE
              MOVE "A" TO IO-MULTI-FG.

      *-----------------------------------------------------------------
       CLOSE-SP1-FILE.
           PERFORM CLOSE-SPA1-FILE.
           PERFORM CLOSE-SPB1-FILE.
           PERFORM CLOSE-SPC1-FILE.

      *================================================================E
       LOAD-SPA1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( SP-PATH-OVERRIDE = "Y" )
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( SP-PATH-OVERRIDE = SPACES )
              MOVE EXT-FILPATH-BASE        TO SPA-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO SPA-ALL-MACHINE
              MOVE SPA-ALL-PATH             TO SPA-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( SP-PATH-OVERRIDE = "B" )
              MOVE FILPATH-BASE            TO SPA-ALL-BASE
              MOVE FILPATH-MACHINE         TO SPA-ALL-MACHINE
              MOVE SPA-ALL-PATH             TO SPA-PATH.

      *-----------------------------------------------------------------
       OPEN-SPA1-FILE.
           PERFORM LOAD-SPA1-FILE.
           PERFORM OPEN-IT.
           MOVE SPA-PATH-SQL TO E-FILE.
      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       START-SPA1-FILE.
           PERFORM START-IT.
           MOVE SPA-PATH-SQL TO E-FILE.
           MOVE SPA1-KEY TO E-KEYX.

           IF ( SPA1-CURSOR-STAT-GOOD )
              PERFORM CLOSE-SPA1-FILE.

           IF ( QSP1-WEND-LAWCODE = 0  ) OR
              ( QSP1-WEND-LAWCODE NOT NUMERIC)
              MOVE SPA-ORGST            TO QSP1-WBEG-ORGST 
              MOVE ALL "Z"              TO QSP1-WEND-ORGST 
              MOVE SPA-SPRCLASS         TO QSP1-WBEG-SPRCLASS
              MOVE ALL "9"              TO QSP1-WEND-SPRCLASS
              MOVE SPA-SUBCLASS         TO QSP1-WBEG-SUBCLASS
              MOVE ALL "9"              TO QSP1-WEND-SUBCLASS
              MOVE SPA-LAWCODE          TO QSP1-WBEG-LAWCODE
              MOVE ALL "9"              TO QSP1-WEND-LAWCODE.

           EXEC SQL
            DECLARE SPA1_CURSOR CURSOR FOR
            SELECT ISNULL(SPAFILE.SPA_ORGST,''),
                   SPAFILE.SPA_SPRCLASS,
                   SPAFILE.SPA_SUBCLASS,
                   SPAFILE.SPA_LAWCODE,
                   SPAFILE.SPA_EXPD_MONTHS,
                   SPAFILE.SPA_EXPD_DAYS,
                   ISNULL(SPAFILE.SPA_EXPD_FRMLA,''),
                   ISNULL(SPAFILE.SPA_EXPR_FRMLA,''),
                   ISNULL(SPAFILE.SPA_EXPR_REDU_FRMLA,''),
                   ISNULL(SPAFILE.SPA_JUDGFRMLA,''),
                   ISNULL(SPAFILE.SPA_ACC_FRMLA,''),
                   ISNULL(SPAFILE.SPA_BANKRULE,''),
                   SPAFILE.SPA_CONTPDTH,
                   ISNULL(SPAFILE.SPA_BANKUNITPER_CD,''),
                   ISNULL(SPAFILE.SPA_TRWBANKRULE,''),
                   SPAFILE.SPA_TRWCONTPDTH,
                   ISNULL(SPAFILE.SPA_TRWBANKUNITPER_CD,''),
                   ISNULL(SPAFILE.SPA_RECFRMLA,''),
                   SPAFILE.SPA_RECPDTH,
                   ISNULL(SPAFILE.SPA_LCUNITPER_CD,''),
                   ISNULL(SPAFILE.SPA_DEFUNITPER_CD,''),
                   SPCFILE.SPC_CONTR_CREDIT,
                   ISNULL(SPCFILE.SPC_CONTR_CREDIT_CD,''),
                   SPCFILE.SPC_CONTR_CREDIT_MIN_DEL_PER,
                   ISNULL(SPCFILE.SPC_CPOT30_CD,''),
                   ISNULL(SPCFILE.SPC_CPOTCHGOFF_CD,''),
                   ISNULL(SPCFILE.SPC_RDPOTCHGOFF_CD,''),
                   SPCFILE.SPC_BANKRULE_RATE
            FROM DBO.SPAFILE
            LEFT JOIN DBO.SPBFILE
              ON (SPAFILE.SPA_ORGST    = SPBFILE.SPB_ORGST
              AND SPAFILE.SPA_SPRCLASS = SPBFILE.SPB_SPRCLASS
              AND SPAFILE.SPA_SUBCLASS = SPBFILE.SPB_SUBCLASS
              AND SPAFILE.SPA_LAWCODE  = SPBFILE.SPB_LAWCODE)
            LEFT JOIN DBO.SPCFILE
              ON (SPBFILE.SPB_ORGST    = SPCFILE.SPC_ORGST
              AND SPBFILE.SPB_SPRCLASS = SPCFILE.SPC_SPRCLASS
              AND SPBFILE.SPB_SUBCLASS = SPCFILE.SPC_SUBCLASS
              AND SPBFILE.SPB_LAWCODE  = SPCFILE.SPC_LAWCODE)
            WHERE SPAFILE.SPA_ORGST    >= :QSP1-WBEG-ORGST
              AND SPAFILE.SPA_ORGST    <= :QSP1-WEND-ORGST
              AND SPAFILE.SPA_SPRCLASS >= :QSP1-WBEG-SPRCLASS
              AND SPAFILE.SPA_SPRCLASS <= :QSP1-WEND-SPRCLASS
              AND SPAFILE.SPA_SUBCLASS >= :QSP1-WBEG-SUBCLASS
              AND SPAFILE.SPA_SUBCLASS <= :QSP1-WEND-SUBCLASS
              AND SPAFILE.SPA_LAWCODE  >= :QSP1-WBEG-LAWCODE
              AND SPAFILE.SPA_LAWCODE  <= :QSP1-WEND-LAWCODE
            ORDER BY SPAFILE.SPA_ORGST,
                     SPAFILE.SPA_SPRCLASS,
                     SPAFILE.SPA_SUBCLASS,
                     SPAFILE.SPA_LAWCODE
           END-EXEC.

           EXEC SQL
               OPEN SPA1_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE SPACES TO QSP1-WBEG-ORGST
                          QSP1-WEND-ORGST.

           MOVE ZEROES TO QSP1-WBEG-SPRCLASS
                          QSP1-WEND-SPRCLASS
                          QSP1-WBEG-SUBCLASS
                          QSP1-WEND-SUBCLASS
                          QSP1-WBEG-LAWCODE
                          QSP1-WEND-LAWCODE.

           MOVE STAT---GOOD TO SPA1-CURSOR-STAT.

      *-----------------------------------------------------------------
       READ-SPA1-FILE.
           PERFORM READ-IT.
           MOVE SPA-PATH-SQL TO E-FILE.
           MOVE SPA1-KEY TO E-KEYX.

           MOVE SPA-ORGST            TO QSPA-ORGST.
           MOVE SPA-SPRCLASS         TO QSPA-SPRCLASS.
           MOVE SPA-SUBCLASS         TO QSPA-SUBCLASS.
           MOVE SPA-LAWCODE          TO QSPA-LAWCODE.

           EXEC SQL
            SELECT ISNULL(SPAFILE.SPA_ORGST,''),
                   SPAFILE.SPA_SPRCLASS,
                   SPAFILE.SPA_SUBCLASS,
                   SPAFILE.SPA_LAWCODE,
                   SPAFILE.SPA_EXPD_MONTHS,
                   SPAFILE.SPA_EXPD_DAYS,
                   ISNULL(SPAFILE.SPA_EXPD_FRMLA,''),
                   ISNULL(SPAFILE.SPA_EXPR_FRMLA,''),
                   ISNULL(SPAFILE.SPA_EXPR_REDU_FRMLA,''),
                   ISNULL(SPAFILE.SPA_JUDGFRMLA,''),
                   ISNULL(SPAFILE.SPA_ACC_FRMLA,''),
                   ISNULL(SPAFILE.SPA_BANKRULE,''),
                   SPAFILE.SPA_CONTPDTH,
                   ISNULL(SPAFILE.SPA_BANKUNITPER_CD,''),
                   ISNULL(SPAFILE.SPA_TRWBANKRULE,''),
                   SPAFILE.SPA_TRWCONTPDTH,
                   ISNULL(SPAFILE.SPA_TRWBANKUNITPER_CD,''),
                   ISNULL(SPAFILE.SPA_RECFRMLA,''),
                   SPAFILE.SPA_RECPDTH,
                   ISNULL(SPAFILE.SPA_LCUNITPER_CD,''),
                   ISNULL(SPAFILE.SPA_DEFUNITPER_CD,''),
                   SPCFILE.SPC_CONTR_CREDIT,
                   ISNULL(SPCFILE.SPC_CONTR_CREDIT_CD,''),
                   SPCFILE.SPC_CONTR_CREDIT_MIN_DEL_PER,
                   ISNULL(SPCFILE.SPC_CPOT30_CD,''),
                   ISNULL(SPCFILE.SPC_CPOTCHGOFF_CD,''),
                   ISNULL(SPCFILE.SPC_RDPOTCHGOFF_CD,''),
                   SPCFILE.SPC_BANKRULE_RATE
            INTO
                   :QSPA-ORGST,
                   :QSPA-SPRCLASS,
                   :QSPA-SUBCLASS,
                   :QSPA-LAWCODE,
                   :QSPA-EXPD-MONTHS,
                   :QSPA-EXPD-DAYS,
                   :QSPA-EXPD-FRMLA,
                   :QSPA-EXPR-FRMLA,
                   :QSPA-EXPR-REDU-FRMLA,
                   :QSPA-JUDGFRMLA,
                   :QSPA-ACC-FRMLA,
                   :QSPA-BANKRULE,
                   :QSPA-CONTPDTH,
                   :QSPA-BANKUNITPER-CD,
                   :QSPA-TRWBANKRULE,
                   :QSPA-TRWCONTPDTH,
                   :QSPA-TRWBANKUNITPER-CD,
                   :QSPA-RECFRMLA,
                   :QSPA-RECPDTH,
                   :QSPA-LCUNITPER-CD,
                   :QSPA-DEFUNITPER-CD,
                   :QSPC-CONTR-CREDIT,
                   :QSPC-CONTR-CREDIT-CD,
                   :QSPC-CONTR-CREDIT-MIN-DEL-PER,
                   :QSPC-CPOT30-CD,
                   :QSPC-CPOTCHGOFF-CD,
                   :QSPC-RDPOTCHGOFF-CD,
                   :QSPC-BANKRULE-RATE
            FROM DBO.SPAFILE
            LEFT JOIN DBO.SPBFILE
              ON (SPAFILE.SPA_ORGST    = SPBFILE.SPB_ORGST
              AND SPAFILE.SPA_SPRCLASS = SPBFILE.SPB_SPRCLASS
              AND SPAFILE.SPA_SUBCLASS = SPBFILE.SPB_SUBCLASS
              AND SPAFILE.SPA_LAWCODE  = SPBFILE.SPB_LAWCODE)
            LEFT JOIN DBO.SPCFILE
              ON (SPBFILE.SPB_ORGST    = SPCFILE.SPC_ORGST
              AND SPBFILE.SPB_SPRCLASS = SPCFILE.SPC_SPRCLASS
              AND SPBFILE.SPB_SUBCLASS = SPCFILE.SPC_SUBCLASS
              AND SPBFILE.SPB_LAWCODE  = SPCFILE.SPC_LAWCODE)
            WHERE SPAFILE.SPA_ORGST = :QSPA-ORGST
              AND SPAFILE.SPA_SPRCLASS = :QSPA-SPRCLASS
              AND SPAFILE.SPA_SUBCLASS = :QSPA-SUBCLASS
              AND SPAFILE.SPA_LAWCODE = :QSPA-LAWCODE 
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-SPA1-FILE.

           IF ( IO-FG = 0 )
              PERFORM GET-SPA-FIELDS.

      *-----------------------------------------------------------------
       READ-SPA1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE SPA-PATH-SQL TO E-FILE.
           MOVE SPA1-KEY TO E-KEYX.
           INITIALIZE QSPA-REC.

           IF ( SPA1-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH SP1_CURSOR INTO
                   :QSPA-ORGST,
                   :QSPA-SPRCLASS,
                   :QSPA-SUBCLASS,
                   :QSPA-LAWCODE,
                   :QSPA-EXPD-MONTHS,
                   :QSPA-EXPD-DAYS,
                   :QSPA-EXPD-FRMLA,
                   :QSPA-EXPR-FRMLA,
                   :QSPA-EXPR-REDU-FRMLA,
                   :QSPA-JUDGFRMLA,
                   :QSPA-ACC-FRMLA,
                   :QSPA-BANKRULE,
                   :QSPA-CONTPDTH,
                   :QSPA-BANKUNITPER-CD,
                   :QSPA-TRWBANKRULE,
                   :QSPA-TRWCONTPDTH,
                   :QSPA-TRWBANKUNITPER-CD,
                   :QSPA-RECFRMLA,
                   :QSPA-RECPDTH,
                   :QSPA-LCUNITPER-CD,
                   :QSPA-DEFUNITPER-CD,
                   :QSPC-CONTR-CREDIT,
                   :QSPC-CONTR-CREDIT-CD,
                   :QSPC-CONTR-CREDIT-MIN-DEL-PER,
                   :QSPC-CPOT30-CD,
                   :QSPC-CPOTCHGOFF-CD,
                   :QSPC-RDPOTCHGOFF-CD,
                   :QSPC-BANKRULE-RATE
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-SPA1-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-SPA-FIELDS.

      *-----------------------------------------------------------------
       CLOSE-SPA1-FILE.
           IF ( SPA1-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE SPA1_CURSOR
              END-EXEC
              MOVE STAT---BAD TO SPA1-CURSOR-STAT.

      *=================================================================
       LOAD-SPB1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( SP-PATH-OVERRIDE = "Y" )
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( SP-PATH-OVERRIDE = SPACES )
              MOVE EXT-FILPATH-BASE        TO SPB-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO SPB-ALL-MACHINE
              MOVE SPB-ALL-PATH             TO SPB-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( SP-PATH-OVERRIDE = "B" )
              MOVE FILPATH-BASE            TO SPB-ALL-BASE
              MOVE FILPATH-MACHINE         TO SPB-ALL-MACHINE
              MOVE SPB-ALL-PATH             TO SPB-PATH.

      *-----------------------------------------------------------------
       OPEN-SPB1-FILE.
           PERFORM LOAD-SPB1-FILE.
           PERFORM OPEN-IT.
           MOVE SPB-PATH-SQL TO E-FILE.

      *-----------------------------------------------------------------
       READ-SPB1-FILE.
           PERFORM READ-IT.
           MOVE SPB-PATH-SQL TO E-FILE.
           MOVE SPB1-KEY TO E-KEYX.

           MOVE SPB-ORGST            TO QSPB-ORGST.
           MOVE SPB-SPRCLASS         TO QSPB-SPRCLASS.
           MOVE SPB-SUBCLASS         TO QSPB-SUBCLASS.
           MOVE SPB-LAWCODE          TO QSPB-LAWCODE.

           EXEC SQL
            SELECT ISNULL(SPBFILE.SPB_ORGST,''),
                   SPBFILE.SPB_SPRCLASS,
                   SPBFILE.SPB_SUBCLASS,
                   SPBFILE.SPB_LAWCODE,
                   SPBFILE.SPB_INSCOM_1,
                   SPBFILE.SPB_INSRATECD_1,
                   SPBFILE.SPB_INSJFAC_1,
                   SPBFILE.SPB_INSDISCNT_1,
                   ISNULL(SPBFILE.SPB_INSCHGABL_1,''),
                   SPBFILE.SPB_INSTERM_1_1,
                   SPBFILE.SPB_INSRATE_1_1,
                   SPBFILE.SPB_INSTERM_1_2,
                   SPBFILE.SPB_INSRATE_1_2,
                   SPBFILE.SPB_INSTERM_1_3,
                   SPBFILE.SPB_INSRATE_1_3,
                   SPBFILE.SPB_INSTERM_1_4,
                   SPBFILE.SPB_INSRATE_1_4,
                   SPBFILE.SPB_INSTERM_1_5,
                   SPBFILE.SPB_INSRATE_1_5,
                   SPBFILE.SPB_INSTERM_1_6,
                   SPBFILE.SPB_INSRATE_1_6,
                   SPBFILE.SPB_INSTERM_1_7,
                   SPBFILE.SPB_INSRATE_1_7,
                   SPBFILE.SPB_INSTERM_1_8,
                   SPBFILE.SPB_INSRATE_1_8,
                   SPBFILE.SPB_INSTERM_1_9,
                   SPBFILE.SPB_INSRATE_1_9,
                   SPBFILE.SPB_INSTERM_1_10,
                   SPBFILE.SPB_INSRATE_1_10,
                   SPBFILE.SPB_INSTERM_1_11,
                   SPBFILE.SPB_INSRATE_1_11,
                   SPBFILE.SPB_INSTERM_1_12,
                   SPBFILE.SPB_INSRATE_1_12,
                   SPBFILE.SPB_INSTERM_1_13,
                   SPBFILE.SPB_INSRATE_1_13,
                   SPBFILE.SPB_INSTERM_1_14,
                   SPBFILE.SPB_INSRATE_1_14,
                   SPBFILE.SPB_INSTERM_1_15,
                   SPBFILE.SPB_INSRATE_1_15,
                   SPBFILE.SPB_INSTERM_1_16,
                   SPBFILE.SPB_INSRATE_1_16,
                   SPBFILE.SPB_INSTERM_1_17,
                   SPBFILE.SPB_INSRATE_1_17,
                   SPBFILE.SPB_INSTERM_1_18,
                   SPBFILE.SPB_INSRATE_1_18,
                   SPBFILE.SPB_INSTERM_1_19,
                   SPBFILE.SPB_INSRATE_1_19,
                   SPBFILE.SPB_INSTERM_1_20,
                   SPBFILE.SPB_INSRATE_1_20,
                   SPBFILE.SPB_INSMINPREM_1,
                   SPBFILE.SPB_INSMINCHGABL_1,
                   SPBFILE.SPB_INSMAXCHGABL_1,
                   SPBFILE.SPB_INSMAXBENIF_1,
                   SPBFILE.SPB_INSMAXTERM_1,
                   ISNULL(SPBFILE.SPB_INSCHGBEN_1,''),
                   SPBFILE.SPB_INSEXCEPT_1,
                   SPBFILE.SPB_INSNOVER_1,
                   SPBFILE.SPB_INSMINAGE_1_1,
                   SPBFILE.SPB_INSMAXAGE_1_1,
                   SPBFILE.SPB_INSMAXCOV_1_1,
                   SPBFILE.SPB_INSMAXTRM_1_1,
                   SPBFILE.SPB_INSMINAGE_1_2,
                   SPBFILE.SPB_INSMAXAGE_1_2,
                   SPBFILE.SPB_INSMAXCOV_1_2,
                   SPBFILE.SPB_INSMAXTRM_1_2,
                   SPBFILE.SPB_INSMINAGE_1_3,
                   SPBFILE.SPB_INSMAXAGE_1_3,
                   SPBFILE.SPB_INSMAXCOV_1_3,
                   SPBFILE.SPB_INSMAXTRM_1_3,
                   SPBFILE.SPB_INSMINAGE_1_4,
                   SPBFILE.SPB_INSMAXAGE_1_4,
                   SPBFILE.SPB_INSMAXCOV_1_4,
                   SPBFILE.SPB_INSMAXTRM_1_4,
                   SPBFILE.SPB_INSMINAGE_1_5,
                   SPBFILE.SPB_INSMAXAGE_1_5,
                   SPBFILE.SPB_INSMAXCOV_1_5,
                   SPBFILE.SPB_INSMAXTRM_1_5,
                   ISNULL(SPBFILE.SPB_INSFEECD_1,''),
                   ISNULL(SPBFILE.SPB_INSFEE_FRMLA_1,''),
                   ISNULL(SPBFILE.SPB_INSCHGABL_FG_1,''),
                   ISNULL(SPBFILE.SPB_INSREJECT_FG_1,''),
                   ISNULL(SPBFILE.SPB_INSFINCHG_1,''),
                   SPBFILE.SPB_INSCOM_2,
                   SPBFILE.SPB_INSRATECD_2,
                   SPBFILE.SPB_INSJFAC_2,
                   SPBFILE.SPB_INSDISCNT_2,
                   ISNULL(SPBFILE.SPB_INSCHGABL_2,''),
                   SPBFILE.SPB_INSTERM_2_1,
                   SPBFILE.SPB_INSRATE_2_1,
                   SPBFILE.SPB_INSTERM_2_2,
                   SPBFILE.SPB_INSRATE_2_2,
                   SPBFILE.SPB_INSTERM_2_3,
                   SPBFILE.SPB_INSRATE_2_3,
                   SPBFILE.SPB_INSTERM_2_4,
                   SPBFILE.SPB_INSRATE_2_4,
                   SPBFILE.SPB_INSTERM_2_5,
                   SPBFILE.SPB_INSRATE_2_5,
                   SPBFILE.SPB_INSTERM_2_6,
                   SPBFILE.SPB_INSRATE_2_6,
                   SPBFILE.SPB_INSTERM_2_7,
                   SPBFILE.SPB_INSRATE_2_7,
                   SPBFILE.SPB_INSTERM_2_8,
                   SPBFILE.SPB_INSRATE_2_8,
                   SPBFILE.SPB_INSTERM_2_9,
                   SPBFILE.SPB_INSRATE_2_9,
                   SPBFILE.SPB_INSTERM_2_10,
                   SPBFILE.SPB_INSRATE_2_10,
                   SPBFILE.SPB_INSTERM_2_11,
                   SPBFILE.SPB_INSRATE_2_11,
                   SPBFILE.SPB_INSTERM_2_12,
                   SPBFILE.SPB_INSRATE_2_12,
                   SPBFILE.SPB_INSTERM_2_13,
                   SPBFILE.SPB_INSRATE_2_13,
                   SPBFILE.SPB_INSTERM_2_14,
                   SPBFILE.SPB_INSRATE_2_14,
                   SPBFILE.SPB_INSTERM_2_15,
                   SPBFILE.SPB_INSRATE_2_15,
                   SPBFILE.SPB_INSTERM_2_16,
                   SPBFILE.SPB_INSRATE_2_16,
                   SPBFILE.SPB_INSTERM_2_17,
                   SPBFILE.SPB_INSRATE_2_17,
                   SPBFILE.SPB_INSTERM_2_18,
                   SPBFILE.SPB_INSRATE_2_18,
                   SPBFILE.SPB_INSTERM_2_19,
                   SPBFILE.SPB_INSRATE_2_19,
                   SPBFILE.SPB_INSTERM_2_20,
                   SPBFILE.SPB_INSRATE_2_20,
                   SPBFILE.SPB_INSMINPREM_2,
                   SPBFILE.SPB_INSMINCHGABL_2,
                   SPBFILE.SPB_INSMAXCHGABL_2,
                   SPBFILE.SPB_INSMAXBENIF_2,
                   SPBFILE.SPB_INSMAXTERM_2,
                   ISNULL(SPBFILE.SPB_INSCHGBEN_2,''),
                   SPBFILE.SPB_INSEXCEPT_2,
                   SPBFILE.SPB_INSNOVER_2,
                   SPBFILE.SPB_INSMINAGE_2_1,
                   SPBFILE.SPB_INSMAXAGE_2_1,
                   SPBFILE.SPB_INSMAXCOV_2_1,
                   SPBFILE.SPB_INSMAXTRM_2_1,
                   SPBFILE.SPB_INSMINAGE_2_2,
                   SPBFILE.SPB_INSMAXAGE_2_2,
                   SPBFILE.SPB_INSMAXCOV_2_2,
                   SPBFILE.SPB_INSMAXTRM_2_2,
                   SPBFILE.SPB_INSMINAGE_2_3,
                   SPBFILE.SPB_INSMAXAGE_2_3,
                   SPBFILE.SPB_INSMAXCOV_2_3,
                   SPBFILE.SPB_INSMAXTRM_2_3,
                   SPBFILE.SPB_INSMINAGE_2_4,
                   SPBFILE.SPB_INSMAXAGE_2_4,
                   SPBFILE.SPB_INSMAXCOV_2_4,
                   SPBFILE.SPB_INSMAXTRM_2_4,
                   SPBFILE.SPB_INSMINAGE_2_5,
                   SPBFILE.SPB_INSMAXAGE_2_5,
                   SPBFILE.SPB_INSMAXCOV_2_5,
                   SPBFILE.SPB_INSMAXTRM_2_5,
                   ISNULL(SPBFILE.SPB_INSFEECD_2,''),
                   ISNULL(SPBFILE.SPB_INSFEE_FRMLA_2,''),
                   ISNULL(SPBFILE.SPB_INSCHGABL_FG_2,''),
                   ISNULL(SPBFILE.SPB_INSREJECT_FG_2,''),
                   ISNULL(SPBFILE.SPB_INSFINCHG_2,''),
                   SPBFILE.SPB_INSCOM_3,
                   SPBFILE.SPB_INSRATECD_3,
                   SPBFILE.SPB_INSJFAC_3,
                   SPBFILE.SPB_INSDISCNT_3,
                   ISNULL(SPBFILE.SPB_INSCHGABL_3,''),
                   SPBFILE.SPB_INSTERM_3_1,
                   SPBFILE.SPB_INSRATE_3_1,
                   SPBFILE.SPB_INSTERM_3_2,
                   SPBFILE.SPB_INSRATE_3_2,
                   SPBFILE.SPB_INSTERM_3_3,
                   SPBFILE.SPB_INSRATE_3_3,
                   SPBFILE.SPB_INSTERM_3_4,
                   SPBFILE.SPB_INSRATE_3_4,
                   SPBFILE.SPB_INSTERM_3_5,
                   SPBFILE.SPB_INSRATE_3_5,
                   SPBFILE.SPB_INSTERM_3_6,
                   SPBFILE.SPB_INSRATE_3_6,
                   SPBFILE.SPB_INSTERM_3_7,
                   SPBFILE.SPB_INSRATE_3_7,
                   SPBFILE.SPB_INSTERM_3_8,
                   SPBFILE.SPB_INSRATE_3_8,
                   SPBFILE.SPB_INSTERM_3_9,
                   SPBFILE.SPB_INSRATE_3_9,
                   SPBFILE.SPB_INSTERM_3_10,
                   SPBFILE.SPB_INSRATE_3_10,
                   SPBFILE.SPB_INSTERM_3_11,
                   SPBFILE.SPB_INSRATE_3_11,
                   SPBFILE.SPB_INSTERM_3_12,
                   SPBFILE.SPB_INSRATE_3_12,
                   SPBFILE.SPB_INSTERM_3_13,
                   SPBFILE.SPB_INSRATE_3_13,
                   SPBFILE.SPB_INSTERM_3_14,
                   SPBFILE.SPB_INSRATE_3_14,
                   SPBFILE.SPB_INSTERM_3_15,
                   SPBFILE.SPB_INSRATE_3_15,
                   SPBFILE.SPB_INSTERM_3_16,
                   SPBFILE.SPB_INSRATE_3_16,
                   SPBFILE.SPB_INSTERM_3_17,
                   SPBFILE.SPB_INSRATE_3_17,
                   SPBFILE.SPB_INSTERM_3_18,
                   SPBFILE.SPB_INSRATE_3_18,
                   SPBFILE.SPB_INSTERM_3_19,
                   SPBFILE.SPB_INSRATE_3_19,
                   SPBFILE.SPB_INSTERM_3_20,
                   SPBFILE.SPB_INSRATE_3_20,
                   SPBFILE.SPB_INSMINPREM_3,
                   SPBFILE.SPB_INSMINCHGABL_3,
                   SPBFILE.SPB_INSMAXCHGABL_3,
                   SPBFILE.SPB_INSMAXBENIF_3,
                   SPBFILE.SPB_INSMAXTERM_3,
                   ISNULL(SPBFILE.SPB_INSCHGBEN_3,''),
                   SPBFILE.SPB_INSEXCEPT_3,
                   SPBFILE.SPB_INSNOVER_3,
                   SPBFILE.SPB_INSMINAGE_3_1,
                   SPBFILE.SPB_INSMAXAGE_3_1,
                   SPBFILE.SPB_INSMAXCOV_3_1,
                   SPBFILE.SPB_INSMAXTRM_3_1,
                   SPBFILE.SPB_INSMINAGE_3_2,
                   SPBFILE.SPB_INSMAXAGE_3_2,
                   SPBFILE.SPB_INSMAXCOV_3_2,
                   SPBFILE.SPB_INSMAXTRM_3_2,
                   SPBFILE.SPB_INSMINAGE_3_3,
                   SPBFILE.SPB_INSMAXAGE_3_3,
                   SPBFILE.SPB_INSMAXCOV_3_3,
                   SPBFILE.SPB_INSMAXTRM_3_3,
                   SPBFILE.SPB_INSMINAGE_3_4,
                   SPBFILE.SPB_INSMAXAGE_3_4,
                   SPBFILE.SPB_INSMAXCOV_3_4,
                   SPBFILE.SPB_INSMAXTRM_3_4,
                   SPBFILE.SPB_INSMINAGE_3_5,
                   SPBFILE.SPB_INSMAXAGE_3_5,
                   SPBFILE.SPB_INSMAXCOV_3_5,
                   SPBFILE.SPB_INSMAXTRM_3_5,
                   ISNULL(SPBFILE.SPB_INSFEECD_3,''),
                   ISNULL(SPBFILE.SPB_INSFEE_FRMLA_3,''),
                   ISNULL(SPBFILE.SPB_INSCHGABL_FG_3,''),
                   ISNULL(SPBFILE.SPB_INSREJECT_FG_3,''),
                   ISNULL(SPBFILE.SPB_INSFINCHG_3,''),
                   SPBFILE.SPB_INSCOM_4,
                   SPBFILE.SPB_INSRATECD_4,
                   SPBFILE.SPB_INSJFAC_4,
                   SPBFILE.SPB_INSDISCNT_4,
                   ISNULL(SPBFILE.SPB_INSCHGABL_4,''),
                   SPBFILE.SPB_INSTERM_4_1,
                   SPBFILE.SPB_INSRATE_4_1,
                   SPBFILE.SPB_INSTERM_4_2,
                   SPBFILE.SPB_INSRATE_4_2,
                   SPBFILE.SPB_INSTERM_4_3,
                   SPBFILE.SPB_INSRATE_4_3,
                   SPBFILE.SPB_INSTERM_4_4,
                   SPBFILE.SPB_INSRATE_4_4,
                   SPBFILE.SPB_INSTERM_4_5,
                   SPBFILE.SPB_INSRATE_4_5,
                   SPBFILE.SPB_INSTERM_4_6,
                   SPBFILE.SPB_INSRATE_4_6,
                   SPBFILE.SPB_INSTERM_4_7,
                   SPBFILE.SPB_INSRATE_4_7,
                   SPBFILE.SPB_INSTERM_4_8,
                   SPBFILE.SPB_INSRATE_4_8,
                   SPBFILE.SPB_INSTERM_4_9,
                   SPBFILE.SPB_INSRATE_4_9,
                   SPBFILE.SPB_INSTERM_4_10,
                   SPBFILE.SPB_INSRATE_4_10,
                   SPBFILE.SPB_INSTERM_4_11,
                   SPBFILE.SPB_INSRATE_4_11,
                   SPBFILE.SPB_INSTERM_4_12,
                   SPBFILE.SPB_INSRATE_4_12,
                   SPBFILE.SPB_INSTERM_4_13,
                   SPBFILE.SPB_INSRATE_4_13,
                   SPBFILE.SPB_INSTERM_4_14,
                   SPBFILE.SPB_INSRATE_4_14,
                   SPBFILE.SPB_INSTERM_4_15,
                   SPBFILE.SPB_INSRATE_4_15,
                   SPBFILE.SPB_INSTERM_4_16,
                   SPBFILE.SPB_INSRATE_4_16,
                   SPBFILE.SPB_INSTERM_4_17,
                   SPBFILE.SPB_INSRATE_4_17,
                   SPBFILE.SPB_INSTERM_4_18,
                   SPBFILE.SPB_INSRATE_4_18,
                   SPBFILE.SPB_INSTERM_4_19,
                   SPBFILE.SPB_INSRATE_4_19,
                   SPBFILE.SPB_INSTERM_4_20,
                   SPBFILE.SPB_INSRATE_4_20,
                   SPBFILE.SPB_INSMINPREM_4,
                   SPBFILE.SPB_INSMINCHGABL_4,
                   SPBFILE.SPB_INSMAXCHGABL_4,
                   SPBFILE.SPB_INSMAXBENIF_4,
                   SPBFILE.SPB_INSMAXTERM_4,
                   ISNULL(SPBFILE.SPB_INSCHGBEN_4,''),
                   SPBFILE.SPB_INSEXCEPT_4,
                   SPBFILE.SPB_INSNOVER_4,
                   SPBFILE.SPB_INSMINAGE_4_1,
                   SPBFILE.SPB_INSMAXAGE_4_1,
                   SPBFILE.SPB_INSMAXCOV_4_1,
                   SPBFILE.SPB_INSMAXTRM_4_1,
                   SPBFILE.SPB_INSMINAGE_4_2,
                   SPBFILE.SPB_INSMAXAGE_4_2,
                   SPBFILE.SPB_INSMAXCOV_4_2,
                   SPBFILE.SPB_INSMAXTRM_4_2,
                   SPBFILE.SPB_INSMINAGE_4_3,
                   SPBFILE.SPB_INSMAXAGE_4_3,
                   SPBFILE.SPB_INSMAXCOV_4_3,
                   SPBFILE.SPB_INSMAXTRM_4_3,
                   SPBFILE.SPB_INSMINAGE_4_4,
                   SPBFILE.SPB_INSMAXAGE_4_4,
                   SPBFILE.SPB_INSMAXCOV_4_4,
                   SPBFILE.SPB_INSMAXTRM_4_4,
                   SPBFILE.SPB_INSMINAGE_4_5,
                   SPBFILE.SPB_INSMAXAGE_4_5,
                   SPBFILE.SPB_INSMAXCOV_4_5,
                   SPBFILE.SPB_INSMAXTRM_4_5,
                   ISNULL(SPBFILE.SPB_INSFEECD_4,''),
                   ISNULL(SPBFILE.SPB_INSFEE_FRMLA_4,''),
                   ISNULL(SPBFILE.SPB_INSCHGABL_FG_4,''),
                   ISNULL(SPBFILE.SPB_INSREJECT_FG_4,''),
                   ISNULL(SPBFILE.SPB_INSFINCHG_4,''),
                   SPBFILE.SPB_INSCOM_5,
                   SPBFILE.SPB_INSRATECD_5,
                   SPBFILE.SPB_INSJFAC_5,
                   SPBFILE.SPB_INSDISCNT_5,
                   ISNULL(SPBFILE.SPB_INSCHGABL_5,''),
                   SPBFILE.SPB_INSTERM_5_1,
                   SPBFILE.SPB_INSRATE_5_1,
                   SPBFILE.SPB_INSTERM_5_2,
                   SPBFILE.SPB_INSRATE_5_2,
                   SPBFILE.SPB_INSTERM_5_3,
                   SPBFILE.SPB_INSRATE_5_3,
                   SPBFILE.SPB_INSTERM_5_4,
                   SPBFILE.SPB_INSRATE_5_4,
                   SPBFILE.SPB_INSTERM_5_5,
                   SPBFILE.SPB_INSRATE_5_5,
                   SPBFILE.SPB_INSTERM_5_6,
                   SPBFILE.SPB_INSRATE_5_6,
                   SPBFILE.SPB_INSTERM_5_7,
                   SPBFILE.SPB_INSRATE_5_7,
                   SPBFILE.SPB_INSTERM_5_8,
                   SPBFILE.SPB_INSRATE_5_8,
                   SPBFILE.SPB_INSTERM_5_9,
                   SPBFILE.SPB_INSRATE_5_9,
                   SPBFILE.SPB_INSTERM_5_10,
                   SPBFILE.SPB_INSRATE_5_10,
                   SPBFILE.SPB_INSTERM_5_11,
                   SPBFILE.SPB_INSRATE_5_11,
                   SPBFILE.SPB_INSTERM_5_12,
                   SPBFILE.SPB_INSRATE_5_12,
                   SPBFILE.SPB_INSTERM_5_13,
                   SPBFILE.SPB_INSRATE_5_13,
                   SPBFILE.SPB_INSTERM_5_14,
                   SPBFILE.SPB_INSRATE_5_14,
                   SPBFILE.SPB_INSTERM_5_15,
                   SPBFILE.SPB_INSRATE_5_15,
                   SPBFILE.SPB_INSTERM_5_16,
                   SPBFILE.SPB_INSRATE_5_16,
                   SPBFILE.SPB_INSTERM_5_17,
                   SPBFILE.SPB_INSRATE_5_17,
                   SPBFILE.SPB_INSTERM_5_18,
                   SPBFILE.SPB_INSRATE_5_18,
                   SPBFILE.SPB_INSTERM_5_19,
                   SPBFILE.SPB_INSRATE_5_19,
                   SPBFILE.SPB_INSTERM_5_20,
                   SPBFILE.SPB_INSRATE_5_20,
                   SPBFILE.SPB_INSMINPREM_5,
                   SPBFILE.SPB_INSMINCHGABL_5,
                   SPBFILE.SPB_INSMAXCHGABL_5,
                   SPBFILE.SPB_INSMAXBENIF_5,
                   SPBFILE.SPB_INSMAXTERM_5,
                   ISNULL(SPBFILE.SPB_INSCHGBEN_5,''),
                   SPBFILE.SPB_INSEXCEPT_5,
                   SPBFILE.SPB_INSNOVER_5,
                   SPBFILE.SPB_INSMINAGE_5_1,
                   SPBFILE.SPB_INSMAXAGE_5_1,
                   SPBFILE.SPB_INSMAXCOV_5_1,
                   SPBFILE.SPB_INSMAXTRM_5_1,
                   SPBFILE.SPB_INSMINAGE_5_2,
                   SPBFILE.SPB_INSMAXAGE_5_2,
                   SPBFILE.SPB_INSMAXCOV_5_2,
                   SPBFILE.SPB_INSMAXTRM_5_2,
                   SPBFILE.SPB_INSMINAGE_5_3,
                   SPBFILE.SPB_INSMAXAGE_5_3,
                   SPBFILE.SPB_INSMAXCOV_5_3,
                   SPBFILE.SPB_INSMAXTRM_5_3,
                   SPBFILE.SPB_INSMINAGE_5_4,
                   SPBFILE.SPB_INSMAXAGE_5_4,
                   SPBFILE.SPB_INSMAXCOV_5_4,
                   SPBFILE.SPB_INSMAXTRM_5_4,
                   SPBFILE.SPB_INSMINAGE_5_5,
                   SPBFILE.SPB_INSMAXAGE_5_5,
                   SPBFILE.SPB_INSMAXCOV_5_5,
                   SPBFILE.SPB_INSMAXTRM_5_5,
                   ISNULL(SPBFILE.SPB_INSFEECD_5,''),
                   ISNULL(SPBFILE.SPB_INSFEE_FRMLA_5,''),
                   ISNULL(SPBFILE.SPB_INSCHGABL_FG_5,''),
                   ISNULL(SPBFILE.SPB_INSREJECT_FG_5,''),
                   ISNULL(SPBFILE.SPB_INSFINCHG_5,''),
                   SPBFILE.SPB_INSCOM_6,
                   SPBFILE.SPB_INSRATECD_6,
                   SPBFILE.SPB_INSJFAC_6,
                   SPBFILE.SPB_INSDISCNT_6,
                   ISNULL(SPBFILE.SPB_INSCHGABL_6,''),
                   SPBFILE.SPB_INSTERM_6_1,
                   SPBFILE.SPB_INSRATE_6_1,
                   SPBFILE.SPB_INSTERM_6_2,
                   SPBFILE.SPB_INSRATE_6_2,
                   SPBFILE.SPB_INSTERM_6_3,
                   SPBFILE.SPB_INSRATE_6_3,
                   SPBFILE.SPB_INSTERM_6_4,
                   SPBFILE.SPB_INSRATE_6_4,
                   SPBFILE.SPB_INSTERM_6_5,
                   SPBFILE.SPB_INSRATE_6_5,
                   SPBFILE.SPB_INSTERM_6_6,
                   SPBFILE.SPB_INSRATE_6_6,
                   SPBFILE.SPB_INSTERM_6_7,
                   SPBFILE.SPB_INSRATE_6_7,
                   SPBFILE.SPB_INSTERM_6_8,
                   SPBFILE.SPB_INSRATE_6_8,
                   SPBFILE.SPB_INSTERM_6_9,
                   SPBFILE.SPB_INSRATE_6_9,
                   SPBFILE.SPB_INSTERM_6_10,
                   SPBFILE.SPB_INSRATE_6_10,
                   SPBFILE.SPB_INSTERM_6_11,
                   SPBFILE.SPB_INSRATE_6_11,
                   SPBFILE.SPB_INSTERM_6_12,
                   SPBFILE.SPB_INSRATE_6_12,
                   SPBFILE.SPB_INSTERM_6_13,
                   SPBFILE.SPB_INSRATE_6_13,
                   SPBFILE.SPB_INSTERM_6_14,
                   SPBFILE.SPB_INSRATE_6_14,
                   SPBFILE.SPB_INSTERM_6_15,
                   SPBFILE.SPB_INSRATE_6_15,
                   SPBFILE.SPB_INSTERM_6_16,
                   SPBFILE.SPB_INSRATE_6_16,
                   SPBFILE.SPB_INSTERM_6_17,
                   SPBFILE.SPB_INSRATE_6_17,
                   SPBFILE.SPB_INSTERM_6_18,
                   SPBFILE.SPB_INSRATE_6_18,
                   SPBFILE.SPB_INSTERM_6_19,
                   SPBFILE.SPB_INSRATE_6_19,
                   SPBFILE.SPB_INSTERM_6_20,
                   SPBFILE.SPB_INSRATE_6_20,
                   SPBFILE.SPB_INSMINPREM_6,
                   SPBFILE.SPB_INSMINCHGABL_6,
                   SPBFILE.SPB_INSMAXCHGABL_6,
                   SPBFILE.SPB_INSMAXBENIF_6,
                   SPBFILE.SPB_INSMAXTERM_6,
                   ISNULL(SPBFILE.SPB_INSCHGBEN_6,''),
                   SPBFILE.SPB_INSEXCEPT_6,
                   SPBFILE.SPB_INSNOVER_6,
                   SPBFILE.SPB_INSMINAGE_6_1,
                   SPBFILE.SPB_INSMAXAGE_6_1,
                   SPBFILE.SPB_INSMAXCOV_6_1,
                   SPBFILE.SPB_INSMAXTRM_6_1,
                   SPBFILE.SPB_INSMINAGE_6_2,
                   SPBFILE.SPB_INSMAXAGE_6_2,
                   SPBFILE.SPB_INSMAXCOV_6_2,
                   SPBFILE.SPB_INSMAXTRM_6_2,
                   SPBFILE.SPB_INSMINAGE_6_3,
                   SPBFILE.SPB_INSMAXAGE_6_3,
                   SPBFILE.SPB_INSMAXCOV_6_3,
                   SPBFILE.SPB_INSMAXTRM_6_3,
                   SPBFILE.SPB_INSMINAGE_6_4,
                   SPBFILE.SPB_INSMAXAGE_6_4,
                   SPBFILE.SPB_INSMAXCOV_6_4,
                   SPBFILE.SPB_INSMAXTRM_6_4,
                   SPBFILE.SPB_INSMINAGE_6_5,
                   SPBFILE.SPB_INSMAXAGE_6_5,
                   SPBFILE.SPB_INSMAXCOV_6_5,
                   SPBFILE.SPB_INSMAXTRM_6_5,
                   ISNULL(SPBFILE.SPB_INSFEECD_6,''),
                   ISNULL(SPBFILE.SPB_INSFEE_FRMLA_6,''),
                   ISNULL(SPBFILE.SPB_INSCHGABL_FG_6,''),
                   ISNULL(SPBFILE.SPB_INSREJECT_FG_6,''),
                   ISNULL(SPBFILE.SPB_INSFINCHG_6,''),
                   SPBFILE.SPB_INSCOM_7,
                   SPBFILE.SPB_INSRATECD_7,
                   SPBFILE.SPB_INSJFAC_7,
                   SPBFILE.SPB_INSDISCNT_7,
                   ISNULL(SPBFILE.SPB_INSCHGABL_7,''),
                   SPBFILE.SPB_INSTERM_7_1,
                   SPBFILE.SPB_INSRATE_7_1,
                   SPBFILE.SPB_INSTERM_7_2,
                   SPBFILE.SPB_INSRATE_7_2,
                   SPBFILE.SPB_INSTERM_7_3,
                   SPBFILE.SPB_INSRATE_7_3,
                   SPBFILE.SPB_INSTERM_7_4,
                   SPBFILE.SPB_INSRATE_7_4,
                   SPBFILE.SPB_INSTERM_7_5,
                   SPBFILE.SPB_INSRATE_7_5,
                   SPBFILE.SPB_INSTERM_7_6,
                   SPBFILE.SPB_INSRATE_7_6,
                   SPBFILE.SPB_INSTERM_7_7,
                   SPBFILE.SPB_INSRATE_7_7,
                   SPBFILE.SPB_INSTERM_7_8,
                   SPBFILE.SPB_INSRATE_7_8,
                   SPBFILE.SPB_INSTERM_7_9,
                   SPBFILE.SPB_INSRATE_7_9,
                   SPBFILE.SPB_INSTERM_7_10,
                   SPBFILE.SPB_INSRATE_7_10,
                   SPBFILE.SPB_INSTERM_7_11,
                   SPBFILE.SPB_INSRATE_7_11,
                   SPBFILE.SPB_INSTERM_7_12,
                   SPBFILE.SPB_INSRATE_7_12,
                   SPBFILE.SPB_INSTERM_7_13,
                   SPBFILE.SPB_INSRATE_7_13,
                   SPBFILE.SPB_INSTERM_7_14,
                   SPBFILE.SPB_INSRATE_7_14,
                   SPBFILE.SPB_INSTERM_7_15,
                   SPBFILE.SPB_INSRATE_7_15,
                   SPBFILE.SPB_INSTERM_7_16,
                   SPBFILE.SPB_INSRATE_7_16,
                   SPBFILE.SPB_INSTERM_7_17,
                   SPBFILE.SPB_INSRATE_7_17,
                   SPBFILE.SPB_INSTERM_7_18,
                   SPBFILE.SPB_INSRATE_7_18,
                   SPBFILE.SPB_INSTERM_7_19,
                   SPBFILE.SPB_INSRATE_7_19,
                   SPBFILE.SPB_INSTERM_7_20,
                   SPBFILE.SPB_INSRATE_7_20,
                   SPBFILE.SPB_INSMINPREM_7,
                   SPBFILE.SPB_INSMINCHGABL_7,
                   SPBFILE.SPB_INSMAXCHGABL_7,
                   SPBFILE.SPB_INSMAXBENIF_7,
                   SPBFILE.SPB_INSMAXTERM_7,
                   ISNULL(SPBFILE.SPB_INSCHGBEN_7,''),
                   SPBFILE.SPB_INSEXCEPT_7,
                   SPBFILE.SPB_INSNOVER_7,
                   SPBFILE.SPB_INSMINAGE_7_1,
                   SPBFILE.SPB_INSMAXAGE_7_1,
                   SPBFILE.SPB_INSMAXCOV_7_1,
                   SPBFILE.SPB_INSMAXTRM_7_1,
                   SPBFILE.SPB_INSMINAGE_7_2,
                   SPBFILE.SPB_INSMAXAGE_7_2,
                   SPBFILE.SPB_INSMAXCOV_7_2,
                   SPBFILE.SPB_INSMAXTRM_7_2,
                   SPBFILE.SPB_INSMINAGE_7_3,
                   SPBFILE.SPB_INSMAXAGE_7_3,
                   SPBFILE.SPB_INSMAXCOV_7_3,
                   SPBFILE.SPB_INSMAXTRM_7_3,
                   SPBFILE.SPB_INSMINAGE_7_4,
                   SPBFILE.SPB_INSMAXAGE_7_4,
                   SPBFILE.SPB_INSMAXCOV_7_4,
                   SPBFILE.SPB_INSMAXTRM_7_4,
                   SPBFILE.SPB_INSMINAGE_7_5,
                   SPBFILE.SPB_INSMAXAGE_7_5,
                   SPBFILE.SPB_INSMAXCOV_7_5,
                   SPBFILE.SPB_INSMAXTRM_7_5,
                   ISNULL(SPBFILE.SPB_INSFEECD_7,''),
                   ISNULL(SPBFILE.SPB_INSFEE_FRMLA_7,''),
                   ISNULL(SPBFILE.SPB_INSCHGABL_FG_7,''),
                   ISNULL(SPBFILE.SPB_INSREJECT_FG_7,''),
                   ISNULL(SPBFILE.SPB_INSFINCHG_7,''),
                   SPBFILE.SPB_INSCOM_8,
                   SPBFILE.SPB_INSRATECD_8,
                   SPBFILE.SPB_INSJFAC_8,
                   SPBFILE.SPB_INSDISCNT_8,
                   ISNULL(SPBFILE.SPB_INSCHGABL_8,''),
                   SPBFILE.SPB_INSTERM_8_1,
                   SPBFILE.SPB_INSRATE_8_1,
                   SPBFILE.SPB_INSTERM_8_2,
                   SPBFILE.SPB_INSRATE_8_2,
                   SPBFILE.SPB_INSTERM_8_3,
                   SPBFILE.SPB_INSRATE_8_3,
                   SPBFILE.SPB_INSTERM_8_4,
                   SPBFILE.SPB_INSRATE_8_4,
                   SPBFILE.SPB_INSTERM_8_5,
                   SPBFILE.SPB_INSRATE_8_5,
                   SPBFILE.SPB_INSTERM_8_6,
                   SPBFILE.SPB_INSRATE_8_6,
                   SPBFILE.SPB_INSTERM_8_7,
                   SPBFILE.SPB_INSRATE_8_7,
                   SPBFILE.SPB_INSTERM_8_8,
                   SPBFILE.SPB_INSRATE_8_8,
                   SPBFILE.SPB_INSTERM_8_9,
                   SPBFILE.SPB_INSRATE_8_9,
                   SPBFILE.SPB_INSTERM_8_10,
                   SPBFILE.SPB_INSRATE_8_10,
                   SPBFILE.SPB_INSTERM_8_11,
                   SPBFILE.SPB_INSRATE_8_11,
                   SPBFILE.SPB_INSTERM_8_12,
                   SPBFILE.SPB_INSRATE_8_12,
                   SPBFILE.SPB_INSTERM_8_13,
                   SPBFILE.SPB_INSRATE_8_13,
                   SPBFILE.SPB_INSTERM_8_14,
                   SPBFILE.SPB_INSRATE_8_14,
                   SPBFILE.SPB_INSTERM_8_15,
                   SPBFILE.SPB_INSRATE_8_15,
                   SPBFILE.SPB_INSTERM_8_16,
                   SPBFILE.SPB_INSRATE_8_16,
                   SPBFILE.SPB_INSTERM_8_17,
                   SPBFILE.SPB_INSRATE_8_17,
                   SPBFILE.SPB_INSTERM_8_18,
                   SPBFILE.SPB_INSRATE_8_18,
                   SPBFILE.SPB_INSTERM_8_19,
                   SPBFILE.SPB_INSRATE_8_19,
                   SPBFILE.SPB_INSTERM_8_20,
                   SPBFILE.SPB_INSRATE_8_20,
                   SPBFILE.SPB_INSMINPREM_8,
                   SPBFILE.SPB_INSMINCHGABL_8,
                   SPBFILE.SPB_INSMAXCHGABL_8,
                   SPBFILE.SPB_INSMAXBENIF_8,
                   SPBFILE.SPB_INSMAXTERM_8,
                   ISNULL(SPBFILE.SPB_INSCHGBEN_8,''),
                   SPBFILE.SPB_INSEXCEPT_8,
                   SPBFILE.SPB_INSNOVER_8,
                   SPBFILE.SPB_INSMINAGE_8_1,
                   SPBFILE.SPB_INSMAXAGE_8_1,
                   SPBFILE.SPB_INSMAXCOV_8_1,
                   SPBFILE.SPB_INSMAXTRM_8_1,
                   SPBFILE.SPB_INSMINAGE_8_2,
                   SPBFILE.SPB_INSMAXAGE_8_2,
                   SPBFILE.SPB_INSMAXCOV_8_2,
                   SPBFILE.SPB_INSMAXTRM_8_2,
                   SPBFILE.SPB_INSMINAGE_8_3,
                   SPBFILE.SPB_INSMAXAGE_8_3,
                   SPBFILE.SPB_INSMAXCOV_8_3,
                   SPBFILE.SPB_INSMAXTRM_8_3,
                   SPBFILE.SPB_INSMINAGE_8_4,
                   SPBFILE.SPB_INSMAXAGE_8_4,
                   SPBFILE.SPB_INSMAXCOV_8_4,
                   SPBFILE.SPB_INSMAXTRM_8_4,
                   SPBFILE.SPB_INSMINAGE_8_5,
                   SPBFILE.SPB_INSMAXAGE_8_5,
                   SPBFILE.SPB_INSMAXCOV_8_5,
                   SPBFILE.SPB_INSMAXTRM_8_5,
                   ISNULL(SPBFILE.SPB_INSFEECD_8,''),
                   ISNULL(SPBFILE.SPB_INSFEE_FRMLA_8,''),
                   ISNULL(SPBFILE.SPB_INSCHGABL_FG_8,''),
                   ISNULL(SPBFILE.SPB_INSREJECT_FG_8,''),
                   ISNULL(SPBFILE.SPB_INSFINCHG_8,''),
                   SPBFILE.SPB_INSCOM_9,
                   SPBFILE.SPB_INSRATECD_9,
                   SPBFILE.SPB_INSJFAC_9,
                   SPBFILE.SPB_INSDISCNT_9,
                   ISNULL(SPBFILE.SPB_INSCHGABL_9,''),
                   SPBFILE.SPB_INSTERM_9_1,
                   SPBFILE.SPB_INSRATE_9_1,
                   SPBFILE.SPB_INSTERM_9_2,
                   SPBFILE.SPB_INSRATE_9_2,
                   SPBFILE.SPB_INSTERM_9_3,
                   SPBFILE.SPB_INSRATE_9_3,
                   SPBFILE.SPB_INSTERM_9_4,
                   SPBFILE.SPB_INSRATE_9_4,
                   SPBFILE.SPB_INSTERM_9_5,
                   SPBFILE.SPB_INSRATE_9_5,
                   SPBFILE.SPB_INSTERM_9_6,
                   SPBFILE.SPB_INSRATE_9_6,
                   SPBFILE.SPB_INSTERM_9_7,
                   SPBFILE.SPB_INSRATE_9_7,
                   SPBFILE.SPB_INSTERM_9_8,
                   SPBFILE.SPB_INSRATE_9_8,
                   SPBFILE.SPB_INSTERM_9_9,
                   SPBFILE.SPB_INSRATE_9_9,
                   SPBFILE.SPB_INSTERM_9_10,
                   SPBFILE.SPB_INSRATE_9_10,
                   SPBFILE.SPB_INSTERM_9_11,
                   SPBFILE.SPB_INSRATE_9_11,
                   SPBFILE.SPB_INSTERM_9_12,
                   SPBFILE.SPB_INSRATE_9_12,
                   SPBFILE.SPB_INSTERM_9_13,
                   SPBFILE.SPB_INSRATE_9_13,
                   SPBFILE.SPB_INSTERM_9_14,
                   SPBFILE.SPB_INSRATE_9_14,
                   SPBFILE.SPB_INSTERM_9_15,
                   SPBFILE.SPB_INSRATE_9_15,
                   SPBFILE.SPB_INSTERM_9_16,
                   SPBFILE.SPB_INSRATE_9_16,
                   SPBFILE.SPB_INSTERM_9_17,
                   SPBFILE.SPB_INSRATE_9_17,
                   SPBFILE.SPB_INSTERM_9_18,
                   SPBFILE.SPB_INSRATE_9_18,
                   SPBFILE.SPB_INSTERM_9_19,
                   SPBFILE.SPB_INSRATE_9_19,
                   SPBFILE.SPB_INSTERM_9_20,
                   SPBFILE.SPB_INSRATE_9_20,
                   SPBFILE.SPB_INSMINPREM_9,
                   SPBFILE.SPB_INSMINCHGABL_9,
                   SPBFILE.SPB_INSMAXCHGABL_9,
                   SPBFILE.SPB_INSMAXBENIF_9,
                   SPBFILE.SPB_INSMAXTERM_9,
                   ISNULL(SPBFILE.SPB_INSCHGBEN_9,''),
                   SPBFILE.SPB_INSEXCEPT_9,
                   SPBFILE.SPB_INSNOVER_9,
                   SPBFILE.SPB_INSMINAGE_9_1,
                   SPBFILE.SPB_INSMAXAGE_9_1,
                   SPBFILE.SPB_INSMAXCOV_9_1,
                   SPBFILE.SPB_INSMAXTRM_9_1,
                   SPBFILE.SPB_INSMINAGE_9_2,
                   SPBFILE.SPB_INSMAXAGE_9_2,
                   SPBFILE.SPB_INSMAXCOV_9_2,
                   SPBFILE.SPB_INSMAXTRM_9_2,
                   SPBFILE.SPB_INSMINAGE_9_3,
                   SPBFILE.SPB_INSMAXAGE_9_3,
                   SPBFILE.SPB_INSMAXCOV_9_3,
                   SPBFILE.SPB_INSMAXTRM_9_3,
                   SPBFILE.SPB_INSMINAGE_9_4,
                   SPBFILE.SPB_INSMAXAGE_9_4,
                   SPBFILE.SPB_INSMAXCOV_9_4,
                   SPBFILE.SPB_INSMAXTRM_9_4,
                   SPBFILE.SPB_INSMINAGE_9_5,
                   SPBFILE.SPB_INSMAXAGE_9_5,
                   SPBFILE.SPB_INSMAXCOV_9_5,
                   SPBFILE.SPB_INSMAXTRM_9_5,
                   ISNULL(SPBFILE.SPB_INSFEECD_9,''),
                   ISNULL(SPBFILE.SPB_INSFEE_FRMLA_9,''),
                   ISNULL(SPBFILE.SPB_INSCHGABL_FG_9,''),
                   ISNULL(SPBFILE.SPB_INSREJECT_FG_9,''),
                   ISNULL(SPBFILE.SPB_INSFINCHG_9,''),
                   SPBFILE.SPB_INSCOM_10,
                   SPBFILE.SPB_INSRATECD_10,
                   SPBFILE.SPB_INSJFAC_10,
                   SPBFILE.SPB_INSDISCNT_10,
                   ISNULL(SPBFILE.SPB_INSCHGABL_10,''),
                   SPBFILE.SPB_INSTERM_10_1,
                   SPBFILE.SPB_INSRATE_10_1,
                   SPBFILE.SPB_INSTERM_10_2,
                   SPBFILE.SPB_INSRATE_10_2,
                   SPBFILE.SPB_INSTERM_10_3,
                   SPBFILE.SPB_INSRATE_10_3,
                   SPBFILE.SPB_INSTERM_10_4,
                   SPBFILE.SPB_INSRATE_10_4,
                   SPBFILE.SPB_INSTERM_10_5,
                   SPBFILE.SPB_INSRATE_10_5,
                   SPBFILE.SPB_INSTERM_10_6,
                   SPBFILE.SPB_INSRATE_10_6,
                   SPBFILE.SPB_INSTERM_10_7,
                   SPBFILE.SPB_INSRATE_10_7,
                   SPBFILE.SPB_INSTERM_10_8,
                   SPBFILE.SPB_INSRATE_10_8,
                   SPBFILE.SPB_INSTERM_10_9,
                   SPBFILE.SPB_INSRATE_10_9,
                   SPBFILE.SPB_INSTERM_10_10,
                   SPBFILE.SPB_INSRATE_10_10,
                   SPBFILE.SPB_INSTERM_10_11,
                   SPBFILE.SPB_INSRATE_10_11,
                   SPBFILE.SPB_INSTERM_10_12,
                   SPBFILE.SPB_INSRATE_10_12,
                   SPBFILE.SPB_INSTERM_10_13,
                   SPBFILE.SPB_INSRATE_10_13,
                   SPBFILE.SPB_INSTERM_10_14,
                   SPBFILE.SPB_INSRATE_10_14,
                   SPBFILE.SPB_INSTERM_10_15,
                   SPBFILE.SPB_INSRATE_10_15,
                   SPBFILE.SPB_INSTERM_10_16,
                   SPBFILE.SPB_INSRATE_10_16,
                   SPBFILE.SPB_INSTERM_10_17,
                   SPBFILE.SPB_INSRATE_10_17,
                   SPBFILE.SPB_INSTERM_10_18,
                   SPBFILE.SPB_INSRATE_10_18,
                   SPBFILE.SPB_INSTERM_10_19,
                   SPBFILE.SPB_INSRATE_10_19,
                   SPBFILE.SPB_INSTERM_10_20,
                   SPBFILE.SPB_INSRATE_10_20,
                   SPBFILE.SPB_INSMINPREM_10,
                   SPBFILE.SPB_INSMINCHGABL_10,
                   SPBFILE.SPB_INSMAXCHGABL_10,
                   SPBFILE.SPB_INSMAXBENIF_10,
                   SPBFILE.SPB_INSMAXTERM_10,
                   ISNULL(SPBFILE.SPB_INSCHGBEN_10,''),
                   SPBFILE.SPB_INSEXCEPT_10,
                   SPBFILE.SPB_INSNOVER_10,
                   SPBFILE.SPB_INSMINAGE_10_1,
                   SPBFILE.SPB_INSMAXAGE_10_1,
                   SPBFILE.SPB_INSMAXCOV_10_1,
                   SPBFILE.SPB_INSMAXTRM_10_1,
                   SPBFILE.SPB_INSMINAGE_10_2,
                   SPBFILE.SPB_INSMAXAGE_10_2,
                   SPBFILE.SPB_INSMAXCOV_10_2,
                   SPBFILE.SPB_INSMAXTRM_10_2,
                   SPBFILE.SPB_INSMINAGE_10_3,
                   SPBFILE.SPB_INSMAXAGE_10_3,
                   SPBFILE.SPB_INSMAXCOV_10_3,
                   SPBFILE.SPB_INSMAXTRM_10_3,
                   SPBFILE.SPB_INSMINAGE_10_4,
                   SPBFILE.SPB_INSMAXAGE_10_4,
                   SPBFILE.SPB_INSMAXCOV_10_4,
                   SPBFILE.SPB_INSMAXTRM_10_4,
                   SPBFILE.SPB_INSMINAGE_10_5,
                   SPBFILE.SPB_INSMAXAGE_10_5,
                   SPBFILE.SPB_INSMAXCOV_10_5,
                   SPBFILE.SPB_INSMAXTRM_10_5,
                   ISNULL(SPBFILE.SPB_INSFEECD_10,''),
                   ISNULL(SPBFILE.SPB_INSFEE_FRMLA_10,''),
                   ISNULL(SPBFILE.SPB_INSCHGABL_FG_10,''),
                   ISNULL(SPBFILE.SPB_INSREJECT_FG_10,''),
                   ISNULL(SPBFILE.SPB_INSFINCHG_10,''),
                   SPBFILE.SPB_INSCOM_11,
                   SPBFILE.SPB_INSRATECD_11,
                   SPBFILE.SPB_INSJFAC_11,
                   SPBFILE.SPB_INSDISCNT_11,
                   ISNULL(SPBFILE.SPB_INSCHGABL_11,''),
                   SPBFILE.SPB_INSTERM_11_1,
                   SPBFILE.SPB_INSRATE_11_1,
                   SPBFILE.SPB_INSTERM_11_2,
                   SPBFILE.SPB_INSRATE_11_2,
                   SPBFILE.SPB_INSTERM_11_3,
                   SPBFILE.SPB_INSRATE_11_3,
                   SPBFILE.SPB_INSTERM_11_4,
                   SPBFILE.SPB_INSRATE_11_4,
                   SPBFILE.SPB_INSTERM_11_5,
                   SPBFILE.SPB_INSRATE_11_5,
                   SPBFILE.SPB_INSTERM_11_6,
                   SPBFILE.SPB_INSRATE_11_6,
                   SPBFILE.SPB_INSTERM_11_7,
                   SPBFILE.SPB_INSRATE_11_7,
                   SPBFILE.SPB_INSTERM_11_8,
                   SPBFILE.SPB_INSRATE_11_8,
                   SPBFILE.SPB_INSTERM_11_9,
                   SPBFILE.SPB_INSRATE_11_9,
                   SPBFILE.SPB_INSTERM_11_10,
                   SPBFILE.SPB_INSRATE_11_10,
                   SPBFILE.SPB_INSTERM_11_11,
                   SPBFILE.SPB_INSRATE_11_11,
                   SPBFILE.SPB_INSTERM_11_12,
                   SPBFILE.SPB_INSRATE_11_12,
                   SPBFILE.SPB_INSTERM_11_13,
                   SPBFILE.SPB_INSRATE_11_13,
                   SPBFILE.SPB_INSTERM_11_14,
                   SPBFILE.SPB_INSRATE_11_14,
                   SPBFILE.SPB_INSTERM_11_15,
                   SPBFILE.SPB_INSRATE_11_15,
                   SPBFILE.SPB_INSTERM_11_16,
                   SPBFILE.SPB_INSRATE_11_16,
                   SPBFILE.SPB_INSTERM_11_17,
                   SPBFILE.SPB_INSRATE_11_17,
                   SPBFILE.SPB_INSTERM_11_18,
                   SPBFILE.SPB_INSRATE_11_18,
                   SPBFILE.SPB_INSTERM_11_19,
                   SPBFILE.SPB_INSRATE_11_19,
                   SPBFILE.SPB_INSTERM_11_20,
                   SPBFILE.SPB_INSRATE_11_20,
                   SPBFILE.SPB_INSMINPREM_11,
                   SPBFILE.SPB_INSMINCHGABL_11,
                   SPBFILE.SPB_INSMAXCHGABL_11,
                   SPBFILE.SPB_INSMAXBENIF_11,
                   SPBFILE.SPB_INSMAXTERM_11,
                   ISNULL(SPBFILE.SPB_INSCHGBEN_11,''),
                   SPBFILE.SPB_INSEXCEPT_11,
                   SPBFILE.SPB_INSNOVER_11,
                   SPBFILE.SPB_INSMINAGE_11_1,
                   SPBFILE.SPB_INSMAXAGE_11_1,
                   SPBFILE.SPB_INSMAXCOV_11_1,
                   SPBFILE.SPB_INSMAXTRM_11_1,
                   SPBFILE.SPB_INSMINAGE_11_2,
                   SPBFILE.SPB_INSMAXAGE_11_2,
                   SPBFILE.SPB_INSMAXCOV_11_2,
                   SPBFILE.SPB_INSMAXTRM_11_2,
                   SPBFILE.SPB_INSMINAGE_11_3,
                   SPBFILE.SPB_INSMAXAGE_11_3,
                   SPBFILE.SPB_INSMAXCOV_11_3,
                   SPBFILE.SPB_INSMAXTRM_11_3,
                   SPBFILE.SPB_INSMINAGE_11_4,
                   SPBFILE.SPB_INSMAXAGE_11_4,
                   SPBFILE.SPB_INSMAXCOV_11_4,
                   SPBFILE.SPB_INSMAXTRM_11_4,
                   SPBFILE.SPB_INSMINAGE_11_5,
                   SPBFILE.SPB_INSMAXAGE_11_5,
                   SPBFILE.SPB_INSMAXCOV_11_5,
                   SPBFILE.SPB_INSMAXTRM_11_5,
                   ISNULL(SPBFILE.SPB_INSFEECD_11,''),
                   ISNULL(SPBFILE.SPB_INSFEE_FRMLA_11,''),
                   ISNULL(SPBFILE.SPB_INSCHGABL_FG_11,''),
                   ISNULL(SPBFILE.SPB_INSREJECT_FG_11,''),
                   ISNULL(SPBFILE.SPB_INSFINCHG_11,''),
                   SPBFILE.SPB_INSCOM_12,
                   SPBFILE.SPB_INSRATECD_12,
                   SPBFILE.SPB_INSJFAC_12,
                   SPBFILE.SPB_INSDISCNT_12,
                   ISNULL(SPBFILE.SPB_INSCHGABL_12,''),
                   SPBFILE.SPB_INSTERM_12_1,
                   SPBFILE.SPB_INSRATE_12_1,
                   SPBFILE.SPB_INSTERM_12_2,
                   SPBFILE.SPB_INSRATE_12_2,
                   SPBFILE.SPB_INSTERM_12_3,
                   SPBFILE.SPB_INSRATE_12_3,
                   SPBFILE.SPB_INSTERM_12_4,
                   SPBFILE.SPB_INSRATE_12_4,
                   SPBFILE.SPB_INSTERM_12_5,
                   SPBFILE.SPB_INSRATE_12_5,
                   SPBFILE.SPB_INSTERM_12_6,
                   SPBFILE.SPB_INSRATE_12_6,
                   SPBFILE.SPB_INSTERM_12_7,
                   SPBFILE.SPB_INSRATE_12_7,
                   SPBFILE.SPB_INSTERM_12_8,
                   SPBFILE.SPB_INSRATE_12_8,
                   SPBFILE.SPB_INSTERM_12_9,
                   SPBFILE.SPB_INSRATE_12_9,
                   SPBFILE.SPB_INSTERM_12_10,
                   SPBFILE.SPB_INSRATE_12_10,
                   SPBFILE.SPB_INSTERM_12_11,
                   SPBFILE.SPB_INSRATE_12_11,
                   SPBFILE.SPB_INSTERM_12_12,
                   SPBFILE.SPB_INSRATE_12_12,
                   SPBFILE.SPB_INSTERM_12_13,
                   SPBFILE.SPB_INSRATE_12_13,
                   SPBFILE.SPB_INSTERM_12_14,
                   SPBFILE.SPB_INSRATE_12_14,
                   SPBFILE.SPB_INSTERM_12_15,
                   SPBFILE.SPB_INSRATE_12_15,
                   SPBFILE.SPB_INSTERM_12_16,
                   SPBFILE.SPB_INSRATE_12_16,
                   SPBFILE.SPB_INSTERM_12_17,
                   SPBFILE.SPB_INSRATE_12_17,
                   SPBFILE.SPB_INSTERM_12_18,
                   SPBFILE.SPB_INSRATE_12_18,
                   SPBFILE.SPB_INSTERM_12_19,
                   SPBFILE.SPB_INSRATE_12_19,
                   SPBFILE.SPB_INSTERM_12_20,
                   SPBFILE.SPB_INSRATE_12_20,
                   SPBFILE.SPB_INSMINPREM_12,
                   SPBFILE.SPB_INSMINCHGABL_12,
                   SPBFILE.SPB_INSMAXCHGABL_12,
                   SPBFILE.SPB_INSMAXBENIF_12,
                   SPBFILE.SPB_INSMAXTERM_12,
                   ISNULL(SPBFILE.SPB_INSCHGBEN_12,''),
                   SPBFILE.SPB_INSEXCEPT_12,
                   SPBFILE.SPB_INSNOVER_12,
                   SPBFILE.SPB_INSMINAGE_12_1,
                   SPBFILE.SPB_INSMAXAGE_12_1,
                   SPBFILE.SPB_INSMAXCOV_12_1,
                   SPBFILE.SPB_INSMAXTRM_12_1,
                   SPBFILE.SPB_INSMINAGE_12_2,
                   SPBFILE.SPB_INSMAXAGE_12_2,
                   SPBFILE.SPB_INSMAXCOV_12_2,
                   SPBFILE.SPB_INSMAXTRM_12_2,
                   SPBFILE.SPB_INSMINAGE_12_3,
                   SPBFILE.SPB_INSMAXAGE_12_3,
                   SPBFILE.SPB_INSMAXCOV_12_3,
                   SPBFILE.SPB_INSMAXTRM_12_3,
                   SPBFILE.SPB_INSMINAGE_12_4,
                   SPBFILE.SPB_INSMAXAGE_12_4,
                   SPBFILE.SPB_INSMAXCOV_12_4,
                   SPBFILE.SPB_INSMAXTRM_12_4,
                   SPBFILE.SPB_INSMINAGE_12_5,
                   SPBFILE.SPB_INSMAXAGE_12_5,
                   SPBFILE.SPB_INSMAXCOV_12_5,
                   SPBFILE.SPB_INSMAXTRM_12_5,
                   ISNULL(SPBFILE.SPB_INSFEECD_12,''),
                   ISNULL(SPBFILE.SPB_INSFEE_FRMLA_12,''),
                   ISNULL(SPBFILE.SPB_INSCHGABL_FG_12,''),
                   ISNULL(SPBFILE.SPB_INSREJECT_FG_12,''),
                   ISNULL(SPBFILE.SPB_INSFINCHG_12,'')
            INTO :QSPB-REC
            FROM DBO.SPBFILE
            WHERE SPBFILE.SPB_ORGST = :QSPB-ORGST
              AND SPBFILE.SPB_SPRCLASS = :QSPB-SPRCLASS
              AND SPBFILE.SPB_SUBCLASS = :QSPB-SUBCLASS
              AND SPBFILE.SPB_LAWCODE = :QSPB-LAWCODE 
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-SPB1-FILE.

           IF ( IO-FG = 0 )
              PERFORM GET-SPB-FIELDS.

      *-----------------------------------------------------------------
       CLOSE-SPB1-FILE.

      *=================================================================
       LOAD-SPC1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( SP-PATH-OVERRIDE = "Y" )
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( SP-PATH-OVERRIDE = SPACES )
              MOVE EXT-FILPATH-BASE        TO SPC-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO SPC-ALL-MACHINE
              MOVE SPC-ALL-PATH             TO SPC-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( SP-PATH-OVERRIDE = "B" )
              MOVE FILPATH-BASE            TO SPC-ALL-BASE
              MOVE FILPATH-MACHINE         TO SPC-ALL-MACHINE
              MOVE SPC-ALL-PATH             TO SPC-PATH.

      *-----------------------------------------------------------------
       OPEN-SPC1-FILE.
           PERFORM LOAD-SPC1-FILE.
           PERFORM OPEN-IT.
           MOVE SPC-PATH-SQL TO E-FILE.

      *-----------------------------------------------------------------
       READ-SPC1-FILE.
           PERFORM READ-IT.
           MOVE SPC-PATH-SQL TO E-FILE.
           MOVE SPC1-KEY TO E-KEYX.

           MOVE SPC-ORGST            TO QSPC-ORGST.
           MOVE SPC-SPRCLASS         TO QSPC-SPRCLASS.
           MOVE SPC-SUBCLASS         TO QSPC-SUBCLASS.
           MOVE SPC-LAWCODE          TO QSPC-LAWCODE.

           EXEC SQL
            SELECT ISNULL(SPCFILE.SPC_ORGST,''),
                   SPCFILE.SPC_SPRCLASS,
                   SPCFILE.SPC_SUBCLASS,
                   SPCFILE.SPC_LAWCODE,
                   SPCFILE.SPC_INSEARN_ORDER_1,
                   SPCFILE.SPC_INSEARN_ORDER_2,
                   SPCFILE.SPC_INSEARN_ORDER_3,
                   SPCFILE.SPC_INSEARN_ORDER_4,
                   SPCFILE.SPC_INSEARN_ORDER_5,
                   SPCFILE.SPC_INSEARN_ORDER_6,
                   SPCFILE.SPC_INSEARN_ORDER_7,
                   SPCFILE.SPC_INSEARN_ORDER_8,
                   SPCFILE.SPC_INSEARN_ORDER_9,
                   SPCFILE.SPC_INSEARN_ORDER_10,
                   SPCFILE.SPC_INSEARN_ORDER_11,
                   SPCFILE.SPC_INSEARN_ORDER_12,
                   ISNULL(SPCFILE.SPC_INSAH_JOINT_FG,''),
                   ISNULL(SPCFILE.SPC_INS_O1_EDIT,''),
                   ISNULL(SPCFILE.SPC_INS_O2_EDIT,''),
                   ISNULL(SPCFILE.SPC_RBFRMLA_1,''),
                   ISNULL(SPCFILE.SPC_RBDTE_1_1,''),
                   SPCFILE.SPC_RBDAYS_1_1,
                   SPCFILE.SPC_RBYRTYPE_1_1,
                   ISNULL(SPCFILE.SPC_RBDTE_1_2,''),
                   SPCFILE.SPC_RBDAYS_1_2,
                   SPCFILE.SPC_RBYRTYPE_1_2,
                   SPCFILE.SPC_RBMINRET_1,
                   SPCFILE.SPC_RBNOMON_1,
                   SPCFILE.SPC_RBDEDCHG_1,
                   SPCFILE.SPC_RBRECAST_1,
                   SPCFILE.SPC_RBEARLY_1,
                   SPCFILE.SPC_RBDEFER_1,
                   SPCFILE.SPC_RBPCT_1,
                   SPCFILE.SPC_RBREDUC_1,
                   SPCFILE.SPC_RBMIN_1,
                   SPCFILE.SPC_RBSPOPT1_1,
                   SPCFILE.SPC_RBSPOPT2_1,
                   SPCFILE.SPC_RBMANAGER_1,
                   ISNULL(SPCFILE.SPC_RBUNITPER_CD_1,''),
                   ISNULL(SPCFILE.SPC_RBFRMLA2_1,''),
                   ISNULL(SPCFILE.SPC_RBFRMLA_2,''),
                   ISNULL(SPCFILE.SPC_RBDTE_2_1,''),
                   SPCFILE.SPC_RBDAYS_2_1,
                   SPCFILE.SPC_RBYRTYPE_2_1,
                   ISNULL(SPCFILE.SPC_RBDTE_2_2,''),
                   SPCFILE.SPC_RBDAYS_2_2,
                   SPCFILE.SPC_RBYRTYPE_2_2,
                   SPCFILE.SPC_RBMINRET_2,
                   SPCFILE.SPC_RBNOMON_2,
                   SPCFILE.SPC_RBDEDCHG_2,
                   SPCFILE.SPC_RBRECAST_2,
                   SPCFILE.SPC_RBEARLY_2,
                   SPCFILE.SPC_RBDEFER_2,
                   SPCFILE.SPC_RBPCT_2,
                   SPCFILE.SPC_RBREDUC_2,
                   SPCFILE.SPC_RBMIN_2,
                   SPCFILE.SPC_RBSPOPT1_2,
                   SPCFILE.SPC_RBSPOPT2_2,
                   SPCFILE.SPC_RBMANAGER_2,
                   ISNULL(SPCFILE.SPC_RBUNITPER_CD_2,''),
                   ISNULL(SPCFILE.SPC_RBFRMLA2_2,''),
                   ISNULL(SPCFILE.SPC_RBFRMLA_3,''),
                   ISNULL(SPCFILE.SPC_RBDTE_3_1,''),
                   SPCFILE.SPC_RBDAYS_3_1,
                   SPCFILE.SPC_RBYRTYPE_3_1,
                   ISNULL(SPCFILE.SPC_RBDTE_3_2,''),
                   SPCFILE.SPC_RBDAYS_3_2,
                   SPCFILE.SPC_RBYRTYPE_3_2,
                   SPCFILE.SPC_RBMINRET_3,
                   SPCFILE.SPC_RBNOMON_3,
                   SPCFILE.SPC_RBDEDCHG_3,
                   SPCFILE.SPC_RBRECAST_3,
                   SPCFILE.SPC_RBEARLY_3,
                   SPCFILE.SPC_RBDEFER_3,
                   SPCFILE.SPC_RBPCT_3,
                   SPCFILE.SPC_RBREDUC_3,
                   SPCFILE.SPC_RBMIN_3,
                   SPCFILE.SPC_RBSPOPT1_3,
                   SPCFILE.SPC_RBSPOPT2_3,
                   SPCFILE.SPC_RBMANAGER_3,
                   ISNULL(SPCFILE.SPC_RBUNITPER_CD_3,''),
                   ISNULL(SPCFILE.SPC_RBFRMLA2_3,''),
                   ISNULL(SPCFILE.SPC_RBFRMLA_4,''),
                   ISNULL(SPCFILE.SPC_RBDTE_4_1,''),
                   SPCFILE.SPC_RBDAYS_4_1,
                   SPCFILE.SPC_RBYRTYPE_4_1,
                   ISNULL(SPCFILE.SPC_RBDTE_4_2,''),
                   SPCFILE.SPC_RBDAYS_4_2,
                   SPCFILE.SPC_RBYRTYPE_4_2,
                   SPCFILE.SPC_RBMINRET_4,
                   SPCFILE.SPC_RBNOMON_4,
                   SPCFILE.SPC_RBDEDCHG_4,
                   SPCFILE.SPC_RBRECAST_4,
                   SPCFILE.SPC_RBEARLY_4,
                   SPCFILE.SPC_RBDEFER_4,
                   SPCFILE.SPC_RBPCT_4,
                   SPCFILE.SPC_RBREDUC_4,
                   SPCFILE.SPC_RBMIN_4,
                   SPCFILE.SPC_RBSPOPT1_4,
                   SPCFILE.SPC_RBSPOPT2_4,
                   SPCFILE.SPC_RBMANAGER_4,
                   ISNULL(SPCFILE.SPC_RBUNITPER_CD_4,''),
                   ISNULL(SPCFILE.SPC_RBFRMLA2_4,''),
                   ISNULL(SPCFILE.SPC_RBFRMLA_5,''),
                   ISNULL(SPCFILE.SPC_RBDTE_5_1,''),
                   SPCFILE.SPC_RBDAYS_5_1,
                   SPCFILE.SPC_RBYRTYPE_5_1,
                   ISNULL(SPCFILE.SPC_RBDTE_5_2,''),
                   SPCFILE.SPC_RBDAYS_5_2,
                   SPCFILE.SPC_RBYRTYPE_5_2,
                   SPCFILE.SPC_RBMINRET_5,
                   SPCFILE.SPC_RBNOMON_5,
                   SPCFILE.SPC_RBDEDCHG_5,
                   SPCFILE.SPC_RBRECAST_5,
                   SPCFILE.SPC_RBEARLY_5,
                   SPCFILE.SPC_RBDEFER_5,
                   SPCFILE.SPC_RBPCT_5,
                   SPCFILE.SPC_RBREDUC_5,
                   SPCFILE.SPC_RBMIN_5,
                   SPCFILE.SPC_RBSPOPT1_5,
                   SPCFILE.SPC_RBSPOPT2_5,
                   SPCFILE.SPC_RBMANAGER_5,
                   ISNULL(SPCFILE.SPC_RBUNITPER_CD_5,''),
                   ISNULL(SPCFILE.SPC_RBFRMLA2_5,''),
                   ISNULL(SPCFILE.SPC_RBFRMLA_6,''),
                   ISNULL(SPCFILE.SPC_RBDTE_6_1,''),
                   SPCFILE.SPC_RBDAYS_6_1,
                   SPCFILE.SPC_RBYRTYPE_6_1,
                   ISNULL(SPCFILE.SPC_RBDTE_6_2,''),
                   SPCFILE.SPC_RBDAYS_6_2,
                   SPCFILE.SPC_RBYRTYPE_6_2,
                   SPCFILE.SPC_RBMINRET_6,
                   SPCFILE.SPC_RBNOMON_6,
                   SPCFILE.SPC_RBDEDCHG_6,
                   SPCFILE.SPC_RBRECAST_6,
                   SPCFILE.SPC_RBEARLY_6,
                   SPCFILE.SPC_RBDEFER_6,
                   SPCFILE.SPC_RBPCT_6,
                   SPCFILE.SPC_RBREDUC_6,
                   SPCFILE.SPC_RBMIN_6,
                   SPCFILE.SPC_RBSPOPT1_6,
                   SPCFILE.SPC_RBSPOPT2_6,
                   SPCFILE.SPC_RBMANAGER_6,
                   ISNULL(SPCFILE.SPC_RBUNITPER_CD_6,''),
                   ISNULL(SPCFILE.SPC_RBFRMLA2_6,''),
                   ISNULL(SPCFILE.SPC_RBFRMLA_7,''),
                   ISNULL(SPCFILE.SPC_RBDTE_7_1,''),
                   SPCFILE.SPC_RBDAYS_7_1,
                   SPCFILE.SPC_RBYRTYPE_7_1,
                   ISNULL(SPCFILE.SPC_RBDTE_7_2,''),
                   SPCFILE.SPC_RBDAYS_7_2,
                   SPCFILE.SPC_RBYRTYPE_7_2,
                   SPCFILE.SPC_RBMINRET_7,
                   SPCFILE.SPC_RBNOMON_7,
                   SPCFILE.SPC_RBDEDCHG_7,
                   SPCFILE.SPC_RBRECAST_7,
                   SPCFILE.SPC_RBEARLY_7,
                   SPCFILE.SPC_RBDEFER_7,
                   SPCFILE.SPC_RBPCT_7,
                   SPCFILE.SPC_RBREDUC_7,
                   SPCFILE.SPC_RBMIN_7,
                   SPCFILE.SPC_RBSPOPT1_7,
                   SPCFILE.SPC_RBSPOPT2_7,
                   SPCFILE.SPC_RBMANAGER_7,
                   ISNULL(SPCFILE.SPC_RBUNITPER_CD_7,''),
                   ISNULL(SPCFILE.SPC_RBFRMLA2_7,''),
                   ISNULL(SPCFILE.SPC_RBFRMLA_8,''),
                   ISNULL(SPCFILE.SPC_RBDTE_8_1,''),
                   SPCFILE.SPC_RBDAYS_8_1,
                   SPCFILE.SPC_RBYRTYPE_8_1,
                   ISNULL(SPCFILE.SPC_RBDTE_8_2,''),
                   SPCFILE.SPC_RBDAYS_8_2,
                   SPCFILE.SPC_RBYRTYPE_8_2,
                   SPCFILE.SPC_RBMINRET_8,
                   SPCFILE.SPC_RBNOMON_8,
                   SPCFILE.SPC_RBDEDCHG_8,
                   SPCFILE.SPC_RBRECAST_8,
                   SPCFILE.SPC_RBEARLY_8,
                   SPCFILE.SPC_RBDEFER_8,
                   SPCFILE.SPC_RBPCT_8,
                   SPCFILE.SPC_RBREDUC_8,
                   SPCFILE.SPC_RBMIN_8,
                   SPCFILE.SPC_RBSPOPT1_8,
                   SPCFILE.SPC_RBSPOPT2_8,
                   SPCFILE.SPC_RBMANAGER_8,
                   ISNULL(SPCFILE.SPC_RBUNITPER_CD_8,''),
                   ISNULL(SPCFILE.SPC_RBFRMLA2_8,''),
                   ISNULL(SPCFILE.SPC_RBFRMLA_9,''),
                   ISNULL(SPCFILE.SPC_RBDTE_9_1,''),
                   SPCFILE.SPC_RBDAYS_9_1,
                   SPCFILE.SPC_RBYRTYPE_9_1,
                   ISNULL(SPCFILE.SPC_RBDTE_9_2,''),
                   SPCFILE.SPC_RBDAYS_9_2,
                   SPCFILE.SPC_RBYRTYPE_9_2,
                   SPCFILE.SPC_RBMINRET_9,
                   SPCFILE.SPC_RBNOMON_9,
                   SPCFILE.SPC_RBDEDCHG_9,
                   SPCFILE.SPC_RBRECAST_9,
                   SPCFILE.SPC_RBEARLY_9,
                   SPCFILE.SPC_RBDEFER_9,
                   SPCFILE.SPC_RBPCT_9,
                   SPCFILE.SPC_RBREDUC_9,
                   SPCFILE.SPC_RBMIN_9,
                   SPCFILE.SPC_RBSPOPT1_9,
                   SPCFILE.SPC_RBSPOPT2_9,
                   SPCFILE.SPC_RBMANAGER_9,
                   ISNULL(SPCFILE.SPC_RBUNITPER_CD_9,''),
                   ISNULL(SPCFILE.SPC_RBFRMLA2_9,''),
                   ISNULL(SPCFILE.SPC_RBFRMLA_10,''),
                   ISNULL(SPCFILE.SPC_RBDTE_10_1,''),
                   SPCFILE.SPC_RBDAYS_10_1,
                   SPCFILE.SPC_RBYRTYPE_10_1,
                   ISNULL(SPCFILE.SPC_RBDTE_10_2,''),
                   SPCFILE.SPC_RBDAYS_10_2,
                   SPCFILE.SPC_RBYRTYPE_10_2,
                   SPCFILE.SPC_RBMINRET_10,
                   SPCFILE.SPC_RBNOMON_10,
                   SPCFILE.SPC_RBDEDCHG_10,
                   SPCFILE.SPC_RBRECAST_10,
                   SPCFILE.SPC_RBEARLY_10,
                   SPCFILE.SPC_RBDEFER_10,
                   SPCFILE.SPC_RBPCT_10,
                   SPCFILE.SPC_RBREDUC_10,
                   SPCFILE.SPC_RBMIN_10,
                   SPCFILE.SPC_RBSPOPT1_10,
                   SPCFILE.SPC_RBSPOPT2_10,
                   SPCFILE.SPC_RBMANAGER_10,
                   ISNULL(SPCFILE.SPC_RBUNITPER_CD_10,''),
                   ISNULL(SPCFILE.SPC_RBFRMLA2_10,''),
                   ISNULL(SPCFILE.SPC_RBFRMLA_11,''),
                   ISNULL(SPCFILE.SPC_RBDTE_11_1,''),
                   SPCFILE.SPC_RBDAYS_11_1,
                   SPCFILE.SPC_RBYRTYPE_11_1,
                   ISNULL(SPCFILE.SPC_RBDTE_11_2,''),
                   SPCFILE.SPC_RBDAYS_11_2,
                   SPCFILE.SPC_RBYRTYPE_11_2,
                   SPCFILE.SPC_RBMINRET_11,
                   SPCFILE.SPC_RBNOMON_11,
                   SPCFILE.SPC_RBDEDCHG_11,
                   SPCFILE.SPC_RBRECAST_11,
                   SPCFILE.SPC_RBEARLY_11,
                   SPCFILE.SPC_RBDEFER_11,
                   SPCFILE.SPC_RBPCT_11,
                   SPCFILE.SPC_RBREDUC_11,
                   SPCFILE.SPC_RBMIN_11,
                   SPCFILE.SPC_RBSPOPT1_11,
                   SPCFILE.SPC_RBSPOPT2_11,
                   SPCFILE.SPC_RBMANAGER_11,
                   ISNULL(SPCFILE.SPC_RBUNITPER_CD_11,''),
                   ISNULL(SPCFILE.SPC_RBFRMLA2_11,''),
                   ISNULL(SPCFILE.SPC_RBFRMLA_12,''),
                   ISNULL(SPCFILE.SPC_RBDTE_12_1,''),
                   SPCFILE.SPC_RBDAYS_12_1,
                   SPCFILE.SPC_RBYRTYPE_12_1,
                   ISNULL(SPCFILE.SPC_RBDTE_12_2,''),
                   SPCFILE.SPC_RBDAYS_12_2,
                   SPCFILE.SPC_RBYRTYPE_12_2,
                   SPCFILE.SPC_RBMINRET_12,
                   SPCFILE.SPC_RBNOMON_12,
                   SPCFILE.SPC_RBDEDCHG_12,
                   SPCFILE.SPC_RBRECAST_12,
                   SPCFILE.SPC_RBEARLY_12,
                   SPCFILE.SPC_RBDEFER_12,
                   SPCFILE.SPC_RBPCT_12,
                   SPCFILE.SPC_RBREDUC_12,
                   SPCFILE.SPC_RBMIN_12,
                   SPCFILE.SPC_RBSPOPT1_12,
                   SPCFILE.SPC_RBSPOPT2_12,
                   SPCFILE.SPC_RBMANAGER_12,
                   ISNULL(SPCFILE.SPC_RBUNITPER_CD_12,''),
                   ISNULL(SPCFILE.SPC_RBFRMLA2_12,''),
                   ISNULL(SPCFILE.SPC_RBFRMLA_13,''),
                   ISNULL(SPCFILE.SPC_RBDTE_13_1,''),
                   SPCFILE.SPC_RBDAYS_13_1,
                   SPCFILE.SPC_RBYRTYPE_13_1,
                   ISNULL(SPCFILE.SPC_RBDTE_13_2,''),
                   SPCFILE.SPC_RBDAYS_13_2,
                   SPCFILE.SPC_RBYRTYPE_13_2,
                   SPCFILE.SPC_RBMINRET_13,
                   SPCFILE.SPC_RBNOMON_13,
                   SPCFILE.SPC_RBDEDCHG_13,
                   SPCFILE.SPC_RBRECAST_13,
                   SPCFILE.SPC_RBEARLY_13,
                   SPCFILE.SPC_RBDEFER_13,
                   SPCFILE.SPC_RBPCT_13,
                   SPCFILE.SPC_RBREDUC_13,
                   SPCFILE.SPC_RBMIN_13,
                   SPCFILE.SPC_RBSPOPT1_13,
                   SPCFILE.SPC_RBSPOPT2_13,
                   SPCFILE.SPC_RBMANAGER_13,
                   ISNULL(SPCFILE.SPC_RBUNITPER_CD_13,''),
                   ISNULL(SPCFILE.SPC_RBFRMLA2_13,''),
                   ISNULL(SPCFILE.SPC_RBFRMLA_14,''),
                   ISNULL(SPCFILE.SPC_RBDTE_14_1,''),
                   SPCFILE.SPC_RBDAYS_14_1,
                   SPCFILE.SPC_RBYRTYPE_14_1,
                   ISNULL(SPCFILE.SPC_RBDTE_14_2,''),
                   SPCFILE.SPC_RBDAYS_14_2,
                   SPCFILE.SPC_RBYRTYPE_14_2,
                   SPCFILE.SPC_RBMINRET_14,
                   SPCFILE.SPC_RBNOMON_14,
                   SPCFILE.SPC_RBDEDCHG_14,
                   SPCFILE.SPC_RBRECAST_14,
                   SPCFILE.SPC_RBEARLY_14,
                   SPCFILE.SPC_RBDEFER_14,
                   SPCFILE.SPC_RBPCT_14,
                   SPCFILE.SPC_RBREDUC_14,
                   SPCFILE.SPC_RBMIN_14,
                   SPCFILE.SPC_RBSPOPT1_14,
                   SPCFILE.SPC_RBSPOPT2_14,
                   SPCFILE.SPC_RBMANAGER_14,
                   ISNULL(SPCFILE.SPC_RBUNITPER_CD_14,''),
                   ISNULL(SPCFILE.SPC_RBFRMLA2_14,''),
                   ISNULL(SPCFILE.SPC_RBFRMLA_15,''),
                   ISNULL(SPCFILE.SPC_RBDTE_15_1,''),
                   SPCFILE.SPC_RBDAYS_15_1,
                   SPCFILE.SPC_RBYRTYPE_15_1,
                   ISNULL(SPCFILE.SPC_RBDTE_15_2,''),
                   SPCFILE.SPC_RBDAYS_15_2,
                   SPCFILE.SPC_RBYRTYPE_15_2,
                   SPCFILE.SPC_RBMINRET_15,
                   SPCFILE.SPC_RBNOMON_15,
                   SPCFILE.SPC_RBDEDCHG_15,
                   SPCFILE.SPC_RBRECAST_15,
                   SPCFILE.SPC_RBEARLY_15,
                   SPCFILE.SPC_RBDEFER_15,
                   SPCFILE.SPC_RBPCT_15,
                   SPCFILE.SPC_RBREDUC_15,
                   SPCFILE.SPC_RBMIN_15,
                   SPCFILE.SPC_RBSPOPT1_15,
                   SPCFILE.SPC_RBSPOPT2_15,
                   SPCFILE.SPC_RBMANAGER_15,
                   ISNULL(SPCFILE.SPC_RBUNITPER_CD_15,''),
                   ISNULL(SPCFILE.SPC_RBFRMLA2_15,''),
                   ISNULL(SPCFILE.SPC_RBFRMLA_16,''),
                   ISNULL(SPCFILE.SPC_RBDTE_16_1,''),
                   SPCFILE.SPC_RBDAYS_16_1,
                   SPCFILE.SPC_RBYRTYPE_16_1,
                   ISNULL(SPCFILE.SPC_RBDTE_16_2,''),
                   SPCFILE.SPC_RBDAYS_16_2,
                   SPCFILE.SPC_RBYRTYPE_16_2,
                   SPCFILE.SPC_RBMINRET_16,
                   SPCFILE.SPC_RBNOMON_16,
                   SPCFILE.SPC_RBDEDCHG_16,
                   SPCFILE.SPC_RBRECAST_16,
                   SPCFILE.SPC_RBEARLY_16,
                   SPCFILE.SPC_RBDEFER_16,
                   SPCFILE.SPC_RBPCT_16,
                   SPCFILE.SPC_RBREDUC_16,
                   SPCFILE.SPC_RBMIN_16,
                   SPCFILE.SPC_RBSPOPT1_16,
                   SPCFILE.SPC_RBSPOPT2_16,
                   SPCFILE.SPC_RBMANAGER_16,
                   ISNULL(SPCFILE.SPC_RBUNITPER_CD_16,''),
                   ISNULL(SPCFILE.SPC_RBFRMLA2_16,''),
                   ISNULL(SPCFILE.SPC_RBFRMLA_17,''),
                   ISNULL(SPCFILE.SPC_RBDTE_17_1,''),
                   SPCFILE.SPC_RBDAYS_17_1,
                   SPCFILE.SPC_RBYRTYPE_17_1,
                   ISNULL(SPCFILE.SPC_RBDTE_17_2,''),
                   SPCFILE.SPC_RBDAYS_17_2,
                   SPCFILE.SPC_RBYRTYPE_17_2,
                   SPCFILE.SPC_RBMINRET_17,
                   SPCFILE.SPC_RBNOMON_17,
                   SPCFILE.SPC_RBDEDCHG_17,
                   SPCFILE.SPC_RBRECAST_17,
                   SPCFILE.SPC_RBEARLY_17,
                   SPCFILE.SPC_RBDEFER_17,
                   SPCFILE.SPC_RBPCT_17,
                   SPCFILE.SPC_RBREDUC_17,
                   SPCFILE.SPC_RBMIN_17,
                   SPCFILE.SPC_RBSPOPT1_17,
                   SPCFILE.SPC_RBSPOPT2_17,
                   SPCFILE.SPC_RBMANAGER_17,
                   ISNULL(SPCFILE.SPC_RBUNITPER_CD_17,''),
                   ISNULL(SPCFILE.SPC_RBFRMLA2_17,''),
                   ISNULL(SPCFILE.SPC_RBFRMLA_18,''),
                   ISNULL(SPCFILE.SPC_RBDTE_18_1,''),
                   SPCFILE.SPC_RBDAYS_18_1,
                   SPCFILE.SPC_RBYRTYPE_18_1,
                   ISNULL(SPCFILE.SPC_RBDTE_18_2,''),
                   SPCFILE.SPC_RBDAYS_18_2,
                   SPCFILE.SPC_RBYRTYPE_18_2,
                   SPCFILE.SPC_RBMINRET_18,
                   SPCFILE.SPC_RBNOMON_18,
                   SPCFILE.SPC_RBDEDCHG_18,
                   SPCFILE.SPC_RBRECAST_18,
                   SPCFILE.SPC_RBEARLY_18,
                   SPCFILE.SPC_RBDEFER_18,
                   SPCFILE.SPC_RBPCT_18,
                   SPCFILE.SPC_RBREDUC_18,
                   SPCFILE.SPC_RBMIN_18,
                   SPCFILE.SPC_RBSPOPT1_18,
                   SPCFILE.SPC_RBSPOPT2_18,
                   SPCFILE.SPC_RBMANAGER_18,
                   ISNULL(SPCFILE.SPC_RBUNITPER_CD_18,''),
                   ISNULL(SPCFILE.SPC_RBFRMLA2_18,''),
                   ISNULL(SPCFILE.SPC_RBFRMLA_19,''),
                   ISNULL(SPCFILE.SPC_RBDTE_19_1,''),
                   SPCFILE.SPC_RBDAYS_19_1,
                   SPCFILE.SPC_RBYRTYPE_19_1,
                   ISNULL(SPCFILE.SPC_RBDTE_19_2,''),
                   SPCFILE.SPC_RBDAYS_19_2,
                   SPCFILE.SPC_RBYRTYPE_19_2,
                   SPCFILE.SPC_RBMINRET_19,
                   SPCFILE.SPC_RBNOMON_19,
                   SPCFILE.SPC_RBDEDCHG_19,
                   SPCFILE.SPC_RBRECAST_19,
                   SPCFILE.SPC_RBEARLY_19,
                   SPCFILE.SPC_RBDEFER_19,
                   SPCFILE.SPC_RBPCT_19,
                   SPCFILE.SPC_RBREDUC_19,
                   SPCFILE.SPC_RBMIN_19,
                   SPCFILE.SPC_RBSPOPT1_19,
                   SPCFILE.SPC_RBSPOPT2_19,
                   SPCFILE.SPC_RBMANAGER_19,
                   ISNULL(SPCFILE.SPC_RBUNITPER_CD_19,''),
                   ISNULL(SPCFILE.SPC_RBFRMLA2_19,''),
                   ISNULL(SPCFILE.SPC_RBFRMLA_20,''),
                   ISNULL(SPCFILE.SPC_RBDTE_20_1,''),
                   SPCFILE.SPC_RBDAYS_20_1,
                   SPCFILE.SPC_RBYRTYPE_20_1,
                   ISNULL(SPCFILE.SPC_RBDTE_20_2,''),
                   SPCFILE.SPC_RBDAYS_20_2,
                   SPCFILE.SPC_RBYRTYPE_20_2,
                   SPCFILE.SPC_RBMINRET_20,
                   SPCFILE.SPC_RBNOMON_20,
                   SPCFILE.SPC_RBDEDCHG_20,
                   SPCFILE.SPC_RBRECAST_20,
                   SPCFILE.SPC_RBEARLY_20,
                   SPCFILE.SPC_RBDEFER_20,
                   SPCFILE.SPC_RBPCT_20,
                   SPCFILE.SPC_RBREDUC_20,
                   SPCFILE.SPC_RBMIN_20,
                   SPCFILE.SPC_RBSPOPT1_20,
                   SPCFILE.SPC_RBSPOPT2_20,
                   SPCFILE.SPC_RBMANAGER_20,
                   ISNULL(SPCFILE.SPC_RBUNITPER_CD_20,''),
                   ISNULL(SPCFILE.SPC_RBFRMLA2_20,''),
                   ISNULL(SPCFILE.SPC_RBFRMLA_21,''),
                   ISNULL(SPCFILE.SPC_RBDTE_21_1,''),
                   SPCFILE.SPC_RBDAYS_21_1,
                   SPCFILE.SPC_RBYRTYPE_21_1,
                   ISNULL(SPCFILE.SPC_RBDTE_21_2,''),
                   SPCFILE.SPC_RBDAYS_21_2,
                   SPCFILE.SPC_RBYRTYPE_21_2,
                   SPCFILE.SPC_RBMINRET_21,
                   SPCFILE.SPC_RBNOMON_21,
                   SPCFILE.SPC_RBDEDCHG_21,
                   SPCFILE.SPC_RBRECAST_21,
                   SPCFILE.SPC_RBEARLY_21,
                   SPCFILE.SPC_RBDEFER_21,
                   SPCFILE.SPC_RBPCT_21,
                   SPCFILE.SPC_RBREDUC_21,
                   SPCFILE.SPC_RBMIN_21,
                   SPCFILE.SPC_RBSPOPT1_21,
                   SPCFILE.SPC_RBSPOPT2_21,
                   SPCFILE.SPC_RBMANAGER_21,
                   ISNULL(SPCFILE.SPC_RBUNITPER_CD_21,''),
                   ISNULL(SPCFILE.SPC_RBFRMLA2_21,''),
                   ISNULL(SPCFILE.SPC_RBFRMLA_22,''),
                   ISNULL(SPCFILE.SPC_RBDTE_22_1,''),
                   SPCFILE.SPC_RBDAYS_22_1,
                   SPCFILE.SPC_RBYRTYPE_22_1,
                   ISNULL(SPCFILE.SPC_RBDTE_22_2,''),
                   SPCFILE.SPC_RBDAYS_22_2,
                   SPCFILE.SPC_RBYRTYPE_22_2,
                   SPCFILE.SPC_RBMINRET_22,
                   SPCFILE.SPC_RBNOMON_22,
                   SPCFILE.SPC_RBDEDCHG_22,
                   SPCFILE.SPC_RBRECAST_22,
                   SPCFILE.SPC_RBEARLY_22,
                   SPCFILE.SPC_RBDEFER_22,
                   SPCFILE.SPC_RBPCT_22,
                   SPCFILE.SPC_RBREDUC_22,
                   SPCFILE.SPC_RBMIN_22,
                   SPCFILE.SPC_RBSPOPT1_22,
                   SPCFILE.SPC_RBSPOPT2_22,
                   SPCFILE.SPC_RBMANAGER_22,
                   ISNULL(SPCFILE.SPC_RBUNITPER_CD_22,''),
                   ISNULL(SPCFILE.SPC_RBFRMLA2_22,''),
                   ISNULL(SPCFILE.SPC_RBCOMCD,''),
                   ISNULL(SPCFILE.SPC_ADDON_DISFRMLA,''),
                   ISNULL(SPCFILE.SPC_ADDON_WHOLE_FG,''),
                   ISNULL(SPCFILE.SPC_ADDON_ORG_MATURITY,''),
                   ISNULL(SPCFILE.SPC_CONSOLIDATE_RT_FG,''),
                   SPCFILE.SPC_1STPY_ADDON_DAYS,
                   SPCFILE.SPC_1STPY_ADDON_YRTYPE,
                   ISNULL(SPCFILE.SPC_ADDON_RATE_FG,''),
                   SPCFILE.SPC_ADDON_RATE,
                   ISNULL(SPCFILE.SPC_FEEFRMLA_1,''),
                   ISNULL(SPCFILE.SPC_FEECD_1,''),
                   ISNULL(SPCFILE.SPC_FEEID_1,''),
                   ISNULL(SPCFILE.SPC_FEEFRMLA_2,''),
                   ISNULL(SPCFILE.SPC_FEECD_2,''),
                   ISNULL(SPCFILE.SPC_FEEID_2,''),
                   ISNULL(SPCFILE.SPC_ROUND_PAY_FG,''),
                   ISNULL(SPCFILE.SPC_IRREGULAR_PAY_FG,''),
                   ISNULL(SPCFILE.SPC_DLRVCHGS_RB_CD_1,''),
                   ISNULL(SPCFILE.SPC_DLRVCHGS_RB_CD_2,''),
                   ISNULL(SPCFILE.SPC_DLRVCHGS_RB_CD_3,''),
                   ISNULL(SPCFILE.SPC_PARVHELD_RB_CD,''),
                   ISNULL(SPCFILE.SPC_PARVPAID_RB_CD,''),
                   SPCFILE.SPC_CONTR_CREDIT,
                   ISNULL(SPCFILE.SPC_CONTR_CREDIT_CD,''),
                   SPCFILE.SPC_CONTR_CREDIT_MIN_DEL_PER,
                   ISNULL(SPCFILE.SPC_OPT_CAL_CHGABLE,''),
                   SPCFILE.SPC_PBDEL_DAYS,
                   SPCFILE.SPC_PBINCR_PERCENT,
                   ISNULL(SPCFILE.SPC_CPOT30_CD,''),
                   ISNULL(SPCFILE.SPC_CPOTCHGOFF_CD,''),
                   ISNULL(SPCFILE.SPC_RDPOT30_CD,''),
                   ISNULL(SPCFILE.SPC_RDPOTCHGOFF_CD,''),
                   SPCFILE.SPC_MAX_ALLOW_DELINQ,
                   ISNULL(SPCFILE.SPC_RESCIND_BUS_DAYS,''),
                   ISNULL(SPCFILE.SPC_RESCIND_SATURDAY,''),
                   SPCFILE.SPC_MIN_CREDIT_SCORE,
                   ISNULL(SPCFILE.SPC_DUAL_IN_BRANCH,''),
                   ISNULL(SPCFILE.SPC_DUAL_IN_STATE,''),
                   ISNULL(SPCFILE.SPC_PACESETTER_FG,''),
                   SPCFILE.SPC_WAGE_ASSIGN_DAYS,
                   ISNULL(SPCFILE.SPC_CAL_PAYMENT_ROUND_FG,''),
                   ISNULL(SPCFILE.SPC_CAN_LOANS_FG,''),
                   ISNULL(SPCFILE.SPC_ALLOW_DELINQ_POLCD,''),
                   SPCFILE.SPC_YTD_MAX_ALLOW_DELINQ,
                   ISNULL(SPCFILE.SPC_LNCOST_FRMLA,''),
                   SPCFILE.SPC_LNCOST_AMT,
                   SPCFILE.SPC_DLDISC_RVPRIN1_PCNT,
                   SPCFILE.SPC_RENEWABLE_PAYS,
                   SPCFILE.SPC_RENEWABLE_DAYS,
                   ISNULL(SPCFILE.SPC_ALLOW_LN_BACK_DATE,''),
                   SPCFILE.SPC_DLDISC_RVPRIN1_AF_PCNT,
                   ISNULL(SPCFILE.SPC_CAL_MAX_AT_ESTIMATE,''),
                   ISNULL(SPCFILE.SPC_INSAH_RETRO,''),
                   ISNULL(SPCFILE.SPC_INSAH_DAYS_WAIT,''),
                   ISNULL(SPCFILE.SPC_ADDON_EXCEPT_FG,''),
                   ISNULL(SPCFILE.SPC_REPO_TRCD_1,''),
                   SPCFILE.SPC_REPO_FMNO_1_1,
                   SPCFILE.SPC_REPO_FMNO_1_2,
                   SPCFILE.SPC_REPO_FMNO_1_3,
                   SPCFILE.SPC_REPO_FMNO_1_4,
                   ISNULL(SPCFILE.SPC_REPO_TRCD_2,''),
                   SPCFILE.SPC_REPO_FMNO_2_1,
                   SPCFILE.SPC_REPO_FMNO_2_2,
                   SPCFILE.SPC_REPO_FMNO_2_3,
                   SPCFILE.SPC_REPO_FMNO_2_4,
                   ISNULL(SPCFILE.SPC_REPO_TRCD_3,''),
                   SPCFILE.SPC_REPO_FMNO_3_1,
                   SPCFILE.SPC_REPO_FMNO_3_2,
                   SPCFILE.SPC_REPO_FMNO_3_3,
                   SPCFILE.SPC_REPO_FMNO_3_4,
                   ISNULL(SPCFILE.SPC_CANCEL_ADDON_FG,''),
                   SPCFILE.SPC_DEFAULT_O2_INS_COMP,
                   ISNULL(SPCFILE.SPC_LC_ON_JUDGEMENTS,''),
                   SPCFILE.SPC_DELQ_IB_AGE,
                   SPCFILE.SPC_DELQ_IB_MAX_DAYS,
                   ISNULL(SPCFILE.SPC_INSURANCE_WRITTEN,''),
                   ISNULL(SPCFILE.SPC_INTERIM_COUPON,''),
                   ISNULL(SPCFILE.SPC_AUTO_ASSESS_LCHG,''),
                   SPCFILE.SPC_MAX_CASH_PROCEEDS,
                   ISNULL(SPCFILE.SPC_ALLOW_ZERO_PROC,''),
                   SPCFILE.SPC_BANKRULE_RATE,
                   ISNULL(SPCFILE.SPC_AUTO_SEND_TRCD_1,''),
                   SPCFILE.SPC_AUTO_SEND_FMNO_1_1,
                   SPCFILE.SPC_AUTO_SEND_FMNO_1_2,
                   SPCFILE.SPC_AUTO_SEND_FMNO_1_3,
                   SPCFILE.SPC_AUTO_SEND_FMNO_1_4,
                   ISNULL(SPCFILE.SPC_AUTO_SEND_TRCD_2,''),
                   SPCFILE.SPC_AUTO_SEND_FMNO_2_1,
                   SPCFILE.SPC_AUTO_SEND_FMNO_2_2,
                   SPCFILE.SPC_AUTO_SEND_FMNO_2_3,
                   SPCFILE.SPC_AUTO_SEND_FMNO_2_4,
                   SPCFILE.SPC_DLDISC_CONV_BF_PCNT,
                   ISNULL(SPCFILE.SPC_UNITPER_CD,''),
                   ISNULL(SPCFILE.SPC_RENEWAL_EDIT,''),
                   ISNULL(SPCFILE.SPC_ROUND_MFEE,''),
                   ISNULL(SPCFILE.SPC_ADV_TO_1ST_NO_CORR,''),
                   ISNULL(SPCFILE.SPC_LC_INT_ON_REPO_NON_PL,''),
                   SPCFILE.SPC_CONTRACTUAL_SET_NONACCRUAL,
                   SPCFILE.SPC_CONTRACTUAL_RESET_ACCRUAL,
                   SPCFILE.SPC_MONTHS_PAY_NAPP_COUNTER,
                   SPCFILE.SPC_SHORT_PAY_THRESHOLD,
                   SPCFILE.SPC_CASH_AVAIL_PERCENT,
                   ISNULL(SPCFILE.SPC_O1_JOINT_FG,''),
                   SPCFILE.SPC_NET_BAL_REQ,
                   SPCFILE.SPC_AMT_PROCEEDS_REQ,
                   SPCFILE.SPC_ALT_PREPAY_MONTHS,
                   SPCFILE.SPC_PERCENT_REGPAY_AVAIL,
                   SPCFILE.SPC_ALT_DEFFRMLA,
                   SPCFILE.SPC_ALT_DEFMIN,
                   SPCFILE.SPC_ALT_DEFMAX,
                   ISNULL(SPCFILE.SPC_NON_LOAN_FLAG,''),
                   ISNULL(SPCFILE.SPC_SKIP_TRW,'')
            INTO :QSPC-REC
            FROM DBO.SPCFILE
            WHERE SPCFILE.SPC_ORGST = :QSPC-ORGST
              AND SPCFILE.SPC_SPRCLASS = :QSPC-SPRCLASS
              AND SPCFILE.SPC_SUBCLASS = :QSPC-SUBCLASS
              AND SPCFILE.SPC_LAWCODE = :QSPC-LAWCODE 
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-SPC1-FILE.

           IF ( IO-FG = 0 )
              PERFORM GET-SPC-FIELDS.

      *-----------------------------------------------------------------
       CLOSE-SPC1-FILE.


      *COPY "LIBLP/LPLTP1RN.CPY".
      * COPYMEMBER: LIBLP/LPLTP1RN
      *=================================================================
       GET-LT-P-REC.
           MOVE 0 TO IO-FG.
           IF NOT (LN-ACCTNO = LTP-ACCTNO)
              MOVE LN-OWNBR  TO LTP-BRNO
              MOVE LN-ACCTNO TO LTP-ACCTNO
              MOVE 1         TO LTP-SEQNO
              PERFORM OPEN-LTP1-FILE
              PERFORM READ-LTP1-FILE
              PERFORM CLOSE-LTP1-FILE.

      *-----------------------------------------------------------------
       LOAD-LTP1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( LTP-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( LTP-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO LTP-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO LTP-ALL-MACHINE
              MOVE EXT-FILPATH-DATA        TO LTP-ALL-DATA
              MOVE LTP-ALL-PATH             TO LTP-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( LTP-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO LTP-ALL-BASE
              MOVE FILPATH-MACHINE         TO LTP-ALL-MACHINE
              MOVE FILPATH-DATA            TO LTP-ALL-DATA
              MOVE LTP-ALL-PATH             TO LTP-PATH.

      *-----------------------------------------------------------------
       OPEN-LTP1-FILE.
           PERFORM LOAD-LTP1-FILE.
           PERFORM OPEN-IT.
           MOVE LTP-PATH-SQL TO E-FILE.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       START-LTP1-FILE.
           PERFORM START-IT.
           MOVE LTP-PATH-OWNBR  TO LTP-BRNO.
           MOVE LTP-PATH-SQL    TO E-FILE.
           MOVE LTP1-KEY        TO E-KEYX.

           IF ( LTP1-CURSOR-STAT-GOOD )
              PERFORM CLOSE-LTP1-FILE.

           IF ( QLTP1-WEND-BRANCH = ZEROES    ) OR
              ( QLTP1-WEND-BRANCH NOT NUMERIC )
              MOVE LTP-PATH-OWNBR        TO QLTP1-WBEG-BRANCH
              MOVE LTP-PATH-OWNBR        TO QLTP1-WEND-BRANCH
              MOVE LTP-ACCTNO            TO QLTP1-WBEG-ACCTNO
              MOVE ALL "9"               TO QLTP1-WEND-ACCTNO
              MOVE LTP-SEQNO             TO QLTP1-WBEG-SEQNO
              MOVE ALL "9"               TO QLTP1-WEND-SEQNO.

           EXEC SQL
            DECLARE LTP1_CURSOR CURSOR FOR
            SELECT LTPFILE.LTP_SCHLD_TERM_1,
                   LTPFILE.LTP_SCHLD_TERM_2,
                   LTPFILE.LTP_SCHLD_TERM_3,
                   LTPFILE.LTP_SCHLD_TERM_4,
                   LTPFILE.LTP_SCHLD_TERM_5,
                   LTPFILE.LTP_SCHLD_TERM_6,
                   LTPFILE.LTP_SCHLD_TERM_7,
                   LTPFILE.LTP_SCHLD_TERM_8,
                   LTPFILE.LTP_SCHLD_TERM_9,
                   LTPFILE.LTP_SCHLD_TERM_10,
                   LTPFILE.LTP_SCHLD_TERM_11,
                   LTPFILE.LTP_SCHLD_TERM_12,
                   LTPFILE.LTP_SCHLD_TERM_13,
                   LTPFILE.LTP_SCHLD_TERM_14,
                   LTPFILE.LTP_SCHLD_TERM_15,
                   LTPFILE.LTP_SCHLD_TERM_16,
                   LTPFILE.LTP_SCHLD_TERM_17,
                   LTPFILE.LTP_SCHLD_TERM_18,
                   LTPFILE.LTP_SCHLD_TERM_19,
                   LTPFILE.LTP_SCHLD_TERM_20,
                   LTPFILE.LTP_SCHLD_TERM_21,
                   LTPFILE.LTP_SCHLD_TERM_22,
                   LTPFILE.LTP_SCHLD_TERM_23,
                   LTPFILE.LTP_SCHLD_TERM_24,
                   LTPFILE.LTP_SCHLD_TERM_25,
                   LTPFILE.LTP_SCHLD_TERM_26,
                   LTPFILE.LTP_SCHLD_TERM_27,
                   LTPFILE.LTP_SCHLD_TERM_28,
                   LTPFILE.LTP_SCHLD_TERM_29,
                   LTPFILE.LTP_SCHLD_TERM_30,
                   LTPFILE.LTP_SCHLD_TERM_31,
                   LTPFILE.LTP_SCHLD_TERM_32,
                   LTPFILE.LTP_SCHLD_TERM_33,
                   LTPFILE.LTP_SCHLD_TERM_34,
                   LTPFILE.LTP_SCHLD_TERM_35,
                   LTPFILE.LTP_SCHLD_TERM_36,
                   LTPFILE.LTP_SCHLD_TERM_37,
                   LTPFILE.LTP_SCHLD_TERM_38,
                   LTPFILE.LTP_SCHLD_TERM_39,
                   LTPFILE.LTP_SCHLD_TERM_40,
                   LTPFILE.LTP_SCHLD_TERM_41,
                   LTPFILE.LTP_SCHLD_PAYMNT_1,
                   LTPFILE.LTP_SCHLD_PAYMNT_2,
                   LTPFILE.LTP_SCHLD_PAYMNT_3,
                   LTPFILE.LTP_SCHLD_PAYMNT_4,
                   LTPFILE.LTP_SCHLD_PAYMNT_5,
                   LTPFILE.LTP_SCHLD_PAYMNT_6,
                   LTPFILE.LTP_SCHLD_PAYMNT_7,
                   LTPFILE.LTP_SCHLD_PAYMNT_8,
                   LTPFILE.LTP_SCHLD_PAYMNT_9,
                   LTPFILE.LTP_SCHLD_PAYMNT_10,
                   LTPFILE.LTP_SCHLD_PAYMNT_11,
                   LTPFILE.LTP_SCHLD_PAYMNT_12,
                   LTPFILE.LTP_SCHLD_PAYMNT_13,
                   LTPFILE.LTP_SCHLD_PAYMNT_14,
                   LTPFILE.LTP_SCHLD_PAYMNT_15,
                   LTPFILE.LTP_SCHLD_PAYMNT_16,
                   LTPFILE.LTP_SCHLD_PAYMNT_17,
                   LTPFILE.LTP_SCHLD_PAYMNT_18,
                   LTPFILE.LTP_SCHLD_PAYMNT_19,
                   LTPFILE.LTP_SCHLD_PAYMNT_20,
                   LTPFILE.LTP_SCHLD_PAYMNT_21,
                   LTPFILE.LTP_SCHLD_PAYMNT_22,
                   LTPFILE.LTP_SCHLD_PAYMNT_23,
                   LTPFILE.LTP_SCHLD_PAYMNT_24,
                   LTPFILE.LTP_SCHLD_PAYMNT_25,
                   LTPFILE.LTP_SCHLD_PAYMNT_26,
                   LTPFILE.LTP_SCHLD_PAYMNT_27,
                   LTPFILE.LTP_SCHLD_PAYMNT_28,
                   LTPFILE.LTP_SCHLD_PAYMNT_29,
                   LTPFILE.LTP_SCHLD_PAYMNT_30,
                   LTPFILE.LTP_SCHLD_PAYMNT_31,
                   LTPFILE.LTP_SCHLD_PAYMNT_32,
                   LTPFILE.LTP_SCHLD_PAYMNT_33,
                   LTPFILE.LTP_SCHLD_PAYMNT_34,
                   LTPFILE.LTP_SCHLD_PAYMNT_35,
                   LTPFILE.LTP_SCHLD_PAYMNT_36,
                   LTPFILE.LTP_SCHLD_PAYMNT_37,
                   LTPFILE.LTP_SCHLD_PAYMNT_38,
                   LTPFILE.LTP_SCHLD_PAYMNT_39,
                   LTPFILE.LTP_SCHLD_PAYMNT_40,
                   LTPFILE.LTP_SCHLD_PAYMNT_41
            FROM DBO.LTPFILE
            WHERE LTPFILE.LTP_BRNO >= :QLTP1-WBEG-BRANCH
              AND LTPFILE.LTP_BRNO <= :QLTP1-WEND-BRANCH
              AND LTPFILE.LTP_ACCTNO >= :QLTP1-WBEG-ACCTNO
              AND LTPFILE.LTP_ACCTNO <= :QLTP1-WEND-ACCTNO
              AND LTPFILE.LTP_SEQNO >= :QLTP1-WEND-SEQNO
              AND LTPFILE.LTP_SEQNO <= :QLTP1-WEND-SEQNO
            ORDER BY LTPFILE.LTP_BRNO,
                     LTPFILE.LTP_ACCTNO,
                     LTPFILE.LTP_SEQNO
           END-EXEC.

           EXEC SQL
               OPEN LTP1_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE ZEROES TO QLTP1-WBEG-BRANCH 
                          QLTP1-WEND-BRANCH
                          QLTP1-WBEG-ACCTNO
                          QLTP1-WEND-ACCTNO
                          QLTP1-WBEG-SEQNO
                          QLTP1-WEND-SEQNO.

           MOVE STAT---GOOD TO LTP1-CURSOR-STAT.

      *-----------------------------------------------------------------
       READ-LTP1-FILE.
           PERFORM READ-IT.
           MOVE LTP-PATH-OWNBR  TO LTP-BRNO. 
           MOVE LTP-PATH-SQL    TO E-FILE.
           MOVE LTP1-KEY        TO E-KEYX.

           MOVE LTP-BRNO   TO QLTP-BRNO.
           MOVE LTP-ACCTNO TO QLTP-ACCTNO.
           MOVE LTP-SEQNO  TO QLTP-SEQNO.

           EXEC SQL
            SELECT LTPFILE.LTP_SCHLD_TERM_1,
                   LTPFILE.LTP_SCHLD_TERM_2,
                   LTPFILE.LTP_SCHLD_TERM_3,
                   LTPFILE.LTP_SCHLD_TERM_4,
                   LTPFILE.LTP_SCHLD_TERM_5,
                   LTPFILE.LTP_SCHLD_TERM_6,
                   LTPFILE.LTP_SCHLD_TERM_7,
                   LTPFILE.LTP_SCHLD_TERM_8,
                   LTPFILE.LTP_SCHLD_TERM_9,
                   LTPFILE.LTP_SCHLD_TERM_10,
                   LTPFILE.LTP_SCHLD_TERM_11,
                   LTPFILE.LTP_SCHLD_TERM_12,
                   LTPFILE.LTP_SCHLD_TERM_13,
                   LTPFILE.LTP_SCHLD_TERM_14,
                   LTPFILE.LTP_SCHLD_TERM_15,
                   LTPFILE.LTP_SCHLD_TERM_16,
                   LTPFILE.LTP_SCHLD_TERM_17,
                   LTPFILE.LTP_SCHLD_TERM_18,
                   LTPFILE.LTP_SCHLD_TERM_19,
                   LTPFILE.LTP_SCHLD_TERM_20,
                   LTPFILE.LTP_SCHLD_TERM_21,
                   LTPFILE.LTP_SCHLD_TERM_22,
                   LTPFILE.LTP_SCHLD_TERM_23,
                   LTPFILE.LTP_SCHLD_TERM_24,
                   LTPFILE.LTP_SCHLD_TERM_25,
                   LTPFILE.LTP_SCHLD_TERM_26,
                   LTPFILE.LTP_SCHLD_TERM_27,
                   LTPFILE.LTP_SCHLD_TERM_28,
                   LTPFILE.LTP_SCHLD_TERM_29,
                   LTPFILE.LTP_SCHLD_TERM_30,
                   LTPFILE.LTP_SCHLD_TERM_31,
                   LTPFILE.LTP_SCHLD_TERM_32,
                   LTPFILE.LTP_SCHLD_TERM_33,
                   LTPFILE.LTP_SCHLD_TERM_34,
                   LTPFILE.LTP_SCHLD_TERM_35,
                   LTPFILE.LTP_SCHLD_TERM_36,
                   LTPFILE.LTP_SCHLD_TERM_37,
                   LTPFILE.LTP_SCHLD_TERM_38,
                   LTPFILE.LTP_SCHLD_TERM_39,
                   LTPFILE.LTP_SCHLD_TERM_40,
                   LTPFILE.LTP_SCHLD_TERM_41,
                   LTPFILE.LTP_SCHLD_PAYMNT_1,
                   LTPFILE.LTP_SCHLD_PAYMNT_2,
                   LTPFILE.LTP_SCHLD_PAYMNT_3,
                   LTPFILE.LTP_SCHLD_PAYMNT_4,
                   LTPFILE.LTP_SCHLD_PAYMNT_5,
                   LTPFILE.LTP_SCHLD_PAYMNT_6,
                   LTPFILE.LTP_SCHLD_PAYMNT_7,
                   LTPFILE.LTP_SCHLD_PAYMNT_8,
                   LTPFILE.LTP_SCHLD_PAYMNT_9,
                   LTPFILE.LTP_SCHLD_PAYMNT_10,
                   LTPFILE.LTP_SCHLD_PAYMNT_11,
                   LTPFILE.LTP_SCHLD_PAYMNT_12,
                   LTPFILE.LTP_SCHLD_PAYMNT_13,
                   LTPFILE.LTP_SCHLD_PAYMNT_14,
                   LTPFILE.LTP_SCHLD_PAYMNT_15,
                   LTPFILE.LTP_SCHLD_PAYMNT_16,
                   LTPFILE.LTP_SCHLD_PAYMNT_17,
                   LTPFILE.LTP_SCHLD_PAYMNT_18,
                   LTPFILE.LTP_SCHLD_PAYMNT_19,
                   LTPFILE.LTP_SCHLD_PAYMNT_20,
                   LTPFILE.LTP_SCHLD_PAYMNT_21,
                   LTPFILE.LTP_SCHLD_PAYMNT_22,
                   LTPFILE.LTP_SCHLD_PAYMNT_23,
                   LTPFILE.LTP_SCHLD_PAYMNT_24,
                   LTPFILE.LTP_SCHLD_PAYMNT_25,
                   LTPFILE.LTP_SCHLD_PAYMNT_26,
                   LTPFILE.LTP_SCHLD_PAYMNT_27,
                   LTPFILE.LTP_SCHLD_PAYMNT_28,
                   LTPFILE.LTP_SCHLD_PAYMNT_29,
                   LTPFILE.LTP_SCHLD_PAYMNT_30,
                   LTPFILE.LTP_SCHLD_PAYMNT_31,
                   LTPFILE.LTP_SCHLD_PAYMNT_32,
                   LTPFILE.LTP_SCHLD_PAYMNT_33,
                   LTPFILE.LTP_SCHLD_PAYMNT_34,
                   LTPFILE.LTP_SCHLD_PAYMNT_35,
                   LTPFILE.LTP_SCHLD_PAYMNT_36,
                   LTPFILE.LTP_SCHLD_PAYMNT_37,
                   LTPFILE.LTP_SCHLD_PAYMNT_38,
                   LTPFILE.LTP_SCHLD_PAYMNT_39,
                   LTPFILE.LTP_SCHLD_PAYMNT_40,
                   LTPFILE.LTP_SCHLD_PAYMNT_41
            INTO
                   :QLTP-SCHLD-TERM-1,
                   :QLTP-SCHLD-TERM-2,
                   :QLTP-SCHLD-TERM-3,
                   :QLTP-SCHLD-TERM-4,
                   :QLTP-SCHLD-TERM-5,
                   :QLTP-SCHLD-TERM-6,
                   :QLTP-SCHLD-TERM-7,
                   :QLTP-SCHLD-TERM-8,
                   :QLTP-SCHLD-TERM-9,
                   :QLTP-SCHLD-TERM-10,
                   :QLTP-SCHLD-TERM-11,
                   :QLTP-SCHLD-TERM-12,
                   :QLTP-SCHLD-TERM-13,
                   :QLTP-SCHLD-TERM-14,
                   :QLTP-SCHLD-TERM-15,
                   :QLTP-SCHLD-TERM-16,
                   :QLTP-SCHLD-TERM-17,
                   :QLTP-SCHLD-TERM-18,
                   :QLTP-SCHLD-TERM-19,
                   :QLTP-SCHLD-TERM-20,
                   :QLTP-SCHLD-TERM-21,
                   :QLTP-SCHLD-TERM-22,
                   :QLTP-SCHLD-TERM-23,
                   :QLTP-SCHLD-TERM-24,
                   :QLTP-SCHLD-TERM-25,
                   :QLTP-SCHLD-TERM-26,
                   :QLTP-SCHLD-TERM-27,
                   :QLTP-SCHLD-TERM-28,
                   :QLTP-SCHLD-TERM-29,
                   :QLTP-SCHLD-TERM-30,
                   :QLTP-SCHLD-TERM-31,
                   :QLTP-SCHLD-TERM-32,
                   :QLTP-SCHLD-TERM-33,
                   :QLTP-SCHLD-TERM-34,
                   :QLTP-SCHLD-TERM-35,
                   :QLTP-SCHLD-TERM-36,
                   :QLTP-SCHLD-TERM-37,
                   :QLTP-SCHLD-TERM-38,
                   :QLTP-SCHLD-TERM-39,
                   :QLTP-SCHLD-TERM-40,
                   :QLTP-SCHLD-TERM-41,
                   :QLTP-SCHLD-PAYMNT-1,
                   :QLTP-SCHLD-PAYMNT-2,
                   :QLTP-SCHLD-PAYMNT-3,
                   :QLTP-SCHLD-PAYMNT-4,
                   :QLTP-SCHLD-PAYMNT-5,
                   :QLTP-SCHLD-PAYMNT-6,
                   :QLTP-SCHLD-PAYMNT-7,
                   :QLTP-SCHLD-PAYMNT-8,
                   :QLTP-SCHLD-PAYMNT-9,
                   :QLTP-SCHLD-PAYMNT-10,
                   :QLTP-SCHLD-PAYMNT-11,
                   :QLTP-SCHLD-PAYMNT-12,
                   :QLTP-SCHLD-PAYMNT-13,
                   :QLTP-SCHLD-PAYMNT-14,
                   :QLTP-SCHLD-PAYMNT-15,
                   :QLTP-SCHLD-PAYMNT-16,
                   :QLTP-SCHLD-PAYMNT-17,
                   :QLTP-SCHLD-PAYMNT-18,
                   :QLTP-SCHLD-PAYMNT-19,
                   :QLTP-SCHLD-PAYMNT-20,
                   :QLTP-SCHLD-PAYMNT-21,
                   :QLTP-SCHLD-PAYMNT-22,
                   :QLTP-SCHLD-PAYMNT-23,
                   :QLTP-SCHLD-PAYMNT-24,
                   :QLTP-SCHLD-PAYMNT-25,
                   :QLTP-SCHLD-PAYMNT-26,
                   :QLTP-SCHLD-PAYMNT-27,
                   :QLTP-SCHLD-PAYMNT-28,
                   :QLTP-SCHLD-PAYMNT-29,
                   :QLTP-SCHLD-PAYMNT-30,
                   :QLTP-SCHLD-PAYMNT-31,
                   :QLTP-SCHLD-PAYMNT-32,
                   :QLTP-SCHLD-PAYMNT-33,
                   :QLTP-SCHLD-PAYMNT-34,
                   :QLTP-SCHLD-PAYMNT-35,
                   :QLTP-SCHLD-PAYMNT-36,
                   :QLTP-SCHLD-PAYMNT-37,
                   :QLTP-SCHLD-PAYMNT-38,
                   :QLTP-SCHLD-PAYMNT-39,
                   :QLTP-SCHLD-PAYMNT-40,
                   :QLTP-SCHLD-PAYMNT-41
            FROM DBO.LTPFILE
            WHERE LTPFILE.LTP_BRNO = :QLTP-BRNO
              AND LTPFILE.LTP_ACCTNO = :QLTP-ACCTNO
              AND LTPFILE.LTP_SEQNO = :QLTP-SEQNO
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-LTP1-FILE.

           IF ( IO-FG = 0 )
              PERFORM GET-LTP-FIELDS.

      *-----------------------------------------------------------------
       READ-LTP1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE LTP-PATH-OWNBR  TO LTP-BRNO.
           MOVE LTP-PATH-SQL    TO E-FILE.
           MOVE LTP1-KEY        TO E-KEYX.
           INITIALIZE QLTP-REC.

           IF ( LTP1-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH LTP1_CURSOR INTO
                   :QLTP-SCHLD-TERM-1,
                   :QLTP-SCHLD-TERM-2,
                   :QLTP-SCHLD-TERM-3,
                   :QLTP-SCHLD-TERM-4,
                   :QLTP-SCHLD-TERM-5,
                   :QLTP-SCHLD-TERM-6,
                   :QLTP-SCHLD-TERM-7,
                   :QLTP-SCHLD-TERM-8,
                   :QLTP-SCHLD-TERM-9,
                   :QLTP-SCHLD-TERM-10,
                   :QLTP-SCHLD-TERM-11,
                   :QLTP-SCHLD-TERM-12,
                   :QLTP-SCHLD-TERM-13,
                   :QLTP-SCHLD-TERM-14,
                   :QLTP-SCHLD-TERM-15,
                   :QLTP-SCHLD-TERM-16,
                   :QLTP-SCHLD-TERM-17,
                   :QLTP-SCHLD-TERM-18,
                   :QLTP-SCHLD-TERM-19,
                   :QLTP-SCHLD-TERM-20,
                   :QLTP-SCHLD-TERM-21,
                   :QLTP-SCHLD-TERM-22,
                   :QLTP-SCHLD-TERM-23,
                   :QLTP-SCHLD-TERM-24,
                   :QLTP-SCHLD-TERM-25,
                   :QLTP-SCHLD-TERM-26,
                   :QLTP-SCHLD-TERM-27,
                   :QLTP-SCHLD-TERM-28,
                   :QLTP-SCHLD-TERM-29,
                   :QLTP-SCHLD-TERM-30,
                   :QLTP-SCHLD-TERM-31,
                   :QLTP-SCHLD-TERM-32,
                   :QLTP-SCHLD-TERM-33,
                   :QLTP-SCHLD-TERM-34,
                   :QLTP-SCHLD-TERM-35,
                   :QLTP-SCHLD-TERM-36,
                   :QLTP-SCHLD-TERM-37,
                   :QLTP-SCHLD-TERM-38,
                   :QLTP-SCHLD-TERM-39,
                   :QLTP-SCHLD-TERM-40,
                   :QLTP-SCHLD-TERM-41,
                   :QLTP-SCHLD-PAYMNT-1,
                   :QLTP-SCHLD-PAYMNT-2,
                   :QLTP-SCHLD-PAYMNT-3,
                   :QLTP-SCHLD-PAYMNT-4,
                   :QLTP-SCHLD-PAYMNT-5,
                   :QLTP-SCHLD-PAYMNT-6,
                   :QLTP-SCHLD-PAYMNT-7,
                   :QLTP-SCHLD-PAYMNT-8,
                   :QLTP-SCHLD-PAYMNT-9,
                   :QLTP-SCHLD-PAYMNT-10,
                   :QLTP-SCHLD-PAYMNT-11,
                   :QLTP-SCHLD-PAYMNT-12,
                   :QLTP-SCHLD-PAYMNT-13,
                   :QLTP-SCHLD-PAYMNT-14,
                   :QLTP-SCHLD-PAYMNT-15,
                   :QLTP-SCHLD-PAYMNT-16,
                   :QLTP-SCHLD-PAYMNT-17,
                   :QLTP-SCHLD-PAYMNT-18,
                   :QLTP-SCHLD-PAYMNT-19,
                   :QLTP-SCHLD-PAYMNT-20,
                   :QLTP-SCHLD-PAYMNT-21,
                   :QLTP-SCHLD-PAYMNT-22,
                   :QLTP-SCHLD-PAYMNT-23,
                   :QLTP-SCHLD-PAYMNT-24,
                   :QLTP-SCHLD-PAYMNT-25,
                   :QLTP-SCHLD-PAYMNT-26,
                   :QLTP-SCHLD-PAYMNT-27,
                   :QLTP-SCHLD-PAYMNT-28,
                   :QLTP-SCHLD-PAYMNT-29,
                   :QLTP-SCHLD-PAYMNT-30,
                   :QLTP-SCHLD-PAYMNT-31,
                   :QLTP-SCHLD-PAYMNT-32,
                   :QLTP-SCHLD-PAYMNT-33,
                   :QLTP-SCHLD-PAYMNT-34,
                   :QLTP-SCHLD-PAYMNT-35,
                   :QLTP-SCHLD-PAYMNT-36,
                   :QLTP-SCHLD-PAYMNT-37,
                   :QLTP-SCHLD-PAYMNT-38,
                   :QLTP-SCHLD-PAYMNT-39,
                   :QLTP-SCHLD-PAYMNT-40,
                   :QLTP-SCHLD-PAYMNT-41
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-LTP1-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-LTP-FIELDS.

      *-----------------------------------------------------------------
       CLOSE-LTP1-FILE.

           IF ( LTP1-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE LTP1_CURSOR
              END-EXEC
              MOVE STAT---BAD TO LTP1-CURSOR-STAT.

       COPY "LIBGB/GBWKIN.CPY".
       OPEN-DUE-FILE.
           PERFORM OPEN-IT.
           MOVE DUE-PATH TO E-FILE.
           OPEN I-O DUE-FILE.
           IF IO-FG = 8
             GO TO OPEN-DUE-FILE.
           IF IO-FG = 7
             CLOSE DUE-FILE
             GO TO OPEN-DUE-FILE.
           UNLOCK DUE-FILE.
       OPEN-DUE-FILE-OUTPUT.
           PERFORM OPEN-IT.
           MOVE DUE-PATH TO E-FILE.
           OPEN OUTPUT DUE-FILE.
           IF IO-FG = 8
             GO TO OPEN-DUE-FILE.
           IF IO-FG = 7
             CLOSE DUE-FILE
             GO TO OPEN-DUE-FILE.
           UNLOCK DUE-FILE.
       WRITE-DUE-FILE.
           PERFORM WRITE-IT.
           MOVE DUE-PATH TO E-FILE.
           MOVE DUE-KEY TO E-KEYX.
           WRITE DUE-REC.
           IF IO-FG = 8
             GO TO WRITE-DUE-FILE.
           UNLOCK DUE-FILE.
       CLOSE-DUE-FILE.
           PERFORM CLOSE-IT.
           CLOSE DUE-FILE.
       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO EXT-EOBUF-ERRCD.

      ******************************************************************
       END-PROGRAM SECTION.
      ******************************************************************
       END-RUN.
           PERFORM CLOSE-FILES.

           IF EXT-ROUTE-BUF = "00"
              PERFORM PRU-OUT.

           IF EXT-ROUTE-BUF = "00"
              IF DUEDATE-SUMMARY = "F"
                 MOVE "DUE DATE ASCII FILE: XXXXXXXXXXXXXXXXXXXXXXXX"
                    TO MESS
                 INSPECT MESS REPLACING FIRST "XXXXXXXXXXXXXXXXXXXXXXXX"
                    BY DUE-PATH
                 PERFORM SEND-MESS.

           IF NO-RECORDS-FOUND
              MOVE "NO RECORDS FOUND FOR SELECTED RANGE" TO MESS
              PERFORM SEND-MESS.

           MOVE MKTEMP-BUF TO ACCESS-BUF.
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

       STOP-RUN.
           IF EXT-ROUTE-BUF = "00" OR "40" OR "50"
              DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN |MJD001
              DESTROY LNAGKC-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.
           EXIT PROGRAM.
