       IDENTIFICATION DIVISION.
       PROGRAM-ID.      CPCORP.
       DATE-WRITTEN.    071195.
      ******************************************************************
      *                      (LP)
      *    DESCRIPTION:  COLLECTION REPORT
      *
      *     NOTE:
      *      BR-CPCORP-ORDER    "1" OR "Y" = ACCOUNT NUMBER
      *                         "2" OR "N" = AGEING
      *                         "3"        = ALPHA (LNAME,FNAME)
      *                         "4"        = RENEWABLE 1ST, THEN HIGHEST
      *                                      CASH AVAILABLE TO LOWEST
      *                                      CURRENTLY ONLY AVAILABLE
      *                                      IF I-AM-WORLD
      *                          NOTE- 4 USED TO BE ALPHA FOR ALL BRANCHES
      *                                BUT THIS WENT AWAY IN A60
      *                         "5"        = RECENCY AGEING (A50)
      *                         "6"        = AGEING, ALPHA(LNAME,FNAME)
      *                         "7"        = DLNO, ALPHA(LNAME,FNAME)
      *
      *=================================================================
      * REV:
      * SLC 042995 ADDED PLTYPE LOGIC
      * BAH 061995 H-TITLE PRINT EO-COMMENT WHICH IS PASSED THRU
      *            EXT-EOBUF-MESS FROM EOEXEC
      * MJD 072195 ADDED GROUP FILE/ALL BRANCH LOGIC & BRANCH RANGE.
      *            ADDED 3RD LEVEL TO TOTALS TABLE FOR BRANCH TOTALS.
      *            MADE PRINT-OPTION 4 INVALID. IF BY CHANCH
      *            BR-CPCORP-ORDER IS SET TO "4" IT WILL BE DEFAULTED
      *            TO "1" TO PREVENT A CRASH.
      * MJD 072595 ADDED WORKERS FOR CALLING CL/SUMMBY:
      *              LIBCL/SUMMBYW AND LIBLP/LP01CRWKBY.
      * MJD 072895 ADDED LOGIC TO CHECK TO (I,S,O) POTENTIALS AND MISS
      *             PAYS.
      * MJD 072895 COPIED LP/CPCORP FROM LP/DELRPT
      * KEC 083195 ADDED LIBLP/LPNOCL
      * KEC 091395 ADDED LOCAL-SPOOLING LOGIC
      * JFF 100495 ADDED CLASS SCAN AND DEALER SCAN
      * KEC 110195 MADE PRCOL-FG A PART OF THE SCREEN-FIELDS.
      * JFF 111495 CHANGED FORM TO INCLUDE LOCAL-SPOOLING QUESTION.
      * MJD 112095 ADDED OPTION TO SKIP COLLECTOR TOTALS.
      * MJD 112395 IF COLLECTOR-FG = "N" THEN COLLECTOR CODE IN WK-KEY
      *             IS SET TO SPACES.
      * MJD 112895 ADDED TOTAL-FG. WHEN PRINTING GRAND-TOTALS YOU
      *             DO NOT WANT TO CALL SUMMBY.C, YOU WANT TO PRINT
      *             ACCUMULATED TOTALS.
      * KEC 113095 FIXED LOCAL-SPOOL TO NOT PRINT THE RANGE-LINES.
      * JFF 121395 CHANGED "IF LOCAL-PRINT-FG = N" TO
      *            "IF LOCAL-PRINT-FG NOT = Y" IN INITIALIZATION.
      * BAH 012496 ADDED TEST OF GP-NO-LC-IN-ARREARAGE IN LIBLP/CPCORP
      *            FOR TODAY-DUE NEXT-DUE AND LAST-DUE, E096
      * MJD 071696 ADDED LIBLP/LPUPWK FOR CHANGES IN LIBLP/AGEING
      * JFF 082996 FIXED "SUMMARY ONLY' TO PRINT LAST COLLECTOR BEFORE
      *            BRANCH TOTALS.
      * KEC 111496 CHANGED LN-POCD FOR RESCISSION.
      * BAH 012197 COLLECTOR TOTALS WERE PRINTING BEFORE BRANCH TOTALS
      *            EVEN IF COLLECTOR-FG WAS "N"
      * GLF 050797 ADDED COPY MEMBERS: SETCTBLW AND SETCTBL
      * MJD 051597 ADDED LPPOF2 COPY MEMBER (PREVIOUSLY WAS PART OF LPPOFF)
      * JTG 052097 ADDED 'LIBLP/ADDONSPR' TO GET CORRECT SPR RE: INS RB
      * BLV 060597 CHANGED FOR LOCAL SPOOL WITH NEW BATCH DRIVER - BREXEC
      * BAH 072997 ADDED D-SPROMDATE FOR ONE LINE DETAIL, SECURITY #531
      * MJD 082297 CHANGED LN-REC/CONPROFILE TO LN-REC/CONPROFILE(1)
      * GLF 090497 REPLACED 'IO' ON GP-FILE WITH GET 'GPENV'
      * BLV 091597 REMOVED DELSUB LOGIC & CHANGED FOR NEW AGEING ROUTINES
      * BLV 110597 CHECK BR-MIDSTREAM-1STPAY-FG TO DETERMINE IF
      *            MIDSTREAM ACCT IS A 1ST PMT MISS
      * JTG 111797 ADDED LOGIC FOR LIBLP/DEFPOL RE: BR-NODEF-1ST-X-MONTHS #167
      * JTG 111897 ADDED LOGIC FOR MIN PAST DUE AMOUNT FROM DELRPT.C, WHICH
      *            IS BEING REMOVED FROM REL: A10, SINCE THEY WILL NOW BE EQUAL
      * MJD 120497 ENLARGED BW-LOANS TO PIC 99.
      * MJD 122397 IF WE SKIP POTS WE STILL WANT TO ALLOW SELECTION OF MISS
      *            PAYS.
      * GLF 022098 DELETED BR-SECURITY-OK AND ADDED LIBGB/BRSECUREW.
      * BLV 031498 ADD NEXT MO AGEING OPTION FOR WORLD; IF DEL-FLAG
      *            ENTERED AS 'Y', BUMP AGE-DATE TO NEXT MONTH
      * MJD 032798 CHANGED TO DISPLAY PROGRESS MESS BY BRANCH RATHER THAN
      *            BY ACCOUNT #
      * JTG 032798 ADDED CONTRACTUAL MISS PAYS & POT'S OPTION TO RANGE PRINT
      *            ADDED RECENCY POT'S OPTION TO RANGE PRINT
      * JTG 040598 MOVED BEFORE PERFORM INITIALIZE, GET GLOBAL PARAMETER
      *            RECORD (FROM GPENV), SINCE IT WAS NEEDED IN ENTER PARAM:
      *            CHANGED VERSION CONTROL TO 'D' ALSO
      * KEC 040698 ADDED CHECK IN 'COLLECTOR-TOTALS' SECTION TO
      *            SUPPRESS PRINTING OF COLLECTOR TOTALS IF THEY ARE
      *            EQUAL TO BRANCH TOTALS.
      * JTG 040798 ADDED TEST TO SKIP PRINT OF GRAND TOTALS WHEN REPORT
      *            IS FOR ONE BRANCH
      * BLV 041398 FIXED TESTS ON SKIP, INCLUDE, ONLY FOR POTS &
      *            MISPAYS - THEY DID NOT WORK
      * MJD 041498 ADDED SCREEN OPTION TO NOT BREAK ON COLLECTOR. ADDED OPTION
      *            TO SUPPRESS/PRINT  TOTALS AND RANGE LINES.
      * MJD 042198 DEFAULT DEL-FLAG (BEG MO DELINQ) TO 'N'
      * MJD 042898 FIXED BUG. IF SKIP ACTION CODE FLAG WAS N, THEN
      *            WE WERE NOT CHECKING TEST OF LN ACTION CODE IN RANGES.
      * BLV 072098 IF RUNNING IN BATCH, CHECK FOR DEL-FLAG NOT = Y
      *            INSTEAD OF DEL-FLAG = N (EO-EXEC)
      * BLV 072398 FIXED COMPUTE PAST DUE ROUTINE; USED HOLD-DATE WHICH WASN'T
      *            INITIALIZED; USE AGE-DATE INSTEAD (MIN PAST DUE WRONG)
      * BLV 980922 ADD ALPHA RANGE ON BW-LAST NAME, FFG PR# 752;  NOTE,
      *            I JURY-RIGGED EO-EXEC FOR BAD RANGE SO ALL BATCHES DON'T
      *            HAVE TO BE RE-ENTERED!
      * BLV 980924 CHANGE ALPHA RANGES TO 4 CHARACTERS, PR #752
      * GLF 981109 MADE MEMO DATE 2000 COMPATIBLE
      *
      * KEC 990408 FIXED THE PROBLEM WITH PRINTING THE VERY FIRST
      *            COLLECTOR TOTALS PAGE.
      * KEC 990517 CHANGED THE TOTALS LINE PROMPT TO "ALL CLASSES:" FROM
      *            "GROUP TOTAL:" BECAUSE IT WAS MISLEADING, REGWAN PL# 218
      * BAH 990831 DOUBLE SPACE THE SINGLE LINE OPTION FOR TFC, PR#53
      * GLF 990831 WHEN ENTERING A GROUP IN BATCH, IF YOU EXIT AND THEN RETURN,
      *            WAS MOVE ZEROES TO DISPLAY, I FIXED TO KEEP GROUP DISPLAYING
      *            PER BARB
      * GLF 990924 REPLACED IO-FG NUMBERS WITH IO-FG 88 LEVEL PER DEVELOPMENT
      *            MEETING (082599)
      * BLV 991119 ADDED PRINT SEQ 4, SORT BY RENEWABLE ACCTS FIRST, THEN
      *            IN ORDER OF MOST CASH AVAILABLE, OPTION AVAILABLE ONLY IF
      *            I-AM-WORLD; ALSO IF I-AM-WORLD PRINT CASH AVAILABLE INSTEAD
      *            OF DEALER #; CHANGED CONTRACTUAL TO PRINT EITHER ALPHA
      *            SUCH AS <M> OR <P> OR # DAYS IN SAME COLUMN INSTEAD OF
      *            SEPARATE COLUMN; ADD PAID THRU TO DETAIL LINE; THESE CHANGES
      *            WERE DONE TO SINGLE-LINE-DETAIL-PROCESS, WORLD PR# 214
      * BLV 000202 ADDED SPECIAL RENEWABLE FORMULA FOR WORLD CLASS 5:
      *            IF SP-RENEWABLE-PAYS = -1, USE LN-ORGTERM / 3
      *            IF SP-RENEWABLE-DAYS = -1, USE LN-ORGTERM / 3 * 30 + 1
      * BLV 000313 IF MERCURY, INCLUDE O2 IN BALANCE FOR TOTALS, MERC PR# 226
      * MJD 000411 ADDED NON-MONTHLY TOTALS FOR TFC (TFC PR#69)
      * GLB 004014 ADDED PRINT "OTHER" COLUMN OPTION (TFC PR#71)
      *             ALSO SEPARATED OTHER TOTALS, REPLACED CONFID/ALLOT
      * MJD 000629 ADDED TEST OF GP-CPCORP-CURBAL (NEWS PR#1230)
      *            FIXED TO SKIP TOTALS IF ZERO.
      * MJD 070500 FIXED BUF MISSED  D-SBALANCE WHEN ADING GP-CPCORP-CURBAL.
      * MJD 000913 CHANGED TO DISPLAY LOCK BOX FLAG IN PLACE OF ALLOTMENT
      *            FLAG IF ACCOUNT LN-LBOX = "Y" (MFC PR#227)
      * BAH 010208 WHEN GP-CPCORP-CURBAL = "Y", SUBTRACT OUT LN-OT2BAL
      *            TOTALS ALREADY DO SUBTRACT IT, BUT DETAIL LINE DOES NOT.
      *             ACE FINANCIAL, PR# 1421
      * BAH 011017 ADD BR-CPCORP-SEQ 7 = DLNO, LNAME, FNAME SMARTCHOICE(FFG)
      *            PR# 1613
      * BAH 030522 ALLOW ALPHA NUMERIC IN BEG-CLASS TO ENTER TYPE OF CLASS
      *            TO BE REPORTED S = SALES, L = DIRECT, M = SMALL,
      *            R = REAL EST, W = WCBC, LENDMARK PR# 2074
      * BAH 030527 ADDED MIN AVAIL ACREDIT: FOR WORLD PR# 324, DID NOT HAVE
      *            TO CHANGE VERSION, CHECKS FOR SPACES FIRST.
      * JTG 030630 ACCOUNTS BYPASS LATE CHARGE ASSESS ROUTINE WHEN
      *            SP-LCFRMLA = ' ' BUT ACCOUNT IS SETUP FOR DEALER LATE
      *            CHARGE FORMULA                               MIDATL #J029
      * BAH 040225 REMOVED D-BRNO FROM DETAIL LINE, ONLY HAD ROOM FOR 2 BYTES
      *            THE BRNO IS IN THE PAGE HEADING ANYWAYS
      * BLF 041004 CHANGED TO SEND CLASS RANGE TO SUMMBY WHEN IT'S CALLED, SO
      *            IT WILL RETURN TOTALS FOR COMPUTING DELINQUENCY PERCENTS
      *            BASED ON CLASS RANGE, NOT TOTAL PORTFOLIO, ACE PR# 2374
      * BLF 041004 FIX RANGE PRINT SO IT FITS ON 1 PAGE
      * BAH 050118 ADDED CHECK OF BW-CONFIDENTIAL, PR# 2343
      * BLF 051025 IF GP-INCLUDE-O2-IN-BAL = "Y", INCLUDE OTHER 2 IN BALANCE
      *            OTHERWISE SUBTRACT IT, PR# 2623
      * BAH 060522 REPLACE SSNO WITH X'S FOR SUN FINANCE, PR# 2790
      *  CS 080909 REGENCY COMPLAINED THAT PDTH DID NOT MATCH COLLECTION INQ
      *            THEY HAVE GP-IGNORE-LCHG-NXDUE SET TO "Y", WHICH SUBTRACTS
      *            LATECHGS & COMPUTES PD-THRU(LPCRN. ADD A GP-I-AM-REGENCY
      *            THAT USES PDTH-DATE INSTEAD OF NO-PDTH-DATE. PR#637
      * BAH 090114 TEST GP-HIDE-FIRST-5-SSNO, WORLD PR# 621
      * BAH 100630 ADDED BW-CELL-PHONE, WORLD PR# 699
      * BAH 120522 DEFAULT DEBTAID TO SKIP FOR REGACC, PR# 311
      *  CS 120821 TOTALS WERE TRUNCATED WHEN YOU EXCEED 99,999,999FOR BALANCE
      *            WE DONT PRINT PERCENTS FOR TOTALS, USED THIS PR#4099 UNITED
      * BAH 121207 REDEFINED DEALER AS POOLID FOR WORLD "MX" AND ADDED
      *            ANOTHER PRINT LINE WITH UNION INFO, WORLD PR# 831
      * BAH 130121 REMOVED 1 FILLER BETWEEN DAY AND NAME SO CASH AVAILABLE
      *            FOR WORLD COULD PRINT 1 MORE DIGIT, IT PRINT DEALER NO FOR
      *            ALL OTHER CUSTOMERS, WORLD PR# 847
      *  CS 130422 SANE AS PR#637 8/09/09 USE PDTH-DATE INSTEAD OF NO-PDTH-DATE
      *            IF GP-I-AM-SECURITY  PL798
      * BAH 150416 ADD FROZEN ACCOUNTS (I,S,O) NEWPORT PR# 4716
      * BAH 150417 PASSWORD ABILITY TO UNMASK THE FIRST 5 OF SSNO FOR "A"
      *            LOGINS, IF GP-HIDE-FIRST-5-SSNO "Y", FINMGM PR#4734
      * BAH 150424 ADD CONFIDENTIAL ACCOUNTS (I,S,O) MODERN PR# 4737
      * BAH 160115 CHANGED LT TO LTC, PD0003
      *
      * KEC 2016.0217 {PD#00003} PARADATA <A30> [JAY CISZEWSKI]
      *         FIXED KEYS & RECS FOR FD-REC.
      *         CHANGED FMNO/FORMNO/NEXTFM/SETFM FROM 3 TO 4 BYTES.
      * BAH 170525 ACTIVATE 8 DIGIT DATES, PD0006
      * KEC 2017.0605 {PR#00000} PARADATA <A30> [JAY CISZEWSKI]
      *          ADDED 'LN' FILE PREFIX FOR WORKING STORAGE FIELDS.
      *  CS 171117 REMOVED TEST ON SP1-KEY TO ONLY READ WHEN KEY CHANGED.
      *            IF YOU USE THE DEALER LATE CHARGE CODE, YOU
      *            MAY HAVE THE WRONG LATE INFO WHEN NEXT ACCOUNT HAS
      *            SAME SP1-KEY, ALWAYS READ THE SPFILE.
      * BAH 181128 GLOBAL LMFILE
      * BAH 181205 GLOBAL LNFILE
      * KEC 190219 A32 REMOVED REDEFINE FOR LN-LC-CODEX; UNUSED
      * BAH 20190919 REMOVED ACCESS-CALL ON LNFILE
      * BAH 20220803 HARD CODED START
      * BAH 20220818 REMOVED GLOBRD, READ GB-FILE INSTEAD
      * MB  20230814 PERFORMANCE TUNING - ADDED STORED PROCEDURE TO RETRIEVE 
      *               LN AND BW DATA FOR WORK FILE CREATION.
      *               ADDED STATE PARAMETER ARRAY PROCESSING
      *       *** SO LNFILE IS NOT READ MULTIPLE TIMES IN THE PROGRAM, 
      *           LN-REC HAS BEEN ADDED TO THE WK FILE, ALONG WITH HARDCODED 
      *           SIZE. IF THE SIZE OF LNFILE TABLE INCREASE, THE 78 LEVEL ON 
      *           WK-FILE WILL ALSO NEED TO BE INCREASED. ***
      *           NO-REC HAS BEEN ADDED TO THE WK FILE, ALONG WITH HARDCODED 
      *           SIZE. IF THE SIZE OF NOFILE TABLE INCREASE, THE 78 LEVEL ON 
      *           WK-FILE WILL ALSO NEED TO BE INCREASED. ***
      *       NOTE - ALL LNFILE COLUMNS RETURNED IN CURSOR
      * BAH 20240201 REMOVED CRNOR2, ADD READ NOTICE RECORD
      * BAH 20240202 REMOVED POOLID REDEFINE OF DEALER WORLD "MX" 
      *              REMOVED PRINT LINE WITH UNION INFO
      *              REMOVED HARD CODED BRANCH TEST > 2999
      * RZ  20240307 ADDED JOIN TO NOFILE TO RETRIEVE NOFILE FIELDS 
      * BAH 20240807 INCREASED WK-NAME FROM 33 TO 71 TO HOLD THE LARGER
      *              NAMES AND ALLOW THE DELIMITED BY SIZE TO WORK S35Q-109
      *              FIRST NAMES WITHIN THE LAST NAMES WERE NOT MATCHING
      * BAH 20250429 ALL THE CONTRACTUAL AND RECENCY FIELDS ARE WIPED OUT
      *              WHEN WE PROCESS THE SAVED WK-FILE, A15 USED TO READ THE
      *              LOAN AGAIN AND CALC AGAIN. MOVE IN THESE FIELDS FROM
      *              THE NOTICE RECORD. S35Q-202
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
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       FD  WK-FILE
           LABEL RECORDS ARE STANDARD.

       78  WK-LN-SIZE                  VALUE 2800.
       78  WK-NO-SIZE                  VALUE 430.

       01  WK-REC.
           03  WK-KEY.
               05  WK-BRNO           PIC 9(4).
               05  WK-COLCTID        PIC X(3).
               05  WK-ACTION-CD      PIC X(4).
               05  WK-AGE            PIC S9(5).
               05  WK-BALDUE         PIC S9(9)V99.
               05  WK-NAME           PIC X(71).
               05  WK-ACCTNO         PIC 9(8).
           03  WK-CSUB               PIC S9(6)  COMP.
           03  WK-CPOT30             PIC X.
           03  WK-RDSUB              PIC S9(6)  COMP.
           03  WK-RDPOT30            PIC X.
           03  WK-PSUB               PIC S9(6)  COMP.
           03  WK-CTFC-NM            PIC S9(6)  COMP.
           03  WK-LN-REC             PIC X(WK-LN-SIZE).
           03  WK-NO-REC             PIC X(WK-NO-SIZE).

      ******************************************************************
      *
      *     W O R K I N G - S T O R A G E   S E C T I O N
      *
      ******************************************************************
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(37) VALUE
           "@(#)LP/CPCORP S35 04/29/25-08:28:30 >".

       COPY "LIBLP/LP01LC.CPY".
       COPY "LIBLP/LP01LC_SQL.CPY".
       COPY "LIBLP/LPWSLC.CPY".
       COPY "LIBLP/LP01NO.CPY".
       COPY "LIBLP/LP01NO_SQL.CPY".
       COPY "LIBLP/LPWSNO.CPY".
       COPY "LIBLP/LP01LN.CPY".
       COPY "LIBLP/LP01LN_SQL.CPY".
       COPY "LIBLP/LPWSLN.CPY".
       COPY "LIBLP/LP01BW.CPY".
       COPY "LIBLP/LP01BW_SQL.CPY".
       COPY "LIBLP/LPWSBW.CPY".
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBGB/GB01GB.CPY".
       COPY "LIBGB/GB01GB_SQL.CPY".
       COPY "LIBGB/GBWSGB.CPY".
       COPY "LIBGB/GB01GR.CPY".
       COPY "LIBGB/GB01GR_SQL.CPY".
       COPY "LIBGB/GBWSGR.CPY".
       COPY "LIBLP/LP01SP.CPY".
       COPY "LIBLP/LP01SPA.CPY".
       COPY "LIBLP/LP01SPB.CPY".
       COPY "LIBLP/LP01SPC.CPY".
       COPY "LIBLP/LP01SP_SQL.CPY".
       COPY "LIBLP/LPWSSP.CPY".
       COPY "LIBLP/LP01LP.CPY".
       COPY "LIBLP/LP01LP_SQL.CPY".
       COPY "LIBLP/LPWSLP.CPY".
       COPY "LIBLP/LP01LTC.CPY".
       COPY "LIBLP/LP01LTC_SQL.CPY".
       COPY "LIBLP/LPWSLTC.CPY".
       COPY "LIBLP/LP01LTP.CPY".
       COPY "LIBLP/LP01LTP_SQL.CPY".
       COPY "LIBLP/LPWSLTP.CPY".
       COPY "LIBLP/LP01LTI.CPY".
       COPY "LIBLP/LP01LTI_SQL.CPY".
       COPY "LIBLP/LPWSLTI.CPY".
       COPY "LIBLP/LP01LM.CPY".
       COPY "LIBLP/LP01LM_SQL.CPY".
       COPY "LIBLP/LPWSLM.CPY".
       COPY "LIBLP/LP01CRWKBY.CPY".

       EXEC SQL BEGIN DECLARE SECTION END-EXEC.
       01  PASS-ZBAL                      PIC X.
       01  PASS-PL                        PIC X.
       01  PASS-BANK                      PIC X.
       01  PASS-PROMISED                  PIC X.
       01  PASS-NO1STPY                   PIC X.
       01  PASS-OWNED                     PIC X.
       01  PASS-PLACTIVE                  PIC X.
       01  PASS-FROZEN                    PIC X.
       01  PASS-REPO                      PIC X.
       01  PASS-LEGL                      PIC X.
       01  PASS-DEBT                      PIC X.
       EXEC SQL END DECLARE SECTION END-EXEC.

       01  LN3-CONN-OPEN-SW            PIC X           VALUE "N".
       01  LM1-CONN-OPEN-SW            PIC X           VALUE "N".
       01  LP1-CONN-OPEN-SW            PIC X           VALUE "N".

      *----------------------------------------------------------------
       01  WK-IFN        PIC XX       VALUE "WK".

       01  HIDE-FIRST-5-FLAG    PIC X.
       01  USERID-WK.
           03  USERID-SECURITY  PIC X.
           03  USERID-BRANCH    PIC 9(4).
           03  FILLER           PIC X(7).

       01  TEST-ZERO       PIC 9(15)V99 COMP VALUE ZERO.
       01  TEST-NM-ZERO    PIC 9(15)V99 COMP VALUE ZERO.
      ******************************************************************
      *
      *   LINE WORKERS
      *
      ******************************************************************
       01  LN-WORKERS.
           03  LN-FEED          PIC 99       VALUE 0.
           03  LN-COUNT         PIC 99       VALUE 61.
           03  LN-WK            PIC 99       VALUE 0.
       01  PAGE-NUMBER          PIC 9(4)     VALUE 0.

       01  TOTAL-FG             PIC X        VALUE "N".
       01  TEST-NAME            PIC X(4)     VALUE SPACES.
       01  CLASS-FG             PIC X(3)     VALUE "   ".
           88  CLASS-BEG                     VALUE "BEG".
           88  CLASS-END                     VALUE "END".
       01  DEALER-FG            PIC X(5)     VALUE "     ".
           88  DEALER-BEG                    VALUE "DLBEG".
           88  DEALER-END                    VALUE "DLEND".

       01  STARTING-TIME        PIC X(8)     VALUE SPACES.

      ******************************************************************
      *
      *     S C R E E N   F I E L D S
      *
      ******************************************************************
       COPY "LIBGB/EOBUF_EXT.CPY".
      *-----------------------------------------------------------------
           03  AGE-DATE               PIC 9(8).
           03  BEG-BRANCH             PIC 9(4).
           03  END-BRANCH             PIC 9(4).
           03  COLCTIDX.
               05  COLCTID            PIC X OCCURS 3.
           03  ACTIONX.
               05  ACTION-CD          PIC X OCCURS 4.
           03  BEG-CLASS              PIC 99.
           03  BEG-CLASSX REDEFINES BEG-CLASS.
               05  BEG-CLASS1         PIC X.
               05  BEG-CLASS2         PIC X.
           03  END-CLASS              PIC 99.
           03  BEG-DEALER             PIC 9(4)V99.
           03  END-DEALER             PIC 9(4)V99.
           03  BEG-AGE                PIC 9(4).
           03  END-AGE                PIC 9(4).
           03  BEG-LNAMT              PIC 9(6).
           03  END-LNAMT              PIC 9(6).
           03  BEG-CAGE               PIC 9(4).
           03  END-CAGE               PIC 9(4).
           03  CPOT                   PIC X.
           03  CMISS                  PIC X.
           03  BEG-RAGE               PIC 9(4).
           03  END-RAGE               PIC 9(4).
           03  RPOT                   PIC X.
           03  BEG-PAST               PIC 9(3).
           03  BEG-SIGN               PIC X.
           03  END-PAST               PIC 9(3).
           03  END-SIGN               PIC X.
           03  BEG-BAL                PIC 9(6).
           03  END-BAL                PIC 9(6).
           03  OWNED                  PIC X.
           03  RC                     PIC X.
           03  PL                     PIC X.
           03  PLACTIVE               PIC X.
           03  REPO                   PIC X.
           03  BANK                   PIC X.
           03  LEGL                   PIC X.
           03  DEBT                   PIC X.
           03  JD                     PIC X.
           03  MATURED                PIC X.
           03  PROMISED               PIC X.
           03  ZBAL                   PIC X.
           03  NO1STPY                PIC X.
           03  BRSEQ                  PIC X.
           03  PRCOL-FG               PIC X.
           03  COLLECTOR-FG           PIC X.
           03  REPORT-TYPE            PIC X.
           03  PLTYPE                 PIC X.
           03  GROUP-FLAG             PIC X.
           03  ENT-GROUP              PIC X(4).
           03  LOCAL-PRINT-FG         PIC X(1).
           03  PAST-D-AMT             PIC 9(3)V99.
           03  DEL-FLAG               PIC X.
           03  ACTION-FG              PIC X.
           03  TOTALS-FG              PIC X.
               88  TOTALS-FG-VALID    VALUE  "Y" "N" "T" "R".
               88  TOTALS-YES         VALUE  "Y" "T".
               88  RANGES-YES         VALUE  "Y" "R".
           03  BEG-ALPHA              PIC X(4).
           03  END-ALPHA              PIC X(4).
           03  PROTH-FG               PIC X.
           03  MIN-AVAILCR            PIC 9(6).
           03  MIN-AVAILCR-X REDEFINES MIN-AVAILCR PIC X(6).
           03  FROZEN-ISO             PIC X.
           03  CONFIDENT-ISO          PIC X.

       01  VERSION-CONTROL     PIC X    VALUE "D".

       01  SV-BEG-BR           PIC 9999 VALUE 9999.
       01  SV-END-BR           PIC 9999 VALUE 9999.
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

      *-----------------------------------------------------------------
       01  HEAD2.
           03  FILLER           PIC X(10)    VALUE "USER ID:".
           03  H-USERID         PIC X(10).
           03  FILLER           PIC X(26)    VALUE " ".
           03  H-TITLE          PIC X(35)    VALUE
           "COLLECTION  REPORT                 ".
           03  FILLER           PIC X(6)     VALUE " FOR: ".
           03  H-AGEDATE        PIC X(10).

      *-----------------------------------------------------------------
       01  HEAD3.
           03  FILLER      PIC X(10)    VALUE "BRANCH   :".
           03  FILLER      PIC XX       VALUE " ".
           03  H-BRNO      PIC X(4).
           03  FILLER      PIC X        VALUE " ".
           03  H-BRDESC    PIC X(20).

      *-----------------------------------------------------------------
       01  HEAD4X.
           03  FILLER      PIC X(10)    VALUE "COLLECTOR:".
           03  FILLER      PIC XX       VALUE " ".
           03  H-COLCTID   PIC X(3).
           03  FILLER      PIC X        VALUE " ".
           03  H-COLDESC   PIC X(20).

      *-----------------------------------------------------------------
       01  HEAD4.
           03  FILLER      PIC X(10)    VALUE "ACTION   :".
           03  FILLER      PIC XX       VALUE " ".
           03  H-ACTION    PIC X(4).

      *-----------------------------------------------------------------
       01  HEAD5.
           03  FILLER      PIC X(52)    VALUE " ".
           03  FILLER      PIC X(47)    VALUE
           "LOAN-AMT 1ST-PMNT    PROFILES   PDTH PROMISED ".
           03  FILLER      PIC X(20)    VALUE SPACES.
           03  FILLER      PIC X(13)  VALUE "BALANCE   CON".

      *-----------------------------------------------------------------
       01  HEAD6.
           03  FILLER      PIC X(53)    VALUE
           "ACCOUNT #        LOANDATE  DEALER    SC CC  PC TRM   ".
           03  FILLER      PIC X(46)    VALUE
           " LC-AMT MATURITY               LCPD FORMDATE  ".
           03  FILLER      PIC X(34)    VALUE
           "LAST-DUE  NEXT-DUE TODAY-DUE  REC".

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    COMPOSITION OF THIS LINE SHOULD MATCH LONPF1
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

      *-----------------------------------------------------------------
       01  HEAD7.
           03  FILLER      PIC X(50)    VALUE
           "-DATE--CD-REF-----AMOUNT--OTHER--INT/LC-----PRIN--".
           03  FILLER      PIC X(30)    VALUE
           "PDTH--OTHER--INT/LC-----PRIN--".

      * SINGLE LINE HEADINGS
       01  HEAD8.
           03  FILLER             PIC X(40) VALUE
           "ACCOUNT #       DUE    NAME             ".
      *     1234567890123456789012345678901234567890123
           03  HEAD8-PROMPT       PIC X(9)  VALUE "   DEALER".
           03  FILLER             PIC X(33) VALUE
           "   LOAN      CURRENT    LAST     ".
           03  FILLER             PIC X(26) VALUE
           "PROM      AMOUNT     TODAY".
           03  HEAD8-FILLER1      PIC X(7)  VALUE " CONFID".
           03  FILLER             PIC X(16) VALUE "  PDTH  CON  REC".
       01  HEAD9.
           03  FILLER             PIC X(40) VALUE
           "                DAY                     ".
           03  HEAD9-PROMPT       PIC X(9)  VALUE "   NUMBER".
           03  FILLER             PIC X(44)  VALUE
           "   DATE      BALANCE   PAYDATE   DATE       ".
           03  FILLER             PIC X(16)  VALUE
           "PAID        DUE ".
           03  HEAD9-FILLER1      PIC X(8) VALUE " ALLOT ".


      **********************
      *   MISC WORKERS     *
      **********************
       01  SUB                  PIC 9(6)     COMP VALUE 0.
       01  WORK-AMT             PIC S9(9)V99 COMP.
       01  HOLD-TODAY-DATE      PIC 9(8)     COMP.
       01  WS-ACCUM-BAL         PIC S9(7)V99 COMP VALUE 0.
       01  FIRST-TIME           PIC X        VALUE "Y".
       01  NEW-BRANCH           PIC X        VALUE "N".
       01  RECORDS-FOUND        PIC X        VALUE " ".
           88  NO-RECORDS-FOUND              VALUE "N".
       01  MESS-LIST            PIC X(5)     VALUE "MESS.".
       01  WK-DISPLAY-MESS.
           03  FILLER           PIC X(22)    VALUE
           "BUILDING WORK FILE....".
           03  DISPLAY-MESS-BR  PIC 9(4).
           03  FILLER           PIC X(10).
       01  RUN-DISPLAY-MESS.
           03  FILLER           PIC X(24)    VALUE
           "REPORT IN PROGRESS......".
           03  RUN-MESS-BR      PIC 9(4).
           03  FILLER           PIC X(10).
       01  NAME-WORKER          PIC X(34).
       01  FIRST-NAME           PIC X(16).
       01  DASH-LINE            PIC X(132)    VALUE ALL "-".
       01  BEG-PAST-PROM        PIC S9(3)     COMP-3 VALUE 0.
       01  END-PAST-PROM        PIC S9(3)     COMP-3 VALUE 0.

       01  SAVE-CO-CPHONE       PIC 9(10) COMP VALUE 0.
       01  SAVE-CO-PHONE        PIC 9(10) COMP VALUE 0.
       01  SAVE-CO-EMPPHONE     PIC 9(10) COMP VALUE 0.
       01  SAVE-BW-FIELDS.
           03  SAVE-BW-ADR1     PIC X(30).
           03  SAVE-BW-ADR2     PIC X(30).
           03  SAVE-BW-CITY     PIC X(30).
           03  SAVE-BW-STATE    PIC XX.
           03  SAVE-BW-ZIP      PIC X(10).
           03  SAVE-BW-PHONE    PIC 9(10)     COMP.
           03  SAVE-BW-LOANS    PIC 99        COMP.
           03  SAVE-BW-EMPLOYER PIC X(35).
           03  SAVE-BW-EMPPHONE PIC 9(10)     COMP.
           03  SAVE-BW-EMPLPOS  PIC X(24).
           03  SAVE-BW-EMPEXT   PIC 9(4).
           03  SAVE-BW-CPHONE   PIC 9(10)     COMP.

       01  LOAN-MEMO-BUF.
           03  LOAN-DATE-MEMO   OCCURS 4.
               07  LOAN-MEMO-DATE PIC 9(6)      COMP.
               07  LOAN-MEMO      PIC X(60).

       01  MEMO-SUB             PIC 9.

       01  LOAN-PMT-BUF.
           03  LOAN-PMT         PIC X(80)     OCCURS 4.
       01  PMT-SUB              PIC 9.

       01  HOLD-LN-CLASS        PIC 9(3)  VALUE 0.
       01  HOLD-BRNO            PIC 9(4)  VALUE 0.
       01  HOLD-GROUP           PIC X(4)  VALUE SPACES.

       01  OLD-COLCTID-CD       PIC X(3)     VALUE "+++".
       01  COLCTID-WK.
           03  COLCTID-CHAR     PIC X        OCCURS 3.
       01  COLCTID-SUB          PIC 9(6) COMP-3.
       01  COLCTID-MATCH        PIC X.
       01  OLD-ACTION-CD        PIC X(4)     VALUE "++++".
       01  ACTION-WK.
           03  ACTION-CHAR      PIC X        OCCURS 4.
       01  ACTION-SUB           PIC 9(6) COMP-3.
       01  ACTION-MATCH         PIC X.
       01  PROFILE-X.
           03  PROFILE-9        PIC 9(12).
       01  NUM-BW               PIC 9(9).
       01  NUM-BW-WK            REDEFINES NUM-BW.
           03  NUM-BW1          PIC 999.
           03  NUM-BW2          PIC 99.
           03  NUM-BW3          PIC 9999.
       01  ALP-BW.
           03  ALP-BW1          PIC 999.
           03  ALP-HYPHEN1      PIC X.
           03  ALP-BW2          PIC 99.
           03  ALP-HYPHEN2      PIC X.
           03  ALP-BW3          PIC 9999.

       01  RS-DEALER            PIC 9(4)V99.
       01  RS-DLR               REDEFINES RS-DEALER.
           03  FILLER           PIC 9(4).
           03  FILLER           PIC 99.
       01  RENEWABLE-FG         PIC X.
       01  WK-RENEWABLE-PAYS    PIC 9(4).
       01  WK-RENEWABLE-DAYS    PIC 9(4).

      *********************************************
      *               T O T A L S                 *
      *********************************************
      *                                           *
      *      CLASS-TBL:                           *
      *        LTYP(1) = DIRECT LOAN              *
      *            (2) = SALES FINANCE            *
      *                                           *
      *      TOTAL-TBL:                           *
      *         LEV(1) = BY COLLECTOR             *
      *            (2) = BY COMPANY (GRAND TOTALS)*
      *            (3) = BY BRANCH                *
      *
      *     NON-MONTHLY ACCOUNTS
      *           (01) =  CURRENT NONMON
      *           (02) =   1  WEEKS
      *            ....
      *           (31) =  30+ WEEKS
      *                                           *
      *      T-AGETBL:                            *
      *         AGE(1)  =    30 DAYS OLD          *
      *            (2)  =    60 DAYS OLD          *
      *            (3)  =    90 DAYS OLD          *
      *            (4)  =   120 DAYS OLD          *
      *            (5)  =   150 DAYS OLD          *
      *            (6)  =   180 DAYS OLD          *
      *            (7)  =   210 +                 *
      *            (8)  =   <M> MISPAYS           *
      *            (9)  =   <P> POTS              *
      *           (10)  =   CURRENT               *
      *                                           *
      *      T-TYPETBL:                           *
      *         TYP(1)  = CONTRACTUAL             *
      *            (2)  = RECENCY                 *
      *            (3)  = PAID AHEAD              *
      *                   CONTRACTUALLY CURRENT   *
      *                    AND DELINQ-RECENCY     *
      *                                           *
      *********************************************
       01  TOTAL-TABLE             VALUE ZEROS.
           03  CLASS-TBL           OCCURS 2 TIMES.
              05  TOTAL-TBL           OCCURS 3 TIMES.
                  07  T-NM-TBL           OCCURS 31 TIMES.
                      09  T-NM-NO       PIC S9(7).
                      09  T-NM-BAL      PIC S9(9)V99.
                      09  T-NM-INT      PIC S9(7)V99.
                      09  T-NM-OTH      PIC S9(9)V99.
                      09  T-NM-OTH2     PIC S9(9)V99.
                      09  T-NM-TDUE     PIC S9(9)V99.
                      09  T-NM-UNE      PIC S9(9)V99.
                      09  T-NM-ERN      PIC S9(7)V99.
                      09  T-NM-IER      PIC S9(7)V99.
                  07  T-AGETBL        OCCURS 10 TIMES.
                      09  T-TYPETBL   OCCURS 3 TIMES.
                          11  T-NO    PIC S9(5)     COMP.
                          11  T-BAL   PIC S9(9)V99  COMP.
                          11  T-INT   PIC S9(7)V99  COMP.
                          11  T-OTH   PIC S9(7)V99  COMP.
                          11  T-OTH2  PIC S9(7)V99  COMP.
                          11  T-TDUE  PIC S9(9)V99  COMP.
                          11  T-UNE   PIC S9(7)V99  COMP.
                          11  T-ERN   PIC S9(7)V99  COMP.
                          11  T-IER   PIC S9(7)V99  COMP.

       01  TT-TOTALS              VALUE ZEROS.
           03  TT-CLASS           OCCURS 2 TIMES.
               05  TT-NM-TBL.
                   07  TT-NM-NO       PIC S9(7).
                   07  TT-NM-BAL      PIC S9(9)V99.
                   07  TT-NM-INT      PIC S9(7)V99.
                   07  TT-NM-OTH      PIC S9(9)V99.
                   07  TT-NM-OTH2     PIC S9(7)V99  COMP.
                   07  TT-NM-TDUE     PIC S9(9)V99.
                   07  TT-NM-UNE      PIC S9(9)V99.
                   07  TT-NM-ERN      PIC S9(7)V99.
                   07  TT-NM-IER      PIC S9(7)V99.
               05  TT-TTYPETBL         OCCURS 3 TIMES.
                   07  TT-NO          PIC S9(5)     COMP.
                   07  TT-BAL         PIC S9(9)V99  COMP.
                   07  TT-INT         PIC S9(7)V99  COMP.
                   07  TT-OTH         PIC S9(7)V99  COMP.
                   07  TT-OTH2        PIC S9(7)V99  COMP.
                   07  TT-TDUE        PIC S9(9)V99  COMP.
                   07  TT-UNE         PIC S9(7)V99  COMP.
                   07  TT-ERN         PIC S9(7)V99  COMP.
                   07  TT-IER         PIC S9(7)V99  COMP.

      * PORTFOLIO-TOTALS:
      *         LEVEL (1) = DIRECT LOAN TOTALS.
      *               (2) = SALES FINANCE TOTALS.
       01  PORTFOLIO-TOTALS  OCCURS 2 TIMES.
           03  PORT-NO        PIC S9(8)      COMP.
           03  PORT-AMT       PIC S9(13)V99  COMP.

      * ACCUMULATED PORTFOLIO TOTALS USED IN GRAND TOTALS.
      *
       01  ACCUM-DL-NO            PIC S9(8)     COMP.
       01  ACCUM-DL-AMT           PIC S9(13)V99 COMP.
       01  ACCUM-SF-NO            PIC S9(8)     COMP.
       01  ACCUM-SF-AMT           PIC S9(13)V99 COMP.

       01  LTYP                   PIC 9(6)  COMP.
       01  LEV                    PIC 9(6)  COMP.
       01  AGE                    PIC 9(6)  COMP.
       01  P-AGE                  PIC 9(6)  COMP.
       01  M-AGE                  PIC 9(6)  COMP.
       01  TYP                    PIC 9(6)  COMP.

       01  AGE-DESC-TABLE.
           03  FILLER             PIC X(12) VALUE "    30 DAYS:".
           03  FILLER             PIC X(12) VALUE "    60 DAYS:".
           03  FILLER             PIC X(12) VALUE "    90 DAYS:".
           03  FILLER             PIC X(12) VALUE "   120 DAYS:".
           03  FILLER             PIC X(12) VALUE "   150 DAYS:".
           03  FILLER             PIC X(12) VALUE "   180 DAYS:".
           03  FILLER             PIC X(12) VALUE "   210 +   :".
           03  FILLER             PIC X(12) VALUE "   MIS-PAYS:".
           03  FILLER             PIC X(12) VALUE "   POT 30'S:".
           03  FILLER             PIC X(12) VALUE "   CURRENT :".
       01  AGE-DESC-TBL           REDEFINES AGE-DESC-TABLE.
           03  AGE-DESC           OCCURS 10 TIMES
                                  PIC X(12).

       01  DETAIL-LINE          PIC X(132) VALUE SPACES.

       01  D-LINE1 REDEFINES DETAIL-LINE.
           03  D-LOAN-NUM  PIC X(16).
           03  FILLER      PIC X.
           03  D-LOANDATE  PIC 99/99/99.
           03  FILLER      PIC X.
           03  D-DLNO      PIC 9(4).99     BLANK ZERO.
           03  FILLER      PIC X(4).
           03  D-SRCD      PIC XX.
           03  FILLER      PIC X.
           03  D-COLLCD    PIC XX.
           03  FILLER      PIC X.
           03  D-PURCD     PIC XXX.
           03  FILLER      PIC X.
           03  D-ORGTERM   PIC ZZ9.
           03  FILLER      PIC X.
           03  D-LNAMT     PIC Z(5)9.99   BLANK ZERO.
           03  FILLER      PIC X.
           03  D-1STPYDATE PIC 99/99/99    BLANK ZERO.
           03  FILLER      PIC X.
           03  FILLER      PIC X.
           03  D-CONPROF   PIC X(12).
           03  FILLER      PIC X.
           03  D-PDTH      PIC 9999.
           03  FILLER      PIC X.
           03  D-PROMISED  PIC 99/99/99    BLANK ZERO.
           03  FILLER      PIC XX.
           03  D-LASTDATE  PIC 99/99/99    BLANK ZERO.
           03  FILLER      PIC XX.
           03  D-NEXTDATE  PIC 99/99/99   BLANK ZERO.
           03  FILLER      PIC X.
           03  D-BALANCE   PIC Z(5)9.99-  BLANK ZERO.
           03  D-CONX.
               05  D-CON   PIC 9999.

       01  D-LINE2 REDEFINES DETAIL-LINE.
           03  FILLER          PIC X.
           03  D-MAKERCD       PIC X.
           03  FILLER          PIC X.
           03  D-SSNO          PIC X(11).
           03  D-SSNO-X-OUT REDEFINES D-SSNO.
               05  D-SOC1      PIC X(3).
               05  FILLER      PIC X.
               05  D-SOC2      PIC XX.
               05  FILLER      PIC X.
               05  D-SOC3      PIC X(4).
           03  FILLER          PIC X.
           03  D-NAME          PIC X(25).
           03  FILLER          PIC X.
           03  D-MSTAG         PIC X(11).
           03  FILLER          PIC X.
           03  D-LCAMT         PIC Z(3)9.99    BLANK ZERO.
           03  FILLER          PIC X.
           03  D-MATURDATE     PIC 99/99/99    BLANK ZERO.
           03  FILLER          PIC X.
           03  FILLER          PIC X.
           03  D-RECPROF       PIC X(12).
           03  FILLER          PIC X.
           03  D-LCPD.
               05  D-LCPD-MO   PIC 99.
               05  D-LCPD-DAYR PIC 99.
           03  FILLER          PIC X.
           03  D-FORMDATE      PIC 99/99/99    BLANK ZERO.
           03  FILLER          PIC X.
           03  D-LASTAMT       PIC Z(5)9.99-   BLANK ZERO.
           03  D-NEXTAMT       PIC Z(5)9.99-   BLANK ZERO.
           03  D-TODAYDUE      PIC Z(5)9.99-   BLANK ZERO.
           03  D-RECX.
               05  D-REC       PIC ZZZ9.

       01  D-LINE2A REDEFINES DETAIL-LINE.
           03  D-COMAKER-PHONE PIC X(5).
           03  D-CO-CELL       PIC X.
           03  D-CO-CPHONE     PIC 999/999/9999 BLANK ZERO.
           03  FILLER          PIC X.
           03  D-CO-WORK       PIC X.
           03  D-CO-WPHONE     PIC 999/999/9999 BLANK ZERO.
           03  FILLER          PIC X.
           03  D-CO-HOME       PIC X.
           03  D-CO-HPHONE     PIC 999/999/9999 BLANK ZERO.

       01  D-LINE3 REDEFINES DETAIL-LINE.
           03  FILLER      PIC X(40).
           03  D-PHTAG     PIC X(7).
           03  D-CELL      PIC X.
           03  D-CPHONE    PIC 999/999/9999 BLANK ZERO.
           03  FILLER      PIC X.
           03  D-WORK      PIC X.
           03  D-WPHONE    PIC 999/999/9999 BLANK ZERO.
           03  D-WORKEXT   PIC X.
           03  D-WEXT      PIC Z(4).
           03  FILLER      PIC X.
           03  D-HOME      PIC X.
           03  D-HPHONE    PIC 999/999/9999 BLANK ZERO.
           03  FILLER      PIC X.
           03  D-N         PIC X.
           03  D-FORMNO    PIC 9999.
           03  FILLER      PIC X.
           03  D-LDUELC    PIC Z(3)9.99  BLANK ZERO.
           03  FILLER      PIC XX.
           03  D-NXDUELC    PIC Z(4)9.99  BLANK ZERO.
           03  FILLER      PIC XX.
           03  D-TDUELC   PIC Z(4)9.99  BLANK ZERO.
           03  FILLER      PIC X.
       01  D-LINE3B REDEFINES DETAIL-LINE.
           03  FILLER       PIC X(106).
           03  D-OTHERX     PIC X(13).
           03  D-OTHER-BALS PIC Z(4)9.99-.
           03  FILLER       PIC X.

       01  D-LINE4 REDEFINES DETAIL-LINE.
           03  FILLER      PIC X(15).
           03  D-ADDR      PIC X(30).
           03  FILLER      PIC X.
           03  D-EMPLOYER.
               05  D-LOANSX    PIC X(8).
               05  D-LOAN-NO.
                   07  FILLER  PIC X.
                   07  D-LOANS PIC Z9.
                   07  FILLER  PIC X(5).
               05  D-PMTX REDEFINES D-LOAN-NO.
                   07  D-PMT   PIC Z(4)9.99   BLANK ZERO.
           03  FILLER          PIC X.
           03  D-MEMOX.
               05  D-MEMO-DT   PIC 9(6).
               05  FILLER      PIC X.
               05  D-MEMO      PIC X(60).
           03  FILLER          PIC X(2).

       01  D-LINE5 REDEFINES DETAIL-LINE.
           03  D-SLOAN-NUM    PIC X(16).
           03  FILLER         PIC X.
           03  D-SDUEDAY      PIC 99.
           03  FILLER         PIC X.
           03  D-SNAME        PIC X(20).
           03  FILLER         PIC X.
           03  D-SDLNO        PIC ZZZZ9.99     BLANK ZERO.
           03  FILLER         PIC X.
           03  D-SLNDATE      PIC 99/99/99     BLANK ZERO.
           03  FILLER         PIC X.
           03  D-SBALANCE     PIC Z(6)9.99-    BLANK ZERO.
           03  FILLER         PIC X.
           03  D-SLASTDATE    PIC 99/99/99     BLANK ZERO.
           03  FILLER         PIC X.
           03  D-SPROMDATE    PIC 99/99/99     BLANK ZERO.
           03  D-STOTAMTPD    PIC Z(6)9.99-    BLANK ZERO.
           03  D-STODAYDUE    PIC Z(5)9.99-    BLANK ZERO.
           03  D-COMMON-AREA1.
               05  FILLER         PIC XXX.
               05  D-SFLAG        PIC X(3).
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
               05  FILLER         PIC X(2).
           03  D-COMMON-AREA2 REDEFINES D-COMMON-AREA1.
               05  D-SOTHBAL  PIC Z(3)9.99-.
           03  D-SPDTH        PIC 9(4).
           03  FILLER         PIC X(1).
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           03  D-SCONX.
               05  D-SCON     PIC ZZZ9         BLANK ZERO.
           03  FILLER         PIC X.
           03  D-SRECX.
               05  D-SREC     PIC ZZZ9         BLANK ZERO.

       01  D-LINE6  REDEFINES DETAIL-LINE.
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
           03  FILLER         PIC X(4).

       01  PMT-LINE REDEFINES DETAIL-LINE.
           03  FILLER      PIC X(52).
      *    COMPOSITION OF THIS LINE SHOULD MATCH LONPF1
           03  D-PMT-LINE.
               05  L-010      PIC 9(6).
               05  FILLER     PIC X.
               05  L-020      PIC XX.
               05  FILLER     PIC X.
               05  L-030      PIC X(5).
               05  L-040      PIC Z(5).99- BLANK ZERO.
               05  L-040RATE  REDEFINES L-040
                              PIC ZZZZ.ZZZ-.
               05  L-050      PIC Z(4).99- BLANK ZERO.
               05  L-060      PIC Z(4).99- BLANK ZERO.
               05  L-070      PIC Z(5).99- BLANK ZERO.
               05  L-080-MO   PIC 99.
               05  L-080-DAYR PIC 99.
               05  L-090      PIC Z(5).99- BLANK ZERO.
               05  L-100      PIC Z(4).99- BLANK ZERO.
               05  L-110      PIC Z(5)9.99.
               05  L-120      PIC X.

       01  SUMMARY-LINE1 REDEFINES DETAIL-LINE.
           03  S-SUMNAME           PIC X(20).
           03  FILLER              PIC X(112).
       01  SUMMARY-LINE2 REDEFINES DETAIL-LINE.
           03  FILLER              PIC X(53).
           03  S-AGETYPE           PIC X(79).
       01  SUMMARY-LINE2B REDEFINES DETAIL-LINE.
           03  FILLER           PIC X(38).
           03  S-DESC           PIC X(12).
           03  FILLER           PIC X(44).
           03  S-DESC2          PIC X(14).
           03  FILLER           PIC X(7).
       01  SUMMARY-LINE3 REDEFINES DETAIL-LINE.
           03  S-NAME              PIC X(12).
           03  FILLER              PIC X(2).
           03  SUMMARY-LINE3X      OCCURS 2 TIMES.
               05  S-NO                PIC ZZZZ9-        BLANK ZERO.
               05  S-BALANCE-X         PIC X(18).
               05  S-BALANCE  REDEFINES S-BALANCE-X.
                   07  FILLER           PIC X(4).
                   07  S-BAL-10         PIC ZZZZZZZZZ9.99-.
               05  FILLER REDEFINES   S-BALANCE-X.
                   07  S-NO-P          PIC ZZ9.9-        BLANK ZERO.
                   07  S-BAL           PIC ZZZZZZZ9.99-  BLANK ZERO.

               05  S-BAL-P             PIC ZZ9.9-        BLANK ZERO.
               05  S-OTH               PIC ZZZZZZ9.99-   BLANK ZERO.
               05  S-TDUE              PIC ZZZZZZZ9.99-  BLANK ZERO.
               05  S-TDUE-P            PIC ZZ9.9-        BLANK ZERO.
               05  FILLER              PIC X(3).


       01  RANGE-LN REDEFINES DETAIL-LINE.
           03  RANGE-LINE        PIC X(20).
           03  RANGE-SELECTION   PIC X(112).
           03  RANGE-SELNUM     REDEFINES RANGE-SELECTION.
               05  RANGE-NUM     PIC Z(6).99.
               05  FILLER        PIC X(103).

       01  SUMMARY1.
           03  FILLER           PIC X(17)  VALUE SPACES.
           03  FILLER           PIC X(53)  VALUE
           "NO----%      BAL DUE----%     --OTHER--  TDAY DUE---%".
           03  FILLER           PIC X(9).
           03  FILLER           PIC X(53)  VALUE
           "NO----%      BAL DUE----%     --OTHER--  TDAY DUE---%".


       COPY "LIBGB/PASSWDW.CPY".
       78  UNMASK-SSNO-SEQ     VALUE 1.
       COPY "LIBWI/GRSCANW.CPY".
       COPY "LIBLP/LPAPRSW.CPY".
       COPY "LIBLP/LPITRMW.CPY".
       COPY "LIBWI/LCSCANW.CPY".
       COPY "LIBLP/LPAPRZW.CPY".
       COPY "LIBLP/LPUPERW.CPY".
       COPY "LIBGB/GRWORKW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBWI/DLSCANW.CPY".
       COPY "LIBLP/SETCTBLW.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBGB/SPPDTHW.CPY".
       COPY "LIBGB/BRSECUREW.CPY".
       COPY "LIBLP/LPAMTSW.CPY".
       COPY "LIBGB/AGEINGW.CPY".
       COPY "LIBLP/LPUPWKW.CPY".
       COPY "LIBLP/LPAVCRW.CPY".
       COPY "LIBLP/LPLCASW.CPY".
       COPY "LIBLP/LPLCHGW.CPY".
       COPY "LIBLP/LPSCHDW.CPY".
       COPY "LIBLP/LPLCAPW.CPY".
       COPY "LIBLP/LPMDTEW.CPY".
       COPY "LIBLP/LPXDTEW.CPY".
       COPY "LIBLP/LPCEPPW.CPY".
       COPY "LIBLP/LPPOFFW.CPY".
       COPY "LIBLP/ADDONSPRW.CPY".
       COPY "LIBLP/LPCRNOW.CPY".
       COPY "LIBLP/LPDEFW.CPY".
       COPY "LIBLP/DEFPOLW.CPY".

       COPY "LIBLP/LPRATEW.CPY".
       COPY "LIBLP/LPIBPCW.CPY".
       COPY "LIBLP/LPPDUEW.CPY".
       COPY "LIBLP/LPCDELW.CPY".
       COPY "LIBGB/REBATEW.CPY".
       COPY "LIBGB/SYSTEMW.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/DACCESSW.CPY".
       COPY "LIBLP/LPSTATW.CPY".
       COPY "LIBGB/MKTEMPW_CMD.CPY".
       COPY "LIBGB/GETPRW.CPY".
       COPY "LIBCL/SUMMBYW.CPY".
       COPY "LIBGB/EOSPOOLW.CPY".
       COPY "LIBGB/RBACTW.CPY".
       COPY "LIBGB/GPENVW.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBLP/CPCORP_DEF.CPY".
       COPY "LIBLP/CPCORP_WKS.CPY".
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
       COPY "LIBLP/CPCORP_SCN.CPY".

       PROCEDURE DIVISION
             USING FORM-PATHNAME EXIT-PATHNAME.
       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
               PR-FILE WK-FILE.
       COPY "LIBGB/DECLRP.CPY".
           PERFORM CLOSE-SP1-FILE.
           PERFORM CLOSE-LTP1-FILE.
           PERFORM CLOSE-LTI1-FILE.
           PERFORM CLOSE-LTC1-FILE.
           PERFORM CLOSE-LP1-FILE.
           PERFORM CLOSE-LN3-FILE.
      *     PERFORM CLOSE-LN1-FILE.
           PERFORM CLOSE-LM1-FILE.
           PERFORM CLOSE-LC1-FILE.
           PERFORM CLOSE-BW1-FILE.
           PERFORM CLOSE-GR1-FILE.
           PERFORM CLOSE-BR-FILE.
           PERFORM CLOSE-GB-FILE.
           PERFORM CLOSE-PR-FILE.
           PERFORM CLOSE-LN3-CONNECTION.
           PERFORM CLOSE-LM1-CONNECTION.
           PERFORM CLOSE-LP1-CONNECTION.
           CLOSE
               WK-FILE.
       COPY "LIBGB/DECLRP2.CPY".

      ******************************************************************
      *
      *    M A I N   M O D U L E
      *
      ******************************************************************
       MAIN-MODULE SECTION.

           PERFORM SQL-CONNECT.

      D    STOP MESS.
           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "CPCORP" TO VDU-FORM-NAME.

           PERFORM GET-GPENV.

           PERFORM INITIALIZATION.
           IF EXT-EOBUF-ERRCD NOT = " "
              GO TO END-PROGRAM.

           MOVE GP-HIDE-FIRST-5-SSNO TO HIDE-FIRST-5-FLAG.

      * FINMGM PR# 4734 PASSWORD THE ABILITY TO UNMASK THE FIRST 5 OF SSNO
      *                 FOR "A" LOGIN USERS
           IF EXT-ROUTE-BUF = "00"
              IF HIDE-FIRST-5-FLAG = "Y"
                 IF REPORT-TYPE NOT = "L"
                    MOVE EXT-CONAME-USER-ID TO USERID-WK
                    IF USERID-SECURITY = X"61"
                       MOVE UNMASK-SSNO-SEQ TO PASSWDW-SEQ
                       MOVE "TO UNMASK FIRST 5 OF SSNO"
                                           TO PASSWDW-PROMPT2
                       PERFORM VERIFY-PASSWORD
                       IF PASSWDW-STAT = "OK"
                          MOVE "N" TO HIDE-FIRST-5-FLAG.


      * NEEDED FOR GP-NO-LC-IN-ARREARAGE IN LIBLP/CPCORP DETAIL-LINE
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
           IF IO-BAD
              GO TO IO-ERROR.

           GO TO MOVE-BRANCH-PATH-INFO.

       NEXT-BRANCH-NO.
           PERFORM READ-BR-FILE-NEXT.
           IF IO-BAD OR BR-NO > END-BRANCH
              GO TO END-ROUTINE.

       MOVE-BRANCH-PATH-INFO.
           IF ( LOCAL-PRINT-FG = "Y" )
              PERFORM CREATE-SPOOL-DIR
              IF ( STAT NOT = "00" )
                 GO TO NEXT-GROUP-LOC.

           MOVE EXT-FILPATH-BASE           TO FILPATH-BASE.
           MOVE BR-MACHINE                 TO FILPATH-MACHINE.
           MOVE BR-DATA-PATH               TO FILPATH-DATA.

           MOVE "B"                        TO BW-PATH-OVERRIDE
                                              NO-PATH-OVERRIDE
                                              LM-PATH-OVERRIDE
                                              LN-PATH-OVERRIDE
                                              LP-PATH-OVERRIDE
                                              NO-PATH-OVERRIDE
                                              LTC-PATH-OVERRIDE
                                              LTP-PATH-OVERRIDE
                                              LTI-PATH-OVERRIDE
                                              GB-PATH-OVERRIDE.

      * NEED FOR LCAS ROUTINES (REPLACES GLOBRD)
           PERFORM OPEN-GB-FILE.
           MOVE GB-PATH-OWNBR TO GB-BRNO.
           MOVE 1             TO GB-NO.
           PERFORM READ-GB-FILE.
           PERFORM CLOSE-GB-FILE.
           IF IO-BAD
              GO TO NEXT-GROUP-LOC.

           PERFORM OPEN-WK-FILE-OUTPUT.
           PERFORM CLOSE-WK-FILE.
           PERFORM OPEN-WK-FILE.
           PERFORM OPEN-BW1-FILE.
           PERFORM OPEN-NO1-FILE.
      *     PERFORM OPEN-LN1-FILE.
           PERFORM OPEN-LP1-FILE.

           IF PRU-LP NOT = "VDU" AND NOT = "PRU"
              IF EXT-ROUTE-BUF = "00"
                 MOVE BR-NO TO DISPLAY-MESS-BR
                 MOVE WK-DISPLAY-MESS TO MESS
                 PERFORM SEND-LEGEND.

           MOVE BR-NO     TO LN-OWNBR
                             QLN3-WBEG-OWNBR
                             QLN3-WEND-OWNBR.
           IF BEG-CLASSX NUMERIC
              MOVE BEG-CLASS TO LN-CLASS
           ELSE
              MOVE 0         TO LN-CLASS.

           MOVE LN-CLASS     TO QLN3-WBEG-CLASS.
           MOVE END-CLASS    TO QLN3-WEND-CLASS.
           MOVE PL           TO PASS-PL.
           MOVE ZBAL         TO PASS-ZBAL.
           MOVE BANK         TO PASS-BANK.
           MOVE PROMISED     TO PASS-PROMISED.
           MOVE NO1STPY      TO PASS-NO1STPY.
           MOVE OWNED        TO PASS-OWNED.
           MOVE PLACTIVE     TO PASS-PLACTIVE.
           MOVE FROZEN-ISO   TO PASS-FROZEN.
           MOVE REPO         TO PASS-REPO.
           MOVE LEGL         TO PASS-LEGL.
           MOVE DEBT         TO PASS-DEBT.
      *     MOVE 0            TO LN-ACCTNO
      *                          QLN3-WBEG-ACCTNO.
      *     MOVE ALL "9"      TO QLN3-WEND-ACCTNO.

           PERFORM START-LN3-FILE.  *> LN, BW, NO
       NEXT-LN.
           PERFORM READ-LN3-FILE-NEXT.    *> LN, BW, NO
           IF IO-BAD                     *> OR (LN-OWNBR NOT = LN-PATH-OWNBR)
              GO TO PROCESS-WK-FILE.

      * THIS CHECK ALSO IN STORED PROCEDURE
           IF LN-CLASS > END-CLASS
              GO TO PROCESS-WK-FILE.

      * THIS CHECK ALSO IN STORED PROCEDURE
           IF ( LN-POCD-NONLOAN )
              GO TO NEXT-LN.

           IF BEG-CLASSX NOT NUMERIC
              PERFORM TEST-CLASS
              IF IO-FG NOT = 0
                 GO TO NEXT-LN
              END-IF
           ELSE
              IF LN-CLASS < BEG-CLASS
                 GO TO NEXT-LN.

           IF LN-DLNO < BEG-DEALER OR LN-DLNO > END-DEALER
              GO TO NEXT-LN.

           IF LN-SUM-LNAMT < BEG-LNAMT OR LN-SUM-LNAMT > END-LNAMT
              GO TO NEXT-LN.

           COMPUTE WS-ACCUM-BAL = LN-CURBAL + LN-OTHBAL + LN-INTBAL
                                  + LN-LCBAL.

           IF WS-ACCUM-BAL < BEG-BAL OR WS-ACCUM-BAL > END-BAL
              GO TO NEXT-LN.

           MOVE LN-COLLECTOR TO COLCTID-WK.
           PERFORM COLCTID-CD-CHECK.

           IF COLCTID-MATCH NOT = "Y"
              GO TO NEXT-LN.

           MOVE LN-ACTIONCD TO ACTION-WK.
           PERFORM ACTION-CD-CHECK.
           IF ACTION-MATCH NOT = "Y"
              GO TO NEXT-LN.

      * THIS CHECK ALSO IN STORED PROCEDURE
           IF OWNED = "O"
              IF LN-SERVACCT = "Y"
                 GO TO NEXT-LN.

      * THIS CHECK ALSO IN STORED PROCEDURE
           IF OWNED = "S"
              IF LN-SERVACCT = "N"
                 GO TO NEXT-LN.

      * THIS CHECK ALSO IN STORED PROCEDURE
           IF PL = "O"
              IF LN-PLDATE = 0
                 GO TO NEXT-LN.

      * THIS CHECK ALSO IN STORED PROCEDURE
           IF PL = "S"
              IF LN-PLDATE NOT = 0
                 GO TO NEXT-LN.

      * THIS CHECK ALSO IN STORED PROCEDURE
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

       CONT-LN.
           IF JD = "O"
              IF LN-JDDATE = 0
                 GO TO NEXT-LN.

           IF JD = "S"
              IF LN-JDDATE NOT = 0
                 GO TO NEXT-LN.

      * THIS CHECK ALSO IN STORED PROCEDURE
           IF REPO = "O"
              IF NOT (LN-REPOCD = "X" OR "S")
                 GO TO NEXT-LN.

      * THIS CHECK ALSO IN STORED PROCEDURE
           IF REPO = "S"
              IF LN-REPOCD = "X" OR "S"
                 GO TO NEXT-LN.

      * THIS CHECK ALSO IN STORED PROCEDURE
           IF BANK = "O"
              IF LN-BNKRPTDATE = 0
                 GO TO NEXT-LN.

      * THIS CHECK ALSO IN STORED PROCEDURE
           IF BANK = "S"
              IF LN-BNKRPTDATE NOT = 0
                 GO TO NEXT-LN.

      * THIS CHECK ALSO IN STORED PROCEDURE
           IF LEGL = "O"
              IF LN-LEGALDATE = 0
                 GO TO NEXT-LN.

      * THIS CHECK ALSO IN STORED PROCEDURE
           IF LEGL = "S"
              IF LN-LEGALDATE NOT = 0
                 GO TO NEXT-LN.

      * THIS CHECK ALSO IN STORED PROCEDURE
           IF DEBT = "O"
              IF LN-DEBTAIDATE = 0
                 GO TO NEXT-LN.

      * THIS CHECK ALSO IN STORED PROCEDURE
           IF DEBT = "S"
              IF LN-DEBTAIDATE NOT = 0
                 GO TO NEXT-LN.

      * THIS CHECK ALSO IN STORED PROCEDURE
           IF FROZEN-ISO = "O"
              IF LN-ACCT-FROZEN NOT = "Y"
                 GO TO NEXT-LN.

      * THIS CHECK ALSO IN STORED PROCEDURE
           IF FROZEN-ISO = "S"
              IF LN-ACCT-FROZEN = "Y"
                 GO TO NEXT-LN.

      * THIS CHECK ALSO IN STORED PROCEDURE
           IF ZBAL = "O"
              IF LN-CURBAL NOT = 0 OR
                  LN-INTBAL NOT = 0 OR
                   LN-LCBAL NOT = 0 OR
                    LN-OTHBAL NOT = 0
                      GO TO NEXT-LN.

      * THIS CHECK ALSO IN STORED PROCEDURE
           IF ZBAL = "S"
              IF LN-CURBAL = 0
                 IF LN-INTBAL = 0
                    IF LN-LCBAL = 0
                       IF LN-OTHBAL = 0
                       GO TO NEXT-LN.

      * THIS CHECK ALSO IN STORED PROCEDURE - FIRST SKIP CHECK
           IF NO1STPY = "O"
              IF LN-MIDSTREAM NOT = "Y"
                 IF LN-DATE-PAID-LAST NOT = 0
                    GO TO NEXT-LN
                 END-IF
              ELSE
                 IF BR-MIDSTREAM-1STPAY-FG NOT = "Y"
                    IF LN-DATE-PAID-LAST NOT = 0
                       GO TO NEXT-LN
                    END-IF
                 ELSE
                    IF LN-ORIGPURAMT NOT = LN-CURBAL
                       GO TO NEXT-LN.

      * THIS CHECK ALSO IN STORED PROCEDURE - FIRST SKIP CHECK
           IF NO1STPY = "S"
              IF LN-MIDSTREAM NOT = "Y"
                 IF LN-DATE-PAID-LAST = 0
                    GO TO NEXT-LN
                 END-IF
              ELSE
                 IF BR-MIDSTREAM-1STPAY-FG NOT = "Y"
                    IF LN-DATE-PAID-LAST = 0
                       GO TO NEXT-LN
                    END-IF
                 ELSE
                    IF LN-ORIGPURAMT = LN-CURBAL
                       GO TO NEXT-LN.

      *  BORROWER INFORMATION RETREIVED IN STORED PROCEDURE
      *   BW-LNAME = SPACES IF BW REC NOT FOUND
      *     MOVE LN-SSNO(1) TO BW-SSNO.
      *     PERFORM READ-BW1-FILE.
      *     IF IO-BAD
            IF BW-LNAME = SPACES
              MOVE "ZZZZ" TO BW-LNAME
              MOVE "N" TO BW-CONFIDENTIAL.

      * EITHER THE LOAN OR THE BORROWER MUST BE CONFIDENTIAL
           IF CONFIDENT-ISO = "O"
              IF LN-CONFIDENTCD NOT = "Y"
                 IF BW-CONFIDENTIAL NOT = "Y"
                    GO TO NEXT-LN.

      * NEITHER THE LOAN NOR THE BORROWER CAN BE CONFIDENTIAL
           IF CONFIDENT-ISO = "S"
              IF LN-CONFIDENTCD = "Y" OR BW-CONFIDENTIAL = "Y"
                 GO TO NEXT-LN.

           IF NOT (BEG-ALPHA = "    " AND END-ALPHA = "ZZZZ")
              MOVE BW-LNAME TO TEST-NAME
              IF TEST-NAME < BEG-ALPHA
                 OR TEST-NAME > END-ALPHA
                    GO TO NEXT-LN.

            PERFORM GET-SP.
            IF IO-FG NOT = 0
               GO TO NEXT-LN.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      * COMPUTE CONTRACTUAL:
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF LN-ACCTNO = 3579
      D       STOP MESS
               MOVE SPACES TO MESS.
           MOVE HOLD-TODAY-DATE TO AGEING-DATE.
           PERFORM COMPUTE-CONTRACTUAL.

           IF CONTRACTUAL < BEG-CAGE OR CONTRACTUAL > END-CAGE
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

           MOVE HOLD-TODAY-DATE TO PDUE-TODAY-DATE.
           MOVE LN-1STPYDATE TO PDUE-1STPYDATE.
           PERFORM PAST-DUE-DAYS-CALCULATION.

           IF PDUE-DAYS < BEG-AGE OR > END-AGE
              GO TO NEXT-LN.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      * TEST PAST DUE AMOUNT:
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF PAST-D-AMT NOT = 0
              PERFORM COMPUTE-PAST-DUE-AMT
              IF CDEL-ARREARAGE < PAST-D-AMT
                 GO TO NEXT-LN.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      * TEST RECENCY:
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

           MOVE HOLD-TODAY-DATE TO AGEING-DATE.
           PERFORM COMPUTE-RECENCY.

           IF BR-RECDEL-NOT-REC = "Y"
              MOVE RDSUB TO RSUB
              MOVE RECDEL TO RECENCY.

           IF RECENCY < BEG-RAGE OR RECENCY > END-RAGE
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

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      * TEST FOR PAST PROMISED
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

           IF PROMISED = "I"
              IF LN-PROMDATE = 0
                 GO TO TEST-MIN-AVAIL
              ELSE
                 GO TO TEST-PASTPROM-DAY.

      * THIS CHECK ALSO IN STORED PROCEDURE
           IF PROMISED = "S"
              IF LN-PROMDATE NOT = 0
                 GO TO NEXT-LN
              ELSE
                 GO TO TEST-MIN-AVAIL.

      * THIS CHECK ALSO IN STORED PROCEDURE
           IF PROMISED = "O"
              IF LN-PROMDATE = 0
                 GO TO NEXT-LN.

       TEST-PASTPROM-DAY.

           IF BEG-PAST-PROM = -999
              IF END-PAST-PROM = 999
                 GO TO TEST-MIN-AVAIL.

           MOVE LN-PROMDATE TO NUM-DATE.
           MOVE HOLD-TODAY-DATE TO SYS-DATE.
           PERFORM TIM.

           IF ELAPSED-DAYS < BEG-PAST-PROM OR
               ELAPSED-DAYS > END-PAST-PROM
                  GO TO NEXT-LN.

       TEST-MIN-AVAIL.

      * MINIMUM AVAILABLE CREDIT
           IF MIN-AVAILCR-X NOT = SPACES
              IF MIN-AVAILCR NOT = 0
                 PERFORM GET-AVAILABLE-CREDIT
                 IF AVCR-AVAILCREDIT < MIN-AVAILCR
                    GO TO NEXT-LN.

      * TEST FOR MATURED ACCOUNTS

           IF MATURED = "I"
              GO TO FILL-WORK-FLDS.

           PERFORM MATURITY-DATE-CALCULATION.

           IF LN-TOTNOALDEL NOT = 0
              MOVE MDTE-DATE TO NDTE-DATE
              MOVE LN-TOTNOALDEL TO NDTE-HOLD
              MOVE LN-UNITPER-CD TO DATER-UNITPER-CD
              MOVE LN-UNITPER-FREQ TO DATER-UNITPER-FREQ
              PERFORM INCREMENT-UNITPER
              MOVE NDTE-DATE TO MDTE-DATE.

           IF MATURED = "S"
              IF MDTE-DATE <= HOLD-TODAY-DATE
                 GO TO NEXT-LN.

           IF MATURED = "O"
              IF MDTE-DATE >= HOLD-TODAY-DATE
                 GO TO NEXT-LN.

       FILL-WORK-FLDS.

           MOVE " " TO WK-KEY.
           MOVE LN-OWNBR TO WK-BRNO.
           IF COLLECTOR-FG = "Y"
              MOVE LN-COLLECTOR TO WK-COLCTID
           ELSE
              MOVE SPACES TO WK-COLCTID.

           IF ACTION-FG = "Y"
              MOVE LN-ACTIONCD TO WK-ACTION-CD
           ELSE
              MOVE SPACES TO WK-ACTION-CD.

           IF BRSEQ = "Y" OR "1"
              MOVE 0 TO WK-AGE WK-BALDUE
              MOVE SPACES TO WK-NAME.

           IF BRSEQ = "N" OR "2" OR " "
              SUBTRACT CONTRACTUAL FROM 99999 GIVING WK-AGE
              SUBTRACT WS-ACCUM-BAL FROM 9999999.99 GIVING WK-BALDUE
              MOVE SPACES TO WK-NAME.

      *  BORROWER INFORMATION RETREIVED IN STORED PROCEDURE
      *   BW-LNAME = SPACES IF BW REC NOT FOUND
           IF BRSEQ = "3" OR "6" OR "7"
              MOVE 0 TO WK-AGE WK-BALDUE
      *        MOVE LN-SSNO(1) TO BW-SSNO
      *        PERFORM READ-BW1-FILE
      *        IF IO-BAD
              IF BW-LNAME = SPACES
                 MOVE "ZZ" TO WK-NAME
              ELSE
                 STRING BW-LNAME "," BW-FNAME DELIMITED BY SIZE
                       INTO WK-NAME.

           IF BRSEQ = "7"
              MOVE LN-DLNO TO WK-BALDUE.

           IF BRSEQ = "4"
              PERFORM CHECK-RENEWABILITY
              IF RENEWABLE-FG = "Y"
                 MOVE 0 TO WK-AGE
              ELSE
                 MOVE 99999 TO WK-AGE
              END-IF
              PERFORM GET-AVAILABLE-CREDIT
              SUBTRACT AVCR-AVAILCREDIT FROM 9999999.99 GIVING WK-BALDUE
              MOVE SPACES TO WK-NAME.

           IF BRSEQ = "5"
              SUBTRACT RECENCY FROM 99999 GIVING WK-AGE
              SUBTRACT WS-ACCUM-BAL FROM 9999999.99 GIVING WK-BALDUE
              MOVE SPACES TO WK-NAME.

           IF BRSEQ = "6"
              SUBTRACT CONTRACTUAL FROM 99999 GIVING WK-AGE.

           MOVE LN-ACCTNO TO WK-ACCTNO.

           PERFORM COMPUTE-PAID-AHEAD.
           MOVE CSUB TO WK-CSUB.
           MOVE CPOT30 TO WK-CPOT30.
           MOVE RSUB TO WK-RDSUB.
           MOVE RDPOT30 TO WK-RDPOT30.
           MOVE PSUB TO WK-PSUB.
      *    ADD 1 TO CTFC-NM: AGEING RETURNS 0-30; WE NEED 1-31
           COMPUTE WK-CTFC-NM = CTFC-NM + 1.

           MOVE LN-REC TO WK-LN-REC.
           MOVE NO-REC TO WK-NO-REC.

           PERFORM WRITE-WK-FILE.
           IF IO-BAD
              GO TO IO-ERROR.

           GO TO NEXT-LN.

       PROCESS-WK-FILE.
           PERFORM CLOSE-LN3-FILE.
           IF PRU-LP NOT = "VDU" AND NOT = "PRU"
              IF EXT-ROUTE-BUF = "00"
                 MOVE BR-NO TO RUN-MESS-BR
                 MOVE RUN-DISPLAY-MESS TO MESS
                 PERFORM SEND-LEGEND.

      *     PERFORM CLOSE-LN1-FILE.
      *     PERFORM OPEN-LN1-FILE.
           PERFORM CLOSE-WK-FILE.
           PERFORM OPEN-WK-FILE.
           MOVE " " TO WK-KEY.
           MOVE 0 TO WK-BRNO WK-AGE WK-BALDUE WK-ACCTNO
                     WK-CSUB WK-RDSUB WK-PSUB.

           PERFORM START-WK-FILE.

       PROCESS-WK-FILE-PROCESS.
           PERFORM READ-WK-FILE-NEXT.
           IF IO-BAD
              IF FIRST-TIME NOT = "Y"
                 IF COLLECTOR-FG = "Y"
                    IF TOTALS-YES
                       PERFORM COLLECTOR-TOTALS
                    END-IF
                 END-IF
                 IF TOTALS-YES
                    PERFORM BRANCH-TOTALS
                 END-IF
                 MOVE WK-COLCTID TO OLD-COLCTID-CD
                 PERFORM SET-COLLECTOR-HEADINGS
                 PERFORM SET-BRANCH-HEADINGS
                 ADD 1 EXT-CONAME-RPT-LINES GIVING LN-COUNT
              END-IF
              GO TO END-OF-GROUP.

           UNLOCK WK-FILE.

           IF FIRST-TIME = "Y"
              MOVE WK-COLCTID TO OLD-COLCTID-CD
              PERFORM SET-COLLECTOR-HEADINGS
              PERFORM SET-BRANCH-HEADINGS
              ADD 1 EXT-CONAME-RPT-LINES GIVING LN-COUNT.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      * TEST-COLLECTOR-BREAK.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
              IF WK-COLCTID NOT = OLD-COLCTID-CD
                 IF TOTALS-YES
                    PERFORM COLLECTOR-TOTALS
                 END-IF
                 MOVE WK-COLCTID TO OLD-COLCTID-CD
                 PERFORM SET-COLLECTOR-HEADINGS
                 ADD 1 EXT-CONAME-RPT-LINES GIVING LN-COUNT.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      * TEST FOR BREAK IN ACTION CODE:
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

           IF ACTION-FG = "Y"
              IF WK-ACTION-CD NOT = OLD-ACTION-CD
                 MOVE WK-ACTION-CD TO OLD-ACTION-CD
                 ADD 1 EXT-CONAME-RPT-LINES GIVING LN-COUNT.

      * LN-REC NOW HELD IN WORK FILE (08/2023)
      *     MOVE WK-BRNO   TO LN-OWNBR.
      *     MOVE WK-ACCTNO TO LN-ACCTNO.

      *     PERFORM READ-LN1-FILE.
      *     IF IO-BAD
      *        GO TO PROCESS-WK-FILE-PROCESS.

           MOVE WK-LN-REC TO LN-REC.
           MOVE WK-NO-REC TO NO-REC.

           PERFORM GET-SP.
           IF IO-FG NOT = 0
              GO TO PROCESS-WK-FILE-PROCESS.

      *    MOVE LN-OWNBR  TO NO-BRNO.
      *    MOVE LN-ACCTNO TO NO-ACCTNO.
      *    PERFORM READ-NO1-FILE.
      *    IF IO-FG NOT = 0

           IF NO-ACCTNO = 0
              MOVE LN-MAKERCD(1)   TO CRNO-MAKERCD
              MOVE HOLD-TODAY-DATE TO CRNO-PAYDATE
              MOVE "N"             TO CRNO-LPREC-FG
              PERFORM CREATE-NOREC.

      * NEED TO SET CONTRACTUAL FIELDS AGAIN
           MOVE NO-CPOT30      TO CPOT30.
           MOVE NO-RDPOT30     TO RDPOT30.
           MOVE NO-CPOTCHGOFF  TO CPOTCHGOFF.
           MOVE NO-RDPOTCHGOFF TO RDPOTCHGOFF.
           MOVE NO-CSUB        TO CSUB.
           MOVE NO-RSUB        TO RSUB.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      * SINGLE LINE AND SUMMARY WILL BOTH PERFORM THE SAME BUT
      * IF SUMMARY IT WONT PERFORM WRITE-DETAIL
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

           IF REPORT-TYPE = "L" OR "S"
              PERFORM SINGLE-LINE-DETAIL-PROCESS
           ELSE
              PERFORM DETAIL-LINE-PROCESS.

           MOVE "N" TO FIRST-TIME.
           MOVE "Y" TO NEW-BRANCH.
           MOVE "Y" TO RECORDS-FOUND.

           GO TO PROCESS-WK-FILE-PROCESS.

       END-OF-GROUP.
           PERFORM CLOSE-NO1-FILE.
           PERFORM CLOSE-BW1-FILE.
           PERFORM CLOSE-LN3-FILE.
           PERFORM CLOSE-LP1-FILE.

           IF ( LOCAL-PRINT-FG = "Y" )
              MOVE 0 TO PAGE-NUMBER
              PERFORM CLOSE-PR-FILE.

           MOVE "Y" TO FIRST-TIME.

           MOVE 1 TO LEV.
           PERFORM ZERO-TOTAL-TBL.
           MOVE 3 TO LEV.
           PERFORM ZERO-TOTAL-TBL.

           GO TO NEXT-GROUP-LOC.

       END-ROUTINE.

           IF NO-RECORDS-FOUND
              GO TO END-PROGRAM.

           IF TOTALS-YES
              IF WK-COLCTID NOT = OLD-COLCTID-CD
                 PERFORM COLLECTOR-TOTALS.

           IF ( LOCAL-PRINT-FG = "N" )
              IF ( BEG-BRANCH NOT = END-BRANCH )
                         OR ( EXT-EOBUF-GROUP-FG = "Y" )
                 IF TOTALS-YES
                    PERFORM GRAND-TOTALS
                 END-IF
              END-IF
              IF RANGES-YES
                 PERFORM HEAD-PAGE
                 PERFORM PRINT-RANGE-LINES.

           GO TO END-PROGRAM.

      ******************************************************************
       INITIALIZATION SECTION.
      ******************************************************************
           MOVE CPCORP-QUEUE-POS TO Q-POS.
           MOVE CPCORP-COPIES-POS TO C-POS.
           MOVE CPCORP-PRU-POS   TO P-POS.
           MOVE SPACES TO Q-BUF C-BUF P-BUF.

       COPY "LIBGB/EOINIT.CPY".

           PERFORM GET-TIME.
           MOVE TIME-EDIT TO STARTING-TIME.

           PERFORM OPEN-LN3-FILE.

           MOVE AGE-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-ALPDATE.
           MOVE ALP-DATE           TO H-AGEDATE.
           MOVE EXT-CONAME-CONAME  TO H-NAME.
           MOVE EXT-CONAME-SYSDATE TO H-DATE.
           MOVE EXT-CONAME-USER-ID TO H-USERID.

           IF EXT-ROUTE-BUF = "70"
              MOVE EXT-EOBUF-MESS TO H-TITLE.

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

           MOVE AGE-DATE TO HOLD-TODAY-DATE AGEING-DATE.
           MOVE " " TO EXT-EOBUF-ERRCD.
           MOVE BEG-PAST TO BEG-PAST-PROM.

           IF BEG-SIGN = "-"
              MULTIPLY -1 BY BEG-PAST-PROM.

           MOVE END-PAST TO END-PAST-PROM.

           IF END-SIGN = "-"
              MULTIPLY -1 BY END-PAST-PROM.

           MOVE "  CASH"    TO HEAD8-PROMPT.
           MOVE "AVAILABLE" TO HEAD9-PROMPT.

           MOVE "N" TO TOTAL-FG.
           MOVE 1 TO LEV.
           PERFORM ZERO-TOTAL-TBL.
           MOVE 2 TO LEV.
           PERFORM ZERO-TOTAL-TBL.
           PERFORM ZERO-TT-TOTALS.
           MOVE 3 TO LEV.
           PERFORM ZERO-TOTAL-TBL.

      * INITIALIZE ACCUMULATED PORTFOLIO TOTALS
           MOVE 0 TO ACCUM-SF-AMT ACCUM-SF-NO
                     ACCUM-DL-AMT ACCUM-DL-NO.

           PERFORM OPEN-BR-FILE.
           PERFORM OPEN-GR1-FILE.
           PERFORM OPEN-SP1-FILE.
           PERFORM OPEN-LC1-FILE.

           IF EXT-ROUTE-BUF = "00"
              MOVE "REPORT IN PROGRESS...................." TO MESS
              MOVE MESS-LIST TO WR-LIST
              MOVE MESS TO WR-BUF
              PERFORM CALL-WRFORM.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      * NEED TO USE MKTEMP HERE FIRST BECAUSE WE NEED THE WORK
      * FILE PATH IN IT LAST FOR THE REMOVE
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

           IF ( LOCAL-PRINT-FG NOT = "Y" )
              PERFORM OPEN-PR-FILE.

           MOVE MKTEMP-DEFAULT TO MKTEMP-BUF.
           PERFORM MKTEMP-CALL.
           PERFORM OPEN-WK-FILE-OUTPUT.
           PERFORM CLOSE-WK-FILE.

           PERFORM ARRAYSP-LOAD.

           GO TO EXIT-INITIALIZE.

       ENDIT.
           MOVE "X" TO EXT-EOBUF-ERRCD.

       EXIT-INITIALIZE.
           EXIT.

      ******************************************************************
       EO-EXEC-AUDIT SECTION.
      ******************************************************************
           MOVE EXT-EOBUF-DATE2 TO AGE-DATE.

           IF BEG-ALPHA = "    "
              IF END-ALPHA = "    "
      * LOWER "ZZZZ"
                 MOVE X'7A7A7A7A' TO END-ALPHA.

           IF DEL-FLAG NOT = "Y"
              GO TO EXIT-EO-EXEC-AUDIT.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      * SETUP NEXT MONTH DATE (WORLD ONLY, FOR CORRECT EOM AGEING):
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE EXT-EOBUF-DATE2 TO NDTE-DATE.
           MOVE 1 TO NDTE-HOLD.
           MOVE 1 TO NDTE-DD.
           PERFORM INCREMENT-MONTHS.
           MOVE NDTE-DATE TO AGE-DATE.

       EXIT-EO-EXEC-AUDIT.
           EXIT.

      ******************************************************************
       ENTER-PARAMETERS SECTION.
      ******************************************************************
       ENTER-PARM.

      * DONT GO FURTHER IF NOT A VALID LOGIN
      * NEED USERS BRANCH FOR BR-FILE

           MOVE EXT-CONAME-USER-ID TO USERID-WK.
           IF USERID-BRANCH NOT NUMERIC
              MOVE "INVALID USER" TO MESS
              PERFORM SEND-MESS
              GO TO ENDIT.

           IF VDUSTATUS = "1U"
              GO TO SUMYN.

           IF EXT-ROUTE-BUF = "00"
              MOVE USERID-BRANCH TO BR-NO
              PERFORM OPEN-BR-FILE
              PERFORM READ-BR-FILE
              PERFORM CLOSE-BR-FILE
              IF IO-BAD
                 GO TO IO-ERROR
              END-IF
              MOVE BR-CPCORP-ORDER TO BRSEQ
              MOVE "S  A010PRSEQ." TO VDU
              MOVE BRSEQ TO VDUBUF
              PERFORM SCREENIO.

      *----------------------------------------------------------------
       AGEDT.

           IF EXT-ROUTE-BUF = "40" OR "50"
              GO TO ENTER-DEL-FLAG.

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
              MOVE DATE-YYYYMMDD TO AGE-DATE VDU-DATE-YYYYMMDD
              PERFORM SCREENIO.

      *----------------------------------------------------------------
       ENTER-DEL-FLAG.
           MOVE "S  A160DELPROMPT:P." TO VDU.
           MOVE "BEG MO DELINQ?: "    TO VDUBUF.
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
           IF VDUSTATUS = "1U"
              IF NOT (EXT-ROUTE-BUF = "40" OR "50")
                 GO TO AGEDT.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO ENTER-DEL-FLAG.

           MOVE VDUBUF TO DEL-FLAG.

           IF DEL-FLAG NOT = "Y" AND NOT = "N"
              GO TO ENTER-DEL-FLAG.

           IF DEL-FLAG = "N"
              GO TO BR-01.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      * SETUP NEXT MONTH DATE:
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE AGE-DATE TO NDTE-DATE.
           MOVE 1        TO NDTE-DD.
           MOVE 1        TO NDTE-HOLD.
           PERFORM INCREMENT-MONTHS.
           MOVE NDTE-DATE TO AGE-DATE.

      *----------------------------------------------------------------
       BR-01.
           MOVE "E RA040BR1." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "16" GO TO ALL-BR.
           IF VDUSTATUS = "1U" GO TO AGEDT.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.

           IF VDUSTATUS = "1<"
              MOVE "WI/GRSCAN" TO FORM-NAM
              CALL FORM-PROGX USING FORM-PATH GRSCAN-BUF EXIT-PATHNAME
              CANCEL FORM-PROGX
              IF GRSCAN-STATUS NOT = "00"
                 GO TO BR-01
              ELSE
                 MOVE "S NA000BR1." TO VDU
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
              MOVE "S  A160DISPLAY-USER." TO VDU
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
           MOVE "ENNN040BR2." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO BR-01.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO BR-02.
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

           MOVE "SNNN040BR2." TO VDU.
           MOVE 9999 TO END-BRANCH VDUNUM0.
           PERFORM SCREENIO.

           MOVE "S NA160DISPLAY-USER." TO VDU.
           MOVE SPACES TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "N" TO GROUP-FLAG.

      *----------------------------------------------------------------
       CHECK-BRANCH-SECURITY.
           PERFORM BR-RANGE-SECURITY.

           IF BR-SECURITY-OKAY = "N"
              GO TO BR-01.

      *----------------------------------------------------------------
       COL-CD.
           MOVE "E  A030COLCTID." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "10" GO TO BR-01.
           IF VDUSTATUS = "16" GO TO ALLCOL.
           IF VDUSTATUS = "1U" GO TO BR-01.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO COL-CD.

           MOVE VDUBUF TO COLCTIDX.
           GO TO ACT-CD.

      *----------------------------------------------------------------
       ALLCOL.
           MOVE "S  A030COLCTID." TO VDU.
           MOVE "???" TO COLCTIDX VDUBUF.
           PERFORM SCREENIO.

      *----------------------------------------------------------------
       ACT-CD.
           MOVE "E  A040ACTCD." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "10" GO TO BR-01.
           IF VDUSTATUS = "16" GO TO ALL-AC.
           IF VDUSTATUS = "1U" GO TO COL-CD.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO ACT-CD.

           MOVE VDUBUF TO ACTIONX.
           GO TO CL-01.

      *----------------------------------------------------------------
       ALL-AC.
           MOVE "S  A040ACTCD." TO VDU.
           MOVE "????" TO ACTIONX VDUBUF.
           PERFORM SCREENIO.

      *----------------------------------------------------------------
       CL-01.
      *    MOVE "ENNN020CL1." TO VDU.
           MOVE SPACES TO VDUBUF.
           MOVE "S  A020LCX." TO VDU.
           PERFORM SCREENIO.
           MOVE "E  A020CL1." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "10" GO TO BR-01.
           IF VDUSTATUS = "16" GO TO ALL-CL.
           IF VDUSTATUS = "1U" GO TO ACT-CD.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "1<"
              MOVE "BEG" TO CLASS-FG
              PERFORM CLASS-SCAN
              IF IO-BAD
              GO TO CL-01.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO CL-01.

      *    MOVE VDUNUM0 TO BEG-CLASS.
           MOVE VDUBUF TO BEG-CLASSX.
      * THIS WILL ONLY HAPPEN IF THEY ENTER 01 OR 02 ETC...
           IF BEG-CLASSX NUMERIC
              GO TO CL-02.

           MOVE "S  A020LCX." TO VDU.
           MOVE SPACES TO VDUBUF.
           IF BEG-CLASS1 = "S"
              MOVE "SALES" TO VDUBUF
           ELSE
           IF BEG-CLASS1 = "L"
              MOVE "DIRCT" TO VDUBUF
           ELSE
           IF BEG-CLASS1 = "M"
              MOVE "SMALL" TO VDUBUF
           ELSE
           IF BEG-CLASS1 = "W"
              MOVE "WCBC " TO VDUBUF
           ELSE
           IF BEG-CLASS1 = "R"
              MOVE "REAL" TO VDUBUF
           ELSE
              IF BEG-CLASS1 NOT NUMERIC
                 MOVE "ENTER S, L, M, W, OR R ONLY" TO MESS
                 PERFORM SEND-MESS
                 GO TO CL-01.
           PERFORM SCREENIO.

           IF BEG-CLASS1 NUMERIC
              MOVE BEG-CLASS1 TO BEG-CLASS2
              MOVE 0 TO BEG-CLASS1
              MOVE "SN N020CL1." TO VDU
              MOVE BEG-CLASS TO VDUNUM0
              PERFORM SCREENIO
              GO TO CL-02.

           MOVE 99 TO END-CLASS.
           GO TO DL-01.


      *----------------------------------------------------------------
       CL-02.
           MOVE "ENNN020CL2." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "10" GO TO BR-01.
           IF VDUSTATUS = "1U" GO TO CL-01.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "1<"
              MOVE "END" TO CLASS-FG
              PERFORM CLASS-SCAN
              IF IO-BAD
                 GO TO CL-02.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO CL-02.

           MOVE VDUNUM0 TO END-CLASS.

           IF BEG-CLASS > END-CLASS
              PERFORM INVALID-RANGE
              GO TO CL-01.

           GO TO DL-01.

      *----------------------------------------------------------------
       ALL-CL.
           MOVE "SNNN020CL1." TO VDU.
           MOVE 0 TO BEG-CLASS VDUNUM0.
           PERFORM SCREENIO.

           MOVE "SNNN020CL2." TO VDU.
           MOVE 99 TO END-CLASS VDUNUM0.
           PERFORM SCREENIO.

      *----------------------------------------------------------------
       DL-01.
           MOVE "ENNN042DL1." TO VDU
           PERFORM SCREENIO.

           IF VDUSTATUS = "10" GO TO BR-01.
           IF VDUSTATUS = "16" GO TO ALL-DL.
           IF VDUSTATUS = "1U" GO TO CL-02.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "1<"
              MOVE "DLBEG" TO DEALER-FG
              PERFORM DEALER-SCAN
              IF IO-BAD
                 GO TO DL-01.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO DL-01.

           MOVE VDUNUM2 TO BEG-DEALER.

      *----------------------------------------------------------------
       DL-02.
           MOVE "ENNN042DL2." TO VDU
           PERFORM SCREENIO.

           IF VDUSTATUS = "10" GO TO BR-01.
           IF VDUSTATUS = "1U" GO TO DL-01.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "1<"
              MOVE "DLEND" TO DEALER-FG
              PERFORM DEALER-SCAN
              IF IO-BAD
                 GO TO DL-02.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO DL-02.

           MOVE VDUNUM2 TO END-DEALER.

           IF BEG-DEALER > END-DEALER
              PERFORM INVALID-RANGE
              GO TO DL-01.

           GO TO ALPHA-01.

      *----------------------------------------------------------------
       ALL-DL.
           MOVE "SNNN042DL1." TO VDU
           MOVE 0 TO BEG-DEALER VDUNUM2
           PERFORM SCREENIO
           MOVE "SNNN042DL2." TO VDU.
           MOVE 9999.99 TO END-DEALER VDUNUM2.
           PERFORM SCREENIO.

      *----------------------------------------------------------------
       ALPHA-01.
           MOVE "E  A040ALP1." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO BR-01.
           IF VDUSTATUS = "16" GO TO ALL-ALPHA.
           IF VDUSTATUS = "1U" GO TO DL-02.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO ALPHA-01.
           MOVE VDUBUF TO BEG-ALPHA.
       ALPHA-02.
           MOVE "E  A040ALP2." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO BR-01.
           IF VDUSTATUS = "1U" GO TO ALPHA-01.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO ALPHA-02.
           MOVE VDUBUF TO END-ALPHA.
           IF BEG-ALPHA > END-ALPHA
              MOVE "INVALID ALPHA RANGE" TO MESS
              PERFORM SEND-MESS
              GO TO ALPHA-01.
           GO TO AGE-01.

       ALL-ALPHA.
           MOVE "S  A000ALP1." TO VDU.
           MOVE "   " TO BEG-ALPHA VDUBUF.
           PERFORM SCREENIO.
           MOVE "S  A000ALP2." TO VDU.
      * LOWER "ZZZZ"
           MOVE X'7A7A7A7A' TO END-ALPHA VDUBUF.
           PERFORM SCREENIO.

      *----------------------------------------------------------------
       AGE-01.
           MOVE "ENNN040AGE1." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "10" GO TO BR-01.
           IF VDUSTATUS = "16" GO TO ALLAGE.
           IF VDUSTATUS = "1U" GO TO ALPHA-02.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO AGE-01.

           MOVE VDUNUM0 TO BEG-AGE.

      *----------------------------------------------------------------
       AGE-02.
           MOVE "ENNN040AGE2." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "10" GO TO BR-01.
           IF VDUSTATUS = "1U" GO TO AGE-01.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO AGE-02.

           MOVE VDUNUM0 TO END-AGE.

           IF BEG-AGE > END-AGE
              PERFORM INVALID-RANGE
              GO TO AGE-01.

           GO TO PAST-DUE-AMT.

      *----------------------------------------------------------------
       ALLAGE.
           MOVE "SNNN040AGE1." TO VDU.
           MOVE 0 TO BEG-AGE VDUNUM0.
           PERFORM SCREENIO.

           MOVE "SNNN040AGE2." TO VDU.
           MOVE 9999 TO END-AGE VDUNUM0.
           PERFORM SCREENIO.

      *----------------------------------------------------------------
       PAST-DUE-AMT.
           MOVE "ENNN032PASTDUE." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "10" GO TO BR-01.
           IF VDUSTATUS = "1U" GO TO AGE-02.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO PAST-DUE-AMT.

           MOVE VDUNUM2 TO PAST-D-AMT.

      *----------------------------------------------------------------
       AMT-01.
           MOVE "ENNN060LA1." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "10" GO TO BR-01.
           IF VDUSTATUS = "16" GO TO ALLLA.
           IF VDUSTATUS = "1U" GO TO PAST-DUE-AMT.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO AMT-01.

           MOVE VDUNUM0 TO BEG-LNAMT.

      *----------------------------------------------------------------
       AMT-02.
           MOVE "ENNN060LA2." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "10" GO TO BR-01.
           IF VDUSTATUS = "1U" GO TO AMT-01.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO AMT-02.

           MOVE VDUNUM0 TO END-LNAMT.

           IF BEG-LNAMT > END-LNAMT
              PERFORM INVALID-RANGE
              GO TO AMT-01.

           GO TO CAGE01.

      *----------------------------------------------------------------
       ALLLA.
           MOVE "SNNN060LA1." TO VDU.
           MOVE 0 TO BEG-LNAMT VDUNUM0.
           PERFORM SCREENIO.

           MOVE "SNNN060LA2." TO VDU.
           MOVE 999999 TO END-LNAMT VDUNUM0.
           PERFORM SCREENIO.

      *----------------------------------------------------------------
       CAGE01.
           MOVE "ENNN040CAGE1." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "10" GO TO BR-01.
           IF VDUSTATUS = "16" GO TO ALCAGE.
           IF VDUSTATUS = "1U" GO TO AMT-02.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO CAGE01.

           MOVE VDUNUM0 TO BEG-CAGE.

      *----------------------------------------------------------------
       CAGE02.
           MOVE "ENNN040CAGE2." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "10" GO TO BR-01.
           IF VDUSTATUS = "1U" GO TO CAGE01.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO CAGE02.

           MOVE VDUNUM0 TO END-CAGE.

           IF BEG-CAGE > END-CAGE
              PERFORM INVALID-RANGE
              GO TO CAGE01.

           IF NOT ( BEG-CAGE < 30)
               MOVE "SNNA010C-POT." TO VDU
               MOVE " " TO VDUBUF CPOT
               PERFORM SCREENIO
               MOVE "SNNA010C-MISS." TO VDU
               MOVE " " TO VDUBUF CMISS
               PERFORM SCREENIO
               GO TO RAGE01.

           GO TO CPOTYN.

      *----------------------------------------------------------------
       ALCAGE.
           MOVE "SNNN040CAGE1." TO VDU.
           MOVE 0 TO BEG-CAGE VDUNUM0.
           PERFORM SCREENIO.

           MOVE "SNNN040CAGE2." TO VDU.
           MOVE 9999 TO END-CAGE VDUNUM0.
           PERFORM SCREENIO.

      *----------------------------------------------------------------
       CPOTYN.
           MOVE "E  A010C-POT." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO CAGE02.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO CPOTYN.

           MOVE VDUBUF TO CPOT.
      * MJD 122397 IF WE SKIP POTS WE STILL WANT TO ALLOW SELECTION OF MISS
      *            PAYS.
      *--->IF CPOT = "S"
      *          MOVE "SNNA010C-MISS." TO VDU
      *          MOVE "S" TO VDUBUF
      *          PERFORM SCREENIO
      *--------->GO TO RAGE01.
           IF CPOT = "O"
                 MOVE "SNNA010C-MISS." TO VDU
                 MOVE "S" TO VDUBUF CMISS
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

           IF VDUSTATUS = "10" GO TO BR-01.
           IF VDUSTATUS = "16" GO TO ALRAGE.
           IF VDUSTATUS = "1U"
                IF NOT (BEG-CAGE < 30)
                   GO TO CAGE02
                ELSE
                IF CPOT = "O"
                    GO TO CPOTYN
                ELSE
      *-------->IF CPOT = "S"
      *             GO TO CPOTYN
      *-------->ELSE
                    GO TO CMISSYN.

           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO RAGE01.

           MOVE VDUNUM0 TO BEG-RAGE.

      *----------------------------------------------------------------
       RAGE02.
           MOVE "ENNN040RAGE2." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "10" GO TO BR-01.
           IF VDUSTATUS = "1U" GO TO RAGE01.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO RAGE02.

           MOVE VDUNUM0 TO END-RAGE.

           IF BEG-RAGE > END-RAGE
              PERFORM INVALID-RANGE
              GO TO RAGE01.

           IF NOT (BEG-RAGE < 30)
               MOVE "SNNA010R-POT." TO VDU
               MOVE " " TO VDUBUF
               PERFORM SCREENIO
               GO TO PAST01.

           GO TO RPOTYN.

      *----------------------------------------------------------------
       ALRAGE.
           MOVE "SNNN040RAGE1." TO VDU.
           MOVE 0 TO BEG-RAGE VDUNUM0.
           PERFORM SCREENIO.

           MOVE "SNNN040RAGE2." TO VDU.
           MOVE 9999 TO END-RAGE VDUNUM0.
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
       MINAVAIL.
           MOVE "ENNN060AVAILCR." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO RPOTYN.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO MINAVAIL.

           MOVE VDUNUM0 TO MIN-AVAILCR.
      *----------------------------------------------------------------
       PAST01.
           MOVE "ENNN030PAST1." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "16" GO TO ALPAST.
           IF VDUSTATUS = "1U" GO TO MINAVAIL.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO PAST01.

           MOVE VDUNUM0 TO BEG-PAST.

      *----------------------------------------------------------------
       SIGN01.
           MOVE "E  A010SIGN1." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "1U" GO TO PAST01.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO SIGN01.

           MOVE VDUBUF TO BEG-SIGN.

      *----------------------------------------------------------------
       PAST02.
           MOVE "ENNN030PAST2." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO PAST01.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO PAST02.

           MOVE VDUNUM0 TO END-PAST.

      *----------------------------------------------------------------
       SIGN02.
           MOVE "E  A010SIGN2." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "1U" GO TO PAST02.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO SIGN02.

           MOVE VDUBUF TO END-SIGN.
           MOVE BEG-PAST TO BEG-PAST-PROM.

           IF BEG-SIGN = "-"
              MULTIPLY -1 BY BEG-PAST-PROM.

           MOVE END-PAST TO END-PAST-PROM.

           IF END-SIGN = "-"
              MULTIPLY -1 BY END-PAST-PROM.

           IF BEG-PAST-PROM > END-PAST-PROM
              MOVE "INVALID PAST PROMISE RANGE !" TO MESS
              PERFORM SEND-MESS
              GO TO PAST01.

           GO TO BAL01.

      *----------------------------------------------------------------
       ALPAST.
           MOVE "SNNN030PAST1." TO VDU.
           MOVE 999 TO BEG-PAST VDUNUM0.
           PERFORM SCREENIO.

           MOVE "S  A010SIGN1." TO VDU.
           MOVE "-" TO BEG-SIGN VDUBUF.
           PERFORM SCREENIO.

           MOVE "SNNN030PAST2." TO VDU.
           MOVE 999 TO END-PAST VDUNUM0.
           PERFORM SCREENIO.

           MOVE "S  A010SIGN2." TO VDU.
           MOVE "+" TO END-SIGN VDUBUF.
           PERFORM SCREENIO.

      *----------------------------------------------------------------
       BAL01.
           MOVE "ENNN060BAL1." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "16" GO TO ALBAL.
           IF VDUSTATUS = "1U" GO TO PAST02.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO BAL01.

           MOVE VDUNUM0 TO BEG-BAL.

      *----------------------------------------------------------------
       BAL02.
           MOVE "ENNN060BAL2." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO BAL01.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO BAL02.

           MOVE VDUNUM0 TO END-BAL.

           IF BEG-BAL > END-BAL
              PERFORM INVALID-RANGE
              GO TO BAL01.

           GO TO TYPEYN.

      *----------------------------------------------------------------
       ALBAL.
           MOVE "SNNN060BAL1." TO VDU.
           MOVE 0 TO BEG-BAL VDUNUM0.
           PERFORM SCREENIO.

           MOVE "SNNN060BAL2." TO VDU.
           MOVE 999999 TO END-BAL VDUNUM0.
           PERFORM SCREENIO.

      *----------------------------------------------------------------
       TYPEYN.
           MOVE "E  A010TYPE." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "10" GO TO BR-01.
           IF VDUSTATUS = "1U" GO TO BAL02.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO TYPEYN.

           MOVE VDUBUF TO OWNED.

           IF OWNED = "O" GO TO RECRSE.
           IF OWNED = "S" GO TO RECRSE.
           IF OWNED NOT = "B" GO TO TYPEYN.

      *----------------------------------------------------------------
       RECRSE.
           MOVE "E  A010RC." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "10" GO TO BR-01.
           IF VDUSTATUS = "1U" GO TO TYPEYN.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO RECRSE.

           MOVE VDUBUF TO RC.

           IF RC = "F" GO TO PLYN.
           IF RC = "P" GO TO PLYN.
           IF RC = "N" GO TO PLYN.
           IF RC NOT = "A" GO TO RECRSE.

      *----------------------------------------------------------------
       PLYN.
           MOVE "E  A010PL." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "10" GO TO BR-01.
           IF VDUSTATUS = "1U" GO TO RECRSE.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO PLYN.

           MOVE VDUBUF TO PL.

           IF PL = "I" OR "O"
              GO TO PLTYPEYN.

           IF PL NOT = "S"
              GO TO PLYN.

           IF PL NOT = "S" GO TO ACTVYN.

           MOVE "B" TO PLACTIVE.
           GO TO REPOYN.

      *----------------------------------------------------------------
       PLTYPEYN.
           MOVE "E  A010PLTYPE." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "10" GO TO BR-01.
           IF VDUSTATUS = "1U" GO TO PLYN.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO PLTYPEYN.

           MOVE VDUBUF TO PLTYPE.

           IF NOT (PLTYPE = "A" OR "P" OR "2" OR "3" OR "4" OR "5")
              GO TO PLTYPEYN.

      *----------------------------------------------------------------
       ACTVYN.
           MOVE "E  A010ACTIVE." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "10" GO TO BR-01.
           IF VDUSTATUS = "1U" GO TO PLTYPEYN.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO ACTVYN.

           MOVE VDUBUF TO PLACTIVE.

           IF PLACTIVE NOT = "I" AND NOT = "A" AND NOT = "B"
              GO TO ACTVYN.

      *----------------------------------------------------------------
       REPOYN.
           MOVE "E  A010RP." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO ACTVYN.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO REPOYN.

           MOVE VDUBUF TO REPO.

           IF REPO NOT = "I" AND NOT = "S" AND NOT = "O"
              GO TO REPOYN.

      *----------------------------------------------------------------
       BANKYN.
           MOVE "E  A010BK." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO REPOYN.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO BANKYN.

           MOVE VDUBUF TO BANK.

           IF BANK NOT = "I" AND NOT = "S" AND NOT = "O"
              GO TO BANKYN.

      *----------------------------------------------------------------
       LEGLYN.
           MOVE "E  A010LG." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO BANKYN.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO LEGLYN.

           MOVE VDUBUF TO LEGL.

           IF LEGL NOT = "I" AND NOT = "S" AND NOT = "O"
              GO TO LEGLYN.

      *----------------------------------------------------------------
       DEBTYN.
           MOVE "E  A010DA." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO LEGLYN.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO DEBTYN.

           MOVE VDUBUF TO DEBT.

           IF DEBT NOT = "I" AND NOT = "S" AND NOT = "O"
              GO TO DEBTYN.

      *----------------------------------------------------------------
       JDYN.
           MOVE "E  A010JD." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "10" GO TO BR-01.
           IF VDUSTATUS = "1U" GO TO DEBTYN.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO JDYN.

           MOVE VDUBUF TO JD.

           IF JD NOT = "I" AND NOT = "S" AND NOT = "O"
              GO TO JDYN.

      *----------------------------------------------------------------
       MATYN.
           MOVE "E  A010MAT." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "10" GO TO BR-01.
           IF VDUSTATUS = "1U" GO TO JDYN.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO MATYN.

           MOVE VDUBUF TO MATURED.

           IF MATURED NOT = "I" AND NOT = "S" AND NOT = "O"
              GO TO MATYN.

      *----------------------------------------------------------------
       PROMYN.
           MOVE "E  A010PROM." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "10" GO TO BR-01.
           IF VDUSTATUS = "1U" GO TO MATYN.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO PROMYN.

           MOVE VDUBUF TO PROMISED.

           IF PROMISED NOT = "I" AND NOT = "S" AND NOT = "O"
              GO TO PROMYN.

      *----------------------------------------------------------------
       ZBALYN.
           MOVE "E  A010ZBAL." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "10" GO TO BR-01.
           IF VDUSTATUS = "1U" GO TO PROMYN.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO ZBALYN.

           MOVE VDUBUF TO ZBAL.

           IF ZBAL NOT = "I" AND NOT = "S" AND NOT = "O"
              GO TO ZBALYN.

      *----------------------------------------------------------------
       1STPYYN.
           MOVE "E  A0101STPY." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "10" GO TO BR-01.
           IF VDUSTATUS = "1U" GO TO ZBALYN.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO 1STPYYN.

           MOVE VDUBUF TO NO1STPY.

           IF NO1STPY NOT = "I" AND NOT = "S" AND NOT = "O"
              GO TO 1STPYYN.

      *----------------------------------------------------------------
       PRINTSEQ.
           MOVE "E  A010PRSEQ." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "10" GO TO BR-01.
           IF VDUSTATUS = "1U" GO TO 1STPYYN.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO PRINTSEQ.

           IF VDUBUF NOT = "1" AND
              VDUBUF NOT = "2" AND
              VDUBUF NOT = "3" AND
              VDUBUF NOT = "4" AND
              VDUBUF NOT = "5" AND
              VDUBUF NOT = "6" AND
              VDUBUF NOT = "7"
                 MOVE "ENTER A VALUE FROM 1 TO 7 " TO MESS
                 PERFORM SEND-MESS
                 GO TO PRINTSEQ
           ELSE
              MOVE VDUBUF TO BRSEQ
              GO TO PRINTCOL.

      *----------------------------------------------------------------
       PRINTOTH.
           MOVE "E  A010PROTH." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "10" GO TO BR-01.
           IF VDUSTATUS = "1U" GO TO PRINTSEQ.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO PRINTOTH.

           IF VDUBUF NOT = "Y" AND
              VDUBUF NOT = "N"
                 MOVE "ENTER Y OR N ONLY " TO MESS
                 PERFORM SEND-MESS
                 GO TO PRINTOTH
           ELSE
                 MOVE VDUBUF TO PROTH-FG.

      *----------------------------------------------------------------
       PRINTCOL.
           MOVE "E  A010PRCOL." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "10" GO TO BR-01.
           IF VDUSTATUS = "1U" GO TO PRINTOTH.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO PRINTCOL.

           IF VDUBUF NOT = "Y" AND
              VDUBUF NOT = "N"
                 MOVE "ENTER Y OR N ONLY " TO MESS
                 PERFORM SEND-MESS
                 GO TO PRINTCOL
           ELSE
                 MOVE VDUBUF TO PRCOL-FG.

      *----------------------------------------------------------------
       ENTER-FROZEN-ISO.
           MOVE "E  A010FROZEN." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "10" GO TO BR-01.
           IF VDUSTATUS = "1U" GO TO PRINTCOL.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D"
              GO TO ENTER-FROZEN-ISO.

           MOVE VDUBUF TO FROZEN-ISO.

           IF FROZEN-ISO NOT = "I" AND NOT = "S" AND NOT = "O"
              GO TO ENTER-FROZEN-ISO.

      *----------------------------------------------------------------
       ENTER-CONFIDENT-ISO.
           MOVE "E  A010CONFIDENT." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "10" GO TO BR-01.
           IF VDUSTATUS = "1U" GO TO ENTER-FROZEN-ISO.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D"
              GO TO ENTER-CONFIDENT-ISO.

           MOVE VDUBUF TO CONFIDENT-ISO.

           IF CONFIDENT-ISO NOT = "I" AND NOT = "S" AND NOT = "O"
              GO TO ENTER-CONFIDENT-ISO.

      *----------------------------------------------------------------
       COLLECTOR-YN.
           MOVE "E  A010COLL." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "10" GO TO BR-01.
           IF VDUSTATUS = "1U" GO TO ENTER-CONFIDENT-ISO.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO COLLECTOR-YN.

           IF VDUBUF NOT = "Y" AND
              VDUBUF NOT = "N"
                 MOVE "ENTER Y OR N ONLY " TO MESS
                 PERFORM SEND-MESS
                 GO TO COLLECTOR-YN
           ELSE
                 MOVE VDUBUF TO COLLECTOR-FG.
      *----------------------------------------------------------------
       ACTION-YN.
           MOVE "E  A010ACT." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "10" GO TO BR-01.
           IF VDUSTATUS = "1U" GO TO COLLECTOR-YN.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO ACTION-YN.

           IF VDUBUF NOT = "Y" AND
              VDUBUF NOT = "N"
                 MOVE "ENTER Y OR N ONLY " TO MESS
                 PERFORM SEND-MESS
                 GO TO ACTION-YN
           ELSE
                 MOVE VDUBUF TO ACTION-FG.
      *----------------------------------------------------------------
       TOTALS-YN.
           MOVE "E  A010TOT." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "10" GO TO BR-01.
           IF VDUSTATUS = "1U" GO TO ACTION-YN.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO TOTALS-YN.

           IF NOT (VDUBUF = "Y" OR "N" OR "T" OR "R" )
                 MOVE "ENTER Y, N, T OR R ONLY " TO MESS
                 PERFORM SEND-MESS
                 GO TO TOTALS-YN
           ELSE
                 MOVE VDUBUF TO TOTALS-FG.

      *----------------------------------------------------------------
       SUMYN.
           MOVE "E  A010SUMONLY." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "10" GO TO BR-01.
           IF VDUSTATUS = "1U" GO TO TOTALS-YN.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO SUMYN.

           MOVE VDUBUF TO REPORT-TYPE.

           IF REPORT-TYPE NOT = "D" AND
                           NOT = "B" AND
                            NOT = "L" AND
                             NOT = "S"
              GO TO SUMYN.

      * IF USERS CHOOSES TO SUPPRESS ALL TOTALS
      *    DO NOT ALLOW THEN TO SELECT SUMMARY ONLY.
           IF NOT TOTALS-YES
              IF REPORT-TYPE = "S"
                 GO TO SUMYN.
      *-----------------------------------------------------------------
       LOCAL-PRINT.
           IF EXT-ROUTE-BUF NOT = "40"
              IF EXT-ROUTE-BUF NOT = "50"
                 MOVE "N" TO LOCAL-PRINT-FG
                 GO TO EXIT-PARM.

           IF BEG-BRANCH = END-BRANCH
              MOVE "N" TO LOCAL-PRINT-FG
              GO TO EXIT-PARM.

      *-----------------------------------------------------------------
       ASK-LOCAL.
           MOVE "SPOOL TO LOCAL DIRECTORY? (Y/N)" TO MESS.
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
           IF VDUSTATUS = "1U" GO TO SUMYN.
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

           MOVE "S  A160DELPROMPT:P." TO VDU.
           MOVE "BEG MO DELINQ?: "    TO VDUBUF.
           PERFORM SCREENIO.
           MOVE "S  A010DELFLAG." TO VDU.
           MOVE DEL-FLAG TO VDUBUF.
           PERFORM SCREENIO.

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
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( (BEG-BRANCH = 0) AND (END-BRANCH = 0) )
              MOVE "S  A160DISPLAY-USER." TO VDU
              MOVE "USER BRANCH ONLY" TO VDUBUF
              PERFORM SCREENIO
           ELSE
              MOVE "S  A160DISPLAY-USER." TO VDU
              MOVE SPACES TO VDUBUF
              PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "S  A030COLCTID." TO VDU.
           MOVE COLCTIDX TO VDUBUF.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "S  A040ACTCD." TO VDU.
           MOVE ACTIONX TO VDUBUF.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "S  A020CL1." TO VDU.
           MOVE BEG-CLASSX TO VDUBUF.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "SNNN020CL2." TO VDU.
           MOVE END-CLASS TO VDUNUM0.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "SNNN042DL1." TO VDU
           MOVE BEG-DEALER TO VDUNUM2
           PERFORM SCREENIO
           MOVE "SNNN042DL2." TO VDU
           MOVE END-DEALER TO VDUNUM2
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "S  A000ALP1." TO VDU.
           MOVE BEG-ALPHA TO VDUBUF.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "S  A000ALP2." TO VDU.
           MOVE END-ALPHA TO VDUBUF.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "SNNN040AGE1." TO VDU.
           MOVE BEG-AGE TO VDUNUM0.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "SNNN040AGE2." TO VDU.
           MOVE END-AGE TO VDUNUM0.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "SNNN032PASTDUE." TO VDU.
           MOVE PAST-D-AMT TO VDUNUM2.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "SNNN060LA1." TO VDU.
           MOVE BEG-LNAMT TO VDUNUM0.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "SNNN060LA2." TO VDU.
           MOVE END-LNAMT TO VDUNUM0.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "SNNN040CAGE1." TO VDU.
           MOVE BEG-CAGE TO VDUNUM0.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "SNNN040CAGE2." TO VDU.
           MOVE END-CAGE TO VDUNUM0.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "SNNA010C-POT." TO VDU.
           MOVE CPOT TO VDUBUF.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "SNNA010C-MISS." TO VDU.
           MOVE CMISS TO VDUBUF.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "SNNN040RAGE1." TO VDU.
           MOVE BEG-RAGE TO VDUNUM0.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "SNNN040RAGE2." TO VDU.
           MOVE END-RAGE TO VDUNUM0.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "SNNA010R-POT." TO VDU.
           MOVE RPOT TO VDUBUF.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "SNNN060AVAILCR." TO VDU.
           MOVE MIN-AVAILCR TO VDUNUM0.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "SNNN030PAST1." TO VDU.
           MOVE BEG-PAST TO VDUNUM0.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "S  A010SIGN1." TO VDU.
           MOVE BEG-SIGN TO VDUBUF.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "SNNN030PAST2." TO VDU.
           MOVE END-PAST TO VDUNUM0.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "S  A010SIGN2." TO VDU.
           MOVE END-SIGN TO VDUBUF.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "SNNN060BAL1." TO VDU.
           MOVE BEG-BAL TO VDUNUM0.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "SNNN060BAL2." TO VDU.
           MOVE END-BAL TO VDUNUM0.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "S  A010TYPE." TO VDU.
           MOVE OWNED TO VDUBUF.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "S  A010RC." TO VDU.
           MOVE RC TO VDUBUF.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "S  A010PL." TO VDU.
           MOVE PL TO VDUBUF.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "S  A010ACTIVE." TO VDU.
           MOVE PLACTIVE TO VDUBUF.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "S  A010PLTYPE." TO VDU.
           MOVE PLTYPE TO VDUBUF.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "S  A010RP." TO VDU.
           MOVE REPO TO VDUBUF.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "S  A010BK." TO VDU.
           MOVE BANK TO VDUBUF.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "S  A010LG." TO VDU.
           MOVE LEGL TO VDUBUF.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "S  A010DA." TO VDU.
           MOVE DEBT TO VDUBUF.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "S  A010JD." TO VDU.
           MOVE JD TO VDUBUF.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "S  A010MAT." TO VDU.
           MOVE MATURED TO VDUBUF.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "S  A010PROM." TO VDU.
           MOVE PROMISED TO VDUBUF.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "S  A010ZBAL." TO VDU.
           MOVE ZBAL TO VDUBUF.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "S  A0101STPY." TO VDU.
           MOVE NO1STPY TO VDUBUF.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "S  A010PRSEQ." TO VDU.
           MOVE BRSEQ TO VDUBUF.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "S  A010PROTH." TO VDU.
           MOVE PROTH-FG TO VDUBUF.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "S  A010PRCOL." TO VDU.
           MOVE PRCOL-FG TO VDUBUF.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "S  A010FROZEN." TO VDU.
           MOVE FROZEN-ISO TO VDUBUF.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "S  A010CONFIDENT." TO VDU.
           MOVE CONFIDENT-ISO TO VDUBUF.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "S  A010COLL." TO VDU.
           MOVE COLLECTOR-FG TO VDUBUF.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "S  A010ACT." TO VDU.
           MOVE ACTION-FG TO VDUBUF.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "S  A010TOT." TO VDU.
           MOVE TOTALS-FG TO VDUBUF.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "S  A010SUMONLY." TO VDU.
           MOVE REPORT-TYPE TO VDUBUF.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "S  A170SPOOL-PROMPT." TO VDU.
           MOVE "SPOOL TO LOCAL  :"    TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "S  A050YES-NO." TO VDU.
           MOVE "(Y/N)"         TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "S  A010LOCAL-SPOOL." TO VDU.
           MOVE LOCAL-PRINT-FG        TO VDUBUF.
           PERFORM SCREENIO.

      ******************************************************************
       DETAIL-LINE-PROCESS SECTION.
      ******************************************************************

           MOVE DASH-LINE TO DETAIL-LINE.
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE LN-SSNO(1) TO BW-SSNO.
           PERFORM READ-BW1-FILE.

           PERFORM LPSTAT-DISPLAY.
           MOVE LPS-DISPLAY-LNNO TO D-LOAN-NUM.
           MOVE LN-LOANDATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO D-LOANDATE.
           MOVE LN-DLNO TO D-DLNO.
           MOVE LN-SRCD TO D-SRCD.
           MOVE LN-COLLCD TO D-COLLCD.
           MOVE LN-PURCD TO D-PURCD.
           MOVE LN-SUM-ORGTERM TO D-ORGTERM.
           MOVE LN-SUM-LNAMT TO D-LNAMT.
           MOVE LN-1STPYDATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO D-1STPYDATE.

           MOVE HOLD-TODAY-DATE TO AGEING-DATE.
           PERFORM COMPUTE-CONTRACTUAL.
           IF CPOT30 = "Y"
              MOVE "<P>" TO D-CONX
           ELSE
           IF CPOT30 = "M"
              MOVE "<M>" TO D-CONX
           ELSE
           IF CPOTCHGOFF = "Y"
              MOVE "<PC>" TO D-CONX
           ELSE
           IF CPOTCHGOFF = "M"
              MOVE "<PL>" TO D-CONX
           ELSE
              MOVE CONTRACTUAL TO D-CON.

           MOVE LN-CONPROFILE(1) TO PROFILE-9.
           PERFORM PROFILE-DASH.
           MOVE PROFILE-X TO D-CONPROF.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      * CANT TRUST PDTH-DATE
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE NO-PDTH-DATE TO PDTH-DATE-WORK DATE-YYYYMMDD.
           IF NOT LN-UNITPER-CD-DAYS
              MOVE DATE-YYYYMMDD-YY TO DATE-YYYYMMDD-DD.
           MOVE DATE-YYYYMMDD TO D-PDTH.

           MOVE LN-PROMDATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO D-PROMISED.

           IF GP-CPCORP-CURBAL = "Y"
              IF GP-INCLUDE-O2-IN-BAL = "Y"
                 MOVE LN-CURBAL TO D-BALANCE
              ELSE
                 COMPUTE D-BALANCE = LN-CURBAL - LN-OT2BAL
              END-IF
           ELSE
              IF GP-INCLUDE-O2-IN-BAL = "Y"
                 COMPUTE D-BALANCE = LN-CURBAL + LN-OTHBAL
                                   + LN-INTBAL + LN-LCBAL
              ELSE
                 COMPUTE D-BALANCE = LN-CURBAL + LN-OTHBAL
                                   + LN-INTBAL + LN-LCBAL
                                   - LN-OT2BAL.

      *-----------------------------------------------------------------
       DETAIL-GET-TODAYDUE.
           MOVE NO-LDUE-DATE  TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY   TO D-LASTDATE.
           MOVE NO-NXDUE-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY   TO D-NEXTDATE.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE " " TO SAVE-BW-FIELDS.
           MOVE 0 TO SAVE-BW-PHONE SAVE-BW-EMPPHONE SAVE-BW-LOANS.
           MOVE 0 TO SAVE-CO-PHONE SAVE-CO-EMPPHONE SAVE-BW-EMPEXT.
           MOVE 1 TO SUB.
           PERFORM PROCESS-BORROWER.

           IF IO-GOOD
              MOVE BW-ADR1 TO SAVE-BW-ADR1
              MOVE BW-ADR2 TO SAVE-BW-ADR2
              MOVE BW-CITY TO SAVE-BW-CITY
              MOVE BW-STATE TO SAVE-BW-STATE
              MOVE BW-ZIP TO SAVE-BW-ZIP
              MOVE BW-PHONE TO SAVE-BW-PHONE
              MOVE BW-CELL-PHONE TO SAVE-BW-CPHONE
              MOVE BW-EMPLOYER TO SAVE-BW-EMPLOYER
              MOVE BW-EMPPHONE TO SAVE-BW-EMPPHONE
              MOVE BW-EMPLPOS TO SAVE-BW-EMPLPOS
              MOVE BW-EMPEXT TO SAVE-BW-EMPEXT
              MOVE BW-LOANS TO SAVE-BW-LOANS.

           IF LN-CONFIDENTCD = "Y" OR BW-CONFIDENTIAL = "Y"
              IF LN-ALLOTCD = "Y"
                 MOVE "*ALOT/CONF*" TO D-MSTAG
              ELSE
              IF LN-LBOX = "Y"
                 MOVE "*LBOX/CONF*" TO D-MSTAG
              ELSE
                 MOVE "*CONFIDENT*" TO D-MSTAG.

           IF LN-CONFIDENTCD NOT = "Y"
              IF BW-CONFIDENTIAL NOT = "Y"
                 IF LN-ALLOTCD = "Y"
                    MOVE "*ALLT PLAN*" TO D-MSTAG
                 ELSE
                 IF LN-LBOX = "Y"
                    MOVE "*LBOX PLAN*" TO D-MSTAG.

           MOVE 0 TO LCHG-LATE.
           MOVE NO-LATE-CHG TO D-LCAMT.

           PERFORM MATURITY-DATE-CALCULATION.

           IF LN-TOTNOALDEL NOT = 0
              MOVE MDTE-DATE TO NDTE-DATE
              MOVE LN-TOTNOALDEL TO NDTE-HOLD
              MOVE LN-UNITPER-CD TO DATER-UNITPER-CD
              MOVE LN-UNITPER-FREQ TO DATER-UNITPER-FREQ
              PERFORM INCREMENT-UNITPER
              MOVE NDTE-DATE TO MDTE-DATE.

           MOVE MDTE-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO D-MATURDATE.

           IF BR-RECDEL-NOT-REC = "Y"
              IF RDPOT30 = "Y"
                 MOVE "<P>" TO D-RECX
              ELSE
              IF RDPOT30 = "M"
                 MOVE "<M>" TO D-RECX
              ELSE
              IF RDPOTCHGOFF = "Y"
                 MOVE "<PC>" TO D-RECX
              ELSE
              IF RDPOTCHGOFF = "M"
                 MOVE "<PL>" TO D-RECX
              ELSE
                 MOVE NO-RECDEL TO D-REC
           ELSE
              MOVE NO-RECENCY TO D-REC.

           MOVE LN-RECPROFILE(1) TO PROFILE-9.
           PERFORM PROFILE-DASH.
           MOVE PROFILE-X TO D-RECPROF.

           IF ( SP-LCFRMLA NOT = SPACES ) OR
              ( LN-LC-CODE NOT = ZEROES )
              MOVE LN-LCPDTH-DATE TO DATE-YYYYMMDD
              MOVE DATE-YYYYMMDD-MM TO D-LCPD-MO
              IF LN-UNITPER-CD-DAYS
                 MOVE DATE-YYYYMMDD-DD TO D-LCPD-DAYR
              ELSE
                 MOVE DATE-YYYYMMDD-YY TO D-LCPD-DAYR.

           MOVE LN-LASTCFORMDATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY      TO D-FORMDATE.
           IF GP-NO-LC-IN-ARREARAGE
              MOVE NO-TDUECONTR TO D-TODAYDUE
              MOVE NO-LDUECONTR TO D-LASTAMT
              MOVE NO-NXDUECONTR TO D-NEXTAMT
           ELSE
              MOVE NO-TDUEAMT TO D-TODAYDUE
              MOVE NO-LDUEAMT TO D-LASTAMT
              MOVE NO-NXDUEAMT TO D-NEXTAMT.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           IF LN-SSNO(2) = 0
              MOVE BW-SPSSNO TO LN-SSNO(2)
              MOVE "S" TO LN-MAKERCD(2).

           MOVE 2 TO SUB.
           PERFORM PROCESS-BORROWER.
           IF IO-GOOD
              IF LN-MAKERCD(SUB) = "J" OR "E"
                 MOVE BW-CELL-PHONE TO SAVE-CO-CPHONE
                 MOVE BW-PHONE TO SAVE-CO-PHONE
                 MOVE BW-EMPPHONE TO SAVE-CO-EMPPHONE.

           MOVE "MKRPH: " TO D-PHTAG.

           IF SAVE-BW-CPHONE NOT = 0
              MOVE "C" TO D-CELL
              MOVE SAVE-BW-CPHONE TO D-CPHONE
              INSPECT D-CPHONE REPLACING ALL "/" BY "-".

           IF SAVE-BW-EMPPHONE NOT = 0
              MOVE "W" TO D-WORK
              MOVE SAVE-BW-EMPPHONE TO D-WPHONE
              INSPECT D-WPHONE REPLACING ALL "/" BY "-"
              IF SAVE-BW-EMPEXT NOT = 0
      * WANTED A SMALL LETTER C
                 MOVE X"78" TO D-WORKEXT
                 MOVE SAVE-BW-EMPEXT TO D-WEXT.

           IF SAVE-BW-PHONE NOT = 0
              MOVE "H" TO D-HOME
              MOVE SAVE-BW-PHONE TO D-HPHONE
              INSPECT D-HPHONE REPLACING ALL "/" BY "-".

           IF LN-LASTCFORMDATE NOT = 0
              MOVE "#" TO D-N
              MOVE LN-LASTCFORMNO TO D-FORMNO.

           MOVE NO-LDUELC TO D-LDUELC.
           MOVE NO-TDUELC TO D-TDUELC.
           MOVE NO-NXDUELC TO D-NXDUELC.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           IF REPORT-TYPE = "B"
              PERFORM PRINT-LT-REC-COL
              GO TO DETAIL-LN-ACCTOT.

           MOVE 3 TO SUB.
           PERFORM PROCESS-BORROWER.

           IF D-NAME = SPACES AND (LN-MAKERCD(2) = "J" OR "E")
              MOVE "COPH:" TO D-COMAKER-PHONE
              IF SAVE-CO-CPHONE NOT = 0
                 MOVE "C" TO D-CO-CELL
                 MOVE SAVE-CO-CPHONE TO D-CO-CPHONE
                 INSPECT D-CO-CPHONE REPLACING ALL "/" BY "-"
              END-IF
              IF SAVE-CO-EMPPHONE NOT = 0
                 MOVE "W" TO D-CO-WORK
                 MOVE SAVE-CO-EMPPHONE TO D-CO-WPHONE
                 INSPECT D-CO-WPHONE REPLACING ALL "/" BY "-"
              END-IF
              IF SAVE-CO-PHONE NOT = 0
                 MOVE "H" TO D-CO-HOME
                 MOVE SAVE-CO-PHONE TO D-CO-HPHONE
                 INSPECT D-CO-HPHONE REPLACING ALL "/" BY "-".

           MOVE "EMPLOYER:" TO D-EMPLOYER.
           PERFORM GET-LOAN-MEMOS.
           MOVE "-----MEMO-----" TO D-MEMOX.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE 4 TO SUB.
           PERFORM PROCESS-BORROWER.
           MOVE SAVE-BW-EMPLOYER TO D-EMPLOYER.

           IF LOAN-MEMO(1) NOT = SPACES
              MOVE LOAN-MEMO(1) TO D-MEMO
              MOVE LOAN-MEMO-DATE(1) TO D-MEMO-DT.

           MOVE 1 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE SAVE-BW-ADR1 TO D-ADDR.
           MOVE SAVE-BW-EMPLPOS TO D-EMPLOYER.

           IF LOAN-MEMO(2) NOT = SPACES
              MOVE LOAN-MEMO-DATE(2) TO D-MEMO-DT
              MOVE LOAN-MEMO(2) TO D-MEMO.

           PERFORM CONDITIONAL-PRINT.

           MOVE SAVE-BW-CITY TO NAME-WORKER.
           INSPECT NAME-WORKER REPLACING FIRST "      " BY ", ++ +".
           INSPECT NAME-WORKER REPLACING FIRST "++" BY SAVE-BW-STATE.

           INSPECT NAME-WORKER REPLACING FIRST "+         "
              BY SAVE-BW-ZIP.

           IF SAVE-BW-ADR2 NOT = " "
              MOVE SAVE-BW-ADR2 TO D-ADDR
           ELSE
              MOVE NAME-WORKER TO D-ADDR.

           MOVE "# LOANS:" TO D-LOANSX.
           MOVE SAVE-BW-LOANS TO D-LOANS.

           IF LOAN-MEMO(3) NOT = SPACES
              MOVE LOAN-MEMO(3) TO D-MEMO
              MOVE LOAN-MEMO-DATE(3) TO D-MEMO-DT.

           PERFORM CONDITIONAL-PRINT.

           IF SAVE-BW-ADR2 NOT = " "
              MOVE NAME-WORKER TO D-ADDR.

           MOVE "REG PMT:" TO D-LOANSX.
           MOVE LN-REGPYAMT TO D-PMT.

           IF LOAN-MEMO(4) NOT = SPACES
              MOVE LOAN-MEMO(4) TO D-MEMO
              MOVE LOAN-MEMO-DATE(4) TO D-MEMO-DT.

           PERFORM CONDITIONAL-PRINT.

           PERFORM GET-LOAN-PMTS.

           IF LOAN-PMT(1) NOT = " "
              MOVE HEAD7 TO D-PMT-LINE
              MOVE 1 TO LN-FEED
              PERFORM WRITE-DETAIL-LINE.

           MOVE LOAN-PMT(1) TO D-PMT-LINE.
           PERFORM CONDITIONAL-PRINT.

           MOVE LOAN-PMT(2) TO D-PMT-LINE.
           PERFORM CONDITIONAL-PRINT.

           MOVE LOAN-PMT(3) TO D-PMT-LINE.
           PERFORM CONDITIONAL-PRINT.

           MOVE LOAN-PMT(4) TO D-PMT-LINE.
           PERFORM CONDITIONAL-PRINT.
           PERFORM PRINT-LT-REC-COL.

      *----------------------------------------------------------------
       DETAIL-LN-ACCTOT.

           MOVE 1 TO LEV.
           PERFORM DETAIL-LN-ACCTBL.
           MOVE 2 TO LEV.
           PERFORM DETAIL-LN-ACCTBL.
           MOVE 3 TO LEV.
           PERFORM DETAIL-LN-ACCTBL.

           GO TO END-DETAIL-LINE-PROCESS.

      *-----------------------------------------------------------------
       DETAIL-LN-ACCTBL.
           IF LN-SALEFIN = "Y"
              MOVE 2 TO LTYP
           ELSE
              MOVE 1 TO LTYP.

      ******************************************************************
      *-CONTRACTUAL-----------------------------------------------------
      ******************************************************************
           ADD 1 TO T-NO(LTYP LEV WK-CSUB 1).

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      * T-OTH (LTYP LEV WK-PSUB 3) = LN-OTHBAL
      * T-OTH2(LTYP LEV WK-PSUB 3) = LN-OT2BAL
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

           ADD LN-OTHBAL TO T-OTH(LTYP LEV WK-CSUB 1).
      *    ADD LN-OT2BAL TO T-OTH(LTYP LEV WK-CSUB 1).
           ADD LN-OT2BAL TO T-OTH2(LTYP LEV WK-CSUB 1).
           ADD LN-INTBAL TO T-INT(LTYP LEV WK-CSUB 1).
           ADD LN-LCBAL TO T-INT(LTYP LEV WK-CSUB 1).

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      * T-BAL(LTYP LEV WK-CSUB 1) = LN-CURBAL - LN-OT2BAL
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

           ADD LN-CURBAL TO T-BAL(LTYP LEV WK-CSUB 1).
           IF GP-INCLUDE-O2-IN-BAL NOT = "Y"
              SUBTRACT LN-OT2BAL FROM T-BAL(LTYP LEV WK-CSUB 1).
           IF GP-NO-LC-IN-ARREARAGE
              ADD NO-TDUECONTR TO T-TDUE(LTYP LEV WK-CSUB 1)
           ELSE
              ADD NO-TDUEAMT TO T-TDUE(LTYP LEV WK-CSUB 1).

      ******************************************************************
      *-RECENCY---------------------------------------------------------
      ******************************************************************
           ADD 1 TO T-NO(LTYP LEV WK-RDSUB 2).
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      * T-OTH(LTYP LEV WK-PSUB 2) = LN-OTHBAL + LN-OT2BAL.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

           ADD LN-OTHBAL TO T-OTH(LTYP LEV WK-RDSUB 2).
      *    ADD LN-OT2BAL TO T-OTH(LTYP LEV WK-RDSUB 2).
           ADD LN-OT2BAL TO T-OTH2(LTYP LEV WK-RDSUB 2).
           ADD LN-INTBAL TO T-INT(LTYP LEV WK-RDSUB 2).
           ADD LN-LCBAL TO T-INT(LTYP LEV WK-RDSUB 2).

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      * T-BAL(LTYP LEV WK-RDSUB 2) = LN-CURBAL - LN-OT2BAL
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

           ADD LN-CURBAL TO T-BAL(LTYP LEV WK-RDSUB 2).
           IF GP-INCLUDE-O2-IN-BAL NOT = "Y"
              SUBTRACT LN-OT2BAL FROM T-BAL(LTYP LEV WK-RDSUB 2).
           IF GP-NO-LC-IN-ARREARAGE
              ADD NO-TDUECONTR TO T-TDUE(LTYP LEV WK-RDSUB 2)
           ELSE
              ADD NO-TDUEAMT TO T-TDUE(LTYP LEV WK-RDSUB 2).

      ******************************************************************
      *-PAID AHEAD------------------------------------------------------
      ******************************************************************
           IF WK-PSUB > 0
              ADD 1 TO T-NO(LTYP LEV WK-PSUB 3)
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      * T-OTH (LTYP LEV WK-PSUB 3) = LN-OTHBAL
      * T-OTH2(LTYP LEV WK-PSUB 3) = LN-OT2BAL
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
              ADD LN-OTHBAL TO T-OTH(LTYP LEV WK-PSUB 3)
              ADD LN-OT2BAL TO T-OTH2(LTYP LEV WK-PSUB 3)
              ADD LN-INTBAL TO T-INT(LTYP LEV WK-PSUB 3)
              ADD LN-LCBAL TO T-INT(LTYP LEV WK-PSUB 3)
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      * T-BAL(LTYP LEV WK-PSUB 3) = LN-CURBAL - LN-OT2BAL
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
              ADD LN-CURBAL TO T-BAL(LTYP LEV WK-PSUB 3)
              IF GP-INCLUDE-O2-IN-BAL NOT = "Y"
                 SUBTRACT LN-OT2BAL FROM T-BAL(LTYP LEV WK-RDSUB 3)
              END-IF
              IF GP-NO-LC-IN-ARREARAGE
                 ADD NO-TDUECONTR TO T-TDUE(LTYP LEV WK-PSUB 3)
              ELSE
                 ADD NO-TDUEAMT TO T-TDUE(LTYP LEV WK-PSUB 3).

       END-DETAIL-LINE-PROCESS.
           EXIT.

      ******************************************************************
       SINGLE-LINE-DETAIL-PROCESS SECTION.
      ******************************************************************

           PERFORM LOAN-CALCULATIONS.

           MOVE 1 TO SUB.
           PERFORM PROCESS-BORROWER.

           PERFORM LPSTAT-DISPLAY.
           MOVE LPS-DISPLAY-LNNO TO D-SLOAN-NUM.
           MOVE LN-1STPYDA       TO D-SDUEDAY.

           IF BR-RECDEL-NOT-REC = "Y"
              IF RDPOT30 = "Y"
                 MOVE "<P>" TO D-SRECX
              ELSE
              IF RDPOT30 = "M"
                 MOVE "<M>" TO D-SRECX
              ELSE
              IF RDPOTCHGOFF = "Y"
                 MOVE "<PC>" TO D-SRECX
              ELSE
              IF RDPOTCHGOFF = "M"
                 MOVE "<PL>" TO D-SRECX
              ELSE
                 MOVE NO-RECDEL TO D-SREC
           ELSE
              MOVE NO-RECENCY TO D-SREC.

           IF CPOT30 = "Y"
              MOVE "<P>" TO D-SCONX
           ELSE
           IF CPOT30 = "M"
              MOVE "<M>" TO D-SCONX
           ELSE
           IF CPOTCHGOFF = "Y"
              MOVE "<PC>" TO D-SCONX
           ELSE
           IF CPOTCHGOFF = "M"
              MOVE "<PL>" TO D-SCONX
           ELSE
              MOVE NO-TRUCONTR TO D-SCON.

           MOVE LN-DATE-PAID-LAST TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY       TO D-SLASTDATE.
           MOVE LN-LOANDATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY       TO D-SLNDATE.
           MOVE LN-PROMDATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY       TO D-SPROMDATE.

           IF GP-CPCORP-CURBAL = "Y"
              IF GP-INCLUDE-O2-IN-BAL = "Y"
                 MOVE LN-CURBAL TO D-SBALANCE
              ELSE
                 COMPUTE D-SBALANCE = LN-CURBAL - LN-OT2BAL
              END-IF
           ELSE
              IF GP-INCLUDE-O2-IN-BAL = "Y"
                 COMPUTE D-SBALANCE = LN-CURBAL + LN-OTHBAL
                                   + LN-INTBAL + LN-LCBAL
              ELSE
                 COMPUTE D-SBALANCE = LN-CURBAL + LN-OTHBAL
                                   + LN-INTBAL + LN-LCBAL
                                   - LN-OT2BAL.

           IF GP-NO-LC-IN-ARREARAGE
              MOVE NO-TDUECONTR TO D-STODAYDUE
           ELSE
              MOVE NO-TDUEAMT TO D-STODAYDUE.

      *????MOVE TOTAL-NOTE TO D-STOTNOTE.

           IF LN-CONFIDENTCD = "Y" OR BW-CONFIDENTIAL = "Y"
              IF LN-ALLOTCD = "Y"
                 MOVE "C A" TO D-SFLAG
              ELSE
              IF LN-LBOX = "Y"
                 MOVE "C L" TO D-SFLAG
              ELSE
                 MOVE "C" TO D-SFLAG.

           IF LN-CONFIDENTCD NOT = "Y"
              IF BW-CONFIDENTIAL NOT = "Y"
                 IF LN-ALLOTCD = "Y"
                    MOVE "A" TO D-SFLAG
                 ELSE
                 IF LN-LBOX = "Y"
                    MOVE "L" TO D-SFLAG.

      *------------------------------------------------------------
      *  OTHER BALANCE REPLACES CONFID/ALLOT FLAGS SET ABOVE
      *------------------------------------------------------------
           IF PROTH-FG = "Y"
              COMPUTE WORK-AMT = LN-OTHBAL + LN-OT2BAL
              MOVE WORK-AMT TO D-SOTHBAL.

           MOVE NO-PDTH-DATE TO DATE-YYYYMMDD.
           IF NOT LN-UNITPER-CD-DAYS
              MOVE DATE-YYYYMMDD-YY TO DATE-YYYYMMDD-DD.
           MOVE 0 TO DATE-YYYYMMDD-CCYY.
           MOVE DATE-YYYYMMDD TO D-SPDTH.

           PERFORM CHECK-RENEWABILITY
           IF RENEWABLE-FG = "Y"
              PERFORM GET-AVAILABLE-CREDIT
              MOVE AVCR-AVAILCREDIT TO D-SDLNO
           ELSE
              MOVE 0                TO D-SDLNO.

           MOVE LN-AMT-PAID-LAST TO D-STOTAMTPD.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      * BOTH SUMMARY AND SINGLE LINE PERFORM THIS ROUTINE
      * IF SUMMARY ONLY, DONT WRITE THE LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

           IF REPORT-TYPE = "L"
              MOVE 1 TO LN-FEED
              PERFORM WRITE-DETAIL-LINE
              PERFORM PRINT-LT-REC-COL
           ELSE
              MOVE SPACES TO DETAIL-LINE.


           MOVE 1 TO LEV.
           PERFORM DETAIL-LN-ACCTBL.
           MOVE 2 TO LEV.
           PERFORM DETAIL-LN-ACCTBL.
           MOVE 3 TO LEV.
           PERFORM DETAIL-LN-ACCTBL.

       END-SINGLE-DETAIL-LINE-PROCESS.
           EXIT.

      ******************************************************************
       PROCESS-BORROWER SECTION.
      ******************************************************************
           IF REPORT-TYPE NOT = "L"
              MOVE SPACES TO D-NAME
           ELSE
              MOVE SPACES TO D-SNAME.

           MOVE LN-SSNO(SUB) TO BW-SSNO.

           IF BW-SSNO = 0
              GO TO END-PROCESS-BORROWER.

           IF REPORT-TYPE NOT = "L"
              MOVE LN-MAKERCD(SUB) TO D-MAKERCD.

           MOVE BW-SSNO TO NUM-BW.
           PERFORM NUMBW-TO-ALPBW.

           IF REPORT-TYPE NOT = "L"
              MOVE ALP-BW TO D-SSNO
              IF HIDE-FIRST-5-FLAG = "Y"
                 MOVE ALL "X" TO D-SOC1 D-SOC2.

           IF (REPORT-TYPE = "L"  OR "S") OR SUB NOT = 1
              PERFORM READ-BW1-FILE.

           IF IO-BAD
               IF REPORT-TYPE = "L"
                  MOVE "**NOT ON FILE**" TO D-SNAME
               ELSE
                  MOVE "**NOT ON FILE**" TO D-NAME.

           IF IO-BAD
              MOVE 0 TO BW-SPSSNO
              GO TO END-PROCESS-BORROWER.

           MOVE BW-LNAME TO NAME-WORKER.
           MOVE BW-FNAME TO FIRST-NAME.
           INSPECT NAME-WORKER REPLACING FIRST "  " BY ",?".

           INSPECT NAME-WORKER REPLACING FIRST "?               "
               BY FIRST-NAME.

           IF REPORT-TYPE = "L"
              MOVE NAME-WORKER TO D-SNAME
           ELSE
              MOVE NAME-WORKER TO D-NAME.

       END-PROCESS-BORROWER.
           EXIT.

      ******************************************************************
       GET-LOAN-MEMOS SECTION.
      ******************************************************************
           PERFORM OPEN-LM1-FILE.

           MOVE LM-PATH-OWNBR TO LM-BRNO
                                 QLM1-WBEG-BRNO
                                 QLM1-WEND-BRNO.
           MOVE LN-ACCTNO     TO LM-ACCTNO
                                 QLM1-WBEG-ACCTNO
                                 QLM1-WEND-ACCTNO.
      *     MOVE 0             TO LM-DATE
      *                           QLM1-WBEG-DATE-REV.
      *     MOVE ALL "9"       TO LM-SEQ 
      *                           QLM1-WBEG-SEQ.
      *     MOVE ALL "9"       TO QLM1-WEND-DATE-REV
      *                           QLM1-WEND-SEQ.

      ***  LM1 CURSOR USES TOP 4 TO LIMIT NUMBER OF ROWS RETURNED ***
      ***  SINCE THIS PROCESS ONLY USES THE FIRST 4 MEMO ROWS     ***

           PERFORM START-LM1-FILE.   *> SSP  LM

           MOVE " " TO LOAN-MEMO-BUF.
           MOVE 1 TO MEMO-SUB.

       NEXT-MEMO.
           PERFORM READ-LM1-FILE-NEXT.    *>  SSP LM
           IF IO-FG NOT = 0                        *> OR (LM-BRNO NOT = LM-PATH-OWNBR)
              GO TO EXIT-GET-LOAN-MEMOS.

      *     IF LM-ACCTNO NOT = LN-ACCTNO
      *        GO TO EXIT-GET-LOAN-MEMOS.

           MOVE LM-MEMO TO LOAN-MEMO (MEMO-SUB).
           SUBTRACT LM-DATE FROM 99999999 GIVING DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO LOAN-MEMO-DATE (MEMO-SUB).

           IF MEMO-SUB < 4
              ADD 1 TO MEMO-SUB
              GO TO NEXT-MEMO.

       EXIT-GET-LOAN-MEMOS.
           PERFORM CLOSE-LM1-FILE.

      ******************************************************************
       GET-LOAN-PMTS SECTION.
      ******************************************************************

           MOVE LP-PATH-OWNBR TO LP-BRNO
                                 QLP1-WBEG-BRNO
                                 QLP1-WEND-BRNO.
           MOVE LN-ACCTNO     TO LP-ACCTNO
                                 QLP1-WBEG-ACCTNO
                                 QLP1-WEND-ACCTNO.
      *     MOVE ALL "9"       TO LP-SEQNO
      *                           QLP1-WEND-SEQNO.
      *     MOVE ALL "0"       TO QLP1-WBEG-SEQNO.

      ***  LP1 CURSOR USES TOP 4 TO LIMIT NUMBER OF ROWS RETURNED ***
      ***  SINCE THIS PROCESS ONLY USES THE FIRST 4 LP ROWS      ***

           PERFORM START-LP1-FILE-NOT-GREATER.   *> SSP LPFILE
           MOVE " " TO LOAN-PMT-BUF.
           MOVE 1 TO PMT-SUB.

       NEXT-PMT.
           PERFORM READ-LP1-FILE-PREVIOUS.   *> SSP LPFILE
           IF IO-BAD
              GO TO EXIT-GET-LOAN-PMTS.

      *     IF (LP-BRNO NOT = LN-OWNBR) OR
      *         (LP-ACCTNO NOT = LN-ACCTNO)
      *        GO TO EXIT-GET-LOAN-PMTS.

           MOVE " "         TO PMT-LINE.
           MOVE LP-PAYDATE  TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO L-010.
           MOVE LP-TRCD     TO L-020.
           MOVE LP-REFNO    TO L-030.
           IF LP-TRCD = "JD"  OR "AC" OR "IR"
              MOVE LP-NEWSTAT-RATE TO L-040RATE
           ELSE
              MOVE LP-TRAMT TO L-040.

           IF LP-TRCD = "AP"
              MOVE LP-APINTOWE TO L-050
              ADD LP-INTDUE LP-LCPARTIALS GIVING L-060
              COMPUTE L-070 = 0 - LP-APINTOWE
                                - LP-INTDUE
                                - LP-LCPARTIALS
           ELSE
              MOVE LP-APOTH TO L-050
              ADD LP-APINT LP-APLC GIVING L-060
              MOVE LP-APCUR TO L-070.

           IF (LP-TRCD = "RV" OR LP-REV NOT = " ") AND
               NOT (LP-TRCD = "RV" AND LP-REVSEQNO = 0)
              MOVE "*" TO L-120
           ELSE
              MOVE LP-PDTH-DATE TO DATE-YYYYMMDD
              MOVE DATE-YYYYMMDD-MM    TO L-080-MO
              IF LN-UNITPER-CD-DAYS
                 MOVE DATE-YYYYMMDD-DD TO L-080-DAYR
              ELSE
                 MOVE DATE-YYYYMMDD-YY TO L-080-DAYR
              END-IF
              COMPUTE L-090 = LP-OTHBAL + LP-OT2BAL
              ADD LP-LCBAL LP-INTBAL GIVING L-100
              MOVE LP-CURBAL TO L-110.

           MOVE LOAN-PMT(3) TO LOAN-PMT(4).
           MOVE LOAN-PMT(2) TO LOAN-PMT(3).
           MOVE LOAN-PMT(1) TO LOAN-PMT(2).
           MOVE D-PMT-LINE TO LOAN-PMT(1).

           IF PMT-SUB < 4
              ADD 1 TO PMT-SUB
              GO TO NEXT-PMT.

       EXIT-GET-LOAN-PMTS.
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
       PRINT-RANGE-LINES SECTION.
      ******************************************************************
           MOVE 2 TO LN-FEED.

           IF GROUP-FLAG = "Y"
              MOVE "BEGINNING BRANCH   :" TO RANGE-LINE
              MOVE ENT-GROUP              TO RANGE-SELECTION
              PERFORM WRITE-DETAIL-LINE
              MOVE "ENDING BRANCH      :" TO RANGE-LINE
              MOVE SPACES                 TO RANGE-SELECTION
              PERFORM WRITE-DETAIL-LINE
           ELSE
              MOVE "BEGINNING BRANCH   :" TO RANGE-LINE
              MOVE BEG-BRANCH             TO RANGE-SELECTION
              PERFORM WRITE-DETAIL-LINE
              MOVE "ENDING BRANCH      :" TO RANGE-LINE
              MOVE END-BRANCH             TO RANGE-SELECTION
              PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "COLLECTOR ID       :" TO RANGE-LINE.
           MOVE COLCTID (1) TO COLCTID-CHAR (1).
           MOVE COLCTID (2) TO COLCTID-CHAR (2).
           MOVE COLCTID (3) TO COLCTID-CHAR (3).
           MOVE COLCTID-WK             TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "ACTION CODE        :" TO RANGE-LINE.
           MOVE ACTION-CD (1) TO ACTION-CHAR (1).
           MOVE ACTION-CD (2) TO ACTION-CHAR (2).
           MOVE ACTION-CD (3) TO ACTION-CHAR (3).
           MOVE ACTION-CD (4) TO ACTION-CHAR (4).
           MOVE ACTION-WK              TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "BEGINNING CLASS    :" TO RANGE-LINE.
           MOVE BEG-CLASSX              TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "ENDING CLASS       :" TO RANGE-LINE.
           IF BEG-CLASSX NUMERIC
              MOVE END-CLASS           TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "BEGINNING DEALER   :" TO RANGE-LINE
           MOVE BEG-DEALER TO RS-DEALER
           MOVE RS-DLR                 TO RANGE-SELECTION
           PERFORM WRITE-DETAIL-LINE
           MOVE "ENDING DEALER      :" TO RANGE-LINE
           MOVE END-DEALER TO RS-DEALER
           MOVE RS-DLR                 TO RANGE-SELECTION
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "BEGINNING LAST NAME:" TO RANGE-LINE.
           MOVE BEG-ALPHA TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "ENDING LAST NAME   :" TO RANGE-LINE.
           MOVE END-ALPHA TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "PAST DUE DAYS      :" TO RANGE-LINE.
           MOVE BEG-AGE                TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "ENDING DAYS        :" TO RANGE-LINE.
           MOVE END-AGE                TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "MIN    PAST-DUE AMT:" TO RANGE-LINE.
           MOVE PAST-D-AMT             TO RANGE-NUM.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "BEGINNING LOAN AMT :" TO RANGE-LINE.
           MOVE BEG-LNAMT              TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "ENDING LOAN AMT    :" TO RANGE-LINE.
           MOVE END-LNAMT              TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "BEGIN CONTRACTUAL  :" TO RANGE-LINE.
           MOVE BEG-CAGE               TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "END   CONTRACTUAL  :" TO RANGE-LINE.
           MOVE END-CAGE               TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "POTS  CONTRACTUAL  :" TO RANGE-LINE.
           MOVE CPOT                   TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "MPAY  CONTRACTUAL  :" TO RANGE-LINE.
           MOVE CMISS                  TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "BEGIN RECENCY      :" TO RANGE-LINE.
           MOVE BEG-RAGE               TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "END   RECENCY      :" TO RANGE-LINE.
           MOVE END-RAGE               TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "POTS  RECENCY      :" TO RANGE-LINE.
           MOVE RPOT                   TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "MIN AVAIL CREDIT   :" TO RANGE-LINE.
           MOVE MIN-AVAILCR            TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "BEGIN PAST PROMISE :" TO RANGE-LINE.
           STRING BEG-PAST BEG-SIGN DELIMITED SIZE
                                     INTO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "END   PAST PROMISE :" TO RANGE-LINE.
           STRING END-PAST END-SIGN DELIMITED SIZE
                                     INTO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "BEGIN BALANCE DUE  :" TO RANGE-LINE.
           MOVE BEG-BAL          TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "END   BALANCE DUE  :" TO RANGE-LINE.
           MOVE END-BAL          TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "OWNED,SERVICE,BOTH :" TO RANGE-LINE.
           MOVE OWNED                  TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "RECOURSE           :" TO RANGE-LINE.
           MOVE RC                     TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "P & L              :" TO RANGE-LINE.
           MOVE PL                     TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "P&L TYPE           :" TO RANGE-LINE.
           MOVE PLTYPE                 TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "ACTIVE             :" TO RANGE-LINE.
           MOVE PLACTIVE               TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "REPO               :" TO RANGE-LINE.
           MOVE REPO                   TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "BANKRUPT           :" TO RANGE-LINE.
           MOVE BANK                   TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "LEGAL              :" TO RANGE-LINE.
           MOVE LEGL                   TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "DEBT-AID           :" TO RANGE-LINE.
           MOVE DEBT                   TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "JUDGEMENTS         :" TO RANGE-LINE.
           MOVE JD                     TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "MATURED            :" TO RANGE-LINE.
           MOVE MATURED                TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "PROMISED           :" TO RANGE-LINE.
           MOVE PROMISED               TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "ZERO BALANCES      :" TO RANGE-LINE.
           MOVE ZBAL                   TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "NO 1ST PAYMENT     :" TO RANGE-LINE.
           MOVE NO1STPY                   TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "PRINT SEQUENCE     :" TO RANGE-LINE.
           MOVE BRSEQ                  TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "PRINT OTHER BALS   :" TO RANGE-LINE.
           MOVE PROTH-FG               TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "PRINT COLLATERAL   :" TO RANGE-LINE.
           MOVE PRCOL-FG               TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "FROZEN ACCOUNTS    :" TO RANGE-LINE.
           MOVE FROZEN-ISO             TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "CONFIDENT ACCOUNTS :" TO RANGE-LINE.
           MOVE CONFIDENT-ISO             TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "COLLECTOR BREAK    :" TO RANGE-LINE.
           MOVE COLLECTOR-FG           TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "ACTION CODE BREAK  :" TO RANGE-LINE.
           MOVE ACTION-FG              TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "TOTALS / RANGES    :" TO RANGE-LINE.
           MOVE TOTALS-FG              TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "REPORT TYPE        :" TO RANGE-LINE.
           MOVE REPORT-TYPE           TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "STARTING TIME (N)  :" TO RANGE-LINE.
           MOVE STARTING-TIME         TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "ENDING   TIME      :" TO RANGE-LINE.
           PERFORM GET-TIME.
           MOVE TIME-EDIT             TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.

      ******************************************************************
       PRINT-COLLECTION-SUMMARY SECTION.
      ******************************************************************
           PERFORM ZERO-TT-TOTALS.
           PERFORM GET-PORTFOLIO-TOTALS.

           MOVE 0 TO TEST-ZERO TEST-NM-ZERO.
           PERFORM ACCUM-NM-AGEING-TOTALS
              VARYING LTYP FROM 1 BY 1 UNTIL LTYP > 2
                AFTER AGE FROM 1 BY 1 UNTIL AGE > 31.

           PERFORM ACCUM-AGEING-TOTALS
              VARYING LTYP FROM 1 BY 1 UNTIL LTYP > 2
                AFTER AGE FROM 1 BY 1 UNTIL AGE > 10
                AFTER TYP FROM 1 BY 1 UNTIL TYP > 3.

           IF TEST-ZERO = 0
              GO TO PRINT-COLLECTOR-TOTALS-EXIT.

           MOVE EXT-CONAME-RPT-LINES TO LN-COUNT.
           MOVE 1 TO LN-FEED.
           EVALUATE LEV
              WHEN 1
                     MOVE "COLLECTOR TOTALS:" TO S-SUMNAME
              WHEN 2
                     MOVE "GRAND TOTALS:" TO S-SUMNAME
              WHEN 3
                     MOVE "BRANCH    TOTALS:" TO S-SUMNAME
           END-EVALUATE.
           PERFORM WRITE-DETAIL-LINE.
           MOVE 1 TO TYP.
           MOVE "********* CONTRACTUAL DELINQUENCY *********"
                                  TO S-AGETYPE.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "DIRECT  LOAN"  TO S-DESC.
           MOVE "SALES  FINANCE"  TO S-DESC2.
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE SUMMARY1 TO DETAIL-LINE.
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE 2 TO LN-FEED.
           PERFORM PRINT-DETAIL-SUM
                  VARYING AGE FROM 1 BY 1 UNTIL AGE > 10.
           MOVE 2 TO LN-FEED.
           PERFORM PRINT-SUM-TOTALS.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    FOR NON MONTHS DELINQUENCY PRINTING
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "  ********* RECENCY DELINQUENCY *********"
                                TO S-AGETYPE.
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "DIRECT  LOAN"  TO S-DESC.
           MOVE "SALES  FINANCE"  TO S-DESC2.
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE SUMMARY1 TO DETAIL-LINE.
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE 2 TO LN-FEED.
           PERFORM PRINT-DETAIL-SUM
                 VARYING AGE FROM 1 BY 1 UNTIL AGE > 10
                AFTER TYP FROM 2 BY 1 UNTIL TYP > 3.
           MOVE 2 TO TYP.
           MOVE 2 TO LN-FEED.
           PERFORM PRINT-SUM-TOTALS.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE 3 TO TYP.
           MOVE 1 TO LN-FEED.
           PERFORM PRINT-SUM-TOTALS.

       PRINT-COLLECTOR-TOTALS-EXIT.
           EXIT.

      ******************************************************************
       ZERO-TT-TOTALS SECTION.
      ******************************************************************
           PERFORM VARYING LTYP FROM 1 BY 1 UNTIL LTYP > 2
              MOVE 0 TO TT-NM-NO  (LTYP) TT-NM-BAL (LTYP)
                        TT-NM-INT (LTYP) TT-NM-OTH (LTYP)
                        TT-NM-OTH2(LTYP)
                        TT-NM-TDUE(LTYP) TT-NM-UNE (LTYP)
                        TT-NM-ERN (LTYP) TT-NM-IER (LTYP)
           END-PERFORM.

           PERFORM VARYING LTYP FROM 1 BY 1 UNTIL LTYP > 2
                     AFTER TYP  FROM 1 BY 1 UNTIL TYP > 3
              MOVE 0 TO TT-NO  (LTYP TYP)
                        TT-BAL (LTYP TYP)
                        TT-INT (LTYP TYP)
                        TT-OTH (LTYP TYP)
                        TT-OTH2(LTYP TYP)
                        TT-TDUE(LTYP TYP)
                        TT-UNE (LTYP TYP)
                        TT-ERN (LTYP TYP)
                        TT-IER (LTYP TYP)
           END-PERFORM.

      ******************************************************************
       ZERO-TOTAL-TBL SECTION.
      ******************************************************************
           PERFORM VARYING LTYP FROM 1 BY 1 UNTIL LTYP > 2
                     AFTER AGE  FROM 1 BY 1 UNTIL AGE  > 31
              MOVE 0 TO T-NM-NO  (LTYP LEV AGE)
                        T-NM-BAL (LTYP LEV AGE)
                        T-NM-INT (LTYP LEV AGE)
                        T-NM-OTH (LTYP LEV AGE)
                        T-NM-OTH2(LTYP LEV AGE)
                        T-NM-TDUE(LTYP LEV AGE)
                        T-NM-UNE (LTYP LEV AGE)
                        T-NM-ERN (LTYP LEV AGE)
                        T-NM-IER (LTYP LEV AGE)
           END-PERFORM.

           PERFORM VARYING LTYP FROM 1 BY 1 UNTIL LTYP > 2
                     AFTER AGE  FROM 1 BY 1 UNTIL AGE  > 10
                     AFTER TYP  FROM 1 BY 1 UNTIL TYP  > 3
              MOVE 0 TO T-NO  (LTYP LEV AGE TYP)
                        T-BAL (LTYP LEV AGE TYP)
                        T-INT (LTYP LEV AGE TYP)
                        T-OTH (LTYP LEV AGE TYP)
                        T-OTH2(LTYP LEV AGE TYP)
                        T-TDUE(LTYP LEV AGE TYP)
                        T-UNE (LTYP LEV AGE TYP)
                        T-ERN (LTYP LEV AGE TYP)
                        T-IER (LTYP LEV AGE TYP)
           END-PERFORM.

      ******************************************************************
       ACCUM-AGEING-TOTALS SECTION.
      ******************************************************************
           ADD T-NO(LTYP LEV AGE TYP) TO TT-NO(LTYP TYP) TEST-ZERO.
           ADD T-BAL(LTYP LEV AGE TYP) TO TT-BAL(LTYP TYP) TEST-ZERO.
           ADD T-INT(LTYP LEV AGE TYP) TO TT-INT(LTYP TYP) TEST-ZERO.
           ADD T-OTH(LTYP LEV AGE TYP) TO TT-OTH(LTYP TYP) TEST-ZERO.
           ADD T-OTH2(LTYP LEV AGE TYP) TO TT-OTH2(LTYP TYP) TEST-ZERO.
           ADD T-TDUE(LTYP LEV AGE TYP) TO TT-TDUE(LTYP TYP) TEST-ZERO.
           ADD T-UNE(LTYP LEV AGE TYP) TO TT-UNE(LTYP TYP) TEST-ZERO.
           ADD T-ERN(LTYP LEV AGE TYP) TO TT-ERN(LTYP TYP) TEST-ZERO.
           ADD T-IER(LTYP LEV AGE TYP) TO TT-IER(LTYP TYP) TEST-ZERO.

      ******************************************************************
       ACCUM-NM-AGEING-TOTALS SECTION.
      ******************************************************************
           ADD T-NM-NO  (LTYP LEV AGE) TO TT-NM-NO  (LTYP) TEST-NM-ZERO.
           ADD T-NM-BAL (LTYP LEV AGE) TO TT-NM-BAL (LTYP) TEST-NM-ZERO.
           ADD T-NM-INT (LTYP LEV AGE) TO TT-NM-INT (LTYP) TEST-NM-ZERO.
           ADD T-NM-OTH (LTYP LEV AGE) TO TT-NM-OTH (LTYP) TEST-NM-ZERO.
           ADD T-NM-OTH2(LTYP LEV AGE) TO TT-NM-OTH2(LTYP) TEST-NM-ZERO.
           ADD T-NM-TDUE(LTYP LEV AGE) TO TT-NM-TDUE(LTYP) TEST-NM-ZERO.
           ADD T-NM-UNE (LTYP LEV AGE) TO TT-NM-UNE (LTYP) TEST-NM-ZERO.
           ADD T-NM-ERN (LTYP LEV AGE) TO TT-NM-ERN (LTYP) TEST-NM-ZERO.
           ADD T-NM-IER (LTYP LEV AGE) TO TT-NM-IER (LTYP) TEST-NM-ZERO.

      ******************************************************************
       GET-PORTFOLIO-TOTALS SECTION.
      ******************************************************************

      *    IF GRAND TOTALS THEN MOVE ACCUMULATED TOTALS TO PORTFOLIO
      *       TOTALS.
           IF TOTAL-FG = "Y"
              MOVE ACCUM-DL-AMT TO  PORT-AMT(1)
              MOVE ACCUM-DL-NO TO  PORT-NO(1)
              MOVE ACCUM-SF-AMT TO  PORT-AMT(2)
              MOVE ACCUM-SF-NO TO  PORT-NO(2)
              GO TO EXIT-PORTFOLIO-TOTALS.

           IF GROUP-FLAG = "Y"
              IF ENT-GROUP = HOLD-GROUP
                 GO TO EXIT-PORTFOLIO-TOTALS
              END-IF
              MOVE ENT-GROUP TO HOLD-GROUP SUMMBY-BRNO
           ELSE
              IF BR-NO = HOLD-BRNO
                 GO TO EXIT-PORTFOLIO-TOTALS
              END-IF
              MOVE BR-NO TO HOLD-BRNO SUMMBY-BRNO.

           MOVE AGE-DATE  TO SUMMBY-DATE.
      * TYPE 1 : REC + O2.
           MOVE 1 TO SUMMBY-TYPE.
           MOVE BEG-CLASS TO SUMMBY-BEG-CLASS.
           MOVE END-CLASS TO SUMMBY-END-CLASS.

           MOVE "CL/SUMMBY" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH SUMMBY-LINK CRWKBY-REC
           END-CALL.
           CANCEL FORM-PROGX.

           IF OWNED = "O"
              MOVE CRWKBY-AMT(2) TO PORT-AMT(2)
              MOVE CRWKBY-NO(2)  TO PORT-NO(2)
           ELSE
           IF OWNED = "S"
              MOVE CRWKBY-AMT(3) TO PORT-AMT(2)
              MOVE CRWKBY-NO(3)  TO PORT-NO(2)
           ELSE
              COMPUTE PORT-AMT(2) =
                  CRWKBY-AMT(2) + CRWKBY-AMT(3)
              COMPUTE PORT-NO(2) =
                  CRWKBY-NO(2) + CRWKBY-NO(3).

            MOVE CRWKBY-AMT(1) TO PORT-AMT(1).
            MOVE CRWKBY-NO(1)  TO PORT-NO(1).

      * IF NEW GROUP OR NEW BRANCH THEN ACCUMULATE TOTALS.
      *
           ADD PORT-AMT(1) TO ACCUM-DL-AMT.
           ADD PORT-NO(1)  TO ACCUM-DL-NO.
           ADD PORT-AMT(2) TO ACCUM-SF-AMT.
           ADD PORT-NO(2)  TO ACCUM-SF-NO.

       EXIT-PORTFOLIO-TOTALS.
           EXIT.

      ******************************************************************
       PRINT-DETAIL-SUM SECTION.
      ******************************************************************
           IF T-NO(1 LEV AGE TYP) = 0 AND
              T-NO(2 LEV AGE TYP) = 0
              IF T-BAL(1 LEV AGE TYP) = 0 AND
                 T-BAL(2 LEV AGE TYP) = 0
                 IF T-TDUE(1 LEV AGE TYP) = 0 AND
                    T-TDUE(2 LEV AGE TYP) = 0
                               GO TO END-PRINT-DETAIL-SUM.

           IF TYP = 3
              MOVE " PAID-AHEAD:" TO S-NAME
           ELSE
              MOVE AGE-DESC(AGE) TO S-NAME.

           COMPUTE P-AGE = AGE + 1.
           COMPUTE M-AGE = AGE - 1.

           PERFORM PRINT-DET-SUM
                    VARYING LTYP FROM 1 BY 1 UNTIL LTYP > 2.

           GO TO PRINT-DET-SUM-EXIT.

       PRINT-DET-SUM.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *--NUMBER----------
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF T-NO(LTYP LEV AGE TYP) NOT = 0
              MOVE T-NO(LTYP LEV AGE TYP) TO S-NO(LTYP)
              IF TYP NOT = 3
                 IF PORT-NO(LTYP) NOT = 0
                    COMPUTE S-NO-P(LTYP) ROUNDED =
                        (T-NO(LTYP LEV AGE TYP) /
                                     PORT-NO(LTYP)) * 100
                 ELSE
                    MOVE 0 TO S-NO-P(LTYP).

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *--BALANCE-DUE------
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

           IF T-BAL(LTYP LEV AGE TYP) NOT = 0
              MOVE T-BAL(LTYP LEV AGE TYP) TO S-BAL(LTYP)
              IF TYP NOT = 3
                 IF PORT-AMT(LTYP) NOT = 0
                    COMPUTE S-BAL-P(LTYP) ROUNDED =
                        (T-BAL(LTYP LEV AGE TYP) /
                                 PORT-AMT(LTYP)) * 100
                 ELSE
                    MOVE 0 TO S-BAL-P(LTYP).

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *--OTHER-DUE------
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           COMPUTE WORK-AMT = T-OTH (LTYP LEV AGE TYP) +
                              T-OTH2(LTYP LEV AGE TYP).
           IF WORK-AMT NOT = 0
              MOVE WORK-AMT TO S-OTH(LTYP).

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *---TODAY DUE-----------
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

           IF T-TDUE(LTYP LEV AGE TYP) NOT = 0
              MOVE T-TDUE(LTYP LEV AGE TYP) TO S-TDUE(LTYP)
              IF TYP NOT = 3
                 IF PORT-AMT(LTYP) NOT = 0
                    COMPUTE S-TDUE-P(LTYP) ROUNDED =
                        (T-TDUE(LTYP LEV AGE TYP) /
                                   PORT-AMT(LTYP)) * 100
                 ELSE
                    MOVE 0 TO S-TDUE-P(LTYP).

      *-----------------------------------------------------------------
       PRINT-DET-SUM-EXIT.
              PERFORM WRITE-DETAIL-LINE.

       END-PRINT-DETAIL-SUM.
           EXIT.



      ******************************************************************
       PRINT-SUM-TOTALS SECTION.
      ******************************************************************
           IF TYP = 3
              MOVE " PAID-AHEAD:" TO S-NAME
           ELSE
              MOVE "      TOTAL:" TO S-NAME.

           MOVE TT-NO(1 TYP) TO S-NO(1).
           MOVE TT-BAL(1 TYP) TO S-BAL-10(1).
           COMPUTE WORK-AMT = TT-OTH(1 TYP) + TT-OTH2(1 TYP).
           MOVE WORK-AMT       TO S-OTH(1).
           MOVE TT-TDUE(1 TYP) TO S-TDUE(1).

           MOVE TT-NO(2 TYP) TO S-NO(2).
           MOVE TT-BAL(2 TYP) TO S-BAL-10(2).
           COMPUTE WORK-AMT = TT-OTH(2 TYP) + TT-OTH2(2 TYP).
           MOVE WORK-AMT       TO S-OTH(2).
           MOVE TT-TDUE(2 TYP) TO S-TDUE(2).

           PERFORM WRITE-DETAIL-LINE.

           IF TYP NOT = 3
              IF TT-NO(1 TYP) NOT = 0
                 IF TT-BAL(1 TYP) NOT = 0
                     MOVE "ALL CLASSES:" TO S-NAME
                     MOVE PORT-NO(1) TO S-NO(1)
                     MOVE PORT-AMT(1) TO S-BAL-10(1)
                 END-IF
              END-IF
              IF TT-NO(2 TYP) NOT = 0
                 IF TT-BAL(2 TYP) NOT = 0
                     MOVE "ALL CLASSES:" TO S-NAME
                     MOVE PORT-NO(2) TO S-NO(2)
                     MOVE PORT-AMT(2) TO S-BAL-10(2)
                 END-IF
              END-IF
              PERFORM WRITE-DETAIL-LINE.

      ******************************************************************
       COLCTID-CD-CHECK SECTION.
      ******************************************************************
           MOVE "Y" TO COLCTID-MATCH.
           MOVE 0 TO COLCTID-SUB.

       COLCTID-NEXT-CHAR.
           ADD 1 TO COLCTID-SUB.

           IF COLCTID-CHAR(COLCTID-SUB) NOT =
              COLCTID(COLCTID-SUB)
                 AND
              COLCTID(COLCTID-SUB) NOT = "?"
              MOVE "N" TO COLCTID-MATCH
              GO TO COLCTID-CD-CHECK-EXIT.

           IF COLCTID-SUB < 3
              GO TO COLCTID-NEXT-CHAR.

       COLCTID-CD-CHECK-EXIT.
           EXIT.

      ******************************************************************
       ACTION-CD-CHECK SECTION.
      ******************************************************************
           MOVE "Y" TO ACTION-MATCH.
           MOVE 0 TO ACTION-SUB.

       ACTION-NEXT-CHAR.
           ADD 1 TO ACTION-SUB.

           IF ACTION-CHAR(ACTION-SUB) NOT = ACTION-CD(ACTION-SUB) AND
              ACTION-CD(ACTION-SUB) NOT = "?"
              MOVE "N" TO ACTION-MATCH
              GO TO ACTION-CD-CHECK-EXIT.

           IF ACTION-SUB < 4
              GO TO ACTION-NEXT-CHAR.

       ACTION-CD-CHECK-EXIT.
           EXIT.


      ************************************
       PRINT-LT-REC-COL SECTION.
      ************************************
           IF PRCOL-FG = "Y"
              MOVE LN-OWNBR  TO LTC-BRNO
              MOVE LN-ACCTNO TO LTC-ACCTNO
              MOVE 1         TO LTC-SEQNO
              PERFORM OPEN-LTC1-FILE
              PERFORM READ-LTC1-FILE
              IF IO-GOOD
                 MOVE "MAKE : " TO D-FIL-MAKE
                 MOVE "MODEL : " TO D-FIL-MODEL
                 MOVE "YEAR : " TO D-FIL-YEAR
                 MOVE "VIN # : " TO D-FIL-ID
                 MOVE LTC-MAKE(1) TO D-MAKE
                 MOVE LTC-MODEL(1) TO D-MODEL
                 MOVE LTC-YEAR(1) TO D-YEAR
                 MOVE LTC-VIN(1) TO D-ID
                 PERFORM WRITE-DETAIL-LINE

                 IF ( LTC-MAKE (2) NOT = SPACES ) OR
                    ( LTC-MODEL(2) NOT = SPACES ) OR
                    ( LTC-YEAR (2) NOT = SPACES ) OR
                    ( LTC-VIN  (2) NOT = SPACES ) 
                    MOVE "MAKE : "    TO D-FIL-MAKE
                    MOVE "MODEL : "   TO D-FIL-MODEL
                    MOVE "YEAR : "    TO D-FIL-YEAR
                    MOVE "VIN # : "   TO D-FIL-ID
                    MOVE LTC-MAKE(2)  TO D-MAKE
                    MOVE LTC-MODEL(2) TO D-MODEL
                    MOVE LTC-YEAR(2)  TO D-YEAR
                    MOVE LTC-VIN(2)   TO D-ID
                    PERFORM WRITE-DETAIL-LINE
                 END-IF
              END-IF
              PERFORM CLOSE-LTC1-FILE
              MOVE SPACES TO D-LINE6.


           IF PROTH-FG = "Y"
              IF REPORT-TYPE = "B"
                 COMPUTE WORK-AMT = LN-OTHBAL + LN-OT2BAL
                 IF WORK-AMT NOT = 0
                    MOVE "OTHER BALS:  " TO D-OTHERX
                    MOVE WORK-AMT TO D-OTHER-BALS
                    PERFORM WRITE-DETAIL-LINE.

       PRINT-LT-REC-COL-EXIT.
           EXIT.

      ******************************************************************
       HEAD-PAGE SECTION.
      ******************************************************************

           ADD 1 TO PAGE-NUMBER.
           MOVE PAGE-NUMBER TO H-PAGE-NO.
           PERFORM GET-TIME.
           MOVE T-HH TO H-HR.
           MOVE T-MM TO H-MIN.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE HEAD1 TO PR-REC.
           MOVE 99 TO LN-FEED.
           PERFORM WRITE-PR-REC.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE HEAD2 TO PR-REC.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-PR-REC.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE HEAD3 TO PR-REC.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-PR-REC.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE LN-COLLECTOR TO H-COLCTID.

      *    IF S-SUMNAME = "GRAND TOTALS:"
           IF TOTAL-FG = "Y"
              MOVE SPACES TO H-COLCTID.

           IF (COLLECTOR-FG = "Y")
              MOVE HEAD4X TO PR-REC
              MOVE 1 TO LN-FEED
              PERFORM WRITE-PR-REC.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF (S-SUMNAME = "GRAND TOTALS:") OR
              (S-SUMNAME = "COLLECTOR TOTALS:") OR
              (S-SUMNAME = "BRANCH    TOTALS:")
              MOVE SPACES TO H-ACTION
           ELSE
              MOVE LN-ACTIONCD TO H-ACTION.

           IF ACTION-FG = "Y"
              MOVE HEAD4 TO PR-REC
              MOVE 1 TO LN-FEED
              PERFORM WRITE-PR-REC.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF (S-SUMNAME = "GRAND TOTALS:") OR
              (S-SUMNAME = "COLLECTOR TOTALS:") OR
              (S-SUMNAME = "BRANCH    TOTALS:")
              GO TO END-HEAD-PAGE.

           IF PROTH-FG = "Y"
              MOVE "  OTHER" TO HEAD8-FILLER1
              MOVE "   BALS" TO HEAD9-FILLER1.

           IF REPORT-TYPE = "L"
              MOVE HEAD8 TO PR-REC
              MOVE 2 TO LN-FEED
              PERFORM WRITE-PR-REC
              MOVE HEAD9 TO PR-REC
              MOVE 1 TO LN-FEED
              PERFORM WRITE-PR-REC
           ELSE
              MOVE HEAD5 TO PR-REC
              MOVE 2 TO LN-FEED
              PERFORM WRITE-PR-REC

              MOVE HEAD6 TO PR-REC
              MOVE 1 TO LN-FEED
              PERFORM WRITE-PR-REC.


      *----------------------------------------------------------------
       END-HEAD-PAGE.
           MOVE 8 TO LN-COUNT.
           MOVE 2 TO LN-FEED.

      ******************************************************************
       SET-BRANCH-HEADINGS SECTION.
      ******************************************************************
           MOVE BR-NO TO H-BRNO.
           MOVE BR-NAME TO H-BRDESC.

      ******************************************************************
       SET-COLLECTOR-HEADINGS SECTION.
      ******************************************************************
           MOVE OLD-COLCTID-CD TO H-COLCTID.
           MOVE SPACES TO H-COLDESC.

      ******************************************************************
       COLLECTOR-TOTALS SECTION.
      ******************************************************************

           IF ( TOTAL-TBL(1,1) = TOTAL-TBL(1,3) ) AND
              ( TOTAL-TBL(2,1) = TOTAL-TBL(2,3) ) AND
              ( COLCTIDX   NOT = "???"          )
              GO TO COLLECTOR-TOTALS-END.

           MOVE 1 TO LEV.
           PERFORM PRINT-COLLECTION-SUMMARY.

       COLLECTOR-TOTALS-END.

           MOVE 1            TO LEV.
           PERFORM ZERO-TOTAL-TBL.
           MOVE EXT-CONAME-RPT-LINES TO LN-COUNT.
           MOVE 1            TO LN-FEED.

       COLLECTOR-TOTALS-EXIT.
           EXIT.

      ******************************************************************
       BRANCH-TOTALS SECTION.
      ******************************************************************
           MOVE 3 TO LEV.
           PERFORM PRINT-COLLECTION-SUMMARY.

       END-BRANCH-TOTALS.
           MOVE 3 TO LEV.
           PERFORM ZERO-TOTAL-TBL.
           MOVE EXT-CONAME-RPT-LINES TO LN-COUNT.
           MOVE 1 TO LN-FEED.

      ******************************************************************
       GRAND-TOTALS SECTION.
      ******************************************************************
           MOVE "Y" TO TOTAL-FG.
           MOVE SPACES TO H-BRNO.
           MOVE "** ALL BRANCHES **" TO H-BRDESC.
           MOVE "** ALL COLLECTORS **" TO H-COLDESC.
           MOVE 2 TO LEV.
           PERFORM PRINT-COLLECTION-SUMMARY.

       END-GRAND-TOTALS.

      *************************************************************
       CLASS-SCAN SECTION.
      *************************************************************
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
       DEALER-SCAN SECTION.
      *****************************************************************
           MOVE 0 TO DLSCAN-NUM.
           MOVE "WI/DLSCAN " TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH DLSCAN-BUF EXIT-PATHNAME.
           CANCEL FORM-PROGX.
           IF DLSCAN-STATUS NOT = "00" OR DLSCAN-NUM = " "
               MOVE 9 TO IO-FG
               GO TO EXIT-DEALER-SCAN.
           IF ( DEALER-FG = "DLBEG" )
              MOVE "SNNN042DL1." TO VDU
           ELSE
              MOVE "SNNN042DL2." TO VDU.
           MOVE DLSCAN-DLNO TO VDUNUM2.
           PERFORM SCREENIO.
           MOVE DLSCAN-DLNO TO VDUNUM2.
           MOVE 0 TO IO-FG.

       EXIT-DEALER-SCAN.
           EXIT.

      ******************************************************************
       CHECK-RENEWABILITY SECTION.
      ******************************************************************
           MOVE "Y" TO RENEWABLE-FG.
           IF SP-RENEWABLE-PAYS = 0
              IF SP-RENEWABLE-DAYS = 0
                 MOVE "N" TO RENEWABLE-FG
                 GO TO EXIT-CHECK-RENEWABILITY.
           MOVE LN-1STPYDATE TO PDTH-DATE-WORK.
           PERFORM PAID-THRU-CALCULATION.
           IF SP-RENEWABLE-PAYS = -1
              COMPUTE WK-RENEWABLE-PAYS = LN-ORGTERM / 3
              IF PDTH-PAYMENTS < WK-RENEWABLE-PAYS
                 MOVE "N" TO RENEWABLE-FG
                 GO TO EXIT-CHECK-RENEWABILITY
              END-IF
           ELSE
           IF PDTH-PAYMENTS < SP-RENEWABLE-PAYS
              MOVE "N" TO RENEWABLE-FG
              GO TO EXIT-CHECK-RENEWABILITY.
           MOVE LN-LOANDATE TO NUM-DATE.
           MOVE HOLD-TODAY-DATE TO SYS-DATE.
           PERFORM TIM.
           IF SP-RENEWABLE-DAYS = -1
              COMPUTE WK-RENEWABLE-DAYS = (LN-ORGTERM / 3) * 30 + 1
              IF ELAPSED-DAYS < WK-RENEWABLE-DAYS
                 MOVE "N" TO RENEWABLE-FG
                 GO TO EXIT-CHECK-RENEWABILITY
              END-IF
           ELSE
           IF ELAPSED-DAYS < SP-RENEWABLE-DAYS
              MOVE "N" TO RENEWABLE-FG.
       EXIT-CHECK-RENEWABILITY.
           EXIT.

      ******************************************************************
       TEST-CLASS SECTION.
      ******************************************************************

           IF LN-CLASS NOT = HOLD-LN-CLASS
              MOVE LN-CLASS TO LC-CODE HOLD-LN-CLASS
              PERFORM READ-LC1-FILE
              IF IO-FG NOT = 0
                 GO TO EXIT-TEST-CLASS.

           IF BEG-CLASS1 = "S"
              IF LC-SALEFIN NOT = "Y"
                 MOVE 9 TO IO-FG
                 GO TO EXIT-TEST-CLASS.

           IF BEG-CLASS1 = "L"
              IF LC-SALEFIN = "Y" OR LC-GIFTNET = "Y"
                 MOVE 9 TO IO-FG
                 GO TO EXIT-TEST-CLASS.

           IF BEG-CLASS1 = "M"
              IF LC-SMALLOAN NOT = "Y"
                 MOVE 9 TO IO-FG
                 GO TO EXIT-TEST-CLASS.

           IF BEG-CLASS1 = "W"
              IF LC-GIFTNET NOT = "Y"
                 MOVE 9 TO IO-FG
                 GO TO EXIT-TEST-CLASS.
           IF BEG-CLASS1 = "R"
              IF LC-SALEFIN = "Y" OR LC-SMALLOAN = "Y"
                 MOVE 9 TO IO-FG
                 GO TO EXIT-TEST-CLASS.


       EXIT-TEST-CLASS.
           EXIT.

      ******************************************************************
       GET-AVAILABLE-CREDIT SECTION.
      ******************************************************************

           IF BR-AVAIL-CREDIT = "Y"
                     AND (LN-CREDITLIM NOT = 0)
                      AND (LN-BNKRPTDATE = 0)
              MOVE HOLD-TODAY-DATE TO AVCR-PAYDATE
              MOVE LN-MAKERCD(1) TO AVCR-MAKERCD
              PERFORM COMPUTE-AVAILABLE-CREDIT
           ELSE
           IF BR-AVAIL-CREDIT = "S"
                     AND (LN-CREDITLIM NOT = 0)
                      AND (LN-BNKRPTDATE = 0)
              MOVE HOLD-TODAY-DATE TO AVCR-PAYDATE
              MOVE LN-MAKERCD(1) TO AVCR-MAKERCD
              PERFORM COMPUTE-AVAILABLE-CREDIT
           ELSE
              MOVE 0 TO AVCR-AVAILCREDIT.

      **********************************************
       VERIFY-PASSWORD SECTION.
      **********************************************
           MOVE " " TO PASSWDW-STAT.
           MOVE "CPCORP" TO PASSWDW-PGM.
           MOVE "PLEASE ENTER PASSWORD, " TO PASSWDW-PROMPT1.
           MOVE "CL/PASSWD"  TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH PASSWDW-BUF.
           CANCEL FORM-PROGX.

      ******************************************************************
      *
      *     C O P Y    L I B R A R Y    S E C T I O N S
      *
      ******************************************************************
       COPY "LIBLP/LPNOCL.CPY".
       COPY "LIBLP/LPCRNO.CPY".
       COPY "LIBLP/LPDEF.CPY".
       COPY "LIBGB/GPENV.CPY".
       COPY "LIBLP/LPITRM.CPY".
       COPY "LIBLP/LPAPRS.CPY".
       COPY "LIBLP/LPAPRZ.CPY".
       COPY "LIBLP/LPUPER.CPY".
       COPY "LIBGB/DATER.CPY".
       COPY "LIBLP/SETCTBL.CPY".
       COPY "LIBLP/LPSTAT.CPY".
       COPY "LIBGB/SPPDTH.CPY".
       COPY "LIBLP/LPSCHD.CPY".
       COPY "LIBLP/LPLCAS.CPY".
       COPY "LIBLP/LPLCHG.CPY".
       COPY "LIBLP/LPLCAP.CPY".
       COPY "LIBLP/LPMDTE.CPY".
       COPY "LIBLP/LPXDTE.CPY".
       COPY "LIBLP/LPPOFF.CPY".
       COPY "LIBLP/LPPOF2.CPY".
       COPY "LIBLP/LPIBPC.CPY".
       COPY "LIBLP/LPPDUE.CPY".
       COPY "LIBGB/RBACT.CPY".
       COPY "LIBLP/LPCDEL.CPY".
       COPY "LIBLP/LPAVCR.CPY".
       COPY "LIBLP/LPTDEL.CPY".
       COPY "LIBLP/DEFPOL.CPY".
       COPY "LIBLP/LPRATE.CPY".
       COPY "LIBGB/REBATE.CPY".
       COPY "LIBLP/LPAMTS.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/AGEING.CPY".
       COPY "LIBLP/LPUPWK.CPY".
       COPY "LIBGB/TIMEIO.CPY".
       COPY "LIBLP/LPCEPP.CPY".
       COPY "LIBGB/GRWORK.CPY".
       COPY "LIBGB/GRSERCH.CPY".
       COPY "LIBGB/BRSECURE.CPY".
       COPY "LIBGB/EOSPOOL.CPY".
       COPY "LIBLP/ADDONSPR.CPY".

       COPY "LIBLP/ARRAYSP.CPY".

      *********************************************************
       FORM-RESET SECTION.
      *********************************************************
           MOVE EXT-CONAME-CONAME TO CPCORP-CONAME.
           MOVE EXT-CONAME-SYSDATE TO CPCORP-SYSDATE.
           DISPLAY CPCORP-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE CPCORP-FORM-FIELDS TO WR-BUF.
           MOVE CPCORP-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      *********************************************************
       SETUP-LINK-INFO SECTION.
      *********************************************************
           MOVE CPCORP-HELP--FILE TO HELP-LINK-FILE.
           MOVE CPCORP-HELP--DIR  TO HELP-LINK-DIR.

      *********************************************************
       VDU-CONVERT-FIELD SECTION.
      *********************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBLP/CPCORP_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ******************************************************************
       MISC-PARA SECTION.
      ******************************************************************
       COPY "LIBGB/GETPR.CPY".
       COPY "LIBGB/MKTEMP.CPY".
       COPY "LIBGB/RMFILE.CPY".
       COPY "LIBGB/SYSTEM.CPY".
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/DACCESS.CPY".

      *----------------------------------------------------------------
       SEND-LEGEND.
           MOVE MESS-LIST TO WR-LIST.
           MOVE MESS TO WR-BUF.
           PERFORM CALL-WRFORM.

      *----------------------------------------------------------------
       COMPUTE-PAST-DUE-AMT.
           MOVE LN-1STPYDATE TO CEPP1-DATE.
           MOVE AGE-DATE     TO CEPP2-DATE.
           MOVE LN-MAKERCD(1) TO CEPP-MAKERCD.
           PERFORM TOTAL-DELINQUENCY.
           IF CDEL-ARREARAGE < 0
              MULTIPLY -1 BY CDEL-ARREARAGE
           ELSE
              MOVE 0 TO CDEL-ARREARAGE.

       INVALID-RANGE.
           MOVE "INVALID RANGE" TO MESS.
           PERFORM SEND-MESS.
       PROFILE-DASH.
           INSPECT PROFILE-X REPLACING ALL "9" BY "-".
       NUMBW-TO-ALPBW.
           MOVE NUM-BW1 TO ALP-BW1.
           MOVE NUM-BW2 TO ALP-BW2.
           MOVE NUM-BW3 TO ALP-BW3.
           MOVE "-" TO ALP-HYPHEN1 ALP-HYPHEN2.
       CONDITIONAL-PRINT.
           IF DETAIL-LINE NOT = " "
              MOVE 1 TO LN-FEED
              PERFORM WRITE-DETAIL-LINE.
       GET-SP.
      * DO NOT USE HOLD FIELDS, ALWAYS READ, LCHG ROUTINES CHANGE SPR VALUES
           MOVE LN-ORGST    TO SP-ORGST.
           MOVE LN-SPRCLASS TO SP-SPRCLASS.
           MOVE LN-SUBCLASS TO SP-SUBCLASS.
           MOVE LN-LAWCODE  TO SP-LAWCODE.
      *     PERFORM READ-SP1-FILE.
           PERFORM ARRAYSP-READ.

      ******************************************************************
       IO-ROUTINE SECTION.
      ******************************************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBLP/LPSPGS_SQL.CPY".
       COPY "LIBLP/LPLTPGS_SQL.CPY".
       COPY "LIBLP/LPLTIGS_SQL.CPY".
       COPY "LIBLP/LPLTCGS_SQL.CPY".
       COPY "LIBLP/LPLPGS_SQL.CPY".
       COPY "LIBLP/LPLNGS_SQL.CPY".
       COPY "LIBLP/LPNOGS_SQL.CPY".
       COPY "LIBLP/LPLMGS_SQL.CPY".
       COPY "LIBLP/LPLCGS_SQL.CPY".
       COPY "LIBLP/LPBWGS_SQL.CPY".
       COPY "LIBGB/GBGRGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBGBGS_SQL.CPY".
       COPY "LIBGB/GBPRINT.CPY".
       COPY "LIBLP/LPBW1RN.CPY".
       COPY "LIBLP/LPLC1RN.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBGB/GBGBRN.CPY".
       COPY "LIBGB/GBGR1RN.CPY".
       COPY "LIBLP/LPSP1RN.CPY".
       COPY "LIBGB/GBWKIN.CPY".
      * COPY "LIBLP/LPLN1RN.CPY".
       COPY "LIBLP/LPNO1RN.CPY".
       COPY "LIBLP/LPLTC1RN.CPY".
       COPY "LIBLP/LPLTP1RN.CPY".
       COPY "LIBLP/LPLTI1RN.CPY".

      * COPY "LIBLP/LPLP1RN.CPY".
      * COPYMEMBER: LIBLP/LPLP1RN
      * BAH 20240212 ADDED LP-REPAY-TRANS-ID #1641
       LOAD-LP1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( LP-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( LP-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO LP-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO LP-ALL-MACHINE
              MOVE EXT-FILPATH-DATA        TO LP-ALL-DATA
              MOVE LP-ALL-PATH             TO LP-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( LP-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO LP-ALL-BASE
              MOVE FILPATH-MACHINE         TO LP-ALL-MACHINE
              MOVE FILPATH-DATA            TO LP-ALL-DATA
              MOVE LP-ALL-PATH             TO LP-PATH.

      *-----------------------------------------------------------------
       OPEN-LP1-FILE.
           PERFORM LOAD-LP1-FILE.
           PERFORM OPEN-IT.
           MOVE LP-PATH-SQL TO E-FILE.

           IF LP1-CONN-OPEN-SW = "N" 
               EXEC SQL CONNECT TO :SQL-CONNECT-SERVER AS LP1_CONNECT
                    USER :SQL-CONNECT-USERID USING :SQL-CONNECT-PSWD
               END-EXEC
               MOVE "Y" TO LP1-CONN-OPEN-SW
               EXEC SQL SET CONNECTION C1 END-EXEC.

      *-----------------------------------------------------------------
      *  ***NOTE*** ONLY USE START-LP1-FILE-SCAN WHEN USING A 
      *  START-LP1-FILE-NOT-GREATER.  THIS IS FOR DISPLAYING MULTIPLE
      *  PAGE IN A UP & DOWN SCENARIO.
      *-----------------------------------------------------------------
       START-LP1-FILE-NOT-GREATER.   *> SSP LPFILE
           PERFORM START-IT.
           MOVE LP-PATH-OWNBR  TO LP-BRNO.
           MOVE LP-PATH-SQL    TO E-FILE.
           MOVE LP1-KEY        TO E-KEYX.

           IF ( LP1-CURSOR-NG-STAT-GOOD )
              PERFORM CLOSE-LP1-FILE.

      *     SELECT LPFILE.LP_BRNO,
      *            LPFILE.LP_ACCTNO,
      *            LPFILE.LP_SEQNO,
      *            CAST(LPFILE.LP_TRDATE AS VARCHAR(10)), 
      *            CAST(LPFILE.LP_PAYDATE AS VARCHAR(10)), 
      *            LPFILE.LP_TRCD, 
      *            LPFILE.LP_REFNO, 
      *            LPFILE.LP_TRAMT,
      *            LPFILE.LP_APOTH,
      *            LPFILE.LP_APINT,
      *            LPFILE.LP_APLC,
      *            LPFILE.LP_APCUR,
      *            LPFILE.LP_OTHBAL,
      *            LPFILE.LP_OT2BAL,
      *            LPFILE.LP_INTBAL,
      *            LPFILE.LP_LCBAL,
      *            LPFILE.LP_CURBAL,
      *            LPFILE.LP_INTDUE,
      *            CAST(LPFILE.LP_PDTH_DATE AS VARCHAR(10)), 
      *            LPFILE.LP_APINTOWE,
      *            LPFILE.LP_REV, 
      *            LPFILE.LP_REVSEQNO,
      *            LPFILE.LP_NEWSTAT_RATE,
      *            LPFILE.LP_LCPARTIALS
      *     FROM DBO.LPFILE
      *     WHERE LPFILE.LP_BRNO   = @QLP1_WBEG_BRNO
      *       AND LPFILE.LP_ACCTNO = @QLP1_WBEG_ACCTNO
      *     ORDER BY LPFILE.LP_SEQNO DESC

           EXEC SQL SET CONNECTION LP1_CONNECT END-EXEC.

           EXEC SQL
            DECLARE LP1_CURSOR_NG CURSOR FOR
                :LP1-RETURN-CODE = 
                 EXEC SSP_CPCORP_LPFILE_SELECT
                  ( :QLP1-WBEG-BRNO, :QLP1-WBEG-ACCTNO)
                END-EXEC.

           EXEC SQL
               OPEN LP1_CURSOR_NG
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           EXEC SQL SET CONNECTION C1 END-EXEC.

           MOVE ZEROES TO QLP1-WBEG-BRNO 
                          QLP1-WBEG-ACCTNO.

           MOVE STAT---GOOD TO LP1-CURSOR-NG-STAT.

      *-----------------------------------------------------------------
       READ-LP1-FILE-PREVIOUS.   *> SSP LPFILE
           PERFORM READ-IT.
           MOVE LP-PATH-OWNBR  TO LP-BRNO.
           MOVE LP-PATH-SQL    TO E-FILE.
           MOVE LP1-KEY        TO E-KEYX.
           INITIALIZE QLP-REC.

           EXEC SQL SET CONNECTION LP1_CONNECT END-EXEC.

           IF ( LP1-CURSOR-NG-STAT-GOOD )
              EXEC SQL
                 FETCH LP1_CURSOR_NG INTO 
                   :QLP-BRNO,
                   :QLP-ACCTNO,
                   :QLP-SEQNO,
                   :QLP-TRDATE, 
                   :QLP-PAYDATE,  
                   :QLP-TRCD, 
                   :QLP-REFNO, 
                   :QLP-TRAMT,
                   :QLP-APOTH,
                   :QLP-APINT,
                   :QLP-APLC,
                   :QLP-APCUR,
                   :QLP-OTHBAL,
                   :QLP-OT2BAL,
                   :QLP-INTBAL,
                   :QLP-LCBAL,
                   :QLP-CURBAL,
                   :QLP-INTDUE,
                   :QLP-PDTH-DATE,
                   :QLP-APINTOWE,
                   :QLP-REV, 
                   :QLP-REVSEQNO,
                   :QLP-NEWSTAT-RATE,
                   :QLP-LCPARTIALS
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDPVCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           EXEC SQL SET CONNECTION C1 END-EXEC.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-LP1-FILE-PREVIOUS.

           IF ( IO-FG = 0 )
              PERFORM GET-LP-FIELDS.

      *-----------------------------------------------------------------
       CLOSE-LP1-FILE.

           PERFORM CLOSE-IT.

           IF ( LP1-CURSOR-NG-STAT-GOOD )
              EXEC SQL SET CONNECTION LP1_CONNECT END-EXEC
              EXEC SQL
                   CLOSE LP1_CURSOR_NG
              END-EXEC
              EXEC SQL SET CONNECTION C1 END-EXEC
              MOVE STAT---BAD TO LP1-CURSOR-NG-STAT.

      *-----------------------------------------------------------------
       CLOSE-LP1-CONNECTION.

           IF LP1-CONN-OPEN-SW = "Y"
               EXEC SQL SET CONNECTION LP1_CONNECT END-EXEC
               EXEC SQL COMMIT END-EXEC
               EXEC SQL DISCONNECT LP1_CONNECT END-EXEC
               EXEC SQL SET CONNECTION C1 END-EXEC
               MOVE "N" TO LP1-CONN-OPEN-SW.
      *-----------------------------------------------------------------


      * COPY "LIBLP/LPLM1RN.CPY".
      * COPYMEMBER: LIBLP/LPLM1RN
       LOAD-LM1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( LM-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( LM-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO LM-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO LM-ALL-MACHINE
              MOVE EXT-FILPATH-DATA        TO LM-ALL-DATA
              MOVE LM-ALL-PATH             TO LM-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( LM-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO LM-ALL-BASE
              MOVE FILPATH-MACHINE         TO LM-ALL-MACHINE
              MOVE FILPATH-DATA            TO LM-ALL-DATA
              MOVE LM-ALL-PATH             TO LM-PATH.

      *-----------------------------------------------------------------
       OPEN-LM1-FILE.
           PERFORM LOAD-LM1-FILE.
           PERFORM OPEN-IT.
           MOVE LM-PATH-SQL TO E-FILE.

           IF LM1-CONN-OPEN-SW = "N" 
               EXEC SQL CONNECT TO :SQL-CONNECT-SERVER AS LM1_CONNECT
                    USER :SQL-CONNECT-USERID USING :SQL-CONNECT-PSWD
               END-EXEC
               MOVE "Y" TO LM1-CONN-OPEN-SW
               EXEC SQL SET CONNECTION C1 END-EXEC.

      *-----------------------------------------------------------------
       START-LM1-FILE.
           PERFORM START-IT.
           MOVE LM-PATH-OWNBR  TO LM-BRNO.
           MOVE LM-PATH-SQL    TO E-FILE.
           MOVE LM1-KEY        TO E-KEYX.

           IF ( LM1-CURSOR-STAT-GOOD )
              PERFORM CLOSE-LM1-FILE.

           EXEC SQL SET CONNECTION LM1_CONNECT END-EXEC.

           EXEC SQL
            DECLARE LM1_CURSOR CURSOR FOR
                :LM1-RETURN-CODE = 
                 EXEC SSP_CPCORP_LMFILE_SELECT
                  ( :QLM1-WBEG-BRNO, :QLM1-WBEG-ACCTNO)
                END-EXEC.

           EXEC SQL
               OPEN LM1_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           EXEC SQL SET CONNECTION C1 END-EXEC.

           MOVE ZEROES TO QLM1-WBEG-BRNO 
                          QLM1-WBEG-ACCTNO.

           MOVE STAT---GOOD TO LM1-CURSOR-STAT.

      *-----------------------------------------------------------------
       READ-LM1-FILE-NEXT.   *> SSP LM
           PERFORM READ-IT.
           MOVE LM-PATH-OWNBR  TO LM-BRNO.
           MOVE LM-PATH-SQL    TO E-FILE.
           MOVE LM1-KEY        TO E-KEYX.
           INITIALIZE QLM-REC.

      *      SELECT TOP 4 LMFILE.LM_BRNO,
      *                   LMFILE.LM_ACCTNO,
      *                   LMFILE.LM_DATE_REVERSED,
      *                   LMFILE.LM_SEQ,
      *                   LMFILE.LM_MEMO
      *      FROM DBO.LMFILE  LMFILE
      *      WHERE LMFILE.LM_BRNO   = @QLM1_WBEG_BRNO
      *        AND LMFILE.LM_ACCTNO = @QLM1_WBEG_ACCTNO
      *      ORDER BY LMFILE.LM_ACCTNO,
      *               LMFILE.LM_DATE_REVERSED,
      *               LMFILE.LM_SEQ

           EXEC SQL SET CONNECTION LM1_CONNECT END-EXEC.

           IF ( LM1-CURSOR-STAT-GOOD )
              EXEC SQL
                FETCH LM1_CURSOR INTO 
                  :QLM-BRNO,
                  :QLM-ACCTNO,
                  :QLM-DATE-REVERSED,
                  :QLM-SEQ,
                  :QLM-MEMO
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           EXEC SQL SET CONNECTION C1 END-EXEC.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-LM1-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-LM-FIELDS.

      *-----------------------------------------------------------------
       CLOSE-LM1-FILE.

           PERFORM CLOSE-IT.
            IF ( LM1-CURSOR-STAT-GOOD )
              EXEC SQL SET CONNECTION LM1_CONNECT END-EXEC
              EXEC SQL
                   CLOSE LM1_CURSOR
              END-EXEC
              EXEC SQL SET CONNECTION C1 END-EXEC
              MOVE STAT---BAD TO LM1-CURSOR-STAT.

      *-----------------------------------------------------------------
       CLOSE-LM1-CONNECTION.

           IF LM1-CONN-OPEN-SW = "Y"
               EXEC SQL SET CONNECTION LM1_CONNECT END-EXEC
               EXEC SQL COMMIT END-EXEC
               EXEC SQL DISCONNECT LM1_CONNECT END-EXEC
               EXEC SQL SET CONNECTION C1 END-EXEC
               MOVE "N" TO LM1-CONN-OPEN-SW.



      * COPY "LIBLP/LPLN3RN.CPY".
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

           IF LN3-CONN-OPEN-SW = "N" 
               EXEC SQL CONNECT TO :SQL-CONNECT-SERVER AS LN3_CONNECT
                    USER :SQL-CONNECT-USERID USING :SQL-CONNECT-PSWD
               END-EXEC
               MOVE "Y" TO LN3-CONN-OPEN-SW
               EXEC SQL SET CONNECTION C1 END-EXEC.

      *-----------------------------------------------------------------
       START-LN3-FILE.   *> SSP - LN, BW, NO
           PERFORM START-IT.
           MOVE LN-PATH-OWNBR  TO LN-OWNBR.
           MOVE LN-PATH-SQL    TO E-FILE.
           MOVE SPACES         TO E-KEYX.
           STRING LN-OWNBR, LN-CLASS,LN-ACCTNO
              DELIMITED BY " " INTO E-KEYX.

           IF ( LN3-CURSOR-STAT-GOOD )
              PERFORM CLOSE-LN3-FILE.

           EXEC SQL SET CONNECTION LN3_CONNECT END-EXEC.

      *     SELECT LNFILE.LN_OWNBR,
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
      *            LNFILE.LN_ORGBR,
      *            LNFILE.LN_OWNST,
      *            LNFILE.LN_SPRCLASS,
      *            LNFILE.LN_CONVERCD,
      *            LNFILE.LN_MIDSTREAM,
      *            LNFILE.LN_PURCD,
      *            LNFILE.LN_SRCD,
      *            LNFILE.LN_COLLCD,
      *            LNFILE.LN_COLLID,
      *            LNFILE.LN_POCD,
      *            LNFILE.LN_PLCD,
      *            LNFILE.LN_PLTYPE,
      *            LNFILE.LN_CONFIDENTCD,
      *            LNFILE.LN_ALLOTCD,
      *            LNFILE.LN_RECASTCD,
      *            LNFILE.LN_SKIP_TRW,
      *            LNFILE.LN_STATUSFG,
      *            LNFILE.LN_SMALLOAN,
      *            LNFILE.LN_SALEFIN,
      *            LNFILE.LN_SERVACCT,
      *            LNFILE.LN_LOANTYPE,
      *            LNFILE.LN_ADDON_FG,
      *            LNFILE.LN_PAY_SCHLD_FG,
      *            LNFILE.LN_LC_CODE,
      *            LNFILE.LN_CAN_LOAN_NO,
      *            LNFILE.LN_STATFLAGS,
      *            CAST(LNFILE.LN_ENTDATE AS VARCHAR(10)),
      *            CAST(LNFILE.LN_LOANDATE AS VARCHAR(10)),
      *            CAST(LNFILE.LN_PURDATE AS VARCHAR(10)),
      *            CAST(LNFILE.LN_INTDATE AS VARCHAR(10)),
      *            CAST(LNFILE.LN_CASHDATE AS VARCHAR(10)),
      *            CAST(LNFILE.LN_FILEDATE AS VARCHAR(10)),
      *            CAST(LNFILE.LN_RESCISSION_DATE AS VARCHAR(10)),
      *            CAST(LNFILE.LN_PART_RECOURSE_DATE AS VARCHAR(10)),
      *            CAST(LNFILE.LN_ORIG_1STPYDATE AS VARCHAR(10)),
      *            CAST(LNFILE.LN_1STPYDATE AS VARCHAR(10)),
      *            CAST(LNFILE.LN_JDDATE AS VARCHAR(10)),
      *            CAST(LNFILE.LN_PLDATE AS VARCHAR(10)),
      *            CAST(LNFILE.LN_PL_TRDATE AS VARCHAR(10)),
      *            CAST(LNFILE.LN_ACCELDATE AS VARCHAR(10)),
      *            CAST(LNFILE.LN_REDUDATE  AS VARCHAR(10)),
      *            CAST(LNFILE.LN_POFFDATE  AS VARCHAR(10)),
      *            CAST(LNFILE.LN_PO_TRDATE AS VARCHAR(10)),
      *            CAST(LNFILE.LN_LEGALDATE AS VARCHAR(10)),
      *            CAST(LNFILE.LN_BNKRPTDATE AS VARCHAR(10)),
      *            CAST(LNFILE.LN_BNKRPT_DISMISS_DATE AS VARCHAR(10)),
      *            CAST(LNFILE.LN_BNKRPT_DISCHARGE_DATE AS VARCHAR(10)),
      *            CAST(LNFILE.LN_BNKRPT_REAFFIRM_DATE AS VARCHAR(10)),
      *            CAST(LNFILE.LN_DEBTAIDATE AS VARCHAR(10)),
      *            CAST(LNFILE.LN_REPODATE   AS VARCHAR(10)),
      *            CAST(LNFILE.LN_REDEEMDATE AS VARCHAR(10)),
      *            CAST(LNFILE.LN_SOLDDATE   AS VARCHAR(10)),
      *            CAST(LNFILE.LN_INS_CONFIRM_DATE AS VARCHAR(10)),
      *            CAST(LNFILE.LN_PRIOR_RN_FOR10_DATE AS VARCHAR(10)),
      *            CAST(LNFILE.LN_LAST_RN_FOR10_DATE  AS VARCHAR(10)),
      *            CAST(LNFILE.LN_TRW_DOFD_DATE AS VARCHAR(10)),
      *            CAST(LNFILE.LN_LAST_PYPA_DATE AS VARCHAR(10)),
      *            CAST(LNFILE.LN_BT_TRDATE AS VARCHAR(10)),
      *            CAST(LNFILE.LN_METRO2_LAST_PMT_DATE AS VARCHAR(10)),
      *            CAST(LNFILE.LN_FOLLOWUP_DATE AS VARCHAR(10)),
      *            CAST(LNFILE.LN_LAST_ACCRUAL_DATE AS VARCHAR(10)),
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
      *            LNFILE.LN_STATE_FLAG,
      *            LNFILE.LN_STATE_WORKER,
      *            LNFILE.LN_STATE_WORKER2,
      *            LNFILE.LN_FEECODE_1,
      *            LNFILE.LN_FEEAMT_1,
      *            LNFILE.LN_FEEOURS_1,
      *            LNFILE.LN_FEEFINCHG_1,
      *            LNFILE.LN_FEECODE_2,
      *            LNFILE.LN_FEEAMT_2,
      *            LNFILE.LN_FEEOURS_2,
      *            LNFILE.LN_FEEFINCHG_2,
      *            LNFILE.LN_FEECODE_3,
      *            LNFILE.LN_FEEAMT_3,
      *            LNFILE.LN_FEEOURS_3,
      *            LNFILE.LN_FEEFINCHG_3,
      *            LNFILE.LN_FEECODE_4,
      *            LNFILE.LN_FEEAMT_4,
      *            LNFILE.LN_FEEOURS_4,
      *            LNFILE.LN_FEEFINCHG_4,
      *            LNFILE.LN_FEECODE_5,
      *            LNFILE.LN_FEEAMT_5,
      *            LNFILE.LN_FEEOURS_5,
      *            LNFILE.LN_FEEFINCHG_5,
      *            LNFILE.LN_FEECODE_6,
      *            LNFILE.LN_FEEAMT_6,
      *            LNFILE.LN_FEEOURS_6,
      *            LNFILE.LN_FEEFINCHG_6,
      *            LNFILE.LN_FEECODE_7,
      *            LNFILE.LN_FEEAMT_7,
      *            LNFILE.LN_FEEOURS_7,
      *            LNFILE.LN_FEEFINCHG_7,
      *            LNFILE.LN_FEECODE_8,
      *            LNFILE.LN_FEEAMT_8,
      *            LNFILE.LN_FEEOURS_8,
      *            LNFILE.LN_FEEFINCHG_8,
      *            LNFILE.LN_FEECODE_9,
      *            LNFILE.LN_FEEAMT_9,
      *            LNFILE.LN_FEEOURS_9,
      *            LNFILE.LN_FEEFINCHG_9,
      *            LNFILE.LN_FEECODE_10,
      *            LNFILE.LN_FEEAMT_10,
      *            LNFILE.LN_FEEOURS_10,
      *            LNFILE.LN_FEEFINCHG_10,
      *            LNFILE.LN_FEECODE_11,
      *            LNFILE.LN_FEEAMT_11,
      *            LNFILE.LN_FEEOURS_11,
      *            LNFILE.LN_FEEFINCHG_11,
      *            LNFILE.LN_FEECODE_12,
      *            LNFILE.LN_FEEAMT_12,
      *            LNFILE.LN_FEEOURS_12,
      *            LNFILE.LN_FEEFINCHG_12,
      *            LNFILE.LN_FEECODE_13,
      *            LNFILE.LN_FEEAMT_13,
      *            LNFILE.LN_FEEOURS_13,
      *            LNFILE.LN_FEEFINCHG_13,
      *            LNFILE.LN_FEECODE_14,
      *            LNFILE.LN_FEEAMT_14,
      *            LNFILE.LN_FEEOURS_14,
      *            LNFILE.LN_FEEFINCHG_14,
      *            LNFILE.LN_FEECODE_15,
      *            LNFILE.LN_FEEAMT_15,
      *            LNFILE.LN_FEEOURS_15,
      *            LNFILE.LN_FEEFINCHG_15,
      *            LNFILE.LN_FEECODE_16,
      *            LNFILE.LN_FEEAMT_16,
      *            LNFILE.LN_FEEOURS_16,
      *            LNFILE.LN_FEEFINCHG_16,
      *            LNFILE.LN_FEECODE_17,
      *            LNFILE.LN_FEEAMT_17,
      *            LNFILE.LN_FEEOURS_17,
      *            LNFILE.LN_FEEFINCHG_17,
      *            LNFILE.LN_FEECODE_18,
      *            LNFILE.LN_FEEAMT_18,
      *            LNFILE.LN_FEEOURS_18,
      *            LNFILE.LN_FEEFINCHG_18,
      *            LNFILE.LN_FEECODE_19,
      *            LNFILE.LN_FEEAMT_19,
      *            LNFILE.LN_FEEOURS_19,
      *            LNFILE.LN_FEEFINCHG_19,
      *            LNFILE.LN_FEECODE_20,
      *            LNFILE.LN_FEEAMT_20,
      *            LNFILE.LN_FEEOURS_20,
      *            LNFILE.LN_FEEFINCHG_20,
      *            LNFILE.LN_INSURED,
      *            LNFILE.LN_INSTYPES,
      *            LNFILE.LN_INSELIGIBLE,
      *            LNFILE.LN_INSCOMP_1,
      *            LNFILE.LN_INSOURS_1,
      *            CAST(LNFILE.LN_INSEFF_DATE_1 AS VARCHAR(10)),
      *            CAST(LNFILE.LN_INSEXP_DATE_1 AS VARCHAR(10)),
      *            LNFILE.LN_INSPREM_1,
      *            LNFILE.LN_INSCOVR_1,
      *            LNFILE.LN_INSCOMM_1,
      *            LNFILE.LN_INSANTICERN_1,
      *            LNFILE.LN_INSANTICADJ_1,
      *            LNFILE.LN_INSUFERN_1,
      *            LNFILE.LN_INSACCERN_1,
      *            LNFILE.LN_INSEARN_ORDER_1,
      *            LNFILE.LN_INSFINCHG_1,
      *            LNFILE.LN_INSCOMP_2,
      *            LNFILE.LN_INSOURS_2,
      *            CAST(LNFILE.LN_INSEFF_DATE_2 AS VARCHAR(10)),
      *            CAST(LNFILE.LN_INSEXP_DATE_2 AS VARCHAR(10)),
      *            LNFILE.LN_INSPREM_2,
      *            LNFILE.LN_INSCOVR_2,
      *            LNFILE.LN_INSCOMM_2,
      *            LNFILE.LN_INSANTICERN_2,
      *            LNFILE.LN_INSANTICADJ_2,
      *            LNFILE.LN_INSUFERN_2,
      *            LNFILE.LN_INSACCERN_2,
      *            LNFILE.LN_INSEARN_ORDER_2,
      *            LNFILE.LN_INSFINCHG_2,
      *            LNFILE.LN_INSCOMP_3,
      *            LNFILE.LN_INSOURS_3,
      *            CAST(LNFILE.LN_INSEFF_DATE_3 AS VARCHAR(10)),
      *            CAST(LNFILE.LN_INSEXP_DATE_3 AS VARCHAR(10)),
      *            LNFILE.LN_INSPREM_3,
      *            LNFILE.LN_INSCOVR_3,
      *            LNFILE.LN_INSCOMM_3,
      *            LNFILE.LN_INSANTICERN_3,
      *            LNFILE.LN_INSANTICADJ_3,
      *            LNFILE.LN_INSUFERN_3,
      *            LNFILE.LN_INSACCERN_3,
      *            LNFILE.LN_INSEARN_ORDER_3,
      *            LNFILE.LN_INSFINCHG_3,
      *            LNFILE.LN_INSCOMP_4,
      *            LNFILE.LN_INSOURS_4,
      *            CAST(LNFILE.LN_INSEFF_DATE_4 AS VARCHAR(10)),
      *            CAST(LNFILE.LN_INSEXP_DATE_4 AS VARCHAR(10)),
      *            LNFILE.LN_INSPREM_4,
      *            LNFILE.LN_INSCOVR_4,
      *            LNFILE.LN_INSCOMM_4,
      *            LNFILE.LN_INSANTICERN_4,
      *            LNFILE.LN_INSANTICADJ_4,
      *            LNFILE.LN_INSUFERN_4,
      *            LNFILE.LN_INSACCERN_4,
      *            LNFILE.LN_INSEARN_ORDER_4,
      *            LNFILE.LN_INSFINCHG_4,
      *            LNFILE.LN_INSCOMP_5,
      *            LNFILE.LN_INSOURS_5,
      *            CAST(LNFILE.LN_INSEFF_DATE_5 AS VARCHAR(10)),
      *            CAST(LNFILE.LN_INSEXP_DATE_5 AS VARCHAR(10)),
      *            LNFILE.LN_INSPREM_5,
      *            LNFILE.LN_INSCOVR_5,
      *            LNFILE.LN_INSCOMM_5,
      *            LNFILE.LN_INSANTICERN_5,
      *            LNFILE.LN_INSANTICADJ_5,
      *            LNFILE.LN_INSUFERN_5,
      *            LNFILE.LN_INSACCERN_5,
      *            LNFILE.LN_INSEARN_ORDER_5,
      *            LNFILE.LN_INSFINCHG_5,
      *            LNFILE.LN_INSCOMP_6,
      *            LNFILE.LN_INSOURS_6,
      *            CAST(LNFILE.LN_INSEFF_DATE_6 AS VARCHAR(10)),
      *            CAST(LNFILE.LN_INSEXP_DATE_6 AS VARCHAR(10)),
      *            LNFILE.LN_INSPREM_6,
      *            LNFILE.LN_INSCOVR_6,
      *            LNFILE.LN_INSCOMM_6,
      *            LNFILE.LN_INSANTICERN_6,
      *            LNFILE.LN_INSANTICADJ_6,
      *            LNFILE.LN_INSUFERN_6,
      *            LNFILE.LN_INSACCERN_6,
      *            LNFILE.LN_INSEARN_ORDER_6,
      *            LNFILE.LN_INSFINCHG_6,
      *            LNFILE.LN_INSCOMP_7,
      *            LNFILE.LN_INSOURS_7,
      *            CAST(LNFILE.LN_INSEFF_DATE_7 AS VARCHAR(10)),
      *            CAST(LNFILE.LN_INSEXP_DATE_7 AS VARCHAR(10)),
      *            LNFILE.LN_INSPREM_7,
      *            LNFILE.LN_INSCOVR_7,
      *            LNFILE.LN_INSCOMM_7,
      *            LNFILE.LN_INSANTICERN_7,
      *            LNFILE.LN_INSANTICADJ_7,
      *            LNFILE.LN_INSUFERN_7,
      *            LNFILE.LN_INSACCERN_7,
      *            LNFILE.LN_INSEARN_ORDER_7,
      *            LNFILE.LN_INSFINCHG_7,
      *            LNFILE.LN_INSCOMP_8,
      *            LNFILE.LN_INSOURS_8,
      *            CAST(LNFILE.LN_INSEFF_DATE_8 AS VARCHAR(10)),
      *            CAST(LNFILE.LN_INSEXP_DATE_8 AS VARCHAR(10)),
      *            LNFILE.LN_INSPREM_8,
      *            LNFILE.LN_INSCOVR_8,
      *            LNFILE.LN_INSCOMM_8,
      *            LNFILE.LN_INSANTICERN_8,
      *            LNFILE.LN_INSANTICADJ_8,
      *            LNFILE.LN_INSUFERN_8,
      *            LNFILE.LN_INSACCERN_8,
      *            LNFILE.LN_INSEARN_ORDER_8,
      *            LNFILE.LN_INSFINCHG_8,
      *            LNFILE.LN_INSCOMP_9,
      *            LNFILE.LN_INSOURS_9,
      *            CAST(LNFILE.LN_INSEFF_DATE_9 AS VARCHAR(10)),
      *            CAST(LNFILE.LN_INSEXP_DATE_9 AS VARCHAR(10)),
      *            LNFILE.LN_INSPREM_9,
      *            LNFILE.LN_INSCOVR_9,
      *            LNFILE.LN_INSCOMM_9,
      *            LNFILE.LN_INSANTICERN_9,
      *            LNFILE.LN_INSANTICADJ_9,
      *            LNFILE.LN_INSUFERN_9,
      *            LNFILE.LN_INSACCERN_9,
      *            LNFILE.LN_INSEARN_ORDER_9,
      *            LNFILE.LN_INSFINCHG_9,
      *            LNFILE.LN_INSCOMP_10,
      *            LNFILE.LN_INSOURS_10,
      *            CAST(LNFILE.LN_INSEFF_DATE_10 AS VARCHAR(10)),
      *            CAST(LNFILE.LN_INSEXP_DATE_10 AS VARCHAR(10)),
      *            LNFILE.LN_INSPREM_10,
      *            LNFILE.LN_INSCOVR_10,
      *            LNFILE.LN_INSCOMM_10,
      *            LNFILE.LN_INSANTICERN_10,
      *            LNFILE.LN_INSANTICADJ_10,
      *            LNFILE.LN_INSUFERN_10,
      *            LNFILE.LN_INSACCERN_10,
      *            LNFILE.LN_INSEARN_ORDER_10,
      *            LNFILE.LN_INSFINCHG_10,
      *            LNFILE.LN_PY_YYYYMM_RECDEL,
      *            LNFILE.LN_MTD_PAYS_RECDEL,
      *            LNFILE.LN_GARNISH_EXTR,
      *            LNFILE.LN_SHIP_SAME_FG,
      *            LNFILE.LN_AD_D_INS_COVR,
      *            LNFILE.LN_TOT_PARTIAL_CHGOFF,
      *            LNFILE.LN_LPO,
      *            LNFILE.LN_REP_CD,
      *            LNFILE.LN_EMP_ID,
      *            LNFILE.LN_PROCESS_CD,
      *            LNFILE.LN_WCBC_CREDITLIM,
      *            LNFILE.LN_FOLLOWUP_CD,
      *            LNFILE.LN_MIP_FG,
      *            LNFILE.LN_MIP_MTD_CLAHPAID,
      *            CAST(LNFILE.LN_MIP_CLPDTH_DATE AS VARCHAR(10)),
      *            LNFILE.LN_MIP_TOT_CLPAID,
      *            CAST(LNFILE.LN_MIP_AHPDTH_DATE AS VARCHAR(10)),
      *            LNFILE.LN_MIP_TOT_AHPAID,
      *            LNFILE.LN_IL_GROSS_INCOME,
      *            LNFILE.LN_IL_GROSS_INCOME2,
      *            LNFILE.LN_RESCISSION,
      *            LNFILE.LN_INTERIM_TRW,
      *            LNFILE.LN_DLNO,
      *            LNFILE.LN_DLPOOL,
      *            LNFILE.LN_DLRECOURSE,
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
      *            CAST(LNFILE.LN_LCPDTH_DATE AS VARCHAR(10)),
      *            CAST(LNFILE.LN_INTPDTH_DATE AS VARCHAR(10)),
      *            LNFILE.LN_CURBAL,
      *            LNFILE.LN_INTBAL,
      *            LNFILE.LN_LCBAL,
      *            LNFILE.LN_OTHBAL,
      *            LNFILE.LN_OT2BAL,
      *            LNFILE.LN_LCPAID,
      *            LNFILE.LN_LCPARTIALS,
      *            LNFILE.LN_STMT_COUNTER,
      *            LNFILE.LN_PRIOR_STMT_COUNTER,
      *            LNFILE.LN_FROZEN_REASON,
      *            LNFILE.LN_COUPON_STATEMENT,
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
      *            LNFILE.LN_ERNDELQ_STOP,
      *            LNFILE.LN_NONACRERN,
      *            LNFILE.LN_PRIOR_NONACRERN,
      *            CAST(LNFILE.LN_ERN_IB_PDTH_DATE AS VARCHAR(10)),
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
      *            LNFILE.LN_PLREASON,
      *            LNFILE.LN_COLLECTOR,
      *            LNFILE.LN_ACTIONCD,
      *            LNFILE.LN_ACTIONCD2,
      *            LNFILE.LN_ATCODE,
      *            LNFILE.LN_RECODE,
      *            LNFILE.LN_ESCROW_FG,
      *            LNFILE.LN_PROMDAYS_PASTDUE,
      *            LNFILE.LN_PROMID,
      *            CAST(LNFILE.LN_PROMDATE AS VARCHAR(10)),
      *            LNFILE.LN_PROMAMT,
      *            CAST(LNFILE.LN_PROMDATE2 AS VARCHAR(10)),
      *            CAST(LNFILE.LN_DATE_PAID_LAST AS VARCHAR(10)),
      *            LNFILE.LN_AMT_PAID_LAST,
      *            LNFILE.LN_LASTCFORMNO,
      *            CAST(LNFILE.LN_LASTCFORMDATE AS VARCHAR(10)),
      *            LNFILE.LN_DELCNT,
      *            LNFILE.LN_DELCNT2,
      *            LNFILE.LN_CREDITLIM,
      *            LNFILE.LN_CRSCORE,
      *            LNFILE.LN_LASTSFORMNO,
      *            CAST(LNFILE.LN_LASTSFORMDATE AS VARCHAR(10)),
      *            LNFILE.LN_SOLCNT,
      *            LNFILE.LN_MESSAGE,
      *            LNFILE.LN_OLDACCTNO,
      *            LNFILE.LN_PRLNNO,
      *            LNFILE.LN_PRBAL,
      *            LNFILE.LN_PRINTDUE,
      *            LNFILE.LN_PRFINCHGRB,
      *            LNFILE.LN_PRINSRB,
      *            LNFILE.LN_PRPOCD,
      *            CAST(LNFILE.LN_PRPODATE AS VARCHAR(10)),
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
      *            LNFILE.LN_UNITPER_CD,
      *            LNFILE.LN_UNITPER_FREQ,
      *            LNFILE.LN_DAYS_IN_UNITPER,
      *            LNFILE.LN_UNITPER_PER_YEAR,
      *            LNFILE.LN_REPOCD,
      *            LNFILE.LN_BR_TFR_FG,
      *            LNFILE.LN_O3INS_DLPROC,
      *            LNFILE.LN_INSPREM_DLADJ,
      *            LNFILE.LN_PLAMT,
      *            LNFILE.LN_MTD_PRIN,
      *            LNFILE.LN_MTD_LC,
      *            LNFILE.LN_MTD_INT,
      *            LNFILE.LN_MTD_OT,
      *            LNFILE.LN_MTD_EX,
      *            LNFILE.LN_BNKRPT_CHAPTER,
      *            LNFILE.LN_BMO_CON_STATUS,
      *            LNFILE.LN_BMO_REC_STATUS,
      *            LNFILE.LN_APP_NUMBER,
      *            LNFILE.LN_GIFTNET,
      *            LNFILE.LN_GIFTNET_OUR_PO,
      *            LNFILE.LN_PROC_DISBURSED,
      *            LNFILE.LN_NOODDPY,
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
      *            LNFILE.LN_TRW_STATUS,
      *            LNFILE.LN_ORIGIN,
      *            LNFILE.LN_PO_TRCD,
      *            LNFILE.LN_NEXIS_CRSCORE,
      *            LNFILE.LN_TRW_B_CONSUMER_IND,
      *            LNFILE.LN_PARVHELD_PAID_FG,
      *            LNFILE.LN_ACCT_FROZEN,
      *            LNFILE.LN_TRW_SPECIAL_CODE,
      *            LNFILE.LN_CREATION_STATUS,
      *            LNFILE.LN_TOTNOLCHG,
      *            LNFILE.LN_LBOX,
      *            LNFILE.LN_EXTRA_PRIN_LFP,
      *            LNFILE.LN_REMAIN_INT_SINCE_LFP,
      *            LNFILE.LN_REMAIN_PRIN_SINCE_LFP,
      *            LNFILE.LN_INTERIM_COUPON,
      *            LNFILE.LN_DLCOMM_O3_CMSI,
      *            LNFILE.LN_DLCOMM_PP_CMSI,
      *            LNFILE.LN_TRW_J_CONSUMER_IND,
      *            LNFILE.LN_ACH,
      *            LNFILE.LN_1099_C_FG,
      *            LNFILE.LN_TRW_COMPLIANCE_CODE,
      *            LNFILE.LN_UNWIND_REASON_CODE,
      *            LNFILE.LN_PBI_PBD_FG,
      *            LNFILE.LN_OLD_ACCT_FORMAT_FG,
      *            LNFILE.LN_APPROVED_BY_USER,
      *            LNFILE.LN_APPROVED_BY_CODE,
      *            CAST(LNFILE.LN_APPROVED_BY_DATE AS VARCHAR(10)),
      *            LNFILE.LN_APPROVED_BY_TIME,
      *            CAST(LNFILE.LN_LTOUCH_DATE AS VARCHAR(10)),
      *            LNFILE.LN_LTOUCH_TIME, 
      *            ISNULL(BWFILE.BW_OWNBR,0),
      *            ISNULL(BWFILE.BW_SSNO,0),
      *            ISNULL(BWFILE.BW_LNAME,''''),
      *            ISNULL(BWFILE.BW_FNAME,''''),
      *            ISNULL(BWFILE.BW_CONFIDENTIAL,''''),
      *            ISNULL(NOFILE.NO_BRNO, 0),
      *            ISNULL(NOFILE.NO_ACCTNO, 0),
      *            ISNULL(CAST(NOFILE.NO_LTOUCH_DATE
      *              AS VARCHAR(10)),''1900-01-01''),
      *            ISNULL(CAST(NOFILE.NO_TDUE_DATE
      *              AS VARCHAR(10)),''1900-01-01''),
      *            ISNULL(NOFILE.NO_TDUELC, 0),
      *            ISNULL(NOFILE.NO_TDUEAMT, 0),
      *            ISNULL(NOFILE.NO_TDUEPRIN, 0),
      *            ISNULL(NOFILE.NO_TDUETBAL, 0),
      *            ISNULL(NOFILE.NO_TDUEPOFF, 0),
      *            ISNULL(CAST(NOFILE.NO_LDUE_DATE
      *              AS VARCHAR(10)),''1900-01-01''),
      *            ISNULL(NOFILE.NO_LDUELC, 0),
      *            ISNULL(NOFILE.NO_LDUEAMT, 0),
      *            ISNULL(CAST(NOFILE.NO_DATE_PAID_LAST
      *              AS VARCHAR(10)),''1900-01-01''),
      *            ISNULL(NOFILE.NO_LSTLPKEY,''''),
      *            ISNULL(NOFILE.NO_AMT_PAID_LAST, 0),
      *            ISNULL(CAST(NOFILE.NO_NXDUE_DATE
      *              AS VARCHAR(10)),''1900-01-01''),
      *            ISNULL(NOFILE.NO_NXDUEAMT, 0),
      *            ISNULL(NOFILE.NO_NXDUELC, 0),
      *            ISNULL(NOFILE.NO_TRUCONTR, 0),
      *            ISNULL(NOFILE.NO_RECENCY, 0),
      *            ISNULL(CAST(NOFILE.NO_PDTH_DATE
      *              AS VARCHAR(10)),''1900-01-01''),
      *            ISNULL(CAST(NOFILE.NO_LCPDTH_DATE
      *              AS VARCHAR(10)),''1900-01-01''),
      *            ISNULL(NOFILE.NO_LCHG_INTDUE, 0),
      *            ISNULL(NOFILE.NO_AVAILCREDIT, 0),
      *            ISNULL(NOFILE.NO_TDUEDELINQ, 0),
      *            ISNULL(NOFILE.NO_DEFERMENT, 0),
      *            ISNULL(CAST(NOFILE.NO_TRUE_DUE_DATE
      *              AS VARCHAR(10)),''1900-01-01''),
      *            ISNULL(NOFILE.NO_SCHED_PAYMENT, 0),
      *            ISNULL(NOFILE.NO_TDUECONTR, 0),
      *            ISNULL(NOFILE.NO_LDUECONTR, 0),
      *            ISNULL(NOFILE.NO_NXDUECONTR, 0),
      *            ISNULL(NOFILE.NO_CURRENT_AVAIL, 0),
      *            ISNULL(NOFILE.NO_LATE_CHG, 0),
      *            ISNULL(NOFILE.NO_PAST_DUE_DAYS, 0),
      *            ISNULL(NOFILE.NO_RECDEL, 0),
      *            ISNULL(NOFILE.NO_AVCR_RESTRICT_FG,''''),
      *            ISNULL(NOFILE.NO_INDU_PENALTY, 0), 
      *            ISNULL(NOFILE.NO_CPOT30,''''), 
      *            ISNULL(NOFILE.NO_RDPOT30,''''), 
      *            ISNULL(NOFILE.NO_CPOTCHGOFF,''''),
      *            ISNULL(NOFILE.NO_RDPOTCHGOFF,''''),
      *            ISNULL(NOFILE.NO_CSUB, 0),
      *            ISNULL(NOFILE.NO_RSUB, 0)
      *
      *  FROM DBO.LNFILE LNFILE
      *  LEFT OUTER JOIN DBO.BWFILE BWFILE 
      *   ON BWFILE.BW_OWNBR = LNFILE.LN_OWNBR  
      *   AND BWFILE.BW_SSNO  = LNFILE.LN_SSNO1 
      *  LEFT OUTER JOIN DBO.NOFILE NOFILE 
      *    ON NOFILE.NO_BRNO   = LNFILE.LN_OWNBR  	
      *   AND NOFILE.NO_ACCTNO = LNFILE.LN_ACCTNO  	  
      * WHERE LNFILE.LN_OWNBR = @QLN3_WBEG_OWNBR
      *   AND LNFILE.LN_CLASS BETWEEN @QLN3_WBEG_CLASS AND @QLN3_WEND_CLASS
      *   AND LNFILE.LN_POCD NOT IN (''**'', ''*I'', ''*R'', ''*U'') '
      * IF @ZBAL = 'O'
      *   SELECT @SQL += ' AND (LN_CURBAL = 0 AND LN_INTBAL = 0 AND LN_LCBAL = 0 AND LN_OTHBAL = 0)'  
      * IF @ZBAL = 'S'
      *   SELECT @SQL += ' AND (LN_CURBAL <> 0 OR LN_INTBAL <> 0 OR LN_LCBAL <> 0 OR LN_OTHBAL <> 0)'  
      * IF @PL = 'O'  SELECT @SQL += ' AND (LN_PLDATE > ''1900-01-01'')'  
      * IF @PL = 'S'  SELECT @SQL += ' AND (LN_PLDATE = ''1900-01-01'')'  
      * IF @BANK = 'O' SELECT @SQL += ' AND (LNFILE.LN_BNKRPTDATE > ''1900-01-01'')'  
      * IF @BANK = 'S' SELECT @SQL += ' AND (LNFILE.LN_BNKRPTDATE = ''1900-01-01'')'  
      * IF @PROMISED = 'O' SELECT @SQL += ' AND (LNFILE.LN_PROMDATE > ''1900-01-01'')'  
      * IF @PROMISED = 'S' SELECT @SQL += ' AND (LNFILE.LN_PROMDATE = ''1900-01-01'')'  
      * IF @NO1STPY = 'O' SELECT @SQL += ' AND (LNFILE.LN_MIDSTREAM = ''Y'' OR LNFILE.LN_DATE_PAID_LAST = ''1900-01-01'')'  
      * IF @NO1STPY = 'S' SELECT @SQL += ' AND (LNFILE.LN_MIDSTREAM = ''Y'' OR LNFILE.LN_DATE_PAID_LAST > ''1900-01-01'')'   
      * IF @OWNED = 'O' SELECT @SQL += ' AND (LNFILE.LN_SERVACCT != ''Y'')'  
      * IF @OWNED = 'S' SELECT @SQL += ' AND (LNFILE.LN_SERVACCT != ''N'')'  
      * IF @PLACTIVE = 'A' SELECT @SQL += ' AND (LNFILE.LN_PLCD = ''P'' OR LNFILE.LN_PLDATE = ''1900-01-01'')'  
      * IF @FROZEN = 'O' SELECT @SQL += ' AND (LNFILE.LN_ACCT_FROZEN = ''Y'')'  
      * IF @FROZEN = 'S' SELECT @SQL += ' AND (LNFILE.LN_ACCT_FROZEN != ''Y'')'  
      * IF @REPO = 'O' SELECT @SQL += ' AND (LNFILE.LN_REPOCD IN (''X'', ''S''))'  
      * IF @REPO = 'S' SELECT @SQL += ' AND (LNFILE.LN_REPOCD NOT IN (''X'', ''S''))'
      * IF @LEGL = 'O' SELECT @SQL += ' AND (LNFILE.LN_LEGALDATE > ''1900-01-01'')'   
      * IF @LEGL = 'S' SELECT @SQL += ' AND (LNFILE.LN_LEGALDATE = ''1900-01-01'')' 
      * IF @DEBT = 'O' SELECT @SQL += ' AND (LNFILE.LN_DEBTAIDATE > ''1900-01-01'')'   
      * IF @DEBT = 'S' SELECT @SQL += ' AND (LNFILE.LN_DEBTAIDATE = ''1900-01-01'')' 
      *   ORDER BY LNFILE.LN_OWNBR, LNFILE.LN_COLLECTOR

           EXEC SQL
            DECLARE LN3_CURSOR CURSOR FOR
                :LN3-RETURN-CODE = 
                 EXEC SSP_CPCORP_LNFILE_SELECT
                  ( :QLN3-WBEG-OWNBR, :QLN3-WBEG-CLASS, 
                    :QLN3-WEND-CLASS, :PASS-ZBAL, :PASS-PL,
                    :PASS-BANK, :PASS-PROMISED, :PASS-NO1STPY,
                    :PASS-OWNED, :PASS-PLACTIVE, :PASS-FROZEN,
                    :PASS-REPO, :PASS-LEGL, :PASS-DEBT)
                END-EXEC.

           EXEC SQL
               OPEN LN3_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           EXEC SQL SET CONNECTION C1 END-EXEC.

           MOVE ZEROES TO QLN3-WBEG-OWNBR
                          QLN3-WBEG-CLASS
                          QLN3-WEND-CLASS.
 
           MOVE STAT---GOOD TO LN3-CURSOR-STAT.

      *-----------------------------------------------------------------
       READ-LN3-FILE-NEXT.   *> SSP - LN, BW, NO
           PERFORM READ-IT.
           MOVE LN-PATH-OWNBR  TO LN-OWNBR.
           MOVE LN-PATH-SQL    TO E-FILE.
           MOVE SPACES         TO E-KEYX.
           STRING LN-OWNBR, LN-CLASS,LN-ACCTNO
              DELIMITED BY " " INTO E-KEYX.

           INITIALIZE QLN-REC QBW-REC QNO-REC.

           EXEC SQL SET CONNECTION LN3_CONNECT END-EXEC.

           IF ( LN3-CURSOR-STAT-GOOD )
              EXEC SQL
                 FETCH LN3_CURSOR INTO 
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
                   :QLN-ORGBR,
                   :QLN-OWNST,
                   :QLN-SPRCLASS,
                   :QLN-CONVERCD,
                   :QLN-MIDSTREAM,
                   :QLN-PURCD,
                   :QLN-SRCD,
                   :QLN-COLLCD,
                   :QLN-COLLID,
                   :QLN-POCD,
                   :QLN-PLCD,
                   :QLN-PLTYPE,
                   :QLN-CONFIDENTCD,
                   :QLN-ALLOTCD,
                   :QLN-RECASTCD,
                   :QLN-SKIP-TRW,
                   :QLN-STATUSFG,
                   :QLN-SMALLOAN,
                   :QLN-SALEFIN,
                   :QLN-SERVACCT,
                   :QLN-LOANTYPE,
                   :QLN-ADDON-FG,
                   :QLN-PAY-SCHLD-FG,
                   :QLN-LC-CODE,
                   :QLN-CAN-LOAN-NO,
                   :QLN-STATFLAGS,
                   :QLN-ENTDATE  ,
                   :QLN-LOANDATE ,
                   :QLN-PURDATE ,
                   :QLN-INTDATE ,
                   :QLN-CASHDATE ,
                   :QLN-FILEDATE ,
                   :QLN-RESCISSION-DATE ,
                   :QLN-PART-RECOURSE-DATE ,
                   :QLN-ORIG-1STPYDATE ,
                   :QLN-1STPYDATE ,
                   :QLN-JDDATE ,
                   :QLN-PLDATE ,
                   :QLN-PL-TRDATE ,
                   :QLN-ACCELDATE ,
                   :QLN-REDUDATE  ,
                   :QLN-POFFDATE  ,
                   :QLN-PO-TRDATE ,
                   :QLN-LEGALDATE ,
                   :QLN-BNKRPTDATE ,
                   :QLN-BNKRPT-DISMISS-DATE ,
                   :QLN-BNKRPT-DISCHARGE-DATE,
                   :QLN-BNKRPT-REAFFIRM-DATE,
                   :QLN-DEBTAIDATE,
                   :QLN-REPODATE,
                   :QLN-REDEEMDATE,
                   :QLN-SOLDDATE,
                   :QLN-INS-CONFIRM-DATE,
                   :QLN-PRIOR-RN-FOR10-DATE,
                   :QLN-LAST-RN-FOR10-DATE,
                   :QLN-TRW-DOFD-DATE,
                   :QLN-LAST-PYPA-DATE,
                   :QLN-BT-TRDATE,
                   :QLN-METRO2-LAST-PMT-DATE,
                   :QLN-FOLLOWUP-DATE,
                   :QLN-LAST-ACCRUAL-DATE,
                   :QLN-APRATE,
                   :QLN-EFFRATE,
                   :QLN-SMPRATE,
                   :QLN-YLDRATE,
                   :QLN-OVRDRATE,
                   :QLN-ENDORATE,
                   :QLN-JDRATE,
                   :QLN-ACCRATE,
                   :QLN-REDURATE,
                   :QLN-CONRATE,
                   :QLN-SERVPER,
                   :QLN-SELLPR,
                   :QLN-DOWNPY,
                   :QLN-TRADEIN,
                   :QLN-LNAMT,
                   :QLN-FINCHG,
                   :QLN-INTCHG,
                   :QLN-SERCHG,
                   :QLN-EXTCHG,
                   :QLN-MAINTFEE,
                   :QLN-POINTS,
                   :QLN-PROCEEDS,
                   :QLN-ORGTERM,
                   :QLN-1STPYAMT,
                   :QLN-NOREGPY,
                   :QLN-REGPYAMT,
                   :QLN-LASTPYAMT,
                   :QLN-PROCASH,
                   :QLN-TILLNO,
                   :QLN-STATE-FLAG,
                   :QLN-STATE-WORKER,
                   :QLN-STATE-WORKER2,
                   :QLN-FEECODE-1,
                   :QLN-FEEAMT-1,
                   :QLN-FEEOURS-1,
                   :QLN-FEEFINCHG-1,
                   :QLN-FEECODE-2,
                   :QLN-FEEAMT-2,
                   :QLN-FEEOURS-2,
                   :QLN-FEEFINCHG-2,
                   :QLN-FEECODE-3,
                   :QLN-FEEAMT-3,
                   :QLN-FEEOURS-3,
                   :QLN-FEEFINCHG-3,
                   :QLN-FEECODE-4,
                   :QLN-FEEAMT-4,
                   :QLN-FEEOURS-4,
                   :QLN-FEEFINCHG-4,
                   :QLN-FEECODE-5,
                   :QLN-FEEAMT-5,
                   :QLN-FEEOURS-5,
                   :QLN-FEEFINCHG-5,
                   :QLN-FEECODE-6,
                   :QLN-FEEAMT-6,
                   :QLN-FEEOURS-6,
                   :QLN-FEEFINCHG-6,
                   :QLN-FEECODE-7,
                   :QLN-FEEAMT-7,
                   :QLN-FEEOURS-7,
                   :QLN-FEEFINCHG-7,
                   :QLN-FEECODE-8,
                   :QLN-FEEAMT-8,
                   :QLN-FEEOURS-8,
                   :QLN-FEEFINCHG-8,
                   :QLN-FEECODE-9,
                   :QLN-FEEAMT-9,
                   :QLN-FEEOURS-9,
                   :QLN-FEEFINCHG-9,
                   :QLN-FEECODE-10,
                   :QLN-FEEAMT-10,
                   :QLN-FEEOURS-10,
                   :QLN-FEEFINCHG-10,
                   :QLN-FEECODE-11,
                   :QLN-FEEAMT-11,
                   :QLN-FEEOURS-11,
                   :QLN-FEEFINCHG-11,
                   :QLN-FEECODE-12,
                   :QLN-FEEAMT-12,
                   :QLN-FEEOURS-12,
                   :QLN-FEEFINCHG-12,
                   :QLN-FEECODE-13,
                   :QLN-FEEAMT-13,
                   :QLN-FEEOURS-13,
                   :QLN-FEEFINCHG-13,
                   :QLN-FEECODE-14,
                   :QLN-FEEAMT-14,
                   :QLN-FEEOURS-14,
                   :QLN-FEEFINCHG-14,
                   :QLN-FEECODE-15,
                   :QLN-FEEAMT-15,
                   :QLN-FEEOURS-15,
                   :QLN-FEEFINCHG-15,
                   :QLN-FEECODE-16,
                   :QLN-FEEAMT-16,
                   :QLN-FEEOURS-16,
                   :QLN-FEEFINCHG-16,
                   :QLN-FEECODE-17,
                   :QLN-FEEAMT-17,
                   :QLN-FEEOURS-17,
                   :QLN-FEEFINCHG-17,
                   :QLN-FEECODE-18,
                   :QLN-FEEAMT-18,
                   :QLN-FEEOURS-18,
                   :QLN-FEEFINCHG-18,
                   :QLN-FEECODE-19,
                   :QLN-FEEAMT-19,
                   :QLN-FEEOURS-19,
                   :QLN-FEEFINCHG-19,
                   :QLN-FEECODE-20,
                   :QLN-FEEAMT-20,
                   :QLN-FEEOURS-20,
                   :QLN-FEEFINCHG-20,
                   :QLN-INSURED,
                   :QLN-INSTYPES,
                   :QLN-INSELIGIBLE,
                   :QLN-INSCOMP-1,
                   :QLN-INSOURS-1,
                   :QLN-INSEFF-DATE-1 ,
                   :QLN-INSEXP-DATE-1 ,
                   :QLN-INSPREM-1,
                   :QLN-INSCOVR-1,
                   :QLN-INSCOMM-1,
                   :QLN-INSANTICERN-1,
                   :QLN-INSANTICADJ-1,
                   :QLN-INSUFERN-1,
                   :QLN-INSACCERN-1,
                   :QLN-INSEARN-ORDER-1,
                   :QLN-INSFINCHG-1,
                   :QLN-INSCOMP-2,
                   :QLN-INSOURS-2,
                   :QLN-INSEFF-DATE-2 ,
                   :QLN-INSEXP-DATE-2 ,
                   :QLN-INSPREM-2,
                   :QLN-INSCOVR-2,
                   :QLN-INSCOMM-2,
                   :QLN-INSANTICERN-2,
                   :QLN-INSANTICADJ-2,
                   :QLN-INSUFERN-2,
                   :QLN-INSACCERN-2,
                   :QLN-INSEARN-ORDER-2,
                   :QLN-INSFINCHG-2,
                   :QLN-INSCOMP-3,
                   :QLN-INSOURS-3,
                   :QLN-INSEFF-DATE-3 ,
                   :QLN-INSEXP-DATE-3 ,
                   :QLN-INSPREM-3,
                   :QLN-INSCOVR-3,
                   :QLN-INSCOMM-3,
                   :QLN-INSANTICERN-3,
                   :QLN-INSANTICADJ-3,
                   :QLN-INSUFERN-3,
                   :QLN-INSACCERN-3,
                   :QLN-INSEARN-ORDER-3,
                   :QLN-INSFINCHG-3,
                   :QLN-INSCOMP-4,
                   :QLN-INSOURS-4,
                   :QLN-INSEFF-DATE-4 ,
                   :QLN-INSEXP-DATE-4 ,
                   :QLN-INSPREM-4,
                   :QLN-INSCOVR-4,
                   :QLN-INSCOMM-4,
                   :QLN-INSANTICERN-4,
                   :QLN-INSANTICADJ-4,
                   :QLN-INSUFERN-4,
                   :QLN-INSACCERN-4,
                   :QLN-INSEARN-ORDER-4,
                   :QLN-INSFINCHG-4,
                   :QLN-INSCOMP-5,
                   :QLN-INSOURS-5,
                   :QLN-INSEFF-DATE-5 ,
                   :QLN-INSEXP-DATE-5 ,
                   :QLN-INSPREM-5,
                   :QLN-INSCOVR-5,
                   :QLN-INSCOMM-5,
                   :QLN-INSANTICERN-5,
                   :QLN-INSANTICADJ-5,
                   :QLN-INSUFERN-5,
                   :QLN-INSACCERN-5,
                   :QLN-INSEARN-ORDER-5,
                   :QLN-INSFINCHG-5,
                   :QLN-INSCOMP-6,
                   :QLN-INSOURS-6,
                   :QLN-INSEFF-DATE-6 ,
                   :QLN-INSEXP-DATE-6 ,
                   :QLN-INSPREM-6,
                   :QLN-INSCOVR-6,
                   :QLN-INSCOMM-6,
                   :QLN-INSANTICERN-6,
                   :QLN-INSANTICADJ-6,
                   :QLN-INSUFERN-6,
                   :QLN-INSACCERN-6,
                   :QLN-INSEARN-ORDER-6,
                   :QLN-INSFINCHG-6,
                   :QLN-INSCOMP-7,
                   :QLN-INSOURS-7,
                   :QLN-INSEFF-DATE-7 ,
                   :QLN-INSEXP-DATE-7 ,
                   :QLN-INSPREM-7,
                   :QLN-INSCOVR-7,
                   :QLN-INSCOMM-7,
                   :QLN-INSANTICERN-7,
                   :QLN-INSANTICADJ-7,
                   :QLN-INSUFERN-7,
                   :QLN-INSACCERN-7,
                   :QLN-INSEARN-ORDER-7,
                   :QLN-INSFINCHG-7,
                   :QLN-INSCOMP-8,
                   :QLN-INSOURS-8,
                   :QLN-INSEFF-DATE-8 ,
                   :QLN-INSEXP-DATE-8 ,
                   :QLN-INSPREM-8,
                   :QLN-INSCOVR-8,
                   :QLN-INSCOMM-8,
                   :QLN-INSANTICERN-8,
                   :QLN-INSANTICADJ-8,
                   :QLN-INSUFERN-8,
                   :QLN-INSACCERN-8,
                   :QLN-INSEARN-ORDER-8,
                   :QLN-INSFINCHG-8,
                   :QLN-INSCOMP-9,
                   :QLN-INSOURS-9,
                   :QLN-INSEFF-DATE-9 ,
                   :QLN-INSEXP-DATE-9 ,
                   :QLN-INSPREM-9,
                   :QLN-INSCOVR-9,
                   :QLN-INSCOMM-9,
                   :QLN-INSANTICERN-9,
                   :QLN-INSANTICADJ-9,
                   :QLN-INSUFERN-9,
                   :QLN-INSACCERN-9,
                   :QLN-INSEARN-ORDER-9,
                   :QLN-INSFINCHG-9,
                   :QLN-INSCOMP-10,
                   :QLN-INSOURS-10,
                   :QLN-INSEFF-DATE-10 ,
                   :QLN-INSEXP-DATE-10 ,
                   :QLN-INSPREM-10,
                   :QLN-INSCOVR-10,
                   :QLN-INSCOMM-10,
                   :QLN-INSANTICERN-10,
                   :QLN-INSANTICADJ-10,
                   :QLN-INSUFERN-10,
                   :QLN-INSACCERN-10,
                   :QLN-INSEARN-ORDER-10,
                   :QLN-INSFINCHG-10,
                   :QLN-PY-YYYYMM-RECDEL,
                   :QLN-MTD-PAYS-RECDEL,
                   :QLN-GARNISH-EXTR,
                   :QLN-SHIP-SAME-FG,
                   :QLN-AD-D-INS-COVR,
                   :QLN-TOT-PARTIAL-CHGOFF,
                   :QLN-LPO,
                   :QLN-REP-CD,
                   :QLN-EMP-ID,
                   :QLN-PROCESS-CD,
                   :QLN-WCBC-CREDITLIM,
                   :QLN-FOLLOWUP-CD,
                   :QLN-MIP-FG,
                   :QLN-MIP-MTD-CLAHPAID,
                   :QLN-MIP-CLPDTH-DATE ,
                   :QLN-MIP-TOT-CLPAID,
                   :QLN-MIP-AHPDTH-DATE ,
                   :QLN-MIP-TOT-AHPAID,
                   :QLN-IL-GROSS-INCOME,
                   :QLN-IL-GROSS-INCOME2,
                   :QLN-RESCISSION,
                   :QLN-INTERIM-TRW,
                   :QLN-DLNO,
                   :QLN-DLPOOL,
                   :QLN-DLRECOURSE,
                   :QLN-DLPARTRCAMT,
                   :QLN-ORIGPURAMT,
                   :QLN-PURBAL,
                   :QLN-DLRVPRIN-1,
                   :QLN-DLRVPRIN-2,
                   :QLN-DLRVPRIN-3,
                   :QLN-DLRVCHGS-1,
                   :QLN-DLRVCHGS-2,
                   :QLN-DLRVCHGS-3,
                   :QLN-DLPARVHELD,
                   :QLN-DLPARVPAID,
                   :QLN-DLDISC,
                   :QLN-LCPDTH-DATE  ,
                   :QLN-INTPDTH-DATE ,
                   :QLN-CURBAL,
                   :QLN-INTBAL,
                   :QLN-LCBAL,
                   :QLN-OTHBAL,
                   :QLN-OT2BAL,
                   :QLN-LCPAID,
                   :QLN-LCPARTIALS,
                   :QLN-STMT-COUNTER,
                   :QLN-PRIOR-STMT-COUNTER,
                   :QLN-FROZEN-REASON,
                   :QLN-COUPON-STATEMENT,
                   :QLN-MONTHEND-BAL,
                   :QLN-ANTICTERM,
                   :QLN-ANTICERN-1,
                   :QLN-ANTICADJ-1,
                   :QLN-UFERN-1,
                   :QLN-ACCERN-1,
                   :QLN-EARN-ORDER-1,
                   :QLN-ANTICERN-2,
                   :QLN-ANTICADJ-2,
                   :QLN-UFERN-2,
                   :QLN-ACCERN-2,
                   :QLN-EARN-ORDER-2,
                   :QLN-ANTICERN-3,
                   :QLN-ANTICADJ-3,
                   :QLN-UFERN-3,
                   :QLN-ACCERN-3,
                   :QLN-EARN-ORDER-3,
                   :QLN-ANTICERN-4,
                   :QLN-ANTICADJ-4,
                   :QLN-UFERN-4,
                   :QLN-ACCERN-4,
                   :QLN-EARN-ORDER-4,
                   :QLN-ANTICERN-5,
                   :QLN-ANTICADJ-5,
                   :QLN-UFERN-5,
                   :QLN-ACCERN-5,
                   :QLN-EARN-ORDER-5,
                   :QLN-ANTICERN-6,
                   :QLN-ANTICADJ-6,
                   :QLN-UFERN-6,
                   :QLN-ACCERN-6,
                   :QLN-EARN-ORDER-6,
                   :QLN-ANTICERN-7,
                   :QLN-ANTICADJ-7,
                   :QLN-UFERN-7,
                   :QLN-ACCERN-7,
                   :QLN-EARN-ORDER-7,
                   :QLN-ANTICERN-8,
                   :QLN-ANTICADJ-8,
                   :QLN-UFERN-8,
                   :QLN-ACCERN-8,
                   :QLN-EARN-ORDER-8,
                   :QLN-ANTICERN-9,
                   :QLN-ANTICADJ-9,
                   :QLN-UFERN-9,
                   :QLN-ACCERN-9,
                   :QLN-EARN-ORDER-9,
                   :QLN-ANTICERN-10,
                   :QLN-ANTICADJ-10,
                   :QLN-UFERN-10,
                   :QLN-ACCERN-10,
                   :QLN-EARN-ORDER-10,
                   :QLN-LMO-PRIOR-INTEREST,
                   :QLN-PRIOR-UNEARNED,
                   :QLN-ERNDELQ-STOP,
                   :QLN-NONACRERN,
                   :QLN-PRIOR-NONACRERN,
                   :QLN-ERN-IB-PDTH-DATE ,
                   :QLN-ERN-IB-PRINBAL,
                   :QLN-ERN-IB-INTOWE,
                   :QLN-TOTERN,
                   :QLN-TOTOTH,
                   :QLN-TOTINT,
                   :QLN-TOTLCHG,
                   :QLN-TOTDEF,
                   :QLN-TOTSERACCT,
                   :QLN-LCWAIVERS,
                   :QLN-INTWAIVERS,
                   :QLN-TOTPAYMNTD,
                   :QLN-TOTEXCPAYMNTD,
                   :QLN-TOTNODEF,
                   :QLN-YTDNODEF,
                   :QLN-TOTNODEFACT,
                   :QLN-YTDNODEFACT,
                   :QLN-TOTNOALDEL,
                   :QLN-YTDNOALDEL,
                   :QLN-YTDINTPD,
                   :QLN-LYRINTPD,
                   :QLN-TOTINTPD,
                   :QLN-RECASTINT,
                   :QLN-RECASTPRIN,
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
                   :QLN-PLREASON,
                   :QLN-COLLECTOR,
                   :QLN-ACTIONCD,
                   :QLN-ACTIONCD2,
                   :QLN-ATCODE,
                   :QLN-RECODE,
                   :QLN-ESCROW-FG,
                   :QLN-PROMDAYS-PASTDUE,
                   :QLN-PROMID,
                   :QLN-PROMDATE ,
                   :QLN-PROMAMT,
                   :QLN-PROMDATE2 ,
                   :QLN-DATE-PAID-LAST ,
                   :QLN-AMT-PAID-LAST,
                   :QLN-LASTCFORMNO,
                   :QLN-LASTCFORMDATE ,
                   :QLN-DELCNT,
                   :QLN-DELCNT2,
                   :QLN-CREDITLIM,
                   :QLN-CRSCORE,
                   :QLN-LASTSFORMNO,
                   :QLN-LASTSFORMDATE ,
                   :QLN-SOLCNT,
                   :QLN-MESSAGE,
                   :QLN-OLDACCTNO,
                   :QLN-PRLNNO,
                   :QLN-PRBAL,
                   :QLN-PRINTDUE,
                   :QLN-PRFINCHGRB,
                   :QLN-PRINSRB,
                   :QLN-PRPOCD,
                   :QLN-PRPODATE ,
                   :QLN-PRDELINQAMT,
                   :QLN-PRLNAMT,
                   :QLN-PRPDUE-DAYS,
                   :QLN-PRDLDISC-UNEARN,
                   :QLN-PRCONPROFILE,
                   :QLN-SUM-ORIGPURAMT,
                   :QLN-SUM-LNAMT,
                   :QLN-SUM-FINCHG,
                   :QLN-SUM-INTCHG,
                   :QLN-SUM-ORGTERM,
                   :QLN-SUM-REGPYAMT,
                   :QLN-SUM-EXTCHG,
                   :QLN-SUM-SERCHG,
                   :QLN-SUM-MAINTFEE,
                   :QLN-UNITPER-CD,
                   :QLN-UNITPER-FREQ,
                   :QLN-DAYS-IN-UNITPER,
                   :QLN-UNITPER-PER-YEAR,
                   :QLN-REPOCD,
                   :QLN-BR-TFR-FG,
                   :QLN-O3INS-DLPROC,
                   :QLN-INSPREM-DLADJ,
                   :QLN-PLAMT,
                   :QLN-MTD-PRIN,
                   :QLN-MTD-LC,
                   :QLN-MTD-INT,
                   :QLN-MTD-OT,
                   :QLN-MTD-EX,
                   :QLN-BNKRPT-CHAPTER,
                   :QLN-BMO-CON-STATUS,
                   :QLN-BMO-REC-STATUS,
                   :QLN-APP-NUMBER,
                   :QLN-GIFTNET,
                   :QLN-GIFTNET-OUR-PO,
                   :QLN-PROC-DISBURSED,
                   :QLN-NOODDPY,
                   :QLN-CONPROFILE-1,
                   :QLN-CONPROFILE-2,
                   :QLN-CONPROFILE-3,
                   :QLN-CONPROFILE-4,
                   :QLN-CONPROFILE-5,
                   :QLN-RECPROFILE-1,
                   :QLN-RECPROFILE-2,
                   :QLN-RECPROFILE-3,
                   :QLN-RECPROFILE-4,
                   :QLN-RECPROFILE-5,
                   :QLN-TRW-STATUS,
                   :QLN-ORIGIN,
                   :QLN-PO-TRCD,
                   :QLN-NEXIS-CRSCORE,
                   :QLN-TRW-B-CONSUMER-IND,
                   :QLN-PARVHELD-PAID-FG,
                   :QLN-ACCT-FROZEN,
                   :QLN-TRW-SPECIAL-CODE,
                   :QLN-CREATION-STATUS,
                   :QLN-TOTNOLCHG,
                   :QLN-LBOX,
                   :QLN-EXTRA-PRIN-LFP,
                   :QLN-REMAIN-INT-SINCE-LFP,
                   :QLN-REMAIN-PRIN-SINCE-LFP,
                   :QLN-INTERIM-COUPON,
                   :QLN-DLCOMM-O3-CMSI,
                   :QLN-DLCOMM-PP-CMSI,
                   :QLN-TRW-J-CONSUMER-IND,
                   :QLN-ACH,
                   :QLN-1099-C-FG,
                   :QLN-TRW-COMPLIANCE-CODE,
                   :QLN-UNWIND-REASON-CODE,
                   :QLN-PBI-PBD-FG,
                   :QLN-OLD-ACCT-FORMAT-FG,
                   :QLN-APPROVED-BY-USER,
                   :QLN-APPROVED-BY-CODE,
                   :QLN-APPROVED-BY-DATE ,
                   :QLN-APPROVED-BY-TIME,
                   :QLN-LTOUCH-DATE ,
                   :QLN-LTOUCH-TIME,
                   :QBW-OWNBR,
                   :QBW-SSNO,
                   :QBW-LNAME,
                   :QBW-FNAME,
                   :QBW-CONFIDENTIAL,
                   :QNO-BRNO,
                   :QNO-ACCTNO,
                   :QNO-LTOUCH-DATE,
                   :QNO-TDUE-DATE,
                   :QNO-TDUELC, 
                   :QNO-TDUEAMT, 
                   :QNO-TDUEPRIN, 
                   :QNO-TDUETBAL, 
                   :QNO-TDUEPOFF, 
                   :QNO-LDUE-DATE,
                   :QNO-LDUELC, 
                   :QNO-LDUEAMT, 
                   :QNO-DATE-PAID-LAST,
                   :QNO-LSTLPKEY,
                   :QNO-AMT-PAID-LAST, 
                   :QNO-NXDUE-DATE,
                   :QNO-NXDUEAMT, 
                   :QNO-NXDUELC, 
                   :QNO-TRUCONTR, 
                   :QNO-RECENCY, 
                   :QNO-PDTH-DATE,
                   :QNO-LCPDTH-DATE,
                   :QNO-LCHG-INTDUE,
                   :QNO-AVAILCREDIT,
                   :QNO-TDUEDELINQ, 
                   :QNO-DEFERMENT, 
                   :QNO-TRUE-DUE-DATE,
                   :QNO-SCHED-PAYMENT, 
                   :QNO-TDUECONTR, 
                   :QNO-LDUECONTR, 
                   :QNO-NXDUECONTR, 
                   :QNO-CURRENT-AVAIL, 
                   :QNO-LATE-CHG, 
                   :QNO-PAST-DUE-DAYS, 
                   :QNO-RECDEL, 
                   :QNO-AVCR-RESTRICT-FG,
                   :QNO-INDU-PENALTY, 
                   :QNO-CPOT30, 
                   :QNO-RDPOT30, 
                   :QNO-CPOTCHGOFF,
                   :QNO-RDPOTCHGOFF,
                   :QNO-CSUB, 
                   :QNO-RSUB
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           EXEC SQL SET CONNECTION C1 END-EXEC.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-LN3-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-LN-FIELDS
              PERFORM GET-BW-FIELDS
              PERFORM GET-NO-FIELDS.

      *-----------------------------------------------------------------
       CLOSE-LN3-FILE.

           PERFORM CLOSE-IT.

           IF ( LN3-CURSOR-STAT-GOOD )
              EXEC SQL SET CONNECTION LN3_CONNECT END-EXEC
              EXEC SQL
                   CLOSE LN3_CURSOR
              END-EXEC
              EXEC SQL SET CONNECTION C1 END-EXEC
              MOVE STAT---BAD TO LN3-CURSOR-STAT.

      *-----------------------------------------------------------------
       CLOSE-LN3-CONNECTION.

           IF LN3-CONN-OPEN-SW = "Y"
               EXEC SQL SET CONNECTION LN3_CONNECT END-EXEC
               EXEC SQL COMMIT END-EXEC
               EXEC SQL DISCONNECT LN3_CONNECT END-EXEC
               EXEC SQL SET CONNECTION C1 END-EXEC
               MOVE "N" TO LN3-CONN-OPEN-SW.



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
              DESTROY CPCORP-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.
           EXIT PROGRAM.
