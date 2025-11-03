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
      * JTG 072897 ADDED LPWDEL/W  MERCURY #E135
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
      * BAH 181128 GLOBAL LMFILE, L2G
      * BAH 181205 GLOBAL LNFILE, L2G
      * KEC 190219 A32 FFR - REMOVED REDEFINE FOR LN-LC-CODEX; UNUSED
      * BAH 20190919 REMOVED ACCESS-CALL ON LNFILE
      * BAH 20220803 HARD CODED START
      * BAH 20220818 REMOVED GLOBRD, READ GB-FILE INSTEAD
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
       01  WK-REC.
           03  WK-KEY.
               05  WK-BRNO           PIC 9(4).
               05  WK-COLCTID        PIC X(3).
               05  WK-ACTION-CD      PIC X(4).
               05  WK-AGE            PIC S9(5).
               05  WK-BALDUE         PIC S9(9)V99.
               05  WK-NAME           PIC X(33).
               05  WK-ACCTNO         PIC 9(8).
           03  WK-CSUB               PIC S9(6)  COMP.
           03  WK-CPOT30             PIC X.
           03  WK-RDSUB              PIC S9(6)  COMP.
           03  WK-RDPOT30            PIC X.
           03  WK-PSUB               PIC S9(6)  COMP.
           03  WK-CTFC-NM            PIC S9(6)  COMP.

      ******************************************************************
      *
      *     W O R K I N G - S T O R A G E   S E C T I O N
      *
      ******************************************************************
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(37) VALUE
           "@(#)UT/RZCORP S35 04/26/23-12:12:01 >".

       COPY "LIBLP/LP01LC.CPY".
       COPY "LIBLP/LP01LC_SQL.CPY".
       COPY "LIBLP/LPWSLC.CPY".
       COPY "LIBLP/LP01LN.CPY".
      *COPY "LIBLP/LP01LN_SQL.CPY".
	  
      * COPYMEMBER: LIBLP/LP01LN_SQL
      ******************************************************************
      *
      * DESCRIPTION: SQL - LOAN MASTER FILE
      *
      *=================================================================
      * REV:
      * JKC 2021-0430 ***NEW***
      *
      ******************************************************************
      *- - -{STANDARD CURSOR FOR START & READ-NEXT}
       01  LN1-CURSOR-STAT              PIC X(2) VALUE SPACES.
           88  LN1-CURSOR-STAT-GOOD              VALUE "00".
           88  LN1-CURSOR-STAT-BAD               VALUE "  " THRU "//"
                                                       "01" THRU "~~".

       01  LN3-CURSOR-STAT              PIC X(2) VALUE SPACES.
           88  LN3-CURSOR-STAT-GOOD              VALUE "00".
           88  LN3-CURSOR-STAT-BAD               VALUE "  " THRU "//"
                                                       "01" THRU "~~".

       01  LN4-CURSOR-STAT              PIC X(2) VALUE SPACES.
           88  LN4-CURSOR-STAT-GOOD              VALUE "00".
           88  LN4-CURSOR-STAT-BAD               VALUE "  " THRU "//"
                                                       "01" THRU "~~".

       01  LN5-CURSOR-STAT              PIC X(2) VALUE SPACES.
           88  LN5-CURSOR-STAT-GOOD              VALUE "00".
           88  LN5-CURSOR-STAT-BAD               VALUE "  " THRU "//"
                                                       "01" THRU "~~".


      *- - -{GREATER CURSOR FOR START-GREATER & READ-NEXT}
       01  LN1-CURSOR-GR-STAT        PIC X(2)    VALUE SPACES.
           88  LN1-CURSOR-GR-STAT-GOOD           VALUE "00".
           88  LN1-CURSOR-GR-STAT-BAD            VALUE "  " THRU "//"
                                                       "01" THRU "~~".

       01  LN3-CURSOR-GR-STAT        PIC X(2)    VALUE SPACES.
           88  LN3-CURSOR-GR-STAT-GOOD           VALUE "00".
           88  LN3-CURSOR-GR-STAT-BAD            VALUE "  " THRU "//"
                                                       "01" THRU "~~".

      *- - -{NOT GREATER CURSOR FOR START-NOT-GREATER & READ-PREVIOUS}
       01  LN4-CURSOR-NG-STAT        PIC X(2)    VALUE SPACES.
           88  LN4-CURSOR-NG-STAT-GOOD           VALUE "00".
           88  LN4-CURSOR-NG-STAT-BAD            VALUE "  " THRU "//"
                                                       "01" THRU "~~".

       01  LN5-CURSOR-NG-STAT        PIC X(2)    VALUE SPACES.
           88  LN5-CURSOR-NG-STAT-GOOD           VALUE "00".
           88  LN5-CURSOR-NG-STAT-BAD            VALUE "  " THRU "//"
                                                       "01" THRU "~~".


       EXEC SQL BEGIN DECLARE SECTION END-EXEC.

       01  LN-COMMAND                PIC X(8192) VALUE SPACES.

       01  RET-CODE                                  PIC S9(4) COMP-5.  

       01  QLN1-WHERE-BEG-KEY.
           03  QLN1-WBEG-OWNBR                       PIC 9(4).
           03  QLN1-WBEG-ACCTNO                      PIC 9(8).

       01  QLN1-WHERE-END-KEY.
           03  QLN1-WEND-OWNBR                       PIC 9(4).
           03  QLN1-WEND-ACCTNO                      PIC 9(8).

       01  QLN3-WHERE-BEG-KEY.
           03  QLN3-WBEG-OWNBR                       PIC 9(4).
           03  QLN3-WBEG-CLASS                       PIC 9(3).
           03  QLN3-WBEG-ACCTNO                      PIC 9(8).

       01  QLN3-WHERE-END-KEY.
           03  QLN3-WEND-OWNBR                       PIC 9(4).
           03  QLN3-WEND-CLASS                       PIC 9(3).
           03  QLN3-WEND-ACCTNO                      PIC 9(8).

       01  QLN4-WHERE-BEG-KEY.
           03  QLN4-WBEG-OWNBR                       PIC 9(4).
           03  QLN4-WBEG-SSNO1                       PIC 9(9).

       01  QLN4-WHERE-END-KEY.
           03  QLN4-WEND-OWNBR                       PIC 9(4).
           03  QLN4-WEND-SSNO1                       PIC 9(9).

       01  QLN5-WHERE-BEG-KEY.
           03  QLN5-WBEG-OWNBR                       PIC 9(4).
           03  QLN5-WBEG-SSNO2                       PIC 9(9).

       01  QLN5-WHERE-END-KEY.
           03  QLN5-WEND-OWNBR                       PIC 9(4).
           03  QLN5-WEND-SSNO2                       PIC 9(9).


       01  QLN-REC.
           03  QLN-OWNBR                             PIC 9(4).
           03  QLN-ACCTNO                            PIC 9(8).
           03  QLN-CLASS                             PIC 9(3).
           03  QLN-SUBCLASS                          PIC 99.
           03  QLN-LAWCODE                           PIC 99.
           03  QLN-SSNO1                             PIC 9(9).
           03  QLN-MAKERCD1                          PIC X.
           03  QLN-SSNO2                             PIC 9(9).
           03  QLN-MAKERCD2                          PIC X.
           03  QLN-SSNO3                             PIC 9(9).
           03  QLN-MAKERCD3                          PIC X.
           03  QLN-SSNO4                             PIC 9(9).
           03  QLN-MAKERCD4                          PIC X.
           03  QLN-ORGST                             PIC XX.
           03  QLN-ORGBR                             PIC 9(4).
           03  QLN-OWNST                             PIC XX.
           03  QLN-SPRCLASS                          PIC 9(3).
           03  QLN-CONVERCD                          PIC X.
           03  QLN-MIDSTREAM                         PIC X.
           03  QLN-PURCD                             PIC XXX.
           03  QLN-SRCD                              PIC XX.
           03  QLN-COLLCD                            PIC X(2).
           03  QLN-COLLID                            PIC X(20).
           03  QLN-POCD                              PIC XX.
           03  QLN-PLCD                              PIC X.
           03  QLN-PLTYPE                            PIC X.
           03  QLN-CONFIDENTCD                       PIC X.
           03  QLN-ALLOTCD                           PIC X.
           03  QLN-RECASTCD                          PIC X.
           03  QLN-SKIP-TRW                          PIC X.
           03  QLN-STATUSFG                          PIC X.
           03  QLN-SMALLOAN                          PIC X.
           03  QLN-SALEFIN                           PIC X.
           03  QLN-SERVACCT                          PIC X.
           03  QLN-LOANTYPE                          PIC X.
           03  QLN-ADDON-FG                          PIC X.
           03  QLN-PAY-SCHLD-FG                      PIC X.
           03  QLN-LC-CODE                           PIC 99.
           03  QLN-CAN-LOAN-NO                       PIC XX.
           03  QLN-STATFLAGS                         PIC X(20).
           03  QLN-ENTDATE                           PIC X(10).
           03  QLN-LOANDATE                          PIC X(10).
           03  QLN-PURDATE                           PIC X(10).
           03  QLN-INTDATE                           PIC X(10).
           03  QLN-CASHDATE                          PIC X(10).
           03  QLN-FILEDATE                          PIC X(10).
           03  QLN-RESCISSION-DATE                   PIC X(10).
           03  QLN-PART-RECOURSE-DATE                PIC X(10).
           03  QLN-ORIG-1STPYDATE                    PIC X(10).
           03  QLN-1STPYDATE                         PIC X(10).
           03  QLN-JDDATE                            PIC X(10).
           03  QLN-PLDATE                            PIC X(10).
           03  QLN-PL-TRDATE                         PIC X(10).
           03  QLN-ACCELDATE                         PIC X(10).
           03  QLN-REDUDATE                          PIC X(10).
           03  QLN-POFFDATE                          PIC X(10).
           03  QLN-PO-TRDATE                         PIC X(10).
           03  QLN-LEGALDATE                         PIC X(10).
           03  QLN-BNKRPTDATE                        PIC X(10).
           03  QLN-BNKRPT-DISMISS-DATE               PIC X(10).
           03  QLN-BNKRPT-DISCHARGE-DATE             PIC X(10).
           03  QLN-BNKRPT-REAFFIRM-DATE              PIC X(10).
           03  QLN-DEBTAIDATE                        PIC X(10).
           03  QLN-REPODATE                          PIC X(10).
           03  QLN-REDEEMDATE                        PIC X(10).
           03  QLN-SOLDDATE                          PIC X(10).
           03  QLN-INS-CONFIRM-DATE                  PIC X(10).
           03  QLN-PRIOR-RN-FOR10-DATE               PIC X(10).
           03  QLN-LAST-RN-FOR10-DATE                PIC X(10).
           03  QLN-TRW-DOFD-DATE                     PIC X(10).
           03  QLN-LAST-PYPA-DATE                    PIC X(10).
           03  QLN-BT-TRDATE                         PIC X(10).
           03  QLN-METRO2-LAST-PMT-DATE              PIC X(10).
           03  QLN-FOLLOWUP-DATE                     PIC X(10).
           03  QLN-LAST-ACCRUAL-DATE                 PIC X(10).
           03  QLN-APRATE                            PIC 999V999.
           03  QLN-EFFRATE                           PIC 999V999.
           03  QLN-SMPRATE                           PIC 999V999.
           03  QLN-YLDRATE                           PIC 999V999.
           03  QLN-OVRDRATE                          PIC 999V999.
           03  QLN-ENDORATE                          PIC 999V999.
           03  QLN-JDRATE                            PIC 999V999.
           03  QLN-ACCRATE                           PIC 999V999.
           03  QLN-REDURATE                          PIC 999V999.
           03  QLN-CONRATE                           PIC 999V999.
           03  QLN-SERVPER                           PIC 9(3)V99.
           03  QLN-SELLPR                            PIC 9(7)V99.
           03  QLN-DOWNPY                            PIC 9(7)V99.
           03  QLN-TRADEIN                           PIC S9(7)V99.
           03  QLN-LNAMT                             PIC 9(7)V99.
           03  QLN-FINCHG                            PIC 9(7)V99.
           03  QLN-INTCHG                            PIC 9(7)V99.
           03  QLN-SERCHG                            PIC 9(5)V99.
           03  QLN-EXTCHG                            PIC 9(5)V99.
           03  QLN-MAINTFEE                          PIC 9(5)V99.
           03  QLN-POINTS                            PIC 99V99.
           03  QLN-PROCEEDS                          PIC S9(7)V99.
           03  QLN-ORGTERM                           PIC 9(3).
           03  QLN-1STPYAMT                          PIC 9(7)V99.
           03  QLN-NOREGPY                           PIC 9(3).
           03  QLN-REGPYAMT                          PIC 9(7)V99 .
           03  QLN-LASTPYAMT                         PIC S9(7)V99.
           03  QLN-PROCASH                           PIC S9(7)V99.
           03  QLN-TILLNO                            PIC 9(2).
           03  QLN-STATE-FLAG                        PIC X.
           03  QLN-STATE-WORKER                      PIC S9(7)V99.
           03  QLN-STATE-WORKER2                     PIC S9(5)V99.
           03  QLN-FEECODE-1                         PIC XX.
           03  QLN-FEEAMT-1                          PIC 9(5)V99.
           03  QLN-FEEOURS-1                         PIC X.
           03  QLN-FEEFINCHG-1                       PIC X.
           03  QLN-FEECODE-2                         PIC XX.
           03  QLN-FEEAMT-2                          PIC 9(5)V99.
           03  QLN-FEEOURS-2                         PIC X.
           03  QLN-FEEFINCHG-2                       PIC X.
           03  QLN-FEECODE-3                         PIC XX.
           03  QLN-FEEAMT-3                          PIC 9(5)V99.
           03  QLN-FEEOURS-3                         PIC X.
           03  QLN-FEEFINCHG-3                       PIC X.
           03  QLN-FEECODE-4                         PIC XX.
           03  QLN-FEEAMT-4                          PIC 9(5)V99.
           03  QLN-FEEOURS-4                         PIC X.
           03  QLN-FEEFINCHG-4                       PIC X.
           03  QLN-FEECODE-5                         PIC XX.
           03  QLN-FEEAMT-5                          PIC 9(5)V99.
           03  QLN-FEEOURS-5                         PIC X.
           03  QLN-FEEFINCHG-5                       PIC X.
           03  QLN-FEECODE-6                         PIC XX.
           03  QLN-FEEAMT-6                          PIC 9(5)V99.
           03  QLN-FEEOURS-6                         PIC X.
           03  QLN-FEEFINCHG-6                       PIC X.
           03  QLN-FEECODE-7                         PIC XX.
           03  QLN-FEEAMT-7                          PIC 9(5)V99.
           03  QLN-FEEOURS-7                         PIC X.
           03  QLN-FEEFINCHG-7                       PIC X.
           03  QLN-FEECODE-8                         PIC XX.
           03  QLN-FEEAMT-8                          PIC 9(5)V99.
           03  QLN-FEEOURS-8                         PIC X.
           03  QLN-FEEFINCHG-8                       PIC X.
           03  QLN-FEECODE-9                         PIC XX.
           03  QLN-FEEAMT-9                          PIC 9(5)V99.
           03  QLN-FEEOURS-9                         PIC X.
           03  QLN-FEEFINCHG-9                       PIC X.
           03  QLN-FEECODE-10                        PIC XX.
           03  QLN-FEEAMT-10                         PIC 9(5)V99.
           03  QLN-FEEOURS-10                        PIC X.
           03  QLN-FEEFINCHG-10                      PIC X.
           03  QLN-FEECODE-11                        PIC XX.
           03  QLN-FEEAMT-11                         PIC 9(5)V99.
           03  QLN-FEEOURS-11                        PIC X.
           03  QLN-FEEFINCHG-11                      PIC X.
           03  QLN-FEECODE-12                        PIC XX.
           03  QLN-FEEAMT-12                         PIC 9(5)V99.
           03  QLN-FEEOURS-12                        PIC X.
           03  QLN-FEEFINCHG-12                      PIC X.
           03  QLN-FEECODE-13                        PIC XX.
           03  QLN-FEEAMT-13                         PIC 9(5)V99.
           03  QLN-FEEOURS-13                        PIC X.
           03  QLN-FEEFINCHG-13                      PIC X.
           03  QLN-FEECODE-14                        PIC XX.
           03  QLN-FEEAMT-14                         PIC 9(5)V99.
           03  QLN-FEEOURS-14                        PIC X.
           03  QLN-FEEFINCHG-14                      PIC X.
           03  QLN-FEECODE-15                        PIC XX.
           03  QLN-FEEAMT-15                         PIC 9(5)V99.
           03  QLN-FEEOURS-15                        PIC X.
           03  QLN-FEEFINCHG-15                      PIC X.
           03  QLN-FEECODE-16                        PIC XX.
           03  QLN-FEEAMT-16                         PIC 9(5)V99.
           03  QLN-FEEOURS-16                        PIC X.
           03  QLN-FEEFINCHG-16                      PIC X.
           03  QLN-FEECODE-17                        PIC XX.
           03  QLN-FEEAMT-17                         PIC 9(5)V99.
           03  QLN-FEEOURS-17                        PIC X.
           03  QLN-FEEFINCHG-17                      PIC X.
           03  QLN-FEECODE-18                        PIC XX.
           03  QLN-FEEAMT-18                         PIC 9(5)V99.
           03  QLN-FEEOURS-18                        PIC X.
           03  QLN-FEEFINCHG-18                      PIC X.
           03  QLN-FEECODE-19                        PIC XX.
           03  QLN-FEEAMT-19                         PIC 9(5)V99.
           03  QLN-FEEOURS-19                        PIC X.
           03  QLN-FEEFINCHG-19                      PIC X.
           03  QLN-FEECODE-20                        PIC XX.
           03  QLN-FEEAMT-20                         PIC 9(5)V99.
           03  QLN-FEEOURS-20                        PIC X.
           03  QLN-FEEFINCHG-20                      PIC X.
           03  QLN-INSURED                           PIC X.
           03  QLN-INSTYPES                          PIC X.
           03  QLN-INSELIGIBLE                       PIC X.
           03  QLN-INSCOMP-1                         PIC 99.
           03  QLN-INSOURS-1                         PIC X.
           03  QLN-INSEFF-DATE-1                     PIC X(10).
           03  QLN-INSEXP-DATE-1                     PIC X(10).
           03  QLN-INSPREM-1                         PIC 9(5)V99.
           03  QLN-INSCOVR-1                         PIC 9(7)V99.
           03  QLN-INSCOMM-1                         PIC 9(5)V99.
           03  QLN-INSANTICERN-1                     PIC S9(7)V99.
           03  QLN-INSANTICADJ-1                     PIC S9(7)V99.
           03  QLN-INSUFERN-1                        PIC S9(7)V99.
           03  QLN-INSACCERN-1                       PIC S9(7)V99.
           03  QLN-INSEARN-ORDER-1                   PIC 99.
           03  QLN-INSFINCHG-1                       PIC X.
           03  QLN-INSCOMP-2                         PIC 99.
           03  QLN-INSOURS-2                         PIC X.
           03  QLN-INSEFF-DATE-2                     PIC X(10).
           03  QLN-INSEXP-DATE-2                     PIC X(10).
           03  QLN-INSPREM-2                         PIC 9(5)V99.
           03  QLN-INSCOVR-2                         PIC 9(7)V99.
           03  QLN-INSCOMM-2                         PIC 9(5)V99.
           03  QLN-INSANTICERN-2                     PIC S9(7)V99.
           03  QLN-INSANTICADJ-2                     PIC S9(7)V99.
           03  QLN-INSUFERN-2                        PIC S9(7)V99.
           03  QLN-INSACCERN-2                       PIC S9(7)V99.
           03  QLN-INSEARN-ORDER-2                   PIC 99.
           03  QLN-INSFINCHG-2                       PIC X.
           03  QLN-INSCOMP-3                         PIC 99.
           03  QLN-INSOURS-3                         PIC X.
           03  QLN-INSEFF-DATE-3                     PIC X(10).
           03  QLN-INSEXP-DATE-3                     PIC X(10).
           03  QLN-INSPREM-3                         PIC 9(5)V99.
           03  QLN-INSCOVR-3                         PIC 9(7)V99.
           03  QLN-INSCOMM-3                         PIC 9(5)V99.
           03  QLN-INSANTICERN-3                     PIC S9(7)V99.
           03  QLN-INSANTICADJ-3                     PIC S9(7)V99.
           03  QLN-INSUFERN-3                        PIC S9(7)V99.
           03  QLN-INSACCERN-3                       PIC S9(7)V99.
           03  QLN-INSEARN-ORDER-3                   PIC 99.
           03  QLN-INSFINCHG-3                       PIC X.
           03  QLN-INSCOMP-4                         PIC 99.
           03  QLN-INSOURS-4                         PIC X.
           03  QLN-INSEFF-DATE-4                     PIC X(10).
           03  QLN-INSEXP-DATE-4                     PIC X(10).
           03  QLN-INSPREM-4                         PIC 9(5)V99.
           03  QLN-INSCOVR-4                         PIC 9(7)V99.
           03  QLN-INSCOMM-4                         PIC 9(5)V99.
           03  QLN-INSANTICERN-4                     PIC S9(7)V99.
           03  QLN-INSANTICADJ-4                     PIC S9(7)V99.
           03  QLN-INSUFERN-4                        PIC S9(7)V99.
           03  QLN-INSACCERN-4                       PIC S9(7)V99.
           03  QLN-INSEARN-ORDER-4                   PIC 99.
           03  QLN-INSFINCHG-4                       PIC X.
           03  QLN-INSCOMP-5                         PIC 99.
           03  QLN-INSOURS-5                         PIC X.
           03  QLN-INSEFF-DATE-5                     PIC X(10).
           03  QLN-INSEXP-DATE-5                     PIC X(10).
           03  QLN-INSPREM-5                         PIC 9(5)V99.
           03  QLN-INSCOVR-5                         PIC 9(7)V99.
           03  QLN-INSCOMM-5                         PIC 9(5)V99.
           03  QLN-INSANTICERN-5                     PIC S9(7)V99.
           03  QLN-INSANTICADJ-5                     PIC S9(7)V99.
           03  QLN-INSUFERN-5                        PIC S9(7)V99.
           03  QLN-INSACCERN-5                       PIC S9(7)V99.
           03  QLN-INSEARN-ORDER-5                   PIC 99.
           03  QLN-INSFINCHG-5                       PIC X.
           03  QLN-INSCOMP-6                         PIC 99.
           03  QLN-INSOURS-6                         PIC X.
           03  QLN-INSEFF-DATE-6                     PIC X(10).
           03  QLN-INSEXP-DATE-6                     PIC X(10).
           03  QLN-INSPREM-6                         PIC 9(5)V99.
           03  QLN-INSCOVR-6                         PIC 9(7)V99.
           03  QLN-INSCOMM-6                         PIC 9(5)V99.
           03  QLN-INSANTICERN-6                     PIC S9(7)V99.
           03  QLN-INSANTICADJ-6                     PIC S9(7)V99.
           03  QLN-INSUFERN-6                        PIC S9(7)V99.
           03  QLN-INSACCERN-6                       PIC S9(7)V99.
           03  QLN-INSEARN-ORDER-6                   PIC 99.
           03  QLN-INSFINCHG-6                       PIC X.
           03  QLN-INSCOMP-7                         PIC 99.
           03  QLN-INSOURS-7                         PIC X.
           03  QLN-INSEFF-DATE-7                     PIC X(10).
           03  QLN-INSEXP-DATE-7                     PIC X(10).
           03  QLN-INSPREM-7                         PIC 9(5)V99.
           03  QLN-INSCOVR-7                         PIC 9(7)V99.
           03  QLN-INSCOMM-7                         PIC 9(5)V99.
           03  QLN-INSANTICERN-7                     PIC S9(7)V99.
           03  QLN-INSANTICADJ-7                     PIC S9(7)V99.
           03  QLN-INSUFERN-7                        PIC S9(7)V99.
           03  QLN-INSACCERN-7                       PIC S9(7)V99.
           03  QLN-INSEARN-ORDER-7                   PIC 99.
           03  QLN-INSFINCHG-7                       PIC X.
           03  QLN-INSCOMP-8                         PIC 99.
           03  QLN-INSOURS-8                         PIC X.
           03  QLN-INSEFF-DATE-8                     PIC X(10).
           03  QLN-INSEXP-DATE-8                     PIC X(10).
           03  QLN-INSPREM-8                         PIC 9(5)V99.
           03  QLN-INSCOVR-8                         PIC 9(7)V99.
           03  QLN-INSCOMM-8                         PIC 9(5)V99.
           03  QLN-INSANTICERN-8                     PIC S9(7)V99.
           03  QLN-INSANTICADJ-8                     PIC S9(7)V99.
           03  QLN-INSUFERN-8                        PIC S9(7)V99.
           03  QLN-INSACCERN-8                       PIC S9(7)V99.
           03  QLN-INSEARN-ORDER-8                   PIC 99.
           03  QLN-INSFINCHG-8                       PIC X.
           03  QLN-INSCOMP-9                         PIC 99.
           03  QLN-INSOURS-9                         PIC X.
           03  QLN-INSEFF-DATE-9                     PIC X(10).
           03  QLN-INSEXP-DATE-9                     PIC X(10).
           03  QLN-INSPREM-9                         PIC 9(5)V99.
           03  QLN-INSCOVR-9                         PIC 9(7)V99.
           03  QLN-INSCOMM-9                         PIC 9(5)V99.
           03  QLN-INSANTICERN-9                     PIC S9(7)V99.
           03  QLN-INSANTICADJ-9                     PIC S9(7)V99.
           03  QLN-INSUFERN-9                        PIC S9(7)V99.
           03  QLN-INSACCERN-9                       PIC S9(7)V99.
           03  QLN-INSEARN-ORDER-9                   PIC 99.
           03  QLN-INSFINCHG-9                       PIC X.
           03  QLN-INSCOMP-10                        PIC 99.
           03  QLN-INSOURS-10                        PIC X.
           03  QLN-INSEFF-DATE-10                    PIC X(10).
           03  QLN-INSEXP-DATE-10                    PIC X(10).
           03  QLN-INSPREM-10                        PIC 9(5)V99.
           03  QLN-INSCOVR-10                        PIC 9(7)V99.
           03  QLN-INSCOMM-10                        PIC 9(5)V99.
           03  QLN-INSANTICERN-10                    PIC S9(7)V99.
           03  QLN-INSANTICADJ-10                    PIC S9(7)V99.
           03  QLN-INSUFERN-10                       PIC S9(7)V99.
           03  QLN-INSACCERN-10                      PIC S9(7)V99.
           03  QLN-INSEARN-ORDER-10                  PIC 99.
           03  QLN-INSFINCHG-10                      PIC X.
           03  QLN-PY-YYYYMM-RECDEL                  PIC 9(6).
           03  QLN-MTD-PAYS-RECDEL                   PIC S9(7)V99.
           03  QLN-GARNISH-EXTR                      PIC X.
           03  QLN-SHIP-SAME-FG                      PIC X.
           03  QLN-AD-D-INS-COVR                     PIC 9(7).
           03  QLN-TOT-PARTIAL-CHGOFF                PIC S9(7)V99.
           03  QLN-LPO                               PIC X(4).
           03  QLN-REP-CD                            PIC X(8).
           03  QLN-EMP-ID                            PIC 9(8).
           03  QLN-PROCESS-CD                        PIC X.
           03  QLN-WCBC-CREDITLIM                    PIC S9(5).
           03  QLN-FOLLOWUP-CD                       PIC X(4).
           03  QLN-MIP-FG                            PIC X.
           03  QLN-MIP-MTD-CLAHPAID                  PIC S9(5)V99.
           03  QLN-MIP-CLPDTH-DATE                   PIC X(10).
           03  QLN-MIP-TOT-CLPAID                    PIC S9(7)V99.
           03  QLN-MIP-AHPDTH-DATE                   PIC X(10).
           03  QLN-MIP-TOT-AHPAID                    PIC S9(7)V99.
           03  QLN-IL-GROSS-INCOME                   PIC S9(7)V99.
           03  QLN-IL-GROSS-INCOME2                  PIC S9(7)V99.
           03  QLN-RESCISSION                        PIC X(1).
           03  QLN-INTERIM-TRW                       PIC X(1).
           03  QLN-DLNO                              PIC 9(4)V99.
           03  QLN-DLPOOL                            PIC 9(5).
           03  QLN-DLRECOURSE                        PIC X.
           03  QLN-DLPARTRCAMT                       PIC 9(7)V99.
           03  QLN-ORIGPURAMT                        PIC 9(7)V99.
           03  QLN-PURBAL                            PIC S9(7)V99.
           03  QLN-DLRVPRIN-1                        PIC 9(7)V99.
           03  QLN-DLRVPRIN-2                        PIC 9(7)V99.
           03  QLN-DLRVPRIN-3                        PIC 9(7)V99.
           03  QLN-DLRVCHGS-1                        PIC 9(7)V99.
           03  QLN-DLRVCHGS-2                        PIC 9(7)V99.
           03  QLN-DLRVCHGS-3                        PIC 9(7)V99.
           03  QLN-DLPARVHELD                        PIC 9(7)V99.
           03  QLN-DLPARVPAID                        PIC 9(7)V99.
           03  QLN-DLDISC                            PIC 9(7)V99.
           03  QLN-LCPDTH-DATE                       PIC X(10).
           03  QLN-INTPDTH-DATE                      PIC X(10).
           03  QLN-CURBAL                            PIC S9(7)V99.
           03  QLN-INTBAL                            PIC S9(7)V99.
           03  QLN-LCBAL                             PIC S9(7)V99.
           03  QLN-OTHBAL                            PIC S9(7)V99.
           03  QLN-OT2BAL                            PIC S9(7)V99.
           03  QLN-LCPAID                            PIC S9(7)V99.
           03  QLN-LCPARTIALS                        PIC S9(7)V99.
           03  QLN-STMT-COUNTER                      PIC 9(5).
           03  QLN-PRIOR-STMT-COUNTER                PIC 9(5).
           03  QLN-FROZEN-REASON                     PIC X(2).
           03  QLN-COUPON-STATEMENT                  PIC X.
           03  QLN-MONTHEND-BAL                      PIC 9(7)V99.
           03  QLN-ANTICTERM                         PIC 9(3).
           03  QLN-ANTICERN-1                        PIC S9(7)V99.
           03  QLN-ANTICADJ-1                        PIC S9(7)V99.
           03  QLN-UFERN-1                           PIC S9(7)V99.
           03  QLN-ACCERN-1                          PIC S9(7)V99.
           03  QLN-EARN-ORDER-1                      PIC 99.
           03  QLN-ANTICERN-2                        PIC S9(7)V99.
           03  QLN-ANTICADJ-2                        PIC S9(7)V99.
           03  QLN-UFERN-2                           PIC S9(7)V99.
           03  QLN-ACCERN-2                          PIC S9(7)V99.
           03  QLN-EARN-ORDER-2                      PIC 99.
           03  QLN-ANTICERN-3                        PIC S9(7)V99.
           03  QLN-ANTICADJ-3                        PIC S9(7)V99.
           03  QLN-UFERN-3                           PIC S9(7)V99.
           03  QLN-ACCERN-3                          PIC S9(7)V99.
           03  QLN-EARN-ORDER-3                      PIC 99.
           03  QLN-ANTICERN-4                        PIC S9(7)V99.
           03  QLN-ANTICADJ-4                        PIC S9(7)V99.
           03  QLN-UFERN-4                           PIC S9(7)V99.
           03  QLN-ACCERN-4                          PIC S9(7)V99.
           03  QLN-EARN-ORDER-4                      PIC 99.
           03  QLN-ANTICERN-5                        PIC S9(7)V99.
           03  QLN-ANTICADJ-5                        PIC S9(7)V99.
           03  QLN-UFERN-5                           PIC S9(7)V99.
           03  QLN-ACCERN-5                          PIC S9(7)V99.
           03  QLN-EARN-ORDER-5                      PIC 99.
           03  QLN-ANTICERN-6                        PIC S9(7)V99.
           03  QLN-ANTICADJ-6                        PIC S9(7)V99.
           03  QLN-UFERN-6                           PIC S9(7)V99.
           03  QLN-ACCERN-6                          PIC S9(7)V99.
           03  QLN-EARN-ORDER-6                      PIC 99.
           03  QLN-ANTICERN-7                        PIC S9(7)V99.
           03  QLN-ANTICADJ-7                        PIC S9(7)V99.
           03  QLN-UFERN-7                           PIC S9(7)V99.
           03  QLN-ACCERN-7                          PIC S9(7)V99.
           03  QLN-EARN-ORDER-7                      PIC 99.
           03  QLN-ANTICERN-8                        PIC S9(7)V99.
           03  QLN-ANTICADJ-8                        PIC S9(7)V99.
           03  QLN-UFERN-8                           PIC S9(7)V99.
           03  QLN-ACCERN-8                          PIC S9(7)V99.
           03  QLN-EARN-ORDER-8                      PIC 99.
           03  QLN-ANTICERN-9                        PIC S9(7)V99.
           03  QLN-ANTICADJ-9                        PIC S9(7)V99.
           03  QLN-UFERN-9                           PIC S9(7)V99.
           03  QLN-ACCERN-9                          PIC S9(7)V99.
           03  QLN-EARN-ORDER-9                      PIC 99.
           03  QLN-ANTICERN-10                       PIC S9(7)V99.
           03  QLN-ANTICADJ-10                       PIC S9(7)V99.
           03  QLN-UFERN-10                          PIC S9(7)V99.
           03  QLN-ACCERN-10                         PIC S9(7)V99.
           03  QLN-EARN-ORDER-10                     PIC 99.
           03  QLN-LMO-PRIOR-INTEREST                PIC S9(7)V99.
           03  QLN-PRIOR-UNEARNED                    PIC S9(7)V99.
           03  QLN-ERNDELQ-STOP                      PIC X.
           03  QLN-NONACRERN                         PIC S9(7)V99.
           03  QLN-PRIOR-NONACRERN                   PIC S9(7)V99.
           03  QLN-ERN-IB-PDTH-DATE                  PIC X(10).
           03  QLN-ERN-IB-PRINBAL                    PIC S9(7)V99.
           03  QLN-ERN-IB-INTOWE                     PIC S9(7)V99.
           03  QLN-TOTERN                            PIC S9(7)V99.
           03  QLN-TOTOTH                            PIC S9(7)V99.
           03  QLN-TOTINT                            PIC S9(7)V99.
           03  QLN-TOTLCHG                           PIC S9(7)V99.
           03  QLN-TOTDEF                            PIC S9(7)V99.
           03  QLN-TOTSERACCT                        PIC S9(7)V99.
           03  QLN-LCWAIVERS                         PIC S9(5)V99.
           03  QLN-INTWAIVERS                        PIC S9(5)V99.
           03  QLN-TOTPAYMNTD                        PIC S9(7)V99.
           03  QLN-TOTEXCPAYMNTD                     PIC S9(7)V99.
           03  QLN-TOTNODEF                          PIC S99.
           03  QLN-YTDNODEF                          PIC S99.
           03  QLN-TOTNODEFACT                       PIC S99.
           03  QLN-YTDNODEFACT                       PIC S99.
           03  QLN-TOTNOALDEL                        PIC S99.
           03  QLN-YTDNOALDEL                        PIC S99.
           03  QLN-YTDINTPD                          PIC S9(7)V99.
           03  QLN-LYRINTPD                          PIC S9(7)V99.
           03  QLN-TOTINTPD                          PIC S9(7)V99.
           03  QLN-RECASTINT                         PIC 9(7)V99.
           03  QLN-RECASTPRIN                        PIC 9(7)V99.
           03  QLN-CL-REBATE                         PIC X.
           03  QLN-AH-REBATE                         PIC X.
           03  QLN-PP-REBATE                         PIC X.
           03  QLN-INT-REBATE                        PIC X.
           03  QLN-SC-REBATE                         PIC X.
           03  QLN-MF-REBATE                         PIC X.
           03  QLN-DF-REBATE                         PIC X.
           03  QLN-EX-REBATE                         PIC X.
           03  QLN-O1-REBATE                         PIC X.
           03  QLN-O2-REBATE                         PIC X.
           03  QLN-O3-REBATE                         PIC X.
           03  QLN-O4-REBATE                         PIC X.
           03  QLN-O5-REBATE                         PIC X.
           03  QLN-O6-REBATE                         PIC X.
           03  QLN-O7-REBATE                         PIC X.
           03  QLN-UN-O7-REB                         PIC X.
           03  QLN-UN-O8-REB                         PIC X.
           03  QLN-UN-O9-REB                         PIC X.
           03  QLN-UN-O10-REB                        PIC X.
           03  QLN-PLREASON                          PIC XX.
           03  QLN-COLLECTOR                         PIC X(3).
           03  QLN-ACTIONCD                          PIC X(4).
           03  QLN-ACTIONCD2                         PIC X(4).
           03  QLN-ATCODE                            PIC X(3).
           03  QLN-RECODE                            PIC X(3).
           03  QLN-ESCROW-FG                         PIC X.
           03  QLN-PROMDAYS-PASTDUE                  PIC S9(3).
           03  QLN-PROMID                            PIC X(3).
           03  QLN-PROMDATE                          PIC X(10).
           03  QLN-PROMAMT                           PIC 9(7)V99.
           03  QLN-PROMDATE2                         PIC X(10).
           03  QLN-DATE-PAID-LAST                    PIC X(10).
           03  QLN-AMT-PAID-LAST                     PIC 9(7)V99.
           03  QLN-LASTCFORMNO                       PIC 9(5).
           03  QLN-LASTCFORMDATE                     PIC X(10).
           03  QLN-DELCNT                            PIC 99.
           03  QLN-DELCNT2                           PIC 99.
           03  QLN-CREDITLIM                         PIC 9(7).
           03  QLN-CRSCORE                           PIC S9(3).
           03  QLN-LASTSFORMNO                       PIC 9(5).
           03  QLN-LASTSFORMDATE                     PIC X(10).
           03  QLN-SOLCNT                            PIC 99.
           03  QLN-MESSAGE                           PIC X(30).
           03  QLN-OLDACCTNO                         PIC 9(8).
           03  QLN-PRLNNO                            PIC 9(8).
           03  QLN-PRBAL                             PIC S9(7)V99.
           03  QLN-PRINTDUE                          PIC S9(7)V99.
           03  QLN-PRFINCHGRB                        PIC S9(7)V99.
           03  QLN-PRINSRB                           PIC S9(7)V99.
           03  QLN-PRPOCD                            PIC XX.
           03  QLN-PRPODATE                          PIC X(10).
           03  QLN-PRDELINQAMT                       PIC S9(7)V99.
           03  QLN-PRLNAMT                           PIC 9(7)V99.
           03  QLN-PRPDUE-DAYS                       PIC S9(5).
           03  QLN-PRDLDISC-UNEARN                   PIC S9(7)V99.
           03  QLN-PRCONPROFILE                      PIC 9(12).
           03  QLN-SUM-ORIGPURAMT                    PIC 9(7)V99.
           03  QLN-SUM-LNAMT                         PIC 9(7)V99.
           03  QLN-SUM-FINCHG                        PIC 9(7)V99.
           03  QLN-SUM-INTCHG                        PIC 9(7)V99.
           03  QLN-SUM-ORGTERM                       PIC 9(3).
           03  QLN-SUM-REGPYAMT                      PIC 9(7)V99.
           03  QLN-SUM-EXTCHG                        PIC 9(5)V99.
           03  QLN-SUM-SERCHG                        PIC 9(5)V99.
           03  QLN-SUM-MAINTFEE                      PIC 9(7)V99.
           03  QLN-UNITPER-CD                        PIC X.
           03  QLN-UNITPER-FREQ                      PIC 999.
           03  QLN-DAYS-IN-UNITPER                   PIC 9(3).
           03  QLN-UNITPER-PER-YEAR                  PIC 9(3)V9(12).
           03  QLN-REPOCD                            PIC X.
           03  QLN-BR-TFR-FG                         PIC X.
           03  QLN-O3INS-DLPROC                      PIC 9(7)V99.
           03  QLN-INSPREM-DLADJ                     PIC S9(5)V99.
           03  QLN-PLAMT                             PIC 9(7)V99.
           03  QLN-MTD-PRIN                          PIC S9(7)V99.
           03  QLN-MTD-LC                            PIC S9(5)V99.
           03  QLN-MTD-INT                           PIC S9(5)V99.
           03  QLN-MTD-OT                            PIC S9(5)V99.
           03  QLN-MTD-EX                            PIC S9(7)V99.
           03  QLN-BNKRPT-CHAPTER                    PIC 9(2).
           03  QLN-BMO-CON-STATUS                    PIC X.
           03  QLN-BMO-REC-STATUS                    PIC X.
           03  QLN-APP-NUMBER                        PIC 9(09).
           03  QLN-GIFTNET                           PIC X.
           03  QLN-GIFTNET-OUR-PO                    PIC 9(7).
           03  QLN-PROC-DISBURSED                    PIC X.
           03  QLN-NOODDPY                           PIC X.
           03  QLN-CONPROFILE-1                      PIC 9(12).
           03  QLN-CONPROFILE-2                      PIC 9(12).
           03  QLN-CONPROFILE-3                      PIC 9(12).
           03  QLN-CONPROFILE-4                      PIC 9(12).
           03  QLN-CONPROFILE-5                      PIC 9(12).
           03  QLN-RECPROFILE-1                      PIC 9(12).
           03  QLN-RECPROFILE-2                      PIC 9(12).
           03  QLN-RECPROFILE-3                      PIC 9(12).
           03  QLN-RECPROFILE-4                      PIC 9(12).
           03  QLN-RECPROFILE-5                      PIC 9(12).
           03  QLN-TRW-STATUS                        PIC XX.
           03  QLN-ORIGIN                            PIC X(4).
           03  QLN-PO-TRCD                           PIC XX.
           03  QLN-NEXIS-CRSCORE                     PIC 9(3).
           03  QLN-TRW-B-CONSUMER-IND                PIC X(2).
           03  QLN-PARVHELD-PAID-FG                  PIC X.
           03  QLN-ACCT-FROZEN                       PIC X.
           03  QLN-TRW-SPECIAL-CODE                  PIC X(2).
           03  QLN-CREATION-STATUS                   PIC X(1).
           03  QLN-TOTNOLCHG                         PIC S9(3).
           03  QLN-LBOX                              PIC X(1).
           03  QLN-EXTRA-PRIN-LFP                    PIC S9(7)V99.
           03  QLN-REMAIN-INT-SINCE-LFP              PIC S9(7)V99.
           03  QLN-REMAIN-PRIN-SINCE-LFP             PIC S9(7)V99.
           03  QLN-INTERIM-COUPON                    PIC X.
           03  QLN-DLCOMM-O3-CMSI                    PIC 9(5)V99.
           03  QLN-DLCOMM-PP-CMSI                    PIC 9(5)V99.
           03  QLN-TRW-J-CONSUMER-IND                PIC X(2).
           03  QLN-ACH                               PIC X.
           03  QLN-1099-C-FG                         PIC X.
           03  QLN-TRW-COMPLIANCE-CODE               PIC XX.
           03  QLN-UNWIND-REASON-CODE                PIC X(2).
           03  QLN-PBI-PBD-FG                        PIC X.
           03  QLN-OLD-ACCT-FORMAT-FG                PIC X.
           03  QLN-APPROVED-BY-USER                  PIC X(10).
           03  QLN-APPROVED-BY-CODE                  PIC X(3).
           03  QLN-APPROVED-BY-DATE                  PIC X(10).
           03  QLN-APPROVED-BY-TIME                  PIC 9(6).
           03  QLN-LTOUCH-DATE                       PIC X(10).
           03  QLN-LTOUCH-TIME                       PIC 9(6).

       EXEC SQL END DECLARE SECTION END-EXEC.

       COPY "LIBLP/LPWSLN.CPY".
       COPY "LIBLP/LP01BW.CPY".
       COPY "LIBLP/LP01BW_SQL.CPY".
       COPY "LIBLP/LPWSBW.CPY".
      * COPY "LIBGB/GB01BR.CPY".
      * COPY "LIBGB/GB01BR_SQL.CPY".

      * COPY "LIBGB/GB01BR.CPY".
      * COPYMEMBER: LIBGB/GB01BR
      **************************************************************************
      *    BRANCH FILE RECORD      (GB01BR)   
      *         INDEXED VERSION
      *
      * BR-NO = BR-NO
      *
      *REV:
      * JTG 033095 ADDED BR-UNITPER-FG AND BR-UNITPER-Y-D-FG
      * JTG 051595 ADDED BR-RI-BEFORE-REPO
      *                  BR-RI-BEFORE-P4P5
      * JTG 061595 ADDED BR-NOT-OURS-REFUND
      * JTG 061995 ADDED BR-SFDLO3-WINDOW-FG
      * JTG 061995 ADDED BR-CALC-INS-ADJ
      * JTG 051595 ADDED BR-AC-BEFORE-REPO
      *                  BR-AC-BEFORE-P4P5
      * MJD 063095 ADDED BR-BANK-CLEARING-GL
      *                  BR-BANK-ROUTING-NO
      *                  BR-BANK-ACCTNO.
      * JTG 070595 ADDED BR-REB-METHOD-2       OCCURS 5
      *                  BR-REBDAYS-MONTH-2    OCCURS 5
      *                  BR-ACCRUAL-MONTHS-2   OCCURS 5
      * JTG 071395 ADDED BR-DEFAULT-SC-SUBCLASS
      * BLL 071795 EXPANDED UNUSED, BANK ACCT, ADDED BANKGL-DEPOSIT
      * BAH 083195 REMOVED BR-CASH-METHOD-ONLY AND BR-IB-ACCRUAL
      *            (MOVED TO GP RECORD)
      * MJD 011296 ADDED BR-NO-OUT-DLR-RSV.
      * KEC 020796 ADDED BR-COLLECTOR-QUEUE AND BR-COLL-QUEUE-CON-REC.
      * JTG 022096 ADDED BR-LNCHGS-WINDOW
      * JTG 062596 ADDED BR-EXPAND-APP-INPUT
      * BAH 082696 ADDED BR-A50-CPCORP TO USE A50'S COLLECTION REPORT (INSTANT)
      * MJD 100396 ADDED BR-INBR-BILNSC-FG. IF 'Y' THEN GLOBAL SCAN IS
      *            CALLED, ELSE A SINGLE BRANCH IS ENTERED.
      * KEC 112096 ADDED BR-LEDGER-JUST-INTCHG FOR QUICK CREDIT,
      *             FOR "MQ" AND "MR" TO ONLY PRINT INTCHG IN THE
      *             FINANCE CHARGE BOX.  FOR COPYING LP/LONPMQ FROM A48.
      * MJD 012897 ADDED BR-BYINQ-START-PAGE AND BR-DSINQ-START-PAGE.
      *            THESE ARE THE DEFAULT START PAGES FOR INQUIRIES.
      * JTG 022097 ADDED BR-APP-REQUIRED-FOR-INPUT  RE: STATISTICS UPDATE
      * BLV 022597 ADDED BR-CREDIT-SCORING-FG
      * BLV 041497 ADDED BR-COL-QUEUE-LOW-TO-HI
      * MJD 071797 ADDED BR-CAN-GROUP-ID.
      * KEC 072297 ADDED BR-CREDIT-BUREAU-SERVICES (5 BYTES).
      * MJD 072397 ADDED BR-AUTO-LNSTAT-WINDOW.
      * BLV 072497 ADDED BR-TIME-ZONE-FACTOR.
      * MJD 072897 ADDED BR-LONPF-OPT-DISPLAY.
      * JTG 081397 ADEED BR-DUEDATE-CHANGE-CD RELIANCE, #63
      * KEC 082097 ***CONVERSION-A90-TO-A10***
      *               ADDED BR-GIFTNET, BR-GIFTNET-LEDGER-LINE.
      * KEC 082697 REMOVED BR-DL-CHECK-NAME-CHG PER BETH BECAUSE OF PR#0543;
      *             CHANGING OF PAYEE IS ALREADY ALLOWING, SO THIS FLAG IS
      *             NOT NECESSARY.  MADE INTO FILLER.
      * MJD 082797 ADDED BR-NO-FT-EMP AND BR-NO-PT-EMP FOR BYMISC
      * MJD 082797 ADDED BR-NO-FT-EMP AND BR-NO-PT-EMP FOR MN/BYMISC.C
      * JTG 092597 ADDED BR-BALLOON-LOANS-FG, CASTLE FINANCE
      * BLV 103097 ADDED BR-GL-ENTRY-DEFAULT, BR-GL-TYPE-ALLOWED
      * BLV 110497 ADDED BR-MIDSTREAM-1STPAY-FG
      * JTG 111197 ADDED BR-DUEDATE-CHANGE-CD OPTION 'M' MERCURY
      * JTG 111197 ADDED BR-NODEF-1ST-X-MONTHS OPTION MERCURY
      * JTG 112197 EXPANDED RECORD BY 52 BYTES 798 --> 850
      * JTG 120197 ADDED WORLD BR-ADD-INS-FEECODE AD&D INSURANCE GA
      *                        BR-ROAD-G-SGL-FEECODE FOR VOYAGER
      *                        BR-ROAD-G-FAM-FEECODE
      * JTG 120397 ADDED BR-NODEF-IF-ZERO-AMT OPTION MERCURY
      * KEC 121797 ADDED BR-FXPRESS-OPTION FOR FORMSXPRESS FORMS.
      * MJD 012898 ADDED BR-NEXT-SOLICIT
      * BAH 022698 ADDED BR-ADDON-FEECD-O3 PIC XX OCCURS 2
      * BAH 022698 REMOVED BR-MEDIA-PURGE
      * MJD 031398 ADDED BR-SIZE - 78 LEVEL DOES NOT AFFECT REC SIZE.
      * BAH 032498 ADDED BR-IMMEDIATE-RECEIPT AND BR-IMMEDIATE-DOC-PRINT
      *            FROM A90 FOR WORLD...
      * BAH 042998 CHANGED BR-ADDON-FEECD TO BR-ADDON-FEECD-O2, BECAUSE WE
      *            DONT ALLOW FEES ON OTHER 1 ANYMORE..
      * BLV 052298 ADDED BR-CHECK-EOM-FG: CHECK EOM & CALENDAR EOY
      *            COMPLETE BEFORE ALLOWING LONPA0 TO OPEN DAY
      * BAH 060298 ADDED BR-AUTO-PL-REF-AC FOR MERCURY #E180 TO AUTOMATICALLY
      *            REFUND EVERYTHING AND ACCELERATE THE ACCOUNT
      * BLV 090298 TRASHED BR-EOM1-PROCEDURE, BR-EOM2-PROCEDURE, THESE ARE
      *            NOT USED & ARE CONFUSING PEOPLE
      * KEC 100698 ADDED BR-REVOLVING & BR-FAX-NUMBER. REGENCY REVOVLING
      * JTG 101298 CHANGED BR-REVOLVING TO BR-REVOLVING-SF-FG
      *                          AND ADDED BR-REVOLVING-LN-FG
      * CS  111798 ADDED BR-ALPHA-DRAWERS
      * KEC 011599 ADDED BR-REVOLVING-CONV-DATE SO TO PREVENT ANY
      *             UPDATES TO 'GL' FOR MANUAL CONVERSION OF REVOLVING
      *             ACCOUNTS (REGENCY).
      * MJD 990125 ----- UPDATED IN FLEXGEN -----
      * CS  1999.0312 >>>>>>>> 'A10-TO-A15' CONVERSION UPDATED <<<<<<<<
      * GLF 990323 ADDED BR-LATECHG-ON-ALLOTMENTS, TFC #018
      * MJD 990323 ADDED BR-PRINTERFG-VALID. 88 LEVEL FOR VALID PRINTER
      *            CODES.
      * MJD 990412 CHANGED BR-BP-NEXT-BATCH TO BE PIC 9(2) COMP-3
      * KEC 1999.0510 >>>>>>>> 'A60-TO-A15' CONVERSION UPDATED <<<<<<<<
      * MJD 990617 ADDED BR-ALLOW-SYS-AS-EFFDATE (REG MGMT REQ # 867)
      * GLF 990715 ADDED BR-TAX-PREBOOK-CUST AND BR-TAX-PREBOOK-NONCUST
      *            WORLD PR#211
      * GLB 990715 ADD BR-PASSWORD-DRAWER FLAG, PR #901, REGMGM
      * BAH 990723 REMOVED BR-FXPRESS-OPTION
      * KEC 1999.0805 >>>>>>>> 'A10-TO-A15' CONVERSION UPDATED <<<<<<<<
      * KEC 1999.0811 >>>>>>>> 'A60-TO-A15' CONVERSION UPDATED <<<<<<<<
      * MJD 990901 ADDED BR-INBR-BILNSC-FG-VALID - FLAG NOW CAN HAVE
      *            VALUE OF "M" (MUL PR# 925)
      * JTG 991129 ADDED BR-ONESTEP-RN-FG, TO ALLOW ONE STEP RENEWALS, WORLD
      * KEC 991130 ADDED BR-LI-LISCAN-FG; FOR CMSI/CSI PROJECT
      * MJD 000223 ADDED BR-HIDE-END-BAL (WORLD PR#219)
      * KEC 2000.0410 >>>>>>>> 'A10-TO-A15' CONVERSION UPDATED <<<<<<<<
      * MJD 000420 ADDED A SECOND SET OF BATCH ALLOTMENT FLAGS FOR LOCK
      *            BOX PROCESSING (MERC PR#227)
      * BAH 000531 ADDED NEW VALUE FOR BR-NO-CASH-PROCEEDS "B" TO ALLOW
      *            CASH PROCEEDS ON PB ACCTS ONLY, SUNBELT PR# 1213
      * MJD 000911 ADDED BR-LBOX AND BR-LATECHG-ON-LBOX (MFC PR#227)
      * JTG 001117 ADDED BR-ALLOW-OV-TRCD FOR 'OV' OVERPAYMENT TRANSACTION
      *                                                            PR#J015
      * MJD 001120 ADDED BR-GLOBAL-QUEUE (TFC PR#60)
      * JTG 001218 ADDED THE FOLLOWING FIELDS, RE: GP-ALLOW-SET-FOR-NONCASH
      *            MERCURY PR#239
      *            03  BR-BP-PAYOFF-NONCASH      PIC X.
      *            03  BR-BP-PAYOFF-OV           PIC X.
      *            03  BR-LBOX-PAYOFF-NONCASH    PIC X.
      *            03  BR-LBOX-PAYOFF-OV         PIC X.
      * BLV 010604 ADDED BR-ALLOW-INTERBR - IF "N" DON'T ALLOW OTHER
      *            BRANCHES TO POST INTERBRANCH PAYMENTS TO THIS BRANCH,
      *            REGACC PR# 1510
      * BAH 010621 NO PHYSICAL CHANGE, JUST CHANGED THE USE OF REB-METHOD-2
      *            SUBSCRIBT 4 FROM O6 TO CL(LEV), THE LOAN RECORD DOES NOT
      *            EVEN ALLOW O6, REGWAN PR# 1500
      * BLV 011207 ADDED BR-COLL-QUEUE-PAST-DUE, REGACC PR# 1677
      * JTG 020208 ADDED BR-FORCE-VALID-CITY-COUNTY                 WORLD PR#288
      * CS  020926 ADDED BR-LI-DLCOMM-CMSI-PP-MAX, BR-LI-DLCOMM-CMSI-O3-MAX..FOR
      *            MAXIUMUM AMOUNT OF DEALER COMMISSION FOR PP & O3 INSURANCE
      *            FED FROM CSMI(LOAN APP INTERFACE UPD) TO LOAN MANAGER
      * BAH 030507 ADDED BR-ASSESS-LATE-CHGS FOR MODERN PR# 2036, THEY WILL
      *            SET THIS TO "N", THEY DONT WANT LATE CHARGES IN LEGAL BRANCH
      * BAH 030917 REMOVED BR-FAIRCO-CPCORP-FG, BR-LNCHGS-WINDOW,
      *            BR-FAIRCO-TRW-OPT
      * BAH 030918 REMOVED BR-AS-TRAILER-NUMBER,MADE FILLER X(5),
      *            NOT USED ANYWHERE
      * KEC 030918 {PR#00039} REGIONAL ACCEPTANCE <A15> [KEVIN MULLEN]
      *            ADDED   03  BR-LR-GROUP-COLLID       PIC X.
      * BLV 031112 ADDED BR-INS-PRODUCER, WORLD NEEDS THIS FIELD IN BRANCH,
      *            NOT GLOBAL FOR CLAIMS; REDEFINED BR-STMNT2 SINCE ALMOST
      *            OUT OF SPACE
      * BLV 040218 ADDED BR-JE-ALLOCATION-SKIP, FLAG TO CAUSE GL JOURNAL ENTRY
      *            ALLOCATIONS TO SKIP BRANCH, WORLD
      * JTG 040219 ALLOWED NON MONTHLY CAN LOANS (NMQUOT.C) WORLD J043
      *             ADDED BR-NON-MONTHLY-CAN-LOANS
      * BLV 040315 RENAME BR-JE-ALLOCATION-SKIP TO BR-WORLD-BRANCH-TYPE, WILL
      *            BE USED BY WORLD TO INDICATE STATE ADMIN BRANCHES, 1ST &
      *            2ND YEAR DENOVO'S
      * BLV 040323 OKAY, NEED BOTH BR-JE-ALLOCATION-SKIP & WORLD-BRANCH-TYPE,
      *            SO RENAMED THE ALLOCATION FLAG BACK & ADDED THE TYPE, WORLD
      * BLV 040615 ADDED BR-COL-QUEUE-BEHAVIOR-FG, WHETHER TO ORDER QUEUE
      *            BY BEHAVIOR GRADE, REGACC PR# 59
      * JTG 040826 ALLOW BATCH PAYMENTS TO ACCOUNTS WITH OTHER BALANCE BUT
      *            NOT APPLY APPLY MONIES TO OTHER BALANCE          REGACC #0070
      * BAH 040830 ADDED BR-REGION-NUMBER FOR REGACC PR# 76
      * BAH 150930 CHANGED FROM RELATIVE TO INDEXED, PD0005
      * BAH 151020 COMBINED WITH B2FILE, CHANGED COMP-6 TO COMP-3, ADDED
      *            FILLER, INCREASED BR-OPEN-BAL TO OCCURS 20, REMOVED
      *            BR-REVOLVING-CONV-DATE, MADE BR-INS-PRODUCER ITS OWN
      *            FIELD INSTEAD OF REDEFINE OF STMNT2, INCREASED
      *            BR-REB-METHOD AND METHOD-2 TO 99, PD0003
      *  CS 160120 REMOVED BR-REB-METHOD-2 FIELDS
      * BLM 160914 ADD BR-ALLOW-SCANNED-CKS, WORLD PR# 1188
      * BLM 160915 ADD BR-CHECK-DEPOSIT-GL, WORLD PR# 1188 
      * AMM 161031 ADD BR-OPTION-PST-LASER, WORLD PR# 1200
      * BAH 180214 ADDED BR-USE-DEBIT-CARDS, BR-GL-DEBIT-CARD-PROCEEDS
      *            FOR NETSPEND, WORLD PR#1306
      * AMM 181113 REMOVED CREDIT BUREAU SERVCES FIELDS FORM RECORD LAYOUT
      *            SDP_002_T02
      *  CS 190124 ADD BR-WKDAY-BANKID FOR BANK INTEGRATION WORKDAY PR#1353A
      *  CS 190408 ADD FUTURE FIELDS,SAVED GB01BR.190408
      *  BAH 20190625 REMOVED BR-REVOLVING-SF-FG BR-REVOLVING-LN-FG PT0153
      *  BAH 20190702 REMOVED BR-USE-PARTIC-FLAT-TABLE PT0051
      *  BAH 20190730 ADDED BR-PURGE-NOW-FLAG TO REPLACE THE "PURGEME" TAG
      *               IN LNARCH. LNARCH GETS PUT IN A BATCH AND ONLY THE
      *               BRANCHES WITH THIS FLAG SET WILL ACTUALLY PURGE
      *  BAH 20200323 CLEANED UP FUTURE FIELDS, REMOVED GROUP LEVELS
      *  BAH 20200326 ADDED BR-EMAIL AND BR-MANAGER-EMAIL
      *  BAH 20210421 REMOVED FUTURE FIEKDS. MOVE BR-NO TO BE GROUP LEVEL
      *  BAH 20220921 ADDED BR-VERITEC-LICENSE FOR "IL" VERITEC EXTRACTS #1570
      *  BAH 20230419 ADDED BR-TEST-PP-AUTO-INSCOVR #1594A
      **************************************************************************
       78  BR-SIZE                      VALUE 1215.
       01  BR-REC.
           03  BR-KEY.
               05  BR-NO                PIC 9(4).
      $XFD DATE=YYYYMMDD
           03  BR-LTOUCH-DATE           PIC 9(8)     COMP-3.
           03  BR-NAME                  PIC X(40).
           03  BR-ADR1                  PIC X(24).
           03  BR-ADR2                  PIC X(24).
           03  BR-CITY                  PIC X(15).
           03  BR-STATE                 PIC XX.
           03  BR-ZIP                   PIC X(10).
           03  BR-PHONE                 PIC 9(10)    COMP-3.
           03  BR-RETAIN                PIC 9(11)    COMP-3.
           03  BR-NEXT-CHECK            PIC 9(6)     COMP-3.
           03  BR-PRINTERS.
               05  BR-PRINTTAB         OCCURS 4.
                   07  BR-PRINTER       PIC 99.
           03  BR-FEE-GL-BRNO           PIC 9(4).
           03  BR-REMIT1                PIC X(30).
           03  BR-REMIT2                PIC X(30).
           03  BR-REMIT3                PIC X(30).
           03  BR-STMNT1                PIC X(60).
           03  BR-STMNT2                PIC X(60).
           03  BR-INS-PRODUCER-NUM      PIC 9(8).
           03  BR-DLRCHK                PIC X.
           03  BR-FRONT-END-FG          PIC X.
           03  BR-EMLNRG-PHONE          PIC X.
           03  BR-POST-GL               PIC X.
           03  BR-ALLOTMNTS             PIC X.
           03  BR-EXTRACT-AUTO-UPD      PIC X.
           03  BR-PCNI-LEDGER           PIC X.
           03  BR-PRINTERFGS.
               05  BR-PRINTFGTAB       OCCURS 4.
                   07  BR-PRINTERFG     PIC X.
                       88 BR-PRINTERFG-CENTRONICS   VALUE "C".
                       88 BR-PRINTERFG-NEC          VALUE "N".
                       88 BR-PRINTERFG-OKIDATA      VALUE "O".
                       88 BR-PRINTERFG-OKIDATA-IBM  VALUE "I".
                       88 BR-PRINTERFG-LASER        VALUE "L".
                       88 BR-PRINTERFG-LASER-IIP    VALUE "2".
                       88 BR-PRINTERFG-PANASONIC    VALUE "P".
                       88 BR-PRINTERFG-DEFAULT      VALUE " ".
                       88 BR-PRINTERFG-VALID VALUE "C" "N" "O" "L"
                                                   "2" "P" " " "I".
           03  BR-NEGDLRSV              PIC X.
           03  BR-CPCORP-ORDER          PIC X.
           03  BR-PRIO-MEMO-FLAG        PIC X.
           03  BR-PREBILL-PL-JD-OPT     PIC X.
           03  BR-LOAN-NUM-FLAG         PIC X.
           03  BR-DOWNPY-FLAG           PIC X.
           03  BR-AUTO-INS-EARN         PIC X.
           03  BR-OTH-INS-LEVELS        PIC X.
           03  BR-ALLOW-INTERBR         PIC X.
           03  BR-UNER-DLANRP           PIC X.
           03  BR-SECOND-PROMISE        PIC X.
           03  BR-AVAIL-CREDIT          PIC X.
           03  BR-RECP1                 PIC X(30).
           03  BR-RECP2                 PIC X(30).
           03  BR-INC-WAIVERS           PIC X.
           03  BR-INC-INS               PIC X.

      *     BATCH PAYMENT FIELDS:
           03  BR-BP-NEXT-BATCH         PIC 9(2). 
           03  BR-BP-NOPAYOFF           PIC X.
           03  BR-BP-ODDPAY             PIC X.
           03  BR-BP-LC                 PIC X.
           03  BR-BP-NOPRIN             PIC X.
           03  BR-BP-FLASH              PIC X.
           03  BR-BP-ACTIONCD           PIC X.
           03  BR-BP-MAXCON             PIC 9(3)     COMP-3.
           03  BR-BP-MAXREC             PIC 9(3)     COMP-3.
           03  BR-BP-PL                 PIC X.
           03  BR-BP-JD                 PIC X.
           03  BR-BP-BRANCH             PIC 9(5)     COMP-3.

           03  BR-AGE-ERN               PIC X.
           03  BR-OTHER-BRANCH-GL       PIC X.
           03  BR-SALES-SMALL           PIC X.
           03  BR-NEW-COLL-INQUIRY      PIC X.
           03  BR-CHANGE-MEMO           PIC X.
           03  BR-INS-ADDON             PIC X.
               88  BR-INS-ADDON-VALID  VALUE "O" "Y" "N".
               88  BR-INS-ADDON-ORIG   VALUE "O".
               88  BR-INS-ADDON-NEW    VALUE "Y".
           03  BR-TRAILER-REC           PIC X.
           03  BR-ALDEL-FLAG            PIC X.
           03  BR-IB-LCHG-MESS          PIC X.
           03  BR-COL-QUEUE-PAST-DUE    PIC X.
           03  BR-LI-DLCOMM-CMSI-PP-MAX PIC 9(4)V99 COMP-3.
           03  BR-ADDON-FEECD-O2        PIC XX OCCURS 2 TIMES.
           03  BR-AUTO-CHECK            PIC X.
           03  BR-RECEIPT-PROMDATE      PIC X.
           03  BR-RECEIPT-INTPD         PIC X.
           03  BR-COLL-ID-SCAN          PIC X.
           03  BR-STOP-ON-COLL          PIC X.
           03  BR-DUEDATE-CHANGE-CD     PIC X.
           03  BR-CHECK-EOM-FG          PIC X.
           03  BR-VOLUME-DLANRP         PIC X.
           03  BR-PDTH-NXDUE            PIC X.
           03  BR-LC-CODE               PIC X.
           03  BR-VALIDATE-PRUS         PIC X.
           03  BR-AUTO-OPEN-BAL         PIC X.
           03  BR-OPEN-BALS.
               05  BR-OPEN-BAL          PIC S9(7)V99 COMP-3 OCCURS 20.
           03  BR-TENDER-CD             PIC X(5)     OCCURS 3.
           03  BR-DLPARV-ADJ-FG         PIC X.

           03  BR-OVERRIDE-BR-STATE     PIC X.
           03  BR-DEFAULT-SC-CLASS      PIC 9(3).
           03  BR-RECDEL-NOT-REC        PIC X.
           03  BR-POST-AHEAD-FG         PIC X.
           03  BR-CAN-LOANS-FG          PIC X.
               88  BR-CAN-LOANS-YES                  VALUE "Y".
               88  BR-CAN-LOANS-NO                   VALUE "N".
               88  BR-CAN-LOANS-ONLY                 VALUE "O".
               88  BR-CAN-LOANS-VALID                VALUE "Y"
                                                           "N" "O".
J043       03  BR-NON-MONTHLY-CAN-LOANS      PIC X.
0070       03  BR-BP-ALLOW-PMT-ACCT-OTHBAL   PIC X.
0070       03  BR-LBOX-ALLOW-PMT-ACCT-OTHBAL PIC X.
           03  BR-REGION-NUMBER              PIC 9(2).
           03  BR-OUR-FEE                    PIC X.

      *=================================================================
      *   THIS IS HOW IT IS USED IN A15.
      *    CL(DEC)  -  1      O1      - 4
      *    AH       -  2      O2      - 5
      *    PP       -  3
      *---------------------------------------------------------------
      *    ONLY METHOD2(1) AND METHOD2(4) ARE USED
      *    O3       -  1      CL(LEV) - 4
      *    O4       -  2      N/A     - 5
      *    O5       -  3
      * THE LOAN RECORD ONLY HAS O1 - O5, SO I USED O6 FOR CREDIT LIFE
      * LEVEL, REGWAN WANTS CREDIT LIFE DEC AND CREDIT LIFE LEV
      *---------------------------------------------------------------
      *   IN A30 
      *    CL(DEC)  -  1      O2       - 5     
      *    AH       -  2      O3       - 6     O5     -  9 
      *    PP       -  3      CL(LEV)  - 7     O6     - 10      
      *    O1       -  4      O4       - 8     O7     - 11
      *---------------------------------------------------------------

           03  BR-REB-METHOD            PIC 99       OCCURS 11.
           03  BR-REBDAYS-MONTH         PIC 99       OCCURS 11.
           03  BR-ACCRUAL-MONTHS        PIC 999      COMP-3 OCCURS 11.

      *=================================================================
           03  BR-GLOVERSHORT           PIC 9(11)    COMP-3.
           03  BR-GLBANK                PIC 9(11)    COMP-3.
           03  BR-GLONHAND              PIC 9(11)    COMP-3.
           03  BR-BANKBAL               PIC S9(9)V99 COMP-3.
           03  BR-RETAIN-AMT            PIC S9(9)V99 COMP-3.
           03  BR-LCARDFG               PIC X.

           03  BR-DATA-PATH             PIC X(6).
           03  BR-MACHINE               PIC XX.
           03  BR-MANAGER               PIC X(25).

           03  BR-EOD-PROCEDURE         PIC XXX.
           03  BR-BP-PAYOFF-NONCASH     PIC X.
           03  BR-BP-PAYOFF-OV          PIC X.
           03  BR-LI-DLCOMM-CMSI-O3-MAX PIC 9(4)V99 COMP-3.
           03  BR-LR-GROUP-COLLID       PIC X.

           03  BR-UNITPER-FG            PIC X.
           03  BR-UNITPER-Y-D-FG        PIC X.

           03  BR-NO-PROMPT-FOR-OTHBR    PIC X.
           03  BR-WORLD-BRANCH-TYPE      PIC X.
           03  BR-COL-QUEUE-BEHAVIOR-FG  PIC X.
           03  BR-CONVERT-AUTO-ACCTNO    PIC X.
           03  BR-TS-REPO-INS-FG         PIC X.
           03  BR-BOOK-SF-BR-ONLY        PIC X.
           03  BR-FORCE-VALID-CITY-COUNTY PIC X.
           03  BR-SET-COLLCD-DL-MFGCD    PIC X.
           03  BR-DEFAULT-SF-UNITPER-CD  PIC X.
           03  BR-DEFAULT-SF-PURCD       PIC X(3).
           03  BR-DEFAULT-LN-UNITPER-CD  PIC X.
           03  BR-DEFAULT-LN-PURCD       PIC X(3).
           03  BR-TAP-OTHER-DL           PIC X.
           03  BR-AH-AND-CL              PIC X.
           03  BR-NO-CASH-PROCEEDS       PIC X.
               88  BR-NO-CASH-PROCEEDS-VALID  VALUE "Y" "N" "B".
               88  BR-CASH-PB-ONLY       VALUE "B".
           03  BR-RI-BEFORE-REPO         PIC X.
           03  BR-RI-BEFORE-P4P5         PIC X.
           03  BR-NOT-OURS-REFUND        PIC X.
           03  BR-SFDLO3-WINDOW-FG       PIC X.
           03  BR-CALC-INS-ADJ           PIC X.
           03  BR-AC-BEFORE-REPO         PIC X.
           03  BR-AC-BEFORE-P4P5         PIC X.
           03  BR-BANK-ROUTING-NO        PIC 9(9)   COMP-3.
           03  BR-BANK-CLEARING-GL       PIC 9(11)  COMP-3.
           03  BR-GLBANK-DEPOSIT         PIC 9(11)  COMP-3.
           03  BR-BANK-ACCTNO            PIC X(17).
           03  BR-DEFAULT-SC-SUBCLASS    PIC 9(3).
           03  BR-NO-OUT-DLR-RSV         PIC X.
           03  BR-COLLECTOR-QUEUE        PIC X.
           03  BR-COLL-QUEUE-CON-REC     PIC X.
           03  BR-JE-ALLOCATION-SKIP     PIC X.
           03  BR-EXPAND-APP-INPUT       PIC X.
           03  BR-A50-CPCORP             PIC X.
           03  BR-INBR-BILNSC-FG         PIC X.
               88  BR-INBR-BILNSC-FG-VALID   VALUE "Y" "N" "M".
      *        ---------------------------------------------------------
      *         "Y" : USE GLOBAL SCAN WINDOW FOR INTER BRANCH SCAN.
      *         "N" : DO NOT USE GLOBAL SCAN WITH INTERBRANCH POSTING
      *               POP WINDOE TO ENTER BRNO AND GO TO THAT BRANCH.
      *         "M" : SAME AS "N" ABOVE PLUS THE FOLLOWING: WHEN IN
      *               COLLECTIONS OR F10 INT. GLOBAL SCAN WILL BE
      *               REPLACED WITH AN ENTRY BOX FOR BRNO ACCTNO TO VIEW.
      *        ---------------------------------------------------------
           03  BR-INBR-BRIEF-FG          PIC X.
           03  BR-LEDGER-JUST-INTCHG     PIC X.
           03  BR-BYINQ-START-PAGE       PIC 99.
           03  BR-DSINQ-START-PAGE       PIC 99.
           03  BR-APP-REQUIRED-FOR-INPUT PIC X.
           03  BR-CREDIT-SCORING-FG      PIC X.
           03  BR-COL-QUEUE-LOW-TO-HI    PIC X.
           03  BR-CAN-GROUP-ID           PIC X(4).
           03  BR-AUTO-LNSTAT-WINDOW     PIC X.
           03  BR-TIME-ZONE-FACTOR       PIC S9.
           03  BR-LONPF-OPT-DISPLAY      PIC X.
           03  BR-GIFTNET                PIC X.
           03  BR-GIFTNET-LEDGER-LINE    PIC X.
           03  BR-NO-FT-EMP              PIC 9(3)  COMP-3.
           03  BR-NO-PT-EMP              PIC 99V99 COMP-3.
           03  BR-BALLOON-LOANS-FG       PIC X.
           03  BR-GL-TYPE-ALLOWED        PIC X.
           03  BR-GL-ENTRY-DEFAULT       PIC X.
           03  BR-MIDSTREAM-1STPAY-FG    PIC X.
           03  BR-NODEF-1ST-X-MONTHS     PIC 9.
           03  BR-ADD-INS-FEECODE        PIC XX.
           03  BR-ROAD-G-SGL-FEECODE     PIC XX.
           03  BR-ROAD-G-FAM-FEECODE     PIC XX.
           03  BR-NODEF-IF-ZERO-AMT      PIC X.
           03  BR-ASSESS-LATE-CHGS       PIC X.
           03  BR-NEXT-SOLICIT-NO        PIC 9(6) COMP-3.
           03  BR-ADDON-FEECD-O3         PIC XX OCCURS 2 TIMES.
           03  BR-IMMEDIATE-RECEIPT      PIC X.
           03  BR-IMMEDIATE-DOC-PRINT    PIC X.
           03  BR-AUTO-PL-REF-AC         PIC X.
               88  BR-AUTO-PL-REF-AC-VALID  VALUE "Y" "2" "3".
           03  BR-FAX-NUMBER             PIC 9(10)    COMP-3.
           03  BR-ALPHA-DRAWERS          PIC X.
           03  BR-LATECHG-ON-ALLOTMENTS  PIC X.
           03  BR-ALLOW-SYS-AS-EFFDATE   PIC X(01).
           03  BR-TAX-PREBOOK-CUST       PIC 9(4) COMP-3.
           03  BR-TAX-PREBOOK-NONCUST    PIC 9(4) COMP-3.
           03  BR-PASSWORD-DRAWER        PIC X.
           03  BR-ONESTEP-RN-FG          PIC X.
           03  BR-LI-LISCAN-FG           PIC X.
           03  BR-HIDE-END-BAL           PIC X(01).
      *     BATCH PAYMENT FIELDS (LOCK BOX PROCESSING):
           03  BR-LBOX-NOPAYOFF          PIC X.
           03  BR-LBOX-ODDPAY            PIC X.
           03  BR-LBOX-LC                PIC X.
           03  BR-LBOX-NOPRIN            PIC X.
           03  BR-LBOX-FLASH             PIC X.
           03  BR-LBOX-ACTIONCD          PIC X.
           03  BR-LBOX-MAXCON            PIC 9(3)     COMP-3.
           03  BR-LBOX-MAXREC            PIC 9(3)     COMP-3.
           03  BR-LBOX-PL                PIC X.
           03  BR-LBOX-JD                PIC X.
           03  BR-LBOX                   PIC X(01).
           03  BR-LATECHG-ON-LBOX        PIC X(01).
           03  BR-ALLOW-OV-TRCD          PIC X.
           03  BR-GLOBAL-QUEUE           PIC X(01).
           03  BR-LBOX-PAYOFF-NONCASH    PIC X.
           03  BR-LBOX-PAYOFF-OV         PIC X.

           03  BR-COLLECT-MANAGER      PIC X(25).
           03  BR-COLLECT-PHONE        PIC 9(10) COMP-3.
           03  BR-COLLECT-EXT          PIC 9(4)  COMP-3.

           03  BR-ALT-INTO-THIS-BRANCH PIC X.
           03  BR-POFF-TITLE-WINDOW    PIC X.
           03  BR-COUPON-STATEMENT     PIC X.
           03  BR-SUPERVISOR           PIC X(25).
           03  BR-AREA-MANAGER         PIC X(25).

           03  BR-MIN-MONTHS-DEF       PIC 99.

      * COLLECTION CENTER INFO, PR# 164 REGACC
           03  BR-CC-FLAG              PIC X.
           03  BR-CC-ADR1              PIC X(24).
           03  BR-CC-CITY              PIC X(15).
           03  BR-CC-STATE             PIC XX.
           03  BR-CC-ZIP               PIC X(10).
      *USED TO INTERRUPT GRADE CODES COMING FROM CMSI TO POPULATE 
      *PARTIC. HELD
           03  BR-MIN-MONTHS-D2          PIC 99. 
           03  BR-SKIP-MIDSTREAM-TRW     PIC X.
           03  BR-GL-ESCROW-BALANCE      PIC 9(11) COMP-3.
           03  BR-GL-ESCROW-PAYABLE      PIC 9(11) COMP-3.
           03  BR-ALLOW-FORCE-OPEN-FG    PIC X.
           03  BR-ACH-BANK-NO            PIC 9.
           03  BR-WIMENU-CHECK-WRITING   PIC X.
           03  BR-ADP-PAY-GROUP          PIC X(3).
           03  BR-ALLOW-SCANNED-CKS      PIC X.
           03  BR-CHECK-DEPOSIT-GL       PIC 9(11) COMP-3.
           03  BR-OPTION-PST-LASER       PIC X.
           03  BR-USE-DEBIT-CARDS        PIC X.
           03  BR-GL-DEBIT-CARD-PROCEEDS PIC 9(11) COMP-3.
           03  BR-WKDAY-BANKID           PIC 9(4) COMP-3.
           03  BR-PURGE-NOW-FLAG         PIC X.
           03  BR-EMAIL                  PIC X(50).
           03  BR-MANAGER-EMAIL          PIC X(50).
           03  BR-VERITEC-LICENSE        PIC 9(7).
      *     03  BR-TEST-PP-AUTO-INSCOVR   PIC X.
	  

      * COPY "LIBGB/GB01BR_SQL.CPY".
      * COPYMEMBER: LIBGB/GB01BR_SQL
      ******************************************************************
      *
      * DESCRIPTION: SQL - BRANCH MASTER FILE
      *
      *=================================================================
      * REV:
      * AMM 2021-0430 ***NEW***
      * BAH 20220923 ADDED QBR-VERITEC-LICENSE #1570
      * BAH 20230419 ADDED QBR-TEST-PP-AUTO-INSCOVR #1594A
      ******************************************************************
      *- - -{STANDARD CURSOR FOR START & READ-NEXT}
       01  BR-CURSOR-STAT               PIC X(2)  VALUE SPACES.
           88  BR-CURSOR-STAT-GOOD                VALUE "00".
           88  BR-CURSOR-STAT-BAD                 VALUE "  " THRU "//"
                                                        "01" THRU "~~".

       EXEC SQL BEGIN DECLARE SECTION END-EXEC.

       01  QBR-WHERE-BEG-KEY.
           03  QBR-WBEG-NO               PIC 9(4).
 
       01  QBR-WHERE-END-KEY.
           03  QBR-WEND-NO               PIC 9(4).
 
       01  QBR-REC.
           03  QBR-NO                                 PIC 9(4).
           03  QBR-LTOUCH-DATE                        PIC X(10).
           03  QBR-NAME                               PIC X(40).
           03  QBR-ADR1                               PIC X(24).
           03  QBR-ADR2                               PIC X(24).
           03  QBR-CITY                               PIC X(15).
           03  QBR-STATE                              PIC XX.
           03  QBR-ZIP                                PIC X(10).
           03  QBR-PHONE                              PIC 9(10).
           03  QBR-RETAIN                             PIC 9(11).
           03  QBR-NEXT-CHECK                         PIC 9(6).
           03  QBR-PRINTER-1                          PIC 99.
           03  QBR-PRINTER-2                          PIC 99.
           03  QBR-PRINTER-3                          PIC 99.
           03  QBR-PRINTER-4                          PIC 99.
           03  QBR-FEE-GL-BRNO                        PIC 9(4).
           03  QBR-REMIT1                             PIC X(30).
           03  QBR-REMIT2                             PIC X(30).
           03  QBR-REMIT3                             PIC X(30).
           03  QBR-STMNT1                             PIC X(60).
           03  QBR-STMNT2                             PIC X(60).
           03  QBR-INS-PRODUCER-NUM                   PIC 9(8).
           03  QBR-DLRCHK                             PIC X.
           03  QBR-FRONT-END-FG                       PIC X.
           03  QBR-EMLNRG-PHONE                       PIC X.
           03  QBR-POST-GL                            PIC X.
           03  QBR-ALLOTMNTS                          PIC X.
           03  QBR-EXTRACT-AUTO-UPD                   PIC X.
           03  QBR-PCNI-LEDGER                        PIC X.
           03  QBR-PRINTERFG-1                        PIC X.
           03  QBR-PRINTERFG-2                        PIC X.
           03  QBR-PRINTERFG-3                        PIC X.
           03  QBR-PRINTERFG-4                        PIC X.
           03  QBR-NEGDLRSV                           PIC X.
           03  QBR-CPCORP-ORDER                       PIC X.
           03  QBR-PRIO-MEMO-FLAG                     PIC X.
           03  QBR-PREBILL-PL-JD-OPT                  PIC X.
           03  QBR-LOAN-NUM-FLAG                      PIC X.
           03  QBR-DOWNPY-FLAG                        PIC X.
           03  QBR-AUTO-INS-EARN                      PIC X.
           03  QBR-OTH-INS-LEVELS                     PIC X.
           03  QBR-ALLOW-INTERBR                      PIC X.
           03  QBR-UNER-DLANRP                        PIC X.
           03  QBR-SECOND-PROMISE                     PIC X.
           03  QBR-AVAIL-CREDIT                       PIC X.
           03  QBR-RECP1                              PIC X(30).
           03  QBR-RECP2                              PIC X(30).
           03  QBR-INC-WAIVERS                        PIC X.
           03  QBR-INC-INS                            PIC X.
           03  QBR-BP-NEXT-BATCH                      PIC 9(2).
           03  QBR-BP-NOPAYOFF                        PIC X.
           03  QBR-BP-ODDPAY                          PIC X.
           03  QBR-BP-LC                              PIC X.
           03  QBR-BP-NOPRIN                          PIC X.
           03  QBR-BP-FLASH                           PIC X.
           03  QBR-BP-ACTIONCD                        PIC X.
           03  QBR-BP-MAXCON                          PIC 9(3).
           03  QBR-BP-MAXREC                          PIC 9(3).
           03  QBR-BP-PL                              PIC X.
           03  QBR-BP-JD                              PIC X.
           03  QBR-BP-BRANCH                          PIC 9(5).
           03  QBR-AGE-ERN                            PIC X.
           03  QBR-OTHER-BRANCH-GL                    PIC X.
           03  QBR-SALES-SMALL                        PIC X.
           03  QBR-NEW-COLL-INQUIRY                   PIC X.
           03  QBR-CHANGE-MEMO                        PIC X.
           03  QBR-INS-ADDON                          PIC X.
           03  QBR-TRAILER-REC                        PIC X.
           03  QBR-ALDEL-FLAG                         PIC X.
           03  QBR-IB-LCHG-MESS                       PIC X.
           03  QBR-COL-QUEUE-PAST-DUE                 PIC X.
           03  QBR-LI-DLCOMM-CMSI-PP-MAX              PIC 9(4)V99.
           03  QBR-ADDON-FEECD-O2-1                   PIC XX.
           03  QBR-ADDON-FEECD-O2-2                   PIC XX.
           03  QBR-AUTO-CHECK                         PIC X.
           03  QBR-RECEIPT-PROMDATE                   PIC X.
           03  QBR-RECEIPT-INTPD                      PIC X.
           03  QBR-COLL-ID-SCAN                       PIC X.
           03  QBR-STOP-ON-COLL                       PIC X.
           03  QBR-DUEDATE-CHANGE-CD                  PIC X.
           03  QBR-CHECK-EOM-FG                       PIC X.
           03  QBR-VOLUME-DLANRP                      PIC X.
           03  QBR-PDTH-NXDUE                         PIC X.
           03  QBR-LC-CODE                            PIC X.
           03  QBR-VALIDATE-PRUS                      PIC X.
           03  QBR-AUTO-OPEN-BAL                      PIC X.
           03  QBR-OPEN-BAL-1                         PIC S9(7)V99.
           03  QBR-OPEN-BAL-2                         PIC S9(7)V99.
           03  QBR-OPEN-BAL-3                         PIC S9(7)V99.
           03  QBR-OPEN-BAL-4                         PIC S9(7)V99.
           03  QBR-OPEN-BAL-5                         PIC S9(7)V99.
           03  QBR-OPEN-BAL-6                         PIC S9(7)V99.
           03  QBR-OPEN-BAL-7                         PIC S9(7)V99.
           03  QBR-OPEN-BAL-8                         PIC S9(7)V99.
           03  QBR-OPEN-BAL-9                         PIC S9(7)V99.
           03  QBR-OPEN-BAL-10                        PIC S9(7)V99.
           03  QBR-OPEN-BAL-11                        PIC S9(7)V99.
           03  QBR-OPEN-BAL-12                        PIC S9(7)V99.
           03  QBR-OPEN-BAL-13                        PIC S9(7)V99.
           03  QBR-OPEN-BAL-14                        PIC S9(7)V99.
           03  QBR-OPEN-BAL-15                        PIC S9(7)V99.
           03  QBR-OPEN-BAL-16                        PIC S9(7)V99.
           03  QBR-OPEN-BAL-17                        PIC S9(7)V99.
           03  QBR-OPEN-BAL-18                        PIC S9(7)V99.
           03  QBR-OPEN-BAL-19                        PIC S9(7)V99.
           03  QBR-OPEN-BAL-20                        PIC S9(7)V99.
           03  QBR-TENDER-CD-1                        PIC X(5).
           03  QBR-TENDER-CD-2                        PIC X(5).
           03  QBR-TENDER-CD-3                        PIC X(5).
           03  QBR-DLPARV-ADJ-FG                      PIC X.
           03  QBR-OVERRIDE-BR-STATE                  PIC X.
           03  QBR-DEFAULT-SC-CLASS                   PIC 9(3).
           03  QBR-RECDEL-NOT-REC                     PIC X.
           03  QBR-POST-AHEAD-FG                      PIC X.
           03  QBR-CAN-LOANS-FG                       PIC X.
           03  QBR-NON-MONTHLY-CAN-LOANS              PIC X.
           03  QBR-BP-ALLOW-PMT-ACCT-OTHBAL           PIC X.
           03  QBR-LBOX-ALLOW-PMT-ACCT-OTHBAL         PIC X.
           03  QBR-REGION-NUMBER                      PIC 9(2).
           03  QBR-OUR-FEE                            PIC X.
           03  QBR-REB-METHOD-1                       PIC 99.
           03  QBR-REB-METHOD-2                       PIC 99.
           03  QBR-REB-METHOD-3                       PIC 99.
           03  QBR-REB-METHOD-4                       PIC 99.
           03  QBR-REB-METHOD-5                       PIC 99.
           03  QBR-REB-METHOD-6                       PIC 99.
           03  QBR-REB-METHOD-7                       PIC 99.
           03  QBR-REB-METHOD-8                       PIC 99.
           03  QBR-REB-METHOD-9                       PIC 99.
           03  QBR-REB-METHOD-10                      PIC 99.
           03  QBR-REB-METHOD-11                      PIC 99.
           03  QBR-REBDAYS-MONTH-1                    PIC 99.
           03  QBR-REBDAYS-MONTH-2                    PIC 99.
           03  QBR-REBDAYS-MONTH-3                    PIC 99.
           03  QBR-REBDAYS-MONTH-4                    PIC 99.
           03  QBR-REBDAYS-MONTH-5                    PIC 99.
           03  QBR-REBDAYS-MONTH-6                    PIC 99.
           03  QBR-REBDAYS-MONTH-7                    PIC 99.
           03  QBR-REBDAYS-MONTH-8                    PIC 99.
           03  QBR-REBDAYS-MONTH-9                    PIC 99.
           03  QBR-REBDAYS-MONTH-10                   PIC 99.
           03  QBR-REBDAYS-MONTH-11                   PIC 99.
           03  QBR-ACCRUAL-MONTHS-1                   PIC 999.
           03  QBR-ACCRUAL-MONTHS-2                   PIC 999.
           03  QBR-ACCRUAL-MONTHS-3                   PIC 999.
           03  QBR-ACCRUAL-MONTHS-4                   PIC 999.
           03  QBR-ACCRUAL-MONTHS-5                   PIC 999.
           03  QBR-ACCRUAL-MONTHS-6                   PIC 999.
           03  QBR-ACCRUAL-MONTHS-7                   PIC 999.
           03  QBR-ACCRUAL-MONTHS-8                   PIC 999.
           03  QBR-ACCRUAL-MONTHS-9                   PIC 999.
           03  QBR-ACCRUAL-MONTHS-10                  PIC 999.
           03  QBR-ACCRUAL-MONTHS-11                  PIC 999.
           03  QBR-GLOVERSHORT                        PIC 9(11).
           03  QBR-GLBANK                             PIC 9(11).
           03  QBR-GLONHAND                           PIC 9(11).
           03  QBR-BANKBAL                            PIC S9(9)V99.
           03  QBR-RETAIN-AMT                         PIC S9(9)V99.
           03  QBR-LCARDFG                            PIC X.
           03  QBR-DATA-PATH                          PIC X(6).
           03  QBR-MACHINE                            PIC XX.
           03  QBR-MANAGER                            PIC X(25).
           03  QBR-EOD-PROCEDURE                      PIC XXX.
           03  QBR-BP-PAYOFF-NONCASH                  PIC X.
           03  QBR-BP-PAYOFF-OV                       PIC X.
           03  QBR-LI-DLCOMM-CMSI-O3-MAX              PIC 9(4)V99.
           03  QBR-LR-GROUP-COLLID                    PIC X.
           03  QBR-UNITPER-FG                         PIC X.
           03  QBR-UNITPER-Y-D-FG                     PIC X.
           03  QBR-NO-PROMPT-FOR-OTHBR                PIC X.
           03  QBR-WORLD-BRANCH-TYPE                  PIC X.
           03  QBR-COL-QUEUE-BEHAVIOR-FG              PIC X.
           03  QBR-CONVERT-AUTO-ACCTNO                PIC X.
           03  QBR-TS-REPO-INS-FG                     PIC X.
           03  QBR-BOOK-SF-BR-ONLY                    PIC X.
           03  QBR-FORCE-VALID-CITY-COUNTY            PIC X.
           03  QBR-SET-COLLCD-DL-MFGCD                PIC X.
           03  QBR-DEFAULT-SF-UNITPER-CD              PIC X.
           03  QBR-DEFAULT-SF-PURCD                   PIC X(3).
           03  QBR-DEFAULT-LN-UNITPER-CD              PIC X.
           03  QBR-DEFAULT-LN-PURCD                   PIC X(3).
           03  QBR-TAP-OTHER-DL                       PIC X.
           03  QBR-AH-AND-CL                          PIC X.
           03  QBR-NO-CASH-PROCEEDS                   PIC X.
           03  QBR-RI-BEFORE-REPO                     PIC X.
           03  QBR-RI-BEFORE-P4P5                     PIC X.
           03  QBR-NOT-OURS-REFUND                    PIC X.
           03  QBR-SFDLO3-WINDOW-FG                   PIC X.
           03  QBR-CALC-INS-ADJ                       PIC X.
           03  QBR-AC-BEFORE-REPO                     PIC X.
           03  QBR-AC-BEFORE-P4P5                     PIC X.
           03  QBR-BANK-ROUTING-NO                    PIC 9(9).
           03  QBR-BANK-CLEARING-GL                   PIC 9(11).
           03  QBR-GLBANK-DEPOSIT                     PIC 9(11).
           03  QBR-BANK-ACCTNO                        PIC X(17).
           03  QBR-DEFAULT-SC-SUBCLASS                PIC 9(3).
           03  QBR-NO-OUT-DLR-RSV                     PIC X.
           03  QBR-COLLECTOR-QUEUE                    PIC X.
           03  QBR-COLL-QUEUE-CON-REC                 PIC X.
           03  QBR-JE-ALLOCATION-SKIP                 PIC X.
           03  QBR-EXPAND-APP-INPUT                   PIC X.
           03  QBR-A50-CPCORP                         PIC X.
           03  QBR-INBR-BILNSC-FG                     PIC X.
           03  QBR-INBR-BRIEF-FG                      PIC X.
           03  QBR-LEDGER-JUST-INTCHG                 PIC X.
           03  QBR-BYINQ-START-PAGE                   PIC 99.
           03  QBR-DSINQ-START-PAGE                   PIC 99.
           03  QBR-APP-REQUIRED-FOR-INPUT             PIC X.
           03  QBR-CREDIT-SCORING-FG                  PIC X.
           03  QBR-COL-QUEUE-LOW-TO-HI                PIC X.
           03  QBR-CAN-GROUP-ID                       PIC X(4).
           03  QBR-AUTO-LNSTAT-WINDOW                 PIC X.
           03  QBR-TIME-ZONE-FACTOR                   PIC S9.
           03  QBR-LONPF-OPT-DISPLAY                  PIC X.
           03  QBR-GIFTNET                            PIC X.
           03  QBR-GIFTNET-LEDGER-LINE                PIC X.
           03  QBR-NO-FT-EMP                          PIC 9(3).
           03  QBR-NO-PT-EMP                          PIC 99V99.
           03  QBR-BALLOON-LOANS-FG                   PIC X.
           03  QBR-GL-TYPE-ALLOWED                    PIC X.
           03  QBR-GL-ENTRY-DEFAULT                   PIC X.
           03  QBR-MIDSTREAM-1STPAY-FG                PIC X.
           03  QBR-NODEF-1ST-X-MONTHS                 PIC 9.
           03  QBR-ADD-INS-FEECODE                    PIC XX.
           03  QBR-ROAD-G-SGL-FEECODE                 PIC XX.
           03  QBR-ROAD-G-FAM-FEECODE                 PIC XX.
           03  QBR-NODEF-IF-ZERO-AMT                  PIC X.
           03  QBR-ASSESS-LATE-CHGS                   PIC X.
           03  QBR-NEXT-SOLICIT-NO                    PIC 9(6).
           03  QBR-ADDON-FEECD-O3-1                   PIC XX.
           03  QBR-ADDON-FEECD-O3-2                   PIC XX.
           03  QBR-IMMEDIATE-RECEIPT                  PIC X.
           03  QBR-IMMEDIATE-DOC-PRINT                PIC X.
           03  QBR-AUTO-PL-REF-AC                     PIC X.
           03  QBR-FAX-NUMBER                         PIC 9(10).
           03  QBR-ALPHA-DRAWERS                      PIC X.
           03  QBR-LATECHG-ON-ALLOTMENTS              PIC X.
           03  QBR-ALLOW-SYS-AS-EFFDATE               PIC X(01).
           03  QBR-TAX-PREBOOK-CUST                   PIC 9(4).
           03  QBR-TAX-PREBOOK-NONCUST                PIC 9(4).
           03  QBR-PASSWORD-DRAWER                    PIC X.
           03  QBR-ONESTEP-RN-FG                      PIC X.
           03  QBR-LI-LISCAN-FG                       PIC X.
           03  QBR-HIDE-END-BAL                       PIC X(01).
           03  QBR-LBOX-NOPAYOFF                      PIC X.
           03  QBR-LBOX-ODDPAY                        PIC X.
           03  QBR-LBOX-LC                            PIC X.
           03  QBR-LBOX-NOPRIN                        PIC X.
           03  QBR-LBOX-FLASH                         PIC X.
           03  QBR-LBOX-ACTIONCD                      PIC X.
           03  QBR-LBOX-MAXCON                        PIC 9(3).
           03  QBR-LBOX-MAXREC                        PIC 9(3).
           03  QBR-LBOX-PL                            PIC X.
           03  QBR-LBOX-JD                            PIC X.
           03  QBR-LBOX                               PIC X(01).
           03  QBR-LATECHG-ON-LBOX                    PIC X(01).
           03  QBR-ALLOW-OV-TRCD                      PIC X.
           03  QBR-GLOBAL-QUEUE                       PIC X(01).
           03  QBR-LBOX-PAYOFF-NONCASH                PIC X.
           03  QBR-LBOX-PAYOFF-OV                     PIC X.
           03  QBR-COLLECT-MANAGER                    PIC X(25).
           03  QBR-COLLECT-PHONE                      PIC 9(10).
           03  QBR-COLLECT-EXT                        PIC 9(4).
           03  QBR-ALT-INTO-THIS-BRANCH               PIC X.
           03  QBR-POFF-TITLE-WINDOW                  PIC X.
           03  QBR-COUPON-STATEMENT                   PIC X.
           03  QBR-SUPERVISOR                         PIC X(25).
           03  QBR-AREA-MANAGER                       PIC X(25).
           03  QBR-MIN-MONTHS-DEF                     PIC 99.
           03  QBR-CC-FLAG                            PIC X.
           03  QBR-CC-ADR1                            PIC X(24).
           03  QBR-CC-CITY                            PIC X(15).
           03  QBR-CC-STATE                           PIC XX.
           03  QBR-CC-ZIP                             PIC X(10).
           03  QBR-MIN-MONTHS-D2                      PIC 99.
           03  QBR-SKIP-MIDSTREAM-TRW                 PIC X.
           03  QBR-GL-ESCROW-BALANCE                  PIC 9(11).
           03  QBR-GL-ESCROW-PAYABLE                  PIC 9(11).
           03  QBR-ALLOW-FORCE-OPEN-FG                PIC X.
           03  QBR-ACH-BANK-NO                        PIC 9.
           03  QBR-WIMENU-CHECK-WRITING               PIC X.
           03  QBR-ADP-PAY-GROUP                      PIC X(3).
           03  QBR-ALLOW-SCANNED-CKS                  PIC X.
           03  QBR-CHECK-DEPOSIT-GL                   PIC 9(11).
           03  QBR-OPTION-PST-LASER                   PIC X.
           03  QBR-USE-DEBIT-CARDS                    PIC X.
           03  QBR-GL-DEBIT-CARD-PROCEEDS             PIC 9(11).
           03  QBR-WKDAY-BANKID                       PIC 9(4).
           03  QBR-PURGE-NOW-FLAG                     PIC X.
           03  QBR-EMAIL                              PIC X(50).
           03  QBR-MANAGER-EMAIL                      PIC X(50).
           03  QBR-VERITEC-LICENSE                    PIC 9(7).
      *     03  QBR-TEST-PP-AUTO-INSCOVR               PIC X.

       EXEC SQL END DECLARE SECTION END-EXEC.

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
      *----------------------------------------------------------------
       01  WK-IFN        PIC XX       VALUE "WK".

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    using original-sp fields as part of the sql stored procedure
      *    processing; sp-rec is not in the stored procedure but need
      *    to save all of it because sp-rec is maybe being changed
      *    within payoff routine.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       01  original-sp-rec                     pic x(sp-size) value " ".
       01  original-sp1-key.
           03  original-sp-orgst               pic xx.
           03  original-sp-sprclass            pic 999.
           03  original-sp-subclass            pic 99.
           03  original-sp-lawcode             pic 99.

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
           03  BEG-POOLID REDEFINES BEG-DEALER PIC 9(6).
           03  END-DEALER             PIC 9(4)V99.
           03  END-POOLID REDEFINES END-DEALER PIC 9(6).
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
       01  LN-ACCUM-BAL         PIC S9(7)V99 COMP VALUE 0.
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
       01  OPEN-LN3-SW          PIC X     VALUE "N".
       01  START-LN3-SW         PIC X     VALUE "N".

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

       01  D-LINE7  REDEFINES DETAIL-LINE.
           03  FILLER                 PIC X(2).
           03  D-TAG-POOLID           PIC X(8).
           03  D-POOLID               PIC Z(6).
           03  FILLER                 PIC X(2).
           03  D-TAG-REGPAY           PIC X(8).
           03  D-REGPYAMT             PIC $$,$$9.99.
           03  FILLER                 PIC X(2).
           03  D-TAG-DATE-LAST-PAID   PIC X(16).
           03  D-DATE-LAST-PAID       PIC 99/99/99.
           03  FILLER                 PIC X(2).
           03  D-TAG-NO-PAYS-PASTDUE  PIC X(18).
           03  D-NO-PAYS-PASTDUE      PIC ZZ9.99.
           03  FILLER                 PIC X(2).

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


      *-----------------------------------------------------------------
       COPY "LIBGB/PASSWDW.CPY".
       78  UNMASK-SSNO-SEQ     VALUE 1.
      *-----------------------------------------------------------------
       COPY "LIBWI/PFSWINDOWSW.CPY".
       COPY "LIBWI/TSTINTW.CPY".
       COPY "LIBWI/GRSCANW.CPY".
       COPY "LIBWI/LCSCANW.CPY".
       COPY "LIBWI/DLSCANW.CPY".

       COPY "LIBLP/LPAPRSW.CPY".
       COPY "LIBLP/LPITRMW.CPY".
       COPY "LIBLP/LPAPRZW.CPY".
       COPY "LIBLP/LPUPERW.CPY".
       COPY "LIBGB/GRWORKW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/DATERW.CPY".
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
       COPY "LIBCL/CRNOR2W.CPY".
       COPY "LIBLP/LP01NO.CPY".
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
      *    PERFORM CLOSE-LN3-FILE.
           PERFORM CLOSE-LN1-FILE.
           PERFORM CLOSE-LM1-FILE.
           PERFORM CLOSE-LC1-FILE.
           PERFORM CLOSE-BW1-FILE.
           PERFORM CLOSE-GR1-FILE.
           PERFORM CLOSE-BR-FILE.
           PERFORM CLOSE-GB-FILE.
           PERFORM CLOSE-PR-FILE.
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
                                              LM-PATH-OVERRIDE
                                              LN-PATH-OVERRIDE
                                              LP-PATH-OVERRIDE
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
           PERFORM OPEN-LN1-FILE.

           IF OPEN-LN3-SW = "N"
               MOVE "Y" TO OPEN-LN3-SW
               PERFORM OPEN-LN3-FILE.

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
           MOVE 0            TO LN-ACCTNO
                                QLN3-WBEG-ACCTNO.
           MOVE ALL "9"      TO QLN3-WEND-ACCTNO.


      *    IF START-LN3-SW = "N"
      *        MOVE "Y" TO START-LN3-SW
           PERFORM START-LN3-FILE.

       NEXT-LN.

      *    STOP MESS.

           PERFORM READ-LN3-FILE-NEXT.
           IF IO-BAD OR (LN-OWNBR NOT = LN-PATH-OWNBR)
              GO TO PROCESS-WK-FILE.

           PERFORM NPR-TRAP-INTERRUPT.

           IF LN-CLASS > END-CLASS
              GO TO PROCESS-WK-FILE.

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

      * WORLD MEXICO UNION LOANS IDENTIFIED BY POOLID - USING DEALER ON SCREEN
           IF (USERID-BRANCH > 2999 AND < 4000)
              IF LN-DLPOOL < BEG-POOLID OR LN-DLPOOL > END-POOLID
                 GO TO NEXT-LN
              END-IF
           ELSE
              IF LN-DLNO < BEG-DEALER OR LN-DLNO > END-DEALER
                 GO TO NEXT-LN.

           IF LN-SUM-LNAMT < BEG-LNAMT OR LN-SUM-LNAMT > END-LNAMT
              GO TO NEXT-LN.

           MOVE LN-COLLECTOR TO COLCTID-WK.
           PERFORM COLCTID-CD-CHECK.

           IF COLCTID-MATCH NOT = "Y"
              GO TO NEXT-LN.

           MOVE LN-ACTIONCD TO ACTION-WK.
           PERFORM ACTION-CD-CHECK.
           IF ACTION-MATCH NOT = "Y"
              GO TO NEXT-LN.

           IF OWNED = "O"
              IF LN-SERVACCT = "Y"
                 GO TO NEXT-LN.

           IF OWNED = "S"
              IF LN-SERVACCT = "N"
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

       CONT-LN.
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

           IF LEGL = "O"
              IF LN-LEGALDATE = 0
                 GO TO NEXT-LN.

           IF LEGL = "S"
              IF LN-LEGALDATE NOT = 0
                 GO TO NEXT-LN.

           IF DEBT = "O"
              IF LN-DEBTAIDATE = 0
                 GO TO NEXT-LN.

           IF DEBT = "S"
              IF LN-DEBTAIDATE NOT = 0
                 GO TO NEXT-LN.

           IF FROZEN-ISO = "O"
              IF LN-ACCT-FROZEN NOT = "Y"
                 GO TO NEXT-LN.

           IF FROZEN-ISO = "S"
              IF LN-ACCT-FROZEN = "Y"
                 GO TO NEXT-LN.

           IF ZBAL = "O"
              IF LN-CURBAL NOT = 0 OR
                  LN-INTBAL NOT = 0 OR
                   LN-LCBAL NOT = 0 OR
                    LN-OTHBAL NOT = 0
                      GO TO NEXT-LN.

           IF ZBAL = "S"
              IF LN-CURBAL = 0
                 IF LN-INTBAL = 0
                    IF LN-LCBAL = 0
                       IF LN-OTHBAL = 0
                       GO TO NEXT-LN.

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

           MOVE LN-SSNO(1) TO BW-SSNO.
      *    PERFORM READ-BW1-FILE.
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

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    valid ln-rec found; process to create wk-rec.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      d    stop mess.

      *%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
           if ( original-sp-rec not = spaces )
              move original-sp-rec to sp-rec.

           move ln-orgst    to sp-orgst.
           move ln-sprclass to sp-sprclass.
           move ln-subclass to sp-subclass.
           move ln-lawcode  to sp-lawcode.

           if ( original-sp1-key not = sp1-key )
              move sp1-key to original-sp1-key
              perform get-sp
              if io-fg not = 0
                 go to process-wk-file-process.

           move sp-rec to original-sp-rec.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


      *     PERFORM GET-SP.
      *     IF IO-FG NOT = 0
      *        GO TO NEXT-LN.

      *D    STOP MESS.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      * COMPUTE CONTRACTUAL:
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
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
                 GO TO TEST-ACCT-BAL
              ELSE
                 GO TO TEST-PASTPROM-DAY.

           IF PROMISED = "S"
              IF LN-PROMDATE NOT = 0
                 GO TO NEXT-LN
              ELSE
                 GO TO TEST-ACCT-BAL.

           IF PROMISED = "O"
              IF LN-PROMDATE = 0
                 GO TO NEXT-LN.

       TEST-PASTPROM-DAY.
           IF BEG-PAST-PROM = -999
              IF END-PAST-PROM = 999
                 GO TO TEST-ACCT-BAL.

           MOVE LN-PROMDATE TO NUM-DATE.
           MOVE HOLD-TODAY-DATE TO SYS-DATE.
           PERFORM TIM.

           IF ELAPSED-DAYS < BEG-PAST-PROM OR
               ELAPSED-DAYS > END-PAST-PROM
                  GO TO NEXT-LN.

       TEST-ACCT-BAL.
           COMPUTE LN-ACCUM-BAL = LN-CURBAL + LN-OTHBAL + LN-INTBAL
                                  + LN-LCBAL.

           IF LN-ACCUM-BAL < BEG-BAL OR
               LN-ACCUM-BAL > END-BAL
                  GO TO NEXT-LN.

      * MINIMUM AVAILABLE CREDIT
           IF MIN-AVAILCR-X NOT = SPACES
              IF MIN-AVAILCR NOT = 0
                 PERFORM GET-AVAILABLE-CREDIT
                 IF AVCR-AVAILCREDIT < MIN-AVAILCR
                    GO TO NEXT-LN.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      * TEST FOR MATURED ACCOUNTS
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

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
              IF MDTE-DATE < HOLD-TODAY-DATE
                 GO TO NEXT-LN.

           IF MATURED = "O"
              IF MDTE-DATE > HOLD-TODAY-DATE
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
              MOVE 0 To WK-AGE WK-BALDUE
              MOVE SPACES TO WK-NAME.

           IF BRSEQ = "N" OR "2" OR " "
              SUBTRACT CONTRACTUAL FROM 99999 GIVING WK-AGE
              SUBTRACT LN-ACCUM-BAL FROM 9999999.99 GIVING WK-BALDUE
              MOVE SPACES TO WK-NAME.

           IF BRSEQ = "3" OR "6" OR "7"
              MOVE 0 TO WK-AGE WK-BALDUE
              MOVE LN-SSNO(1) TO BW-SSNO
      *       PERFORM READ-BW1-FILE
              IF BW-LNAME = SPACES
                 MOVE "ZZ" TO WK-NAME
              ELSE
                 STRING BW-LNAME "," BW-FNAME
                    DELIMITED BY SIZE
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
              SUBTRACT LN-ACCUM-BAL FROM 9999999.99 GIVING WK-BALDUE
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

      D    STOP MESS.

           PERFORM WRITE-WK-FILE.

           IF IO-BAD
              GO TO IO-ERROR.

           GO TO NEXT-LN.

      D    STOP MESS.

       PROCESS-WK-FILE.

           PERFORM DISABLE-INTERRUPT.
           PERFORM ENABLE-INTERRUPT.

           IF PRU-LP NOT = "VDU" AND NOT = "PRU"
              IF EXT-ROUTE-BUF = "00"
                 MOVE BR-NO TO RUN-MESS-BR
                 MOVE RUN-DISPLAY-MESS TO MESS
                 PERFORM SEND-LEGEND.

           PERFORM CLOSE-LN1-FILE.
           PERFORM OPEN-LN1-FILE.
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

           PERFORM TRAP-INTERRUPT.

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


           MOVE WK-BRNO   TO LN-OWNBR.
           MOVE WK-ACCTNO TO LN-ACCTNO.

           PERFORM READ-LN1-FILE.
           IF IO-BAD
              GO TO PROCESS-WK-FILE-PROCESS.


            PERFORM GET-SP.
            IF IO-FG NOT = 0
              GO TO PROCESS-WK-FILE-PROCESS.

           MOVE LN-ACCTNO       TO CRNOR2-ACCTNO.
           MOVE HOLD-TODAY-DATE TO CRNOR2-SYSDATE.
           MOVE "PO"            TO CRNOR2-LPTRCD.
           MOVE "CL/CRNOR2"     TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH CRNOR2-LINK NO-REC
                                 POFF-WORKERS LN-REC.
           CANCEL FORM-PROGX.

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

           PERFORM ENABLE-INTERRUPT.

           MOVE MKTEMP-DEFAULT TO MKTEMP-BUF.
           PERFORM MKTEMP-CALL.
           PERFORM OPEN-WK-FILE-OUTPUT.
           PERFORM CLOSE-WK-FILE.

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
              MOVE GRSCAN-BUF TO SCAN-BUF
              PERFORM CALL-WINDOW-PROGRAM
              MOVE SCAN-BUF TO GRSCAN-BUF
              IF SCAN-STATUS NOT = "00"
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
      * WORLD MEXICO, ENTER POOLID FOR UNION
           IF (USERID-BRANCH > 2999 AND < 4000)
              MOVE "ENNN060DL1." TO VDU
              PERFORM SCREENIO
           ELSE
              MOVE "ENNN042DL1." TO VDU
              PERFORM SCREENIO.

           IF VDUSTATUS = "10" GO TO BR-01.
           IF VDUSTATUS = "16" GO TO ALL-DL.
           IF VDUSTATUS = "1U" GO TO CL-02.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.

           IF NOT (USERID-BRANCH > 2999 AND < 4000)
              IF VDUSTATUS = "1<"
                 MOVE "DLBEG" TO DEALER-FG
                 PERFORM DEALER-SCAN
                 IF IO-BAD
                    GO TO DL-01.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO DL-01.

           IF (USERID-BRANCH > 2999 AND < 4000)
              MOVE VDUNUM0 TO BEG-POOLID
           ELSE
              MOVE VDUNUM2 TO BEG-DEALER.

      *----------------------------------------------------------------
       DL-02.
      * WORLD MEXICO, ENTER POOLID FOR UNION
           IF (USERID-BRANCH > 2999 AND < 4000)
              MOVE "ENNN060DL2." TO VDU
              PERFORM SCREENIO
           ELSE
              MOVE "ENNN042DL2." TO VDU
              PERFORM SCREENIO.

           IF VDUSTATUS = "10" GO TO BR-01.
           IF VDUSTATUS = "1U" GO TO DL-01.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF NOT (USERID-BRANCH > 2999 AND < 4000)
              IF VDUSTATUS = "1<"
                 MOVE "DLEND" TO DEALER-FG
                 PERFORM DEALER-SCAN
                 IF IO-BAD
                    GO TO DL-02.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO DL-02.

           IF (USERID-BRANCH > 2999 AND < 4000)
              MOVE VDUNUM0 TO END-POOLID
           ELSE
              MOVE VDUNUM2 TO END-DEALER.

           IF (USERID-BRANCH > 2999 AND < 4000)
              IF BEG-POOLID > END-POOLID
                 PERFORM INVALID-RANGE
                 GO TO DL-01
              END-IF
           ELSE
              IF BEG-DEALER > END-DEALER
                 PERFORM INVALID-RANGE
                 GO TO DL-01.

           GO TO ALPHA-01.

      *----------------------------------------------------------------
       ALL-DL.
           IF (USERID-BRANCH > 2999 AND < 4000)
              MOVE "SNNN060DL1." TO VDU
              MOVE 0 TO BEG-POOLID VDUNUM0
              PERFORM SCREENIO
              MOVE "SNNN060DL2." TO VDU
              MOVE 999999 TO END-POOLID VDUNUM0
              PERFORM SCREENIO
           ELSE
              MOVE "SNNN042DL1." TO VDU
              MOVE 0 TO BEG-DEALER VDUNUM2
              PERFORM SCREENIO
              MOVE "SNNN042DL2." TO VDU
              MOVE 9999.99 TO END-DEALER VDUNUM2
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
           IF (USERID-BRANCH > 2999 AND < 4000)
              MOVE "SNNN060DL1." TO VDU
              MOVE BEG-POOLID TO VDUNUM0
              PERFORM SCREENIO
              MOVE "SNNN060DL2." TO VDU
              MOVE END-POOLID TO VDUNUM0
              PERFORM SCREENIO
           ELSE
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
              IF CRNOR2-RDPOT30 = "Y"
                 MOVE "<P>" TO D-RECX
              ELSE
              IF CRNOR2-RDPOT30 = "M"
                 MOVE "<M>" TO D-RECX
              ELSE
              IF CRNOR2-RDPOTCHGOFF = "Y"
                 MOVE "<PC>" TO D-RECX
              ELSE
              IF CRNOR2-RDPOTCHGOFF = "M"
                 MOVE "<PL>" TO D-RECX
              ELSE
                 MOVE NO-RECDEL TO D-REC
           ELSE
              MOVE NO-RECENCY TO D-REC.

           MOVE LN-RECPROFILE(1) TO PROFILE-9.
           PERFORM PROFILE-DASH.
           MOVE PROFILE-X TO D-RECPROF.

J029  *KC  IF ( SP-LCFRMLA NOT = " "            ) OR    *> FFR
J029  *KC     ( NOT (LN-LC-CODEX = "00" OR " ") )       *> FFR
           IF ( SP-LCFRMLA NOT = SPACES ) OR           *> FFR
              ( LN-LC-CODE NOT = ZEROES )              *> FFR
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
              IF (USERID-BRANCH > 2999 AND < 4000)
                 PERFORM PRINT-UNION-INFO
              END-IF
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
              IF CRNOR2-RDPOT30 = "Y"
                 MOVE "<P>" TO D-SRECX
              ELSE
              IF CRNOR2-RDPOT30 = "M"
                 MOVE "<M>" TO D-SRECX
              ELSE
              IF CRNOR2-RDPOTCHGOFF = "Y"
                 MOVE "<PC>" TO D-SRECX
              ELSE
              IF CRNOR2-RDPOTCHGOFF = "M"
                 MOVE "<PL>" TO D-SRECX
              ELSE
                 MOVE NO-RECDEL TO D-SREC
           ELSE
              MOVE NO-RECENCY TO D-SREC.

           IF CRNOR2-CPOT30 = "Y"
              MOVE "<P>" TO D-SCONX
           ELSE
           IF CRNOR2-CPOT30 = "M"
              MOVE "<M>" TO D-SCONX
           ELSE
           IF CRNOR2-CPOTCHGOFF = "Y"
              MOVE "<PC>" TO D-SCONX
           ELSE
           IF CRNOR2-CPOTCHGOFF = "M"
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
           MOVE 0             TO LM-DATE
                                 QLM1-WBEG-DATE-REV.
           MOVE ALL "9"       TO LM-SEQ 
                                 QLM1-WBEG-SEQ.
           MOVE ALL "9"       TO QLM1-WEND-DATE-REV
                                 QLM1-WEND-SEQ.

           PERFORM START-LM1-FILE.

           MOVE " " TO LOAN-MEMO-BUF.
           MOVE 1 TO MEMO-SUB.

       NEXT-MEMO.
           PERFORM READ-LM1-FILE-NEXT.
           IF IO-FG NOT = 0 OR (LM-BRNO NOT = LM-PATH-OWNBR)
              GO TO EXIT-GET-LOAN-MEMOS.

           IF LM-ACCTNO NOT = LN-ACCTNO
              GO TO EXIT-GET-LOAN-MEMOS.

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
           MOVE ALL "9"       TO LP-SEQNO
                                 QLP1-WEND-SEQNO.
           MOVE ALL "0"       TO QLP1-WBEG-SEQNO.

           PERFORM START-LP1-FILE-NOT-GREATER.
           MOVE " " TO LOAN-PMT-BUF.
           MOVE 1 TO PMT-SUB.

       NEXT-PMT.
           PERFORM READ-LP1-FILE-PREVIOUS.
           IF IO-BAD
              GO TO EXIT-GET-LOAN-PMTS.

           IF (LP-BRNO NOT = LN-OWNBR) OR
               (LP-ACCTNO NOT = LN-ACCTNO)
              GO TO EXIT-GET-LOAN-PMTS.

           MOVE " " TO PMT-LINE.
           MOVE LP-PAYDATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO L-010.
           MOVE LP-TRCD TO L-020.
           MOVE LP-REFNO TO L-030.
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
              MOVE DATE-YYYYMMDD-MM TO L-080-MO
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
           IF (USERID-BRANCH > 2999 AND < 4000)
              MOVE "BEGINNING POOLID   :" TO RANGE-LINE
              MOVE BEG-POOLID TO RANGE-NUM
              PERFORM WRITE-DETAIL-LINE
              MOVE "ENDING POOLID      :" TO RANGE-LINE
              MOVE END-POOLID TO RANGE-NUM
              PERFORM WRITE-DETAIL-LINE
           ELSE
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
       PRINT-UNION-INFO SECTION.
      ************************************
           MOVE "UNION:" TO D-TAG-POOLID.
           MOVE LN-DLPOOL TO D-POOLID.

           MOVE "REGPAY: " TO D-TAG-REGPAY.
           MOVE LN-REGPYAMT TO D-REGPYAMT.

           MOVE "DATE LAST PAID: " TO D-TAG-DATE-LAST-PAID.
           MOVE LN-LAST-PYPA-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO D-DATE-LAST-PAID.

           MOVE "NO. PAYS PASTDUE: " TO D-TAG-NO-PAYS-PASTDUE.
           IF GP-NO-LC-IN-ARREARAGE
              COMPUTE D-NO-PAYS-PASTDUE ROUNDED =
                 NO-TDUECONTR / LN-REGPYAMT
           ELSE
              COMPUTE D-NO-PAYS-PASTDUE ROUNDED =
                 NO-TDUEAMT / LN-REGPYAMT.

           PERFORM WRITE-DETAIL-LINE.

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

                 MOVE "MAKE : " TO D-FIL-MAKE
                 MOVE "MODEL : " TO D-FIL-MODEL
                 MOVE "YEAR : " TO D-FIL-YEAR
                 MOVE "VIN # : " TO D-FIL-ID
                 MOVE LTC-MAKE(2) TO D-MAKE
                 MOVE LTC-MODEL(2) TO D-MODEL
                 MOVE LTC-YEAR(2) TO D-YEAR
                 MOVE LTC-VIN(2) TO D-ID
                 PERFORM WRITE-DETAIL-LINE
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

      *****************************************************************
       DEALER-SCAN SECTION.
      *****************************************************************
           MOVE 0 TO DLSCAN-NUM.
           MOVE DLSCAN-BUF TO SCAN-BUF.
           PERFORM CALL-WINDOW-PROGRAM.
           MOVE SCAN-BUF TO DLSCAN-BUF.
           IF SCAN-STATUS NOT = "00"
            OR DLSCAN-NUM = " "
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
      *       PERFORM READ-LC1-FILE
              IF LC-SALEFIN = SPACES
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
       COPY "LIBWI/PFSWINDOWS.CPY".
       COPY "LIBGB/GRWORK.CPY".
       COPY "LIBGB/GRSERCH.CPY".
       COPY "LIBGB/BRSECURE.CPY".
       COPY "LIBGB/EOSPOOL.CPY".
       COPY "LIBLP/ADDONSPR.CPY".

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
           PERFORM READ-SP1-FILE.

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
       COPY "LIBLP/LPLMGS_SQL.CPY".
       COPY "LIBLP/LPLCGS_SQL.CPY".
       COPY "LIBLP/LPBWGS_SQL.CPY".
       COPY "LIBGB/GBGRGS_SQL.CPY".
      * COPY "LIBGB/GBBRGS_SQL.CPY".
      * COPYMEMBER: LIBGB/GBBRGS_SQL
      ******************************************************************
      *
      * DESCRIPTION: SQL GET & SET - BRFILE
      *
      *=================================================================
      * REV:
      * BAH 2021-0507 ***NEW***
      * BAH 20220923 ADDED QBR-VERITEC-LICENSE #1570
      * BAH 20230419 ADDED QBR-TEST-PP-AUTO-INSCOVR #1594A
      *
      ******************************************************************
      *-----------------------------------------------------------------
      *    LOAD BR-REC FROM QBR-REC; FOR READ & READ-NEXT
      *-----------------------------------------------------------------
       GET-BR-FIELDS.
           MOVE QBR-NO TO BR-NO.

           IF ( QBR-LTOUCH-DATE = "1900-01-01" )
              MOVE ZEROES TO BR-LTOUCH-DATE
           ELSE
              MOVE QBR-LTOUCH-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO BR-LTOUCH-DATE.

           MOVE QBR-NAME TO BR-NAME.
           MOVE QBR-ADR1 TO BR-ADR1.
           MOVE QBR-ADR2 TO BR-ADR2.
           MOVE QBR-CITY TO BR-CITY.
           MOVE QBR-STATE TO BR-STATE.
           MOVE QBR-ZIP TO BR-ZIP.
           MOVE QBR-PHONE TO BR-PHONE.
           MOVE QBR-RETAIN TO BR-RETAIN.
           MOVE QBR-NEXT-CHECK TO BR-NEXT-CHECK.
           MOVE QBR-PRINTER-1 TO BR-PRINTER(1).
           MOVE QBR-PRINTER-2 TO BR-PRINTER(2).
           MOVE QBR-PRINTER-3 TO BR-PRINTER(3).
           MOVE QBR-PRINTER-4 TO BR-PRINTER(4).
           MOVE QBR-FEE-GL-BRNO TO BR-FEE-GL-BRNO.
           MOVE QBR-REMIT1 TO BR-REMIT1.
           MOVE QBR-REMIT2 TO BR-REMIT2.
           MOVE QBR-REMIT3 TO BR-REMIT3.
           MOVE QBR-STMNT1 TO BR-STMNT1.
           MOVE QBR-STMNT2 TO BR-STMNT2.
           MOVE QBR-INS-PRODUCER-NUM TO BR-INS-PRODUCER-NUM.
           MOVE QBR-DLRCHK TO BR-DLRCHK.
           MOVE QBR-FRONT-END-FG TO BR-FRONT-END-FG.
           MOVE QBR-EMLNRG-PHONE TO BR-EMLNRG-PHONE.
           MOVE QBR-POST-GL TO BR-POST-GL.
           MOVE QBR-ALLOTMNTS TO BR-ALLOTMNTS.
           MOVE QBR-EXTRACT-AUTO-UPD TO BR-EXTRACT-AUTO-UPD.
           MOVE QBR-PCNI-LEDGER TO BR-PCNI-LEDGER.
           MOVE QBR-PRINTERFG-1 TO BR-PRINTERFG(1).
           MOVE QBR-PRINTERFG-2 TO BR-PRINTERFG(2).
           MOVE QBR-PRINTERFG-3 TO BR-PRINTERFG(3).
           MOVE QBR-PRINTERFG-4 TO BR-PRINTERFG(4).
           MOVE QBR-NEGDLRSV TO BR-NEGDLRSV.
           MOVE QBR-CPCORP-ORDER TO BR-CPCORP-ORDER.
           MOVE QBR-PRIO-MEMO-FLAG TO BR-PRIO-MEMO-FLAG.
           MOVE QBR-PREBILL-PL-JD-OPT TO BR-PREBILL-PL-JD-OPT.
           MOVE QBR-LOAN-NUM-FLAG TO BR-LOAN-NUM-FLAG.
           MOVE QBR-DOWNPY-FLAG TO BR-DOWNPY-FLAG.
           MOVE QBR-AUTO-INS-EARN TO BR-AUTO-INS-EARN.
           MOVE QBR-OTH-INS-LEVELS TO BR-OTH-INS-LEVELS.
           MOVE QBR-ALLOW-INTERBR TO BR-ALLOW-INTERBR.
           MOVE QBR-UNER-DLANRP TO BR-UNER-DLANRP.
           MOVE QBR-SECOND-PROMISE TO BR-SECOND-PROMISE.
           MOVE QBR-AVAIL-CREDIT TO BR-AVAIL-CREDIT.
           MOVE QBR-RECP1 TO BR-RECP1.
           MOVE QBR-RECP2 TO BR-RECP2.
           MOVE QBR-INC-WAIVERS TO BR-INC-WAIVERS.
           MOVE QBR-INC-INS TO BR-INC-INS.
           MOVE QBR-BP-NEXT-BATCH TO BR-BP-NEXT-BATCH.
           MOVE QBR-BP-NOPAYOFF TO BR-BP-NOPAYOFF.
           MOVE QBR-BP-ODDPAY TO BR-BP-ODDPAY.
           MOVE QBR-BP-LC TO BR-BP-LC.
           MOVE QBR-BP-NOPRIN TO BR-BP-NOPRIN.
           MOVE QBR-BP-FLASH TO BR-BP-FLASH.
           MOVE QBR-BP-ACTIONCD TO BR-BP-ACTIONCD.
           MOVE QBR-BP-MAXCON TO BR-BP-MAXCON.
           MOVE QBR-BP-MAXREC TO BR-BP-MAXREC.
           MOVE QBR-BP-PL TO BR-BP-PL.
           MOVE QBR-BP-JD TO BR-BP-JD.
           MOVE QBR-BP-BRANCH TO BR-BP-BRANCH.
           MOVE QBR-AGE-ERN TO BR-AGE-ERN.
           MOVE QBR-OTHER-BRANCH-GL TO BR-OTHER-BRANCH-GL.
           MOVE QBR-SALES-SMALL TO BR-SALES-SMALL.
           MOVE QBR-NEW-COLL-INQUIRY TO BR-NEW-COLL-INQUIRY.
           MOVE QBR-CHANGE-MEMO TO BR-CHANGE-MEMO.
           MOVE QBR-INS-ADDON TO BR-INS-ADDON.
           MOVE QBR-TRAILER-REC TO BR-TRAILER-REC.
           MOVE QBR-ALDEL-FLAG TO BR-ALDEL-FLAG.
           MOVE QBR-IB-LCHG-MESS TO BR-IB-LCHG-MESS.
           MOVE QBR-COL-QUEUE-PAST-DUE TO BR-COL-QUEUE-PAST-DUE.
           MOVE QBR-LI-DLCOMM-CMSI-PP-MAX TO BR-LI-DLCOMM-CMSI-PP-MAX.
           MOVE QBR-ADDON-FEECD-O2-1 TO BR-ADDON-FEECD-O2(1).
           MOVE QBR-ADDON-FEECD-O2-2 TO BR-ADDON-FEECD-O2(2).
           MOVE QBR-AUTO-CHECK TO BR-AUTO-CHECK.
           MOVE QBR-RECEIPT-PROMDATE TO BR-RECEIPT-PROMDATE.
           MOVE QBR-RECEIPT-INTPD TO BR-RECEIPT-INTPD.
           MOVE QBR-COLL-ID-SCAN TO BR-COLL-ID-SCAN.
           MOVE QBR-STOP-ON-COLL TO BR-STOP-ON-COLL.
           MOVE QBR-DUEDATE-CHANGE-CD TO BR-DUEDATE-CHANGE-CD.
           MOVE QBR-CHECK-EOM-FG TO BR-CHECK-EOM-FG.
           MOVE QBR-VOLUME-DLANRP TO BR-VOLUME-DLANRP.
           MOVE QBR-PDTH-NXDUE TO BR-PDTH-NXDUE.
           MOVE QBR-LC-CODE TO BR-LC-CODE.
           MOVE QBR-VALIDATE-PRUS TO BR-VALIDATE-PRUS.
           MOVE QBR-AUTO-OPEN-BAL TO BR-AUTO-OPEN-BAL.
           MOVE QBR-OPEN-BAL-1 TO BR-OPEN-BAL(1).
           MOVE QBR-OPEN-BAL-2 TO BR-OPEN-BAL(2).
           MOVE QBR-OPEN-BAL-3 TO BR-OPEN-BAL(3).
           MOVE QBR-OPEN-BAL-4 TO BR-OPEN-BAL(4).
           MOVE QBR-OPEN-BAL-5 TO BR-OPEN-BAL(5).
           MOVE QBR-OPEN-BAL-6 TO BR-OPEN-BAL(6).
           MOVE QBR-OPEN-BAL-7 TO BR-OPEN-BAL(7).
           MOVE QBR-OPEN-BAL-8 TO BR-OPEN-BAL(8).
           MOVE QBR-OPEN-BAL-9 TO BR-OPEN-BAL(9).
           MOVE QBR-OPEN-BAL-10 TO BR-OPEN-BAL(10).
           MOVE QBR-OPEN-BAL-11 TO BR-OPEN-BAL(11).
           MOVE QBR-OPEN-BAL-12 TO BR-OPEN-BAL(12).
           MOVE QBR-OPEN-BAL-13 TO BR-OPEN-BAL(13).
           MOVE QBR-OPEN-BAL-14 TO BR-OPEN-BAL(14).
           MOVE QBR-OPEN-BAL-15 TO BR-OPEN-BAL(15).
           MOVE QBR-OPEN-BAL-16 TO BR-OPEN-BAL(16).
           MOVE QBR-OPEN-BAL-17 TO BR-OPEN-BAL(17).
           MOVE QBR-OPEN-BAL-18 TO BR-OPEN-BAL(18).
           MOVE QBR-OPEN-BAL-19 TO BR-OPEN-BAL(19).
           MOVE QBR-OPEN-BAL-20 TO BR-OPEN-BAL(20).
           MOVE QBR-TENDER-CD-1 TO BR-TENDER-CD(1).
           MOVE QBR-TENDER-CD-2 TO BR-TENDER-CD(2).
           MOVE QBR-TENDER-CD-3 TO BR-TENDER-CD(3).
           MOVE QBR-DLPARV-ADJ-FG TO BR-DLPARV-ADJ-FG.
           MOVE QBR-OVERRIDE-BR-STATE TO BR-OVERRIDE-BR-STATE.
           MOVE QBR-DEFAULT-SC-CLASS TO BR-DEFAULT-SC-CLASS.
           MOVE QBR-RECDEL-NOT-REC TO BR-RECDEL-NOT-REC.
           MOVE QBR-POST-AHEAD-FG TO BR-POST-AHEAD-FG.
           MOVE QBR-CAN-LOANS-FG TO BR-CAN-LOANS-FG.
           MOVE QBR-NON-MONTHLY-CAN-LOANS TO BR-NON-MONTHLY-CAN-LOANS.
           MOVE QBR-BP-ALLOW-PMT-ACCT-OTHBAL
              TO BR-BP-ALLOW-PMT-ACCT-OTHBAL.
           MOVE QBR-LBOX-ALLOW-PMT-ACCT-OTHBAL
              TO BR-LBOX-ALLOW-PMT-ACCT-OTHBAL.
           MOVE QBR-REGION-NUMBER TO BR-REGION-NUMBER.
           MOVE QBR-OUR-FEE TO BR-OUR-FEE.
           MOVE QBR-REB-METHOD-1 TO BR-REB-METHOD(1).
           MOVE QBR-REB-METHOD-2 TO BR-REB-METHOD(2).
           MOVE QBR-REB-METHOD-3 TO BR-REB-METHOD(3).
           MOVE QBR-REB-METHOD-4 TO BR-REB-METHOD(4).
           MOVE QBR-REB-METHOD-5 TO BR-REB-METHOD(5).
           MOVE QBR-REB-METHOD-6 TO BR-REB-METHOD(6).
           MOVE QBR-REB-METHOD-7 TO BR-REB-METHOD(7).
           MOVE QBR-REB-METHOD-8 TO BR-REB-METHOD(8).
           MOVE QBR-REB-METHOD-9 TO BR-REB-METHOD(9).
           MOVE QBR-REB-METHOD-10 TO BR-REB-METHOD(10).
           MOVE QBR-REB-METHOD-11 TO BR-REB-METHOD(11).
           MOVE QBR-REBDAYS-MONTH-1 TO BR-REBDAYS-MONTH(1).
           MOVE QBR-REBDAYS-MONTH-2 TO BR-REBDAYS-MONTH(2).
           MOVE QBR-REBDAYS-MONTH-3 TO BR-REBDAYS-MONTH(3).
           MOVE QBR-REBDAYS-MONTH-4 TO BR-REBDAYS-MONTH(4).
           MOVE QBR-REBDAYS-MONTH-5 TO BR-REBDAYS-MONTH(5).
           MOVE QBR-REBDAYS-MONTH-6 TO BR-REBDAYS-MONTH(6).
           MOVE QBR-REBDAYS-MONTH-7 TO BR-REBDAYS-MONTH(7).
           MOVE QBR-REBDAYS-MONTH-8 TO BR-REBDAYS-MONTH(8).
           MOVE QBR-REBDAYS-MONTH-9 TO BR-REBDAYS-MONTH(9).
           MOVE QBR-REBDAYS-MONTH-10 TO BR-REBDAYS-MONTH(10).
           MOVE QBR-REBDAYS-MONTH-11 TO BR-REBDAYS-MONTH(11).
           MOVE QBR-ACCRUAL-MONTHS-1 TO BR-ACCRUAL-MONTHS(1).
           MOVE QBR-ACCRUAL-MONTHS-2 TO BR-ACCRUAL-MONTHS(2).
           MOVE QBR-ACCRUAL-MONTHS-3 TO BR-ACCRUAL-MONTHS(3).
           MOVE QBR-ACCRUAL-MONTHS-4 TO BR-ACCRUAL-MONTHS(4).
           MOVE QBR-ACCRUAL-MONTHS-5 TO BR-ACCRUAL-MONTHS(5).
           MOVE QBR-ACCRUAL-MONTHS-6 TO BR-ACCRUAL-MONTHS(6).
           MOVE QBR-ACCRUAL-MONTHS-7 TO BR-ACCRUAL-MONTHS(7).
           MOVE QBR-ACCRUAL-MONTHS-8 TO BR-ACCRUAL-MONTHS(8).
           MOVE QBR-ACCRUAL-MONTHS-9 TO BR-ACCRUAL-MONTHS(9).
           MOVE QBR-ACCRUAL-MONTHS-10 TO BR-ACCRUAL-MONTHS(10).
           MOVE QBR-ACCRUAL-MONTHS-11 TO BR-ACCRUAL-MONTHS(11).
           MOVE QBR-GLOVERSHORT TO BR-GLOVERSHORT.
           MOVE QBR-GLBANK TO BR-GLBANK.
           MOVE QBR-GLONHAND TO BR-GLONHAND.
           MOVE QBR-BANKBAL TO BR-BANKBAL.
           MOVE QBR-RETAIN-AMT TO BR-RETAIN-AMT.
           MOVE QBR-LCARDFG TO BR-LCARDFG.
           MOVE QBR-DATA-PATH TO BR-DATA-PATH.
           MOVE QBR-MACHINE TO BR-MACHINE.
           MOVE QBR-MANAGER TO BR-MANAGER.
           MOVE QBR-EOD-PROCEDURE TO BR-EOD-PROCEDURE.
           MOVE QBR-BP-PAYOFF-NONCASH TO BR-BP-PAYOFF-NONCASH.
           MOVE QBR-BP-PAYOFF-OV TO BR-BP-PAYOFF-OV.
           MOVE QBR-LI-DLCOMM-CMSI-O3-MAX TO BR-LI-DLCOMM-CMSI-O3-MAX.
           MOVE QBR-LR-GROUP-COLLID TO BR-LR-GROUP-COLLID.
           MOVE QBR-UNITPER-FG TO BR-UNITPER-FG.
           MOVE QBR-UNITPER-Y-D-FG TO BR-UNITPER-Y-D-FG.
           MOVE QBR-NO-PROMPT-FOR-OTHBR TO BR-NO-PROMPT-FOR-OTHBR.
           MOVE QBR-WORLD-BRANCH-TYPE TO BR-WORLD-BRANCH-TYPE.
           MOVE QBR-COL-QUEUE-BEHAVIOR-FG TO BR-COL-QUEUE-BEHAVIOR-FG.
           MOVE QBR-CONVERT-AUTO-ACCTNO TO BR-CONVERT-AUTO-ACCTNO.
           MOVE QBR-TS-REPO-INS-FG TO BR-TS-REPO-INS-FG.
           MOVE QBR-BOOK-SF-BR-ONLY TO BR-BOOK-SF-BR-ONLY.
           MOVE QBR-FORCE-VALID-CITY-COUNTY
              TO BR-FORCE-VALID-CITY-COUNTY.
           MOVE QBR-SET-COLLCD-DL-MFGCD TO BR-SET-COLLCD-DL-MFGCD.
           MOVE QBR-DEFAULT-SF-UNITPER-CD TO BR-DEFAULT-SF-UNITPER-CD.
           MOVE QBR-DEFAULT-SF-PURCD TO BR-DEFAULT-SF-PURCD.
           MOVE QBR-DEFAULT-LN-UNITPER-CD TO BR-DEFAULT-LN-UNITPER-CD.
           MOVE QBR-DEFAULT-LN-PURCD TO BR-DEFAULT-LN-PURCD.
           MOVE QBR-TAP-OTHER-DL TO BR-TAP-OTHER-DL.
           MOVE QBR-AH-AND-CL TO BR-AH-AND-CL.
           MOVE QBR-NO-CASH-PROCEEDS TO BR-NO-CASH-PROCEEDS.
           MOVE QBR-RI-BEFORE-REPO TO BR-RI-BEFORE-REPO.
           MOVE QBR-RI-BEFORE-P4P5 TO BR-RI-BEFORE-P4P5.
           MOVE QBR-NOT-OURS-REFUND TO BR-NOT-OURS-REFUND.
           MOVE QBR-SFDLO3-WINDOW-FG TO BR-SFDLO3-WINDOW-FG.
           MOVE QBR-CALC-INS-ADJ TO BR-CALC-INS-ADJ.
           MOVE QBR-AC-BEFORE-REPO TO BR-AC-BEFORE-REPO.
           MOVE QBR-AC-BEFORE-P4P5 TO BR-AC-BEFORE-P4P5.
           MOVE QBR-BANK-ROUTING-NO TO BR-BANK-ROUTING-NO.
           MOVE QBR-BANK-CLEARING-GL TO BR-BANK-CLEARING-GL.
           MOVE QBR-GLBANK-DEPOSIT TO BR-GLBANK-DEPOSIT.
           MOVE QBR-BANK-ACCTNO TO BR-BANK-ACCTNO.
           MOVE QBR-DEFAULT-SC-SUBCLASS TO BR-DEFAULT-SC-SUBCLASS.
           MOVE QBR-NO-OUT-DLR-RSV TO BR-NO-OUT-DLR-RSV.
           MOVE QBR-COLLECTOR-QUEUE TO BR-COLLECTOR-QUEUE.
           MOVE QBR-COLL-QUEUE-CON-REC TO BR-COLL-QUEUE-CON-REC.
           MOVE QBR-JE-ALLOCATION-SKIP TO BR-JE-ALLOCATION-SKIP.
           MOVE QBR-EXPAND-APP-INPUT TO BR-EXPAND-APP-INPUT.
           MOVE QBR-A50-CPCORP TO BR-A50-CPCORP.
           MOVE QBR-INBR-BILNSC-FG TO BR-INBR-BILNSC-FG.
           MOVE QBR-INBR-BRIEF-FG TO BR-INBR-BRIEF-FG.
           MOVE QBR-LEDGER-JUST-INTCHG TO BR-LEDGER-JUST-INTCHG.
           MOVE QBR-BYINQ-START-PAGE TO BR-BYINQ-START-PAGE.
           MOVE QBR-DSINQ-START-PAGE TO BR-DSINQ-START-PAGE.
           MOVE QBR-APP-REQUIRED-FOR-INPUT
              TO BR-APP-REQUIRED-FOR-INPUT.
           MOVE QBR-CREDIT-SCORING-FG TO BR-CREDIT-SCORING-FG.
           MOVE QBR-COL-QUEUE-LOW-TO-HI TO BR-COL-QUEUE-LOW-TO-HI.
           MOVE QBR-CAN-GROUP-ID TO BR-CAN-GROUP-ID.
           MOVE QBR-AUTO-LNSTAT-WINDOW TO BR-AUTO-LNSTAT-WINDOW.
           MOVE QBR-TIME-ZONE-FACTOR TO BR-TIME-ZONE-FACTOR.
           MOVE QBR-LONPF-OPT-DISPLAY TO BR-LONPF-OPT-DISPLAY.
           MOVE QBR-GIFTNET TO BR-GIFTNET.
           MOVE QBR-GIFTNET-LEDGER-LINE TO BR-GIFTNET-LEDGER-LINE.
           MOVE QBR-NO-FT-EMP TO BR-NO-FT-EMP.
           MOVE QBR-NO-PT-EMP TO BR-NO-PT-EMP.
           MOVE QBR-BALLOON-LOANS-FG TO BR-BALLOON-LOANS-FG.
           MOVE QBR-GL-TYPE-ALLOWED TO BR-GL-TYPE-ALLOWED.
           MOVE QBR-GL-ENTRY-DEFAULT TO BR-GL-ENTRY-DEFAULT.
           MOVE QBR-MIDSTREAM-1STPAY-FG TO BR-MIDSTREAM-1STPAY-FG.
           MOVE QBR-NODEF-1ST-X-MONTHS TO BR-NODEF-1ST-X-MONTHS.
           MOVE QBR-ADD-INS-FEECODE TO BR-ADD-INS-FEECODE.
           MOVE QBR-ROAD-G-SGL-FEECODE TO BR-ROAD-G-SGL-FEECODE.
           MOVE QBR-ROAD-G-FAM-FEECODE TO BR-ROAD-G-FAM-FEECODE.
           MOVE QBR-NODEF-IF-ZERO-AMT TO BR-NODEF-IF-ZERO-AMT.
           MOVE QBR-ASSESS-LATE-CHGS TO BR-ASSESS-LATE-CHGS.
           MOVE QBR-NEXT-SOLICIT-NO TO BR-NEXT-SOLICIT-NO.
           MOVE QBR-ADDON-FEECD-O3-1 TO BR-ADDON-FEECD-O3(1).
           MOVE QBR-ADDON-FEECD-O3-2 TO BR-ADDON-FEECD-O3(2).
           MOVE QBR-IMMEDIATE-RECEIPT TO BR-IMMEDIATE-RECEIPT.
           MOVE QBR-IMMEDIATE-DOC-PRINT TO BR-IMMEDIATE-DOC-PRINT.
           MOVE QBR-AUTO-PL-REF-AC TO BR-AUTO-PL-REF-AC.
           MOVE QBR-FAX-NUMBER TO BR-FAX-NUMBER.
           MOVE QBR-ALPHA-DRAWERS TO BR-ALPHA-DRAWERS.
           MOVE QBR-LATECHG-ON-ALLOTMENTS TO BR-LATECHG-ON-ALLOTMENTS.
           MOVE QBR-ALLOW-SYS-AS-EFFDATE TO BR-ALLOW-SYS-AS-EFFDATE.
           MOVE QBR-TAX-PREBOOK-CUST TO BR-TAX-PREBOOK-CUST.
           MOVE QBR-TAX-PREBOOK-NONCUST TO BR-TAX-PREBOOK-NONCUST.
           MOVE QBR-PASSWORD-DRAWER TO BR-PASSWORD-DRAWER.
           MOVE QBR-ONESTEP-RN-FG TO BR-ONESTEP-RN-FG.
           MOVE QBR-LI-LISCAN-FG TO BR-LI-LISCAN-FG.
           MOVE QBR-HIDE-END-BAL TO BR-HIDE-END-BAL.
           MOVE QBR-LBOX-NOPAYOFF TO BR-LBOX-NOPAYOFF.
           MOVE QBR-LBOX-ODDPAY TO BR-LBOX-ODDPAY.
           MOVE QBR-LBOX-LC TO BR-LBOX-LC.
           MOVE QBR-LBOX-NOPRIN TO BR-LBOX-NOPRIN.
           MOVE QBR-LBOX-FLASH TO BR-LBOX-FLASH.
           MOVE QBR-LBOX-ACTIONCD TO BR-LBOX-ACTIONCD.
           MOVE QBR-LBOX-MAXCON TO BR-LBOX-MAXCON.
           MOVE QBR-LBOX-MAXREC TO BR-LBOX-MAXREC.
           MOVE QBR-LBOX-PL TO BR-LBOX-PL.
           MOVE QBR-LBOX-JD TO BR-LBOX-JD.
           MOVE QBR-LBOX TO BR-LBOX.
           MOVE QBR-LATECHG-ON-LBOX TO BR-LATECHG-ON-LBOX.
           MOVE QBR-ALLOW-OV-TRCD TO BR-ALLOW-OV-TRCD.
           MOVE QBR-GLOBAL-QUEUE TO BR-GLOBAL-QUEUE.
           MOVE QBR-LBOX-PAYOFF-NONCASH TO BR-LBOX-PAYOFF-NONCASH.
           MOVE QBR-LBOX-PAYOFF-OV TO BR-LBOX-PAYOFF-OV.
           MOVE QBR-COLLECT-MANAGER TO BR-COLLECT-MANAGER.
           MOVE QBR-COLLECT-PHONE TO BR-COLLECT-PHONE.
           MOVE QBR-COLLECT-EXT TO BR-COLLECT-EXT.
           MOVE QBR-ALT-INTO-THIS-BRANCH TO BR-ALT-INTO-THIS-BRANCH.
           MOVE QBR-POFF-TITLE-WINDOW TO BR-POFF-TITLE-WINDOW.
           MOVE QBR-COUPON-STATEMENT TO BR-COUPON-STATEMENT.
           MOVE QBR-SUPERVISOR TO BR-SUPERVISOR.
           MOVE QBR-AREA-MANAGER TO BR-AREA-MANAGER.
           MOVE QBR-MIN-MONTHS-DEF TO BR-MIN-MONTHS-DEF.
           MOVE QBR-CC-FLAG TO BR-CC-FLAG.
           MOVE QBR-CC-ADR1 TO BR-CC-ADR1.
           MOVE QBR-CC-CITY TO BR-CC-CITY.
           MOVE QBR-CC-STATE TO BR-CC-STATE.
           MOVE QBR-CC-ZIP TO BR-CC-ZIP.
           MOVE QBR-MIN-MONTHS-D2 TO BR-MIN-MONTHS-D2.
           MOVE QBR-SKIP-MIDSTREAM-TRW TO BR-SKIP-MIDSTREAM-TRW.
           MOVE QBR-GL-ESCROW-BALANCE TO BR-GL-ESCROW-BALANCE.
           MOVE QBR-GL-ESCROW-PAYABLE TO BR-GL-ESCROW-PAYABLE.
           MOVE QBR-ALLOW-FORCE-OPEN-FG TO BR-ALLOW-FORCE-OPEN-FG.
           MOVE QBR-ACH-BANK-NO TO BR-ACH-BANK-NO.
           MOVE QBR-WIMENU-CHECK-WRITING TO BR-WIMENU-CHECK-WRITING.
           MOVE QBR-ADP-PAY-GROUP TO BR-ADP-PAY-GROUP.
           MOVE QBR-ALLOW-SCANNED-CKS TO BR-ALLOW-SCANNED-CKS.
           MOVE QBR-CHECK-DEPOSIT-GL TO BR-CHECK-DEPOSIT-GL.
           MOVE QBR-OPTION-PST-LASER TO BR-OPTION-PST-LASER.
           MOVE QBR-USE-DEBIT-CARDS TO BR-USE-DEBIT-CARDS.
           MOVE QBR-GL-DEBIT-CARD-PROCEEDS
                                    TO BR-GL-DEBIT-CARD-PROCEEDS.
           MOVE QBR-WKDAY-BANKID    TO BR-WKDAY-BANKID.
           MOVE QBR-PURGE-NOW-FLAG  TO BR-PURGE-NOW-FLAG.
           MOVE QBR-EMAIL           TO BR-EMAIL.
           MOVE QBR-MANAGER-EMAIL   TO BR-MANAGER-EMAIL.
           MOVE QBR-VERITEC-LICENSE TO BR-VERITEC-LICENSE.
      *     MOVE QBR-TEST-PP-AUTO-INSCOVR TO BR-TEST-PP-AUTO-INSCOVR.
 
      *-----------------------------------------------------------------
      *    LOAD QBR-REC FROM BR-REC; FOR WRITE & REWRITE
      *-----------------------------------------------------------------
       SET-BR-FIELDS.
           MOVE BR-NO TO QBR-NO.

           IF ( BR-LTOUCH-DATE = ZEROES )
              MOVE "1900-01-01" TO QBR-LTOUCH-DATE
           ELSE
              MOVE BR-LTOUCH-DATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QBR-LTOUCH-DATE.

           MOVE BR-NAME TO QBR-NAME.
           MOVE BR-ADR1 TO QBR-ADR1.
           MOVE BR-ADR2 TO QBR-ADR2.
           MOVE BR-CITY TO QBR-CITY.
           MOVE BR-STATE TO QBR-STATE.
           MOVE BR-ZIP TO QBR-ZIP.
           MOVE BR-PHONE TO QBR-PHONE.
           MOVE BR-RETAIN TO QBR-RETAIN.
           MOVE BR-NEXT-CHECK TO QBR-NEXT-CHECK.
           MOVE BR-PRINTER(1) TO QBR-PRINTER-1.
           MOVE BR-PRINTER(2) TO QBR-PRINTER-2.
           MOVE BR-PRINTER(3) TO QBR-PRINTER-3.
           MOVE BR-PRINTER(4) TO QBR-PRINTER-4.
           MOVE BR-FEE-GL-BRNO TO QBR-FEE-GL-BRNO.
           MOVE BR-REMIT1 TO QBR-REMIT1.
           MOVE BR-REMIT2 TO QBR-REMIT2.
           MOVE BR-REMIT3 TO QBR-REMIT3.
           MOVE BR-STMNT1 TO QBR-STMNT1.
           MOVE BR-STMNT2 TO QBR-STMNT2.
           MOVE BR-INS-PRODUCER-NUM TO QBR-INS-PRODUCER-NUM.
           MOVE BR-DLRCHK TO QBR-DLRCHK.
           MOVE BR-FRONT-END-FG TO QBR-FRONT-END-FG.
           MOVE BR-EMLNRG-PHONE TO QBR-EMLNRG-PHONE.
           MOVE BR-POST-GL TO QBR-POST-GL.
           MOVE BR-ALLOTMNTS TO QBR-ALLOTMNTS.
           MOVE BR-EXTRACT-AUTO-UPD TO QBR-EXTRACT-AUTO-UPD.
           MOVE BR-PCNI-LEDGER TO QBR-PCNI-LEDGER.
           MOVE BR-PRINTERFG(1) TO QBR-PRINTERFG-1.
           MOVE BR-PRINTERFG(2) TO QBR-PRINTERFG-2.
           MOVE BR-PRINTERFG(3) TO QBR-PRINTERFG-3.
           MOVE BR-PRINTERFG(4) TO QBR-PRINTERFG-4.
           MOVE BR-NEGDLRSV TO QBR-NEGDLRSV.
           MOVE BR-CPCORP-ORDER TO QBR-CPCORP-ORDER.
           MOVE BR-PRIO-MEMO-FLAG TO QBR-PRIO-MEMO-FLAG.
           MOVE BR-PREBILL-PL-JD-OPT TO QBR-PREBILL-PL-JD-OPT.
           MOVE BR-LOAN-NUM-FLAG TO QBR-LOAN-NUM-FLAG.
           MOVE BR-DOWNPY-FLAG TO QBR-DOWNPY-FLAG.
           MOVE BR-AUTO-INS-EARN TO QBR-AUTO-INS-EARN.
           MOVE BR-OTH-INS-LEVELS TO QBR-OTH-INS-LEVELS.
           MOVE BR-ALLOW-INTERBR TO QBR-ALLOW-INTERBR.
           MOVE BR-UNER-DLANRP TO QBR-UNER-DLANRP.
           MOVE BR-SECOND-PROMISE TO QBR-SECOND-PROMISE.
           MOVE BR-AVAIL-CREDIT TO QBR-AVAIL-CREDIT.
           MOVE BR-RECP1 TO QBR-RECP1.
           MOVE BR-RECP2 TO QBR-RECP2.
           MOVE BR-INC-WAIVERS TO QBR-INC-WAIVERS.
           MOVE BR-INC-INS TO QBR-INC-INS.
           MOVE BR-BP-NEXT-BATCH TO QBR-BP-NEXT-BATCH.
           MOVE BR-BP-NOPAYOFF TO QBR-BP-NOPAYOFF.
           MOVE BR-BP-ODDPAY TO QBR-BP-ODDPAY.
           MOVE BR-BP-LC TO QBR-BP-LC.
           MOVE BR-BP-NOPRIN TO QBR-BP-NOPRIN.
           MOVE BR-BP-FLASH TO QBR-BP-FLASH.
           MOVE BR-BP-ACTIONCD TO QBR-BP-ACTIONCD.
           MOVE BR-BP-MAXCON TO QBR-BP-MAXCON.
           MOVE BR-BP-MAXREC TO QBR-BP-MAXREC.
           MOVE BR-BP-PL TO QBR-BP-PL.
           MOVE BR-BP-JD TO QBR-BP-JD.
           MOVE BR-BP-BRANCH TO QBR-BP-BRANCH.
           MOVE BR-AGE-ERN TO QBR-AGE-ERN.
           MOVE BR-OTHER-BRANCH-GL TO QBR-OTHER-BRANCH-GL.
           MOVE BR-SALES-SMALL TO QBR-SALES-SMALL.
           MOVE BR-NEW-COLL-INQUIRY TO QBR-NEW-COLL-INQUIRY.
           MOVE BR-CHANGE-MEMO TO QBR-CHANGE-MEMO.
           MOVE BR-INS-ADDON TO QBR-INS-ADDON.
           MOVE BR-TRAILER-REC TO QBR-TRAILER-REC.
           MOVE BR-ALDEL-FLAG TO QBR-ALDEL-FLAG.
           MOVE BR-IB-LCHG-MESS TO QBR-IB-LCHG-MESS.
           MOVE BR-COL-QUEUE-PAST-DUE TO QBR-COL-QUEUE-PAST-DUE.
           MOVE BR-LI-DLCOMM-CMSI-PP-MAX TO QBR-LI-DLCOMM-CMSI-PP-MAX.
           MOVE BR-ADDON-FEECD-O2(1) TO QBR-ADDON-FEECD-O2-1.
           MOVE BR-ADDON-FEECD-O2(2) TO QBR-ADDON-FEECD-O2-2.
           MOVE BR-AUTO-CHECK TO QBR-AUTO-CHECK.
           MOVE BR-RECEIPT-PROMDATE TO QBR-RECEIPT-PROMDATE.
           MOVE BR-RECEIPT-INTPD TO QBR-RECEIPT-INTPD.
           MOVE BR-COLL-ID-SCAN TO QBR-COLL-ID-SCAN.
           MOVE BR-STOP-ON-COLL TO QBR-STOP-ON-COLL.
           MOVE BR-DUEDATE-CHANGE-CD TO QBR-DUEDATE-CHANGE-CD.
           MOVE BR-CHECK-EOM-FG TO QBR-CHECK-EOM-FG.
           MOVE BR-VOLUME-DLANRP TO QBR-VOLUME-DLANRP.
           MOVE BR-PDTH-NXDUE TO QBR-PDTH-NXDUE.
           MOVE BR-LC-CODE TO QBR-LC-CODE.
           MOVE BR-VALIDATE-PRUS TO QBR-VALIDATE-PRUS.
           MOVE BR-AUTO-OPEN-BAL TO QBR-AUTO-OPEN-BAL.
           MOVE BR-OPEN-BAL(1) TO QBR-OPEN-BAL-1.
           MOVE BR-OPEN-BAL(2) TO QBR-OPEN-BAL-2.
           MOVE BR-OPEN-BAL(3) TO QBR-OPEN-BAL-3.
           MOVE BR-OPEN-BAL(4) TO QBR-OPEN-BAL-4.
           MOVE BR-OPEN-BAL(5) TO QBR-OPEN-BAL-5.
           MOVE BR-OPEN-BAL(6) TO QBR-OPEN-BAL-6.
           MOVE BR-OPEN-BAL(7) TO QBR-OPEN-BAL-7.
           MOVE BR-OPEN-BAL(8) TO QBR-OPEN-BAL-8.
           MOVE BR-OPEN-BAL(9) TO QBR-OPEN-BAL-9.
           MOVE BR-OPEN-BAL(10) TO QBR-OPEN-BAL-10.
           MOVE BR-OPEN-BAL(11) TO QBR-OPEN-BAL-11.
           MOVE BR-OPEN-BAL(12) TO QBR-OPEN-BAL-12.
           MOVE BR-OPEN-BAL(13) TO QBR-OPEN-BAL-13.
           MOVE BR-OPEN-BAL(14) TO QBR-OPEN-BAL-14.
           MOVE BR-OPEN-BAL(15) TO QBR-OPEN-BAL-15.
           MOVE BR-OPEN-BAL(16) TO QBR-OPEN-BAL-16.
           MOVE BR-OPEN-BAL(17) TO QBR-OPEN-BAL-17.
           MOVE BR-OPEN-BAL(18) TO QBR-OPEN-BAL-18.
           MOVE BR-OPEN-BAL(19) TO QBR-OPEN-BAL-19.
           MOVE BR-OPEN-BAL(20) TO QBR-OPEN-BAL-20.
           MOVE BR-TENDER-CD(1) TO QBR-TENDER-CD-1.
           MOVE BR-TENDER-CD(2) TO QBR-TENDER-CD-2.
           MOVE BR-TENDER-CD(3) TO QBR-TENDER-CD-3.
           MOVE BR-DLPARV-ADJ-FG TO QBR-DLPARV-ADJ-FG.
           MOVE BR-OVERRIDE-BR-STATE TO QBR-OVERRIDE-BR-STATE.
           MOVE BR-DEFAULT-SC-CLASS TO QBR-DEFAULT-SC-CLASS.
           MOVE BR-RECDEL-NOT-REC TO QBR-RECDEL-NOT-REC.
           MOVE BR-POST-AHEAD-FG TO QBR-POST-AHEAD-FG.
           MOVE BR-CAN-LOANS-FG TO QBR-CAN-LOANS-FG.
           MOVE BR-NON-MONTHLY-CAN-LOANS TO QBR-NON-MONTHLY-CAN-LOANS.
           MOVE BR-BP-ALLOW-PMT-ACCT-OTHBAL
            TO QBR-BP-ALLOW-PMT-ACCT-OTHBAL.
           MOVE BR-LBOX-ALLOW-PMT-ACCT-OTHBAL
            TO QBR-LBOX-ALLOW-PMT-ACCT-OTHBAL.
           MOVE BR-REGION-NUMBER TO QBR-REGION-NUMBER.
           MOVE BR-OUR-FEE TO QBR-OUR-FEE.
           MOVE BR-REB-METHOD(1) TO QBR-REB-METHOD-1.
           MOVE BR-REB-METHOD(2) TO QBR-REB-METHOD-2.
           MOVE BR-REB-METHOD(3) TO QBR-REB-METHOD-3.
           MOVE BR-REB-METHOD(4) TO QBR-REB-METHOD-4.
           MOVE BR-REB-METHOD(5) TO QBR-REB-METHOD-5.
           MOVE BR-REB-METHOD(6) TO QBR-REB-METHOD-6.
           MOVE BR-REB-METHOD(7) TO QBR-REB-METHOD-7.
           MOVE BR-REB-METHOD(8) TO QBR-REB-METHOD-8.
           MOVE BR-REB-METHOD(9) TO QBR-REB-METHOD-9.
           MOVE BR-REB-METHOD(10) TO QBR-REB-METHOD-10.
           MOVE BR-REB-METHOD(11) TO QBR-REB-METHOD-11.
           MOVE BR-REBDAYS-MONTH(1) TO QBR-REBDAYS-MONTH-1.
           MOVE BR-REBDAYS-MONTH(2) TO QBR-REBDAYS-MONTH-2.
           MOVE BR-REBDAYS-MONTH(3) TO QBR-REBDAYS-MONTH-3.
           MOVE BR-REBDAYS-MONTH(4) TO QBR-REBDAYS-MONTH-4.
           MOVE BR-REBDAYS-MONTH(5) TO QBR-REBDAYS-MONTH-5.
           MOVE BR-REBDAYS-MONTH(6) TO QBR-REBDAYS-MONTH-6.
           MOVE BR-REBDAYS-MONTH(7) TO QBR-REBDAYS-MONTH-7.
           MOVE BR-REBDAYS-MONTH(8) TO QBR-REBDAYS-MONTH-8.
           MOVE BR-REBDAYS-MONTH(9) TO QBR-REBDAYS-MONTH-9.
           MOVE BR-REBDAYS-MONTH(10) TO QBR-REBDAYS-MONTH-10.
           MOVE BR-REBDAYS-MONTH(11) TO QBR-REBDAYS-MONTH-11.
           MOVE BR-ACCRUAL-MONTHS(1) TO QBR-ACCRUAL-MONTHS-1.
           MOVE BR-ACCRUAL-MONTHS(2) TO QBR-ACCRUAL-MONTHS-2.
           MOVE BR-ACCRUAL-MONTHS(3) TO QBR-ACCRUAL-MONTHS-3.
           MOVE BR-ACCRUAL-MONTHS(4) TO QBR-ACCRUAL-MONTHS-4.
           MOVE BR-ACCRUAL-MONTHS(5) TO QBR-ACCRUAL-MONTHS-5.
           MOVE BR-ACCRUAL-MONTHS(6) TO QBR-ACCRUAL-MONTHS-6.
           MOVE BR-ACCRUAL-MONTHS(7) TO QBR-ACCRUAL-MONTHS-7.
           MOVE BR-ACCRUAL-MONTHS(8) TO QBR-ACCRUAL-MONTHS-8.
           MOVE BR-ACCRUAL-MONTHS(9) TO QBR-ACCRUAL-MONTHS-9.
           MOVE BR-ACCRUAL-MONTHS(10) TO QBR-ACCRUAL-MONTHS-10.
           MOVE BR-ACCRUAL-MONTHS(11) TO QBR-ACCRUAL-MONTHS-11.
           MOVE BR-GLOVERSHORT TO QBR-GLOVERSHORT.
           MOVE BR-GLBANK TO QBR-GLBANK.
           MOVE BR-GLONHAND TO QBR-GLONHAND.
           MOVE BR-BANKBAL TO QBR-BANKBAL.
           MOVE BR-RETAIN-AMT TO QBR-RETAIN-AMT.
           MOVE BR-LCARDFG TO QBR-LCARDFG.
           MOVE BR-DATA-PATH TO QBR-DATA-PATH.
           MOVE BR-MACHINE TO QBR-MACHINE.
           MOVE BR-MANAGER TO QBR-MANAGER.
           MOVE BR-EOD-PROCEDURE TO QBR-EOD-PROCEDURE.
           MOVE BR-BP-PAYOFF-NONCASH TO QBR-BP-PAYOFF-NONCASH.
           MOVE BR-BP-PAYOFF-OV TO QBR-BP-PAYOFF-OV.
           MOVE BR-LI-DLCOMM-CMSI-O3-MAX TO QBR-LI-DLCOMM-CMSI-O3-MAX.
           MOVE BR-LR-GROUP-COLLID TO QBR-LR-GROUP-COLLID.
           MOVE BR-UNITPER-FG TO QBR-UNITPER-FG.
           MOVE BR-UNITPER-Y-D-FG TO QBR-UNITPER-Y-D-FG.
           MOVE BR-NO-PROMPT-FOR-OTHBR TO QBR-NO-PROMPT-FOR-OTHBR.
           MOVE BR-WORLD-BRANCH-TYPE TO QBR-WORLD-BRANCH-TYPE.
           MOVE BR-COL-QUEUE-BEHAVIOR-FG TO QBR-COL-QUEUE-BEHAVIOR-FG.
           MOVE BR-CONVERT-AUTO-ACCTNO TO QBR-CONVERT-AUTO-ACCTNO.
           MOVE BR-TS-REPO-INS-FG TO QBR-TS-REPO-INS-FG.
           MOVE BR-BOOK-SF-BR-ONLY TO QBR-BOOK-SF-BR-ONLY.
           MOVE BR-FORCE-VALID-CITY-COUNTY
            TO QBR-FORCE-VALID-CITY-COUNTY.
           MOVE BR-SET-COLLCD-DL-MFGCD TO QBR-SET-COLLCD-DL-MFGCD.
           MOVE BR-DEFAULT-SF-UNITPER-CD TO QBR-DEFAULT-SF-UNITPER-CD.
           MOVE BR-DEFAULT-SF-PURCD TO QBR-DEFAULT-SF-PURCD.
           MOVE BR-DEFAULT-LN-UNITPER-CD TO QBR-DEFAULT-LN-UNITPER-CD.
           MOVE BR-DEFAULT-LN-PURCD TO QBR-DEFAULT-LN-PURCD.
           MOVE BR-TAP-OTHER-DL TO QBR-TAP-OTHER-DL.
           MOVE BR-AH-AND-CL TO QBR-AH-AND-CL.
           MOVE BR-NO-CASH-PROCEEDS TO QBR-NO-CASH-PROCEEDS.
           MOVE BR-RI-BEFORE-REPO TO QBR-RI-BEFORE-REPO.
           MOVE BR-RI-BEFORE-P4P5 TO QBR-RI-BEFORE-P4P5.
           MOVE BR-NOT-OURS-REFUND TO QBR-NOT-OURS-REFUND.
           MOVE BR-SFDLO3-WINDOW-FG TO QBR-SFDLO3-WINDOW-FG.
           MOVE BR-CALC-INS-ADJ TO QBR-CALC-INS-ADJ.
           MOVE BR-AC-BEFORE-REPO TO QBR-AC-BEFORE-REPO.
           MOVE BR-AC-BEFORE-P4P5 TO QBR-AC-BEFORE-P4P5.
           MOVE BR-BANK-ROUTING-NO TO QBR-BANK-ROUTING-NO.
           MOVE BR-BANK-CLEARING-GL TO QBR-BANK-CLEARING-GL.
           MOVE BR-GLBANK-DEPOSIT TO QBR-GLBANK-DEPOSIT.
           MOVE BR-BANK-ACCTNO TO QBR-BANK-ACCTNO.
           MOVE BR-DEFAULT-SC-SUBCLASS TO QBR-DEFAULT-SC-SUBCLASS.
           MOVE BR-NO-OUT-DLR-RSV TO QBR-NO-OUT-DLR-RSV.
           MOVE BR-COLLECTOR-QUEUE TO QBR-COLLECTOR-QUEUE.
           MOVE BR-COLL-QUEUE-CON-REC TO QBR-COLL-QUEUE-CON-REC.
           MOVE BR-JE-ALLOCATION-SKIP TO QBR-JE-ALLOCATION-SKIP.
           MOVE BR-EXPAND-APP-INPUT TO QBR-EXPAND-APP-INPUT.
           MOVE BR-A50-CPCORP TO QBR-A50-CPCORP.
           MOVE BR-INBR-BILNSC-FG TO QBR-INBR-BILNSC-FG.
           MOVE BR-INBR-BRIEF-FG TO QBR-INBR-BRIEF-FG.
           MOVE BR-LEDGER-JUST-INTCHG TO QBR-LEDGER-JUST-INTCHG.
           MOVE BR-BYINQ-START-PAGE TO QBR-BYINQ-START-PAGE.
           MOVE BR-DSINQ-START-PAGE TO QBR-DSINQ-START-PAGE.
           MOVE BR-APP-REQUIRED-FOR-INPUT
            TO QBR-APP-REQUIRED-FOR-INPUT.
           MOVE BR-CREDIT-SCORING-FG TO QBR-CREDIT-SCORING-FG.
           MOVE BR-COL-QUEUE-LOW-TO-HI TO QBR-COL-QUEUE-LOW-TO-HI.
           MOVE BR-CAN-GROUP-ID TO QBR-CAN-GROUP-ID.
           MOVE BR-AUTO-LNSTAT-WINDOW TO QBR-AUTO-LNSTAT-WINDOW.
           MOVE BR-TIME-ZONE-FACTOR TO QBR-TIME-ZONE-FACTOR.
           MOVE BR-LONPF-OPT-DISPLAY TO QBR-LONPF-OPT-DISPLAY.
           MOVE BR-GIFTNET TO QBR-GIFTNET.
           MOVE BR-GIFTNET-LEDGER-LINE TO QBR-GIFTNET-LEDGER-LINE.
           MOVE BR-NO-FT-EMP TO QBR-NO-FT-EMP.
           MOVE BR-NO-PT-EMP TO QBR-NO-PT-EMP.
           MOVE BR-BALLOON-LOANS-FG TO QBR-BALLOON-LOANS-FG.
           MOVE BR-GL-TYPE-ALLOWED TO QBR-GL-TYPE-ALLOWED.
           MOVE BR-GL-ENTRY-DEFAULT TO QBR-GL-ENTRY-DEFAULT.
           MOVE BR-MIDSTREAM-1STPAY-FG TO QBR-MIDSTREAM-1STPAY-FG.
           MOVE BR-NODEF-1ST-X-MONTHS TO QBR-NODEF-1ST-X-MONTHS.
           MOVE BR-ADD-INS-FEECODE TO QBR-ADD-INS-FEECODE.
           MOVE BR-ROAD-G-SGL-FEECODE TO QBR-ROAD-G-SGL-FEECODE.
           MOVE BR-ROAD-G-FAM-FEECODE TO QBR-ROAD-G-FAM-FEECODE.
           MOVE BR-NODEF-IF-ZERO-AMT TO QBR-NODEF-IF-ZERO-AMT.
           MOVE BR-ASSESS-LATE-CHGS TO QBR-ASSESS-LATE-CHGS.
           MOVE BR-NEXT-SOLICIT-NO TO QBR-NEXT-SOLICIT-NO.
           MOVE BR-ADDON-FEECD-O3(1) TO QBR-ADDON-FEECD-O3-1.
           MOVE BR-ADDON-FEECD-O3(2) TO QBR-ADDON-FEECD-O3-2.
           MOVE BR-IMMEDIATE-RECEIPT TO QBR-IMMEDIATE-RECEIPT.
           MOVE BR-IMMEDIATE-DOC-PRINT TO QBR-IMMEDIATE-DOC-PRINT.
           MOVE BR-AUTO-PL-REF-AC TO QBR-AUTO-PL-REF-AC.
           MOVE BR-FAX-NUMBER TO QBR-FAX-NUMBER.
           MOVE BR-ALPHA-DRAWERS TO QBR-ALPHA-DRAWERS.
           MOVE BR-LATECHG-ON-ALLOTMENTS TO QBR-LATECHG-ON-ALLOTMENTS.
           MOVE BR-ALLOW-SYS-AS-EFFDATE TO QBR-ALLOW-SYS-AS-EFFDATE.
           MOVE BR-TAX-PREBOOK-CUST TO QBR-TAX-PREBOOK-CUST.
           MOVE BR-TAX-PREBOOK-NONCUST TO QBR-TAX-PREBOOK-NONCUST.
           MOVE BR-PASSWORD-DRAWER TO QBR-PASSWORD-DRAWER.
           MOVE BR-ONESTEP-RN-FG TO QBR-ONESTEP-RN-FG.
           MOVE BR-LI-LISCAN-FG TO QBR-LI-LISCAN-FG.
           MOVE BR-HIDE-END-BAL TO QBR-HIDE-END-BAL.
           MOVE BR-LBOX-NOPAYOFF TO QBR-LBOX-NOPAYOFF.
           MOVE BR-LBOX-ODDPAY TO QBR-LBOX-ODDPAY.
           MOVE BR-LBOX-LC TO QBR-LBOX-LC.
           MOVE BR-LBOX-NOPRIN TO QBR-LBOX-NOPRIN.
           MOVE BR-LBOX-FLASH TO QBR-LBOX-FLASH.
           MOVE BR-LBOX-ACTIONCD TO QBR-LBOX-ACTIONCD.
           MOVE BR-LBOX-MAXCON TO QBR-LBOX-MAXCON.
           MOVE BR-LBOX-MAXREC TO QBR-LBOX-MAXREC.
           MOVE BR-LBOX-PL TO QBR-LBOX-PL.
           MOVE BR-LBOX-JD TO QBR-LBOX-JD.
           MOVE BR-LBOX TO QBR-LBOX.
           MOVE BR-LATECHG-ON-LBOX TO QBR-LATECHG-ON-LBOX.
           MOVE BR-ALLOW-OV-TRCD TO QBR-ALLOW-OV-TRCD.
           MOVE BR-GLOBAL-QUEUE TO QBR-GLOBAL-QUEUE.
           MOVE BR-LBOX-PAYOFF-NONCASH TO QBR-LBOX-PAYOFF-NONCASH.
           MOVE BR-LBOX-PAYOFF-OV TO QBR-LBOX-PAYOFF-OV.
           MOVE BR-COLLECT-MANAGER TO QBR-COLLECT-MANAGER.
           MOVE BR-COLLECT-PHONE TO QBR-COLLECT-PHONE.
           MOVE BR-COLLECT-EXT TO QBR-COLLECT-EXT.
           MOVE BR-ALT-INTO-THIS-BRANCH TO QBR-ALT-INTO-THIS-BRANCH.
           MOVE BR-POFF-TITLE-WINDOW TO QBR-POFF-TITLE-WINDOW.
           MOVE BR-COUPON-STATEMENT TO QBR-COUPON-STATEMENT.
           MOVE BR-SUPERVISOR TO QBR-SUPERVISOR.
           MOVE BR-AREA-MANAGER TO QBR-AREA-MANAGER.
           MOVE BR-MIN-MONTHS-DEF TO QBR-MIN-MONTHS-DEF.
           MOVE BR-CC-FLAG TO QBR-CC-FLAG.
           MOVE BR-CC-ADR1 TO QBR-CC-ADR1.
           MOVE BR-CC-CITY TO QBR-CC-CITY.
           MOVE BR-CC-STATE TO QBR-CC-STATE.
           MOVE BR-CC-ZIP TO QBR-CC-ZIP.
           MOVE BR-MIN-MONTHS-D2 TO QBR-MIN-MONTHS-D2.
           MOVE BR-SKIP-MIDSTREAM-TRW TO QBR-SKIP-MIDSTREAM-TRW.
           MOVE BR-GL-ESCROW-BALANCE TO QBR-GL-ESCROW-BALANCE.
           MOVE BR-GL-ESCROW-PAYABLE TO QBR-GL-ESCROW-PAYABLE.
           MOVE BR-ALLOW-FORCE-OPEN-FG TO QBR-ALLOW-FORCE-OPEN-FG.
           MOVE BR-ACH-BANK-NO TO QBR-ACH-BANK-NO.
           MOVE BR-WIMENU-CHECK-WRITING TO QBR-WIMENU-CHECK-WRITING.
           MOVE BR-ADP-PAY-GROUP TO QBR-ADP-PAY-GROUP.
           MOVE BR-ALLOW-SCANNED-CKS TO QBR-ALLOW-SCANNED-CKS.
           MOVE BR-CHECK-DEPOSIT-GL TO QBR-CHECK-DEPOSIT-GL.
           MOVE BR-OPTION-PST-LASER TO QBR-OPTION-PST-LASER.
           MOVE BR-USE-DEBIT-CARDS TO QBR-USE-DEBIT-CARDS.
           MOVE BR-GL-DEBIT-CARD-PROCEEDS
                                   TO QBR-GL-DEBIT-CARD-PROCEEDS.
           MOVE BR-WKDAY-BANKID    TO QBR-WKDAY-BANKID.
           MOVE BR-PURGE-NOW-FLAG  TO QBR-PURGE-NOW-FLAG.
           MOVE BR-EMAIL           TO QBR-EMAIL.
           MOVE BR-MANAGER-EMAIL   TO QBR-MANAGER-EMAIL.
           MOVE BR-VERITEC-LICENSE TO QBR-VERITEC-LICENSE.
      *     MOVE BR-TEST-PP-AUTO-INSCOVR TO QBR-TEST-PP-AUTO-INSCOVR.
 
      * COPY "LIBGB/GBBRRN.CPY".
      * COPYMEMBER: LIBGB/GBBRRN
      * 20220923 BAH ADDED BR_VERITEC_LICENSE #1570
      * 20230419 BAH ADDED BR-TEST-PP-AUTO-INSCOVR #1594A
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
                   ISNULL(CAST(BRFILE.BR_LTOUCH_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(BRFILE.BR_NAME,''),
                   ISNULL(BRFILE.BR_ADR1,''),
                   ISNULL(BRFILE.BR_ADR2,''),
                   ISNULL(BRFILE.BR_CITY,''),
                   ISNULL(BRFILE.BR_STATE,''),
                   ISNULL(BRFILE.BR_ZIP,''),
                   BRFILE.BR_PHONE,
                   BRFILE.BR_RETAIN,
                   BRFILE.BR_NEXT_CHECK,
                   BRFILE.BR_PRINTER_1,
                   BRFILE.BR_PRINTER_2,
                   BRFILE.BR_PRINTER_3,
                   BRFILE.BR_PRINTER_4,
                   BRFILE.BR_FEE_GL_BRNO,
                   ISNULL(BRFILE.BR_REMIT1,''),
                   ISNULL(BRFILE.BR_REMIT2,''),
                   ISNULL(BRFILE.BR_REMIT3,''),
                   ISNULL(BRFILE.BR_STMNT1,''),
                   ISNULL(BRFILE.BR_STMNT2,''),
                   BRFILE.BR_INS_PRODUCER_NUM,
                   ISNULL(BRFILE.BR_DLRCHK,''),
                   ISNULL(BRFILE.BR_FRONT_END_FG,''),
                   ISNULL(BRFILE.BR_EMLNRG_PHONE,''),
                   ISNULL(BRFILE.BR_POST_GL,''),
                   ISNULL(BRFILE.BR_ALLOTMNTS,''),
                   ISNULL(BRFILE.BR_EXTRACT_AUTO_UPD,''),
                   ISNULL(BRFILE.BR_PCNI_LEDGER,''),
                   ISNULL(BRFILE.BR_PRINTERFG_1,''),
                   ISNULL(BRFILE.BR_PRINTERFG_2,''),
                   ISNULL(BRFILE.BR_PRINTERFG_3,''),
                   ISNULL(BRFILE.BR_PRINTERFG_4,''),
                   ISNULL(BRFILE.BR_NEGDLRSV,''),
                   ISNULL(BRFILE.BR_CPCORP_ORDER,''),
                   ISNULL(BRFILE.BR_PRIO_MEMO_FLAG,''),
                   ISNULL(BRFILE.BR_PREBILL_PL_JD_OPT,''),
                   ISNULL(BRFILE.BR_LOAN_NUM_FLAG,''),
                   ISNULL(BRFILE.BR_DOWNPY_FLAG,''),
                   ISNULL(BRFILE.BR_AUTO_INS_EARN,''),
                   ISNULL(BRFILE.BR_OTH_INS_LEVELS,''),
                   ISNULL(BRFILE.BR_ALLOW_INTERBR,''),
                   ISNULL(BRFILE.BR_UNER_DLANRP,''),
                   ISNULL(BRFILE.BR_SECOND_PROMISE,''),
                   ISNULL(BRFILE.BR_AVAIL_CREDIT,''),
                   ISNULL(BRFILE.BR_RECP1,''),
                   ISNULL(BRFILE.BR_RECP2,''),
                   ISNULL(BRFILE.BR_INC_WAIVERS,''),
                   ISNULL(BRFILE.BR_INC_INS,''),
                   BRFILE.BR_BP_NEXT_BATCH,
                   ISNULL(BRFILE.BR_BP_NOPAYOFF,''),
                   ISNULL(BRFILE.BR_BP_ODDPAY,''),
                   ISNULL(BRFILE.BR_BP_LC,''),
                   ISNULL(BRFILE.BR_BP_NOPRIN,''),
                   ISNULL(BRFILE.BR_BP_FLASH,''),
                   ISNULL(BRFILE.BR_BP_ACTIONCD,''),
                   BRFILE.BR_BP_MAXCON,
                   BRFILE.BR_BP_MAXREC,
                   ISNULL(BRFILE.BR_BP_PL,''),
                   ISNULL(BRFILE.BR_BP_JD,''),
                   BRFILE.BR_BP_BRANCH,
                   ISNULL(BRFILE.BR_AGE_ERN,''),
                   ISNULL(BRFILE.BR_OTHER_BRANCH_GL,''),
                   ISNULL(BRFILE.BR_SALES_SMALL,''),
                   ISNULL(BRFILE.BR_NEW_COLL_INQUIRY,''),
                   ISNULL(BRFILE.BR_CHANGE_MEMO,''),
                   ISNULL(BRFILE.BR_INS_ADDON,''),
                   ISNULL(BRFILE.BR_TRAILER_REC,''),
                   ISNULL(BRFILE.BR_ALDEL_FLAG,''),
                   ISNULL(BRFILE.BR_IB_LCHG_MESS,''),
                   ISNULL(BRFILE.BR_COL_QUEUE_PAST_DUE,''),
                   BRFILE.BR_LI_DLCOMM_CMSI_PP_MAX,
                   ISNULL(BRFILE.BR_ADDON_FEECD_O2_1,''),
                   ISNULL(BRFILE.BR_ADDON_FEECD_O2_2,''),
                   ISNULL(BRFILE.BR_AUTO_CHECK,''),
                   ISNULL(BRFILE.BR_RECEIPT_PROMDATE,''),
                   ISNULL(BRFILE.BR_RECEIPT_INTPD,''),
                   ISNULL(BRFILE.BR_COLL_ID_SCAN,''),
                   ISNULL(BRFILE.BR_STOP_ON_COLL,''),
                   ISNULL(BRFILE.BR_DUEDATE_CHANGE_CD,''),
                   ISNULL(BRFILE.BR_CHECK_EOM_FG,''),
                   ISNULL(BRFILE.BR_VOLUME_DLANRP,''),
                   ISNULL(BRFILE.BR_PDTH_NXDUE,''),
                   ISNULL(BRFILE.BR_LC_CODE,''),
                   ISNULL(BRFILE.BR_VALIDATE_PRUS,''),
                   ISNULL(BRFILE.BR_AUTO_OPEN_BAL,''),
                   BRFILE.BR_OPEN_BAL_1,
                   BRFILE.BR_OPEN_BAL_2,
                   BRFILE.BR_OPEN_BAL_3,
                   BRFILE.BR_OPEN_BAL_4,
                   BRFILE.BR_OPEN_BAL_5,
                   BRFILE.BR_OPEN_BAL_6,
                   BRFILE.BR_OPEN_BAL_7,
                   BRFILE.BR_OPEN_BAL_8,
                   BRFILE.BR_OPEN_BAL_9,
                   BRFILE.BR_OPEN_BAL_10,
                   BRFILE.BR_OPEN_BAL_11,
                   BRFILE.BR_OPEN_BAL_12,
                   BRFILE.BR_OPEN_BAL_13,
                   BRFILE.BR_OPEN_BAL_14,
                   BRFILE.BR_OPEN_BAL_15,
                   BRFILE.BR_OPEN_BAL_16,
                   BRFILE.BR_OPEN_BAL_17,
                   BRFILE.BR_OPEN_BAL_18,
                   BRFILE.BR_OPEN_BAL_19,
                   BRFILE.BR_OPEN_BAL_20,
                   ISNULL(BRFILE.BR_TENDER_CD_1,''),
                   ISNULL(BRFILE.BR_TENDER_CD_2,''),
                   ISNULL(BRFILE.BR_TENDER_CD_3,''),
                   ISNULL(BRFILE.BR_DLPARV_ADJ_FG,''),
                   ISNULL(BRFILE.BR_OVERRIDE_BR_STATE,''),
                   BRFILE.BR_DEFAULT_SC_CLASS,
                   ISNULL(BRFILE.BR_RECDEL_NOT_REC,''),
                   ISNULL(BRFILE.BR_POST_AHEAD_FG,''),
                   ISNULL(BRFILE.BR_CAN_LOANS_FG,''),
                   ISNULL(BRFILE.BR_NON_MONTHLY_CAN_LOANS,''),
                   ISNULL(BRFILE.BR_BP_ALLOW_PMT_ACCT_OTHBAL,''),
                   ISNULL(BRFILE.BR_LBOX_ALLOW_PMT_ACCT_OTHBAL,''),
                   BRFILE.BR_REGION_NUMBER,
                   ISNULL(BRFILE.BR_OUR_FEE,''),
                   BRFILE.BR_REB_METHOD_1,
                   BRFILE.BR_REB_METHOD_2,
                   BRFILE.BR_REB_METHOD_3,
                   BRFILE.BR_REB_METHOD_4,
                   BRFILE.BR_REB_METHOD_5,
                   BRFILE.BR_REB_METHOD_6,
                   BRFILE.BR_REB_METHOD_7,
                   BRFILE.BR_REB_METHOD_8,
                   BRFILE.BR_REB_METHOD_9,
                   BRFILE.BR_REB_METHOD_10,
                   BRFILE.BR_REB_METHOD_11,
                   BRFILE.BR_REBDAYS_MONTH_1,
                   BRFILE.BR_REBDAYS_MONTH_2,
                   BRFILE.BR_REBDAYS_MONTH_3,
                   BRFILE.BR_REBDAYS_MONTH_4,
                   BRFILE.BR_REBDAYS_MONTH_5,
                   BRFILE.BR_REBDAYS_MONTH_6,
                   BRFILE.BR_REBDAYS_MONTH_7,
                   BRFILE.BR_REBDAYS_MONTH_8,
                   BRFILE.BR_REBDAYS_MONTH_9,
                   BRFILE.BR_REBDAYS_MONTH_10,
                   BRFILE.BR_REBDAYS_MONTH_11,
                   BRFILE.BR_ACCRUAL_MONTHS_1,
                   BRFILE.BR_ACCRUAL_MONTHS_2,
                   BRFILE.BR_ACCRUAL_MONTHS_3,
                   BRFILE.BR_ACCRUAL_MONTHS_4,
                   BRFILE.BR_ACCRUAL_MONTHS_5,
                   BRFILE.BR_ACCRUAL_MONTHS_6,
                   BRFILE.BR_ACCRUAL_MONTHS_7,
                   BRFILE.BR_ACCRUAL_MONTHS_8,
                   BRFILE.BR_ACCRUAL_MONTHS_9,
                   BRFILE.BR_ACCRUAL_MONTHS_10,
                   BRFILE.BR_ACCRUAL_MONTHS_11,
                   BRFILE.BR_GLOVERSHORT,
                   BRFILE.BR_GLBANK,
                   BRFILE.BR_GLONHAND,
                   BRFILE.BR_BANKBAL,
                   BRFILE.BR_RETAIN_AMT,
                   ISNULL(BRFILE.BR_LCARDFG,''),
                   ISNULL(BRFILE.BR_DATA_PATH,''),
                   ISNULL(BRFILE.BR_MACHINE,''),
                   ISNULL(BRFILE.BR_MANAGER,''),
                   ISNULL(BRFILE.BR_EOD_PROCEDURE,''),
                   ISNULL(BRFILE.BR_BP_PAYOFF_NONCASH,''),
                   ISNULL(BRFILE.BR_BP_PAYOFF_OV,''),
                   BRFILE.BR_LI_DLCOMM_CMSI_O3_MAX,
                   ISNULL(BRFILE.BR_LR_GROUP_COLLID,''),
                   ISNULL(BRFILE.BR_UNITPER_FG,''),
                   ISNULL(BRFILE.BR_UNITPER_Y_D_FG,''),
                   ISNULL(BRFILE.BR_NO_PROMPT_FOR_OTHBR,''),
                   ISNULL(BRFILE.BR_WORLD_BRANCH_TYPE,''),
                   ISNULL(BRFILE.BR_COL_QUEUE_BEHAVIOR_FG,''),
                   ISNULL(BRFILE.BR_CONVERT_AUTO_ACCTNO,''),
                   ISNULL(BRFILE.BR_TS_REPO_INS_FG,''),
                   ISNULL(BRFILE.BR_BOOK_SF_BR_ONLY,''),
                   ISNULL(BRFILE.BR_FORCE_VALID_CITY_COUNTY,''),
                   ISNULL(BRFILE.BR_SET_COLLCD_DL_MFGCD,''),
                   ISNULL(BRFILE.BR_DEFAULT_SF_UNITPER_CD,''),
                   ISNULL(BRFILE.BR_DEFAULT_SF_PURCD,''),
                   ISNULL(BRFILE.BR_DEFAULT_LN_UNITPER_CD,''),
                   ISNULL(BRFILE.BR_DEFAULT_LN_PURCD,''),
                   ISNULL(BRFILE.BR_TAP_OTHER_DL,''),
                   ISNULL(BRFILE.BR_AH_AND_CL,''),
                   ISNULL(BRFILE.BR_NO_CASH_PROCEEDS,''),
                   ISNULL(BRFILE.BR_RI_BEFORE_REPO,''),
                   ISNULL(BRFILE.BR_RI_BEFORE_P4P5,''),
                   ISNULL(BRFILE.BR_NOT_OURS_REFUND,''),
                   ISNULL(BRFILE.BR_SFDLO3_WINDOW_FG,''),
                   ISNULL(BRFILE.BR_CALC_INS_ADJ,''),
                   ISNULL(BRFILE.BR_AC_BEFORE_REPO,''),
                   ISNULL(BRFILE.BR_AC_BEFORE_P4P5,''),
                   BRFILE.BR_BANK_ROUTING_NO,
                   BRFILE.BR_BANK_CLEARING_GL,
                   BRFILE.BR_GLBANK_DEPOSIT,
                   ISNULL(BRFILE.BR_BANK_ACCTNO,''),
                   BRFILE.BR_DEFAULT_SC_SUBCLASS,
                   ISNULL(BRFILE.BR_NO_OUT_DLR_RSV,''),
                   ISNULL(BRFILE.BR_COLLECTOR_QUEUE,''),
                   ISNULL(BRFILE.BR_COLL_QUEUE_CON_REC,''),
                   ISNULL(BRFILE.BR_JE_ALLOCATION_SKIP,''),
                   ISNULL(BRFILE.BR_EXPAND_APP_INPUT,''),
                   ISNULL(BRFILE.BR_A50_CPCORP,''),
                   ISNULL(BRFILE.BR_INBR_BILNSC_FG,''),
                   ISNULL(BRFILE.BR_INBR_BRIEF_FG,''),
                   ISNULL(BRFILE.BR_LEDGER_JUST_INTCHG,''),
                   BRFILE.BR_BYINQ_START_PAGE,
                   BRFILE.BR_DSINQ_START_PAGE,
                   ISNULL(BRFILE.BR_APP_REQUIRED_FOR_INPUT,''),
                   ISNULL(BRFILE.BR_CREDIT_SCORING_FG,''),
                   ISNULL(BRFILE.BR_COL_QUEUE_LOW_TO_HI,''),
                   ISNULL(BRFILE.BR_CAN_GROUP_ID,''),
                   ISNULL(BRFILE.BR_AUTO_LNSTAT_WINDOW,''),
                   BRFILE.BR_TIME_ZONE_FACTOR,
                   ISNULL(BRFILE.BR_LONPF_OPT_DISPLAY,''),
                   ISNULL(BRFILE.BR_GIFTNET,''),
                   ISNULL(BRFILE.BR_GIFTNET_LEDGER_LINE,''),
                   BRFILE.BR_NO_FT_EMP,
                   BRFILE.BR_NO_PT_EMP,
                   ISNULL(BRFILE.BR_BALLOON_LOANS_FG,''),
                   ISNULL(BRFILE.BR_GL_TYPE_ALLOWED,''),
                   ISNULL(BRFILE.BR_GL_ENTRY_DEFAULT,''),
                   ISNULL(BRFILE.BR_MIDSTREAM_1STPAY_FG,''),
                   BRFILE.BR_NODEF_1ST_X_MONTHS,
                   ISNULL(BRFILE.BR_ADD_INS_FEECODE,''),
                   ISNULL(BRFILE.BR_ROAD_G_SGL_FEECODE,''),
                   ISNULL(BRFILE.BR_ROAD_G_FAM_FEECODE,''),
                   ISNULL(BRFILE.BR_NODEF_IF_ZERO_AMT,''),
                   ISNULL(BRFILE.BR_ASSESS_LATE_CHGS,''),
                   BRFILE.BR_NEXT_SOLICIT_NO,
                   ISNULL(BRFILE.BR_ADDON_FEECD_O3_1,''),
                   ISNULL(BRFILE.BR_ADDON_FEECD_O3_2,''),
                   ISNULL(BRFILE.BR_IMMEDIATE_RECEIPT,''),
                   ISNULL(BRFILE.BR_IMMEDIATE_DOC_PRINT,''),
                   ISNULL(BRFILE.BR_AUTO_PL_REF_AC,''),
                   BRFILE.BR_FAX_NUMBER,
                   ISNULL(BRFILE.BR_ALPHA_DRAWERS,''),
                   ISNULL(BRFILE.BR_LATECHG_ON_ALLOTMENTS,''),
                   ISNULL(BRFILE.BR_ALLOW_SYS_AS_EFFDATE,''),
                   BRFILE.BR_TAX_PREBOOK_CUST,
                   BRFILE.BR_TAX_PREBOOK_NONCUST,
                   ISNULL(BRFILE.BR_PASSWORD_DRAWER,''),
                   ISNULL(BRFILE.BR_ONESTEP_RN_FG,''),
                   ISNULL(BRFILE.BR_LI_LISCAN_FG,''),
                   ISNULL(BRFILE.BR_HIDE_END_BAL,''),
                   ISNULL(BRFILE.BR_LBOX_NOPAYOFF,''),
                   ISNULL(BRFILE.BR_LBOX_ODDPAY,''),
                   ISNULL(BRFILE.BR_LBOX_LC,''),
                   ISNULL(BRFILE.BR_LBOX_NOPRIN,''),
                   ISNULL(BRFILE.BR_LBOX_FLASH,''),
                   ISNULL(BRFILE.BR_LBOX_ACTIONCD,''),
                   BRFILE.BR_LBOX_MAXCON,
                   BRFILE.BR_LBOX_MAXREC,
                   ISNULL(BRFILE.BR_LBOX_PL,''),
                   ISNULL(BRFILE.BR_LBOX_JD,''),
                   ISNULL(BRFILE.BR_LBOX,''),
                   ISNULL(BRFILE.BR_LATECHG_ON_LBOX,''),
                   ISNULL(BRFILE.BR_ALLOW_OV_TRCD,''),
                   ISNULL(BRFILE.BR_GLOBAL_QUEUE,''),
                   ISNULL(BRFILE.BR_LBOX_PAYOFF_NONCASH,''),
                   ISNULL(BRFILE.BR_LBOX_PAYOFF_OV,''),
                   ISNULL(BRFILE.BR_COLLECT_MANAGER,''),
                   BRFILE.BR_COLLECT_PHONE,
                   BRFILE.BR_COLLECT_EXT,
                   ISNULL(BRFILE.BR_ALT_INTO_THIS_BRANCH,''),
                   ISNULL(BRFILE.BR_POFF_TITLE_WINDOW,''),
                   ISNULL(BRFILE.BR_COUPON_STATEMENT,''),
                   ISNULL(BRFILE.BR_SUPERVISOR,''),
                   ISNULL(BRFILE.BR_AREA_MANAGER,''),
                   BRFILE.BR_MIN_MONTHS_DEF,
                   ISNULL(BRFILE.BR_CC_FLAG,''),
                   ISNULL(BRFILE.BR_CC_ADR1,''),
                   ISNULL(BRFILE.BR_CC_CITY,''),
                   ISNULL(BRFILE.BR_CC_STATE,''),
                   ISNULL(BRFILE.BR_CC_ZIP,''),
                   BRFILE.BR_MIN_MONTHS_D2,
                   ISNULL(BRFILE.BR_SKIP_MIDSTREAM_TRW,''),
                   BRFILE.BR_GL_ESCROW_BALANCE,
                   BRFILE.BR_GL_ESCROW_PAYABLE,
                   ISNULL(BRFILE.BR_ALLOW_FORCE_OPEN_FG,''),
                   BRFILE.BR_ACH_BANK_NO,
                   ISNULL(BRFILE.BR_WIMENU_CHECK_WRITING,''),
                   ISNULL(BRFILE.BR_ADP_PAY_GROUP,''),
                   ISNULL(BRFILE.BR_ALLOW_SCANNED_CKS,''),
                   BRFILE.BR_CHECK_DEPOSIT_GL,
                   ISNULL(BRFILE.BR_OPTION_PST_LASER,''),
                   ISNULL(BRFILE.BR_USE_DEBIT_CARDS,''),
                   BRFILE.BR_GL_DEBIT_CARD_PROCEEDS,
                   BRFILE.BR_WKDAY_BANKID,
                   ISNULL(BRFILE.BR_PURGE_NOW_FLAG,''),
                   ISNULL(BRFILE.BR_EMAIL,''),
                   ISNULL(BRFILE.BR_MANAGER_EMAIL,''),
                   BRFILE.BR_VERITEC_LICENSE
            FROM DBO.BRFILE
            WHERE BRFILE.BR_NO >= :QBR-WBEG-NO
              AND BRFILE.BR_NO <= :QBR-WEND-NO
            ORDER BY BR_NO
           END-EXEC.

           EXEC SQL
               OPEN BR_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

      *            ISNULL(BRFILE.BR_TEST_PP_AUTO_INSCOVR,'')

           MOVE ZEROES TO QBR-WBEG-NO
                          QBR-WEND-NO.

           MOVE STAT---GOOD TO BR-CURSOR-STAT.

      *-----------------------------------------------------------------
       READ-BR-FILE.
           PERFORM READ-IT.
           MOVE BR-SELECT-PATH TO E-FILE.
           MOVE BR-NO TO E-KEYX.

           MOVE BR-NO TO QBR-NO.

           EXEC SQL
            SELECT BRFILE.BR_NO,
                   ISNULL(CAST(BRFILE.BR_LTOUCH_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(BRFILE.BR_NAME,''),
                   ISNULL(BRFILE.BR_ADR1,''),
                   ISNULL(BRFILE.BR_ADR2,''),
                   ISNULL(BRFILE.BR_CITY,''),
                   ISNULL(BRFILE.BR_STATE,''),
                   ISNULL(BRFILE.BR_ZIP,''),
                   BRFILE.BR_PHONE,
                   BRFILE.BR_RETAIN,
                   BRFILE.BR_NEXT_CHECK,
                   BRFILE.BR_PRINTER_1,
                   BRFILE.BR_PRINTER_2,
                   BRFILE.BR_PRINTER_3,
                   BRFILE.BR_PRINTER_4,
                   BRFILE.BR_FEE_GL_BRNO,
                   ISNULL(BRFILE.BR_REMIT1,''),
                   ISNULL(BRFILE.BR_REMIT2,''),
                   ISNULL(BRFILE.BR_REMIT3,''),
                   ISNULL(BRFILE.BR_STMNT1,''),
                   ISNULL(BRFILE.BR_STMNT2,''),
                   BRFILE.BR_INS_PRODUCER_NUM,
                   ISNULL(BRFILE.BR_DLRCHK,''),
                   ISNULL(BRFILE.BR_FRONT_END_FG,''),
                   ISNULL(BRFILE.BR_EMLNRG_PHONE,''),
                   ISNULL(BRFILE.BR_POST_GL,''),
                   ISNULL(BRFILE.BR_ALLOTMNTS,''),
                   ISNULL(BRFILE.BR_EXTRACT_AUTO_UPD,''),
                   ISNULL(BRFILE.BR_PCNI_LEDGER,''),
                   ISNULL(BRFILE.BR_PRINTERFG_1,''),
                   ISNULL(BRFILE.BR_PRINTERFG_2,''),
                   ISNULL(BRFILE.BR_PRINTERFG_3,''),
                   ISNULL(BRFILE.BR_PRINTERFG_4,''),
                   ISNULL(BRFILE.BR_NEGDLRSV,''),
                   ISNULL(BRFILE.BR_CPCORP_ORDER,''),
                   ISNULL(BRFILE.BR_PRIO_MEMO_FLAG,''),
                   ISNULL(BRFILE.BR_PREBILL_PL_JD_OPT,''),
                   ISNULL(BRFILE.BR_LOAN_NUM_FLAG,''),
                   ISNULL(BRFILE.BR_DOWNPY_FLAG,''),
                   ISNULL(BRFILE.BR_AUTO_INS_EARN,''),
                   ISNULL(BRFILE.BR_OTH_INS_LEVELS,''),
                   ISNULL(BRFILE.BR_ALLOW_INTERBR,''),
                   ISNULL(BRFILE.BR_UNER_DLANRP,''),
                   ISNULL(BRFILE.BR_SECOND_PROMISE,''),
                   ISNULL(BRFILE.BR_AVAIL_CREDIT,''),
                   ISNULL(BRFILE.BR_RECP1,''),
                   ISNULL(BRFILE.BR_RECP2,''),
                   ISNULL(BRFILE.BR_INC_WAIVERS,''),
                   ISNULL(BRFILE.BR_INC_INS,''),
                   BRFILE.BR_BP_NEXT_BATCH,
                   ISNULL(BRFILE.BR_BP_NOPAYOFF,''),
                   ISNULL(BRFILE.BR_BP_ODDPAY,''),
                   ISNULL(BRFILE.BR_BP_LC,''),
                   ISNULL(BRFILE.BR_BP_NOPRIN,''),
                   ISNULL(BRFILE.BR_BP_FLASH,''),
                   ISNULL(BRFILE.BR_BP_ACTIONCD,''),
                   BRFILE.BR_BP_MAXCON,
                   BRFILE.BR_BP_MAXREC,
                   ISNULL(BRFILE.BR_BP_PL,''),
                   ISNULL(BRFILE.BR_BP_JD,''),
                   BRFILE.BR_BP_BRANCH,
                   ISNULL(BRFILE.BR_AGE_ERN,''),
                   ISNULL(BRFILE.BR_OTHER_BRANCH_GL,''),
                   ISNULL(BRFILE.BR_SALES_SMALL,''),
                   ISNULL(BRFILE.BR_NEW_COLL_INQUIRY,''),
                   ISNULL(BRFILE.BR_CHANGE_MEMO,''),
                   ISNULL(BRFILE.BR_INS_ADDON,''),
                   ISNULL(BRFILE.BR_TRAILER_REC,''),
                   ISNULL(BRFILE.BR_ALDEL_FLAG,''),
                   ISNULL(BRFILE.BR_IB_LCHG_MESS,''),
                   ISNULL(BRFILE.BR_COL_QUEUE_PAST_DUE,''),
                   BRFILE.BR_LI_DLCOMM_CMSI_PP_MAX,
                   ISNULL(BRFILE.BR_ADDON_FEECD_O2_1,''),
                   ISNULL(BRFILE.BR_ADDON_FEECD_O2_2,''),
                   ISNULL(BRFILE.BR_AUTO_CHECK,''),
                   ISNULL(BRFILE.BR_RECEIPT_PROMDATE,''),
                   ISNULL(BRFILE.BR_RECEIPT_INTPD,''),
                   ISNULL(BRFILE.BR_COLL_ID_SCAN,''),
                   ISNULL(BRFILE.BR_STOP_ON_COLL,''),
                   ISNULL(BRFILE.BR_DUEDATE_CHANGE_CD,''),
                   ISNULL(BRFILE.BR_CHECK_EOM_FG,''),
                   ISNULL(BRFILE.BR_VOLUME_DLANRP,''),
                   ISNULL(BRFILE.BR_PDTH_NXDUE,''),
                   ISNULL(BRFILE.BR_LC_CODE,''),
                   ISNULL(BRFILE.BR_VALIDATE_PRUS,''),
                   ISNULL(BRFILE.BR_AUTO_OPEN_BAL,''),
                   BRFILE.BR_OPEN_BAL_1,
                   BRFILE.BR_OPEN_BAL_2,
                   BRFILE.BR_OPEN_BAL_3,
                   BRFILE.BR_OPEN_BAL_4,
                   BRFILE.BR_OPEN_BAL_5,
                   BRFILE.BR_OPEN_BAL_6,
                   BRFILE.BR_OPEN_BAL_7,
                   BRFILE.BR_OPEN_BAL_8,
                   BRFILE.BR_OPEN_BAL_9,
                   BRFILE.BR_OPEN_BAL_10,
                   BRFILE.BR_OPEN_BAL_11,
                   BRFILE.BR_OPEN_BAL_12,
                   BRFILE.BR_OPEN_BAL_13,
                   BRFILE.BR_OPEN_BAL_14,
                   BRFILE.BR_OPEN_BAL_15,
                   BRFILE.BR_OPEN_BAL_16,
                   BRFILE.BR_OPEN_BAL_17,
                   BRFILE.BR_OPEN_BAL_18,
                   BRFILE.BR_OPEN_BAL_19,
                   BRFILE.BR_OPEN_BAL_20,
                   ISNULL(BRFILE.BR_TENDER_CD_1,''),
                   ISNULL(BRFILE.BR_TENDER_CD_2,''),
                   ISNULL(BRFILE.BR_TENDER_CD_3,''),
                   ISNULL(BRFILE.BR_DLPARV_ADJ_FG,''),
                   ISNULL(BRFILE.BR_OVERRIDE_BR_STATE,''),
                   BRFILE.BR_DEFAULT_SC_CLASS,
                   ISNULL(BRFILE.BR_RECDEL_NOT_REC,''),
                   ISNULL(BRFILE.BR_POST_AHEAD_FG,''),
                   ISNULL(BRFILE.BR_CAN_LOANS_FG,''),
                   ISNULL(BRFILE.BR_NON_MONTHLY_CAN_LOANS,''),
                   ISNULL(BRFILE.BR_BP_ALLOW_PMT_ACCT_OTHBAL,''),
                   ISNULL(BRFILE.BR_LBOX_ALLOW_PMT_ACCT_OTHBAL,''),
                   BRFILE.BR_REGION_NUMBER,
                   ISNULL(BRFILE.BR_OUR_FEE,''),
                   BRFILE.BR_REB_METHOD_1,
                   BRFILE.BR_REB_METHOD_2,
                   BRFILE.BR_REB_METHOD_3,
                   BRFILE.BR_REB_METHOD_4,
                   BRFILE.BR_REB_METHOD_5,
                   BRFILE.BR_REB_METHOD_6,
                   BRFILE.BR_REB_METHOD_7,
                   BRFILE.BR_REB_METHOD_8,
                   BRFILE.BR_REB_METHOD_9,
                   BRFILE.BR_REB_METHOD_10,
                   BRFILE.BR_REB_METHOD_11,
                   BRFILE.BR_REBDAYS_MONTH_1,
                   BRFILE.BR_REBDAYS_MONTH_2,
                   BRFILE.BR_REBDAYS_MONTH_3,
                   BRFILE.BR_REBDAYS_MONTH_4,
                   BRFILE.BR_REBDAYS_MONTH_5,
                   BRFILE.BR_REBDAYS_MONTH_6,
                   BRFILE.BR_REBDAYS_MONTH_7,
                   BRFILE.BR_REBDAYS_MONTH_8,
                   BRFILE.BR_REBDAYS_MONTH_9,
                   BRFILE.BR_REBDAYS_MONTH_10,
                   BRFILE.BR_REBDAYS_MONTH_11,
                   BRFILE.BR_ACCRUAL_MONTHS_1,
                   BRFILE.BR_ACCRUAL_MONTHS_2,
                   BRFILE.BR_ACCRUAL_MONTHS_3,
                   BRFILE.BR_ACCRUAL_MONTHS_4,
                   BRFILE.BR_ACCRUAL_MONTHS_5,
                   BRFILE.BR_ACCRUAL_MONTHS_6,
                   BRFILE.BR_ACCRUAL_MONTHS_7,
                   BRFILE.BR_ACCRUAL_MONTHS_8,
                   BRFILE.BR_ACCRUAL_MONTHS_9,
                   BRFILE.BR_ACCRUAL_MONTHS_10,
                   BRFILE.BR_ACCRUAL_MONTHS_11,
                   BRFILE.BR_GLOVERSHORT,
                   BRFILE.BR_GLBANK,
                   BRFILE.BR_GLONHAND,
                   BRFILE.BR_BANKBAL,
                   BRFILE.BR_RETAIN_AMT,
                   ISNULL(BRFILE.BR_LCARDFG,''),
                   ISNULL(BRFILE.BR_DATA_PATH,''),
                   ISNULL(BRFILE.BR_MACHINE,''),
                   ISNULL(BRFILE.BR_MANAGER,''),
                   ISNULL(BRFILE.BR_EOD_PROCEDURE,''),
                   ISNULL(BRFILE.BR_BP_PAYOFF_NONCASH,''),
                   ISNULL(BRFILE.BR_BP_PAYOFF_OV,''),
                   BRFILE.BR_LI_DLCOMM_CMSI_O3_MAX,
                   ISNULL(BRFILE.BR_LR_GROUP_COLLID,''),
                   ISNULL(BRFILE.BR_UNITPER_FG,''),
                   ISNULL(BRFILE.BR_UNITPER_Y_D_FG,''),
                   ISNULL(BRFILE.BR_NO_PROMPT_FOR_OTHBR,''),
                   ISNULL(BRFILE.BR_WORLD_BRANCH_TYPE,''),
                   ISNULL(BRFILE.BR_COL_QUEUE_BEHAVIOR_FG,''),
                   ISNULL(BRFILE.BR_CONVERT_AUTO_ACCTNO,''),
                   ISNULL(BRFILE.BR_TS_REPO_INS_FG,''),
                   ISNULL(BRFILE.BR_BOOK_SF_BR_ONLY,''),
                   ISNULL(BRFILE.BR_FORCE_VALID_CITY_COUNTY,''),
                   ISNULL(BRFILE.BR_SET_COLLCD_DL_MFGCD,''),
                   ISNULL(BRFILE.BR_DEFAULT_SF_UNITPER_CD,''),
                   ISNULL(BRFILE.BR_DEFAULT_SF_PURCD,''),
                   ISNULL(BRFILE.BR_DEFAULT_LN_UNITPER_CD,''),
                   ISNULL(BRFILE.BR_DEFAULT_LN_PURCD,''),
                   ISNULL(BRFILE.BR_TAP_OTHER_DL,''),
                   ISNULL(BRFILE.BR_AH_AND_CL,''),
                   ISNULL(BRFILE.BR_NO_CASH_PROCEEDS,''),
                   ISNULL(BRFILE.BR_RI_BEFORE_REPO,''),
                   ISNULL(BRFILE.BR_RI_BEFORE_P4P5,''),
                   ISNULL(BRFILE.BR_NOT_OURS_REFUND,''),
                   ISNULL(BRFILE.BR_SFDLO3_WINDOW_FG,''),
                   ISNULL(BRFILE.BR_CALC_INS_ADJ,''),
                   ISNULL(BRFILE.BR_AC_BEFORE_REPO,''),
                   ISNULL(BRFILE.BR_AC_BEFORE_P4P5,''),
                   BRFILE.BR_BANK_ROUTING_NO,
                   BRFILE.BR_BANK_CLEARING_GL,
                   BRFILE.BR_GLBANK_DEPOSIT,
                   ISNULL(BRFILE.BR_BANK_ACCTNO,''),
                   BRFILE.BR_DEFAULT_SC_SUBCLASS,
                   ISNULL(BRFILE.BR_NO_OUT_DLR_RSV,''),
                   ISNULL(BRFILE.BR_COLLECTOR_QUEUE,''),
                   ISNULL(BRFILE.BR_COLL_QUEUE_CON_REC,''),
                   ISNULL(BRFILE.BR_JE_ALLOCATION_SKIP,''),
                   ISNULL(BRFILE.BR_EXPAND_APP_INPUT,''),
                   ISNULL(BRFILE.BR_A50_CPCORP,''),
                   ISNULL(BRFILE.BR_INBR_BILNSC_FG,''),
                   ISNULL(BRFILE.BR_INBR_BRIEF_FG,''),
                   ISNULL(BRFILE.BR_LEDGER_JUST_INTCHG,''),
                   BRFILE.BR_BYINQ_START_PAGE,
                   BRFILE.BR_DSINQ_START_PAGE,
                   ISNULL(BRFILE.BR_APP_REQUIRED_FOR_INPUT,''),
                   ISNULL(BRFILE.BR_CREDIT_SCORING_FG,''),
                   ISNULL(BRFILE.BR_COL_QUEUE_LOW_TO_HI,''),
                   ISNULL(BRFILE.BR_CAN_GROUP_ID,''),
                   ISNULL(BRFILE.BR_AUTO_LNSTAT_WINDOW,''),
                   BRFILE.BR_TIME_ZONE_FACTOR,
                   ISNULL(BRFILE.BR_LONPF_OPT_DISPLAY,''),
                   ISNULL(BRFILE.BR_GIFTNET,''),
                   ISNULL(BRFILE.BR_GIFTNET_LEDGER_LINE,''),
                   BRFILE.BR_NO_FT_EMP,
                   BRFILE.BR_NO_PT_EMP,
                   ISNULL(BRFILE.BR_BALLOON_LOANS_FG,''),
                   ISNULL(BRFILE.BR_GL_TYPE_ALLOWED,''),
                   ISNULL(BRFILE.BR_GL_ENTRY_DEFAULT,''),
                   ISNULL(BRFILE.BR_MIDSTREAM_1STPAY_FG,''),
                   BRFILE.BR_NODEF_1ST_X_MONTHS,
                   ISNULL(BRFILE.BR_ADD_INS_FEECODE,''),
                   ISNULL(BRFILE.BR_ROAD_G_SGL_FEECODE,''),
                   ISNULL(BRFILE.BR_ROAD_G_FAM_FEECODE,''),
                   ISNULL(BRFILE.BR_NODEF_IF_ZERO_AMT,''),
                   ISNULL(BRFILE.BR_ASSESS_LATE_CHGS,''),
                   BRFILE.BR_NEXT_SOLICIT_NO,
                   ISNULL(BRFILE.BR_ADDON_FEECD_O3_1,''),
                   ISNULL(BRFILE.BR_ADDON_FEECD_O3_2,''),
                   ISNULL(BRFILE.BR_IMMEDIATE_RECEIPT,''),
                   ISNULL(BRFILE.BR_IMMEDIATE_DOC_PRINT,''),
                   ISNULL(BRFILE.BR_AUTO_PL_REF_AC,''),
                   BRFILE.BR_FAX_NUMBER,
                   ISNULL(BRFILE.BR_ALPHA_DRAWERS,''),
                   ISNULL(BRFILE.BR_LATECHG_ON_ALLOTMENTS,''),
                   ISNULL(BRFILE.BR_ALLOW_SYS_AS_EFFDATE,''),
                   BRFILE.BR_TAX_PREBOOK_CUST,
                   BRFILE.BR_TAX_PREBOOK_NONCUST,
                   ISNULL(BRFILE.BR_PASSWORD_DRAWER,''),
                   ISNULL(BRFILE.BR_ONESTEP_RN_FG,''),
                   ISNULL(BRFILE.BR_LI_LISCAN_FG,''),
                   ISNULL(BRFILE.BR_HIDE_END_BAL,''),
                   ISNULL(BRFILE.BR_LBOX_NOPAYOFF,''),
                   ISNULL(BRFILE.BR_LBOX_ODDPAY,''),
                   ISNULL(BRFILE.BR_LBOX_LC,''),
                   ISNULL(BRFILE.BR_LBOX_NOPRIN,''),
                   ISNULL(BRFILE.BR_LBOX_FLASH,''),
                   ISNULL(BRFILE.BR_LBOX_ACTIONCD,''),
                   BRFILE.BR_LBOX_MAXCON,
                   BRFILE.BR_LBOX_MAXREC,
                   ISNULL(BRFILE.BR_LBOX_PL,''),
                   ISNULL(BRFILE.BR_LBOX_JD,''),
                   ISNULL(BRFILE.BR_LBOX,''),
                   ISNULL(BRFILE.BR_LATECHG_ON_LBOX,''),
                   ISNULL(BRFILE.BR_ALLOW_OV_TRCD,''),
                   ISNULL(BRFILE.BR_GLOBAL_QUEUE,''),
                   ISNULL(BRFILE.BR_LBOX_PAYOFF_NONCASH,''),
                   ISNULL(BRFILE.BR_LBOX_PAYOFF_OV,''),
                   ISNULL(BRFILE.BR_COLLECT_MANAGER,''),
                   BRFILE.BR_COLLECT_PHONE,
                   BRFILE.BR_COLLECT_EXT,
                   ISNULL(BRFILE.BR_ALT_INTO_THIS_BRANCH,''),
                   ISNULL(BRFILE.BR_POFF_TITLE_WINDOW,''),
                   ISNULL(BRFILE.BR_COUPON_STATEMENT,''),
                   ISNULL(BRFILE.BR_SUPERVISOR,''),
                   ISNULL(BRFILE.BR_AREA_MANAGER,''),
                   BRFILE.BR_MIN_MONTHS_DEF,
                   ISNULL(BRFILE.BR_CC_FLAG,''),
                   ISNULL(BRFILE.BR_CC_ADR1,''),
                   ISNULL(BRFILE.BR_CC_CITY,''),
                   ISNULL(BRFILE.BR_CC_STATE,''),
                   ISNULL(BRFILE.BR_CC_ZIP,''),
                   BRFILE.BR_MIN_MONTHS_D2,
                   ISNULL(BRFILE.BR_SKIP_MIDSTREAM_TRW,''),
                   BRFILE.BR_GL_ESCROW_BALANCE,
                   BRFILE.BR_GL_ESCROW_PAYABLE,
                   ISNULL(BRFILE.BR_ALLOW_FORCE_OPEN_FG,''),
                   BRFILE.BR_ACH_BANK_NO,
                   ISNULL(BRFILE.BR_WIMENU_CHECK_WRITING,''),
                   ISNULL(BRFILE.BR_ADP_PAY_GROUP,''),
                   ISNULL(BRFILE.BR_ALLOW_SCANNED_CKS,''),
                   BRFILE.BR_CHECK_DEPOSIT_GL,
                   ISNULL(BRFILE.BR_OPTION_PST_LASER,''),
                   ISNULL(BRFILE.BR_USE_DEBIT_CARDS,''),
                   BRFILE.BR_GL_DEBIT_CARD_PROCEEDS,
                   BRFILE.BR_WKDAY_BANKID,
                   ISNULL(BRFILE.BR_PURGE_NOW_FLAG,''),
                   ISNULL(BRFILE.BR_EMAIL,''),
                   ISNULL(BRFILE.BR_MANAGER_EMAIL,''),
                   BRFILE.BR_VERITEC_LICENSE 
            INTO :QBR-REC
            FROM DBO.BRFILE
            WHERE BRFILE.BR_NO = :QBR-NO
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

      *            ISNULL(BRFILE.BR_TEST_PP_AUTO_INSCOVR,'')

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
                   FETCH BR_CURSOR INTO :QBR-REC
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
	 
       COPY "LIBGB/GBGBGS_SQL.CPY".
       COPY "LIBGB/GBPRINT.CPY".
       COPY "LIBLP/LPLC1RN.CPY".
       COPY "LIBLP/LPBW1RN.CPY".
      * COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBGB/GBGBRN.CPY".
       COPY "LIBGB/GBGR1RN.CPY".
       COPY "LIBLP/LPSP1RN.CPY".
       COPY "LIBLP/LPLP1RN.CPY".
       COPY "LIBLP/LPLM1RN.CPY".
       COPY "LIBGB/GBWKIN.CPY".
       COPY "LIBLP/LPLN1RN.CPY".
       COPY "LIBLP/LPLTC1RN.CPY".
       COPY "LIBLP/LPLTP1RN.CPY".
       COPY "LIBLP/LPLTI1RN.CPY".
      * COPY "LIBLP/LPLN3RN.CPY".

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

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    CREATE PATH FOR FILE SELECT AND SQL TABLE NAME
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( EXT-IO-LNFILE NOT = "S" )
              MOVE LN-PATH-BEG             TO LN-SELECT-PATH-BEG
              MOVE LN-PATH-FILE            TO LN-SELECT-PATH-FILE
           ELSE
              MOVE LN-PATH-SQL             TO LN-SELECT-PATH.

      *-----------------------------------------------------------------
       OPEN-LN3-FILE.
           PERFORM LOAD-LN3-FILE.
           PERFORM OPEN-IT.
           MOVE LN-SELECT-PATH TO E-FILE.

      *    EXEC SQL
      *         CONNECT TO :SQL-CONNECT-SERVER AS LN3_CONNECT
      *               USER :SQL-CONNECT-USERID USING :SQL-CONNECT-PSWD
      *    END-EXEC.


      *    ACCEPT SQL-CONNECT-SERVER FROM ENVIRONMENT
      *                 "OTIS_DSN_NAME".
      *    ACCEPT SQL-CONNECT-USERID 
      *      FROM ENVIRONMENT "OTIS_ACUSQL_LOGIN".
      *    ACCEPT SQL-CONNECT-PSWD FROM ENVIRONMENT "OTIS_ACUSQL_PSWD".


           EXEC SQL CONNECT TO :SQL-CONNECT-SERVER AS LN3_CONNECT
                    USER :SQL-CONNECT-USERID USING :SQL-CONNECT-PSWD
           END-EXEC.


      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

           EXEC SQL SET CONNECTION C1 END-EXEC.

      *-----------------------------------------------------------------
       START-LN3-FILE.
           PERFORM START-IT.
           MOVE LN-PATH-OWNBR  TO LN-OWNBR.
           MOVE LN-SELECT-PATH TO E-FILE.
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

           EXEC SQL SET CONNECTION LN3_CONNECT END-EXEC.

           EXEC SQL
            DECLARE LN3_CURSOR CURSOR FOR
                :RET-CODE = EXEC SP_RZCORP_LNFILE_SELECT
              ( :QLN3-WBEG-OWNBR, :QLN3-WEND-OWNBR,  :QLN3-WBEG-CLASS,
                :QLN3-WEND-CLASS, :QLN3-WBEG-ACCTNO, :QLN3-WEND-ACCTNO )
           END-EXEC.

 
           EXEC SQL
               OPEN LN3_CURSOR
           END-EXEC.

      *    EXEC SQL SET CONNECTION LN3_CONNECT END-EXEC.

      *    EXEC SQL
      *     DECLARE LN3_CURSOR CURSOR FOR DYNAMIC_SQL
      *    END-EXEC.

      *    MOVE '{CALL SP_RZAGED_LNFILE_SELECT(?, ?, ?, ?, ?, ?)}'
      *                               TO LN-COMMAND.
      *    EXEC SQL
      *         PREPARE DYNAMIC_SQL FROM :LN-COMMAND
      *    END-EXEC.

      *    EXEC SQL
      *     OPEN LN3_CURSOR USING :QLN3-WBEG-OWNBR, :QLN3-WEND-OWNBR,
      *                           :QLN3-WBEG-CLASS, :QLN3-WEND-CLASS,
      *                           :QLN3-WBEG-ACCTNO, :QLN3-WEND-ACCTNO
      *    END-EXEC.


      *    PERFORM SQL-IO-VALIDATION.

      *    MOVE ZEROES TO QLN3-WBEG-OWNBR
      *                   QLN3-WEND-OWNBR
      *                   QLN3-WBEG-CLASS
      *                   QLN3-WEND-CLASS
      *                   QLN3-WBEG-ACCTNO
      *                   QLN3-WEND-ACCTNO.

           MOVE STAT---GOOD TO LN3-CURSOR-STAT.

           EXEC SQL SET CONNECTION C1 END-EXEC. 

      *-----------------------------------------------------------------
       READ-LN3-FILE.
           PERFORM READ-IT.
           MOVE LN-PATH-OWNBR  TO LN-OWNBR.
           MOVE LN-SELECT-PATH TO E-FILE.
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
                   LNFILE.LN_SSNO1,
                   ISNULL(LNFILE.LN_MAKERCD1,''),
                   LNFILE.LN_SSNO2,
                   ISNULL(LNFILE.LN_MAKERCD2,''),
                   LNFILE.LN_SSNO3,
                   ISNULL(LNFILE.LN_MAKERCD3,''),
                   LNFILE.LN_SSNO4,
                   ISNULL(LNFILE.LN_MAKERCD4,''),
                   ISNULL(LNFILE.LN_ORGST,''),
                   LNFILE.LN_ORGBR,
                   ISNULL(LNFILE.LN_OWNST,''),
                   LNFILE.LN_SPRCLASS,
                   ISNULL(LNFILE.LN_CONVERCD,''),
                   ISNULL(LNFILE.LN_MIDSTREAM,''),
                   ISNULL(LNFILE.LN_PURCD,''),
                   ISNULL(LNFILE.LN_SRCD,''),
                   ISNULL(LNFILE.LN_COLLCD,''),
                   ISNULL(LNFILE.LN_COLLID,''),
                   ISNULL(LNFILE.LN_POCD,''),
                   ISNULL(LNFILE.LN_PLCD,''),
                   ISNULL(LNFILE.LN_PLTYPE,''),
                   ISNULL(LNFILE.LN_CONFIDENTCD,''),
                   ISNULL(LNFILE.LN_ALLOTCD,''),
                   ISNULL(LNFILE.LN_RECASTCD,''),
                   ISNULL(LNFILE.LN_SKIP_TRW,''),
                   ISNULL(LNFILE.LN_STATUSFG,''),
                   ISNULL(LNFILE.LN_SMALLOAN,''),
                   ISNULL(LNFILE.LN_SALEFIN,''),
                   ISNULL(LNFILE.LN_SERVACCT,''),
                   ISNULL(LNFILE.LN_LOANTYPE,''),
                   ISNULL(LNFILE.LN_ADDON_FG,''),
                   ISNULL(LNFILE.LN_PAY_SCHLD_FG,''),
                   LNFILE.LN_LC_CODE,
                   ISNULL(LNFILE.LN_CAN_LOAN_NO,''),
                   ISNULL(LNFILE.LN_STATFLAGS,''),
                   ISNULL(CAST(LNFILE.LN_ENTDATE AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_LOANDATE AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_PURDATE AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_INTDATE AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_CASHDATE AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_FILEDATE AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_RESCISSION_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_PART_RECOURSE_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_ORIG_1STPYDATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_1STPYDATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_JDDATE AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_PLDATE AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_PL_TRDATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_ACCELDATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_REDUDATE AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_POFFDATE AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_PO_TRDATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_LEGALDATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_BNKRPTDATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_BNKRPT_DISMISS_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_BNKRPT_DISCHARGE_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_BNKRPT_REAFFIRM_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_DEBTAIDATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_REPODATE AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_REDEEMDATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_SOLDDATE AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_INS_CONFIRM_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_PRIOR_RN_FOR10_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_LAST_RN_FOR10_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_TRW_DOFD_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_LAST_PYPA_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_BT_TRDATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_METRO2_LAST_PMT_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_FOLLOWUP_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_LAST_ACCRUAL_DATE
                                                  AS VARCHAR(10)),'0'),
                   LNFILE.LN_APRATE,
                   LNFILE.LN_EFFRATE,
                   LNFILE.LN_SMPRATE,
                   LNFILE.LN_YLDRATE,
                   LNFILE.LN_OVRDRATE,
                   LNFILE.LN_ENDORATE,
                   LNFILE.LN_JDRATE,
                   LNFILE.LN_ACCRATE,
                   LNFILE.LN_REDURATE,
                   LNFILE.LN_CONRATE,
                   LNFILE.LN_SERVPER,
                   LNFILE.LN_SELLPR,
                   LNFILE.LN_DOWNPY,
                   LNFILE.LN_TRADEIN,
                   LNFILE.LN_LNAMT,
                   LNFILE.LN_FINCHG,
                   LNFILE.LN_INTCHG,
                   LNFILE.LN_SERCHG,
                   LNFILE.LN_EXTCHG,
                   LNFILE.LN_MAINTFEE,
                   LNFILE.LN_POINTS,
                   LNFILE.LN_PROCEEDS,
                   LNFILE.LN_ORGTERM,
                   LNFILE.LN_1STPYAMT,
                   LNFILE.LN_NOREGPY,
                   LNFILE.LN_REGPYAMT,
                   LNFILE.LN_LASTPYAMT,
                   LNFILE.LN_PROCASH,
                   LNFILE.LN_TILLNO,
                   ISNULL(LNFILE.LN_STATE_FLAG,''),
                   LNFILE.LN_STATE_WORKER,
                   LNFILE.LN_STATE_WORKER2,
                   ISNULL(LNFILE.LN_FEECODE_1,''),
                   LNFILE.LN_FEEAMT_1,
                   ISNULL(LNFILE.LN_FEEOURS_1,''),
                   ISNULL(LNFILE.LN_FEEFINCHG_1,''),
                   ISNULL(LNFILE.LN_FEECODE_2,''),
                   LNFILE.LN_FEEAMT_2,
                   ISNULL(LNFILE.LN_FEEOURS_2,''),
                   ISNULL(LNFILE.LN_FEEFINCHG_2,''),
                   ISNULL(LNFILE.LN_FEECODE_3,''),
                   LNFILE.LN_FEEAMT_3,
                   ISNULL(LNFILE.LN_FEEOURS_3,''),
                   ISNULL(LNFILE.LN_FEEFINCHG_3,''),
                   ISNULL(LNFILE.LN_FEECODE_4,''),
                   LNFILE.LN_FEEAMT_4,
                   ISNULL(LNFILE.LN_FEEOURS_4,''),
                   ISNULL(LNFILE.LN_FEEFINCHG_4,''),
                   ISNULL(LNFILE.LN_FEECODE_5,''),
                   LNFILE.LN_FEEAMT_5,
                   ISNULL(LNFILE.LN_FEEOURS_5,''),
                   ISNULL(LNFILE.LN_FEEFINCHG_5,''),
                   ISNULL(LNFILE.LN_FEECODE_6,''),
                   LNFILE.LN_FEEAMT_6,
                   ISNULL(LNFILE.LN_FEEOURS_6,''),
                   ISNULL(LNFILE.LN_FEEFINCHG_6,''),
                   ISNULL(LNFILE.LN_FEECODE_7,''),
                   LNFILE.LN_FEEAMT_7,
                   ISNULL(LNFILE.LN_FEEOURS_7,''),
                   ISNULL(LNFILE.LN_FEEFINCHG_7,''),
                   ISNULL(LNFILE.LN_FEECODE_8,''),
                   LNFILE.LN_FEEAMT_8,
                   ISNULL(LNFILE.LN_FEEOURS_8,''),
                   ISNULL(LNFILE.LN_FEEFINCHG_8,''),
                   ISNULL(LNFILE.LN_FEECODE_9,''),
                   LNFILE.LN_FEEAMT_9,
                   ISNULL(LNFILE.LN_FEEOURS_9,''),
                   ISNULL(LNFILE.LN_FEEFINCHG_9,''),
                   ISNULL(LNFILE.LN_FEECODE_10,''),
                   LNFILE.LN_FEEAMT_10,
                   ISNULL(LNFILE.LN_FEEOURS_10,''),
                   ISNULL(LNFILE.LN_FEEFINCHG_10,''),
                   ISNULL(LNFILE.LN_FEECODE_11,''),
                   LNFILE.LN_FEEAMT_11,
                   ISNULL(LNFILE.LN_FEEOURS_11,''),
                   ISNULL(LNFILE.LN_FEEFINCHG_11,''),
                   ISNULL(LNFILE.LN_FEECODE_12,''),
                   LNFILE.LN_FEEAMT_12,
                   ISNULL(LNFILE.LN_FEEOURS_12,''),
                   ISNULL(LNFILE.LN_FEEFINCHG_12,''),
                   ISNULL(LNFILE.LN_FEECODE_13,''),
                   LNFILE.LN_FEEAMT_13,
                   ISNULL(LNFILE.LN_FEEOURS_13,''),
                   ISNULL(LNFILE.LN_FEEFINCHG_13,''),
                   ISNULL(LNFILE.LN_FEECODE_14,''),
                   LNFILE.LN_FEEAMT_14,
                   ISNULL(LNFILE.LN_FEEOURS_14,''),
                   ISNULL(LNFILE.LN_FEEFINCHG_14,''),
                   ISNULL(LNFILE.LN_FEECODE_15,''),
                   LNFILE.LN_FEEAMT_15,
                   ISNULL(LNFILE.LN_FEEOURS_15,''),
                   ISNULL(LNFILE.LN_FEEFINCHG_15,''),
                   ISNULL(LNFILE.LN_FEECODE_16,''),
                   LNFILE.LN_FEEAMT_16,
                   ISNULL(LNFILE.LN_FEEOURS_16,''),
                   ISNULL(LNFILE.LN_FEEFINCHG_16,''),
                   ISNULL(LNFILE.LN_FEECODE_17,''),
                   LNFILE.LN_FEEAMT_17,
                   ISNULL(LNFILE.LN_FEEOURS_17,''),
                   ISNULL(LNFILE.LN_FEEFINCHG_17,''),
                   ISNULL(LNFILE.LN_FEECODE_18,''),
                   LNFILE.LN_FEEAMT_18,
                   ISNULL(LNFILE.LN_FEEOURS_18,''),
                   ISNULL(LNFILE.LN_FEEFINCHG_18,''),
                   ISNULL(LNFILE.LN_FEECODE_19,''),
                   LNFILE.LN_FEEAMT_19,
                   ISNULL(LNFILE.LN_FEEOURS_19,''),
                   ISNULL(LNFILE.LN_FEEFINCHG_19,''),
                   ISNULL(LNFILE.LN_FEECODE_20,''),
                   LNFILE.LN_FEEAMT_20,
                   ISNULL(LNFILE.LN_FEEOURS_20,''),
                   ISNULL(LNFILE.LN_FEEFINCHG_20,''),
                   ISNULL(LNFILE.LN_INSURED,''),
                   ISNULL(LNFILE.LN_INSTYPES,''),
                   ISNULL(LNFILE.LN_INSELIGIBLE,''),
                   LNFILE.LN_INSCOMP_1,
                   ISNULL(LNFILE.LN_INSOURS_1,''),
                   ISNULL(CAST(LNFILE.LN_INSEFF_DATE_1
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_INSEXP_DATE_1
                                                  AS VARCHAR(10)),'0'),
                   LNFILE.LN_INSPREM_1,
                   LNFILE.LN_INSCOVR_1,
                   LNFILE.LN_INSCOMM_1,
                   LNFILE.LN_INSANTICERN_1,
                   LNFILE.LN_INSANTICADJ_1,
                   LNFILE.LN_INSUFERN_1,
                   LNFILE.LN_INSACCERN_1,
                   LNFILE.LN_INSEARN_ORDER_1,
                   ISNULL(LNFILE.LN_INSFINCHG_1,''),
                   LNFILE.LN_INSCOMP_2,
                   ISNULL(LNFILE.LN_INSOURS_2,''),
                   ISNULL(CAST(LNFILE.LN_INSEFF_DATE_2
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_INSEXP_DATE_2
                                                  AS VARCHAR(10)),'0'),
                   LNFILE.LN_INSPREM_2,
                   LNFILE.LN_INSCOVR_2,
                   LNFILE.LN_INSCOMM_2,
                   LNFILE.LN_INSANTICERN_2,
                   LNFILE.LN_INSANTICADJ_2,
                   LNFILE.LN_INSUFERN_2,
                   LNFILE.LN_INSACCERN_2,
                   LNFILE.LN_INSEARN_ORDER_2,
                   ISNULL(LNFILE.LN_INSFINCHG_2,''),
                   LNFILE.LN_INSCOMP_3,
                   ISNULL(LNFILE.LN_INSOURS_3,''),
                   ISNULL(CAST(LNFILE.LN_INSEFF_DATE_3
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_INSEXP_DATE_3
                                                  AS VARCHAR(10)),'0'),
                   LNFILE.LN_INSPREM_3,
                   LNFILE.LN_INSCOVR_3,
                   LNFILE.LN_INSCOMM_3,
                   LNFILE.LN_INSANTICERN_3,
                   LNFILE.LN_INSANTICADJ_3,
                   LNFILE.LN_INSUFERN_3,
                   LNFILE.LN_INSACCERN_3,
                   LNFILE.LN_INSEARN_ORDER_3,
                   ISNULL(LNFILE.LN_INSFINCHG_3,''),
                   LNFILE.LN_INSCOMP_4,
                   ISNULL(LNFILE.LN_INSOURS_4,''),
                   ISNULL(CAST(LNFILE.LN_INSEFF_DATE_4
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_INSEXP_DATE_4
                                                  AS VARCHAR(10)),'0'),
                   LNFILE.LN_INSPREM_4,
                   LNFILE.LN_INSCOVR_4,
                   LNFILE.LN_INSCOMM_4,
                   LNFILE.LN_INSANTICERN_4,
                   LNFILE.LN_INSANTICADJ_4,
                   LNFILE.LN_INSUFERN_4,
                   LNFILE.LN_INSACCERN_4,
                   LNFILE.LN_INSEARN_ORDER_4,
                   ISNULL(LNFILE.LN_INSFINCHG_4,''),
                   LNFILE.LN_INSCOMP_5,
                   ISNULL(LNFILE.LN_INSOURS_5,''),
                   ISNULL(CAST(LNFILE.LN_INSEFF_DATE_5
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_INSEXP_DATE_5
                                                  AS VARCHAR(10)),'0'),
                   LNFILE.LN_INSPREM_5,
                   LNFILE.LN_INSCOVR_5,
                   LNFILE.LN_INSCOMM_5,
                   LNFILE.LN_INSANTICERN_5,
                   LNFILE.LN_INSANTICADJ_5,
                   LNFILE.LN_INSUFERN_5,
                   LNFILE.LN_INSACCERN_5,
                   LNFILE.LN_INSEARN_ORDER_5,
                   ISNULL(LNFILE.LN_INSFINCHG_5,''),
                   LNFILE.LN_INSCOMP_6,
                   ISNULL(LNFILE.LN_INSOURS_6,''),
                   ISNULL(CAST(LNFILE.LN_INSEFF_DATE_6
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_INSEXP_DATE_6
                                                  AS VARCHAR(10)),'0'),
                   LNFILE.LN_INSPREM_6,
                   LNFILE.LN_INSCOVR_6,
                   LNFILE.LN_INSCOMM_6,
                   LNFILE.LN_INSANTICERN_6,
                   LNFILE.LN_INSANTICADJ_6,
                   LNFILE.LN_INSUFERN_6,
                   LNFILE.LN_INSACCERN_6,
                   LNFILE.LN_INSEARN_ORDER_6,
                   ISNULL(LNFILE.LN_INSFINCHG_6,''),
                   LNFILE.LN_INSCOMP_7,
                   ISNULL(LNFILE.LN_INSOURS_7,''),
                   ISNULL(CAST(LNFILE.LN_INSEFF_DATE_7
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_INSEXP_DATE_7
                                                  AS VARCHAR(10)),'0'),
                   LNFILE.LN_INSPREM_7,
                   LNFILE.LN_INSCOVR_7,
                   LNFILE.LN_INSCOMM_7,
                   LNFILE.LN_INSANTICERN_7,
                   LNFILE.LN_INSANTICADJ_7,
                   LNFILE.LN_INSUFERN_7,
                   LNFILE.LN_INSACCERN_7,
                   LNFILE.LN_INSEARN_ORDER_7,
                   ISNULL(LNFILE.LN_INSFINCHG_7,''),
                   LNFILE.LN_INSCOMP_8,
                   ISNULL(LNFILE.LN_INSOURS_8,''),
                   ISNULL(CAST(LNFILE.LN_INSEFF_DATE_8
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_INSEXP_DATE_8
                                                  AS VARCHAR(10)),'0'),
                   LNFILE.LN_INSPREM_8,
                   LNFILE.LN_INSCOVR_8,
                   LNFILE.LN_INSCOMM_8,
                   LNFILE.LN_INSANTICERN_8,
                   LNFILE.LN_INSANTICADJ_8,
                   LNFILE.LN_INSUFERN_8,
                   LNFILE.LN_INSACCERN_8,
                   LNFILE.LN_INSEARN_ORDER_8,
                   ISNULL(LNFILE.LN_INSFINCHG_8,''),
                   LNFILE.LN_INSCOMP_9,
                   ISNULL(LNFILE.LN_INSOURS_9,''),
                   ISNULL(CAST(LNFILE.LN_INSEFF_DATE_9
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_INSEXP_DATE_9
                                                  AS VARCHAR(10)),'0'),
                   LNFILE.LN_INSPREM_9,
                   LNFILE.LN_INSCOVR_9,
                   LNFILE.LN_INSCOMM_9,
                   LNFILE.LN_INSANTICERN_9,
                   LNFILE.LN_INSANTICADJ_9,
                   LNFILE.LN_INSUFERN_9,
                   LNFILE.LN_INSACCERN_9,
                   LNFILE.LN_INSEARN_ORDER_9,
                   ISNULL(LNFILE.LN_INSFINCHG_9,''),
                   LNFILE.LN_INSCOMP_10,
                   ISNULL(LNFILE.LN_INSOURS_10,''),
                   ISNULL(CAST(LNFILE.LN_INSEFF_DATE_10
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_INSEXP_DATE_10
                                                  AS VARCHAR(10)),'0'),
                   LNFILE.LN_INSPREM_10,
                   LNFILE.LN_INSCOVR_10,
                   LNFILE.LN_INSCOMM_10,
                   LNFILE.LN_INSANTICERN_10,
                   LNFILE.LN_INSANTICADJ_10,
                   LNFILE.LN_INSUFERN_10,
                   LNFILE.LN_INSACCERN_10,
                   LNFILE.LN_INSEARN_ORDER_10,
                   ISNULL(LNFILE.LN_INSFINCHG_10,''),
                   LNFILE.LN_PY_YYYYMM_RECDEL,
                   LNFILE.LN_MTD_PAYS_RECDEL,
                   ISNULL(LNFILE.LN_GARNISH_EXTR,''),
                   ISNULL(LNFILE.LN_SHIP_SAME_FG,''),
                   LNFILE.LN_AD_D_INS_COVR,
                   LNFILE.LN_TOT_PARTIAL_CHGOFF,
                   ISNULL(LNFILE.LN_LPO,''),
                   ISNULL(LNFILE.LN_REP_CD,''),
                   LNFILE.LN_EMP_ID,
                   ISNULL(LNFILE.LN_PROCESS_CD,''),
                   LNFILE.LN_WCBC_CREDITLIM,
                   ISNULL(LNFILE.LN_FOLLOWUP_CD,''),
                   ISNULL(LNFILE.LN_MIP_FG,''),
                   LNFILE.LN_MIP_MTD_CLAHPAID,
                   ISNULL(CAST(LNFILE.LN_MIP_CLPDTH_DATE
                                                  AS VARCHAR(10)),'0'),
                   LNFILE.LN_MIP_TOT_CLPAID,
                   ISNULL(CAST(LNFILE.LN_MIP_AHPDTH_DATE
                                                  AS VARCHAR(10)),'0'),
                   LNFILE.LN_MIP_TOT_AHPAID,
                   LNFILE.LN_IL_GROSS_INCOME,
                   LNFILE.LN_IL_GROSS_INCOME2,
                   ISNULL(LNFILE.LN_RESCISSION,''),
                   ISNULL(LNFILE.LN_INTERIM_TRW,''),
                   LNFILE.LN_DLNO,
                   LNFILE.LN_DLPOOL,
                   ISNULL(LNFILE.LN_DLRECOURSE,''),
                   LNFILE.LN_DLPARTRCAMT,
                   LNFILE.LN_ORIGPURAMT,
                   LNFILE.LN_PURBAL,
                   LNFILE.LN_DLRVPRIN_1,
                   LNFILE.LN_DLRVPRIN_2,
                   LNFILE.LN_DLRVPRIN_3,
                   LNFILE.LN_DLRVCHGS_1,
                   LNFILE.LN_DLRVCHGS_2,
                   LNFILE.LN_DLRVCHGS_3,
                   LNFILE.LN_DLPARVHELD,
                   LNFILE.LN_DLPARVPAID,
                   LNFILE.LN_DLDISC,
                   ISNULL(CAST(LNFILE.LN_LCPDTH_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_INTPDTH_DATE
                                                  AS VARCHAR(10)),'0'),
                   LNFILE.LN_CURBAL,
                   LNFILE.LN_INTBAL,
                   LNFILE.LN_LCBAL,
                   LNFILE.LN_OTHBAL,
                   LNFILE.LN_OT2BAL,
                   LNFILE.LN_LCPAID,
                   LNFILE.LN_LCPARTIALS,
                   LNFILE.LN_STMT_COUNTER,
                   LNFILE.LN_PRIOR_STMT_COUNTER,
                   ISNULL(LNFILE.LN_FROZEN_REASON,''),
                   ISNULL(LNFILE.LN_COUPON_STATEMENT,''),
                   LNFILE.LN_MONTHEND_BAL,
                   LNFILE.LN_ANTICTERM,
                   LNFILE.LN_ANTICERN_1,
                   LNFILE.LN_ANTICADJ_1,
                   LNFILE.LN_UFERN_1,
                   LNFILE.LN_ACCERN_1,
                   LNFILE.LN_EARN_ORDER_1,
                   LNFILE.LN_ANTICERN_2,
                   LNFILE.LN_ANTICADJ_2,
                   LNFILE.LN_UFERN_2,
                   LNFILE.LN_ACCERN_2,
                   LNFILE.LN_EARN_ORDER_2,
                   LNFILE.LN_ANTICERN_3,
                   LNFILE.LN_ANTICADJ_3,
                   LNFILE.LN_UFERN_3,
                   LNFILE.LN_ACCERN_3,
                   LNFILE.LN_EARN_ORDER_3,
                   LNFILE.LN_ANTICERN_4,
                   LNFILE.LN_ANTICADJ_4,
                   LNFILE.LN_UFERN_4,
                   LNFILE.LN_ACCERN_4,
                   LNFILE.LN_EARN_ORDER_4,
                   LNFILE.LN_ANTICERN_5,
                   LNFILE.LN_ANTICADJ_5,
                   LNFILE.LN_UFERN_5,
                   LNFILE.LN_ACCERN_5,
                   LNFILE.LN_EARN_ORDER_5,
                   LNFILE.LN_ANTICERN_6,
                   LNFILE.LN_ANTICADJ_6,
                   LNFILE.LN_UFERN_6,
                   LNFILE.LN_ACCERN_6,
                   LNFILE.LN_EARN_ORDER_6,
                   LNFILE.LN_ANTICERN_7,
                   LNFILE.LN_ANTICADJ_7,
                   LNFILE.LN_UFERN_7,
                   LNFILE.LN_ACCERN_7,
                   LNFILE.LN_EARN_ORDER_7,
                   LNFILE.LN_ANTICERN_8,
                   LNFILE.LN_ANTICADJ_8,
                   LNFILE.LN_UFERN_8,
                   LNFILE.LN_ACCERN_8,
                   LNFILE.LN_EARN_ORDER_8,
                   LNFILE.LN_ANTICERN_9,
                   LNFILE.LN_ANTICADJ_9,
                   LNFILE.LN_UFERN_9,
                   LNFILE.LN_ACCERN_9,
                   LNFILE.LN_EARN_ORDER_9,
                   LNFILE.LN_ANTICERN_10,
                   LNFILE.LN_ANTICADJ_10,
                   LNFILE.LN_UFERN_10,
                   LNFILE.LN_ACCERN_10,
                   LNFILE.LN_EARN_ORDER_10,
                   LNFILE.LN_LMO_PRIOR_INTEREST,
                   LNFILE.LN_PRIOR_UNEARNED,
                   ISNULL(LNFILE.LN_ERNDELQ_STOP,''),
                   LNFILE.LN_NONACRERN,
                   LNFILE.LN_PRIOR_NONACRERN,
                   ISNULL(CAST(LNFILE.LN_ERN_IB_PDTH_DATE
                                                  AS VARCHAR(10)),'0'),
                   LNFILE.LN_ERN_IB_PRINBAL,
                   LNFILE.LN_ERN_IB_INTOWE,
                   LNFILE.LN_TOTERN,
                   LNFILE.LN_TOTOTH,
                   LNFILE.LN_TOTINT,
                   LNFILE.LN_TOTLCHG,
                   LNFILE.LN_TOTDEF,
                   LNFILE.LN_TOTSERACCT,
                   LNFILE.LN_LCWAIVERS,
                   LNFILE.LN_INTWAIVERS,
                   LNFILE.LN_TOTPAYMNTD,
                   LNFILE.LN_TOTEXCPAYMNTD,
                   LNFILE.LN_TOTNODEF,
                   LNFILE.LN_YTDNODEF,
                   LNFILE.LN_TOTNODEFACT,
                   LNFILE.LN_YTDNODEFACT,
                   LNFILE.LN_TOTNOALDEL,
                   LNFILE.LN_YTDNOALDEL,
                   LNFILE.LN_YTDINTPD,
                   LNFILE.LN_LYRINTPD,
                   LNFILE.LN_TOTINTPD,
                   LNFILE.LN_RECASTINT,
                   LNFILE.LN_RECASTPRIN,
                   ISNULL(LNFILE.LN_CL_REBATE,''),
                   ISNULL(LNFILE.LN_AH_REBATE,''),
                   ISNULL(LNFILE.LN_PP_REBATE,''),
                   ISNULL(LNFILE.LN_INT_REBATE,''),
                   ISNULL(LNFILE.LN_SC_REBATE,''),
                   ISNULL(LNFILE.LN_MF_REBATE,''),
                   ISNULL(LNFILE.LN_DF_REBATE,''),
                   ISNULL(LNFILE.LN_EX_REBATE,''),
                   ISNULL(LNFILE.LN_O1_REBATE,''),
                   ISNULL(LNFILE.LN_O2_REBATE,''),
                   ISNULL(LNFILE.LN_O3_REBATE,''),
                   ISNULL(LNFILE.LN_O4_REBATE,''),
                   ISNULL(LNFILE.LN_O5_REBATE,''),
                   ISNULL(LNFILE.LN_O6_REBATE,''),
                   ISNULL(LNFILE.LN_O7_REBATE,''),
                   ISNULL(LNFILE.LN_UN_O7_REB,''),
                   ISNULL(LNFILE.LN_UN_O8_REB,''),
                   ISNULL(LNFILE.LN_UN_O9_REB,''),
                   ISNULL(LNFILE.LN_UN_O10_REB,''),
                   ISNULL(LNFILE.LN_PLREASON,''),
                   ISNULL(LNFILE.LN_COLLECTOR,''),
                   ISNULL(LNFILE.LN_ACTIONCD,''),
                   ISNULL(LNFILE.LN_ACTIONCD2,''),
                   ISNULL(LNFILE.LN_ATCODE,''),
                   ISNULL(LNFILE.LN_RECODE,''),
                   ISNULL(LNFILE.LN_ESCROW_FG,''),
                   LNFILE.LN_PROMDAYS_PASTDUE,
                   ISNULL(LNFILE.LN_PROMID,''),
                   ISNULL(CAST(LNFILE.LN_PROMDATE AS VARCHAR(10)),'0'),
                   LNFILE.LN_PROMAMT,
                   ISNULL(CAST(LNFILE.LN_PROMDATE2
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_DATE_PAID_LAST
                                                  AS VARCHAR(10)),'0'),
                   LNFILE.LN_AMT_PAID_LAST,
                   LNFILE.LN_LASTCFORMNO,
                   ISNULL(CAST(LNFILE.LN_LASTCFORMDATE
                                                  AS VARCHAR(10)),'0'),
                   LNFILE.LN_DELCNT,
                   LNFILE.LN_DELCNT2,
                   LNFILE.LN_CREDITLIM,
                   LNFILE.LN_CRSCORE,
                   LNFILE.LN_LASTSFORMNO,
                   ISNULL(CAST(LNFILE.LN_LASTSFORMDATE
                                                  AS VARCHAR(10)),'0'),
                   LNFILE.LN_SOLCNT,
                   ISNULL(LNFILE.LN_MESSAGE,''),
                   LNFILE.LN_OLDACCTNO,
                   LNFILE.LN_PRLNNO,
                   LNFILE.LN_PRBAL,
                   LNFILE.LN_PRINTDUE,
                   LNFILE.LN_PRFINCHGRB,
                   LNFILE.LN_PRINSRB,
                   ISNULL(LNFILE.LN_PRPOCD,''),
                   ISNULL(CAST(LNFILE.LN_PRPODATE AS VARCHAR(10)),'0'),
                   LNFILE.LN_PRDELINQAMT,
                   LNFILE.LN_PRLNAMT,
                   LNFILE.LN_PRPDUE_DAYS,
                   LNFILE.LN_PRDLDISC_UNEARN,
                   LNFILE.LN_PRCONPROFILE,
                   LNFILE.LN_SUM_ORIGPURAMT,
                   LNFILE.LN_SUM_LNAMT,
                   LNFILE.LN_SUM_FINCHG,
                   LNFILE.LN_SUM_INTCHG,
                   LNFILE.LN_SUM_ORGTERM,
                   LNFILE.LN_SUM_REGPYAMT,
                   LNFILE.LN_SUM_EXTCHG,
                   LNFILE.LN_SUM_SERCHG,
                   LNFILE.LN_SUM_MAINTFEE,
                   ISNULL(LNFILE.LN_UNITPER_CD,''),
                   LNFILE.LN_UNITPER_FREQ,
                   LNFILE.LN_DAYS_IN_UNITPER,
                   LNFILE.LN_UNITPER_PER_YEAR,
                   ISNULL(LNFILE.LN_REPOCD,''),
                   ISNULL(LNFILE.LN_BR_TFR_FG,''),
                   LNFILE.LN_O3INS_DLPROC,
                   LNFILE.LN_INSPREM_DLADJ,
                   LNFILE.LN_PLAMT,
                   LNFILE.LN_MTD_PRIN,
                   LNFILE.LN_MTD_LC,
                   LNFILE.LN_MTD_INT,
                   LNFILE.LN_MTD_OT,
                   LNFILE.LN_MTD_EX,
                   LNFILE.LN_BNKRPT_CHAPTER,
                   ISNULL(LNFILE.LN_BMO_CON_STATUS,''),
                   ISNULL(LNFILE.LN_BMO_REC_STATUS,''),
                   LNFILE.LN_APP_NUMBER,
                   ISNULL(LNFILE.LN_GIFTNET,''),
                   LNFILE.LN_GIFTNET_OUR_PO,
                   ISNULL(LNFILE.LN_PROC_DISBURSED,''),
                   ISNULL(LNFILE.LN_NOODDPY,''),
                   LNFILE.LN_CONPROFILE_1,
                   LNFILE.LN_CONPROFILE_2,
                   LNFILE.LN_CONPROFILE_3,
                   LNFILE.LN_CONPROFILE_4,
                   LNFILE.LN_CONPROFILE_5,
                   LNFILE.LN_RECPROFILE_1,
                   LNFILE.LN_RECPROFILE_2,
                   LNFILE.LN_RECPROFILE_3,
                   LNFILE.LN_RECPROFILE_4,
                   LNFILE.LN_RECPROFILE_5,
                   ISNULL(LNFILE.LN_TRW_STATUS,''),
                   ISNULL(LNFILE.LN_ORIGIN,''),
                   ISNULL(LNFILE.LN_PO_TRCD,''),
                   LNFILE.LN_NEXIS_CRSCORE,
                   ISNULL(LNFILE.LN_TRW_B_CONSUMER_IND,''),
                   ISNULL(LNFILE.LN_PARVHELD_PAID_FG,''),
                   ISNULL(LNFILE.LN_ACCT_FROZEN,''),
                   ISNULL(LNFILE.LN_TRW_SPECIAL_CODE,''),
                   ISNULL(LNFILE.LN_CREATION_STATUS,''),
                   LNFILE.LN_TOTNOLCHG,
                   ISNULL(LNFILE.LN_LBOX,''),
                   LNFILE.LN_EXTRA_PRIN_LFP,
                   LNFILE.LN_REMAIN_INT_SINCE_LFP,
                   LNFILE.LN_REMAIN_PRIN_SINCE_LFP,
                   ISNULL(LNFILE.LN_INTERIM_COUPON,''),
                   LNFILE.LN_DLCOMM_O3_CMSI,
                   LNFILE.LN_DLCOMM_PP_CMSI,
                   ISNULL(LNFILE.LN_TRW_J_CONSUMER_IND,''),
                   ISNULL(LNFILE.LN_ACH,''),
                   ISNULL(LNFILE.LN_1099_C_FG,''),
                   ISNULL(LNFILE.LN_TRW_COMPLIANCE_CODE,''),
                   ISNULL(LNFILE.LN_UNWIND_REASON_CODE,''),
                   ISNULL(LNFILE.LN_PBI_PBD_FG,''),
                   ISNULL(LNFILE.LN_OLD_ACCT_FORMAT_FG,''),
                   ISNULL(LNFILE.LN_APPROVED_BY_USER,''),
                   ISNULL(LNFILE.LN_APPROVED_BY_CODE,''),
                   ISNULL(CAST(LNFILE.LN_APPROVED_BY_DATE
                                                  AS VARCHAR(10)),'0'),
                   LNFILE.LN_APPROVED_BY_TIME,
                   ISNULL(CAST(LNFILE.LN_LTOUCH_DATE
                                                  AS VARCHAR(10)),'0'),
                   LNFILE.LN_LTOUCH_TIME
            INTO :QLN-REC
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

          EXEC SQL SET CONNECTION C1 END-EXEC.

      *-----------------------------------------------------------------
       READ-LN3-FILE-NEXT.
           PERFORM READ-IT.
           MOVE LN-PATH-OWNBR  TO LN-OWNBR.
           MOVE LN-SELECT-PATH TO E-FILE.
           MOVE SPACES         TO E-KEYX.
           STRING LN-OWNBR, LN-CLASS,LN-ACCTNO
              DELIMITED BY " " INTO E-KEYX.
           INITIALIZE QLN-REC.

           EXEC SQL SET CONNECTION LN3_CONNECT END-EXEC.

              EXEC SQL
                   FETCH NEXT LN3_CURSOR INTO
                        :QLN-OWNBR                              
                       ,:QLN-CLASS
                       ,:QLN-SUBCLASS
                       ,:QLN-ACCTNO
                       ,:QLN-SSNO1
                       ,:QLN-POCD
                       ,:QLN-DLNO
                       ,:QLN-DLPOOL
                       ,:QLN-SUM-LNAMT
                       ,:QLN-COLLECTOR
                       ,:QLN-ACTIONCD
                       ,:QLN-SERVACCT
                       ,:QLN-PLDATE 
                       ,:QLN-PLCD
                       ,:QLN-PLTYPE
                       ,:QLN-SALEFIN
                       ,:QLN-DLRECOURSE
                       ,:QLN-JDDATE
                       ,:QLN-REPOCD
                       ,:QLN-BNKRPTDATE
                       ,:QLN-LEGALDATE 
                       ,:QLN-DEBTAIDATE
                       ,:QLN-ACCT-FROZEN
                       ,:QLN-CURBAL 
                       ,:QLN-INTBAL 
                       ,:QLN-LCBAL
                       ,:QLN-OTHBAL
                       ,:QLN-MIDSTREAM
                       ,:QLN-DATE-PAID-LAST
                       ,:QLN-ORIGPURAMT
                       ,:QLN-CONFIDENTCD
                       ,:QLN-ORGST
                       ,:QLN-SPRCLASS
                       ,:QLN-LAWCODE
                       ,:QLN-1STPYDATE
                       ,:QLN-1STPYAMT
                       ,:QLN-REGPYAMT
                       ,:QLN-LASTPYAMT
                       ,:QLN-TOTPAYMNTD
                       ,:QLN-PAY-SCHLD-FG
                       ,:QLN-TOTNODEF
                       ,:QLN-TOTNOALDEL
                       ,:QLN-UNITPER-CD
                       ,:QLN-UNITPER-FREQ
                       ,:QLN-NOREGPY
                       ,:QLN-MAKERCD1
                       ,:QLN-ACCELDATE
                       ,:QLN-TOTLCHG
                       ,:QLN-LOANTYPE
                       ,:QLN-INTBAL
                       ,:QLN-REPOCD
                       ,:QLN-PROMDATE 
                       ,:QLN-CREDITLIM
                       ,:QLN-BNKRPTDATE
                       ,:QLN-TOTNOALDEL
                       ,:QLN-LOANDATE 
                       ,:QLN-MTD-PAYS-RECDEL 
                       ,:QLN-PY-YYYYMM-RECDEL                 
                       ,:QBW-OWNBR
                       ,:QBW-SSNO
                       ,:QBW-LNAME
                       ,:QBW-FNAME
                       ,:QBW-ADR1
                       ,:QBW-ADR2
                       ,:QBW-CITY
                       ,:QBW-STATE
                       ,:QBW-ZIP
                       ,:QBW-PHONE
                       ,:QBW-CELL-PHONE
                       ,:QBW-EMPLOYER
                       ,:QBW-EMPLPOS
                       ,:QBW-EMPPHONE
                       ,:QBW-EMPEXT
                       ,:QBW-LOANS
                       ,:QBW-SPSSNO
                       ,:QBW-LAST-RN-FOR10-DATE
                       ,:QBW-SOLICIT
                       ,:QBW-CONFIDENTIAL
                       ,:QLC-CODE 
                       ,:QLC-SMALLOAN
                       ,:QLC-SALEFIN
                       ,:QLC-GIFTNET			   
              END-EXEC.


      *    IF ( LN3-CURSOR-STAT-GOOD )
      *       EXEC SQL
      *            FETCH LN3_CURSOR INTO :QLN-REC
      *       END-EXEC
      *    ELSE
      *    IF ( LN3-CURSOR-GR-STAT-GOOD )
      *       EXEC SQL
      *            FETCH LN3_CURSOR_GR INTO :QLN-REC
      *       END-EXEC
      *    ELSE
      *       MOVE 9        TO SQLCODE
      *       MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-LN3-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-LN-FIELDS
			  PERFORM GET-BW-FIELDS
			  PERFORM GET-LC-FIELDS.

          EXEC SQL SET CONNECTION C1 END-EXEC.

      *-----------------------------------------------------------------
       CLOSE-LN3-FILE.

           PERFORM CLOSE-IT.
           IF ( LN3-CURSOR-STAT-GOOD )
              EXEC SQL SET CONNECTION LN3_CONNECT END-EXEC. 
              EXEC SQL
                   CLOSE LN3_CURSOR
              END-EXEC
              MOVE STAT---BAD TO LN3-CURSOR-STAT.

           EXEC SQL SET CONNECTION C1 END-EXEC.

      *    IF ( LN3-CURSOR-GR-STAT-GOOD )
      *       EXEC SQL
      *            CLOSE LN3_CURSOR_GR
      *       END-EXEC
      *       MOVE STAT---BAD TO LN3-CURSOR-GR-STAT.	   
	   	  
	
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
