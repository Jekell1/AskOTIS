       IDENTIFICATION DIVISION.
       PROGRAM-ID.       MBRUN.
       DATE-WRITTEN.   4/16/96.
      **************************************************************
      *    DIR :  LP
      *    DESC:  COLLECTOR QUEUE EXTRACTION RUN
      *
      * REV :
      *  042296 BLL TEST RANGES FOR EA-PL, EA-REPO, EA-BANKRUPT
      *  060496 BLL FIX EA GROUP SCAN; WASN'T FINING RECORDS
      *  062496 KEC ADDED CHECKS FOR EA-PL-IN-ACTIVE-BOTH.
      *  071696 BAH ADDED TEST OF GP-ALLOW-MORE-COLLECT TO ALLOW MORE THAN
      *             ONE COLLECTION TYPE FORM REQUEST, PR # E128
      *  MJD 071696 ADDED LIBLP/LPUPWK FOR CHANGES IN LIBLP/AGEING
      *  BLL 080696 FIXED TO WORK IN BATCH
      *  KEC 111496 CHANGED LN-POCD FOR RESCISSION.
      *  BLV 120596 PURGE RESCHEDULED RECS OLDER THAN EXTRACTION DATE
      *  BLV 012097 FIXED BUG-WAS PURGING ALL RESCHEDULES REGARDLESS
      *             OF FLAG
      *  CLS 031097 ADDED TEST OF EA-CONFIDENTIAL-FG,EA-1ST-PMT-MISSES
      *  BLV 031097 FIX EA GROUP SCAN
      *  BLV 041497 CHECK BR-COL-QUEUE-LOW-TO-HI TO PUT QUEUE IN ORDER
      *             OF DELINQUENCY INSTEAD OF REVERSE DELINQUENCY
      *  GLF 050797 ADDED COPY MEMBERS: SETCTBLW AND SETCTBL
      *  MJD 051597 ADDED LPPOF2 COPY MEMBER (PREVIOUSLY WAS PART OF LPPOFF)
      *  JTG 052097 ADDED 'LIBLP/ADDONSPR' TO GET CORRECT SPR RE: INS RB
      *  BLV 070197 OPTION TO BREAK OUT BROKEN PROMISES WITH PAST
      *             PROMISE RANGE & THEY WILL APPEAR 1ST IN QUEUE
      *   CS 070197 ADDED EA-LEGAL,EA-DEBTAID,EA-JUDGEMENT
      *  BLV 072197 CHANGE CQ2-KEY, ADD CQ-SEQ-FG, ALWAYS FILL IN
      *             CQ-DATE, OPTION TO KEEP UNWORKED ACCOUNTS IN QUEUE
      *  JTG 072897 ADDED LPWDEL/W  MERCURY #E135
      *  BLV 081197 WAS MISSING REMOVE RESCHEDULE & UNWORKED INFO
      *             IN DISPLAY PARAMETERS
      *  BLV 081997 FIXED CLEAR ON CP-FILE
      *  BLV 110597 CHECK BR-MIDSTREAM-1STPAY-FG TO DETERMINE
      *             1ST PAY MISSES FOR MIDSTREAM ACCOUNTS
      *  JTG 111797 ADDED LOGIC FOR LIBLP/DEFPOL RE: BR-NODEF-1ST-X-MONTHS #167
      *  BLV 010798 ALLOW BRANCH RANGE OR GROUP
      *  GLF 012298 REMOVED GLOBAL FILE AND RELACED 'IO' ON GP WITH GET GPENV
      *  GLF 022098 DELETED BR-SECURITY-OK AND ADDED LIBGB/BRSECUREW.
      *   CS 022798 ADDED READ OF EA-FILE BEFORE PROCESSING BRANCH SO WE
      *             ARE ONLY PROCESSING BRANCHES THAT MEET THE GROUP ID.
      *  BAH 041598 COMMENTED OUT CS CHANGE FROM 022798
      *   CS 042198 ADDED TEST OF EA-ORIGIN
      *  BLV 082498 ADDED QUEUE KEY SEQUENCING FOR MISPAYS
      *  BLV 990630 TFC PR#35 - IF NON-MONTHLY POT30 & I-AM-TFC, MOVE 0 TO
      *             PAST DUE DAYS IN CQ KEY- THESE WILL ORDER BY DESCENDING BAL
      *  GLF 990924 REPLACED IO-FG NUMBERS WITH IO-FG 88 LEVEL PER DEVELOPMENT
      *             MEETING (082599)
      * KEC 2000.0427 {PR#01138} REGIONAL ACCEPTANCE; KEVIN MULLEN
      *                ADDED EA-PL-BEG-DATE AND EA-PL-END-DATE
      *  BLV 000817 ADDED LOGIC FOR QUEUING BY TIME ZONE, TFC PR #25
      *  BLV 000817 TWEAK LOGIC FOR QUEUING BY TIME ZONE, ADD UPDATE OF
      *             CQ-TIME-ZONE-ERROR, TFC PR #25
      *  BLV 001020 TEST FOR SKIP, INCLUDE, ONLY OF RECENCY POTS, MISPAYS,
      *             (EA-RPOTS, EA-MISPAYS), WORLD PR #244
      *  BLV 001020 IF I-AM-WORLD, SEQUENCE QUEUE BY RENEWABILITY BY HIGHEST
      *             AVAILABLE CREDIT
      *  BLV 001127 ADD GQFILE, GLOBAL QUEUING, LPCPUPD MAY UPDATE GQFILE,
      *             TFC PR# 60
      *  BLV 001208 IF GLOBAL QUEUING, SET UP COLLECTOR POOL GLOBAL PATH
      *             INSTEAD OF LOCAL (CP-ALL-PATH), TFC PR# 60
      *  BLV 010102 STARRED OFF THE REWRITE OF THE LOAN FILE - I THINK IT MUST
      *             HAVE BEEN LEFT OVER FROM CPRUN - I CAN'T FIND ANYTHING
      *             HERE THAT IS BEING UPDATED TO THE LOAN FILE
      *  BLV 010105 ADDED ROUTINE TO INITIALIZATION TO PURGE GQFILE IF DOING
      *             GLOBAL QUEUING
      *  BLV 010209 ALTERED TFC PR#35 - IT WAS IF NON-MONTHLY POT30 & I-AM-TFC,       *             MOVE 0 TO PAST DUE DAYS IN CQ KEY- THESE WILL ORDER BY
      *             DESCENDING BAL, CHANGED TO NOT CHECK NON-MONTHLY, DO FOR
      *             ALL POTS FOR TFC, PER JUDY & TRACY AT TFC
      *  BLV 010418 FIX PROBLEM IN INITIALIZING GQ-KEY IN PURGE-GQ-RECORDS
      *             ROUTINE, WAS LOCKING UP TESTING AT TFC
      *   CS 010507 ADDED TEST FOR ALLOTMENTS, ACE PR# 1472
      *  BLV 011011 ADD TEST ON BEG & ENDING DAYS SINCE LOAN DATE, BEG & ENDING
      *             DAYS UNTIL MATURITY, REGWAN PR# 1587
      *  BLV 011210 IF BR-COL-QUEUE-PAST-DUE = "Y", ORDER QUEUE BY
      *             PAST DUE DAYS AFTER AGEING CATEGORY, REGACC PR# 1677
      *  BAH 020516 ADDED TEST OF FD-SMARTPORT = "E" FOR CREATING ALL FORM
      *             REQUESTS IN THE CORPORATE BRNO AND THEN THEY WILL PRINT
      *             THEM INTO AN ASCII FILE EXTRACT, REGACC PR# 1800(LPCPUPD)
      *  BLV 020821 CHANGED TO ONLY OPEN LNFILE TO INPUT, & FIX LOCKING
      *             PROBLEMS WITH CP-FILE, PROBLEMS AT TFC
      *  BLV 021030 CHECK DAY OF MONTH DUE DAY RANGE, REGACC PR# 0009,
      *             WILL IGNORE THESE PARAMETERS IF THEY = 20, 20...EVEN
      *             THOUGH THAT COULD BE A VALID RANGE
      *  BLV 030403 TFC USES LAST DAY OF MONTH FOR EXTRACTIONS, SOME-
      *             TO DO WITH THEIR AGEING, SO IT MESSES UP DAYLIGHT
      *             SAVINGS START/STOP, JUST USE SYSTEM DATE FOR
      *             DAYLIGHT SAVINGS ROUTINE IF I-AM-TFC
      *  BLV 030630 IF REGACC, USE LNDATE-DAYS & MATURE DAYS PARAMETERS FOR
      *             TRIAD RANGES, RAW SCORE & DELQ SEEN ID, REGACC PR# 0033
      *  BAH 040812 ADDED TEST OF BW-CONFIDENTIAL, THIS FLAG OVERRIDES THE
      *             LOAN FLAG, IF THE LOAN IS "N", BUT THE BW IS "Y", THEN
      *             ITS CONFIDENTIAL ! PR# 2343
      *  BLF 050418 IF I-AM-WAGNER & BR-RECDEL-NOT-REC = Y, USE CALENDAR
      *             RECENCY, WAGNER PL# 478
      *   CS 050502 USING CALANDAR RECENCY, 1ST PAY DEFAULTS WERE BEING SKIPPED
      *             WAS: IF LN-DATE-PAID-LAST = 0, IF RECENCY 0 GO TO EXIT
      *             USING TRUE DAYS, RECENCY OF 0 IS TODAY... USING CALANDAR
      *             IT MANY DAYS, CHANGED TO IGNORE THE TEST ON LN-DATE-PAID-
      *             LAST IF BR-RECDEL-NOT-REC = Y AND GP-I-AM-WAGNER  [PR#478]
      *  BLF 050512 IF I-AM-WAGNER & BR-RECDEL-NOT-REC = Y, USE CALENDAR
      *             RECENCY, FIX IN RECENCY POTS SECTION, WAGER PR# 478
      *  BAH 050721 ADDED LITTLE-Z TEST AND ALPNAME2 = "~~~" FOR WORLD MEXICO
      *             PR# 416
      *  BLF 060228 ADDED TEST ON EA-INCL-COLLECTOR, LENDMARK PR# 2719
      *  KEC 070202 {PR#00000} REGIONAL ACCEPTANCE <A15> [K.MULLEN]
      *             ADDED GETTING BX-REC FOR TRIAD.
      *             ADDED BR-REC FOR BETH.
      * BLF 070906  IF BR-COLL-QUEUE-CON-REC = "N", MOVE 0 TO CQ- OR GQ-REV-DEL,
      *             SO NO DELINQUENCY IN QUEUE ORDERING (CHANGE TO
      *             LIBLP/LPCPUPD), PCCREDIT PR# 3054
      * BLM 081009  UPDATE QUEUE GROUP ID TO HEADER REC IN GQFILE, GQ-GROUP-ID,
      *             REGACC PR# 220
      * BAH 091002 ADDED FD-SKIP-BAD-ADR TO TEST PRIMARY MAKER ONLY IF BAD
      *            ADDRESS FLAG SET, DO NOT CREATE FORM REQUEST REGACC PR# 256
      *            CHANGE IS IN LPCPUPD
      * BLM 100304 IF I-AM-UNITED & CELL-PHONE NOT 2020, INCLUDE CELL PHONE
      *            WHEN CHECKING TIME ZONES FOR TIME ZONE QUEUING,
      *            UNITED PR# 3474
      *  CS 110228 ADD FD-SKIP-BAD-ADR = 'M' CHANGE IN LIBLP/LPCPUPD
      *            ADDED S1-SUBSCRIPT TO THIS PROGRAM.  MODERN PR#3720
      * BLM 120626 NOTE:  INVESTIGATED WORLD GETTING WRITE ERRORS OF 22 ON
      *            CQFILE.  AT FIRST I THOUGHT THIS WAS CREATE-CQ-HEADERS
      *            WHICH WRITES CQ IF COLL ID CHANGES OR IF IT HITS END-OF-
      *            FILE.  I THOUGHT IF THERE WAS ONLY 1 RECORD FOR A COLL
      *            ID & IT WAS LAST, THIS COULD CAUSE A 22, BUT I COULDN'T
      *            DUPLICATE IT.  AND THE EXAMPLES AT WORLD DIDN'T MATCH
      *            THIS THEORY.  I WAS ABLE TO DUPLICATE THE 22 BY RUNNING
      *            THE QUEUE CREATE AT THE SAME TIME ON 2 DIFFERENT TERMINALS.
      *            PUT ONE IN DEBUG & THE OTHER GOT A BUNCH OF RECORD LOCKED
      *            BY ANOTHER USER AS WELL AS THE 22.  SO I GUESS THEY
      *            SHOULDN'T RUN THIS AT THE SAME TIME.
      * BLM 130618 IF I'M A POT & GP-I-AM-GLFIN, MOVE 0 TO AW-REV-PDUE-DAYS
      *            SO POTS WILL ORDER BY BALANCE, NOT PAST DUE DAYS FOR
      *            GL FINANCIAL, GL FINANCIAL PR# 4308
      *  CS 140910 SET CP-PATH FOR GLOBAL QUEUEING, MOVE EXT-FILPATH-BASE,
      *            EXT-FILPATH-MACHINE TO CP-GLOBAL-BASE,CP-GLOBAL-MACHINE,
      *            CP-PATH-OVERRIDE TO "Y" IF GLOBAL QUEUEING.
      *
      * KEC 2016.0121 {PD#00003} PARADATA <A30> [JAY CISZEWSKI]
      *         FIXED KEYS & RECS FOR FD-REC.
      *         ADDED NEW FDB-FILE; WAS IN FD-FILE.
      *         REMOVED FT-FILE; MERGED INTO FD-FILE.
      *
      * BAH 160125 SPLIT OUT LTFILE, PD0003
      *
      * KEC 2016.0217 {PD#00003} PARADATA <A30> [JAY CISZEWSKI]
      *         FIXED KEYS & RECS FOR FD-REC.
      *         CHANGED FMNO/FORMNO/NEXTFM/SETFM FROM 3 TO 4 BYTES.
      *
      * KEC 2016.0301 {PD#00003} PARADATA <A30> [JAY CISZEWSKI]
      *         CHANGED EA-ALPNAME1 & EA-ALPNAME2 3 TO 4 BYTES.
      *
      * KEC 2016.0310 {PD#00003} PARADATA <A30> [JAY CISZEWSKI]
      *         ADDED CD-EA-FILLER FOR CHANGE TO CD-EA-REC.
      * KEC 2016.0324 {PD#00003} PARADATA <A30> [JAY CISZEWSKI]
      *         CHANGED CDSCAN-WINDOW-BUF TO HAVE MATCHING CD1-KEY
      *         LAYOUTS (FOR EACH CODE TYPE; CD-TYPE).
      *         CDSCAN-CD1-KEY IS THE NEW (PRIMARY) WITH REDEFINES OF
      *         CDSCAN-??-KEY (NEW).
      * BLM 160915 FIX CQ-DATE, GQ-DATE, CQ-SEQ, PD0003
      *
      * KEC 2016.1005 {PD#00003} PARADATA <A30> [JAY CISZEWSKI]
      *         ADDED SETTING CQ-BRNO.
      *
      * BAH 170525 ACTIVATE 8 DIGIT DATES, PD0006
      * BAH 180313 BR-01 WAS RESETTING FIELD, NEEDED TO KEEP ENTRY FOR
      *            BATCH, CHANGED TO "R  A000BR1.".
      * BAH 180314 WAS MISSING THE FOLLOWING CHANGE FROM BLM DATED 131030;
      *            IF I-AM-REGACC, USE 2 CHARACTERS OF EA-ORIGIN FIELD TO
      *            TEST AGAINST REPO DISPOSITION (LS-Y-DISPOSAL-HOLD)
      *            INSTEAD OF ORIGIN, REGACC PR# 344
      * BAH 181003 GLOBAL BWFILE, L2G
      * KEC 2018-1026 CONVERTED CPFILE FOR A32 & A34; L2G & G2G
      *               CP-PATH-OVERRIDE TO "G" IF GLOBAL QUEUEING.
      * BAH 181205 GLOBAL LNFILE, L2G
      * BAH 190122 SPLIT CDFILE, G2G
      *  CS 190130 GLOBAL CQFILE,L2G
      *            SEE SPCQ2RN/SPCQ2IN FOR CQ2-KEY
      *  CS 190301 MADE SEVERAL CHANGES, L2G, NEEDED SAVE-COLLID 
      *            REPLACED MOVE OF CQ2-KEY TO HOLD-CQ2 WITH SAVE-CQ2-KEY
      *            AND MOVE OF HOLD-CQ2-KEY TO CQ2-KEY WITH LOAD-CQ2-KEY
      *            TURNS OUT WE DID NEED TO RESTART CQ
      * BAH 190311 MOVED FM-BRNO2 INTO LPWSFM-EDIT-FM-INITKEY TO REMOVE THE
      *            REDEFINE IN THE FM-REC
      *  CS 190314 SPLIT OUT THE CQFILE INTO DETAIL(CQ-FILE) AND SUMMARY,
      *            (PREVIOUS CALLED HEADER) AND THE GQFILE INTO GQFILE
      *            (DETAIL) AND HEADER (GQSFILE)
      *  190327 CS GQS-BRNO IS ALWAYS POPULATED WITH ZERO, PER KEN REMOVE
      *            FROM FILE LAYOUT
      *  190329 CS IN THE MIDDLE OF BRANCH LOGIC THERE WAS AS IF IF GLOBAL
      *            QUEUING, PURGE GQS. THE PURGE OF GQS NEEDS TO HAPPEN
      *            IN INITIALIZATION
      * BAH 20190705 REMOVED LRR AND LRB PT0151
      * BAH 20190920 REMOVED ACCESS-CALL ON LNFILE
      * BAH 20200709 MOVED OPENS FROM LPCPUPD TO HERE #TST001
      *  CS 20210506 REMOVE KEEP-CQ-FUTURE-KEY1,KEY2
      * BAH 20220818 REMOVED GLOBRD, READ GB-FILE INSTEAD
      * BAH 20220907 HARD CODED START
      * BAH 20221129 NEED DEFINITION OF RECENCY TO BE RECDEL BASED ON WAC'S
      *              RECDEL METHOD "D", NOT STANDARD EXACT DAYS SINCE LAST
      *              TRANSACTION.  MOVE RECDEL TO RECENCY #1580
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "LIBGB/GBFSPR.CPY".
      *COPY "LIBLP/LPFSAW.CPY".
       I-O-CONTROL.
       DATA DIVISION.
       FILE SECTION.
       COPY "LIBGB/GBFDPR.CPY".
      *COPY "LIBLP/LPFDAW.CPY".

       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(36) VALUE
           "@(#)UT/MBRUN S35 07/07/23-07:55:34 >".
      ****************************
      *     F I L E   I F N      *
      ****************************
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBGB/GB01GB.CPY".
       COPY "LIBGB/GB01GB_SQL.CPY".
       COPY "LIBGB/GBWSGB.CPY".
       COPY "LIBGB/GB01GR.CPY".
       COPY "LIBGB/GB01GR_SQL.CPY".
       COPY "LIBGB/GBWSGR.CPY".
       COPY "LIBLP/LP01LN.CPY".
       COPY "LIBLP/LP01LN_SQL.CPY".
       COPY "LIBLP/LPWSLN.CPY".
       COPY "LIBLP/LP01EA.CPY".
       COPY "LIBLP/LP01EA_SQL.CPY".
       COPY "LIBLP/LPWSEA.CPY".
      *COPY "LIBLP/LPWSAW.CPY".
       COPY "LIBLP/LP01SP.CPY".
       COPY "LIBLP/LP01SPA.CPY".
       COPY "LIBLP/LP01SPB.CPY".
       COPY "LIBLP/LP01SPC.CPY".
       COPY "LIBLP/LP01SP_SQL.CPY".
       COPY "LIBLP/LPWSSP.CPY".
       COPY "LIBLP/LP01BW.CPY".
       COPY "LIBLP/LP01BW_SQL.CPY".
       COPY "LIBLP/LPWSBW.CPY".
       COPY "LIBLP/LP01FD.CPY".
       COPY "LIBLP/LP01FD_SQL.CPY".
       COPY "LIBLP/LPWSFD.CPY".
       COPY "LIBLP/LP01LTI.CPY".
       COPY "LIBLP/LP01LTI_SQL.CPY".
       COPY "LIBLP/LPWSLTI.CPY".
       COPY "LIBLP/LP01LTP.CPY".
       COPY "LIBLP/LP01LTP_SQL.CPY".
       COPY "LIBLP/LPWSLTP.CPY".
       COPY "LIBSP/SP01CQS.CPY".
       COPY "LIBSP/SP01CQS_SQL.CPY".
       COPY "LIBSP/SPWSCQS.CPY".
       COPY "LIBSP/SP01CQ.CPY".
       COPY "LIBSP/SP01CQ_SQL.CPY".
       COPY "LIBSP/SPWSCQ.CPY".
       COPY "LIBSP/SP01GQ.CPY".
       COPY "LIBSP/SP01GQ_SQL.CPY".
       COPY "LIBSP/SPWSGQ.CPY".
       COPY "LIBSP/SP01GQS.CPY".
       COPY "LIBSP/SP01GQS_SQL.CPY".
       COPY "LIBSP/SPWSGQS.CPY".
       COPY "LIBSP/SP01CP.CPY".
       COPY "LIBSP/SP01CP_SQL.CPY".
       COPY "LIBSP/SPWSCP.CPY".
       COPY "LIBLP/LP01CD.CPY".
       COPY "LIBLP/LP01CD_SQL.CPY".
       COPY "LIBLP/LPWSCD.CPY".
       COPY "LIBLP/LP01FM.CPY".
       COPY "LIBLP/LP01FM_SQL.CPY".
       COPY "LIBLP/LPWSFM.CPY".
       COPY "LIBLP/LP01LP.CPY".
       COPY "LIBLP/LP01LP_SQL.CPY".
       COPY "LIBLP/LPWSLP.CPY".
       COPY "LIBLP/LP01NO.CPY".
       COPY "LIBLP/LP01NO_SQL.CPY".
       COPY "LIBLP/LPWSNO.CPY".
       COPY "LIBLP/LP01TZ.CPY".
       COPY "LIBLP/LP01TZ_SQL.CPY".
       COPY "LIBLP/LPWSTZ.CPY".
       01  PR-IFN               PIC X(2)      VALUE "PR".

       01  HOLD-BR-KEY          PIC 9(4)      VALUE 0.
       01  HOLD-FM1-KEY         PIC X(28)     VALUE SPACES.
       01  HOLD-BW-SSNO         PIC 9(9)      COMP-3 VALUE 0.

       01  HOLD-PHONE        PIC 9(10).
       01  HOLD-PHONEX REDEFINES HOLD-PHONE.
           03  HOLD-AC       PIC 9(3).
           03  HOLD-PREFIX   PIC 9(3).
           03  HOLD-4567     PIC 9(4).


       01  SAVE-CQ-SEQ                   PIC 9(3) VALUE 0.
       01  SAVE-GQ-SEQ                   PIC 9(3) VALUE 0.
       01  FIRST-TIME-CQ        PIC X         VALUE "Y".
       01  GQ-FIRST-TIME        PIC X         VALUE "Y".
       01  TOTAL-CQS-RECS       PIC 9(5)      VALUE 0.
       01  TOTAL-GQS-RECS       PIC 9(5)      VALUE 0.       
       01  TOTAL-CQS-TIMED      PIC 9(5)      VALUE 0.
       01  TOTAL-GQS-TIMED      PIC 9(5)      VALUE 0.
       01  SAVE-COLLID          PIC XXX.
       01  COL-COUNT            PIC 99        VALUE 0.
       01  LITTLE-ZX.
           03  FILLER           PIC X VALUE X"7A".
           03  FILLER           PIC X VALUE X"7A".
           03  FILLER           PIC X VALUE X"7A".
           03  FILLER           PIC X VALUE X"7A".
       01  LITTLE-Z  REDEFINES LITTLE-ZX PIC X(4).
       01  SUB                  PIC 9(6)      COMP-3 VALUE 0.
       01  SUB1                 PIC 9(6)      COMP-3 VALUE 0.
       01  LN-ACCUM-BAL         PIC S9(7)V99  COMP-3 VALUE 0.
       01  WORK-LNAME           PIC XXXX.
       01  HOLD-LOAN-DATE       PIC 9(8)      COMP-3 VALUE 0.

       01  WK-RENEWABLE-PAYS    PIC 9(4).
       01  WK-RENEWABLE-DAYS    PIC 9(4).

       01  RTN-FG               PIC XXX.
       01  UPD-FLAG             PIC X         VALUE "N".
       01  GLOBAL-QUEUING-FG    PIC X         VALUE "N".

       01  DATE-TABLE3.
           03  X-CONTRACT       PIC X.
           03  X-RECENCY        PIC X.
           03  X-PDAYS          PIC X.
           03  X-ARREAR         PIC X.

      **********************
      *   LINE WORKERS     *
      **********************
       01  LN-FEED          PIC 99       VALUE 0.

       COPY "LIBGB/EOBUF_EXT.CPY".
           03  BEG-BRANCH         PIC 9(4).
           03  END-BRANCH         PIC 9(4).
           03  BEG-CLASS          PIC 9(3).
           03  END-CLASS          PIC 9(3).
           03  BEG-GROUP-ID       PIC X(6).
           03  CLEAR-AWFILE       PIC X.
           03  EXTRDATE           PIC 9(8).
           03  REMOVE-RESCHED-FG  PIC X.
           03  RESCHED-DAYS       PIC 99.
           03  REMOVE-UNWORKED-FG PIC X.
           03  REPLACE-COL-FG     PIC X.
           03  GROUP-FLAG         PIC X.
           03  ENT-GROUP          PIC X(4).

       01  VERSION-CONTROL      PIC X VALUE "C".


       01  SV-BEG-BR           PIC 9999  VALUE 9999.
       01  SV-END-BR           PIC 9999  VALUE 9999.

       01  MESS-LIST            PIC X(5) VALUE "MESS.".

       01  HOLD-DATE            PIC 9(8) COMP-3 VALUE 0.

       01  WK-FORMNO            PIC 9(4) COMP-3 VALUE 0.
       01  NO1-EXISTS           PIC X           VALUE SPACES.
       01  CQ1-EXISTS           PIC X           VALUE SPACES.
       01  GQ1-EXISTS           PIC X           VALUE SPACES.
       01  FIRST-TIME           PIC X           VALUE "Y".

       01  TEST-ACTION-TABLE1.
           03  TEST-ACTION-TAB1 PIC X     OCCURS 4.
       01  TEST-ACTION-TABLE2.
           03  TEST-ACTION-TAB2 PIC X     OCCURS 4.
       01  TEST-LIMIT           PIC 9     VALUE 4.

       01  TEST-COLLECTOR-TABLE1.
           03  TEST-COLLECTOR-TAB1 PIC X     OCCURS 3.
       01  TEST-COLLECTOR-TABLE2.
           03  TEST-COLLECTOR-TAB2 PIC X     OCCURS 3.

       COPY "LIBWI/PFSWINDOWSW.CPY".
       COPY "LIBWI/TSTINTW.CPY".
       COPY "LIBWI/CDSCANW.CPY".
       COPY "LIBWI/GRSCANW.CPY".
       COPY "LIBGB/BRSECUREW.CPY".
       COPY "LIBGB/GETPRW.CPY".
       COPY "LIBGB/SPPDTHW.CPY".
       COPY "LIBLP/LPLCASW.CPY".
       COPY "LIBLP/LPLCHGW.CPY".
       COPY "LIBLP/LPSCHDW.CPY".
       COPY "LIBLP/LPLCAPW.CPY".
       COPY "LIBLP/LPCPAW.CPY".
       COPY "LIBCL/CRNOR2W.CPY".
       COPY "LIBLP/DEFPOLW.CPY".
       COPY "LIBLP/LPPOFFW.CPY".
       COPY "LIBLP/ADDONSPRW.CPY".
       COPY "LIBLP/LPITRMW.CPY".
       COPY "LIBLP/LPAVCRW.CPY".
       COPY "LIBGB/REBATEW.CPY".
       COPY "LIBLP/LPAPRSW.CPY".
       COPY "LIBLP/LPAPRZW.CPY".
       COPY "LIBLP/LPUPERW.CPY".
       COPY "LIBLP/LPMDTEW.CPY".
       COPY "LIBLP/LPXDTEW.CPY".
       COPY "LIBLP/LPIBPCW.CPY".
       COPY "LIBLP/LPRATEW.CPY".
       COPY "LIBLP/LPPDUEW.CPY".
       COPY "LIBLP/LPCDELW.CPY".
       COPY "LIBLP/LPAMTSW.CPY".
       COPY "LIBGB/AGEINGW.CPY".
       COPY "LIBLP/LPUPWKW.CPY".
       COPY "LIBLP/LPCEPPW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBLP/SETCTBLW.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/RBACTW.CPY".
       COPY "LIBGB/GRWORKW.CPY".
       COPY "LIBGB/PASSWDW.CPY".
       COPY "LIBGB/GETDLSW.CPY".
       COPY "LIBGB/GPENVW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/PRU_LNK.CPY".
       COPY "LIBGB/SCREENW.CPY".

       COPY "LIBLP/CQRUN_DEF.CPY".
       COPY "LIBLP/CQRUN_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".

      /***************************************
      *     P R O G R A M   C O N T R O L    *
      ****************************************
       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBLP/CQRUN_SCN.CPY".
       PROCEDURE DIVISION
             USING FORM-PATHNAME EXIT-PATHNAME.
       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON PR-FILE.
       COPY "LIBGB/DECLRP.CPY".
           PERFORM CLOSE-GQS1-FILE.
           PERFORM CLOSE-GQ2-FILE.
           PERFORM CLOSE-GQ1-FILE.
           PERFORM CLOSE-CQS1-FILE.
           PERFORM CLOSE-CQ2-FILE.
           PERFORM CLOSE-CQ1-FILE.
           PERFORM CLOSE-CP1-FILE.
           PERFORM CLOSE-TZ1-FILE.
           PERFORM CLOSE-SP1-FILE.
           PERFORM CLOSE-NO1-FILE.
           PERFORM CLOSE-LTP1-FILE.
           PERFORM CLOSE-LTI1-FILE.
           PERFORM CLOSE-LP1-FILE.
           PERFORM CLOSE-LN3-FILE.
           PERFORM CLOSE-FM1-FILE.
           PERFORM CLOSE-FD1-FILE.
           PERFORM CLOSE-EA1-FILE.
           PERFORM CLOSE-CD1-FILE.
           PERFORM CLOSE-BW1-FILE.
           PERFORM CLOSE-GR1-FILE.
           PERFORM CLOSE-BR-FILE.
           PERFORM CLOSE-GB-FILE.
           PERFORM CLOSE-PR-FILE.
       COPY "LIBGB/DECLRP2.CPY".

      ********************************
      *  MAIN MODULE:
      ********************************
       MAIN-MODULE SECTION.

           PERFORM SQL-CONNECT.

      D    STOP MESS.
           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "CQRUN" TO FORM-PROG VDU-FORM-NAME.

           PERFORM GET-GPENV.

           PERFORM INITIALIZATION.
           IF RTN-FG = "END"
              GO TO EXIT-PROG.

           IF GROUP-FLAG = "N"
              MOVE BEG-BRANCH TO BR-NO
              PERFORM START-BR-FILE
              GO TO NEXT-BRANCH-NO.

           MOVE ENT-GROUP TO GR-ID.
           PERFORM SEARCH-GR-INIT.

      *----------------------------------------------------------------
       NEXT-GROUP-LOC.
      D    STOP MESS.
           IF GROUP-FLAG NOT = "Y"
              GO TO NEXT-BRANCH-NO.

           PERFORM SEARCH-GR-NEXT.
           IF WGR-SRESULT = 0
              GO TO END-ROUTINE.

           MOVE WGR-SRESULT TO BR-NO.
           PERFORM READ-BR-FILE.
           IF IO-BAD
              GO TO IO-ERROR.

           IF BR-COLLECTOR-QUEUE NOT = "Y"
              GO TO NEXT-GROUP-LOC.

           GO TO MOVE-BRANCH-PATH-INFO.

      *----------------------------------------------------------------
       NEXT-BRANCH-NO.
           PERFORM READ-BR-FILE-NEXT.
           IF IO-BAD OR BR-NO > END-BRANCH
              GO TO END-ROUTINE.

      *----------------------------------------------------------------
       MOVE-BRANCH-PATH-INFO.
      *---------------------------------------------------------------

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      * CHECK EA TO SEE IF ANY CLASS & GROUP MATCHES FOR THIS BRANCH
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

           MOVE BR-NO        TO EA-BRANCH
                                QEA1-WBEG-BRANCH
                                QEA1-WEND-BRANCH.
           MOVE BEG-CLASS    TO EA-CLASS
                                QEA1-WBEG-CLASS.
           MOVE END-CLASS    TO QEA1-WEND-CLASS.
           MOVE BEG-GROUP-ID TO EA-GROUP-ID
                                QEA1-WBEG-GROUP-ID
                                QEA1-WEND-GROUP-ID.
           MOVE 0            TO EA-SEQNO
                                QEA1-WBEG-SEQNO.
           MOVE ALL "9"      TO QEA1-WEND-SEQNO.

           PERFORM START-EA1-FILE.

       FIND-EA1FILE-NEXT.
           PERFORM READ-EA1-FILE-NEXT.
           IF IO-BAD
              GO TO NEXT-GROUP-LOC.

      *   EA-BRANCH NOT = BRANCH NUMBER GET NEW BRANCH
           IF EA-BRANCH NOT = BR-NO
               GO TO NEXT-GROUP-LOC.

      *   EA-CLASS IS GREATER THAN ENDING CLASS, GET NEW BRANCH
           IF EA-CLASS > END-CLASS
               GO TO NEXT-GROUP-LOC.

      *   TEST FOR EA WITHIN CLASS RANGE AND GROUP-ID
           IF EA-CLASS  NOT < BEG-CLASS
              IF EA-CLASS  NOT > END-CLASS
                 IF EA-GROUP-ID = BEG-GROUP-ID
                    GO TO BEGIN-PROCESS.

           GO TO FIND-EA1FILE-NEXT.

      *  EA EXISTS FOR CLASS AND GROUP ID
       BEGIN-PROCESS.

           MOVE EXT-FILPATH-BASE           TO FILPATH-BASE.
           MOVE BR-MACHINE                 TO FILPATH-MACHINE.
           MOVE BR-DATA-PATH               TO FILPATH-DATA.

           MOVE "B"                        TO BW-PATH-OVERRIDE
                                              CP-PATH-OVERRIDE
                                              CQ-PATH-OVERRIDE
                                              CQS-PATH-OVERRIDE
                                              FM-PATH-OVERRIDE
                                              GQ-PATH-OVERRIDE
                                              GQS-PATH-OVERRIDE
                                              LN-PATH-OVERRIDE
                                              LP-PATH-OVERRIDE
                                              LTI-PATH-OVERRIDE
                                              LTP-PATH-OVERRIDE
                                              NO-PATH-OVERRIDE
                                              GB-PATH-OVERRIDE.

           IF ( GLOBAL-QUEUING-FG = "Y" )
              MOVE EXT-FILPATH-BASE        TO CP-GLOBAL-BASE
              MOVE EXT-FILPATH-MACHINE     TO CP-GLOBAL-MACHINE
              MOVE "G"                     TO CP-PATH-OVERRIDE
              MOVE CP-GLOBAL-PATH          TO CP-PATH.


           IF GLOBAL-QUEUING-FG NOT = "Y"
              PERFORM PURGE-CQS-RECORDS   *>SUMMARY
              PERFORM PURGE-CQ-RECORDS    *>DETAIL
              PERFORM INITIALIZE-COLLECTOR-POOLS
           ELSE
              PERFORM PURGE-GQ-RECORDS.   *>DETAIL

      * NEED FOR LCAS ROUTINES (REPLACES GLOBRD)
           PERFORM OPEN-GB-FILE.
           MOVE GB-PATH-OWNBR TO GB-BRNO.
           MOVE 1             TO GB-NO.
           PERFORM READ-GB-FILE.
           PERFORM CLOSE-GB-FILE.
           IF IO-BAD
              GO TO END-OF-GROUP.

           PERFORM OPEN-LN3-FILE.
           PERFORM OPEN-BW1-FILE.
           PERFORM OPEN-CP1-FILE.
           PERFORM OPEN-CQ1-FILE.
           PERFORM OPEN-CQS1-FILE.
           PERFORM OPEN-EA1-FILE.

      ********************************
      *  START NEW BRANCH:
      ********************************
       START-NEW-BRANCH.
           MOVE BR-NO     TO LN-OWNBR
                             QLN3-WBEG-OWNBR
                             QLN3-WEND-OWNBR.
           MOVE BEG-CLASS TO LN-CLASS
                             QLN3-WBEG-CLASS.
           MOVE END-CLASS TO QLN3-WEND-CLASS.
           MOVE 0         TO LN-ACCTNO
                             QLN3-WBEG-ACCTNO.
           MOVE ALL "9"   TO QLN3-WEND-ACCTNO.

           PERFORM START-LN3-FILE.
       READ-LN3FILE-NEXT.
           PERFORM READ-LN3-FILE-NEXT.
           IF IO-BAD OR (LN-OWNBR NOT = LN-PATH-OWNBR)
              GO TO END-OF-GROUP.

      *  POCD - **  IS A DELETED CONTRACT
           IF ( LN-POCD-NONLOAN )
              GO TO READ-LN3FILE-NEXT.

           PERFORM NPR-TRAP-INTERRUPT.
        
           IF LN-OWNBR > BR-NO
              GO TO END-OF-GROUP.

           IF LN-CLASS > END-CLASS
              GO TO END-OF-GROUP.

      * CLEAR LOAN FIELDS FOR COMPARE:
           MOVE " " TO DATE-TABLE3.
    

           PERFORM GET-MAKER.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      * START READING EA1FILE:
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

           MOVE LN-OWNBR     TO EA-BRANCH
                                QEA1-WBEG-BRANCH
                                QEA1-WEND-BRANCH.
           MOVE LN-CLASS     TO EA-CLASS
                                QEA1-WBEG-CLASS
                                QEA1-WEND-CLASS.
           MOVE BEG-GROUP-ID TO EA-GROUP-ID
                                QEA1-WBEG-GROUP-ID
                                QEA1-WEND-GROUP-ID.
           MOVE 0            TO EA-SEQNO
                                QEA1-WBEG-SEQNO.
           MOVE ALL "9"      TO QEA1-WEND-SEQNO.
           PERFORM START-EA1-FILE.

      *-----------------------------------------------------------------
       READ-EA1FILE-NEXT.
           PERFORM READ-EA1-FILE-NEXT.
           IF IO-BAD
              GO TO READ-LN3FILE-NEXT.

           IF EA-BRANCH NOT = LN-OWNBR
              GO TO READ-LN3FILE-NEXT.

           IF EA-CLASS NOT = LN-CLASS
              GO TO READ-LN3FILE-NEXT.

           IF EA-GROUP-ID NOT = BEG-GROUP-ID
              GO TO READ-LN3FILE-NEXT.

           IF EA-CQ-FG NOT = "Y"
              GO TO READ-EA1FILE-NEXT.

      *  2/28/06, ADDED EA-INCL-COLLECTOR, OLD RECORDS WOULD HAVE
      *           SPACES, SO FORCE ??? TO INCLUDE, BARB

           IF EA-INCL-COLLECTOR = SPACES
              MOVE "???" TO EA-INCL-COLLECTOR.

           PERFORM TEST-EA1-FIELDS.

           IF RTN-FG = "NL"
              GO TO READ-LN3FILE-NEXT.
           IF RTN-FG NOT = "AW"
              GO TO READ-EA1FILE-NEXT.

      *  SETUP ACTION WORK RECORD:
           MOVE SPACES TO AW-WORKERS.
           MOVE LN-OWNBR        TO AW-LN-BRNO.
           MOVE LN-ACCTNO       TO AW-ACCTNO.
           MOVE EA-LN-COL-ID    TO AW-COLCTID.
           MOVE EA-SET-LN-COL   TO AW-SET-LN-COL.
           MOVE EA-SETACT-CD    TO AW-SETACTCD.
           MOVE EA-SETFM-NO     TO AW-SETFMNO.
           MOVE EA-INC-CNT      TO AW-INCCNT.
           MOVE HOLD-DATE       TO AW-TODAYDUE-DATE.
           MOVE EA-INC-PROMISED TO AW-INCPROM.
           MOVE 0 TO AW-SETFM(1) AW-SETFM(2) AW-SETFM(3).
           MOVE "Y"             TO AW-CQ-FG.
           MOVE 0               TO AW-REV-PDUE-DAYS.
           IF X-CONTRACT NOT = "Y"
              PERFORM COMPUTE-CONTRACT-SUB.

       CREATE-DELINQUENCY-KEY.
           PERFORM CREATE-RENEWABILITY-KEY.

           GO TO EXIT-DELINQUENCY-KEY.

      *    IF BR-COL-QUEUE-LOW-TO-HI = "Y"
      *       MOVE CONTRACTUAL TO AW-REV-DEL
      *       MOVE 0 TO AW-REV-PDUE-DAYS
      *       IF CPOT30 = "Y"
      *          MOVE 15 TO AW-REV-DEL
      *          IF X-PDAYS NOT = "Y"
      *             PERFORM COMPUTE-PASTDUE
      *          END-IF
      *          MOVE PDUE-DAYS TO AW-REV-PDUE-DAYS
      *       END-IF
      *       IF CPOT30 = "M"
      *          MOVE 20 TO AW-REV-DEL
      *          IF X-PDAYS NOT = "Y"
      *             PERFORM COMPUTE-PASTDUE
      *          END-IF
      *          MOVE PDUE-DAYS TO AW-REV-PDUE-DAYS
      *       END-IF
      *       IF (BR-COL-QUEUE-PAST-DUE = "Y" AND CPOT30 = " ")
      *          IF X-PDAYS NOT = "Y"
      *             PERFORM COMPUTE-PASTDUE
      *          END-IF
      *          MOVE PDUE-DAYS TO AW-REV-PDUE-DAYS
      *       END-IF
      *    ELSE
      *       COMPUTE AW-REV-DEL = 99000 - CONTRACTUAL
      *       MOVE 0 TO AW-REV-PDUE-DAYS
      *       IF CPOT30 = "Y"
      *          MOVE 98985 TO AW-REV-DEL
      *          IF X-PDAYS NOT = "Y"
      *             PERFORM COMPUTE-PASTDUE
      *          END-IF
      *          COMPUTE AW-REV-PDUE-DAYS =
      *             99999 - PDUE-DAYS
      *       END-IF
      *       IF CPOT30 = "M"
      *          MOVE 98980 TO AW-REV-DEL
      *          IF X-PDAYS NOT = "Y"
      *             PERFORM COMPUTE-PASTDUE
      *          END-IF
      *          COMPUTE AW-REV-PDUE-DAYS =
      *             99999 - PDUE-DAYS
      *       END-IF
      *       IF (BR-COL-QUEUE-PAST-DUE = "Y" AND CPOT30 = " ")
      *          IF X-PDAYS NOT = "Y"
      *             PERFORM COMPUTE-PASTDUE
      *          END-IF
      *          COMPUTE AW-REV-PDUE-DAYS = 99999 - PDUE-DAYS
      *       END-IF.
      *    IF LN-UNITPER-CD NOT = "M"
      *       IF CPOT30 = "Y"
      *          IF (GP-I-AM-TFC OR GP-I-AM-GLFIN)
      *             MOVE 0 TO AW-REV-PDUE-DAYS.

       EXIT-DELINQUENCY-KEY.
           MOVE "N" TO AW-CQ-SEQ-FG.
           IF EA-BROKEN-PROM-FG = "Y"
              IF LN-PROMDATE NOT = 0
                 MOVE LN-PROMDATE TO NUM-DATE
                 MOVE HOLD-DATE TO SYS-DATE
                 PERFORM TIM
                 IF ELAPSED-DAYS NOT LESS EA-PROM-BREAK1
                    IF ELAPSED-DAYS NOT GREATER EA-PROM-BREAK2
                       MOVE "B" TO AW-CQ-SEQ-FG.

           MOVE 0 TO AW-CQ-BEHAVIOR-RISK.

       SETUP-CQ.
      * FOR NOW, USED LOAN COLLECTOR UNLESS ITS BLANK, THEN USE POOL
      *    IF EA-CQ-FG = "L"
              IF LN-COLLECTOR NOT = "   "
                 MOVE LN-COLLECTOR TO AW-CQ-COL-ID AW-COLCTID
                 GO TO EXIT-SETUP-CQ.
      * IF NO LOAN COLLECTOR, GET ONE FROM POOL
           MOVE EA-COL-POOL TO CP-ID.
           PERFORM READ-CP1-FILE.
           IF IO-BAD
      *       UNLOCK CP-FILE
              GO TO EXIT-SETUP-CQ.

           IF CP-COLLECTOR-COUNT = 1
      *       UNLOCK CP-FILE
              MOVE CP-COL-ID(1) TO AW-CQ-COL-ID AW-COLCTID
              GO TO EXIT-SETUP-CQ.
           IF CP-LAST-SUBSCRIPT < CP-COLLECTOR-COUNT
              ADD 1 TO CP-LAST-SUBSCRIPT
           ELSE
              MOVE 1 TO CP-LAST-SUBSCRIPT.
           MOVE CP-COL-ID(CP-LAST-SUBSCRIPT) TO AW-CQ-COL-ID AW-COLCTID.
           PERFORM REWRITE-CP1-FILE.
       EXIT-SETUP-CQ.
           IF GP-TIME-ZONE-Q = "Y"
              PERFORM GET-TIME-ZONE
           ELSE
              MOVE 0 TO AW-START-TIME.
           MOVE LN-OWNBR TO AW-LN-BRNO.

      * NO AUTOMATIC UPDATE, CREATE AW-RECORD:
      *    IF BR-EXTRACT-AUTO-UPD NOT = "Y"
      *       PERFORM WRITE-AW1-FILE
      *       IF IO-BAD
      *          PERFORM REWRITE-AW1-FILE
      *          IF IO-BAD
      *             GO TO IO-ERROR
      *          ELSE
      *             GO TO READ-LN3FILE-NEXT
      *       ELSE
      *          GO TO READ-LN3FILE-NEXT.

      * AUTOMATIC UPDATE:
      * IF RANGES ARE "ALL", THE SP RECORD MAY NOT HAVE BEEN READ!!
           PERFORM READ-SP-RECORD.
           MOVE "N" TO UPD-FLAG.

           PERFORM EXTRACTION-UPDATE.

      *    PERFORM REWRITE-LN3-FILE.
      *    IF IO-BAD
      *       GO TO IO-ERROR.

           GO TO READ-LN3FILE-NEXT.

       END-OF-GROUP.
           IF GLOBAL-QUEUING-FG NOT = "Y"
              PERFORM CREATE-CQS-RECORD.
      *    PERFORM CLOSE-AW1-FILE.
           PERFORM CLOSE-LN3-FILE.
           PERFORM CLOSE-BW1-FILE.
           PERFORM CLOSE-CP1-FILE.
           PERFORM CLOSE-CQ1-FILE.
           GO TO NEXT-GROUP-LOC.

       END-ROUTINE.
           IF GLOBAL-QUEUING-FG = "Y"
              PERFORM CREATE-GQS-RECORD.
           GO TO END-PROGRAM.

      *********************************************
       INITIALIZATION SECTION.
      *********************************************
           MOVE CQRUN-QUEUE-POS  TO Q-POS.
           MOVE CQRUN-COPIES-POS TO C-POS.
           MOVE CQRUN-PRU-POS    TO P-POS.
           MOVE SPACES TO Q-BUF C-BUF P-BUF.

       COPY "LIBGB/EOINIT.CPY".

           MOVE EXTRDATE TO HOLD-DATE.

           IF EXT-ROUTE-BUF = "00"
              MOVE "COLLECTOR QUEUE CREATION IN PROGRESS..." TO WR-BUF
              MOVE MESS-LIST TO WR-LIST
              PERFORM CALL-WRFORM.

           MOVE "Y" TO TSTINT-CONTINUE-ONLY.
           PERFORM ENABLE-INTERRUPT.

           IF ( BEG-BRANCH = 0 AND END-BRANCH = 0 )
              MOVE BEG-BRANCH TO SV-BEG-BR
              MOVE END-BRANCH TO SV-END-BR
              MOVE EXT-CONAME-USER-LOC TO BEG-BRANCH END-BRANCH.

           MOVE "EA"         TO CD-TYPE.
           MOVE BEG-GROUP-ID TO CD-CODE.
           PERFORM OPEN-CD1-FILE.
           PERFORM READ-CD1-FILE.
           PERFORM CLOSE-CD1-FILE.
           IF ( IO-BAD )
              GO TO INITIALIZATION-END.

           IF CD-EA-CQ-FG NOT = "Y"
              GO TO INITIALIZATION-END.

           MOVE CD-EA-GQ-FG TO GLOBAL-QUEUING-FG.

           IF GLOBAL-QUEUING-FG = "Y"
              PERFORM OPEN-GQ1-FILE
              PERFORM OPEN-GQS1-FILE.

           PERFORM OPEN-BR-FILE.
           PERFORM OPEN-GR1-FILE.
      * MUST LEAVE SP1-FILE OPEN BECAUSE ADDONSPR READS IT
           PERFORM OPEN-SP1-FILE.
           PERFORM OPEN-EA1-FILE.
           PERFORM OPEN-FD1-FILE.
           PERFORM OPEN-FM1-FILE.
           PERFORM OPEN-NO1-FILE.

      * 4/3/03 HEY, TFC USES LAST DAY OF MONTH FOR CPRUN, CQRUN
      * EXTRACT DATE, SO IT MESSES UP DAYLIGHT SAVINGS, USE
      * SYSTEM DATE FOR THEM:

           IF GP-TIME-ZONE-Q = "Y"
              MOVE EXTRDATE TO DLS-DATE
              PERFORM GET-DLS
              PERFORM OPEN-TZ1-FILE.

           IF GLOBAL-QUEUING-FG = "Y"
              PERFORM PURGE-GQS-SUMMARY
              PERFORM GLOBAL-INIT-COLLECTOR-POOLS.

           GO TO INITIALIZATION-EXIT.
       INITIALIZATION-END.
           MOVE "END" TO RTN-FG.
       INITIALIZATION-EXIT.
           EXIT.

      *********************************************
       EO-EXEC-AUDIT SECTION.
      *********************************************

           MOVE EXT-EOBUF-DATE2 TO EXTRDATE.

       EO-EXEC-EXIT.
           EXIT.

      *********************************************
       ENTER-PARAMETERS SECTION.
      *********************************************
       ENTER-PARM.

           IF VDUSTATUS = "1U"
              GO TO GROUP-ID-YN.

      *----------------------------------------------------------------
       BR-01.
           MOVE "R  A000BR1." TO VDU.
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
              MOVE "INVALID RANGE" TO MESS
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
           MOVE 0             TO BEG-BRANCH VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN040BR2." TO VDU.
           MOVE 9999          TO END-BRANCH VDUNUM0.
           PERFORM SCREENIO.
           MOVE "N"           TO GROUP-FLAG.
           MOVE "S  A160DISPLAY-USER." TO VDU.
           MOVE SPACES TO VDUBUF.
           PERFORM SCREENIO.

      *-----------------------------------------------------------------
       CHECK-BRANCH-SECURITY.
           PERFORM BR-RANGE-SECURITY.

           IF BR-SECURITY-OKAY = "N"
              GO TO BR-01.

      *----------------------------------------------------------------
       EXTRACT-DATE.
      * BATCH WILL USE DATE RAN
           IF EXT-ROUTE-BUF = "40" OR "50"
              GO TO CL-01.

           MOVE "ERNM060EXTRDT." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO BR-02.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO EXTRACT-DATE.
           MOVE VDU-DATE-YYYYMMDD TO EXTRDATE HOLD-DATE.

           IF EXTRDATE = ZERO
              MOVE EXT-CONAME-SYSDATE TO ALP-DATE
              PERFORM CONVERT-ALPDATE-TO-YYYYMMDD
              MOVE "S  8000EXTRDT." TO VDU
              MOVE DATE-YYYYMMDD TO EXTRDATE HOLD-DATE VDU-DATE-YYYYMMDD
              PERFORM SCREENIO.
       CL-01.
           MOVE "ENNN020CL1." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "16" GO TO ALL-CL.
           IF VDUSTATUS = "1U" GO TO EXTRACT-DATE.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO CL-01.
           MOVE VDUNUM0 TO BEG-CLASS.
       CL-02.
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
           GO TO GROUP-ID-YN.
       ALL-CL.
           MOVE "SNNN020CL1." TO VDU.
           MOVE 1 TO BEG-CLASS VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN020CL2." TO VDU.
           MOVE 99 TO END-CLASS VDUNUM0.
           PERFORM SCREENIO.

      *-----------------------------------------------------------------
       GROUP-ID-YN.
           MOVE "E  A060GROUP-ID." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO CL-02.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    FOR COLLECTOR QUEUE GROUP ID SCAN
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( VDUSTATUS = "1<" )
              PERFORM CODE-SCAN
              IF ( IO-BAD )
                 GO TO GROUP-ID-YN
              ELSE
                 MOVE CDSCAN-CODE TO VDUBUF
                 MOVE "00"        TO VDUSTATUS.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO GROUP-ID-YN.

           MOVE "EA"   TO CD-TYPE.
           MOVE VDUBUF TO CD-CODE BEG-GROUP-ID.
           PERFORM OPEN-CD1-FILE.
           PERFORM READ-CD1-FILE.
           PERFORM CLOSE-CD1-FILE.
           IF ( IO-BAD )
              GO TO GROUP-ID-YN.

           IF CD-EA-CQ-FG NOT = "Y"
              GO TO GROUP-ID-YN.

      *-----------------------------------------------------------------
       REMOVE-RESCHEDS.
           MOVE "E  A010REMRES." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO GROUP-ID-YN.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D"
              GO TO REMOVE-RESCHEDS.
           MOVE VDUBUF TO REMOVE-RESCHED-FG.
           IF REMOVE-RESCHED-FG = "N"
              MOVE 99 TO RESCHED-DAYS
              GO TO REMOVE-UNWORKED.
           IF REMOVE-RESCHED-FG NOT = "Y" GO TO REMOVE-RESCHEDS.
       ENTER-RESCHED-DAYS.
           MOVE "ENNN020RESDAYS." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO REMOVE-RESCHEDS.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO ENTER-RESCHED-DAYS.
           MOVE VDUNUM0 TO RESCHED-DAYS.

      *-----------------------------------------------------------------
       REMOVE-UNWORKED.
           MOVE "E  A010REMUNW." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO REMOVE-RESCHEDS
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D"
              GO TO REMOVE-UNWORKED.
           MOVE VDUBUF TO REMOVE-UNWORKED-FG.
           IF REMOVE-UNWORKED-FG NOT = "N"
              IF REMOVE-UNWORKED-FG NOT = "Y"
                 GO TO REMOVE-UNWORKED.
       EXIT-PARM.
           EXIT.

      *********************************************
       DISPLAY-PARAMETERS SECTION.
      *********************************************
       DISPLAY-PARM.

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
           MOVE "SNNN020CL2." TO VDU.
           MOVE END-CLASS TO VDUNUM0.
           PERFORM SCREENIO.
           MOVE "S  A060GROUP-ID." TO VDU.
           MOVE BEG-GROUP-ID TO VDUBUF.
           PERFORM SCREENIO.
           MOVE "S  A010REMRES." TO VDU.
           MOVE REMOVE-RESCHED-FG TO VDUBUF.
           PERFORM SCREENIO.
           MOVE "SN N020RESDAYS." TO VDU.
           MOVE RESCHED-DAYS TO VDUNUM0.
           PERFORM SCREENIO.
           MOVE "S  A010REMUNW." TO VDU.
           MOVE REMOVE-UNWORKED-FG TO VDUBUF.
           PERFORM SCREENIO.

      *********************************************
       TEST-EA1-FIELDS SECTION.
      *********************************************
           MOVE SPACES TO RTN-FG.

      *TEST FOR LEGAL
           IF EA-LEGAL = "O"
              IF LN-LEGALDATE = 0
                 GO TO TEST-EA1-FIELDS-EXIT.

           IF EA-LEGAL = "S"
              IF LN-LEGALDATE NOT = 0
                 GO TO TEST-EA1-FIELDS-EXIT.

      *TEST FOR DEBT-AID
           IF EA-DEBTAID = "O"
              IF LN-DEBTAIDATE = 0
                 GO TO TEST-EA1-FIELDS-EXIT.

           IF EA-DEBTAID = "S"
              IF LN-DEBTAIDATE NOT = 0
                 GO TO TEST-EA1-FIELDS-EXIT.

      *TEST FOR JUDGEMENTS
           IF EA-JUDGEMENT = "O"
              IF LN-JDDATE = 0
                 GO TO TEST-EA1-FIELDS-EXIT.

           IF EA-JUDGEMENT = "S"
              IF LN-JDDATE NOT = 0
                 GO TO TEST-EA1-FIELDS-EXIT.

      *  TEST LOAN CONFIDENTIAL FLAG
           IF EA-CONFIDENTIAL-FG = "O"
              IF LN-CONFIDENTCD NOT = "Y"
      * BOTH FLAGS MUST NOT BE SET
                 IF BW-CONFIDENTIAL NOT = "Y"
                    GO TO TEST-EA1-FIELDS-EXIT.
           IF EA-CONFIDENTIAL-FG = "S"
      * IF EITHER FLAG IS SET, SKIP IT
              IF LN-CONFIDENTCD = "Y" OR BW-CONFIDENTIAL = "Y"
                 GO TO TEST-EA1-FIELDS-EXIT.

      *  TEST 1ST PAYMENT MISSES
           IF EA-1ST-PMT-MISSES = "O"
              IF LN-MIDSTREAM NOT = "Y"
                 IF LN-DATE-PAID-LAST NOT = 0
                    GO TO TEST-EA1-FIELDS-EXIT
                 END-IF
              ELSE
                 IF BR-MIDSTREAM-1STPAY-FG NOT = "Y"
                    IF LN-DATE-PAID-LAST NOT = 0
                       GO TO TEST-EA1-FIELDS-EXIT
                    END-IF
                 ELSE
                    IF LN-ORIGPURAMT NOT = LN-CURBAL
                       GO TO TEST-EA1-FIELDS-EXIT.

           IF EA-1ST-PMT-MISSES = "S"
              IF LN-MIDSTREAM NOT = "Y"
                 IF LN-DATE-PAID-LAST = 0
                    GO TO TEST-EA1-FIELDS-EXIT
                 END-IF
              ELSE
              IF BR-MIDSTREAM-1STPAY-FG NOT = "Y"
                 IF LN-DATE-PAID-LAST = 0
                    GO TO TEST-EA1-FIELDS-EXIT
                 END-IF
              ELSE
                 IF LN-ORIGPURAMT = LN-CURBAL
                 GO TO TEST-EA1-FIELDS-EXIT.

      *  TEST ACCOUNT BALANCES:
           COMPUTE LN-ACCUM-BAL = LN-CURBAL + LN-OTHBAL + LN-INTBAL
                                + LN-LCBAL.
           IF LN-ACCUM-BAL < EA-ACCT-BAL1 OR
              LN-ACCUM-BAL > EA-ACCT-BAL2
              GO TO TEST-EA1-FIELDS-EXIT.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      * TEST PL
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( EA-PL = "O" )
              IF ( LN-PLDATE = ZEROES )
                 GO TO TEST-EA1-FIELDS-EXIT.

           IF ( EA-PL = "S" )
              IF ( LN-PLDATE NOT = ZEROES )
                 GO TO TEST-EA1-FIELDS-EXIT.

           IF ( LN-PLDATE NOT = ZEROES )
              IF ( EA-PL-IN-ACTIVE-BOTH = "A" )
                 IF ( LN-PLCD NOT = "P" )
                    GO TO TEST-EA1-FIELDS-EXIT.

           IF ( LN-PLDATE NOT = ZEROES )
              IF ( EA-PL-IN-ACTIVE-BOTH = "I" )
                 IF ( LN-PLCD NOT = "I" )
                    IF ( LN-PLCD NOT = "W" )
                       GO TO TEST-EA1-FIELDS-EXIT.

           IF ( LN-PLDATE      = ZEROES    ) OR
              ( EA-PL-BEG-DATE NOT NUMERIC ) OR
              ( EA-PL-BEG-DATE = 02020202    ) OR
              ( EA-PL-BEG-DATE = 20202020    )
              GO TO TEST-EA1-REPO.

      *- - - - - - - - - - - - - - - - - - - - - - - -{ CHECK PL DATE  }
           IF LN-PLDATE < EA-PL-BEG-DATE OR LN-PLDATE > EA-PL-END-DATE
              GO TO TEST-EA1-FIELDS-EXIT.

      *----------------------------------------------------{ TEST REPO }
       TEST-EA1-REPO.
           IF EA-REPO = "O"
              IF NOT (LN-REPOCD = "X" OR "S")
                 GO TO TEST-EA1-FIELDS-EXIT.

           IF EA-REPO = "S"
              IF LN-REPOCD = "X" OR "S"
                 GO TO TEST-EA1-FIELDS-EXIT.

      * TEST BANKRUPT
           IF EA-BANKRUPT = "O"
              IF LN-BNKRPTDATE = 0
                 GO TO TEST-EA1-FIELDS-EXIT.

           IF EA-BANKRUPT = "S"
              IF LN-BNKRPTDATE NOT = 0
                 GO TO TEST-EA1-FIELDS-EXIT.

      * TEST ALLOTMENTS
           IF EA-ALLOTMENTS = "O"
              IF LN-ALLOTCD NOT = "Y"
                 GO TO TEST-EA1-FIELDS-EXIT.

           IF EA-ALLOTMENTS = "S"
              IF LN-ALLOTCD = "Y"
                 GO TO TEST-EA1-FIELDS-EXIT.

      * TEST DEALER NUMBER RANGE
           IF LN-DLNO < EA-DLNO1 OR
              LN-DLNO > EA-DLNO2
              GO TO TEST-EA1-FIELDS-EXIT.

      * DAY OF MONTH DUE DATE RANGE
      * NOTE: NEW FIELDS 10/30/02, SO MIGHT BE SPACES:
           IF EA-DAY-OF-MO-DUE1 = 20
              IF EA-DAY-OF-MO-DUE2 = 20
                 GO TO TEST-PASTDUE-DAYS.
           IF LN-1STPYDA < EA-DAY-OF-MO-DUE1 OR
              LN-1STPYDA > EA-DAY-OF-MO-DUE2
              GO TO TEST-EA1-FIELDS-EXIT.

      *  TEST PASTDUE DAYS:
       TEST-PASTDUE-DAYS.
           IF EA-PASTDUE-DAY1 = 0
              IF EA-PASTDUE-DAY2 = 9999
                 GO TO TEST-EA1-PASTDUE-AMT.
           IF X-PDAYS NOT = "Y"
              PERFORM COMPUTE-PASTDUE
              IF RTN-FG = "NL"
                 GO TO TEST-EA1-FIELDS-EXIT.
           IF PDUE-DAYS < EA-PASTDUE-DAY1
                      OR > EA-PASTDUE-DAY2
              GO TO TEST-EA1-FIELDS-EXIT.

      *  TEST PASTDUE AMOUNT:
       TEST-EA1-PASTDUE-AMT.
           IF EA-PASTDUE-AMT1 = 0
              IF EA-PASTDUE-AMT2 = 9999.99
                 GO TO TEST-EA1-RECENCY.
           IF X-ARREAR NOT = "Y"
              PERFORM COMPUTE-ARREAR
              IF RTN-FG = "NL"
                 GO TO TEST-EA1-FIELDS-EXIT.
           IF CDEL-ARREARAGE < EA-PASTDUE-AMT1
            OR (EA-PASTDUE-AMT2 NOT = 9999.99 AND
                    CDEL-ARREARAGE > EA-PASTDUE-AMT2)
              GO TO TEST-EA1-FIELDS-EXIT.

      *  TEST RECENCY DAYS:
       TEST-EA1-RECENCY.
           IF EA-RECENCY-DAY1 = 0
              IF EA-RECENCY-DAY2 = 9999
                 GO TO TEST-EA1-CONTR.

          IF X-RECENCY NOT = "Y"
              PERFORM COMPUTE-RECENCY-SUB
              IF RTN-FG = "NL"
                 GO TO TEST-EA1-FIELDS-EXIT.

      * #1580 (RECENCY GETS PERFORMED IN OTHER PLACES, SO ALWAYS
      *        MOVE RECDEL TO RECENCY HERE)
           IF (BR-RECDEL-NOT-REC = "Y" AND GP-I-AM-WAGNER) OR
                                          (GP-I-AM-WORLD)
              MOVE RECDEL TO RECENCY.

           IF RECENCY < 0
              MOVE 0 TO RECENCY.

      * SKIP ACCOUNTS DUE FOR 1ST PAYMENT TODAY:
           IF NOT GP-I-AM-WORLD
              IF LN-DATE-PAID-LAST = 0
                 IF RECENCY = 0
                    GO TO TEST-EA1-FIELDS-EXIT.

           IF RECENCY < EA-RECENCY-DAY1 OR > EA-RECENCY-DAY2
              GO TO TEST-EA1-FIELDS-EXIT.

      *  TEST CONTRACTUAL DAYS:
       TEST-EA1-CONTR.
           IF EA-CONTRACTUAL-DAY1 = 0
              IF EA-CONTRACTUAL-DAY2 = 9999
                 GO TO TEST-EA1-CPOTS.
           IF X-CONTRACT NOT = "Y"
              PERFORM COMPUTE-CONTRACT-SUB
              IF RTN-FG = "NL"
                 GO TO TEST-EA1-FIELDS-EXIT.
           IF CONTRACTUAL < EA-CONTRACTUAL-DAY1
                    OR > EA-CONTRACTUAL-DAY2
              GO TO TEST-EA1-FIELDS-EXIT.

      *  TEST CONTRACTUAL POTS:
       TEST-EA1-CPOTS.
           IF X-CONTRACT NOT = "Y"
              PERFORM COMPUTE-CONTRACT-SUB.
           IF EA-CPOTS = "S"
              IF CPOT30 = "Y"
                 GO TO TEST-EA1-FIELDS-EXIT.
           IF EA-CPOTS = "O"
              IF CPOT30 NOT = "Y"
                 GO TO TEST-EA1-FIELDS-EXIT.

      *  TEST RECENCY POTS:
       TEST-EA1-RPOTS.
      * NOTE: NEW FIELD 10/17/00, SO MIGHT BE SPACES:
           IF EA-RPOTS NOT = "S"
              IF EA-RPOTS NOT = "I"
                 IF EA-RPOTS NOT = "O"
                    GO TO TEST-EA1-MISPAYS.

           IF X-RECENCY NOT = "Y"
              PERFORM COMPUTE-RECENCY-SUB.

      * #1580 (RECENCY GETS PERFORMED IN OTHER PLACES, SO ALWAYS
      *        MOVE RECDEL TO RECENCY HERE)
           IF (BR-RECDEL-NOT-REC = "Y" AND GP-I-AM-WAGNER) OR
                                          (GP-I-AM-WORLD)
               MOVE RECDEL TO RECENCY.

           IF EA-RPOTS = "S"
              IF RDPOT30 = "Y"
                 GO TO TEST-EA1-FIELDS-EXIT.
           IF EA-RPOTS = "O"
              IF RDPOT30 NOT = "Y"
                 GO TO TEST-EA1-FIELDS-EXIT.

      *  TEST MISPAYS:
       TEST-EA1-MISPAYS.
           IF EA-MISPAYS NOT = "S"
              IF EA-MISPAYS NOT = "I"
                 IF EA-MISPAYS NOT = "O"
                    GO TO TEST-EA1-PROMISED.
           IF X-CONTRACT NOT = "Y"
              PERFORM COMPUTE-CONTRACT-SUB.
           IF EA-MISPAYS = "S"
              IF CPOT30 = "M"
                 GO TO TEST-EA1-FIELDS-EXIT.
           IF EA-MISPAYS = "O"
              IF CPOT30 NOT = "M"
                 GO TO TEST-EA1-FIELDS-EXIT.

      *  TEST PROMISED ACCOUNTS:
       TEST-EA1-PROMISED.
           IF EA-PROMISED = "I"
              IF LN-PROMDATE = 0
                 GO TO TEST-ACTION
               ELSE
                 GO TO TEST-PASTPROM-DAY.
           IF EA-PROMISED = "S"
              IF LN-PROMDATE NOT = 0
                 GO TO TEST-EA1-FIELDS-EXIT
               ELSE
                 GO TO TEST-ACTION.
           IF EA-PROMISED = "O"
              IF LN-PROMDATE = 0
                 GO TO TEST-EA1-FIELDS-EXIT.

      *  TEST PAST PROMISED DAYS:
       TEST-PASTPROM-DAY.
           MOVE LN-PROMDATE TO NUM-DATE.
           MOVE HOLD-DATE TO SYS-DATE.
           PERFORM TIM.
           IF ELAPSED-DAYS < EA-PASTPROM-DAY1
            OR ELAPSED-DAYS > EA-PASTPROM-DAY2
               GO TO TEST-EA1-FIELDS-EXIT.


      *  TEST ACTION CODES:
       TEST-ACTION.
           MOVE EA-ACTION TO TEST-ACTION-TABLE1.
           MOVE LN-ACTIONCD TO TEST-ACTION-TABLE2.
           MOVE 0 TO SUB.

       TEST-ACTION-CONT.
           ADD 1 TO SUB.
           IF SUB > 4
              GO TO TEST-ORIGIN.
           IF TEST-ACTION-TAB1(SUB) = "?"
              GO TO TEST-ACTION-CONT.
           IF TEST-ACTION-TAB1(SUB) NOT = TEST-ACTION-TAB2(SUB)
              GO TO TEST-EA1-FIELDS-EXIT.
           GO TO TEST-ACTION-CONT.

       TEST-ORIGIN.
           MOVE EA-ORIGIN TO TEST-ACTION-TABLE1.
           MOVE LN-ORIGIN TO TEST-ACTION-TABLE2.
           MOVE 4 TO TEST-LIMIT.
 
           MOVE 0 TO SUB.

       TEST-ORIGIN-CONT.
           ADD 1 TO SUB.
           IF SUB > TEST-LIMIT
              GO TO TEST-COLLECTOR.
           IF TEST-ACTION-TAB1(SUB) = "?"
              GO TO TEST-ORIGIN-CONT.
           IF TEST-ACTION-TAB1(SUB) NOT = TEST-ACTION-TAB2(SUB)
              GO TO TEST-EA1-FIELDS-EXIT.
           GO TO TEST-ORIGIN-CONT.

       TEST-COLLECTOR.
           MOVE EA-INCL-COLLECTOR TO TEST-COLLECTOR-TABLE1.
           MOVE LN-COLLECTOR TO TEST-COLLECTOR-TABLE2.
           MOVE 0 TO SUB.

       TEST-COLLECTOR-CONT.
           ADD 1 TO SUB.
           IF SUB > 3
              GO TO TEST-LNAME.
           IF TEST-COLLECTOR-TAB1(SUB) = "?"
              GO TO TEST-COLLECTOR-CONT.
           IF TEST-COLLECTOR-TAB1(SUB) NOT = TEST-COLLECTOR-TAB2(SUB)
              GO TO TEST-EA1-FIELDS-EXIT.
           GO TO TEST-COLLECTOR-CONT.

      *  TEST LAST NAME RANGE:
       TEST-LNAME.
      *-----------------------------------------------------------------
           IF (EA-ALPNAME1 = "AAAA" AND EA-ALPNAME2 = LITTLE-Z) OR
               (EA-ALPNAME1 = "AAAA" AND EA-ALPNAME2 = "~~~~")
              GO TO TEST-MATURED.
           IF EA-ALPNAME1 = " " AND EA-ALPNAME2 = " "
              GO TO TEST-MATURED.
           MOVE BW-LNAME TO WORK-LNAME.
           IF WORK-LNAME < EA-ALPNAME1
                       OR > EA-ALPNAME2
              GO TO TEST-EA1-FIELDS-EXIT.

      *  TEST MATURED ACCOUNT:
       TEST-MATURED.
           IF EA-MATURED NOT = "I"
              PERFORM MATURITY-DATE-CALCULATION
              IF EA-MATURED = "S"
                 IF MDTE-DATE < HOLD-DATE
                    GO TO TEST-EA1-FIELDS-EXIT
                 ELSE
                    NEXT SENTENCE
              ELSE
                 IF EA-MATURED = "O"
                    IF MDTE-DATE > HOLD-DATE
                       GO TO TEST-EA1-FIELDS-EXIT.

      * NOTE: NEW FIELDS 10/8/01, SO MIGHT BE SPACES:
           IF EA-LNDATE-DAYS1 = 2020
              IF EA-LNDATE-DAYS2 = 2020
                 GO TO TEST-MATURE-DAYS.
           IF EA-LNDATE-DAYS1 = 0
              IF EA-LNDATE-DAYS2 = 9999
                 GO TO TEST-MATURE-DAYS.

           MOVE LN-LOANDATE TO NUM-DATE.
           MOVE HOLD-DATE TO SYS-DATE.
           PERFORM TIM.
           IF ELAPSED-DAYS < EA-LNDATE-DAYS1
            OR ELAPSED-DAYS > EA-LNDATE-DAYS2
              GO TO TEST-EA1-FIELDS-EXIT.

       TEST-MATURE-DAYS.
      * NOTE: NEW FIELDS 10/8/01, SO MIGHT BE SPACES:
           IF EA-MATURE-DAYS1 = 2020
              IF EA-MATURE-DAYS2 = 2020
                 GO TO TEST-NEXT-FIELD.
           IF EA-MATURE-DAYS1 = 0
              IF EA-MATURE-DAYS2 = 9999
                 GO TO TEST-NEXT-FIELD.

           PERFORM MATURITY-DATE-CALCULATION.
           MOVE HOLD-DATE TO NUM-DATE.
           MOVE MDTE-DATE TO SYS-DATE.
           PERFORM TIM.
           IF ELAPSED-DAYS < EA-MATURE-DAYS1
            OR (ELAPSED-DAYS > EA-MATURE-DAYS2)
              GO TO TEST-EA1-FIELDS-EXIT.

      * TEST-NEXT-FIELD:  THIS IS A DUMMY PARAGRAPH WHICH PRIOR TEST
      * SKIPS TO - WHEN YOU ADD A PARAMETER, CHANGE WHATEVER SKIPS TO
      * HERE TO SKIP TO YOUR NEW TEST.    BLV

       TEST-NEXT-FIELD.
           EXIT.

       TEST-EA1-FIELDS-AWUPD.
           MOVE "AW" TO RTN-FG.
       TEST-EA1-FIELDS-EXIT.
           EXIT.

      **********************************************
      * RECENCY FOR THIS PROGRAM ONLY COMPUTED ON 1STPYDATE
      * INSTEAD OF LOANDATE (WHEN LSTPAYMNT = 0):
      * 121688 BAH ADDED EA-RECENCYDATE FOR CITIZENS
      *********************************************
       COMPUTE-RECENCY-SUB SECTION.
      *********************************************
           PERFORM READ-SP-RECORD.
           MOVE HOLD-DATE   TO AGEING-DATE.
           MOVE LN-LOANDATE TO HOLD-LOAN-DATE.
           IF EA-RECENCYDATE NOT = "Y"
              MOVE LN-1STPYDATE TO LN-LOANDATE.
           PERFORM COMPUTE-RECENCY.
           MOVE HOLD-LOAN-DATE TO LN-LOANDATE.
           MOVE "Y" TO X-RECENCY.

      *********************************************
       COMPUTE-CONTRACT-SUB SECTION.
      *********************************************
           PERFORM READ-SP-RECORD.
           MOVE HOLD-DATE TO AGEING-DATE.
           PERFORM COMPUTE-CONTRACTUAL.
           MOVE "Y" TO X-CONTRACT.

      * COMPUTE PASTDUE AMOUNT:
      *********************************************
       COMPUTE-ARREAR SECTION.
      *********************************************
           PERFORM READ-SP-RECORD.
           MOVE LN-1STPYDATE TO CEPP1-DATE.
           MOVE HOLD-DATE    TO CEPP2-DATE.
           MOVE LN-MAKERCD(1) TO CEPP-MAKERCD.
           PERFORM TOTAL-DELINQUENCY.
           IF CDEL-ARREARAGE < 0
              MULTIPLY -1 BY CDEL-ARREARAGE
           ELSE
              MOVE 0 TO CDEL-ARREARAGE.
           MOVE "Y" TO X-ARREAR.

      * COMPUTE PASTDUE DAYS:
      *********************************************
       COMPUTE-PASTDUE SECTION.
      *********************************************
           PERFORM READ-SP-RECORD.
           MOVE LN-1STPYDATE TO PDUE-1STPYDATE.
           MOVE HOLD-DATE TO PDUE-TODAY-DATE.
           PERFORM PAST-DUE-DAYS-CALCULATION.
           MOVE "Y" TO X-PDAYS.

      *******************************************************************
      *  THIS LOOKS COMPLICATED - IT'S REALLY NOT.  THE TIME ZONE FILE
      *  IS KEYED BY AREA CODE & PREFIX.  ALMOST ALWAYS IT WILL BE
      *  AREA CODE & A PREFIX OF 0.  IT'S POSSIBLE TO HAVE AREA CODES
      *  THAT ENCOMPASS 2 DIFFERENT AREA CODES, THEN YOU COULD SET UP
      *  A ZONE WITH THE AREA CODE & THE 3 DIGIT PREFIX FOR THIS
      *  UNUSUAL SITUATION.
      *
      *  GET THE TIME ZONE:  FIRST TRY HOME PHONE, AREA CODE & PREFIX
      *                IF NO GOOD, TRY HOME PHONE, AREA CODE & 0 PREFIX
      *                IF NO GOOD, TRY EMP PHONE, AREA CODE & PREFIX
      *                IF NO GOOD, TRY EMP PHONE, AREA CODE & 0 PREFIX
      *                IF I-AM-UNITED & CELL PHONE NOT 2020,
      *                   TRY CELL PHONE, AREA CODE & PREFIX
      *                   IF NO GOOD, AREA CODE & 0 PREFIX
      *                IF NO GOOD, USE GP DEFAULT TIME ZONE
      *                IF ZONE IS DAYLIGHT SAVINGS SUBTRACT 1 IF NEEDED
      *******************************************************************
      *********************************************
       GET-TIME-ZONE SECTION.
      *********************************************
           MOVE 1 TO SUB.
           PERFORM GET-BORROWER.
       TRY-HOME-PHONE.
           MOVE BW-PHONE TO HOLD-PHONE.
           MOVE HOLD-AC TO TZ-CODE.
           MOVE HOLD-PREFIX TO TZ-PREFIX.
           PERFORM READ-TZ1-FILE.
           IF IO-FG NOT = 0
              MOVE HOLD-AC TO TZ-CODE
              MOVE 0 TO TZ-PREFIX
              PERFORM READ-TZ1-FILE.
           IF IO-FG NOT = 0
              MOVE BW-EMPPHONE TO HOLD-PHONE
              MOVE HOLD-AC TO TZ-CODE
              MOVE HOLD-PREFIX TO TZ-PREFIX
              PERFORM READ-TZ1-FILE.
           IF IO-FG NOT = 0
              MOVE HOLD-AC TO TZ-CODE
              MOVE 0 TO TZ-PREFIX
              PERFORM READ-TZ1-FILE.
           IF IO-FG NOT = 0
              MOVE GP-TIME-ZONE-FACTOR TO TZ-FACTOR
              MOVE GP-TIME-ZONE-DLS TO TZ-DLS
              MOVE "Y" TO AW-TIME-ZONE-ERROR.
           IF TZ-DLS = "Y"
              IF DLS-FG = "Y"
                 ADD 1 TO TZ-FACTOR.
           MOVE TZ-FACTOR TO AW-START-TIME.
       EXIT-GET-TIME-ZONE.
           EXIT.

      ********************************************************************
      *  FOR WORLD, KEY QUEUE BY RENEWABILITY BY HIGHEST AVAILABLE CREDIT
      *  IF ACCT DETERMINED TO BE RENEWABLE, KEY FIRST WITH REV-DEL OF 0
      *  IF ACCT DETERMINED NOT RENEWABLE, KEY LAST WITH REV-DEL OF 99999
      *  NEXT IN KEY IS REV-PDUE-DAYS, PUT IN INVERSE OF AVAIL CREDIT AMT
      ********************************************************************
      *********************************************
       CREATE-RENEWABILITY-KEY SECTION.
      *********************************************
           MOVE 0 TO AW-REV-DEL.
           IF SP-RENEWABLE-PAYS = 0
              IF SP-RENEWABLE-DAYS = 0
                 MOVE 99999 TO AW-REV-DEL
                 GO TO GET-AVAILABLE-CREDIT.
           MOVE LN-1STPYDATE TO PDTH-DATE-WORK.
           PERFORM PAID-THRU-CALCULATION.
           IF SP-RENEWABLE-PAYS = -1
              COMPUTE WK-RENEWABLE-PAYS = LN-ORGTERM / 3
              IF PDTH-PAYMENTS < WK-RENEWABLE-PAYS
                 MOVE 99999 TO AW-REV-DEL
                 GO TO GET-AVAILABLE-CREDIT
              END-IF
           ELSE
           IF PDTH-PAYMENTS < SP-RENEWABLE-PAYS
              MOVE 99999 TO AW-REV-DEL
              GO TO GET-AVAILABLE-CREDIT.
           MOVE LN-LOANDATE TO NUM-DATE.
           MOVE HOLD-DATE TO SYS-DATE.
           PERFORM TIM.
           IF SP-RENEWABLE-DAYS = -1
              COMPUTE WK-RENEWABLE-DAYS = (LN-ORGTERM / 3) * 30 + 1
              IF ELAPSED-DAYS < WK-RENEWABLE-DAYS
                 MOVE 99999 TO AW-REV-DEL
                 GO TO GET-AVAILABLE-CREDIT
              END-IF
           ELSE
           IF ELAPSED-DAYS < SP-RENEWABLE-DAYS
              MOVE 99999 TO AW-REV-DEL.

       GET-AVAILABLE-CREDIT.

           IF BR-AVAIL-CREDIT = "Y"
                     AND (LN-CREDITLIM NOT = 0)
                      AND (LN-BNKRPTDATE = 0)
              MOVE HOLD-DATE TO AVCR-PAYDATE
              MOVE LN-MAKERCD(1) TO AVCR-MAKERCD
              PERFORM COMPUTE-AVAILABLE-CREDIT
           ELSE
           IF BR-AVAIL-CREDIT = "S"
                     AND (LN-CREDITLIM NOT = 0)
                      AND (LN-BNKRPTDATE = 0)
              MOVE HOLD-DATE TO AVCR-PAYDATE
              MOVE LN-MAKERCD(1) TO AVCR-MAKERCD
              PERFORM COMPUTE-AVAILABLE-CREDIT
           ELSE
              MOVE 0 TO AVCR-AVAILCREDIT.

           SUBTRACT AVCR-AVAILCREDIT FROM 9999999.99
                                          GIVING AW-REV-PDUE-DAYS.


      *****************************************
       PURGE-CQ-RECORDS SECTION.
      *****************************************
           PERFORM OPEN-CQ1-FILE.

           MOVE BR-NO   TO CQ-BRNO
                           QCQ1-WBEG-BRNO
                           QCQ1-WEND-BRNO.
           MOVE 0       TO CQ-ACCTNO
                           QCQ1-WBEG-ACCTNO.
           MOVE ALL "9" TO QCQ1-WEND-ACCTNO.

           PERFORM START-CQ1-FILE.
       PURGE-CQ-NEXT.
           PERFORM READ-CQ1-FILE-NEXT.
           IF IO-BAD OR (CQ-BRNO NOT = CQ-PATH-OWNBR)
              GO TO END-PURGE-CQ.

      *    IF CQ-TYPE = " "        *> SUMMARY
      *       GO TO DELETE-CQ-RECORD.

           IF CQ-WORKED-FG = "R"   *>DETAIL
              IF REMOVE-RESCHED-FG = "N"
                 GO TO PURGE-CQ-NEXT
              ELSE
                 MOVE "D" TO DATER-UNITPER-CD
                 MOVE 1   TO DATER-UNITPER-FREQ
                 MOVE EXTRDATE TO NDTE-DATE
                 COMPUTE NDTE-HOLD = RESCHED-DAYS * -1
                 PERFORM INCREMENT-UNITPER
                 IF NDTE-DATE NOT > CQ-DATE
                    GO TO PURGE-CQ-NEXT
                 ELSE
                    GO TO DELETE-CQ-RECORD.
           IF REMOVE-UNWORKED-FG = "N"
              IF CQ-WORKED-FG NOT = "Y"
                 GO TO PURGE-CQ-NEXT.
       DELETE-CQ-RECORD.
           PERFORM DELETE-CQ1-FILE.
           GO TO PURGE-CQ-NEXT.
       END-PURGE-CQ.
           PERFORM CLOSE-CQ1-FILE.

      ******************************************************
       PURGE-CQS-RECORDS SECTION.   *> SUMMARY TOTAL RECORDS
      *******************************************************
           PERFORM OPEN-CQS1-FILE.

           MOVE BR-NO   TO CQS-BRNO
                           QCQS1-WBEG-BRNO
                           QCQS1-WEND-BRNO.
           MOVE SPACES  TO CQS-COLLID
                           QCQS1-WBEG-COLLID.
           MOVE ALL "Z" TO QCQS1-WEND-COLLID.
           CALL "C$TOLOWER" USING QCQS1-WEND-COLLID, VALUE 3.

           PERFORM START-CQS1-FILE.
       PURGE-CQS-NEXT.
           PERFORM READ-CQS1-FILE-NEXT.
           IF IO-BAD OR (CQS-BRNO NOT = CQS-PATH-OWNBR)
              GO TO END-PURGE-CQS.

      *    IF CQ-TYPE = " "     
      *       GO TO DELETE-CQ-RECORD.

       DELETE-CQS-RECORD.
           PERFORM DELETE-CQS1-FILE.
           GO TO PURGE-CQS-NEXT.
       END-PURGE-CQS.
           PERFORM CLOSE-CQS1-FILE.


      *****************************************
       INITIALIZE-COLLECTOR-POOLS SECTION.
      *****************************************
           PERFORM OPEN-CP1-FILE.
           PERFORM OPEN-EA1-FILE.

           MOVE BR-NO        TO EA-BRANCH
                                QEA1-WBEG-BRANCH
                                QEA1-WEND-BRANCH.
           MOVE BEG-CLASS    TO EA-CLASS
                                QEA1-WBEG-CLASS.
           MOVE END-CLASS    TO QEA1-WEND-CLASS.
           MOVE BEG-GROUP-ID TO EA-GROUP-ID
                                QEA1-WBEG-GROUP-ID
                                QEA1-WEND-GROUP-ID.
           MOVE 0            TO EA-SEQNO
                                QEA1-WBEG-SEQNO.
           MOVE ALL "9"      TO QEA1-WEND-SEQNO.

           PERFORM START-EA1-FILE.
       CLEAR-CP-NEXT.
           PERFORM READ-EA1-FILE-NEXT.
           IF IO-BAD
              GO TO END-INITIALIZE-POOLS.

           IF EA-BRANCH NOT = BR-NO
              GO TO END-INITIALIZE-POOLS.

           IF EA-CLASS > END-CLASS
              OR EA-GROUP-ID NOT = BEG-GROUP-ID
                 GO TO CLEAR-CP-NEXT.

           MOVE EA-COL-POOL TO CP-ID.
           PERFORM READ-CP1-FILE.
           IF IO-BAD
      *       UNLOCK CP-FILE
              GO TO CLEAR-CP-NEXT.
           MOVE 0 TO SUB COL-COUNT.
       COUNT-COL-LOOP.
           ADD 1 TO SUB.
           IF SUB > 80
              GO TO EXIT-CP-LOOP.
           IF CP-COL-ID(SUB) NOT = SPACES
              ADD 1 TO COL-COUNT
              GO TO COUNT-COL-LOOP.
       EXIT-CP-LOOP.
           MOVE COL-COUNT TO CP-COLLECTOR-COUNT.
           MOVE 0 TO CP-LAST-SUBSCRIPT.
           PERFORM REWRITE-CP1-FILE.
           GO TO CLEAR-CP-NEXT.
       END-INITIALIZE-POOLS.
           PERFORM CLOSE-EA1-FILE.
           PERFORM CLOSE-CP1-FILE.

      *****************************************
       PURGE-GQS-SUMMARY SECTION.  *>SUMMARY GLOBAL QUEUE RECORDS 
      *****************************************
           MOVE SPACES TO GQS1-KEY.

           PERFORM START-GQS1-FILE.
       NEXT-GQS-SUMMARY.
           PERFORM READ-GQS1-FILE-NEXT.
           IF IO-BAD
              GO TO EXIT-PURGE-GQS-SUMMARY.

           PERFORM DELETE-GQS1-FILE.

           GO TO NEXT-GQS-SUMMARY.
       EXIT-PURGE-GQS-SUMMARY.
           EXIT.

      *****************************************
       PURGE-GQ-RECORDS SECTION.
      *****************************************
       SETUP-GQ-DETAIL-KEY.
           MOVE BR-NO   TO GQ-BRNO
                           QGQ1-WBEG-BRNO
                           QGQ1-WEND-BRNO.
           MOVE 0       TO GQ-ACCTNO
                           QGQ1-WBEG-ACCTNO.
           MOVE ALL "9" TO QGQ1-WEND-ACCTNO.

           PERFORM START-GQ1-FILE.
       PURGE-GQ-NEXT.
           PERFORM READ-GQ1-FILE-NEXT.
           IF IO-BAD
              GO TO END-PURGE-GQ.

           IF GQ-BRNO NOT = BR-NO
              GO TO END-PURGE-GQ.

           IF GQ-WORKED-FG = "R"
              IF REMOVE-RESCHED-FG = "N"
                 GO TO PURGE-GQ-NEXT
              ELSE
                 MOVE "D" TO DATER-UNITPER-CD
                 MOVE 1 TO DATER-UNITPER-FREQ
                 MOVE EXTRDATE TO NDTE-DATE
                 COMPUTE NDTE-HOLD = RESCHED-DAYS * -1
                 PERFORM INCREMENT-UNITPER
                 IF NDTE-DATE NOT > GQ-DATE
                    GO TO PURGE-GQ-NEXT
                 ELSE
                    GO TO DELETE-GQ-RECORD.
           IF REMOVE-UNWORKED-FG = "N"
              IF GQ-WORKED-FG NOT = "Y"
                 GO TO PURGE-GQ-NEXT.
       DELETE-GQ-RECORD.
           PERFORM DELETE-GQ1-FILE.
           GO TO PURGE-GQ-NEXT.
       END-PURGE-GQ.
           EXIT.

      * 12/19/00 - NOT SURE THIS IS COMPLETELY CORRECT.  THIS IS CLEARING
      * GLOBAL CPFILE, SO CHECK BEG-BRANCH & END-BRANCH IN READ-EA LOOP.
      * IF RUNNING BY GROUP, THIS WILL INCLUDE EA'S FROM ALL BRANCHES
      * WITH MATCHING GROUP.  IF THIS IS REALLY A PROBLEM, COULD ADD CHECK-
      * MEMBER TO THIS ROUTINE.  ALSO, GOOD CHANCE OF CLEARING SAME CP
      * MANY TIMES, BUT CAN'T COME UP WITH A GOOD CHECK TO STOP THIS,
      * BUT IT SHOULDN'T HURT, JUST SOME EXTRA IO.  FOR THIS GLOBAL
      * CLEAR I CONSIDERED JUST LOOPING THROUGH THE ENTIRE GLOBAL CPFILE
      * BUT TECHNICALLY SOMEONE COULD BE RUNNING ANOTHER GLOBAL RUN AT
      * THE SAME TIME.....                             BARB
      *****************************************
       GLOBAL-INIT-COLLECTOR-POOLS SECTION.
      *****************************************

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    NEED TO SET CP-GLOBAL-PATH HERE (CP-PATH-OVERRIDE EQUALS "G")
      *    BECAUSE THIS IS EXECUTED PRIOR TO THE STANDARD BRANCH/GROUP
      *    LOGIC BEING DONE.  SET AGAIN IN BRANCH/GROUP LOGIC.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE EXT-FILPATH-BASE           TO CP-GLOBAL-BASE.
           MOVE EXT-FILPATH-MACHINE        TO CP-GLOBAL-MACHINE.
           MOVE "G"                        TO CP-PATH-OVERRIDE.
           MOVE CP-GLOBAL-PATH             TO CP-PATH.

           PERFORM OPEN-CP1-FILE.

           MOVE BEG-BRANCH   TO EA-BRANCH
                                QEA1-WBEG-BRANCH.
           MOVE END-BRANCH   TO QEA1-WEND-BRANCH.
           MOVE BEG-CLASS    TO EA-CLASS
                                QEA1-WBEG-CLASS.
           MOVE END-CLASS    TO QEA1-WEND-CLASS.
           MOVE BEG-GROUP-ID TO EA-GROUP-ID
                                QEA1-WBEG-GROUP-ID
                                QEA1-WEND-GROUP-ID.
           MOVE 0            TO EA-SEQNO
                                QEA1-WBEG-SEQNO.
           MOVE ALL "9"      TO QEA1-WEND-SEQNO.

           PERFORM START-EA1-FILE.
       GLOBAL-CLEAR-CP-NEXT.
           PERFORM READ-EA1-FILE-NEXT.
           IF IO-BAD
              GO TO GLOBAL-END-INITIALIZE-POOLS.

           IF EA-BRANCH > END-BRANCH
              GO TO GLOBAL-END-INITIALIZE-POOLS.
           IF EA-CLASS > END-CLASS
              OR EA-GROUP-ID NOT = BEG-GROUP-ID
                 GO TO GLOBAL-CLEAR-CP-NEXT.

           MOVE EA-COL-POOL TO CP-ID.
           PERFORM READ-CP1-FILE.
           IF IO-BAD
      *       UNLOCK CP-FILE
              GO TO GLOBAL-CLEAR-CP-NEXT.
           MOVE 0 TO SUB COL-COUNT.
       GLOBAL-COUNT-COL-LOOP.
           ADD 1 TO SUB.
           IF SUB > 80
              GO TO GLOBAL-EXIT-CP-LOOP.
           IF CP-COL-ID(SUB) NOT = SPACES
              ADD 1 TO COL-COUNT
              GO TO GLOBAL-COUNT-COL-LOOP.
       GLOBAL-EXIT-CP-LOOP.
           MOVE COL-COUNT TO CP-COLLECTOR-COUNT.
           MOVE 0 TO CP-LAST-SUBSCRIPT.
           MOVE 0 TO CP-TOTAL-ACCOUNTS.
           PERFORM REWRITE-CP1-FILE.
           GO TO GLOBAL-CLEAR-CP-NEXT.
       GLOBAL-END-INITIALIZE-POOLS.
           PERFORM CLOSE-CP1-FILE.

      *****************************************
       CREATE-CQS-RECORD SECTION.
      *****************************************
           MOVE "Y" TO FIRST-TIME-CQ.

           MOVE 0 TO TOTAL-CQS-RECS TOTAL-CQS-TIMED.

           PERFORM OPEN-CQ1-FILE.
           PERFORM OPEN-CQS1-FILE.

           MOVE CQ-PATH-OWNBR TO CQ-BRNO.
           MOVE SPACES        TO CQ-COLLID CQ-SEQ-FG CQ-MISC-KEY-FLD.
           MOVE 19000101      TO CQ-DATE.
           MOVE 0             TO CQ-START-TIME CQ-REV-DEL
                                 CQ-REV-PDUE-DAYS CQ-REV-BAL CQ-SEQ
                                 CQ-REV-BEHAVIOR-RISK.

           PERFORM START-CQ2-FILE.
       GET-NEXT-CQ.
           PERFORM READ-CQ2-FILE-NEXT.
           IF IO-BAD OR (CQ-BRNO NOT = CQ-PATH-OWNBR)
              PERFORM WRITE-CQS-RECORD
              GO TO END-CREATE-CQS-RECORD.

           IF FIRST-TIME-CQ = "Y"
      *       MOVE CQ2-KEY TO HOLD-CQ2-KEY
              PERFORM SAVE-CQ2-KEY
              MOVE "N" TO FIRST-TIME-CQ.

      *    IF CQ-COLLID NOT = HOLD-CQ-COLLID
           IF CQ-COLLID NOT = SPWSCQ-CQ2-COLLID
      *%%% CANT SAVE THIS HERE BECAUSE WE NEED THE PRIOR COLLID
*FLR%%*       PERFORM SAVE-CQ2-KEY     *> MOVE CQ2-KEY TO SPWSCQ-CQ2-KEY
              PERFORM WRITE-CQS-RECORD
              GO TO GET-NEXT-CQ.
           ADD 1 TO TOTAL-CQS-RECS.
      * 8/17/00 - IF QUEUE ORDERED BY TIME ZONE, CQ-START-TIME MAY
      *           NOW CONTAIN TIME ZONE FACTOR, SO A LOW VALUE IS
      *           JUST A NORMAL RECORD, HIGHER VALUE IS A RESCHEDULE
           IF CQ-START-TIME > 24
              ADD 1 TO TOTAL-CQS-TIMED.
           GO TO GET-NEXT-CQ.
       END-CREATE-CQS-RECORD.
           PERFORM CLOSE-CQ1-FILE.
           PERFORM CLOSE-CQS1-FILE.

      *****************************************
       WRITE-CQS-RECORD SECTION.
      *****************************************
           IF TOTAL-CQS-RECS = 0
              GO TO EXIT-WRITE-CQS-RECORD.

           MOVE SPWSCQ-CQ2-COLLID TO SAVE-COLLID.
           PERFORM SAVE-CQ2-KEY.

           MOVE SPACES      TO CQS-REC.

           MOVE BR-NO       TO CQS-BRNO   CQS-CQ-BRNO.
           MOVE SAVE-COLLID TO CQS-COLLID CQS-CQ-COLLID.

           MOVE 0 TO CQS-CQ-REV-BEHAVIOR-RISK CQS-CQ-DATE
                     CQS-CQ-START-TIME
                     CQS-CQ-REV-DEL CQS-CQ-REV-PDUE-DAYS CQS-CQ-REV-BAL
                     CQS-WORKED-REC CQS-CQ-SEQ.

           MOVE TOTAL-CQS-RECS  TO CQS-TOT-REC.
           MOVE TOTAL-CQS-TIMED TO CQS-TIMED-REC.
           PERFORM WRITE-CQS1-FILE.
           IF IO-BAD
              GO TO IO-ERROR.

           MOVE 0 TO TOTAL-CQS-RECS TOTAL-CQS-TIMED.

           PERFORM LOAD-CQ2-KEY.
           IF CQ-DATE = 0
              MOVE 19000101 TO CQ-DATE.

           IF CQ-COLLID = "BK"
              MOVE SPACES TO MESS.

           PERFORM START-CQ2-FILE.

       EXIT-WRITE-CQS-RECORD.
           EXIT.

      *****************************************
       CREATE-GQS-RECORD SECTION.   *>GQS
      *****************************************
           MOVE "Y" TO GQ-FIRST-TIME.

           PERFORM OPEN-GQS1-FILE.

           MOVE 0 TO TOTAL-GQS-RECS TOTAL-GQS-TIMED.

           MOVE SPACES   TO GQ-COLLID.
           MOVE 19000101 TO GQ-DATE.
           MOVE 0        TO GQ-START-TIME GQ-REV-DEL
                            GQ-REV-PDUE-DAYS GQ-REV-BAL GQ-BRNO
                            GQ-SEQ.
           PERFORM START-GQ2-FILE.
       GET-NEXT-GQ.
           PERFORM READ-GQ2-FILE-NEXT.
           IF IO-BAD
              PERFORM WRITE-GQS-RECORD      
              GO TO END-CREATE-GQS-RECORD.

           IF GQ-FIRST-TIME = "Y"
              PERFORM SAVE-GQ2-KEY
              MOVE "N" TO GQ-FIRST-TIME.
           IF GQ-COLLID NOT = SPWSGQ-GQ2-COLLID
              PERFORM WRITE-GQS-RECORD     
              GO TO GET-NEXT-GQ.
           ADD 1 TO TOTAL-GQS-RECS.
      * 8/17/00 - IF QUEUE ORDERED BY TIME ZONE, GQ-START-TIME MAY
      *           NOW CONTAIN TIME ZONE FACTOR, SO A LOW VALUE IS
      *           JUST A NORMAL RECORD, HIGHER VALUE IS A RESCHEDULE
           IF GQ-START-TIME > 24
              ADD 1 TO TOTAL-GQS-TIMED.
           GO TO GET-NEXT-GQ.
       END-CREATE-GQS-RECORD.
           PERFORM CLOSE-GQ1-FILE.
           PERFORM CLOSE-GQS1-FILE.
      *****************************************
       WRITE-GQS-RECORD SECTION.
      *****************************************
      D    STOP MESS.
           IF TOTAL-GQS-RECS = 0
              GO TO EXIT-WRITE-GQS-RECORD.
           MOVE SPWSGQ-GQ2-COLLID TO SAVE-COLLID.
           PERFORM SAVE-GQ2-KEY.
           MOVE SPACES TO GQS-REC.
           MOVE SAVE-COLLID TO GQS-COLLID GQ-COLLID.
           MOVE 0 TO GQS-GQ-DATE GQS-GQ-START-TIME
                      GQS-GQ-REV-DEL GQS-GQ-REV-PDUE-DAYS
                      GQS-GQ-REV-BAL GQS-WORKED-REC  GQS-GQ-SEQ.

           MOVE TOTAL-GQS-RECS TO GQS-TOT-REC.
           MOVE TOTAL-GQS-TIMED TO GQS-TIMED-REC.
           PERFORM WRITE-GQS1-FILE.
           IF IO-BAD
              GO TO IO-ERROR.

           MOVE 0 TO TOTAL-GQS-RECS TOTAL-GQS-TIMED.
       RESTART-GQ.
           PERFORM LOAD-GQ2-KEY.
           IF GQ-DATE = 0
              MOVE 19000101 TO GQ-DATE.

           PERFORM START-GQ2-FILE.
       EXIT-WRITE-GQS-RECORD.
           EXIT.

      *****************************************
       CODE-SCAN SECTION.
      *****************************************
           MOVE "EA"       TO CDSCAN-TYPE.
           MOVE SPACES     TO CDSCAN-CODE.
           MOVE "Y"        TO CDSCAN-EA-CQ-FG.
           MOVE CDSCAN-BUF TO SCAN-BUF.
           PERFORM CALL-WINDOW-PROGRAM.
           MOVE SCAN-BUF   TO CDSCAN-BUF.

           IF ( SCAN-STATUS NOT = "00" ) OR ( CDSCAN-CODE = " " )
               MOVE 9 TO IO-FG
               GO TO EXIT-CODE-SCAN.

           MOVE "S  A060GROUP-ID." TO VDU
           MOVE CDSCAN-CODE       TO VDUBUF
           PERFORM SCREENIO.

           MOVE CDSCAN-CODE TO VDUBUF.
           MOVE 0           TO IO-FG.

       EXIT-CODE-SCAN.
           EXIT.


      ******************************************************************
      *
      *    C O P Y - L I B R A R Y - S E C T I O N S
      *
      ******************************************************************
       COPY "LIBLP/LPCPUPD.CPY".
       COPY "LIBWI/PFSWINDOWS.CPY".
       COPY "LIBLP/LPRATE.CPY".
       COPY "LIBLP/LPPDUE.CPY".
       COPY "LIBLP/LPCDEL.CPY".
       COPY "LIBLP/LPTDEL.CPY".
       COPY "LIBGB/SPPDTH.CPY".
       COPY "LIBGB/GPENV.CPY".
       COPY "LIBLP/LPSCHD.CPY".
       COPY "LIBLP/LPLCAS.CPY".
       COPY "LIBLP/LPLCHG.CPY".
       COPY "LIBLP/LPLCAP.CPY".
       COPY "LIBLP/DEFPOL.CPY".
       COPY "LIBLP/LPPOFF.CPY".
       COPY "LIBLP/LPPOF2.CPY".
       COPY "LIBLP/LPITRM.CPY".
       COPY "LIBLP/LPAVCR.CPY".
       COPY "LIBGB/REBATE.CPY".
       COPY "LIBLP/LPAPRS.CPY".
       COPY "LIBLP/LPAPRZ.CPY".
       COPY "LIBLP/LPUPER.CPY".
       COPY "LIBLP/LPMDTE.CPY".
       COPY "LIBLP/LPXDTE.CPY".
       COPY "LIBLP/LPIBPC.CPY".
       COPY "LIBLP/LPAMTS.CPY".
       COPY "LIBGB/DATER.CPY".
       COPY "LIBLP/SETCTBL.CPY".
       COPY "LIBGB/AGEING.CPY".
       COPY "LIBLP/LPUPWK.CPY".
       COPY "LIBLP/LPCEPP.CPY".
       COPY "LIBLP/LPFMCL.CPY".
       COPY "LIBSP/SPGQCL.CPY".
       COPY "LIBSP/SPCQCL.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/RBACT.CPY".
       COPY "LIBLP/ADDONSPR.CPY".
       COPY "LIBGB/GRWORK.CPY".
       COPY "LIBGB/GRSERCH.CPY".
       COPY "LIBGB/BRSECURE.CPY".
       COPY "LIBGB/GETDLS.CPY".

      *********************************************************
       FORM-RESET SECTION.
      *********************************************************
           MOVE EXT-CONAME-CONAME TO CQRUN-CONAME.
           MOVE EXT-CONAME-SYSDATE TO CQRUN-SYSDATE.
           DISPLAY CQRUN-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE CQRUN-FORM-FIELDS TO WR-BUF.
           MOVE CQRUN-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.
       FORM-RESET-EXIT.
           EXIT.

      *********************************************************
       SETUP-LINK-INFO SECTION.
      *********************************************************
           MOVE CQRUN-HELP--FILE TO HELP-LINK-FILE.
           MOVE CQRUN-HELP--DIR  TO HELP-LINK-DIR.

      *********************************************************
       VDU-CONVERT-FIELD SECTION.
      *********************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBLP/CQRUN_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      *********************************************************
       MISC-ROUTINES SECTION.
      *********************************************************
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/GETENV.CPY".
       COPY "LIBGB/GETPR.CPY".

       READ-SP-RECORD.
           MOVE LN-ORGST    TO SP-ORGST.
           MOVE LN-SPRCLASS TO SP-SPRCLASS.
           MOVE LN-SUBCLASS TO SP-SUBCLASS.
           MOVE LN-LAWCODE  TO SP-LAWCODE.
           IF SP1-KEY NOT = LPWSSP-SP1-KEY
              PERFORM READ-SP1-FILE
              IF IO-BAD
                 PERFORM FILE-ERRORS
                 PERFORM SEND-MESS
                 MOVE SPACES TO LPWSSP-SP1-KEY
                 MOVE "NL" TO RTN-FG
              ELSE
                 MOVE SP1-KEY TO LPWSSP-SP1-KEY.

      *=================================================================
       GET-MAKER.
           MOVE 0 TO IO-FG.

           IF LN-SSNO(1) NOT = HOLD-BW-SSNO
              MOVE LN-SSNO(1) TO BW-SSNO HOLD-BW-SSNO
              PERFORM READ-BW1-FILE
              IF IO-BAD
                 MOVE ZEROES TO BW-SCORE-TABLE
                 MOVE SPACES TO BW-ZIP
                 MOVE "A" TO BW-LNAME.

      *-----------------------------------------------------------------
       GET-BORROWER.

           MOVE LN-SSNO(SUB) TO BW-SSNO.
           PERFORM READ-BW1-FILE.
           IF IO-BAD
              MOVE SPACES TO BW-ZIP
              MOVE ZEROES TO BW-SCORE-TABLE
              MOVE 0 TO BW-PHONE BW-EMPPHONE BW-CELL-PHONE.

      ******************************************************************
       IO-ROUTINE SECTION.
      ******************************************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBSP/SPGQSGS_SQL.CPY".
       COPY "LIBSP/SPGQGS_SQL.CPY".
       COPY "LIBSP/SPCQSGS_SQL.CPY".
       COPY "LIBSP/SPCQGS_SQL.CPY".
       COPY "LIBSP/SPCPGS_SQL.CPY".
       COPY "LIBLP/LPTZGS_SQL.CPY".
       COPY "LIBLP/LPSPGS_SQL.CPY".
       COPY "LIBLP/LPNOGS_SQL.CPY".
       COPY "LIBLP/LPLTPGS_SQL.CPY".
       COPY "LIBLP/LPLTIGS_SQL.CPY".
       COPY "LIBLP/LPLPGS_SQL.CPY".
       COPY "LIBLP/LPLNGS_SQL.CPY".
       COPY "LIBLP/LPFMGS_SQL.CPY".
       COPY "LIBLP/LPFDGS_SQL.CPY".
       COPY "LIBLP/LPEAGS_SQL.CPY".
       COPY "LIBLP/LPCDGS_SQL.CPY".
       COPY "LIBLP/LPBWGS_SQL.CPY".
       COPY "LIBGB/GBGRGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBGBGS_SQL.CPY".
       COPY "LIBGB/GBGR1RN.CPY".
       COPY "LIBGB/GBPRINT.CPY".
       COPY "LIBLP/LPLN3RN.CPY".
       COPY "LIBLP/LPEA1RN.CPY".
       COPY "LIBLP/LPSP1RN.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBGB/GBGBRN.CPY".
       COPY "LIBLP/LPBW1RN.CPY".
       COPY "LIBLP/LPFD1RN.CPY".
       COPY "LIBLP/LPLTI1RN.CPY".
       COPY "LIBLP/LPLTP1RN.CPY".
       COPY "LIBSP/SPCQ1IN.CPY".
       COPY "LIBSP/SPCQS1IN.CPY".
       COPY "LIBSP/SPGQ1IN.CPY".
       COPY "LIBSP/SPGQS1IN.CPY".
       COPY "LIBSP/SPCP1IN.CPY".

      *COPY "LIBSP/SPCQ2RN.CPY".
      * 190128 ADDED LOAD-CQ2-NON-COMP-KEY.CQ-START-TIME IS COMP3.
      * COPYMEMBER: LIBSP/SPCQ2RN
      *20210506 REMOVE SPWSCQ-CQ2-FUTURE-FIELD1 AND SPWSCQ-CQ2-FUTURE-FIELD2
      ***********************************************************************
      *-----------------------------------------------------------------
      *    USED FOR CREATING SPWSCQ-CQ2-KEY-NON-COMP GROUP FIELD;
      *    NEEDED FOR POPULATING E-KEYX; CANNOT STRING ALL CQ2-KEY
      *    FIELDS INTO E-KEYX BECAUSE CANNOT STRING CQ-START-TIME
      *    BECAUSE IT CONTAINS A COMP-3 (STRING CANNOT HANDLE COMP).
      *-----------------------------------------------------------------
       LOAD-CQ2-NON-COMP-KEY.
           MOVE CQ-BRNO                 TO SPWSCQ-CQ2-BRNO-NC.
           MOVE CQ-COLLID               TO SPWSCQ-CQ2-COLLID-NC.
           MOVE CQ-REV-BEHAVIOR-RISK TO SPWSCQ-CQ2-REV-BEHAVIOR-RISK-NC.
           MOVE CQ-SEQ-FG               TO SPWSCQ-CQ2-SEQ-FG-NC.
           MOVE CQ-DATE                 TO SPWSCQ-CQ2-DATE-NC.
           MOVE CQ-START-TIME           TO SPWSCQ-CQ2-START-TIME-NC.
           MOVE CQ-REV-DEL              TO SPWSCQ-CQ2-REV-DEL-NC.
           MOVE CQ-MISC-KEY-FLD         TO SPWSCQ-CQ2-MISC-KEY-FLD-NC.
           MOVE CQ-REV-PDUE-DAYS        TO SPWSCQ-CQ2-REV-PDUE-DAYS-NC.
           MOVE CQ-REV-BAL              TO SPWSCQ-CQ2-REV-BAL-NC.
           MOVE CQ-SEQ                  TO SPWSCQ-CQ2-SEQ-NC.

      *-----------------------------------------------------------------
      *    USED TO CLEAR (SPACES/ZEROES) ALL CQ2-KEY FIELDS.
      *-----------------------------------------------------------------
       CLEAR-CQ2-KEY.
           MOVE ZEROES                       TO CQ-BRNO.
           MOVE SPACES                       TO CQ-COLLID.
           MOVE ZEROES                       TO CQ-REV-BEHAVIOR-RISK.
           MOVE SPACES                       TO CQ-SEQ-FG.
           MOVE ZEROES                       TO CQ-DATE.
           MOVE ZEROES                       TO CQ-START-TIME.
           MOVE ZEROES                       TO CQ-REV-DEL.
           MOVE SPACES                       TO CQ-MISC-KEY-FLD.
           MOVE ZEROES                       TO CQ-REV-PDUE-DAYS.
           MOVE ZEROES                       TO CQ-REV-BAL.
           MOVE ZEROES                       TO CQ-SEQ.

      *-----------------------------------------------------------------
      *    USED TO LOAD THE CQ2-KEY FIELDS FROM THE HOLD-CQ2-KEY OR
      *    SAVE-CQ2-KEY FIELDS (SPWSCQ-CQ2-KEY).
      *    SPWSCQ-CQ2-KEY FIELDS ARE USED TO POPULATE CQ2-KEY FIELDS.
      *    PSEUDO CODE:  MOVE SPWSCQ-CQ2-KEY TO CQ2-KEY.
      *-----------------------------------------------------------------
       LOAD-CQ2-KEY.
           MOVE SPWSCQ-CQ2-BRNO              TO CQ-BRNO.
           MOVE SPWSCQ-CQ2-COLLID            TO CQ-COLLID.
           MOVE SPWSCQ-CQ2-REV-BEHAVIOR-RISK TO CQ-REV-BEHAVIOR-RISK.
           MOVE SPWSCQ-CQ2-SEQ-FG            TO CQ-SEQ-FG.
           MOVE SPWSCQ-CQ2-DATE              TO CQ-DATE.
           MOVE SPWSCQ-CQ2-START-TIME        TO CQ-START-TIME.
           MOVE SPWSCQ-CQ2-REV-DEL           TO CQ-REV-DEL.
           MOVE SPWSCQ-CQ2-MISC-KEY-FLD      TO CQ-MISC-KEY-FLD.
           MOVE SPWSCQ-CQ2-REV-PDUE-DAYS     TO CQ-REV-PDUE-DAYS.
           MOVE SPWSCQ-CQ2-REV-BAL           TO CQ-REV-BAL.
           MOVE SPWSCQ-CQ2-SEQ               TO CQ-SEQ.

      *-----------------------------------------------------------------
      *    USED TO SAVE THE CQ2-KEY FIELDS TO THE HOLD-CQ2-KEY OR
      *    SAVE-CQ2-KEY FIELDS (SPWSCQ-CQ2-KEY).
      *    CQ2-KEY FIELDS ARE USED TO POPULATE SPWSCQ-CQ2-KEY FIELDS.
      *    PSEUDO CODE:  MOVE CQ2-KEY TO SPWSCQ-CQ2-KEY.
      *-----------------------------------------------------------------
       SAVE-CQ2-KEY.
           MOVE CQ-BRNO                 TO SPWSCQ-CQ2-BRNO.
           MOVE CQ-COLLID               TO SPWSCQ-CQ2-COLLID.
           MOVE CQ-REV-BEHAVIOR-RISK TO SPWSCQ-CQ2-REV-BEHAVIOR-RISK.
           MOVE CQ-SEQ-FG               TO SPWSCQ-CQ2-SEQ-FG.
           MOVE CQ-DATE                 TO SPWSCQ-CQ2-DATE.
           MOVE CQ-START-TIME           TO SPWSCQ-CQ2-START-TIME.
           MOVE CQ-REV-DEL              TO SPWSCQ-CQ2-REV-DEL.
           MOVE CQ-MISC-KEY-FLD         TO SPWSCQ-CQ2-MISC-KEY-FLD.
           MOVE CQ-REV-PDUE-DAYS        TO SPWSCQ-CQ2-REV-PDUE-DAYS.
           MOVE CQ-REV-BAL              TO SPWSCQ-CQ2-REV-BAL.
           MOVE CQ-SEQ                  TO SPWSCQ-CQ2-SEQ.

      *------------------------------------------------------------------
       LOAD-CQ2-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( CQ-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( CQ-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO CQ-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO CQ-ALL-MACHINE
              MOVE EXT-FILPATH-DATA        TO CQ-ALL-DATA
              MOVE CQ-ALL-PATH             TO CQ-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( CQ-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO CQ-ALL-BASE
              MOVE FILPATH-MACHINE         TO CQ-ALL-MACHINE
              MOVE FILPATH-DATA            TO CQ-ALL-DATA
              MOVE CQ-ALL-PATH             TO CQ-PATH.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    CREATE PATH FOR FILE SELECT AND SQL TABLE NAME
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( EXT-IO-CQFILE NOT = "S" )
              MOVE CQ-PATH-BEG             TO CQ-SELECT-PATH-BEG
              MOVE CQ-PATH-FILE            TO CQ-SELECT-PATH-FILE
           ELSE
              MOVE CQ-PATH-SQL             TO CQ-SELECT-PATH.


      *-----------------------------------------------------------------
       OPEN-CQ2-FILE.
           PERFORM LOAD-CQ2-FILE.
           PERFORM OPEN-IT.
           MOVE CQ-SELECT-PATH TO E-FILE.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       START-CQ2-FILE.
           PERFORM START-IT.
           MOVE CQ-PATH-OWNBR  TO CQ-BRNO.
           MOVE CQ-SELECT-PATH TO E-FILE.
           PERFORM LOAD-CQ2-NON-COMP-KEY.
           MOVE SPWSCQ-CQ2-KEY-NON-COMP TO E-KEYX.

           IF ( CQ2-CURSOR-STAT-GOOD )
              PERFORM CLOSE-CQ2-FILE.

           IF ( QCQ2-WEND-SEQ = ZEROES    ) OR
              ( QCQ2-WEND-SEQ NOT NUMERIC )
              MOVE CQ-PATH-OWNBR     TO QCQ2-WBEG-BRNO
              MOVE CQ-PATH-OWNBR     TO QCQ2-WEND-BRNO

              MOVE CQ-COLLID         TO QCQ2-WBEG-COLLID
              MOVE CQ-REV-BEHAVIOR-RISK TO
                                     QCQ2-WBEG-REV-BEHAVIOR-RISK
              MOVE CQ-SEQ-FG           TO QCQ2-WBEG-SEQ-FG
              MOVE CQ-DATE             TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QCQ2-WBEG-DATE
              MOVE JULIAN-END          TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QCQ2-WEND-DATE
              MOVE CQ-START-TIME       TO QCQ2-WBEG-START-TIME
              MOVE CQ-REV-DEL          TO QCQ2-WBEG-REV-DEL
              MOVE CQ-MISC-KEY-FLD     TO QCQ2-WBEG-MISC-KEY-FLD
              MOVE CQ-REV-PDUE-DAYS    TO QCQ2-WBEG-REV-PDUE-DAYS
              MOVE CQ-REV-BAL          TO QCQ2-WBEG-REV-BAL
              MOVE CQ-SEQ              TO QCQ2-WBEG-SEQ

              MOVE ALL "Z"         TO QCQ2-WEND-COLLID
                                      QCQ2-WEND-SEQ-FG
                                      QCQ2-WEND-MISC-KEY-FLD
              MOVE ALL "9"         TO QCQ2-WEND-REV-BEHAVIOR-RISK
                                      QCQ2-WEND-START-TIME
                                      QCQ2-WEND-REV-DEL
                                      QCQ2-WEND-REV-PDUE-DAYS
                                      QCQ2-WEND-REV-BAL
                                      QCQ2-WEND-SEQ
              CALL "C$TOLOWER" USING QCQ2-WEND-COLLID, VALUE 3
              CALL "C$TOLOWER" USING QCQ2-WEND-SEQ-FG, VALUE 1
      *- - - -{ ADD ONE TO WHERE SCAN CONTINUE FIELDS
              MOVE 03                   TO SQL-TEXT-SIZE
              MOVE CQ-COLLID            TO SQL-TEXT
              PERFORM SQL-TEXT-INCREMENT
              MOVE SQL-TEXT             TO QCQ2-WSBEG-COLLID

              IF ( CQ-REV-BEHAVIOR-RISK = 9 )
                 MOVE CQ-REV-BEHAVIOR-RISK TO
                                     QCQ2-WSBEG-REV-BEHAVIOR-RISK
              ELSE
                 COMPUTE QCQ2-WSBEG-REV-BEHAVIOR-RISK =
                         CQ-REV-BEHAVIOR-RISK + 1
              END-IF

              MOVE 01                   TO SQL-TEXT-SIZE
              MOVE CQ-SEQ-FG            TO SQL-TEXT
              PERFORM SQL-TEXT-INCREMENT
              MOVE SQL-TEXT             TO QCQ2-WSBEG-SEQ-FG

              MOVE CQ-DATE              TO NDTE-DATE
              MOVE 1                    TO NDTE-HOLD
              PERFORM INCREMENT-DAYS
              MOVE NDTE-DATE            TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD  TO QCQ2-WSBEG-DATE

              COMPUTE QCQ2-WSBEG-START-TIME = CQ-START-TIME + 1

              IF ( CQ-REV-DEL = 99999 )
                 MOVE CQ-REV-DEL        TO QCQ2-WSBEG-REV-DEL
              ELSE
                 COMPUTE QCQ2-WSBEG-REV-DEL = CQ-REV-DEL + 1
              END-IF

              MOVE 03                   TO SQL-TEXT-SIZE
              MOVE CQ-MISC-KEY-FLD      TO SQL-TEXT
              PERFORM SQL-TEXT-INCREMENT
              MOVE SQL-TEXT             TO QCQ2-WSBEG-MISC-KEY-FLD

              COMPUTE QCQ2-WSBEG-REV-PDUE-DAYS = CQ-REV-PDUE-DAYS + 1
              COMPUTE QCQ2-WSBEG-REV-BAL = CQ-REV-BAL + 1

              IF ( CQ-SEQ  = 9 )
                 MOVE CQ-SEQ            TO QCQ2-WSBEG-SEQ
              ELSE
                 COMPUTE QCQ2-WSBEG-SEQ = CQ-SEQ + 1.


           EXEC SQL
            DECLARE CQ2_CURSOR CURSOR FOR
            SELECT CQFILE.CQ_BRNO,
                   CQFILE.CQ_ACCTNO,
                   ISNULL(CQFILE.CQ_COLLID,''),
                   CQFILE.CQ_REV_BEHAVIOR_RISK,
                   ISNULL(CQFILE.CQ_SEQ_FG,''),
                   ISNULL(CAST(CQFILE.CQ_DATE AS VARCHAR(10)),'0'),
                   CQFILE.CQ_START_TIME,
                   CQFILE.CQ_REV_DEL,
                   ISNULL(CQFILE.CQ_MISC_KEY_FLD,''),
                   CQFILE.CQ_REV_PDUE_DAYS,
                   CQFILE.CQ_REV_BAL,
                   CQFILE.CQ_SEQ,
                   ISNULL(CAST(CQFILE.CQ_LTOUCH_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CQFILE.CQ_WORK_COLLID,''),
                   ISNULL(CQFILE.CQ_WORKED_FG,''),
                   ISNULL(CQFILE.CQ_IN_PROCESS,''),
                   ISNULL(CAST(CQFILE.CQ_DATE_QUEUED
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CQFILE.CQ_TIME_ZONE_ERROR,'')
            FROM DBO.CQFILE  
            WHERE (( CQFILE.CQ_BRNO   >= :QCQ2-WBEG-BRNO
              AND    CQFILE.CQ_COLLID >= :QCQ2-WBEG-COLLID
              AND    CQFILE.CQ_REV_BEHAVIOR_RISK
                                         >= :QCQ2-WBEG-REV-BEHAVIOR-RISK
              AND    CQFILE.CQ_SEQ_FG    >= :QCQ2-WBEG-SEQ-FG
              AND    CQFILE.CQ_DATE      >= :QCQ2-WBEG-DATE
              AND    CQFILE.CQ_START_TIME >= :QCQ2-WBEG-START-TIME
              AND    CQFILE.CQ_REV_DEL   >= :QCQ2-WBEG-REV-DEL
              AND    CQFILE.CQ_MISC_KEY_FLD >= :QCQ2-WBEG-MISC-KEY-FLD
              AND    CQFILE.CQ_REV_PDUE_DAYS >= :QCQ2-WBEG-REV-PDUE-DAYS
              AND    CQFILE.CQ_REV_BAL   >= :QCQ2-WBEG-REV-BAL
              AND    CQFILE.CQ_SEQ       >= :QCQ2-WBEG-SEQ            )
               OR  ( CQFILE.CQ_BRNO   >= :QCQ2-WBEG-BRNO
              AND    CQFILE.CQ_COLLID >= :QCQ2-WBEG-COLLID
              AND    CQFILE.CQ_REV_BEHAVIOR_RISK
                                         >= :QCQ2-WBEG-REV-BEHAVIOR-RISK
              AND    CQFILE.CQ_SEQ_FG    >= :QCQ2-WBEG-SEQ-FG
              AND    CQFILE.CQ_DATE      >= :QCQ2-WBEG-DATE
              AND    CQFILE.CQ_START_TIME >= :QCQ2-WBEG-START-TIME
              AND    CQFILE.CQ_REV_DEL   >= :QCQ2-WBEG-REV-DEL
              AND    CQFILE.CQ_MISC_KEY_FLD >= :QCQ2-WBEG-MISC-KEY-FLD
              AND    CQFILE.CQ_REV_PDUE_DAYS >= :QCQ2-WBEG-REV-PDUE-DAYS
              AND    CQFILE.CQ_REV_BAL   >= :QCQ2-WSBEG-REV-BAL
              AND    CQFILE.CQ_SEQ       >= 0                         )
               OR  ( CQFILE.CQ_BRNO   >= :QCQ2-WBEG-BRNO
              AND    CQFILE.CQ_COLLID >= :QCQ2-WBEG-COLLID
              AND    CQFILE.CQ_REV_BEHAVIOR_RISK
                                         >= :QCQ2-WBEG-REV-BEHAVIOR-RISK
              AND    CQFILE.CQ_SEQ_FG    >= :QCQ2-WBEG-SEQ-FG
              AND    CQFILE.CQ_DATE      >= :QCQ2-WBEG-DATE
              AND    CQFILE.CQ_START_TIME >= :QCQ2-WBEG-START-TIME
              AND    CQFILE.CQ_REV_DEL   >= :QCQ2-WBEG-REV-DEL
              AND    CQFILE.CQ_MISC_KEY_FLD >= :QCQ2-WBEG-MISC-KEY-FLD
              AND    CQFILE.CQ_REV_PDUE_DAYS
                                         >= :QCQ2-WSBEG-REV-PDUE-DAYS
              AND    CQFILE.CQ_REV_BAL   >= 0
              AND    CQFILE.CQ_SEQ       >= 0                         )
               OR  ( CQFILE.CQ_BRNO   >= :QCQ2-WBEG-BRNO
              AND    CQFILE.CQ_COLLID >= :QCQ2-WBEG-COLLID
              AND    CQFILE.CQ_REV_BEHAVIOR_RISK
                                         >= :QCQ2-WBEG-REV-BEHAVIOR-RISK
              AND    CQFILE.CQ_SEQ_FG    >= :QCQ2-WBEG-SEQ-FG
              AND    CQFILE.CQ_DATE      >= :QCQ2-WBEG-DATE
              AND    CQFILE.CQ_START_TIME >= :QCQ2-WBEG-START-TIME
              AND    CQFILE.CQ_REV_DEL   >= :QCQ2-WBEG-REV-DEL
              AND    CQFILE.CQ_MISC_KEY_FLD >= :QCQ2-WSBEG-MISC-KEY-FLD
              AND    CQFILE.CQ_REV_PDUE_DAYS >= 0
              AND    CQFILE.CQ_REV_BAL   >= 0
              AND    CQFILE.CQ_SEQ       >= 0                         )
               OR  ( CQFILE.CQ_BRNO   >= :QCQ2-WBEG-BRNO
              AND    CQFILE.CQ_COLLID >= :QCQ2-WBEG-COLLID
              AND    CQFILE.CQ_REV_BEHAVIOR_RISK
                                         >= :QCQ2-WBEG-REV-BEHAVIOR-RISK
              AND    CQFILE.CQ_SEQ_FG    >= :QCQ2-WBEG-SEQ-FG
              AND    CQFILE.CQ_DATE      >= :QCQ2-WBEG-DATE
              AND    CQFILE.CQ_START_TIME >= :QCQ2-WBEG-START-TIME
              AND    CQFILE.CQ_REV_DEL   >= :QCQ2-WSBEG-REV-DEL
              AND    CQFILE.CQ_MISC_KEY_FLD >= ''
              AND    CQFILE.CQ_REV_PDUE_DAYS >= 0
              AND    CQFILE.CQ_REV_BAL   >= 0
              AND    CQFILE.CQ_SEQ       >= 0                         )
               OR  ( CQFILE.CQ_BRNO   >= :QCQ2-WBEG-BRNO
              AND    CQFILE.CQ_COLLID >= :QCQ2-WBEG-COLLID
              AND    CQFILE.CQ_REV_BEHAVIOR_RISK
                                         >= :QCQ2-WBEG-REV-BEHAVIOR-RISK
              AND    CQFILE.CQ_SEQ_FG    >= :QCQ2-WBEG-SEQ-FG
              AND    CQFILE.CQ_DATE      >= :QCQ2-WBEG-DATE
              AND    CQFILE.CQ_START_TIME >= :QCQ2-WSBEG-START-TIME
              AND    CQFILE.CQ_REV_DEL   >= 0
              AND    CQFILE.CQ_MISC_KEY_FLD >= ''
              AND    CQFILE.CQ_REV_PDUE_DAYS >= 0
              AND    CQFILE.CQ_REV_BAL   >= 0
              AND    CQFILE.CQ_SEQ       >= 0                         )
               OR  ( CQFILE.CQ_BRNO   >= :QCQ2-WBEG-BRNO
              AND    CQFILE.CQ_COLLID >= :QCQ2-WBEG-COLLID
              AND    CQFILE.CQ_REV_BEHAVIOR_RISK
                                         >= :QCQ2-WBEG-REV-BEHAVIOR-RISK
              AND    CQFILE.CQ_SEQ_FG    >= :QCQ2-WBEG-SEQ-FG
              AND    CQFILE.CQ_DATE      >= :QCQ2-WSBEG-DATE
              AND    CQFILE.CQ_START_TIME >= 0
              AND    CQFILE.CQ_REV_DEL   >= 0
              AND    CQFILE.CQ_MISC_KEY_FLD >= ''
              AND    CQFILE.CQ_REV_PDUE_DAYS >= 0
              AND    CQFILE.CQ_REV_BAL   >= 0
              AND    CQFILE.CQ_SEQ       >= 0                         )
               OR  ( CQFILE.CQ_BRNO   >= :QCQ2-WBEG-BRNO
              AND    CQFILE.CQ_COLLID >= :QCQ2-WBEG-COLLID
              AND    CQFILE.CQ_REV_BEHAVIOR_RISK
                                         >= :QCQ2-WBEG-REV-BEHAVIOR-RISK
              AND    CQFILE.CQ_SEQ_FG    >= :QCQ2-WSBEG-SEQ-FG
              AND    CQFILE.CQ_DATE      >= '1900-01-01'
              AND    CQFILE.CQ_START_TIME >= 0
              AND    CQFILE.CQ_REV_DEL   >= 0
              AND    CQFILE.CQ_MISC_KEY_FLD >= ''
              AND    CQFILE.CQ_REV_PDUE_DAYS >= 0
              AND    CQFILE.CQ_REV_BAL   >= 0
              AND    CQFILE.CQ_SEQ       >= 0                         )
               OR  ( CQFILE.CQ_BRNO   >= :QCQ2-WBEG-BRNO
              AND    CQFILE.CQ_COLLID >= :QCQ2-WBEG-COLLID
              AND    CQFILE.CQ_REV_BEHAVIOR_RISK
                                        >= :QCQ2-WSBEG-REV-BEHAVIOR-RISK
              AND    CQFILE.CQ_SEQ_FG    >= ''
              AND    CQFILE.CQ_DATE      >= '1900-01-01'
              AND    CQFILE.CQ_START_TIME >= 0
              AND    CQFILE.CQ_REV_DEL   >= 0
              AND    CQFILE.CQ_MISC_KEY_FLD >= ''
              AND    CQFILE.CQ_REV_PDUE_DAYS >= 0
              AND    CQFILE.CQ_REV_BAL   >= 0
              AND    CQFILE.CQ_SEQ       >= 0                         )
               OR  ( CQFILE.CQ_BRNO   >= :QCQ2-WBEG-BRNO
              AND    CQFILE.CQ_COLLID >= :QCQ2-WSBEG-COLLID
              AND    CQFILE.CQ_REV_BEHAVIOR_RISK
                                         >= 0
              AND    CQFILE.CQ_SEQ_FG    >= ''
              AND    CQFILE.CQ_DATE      >= '1900-01-01'
              AND    CQFILE.CQ_START_TIME >= 0
              AND    CQFILE.CQ_REV_DEL   >= 0
              AND    CQFILE.CQ_MISC_KEY_FLD >= ''
              AND    CQFILE.CQ_REV_PDUE_DAYS >= 0
              AND    CQFILE.CQ_REV_BAL   >= 0
              AND    CQFILE.CQ_SEQ       >= 0                        ))
              AND (  CQFILE.CQ_BRNO   <= :QCQ2-WEND-BRNO
              AND    CQFILE.CQ_COLLID <= :QCQ2-WEND-COLLID
              AND    CQFILE.CQ_REV_BEHAVIOR_RISK
                                         <= :QCQ2-WEND-REV-BEHAVIOR-RISK
              AND    CQFILE.CQ_SEQ_FG    <= :QCQ2-WEND-SEQ-FG
              AND    CQFILE.CQ_DATE      <= :QCQ2-WEND-DATE
              AND    CQFILE.CQ_START_TIME <= :QCQ2-WEND-START-TIME
              AND    CQFILE.CQ_REV_DEL   <= :QCQ2-WEND-REV-DEL
              AND    CQFILE.CQ_MISC_KEY_FLD <= :QCQ2-WEND-MISC-KEY-FLD
              AND    CQFILE.CQ_REV_PDUE_DAYS <= :QCQ2-WEND-REV-PDUE-DAYS
              AND    CQFILE.CQ_REV_BAL   <= :QCQ2-WEND-REV-BAL
              AND    CQFILE.CQ_SEQ       <= :QCQ2-WEND-SEQ )
            ORDER BY CQFILE.CQ_BRNO,
                     CQFILE.CQ_COLLID,
                     CQFILE.CQ_REV_BEHAVIOR_RISK,
                     CQFILE.CQ_SEQ_FG,
                     CQFILE.CQ_DATE,
                     CQFILE.CQ_START_TIME,
                     CQFILE.CQ_REV_DEL,
                     CQFILE.CQ_MISC_KEY_FLD,
                     CQFILE.CQ_REV_PDUE_DAYS,
                     CQFILE.CQ_REV_BAL,
                     CQFILE.CQ_SEQ,
                     CQFILE.CQ_ACCTNO
           END-EXEC.

           EXEC SQL
               OPEN CQ2_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE ZEROES TO QCQ2-WBEG-BRNO 
                          QCQ2-WEND-BRNO
                          QCQ2-WBEG-REV-BEHAVIOR-RISK
                          QCQ2-WEND-REV-BEHAVIOR-RISK
                          QCQ2-WBEG-START-TIME
                          QCQ2-WEND-START-TIME
                          QCQ2-WBEG-REV-DEL
                          QCQ2-WEND-REV-DEL
                          QCQ2-WBEG-REV-PDUE-DAYS
                          QCQ2-WEND-REV-PDUE-DAYS
                          QCQ2-WBEG-REV-BAL
                          QCQ2-WEND-REV-BAL
                          QCQ2-WBEG-SEQ
                          QCQ2-WEND-SEQ
                          QCQ2-WSBEG-REV-BEHAVIOR-RISK
                          QCQ2-WSBEG-START-TIME
                          QCQ2-WSBEG-REV-DEL
                          QCQ2-WSBEG-REV-PDUE-DAYS
                          QCQ2-WSBEG-REV-BAL
                          QCQ2-WSBEG-SEQ.
 
           MOVE SPACES TO QCQ2-WBEG-DATE
                          QCQ2-WEND-DATE
                          QCQ2-WBEG-COLLID
                          QCQ2-WEND-COLLID
                          QCQ2-WBEG-SEQ-FG
                          QCQ2-WEND-SEQ-FG
                          QCQ2-WBEG-MISC-KEY-FLD
                          QCQ2-WEND-MISC-KEY-FLD
                          QCQ2-WSBEG-COLLID
                          QCQ2-WSBEG-SEQ-FG
                          QCQ2-WSBEG-DATE
                          QCQ2-WSBEG-MISC-KEY-FLD.
 
           MOVE STAT---GOOD TO CQ2-CURSOR-STAT.

      *-----------------------------------------------------------------
       READ-CQ2-FILE.
           PERFORM READ-IT.
           MOVE CQ-PATH-OWNBR  TO CQ-BRNO. 
           MOVE CQ-SELECT-PATH TO E-FILE.
           PERFORM LOAD-CQ2-NON-COMP-KEY.
           MOVE SPWSCQ-CQ2-KEY-NON-COMP TO E-KEYX.

           MOVE CQ-BRNO              TO QCQ-BRNO.
           MOVE CQ-COLLID            TO QCQ-COLLID.
           MOVE CQ-REV-BEHAVIOR-RISK TO QCQ-REV-BEHAVIOR-RISK.
           MOVE CQ-SEQ-FG            TO QCQ-SEQ-FG.
           MOVE CQ-DATE    TO SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD  TO QCQ-DATE.
           MOVE CQ-START-TIME        TO QCQ-START-TIME.
           MOVE CQ-REV-DEL           TO QCQ-REV-DEL.
           MOVE CQ-MISC-KEY-FLD      TO QCQ-MISC-KEY-FLD.
           MOVE CQ-REV-PDUE-DAYS     TO QCQ-REV-PDUE-DAYS.
           MOVE CQ-REV-BAL           TO QCQ-REV-BAL.
           MOVE CQ-SEQ               TO QCQ-SEQ.

           EXEC SQL
            SELECT CQFILE.CQ_BRNO,
                   CQFILE.CQ_ACCTNO,
                   ISNULL(CQFILE.CQ_COLLID,''),
                   CQFILE.CQ_REV_BEHAVIOR_RISK,
                   ISNULL(CQFILE.CQ_SEQ_FG,''),
                   ISNULL(CAST(CQFILE.CQ_DATE AS VARCHAR(10)),'0'),
                   CQFILE.CQ_START_TIME,
                   CQFILE.CQ_REV_DEL,
                   ISNULL(CQFILE.CQ_MISC_KEY_FLD,''),
                   CQFILE.CQ_REV_PDUE_DAYS,
                   CQFILE.CQ_REV_BAL,
                   CQFILE.CQ_SEQ,
                   ISNULL(CAST(CQFILE.CQ_LTOUCH_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CQFILE.CQ_WORK_COLLID,''),
                   ISNULL(CQFILE.CQ_WORKED_FG,''),
                   ISNULL(CQFILE.CQ_IN_PROCESS,''),
                   ISNULL(CAST(CQFILE.CQ_DATE_QUEUED
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CQFILE.CQ_TIME_ZONE_ERROR,'')
            INTO :QCQ-REC
            FROM DBO.CQFILE
            WHERE CQFILE.CQ_BRNO = :QCQ-BRNO
              AND CQFILE.CQ_COLLID = :QCQ-COLLID
              AND CQFILE.CQ_REV_BEHAVIOR_RISK = :QCQ-REV-BEHAVIOR-RISK
              AND CQFILE.CQ_SEQ_FG = :QCQ-SEQ-FG
              AND CQFILE.CQ_DATE = :QCQ-DATE
              AND CQFILE.CQ_START_TIME = :QCQ-START-TIME
              AND CQFILE.CQ_REV_DEL = :QCQ-REV-DEL
              AND CQFILE.CQ_MISC_KEY_FLD = :QCQ-MISC-KEY-FLD
              AND CQFILE.CQ_REV_PDUE_DAYS = :QCQ-REV-PDUE-DAYS
              AND CQFILE.CQ_REV_BAL = :QCQ-REV-BAL
              AND CQFILE.CQ_SEQ = :QCQ-SEQ
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-CQ2-FILE.

           IF ( IO-FG = 0 )
              PERFORM GET-CQ-FIELDS.

      *-----------------------------------------------------------------
       READ-CQ2-FILE-NEXT.
           PERFORM READ-IT.
           MOVE CQ-PATH-OWNBR  TO CQ-BRNO.
           MOVE CQ-SELECT-PATH TO E-FILE.
           PERFORM LOAD-CQ2-NON-COMP-KEY.
           MOVE SPWSCQ-CQ2-KEY-NON-COMP TO E-KEYX.
           INITIALIZE QCQ-REC.

           IF ( CQ2-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH CQ2_CURSOR INTO :QCQ-REC
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-CQ2-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-CQ-FIELDS.

      *-----------------------------------------------------------------
       CLOSE-CQ2-FILE.

           PERFORM CLOSE-IT.
           IF ( CQ2-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE CQ2_CURSOR
              END-EXEC
              MOVE STAT---BAD TO CQ1-CURSOR-STAT.





       COPY "LIBSP/SPGQ2RN.CPY".
       COPY "LIBLP/LPCD1RN.CPY".
       COPY "LIBLP/LPFM1IN.CPY".
       COPY "LIBLP/LPLP1RN.CPY".
       COPY "LIBLP/LPNO1IN.CPY".
       COPY "LIBLP/LPTZ1RN.CPY".
       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO EXT-EOBUF-ERRCD.

      ******************************************************************
       END-PROGRAM SECTION.
      ******************************************************************

           PERFORM CLOSE-FILES.

       EXIT-PROG.
           IF SV-BEG-BR NOT = 9999
              MOVE SV-BEG-BR TO BEG-BRANCH
              MOVE SV-END-BR TO END-BRANCH.

           IF EXT-ROUTE-BUF = "00"
              MOVE "LP/LPCPMU" TO FORM-NAM
              MOVE FORM-PATH TO FORM-PATHNAME.

       STOP-RUN.
           IF EXT-ROUTE-BUF = "00" OR "40" OR "50"
              DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN
              DESTROY CQRUN-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.
           EXIT PROGRAM.
