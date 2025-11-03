       IDENTIFICATION DIVISION.
       PROGRAM-ID.       CPRUN.
       DATE-WRITTEN.   7/12/84.
      ******************************************************************
      *    DIR :  LP
      *    DESC:  EXTRACTION RUN
      *
      * USES TRUE RECENCY DAYS
      *
      * REV :
      *  062895 BAH ADDED CONTRACTUAL-DAY1, CONTRACTUAL-DAY2
      *                   DLNO1 AND DLNO2    # E021
      *  020696 BLL ADD LOGIC FOR UPDATING CQ
      *  031396 BLL ADD PURGING OF CQ, COLLECTOR HEADER CREATES FOR CQ
      *  031496 KEC REMOVED SETUP FOR BEGINNING AND ENDING SEQNO AND
      *             INSERTED INPUT OF A GROUP-ID.
      *  032796 BLL REMOVED CALLING OF CPUPDS; PUT UPDATE LOGIC IN
      *             COPY MEMBER LPCPUPDS
      *  032996 BLL ADD COLLECTOR POOL LOGIC (CP-FILE)
      *  040496 BLL CHECK FOR CONTRACTUAL POTS (S,I,O)
      *  040896 BLL MOVE IN PASTDUE DAYS FOR POTS IN COLLECTOR QUEUE
      *  042296 BLL DISABLE COLLECTOR QUEUE LOGIC; USE CQRUN TO CREATE
      *             COLLECTOR QUEUE
      *  060496 BLL FIX GROUP SCAN - WASN'T FINDING RECORDS
      *  062496 KEC ADDED CHECKS ON: EA-REPO, EA-BANKRUPT, EA-PL AND
      *             EA-PL-IN-ACTIVE-BOTH.
      *  070896 KEC MADE SMALL FIX TO GOING TO 'TEST-EA-PL' INSTEAD OF
      *             'TEST-EA1-FIELDS-AWUPD' IN 'TEST-MATURED.
      *  071196 KEC ADDED 'REPLACE-COL-FG' TO 'DISPLAY-PARAMETERS'
      *             SECTION.
      *  071696 BAH ADDED TEST OF GP-ALLOW-MORE-COLLECT TO ALLOW MORE THAN
      *             ONE COLLECTION TYPE FORM REQUEST, PR # E128
      *  MJD 071696 ADDED LIBLP/LPUPWK FOR CHANGES IN LIBLP/AGEING
      *  KEC 111496 CHANGED LN-POCD FOR RESCISSION.
      *  CS  021097 ADDED TEST OF EA-CONFIDENTIAL-FG AND EA-1ST-PMT-MISSES
      *  BAH 031397 MOVED TEST OF P&L, REPO, AND BANKRUPT TO TOP
      *             OF TEST-EA
      *  BLV 041497 ALLOW ALPHA SPLIT OF QUEUES FOR COLLECTOR POOL
      *  GLF 050797 ADDED COPY MEMBERS: SETCTBLW AND SETCTBL
      *  MJD 051597 ADDED LPPOF2 COPY MEMBER (PREVIOUSLY WAS PART OF LPPOFF)
      *  JTG 052097 ADDED 'LIBLP/ADDONSPR' TO GET CORRECT SPR RE: INS RB
      *   CS 061997 ADDED EA-LEGAL, EA-DEBTAID, EA-JUDGEMENT
      *  BLV 041497 ALLOW ALPHA SPLIT OF QUEUES FOR COLLECTOR POOL
      *  BLV 072897 COLLECTOR POOL & ALPHA POOL NOT CHECKING OPTION
      *             OF WHETHER TO REPLACE LOAN COLLECTORS
      *  JTG 072897 ADDED LPWDEL/W  MERCURY #E135
      *  BLV 081497 DISTRIBUTIONS TO COLLECTOR POOLS MAY HAVE BEEN
      *             INCORRECT DEPENDING ON CLASS RANGE
      *  BLV 102197 SET BEG & ENDING BRANCH = FIL-PATH IN EO-EXEC
      *  BLV 110397 CHECK BR-MIDSTREAM-1STPAY-FG, IF "Y", CONSIDER
      *             MIDSTREAMS AS 1ST PAY MISSES IF CURBAL = ORIGPURAMT
      *  JTG 111797 ADDED LOGIC FOR LIBLP/DEFPOL RE: BR-NODEF-1ST-X-MONTHS #167
      *  BLV 010598 PR #E172; CROSS DATA PATHS & ALLOW GROUP
      *  GLF 012298 REMOVED ALL USE OF GLOBAL
      *  GLF 012298 REPLACED 'IO' ON GP-FILE WITH GET 'GPENV'
      *  GLF 022098 DELETED BR-SECURITY-OK AND ADDED LIBGB/BRSECUREW.
      *   CS 022798 ADDED READ OF EA-FILE BEFORE PROCESSING BRANCH TO MAKE
      *             SURE EXTRACTIONS EXISTED FOR THE GROUP-ID
      *  BAH 041598 COMMENTED OUT CS CHANGE FROM 022798, WILL READ ALL EA'S FOR
      *             EACH BRANCH
      *   CS 042098 ADDED TEST OF EA-ORIGIN
      *  GLF 990924 REPLACED IO-FG NUMBERS WITH IO-FG 88 LEVEL PER DEVELOPMENT
      *             MEETING (082599)
      *  BLV 000516 IF USING ALPHA POOLS & F3 ALL BORROWER NAMES, GET-BORROWER
      *             WAS SKIPPED & ALPHA ROUTINE LOOPED WRITING THE WORK FILE;
      *             CHANGED TO ALWAYS GET BORROWER FOR ALPHA POOL & FIXED
      *             LOOP IN WRITING WK-FILE
      *  BLV 001017 TEST FOR SKIP, INCLUDE, ONLY OF RECENCY POTS, MISPAYS,
      *             (EA-RPOTS, EA-MISPAYS), WORLD PR #244
      *  BLV 001127 ADD GQFILE (GLOBAL QUEUE); LPCPUPD NOW MAY UPDATE GQFILE,
      *             TFC PR#60
      *  BLV 121800 IF GLOBAL QUEUING, SET UP COLLECTOR POOL GLOBAL PATH
      *             INSTEAD OF LOCAL (CP-ALL-PATH), TFC PR# 60
      *  BLV 121900 IF GLOBAL QUEUING, PUT MACHINE & PATH IN ALPHA POOL WORK
      *             FILE, FIX INITIALIZE POOLS & ALPHA SPLIT ROUTINES FOR
      *             GLOBAL QUEUING, TFC PR# 60
      *   CS 010503 ADDED TEST OF EA-ALLOTMENTS(I,S,O), ACE PR# 1472
      *   CS 010717 ADDED TEST OF GP-I-AM-MODERN.  GARNISHMENT FORMS
      *             NEED TO SKIP TEST ON SPOUSE.
      *  BLV 011009 ADD TEST ON BEG & ENDING MONTHS SINCE LOAN DATE, MONTHS
      *             UNTIL MATURITY, REGWAN PR# 1587
      *  BLV 011011 CHANGE BEG & ENDING MONTHS SINCE LOAN DATE, MONTHS
      *             UNTIL MATURITY, TO BE DAYS, REGWAN PR# 1587
      *  BAH 020516 ADDED TEST OF FD-SMARTPORT = "E" FOR CREATING ALL FORM
      *             REQUESTS IN THE CORPORATE BRNO AND THEN THEY WILL PRINT
      *             THEM INTO AN ASCII FILE EXTRACT, REGACC PR# 1800(LPCPUPD)
      *  BLV 020820 FIX LOCKING PROBLEMS WITH CP-FILE
      *  BLV 021029 CHECK DAY OF MONTH DUE DAY RANGE, REGACC PR# 0009,
      *             WILL IGNORE THESE PARAMETERS IF THEY = 20, 20...EVEN
      *             THOUGH THAT COULD BE A VALID RANGE
      *  BLV 030702 IF I-AM-REGACC, USE LNDATE DAYS & MATURE DAYS FOR RAW SCORE
      *             AND DELQ SCEN ID RANGES, REGACC PR# 0033
      *  BLV 030702 IF CP-ALPHA-FLAG = "B", TRY TO SPLIT ACCOUNTS BETWEEN POOL
      *             MEMBERS SO THE # OF ACCTS & BALANCE DUE IS SPLIT AS
      *             EVENLY AS POSSIBLE AMONG POOL MEMBERS, REGACC PR# 0034
      *  BAH 040812 ADDED TEST OF BW-CONFIDENTIAL, THIS FLAG OVERRIDES THE
      *             LOAN FLAG, IF THE LOAN IS "N", BUT THE BW IS "Y", THEN
      *             ITS CONFIDENTIAL ! PR# 2343
      *  BLF 050418 IF I-AM-WAGNER & BR-RECDEL-NOT-REC = Y, USE CALENDAR
      *             RECENCY, WAGNER PL# 478
      *   CS 050502 USING CALANDAR RECENCY, 1ST PAY DEFAULTS WERE BEING SKIPPED
      *             WAS: IF LN-DATE-PAID-LAST = 0, IF RECENCY 0 GO TO EXIT
      *             USING TRUE DAYS, RECENCY OF 0 IS TODAY... USING CALANDAR
      *             IT MANY DAYS, CHANGED TO IGNORE THE TEST ON LN-DATE-PAID-
      *             LAST IF BR-RECDEL-NOT-REC = Y AND GP-I-AM-WAGNER [PR#478]
      *  BLF 050512 IF I-AM-WAGNER & BR-RECDEL-NOT-REC = Y, WASNN'T CHECKING
      *             FLAG WHEN COMPUTING RECENCY FOR RECENCY POTS, WAGNER PL# 478
      *  BAH 050721 ADDED LITTLE-Z TEST AND ALPNAME2 = "~~~" FOR WORLD MEXICO
      *             PR# 416
      * BLF 070906  IF BR-COLL-QUEUE-CON-REC = "N", MOVE 0 TO CQ- OR GQ-REV-DEL,
      *             SO NO DELINQUENCY IN QUEUE ORDERING (CHANGE TO
      *             LIBLP/LPCPUPD), PCCREDIT PR# 3054
      * BAH 091002 ADDED FD-SKIP-BAD-ADR TO TEST PRIMARY MAKER ONLY IF BAD
      *            ADDRESS FLAG SET, DO NOT CREATE FORM REQUEST REGACC PR# 256
      *            CHANGE IS IN LPCPUPD
      *  CS 110705 IF GP-I-AM-MODERN, IF LN-LPO = AW-EA-DESC MOVE EA-DESC TO
      *            LN-LPO  MODERN PR#3821
      * BLM 130422 ADD NEW PARAMETER, EA-BORR-STATE, VERIFIES MATCH WITH
      *            PRIMARY BORR STATE IF EA-BORR-STATE NOT BLANK OR *,
      *            SECURITY AUTO PR# 4248
      *  CS 140910 SET CP-PATH FOR GLOBAL QUEUEING, MOVE EXT-FILPATH-BASE,
      *            EXT-FILPATH-MACHINE TO CP-GLOBAL-BASE,CP-GLOBAL-MACHINE,
      *            CP-PATH-OVERRIDE TO "Y" IF GLOBAL QUEUEING.
      *
      * KEC 2016.0121 {PD#00003} PARADATA <A30> [JAY CISZEWSKI]
      *         FIXED KEYS & RECS FOR FD-REC.
      *         ADDED NEW FDB-FILE; WAS IN FD-FILE.
      *         REMOVED FT-FILE; MERGED INTO FD-FILE.
      *
      *  BAH 160125 SPLIT OUT LTFILE, PD0003
      *
      * KEC 2016.0218 {PD#00003} PARADATA <A30> [JAY CISZEWSKI]
      *         FIXED KEYS & RECS FOR FD-REC.
      *         CHANGED FMNO/FORMNO/NEXTFM/SETFM FROM 3 TO 4 BYTES.
      *
      * KEC 2016.0301 {PD#00003} PARADATA <A30> [JAY CISZEWSKI]
      *         FIXED KEYS & RECS FOR EA-REC (HOLD/ORIG/SAVE/WK).
      *         CHANGED FMNO/FORMNO/NEXTFM/SETFM FROM 3 TO 4 BYTES.
      *         CHANGED EA-ALPNAME1 & EA-ALPNAME2 3 TO 4 BYTES.
      *
      * KEC 2016.0310 {PD#00003} PARADATA <A30> [JAY CISZEWSKI]
      *         ADDED CD-EA-FILLER FOR CHANGE TO CD-EA-REC.
      *
      * KEC 2016.0324 {PD#00003} PARADATA <A30> [JAY CISZEWSKI]
      *         CHANGED CDSCAN-WINDOW-BUF TO HAVE MATCHING CD1-KEY
      *         LAYOUTS (FOR EACH CODE TYPE; CD-TYPE).
      *         CDSCAN-CD1-KEY IS THE NEW (PRIMARY) WITH REDEFINES OF
      *         CDSCAN-??-KEY (NEW).
      *
      *  BAH 170525 ACTIVATE 8 DIGIT DATES, PD0006
      *   CS 171114 FIXED EXTRACT DATE, PRESSING RETURN DID NOT DEFAULT
      *             SYSTEM DATE, THE DATE WAS ZERO.
      *  BAH 180312 BR-01 WAS RESETTING FIELD, NEEDED TO KEEP ENTRY FOR
      *             BATCH, CHANGED TO "R  A000BR1.".
      *  BAH 181003 GLOBAL BWFILE
      * KEC 2018-1026 CONVERTED CPFILE FOR A32 & A34
      *               CP-PATH-OVERRIDE TO "G" IF GLOBAL QUEUEING.
      * BAH 181205 GLOBAL LNFILE
      * BAH 190122 SPLIT CDFILE
      * BAH 190311 MOVED FM-BRNO2 INTO LPWSFM-EDIT-FM-INITKEY TO REMOVE THE
      *            REDEFINE IN THE FM-REC
      * BAH 20190919 REMOVED ACCESS-CALL ON LNFILE
      * BAH 20200709 MOVED OPENS FROM LPCPUPD TO HERE #TST001
      * BAH 20220818 REMOVED GLOBRD, READ GB-FILE INSTEAD
      * BAH 20220914 HARD CODED START
      * BAH 20221129 NEED DEFINITION OF RECENCY TO BE RECDEL BASED ON WAC'S
      *              RECDEL METHOD "D", NOT STANDARD EXACT DAYS SINCE LAST
      *              TRANSACTION.  MOVE RECDEL TO RECENCY #1580
      * BAH 20230725 BUG IN THE EAFILE UPGRADE PUTS 1 AND 1 IN THE
      *              FIELD 13 DAY OF MO DU:, THIS SHOULD BE 1 AND 31
      * MB  20231115 PERFORMANCE TUNING - CREATED STORED PROCEDURE TO 
      *                RETRIEVE LN3 ROWS WITH BW DATA
      *               ADDED STATE PARAMETER ARRAY PROCESSING
      *               EMBEDDED ALL INSERT/DELETE/UPDATE QUERIES TO USE
      *                 UPDATE_CONNECT CONNECTION
      * RMZ 20240116 CHANGED JULIAN-BEG(END,CC) TO EXT-JULIAN-BEG(END,CC)
      * BAH 2024.0201 REMOVED CRNOR2 FOR A FORM (LPCPUPD) REMOVE NOTICE FILE
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "LIBGB/GBFSPR.CPY".
      *COPY "LIBLP/LPFSAW.CPY".
       COPY "LIBGB/GBFSWKI.CPY".
       I-O-CONTROL.
       DATA DIVISION.
       FILE SECTION.
       COPY "LIBGB/GBFDPR.CPY".
      *COPY "LIBLP/LPFDAW.CPY".
      **************************************************
      *          ALPHA SPLIT WORK FILE
      **************************************************
       FD  WK-FILE
           LABEL RECORDS ARE STANDARD.
       01  WK-REC.
           03  WK-KEY.
               05  WK-POOL              PIC X(3).
               05  WK-LAST-NAME         PIC X(36).
               05  WK-BALANCE-KEY REDEFINES WK-LAST-NAME.
                   07  WK-BALANCE       PIC S9(6)V99.
                   07  FILLER           PIC X(28).
               05  WK-SEQNO             PIC 9(3).
           03  WK-BRNO                  PIC 9(4).
           03  WK-ACCTNO                PIC 9(8).
           03  WK-MACHINE               PIC XX.
           03  WK-DATA-PATH             PIC X(6).

       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(36) VALUE
           "@(#)UT/CPRUN S35 05/16/24-09:56:27 >".
      ****************************
      *     F I L E   I F N      *
      ****************************
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBGB/GB01GB.CPY".
       COPY "LIBGB/GB01GB_SQL.CPY".
       COPY "LIBGB/GBWSGB.CPY".
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
       COPY "LIBSP/SP01CQ.CPY".
       COPY "LIBSP/SP01CQ_SQL.CPY".
       COPY "LIBSP/SPWSCQ.CPY".
       COPY "LIBSP/SP01GQ.CPY".
       COPY "LIBSP/SP01GQ_SQL.CPY".
       COPY "LIBSP/SPWSGQ.CPY".
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
       COPY "LIBGB/GB01GR.CPY".
       COPY "LIBGB/GB01GR_SQL.CPY".
       COPY "LIBGB/GBWSGR.CPY".

       EXEC SQL BEGIN DECLARE SECTION END-EXEC.
       01  WS-EA1-ROWCOUNT             PIC 9(5).
       EXEC SQL END DECLARE SECTION END-EXEC.

       01  LN3-CONN-OPEN-SW     PIC X          VALUE "N".
       01  PREV-ACCTNO          PIC 9(6)       VALUE 0.
       01  ORIG-LN-REC          PIC X(LN-SIZE) VALUE SPACES.

       01  PR-IFN               PIC X(2)      VALUE "PR".
       01  WK-IFN               PIC X(2)      VALUE "WK".

       01  HOLD-BR-KEY          PIC 9(4)      VALUE 0.
       01  HOLD-FM1-KEY         PIC X(28)     VALUE SPACES.
       01  HOLD-BW-SSNO         PIC 9(9)      COMP-3 VALUE 0.

       01  COL-COUNT            PIC 99        VALUE 0.
       01  HOLD-POOL-ID         PIC XXX.
       01  TOTAL-PER-COLLECTOR  PIC 9(5)      VALUE 0.
       01  COLLECTOR-SUB        PIC 99        VALUE 0.
       01  CURRENT-COLLECTOR-COUNTER PIC 9(5) VALUE 0.
       01  LITTLE-ZX.
           03  FILLER           PIC X VALUE X"7A".
           03  FILLER           PIC X VALUE X"7A".
           03  FILLER           PIC X VALUE X"7A".
           03  FILLER           PIC X VALUE X"7A".
       01  LITTLE-Z  REDEFINES LITTLE-ZX PIC X(4).
       01  SUB                  PIC 9(6)      COMP-3 VALUE 0.
       01  SUB1                 PIC 9(6)      COMP-3 VALUE 0.
       01  LN-ACCUM-BAL         PIC S9(7)V99  COMP-3 VALUE 0.
       01  WORK-LNAME           PIC X(04).
       01  HOLD-LOAN-DATE       PIC 9(8)      COMP-3 VALUE 0.
       01  SAVE-CQ-SEQ          PIC 999       VALUE 0.
       01  SAVE-GQ-SEQ          PIC 999       VALUE 0.

       01  UP-DOWN-FG           PIC X         VALUE "U".
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

      ***********************************
      *     P R I N T   B U F F E R S
      ***********************************

      *01  DETAIL-LINE     PIC X(132) VALUE SPACES.
      *01  FILLER          REDEFINES DETAIL-LINE.
      *    03  D-ACCTNO    PIC 999999.99.
      *    03  FILLER      PIC X(9).
      *    03  D-ERROR     PIC X(50).
      *    03  D-ACTION    PIC X(50).

       COPY "LIBGB/EOBUF_EXT.CPY".
           03  BEG-BRANCH       PIC 9(4).
           03  END-BRANCH       PIC 9(4).
           03  BEG-CLASS        PIC 9(3).
           03  END-CLASS        PIC 9(3).
           03  BEG-GROUP-ID     PIC X(6).
           03  CLEAR-AWFILE     PIC X.
           03  EXTRDATE         PIC 9(8).
           03  REPLACE-COL-FG   PIC X.
           03  GROUP-FLAG       PIC X.
           03  ENT-GROUP        PIC X(4).

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

       COPY "LIBWI/GRSCANW.CPY".
       COPY "LIBGB/GRWORKW.CPY".
       COPY "LIBGB/BRSECUREW.CPY".
       COPY "LIBGB/GETPRW.CPY".
       COPY "LIBGB/SPPDTHW.CPY".
       COPY "LIBLP/LPAVCRW.CPY".
       COPY "LIBLP/LPDEFW.CPY".
       COPY "LIBLP/LPWDELW.CPY".
       COPY "LIBWI/CDSCANW.CPY".
       COPY "LIBLP/LPLCASW.CPY".
       COPY "LIBLP/LPLCHGW.CPY".
       COPY "LIBLP/LPSCHDW.CPY".
       COPY "LIBLP/LPLCAPW.CPY".
       COPY "LIBLP/DEFPOLW.CPY".
       COPY "LIBLP/ADDONSPRW.CPY".
       COPY "LIBLP/LPPOFFW.CPY".
       COPY "LIBLP/LPITRMW.CPY".
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
       COPY "LIBLP/LPCPAW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBLP/SETCTBLW.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/RBACTW.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/MKTEMPW_CMD.CPY".
       COPY "LIBGB/SYSTEMW.CPY".
       COPY "LIBGB/PASSWDW.CPY".
       COPY "LIBGB/GPENVW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/PRU_LNK.CPY".
       COPY "LIBGB/SCREENW.CPY".

       COPY "LIBLP/CPRUN_DEF.CPY".
       COPY "LIBLP/CPRUN_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".

       COPY "LIBLP/ARRAYSPW.CPY". 

      ****************************************
      *     P R O G R A M   C O N T R O L    *
      ****************************************
       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBLP/CPRUN_SCN.CPY".

       PROCEDURE DIVISION
             USING FORM-PATHNAME EXIT-PATHNAME.
       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
               PR-FILE WK-FILE.
       COPY "LIBGB/DECLRP.CPY".
           PERFORM CLOSE-GQ1-FILE.
           PERFORM CLOSE-CQ1-FILE.
           PERFORM CLOSE-CP1-FILE.
           PERFORM CLOSE-SP1-FILE.
           PERFORM CLOSE-LTP1-FILE.
           PERFORM CLOSE-LTI1-FILE.
           PERFORM CLOSE-LP1-FILE.
           PERFORM CLOSE-LN3-FILE.
      *     PERFORM CLOSE-LN1-FILE.
           PERFORM CLOSE-FM1-FILE.
           PERFORM CLOSE-FD1-FILE.
           PERFORM CLOSE-EA1-FILE.
           PERFORM CLOSE-CD1-FILE.
           PERFORM CLOSE-BW1-FILE.
           PERFORM CLOSE-GR1-FILE.
           PERFORM CLOSE-BR-FILE.
           PERFORM CLOSE-GB-FILE.
           PERFORM CLOSE-PR-FILE.
           PERFORM CLOSE-LN3-CONNECTION.
           PERFORM SQL-CLOSE-UPDATE-CONNECTION.
           CLOSE WK-FILE.
       COPY "LIBGB/DECLRP2.CPY".

      ********************************
      *  MAIN MODULE:
      ********************************
       MAIN-MODULE SECTION.

           PERFORM SQL-CONNECT.

      D    STOP MESS.
           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "CPRUN" TO VDU-FORM-NAME.

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

      *----------------------------------------------------------------
       NEXT-BRANCH-NO.
           PERFORM READ-BR-FILE-NEXT.
           IF IO-BAD OR BR-NO > END-BRANCH
              GO TO END-ROUTINE.

      *---------------------------------------------------------------
       MOVE-BRANCH-PATH-INFO.

           IF EXT-ROUTE-BUF = "00"
              STRING "EXTRACTION RUN IN PROGRESS.........." , BR-NO
                          DELIMITED "  " INTO WR-BUF
              MOVE MESS-LIST TO WR-LIST
              PERFORM CALL-WRFORM.

      * CHECK EA TO SEE IF ANY CLASS & GROUP MATCHES FOR THIS BRANCH
      
           MOVE BR-NO        TO EA-BRANCH
                                QEA1-WBEG-BRANCH
                                QEA1-WEND-BRANCH.
           MOVE BEG-CLASS    TO EA-CLASS
                                QEA1-WBEG-CLASS.
           MOVE END-CLASS    TO QEA1-WEND-CLASS.
           MOVE BEG-GROUP-ID TO EA-GROUP-ID
                                QEA1-WBEG-GROUP-ID
                                QEA1-WEND-GROUP-ID.

      *     MOVE 0            TO EA-SEQNO
      *                          QEA1-WBEG-SEQNO.
      *     MOVE ALL "9"      TO QEA1-WEND-SEQNO.
      *     PERFORM START-EA1-FILE.

      ***************************************************************
      * THIS CHECK REPLACED WITH EAFILE COUNT CALL - SEE BELOW
      ***************************************************************
      * FIND-EA1FILE-NEXT.
      *     PERFORM READ-EA1-FILE-NEXT.
      *     IF IO-BAD
      *        GO TO NEXT-GROUP-LOC.
      *
      * EA-BRANCH NOT = BRANCH NUMBER GET NEW BRANCH
      *     IF EA-BRANCH NOT = BR-NO
      *         GO TO NEXT-GROUP-LOC.
      *
      * EA-CLASS IS GREATER THAN ENDING CLASS, GET NEW BRANCH
      *     IF EA-CLASS > END-CLASS
      *         GO TO NEXT-GROUP-LOC.
      *
      * TEST FOR EA WITHIN CLASS RANGE AND GROUP-ID
      *     IF EA-CLASS  NOT < BEG-CLASS
      *        IF EA-CLASS  NOT > END-CLASS
      *           IF EA-GROUP-ID = BEG-GROUP-ID
      *              GO TO BEGIN-PROCESS.
      *
      *     GO TO FIND-EA1FILE-NEXT.

           MOVE 0 TO WS-EA1-ROWCOUNT.
           PERFORM COUNT-EA1-FILE.
           IF WS-EA1-ROWCOUNT > 0
              GO TO BEGIN-PROCESS
           ELSE
              GO TO NEXT-GROUP-LOC.


      *  EA EXISTS FOR CLASS AND GROUP ID
       BEGIN-PROCESS.

           MOVE EXT-FILPATH-BASE           TO FILPATH-BASE.
           MOVE BR-MACHINE                 TO FILPATH-MACHINE.
           MOVE BR-DATA-PATH               TO FILPATH-DATA.

           MOVE "B"                        TO BW-PATH-OVERRIDE
                                              CP-PATH-OVERRIDE
                                              CQ-PATH-OVERRIDE
                                              FM-PATH-OVERRIDE
                                              GQ-PATH-OVERRIDE
                                              LN-PATH-OVERRIDE
                                              LP-PATH-OVERRIDE
                                              LTI-PATH-OVERRIDE
                                              LTP-PATH-OVERRIDE
                                              GB-PATH-OVERRIDE.

           IF ( GLOBAL-QUEUING-FG = "Y" )
              MOVE EXT-FILPATH-BASE        TO CP-GLOBAL-BASE
              MOVE EXT-FILPATH-MACHINE     TO CP-GLOBAL-MACHINE
              MOVE CP-GLOBAL-PATH          TO CP-PATH
              MOVE "G"                     TO CP-PATH-OVERRIDE.


           IF GLOBAL-QUEUING-FG NOT = "Y"
              PERFORM INITIALIZE-COLLECTOR-POOLS.

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

      ********************************
      *  START NEW BRANCH:
      ********************************
           MOVE BR-NO     TO LN-OWNBR
                             QLN3-WBEG-OWNBR
                             QLN3-WEND-OWNBR.
           MOVE BEG-CLASS TO LN-CLASS
                             QLN3-WBEG-CLASS.
           MOVE END-CLASS TO QLN3-WEND-CLASS.

      *     MOVE 0         TO LN-ACCTNO
      *                       QLN3-WBEG-ACCTNO.
      *     MOVE ALL "9"   TO QLN3-WEND-ACCTNO.

           MOVE BEG-GROUP-ID TO QEA1-WBEG-GROUP-ID.

           PERFORM START-LN3-FILE.  *> SSP LN BW 

      * LNFILE CURSOR CONTAINS BWFILE (FOR SSNO1) DATA
       READ-LN3FILE-NEXT.
           PERFORM READ-LN3-FILE-NEXT.   *> EMBEDDED LN BW  
           IF IO-BAD                    *> OR (LN-OWNBR NOT = LN-PATH-OWNBR)
              GO TO END-OF-GROUP.

           MOVE LN-REC TO ORIG-LN-REC.

      *  POCD - **  IS A DELETED CONTRACT
      * THIS ALSO CHECKED IN STORED PROCEDURE
           IF ( LN-POCD-NONLOAN )
              GO TO READ-LN3FILE-NEXT.

      * THIS ALSO CHECKED IN STORED PROCEDURE
           IF LN-OWNBR > BR-NO
              GO TO END-OF-GROUP.

      * THIS ALSO CHECKED IN STORED PROCEDURE
           IF LN-CLASS > END-CLASS
              GO TO END-OF-GROUP.

      * CLEAR LOAN FIELDS FOR COMPARE:
           MOVE " "       TO DATE-TABLE3.

      * READ BORROWER, DONT NEED TO READ FOR EVERY EA, JUST EVERY LOAN
      * BW DATA IN LN3 CURSOR FOR LN_SSNO1. IF ROW NOT FOUND BW-OWNBR = 0 
      *    PERFORM GET-MAKER.
           IF BW-OWNBR = 0 
              MOVE SPACES TO BW-ZIP
              MOVE "A"    TO BW-LNAME.

      * THE EAFILE IS TRICKY. ONCE YOU FIND THE FIRST EAFILE THAT THE
      * LOAN MATCHES ALL THE RANGES FOR, THAT IS THE LAST EAFILE ROW THAT
      * YOU CAN READ. IT STAMPS THE LOAN WITH THAT COLLECTOR AND MOVES
      * ON TO THE NEXT LOAN.

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

           PERFORM TEST-EA1-FIELDS.

           IF RTN-FG = "NL"
              GO TO READ-LN3FILE-NEXT.
           IF RTN-FG NOT = "AW"
              GO TO READ-EA1FILE-NEXT.

      * TEST FOR NO ACTION TO BE PERFORMED (SKIP IF SO):
      * REMOVED NULL CHECK (20) SINCE SQL DOES NOT ALLOW NULLS AND 202/2020
      * COULD BE A VALID VALUE. MB 11/2023
           IF EA-SETFM-NO = 0               
              IF EA-SETFM(1) = 0           
                 IF EA-SETFM(2) = 0       
                    IF EA-SETFM(3) = 0   
                       IF EA-SETACT-CD = "===="
                          IF EA-SET-LN-COL = "N"
                             IF EA-INC-CNT = "N"
                                IF EA-INC-PROMISED = 0
                                   GO TO READ-LN3FILE-NEXT.

      *  SETUP ACTION WORK RECORD:
           MOVE SPACES          TO AW-WORKERS.
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
           PERFORM CHECK-FOR-MORE-FORMS.
           MOVE "N"             TO AW-CQ-FG.
           MOVE 0 TO AW-REV-PDUE-DAYS AW-REV-DEL.

           IF EA-SET-LN-COL = "N"
              GO TO SKIP-COLLECTOR-POOL
           ELSE
              IF EA-SET-LN-COL = "Y"
                 MOVE EA-LN-COL-ID TO AW-CQ-COL-ID AW-COLCTID
                 GO TO SKIP-COLLECTOR-POOL.

           IF REPLACE-COL-FG = "N"
              IF LN-COLLECTOR NOT = "   "
                 GO TO SKIP-COLLECTOR-POOL.

       GET-COLLECTOR-FROM-POOL.
           MOVE EA-COL-POOL TO CP-ID.
           PERFORM READ-CP1-FILE.
           IF IO-BAD
      *       UNLOCK CP-FILE
              GO TO SKIP-COLLECTOR-POOL.
           IF CP-COLLECTOR-COUNT = 1
      *       UNLOCK CP-FILE
              MOVE CP-COL-ID(1) TO AW-CQ-COL-ID AW-COLCTID
              GO TO SKIP-COLLECTOR-POOL.

           IF CP-LAST-SUBSCRIPT < CP-COLLECTOR-COUNT
              ADD 1 TO CP-LAST-SUBSCRIPT
           ELSE
              MOVE 1 TO CP-LAST-SUBSCRIPT.

           MOVE CP-COL-ID(CP-LAST-SUBSCRIPT) TO AW-CQ-COL-ID AW-COLCTID.

           IF CP-ALPHA-FLAG NOT = "Y"
              IF CP-ALPHA-FLAG NOT = "B"
                 GO TO REWRITE-POOL.

           ADD 1 TO CP-TOTAL-ACCOUNTS.
           MOVE 1 TO WK-SEQNO.

       CREATE-ALPHA-WK.
           MOVE CP-ID TO WK-POOL.
           MOVE 1 TO SUB.
           IF CP-ALPHA-FLAG = "Y"
              PERFORM GET-BORROWER
              MOVE BW-LNAME TO WK-LAST-NAME
              INSPECT WK-LAST-NAME REPLACING FIRST
                   "                                   " BY BW-FNAME
           ELSE
              MOVE LN-ACCUM-BAL TO WK-BALANCE.
           MOVE LN-OWNBR  TO WK-BRNO.
           MOVE LN-ACCTNO TO WK-ACCTNO.
           IF GLOBAL-QUEUING-FG = "Y"
              MOVE BR-MACHINE TO WK-MACHINE
              MOVE BR-DATA-PATH TO WK-DATA-PATH.

           PERFORM WRITE-WK-FILE.
           IF IO-BAD
              IF WK-SEQNO < 999
                 ADD 1 TO WK-SEQNO
                 GO TO CREATE-ALPHA-WK
              ELSE
                 GO TO IO-ERROR.

       REWRITE-POOL.
           MOVE '2' TO SQL-UPDATE-CONN-USE-SW.
           PERFORM SQL-SWAP-UPDATE-CONNECTION.
           PERFORM REWRITE-CP1-FILE.
           MOVE '1' TO SQL-UPDATE-CONN-USE-SW.
           PERFORM SQL-SWAP-UPDATE-CONNECTION.

       SKIP-COLLECTOR-POOL.
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

      * UPDATE LNFILE ONLY IF FIELD(S) HAVE CHANGED.  MB 11/2023
           IF LN-REC NOT = ORIG-LN-REC
               PERFORM REWRITE-LN3-FILE
               IF IO-BAD
                  GO TO IO-ERROR.

           GO TO READ-LN3FILE-NEXT.

       END-OF-GROUP.
           IF GLOBAL-QUEUING-FG NOT = "Y"
              PERFORM ALPHA-SPLIT-ROUTINE
              PERFORM OPEN-WK-FILE-OUTPUT.

           PERFORM CLOSE-LN3-FILE.
           PERFORM CLOSE-BW1-FILE.
           PERFORM CLOSE-CP1-FILE.

           GO TO NEXT-GROUP-LOC.

       END-ROUTINE.
           IF GLOBAL-QUEUING-FG = "Y"
              PERFORM ALPHA-SPLIT-ROUTINE.

           GO TO END-PROGRAM.

      *********************************************
       INITIALIZATION SECTION.
      *********************************************
           MOVE CPRUN-QUEUE-POS  TO Q-POS.
           MOVE CPRUN-COPIES-POS TO C-POS.
           MOVE CPRUN-PRU-POS    TO P-POS.
           MOVE SPACES TO Q-BUF C-BUF P-BUF.

       COPY "LIBGB/EOINIT.CPY".

           MOVE EXTRDATE TO HOLD-DATE.

           IF EXT-ROUTE-BUF = "00"
              MOVE "EXTRACTION RUN IN PROGRESS.........." TO WR-BUF
              MOVE MESS-LIST TO WR-LIST
              PERFORM CALL-WRFORM.

           IF ( BEG-BRANCH = 0 AND END-BRANCH = 0 )
              MOVE BEG-BRANCH TO SV-BEG-BR
              MOVE END-BRANCH TO SV-END-BR
              MOVE EXT-CONAME-USER-LOC TO BEG-BRANCH END-BRANCH.

           PERFORM OPEN-CD1-FILE.
           MOVE "EA"         TO CD-TYPE.
           MOVE BEG-GROUP-ID TO CD-CODE.
           PERFORM READ-CD1-FILE.
           PERFORM CLOSE-CD1-FILE.
           IF ( IO-BAD )
              GO TO INITIALIZATION-END.

           IF CD-EA-CQ-FG = "Y"
              GO TO INITIALIZATION-END.

           MOVE CD-EA-GQ-FG TO GLOBAL-QUEUING-FG.

           MOVE MKTEMP-DEFAULT TO MKTEMP-BUF.
           PERFORM MKTEMP-CALL.
           PERFORM OPEN-WK-FILE-OUTPUT.
           PERFORM CLOSE-WK-FILE.

           PERFORM OPEN-WK-FILE.
           PERFORM OPEN-BR-FILE.
           PERFORM OPEN-GR1-FILE.
           PERFORM OPEN-EA1-FILE.
      * MUST LEAVE SP1-FILE OPEN BECAUSE ADDONSPR READS IT
           PERFORM OPEN-SP1-FILE.
           PERFORM OPEN-FD1-FILE.
           PERFORM OPEN-FM1-FILE.

           IF GLOBAL-QUEUING-FG = "Y"
              PERFORM GLOBAL-INIT-COLLECTOR-POOLS.

           PERFORM ARRAYSP-LOAD.

           PERFORM SQL-OPEN-UPDATE-CONNECTION.

           GO TO INITIALIZATION-EXIT.

       INITIALIZATION-END.
           MOVE "END" TO RTN-FG.

       INITIALIZATION-EXIT.
           EXIT.

      ************************
       EO-EXEC-AUDIT SECTION.
      ************************

           MOVE EXT-EOBUF-DATE2 TO EXTRDATE.

      ************************
       ENTER-PARAMETERS SECTION.
      ************************
       ENTER-PARM.

      *%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
           MOVE "R"    TO P-TIMER-TYPE-SET.
      *%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
           IF VDUSTATUS = "1U"
              GO TO CLRAW.

       EXTRACT-DATE.
      * BATCH WILL USE DATE RUN
           IF EXT-ROUTE-BUF = "40" OR "50"
              GO TO BR-01.

           MOVE "ERNM060EXTRDT." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO EXTRACT-DATE.
           MOVE VDU-DATE-YYYYMMDD TO EXTRDATE HOLD-DATE.

           IF EXTRDATE = ZERO
              MOVE EXT-CONAME-SYSDATE TO ALP-DATE
              PERFORM CONVERT-ALPDATE-TO-YYYYMMDD
              MOVE "S  8000EXTRDT." TO VDU
              MOVE DATE-YYYYMMDD     TO VDU-DATE-YYYYMMDD 
                                        EXTRDATE HOLD-DATE
              PERFORM SCREENIO.

      *----------------------------------------------------------------
       BR-01.
           MOVE "R  A000BR1." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "16" GO TO ALL-BR.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO EXTRACT-DATE.

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
       CL-01.
           MOVE "ENNN020CL1." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "16" GO TO ALL-CL.
           IF VDUSTATUS = "1U" GO TO BR-01.
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

           PERFORM OPEN-CD1-FILE.
           MOVE "EA"   TO CD-TYPE.
           MOVE VDUBUF TO CD-CODE BEG-GROUP-ID.
           PERFORM READ-CD1-FILE.
           PERFORM CLOSE-CD1-FILE.
           IF ( IO-BAD )
              GO TO GROUP-ID-YN.

           IF CD-EA-CQ-FG = "Y"
              MOVE "PLEASE USE `CREATE COLLECTOR QUEUE' FOR THIS GROUP"
                                     TO MESS
              PERFORM SEND-MESS
              GO TO GROUP-ID-YN.
      *-----------------------------------------------------------------
       CLRAW.
           MOVE "Y" TO CLEAR-AWFILE.

      *-----------------------------------------------------------------
       REPLACE-COL.
           MOVE "E  A010REPLACECOL." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U" GO TO GROUP-ID-YN.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO REPLACE-COL.
           MOVE VDUBUF TO REPLACE-COL-FG.
           IF REPLACE-COL-FG = "N" GO TO EXIT-PARM.
           IF REPLACE-COL-FG NOT = "Y" GO TO REPLACE-COL.

       EXIT-PARM.
           EXIT.

      ****************************
       DISPLAY-PARAMETERS SECTION.
      ****************************
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
           MOVE "S  A010REPLACECOL." TO VDU.
           MOVE REPLACE-COL-FG TO VDUBUF.
           PERFORM SCREENIO.

      ************************************
       TEST-EA1-FIELDS SECTION.
      ************************************

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

      * TEST LOAN CONFIDENTIAL FLAG
           IF EA-CONFIDENTIAL-FG = "O"
              IF LN-CONFIDENTCD NOT = "Y"
      * BOTH FLAGS MUST NOT BE SET
                 IF BW-CONFIDENTIAL NOT = "Y"
                    GO TO TEST-EA1-FIELDS-EXIT.

           IF EA-CONFIDENTIAL-FG = "S"
      * IF EITHER FLAG IS SET, SKIP IT
              IF LN-CONFIDENTCD = "Y" OR BW-CONFIDENTIAL = "Y"
                 GO TO TEST-EA1-FIELDS-EXIT.

      * THIS ENTIRE CHECK NOT DONE IN LN3 STORED PROCEDURE CURSOR
      * TEST FOR 1ST PAYMENT MISSES
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

      * THIS ENTIRE CHECK NOT DONE IN LN3 STORED PROCEDURE CURSOR
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

      * TEST FOR P&L
           IF EA-PL = "O"
              IF LN-PLDATE = 0
                 GO TO TEST-EA1-FIELDS-EXIT.

           IF EA-PL = "S"
              IF LN-PLDATE NOT = 0
                 GO TO TEST-EA1-FIELDS-EXIT.

           IF LN-PLDATE NOT = 0
              IF EA-PL-IN-ACTIVE-BOTH = "A"
                 IF LN-PLCD NOT = "P"
                    GO TO TEST-EA1-FIELDS-EXIT.

           IF LN-PLDATE NOT = 0
              IF EA-PL-IN-ACTIVE-BOTH = "I"
                 IF LN-PLCD NOT = "I"
                    IF LN-PLCD NOT = "W"
                       GO TO TEST-EA1-FIELDS-EXIT.

      * TEST FOR REPO
           IF EA-REPO = "O"
              IF NOT ( LN-REPOCD = "X" OR "S" )
                 GO TO TEST-EA1-FIELDS-EXIT.

           IF EA-REPO = "S"
              IF ( LN-REPOCD = "X" OR "S" )
                 GO TO TEST-EA1-FIELDS-EXIT.

      * TEST FOR BANKRUPT
           IF EA-BANKRUPT = "O"
              IF LN-BNKRPTDATE = 0
                 GO TO TEST-EA1-FIELDS-EXIT.

           IF EA-BANKRUPT = "S"
              IF LN-BNKRPTDATE NOT = 0
                 GO TO TEST-EA1-FIELDS-EXIT.

      * TEST FOR ALLOTMENTS
           IF EA-ALLOTMENTS = "O"
              IF LN-ALLOTCD NOT = "Y"
                 GO TO TEST-EA1-FIELDS-EXIT.

           IF EA-ALLOTMENTS = "S"
              IF LN-ALLOTCD  = "Y"
                 GO TO TEST-EA1-FIELDS-EXIT.

      *  TEST ACCOUNT BALANCES:
           COMPUTE LN-ACCUM-BAL = LN-CURBAL + LN-OTHBAL + LN-INTBAL
                                + LN-LCBAL.
           IF LN-ACCUM-BAL < EA-ACCT-BAL1 OR
              LN-ACCUM-BAL > EA-ACCT-BAL2
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

           MOVE RECDEL TO RECENCY.

           IF RECENCY < 0
              MOVE 0 TO RECENCY.

           IF RECENCY < EA-RECENCY-DAY1
                    OR > EA-RECENCY-DAY2
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
                 GO TO TEST-LSTFM-NO
               ELSE
                 GO TO TEST-PASTPROM-DAY.
           IF EA-PROMISED = "S"
              IF LN-PROMDATE NOT = 0
                 GO TO TEST-EA1-FIELDS-EXIT
               ELSE
                 GO TO TEST-LSTFM-NO.
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

      *  TEST LAST FORM NO:
       TEST-LSTFM-NO.
           IF LN-LASTCFORMNO < EA-LSTFM-NO1
            OR LN-LASTCFORMNO > EA-LSTFM-NO2
               GO TO TEST-EA1-FIELDS-EXIT.

      *  TEST LAST FORM DATES:
       TEST-LSTFM-DATE.
           IF EA-LSTFM-DATE1 = EXT-JULIAN-BEG
              IF EA-LSTFM-DATE2 = EXT-JULIAN-END
                 GO TO TEST-ACTION.
           IF LN-LASTCFORMDATE = 0
              GO TO TEST-ACTION.

           IF LN-LASTCFORMDATE < EA-LSTFM-DATE1 OR
               LN-LASTCFORMDATE > EA-LSTFM-DATE2
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
  
      * TEST ORIGIN
       TEST-ORIGIN.
           MOVE EA-ORIGIN TO TEST-ACTION-TABLE1.
           MOVE LN-ORIGIN TO TEST-ACTION-TABLE2.
           MOVE 0 TO SUB.

       TEST-ORIGIN-CONT.
           ADD 1 TO SUB.
           IF SUB > 4
              GO TO TEST-LNAME.
           IF TEST-ACTION-TAB1(SUB) = "?"
              GO TO TEST-ORIGIN-CONT.
           IF TEST-ACTION-TAB1(SUB) NOT = TEST-ACTION-TAB2(SUB)
              GO TO TEST-EA1-FIELDS-EXIT.
           GO TO TEST-ORIGIN-CONT.

      *  TEST LAST NAME RANGE:
       TEST-LNAME.
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
                 GO TO SKIP-MATURITY-TEST.
           IF EA-MATURE-DAYS1 = 0
              IF EA-MATURE-DAYS2 = 9999
                 GO TO SKIP-MATURITY-TEST.

           PERFORM MATURITY-DATE-CALCULATION.
           MOVE HOLD-DATE TO NUM-DATE.
           MOVE MDTE-DATE TO SYS-DATE.
           PERFORM TIM.
           IF ELAPSED-DAYS < EA-MATURE-DAYS1
            OR (ELAPSED-DAYS > EA-MATURE-DAYS2)
              GO TO TEST-EA1-FIELDS-EXIT.

       SKIP-MATURITY-TEST.
      *  TEST BORROWER STATE:
       TEST-STATE.
           IF EA-BORR-STATE NOT = "  "
              IF BW-STATE NOT = EA-BORR-STATE
                 GO TO TEST-EA1-FIELDS-EXIT.

       TEST-EA1-FIELDS-AWUPD.
           MOVE "AW" TO RTN-FG.

       TEST-EA1-FIELDS-EXIT.
           EXIT.

      **************************************************
       ALPHA-SPLIT-ROUTINE SECTION.
      **************************************************
           MOVE "~~~" TO HOLD-POOL-ID.
           IF ( GLOBAL-QUEUING-FG = "Y" )
              MOVE EXT-FILPATH-BASE        TO CP-GLOBAL-BASE
              MOVE EXT-FILPATH-MACHINE     TO CP-GLOBAL-MACHINE
              MOVE CP-GLOBAL-PATH          TO CP-PATH
              MOVE "G"                     TO CP-PATH-OVERRIDE.

           PERFORM OPEN-CP1-FILE.
           PERFORM OPEN-WK-FILE.

           MOVE SPACES TO WK-POOL WK-LAST-NAME.
           MOVE 0      TO WK-SEQNO.
           PERFORM START-WK-FILE.
       NEXT-ALPHA-SPLIT.
           PERFORM READ-WK-FILE-NEXT.
           IF IO-BAD
              GO TO EXIT-ALPHA-SPLIT.

           IF WK-POOL NOT = HOLD-POOL-ID
              MOVE WK-POOL TO CP-ID
              PERFORM READ-CP1-FILE
      *       UNLOCK CP-FILE
              IF IO-BAD
                 GO TO NEXT-ALPHA-SPLIT
              ELSE
                 MOVE CP-ID TO HOLD-POOL-ID
                 MOVE 0 TO CURRENT-COLLECTOR-COUNTER
                 DIVIDE CP-TOTAL-ACCOUNTS BY CP-COLLECTOR-COUNT
                    GIVING TOTAL-PER-COLLECTOR ROUNDED
                 MOVE 1 TO COLLECTOR-SUB.
           IF CP-ALPHA-FLAG = "Y"
              IF CURRENT-COLLECTOR-COUNTER NOT < TOTAL-PER-COLLECTOR
                 IF COLLECTOR-SUB < CP-COLLECTOR-COUNT
                    ADD 1 TO COLLECTOR-SUB
                    MOVE 0 TO CURRENT-COLLECTOR-COUNTER
                    GO TO UPDATE-SPLIT-COLLECTOR.

           IF CP-ALPHA-FLAG = "B"
              IF UP-DOWN-FG = "U"
                 IF COLLECTOR-SUB < CP-COLLECTOR-COUNT
                    ADD 1 TO COLLECTOR-SUB
                 ELSE
                    MOVE "D" TO UP-DOWN-FG
                 END-IF
                 GO TO UPDATE-SPLIT-COLLECTOR.

           IF CP-ALPHA-FLAG = "B"
              IF UP-DOWN-FG = "D"
                 IF COLLECTOR-SUB > 1
                    SUBTRACT 1 FROM COLLECTOR-SUB
                 ELSE
                    MOVE "U" TO UP-DOWN-FG.

       UPDATE-SPLIT-COLLECTOR.
           IF GLOBAL-QUEUING-FG = "Y"
              MOVE EXT-FILPATH-BASE        TO LN-ALL-BASE
              MOVE WK-MACHINE              TO LN-ALL-MACHINE
              MOVE WK-DATA-PATH            TO LN-ALL-DATA
              MOVE LN-ALL-PATH             TO LN-PATH
              MOVE "Y"                     TO LN-PATH-OVERRIDE.

      * UPDATE LN-COLLECTOR IN LNFILE WITH CP-COL-ID(COLLECTOR-SUB)
      * NO NEED TO PERFORM READ FIRST.  MB 11/2023 
      * CHANGED TO USE LN3 INSTEAD OF LN1 SINCE PROGRAM ALREADY USES LN3

      *     PERFORM OPEN-LN1-FILE.
           PERFORM OPEN-LN3-FILE.
           MOVE WK-BRNO   TO LN-OWNBR.
           MOVE WK-ACCTNO TO LN-ACCTNO.
      *    PERFORM READ-LN1-FILE.
      *    IF IO-FG = 0
           MOVE CP-COL-ID(COLLECTOR-SUB) TO LN-COLLECTOR.
      *     PERFORM REWRITE-LN1-FILE.
           PERFORM REWRITE-LN3-FILE-LMTD.
           ADD 1 TO CURRENT-COLLECTOR-COUNTER.

           IF GLOBAL-QUEUING-FG = "Y"
              MOVE "B"                     TO LN-PATH-OVERRIDE.

      *     PERFORM OPEN-LN1-FILE.
      *     PERFORM CLOSE-LN1-FILE.

           PERFORM OPEN-LN3-FILE.
           PERFORM CLOSE-LN3-FILE.

           GO TO NEXT-ALPHA-SPLIT.

       EXIT-ALPHA-SPLIT.
           PERFORM CLOSE-WK-FILE.
           PERFORM CLOSE-CP1-FILE.
           IF ( GLOBAL-QUEUING-FG = "Y" )
              MOVE "G"                     TO CP-PATH-OVERRIDE.

      **********************************************
      * RECENCY FOR THIS PROGRAM ONLY COMPUTED ON 1STPYDATE
      * INSTEAD OF LOANDATE (WHEN LSTPAYMNT = 0):
      **********************************************
       COMPUTE-RECENCY-SUB SECTION.
      **********************************************
           PERFORM READ-SP-RECORD.

           MOVE HOLD-DATE TO AGEING-DATE.
           MOVE LN-LOANDATE TO HOLD-LOAN-DATE.
           IF EA-RECENCYDATE NOT = "Y"
              MOVE LN-1STPYDATE TO LN-LOANDATE.

           PERFORM COMPUTE-RECENCY.

           MOVE HOLD-LOAN-DATE TO LN-LOANDATE.
           MOVE "Y" TO X-RECENCY.

      ******************************
       COMPUTE-CONTRACT-SUB SECTION.
      ******************************
           PERFORM READ-SP-RECORD.
           MOVE HOLD-DATE TO AGEING-DATE.
           PERFORM COMPUTE-CONTRACTUAL.
           MOVE "Y" TO X-CONTRACT.

      ******************************
      * COMPUTE PASTDUE AMOUNT:
      ******************************
       COMPUTE-ARREAR SECTION.
      ******************************
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

      ******************************
      * COMPUTE PASTDUE DAYS:
      ******************************
       COMPUTE-PASTDUE SECTION.
           PERFORM READ-SP-RECORD.
           MOVE LN-1STPYDATE TO PDUE-1STPYDATE.
           MOVE HOLD-DATE    TO PDUE-TODAY-DATE.
           PERFORM PAST-DUE-DAYS-CALCULATION.
           MOVE "Y" TO X-PDAYS.

      ******************************
       CHECK-FOR-MORE-FORMS SECTION.
      ******************************
           MOVE 0 TO SUB.
       CHECK-NEXT-EA.
           ADD 1 TO SUB.
           IF SUB > 3
              GO TO CHECK-FOR-MORE-EXIT.

      * REMOVED NULL CHECK (20) SINCE SQL NOT ALLOW NULLS AND 202/2020
      * WOULD BE A VALID VALUE. MB 11/2023
           IF EA-SETFM(SUB) = 0 
              GO TO CHECK-NEXT-EA.

           IF LN-SSNO(SUB + 1) = 0
              GO TO CHECK-NEXT-EA.

      * DONT CREATE FM REQUEST FOR SPOUSE, JUST ENDORSER OR COMAKER
           IF LN-SSNO(SUB + 1) = BW-SPSSNO
              GO TO CHECK-NEXT-EA.

       CREATE-REQUEST.
           MOVE EA-SETFM(SUB) TO AW-SETFM(SUB).
           GO TO CHECK-NEXT-EA.

       CHECK-FOR-MORE-EXIT.
           EXIT.

      *************************************
       INITIALIZE-COLLECTOR-POOLS SECTION.
      *************************************
           PERFORM OPEN-CP1-FILE.

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
           IF EA-CLASS > END-CLASS OR
               EA-GROUP-ID NOT = BEG-GROUP-ID
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
           MOVE 0 TO CP-TOTAL-ACCOUNTS.

           MOVE '2' TO SQL-UPDATE-CONN-USE-SW.
           PERFORM SQL-SWAP-UPDATE-CONNECTION.
           PERFORM REWRITE-CP1-FILE.
           MOVE '1' TO SQL-UPDATE-CONN-USE-SW.
           PERFORM SQL-SWAP-UPDATE-CONNECTION.

           GO TO CLEAR-CP-NEXT.
       END-INITIALIZE-POOLS.
           PERFORM CLOSE-CP1-FILE.

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
      *************************************
       GLOBAL-INIT-COLLECTOR-POOLS SECTION.
      *************************************
           MOVE EXT-FILPATH-BASE           TO CP-GLOBAL-BASE
           MOVE EXT-FILPATH-MACHINE        TO CP-GLOBAL-MACHINE
           MOVE CP-GLOBAL-PATH             TO CP-PATH.
           MOVE "G"                        TO CP-PATH-OVERRIDE

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

           IF EA-CLASS > END-CLASS OR
               EA-GROUP-ID NOT = BEG-GROUP-ID
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

           MOVE '2' TO SQL-UPDATE-CONN-USE-SW.
           PERFORM SQL-SWAP-UPDATE-CONNECTION.
           PERFORM REWRITE-CP1-FILE.
           MOVE '1' TO SQL-UPDATE-CONN-USE-SW.
           PERFORM SQL-SWAP-UPDATE-CONNECTION.

           GO TO GLOBAL-CLEAR-CP-NEXT.

       GLOBAL-END-INITIALIZE-POOLS.
           PERFORM CLOSE-CP1-FILE.

      ******************************************************************
       CODE-SCAN SECTION.
      ******************************************************************
           MOVE "EA"       TO CDSCAN-TYPE.
           MOVE SPACES     TO CDSCAN-CODE.
           MOVE "N"        TO CDSCAN-EA-CQ-FG.

           MOVE "WI/CDSCAN" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH CDSCAN-BUF EXIT-PATHNAME.
           CANCEL FORM-PROGX.
           IF ( CDSCAN-STATUS NOT = "00" ) OR ( CDSCAN-CODE = " " )
               MOVE 9 TO IO-FG
               GO TO EXIT-CODE-SCAN.

           MOVE "S  A060GROUP-ID." TO VDU
           MOVE CDSCAN-CODE        TO VDUBUF
           PERFORM SCREENIO.

           MOVE CDSCAN-CODE TO VDUBUF.
           MOVE 0            TO IO-FG.

       EXIT-CODE-SCAN.
           EXIT.


      ******************************************************************
      *
      *    C O P Y - L I B R A R Y - S E C T I O N S
      *
      ******************************************************************
       COPY "LIBGB/GPENV.CPY".
       COPY "LIBLP/LPCPUPD.CPY".
       COPY "LIBLP/LPRATE.CPY".
       COPY "LIBLP/LPPDUE.CPY".
       COPY "LIBLP/LPCDEL.CPY".
       COPY "LIBLP/LPTDEL.CPY".
       COPY "LIBGB/SPPDTH.CPY".
       COPY "LIBLP/LPSCHD.CPY".
       COPY "LIBLP/LPLCAS.CPY".
       COPY "LIBLP/LPLCHG.CPY".
       COPY "LIBLP/LPLCAP.CPY".
       COPY "LIBLP/DEFPOL.CPY".
       COPY "LIBLP/LPPOF2.CPY".
       COPY "LIBLP/LPITRM.CPY".
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

       COPY "LIBLP/ARRAYSP.CPY".

      *********************************************************
       FORM-RESET SECTION.
      *********************************************************
           MOVE EXT-CONAME-CONAME TO CPRUN-CONAME.
           MOVE EXT-CONAME-SYSDATE TO CPRUN-SYSDATE.
           DISPLAY CPRUN-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE CPRUN-FORM-FIELDS TO WR-BUF.
           MOVE CPRUN-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.
       FORM-RESET-EXIT.
           EXIT.

      *********************************************************
       SETUP-LINK-INFO SECTION.
      *********************************************************
           MOVE CPRUN-HELP--FILE TO HELP-LINK-FILE.
           MOVE CPRUN-HELP--DIR  TO HELP-LINK-DIR.

      *********************************************************
       VDU-CONVERT-FIELD SECTION.
      *********************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBLP/CPRUN_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      *******************************
       MISCELLANEOUS-ROUTINES SECTION.
      *******************************
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/GETPR.CPY".
       COPY "LIBGB/GETENV.CPY".
       COPY "LIBGB/MKTEMP.CPY".
       COPY "LIBGB/RMFILE.CPY".
       COPY "LIBGB/SYSTEM.CPY".
       READ-SP-RECORD.
      * DO NOT USE A HOLD FIELD FOR THE KEY, LCHG ROUTINES CHANGE SPR FIELDS
      * NEEDS A CLEAN READ EVEY TIME
           MOVE LN-ORGST    TO SP-ORGST.
           MOVE LN-SPRCLASS TO SP-SPRCLASS.
           MOVE LN-SUBCLASS TO SP-SUBCLASS.
           MOVE LN-LAWCODE  TO SP-LAWCODE.
      *     PERFORM READ-SP1-FILE.
           PERFORM ARRAYSP-READ.
           IF IO-BAD
              PERFORM FILE-ERRORS
              PERFORM SEND-MESS
              MOVE "NL" TO RTN-FG.

       GET-MAKER.
           MOVE 0 TO IO-FG.
           IF LN-SSNO(1) NOT = HOLD-BW-SSNO
              MOVE LN-SSNO(1) TO BW-SSNO HOLD-BW-SSNO
              PERFORM READ-BW1-FILE
              IF IO-BAD
                 MOVE SPACES TO BW-ZIP
                 MOVE "A" TO BW-LNAME.
       GET-BORROWER.
           MOVE LN-SSNO(SUB) TO BW-SSNO.
           PERFORM READ-BW1-FILE.
           IF IO-BAD
              MOVE SPACES TO BW-ZIP.


      **************************************************
       IO-ROUTINE SECTION.
      **************************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBSP/SPGQGS_SQL.CPY".
       COPY "LIBSP/SPCQGS_SQL.CPY".
       COPY "LIBSP/SPCPGS_SQL.CPY".
       COPY "LIBLP/LPSPGS_SQL.CPY".
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
       COPY "LIBGB/GBPRINT.CPY".

       COPY "LIBLP/LPSP1RN.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBGB/GBGBRN.CPY".
       COPY "LIBLP/LPBW1RN.CPY".
       COPY "LIBLP/LPFD1RN.CPY".
       COPY "LIBLP/LPLTP1RN.CPY".
       COPY "LIBLP/LPLTI1RN.CPY".
       COPY "LIBLP/LPLP1RN.CPY".
       COPY "LIBLP/LPCD1RN.CPY".
       COPY "LIBGB/GBGR1RN.CPY".
       COPY "LIBLP/LPFM1IN.CPY".
       COPY "LIBSP/SPGQ1IN.CPY".
       COPY "LIBSP/SPCP1IN.CPY".
       COPY "LIBSP/SPCQ1IN.CPY".

       COPY "LIBLP/LPEA1RN.CPY".
      *-----------------------------------------------------------------
       COUNT-EA1-FILE.
           PERFORM COUNT-IT.
           MOVE EA-PATH-SQL TO E-FILE.
           MOVE SPACES      TO E-KEYX.

           EXEC SQL
            SELECT COUNT(*)
            INTO :WS-EA1-ROWCOUNT
            FROM DBO.EAFILE
            WHERE EAFILE.EA_BRANCH = :QEA1-WBEG-BRANCH
              AND EAFILE.EA_CLASS BETWEEN :QEA1-WBEG-CLASS
              AND :QEA1-WEND-CLASS
              AND EAFILE.EA_GROUP_ID = :QEA1-WBEG-GROUP-ID
          END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO COUNT-EA1-FILE.
      *-----------------------------------------------------------------


      * CHANGED PROGRAM TO USE LN3 (IN UPDATE FROM WORK FILE) INSTEAD OF LN1
      *  SINCE PROGRAM WAS ALREADY USING LN3
      * COPY "LIBLP/LPLN1IN.CPY".

      * COPY "LIBLP/LPLN3IN.CPY".
      * COPYMEMBER: LIBLP/LPLN3IN
      ***********************************************************
      *                (LPLN3IN)
      * REV:
      *  JTG 081497 CHANGED LN-SALEFIN TO LN-GIFTNET WORLD A90
      *  BLV 012298 RENAME UPDATE-BI-FILE PARAGRAPH TO
      *             LN3-UPDATE-BI-FILE (CONFLICT WITH LPLN1IN)
      *  BLV 020698 ADDED LN-SALEFIN BACK TO BICREA CALL
      *  MJD ADDED LN-PLCD AND LN-CONVERCD TO BICREA CALL
      *  BAH 04/22/2016 ADDED LN-SMALLOAN TO BICREA CALL, REGENCY PR#5030
      *  BAH 20180316 ADDED LN-PLDATE TO BICREA CALL, WORLD #1308
      ***********************************************************
       LN3-UPDATE-BI-FILE.
           IF IO-FG = 0
              MOVE "CL/BICREA" TO FORM-NAM
              CALL FORM-PROGX USING FORM-PATH
                                   STAT
                                   LN-ACCTNO
                                   LN-BWTABLE
                                   LN-OWNBR
                                   LN-ORGST
                                   LN-SALEFIN
                                   LN-GIFTNET
                                   LN-ENTDATE
                                   LN-POFFDATE
                                   LN-PLCD
                                   LN-CONVERCD
                                   "000000000"
                                   LN-SMALLOAN
                                   LN-PLDATE
             CANCEL FORM-PROGX.

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
       START-LN3-FILE.  *> SSP LN BW 
           PERFORM START-IT.
           MOVE LN-PATH-OWNBR  TO LN-OWNBR.
           MOVE LN-PATH-SQL TO E-FILE.
           MOVE SPACES         TO E-KEYX.
           STRING LN-OWNBR, LN-CLASS
              DELIMITED BY " " INTO E-KEYX.

           IF ( LN3-CURSOR-STAT-GOOD )
              PERFORM CLOSE-LN3-FILE.

      *  SELECT *
      *  FROM (SELECT LNFILE.LN_OWNBR,
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
      *            CAST(LNFILE.LN_ENTDATE  AS VARCHAR(10)) AS LN_ENTDATE,
      *            CAST(LNFILE.LN_LOANDATE AS VARCHAR(10)) AS LN_LOANDATE,
      *            CAST(LNFILE.LN_PURDATE  AS VARCHAR(10)) AS LN_PURDATE,
      *            CAST(LNFILE.LN_INTDATE  AS VARCHAR(10)) AS LN_INTDATE,
      *            CAST(LNFILE.LN_CASHDATE AS VARCHAR(10)) AS LN_CASHDATE,
      *            CAST(LNFILE.LN_FILEDATE AS VARCHAR(10)) AS LN_FILEDATE,
      *            CAST(LNFILE.LN_RESCISSION_DATE    AS VARCHAR(10)) AS LN_RESCISSION_DATE,
      *            CAST(LNFILE.LN_PART_RECOURSE_DATE AS VARCHAR(10)) AS LN_PART_RECOURSE_DATE,
      *            CAST(LNFILE.LN_ORIG_1STPYDATE     AS VARCHAR(10)) AS LN_ORIG_1STPYDATE,
      *            CAST(LNFILE.LN_1STPYDATE  AS VARCHAR(10)) AS LN_1STPYDATE,
      *            CAST(LNFILE.LN_JDDATE     AS VARCHAR(10)) AS LN_JDDATE,
      *            CAST(LNFILE.LN_PLDATE     AS VARCHAR(10)) AS LN_PLDATE,
      *            CAST(LNFILE.LN_PL_TRDATE  AS VARCHAR(10)) AS LN_PL_TRDATE,
      *            CAST(LNFILE.LN_ACCELDATE  AS VARCHAR(10)) AS LN_ACCELDATE,
      *            CAST(LNFILE.LN_REDUDATE   AS VARCHAR(10)) AS LN_REDUDATE,
      *            CAST(LNFILE.LN_POFFDATE   AS VARCHAR(10)) AS LN_POFFDATE,
      *            CAST(LNFILE.LN_PO_TRDATE  AS VARCHAR(10)) AS LN_PO_TRDATE,
      *            CAST(LNFILE.LN_LEGALDATE  AS VARCHAR(10)) AS LN_LEGALDATE,
      *            CAST(LNFILE.LN_BNKRPTDATE AS VARCHAR(10)) AS LN_BNKRPTDATE,
      *            CAST(LNFILE.LN_BNKRPT_DISMISS_DATE   AS VARCHAR(10)) AS LN_BNKRPT_DISMISS_DATE,
      *            CAST(LNFILE.LN_BNKRPT_DISCHARGE_DATE AS VARCHAR(10)) AS LN_BNKRPT_DISCHARGE_DATE,
      *            CAST(LNFILE.LN_BNKRPT_REAFFIRM_DATE  AS VARCHAR(10)) AS LN_BNKRPT_REAFFIRM_DATE,
      *            CAST(LNFILE.LN_DEBTAIDATE            AS VARCHAR(10)) AS LN_DEBTAIDATE,
      *            CAST(LNFILE.LN_REPODATE   AS VARCHAR(10)) AS LN_REPODATE,
      *            CAST(LNFILE.LN_REDEEMDATE AS VARCHAR(10)) AS LN_REDEEMDATE,
      *            CAST(LNFILE.LN_SOLDDATE   AS VARCHAR(10)) AS LN_SOLDDATE,
      *            CAST(LNFILE.LN_INS_CONFIRM_DATE     AS VARCHAR(10)) AS LN_INS_CONFIRM_DATE,
      *            CAST(LNFILE.LN_PRIOR_RN_FOR10_DATE  AS VARCHAR(10)) AS LN_PRIOR_RN_FOR10_DATE,
      *            CAST(LNFILE.LN_LAST_RN_FOR10_DATE   AS VARCHAR(10)) AS LN_LAST_RN_FOR10_DATE,
      *            CAST(LNFILE.LN_TRW_DOFD_DATE        AS VARCHAR(10)) AS LN_TRW_DOFD_DATE,
      *            CAST(LNFILE.LN_LAST_PYPA_DATE       AS VARCHAR(10)) AS LN_LAST_PYPA_DATE,
      *            CAST(LNFILE.LN_BT_TRDATE            AS VARCHAR(10)) AS LN_BT_TRDATE,
      *            CAST(LNFILE.LN_METRO2_LAST_PMT_DATE AS VARCHAR(10)) AS LN_METRO2_LAST_PMT_DATE,
      *            CAST(LNFILE.LN_FOLLOWUP_DATE        AS VARCHAR(10)) AS LN_FOLLOWUP_DATE,
      *            CAST(LNFILE.LN_LAST_ACCRUAL_DATE    AS VARCHAR(10)) AS LN_LAST_ACCRUAL_DATE,
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
      *            CAST(LNFILE.LN_INSEFF_DATE_1 AS VARCHAR(10)) AS LN_INSEFF_DATE_1,
      *            CAST(LNFILE.LN_INSEXP_DATE_1 AS VARCHAR(10)) AS LN_INSEXP_DATE_1,
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
      *            CAST(LNFILE.LN_INSEFF_DATE_2 AS VARCHAR(10)) AS LN_INSEFF_DATE_2,
      *            CAST(LNFILE.LN_INSEXP_DATE_2 AS VARCHAR(10)) AS LN_INSEXP_DATE_2,
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
      *            CAST(LNFILE.LN_INSEFF_DATE_3 AS VARCHAR(10)) AS LN_INSEFF_DATE_3,
      *            CAST(LNFILE.LN_INSEXP_DATE_3 AS VARCHAR(10)) AS LN_INSEXP_DATE_3,
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
      *            CAST(LNFILE.LN_INSEFF_DATE_4 AS VARCHAR(10)) AS LN_INSEFF_DATE_4,
      *            CAST(LNFILE.LN_INSEXP_DATE_4 AS VARCHAR(10)) AS LN_INSEXP_DATE_4,
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
      *            CAST(LNFILE.LN_INSEFF_DATE_5 AS VARCHAR(10)) AS LN_INSEFF_DATE_5,
      *            CAST(LNFILE.LN_INSEXP_DATE_5 AS VARCHAR(10)) AS LN_INSEXP_DATE_5,
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
      *            CAST(LNFILE.LN_INSEFF_DATE_6 AS VARCHAR(10)) AS LN_INSEFF_DATE_6,
      *            CAST(LNFILE.LN_INSEXP_DATE_6 AS VARCHAR(10)) AS LN_INSEXP_DATE_6,
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
      *            CAST(LNFILE.LN_INSEFF_DATE_7 AS VARCHAR(10)) AS LN_INSEFF_DATE_7,
      *            CAST(LNFILE.LN_INSEXP_DATE_7 AS VARCHAR(10)) AS LN_INSEXP_DATE_7 ,
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
      *            CAST(LNFILE.LN_INSEFF_DATE_8 AS VARCHAR(10)) AS LN_INSEFF_DATE_8,
      *            CAST(LNFILE.LN_INSEXP_DATE_8 AS VARCHAR(10)) AS LN_INSEXP_DATE_8,
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
      *            CAST(LNFILE.LN_INSEFF_DATE_9 AS VARCHAR(10)) AS LN_INSEFF_DATE_9,
      *            CAST(LNFILE.LN_INSEXP_DATE_9 AS VARCHAR(10)) AS LN_INSEXP_DATE_9,
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
      *            CAST(LNFILE.LN_INSEFF_DATE_10 AS VARCHAR(10)) AS LN_INSEFF_DATE_10,
      *            CAST(LNFILE.LN_INSEXP_DATE_10 AS VARCHAR(10)) AS LN_INSEXP_DATE_10,
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
      *            CAST(LNFILE.LN_MIP_CLPDTH_DATE AS VARCHAR(10)) AS LN_MIP_CLPDTH_DATE,
      *            LNFILE.LN_MIP_TOT_CLPAID,
      *            CAST(LNFILE.LN_MIP_AHPDTH_DATE AS VARCHAR(10)) AS LN_MIP_AHPDTH_DATE,
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
      *            CAST(LNFILE.LN_LCPDTH_DATE  AS VARCHAR(10)) AS LN_LCPDTH_DATE,
      *            CAST(LNFILE.LN_INTPDTH_DATE AS VARCHAR(10)) AS LN_INTPDTH_DATE,
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
      *            CAST(LNFILE.LN_ERN_IB_PDTH_DATE AS VARCHAR(10)) AS LN_ERN_IB_PDTH_DATE,
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
      *            CAST(LNFILE.LN_PROMDATE AS VARCHAR(10)) AS LN_PROMDATE,
      *            LNFILE.LN_PROMAMT,
      *            CAST(LNFILE.LN_PROMDATE2 AS VARCHAR(10)) AS LN_PROMDATE2,
      *            CAST(LNFILE.LN_DATE_PAID_LAST AS VARCHAR(10)) AS LN_DATE_PAID_LAST,
      *            LNFILE.LN_AMT_PAID_LAST,
      *            LNFILE.LN_LASTCFORMNO,
      *            CAST(LNFILE.LN_LASTCFORMDATE AS VARCHAR(10)) AS LN_LASTCFORMDATE,
      *            LNFILE.LN_DELCNT,
      *            LNFILE.LN_DELCNT2,
      *            LNFILE.LN_CREDITLIM,
      *            LNFILE.LN_CRSCORE,
      *            LNFILE.LN_LASTSFORMNO,
      *            CAST(LNFILE.LN_LASTSFORMDATE AS VARCHAR(10)) AS LN_LASTSFORMDATE,
      *            LNFILE.LN_SOLCNT,
      *            LNFILE.LN_MESSAGE,
      *            LNFILE.LN_OLDACCTNO,
      *            LNFILE.LN_PRLNNO,
      *            LNFILE.LN_PRBAL,
      *            LNFILE.LN_PRINTDUE,
      *            LNFILE.LN_PRFINCHGRB,
      *            LNFILE.LN_PRINSRB,
      *            LNFILE.LN_PRPOCD,
      *            CAST(LNFILE.LN_PRPODATE AS VARCHAR(10)) AS LN_PRPODATE,
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
      *            CAST(LNFILE.LN_APPROVED_BY_DATE AS VARCHAR(10)) AS LN_APPROVED_BY_DATE,
      *            LNFILE.LN_APPROVED_BY_TIME,
      *            CAST(LNFILE.LN_LTOUCH_DATE AS VARCHAR(10)) AS LN_LTOUCH_DATE,
      *            LNFILE.LN_LTOUCH_TIME,
      *            ISNULL(BWFILE.BW_OWNBR,0),  
      *            ISNULL(BWFILE.BW_SSNO,0),
      *            ISNULL(BWFILE.BW_LNAME,''),
      *            ISNULL(BWFILE.BW_FNAME,''),
      *            ISNULL(BWFILE.BW_STATE,''),
      *            ISNULL(BWFILE.BW_ZIP,''),
      *            ISNULL(BWFILE.BW_SPSSNO,0),
      *            ISNULL(BWFILE.BW_BAD_ADR_FG,''),
      *            ISNULL(BWFILE.BW_CONFIDENTIAL,'')
      *   FROM DBO.LNFILE LNFILE
      *   LEFT OUTER JOIN DBO.BWFILE BWFILE
      *    ON BWFILE.BW_OWNBR = LNFILE.LN_OWNBR 
      *   AND BWFILE.BW_SSNO  = LNFILE.LN_SSNO1
      *   WHERE LNFILE.LN_OWNBR = @QLN3_WBEG_BRNO
      *    AND LNFILE.LN_CLASS BETWEEN @QLN3_WBEG_CLASS AND @QLN3_WEND_CLASS 
      *   ORDER BY LNFILE.LN_CLASS, LNFILE.LN_ACCTNO

           EXEC SQL SET CONNECTION LN3_CONNECT END-EXEC.

           EXEC SQL
            DECLARE LN3_CURSOR CURSOR FOR
                :LN3-RETURN-CODE = 
                 EXEC SSP_CPRUN_LNFILE_SELECT
                  (:QLN3-WBEG-OWNBR, 
                   :QLN3-WBEG-CLASS, :QLN3-WEND-CLASS) 
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
       READ-LN3-FILE-NEXT.  *> EMBEDDED LN BW
           PERFORM READ-IT.
           MOVE LN-PATH-OWNBR  TO LN-OWNBR.
           MOVE LN-PATH-SQL    TO E-FILE.
           MOVE SPACES         TO E-KEYX.
           STRING LN-OWNBR, LN-CLASS,LN-ACCTNO
              DELIMITED BY " " INTO E-KEYX.
           INITIALIZE QLN-REC QBW-REC.

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
                   :QBW-STATE, 
                   :QBW-ZIP, 
                   :QBW-SPSSNO,
                   :QBW-BAD-ADR-FG,
                   :QBW-CONFIDENTIAL
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
              PERFORM GET-BW-FIELDS.

      *-----------------------------------------------------------------
       REWRITE-LN3-FILE.
           MOVE LN-PATH-OWNBR  TO LN-OWNBR.
           ACCEPT LN-LTOUCH-DATE FROM CENTURY-DATE.
           PERFORM REWRITE-IT.
           MOVE LN-PATH-SQL  TO E-FILE.
           MOVE SPACES       TO E-KEYX.
           STRING LN-OWNBR, LN-CLASS,LN-ACCTNO
              DELIMITED BY " " INTO E-KEYX.
      D    STOP MESS.
           PERFORM SET-LN-FIELDS.

           MOVE '2' TO SQL-UPDATE-CONN-USE-SW.  *> SET CONNECTION TO UPDATE-CONNECT
           PERFORM SQL-SWAP-UPDATE-CONNECTION.

           EXEC SQL
            UPDATE DBO.LNFILE
             SET
                 LNFILE.LN_CLASS               = :QLN-CLASS,
                 LNFILE.LN_SUBCLASS            = :QLN-SUBCLASS,
                 LNFILE.LN_LAWCODE             = :QLN-LAWCODE,
                 LNFILE.LN_SSNO1               = :QLN-SSNO1, 
                 LNFILE.LN_MAKERCD1            = :QLN-MAKERCD1,
                 LNFILE.LN_SSNO2               = :QLN-SSNO2, 
                 LNFILE.LN_MAKERCD2            = :QLN-MAKERCD2,
                 LNFILE.LN_SSNO3               = :QLN-SSNO3,
                 LNFILE.LN_MAKERCD3            = :QLN-MAKERCD3,
                 LNFILE.LN_SSNO4               = :QLN-SSNO4,
                 LNFILE.LN_MAKERCD4            = :QLN-MAKERCD4,
                 LNFILE.LN_ORGST               = :QLN-ORGST,
                 LNFILE.LN_SPRCLASS            = :QLN-SPRCLASS,
                 LNFILE.LN_CONVERCD            = :QLN-CONVERCD,
                 LNFILE.LN_MIDSTREAM           = :QLN-MIDSTREAM,
                 LNFILE.LN_POCD                = :QLN-POCD,
                 LNFILE.LN_PLCD                = :QLN-PLCD,
                 LNFILE.LN_CONFIDENTCD         = :QLN-CONFIDENTCD,
                 LNFILE.LN_ALLOTCD             = :QLN-ALLOTCD,
                 LNFILE.LN_STATUSFG            = :QLN-STATUSFG,
                 LNFILE.LN_SALEFIN             = :QLN-SALEFIN,
                 LNFILE.LN_LOANTYPE            = :QLN-LOANTYPE,
                 LNFILE.LN_ADDON_FG            = :QLN-ADDON-FG, 
                 LNFILE.LN_PAY_SCHLD_FG        = :QLN-PAY-SCHLD-FG,
                 LNFILE.LN_LC_CODE             = :QLN-LC-CODE,
                 LNFILE.LN_LOANDATE            = :QLN-LOANDATE,
                 LNFILE.LN_PURDATE             = :QLN-PURDATE,
                 LNFILE.LN_INTDATE             = :QLN-INTDATE,
                 LNFILE.LN_CASHDATE            = :QLN-CASHDATE,
                 LNFILE.LN_ORIG_1STPYDATE      = :QLN-ORIG-1STPYDATE,
                 LNFILE.LN_1STPYDATE           = :QLN-1STPYDATE,
                 LNFILE.LN_JDDATE              = :QLN-JDDATE,
                 LNFILE.LN_PLDATE              = :QLN-PLDATE,
                 LNFILE.LN_ACCELDATE           = :QLN-ACCELDATE,
                 LNFILE.LN_REDUDATE            = :QLN-REDUDATE,
                 LNFILE.LN_LEGALDATE           = :QLN-LEGALDATE,
                 LNFILE.LN_BNKRPTDATE          = :QLN-BNKRPTDATE,
                 LNFILE.LN_DEBTAIDATE          = :QLN-DEBTAIDATE,
                 LNFILE.LN_APRATE              = :QLN-APRATE,
                 LNFILE.LN_EFFRATE             = :QLN-EFFRATE,
                 LNFILE.LN_SMPRATE             = :QLN-SMPRATE,
                 LNFILE.LN_OVRDRATE            = :QLN-OVRDRATE,
                 LNFILE.LN_ENDORATE            = :QLN-ENDORATE,
                 LNFILE.LN_JDRATE              = :QLN-JDRATE,
                 LNFILE.LN_ACCRATE             = :QLN-ACCRATE,
                 LNFILE.LN_REDURATE            = :QLN-REDURATE,
                 LNFILE.LN_LNAMT               = :QLN-LNAMT,
                 LNFILE.LN_FINCHG              = :QLN-FINCHG,
                 LNFILE.LN_INTCHG              = :QLN-INTCHG,
                 LNFILE.LN_SERCHG              = :QLN-SERCHG,
                 LNFILE.LN_EXTCHG              = :QLN-EXTCHG,
                 LNFILE.LN_MAINTFEE            = :QLN-MAINTFEE,
                 LNFILE.LN_POINTS              = :QLN-POINTS,
                 LNFILE.LN_ORGTERM             = :QLN-ORGTERM,
                 LNFILE.LN_1STPYAMT            = :QLN-1STPYAMT,
                 LNFILE.LN_NOREGPY             = :QLN-NOREGPY,
                 LNFILE.LN_REGPYAMT            = :QLN-REGPYAMT,
                 LNFILE.LN_LASTPYAMT           = :QLN-LASTPYAMT,
                 LNFILE.LN_FEECODE_1           = :QLN-FEECODE-1,
                 LNFILE.LN_FEECODE_2           = :QLN-FEECODE-2,
                 LNFILE.LN_FEECODE_3           = :QLN-FEECODE-3,
                 LNFILE.LN_FEECODE_4           = :QLN-FEECODE-4,
                 LNFILE.LN_FEECODE_5           = :QLN-FEECODE-5,
                 LNFILE.LN_FEECODE_6           = :QLN-FEECODE-6,
                 LNFILE.LN_FEECODE_7           = :QLN-FEECODE-7,
                 LNFILE.LN_FEECODE_8           = :QLN-FEECODE-8,
                 LNFILE.LN_FEECODE_9           = :QLN-FEECODE-9,
                 LNFILE.LN_FEECODE_10          = :QLN-FEECODE-10,
                 LNFILE.LN_FEECODE_11          = :QLN-FEECODE-11,
                 LNFILE.LN_FEECODE_12          = :QLN-FEECODE-12,
                 LNFILE.LN_FEECODE_13          = :QLN-FEECODE-13,
                 LNFILE.LN_FEECODE_14          = :QLN-FEECODE-14,
                 LNFILE.LN_FEECODE_15          = :QLN-FEECODE-15,
                 LNFILE.LN_FEECODE_16          = :QLN-FEECODE-16,
                 LNFILE.LN_FEECODE_17          = :QLN-FEECODE-17,
                 LNFILE.LN_FEECODE_18          = :QLN-FEECODE-18,
                 LNFILE.LN_FEECODE_19          = :QLN-FEECODE-19,
                 LNFILE.LN_FEECODE_20          = :QLN-FEECODE-20,
                 LNFILE.LN_FEEAMT_1            = :QLN-FEEAMT-1,
                 LNFILE.LN_FEEAMT_2            = :QLN-FEEAMT-2,
                 LNFILE.LN_FEEAMT_3            = :QLN-FEEAMT-3,
                 LNFILE.LN_FEEAMT_4            = :QLN-FEEAMT-4,
                 LNFILE.LN_FEEAMT_5            = :QLN-FEEAMT-5,
                 LNFILE.LN_FEEAMT_6            = :QLN-FEEAMT-6,
                 LNFILE.LN_FEEAMT_7            = :QLN-FEEAMT-7,
                 LNFILE.LN_FEEAMT_8            = :QLN-FEEAMT-8,
                 LNFILE.LN_FEEAMT_9            = :QLN-FEEAMT-9,
                 LNFILE.LN_FEEAMT_10           = :QLN-FEEAMT-10,
                 LNFILE.LN_FEEAMT_11           = :QLN-FEEAMT-11,
                 LNFILE.LN_FEEAMT_12           = :QLN-FEEAMT-12,
                 LNFILE.LN_FEEAMT_13           = :QLN-FEEAMT-13,
                 LNFILE.LN_FEEAMT_14           = :QLN-FEEAMT-14,
                 LNFILE.LN_FEEAMT_15           = :QLN-FEEAMT-15,
                 LNFILE.LN_FEEAMT_16           = :QLN-FEEAMT-16,
                 LNFILE.LN_FEEAMT_17           = :QLN-FEEAMT-17,
                 LNFILE.LN_FEEAMT_18           = :QLN-FEEAMT-18,
                 LNFILE.LN_FEEAMT_19           = :QLN-FEEAMT-19,
                 LNFILE.LN_FEEAMT_20           = :QLN-FEEAMT-20,
                 LNFILE.LN_FEEOURS_1           = :QLN-FEEOURS-1,
                 LNFILE.LN_FEEOURS_2           = :QLN-FEEOURS-2,
                 LNFILE.LN_FEEOURS_3           = :QLN-FEEOURS-3,
                 LNFILE.LN_FEEOURS_4           = :QLN-FEEOURS-4,
                 LNFILE.LN_FEEOURS_5           = :QLN-FEEOURS-5,
                 LNFILE.LN_FEEOURS_6           = :QLN-FEEOURS-6,
                 LNFILE.LN_FEEOURS_7           = :QLN-FEEOURS-7,
                 LNFILE.LN_FEEOURS_8           = :QLN-FEEOURS-8,
                 LNFILE.LN_FEEOURS_9           = :QLN-FEEOURS-9,
                 LNFILE.LN_FEEOURS_10          = :QLN-FEEOURS-10,
                 LNFILE.LN_FEEOURS_11          = :QLN-FEEOURS-11,
                 LNFILE.LN_FEEOURS_12          = :QLN-FEEOURS-12,
                 LNFILE.LN_FEEOURS_13          = :QLN-FEEOURS-13,
                 LNFILE.LN_FEEOURS_14          = :QLN-FEEOURS-14,
                 LNFILE.LN_FEEOURS_15          = :QLN-FEEOURS-15,
                 LNFILE.LN_FEEOURS_16          = :QLN-FEEOURS-16,
                 LNFILE.LN_FEEOURS_17          = :QLN-FEEOURS-17,
                 LNFILE.LN_FEEOURS_18          = :QLN-FEEOURS-18,
                 LNFILE.LN_FEEOURS_19          = :QLN-FEEOURS-19,
                 LNFILE.LN_FEEOURS_20          = :QLN-FEEOURS-20,
                 LNFILE.LN_FEEFINCHG_1         = :QLN-FEEFINCHG-1,
                 LNFILE.LN_FEEFINCHG_2         = :QLN-FEEFINCHG-2,
                 LNFILE.LN_FEEFINCHG_3         = :QLN-FEEFINCHG-3,
                 LNFILE.LN_FEEFINCHG_4         = :QLN-FEEFINCHG-4,
                 LNFILE.LN_FEEFINCHG_5         = :QLN-FEEFINCHG-5,
                 LNFILE.LN_FEEFINCHG_6         = :QLN-FEEFINCHG-6,
                 LNFILE.LN_FEEFINCHG_7         = :QLN-FEEFINCHG-7,
                 LNFILE.LN_FEEFINCHG_8         = :QLN-FEEFINCHG-8,
                 LNFILE.LN_FEEFINCHG_9         = :QLN-FEEFINCHG-9,
                 LNFILE.LN_FEEFINCHG_10        = :QLN-FEEFINCHG-10,
                 LNFILE.LN_FEEFINCHG_11        = :QLN-FEEFINCHG-11,
                 LNFILE.LN_FEEFINCHG_12        = :QLN-FEEFINCHG-12,
                 LNFILE.LN_FEEFINCHG_13        = :QLN-FEEFINCHG-13,
                 LNFILE.LN_FEEFINCHG_14        = :QLN-FEEFINCHG-14,
                 LNFILE.LN_FEEFINCHG_15        = :QLN-FEEFINCHG-15,
                 LNFILE.LN_FEEFINCHG_16        = :QLN-FEEFINCHG-16,
                 LNFILE.LN_FEEFINCHG_17        = :QLN-FEEFINCHG-17,
                 LNFILE.LN_FEEFINCHG_18        = :QLN-FEEFINCHG-18,
                 LNFILE.LN_FEEFINCHG_19        = :QLN-FEEFINCHG-19,
                 LNFILE.LN_FEEFINCHG_20        = :QLN-FEEFINCHG-20,
                 LNFILE.LN_INSURED             = :QLN-INSURED,
                 LNFILE.LN_INSTYPES            = :QLN-INSTYPES,
                 LNFILE.LN_INSCOMP_1           = :QLN-INSCOMP-1,
                 LNFILE.LN_INSCOMP_2           = :QLN-INSCOMP-2,
                 LNFILE.LN_INSCOMP_3           = :QLN-INSCOMP-3,
                 LNFILE.LN_INSCOMP_4           = :QLN-INSCOMP-4,
                 LNFILE.LN_INSCOMP_5           = :QLN-INSCOMP-5,
                 LNFILE.LN_INSCOMP_6           = :QLN-INSCOMP-6,
                 LNFILE.LN_INSCOMP_7           = :QLN-INSCOMP-7,
                 LNFILE.LN_INSCOMP_8           = :QLN-INSCOMP-8,
                 LNFILE.LN_INSCOMP_9           = :QLN-INSCOMP-9,
                 LNFILE.LN_INSCOMP_10          = :QLN-INSCOMP-10,
                 LNFILE.LN_INSOURS_1           = :QLN-INSOURS-1,
                 LNFILE.LN_INSOURS_2           = :QLN-INSOURS-2,
                 LNFILE.LN_INSOURS_3           = :QLN-INSOURS-3,
                 LNFILE.LN_INSOURS_4           = :QLN-INSOURS-4,
                 LNFILE.LN_INSOURS_5           = :QLN-INSOURS-5,
                 LNFILE.LN_INSOURS_6           = :QLN-INSOURS-6,
                 LNFILE.LN_INSOURS_7           = :QLN-INSOURS-7,
                 LNFILE.LN_INSOURS_8           = :QLN-INSOURS-8,
                 LNFILE.LN_INSOURS_9           = :QLN-INSOURS-9 ,
                 LNFILE.LN_INSOURS_10          = :QLN-INSOURS-10,
                 LNFILE.LN_INSEFF_DATE_1       = :QLN-INSEFF-DATE-1,
                 LNFILE.LN_INSEFF_DATE_2       = :QLN-INSEFF-DATE-2,
                 LNFILE.LN_INSEFF_DATE_3       = :QLN-INSEFF-DATE-3,
                 LNFILE.LN_INSEFF_DATE_4       = :QLN-INSEFF-DATE-4,
                 LNFILE.LN_INSEFF_DATE_5       = :QLN-INSEFF-DATE-5,
                 LNFILE.LN_INSEFF_DATE_6       = :QLN-INSEFF-DATE-6,
                 LNFILE.LN_INSEFF_DATE_7       = :QLN-INSEFF-DATE-7,
                 LNFILE.LN_INSEFF_DATE_8       = :QLN-INSEFF-DATE-8,
                 LNFILE.LN_INSEFF_DATE_9       = :QLN-INSEFF-DATE-9,
                 LNFILE.LN_INSEFF_DATE_10      = :QLN-INSEFF-DATE-10,
                 LNFILE.LN_INSEXP_DATE_1       = :QLN-INSEXP-DATE-1,
                 LNFILE.LN_INSEXP_DATE_2       = :QLN-INSEXP-DATE-2,
                 LNFILE.LN_INSEXP_DATE_3       = :QLN-INSEXP-DATE-3,
                 LNFILE.LN_INSEXP_DATE_4       = :QLN-INSEXP-DATE-4,
                 LNFILE.LN_INSEXP_DATE_5       = :QLN-INSEXP-DATE-5,
                 LNFILE.LN_INSEXP_DATE_6       = :QLN-INSEXP-DATE-6,
                 LNFILE.LN_INSEXP_DATE_7       = :QLN-INSEXP-DATE-7,
                 LNFILE.LN_INSEXP_DATE_8       = :QLN-INSEXP-DATE-8,
                 LNFILE.LN_INSEXP_DATE_9       = :QLN-INSEXP-DATE-9,
                 LNFILE.LN_INSEXP_DATE_10      = :QLN-INSEXP-DATE-10,
                 LNFILE.LN_INSPREM_1           = :QLN-INSPREM-1,
                 LNFILE.LN_INSPREM_2           = :QLN-INSPREM-2,
                 LNFILE.LN_INSPREM_3           = :QLN-INSPREM-3,
                 LNFILE.LN_INSPREM_4           = :QLN-INSPREM-4,
                 LNFILE.LN_INSPREM_5           = :QLN-INSPREM-5,
                 LNFILE.LN_INSPREM_6           = :QLN-INSPREM-6,
                 LNFILE.LN_INSPREM_7           = :QLN-INSPREM-7,
                 LNFILE.LN_INSPREM_8           = :QLN-INSPREM-8,
                 LNFILE.LN_INSPREM_9           = :QLN-INSPREM-9,
                 LNFILE.LN_INSPREM_10          = :QLN-INSPREM-10,
                 LNFILE.LN_INSCOVR_1           = :QLN-INSCOVR-1,
                 LNFILE.LN_INSCOVR_2           = :QLN-INSCOVR-2,
                 LNFILE.LN_INSCOVR_3           = :QLN-INSCOVR-3,
                 LNFILE.LN_INSCOVR_4           = :QLN-INSCOVR-4,
                 LNFILE.LN_INSCOVR_5           = :QLN-INSCOVR-5,
                 LNFILE.LN_INSCOVR_6           = :QLN-INSCOVR-6,
                 LNFILE.LN_INSCOVR_7           = :QLN-INSCOVR-7,
                 LNFILE.LN_INSCOVR_8           = :QLN-INSCOVR-8,
                 LNFILE.LN_INSCOVR_9           = :QLN-INSCOVR-9,
                 LNFILE.LN_INSCOVR_10          = :QLN-INSCOVR-10,
                 LNFILE.LN_INSFINCHG_1         = :QLN-INSFINCHG-1,
                 LNFILE.LN_INSFINCHG_2         = :QLN-INSFINCHG-2,
                 LNFILE.LN_INSFINCHG_3         = :QLN-INSFINCHG-3, 
                 LNFILE.LN_INSFINCHG_4         = :QLN-INSFINCHG-4, 
                 LNFILE.LN_INSFINCHG_5         = :QLN-INSFINCHG-5,
                 LNFILE.LN_INSFINCHG_6         = :QLN-INSFINCHG-6,
                 LNFILE.LN_INSFINCHG_7         = :QLN-INSFINCHG-7,
                 LNFILE.LN_INSFINCHG_8         = :QLN-INSFINCHG-8,
                 LNFILE.LN_INSFINCHG_9         = :QLN-INSFINCHG-9,
                 LNFILE.LN_INSFINCHG_10        = :QLN-INSFINCHG-10,
                 LNFILE.LN_PY_YYYYMM_RECDEL    = :QLN-PY-YYYYMM-RECDEL,
                 LNFILE.LN_MTD_PAYS_RECDEL     = :QLN-MTD-PAYS-RECDEL,
                 LNFILE.LN_DLNO                = :QLN-DLNO,
                 LNFILE.LN_ORIGPURAMT          = :QLN-ORIGPURAMT,
                 LNFILE.LN_LCPDTH_DATE         = :QLN-LCPDTH-DATE,
                 LNFILE.LN_INTPDTH_DATE        = :QLN-INTPDTH-DATE,
                 LNFILE.LN_CURBAL              = :QLN-CURBAL,
                 LNFILE.LN_INTBAL              = :QLN-INTBAL, 
                 LNFILE.LN_LCBAL               = :QLN-LCBAL, 
                 LNFILE.LN_OTHBAL              = :QLN-OTHBAL,
                 LNFILE.LN_OT2BAL              = :QLN-OT2BAL,
                 LNFILE.LN_LCPAID              = :QLN-LCPAID,
                 LNFILE.LN_LCPARTIALS          = :QLN-LCPARTIALS,
                 LNFILE.LN_ANTICERN_1          = :QLN-ANTICERN-1,
                 LNFILE.LN_ANTICERN_2          = :QLN-ANTICERN-2,
                 LNFILE.LN_ANTICERN_3          = :QLN-ANTICERN-3,
                 LNFILE.LN_ANTICERN_4          = :QLN-ANTICERN-4,
                 LNFILE.LN_ANTICERN_5          = :QLN-ANTICERN-5,
                 LNFILE.LN_ANTICERN_6          = :QLN-ANTICERN-6,
                 LNFILE.LN_ANTICERN_7          = :QLN-ANTICERN-7,
                 LNFILE.LN_ANTICERN_8          = :QLN-ANTICERN-8,
                 LNFILE.LN_ANTICERN_9          = :QLN-ANTICERN-9,
                 LNFILE.LN_ANTICERN_10         = :QLN-ANTICERN-10,
                 LNFILE.LN_ANTICADJ_1          = :QLN-ANTICADJ-1,
                 LNFILE.LN_ANTICADJ_2          = :QLN-ANTICADJ-2,    
                 LNFILE.LN_ANTICADJ_3          = :QLN-ANTICADJ-3,    
                 LNFILE.LN_ANTICADJ_4          = :QLN-ANTICADJ-4,    
                 LNFILE.LN_ANTICADJ_5          = :QLN-ANTICADJ-5,    
                 LNFILE.LN_ANTICADJ_6          = :QLN-ANTICADJ-6,    
                 LNFILE.LN_ANTICADJ_7          = :QLN-ANTICADJ-7,    
                 LNFILE.LN_ANTICADJ_8          = :QLN-ANTICADJ-8,    
                 LNFILE.LN_ANTICADJ_9          = :QLN-ANTICADJ-9,    
                 LNFILE.LN_ANTICADJ_10         = :QLN-ANTICADJ-10,   
                 LNFILE.LN_UFERN_1             = :QLN-UFERN-1,       
                 LNFILE.LN_UFERN_2             = :QLN-UFERN-2,       
                 LNFILE.LN_UFERN_3             = :QLN-UFERN-3,       
                 LNFILE.LN_UFERN_4             = :QLN-UFERN-4,       
                 LNFILE.LN_UFERN_5             = :QLN-UFERN-5,       
                 LNFILE.LN_UFERN_6             = :QLN-UFERN-6,       
                 LNFILE.LN_UFERN_7             = :QLN-UFERN-7,       
                 LNFILE.LN_UFERN_8             = :QLN-UFERN-8,       
                 LNFILE.LN_UFERN_9             = :QLN-UFERN-9,       
                 LNFILE.LN_UFERN_10            = :QLN-UFERN-10,      
                 LNFILE.LN_TOTINT              = :QLN-TOTINT,        
                 LNFILE.LN_TOTLCHG             = :QLN-TOTLCHG,       
                 LNFILE.LN_TOTDEF              = :QLN-TOTDEF,        
                 LNFILE.LN_TOTPAYMNTD          = :QLN-TOTPAYMNTD,    
                 LNFILE.LN_TOTNODEF            = :QLN-TOTNODEF,      
                 LNFILE.LN_YTDNODEF            = :QLN-YTDNODEF,      
                 LNFILE.LN_TOTNODEFACT         = :QLN-TOTNODEFACT,   
                 LNFILE.LN_YTDNODEFACT         = :QLN-YTDNODEFACT,   
                 LNFILE.LN_TOTNOALDEL          = :QLN-TOTNOALDEL,    
                 LNFILE.LN_CL_REBATE           = :QLN-CL-REBATE,     
                 LNFILE.LN_AH_REBATE           = :QLN-AH-REBATE,     
                 LNFILE.LN_PP_REBATE           = :QLN-PP-REBATE,     
                 LNFILE.LN_INT_REBATE          = :QLN-INT-REBATE,    
                 LNFILE.LN_SC_REBATE           = :QLN-SC-REBATE,     
                 LNFILE.LN_MF_REBATE           = :QLN-MF-REBATE,     
                 LNFILE.LN_DF_REBATE           = :QLN-DF-REBATE,     
                 LNFILE.LN_EX_REBATE           = :QLN-EX-REBATE,     
                 LNFILE.LN_O1_REBATE           = :QLN-O1-REBATE,     
                 LNFILE.LN_O2_REBATE           = :QLN-O2-REBATE,     
                 LNFILE.LN_O3_REBATE           = :QLN-O3-REBATE,     
                 LNFILE.LN_O4_REBATE           = :QLN-O4-REBATE,     
                 LNFILE.LN_O5_REBATE           = :QLN-O5-REBATE,     
                 LNFILE.LN_O6_REBATE           = :QLN-O6-REBATE,     
                 LNFILE.LN_O7_REBATE           = :QLN-O7-REBATE,     
                 LNFILE.LN_UN_O7_REB           = :QLN-UN-O7-REB,     
                 LNFILE.LN_UN_O8_REB           = :QLN-UN-O8-REB,     
                 LNFILE.LN_UN_O9_REB           = :QLN-UN-O9-REB,     
                 LNFILE.LN_UN_O10_REB          = :QLN-UN-O10-REB,    
                 LNFILE.LN_COLLECTOR           = :QLN-COLLECTOR,     
                 LNFILE.LN_ACTIONCD            = :QLN-ACTIONCD,      
                 LNFILE.LN_ESCROW_FG           = :QLN-ESCROW-FG,     
                 LNFILE.LN_PROMDATE            = :QLN-PROMDATE,      
                 LNFILE.LN_PROMDATE2           = :QLN-PROMDATE2,     
                 LNFILE.LN_DATE_PAID_LAST      = :QLN-DATE-PAID-LAST,
                 LNFILE.LN_LASTCFORMNO         = :QLN-LASTCFORMNO,   
                 LNFILE.LN_LASTCFORMDATE       = :QLN-LASTCFORMDATE, 
                 LNFILE.LN_DELCNT              = :QLN-DELCNT,
                 LNFILE.LN_CREDITLIM           = :QLN-CREDITLIM,
                 LNFILE.LN_OLDACCTNO           = :QLN-OLDACCTNO,
                 LNFILE.LN_PRLNNO              = :QLN-PRLNNO,
                 LNFILE.LN_SUM_FINCHG          = :QLN-SUM-FINCHG,
                 LNFILE.LN_SUM_ORGTERM         = :QLN-SUM-ORGTERM,
                 LNFILE.LN_UNITPER_CD          = :QLN-UNITPER-CD,
                 LNFILE.LN_UNITPER_FREQ        = :QLN-UNITPER-FREQ,
                 LNFILE.LN_REPOCD              = :QLN-REPOCD,
                 LNFILE.LN_ORIGIN              = :QLN-ORIGIN,
                 LNFILE.LN_LBOX                = :QLN-LBOX,
                 LNFILE.LN_LTOUCH_DATE         = :QLN-LTOUCH-DATE,
                 LNFILE.LN_LTOUCH_TIME         = :QLN-LTOUCH-TIME

             WHERE LNFILE.LN_OWNBR  = :QLN-OWNBR
               AND LNFILE.LN_ACCTNO = :QLN-ACCTNO
           END-EXEC.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      * NOTE: THE WHERE CONSTRAINT IN THE ABOVE REWRITE NEEDS TO USE
      *       THE PRIMARY KEY WHERE CONSTRAINT; CANNOT DO A REWRITE ON
      *       THE SECONDARY KEY SINCE THE Q??#-KEY IS CHANGED AND WILL
      *       NOT FIND THE ORIGINAL KEY, AS WELL AS Q??#-KEY COULD
      *       MATCH ANOTHER RECORD/ROW THAT SHOULD NOT CHANGE!!!
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO REWRITE-LN3-FILE.

           MOVE '1' TO SQL-UPDATE-CONN-USE-SW.  *> SET CONNECTION BACK TO C1
           PERFORM SQL-SWAP-UPDATE-CONNECTION.

           IF ( IO-FG = 0 )
      *       EXEC SQL COMMIT END-EXEC
      *       PERFORM SQL-IO-VALIDATION
              IF LN-BI-FG = "Y"
                 MOVE "R" TO STAT
                 PERFORM LN3-UPDATE-BI-FILE.

      *-----------------------------------------------------------------
       REWRITE-LN3-FILE-LMTD.
      *** ONLY UPDATES LN_COLLECTOR AND LN_LTOUCH_DATE ***
           MOVE LN-PATH-OWNBR  TO LN-OWNBR.
           ACCEPT LN-LTOUCH-DATE FROM CENTURY-DATE.
           PERFORM REWRITE-IT.
           MOVE LN-PATH-SQL    TO E-FILE.
           MOVE LN1-KEY        TO E-KEYX.
      D    STOP MESS.
           PERFORM SET-LN-FIELDS.

           MOVE '2' TO SQL-UPDATE-CONN-USE-SW.    *> CHANGE CONNECTION TO UPDATE_CONNECT
           PERFORM SQL-SWAP-UPDATE-CONNECTION.

           EXEC SQL
            UPDATE DBO.LNFILE
             SET LNFILE.LN_COLLECTOR   = :QLN-COLLECTOR,
                 LNFILE.LN_LTOUCH_DATE = :QLN-LTOUCH-DATE
             WHERE LNFILE.LN_OWNBR  = :QLN-OWNBR
               AND LNFILE.LN_ACCTNO = :QLN-ACCTNO
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO REWRITE-LN3-FILE-LMTD.

           MOVE '1' TO SQL-UPDATE-CONN-USE-SW.    *> CHANGE CONNECTION BACK TO C1
           PERFORM SQL-SWAP-UPDATE-CONNECTION.

           IF ( IO-FG = 0 )
      *       EXEC SQL COMMIT END-EXEC
      *       PERFORM SQL-IO-VALIDATION
              IF LN-BI-FG = "Y"
                 MOVE "R" TO STAT
                 PERFORM LN3-UPDATE-BI-FILE.

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
      *-----------------------------------------------------------------


       COPY "LIBGB/GBWKIN.CPY".
       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO EXT-EOBUF-ERRCD.

      ********************************************
       END-PROGRAM SECTION.
      ********************************************

      *%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
           MOVE "GB/TIMER" TO FORM-NAM.
           CALL FORM-PROGX USING P-TIMER-LINK-BUF.
           CANCEL FORM-PROGX.
      *%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
           PERFORM CLOSE-FILES.

           MOVE MKTEMP-BUF TO ACCESS-BUF.
           PERFORM REMOVE-WORKFILE.

       EXIT-PROG.
           IF SV-BEG-BR NOT = 9999
              MOVE SV-BEG-BR TO BEG-BRANCH
              MOVE SV-END-BR TO END-BRANCH.

           IF EXT-ROUTE-BUF  = "00"
              MOVE "LP/LPCPMU" TO FORM-NAM
              MOVE FORM-PATH TO FORM-PATHNAME.
       STOP-RUN.
           IF EXT-ROUTE-BUF = "00" OR "40" OR "50"
              DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN
              DESTROY CPRUN-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.
           EXIT PROGRAM.
