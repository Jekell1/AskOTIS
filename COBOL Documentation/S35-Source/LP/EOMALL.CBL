       IDENTIFICATION DIVISION.
       PROGRAM-ID.      EOMALL.
       DATE-WRITTEN. 08-16-95.
      *****************************************************************
      *
      *  DESCRIPTION: END OF MONTH DRIVER FOR MULTIPLE DATA SETS
      *
      *  THIS UPDATE IS DRIVEN OFF THE RC-FILE, SO IT IS CONTROLLED
      *  BY & UPDATES RC-STATUS.  BEFORE THE UPDATE CAN BEGIN, THE
      *  PROGRAM CL/CKSTAT IS CALLED TO VERIFY THAT ALL BRANCHES IN
      *  THE SELECTED BRANCH RANGE OR GROUP ARE AT THE CORRECT STATUS.
      *
      *  IF CUSTOMER IS NOT CASH METHOD AND/OR DOES IB ACCRUALS,
      *  MOST OF THE STATUS CHECKING HAS BEEN HANDLED IN THE EOM
      *  ACCRUAL.  THE ACCRUAL PROGRAM MADE SURE ALL DAYS WERE
      *  CLOSED & UPDATED, AND EITHER WROTE OR RE-WROTE AN RC RECORD
      *  FOR THE MONTH-END DATE WITH A STATUS OF G1 FOR EARNINGS,
      *  SO THE ONLY CHECK DONE HERE IS THAT ALL BRANCHES HAVE A G1
      *  STATUS FOR THE MONTH-END DATE.
      *
      *  IF CUSTOMER IS CASH METHOD ONLY AND DOESN'T DO IB ACCRUALS
      *  (IF GP-CASH-METHOD-ONLY = "Y" AND GP-IB-ACCRUALS-DONE = "N"),
      *  THEN WE HAVEN'T CHECKED FOR OPEN DAYS YET.  THEREFORE, WE
      *  CALL CL/CKSTAT HERE TO VERIFY THERE ARE NO NON-UPDATED DAYS
      *  (RC-STATUS < D1).  ONCE THE STATUSES ARE CHECKED, THIS PROGRAM
      *  WRITES OR REWRITES AN RC RECORD FOR EACH BRANCH FOR THE
      *  MONTH-END DATE WITH A STATUS OF G1.  THEREFORE THIS UPDATE
      *  CAN PROCEED TO PROCESS CASH OR NON-CASH CUSTOMERS EXPECTING
      *  THE SAME STATUSES:  IN BOTH CASES THE RC-FILE IS STARTED
      *  WITH A STATUS OF G1 FOR THE MONTH-END DATE, & REWRITTEN
      *  WITH A STATUS OF I1 FOR MONTH-END UPDATED.
      *
      *  5/25/99 - THERE STILL IS A POSSIBLE PROBLEM IF A CUSTOMER
      *  WHO DOES NOT ACCRUE RUNS EOM WITH A DAY WHICH WAS NOT OPEN
      *  FOR LOAN PROCESSING. THIS SITUATION WOULD PASS THE STATUS
      *  CHECKS HERE (BECAUSE THERE IS NO RC FOR THE MONTH-END
      *  DATE), & THEN WOULDN'T DO ANYTHING BECAUSE THIS PROGRAM
      *  DRIVES OFF RC.  I ADDED A FIRST-TIME CHECK TO GIVE A
      *  MSG IF NO BRANCHES PROCESSED HERE; ALSO, IF THE GLOBAL EOM
      *  CHECK FOR LONPA0 IS TURNED ON, THEY WON'T BE ABLE TO OPEN
      *  THE FIRST DAY OF NEXT MONTH UNLESS THE FLAG IS TURNED ON. (BLV)
      *
      **********************************************************************
      *   @@@@   ERROR 27 WHEN CALLING SCRIPT   @@@@
      *  THERE IS A PROBLEM CALLING EC PROGRAM THAT RUNS THE SHELL .SH FILE 
      *  THE LAST ISSUE WAS:
      *    MISSING LINUX LOAD;
      *    NEED GLIBC.I686 PACKAGE INSTALLED
      *    SUDO YUM INSTALL GLIBC.I686
      *
      *  ALSO WATCH PERMISSIONS ON .SH FILES
      **********************************************************************
      *
      * REV:
      *  MJD 091195 CHANGED CHECK OF RC-STATUS TO INCLUDE "G1".
      *             CHANGED DATE RANGE OF CHECK TO SCAN EOM-DATE ONLY.
      *             ACRERN RUNS PRIOR AND CHECKS FOR OTHER STATUSES.
      *  MJD 091495 ADDED LOGIC FOR DS-DATE IN DS1-KEY.
      *  JTG 092295 ADDED LN-BMO-CON-STATUS LN-BMO-REC-STATUS
      *  BLL 111795 CLEAR BY-IB-ACCRUED IN NEW MONTH'S RECORD
      *  JTG 111795 CHANGED INDEX FOR BY-UNITS TO 10
      *  BAH 122995 CORRECTED SPELLING OF "AGING" TO "AGEING"
      *  MJD 012396 ADDED CLEAR OF O2 & INTEREST FIELDS OF PERIOD & REPO
      *             TABLES. ADDED PL TABLE CLEAR. ADDED CLEAR FOR DLR
      *             RESERVES AND LOANS SOLD/TRANSFERRED/PURCHASED.
      *             ADDED CLEAR OF PL PRIN/INT/LATE COLLECTED.
      *  BLL 012696 CHANGE BY CLR OF DLR-TABLE, ADD PL-O2COL
      *  MJD 071696 ADDED LIBLP/LPUPWK FOR CHANGES IN LIBLP/AGEING
      *  KEC 111496 CHANGED LN-POCD FOR RESCISSION.
      *  BLV 021997 DON'T EXPECT RC-STAT FOR EARNINGS IF CASH METHOD;
      *             RUN FROM UTMENU OR LPEMMU
      *  KEC 030797 CHANGED TO HANDLE NEW APPLICATIONS STATISTICS IN
      *              BY-FILE, DS-FILE AND TY-FILE.
      *  BLV 050597 ACTIVATE REMOVE-OP-AND-SPOOL SECTION; FIX DELETE
      *             DATE IN REMOVE-TY & REMOVE-CA *** NOTE: YOU MUST
      *             SEND UPDATED SHELL /USR/A60/SH/EOM2.SH !!!!!
      *  KEC 051297 CHANGED TO HANDLE NEW APPLICATIONS STATISTICS IN
      *              BY-FILE, DS-FILE AND TY-FILE. (?-NO-MADE).
      *  BLV 051397 ADD ROUTINE TO CREATE RCS FOR NON-ACCRUING
      *             CUSTOMERS...IF NO DAY OPENED FOR EOM DATE
      *  MJD 070297 ADDED CLEAR OF DLDISC MTD VOLUME.
      *  BLV 071097 FIX CREATE RCS FOR NON-ACCRUAL; WAS GIVING
      *             WRITE ERROR OF 22
      *  MJD 082297 CHANGED LN-REC/CONPROFILE TO LN-REC/CONPROFILE(1..5)
      *  GLF 090897 REPLACED 'IO' ON GP-FILE WITH GET 'GPENV'
      *  BAH 091297 UPDATE LN-TRW-DOFD-DATE TO EOM DATE IF ACCOUNT IS A 30,
      *             OR 0 IT OUT IF CURRENT.
      *  MJD 091797 REMOVED DELSUB-SET LOGIC; ADDED LPPDUE(W) COPY MEMBERS
      *  MJD 100997 CHANGED DELINQ TABLE TO OCCURS 10. (WAS 18)
      *  MJD 102297 CHANGED TO REGENERATE DEALER STATS OUTSTANDING.
      *  BLV 121297 MOVED CLEAR ON LN-MTD-PAYS-RECDEL; WAS
      *             MESSING UP DELINQUENCY IN BYFILE
      *  BLV 030498 FIXED TO MOVE IN GR-ID BEFORE SEARCH-GR-INIT:
      *             NON-ACCRUAL ROUTINE WAS MESSING UP ID & EOM
      *             COULD SKIP BRANCHES!!!
      *  BLV 031598 FIXED TO CLEAR SPECIAL FIELDS FOR WORLD FOR NEW MONTH:
      *             BY-30-DAY-MOVED, BY-PROJ-PROFIT, BY-PROJ-INCREASE
      *  MJD 032098 ADDED GETENV(W) FOR USE IN LIBLP/POSTDATE
      *  MJD 051998 REMOVED XY-PASTDUE. CHANGED XY-EXREB TO BE XY-PART-CHGOFF
      *  BLV 052198 FIXED TO RUN IN BATCH
      *  JTG 061598 ADDED LOGIC TO UPDATE BANKRUPT DELINQUENT STATISTICS
      *  BAH 062598 CHANGED CA-TILLNO TO PIC X, ADDED CA-TILLNO-ZERO SO OLD
      *             RECORDS WOULD STILL MATCH.
      *  JTG 062998 CHANGED DS-CNVT.... TO DS-RNEW....
      *                     DS-SVOS.... TO DS-TODL....
      *  BAH 073098 IF GP-TR-RETAIN-MONTHS = 99, DONT EVEN TRY TO DELETE TY'S
      *             OR CA'S, AND IF GP-TR-RETAIN-MONTHS 99 AND GP-EOD-RETAIN-
      *             MONTHS 99, DONT RUN EOM2.SH SHELL TO CLEAN UP DIRECTORIES.
      *  JTG 080798 CHANGED SET OF LN-BMO-CON-STATUS & LN-BMO-REC-STATUS
      *             TO MATCH A60 FOR MERCURY STEVE HAILEY
      *  BAH 081098 ADDED CLEAR OF LN-YTDNOALDEL FOR MERCURY'S NEW EDIT
      *             ON ALLOWABLE DELINQUENCY, #193
      *  JTG 990226 TEMPORARY SKIP OF UPDATE OF BY & DS DELINQUENCY
      *             FOR REVOLVING ACCOUNTS
      *  BLV 990318 IN CREATING RC'S FOR NON-ACCRUAL CUSTOMERS, DON'T BOMB OUT
      *             IF RC'S EXIST FOR MONTH-END DATE WITH STATUS OF G1 OR I1.
      *             IN THE CASE OF G1, THIS PROGRAM BOMBED (EG BAD GROUP) WHEN
      *             IN THE MIDDLE OF THE CREATE-RCS-FOR-NO-ACCRUAL - THIS IS
      *             OKAY, FIX GROUP & RE-RUN.  IF I1'S EXIST, SOMETHING
      *             HAPPENED IN THE MIDDLE OF EOM, & SOMEONE WILL HAVE TO
      *             ANALYZE THE PROBLEM BY LOOKING AT THE LOGS, ETC. HOWEVER,
      *             I DON'T SEE THE POINT IN STOPPING THE EOM PROCESS FROM
      *             BEING RERUN.....
      *  MJD 990416 CHANGED TO INCL/EXCL O2 FROM DELINQ BASED ON
      *             GP-DELINQ-WITH-O2
      *  JTG 990511 ADDED SP-RECFRMLA-PERCNT-PMT    'P'            MERCURY #183
      *  BLV 990525 ADD FIRST-TIME FLAG - DISPLAY IF NO BRANCHES UPDATED
      *  JTG 990716 ADDED LOGIC TO UPDATE DS & TY FOR REVOLVING
      *  JTG 990720 CHANGED TO TEST FOR LN-CURBAL,INTBAL,LCBAL,OT2BAL & OTHBAL
      *             EQUAL TO ZERO SINCE REVOLVING ACCOUNTS CAN HAVE LN-CURBAL
      *             GO TO ZERO AND THE LAST STATEMENT RUN COULD GENERATE
      *             AMOUNTS DUE IN LN-INTBAL / LN-LCBAL / LN-OT2BAL / LN-OTHBAL
      *  MJD 991220 ADDED LN-TOTNODEFACT AND LN-YTDNODEFACT (WORLD PR#216)
      *  MJD 000322 REMOVED DS-CNEARN/EARNTAKN/EARNDUE/CNUNEARN/ERNOTTAK
      *             ADDED DS-UNEARNED-AMT (MERC PR#223)
      *  MJD 000329 ADDED BY-UNEARNED-AMT (MERC PR#223)
      *  MJD 000331 ADDED XX-PROCEEDS (MERC PR#223)
      *  JTG 000426 CHANGED LOGIC FOR UPDATE OF (BY-DS)-UNEARNED-AMT MERC PR#223
      *             TO CORRECTLY HANDLE UPDATE OF UNEARNED AFTER ACCRUAL
      *  BLV 000726 CHANGED TO CLEAR & REGENERATE CURRENT OUTSTANDING FOR REPO
      *             & PL ACCOUNTS - WE THINK EOM SHOULD DO THIS(JOHN CINDY BARB)
      *  BAH 000815 LN-TRW-DOFD-DATE WAS GETTING SET AT 60 DAY INSTEAD OF 30 DAY
      *             BECAUSE OF THE DIFFERENCE IN CSUB FROM OLD RELEASES !!
      *             LAURA FOUND AT NEWPORT, CA
      *  BAH 000927 LN-TRW-DOFD-DATE WAS GETTING SET EVERYTIME AN ACCOUNT WAS 30
      *             INSTEAD OF JUST THE FIRST TIME AFTER BEING CURRENT !!
      *             EXAMPLE: IF PROFILE AT 8/31/00 WAS  111000000000 THEN THE
      *             DATE SHOULD BE 6/30/00 AND NOT 8/31/00 LIKE IT WAS DOING..
      *  MJD 000929 ADDED BY-LOCKBOX (MFC PR#237)
      *  BAH 010327 SET LN-TRW-DOFD-DATE CORRECTLY FOR REVERSED PAYMENTS. DECREASE
      *             MONTHLY DEPENDING ON CSUB
      *  BAH 030821 DO NOT RESET LN-TRW-DOFD-DATE TO 0 ON A CURRENT ACCOUNT THAT
      *             JUST GOT PAIDOUT !! HE COULD GO FROM A 4 TO CURRENT JUST
      *             BECAUSE HE PAID OUT, BUT WE NEED TO STILL KNOW WHEN HE
      *             BECAME THE 30
      *  BAH 050601 DO NOT RESET LN-TRW-DOFD-DATE TO 0 IF REPO OR P&L, KEVIN PR#2540
      *  BAH 070803 RESET LN-TRW-DOFD-DATE CURRENT IF REPO AND REDEEMED, PL# 595
      *  BAH 091111 SKIP PROFILE-UPDATE FOR GP-I-AM-UNITED CLASS 90 AND 91
      *             SERV ACCTS CONVERTED ON AND TP NOT DONE YET, UNITED PR# 3493
      *   CS 110527 ADD TEST ON SP-BANKRULE = 'F', WHICH IS WORLDS' CONTRACTUAL
      *             FORMULA. 'F' WORKS ON SEMI-MONTHLY ACCOUNTS ONLY
      *             SET LN-PYMMYY = 2*REGPYAMT HAVE BEEN PAID PR#752
      *  BAH 130702 ADDED SEPARATE SECTION FOR MEXICO LN-TRW-DOFD-DATE, PR# 901
      *  BAH 130904 REMOVE STOP MESS BURIED IN AN 'IF', WORLD PR# 925
      *             VOIDS WITH NO POFFFDATE CONTINUED TO MAKE IT THRU!
      *  BLM 140206 FIX TO RUN IN BATCH PROCESSING, REGACC PR# 349
      *  BLM 140610 MISSING A PERFORM ON LOG-ERROR FOR "END OF MONTH
      *             UPDATE COMPLETE", REGACC PR# 349A
      *
      * KEC 2015.1120 {PD#00003} PARADATA <A30> [JAY CISZEWSKI]
      *         FIXED RC-TRANS-DATE FROM YYYMMDD TO CCYYMMDD.
      *
      *  BAH 160125 SPLIT OUT LTFILE, PD0003
      *  BLM 160314 FIX FOR NEW BY-DATE FORMAT, PD0003
      *  BLM 160314 FIX FOR NEW DS-DATE FORMAT, FIX DS-UNEARNED, PD0003
      *
      * KEC 2016.0929 {PD#00003} PARADATA <A30> [JAY CISZEWSKI]
      *         CHANGED BY-DATE TO BY-DATE-CCYYMM FOR CONSISTENCY.
      *  CS 161107 ADDED SET OF "Y" TO NOACCEPT-BUF, WAS HITTING THE ACCEPT
      *            IN EOINIT AND THEN IN EOMALL.
      * KEC 2017.0105 {PR#00000} PARADATA <A30> [JAY CISZEWSKI]
      *          CHANGED DS-DATE TO DS-DATE-CCYYMM FOR CONSISTENCY
      *          WITH THE DATE FIELDS IN BY & UD FILE.
      * BLM 170113 FIX OLD BUG, IF NO BY OR DS RECS FOR CURRENT MO,
      *            NEXT MO BY/DS DATE NOT SET & RECORDS FOR NEW MONTH
      *            APPEAR TO NOT BE WRITTEN (ACTUALLY WRITTEN TO 
      *            RECORDS WITH 0 IN DATE.  FIXED TO SETUP BY & DS
      *            DATES IN INITIALIZATION.  FIXED A COUPLE OF MESSAGES
      *            IN LOG TO HAVE BR# INSTEAD OF &&&&
      *
      * KEC 2017.0321 {PR#00000} PARADATA <A30> [JAY CISZEWSKI]
      *          ADDED 'RC' FILE PREFIX FOR WORKING STORAGE FIELDS.
      *
      *  BAH 170531 ACTIVATE 8 DIGIT DATES, PD0006
      *
      * KEC 2017.0925 {PR#00000} PARADATA <A30> [JAY CISZEWSKI]
      *          ADDED RM-CMD TO LIBGB/SYSTEMW.CPY
      * BLM 2018.0228 FIX TO CLEAR MTD BY-FEE-TYPE TABLE WHEN CREATING
      *               NEW MONTHS BY. FIX FOR NEW OCCURENCES OF BY-UNEARNED-AMT
      *               & DS-UNEARNED-AMT, NOW OCCUR 10 INSTEAD OF 6
      * BAH 181003 GLOBAL BWFILE
      * BAH 181008 GLOBAL CAFILE
      * BAH 181015 GLOBAL DSFILE
      * BAH 181018 GLOBAL TYFILE
      * BAH 181207 GLOBAL LNFILE
      *  CS 190111 GLOBAL LTPFILE
      * KEC 190212 A32 F2SL - REVISED FOR SPLITTING THE BYFILE.
      *            REMOVED LOAN APP FIELDS IN BY & TY.
      *  CS 190423 REMOVE DEALER APP STATS FIELDS
      *  CS 190618 REPLACED CREATE-EOM-TAG WITH UPDATE PERFORM 
      *            UPDATE-EOM-CCYYMM-DATE WHICH UPDATES GB-EOM-TAG-CCYYMM
      *            SINCE IT DOESNT SEEM THAT THIS PROGRAMS USES THAT TAG
      *            AFTER CREATING IT(OTHER PROGRAMS DO), I MOVED IT DOWN
      *            FURTHER IN THE UPDATE PROCESS PRIOR TO THE UPDATE OF RCFILE
      * CS 190625 REMOVE TAGPATHWK COPY
      * BAH 2019-0625 REMOVED LN-ZERO-BAL-CNT AND LN-REVOLVING PT0153
      *  CS 2019-0712 ADD GB-EOM-UPDATE-DATE-TIME, GB-EOM-UPDATE-USERID
      * BAH 2019-0922 REMOVED ACCESS-CALL LNFILE, GB,DS,BW,LTP,DS,TY,CA
      * BAH 2020-0401 ADDED ALL REMOVE-TR SINCE NOW A GLOBAL FILE
      * BAH 20200827 REMOVED RESET ON BR1 
      * BAH 20210107 CAN NOT MOVE 0 TO A DATE IN THE KEY FOR A START, WILL
      *              NOT WORK IN A DATABASE. CHANGE ALL THE 0 TO 19000101
      * BLM 20210810 REMOVE TP-CODE, TP-POOLID NO LONGER IN KEY
      * MB  20240301 PERFORMANCE TUNING.  EMBEDDED SQL QUERIES, ADDED STORED
      *                PROCEDURE TO RETRIEVE LN DATA.
      *               ADDED STATE PARAMETER ARRAY PROCESSING
      * MB  20240611 CHANGED CURSORS USED IN UPDATE PROCESSING TO USE TEMP
      *              TABLE IN CURSOR (ONLY FOR LNFILE AND BYFILE) S35Q-67
      * MB  20240801 CHANGES FOR S35Q-104
      *               CHANGED: -LN CURSOR ORDER BY FROM LN_ACCTNO TO LN_CLASS
      *                        -BYFILE UPDATE (DURING LNFILE LOOP) TO UPDATE
      *                           ON LN-CLASS BREAK INSTEAD OF EVERY LN REC. 
      *                        -DSFILE UPDATE (DURING LNFILE LOOP) TO UPDATE
      *                           ON LN-DLNO CHANGES INSTEAD OF EVERY LN REC
      *                           WARNING - LN CURSOR ORDER BY LN-CLASS
      *                           CURRENTLY ONLY 1 DSFILE REC PER BRANCH.
      * JKC 20240802 CHANGES FOR S35Q-104:
      *         0805 ADDED LOGIC IN END-PROGRAM SECTION TO CALL
      *              TO GB/TIMER; TO STOP THE TIMER AND DISPLAY THE
      *              WINDOW IF NOT BATCH.
      *              ADDED LOGIC TO CREATE LINE TO THE LOG FILE FOR
      *              WHEN THE BRANCH IS COMPLETED.
      *              COMMENTED OUT ALL REFERENCES TO USING & UPDATING
      *              THE DSFILE; DETERMINED THAT IT IS NOT NEEDED.
      *         0806 CHANGED REFERENCE OF USING READ-LN1 TO BE
      *              READ-LN3 DUE TO MARK CHANGING THE SSP TO INCLUDE
      *              LOAN CLASS TO HELP PERFORMANCE.
      * MB  20240807 CHANGES FOR S35Q-104
      *              LIMITED LNFILE SSP TO RETURN ONLY COLS LISTED IN XCOL
      *              CHANGED LNFILE UPDATE TO ONLY UPDATE COLS LISTED IN XCOL
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "LIBGB/GBFSPR.CPY".
       COPY "LIBLP/LPFSVBY.CPY".
       COPY "LIBGB/GBFSVRC.CPY".
       COPY "LIBGB/GBFSVGB.CPY".

           SELECT BYADATA-OUT-FILE ASSIGN BYADATA-OUT-PATH
                             ORGANIZATION LINE SEQUENTIAL
                             ACCESS SEQUENTIAL
                             FILE STATUS FILE-STAT.

           SELECT BYBDATA-OUT-FILE ASSIGN BYBDATA-OUT-PATH
                             ORGANIZATION LINE SEQUENTIAL
                             ACCESS SEQUENTIAL
                             FILE STATUS FILE-STAT.

           SELECT RCDATA-OUT-FILE ASSIGN RCDATA-OUT-PATH
                             ORGANIZATION LINE SEQUENTIAL
                             ACCESS SEQUENTIAL
                             FILE STATUS FILE-STAT.

           SELECT LNDATA-OUT-FILE ASSIGN LNDATA-OUT-PATH
                             ORGANIZATION LINE SEQUENTIAL
                             ACCESS SEQUENTIAL
                             FILE STATUS FILE-STAT.

           SELECT GBDATA-OUT-FILE ASSIGN GBDATA-OUT-PATH
                             ORGANIZATION LINE SEQUENTIAL
                             ACCESS SEQUENTIAL
                             FILE STATUS FILE-STAT.

           SELECT STAT-OUT-FILE ASSIGN STAT-OUT-PATH
                             ORGANIZATION LINE SEQUENTIAL
                             ACCESS SEQUENTIAL
                             FILE STATUS FILE-STAT.

       I-O-CONTROL.
       DATA DIVISION.
       FILE SECTION.
       COPY "LIBGB/GBFDPR.CPY".
       COPY "LIBLP/LPFDVBY.CPY".
       COPY "LIBGB/GBFDVRC.CPY".
       COPY "LIBGB/GBFDVGB.CPY".

       FD  BYADATA-OUT-FILE
           RECORD IS VARYING IN SIZE FROM 5 TO 9000 CHARACTERS
           LABEL RECORDS ARE STANDARD.
       01  BYADATA-OUT-REC.
           03  BYADATA-REC-OUT          PIC X OCCURS 9000 TIMES.

       FD  BYBDATA-OUT-FILE
           RECORD IS VARYING IN SIZE FROM 5 TO 9000 CHARACTERS
           LABEL RECORDS ARE STANDARD.
       01  BYBDATA-OUT-REC.
           03  BYBDATA-REC-OUT          PIC X OCCURS 9000 TIMES.

       FD  RCDATA-OUT-FILE
           LABEL RECORDS ARE STANDARD.
       01  RCDATA-OUT-REC.
           03  RCDATA-REC-OUT          PIC X(16).

       FD  LNDATA-OUT-FILE
           RECORD IS VARYING IN SIZE FROM 5 TO 2000 CHARACTERS
           LABEL RECORDS ARE STANDARD.
       01  LNDATA-OUT-REC.
           03  LNDATA-REC-OUT          PIC X OCCURS 2000 TIMES.

       FD  GBDATA-OUT-FILE
           LABEL RECORDS ARE STANDARD.
       01  GBDATA-OUT-REC.
           03  GBDATA-REC-OUT          PIC X(24).

       FD  STAT-OUT-FILE
           LABEL RECORDS ARE STANDARD.
       01  STAT-OUT-REC.
           03  STAT-REC-OUT          PIC X(100).

      *****************************************************************
      *
      *    W O R K I N G - S T O R A G E    S E C T I O N
      *
      *****************************************************************
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(37) VALUE
           "@(#)LP/EOMALL S35 02/12/25-08:07:30 >".

      *****************************************************************
      *     F I L E   I F N
      *****************************************************************
       COPY "LIBGB/GB01RC.CPY".
       COPY "LIBGB/GB01RC_SQL.CPY".
       COPY "LIBGB/GBWSRC.CPY".
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBGB/GB01GB.CPY".
       COPY "LIBGB/GB01GB_SQL.CPY".
       COPY "LIBGB/GBWSGB.CPY".
       COPY "LIBGB/GB01GR.CPY".
       COPY "LIBGB/GB01GR_SQL.CPY".
       COPY "LIBGB/GBWSGR.CPY".
       COPY "LIBLP/LP01BY.CPY".
       COPY "LIBLP/LP01BYA.CPY".
       COPY "LIBLP/LP01BYB.CPY".
       COPY "LIBLP/LP01BY_SQL.CPY".
       COPY "LIBLP/LPWSBY.CPY".
       COPY "LIBLP/LPWSVBY.CPY".      *> VISION FILE
       COPY "LIBGB/GBWSVRC.CPY".      *> VISION FILE
       COPY "LIBGB/GBWSVGB.CPY".      *> VISION FILE
       COPY "LIBLP/LP01SP.CPY".
       COPY "LIBLP/LP01SPA.CPY".
       COPY "LIBLP/LP01SPB.CPY".
       COPY "LIBLP/LP01SPC.CPY".
       COPY "LIBLP/LP01SP_SQL.CPY".
       COPY "LIBLP/LPWSSP.CPY".
       COPY "LIBLP/LP01DS.CPY".
       COPY "LIBLP/LP01DS_SQL.CPY".
       COPY "LIBLP/LPWSDS.CPY".
       COPY "LIBLP/LP01LN.CPY".
       COPY "LIBLP/LP01LN_SQL.CPY".
       COPY "LIBLP/LPWSLN.CPY".
       COPY "LIBLP/LP01LTP.CPY".
       COPY "LIBLP/LP01LTP_SQL.CPY".
       COPY "LIBLP/LPWSLTP.CPY".
       COPY "LIBLP/LP01CA.CPY".
       COPY "LIBLP/LP01CA_SQL.CPY".
       COPY "LIBLP/LPWSCA.CPY".
       COPY "LIBLP/LP01TY.CPY".
       COPY "LIBLP/LP01TY_SQL.CPY".
       COPY "LIBLP/LPWSTY.CPY".
       COPY "LIBLP/LP01TRB.CPY".
       COPY "LIBLP/LP01TRB_SQL.CPY".
       COPY "LIBLP/LPWSTRB.CPY".
       COPY "LIBLP/LP01TRC.CPY".
       COPY "LIBLP/LP01TRC_SQL.CPY".
       COPY "LIBLP/LPWSTRC.CPY".
       COPY "LIBLP/LP01TRD.CPY".
       COPY "LIBLP/LP01TRD_SQL.CPY".
       COPY "LIBLP/LPWSTRD.CPY".
       COPY "LIBLP/LP01TRI.CPY".
       COPY "LIBLP/LP01TRI_SQL.CPY".
       COPY "LIBLP/LPWSTRI.CPY".
       COPY "LIBLP/LP01TRJ.CPY".
       COPY "LIBLP/LP01TRJ_SQL.CPY".
       COPY "LIBLP/LPWSTRJ.CPY".
       COPY "LIBLP/LP01TRK.CPY".
       COPY "LIBLP/LP01TRK_SQL.CPY".
       COPY "LIBLP/LPWSTRK.CPY".
       COPY "LIBLP/LP01TRL.CPY".
       COPY "LIBLP/LP01TRL_SQL.CPY".
       COPY "LIBLP/LPWSTRL.CPY".
       COPY "LIBLP/LP01TRP.CPY".
       COPY "LIBLP/LP01TRP_SQL.CPY".
       COPY "LIBLP/LPWSTRP.CPY".
       COPY "LIBLP/LP01TRS.CPY".
       COPY "LIBLP/LP01TRS_SQL.CPY".
       COPY "LIBLP/LPWSTRS.CPY".
       COPY "LIBLP/LP01TRW.CPY".
       COPY "LIBLP/LP01TRW_SQL.CPY".
       COPY "LIBLP/LPWSTRW.CPY".
       COPY "LIBLP/LP01TRX.CPY".
       COPY "LIBLP/LP01TRX_SQL.CPY".
       COPY "LIBLP/LPWSTRX.CPY".

      *----------------------------------------------------------------
       01  PR-IFN                      PIC XX      VALUE "PR".
       01  LN3-CONN-OPEN-SW            PIC X       VALUE "N".
       01  PROC-BRANCH                 PIC 9(4)    VALUE 0.
       01  PROC-BRANCH-RDF             REDEFINES PROC-BRANCH
                                       PIC X(4).
       01  PROC-BRANCH-ARRAY           OCCURS 9999 TIMES
                                       PIC X.
       01  STARTING-TIME               PIC X(8)    VALUE SPACES.
       01  SAVED-OUT-FILES-CTR         PIC 9(9)    VALUE 0.
       01  SAVED-FILES-ARRAY           OCCURS 200 TIMES
                                       PIC X(100).
       01  WS-PGM-FAILED-SW            PIC X       VALUE "Y".
       01  WS-BR-GR-TEXT               PIC X(4)    VALUE SPACE.

       01  EOMALL-OUTPUT-DIR.
          03  OUTPUT-PATH-BASE         PIC X(9).
          03  FILLER                   PIC X      VALUE "/".
          03  OUTPUT-PATH-MACHINE      PIC X(2).
          03  FILLER                   PIC X(3)  VALUE "/WK".

       01  BYADATA-OUT-PATH.
           03  BYADATA-PATH-DIR        PIC X(15).
           03  FILLER                  PIC X(12)  VALUE "/EOMALL-BYA-".
           03  BYADATA-PATH-PROC-ID    PIC X(6).
           03  FILLER                  PIC X      VALUE "-".
           03  BYADATA-PATH-BEGBRNO    PIC X(8)   VALUE SPACES.

       01  BYBDATA-OUT-PATH.
           03  BYBDATA-PATH-DIR        PIC X(15).
           03  FILLER                  PIC X(12)  VALUE "/EOMALL-BYB-".
           03  BYBDATA-PATH-PROC-ID    PIC X(6).
           03  FILLER                  PIC X      VALUE "-".
           03  BYBDATA-PATH-BEGBRNO    PIC X(8)   VALUE SPACES.

       01  RCDATA-OUT-PATH.
           03  RCDATA-PATH-DIR         PIC X(15).
           03  FILLER                  PIC X(11)  VALUE "/EOMALL-RC-".
           03  RCDATA-PATH-PROC-ID     PIC X(6).
           03  FILLER                  PIC X      VALUE "-".
           03  RCDATA-PATH-BEGBRNO     PIC X(8).

       01  LNDATA-OUT-PATH.
           03  LNDATA-PATH-DIR         PIC X(15).
           03  FILLER                  PIC X(11)  VALUE "/EOMALL-LN-".
           03  LNDATA-PATH-PROC-ID     PIC X(6).
           03  FILLER                  PIC X      VALUE "-".
           03  LNDATA-PATH-BEGBRNO     PIC X(8)   VALUE SPACES.

       01  GBDATA-OUT-PATH.
           03  GBDATA-PATH-DIR         PIC X(15).
           03  FILLER                  PIC X(11)  VALUE "/EOMALL-GB-".
           03  GBDATA-PATH-PROC-ID     PIC X(6).
           03  FILLER                  PIC X      VALUE "-".
           03  GBDATA-PATH-BEGBRNO     PIC X(8)   VALUE SPACES.

       01  STAT-OUT-PATH.
           03  STAT-PATH-DIR           PIC X(15).
           03  FILLER                  PIC X(13)  VALUE "/EOMALL-STAT-".
           03  STAT-PATH-BEGBRNO       PIC X(4)   VALUE SPACES.
           03  FILLER                  PIC X      VALUE "-".
           03  STAT-PATH-EOM-MM        PIC XX.
           03  STAT-PATH-EOM-DD        PIC XX.
           03  STAT-PATH-EOM-YY        PIC XX.

       01  TCLP-SHELL-COMMAND. 
           03  TCLP-SHELL-PGM.
              05  FILLER               PIC X(09)  VALUE "/$REL/SH/".
              05  TASC-EC              PIC X(03)  VALUE "EC ".
              05  FILLER               PIC X(09)  VALUE "/$REL/SH/".
              05  TASC-SCRIPT          PIC X(20)  VALUE 
                                                  "EOMALL_WK_CLEAR.SH ".
          03  TCLP-SHELL-PARMS         PIC X(200) VALUE SPACES.

       01  WS-MISC-STRING              OCCURS 7 TIMES
                                       PIC X(1700).

       01  MISC-CTR-ZZ                 PIC ZZZ9.
       01  BRANCH-OUT-RECS-CTR         PIC 9(6)   VALUE 0.
       01  TOT-BRANCH-OUT-RECS-CTR     PIC 9(9)   VALUE 0.
       01  PIPE-1                      PIC X      VALUE X"7C".
       01  WS-LNOUT-CTR                PIC 9(9)   VALUE 0.
       01  WS-LNSEQ-CTR                PIC 9(3)   VALUE 1.
       01  WS-LNSEQ-CTR-RDF            REDEFINES WS-LNSEQ-CTR
                                       PIC X(3).
       01  WS-GB-EOM-UPDATE-DATE       PIC 9(8).
       01  WS-WKRC-FIELDS.
           03  WS-RC-TRANS-DATE        PIC 9(8).
       01  WS-WKLN-FIELDS.
           03  WS-QLN-LNAMT            PIC +9(7).99.
           03  WS-QLN-1STPYAMT         PIC +9(7).99.
           03  WS-QLN-REGPYAMT         PIC +9(7).99.
           03  WS-QLN-LASTPYAMT        PIC +9(7).99.
           03  WS-QLN-MTD-PAYS-RECDEL  PIC +9(7).99.
           03  WS-QLN-DLNO             PIC +9(4).99.
           03  WS-QLN-CURBAL           PIC +9(7).99.
           03  WS-QLN-INTBAL           PIC +9(7).99.
           03  WS-QLN-LCBAL            PIC +9(7).99.
           03  WS-QLN-OTHBAL           PIC +9(7).99.
           03  WS-QLN-OT2BAL           PIC +9(7).99.
           03  WS-QLN-MONTHEND-BAL     PIC +9(7).99.
           03  WS-QLN-ANTICERN-1       PIC +9(7).99.
           03  WS-QLN-ANTICADJ-1       PIC +9(7).99.
           03  WS-QLN-UFERN-1          PIC +9(7).99.
           03  WS-QLN-ACCERN-1         PIC +9(7).99.
           03  WS-QLN-ANTICERN-2       PIC +9(7).99.
           03  WS-QLN-ANTICADJ-2       PIC +9(7).99.
           03  WS-QLN-UFERN-2          PIC +9(7).99.
           03  WS-QLN-ACCERN-2         PIC +9(7).99.
           03  WS-QLN-ANTICERN-3       PIC +9(7).99.
           03  WS-QLN-ANTICADJ-3       PIC +9(7).99.
           03  WS-QLN-UFERN-3          PIC +9(7).99.
           03  WS-QLN-ACCERN-3         PIC +9(7).99.
           03  WS-QLN-ANTICERN-4       PIC +9(7).99.
           03  WS-QLN-ANTICADJ-4       PIC +9(7).99.
           03  WS-QLN-UFERN-4          PIC +9(7).99.
           03  WS-QLN-ACCERN-4         PIC +9(7).99.
           03  WS-QLN-ANTICERN-5       PIC +9(7).99.
           03  WS-QLN-ANTICADJ-5       PIC +9(7).99.
           03  WS-QLN-UFERN-5          PIC +9(7).99.
           03  WS-QLN-ACCERN-5         PIC +9(7).99.
           03  WS-QLN-ANTICERN-6       PIC +9(7).99.
           03  WS-QLN-ANTICADJ-6       PIC +9(7).99.
           03  WS-QLN-UFERN-6          PIC +9(7).99.
           03  WS-QLN-ACCERN-6         PIC +9(7).99.
           03  WS-QLN-ANTICERN-7       PIC +9(7).99.
           03  WS-QLN-ANTICADJ-7       PIC +9(7).99.
           03  WS-QLN-UFERN-7          PIC +9(7).99.
           03  WS-QLN-ACCERN-7         PIC +9(7).99.
           03  WS-QLN-ANTICERN-8       PIC +9(7).99.
           03  WS-QLN-ANTICADJ-8       PIC +9(7).99.
           03  WS-QLN-UFERN-8          PIC +9(7).99.
           03  WS-QLN-ACCERN-8         PIC +9(7).99.
           03  WS-QLN-ANTICERN-9       PIC +9(7).99.
           03  WS-QLN-ANTICADJ-9       PIC +9(7).99.
           03  WS-QLN-UFERN-9          PIC +9(7).99.
           03  WS-QLN-ACCERN-9         PIC +9(7).99.
           03  WS-QLN-ANTICERN-10      PIC +9(7).99.
           03  WS-QLN-ANTICADJ-10      PIC +9(7).99.
           03  WS-QLN-UFERN-10         PIC +9(7).99.
           03  WS-QLN-ACCERN-10        PIC +9(7).99.
           03  WS-QLN-TOTPAYMNTD       PIC +9(7).99.
           03  WS-QLN-TOTNODEF         PIC +99.
           03  WS-QLN-YTDNODEF         PIC +99.
           03  WS-QLN-TOTNODEFACT      PIC +99.
           03  WS-QLN-YTDNODEFACT      PIC +99.
           03  WS-QLN-TOTNOALDEL       PIC +99.
           03  WS-QLN-YTDNOALDEL       PIC +99.
           03  WS-QLN-PRLNAMT          PIC +9(7).99.
           03  WS-QLN-PRPDUE-DAYS      PIC +9(5).
           03  WS-QLN-MTD-PRIN         PIC +9(7).99.
           03  WS-QLN-MTD-LC           PIC +9(5).99.
           03  WS-QLN-MTD-INT          PIC +9(5).99.
           03  WS-QLN-MTD-OT           PIC +9(5).99.
           03  WS-QLN-MTD-EX           PIC +9(7).99.
           03  WS-QLN-LOANDATE         PIC 9(8).
           03  WS-QLN-ORIG-1STPYDATE   PIC 9(8).
           03  WS-QLN-1STPYDATE        PIC 9(8).
           03  WS-QLN-PLDATE           PIC 9(8).
           03  WS-QLN-POFFDATE         PIC 9(8).
           03  WS-QLN-BNKRPTDATE       PIC 9(8).
           03  WS-QLN-REPODATE         PIC 9(8).
           03  WS-QLN-REDEEMDATE       PIC 9(8).
           03  WS-QLN-TRW-DOFD-DATE    PIC 9(8).
           03  WS-QLN-DATE-PAID-LAST   PIC 9(8).

       01  WS-BYA-REC.
           03  WS-BYA-PERIOD-TBL              OCCURS 3.
               05  WS-BYA-ACTIVITY-TBL        OCCURS 2.
                 07  WS-BYA-TYPE-TBL          OCCURS 11.
                   09  WS-BYA-NO       PIC +9(7).
                   09  WS-BYA-AMT      PIC +9(9).99.
                   09  WS-BYA-O2       PIC +9(9).99.
                   09  WS-BYA-INT      PIC +9(9).99.

           03  WS-BYA-REPO-PERIOD-TBL         OCCURS 3.
               05  WS-BYA-REPO-ACTIVITY-TBL   OCCURS 2.
                 07  WS-BYA-REPO-TYPE-TBL     OCCURS 5.
                   09  WS-BYA-NO-REPO  PIC +9(7).
                   09  WS-BYA-AMT-REPO PIC +9(9).99.
                   09  WS-BYA-O2-REPO  PIC +9(9).99.
                   09  WS-BYA-INT-REPO PIC +9(9).99.

           03  WS-BYA-PL-PERIOD-TBL           OCCURS 3.
               05  WS-BYA-PL-ACTIVITY-TBL     OCCURS 2.
                   07  WS-BYA-PL-TYPE-TBL     OCCURS 10.
                      09  WS-BYA-NO-PL    PIC +9(7).
                      09  WS-BYA-AMT-PL   PIC +9(9).99.
                      09  WS-BYA-O2-PL    PIC +9(9).99.
                      09  WS-BYA-INT-PL   PIC +9(9).99.

           03  WS-BYA-DLR-PERIOD-TBL              OCCURS 3.
               05  WS-BYA-DLR-ACTIVITY-TBL        OCCURS 2.
                   07  WS-BYA-DLRVPRIN            OCCURS 3
                                          PIC +9(9).99.
                   07  WS-BYA-DLRVCHGS            OCCURS 3
                                          PIC +9(9).99.
                   07  WS-BYA-DLPARVHELD  PIC +9(9).99.

       01  WS-BYB-REC.
           03  WS-BYB-MTD-YTD              OCCURS 2.
               05  WS-BYB-PL-RECOVERED       OCCURS 5.
                   07  WS-BYB-PL-PRINCOL  PIC +9(9).99.
                   07  WS-BYB-PL-INTCOL   PIC +9(9).99.
                   07  WS-BYB-PL-LATECOL  PIC +9(9).99.
                   07  WS-BYB-PL-O2COL    PIC +9(9).99.
               05  WS-BYB-AMT-SOLD        PIC +9(9).99.
               05  WS-BYB-AMT-TRANS       PIC +9(9).99.
               05  WS-BYB-AMT-PURCH       PIC +9(9).99.
               05  WS-BYB-NO-SOLD         PIC +9(7).
               05  WS-BYB-NO-TRANS        PIC +9(7).
               05  WS-BYB-NO-PURCH        PIC +9(7).
               05  WS-BYB-OTHCOL          PIC +9(9).99.
               05  WS-BYB-OT2COL          PIC +9(9).99.
               05  WS-BYB-INTCOL          PIC +9(9).99.
               05  WS-BYB-LATECOL         PIC +9(9).99.
               05  WS-BYB-PRINCOL         PIC +9(9).99.
               05  WS-BYB-DEFCOL          PIC +9(9).99.
               05  WS-BYB-RNPRIN          PIC +9(9).99.
               05  WS-BYB-INTREB          PIC +9(9).99.
               05  WS-BYB-CLREB1          PIC +9(9).99.
               05  WS-BYB-CLREB2          PIC +9(9).99.
               05  WS-BYB-CLREB3          PIC +9(9).99.
               05  WS-BYB-CLREB4          PIC +9(9).99.
               05  WS-BYB-AHREB           PIC +9(9).99.
               05  WS-BYB-PPREB           PIC +9(9).99.
               05  WS-BYB-O1REB           PIC +9(9).99.
               05  WS-BYB-O2REB           PIC +9(9).99.
               05  WS-BYB-O3REB           PIC +9(9).99.
               05  WS-BYB-O4REB           PIC +9(9).99.
               05  WS-BYB-O5REB           PIC +9(9).99.
               05  WS-BYB-O6REB           PIC +9(9).99.
               05  WS-BYB-O7REB           PIC +9(9).99.
               05  WS-BYB-SCREB           PIC +9(9).99.
               05  WS-BYB-MFREB           PIC +9(9).99.
               05  WS-BYB-DFREB           PIC +9(9).99.
               05  WS-BYB-UN7-REB         PIC +9(9).99.
               05  WS-BYB-UN8-REB         PIC +9(9).99.
               05  WS-BYB-UN9-REB         PIC +9(9).99.
               05  WS-BYB-UN10-REB        PIC +9(9).99.
               05  WS-BYB-PART-CHGOFF     PIC +9(9).99.
               05  WS-BYB-ALLOTMENTS      PIC +9(9).99.
               05  WS-BYB-INS-TYPE            OCCURS 10.
                   07  WS-BYB-INS-NO-ELIG     PIC +9(7).
                   07  WS-BYB-INS-NO-WROTE    PIC +9(7).
                   07  WS-BYB-INS-NO-NOTWROTE PIC +9(7).
                   07  WS-BYB-INS-AMT-WROTE   PIC +9(9).99.
               05  WS-BYB-FEE-TYPE            OCCURS 3.
                   07  WS-BYB-FEE-NO-ELIG     PIC +9(7).
                   07  WS-BYB-FEE-NO-WROTE    PIC +9(7).
                   07  WS-BYB-FEE-NO-NOTWROTE PIC +9(7).
                   07  WS-BYB-FEE-AMT-WROTE   PIC +9(9).99.
               05  WS-BYB-OTH-NO-COL          PIC +9(7).
               05  WS-BYB-OT2-NO-COL          PIC +9(7).
               05  WS-BYB-INT-NO-COL          PIC +9(7).
               05  WS-BYB-LATE-NO-COL         PIC +9(7).
               05  WS-BYB-OTH-NO-WAIVED       PIC +9(7).
               05  WS-BYB-OT2-NO-WAIVED       PIC +9(7).
               05  WS-BYB-INT-NO-WAIVED       PIC +9(7).
               05  WS-BYB-LATE-NO-WAIVED      PIC +9(7).
               05  WS-BYB-OTH-AMT-WAIVED      PIC +9(9).99.
               05  WS-BYB-OT2-AMT-WAIVED      PIC +9(9).99.
               05  WS-BYB-INT-AMT-WAIVED      PIC +9(9).99.
               05  WS-BYB-LATE-AMT-WAIVED     PIC +9(9).99.
           03  WS-BYB-NOW-BMO-LME        OCCURS 3.
               05  WS-BYB-REC-CON         OCCURS 2.
                   07  WS-BYB-DEL-BUCKET   OCCURS 10.
                       09  WS-BYB-UNITS     PIC +9(7).
                       09  WS-BYB-OUTBAL    PIC +9(9).99.
           03  WS-BYB-NOW-BMO-LME-BNKRPT        OCCURS 3.
               05  WS-BYB-REC-CON-BNKRPT         OCCURS 2.
                   07  WS-BYB-DEL-BUCKET-BNKRPT   OCCURS 10.
                       09  WS-BYB-UNITS-BNKRPT     PIC +9(7).
                       09  WS-BYB-OUTBAL-BNKRPT    PIC +9(9).99.
           03  WS-BYB-SUMWGTTM         PIC +9(8).
           03  WS-BYB-SUMWGTAV         PIC +9(9).99.
           03  WS-BYB-SUMWGTTA         PIC +9(11).99.
           03  WS-BYB-IB-ACCRUED       PIC +9(7).99.
           03  WS-BYB-DLDISC-VOL-TBL     OCCURS 3.
               05 WS-BYB-DLDISC        PIC +9(9).99.
           03  WS-BYB-STD-POS          PIC +9(7).
           03  WS-BYB-30-DAY-MOVED     PIC +9(7).
           03  WS-BYB-PROJ-INCREASE    PIC +9(7).
           03  WS-BYB-PROJ-PROFIT      PIC +9(7).
           03  WS-BYB-PROCEEDS        OCCURS 2  PIC +9(9).99.
           03  WS-BYB-LOCKBOX         OCCURS 2  PIC +9(9).99.
           03  WS-BYB-UNEARNED-PERIOD         OCCURS 3.
               05  WS-BYB-UNEARNED-ACTIVITY    OCCURS 2.
                   07  WS-BYB-UNEARNED-TYPE     OCCURS 10.
                       09  WS-BYB-UNEARNED-AMT    PIC +9(9).99.

      *****************************************************************
      *   MISC WORKERS     *
      *****************************************************************
       01  WS-PROC-CLASS           PIC 9(3)      VALUE 0.
       01  WS-PROC-DLNO            PIC 9(4)V99   VALUE 0.

       01  SUB                     PIC 9(6) COMP.
       01  SUB2                    PIC 9(6) COMP.
       01  SUB3                    PIC 9(6) COMP.
       01  BRANCH-SUB              PIC 9(9).
       01  SUBX.
           03  SUBXN               PIC 9.
       01  ONE                     PIC S9.
       01  FIRST-TIME              PIC X    VALUE "Y".
       01  MESS-LIST               PIC X(5) VALUE "MESS.".
       01  WARNING-LIST            PIC X(8) VALUE "WARNING.".
       01  LEGEND                  PIC X(70) VALUE SPACES.
       01  PROGRESS-MESS REDEFINES LEGEND.
           03  PROGRESS-1          PIC X(19).
           03  PROGRESS-BRNO       PIC Z999.
           03  PROGRESS-2          PIC X(47).
       01  AGING-DATE-MESS.
           03  FILLER              PIC X(28) VALUE
               "END OF MONTH AGEING DATE : ".
           03  D-AGE-DATE          PIC 99/99/99.
       01  HOLD-RC2-KEY.
           03  HOLD-RC-STATUS      PIC X(2).
           03  HOLD-RC-BRNO        PIC 9(4).
           03  HOLD-RC-TRANS-DATE  PIC 9(8).

       01  EOM-RC-TRANS-DATE       PIC 9(8).
       01  LAST-DAY-OF-THIS-MONTH  PIC 9(8).
       01  FIRST-DAY-OF-NEXT-MONTH PIC 9(8).
       01  HOLD-BY1-KEY            PIC 9(13) VALUE 0.
       01  THIS-MO-BY-DATE-CCYYMM  PIC 9(6)  VALUE 0.
       01  NEXT-MO-BY-DATE-CCYYMM  PIC 9(6)  VALUE 0.
       01  HOLD-DS1-KEY            PIC X(19).
       01  THIS-MO-SETDS-DATE-CCYYMM   PIC 9(6)  VALUE 0.
       01  NEXT-MO-SETDS-DATE-CCYYMM   PIC 9(6)  VALUE 0.
       01  SAVE-OUTBAL             PIC S9(7)V99 VALUE 0.
       01  SAVE-RDSUB          PIC S9(6)   COMP.
       01  SAVE-RECDEL         PIC S9(5)   COMP.
       01  SAVE-RDPOT30        PIC X.
       01  SAVE-RSUB           PIC S9(6)   COMP.
       01  SAVE-RECENCY        PIC S9(5)   COMP.
       01  SAVE-CSUB           PIC S9(6)   COMP.
       01  SAVE-CONTRACTUAL    PIC S9(5)   COMP.
       01  SAVE-CPOT30         PIC X.
       01  SAVE-PSUB           PIC S9(6)   COMP.

       01  RM-TR-DATE        PIC 9(8) VALUE 0.
       01  RM-TR-DATEX       REDEFINES RM-TR-DATE.
           05  RM-TR-CC      PIC 99.
           05  RM-TR-YEAR    PIC 99.
           05  RM-TR-MONTH   PIC 99.
           05  RM-TR-DAY     PIC 99.

       01  RM-EOD-DATE       PIC 9(8) VALUE 0.
       01  RM-EOD-DATEX      REDEFINES RM-EOD-DATE.
           05  RM-EOD-CC     PIC 99.
           05  RM-EOD-YEAR   PIC 99.
           05  RM-EOD-MONTH  PIC 99.
           05  RM-EOD-DAY    PIC 99.

       01  PROFILE-WORKER.
           03  WORK-LN-PROFILE   PIC 9(12)  OCCURS 5.
       01  PROFILE-WORKER2      REDEFINES PROFILE-WORKER.
           03 WORK-PROFILE       PIC 9      OCCURS 60.

       01  SAVE-UNEARN.
           03  SAVE-UNEARNED     OCCURS 10
                                 PIC S9(9)V99   COMP.

       01  SAVE-LOGNAME          PIC X(10) VALUE SPACES.

      *****************************************************************
      *     S C R E E N   F I E L D S   *
      *****************************************************************
       COPY "LIBGB/EOBUF_EXT.CPY".
           03  BEG-BRANCH          PIC 9(4).
           03  END-BRANCH          PIC 9(4).
           03  EOM-DATE            PIC 9(8).
           03  DATE1 REDEFINES EOM-DATE.
               05  EOM-CC          PIC 99.
               05  EOM-YR          PIC 99.
               05  EOM-MO          PIC 99.
               05  EOM-DA          PIC 99.
           03  GROUP-FLAG          PIC X.
           03  ENT-GROUP           PIC X(4).

       01 VERSION-CONTROL          PIC X VALUE "B".
      ********************************
      *   EOMUPD - COMMANDS
      ********************************
      * "/$REL/SH/EC /$REL/SH/EOM2.SH ".
       01  REMOVE-CMD.
           03  FILLER         PIC X    VALUE "/".
           03  REMOVE-REL     PIC X(7).
           03  FILLER         PIC X(4) VALUE "/SH/".
           03  REMOVE-EC      PIC X(3) VALUE "EC ".
           03  FILLER         PIC X    VALUE "/".
           03  REMOVE-REL2    PIC X(7).
           03  FILLER         PIC X(4) VALUE "/SH/".
           03  REMOVE-SHELL   PIC X(8) VALUE "EOM2.SH ".
           03  REMOVE-MM      PIC 99.
           03  FILLER         PIC X.
           03  REMOVE-YY      PIC 99.
           03  FILLER         PIC X.
           03  REMOVE-MACHINE PIC XX.
           03  FILLER         PIC X.
           03  REMOVE-DATA    PIC X(6).
           03  FILLER         PIC X.
           03  REMOVE-EOD-MM  PIC 99.
           03  FILLER         PIC X.
           03  REMOVE-EOD-YY  PIC 99.
           03  FILLER         PIC X.
           03  REMOVE-FIL-PATH PIC X(8).

      ******************************************************************
      *    P R I N T - W O R K E R S
      ******************************************************************
       01  LN-WORKERS.
           03  LN-FEED                 PIC 99        VALUE 0.
           03  LN-COUNT                PIC 99        VALUE 61.
           03  LN-WK                   PIC 99        VALUE 0.
       01  PAGE-NUMBER                 PIC 9(4)      VALUE 0.

      *=================================================================
       01  DETAIL-LINE                 PIC X(132)    VALUE SPACES.

       01  D01-BRANCH-TOTALS           REDEFINES DETAIL-LINE.
           03  D01-TOTAL-BRNO          PIC 9(4).
           03  FILLER                  PIC X(6).
           03  D01-START-TIME-HH       PIC 99.
           03  D01-START-TIME-HH-COLON PIC X(1).
           03  D01-START-TIME-MM       PIC 99.
           03  D01-START-TIME-MM-COLON PIC X(1).
           03  D01-START-TIME-SS       PIC 99.
           03  FILLER                  PIC X(3).
           03  D01-END-TIME-HH         PIC 99.
           03  D01-END-TIME-HH-COLON   PIC X(1).
           03  D01-END-TIME-MM         PIC 99.
           03  D01-END-TIME-MM-COLON   PIC X(1).
           03  D01-END-TIME-SS         PIC 99.
           03  FILLER                  PIC X(2).
           03  D01-LNRECS-OUTPUT       PIC ZZZ,ZZZ,ZZ9.
           03  FILLER                  PIC X(1).

       01  T01-TOTAL-TOTALS-D          REDEFINES DETAIL-LINE.
           03  FILLER                  PIC X(31).
           03  T01-DASHES              PIC X(11).
           03  FILLER                  PIC X(90).

       01  T01-TOTAL-TOTALS            REDEFINES DETAIL-LINE.
           03  FILLER                  PIC X(31).
           03  T01-TOT-LNRECS-OUTPUT   PIC ZZZ,ZZZ,ZZ9.
           03  FILLER                  PIC X(90).

       01  OUTFILE-LN            REDEFINES DETAIL-LINE.
           03  OUTFILE-LN-FILE-V1      PIC X(12).
           03  OUTFILE-LN-FILE         PIC X(120).

       01  RANGE-LN          REDEFINES DETAIL-LINE.
           03  RANGE-LINE              PIC X(31).
           03  RANGE-SELECTION         PIC X(101).

      *=================================================================
       01  HEAD1.
           03  H-DATE                  PIC X(8)      VALUE " ".
           03  FILLER                  PIC XX        VALUE " ".
           03  H-HR                    PIC 99        VALUE 0.
           03  FILLER                  PIC X         VALUE ":".
           03  H-MIN                   PIC 99        VALUE 0.
           03  FILLER                  PIC X(31)     VALUE " ".
           03  H-NAME                  PIC X(40)     VALUE " ".
           03  FILLER                  PIC X(37)     VALUE " ".
           03  FILLER                  PIC X(5)      VALUE "PAGE ".
           03  H-PAGE-NO               PIC ZZZ9.

      *-----------------------------------------------------------------
       01  HEAD2.
           03  FILLER                  PIC X(10)     VALUE "USER ID:".
           03  H-USERID                PIC X(10).
           03  FILLER                  PIC X(20)     VALUE " ".
           03  FILLER                  PIC X(40)     VALUE 
                          "END OF MONTH CLEAR & PROFILE UPDATE".
           03  FILLER                  PIC X(6)      VALUE " FOR: ".
           03  H-AGEDATE               PIC X(10).

      *-----------------------------------------------------------------
       01  HEAD3.
           03  FILLER PIC X(30)  VALUE "BRANCH   START TIME  END TIME".
           03  FILLER PIC X(102) VALUE "   LN RECS OUT".


      *****************************************************************
      *
      *     C O P Y    L I B R A R Y    W O R K E R S
      *
      *****************************************************************
       COPY "LIBWI/GRSCANW.CPY".
       COPY "LIBCL/CKSTATW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/MKTEMPW_CMD.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/GRWORKW.CPY".
       COPY "LIBWI/RCINQW.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBLP/RANGE_CMD.CPY".
       COPY "LIBGB/SYSTEMW.CPY".
       COPY "LIBGB/GETPRW.CPY".
       COPY "LIBGB/GPENVW.CPY".
       COPY "LIBLP/POSTDATEW.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBLP/LPBYCLW.CPY".
       COPY "LIBLP/LPDSCLW.CPY".
       COPY "LIBLP/LPSETTYW.CPY".
       COPY "LIBGB/AGEINGW.CPY".
       COPY "LIBLP/LPUPWKW.CPY".
       COPY "LIBLP/LPPDUEW.CPY".
       COPY "LIBGB/SPPDTHW.CPY".
       COPY "LIBLP/LPCEPPW.CPY".
       COPY "LIBLP/LPMDTEW.CPY".
       COPY "LIBLP/LPEARNW.CPY".
       COPY "LIBLP/LPSRCDW.CPY".
       COPY "LIBGB/DACCESSW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBLP/EOMALL_DEF.CPY".
       COPY "LIBLP/EOMALL_WKS.CPY".
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
       COPY "LIBLP/EOMALL_SCN.CPY".

       PROCEDURE DIVISION
             USING FORM-PATHNAME EXIT-PATHNAME.

       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
               PR-FILE VBY-FILE VRC-FILE VGB-FILE STAT-OUT-FILE
                BYADATA-OUT-FILE BYBDATA-OUT-FILE RCDATA-OUT-FILE 
                GBDATA-OUT-FILE LNDATA-OUT-FILE.
       COPY "LIBGB/DECLRP_ASCII.CPY".
           PERFORM CLOSE-TY1-FILE.
           PERFORM CLOSE-TRX1-FILE.
           PERFORM CLOSE-TRW1-FILE.
           PERFORM CLOSE-TRS1-FILE.
           PERFORM CLOSE-TRP1-FILE.
           PERFORM CLOSE-TRL1-FILE.
           PERFORM CLOSE-TRK1-FILE.
           PERFORM CLOSE-TRJ1-FILE.
           PERFORM CLOSE-TRI1-FILE.
           PERFORM CLOSE-TRD1-FILE.
           PERFORM CLOSE-TRC1-FILE.
           PERFORM CLOSE-TRB1-FILE.
           PERFORM CLOSE-SP1-FILE.
           PERFORM CLOSE-LTP1-FILE.
           PERFORM CLOSE-LN3-FILE.
      *    PERFORM CLOSE-DS1-FILE.                         *> S35Q-104
           PERFORM CLOSE-CA1-FILE.
           PERFORM CLOSE-BY1-FILE.
      *     PERFORM CLOSE-RC2-FILE.
           PERFORM CLOSE-RC1-FILE.
           PERFORM CLOSE-GR1-FILE.
           PERFORM CLOSE-GB-FILE.
           PERFORM CLOSE-BR-FILE.
           PERFORM CLOSE-PR-FILE.
           PERFORM CLOSE-VBY1-FILE.                      *> VISION FILE
           PERFORM CLOSE-VRC1-FILE.                      *> VISION FILE
           PERFORM CLOSE-VRC2-FILE.                      *> VISION FILE
           PERFORM CLOSE-VGB-FILE.                       *> VISION FILE
           PERFORM SQL-CLOSE-UPDATE-CONNECTION.
           PERFORM CLOSE-LN3-CONNECTION.
       COPY "LIBGB/DECLAR2.CPY".

      *****************************************************************
      *
      *    M A I N    M O D U L E
      *
      *****************************************************************
       MAIN-MODULE SECTION.

           PERFORM SQL-CONNECT.

      D    STOP MESS.
           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "EOMALL" TO VDU-FORM-NAME.

           PERFORM INITIALIZATION.
           IF EXT-EOBUF-ERRCD NOT = " "
              GO TO END-PROGRAM.

      D    STOP MESS.

           PERFORM EOM-UPDATE.

           PERFORM CLOSE-LNDATA-FILE.

           IF TOT-BRANCH-OUT-RECS-CTR = 0
      * IF NO LN RECORDS WRITTEN DELETE LN OUTPUT FILE
              MOVE LNDATA-OUT-PATH TO ACCESS-BUF
              PERFORM REMOVE-WORKFILE
              SUBTRACT 1 FROM SAVED-OUT-FILES-CTR
           ELSE
              PERFORM CREATE-ASCII-BY
              PERFORM CREATE-ASCII-RC
              PERFORM CREATE-ASCII-GB
              MOVE ALL '-' TO T01-DASHES
              PERFORM WRITE-DETAIL-LINE
              MOVE TOT-BRANCH-OUT-RECS-CTR TO T01-TOT-LNRECS-OUTPUT
              PERFORM WRITE-DETAIL-LINE
              PERFORM WRITE-DETAIL-LINE
              PERFORM VARYING SUB FROM 1 BY 1
               UNTIL SUB > SAVED-OUT-FILES-CTR
                 MOVE "SAVED FILE: " TO OUTFILE-LN-FILE-V1
                 MOVE SAVED-FILES-ARRAY(SUB) TO OUTFILE-LN-FILE
                 PERFORM WRITE-DETAIL-LINE
              END-PERFORM.

           PERFORM PRINT-RANGE-LINES.

           IF FIRST-TIME = "N"
              MOVE "END-OF-MONTH UPDATE COMPLETE" TO MESS EXT-EOBUF-MESS
           ELSE
              MOVE "NO BRANCHES UPDATED FOR END-OF-MONTH!!"
                 TO MESS EXT-EOBUF-MESS.
           PERFORM SEND-MESS.

           IF EXT-ROUTE-BUF = "70"
              IF FIRST-TIME = "N"
                 MOVE "END-OF-MONTH UPDATE COMPLETE"
                    TO ERRLOGW-EOM-MSG
                 PERFORM LOG-ERROR
              ELSE
                 MOVE "NO BRANCHES UPDATED FOR END-OF-MONTH!!"
                    TO ERRLOGW-EOM-MSG
                 PERFORM LOG-ERROR.

           MOVE "N" TO WS-PGM-FAILED-SW.

           MOVE SPACES TO STAT-REC-OUT.
           IF TOT-BRANCH-OUT-RECS-CTR = 0
              MOVE "ERROR - NO FILES CREATED" TO STAT-REC-OUT
           ELSE 
              MOVE SAVED-OUT-FILES-CTR TO MISC-CTR-ZZ
              STRING "COMPLETED - FILES CREATED: ", MISC-CTR-ZZ
               INTO STAT-REC-OUT
           END-IF.
           PERFORM UPDATE-STAT-FILE.

       END-ROUTINE.
           GO TO END-PROGRAM.

      *****************************************************************
       EOM-UPDATE SECTION.
      *****************************************************************
       NEXT-RC.
      *     PERFORM READ-RC2-FILE-NEXT.
           PERFORM READ-VRC2-FILE-NEXT. 
           IF IO-BAD 
              GO TO EXIT-EOM-UPDATE.

           MOVE VRC-RC-REC TO RC-REC.

           IF RC-STATUS > "G1"
              GO TO EXIT-EOM-UPDATE.

           IF RC-BRNO > END-BRANCH
              GO TO EXIT-EOM-UPDATE.

           IF RC-BRNO < BEG-BRANCH
              GO TO NEXT-RC.

           IF RC-TRANS-DATE NOT = EOM-RC-TRANS-DATE
              GO TO NEXT-RC.

           MOVE RC2-KEY TO HOLD-RC2-KEY.

           IF GROUP-FLAG = "N"
              GO TO READ-BR.

           MOVE RC-BRNO TO WGR-SRESULT.
           PERFORM SEARCH-GR-VERIFY.
           IF WGR-ERR > 0
              GO TO NEXT-RC.

       READ-BR.
           PERFORM GET-TIME.
           MOVE CUR-HH TO D01-START-TIME-HH.
           MOVE CUR-MM TO D01-START-TIME-MM.
           MOVE CUR-SS TO D01-START-TIME-SS.
           MOVE ":"  TO D01-START-TIME-HH-COLON 
                        D01-START-TIME-MM-COLON
                        D01-END-TIME-HH-COLON
                        D01-END-TIME-MM-COLON.

           MOVE 0 TO BRANCH-OUT-RECS-CTR.

           MOVE RC-BRNO TO BR-NO D01-TOTAL-BRNO PROC-BRANCH.
           PERFORM READ-BR-FILE.
           IF IO-BAD
              GO TO IO-ERROR.

      *----------------------------------------------------------------
       GET-BRANCH-PATHS.
           MOVE "BEGINNING END-OF-MONTH PROCESSING" TO ERRLOGW-EOM-MSG.
           PERFORM LOG-ERROR.
           IF BR-DATA-PATH = SPACES OR BR-MACHINE = SPACES
              MOVE "BRANCH &&&& HAS UNDEFINED DATA PATH"
                 TO ERRLOGW-EOM-MSG
              INSPECT ERRLOGW-EOM-MSG
                 REPLACING FIRST "&&&&" BY BR-NO
              PERFORM LOG-ERROR
              GO TO NEXT-RC.

           MOVE EXT-FILPATH-BASE           TO FILPATH-BASE.
           MOVE BR-MACHINE                 TO FILPATH-MACHINE.
           MOVE BR-DATA-PATH               TO FILPATH-DATA.

           MOVE "B"                        TO BY-PATH-OVERRIDE
                                              CA-PATH-OVERRIDE
                                              DS-PATH-OVERRIDE
                                              LN-PATH-OVERRIDE
                                              LTP-PATH-OVERRIDE
                                              TY-PATH-OVERRIDE 
                                              GB-PATH-OVERRIDE
                                              TRB-PATH-OVERRIDE
                                              TRC-PATH-OVERRIDE
                                              TRD-PATH-OVERRIDE
                                              TRI-PATH-OVERRIDE
                                              TRJ-PATH-OVERRIDE
                                              TRK-PATH-OVERRIDE
                                              TRL-PATH-OVERRIDE
                                              TRP-PATH-OVERRIDE
                                              TRS-PATH-OVERRIDE
                                              TRW-PATH-OVERRIDE
                                              TRX-PATH-OVERRIDE.

      *    PERFORM CREATE-EOM-TAG.

           PERFORM UPDATE-BY-EOM.
      *    PERFORM UPDATE-DS-EOM.                           *> S35Q-104

           MOVE 0 TO WS-PROC-CLASS WS-PROC-DLNO.
           PERFORM UPDATE-LN-EOM.

           PERFORM UPDATE-BY-SYNC-P2-P3.
      *    PERFORM UPDATE-DS-SYNC-P2-P3.                    *> S35Q-104

           IF GP-UNEARNED-STATS = "Y"
              PERFORM UPDATE-BY-CUR-UNEARNED.
      *       PERFORM UPDATE-DS-CUR-UNEARNED.               *> S35Q-104

      *DONT SEE THAT THIS SERVES ANY PURPOSE
      *    PERFORM REMOVE-PREV-MONTH-TAG.

           IF ( GP-TR-RETAIN-MONTHS  NOT = 99 ) OR
              ( GP-EOD-RETAIN-MONTHS NOT = 99 )
              PERFORM REMOVE-OP-AND-SPOOL.

           IF GP-TR-RETAIN-MONTHS NOT = 99
              PERFORM REMOVE-TY-RECORDS
              PERFORM REMOVE-CA-RECORDS
              PERFORM REMOVE-TR-RECORDS.

      *EOM DOESNT APPEAR TO USE THE END OF MONTH TAG BUT OTHER PROGRAMS
      *CHECK IT, I MOVED IT DOWN FURTHER IN THE UPDATE PROCESS PRIOR
      *TO REWRITING THE RC-FILE WITH AN I1 STATUS.
           PERFORM UPDATE-GB-EOM-CCYYMM-DATE.

           PERFORM UPDATE-RC-STATUS.

           MOVE "N" TO FIRST-TIME.

           MOVE "COMPLETED END-OF-MONTH PROCESSING" TO ERRLOGW-EOM-MSG.
           PERFORM LOG-ERROR.                              *> S35Q-104

           MOVE "Y" TO PROC-BRANCH-ARRAY(BR-NO).

           PERFORM GET-TIME.
           MOVE CUR-HH TO D01-END-TIME-HH.
           MOVE CUR-MM TO D01-END-TIME-MM.
           MOVE CUR-SS TO D01-END-TIME-SS.
           MOVE BRANCH-OUT-RECS-CTR TO D01-LNRECS-OUTPUT.

           PERFORM WRITE-DETAIL-LINE.

           GO TO NEXT-RC.

       EXIT-EOM-UPDATE.
           EXIT.


      ***************************************************************
      *   UPDATE GLOBAL WITH MONTH END YYYYMM (REPLACES THE TAG FILE
      *   THAT WAS IN LOCAL ED DIRECTORY
      *****************************************************************
       UPDATE-GB-EOM-CCYYMM-DATE SECTION.
           PERFORM OPEN-GB-FILE.

      *     MOVE GB-PATH-OWNBR              TO GB-BRNO.
      *     MOVE 1                          TO GB-NO.
      *     PERFORM READ-GB-FILE.

      *    MOVE BR-NO                       TO VGB-BRNO.
      *    MOVE 1                           TO VGB-NO.
      *    PERFORM READ-VGB-FILE.
      *    IF IO-BAD                              
      *       MOVE "VGBFILE ACCESS ERROR " TO ERRLOGW-EOM-MSG
      *       PERFORM LOG-ERROR
      *       GO TO UPDATE-GB-EOM-CCYYMM-END   
      *    ELSE
      *        MOVE VGB-REC  TO GB-REC
      *        MOVE EOM-DATE TO DATE-YYYYMMDD
      *        PERFORM CONVERT-YYYYMMDD-TO-YYYYMM
      *        MOVE DATE-YYYYMM            TO GB-EOM-TAG-CCYYMM
      *        ACCEPT GB-EOM-UPDATE-DATE     FROM CENTURY-DATE
      *        MOVE "Y" TO TIME-MILITARY-FG
      *        PERFORM GET-TIME
      *        MOVE HH-MM-SS               TO GB-EOM-UPDATE-TIME
      *        MOVE SAVE-LOGNAME           TO GB-EOM-UPDATE-USERID
      *         PERFORM REWRITE-GB-FILE.
      *        MOVE GB-KEY  TO VGB-KEY
      *        MOVE GB-REC  TO VGB-GB-REC
      *        PERFORM REWRITE-VGB-FILE.

      *****************************************************************
      * GET GBFILE ROW - UPDATE FIELDS - WRITE/INSERT INTO VGBFILE
      *****************************************************************
           MOVE BR-NO                       TO GB-BRNO.
           MOVE 1                           TO GB-NO.
           PERFORM READ-GB-FILE.
           IF IO-BAD                              
              MOVE "GBFILE ACCESS ERROR " TO ERRLOGW-EOM-MSG
              PERFORM LOG-ERROR
              GO TO UPDATE-GB-EOM-CCYYMM-END.

           MOVE EOM-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-YYYYMM.
           MOVE DATE-YYYYMM            TO GB-EOM-TAG-CCYYMM.
           ACCEPT GB-EOM-UPDATE-DATE FROM CENTURY-DATE.
           MOVE "Y" TO TIME-MILITARY-FG.
           PERFORM GET-TIME.
           MOVE HH-MM-SS               TO GB-EOM-UPDATE-TIME.
           MOVE SAVE-LOGNAME           TO GB-EOM-UPDATE-USERID.
           MOVE GB-KEY  TO VGB-KEY.
           MOVE GB-REC  TO VGB-GB-REC.

           PERFORM WRITE-VGB-FILE.
           IF IO-BAD                              
              MOVE "VGBFILE ACCESS ERROR " TO ERRLOGW-EOM-MSG
              PERFORM LOG-ERROR
              GO TO UPDATE-GB-EOM-CCYYMM-END.

       UPDATE-GB-EOM-CCYYMM-END.
           PERFORM CLOSE-GB-FILE.

       UPDATE-GB-EOM-CCYYMM-EXIT. 
           EXIT.

      *****************************************************************
      *   UPDATE BY EOM CREATE NEXT MONTH RECORD AND SET & CLEAR
      *****************************************************************
       UPDATE-BY-EOM SECTION.
      *    PERFORM OPEN-BY1-FILE.
           PERFORM OPEN-VBY1-FILE.

           MOVE "PROCESSING BRANCH #"              TO PROGRESS-1.
           MOVE BR-NO                              TO PROGRESS-BRNO.
           MOVE ": CLEARING BRANCH STATISTICS EOM" TO PROGRESS-2.
           PERFORM SEND-LEGEND.

           MOVE LAST-DAY-OF-THIS-MONTH TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-YYYYMM.
           MOVE DATE-YYYYMM TO BY-DATE-CCYYMM THIS-MO-BY-DATE-CCYYMM
                               QBY1-WBEG-DATE-CCYYMM
                               QBY1-WEND-DATE-CCYYMM.
           MOVE BR-NO       TO BY-BRNO
                               QBY1-WBEG-BRNO
                               QBY1-WEND-BRNO.
           MOVE 0           TO BY-CLASS
                               QBY1-WBEG-CLASS.
           MOVE ALL "9"     TO QBY1-WEND-CLASS.

      *    PERFORM START-BY1-FILE.
           MOVE BY1-KEY TO VBY1-KEY.
           PERFORM START-VBY1-FILE.

       BY-BRANCH-FILE-READ-NEXT.
      *    PERFORM READ-BY1-FILE-NEXT.
           PERFORM READ-VBY1-FILE-NEXT.

           IF IO-BAD
              GO TO EXIT-UPDATE-BY.

           MOVE VBY-BY-REC TO BY-REC.

           IF BY-BRNO NOT = BR-NO
              GO TO EXIT-UPDATE-BY.

           IF BY-DATE-CCYYMM NOT = THIS-MO-BY-DATE-CCYYMM
              GO TO EXIT-UPDATE-BY.

      * WHEN YOU FIND THIS MONTH'S END OF MONTH RECORD, CREATE NEXT MONTH'S
      * RECORD AND ONLY CLEAR THAT NEW RECORDS MTD FIGURES.
           MOVE BY1-KEY TO HOLD-BY1-KEY.
           PERFORM CREATE-NEW-MONTHS-BY.

      * CLEARING OUT P&L COLLECTED:
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 5
               MOVE 0 TO BY-PL-PRINCOL(1 SUB)
                         BY-PL-INTCOL(1 SUB)
                         BY-PL-LATECOL(1 SUB)
                         BY-PL-O2COL(1 SUB)
           END-PERFORM.

      * CLEAR MTD FIELDS:
           MOVE 0 TO BY-NO-SOLD(1)  BY-AMT-SOLD(1)
                     BY-NO-TRANS(1) BY-AMT-TRANS(1)
                     BY-NO-PURCH(1) BY-AMT-PURCH(1)
                     BY-OTHCOL(1)   BY-OT2COL(1)  BY-INTCOL(1)
                     BY-LATECOL(1)  BY-PRINCOL(1) BY-DEFCOL(1)
                     BY-RNPRIN(1)   BY-INTREB(1)
                     BY-CLREB1(1)   BY-CLREB2(1)  BY-CLREB3(1)
                     BY-CLREB4(1)   BY-AHREB(1)   BY-PPREB(1)
                     BY-O1REB(1)    BY-O2REB(1)   BY-O3REB(1)
                     BY-O4REB(1)    BY-O5REB(1)   BY-O6REB(1)
                     BY-O7REB(1)    BY-SCREB(1)   BY-MFREB(1)
                     BY-DFREB(1)    BY-PART-CHGOFF(1) BY-ALLOTMENTS(1)
                     BY-IB-ACCRUED  BY-30-DAY-MOVED
                     BY-PROJ-PROFIT BY-PROJ-INCREASE
                     BY-PROCEEDS(1) BY-LOCKBOX(1).

      * CLEAR INS. ELIG AND WROTE MTD:
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 10
               MOVE 0 TO BY-INS-NO-ELIG(1 SUB)
                         BY-INS-NO-WROTE(1 SUB)
                         BY-INS-NO-NOTWROTE(1 SUB)
                         BY-INS-AMT-WROTE(1 SUB)
           END-PERFORM.

      * CLEAR FEE INSURANCES (NONFILE, AD&D, ROADGARD)
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 3
               MOVE 0 TO BY-FEE-NO-ELIG(1 SUB)
                         BY-FEE-NO-WROTE(1 SUB)
                         BY-FEE-NO-NOTWROTE(1 SUB)
                         BY-FEE-AMT-WROTE(1 SUB)
           END-PERFORM.

      * CLEAR OUT MISC COLLECTED & WAIVED:
           MOVE 0 TO BY-OTH-NO-COL(1)
                     BY-OT2-NO-COL(1)
                     BY-INT-NO-COL(1)
                     BY-LATE-NO-COL(1)

                     BY-OTH-NO-WAIVED(1)
                     BY-OT2-NO-WAIVED(1)
                     BY-INT-NO-WAIVED(1)
                     BY-LATE-NO-WAIVED(1)

                     BY-OTH-AMT-WAIVED(1)
                     BY-OT2-AMT-WAIVED(1)
                     BY-INT-AMT-WAIVED(1)
                     BY-LATE-AMT-WAIVED(1).

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      * CLEAR MTD VOL PERIOD TABLE AND SET ELM NET/GAIN:
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 11
               MOVE 0 TO BY-NO(1 2 SUB)
                         BY-AMT(1 2 SUB)
                         BY-O2(1 2 SUB)
                         BY-INT(1 2 SUB)
      *-       -       -       -       -       -       -       -       -
      *        CLEAR CURRENT NET, WHICH WILL BE REGENERATED:
      *-       -       -       -       -       -       -       -       -
               MOVE 0 TO BY-NO(1 1 SUB)
                         BY-AMT(1 1 SUB)
                         BY-O2(1 1 SUB)
                         BY-INT(1 1 SUB)
           END-PERFORM.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      * CLEAR CURRENT NET REPO WHICH WILL BE REGENERATED (BUT SKIP
      * REPO BUCKET 2, CAN'T REGENERATE REDEEMED.....
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 1
               MOVE 0 TO BY-NO-REPO(1 1 SUB)
                         BY-AMT-REPO(1 1 SUB)
                         BY-O2-REPO(1 1 SUB)
                         BY-INT-REPO(1 1 SUB)
           END-PERFORM.
           PERFORM VARYING SUB FROM 3 BY 1 UNTIL SUB > 5
               MOVE 0 TO BY-NO-REPO(1 1 SUB)
                         BY-AMT-REPO(1 1 SUB)
                         BY-O2-REPO(1 1 SUB)
                         BY-INT-REPO(1 1 SUB)
           END-PERFORM.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      * CLEAR CURRENT NET P&L WHICH WILL BE REGENERATED:
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 10
               MOVE 0 TO BY-NO-PL(1 1 SUB)
                         BY-AMT-PL(1 1 SUB)
                         BY-O2-PL(1 1 SUB)
                         BY-INT-PL(1 1 SUB)
           END-PERFORM.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *     CLEAR MTD VOL UNEARNED:
      *-    -     -    -    -    -    -    -    -    -    -
      *     CLEAR CURRENT & ELM NET UNEARNED, WHICH WILL BE REGENERATED:
      *-    -     -    -    -    -    -    -    -    -    -
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 10
             MOVE 0 TO BY-UNEARNED-AMT(BYMTD BYVOL SUB)

             MOVE 0 TO BY-UNEARNED-AMT(BYCUR BYNET SUB)
                       BY-UNEARNED-AMT(BYELM BYNET SUB)
           END-PERFORM.

      * CLEAR MTD VOL REPO:
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 5
               MOVE 0 TO BY-NO-REPO(1 2 SUB)
                         BY-AMT-REPO(1 2 SUB)
                         BY-O2-REPO(1 2 SUB)
                         BY-INT-REPO(1 2 SUB)
           END-PERFORM.

      * SET ELM NET REPO ONLY FOR REDEEMED, THE REST WILL BE REGENERATED:
            MOVE BY-NO-REPO(1 1 2)  TO BY-NO-REPO(2 1 2).
            MOVE BY-AMT-REPO(1 1 2) TO BY-AMT-REPO(2 1 2).
            MOVE BY-O2-REPO(1 1 2)  TO BY-O2-REPO(2 1 2).
            MOVE BY-INT-REPO(1 1 2) TO BY-INT-REPO(2 1 2).

      * CLEAR MTD VOL PL:
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 10
               MOVE 0 TO BY-NO-PL(1 2 SUB)
                         BY-AMT-PL(1 2 SUB)
                         BY-O2-PL(1 2 SUB)
                         BY-INT-PL(1 2 SUB)
           END-PERFORM.

      * CLEAR MTD VOL RESERVES AND SET ELM NET RESERVES:
           MOVE BY-DLPARVHELD(1 1) TO BY-DLPARVHELD(2 1).
           MOVE 0 TO BY-DLPARVHELD(1 2).
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 3
               MOVE 0 TO BY-DLRVPRIN(1 2 SUB)
                         BY-DLRVCHGS(1 2 SUB)
               MOVE BY-DLRVPRIN(1 1 SUB) TO BY-DLRVPRIN(2 1 SUB)
               MOVE BY-DLRVCHGS(1 1 SUB) TO BY-DLRVCHGS(2 1 SUB)
           END-PERFORM.

      * CLEAR DLDISC MTD VOL
           MOVE 0 TO BY-DLDISC(1).

      * SET END OF LAST MONTH, DELINQUENCY AND CLEAR CURRENT
      * AND BEGIN OF MONTH:
           PERFORM VARYING SUB3 FROM 1 BY 1 UNTIL SUB3 > 10
                      AFTER SUB2 FROM 1 BY 1 UNTIL SUB2 > 2
               MOVE BY-UNITS(1 SUB2 SUB3)  TO BY-UNITS(3 SUB2 SUB3)
               MOVE BY-OUTBAL(1 SUB2 SUB3) TO BY-OUTBAL(3 SUB2 SUB3)
               MOVE BY-UNITS-BNKRPT(1 SUB2 SUB3)
                                     TO BY-UNITS-BNKRPT(3 SUB2 SUB3)
               MOVE BY-OUTBAL-BNKRPT(1 SUB2 SUB3)
                                     TO BY-OUTBAL-BNKRPT(3 SUB2 SUB3)

               MOVE 0 TO BY-UNITS(1 SUB2 SUB3)
                         BY-UNITS(2 SUB2 SUB3)

                         BY-OUTBAL(1 SUB2 SUB3)
                         BY-OUTBAL(2 SUB2 SUB3)

                         BY-UNITS-BNKRPT(1 SUB2 SUB3)
                         BY-UNITS-BNKRPT(2 SUB2 SUB3)

                         BY-OUTBAL-BNKRPT(1 SUB2 SUB3)
                         BY-OUTBAL-BNKRPT(2 SUB2 SUB3)
           END-PERFORM.
      
      *    PERFORM REWRITE-BY1-FILE.
           MOVE BY1-KEY TO VBY1-KEY.
           MOVE BY-REC  TO VBY-BY-REC.
           PERFORM REWRITE-VBY1-FILE.


      * GET ORIGINAL EOM RECORD BACK IN
           MOVE HOLD-BY1-KEY TO BY1-KEY.
           MOVE BY-DATE-CCYYMM TO QBY1-WBEG-DATE-CCYYMM
                                  QBY1-WEND-DATE-CCYYMM.
           MOVE BY-BRNO        TO QBY1-WBEG-BRNO
                                  QBY1-WEND-BRNO.
           MOVE BY-CLASS       TO QBY1-WBEG-CLASS.
           MOVE ALL "9"        TO QBY1-WEND-CLASS.
      *    PERFORM START-BY1-FILE.
      *    PERFORM READ-BY1-FILE-NEXT.
           MOVE BY1-KEY TO VBY1-KEY.
           PERFORM START-VBY1-FILE.
           PERFORM READ-VBY1-FILE-NEXT.
           MOVE VBY-BY-REC TO BY-REC.

           GO TO BY-BRANCH-FILE-READ-NEXT.

       EXIT-UPDATE-BY.
      *    PERFORM CLOSE-BY1-FILE.
           MOVE "BRANCH STATISTICS MTD CLEAR COMPLETED"
                                     TO ERRLOGW-EOM-MSG.
           PERFORM LOG-ERROR.

      *****************************************************
       CREATE-NEW-MONTHS-BY SECTION.
      *****************************************************
      * STILL HAVE LAST MONTHS RECORD IN THE BY-REC
      * FILL IN NEW DATE AND WRITE NEW RECORD FOR NEXT MONTH

           MOVE NEXT-MO-BY-DATE-CCYYMM TO BY-DATE-CCYYMM.
      *    PERFORM WRITE-BY1-FILE.

           MOVE BY1-KEY TO VBY1-KEY.
           MOVE BY-REC  TO VBY-BY-REC.
           PERFORM WRITE-VBY1-FILE.

           IF IO-BAD
      *       PERFORM REWRITE-BY1-FILE.
              PERFORM REWRITE-VBY1-FILE.

      *****************************************************************
      *   UPDATE DS EOM CREATE NEXT MONTH RECORD AND SET & CLEAR
      *****************************************************************
      * UPDATE-DS-EOM SECTION.                             *> S35Q-104

      *     PERFORM OPEN-DS1-FILE.

      *     MOVE "PROCESSING BRANCH #" TO PROGRESS-1.
      *     MOVE BR-NO TO PROGRESS-BRNO.
      *     MOVE ": CLEARING DEALER STATISTICS EOM" TO PROGRESS-2.
      *     PERFORM SEND-LEGEND.

      *     MOVE DS-PATH-OWNBR TO DS-BRANCH
      *                           QDS1-WBEG-BRANCH
      *                           QDS1-WEND-BRANCH.
      *     MOVE LAST-DAY-OF-THIS-MONTH TO DATE-YYYYMMDD.
      *     PERFORM CONVERT-YYYYMMDD-TO-YYYYMM.
      *     MOVE DATE-YYYYMM   TO DS-DATE-CCYYMM
      *                           THIS-MO-SETDS-DATE-CCYYMM
      *                           QDS1-WBEG-DATE-CCYYMM
      *                           QDS1-WEND-DATE-CCYYMM.

      *     MOVE 0             TO DS-DLNO DS-CLASS
      *                           QDS1-WBEG-NUM
      *                           QDS1-WBEG-CLASS.

      *     MOVE ALL "9"       TO QDS1-WEND-NUM
      *                           QDS1-WEND-CLASS.

      *     PERFORM START-DS1-FILE.

      * DEALER-EOM-READ-NEXT.
      *     PERFORM READ-DS1-FILE-NEXT.
      *     IF IO-BAD OR (DS-BRANCH NOT = DS-PATH-OWNBR)
      *        GO TO EXIT-UPDATE-DS.

      *     IF DS-DATE-CCYYMM NOT = THIS-MO-SETDS-DATE-CCYYMM
      *        GO TO EXIT-UPDATE-DS.

      ** WHEN YOU FIND THIS MONTH'S END OF MONTH RECORD, CREATE NEXT MONTH'S
      ** RECORD AND ONLY CLEAR THAT NEW RECORDS MTD FIGURES.
      *     MOVE DS1-KEY TO HOLD-DS1-KEY.

      *     PERFORM CREATE-NEW-MONTHS-DS.

      ** CLEARING OUT RECORD WITH NEXT MONTHS DATE

      **    CLEAR OUTSTANDING WICH WILL BE REGENERATED
      *     MOVE 0              TO DS-OUTSUNIT(1)  DS-OUTSAMT(1).
      *     MOVE 0              TO DS-PLOSUNIT(1)  DS-PLOSAMT(1).

      *     MOVE DS-RCPLUNIT(1) TO DS-RCPLUNIT(2).
      *     MOVE DS-JDOSUNIT(1) TO DS-JDOSUNIT(2).

      *     MOVE DS-RCPLAMT(1)  TO DS-RCPLAMT(2).
      *     MOVE DS-JDOSAMT(1)  TO DS-JDOSAMT(2).

      *     MOVE 0 TO DS-NPURAMT(1) DS-NPURUNIT(1).
      *     MOVE 0 TO DS-OPURAMT(1) DS-OPURUNIT(1).
      *     MOVE 0 TO DS-RNEWAMT(1) DS-RNEWUNIT(1).
      *     MOVE 0 TO DS-PAIDAMT(1) DS-PAIDUNIT(1).
      *     MOVE 0 TO DS-TODLAMT(1) DS-TODLUNIT(1).

      *     MOVE AGEING-DATE TO DS-DELUNEARN-DATE.

      ** CLEAR DLDISC MTD VOLUME
      *     MOVE 0 TO DS-DLDISC-VOL(1).

      ** PROCEEDS
      *     MOVE 0 TO DS-PROCEEDS(1).

      **- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      ** CLEAR MTD VOL UNEARNED:
      **-    -     -    -    -    -    -    -    -    -    -
      **    CLEAR CURRENT & ELM NET UNEARNED, WHICH WILL BE REGENERATED:
      **-    -     -    -    -    -    -    -    -    -    -
      **- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *     PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 10
      *       MOVE 0 TO DS-UNEARNED-AMT(1 DSVOL SUB)
      *       MOVE 0 TO DS-UNEARNED-AMT(1 DSNET SUB)
      *                 DS-UNEARNED-AMT(2 DSNET SUB)
      *     END-PERFORM.


      **- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      ** CLEAR CURRENT NET REPO WHICH WILL BE REGENERATED (BUT SKIP
      ** REPO BUCKET 2, CAN'T REGENERATE REDEEMED.....
      **- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *     PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 1
      *         MOVE 0 TO DS-NO-REPO(1 1 SUB)
      *                   DS-AMT-REPO(1 1 SUB)
      *     END-PERFORM.
      *     PERFORM VARYING SUB FROM 3 BY 1 UNTIL SUB > 5
      *         MOVE 0 TO DS-NO-REPO(1 1 SUB)
      *                   DS-AMT-REPO(1 1 SUB)
      *     END-PERFORM.

      **- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      ** CLEAR MTD VOL REPO:
      **- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *     PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 5
      *         MOVE 0 TO DS-NO-REPO(1 2 SUB)
      *                   DS-AMT-REPO(1 2 SUB)
      *     END-PERFORM.


      **- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      ** SET END OF LAST MONTH, DELINQUENCY AND CLEAR
      ** CURRENT & BEGIN OF MONTH:
      *     PERFORM VARYING SUB3 FROM 1 BY 1 UNTIL SUB3 > 10
      *                AFTER SUB2 FROM 1 BY 1 UNTIL SUB2 > 2
      *         MOVE DS-UNITS(1 SUB2 SUB3)
      *                               TO DS-UNITS(3 SUB2 SUB3)
      *         MOVE DS-OUTBAL(1 SUB2 SUB3)
      *                               TO DS-OUTBAL(3 SUB2 SUB3)
      *         MOVE DS-UNITS-BNKRPT(1 SUB2 SUB3)
      *                               TO DS-UNITS-BNKRPT(3 SUB2 SUB3)
      *         MOVE DS-OUTBAL-BNKRPT(1 SUB2 SUB3)
      *                               TO DS-OUTBAL-BNKRPT(3 SUB2 SUB3)

      *         MOVE 0 TO DS-UNITS(1 SUB2 SUB3)
      *                   DS-UNITS(2 SUB2 SUB3)

      *                   DS-OUTBAL(1 SUB2 SUB3)
      *                   DS-OUTBAL(2 SUB2 SUB3)

      *                   DS-UNITS-BNKRPT(1 SUB2 SUB3)
      *                   DS-UNITS-BNKRPT(2 SUB2 SUB3)

      *                   DS-OUTBAL-BNKRPT(1 SUB2 SUB3)
      *                   DS-OUTBAL-BNKRPT(2 SUB2 SUB3)
      *     END-PERFORM.

      *     PERFORM REWRITE-DS1-FILE.

      ** GET ORIGINAL EOM RECORD BACK IN
      *     MOVE HOLD-DS1-KEY TO DS1-KEY.

      *     MOVE DS-BRANCH      TO QDS1-WBEG-BRANCH
      *                            QDS1-WEND-BRANCH.
      *     MOVE DS-DATE-CCYYMM TO QDS1-WBEG-DATE-CCYYMM
      *                            QDS1-WEND-DATE-CCYYMM.

      *     MOVE DS-NUM         TO QDS1-WBEG-NUM.
      *     MOVE DS-CLASS       TO QDS1-WBEG-CLASS.

      *     MOVE ALL "9"       TO QDS1-WEND-NUM
      *                           QDS1-WEND-CLASS.

      *     PERFORM START-DS1-FILE.

      *     PERFORM READ-DS1-FILE-NEXT.

      ** THE NEXT READ-NEXT WILL GET NEXT RECORD
      *     GO TO DEALER-EOM-READ-NEXT.

      * EXIT-UPDATE-DS.
      *     PERFORM CLOSE-DS1-FILE.
      *     MOVE "DEALER STATISTICS MTD CLEAR COMPLETED"
      *        TO ERRLOGW-EOM-MSG.
      *     PERFORM LOG-ERROR.

      **********************************************
      * CREATE-NEW-MONTHS-DS SECTION.                       *> S35Q-104
      **********************************************
      ** STILL HAVE LAST MONTHS RECORD IN THE DS-REC
      ** FILL IN NEW DATE AND WRITE NEW RECORD FOR NEXT MONTH
      *     MOVE NEXT-MO-SETDS-DATE-CCYYMM TO DS-DATE-CCYYMM.
      *     PERFORM WRITE-DS1-FILE.
      *     IF IO-BAD
      *        PERFORM REWRITE-DS1-FILE.

      **********************************************************************
      *   UPDATE LN EOM
      *  DOES NOT ACTUALLY UPDATE LNFILE.  WRITES OUTPUT FILE FOR SUCCEEDING
      *   PROCESS TO UPDATE LNFILE.
      **********************************************************************
       UPDATE-LN-EOM SECTION.

           PERFORM OPEN-LN3-FILE.
      *    PERFORM OPEN-BY1-FILE.
           PERFORM OPEN-VBY1-FILE.
      *    PERFORM OPEN-DS1-FILE.                          *> S35Q-104

           MOVE "PROCESSING BRANCH #" TO PROGRESS-1.
           MOVE BR-NO TO PROGRESS-BRNO.
           MOVE ": LOAN CONTRACTUAL & RECENCY UPDATE"
                                      TO PROGRESS-2.
           PERFORM SEND-LEGEND.

           MOVE BR-NO TO LN-OWNBR  QLN3-WBEG-OWNBR.
      *     MOVE LN-PATH-OWNBR TO LN-OWNBR  QLN3-WBEG-OWNBR.
      *     MOVE 0             TO LN-ACCTNO QLN1-WBEG-ACCTNO.
      *     MOVE ALL "9"       TO           QLN1-WEND-ACCTNO.

           PERFORM START-LN3-FILE.  *> SSP LNFILE
 
      * PROCESS EXPECTS LNFILE IN LN-CLASS SEQUENCE
      *  FOR PERFORMANCE.  PROCESS OK IF NOT IN LN-CLASS SEQUENCE.
       UPDATE-LN-EOM-NEXT.
           PERFORM READ-LN3-FILE-NEXT.   *> SSP LNFILE
           IF IO-BAD                     *> OR (LN-OWNBR NOT = LN-PATH-OWNBR)
              IF WS-PROC-CLASS > 0
      *          PERFORM REWRITE-BY1-FILE
                 MOVE BY1-KEY TO VBY1-KEY
                 MOVE BY-REC  TO VBY-BY-REC
                 PERFORM REWRITE-VBY1-FILE
              END-IF
      *       IF WS-PROC-DLNO > 0                           *> S35Q-104
      *          PERFORM REWRITE-DS1-FILE
      *       END-IF
              GO TO EXIT-UPDATE-LN-EOM.


      * FOLLOWING ALSO CHECKED IN STORED PROCEDURE
           IF LN-OWNBR < BEG-BRANCH 
      *       UNLOCK LN-FILE
              GO TO UPDATE-LN-EOM-NEXT
           ELSE
           IF LN-OWNBR > END-BRANCH 
              GO TO EXIT-UPDATE-LN-EOM.

      * FOLLOWING ALSO CHECKED IN STORED PROCEDURE
      *  POCD - **  IS A DELETED CONTRACT
           IF ( LN-POCD-NONLOAN )
      *       UNLOCK LN-FILE
              GO TO UPDATE-LN-EOM-NEXT.


      * ON LN-CLASS BREAKS, UPDATE PREV BY REC, GET NEW BY REC
           IF LN-CLASS NOT = WS-PROC-CLASS
              IF WS-PROC-CLASS > 0 
      *          PERFORM REWRITE-BY1-FILE
                 MOVE BY1-KEY TO VBY1-KEY
                 MOVE BY-REC  TO VBY-BY-REC
                 PERFORM REWRITE-VBY1-FILE
              END-IF
              MOVE NEXT-MO-BY-DATE-CCYYMM TO BY-DATE-CCYYMM
              MOVE LN-OWNBR               TO BY-BRNO
              MOVE LN-CLASS               TO BY-CLASS
      *       PERFORM READ-BY1-FILE
              MOVE BY1-KEY                TO VBY1-KEY
              PERFORM READ-VBY1-FILE
              IF IO-BAD
                 PERFORM CREATE-BY1-REC
                 IF IO-BAD
                   GO TO IO-ERROR
                 END-IF
              END-IF
              MOVE VBY-BY-REC TO BY-REC
           END-IF.

      * ON LN-DLNO OR LN-CLASS CHANGES, UPDATE PREV DS REC, GET NEW DS REC
      *    IF LN-CLASS NOT = WS-PROC-CLASS OR              *> S35Q-104
      *       LN-DLNO  NOT = WS-PROC-DLNO 
      *       IF WS-PROC-DLNO > 0 
      *          PERFORM REWRITE-DS1-FILE
      *       END-IF
      *       MOVE NEXT-MO-SETDS-DATE-CCYYMM TO DS-DATE-CCYYMM
      *       MOVE LN-OWNBR           TO DS-BRANCH
      *       MOVE LN-DLNO            TO DS-DLNO
      *       MOVE LN-CLASS           TO DS-CLASS
      *       PERFORM READ-DS1-FILE
      *       IF IO-BAD
      *          PERFORM CREATE-DS1-REC
      *          IF IO-BAD
      *             GO TO IO-ERROR
      *          END-IF
      *       END-IF
      *    END-IF.

           MOVE LN-CLASS TO WS-PROC-CLASS.
      *    MOVE LN-DLNO  TO WS-PROC-DLNO.                   *> S35Q-104

           MOVE LN-ORGST    TO SP-ORGST.
           MOVE LN-SPRCLASS TO SP-SPRCLASS.
           MOVE LN-SUBCLASS TO SP-SUBCLASS.
           MOVE LN-LAWCODE  TO SP-LAWCODE.
           IF LPWSSP-SP1-KEY NOT = SP1-KEY
      *       PERFORM READ-SP1-FILE
              PERFORM ARRAYSP-READ
              IF IO-BAD
                 MOVE "BAD READ SP - ACCT # &&&&&&&&" TO ERRLOGW-EOM-MSG
                 INSPECT ERRLOGW-EOM-MSG REPLACING FIRST "&&&&&&&&"
                                 BY LN-ACCTNO
                 PERFORM LOG-ERROR
      *          UNLOCK LN-FILE
                 GO TO UPDATE-LN-EOM-NEXT
              ELSE
                 MOVE SP1-KEY TO LPWSSP-SP1-KEY.

      *    -------------------------------------------------------------
      *    SETUP OUTBAL WORKER FOR UPDATE OF STATS
      *    -------------------------------------------------------------
           IF GP-DELINQ-WITH-O2 = "Y"
              MOVE LN-CURBAL TO SAVE-OUTBAL
           ELSE
              SUBTRACT LN-OT2BAL FROM LN-CURBAL GIVING SAVE-OUTBAL.

      * GET AGEING STATUS ON ACCOUNT FOR END OF MONTH:
           MOVE LAST-DAY-OF-THIS-MONTH TO AGEING-DATE.
           PERFORM COMPUTE-CONTRACTUAL.
           PERFORM COMPUTE-RECENCY.

      * ...........MUST ESTABLISH FULL AND MTD ON THESE ACCOUNTS.
           IF SP-RECFRMLA-PERCNT-PMT
              IF SP-RECPDTH = 0
                 MOVE .01 TO AGEING-WORK
              ELSE
                 COMPUTE AGEING-WORK ROUNDED =
                         LN-REGPYAMT * SP-RECPDTH * .01
              END-IF
              IF LN-MTD-PAYS-RECDEL >= AGEING-WORK
                 MOVE EOM-DATE TO DATE-YYYYMMDD
                 PERFORM CONVERT-YYYYMMDD-TO-YYYYMM
                 MOVE DATE-YYYYMM TO LN-PY-YYYYMM-RECDEL
              END-IF
           ELSE
              IF SP-BANKRULE = "F"
                  IF LN-UNITPER-CD = "S"
                     IF LN-MTD-PAYS-RECDEL >= (LN-REGPYAMT * 2)
                        MOVE EOM-DATE TO DATE-YYYYMMDD
                        PERFORM CONVERT-YYYYMMDD-TO-YYYYMM
                        MOVE DATE-YYYYMM TO LN-PY-YYYYMM-RECDEL
                     END-IF
                  END-IF
              ELSE
                 IF LN-MTD-PAYS-RECDEL >= LN-REGPYAMT
                    MOVE EOM-DATE TO DATE-YYYYMMDD
                    PERFORM CONVERT-YYYYMMDD-TO-YYYYMM
                    MOVE DATE-YYYYMM TO LN-PY-YYYYMM-RECDEL.

           MOVE 0 TO LN-MTD-PAYS-RECDEL.

      *..........................................................
      *    SET BYFILE FOR CURRENT & BEGIN OF MONTH:
      *..........................................................
           IF LN-CURBAL = 0 AND
              LN-INTBAL = 0 AND
              LN-LCBAL  = 0 AND
              LN-OTHBAL = 0 AND
              LN-OT2BAL = 0
                 GO TO UPDATE-LN-EOM-SKIP-SAVE.

      *    IF LN-PLCD NOT = " "
      *       GO TO UPDATE-LN-EOM-SKIP-SAVE.

      *       ------SAVE AGEING CATAGORIES BEFORE MONTH END:
              MOVE RDSUB       TO SAVE-RDSUB.
              MOVE RECDEL      TO SAVE-RECDEL.
              MOVE RDPOT30     TO SAVE-RDPOT30.
              MOVE RSUB        TO SAVE-RSUB.
              MOVE RECENCY     TO SAVE-RECENCY.
              MOVE CSUB        TO SAVE-CSUB.
              MOVE CONTRACTUAL TO SAVE-CONTRACTUAL.
              MOVE CPOT30      TO SAVE-CPOT30.
              MOVE PSUB        TO SAVE-PSUB.

      *       ------GET AGEING CATAGORIES AFTER MONTH END:
              MOVE FIRST-DAY-OF-NEXT-MONTH TO AGEING-DATE.
              PERFORM COMPUTE-CONTRACTUAL.
              IF CPOT30 NOT = " "
                 MOVE CPOT30 TO LN-BMO-CON-STATUS
              ELSE
                 IF CSUB = 10
                    MOVE 0 TO SUBXN
                 ELSE
                    MOVE CSUB TO SUBXN
                 END-IF
                 MOVE SUBX TO LN-BMO-CON-STATUS.

              PERFORM COMPUTE-RECENCY.
              IF RDPOT30 NOT = " "
                 MOVE RDPOT30 TO LN-BMO-REC-STATUS
              ELSE
                 IF RDSUB = 10
                    MOVE 0 TO SUBXN
                 ELSE
                    MOVE RDSUB TO SUBXN
                 END-IF
                 MOVE SUBX TO LN-BMO-REC-STATUS.

              MOVE +1 TO ONE.

              IF GP-UNEARNED-STATS = "Y"
                 PERFORM COMPUTE-EARNINGS
              END-IF.

              PERFORM UPDATE-BYFILE-CUR-BMO.
      *       PERFORM UPDATE-DSFILE-CUR-BMO.               *> S35Q-104

              MOVE LAST-DAY-OF-THIS-MONTH TO AGEING-DATE.

      *          ------RESTORE AGEING CATAGORIES BEFORE MONTH END:
              MOVE SAVE-RDSUB       TO RDSUB.
              MOVE SAVE-RECDEL      TO RECDEL.
              MOVE SAVE-RDPOT30     TO RDPOT30.
              MOVE SAVE-RSUB        TO RSUB.
              MOVE SAVE-RECENCY     TO RECENCY.
              MOVE SAVE-CSUB        TO CSUB.
              MOVE SAVE-CONTRACTUAL TO CONTRACTUAL.
              MOVE SAVE-CPOT30      TO CPOT30.
              MOVE SAVE-PSUB        TO PSUB.
      *..........................................................

       UPDATE-LN-EOM-SKIP-SAVE.

           MOVE 0 TO LN-MTD-PRIN LN-MTD-LC LN-MTD-INT
                                 LN-MTD-OT LN-MTD-EX.
           MOVE LN-CURBAL TO LN-MONTHEND-BAL.

      * SKIP ACCOUNTS THAT ARE NOT DUE FOR A PAYMENT YET?
           IF LN-1STPYDATE > EOM-DATE
              GO TO REWRITE-LOAN-FILE.

      * PROCESS ALL CURRENT ACCOUNTS WHICH ARE DUE:
           IF NOT ( LN-CURBAL = 0 AND
                    LN-INTBAL = 0 AND
                    LN-LCBAL  = 0 AND
                    LN-OTHBAL = 0 AND
                    LN-OT2BAL = 0 )
              PERFORM TEST-CLEAR-DEFMNTS
              PERFORM PROFILE-UPDATE
              GO TO REWRITE-LOAN-FILE.

      * PROCESS FOR THE LAST TIME ACCOUNTS THAT PAID OFF
      * THIS MONTH: (FINAL ZERO IN THE FIRST POSITION OF CONTRACTUAL PROFILE)
           MOVE LN-DATE-PAID-LAST TO NUM-DATE.
           IF NUM-MO = EOM-MO AND NUM-YR = EOM-YR
              PERFORM PROFILE-UPDATE.

       REWRITE-LOAN-FILE.

      *     PERFORM REWRITE-LN3-FILE.
      * SINCE PERFORM REWRITE-LN3-FILE IS NOT PERFORMED (LN UPDATED IN SUCCEEDING
      *    PROCESS, THE FOLLOWING IS NOT CHECK AND DEEMED NOT NEEDED PER JAY.
      *    EOMALL NEVER MOVED A VALUE TO LN-BI-FG (LEFT AS SPACE).  
      *        IF LN-BI-FG = "Y"
      *           MOVE "R" TO STAT
      *           PERFORM UPDATE-BI-FILE.

           IF WS-LNOUT-CTR = 100000
               PERFORM CLOSE-LNDATA-FILE
               ADD 1 TO WS-LNSEQ-CTR
               STRING WS-BR-GR-TEXT, "-", WS-LNSEQ-CTR-RDF 
                 DELIMITED BY "  " INTO LNDATA-PATH-BEGBRNO 
               PERFORM OPEN-LNDATA-FILE-OUTPUT
               MOVE 0 TO WS-LNOUT-CTR
               ADD 1 TO SAVED-OUT-FILES-CTR 
               MOVE LNDATA-OUT-PATH TO 
                    SAVED-FILES-ARRAY(SAVED-OUT-FILES-CTR).

           PERFORM CREATE-LN-UPDATE-REC.
           PERFORM WRITE-LNDATA-FILE.
           ADD 1 TO WS-LNOUT-CTR
                    TOT-BRANCH-OUT-RECS-CTR
                    BRANCH-OUT-RECS-CTR.

           GO TO UPDATE-LN-EOM-NEXT.

       EXIT-UPDATE-LN-EOM.
           PERFORM CLOSE-LN3-FILE.
      *    PERFORM CLOSE-BY1-FILE.
      *    PERFORM CLOSE-DS1-FILE.                          *> S35Q-104

           MOVE "LOAN FILE MTD CLEAR COMPLETED" TO ERRLOGW-EOM-MSG.
           PERFORM LOG-ERROR.


      *************************************************
      *    BYFILE CURRENT AND BEGIN OF MONTH UPDATE
      *************************************************
      *** BY REC RETRIEVAL AND UPDATE PERFORMED IN LN READ LOOP.   ***
      *** THIS PARAGRAPH DOES NOT REWRITE BY    MB 2024-08-01      ***
      ***                                                          ***
       UPDATE-BYFILE-CUR-BMO SECTION.
      *     MOVE NEXT-MO-BY-DATE-CCYYMM TO BY-DATE-CCYYMM.
      *     MOVE LN-OWNBR               TO BY-BRNO.
      *     MOVE LN-CLASS               TO BY-CLASS.
      *     PERFORM READ-BY1-FILE.
      *     IF IO-BAD
      *        PERFORM CREATE-BY1-REC
      *        IF IO-BAD
      *           GO TO UPDATE-BY-CUR-BMO-EXIT.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      * ONLY & REGENERATE  CURRENT OUTSTANDING FOR P&L'S'S
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF LN-PLDATE NOT = 0
              PERFORM PL-BY1-UPD
      *        PERFORM REWRITE-BY1-FILE
              GO TO UPDATE-BY-CUR-BMO-EXIT.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    UPDATE UNEARNED STATISTICS NET (CUR AND ELM)     MERCURY #223
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF GP-UNEARNED-STATS = "Y"
              PERFORM VARYING BYSUB3 FROM 1 BY 1 UNTIL BYSUB3 > 6
                 ADD EARN-UNEARN(BYSUB3)
                            TO BY-UNEARNED-AMT(BYCUR BYNET BYSUB3)
                               BY-UNEARNED-AMT(BYELM BYNET BYSUB3)
              END-PERFORM.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      * ONLY UPDATE UNEARNED & REGEN CURRENT OUTSTANDING FOR REPO'S
           IF (LN-REPOCD = "X" OR "S")
              PERFORM REPO-BY1-UPD
      *        PERFORM REWRITE-BY1-FILE
              GO TO UPDATE-BY-CUR-BMO-EXIT.

      *--------------------------------------------------------------
      * UPDATE NORMAL DELINQUENCY:
      *--------------------------------------------------------------
      * SET CURRENT RECENCY:
           ADD ONE TO BY-UNITS(1 1 RDSUB).
           ADD SAVE-OUTBAL TO BY-OUTBAL(1 1 RDSUB).

      * SET BEGINNING OF MONTH RECENCY:
           ADD ONE TO BY-UNITS(2 1 RDSUB).
           ADD SAVE-OUTBAL TO BY-OUTBAL(2 1 RDSUB).

      * SET CURRENT CONTRACTUAL:
           ADD ONE TO BY-UNITS(1 2 CSUB).
           ADD SAVE-OUTBAL TO BY-OUTBAL(1 2 CSUB).

      * SET BEGINNING OF MONTH AMOUNTS CONTRACTUAL:
           ADD ONE TO BY-UNITS(2 2 CSUB).
           ADD SAVE-OUTBAL TO BY-OUTBAL(2 2 CSUB).

      *--------------------------------------------------------------
      * UPDATE BANKRUPT DELINQUENCY:
      *--------------------------------------------------------------
           IF LN-BNKRPTDATE NOT = 0
      *      SET CURRENT RECENCY:
              ADD ONE TO BY-UNITS-BNKRPT(1 1 RDSUB)
              ADD SAVE-OUTBAL TO BY-OUTBAL-BNKRPT(1 1 RDSUB)

      *      SET BEGINNING OF MONTH RECENCY:
              ADD ONE TO BY-UNITS-BNKRPT(2 1 RDSUB)
              ADD SAVE-OUTBAL TO BY-OUTBAL-BNKRPT(2 1 RDSUB)

      *      SET CURRENT CONTRACTUAL:
              ADD ONE TO BY-UNITS-BNKRPT(1 2 CSUB)
              ADD SAVE-OUTBAL TO BY-OUTBAL-BNKRPT(1 2 CSUB)

      *      SET BEGINNING OF MONTH AMOUNTS CONTRACTUAL:
              ADD ONE TO BY-UNITS-BNKRPT(2 2 CSUB)
              ADD SAVE-OUTBAL TO BY-OUTBAL-BNKRPT(2 2 CSUB).
      *--------------------------------------------------------------

      * REGENERATE CURRENT NET AMOUNTS:
           PERFORM GET-SRCD-SUB.
           ADD ONE TO BY-NO(1 1 SRCD).
           ADD LN-CURBAL TO BY-AMT(1 1 SRCD).
           ADD LN-OT2BAL TO BY-O2(1 1 SRCD).

      *     PERFORM REWRITE-BY1-FILE.

       UPDATE-BY-CUR-BMO-EXIT.
           EXIT.

      ****************************************************
      *    REPO ACCOUNT BY1 UPDATE
      ****************************************************
       REPO-BY1-UPD SECTION.

           IF LN-REPOCD = "X"
                MOVE 1 TO SRCD
             ELSE
                MOVE 3 TO SRCD.

           ADD ONE       TO BY-NO-REPO(BYCUR BYNET SRCD).
           ADD LN-CURBAL TO BY-AMT-REPO(BYCUR BYNET SRCD).
           ADD LN-OT2BAL TO BY-O2-REPO(BYCUR BYNET SRCD).

       REPO-BY1-UPD-EXIT.
           EXIT.

      ****************************************************
      *    P&L ACCOUNT BY1 UPDATE
      ****************************************************
       PL-BY1-UPD SECTION.

           PERFORM GET-SRCD-SUB.

           ADD ONE       TO BY-NO-PL(BYCUR BYNET SRCD).
           ADD LN-CURBAL TO BY-AMT-PL(BYCUR BYNET SRCD).
           ADD LN-OT2BAL TO BY-O2-PL(BYCUR BYNET SRCD).

           IF NOT (LN-REPOCD = "X" OR "S")
              GO TO PL-BY1-UPD-EXIT.

           IF LN-REPOCD = "X"
                MOVE 4 TO SRCD
             ELSE
                MOVE 5 TO SRCD.

           ADD ONE TO BY-NO-REPO(BYCUR BYNET SRCD).
           ADD LN-CURBAL TO BY-AMT-REPO(BYCUR BYNET SRCD).
           ADD LN-OT2BAL TO BY-O2-REPO(BYCUR BYNET SRCD).

       PL-BY1-UPD-EXIT.
           EXIT.

      *************************************************
      *    BYFILE CURRENT TO ELM           SYNC-P2-P3
      *************************************************
       UPDATE-BY-SYNC-P2-P3 SECTION.
      *    PERFORM OPEN-BY1-FILE.
           PERFORM OPEN-VBY1-FILE.

           MOVE NEXT-MO-BY-DATE-CCYYMM    TO BY-DATE-CCYYMM
                                             QBY1-WBEG-DATE-CCYYMM
                                             QBY1-WEND-DATE-CCYYMM.
           MOVE BR-NO                     TO BY-BRNO
                                             QBY1-WBEG-BRNO
                                             QBY1-WEND-BRNO.
           MOVE 0                         TO BY-CLASS
                                             QBY1-WBEG-CLASS.
           MOVE ALL "9"                   TO QBY1-WEND-CLASS.

      *    PERFORM START-BY1-FILE.
           MOVE BY1-KEY                   TO VBY1-KEY.
           PERFORM START-VBY1-FILE.

       UPDATE-BY-SYNC-P2-P3-NEXT.
      *    PERFORM READ-BY1-FILE-NEXT.
           PERFORM READ-VBY1-FILE-NEXT.

           IF ( IO-BAD )
              GO TO UPDATE-BY-SYNC-P2-P3-EXIT.

           MOVE VBY-BY-REC TO BY-REC.

           IF ( BY-BRNO        NOT = BR-NO                  ) OR
              ( BY-DATE-CCYYMM NOT = NEXT-MO-BY-DATE-CCYYMM )
              GO TO UPDATE-BY-SYNC-P2-P3-EXIT.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      * SET REGENERATED CURRENT NET TO ELM NET:
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 11
              MOVE BY-NO(1 1 SUB)  TO BY-NO(2 1 SUB)
              MOVE BY-AMT(1 1 SUB) TO BY-AMT(2 1 SUB)
              MOVE BY-O2(1 1 SUB)  TO BY-O2(2 1 SUB)
              MOVE BY-INT(1 1 SUB) TO BY-INT(2 1 SUB)
           END-PERFORM.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      * SET REGENERATED CURRENT NET REPO TO ELM NET REPO:
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 1
               MOVE BY-NO-REPO(1 1 SUB)  TO BY-NO-REPO(2 1 SUB)
               MOVE BY-AMT-REPO(1 1 SUB) TO BY-AMT-REPO(2 1 SUB)
               MOVE BY-O2-REPO(1 1 SUB)  TO BY-O2-REPO(2 1 SUB)
               MOVE BY-INT-REPO(1 1 SUB) TO BY-INT-REPO(2 1 SUB)
           END-PERFORM.

           PERFORM VARYING SUB FROM 3 BY 1 UNTIL SUB > 5
               MOVE BY-NO-REPO(1 1 SUB)  TO BY-NO-REPO(2 1 SUB)
               MOVE BY-AMT-REPO(1 1 SUB) TO BY-AMT-REPO(2 1 SUB)
               MOVE BY-O2-REPO(1 1 SUB)  TO BY-O2-REPO(2 1 SUB)
               MOVE BY-INT-REPO(1 1 SUB) TO BY-INT-REPO(2 1 SUB)
           END-PERFORM.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      * SET REGENERATED CURRENT NET P&L TO ELM NET P&L:
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 10
               MOVE BY-NO-PL(1 1 SUB)  TO BY-NO-PL(2 1 SUB)
               MOVE BY-AMT-PL(1 1 SUB) TO BY-AMT-PL(2 1 SUB)
               MOVE BY-O2-PL(1 1 SUB)  TO BY-O2-PL(2 1 SUB)
               MOVE BY-INT-PL(1 1 SUB) TO BY-INT-PL(2 1 SUB)
           END-PERFORM.


      *    PERFORM REWRITE-BY1-FILE.
           MOVE BY1-KEY TO VBY1-KEY.
           MOVE BY-REC  TO VBY-BY-REC.
           PERFORM REWRITE-VBY1-FILE.
           GO TO UPDATE-BY-SYNC-P2-P3-NEXT.

       UPDATE-BY-SYNC-P2-P3-EXIT.
      *    PERFORM CLOSE-BY1-FILE.

      *****************************************************************
      *   UPDATE BY CURRENT UPEARNED INTO THIS MONTH'S RECORD
      *****************************************************************
       UPDATE-BY-CUR-UNEARNED SECTION.
      *    PERFORM OPEN-BY1-FILE.
           PERFORM OPEN-VBY1-FILE.
           MOVE "PROCESSING BRANCH #"          TO PROGRESS-1.
           MOVE BR-NO TO PROGRESS-BRNO.
           MOVE ": SETTING UNEARNED IN THIS MONTH'S BY-REC!"
                                               TO PROGRESS-2.
           PERFORM SEND-LEGEND.

           MOVE FIRST-DAY-OF-NEXT-MONTH TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-YYYYMM.
           MOVE DATE-YYYYMM TO BY-DATE-CCYYMM NEXT-MO-BY-DATE-CCYYMM
                               QBY1-WBEG-DATE-CCYYMM
                               QBY1-WEND-DATE-CCYYMM.

           MOVE BR-NO       TO BY-BRNO
                               QBY1-WBEG-BRNO
                               QBY1-WEND-BRNO.
           MOVE 0           TO BY-CLASS
                               QBY1-WBEG-CLASS.
           MOVE ALL "9"     TO QBY1-WEND-CLASS.

      *    PERFORM START-BY1-FILE.
           MOVE BY1-KEY     TO VBY1-KEY.
           PERFORM START-VBY1-FILE.

       UPDATE-BY-CUR-UNEARNED-NEXT.
      *    PERFORM READ-BY1-FILE-NEXT.
           PERFORM READ-VBY1-FILE-NEXT.

           IF ( IO-BAD )
              GO TO UPDATE-BY-CUR-UNEARNED-EXIT.

           MOVE VBY-BY-REC TO BY-REC.

           IF ( BY-BRNO        NOT = BR-NO                  ) OR
              ( BY-DATE-CCYYMM NOT = NEXT-MO-BY-DATE-CCYYMM )
              GO TO UPDATE-BY-CUR-UNEARNED-EXIT.

      * SAVE NEXT MONTH'S CURRENT UNEARNED:
           PERFORM VARYING BYSUB3 FROM 1 BY 1 UNTIL BYSUB3 > 10
              MOVE BY-UNEARNED-AMT(BYCUR BYNET BYSUB3)
                                      TO SAVE-UNEARNED(BYSUB3)
           END-PERFORM.

      * WHEN YOU FIND NEXT MONTH'S RECORD, WHICH WAS UPDATED
      * WITH CURRENT UNEARNED FIGURES. SET THOSE INTO
      * THIS END OF MONTH'S RECORD.

           MOVE BY1-KEY TO HOLD-BY1-KEY.
           MOVE THIS-MO-BY-DATE-CCYYMM TO BY-DATE-CCYYMM.
      *    PERFORM READ-BY1-FILE.
           MOVE BY1-KEY TO VBY1-KEY.
           PERFORM READ-VBY1-FILE.

           IF IO-BAD
              MOVE " " TO ERRLOGW-EOM-MSG
              STRING "BR: &&&&, THIS MONTH'S BY-FILE REC"
                     "WAS NOT UPDATED WITH CUR UNEARNED"
                 DELIMITED BY SIZE INTO ERRLOGW-EOM-MSG
              INSPECT ERRLOGW-EOM-MSG REPLACING FIRST "&&&&" BY BR-NO
              PERFORM LOG-ERROR
              GO TO UPDATE-BY-CUR-UNEARNED-RESTART.

           MOVE VBY-BY-REC TO BY-REC.

      * MOVE NEXT MONTH'S CURRENT UNEARNED INTO THIS MONTH'S RECORD:
           PERFORM VARYING BYSUB3 FROM 1 BY 1 UNTIL BYSUB3 > 10
              MOVE SAVE-UNEARNED(BYSUB3) TO
                                 BY-UNEARNED-AMT(BYCUR BYNET BYSUB3)
           END-PERFORM.

      *    PERFORM REWRITE-BY1-FILE.
           MOVE BY1-KEY TO VBY1-KEY.
           MOVE BY-REC  TO VBY-BY-REC.
           PERFORM REWRITE-VBY1-FILE.

      * GET ORIGINAL EOM RECORD BACK IN
       UPDATE-BY-CUR-UNEARNED-RESTART.
           MOVE HOLD-BY1-KEY TO BY1-KEY.

           MOVE BY-DATE-CCYYMM TO QBY1-WBEG-DATE-CCYYMM
                                  QBY1-WEND-DATE-CCYYMM.

           MOVE BY-BRNO        TO QBY1-WBEG-BRNO
                                  QBY1-WEND-BRNO.
           MOVE BY-CLASS       TO QBY1-WBEG-CLASS.
           MOVE ALL "9"        TO QBY1-WEND-CLASS.

      *    PERFORM START-BY1-FILE.
           MOVE BY1-KEY TO VBY1-KEY.
           PERFORM START-VBY1-FILE.

      *    PERFORM READ-BY1-FILE-NEXT.
           PERFORM READ-VBY1-FILE-NEXT.

      * THE NEXT READ-NEXT WILL GET NEXT RECORD
           GO TO UPDATE-BY-CUR-UNEARNED-NEXT.

       UPDATE-BY-CUR-UNEARNED-EXIT.
      *    PERFORM CLOSE-BY1-FILE.

      *************************************************
      *    DSFILE CURRENT AND BEGIN OF MONTH UPDATE
      *************************************************
      *** DS REC RETRIEVAL AND UPDATE PERFORMED IN LN READ LOOP.   ***
      *** THIS PARAGRAPH DOES NOT REWRITE DS    MB 2024-08-01      ***
      ***   
      * UPDATE-DSFILE-CUR-BMO SECTION.                      *> S35Q-104
      **     MOVE NEXT-MO-SETDS-DATE-CCYYMM TO DS-DATE-CCYYMM.
      **     MOVE LN-OWNBR           TO DS-BRANCH.
      **     MOVE LN-DLNO            TO DS-DLNO.
      **     MOVE LN-CLASS           TO DS-CLASS.
      **     PERFORM READ-DS1-FILE.
      **     IF IO-BAD
      **        PERFORM CREATE-DS1-REC
      **        IF IO-BAD
      **           GO TO UPDATE-DS-CUR-BMO-EXIT.

      **- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      ** ONLY & REGENERATE  CURRENT OUTSTANDING FOR P&L'S'S
      **- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *     IF LN-PLDATE NOT = 0
      *        PERFORM PL-DS1-UPD
      **        PERFORM REWRITE-DS1-FILE
      *        GO TO UPDATE-DS-CUR-BMO-EXIT.

      **- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      **    UPDATE UNEARNED STATISTICS NET (CUR AND ELM)    MERCURY #223
      **- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *     IF GP-UNEARNED-STATS = "Y"
      *        PERFORM VARYING DSSUB3 FROM 1 BY 1 UNTIL DSSUB3 > 6
      *           ADD EARN-UNEARN(DSSUB3)
      *                           TO DS-UNEARNED-AMT(1 DSNET DSSUB3)
      *                              DS-UNEARNED-AMT(2 DSNET DSSUB3)
      *        END-PERFORM.
      **- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

      **- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      ** ONLY UPDATE UNEARNED & REGEN CURRENT OUTSTANDING FOR REPO'S
      **- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *     IF (LN-REPOCD = "X" OR "S")
      *        PERFORM REPO-DS1-UPD
      **        PERFORM REWRITE-DS1-FILE
      *        GO TO UPDATE-DS-CUR-BMO-EXIT.

      **--------------------------------------------------------------
      ** UPDATE NORMAL DELINQUENCY:
      **--------------------------------------------------------------
      ** SET CURRENT RECENCY:
      *     ADD ONE TO DS-UNITS(1 1 RDSUB).
      *     ADD SAVE-OUTBAL TO DS-OUTBAL(1 1 RDSUB).

      ** SET BEGINNING OF MONTH RECENCY:
      *     ADD ONE TO DS-UNITS(2 1 RDSUB).
      *     ADD SAVE-OUTBAL TO DS-OUTBAL(2 1 RDSUB).

      ** SET CURRENT CONTRACTUAL:
      *     ADD ONE TO DS-UNITS(1 2 CSUB).
      *     ADD SAVE-OUTBAL TO DS-OUTBAL(1 2 CSUB).

      ** SET BEGINNING OF MONTH CONTRACTUAL:
      *     ADD ONE TO DS-UNITS(2 2 CSUB).
      *     ADD SAVE-OUTBAL TO DS-OUTBAL(2 2 CSUB).

      **--------------------------------------------------------------
      ** UPDATE BANKRUPT DELINQUENCY:
      **--------------------------------------------------------------
      *     IF LN-BNKRPTDATE NOT = 0
      **     SET CURRENT RECENCY:
      *        ADD ONE TO DS-UNITS-BNKRPT(1 1 RDSUB)
      *        ADD SAVE-OUTBAL TO DS-OUTBAL-BNKRPT(1 1 RDSUB)

      **     SET BEGINNING OF MONTH RECENCY:
      *        ADD ONE TO DS-UNITS-BNKRPT(2 1 RDSUB)
      *        ADD SAVE-OUTBAL TO DS-OUTBAL-BNKRPT(2 1 RDSUB)

      **     SET CURRENT CONTRACTUAL:
      *        ADD ONE TO DS-UNITS-BNKRPT(1 2 CSUB)
      *        ADD SAVE-OUTBAL TO DS-OUTBAL-BNKRPT(1 2 CSUB)

      **     SET BEGINNING OF MONTH CONTRACTUAL:
      *        ADD ONE TO DS-UNITS-BNKRPT(2 2 CSUB)
      *        ADD SAVE-OUTBAL TO DS-OUTBAL-BNKRPT(2 2 CSUB).
      **--------------------------------------------------------------

      ** REGENERATE CURRENT NET AMOUNTS:
      *     ADD ONE TO DS-OUTSUNIT(1).
      *     ADD LN-CURBAL TO DS-OUTSAMT(1).
      **----FXSTAT DOES NOT INCLUDE O2 SO I SKIPPED IT HERE ALSO
      **--->ADD LN-OT2BAL TO DS-OUTSAMT(1).

      **     PERFORM REWRITE-DS1-FILE.

      * UPDATE-DS-CUR-BMO-EXIT.
      *     EXIT.

      ****************************************************
      *    REPO ACCOUNT DS1 UPDATE
      ****************************************************
      * REPO-DS1-UPD SECTION.                               *> S35Q-104

      *     IF LN-REPOCD = "X"
      *        MOVE 1 TO SRCD
      *     ELSE
      *        MOVE 3 TO SRCD.

      *     ADD ONE       TO DS-NO-REPO(DSCUR DSNET SRCD).
      *     ADD LN-CURBAL TO DS-AMT-REPO(DSCUR DSNET SRCD).

      * REPO-DS1-UPD-EXIT.
      *     EXIT.

      ****************************************************
      *    P&L ACCOUNT DS1 UPDATE
      ****************************************************
      * PL-DS1-UPD SECTION.                                 *> S35Q-104

      ** UPDATE P&L ACTIVITY:
      *     ADD ONE       TO DS-PLOSUNIT(1).
      *     ADD LN-CURBAL TO DS-PLOSAMT(1).

      *     IF NOT (LN-REPOCD = "X" OR "S")
      *        GO TO PL-DS1-UPD-EXIT.

      *     IF LN-REPOCD = "X"
      *          MOVE 4 TO SRCD
      *       ELSE
      *          MOVE 5 TO SRCD.

      *     ADD 1 TO DS-NO-REPO(DSCUR DSNET SRCD).
      *     ADD LN-CURBAL TO DS-AMT-REPO(DSCUR DSNET SRCD).

      * PL-DS1-UPD-EXIT.
      *     EXIT.

      *************************************************
      *    DSFILE CURRENT TO ELM           SYNC-P2-P3
      *************************************************
      * UPDATE-DS-SYNC-P2-P3 SECTION.                      *> S35Q-104

      *     PERFORM OPEN-DS1-FILE.

      *     MOVE NEXT-MO-SETDS-DATE-CCYYMM TO DS-DATE-CCYYMM
      *                                       QDS1-WBEG-DATE-CCYYMM
      *                                       QDS1-WEND-DATE-CCYYMM.
      *     MOVE BR-NO         TO DS-BRANCH
      *                           QDS1-WBEG-BRANCH
      *                           QDS1-WEND-BRANCH.

      *     MOVE 0             TO DS-DLNO DS-CLASS
      *                           QDS1-WBEG-NUM
      *                           QDS1-WBEG-CLASS.

      *     MOVE ALL "9"       TO QDS1-WEND-NUM
      *                           QDS1-WEND-CLASS.

      *     PERFORM START-DS1-FILE.
      * UPDATE-DS-SYNC-P2-P3-NEXT.
      *     PERFORM READ-DS1-FILE-NEXT.
      *     IF ( IO-BAD                                         ) OR
      *        ( DS-BRANCH      NOT = BR-NO                     ) OR
      *        ( DS-DATE-CCYYMM NOT = NEXT-MO-SETDS-DATE-CCYYMM )
      *        GO TO UPDATE-DS-SYNC-P2-P3-EXIT.

      **- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      ** SET REGENERATED CURRENT NET TO ELM NET:
      **- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *     MOVE DS-OUTSUNIT(1) TO DS-OUTSUNIT(2).
      *     MOVE DS-OUTSAMT(1)  TO DS-OUTSAMT(2).
      *     MOVE DS-PLOSUNIT(1) TO DS-PLOSUNIT(2).
      *     MOVE DS-PLOSAMT(1)  TO DS-PLOSAMT(2).

      **- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      ** SET REGENERATED CURRENT NET REPO TO ELM NET REPO:
      **- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *     PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 1
      *         MOVE DS-NO-REPO(1 1 SUB)  TO DS-NO-REPO(2 1 SUB)
      *         MOVE DS-AMT-REPO(1 1 SUB) TO DS-AMT-REPO(2 1 SUB)
      *     END-PERFORM.

      *     PERFORM VARYING SUB FROM 3 BY 1 UNTIL SUB > 5
      *         MOVE DS-NO-REPO(1 1 SUB)  TO DS-NO-REPO(2 1 SUB)
      *         MOVE DS-AMT-REPO(1 1 SUB) TO DS-AMT-REPO(2 1 SUB)
      *     END-PERFORM.

      *     PERFORM REWRITE-DS1-FILE.
      *     GO TO UPDATE-DS-SYNC-P2-P3-NEXT.

      * UPDATE-DS-SYNC-P2-P3-EXIT.
      *     PERFORM CLOSE-DS1-FILE.

      *****************************************************************
      *   UPDATE DS CURRENT UPEARNED INTO THIS MONTH'S RECORD
      *****************************************************************
      * UPDATE-DS-CUR-UNEARNED SECTION.                    *> S35Q-104

      *     PERFORM OPEN-DS1-FILE.

      *     MOVE "PROCESSING BRANCH #"          TO PROGRESS-1.
      *     MOVE BR-NO TO PROGRESS-BRNO.
      *     MOVE ": SETTING UNEARNED IN THIS MONTH'S DS-REC!"
      *                                         TO PROGRESS-2.
      *     PERFORM SEND-LEGEND.

      *     MOVE FIRST-DAY-OF-NEXT-MONTH TO DATE-YYYYMMDD.
      *     PERFORM CONVERT-YYYYMMDD-TO-YYYYMM.
      *     MOVE DATE-YYYYMM TO DS-DATE-CCYYMM NEXT-MO-SETDS-DATE-CCYYMM
      *                         QDS1-WBEG-DATE-CCYYMM
      *                         QDS1-WEND-DATE-CCYYMM.

      *     MOVE BR-NO       TO DS-BRANCH
      *                         QDS1-WBEG-BRANCH
      *                         QDS1-WEND-BRANCH.

      *     MOVE 0             TO DS-DLNO DS-CLASS
      *                           QDS1-WBEG-NUM
      *                           QDS1-WBEG-CLASS.

      *     MOVE ALL "9"       TO QDS1-WEND-NUM
      *                           QDS1-WEND-CLASS.

      *     PERFORM START-DS1-FILE.

      * UPDATE-DS-CUR-UNEARNED-NEXT.
      *     PERFORM READ-DS1-FILE-NEXT.
      *     IF ( IO-BAD                                         ) OR
      *        ( DS-BRANCH      NOT = BR-NO                     ) OR
      *        ( DS-DATE-CCYYMM NOT = NEXT-MO-SETDS-DATE-CCYYMM )
      *        GO TO UPDATE-DS-CUR-UNEARNED-EXIT.

      ** SAVE NEXT MONTH'S CURRENT UNEARNED:
      *     PERFORM VARYING DSSUB3 FROM 1 BY 1 UNTIL DSSUB3 > 10
      *        MOVE DS-UNEARNED-AMT(1 DSCUR DSSUB3)
      *                                TO SAVE-UNEARNED(DSSUB3)
      *     END-PERFORM.

      ** WHEN YOU FIND NEXT MONTH'S RECORD, WHICH WAS UPDATED
      ** WITH CURRENT UNEARNED FIGURES. SET THOSE INTO
      ** THIS END OF MONTH'S RECORD.

      *     MOVE DS1-KEY                   TO HOLD-DS1-KEY.
      *     MOVE THIS-MO-SETDS-DATE-CCYYMM TO DS-DATE-CCYYMM.
      *     PERFORM READ-DS1-FILE.
      *     IF IO-BAD
      *        MOVE " " TO ERRLOGW-EOM-MSG
      *        STRING "BR: &&&&, THIS MONTH'S DS-FILE REC"
      *               "WAS NOT UPDATED WITH CUR UNEARNED"
      *           DELIMITED BY SIZE INTO ERRLOGW-EOM-MSG
      *        INSPECT ERRLOGW-EOM-MSG REPLACING FIRST "&&&&" BY BR-NO
      *        PERFORM LOG-ERROR
      *        GO TO UPDATE-DS-CUR-UNEARNED-RESTART.

      ** MOVE NEXT MONTH'S CURRENT UNEARNED INTO THIS MONTH'S RECORD:
      *     PERFORM VARYING DSSUB3 FROM 1 BY 1 UNTIL DSSUB3 > 10
      *        MOVE SAVE-UNEARNED(DSSUB3) TO
      *                           DS-UNEARNED-AMT(1 DSCUR DSSUB3)
      *     END-PERFORM.

      *     PERFORM REWRITE-DS1-FILE.

      ** GET ORIGINAL EOM RECORD BACK IN
      * UPDATE-DS-CUR-UNEARNED-RESTART.
      *     MOVE HOLD-DS1-KEY TO DS1-KEY.
      *     MOVE DS-DATE-CCYYMM TO QDS1-WBEG-DATE-CCYYMM
      *                            QDS1-WEND-DATE-CCYYMM.
      *     MOVE DS-BRANCH      TO QDS1-WBEG-BRANCH
      *                            QDS1-WEND-BRANCH.
      *     MOVE DS-NUM         TO QDS1-WBEG-NUM.
      *     MOVE DS-CLASS       TO QDS1-WBEG-CLASS.

      *     MOVE ALL "9"       TO QDS1-WEND-NUM
      *                           QDS1-WEND-CLASS.

      *     PERFORM START-DS1-FILE.
      *     PERFORM READ-DS1-FILE-NEXT.

      ** THE NEXT READ-NEXT WILL GET NEXT RECORD
      *     GO TO UPDATE-DS-CUR-UNEARNED-NEXT.

      * UPDATE-DS-CUR-UNEARNED-EXIT.
      *     PERFORM CLOSE-DS1-FILE.


      *************************************************
      *    REMOVE OPEN FLAGS AND SPOOLS EO
      * CLOSE AND UPDATE TAGS ARE GONE, USING RCFILE
      *************************************************
       REMOVE-OP-AND-SPOOL SECTION.
           MOVE "PROCESSING BRANCH #" TO PROGRESS-1.
           MOVE BR-NO TO PROGRESS-BRNO.

           MOVE "REMOVING TAGS AND SPOOL DIRECTORIES" TO PROGRESS-2.
           PERFORM SEND-LEGEND.

           MOVE SPACES TO SYSTEM-BUF.

           MOVE "REL" TO GETENV-BUF.
           PERFORM GETENV-CALL.
           MOVE GETENV-BUF TO REMOVE-REL REMOVE-REL2.
           CALL "C$TOLOWER" USING REMOVE-EC, VALUE 3.
           CALL "C$TOLOWER" USING REMOVE-SHELL, VALUE 10.
           MOVE RM-TR-MONTH     TO REMOVE-MM.
           MOVE RM-TR-YEAR      TO REMOVE-YY.
           MOVE BR-MACHINE      TO REMOVE-MACHINE.
           MOVE BR-DATA-PATH    TO REMOVE-DATA.
           MOVE RM-EOD-MONTH    TO REMOVE-EOD-MM.
           MOVE RM-EOD-YEAR     TO REMOVE-EOD-YY.
           MOVE EXT-FILPATH-FIL TO REMOVE-FIL-PATH.
           MOVE REMOVE-CMD TO SYSTEM-BUF.
           PERFORM SYSTEM-CALL.

      *************************************************
      *    REMOVE ALL TY RECORDS USING GP-TR-RETAIN
      *************************************************
       REMOVE-TY-RECORDS SECTION.

           PERFORM OPEN-TY1-FILE.

           MOVE "PROCESSING BRANCH #" TO PROGRESS-1.
           MOVE BR-NO TO PROGRESS-BRNO.
           MOVE "REMOVING PRIOR DAILY STAT FILES" TO PROGRESS-2.
           PERFORM SEND-LEGEND.

           MOVE TY-PATH-OWNBR TO TY-BRNO
                                 QTY1-WBEG-BRNO
                                 QTY1-WEND-BRNO.

           MOVE 19000101      TO TY-DATE SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO QTY1-WBEG-DATE.
           MOVE RM-TR-DATE TO SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO QTY1-WEND-DATE.
           MOVE 0                   TO TY-CLASS
                                       QTY1-WBEG-CLASS.
           MOVE ALL "9"             TO QTY1-WEND-CLASS.

      *     PERFORM START-TY1-FILE.
      * NEXT-TY1-DELETE.
      *     PERFORM READ-TY1-FILE-NEXT.
      *     IF (IO-BAD) OR (TY-BRNO NOT = TY-PATH-OWNBR)
      *        GO TO REMOVE-TY-RECORDS-EXIT.
      *
      *     IF TY-DATE > RM-TR-DATE
      *        GO TO REMOVE-TY-RECORDS-EXIT.
      *
      *     PERFORM DELETE-TY1-FILE.
      *     GO TO NEXT-TY1-DELETE.

           PERFORM DELETE-TY1-FILE-MULT.

      * REMOVE-TY-RECORDS-EXIT.
           PERFORM CLOSE-TY1-FILE.

      *************************************************
      *    REMOVE ALL CA RECORDS FOR 1 MONTH AGO
      * 5/5/97 BLV FOR NOW, USE GP-TR-RETAIN FOR CA DELETE
      *************************************************
       REMOVE-CA-RECORDS SECTION.

           PERFORM OPEN-CA1-FILE.

           MOVE "PROCESSING BRANCH #" TO PROGRESS-1.
           MOVE BR-NO TO PROGRESS-BRNO.
           MOVE "REMOVING PRIOR CASH DRAWER FILES" TO PROGRESS-2.
           PERFORM SEND-LEGEND.

           MOVE BR-NO    TO CA-BRNO
                            QCA1-WBEG-BRNO
                            QCA1-WEND-BRNO.

           MOVE 19000101 TO CA-DATE SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD  TO QCA1-WBEG-DATE.
           MOVE RM-TR-DATE TO SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD  TO QCA1-WEND-DATE.

           MOVE 0        TO CA-TILLNO
                            QCA1-WBEG-TILLNO.
           MOVE ALL "9"  TO QCA1-WEND-TILLNO.

      *     PERFORM START-CA1-FILE.
      * NEXT-CA1-DELETE.
      *     PERFORM READ-CA1-FILE-NEXT.
      *     IF IO-BAD OR ( CA-BRNO NOT = CA-PATH-OWNBR )
      *        GO TO REMOVE-CA-RECORDS-EXIT.
      *
      *     IF CA-DATE > RM-TR-DATE
      *        GO TO REMOVE-CA-RECORDS-EXIT.
      *
      *    PERFORM DELETE-CA1-FILE.

           PERFORM DELETE-CA1-FILE-MULT.

      *     GO TO NEXT-CA1-DELETE.
      *
      * REMOVE-CA-RECORDS-EXIT.
           PERFORM CLOSE-CA1-FILE.

      *************************************************
      *    REMOVE ALL TR RECORDS USING GP-TR-RETAIN
      *************************************************
       REMOVE-TR-RECORDS SECTION.

           MOVE "PROCESSING BRANCH #" TO PROGRESS-1.
           MOVE BR-NO TO PROGRESS-BRNO.
           MOVE "REMOVING TRANSACTION FILES" TO PROGRESS-2.
           PERFORM SEND-LEGEND.

           PERFORM REMOVE-TRB-RECORDS.
           PERFORM REMOVE-TRC-RECORDS.
           PERFORM REMOVE-TRD-RECORDS.
           PERFORM REMOVE-TRI-RECORDS.
           PERFORM REMOVE-TRJ-RECORDS.
           PERFORM REMOVE-TRK-RECORDS.
           PERFORM REMOVE-TRL-RECORDS.
           PERFORM REMOVE-TRP-RECORDS.
           PERFORM REMOVE-TRS-RECORDS.
           PERFORM REMOVE-TRW-RECORDS.
           PERFORM REMOVE-TRX-RECORDS.

       REMOVE-TR-RECORDS-EXIT.
           EXIT.

      *****************************************
      * BRANCH TRANSFERS
      *****************************************
       REMOVE-TRX-RECORDS SECTION.
      *****************************************

           PERFORM OPEN-TRX1-FILE.

           MOVE TRX-PATH-OWNBR TO TX-BRNO
                                  QTRX1-WBEG-BRNO
                                  QTRX1-WEND-BRNO.

           MOVE 19000101       TO TX-DATE SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO QTRX1-WBEG-DATE.
           MOVE RM-TR-DATE     TO SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO QTRX1-WEND-DATE.
 
           MOVE 0              TO TX-CLASS TX-ACCTNO
                                  TX-LAWCODE TX-LINE
                                  QTRX1-WBEG-CLASS
                                  QTRX1-WBEG-ACCTNO
                                  QTRX1-WBEG-LAWCODE
                                  QTRX1-WBEG-LINE.
           MOVE ALL "9"        TO QTRX1-WEND-CLASS
                                  QTRX1-WEND-ACCTNO
                                  QTRX1-WEND-LAWCODE
                                  QTRX1-WEND-LINE.

      *     PERFORM START-TRX1-FILE.
      * NEXT-TRX1-DELETE.
      *     PERFORM READ-TRX1-FILE-NEXT.
      *     IF (IO-BAD) OR (TX-BRNO NOT = TRX-PATH-OWNBR)
      *        GO TO REMOVE-TRX-RECORDS-EXIT.
      *
      *     IF TX-DATE > RM-TR-DATE
      *        GO TO REMOVE-TRX-RECORDS-EXIT.
      *
      *     PERFORM DELETE-TRX1-FILE.
      *
      *     GO TO NEXT-TRX1-DELETE.

           PERFORM DELETE-TRX1-FILE-MULT.

      * REMOVE-TRX-RECORDS-EXIT.
           PERFORM CLOSE-TRX1-FILE.

      *****************************************
      * CASH BALANCING
      *****************************************
       REMOVE-TRW-RECORDS SECTION.
      *****************************************

           PERFORM OPEN-TRW1-FILE.

           MOVE TRW-PATH-OWNBR TO TW-BRNO
                                  QTRW1-WBEG-BRNO
                                  QTRW1-WEND-BRNO.

           MOVE 19000101       TO TW-DATE SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO QTRW1-WBEG-DATE.
           MOVE RM-TR-DATE     TO SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO QTRW1-WEND-DATE.
 
           MOVE 0              TO TW-TILLNO
                                  QTRW1-WBEG-TILLNO.
           MOVE ALL "9"        TO QTRW1-WEND-TILLNO.

      *     PERFORM START-TRW1-FILE.
      * NEXT-TRW1-DELETE.
      *     PERFORM READ-TRW1-FILE-NEXT.
      *     IF (IO-BAD) OR (TW-BRNO NOT = TRW-PATH-OWNBR)
      *        GO TO REMOVE-TRW-RECORDS-EXIT.
      *
      *     IF TW-DATE > RM-TR-DATE
      *        GO TO REMOVE-TRW-RECORDS-EXIT.
      * 
      *     PERFORM DELETE-TRW1-FILE.
      *
      *     GO TO NEXT-TRW1-DELETE.

           PERFORM DELETE-TRW1-FILE-MULT.

      * REMOVE-TRW-RECORDS-EXIT.
           PERFORM CLOSE-TRW1-FILE.

      *****************************************
      * SALES
      *****************************************
       REMOVE-TRS-RECORDS SECTION.
      *****************************************

           PERFORM OPEN-TRS1-FILE.

           MOVE TRS-PATH-OWNBR TO TS-BRNO
                                  QTRS1-WBEG-BRNO
                                  QTRS1-WEND-BRNO.

           MOVE 19000101       TO TS-DATE SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO QTRS1-WBEG-DATE.
           MOVE RM-TR-DATE     TO SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO QTRS1-WEND-DATE.
 
           MOVE 0              TO TS-CLASS TS-ACCTNO
                                  TS-LAWCODE TS-LINE
                                  QTRS1-WBEG-CLASS
                                  QTRS1-WBEG-ACCTNO
                                  QTRS1-WBEG-LAWCODE
                                  QTRS1-WBEG-LINE.
           MOVE ALL "9"        TO QTRS1-WEND-CLASS
                                  QTRS1-WEND-ACCTNO
                                  QTRS1-WEND-LAWCODE
                                  QTRS1-WEND-LINE.

      *     PERFORM START-TRS1-FILE.
      * NEXT-TRS1-DELETE.
      *     PERFORM READ-TRS1-FILE-NEXT.
      *     IF (IO-BAD) OR (TS-BRNO NOT = TRS-PATH-OWNBR)
      *        GO TO REMOVE-TRS-RECORDS-EXIT.
      *
      *     IF TS-DATE > RM-TR-DATE
      *        GO TO REMOVE-TRS-RECORDS-EXIT.
      *
      *     PERFORM DELETE-TRS1-FILE.
      *
      *     GO TO NEXT-TRS1-DELETE.

           PERFORM DELETE-TRS1-FILE-MULT.

      * REMOVE-TRS-RECORDS-EXIT.
           PERFORM CLOSE-TRS1-FILE.

      *****************************************
      * PAYMENTS
      *****************************************
       REMOVE-TRP-RECORDS SECTION.
      *****************************************

           PERFORM OPEN-TRP1-FILE.

           MOVE TRP-PATH-OWNBR TO TP-BRNO
                                  QTRP1-WBEG-BRNO
                                  QTRP1-WEND-BRNO.

           MOVE 19000101       TO TP-DATE SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO QTRP1-WBEG-DATE.
           MOVE RM-TR-DATE     TO SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO QTRP1-WEND-DATE.

           MOVE 0              TO TP-CLASS TP-ACCTNO
                                  TP-LINE
                                  QTRP1-WBEG-CLASS
                                  QTRP1-WBEG-ACCTNO
                                  QTRP1-WBEG-LINE.
           MOVE ALL "9"        TO QTRP1-WEND-CLASS
                                  QTRP1-WEND-ACCTNO
                                  QTRP1-WEND-LINE.

      *     PERFORM START-TRP1-FILE.
      * NEXT-TRP1-DELETE.
      *     PERFORM READ-TRP1-FILE-NEXT.
      *     IF (IO-BAD) OR (TP-BRNO NOT = TRP-PATH-OWNBR)
      *        GO TO REMOVE-TRP-RECORDS-EXIT.
      *
      *     IF TP-DATE > RM-TR-DATE
      *        GO TO REMOVE-TRP-RECORDS-EXIT.
      *
      *     PERFORM DELETE-TRP1-FILE.
      *
      *     GO TO NEXT-TRP1-DELETE.

           PERFORM DELETE-TRP1-FILE-MULT.

      * REMOVE-TRP-RECORDS-EXIT.
           PERFORM CLOSE-TRP1-FILE.

      *****************************************
      * LOANS
      *****************************************
       REMOVE-TRL-RECORDS SECTION.
      *****************************************

           PERFORM OPEN-TRL1-FILE.

           MOVE TRL-PATH-OWNBR TO TL-BRNO
                                  QTRL1-WBEG-BRNO
                                  QTRL1-WEND-BRNO.

           MOVE 19000101       TO TL-DATE SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO QTRL1-WBEG-DATE.
           MOVE RM-TR-DATE     TO SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO QTRL1-WEND-DATE.
 
           MOVE 0              TO TL-CLASS TL-ACCTNO
                                  TL-LAWCODE TL-LINE
                                  QTRL1-WBEG-CLASS
                                  QTRL1-WBEG-ACCTNO
                                  QTRL1-WBEG-LAWCODE
                                  QTRL1-WBEG-LINE.
           MOVE ALL "9"        TO QTRL1-WEND-CLASS
                                  QTRL1-WEND-ACCTNO
                                  QTRL1-WEND-LAWCODE
                                  QTRL1-WEND-LINE.

      *     PERFORM START-TRL1-FILE.
      * NEXT-TRL1-DELETE.
      *     PERFORM READ-TRL1-FILE-NEXT.
      *     IF (IO-BAD) OR (TL-BRNO NOT = TRL-PATH-OWNBR)
      *        GO TO REMOVE-TRL-RECORDS-EXIT.
      *
      *     IF TL-DATE > RM-TR-DATE
      *        GO TO REMOVE-TRL-RECORDS-EXIT.
      *
      *    PERFORM DELETE-TRL1-FILE.
      *
      *     GO TO NEXT-TRL1-DELETE.

           PERFORM DELETE-TRL1-FILE-MULT.

      * REMOVE-TRL-RECORDS-EXIT.
           PERFORM CLOSE-TRL1-FILE.

      *****************************************
      * CONVERSIONS
      *****************************************
       REMOVE-TRK-RECORDS SECTION.
      *****************************************

           PERFORM OPEN-TRK1-FILE.

           MOVE TRK-PATH-OWNBR TO TK-BRNO
                                  QTRK1-WBEG-BRNO
                                  QTRK1-WEND-BRNO.

           MOVE 19000101       TO TK-DATE SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO QTRK1-WBEG-DATE.
           MOVE RM-TR-DATE     TO SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO QTRK1-WEND-DATE.
 
           MOVE 0              TO TK-CLASS TK-ACCTNO
                                  TK-LAWCODE TK-LINE
                                  QTRK1-WBEG-CLASS
                                  QTRK1-WBEG-ACCTNO
                                  QTRK1-WBEG-LAWCODE
                                  QTRK1-WBEG-LINE.
           MOVE ALL "9"        TO QTRK1-WEND-CLASS
                                  QTRK1-WEND-ACCTNO
                                  QTRK1-WEND-LAWCODE
                                  QTRK1-WEND-LINE.

      *     PERFORM START-TRK1-FILE.
      * NEXT-TRK1-DELETE.
      *     PERFORM READ-TRK1-FILE-NEXT.
      *     IF (IO-BAD) OR (TK-BRNO NOT = TRK-PATH-OWNBR)
      *        GO TO REMOVE-TRK-RECORDS-EXIT.
      *
      *     IF TK-DATE > RM-TR-DATE
      *        GO TO REMOVE-TRK-RECORDS-EXIT.
      *
      *     PERFORM DELETE-TRK1-FILE.
      *
      *     GO TO NEXT-TRK1-DELETE.

           PERFORM DELETE-TRK1-FILE-MULT.

      * REMOVE-TRK-RECORDS-EXIT.
           PERFORM CLOSE-TRK1-FILE.

      *****************************************
      * CLASS TRANSFERS WITHIN A BRANCH
      *****************************************
       REMOVE-TRJ-RECORDS SECTION.
      *****************************************

           PERFORM OPEN-TRJ1-FILE.

           MOVE TRJ-PATH-OWNBR TO TJ-BRNO.
      * CAN NOT USE 0 IN DATABASE
           MOVE 19000101       TO TJ-DATE.
           MOVE 0              TO TJ-CLASS TJ-ACCTNO
                                  TJ-LAWCODE TJ-LINE.

           MOVE TRJ-PATH-OWNBR TO TJ-BRNO
                                  QTRJ1-WBEG-BRNO
                                  QTRJ1-WEND-BRNO.

           MOVE 19000101       TO TJ-DATE SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO QTRJ1-WBEG-DATE.
           MOVE RM-TR-DATE     TO SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO QTRJ1-WEND-DATE.
 
           MOVE 0              TO TJ-CLASS TJ-ACCTNO
                                  TJ-LAWCODE TJ-LINE
                                  QTRJ1-WBEG-CLASS
                                  QTRJ1-WBEG-ACCTNO
                                  QTRJ1-WBEG-LAWCODE
                                  QTRJ1-WBEG-LINE.
           MOVE ALL "9"        TO QTRJ1-WEND-CLASS
                                  QTRJ1-WEND-ACCTNO
                                  QTRJ1-WEND-LAWCODE
                                  QTRJ1-WEND-LINE.

      *     PERFORM START-TRJ1-FILE.
      * NEXT-TRJ1-DELETE.
      *     PERFORM READ-TRJ1-FILE-NEXT.
      *     IF (IO-BAD) OR (TJ-BRNO NOT = TRJ-PATH-OWNBR)
      *        GO TO REMOVE-TRJ-RECORDS-EXIT.
      *
      *     IF TJ-DATE > RM-TR-DATE
      *        GO TO REMOVE-TRJ-RECORDS-EXIT.
      *
      *     PERFORM DELETE-TRJ1-FILE.
      *     GO TO NEXT-TRJ1-DELETE.

           PERFORM DELETE-TRJ1-FILE-MULT.

      * REMOVE-TRJ-RECORDS-EXIT.
           PERFORM CLOSE-TRJ1-FILE.

      *****************************************
      * INSURANCE CLAIMS
      *****************************************
       REMOVE-TRI-RECORDS SECTION.
      *****************************************

           PERFORM OPEN-TRI1-FILE.

           MOVE TRI-PATH-OWNBR TO TI-BRNO
                                  QTRI1-WBEG-BRNO
                                  QTRI1-WEND-BRNO.

           MOVE 19000101       TO TI-KEY-DATE SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO QTRI1-WBEG-DATE.
           MOVE RM-TR-DATE     TO SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO QTRI1-WEND-DATE.
 
           MOVE 0              TO TI-SEQ
                                  QTRI1-WBEG-SEQ.
           MOVE ALL "9"        TO QTRI1-WEND-SEQ.

      *     PERFORM START-TRI1-FILE.
      * NEXT-TRI1-DELETE.
      *     PERFORM READ-TRI1-FILE-NEXT.
      *     IF (IO-BAD) OR (TI-BRNO NOT = TRI-PATH-OWNBR)
      *        GO TO REMOVE-TRI-RECORDS-EXIT.
      *
      *     IF TI-KEY-DATE > RM-TR-DATE
      *        GO TO REMOVE-TRI-RECORDS-EXIT.
      *
      *     PERFORM DELETE-TRI1-FILE.
      *     GO TO NEXT-TRI1-DELETE.

           PERFORM DELETE-TRI1-FILE-MULT. 

      * REMOVE-TRI-RECORDS-EXIT.
           PERFORM CLOSE-TRI1-FILE.

      *****************************************
      * DEPOSITS
      *****************************************
       REMOVE-TRD-RECORDS SECTION.
      *****************************************

           PERFORM OPEN-TRD1-FILE.

           MOVE TRD-PATH-OWNBR TO TD-BRNO
                                  QTRD1-WBEG-BRNO
                                  QTRD1-WEND-BRNO.

           MOVE 19000101       TO TD-DATE SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO QTRD1-WBEG-DATE.
           MOVE RM-TR-DATE     TO SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO QTRD1-WEND-DATE.
 
      *     PERFORM START-TRD1-FILE.
      * NEXT-TRD1-DELETE.
      *     PERFORM READ-TRD1-FILE-NEXT.
      *     IF (IO-BAD) OR (TD-BRNO NOT = TRD-PATH-OWNBR)
      *        GO TO REMOVE-TRD-RECORDS-EXIT.
      *
      *     IF TD-DATE > RM-TR-DATE
      *        GO TO REMOVE-TRD-RECORDS-EXIT.
      *
      *     PERFORM DELETE-TRD1-FILE.
      *     GO TO NEXT-TRD1-DELETE.

           PERFORM DELETE-TRD1-FILE-MULT.

      * REMOVE-TRD-RECORDS-EXIT.
           PERFORM CLOSE-TRD1-FILE.

      *****************************************
      * CASH/CHECK 
      *****************************************
       REMOVE-TRC-RECORDS SECTION.
      *****************************************

           PERFORM OPEN-TRC1-FILE.

           MOVE TRC-PATH-OWNBR TO TC-BRNO
                                  QTRC1-WBEG-BRNO
                                  QTRC1-WEND-BRNO.

           MOVE 19000101       TO TC-KEY-DATE SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO QTRC1-WBEG-KEY-DATE.
           MOVE RM-TR-DATE     TO SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO QTRC1-WEND-KEY-DATE.
 
           MOVE 0              TO TC-CKNO TC-CLASS TC-SEQ 
                                  QTRC1-WBEG-CKNO
                                  QTRC1-WBEG-CLASS
                                  QTRC1-WBEG-SEQ.
           MOVE ALL "9"        TO QTRC1-WEND-CKNO
                                  QTRC1-WEND-CLASS
                                  QTRC1-WEND-SEQ.

      *     PERFORM START-TRC1-FILE.
      * NEXT-TRC1-DELETE.
      *     PERFORM READ-TRC1-FILE-NEXT.
      *     IF (IO-BAD) OR (TC-BRNO NOT = TRC-PATH-OWNBR)
      *        GO TO REMOVE-TRC-RECORDS-EXIT.
      *
      *     IF TC-KEY-DATE > RM-TR-DATE
      *        GO TO REMOVE-TRC-RECORDS-EXIT.
      *  
      *     PERFORM DELETE-TRC1-FILE.
      *     GO TO NEXT-TRC1-DELETE.

           PERFORM DELETE-TRC1-FILE-MULT.

      * REMOVE-TRC-RECORDS-EXIT.
           PERFORM CLOSE-TRC1-FILE.

      *****************************************
      * BRANCH TRANSFERS
      *****************************************
       REMOVE-TRB-RECORDS SECTION.
      *****************************************

           PERFORM OPEN-TRB1-FILE.

           MOVE TRB-PATH-OWNBR TO TB-BRNO
                                  QTRB1-WBEG-BRNO
                                  QTRB1-WEND-BRNO.

           MOVE 19000101       TO TB-DATE SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO QTRB1-WBEG-DATE.
           MOVE RM-TR-DATE     TO SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO QTRB1-WEND-DATE.
 
           MOVE 0              TO TB-CLASS TB-ACCTNO
                                  TB-LAWCODE TB-LINE
                                  QTRB1-WBEG-CLASS
                                  QTRB1-WBEG-ACCTNO
                                  QTRB1-WBEG-LAWCODE
                                  QTRB1-WBEG-LINE.
           MOVE ALL "9"        TO QTRB1-WEND-CLASS
                                  QTRB1-WEND-ACCTNO
                                  QTRB1-WEND-LAWCODE
                                  QTRB1-WEND-LINE.

      *     PERFORM START-TRB1-FILE.
      * NEXT-TRB1-DELETE.
      *     PERFORM READ-TRB1-FILE-NEXT.
      *     IF (IO-BAD) OR (TB-BRNO NOT = TRB-PATH-OWNBR)
      *        GO TO REMOVE-TRB-RECORDS-EXIT.
      *
      *     IF TB-DATE > RM-TR-DATE
      *        GO TO REMOVE-TRB-RECORDS-EXIT.
      *
      *     PERFORM DELETE-TRB1-FILE.
      *     GO TO NEXT-TRB1-DELETE.

           PERFORM DELETE-TRB1-FILE-MULT.

      * REMOVE-TRB-RECORDS-EXIT.
           PERFORM CLOSE-TRB1-FILE.

      *****************************************************************
      *   UPDATE RC FILE
      *****************************************************************
       UPDATE-RC-STATUS SECTION.

      *    PERFORM CLOSE-RC2-FILE.
      *    PERFORM OPEN-RC1-FILE.

           PERFORM CLOSE-VRC2-FILE.
           PERFORM OPEN-VRC1-FILE.

      *     MOVE HOLD-RC-BRNO       TO RC-BRNO.
      *     MOVE HOLD-RC-TRANS-DATE TO RC-TRANS-DATE.
      *     PERFORM READ-RC1-FILE.

           MOVE HOLD-RC-BRNO       TO VRC-BRNO.
           MOVE HOLD-RC-TRANS-DATE TO VRC-TRANS-DATE.
           PERFORM READ-VRC1-FILE.
           IF IO-BAD
              GO TO IO-ERROR.

           MOVE VRC-RC-REC TO RC-REC.

           MOVE "I1" TO RC-STATUS.

      *     PERFORM SET-POSTDATE.
      *     MOVE POST-TIME TO RC-EOM-UPDATED-TIME.
      *     MOVE POST-DATE TO RC-EOM-UPDATED-DATE.

      *     PERFORM REWRITE-RC1-FILE.
      *     PERFORM CLOSE-RC1-FILE.

           MOVE RC2-KEY TO VRC2-KEY.
           MOVE RC-REC  TO VRC-RC-REC.
           PERFORM REWRITE-VRC1-FILE.
           PERFORM CLOSE-VRC1-FILE.

      *    PERFORM OPEN-RC2-FILE.
      *    MOVE HOLD-RC2-KEY         TO RC2-KEY.
      *    MOVE RC-STATUS            TO QRC2-WBEG-STATUS
      *                                 QRC2-WEND-STATUS.
      *    MOVE RC-BRNO              TO QRC2-WBEG-BRNO
      *    MOVE END-BRANCH           TO QRC2-WEND-BRNO.
      *    MOVE RC-TRANS-DATE        TO SQL-DATE-YYYYMMDD.
      *    PERFORM SQL-SET-DATE.
      *    MOVE SQL-DATE-YYYY-MM-DD  TO QRC2-WBEG-TRANS-DATE
      *                                 QRC2-WEND-TRANS-DATE.
      *    PERFORM START-RC2-FILE.

           MOVE HOLD-RC2-KEY  TO VRC2-KEY.
           PERFORM OPEN-VRC2-FILE.
           PERFORM START-VRC2-FILE.

       EXIT-UPDATE-RC.
           EXIT.

      *****************************************************************
      *
      *   I N I T I A L I Z A T I O N
      *
      *****************************************************************
       INITIALIZATION SECTION.

           PERFORM GET-GPENV.

           MOVE EOMALL-QUEUE-POS TO Q-POS.
           MOVE EOMALL-COPIES-POS TO C-POS.
           MOVE EOMALL-PRU-POS   TO P-POS.
           MOVE SPACES TO Q-BUF C-BUF P-BUF.

       COPY "LIBGB/EOINIT.CPY".

           PERFORM GET-TIME.
           MOVE TIME-EDIT TO STARTING-TIME.

      * SETUP TRUE LAST DAY OF THE MONTH END:
           MOVE EOM-DATE TO NDTE-DATE.
           MOVE 31       TO NDTE-DD.
           PERFORM NEW-DATE-CALCULATION.

           MOVE "S  A000AGEDATE." TO VDU.
           MOVE NDTE-DATE TO LAST-DAY-OF-THIS-MONTH
                             AGEING-DATE
                             DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO D-AGE-DATE.
           MOVE AGING-DATE-MESS TO VDUBUF.
           IF EXT-ROUTE-BUF = "00"
              PERFORM SCREENIO.

      * SETUP FIRST DAY OF THE NEXT MONTH:
           MOVE AGEING-DATE TO NDTE-DATE.
           MOVE 1           TO NDTE-HOLD.
           PERFORM INCREMENT-MONTHS.
           MOVE 1           TO NDTE-DD.

           MOVE NDTE-DATE TO FIRST-DAY-OF-NEXT-MONTH 
                             DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-YYYYMM.
           MOVE DATE-YYYYMM TO NEXT-MO-BY-DATE-CCYYMM
                               NEXT-MO-SETDS-DATE-CCYYMM.

      * SETUP MONTH/YEAR FOR TR REMOVAL:
           MOVE EOM-DATE TO NDTE-DATE.
           IF GP-TR-RETAIN-MONTHS < 2
              MOVE 2 TO GP-TR-RETAIN-MONTHS.
           COMPUTE NDTE-HOLD = GP-TR-RETAIN-MONTHS * -1.
           PERFORM INCREMENT-MONTHS.
           MOVE NDTE-DATE TO RM-TR-DATE.

      * SETUP MONTH/YEAR FOR EOD SPOOL REMOVAL:
           MOVE EOM-DATE TO NDTE-DATE.
           IF GP-EOD-RETAIN-MONTHS = 0
              MOVE 1 TO GP-EOD-RETAIN-MONTHS.
           COMPUTE NDTE-HOLD = GP-EOD-RETAIN-MONTHS * -1.
           PERFORM INCREMENT-MONTHS.
           MOVE NDTE-DATE TO RM-EOD-DATE.

           MOVE "LOGNAME" TO GETENV-BUF.
           PERFORM GETENV-CALL.
           MOVE GETENV-BUF TO SAVE-LOGNAME.

      * CALL MKTEMP TO GET RANDOM PROCESS-ID
           MOVE MKTEMP-DEFAULT TO MKTEMP-BUF.
           PERFORM MKTEMP-CALL.
           IF STAT NOT = "00"
              MOVE "CANNOT MAKE WORK FILE" TO MESS  
              PERFORM SEND-MESS
              GO TO ENDIT.

           MOVE MKTEMP-BUF TO ACCESS-BUF.
           PERFORM REMOVE-WORKFILE.

           MOVE EXT-FILPATH-BASE    TO OUTPUT-PATH-BASE.

           MOVE EXT-FILPATH-MACHINE TO OUTPUT-PATH-MACHINE.

           MOVE PROCESS-ID-BUF      TO BYADATA-PATH-PROC-ID
                                       BYBDATA-PATH-PROC-ID
                                       LNDATA-PATH-PROC-ID
                                       RCDATA-PATH-PROC-ID
                                       GBDATA-PATH-PROC-ID.

           MOVE EOMALL-OUTPUT-DIR TO STAT-PATH-DIR
                                     BYADATA-PATH-DIR
                                     BYBDATA-PATH-DIR
                                     RCDATA-PATH-DIR
                                     LNDATA-PATH-DIR
                                     GBDATA-PATH-DIR.

           IF GROUP-FLAG = "Y"
             MOVE ENT-GROUP TO WS-BR-GR-TEXT
           ELSE
             MOVE BEG-BRANCH TO WS-BR-GR-TEXT.

           STRING WS-BR-GR-TEXT, "-001" 
               DELIMITED BY "  " INTO BYADATA-PATH-BEGBRNO.
           STRING WS-BR-GR-TEXT, "-001" 
               DELIMITED BY "  " INTO BYBDATA-PATH-BEGBRNO.
           STRING WS-BR-GR-TEXT, "-001" 
               DELIMITED BY "  " INTO RCDATA-PATH-BEGBRNO.
           STRING WS-BR-GR-TEXT, "-001" 
               DELIMITED BY "  " INTO GBDATA-PATH-BEGBRNO.
           STRING WS-BR-GR-TEXT, "-001" 
               DELIMITED BY "  " INTO LNDATA-PATH-BEGBRNO.

           MOVE WS-BR-GR-TEXT      TO STAT-PATH-BEGBRNO.
           INSPECT STAT-PATH-BEGBRNO REPLACING ALL " " BY "X".
           MOVE EOM-YR            TO STAT-PATH-EOM-YY.
           MOVE EOM-MO            TO STAT-PATH-EOM-MM.
           MOVE EOM-DA            TO STAT-PATH-EOM-DD.
 
           MOVE EXT-CONAME-CONAME  TO H-NAME.
           MOVE EXT-CONAME-SYSDATE TO H-DATE.
           MOVE EXT-CONAME-USER-ID TO H-USERID.
           MOVE AGEING-DATE        TO H-AGEDATE.

           PERFORM OPEN-PR-FILE.
 
           IF EXT-ROUTE-BUF NOT = "00"
              GO TO BEGIN-EOM.

      * CHECK OPEN DAYS
           PERFORM STATUS-CHECK.
           IF CKSTAT-RETURN-STAT = "N"
              MOVE "RANGE INCLUDES BRANCHES NOT AT CORRECT STATUS!"
                 TO MESS  
              PERFORM SEND-MESS
              PERFORM RC-INQUIRY
           ELSE
              MOVE "ALL BRANCHES AT CORRECT STATUS" TO MESS
              PERFORM SEND-MESS.

       ACCEPT-IT.
           IF CKSTAT-RETURN-STAT = "N" MOVE
             "END-OF-MONTH CAN'T RUN UNTIL BRANCHES AT CORRECT STATUS!"
              TO MESS  
              PERFORM SEND-MESS
              GO TO ENDIT.

           MOVE SPACES       TO LEGEND.
           MOVE LEGEND       TO WR-BUF.
           MOVE WARNING-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

           MOVE "F1-CANCEL, F6-SCAN STATUSES, F7-EXIT, 'Y' TO ACCEPT"
                             TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "E  A000ACCEPT." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO ENDIT.
           IF VDUSTATUS = "10" GO TO EOINIT-SETPR.
           IF VDUSTATUS = "1U" GO TO EOINIT-ENTER-MENU.
           IF VDUSTATUS = "1<"
              PERFORM RC-INQUIRY
              GO TO ACCEPT-IT.
           IF VDUSTATUS NOT = "00" GO TO ACCEPT-IT.
           IF VDUBUF = "N" GO TO EOINIT-ENTER-MENU.
           IF VDUBUF NOT = "Y"
              GO TO ACCEPT-IT.

      * PERFORM TIMER-START FUNCTION; SET BEG TIME FIELD.
           MOVE ZEROES TO P-TIMER-DAYSEC-BEG.

           IF ( P-TIMER-TYPE-SET-VALID )
               MOVE P-TIMER-TYPE-SET TO P-TIMER-TYPE
           ELSE
      * REPORT OPTION
               MOVE "R" TO P-TIMER-TYPE. 

           MOVE "GB/TIMER" TO FORM-NAM.
           CALL FORM-PROGX USING P-TIMER-LINK-BUF.
           CANCEL FORM-PROGX.

       BEGIN-EOM.
           PERFORM OPEN-BR-FILE.
           PERFORM OPEN-SP1-FILE.

           PERFORM SQL-OPEN-UPDATE-CONNECTION.

           MOVE EOM-DATE TO EOM-RC-TRANS-DATE NUM-DATE.

           IF GP-CASH-METHOD-ONLY = "Y" AND GP-IB-ACCRUALS-DONE = "N"
              PERFORM CREATE-RCS-FOR-NO-ACCRUAL
              IF EXT-EOBUF-ERRCD = "X"
                 GO TO ENDIT.

      *     PERFORM OPEN-RC2-FILE.
      *    MOVE "G1"              TO RC-STATUS
      *                              QRC2-WBEG-STATUS
      *                              QRC2-WEND-STATUS.
      *    MOVE BEG-BRANCH        TO RC-BRNO
      *                              QRC2-WBEG-BRNO.
      *    MOVE END-BRANCH        TO QRC2-WEND-BRNO.
      *
      *    MOVE EOM-RC-TRANS-DATE TO RC-TRANS-DATE SQL-DATE-YYYYMMDD.
      *    PERFORM SQL-SET-DATE.
      *    MOVE SQL-DATE-YYYY-MM-DD  TO QRC2-WBEG-TRANS-DATE
      *                                 QRC2-WEND-TRANS-DATE.
      *     PERFORM START-RC2-FILE.

           PERFORM ARRAYSP-LOAD.

           IF GROUP-FLAG = "Y"
              PERFORM OPEN-GR1-FILE
              MOVE ENT-GROUP TO GR-ID
              PERFORM SEARCH-GR-INIT.

      * REMOVE WORK FILES IS ALREADY EXIST.
           PERFORM CLEAR-WK-FILES.

      * CREATE STAT FILE - ADD REC WITH "STARTED"
           MOVE "STARTED" TO STAT-REC-OUT.
           PERFORM UPDATE-STAT-FILE.

      * COPY SQL RCFILE TO TEMP VISION VRCFILE (JUST FOR REQUESTED EOM DATE)
           MOVE EOM-RC-TRANS-DATE TO RC-TRANS-DATE.
           PERFORM CREATE-VRC1-FILE.

      * COPY SQL BYFILE TO TEMP VISION VBYFILE (JUST FOR REQUESTED EOM DATE)
           MOVE LAST-DAY-OF-THIS-MONTH TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-YYYYMM.
           MOVE DATE-YYYYMM TO BY-DATE-CCYYMM.
           PERFORM CREATE-VBY1-FILE.

      *    VGBFILE WILL BE CREATED AS GB UPDATES OCCUR
      *    FOLLOWING JUST CREATES VISION FILE
           MOVE MKTEMP-DEFAULT TO MKTEMP-BUF.
           PERFORM MKTEMP-CALL.
           MOVE MKTEMP-BUF     TO VGB-PATH.
           PERFORM OPEN-VGB-FILE-OUTPUT.

           ADD 1 TO SAVED-OUT-FILES-CTR. 
           MOVE LNDATA-OUT-PATH TO 
                    SAVED-FILES-ARRAY(SAVED-OUT-FILES-CTR).
           PERFORM OPEN-LNDATA-FILE-OUTPUT.

      * DRIVE OFF TEMP VISION VRCFILE (NOT SQL!!)
           PERFORM OPEN-VRC2-FILE.
           MOVE "G1"              TO VRC-STATUS.
           MOVE BEG-BRANCH        TO VRC-BRNO.
           MOVE EOM-RC-TRANS-DATE TO VRC-TRANS-DATE.
           PERFORM START-VRC2-FILE.

           GO TO EXIT-INITIALIZE.

       ENDIT.
           MOVE "X" TO EXT-EOBUF-ERRCD.

       EXIT-INITIALIZE.
           EXIT.

      *****************************************************************
      *
      *     EO-EXEC-AUDIT SECTION.
      *
      *****************************************************************
       EO-EXEC-AUDIT SECTION.

           MOVE EXT-EOBUF-DATE2 TO EOM-DATE.
           PERFORM CHECK-EOM-DATE.
           IF SUB = 0
              MOVE "INVALID MONTH-END DATE!!!" 
               TO MESS ERRLOGW-EOM-MSG  
              PERFORM SEND-MESS
              PERFORM LOG-ERROR
              MOVE "X" TO EXT-EOBUF-ERRCD
              GO TO EO-EXEC-EXIT.

           PERFORM STATUS-CHECK.
           IF CKSTAT-RETURN-STAT = "N"
              MOVE "RANGE INCLUDES BRANCHES NOT AT CORRECT STATUS!"
                 TO MESS ERRLOGW-EOM-MSG  
              PERFORM SEND-MESS
              PERFORM LOG-ERROR
              MOVE "X" TO EXT-EOBUF-ERRCD
              GO TO END-PROGRAM
           ELSE
              MOVE "ALL BRANCHES AT CORRECT STATUS" TO MESS
                                                       ERRLOGW-EOM-MSG
              PERFORM SEND-MESS
              PERFORM LOG-ERROR.

           MOVE "N" TO WS-PGM-FAILED-SW.

       EO-EXEC-EXIT.
           EXIT.

      *****************************************************************
      *
      *    ENTER-PARAMETERS SECTION.
      *
      *****************************************************************
       ENTER-PARAMETERS SECTION.
      *----------------------------------------------------------------
       ENTER-PARM.

           MOVE SPACES TO WR-BUF.
           MOVE WARNING-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

           MOVE "Y" TO NOACCEPT-BUF. 

      *----------------------------------------------------------------

       BR-01.
           MOVE LEGEND-BR-01 TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "E  A000BR1." TO VDU.
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
              GO TO EOM-DT.
       BRANCH-IS-NUMERIC.
           MOVE "N" TO GROUP-FLAG.
           MOVE WGR-SRESULT TO BEG-BRANCH.
           MOVE "SN N040BR1." TO VDU.
           MOVE BEG-BRANCH TO VDUNUM0.
           PERFORM SCREENIO.
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

           GO TO EOM-DT.
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

      *----------------------------------------------------------------
       EOM-DT.
           IF EXT-ROUTE-BUF NOT = "00"
              GO TO EXIT-PARM.

           PERFORM GET-DEFAULT-DATE.

           MOVE LEGEND-EOM-DTE TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "ERNM060EOMDT."    TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO BR-01.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO EOM-DT.

           MOVE VDU-DATE-YYYYMMDD TO EOM-DATE.
           PERFORM CHECK-EOM-DATE.
           IF SUB = 0
              MOVE "YOU MUST ENTER A VALID MONTH-END DATE" TO MESS
              PERFORM SEND-MESS
              GO TO EOM-DT.

           MOVE SPACES TO LEGEND.
           PERFORM SEND-LEGEND.

           GO TO EXIT-PARM.

       EXIT-PARM.
           EXIT.

      *****************************************************************
      *
      *    DISPLAY-PARAMETERS SECTION.
      *
      *****************************************************************
       DISPLAY-PARAMETERS SECTION.
      *----------------------------------------------------------------
       DISPLAY-PARM.

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

           IF ( (BEG-BRANCH = 0) AND (END-BRANCH = 0) )
              MOVE "S  A160DISPLAY-USER." TO VDU
              MOVE "USER BRANCH ONLY" TO VDUBUF
              PERFORM SCREENIO
           ELSE
              MOVE "S  A160DISPLAY-USER." TO VDU
              MOVE SPACES TO VDUBUF
              PERFORM SCREENIO.


      *****************************************************************
      *
      *   STATUS CHECK
      *
      *****************************************************************
       STATUS-CHECK SECTION.
           MOVE "C" TO CKSTAT-ACTION.
           IF GROUP-FLAG = "Y"
              MOVE ENT-GROUP TO CKSTAT-GROUP
           ELSE
              MOVE BEG-BRANCH TO CKSTAT-BEG-BRANCH.
           MOVE END-BRANCH TO CKSTAT-END-BRANCH.
           MOVE EOM-DATE TO CKSTAT-DATE1 CKSTAT-DATE2.
           MOVE "G1" TO CKSTAT-HIGH-STATUS.

           IF GP-CASH-METHOD-ONLY = "Y" AND GP-IB-ACCRUALS-DONE = "N"
              MOVE EXT-JULIAN-BEG TO CKSTAT-DATE1
              MOVE EXT-JULIAN-END TO CKSTAT-DATE2
              MOVE "C1" TO CKSTAT-HIGH-STATUS.

           IF EXT-ROUTE-BUF = "00"
              MOVE "CHECKING BRANCH STATUS, PLEASE WAIT...."
                    TO LEGEND
              PERFORM SEND-LEGEND.

           MOVE "CL/CKSTAT" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH CKSTAT-BUF EXIT-PATHNAME.
           CANCEL FORM-PROGX.

           IF EXT-ROUTE-BUF = "00"
              MOVE SPACES TO LEGEND
              PERFORM SEND-LEGEND.

      *****************************************************************
      *
      *   RC INQUIRY
      *
      *****************************************************************
       RC-INQUIRY SECTION.
           IF GROUP-FLAG = "Y"
              MOVE ENT-GROUP TO RCINQ-GROUP
           ELSE
              MOVE BEG-BRANCH TO RCINQ-BEG-BRANCH.
           MOVE END-BRANCH TO RCINQ-END-BRANCH.
           MOVE EOM-DATE   TO RCINQW-DATE1
                              RCINQW-DATE2.
           MOVE "N" TO RCINQ-ENTER-RANGES.
           MOVE "Y" TO RCINQ-OPEN-FG.
           MOVE "Y" TO RCINQ-UPDATED-FG.
           MOVE "N" TO RCINQ-EARNINGS-FG.
           MOVE "N" TO RCINQ-EOMUPD-FG.
           MOVE "N" TO RCINQ-DEPOSIT-FG.
           MOVE "N" TO RCINQ-CHECKS-FG.
           IF GP-CASH-METHOD-ONLY = "Y" AND GP-IB-ACCRUALS-DONE = "N"
              MOVE EXT-JULIAN-BEG TO RCINQW-DATE1
              MOVE EXT-JULIAN-END TO RCINQW-DATE2
              MOVE "N" TO RCINQ-UPDATED-FG.
           MOVE "WI/RCINQ" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH RCINQ-BUF EXIT-PATHNAME.
           CANCEL FORM-PROGX.

      ****************************************************
      *   GET DEFAULT EOM DATE
      ****************************************************
       GET-DEFAULT-DATE SECTION.

           MOVE EXT-CONAME-SYSDATE TO ALP-DATE.
           PERFORM CONVERT-ALPDATE-TO-YYYYMMDD.
           MOVE DATE-YYYYMMDD TO NUM-DATE.
           MOVE 1 TO SUB.
       CHECK-TODAY-LOOP.
           IF SUB > 12
              MOVE 0 TO SUB
              GO TO EXIT-CHECK-TODAY
           ELSE
              IF GP-END-FISCAL-DATE(SUB) = NUM-DATE
                 GO TO EXIT-CHECK-TODAY
              ELSE
                 ADD 1 TO SUB
                 GO TO CHECK-TODAY-LOOP.
       EXIT-CHECK-TODAY.
           MOVE "S  A000EOMDT." TO VDU.
           IF SUB NOT = 0
              IF NUM-DATE = GP-END-FISCAL-DATE(SUB)
                 MOVE GP-END-FISCAL-DATE(SUB) TO DATE-YYYYMMDD
                 PERFORM CONVERT-YYYYMMDD-TO-MMDDYY 
                 MOVE DATE-MMDDYY TO VDU-DATE-EDIT 
              ELSE
                  MOVE 0 TO VDU-DATE-EDIT.
           PERFORM SCREENIO.

      ****************************************************
      *   CHECK EOM DATE
      ****************************************************
       CHECK-EOM-DATE SECTION.
           MOVE 1 TO SUB.
       CHECK-EOM-LOOP.
           IF SUB > 12
              MOVE 0 TO SUB
              GO TO EXIT-CHECK-EOM
           ELSE
              IF GP-END-FISCAL-DATE(SUB) = EOM-DATE
                 GO TO EXIT-CHECK-EOM
              ELSE
                 ADD 1 TO SUB
                 GO TO CHECK-EOM-LOOP.
       EXIT-CHECK-EOM.
           EXIT.

      ***************************************************
      *    TEST FOR YEARLY ANIVERSARY OF LOAN
      *    AND IF SO CLEAR YEAR TO DATE DEFERMENTS:
      *                    YEAR TO DATE ALLOTMENTS:
      ***************************************************
       TEST-CLEAR-DEFMNTS SECTION.
           MOVE LN-LOANDATE TO NDTE-DATE.
           MOVE -1          TO NDTE-HOLD.
           PERFORM INCREMENT-MONTHS.
           IF EOM-MO = NDTE-MM
              MOVE 0 TO LN-YTDNODEF LN-YTDNODEFACT LN-YTDNOALDEL.

      ***************************************************
      *    UPDATE PROFILES
      ***************************************************
       PROFILE-UPDATE SECTION.

       PROFILE-UPDATE-CONPROFILE.
      *    -------------------------------------------------------------
      *    CONVERT LN-CONPROFILE() TO A STRING PIC 9 OCCURS 60
      *    -------------------------------------------------------------
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 5
              MOVE LN-CONPROFILE(SUB) TO WORK-LN-PROFILE(SUB)
           END-PERFORM.
      *    -------------------------------------------------------------
      *    SHIFT EVERYTHING 1 BYTE TO THE RIGHT, LOSING THE RIGHTMOST BYTE
      *    -------------------------------------------------------------
           PERFORM VARYING SUB FROM 60 BY -1 UNTIL SUB < 2
              MOVE WORK-PROFILE(SUB - 1) TO WORK-PROFILE(SUB)
           END-PERFORM.
           IF CSUB > 7
              MOVE 0 TO WORK-PROFILE(1)
           ELSE
              MOVE CSUB TO WORK-PROFILE(1).
      *    -------------------------------------------------------------
      *    CONVERT PIC 9 OCCURS 60 TO LN-CONPROFILE()
      *    -------------------------------------------------------------
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 5
              MOVE WORK-LN-PROFILE(SUB) TO LN-CONPROFILE(SUB)
           END-PERFORM.

       PROFILE-UPDATE-RECPROFILE.
      *    -------------------------------------------------------------
      *    CONVERT LN-RECPROFILE() TO A STRING PIC 9 OCCURS 60
      *    -------------------------------------------------------------
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 5
              MOVE LN-RECPROFILE(SUB) TO WORK-LN-PROFILE(SUB)
           END-PERFORM.
      *    -------------------------------------------------------------
      *    SHIFT EVERYTHING 1 BYTE TO THE RIGHT, LOSING THE RIGHTMOST BYTE
      *    -------------------------------------------------------------
           PERFORM VARYING SUB FROM 60 BY -1 UNTIL SUB < 2
              MOVE WORK-PROFILE(SUB - 1) TO WORK-PROFILE(SUB)
           END-PERFORM.
           IF RDSUB > 7
              MOVE 0 TO WORK-PROFILE(1)
           ELSE
              MOVE RDSUB TO WORK-PROFILE(1).
      *    -------------------------------------------------------------
      *    CONVERT PIC 9 OCCURS 60 TO LN-RECPROFILE()
      *    -------------------------------------------------------------
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 5
              MOVE WORK-LN-PROFILE(SUB) TO LN-RECPROFILE(SUB)
           END-PERFORM.

      *    IF BR-STATE = "MX"
      *       PERFORM UPDATE-MEXICO-DOFD
      *       GO TO EXIT-PROFILE-UPDATE.

      * CURRENT 8 = MISPAYS, 9 = POTS, 10 = CURRENT, ALL CURRENT FOR TRW
      * DO NOT RESET THE DATE IF A PAIDOUT, THEY BECOME CURRENT (0), BUT
      * THE BUREAU NEEDS TO KNOW BEFORE THE PAYOUT, THEY WERE A WHAT ???
           IF LN-POFFDATE = 0
              IF (CSUB = 8 OR 9 OR 10)
      * IF THE ACCOUNT WAS REDEEMED BUREAU SAYS IT CAN GO CURRENT
                IF (LN-REPODATE NOT = 0 AND LN-REDEEMDATE NOT = 0) OR
      * CAN NOT BE REPO OR PL TO BE FLAGGED AS CURRENT
                       ((LN-REPODATE = 0) AND (LN-PLDATE = 0))
                    MOVE 0 TO LN-TRW-DOFD-DATE
                 END-IF
              ELSE
      * EVERYTHING ELSE IS DELINQUENT, BUT ONLY SET IT FIRST TIME AFTER CURRENT
                 IF LN-TRW-DOFD-DATE = 0
                    MOVE EOM-DATE TO LN-TRW-DOFD-DATE
      * IF ANYTHING OTHER THAN A 30, SET DATE BACKWARDS ACCORDINGLY, TO GET THE
      * DATE IT FIRST BECAME A 30 !!
      * EXAMPLE:  PROFILE = 2000000000 (CSUB=2)(60), LN-TRW-DOFD-DATE SHOULD BE
      *           LAST MONTHS DATE
      *           PROFILE = 3000000000 (CSUB=3)(90), LN-TRW-DOFD-DATE, 2 MONTHS AGO
      * HOWEVER IS CSUB IS 1 (30), THEN ITS EOM-DATE !!
                    IF CSUB > 1 AND CSUB < 8
                       COMPUTE NDTE-HOLD = (CSUB - 1) * -1
                       MOVE EOM-DATE TO NDTE-DATE
                       PERFORM INCREMENT-MONTHS
                       MOVE NDTE-DATE TO LN-TRW-DOFD-DATE.

       EXIT-PROFILE-UPDATE.
           EXIT.

      **************************************
      *UPDATE-MEXICO-DOFD SECTION.
      **************************************

      * MEXICO NEEDS POTS AND MISPAYS TO BE CONSIDERED DELINQUENT (30 DAY)
      * SO ONLY SUB CONSIDERED CURRENT IS 10 FOR CURRENT!

      * 10 = CURRENT
      * DO NOT RESET THE DATE IF A PAIDOUT, THEY BECOME CURRENT (0), BUT
      * THE BUREAU NEEDS TO KNOW BEFORE THE PAYOUT, THEY WERE A WHAT ???
      *    IF LN-POFFDATE = 0
      *       IF (CSUB = 10)
      * IF THE ACCOUNT WAS REDEEMED BUREAU SAYS IT CAN GO CURRENT
      *         IF (LN-REPODATE NOT = 0 AND LN-REDEEMDATE NOT = 0) OR
      * CAN NOT BE REPO OR PL TO BE FLAGGED AS CURRENT
      *                ((LN-REPODATE = 0) AND (LN-PLDATE = 0))
      *             MOVE 0 TO LN-TRW-DOFD-DATE
      *         END-IF
      *       ELSE
      * EVERYTHING ELSE IS DELINQUENT, BUT ONLY SET IT FIRST TIME AFTER CURRENT
      *          IF LN-TRW-DOFD-DATE = 0
      *             MOVE EOM-DATE TO LN-TRW-DOFD-DATE
      * IF ANYTHING OTHER THAN A 30, SET DATE BACKWARDS ACCORDINGLY, TO GET THE
      * DATE IT FIRST BECAME A 30 !!
      * EXAMPLE:  PROFILE = 2000000000 (CSUB=2)(60), LN-TRW-DOFD-DATE SHOULD BE
      *           LAST MONTHS DATE
      *           PROFILE = 3000000000 (CSUB=3)(90), LN-TRW-DOFD-DATE, 2 MONTHS AGO
      * HOWEVER IS CSUB IS 1 (30), THEN ITS EOM-DATE !!
      *             IF CSUB > 1 AND CSUB < 8
      *                COMPUTE NDTE-HOLD = (CSUB - 1) * -1
      *                MOVE EOM-DATE TO NDTE-DATE
      *                PERFORM INCREMENT-MONTHS
      *                MOVE NDTE-DATE TO LN-TRW-DOFD-DATE.

      ****************************
      *   CREATE BY1 RECORD      *
      ****************************
       CREATE-BY1-REC SECTION.
           PERFORM CLEAR-BY-FILE.
           MOVE NEXT-MO-BY-DATE-CCYYMM TO BY-DATE-CCYYMM.
           MOVE LN-OWNBR TO BY-BRNO.
           MOVE LN-CLASS TO BY-CLASS.
      *    PERFORM WRITE-BY1-FILE.
           MOVE BY1-KEY TO VBY1-KEY.
           MOVE BY-REC  TO VBY-BY-REC.
           PERFORM WRITE-VBY1-FILE.
           IF IO-BAD
              MOVE "UNABLE TO CREATE BRANCH STATS RECORD"
                 TO ERRLOGW-EOM-MSG  
              PERFORM LOG-ERROR
              MOVE 9 TO IO-FG.


      ****************************
      *   CREATE DS1 RECORD      *
      ****************************
      * CREATE-DS1-REC SECTION.                            *> S35Q-104
      *     PERFORM CLEAR-DS-FILE.
      *     MOVE NEXT-MO-SETDS-DATE-CCYYMM TO DS-DATE-CCYYMM.
      *     MOVE LN-OWNBR TO DS-BRANCH.
      *     MOVE LN-DLNO TO DS-DLNO.
      *     MOVE LN-CLASS TO DS-CLASS.
      *     PERFORM WRITE-DS1-FILE.
      *     IF IO-BAD
      *        MOVE "UNABLE TO CREATE DEALER STATS RECORD"
      *           TO ERRLOGW-EOM-MSG
      *        PERFORM LOG-ERROR
      *        MOVE 9 TO IO-FG.

      *-----------------------------------------------------------------
      * IF CUSTOMER DOES NOT DO ACCRUALS (GP-CASH-METHOD-ONLY = "Y"
      * AND GP-IB-ACCRUALS-DONE = "N"), THEN WE NEED TO CREATE RC'S
      * WITH CORRECT STATUSES FOR EOM TO DRIVE OFF.  (ACCRUAL DRIVES
      * OFF BRANCH OR GROUP & CREATES RC'S IF NEEDED)
      *-----------------------------------------------------------------
       CREATE-RCS-FOR-NO-ACCRUAL SECTION.

      *     PERFORM OPEN-RC1-FILE.
           PERFORM OPEN-VRC1-FILE.

           IF GROUP-FLAG = "N"
              MOVE BEG-BRANCH TO BR-NO
              PERFORM START-BR-FILE
              GO TO NEXT-BRANCH-NO.

           MOVE ENT-GROUP TO GR-ID.
           PERFORM OPEN-GR1-FILE.
           PERFORM SEARCH-GR-INIT.

      *-----------------------------------------------------------------
       NEXT-GROUP-LOC.

           IF GROUP-FLAG NOT = "Y"
              GO TO NEXT-BRANCH-NO.

           PERFORM SEARCH-GR-NEXT.

           IF WGR-SRESULT = 0
              GO TO EXIT-CREATE-RCS-NO-ACCRUAL.

           MOVE WGR-SRESULT TO BR-NO.
           PERFORM READ-BR-FILE.
           IF IO-BAD
              GO TO IO-ERROR.

           GO TO CHECK-EOM-RC.

      *-----------------------------------------------------------------
       NEXT-BRANCH-NO.
           PERFORM READ-BR-FILE-NEXT.
           IF IO-BAD OR BR-NO > END-BRANCH
              GO TO EXIT-CREATE-RCS-NO-ACCRUAL.

           IF BR-DATA-PATH = SPACES OR BR-MACHINE = SPACES
              GO TO NEXT-GROUP-LOC.

      *-----------------------------------------------------------------
       CHECK-EOM-RC.
           MOVE BR-NO             TO RC-BRNO.
           MOVE EOM-RC-TRANS-DATE TO RC-TRANS-DATE.

           MOVE RC1-KEY TO VRC1-KEY.
      *     PERFORM READ-RC1-FILE.
           PERFORM READ-VRC1-FILE.
           IF IO-BAD
              GO TO CREATE-EOM-RC.

           MOVE VRC-RC-REC TO RC-REC.

           IF RC-STATUS = "I1"
              GO TO NEXT-GROUP-LOC.

           IF RC-STATUS = "G1"
              GO TO NEXT-GROUP-LOC.

           IF RC-STATUS NOT = "D1"
              IF RC-STATUS NOT = "C1"
                 MOVE "RC STATUS ERROR - BRANCH #&&&&" TO MESS
                 INSPECT MESS REPLACING FIRST "&&&&" BY BR-NO
                 MOVE MESS TO ERRLOGW-EOM-MSG  
                 PERFORM SEND-MESS
                 PERFORM LOG-ERROR
                 MOVE "X" TO EXT-EOBUF-ERRCD
                 GO TO EXIT-CREATE-RCS-NO-ACCRUAL.

           MOVE "G1" TO RC-STATUS.

      *     PERFORM REWRITE-RC1-FILE.

           MOVE RC2-KEY TO VRC2-KEY.
           MOVE RC-REC  TO VRC-RC-REC.
           PERFORM REWRITE-VRC1-FILE.

           GO TO NEXT-GROUP-LOC.

      *----------------------------------------------------------------
       CREATE-EOM-RC.

           PERFORM CLEAR-RC-FILE.

           MOVE BR-NO             TO RC-BRNO.
           MOVE EOM-RC-TRANS-DATE TO RC-TRANS-DATE.
           MOVE "CREATED BY EOM"  TO RC-ERROR-MSG.
           MOVE "G1"              TO RC-STATUS.

      *     PERFORM WRITE-RC1-FILE.
           MOVE RC1-KEY TO VRC1-KEY.
           MOVE RC2-KEY TO VRC2-KEY.
           MOVE RC-REC  TO VRC-RC-REC.
           PERFORM WRITE-VRC1-FILE.
           IF IO-BAD
              GO TO IO-ERROR.

           GO TO NEXT-GROUP-LOC.

      *-----------------------------------------------------------------
       EXIT-CREATE-RCS-NO-ACCRUAL.
      *     PERFORM CLOSE-RC1-FILE.
           PERFORM CLOSE-VRC1-FILE.
           PERFORM CLOSE-GR1-FILE.

      ***************************************************
      *    LOG ERROR IN GLOBAL LG DIRECTORY
      ***************************************************
       LOG-ERROR SECTION.
           MOVE "M" TO ERRLOGW-ACTION.
           MOVE BR-NO TO ERRLOGW-BRNO.
           MOVE FORM-PATHNAME TO ERRLOGW-PROGX.
           MOVE "CL/ERRLOG" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH ERRLOGW-AREA.
           CANCEL FORM-PROGX.


       CREATE-ASCII-BY SECTION.
           PERFORM OPEN-BYADATA-FILE-OUTPUT.
           PERFORM OPEN-BYBDATA-FILE-OUTPUT.

           MOVE LAST-DAY-OF-THIS-MONTH TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-YYYYMM.
           MOVE DATE-YYYYMM TO VBY-DATE-CCYYMM.

           MOVE 0           TO VBY-CLASS.

           PERFORM VARYING BRANCH-SUB FROM 1 BY 1
            UNTIL BRANCH-SUB > 9999
              IF PROC-BRANCH-ARRAY(BRANCH-SUB) = "Y"
                 MOVE BRANCH-SUB TO VBY-BRNO
                 PERFORM OPEN-VBY1-FILE
                 MOVE ZEROS TO VBY1-KEY
                 PERFORM START-VBY1-FILE
                 PERFORM CREATE-ASCII-BY-OUT-REC
                 PERFORM CLOSE-VBY1-FILE
              END-IF
           END-PERFORM.

           PERFORM CLOSE-BYADATA-FILE.
           PERFORM CLOSE-BYBDATA-FILE.

           ADD 1 TO SAVED-OUT-FILES-CTR.
           MOVE BYADATA-OUT-PATH TO 
                    SAVED-FILES-ARRAY(SAVED-OUT-FILES-CTR).

           ADD 1 TO SAVED-OUT-FILES-CTR.
           MOVE BYBDATA-OUT-PATH TO 
                    SAVED-FILES-ARRAY(SAVED-OUT-FILES-CTR).

           GO TO CREATE-ASCII-BY-EXIT.


       CREATE-ASCII-BY-OUT-REC.
           PERFORM READ-VBY1-FILE-NEXT.

           IF IO-BAD
              EXIT PARAGRAPH.

           IF VBY-BRNO = BRANCH-SUB
              MOVE VBY1-KEY   TO BY1-KEY BYA1-KEY BYB1-KEY
              MOVE VBY-BY-REC TO BY-REC 
              MOVE BY-AREA-A  TO BYA-AREA-A
              MOVE BY-AREA-B  TO BYB-AREA-B
              PERFORM CREATE-ASCII-BYA-WS-BYA
              PERFORM CREATE-ASCII-BYB-WS-BYB
              PERFORM CREATE-ASCII-BYA-STRING
              PERFORM CREATE-ASCII-BYB-STRING
              PERFORM WRITE-BYADATA-FILE
              PERFORM WRITE-BYBDATA-FILE
           END-IF.

           GO TO CREATE-ASCII-BY-OUT-REC.
 
       CREATE-ASCII-BYB-WS-BYB.
 
           PERFORM VARYING SUB FROM 1 BY 1
            UNTIL SUB > 2
              MOVE BYB-AMT-SOLD  (SUB)   TO WS-BYB-AMT-SOLD  (SUB)
              MOVE BYB-AMT-TRANS (SUB)   TO WS-BYB-AMT-TRANS (SUB)      
              MOVE BYB-AMT-PURCH (SUB)   TO WS-BYB-AMT-PURCH (SUB)      
              MOVE BYB-NO-SOLD   (SUB)   TO WS-BYB-NO-SOLD   (SUB)     
              MOVE BYB-NO-TRANS  (SUB)   TO WS-BYB-NO-TRANS  (SUB)     
              MOVE BYB-NO-PURCH  (SUB)   TO WS-BYB-NO-PURCH  (SUB)     
              MOVE BYB-OTHCOL    (SUB)   TO WS-BYB-OTHCOL    (SUB)      
              MOVE BYB-OT2COL    (SUB)   TO WS-BYB-OT2COL    (SUB)      
              MOVE BYB-INTCOL    (SUB)   TO WS-BYB-INTCOL    (SUB)      
              MOVE BYB-LATECOL   (SUB)   TO WS-BYB-LATECOL   (SUB)      
              MOVE BYB-PRINCOL   (SUB)   TO WS-BYB-PRINCOL   (SUB)      
              MOVE BYB-DEFCOL    (SUB)   TO WS-BYB-DEFCOL    (SUB)      
              MOVE BYB-RNPRIN    (SUB)   TO WS-BYB-RNPRIN    (SUB)      
              MOVE BYB-INTREB    (SUB)   TO WS-BYB-INTREB    (SUB)      
              MOVE BYB-CLREB1    (SUB)   TO WS-BYB-CLREB1    (SUB)      
              MOVE BYB-CLREB2    (SUB)   TO WS-BYB-CLREB2    (SUB)      
              MOVE BYB-CLREB3    (SUB)   TO WS-BYB-CLREB3    (SUB)      
              MOVE BYB-CLREB4    (SUB)   TO WS-BYB-CLREB4    (SUB)      
              MOVE BYB-AHREB     (SUB)   TO WS-BYB-AHREB     (SUB)      
              MOVE BYB-PPREB     (SUB)   TO WS-BYB-PPREB     (SUB)      
              MOVE BYB-O1REB     (SUB)   TO WS-BYB-O1REB     (SUB)      
              MOVE BYB-O2REB     (SUB)   TO WS-BYB-O2REB     (SUB)      
              MOVE BYB-O3REB     (SUB)   TO WS-BYB-O3REB     (SUB)      
              MOVE BYB-O4REB     (SUB)   TO WS-BYB-O4REB     (SUB)      
              MOVE BYB-O5REB     (SUB)   TO WS-BYB-O5REB     (SUB)      
              MOVE BYB-O6REB     (SUB)   TO WS-BYB-O6REB     (SUB)      
              MOVE BYB-O7REB     (SUB)   TO WS-BYB-O7REB     (SUB)      
              MOVE BYB-SCREB     (SUB)   TO WS-BYB-SCREB     (SUB)      
              MOVE BYB-MFREB     (SUB)   TO WS-BYB-MFREB     (SUB)      
              MOVE BYB-DFREB     (SUB)   TO WS-BYB-DFREB     (SUB)      
              MOVE BYB-UN7-REB   (SUB)   TO WS-BYB-UN7-REB   (SUB)      
              MOVE BYB-UN8-REB   (SUB)   TO WS-BYB-UN8-REB   (SUB)      
              MOVE BYB-UN9-REB   (SUB)   TO WS-BYB-UN9-REB   (SUB)      
              MOVE BYB-UN10-REB  (SUB)   TO WS-BYB-UN10-REB  (SUB)      
              MOVE BYB-PART-CHGOFF(SUB)  TO WS-BYB-PART-CHGOFF (SUB)      
              MOVE BYB-ALLOTMENTS(SUB)   TO WS-BYB-ALLOTMENTS  (SUB)
              MOVE BYB-OTH-NO-COL(SUB)   TO WS-BYB-OTH-NO-COL(SUB)         
              MOVE BYB-OT2-NO-COL(SUB)   TO WS-BYB-OT2-NO-COL(SUB)         
              MOVE BYB-INT-NO-COL(SUB)   TO WS-BYB-INT-NO-COL(SUB)         
              MOVE BYB-LATE-NO-COL(SUB)  TO WS-BYB-LATE-NO-COL(SUB)        
              MOVE BYB-OTH-NO-WAIVED(SUB) TO WS-BYB-OTH-NO-WAIVED(SUB)      
              MOVE BYB-OT2-NO-WAIVED(SUB) TO WS-BYB-OT2-NO-WAIVED(SUB)      
              MOVE BYB-INT-NO-WAIVED(SUB) TO WS-BYB-INT-NO-WAIVED(SUB)      
              MOVE BYB-LATE-NO-WAIVED(SUB) 
                 TO WS-BYB-LATE-NO-WAIVED(SUB)     
              MOVE BYB-OTH-AMT-WAIVED(SUB) 
                 TO WS-BYB-OTH-AMT-WAIVED(SUB)      
              MOVE BYB-OT2-AMT-WAIVED(SUB) 
                 TO WS-BYB-OT2-AMT-WAIVED(SUB)      
              MOVE BYB-INT-AMT-WAIVED(SUB) 
                 TO WS-BYB-INT-AMT-WAIVED(SUB)      
              MOVE BYB-LATE-AMT-WAIVED(SUB) 
                 TO WS-BYB-LATE-AMT-WAIVED(SUB)   
             
              PERFORM VARYING SUB2 FROM 1 BY 1
               UNTIL SUB2 > 5
                 MOVE BYB-PL-PRINCOL(SUB,SUB2)
                   TO WS-BYB-PL-PRINCOL(SUB,SUB2)
                 MOVE BYB-PL-INTCOL(SUB,SUB2)
                   TO WS-BYB-PL-INTCOL(SUB,SUB2) 
                 MOVE BYB-PL-LATECOL(SUB,SUB2)   
                   TO WS-BYB-PL-LATECOL(SUB,SUB2) 
                 MOVE BYB-PL-O2COL(SUB,SUB2)       
                   TO WS-BYB-PL-O2COL(SUB,SUB2) 
              END-PERFORM

              PERFORM VARYING SUB2 FROM 1 BY 1
               UNTIL SUB2 > 10
                 MOVE BYB-INS-NO-ELIG(SUB,SUB2)
                   TO WS-BYB-INS-NO-ELIG(SUB,SUB2)     
                 MOVE BYB-INS-NO-WROTE(SUB,SUB2)      
                   TO WS-BYB-INS-NO-WROTE(SUB,SUB2)     
                 MOVE BYB-INS-NO-NOTWROTE(SUB,SUB2)
                   TO WS-BYB-INS-NO-NOTWROTE(SUB,SUB2) 
                 MOVE BYB-INS-AMT-WROTE(SUB,SUB2)
                   TO WS-BYB-INS-AMT-WROTE(SUB,SUB2)   
              END-PERFORM

              PERFORM VARYING SUB2 FROM 1 BY 1
               UNTIL SUB2 > 3
                 MOVE BYB-FEE-NO-ELIG(SUB,SUB2)
                   TO WS-BYB-FEE-NO-ELIG(SUB,SUB2)
                 MOVE BYB-FEE-NO-WROTE(SUB,SUB2)
                   TO WS-BYB-FEE-NO-WROTE(SUB,SUB2)
                 MOVE BYB-FEE-NO-NOTWROTE(SUB,SUB2)
                   TO WS-BYB-FEE-NO-NOTWROTE(SUB,SUB2)
                 MOVE BYB-FEE-AMT-WROTE(SUB,SUB2)
                   TO WS-BYB-FEE-AMT-WROTE(SUB,SUB2)
              END-PERFORM
           END-PERFORM.

           MOVE BYB-SUMWGTTM      TO WS-BYB-SUMWGTTM.
           MOVE BYB-SUMWGTAV      TO WS-BYB-SUMWGTAV.
           MOVE BYB-SUMWGTTA      TO WS-BYB-SUMWGTTA.
           MOVE BYB-IB-ACCRUED    TO WS-BYB-IB-ACCRUED.
           MOVE BYB-STD-POS       TO WS-BYB-STD-POS.
           MOVE BYB-30-DAY-MOVED  TO WS-BYB-30-DAY-MOVED.
           MOVE BYB-PROJ-INCREASE TO WS-BYB-PROJ-INCREASE.
           MOVE BYB-PROJ-PROFIT   TO WS-BYB-PROJ-PROFIT.

           PERFORM VARYING SUB FROM 1 BY 1
            UNTIL SUB > 3
             MOVE BYB-DLDISC(SUB) TO WS-BYB-DLDISC(SUB)

             PERFORM VARYING SUB2 FROM 1 BY 1
              UNTIL SUB2 > 2
               PERFORM VARYING SUB3 FROM 1 BY 1
                UNTIL SUB3 > 10
                MOVE BYB-UNITS(SUB,SUB2,SUB3) 
                  TO WS-BYB-UNITS(SUB,SUB2,SUB3)
                MOVE BYB-OUTBAL(SUB,SUB2,SUB3) 
                  TO WS-BYB-OUTBAL(SUB,SUB2,SUB3)
                MOVE BYB-UNITS-BNKRPT(SUB,SUB2,SUB3)
                  TO WS-BYB-UNITS-BNKRPT(SUB,SUB2,SUB3)
                MOVE BYB-OUTBAL-BNKRPT(SUB,SUB2,SUB3)
                  TO WS-BYB-OUTBAL-BNKRPT(SUB,SUB2,SUB3)
                MOVE BYB-UNEARNED-AMT(SUB,SUB2,SUB3)
                  TO WS-BYB-UNEARNED-AMT(SUB,SUB2,SUB3)
               END-PERFORM
             END-PERFORM.

           PERFORM VARYING SUB FROM 1 BY 1
            UNTIL SUB > 2
             MOVE BYB-PROCEEDS(SUB) TO WS-BYB-PROCEEDS(SUB)
             MOVE BYB-LOCKBOX(SUB)  TO WS-BYB-LOCKBOX(SUB)
           END-PERFORM.


       CREATE-ASCII-BYA-WS-BYA.

           PERFORM VARYING SUB FROM 1 BY 1
            UNTIL SUB > 3
              PERFORM VARYING SUB2 FROM 1 BY 1
               UNTIL SUB2 > 2
                 PERFORM VARYING SUB3 FROM 1 BY 1
                  UNTIL SUB3 > 11
                   MOVE BYA-NO(SUB,SUB2,SUB3) 
                     TO WS-BYA-NO(SUB,SUB2,SUB3)
                   MOVE BYA-AMT(SUB,SUB2,SUB3)
                     TO WS-BYA-AMT(SUB,SUB2,SUB3)
                   MOVE BYA-O2(SUB,SUB2,SUB3)
                     TO WS-BYA-O2(SUB,SUB2,SUB3)
                   MOVE BYA-INT(SUB,SUB2,SUB3)
                     TO WS-BYA-INT(SUB,SUB2,SUB3)
                 END-PERFORM
              END-PERFORM
           END-PERFORM.

           PERFORM VARYING SUB FROM 1 BY 1
            UNTIL SUB > 3
              PERFORM VARYING SUB2 FROM 1 BY 1
               UNTIL SUB2 > 2
                 PERFORM VARYING SUB3 FROM 1 BY 1
                  UNTIL SUB3 > 5
                    MOVE BYA-NO-REPO(SUB,SUB2,SUB3)
                      TO WS-BYA-NO-REPO(SUB,SUB2,SUB3)
                    MOVE BYA-AMT-REPO(SUB,SUB2,SUB3)
                      TO WS-BYA-AMT-REPO(SUB,SUB2,SUB3)
                    MOVE BYA-O2-REPO(SUB,SUB2,SUB3)
                      TO WS-BYA-O2-REPO(SUB,SUB2,SUB3)
                    MOVE BYA-INT-REPO(SUB,SUB2,SUB3)
                      TO WS-BYA-INT-REPO(SUB,SUB2,SUB3)
                 END-PERFORM
              END-PERFORM
           END-PERFORM.
 
           PERFORM VARYING SUB FROM 1 BY 1
            UNTIL SUB > 3
              PERFORM VARYING SUB2 FROM 1 BY 1
               UNTIL SUB2 > 2
                 PERFORM VARYING SUB3 FROM 1 BY 1
                  UNTIL SUB3 > 10
                    MOVE BYA-NO-PL(SUB,SUB2,SUB3)
                      TO WS-BYA-NO-PL(SUB,SUB2,SUB3)
                    MOVE BYA-AMT-PL(SUB,SUB2,SUB3)
                      TO WS-BYA-AMT-PL(SUB,SUB2,SUB3)
                    MOVE BYA-O2-PL(SUB,SUB2,SUB3)
                      TO WS-BYA-O2-PL(SUB,SUB2,SUB3) 
                    MOVE BYA-INT-PL(SUB,SUB2,SUB3)
                      TO WS-BYA-INT-PL(SUB,SUB2,SUB3)
                 END-PERFORM
              END-PERFORM
           END-PERFORM.

           PERFORM VARYING SUB FROM 1 BY 1
            UNTIL SUB > 3
              PERFORM VARYING SUB2 FROM 1 BY 1
               UNTIL SUB2 > 2
                MOVE BYA-DLPARVHELD(SUB,SUB2)
                  TO WS-BYA-DLPARVHELD(SUB,SUB2)
                 PERFORM VARYING SUB3 FROM 1 BY 1
                  UNTIL SUB3 > 3
                   MOVE BYA-DLRVPRIN(SUB,SUB2,SUB3)  
                     TO WS-BYA-DLRVPRIN(SUB,SUB2,SUB3)
                   MOVE BYA-DLRVCHGS(SUB,SUB2,SUB3) 
                     TO WS-BYA-DLRVCHGS(SUB,SUB2,SUB3)
                 END-PERFORM
              END-PERFORM
           END-PERFORM.


       CREATE-ASCII-BYA-STRING.

           MOVE SPACES TO BYADATA-OUT-REC
                          WS-MISC-STRING(1)
                          WS-MISC-STRING(2)
                          WS-MISC-STRING(3)
                          WS-MISC-STRING(4)
                          WS-MISC-STRING(5)
                          WS-MISC-STRING(6)
                          WS-MISC-STRING(7).

           STRING BYA-DATE-CCYYMM     ,PIPE-1,
                  BYA-BRNO            ,PIPE-1,  
                  BYA-CLASS           ,PIPE-1,  
                  WS-BYA-NO(1,1,1)    ,PIPE-1, 
                  WS-BYA-NO(1,1,2)    ,PIPE-1, 
                  WS-BYA-NO(1,1,3)    ,PIPE-1, 
                  WS-BYA-NO(1,1,4)    ,PIPE-1, 
                  WS-BYA-NO(1,1,5)    ,PIPE-1, 
                  WS-BYA-NO(1,1,6)    ,PIPE-1, 
                  WS-BYA-NO(1,1,7)    ,PIPE-1, 
                  WS-BYA-NO(1,1,8)    ,PIPE-1, 
                  WS-BYA-NO(1,1,9)    ,PIPE-1, 
                  WS-BYA-NO(1,1,10)   ,PIPE-1, 
                  WS-BYA-NO(1,1,11)   ,PIPE-1,
                  WS-BYA-NO(1,2,1)    ,PIPE-1, 
                  WS-BYA-NO(1,2,2)    ,PIPE-1, 
                  WS-BYA-NO(1,2,3)    ,PIPE-1, 
                  WS-BYA-NO(1,2,4)    ,PIPE-1,
                  WS-BYA-NO(1,2,5)    ,PIPE-1,
                  WS-BYA-NO(1,2,6)    ,PIPE-1, 
                  WS-BYA-NO(1,2,7)    ,PIPE-1,
                  WS-BYA-NO(1,2,8)    ,PIPE-1, 
                  WS-BYA-NO(1,2,9)    ,PIPE-1, 
                  WS-BYA-NO(1,2,10)   ,PIPE-1, 
                  WS-BYA-NO(1,2,11)   ,PIPE-1,
                  WS-BYA-NO(2,1,1)    ,PIPE-1, 
                  WS-BYA-NO(2,1,2)    ,PIPE-1, 
                  WS-BYA-NO(2,1,3)    ,PIPE-1, 
                  WS-BYA-NO(2,1,4)    ,PIPE-1, 
                  WS-BYA-NO(2,1,5)    ,PIPE-1, 
                  WS-BYA-NO(2,1,6)    ,PIPE-1, 
                  WS-BYA-NO(2,1,7)    ,PIPE-1, 
                  WS-BYA-NO(2,1,8)    ,PIPE-1, 
                  WS-BYA-NO(2,1,9)    ,PIPE-1, 
                  WS-BYA-NO(2,1,10)   ,PIPE-1, 
                  WS-BYA-NO(2,1,11)   ,PIPE-1,
                  WS-BYA-NO(2,2,1)    ,PIPE-1, 
                  WS-BYA-NO(2,2,2)    ,PIPE-1, 
                  WS-BYA-NO(2,2,3)    ,PIPE-1, 
                  WS-BYA-NO(2,2,4)    ,PIPE-1,
                  WS-BYA-NO(2,2,5)    ,PIPE-1,
                  WS-BYA-NO(2,2,6)    ,PIPE-1, 
                  WS-BYA-NO(2,2,7)    ,PIPE-1,
                  WS-BYA-NO(2,2,8)    ,PIPE-1, 
                  WS-BYA-NO(2,2,9)    ,PIPE-1, 
                  WS-BYA-NO(2,2,10)   ,PIPE-1, 
                  WS-BYA-NO(2,2,11)   ,PIPE-1,
                  WS-BYA-NO(3,1,1)    ,PIPE-1, 
                  WS-BYA-NO(3,1,2)    ,PIPE-1, 
                  WS-BYA-NO(3,1,3)    ,PIPE-1, 
                  WS-BYA-NO(3,1,4)    ,PIPE-1, 
                  WS-BYA-NO(3,1,5)    ,PIPE-1, 
                  WS-BYA-NO(3,1,6)    ,PIPE-1, 
                  WS-BYA-NO(3,1,7)    ,PIPE-1, 
                  WS-BYA-NO(3,1,8)    ,PIPE-1, 
                  WS-BYA-NO(3,1,9)    ,PIPE-1, 
                  WS-BYA-NO(3,1,10)   ,PIPE-1, 
                  WS-BYA-NO(3,1,11)   ,PIPE-1,
                  WS-BYA-NO(3,2,1)    ,PIPE-1, 
                  WS-BYA-NO(3,2,2)    ,PIPE-1, 
                  WS-BYA-NO(3,2,3)    ,PIPE-1, 
                  WS-BYA-NO(3,2,4)    ,PIPE-1,
                  WS-BYA-NO(3,2,5)    ,PIPE-1,
                  WS-BYA-NO(3,2,6)    ,PIPE-1, 
                  WS-BYA-NO(3,2,7)    ,PIPE-1,
                  WS-BYA-NO(3,2,8)    ,PIPE-1, 
                  WS-BYA-NO(3,2,9)    ,PIPE-1, 
                  WS-BYA-NO(3,2,10)   ,PIPE-1, 
                  WS-BYA-NO(3,2,11)   ,PIPE-1,
                  WS-BYA-AMT(1,1,1)   ,PIPE-1, 
                  WS-BYA-AMT(1,1,2)   ,PIPE-1, 
                  WS-BYA-AMT(1,1,3)   ,PIPE-1, 
                  WS-BYA-AMT(1,1,4)   ,PIPE-1, 
                  WS-BYA-AMT(1,1,5)   ,PIPE-1, 
                  WS-BYA-AMT(1,1,6)   ,PIPE-1,
                  WS-BYA-AMT(1,1,7)   ,PIPE-1, 
                  WS-BYA-AMT(1,1,8)   ,PIPE-1, 
                  WS-BYA-AMT(1,1,9)   ,PIPE-1, 
                  WS-BYA-AMT(1,1,10)  ,PIPE-1, 
                  WS-BYA-AMT(1,1,11)  ,PIPE-1,
                  WS-BYA-AMT(1,2,1)   ,PIPE-1, 
                  WS-BYA-AMT(1,2,2)   ,PIPE-1, 
                  WS-BYA-AMT(1,2,3)   ,PIPE-1, 
                  WS-BYA-AMT(1,2,4)   ,PIPE-1, 
                  WS-BYA-AMT(1,2,5)   ,PIPE-1, 
                  WS-BYA-AMT(1,2,6)   ,PIPE-1,
                  WS-BYA-AMT(1,2,7)   ,PIPE-1, 
                  WS-BYA-AMT(1,2,8)   ,PIPE-1, 
                  WS-BYA-AMT(1,2,9)   ,PIPE-1, 
                  WS-BYA-AMT(1,2,10)  ,PIPE-1, 
                  WS-BYA-AMT(1,2,11)  ,PIPE-1,
                  WS-BYA-AMT(2,1,1)   ,PIPE-1, 
                  WS-BYA-AMT(2,1,2)   ,PIPE-1, 
                  WS-BYA-AMT(2,1,3)   ,PIPE-1, 
                  WS-BYA-AMT(2,1,4)   ,PIPE-1, 
                  WS-BYA-AMT(2,1,5)   ,PIPE-1, 
                  WS-BYA-AMT(2,1,6)   ,PIPE-1,
                  WS-BYA-AMT(2,1,7)   ,PIPE-1, 
                  WS-BYA-AMT(2,1,8)   ,PIPE-1, 
                  WS-BYA-AMT(2,1,9)   ,PIPE-1, 
                  WS-BYA-AMT(2,1,10)  ,PIPE-1, 
                  WS-BYA-AMT(2,1,11)  ,PIPE-1,
                  WS-BYA-AMT(2,2,1)   ,PIPE-1, 
                  WS-BYA-AMT(2,2,2)   ,PIPE-1, 
                  WS-BYA-AMT(2,2,3)   ,PIPE-1, 
                  WS-BYA-AMT(2,2,4)   ,PIPE-1, 
                  WS-BYA-AMT(2,2,5)   ,PIPE-1, 
                  WS-BYA-AMT(2,2,6)   ,PIPE-1,
                  WS-BYA-AMT(2,2,7)   ,PIPE-1, 
                  WS-BYA-AMT(2,2,8)   ,PIPE-1, 
                  WS-BYA-AMT(2,2,9)   ,PIPE-1, 
                  WS-BYA-AMT(2,2,10)  ,PIPE-1, 
                  WS-BYA-AMT(2,2,11)  ,PIPE-1 
              DELIMITED BY SIZE INTO WS-MISC-STRING(1).

           STRING WS-BYA-AMT(3,1,1)   ,PIPE-1, 
                  WS-BYA-AMT(3,1,2)   ,PIPE-1, 
                  WS-BYA-AMT(3,1,3)   ,PIPE-1, 
                  WS-BYA-AMT(3,1,4)   ,PIPE-1, 
                  WS-BYA-AMT(3,1,5)   ,PIPE-1, 
                  WS-BYA-AMT(3,1,6)   ,PIPE-1,
                  WS-BYA-AMT(3,1,7)   ,PIPE-1, 
                  WS-BYA-AMT(3,1,8)   ,PIPE-1, 
                  WS-BYA-AMT(3,1,9)   ,PIPE-1, 
                  WS-BYA-AMT(3,1,10)  ,PIPE-1, 
                  WS-BYA-AMT(3,1,11)  ,PIPE-1,
                  WS-BYA-AMT(3,2,1)   ,PIPE-1, 
                  WS-BYA-AMT(3,2,2)   ,PIPE-1, 
                  WS-BYA-AMT(3,2,3)   ,PIPE-1, 
                  WS-BYA-AMT(3,2,4)   ,PIPE-1, 
                  WS-BYA-AMT(3,2,5)   ,PIPE-1, 
                  WS-BYA-AMT(3,2,6)   ,PIPE-1,
                  WS-BYA-AMT(3,2,7)   ,PIPE-1, 
                  WS-BYA-AMT(3,2,8)   ,PIPE-1, 
                  WS-BYA-AMT(3,2,9)   ,PIPE-1, 
                  WS-BYA-AMT(3,2,10)  ,PIPE-1, 
                  WS-BYA-AMT(3,2,11)  ,PIPE-1,
                  WS-BYA-O2(1,1,1)    ,PIPE-1, 
                  WS-BYA-O2(1,1,2)    ,PIPE-1, 
                  WS-BYA-O2(1,1,3)    ,PIPE-1, 
                  WS-BYA-O2(1,1,4)    ,PIPE-1, 
                  WS-BYA-O2(1,1,5)    ,PIPE-1, 
                  WS-BYA-O2(1,1,6)    ,PIPE-1, 
                  WS-BYA-O2(1,1,7)    ,PIPE-1, 
                  WS-BYA-O2(1,1,8)    ,PIPE-1, 
                  WS-BYA-O2(1,1,9)    ,PIPE-1, 
                  WS-BYA-O2(1,1,10)   ,PIPE-1, 
                  WS-BYA-O2(1,1,11)   ,PIPE-1, 
                  WS-BYA-O2(1,2,1)    ,PIPE-1, 
                  WS-BYA-O2(1,2,2)    ,PIPE-1, 
                  WS-BYA-O2(1,2,3)    ,PIPE-1, 
                  WS-BYA-O2(1,2,4)    ,PIPE-1, 
                  WS-BYA-O2(1,2,5)    ,PIPE-1, 
                  WS-BYA-O2(1,2,6)    ,PIPE-1, 
                  WS-BYA-O2(1,2,7)    ,PIPE-1, 
                  WS-BYA-O2(1,2,8)    ,PIPE-1, 
                  WS-BYA-O2(1,2,9)    ,PIPE-1, 
                  WS-BYA-O2(1,2,10)   ,PIPE-1, 
                  WS-BYA-O2(1,2,11)   ,PIPE-1, 
                  WS-BYA-O2(2,1,1)    ,PIPE-1, 
                  WS-BYA-O2(2,1,2)    ,PIPE-1, 
                  WS-BYA-O2(2,1,3)    ,PIPE-1, 
                  WS-BYA-O2(2,1,4)    ,PIPE-1, 
                  WS-BYA-O2(2,1,5)    ,PIPE-1, 
                  WS-BYA-O2(2,1,6)    ,PIPE-1, 
                  WS-BYA-O2(2,1,7)    ,PIPE-1, 
                  WS-BYA-O2(2,1,8)    ,PIPE-1, 
                  WS-BYA-O2(2,1,9)    ,PIPE-1, 
                  WS-BYA-O2(2,1,10)   ,PIPE-1, 
                  WS-BYA-O2(2,1,11)   ,PIPE-1, 
                  WS-BYA-O2(2,2,1)    ,PIPE-1, 
                  WS-BYA-O2(2,2,2)    ,PIPE-1, 
                  WS-BYA-O2(2,2,3)    ,PIPE-1, 
                  WS-BYA-O2(2,2,4)    ,PIPE-1, 
                  WS-BYA-O2(2,2,5)    ,PIPE-1, 
                  WS-BYA-O2(2,2,6)    ,PIPE-1, 
                  WS-BYA-O2(2,2,7)    ,PIPE-1, 
                  WS-BYA-O2(2,2,8)    ,PIPE-1, 
                  WS-BYA-O2(2,2,9)    ,PIPE-1, 
                  WS-BYA-O2(2,2,10)   ,PIPE-1, 
                  WS-BYA-O2(2,2,11)   ,PIPE-1, 
                  WS-BYA-O2(3,1,1)    ,PIPE-1, 
                  WS-BYA-O2(3,1,2)    ,PIPE-1, 
                  WS-BYA-O2(3,1,3)    ,PIPE-1, 
                  WS-BYA-O2(3,1,4)    ,PIPE-1, 
                  WS-BYA-O2(3,1,5)    ,PIPE-1, 
                  WS-BYA-O2(3,1,6)    ,PIPE-1, 
                  WS-BYA-O2(3,1,7)    ,PIPE-1, 
                  WS-BYA-O2(3,1,8)    ,PIPE-1, 
                  WS-BYA-O2(3,1,9)    ,PIPE-1, 
                  WS-BYA-O2(3,1,10)   ,PIPE-1, 
                  WS-BYA-O2(3,1,11)   ,PIPE-1, 
                  WS-BYA-O2(3,2,1)    ,PIPE-1, 
                  WS-BYA-O2(3,2,2)    ,PIPE-1, 
                  WS-BYA-O2(3,2,3)    ,PIPE-1, 
                  WS-BYA-O2(3,2,4)    ,PIPE-1, 
                  WS-BYA-O2(3,2,5)    ,PIPE-1, 
                  WS-BYA-O2(3,2,6)    ,PIPE-1, 
                  WS-BYA-O2(3,2,7)    ,PIPE-1, 
                  WS-BYA-O2(3,2,8)    ,PIPE-1, 
                  WS-BYA-O2(3,2,9)    ,PIPE-1, 
                  WS-BYA-O2(3,2,10)   ,PIPE-1, 
                  WS-BYA-O2(3,2,11)   ,PIPE-1 
              DELIMITED BY SIZE INTO WS-MISC-STRING(2).

           STRING WS-BYA-INT(1,1,1)   ,PIPE-1, 
                  WS-BYA-INT(1,1,2)   ,PIPE-1, 
                  WS-BYA-INT(1,1,3)   ,PIPE-1, 
                  WS-BYA-INT(1,1,4)   ,PIPE-1, 
                  WS-BYA-INT(1,1,5)   ,PIPE-1, 
                  WS-BYA-INT(1,1,6)   ,PIPE-1, 
                  WS-BYA-INT(1,1,7)   ,PIPE-1, 
                  WS-BYA-INT(1,1,8)   ,PIPE-1, 
                  WS-BYA-INT(1,1,9)   ,PIPE-1, 
                  WS-BYA-INT(1,1,10)  ,PIPE-1, 
                  WS-BYA-INT(1,1,11)  ,PIPE-1, 
                  WS-BYA-INT(1,2,1)   ,PIPE-1, 
                  WS-BYA-INT(1,2,2)   ,PIPE-1, 
                  WS-BYA-INT(1,2,3)   ,PIPE-1, 
                  WS-BYA-INT(1,2,4)   ,PIPE-1, 
                  WS-BYA-INT(1,2,5)   ,PIPE-1, 
                  WS-BYA-INT(1,2,6)   ,PIPE-1, 
                  WS-BYA-INT(1,2,7)   ,PIPE-1, 
                  WS-BYA-INT(1,2,8)   ,PIPE-1, 
                  WS-BYA-INT(1,2,9)   ,PIPE-1, 
                  WS-BYA-INT(1,2,10)  ,PIPE-1, 
                  WS-BYA-INT(1,2,11)  ,PIPE-1,
                  WS-BYA-INT(2,1,1)   ,PIPE-1, 
                  WS-BYA-INT(2,1,2)   ,PIPE-1, 
                  WS-BYA-INT(2,1,3)   ,PIPE-1, 
                  WS-BYA-INT(2,1,4)   ,PIPE-1, 
                  WS-BYA-INT(2,1,5)   ,PIPE-1, 
                  WS-BYA-INT(2,1,6)   ,PIPE-1, 
                  WS-BYA-INT(2,1,7)   ,PIPE-1, 
                  WS-BYA-INT(2,1,8)   ,PIPE-1, 
                  WS-BYA-INT(2,1,9)   ,PIPE-1, 
                  WS-BYA-INT(2,1,10)  ,PIPE-1, 
                  WS-BYA-INT(2,1,11)  ,PIPE-1, 
                  WS-BYA-INT(2,2,1)   ,PIPE-1, 
                  WS-BYA-INT(2,2,2)   ,PIPE-1, 
                  WS-BYA-INT(2,2,3)   ,PIPE-1, 
                  WS-BYA-INT(2,2,4)   ,PIPE-1, 
                  WS-BYA-INT(2,2,5)   ,PIPE-1, 
                  WS-BYA-INT(2,2,6)   ,PIPE-1, 
                  WS-BYA-INT(2,2,7)   ,PIPE-1, 
                  WS-BYA-INT(2,2,8)   ,PIPE-1, 
                  WS-BYA-INT(2,2,9)   ,PIPE-1, 
                  WS-BYA-INT(2,2,10)  ,PIPE-1, 
                  WS-BYA-INT(2,2,11)  ,PIPE-1,
                  WS-BYA-INT(3,1,1)   ,PIPE-1, 
                  WS-BYA-INT(3,1,2)   ,PIPE-1, 
                  WS-BYA-INT(3,1,3)   ,PIPE-1, 
                  WS-BYA-INT(3,1,4)   ,PIPE-1, 
                  WS-BYA-INT(3,1,5)   ,PIPE-1, 
                  WS-BYA-INT(3,1,6)   ,PIPE-1, 
                  WS-BYA-INT(3,1,7)   ,PIPE-1, 
                  WS-BYA-INT(3,1,8)   ,PIPE-1, 
                  WS-BYA-INT(3,1,9)   ,PIPE-1, 
                  WS-BYA-INT(3,1,10)  ,PIPE-1, 
                  WS-BYA-INT(3,1,11)  ,PIPE-1, 
                  WS-BYA-INT(3,2,1)   ,PIPE-1, 
                  WS-BYA-INT(3,2,2)   ,PIPE-1, 
                  WS-BYA-INT(3,2,3)   ,PIPE-1, 
                  WS-BYA-INT(3,2,4)   ,PIPE-1, 
                  WS-BYA-INT(3,2,5)   ,PIPE-1, 
                  WS-BYA-INT(3,2,6)   ,PIPE-1, 
                  WS-BYA-INT(3,2,7)   ,PIPE-1, 
                  WS-BYA-INT(3,2,8)   ,PIPE-1, 
                  WS-BYA-INT(3,2,9)   ,PIPE-1, 
                  WS-BYA-INT(3,2,10)  ,PIPE-1, 
                  WS-BYA-INT(3,2,11)  ,PIPE-1,
                  WS-BYA-NO-REPO(1,1,1) ,PIPE-1, 
                  WS-BYA-NO-REPO(1,1,2) ,PIPE-1,
                  WS-BYA-NO-REPO(1,1,3) ,PIPE-1, 
                  WS-BYA-NO-REPO(1,1,4) ,PIPE-1, 
                  WS-BYA-NO-REPO(1,1,5) ,PIPE-1, 
                  WS-BYA-NO-REPO(1,2,1) ,PIPE-1, 
                  WS-BYA-NO-REPO(1,2,2) ,PIPE-1,
                  WS-BYA-NO-REPO(1,2,3) ,PIPE-1, 
                  WS-BYA-NO-REPO(1,2,4) ,PIPE-1, 
                  WS-BYA-NO-REPO(1,2,5) ,PIPE-1, 
                  WS-BYA-NO-REPO(2,1,1) ,PIPE-1, 
                  WS-BYA-NO-REPO(2,1,2) ,PIPE-1,
                  WS-BYA-NO-REPO(2,1,3) ,PIPE-1, 
                  WS-BYA-NO-REPO(2,1,4) ,PIPE-1, 
                  WS-BYA-NO-REPO(2,1,5) ,PIPE-1, 
                  WS-BYA-NO-REPO(2,2,1) ,PIPE-1, 
                  WS-BYA-NO-REPO(2,2,2) ,PIPE-1,
                  WS-BYA-NO-REPO(2,2,3) ,PIPE-1, 
                  WS-BYA-NO-REPO(2,2,4) ,PIPE-1, 
                  WS-BYA-NO-REPO(2,2,5) ,PIPE-1, 
                  WS-BYA-NO-REPO(3,1,1) ,PIPE-1, 
                  WS-BYA-NO-REPO(3,1,2)  ,PIPE-1,
                  WS-BYA-NO-REPO(3,1,3)  ,PIPE-1, 
                  WS-BYA-NO-REPO(3,1,4)  ,PIPE-1, 
                  WS-BYA-NO-REPO(3,1,5)  ,PIPE-1, 
                  WS-BYA-NO-REPO(3,2,1)  ,PIPE-1, 
                  WS-BYA-NO-REPO(3,2,2)  ,PIPE-1,
                  WS-BYA-NO-REPO(3,2,3)  ,PIPE-1, 
                  WS-BYA-NO-REPO(3,2,4)  ,PIPE-1, 
                  WS-BYA-NO-REPO(3,2,5)  ,PIPE-1 
              DELIMITED BY SIZE INTO WS-MISC-STRING(3).

           STRING WS-BYA-AMT-REPO(1,1,1) ,PIPE-1, 
                  WS-BYA-AMT-REPO(1,1,2) ,PIPE-1, 
                  WS-BYA-AMT-REPO(1,1,3) ,PIPE-1, 
                  WS-BYA-AMT-REPO(1,1,4) ,PIPE-1,
                  WS-BYA-AMT-REPO(1,1,5) ,PIPE-1,
                  WS-BYA-AMT-REPO(1,2,1) ,PIPE-1,
                  WS-BYA-AMT-REPO(1,2,2) ,PIPE-1,
                  WS-BYA-AMT-REPO(1,2,3) ,PIPE-1, 
                  WS-BYA-AMT-REPO(1,2,4) ,PIPE-1, 
                  WS-BYA-AMT-REPO(1,2,5) ,PIPE-1, 
                  WS-BYA-AMT-REPO(2,1,1) ,PIPE-1, 
                  WS-BYA-AMT-REPO(2,1,2) ,PIPE-1, 
                  WS-BYA-AMT-REPO(2,1,3) ,PIPE-1, 
                  WS-BYA-AMT-REPO(2,1,4) ,PIPE-1,
                  WS-BYA-AMT-REPO(2,1,5) ,PIPE-1,
                  WS-BYA-AMT-REPO(2,2,1) ,PIPE-1,
                  WS-BYA-AMT-REPO(2,2,2) ,PIPE-1,
                  WS-BYA-AMT-REPO(2,2,3) ,PIPE-1, 
                  WS-BYA-AMT-REPO(2,2,4) ,PIPE-1, 
                  WS-BYA-AMT-REPO(2,2,5) ,PIPE-1, 
                  WS-BYA-AMT-REPO(3,1,1) ,PIPE-1, 
                  WS-BYA-AMT-REPO(3,1,2),PIPE-1, 
                  WS-BYA-AMT-REPO(3,1,3) ,PIPE-1, 
                  WS-BYA-AMT-REPO(3,1,4) ,PIPE-1,
                  WS-BYA-AMT-REPO(3,1,5) ,PIPE-1,
                  WS-BYA-AMT-REPO(3,2,1) ,PIPE-1,
                  WS-BYA-AMT-REPO(3,2,2) ,PIPE-1,
                  WS-BYA-AMT-REPO(3,2,3) ,PIPE-1, 
                  WS-BYA-AMT-REPO(3,2,4) ,PIPE-1, 
                  WS-BYA-AMT-REPO(3,2,5) ,PIPE-1, 
                  WS-BYA-O2-REPO(1,1,1) ,PIPE-1, 
                  WS-BYA-O2-REPO(1,1,2) ,PIPE-1, 
                  WS-BYA-O2-REPO(1,1,3) ,PIPE-1, 
                  WS-BYA-O2-REPO(1,1,4) ,PIPE-1, 
                  WS-BYA-O2-REPO(1,1,5) ,PIPE-1, 
                  WS-BYA-O2-REPO(1,2,1) ,PIPE-1, 
                  WS-BYA-O2-REPO(1,2,2) ,PIPE-1,
                  WS-BYA-O2-REPO(1,2,3) ,PIPE-1, 
                  WS-BYA-O2-REPO(1,2,4) ,PIPE-1, 
                  WS-BYA-O2-REPO(1,2,5) ,PIPE-1, 
                  WS-BYA-O2-REPO(2,1,1) ,PIPE-1, 
                  WS-BYA-O2-REPO(2,1,2) ,PIPE-1, 
                  WS-BYA-O2-REPO(2,1,3) ,PIPE-1, 
                  WS-BYA-O2-REPO(2,1,4) ,PIPE-1, 
                  WS-BYA-O2-REPO(2,1,5) ,PIPE-1, 
                  WS-BYA-O2-REPO(2,2,1) ,PIPE-1, 
                  WS-BYA-O2-REPO(2,2,2) ,PIPE-1,
                  WS-BYA-O2-REPO(2,2,3) ,PIPE-1, 
                  WS-BYA-O2-REPO(2,2,4) ,PIPE-1, 
                  WS-BYA-O2-REPO(2,2,5) ,PIPE-1,
                  WS-BYA-O2-REPO(3,1,1) ,PIPE-1, 
                  WS-BYA-O2-REPO(3,1,2) ,PIPE-1, 
                  WS-BYA-O2-REPO(3,1,3) ,PIPE-1, 
                  WS-BYA-O2-REPO(3,1,4) ,PIPE-1, 
                  WS-BYA-O2-REPO(3,1,5) ,PIPE-1, 
                  WS-BYA-O2-REPO(3,2,1) ,PIPE-1, 
                  WS-BYA-O2-REPO(3,2,2) ,PIPE-1,
                  WS-BYA-O2-REPO(3,2,3) ,PIPE-1, 
                  WS-BYA-O2-REPO(3,2,4) ,PIPE-1, 
                  WS-BYA-O2-REPO(3,2,5) ,PIPE-1,
                  WS-BYA-INT-REPO(1,1,1) ,PIPE-1, 
                  WS-BYA-INT-REPO(1,1,2) ,PIPE-1, 
                  WS-BYA-INT-REPO(1,1,3) ,PIPE-1, 
                  WS-BYA-INT-REPO(1,1,4) ,PIPE-1, 
                  WS-BYA-INT-REPO(1,1,5) ,PIPE-1, 
                  WS-BYA-INT-REPO(1,2,1) ,PIPE-1, 
                  WS-BYA-INT-REPO(1,2,2) ,PIPE-1, 
                  WS-BYA-INT-REPO(1,2,3) ,PIPE-1, 
                  WS-BYA-INT-REPO(1,2,4) ,PIPE-1, 
                  WS-BYA-INT-REPO(1,2,5) ,PIPE-1, 
                  WS-BYA-INT-REPO(2,1,1) ,PIPE-1, 
                  WS-BYA-INT-REPO(2,1,2) ,PIPE-1, 
                  WS-BYA-INT-REPO(2,1,3) ,PIPE-1, 
                  WS-BYA-INT-REPO(2,1,4) ,PIPE-1, 
                  WS-BYA-INT-REPO(2,1,5) ,PIPE-1, 
                  WS-BYA-INT-REPO(2,2,1) ,PIPE-1, 
                  WS-BYA-INT-REPO(2,2,2) ,PIPE-1, 
                  WS-BYA-INT-REPO(2,2,3) ,PIPE-1, 
                  WS-BYA-INT-REPO(2,2,4) ,PIPE-1, 
                  WS-BYA-INT-REPO(2,2,5) ,PIPE-1, 
                  WS-BYA-INT-REPO(3,1,1) ,PIPE-1, 
                  WS-BYA-INT-REPO(3,1,2) ,PIPE-1, 
                  WS-BYA-INT-REPO(3,1,3) ,PIPE-1, 
                  WS-BYA-INT-REPO(3,1,4) ,PIPE-1, 
                  WS-BYA-INT-REPO(3,1,5) ,PIPE-1, 
                  WS-BYA-INT-REPO(3,2,1) ,PIPE-1, 
                  WS-BYA-INT-REPO(3,2,2) ,PIPE-1, 
                  WS-BYA-INT-REPO(3,2,3) ,PIPE-1, 
                  WS-BYA-INT-REPO(3,2,4) ,PIPE-1, 
                  WS-BYA-INT-REPO(3,2,5) ,PIPE-1 
              DELIMITED BY SIZE INTO WS-MISC-STRING(4).

           STRING WS-BYA-NO-PL(1,1,1)   ,PIPE-1, 
                  WS-BYA-NO-PL(1,1,2)   ,PIPE-1, 
                  WS-BYA-NO-PL(1,1,3)   ,PIPE-1, 
                  WS-BYA-NO-PL(1,1,4)   ,PIPE-1, 
                  WS-BYA-NO-PL(1,1,5)   ,PIPE-1, 
                  WS-BYA-NO-PL(1,1,6)   ,PIPE-1, 
                  WS-BYA-NO-PL(1,1,7)   ,PIPE-1, 
                  WS-BYA-NO-PL(1,1,8)   ,PIPE-1, 
                  WS-BYA-NO-PL(1,1,9)   ,PIPE-1, 
                  WS-BYA-NO-PL(1,1,10)  ,PIPE-1, 
                  WS-BYA-NO-PL(1,2,1)   ,PIPE-1, 
                  WS-BYA-NO-PL(1,2,2)   ,PIPE-1, 
                  WS-BYA-NO-PL(1,2,3)   ,PIPE-1, 
                  WS-BYA-NO-PL(1,2,4)   ,PIPE-1, 
                  WS-BYA-NO-PL(1,2,5)   ,PIPE-1, 
                  WS-BYA-NO-PL(1,2,6)   ,PIPE-1, 
                  WS-BYA-NO-PL(1,2,7)   ,PIPE-1, 
                  WS-BYA-NO-PL(1,2,8)   ,PIPE-1, 
                  WS-BYA-NO-PL(1,2,9)   ,PIPE-1, 
                  WS-BYA-NO-PL(1,2,10)  ,PIPE-1, 
                  WS-BYA-NO-PL(2,1,1)   ,PIPE-1, 
                  WS-BYA-NO-PL(2,1,2)   ,PIPE-1, 
                  WS-BYA-NO-PL(2,1,3)   ,PIPE-1, 
                  WS-BYA-NO-PL(2,1,4)   ,PIPE-1, 
                  WS-BYA-NO-PL(2,1,5)   ,PIPE-1, 
                  WS-BYA-NO-PL(2,1,6)   ,PIPE-1, 
                  WS-BYA-NO-PL(2,1,7)   ,PIPE-1, 
                  WS-BYA-NO-PL(2,1,8)   ,PIPE-1, 
                  WS-BYA-NO-PL(2,1,9)   ,PIPE-1, 
                  WS-BYA-NO-PL(2,1,10)  ,PIPE-1, 
                  WS-BYA-NO-PL(2,2,1)   ,PIPE-1, 
                  WS-BYA-NO-PL(2,2,2)   ,PIPE-1, 
                  WS-BYA-NO-PL(2,2,3)   ,PIPE-1, 
                  WS-BYA-NO-PL(2,2,4)   ,PIPE-1, 
                  WS-BYA-NO-PL(2,2,5)   ,PIPE-1, 
                  WS-BYA-NO-PL(2,2,6)   ,PIPE-1, 
                  WS-BYA-NO-PL(2,2,7)   ,PIPE-1, 
                  WS-BYA-NO-PL(2,2,8)   ,PIPE-1, 
                  WS-BYA-NO-PL(2,2,9)   ,PIPE-1, 
                  WS-BYA-NO-PL(2,2,10)  ,PIPE-1, 
                  WS-BYA-NO-PL(3,1,1)   ,PIPE-1, 
                  WS-BYA-NO-PL(3,1,2)   ,PIPE-1, 
                  WS-BYA-NO-PL(3,1,3)   ,PIPE-1, 
                  WS-BYA-NO-PL(3,1,4)   ,PIPE-1, 
                  WS-BYA-NO-PL(3,1,5)   ,PIPE-1, 
                  WS-BYA-NO-PL(3,1,6)   ,PIPE-1, 
                  WS-BYA-NO-PL(3,1,7)   ,PIPE-1, 
                  WS-BYA-NO-PL(3,1,8)   ,PIPE-1, 
                  WS-BYA-NO-PL(3,1,9)   ,PIPE-1, 
                  WS-BYA-NO-PL(3,1,10)  ,PIPE-1, 
                  WS-BYA-NO-PL(3,2,1)   ,PIPE-1, 
                  WS-BYA-NO-PL(3,2,2)   ,PIPE-1, 
                  WS-BYA-NO-PL(3,2,3)   ,PIPE-1, 
                  WS-BYA-NO-PL(3,2,4)   ,PIPE-1, 
                  WS-BYA-NO-PL(3,2,5)   ,PIPE-1, 
                  WS-BYA-NO-PL(3,2,6)   ,PIPE-1, 
                  WS-BYA-NO-PL(3,2,7)   ,PIPE-1, 
                  WS-BYA-NO-PL(3,2,8)   ,PIPE-1, 
                  WS-BYA-NO-PL(3,2,9)   ,PIPE-1, 
                  WS-BYA-NO-PL(2,2,10)  ,PIPE-1, 
                 WS-BYA-AMT-PL(1,1,1)   ,PIPE-1, 
                 WS-BYA-AMT-PL(1,1,2)   ,PIPE-1, 
                 WS-BYA-AMT-PL(1,1,3)   ,PIPE-1, 
                 WS-BYA-AMT-PL(1,1,4)   ,PIPE-1, 
                 WS-BYA-AMT-PL(1,1,5)   ,PIPE-1, 
                 WS-BYA-AMT-PL(1,1,6)   ,PIPE-1, 
                 WS-BYA-AMT-PL(1,1,7)   ,PIPE-1, 
                 WS-BYA-AMT-PL(1,1,8)   ,PIPE-1, 
                 WS-BYA-AMT-PL(1,1,9)   ,PIPE-1, 
                 WS-BYA-AMT-PL(1,1,10)  ,PIPE-1, 
                 WS-BYA-AMT-PL(1,2,1)   ,PIPE-1, 
                 WS-BYA-AMT-PL(1,2,2)   ,PIPE-1, 
                 WS-BYA-AMT-PL(1,2,3)   ,PIPE-1, 
                 WS-BYA-AMT-PL(1,2,4)   ,PIPE-1, 
                 WS-BYA-AMT-PL(1,2,5)   ,PIPE-1, 
                 WS-BYA-AMT-PL(1,2,6)   ,PIPE-1, 
                 WS-BYA-AMT-PL(1,2,7)   ,PIPE-1, 
                 WS-BYA-AMT-PL(1,2,8)   ,PIPE-1, 
                 WS-BYA-AMT-PL(1,2,9)   ,PIPE-1, 
                 WS-BYA-AMT-PL(1,2,10)  ,PIPE-1, 
                 WS-BYA-AMT-PL(2,1,1)   ,PIPE-1, 
                 WS-BYA-AMT-PL(2,1,2)   ,PIPE-1, 
                 WS-BYA-AMT-PL(2,1,3)   ,PIPE-1, 
                 WS-BYA-AMT-PL(2,1,4)   ,PIPE-1, 
                 WS-BYA-AMT-PL(2,1,5)   ,PIPE-1, 
                 WS-BYA-AMT-PL(2,1,6)   ,PIPE-1, 
                 WS-BYA-AMT-PL(2,1,7)   ,PIPE-1, 
                 WS-BYA-AMT-PL(2,1,8)   ,PIPE-1, 
                 WS-BYA-AMT-PL(2,1,9)   ,PIPE-1, 
                 WS-BYA-AMT-PL(2,1,10)  ,PIPE-1, 
                 WS-BYA-AMT-PL(2,2,1)   ,PIPE-1, 
                 WS-BYA-AMT-PL(2,2,2)   ,PIPE-1, 
                 WS-BYA-AMT-PL(2,2,3)   ,PIPE-1, 
                 WS-BYA-AMT-PL(2,2,4)   ,PIPE-1, 
                 WS-BYA-AMT-PL(2,2,5)   ,PIPE-1, 
                 WS-BYA-AMT-PL(2,2,6)   ,PIPE-1, 
                 WS-BYA-AMT-PL(2,2,7)   ,PIPE-1, 
                 WS-BYA-AMT-PL(2,2,8)   ,PIPE-1, 
                 WS-BYA-AMT-PL(2,2,9)   ,PIPE-1, 
                 WS-BYA-AMT-PL(2,2,10)  ,PIPE-1, 
                 WS-BYA-AMT-PL(3,1,1)   ,PIPE-1, 
                 WS-BYA-AMT-PL(3,1,2)   ,PIPE-1, 
                 WS-BYA-AMT-PL(3,1,3)   ,PIPE-1, 
                 WS-BYA-AMT-PL(3,1,4)   ,PIPE-1, 
                 WS-BYA-AMT-PL(3,1,5)   ,PIPE-1, 
                 WS-BYA-AMT-PL(3,1,6)   ,PIPE-1, 
                 WS-BYA-AMT-PL(3,1,7)   ,PIPE-1, 
                 WS-BYA-AMT-PL(3,1,8)   ,PIPE-1, 
                 WS-BYA-AMT-PL(3,1,9)   ,PIPE-1, 
                 WS-BYA-AMT-PL(3,1,10)  ,PIPE-1, 
                 WS-BYA-AMT-PL(3,2,1)   ,PIPE-1, 
                 WS-BYA-AMT-PL(3,2,2)   ,PIPE-1, 
                 WS-BYA-AMT-PL(3,2,3)   ,PIPE-1, 
                 WS-BYA-AMT-PL(3,2,4)   ,PIPE-1, 
                 WS-BYA-AMT-PL(3,2,5)   ,PIPE-1, 
                 WS-BYA-AMT-PL(3,2,6)   ,PIPE-1, 
                 WS-BYA-AMT-PL(3,2,7)   ,PIPE-1, 
                 WS-BYA-AMT-PL(3,2,8)   ,PIPE-1, 
                 WS-BYA-AMT-PL(3,2,9)   ,PIPE-1, 
                 WS-BYA-AMT-PL(3,2,10)  ,PIPE-1 
              DELIMITED BY SIZE INTO WS-MISC-STRING(5).

           STRING WS-BYA-O2-PL(1,1,1) ,PIPE-1, 
             WS-BYA-O2-PL(1,1,2)      ,PIPE-1, 
             WS-BYA-O2-PL(1,1,3)      ,PIPE-1, 
             WS-BYA-O2-PL(1,1,4)      ,PIPE-1, 
             WS-BYA-O2-PL(1,1,5)      ,PIPE-1, 
             WS-BYA-O2-PL(1,1,6)      ,PIPE-1, 
             WS-BYA-O2-PL(1,1,7)      ,PIPE-1, 
             WS-BYA-O2-PL(1,1,8)      ,PIPE-1, 
             WS-BYA-O2-PL(1,1,9)      ,PIPE-1, 
             WS-BYA-O2-PL(1,1,10)     ,PIPE-1, 
             WS-BYA-O2-PL(1,2,1)      ,PIPE-1, 
             WS-BYA-O2-PL(1,2,2)      ,PIPE-1, 
             WS-BYA-O2-PL(1,2,3)      ,PIPE-1, 
             WS-BYA-O2-PL(1,2,4)      ,PIPE-1, 
             WS-BYA-O2-PL(1,2,5)      ,PIPE-1, 
             WS-BYA-O2-PL(1,2,6)      ,PIPE-1, 
             WS-BYA-O2-PL(1,2,7)      ,PIPE-1, 
             WS-BYA-O2-PL(1,2,8)      ,PIPE-1, 
             WS-BYA-O2-PL(1,2,9)      ,PIPE-1, 
             WS-BYA-O2-PL(1,2,10)     ,PIPE-1, 
             WS-BYA-O2-PL(2,1,1)      ,PIPE-1, 
             WS-BYA-O2-PL(2,1,2)      ,PIPE-1, 
             WS-BYA-O2-PL(2,1,3)      ,PIPE-1, 
             WS-BYA-O2-PL(2,1,4)      ,PIPE-1, 
             WS-BYA-O2-PL(2,1,5)      ,PIPE-1, 
             WS-BYA-O2-PL(2,1,6)      ,PIPE-1, 
             WS-BYA-O2-PL(2,1,7)      ,PIPE-1, 
             WS-BYA-O2-PL(2,1,8)      ,PIPE-1, 
             WS-BYA-O2-PL(2,1,9)      ,PIPE-1, 
             WS-BYA-O2-PL(2,1,10)     ,PIPE-1, 
             WS-BYA-O2-PL(2,2,1)      ,PIPE-1, 
             WS-BYA-O2-PL(2,2,2)      ,PIPE-1, 
             WS-BYA-O2-PL(2,2,3)      ,PIPE-1, 
             WS-BYA-O2-PL(2,2,4)      ,PIPE-1, 
             WS-BYA-O2-PL(2,2,5)      ,PIPE-1, 
             WS-BYA-O2-PL(2,2,6)      ,PIPE-1, 
             WS-BYA-O2-PL(2,2,7)      ,PIPE-1, 
             WS-BYA-O2-PL(2,2,8)      ,PIPE-1, 
             WS-BYA-O2-PL(2,2,9)      ,PIPE-1, 
             WS-BYA-O2-PL(2,2,10)     ,PIPE-1, 
             WS-BYA-O2-PL(3,1,1)      ,PIPE-1, 
             WS-BYA-O2-PL(3,1,2)      ,PIPE-1, 
             WS-BYA-O2-PL(3,1,3)      ,PIPE-1, 
             WS-BYA-O2-PL(3,1,4)      ,PIPE-1, 
             WS-BYA-O2-PL(3,1,5)      ,PIPE-1, 
             WS-BYA-O2-PL(3,1,6)      ,PIPE-1, 
             WS-BYA-O2-PL(3,1,7)      ,PIPE-1, 
             WS-BYA-O2-PL(3,1,8)      ,PIPE-1, 
             WS-BYA-O2-PL(3,1,9)      ,PIPE-1, 
             WS-BYA-O2-PL(3,1,10)     ,PIPE-1, 
             WS-BYA-O2-PL(3,2,1)      ,PIPE-1, 
             WS-BYA-O2-PL(3,2,2)      ,PIPE-1, 
             WS-BYA-O2-PL(3,2,3)      ,PIPE-1,  
             WS-BYA-O2-PL(3,2,4)      ,PIPE-1, 
             WS-BYA-O2-PL(3,2,5)      ,PIPE-1, 
             WS-BYA-O2-PL(3,2,6)      ,PIPE-1, 
             WS-BYA-O2-PL(3,2,7)      ,PIPE-1, 
             WS-BYA-O2-PL(3,2,8)      ,PIPE-1, 
             WS-BYA-O2-PL(3,2,9)      ,PIPE-1, 
             WS-BYA-O2-PL(3,2,10)     ,PIPE-1, 
             WS-BYA-INT-PL(1,1,1)     ,PIPE-1, 
             WS-BYA-INT-PL(1,1,2)     ,PIPE-1, 
             WS-BYA-INT-PL(1,1,3)     ,PIPE-1, 
             WS-BYA-INT-PL(1,1,4)     ,PIPE-1, 
             WS-BYA-INT-PL(1,1,5)     ,PIPE-1, 
             WS-BYA-INT-PL(1,1,6)     ,PIPE-1, 
             WS-BYA-INT-PL(1,1,7)     ,PIPE-1, 
             WS-BYA-INT-PL(1,1,8)     ,PIPE-1, 
             WS-BYA-INT-PL(1,1,9)     ,PIPE-1, 
             WS-BYA-INT-PL(1,1,10)    ,PIPE-1, 
             WS-BYA-INT-PL(1,2,1)     ,PIPE-1, 
             WS-BYA-INT-PL(1,2,2)     ,PIPE-1, 
             WS-BYA-INT-PL(1,2,3)     ,PIPE-1, 
             WS-BYA-INT-PL(1,2,4)     ,PIPE-1, 
             WS-BYA-INT-PL(1,2,5)     ,PIPE-1, 
             WS-BYA-INT-PL(1,2,6)     ,PIPE-1, 
             WS-BYA-INT-PL(1,2,7)     ,PIPE-1, 
             WS-BYA-INT-PL(1,2,8)     ,PIPE-1, 
             WS-BYA-INT-PL(1,2,9)     ,PIPE-1, 
             WS-BYA-INT-PL(1,2,10)    ,PIPE-1, 
             WS-BYA-INT-PL(2,1,1)     ,PIPE-1, 
             WS-BYA-INT-PL(2,1,2)     ,PIPE-1, 
             WS-BYA-INT-PL(2,1,3)     ,PIPE-1, 
             WS-BYA-INT-PL(2,1,4)     ,PIPE-1, 
             WS-BYA-INT-PL(2,1,5)     ,PIPE-1, 
             WS-BYA-INT-PL(2,1,6)     ,PIPE-1, 
             WS-BYA-INT-PL(2,1,7)     ,PIPE-1, 
             WS-BYA-INT-PL(2,1,8)     ,PIPE-1, 
             WS-BYA-INT-PL(2,1,9)     ,PIPE-1, 
             WS-BYA-INT-PL(2,1,10)    ,PIPE-1, 
             WS-BYA-INT-PL(2,2,1)     ,PIPE-1, 
             WS-BYA-INT-PL(2,2,2)     ,PIPE-1, 
             WS-BYA-INT-PL(2,2,3)     ,PIPE-1, 
             WS-BYA-INT-PL(2,2,4)     ,PIPE-1, 
             WS-BYA-INT-PL(2,2,5)     ,PIPE-1, 
             WS-BYA-INT-PL(2,2,6)     ,PIPE-1, 
             WS-BYA-INT-PL(2,2,7)     ,PIPE-1, 
             WS-BYA-INT-PL(2,2,8)     ,PIPE-1, 
             WS-BYA-INT-PL(2,2,9)     ,PIPE-1, 
             WS-BYA-INT-PL(2,2,10)    ,PIPE-1, 
             WS-BYA-INT-PL(3,1,1)     ,PIPE-1, 
             WS-BYA-INT-PL(3,1,2)     ,PIPE-1, 
             WS-BYA-INT-PL(3,1,3)     ,PIPE-1, 
             WS-BYA-INT-PL(3,1,4)     ,PIPE-1, 
             WS-BYA-INT-PL(3,1,5)     ,PIPE-1, 
             WS-BYA-INT-PL(3,1,6)     ,PIPE-1, 
             WS-BYA-INT-PL(3,1,7)     ,PIPE-1, 
             WS-BYA-INT-PL(3,1,8)     ,PIPE-1, 
             WS-BYA-INT-PL(3,1,9)     ,PIPE-1, 
             WS-BYA-INT-PL(3,1,10)    ,PIPE-1, 
             WS-BYA-INT-PL(3,2,1)     ,PIPE-1, 
             WS-BYA-INT-PL(3,2,2)     ,PIPE-1, 
             WS-BYA-INT-PL(3,2,3)     ,PIPE-1, 
             WS-BYA-INT-PL(3,2,4)     ,PIPE-1, 
             WS-BYA-INT-PL(3,2,5)     ,PIPE-1, 
             WS-BYA-INT-PL(3,2,6)     ,PIPE-1, 
             WS-BYA-INT-PL(3,2,7)     ,PIPE-1, 
             WS-BYA-INT-PL(3,2,8)     ,PIPE-1, 
             WS-BYA-INT-PL(3,2,9)     ,PIPE-1, 
             WS-BYA-INT-PL(3,2,10)    ,PIPE-1 
              DELIMITED BY SIZE INTO WS-MISC-STRING(6).

           STRING WS-BYA-DLRVPRIN(1,1,1),PIPE-1, 
             WS-BYA-DLRVPRIN(1,1,2)     ,PIPE-1, 
             WS-BYA-DLRVPRIN(1,1,3)     ,PIPE-1, 
             WS-BYA-DLRVPRIN(1,2,1)     ,PIPE-1, 
             WS-BYA-DLRVPRIN(1,2,2)     ,PIPE-1, 
             WS-BYA-DLRVPRIN(1,2,3)     ,PIPE-1, 
             WS-BYA-DLRVPRIN(2,1,1)     ,PIPE-1, 
             WS-BYA-DLRVPRIN(2,1,2)     ,PIPE-1, 
             WS-BYA-DLRVPRIN(2,1,3)     ,PIPE-1, 
             WS-BYA-DLRVPRIN(2,2,1)     ,PIPE-1, 
             WS-BYA-DLRVPRIN(2,2,2)     ,PIPE-1, 
             WS-BYA-DLRVPRIN(2,2,3)     ,PIPE-1, 
             WS-BYA-DLRVPRIN(3,1,1)     ,PIPE-1, 
             WS-BYA-DLRVPRIN(3,1,2)     ,PIPE-1, 
             WS-BYA-DLRVPRIN(3,1,3)     ,PIPE-1, 
             WS-BYA-DLRVPRIN(3,2,1)     ,PIPE-1, 
             WS-BYA-DLRVPRIN(3,2,2)     ,PIPE-1, 
             WS-BYA-DLRVPRIN(3,2,3)     ,PIPE-1, 
             WS-BYA-DLRVCHGS(1,1,1)     ,PIPE-1, 
             WS-BYA-DLRVCHGS(1,1,2)     ,PIPE-1, 
             WS-BYA-DLRVCHGS(1,1,3)     ,PIPE-1, 
             WS-BYA-DLRVCHGS(1,2,1)     ,PIPE-1, 
             WS-BYA-DLRVCHGS(1,2,2)     ,PIPE-1, 
             WS-BYA-DLRVCHGS(1,2,3)     ,PIPE-1, 
             WS-BYA-DLRVCHGS(2,1,1)     ,PIPE-1, 
             WS-BYA-DLRVCHGS(2,1,2)     ,PIPE-1, 
             WS-BYA-DLRVCHGS(2,1,3)     ,PIPE-1, 
             WS-BYA-DLRVCHGS(2,2,1)     ,PIPE-1, 
             WS-BYA-DLRVCHGS(2,2,2)     ,PIPE-1, 
             WS-BYA-DLRVCHGS(2,2,3)     ,PIPE-1, 
             WS-BYA-DLRVCHGS(3,1,1)     ,PIPE-1, 
             WS-BYA-DLRVCHGS(3,1,2)     ,PIPE-1, 
             WS-BYA-DLRVCHGS(3,1,3)     ,PIPE-1, 
             WS-BYA-DLRVCHGS(3,2,1)     ,PIPE-1, 
             WS-BYA-DLRVCHGS(3,2,2)     ,PIPE-1, 
             WS-BYA-DLRVCHGS(3,2,3)     ,PIPE-1, 
             WS-BYA-DLPARVHELD(1,1)     ,PIPE-1, 
             WS-BYA-DLPARVHELD(1,2)     ,PIPE-1, 
             WS-BYA-DLPARVHELD(2,1)     ,PIPE-1, 
             WS-BYA-DLPARVHELD(2,2)     ,PIPE-1, 
             WS-BYA-DLPARVHELD(3,1)     ,PIPE-1, 
             WS-BYA-DLPARVHELD(3,2) 
              DELIMITED BY SIZE INTO WS-MISC-STRING(7).

           STRING WS-MISC-STRING(1), WS-MISC-STRING(2),
                  WS-MISC-STRING(3), WS-MISC-STRING(4),
                  WS-MISC-STRING(5), WS-MISC-STRING(6),
                  WS-MISC-STRING(7)
             DELIMITED BY ' ' INTO BYADATA-OUT-REC.

           INSPECT BYADATA-OUT-REC REPLACING ALL "+" BY " ".

       CREATE-ASCII-BYB-STRING.
           MOVE SPACES TO BYBDATA-OUT-REC
                          WS-MISC-STRING(1)
                          WS-MISC-STRING(2)
                          WS-MISC-STRING(3)
                          WS-MISC-STRING(4)
                          WS-MISC-STRING(5)
                          WS-MISC-STRING(6).

           STRING BYB-DATE-CCYYMM        ,PIPE-1,
                  BYB-BRNO               ,PIPE-1,  
                  BYB-CLASS              ,PIPE-1,  
             WS-BYB-PL-PRINCOL(1,1)      ,PIPE-1, 
             WS-BYB-PL-PRINCOL(1,2)      ,PIPE-1, 
             WS-BYB-PL-PRINCOL(1,3)      ,PIPE-1, 
             WS-BYB-PL-PRINCOL(1,4)      ,PIPE-1, 
             WS-BYB-PL-PRINCOL(1,5)      ,PIPE-1,
             WS-BYB-PL-PRINCOL(2,1)      ,PIPE-1, 
             WS-BYB-PL-PRINCOL(2,2)      ,PIPE-1, 
             WS-BYB-PL-PRINCOL(2,3)      ,PIPE-1, 
             WS-BYB-PL-PRINCOL(2,4)      ,PIPE-1, 
             WS-BYB-PL-PRINCOL(2,5)      ,PIPE-1,
             WS-BYB-PL-INTCOL(1,1)       ,PIPE-1, 
             WS-BYB-PL-INTCOL(1,2)       ,PIPE-1, 
             WS-BYB-PL-INTCOL(1,3)       ,PIPE-1, 
             WS-BYB-PL-INTCOL(1,4)       ,PIPE-1, 
             WS-BYB-PL-INTCOL(1,5)       ,PIPE-1,
             WS-BYB-PL-INTCOL(2,1)       ,PIPE-1, 
             WS-BYB-PL-INTCOL(2,2)       ,PIPE-1, 
             WS-BYB-PL-INTCOL(2,3)       ,PIPE-1, 
             WS-BYB-PL-INTCOL(2,4)       ,PIPE-1, 
             WS-BYB-PL-INTCOL(2,5)       ,PIPE-1,
             WS-BYB-PL-LATECOL(1,1)      ,PIPE-1, 
             WS-BYB-PL-LATECOL(1,2)      ,PIPE-1, 
             WS-BYB-PL-LATECOL(1,3)      ,PIPE-1, 
             WS-BYB-PL-LATECOL(1,4)      ,PIPE-1, 
             WS-BYB-PL-LATECOL(1,5)      ,PIPE-1, 
             WS-BYB-PL-LATECOL(2,1)      ,PIPE-1, 
             WS-BYB-PL-LATECOL(2,2)      ,PIPE-1, 
             WS-BYB-PL-LATECOL(2,3)      ,PIPE-1, 
             WS-BYB-PL-LATECOL(2,4)      ,PIPE-1, 
             WS-BYB-PL-LATECOL(2,5)      ,PIPE-1, 
             WS-BYB-PL-O2COL(1,1)        ,PIPE-1, 
             WS-BYB-PL-O2COL(1,2)        ,PIPE-1, 
             WS-BYB-PL-O2COL(1,3)        ,PIPE-1, 
             WS-BYB-PL-O2COL(1,4)        ,PIPE-1, 
             WS-BYB-PL-O2COL(1,5)        ,PIPE-1, 
             WS-BYB-PL-O2COL(2,1)        ,PIPE-1, 
             WS-BYB-PL-O2COL(2,2)        ,PIPE-1, 
             WS-BYB-PL-O2COL(2,3)        ,PIPE-1, 
             WS-BYB-PL-O2COL(2,4)        ,PIPE-1, 
             WS-BYB-PL-O2COL(2,5)        ,PIPE-1, 
             WS-BYB-AMT-SOLD(1)          ,PIPE-1, 
             WS-BYB-AMT-SOLD(2)          ,PIPE-1, 
             WS-BYB-AMT-TRANS(1)         ,PIPE-1, 
             WS-BYB-AMT-TRANS(2)         ,PIPE-1, 
             WS-BYB-AMT-PURCH(1)         ,PIPE-1, 
             WS-BYB-AMT-PURCH(2)         ,PIPE-1, 
             WS-BYB-NO-SOLD(1)           ,PIPE-1, 
             WS-BYB-NO-SOLD(2)           ,PIPE-1, 
             WS-BYB-NO-TRANS(1)          ,PIPE-1, 
             WS-BYB-NO-TRANS(2)          ,PIPE-1,
             WS-BYB-NO-PURCH(1)          ,PIPE-1, 
             WS-BYB-NO-PURCH(2)          ,PIPE-1, 
             WS-BYB-OTHCOL(1)            ,PIPE-1, 
             WS-BYB-OTHCOL(2)            ,PIPE-1, 
             WS-BYB-OT2COL(1)            ,PIPE-1, 
             WS-BYB-OT2COL(2)            ,PIPE-1, 
             WS-BYB-INTCOL(1)            ,PIPE-1, 
             WS-BYB-INTCOL(2)            ,PIPE-1, 
             WS-BYB-LATECOL(1)           ,PIPE-1, 
             WS-BYB-LATECOL(2)           ,PIPE-1, 
             WS-BYB-PRINCOL(1)           ,PIPE-1, 
             WS-BYB-PRINCOL(2)           ,PIPE-1, 
             WS-BYB-DEFCOL(1)            ,PIPE-1, 
             WS-BYB-DEFCOL(2)            ,PIPE-1, 
             WS-BYB-RNPRIN(1)            ,PIPE-1, 
             WS-BYB-RNPRIN(2)            ,PIPE-1, 
             WS-BYB-INTREB(1)            ,PIPE-1, 
             WS-BYB-INTREB(2)            ,PIPE-1, 
             WS-BYB-CLREB1(1)            ,PIPE-1, 
             WS-BYB-CLREB1(2)            ,PIPE-1, 
             WS-BYB-CLREB2(1)            ,PIPE-1, 
             WS-BYB-CLREB2(2)            ,PIPE-1, 
             WS-BYB-CLREB3(1)            ,PIPE-1, 
             WS-BYB-CLREB3(2)            ,PIPE-1, 
             WS-BYB-CLREB4(1)            ,PIPE-1, 
             WS-BYB-CLREB4(2)            ,PIPE-1, 
             WS-BYB-AHREB(1)             ,PIPE-1, 
             WS-BYB-AHREB(2)             ,PIPE-1, 
             WS-BYB-PPREB(1)             ,PIPE-1, 
             WS-BYB-PPREB(2)             ,PIPE-1, 
             WS-BYB-O1REB(1)             ,PIPE-1, 
             WS-BYB-O1REB(2)             ,PIPE-1, 
             WS-BYB-O2REB(1)             ,PIPE-1, 
             WS-BYB-O2REB(2)             ,PIPE-1, 
             WS-BYB-O3REB(1)             ,PIPE-1, 
             WS-BYB-O3REB(2)             ,PIPE-1, 
             WS-BYB-O4REB(1)             ,PIPE-1, 
             WS-BYB-O4REB(2)             ,PIPE-1, 
             WS-BYB-O5REB(1)             ,PIPE-1, 
             WS-BYB-O5REB(2)             ,PIPE-1, 
             WS-BYB-O6REB(1)             ,PIPE-1, 
             WS-BYB-O6REB(2)             ,PIPE-1, 
             WS-BYB-O7REB(1)             ,PIPE-1, 
             WS-BYB-O7REB(2)             ,PIPE-1, 
             WS-BYB-SCREB(1)             ,PIPE-1, 
             WS-BYB-SCREB(2)             ,PIPE-1, 
             WS-BYB-MFREB(1)             ,PIPE-1, 
             WS-BYB-MFREB(2)             ,PIPE-1, 
             WS-BYB-DFREB(1)             ,PIPE-1 
              DELIMITED BY SIZE INTO WS-MISC-STRING(1).

            STRING WS-BYB-DFREB(2)      ,PIPE-1, 
             WS-BYB-UN7-REB(1)          ,PIPE-1, 
             WS-BYB-UN7-REB(2)          ,PIPE-1, 
             WS-BYB-UN8-REB(1)          ,PIPE-1, 
             WS-BYB-UN8-REB(2)          ,PIPE-1, 
             WS-BYB-UN9-REB(1)          ,PIPE-1, 
             WS-BYB-UN9-REB(2)          ,PIPE-1, 
             WS-BYB-UN10-REB(1)         ,PIPE-1, 
             WS-BYB-UN10-REB(2)         ,PIPE-1, 
             WS-BYB-PART-CHGOFF(1)      ,PIPE-1, 
             WS-BYB-PART-CHGOFF(2)      ,PIPE-1, 
             WS-BYB-ALLOTMENTS(1)       ,PIPE-1, 
             WS-BYB-ALLOTMENTS(2)       ,PIPE-1, 
             WS-BYB-INS-NO-ELIG(1,1)    ,PIPE-1, 
             WS-BYB-INS-NO-ELIG(1,2)    ,PIPE-1, 
             WS-BYB-INS-NO-ELIG(1,3)    ,PIPE-1, 
             WS-BYB-INS-NO-ELIG(1,4)    ,PIPE-1, 
             WS-BYB-INS-NO-ELIG(1,5)    ,PIPE-1, 
             WS-BYB-INS-NO-ELIG(1,6)    ,PIPE-1, 
             WS-BYB-INS-NO-ELIG(1,7)    ,PIPE-1, 
             WS-BYB-INS-NO-ELIG(1,8)    ,PIPE-1, 
             WS-BYB-INS-NO-ELIG(1,9)    ,PIPE-1, 
             WS-BYB-INS-NO-ELIG(1,10)   ,PIPE-1, 
             WS-BYB-INS-NO-ELIG(2,1)    ,PIPE-1, 
             WS-BYB-INS-NO-ELIG(2,2)    ,PIPE-1, 
             WS-BYB-INS-NO-ELIG(2,3)    ,PIPE-1, 
             WS-BYB-INS-NO-ELIG(2,4)    ,PIPE-1, 
             WS-BYB-INS-NO-ELIG(2,5)    ,PIPE-1, 
             WS-BYB-INS-NO-ELIG(2,6)    ,PIPE-1, 
             WS-BYB-INS-NO-ELIG(2,7)    ,PIPE-1, 
             WS-BYB-INS-NO-ELIG(2,8)    ,PIPE-1, 
             WS-BYB-INS-NO-ELIG(2,9)    ,PIPE-1, 
             WS-BYB-INS-NO-ELIG(2,10)   ,PIPE-1, 
             WS-BYB-INS-NO-WROTE(1,1)    ,PIPE-1, 
             WS-BYB-INS-NO-WROTE(1,2)    ,PIPE-1, 
             WS-BYB-INS-NO-WROTE(1,3)    ,PIPE-1, 
             WS-BYB-INS-NO-WROTE(1,4)    ,PIPE-1, 
             WS-BYB-INS-NO-WROTE(1,5)    ,PIPE-1, 
             WS-BYB-INS-NO-WROTE(1,6)    ,PIPE-1, 
             WS-BYB-INS-NO-WROTE(1,7)    ,PIPE-1, 
             WS-BYB-INS-NO-WROTE(1,8)    ,PIPE-1, 
             WS-BYB-INS-NO-WROTE(1,9)    ,PIPE-1, 
             WS-BYB-INS-NO-WROTE(1,10)   ,PIPE-1, 
             WS-BYB-INS-NO-WROTE(2,1)    ,PIPE-1, 
             WS-BYB-INS-NO-WROTE(2,2)    ,PIPE-1, 
             WS-BYB-INS-NO-WROTE(2,3)    ,PIPE-1, 
             WS-BYB-INS-NO-WROTE(2,4)    ,PIPE-1, 
             WS-BYB-INS-NO-WROTE(2,5)    ,PIPE-1, 
             WS-BYB-INS-NO-WROTE(2,6)    ,PIPE-1
             WS-BYB-INS-NO-WROTE(2,7)    ,PIPE-1, 
             WS-BYB-INS-NO-WROTE(2,8)    ,PIPE-1, 
             WS-BYB-INS-NO-WROTE(2,9)    ,PIPE-1, 
             WS-BYB-INS-NO-WROTE(2,10)   ,PIPE-1, 
             WS-BYB-INS-NO-NOTWROTE(1,1) ,PIPE-1, 
             WS-BYB-INS-NO-NOTWROTE(1,2) ,PIPE-1, 
             WS-BYB-INS-NO-NOTWROTE(1,3) ,PIPE-1, 
             WS-BYB-INS-NO-NOTWROTE(1,4) ,PIPE-1, 
             WS-BYB-INS-NO-NOTWROTE(1,5) ,PIPE-1, 
             WS-BYB-INS-NO-NOTWROTE(1,6) ,PIPE-1, 
             WS-BYB-INS-NO-NOTWROTE(1,7) ,PIPE-1, 
             WS-BYB-INS-NO-NOTWROTE(1,8) ,PIPE-1, 
             WS-BYB-INS-NO-NOTWROTE(1,9) ,PIPE-1, 
             WS-BYB-INS-NO-NOTWROTE(1,10),PIPE-1, 
             WS-BYB-INS-NO-NOTWROTE(2,1) ,PIPE-1, 
             WS-BYB-INS-NO-NOTWROTE(2,2) ,PIPE-1, 
             WS-BYB-INS-NO-NOTWROTE(2,3) ,PIPE-1, 
             WS-BYB-INS-NO-NOTWROTE(2,4) ,PIPE-1, 
             WS-BYB-INS-NO-NOTWROTE(2,5) ,PIPE-1, 
             WS-BYB-INS-NO-NOTWROTE(2,6) ,PIPE-1, 
             WS-BYB-INS-NO-NOTWROTE(2,7) ,PIPE-1, 
             WS-BYB-INS-NO-NOTWROTE(2,8) ,PIPE-1, 
             WS-BYB-INS-NO-NOTWROTE(2,9) ,PIPE-1, 
             WS-BYB-INS-NO-NOTWROTE(2,10) ,PIPE-1, 
             WS-BYB-INS-AMT-WROTE(1,1)    ,PIPE-1, 
             WS-BYB-INS-AMT-WROTE(1,2)    ,PIPE-1, 
             WS-BYB-INS-AMT-WROTE(1,3)    ,PIPE-1, 
             WS-BYB-INS-AMT-WROTE(1,4)    ,PIPE-1, 
             WS-BYB-INS-AMT-WROTE(1,5)    ,PIPE-1, 
             WS-BYB-INS-AMT-WROTE(1,6)    ,PIPE-1, 
             WS-BYB-INS-AMT-WROTE(1,7)    ,PIPE-1, 
             WS-BYB-INS-AMT-WROTE(1,8)    ,PIPE-1, 
             WS-BYB-INS-AMT-WROTE(1,9)    ,PIPE-1, 
             WS-BYB-INS-AMT-WROTE(1,10)   ,PIPE-1, 
             WS-BYB-INS-AMT-WROTE(2,1)    ,PIPE-1, 
             WS-BYB-INS-AMT-WROTE(2,2)    ,PIPE-1, 
             WS-BYB-INS-AMT-WROTE(2,3)    ,PIPE-1, 
             WS-BYB-INS-AMT-WROTE(2,4)    ,PIPE-1, 
             WS-BYB-INS-AMT-WROTE(2,5)    ,PIPE-1, 
             WS-BYB-INS-AMT-WROTE(2,6)    ,PIPE-1, 
             WS-BYB-INS-AMT-WROTE(2,7)    ,PIPE-1, 
             WS-BYB-INS-AMT-WROTE(2,8)    ,PIPE-1, 
             WS-BYB-INS-AMT-WROTE(2,9)    ,PIPE-1, 
             WS-BYB-INS-AMT-WROTE(2,10)   ,PIPE-1, 
             WS-BYB-FEE-NO-ELIG(1,1)      ,PIPE-1, 
             WS-BYB-FEE-NO-ELIG(1,2)      ,PIPE-1, 
             WS-BYB-FEE-NO-ELIG(1,3)      ,PIPE-1, 
             WS-BYB-FEE-NO-ELIG(2,1)      ,PIPE-1, 
             WS-BYB-FEE-NO-ELIG(2,2)      ,PIPE-1, 
             WS-BYB-FEE-NO-ELIG(2,3)      ,PIPE-1, 
             WS-BYB-FEE-NO-WROTE(1,1)     ,PIPE-1, 
             WS-BYB-FEE-NO-WROTE(1,2)     ,PIPE-1 
              DELIMITED BY SIZE INTO WS-MISC-STRING(2).

           STRING WS-BYB-FEE-NO-WROTE(1,3) ,PIPE-1, 
             WS-BYB-FEE-NO-WROTE(2,1)      ,PIPE-1, 
             WS-BYB-FEE-NO-WROTE(2,2)      ,PIPE-1, 
             WS-BYB-FEE-NO-WROTE(2,3)      ,PIPE-1, 
             WS-BYB-FEE-NO-NOTWROTE(1,1)   ,PIPE-1, 
             WS-BYB-FEE-NO-NOTWROTE(1,2)   ,PIPE-1, 
             WS-BYB-FEE-NO-NOTWROTE(1,3)   ,PIPE-1, 
             WS-BYB-FEE-NO-NOTWROTE(2,1)   ,PIPE-1, 
             WS-BYB-FEE-NO-NOTWROTE(2,2)   ,PIPE-1, 
             WS-BYB-FEE-NO-NOTWROTE(2,3)   ,PIPE-1, 
             WS-BYB-FEE-AMT-WROTE(1,1)     ,PIPE-1, 
             WS-BYB-FEE-AMT-WROTE(1,2)     ,PIPE-1, 
             WS-BYB-FEE-AMT-WROTE(1,3)     ,PIPE-1, 
             WS-BYB-FEE-AMT-WROTE(2,1)     ,PIPE-1, 
             WS-BYB-FEE-AMT-WROTE(2,2)     ,PIPE-1, 
             WS-BYB-FEE-AMT-WROTE(2,3)     ,PIPE-1, 
             WS-BYB-OTH-NO-COL(1)          ,PIPE-1, 
             WS-BYB-OTH-NO-COL(2)          ,PIPE-1, 
             WS-BYB-OT2-NO-COL(1)          ,PIPE-1, 
             WS-BYB-OT2-NO-COL(2)          ,PIPE-1, 
             WS-BYB-INT-NO-COL(1)          ,PIPE-1, 
             WS-BYB-INT-NO-COL(2)          ,PIPE-1, 
             WS-BYB-LATE-NO-COL(1)         ,PIPE-1, 
             WS-BYB-LATE-NO-COL(2)         ,PIPE-1, 
             WS-BYB-OTH-NO-WAIVED(1)       ,PIPE-1, 
             WS-BYB-OTH-NO-WAIVED(2)       ,PIPE-1, 
             WS-BYB-OT2-NO-WAIVED(1)       ,PIPE-1, 
             WS-BYB-OT2-NO-WAIVED(2)       ,PIPE-1, 
             WS-BYB-INT-NO-WAIVED(1)       ,PIPE-1, 
             WS-BYB-INT-NO-WAIVED(2)       ,PIPE-1, 
             WS-BYB-LATE-NO-WAIVED(1)      ,PIPE-1, 
             WS-BYB-LATE-NO-WAIVED(2)      ,PIPE-1, 
             WS-BYB-OTH-AMT-WAIVED(1)      ,PIPE-1, 
             WS-BYB-OTH-AMT-WAIVED(2)      ,PIPE-1, 
             WS-BYB-OT2-AMT-WAIVED(1)      ,PIPE-1, 
             WS-BYB-OT2-AMT-WAIVED(2)      ,PIPE-1, 
             WS-BYB-INT-AMT-WAIVED(1)      ,PIPE-1, 
             WS-BYB-INT-AMT-WAIVED(2)      ,PIPE-1, 
             WS-BYB-LATE-AMT-WAIVED(1)     ,PIPE-1, 
             WS-BYB-LATE-AMT-WAIVED(2)     ,PIPE-1, 
             WS-BYB-UNITS(1,1,1)           ,PIPE-1, 
             WS-BYB-UNITS(1,1,2)           ,PIPE-1, 
             WS-BYB-UNITS(1,1,3)           ,PIPE-1, 
             WS-BYB-UNITS(1,1,4)           ,PIPE-1, 
             WS-BYB-UNITS(1,1,5)           ,PIPE-1, 
             WS-BYB-UNITS(1,1,6)           ,PIPE-1, 
             WS-BYB-UNITS(1,1,7)           ,PIPE-1, 
             WS-BYB-UNITS(1,1,8)           ,PIPE-1, 
             WS-BYB-UNITS(1,1,9)           ,PIPE-1, 
             WS-BYB-UNITS(1,1,10)          ,PIPE-1, 
             WS-BYB-UNITS(1,2,1)           ,PIPE-1, 
             WS-BYB-UNITS(1,2,2)           ,PIPE-1, 
             WS-BYB-UNITS(1,2,3)           ,PIPE-1, 
             WS-BYB-UNITS(1,2,4)           ,PIPE-1, 
             WS-BYB-UNITS(1,2,5)           ,PIPE-1, 
             WS-BYB-UNITS(1,2,6)           ,PIPE-1, 
             WS-BYB-UNITS(1,2,7)           ,PIPE-1, 
             WS-BYB-UNITS(1,2,8)           ,PIPE-1, 
             WS-BYB-UNITS(1,2,9)           ,PIPE-1, 
             WS-BYB-UNITS(1,2,10)          ,PIPE-1, 
             WS-BYB-UNITS(2,1,1)           ,PIPE-1, 
             WS-BYB-UNITS(2,1,2)           ,PIPE-1, 
             WS-BYB-UNITS(2,1,3)           ,PIPE-1, 
             WS-BYB-UNITS(2,1,4)           ,PIPE-1, 
             WS-BYB-UNITS(2,1,5)           ,PIPE-1, 
             WS-BYB-UNITS(2,1,6)           ,PIPE-1,
             WS-BYB-UNITS(2,1,7)           ,PIPE-1, 
             WS-BYB-UNITS(2,1,8)           ,PIPE-1, 
             WS-BYB-UNITS(2,1,9)           ,PIPE-1, 
             WS-BYB-UNITS(2,1,10)          ,PIPE-1
              DELIMITED BY SIZE INTO WS-MISC-STRING(3).

           STRING WS-BYB-UNITS(2,2,1)      ,PIPE-1, 
             WS-BYB-UNITS(2,2,2)           ,PIPE-1, 
             WS-BYB-UNITS(2,2,3)           ,PIPE-1, 
             WS-BYB-UNITS(2,2,4)           ,PIPE-1, 
             WS-BYB-UNITS(2,2,5)           ,PIPE-1, 
             WS-BYB-UNITS(2,2,6)           ,PIPE-1, 
             WS-BYB-UNITS(2,2,7)           ,PIPE-1, 
             WS-BYB-UNITS(2,2,8)           ,PIPE-1, 
             WS-BYB-UNITS(2,2,9)           ,PIPE-1, 
             WS-BYB-UNITS(2,2,10)          ,PIPE-1, 
             WS-BYB-UNITS(3,1,1)           ,PIPE-1, 
             WS-BYB-UNITS(3,1,2)           ,PIPE-1, 
             WS-BYB-UNITS(3,1,3)           ,PIPE-1, 
             WS-BYB-UNITS(3,1,4)           ,PIPE-1, 
             WS-BYB-UNITS(3,1,5)           ,PIPE-1, 
             WS-BYB-UNITS(3,1,6)           ,PIPE-1, 
             WS-BYB-UNITS(3,1,7)           ,PIPE-1, 
             WS-BYB-UNITS(3,1,8)           ,PIPE-1, 
             WS-BYB-UNITS(3,1,9)           ,PIPE-1, 
             WS-BYB-UNITS(3,1,10)          ,PIPE-1,
             WS-BYB-UNITS(3,2,1)           ,PIPE-1, 
             WS-BYB-UNITS(3,2,2)           ,PIPE-1, 
             WS-BYB-UNITS(3,2,3)           ,PIPE-1, 
             WS-BYB-UNITS(3,2,4)           ,PIPE-1, 
             WS-BYB-UNITS(3,2,5)           ,PIPE-1, 
             WS-BYB-UNITS(3,2,6)           ,PIPE-1, 
             WS-BYB-UNITS(3,2,7)           ,PIPE-1, 
             WS-BYB-UNITS(3,2,8)           ,PIPE-1, 
             WS-BYB-UNITS(3,2,9)           ,PIPE-1, 
             WS-BYB-UNITS(3,2,10)          ,PIPE-1, 
             WS-BYB-OUTBAL(1,1,1)          ,PIPE-1
             WS-BYB-OUTBAL(1,1,2)          ,PIPE-1, 
             WS-BYB-OUTBAL(1,1,3)          ,PIPE-1, 
             WS-BYB-OUTBAL(1,1,4)          ,PIPE-1, 
             WS-BYB-OUTBAL(1,1,5)          ,PIPE-1, 
             WS-BYB-OUTBAL(1,1,6)          ,PIPE-1, 
             WS-BYB-OUTBAL(1,1,7)          ,PIPE-1, 
             WS-BYB-OUTBAL(1,1,8)          ,PIPE-1, 
             WS-BYB-OUTBAL(1,1,9)          ,PIPE-1, 
             WS-BYB-OUTBAL(1,1,10)         ,PIPE-1, 
             WS-BYB-OUTBAL(1,2,1)          ,PIPE-1, 
             WS-BYB-OUTBAL(1,2,2)          ,PIPE-1, 
             WS-BYB-OUTBAL(1,2,3)          ,PIPE-1, 
             WS-BYB-OUTBAL(1,2,4)          ,PIPE-1, 
             WS-BYB-OUTBAL(1,2,5)          ,PIPE-1, 
             WS-BYB-OUTBAL(1,2,6)          ,PIPE-1, 
             WS-BYB-OUTBAL(1,2,7)          ,PIPE-1, 
             WS-BYB-OUTBAL(1,2,8)          ,PIPE-1, 
             WS-BYB-OUTBAL(1,2,9)          ,PIPE-1, 
             WS-BYB-OUTBAL(1,2,10)         ,PIPE-1, 
             WS-BYB-OUTBAL(2,1,1)          ,PIPE-1, 
             WS-BYB-OUTBAL(2,1,2)          ,PIPE-1, 
             WS-BYB-OUTBAL(2,1,3)          ,PIPE-1, 
             WS-BYB-OUTBAL(2,1,4)          ,PIPE-1, 
             WS-BYB-OUTBAL(2,1,5)          ,PIPE-1, 
             WS-BYB-OUTBAL(2,1,6)          ,PIPE-1, 
             WS-BYB-OUTBAL(2,1,7)          ,PIPE-1, 
             WS-BYB-OUTBAL(2,1,8)          ,PIPE-1, 
             WS-BYB-OUTBAL(2,1,9)          ,PIPE-1, 
             WS-BYB-OUTBAL(2,1,10)         ,PIPE-1, 
             WS-BYB-OUTBAL(2,2,1)          ,PIPE-1, 
             WS-BYB-OUTBAL(2,2,2)          ,PIPE-1, 
             WS-BYB-OUTBAL(2,2,3)          ,PIPE-1, 
             WS-BYB-OUTBAL(2,2,4)          ,PIPE-1, 
             WS-BYB-OUTBAL(2,2,5)          ,PIPE-1, 
             WS-BYB-OUTBAL(2,2,6)          ,PIPE-1, 
             WS-BYB-OUTBAL(2,2,7)          ,PIPE-1, 
             WS-BYB-OUTBAL(2,2,8)          ,PIPE-1, 
             WS-BYB-OUTBAL(2,2,9)          ,PIPE-1, 
             WS-BYB-OUTBAL(2,2,10)         ,PIPE-1, 
             WS-BYB-OUTBAL(3,1,1)          ,PIPE-1, 
             WS-BYB-OUTBAL(3,1,2)          ,PIPE-1, 
             WS-BYB-OUTBAL(3,1,3)          ,PIPE-1, 
             WS-BYB-OUTBAL(3,1,4)          ,PIPE-1, 
             WS-BYB-OUTBAL(3,1,5)          ,PIPE-1, 
             WS-BYB-OUTBAL(3,1,6)          ,PIPE-1, 
             WS-BYB-OUTBAL(3,1,7)          ,PIPE-1, 
             WS-BYB-OUTBAL(3,1,8)          ,PIPE-1, 
             WS-BYB-OUTBAL(3,1,9)          ,PIPE-1, 
             WS-BYB-OUTBAL(3,1,10)         ,PIPE-1, 
             WS-BYB-OUTBAL(3,2,1)          ,PIPE-1, 
             WS-BYB-OUTBAL(3,2,2)          ,PIPE-1, 
             WS-BYB-OUTBAL(3,2,3)          ,PIPE-1, 
             WS-BYB-OUTBAL(3,2,4)          ,PIPE-1, 
             WS-BYB-OUTBAL(3,2,5)          ,PIPE-1, 
             WS-BYB-OUTBAL(3,2,6)          ,PIPE-1, 
             WS-BYB-OUTBAL(3,2,7)          ,PIPE-1, 
             WS-BYB-OUTBAL(3,2,8)          ,PIPE-1, 
             WS-BYB-OUTBAL(3,2,9)          ,PIPE-1, 
             WS-BYB-OUTBAL(3,2,10)         ,PIPE-1, 
             WS-BYB-UNITS-BNKRPT(1,1,1)    ,PIPE-1, 
             WS-BYB-UNITS-BNKRPT(1,1,2)    ,PIPE-1, 
             WS-BYB-UNITS-BNKRPT(1,1,3)    ,PIPE-1, 
             WS-BYB-UNITS-BNKRPT(1,1,4)    ,PIPE-1, 
             WS-BYB-UNITS-BNKRPT(1,1,5)    ,PIPE-1, 
             WS-BYB-UNITS-BNKRPT(1,1,6)    ,PIPE-1, 
             WS-BYB-UNITS-BNKRPT(1,1,7)    ,PIPE-1, 
             WS-BYB-UNITS-BNKRPT(1,1,8)    ,PIPE-1, 
             WS-BYB-UNITS-BNKRPT(1,1,9)    ,PIPE-1, 
             WS-BYB-UNITS-BNKRPT(1,1,10)   ,PIPE-1, 
             WS-BYB-UNITS-BNKRPT(1,2,1)    ,PIPE-1, 
             WS-BYB-UNITS-BNKRPT(1,2,2)    ,PIPE-1, 
             WS-BYB-UNITS-BNKRPT(1,2,3)    ,PIPE-1, 
             WS-BYB-UNITS-BNKRPT(1,2,4)    ,PIPE-1, 
             WS-BYB-UNITS-BNKRPT(1,2,5)    ,PIPE-1, 
             WS-BYB-UNITS-BNKRPT(1,2,6)    ,PIPE-1, 
             WS-BYB-UNITS-BNKRPT(1,2,7)    ,PIPE-1, 
             WS-BYB-UNITS-BNKRPT(1,2,8)    ,PIPE-1, 
             WS-BYB-UNITS-BNKRPT(1,2,9)    ,PIPE-1, 
             WS-BYB-UNITS-BNKRPT(1,2,10)   ,PIPE-1,
             WS-BYB-UNITS-BNKRPT(2,1,1)    ,PIPE-1, 
             WS-BYB-UNITS-BNKRPT(2,1,2)    ,PIPE-1, 
             WS-BYB-UNITS-BNKRPT(2,1,3)    ,PIPE-1, 
             WS-BYB-UNITS-BNKRPT(2,1,4)    ,PIPE-1, 
             WS-BYB-UNITS-BNKRPT(2,1,5)    ,PIPE-1, 
             WS-BYB-UNITS-BNKRPT(2,1,6)    ,PIPE-1, 
             WS-BYB-UNITS-BNKRPT(2,1,7)    ,PIPE-1, 
             WS-BYB-UNITS-BNKRPT(2,1,8)    ,PIPE-1, 
             WS-BYB-UNITS-BNKRPT(2,1,9)    ,PIPE-1, 
             WS-BYB-UNITS-BNKRPT(2,1,10)   ,PIPE-1, 
             WS-BYB-UNITS-BNKRPT(2,2,1)    ,PIPE-1, 
             WS-BYB-UNITS-BNKRPT(2,2,2)    ,PIPE-1, 
             WS-BYB-UNITS-BNKRPT(2,2,3)    ,PIPE-1, 
             WS-BYB-UNITS-BNKRPT(2,2,4)    ,PIPE-1, 
             WS-BYB-UNITS-BNKRPT(2,2,5)    ,PIPE-1, 
             WS-BYB-UNITS-BNKRPT(2,2,6)    ,PIPE-1, 
             WS-BYB-UNITS-BNKRPT(2,2,7)    ,PIPE-1, 
             WS-BYB-UNITS-BNKRPT(2,2,8)    ,PIPE-1, 
             WS-BYB-UNITS-BNKRPT(2,2,9)    ,PIPE-1, 
             WS-BYB-UNITS-BNKRPT(2,2,10)   ,PIPE-1, 
             WS-BYB-UNITS-BNKRPT(3,1,1)    ,PIPE-1, 
             WS-BYB-UNITS-BNKRPT(3,1,2)    ,PIPE-1
              DELIMITED BY SIZE INTO WS-MISC-STRING(4).

           STRING  WS-BYB-UNITS-BNKRPT(3,1,3)  ,PIPE-1, 
             WS-BYB-UNITS-BNKRPT(3,1,4)        ,PIPE-1, 
             WS-BYB-UNITS-BNKRPT(3,1,5)        ,PIPE-1, 
             WS-BYB-UNITS-BNKRPT(3,1,6)        ,PIPE-1, 
             WS-BYB-UNITS-BNKRPT(3,1,7)        ,PIPE-1, 
             WS-BYB-UNITS-BNKRPT(3,1,8)        ,PIPE-1, 
             WS-BYB-UNITS-BNKRPT(3,1,9)        ,PIPE-1, 
             WS-BYB-UNITS-BNKRPT(3,1,10)       ,PIPE-1, 
             WS-BYB-UNITS-BNKRPT(3,2,1)        ,PIPE-1, 
             WS-BYB-UNITS-BNKRPT(3,2,2)        ,PIPE-1, 
             WS-BYB-UNITS-BNKRPT(3,2,3)        ,PIPE-1, 
             WS-BYB-UNITS-BNKRPT(3,2,4)        ,PIPE-1, 
             WS-BYB-UNITS-BNKRPT(3,2,5)        ,PIPE-1, 
             WS-BYB-UNITS-BNKRPT(3,2,6)        ,PIPE-1, 
             WS-BYB-UNITS-BNKRPT(3,2,7)        ,PIPE-1, 
             WS-BYB-UNITS-BNKRPT(3,2,8)        ,PIPE-1, 
             WS-BYB-UNITS-BNKRPT(3,2,9)        ,PIPE-1, 
             WS-BYB-UNITS-BNKRPT(3,2,10)       ,PIPE-1, 
             WS-BYB-OUTBAL-BNKRPT(1,1,1)       ,PIPE-1, 
             WS-BYB-OUTBAL-BNKRPT(1,1,2)       ,PIPE-1, 
             WS-BYB-OUTBAL-BNKRPT(1,1,3)       ,PIPE-1, 
             WS-BYB-OUTBAL-BNKRPT(1,1,4)       ,PIPE-1, 
             WS-BYB-OUTBAL-BNKRPT(1,1,5)       ,PIPE-1, 
             WS-BYB-OUTBAL-BNKRPT(1,1,6)       ,PIPE-1, 
             WS-BYB-OUTBAL-BNKRPT(1,1,7)       ,PIPE-1, 
             WS-BYB-OUTBAL-BNKRPT(1,1,8)       ,PIPE-1, 
             WS-BYB-OUTBAL-BNKRPT(1,1,9)       ,PIPE-1, 
             WS-BYB-OUTBAL-BNKRPT(1,1,10)      ,PIPE-1, 
             WS-BYB-OUTBAL-BNKRPT(1,2,1)       ,PIPE-1, 
             WS-BYB-OUTBAL-BNKRPT(1,2,2)       ,PIPE-1, 
             WS-BYB-OUTBAL-BNKRPT(1,2,3)       ,PIPE-1, 
             WS-BYB-OUTBAL-BNKRPT(1,2,4)       ,PIPE-1, 
             WS-BYB-OUTBAL-BNKRPT(1,2,5)       ,PIPE-1, 
             WS-BYB-OUTBAL-BNKRPT(1,2,6)       ,PIPE-1, 
             WS-BYB-OUTBAL-BNKRPT(1,2,7)       ,PIPE-1, 
             WS-BYB-OUTBAL-BNKRPT(1,2,8)       ,PIPE-1, 
             WS-BYB-OUTBAL-BNKRPT(1,2,9)       ,PIPE-1, 
             WS-BYB-OUTBAL-BNKRPT(1,2,10)      ,PIPE-1, 
             WS-BYB-OUTBAL-BNKRPT(2,1,1)       ,PIPE-1, 
             WS-BYB-OUTBAL-BNKRPT(2,1,2)       ,PIPE-1, 
             WS-BYB-OUTBAL-BNKRPT(2,1,3)       ,PIPE-1, 
             WS-BYB-OUTBAL-BNKRPT(2,1,4)       ,PIPE-1, 
             WS-BYB-OUTBAL-BNKRPT(2,1,5)       ,PIPE-1, 
             WS-BYB-OUTBAL-BNKRPT(2,1,6)       ,PIPE-1, 
             WS-BYB-OUTBAL-BNKRPT(2,1,7)       ,PIPE-1, 
             WS-BYB-OUTBAL-BNKRPT(2,1,8)       ,PIPE-1, 
             WS-BYB-OUTBAL-BNKRPT(2,1,9)       ,PIPE-1, 
             WS-BYB-OUTBAL-BNKRPT(2,1,10)      ,PIPE-1, 
             WS-BYB-OUTBAL-BNKRPT(2,2,1)       ,PIPE-1, 
             WS-BYB-OUTBAL-BNKRPT(2,2,2)       ,PIPE-1, 
             WS-BYB-OUTBAL-BNKRPT(2,2,3)       ,PIPE-1, 
             WS-BYB-OUTBAL-BNKRPT(2,2,4)       ,PIPE-1, 
             WS-BYB-OUTBAL-BNKRPT(2,2,5)       ,PIPE-1, 
             WS-BYB-OUTBAL-BNKRPT(2,2,6)       ,PIPE-1, 
             WS-BYB-OUTBAL-BNKRPT(2,2,7)       ,PIPE-1, 
             WS-BYB-OUTBAL-BNKRPT(2,2,8)       ,PIPE-1, 
             WS-BYB-OUTBAL-BNKRPT(2,2,9)       ,PIPE-1, 
             WS-BYB-OUTBAL-BNKRPT(2,2,10)      ,PIPE-1, 
             WS-BYB-OUTBAL-BNKRPT(3,1,1)       ,PIPE-1, 
             WS-BYB-OUTBAL-BNKRPT(3,1,2)       ,PIPE-1, 
             WS-BYB-OUTBAL-BNKRPT(3,1,3)       ,PIPE-1, 
             WS-BYB-OUTBAL-BNKRPT(3,1,4)       ,PIPE-1, 
             WS-BYB-OUTBAL-BNKRPT(3,1,5)       ,PIPE-1, 
             WS-BYB-OUTBAL-BNKRPT(3,1,6)       ,PIPE-1, 
             WS-BYB-OUTBAL-BNKRPT(3,1,7)       ,PIPE-1, 
             WS-BYB-OUTBAL-BNKRPT(3,1,8)       ,PIPE-1, 
             WS-BYB-OUTBAL-BNKRPT(3,1,9)       ,PIPE-1, 
             WS-BYB-OUTBAL-BNKRPT(3,1,10)      ,PIPE-1, 
             WS-BYB-OUTBAL-BNKRPT(3,2,1)       ,PIPE-1, 
             WS-BYB-OUTBAL-BNKRPT(3,2,2)       ,PIPE-1, 
             WS-BYB-OUTBAL-BNKRPT(3,2,3)       ,PIPE-1, 
             WS-BYB-OUTBAL-BNKRPT(3,2,4)       ,PIPE-1, 
             WS-BYB-OUTBAL-BNKRPT(3,2,5)       ,PIPE-1, 
             WS-BYB-OUTBAL-BNKRPT(3,2,6)       ,PIPE-1, 
             WS-BYB-OUTBAL-BNKRPT(3,2,7)       ,PIPE-1, 
             WS-BYB-OUTBAL-BNKRPT(3,2,8)       ,PIPE-1, 
             WS-BYB-OUTBAL-BNKRPT(3,2,9)       ,PIPE-1, 
             WS-BYB-OUTBAL-BNKRPT(3,2,10)      ,PIPE-1, 
             WS-BYB-SUMWGTTM                   ,PIPE-1,  
             WS-BYB-SUMWGTAV                   ,PIPE-1, 
             WS-BYB-SUMWGTTA                   ,PIPE-1,  
             WS-BYB-IB-ACCRUED                 ,PIPE-1,  
             WS-BYB-DLDISC(1)                  ,PIPE-1, 
             WS-BYB-DLDISC(2)                  ,PIPE-1, 
             WS-BYB-DLDISC(3)                  ,PIPE-1, 
             WS-BYB-STD-POS                    ,PIPE-1, 
             WS-BYB-30-DAY-MOVED               ,PIPE-1, 
             WS-BYB-PROJ-INCREASE              ,PIPE-1, 
             WS-BYB-PROJ-PROFIT                ,PIPE-1, 
             WS-BYB-PROCEEDS(1)                ,PIPE-1, 
             WS-BYB-PROCEEDS(2)                ,PIPE-1, 
             WS-BYB-LOCKBOX(1)                 ,PIPE-1, 
             WS-BYB-LOCKBOX(2)                 ,PIPE-1, 
             WS-BYB-UNEARNED-AMT(1,1,1)        ,PIPE-1, 
             WS-BYB-UNEARNED-AMT(1,1,2)        ,PIPE-1, 
             WS-BYB-UNEARNED-AMT(1,1,3)        ,PIPE-1, 
             WS-BYB-UNEARNED-AMT(1,1,4)        ,PIPE-1, 
             WS-BYB-UNEARNED-AMT(1,1,5)        ,PIPE-1, 
             WS-BYB-UNEARNED-AMT(1,1,6)        ,PIPE-1, 
             WS-BYB-UNEARNED-AMT(1,1,7)        ,PIPE-1, 
             WS-BYB-UNEARNED-AMT(1,1,8)        ,PIPE-1
              DELIMITED BY SIZE INTO WS-MISC-STRING(5).

           STRING WS-BYB-UNEARNED-AMT(1,1,9)   ,PIPE-1, 
             WS-BYB-UNEARNED-AMT(1,1,10)       ,PIPE-1, 
             WS-BYB-UNEARNED-AMT(1,2,1)        ,PIPE-1, 
             WS-BYB-UNEARNED-AMT(1,2,2)        ,PIPE-1, 
             WS-BYB-UNEARNED-AMT(1,2,3)        ,PIPE-1, 
             WS-BYB-UNEARNED-AMT(1,2,4)        ,PIPE-1, 
             WS-BYB-UNEARNED-AMT(1,2,5)        ,PIPE-1, 
             WS-BYB-UNEARNED-AMT(1,2,6)        ,PIPE-1, 
             WS-BYB-UNEARNED-AMT(1,2,7)        ,PIPE-1, 
             WS-BYB-UNEARNED-AMT(1,2,8)        ,PIPE-1, 
             WS-BYB-UNEARNED-AMT(1,2,9)        ,PIPE-1, 
             WS-BYB-UNEARNED-AMT(1,2,10)       ,PIPE-1, 
             WS-BYB-UNEARNED-AMT(2,1,1)        ,PIPE-1, 
             WS-BYB-UNEARNED-AMT(2,1,2)        ,PIPE-1, 
             WS-BYB-UNEARNED-AMT(2,1,3)        ,PIPE-1, 
             WS-BYB-UNEARNED-AMT(2,1,4)        ,PIPE-1, 
             WS-BYB-UNEARNED-AMT(2,1,5)        ,PIPE-1, 
             WS-BYB-UNEARNED-AMT(2,1,6)        ,PIPE-1, 
             WS-BYB-UNEARNED-AMT(2,1,7)        ,PIPE-1, 
             WS-BYB-UNEARNED-AMT(2,1,8)        ,PIPE-1, 
             WS-BYB-UNEARNED-AMT(2,1,9)        ,PIPE-1, 
             WS-BYB-UNEARNED-AMT(2,1,10)       ,PIPE-1, 
             WS-BYB-UNEARNED-AMT(2,2,1)        ,PIPE-1, 
             WS-BYB-UNEARNED-AMT(2,2,2)        ,PIPE-1, 
             WS-BYB-UNEARNED-AMT(2,2,3)        ,PIPE-1, 
             WS-BYB-UNEARNED-AMT(2,2,4)        ,PIPE-1, 
             WS-BYB-UNEARNED-AMT(2,2,5)        ,PIPE-1, 
             WS-BYB-UNEARNED-AMT(2,2,6)        ,PIPE-1, 
             WS-BYB-UNEARNED-AMT(2,2,7)        ,PIPE-1, 
             WS-BYB-UNEARNED-AMT(2,2,8)        ,PIPE-1, 
             WS-BYB-UNEARNED-AMT(2,2,9)        ,PIPE-1, 
             WS-BYB-UNEARNED-AMT(2,2,10)       ,PIPE-1, 
             WS-BYB-UNEARNED-AMT(3,1,1)        ,PIPE-1, 
             WS-BYB-UNEARNED-AMT(3,1,2)        ,PIPE-1, 
             WS-BYB-UNEARNED-AMT(3,1,3)        ,PIPE-1, 
             WS-BYB-UNEARNED-AMT(3,1,4)        ,PIPE-1, 
             WS-BYB-UNEARNED-AMT(3,1,5)        ,PIPE-1, 
             WS-BYB-UNEARNED-AMT(3,1,6)        ,PIPE-1, 
             WS-BYB-UNEARNED-AMT(3,1,7)        ,PIPE-1, 
             WS-BYB-UNEARNED-AMT(3,1,8)        ,PIPE-1, 
             WS-BYB-UNEARNED-AMT(3,1,9)        ,PIPE-1, 
             WS-BYB-UNEARNED-AMT(3,1,10)       ,PIPE-1, 
             WS-BYB-UNEARNED-AMT(3,2,1)        ,PIPE-1, 
             WS-BYB-UNEARNED-AMT(3,2,2)        ,PIPE-1, 
             WS-BYB-UNEARNED-AMT(3,2,3)        ,PIPE-1, 
             WS-BYB-UNEARNED-AMT(3,2,4)        ,PIPE-1, 
             WS-BYB-UNEARNED-AMT(3,2,5)        ,PIPE-1, 
             WS-BYB-UNEARNED-AMT(3,2,6)        ,PIPE-1, 
             WS-BYB-UNEARNED-AMT(3,2,7)        ,PIPE-1, 
             WS-BYB-UNEARNED-AMT(3,2,8)        ,PIPE-1, 
             WS-BYB-UNEARNED-AMT(3,2,9)        ,PIPE-1, 
             WS-BYB-UNEARNED-AMT(3,2,10) 
              DELIMITED BY SIZE INTO WS-MISC-STRING(6).

           STRING WS-MISC-STRING(1), WS-MISC-STRING(2),
                  WS-MISC-STRING(3), WS-MISC-STRING(4),
                  WS-MISC-STRING(5), WS-MISC-STRING(6)
                DELIMITED BY ' ' INTO BYBDATA-OUT-REC.
 
           INSPECT BYBDATA-OUT-REC REPLACING ALL "+" BY " ".

       CREATE-ASCII-BY-EXIT.
           EXIT.


       CREATE-ASCII-RC SECTION.
           PERFORM OPEN-RCDATA-FILE-OUTPUT.

           MOVE EOM-RC-TRANS-DATE TO VRC-TRANS-DATE.

           PERFORM VARYING SUB FROM 1 BY 1
            UNTIL SUB > 9999
              IF PROC-BRANCH-ARRAY(SUB) = "Y"
                 MOVE SUB TO VRC-BRNO
                 PERFORM OPEN-VRC1-FILE
                 PERFORM START-VRC1-FILE
                 PERFORM CREATE-ASCII-RC-OUT-REC
                 PERFORM CLOSE-VRC1-FILE
              END-IF
           END-PERFORM.

           PERFORM CLOSE-RCDATA-FILE.

           ADD 1 TO SAVED-OUT-FILES-CTR.
           MOVE RCDATA-OUT-PATH TO 
                    SAVED-FILES-ARRAY(SAVED-OUT-FILES-CTR).

           GO TO CREATE-ASCII-RC-EXIT.

       CREATE-ASCII-RC-OUT-REC.
           PERFORM READ-VRC1-FILE-NEXT.
           IF IO-BAD OR VRC-BRNO NOT = SUB
              EXIT PARAGRAPH.

           MOVE SPACES TO RCDATA-OUT-REC.
           STRING VRC-BRNO          ,PIPE-1, 
                  VRC-TRANS-DATE    ,PIPE-1, 
                  VRC-STATUS
              DELIMITED BY SIZE INTO RCDATA-OUT-REC. 

           PERFORM WRITE-RCDATA-FILE.

           GO TO CREATE-ASCII-RC-OUT-REC.

       CREATE-ASCII-RC-EXIT.
           EXIT.


      *** OUTPUT ALL VGBFILE ROWS ***
       CREATE-ASCII-GB SECTION.
           PERFORM OPEN-GBDATA-FILE-OUTPUT.
           PERFORM OPEN-VGB-FILE.
           MOVE ZEROS TO VGB-BRNO VGB-NO.
           PERFORM START-VGB-FILE.

       CREATE-ASCII-GB-LOOP.

           PERFORM READ-VGB-FILE-NEXT.

           IF IO-BAD
              PERFORM CLOSE-GBDATA-FILE
              PERFORM CLOSE-VGB-FILE
              ADD 1 TO SAVED-OUT-FILES-CTR
              MOVE GBDATA-OUT-PATH TO 
                    SAVED-FILES-ARRAY(SAVED-OUT-FILES-CTR)
              GO TO CREATE-ASCII-GB-EXIT.

           IF PROC-BRANCH-ARRAY(VGB-BRNO) NOT = "Y" 
              GO TO CREATE-ASCII-GB-LOOP.

           MOVE VGB-GB-REC TO GB-REC.
           PERFORM SET-GB-FIELDS.

           MOVE SPACES TO GBDATA-OUT-REC.
           STRING QGB-BRNO              ,PIPE-1,    
                  QGB-NO                ,PIPE-1,    
                  QGB-EOM-TAG-CCYYMM    ,PIPE-1,    
                  QGB-EOM-UPDATE-USERID          
              DELIMITED BY SIZE INTO GBDATA-OUT-REC. 

           PERFORM WRITE-GBDATA-FILE.

           GO TO CREATE-ASCII-GB-LOOP.

       CREATE-ASCII-GB-EXIT.
           EXIT.


      *****************************************************************
      *
      *    C O P Y    L I B R A R Y    W O R K E R S
      *
      *****************************************************************
       COPY "LIBGB/GBRCCL.CPY".
       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/GRWORK.CPY".
       COPY "LIBGB/GRSERCH.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/TIMEIO.CPY".
       COPY "LIBGB/GPENV.CPY".
       COPY "LIBLP/POSTDATE.CPY".
       COPY "LIBLP/LPBYCL.CPY".
       COPY "LIBLP/LPDSCL.CPY".
       COPY "LIBGB/AGEING.CPY".
       COPY "LIBLP/LPUPWK.CPY".
       COPY "LIBLP/LPPDUE.CPY".
       COPY "LIBGB/SPPDTH.CPY".
       COPY "LIBLP/LPCEPP.CPY".
       COPY "LIBLP/LPMDTE.CPY".
       COPY "LIBLP/LPEARN.CPY".
       COPY "LIBLP/LPSRCD.CPY".
       COPY "LIBLP/ARRAYSP.CPY". 
       COPY "LIBLP/LPFRVBY.CPY".
       COPY "LIBGB/GBFRVRC.CPY".
       COPY "LIBGB/GBFRVGB.CPY".

      *********************************************************
       FORM-RESET SECTION.
      *********************************************************
           MOVE EXT-CONAME-CONAME TO EOMALL-CONAME.
           MOVE EXT-CONAME-SYSDATE TO EOMALL-SYSDATE.
           DISPLAY EOMALL-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE EOMALL-FORM-FIELDS TO WR-BUF.
           MOVE EOMALL-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      *********************************************************
       SETUP-LINK-INFO SECTION.
      *********************************************************
           MOVE EOMALL-HELP--FILE TO HELP-LINK-FILE.
           MOVE EOMALL-HELP--DIR  TO HELP-LINK-DIR.

      *********************************************************
       VDU-CONVERT-FIELD SECTION.
      *********************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBLP/EOMALL_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ******************************************************************
      *    W R I T E - D E T A I L - L I N E
      ******************************************************************
       WRITE-DETAIL-LINE SECTION.

           ADD LN-FEED LN-COUNT GIVING LN-WK.

           IF LN-WK > EXT-CONAME-RPT-LINES
              PERFORM WRITE-HEADER.

           MOVE DETAIL-LINE TO PR-REC.
           PERFORM WRITE-PR-REC.

           ADD LN-FEED TO LN-COUNT.
           MOVE 1 TO LN-FEED.
           MOVE SPACES TO DETAIL-LINE.

      *=================================================================
       WRITE-DETAIL-LINE-EXIT.
           EXIT.

      ******************************************************************
      *    P R I N T - R A N G E - L I N E S
      ******************************************************************
       PRINT-RANGE-LINES SECTION.

           MOVE 2 TO LN-FEED.

           IF GROUP-FLAG = "Y"
              MOVE "BEGINNING BRANCH   :"  TO RANGE-LINE
              MOVE ENT-GROUP               TO RANGE-SELECTION
              PERFORM WRITE-DETAIL-LINE
              MOVE "ENDING BRANCH      :"  TO RANGE-LINE
              MOVE SPACES                  TO RANGE-SELECTION
              PERFORM WRITE-DETAIL-LINE
           ELSE
              MOVE "BEGINNING BRANCH   :" TO RANGE-LINE
              MOVE BEG-BRANCH             TO RANGE-SELECTION
              PERFORM WRITE-DETAIL-LINE
              MOVE "ENDING BRANCH      :" TO RANGE-LINE
              MOVE END-BRANCH             TO RANGE-SELECTION
              PERFORM WRITE-DETAIL-LINE.

           MOVE "STARTING TIME      :"    TO RANGE-LINE.
           MOVE STARTING-TIME             TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "ENDING   TIME      :"    TO RANGE-LINE.
           PERFORM GET-TIME.
           MOVE TIME-EDIT                 TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.

           MOVE 2 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      ******************************************************************
      *    W R I T E - H E A D E R
      ******************************************************************
       WRITE-HEADER SECTION.

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
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-PR-REC.

           MOVE ALL "="  TO PR-REC.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-PR-REC.

           MOVE 5 TO LN-COUNT.
           MOVE 1 TO LN-FEED.

      *=================================================================
       WRITE-HEADER-EXIT.
           EXIT.

      *****************************************************************
       MISC-ROUTINES SECTION.
      *****************************************************************
       COPY "LIBGB/GETPR.CPY".
       COPY "LIBGB/SYSTEM.CPY".
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/DACCESS.CPY".
       COPY "LIBGB/GETENV.CPY".
       COPY "LIBGB/MKTEMP.CPY".
       COPY "LIBGB/RMFILE.CPY".

       SEND-LEGEND.
           MOVE LEGEND TO WR-BUF.
           MOVE MESS-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       CREATE-LN-UPDATE-REC.

           MOVE SPACES TO LNDATA-OUT-REC.
           PERFORM SET-LN-FIELDS.

           MOVE QLN-LNAMT         TO  WS-QLN-LNAMT.
           INSPECT WS-QLN-LNAMT REPLACING LEADING "+" BY " ".   
           MOVE QLN-1STPYAMT      TO  WS-QLN-1STPYAMT.
           INSPECT WS-QLN-1STPYAMT REPLACING LEADING "+" BY " ".   
           MOVE QLN-REGPYAMT      TO  WS-QLN-REGPYAMT.
           INSPECT WS-QLN-REGPYAMT REPLACING LEADING "+" BY " ".   
           MOVE QLN-LASTPYAMT     TO  WS-QLN-LASTPYAMT.
           INSPECT WS-QLN-LASTPYAMT REPLACING LEADING "+" BY " ".   
           MOVE QLN-MTD-PAYS-RECDEL TO WS-QLN-MTD-PAYS-RECDEL.
           INSPECT WS-QLN-MTD-PAYS-RECDEL REPLACING LEADING "+" BY " ".
           MOVE QLN-DLNO TO WS-QLN-DLNO.
           INSPECT WS-QLN-DLNO REPLACING LEADING "+" BY " ".   
           MOVE QLN-CURBAL        TO  WS-QLN-CURBAL.
           INSPECT WS-QLN-CURBAL REPLACING LEADING "+" BY " ".   
           MOVE QLN-INTBAL        TO  WS-QLN-INTBAL.
           INSPECT WS-QLN-INTBAL REPLACING LEADING "+" BY " ".   
           MOVE QLN-LCBAL         TO  WS-QLN-LCBAL.
           INSPECT WS-QLN-LCBAL REPLACING LEADING "+" BY " ".   
           MOVE QLN-OTHBAL        TO  WS-QLN-OTHBAL.
           INSPECT WS-QLN-OTHBAL REPLACING LEADING "+" BY " ".   
           MOVE QLN-OT2BAL        TO  WS-QLN-OT2BAL.
           INSPECT WS-QLN-OT2BAL REPLACING LEADING "+" BY " ".   
           MOVE QLN-MONTHEND-BAL  TO  WS-QLN-MONTHEND-BAL.
           INSPECT WS-QLN-MONTHEND-BAL REPLACING LEADING "+" BY " ".   
           MOVE QLN-ANTICERN-1    TO  WS-QLN-ANTICERN-1.
           INSPECT WS-QLN-ANTICERN-1 REPLACING LEADING "+" BY " ".   
           MOVE QLN-ANTICADJ-1    TO  WS-QLN-ANTICADJ-1.
           INSPECT WS-QLN-ANTICADJ-1 REPLACING LEADING "+" BY " ".   
           MOVE QLN-UFERN-1       TO  WS-QLN-UFERN-1.
           INSPECT WS-QLN-UFERN-1 REPLACING LEADING "+" BY " ".   
           MOVE QLN-ACCERN-1      TO  WS-QLN-ACCERN-1.
           INSPECT WS-QLN-ACCERN-1 REPLACING LEADING "+" BY " ".   
           MOVE QLN-ANTICERN-2    TO  WS-QLN-ANTICERN-2.
           INSPECT WS-QLN-ANTICERN-2 REPLACING LEADING "+" BY " ".   
           MOVE QLN-ANTICADJ-2    TO  WS-QLN-ANTICADJ-2.
           INSPECT WS-QLN-ANTICADJ-2 REPLACING LEADING "+" BY " ".   
           MOVE QLN-UFERN-2       TO  WS-QLN-UFERN-2.
           INSPECT WS-QLN-UFERN-2 REPLACING LEADING "+" BY " ".   
           MOVE QLN-ACCERN-2      TO  WS-QLN-ACCERN-2.
           INSPECT WS-QLN-ACCERN-2 REPLACING LEADING "+" BY " ".   
           MOVE QLN-ANTICERN-3    TO  WS-QLN-ANTICERN-3.
           INSPECT WS-QLN-ANTICERN-3 REPLACING LEADING "+" BY " ".   
           MOVE QLN-ANTICADJ-3    TO  WS-QLN-ANTICADJ-3.
           INSPECT WS-QLN-ANTICADJ-3 REPLACING LEADING "+" BY " ".   
           MOVE QLN-UFERN-3       TO  WS-QLN-UFERN-3.
           INSPECT WS-QLN-UFERN-3 REPLACING LEADING "+" BY " ".   
           MOVE QLN-ACCERN-3      TO  WS-QLN-ACCERN-3.
           INSPECT WS-QLN-ACCERN-3 REPLACING LEADING "+" BY " ".   
           MOVE QLN-ANTICERN-4    TO  WS-QLN-ANTICERN-4.
           INSPECT WS-QLN-ANTICERN-4 REPLACING LEADING "+" BY " ".   
           MOVE QLN-ANTICADJ-4    TO  WS-QLN-ANTICADJ-4.
           INSPECT WS-QLN-ANTICADJ-4 REPLACING LEADING "+" BY " ".   
           MOVE QLN-UFERN-4       TO  WS-QLN-UFERN-4.
           INSPECT WS-QLN-UFERN-4 REPLACING LEADING "+" BY " ".   
           MOVE QLN-ACCERN-4      TO  WS-QLN-ACCERN-4.
           INSPECT WS-QLN-ACCERN-4 REPLACING LEADING "+" BY " ".   
           MOVE QLN-ANTICERN-5    TO  WS-QLN-ANTICERN-5.
           INSPECT WS-QLN-ANTICERN-5 REPLACING LEADING "+" BY " ".   
           MOVE QLN-ANTICADJ-5    TO  WS-QLN-ANTICADJ-5.
           INSPECT WS-QLN-ANTICADJ-5 REPLACING LEADING "+" BY " ".   
           MOVE QLN-UFERN-5       TO  WS-QLN-UFERN-5.
           INSPECT WS-QLN-UFERN-5 REPLACING LEADING "+" BY " ".   
           MOVE QLN-ACCERN-5      TO  WS-QLN-ACCERN-5.
           INSPECT WS-QLN-ACCERN-5 REPLACING LEADING "+" BY " ".   
           MOVE QLN-ANTICERN-6    TO  WS-QLN-ANTICERN-6.
           INSPECT WS-QLN-ANTICERN-6 REPLACING LEADING "+" BY " ".   
           MOVE QLN-ANTICADJ-6    TO  WS-QLN-ANTICADJ-6.
           INSPECT WS-QLN-ANTICADJ-6 REPLACING LEADING "+" BY " ".   
           MOVE QLN-UFERN-6       TO  WS-QLN-UFERN-6.
           INSPECT WS-QLN-UFERN-6 REPLACING LEADING "+" BY " ".   
           MOVE QLN-ACCERN-6      TO  WS-QLN-ACCERN-6.
           INSPECT WS-QLN-ACCERN-6 REPLACING LEADING "+" BY " ".   
           MOVE QLN-ANTICERN-7    TO  WS-QLN-ANTICERN-7.
           INSPECT WS-QLN-ANTICERN-7 REPLACING LEADING "+" BY " ".   
           MOVE QLN-ANTICADJ-7    TO  WS-QLN-ANTICADJ-7.
           INSPECT WS-QLN-ANTICADJ-7 REPLACING LEADING "+" BY " ".   
           MOVE QLN-UFERN-7       TO  WS-QLN-UFERN-7.
           INSPECT WS-QLN-UFERN-7 REPLACING LEADING "+" BY " ".   
           MOVE QLN-ACCERN-7      TO  WS-QLN-ACCERN-7.
           INSPECT WS-QLN-ACCERN-7 REPLACING LEADING "+" BY " ".   
           MOVE QLN-ANTICERN-8    TO  WS-QLN-ANTICERN-8.
           INSPECT WS-QLN-ANTICERN-8 REPLACING LEADING "+" BY " ".   
           MOVE QLN-ANTICADJ-8    TO  WS-QLN-ANTICADJ-8.
           INSPECT WS-QLN-ANTICADJ-8 REPLACING LEADING "+" BY " ".   
           MOVE QLN-UFERN-8       TO  WS-QLN-UFERN-8.
           INSPECT WS-QLN-UFERN-8 REPLACING LEADING "+" BY " ".   
           MOVE QLN-ACCERN-8      TO  WS-QLN-ACCERN-8.
           INSPECT WS-QLN-ACCERN-8 REPLACING LEADING "+" BY " ".   
           MOVE QLN-ANTICERN-9    TO  WS-QLN-ANTICERN-9.
           INSPECT WS-QLN-ANTICERN-9 REPLACING LEADING "+" BY " ".   
           MOVE QLN-ANTICADJ-9    TO  WS-QLN-ANTICADJ-9.
           INSPECT WS-QLN-ANTICADJ-9 REPLACING LEADING "+" BY " ".   
           MOVE QLN-UFERN-9       TO  WS-QLN-UFERN-9.
           INSPECT WS-QLN-UFERN-9 REPLACING LEADING "+" BY " ".   
           MOVE QLN-ACCERN-9      TO  WS-QLN-ACCERN-9.
           INSPECT WS-QLN-ACCERN-9 REPLACING LEADING "+" BY " ".   
           MOVE QLN-ANTICERN-10   TO  WS-QLN-ANTICERN-10.
           INSPECT WS-QLN-ANTICERN-10 REPLACING LEADING "+" BY " ".   
           MOVE QLN-ANTICADJ-10   TO  WS-QLN-ANTICADJ-10.
           INSPECT WS-QLN-ANTICADJ-10 REPLACING LEADING "+" BY " ".   
           MOVE QLN-UFERN-10      TO  WS-QLN-UFERN-10.
           INSPECT WS-QLN-UFERN-10 REPLACING LEADING "+" BY " ".   
           MOVE QLN-ACCERN-10     TO  WS-QLN-ACCERN-10.
           INSPECT WS-QLN-ACCERN-10 REPLACING LEADING "+" BY " ".   
           MOVE QLN-TOTPAYMNTD    TO  WS-QLN-TOTPAYMNTD.
           INSPECT WS-QLN-TOTPAYMNTD REPLACING LEADING "+" BY " ".   
           MOVE QLN-TOTNODEF      TO  WS-QLN-TOTNODEF.
           INSPECT WS-QLN-TOTNODEF REPLACING LEADING "+" BY " ".   
           MOVE QLN-YTDNODEF      TO  WS-QLN-YTDNODEF.
           INSPECT WS-QLN-YTDNODEF REPLACING LEADING "+" BY " ".   
           MOVE QLN-TOTNODEFACT   TO  WS-QLN-TOTNODEFACT.
           INSPECT WS-QLN-TOTNODEFACT REPLACING LEADING "+" BY " ".   
           MOVE QLN-YTDNODEFACT   TO  WS-QLN-YTDNODEFACT.
           INSPECT WS-QLN-YTDNODEFACT REPLACING LEADING "+" BY " ".   
           MOVE QLN-TOTNOALDEL    TO  WS-QLN-TOTNOALDEL.
           INSPECT WS-QLN-TOTNOALDEL REPLACING LEADING "+" BY " ".   
           MOVE QLN-YTDNOALDEL    TO  WS-QLN-YTDNOALDEL.
           INSPECT WS-QLN-YTDNOALDEL REPLACING LEADING "+" BY " ".   
           MOVE QLN-PRLNAMT       TO  WS-QLN-PRLNAMT.
           INSPECT WS-QLN-PRLNAMT REPLACING LEADING "+" BY " ".   
           MOVE QLN-PRPDUE-DAYS   TO  WS-QLN-PRPDUE-DAYS.
           INSPECT WS-QLN-PRPDUE-DAYS REPLACING LEADING "+" BY " ".   
           MOVE QLN-MTD-PRIN      TO  WS-QLN-MTD-PRIN.
           INSPECT WS-QLN-MTD-PRIN REPLACING LEADING "+" BY " ".   
           MOVE QLN-MTD-LC        TO  WS-QLN-MTD-LC.
           INSPECT WS-QLN-MTD-LC REPLACING LEADING "+" BY " ".   
           MOVE QLN-MTD-INT       TO  WS-QLN-MTD-INT.
           INSPECT WS-QLN-MTD-INT REPLACING LEADING "+" BY " ".   
           MOVE QLN-MTD-OT        TO  WS-QLN-MTD-OT.
           INSPECT WS-QLN-MTD-OT REPLACING LEADING "+" BY " ".   
           MOVE QLN-MTD-EX        TO  WS-QLN-MTD-EX.
           INSPECT WS-QLN-MTD-EX REPLACING LEADING "+" BY " ".   

           IF LN-LOANDATE  = 0
              MOVE 19000101 TO LN-LOANDATE.
           MOVE LN-LOANDATE TO WS-QLN-LOANDATE.
           IF LN-ORIG-1STPYDATE  = 0
              MOVE 19000101 TO LN-ORIG-1STPYDATE.
           MOVE LN-ORIG-1STPYDATE TO WS-QLN-ORIG-1STPYDATE.
           IF LN-1STPYDATE  = 0
              MOVE 19000101 TO LN-1STPYDATE.
           MOVE LN-1STPYDATE TO WS-QLN-1STPYDATE.
           IF LN-PLDATE     = 0
              MOVE 19000101 TO LN-PLDATE.
           MOVE LN-PLDATE TO WS-QLN-PLDATE.
           IF LN-POFFDATE   = 0
              MOVE 19000101 TO LN-POFFDATE.
           MOVE LN-POFFDATE TO WS-QLN-POFFDATE.
           IF LN-BNKRPTDATE = 0
              MOVE 19000101 TO LN-BNKRPTDATE.
           MOVE LN-BNKRPTDATE TO WS-QLN-BNKRPTDATE.
           IF LN-REPODATE = 0
              MOVE 19000101 TO LN-REPODATE.
           MOVE LN-REPODATE TO WS-QLN-REPODATE.
           IF LN-REDEEMDATE = 0
              MOVE 19000101 TO LN-REDEEMDATE.
           MOVE LN-REDEEMDATE TO WS-QLN-REDEEMDATE.
           IF LN-TRW-DOFD-DATE = 0
              MOVE 19000101 TO LN-TRW-DOFD-DATE.
           MOVE LN-TRW-DOFD-DATE TO WS-QLN-TRW-DOFD-DATE.
           IF LN-DATE-PAID-LAST = 0
              MOVE 19000101 TO LN-DATE-PAID-LAST.
           MOVE LN-DATE-PAID-LAST TO WS-QLN-DATE-PAID-LAST.

           STRING  QLN-OWNBR               ,PIPE-1,
                   QLN-ACCTNO              ,PIPE-1,
                   QLN-CLASS               ,PIPE-1,
                   QLN-SUBCLASS            ,PIPE-1,
                   QLN-LAWCODE             ,PIPE-1,
                   QLN-ORGST               ,PIPE-1,
                   QLN-SPRCLASS            ,PIPE-1,
                   QLN-SRCD                ,PIPE-1,
                   QLN-POCD                ,PIPE-1,
                   QLN-PLCD                ,PIPE-1,
                   QLN-PLTYPE              ,PIPE-1,
                   QLN-SERVACCT            ,PIPE-1,
                   QLN-PAY-SCHLD-FG        ,PIPE-1,
                   WS-QLN-LOANDATE         ,PIPE-1,
                   WS-QLN-ORIG-1STPYDATE   ,PIPE-1,
                   WS-QLN-1STPYDATE        ,PIPE-1,
                   WS-QLN-PLDATE           ,PIPE-1,
                   WS-QLN-POFFDATE         ,PIPE-1, 
                   WS-QLN-BNKRPTDATE       ,PIPE-1,
                   WS-QLN-REPODATE         ,PIPE-1,
                   WS-QLN-REDEEMDATE       ,PIPE-1,
                   WS-QLN-TRW-DOFD-DATE    ,PIPE-1,
                   WS-QLN-LNAMT            ,PIPE-1, 
                   QLN-ORGTERM             ,PIPE-1,
                   WS-QLN-1STPYAMT         ,PIPE-1,
                   QLN-NOREGPY             ,PIPE-1,  
                   WS-QLN-REGPYAMT         ,PIPE-1, 
                   WS-QLN-LASTPYAMT        ,PIPE-1,
                   QLN-PY-YYYYMM-RECDEL    ,PIPE-1,
                   WS-QLN-MTD-PAYS-RECDEL  ,PIPE-1,
                   WS-QLN-DLNO             ,PIPE-1,
                   WS-QLN-CURBAL           ,PIPE-1, 
                   WS-QLN-INTBAL           ,PIPE-1, 
                   WS-QLN-LCBAL            ,PIPE-1,  
                   WS-QLN-OTHBAL           ,PIPE-1, 
                   WS-QLN-OT2BAL           ,PIPE-1, 
                   WS-QLN-MONTHEND-BAL     ,PIPE-1,
                   WS-QLN-ANTICERN-1       ,PIPE-1,
                   WS-QLN-ANTICERN-2       ,PIPE-1,
                   WS-QLN-ANTICERN-3       ,PIPE-1,
                   WS-QLN-ANTICERN-4       ,PIPE-1,
                   WS-QLN-ANTICERN-5       ,PIPE-1,
                   WS-QLN-ANTICERN-6       ,PIPE-1,
                   WS-QLN-ANTICERN-7       ,PIPE-1,
                   WS-QLN-ANTICERN-8       ,PIPE-1,
                   WS-QLN-ANTICERN-9       ,PIPE-1,
                   WS-QLN-ANTICERN-10      ,PIPE-1,
                   WS-QLN-ANTICADJ-1       ,PIPE-1,
                   WS-QLN-ANTICADJ-2       ,PIPE-1,
                   WS-QLN-ANTICADJ-3       ,PIPE-1,
                   WS-QLN-ANTICADJ-4       ,PIPE-1,
                   WS-QLN-ANTICADJ-5       ,PIPE-1,
                   WS-QLN-ANTICADJ-6       ,PIPE-1,
                   WS-QLN-ANTICADJ-7       ,PIPE-1,
                   WS-QLN-ANTICADJ-8       ,PIPE-1,
                   WS-QLN-ANTICADJ-9       ,PIPE-1,
                   WS-QLN-ANTICADJ-10      ,PIPE-1,
                   WS-QLN-UFERN-1          ,PIPE-1, 
                   WS-QLN-UFERN-2          ,PIPE-1, 
                   WS-QLN-UFERN-3          ,PIPE-1, 
                   WS-QLN-UFERN-4          ,PIPE-1, 
                   WS-QLN-UFERN-5          ,PIPE-1, 
                   WS-QLN-UFERN-6          ,PIPE-1, 
                   WS-QLN-UFERN-7          ,PIPE-1, 
                   WS-QLN-UFERN-8          ,PIPE-1, 
                   WS-QLN-UFERN-9          ,PIPE-1, 
                   WS-QLN-UFERN-10         ,PIPE-1,
                   WS-QLN-ACCERN-1         ,PIPE-1,
                   WS-QLN-ACCERN-2         ,PIPE-1,
                   WS-QLN-ACCERN-3         ,PIPE-1,
                   WS-QLN-ACCERN-4         ,PIPE-1,
                   WS-QLN-ACCERN-5         ,PIPE-1,
                   WS-QLN-ACCERN-6         ,PIPE-1,
                   WS-QLN-ACCERN-7         ,PIPE-1,
                   WS-QLN-ACCERN-8         ,PIPE-1,
                   WS-QLN-ACCERN-9         ,PIPE-1,
                   WS-QLN-ACCERN-10        ,PIPE-1, 
                   WS-QLN-TOTPAYMNTD       ,PIPE-1,
                   WS-QLN-TOTNODEF         ,PIPE-1,  
                   WS-QLN-YTDNODEF         ,PIPE-1,  
                   WS-QLN-YTDNODEFACT      ,PIPE-1,
                   WS-QLN-TOTNOALDEL       ,PIPE-1,
                   WS-QLN-YTDNOALDEL       ,PIPE-1,
                   WS-QLN-DATE-PAID-LAST   ,PIPE-1,
                   WS-QLN-PRLNAMT          ,PIPE-1,
                   WS-QLN-PRPDUE-DAYS      ,PIPE-1,
                   QLN-SUM-ORGTERM         ,PIPE-1,
                   QLN-UNITPER-CD          ,PIPE-1,
                   QLN-UNITPER-FREQ        ,PIPE-1,
                   QLN-REPOCD              ,PIPE-1,
                   WS-QLN-MTD-PRIN         ,PIPE-1,
                   WS-QLN-MTD-LC           ,PIPE-1,
                   WS-QLN-MTD-INT          ,PIPE-1,
                   WS-QLN-MTD-OT           ,PIPE-1,
                   WS-QLN-MTD-EX           ,PIPE-1,
                   QLN-BMO-CON-STATUS      ,PIPE-1,
                   QLN-BMO-REC-STATUS      ,PIPE-1,
                   QLN-CONPROFILE-1        ,PIPE-1,
                   QLN-CONPROFILE-2        ,PIPE-1,
                   QLN-CONPROFILE-3        ,PIPE-1,
                   QLN-CONPROFILE-4        ,PIPE-1,
                   QLN-CONPROFILE-5        ,PIPE-1,
                   QLN-RECPROFILE-1        ,PIPE-1,
                   QLN-RECPROFILE-2        ,PIPE-1,
                   QLN-RECPROFILE-3        ,PIPE-1,
                   QLN-RECPROFILE-4        ,PIPE-1,
                   QLN-RECPROFILE-5
              DELIMITED BY SIZE INTO LNDATA-OUT-REC.

       CLEAR-WK-FILES.
      * CALL SCRIPT TO REMOVE (IF ANY) EOMALL FILES FROM WORK DIR
      *  FOR THIS GROUP/BRANCH REQUEST
           MOVE SPACES TO TCLP-SHELL-PARMS.
           STRING EOMALL-OUTPUT-DIR, " ", WS-BR-GR-TEXT
             DELIMITED BY "  " INTO TCLP-SHELL-PARMS.
           CALL "C$TOLOWER" USING TASC-SCRIPT VALUE 20.
           CALL "C$TOLOWER" USING TASC-EC VALUE 3.
           MOVE TCLP-SHELL-COMMAND  TO SYSTEM-BUF.
           PERFORM SYSTEM-CALL.

       UPDATE-STAT-FILE.
           PERFORM OPEN-STAT-FILE.
           PERFORM WRITE-STAT-FILE.
           PERFORM CLOSE-STAT-FILE.

      *****************************************************************
      *
      *    I O - R O U T I N E
      *
      *****************************************************************
       IO-ROUTINE SECTION.
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBLP/LPTYGS_SQL.CPY".
       COPY "LIBLP/LPTRXGS_SQL.CPY".
       COPY "LIBLP/LPTRWGS_SQL.CPY".
       COPY "LIBLP/LPTRSGS_SQL.CPY".
       COPY "LIBLP/LPTRPGS_SQL.CPY".
       COPY "LIBLP/LPTRLGS_SQL.CPY".
       COPY "LIBLP/LPTRKGS_SQL.CPY".
       COPY "LIBLP/LPTRJGS_SQL.CPY".
       COPY "LIBLP/LPTRIGS_SQL.CPY".
       COPY "LIBLP/LPTRDGS_SQL.CPY".
       COPY "LIBLP/LPTRCGS_SQL.CPY".
       COPY "LIBLP/LPTRBGS_SQL.CPY".
       COPY "LIBLP/LPSPGS_SQL.CPY".
       COPY "LIBLP/LPLTPGS_SQL.CPY".
       COPY "LIBLP/LPLNGS_SQL.CPY".
       COPY "LIBLP/LPDSGS_SQL.CPY".
       COPY "LIBLP/LPCAGS_SQL.CPY".
       COPY "LIBLP/LPBYGS_SQL.CPY".
       COPY "LIBGB/GBRCGS_SQL.CPY".
       COPY "LIBGB/GBGRGS_SQL.CPY".
       COPY "LIBGB/GBGBGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBPRINT.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBGB/GBGR1RN.CPY".
       COPY "LIBLP/LPVBY1IN.CPY".   *> VISION FILE
       COPY "LIBGB/GBVRC1IN.CPY".   *> VISION FILE
       COPY "LIBGB/GBVRC2RN.CPY".   *> VISION FILE
       COPY "LIBGB/GBVGBIN.CPY".    *> VISION FILE
       COPY "LIBLP/LPSP1RN.CPY".
       COPY "LIBLP/LPLTP1RN.CPY".

      * COPY "LIBLP/LPBY1IN.CPY".
      * COPY "LIBGB/GBRC2RN.CPY".
      * COPY "LIBGB/GBRC1IN.CPY".
      * COPY "LIBGB/GBGBIN.CPY".

       COPY "LIBLP/LPBY1RN.CPY".
       COPY "LIBGB/GBRC1RN.CPY".
       COPY "LIBGB/GBGBRN.CPY".
 
      *-----------------------------------------------------------------


      ** COPY "LIBLP/LPDS1IN.CPY".                         *> S35Q-104
      ** COPYMEMBER: LIBLP/LPDS1IN
      * LOAD-DS1-FILE.
      **- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- 
      *     IF ( DS-PATH-OVERRIDE = "Y" ) 
      *        NEXT SENTENCE
      *     ELSE
      **- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - 
      *     IF ( DS-PATH-OVERRIDE = SPACES ) 
      *        MOVE EXT-FILPATH-BASE        TO DS-ALL-BASE
      *        MOVE EXT-FILPATH-MACHINE     TO DS-ALL-MACHINE
      *        MOVE EXT-FILPATH-DATA        TO DS-ALL-DATA
      *        MOVE DS-ALL-PATH             TO DS-PATH
      *     ELSE
      **- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - 
      *     IF ( DS-PATH-OVERRIDE = "B" ) 
      *        MOVE FILPATH-BASE            TO DS-ALL-BASE
      *        MOVE FILPATH-MACHINE         TO DS-ALL-MACHINE
      *        MOVE FILPATH-DATA            TO DS-ALL-DATA
      *        MOVE DS-ALL-PATH             TO DS-PATH.

      **----------------------------------------------------------------
      * OPEN-DS1-FILE.
      *     PERFORM LOAD-DS1-FILE.
      *     PERFORM OPEN-IT.
      *     MOVE DS-PATH-SQL TO E-FILE.

      **    IF ( SQL-CONNECT-STAT-BAD )
      **       PERFORM SQL-CONNECT.

      **----------------------------------------------------------------
      * START-DS1-FILE.
      *     PERFORM START-IT.
      *     MOVE DS-PATH-OWNBR  TO DS-BRANCH.
      *     MOVE DS-PATH-SQL    TO E-FILE.
      *     MOVE DS1-KEY        TO E-KEYX.

      *     IF ( DS1-CURSOR-STAT-GOOD )
      *        PERFORM CLOSE-DS1-FILE.

      *     IF ( QDS1-WEND-CLASS = ZEROES    ) OR
      *        ( QDS1-WEND-CLASS NOT NUMERIC )
      *        MOVE DS-PATH-OWNBR        TO QDS1-WBEG-BRANCH
      *        MOVE DS-PATH-OWNBR        TO QDS1-WEND-BRANCH
      *        MOVE DS-DATE-CCYYMM       TO QDS1-WBEG-DATE-CCYYMM
      *        MOVE ALL "9"              TO QDS1-WEND-DATE-CCYYMM
      *                                     QDS1-WEND-NUM
      *                                     QDS1-WEND-CLASS
      *        MOVE DS-NUM               TO QDS1-WBEG-NUM
      *        MOVE DS-CLASS             TO QDS1-WBEG-CLASS.


      *     EXEC SQL
      *      DECLARE DS1_CURSOR CURSOR FOR
      *      SELECT DSFILE.DS_DATE_CCYYMM,
      *             DSFILE.DS_BRANCH,
      *             DSFILE.DS_NUM,
      *             DSFILE.DS_CLASS,
      *             DSFILE.DS_DLRVPRIN_1,
      *             DSFILE.DS_DLRVPRIN_2,
      *             DSFILE.DS_DLRVPRIN_3,
      *             DSFILE.DS_DLRVCHGS_1,
      *             DSFILE.DS_DLRVCHGS_2,
      *             DSFILE.DS_DLRVCHGS_3,
      *             DSFILE.DS_DLPARVHELD,
      *             DSFILE.DS_UNPAIDCK,
      *             DSFILE.DS_DLDISC,
      *             DSFILE.DS_DLPARVPAID,
      *             DSFILE.DS_SUMWGTTM,
      *             DSFILE.DS_SUMWGTAV,
      *             DSFILE.DS_SUMWGTTA,
      *             DSFILE.DS_RCOURSE_UNITS_1,
      *             DSFILE.DS_RCOURSE_UNITS_2,
      *             DSFILE.DS_RCOURSE_UNITS_3,
      *             DSFILE.DS_RCOURSE_AMT_1,
      *             DSFILE.DS_RCOURSE_AMT_2,
      *             DSFILE.DS_RCOURSE_AMT_3,
      *             DSFILE.DS_NPURUNIT_1,
      *             DSFILE.DS_NPURUNIT_2,
      *             DSFILE.DS_NPURUNIT_3,
      *             DSFILE.DS_NPURAMT_1,
      *             DSFILE.DS_NPURAMT_2,
      *             DSFILE.DS_NPURAMT_3,
      *             DSFILE.DS_OPURUNIT_1,
      *             DSFILE.DS_OPURUNIT_2,
      *             DSFILE.DS_OPURUNIT_3,
      *             DSFILE.DS_OPURAMT_1,
      *             DSFILE.DS_OPURAMT_2,
      *             DSFILE.DS_OPURAMT_3,
      *             DSFILE.DS_RNEWUNIT_1,
      *             DSFILE.DS_RNEWUNIT_2,
      *             DSFILE.DS_RNEWUNIT_3,
      *             DSFILE.DS_RNEWAMT_1,
      *             DSFILE.DS_RNEWAMT_2,
      *             DSFILE.DS_RNEWAMT_3,
      *             DSFILE.DS_PAIDUNIT_1,
      *             DSFILE.DS_PAIDUNIT_2,
      *             DSFILE.DS_PAIDUNIT_3,
      *             DSFILE.DS_PAIDAMT_1,
      *             DSFILE.DS_PAIDAMT_2,
      *             DSFILE.DS_PAIDAMT_3,
      *             DSFILE.DS_TODLUNIT_1,
      *             DSFILE.DS_TODLUNIT_2,
      *             DSFILE.DS_TODLUNIT_3,
      *             DSFILE.DS_TODLAMT_1,
      *             DSFILE.DS_TODLAMT_2,
      *             DSFILE.DS_TODLAMT_3,
      *             DSFILE.DS_OUTSUNIT_1,
      *             DSFILE.DS_OUTSUNIT_2,
      *             DSFILE.DS_OUTSUNIT_3,
      *             DSFILE.DS_OUTSAMT_1,
      *             DSFILE.DS_OUTSAMT_2,
      *             DSFILE.DS_OUTSAMT_3,
      *             DSFILE.DS_PLOSUNIT_1,
      *             DSFILE.DS_PLOSUNIT_2,
      *             DSFILE.DS_PLOSUNIT_3,
      *             DSFILE.DS_PLOSAMT_1,
      *             DSFILE.DS_PLOSAMT_2,
      *             DSFILE.DS_PLOSAMT_3,
      *             DSFILE.DS_RCPLUNIT_1,
      *             DSFILE.DS_RCPLUNIT_2,
      *             DSFILE.DS_RCPLUNIT_3,
      *             DSFILE.DS_RCPLAMT_1,
      *             DSFILE.DS_RCPLAMT_2,
      *             DSFILE.DS_RCPLAMT_3,
      *             DSFILE.DS_JDOSUNIT_1,
      *             DSFILE.DS_JDOSUNIT_2,
      *             DSFILE.DS_JDOSUNIT_3,
      *             DSFILE.DS_JDOSAMT_1,
      *             DSFILE.DS_JDOSAMT_2,
      *             DSFILE.DS_JDOSAMT_3,
      *             CAST(DSFILE.DS_DELUNEARN_DATE AS VARCHAR(10)),
      *             DSFILE.DS_PROCEEDS_1,
      *             DSFILE.DS_PROCEEDS_2,
      *             DSFILE.DS_NO_REPO_1_1_1,
      *             DSFILE.DS_AMT_REPO_1_1_1,
      *             DSFILE.DS_NO_REPO_1_1_2,
      *             DSFILE.DS_AMT_REPO_1_1_2,
      *             DSFILE.DS_NO_REPO_1_1_3,
      *             DSFILE.DS_AMT_REPO_1_1_3,
      *             DSFILE.DS_NO_REPO_1_1_4,
      *             DSFILE.DS_AMT_REPO_1_1_4,
      *             DSFILE.DS_NO_REPO_1_1_5,
      *             DSFILE.DS_AMT_REPO_1_1_5,
      *             DSFILE.DS_NO_REPO_1_2_1,
      *             DSFILE.DS_AMT_REPO_1_2_1,
      *             DSFILE.DS_NO_REPO_1_2_2,
      *             DSFILE.DS_AMT_REPO_1_2_2,
      *             DSFILE.DS_NO_REPO_1_2_3,
      *             DSFILE.DS_AMT_REPO_1_2_3,
      *             DSFILE.DS_NO_REPO_1_2_4,
      *             DSFILE.DS_AMT_REPO_1_2_4,
      *             DSFILE.DS_NO_REPO_1_2_5,
      *             DSFILE.DS_AMT_REPO_1_2_5,
      *             DSFILE.DS_NO_REPO_2_1_1,
      *             DSFILE.DS_AMT_REPO_2_1_1,
      *             DSFILE.DS_NO_REPO_2_1_2,
      *             DSFILE.DS_AMT_REPO_2_1_2,
      *             DSFILE.DS_NO_REPO_2_1_3,
      *             DSFILE.DS_AMT_REPO_2_1_3,
      *             DSFILE.DS_NO_REPO_2_1_4,
      *             DSFILE.DS_AMT_REPO_2_1_4,
      *             DSFILE.DS_NO_REPO_2_1_5,
      *             DSFILE.DS_AMT_REPO_2_1_5,
      *             DSFILE.DS_NO_REPO_2_2_1,
      *             DSFILE.DS_AMT_REPO_2_2_1,
      *             DSFILE.DS_NO_REPO_2_2_2,
      *             DSFILE.DS_AMT_REPO_2_2_2,
      *             DSFILE.DS_NO_REPO_2_2_3,
      *             DSFILE.DS_AMT_REPO_2_2_3,
      *             DSFILE.DS_NO_REPO_2_2_4,
      *             DSFILE.DS_AMT_REPO_2_2_4,
      *             DSFILE.DS_NO_REPO_2_2_5,
      *             DSFILE.DS_AMT_REPO_2_2_5,
      *             DSFILE.DS_NO_REPO_3_1_1,
      *             DSFILE.DS_AMT_REPO_3_1_1,
      *             DSFILE.DS_NO_REPO_3_1_2,
      *             DSFILE.DS_AMT_REPO_3_1_2,
      *             DSFILE.DS_NO_REPO_3_1_3,
      *             DSFILE.DS_AMT_REPO_3_1_3,
      *             DSFILE.DS_NO_REPO_3_1_4,
      *             DSFILE.DS_AMT_REPO_3_1_4,
      *             DSFILE.DS_NO_REPO_3_1_5,
      *             DSFILE.DS_AMT_REPO_3_1_5,
      *             DSFILE.DS_NO_REPO_3_2_1,
      *             DSFILE.DS_AMT_REPO_3_2_1,
      *             DSFILE.DS_NO_REPO_3_2_2,
      *             DSFILE.DS_AMT_REPO_3_2_2,
      *             DSFILE.DS_NO_REPO_3_2_3,
      *             DSFILE.DS_AMT_REPO_3_2_3,
      *             DSFILE.DS_NO_REPO_3_2_4,
      *             DSFILE.DS_AMT_REPO_3_2_4,
      *             DSFILE.DS_NO_REPO_3_2_5,
      *             DSFILE.DS_AMT_REPO_3_2_5,
      *             DSFILE.DS_UNITS_1_1_1,
      *             DSFILE.DS_OUTBAL_1_1_1,
      *             DSFILE.DS_UNITS_1_1_2,
      *             DSFILE.DS_OUTBAL_1_1_2,
      *             DSFILE.DS_UNITS_1_1_3,
      *             DSFILE.DS_OUTBAL_1_1_3,
      *             DSFILE.DS_UNITS_1_1_4,
      *             DSFILE.DS_OUTBAL_1_1_4,
      *             DSFILE.DS_UNITS_1_1_5,
      *             DSFILE.DS_OUTBAL_1_1_5,
      *             DSFILE.DS_UNITS_1_1_6,
      *             DSFILE.DS_OUTBAL_1_1_6,
      *             DSFILE.DS_UNITS_1_1_7,
      *             DSFILE.DS_OUTBAL_1_1_7,
      *             DSFILE.DS_UNITS_1_1_8,
      *             DSFILE.DS_OUTBAL_1_1_8,
      *             DSFILE.DS_UNITS_1_1_9,
      *             DSFILE.DS_OUTBAL_1_1_9,
      *             DSFILE.DS_UNITS_1_1_10,
      *             DSFILE.DS_OUTBAL_1_1_10,
      *             DSFILE.DS_UNITS_1_2_1,
      *             DSFILE.DS_OUTBAL_1_2_1,
      *             DSFILE.DS_UNITS_1_2_2,
      *             DSFILE.DS_OUTBAL_1_2_2,
      *             DSFILE.DS_UNITS_1_2_3,
      *             DSFILE.DS_OUTBAL_1_2_3,
      *             DSFILE.DS_UNITS_1_2_4,
      *             DSFILE.DS_OUTBAL_1_2_4,
      *             DSFILE.DS_UNITS_1_2_5,
      *             DSFILE.DS_OUTBAL_1_2_5,
      *             DSFILE.DS_UNITS_1_2_6,
      *             DSFILE.DS_OUTBAL_1_2_6,
      *             DSFILE.DS_UNITS_1_2_7,
      *             DSFILE.DS_OUTBAL_1_2_7,
      *             DSFILE.DS_UNITS_1_2_8,
      *             DSFILE.DS_OUTBAL_1_2_8,
      *             DSFILE.DS_UNITS_1_2_9,
      *             DSFILE.DS_OUTBAL_1_2_9,
      *             DSFILE.DS_UNITS_1_2_10,
      *             DSFILE.DS_OUTBAL_1_2_10,
      *             DSFILE.DS_UNITS_2_1_1,
      *             DSFILE.DS_OUTBAL_2_1_1,
      *             DSFILE.DS_UNITS_2_1_2,
      *             DSFILE.DS_OUTBAL_2_1_2,
      *             DSFILE.DS_UNITS_2_1_3,
      *             DSFILE.DS_OUTBAL_2_1_3,
      *             DSFILE.DS_UNITS_2_1_4,
      *             DSFILE.DS_OUTBAL_2_1_4,
      *             DSFILE.DS_UNITS_2_1_5,
      *             DSFILE.DS_OUTBAL_2_1_5,
      *             DSFILE.DS_UNITS_2_1_6,
      *             DSFILE.DS_OUTBAL_2_1_6,
      *             DSFILE.DS_UNITS_2_1_7,
      *             DSFILE.DS_OUTBAL_2_1_7,
      *             DSFILE.DS_UNITS_2_1_8,
      *             DSFILE.DS_OUTBAL_2_1_8,
      *             DSFILE.DS_UNITS_2_1_9,
      *             DSFILE.DS_OUTBAL_2_1_9,
      *             DSFILE.DS_UNITS_2_1_10,
      *             DSFILE.DS_OUTBAL_2_1_10,
      *             DSFILE.DS_UNITS_2_2_1,
      *             DSFILE.DS_OUTBAL_2_2_1,
      *             DSFILE.DS_UNITS_2_2_2,
      *             DSFILE.DS_OUTBAL_2_2_2,
      *             DSFILE.DS_UNITS_2_2_3,
      *             DSFILE.DS_OUTBAL_2_2_3,
      *             DSFILE.DS_UNITS_2_2_4,
      *             DSFILE.DS_OUTBAL_2_2_4,
      *             DSFILE.DS_UNITS_2_2_5,
      *             DSFILE.DS_OUTBAL_2_2_5,
      *             DSFILE.DS_UNITS_2_2_6,
      *             DSFILE.DS_OUTBAL_2_2_6,
      *             DSFILE.DS_UNITS_2_2_7,
      *             DSFILE.DS_OUTBAL_2_2_7,
      *             DSFILE.DS_UNITS_2_2_8,
      *             DSFILE.DS_OUTBAL_2_2_8,
      *             DSFILE.DS_UNITS_2_2_9,
      *             DSFILE.DS_OUTBAL_2_2_9,
      *             DSFILE.DS_UNITS_2_2_10,
      *             DSFILE.DS_OUTBAL_2_2_10,
      *             DSFILE.DS_UNITS_3_1_1,
      *             DSFILE.DS_OUTBAL_3_1_1,
      *             DSFILE.DS_UNITS_3_1_2,
      *             DSFILE.DS_OUTBAL_3_1_2,
      *             DSFILE.DS_UNITS_3_1_3,
      *             DSFILE.DS_OUTBAL_3_1_3,
      *             DSFILE.DS_UNITS_3_1_4,
      *             DSFILE.DS_OUTBAL_3_1_4,
      *             DSFILE.DS_UNITS_3_1_5,
      *             DSFILE.DS_OUTBAL_3_1_5,
      *             DSFILE.DS_UNITS_3_1_6,
      *             DSFILE.DS_OUTBAL_3_1_6,
      *             DSFILE.DS_UNITS_3_1_7,
      *             DSFILE.DS_OUTBAL_3_1_7,
      *             DSFILE.DS_UNITS_3_1_8,
      *             DSFILE.DS_OUTBAL_3_1_8,
      *             DSFILE.DS_UNITS_3_1_9,
      *             DSFILE.DS_OUTBAL_3_1_9,
      *             DSFILE.DS_UNITS_3_1_10,
      *             DSFILE.DS_OUTBAL_3_1_10,
      *             DSFILE.DS_UNITS_3_2_1,
      *             DSFILE.DS_OUTBAL_3_2_1,
      *             DSFILE.DS_UNITS_3_2_2,
      *             DSFILE.DS_OUTBAL_3_2_2,
      *             DSFILE.DS_UNITS_3_2_3,
      *             DSFILE.DS_OUTBAL_3_2_3,
      *             DSFILE.DS_UNITS_3_2_4,
      *             DSFILE.DS_OUTBAL_3_2_4,
      *             DSFILE.DS_UNITS_3_2_5,
      *             DSFILE.DS_OUTBAL_3_2_5,
      *             DSFILE.DS_UNITS_3_2_6,
      *             DSFILE.DS_OUTBAL_3_2_6,
      *             DSFILE.DS_UNITS_3_2_7,
      *             DSFILE.DS_OUTBAL_3_2_7,
      *             DSFILE.DS_UNITS_3_2_8,
      *             DSFILE.DS_OUTBAL_3_2_8,
      *             DSFILE.DS_UNITS_3_2_9,
      *             DSFILE.DS_OUTBAL_3_2_9,
      *             DSFILE.DS_UNITS_3_2_10,
      *             DSFILE.DS_OUTBAL_3_2_10,
      *             DSFILE.DS_UNITS_BNKRPT_1_1_1,
      *             DSFILE.DS_OUTBAL_BNKRPT_1_1_1,
      *             DSFILE.DS_UNITS_BNKRPT_1_1_2,
      *             DSFILE.DS_OUTBAL_BNKRPT_1_1_2,
      *             DSFILE.DS_UNITS_BNKRPT_1_1_3,
      *             DSFILE.DS_OUTBAL_BNKRPT_1_1_3,
      *             DSFILE.DS_UNITS_BNKRPT_1_1_4,
      *             DSFILE.DS_OUTBAL_BNKRPT_1_1_4,
      *             DSFILE.DS_UNITS_BNKRPT_1_1_5,
      *             DSFILE.DS_OUTBAL_BNKRPT_1_1_5,
      *             DSFILE.DS_UNITS_BNKRPT_1_1_6,
      *             DSFILE.DS_OUTBAL_BNKRPT_1_1_6,
      *             DSFILE.DS_UNITS_BNKRPT_1_1_7,
      *             DSFILE.DS_OUTBAL_BNKRPT_1_1_7,
      *             DSFILE.DS_UNITS_BNKRPT_1_1_8,
      *             DSFILE.DS_OUTBAL_BNKRPT_1_1_8,
      *             DSFILE.DS_UNITS_BNKRPT_1_1_9,
      *             DSFILE.DS_OUTBAL_BNKRPT_1_1_9,
      *             DSFILE.DS_UNITS_BNKRPT_1_1_10,
      *             DSFILE.DS_OUTBAL_BNKRPT_1_1_10,
      *             DSFILE.DS_UNITS_BNKRPT_1_2_1,
      *             DSFILE.DS_OUTBAL_BNKRPT_1_2_1,
      *             DSFILE.DS_UNITS_BNKRPT_1_2_2,
      *             DSFILE.DS_OUTBAL_BNKRPT_1_2_2,
      *             DSFILE.DS_UNITS_BNKRPT_1_2_3,
      *             DSFILE.DS_OUTBAL_BNKRPT_1_2_3,
      *             DSFILE.DS_UNITS_BNKRPT_1_2_4,
      *             DSFILE.DS_OUTBAL_BNKRPT_1_2_4,
      *             DSFILE.DS_UNITS_BNKRPT_1_2_5,
      *             DSFILE.DS_OUTBAL_BNKRPT_1_2_5,
      *             DSFILE.DS_UNITS_BNKRPT_1_2_6,
      *             DSFILE.DS_OUTBAL_BNKRPT_1_2_6,
      *             DSFILE.DS_UNITS_BNKRPT_1_2_7,
      *             DSFILE.DS_OUTBAL_BNKRPT_1_2_7,
      *             DSFILE.DS_UNITS_BNKRPT_1_2_8,
      *             DSFILE.DS_OUTBAL_BNKRPT_1_2_8,
      *             DSFILE.DS_UNITS_BNKRPT_1_2_9,
      *             DSFILE.DS_OUTBAL_BNKRPT_1_2_9,
      *             DSFILE.DS_UNITS_BNKRPT_1_2_10,
      *             DSFILE.DS_OUTBAL_BNKRPT_1_2_10,
      *             DSFILE.DS_UNITS_BNKRPT_2_1_1,
      *             DSFILE.DS_OUTBAL_BNKRPT_2_1_1,
      *             DSFILE.DS_UNITS_BNKRPT_2_1_2,
      *             DSFILE.DS_OUTBAL_BNKRPT_2_1_2,
      *             DSFILE.DS_UNITS_BNKRPT_2_1_3,
      *             DSFILE.DS_OUTBAL_BNKRPT_2_1_3,
      *             DSFILE.DS_UNITS_BNKRPT_2_1_4,
      *             DSFILE.DS_OUTBAL_BNKRPT_2_1_4,
      *             DSFILE.DS_UNITS_BNKRPT_2_1_5,
      *             DSFILE.DS_OUTBAL_BNKRPT_2_1_5,
      *             DSFILE.DS_UNITS_BNKRPT_2_1_6,
      *             DSFILE.DS_OUTBAL_BNKRPT_2_1_6,
      *             DSFILE.DS_UNITS_BNKRPT_2_1_7,
      *             DSFILE.DS_OUTBAL_BNKRPT_2_1_7,
      *             DSFILE.DS_UNITS_BNKRPT_2_1_8,
      *             DSFILE.DS_OUTBAL_BNKRPT_2_1_8,
      *             DSFILE.DS_UNITS_BNKRPT_2_1_9,
      *             DSFILE.DS_OUTBAL_BNKRPT_2_1_9,
      *             DSFILE.DS_UNITS_BNKRPT_2_1_10,
      *             DSFILE.DS_OUTBAL_BNKRPT_2_1_10,
      *             DSFILE.DS_UNITS_BNKRPT_2_2_1,
      *             DSFILE.DS_OUTBAL_BNKRPT_2_2_1,
      *             DSFILE.DS_UNITS_BNKRPT_2_2_2,
      *             DSFILE.DS_OUTBAL_BNKRPT_2_2_2,
      *             DSFILE.DS_UNITS_BNKRPT_2_2_3,
      *             DSFILE.DS_OUTBAL_BNKRPT_2_2_3,
      *             DSFILE.DS_UNITS_BNKRPT_2_2_4,
      *             DSFILE.DS_OUTBAL_BNKRPT_2_2_4,
      *             DSFILE.DS_UNITS_BNKRPT_2_2_5,
      *             DSFILE.DS_OUTBAL_BNKRPT_2_2_5,
      *             DSFILE.DS_UNITS_BNKRPT_2_2_6,
      *             DSFILE.DS_OUTBAL_BNKRPT_2_2_6,
      *             DSFILE.DS_UNITS_BNKRPT_2_2_7,
      *             DSFILE.DS_OUTBAL_BNKRPT_2_2_7,
      *             DSFILE.DS_UNITS_BNKRPT_2_2_8,
      *             DSFILE.DS_OUTBAL_BNKRPT_2_2_8,
      *             DSFILE.DS_UNITS_BNKRPT_2_2_9,
      *             DSFILE.DS_OUTBAL_BNKRPT_2_2_9,
      *             DSFILE.DS_UNITS_BNKRPT_2_2_10,
      *             DSFILE.DS_OUTBAL_BNKRPT_2_2_10,
      *             DSFILE.DS_UNITS_BNKRPT_3_1_1,
      *             DSFILE.DS_OUTBAL_BNKRPT_3_1_1,
      *             DSFILE.DS_UNITS_BNKRPT_3_1_2,
      *             DSFILE.DS_OUTBAL_BNKRPT_3_1_2,
      *             DSFILE.DS_UNITS_BNKRPT_3_1_3,
      *             DSFILE.DS_OUTBAL_BNKRPT_3_1_3,
      *             DSFILE.DS_UNITS_BNKRPT_3_1_4,
      *             DSFILE.DS_OUTBAL_BNKRPT_3_1_4,
      *             DSFILE.DS_UNITS_BNKRPT_3_1_5,
      *             DSFILE.DS_OUTBAL_BNKRPT_3_1_5,
      *             DSFILE.DS_UNITS_BNKRPT_3_1_6,
      *             DSFILE.DS_OUTBAL_BNKRPT_3_1_6,
      *             DSFILE.DS_UNITS_BNKRPT_3_1_7,
      *             DSFILE.DS_OUTBAL_BNKRPT_3_1_7,
      *             DSFILE.DS_UNITS_BNKRPT_3_1_8,
      *             DSFILE.DS_OUTBAL_BNKRPT_3_1_8,
      *             DSFILE.DS_UNITS_BNKRPT_3_1_9,
      *             DSFILE.DS_OUTBAL_BNKRPT_3_1_9,
      *             DSFILE.DS_UNITS_BNKRPT_3_1_10,
      *             DSFILE.DS_OUTBAL_BNKRPT_3_1_10,
      *             DSFILE.DS_UNITS_BNKRPT_3_2_1,
      *             DSFILE.DS_OUTBAL_BNKRPT_3_2_1,
      *             DSFILE.DS_UNITS_BNKRPT_3_2_2,
      *             DSFILE.DS_OUTBAL_BNKRPT_3_2_2,
      *             DSFILE.DS_UNITS_BNKRPT_3_2_3,
      *             DSFILE.DS_OUTBAL_BNKRPT_3_2_3,
      *             DSFILE.DS_UNITS_BNKRPT_3_2_4,
      *             DSFILE.DS_OUTBAL_BNKRPT_3_2_4,
      *             DSFILE.DS_UNITS_BNKRPT_3_2_5,
      *             DSFILE.DS_OUTBAL_BNKRPT_3_2_5,
      *             DSFILE.DS_UNITS_BNKRPT_3_2_6,
      *             DSFILE.DS_OUTBAL_BNKRPT_3_2_6,
      *             DSFILE.DS_UNITS_BNKRPT_3_2_7,
      *             DSFILE.DS_OUTBAL_BNKRPT_3_2_7,
      *             DSFILE.DS_UNITS_BNKRPT_3_2_8,
      *             DSFILE.DS_OUTBAL_BNKRPT_3_2_8,
      *             DSFILE.DS_UNITS_BNKRPT_3_2_9,
      *             DSFILE.DS_OUTBAL_BNKRPT_3_2_9,
      *             DSFILE.DS_UNITS_BNKRPT_3_2_10,
      *             DSFILE.DS_OUTBAL_BNKRPT_3_2_10,
      *             DSFILE.DS_UNEARNED_AMT_1_1_1,
      *             DSFILE.DS_UNEARNED_AMT_1_1_2,
      *             DSFILE.DS_UNEARNED_AMT_1_1_3,
      *             DSFILE.DS_UNEARNED_AMT_1_1_4,
      *             DSFILE.DS_UNEARNED_AMT_1_1_5,
      *             DSFILE.DS_UNEARNED_AMT_1_1_6,
      *             DSFILE.DS_UNEARNED_AMT_1_1_7,
      *             DSFILE.DS_UNEARNED_AMT_1_1_8,
      *             DSFILE.DS_UNEARNED_AMT_1_1_9,
      *             DSFILE.DS_UNEARNED_AMT_1_1_10,
      *             DSFILE.DS_UNEARNED_AMT_1_2_1,
      *             DSFILE.DS_UNEARNED_AMT_1_2_2,
      *             DSFILE.DS_UNEARNED_AMT_1_2_3,
      *             DSFILE.DS_UNEARNED_AMT_1_2_4,
      *             DSFILE.DS_UNEARNED_AMT_1_2_5,
      *             DSFILE.DS_UNEARNED_AMT_1_2_6,
      *             DSFILE.DS_UNEARNED_AMT_1_2_7,
      *             DSFILE.DS_UNEARNED_AMT_1_2_8,
      *             DSFILE.DS_UNEARNED_AMT_1_2_9,
      *             DSFILE.DS_UNEARNED_AMT_1_2_10,
      *             DSFILE.DS_UNEARNED_AMT_2_1_1,
      *             DSFILE.DS_UNEARNED_AMT_2_1_2,
      *             DSFILE.DS_UNEARNED_AMT_2_1_3,
      *             DSFILE.DS_UNEARNED_AMT_2_1_4,
      *             DSFILE.DS_UNEARNED_AMT_2_1_5,
      *             DSFILE.DS_UNEARNED_AMT_2_1_6,
      *             DSFILE.DS_UNEARNED_AMT_2_1_7,
      *             DSFILE.DS_UNEARNED_AMT_2_1_8,
      *             DSFILE.DS_UNEARNED_AMT_2_1_9,
      *             DSFILE.DS_UNEARNED_AMT_2_1_10,
      *             DSFILE.DS_UNEARNED_AMT_2_2_1,
      *             DSFILE.DS_UNEARNED_AMT_2_2_2,
      *             DSFILE.DS_UNEARNED_AMT_2_2_3,
      *             DSFILE.DS_UNEARNED_AMT_2_2_4,
      *             DSFILE.DS_UNEARNED_AMT_2_2_5,
      *             DSFILE.DS_UNEARNED_AMT_2_2_6,
      *             DSFILE.DS_UNEARNED_AMT_2_2_7,
      *             DSFILE.DS_UNEARNED_AMT_2_2_8,
      *             DSFILE.DS_UNEARNED_AMT_2_2_9,
      *             DSFILE.DS_UNEARNED_AMT_2_2_10,
      *             DSFILE.DS_UNEARNED_AMT_3_1_1,
      *             DSFILE.DS_UNEARNED_AMT_3_1_2,
      *             DSFILE.DS_UNEARNED_AMT_3_1_3,
      *             DSFILE.DS_UNEARNED_AMT_3_1_4,
      *             DSFILE.DS_UNEARNED_AMT_3_1_5,
      *             DSFILE.DS_UNEARNED_AMT_3_1_6,
      *             DSFILE.DS_UNEARNED_AMT_3_1_7,
      *             DSFILE.DS_UNEARNED_AMT_3_1_8,
      *             DSFILE.DS_UNEARNED_AMT_3_1_9,
      *             DSFILE.DS_UNEARNED_AMT_3_1_10,
      *             DSFILE.DS_UNEARNED_AMT_3_2_1,
      *             DSFILE.DS_UNEARNED_AMT_3_2_2,
      *             DSFILE.DS_UNEARNED_AMT_3_2_3,
      *             DSFILE.DS_UNEARNED_AMT_3_2_4,
      *             DSFILE.DS_UNEARNED_AMT_3_2_5,
      *             DSFILE.DS_UNEARNED_AMT_3_2_6,
      *             DSFILE.DS_UNEARNED_AMT_3_2_7,
      *             DSFILE.DS_UNEARNED_AMT_3_2_8,
      *             DSFILE.DS_UNEARNED_AMT_3_2_9,
      *             DSFILE.DS_UNEARNED_AMT_3_2_10,
      *             DSFILE.DS_DLDISC_VOL_1,
      *             DSFILE.DS_DLDISC_VOL_2,
      *             DSFILE.DS_DLDISC_VOL_3,
      *             DSFILE.DS_AFPDUEDL_MEMO,
      *             DSFILE.DS_UNPAID_MANUAL,
      *             CAST(DSFILE.DS_LTOUCH_DATE AS VARCHAR(10))
      *      FROM DBO.DSFILE
      *      WHERE DSFILE.DS_DATE_CCYYMM >= :QDS1-WBEG-DATE-CCYYMM
      *        AND DSFILE.DS_DATE_CCYYMM <= :QDS1-WEND-DATE-CCYYMM
      *        AND DSFILE.DS_BRANCH = :QDS1-WBEG-BRANCH
      *        AND DSFILE.DS_NUM >= :QDS1-WBEG-NUM
      *        AND DSFILE.DS_NUM <= :QDS1-WEND-NUM
      *        AND DSFILE.DS_CLASS >= :QDS1-WBEG-CLASS
      *        AND DSFILE.DS_CLASS <= :QDS1-WEND-CLASS
      *      ORDER BY DSFILE.DS_DATE_CCYYMM,
      *               DSFILE.DS_BRANCH,
      *               DSFILE.DS_NUM,
      *               DSFILE.DS_CLASS
      *     END-EXEC.

      *     EXEC SQL
      *         OPEN DS1_CURSOR
      *     END-EXEC.

      *     PERFORM SQL-IO-VALIDATION.

      *     MOVE ZEROES TO QDS1-WBEG-BRANCH 
      *                    QDS1-WEND-BRANCH
      *                    QDS1-WBEG-DATE-CCYYMM
      *                    QDS1-WEND-DATE-CCYYMM
      *                    QDS1-WBEG-NUM
      *                    QDS1-WEND-NUM
      *                    QDS1-WBEG-CLASS
      *                    QDS1-WEND-CLASS.
 
      *     MOVE STAT---GOOD TO DS1-CURSOR-STAT.

      **----------------------------------------------------------------
      * READ-DS1-FILE.
      *     PERFORM READ-IT.
      *     MOVE DS-PATH-OWNBR  TO DS-BRANCH. 
      *     MOVE DS-PATH-SQL    TO E-FILE.
      *     MOVE DS1-KEY        TO E-KEYX.
      *     INITIALIZE QDS-REC.  

      *     MOVE DS-DATE-CCYYMM  TO QDS-DATE-CCYYMM.
      *     MOVE DS-BRANCH       TO QDS-BRANCH.
      *     MOVE DS-NUM          TO QDS-NUM.
      *     MOVE DS-CLASS        TO QDS-CLASS.

      *     EXEC SQL
      *      SELECT DSFILE.DS_DATE_CCYYMM,
      *             DSFILE.DS_BRANCH,
      *             DSFILE.DS_NUM,
      *             DSFILE.DS_CLASS,
      *             DSFILE.DS_DLRVPRIN_1,
      *             DSFILE.DS_DLRVPRIN_2,
      *             DSFILE.DS_DLRVPRIN_3,
      *             DSFILE.DS_DLRVCHGS_1,
      *             DSFILE.DS_DLRVCHGS_2,
      *             DSFILE.DS_DLRVCHGS_3,
      *             DSFILE.DS_DLPARVHELD,
      *             DSFILE.DS_UNPAIDCK,
      *             DSFILE.DS_DLDISC,
      *             DSFILE.DS_DLPARVPAID,
      *             DSFILE.DS_SUMWGTTM,
      *             DSFILE.DS_SUMWGTAV,
      *             DSFILE.DS_SUMWGTTA,
      *             DSFILE.DS_RCOURSE_UNITS_1,
      *             DSFILE.DS_RCOURSE_UNITS_2,
      *             DSFILE.DS_RCOURSE_UNITS_3,
      *             DSFILE.DS_RCOURSE_AMT_1,
      *             DSFILE.DS_RCOURSE_AMT_2,
      *             DSFILE.DS_RCOURSE_AMT_3,
      *             DSFILE.DS_NPURUNIT_1,
      *             DSFILE.DS_NPURUNIT_2,
      *             DSFILE.DS_NPURUNIT_3,
      *             DSFILE.DS_NPURAMT_1,
      *             DSFILE.DS_NPURAMT_2,
      *             DSFILE.DS_NPURAMT_3,
      *             DSFILE.DS_OPURUNIT_1,
      *             DSFILE.DS_OPURUNIT_2,
      *             DSFILE.DS_OPURUNIT_3,
      *             DSFILE.DS_OPURAMT_1,
      *             DSFILE.DS_OPURAMT_2,
      *             DSFILE.DS_OPURAMT_3,
      *             DSFILE.DS_RNEWUNIT_1,
      *             DSFILE.DS_RNEWUNIT_2,
      *             DSFILE.DS_RNEWUNIT_3,
      *             DSFILE.DS_RNEWAMT_1,
      *             DSFILE.DS_RNEWAMT_2,
      *             DSFILE.DS_RNEWAMT_3,
      *             DSFILE.DS_PAIDUNIT_1,
      *             DSFILE.DS_PAIDUNIT_2,
      *             DSFILE.DS_PAIDUNIT_3,
      *             DSFILE.DS_PAIDAMT_1,
      *             DSFILE.DS_PAIDAMT_2,
      *             DSFILE.DS_PAIDAMT_3,
      *             DSFILE.DS_TODLUNIT_1,
      *             DSFILE.DS_TODLUNIT_2,
      *             DSFILE.DS_TODLUNIT_3,
      *             DSFILE.DS_TODLAMT_1,
      *             DSFILE.DS_TODLAMT_2,
      *             DSFILE.DS_TODLAMT_3,
      *             DSFILE.DS_OUTSUNIT_1,
      *             DSFILE.DS_OUTSUNIT_2,
      *             DSFILE.DS_OUTSUNIT_3,
      *             DSFILE.DS_OUTSAMT_1,
      *             DSFILE.DS_OUTSAMT_2,
      *             DSFILE.DS_OUTSAMT_3,
      *             DSFILE.DS_PLOSUNIT_1,
      *             DSFILE.DS_PLOSUNIT_2,
      *             DSFILE.DS_PLOSUNIT_3,
      *             DSFILE.DS_PLOSAMT_1,
      *             DSFILE.DS_PLOSAMT_2,
      *             DSFILE.DS_PLOSAMT_3,
      *             DSFILE.DS_RCPLUNIT_1,
      *             DSFILE.DS_RCPLUNIT_2,
      *             DSFILE.DS_RCPLUNIT_3,
      *             DSFILE.DS_RCPLAMT_1,
      *             DSFILE.DS_RCPLAMT_2,
      *             DSFILE.DS_RCPLAMT_3,
      *             DSFILE.DS_JDOSUNIT_1,
      *             DSFILE.DS_JDOSUNIT_2,
      *             DSFILE.DS_JDOSUNIT_3,
      *             DSFILE.DS_JDOSAMT_1,
      *             DSFILE.DS_JDOSAMT_2,
      *             DSFILE.DS_JDOSAMT_3,
      *             CAST(DSFILE.DS_DELUNEARN_DATE AS VARCHAR(10)),
      *             DSFILE.DS_PROCEEDS_1,
      *             DSFILE.DS_PROCEEDS_2,
      *             DSFILE.DS_NO_REPO_1_1_1,
      *             DSFILE.DS_AMT_REPO_1_1_1,
      *             DSFILE.DS_NO_REPO_1_1_2,
      *             DSFILE.DS_AMT_REPO_1_1_2,
      *             DSFILE.DS_NO_REPO_1_1_3,
      *             DSFILE.DS_AMT_REPO_1_1_3,
      *             DSFILE.DS_NO_REPO_1_1_4,
      *             DSFILE.DS_AMT_REPO_1_1_4,
      *             DSFILE.DS_NO_REPO_1_1_5,
      *             DSFILE.DS_AMT_REPO_1_1_5,
      *             DSFILE.DS_NO_REPO_1_2_1,
      *             DSFILE.DS_AMT_REPO_1_2_1,
      *             DSFILE.DS_NO_REPO_1_2_2,
      *             DSFILE.DS_AMT_REPO_1_2_2,
      *             DSFILE.DS_NO_REPO_1_2_3,
      *             DSFILE.DS_AMT_REPO_1_2_3,
      *             DSFILE.DS_NO_REPO_1_2_4,
      *             DSFILE.DS_AMT_REPO_1_2_4,
      *             DSFILE.DS_NO_REPO_1_2_5,
      *             DSFILE.DS_AMT_REPO_1_2_5,
      *             DSFILE.DS_NO_REPO_2_1_1,
      *             DSFILE.DS_AMT_REPO_2_1_1,
      *             DSFILE.DS_NO_REPO_2_1_2,
      *             DSFILE.DS_AMT_REPO_2_1_2,
      *             DSFILE.DS_NO_REPO_2_1_3,
      *             DSFILE.DS_AMT_REPO_2_1_3,
      *             DSFILE.DS_NO_REPO_2_1_4,
      *             DSFILE.DS_AMT_REPO_2_1_4,
      *             DSFILE.DS_NO_REPO_2_1_5,
      *             DSFILE.DS_AMT_REPO_2_1_5,
      *             DSFILE.DS_NO_REPO_2_2_1,
      *             DSFILE.DS_AMT_REPO_2_2_1,
      *             DSFILE.DS_NO_REPO_2_2_2,
      *             DSFILE.DS_AMT_REPO_2_2_2,
      *             DSFILE.DS_NO_REPO_2_2_3,
      *             DSFILE.DS_AMT_REPO_2_2_3,
      *             DSFILE.DS_NO_REPO_2_2_4,
      *             DSFILE.DS_AMT_REPO_2_2_4,
      *             DSFILE.DS_NO_REPO_2_2_5,
      *             DSFILE.DS_AMT_REPO_2_2_5,
      *             DSFILE.DS_NO_REPO_3_1_1,
      *             DSFILE.DS_AMT_REPO_3_1_1,
      *             DSFILE.DS_NO_REPO_3_1_2,
      *             DSFILE.DS_AMT_REPO_3_1_2,
      *             DSFILE.DS_NO_REPO_3_1_3,
      *             DSFILE.DS_AMT_REPO_3_1_3,
      *             DSFILE.DS_NO_REPO_3_1_4,
      *             DSFILE.DS_AMT_REPO_3_1_4,
      *             DSFILE.DS_NO_REPO_3_1_5,
      *             DSFILE.DS_AMT_REPO_3_1_5,
      *             DSFILE.DS_NO_REPO_3_2_1,
      *             DSFILE.DS_AMT_REPO_3_2_1,
      *             DSFILE.DS_NO_REPO_3_2_2,
      *             DSFILE.DS_AMT_REPO_3_2_2,
      *             DSFILE.DS_NO_REPO_3_2_3,
      *             DSFILE.DS_AMT_REPO_3_2_3,
      *             DSFILE.DS_NO_REPO_3_2_4,
      *             DSFILE.DS_AMT_REPO_3_2_4,
      *             DSFILE.DS_NO_REPO_3_2_5,
      *             DSFILE.DS_AMT_REPO_3_2_5,
      *             DSFILE.DS_UNITS_1_1_1,
      *             DSFILE.DS_OUTBAL_1_1_1,
      *             DSFILE.DS_UNITS_1_1_2,
      *             DSFILE.DS_OUTBAL_1_1_2,
      *             DSFILE.DS_UNITS_1_1_3,
      *             DSFILE.DS_OUTBAL_1_1_3,
      *             DSFILE.DS_UNITS_1_1_4,
      *             DSFILE.DS_OUTBAL_1_1_4,
      *             DSFILE.DS_UNITS_1_1_5,
      *             DSFILE.DS_OUTBAL_1_1_5,
      *             DSFILE.DS_UNITS_1_1_6,
      *             DSFILE.DS_OUTBAL_1_1_6,
      *             DSFILE.DS_UNITS_1_1_7,
      *             DSFILE.DS_OUTBAL_1_1_7,
      *             DSFILE.DS_UNITS_1_1_8,
      *             DSFILE.DS_OUTBAL_1_1_8,
      *             DSFILE.DS_UNITS_1_1_9,
      *             DSFILE.DS_OUTBAL_1_1_9,
      *             DSFILE.DS_UNITS_1_1_10,
      *             DSFILE.DS_OUTBAL_1_1_10,
      *             DSFILE.DS_UNITS_1_2_1,
      *             DSFILE.DS_OUTBAL_1_2_1,
      *             DSFILE.DS_UNITS_1_2_2,
      *             DSFILE.DS_OUTBAL_1_2_2,
      *             DSFILE.DS_UNITS_1_2_3,
      *             DSFILE.DS_OUTBAL_1_2_3,
      *             DSFILE.DS_UNITS_1_2_4,
      *             DSFILE.DS_OUTBAL_1_2_4,
      *             DSFILE.DS_UNITS_1_2_5,
      *             DSFILE.DS_OUTBAL_1_2_5,
      *             DSFILE.DS_UNITS_1_2_6,
      *             DSFILE.DS_OUTBAL_1_2_6,
      *             DSFILE.DS_UNITS_1_2_7,
      *             DSFILE.DS_OUTBAL_1_2_7,
      *             DSFILE.DS_UNITS_1_2_8,
      *             DSFILE.DS_OUTBAL_1_2_8,
      *             DSFILE.DS_UNITS_1_2_9,
      *             DSFILE.DS_OUTBAL_1_2_9,
      *             DSFILE.DS_UNITS_1_2_10,
      *             DSFILE.DS_OUTBAL_1_2_10,
      *             DSFILE.DS_UNITS_2_1_1,
      *             DSFILE.DS_OUTBAL_2_1_1,
      *             DSFILE.DS_UNITS_2_1_2,
      *             DSFILE.DS_OUTBAL_2_1_2,
      *             DSFILE.DS_UNITS_2_1_3,
      *             DSFILE.DS_OUTBAL_2_1_3,
      *             DSFILE.DS_UNITS_2_1_4,
      *             DSFILE.DS_OUTBAL_2_1_4,
      *             DSFILE.DS_UNITS_2_1_5,
      *             DSFILE.DS_OUTBAL_2_1_5,
      *             DSFILE.DS_UNITS_2_1_6,
      *             DSFILE.DS_OUTBAL_2_1_6,
      *             DSFILE.DS_UNITS_2_1_7,
      *             DSFILE.DS_OUTBAL_2_1_7,
      *             DSFILE.DS_UNITS_2_1_8,
      *             DSFILE.DS_OUTBAL_2_1_8,
      *             DSFILE.DS_UNITS_2_1_9,
      *             DSFILE.DS_OUTBAL_2_1_9,
      *             DSFILE.DS_UNITS_2_1_10,
      *             DSFILE.DS_OUTBAL_2_1_10,
      *             DSFILE.DS_UNITS_2_2_1,
      *             DSFILE.DS_OUTBAL_2_2_1,
      *             DSFILE.DS_UNITS_2_2_2,
      *             DSFILE.DS_OUTBAL_2_2_2,
      *             DSFILE.DS_UNITS_2_2_3,
      *             DSFILE.DS_OUTBAL_2_2_3,
      *             DSFILE.DS_UNITS_2_2_4,
      *             DSFILE.DS_OUTBAL_2_2_4,
      *             DSFILE.DS_UNITS_2_2_5,
      *             DSFILE.DS_OUTBAL_2_2_5,
      *             DSFILE.DS_UNITS_2_2_6,
      *             DSFILE.DS_OUTBAL_2_2_6,
      *             DSFILE.DS_UNITS_2_2_7,
      *             DSFILE.DS_OUTBAL_2_2_7,
      *             DSFILE.DS_UNITS_2_2_8,
      *             DSFILE.DS_OUTBAL_2_2_8,
      *             DSFILE.DS_UNITS_2_2_9,
      *             DSFILE.DS_OUTBAL_2_2_9,
      *             DSFILE.DS_UNITS_2_2_10,
      *             DSFILE.DS_OUTBAL_2_2_10,
      *             DSFILE.DS_UNITS_3_1_1,
      *             DSFILE.DS_OUTBAL_3_1_1,
      *             DSFILE.DS_UNITS_3_1_2,
      *             DSFILE.DS_OUTBAL_3_1_2,
      *             DSFILE.DS_UNITS_3_1_3,
      *             DSFILE.DS_OUTBAL_3_1_3,
      *             DSFILE.DS_UNITS_3_1_4,
      *             DSFILE.DS_OUTBAL_3_1_4,
      *             DSFILE.DS_UNITS_3_1_5,
      *             DSFILE.DS_OUTBAL_3_1_5,
      *             DSFILE.DS_UNITS_3_1_6,
      *             DSFILE.DS_OUTBAL_3_1_6,
      *             DSFILE.DS_UNITS_3_1_7,
      *             DSFILE.DS_OUTBAL_3_1_7,
      *             DSFILE.DS_UNITS_3_1_8,
      *             DSFILE.DS_OUTBAL_3_1_8,
      *             DSFILE.DS_UNITS_3_1_9,
      *             DSFILE.DS_OUTBAL_3_1_9,
      *             DSFILE.DS_UNITS_3_1_10,
      *             DSFILE.DS_OUTBAL_3_1_10,
      *             DSFILE.DS_UNITS_3_2_1,
      *             DSFILE.DS_OUTBAL_3_2_1,
      *             DSFILE.DS_UNITS_3_2_2,
      *             DSFILE.DS_OUTBAL_3_2_2,
      *             DSFILE.DS_UNITS_3_2_3,
      *             DSFILE.DS_OUTBAL_3_2_3,
      *             DSFILE.DS_UNITS_3_2_4,
      *             DSFILE.DS_OUTBAL_3_2_4,
      *             DSFILE.DS_UNITS_3_2_5,
      *             DSFILE.DS_OUTBAL_3_2_5,
      *             DSFILE.DS_UNITS_3_2_6,
      *             DSFILE.DS_OUTBAL_3_2_6,
      *             DSFILE.DS_UNITS_3_2_7,
      *             DSFILE.DS_OUTBAL_3_2_7,
      *             DSFILE.DS_UNITS_3_2_8,
      *             DSFILE.DS_OUTBAL_3_2_8,
      *             DSFILE.DS_UNITS_3_2_9,
      *             DSFILE.DS_OUTBAL_3_2_9,
      *             DSFILE.DS_UNITS_3_2_10,
      *             DSFILE.DS_OUTBAL_3_2_10,
      *             DSFILE.DS_UNITS_BNKRPT_1_1_1,
      *             DSFILE.DS_OUTBAL_BNKRPT_1_1_1,
      *             DSFILE.DS_UNITS_BNKRPT_1_1_2,
      *             DSFILE.DS_OUTBAL_BNKRPT_1_1_2,
      *             DSFILE.DS_UNITS_BNKRPT_1_1_3,
      *             DSFILE.DS_OUTBAL_BNKRPT_1_1_3,
      *             DSFILE.DS_UNITS_BNKRPT_1_1_4,
      *             DSFILE.DS_OUTBAL_BNKRPT_1_1_4,
      *             DSFILE.DS_UNITS_BNKRPT_1_1_5,
      *             DSFILE.DS_OUTBAL_BNKRPT_1_1_5,
      *             DSFILE.DS_UNITS_BNKRPT_1_1_6,
      *             DSFILE.DS_OUTBAL_BNKRPT_1_1_6,
      *             DSFILE.DS_UNITS_BNKRPT_1_1_7,
      *             DSFILE.DS_OUTBAL_BNKRPT_1_1_7,
      *             DSFILE.DS_UNITS_BNKRPT_1_1_8,
      *             DSFILE.DS_OUTBAL_BNKRPT_1_1_8,
      *             DSFILE.DS_UNITS_BNKRPT_1_1_9,
      *             DSFILE.DS_OUTBAL_BNKRPT_1_1_9,
      *             DSFILE.DS_UNITS_BNKRPT_1_1_10,
      *             DSFILE.DS_OUTBAL_BNKRPT_1_1_10,
      *             DSFILE.DS_UNITS_BNKRPT_1_2_1,
      *             DSFILE.DS_OUTBAL_BNKRPT_1_2_1,
      *             DSFILE.DS_UNITS_BNKRPT_1_2_2,
      *             DSFILE.DS_OUTBAL_BNKRPT_1_2_2,
      *             DSFILE.DS_UNITS_BNKRPT_1_2_3,
      *             DSFILE.DS_OUTBAL_BNKRPT_1_2_3,
      *             DSFILE.DS_UNITS_BNKRPT_1_2_4,
      *             DSFILE.DS_OUTBAL_BNKRPT_1_2_4,
      *             DSFILE.DS_UNITS_BNKRPT_1_2_5,
      *             DSFILE.DS_OUTBAL_BNKRPT_1_2_5,
      *             DSFILE.DS_UNITS_BNKRPT_1_2_6,
      *             DSFILE.DS_OUTBAL_BNKRPT_1_2_6,
      *             DSFILE.DS_UNITS_BNKRPT_1_2_7,
      *             DSFILE.DS_OUTBAL_BNKRPT_1_2_7,
      *             DSFILE.DS_UNITS_BNKRPT_1_2_8,
      *             DSFILE.DS_OUTBAL_BNKRPT_1_2_8,
      *             DSFILE.DS_UNITS_BNKRPT_1_2_9,
      *             DSFILE.DS_OUTBAL_BNKRPT_1_2_9,
      *             DSFILE.DS_UNITS_BNKRPT_1_2_10,
      *             DSFILE.DS_OUTBAL_BNKRPT_1_2_10,
      *             DSFILE.DS_UNITS_BNKRPT_2_1_1,
      *             DSFILE.DS_OUTBAL_BNKRPT_2_1_1,
      *             DSFILE.DS_UNITS_BNKRPT_2_1_2,
      *             DSFILE.DS_OUTBAL_BNKRPT_2_1_2,
      *             DSFILE.DS_UNITS_BNKRPT_2_1_3,
      *             DSFILE.DS_OUTBAL_BNKRPT_2_1_3,
      *             DSFILE.DS_UNITS_BNKRPT_2_1_4,
      *             DSFILE.DS_OUTBAL_BNKRPT_2_1_4,
      *             DSFILE.DS_UNITS_BNKRPT_2_1_5,
      *             DSFILE.DS_OUTBAL_BNKRPT_2_1_5,
      *             DSFILE.DS_UNITS_BNKRPT_2_1_6,
      *             DSFILE.DS_OUTBAL_BNKRPT_2_1_6,
      *             DSFILE.DS_UNITS_BNKRPT_2_1_7,
      *             DSFILE.DS_OUTBAL_BNKRPT_2_1_7,
      *             DSFILE.DS_UNITS_BNKRPT_2_1_8,
      *             DSFILE.DS_OUTBAL_BNKRPT_2_1_8,
      *             DSFILE.DS_UNITS_BNKRPT_2_1_9,
      *             DSFILE.DS_OUTBAL_BNKRPT_2_1_9,
      *             DSFILE.DS_UNITS_BNKRPT_2_1_10,
      *             DSFILE.DS_OUTBAL_BNKRPT_2_1_10,
      *             DSFILE.DS_UNITS_BNKRPT_2_2_1,
      *             DSFILE.DS_OUTBAL_BNKRPT_2_2_1,
      *             DSFILE.DS_UNITS_BNKRPT_2_2_2,
      *             DSFILE.DS_OUTBAL_BNKRPT_2_2_2,
      *             DSFILE.DS_UNITS_BNKRPT_2_2_3,
      *             DSFILE.DS_OUTBAL_BNKRPT_2_2_3,
      *             DSFILE.DS_UNITS_BNKRPT_2_2_4,
      *             DSFILE.DS_OUTBAL_BNKRPT_2_2_4,
      *             DSFILE.DS_UNITS_BNKRPT_2_2_5,
      *             DSFILE.DS_OUTBAL_BNKRPT_2_2_5,
      *             DSFILE.DS_UNITS_BNKRPT_2_2_6,
      *             DSFILE.DS_OUTBAL_BNKRPT_2_2_6,
      *             DSFILE.DS_UNITS_BNKRPT_2_2_7,
      *             DSFILE.DS_OUTBAL_BNKRPT_2_2_7,
      *             DSFILE.DS_UNITS_BNKRPT_2_2_8,
      *             DSFILE.DS_OUTBAL_BNKRPT_2_2_8,
      *             DSFILE.DS_UNITS_BNKRPT_2_2_9,
      *             DSFILE.DS_OUTBAL_BNKRPT_2_2_9,
      *             DSFILE.DS_UNITS_BNKRPT_2_2_10,
      *             DSFILE.DS_OUTBAL_BNKRPT_2_2_10,
      *             DSFILE.DS_UNITS_BNKRPT_3_1_1,
      *             DSFILE.DS_OUTBAL_BNKRPT_3_1_1,
      *             DSFILE.DS_UNITS_BNKRPT_3_1_2,
      *             DSFILE.DS_OUTBAL_BNKRPT_3_1_2,
      *             DSFILE.DS_UNITS_BNKRPT_3_1_3,
      *             DSFILE.DS_OUTBAL_BNKRPT_3_1_3,
      *             DSFILE.DS_UNITS_BNKRPT_3_1_4,
      *             DSFILE.DS_OUTBAL_BNKRPT_3_1_4,
      *             DSFILE.DS_UNITS_BNKRPT_3_1_5,
      *             DSFILE.DS_OUTBAL_BNKRPT_3_1_5,
      *             DSFILE.DS_UNITS_BNKRPT_3_1_6,
      *             DSFILE.DS_OUTBAL_BNKRPT_3_1_6,
      *             DSFILE.DS_UNITS_BNKRPT_3_1_7,
      *             DSFILE.DS_OUTBAL_BNKRPT_3_1_7,
      *             DSFILE.DS_UNITS_BNKRPT_3_1_8,
      *             DSFILE.DS_OUTBAL_BNKRPT_3_1_8,
      *             DSFILE.DS_UNITS_BNKRPT_3_1_9,
      *             DSFILE.DS_OUTBAL_BNKRPT_3_1_9,
      *             DSFILE.DS_UNITS_BNKRPT_3_1_10,
      *             DSFILE.DS_OUTBAL_BNKRPT_3_1_10,
      *             DSFILE.DS_UNITS_BNKRPT_3_2_1,
      *             DSFILE.DS_OUTBAL_BNKRPT_3_2_1,
      *             DSFILE.DS_UNITS_BNKRPT_3_2_2,
      *             DSFILE.DS_OUTBAL_BNKRPT_3_2_2,
      *             DSFILE.DS_UNITS_BNKRPT_3_2_3,
      *             DSFILE.DS_OUTBAL_BNKRPT_3_2_3,
      *             DSFILE.DS_UNITS_BNKRPT_3_2_4,
      *             DSFILE.DS_OUTBAL_BNKRPT_3_2_4,
      *             DSFILE.DS_UNITS_BNKRPT_3_2_5,
      *             DSFILE.DS_OUTBAL_BNKRPT_3_2_5,
      *             DSFILE.DS_UNITS_BNKRPT_3_2_6,
      *             DSFILE.DS_OUTBAL_BNKRPT_3_2_6,
      *             DSFILE.DS_UNITS_BNKRPT_3_2_7,
      *             DSFILE.DS_OUTBAL_BNKRPT_3_2_7,
      *             DSFILE.DS_UNITS_BNKRPT_3_2_8,
      *             DSFILE.DS_OUTBAL_BNKRPT_3_2_8,
      *             DSFILE.DS_UNITS_BNKRPT_3_2_9,
      *             DSFILE.DS_OUTBAL_BNKRPT_3_2_9,
      *             DSFILE.DS_UNITS_BNKRPT_3_2_10,
      *             DSFILE.DS_OUTBAL_BNKRPT_3_2_10,
      *             DSFILE.DS_UNEARNED_AMT_1_1_1,
      *             DSFILE.DS_UNEARNED_AMT_1_1_2,
      *             DSFILE.DS_UNEARNED_AMT_1_1_3,
      *             DSFILE.DS_UNEARNED_AMT_1_1_4,
      *             DSFILE.DS_UNEARNED_AMT_1_1_5,
      *             DSFILE.DS_UNEARNED_AMT_1_1_6,
      *             DSFILE.DS_UNEARNED_AMT_1_1_7,
      *             DSFILE.DS_UNEARNED_AMT_1_1_8,
      *             DSFILE.DS_UNEARNED_AMT_1_1_9,
      *             DSFILE.DS_UNEARNED_AMT_1_1_10,
      *             DSFILE.DS_UNEARNED_AMT_1_2_1,
      *             DSFILE.DS_UNEARNED_AMT_1_2_2,
      *             DSFILE.DS_UNEARNED_AMT_1_2_3,
      *             DSFILE.DS_UNEARNED_AMT_1_2_4,
      *             DSFILE.DS_UNEARNED_AMT_1_2_5,
      *             DSFILE.DS_UNEARNED_AMT_1_2_6,
      *             DSFILE.DS_UNEARNED_AMT_1_2_7,
      *             DSFILE.DS_UNEARNED_AMT_1_2_8,
      *             DSFILE.DS_UNEARNED_AMT_1_2_9,
      *             DSFILE.DS_UNEARNED_AMT_1_2_10,
      *             DSFILE.DS_UNEARNED_AMT_2_1_1,
      *             DSFILE.DS_UNEARNED_AMT_2_1_2,
      *             DSFILE.DS_UNEARNED_AMT_2_1_3,
      *             DSFILE.DS_UNEARNED_AMT_2_1_4,
      *             DSFILE.DS_UNEARNED_AMT_2_1_5,
      *             DSFILE.DS_UNEARNED_AMT_2_1_6,
      *             DSFILE.DS_UNEARNED_AMT_2_1_7,
      *             DSFILE.DS_UNEARNED_AMT_2_1_8,
      *             DSFILE.DS_UNEARNED_AMT_2_1_9,
      *             DSFILE.DS_UNEARNED_AMT_2_1_10,
      *             DSFILE.DS_UNEARNED_AMT_2_2_1,
      *             DSFILE.DS_UNEARNED_AMT_2_2_2,
      *             DSFILE.DS_UNEARNED_AMT_2_2_3,
      *             DSFILE.DS_UNEARNED_AMT_2_2_4,
      *             DSFILE.DS_UNEARNED_AMT_2_2_5,
      *             DSFILE.DS_UNEARNED_AMT_2_2_6,
      *             DSFILE.DS_UNEARNED_AMT_2_2_7,
      *             DSFILE.DS_UNEARNED_AMT_2_2_8,
      *             DSFILE.DS_UNEARNED_AMT_2_2_9,
      *             DSFILE.DS_UNEARNED_AMT_2_2_10,
      *             DSFILE.DS_UNEARNED_AMT_3_1_1,
      *             DSFILE.DS_UNEARNED_AMT_3_1_2,
      *             DSFILE.DS_UNEARNED_AMT_3_1_3,
      *             DSFILE.DS_UNEARNED_AMT_3_1_4,
      *             DSFILE.DS_UNEARNED_AMT_3_1_5,
      *             DSFILE.DS_UNEARNED_AMT_3_1_6,
      *             DSFILE.DS_UNEARNED_AMT_3_1_7,
      *             DSFILE.DS_UNEARNED_AMT_3_1_8,
      *             DSFILE.DS_UNEARNED_AMT_3_1_9,
      *             DSFILE.DS_UNEARNED_AMT_3_1_10,
      *             DSFILE.DS_UNEARNED_AMT_3_2_1,
      *             DSFILE.DS_UNEARNED_AMT_3_2_2,
      *             DSFILE.DS_UNEARNED_AMT_3_2_3,
      *             DSFILE.DS_UNEARNED_AMT_3_2_4,
      *             DSFILE.DS_UNEARNED_AMT_3_2_5,
      *             DSFILE.DS_UNEARNED_AMT_3_2_6,
      *             DSFILE.DS_UNEARNED_AMT_3_2_7,
      *             DSFILE.DS_UNEARNED_AMT_3_2_8,
      *             DSFILE.DS_UNEARNED_AMT_3_2_9,
      *             DSFILE.DS_UNEARNED_AMT_3_2_10,
      *             DSFILE.DS_DLDISC_VOL_1,
      *             DSFILE.DS_DLDISC_VOL_2,
      *             DSFILE.DS_DLDISC_VOL_3,
      *             DSFILE.DS_AFPDUEDL_MEMO,
      *             DSFILE.DS_UNPAID_MANUAL,
      *             CAST(DSFILE.DS_LTOUCH_DATE AS VARCHAR(10))
      *      INTO :QDS-REC
      *      FROM DBO.DSFILE
      *      WHERE DSFILE.DS_DATE_CCYYMM = :QDS-DATE-CCYYMM
      *        AND DSFILE.DS_BRANCH = :QDS-BRANCH
      *        AND DSFILE.DS_NUM = :QDS-NUM
      *        AND DSFILE.DS_CLASS = :QDS-CLASS
      *     END-EXEC.

      *     PERFORM SQL-IO-VALIDATION.

      *     IF ( IO-FG = 8 )     *>LOCKED
      *        GO TO READ-DS1-FILE.

      *     IF ( IO-FG = 0 )
      *        PERFORM GET-DS-FIELDS.

      **----------------------------------------------------------------
      * READ-DS1-FILE-NEXT.
      *     PERFORM READ-IT.
      *     MOVE DS-PATH-OWNBR  TO DS-BRANCH.
      *     MOVE DS-PATH-SQL    TO E-FILE.
      *     MOVE DS1-KEY        TO E-KEYX.
      *     INITIALIZE QDS-REC.

      *     IF ( DS1-CURSOR-STAT-GOOD )
      *        EXEC SQL
      *             FETCH DS1_CURSOR INTO :QDS-REC
      *        END-EXEC
      *     ELSE
      *        MOVE 9        TO SQLCODE
      *        MOVE "RDNXCR" TO SQLSTATE.

      *     PERFORM SQL-IO-VALIDATION.

      *     IF ( IO-FG = 8 )     *>LOCKED
      *        GO TO READ-DS1-FILE-NEXT.

      *     IF ( IO-FG = 0 )
      *        PERFORM GET-DS-FIELDS.

      **-----------------------------------------------------------------
      * WRITE-DS1-FILE.

      *     MOVE DS-PATH-OWNBR  TO DS-BRANCH.
      *     ACCEPT DS-LTOUCH-DATE FROM CENTURY-DATE.
      *     PERFORM WRITE-IT.
      *     MOVE DS-PATH-SQL    TO E-FILE.
      *     MOVE DS1-KEY        TO E-KEYX.
      *     PERFORM SET-DS-FIELDS.

      *     MOVE '2' TO SQL-UPDATE-CONN-USE-SW.    *> CHANGE CONNECTION TO UPDATE_CONNECT
      *     PERFORM SQL-SWAP-UPDATE-CONNECTION.

      *     EXEC SQL
      *      INSERT INTO DBO.DSFILE
      *       (DSFILE.DS_DATE_CCYYMM,
      *        DSFILE.DS_BRANCH,
      *        DSFILE.DS_NUM,
      *        DSFILE.DS_CLASS,
      *        DSFILE.DS_DLRVPRIN_1,
      *        DSFILE.DS_DLRVPRIN_2,
      *        DSFILE.DS_DLRVPRIN_3,
      *        DSFILE.DS_DLRVCHGS_1,
      *        DSFILE.DS_DLRVCHGS_2,
      *        DSFILE.DS_DLRVCHGS_3,
      *        DSFILE.DS_DLPARVHELD,
      *        DSFILE.DS_UNPAIDCK,
      *        DSFILE.DS_DLDISC,
      *        DSFILE.DS_DLPARVPAID,
      *        DSFILE.DS_SUMWGTTM,
      *        DSFILE.DS_SUMWGTAV,
      *        DSFILE.DS_SUMWGTTA,
      *        DSFILE.DS_RCOURSE_UNITS_1,
      *        DSFILE.DS_RCOURSE_UNITS_2,
      *        DSFILE.DS_RCOURSE_UNITS_3,
      *        DSFILE.DS_RCOURSE_AMT_1,
      *        DSFILE.DS_RCOURSE_AMT_2,
      *        DSFILE.DS_RCOURSE_AMT_3,
      *        DSFILE.DS_NPURUNIT_1,
      *        DSFILE.DS_NPURUNIT_2,
      *        DSFILE.DS_NPURUNIT_3,
      *        DSFILE.DS_NPURAMT_1,
      *        DSFILE.DS_NPURAMT_2,
      *        DSFILE.DS_NPURAMT_3,
      *        DSFILE.DS_OPURUNIT_1,
      *        DSFILE.DS_OPURUNIT_2,
      *        DSFILE.DS_OPURUNIT_3,
      *        DSFILE.DS_OPURAMT_1,
      *        DSFILE.DS_OPURAMT_2,
      *        DSFILE.DS_OPURAMT_3,
      *        DSFILE.DS_RNEWUNIT_1,
      *        DSFILE.DS_RNEWUNIT_2,
      *        DSFILE.DS_RNEWUNIT_3,
      *        DSFILE.DS_RNEWAMT_1,
      *        DSFILE.DS_RNEWAMT_2,
      *        DSFILE.DS_RNEWAMT_3,
      *        DSFILE.DS_PAIDUNIT_1,
      *        DSFILE.DS_PAIDUNIT_2,
      *        DSFILE.DS_PAIDUNIT_3,
      *        DSFILE.DS_PAIDAMT_1,
      *        DSFILE.DS_PAIDAMT_2,
      *        DSFILE.DS_PAIDAMT_3,
      *        DSFILE.DS_TODLUNIT_1,
      *        DSFILE.DS_TODLUNIT_2,
      *        DSFILE.DS_TODLUNIT_3,
      *        DSFILE.DS_TODLAMT_1,
      *        DSFILE.DS_TODLAMT_2,
      *        DSFILE.DS_TODLAMT_3,
      *        DSFILE.DS_OUTSUNIT_1,
      *        DSFILE.DS_OUTSUNIT_2,
      *        DSFILE.DS_OUTSUNIT_3,
      *        DSFILE.DS_OUTSAMT_1,
      *        DSFILE.DS_OUTSAMT_2,
      *        DSFILE.DS_OUTSAMT_3,
      *        DSFILE.DS_PLOSUNIT_1,
      *        DSFILE.DS_PLOSUNIT_2,
      *        DSFILE.DS_PLOSUNIT_3,
      *        DSFILE.DS_PLOSAMT_1,
      *        DSFILE.DS_PLOSAMT_2,
      *        DSFILE.DS_PLOSAMT_3,
      *        DSFILE.DS_RCPLUNIT_1,
      *        DSFILE.DS_RCPLUNIT_2,
      *        DSFILE.DS_RCPLUNIT_3,
      *        DSFILE.DS_RCPLAMT_1,
      *        DSFILE.DS_RCPLAMT_2,
      *        DSFILE.DS_RCPLAMT_3,
      *        DSFILE.DS_JDOSUNIT_1,
      *        DSFILE.DS_JDOSUNIT_2,
      *        DSFILE.DS_JDOSUNIT_3,
      *        DSFILE.DS_JDOSAMT_1,
      *        DSFILE.DS_JDOSAMT_2,
      *        DSFILE.DS_JDOSAMT_3,
      *        DSFILE.DS_DELUNEARN_DATE,
      *        DSFILE.DS_PROCEEDS_1,
      *        DSFILE.DS_PROCEEDS_2,
      *        DSFILE.DS_NO_REPO_1_1_1,
      *        DSFILE.DS_AMT_REPO_1_1_1,
      *        DSFILE.DS_NO_REPO_1_1_2,
      *        DSFILE.DS_AMT_REPO_1_1_2,
      *        DSFILE.DS_NO_REPO_1_1_3,
      *        DSFILE.DS_AMT_REPO_1_1_3,
      *        DSFILE.DS_NO_REPO_1_1_4,
      *        DSFILE.DS_AMT_REPO_1_1_4,
      *        DSFILE.DS_NO_REPO_1_1_5,
      *        DSFILE.DS_AMT_REPO_1_1_5,
      *        DSFILE.DS_NO_REPO_1_2_1,
      *        DSFILE.DS_AMT_REPO_1_2_1,
      *        DSFILE.DS_NO_REPO_1_2_2,
      *        DSFILE.DS_AMT_REPO_1_2_2,
      *        DSFILE.DS_NO_REPO_1_2_3,
      *        DSFILE.DS_AMT_REPO_1_2_3,
      *        DSFILE.DS_NO_REPO_1_2_4,
      *        DSFILE.DS_AMT_REPO_1_2_4,
      *        DSFILE.DS_NO_REPO_1_2_5,
      *        DSFILE.DS_AMT_REPO_1_2_5,
      *        DSFILE.DS_NO_REPO_2_1_1,
      *        DSFILE.DS_AMT_REPO_2_1_1,
      *        DSFILE.DS_NO_REPO_2_1_2,
      *        DSFILE.DS_AMT_REPO_2_1_2,
      *        DSFILE.DS_NO_REPO_2_1_3,
      *        DSFILE.DS_AMT_REPO_2_1_3,
      *        DSFILE.DS_NO_REPO_2_1_4,
      *        DSFILE.DS_AMT_REPO_2_1_4,
      *        DSFILE.DS_NO_REPO_2_1_5,
      *        DSFILE.DS_AMT_REPO_2_1_5,
      *        DSFILE.DS_NO_REPO_2_2_1,
      *        DSFILE.DS_AMT_REPO_2_2_1,
      *        DSFILE.DS_NO_REPO_2_2_2,
      *        DSFILE.DS_AMT_REPO_2_2_2,
      *        DSFILE.DS_NO_REPO_2_2_3,
      *        DSFILE.DS_AMT_REPO_2_2_3,
      *        DSFILE.DS_NO_REPO_2_2_4,
      *        DSFILE.DS_AMT_REPO_2_2_4,
      *        DSFILE.DS_NO_REPO_2_2_5,
      *        DSFILE.DS_AMT_REPO_2_2_5,
      *        DSFILE.DS_NO_REPO_3_1_1,
      *        DSFILE.DS_AMT_REPO_3_1_1,
      *        DSFILE.DS_NO_REPO_3_1_2,
      *        DSFILE.DS_AMT_REPO_3_1_2,
      *        DSFILE.DS_NO_REPO_3_1_3,
      *        DSFILE.DS_AMT_REPO_3_1_3,
      *        DSFILE.DS_NO_REPO_3_1_4,
      *        DSFILE.DS_AMT_REPO_3_1_4,
      *        DSFILE.DS_NO_REPO_3_1_5,
      *        DSFILE.DS_AMT_REPO_3_1_5,
      *        DSFILE.DS_NO_REPO_3_2_1,
      *        DSFILE.DS_AMT_REPO_3_2_1,
      *        DSFILE.DS_NO_REPO_3_2_2,
      *        DSFILE.DS_AMT_REPO_3_2_2,
      *        DSFILE.DS_NO_REPO_3_2_3,
      *        DSFILE.DS_AMT_REPO_3_2_3,
      *        DSFILE.DS_NO_REPO_3_2_4,
      *        DSFILE.DS_AMT_REPO_3_2_4,
      *        DSFILE.DS_NO_REPO_3_2_5,
      *        DSFILE.DS_AMT_REPO_3_2_5,
      *        DSFILE.DS_UNITS_1_1_1,
      *        DSFILE.DS_OUTBAL_1_1_1,
      *        DSFILE.DS_UNITS_1_1_2,
      *        DSFILE.DS_OUTBAL_1_1_2,
      *        DSFILE.DS_UNITS_1_1_3,
      *        DSFILE.DS_OUTBAL_1_1_3,
      *        DSFILE.DS_UNITS_1_1_4,
      *        DSFILE.DS_OUTBAL_1_1_4,
      *        DSFILE.DS_UNITS_1_1_5,
      *        DSFILE.DS_OUTBAL_1_1_5,
      *        DSFILE.DS_UNITS_1_1_6,
      *        DSFILE.DS_OUTBAL_1_1_6,
      *        DSFILE.DS_UNITS_1_1_7,
      *        DSFILE.DS_OUTBAL_1_1_7,
      *        DSFILE.DS_UNITS_1_1_8,
      *        DSFILE.DS_OUTBAL_1_1_8,
      *        DSFILE.DS_UNITS_1_1_9,
      *        DSFILE.DS_OUTBAL_1_1_9,
      *        DSFILE.DS_UNITS_1_1_10,
      *        DSFILE.DS_OUTBAL_1_1_10,
      *        DSFILE.DS_UNITS_1_2_1,
      *        DSFILE.DS_OUTBAL_1_2_1,
      *        DSFILE.DS_UNITS_1_2_2,
      *        DSFILE.DS_OUTBAL_1_2_2,
      *        DSFILE.DS_UNITS_1_2_3,
      *        DSFILE.DS_OUTBAL_1_2_3,
      *        DSFILE.DS_UNITS_1_2_4,
      *        DSFILE.DS_OUTBAL_1_2_4,
      *        DSFILE.DS_UNITS_1_2_5,
      *        DSFILE.DS_OUTBAL_1_2_5,
      *        DSFILE.DS_UNITS_1_2_6,
      *        DSFILE.DS_OUTBAL_1_2_6,
      *        DSFILE.DS_UNITS_1_2_7,
      *        DSFILE.DS_OUTBAL_1_2_7,
      *        DSFILE.DS_UNITS_1_2_8,
      *        DSFILE.DS_OUTBAL_1_2_8,
      *        DSFILE.DS_UNITS_1_2_9,
      *        DSFILE.DS_OUTBAL_1_2_9,
      *        DSFILE.DS_UNITS_1_2_10,
      *        DSFILE.DS_OUTBAL_1_2_10,
      *        DSFILE.DS_UNITS_2_1_1,
      *        DSFILE.DS_OUTBAL_2_1_1,
      *        DSFILE.DS_UNITS_2_1_2,
      *        DSFILE.DS_OUTBAL_2_1_2,
      *        DSFILE.DS_UNITS_2_1_3,
      *        DSFILE.DS_OUTBAL_2_1_3,
      *        DSFILE.DS_UNITS_2_1_4,
      *        DSFILE.DS_OUTBAL_2_1_4,
      *        DSFILE.DS_UNITS_2_1_5,
      *        DSFILE.DS_OUTBAL_2_1_5,
      *        DSFILE.DS_UNITS_2_1_6,
      *        DSFILE.DS_OUTBAL_2_1_6,
      *        DSFILE.DS_UNITS_2_1_7,
      *        DSFILE.DS_OUTBAL_2_1_7,
      *        DSFILE.DS_UNITS_2_1_8,
      *        DSFILE.DS_OUTBAL_2_1_8,
      *        DSFILE.DS_UNITS_2_1_9,
      *        DSFILE.DS_OUTBAL_2_1_9,
      *        DSFILE.DS_UNITS_2_1_10,
      *        DSFILE.DS_OUTBAL_2_1_10,
      *        DSFILE.DS_UNITS_2_2_1,
      *        DSFILE.DS_OUTBAL_2_2_1,
      *        DSFILE.DS_UNITS_2_2_2,
      *        DSFILE.DS_OUTBAL_2_2_2,
      *        DSFILE.DS_UNITS_2_2_3,
      *        DSFILE.DS_OUTBAL_2_2_3,
      *        DSFILE.DS_UNITS_2_2_4,
      *        DSFILE.DS_OUTBAL_2_2_4,
      *        DSFILE.DS_UNITS_2_2_5,
      *        DSFILE.DS_OUTBAL_2_2_5,
      *        DSFILE.DS_UNITS_2_2_6,
      *        DSFILE.DS_OUTBAL_2_2_6,
      *        DSFILE.DS_UNITS_2_2_7,
      *        DSFILE.DS_OUTBAL_2_2_7,
      *        DSFILE.DS_UNITS_2_2_8,
      *        DSFILE.DS_OUTBAL_2_2_8,
      *        DSFILE.DS_UNITS_2_2_9,
      *        DSFILE.DS_OUTBAL_2_2_9,
      *        DSFILE.DS_UNITS_2_2_10,
      *        DSFILE.DS_OUTBAL_2_2_10,
      *        DSFILE.DS_UNITS_3_1_1,
      *        DSFILE.DS_OUTBAL_3_1_1,
      *        DSFILE.DS_UNITS_3_1_2,
      *        DSFILE.DS_OUTBAL_3_1_2,
      *        DSFILE.DS_UNITS_3_1_3,
      *        DSFILE.DS_OUTBAL_3_1_3,
      *        DSFILE.DS_UNITS_3_1_4,
      *        DSFILE.DS_OUTBAL_3_1_4,
      *        DSFILE.DS_UNITS_3_1_5,
      *        DSFILE.DS_OUTBAL_3_1_5,
      *        DSFILE.DS_UNITS_3_1_6,
      *        DSFILE.DS_OUTBAL_3_1_6,
      *        DSFILE.DS_UNITS_3_1_7,
      *        DSFILE.DS_OUTBAL_3_1_7,
      *        DSFILE.DS_UNITS_3_1_8,
      *        DSFILE.DS_OUTBAL_3_1_8,
      *        DSFILE.DS_UNITS_3_1_9,
      *        DSFILE.DS_OUTBAL_3_1_9,
      *        DSFILE.DS_UNITS_3_1_10,
      *        DSFILE.DS_OUTBAL_3_1_10,
      *        DSFILE.DS_UNITS_3_2_1,
      *        DSFILE.DS_OUTBAL_3_2_1,
      *        DSFILE.DS_UNITS_3_2_2,
      *        DSFILE.DS_OUTBAL_3_2_2,
      *        DSFILE.DS_UNITS_3_2_3,
      *        DSFILE.DS_OUTBAL_3_2_3,
      *        DSFILE.DS_UNITS_3_2_4,
      *        DSFILE.DS_OUTBAL_3_2_4,
      *        DSFILE.DS_UNITS_3_2_5,
      *        DSFILE.DS_OUTBAL_3_2_5,
      *        DSFILE.DS_UNITS_3_2_6,
      *        DSFILE.DS_OUTBAL_3_2_6,
      *        DSFILE.DS_UNITS_3_2_7,
      *        DSFILE.DS_OUTBAL_3_2_7,
      *        DSFILE.DS_UNITS_3_2_8,
      *        DSFILE.DS_OUTBAL_3_2_8,
      *        DSFILE.DS_UNITS_3_2_9,
      *        DSFILE.DS_OUTBAL_3_2_9,
      *        DSFILE.DS_UNITS_3_2_10,
      *        DSFILE.DS_OUTBAL_3_2_10,
      *        DSFILE.DS_UNITS_BNKRPT_1_1_1,
      *        DSFILE.DS_OUTBAL_BNKRPT_1_1_1,
      *        DSFILE.DS_UNITS_BNKRPT_1_1_2,
      *        DSFILE.DS_OUTBAL_BNKRPT_1_1_2,
      *        DSFILE.DS_UNITS_BNKRPT_1_1_3,
      *        DSFILE.DS_OUTBAL_BNKRPT_1_1_3,
      *        DSFILE.DS_UNITS_BNKRPT_1_1_4,
      *        DSFILE.DS_OUTBAL_BNKRPT_1_1_4,
      *        DSFILE.DS_UNITS_BNKRPT_1_1_5,
      *        DSFILE.DS_OUTBAL_BNKRPT_1_1_5,
      *        DSFILE.DS_UNITS_BNKRPT_1_1_6,
      *        DSFILE.DS_OUTBAL_BNKRPT_1_1_6,
      *        DSFILE.DS_UNITS_BNKRPT_1_1_7,
      *        DSFILE.DS_OUTBAL_BNKRPT_1_1_7,
      *        DSFILE.DS_UNITS_BNKRPT_1_1_8,
      *        DSFILE.DS_OUTBAL_BNKRPT_1_1_8,
      *        DSFILE.DS_UNITS_BNKRPT_1_1_9,
      *        DSFILE.DS_OUTBAL_BNKRPT_1_1_9,
      *        DSFILE.DS_UNITS_BNKRPT_1_1_10,
      *        DSFILE.DS_OUTBAL_BNKRPT_1_1_10,
      *        DSFILE.DS_UNITS_BNKRPT_1_2_1,
      *        DSFILE.DS_OUTBAL_BNKRPT_1_2_1,
      *        DSFILE.DS_UNITS_BNKRPT_1_2_2,
      *        DSFILE.DS_OUTBAL_BNKRPT_1_2_2,
      *        DSFILE.DS_UNITS_BNKRPT_1_2_3,
      *        DSFILE.DS_OUTBAL_BNKRPT_1_2_3,
      *        DSFILE.DS_UNITS_BNKRPT_1_2_4,
      *        DSFILE.DS_OUTBAL_BNKRPT_1_2_4,
      *        DSFILE.DS_UNITS_BNKRPT_1_2_5,
      *        DSFILE.DS_OUTBAL_BNKRPT_1_2_5,
      *        DSFILE.DS_UNITS_BNKRPT_1_2_6,
      *        DSFILE.DS_OUTBAL_BNKRPT_1_2_6,
      *        DSFILE.DS_UNITS_BNKRPT_1_2_7,
      *        DSFILE.DS_OUTBAL_BNKRPT_1_2_7,
      *        DSFILE.DS_UNITS_BNKRPT_1_2_8,
      *        DSFILE.DS_OUTBAL_BNKRPT_1_2_8,
      *        DSFILE.DS_UNITS_BNKRPT_1_2_9,
      *        DSFILE.DS_OUTBAL_BNKRPT_1_2_9,
      *        DSFILE.DS_UNITS_BNKRPT_1_2_10,
      *        DSFILE.DS_OUTBAL_BNKRPT_1_2_10,
      *        DSFILE.DS_UNITS_BNKRPT_2_1_1,
      *        DSFILE.DS_OUTBAL_BNKRPT_2_1_1,
      *        DSFILE.DS_UNITS_BNKRPT_2_1_2,
      *        DSFILE.DS_OUTBAL_BNKRPT_2_1_2,
      *        DSFILE.DS_UNITS_BNKRPT_2_1_3,
      *        DSFILE.DS_OUTBAL_BNKRPT_2_1_3,
      *        DSFILE.DS_UNITS_BNKRPT_2_1_4,
      *        DSFILE.DS_OUTBAL_BNKRPT_2_1_4,
      *        DSFILE.DS_UNITS_BNKRPT_2_1_5,
      *        DSFILE.DS_OUTBAL_BNKRPT_2_1_5,
      *        DSFILE.DS_UNITS_BNKRPT_2_1_6,
      *        DSFILE.DS_OUTBAL_BNKRPT_2_1_6,
      *        DSFILE.DS_UNITS_BNKRPT_2_1_7,
      *        DSFILE.DS_OUTBAL_BNKRPT_2_1_7,
      *        DSFILE.DS_UNITS_BNKRPT_2_1_8,
      *        DSFILE.DS_OUTBAL_BNKRPT_2_1_8,
      *        DSFILE.DS_UNITS_BNKRPT_2_1_9,
      *        DSFILE.DS_OUTBAL_BNKRPT_2_1_9,
      *        DSFILE.DS_UNITS_BNKRPT_2_1_10,
      *        DSFILE.DS_OUTBAL_BNKRPT_2_1_10,
      *        DSFILE.DS_UNITS_BNKRPT_2_2_1,
      *        DSFILE.DS_OUTBAL_BNKRPT_2_2_1,
      *        DSFILE.DS_UNITS_BNKRPT_2_2_2,
      *        DSFILE.DS_OUTBAL_BNKRPT_2_2_2,
      *        DSFILE.DS_UNITS_BNKRPT_2_2_3,
      *        DSFILE.DS_OUTBAL_BNKRPT_2_2_3,
      *        DSFILE.DS_UNITS_BNKRPT_2_2_4,
      *        DSFILE.DS_OUTBAL_BNKRPT_2_2_4,
      *        DSFILE.DS_UNITS_BNKRPT_2_2_5,
      *        DSFILE.DS_OUTBAL_BNKRPT_2_2_5,
      *        DSFILE.DS_UNITS_BNKRPT_2_2_6,
      *        DSFILE.DS_OUTBAL_BNKRPT_2_2_6,
      *        DSFILE.DS_UNITS_BNKRPT_2_2_7,
      *        DSFILE.DS_OUTBAL_BNKRPT_2_2_7,
      *        DSFILE.DS_UNITS_BNKRPT_2_2_8,
      *        DSFILE.DS_OUTBAL_BNKRPT_2_2_8,
      *        DSFILE.DS_UNITS_BNKRPT_2_2_9,
      *        DSFILE.DS_OUTBAL_BNKRPT_2_2_9,
      *        DSFILE.DS_UNITS_BNKRPT_2_2_10,
      *        DSFILE.DS_OUTBAL_BNKRPT_2_2_10,
      *        DSFILE.DS_UNITS_BNKRPT_3_1_1,
      *        DSFILE.DS_OUTBAL_BNKRPT_3_1_1,
      *        DSFILE.DS_UNITS_BNKRPT_3_1_2,
      *        DSFILE.DS_OUTBAL_BNKRPT_3_1_2,
      *        DSFILE.DS_UNITS_BNKRPT_3_1_3,
      *        DSFILE.DS_OUTBAL_BNKRPT_3_1_3,
      *        DSFILE.DS_UNITS_BNKRPT_3_1_4,
      *        DSFILE.DS_OUTBAL_BNKRPT_3_1_4,
      *        DSFILE.DS_UNITS_BNKRPT_3_1_5,
      *        DSFILE.DS_OUTBAL_BNKRPT_3_1_5,
      *        DSFILE.DS_UNITS_BNKRPT_3_1_6,
      *        DSFILE.DS_OUTBAL_BNKRPT_3_1_6,
      *        DSFILE.DS_UNITS_BNKRPT_3_1_7,
      *        DSFILE.DS_OUTBAL_BNKRPT_3_1_7,
      *        DSFILE.DS_UNITS_BNKRPT_3_1_8,
      *        DSFILE.DS_OUTBAL_BNKRPT_3_1_8,
      *        DSFILE.DS_UNITS_BNKRPT_3_1_9,
      *        DSFILE.DS_OUTBAL_BNKRPT_3_1_9,
      *        DSFILE.DS_UNITS_BNKRPT_3_1_10,
      *        DSFILE.DS_OUTBAL_BNKRPT_3_1_10,
      *        DSFILE.DS_UNITS_BNKRPT_3_2_1,
      *        DSFILE.DS_OUTBAL_BNKRPT_3_2_1,
      *        DSFILE.DS_UNITS_BNKRPT_3_2_2,
      *        DSFILE.DS_OUTBAL_BNKRPT_3_2_2,
      *        DSFILE.DS_UNITS_BNKRPT_3_2_3,
      *        DSFILE.DS_OUTBAL_BNKRPT_3_2_3,
      *        DSFILE.DS_UNITS_BNKRPT_3_2_4,
      *        DSFILE.DS_OUTBAL_BNKRPT_3_2_4,
      *        DSFILE.DS_UNITS_BNKRPT_3_2_5,
      *        DSFILE.DS_OUTBAL_BNKRPT_3_2_5,
      *        DSFILE.DS_UNITS_BNKRPT_3_2_6,
      *        DSFILE.DS_OUTBAL_BNKRPT_3_2_6,
      *        DSFILE.DS_UNITS_BNKRPT_3_2_7,
      *        DSFILE.DS_OUTBAL_BNKRPT_3_2_7,
      *        DSFILE.DS_UNITS_BNKRPT_3_2_8,
      *        DSFILE.DS_OUTBAL_BNKRPT_3_2_8,
      *        DSFILE.DS_UNITS_BNKRPT_3_2_9,
      *        DSFILE.DS_OUTBAL_BNKRPT_3_2_9,
      *        DSFILE.DS_UNITS_BNKRPT_3_2_10,
      *        DSFILE.DS_OUTBAL_BNKRPT_3_2_10,
      *        DSFILE.DS_UNEARNED_AMT_1_1_1,
      *        DSFILE.DS_UNEARNED_AMT_1_1_2,
      *        DSFILE.DS_UNEARNED_AMT_1_1_3,
      *        DSFILE.DS_UNEARNED_AMT_1_1_4,
      *        DSFILE.DS_UNEARNED_AMT_1_1_5,
      *        DSFILE.DS_UNEARNED_AMT_1_1_6,
      *        DSFILE.DS_UNEARNED_AMT_1_1_7,
      *        DSFILE.DS_UNEARNED_AMT_1_1_8,
      *        DSFILE.DS_UNEARNED_AMT_1_1_9,
      *        DSFILE.DS_UNEARNED_AMT_1_1_10,
      *        DSFILE.DS_UNEARNED_AMT_1_2_1,
      *        DSFILE.DS_UNEARNED_AMT_1_2_2,
      *        DSFILE.DS_UNEARNED_AMT_1_2_3,
      *        DSFILE.DS_UNEARNED_AMT_1_2_4,
      *        DSFILE.DS_UNEARNED_AMT_1_2_5,
      *        DSFILE.DS_UNEARNED_AMT_1_2_6,
      *        DSFILE.DS_UNEARNED_AMT_1_2_7,
      *        DSFILE.DS_UNEARNED_AMT_1_2_8,
      *        DSFILE.DS_UNEARNED_AMT_1_2_9,
      *        DSFILE.DS_UNEARNED_AMT_1_2_10,
      *        DSFILE.DS_UNEARNED_AMT_2_1_1,
      *        DSFILE.DS_UNEARNED_AMT_2_1_2,
      *        DSFILE.DS_UNEARNED_AMT_2_1_3,
      *        DSFILE.DS_UNEARNED_AMT_2_1_4,
      *        DSFILE.DS_UNEARNED_AMT_2_1_5,
      *        DSFILE.DS_UNEARNED_AMT_2_1_6,
      *        DSFILE.DS_UNEARNED_AMT_2_1_7,
      *        DSFILE.DS_UNEARNED_AMT_2_1_8,
      *        DSFILE.DS_UNEARNED_AMT_2_1_9,
      *        DSFILE.DS_UNEARNED_AMT_2_1_10,
      *        DSFILE.DS_UNEARNED_AMT_2_2_1,
      *        DSFILE.DS_UNEARNED_AMT_2_2_2,
      *        DSFILE.DS_UNEARNED_AMT_2_2_3,
      *        DSFILE.DS_UNEARNED_AMT_2_2_4,
      *        DSFILE.DS_UNEARNED_AMT_2_2_5,
      *        DSFILE.DS_UNEARNED_AMT_2_2_6,
      *        DSFILE.DS_UNEARNED_AMT_2_2_7,
      *        DSFILE.DS_UNEARNED_AMT_2_2_8,
      *        DSFILE.DS_UNEARNED_AMT_2_2_9,
      *        DSFILE.DS_UNEARNED_AMT_2_2_10,
      *        DSFILE.DS_UNEARNED_AMT_3_1_1,
      *        DSFILE.DS_UNEARNED_AMT_3_1_2,
      *        DSFILE.DS_UNEARNED_AMT_3_1_3,
      *        DSFILE.DS_UNEARNED_AMT_3_1_4,
      *        DSFILE.DS_UNEARNED_AMT_3_1_5,
      *        DSFILE.DS_UNEARNED_AMT_3_1_6,
      *        DSFILE.DS_UNEARNED_AMT_3_1_7,
      *        DSFILE.DS_UNEARNED_AMT_3_1_8,
      *        DSFILE.DS_UNEARNED_AMT_3_1_9,
      *        DSFILE.DS_UNEARNED_AMT_3_1_10,
      *        DSFILE.DS_UNEARNED_AMT_3_2_1,
      *        DSFILE.DS_UNEARNED_AMT_3_2_2,
      *        DSFILE.DS_UNEARNED_AMT_3_2_3,
      *        DSFILE.DS_UNEARNED_AMT_3_2_4,
      *        DSFILE.DS_UNEARNED_AMT_3_2_5,
      *        DSFILE.DS_UNEARNED_AMT_3_2_6,
      *        DSFILE.DS_UNEARNED_AMT_3_2_7,
      *        DSFILE.DS_UNEARNED_AMT_3_2_8,
      *        DSFILE.DS_UNEARNED_AMT_3_2_9,
      *        DSFILE.DS_UNEARNED_AMT_3_2_10,
      *        DSFILE.DS_DLDISC_VOL_1,
      *        DSFILE.DS_DLDISC_VOL_2,
      *        DSFILE.DS_DLDISC_VOL_3,
      *        DSFILE.DS_AFPDUEDL_MEMO,
      *        DSFILE.DS_UNPAID_MANUAL,
      *        DSFILE.DS_LTOUCH_DATE)
      *      VALUES
      *       (:QDS-DATE-CCYYMM,
      *        :QDS-BRANCH,
      *        :QDS-NUM,
      *        :QDS-CLASS,
      *        :QDS-DLRVPRIN-1,
      *        :QDS-DLRVPRIN-2,
      *        :QDS-DLRVPRIN-3,
      *        :QDS-DLRVCHGS-1,
      *        :QDS-DLRVCHGS-2,
      *        :QDS-DLRVCHGS-3,
      *        :QDS-DLPARVHELD,
      *        :QDS-UNPAIDCK,
      *        :QDS-DLDISC,
      *        :QDS-DLPARVPAID,
      *        :QDS-SUMWGTTM,
      *        :QDS-SUMWGTAV,
      *        :QDS-SUMWGTTA,
      *        :QDS-RCOURSE-UNITS-1,
      *        :QDS-RCOURSE-UNITS-2,
      *        :QDS-RCOURSE-UNITS-3,
      *        :QDS-RCOURSE-AMT-1,
      *        :QDS-RCOURSE-AMT-2,
      *        :QDS-RCOURSE-AMT-3,
      *        :QDS-NPURUNIT-1,
      *        :QDS-NPURUNIT-2,
      *        :QDS-NPURUNIT-3,
      *        :QDS-NPURAMT-1,
      *        :QDS-NPURAMT-2,
      *        :QDS-NPURAMT-3,
      *        :QDS-OPURUNIT-1,
      *        :QDS-OPURUNIT-2,
      *        :QDS-OPURUNIT-3,
      *        :QDS-OPURAMT-1,
      *        :QDS-OPURAMT-2,
      *        :QDS-OPURAMT-3,
      *        :QDS-RNEWUNIT-1,
      *        :QDS-RNEWUNIT-2,
      *        :QDS-RNEWUNIT-3,
      *        :QDS-RNEWAMT-1,
      *        :QDS-RNEWAMT-2,
      *        :QDS-RNEWAMT-3,
      *        :QDS-PAIDUNIT-1,
      *        :QDS-PAIDUNIT-2,
      *        :QDS-PAIDUNIT-3,
      *        :QDS-PAIDAMT-1,
      *        :QDS-PAIDAMT-2,
      *        :QDS-PAIDAMT-3,
      *        :QDS-TODLUNIT-1,
      *        :QDS-TODLUNIT-2,
      *        :QDS-TODLUNIT-3,
      *        :QDS-TODLAMT-1,
      *        :QDS-TODLAMT-2,
      *        :QDS-TODLAMT-3,
      *        :QDS-OUTSUNIT-1,
      *        :QDS-OUTSUNIT-2,
      *        :QDS-OUTSUNIT-3,
      *        :QDS-OUTSAMT-1,
      *        :QDS-OUTSAMT-2,
      *        :QDS-OUTSAMT-3,
      *        :QDS-PLOSUNIT-1,
      *        :QDS-PLOSUNIT-2,
      *        :QDS-PLOSUNIT-3,
      *        :QDS-PLOSAMT-1,
      *        :QDS-PLOSAMT-2,
      *        :QDS-PLOSAMT-3,
      *        :QDS-RCPLUNIT-1,
      *        :QDS-RCPLUNIT-2,
      *        :QDS-RCPLUNIT-3,
      *        :QDS-RCPLAMT-1,
      *        :QDS-RCPLAMT-2,
      *        :QDS-RCPLAMT-3,
      *        :QDS-JDOSUNIT-1,
      *        :QDS-JDOSUNIT-2,
      *        :QDS-JDOSUNIT-3,
      *        :QDS-JDOSAMT-1,
      *        :QDS-JDOSAMT-2,
      *        :QDS-JDOSAMT-3,
      *        :QDS-DELUNEARN-DATE,
      *        :QDS-PROCEEDS-1,
      *        :QDS-PROCEEDS-2,
      *        :QDS-NO-REPO-1-1-1,
      *        :QDS-AMT-REPO-1-1-1,
      *        :QDS-NO-REPO-1-1-2,
      *        :QDS-AMT-REPO-1-1-2,
      *        :QDS-NO-REPO-1-1-3,
      *        :QDS-AMT-REPO-1-1-3,
      *        :QDS-NO-REPO-1-1-4,
      *        :QDS-AMT-REPO-1-1-4,
      *        :QDS-NO-REPO-1-1-5,
      *        :QDS-AMT-REPO-1-1-5,
      *        :QDS-NO-REPO-1-2-1,
      *        :QDS-AMT-REPO-1-2-1,
      *        :QDS-NO-REPO-1-2-2,
      *        :QDS-AMT-REPO-1-2-2,
      *        :QDS-NO-REPO-1-2-3,
      *        :QDS-AMT-REPO-1-2-3,
      *        :QDS-NO-REPO-1-2-4,
      *        :QDS-AMT-REPO-1-2-4,
      *        :QDS-NO-REPO-1-2-5,
      *        :QDS-AMT-REPO-1-2-5,
      *        :QDS-NO-REPO-2-1-1,
      *        :QDS-AMT-REPO-2-1-1,
      *        :QDS-NO-REPO-2-1-2,
      *        :QDS-AMT-REPO-2-1-2,
      *        :QDS-NO-REPO-2-1-3,
      *        :QDS-AMT-REPO-2-1-3,
      *        :QDS-NO-REPO-2-1-4,
      *        :QDS-AMT-REPO-2-1-4,
      *        :QDS-NO-REPO-2-1-5,
      *        :QDS-AMT-REPO-2-1-5,
      *        :QDS-NO-REPO-2-2-1,
      *        :QDS-AMT-REPO-2-2-1,
      *        :QDS-NO-REPO-2-2-2,
      *        :QDS-AMT-REPO-2-2-2,
      *        :QDS-NO-REPO-2-2-3,
      *        :QDS-AMT-REPO-2-2-3,
      *        :QDS-NO-REPO-2-2-4,
      *        :QDS-AMT-REPO-2-2-4,
      *        :QDS-NO-REPO-2-2-5,
      *        :QDS-AMT-REPO-2-2-5,
      *        :QDS-NO-REPO-3-1-1,
      *        :QDS-AMT-REPO-3-1-1,
      *        :QDS-NO-REPO-3-1-2,
      *        :QDS-AMT-REPO-3-1-2,
      *        :QDS-NO-REPO-3-1-3,
      *        :QDS-AMT-REPO-3-1-3,
      *        :QDS-NO-REPO-3-1-4,
      *        :QDS-AMT-REPO-3-1-4,
      *        :QDS-NO-REPO-3-1-5,
      *        :QDS-AMT-REPO-3-1-5,
      *        :QDS-NO-REPO-3-2-1,
      *        :QDS-AMT-REPO-3-2-1,
      *        :QDS-NO-REPO-3-2-2,
      *        :QDS-AMT-REPO-3-2-2,
      *        :QDS-NO-REPO-3-2-3,
      *        :QDS-AMT-REPO-3-2-3,
      *        :QDS-NO-REPO-3-2-4,
      *        :QDS-AMT-REPO-3-2-4,
      *        :QDS-NO-REPO-3-2-5,
      *        :QDS-AMT-REPO-3-2-5,
      *        :QDS-UNITS-1-1-1,
      *        :QDS-OUTBAL-1-1-1,
      *        :QDS-UNITS-1-1-2,
      *        :QDS-OUTBAL-1-1-2,
      *        :QDS-UNITS-1-1-3,
      *        :QDS-OUTBAL-1-1-3,
      *        :QDS-UNITS-1-1-4,
      *        :QDS-OUTBAL-1-1-4,
      *        :QDS-UNITS-1-1-5,
      *        :QDS-OUTBAL-1-1-5,
      *        :QDS-UNITS-1-1-6,
      *        :QDS-OUTBAL-1-1-6,
      *        :QDS-UNITS-1-1-7,
      *        :QDS-OUTBAL-1-1-7,
      *        :QDS-UNITS-1-1-8,
      *        :QDS-OUTBAL-1-1-8,
      *        :QDS-UNITS-1-1-9,
      *        :QDS-OUTBAL-1-1-9,
      *        :QDS-UNITS-1-1-10,
      *        :QDS-OUTBAL-1-1-10,
      *        :QDS-UNITS-1-2-1,
      *        :QDS-OUTBAL-1-2-1,
      *        :QDS-UNITS-1-2-2,
      *        :QDS-OUTBAL-1-2-2,
      *        :QDS-UNITS-1-2-3,
      *        :QDS-OUTBAL-1-2-3,
      *        :QDS-UNITS-1-2-4,
      *        :QDS-OUTBAL-1-2-4,
      *        :QDS-UNITS-1-2-5,
      *        :QDS-OUTBAL-1-2-5,
      *        :QDS-UNITS-1-2-6,
      *        :QDS-OUTBAL-1-2-6,
      *        :QDS-UNITS-1-2-7,
      *        :QDS-OUTBAL-1-2-7,
      *        :QDS-UNITS-1-2-8,
      *        :QDS-OUTBAL-1-2-8,
      *        :QDS-UNITS-1-2-9,
      *        :QDS-OUTBAL-1-2-9,
      *        :QDS-UNITS-1-2-10,
      *        :QDS-OUTBAL-1-2-10,
      *        :QDS-UNITS-2-1-1,
      *        :QDS-OUTBAL-2-1-1,
      *        :QDS-UNITS-2-1-2,
      *        :QDS-OUTBAL-2-1-2,
      *        :QDS-UNITS-2-1-3,
      *        :QDS-OUTBAL-2-1-3,
      *        :QDS-UNITS-2-1-4,
      *        :QDS-OUTBAL-2-1-4,
      *        :QDS-UNITS-2-1-5,
      *        :QDS-OUTBAL-2-1-5,
      *        :QDS-UNITS-2-1-6,
      *        :QDS-OUTBAL-2-1-6,
      *        :QDS-UNITS-2-1-7,
      *        :QDS-OUTBAL-2-1-7,
      *        :QDS-UNITS-2-1-8,
      *        :QDS-OUTBAL-2-1-8,
      *        :QDS-UNITS-2-1-9,
      *        :QDS-OUTBAL-2-1-9,
      *        :QDS-UNITS-2-1-10,
      *        :QDS-OUTBAL-2-1-10,
      *        :QDS-UNITS-2-2-1,
      *        :QDS-OUTBAL-2-2-1,
      *        :QDS-UNITS-2-2-2,
      *        :QDS-OUTBAL-2-2-2,
      *        :QDS-UNITS-2-2-3,
      *        :QDS-OUTBAL-2-2-3,
      *        :QDS-UNITS-2-2-4,
      *        :QDS-OUTBAL-2-2-4,
      *        :QDS-UNITS-2-2-5,
      *        :QDS-OUTBAL-2-2-5,
      *        :QDS-UNITS-2-2-6,
      *        :QDS-OUTBAL-2-2-6,
      *        :QDS-UNITS-2-2-7,
      *        :QDS-OUTBAL-2-2-7,
      *        :QDS-UNITS-2-2-8,
      *        :QDS-OUTBAL-2-2-8,
      *        :QDS-UNITS-2-2-9,
      *        :QDS-OUTBAL-2-2-9,
      *        :QDS-UNITS-2-2-10,
      *        :QDS-OUTBAL-2-2-10,
      *        :QDS-UNITS-3-1-1,
      *        :QDS-OUTBAL-3-1-1,
      *        :QDS-UNITS-3-1-2,
      *        :QDS-OUTBAL-3-1-2,
      *        :QDS-UNITS-3-1-3,
      *        :QDS-OUTBAL-3-1-3,
      *        :QDS-UNITS-3-1-4,
      *        :QDS-OUTBAL-3-1-4,
      *        :QDS-UNITS-3-1-5,
      *        :QDS-OUTBAL-3-1-5,
      *        :QDS-UNITS-3-1-6,
      *        :QDS-OUTBAL-3-1-6,
      *        :QDS-UNITS-3-1-7,
      *        :QDS-OUTBAL-3-1-7,
      *        :QDS-UNITS-3-1-8,
      *        :QDS-OUTBAL-3-1-8,
      *        :QDS-UNITS-3-1-9,
      *        :QDS-OUTBAL-3-1-9,
      *        :QDS-UNITS-3-1-10,
      *        :QDS-OUTBAL-3-1-10,
      *        :QDS-UNITS-3-2-1,
      *        :QDS-OUTBAL-3-2-1,
      *        :QDS-UNITS-3-2-2,
      *        :QDS-OUTBAL-3-2-2,
      *        :QDS-UNITS-3-2-3,
      *        :QDS-OUTBAL-3-2-3,
      *        :QDS-UNITS-3-2-4,
      *        :QDS-OUTBAL-3-2-4,
      *        :QDS-UNITS-3-2-5,
      *        :QDS-OUTBAL-3-2-5,
      *        :QDS-UNITS-3-2-6,
      *        :QDS-OUTBAL-3-2-6,
      *        :QDS-UNITS-3-2-7,
      *        :QDS-OUTBAL-3-2-7,
      *        :QDS-UNITS-3-2-8,
      *        :QDS-OUTBAL-3-2-8,
      *        :QDS-UNITS-3-2-9,
      *        :QDS-OUTBAL-3-2-9,
      *        :QDS-UNITS-3-2-10,
      *        :QDS-OUTBAL-3-2-10,
      *        :QDS-UNITS-BNKRPT-1-1-1,
      *        :QDS-OUTBAL-BNKRPT-1-1-1,
      *        :QDS-UNITS-BNKRPT-1-1-2,
      *        :QDS-OUTBAL-BNKRPT-1-1-2,
      *        :QDS-UNITS-BNKRPT-1-1-3,
      *        :QDS-OUTBAL-BNKRPT-1-1-3,
      *        :QDS-UNITS-BNKRPT-1-1-4,
      *        :QDS-OUTBAL-BNKRPT-1-1-4,
      *        :QDS-UNITS-BNKRPT-1-1-5,
      *        :QDS-OUTBAL-BNKRPT-1-1-5,
      *        :QDS-UNITS-BNKRPT-1-1-6,
      *        :QDS-OUTBAL-BNKRPT-1-1-6,
      *        :QDS-UNITS-BNKRPT-1-1-7,
      *        :QDS-OUTBAL-BNKRPT-1-1-7,
      *        :QDS-UNITS-BNKRPT-1-1-8,
      *        :QDS-OUTBAL-BNKRPT-1-1-8,
      *        :QDS-UNITS-BNKRPT-1-1-9,
      *        :QDS-OUTBAL-BNKRPT-1-1-9,
      *        :QDS-UNITS-BNKRPT-1-1-10,
      *        :QDS-OUTBAL-BNKRPT-1-1-10,
      *        :QDS-UNITS-BNKRPT-1-2-1,
      *        :QDS-OUTBAL-BNKRPT-1-2-1,
      *        :QDS-UNITS-BNKRPT-1-2-2,
      *        :QDS-OUTBAL-BNKRPT-1-2-2,
      *        :QDS-UNITS-BNKRPT-1-2-3,
      *        :QDS-OUTBAL-BNKRPT-1-2-3,
      *        :QDS-UNITS-BNKRPT-1-2-4,
      *        :QDS-OUTBAL-BNKRPT-1-2-4,
      *        :QDS-UNITS-BNKRPT-1-2-5,
      *        :QDS-OUTBAL-BNKRPT-1-2-5,
      *        :QDS-UNITS-BNKRPT-1-2-6,
      *        :QDS-OUTBAL-BNKRPT-1-2-6,
      *        :QDS-UNITS-BNKRPT-1-2-7,
      *        :QDS-OUTBAL-BNKRPT-1-2-7,
      *        :QDS-UNITS-BNKRPT-1-2-8,
      *        :QDS-OUTBAL-BNKRPT-1-2-8,
      *        :QDS-UNITS-BNKRPT-1-2-9,
      *        :QDS-OUTBAL-BNKRPT-1-2-9,
      *        :QDS-UNITS-BNKRPT-1-2-10,
      *        :QDS-OUTBAL-BNKRPT-1-2-10,
      *        :QDS-UNITS-BNKRPT-2-1-1,
      *        :QDS-OUTBAL-BNKRPT-2-1-1,
      *        :QDS-UNITS-BNKRPT-2-1-2,
      *        :QDS-OUTBAL-BNKRPT-2-1-2,
      *        :QDS-UNITS-BNKRPT-2-1-3,
      *        :QDS-OUTBAL-BNKRPT-2-1-3,
      *        :QDS-UNITS-BNKRPT-2-1-4,
      *        :QDS-OUTBAL-BNKRPT-2-1-4,
      *        :QDS-UNITS-BNKRPT-2-1-5,
      *        :QDS-OUTBAL-BNKRPT-2-1-5,
      *        :QDS-UNITS-BNKRPT-2-1-6,
      *        :QDS-OUTBAL-BNKRPT-2-1-6,
      *        :QDS-UNITS-BNKRPT-2-1-7,
      *        :QDS-OUTBAL-BNKRPT-2-1-7,
      *        :QDS-UNITS-BNKRPT-2-1-8,
      *        :QDS-OUTBAL-BNKRPT-2-1-8,
      *        :QDS-UNITS-BNKRPT-2-1-9,
      *        :QDS-OUTBAL-BNKRPT-2-1-9,
      *        :QDS-UNITS-BNKRPT-2-1-10,
      *        :QDS-OUTBAL-BNKRPT-2-1-10,
      *        :QDS-UNITS-BNKRPT-2-2-1,
      *        :QDS-OUTBAL-BNKRPT-2-2-1,
      *        :QDS-UNITS-BNKRPT-2-2-2,
      *        :QDS-OUTBAL-BNKRPT-2-2-2,
      *        :QDS-UNITS-BNKRPT-2-2-3,
      *        :QDS-OUTBAL-BNKRPT-2-2-3,
      *        :QDS-UNITS-BNKRPT-2-2-4,
      *        :QDS-OUTBAL-BNKRPT-2-2-4,
      *        :QDS-UNITS-BNKRPT-2-2-5,
      *        :QDS-OUTBAL-BNKRPT-2-2-5,
      *        :QDS-UNITS-BNKRPT-2-2-6,
      *        :QDS-OUTBAL-BNKRPT-2-2-6,
      *        :QDS-UNITS-BNKRPT-2-2-7,
      *        :QDS-OUTBAL-BNKRPT-2-2-7,
      *        :QDS-UNITS-BNKRPT-2-2-8,
      *        :QDS-OUTBAL-BNKRPT-2-2-8,
      *        :QDS-UNITS-BNKRPT-2-2-9,
      *        :QDS-OUTBAL-BNKRPT-2-2-9,
      *        :QDS-UNITS-BNKRPT-2-2-10,
      *        :QDS-OUTBAL-BNKRPT-2-2-10,
      *        :QDS-UNITS-BNKRPT-3-1-1,
      *        :QDS-OUTBAL-BNKRPT-3-1-1,
      *        :QDS-UNITS-BNKRPT-3-1-2,
      *        :QDS-OUTBAL-BNKRPT-3-1-2,
      *        :QDS-UNITS-BNKRPT-3-1-3,
      *        :QDS-OUTBAL-BNKRPT-3-1-3,
      *        :QDS-UNITS-BNKRPT-3-1-4,
      *        :QDS-OUTBAL-BNKRPT-3-1-4,
      *        :QDS-UNITS-BNKRPT-3-1-5,
      *        :QDS-OUTBAL-BNKRPT-3-1-5,
      *        :QDS-UNITS-BNKRPT-3-1-6,
      *        :QDS-OUTBAL-BNKRPT-3-1-6,
      *        :QDS-UNITS-BNKRPT-3-1-7,
      *        :QDS-OUTBAL-BNKRPT-3-1-7,
      *        :QDS-UNITS-BNKRPT-3-1-8,
      *        :QDS-OUTBAL-BNKRPT-3-1-8,
      *        :QDS-UNITS-BNKRPT-3-1-9,
      *        :QDS-OUTBAL-BNKRPT-3-1-9,
      *        :QDS-UNITS-BNKRPT-3-1-10,
      *        :QDS-OUTBAL-BNKRPT-3-1-10,
      *        :QDS-UNITS-BNKRPT-3-2-1,
      *        :QDS-OUTBAL-BNKRPT-3-2-1,
      *        :QDS-UNITS-BNKRPT-3-2-2,
      *        :QDS-OUTBAL-BNKRPT-3-2-2,
      *        :QDS-UNITS-BNKRPT-3-2-3,
      *        :QDS-OUTBAL-BNKRPT-3-2-3,
      *        :QDS-UNITS-BNKRPT-3-2-4,
      *        :QDS-OUTBAL-BNKRPT-3-2-4,
      *        :QDS-UNITS-BNKRPT-3-2-5,
      *        :QDS-OUTBAL-BNKRPT-3-2-5,
      *        :QDS-UNITS-BNKRPT-3-2-6,
      *        :QDS-OUTBAL-BNKRPT-3-2-6,
      *        :QDS-UNITS-BNKRPT-3-2-7,
      *        :QDS-OUTBAL-BNKRPT-3-2-7,
      *        :QDS-UNITS-BNKRPT-3-2-8,
      *        :QDS-OUTBAL-BNKRPT-3-2-8,
      *        :QDS-UNITS-BNKRPT-3-2-9,
      *        :QDS-OUTBAL-BNKRPT-3-2-9,
      *        :QDS-UNITS-BNKRPT-3-2-10,
      *        :QDS-OUTBAL-BNKRPT-3-2-10,
      *        :QDS-UNEARNED-AMT-1-1-1,
      *        :QDS-UNEARNED-AMT-1-1-2,
      *        :QDS-UNEARNED-AMT-1-1-3,
      *        :QDS-UNEARNED-AMT-1-1-4,
      *        :QDS-UNEARNED-AMT-1-1-5,
      *        :QDS-UNEARNED-AMT-1-1-6,
      *        :QDS-UNEARNED-AMT-1-1-7,
      *        :QDS-UNEARNED-AMT-1-1-8,
      *        :QDS-UNEARNED-AMT-1-1-9,
      *        :QDS-UNEARNED-AMT-1-1-10,
      *        :QDS-UNEARNED-AMT-1-2-1,
      *        :QDS-UNEARNED-AMT-1-2-2,
      *        :QDS-UNEARNED-AMT-1-2-3,
      *        :QDS-UNEARNED-AMT-1-2-4,
      *        :QDS-UNEARNED-AMT-1-2-5,
      *        :QDS-UNEARNED-AMT-1-2-6,
      *        :QDS-UNEARNED-AMT-1-2-7,
      *        :QDS-UNEARNED-AMT-1-2-8,
      *        :QDS-UNEARNED-AMT-1-2-9,
      *        :QDS-UNEARNED-AMT-1-2-10,
      *        :QDS-UNEARNED-AMT-2-1-1,
      *        :QDS-UNEARNED-AMT-2-1-2,
      *        :QDS-UNEARNED-AMT-2-1-3,
      *        :QDS-UNEARNED-AMT-2-1-4,
      *        :QDS-UNEARNED-AMT-2-1-5,
      *        :QDS-UNEARNED-AMT-2-1-6,
      *        :QDS-UNEARNED-AMT-2-1-7,
      *        :QDS-UNEARNED-AMT-2-1-8,
      *        :QDS-UNEARNED-AMT-2-1-9,
      *        :QDS-UNEARNED-AMT-2-1-10,
      *        :QDS-UNEARNED-AMT-2-2-1,
      *        :QDS-UNEARNED-AMT-2-2-2,
      *        :QDS-UNEARNED-AMT-2-2-3,
      *        :QDS-UNEARNED-AMT-2-2-4,
      *        :QDS-UNEARNED-AMT-2-2-5,
      *        :QDS-UNEARNED-AMT-2-2-6,
      *        :QDS-UNEARNED-AMT-2-2-7,
      *        :QDS-UNEARNED-AMT-2-2-8,
      *        :QDS-UNEARNED-AMT-2-2-9,
      *        :QDS-UNEARNED-AMT-2-2-10,
      *        :QDS-UNEARNED-AMT-3-1-1,
      *        :QDS-UNEARNED-AMT-3-1-2,
      *        :QDS-UNEARNED-AMT-3-1-3,
      *        :QDS-UNEARNED-AMT-3-1-4,
      *        :QDS-UNEARNED-AMT-3-1-5,
      *        :QDS-UNEARNED-AMT-3-1-6,
      *        :QDS-UNEARNED-AMT-3-1-7,
      *        :QDS-UNEARNED-AMT-3-1-8,
      *        :QDS-UNEARNED-AMT-3-1-9,
      *        :QDS-UNEARNED-AMT-3-1-10,
      *        :QDS-UNEARNED-AMT-3-2-1,
      *        :QDS-UNEARNED-AMT-3-2-2,
      *        :QDS-UNEARNED-AMT-3-2-3,
      *        :QDS-UNEARNED-AMT-3-2-4,
      *        :QDS-UNEARNED-AMT-3-2-5,
      *        :QDS-UNEARNED-AMT-3-2-6,
      *        :QDS-UNEARNED-AMT-3-2-7,
      *        :QDS-UNEARNED-AMT-3-2-8,
      *        :QDS-UNEARNED-AMT-3-2-9,
      *        :QDS-UNEARNED-AMT-3-2-10,
      *        :QDS-DLDISC-VOL-1,
      *        :QDS-DLDISC-VOL-2,
      *        :QDS-DLDISC-VOL-3,
      *        :QDS-AFPDUEDL-MEMO,
      *        :QDS-UNPAID-MANUAL,
      *        :QDS-LTOUCH-DATE)
      *     END-EXEC.

      *     PERFORM SQL-IO-VALIDATION.

      *     MOVE '1' TO SQL-UPDATE-CONN-USE-SW.    *> CHANGE CONNECTION BACK TO C1
      *     PERFORM SQL-SWAP-UPDATE-CONNECTION.

      *     IF ( IO-FG = 8 )     *> LOCKED
      *        GO TO WRITE-DS1-FILE.

      **    IF ( IO-FG = 0 )
      **       EXEC SQL COMMIT END-EXEC
      **       PERFORM SQL-IO-VALIDATION.

      **----------------------------------------------------------------
      * REWRITE-DS1-FILE.

      *     MOVE DS-PATH-OWNBR  TO DS-BRANCH.
      *     ACCEPT DS-LTOUCH-DATE FROM CENTURY-DATE.
      *     PERFORM REWRITE-IT.
      *     MOVE DS-PATH-SQL    TO E-FILE.
      *     MOVE DS1-KEY        TO E-KEYX.
      *     PERFORM SET-DS-FIELDS.

      *     MOVE '2' TO SQL-UPDATE-CONN-USE-SW.    *> CHANGE CONNECTION TO UPDATE_CONNECT
      *     PERFORM SQL-SWAP-UPDATE-CONNECTION.

      *     EXEC SQL
      *      UPDATE DBO.DSFILE
      *       SET
      *        DSFILE.DS_DATE_CCYYMM = :QDS-DATE-CCYYMM,
      *        DSFILE.DS_BRANCH      = :QDS-BRANCH,
      *        DSFILE.DS_NUM         = :QDS-NUM,
      *        DSFILE.DS_CLASS       = :QDS-CLASS,
      *        DSFILE.DS_DLRVPRIN_1  = :QDS-DLRVPRIN-1,
      *        DSFILE.DS_DLRVPRIN_2  = :QDS-DLRVPRIN-2,
      *        DSFILE.DS_DLRVPRIN_3  = :QDS-DLRVPRIN-3,
      *        DSFILE.DS_DLRVCHGS_1  = :QDS-DLRVCHGS-1,
      *        DSFILE.DS_DLRVCHGS_2  = :QDS-DLRVCHGS-2,
      *        DSFILE.DS_DLRVCHGS_3  = :QDS-DLRVCHGS-3,
      *        DSFILE.DS_DLPARVHELD  = :QDS-DLPARVHELD,
      *        DSFILE.DS_UNPAIDCK    = :QDS-UNPAIDCK,
      *        DSFILE.DS_DLDISC      = :QDS-DLDISC,
      *        DSFILE.DS_DLPARVPAID  = :QDS-DLPARVPAID,
      *        DSFILE.DS_SUMWGTTM    = :QDS-SUMWGTTM,
      *        DSFILE.DS_SUMWGTAV    = :QDS-SUMWGTAV,
      *        DSFILE.DS_SUMWGTTA    = :QDS-SUMWGTTA,
      *        DSFILE.DS_RCOURSE_UNITS_1 = :QDS-RCOURSE-UNITS-1,
      *        DSFILE.DS_RCOURSE_UNITS_2 = :QDS-RCOURSE-UNITS-2,
      *        DSFILE.DS_RCOURSE_UNITS_3 = :QDS-RCOURSE-UNITS-3,
      *        DSFILE.DS_RCOURSE_AMT_1   = :QDS-RCOURSE-AMT-1,
      *        DSFILE.DS_RCOURSE_AMT_2   = :QDS-RCOURSE-AMT-2,
      *        DSFILE.DS_RCOURSE_AMT_3   = :QDS-RCOURSE-AMT-3,
      *        DSFILE.DS_NPURUNIT_1 = :QDS-NPURUNIT-1,
      *        DSFILE.DS_NPURUNIT_2 = :QDS-NPURUNIT-2,
      *        DSFILE.DS_NPURUNIT_3 = :QDS-NPURUNIT-3,
      *        DSFILE.DS_NPURAMT_1  = :QDS-NPURAMT-1,
      *        DSFILE.DS_NPURAMT_2  = :QDS-NPURAMT-2,
      *        DSFILE.DS_NPURAMT_3  = :QDS-NPURAMT-3,
      *        DSFILE.DS_OPURUNIT_1 = :QDS-OPURUNIT-1,
      *        DSFILE.DS_OPURUNIT_2 = :QDS-OPURUNIT-2,
      *        DSFILE.DS_OPURUNIT_3 = :QDS-OPURUNIT-3,
      *        DSFILE.DS_OPURAMT_1  = :QDS-OPURAMT-1,
      *        DSFILE.DS_OPURAMT_2  = :QDS-OPURAMT-2,
      *        DSFILE.DS_OPURAMT_3  = :QDS-OPURAMT-3,
      *        DSFILE.DS_RNEWUNIT_1 = :QDS-RNEWUNIT-1,
      *        DSFILE.DS_RNEWUNIT_2 = :QDS-RNEWUNIT-2,
      *        DSFILE.DS_RNEWUNIT_3 = :QDS-RNEWUNIT-3,
      *        DSFILE.DS_RNEWAMT_1  = :QDS-RNEWAMT-1,
      *        DSFILE.DS_RNEWAMT_2  = :QDS-RNEWAMT-2,
      *        DSFILE.DS_RNEWAMT_3  = :QDS-RNEWAMT-3,
      *        DSFILE.DS_PAIDUNIT_1 = :QDS-PAIDUNIT-1,
      *        DSFILE.DS_PAIDUNIT_2 = :QDS-PAIDUNIT-2,
      *        DSFILE.DS_PAIDUNIT_3 = :QDS-PAIDUNIT-3,
      *        DSFILE.DS_PAIDAMT_1  = :QDS-PAIDAMT-1,
      *        DSFILE.DS_PAIDAMT_2  = :QDS-PAIDAMT-2,
      *        DSFILE.DS_PAIDAMT_3  = :QDS-PAIDAMT-3,
      *        DSFILE.DS_TODLUNIT_1 = :QDS-TODLUNIT-1,
      *        DSFILE.DS_TODLUNIT_2 = :QDS-TODLUNIT-2,
      *        DSFILE.DS_TODLUNIT_3 = :QDS-TODLUNIT-3,
      *        DSFILE.DS_TODLAMT_1  = :QDS-TODLAMT-1,
      *        DSFILE.DS_TODLAMT_2  = :QDS-TODLAMT-2,
      *        DSFILE.DS_TODLAMT_3  = :QDS-TODLAMT-3,
      *        DSFILE.DS_OUTSUNIT_1 = :QDS-OUTSUNIT-1,
      *        DSFILE.DS_OUTSUNIT_2 = :QDS-OUTSUNIT-2,
      *        DSFILE.DS_OUTSUNIT_3 = :QDS-OUTSUNIT-3,
      *        DSFILE.DS_OUTSAMT_1  = :QDS-OUTSAMT-1,
      *        DSFILE.DS_OUTSAMT_2  = :QDS-OUTSAMT-2,
      *        DSFILE.DS_OUTSAMT_3  = :QDS-OUTSAMT-3,
      *        DSFILE.DS_PLOSUNIT_1 = :QDS-PLOSUNIT-1,
      *        DSFILE.DS_PLOSUNIT_2 = :QDS-PLOSUNIT-2,
      *        DSFILE.DS_PLOSUNIT_3 = :QDS-PLOSUNIT-3,
      *        DSFILE.DS_PLOSAMT_1  = :QDS-PLOSAMT-1,
      *        DSFILE.DS_PLOSAMT_2  = :QDS-PLOSAMT-2,
      *        DSFILE.DS_PLOSAMT_3  = :QDS-PLOSAMT-3,
      *        DSFILE.DS_RCPLUNIT_1 = :QDS-RCPLUNIT-1,
      *        DSFILE.DS_RCPLUNIT_2 = :QDS-RCPLUNIT-2,
      *        DSFILE.DS_RCPLUNIT_3 = :QDS-RCPLUNIT-3,
      *        DSFILE.DS_RCPLAMT_1  = :QDS-RCPLAMT-1,
      *        DSFILE.DS_RCPLAMT_2  = :QDS-RCPLAMT-2,
      *        DSFILE.DS_RCPLAMT_3  = :QDS-RCPLAMT-3,
      *        DSFILE.DS_JDOSUNIT_1 = :QDS-JDOSUNIT-1,
      *        DSFILE.DS_JDOSUNIT_2 = :QDS-JDOSUNIT-2,
      *        DSFILE.DS_JDOSUNIT_3 = :QDS-JDOSUNIT-3,
      *        DSFILE.DS_JDOSAMT_1  = :QDS-JDOSAMT-1,
      *        DSFILE.DS_JDOSAMT_2  = :QDS-JDOSAMT-2,
      *        DSFILE.DS_JDOSAMT_3  = :QDS-JDOSAMT-3,
      *        DSFILE.DS_DELUNEARN_DATE = :QDS-DELUNEARN-DATE,
      *        DSFILE.DS_PROCEEDS_1 = :QDS-PROCEEDS-1,
      *        DSFILE.DS_PROCEEDS_2 = :QDS-PROCEEDS-2,
      *        DSFILE.DS_NO_REPO_1_1_1  = :QDS-NO-REPO-1-1-1,
      *        DSFILE.DS_AMT_REPO_1_1_1 = :QDS-AMT-REPO-1-1-1,
      *        DSFILE.DS_NO_REPO_1_1_2  = :QDS-NO-REPO-1-1-2,
      *        DSFILE.DS_AMT_REPO_1_1_2 = :QDS-AMT-REPO-1-1-2,
      *        DSFILE.DS_NO_REPO_1_1_3  = :QDS-NO-REPO-1-1-3,
      *        DSFILE.DS_AMT_REPO_1_1_3 = :QDS-AMT-REPO-1-1-3,
      *        DSFILE.DS_NO_REPO_1_1_4  = :QDS-NO-REPO-1-1-4,
      *        DSFILE.DS_AMT_REPO_1_1_4 = :QDS-AMT-REPO-1-1-4,
      *        DSFILE.DS_NO_REPO_1_1_5  = :QDS-NO-REPO-1-1-5,
      *        DSFILE.DS_AMT_REPO_1_1_5 = :QDS-AMT-REPO-1-1-5,
      *        DSFILE.DS_NO_REPO_1_2_1  = :QDS-NO-REPO-1-2-1,
      *        DSFILE.DS_AMT_REPO_1_2_1 = :QDS-AMT-REPO-1-2-1,
      *        DSFILE.DS_NO_REPO_1_2_2  = :QDS-NO-REPO-1-2-2,
      *        DSFILE.DS_AMT_REPO_1_2_2 = :QDS-AMT-REPO-1-2-2,
      *        DSFILE.DS_NO_REPO_1_2_3  = :QDS-NO-REPO-1-2-3,
      *        DSFILE.DS_AMT_REPO_1_2_3 = :QDS-AMT-REPO-1-2-3,
      *        DSFILE.DS_NO_REPO_1_2_4  = :QDS-NO-REPO-1-2-4,
      *        DSFILE.DS_AMT_REPO_1_2_4 = :QDS-AMT-REPO-1-2-4,
      *        DSFILE.DS_NO_REPO_1_2_5  = :QDS-NO-REPO-1-2-5,
      *        DSFILE.DS_AMT_REPO_1_2_5 = :QDS-AMT-REPO-1-2-5,
      *        DSFILE.DS_NO_REPO_2_1_1  = :QDS-NO-REPO-2-1-1,
      *        DSFILE.DS_AMT_REPO_2_1_1 = :QDS-AMT-REPO-2-1-1,
      *        DSFILE.DS_NO_REPO_2_1_2  = :QDS-NO-REPO-2-1-2,
      *        DSFILE.DS_AMT_REPO_2_1_2 = :QDS-AMT-REPO-2-1-2,
      *        DSFILE.DS_NO_REPO_2_1_3  = :QDS-NO-REPO-2-1-3,
      *        DSFILE.DS_AMT_REPO_2_1_3 = :QDS-AMT-REPO-2-1-3,
      *        DSFILE.DS_NO_REPO_2_1_4  = :QDS-NO-REPO-2-1-4,
      *        DSFILE.DS_AMT_REPO_2_1_4 = :QDS-AMT-REPO-2-1-4,
      *        DSFILE.DS_NO_REPO_2_1_5  = :QDS-NO-REPO-2-1-5,
      *        DSFILE.DS_AMT_REPO_2_1_5 = :QDS-AMT-REPO-2-1-5,
      *        DSFILE.DS_NO_REPO_2_2_1  = :QDS-NO-REPO-2-2-1,
      *        DSFILE.DS_AMT_REPO_2_2_1 = :QDS-AMT-REPO-2-2-1,
      *        DSFILE.DS_NO_REPO_2_2_2  = :QDS-NO-REPO-2-2-2,
      *        DSFILE.DS_AMT_REPO_2_2_2 = :QDS-AMT-REPO-2-2-2,
      *        DSFILE.DS_NO_REPO_2_2_3  = :QDS-NO-REPO-2-2-3,
      *        DSFILE.DS_AMT_REPO_2_2_3 = :QDS-AMT-REPO-2-2-3,
      *        DSFILE.DS_NO_REPO_2_2_4  = :QDS-NO-REPO-2-2-4,
      *        DSFILE.DS_AMT_REPO_2_2_4 = :QDS-AMT-REPO-2-2-4,
      *        DSFILE.DS_NO_REPO_2_2_5  = :QDS-NO-REPO-2-2-5,
      *        DSFILE.DS_AMT_REPO_2_2_5 = :QDS-AMT-REPO-2-2-5,
      *        DSFILE.DS_NO_REPO_3_1_1  = :QDS-NO-REPO-3-1-1,
      *        DSFILE.DS_AMT_REPO_3_1_1 = :QDS-AMT-REPO-3-1-1,
      *        DSFILE.DS_NO_REPO_3_1_2  = :QDS-NO-REPO-3-1-2,
      *        DSFILE.DS_AMT_REPO_3_1_2 = :QDS-AMT-REPO-3-1-2,
      *        DSFILE.DS_NO_REPO_3_1_3  = :QDS-NO-REPO-3-1-3,
      *        DSFILE.DS_AMT_REPO_3_1_3 = :QDS-AMT-REPO-3-1-3,
      *        DSFILE.DS_NO_REPO_3_1_4  = :QDS-NO-REPO-3-1-4,
      *        DSFILE.DS_AMT_REPO_3_1_4 = :QDS-AMT-REPO-3-1-4,
      *        DSFILE.DS_NO_REPO_3_1_5  = :QDS-NO-REPO-3-1-5,
      *        DSFILE.DS_AMT_REPO_3_1_5 = :QDS-AMT-REPO-3-1-5,
      *        DSFILE.DS_NO_REPO_3_2_1  = :QDS-NO-REPO-3-2-1,
      *        DSFILE.DS_AMT_REPO_3_2_1 = :QDS-AMT-REPO-3-2-1,
      *        DSFILE.DS_NO_REPO_3_2_2  = :QDS-NO-REPO-3-2-2,
      *        DSFILE.DS_AMT_REPO_3_2_2 = :QDS-AMT-REPO-3-2-2,
      *        DSFILE.DS_NO_REPO_3_2_3  = :QDS-NO-REPO-3-2-3,
      *        DSFILE.DS_AMT_REPO_3_2_3 = :QDS-AMT-REPO-3-2-3,
      *        DSFILE.DS_NO_REPO_3_2_4  = :QDS-NO-REPO-3-2-4,
      *        DSFILE.DS_AMT_REPO_3_2_4 = :QDS-AMT-REPO-3-2-4,
      *        DSFILE.DS_NO_REPO_3_2_5  = :QDS-NO-REPO-3-2-5,
      *        DSFILE.DS_AMT_REPO_3_2_5 = :QDS-AMT-REPO-3-2-5,
      *        DSFILE.DS_UNITS_1_1_1    = :QDS-UNITS-1-1-1,
      *        DSFILE.DS_OUTBAL_1_1_1   = :QDS-OUTBAL-1-1-1,
      *        DSFILE.DS_UNITS_1_1_2    = :QDS-UNITS-1-1-2,
      *        DSFILE.DS_OUTBAL_1_1_2   = :QDS-OUTBAL-1-1-2,
      *        DSFILE.DS_UNITS_1_1_3    = :QDS-UNITS-1-1-3,
      *        DSFILE.DS_OUTBAL_1_1_3   = :QDS-OUTBAL-1-1-3,
      *        DSFILE.DS_UNITS_1_1_4    = :QDS-UNITS-1-1-4,
      *        DSFILE.DS_OUTBAL_1_1_4   = :QDS-OUTBAL-1-1-4,
      *        DSFILE.DS_UNITS_1_1_5    = :QDS-UNITS-1-1-5,
      *        DSFILE.DS_OUTBAL_1_1_5   = :QDS-OUTBAL-1-1-5,
      *        DSFILE.DS_UNITS_1_1_6    = :QDS-UNITS-1-1-6,
      *        DSFILE.DS_OUTBAL_1_1_6   = :QDS-OUTBAL-1-1-6,
      *        DSFILE.DS_UNITS_1_1_7    = :QDS-UNITS-1-1-7,
      *        DSFILE.DS_OUTBAL_1_1_7   = :QDS-OUTBAL-1-1-7,
      *        DSFILE.DS_UNITS_1_1_8    = :QDS-UNITS-1-1-8,
      *        DSFILE.DS_OUTBAL_1_1_8   = :QDS-OUTBAL-1-1-8,
      *        DSFILE.DS_UNITS_1_1_9    = :QDS-UNITS-1-1-9,
      *        DSFILE.DS_OUTBAL_1_1_9   = :QDS-OUTBAL-1-1-9,
      *        DSFILE.DS_UNITS_1_1_10   = :QDS-UNITS-1-1-10,
      *        DSFILE.DS_OUTBAL_1_1_10  = :QDS-OUTBAL-1-1-10,
      *        DSFILE.DS_UNITS_1_2_1    = :QDS-UNITS-1-2-1,
      *        DSFILE.DS_OUTBAL_1_2_1   = :QDS-OUTBAL-1-2-1,
      *        DSFILE.DS_UNITS_1_2_2    = :QDS-UNITS-1-2-2,
      *        DSFILE.DS_OUTBAL_1_2_2   = :QDS-OUTBAL-1-2-2,
      *        DSFILE.DS_UNITS_1_2_3    = :QDS-UNITS-1-2-3,
      *        DSFILE.DS_OUTBAL_1_2_3   = :QDS-OUTBAL-1-2-3,
      *        DSFILE.DS_UNITS_1_2_4    = :QDS-UNITS-1-2-4,
      *        DSFILE.DS_OUTBAL_1_2_4   = :QDS-OUTBAL-1-2-4,
      *        DSFILE.DS_UNITS_1_2_5    = :QDS-UNITS-1-2-5,
      *        DSFILE.DS_OUTBAL_1_2_5   = :QDS-OUTBAL-1-2-5,
      *        DSFILE.DS_UNITS_1_2_6    = :QDS-UNITS-1-2-6,
      *        DSFILE.DS_OUTBAL_1_2_6   = :QDS-OUTBAL-1-2-6,
      *        DSFILE.DS_UNITS_1_2_7    = :QDS-UNITS-1-2-7,
      *        DSFILE.DS_OUTBAL_1_2_7   = :QDS-OUTBAL-1-2-7,
      *        DSFILE.DS_UNITS_1_2_8    = :QDS-UNITS-1-2-8,
      *        DSFILE.DS_OUTBAL_1_2_8   = :QDS-OUTBAL-1-2-8,
      *        DSFILE.DS_UNITS_1_2_9    = :QDS-UNITS-1-2-9,
      *        DSFILE.DS_OUTBAL_1_2_9   = :QDS-OUTBAL-1-2-9,
      *        DSFILE.DS_UNITS_1_2_10   = :QDS-UNITS-1-2-10,
      *        DSFILE.DS_OUTBAL_1_2_10  = :QDS-OUTBAL-1-2-10,
      *        DSFILE.DS_UNITS_2_1_1    = :QDS-UNITS-2-1-1,
      *        DSFILE.DS_OUTBAL_2_1_1   = :QDS-OUTBAL-2-1-1,
      *        DSFILE.DS_UNITS_2_1_2    = :QDS-UNITS-2-1-2,
      *        DSFILE.DS_OUTBAL_2_1_2   = :QDS-OUTBAL-2-1-2,
      *        DSFILE.DS_UNITS_2_1_3    = :QDS-UNITS-2-1-3,
      *        DSFILE.DS_OUTBAL_2_1_3   = :QDS-OUTBAL-2-1-3,
      *        DSFILE.DS_UNITS_2_1_4    = :QDS-UNITS-2-1-4,
      *        DSFILE.DS_OUTBAL_2_1_4   = :QDS-OUTBAL-2-1-4,
      *        DSFILE.DS_UNITS_2_1_5    = :QDS-UNITS-2-1-5,
      *        DSFILE.DS_OUTBAL_2_1_5   = :QDS-OUTBAL-2-1-5,
      *        DSFILE.DS_UNITS_2_1_6    = :QDS-UNITS-2-1-6,
      *        DSFILE.DS_OUTBAL_2_1_6   = :QDS-OUTBAL-2-1-6,
      *        DSFILE.DS_UNITS_2_1_7    = :QDS-UNITS-2-1-7,
      *        DSFILE.DS_OUTBAL_2_1_7   = :QDS-OUTBAL-2-1-7,
      *        DSFILE.DS_UNITS_2_1_8    = :QDS-UNITS-2-1-8,
      *        DSFILE.DS_OUTBAL_2_1_8   = :QDS-OUTBAL-2-1-8,
      *        DSFILE.DS_UNITS_2_1_9    = :QDS-UNITS-2-1-9,
      *        DSFILE.DS_OUTBAL_2_1_9   = :QDS-OUTBAL-2-1-9,
      *        DSFILE.DS_UNITS_2_1_10   = :QDS-UNITS-2-1-10,
      *        DSFILE.DS_OUTBAL_2_1_10  = :QDS-OUTBAL-2-1-10,
      *        DSFILE.DS_UNITS_2_2_1    = :QDS-UNITS-2-2-1,
      *        DSFILE.DS_OUTBAL_2_2_1   = :QDS-OUTBAL-2-2-1,
      *        DSFILE.DS_UNITS_2_2_2    = :QDS-UNITS-2-2-2,
      *        DSFILE.DS_OUTBAL_2_2_2   = :QDS-OUTBAL-2-2-2,
      *        DSFILE.DS_UNITS_2_2_3    = :QDS-UNITS-2-2-3,
      *        DSFILE.DS_OUTBAL_2_2_3   = :QDS-OUTBAL-2-2-3,
      *        DSFILE.DS_UNITS_2_2_4    = :QDS-UNITS-2-2-4,
      *        DSFILE.DS_OUTBAL_2_2_4   = :QDS-OUTBAL-2-2-4,
      *        DSFILE.DS_UNITS_2_2_5    = :QDS-UNITS-2-2-5,
      *        DSFILE.DS_OUTBAL_2_2_5   = :QDS-OUTBAL-2-2-5,
      *        DSFILE.DS_UNITS_2_2_6    = :QDS-UNITS-2-2-6,
      *        DSFILE.DS_OUTBAL_2_2_6   = :QDS-OUTBAL-2-2-6,
      *        DSFILE.DS_UNITS_2_2_7    = :QDS-UNITS-2-2-7,
      *        DSFILE.DS_OUTBAL_2_2_7   = :QDS-OUTBAL-2-2-7,
      *        DSFILE.DS_UNITS_2_2_8    = :QDS-UNITS-2-2-8,
      *        DSFILE.DS_OUTBAL_2_2_8   = :QDS-OUTBAL-2-2-8,
      *        DSFILE.DS_UNITS_2_2_9    = :QDS-UNITS-2-2-9,
      *        DSFILE.DS_OUTBAL_2_2_9   = :QDS-OUTBAL-2-2-9,
      *        DSFILE.DS_UNITS_2_2_10   = :QDS-UNITS-2-2-10,
      *        DSFILE.DS_OUTBAL_2_2_10  = :QDS-OUTBAL-2-2-10,
      *        DSFILE.DS_UNITS_3_1_1    = :QDS-UNITS-3-1-1,
      *        DSFILE.DS_OUTBAL_3_1_1   = :QDS-OUTBAL-3-1-1,
      *        DSFILE.DS_UNITS_3_1_2    = :QDS-UNITS-3-1-2,
      *        DSFILE.DS_OUTBAL_3_1_2   = :QDS-OUTBAL-3-1-2,
      *        DSFILE.DS_UNITS_3_1_3    = :QDS-UNITS-3-1-3,
      *        DSFILE.DS_OUTBAL_3_1_3   = :QDS-OUTBAL-3-1-3,
      *        DSFILE.DS_UNITS_3_1_4    = :QDS-UNITS-3-1-4,
      *        DSFILE.DS_OUTBAL_3_1_4   = :QDS-OUTBAL-3-1-4,
      *        DSFILE.DS_UNITS_3_1_5    = :QDS-UNITS-3-1-5,
      *        DSFILE.DS_OUTBAL_3_1_5   = :QDS-OUTBAL-3-1-5,
      *        DSFILE.DS_UNITS_3_1_6    = :QDS-UNITS-3-1-6,
      *        DSFILE.DS_OUTBAL_3_1_6   = :QDS-OUTBAL-3-1-6,
      *        DSFILE.DS_UNITS_3_1_7    = :QDS-UNITS-3-1-7,
      *        DSFILE.DS_OUTBAL_3_1_7   = :QDS-OUTBAL-3-1-7,
      *        DSFILE.DS_UNITS_3_1_8    = :QDS-UNITS-3-1-8,
      *        DSFILE.DS_OUTBAL_3_1_8   = :QDS-OUTBAL-3-1-8,
      *        DSFILE.DS_UNITS_3_1_9    = :QDS-UNITS-3-1-9,
      *        DSFILE.DS_OUTBAL_3_1_9   = :QDS-OUTBAL-3-1-9,
      *        DSFILE.DS_UNITS_3_1_10   = :QDS-UNITS-3-1-10,
      *        DSFILE.DS_OUTBAL_3_1_10  = :QDS-OUTBAL-3-1-10,
      *        DSFILE.DS_UNITS_3_2_1    = :QDS-UNITS-3-2-1,
      *        DSFILE.DS_OUTBAL_3_2_1   = :QDS-OUTBAL-3-2-1,
      *        DSFILE.DS_UNITS_3_2_2    = :QDS-UNITS-3-2-2,
      *        DSFILE.DS_OUTBAL_3_2_2   = :QDS-OUTBAL-3-2-2,
      *        DSFILE.DS_UNITS_3_2_3    = :QDS-UNITS-3-2-3,
      *        DSFILE.DS_OUTBAL_3_2_3   = :QDS-OUTBAL-3-2-3,
      *        DSFILE.DS_UNITS_3_2_4    = :QDS-UNITS-3-2-4,
      *        DSFILE.DS_OUTBAL_3_2_4   = :QDS-OUTBAL-3-2-4,
      *        DSFILE.DS_UNITS_3_2_5    = :QDS-UNITS-3-2-5,
      *        DSFILE.DS_OUTBAL_3_2_5   = :QDS-OUTBAL-3-2-5,
      *        DSFILE.DS_UNITS_3_2_6    = :QDS-UNITS-3-2-6,
      *        DSFILE.DS_OUTBAL_3_2_6   = :QDS-OUTBAL-3-2-6,
      *        DSFILE.DS_UNITS_3_2_7    = :QDS-UNITS-3-2-7,
      *        DSFILE.DS_OUTBAL_3_2_7   = :QDS-OUTBAL-3-2-7,
      *        DSFILE.DS_UNITS_3_2_8    = :QDS-UNITS-3-2-8,
      *        DSFILE.DS_OUTBAL_3_2_8   = :QDS-OUTBAL-3-2-8,
      *        DSFILE.DS_UNITS_3_2_9    = :QDS-UNITS-3-2-9,
      *        DSFILE.DS_OUTBAL_3_2_9   = :QDS-OUTBAL-3-2-9,
      *        DSFILE.DS_UNITS_3_2_10   = :QDS-UNITS-3-2-10,
      *        DSFILE.DS_OUTBAL_3_2_10  = :QDS-OUTBAL-3-2-10,
      *        DSFILE.DS_UNITS_BNKRPT_1_1_1 =
      *             :QDS-UNITS-BNKRPT-1-1-1,
      *        DSFILE.DS_OUTBAL_BNKRPT_1_1_1 =
      *             :QDS-OUTBAL-BNKRPT-1-1-1,
      *        DSFILE.DS_UNITS_BNKRPT_1_1_2 =
      *             :QDS-UNITS-BNKRPT-1-1-2,
      *        DSFILE.DS_OUTBAL_BNKRPT_1_1_2 =
      *             :QDS-OUTBAL-BNKRPT-1-1-2,
      *        DSFILE.DS_UNITS_BNKRPT_1_1_3 =
      *             :QDS-UNITS-BNKRPT-1-1-3,
      *        DSFILE.DS_OUTBAL_BNKRPT_1_1_3 =
      *             :QDS-OUTBAL-BNKRPT-1-1-3,
      *        DSFILE.DS_UNITS_BNKRPT_1_1_4 =
      *             :QDS-UNITS-BNKRPT-1-1-4,
      *        DSFILE.DS_OUTBAL_BNKRPT_1_1_4 =
      *             :QDS-OUTBAL-BNKRPT-1-1-4,
      *        DSFILE.DS_UNITS_BNKRPT_1_1_5 =
      *             :QDS-UNITS-BNKRPT-1-1-5,
      *        DSFILE.DS_OUTBAL_BNKRPT_1_1_5 =
      *             :QDS-OUTBAL-BNKRPT-1-1-5,
      *        DSFILE.DS_UNITS_BNKRPT_1_1_6 =
      *             :QDS-UNITS-BNKRPT-1-1-6,
      *        DSFILE.DS_OUTBAL_BNKRPT_1_1_6 =
      *             :QDS-OUTBAL-BNKRPT-1-1-6,
      *        DSFILE.DS_UNITS_BNKRPT_1_1_7 =
      *             :QDS-UNITS-BNKRPT-1-1-7,
      *        DSFILE.DS_OUTBAL_BNKRPT_1_1_7 =
      *             :QDS-OUTBAL-BNKRPT-1-1-7,
      *        DSFILE.DS_UNITS_BNKRPT_1_1_8 =
      *             :QDS-UNITS-BNKRPT-1-1-8,
      *        DSFILE.DS_OUTBAL_BNKRPT_1_1_8 =
      *             :QDS-OUTBAL-BNKRPT-1-1-8,
      *        DSFILE.DS_UNITS_BNKRPT_1_1_9 =
      *             :QDS-UNITS-BNKRPT-1-1-9,
      *        DSFILE.DS_OUTBAL_BNKRPT_1_1_9 =
      *             :QDS-OUTBAL-BNKRPT-1-1-9,
      *        DSFILE.DS_UNITS_BNKRPT_1_1_10 =
      *             :QDS-UNITS-BNKRPT-1-1-10,
      *        DSFILE.DS_OUTBAL_BNKRPT_1_1_10 =
      *             :QDS-OUTBAL-BNKRPT-1-1-10,
      *        DSFILE.DS_UNITS_BNKRPT_1_2_1 =
      *             :QDS-UNITS-BNKRPT-1-2-1,
      *        DSFILE.DS_OUTBAL_BNKRPT_1_2_1 =
      *             :QDS-OUTBAL-BNKRPT-1-2-1,
      *        DSFILE.DS_UNITS_BNKRPT_1_2_2 =
      *             :QDS-UNITS-BNKRPT-1-2-2,
      *        DSFILE.DS_OUTBAL_BNKRPT_1_2_2 =
      *             :QDS-OUTBAL-BNKRPT-1-2-2,
      *        DSFILE.DS_UNITS_BNKRPT_1_2_3 =
      *             :QDS-UNITS-BNKRPT-1-2-3,
      *        DSFILE.DS_OUTBAL_BNKRPT_1_2_3 =
      *             :QDS-OUTBAL-BNKRPT-1-2-3,
      *        DSFILE.DS_UNITS_BNKRPT_1_2_4 =
      *             :QDS-UNITS-BNKRPT-1-2-4,
      *        DSFILE.DS_OUTBAL_BNKRPT_1_2_4 =
      *             :QDS-OUTBAL-BNKRPT-1-2-4,
      *        DSFILE.DS_UNITS_BNKRPT_1_2_5 =
      *             :QDS-UNITS-BNKRPT-1-2-5,
      *        DSFILE.DS_OUTBAL_BNKRPT_1_2_5 =
      *             :QDS-OUTBAL-BNKRPT-1-2-5,
      *        DSFILE.DS_UNITS_BNKRPT_1_2_6 =
      *             :QDS-UNITS-BNKRPT-1-2-6,
      *        DSFILE.DS_OUTBAL_BNKRPT_1_2_6 =
      *             :QDS-OUTBAL-BNKRPT-1-2-6,
      *        DSFILE.DS_UNITS_BNKRPT_1_2_7 =
      *             :QDS-UNITS-BNKRPT-1-2-7,
      *        DSFILE.DS_OUTBAL_BNKRPT_1_2_7 =
      *             :QDS-OUTBAL-BNKRPT-1-2-7,
      *        DSFILE.DS_UNITS_BNKRPT_1_2_8 =
      *             :QDS-UNITS-BNKRPT-1-2-8,
      *        DSFILE.DS_OUTBAL_BNKRPT_1_2_8 =
      *             :QDS-OUTBAL-BNKRPT-1-2-8,
      *        DSFILE.DS_UNITS_BNKRPT_1_2_9 =
      *             :QDS-UNITS-BNKRPT-1-2-9,
      *        DSFILE.DS_OUTBAL_BNKRPT_1_2_9 =
      *             :QDS-OUTBAL-BNKRPT-1-2-9,
      *        DSFILE.DS_UNITS_BNKRPT_1_2_10 =
      *             :QDS-UNITS-BNKRPT-1-2-10,
      *        DSFILE.DS_OUTBAL_BNKRPT_1_2_10 =
      *             :QDS-OUTBAL-BNKRPT-1-2-10,
      *        DSFILE.DS_UNITS_BNKRPT_2_1_1 =
      *             :QDS-UNITS-BNKRPT-2-1-1,
      *        DSFILE.DS_OUTBAL_BNKRPT_2_1_1 =
      *             :QDS-OUTBAL-BNKRPT-2-1-1,
      *        DSFILE.DS_UNITS_BNKRPT_2_1_2 =
      *             :QDS-UNITS-BNKRPT-2-1-2,
      *        DSFILE.DS_OUTBAL_BNKRPT_2_1_2 =
      *             :QDS-OUTBAL-BNKRPT-2-1-2,
      *        DSFILE.DS_UNITS_BNKRPT_2_1_3 =
      *             :QDS-UNITS-BNKRPT-2-1-3,
      *        DSFILE.DS_OUTBAL_BNKRPT_2_1_3 =
      *             :QDS-OUTBAL-BNKRPT-2-1-3,
      *        DSFILE.DS_UNITS_BNKRPT_2_1_4 =
      *             :QDS-UNITS-BNKRPT-2-1-4,
      *        DSFILE.DS_OUTBAL_BNKRPT_2_1_4 =
      *             :QDS-OUTBAL-BNKRPT-2-1-4,
      *        DSFILE.DS_UNITS_BNKRPT_2_1_5 =
      *             :QDS-UNITS-BNKRPT-2-1-5,
      *        DSFILE.DS_OUTBAL_BNKRPT_2_1_5 =
      *             :QDS-OUTBAL-BNKRPT-2-1-5,
      *        DSFILE.DS_UNITS_BNKRPT_2_1_6 =
      *             :QDS-UNITS-BNKRPT-2-1-6,
      *        DSFILE.DS_OUTBAL_BNKRPT_2_1_6 =
      *             :QDS-OUTBAL-BNKRPT-2-1-6,
      *        DSFILE.DS_UNITS_BNKRPT_2_1_7 =
      *             :QDS-UNITS-BNKRPT-2-1-7,
      *        DSFILE.DS_OUTBAL_BNKRPT_2_1_7 =
      *             :QDS-OUTBAL-BNKRPT-2-1-7,
      *        DSFILE.DS_UNITS_BNKRPT_2_1_8 =
      *             :QDS-UNITS-BNKRPT-2-1-8,
      *        DSFILE.DS_OUTBAL_BNKRPT_2_1_8 =
      *             :QDS-OUTBAL-BNKRPT-2-1-8,
      *        DSFILE.DS_UNITS_BNKRPT_2_1_9 =
      *             :QDS-UNITS-BNKRPT-2-1-9,
      *        DSFILE.DS_OUTBAL_BNKRPT_2_1_9 =
      *             :QDS-OUTBAL-BNKRPT-2-1-9,
      *        DSFILE.DS_UNITS_BNKRPT_2_1_10 =
      *             :QDS-UNITS-BNKRPT-2-1-10,
      *        DSFILE.DS_OUTBAL_BNKRPT_2_1_10 =
      *             :QDS-OUTBAL-BNKRPT-2-1-10,
      *        DSFILE.DS_UNITS_BNKRPT_2_2_1 =
      *             :QDS-UNITS-BNKRPT-2-2-1,
      *        DSFILE.DS_OUTBAL_BNKRPT_2_2_1 =
      *             :QDS-OUTBAL-BNKRPT-2-2-1,
      *        DSFILE.DS_UNITS_BNKRPT_2_2_2 =
      *             :QDS-UNITS-BNKRPT-2-2-2,
      *        DSFILE.DS_OUTBAL_BNKRPT_2_2_2 =
      *             :QDS-OUTBAL-BNKRPT-2-2-2,
      *        DSFILE.DS_UNITS_BNKRPT_2_2_3 =
      *             :QDS-UNITS-BNKRPT-2-2-3,
      *        DSFILE.DS_OUTBAL_BNKRPT_2_2_3 =
      *             :QDS-OUTBAL-BNKRPT-2-2-3,
      *        DSFILE.DS_UNITS_BNKRPT_2_2_4 =
      *             :QDS-UNITS-BNKRPT-2-2-4,
      *        DSFILE.DS_OUTBAL_BNKRPT_2_2_4 =
      *             :QDS-OUTBAL-BNKRPT-2-2-4,
      *        DSFILE.DS_UNITS_BNKRPT_2_2_5 =
      *             :QDS-UNITS-BNKRPT-2-2-5,
      *        DSFILE.DS_OUTBAL_BNKRPT_2_2_5 =
      *             :QDS-OUTBAL-BNKRPT-2-2-5,
      *        DSFILE.DS_UNITS_BNKRPT_2_2_6 =
      *             :QDS-UNITS-BNKRPT-2-2-6,
      *        DSFILE.DS_OUTBAL_BNKRPT_2_2_6 =
      *             :QDS-OUTBAL-BNKRPT-2-2-6,
      *        DSFILE.DS_UNITS_BNKRPT_2_2_7 =
      *             :QDS-UNITS-BNKRPT-2-2-7,
      *        DSFILE.DS_OUTBAL_BNKRPT_2_2_7 =
      *             :QDS-OUTBAL-BNKRPT-2-2-7,
      *        DSFILE.DS_UNITS_BNKRPT_2_2_8 =
      *             :QDS-UNITS-BNKRPT-2-2-8,
      *        DSFILE.DS_OUTBAL_BNKRPT_2_2_8 =
      *             :QDS-OUTBAL-BNKRPT-2-2-8,
      *        DSFILE.DS_UNITS_BNKRPT_2_2_9 =
      *             :QDS-UNITS-BNKRPT-2-2-9,
      *        DSFILE.DS_OUTBAL_BNKRPT_2_2_9 =
      *             :QDS-OUTBAL-BNKRPT-2-2-9,
      *        DSFILE.DS_UNITS_BNKRPT_2_2_10 =
      *             :QDS-UNITS-BNKRPT-2-2-10,
      *        DSFILE.DS_OUTBAL_BNKRPT_2_2_10 =
      *             :QDS-OUTBAL-BNKRPT-2-2-10,
      *        DSFILE.DS_UNITS_BNKRPT_3_1_1 =
      *             :QDS-UNITS-BNKRPT-3-1-1,
      *        DSFILE.DS_OUTBAL_BNKRPT_3_1_1 =
      *             :QDS-OUTBAL-BNKRPT-3-1-1,
      *        DSFILE.DS_UNITS_BNKRPT_3_1_2 =
      *             :QDS-UNITS-BNKRPT-3-1-2,
      *        DSFILE.DS_OUTBAL_BNKRPT_3_1_2 =
      *             :QDS-OUTBAL-BNKRPT-3-1-2,
      *        DSFILE.DS_UNITS_BNKRPT_3_1_3 =
      *             :QDS-UNITS-BNKRPT-3-1-3,
      *        DSFILE.DS_OUTBAL_BNKRPT_3_1_3 =
      *             :QDS-OUTBAL-BNKRPT-3-1-3,
      *        DSFILE.DS_UNITS_BNKRPT_3_1_4 =
      *             :QDS-UNITS-BNKRPT-3-1-4,
      *        DSFILE.DS_OUTBAL_BNKRPT_3_1_4 =
      *             :QDS-OUTBAL-BNKRPT-3-1-4,
      *        DSFILE.DS_UNITS_BNKRPT_3_1_5 =
      *             :QDS-UNITS-BNKRPT-3-1-5,
      *        DSFILE.DS_OUTBAL_BNKRPT_3_1_5 =
      *             :QDS-OUTBAL-BNKRPT-3-1-5,
      *        DSFILE.DS_UNITS_BNKRPT_3_1_6 =
      *             :QDS-UNITS-BNKRPT-3-1-6,
      *        DSFILE.DS_OUTBAL_BNKRPT_3_1_6 =
      *             :QDS-OUTBAL-BNKRPT-3-1-6,
      *        DSFILE.DS_UNITS_BNKRPT_3_1_7 =
      *             :QDS-UNITS-BNKRPT-3-1-7,
      *        DSFILE.DS_OUTBAL_BNKRPT_3_1_7 =
      *             :QDS-OUTBAL-BNKRPT-3-1-7,
      *        DSFILE.DS_UNITS_BNKRPT_3_1_8 =
      *             :QDS-UNITS-BNKRPT-3-1-8,
      *        DSFILE.DS_OUTBAL_BNKRPT_3_1_8 =
      *             :QDS-OUTBAL-BNKRPT-3-1-8,
      *        DSFILE.DS_UNITS_BNKRPT_3_1_9 =
      *             :QDS-UNITS-BNKRPT-3-1-9,
      *        DSFILE.DS_OUTBAL_BNKRPT_3_1_9 =
      *             :QDS-OUTBAL-BNKRPT-3-1-9,
      *        DSFILE.DS_UNITS_BNKRPT_3_1_10 =
      *             :QDS-UNITS-BNKRPT-3-1-10,
      *        DSFILE.DS_OUTBAL_BNKRPT_3_1_10 =
      *             :QDS-OUTBAL-BNKRPT-3-1-10,
      *        DSFILE.DS_UNITS_BNKRPT_3_2_1 =
      *             :QDS-UNITS-BNKRPT-3-2-1,
      *        DSFILE.DS_OUTBAL_BNKRPT_3_2_1 =
      *             :QDS-OUTBAL-BNKRPT-3-2-1,
      *        DSFILE.DS_UNITS_BNKRPT_3_2_2 =
      *             :QDS-UNITS-BNKRPT-3-2-2,
      *        DSFILE.DS_OUTBAL_BNKRPT_3_2_2 =
      *             :QDS-OUTBAL-BNKRPT-3-2-2,
      *        DSFILE.DS_UNITS_BNKRPT_3_2_3 =
      *             :QDS-UNITS-BNKRPT-3-2-3,
      *        DSFILE.DS_OUTBAL_BNKRPT_3_2_3 =
      *             :QDS-OUTBAL-BNKRPT-3-2-3,
      *        DSFILE.DS_UNITS_BNKRPT_3_2_4 =
      *             :QDS-UNITS-BNKRPT-3-2-4,
      *        DSFILE.DS_OUTBAL_BNKRPT_3_2_4 =
      *             :QDS-OUTBAL-BNKRPT-3-2-4,
      *        DSFILE.DS_UNITS_BNKRPT_3_2_5 =
      *             :QDS-UNITS-BNKRPT-3-2-5,
      *        DSFILE.DS_OUTBAL_BNKRPT_3_2_5 =
      *             :QDS-OUTBAL-BNKRPT-3-2-5,
      *        DSFILE.DS_UNITS_BNKRPT_3_2_6 =
      *             :QDS-UNITS-BNKRPT-3-2-6,
      *        DSFILE.DS_OUTBAL_BNKRPT_3_2_6 =
      *             :QDS-OUTBAL-BNKRPT-3-2-6,
      *        DSFILE.DS_UNITS_BNKRPT_3_2_7 =
      *             :QDS-UNITS-BNKRPT-3-2-7,
      *        DSFILE.DS_OUTBAL_BNKRPT_3_2_7 =
      *             :QDS-OUTBAL-BNKRPT-3-2-7,
      *        DSFILE.DS_UNITS_BNKRPT_3_2_8 =
      *             :QDS-UNITS-BNKRPT-3-2-8,
      *        DSFILE.DS_OUTBAL_BNKRPT_3_2_8 =
      *             :QDS-OUTBAL-BNKRPT-3-2-8,
      *        DSFILE.DS_UNITS_BNKRPT_3_2_9 =
      *             :QDS-UNITS-BNKRPT-3-2-9,
      *        DSFILE.DS_OUTBAL_BNKRPT_3_2_9 =
      *             :QDS-OUTBAL-BNKRPT-3-2-9,
      *        DSFILE.DS_UNITS_BNKRPT_3_2_10 =
      *             :QDS-UNITS-BNKRPT-3-2-10,
      *        DSFILE.DS_OUTBAL_BNKRPT_3_2_10 =
      *             :QDS-OUTBAL-BNKRPT-3-2-10,
      *        DSFILE.DS_UNEARNED_AMT_1_1_1 =
      *             :QDS-UNEARNED-AMT-1-1-1,
      *        DSFILE.DS_UNEARNED_AMT_1_1_2 =
      *             :QDS-UNEARNED-AMT-1-1-2,
      *        DSFILE.DS_UNEARNED_AMT_1_1_3 =
      *             :QDS-UNEARNED-AMT-1-1-3,
      *        DSFILE.DS_UNEARNED_AMT_1_1_4 =
      *             :QDS-UNEARNED-AMT-1-1-4,
      *        DSFILE.DS_UNEARNED_AMT_1_1_5 =
      *             :QDS-UNEARNED-AMT-1-1-5,
      *        DSFILE.DS_UNEARNED_AMT_1_1_6 =
      *             :QDS-UNEARNED-AMT-1-1-6,
      *        DSFILE.DS_UNEARNED_AMT_1_1_7 =
      *             :QDS-UNEARNED-AMT-1-1-7,
      *        DSFILE.DS_UNEARNED_AMT_1_1_8 =
      *             :QDS-UNEARNED-AMT-1-1-8,
      *        DSFILE.DS_UNEARNED_AMT_1_1_9 =
      *             :QDS-UNEARNED-AMT-1-1-9,
      *        DSFILE.DS_UNEARNED_AMT_1_1_10 =
      *             :QDS-UNEARNED-AMT-1-1-10,
      *        DSFILE.DS_UNEARNED_AMT_1_2_1 =
      *             :QDS-UNEARNED-AMT-1-2-1,
      *        DSFILE.DS_UNEARNED_AMT_1_2_2 =
      *             :QDS-UNEARNED-AMT-1-2-2,
      *        DSFILE.DS_UNEARNED_AMT_1_2_3 =
      *             :QDS-UNEARNED-AMT-1-2-3,
      *        DSFILE.DS_UNEARNED_AMT_1_2_4 =
      *             :QDS-UNEARNED-AMT-1-2-4,
      *        DSFILE.DS_UNEARNED_AMT_1_2_5 =
      *             :QDS-UNEARNED-AMT-1-2-5,
      *        DSFILE.DS_UNEARNED_AMT_1_2_6 =
      *             :QDS-UNEARNED-AMT-1-2-6,
      *        DSFILE.DS_UNEARNED_AMT_1_2_7 =
      *             :QDS-UNEARNED-AMT-1-2-7,
      *        DSFILE.DS_UNEARNED_AMT_1_2_8 =
      *             :QDS-UNEARNED-AMT-1-2-8,
      *        DSFILE.DS_UNEARNED_AMT_1_2_9 =
      *             :QDS-UNEARNED-AMT-1-2-9,
      *        DSFILE.DS_UNEARNED_AMT_1_2_10 =
      *             :QDS-UNEARNED-AMT-1-2-10,
      *        DSFILE.DS_UNEARNED_AMT_2_1_1 =
      *             :QDS-UNEARNED-AMT-2-1-1,
      *        DSFILE.DS_UNEARNED_AMT_2_1_2 =
      *             :QDS-UNEARNED-AMT-2-1-2,
      *        DSFILE.DS_UNEARNED_AMT_2_1_3 =
      *             :QDS-UNEARNED-AMT-2-1-3,
      *        DSFILE.DS_UNEARNED_AMT_2_1_4 =
      *             :QDS-UNEARNED-AMT-2-1-4,
      *        DSFILE.DS_UNEARNED_AMT_2_1_5 =
      *             :QDS-UNEARNED-AMT-2-1-5,
      *        DSFILE.DS_UNEARNED_AMT_2_1_6 =
      *             :QDS-UNEARNED-AMT-2-1-6,
      *        DSFILE.DS_UNEARNED_AMT_2_1_7 =
      *             :QDS-UNEARNED-AMT-2-1-7,
      *        DSFILE.DS_UNEARNED_AMT_2_1_8 =
      *             :QDS-UNEARNED-AMT-2-1-8,
      *        DSFILE.DS_UNEARNED_AMT_2_1_9 =
      *             :QDS-UNEARNED-AMT-2-1-9,
      *        DSFILE.DS_UNEARNED_AMT_2_1_10 =
      *             :QDS-UNEARNED-AMT-2-1-10,
      *        DSFILE.DS_UNEARNED_AMT_2_2_1 =
      *             :QDS-UNEARNED-AMT-2-2-1,
      *        DSFILE.DS_UNEARNED_AMT_2_2_2 =
      *             :QDS-UNEARNED-AMT-2-2-2,
      *        DSFILE.DS_UNEARNED_AMT_2_2_3 =
      *             :QDS-UNEARNED-AMT-2-2-3,
      *        DSFILE.DS_UNEARNED_AMT_2_2_4 =
      *             :QDS-UNEARNED-AMT-2-2-4,
      *        DSFILE.DS_UNEARNED_AMT_2_2_5 =
      *             :QDS-UNEARNED-AMT-2-2-5,
      *        DSFILE.DS_UNEARNED_AMT_2_2_6 =
      *             :QDS-UNEARNED-AMT-2-2-6,
      *        DSFILE.DS_UNEARNED_AMT_2_2_7 =
      *             :QDS-UNEARNED-AMT-2-2-7,
      *        DSFILE.DS_UNEARNED_AMT_2_2_8 =
      *             :QDS-UNEARNED-AMT-2-2-8,
      *        DSFILE.DS_UNEARNED_AMT_2_2_9 =
      *             :QDS-UNEARNED-AMT-2-2-9,
      *        DSFILE.DS_UNEARNED_AMT_2_2_10 =
      *             :QDS-UNEARNED-AMT-2-2-10,
      *        DSFILE.DS_UNEARNED_AMT_3_1_1 =
      *             :QDS-UNEARNED-AMT-3-1-1,
      *        DSFILE.DS_UNEARNED_AMT_3_1_2 =
      *             :QDS-UNEARNED-AMT-3-1-2,
      *        DSFILE.DS_UNEARNED_AMT_3_1_3 =
      *             :QDS-UNEARNED-AMT-3-1-3,
      *        DSFILE.DS_UNEARNED_AMT_3_1_4 =
      *             :QDS-UNEARNED-AMT-3-1-4,
      *        DSFILE.DS_UNEARNED_AMT_3_1_5 =
      *             :QDS-UNEARNED-AMT-3-1-5,
      *        DSFILE.DS_UNEARNED_AMT_3_1_6 =
      *             :QDS-UNEARNED-AMT-3-1-6,
      *        DSFILE.DS_UNEARNED_AMT_3_1_7 =
      *             :QDS-UNEARNED-AMT-3-1-7,
      *        DSFILE.DS_UNEARNED_AMT_3_1_8 =
      *             :QDS-UNEARNED-AMT-3-1-8,
      *        DSFILE.DS_UNEARNED_AMT_3_1_9 =
      *             :QDS-UNEARNED-AMT-3-1-9,
      *        DSFILE.DS_UNEARNED_AMT_3_1_10 =
      *             :QDS-UNEARNED-AMT-3-1-10,
      *        DSFILE.DS_UNEARNED_AMT_3_2_1 =
      *             :QDS-UNEARNED-AMT-3-2-1,
      *        DSFILE.DS_UNEARNED_AMT_3_2_2 =
      *             :QDS-UNEARNED-AMT-3-2-2,
      *        DSFILE.DS_UNEARNED_AMT_3_2_3 =
      *             :QDS-UNEARNED-AMT-3-2-3,
      *        DSFILE.DS_UNEARNED_AMT_3_2_4 =
      *             :QDS-UNEARNED-AMT-3-2-4,
      *        DSFILE.DS_UNEARNED_AMT_3_2_5 =
      *             :QDS-UNEARNED-AMT-3-2-5,
      *        DSFILE.DS_UNEARNED_AMT_3_2_6 =
      *             :QDS-UNEARNED-AMT-3-2-6,
      *        DSFILE.DS_UNEARNED_AMT_3_2_7 =
      *             :QDS-UNEARNED-AMT-3-2-7,
      *        DSFILE.DS_UNEARNED_AMT_3_2_8 =
      *             :QDS-UNEARNED-AMT-3-2-8,
      *        DSFILE.DS_UNEARNED_AMT_3_2_9 =
      *             :QDS-UNEARNED-AMT-3-2-9,
      *        DSFILE.DS_UNEARNED_AMT_3_2_10 =
      *             :QDS-UNEARNED-AMT-3-2-10,
      *        DSFILE.DS_DLDISC_VOL_1  = :QDS-DLDISC-VOL-1,
      *        DSFILE.DS_DLDISC_VOL_2  = :QDS-DLDISC-VOL-2,
      *        DSFILE.DS_DLDISC_VOL_3  = :QDS-DLDISC-VOL-3,
      *        DSFILE.DS_AFPDUEDL_MEMO = :QDS-AFPDUEDL-MEMO,
      *        DSFILE.DS_UNPAID_MANUAL = :QDS-UNPAID-MANUAL,
      *        DSFILE.DS_LTOUCH_DATE   = :QDS-LTOUCH-DATE
      *       WHERE DSFILE.DS_DATE_CCYYMM = :QDS-DATE-CCYYMM
      *         AND DSFILE.DS_BRANCH = :QDS-BRANCH
      *         AND DSFILE.DS_NUM = :QDS-NUM
      *         AND DSFILE.DS_CLASS = :QDS-CLASS
      *     END-EXEC.

      *     PERFORM SQL-IO-VALIDATION.

      *     MOVE '1' TO SQL-UPDATE-CONN-USE-SW.    *> CHANGE CONNECTION BACK TO C1
      *     PERFORM SQL-SWAP-UPDATE-CONNECTION.

      *     IF ( IO-FG = 8 )     *> LOCKED
      *        GO TO REWRITE-DS1-FILE.

      **    IF ( IO-FG = 0 )
      **       EXEC SQL COMMIT END-EXEC
      **       PERFORM SQL-IO-VALIDATION.

      **----------------------------------------------------------------
      * CLOSE-DS1-FILE.

      *     PERFORM CLOSE-IT.

      *     IF ( DS1-CURSOR-STAT-GOOD )
      *        EXEC SQL
      *             CLOSE DS1_CURSOR
      *        END-EXEC
      *        MOVE STAT---BAD TO DS1-CURSOR-STAT.


      * COPY "LIBLP/LPLN1IN.CPY".
      * COPYMEMBER: LIBLP/LPLN1IN
      ************************************************************
      *                (LPLN1IN)
      * REV:
      *  JTG 081497 CHANGED LN-SALEFIN TO LN-GIFTNET WORLD A90
      *  BLV 020698 ADDED LN-SALEFIN BACK TO BICREA CALL
      *  MJD 030398 ADDED LN-PLCD AND LN-CONVERCD TO BICREA CALL
      *  BAH 121023 ADDED FILPATH
      *  BAH 04/22/2016 ADDED LN-SMALLOAN TO BICREA CALL, REGENCY PR#5030
      *  BAH 20180316 ADDED LN-PLDATE TO BICREA CALL, WORLD #1308
      *  BAH 181130 GLOBAL LNFILE
      ************************************************************
       UPDATE-BI-FILE.
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
       START-LN3-FILE.  *> SSP - LN 
           PERFORM START-IT.
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
      *            LNFILE.LN_ORGST,
      *            LNFILE.LN_SPRCLASS,
      *            LNFILE.LN_SRCD,
      *            LNFILE.LN_POCD,
      *            LNFILE.LN_PLCD,
      *            LNFILE.LN_PLTYPE,
      *            LNFILE.LN_SERVACCT,
      *            LNFILE.LN_PAY_SCHLD_FG,
      *            CAST(LNFILE.LN_LOANDATE       AS VARCHAR(10)) AS LN_LOANDATE,
      *            CAST(LNFILE.LN_ORIG_1STPYDATE AS VARCHAR(10)) AS LN_ORIG_1STPYDATE,
      *            CAST(LNFILE.LN_1STPYDATE      AS VARCHAR(10)) AS LN_1STPYDATE,
      *            CAST(LNFILE.LN_PLDATE         AS VARCHAR(10)) AS LN_PLDATE,
      *            CAST(LNFILE.LN_POFFDATE       AS VARCHAR(10)) AS LN_POFFDATE,
      *            CAST(LNFILE.LN_BNKRPTDATE     AS VARCHAR(10)) AS LN_BNKRPTDATE,
      *            CAST(LNFILE.LN_REPODATE       AS VARCHAR(10)) AS LN_REPODATE,
      *            CAST(LNFILE.LN_REDEEMDATE     AS VARCHAR(10)) AS LN_REDEEMDATE,
      *            CAST(LNFILE.LN_TRW_DOFD_DATE  AS VARCHAR(10)) AS LN_TRW_DOFD_DATE,
      *            LNFILE.LN_LNAMT,
      *            LNFILE.LN_ORGTERM,
      *            LNFILE.LN_1STPYAMT,
      *            LNFILE.LN_NOREGPY,
      *            LNFILE.LN_REGPYAMT,
      *            LNFILE.LN_LASTPYAMT,
      *            LNFILE.LN_PY_YYYYMM_RECDEL,
      *            LNFILE.LN_MTD_PAYS_RECDEL,
      *            LNFILE.LN_DLNO,
      *            LNFILE.LN_CURBAL,
      *            LNFILE.LN_INTBAL,
      *            LNFILE.LN_LCBAL,
      *            LNFILE.LN_OTHBAL,
      *            LNFILE.LN_OT2BAL,
      *            LNFILE.LN_MONTHEND_BAL,
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
      *            LNFILE.LN_ANTICADJ_1,
      *            LNFILE.LN_ANTICADJ_2,
      *            LNFILE.LN_ANTICADJ_3,
      *            LNFILE.LN_ANTICADJ_4,
      *            LNFILE.LN_ANTICADJ_5,
      *            LNFILE.LN_ANTICADJ_6,
      *            LNFILE.LN_ANTICADJ_7,
      *            LNFILE.LN_ANTICADJ_8,
      *            LNFILE.LN_ANTICADJ_9,
      *            LNFILE.LN_ANTICADJ_10,
      *            LNFILE.LN_UFERN_1,
      *            LNFILE.LN_UFERN_2,
      *            LNFILE.LN_UFERN_3,
      *            LNFILE.LN_UFERN_4,
      *            LNFILE.LN_UFERN_5,
      *            LNFILE.LN_UFERN_6,
      *            LNFILE.LN_UFERN_7,
      *            LNFILE.LN_UFERN_8,
      *            LNFILE.LN_UFERN_9,
      *            LNFILE.LN_UFERN_10,
      *            LNFILE.LN_ACCERN_1,
      *            LNFILE.LN_ACCERN_2,
      *            LNFILE.LN_ACCERN_3,
      *            LNFILE.LN_ACCERN_4,
      *            LNFILE.LN_ACCERN_5,
      *            LNFILE.LN_ACCERN_6,
      *            LNFILE.LN_ACCERN_7,
      *            LNFILE.LN_ACCERN_8,
      *            LNFILE.LN_ACCERN_9,
      *            LNFILE.LN_ACCERN_10,
      *            LNFILE.LN_TOTPAYMNTD,
      *            LNFILE.LN_TOTNODEF,
      *            LNFILE.LN_YTDNODEF,
      *            LNFILE.LN_YTDNODEFACT,
      *            LNFILE.LN_TOTNOALDEL,
      *            LNFILE.LN_YTDNOALDEL,
      *            CAST(LNFILE.LN_DATE_PAID_LAST AS VARCHAR(10)) AS LN_DATE_PAID_LAST,
      *            LNFILE.LN_PRLNAMT,
      *            LNFILE.LN_PRPDUE_DAYS,
      *            LNFILE.LN_SUM_ORGTERM,
      *            LNFILE.LN_UNITPER_CD,
      *            LNFILE.LN_UNITPER_FREQ,
      *            LNFILE.LN_REPOCD,
      *            LNFILE.LN_MTD_PRIN,
      *            LNFILE.LN_MTD_LC,
      *            LNFILE.LN_MTD_INT,
      *            LNFILE.LN_MTD_OT,
      *            LNFILE.LN_MTD_EX,
      *            LNFILE.LN_BMO_CON_STATUS,
      *            LNFILE.LN_BMO_REC_STATUS,
      *            LNFILE.LN_CONPROFILE_1,
      *            LNFILE.LN_CONPROFILE_2,
      *            LNFILE.LN_CONPROFILE_3,
      *            LNFILE.LN_CONPROFILE_4,
      *            LNFILE.LN_CONPROFILE_5,
      *            LNFILE.LN_RECPROFILE_1,
      *            LNFILE.LN_RECPROFILE_2,
      *            LNFILE.LN_RECPROFILE_3,
      *            LNFILE.LN_RECPROFILE_4,
      *            LNFILE.LN_RECPROFILE_5
      *        FROM DBO.LNFILE
      *        WHERE LNFILE.LN_OWNBR = @QLN1_WBEG_OWNBR
      *          AND LNFILE.LN_POCD  NOT IN ('**', '*I', '*R', '*U')
      *        ORDER BY LN_CLASS;

           EXEC SQL
            DECLARE LN3_CURSOR CURSOR FOR
                :LN3-RETURN-CODE = 
                 EXEC SSP_EOMALL_LNFILE_SELECT
                  ( :QLN3-WBEG-OWNBR)
                END-EXEC.

           EXEC SQL
               OPEN LN3_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           EXEC SQL SET CONNECTION C1 END-EXEC.

           MOVE STAT---GOOD TO LN3-CURSOR-STAT.

      *-----------------------------------------------------------------
       READ-LN3-FILE-NEXT.   *> SSP - LN 
           PERFORM READ-IT.
           MOVE LN-PATH-OWNBR  TO LN-OWNBR.
           MOVE LN-PATH-SQL    TO E-FILE.
           MOVE SPACES         TO E-KEYX.
           STRING LN-OWNBR, LN-CLASS,LN-ACCTNO
              DELIMITED BY " " INTO E-KEYX.
           INITIALIZE QLN-REC.

           EXEC SQL SET CONNECTION LN3_CONNECT END-EXEC.

           IF ( LN3-CURSOR-STAT-GOOD )
              EXEC SQL
                  FETCH LN3_CURSOR INTO 
                   :QLN-OWNBR,
                   :QLN-ACCTNO,
                   :QLN-CLASS,
                   :QLN-SUBCLASS,
                   :QLN-LAWCODE,
                   :QLN-ORGST,
                   :QLN-SPRCLASS,
                   :QLN-SRCD,
                   :QLN-POCD,
                   :QLN-PLCD,
                   :QLN-PLTYPE,
                   :QLN-SERVACCT,
                   :QLN-PAY-SCHLD-FG,
                   :QLN-LOANDATE,
                   :QLN-ORIG-1STPYDATE, 
                   :QLN-1STPYDATE,
                   :QLN-PLDATE,     
                   :QLN-POFFDATE,
                   :QLN-BNKRPTDATE,
                   :QLN-REPODATE,
                   :QLN-REDEEMDATE,
                   :QLN-TRW-DOFD-DATE,
                   :QLN-LNAMT,
                   :QLN-ORGTERM,
                   :QLN-1STPYAMT,
                   :QLN-NOREGPY,
                   :QLN-REGPYAMT,
                   :QLN-LASTPYAMT,
                   :QLN-PY-YYYYMM-RECDEL,
                   :QLN-MTD-PAYS-RECDEL,
                   :QLN-DLNO,
                   :QLN-CURBAL,
                   :QLN-INTBAL,
                   :QLN-LCBAL,
                   :QLN-OTHBAL,
                   :QLN-OT2BAL,
                   :QLN-MONTHEND-BAL,
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
                   :QLN-YTDNODEF,
                   :QLN-YTDNODEFACT,
                   :QLN-TOTNOALDEL,
                   :QLN-YTDNOALDEL,
                   :QLN-DATE-PAID-LAST,
                   :QLN-PRLNAMT,
                   :QLN-PRPDUE-DAYS,
                   :QLN-SUM-ORGTERM,
                   :QLN-UNITPER-CD,
                   :QLN-UNITPER-FREQ,
                   :QLN-REPOCD,
                   :QLN-MTD-PRIN,
                   :QLN-MTD-LC,
                   :QLN-MTD-INT,
                   :QLN-MTD-OT,
                   :QLN-MTD-EX,
                   :QLN-BMO-CON-STATUS,
                   :QLN-BMO-REC-STATUS,
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
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           EXEC SQL SET CONNECTION C1 END-EXEC.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-LN3-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-LN-FIELDS.

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


       COPY "LIBLP/LPCA1IN.CPY".
      *-----------------------------------------------------------------
       DELETE-CA1-FILE-MULT.
           PERFORM DELETE-IT.
           MOVE CA-PATH-SQL    TO E-FILE.
           MOVE CA1-KEY        TO E-KEYX.

           MOVE "2" TO SQL-UPDATE-CONN-USE-SW. *> USE UPDATE_CONNECT
           PERFORM SQL-SWAP-UPDATE-CONNECTION.

           EXEC SQL
            DELETE FROM DBO.CAFILE
            WHERE CAFILE.CA_BRNO         = :QCA1-WBEG-BRNO
              AND CAFILE.CA_DATE   BETWEEN :QCA1-WBEG-DATE
                                       AND :QCA1-WEND-DATE
              AND CAFILE.CA_TILLNO BETWEEN :QCA1-WBEG-TILLNO
                                       AND :QCA1-WEND-TILLNO
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO DELETE-CA1-FILE-MULT.

           MOVE "1" TO SQL-UPDATE-CONN-USE-SW. *> USE C1 CONNECT
           PERFORM SQL-SWAP-UPDATE-CONNECTION.


       COPY "LIBLP/LPTY1IN.CPY".
      *-----------------------------------------------------------------
       DELETE-TY1-FILE-MULT.
           PERFORM DELETE-IT.
           MOVE TY-PATH-SQL    TO E-FILE.
           MOVE TY1-KEY        TO E-KEYX.

           MOVE "2" TO SQL-UPDATE-CONN-USE-SW. *> USE UPDATE_CONNECT
           PERFORM SQL-SWAP-UPDATE-CONNECTION.

           EXEC SQL
            DELETE FROM DBO.TYFILE
            WHERE TYFILE.TY_BRNO        = :QTY1-WBEG-BRNO
              AND TYFILE.TY_DATE  BETWEEN :QTY1-WBEG-DATE
                                      AND :QTY1-WEND-DATE
              AND TYFILE.TY_CLASS BETWEEN :QTY1-WBEG-CLASS
                                      AND :QTY1-WEND-CLASS
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO DELETE-TY1-FILE-MULT.

           MOVE "1" TO SQL-UPDATE-CONN-USE-SW. *> USE C1 CONNECT
           PERFORM SQL-SWAP-UPDATE-CONNECTION.


       COPY "LIBLP/LPTRB1IN.CPY".
      *-----------------------------------------------------------------
       DELETE-TRB1-FILE-MULT.
           PERFORM DELETE-IT.
           MOVE TRB-PATH-SQL    TO E-FILE.
           MOVE TRB1-KEY        TO E-KEYX.

           MOVE '2' TO SQL-UPDATE-CONN-USE-SW. *> USE UPDATE_CONNECT
           PERFORM SQL-SWAP-UPDATE-CONNECTION.

           EXEC SQL
            DELETE FROM DBO.TRBFILE
            WHERE TRBFILE.TB_BRNO          = :QTRB1-WBEG-BRNO
              AND TRBFILE.TB_DATE    BETWEEN :QTRB1-WBEG-DATE
                                         AND :QTRB1-WEND-DATE
              AND TRBFILE.TB_CLASS   BETWEEN :QTRB1-WBEG-CLASS
                                         AND :QTRB1-WEND-CLASS
              AND TRBFILE.TB_ACCTNO  BETWEEN :QTRB1-WBEG-ACCTNO
                                         AND :QTRB1-WEND-ACCTNO
              AND TRBFILE.TB_LAWCODE BETWEEN :QTRB1-WBEG-LAWCODE
                                         AND :QTRB1-WEND-LAWCODE
              AND TRBFILE.TB_LINE    BETWEEN :QTRB1-WBEG-LINE
                                         AND :QTRB1-WEND-LINE
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO DELETE-TRB1-FILE-MULT.

           MOVE "1" TO SQL-UPDATE-CONN-USE-SW. *> USE C1 CONNECT
           PERFORM SQL-SWAP-UPDATE-CONNECTION.


       COPY "LIBLP/LPTRC1IN.CPY".
      *-----------------------------------------------------------------
       DELETE-TRC1-FILE-MULT.
           PERFORM DELETE-IT.
           MOVE TRC-PATH-SQL    TO E-FILE.
           MOVE TRC1-KEY        TO E-KEYX.

           MOVE "2" TO SQL-UPDATE-CONN-USE-SW. *> USE UPDATE_CONNECT
           PERFORM SQL-SWAP-UPDATE-CONNECTION.

           EXEC SQL
            DELETE FROM DBO.TRCFILE
            WHERE TRCFILE.TC_BRNO = :QTRC1-WBEG-BRNO
              AND TRCFILE.TC_KEY_DATE BETWEEN :QTRC1-WBEG-KEY-DATE
                                          AND :QTRC1-WEND-KEY-DATE
              AND TRCFILE.TC_CKNO     BETWEEN :QTRC1-WBEG-CKNO
                                          AND :QTRC1-WEND-CKNO
              AND TRCFILE.TC_CLASS    BETWEEN :QTRC1-WBEG-CLASS
                                          AND :QTRC1-WEND-CLASS
              AND TRCFILE.TC_SEQ      BETWEEN :QTRC1-WBEG-SEQ
                                          AND :QTRC1-WEND-SEQ
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO DELETE-TRC1-FILE-MULT.

           MOVE "1" TO SQL-UPDATE-CONN-USE-SW. *> USE C1 CONNECT
           PERFORM SQL-SWAP-UPDATE-CONNECTION.


       COPY "LIBLP/LPTRD1IN.CPY".
      *-----------------------------------------------------------------
       DELETE-TRD1-FILE-MULT.
           PERFORM DELETE-IT.
           MOVE TRD-PATH-SQL    TO E-FILE.
           MOVE TRD1-KEY        TO E-KEYX.

           MOVE "2" TO SQL-UPDATE-CONN-USE-SW. *> USE UPDATE_CONNECT
           PERFORM SQL-SWAP-UPDATE-CONNECTION.

           EXEC SQL
            DELETE FROM DBO.TRDFILE
            WHERE TRDFILE.TD_BRNO       = :QTRD1-WBEG-BRNO
              AND TRDFILE.TD_DATE BETWEEN :QTRD1-WBEG-DATE 
                                      AND :QTRD1-WEND-DATE
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO DELETE-TRD1-FILE-MULT.

           MOVE "1" TO SQL-UPDATE-CONN-USE-SW. *> USE C1 CONNECT
           PERFORM SQL-SWAP-UPDATE-CONNECTION.


       COPY "LIBLP/LPTRI1IN.CPY".
      *-----------------------------------------------------------------
       DELETE-TRI1-FILE-MULT.
           PERFORM DELETE-IT.
           MOVE TRI-PATH-SQL    TO E-FILE.
           MOVE TRI1-KEY        TO E-KEYX.

           MOVE "2" TO SQL-UPDATE-CONN-USE-SW. *> USE UPDATE_CONNECT
           PERFORM SQL-SWAP-UPDATE-CONNECTION.

           EXEC SQL
            DELETE FROM DBO.TRIFILE
            WHERE TRIFILE.TI_BRNO           = :QTRI1-WBEG-BRNO
              AND TRIFILE.TI_KEY_DATE BETWEEN :QTRI1-WBEG-DATE
                                          AND :QTRI1-WEND-DATE
              AND TRIFILE.TI_SEQ      BETWEEN :QTRI1-WBEG-SEQ
                                          AND :QTRI1-WEND-SEQ
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO DELETE-TRI1-FILE-MULT.

           MOVE "1" TO SQL-UPDATE-CONN-USE-SW. *> USE C1 CONNECT
           PERFORM SQL-SWAP-UPDATE-CONNECTION.


       COPY "LIBLP/LPTRJ1IN.CPY".
      *-----------------------------------------------------------------
       DELETE-TRJ1-FILE-MULT.
           PERFORM DELETE-IT.
           MOVE TRJ-PATH-SQL    TO E-FILE.
           MOVE TRJ1-KEY        TO E-KEYX.

           MOVE "2" TO SQL-UPDATE-CONN-USE-SW. *> USE UPDATE_CONNECT
           PERFORM SQL-SWAP-UPDATE-CONNECTION.

           EXEC SQL
            DELETE FROM DBO.TRJFILE
            WHERE TRJFILE.TJ_BRNO          = :QTRJ1-WBEG-BRNO
              AND TRJFILE.TJ_DATE    BETWEEN :QTRJ1-WBEG-DATE
                                         AND :QTRJ1-WEND-DATE
              AND TRJFILE.TJ_CLASS   BETWEEN :QTRJ1-WBEG-CLASS
                                         AND :QTRJ1-WEND-CLASS
              AND TRJFILE.TJ_ACCTNO  BETWEEN :QTRJ1-WBEG-ACCTNO
                                         AND :QTRJ1-WEND-ACCTNO
              AND TRJFILE.TJ_LAWCODE BETWEEN :QTRJ1-WBEG-LAWCODE
                                         AND :QTRJ1-WEND-LAWCODE
              AND TRJFILE.TJ_LINE    BETWEEN :QTRJ1-WBEG-LINE
                                         AND :QTRJ1-WEND-LINE
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO DELETE-TRJ1-FILE-MULT.

           MOVE "1" TO SQL-UPDATE-CONN-USE-SW. *> USE C1 CONNECT
           PERFORM SQL-SWAP-UPDATE-CONNECTION.


       COPY "LIBLP/LPTRK1IN.CPY".
      *-----------------------------------------------------------------
       DELETE-TRK1-FILE-MULT.
           PERFORM DELETE-IT.
           MOVE TRK-PATH-SQL    TO E-FILE.
           MOVE TRK1-KEY        TO E-KEYX.

           MOVE '2' TO SQL-UPDATE-CONN-USE-SW. *> USE UPDATE_CONNECT
           PERFORM SQL-SWAP-UPDATE-CONNECTION.

           EXEC SQL
            DELETE FROM DBO.TRKFILE
            WHERE TRKFILE.TK_BRNO          = :QTRK1-WBEG-BRNO
              AND TRKFILE.TK_DATE    BETWEEN :QTRK1-WBEG-DATE
                                         AND :QTRK1-WEND-DATE
              AND TRKFILE.TK_CLASS   BETWEEN :QTRK1-WBEG-CLASS
                                         AND :QTRK1-WEND-CLASS
              AND TRKFILE.TK_ACCTNO  BETWEEN :QTRK1-WBEG-ACCTNO
                                         AND :QTRK1-WEND-ACCTNO
              AND TRKFILE.TK_LAWCODE BETWEEN :QTRK1-WBEG-LAWCODE
                                         AND :QTRK1-WEND-LAWCODE
              AND TRKFILE.TK_LINE    BETWEEN :QTRK1-WBEG-LINE
                                         AND :QTRK1-WEND-LINE
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO DELETE-TRK1-FILE-MULT.

           MOVE "1" TO SQL-UPDATE-CONN-USE-SW. *> USE C1 CONNECT
           PERFORM SQL-SWAP-UPDATE-CONNECTION.


       COPY "LIBLP/LPTRL1IN.CPY".
      *-----------------------------------------------------------------
       DELETE-TRL1-FILE-MULT.
           PERFORM DELETE-IT.
           MOVE TRL-PATH-SQL    TO E-FILE.
           MOVE TRL1-KEY        TO E-KEYX.

           MOVE "2" TO SQL-UPDATE-CONN-USE-SW. *> USE UPDATE_CONNECT
           PERFORM SQL-SWAP-UPDATE-CONNECTION.
 
           EXEC SQL
            DELETE FROM DBO.TRLFILE
            WHERE TRLFILE.TL_BRNO          = :QTRL1-WBEG-BRNO
              AND TRLFILE.TL_DATE    BETWEEN :QTRL1-WBEG-DATE
                                         AND :QTRL1-WEND-DATE
              AND TRLFILE.TL_CLASS   BETWEEN :QTRL1-WBEG-CLASS
                                         AND :QTRL1-WEND-CLASS
              AND TRLFILE.TL_ACCTNO  BETWEEN :QTRL1-WBEG-ACCTNO
                                         AND :QTRL1-WEND-ACCTNO
              AND TRLFILE.TL_LAWCODE BETWEEN :QTRL1-WBEG-LAWCODE
                                         AND :QTRL1-WEND-LAWCODE
              AND TRLFILE.TL_LINE    BETWEEN :QTRL1-WBEG-LINE
                                         AND :QTRL1-WEND-LINE
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO DELETE-TRL1-FILE-MULT.

           MOVE "1" TO SQL-UPDATE-CONN-USE-SW. *> USE C1 CONNECT
           PERFORM SQL-SWAP-UPDATE-CONNECTION.


       COPY "LIBLP/LPTRP1IN.CPY".
      *-----------------------------------------------------------------
       DELETE-TRP1-FILE-MULT.
           PERFORM DELETE-IT.
           MOVE TRP-PATH-SQL    TO E-FILE.
           MOVE TRP1-KEY        TO E-KEYX.

           MOVE "2" TO SQL-UPDATE-CONN-USE-SW. *> USE UPDATE_CONNECT
           PERFORM SQL-SWAP-UPDATE-CONNECTION.

           EXEC SQL
            DELETE FROM DBO.TRPFILE
            WHERE TRPFILE.TP_BRNO         = :QTRP1-WBEG-BRNO
              AND TRPFILE.TP_DATE   BETWEEN :QTRP1-WBEG-DATE
                                        AND :QTRP1-WEND-DATE
              AND TRPFILE.TP_CLASS  BETWEEN :QTRP1-WBEG-CLASS
                                        AND :QTRP1-WEND-CLASS
              AND TRPFILE.TP_ACCTNO BETWEEN :QTRP1-WBEG-ACCTNO
                                        AND :QTRP1-WEND-ACCTNO
              AND TRPFILE.TP_LINE   BETWEEN :QTRP1-WBEG-LINE
                                        AND :QTRP1-WEND-LINE
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO DELETE-TRP1-FILE-MULT.

           MOVE "1" TO SQL-UPDATE-CONN-USE-SW. *> USE C1 CONNECT
           PERFORM SQL-SWAP-UPDATE-CONNECTION.


       COPY "LIBLP/LPTRS1IN.CPY".
      *-----------------------------------------------------------------
       DELETE-TRS1-FILE-MULT.
           PERFORM DELETE-IT.
           MOVE TRS-PATH-SQL    TO E-FILE.
           MOVE TRS1-KEY        TO E-KEYX.

           MOVE "2" TO SQL-UPDATE-CONN-USE-SW. *> USE UPDATE_CONNECT
           PERFORM SQL-SWAP-UPDATE-CONNECTION.

           EXEC SQL
            DELETE FROM DBO.TRSFILE
            WHERE TRSFILE.TS_BRNO          = :QTRS1-WBEG-BRNO
              AND TRSFILE.TS_DATE    BETWEEN :QTRS1-WBEG-DATE
                                         AND :QTRS1-WEND-DATE
              AND TRSFILE.TS_CLASS   BETWEEN :QTRS1-WBEG-CLASS
                                         AND :QTRS1-WEND-CLASS
              AND TRSFILE.TS_ACCTNO  BETWEEN :QTRS1-WBEG-ACCTNO
                                         AND :QTRS1-WEND-ACCTNO
              AND TRSFILE.TS_LAWCODE BETWEEN :QTRS1-WBEG-LAWCODE
                                         AND :QTRS1-WEND-LAWCODE
              AND TRSFILE.TS_LINE    BETWEEN :QTRS1-WBEG-LINE
                                         AND :QTRS1-WEND-LINE
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO DELETE-TRS1-FILE-MULT.

           MOVE "1" TO SQL-UPDATE-CONN-USE-SW. *> USE C1 CONNECT
           PERFORM SQL-SWAP-UPDATE-CONNECTION.


       COPY "LIBLP/LPTRW1IN.CPY".
      *-----------------------------------------------------------------
       DELETE-TRW1-FILE-MULT.
           PERFORM DELETE-IT.
           MOVE TRW-PATH-SQL TO E-FILE.
           MOVE TRW1-KEY     TO E-KEYX.

           MOVE "2" TO SQL-UPDATE-CONN-USE-SW. *> USE UPDATE_CONNECT
           PERFORM SQL-SWAP-UPDATE-CONNECTION.

           EXEC SQL
            DELETE FROM DBO.TRWFILE
            WHERE TRWFILE.TW_BRNO         = :QTRW1-WBEG-BRNO
              AND TRWFILE.TW_DATE   BETWEEN :QTRW1-WBEG-DATE
                                        AND :QTRW1-WEND-DATE
              AND TRWFILE.TW_TILLNO BETWEEN :QTRW1-WBEG-TILLNO 
                                        AND :QTRW1-WEND-TILLNO
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO DELETE-TRW1-FILE-MULT.

           MOVE "1" TO SQL-UPDATE-CONN-USE-SW. *> USE C1 CONNECT
           PERFORM SQL-SWAP-UPDATE-CONNECTION.


       COPY "LIBLP/LPTRX1IN.CPY".
      *-----------------------------------------------------------------
       DELETE-TRX1-FILE-MULT.
           PERFORM DELETE-IT.
           MOVE TRX-PATH-SQL    TO E-FILE.
           MOVE TRX1-KEY        TO E-KEYX.

           MOVE "2" TO SQL-UPDATE-CONN-USE-SW. *> USE UPDATE_CONNECT
           PERFORM SQL-SWAP-UPDATE-CONNECTION.

           EXEC SQL
            DELETE FROM DBO.TRXFILE
            WHERE TRXFILE.TX_BRNO          = :QTRX1-WBEG-BRNO
              AND TRXFILE.TX_DATE    BETWEEN :QTRX1-WBEG-DATE
                                         AND :QTRX1-WEND-DATE
              AND TRXFILE.TX_CLASS   BETWEEN :QTRX1-WBEG-CLASS
                                         AND :QTRX1-WEND-CLASS
              AND TRXFILE.TX_ACCTNO  BETWEEN :QTRX1-WBEG-ACCTNO
                                         AND :QTRX1-WEND-ACCTNO
              AND TRXFILE.TX_LAWCODE BETWEEN :QTRX1-WBEG-LAWCODE
                                         AND :QTRX1-WEND-LAWCODE
              AND TRXFILE.TX_LINE    BETWEEN :QTRX1-WBEG-LINE
                                         AND :QTRX1-WEND-LINE
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO DELETE-TRX1-FILE-MULT.

           MOVE "1" TO SQL-UPDATE-CONN-USE-SW. *> USE C1 CONNECT
           PERFORM SQL-SWAP-UPDATE-CONNECTION.

       OPEN-BYADATA-FILE-OUTPUT.
           PERFORM OPEN-IT.
           MOVE BYADATA-OUT-PATH TO E-FILE.
           OPEN OUTPUT BYADATA-OUT-FILE.
           IF IO-FG = 8
              GO TO OPEN-BYADATA-FILE-OUTPUT.
           IF IO-FG = 7
              CLOSE BYADATA-OUT-FILE
              GO TO OPEN-BYADATA-FILE-OUTPUT.
       WRITE-BYADATA-FILE.
           PERFORM WRITE-IT.
           MOVE BYADATA-OUT-PATH TO E-FILE.
           WRITE BYADATA-OUT-REC.
           IF IO-FG = 8
             GO TO WRITE-BYADATA-FILE.
       CLOSE-BYADATA-FILE.
           PERFORM CLOSE-IT.
           CLOSE BYADATA-OUT-FILE.

       OPEN-BYBDATA-FILE-OUTPUT.
           PERFORM OPEN-IT.
           MOVE BYBDATA-OUT-PATH TO E-FILE.
           OPEN OUTPUT BYBDATA-OUT-FILE.
           IF IO-FG = 8
              GO TO OPEN-BYBDATA-FILE-OUTPUT.
           IF IO-FG = 7
              CLOSE BYBDATA-OUT-FILE
              GO TO OPEN-BYBDATA-FILE-OUTPUT.
       WRITE-BYBDATA-FILE.
           PERFORM WRITE-IT.
           MOVE BYBDATA-OUT-PATH TO E-FILE.
           WRITE BYBDATA-OUT-REC.
           IF IO-FG = 8
             GO TO WRITE-BYBDATA-FILE.
       CLOSE-BYBDATA-FILE.
           PERFORM CLOSE-IT.
           CLOSE BYBDATA-OUT-FILE.



       OPEN-RCDATA-FILE-OUTPUT.
           PERFORM OPEN-IT.
           MOVE RCDATA-OUT-PATH TO E-FILE.
           OPEN OUTPUT RCDATA-OUT-FILE.
           IF IO-FG = 8
              GO TO OPEN-RCDATA-FILE-OUTPUT.
           IF IO-FG = 7
              CLOSE RCDATA-OUT-FILE
              GO TO OPEN-RCDATA-FILE-OUTPUT.
       WRITE-RCDATA-FILE.
           PERFORM WRITE-IT.
           MOVE RCDATA-OUT-PATH TO E-FILE.
           WRITE RCDATA-OUT-REC.
           IF IO-FG = 8
             GO TO WRITE-RCDATA-FILE.
       CLOSE-RCDATA-FILE.
           PERFORM CLOSE-IT.
           CLOSE RCDATA-OUT-FILE.

       OPEN-LNDATA-FILE-OUTPUT.
           PERFORM OPEN-IT.
           MOVE LNDATA-OUT-PATH TO E-FILE.
           OPEN OUTPUT LNDATA-OUT-FILE.
           IF IO-FG = 8
              GO TO OPEN-LNDATA-FILE-OUTPUT.
           IF IO-FG = 7
              CLOSE LNDATA-OUT-FILE
              GO TO OPEN-LNDATA-FILE-OUTPUT.
       WRITE-LNDATA-FILE.
           PERFORM WRITE-IT.
           MOVE LNDATA-OUT-PATH TO E-FILE.
           WRITE LNDATA-OUT-REC.
           IF IO-FG = 8
             GO TO WRITE-LNDATA-FILE.
       CLOSE-LNDATA-FILE.
           PERFORM CLOSE-IT.
           CLOSE LNDATA-OUT-FILE.


       OPEN-GBDATA-FILE-OUTPUT.
           PERFORM OPEN-IT.
           MOVE GBDATA-OUT-PATH TO E-FILE.
           OPEN OUTPUT GBDATA-OUT-FILE.
           IF IO-FG = 8
              GO TO OPEN-GBDATA-FILE-OUTPUT.
           IF IO-FG = 7
              CLOSE GBDATA-OUT-FILE
              GO TO OPEN-GBDATA-FILE-OUTPUT.
       WRITE-GBDATA-FILE.
           PERFORM WRITE-IT.
           MOVE GBDATA-OUT-PATH TO E-FILE.
           WRITE GBDATA-OUT-REC.
           IF IO-FG = 8
             GO TO WRITE-GBDATA-FILE.
       CLOSE-GBDATA-FILE.
           PERFORM CLOSE-IT.
           CLOSE GBDATA-OUT-FILE.


       OPEN-STAT-FILE.
           PERFORM OPEN-IT.
           MOVE STAT-OUT-PATH TO E-FILE.
           OPEN OUTPUT STAT-OUT-FILE.
       WRITE-STAT-FILE.
           PERFORM WRITE-IT.
           MOVE STAT-OUT-PATH TO E-FILE.
           WRITE STAT-OUT-REC.
           IF IO-FG = 8
             GO TO WRITE-STAT-FILE.
       CLOSE-STAT-FILE.
           PERFORM CLOSE-IT.
           CLOSE STAT-OUT-FILE.

      *=================================================================
       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO EXT-EOBUF-ERRCD.

      *****************************************************************
      *
      *    E N D     P R O G R A M
      *
      *****************************************************************
       END-PROGRAM SECTION.
       END-RUN.

      * STOPPING TIMER AND DISPLAYING WINDOW IF NOT BATCH - S35Q-104
           MOVE "GB/TIMER" TO FORM-NAM.
           CALL FORM-PROGX USING P-TIMER-LINK-BUF.
           CANCEL FORM-PROGX.

           PERFORM CLOSE-FILES.

      *----------------------------------------------------------------
       EXIT-PROG.

           MOVE VGB-PATH TO ACCESS-BUF.
           PERFORM REMOVE-WORKFILE.

           MOVE VRC-PATH TO ACCESS-BUF.
           PERFORM REMOVE-WORKFILE.

           MOVE VBY-PATH TO ACCESS-BUF.
           PERFORM REMOVE-WORKFILE.

           IF EXT-ROUTE-BUF NOT = "60"  
              IF WS-PGM-FAILED-SW = "Y"
                 PERFORM CLEAR-WK-FILES.
 
           IF EXT-ROUTE-BUF = "00"
              MOVE EXIT-PATH TO FORM-NAM
              MOVE FORM-PATH TO FORM-PATHNAME.

       STOP-RUN.
           IF EXT-ROUTE-BUF = "00" OR "40" OR "50"
              DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN
              DESTROY EOMALL-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.
           EXIT PROGRAM.
