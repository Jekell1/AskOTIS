       IDENTIFICATION DIVISION.
       PROGRAM-ID.      LONPMC.
       DATE-WRITTEN.    120699.
      ******************************************************************
      *                      (LP)
      *      DESCRIPTION: VARIABLE FORM PRINT -
      *                   CONTINUOUS FORMS PRINT
      *                   GLOBAL VERSION - CROSSES DATA PATHS
      *
      * IF PRINTING A "MC" FORM FROM SCREEN 7 DOC PRINT, IT WILL ONLY PRINT
      * YOUR ONE ACCOUNT # AND EXIT, BASED UPON EDIT-FM-INITKEY BEING SPACES
      * THE FIRST TIME THROUGH AND THEN IT GETS FILLED IN SO YOU EXIT.
      * (IN NEXT-FM)
      *=================================================================
      * REV:
      * BAH 042895 ALLOW COLLATERAL TRAILER 7
      *            NOTE: CANT BE INITIAL FILE - USE LN AS INITIAL;
      *            LOADS ALL LT RECORD SEQ'S INTO ONE BIG WORKING
      *            STORAGE GUY
      * BAH 060195 CHANGED WK-COLL-AMT1 TO WK-MILEAGE
      *                    WK-COLL-AMT2 TO WK-VALUE
      *                    WK-COLL-AMT3 TO WK-SELLPR
      *                    WK-COLL-AMT4 TO WK-MISC-AMT
      * BAH 062696 ADDED FORM RANGE
      * KEC 011097 FIXED UPDATING THE LOAN MEMO FILE: MOVING FM-FMNO TO
      *            IP-FMNO IN 'BYPASS-FM' PARAGRAPH IN ORDER TO HAVE
      *            CORRECT 'F-B-REC' INFORMATION WHEN UPDATING THE LOAN
      *            MEMO FILE; WAS A ONLY A PROBLEM WHEN ENTERING A
      *            FORM NUMBER RANGE.
      * JTG 021797 ADDED LOGIC FOR FD-FORM-CHG-CD, BHFC
      * JTG 022497 CHANGED LOGIC FOR FD-FORM-CHG-CD, TO SET TP-BRNO
      *            AND TP-POSTING-BR CORRECTLY WHEN F3-ALL BRANCHES
      * BAH 062497 ADDED FILE # 8-AUTO INS TRAILER; NOTE: CAN'T
      *            BE INITIAL FILE - USE LN AS INITIAL; LOADS ALL LT
      *            RECORD SEQUENCES INTO ONE BIG WORKING STORAGE GUY
      * BAH 071097 ADDED FILE # 10 - REPO TRAILER (LSFILE) : CANT BE INITIAL
      *            FILE - USE LN AS INITIAL
      * BAH 072897 USE IP-FMNO TO REWRITE NO FILE WITH NO-PRINT = 'Y',
      *            WAS USING LASTCFORMNO, WHICH WAS LEAVING SOLICIT FORMS
      *            BLANK, OR IF FD REC DIDNT HAVE 'TYPE' FILLED IN
      * GLF 091997 ADDED UPDATE OF BW-LAST-FORM-DATE BW-LAST-FORM-NO
      * MJD 112597 EXPANDED FD-REC TO 2828 BYTES (ADDED 30)
      * BAH 021798 ADDED MA, LA, AND BA FILES
      * MJD 032098 ADDED GETENV(W) FOR USE IN LIBLP/POSTDATE
      * BAH 040998 ADDED MLFILE (14) FOR WORLD MAIL LIST
      * BAH 052898 CREATE AN APP MEMO IF 1STFILE 13 AND MEMO = "Y"
      * BAH 062598 CHANGED TP-TILL PIC 9 TO TP-TILLNO PIC X, CHANGED LP-TILLNO
      *            TO PIC X.
      * BAH 101698 ADDED UNLOCKS AFTER READ-BW AND READ-LN, IF A PRINTER
      *            JAMS AND USER IS KILLED, FILES STAY LOCKED !!
      * KEC 110698 MADE CHANGE FOR 'YEAR 2000' FORMAT FOR [AM/LM]-DATE.
      * BAH 991020 CHANGED LPEXTW DATA-AREA FROM 1800 TO 4500 FOR LA-FILE
      * BLV 991214 CHANGED TO CROSS BRANCHES, OLD SOURCE AND OBJECT SAVED
      *            IN LONPMC.LOCAL, PR #1030
      * BLV 000114 INITIALIZATION READING BRFILE BASED ON BRANCH, IF F3
      *            ALL BRANCHES BRANCH = 0 & DEFAULTED TO BRANCH 1, WHICH
      *            BOMBS IF NO BRANCH 1; CHANGED TO DEFAULT TO "LOGNAME"
      *            BRANCH
      *
      * KEC 000316 REVERSED THE PREVIOUS CHANGE (IN A10).  IT DID NOT
      *             WORK AND WAS CAUSING MERCURY FINANCE TO LOCK UP.
      *             WHAT I DID WAS TO COMMENT OUT THE FOLLOWING IN
      *             'GET-FD':
      *
      *                  PERFORM OPEN-FD1-FILE.
      *                  MOVE " " TO LAST-FD1-KEY.
      *             *KC->IF FD1-KEY = F-B-FMNO
      *             *KC->   GO TO EXIT-FIND-FIRST-FIELD.
      *                  PERFORM READ-FD1-FILE.
      *
      *            TESTED THIS CHANGE FOR BOTH SITUATIONS (SINGLE FORM/
      *             F5-RANGE) AND WORKS.
      * BLV 000619 CHANGED INIT-KEY-MISSING & ALL SEND-MESSES TO PERFORM
      *            CHECK-PIGGY-SEND-MESS - THIS WILL CLOSE & RESET PRINTER
      *            BEFORE SEND-MESS IF PIGGY-BACK PRINTING, & RE-OPEN
      *            PRINTER AFTERWARDS; WILL PREVENT LOCK-UPS
      * BAH 000711 GB-FILE WAS NOT OPEN WHEN USING THE 800 FIELDS, TFC
      * BAH 000816 ADDED BANKRUPT TRAILER, FILE 15 , CAN NOT BE INITIAL
      *            FILE - USE LN (1) AS INITIAL; LOADS ALL 4 SEQUENCES
      *            INTO WK-BANKR-RECORD, REGACC PR# 1279
      * BAH 001013 ADDED FILE 16 (LN) LOAN APPENDAGE, REGENCY NEEDED REVOLVING
      *            FIELDS AND FILE 1 (LN) IS OUT OF ROOM , PR# 1324
      *        **  YOU CAN USE FIRST FILE 1 OR 16, AND ACCESS BOTH SETS OF LOAN
      *            FIELDS   FIRST FILE = 1 AND CAN PRINT FIELD 1509, RPDUEPRN
      * MJD 001016 ADDED UPDATE OF TP-COLLECTOR AND LP-COLLECTOR (BAY PR#1317)
      * BAH 001108 ADDED FILE 17 (VN) A/P VENDOR, WORLD 1099
      * BAH 030429 ADDED UPDATE OF LR-FILE TRIAD TRAILER, REGACC PR# 26
      * BAH 040114 ADDED FILE 18 POOLID (CDFILE), CENTRIX 2228
      *            CAN NOT BE FIRST FILE EVER !
      * BAH 040826 ADDED USERID AND TIME TO THE LOAN AND APP MEMO WHEN THE
      *            FORM IS PHYSICALLY PRINTED. LENDMARK PR# 2367
      * BAH 060711 ADDED LIEN/TITLE TRAILER, FILE 19 , CAN NOT BE INITIAL
      *            FILE - USE LN (1) AS INITIAL; LOADS ALL 5 "L" SEQUENCES
      *            INTO WK-LIEN-RECORD, REGACC PR# 136
      * BAH 060818 DO NOT PUT OTHER CHARGES "OT" ON A PAID OUT ACCOUNT
      *            CITIZENS PL# 550
      * BAH 061107 ADDED FILE 30 (PA) MISC PAYEE FILE, MAS PR# 2868
      * BAH 100921 ADDED WK-L-TITLE-ADR2 TO WK-L-RECORD-5, REGACC PR# 272
      * BAH 111216 ADDED FILE 31 (ES) ESCROW FILE, REGENCY PR# 3930
      * BAH 130329 ADDED FT-FORM-RIGHT-TO-CURE TO UPDATE LR RTC FIELDS
      *                                          REGACC PR# 337
      *  CS 151214 IN SETUP-LP-FORM-CHANGE MOVE LN-OWNBR TO LP-BRNO
      * KEC 2016.0118 FIXED KEYS & RECS FOR FD-REC.
      *               ADDED NEW FDB-FILE; WAS IN FD-FILE.
      *               REMOVED FT-FILE; MERGED INTO FD-FILE.
      *  BAH 160118 SPLIT OUT LTFILE, PD0003
      *  BAH 160209 SPLIT OUT LSFILE, PD0003
      * KEC 2016.0211 FIXED KEYS & RECS FOR FD-REC.
      *               CHANGED FMNO/FORMNO/NEXTFM/SETFM FROM 3 TO 4 BYTES.
      * KEC 2016.0310 CHANGED CD-FC???? TO CD-FC-????
      *               CHANGED CD-POOL???? TO CD-PL-????
      * KEC 2016.0502 CHANGED EDIT-FM-KEY TO BE EDIT-FM-INITKEY.
      *               ADDED   EDIT-FM-INITKEY REDEFINES FOR MA-APP-NUMBER.
      * KEC 2016.0818 REMOVED USE OF THE BA-FILE [LIBLA/LAFDMX].
      *               REVISED LA-REC LAYOUT.
      * KEC 2016.0914 SET LR-BRNO USING LN-OWNBR.
      * KEC 2016.1026 REMOVED USING HOLD-TR1-KEY.
      *               ADDED LIBLP/LPWSTR (NEW) FOR LPWSTR-TR1-KEY (HOLD/SAVE).
      * KEC 2018.0327 ADDED STATEMENT OF MOVING LN-OWNBR TO LP-BRNO 
      *               BEFORE THE PERFORM WRITE-LP1-FILE.
      * BAH 2018.0823 REMOVED APP REFERENCES, LA, MA, AM, LARM30
      * BAH 181004 GLOBAL BWFILE 
      * BAH 181010 GLOBAL DLFILE
      * BAH 181115 GLOBAL FMFILE
      * BAH 181128 GLOBAL LMFILE
      * BAH 181210 GLOBAL LNFILE
      * BAH 181231 GLOBAL LPFILE
      * AMM 190108 GLOBAL MLFILE
      * BAH 190108 GLOBAL NOFILE
      * BAH 190122 SPLIT CDFILE
      * AMM 190205 GLOBAL PAFILE
      *  CS 190212 GLOBAL ESFILE
      * BAH 190313 MOVED EDIT-FM-INITKEY TO LPWSFM-EDIT-FM-INITKEY 
      * BAH 20190709 REMOVED GP-LR-TRAILER-TRIAD, PT0151 
      * BAH 20190917 REMOVED AP VNFILE, NOT IN CRMISS SO OPEN ERROR
      * BLM 20210810 REMOVE TP-CODE, TP-POOLID NO LONGER IN KEY
      * BAH 20220422 UPDATE TP-ACTIONCD2, SECURITIZATION #1551
      * BAH 20220803 HARD CODED START
      * BAH 20230512 REMOVED MLFILE
      * BAH 20230907 REMOVED PA-FILE
      * 20240129 BAH CALL CRNOR2 AND WRITE NOTICE RECORD
      * 20240320 ALLOW CORPORATE PRINT TO WORK
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "LIBGB/GBFSPR.CPY".

       I-O-CONTROL.
       DATA DIVISION.
       FILE SECTION.
       COPY "LIBGB/GBFDPR.CPY".

      ****************************
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(37) VALUE
           "@(#)LP/LONPMC S35 03/20/24-13:50:16 >".

       COPY "LIBGB/GB01GB.CPY".
       COPY "LIBGB/GB01GB_SQL.CPY".
       COPY "LIBGB/GBWSGB.CPY".
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBLP/LP01DL.CPY".
       COPY "LIBLP/LP01DL_SQL.CPY".
       COPY "LIBLP/LPWSDL.CPY".
       COPY "LIBLP/LP01CD.CPY".
       COPY "LIBLP/LP01CD_SQL.CPY".
       COPY "LIBLP/LPWSCD.CPY".
       COPY "LIBLP/LP01SP.CPY".
       COPY "LIBLP/LP01SPA.CPY".
       COPY "LIBLP/LP01SPB.CPY".
       COPY "LIBLP/LP01SPC.CPY".
       COPY "LIBLP/LP01SP_SQL.CPY".
       COPY "LIBLP/LPWSSP.CPY".
       COPY "LIBLP/LP01LN.CPY".
       COPY "LIBLP/LP01LN_SQL.CPY".
       COPY "LIBLP/LPWSLN.CPY".
       COPY "LIBLP/LP01LP.CPY".
       COPY "LIBLP/LP01LP_SQL.CPY".
       COPY "LIBLP/LPWSLP.CPY".
       COPY "LIBLP/LP01BW.CPY".
       COPY "LIBLP/LP01BW_SQL.CPY".
       COPY "LIBLP/LPWSBW.CPY".
       COPY "LIBLP/LP01FD.CPY".
       COPY "LIBLP/LP01FD_SQL.CPY".
       COPY "LIBLP/LPWSFD.CPY".
       COPY "LIBLP/LP01FDB.CPY".
       COPY "LIBLP/LP01FDB_SQL.CPY".
       COPY "LIBLP/LPWSFDB.CPY".
       COPY "LIBLP/LP01FM.CPY".
       COPY "LIBLP/LP01FM_SQL.CPY".
       COPY "LIBLP/LPWSFM.CPY".
       COPY "LIBLP/LP01NO.CPY".
       COPY "LIBLP/LP01NO_SQL.CPY".
       COPY "LIBLP/LPWSNO.CPY".
       COPY "LIBLP/LP01LM.CPY".
       COPY "LIBLP/LP01LM_SQL.CPY".
       COPY "LIBLP/LPWSLM.CPY".
       COPY "LIBLP/LP01LTA.CPY".
       COPY "LIBLP/LP01LTA_SQL.CPY".
       COPY "LIBLP/LPWSLTA.CPY".
       COPY "LIBLP/LP01LTB.CPY".
       COPY "LIBLP/LP01LTB_SQL.CPY".
       COPY "LIBLP/LPWSLTB.CPY".
       COPY "LIBLP/LP01LTC.CPY".
       COPY "LIBLP/LP01LTC_SQL.CPY".
       COPY "LIBLP/LPWSLTC.CPY".
       COPY "LIBLP/LP01LTL.CPY".
       COPY "LIBLP/LP01LTL_SQL.CPY".
       COPY "LIBLP/LPWSLTL.CPY".
       COPY "LIBLP/LP01LTP.CPY".
       COPY "LIBLP/LP01LTP_SQL.CPY".
       COPY "LIBLP/LPWSLTP.CPY".
       COPY "LIBLP/LP01LSX.CPY".
       COPY "LIBLP/LP01LSX_SQL.CPY".
       COPY "LIBLP/LPWSLSX.CPY".
       COPY "LIBLP/LP01ES.CPY".
       COPY "LIBLP/LP01ES_SQL.CPY".
       COPY "LIBLP/LPWSES.CPY".
       COPY "LIBLP/LP01TRP.CPY".
       COPY "LIBLP/LP01TRP_SQL.CPY".
       COPY "LIBLP/LPWSTRP.CPY".
       COPY "LIBLP/LPWKPATH.CPY".

       COPY "LIBLP/LTAUTOW.CPY".
       COPY "LIBLP/LP01WK.CPY".
      * IF THE LT BANKRUPT RECORD CHANGES, THIS NEEDS CHANGED TOO
       01  WK-BANKR-RECORD.
           03  WK-B-FILEDATE         PIC 9(8)  COMP-3.
           03  WK-B-CASE-NO          PIC X(9).
           03  WK-B-CHAPTER          PIC 99.
           03  WK-B-MISC1            PIC X(30).
           03  WK-B-MISC2            PIC X(30).
           03  WK-B-MISC3            PIC X(30).

           03  WK-B-BDATE            PIC 9(8)    COMP-3.
           03  WK-B-WHO-FILED        PIC X.
           03  WK-B-ENDORSER-FILED   PIC X.
           03  WK-B-CLAIM-AMT        PIC S9(7)V99    COMP-3.
           03  WK-B-ACTIVE-INACTIVE  PIC X.
           03  WK-B-INT-LC-DUE       PIC S9(7)V99    COMP-3.
           03  WK-B-OTH-DUE          PIC S9(7)V99    COMP-3.
           03  WK-B-TDUEAMT          PIC S9(7)V99    COMP-3.

      * DEBTORS ATTORNEY
           03  WK-B-NAME             PIC X(24).
           03  WK-B-ADR1             PIC X(24).
           03  WK-B-ADR2             PIC X(24).
           03  WK-B-CITY             PIC X(15).
           03  WK-B-STATE            PIC XX.
           03  WK-B-ZIP              PIC X(10).
           03  WK-B-PHONE            PIC 9(10)    COMP-3.

      * CLERK OF BANKRUPTCY COURT
           03  WK-B-COURT-NAME       PIC X(24).
           03  WK-B-COURT-ADR1       PIC X(24).
           03  WK-B-COURT-ADR2       PIC X(24).
           03  WK-B-COURT-CITY       PIC X(15).
           03  WK-B-COURT-STATE      PIC XX.
           03  WK-B-COURT-ZIP        PIC X(10).
           03  WK-B-COURT-PHONE      PIC 9(10)    COMP-3.

      * TRUSTEE
           03  WK-B-TRUST-NAME       PIC X(24).
           03  WK-B-TRUST-ADR1       PIC X(24).
           03  WK-B-TRUST-ADR2       PIC X(24).
           03  WK-B-TRUST-CITY       PIC X(15).
           03  WK-B-TRUST-STATE      PIC XX.
           03  WK-B-TRUST-ZIP        PIC X(10).
           03  WK-B-TRUST-PHONE      PIC 9(10)    COMP-3.

           03  WK-B-REAFF-DATE       PIC 9(8)     COMP-3.
           03  WK-B-REAFF-SYSDATE    PIC 9(8)     COMP-3.
           03  WK-B-REAFF-BALANCE    PIC 9(6)V99  COMP-3.
           03  WK-B-REAFF-PAYAMT     PIC 9(6)V99  COMP-3.
           03  WK-B-REAFF-NO-PAYS    PIC 9(3).
           03  WK-B-INTRATE          PIC 9(3)V9(3).
           03  WK-B-RESCIND-DATE     PIC 9(8)     COMP-3.
           03  WK-B-PARTIAL-FULL     PIC X.
           03  WK-B-DISCHARGE-DATE   PIC 9(8)     COMP-3.
           03  WK-B-DISCHARGE-SYSDATE  PIC 9(8)     COMP-3.

      *************************************
      * LIEN/TITLE TRAILER RECORD LTFILE   FILE = 19
      *************************************
       01  WK-LIEN-RECORD.
           03  WK-1-LIEN-TYPE        PIC 9.
           03  WK-1-LIEN-NAME        PIC X(30).
           03  WK-1-LIEN-ADR         PIC X(24).
           03  WK-1-LIEN-CITY        PIC X(15).
           03  WK-1-LIEN-STATE       PIC X(2).
           03  WK-1-LIEN-ZIP         PIC X(10).
           03  WK-1-LIEN-DATE        PIC 9(8)    COMP-3.

           03  WK-2-LIEN-TYPE        PIC 9.
           03  WK-2-LIEN-NAME        PIC X(30).
           03  WK-2-LIEN-ADR         PIC X(24).
           03  WK-2-LIEN-CITY        PIC X(15).
           03  WK-2-LIEN-STATE       PIC X(2).
           03  WK-2-LIEN-ZIP         PIC X(10).
           03  WK-2-LIEN-DATE        PIC 9(8)    COMP-3.

           03  WK-3-LIEN-TYPE        PIC 9.
           03  WK-3-LIEN-NAME        PIC X(30).
           03  WK-3-LIEN-ADR         PIC X(24).
           03  WK-3-LIEN-CITY        PIC X(15).
           03  WK-3-LIEN-STATE       PIC X(2).
           03  WK-3-LIEN-ZIP         PIC X(10).
           03  WK-3-LIEN-DATE        PIC 9(8)   COMP-3.

           03  WK-LIEN-AREA-REST.
                05  WK-TITLE-RCV-DATE     PIC 9(8) COMP-3.
                05  WK-TITLE-LIEN-DATE    PIC 9(8) COMP-3.
                05  WK-TITLE-NO           PIC X(20).
                05  WK-LIEN-LETTER-SENT   PIC X.

                05  WK-L-LIEN-COMMENT1    PIC X(60).
                05  WK-L-LIEN-COMMENT2    PIC X(60).
                05  WK-L-LIEN-TITLE-STATE  PIC XX.
                05  WK-L-TITLE-STATUS-CODE PIC XX.
                05  WK-L-TITLE-SEND-FLAG   PIC X.
                05  WK-L-TITLE-NAME        PIC X(25).
                05  WK-L-TITLE-ADR         PIC X(25).
                05  WK-L-TITLE-CITY        PIC X(15).
                05  WK-L-TITLE-STATE       PIC X(2).
                05  WK-L-TITLE-ZIP         PIC X(10).
                05  WK-L-TITLE-DATE        PIC 9(8)   COMP-3.
                05  WK-L-TITLE-SENT-BY     PIC X(25).
                05  WK-L-SEND-ASAP         PIC X.
                05  WK-L-TITLE-SEND-FMNO   PIC 9(4).
                05  WK-L-TITLE-ADR2        PIC X(22).

                05  WK-L-TITLE-COMMENT1    PIC X(40).
                05  WK-L-TITLE-COMMENT2    PIC X(40).

                05  WK-L-RELEASE-TYPE      PIC XX.
                05  WK-L-CARRIER-TYPE      PIC X(4).
                05  WK-L-CARRIER-ACCTNO    PIC X(20).


      *=================================================================
      *   F O R M - B U F F E R
      *-----------------------------------------------------------------
      *   F-B-REC FROM 'REPLACING LEADING' ON FD-REC.
      *=================================================================
       COPY "LIBLP/LP01FD.CPY" REPLACING LEADING "FD" BY "F-B".

      ******************************************************************
       01  SAVE-E-KEYX          PIC X(55)   VALUE SPACES.
       01  BAD-FM-READ          PIC X        VALUE SPACES.

       01  SAVE-LM1-KEY         VALUE SPACES.
           03  SAVE-LM-BRNO     PIC 9(4).
           03  SAVE-LM-ACCTNO   PIC 9(8).
           03  SAVE-LM-DATE     PIC 9(8).
           03  SAVE-LM-SEQ      PIC 9(3).

       01  SAVE-LM-MEMO.
           03  FILLER           PIC X(7)  VALUE "FORM # ".
           03  MEMO-FMNO        PIC 9(4)  VALUE 0.
           03  FILLER           PIC X(3)  VALUE ",  ".
           03  MEMO-DESC        PIC X(25) VALUE SPACES.
           03  MEMO-CHG-CD      VALUE " ".
               05  MEMO-CHG1    PIC X(13).
               05  MEMO-CHG2    PIC ZZZZZ9.99.
           03  MEMO-IDENTIFICATION REDEFINES MEMO-CHG-CD.
               05  FILLER       PIC X(5).
               05  MEMO-TIME    PIC 99/99/99.
               05  FILLER       PIC X.
               05  MEMO-USERID  PIC X(8).

      **********************
      *   LINE WORKERS     *
      **********************
       01  LN-FEED          PIC 999  COMP    VALUE 0.
       01  SAVE-LN-FEED         PIC 99   COMP.

      **********************
      *   MISC WORKERS     *
      **********************
       01  SAVE-F-B-TOP         PIC 9(4) COMP.
       01  ROW                  PIC 9(6) COMP.
       01  D-B-SUB              PIC 9(6) COMP.
       01  FIELD-SUB            PIC 9(6) COMP.
       01  FIRST-FIELD          PIC 9(2).
       01  FILE-CODE            PIC 9(2).
       01  DATA-ERR-FLAG        PIC 9    VALUE 0.
       01  RECORD-EXISTS        PIC X.


       01  LAST-BR-KEY          PIC 9(4)  VALUE 0.
       01  LAST-GB-KEY          PIC 9(4)  VALUE 0.
       01  LAST-FDB1-KEY        PIC X(2)  VALUE " ".   

      ************************************************
      * MADE ALL THE "LAST" KEYS MATCH THEIR TRUE KEY SIZE
      * THESE ARE ONLY USED TO TEST IF THE RECORD NEEDS TO BE READ
      * IN AGAIN DOWN IN THE MAIN LOGIC. 
      *
      * THESE HAVE NOTHING TO DO WITH FM-INITKEY, WHICH WILL NOT HAVE
      * THE BRANCH IN IT AND USED TO GET MOVED TO THE KEYS THAT
      * WERE JUST ACCTNO, CAN NOT DO THAT ANYMORE. 
      *
      * **** DO NOT CHANGE WHETHER NUMERIC OR ALPHA, THAT IS BURIED
      * *** FURTHER DOWN IN THE CODE
      ************************************************
       01  LAST-LN1-KEY         PIC 9(12) COMP VALUE 0.
       01  LAST-BW1-KEY         PIC 9(13) COMP VALUE 0.
       01  LAST-LP1-KEY         PIC X(17) VALUE SPACES.
       01  LAST-DL1-KEY         PIC 9(10) COMP VALUE 0.
       01  LAST-NO1-KEY         PIC 9(12) COMP VALUE 0.
       01  LAST-LT-COLL-KEY     PIC 9(8)  COMP VALUE 0.
       01  LAST-LT-AUTO-KEY     PIC 9(8)  COMP VALUE 0.
       01  LAST-LT-BANKR-KEY    PIC 9(8)  COMP VALUE 0.
       01  LAST-LT-LIEN-KEY     PIC 9(8)  COMP VALUE 0.
       01  LAST-LSX1-KEY        PIC 9(14) COMP VALUE 0.
       01  LAST-CD1-KEY         PIC X(10) VALUE " ".
       01  LAST-ES1-KEY         PIC 9(12) COMP   VALUE 0.
       01  KEY-BR-WK.
           03  INIT-BR          PIC 9(4).

       01  SFT-CT               PIC 9(6) COMP.
       01  SFT-SUB              PIC 9(6) COMP.
       01  SFT-SUB1             PIC 9(6) COMP.

       01  SAVE-SYS-DATE        PIC 9(8)        VALUE 0.
       01  SAVE-LP1-KEY         PIC X(17).
       01  NEXT-FMNO            PIC 9(4)  COMP.
       01  HOLD-FM2-KEY.
           03  HOLD-FM2-BRNO PIC 9(4).
           03  HOLD-FM2-FMNO PIC 9(4).
           03  HOLD-FM2-ZIP  PIC X(15).
       01  HOLD-FM1-KEY         PIC X(28).
       01  HOLD-CD1-KEY         PIC X(10)  VALUE " ".
       01  HOLD-TP-LINE         PIC 9(3).
       01  HOLD-BRANCH          PIC 9(4).

      ***********************************
      *     S C R E E N   F I E L D S   *
      ***********************************
      *01  SCREEN-BUF          VALUE SPACES.
      *    03  ERRCD           PIC X.
      *    03  SYSDATE         PIC 99/99/99.
      *    03  PRINT-REPRINT   PIC X.
      *    03  ALIGNMENT-OK    PIC X.
      *    03  LOAN-KEY        PIC 9(8).
      *    03  BRANCH          PIC 9(4).
      *    03  IP-PROG         PIC XX.
      *    03  IP-FMNO         PIC 999.
      *    03  CONT-PRINT      PIC X.
      *01  SCREEN-LIST.
      *    03  FILLER          PIC X(40) VALUE
      *    "CD SYSDATE PRFG AOK LN BR IP FM CONT.".

      *01  IP2-LIST            PIC X(13) VALUE "FM1 FM2 TYPE.".
      *01  IP2-BUF             VALUE SPACES.
      *    03  IP-BEG-FMNO     PIC 999.
      *    03  IP-BEG-FMNOX   REDEFINES IP-BEG-FMNO
      *                        PIC XXX.
      *    03  IP-END-FMNO     PIC 999.
      *    03  IP-END-FMNOX   REDEFINES IP-END-FMNO
      *                        PIC XXX.
      *    03  IP-FMTYPE       PIC X.

      ***********************************
      *  CHECK ALIGNMENT PRINT BUFFERS  *
      ***********************************
       01  CHECK-LINE-1.
           03  FILLER               PIC X(47) VALUE " ".
           03  FILLER               PIC X(11) VALUE "99/99/99   ".
           03  FILLER               PIC X(7)  VALUE "123456 ".
           03  FILLER               PIC X(15) VALUE "$1,234,567.89  ".
       01  CHECK-LINE-1Y.
           03  FILLER               PIC X(61) VALUE " ".
           03  FILLER               PIC X(08) VALUE "99/99/99".
       01  CHECK-LINE-2.
           03  FILLER               PIC X(47) VALUE " ".
           03  FILLER               PIC X(11) VALUE "99/99/99   ".
           03  FILLER               PIC X(7)  VALUE "123456 ".
           03  FILLER               PIC X(15) VALUE "$1,234,567.89  ".
       01  CHECK-LINE-2Y.
           03  FILLER               PIC X(56) VALUE " ".
           03  FILLER               PIC X(15) VALUE "$00,000,000.00".
       01  CHECK-LINE-3.
           03  FILLER               PIC X(40) VALUE
               "***VOID***VOID***VOID***VOID***VOID***VO".
           03  FILLER               PIC X(40) VALUE
               "ID***VOID***VOID***VOID***VOID***VOID***".
       01  CHECK-LINE-3Y.
           03  FILLER               PIC X(9)  VALUE SPACES.
           03  FILLER               PIC X(27) VALUE
               "***VOID***VOID***VOID***VOI".
       01  CHECK-LINE-4Y.
           03  FILLER               PIC X(9)  VALUE SPACES.
           03  FILLER               PIC X(27) VALUE
               "***VOID***VOID***VOID***VOI".
           03  FILLER               PIC X(17)  VALUE SPACES.
           03  FILLER               PIC X(23) VALUE
               "***VOID***VOID***VOID**".
       01  CHECK-LINE-6.
           03  FILLER               PIC X(13) VALUE " ".
           03  FILLER               PIC X(30) VALUE
               "ZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZ".
           03  FILLER               PIC X(37) VALUE " ".
       01  CHECK-LINE-16.
           03  FILLER               PIC XX    VALUE " ".
           03  FILLER               PIC X(8)  VALUE "DATE    ".
           03  FILLER               PIC X(9)  VALUE "REFER #  ".
           03  FILLER               PIC X(6)  VALUE "REASON".
           03  FILLER               PIC X(48) VALUE " ".
           03  FILLER               PIC X(6)  VALUE "AMOUNT".

      ******************************************************************
       COPY "LIBLP/LPEXTW.CPY".
       COPY "LIBLP/LPEDTW.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBCL/CRNOR2W.CPY".
       COPY "LIBLP/LPPOFFW.CPY".
       COPY "LIBLP/LPXDTEW.CPY".
       COPY "LIBLP/LPMDTEW.CPY".
       COPY "LIBLP/LPIBPCW.CPY".
       COPY "LIBGB/SPPDTHW.CPY".
       COPY "LIBLP/POSTDATEW.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/GPENVW.CPY".

       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

       COPY "LIBGB/GETFMW.CPY".
       COPY "LIBLP/LPLOANW.CPY".
       COPY "LIBLP/LPIPBUF.CPY".
       COPY "LIBGB/GETPRW.CPY".

       PROCEDURE DIVISION
              USING FORM-PATHNAME PROG-BUF IP-BUF PRU-PATH
                    EXIT-PATHNAME.
       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
               PR-FILE.
       COPY "LIBGB/DECLARE.CPY".
           PERFORM CLOSE-TRP1-FILE.
           PERFORM CLOSE-SP1-FILE.
           PERFORM CLOSE-NO1-FILE.
           PERFORM CLOSE-LTP1-FILE.
           PERFORM CLOSE-LTL1-FILE.
           PERFORM CLOSE-LTC1-FILE.
           PERFORM CLOSE-LTB1-FILE.
           PERFORM CLOSE-LTA1-FILE.
           PERFORM CLOSE-LSX1-FILE.
           PERFORM CLOSE-LP1-FILE.
           PERFORM CLOSE-LN1-FILE.
           PERFORM CLOSE-LM1-FILE.
           PERFORM CLOSE-FM2-FILE.
           PERFORM CLOSE-FM1-FILE.
           PERFORM CLOSE-FDB1-FILE.
           PERFORM CLOSE-FD1-FILE.
           PERFORM CLOSE-ES1-FILE.
           PERFORM CLOSE-DL1-FILE.
           PERFORM CLOSE-CD1-FILE.
           PERFORM CLOSE-BW1-FILE.
           PERFORM CLOSE-GB-FILE.
           PERFORM CLOSE-BR-FILE.
           PERFORM CLOSE-PR-FILE.
       COPY "LIBGB/DECLAR2.CPY".

      ******************************************************************
       MAIN-MODULE SECTION.
      ******************************************************************

           PERFORM SQL-CONNECT.

      D    STOP MESS.
           MOVE FORM-PATHNAME TO FORM-PATH.

           PERFORM GET-GPENV.

           PERFORM INITIALIZATION.
           IF ( ERRCD NOT = SPACES )
              IF ( LOAN-KEY = ZEROES )
                 GO TO END-ROUTINE.

       NEXT-FM.
           IF ( LOAN-KEY NOT = ZEROES )
              GO TO ONLY-FM.

           PERFORM READ-FM2-FILE-NEXT.
           IF ( IO-FG = 9 ) OR ( FM-BRNO NOT = FM-PATH-OWNBR )
              GO TO END-ROUTINE.

      *    UNLOCK FM-FILE.

           IF ( FM-FMNO NOT = IP-FMNO )
              IF ( IP-BEG-FMNO NOT = ZEROES ) AND
                 ( IP-END-FMNO NOT = ZEROES )
                 IF ( FM-FMNO < IP-BEG-FMNO ) OR
                    ( FM-FMNO > IP-END-FMNO )
                    GO TO END-ROUTINE
                 ELSE
                    PERFORM TEST-NEXT-FMNO
                    IF ( FD-FMTYPE NOT = IP-FMTYPE )
                       GO TO NEXT-FM
                    ELSE
                       GO TO BYPASS-FM
              ELSE
                 GO TO END-ROUTINE
            ELSE
      * EVEN IF FORM # MATCHES, IT MIGHT NOT BE THE RIGHT FMTYPE
      * FROM WITHIN THE RANGE OF FORMS
              IF ( IP-BEG-FMNO NOT = ZEROES ) AND
                 ( IP-END-FMNO NOT = ZEROES )
                 IF ( FD-FMTYPE NOT = IP-FMTYPE )
                    GO TO NEXT-FM.

           GO TO BYPASS-FM.

       ONLY-FM.

      * IF PRINTING FROM DOCUMENT PRINT SCREEN 7, FIRST TIME THROUGH THIS
      * IS SPACES SO IT WILL PRINT ONCE, THEN IT FILLS IN EDIT-FM-INITKEY
      * FURTHER DOWN, AND WILL NOT BE SPACES NEXT TIME THROUGH HERE,
      * THEREFORE, YOU ONLY PRINT YOUR ONE FORM FOR THE ONE ACCOUNT YOU
      * REQUESTED.
 
           IF ( LPWSFM-EDIT-FM-INITKEY NOT = SPACES )
              GO TO END-ROUTINE.

           IF ( F-B-1STFILE = 2 )
      * LOAN-KEY FOR THE BORROWER FILE IS TRUNCATED BY 1 DIGIT
      * MUST DO READ NEXT TO FIND RIGHT RECORD
              PERFORM FIND-1ST-FM1-RECORD
           ELSE
              MOVE FM-PATH-OWNBR          TO FM-BRNO
              MOVE SPACES                 TO LPWSFM-EDIT-FM-INITKEY
              MOVE LOAN-KEY               TO LPWSFM-EDIT-FM-ACCTNO
              MOVE IP-FMNO                TO LPWSFM-EDIT-FM-FORMNO
                                             FM-FMNO
              MOVE LPWSFM-EDIT-FM-INITKEY TO FM-INITKEY
              PERFORM READ-FM1-FILE.

      *    UNLOCK FM-FILE.

           IF ( IO-FG NOT = 0 ) OR ( FM-BRNO NOT = FM-PATH-OWNBR )
              MOVE SPACES TO FM-PRINT PRINT-REPRINT
              MOVE BRANCH TO FM-BRNO
              MOVE "Y" TO BAD-FM-READ.

       BYPASS-FM.
      D    STOP MESS.
           IF ( BRANCH NOT = ZEROES )
              IF ( FM-BRNO NOT = BRANCH )
                 GO TO NEXT-FM.

           IF FM-BRNO NOT = HOLD-BRANCH
              MOVE FM-BRNO TO HOLD-BRANCH
              PERFORM SETUP-FILE-PATHS.

      * MOVING FM-FMNO TO IP-FMNO TO WRITE THE FORM INFO. TO LM-REC
      * FOR USING FORM NUMBER RANGE.                     KEC 01/10/97

           MOVE FM-FMNO TO IP-FMNO.

           PERFORM INITIAL-FILE.
           IF ( IO-FG = 9 )
              GO TO NEXT-FM.

           IF ( LOAN-KEY = ZEROES )
              IF ( PRINT-REPRINT = "P" )
                 IF ( FM-PRINT = "Y" )
                    GO TO NEXT-FM.

           IF ( LOAN-KEY = ZEROES )
              IF ( PRINT-REPRINT = "R" )
                 IF ( FM-PRINT NOT = "Y" )
                    GO TO NEXT-FM.

      * PROVISION FOR UNLIMITED FORM LENGTH

       NEXT-LINKED-FORM.

           PERFORM PRINT-FORM.

           IF ( NEXT-FMNO = ZEROES )
              MOVE IP-FMNO TO FD-FMNO
              PERFORM GET-FD
              PERFORM CLEAR-F-B-TOP
              GO TO FORM-CHECK-FLAGS.

           MOVE NEXT-FMNO TO FD-FMNO.
           PERFORM GET-FD.
           GO TO NEXT-LINKED-FORM.

       FORM-CHECK-FLAGS.

           IF PRINT-REPRINT = "P"
              IF F-B-FORM-CHG-CD NOT = " "
                 PERFORM UPDATE-FORM-CHARGE.

           IF ( F-B-INCR-DELCNT = "Y" ) 
              IF ( F-B-1STFILE = 1 OR 4 OR 16)
                 PERFORM UPDATE-LOAN-RECORD.

           IF ( F-B-LNMEMO = "Y" )
              IF ( F-B-1STFILE = 1 OR 4  OR 16)
                 PERFORM UPDATE-MEMO-RECORD.

           IF ( F-B-FMTYPE NOT = " " )
              IF ( F-B-1STFILE = 1 OR 4 OR 16)
                    PERFORM UPDATE-LOAN-FORMS.

           IF F-B-1STFILE = 2
              PERFORM UPDATE-BW-RECORD.

           IF ( BAD-FM-READ = "Y" )
              GO TO NEXT-FM.

           MOVE FM-BRNO TO HOLD-FM2-BRNO.
           MOVE FM-FMNO TO HOLD-FM2-FMNO.
           MOVE FM-ZIP  TO HOLD-FM2-ZIP.
           MOVE FM1-KEY TO HOLD-FM1-KEY.
           PERFORM READ-FM1-FILE.
           IF ( IO-FG NOT = ZEROES ) 
              GO TO IO-ERROR.

           MOVE "Y" TO FM-PRINT.
           PERFORM REWRITE-FM1-FILE.

           GO TO NEXT-FM.

      ******************************************************************
       INITIALIZATION SECTION.
      ******************************************************************
       CHECK-RANGES.

      *    IF PRU-LP = "PRU"
      *       MOVE "PIGGY" TO PIGGY-BACK-FLAG.
           IF IP-BEG-FMNOX = SPACES AND IP-END-FMNOX = SPACES
              MOVE 0 TO IP-BEG-FMNO IP-END-FMNO.

           MOVE SPACES TO IP-PROG.

           MOVE 000000 TO Q-POS C-POS P-POS.
           MOVE "N" TO Q-BUF.
           MOVE "00" TO P-BUF.
       
           PERFORM OPEN-PR-FILE.

      *   THIS LOGIC WAS NEEDED BECAUSE TO MANY FILES WERE OPENED
      *   12/13/99 WE WERE READING GB HERE & SAVING RECORD, NOW CROSSING
      *   PATHS SO WILL READ-GB IN NORMAL PLACES - LEFT THIS READ HERE
      *   JUST IN CASE FIELDS NEEDED, E.G. GB-CHECK-PROG

           PERFORM OPEN-GB-FILE.
           MOVE 1             TO GB-NO.
           PERFORM READ-GB-FILE.
           IF IO-FG = 9
              GO TO END-ROUTINE.
           PERFORM CLOSE-GB-FILE.

           MOVE IP-FMNO TO FD-FMNO.
           PERFORM GET-FD.
           MOVE F-B-TOP TO SAVE-F-B-TOP.
           PERFORM CLEAR-F-B-TOP.

           IF IP-AOK = "N"
              PERFORM ALIGNMENT-PRINT
              MOVE " " TO ERRCD
              MOVE "Y" TO IP-AOK
              GO TO END-ROUTINE.

      * GET USERID AND DATE AND TIME
           PERFORM SET-POSTDATE.

           PERFORM OPEN-BR-FILE.
           IF BRANCH NOT = 0
              MOVE BRANCH TO BR-NO
           ELSE
              MOVE POST-USERID-BR TO BR-NO.
           PERFORM READ-BR-FILE.
           IF IO-FG = 9
              GO TO IO-ERROR.
           MOVE BR-NO TO LAST-BR-KEY.
           PERFORM CLOSE-BR-FILE.

           MOVE EXT-CONAME-SYSDATE TO ALP-DATE.
           PERFORM CONVERT-ALPDATE-TO-YYYYMMDD.
           MOVE DATE-YYYYMMDD TO SAVE-SYS-DATE.

           PERFORM OPEN-FM1-FILE.

           MOVE FM-PATH-OWNBR TO FM-BRNO.
           MOVE IP-FMNO       TO FM-FMNO.
           MOVE SPACES        TO FM-ZIP.
    
           PERFORM START-FM2-FILE.

           MOVE HIGH-VALUES TO HOLD-BRANCH.

       END-INITIALIZE.
           IF LOAN-KEY = 0
              MOVE " " TO ERRCD.
       EXIT-INITIALIZE.
           EXIT.


      *********************************************************
       INITIAL-FILE SECTION.
      *********************************************************
           GO TO INIT-LN1-FILE INIT-BW1-FILE
                 INIT-BR-FILE INIT-NO1-FILE
      * 5 LPFILE - NEVER A FIRST FILE  6 DLFILE - NEVER A FIRST FILE
                 INIT-BOGUS-FILE INIT-BOGUS-FILE
                 INIT-07-FILE INIT-08-FILE
                 INIT-GB-FILE INIT-LSX-FILE
                 INIT-BOGUS-FILE INIT-BOGUS-FILE INIT-BOGUS-FILE
                 INIT-ML1-FILE INIT-BANKR-FILE INIT-LN1-FILE
                 INIT-BOGUS-FILE INIT-POOLID-FILE INIT-LIEN-FILE
      * 20 - 29
                 INIT-BOGUS-FILE INIT-BOGUS-FILE INIT-BOGUS-FILE
                 INIT-BOGUS-FILE INIT-BOGUS-FILE INIT-BOGUS-FILE
                 INIT-BOGUS-FILE INIT-BOGUS-FILE INIT-BOGUS-FILE
                 INIT-BOGUS-FILE INIT-BOGUS-FILE
                 INIT-ES1-FILE
                 DEPENDING ON F-B-1STFILE.
           MOVE "FORM DRIVER FILE NOT ESTABLISHED" TO MESS.
           PERFORM CHECK-PIGGY-SEND-MESS.
           MOVE "E" TO ERRCD.
           GO TO END-ROUTINE.
       INIT-BOGUS-FILE.
           MOVE "FILE INACTIVE" TO MESS
           PERFORM CHECK-PIGGY-SEND-MESS.
           MOVE 9 TO IO-FG.
           GO TO EXIT-INITIAL-FILE.
       INIT-LN1-FILE.
      D    STOP MESS.
           MOVE FM-INITKEY            TO LPWSFM-EDIT-FM-INITKEY.
           MOVE LPWSFM-EDIT-FM-ACCTNO TO LN-ACCTNO.
           PERFORM READ-LN1-FILE.
           IF IO-FG = 9
              GO TO INIT-KEY-MISSING.
      *    UNLOCK LN-FILE.
           MOVE LN1-KEY TO LAST-LN1-KEY.
           GO TO EXIT-INITIAL-FILE.
       INIT-BW1-FILE.
           MOVE FM-INITKEY          TO LPWSFM-EDIT-FM-INITKEY.
           MOVE LPWSFM-EDIT-FM-SSNO TO BW-SSNO.
           PERFORM READ-BW1-FILE.
           IF IO-FG = 9
              GO TO INIT-KEY-MISSING.
      *    UNLOCK BW-FILE.
           MOVE BW1-KEY TO LAST-BW1-KEY.
           GO TO EXIT-INITIAL-FILE.
       INIT-BR-FILE.
           PERFORM OPEN-BR-FILE.
           MOVE FM-INITKEY TO KEY-BR-WK.
           IF INIT-BR NOT NUMERIC
              MOVE 9999 TO INIT-BR.
           MOVE INIT-BR TO BR-NO.
           PERFORM READ-BR-FILE.
           IF IO-FG = 9
              GO TO INIT-KEY-MISSING.
           MOVE BR-NO TO LAST-BR-KEY.
           PERFORM CLOSE-BR-FILE.
           GO TO EXIT-INITIAL-FILE.
       INIT-NO1-FILE.
      D    STOP MESS.
           MOVE FM-INITKEY TO LPWSFM-EDIT-FM-INITKEY.
           MOVE LPWSFM-EDIT-FM-ACCTNO TO NO-ACCTNO.
           PERFORM READ-NO1-FILE.
           IF IO-FG = 9
              GO TO INIT-KEY-MISSING.
      *    UNLOCK NO-FILE.
           MOVE NO1-KEY TO LAST-NO1-KEY.
           GO TO EXIT-INITIAL-FILE.
       INIT-07-FILE.
           MOVE "SEVENTH FILE INACTIVE" TO MESS
           PERFORM CHECK-PIGGY-SEND-MESS.
           MOVE 9 TO IO-FG.
           GO TO EXIT-INITIAL-FILE.
       INIT-08-FILE.
           MOVE "EIGHTH FILE INACTIVE" TO MESS
           PERFORM CHECK-PIGGY-SEND-MESS.
           MOVE 9 TO IO-FG.
           GO TO EXIT-INITIAL-FILE.
       INIT-GB-FILE.
           MOVE 1             TO GB-NO.
           PERFORM READ-GB-FILE.
           IF IO-FG = 9
              GO TO INIT-KEY-MISSING.
           MOVE 1 TO LAST-GB-KEY.
           GO TO EXIT-INITIAL-FILE.
       INIT-LSX-FILE.
           MOVE "TENTH FILE INACTIVE" TO MESS.
           PERFORM CHECK-PIGGY-SEND-MESS.
           MOVE 9 TO IO-FG.
           GO TO EXIT-INITIAL-FILE.
       INIT-ML1-FILE.
           GO TO INIT-KEY-MISSING.
       INIT-BANKR-FILE.
           MOVE "BANKRUPT FILE INACTIVE" TO MESS
           PERFORM CHECK-PIGGY-SEND-MESS.
           MOVE 9 TO IO-FG.
           GO TO EXIT-INITIAL-FILE.
       INIT-POOLID-FILE.
           MOVE "POOLID FILE INACTIVE" TO MESS
           PERFORM CHECK-PIGGY-SEND-MESS.
           MOVE 9 TO IO-FG.
           GO TO EXIT-INITIAL-FILE.
       INIT-LIEN-FILE.
           MOVE "LIEN/TITLE FILE INACTIVE" TO MESS
           PERFORM CHECK-PIGGY-SEND-MESS.
           MOVE 9 TO IO-FG.
           GO TO EXIT-INITIAL-FILE.
       INIT-ES1-FILE.
           MOVE FM-INITKEY            TO LPWSFM-EDIT-FM-INITKEY.
           MOVE LPWSFM-EDIT-FM-ACCTNO TO ES-ACCTNO.
           PERFORM READ-ES1-FILE.
           IF IO-FG = 9
              GO TO INIT-KEY-MISSING.
      *    MOVE ES-ACCTNO TO LAST-ES1-KEY.
           MOVE ES1-KEY   TO LAST-ES1-KEY.
           GO TO EXIT-INITIAL-FILE.
       INIT-KEY-MISSING.
           MOVE "INITIAL KEY MISSING, FORM SKIPPED, KEY IS:" TO MESS.
           MOVE E-KEYX TO SAVE-E-KEYX.
           PERFORM CHECK-PIGGY-SEND-MESS.
           MOVE SAVE-E-KEYX TO MESS.
           PERFORM CHECK-PIGGY-SEND-MESS.
           MOVE 9 TO IO-FG.
       EXIT-INITIAL-FILE.
           EXIT.

      *****************************************
       SETUP-FILE-PATHS SECTION.
      *****************************************

           PERFORM OPEN-BR-FILE.
      * CHECK FOR CORPORATE FORM PRINT
           IF HOLD-BRANCH = GP-CORP-BRNO
              MOVE FM-INITKEY TO LPWSFM-EDIT-FM-INITKEY
              MOVE LPWSFM-EDIT-FM-BRNO2 TO BR-NO
           ELSE
              MOVE HOLD-BRANCH TO BR-NO.
           PERFORM READ-BR-FILE.
           PERFORM CLOSE-BR-FILE.
           IF IO-BAD
              GO TO IO-ERROR.

           MOVE EXT-FILPATH-BASE TO FILPATH-BASE.
           MOVE BR-MACHINE       TO FILPATH-MACHINE.
           MOVE BR-DATA-PATH     TO FILPATH-DATA.

           MOVE "B"        TO LN-PATH-OVERRIDE
                              BW-PATH-OVERRIDE
                              NO-PATH-OVERRIDE
                              LP-PATH-OVERRIDE
                              DL-PATH-OVERRIDE
                              LSX-PATH-OVERRIDE
                              LM-PATH-OVERRIDE
                              LTC-PATH-OVERRIDE
                              LTB-PATH-OVERRIDE
                              LTL-PATH-OVERRIDE
                              LTA-PATH-OVERRIDE
                              LTP-PATH-OVERRIDE
                              ES-PATH-OVERRIDE
                              GB-PATH-OVERRIDE.

           PERFORM OPEN-LN1-FILE.
           PERFORM OPEN-BW1-FILE.
           PERFORM OPEN-NO1-FILE.
           PERFORM OPEN-LP1-FILE.
           PERFORM OPEN-DL1-FILE.
           PERFORM OPEN-LSX1-FILE.
           PERFORM OPEN-ES1-FILE.
           PERFORM OPEN-GB-FILE.

      *******************************************************
       PRINT-FORM SECTION.
      *******************************************************

      * PRINT TOP MARGIN:
      * NOTE: THE TOP MARGIN WILL ONLY SPACE AT
      * THIS POINT IF THE FORMS ARE CHAINED AND
      * WE ARE PRINTING OTHER THAN THE 1ST FORM.
           MOVE F-B-TOP TO LN-FEED.
           MOVE SPACES TO EDT-LINE.
           IF LN-FEED NOT = 0
              PERFORM WRITE-FORM-LINE.

      *    WAS 0; (NEEDED FOR G.E. PRINTERS)
           MOVE 1 TO LN-FEED.
           MOVE 1 TO ROW.
           MOVE FIRST-FIELD TO FIELD-SUB.
       PRINT-BODY.
           MOVE F-B-PROMPTS(ROW) TO EDT-LINE.
           IF IP-AOK = "N"
              GO TO PRINT-EDITED-LINE.
       NEXT-FIELD-SET.
           IF FIELD-SUB = 51
              GO TO PRINT-EDITED-LINE.
           IF F-B-FIELDROW(FIELD-SUB) NOT = ROW
              GO TO PRINT-EDITED-LINE.
           PERFORM READ-DATA-FILE.
           IF DATA-ERR-FLAG = 1
              MOVE "Y" TO EDT-CLEAR-FG
              GO TO PRINT-EDIT-FIELD.
           IF DATA-ERR-FLAG = 9
              MOVE E-FILE TO E-FILEX
              MOVE FILE-STAT TO E-CODE
              MOVE ERROR-MSG-HEAD TO EDT-LINE
              GO TO PRINT-EDITED-LINE.
           PERFORM EXTRACT-DATA.
       PRINT-EDIT-FIELD.
           MOVE F-B-FIELDCOL(FIELD-SUB) TO EDT-COL.
           MOVE F-B-FIELDED(FIELD-SUB) TO EDT-EDIT.
           PERFORM EDIT-DATA.
           ADD 1 TO FIELD-SUB.
           GO TO NEXT-FIELD-SET.
       PRINT-EDITED-LINE.
           PERFORM ALPHA-SHIFT.
           PERFORM WRITE-FORM-LINE.
           IF ROW < F-B-BODY
              IF ROW < 30
                 ADD 1 TO ROW
                 GO TO PRINT-BODY.
       PRINT-BOTTOM.
           MOVE F-B-BOTTOM TO LN-FEED.
       PRINT-TOP.
      * INCLUDE TOP MARGIN ONLY IF LAST FORM IN CHAIN.
           IF NEXT-FMNO = 0
              IF CONT-PRINT = "1"
                 ADD SAVE-F-B-TOP TO LN-FEED.
           MOVE SPACE TO EDT-LINE.
      *    (NEEDED FOR G.E. PRINTERS)
           IF LN-FEED NOT = 0
              PERFORM WRITE-FORM-LINE.

      /******************************************
      *      R E A D   D A T A   F I L E        *
      *   ALL DATABASE ERRORS ARE FATAL (THEY   *
      *   PREVENT ANY DOCUMENT FROM PRINTING)   *
      *   AND ARE THERFORE ROUTED TO IO-ERROR.  *
      *   0 OR SPACE KEYS SET DATA-ERR-FLAG TO  *
      *   1 AND RESULT IN SPACES FOR FIELD      *
      *   READ-ERROR SETS DATA-ERR-FLAG TO 9    *
      *   AND RESULTS IN THE FILE-ERRORS BANNER *
      *   REPLACING THAT LINE. ALL NON-ZERO     *
      *   DATA-ERR-FLAGS ARE NON-FATAL          *
      *******************************************

      ************************************************
       READ-DATA-FILE SECTION.
      ************************************************
       GET-KEY-SPECS.
           MOVE 0 TO DATA-ERR-FLAG.
           MOVE F-B-FIELDKEY(FIELD-SUB) TO DATA-FIELD.
           IF DATA-FIELD = 0
              GO TO GET-FIELD-SPECS.
           COMPUTE FILE-CODE = (DATA-FIELD - 1) / 100 + 1.
      * HAD TO ADD THIS CODE BECAUSE TOO MANY FILES WERE OPEN
           PERFORM OPEN-FDB1-FILE.
           PERFORM GET-DATABASE.
           IF IO-FG = 9
              MOVE "KEY FIELD NOT ESTABLISHED" TO MESS
              PERFORM CHECK-PIGGY-SEND-MESS
              GO TO IO-ERROR.
           PERFORM CLOSE-FDB1-FILE.

           PERFORM SET-EXT-WK.
           GO TO KEY-LN1-REC KEY-BW1-REC
                 KEY-BR-REC KEY-NO1-REC
                 KEY-LP1-REC KEY-DL1-REC
                 KEY-F7-REC KEY-F8-REC
                 KEY-GB-REC KEY-LS1-REC
                 KEY-BOGUS-REC KEY-BOGUS-REC KEY-BOGUS-REC
                 KEY-ML1-REC KEY-BANKR-REC KEY-LN1-REC
                 KEY-BOGUS-REC KEY-POOLID-REC KEY-LIEN-REC
      * 20 - 29
                 KEY-BOGUS-REC KEY-BOGUS-REC KEY-BOGUS-REC
                 KEY-BOGUS-REC KEY-BOGUS-REC KEY-BOGUS-REC
                 KEY-BOGUS-REC KEY-BOGUS-REC KEY-BOGUS-REC
                 KEY-BOGUS-REC KEY-BOGUS-REC
                 KEY-ES1-REC
                 DEPENDING FILE-CODE.
           MOVE "KEY CODE UNDEFINED" TO MESS.
           PERFORM CHECK-PIGGY-SEND-MESS.
           MOVE "E" TO ERRCD.
           GO TO END-ROUTINE.
       KEY-BOGUS-REC.
           MOVE SPACES TO DATA-AREA.
           GO TO KEY-EXTRACT.
       KEY-LN1-REC.
           MOVE LN-REC TO DATA-AREA.
           GO TO KEY-EXTRACT.
       KEY-BW1-REC.
           MOVE BW-REC TO DATA-AREA.
           GO TO KEY-EXTRACT.
       KEY-BR-REC.
           MOVE BR-REC TO DATA-AREA.
           GO TO KEY-EXTRACT.
       KEY-NO1-REC.
           MOVE NO-REC TO DATA-AREA.
           GO TO KEY-EXTRACT.
       KEY-LP1-REC.
           MOVE LP-REC TO DATA-AREA.
           GO TO KEY-EXTRACT.
       KEY-DL1-REC.
           MOVE DL-REC TO DATA-AREA.
           GO TO KEY-EXTRACT.
       KEY-F7-REC.
           MOVE WK-LT-RECORD TO DATA-AREA.
           GO TO KEY-EXTRACT.
       KEY-F8-REC.
           MOVE WK-AUTO-INS TO DATA-AREA.
           GO TO KEY-EXTRACT.
       KEY-GB-REC.
           MOVE GB-REC TO DATA-AREA.
           GO TO KEY-EXTRACT.
       KEY-LS1-REC.
           MOVE LSX-REC TO DATA-AREA.
           GO TO KEY-EXTRACT.
       KEY-ML1-REC.
           GO TO KEY-EXTRACT.
       KEY-BANKR-REC.
           MOVE WK-BANKR-RECORD TO DATA-AREA.
           GO TO KEY-EXTRACT.
       KEY-POOLID-REC.
           MOVE CD-REC TO DATA-AREA.
           GO TO KEY-EXTRACT.
       KEY-LIEN-REC.
           MOVE WK-LIEN-RECORD TO DATA-AREA.
           GO TO KEY-EXTRACT.
       KEY-ES1-REC.
           MOVE ES-REC TO DATA-AREA.
           GO TO KEY-EXTRACT.
       KEY-EXTRACT.
           PERFORM EXTRACT-DATA.

      ********************************
      *   FIELD SPECIFICATIONS       *
      ********************************
       GET-FIELD-SPECS.
           MOVE F-B-FIELDNO(FIELD-SUB) TO DATA-FIELD.
           COMPUTE FILE-CODE = (DATA-FIELD - 1) / 100 + 1.
      * HAD TO ADD THIS CODE BECAUSE TOO MANY FILES WERE OPEN
           PERFORM OPEN-FDB1-FILE.
           PERFORM GET-DATABASE.
           IF IO-FG = 9
              MOVE "DATA FIELD NOT ESTABLISHED" TO MESS
              PERFORM CHECK-PIGGY-SEND-MESS
              GO TO IO-ERROR.
           PERFORM CLOSE-FDB1-FILE.
      *
           PERFORM SET-EXT-WK.
           IF F-B-FIELDKEY(FIELD-SUB) = 0
              GO TO LN1-NO-KEY BW1-NO-KEY
                    BR-NO-KEY NO1-NO-KEY
                    LP1-NO-KEY DL1-NO-KEY
                    F7-NO-KEY F8-NO-KEY
                    GB-NO-KEY LS1-NO-KEY
                    BOGUS-NO-KEY BOGUS-NO-KEY BOGUS-NO-KEY
                    ML1-NO-KEY BANKR-NO-KEY LN1-NO-KEY
                    BOGUS-NO-KEY POOLID-NO-KEY LIEN-NO-KEY
      * 20 - 29
                    BOGUS-NO-KEY BOGUS-NO-KEY BOGUS-NO-KEY
                    BOGUS-NO-KEY BOGUS-NO-KEY BOGUS-NO-KEY
                    BOGUS-NO-KEY BOGUS-NO-KEY BOGUS-NO-KEY
                    BOGUS-NO-KEY BOGUS-NO-KEY ES1-NO-KEY
                 DEPENDING FILE-CODE.

           GO TO GET-LN1-FILE GET-BW1-FILE
                 GET-BR-FILE GET-NO1-FILE
                 GET-LP1-FILE GET-DL1-FILE
                 GET-F7-FILE GET-F8-FILE
                 GET-GB-FILE GET-LS1-FILE
                 GET-BOGUS-FILE GET-BOGUS-FILE GET-BOGUS-FILE
                 GET-ML1-FILE GET-BANKR-FILE GET-LN1-FILE
                 GET-BOGUS-FILE GET-POOLID-FILE GET-LIEN-FILE
      * 20 - 29
                 GET-BOGUS-FILE GET-BOGUS-FILE GET-BOGUS-FILE
                 GET-BOGUS-FILE GET-BOGUS-FILE GET-BOGUS-FILE
                 GET-BOGUS-FILE GET-BOGUS-FILE GET-BOGUS-FILE
                 GET-BOGUS-FILE GET-BOGUS-FILE GET-ES1-FILE
                 DEPENDING FILE-CODE.
           MOVE "FILE CODE UNDEFINED" TO MESS.
           PERFORM CHECK-PIGGY-SEND-MESS.
           MOVE "E" TO ERRCD.
           GO TO END-ROUTINE.
       GET-BOGUS-FILE.
           MOVE SPACES TO DATA-AREA.
           GO TO EXIT-READ-DATA-FILE.
       GET-LN1-FILE.
           IF DATA-NUMOUT = 0
              MOVE 1 TO DATA-ERR-FLAG
              GO TO EXIT-READ-DATA-FILE.
           IF DATA-NUMOUT = LAST-LN1-KEY
              MOVE LN-REC TO DATA-AREA
              GO TO EXIT-READ-DATA-FILE.
           MOVE DATA-NUMOUT TO LN-ACCTNO LAST-LN1-KEY.
           PERFORM READ-LN1-FILE.
      *    UNLOCK LN-FILE.
           IF IO-FG = 9
              MOVE 0 TO LAST-LN1-KEY
              MOVE 9 TO DATA-ERR-FLAG
              GO TO EXIT-READ-DATA-FILE.
           MOVE LN-REC TO DATA-AREA.
           GO TO EXIT-READ-DATA-FILE.
       GET-BW1-FILE.
           IF DATA-NUMOUT = 0
              MOVE 1 TO DATA-ERR-FLAG
              GO TO EXIT-READ-DATA-FILE.
           IF DATA-NUMOUT = LAST-BW1-KEY
              MOVE BW-REC TO DATA-AREA
              GO TO EXIT-READ-DATA-FILE.
           MOVE DATA-NUMOUT TO BW-SSNO LAST-BW1-KEY.
           PERFORM READ-BW1-FILE.
      *    UNLOCK BW-FILE.
           IF IO-FG = 9
              MOVE 0 TO LAST-BW1-KEY
              MOVE 9 TO DATA-ERR-FLAG
              GO TO EXIT-READ-DATA-FILE.
           MOVE BW-REC TO DATA-AREA.
           GO TO EXIT-READ-DATA-FILE.
       GET-BR-FILE.
           IF DATA-NUMOUT = 0
              MOVE 1 TO DATA-ERR-FLAG
              GO TO EXIT-READ-DATA-FILE.
           IF DATA-NUMOUT = LAST-BR-KEY
              MOVE BR-REC TO DATA-AREA
              GO TO EXIT-READ-DATA-FILE.
           PERFORM OPEN-BR-FILE.
           MOVE DATA-NUMOUT TO BR-NO LAST-BR-KEY.
           PERFORM READ-BR-FILE.
           IF IO-FG = 9
              MOVE 0 TO LAST-BR-KEY
              MOVE 9 TO DATA-ERR-FLAG
              GO TO EXIT-READ-DATA-FILE.
           MOVE BR-REC TO DATA-AREA.
           PERFORM CLOSE-BR-FILE.
           GO TO EXIT-READ-DATA-FILE.
       GET-NO1-FILE.
           IF DATA-NUMOUT = 0
              MOVE 1 TO DATA-ERR-FLAG
              GO TO EXIT-READ-DATA-FILE.
           IF DATA-NUMOUT = LAST-NO1-KEY
              MOVE NO-REC TO DATA-AREA
              GO TO EXIT-READ-DATA-FILE.
           MOVE DATA-NUMOUT TO NO-ACCTNO LAST-NO1-KEY.
           PERFORM READ-NO1-FILE.
      *    UNLOCK NO-FILE.
           IF IO-FG = 9
              MOVE 0 TO LAST-NO1-KEY
              MOVE 9 TO DATA-ERR-FLAG
              GO TO EXIT-READ-DATA-FILE.
           MOVE NO-REC TO DATA-AREA.
           GO TO EXIT-READ-DATA-FILE.
       GET-LP1-FILE.
           IF A-BUF = " "
              MOVE 1 TO DATA-ERR-FLAG
              GO TO EXIT-READ-DATA-FILE.
           IF A-BUF = LAST-LP1-KEY
              MOVE LP-REC TO DATA-AREA
              GO TO EXIT-READ-DATA-FILE.
           MOVE A-BUF TO LP1-KEY LAST-LP1-KEY.
           PERFORM READ-LP1-FILE.
      *    UNLOCK LP-FILE.
           IF IO-FG = 9
              MOVE " " TO LAST-LP1-KEY
              MOVE 9 TO DATA-ERR-FLAG
              GO TO EXIT-READ-DATA-FILE.
           MOVE LP-REC TO DATA-AREA.
           GO TO EXIT-READ-DATA-FILE.
       GET-DL1-FILE.
           IF DATA-NUMOUT = 0
              MOVE 1 TO DATA-ERR-FLAG
              GO TO EXIT-READ-DATA-FILE.
           IF DATA-NUMOUT = LAST-DL1-KEY
              MOVE DL-REC TO DATA-AREA
              GO TO EXIT-READ-DATA-FILE.
           MOVE DATA-NUMOUT TO DL-NUM LAST-DL1-KEY.
           PERFORM READ-DL1-FILE.
           IF IO-FG = 9
              MOVE 0 TO LAST-DL1-KEY
              MOVE 9 TO DATA-ERR-FLAG
              GO TO EXIT-READ-DATA-FILE.
           MOVE DL-REC TO DATA-AREA.
           GO TO EXIT-READ-DATA-FILE.
       GET-F7-FILE.
           IF DATA-NUMOUT = 0
              MOVE 1 TO DATA-ERR-FLAG
              GO TO EXIT-READ-DATA-FILE.
           IF DATA-NUMOUT = LAST-LT-COLL-KEY
              MOVE WK-LT-RECORD TO DATA-AREA
              GO TO EXIT-READ-DATA-FILE.
           PERFORM GET-LT-COLL.
           IF RECORD-EXISTS = "N"
              MOVE 0 TO LAST-LT-COLL-KEY
              MOVE 9 TO DATA-ERR-FLAG
              GO TO EXIT-READ-DATA-FILE.
           MOVE WK-LT-RECORD TO DATA-AREA.
           GO TO EXIT-READ-DATA-FILE.
       GET-F8-FILE.
           IF DATA-NUMOUT = 0
              MOVE 1 TO DATA-ERR-FLAG
              GO TO EXIT-READ-DATA-FILE.
           IF DATA-NUMOUT = LAST-LT-AUTO-KEY
              MOVE WK-AUTO-INS TO DATA-AREA
              GO TO EXIT-READ-DATA-FILE.
           PERFORM GET-LT-AUTO.
           IF RECORD-EXISTS = "N"
              MOVE 0 TO LAST-LT-AUTO-KEY
              MOVE 9 TO DATA-ERR-FLAG
              GO TO EXIT-READ-DATA-FILE.
           MOVE WK-AUTO-INS TO DATA-AREA.
           GO TO EXIT-READ-DATA-FILE.
       GET-GB-FILE.
           MOVE 1             TO GB-NO.
           PERFORM READ-GB-FILE.
           IF IO-FG = 9
              MOVE 0 TO LAST-GB-KEY
              MOVE 9 TO DATA-ERR-FLAG
              GO TO EXIT-READ-DATA-FILE.
           MOVE GB-REC TO DATA-AREA.
           GO TO EXIT-READ-DATA-FILE.
       GET-LS1-FILE.
           IF DATA-NUMOUT = 0
              MOVE 1 TO DATA-ERR-FLAG
              GO TO EXIT-READ-DATA-FILE.
           IF DATA-NUMOUT = LAST-LSX1-KEY
              MOVE LSX-REC TO DATA-AREA
              GO TO EXIT-READ-DATA-FILE.
           MOVE DATA-NUMOUT TO LSX-ACCTNO LAST-LSX1-KEY.
           MOVE 1           TO LSX-SEQNO.
           PERFORM READ-LSX1-FILE.
           IF IO-FG = 9
              MOVE 0 TO LAST-LSX1-KEY
              MOVE 9 TO DATA-ERR-FLAG
              GO TO EXIT-READ-DATA-FILE.
           MOVE LSX-REC TO DATA-AREA.
           GO TO EXIT-READ-DATA-FILE.
       GET-ML1-FILE.
           MOVE 1 TO DATA-ERR-FLAG.
           GO TO EXIT-READ-DATA-FILE.
       GET-BANKR-FILE.
           IF DATA-NUMOUT = 0
              MOVE 1 TO DATA-ERR-FLAG
              GO TO EXIT-READ-DATA-FILE.
           IF DATA-NUMOUT = LAST-LT-BANKR-KEY
              MOVE WK-BANKR-RECORD TO DATA-AREA
              GO TO EXIT-READ-DATA-FILE.
           PERFORM GET-LT-BANKRUPT.
           IF RECORD-EXISTS = "N"
              MOVE 0 TO LAST-LT-BANKR-KEY
              MOVE 9 TO DATA-ERR-FLAG
              GO TO EXIT-READ-DATA-FILE.
           MOVE WK-BANKR-RECORD TO DATA-AREA.
           GO TO EXIT-READ-DATA-FILE.
       GET-POOLID-FILE.
           IF A-BUF = " "
              MOVE 1 TO DATA-ERR-FLAG
              GO TO EXIT-READ-DATA-FILE.
           IF A-BUF = LAST-CD1-KEY
              MOVE CD-REC TO DATA-AREA
              GO TO EXIT-READ-DATA-FILE.
           PERFORM OPEN-CD1-FILE.
           MOVE "PL"      TO CD-TYPE.
           MOVE LN-DLPOOL TO CD-PL-POOLID.
           MOVE SPACES    TO CD-PL-FILLER.
           PERFORM READ-CD1-FILE.
           PERFORM CLOSE-CD1-FILE.
           MOVE CD1-KEY TO LAST-CD1-KEY.
           IF IO-FG = 9
              MOVE 0 TO LAST-CD1-KEY
              MOVE 9 TO DATA-ERR-FLAG
              GO TO EXIT-READ-DATA-FILE.
           MOVE CD-REC TO DATA-AREA.
           GO TO EXIT-READ-DATA-FILE.
       GET-LIEN-FILE.
           IF DATA-NUMOUT = 0
              MOVE 1 TO DATA-ERR-FLAG
              GO TO EXIT-READ-DATA-FILE.
           IF DATA-NUMOUT = LAST-LT-LIEN-KEY
              MOVE WK-LIEN-RECORD TO DATA-AREA
              GO TO EXIT-READ-DATA-FILE.
           PERFORM GET-LT-LIEN.
           IF RECORD-EXISTS = "N"
              MOVE 0 TO LAST-LT-LIEN-KEY
              MOVE 9 TO DATA-ERR-FLAG
              GO TO EXIT-READ-DATA-FILE.
           MOVE WK-LIEN-RECORD TO DATA-AREA.
           GO TO EXIT-READ-DATA-FILE.
       GET-ES1-FILE.
           IF DATA-NUMOUT = 0
              MOVE 1 TO DATA-ERR-FLAG
              GO TO EXIT-READ-DATA-FILE.
           IF DATA-NUMOUT = LAST-ES1-KEY
              MOVE ES-REC TO DATA-AREA
              GO TO EXIT-READ-DATA-FILE.
           MOVE DATA-NUMOUT TO ES-ACCTNO LAST-ES1-KEY.
           PERFORM READ-ES1-FILE.
      *    UNLOCK ES-FILE.
           IF IO-FG = 9
              MOVE 0 TO LAST-ES1-KEY
              MOVE 9 TO DATA-ERR-FLAG
              GO TO EXIT-READ-DATA-FILE.
           MOVE ES-REC TO DATA-AREA.
           GO TO EXIT-READ-DATA-FILE.

      ***********************************
      *   THESE ROUTINES ARE USED WHEN  *
      *   NO KEY IS PROVIDED (KEY=  0 ) *
      ***********************************
       BOGUS-NO-KEY.
           MOVE SPACES TO DATA-AREA.
           GO TO EXIT-READ-DATA-FILE.
       LN1-NO-KEY.
           MOVE LN-REC TO DATA-AREA.
           GO TO EXIT-READ-DATA-FILE.
       BW1-NO-KEY.
           MOVE BW-REC TO DATA-AREA.
           GO TO EXIT-READ-DATA-FILE.
       BR-NO-KEY.
           MOVE BR-REC TO DATA-AREA.
           GO TO EXIT-READ-DATA-FILE.
       NO1-NO-KEY.
           MOVE NO-REC TO DATA-AREA.
           GO TO EXIT-READ-DATA-FILE.
       LP1-NO-KEY.
           MOVE LP-REC TO DATA-AREA.
           GO TO EXIT-READ-DATA-FILE.
       DL1-NO-KEY.
           MOVE DL-REC TO DATA-AREA.
           GO TO EXIT-READ-DATA-FILE.
       F7-NO-KEY.
           GO TO EXIT-READ-DATA-FILE.
       F8-NO-KEY.
           GO TO EXIT-READ-DATA-FILE.
       GB-NO-KEY.
           MOVE GB-REC TO DATA-AREA.
           GO TO EXIT-READ-DATA-FILE.
       LS1-NO-KEY.
           GO TO EXIT-READ-DATA-FILE.
       ML1-NO-KEY.
           GO TO EXIT-READ-DATA-FILE.
       BANKR-NO-KEY.
           GO TO EXIT-READ-DATA-FILE.
       POOLID-NO-KEY.
           GO TO EXIT-READ-DATA-FILE.
       LIEN-NO-KEY.
           GO TO EXIT-READ-DATA-FILE.
       ES1-NO-KEY.
           MOVE ES-REC TO DATA-AREA.
           GO TO EXIT-READ-DATA-FILE.
       EXIT-READ-DATA-FILE.
           EXIT.

      ************************************************
       WRITE-FORM-LINE SECTION.
      ************************************************
       WRITE-FORM-LN.
           IF LN-FEED > 15
              MOVE SPACE TO PR-REC
              MOVE LN-FEED TO SAVE-LN-FEED
              MOVE 15 TO LN-FEED
              PERFORM WRITE-PR-REC
              SUBTRACT 15 FROM SAVE-LN-FEED GIVING LN-FEED
              GO TO WRITE-FORM-LN.
           IF CONT-PRINT NOT = "1"
              MOVE EDT-LINE-BUFFER TO PR-REC-CUTSHEET
            ELSE
              MOVE EDT-LINE-BUFFER TO PR-REC.
           PERFORM WRITE-PR-REC.
           MOVE SPACE TO EDT-LINE-BUFFER PR-REC.
           MOVE 1 TO LN-FEED.

      **********************************************
       ALIGNMENT-PRINT SECTION.
      **********************************************

           IF FD-PROG = "M2"
              MOVE 0 TO F-B-TOP
              MOVE 26 TO F-B-BOTTOM
              MOVE 16 TO F-B-BODY
              MOVE CHECK-LINE-1 TO F-B-PROMPTS(1)
              MOVE CHECK-LINE-3 TO F-B-PROMPTS(3)
              MOVE CHECK-LINE-6 TO F-B-PROMPTS(6) F-B-PROMPTS(7)
                                   F-B-PROMPTS(8) F-B-PROMPTS(9)
              MOVE CHECK-LINE-16 TO F-B-PROMPTS(16)
              MOVE SPACES TO EDT-LINE-BUFFER PR-REC
              MOVE 1 TO ROW LN-FEED
              GO TO ALIGN-PR-BODY.
           IF FD-PROG = "2Y"
              MOVE 0 TO F-B-TOP
              MOVE 26 TO F-B-BOTTOM
              MOVE 16 TO F-B-BODY
              MOVE CHECK-LINE-1Y TO F-B-PROMPTS(1)
              MOVE CHECK-LINE-2Y TO F-B-PROMPTS(5)
              MOVE CHECK-LINE-3Y TO F-B-PROMPTS(11) F-B-PROMPTS(12)
              MOVE CHECK-LINE-4Y TO F-B-PROMPTS(13)
              MOVE SPACES TO EDT-LINE-BUFFER PR-REC
              MOVE 1 TO ROW LN-FEED
              GO TO ALIGN-PR-BODY.
           MOVE SPACE TO EDT-LINE.
           MOVE F-B-TOP TO LN-FEED.
           PERFORM WRITE-FORM-LINE.
           MOVE 1 TO ROW.
       ALIGN-PR-BODY.
           IF FD-PROG = "MC"
              IF ROW > 30
                 GO TO ALIGN-PR-BOTTOM.
           IF FD-PROG = "MC" OR FD-PROG = GB-CHECK-PROG
              MOVE F-B-PROMPTS(ROW) TO EDT-LINE
           ELSE
              MOVE SPACES TO EDT-LINE
              INSPECT EDT-LINE REPLACING ALL " " BY "X".
           PERFORM WRITE-FORM-LINE.
           IF ROW < F-B-BODY
              ADD 1 TO ROW
              GO TO ALIGN-PR-BODY.
       ALIGN-PR-BOTTOM.
           MOVE SPACE TO EDT-LINE.
           MOVE F-B-BOTTOM TO LN-FEED.
           PERFORM WRITE-FORM-LINE.

      ***********************************
       UPDATE-FORM-CHARGE SECTION.
      ***********************************
           MOVE "FC"            TO CD-TYPE.
           MOVE F-B-FORM-CHG-CD TO CD-CODE.
           PERFORM GET-CD.
           IF IO-FG = 9
              GO TO UPDATE-FORM-CHARGE-EXIT.

           MOVE FM-INITKEY            TO LPWSFM-EDIT-FM-INITKEY.
           MOVE LPWSFM-EDIT-FM-ACCTNO TO LN-ACCTNO.
           PERFORM READ-LN1-FILE.
           IF IO-FG = 9
              GO TO IO-ERROR.

      * DO NOT ADD CHARGES TO A PAID OUT ACCOUNT !!! PL# 550
           IF LN-CURBAL = 0
              GO TO UPDATE-FORM-CHARGE-EXIT.

           ADD CD-FC-FEEAMT TO LN-OTHBAL
           PERFORM REWRITE-LN1-FILE.

           PERFORM UPDATE-NOTICE-FILE.

           PERFORM UPDATE-NOTICE-FILE.

           PERFORM GET-SP.
           IF IO-FG NOT = 0
              PERFORM IO-ERROR
              GO TO UPDATE-FORM-CHARGE-ERR.

      * GET LAST LP RECORD:
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
           PERFORM READ-LP1-FILE-PREVIOUS.
      *    UNLOCK LP-FILE.
           IF IO-FG = 9 OR (LP-BRNO NOT = LP-PATH-OWNBR) OR
                            (LP-ACCTNO NOT = LN-ACCTNO)
              GO TO UPDATE-FORM-CHARGE-ERR.

       UPDATE-FORM-CHARGE-LP-NEXT.
           ADD 1 TO LP-SEQNO.
           MOVE LP1-KEY TO SAVE-LP1-KEY.
           PERFORM READ-LP1-FILE.
      *    UNLOCK LP-FILE.
           IF IO-FG = 0
              IF LP-SEQNO NOT = 99999
                 GO TO UPDATE-FORM-CHARGE-LP-NEXT
              ELSE
                 GO TO UPDATE-FORM-CHARGE-ERR.

           PERFORM CLEAR-LP-FILE.
           MOVE SAVE-LP1-KEY TO LP1-KEY.

           PERFORM SETUP-LP-FORM-CHARGE.

           PERFORM WRITE-LP1-FILE.
           IF IO-FG = 9
              IF E-CODE = "22"
                 GO TO UPDATE-FORM-CHARGE-LP-NEXT
              ELSE
                 GO TO UPDATE-FORM-CHARGE-ERR.

      *--------------------------------------------
      *    CREATE TRP RECORD:
      *--------------------------------------------
           MOVE "Y"              TO TRP-PATH-OVERRIDE.
           MOVE EXT-FILPATH-BASE TO TRP-ALL-BASE.
           MOVE BR-MACHINE       TO TRP-ALL-MACHINE.
           MOVE BR-DATA-PATH     TO TRP-ALL-DATA.
           MOVE TRP-ALL-PATH     TO TRP-PATH.

           PERFORM OPEN-TRP1-FILE.

           MOVE FM-BRNO          TO TP-BRNO.
           MOVE SAVE-SYS-DATE    TO TP-DATE.
      *    MOVE "P"              TO TP-CODE.
      *    MOVE 0                TO TP-POOLID.
           MOVE LN-CLASS         TO TP-CLASS.
           MOVE LN-ACCTNO        TO TP-ACCTNO.
           MOVE 0                TO HOLD-TP-LINE.
       UPDATE-FORM-CHARGE-TP-EXISTS.
           ADD 1             TO HOLD-TP-LINE.
           MOVE HOLD-TP-LINE TO TP-LINE.

           MOVE TRP1-KEY TO LPWSTRP-TRP1-KEY.
           PERFORM READ-TRP1-FILE.
           IF IO-FG = 0
      *       UNLOCK TRP-FILE
              GO TO UPDATE-FORM-CHARGE-TP-EXISTS.

           PERFORM CLEAR-TRP-FILE.
           MOVE LPWSTRP-TRP1-KEY TO TRP1-KEY.

           PERFORM SETUP-TRP-FORM-CHARGE.

           PERFORM WRITE-TRP1-FILE.
           IF IO-FG = 9
              GO TO UPDATE-FORM-CHARGE-ERR.

           PERFORM CLOSE-TRP1-FILE.

           GO TO UPDATE-FORM-CHARGE-EXIT.

       UPDATE-FORM-CHARGE-ERR.
           MOVE "LP RECORD NOT WRITTEN,OUT OF LP-SEQNO'S" TO MESS.
           PERFORM CHECK-PIGGY-SEND-MESS.

       UPDATE-FORM-CHARGE-EXIT.
           EXIT.


      **************************************************
       UPDATE-NOTICE-FILE SECTION.
      **************************************************
           MOVE LN-OWNBR  TO NO-BRNO.
           MOVE LN-ACCTNO TO NO-ACCTNO CRNOR2-ACCTNO.
           PERFORM READ-NO1-FILE.

           MOVE SAVE-SYS-DATE TO CRNOR2-SYSDATE.
           MOVE "CL/CRNOR2"    TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH CRNOR2-LINK NO-REC
                                 POFF-WORKERS LN-REC.
           CANCEL FORM-PROGX.

           MOVE LN-OWNBR  TO NO-BRNO.
           MOVE LN-ACCTNO TO NO-ACCTNO.
           IF IO-FG = 0
              PERFORM REWRITE-NO1-FILE
           ELSE
              PERFORM WRITE-NO1-FILE.

      **************************************************
       SETUP-LP-FORM-CHARGE SECTION.
      **************************************************

      * SETUP LP-REC RE: FORM CHARGE 'OT'

           MOVE SAVE-SYS-DATE TO LP-TRDATE LP-PAYDATE.
           MOVE "OT" TO LP-TRCD.
           STRING "FM" F-B-FMNO DELIMITED BY SIZE INTO LP-REFNO.
           MOVE POST-USERID TO LP-USERID.
           MOVE POST-TIME TO LP-POSTTIME.
           MOVE POST-DATE TO LP-POSTDATE.
           MOVE LN-COLLECTOR TO LP-COLLECTOR.

           MOVE SPACES TO LP-TILLNO.

           MOVE SAVE-SYS-DATE TO IBPC-DATE.
           PERFORM IBPC-TEST.
           MOVE IBPC-FG TO LP-IBPC.

           MOVE LN-1STPYDATE TO PDTH-DATE-WORK.
           PERFORM PAID-THRU-CALCULATION.
           MOVE PDTH-DATE-FULL TO LP-PDTH-DATE.
           MOVE LN-LCPDTH-DATE TO LP-LCPDTH-DATE.

           MOVE LN-SSNO(1) TO LP-SSNO.

           MOVE CD-FC-FEEAMT TO LP-INTDUE.
           MOVE LN-OTHBAL TO LP-OTHBAL.
           MOVE LN-OT2BAL TO LP-OT2BAL.
           MOVE LN-INTBAL TO LP-INTBAL.
           MOVE LN-LCBAL TO LP-LCBAL.
           MOVE LN-CURBAL TO LP-CURBAL.

      **************************************************
       SETUP-TRP-FORM-CHARGE SECTION.
      **************************************************

      * SETUP TP-REC RE: FORM CHARGE 'OT'

           MOVE FM-BRNO TO TP-POSTING-BR.

           MOVE LN-SSNO(1) TO TP-SSNO.
           MOVE LN-MAKERCD(1) TO TP-MAKER.

           MOVE LN-INSTYPES TO TP-INSTYPES.
           MOVE LN-INSURED TO TP-INSURED.
           MOVE LN-DLNO TO TP-DLNO.
           MOVE LN-UNITPER-CD TO TP-UNITPER-CD.
           MOVE LN-UNITPER-FREQ TO TP-UNITPER-FREQ.

           MOVE LN-LOANTYPE TO TP-LOANTYPE.
           MOVE LN-SALEFIN TO TP-SALEFIN.
           MOVE LN-SERVACCT TO TP-SERVACCT.

           MOVE LN-LAWCODE TO TP-LAWCODE.

           MOVE LN-PLCD TO TP-PLCD.
           MOVE LN-PLTYPE TO TP-PLTYPE.

           MOVE LP-PAYDATE TO TP-PAYDATE
                              TP-DUEDATE.
           MOVE LP-TRAMT TO TP-TRAMT.
           MOVE "OT" TO TP-TRCD.
           MOVE LP-INTDUE TO TP-INTDUE.
           MOVE LP-APOTH TO TP-APOTH.
           MOVE LP-APOT2 TO TP-APOT2.
           MOVE LP-APINT TO TP-APINT.
           MOVE LP-APLC TO TP-APLC.
           MOVE LP-APCUR TO TP-APCUR.

           MOVE LP-OTHBAL TO TP-OTHBAL.
           MOVE LP-OT2BAL TO TP-OT2BAL.
           MOVE LP-INTBAL TO TP-INTBAL.
           MOVE LP-LCBAL TO TP-LCBAL.
           MOVE LP-CURBAL TO TP-CURBAL.
           MOVE LP-PDTH-DATE TO TP-PDTH-DATE.
           MOVE LP-LCPDTH-DATE TO TP-LCPDTH-DATE.

           MOVE POST-USERID TO TP-USERID.
           MOVE POST-TIME TO TP-POSTTIME.
           MOVE POST-DATE TO TP-POSTDATE.
           MOVE LN-COLLECTOR TO TP-COLLECTOR.
           MOVE LN-ACTIONCD2 TO TP-ACTIONCD2.

           MOVE LP-TILLNO TO TP-TILLNO.

           MOVE LP-IBPC TO TP-IBPC.

      ****************************
       UPDATE-LOAN-RECORD SECTION.
      ****************************
           MOVE FM-INITKEY            TO LPWSFM-EDIT-FM-INITKEY.
           MOVE LPWSFM-EDIT-FM-ACCTNO TO LN-ACCTNO.
           PERFORM READ-LN1-FILE.
           IF IO-FG = 9
              GO TO IO-ERROR.

           IF F-B-FMTYPE = "S"
              ADD 1 TO LN-SOLCNT
           ELSE
              IF F-B-FMTYPE = "C"
                 ADD 1 TO LN-DELCNT2.

           PERFORM REWRITE-LN1-FILE.


      ****************************
       UPDATE-BW-RECORD SECTION.
      ****************************
           MOVE FM-INITKEY          TO LPWSFM-EDIT-FM-INITKEY.
           MOVE LPWSFM-EDIT-FM-SSNO TO BW-SSNO.
           PERFORM READ-BW1-FILE.
           IF IO-FG = 9
              GO TO IO-ERROR.

           MOVE SAVE-SYS-DATE TO BW-LAST-FORM-DATE.
           MOVE IP-FMNO TO BW-LAST-FORM-NO.
           PERFORM REWRITE-BW1-FILE.

      ******************************************************************
       UPDATE-MEMO-RECORD SECTION.
      ******************************************************************

           PERFORM OPEN-LM1-FILE.

           MOVE SPACES         TO LM-REC.

           MOVE LM-PATH-OWNBR         TO SAVE-LM-BRNO.
           MOVE FM-INITKEY            TO LPWSFM-EDIT-FM-INITKEY.
           MOVE LPWSFM-EDIT-FM-ACCTNO TO SAVE-LM-ACCTNO.
           COMPUTE SAVE-LM-DATE = 99999999 - SAVE-SYS-DATE
           MOVE 1                    TO SAVE-LM-SEQ.

       CREATE-MEMO.
           MOVE SAVE-LM1-KEY TO LM1-KEY.
           PERFORM READ-LM1-FILE.
           IF ( IO-FG = 0 )
              IF ( LM-SEQ < 999 )
                 ADD 1 TO SAVE-LM-SEQ
                 GO TO CREATE-MEMO
              ELSE
                 MOVE "MEMO NOT CREATED" TO MESS
                 PERFORM CHECK-PIGGY-SEND-MESS
                 GO TO UPDATE-MEMO-EXIT.

           MOVE SPACES       TO LM-REC.
           MOVE SAVE-LM1-KEY TO LM1-KEY.
           MOVE F-B-FMNO     TO MEMO-FMNO.
           MOVE F-B-DESC     TO MEMO-DESC.
           MOVE " "          TO MEMO-CHG-CD.
           IF PRINT-REPRINT = "P"
              IF F-B-FORM-CHG-CD = " " OR LN-CURBAL = 0
                 MOVE POST-USERID TO MEMO-USERID
                 MOVE POST-TIME TO MEMO-TIME
                 INSPECT MEMO-TIME REPLACING ALL "/" BY ":"
              ELSE
                 MOVE "FC"            TO CD-TYPE
                 MOVE F-B-FORM-CHG-CD TO CD-CODE
                 PERFORM GET-CD
                 IF IO-FG = 0
                    MOVE ", FORM CHG: $"  TO MEMO-CHG1
                    MOVE CD-FC-FEEAMT     TO MEMO-CHG2.

           MOVE SAVE-LM-MEMO TO LM-MEMO.
           PERFORM WRITE-LM1-FILE.

       UPDATE-MEMO-EXIT.
           PERFORM CLOSE-LM1-FILE.


      ***********************************
       UPDATE-LOAN-FORMS SECTION.
      ***********************************
           MOVE FM-INITKEY            TO LPWSFM-EDIT-FM-INITKEY.
           MOVE LPWSFM-EDIT-FM-ACCTNO TO LN-ACCTNO.
           PERFORM READ-LN1-FILE.
           IF IO-FG = 9
              GO TO IO-ERROR.

           IF F-B-FMTYPE = "C"
              MOVE SAVE-SYS-DATE TO LN-LASTCFORMDATE
              MOVE IP-FMNO TO LN-LASTCFORMNO
           ELSE
              IF F-B-FMTYPE = "S"
                 MOVE SAVE-SYS-DATE TO LN-LASTSFORMDATE
                 MOVE IP-FMNO TO LN-LASTSFORMNO.
           PERFORM REWRITE-LN1-FILE.

      ************************************
       GET-FD SECTION.
      ************************************

      * FIELDS ARE SORTED BY ROW/COL IN FDMAIN.
      * 00/00 ROW/COL APPEAR AT LOW OFFSETS AND ARE SKIPPED.
           PERFORM OPEN-FD1-FILE.
           PERFORM READ-FD1-FILE.
           IF IO-FG = 9
              MOVE "FORM# NOT LOCATED" TO MESS
              PERFORM CHECK-PIGGY-SEND-MESS
              MOVE "?" TO ERRCD
              MOVE 0 TO LOAN-KEY
              GO TO END-ROUTINE.

           MOVE FD-REC TO F-B-REC.
           MOVE FD-NEXTFM TO NEXT-FMNO.
           MOVE 1 TO FIRST-FIELD.
       FIND-1ST-FIELD.
           IF F-B-FIELDROW(FIRST-FIELD) NOT = 0
              GO TO EXIT-FIND-FIRST-FIELD.

           ADD 1 TO FIRST-FIELD.
           IF FIRST-FIELD < 51
              GO TO FIND-1ST-FIELD.

       EXIT-FIND-FIRST-FIELD.
           PERFORM CLOSE-FD1-FILE.

       COPY "LIBGB/GPENV.CPY".
       COPY "LIBLP/LPEXT.CPY".
       COPY "LIBLP/LPEDT.CPY".
       COPY "LIBLP/LPMSC.CPY".
       COPY "LIBLP/POSTDATE.CPY".
       COPY "LIBGB/DATER.CPY".
       COPY "LIBLP/LPMDTE.CPY".
       COPY "LIBLP/LPXDTE.CPY".
       COPY "LIBLP/LPIBPC.CPY".
       COPY "LIBGB/SPPDTH.CPY".


      **************************************************
       FIND-1ST-FM1-RECORD SECTION.
      **************************************************

      * THE LOAN-KEY FOR A BORROWER FILE FORM IS TRUNCATED BY 1
      * DO A READ NEXT UNTIL RECORD IS FOUND

           MOVE SPACES TO BAD-FM-READ FM-REC LPWSFM-EDIT-FM-INITKEY.

           MOVE LOAN-KEY TO LPWSFM-EDIT-FM-ACCTNO.

           MOVE FM-PATH-OWNBR          TO FM-BRNO.
           MOVE LPWSFM-EDIT-FM-INITKEY TO FM-INITKEY.
           MOVE 0                      TO FM-FMNO.

           PERFORM START-FM1-FILE.
       FIND-1ST-FM-NEXT.
           PERFORM READ-FM1-FILE-NEXT.
           IF IO-FG NOT = 0 OR (FM-BRNO NOT = FM-PATH-OWNBR)
              MOVE "Y" TO BAD-FM-READ
              GO TO EXIT-FIND-1ST-FM.

           IF FM-FMNO NOT = IP-FMNO
      *       UNLOCK FM-FILE
              GO TO FIND-1ST-FM-NEXT.

           MOVE FM-INITKEY TO LPWSFM-EDIT-FM-INITKEY.

           MOVE 0 TO LPWSFM-EDIT-FM-FORMNO.

           IF LPWSFM-EDIT-FM-ACCTNO NOT = LOAN-KEY
      *       UNLOCK FM-FILE
              GO TO FIND-1ST-FM-NEXT.

       EXIT-FIND-1ST-FM.
           EXIT.

      *************************
       GET-LT-AUTO SECTION.
      *************************
           MOVE "N" TO RECORD-EXISTS.

           PERFORM OPEN-LTA1-FILE.

           MOVE BR-NO       TO LTA-BRNO.
           MOVE DATA-NUMOUT TO LTA-ACCTNO LAST-LT-AUTO-KEY.
           MOVE 1           TO LTA-SEQNO.
           PERFORM READ-LTA1-FILE.
      *    UNLOCK LTA-FILE.
           IF IO-FG NOT = 0
              MOVE SPACES TO WK-AUTO-INS
              MOVE 0 TO WK-A-PHONE
                        WK-A-EFF-DATE
                        WK-A-EXP-DATE
                        WK-A-CAN-DATE
                        WK-A-CONF-DATE
              MOVE 0 TO WK-A-BIL-COV
                        WK-A-PDL-COV
                        WK-A-PIP-COV
                        WK-A-UMB-COV
                        WK-A-COL-COV
           ELSE
              MOVE "Y" TO RECORD-EXISTS
              MOVE LTA-AUTO-RECORD TO WK-AUTO-INS.


      *************************
       GET-LT-COLL SECTION.
      *************************
           MOVE "N" TO RECORD-EXISTS.

           PERFORM OPEN-LTC1-FILE.
           MOVE BR-NO       TO LTC-BRNO.
           MOVE DATA-NUMOUT TO LTC-ACCTNO LAST-LT-COLL-KEY.
           MOVE 1           TO LTC-SEQNO.
           PERFORM READ-LTC1-FILE.
      *    UNLOCK LTC-FILE.
           IF IO-FG NOT = 0
              MOVE SPACES TO WK-AUTO-1
              MOVE 0 TO WK-MILEAGE-1 WK-VALUE-1
                        WK-SELLPR-1 WK-MISC-AMT-1
                        WK-VIN-CHG-DATE-1
              MOVE SPACES TO WK-AUTO-2
              MOVE 0 TO WK-MILEAGE-2 WK-VALUE-2
                        WK-SELLPR-2 WK-MISC-AMT-2
                        WK-VIN-CHG-DATE-2
              MOVE SPACES TO WK-HHGS-1
              MOVE SPACES TO WK-HHGS-2
           ELSE
              MOVE "Y" TO RECORD-EXISTS
              MOVE LTC-AUTO(1) TO WK-AUTO-1
              MOVE LTC-AUTO(2) TO WK-AUTO-2
              MOVE LTC-AUTO(3) TO WK-AUTO-3
              MOVE LTC-HHGS(1) TO WK-HHGS-1
              MOVE LTC-HHGS(2) TO WK-HHGS-2
              MOVE LTC-HHGS(3) TO WK-HHGS-3.

      *************************
       GET-LT-BANKRUPT SECTION.
      *************************
           MOVE "N" TO RECORD-EXISTS.

           PERFORM OPEN-LTB1-FILE.

           MOVE BR-NO       TO LTB-BRNO.
           MOVE DATA-NUMOUT TO LTB-ACCTNO LAST-LT-BANKR-KEY.
           MOVE 1           TO LTB-SEQNO.
           PERFORM READ-LTB1-FILE.
      *    UNLOCK LTB-FILE.
           IF IO-FG NOT = 0
              MOVE SPACES TO WK-BANKR-RECORD
              MOVE 0 TO WK-B-FILEDATE WK-B-CHAPTER
                        WK-B-BDATE WK-B-CLAIM-AMT
                        WK-B-INT-LC-DUE WK-B-OTH-DUE WK-B-TDUEAMT
              MOVE 0 TO WK-B-PHONE WK-B-COURT-PHONE
              MOVE 0 TO WK-B-TRUST-PHONE
           ELSE
              MOVE "Y" TO RECORD-EXISTS
              MOVE LTB-BANKRUPT-AREA TO WK-BANKR-RECORD.


      *************************
       GET-LT-LIEN SECTION.
      *************************
           MOVE "N" TO RECORD-EXISTS.

           PERFORM OPEN-LTL1-FILE.
           MOVE BR-NO       TO LTL-BRNO.
           MOVE DATA-NUMOUT TO LTL-ACCTNO LAST-LT-LIEN-KEY.
           MOVE 1           TO LTL-SEQNO.
           PERFORM READ-LTL1-FILE.
      *    UNLOCK LTL-FILE.
           IF IO-FG NOT = 0
               MOVE SPACES TO WK-LIEN-RECORD
               MOVE 0 TO WK-1-LIEN-TYPE WK-1-LIEN-DATE
               MOVE 0 TO WK-2-LIEN-TYPE WK-2-LIEN-DATE
               MOVE 0 TO WK-3-LIEN-TYPE WK-3-LIEN-DATE
                         WK-TITLE-RCV-DATE WK-TITLE-LIEN-DATE
               MOVE 0 TO WK-L-TITLE-DATE WK-L-TITLE-SEND-FMNO
           ELSE
              MOVE "Y" TO RECORD-EXISTS
              MOVE LTL-LIEN-TYPE(1) TO WK-1-LIEN-TYPE
              MOVE LTL-LIEN-NAME(1) TO WK-1-LIEN-NAME
              MOVE LTL-LIEN-ADR(1) TO WK-1-LIEN-ADR
              MOVE LTL-LIEN-CITY(1) TO WK-1-LIEN-CITY
              MOVE LTL-LIEN-STATE(1) TO WK-1-LIEN-STATE
              MOVE LTL-LIEN-ZIP(1) TO WK-1-LIEN-ZIP
              MOVE LTL-LIEN-DATE(1) TO WK-1-LIEN-DATE
              MOVE LTL-LIEN-TYPE(2) TO WK-2-LIEN-TYPE
              MOVE LTL-LIEN-NAME(2) TO WK-2-LIEN-NAME
              MOVE LTL-LIEN-ADR(2) TO WK-2-LIEN-ADR
              MOVE LTL-LIEN-CITY(2) TO WK-2-LIEN-CITY
              MOVE LTL-LIEN-STATE(2) TO WK-2-LIEN-STATE
              MOVE LTL-LIEN-ZIP(2) TO WK-2-LIEN-ZIP
              MOVE LTL-LIEN-DATE(2) TO WK-2-LIEN-DATE
              MOVE LTL-LIEN-TYPE(3) TO WK-3-LIEN-TYPE
              MOVE LTL-LIEN-NAME(3) TO WK-3-LIEN-NAME
              MOVE LTL-LIEN-ADR(3) TO WK-3-LIEN-ADR
              MOVE LTL-LIEN-CITY(3) TO WK-3-LIEN-CITY
              MOVE LTL-LIEN-STATE(3) TO WK-3-LIEN-STATE
              MOVE LTL-LIEN-ZIP(3) TO WK-3-LIEN-ZIP
              MOVE LTL-LIEN-DATE(3) TO WK-3-LIEN-DATE
              MOVE LTL-LIEN-AREA-REST TO WK-LIEN-AREA-REST.

      ***********************************
       MISC-PARAGRAPHS SECTION.
      ***********************************
       COPY "LIBGB/GETPR.CPY".
       COPY "LIBGB/GETENV.CPY".

       GET-SP.
           MOVE LN-ORGST    TO SP-ORGST.
           MOVE LN-SPRCLASS TO SP-SPRCLASS.
           MOVE LN-SUBCLASS TO SP-SUBCLASS.
           MOVE LN-LAWCODE  TO SP-LAWCODE.
           IF LPWSSP-SP1-KEY NOT = SP1-KEY
              PERFORM OPEN-SP1-FILE
              PERFORM READ-SP1-FILE
              PERFORM CLOSE-SP1-FILE
              IF IO-FG  = 0
                 MOVE SP1-KEY TO LPWSSP-SP1-KEY
              END-IF
           ELSE
              MOVE 0 TO IO-FG.
       GET-CD.
           IF HOLD-CD1-KEY NOT = CD1-KEY
              PERFORM OPEN-CD1-FILE
              PERFORM READ-CD1-FILE
              PERFORM CLOSE-CD1-FILE
              IF IO-FG = 0
                 MOVE CD1-KEY TO HOLD-CD1-KEY
              END-IF
           ELSE
              MOVE 0 TO IO-FG.

      * READ FD AND TEST IF CORRECT FORM TYPE, FIRST
       TEST-NEXT-FMNO.
           MOVE FM-FMNO TO FD-FMNO.
           PERFORM GET-FD.
           MOVE F-B-TOP TO SAVE-F-B-TOP.
           PERFORM CLEAR-F-B-TOP.
       SET-EXT-WK.
           MOVE FDB-DOFFSET(D-B-SUB) TO DATA-OFFSET.
           MOVE FDB-DDIGITS(D-B-SUB) TO DATA-DIGITS.
           MOVE FDB-DTYPE(D-B-SUB) TO DATA-TYPE.
           MOVE FDB-DDECIMAL(D-B-SUB) TO DATA-DECIMAL.
       CLEAR-F-B-TOP.
           MOVE 0 TO F-B-TOP.
       CHECK-PIGGY-SEND-MESS.
           IF PRU-LP = "PRU"
              PERFORM CLOSE-PR-FILE
              PERFORM PRU-RESET.
           PERFORM SEND-MESS.
           IF PRU-LP = "PRU"
              PERFORM OPEN-PR-FILE.

       COPY "LIBLP/LPLPCL.CPY".
       COPY "LIBLP/LPTRPCL.CPY".

      *********************************************
       IO-ROUTINE SECTION.
      *********************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBLP/LPTRPGS_SQL.CPY".
       COPY "LIBLP/LPSPGS_SQL.CPY".
       COPY "LIBLP/LPNOGS_SQL.CPY".
       COPY "LIBLP/LPLTPGS_SQL.CPY".
       COPY "LIBLP/LPLTLGS_SQL.CPY".
       COPY "LIBLP/LPLTCGS_SQL.CPY".
       COPY "LIBLP/LPLTBGS_SQL.CPY".
       COPY "LIBLP/LPLTAGS_SQL.CPY".
       COPY "LIBLP/LPLSXGS_SQL.CPY".
       COPY "LIBLP/LPLPGS_SQL.CPY".
       COPY "LIBLP/LPLNGS_SQL.CPY".
       COPY "LIBLP/LPLMGS_SQL.CPY".
       COPY "LIBLP/LPFMGS_SQL.CPY".
       COPY "LIBLP/LPFDBGS_SQL.CPY".
       COPY "LIBLP/LPFDGS_SQL.CPY".
       COPY "LIBLP/LPESGS_SQL.CPY".
       COPY "LIBLP/LPDLGS_SQL.CPY".
       COPY "LIBLP/LPCDGS_SQL.CPY".
       COPY "LIBLP/LPBWGS_SQL.CPY".
       COPY "LIBGB/GBGBGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBPRINT.CPY".
       COPY "LIBGB/GBGBRN.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBLP/LPDL1RN.CPY".
       COPY "LIBLP/LPFD1RN.CPY".
       COPY "LIBLP/LPFDB1RN.CPY".
       COPY "LIBLP/LPBW1IN.CPY".
       COPY "LIBLP/LPTRP1IN.CPY".
       COPY "LIBLP/LPLN1IN.CPY".
       COPY "LIBLP/LPES1IN.CPY".
       COPY "LIBLP/LPLP1IN.CPY".
       COPY "LIBLP/LPFM1IN.CPY".
       COPY "LIBLP/LPFM2RN.CPY".
       COPY "LIBLP/LPNO1IN.CPY".
       COPY "LIBLP/LPLM1IN.CPY".
       COPY "LIBLP/LPLTA1RN.CPY".
       COPY "LIBLP/LPLTC1RN.CPY".
       COPY "LIBLP/LPLTB1RN.CPY".
       COPY "LIBLP/LPLTL1RN.CPY".
       COPY "LIBLP/LPLTP1RN.CPY".
       COPY "LIBLP/LPCD1RN.CPY".
       COPY "LIBLP/LPSP1RN.CPY".
       COPY "LIBLP/LPLSX1RN.CPY".
       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO ERRCD.

      ***********************************
       END-ROUTINE SECTION.
      ***********************************
       END-RUN.
           PERFORM CLOSE-FILES.
           PERFORM PRU-RESET.
       EXIT-PROG.
       STOP-RUN.
           EXIT PROGRAM.
