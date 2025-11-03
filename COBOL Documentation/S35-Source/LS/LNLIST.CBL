       IDENTIFICATION DIVISION.
       PROGRAM-ID.      LNLIST.
       DATE-WRITTEN. 02-10-84
      **************************************************************************
      *
      *                      (LS)
      *    DESCRIPTION:  LOAN MASTER FILE LIST
      *
      *****************************************************
      *  *** CURRENT BRANCH ONLY, DOES NOT CROSS DATA PATHS
      *****************************************************
      *
      *REV:
      * SLC 043095 ADDED PLTYPE LOGIC.
      * MJD 062295 ADDED REPO CODE (LN-REPOCD).
      * JTG 072095 ADDED LN-PRLNAMT & LN-PRPDUE-DAYS
      * KEC 082595 ADDED LN-MTD-PRIN, LC, INT, OT, AND EX.
      * KEC 082595 ADDED LN-MONTHEND-BAL.
      * KEC 083095 ADDED LN-BNKRPT-CHAPTER.
      * JFF 101995 ADDED CLASS SCAN
      * MJD 042696 ADDED GP-FILE, GP-DLR-DISP-TYPE.
      * JTG 061496 ADDED PRINT OF LN-SPRCLASS
      * MJD 072996 ADDED LOGIC TO PRINT LN-1STPYDATE AS MM/DDWYY
      *            FOR WEEKLY UNIT PERIODS.
      * GLF 072996 ADDED TO PG2: REDEEM DATE AND SOLD REPO DATE;
      *            ALSO CHANGED RATE CHAGED DATE TO INTEREST
      *            REDUCED DATE. ADDED TO PAGE 4: INS TYPES O3,
      *            04, AMD 05.
      * GLF 073196 ADDED TO PAGE 3: PAYMENT EVERY __________ TO
      *            MESSAGE, ADDED TO PADE 4: DLADJ/DLCOMM, AND
      *            ADDED TO PAGE 8: PRIOR LOAN AMOUNT AND PRIOR
      *            PAST DUE DAYS.
      * JFF 101696 ADDED IB ASSESSED THRU, IB PRINCIPAL BAL,
      *            IB INT. OWING TO PAGE 6.
      * KEC 111496 CHANGED LN-POCD FOR RESCISSION.
      * MJD 121096 CHANGED TO PERFORM LPSTAT-DISPLAY AFTER READING BWFILE.
      *            REMOVED REFERENCE TO LN-O6-REBAT, LN-O7-REBAT.
      * KEC 042397 ADDED LN-APPNO TO PAGE 9.
      * MJD 082697 ADDED LN-RECPROFILE(1..5) AND LN-CONPROFILE(1..5)
      *            CHANGED UP ARROW TEST FROM "18" TO "1U"
      * GLF 090997 REPLACED 'IO' ON GP-FILE WITH GET 'GPENV'
      * BAH 091297 CHANGED SPREFUNDATE TO TRW-1ST-30
      * BAH 062998 TILLNO IS NOW ALPHANUMERIC
      * BAH 081098 ADDED LN-YTDNOALDEL FOR MERCURY'S NEW EDIT ON ALLOWABLE
      *            DELINQUENCY, #193
      * MJD 083198 ADDED LN-PRDLDISC-UNEARN.
      * JTG 080898 ADDED PRINT OF LN-REVOLVING & LN-GIFTNET
      * CS  102098 ADDED PRINT OF PAGE 12 (REVOLVING)
      * JTG 121698 DISPLAYED 'R' VIA UNITPER-MASK LOGIC FOR REVOLVING ACCOUNT
      * GLF 020199 ADDED TO PAGE 8: FIELDS 31, 32, 33
      * GLF 082799 ADJUSTED PAGE 11, FIELD 9-16 TO MATCH CHANGES MADE TO
      *            MAINTENANCE SCREEN PER JTG
      *  CS 991018 ADDED LN-CREATION-STATUS, LN-ACTIVE-FG(PAGE 11)
      * MJD 991221 ADDED LN-TOTNODEFACT AND LN-YTDNODEFACT (WORLD PR#216)
      * MJD 000119 ADDED LN-TOTNOLCHG (WORLD PR#218)
      * MJD 000403 ADDED LN-PRDLDISC-UNEARN AND LN-LAST-ACCRUAL
      * MJD 000912 ADDED PRINT OF LN-LBOX (MFC PR#227)
      *  CS 010124 ADDED LN-LMO-PRIOR-INTEREST(MFC PR#240)
      *  MG 010228 ADDED LN-EXTRA-PRIN-LFP, LN-REMAIN-INT-SINCE-LFP AND
      *            LN-REMAIN-PRIN-SINCE-LFP PER JOHN & KEN.
      * JTG 010507 ALLOWED ENTRY OF 'TRADEIN' RE: SALE PRICE-DP-TI PAACO PR#1467
      * BAH 011029 ADDED LN-INTERIM-COUPON, REGACC PR# 1632
      *            ADDED THE REST OF PAGE 10 THAT WAS MISSING !!
      * BAH 011210 ADDED LN-PROMID, REGACC PR# 1662
      * JTG 020123 EXPANDED PAYMNTD FIELDS TO 1,000,000        LENDMARK PR#J021
      * BAH 020218 ADDED LN-LAST-PYPA-DATE FOR REGACC PR# 1721
      * JTG 020530 CHANGED FOR (MIP) MONTHLY INSURANCE PREMIUMS   REGENCY #1801
      *             ADDED  LN-MIP-FG         LN-MIP-MTD-CLAHPAID,
      *                    LN-MIP-CLPDTH-DATE,    LN-MIP-TOT-CLPAID
      *                    LN-MIP-AHPDTH-DATE     LN-MIP-TOT-AHPAID
      * BAH 020625 ADDED LN-PLREASON REGACC PR# 1775
      * BAH 021016 WAS NOT PRINTING LN-FEECODES CORRECTLY !! TFC PL# 361
      * BAH 021017 WAS ONLY PRINTING LN-PROCEEDS FOR SALESFIN, SUPPORT PL#363
      * BAH 050601 CHANGED LN-TRW-1ST-30 TO LN-TRW-DOFD-DATE, DATE OF FIRST
      *            DELINQUENCY
      * BAH 051025 REMOVED LN-LEDGERCNT, LN-LPCNT,  NOT USED....
      * AMM 070227 {PL#573} JUDY; FIXED FIELDS 18 AND 24 ON PG 4
      * BAH 160118 SPLIT OUT LTFILE, PD0003
      *
      * KEC 2016.0315 {PD#00003} PARADATA <A30> [JAY CISZEWSKI]
      *         CHANGED CD-FEE???? TO CD-GC-????.
      *
      * KEC 2016.0509 {PD#00003} PARADATA <A30> [JAY CISZEWSKI]
      *         CHANGED LN-APPNO TO USE LN-APP-NUMBER FOR MA1-KEY.
      *
      * KEC 2016.1115 {PD#00003} PARADATA <A30> [JAY CISZEWSKI]
      *         ADDED LTR-SEQNO TO LTR1-KEY.
      *
      * BAH 170504 ACTIVATE 8 DIGIT DATES, PD0006
      * BAH 180306 FIXED PRINTING OF LN-LASTCFORMNO, LORI
      * BAH 181211 GLOBAL LNFILE
      * BAH 2019-0625 REMOVED LN-ZERO-BAL-CNT AND LN-REVOLVING PT0153
      * BAH 2019-0627 REMOVED LN-REVOLVING, PT0153
      * BAH 2019-0703 REMOVED LTR PT0150
      * JKC 2022-0222 AUTO-POPULATE BEG-BRANCH & END-BRANCH WITH USER'S
      *               BRANCH SINCE THIS PROGRAM ONLY EXECUTES USING
      *               THE USER'S BRANCH.
      * BAH 20220805 HARD CODED START
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
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)LS/LNLIST.CBL S35 02/21/24-12:02:04 >".
      ****************************
      *     F I L E   I F N      *
      ****************************
       COPY "LIBLP/LP01LN.CPY".
       COPY "LIBLP/LP01LN_SQL.CPY".
       COPY "LIBLP/LPWSLN.CPY".
       COPY "LIBLP/LP01BW.CPY".
       COPY "LIBLP/LP01BW_SQL.CPY".
       COPY "LIBLP/LPWSBW.CPY".
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBLP/LP01SP.CPY".
       COPY "LIBLP/LP01SPA.CPY".
       COPY "LIBLP/LP01SPB.CPY".
       COPY "LIBLP/LP01SPC.CPY".
       COPY "LIBLP/LP01SP_SQL.CPY".
       COPY "LIBLP/LPWSSP.CPY".
       COPY "LIBLP/LP01CDB.CPY".
       COPY "LIBLP/LP01CDB_SQL.CPY".
       COPY "LIBLP/LPWSCDB.CPY".
       COPY "LIBLP/LP01LTC.CPY".
       COPY "LIBLP/LP01LTC_SQL.CPY".
       COPY "LIBLP/LPWSLTC.CPY".
       01  PR-IFN               PIC XX      VALUE "PR".


      **********************
      *   LINE WORKERS     *
      **********************
       01  LN-FEED          PIC 99       VALUE 0.
       01  LN-COUNT         PIC 99       VALUE 41.
       01  PAGE-NUMBER          PIC 9(4)     VALUE 0.

      **********************
      *   MISC WORKERS     *
      **********************
       01  SUB                  PIC 9(6)     COMP-3 VALUE 0.
       01  FIRST-TIME           PIC X        VALUE "Y".
       01  MESS-LIST            PIC X(5)     VALUE "MESS.".

       01  HOLD-LN-OWNBR       PIC 9(4)      VALUE ZERO.
       01  CLASS-FG            PIC X(3)      VALUE "   ".
           88  CLASS-BEG                     VALUE "BEG".
           88  CLASS-END                     VALUE "END".

      ***********************************
      *     S C R E E N   F I E L D S   *
      ***********************************
       01  SCREEN-BUF          VALUE " ".
           03  ERRCD           PIC X.
           03  SEQ             PIC 9.
           03  BEG-KEY.
               05  BEG-BRANCH  PIC 9(4).
               05  BEG-CLASS   PIC 99.
               05  BEG-ACCT    PIC 9(6).
           03  END-KEY.
               05  END-BRANCH  PIC 9(4).
               05  END-CLASS   PIC 99.
               05  END-ACCT    PIC 9(6).

       01  WORK-KEY.
           03  WORK-BRANCH   PIC 9(4).
           03  WORK-CLASS    PIC 99.
           03  WORK-NUMBER   PIC 9(6).
       01  ACCEPT-BUF      PIC X    VALUE " ".

      ***********************************
      *     P R I N T   B U F F E R S   *
      ***********************************
       01  TEXT2A.
           03  FILLER          PIC X(48) VALUE
           "    CO OURS EFF DATE EXP DATE   TERM  COVERAGE".
           03  FILLER          PIC X(16) VALUE
           "PREMIUM    COMM.".
       01  TEXT2B.
           03  FILLER          PIC X(54) VALUE
           "CD DESCRIPTION        AMOUNT O  CD DESCRIPTION".
           03  FILLER          PIC X(9) VALUE
           "AMOUNT O".
       01  TEXT3A.
           03  FILLER          PIC X(50) VALUE
           " ORIG ANTIC - ADJ ANTIC = NET ANTIC  - EARNINGS".
           03  FILLER          PIC X(23) VALUE
           "= UNEARNED    ..UPFRONT".
       01  TEXT3B.
           03  FILLER          PIC X(50) VALUE
           "  --------- - --------- = ---------- - ----------".
           03  FILLER          PIC X(23) VALUE
           "= ----------  ---------".

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
           03  FILLER           PIC X(41)    VALUE
           "- - - LOAN MASTER FILE LISTING - - -".
           03  FILLER           PIC X(45)    VALUE " ".

       01  HEAD3.
           03  FILLER      PIC X(8)     VALUE "LOAN NO.".
           03  FILLER      PIC X        VALUE " ".
           03  D-LOAN-NUM  PIC X(16).
           03  FILLER      PIC X.
           03  D-TITLE     PIC X(26).
           03  FILLER      PIC X.
           03  FILLER      PIC X(7) VALUE "PAGE : ".
           03  D-PAGE      PIC 9.
           03  FILLER      PIC X(27)    VALUE " ".
           03  D-TITLE2    PIC X(26).
           03  FILLER      PIC X.
           03  FILLER      PIC X(7) VALUE "PAGE : ".
           03  D-PAGE2     PIC 9.

       01  HEAD4.
           03  FILLER         PIC X(8)     VALUE "LOAN NO.".
           03  FILLER         PIC X        VALUE " ".
           03  D2-LOAN-NUM    PIC X(16).
           03  FILLER         PIC X.
           03  D2-TITLE       PIC X(21).
           03  FILLER         PIC X.
           03  FILLER         PIC X(7) VALUE "PAGE : ".
           03  D2-PAGE        PIC ZZ.
           03  FILLER         PIC X(31)    VALUE " ".
           03  D2-TITLE2      PIC X(26).
           03  FILLER         PIC X.
           03  FILLER         PIC X(7) VALUE "PAGE : ".
           03  D2-PAGE2       PIC ZZ.

       01  HEAD5.
           03  FILLER         PIC X(8)     VALUE "LOAN NO.".
           03  FILLER         PIC X        VALUE " ".
           03  D3-LOAN-NUM    PIC X(16).
           03  FILLER         PIC X(23).
           03  FILLER         PIC X(7)     VALUE "PAGE : ".
           03  D3-PAGE        PIC ZZ.
           03  FILLER         PIC X(31)    VALUE " ".
           03  D3-TITLE2      PIC X(26).
           03  FILLER         PIC X(18).

       01  HEAD-10.
           03  FILLER           PIC X(40) VALUE " ".
           03  FILLER           PIC X(11) VALUE "DESCRIPT".
           03  FILLER           PIC X(16) VALUE "MAKE".
           03  FILLER           PIC X(16) VALUE "MODEL".
           03  FILLER           PIC X(5)  VALUE "YEAR".
           03  FILLER           PIC X(9)  VALUE "STYLE".
           03  FILLER           PIC X(9)  VALUE "CLASS".
           03  FILLER           PIC X(18) VALUE "IDENTIFICATION NO ".

       01  DASH-LINE            PIC X(132) VALUE ALL "-".

       01  DETAIL-LINE          PIC X(132) VALUE SPACES.

       01  D-LINE1 REDEFINES DETAIL-LINE.
      * COLUMN 1
           03  D-COLUMN1A.
               05  D-P1A                  PIC X(14).
               05  D-FIELD1AA             PIC X(18).
               05  D-F-1A REDEFINES D-FIELD1AA.
                   07  D-1A-STATE         PIC XX.
                   07  D-LAWCODE REDEFINES D-1A-STATE.
                       09  D-1A-LAWCODE   PIC 99.
                   07  D-1A-DASH          PIC X.
                   07  D-1A-OWNBR         PIC X(4).
                   07  FILLER             PIC X(11).
               05  D-F-1AB REDEFINES D-FIELD1AA.
                   07  D-1A-SSNO          PIC 999/99/9999.
                   07  FILLER             PIC XX.
                   07  D-1A-MAKERCD       PIC X.
                   07  FILLER             PIC X(4).
      * COLUMN 2
           03  D-COLUMN1B.
               05  D-P1B              PIC X(11).
               05  D-FIELD1B          PIC X(25).
               05  D-F-1B REDEFINES D-FIELD1B.
                   07  D-1B-STATE     PIC XX.
                   07  D-1B-DASH      PIC X.
                   07  D-1B-OWNBR     PIC X(4).
                   07  FILLER         PIC X(18).
               05  D-F-1BA REDEFINES D-FIELD1B.
                   07  D-1B-INT-FG1   PIC X(1).
                   07  D-1B-INT-FG2   PIC X(1).
                   07  D-1B-INT-FG3   PIC X(1).
                   07  D-1B-INT-FG4   PIC X(1).
                   07  D-1B-INT-FG5   PIC X(1).
                   07  FILLER         PIC X(2).
                   07  D-1B-FLAGS     PIC X(18).
               05  D-F-1BB REDEFINES D-FIELD1B.
                   07  FILLER         PIC X(04).
                   07  D-P1B-LBX-PMPT PIC X(10).
                   07  D-P1B-LBOX     PIC X(01).
                   07  FILLER         PIC X(10).
      * COLUMN 3
           03  D-COLUMN1CD.
               05  D-COLUMN1C.
                   07  D-P1C          PIC X(18).
                   07  D-FIELD1C      PIC X(14).
                   07  D-F-1CA REDEFINES D-FIELD1C.
                       09  D-1C-DATE  PIC 99/99/99 BLANK ZERO.
                       09  FILLER     PIC X(6).
                   07  D-F-1CB REDEFINES D-FIELD1C.
                       09  D-1C-RATE  PIC ZZZ.ZZZ.
                       09  FILLER     PIC X(7).
                   07  D-F-1CC REDEFINES D-FIELD1C.
                       09  D-1C-AMT   PIC ZZZZZ.ZZ.
                       09  FILLER      PIC X(6).
      * COLUMN 4
               05  D-COLUMN1D.
                   07  D-P1D          PIC X(18).
                   07  D-FIELD1D      PIC X(14).
                   07  D-F-1DA REDEFINES D-FIELD1D.
                       09  D-1D-DATE  PIC 99/99/99 BLANK ZERO.
                       09  FILLER     PIC X(6).
                   07  D-F-1DB REDEFINES D-FIELD1D.
                       09  D-1D-RATE  PIC ZZZ.ZZZ.
                       09  FILLER     PIC X(7).

       01  D-LINE2 REDEFINES DETAIL-LINE.
      * COLUMN 1
           03  D-COLUMN2A.
               05  D-P2A              PIC X(16).
               05  D-FIELD2AA         PIC X(16).
               05  D-F-2A REDEFINES D-FIELD2AA.
                   07  D-2A-NOPY      PIC ZZZ.
                   07  FILLER         PIC X.
                   07  D-2A-DASH      PIC X.
                   07  FILLER         PIC X.
                   07  D-2A-PYAMT     PIC Z(6).ZZ-.
               05  D-F-2AB REDEFINES D-FIELD2AA.
                   07  D-2A-AMT       PIC Z(7).ZZ-.
                   07  FILLER         PIC X(3).
                   07  D-2A-TILL      PIC Z.
                   07  FILLER         PIC X.
               05  D-F-2AC REDEFINES D-FIELD2AA.
                   07  D-2A-UNIT-PY   PIC ZZ9.999999999999.
      * COLUMN 2
           03  D-COLUMN2B.
               05  D-P2B              PIC X(17).
               05  D-FIELD2B          PIC X(19).
               05  D-F-2B REDEFINES D-FIELD2B.
                   07  D-2B-AMT       PIC Z(6).ZZ-.
                   07  FILLER         PIC X(9).
      * COLUMN 3
           03  D-COLUMN2CD.
               05  D-COLUMN2C.
                   07  D-2C-P1        PIC X(9).
                   07  D-INSURED      PIC X.
                   07  FILLER         PIC XX.
                   07  D-2C-P2        PIC X(7).
                   07  D-INSTYPES     PIC X.
                   07  FILLER         PIC X.
                   07  D-DESC1        PIC X(15).
                   07  FILLER         PIC X.
                   07  D-2C-P3        PIC X(10).
                   07  D-ELIG         PIC X.
                   07  FILLER         PIC X.
                   07  D-DESC2        PIC X(15).
               05  D-COLUMN2D REDEFINES D-COLUMN2C.
                   07  D-2C-P4        PIC X(4).
                   07  D-INSCO        PIC ZZ.
                   07  FILLER         PIC XX.
                   07  D-INSOURS      PIC X.
                   07  FILLER         PIC XXX.
                   07  D-INSEFF       PIC 99/99/99  BLANK ZERO.
                   07  FILLER         PIC X.
                   07  D-INSEXP       PIC 99/99/99  BLANK ZERO.
                   07  FILLER         PIC X(4).
                   07  D-INSTERM      PIC ZZZ.
                   07  FILLER         PIC X.
                   07  D-INSCOVR      PIC Z(6).ZZ-.
                   07  D-INSPREM      PIC Z(5).ZZ-.
                   07  D-INSCOMM      PIC Z(5).ZZ.
               05  D-COLUMN2E REDEFINES D-COLUMN2C.
                   07  D-FEECD1       PIC XX.
                   07  FILLER         PIC X.
                   07  D-FEEDESC1     PIC X(16).
                   07  FILLER         PIC X.
                   07  D-FEEAMT1      PIC Z(5).ZZ-.
                   07  D-FEEOURS1     PIC X.
                   07  FILLER         PIC XX.
                   07  D-FEECD2       PIC XX.
                   07  FILLER         PIC X.
                   07  D-FEEDESC2     PIC X(16).
                   07  FILLER         PIC X.
                   07  D-FEEAMT2      PIC Z(5).ZZ-.
                   07  D-AD-D-INSCOVR REDEFINES D-FEEAMT2
                                      PIC Z(7)9.
                   07  D-FEEOURS2     PIC X.

       01  D-LINE3 REDEFINES DETAIL-LINE.
      * COLUMN 1
           03  D-COLUMN3AB.
               05  D-COLUMN3A.
                   07  D-P3A              PIC X(17).
                   07  D-FIELD3AA         PIC X(13).
                   07  D-F-3A REDEFINES D-FIELD3AA.
                       09  D-3A-AMT       PIC Z(7).ZZ-.
                       09  FILLER         PIC X(2).
                   07  D-F-3AB REDEFINES D-FIELD3AA.
                       09  FILLER         PIC X(2).
                       09  D-3A-DATE      PIC 99/99/99 BLANK ZERO.
                       09  FILLER         PIC X(3).
                   07  D-F-3AC REDEFINES D-FIELD3AA.
                       09  FILLER         PIC X(2).
                       09  D-3A-PDTH      PIC 99/99/99 BLANK ZERO.
                       09  FILLER         PIC X(3).
                   07  D-F-3AD REDEFINES D-FIELD3AA.
                       09  FILLER         PIC X(2).
                       09  D-3A-ALP-DATE  PIC X(8).
                       09  FILLER         PIC X(3).
                   07  D-F-3AE REDEFINES D-FIELD3AA.
                       09  D-3A-AMT0      PIC Z(10)-.
                       09  FILLER         PIC X(2).
      * COLUMN 2
               05  D-COLUMN3B.
                   07  D-P3B              PIC X(17).
                   07  D-FIELD3B          PIC X(12).
                   07  D-F-3BA REDEFINES D-FIELD3B.
                       09  D-3B-AMT       PIC Z(7).ZZ-.
                       09  FILLER         PIC X(1).
                   07  D-F-3BB REDEFINES D-FIELD3B.
                       09  D-3B-COUNT     PIC Z(9)-.
                       09  FILLER         PIC X(2).
                   07  D-F-3BC REDEFINES D-FIELD3B.
                       09  FILLER         PIC X(01).
                       09  D-3B-COUNT-1   PIC Z(03).
                       09  FILLER         PIC X(03).
                       09  D-3B-COUNT-2   PIC Z(03).
                       09  FILLER         PIC X(02).
      * COLUMN 3
           03  D-COLUMN3CD.
               05  D-COLUMN3C.
                   07  D-P3C          PIC X(25).
                   07  D-3C-AMT       PIC Z(6).ZZ-.
                   07  D-3C-FLAG REDEFINES D-3C-AMT PIC X(10).
                   07  D-3C-DATEX REDEFINES D-3C-AMT.
                       09  FILLER    PIC X(02).
                       09  D-3C-DATE PIC 99/99/99.
                   07  FILLER         PIC X(38).
               05  D-COLUMN3CA REDEFINES D-COLUMN3C.
                   07  D-3C-P1        PIC X(2).
                   07  D-ANTIC        PIC Z(6).ZZ-.
                   07  FILLER         PIC XX.
                   07  D-ADJ          PIC Z(6).ZZ-.
                   07  FILLER         PIC XX.
                   07  D-NET          PIC Z(6).ZZ-.
                   07  FILLER         PIC XX.
                   07  D-EARN         PIC Z(6).ZZ-.
                   07  FILLER         PIC XX.
                   07  D-UNEARN       PIC Z(6).ZZ-.
                   07  D-UN-ALP REDEFINES D-UNEARN PIC X(10).
                   07  FILLER         PIC X(3).
                   07  D-UFRONT       PIC Z(6).ZZ.

       01  D-LINE4 REDEFINES DETAIL-LINE.
      * COLUMN 1
           03  D-COLUMN4A.
               05  D-P4A              PIC X(18).
               05  D-FIELD4A          PIC X(13).
               05  D-F-4A REDEFINES D-FIELD4A.
                   07  D-4A-AMT       PIC Z(6).ZZ-.
                   07  D-4A-COUNT REDEFINES D-4A-AMT PIC Z(9)-.
                   07  FILLER         PIC X(3).
               05  D-F-4AB REDEFINES D-FIELD4A.
                   07  FILLER         PIC XX.
                   07  D-4A-DATE      PIC 99/99/99  BLANK ZERO.
                   07  FILLER         PIC X(3).
      * COLUMN 2
           03  D-COLUMN4B.
               05  D-P4B              PIC X(17).
               05  D-FIELD4B          PIC X(12).
               05  FILLER  REDEFINES D-FIELD4B.
                   07  D-4B-AMT1      PIC Z(7).ZZ-.
                   07  FILLER         PIC X(1).
               05  D-F-4BA REDEFINES D-FIELD4B.
                   07  D-4B-FLAG      PIC X.
                   07  FILLER         PIC X(11).
               05  D-F-4BB REDEFINES D-FIELD4B.
                   07  D-4B-DATE      PIC 99/99/99  BLANK ZERO.
                   07  FILLER         PIC X(4).
      * COLUMN 3
           03  D-COLUMN4C.
               05  D-P4C          PIC X(18).
               05  D-FIELD4C      PIC X(14).
               05  D-F-4CA REDEFINES D-FIELD4C.
                   07  FILLER     PIC X.
                   07  D-4C-DATE  PIC 99/99/99 BLANK ZERO.
                   07  FILLER     PIC X.
                   07  D-4C-CODE.
                       09  D-4C-NUM  PIC Z(4).
               05  D-F-4CB REDEFINES D-FIELD4C.
                   07  D-4C-AMT   PIC Z(6).ZZ-.
                   07  D-4C-COUNT REDEFINES D-4C-AMT PIC Z(9)-.
                   07  FILLER     PIC X(4).
      * COLUMN 4
           03  D-COLUMN4D.
               05  D-P4D          PIC X(17).
               05  D-FIELD4D      PIC X(24).
               05  D-F-4DA REDEFINES D-FIELD4D.
                   07  FILLER     PIC X.
                   07  D-4D-DATE  PIC 99/99/99 BLANK ZERO.
                   07  FILLER     PIC X(15).
               05  D-F-4DB REDEFINES D-FIELD4D.
                   07  D-4D-AMT   PIC Z(6).ZZ-.
                   07  D-4D-COUNT REDEFINES D-4D-AMT PIC Z(9)-.
                   07  FILLER     PIC X(14).
               05  D-F-4DD REDEFINES D-FIELD4D.
                   07  FILLER       PIC X.
                   07  D-4D-FG      PIC X.
                   07  FILLER       PIC X(22).

           03  D-COLUMN4D1 REDEFINES D-COLUMN4D.
               05  FILLER            PIC X(4).
               05  D-4D-PROFILE2A-ALP.
                   07 D-4D-PROFILE2A PIC 9(12).
               05  FILLER            PIC X(9).
               05  D-4D-PROFILE2B-ALP.
                   07 D-4D-PROFILE2B PIC 9(12).
               05  FILLER            PIC X(4).

           03  D-COLUMN4D2 REDEFINES D-COLUMN4D.
               05  FILLER            PIC X(5).
               05  D-4D2-TEXT        PIC X(23).
               05  FILLER            PIC X(2).
               05  D-4D2-AMT1        PIC ZZZZZ9.99.
               05  FILLER            PIC X(2).

       01  D-LINE9 REDEFINES DETAIL-LINE.
           03  FILLER           PIC X(40).
           03  D-COLLAT.
               05  D-DESCRIPTION    PIC X(10).
               05  FILLER           PIC X.
               05  D-MAKE           PIC X(15).
               05  FILLER           PIC X.
               05  D-MODEL          PIC X(15).
               05  FILLER           PIC X.
               05  D-YEAR           PIC ZZZZ.
               05  FILLER           PIC X.
               05  D-STYLE          PIC X(8).
               05  FILLER           PIC X.
               05  D-LTCLASS        PIC X(8).
               05  FILLER           PIC X.
               05  D-ID             PIC X(20).

       01  RANGE-LINE REDEFINES DETAIL-LINE.
           03  FILLER           PIC X(35).
           03  RANGE-SELECTION.
               04  R-BRANCH     PIC 9(4).
               04  FILLER       PIC XX.
               04  R-CLASS      PIC 99.
               04  FILLER       PIC XX.
               04  R-ACCT       PIC 9(6).
               04  FILLER       PIC XX.

       COPY "LIBWI/LCSCANW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBGB/GETPRW.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBLP/LPSTATW.CPY".
       COPY "LIBGB/GPENVW.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBLP/LPMDTEW.CPY".
       COPY "LIBLP/LPITRMW.CPY".
       COPY "LIBLP/LPEARNW.CPY".
       COPY "LIBLP/UPERTSTW.CPY".
       01  LNLIST-WINDOW-HANDLE PIC X(10).
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBLS/LNLIST_DEF.CPY".
       COPY "LIBLS/LNLIST_WKS.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

      /***************************************
      *     P R O G R A M   C O N T R O L    *
      ****************************************
       COPY "LIBGB/GETFMW.CPY".

       SCREEN SECTION.
       COPY "LIBLS/LNLIST_SCN.CPY".

       PROCEDURE DIVISION
             USING FORM-PATHNAME EXIT-PATHNAME.
       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON PR-FILE.
       COPY "LIBGB/DECLARE.CPY".
           PERFORM CLOSE-SP1-FILE.
           PERFORM CLOSE-LTC1-FILE.
           PERFORM CLOSE-LN3-FILE.
           PERFORM CLOSE-LN1-FILE.
           PERFORM CLOSE-CDB1-FILE.
           PERFORM CLOSE-BW1-FILE.
           PERFORM CLOSE-BR-FILE.
           PERFORM CLOSE-PR-FILE.
       COPY "LIBGB/DECLAR2.CPY".

      *************************************
       MAIN-MODULE SECTION.
      *************************************
           PERFORM SQL-CONNECT.

      D    STOP FORM-PATHNAME.

       COPY "LIBGB/CHKSEC.CPY".

           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "LNLIST" TO VDU-FORM-NAME.

           DISPLAY FLOATING GRAPHICAL WINDOW
                   SCREEN LINE 01 * VDU-WIN-L-OFFSET
                   SCREEN COL 01 * VDU-WIN-C-OFFSET
                   CELL SIZE IS LABEL FONT
                   SIZE 80 + VDU-WIN-W-MOD
                   LINES 24 + VDU-WIN-H-MOD
                   BLANK SCREEN
                   HANDLE IS LNLIST-WINDOW-HANDLE.

           PERFORM FORM-RESET.

           IF EXT-ROUTE-BUF NOT = "00"                                          
              MOVE "PROGRAM NOT BATCHABLE" TO MESS                              
              PERFORM SEND-MESS        
              MOVE "99" TO EXT-ROUTE-BUF                                        
              GO TO EXIT-PROG.

           PERFORM INITIALIZATION.
           IF ERRCD NOT = " "
              GO TO END-PROGRAM.

           MOVE "R" TO TIMER-TYPE.
           PERFORM TIMER-START.
           MOVE TIMER-LINK-BUF TO P-TIMER-LINK-BUF.
           PERFORM GET-GPENV.

       NEXT-LN.
           IF SEQ = 1
              PERFORM READ-LN1-FILE-NEXT
              IF IO-FG = 0
                 IF (LN-ACCTNO > END-ACCT) OR
                      (LN-OWNBR NOT = LN-PATH-OWNBR)
                     GO TO END-ROUTINE.
           IF SEQ = 2
              PERFORM READ-LN3-FILE-NEXT
              IF IO-FG = 0
                 IF (LN-CLASS > END-CLASS) OR
                      (LN-OWNBR NOT = LN-PATH-OWNBR)
                    GO TO END-ROUTINE
                 ELSE
                    IF LN-ACCTNO < BEG-ACCT OR LN-ACCTNO > END-ACCT
                       GO TO NEXT-LN.
           IF SEQ = 3
              PERFORM READ-LN3-FILE-NEXT
              IF IO-FG = 0
                 MOVE LN-OWNBR TO WORK-BRANCH
                 MOVE LN-CLASS TO WORK-CLASS
                 MOVE LN-ACCTNO TO WORK-NUMBER
                 IF (WORK-KEY > END-KEY) OR
                      (LN-OWNBR NOT = LN-PATH-OWNBR)
                    GO TO END-ROUTINE
                 ELSE
                    IF LN-ACCTNO < BEG-ACCT OR LN-CLASS < BEG-CLASS
                          OR
                       LN-ACCTNO > END-ACCT OR LN-CLASS > END-CLASS
                          GO TO NEXT-LN.

           IF IO-FG = 9
              GO TO END-ROUTINE.

           IF ( LN-POCD-NONLOAN )
              GO TO NEXT-LN.

           IF HOLD-LN-OWNBR NOT = LN-OWNBR
              MOVE LN-OWNBR TO HOLD-LN-OWNBR
              MOVE LN-OWNBR TO BR-NO
              PERFORM OPEN-BR-FILE
              PERFORM READ-BR-FILE
              IF IO-FG NOT = 0
                 GO TO IO-ERROR
              ELSE
                 PERFORM CLOSE-BR-FILE.

           MOVE LN-ORGST    TO SP-ORGST.
           MOVE LN-SPRCLASS TO SP-SPRCLASS.
           MOVE LN-SUBCLASS TO SP-SUBCLASS.
           MOVE LN-LAWCODE  TO SP-LAWCODE.
           IF LPWSSP-SP1-KEY NOT = SP1-KEY
              MOVE SP1-KEY TO LPWSSP-SP1-KEY
              PERFORM READ-SP1-FILE
              IF IO-FG NOT = 0
                 GO TO IO-ERROR.

           PERFORM COMPUTE-EARNINGS.

           PERFORM DETAIL-LINE-PROCESS.

           GO TO NEXT-LN.
       END-ROUTINE.
           IF FIRST-TIME = "Y"
              MOVE " NO RECORDS FOUND FOR SELECTED RANGE !" TO MESS
              PERFORM SEND-MESS
              MOVE SPACES TO PR-REC
              MOVE 99 TO LN-FEED
              PERFORM WRITE-PR-REC.

           PERFORM PRINT-RANGE-LINES.
           GO TO END-PROGRAM.

      ***********************************
       INITIALIZATION SECTION.
      ***********************************
       SETRP.
           MOVE " " TO ERRCD.
           MOVE LNLIST-QUEUE-POS TO Q-POS.
           MOVE LNLIST-COPIES-POS TO C-POS.
           MOVE LNLIST-PRU-POS TO P-POS.
           MOVE SPACES TO Q-BUF C-BUF P-BUF.
           PERFORM PRU-IN.
           IF STAT NOT = "00"
              GO TO ENDIT.
       RRSEQ.
           MOVE "ENNN010SEQ." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO SETRP.
           IF VDUSTATUS = "1>" GO TO ENDIT.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO RRSEQ.
           MOVE VDUNUM0 TO SEQ.
           IF SEQ NOT = 1 AND SEQ NOT = 2 AND SEQ NOT = 3
              GO TO RRSEQ.

           IF SEQ = 1 GO TO AC-01.
           IF SEQ = 2 GO TO CL-01.

      * FORCE USER'S BRANCH
           MOVE "SNNN040BR1." TO VDU.    
           MOVE EXT-FILPATH-BRNO TO VDUNUM0. 
           PERFORM SCREENIO.

           MOVE "SNNN040BR2." TO VDU.
           MOVE EXT-FILPATH-BRNO TO VDUNUM0. 
           PERFORM SCREENIO.

       CL-01.
           MOVE "ENNN020CL1." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO SETRP.
           IF VDUSTATUS = "16" GO TO ALL-CL.
           IF VDUSTATUS = "1U"
      *KC     IF SEQ < 3
                 GO TO RRSEQ.                    *> FORCE USER'S BRANCH
      *KC     ELSE
      *KC        GO TO BR-02.
           IF VDUSTATUS = "1<"
              MOVE "BEG" TO CLASS-FG
              PERFORM CLASS-SCAN
              IF IO-FG NOT = 0
                 GO TO CL-01.
           IF VDUSTATUS = "1>" GO TO ENDIT.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO CL-01.
           MOVE VDUNUM0 TO BEG-CLASS.
       CL-02.
           MOVE "ENNN020CL2." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO SETRP.
           IF VDUSTATUS = "1U" GO TO CL-01.
           IF VDUSTATUS = "1<"
              MOVE "END" TO CLASS-FG
              PERFORM CLASS-SCAN
              IF IO-FG NOT = 0
                 GO TO CL-02.
           IF VDUSTATUS = "1>" GO TO ENDIT.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO CL-02.
           MOVE VDUNUM0 TO END-CLASS.
           IF BEG-CLASS > END-CLASS
              MOVE "INVALID CLASS RANGE" TO MESS
              PERFORM SEND-MESS
              GO TO CL-01.
           GO TO AC-01.
       ALL-CL.
           MOVE "SN N020CL1." TO VDU.
           MOVE 0 TO BEG-CLASS VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SN N020CL2." TO VDU.
           MOVE 99 TO END-CLASS VDUNUM0.
           PERFORM SCREENIO.
       AC-01.
           MOVE "ENNN060AC1." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO SETRP.
           IF VDUSTATUS = "16" GO TO ALL-AC.
           IF VDUSTATUS = "1U"
              IF SEQ = 1
                 GO TO RRSEQ
              ELSE
                 GO TO CL-02.
           IF VDUSTATUS = "1>" GO TO ENDIT.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO AC-01.
           MOVE VDUNUM0 TO BEG-ACCT.
       AC-02.
           MOVE "ENNN060AC2." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO SETRP.
           IF VDUSTATUS = "1U"  GO TO AC-01.
           IF VDUSTATUS = "1>" GO TO ENDIT.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO AC-02.
           MOVE VDUNUM0 TO END-ACCT.
           IF BEG-ACCT > END-ACCT
              MOVE "INVALID ACCOUNT RANGE" TO MESS
              PERFORM SEND-MESS
              GO TO AC-01.
           GO TO OK-YN.
       ALL-AC.
           MOVE "SN N060AC1." TO VDU.
           MOVE 0 TO BEG-ACCT VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SN N060AC2." TO VDU.
           MOVE 999999 TO END-ACCT VDUNUM0.
           PERFORM SCREENIO.
       OK-YN.
           MOVE " " TO ERRCD.
           MOVE "E  A010ACCEPT." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO ENDIT.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO OK-YN.
           MOVE VDUBUF TO ACCEPT-BUF.
           IF ACCEPT-BUF = "Y" GO TO EXPR-G.
           IF ACCEPT-BUF NOT = "N" GO TO OK-YN.
           GO TO RRSEQ.

       EXPR-G.
           PERFORM OPEN-BW1-FILE.
           PERFORM OPEN-LTC1-FILE.
           PERFORM OPEN-SP1-FILE.
           PERFORM OPEN-CDB1-FILE.

           PERFORM OPEN-LN1-FILE.

           MOVE LN-PATH-OWNBR TO LN-OWNBR.

           IF SEQ = 1
              MOVE LN-OWNBR   TO QLN1-WBEG-OWNBR
                                 QLN1-WEND-OWNBR
              MOVE BEG-ACCT   TO LN-ACCTNO
                                 QLN1-WBEG-ACCTNO
              MOVE END-ACCT   TO QLN1-WEND-ACCTNO
              PERFORM START-LN1-FILE
              GO TO SET-HEAD.

           IF SEQ = 2 OR 3
              MOVE LN-OWNBR   TO QLN3-WBEG-OWNBR
                                 QLN3-WEND-OWNBR
              MOVE BEG-CLASS  TO LN-CLASS
                                 QLN3-WBEG-CLASS
              MOVE END-CLASS  TO QLN3-WEND-CLASS
              MOVE BEG-ACCT   TO LN-ACCTNO
                                 QLN3-WBEG-ACCTNO
              MOVE END-ACCT   TO QLN3-WEND-ACCTNO
              PERFORM START-LN3-FILE
              GO TO SET-HEAD.

           GO TO RRSEQ.

       SET-HEAD.
           MOVE EXT-CONAME-CONAME TO H-NAME.
           MOVE EXT-CONAME-SYSDATE TO H-DATE.
           MOVE EXT-CONAME-USER-ID TO H-USERID.

           MOVE "LISTING IN PROGRESS...................." TO WR-BUF.
           MOVE MESS-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

           PERFORM OPEN-PR-FILE.

           GO TO EXIT-INITIALIZE.
       ENDIT.
           MOVE "X" TO ERRCD.
       EXIT-INITIALIZE.
           EXIT.

      **************************************
       DETAIL-LINE-PROCESS SECTION.
      **************************************

           PERFORM HEAD-PAGE.
           MOVE DASH-LINE TO DETAIL-LINE.
           PERFORM WRITE-DETAIL-LINE.

      *--------------PAGE 1 AND 2 -----------------------------------
           MOVE LN-SSNO(1) TO BW-SSNO.
           PERFORM READ-BW1-FILE.

           PERFORM LPSTAT-DISPLAY.
           MOVE LPS-DISPLAY-LNNO TO D-LOAN-NUM.
           MOVE "BORROWERS AND STATUS CODES" TO D-TITLE.
           MOVE 1 TO D-PAGE.
           MOVE "DATES AND RATES" TO D-TITLE2.
           MOVE 2 TO D-PAGE2.
           MOVE HEAD3 TO PR-REC.
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-PR-REC.

           MOVE "ORIG ST/BR  :" TO D-P1A.
           MOVE LN-ORGST TO D-1A-STATE.
           MOVE "-" TO D-1A-DASH.
           MOVE LN-ORGBR TO D-1A-OWNBR.
           MOVE "LOAN STATUS CODES :" TO D-COLUMN1B.
           MOVE "ORIGINAL DATES:" TO D-P1C.
           MOVE "STATUS CHANGE DATES:" TO D-COLUMN1D.
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "CLASS / SUB :" TO D-P1A.
           MOVE LN-CLASS    TO D-1A-LAWCODE.
           MOVE LN-SUBCLASS TO D-1A-OWNBR.
           MOVE "OWN ST/BR:" TO D-P1B.
           MOVE LN-OWNST TO D-1B-STATE.
           MOVE "-" TO D-1B-DASH.
           MOVE LN-OWNBR TO D-1B-OWNBR.
           MOVE "ENTERED DATE    :" TO D-P1C.
           MOVE LN-ENTDATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO D-1C-DATE.
           MOVE "PAID OFF DATE   :" TO D-P1D.
           MOVE LN-POFFDATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO D-1D-DATE.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "LAW CODE    :" TO D-P1A.
           MOVE LN-LAWCODE TO D-1A-LAWCODE.
           MOVE "PURCHASED DATE  :" TO D-P1C.
           MOVE LN-PURDATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO D-1C-DATE.
           MOVE "JUDGEMENT DATE  :" TO D-P1D.
           MOVE LN-JDDATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO D-1D-DATE.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "SPR CLASS   :" TO D-P1A.
           MOVE LN-SPRCLASS TO D-1A-LAWCODE.
           MOVE "PURP CODE:" TO D-P1B.
           MOVE LN-PURCD TO D-FIELD1B.
           MOVE "LOAN DATE       :" TO D-P1C.
           MOVE LN-LOANDATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO D-1C-DATE.
           MOVE "P & L DATE      :" TO D-P1D.
           MOVE LN-PLDATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO D-1D-DATE.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "             " TO D-P1A.
           MOVE SPACES          TO D-1A-STATE.
           MOVE "COLAT CD :" TO D-P1B.
           MOVE LN-COLLCD TO D-1B-STATE.
           MOVE "ORG 1ST PAY DATE:" TO D-P1C.
           MOVE LN-ORIG-1STPYDATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO D-1C-DATE.
           MOVE "ACCEL DATE      :" TO D-P1D.
           MOVE LN-ACCELDATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO D-1D-DATE.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "BORROWER INFORMATION:" TO D-COLUMN1A.
           MOVE "COLAT ID :" TO D-P1B.
           MOVE LN-COLLID TO D-F-1B.
           MOVE "DATE INT START  :" TO D-P1C.
           MOVE LN-INTDATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO D-1C-DATE.
           MOVE "INT REDUCED DATE:" TO D-P1D.
           MOVE LN-REDUDATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO D-1D-DATE.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "BORR #1  CD :" TO D-P1A.
           MOVE 1 TO SUB.
           PERFORM DISPLAY-BORROWER.
           MOVE "SOURCE CD:" TO D-P1B.
           MOVE LN-SRCD TO D-1B-STATE.
           MOVE "FILING DATE     :" TO D-P1C.
           MOVE LN-FILEDATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO D-1C-DATE.
           MOVE "LEGAL ACT DATE  :" TO D-P1D.
           MOVE LN-LEGALDATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO D-1D-DATE.
           PERFORM WRITE-DETAIL-LINE.

           STRING BW-FNAME," ",BW-LNAME DELIMITED BY "   "
                 INTO D-FIELD1AA.
           MOVE "CONFIDENT:" TO D-P1B.
           MOVE LN-CONFIDENTCD TO D-1B-STATE.
           MOVE "SAME AS CASH    :" TO D-P1C.
           MOVE LN-CASHDATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO D-1C-DATE.
           MOVE "BANKRUPT DATE   :" TO D-P1D.
           MOVE LN-BNKRPTDATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO D-1D-DATE.
           PERFORM WRITE-DETAIL-LINE.

           MOVE SPACE TO BW-FNAME BW-LNAME.
           MOVE "BORR #2  CD :" TO D-P1A.
           MOVE 2 TO SUB.
           PERFORM DISPLAY-BORROWER.
           MOVE "ALLOTMENT:" TO D-P1B.
           MOVE LN-ALLOTCD TO D-1B-STATE.
           MOVE "LOCK BOX: " TO D-P1B-LBX-PMPT.
           MOVE LN-LBOX TO D-P1B-LBOX.
           MOVE "DEBT AID DATE   :" TO D-P1D.
           MOVE LN-DEBTAIDATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO D-1D-DATE.
           PERFORM WRITE-DETAIL-LINE.

           STRING BW-FNAME," ",BW-LNAME DELIMITED BY "   "
                 INTO D-FIELD1AA.
           MOVE "CONV ACCT:" TO D-P1B.
           MOVE LN-CONVERCD TO D-1B-STATE.
           MOVE "REPOSESSION DATE:" TO D-P1D.
           MOVE LN-REPODATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO D-1D-DATE.
           PERFORM WRITE-DETAIL-LINE.

           MOVE SPACE TO BW-FNAME BW-LNAME.
           MOVE "BORR #3  CD :" TO D-P1A.
           MOVE 3 TO SUB.
           PERFORM DISPLAY-BORROWER.
           MOVE "MIDSTREAM:" TO D-P1B.
           MOVE LN-MIDSTREAM TO D-1B-STATE.
           MOVE "REDEEM DATE     :" TO D-P1D.
           MOVE LN-REDEEMDATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO D-1D-DATE.
           PERFORM WRITE-DETAIL-LINE.

           STRING BW-FNAME," ",BW-LNAME DELIMITED BY "   "
                 INTO D-FIELD1AA.
           MOVE "SKIP TRW :" TO D-P1B.
           MOVE LN-SKIP-TRW  TO D-1B-STATE.
           MOVE "SOLD REPO DATE  :" TO D-P1D.
           MOVE LN-SOLDDATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO D-1D-DATE.
           PERFORM WRITE-DETAIL-LINE.

           MOVE SPACE TO BW-FNAME BW-LNAME.
           MOVE "BORR #4  CD :" TO D-P1A.
           MOVE 4 TO SUB.
           PERFORM DISPLAY-BORROWER.
           MOVE "RECAST CD:" TO D-P1B.
           MOVE LN-RECASTCD TO D-1B-STATE.
           PERFORM WRITE-DETAIL-LINE.

           STRING BW-FNAME," ",BW-LNAME DELIMITED BY "   "
                 INTO D-FIELD1AA.
           MOVE "P & L CD :" TO D-P1B.
           MOVE LN-PLCD TO D-1B-STATE.
           MOVE "P&L TYPE: " TO D-P1B-LBX-PMPT.
           MOVE LN-PLTYPE TO D-P1B-LBOX.
           STRING "-----------------------!","  RATE INFORMATION !",
                  "-------------------!" DELIMITED BY "!"
                  INTO D-COLUMN1CD.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "PAYOFF CD:" TO D-P1B.
           MOVE LN-POCD TO D-1B-STATE.
           MOVE "A.P.R            :" TO D-P1C.
           MOVE LN-APRATE TO D-1C-RATE.
           MOVE "JUDGEMENT RATE   :" TO D-P1D.
           MOVE LN-JDRATE TO D-1D-RATE.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "P&L REASN:" TO D-P1B.
           MOVE LN-PLREASON TO D-1B-STATE.
           MOVE "EFFECT INT RATE  :" TO D-P1C.
           MOVE LN-EFFRATE TO D-1C-RATE.
           MOVE "ACCEL RATE       :" TO D-P1D.
           MOVE LN-ACCRATE TO D-1D-RATE.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "REPO CODE:" TO D-P1B.
           MOVE LN-REPOCD TO D-1B-STATE.
           MOVE "SIMPLE RATE      :" TO D-P1C.
           MOVE LN-SMPRATE TO D-1C-RATE.
           MOVE "RATE CHANGE/REDUC:" TO D-P1D.
           MOVE LN-REDURATE TO D-1D-RATE.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "LOAN STATUS CHANGES:" TO D-COLUMN1A.
           MOVE "ADDON  FG:" TO D-P1B.
           MOVE LN-ADDON-FG TO D-1B-STATE.
           MOVE "OVERRIDING RATE  :" TO D-P1C.
           MOVE LN-OVRDRATE TO D-1C-RATE.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "STATUS FLAGS:" TO D-P1A.
           MOVE LN-STATFLAGS TO D-FIELD1AA.
           MOVE "PYSCHD FG:" TO D-P1B.
           MOVE LN-PAY-SCHLD-FG TO D-1B-STATE.
           MOVE "ENDORSER RATE    :" TO D-P1C.
           MOVE LN-ENDORATE TO D-1C-RATE.
           MOVE "SERVICE PERCENT :" TO D-P1D.
           MOVE LN-SERVPER TO D-1D-RATE.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "LAST STATCHG:" TO D-P1A.
           MOVE LN-STATUSFG     TO D-1A-STATE.
           MOVE "CAN LOAN#:"    TO D-P1B.
           MOVE LN-CAN-LOAN-NO  TO D-1B-STATE.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "INT FLAGS:"    TO D-P1B.
           MOVE LN-SMALLOAN     TO D-1B-INT-FG1.
           MOVE LN-SALEFIN      TO D-1B-INT-FG2.
           MOVE LN-SERVACCT     TO D-1B-INT-FG3.
           MOVE LN-LOANTYPE     TO D-1B-INT-FG4.
           MOVE LN-GIFTNET      TO D-1B-INT-FG5.
           MOVE "SL,SF,SA,IBPC,GN" TO D-1B-FLAGS.
           PERFORM WRITE-DETAIL-LINE.

           PERFORM CHECK-PAGE.
           MOVE DASH-LINE TO DETAIL-LINE.
           PERFORM WRITE-DETAIL-LINE.

      *--------------PAGE 3 AND 4 -----------------------------------
           MOVE LPS-DISPLAY-LNNO TO D-LOAN-NUM.
           MOVE "GENERAL LOAN INFORMATION  " TO D-TITLE.
           MOVE 3 TO D-PAGE.
           MOVE "INSURANCE AND FEES" TO D-TITLE2.
           MOVE 4 TO D-PAGE2.
           MOVE HEAD3 TO PR-REC.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-PR-REC.

           MOVE "GENERAL LOAN INFORMATION:" TO D-COLUMN2A.
           MOVE "LOAN FINANCE CHARGES:" TO D-COLUMN2B.
           STRING "-------------------!"," INSURANCE INFORMATION -----!",
                  "----------------!" DELIMITED BY "!" INTO D-COLUMN2CD.
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "SELLING PRICE :" TO D-P2A.
           MOVE LN-SELLPR TO D-2A-AMT.
           MOVE "FINANCE CHARGE :" TO D-P2B.
           MOVE LN-FINCHG TO D-2B-AMT.
           MOVE "INSURED:" TO D-2C-P1.
           MOVE LN-INSURED TO D-INSURED.
           MOVE "TYPE :" TO D-2C-P2.
           MOVE LN-INSTYPES TO D-INSTYPES.
           IF LN-INSTYPES = "B"
              MOVE " LL" TO D-DESC1
           ELSE
           IF LN-INSTYPES = "C"
              MOVE " DL" TO D-DESC1
           ELSE
           IF LN-INSTYPES = "D"
              MOVE " A&H" TO D-DESC1
           ELSE
           IF LN-INSTYPES = "E"
              MOVE " OTHER" TO D-DESC1
           ELSE
           IF LN-INSTYPES = "F"
              MOVE " LL AND A&H" TO D-DESC1
           ELSE
           IF LN-INSTYPES = "G"
              MOVE " DL AND A&H" TO D-DESC1
           ELSE
           IF LN-INSTYPES = "H"
              MOVE " LL AND OTHER" TO D-DESC1
           ELSE
           IF LN-INSTYPES = "J"
              MOVE " DL AND OTHER" TO D-DESC1
           ELSE
           IF LN-INSTYPES = "K"
              MOVE " A&H AND OTHER" TO D-DESC1
           ELSE
           IF LN-INSTYPES = "L"
              MOVE " LL, A&H, OTHER" TO D-DESC1
           ELSE
           IF LN-INSTYPES = "M"
              MOVE " DL, A&H, OTHER" TO D-DESC1
           ELSE
           IF LN-INSTYPES = "N"
              MOVE " NONE" TO D-DESC1.
           MOVE "ELIGABLE:" TO D-2C-P3.
           MOVE LN-INSELIGIBLE TO D-ELIG.
           IF LN-INSELIGIBLE = "B"
              MOVE " LL" TO D-DESC2
           ELSE
           IF LN-INSELIGIBLE = "C"
              MOVE " DL" TO D-DESC2
           ELSE
           IF LN-INSELIGIBLE = "D"
              MOVE " A&H" TO D-DESC2
           ELSE
           IF LN-INSELIGIBLE = "E"
              MOVE " OTHER" TO D-DESC2
           ELSE
           IF LN-INSELIGIBLE = "F"
              MOVE " LL AND A&H" TO D-DESC2
           ELSE
           IF LN-INSELIGIBLE = "G"
              MOVE " DL AND A&H" TO D-DESC2
           ELSE
           IF LN-INSELIGIBLE = "H"
              MOVE " LL AND OTH." TO D-DESC2
           ELSE
           IF LN-INSELIGIBLE = "J"
              MOVE " DL AND OTH." TO D-DESC2
           ELSE
           IF LN-INSELIGIBLE = "K"
              MOVE " A&H AND OTH." TO D-DESC2
           ELSE
           IF LN-INSELIGIBLE = "L"
              MOVE " LL, A&H, OTH." TO D-DESC2
           ELSE
           IF LN-INSELIGIBLE = "M"
              MOVE " DL, A&H, OTH." TO D-DESC2
           ELSE
           IF LN-INSELIGIBLE = "N"
              MOVE " NONE" TO D-DESC2.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "DOWN PAYMENT  :" TO D-P2A.
           MOVE LN-DOWNPY TO D-2A-AMT.
           MOVE "INTEREST CHG.  :" TO D-P2B.
           MOVE LN-INTCHG TO D-2B-AMT.
           MOVE TEXT2A TO D-COLUMN2CD.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "TRADE IN      :" TO D-P2A.
           MOVE LN-TRADEIN        TO D-2A-AMT.
           MOVE "1ST PAY EXT CHG:" TO D-P2B.
           MOVE LN-EXTCHG TO D-2B-AMT.
           MOVE "CL:" TO D-2C-P4.
           MOVE 1 TO ITRM-SUB SUB.
           PERFORM SET-INSURANCE.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "LOAN AMOUNT   :" TO D-P2A.
           MOVE LN-LNAMT TO D-2A-AMT.
           MOVE "SERVICE CHARGE :" TO D-P2B.
           MOVE LN-SERCHG TO D-2B-AMT.
           MOVE "AH:" TO D-2C-P4.
           MOVE 2 TO ITRM-SUB SUB.
           PERFORM SET-INSURANCE.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "ORIGINAL TERM :" TO D-P2A.
           MOVE LN-ORGTERM TO D-2A-NOPY.
           MOVE "MAINT. FEE     :" TO D-P2B.
           MULTIPLY LN-MAINTFEE BY LN-ORGTERM GIVING D-2B-AMT.
           MOVE "PP:" TO D-2C-P4.
           MOVE 3 TO ITRM-SUB SUB.
           PERFORM SET-INSURANCE.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "1ST PAYMENT   :" TO D-P2A.
           MOVE LN-1STPYAMT TO D-2A-PYAMT.
           MOVE "O1:" TO D-2C-P4.
           MOVE 4 TO ITRM-SUB SUB.
           PERFORM SET-INSURANCE.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "REG PAYMENTS  :" TO D-P2A.
           MOVE LN-NOREGPY TO D-2A-NOPY.
           MOVE "@" TO D-2A-DASH.
           MOVE LN-REGPYAMT TO D-2A-PYAMT.
           MOVE "RELATED FIN CHARGE FIELDS:" TO D-COLUMN2B.
           MOVE "O2:" TO D-2C-P4.
           MOVE 5 TO ITRM-SUB SUB.
           PERFORM SET-INSURANCE.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "LAST PAY AMT  :"  TO D-P2A.
           MOVE LN-LASTPYAMT       TO D-2A-PYAMT.
           MOVE "POINTS         :" TO D-P2B.
           MOVE LN-POINTS          TO D-2B-AMT.
           MOVE "O3:" TO D-2C-P4.
           MOVE 6 TO ITRM-SUB SUB.
           PERFORM SET-INSURANCE.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "N/A            :" TO D-P2B.
           MOVE "O4:" TO D-2C-P4.
           MOVE 7 TO ITRM-SUB SUB.
           PERFORM SET-INSURANCE.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "LOAN PROCEEDS :" TO D-P2A.
           MOVE LN-PROCEEDS TO D-2A-AMT.
           MOVE "STATE FLAG     :" TO D-P2B.
           MOVE LN-STATE-FLAG TO D-FIELD2B.
           MOVE "O5:" TO D-2C-P4.
           MOVE 8 TO ITRM-SUB SUB.
           PERFORM SET-INSURANCE.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "CASH PROCEEDS :" TO D-P2A.
           MOVE LN-PROCASH TO D-2A-AMT.
           MOVE LN-TILLNO TO D-2A-TILL.
           MOVE "STATE WORKER   :" TO D-P2B.
           MOVE LN-STATE-WORKER TO D-2B-AMT.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "TRADE-IN      :"  TO D-P2A.
           MOVE LN-TRADEIN         TO D-2A-AMT.
           MOVE "STATE WORKER 2 :" TO D-P2B.
           MOVE LN-STATE-WORKER2   TO D-2B-AMT.
           MOVE "DLADJ/DLCOMM     :" TO D-P1C.
           MOVE LN-INSPREM-DLADJ TO D-1C-AMT.
           MOVE LN-O3INS-DLPROC TO D-4D-AMT.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "PAY PERIOD INFORMATION  :" TO D-COLUMN2A.
           MOVE "LATE CHARGE CD :" TO D-P2B.
           MOVE LN-LC-CODE TO D-FIELD2B.
           MOVE "INS CONFIRM DATE :" TO D-P1C.
           MOVE LN-INS-CONFIRM-DATE  TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY  TO D-1C-DATE.
           PERFORM WRITE-DETAIL-LINE.

           STRING "-------------------!",
            " FEE INFORMATION -----!","--------------------!"
                DELIMITED BY "!" INTO D-COLUMN2CD.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "PAY PERIOD CD :" TO D-P2A.
           MOVE TEXT2B TO D-COLUMN2CD.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "PAY PER FREQ  :" TO D-P2A.
           MOVE LN-UNITPER-FREQ   TO D-2A-NOPY.
           MOVE LN-UNITPER-CD     TO D-FIELD2AA.
           MOVE 1 TO SUB.
           PERFORM SET-FEE-LEFT.
           MOVE 6 TO SUB.
           PERFORM SET-FEE-RIGHT.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "PAYS PER YEAR :"   TO D-P2A.
           MOVE LN-UNITPER-PER-YEAR TO D-2A-UNIT-PY.
           MOVE 2 TO SUB.
           PERFORM SET-FEE-LEFT.
           MOVE 7 TO SUB.
           PERFORM SET-FEE-RIGHT.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "DAY PER PERIOD:"  TO D-P2A.
           MOVE LN-DAYS-IN-UNITPER TO D-2A-NOPY.
           MOVE 3 TO SUB.
           PERFORM SET-FEE-LEFT.
           MOVE 8 TO SUB.
           PERFORM SET-FEE-RIGHT.
           PERFORM WRITE-DETAIL-LINE.

           MOVE LN-UNITPER-CD   TO UPERTST-UNITPER-CD.
           MOVE LN-UNITPER-FREQ TO UPERTST-UNITPER-FREQ.
           PERFORM UPERTST-SET-MESS.
           MOVE UPERTST-MESS TO D-COLUMN1A.
           MOVE 4 TO SUB.
           PERFORM SET-FEE-LEFT.
           MOVE 9 TO SUB.
           PERFORM SET-FEE-RIGHT.
           PERFORM WRITE-DETAIL-LINE.

           MOVE 5 TO SUB.
           PERFORM SET-FEE-LEFT.
           MOVE 10 TO SUB.
           PERFORM SET-FEE-RIGHT.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "AD&D COVERAGE"            TO D-FEEDESC2.
           MOVE LN-AD-D-INS-COVR           TO D-AD-D-INSCOVR .
           PERFORM WRITE-DETAIL-LINE.

           PERFORM CHECK-PAGE.
           MOVE DASH-LINE TO DETAIL-LINE.
           PERFORM WRITE-DETAIL-LINE.

      *--------------PAGE 5 AND 6 -----------------------------------
           MOVE LPS-DISPLAY-LNNO TO D2-LOAN-NUM.
           MOVE "CUR. INFO / TOTALS     " TO D2-TITLE.
           MOVE 5 TO D2-PAGE.
           MOVE "ANTIC EARNINGS INFO" TO D2-TITLE2.
           MOVE 6 TO D2-PAGE2.
           MOVE HEAD4 TO PR-REC.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-PR-REC.

           MOVE "CURRENT LOAN INFORMATION:" TO D-COLUMN3A.
           MOVE "CURRENT LOAN WORKERS:" TO D-COLUMN3B.
           MOVE TEXT3A TO D-COLUMN3CD.
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "CURRENT BAL    :" TO D-P3A.
           MOVE LN-CURBAL TO D-3A-AMT.
           MOVE "LCPAID WORKER  :" TO D-P3B.
           MOVE LN-LCPAID TO D-3B-AMT.
           MOVE 0 TO SUB.
           MOVE "I:" TO D-3C-P1.
           PERFORM DISPLAY-COMPONENT-EARNINGS.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "INTEREST BAL   :" TO D-P3A.
           MOVE LN-INTBAL TO D-3A-AMT.
           MOVE "LCPARTIAL      :" TO D-P3B.
           MOVE LN-LCPARTIALS TO D-3B-AMT.
           MOVE "A:" TO D-3C-P1.
           PERFORM DISPLAY-COMPONENT-EARNINGS.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "LATE CHG BAL   :" TO D-P3A.
           MOVE LN-LCBAL TO D-3A-AMT.
           MOVE "STATEMENT CNTR :" TO D-P3B.
           MOVE LN-STMT-COUNTER TO D-3B-COUNT.
           MOVE "S:" TO D-3C-P1.
           PERFORM DISPLAY-COMPONENT-EARNINGS.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "OTHER    BAL   :"  TO D-P3A.
           MOVE LN-OTHBAL           TO D-3A-AMT.
           MOVE "M:"                TO D-3C-P1.
           PERFORM DISPLAY-COMPONENT-EARNINGS.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "OTHER 2  BAL   :" TO D-P3A.
           MOVE LN-OT2BAL        TO D-3A-AMT.
           MOVE "D:"               TO D-3C-P1.
           PERFORM DISPLAY-COMPONENT-EARNINGS.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "LATE CHG PDTH  :" TO D-P3A.
           MOVE LN-LCPDTH-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO D-3A-PDTH.
           MOVE "RECAST INTEREST:" TO D-P3B.
           MOVE LN-RECASTINT TO D-3B-AMT.
           MOVE "O:" TO D-3C-P1.
           PERFORM DISPLAY-COMPONENT-EARNINGS.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "INTEREST PDTH  :" TO D-P3A.
           MOVE LN-INTPDTH-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO D-3A-DATE.
           MOVE "RECAST PRIN    :" TO D-P3B.
           MOVE LN-RECASTPRIN TO D-3B-AMT.
           MOVE TEXT3B TO D-COLUMN3CD.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "CUR 1ST DUE    :" TO D-P3A.

           MOVE LN-1STPYDATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
      * TO PLUG 1STPYDATE WITH UNITPER "S" OR "B" OR WHATEVER
           MOVE DATE-MMDDYY TO ALP-DATE-EDIT.
           MOVE LN-UNITPER-CD TO DATER-UNITPER-CD.
           MOVE LN-UNITPER-FREQ TO DATER-UNITPER-FREQ.
           PERFORM UNITPER-MASK.
           MOVE ALP-DATE-EDIT TO D-3A-ALP-DATE.

           MOVE "MONIES TO PDTH :" TO D-P3B.
           MOVE LN-TOTPAYMNTD TO D-3B-AMT.
           MOVE "TOTALS :" TO D-COLUMN3CD.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "NONCONTR MONIES:" TO D-P3B.
           MOVE LN-TOTEXCPAYMNTD TO D-3B-AMT.
           COMPUTE D-ANTIC = EARN-TOT-ANTICERN + EARN-TOT-ANTICADJ.
           MOVE EARN-TOT-ANTICADJ TO D-ADJ.
           MOVE EARN-TOT-ANTICERN TO D-NET.
           MOVE EARN-TOT-ACCERN TO D-EARN.
           MOVE EARN-TOT-UNEARN TO D-UNEARN.
           MOVE EARN-TOT-UFERN TO D-UFRONT.
           PERFORM WRITE-DETAIL-LINE.

           STRING "---------------------- LOAN",
             " TOTALS ---------------------!" DELIMITED BY "!"
                  INTO D-COLUMN3AB.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "EARNINGS       :" TO D-P3A.
           MOVE LN-TOTERN TO D-3A-AMT.
           MOVE "OTHER CHGS COLL:" TO D-P3B.
           MOVE LN-TOTOTH TO D-3B-AMT.
           MOVE "LAST ACCRUAL DATE      :" TO D-P3C.
           MOVE LN-LAST-ACCRUAL-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO D-3C-DATE.
           MOVE "PRIOR MONTHEND INTEREST"   TO D-4D2-TEXT.
           MOVE LN-LMO-PRIOR-INTEREST       TO D-4D2-AMT1.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "ANTIC INCOME   :" TO D-P3A.
           COMPUTE D-3A-AMT = LN-ACCERN(1) + LN-ACCERN(2) +
                              LN-ACCERN(3) + LN-ACCERN(4) +
                              LN-ACCERN(5) + LN-ACCERN(6).
           MOVE "TOT MO/TRAN DEF:" TO D-P3B.
           MOVE LN-TOTNODEF TO D-3B-COUNT-1.
           MOVE LN-TOTNODEFACT TO D-3B-COUNT-2.
           MOVE "PRIOR MONTHEND UNEARNED:" TO D-P3C.
           MOVE LN-PRIOR-UNEARNED TO D-3C-AMT.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "INTEREST COLLCT:" TO D-P3A.
           MOVE LN-TOTINT TO D-3A-AMT.
           MOVE "YTD MO/TRAN DEF:" TO D-P3B.
           MOVE LN-YTDNODEF    TO D-3B-COUNT-1.
           MOVE LN-YTDNODEFACT TO D-3B-COUNT-2.
           MOVE "ACCR EARN DELIQ STOP CD:" TO D-P3C.
           MOVE LN-ERNDELQ-STOP TO D-3C-FLAG.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "# LATE CHG COLL:" TO D-P3A.
           MOVE LN-TOTNOLCHG TO D-3A-AMT0.
           MOVE "INTEREST PAID  :" TO D-P3B.
           MOVE LN-TOTINTPD TO D-3B-AMT.
           MOVE "NON ACCRUAL EARNINGS   :" TO D-P3C.
           MOVE LN-NONACRERN TO D-3C-AMT.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "LATE CHG COLLCT:" TO D-P3A.
           MOVE LN-TOTLCHG TO D-3A-AMT.
           MOVE "YTD INT. PAID  :" TO D-P3B.
           MOVE LN-YTDINTPD TO D-3B-AMT.
           MOVE "PRIOR NON ACCRUAL EARN :" TO D-P3C.
           MOVE LN-PRIOR-NONACRERN TO D-3C-AMT.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "DEFER COLLECTED:" TO D-P3A.
           MOVE LN-TOTDEF TO D-3A-AMT.
           MOVE "LYR INT. PAID  :" TO D-P3B.
           MOVE LN-LYRINTPD TO D-3B-AMT.
           MOVE "IB ASSESSED THRU       :" TO D-P3C.
           MOVE LN-ERN-IB-PDTH-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO D-3C-AMT.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "SERVACCT INCOME:" TO D-P3A.
           MOVE LN-TOTSERACCT TO D-3A-AMT.
           MOVE "IB PRINCIPAL BALANCE   :" TO D-P3C.
           MOVE LN-ERN-IB-PRINBAL TO D-3C-AMT.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "INTEREST WAIVER:" TO D-P3A.
           MOVE LN-INTWAIVERS TO D-3A-AMT.
           MOVE "IB INTEREST OWING      :" TO D-P3C.
           MOVE LN-ERN-IB-INTOWE TO D-3C-AMT.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "LATE CHG WAIVER:" TO D-P3A.
           MOVE LN-LCWAIVERS TO D-3A-AMT.
           MOVE "PRIOR DLDISC UNEARNED  :" TO D-P3C.
           MOVE LN-PRDLDISC-UNEARN TO D-3C-AMT.
           PERFORM WRITE-DETAIL-LINE.

           PERFORM CHECK-PAGE.
           MOVE DASH-LINE TO DETAIL-LINE.
           PERFORM WRITE-DETAIL-LINE.

      *--------------PAGE 7 AND 8------------------------------------
           MOVE 2                          TO LN-FEED.
           MOVE LPS-DISPLAY-LNNO           TO D2-LOAN-NUM.
           MOVE "SALES FIN. AND REBATES "  TO D2-TITLE.
           MOVE 7                          TO D2-PAGE.
           MOVE "COLLECT/SOLICIT/PRIOR"    TO D2-TITLE2.
           MOVE 8                          TO D2-PAGE2.
           MOVE HEAD4 TO PR-REC.
           PERFORM WRITE-PR-REC.
           MOVE 2                          TO LN-FEED.

           MOVE "SALES FINANCE PURCHASE INFO:"  TO D-COLUMN4A.
           MOVE "REFUND INFORMATION:"           TO D-COLUMN4B.
           MOVE "COLLECTION INFORMATION:"       TO D-COLUMN4C.
           MOVE "SOLICITATION INFORMATION"      TO D-COLUMN4D.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "DEALER NUMBER  :"   TO D-P4A.
           MOVE LN-DLNO              TO D-4A-AMT.
           MOVE "TRW DOFD       :"   TO D-P4B.
           MOVE LN-TRW-DOFD-DATE     TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY          TO D-4B-DATE.
           MOVE "COLLECTOR'S ID  :"  TO D-P4C.
           MOVE LN-COLLECTOR         TO D-FIELD4C.
           MOVE "HIGH CREDIT LIM:"   TO D-P4D.
           MOVE LN-CREDITLIM         TO D-4D-COUNT.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "RSV FROM PRIN 1:"   TO D-P4A.
           MOVE LN-DLRVPRIN(1)       TO D-4A-AMT.
           MOVE "ACTION CODE     :"  TO D-P4C.
           MOVE LN-ACTIONCD          TO D-FIELD4C.
           MOVE "CREDIT SCORE NO:"   TO D-P4D.
           MOVE LN-CRSCORE           TO D-4D-COUNT.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "RSV FROM PRIN 2:"  TO D-P4A.
           MOVE LN-DLRVPRIN(2)      TO D-4A-AMT.
           MOVE "CL REBATE FLAG :"  TO D-P4B.
           MOVE LN-CL-REBATE        TO D-FIELD4B.
           MOVE "PROMISED DATE/ID:" TO D-P4C.
           MOVE LN-PROMDATE         TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY         TO D-4C-DATE.
           MOVE LN-PROMID           TO D-4C-CODE.
           MOVE "LAST FORM SENT :"  TO D-P4D.
           MOVE LN-LASTSFORMNO      TO D-4D-COUNT.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "RSV FROM PRIN 3:"  TO D-P4A.
           MOVE LN-DLRVPRIN(3)      TO D-4A-AMT.
           MOVE "AH REBATE FLAG :"  TO D-P4B.
           MOVE LN-AH-REBATE        TO D-FIELD4B.
           MOVE "PROMISED AMOUNT :" TO D-P4C.
           MOVE LN-PROMAMT          TO D-4C-AMT.
           MOVE "DATE FORM SENT :"  TO D-P4D.
           MOVE LN-LASTSFORMDATE    TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY         TO D-4D-DATE.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "RSV FROM CHGS 1:"  TO D-P4A.
           MOVE LN-DLRVCHGS(1)      TO D-4A-AMT.
           MOVE "PP REBATE FLAG :"  TO D-P4B.
           MOVE LN-PP-REBATE        TO D-FIELD4B.
           MOVE "PROMISED DATE #2:" TO D-P4C.
           MOVE LN-PROMDATE2        TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY         TO D-4C-DATE.
           MOVE "SOLICT FORM CNT:"  TO D-P4D.
           MOVE LN-SOLCNT           TO D-4D-COUNT.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "RSV FROM CHGS 2:"   TO D-P4A.
           MOVE LN-DLRVCHGS(2)       TO D-4A-AMT.
           MOVE "O1 REBATE FLAG :"   TO D-P4B.
           MOVE LN-O1-REBATE         TO D-FIELD4B.
           MOVE "LAST PMT DATE   :"  TO D-P4C.
           MOVE LN-DATE-PAID-LAST    TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY          TO D-4C-DATE.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "RSV FROM CHGS 3:"   TO D-P4A.
           MOVE LN-DLRVCHGS(3)       TO D-4A-AMT.
           MOVE "O2 REBATE FLAG :"   TO D-P4B.
           MOVE LN-O2-REBATE         TO D-FIELD4B.
           MOVE "LAST PMT AMT    :"  TO D-P4C.
           MOVE LN-AMT-PAID-LAST     TO D-4C-AMT.
           MOVE "PROFILES:              " TO D-COLUMN4D.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "PART RSV HELD  :"  TO D-P4A.
           MOVE LN-DLPARVHELD       TO D-4A-AMT.
           MOVE "O3 REBATE FLAG :"  TO D-P4B.
           MOVE LN-O3-REBATE        TO D-FIELD4B.
           MOVE "LAST PY/PA DATE :" TO D-P4C.
           MOVE LN-LAST-PYPA-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY         TO D-4C-DATE.
           MOVE "CONTRACTUAL"       TO D-4D-PROFILE2A-ALP.
           MOVE "RECENCY"           TO D-4D-PROFILE2B-ALP.
           PERFORM WRITE-DETAIL-LINE.


           MOVE "PART RSV PAID  :"   TO D-P4A.
           MOVE LN-DLPARVPAID        TO D-4A-AMT.
           MOVE "O4 REBATE FLAG :"   TO D-P4B.
           MOVE LN-O4-REBATE         TO D-FIELD4B.
           MOVE "LAST COLC FORM  :"  TO D-P4C.
           MOVE LN-LASTCFORMDATE     TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY          TO D-4C-DATE.
           IF LN-LASTCFORMNO NOT = 0
              MOVE LN-LASTCFORMNO    TO D-4C-NUM.
           MOVE LN-CONPROFILE(1)     TO D-4D-PROFILE2A
           INSPECT D-4D-PROFILE2A-ALP REPLACING ALL '9' BY '-'.
           MOVE LN-RECPROFILE(1)     TO D-4D-PROFILE2B
           INSPECT D-4D-PROFILE2B-ALP REPLACING ALL '9' BY '-'.
           PERFORM WRITE-DETAIL-LINE.

           IF GP-DLR-DISP-TYPE = "C"
              MOVE "DEALER COMM    :"   TO D-P4A
           ELSE
              MOVE "DEALER DISCOUNT:"   TO D-P4A.
           MOVE LN-DLDISC            TO D-4A-AMT.
           MOVE "O5 REBATE FLAG :"   TO D-P4B.
           MOVE LN-O5-REBATE         TO D-FIELD4B.
           MOVE "DEL EXTRACT CNT :" TO D-P4C.
           MOVE LN-DELCNT           TO D-4C-COUNT.
           MOVE LN-CONPROFILE(2)     TO D-4D-PROFILE2A
           INSPECT D-4D-PROFILE2A-ALP REPLACING ALL '9' BY '-'.
           MOVE LN-RECPROFILE(2)     TO D-4D-PROFILE2B
           INSPECT D-4D-PROFILE2B-ALP REPLACING ALL '9' BY '-'.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "RECOURSE CODE  :"   TO D-P4A.
           MOVE LN-DLRECOURSE        TO D-FIELD4A.
      *--->MOVE "O6 REBATE FLAG :"   TO D-P4B.
      *--->MOVE LN-O6-REBATE         TO D-FIELD4B.
           MOVE "DEL FORM CNT    :" TO D-P4C.
           MOVE LN-DELCNT2          TO D-4C-COUNT.
           MOVE LN-CONPROFILE(3)     TO D-4D-PROFILE2A
           INSPECT D-4D-PROFILE2A-ALP REPLACING ALL '9' BY '-'.
           MOVE LN-RECPROFILE(3)     TO D-4D-PROFILE2B
           INSPECT D-4D-PROFILE2B-ALP REPLACING ALL '9' BY '-'.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "PART RECOURSE  :"  TO D-P4A.
           MOVE LN-DLPARTRCAMT      TO D-4A-AMT.
      *--->MOVE "O7 REBATE FLAG :"  TO D-P4B.
      *--->MOVE LN-O7-REBATE        TO D-FIELD4B.
           MOVE "TOT ALLOW DELINQ:"  TO D-P4C.
           MOVE LN-TOTNOALDEL        TO D-4C-COUNT.
           MOVE LN-CONPROFILE(4)     TO D-4D-PROFILE2A
           INSPECT D-4D-PROFILE2A-ALP REPLACING ALL '9' BY '-'.
           MOVE LN-RECPROFILE(4)     TO D-4D-PROFILE2B
           INSPECT D-4D-PROFILE2B-ALP REPLACING ALL '9' BY '-'.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "DEALER POOL    :"  TO D-P4A.
           MOVE LN-DLPOOL           TO D-4A-COUNT.
           MOVE "IN REBATE FLAG :"  TO D-P4B.
           MOVE LN-INT-REBATE       TO D-FIELD4B.
           MOVE "YTD ALLOW DELINQ:"  TO D-P4C.
           MOVE LN-YTDNOALDEL        TO D-4C-COUNT.
           MOVE LN-CONPROFILE(5)     TO D-4D-PROFILE2A
           INSPECT D-4D-PROFILE2A-ALP REPLACING ALL '9' BY '-'.
           MOVE LN-RECPROFILE(5)     TO D-4D-PROFILE2B
           INSPECT D-4D-PROFILE2B-ALP REPLACING ALL '9' BY '-'.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "DEALER CHK AMT :"   TO D-P4A.
           IF LN-SALEFIN = "Y"
              MOVE LN-PROCEEDS       TO D-4A-AMT.
           MOVE "SC REBATE FLAG :"   TO D-P4B.
           MOVE LN-SC-REBATE         TO D-FIELD4B.
           MOVE "FOLLOWUP DATE   :"  TO D-P4C.
           MOVE LN-FOLLOWUP-DATE     TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY          TO D-4C-DATE.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "ORIG PURCH AMT :"  TO D-P4A.
           MOVE LN-ORIGPURAMT       TO D-4A-AMT.
           MOVE "MF REBATE FLAG :"  TO D-P4B.
           MOVE LN-MF-REBATE        TO D-FIELD4B.
           MOVE "ORIGIN          :" TO D-P4C.
           MOVE LN-ORIGIN           TO D-FIELD4C.
           MOVE "LN PROC OFFICE :"  TO D-P4D.
           MOVE LN-LPO              TO D-FIELD4D.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "ORIG PURCH TERM:"  TO D-P4A.
           MOVE LN-ANTICTERM        TO D-4A-COUNT.
           MOVE "DF REBATE FLAG :"  TO D-P4B.
           MOVE LN-DF-REBATE        TO D-FIELD4B.
           MOVE "SCREEN BEEP MESS:" TO D-P4C.
           MOVE "REP CODE       :"  TO D-P4D.
           MOVE LN-REP-CD           TO D-FIELD4D.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "PURCHASE BAL   :"  TO D-P4A.
           MOVE LN-PURBAL           TO D-4A-AMT.
           MOVE LN-MESSAGE          TO D-COLUMN4C.
           MOVE "PROCESS CODE   :"  TO D-P4D.
           MOVE LN-PROCESS-CD       TO D-4D-FG.
           PERFORM WRITE-DETAIL-LINE.

           PERFORM WRITE-DETAIL-LINE.

           PERFORM CHECK-PAGE.
           MOVE DASH-LINE TO DETAIL-LINE.
           PERFORM WRITE-DETAIL-LINE.

      *--- PAGE 10 & 11 -------------------------------------------------
           MOVE LPS-DISPLAY-LNNO TO D3-LOAN-NUM.
           PERFORM LPSTAT-DISPLAY.
           MOVE LPS-DISPLAY-LNNO TO D3-LOAN-NUM.
           MOVE "ADDON INSURANCE INFO   " TO D2-TITLE.
           MOVE SPACES TO D3-TITLE2.
           MOVE 9  TO D3-PAGE.
      *    MOVE 10 TO D2-PAGE2.
           MOVE HEAD5 TO DETAIL-LINE.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "CONSOLIDATED TOTALS:" TO D-COLUMN4A.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "LOAN AMOUNT    :" TO D-P4A.
           MOVE LN-SUM-LNAMT TO D-4A-AMT.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "FINANCE  CHARGE:" TO D-P4A.
           MOVE LN-SUM-FINCHG TO D-4A-AMT.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "INTEREST CHARGE:" TO D-P4A.
           MOVE LN-SUM-INTCHG TO D-4A-AMT.
           PERFORM WRITE-DETAIL-LINE.

           MOVE HEAD-10 TO DETAIL-LINE.
           MOVE "SERVICE  CHARGE:" TO D-P4A.
           MOVE LN-SUM-SERCHG TO D-4A-AMT.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "EXTENSION CHG  :" TO D-P4A.
           MOVE LN-SUM-EXTCHG TO D-4A-AMT.
           PERFORM WRITE-DETAIL-LINE.

           MOVE LN-OWNBR  TO LTC-BRNO.
           MOVE LN-ACCTNO TO LTC-ACCTNO.
           MOVE 1         TO LTC-SEQNO.
           PERFORM READ-LTC1-FILE.
           IF IO-FG NOT = 0
              MOVE SPACES TO LTC-COLLATERAL-INFO.

           MOVE LTC-DESC(1)  TO D-DESCRIPTION.
           MOVE LTC-MAKE(1)  TO D-MAKE.
           MOVE LTC-MODEL(1) TO D-MODEL.
           MOVE LTC-YEAR(1)  TO D-YEAR.
           MOVE LTC-STYLE(1) TO D-STYLE.
           MOVE LTC-CLASS(1) TO D-LTCLASS.
           MOVE LTC-VIN(1)   TO D-ID.
           MOVE "MAINT FEE      :" TO D-P4A.
           MOVE LN-SUM-MAINTFEE TO D-4A-AMT.
           PERFORM WRITE-DETAIL-LINE.

           MOVE LTC-DESC(2)  TO D-DESCRIPTION.
           MOVE LTC-MAKE(2)  TO D-MAKE.
           MOVE LTC-MODEL(2) TO D-MODEL.
           MOVE LTC-YEAR(2)  TO D-YEAR.
           MOVE LTC-STYLE(2) TO D-STYLE.
           MOVE LTC-CLASS(2) TO D-LTCLASS.
           MOVE LTC-VIN(2)   TO D-ID.
           MOVE "ORIGINAL TERM  : !" TO D-P4A.
           MOVE LN-SUM-ORGTERM TO D-4A-COUNT.
           PERFORM WRITE-DETAIL-LINE.

           MOVE LTC-HHGS(1) TO D-COLLAT.
           MOVE "ORIGINAL PURAMT:" TO D-P4A.
           MOVE LN-SUM-ORIGPURAMT TO D-4A-AMT.
           PERFORM WRITE-DETAIL-LINE.

           MOVE LTC-HHGS(2) TO D-COLLAT.
           MOVE "REG PAYMENT AMT:" TO D-P4A.
           MOVE LN-SUM-REGPYAMT TO D-4A-AMT.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "CONFIRM. DATE  :"  TO D-P4A.
           MOVE LN-INS-CONFIRM-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY         TO D-4A-DATE.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "MONEYS APPLIED TO  :" TO D-COLUMN4A.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "PRINCIPAL      :" TO D-P4A.
           MOVE LN-MTD-PRIN        TO D-4A-AMT.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "LATE CHARGES   :" TO D-P4A.
           MOVE LN-MTD-LC          TO D-4A-AMT.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "INTEREST       :" TO D-P4A.
           MOVE LN-MTD-INT         TO D-4A-AMT.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "OTHER 1 & 2    :" TO D-P4A.
           MOVE LN-MTD-OT          TO D-4A-AMT.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "EXCEPTION PYMTS:" TO D-P4A.
           MOVE LN-MTD-EX          TO D-4A-AMT.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "MONTHEND BAL.  :" TO D-P4A.
           MOVE LN-MONTHEND-BAL    TO D-4A-AMT.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "BANKRUTCY CHAP.:" TO D-P4A.
           MOVE LN-BNKRPT-CHAPTER  TO D-4A-COUNT.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "BEG MNTH CON STAT:" TO D-P4A.
           MOVE LN-BMO-CON-STATUS TO D-4A-COUNT.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "BEG MNTH REC STAT:" TO D-P4A.
           MOVE LN-BMO-REC-STATUS TO D-4A-COUNT.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "APPLICATION NO.  :" TO D-P4A.
           MOVE LN-APP-NUMBER     TO D-4A-COUNT.
           PERFORM WRITE-DETAIL-LINE.

           PERFORM CHECK-PAGE.
           MOVE DASH-LINE TO DETAIL-LINE.
           PERFORM WRITE-DETAIL-LINE.

      *--- PAGE 10 AND PAGE 11 --------------------------

           MOVE LPS-DISPLAY-LNNO TO D2-LOAN-NUM.
           PERFORM LPSTAT-DISPLAY.
           MOVE "PRIOR LOAN INFO        " TO D2-TITLE.
           MOVE SPACES                    TO D2-TITLE2.
           MOVE 10 TO D2-PAGE.
           MOVE 11 TO D2-PAGE2.
           MOVE HEAD4 TO DETAIL-LINE.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "PRIOR LOAN INFORMATION:" TO D-COLUMN4A.
           MOVE SPACES                    TO D-COLUMN4C.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "CONVERTED ACCT#:"  TO D-P4A.
           MOVE LN-OLDACCTNO        TO D-4A-COUNT.
           MOVE "EXTRA PRIN LFP :"  TO D-P4B.
           MOVE LN-EXTRA-PRIN-LFP   TO D-4B-AMT1.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "PRIOR LOAN NUM.:"   TO D-P4A.
           MOVE LN-PRLNNO            TO D-4A-COUNT.
           MOVE "REM INT SIN LFP:"  TO D-P4B.
           MOVE LN-REMAIN-INT-SINCE-LFP TO D-4B-AMT1.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "PRIOR LOAN BAL :"   TO D-P4A.
           MOVE LN-PRBAL             TO D-4A-AMT.
           MOVE "REM PRN SIN LFP:"  TO D-P4B.
           MOVE LN-REMAIN-PRIN-SINCE-LFP TO D-4B-AMT1.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "INTEREST DUE   :"   TO D-P4A.
           MOVE LN-PRINTDUE          TO D-4A-AMT.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "FIN CHG REBS   :"  TO D-P4A.
           MOVE LN-PRFINCHGRB       TO D-4A-AMT.
           MOVE "PR  RN FOR <10%:"  TO D-P4B.
           MOVE LN-PRIOR-RN-FOR10-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY        TO D-4B-AMT1.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "INSURANCE REBS :"  TO D-P4A.
           MOVE LN-PRINSRB          TO D-4A-AMT.
           MOVE "LST RN FOR <10%:"  TO D-P4B.
           MOVE LN-LAST-RN-FOR10-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY         TO D-4B-AMT1.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "PAID OFF CODE  :"   TO D-P4A.
           MOVE LN-PRPOCD            TO D-FIELD4A.
           MOVE "WCBC CREDIT LIM:" TO D-P4B.
           MOVE LN-WCBC-CREDITLIM TO D-4B-AMT1.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "PAID OFF DATE  :"  TO D-P4A.
           MOVE LN-PRPODATE         TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY         TO D-4A-DATE.
           MOVE "INTERIM COUPON :" TO D-P4B.
           MOVE LN-INTERIM-COUPON TO D-4B-FLAG.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "PRIOR LOAN AMT :"  TO D-P4A.
           MOVE LN-PRLNAMT          TO D-4A-AMT.
           MOVE "MIP FLAG       :"  TO D-P4B.
           MOVE LN-MIP-FG           TO D-4B-FLAG.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "PRIOR P/DUE DAY:"  TO D-P4A.
           MOVE LN-PRPDUE-DAYS      TO D-4A-COUNT.
           MOVE "MIP MTD PAYMENT:"  TO D-P4B.
           MOVE LN-MIP-MTD-CLAHPAID TO D-4B-AMT1.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "PRIOR DLDSC UNE:"  TO D-P4A.
           MOVE LN-PRDLDISC-UNEARN  TO D-4A-AMT.
           MOVE "MIP CL PDTH    :"  TO D-P4B.
           MOVE LN-MIP-CLPDTH-DATE  TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY         TO D-4B-DATE.
           MOVE "CREATION STAT   :" TO D-P4D.
           MOVE LN-CREATION-STATUS  TO D-4D-FG.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "MIP TOT CL PAYS:"  TO D-P4B.
           MOVE LN-MIP-TOT-CLPAID  TO D-4B-AMT1.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "MIP AH PDTH    :"  TO D-P4B.
           MOVE LN-MIP-AHPDTH-DATE  TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY         TO D-4B-DATE.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "MIP TOT AH PAYS:"  TO D-P4B.
           MOVE LN-MIP-TOT-AHPAID  TO D-4B-AMT1.
           MOVE "CREDIT LIMIT   :"  TO D-P4D.
           MOVE LN-CREDITLIM        TO D-4D-AMT.
           PERFORM WRITE-DETAIL-LINE.

           PERFORM WRITE-DETAIL-LINE.

           PERFORM WRITE-DETAIL-LINE.

           MOVE SPACES               TO D-P4C D-FIELD4C.
           PERFORM WRITE-DETAIL-LINE.

           PERFORM CHECK-PAGE.
           MOVE DASH-LINE TO DETAIL-LINE.
           PERFORM WRITE-DETAIL-LINE.

      *---------- PAGE 12  
           MOVE LPS-DISPLAY-LNNO TO D2-LOAN-NUM.
           PERFORM LPSTAT-DISPLAY.
           MOVE 12                       TO D3-PAGE.
           MOVE HEAD5                    TO DETAIL-LINE.
           PERFORM WRITE-DETAIL-LINE.

           PERFORM WRITE-DETAIL-LINE.

           PERFORM WRITE-DETAIL-LINE.

           PERFORM WRITE-DETAIL-LINE.


       END-DETAIL-LN-PROCESS.
           EXIT.

       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/TIMEIO.CPY".
       COPY "LIBLP/LPSTAT.CPY".
       COPY "LIBLP/LPMDTE.CPY".
       COPY "LIBLP/LPITRM.CPY".
       COPY "LIBLP/LPEARN.CPY".

      *******************************************
       DISPLAY-BORROWER SECTION.
      *******************************************
           MOVE LN-SSNO(SUB) TO BW-SSNO.
           IF SUB NOT = 1
              PERFORM READ-BW1-FILE.
           IF LN-SSNO(SUB) NOT = 0
              MOVE BW-SSNO TO D-1A-SSNO
              INSPECT D-1A-SSNO REPLACING ALL "/" BY "-"
              MOVE LN-MAKERCD(SUB) TO D-1A-MAKERCD.

      ****************************
       WRITE-DETAIL-LINE SECTION.
      ****************************
           MOVE DETAIL-LINE TO PR-REC.
           PERFORM WRITE-PR-REC.
           ADD LN-FEED TO LN-COUNT.
           MOVE 1 TO LN-FEED.
           MOVE SPACES TO DETAIL-LINE.

      *******************************
       HEAD-PAGE SECTION.
      *******************************

           MOVE "N" TO FIRST-TIME.
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

       END-HEAD-PAGE.
           MOVE 2 TO LN-FEED.
           MOVE 1 TO LN-COUNT.

      *******************************
       PRINT-RANGE-LINES SECTION.
      *******************************

           MOVE 4 TO LN-FEED.
           IF SEQ = 1
              GO TO ACCT-RANGE.
           IF SEQ = 2
              GO TO CLASS-RANGE.

           MOVE "BEGINNING BRANCH, CLASS, ACCOUNT #" TO RANGE-LINE.
           MOVE EXT-FILPATH-BRNO TO R-BRANCH.   
           MOVE BEG-CLASS TO R-CLASS.
           MOVE BEG-ACCT TO R-ACCT.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "ENDING BRANCH, CLASS, ACCOUNT #" TO RANGE-LINE.
           MOVE EXT-FILPATH-BRNO TO R-BRANCH.  
           MOVE END-CLASS TO R-CLASS.
           MOVE END-ACCT TO R-ACCT.
           PERFORM WRITE-DETAIL-LINE.

           GO TO END-RANGE.

       CLASS-RANGE.
           MOVE "BEGINNING CLASS, ACCOUNT #" TO RANGE-LINE.
           MOVE BEG-CLASS TO R-CLASS.
           MOVE BEG-ACCT TO R-ACCT.
           PERFORM WRITE-DETAIL-LINE.
           MOVE "ENDING CLASS, ACCOUNT #" TO RANGE-LINE.
           MOVE END-CLASS TO R-CLASS.
           MOVE END-ACCT TO R-ACCT.
           PERFORM WRITE-DETAIL-LINE.
           GO TO END-RANGE.

       ACCT-RANGE.
           MOVE "               BEGINNING ACCOUNT #" TO RANGE-LINE.
           MOVE BEG-ACCT TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
           MOVE "               ENDING ACCOUNT #" TO RANGE-LINE.
           MOVE END-ACCT TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.

       END-RANGE.
           EXIT.

      ************************************************************
       CLASS-SCAN SECTION.
      ************************************************************
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


       COPY "LIBGB/GPENV.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBLP/UPERTST.CPY".

      ****************************************************
       FORM-RESET SECTION.
      ****************************************************
           MOVE EXT-CONAME-CONAME TO LNLIST-CONAME.
           MOVE EXT-CONAME-SYSDATE TO LNLIST-SYSDATE.
           DISPLAY LNLIST-SCREEN UPON LNLIST-WINDOW-HANDLE.
           MOVE LNLIST-FORM-FIELDS TO WR-BUF.
           MOVE LNLIST-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      ****************************************************
       SETUP-LINK-INFO SECTION.
      ****************************************************
           MOVE LNLIST-HELP--FILE TO HELP-LINK-FILE.
           MOVE LNLIST-HELP--DIR  TO HELP-LINK-DIR.

      ****************************************************
       VDU-CONVERT-FIELD SECTION.
      ****************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBLS/LNLIST_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      *********************************
       MISC SECTION.
      *********************************
       COPY "LIBGB/GETPR.CPY".
       COPY "LIBGB/ACCESS.CPY".
       GET-CODE.
           MOVE "GC"     TO CDB-TYPE.
           MOVE LN-OWNBR TO CDB-BRANCH.
      *--> SET BEFORE       CDB-CODE.
           PERFORM READ-CDB1-FILE.
           IF IO-FG NOT = 0
              MOVE SPACES TO CDB-DESC.
      *----------------------------------------------------------
       SET-FEE-LEFT.
           MOVE LN-FEECODE(SUB) TO D-FEECD1 CDB-CODE.
           PERFORM GET-CODE.
           MOVE CDB-DESC TO D-FEEDESC1.
           MOVE LN-FEEAMT(SUB) TO D-FEEAMT1.
           MOVE LN-FEEOURS(SUB) TO D-FEEOURS1.
      *----------------------------------------------------------
       SET-FEE-RIGHT.
           MOVE LN-FEECODE(SUB) TO D-FEECD2 CDB-CODE.
           PERFORM GET-CODE.
           MOVE CDB-DESC TO D-FEEDESC2.
           MOVE LN-FEEAMT(SUB) TO D-FEEAMT2.
           MOVE LN-FEEOURS(SUB) TO D-FEEOURS2.
      *----------------------------------------------------------
       SET-INSURANCE.
           PERFORM INS-TERM-CALCULATION.
           MOVE ITRM-INSTERM TO D-INSTERM.
           MOVE LN-INSCOMP(SUB) TO D-INSCO.
           MOVE LN-INSOURS(SUB) TO D-INSOURS.
           MOVE LN-INSEFF-DATE(SUB) TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY     TO D-INSEFF.
           MOVE LN-INSEXP-DATE(SUB) TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY     TO D-INSEXP.
           MOVE LN-INSCOVR(SUB) TO D-INSCOVR.
           MOVE LN-INSPREM(SUB) TO D-INSPREM.
           MOVE LN-INSCOMM(SUB) TO D-INSCOMM.
      *---------------------------------------------------------
       DISPLAY-COMPONENT-EARNINGS.
           ADD 1 TO SUB.
           ADD LN-ANTICADJ(SUB) LN-ANTICERN(SUB) GIVING D-ANTIC.
           MOVE LN-ANTICADJ(SUB) TO D-ADJ.
           MOVE LN-ANTICERN(SUB) TO D-NET.
           MOVE LN-ACCERN(SUB) TO D-EARN.
           IF LN-PLCD = " " AND LN-SERVACCT NOT = "Y"
              SUBTRACT LN-ACCERN(SUB) FROM LN-ANTICERN(SUB)
                                       GIVING D-UNEARN
              IF D-UN-ALP NOT = " "
                 IF SUB = 1 AND
                    ( LN-ANTICADJ(1) NOT = 0
                            OR (NOT (LN-REBATE(4) = " " OR "C")) )
                    MOVE "  REBATED?" TO D-UN-ALP
                 ELSE
                 IF SUB = 3 AND
                    ( LN-ANTICADJ(3) NOT = 0
                            OR (NOT (LN-REBATE(5) = " " OR "C")) )
                    MOVE "  REBATED?" TO D-UN-ALP
                 ELSE
                 IF SUB = 4 AND
                    ( LN-ANTICADJ(4) NOT = 0
                            OR (NOT (LN-REBATE(6) = " " OR "C")) )
                    MOVE "  REBATED?" TO D-UN-ALP.
           MOVE LN-UFERN(SUB) TO D-UFRONT.

      *-------------------------------------------------------------
       CHECK-PAGE.
           ADD 1 TO LN-COUNT.
           IF LN-COUNT > 40
               PERFORM HEAD-PAGE.
           IF LN-COUNT = 2
               MOVE 6 TO LN-FEED
           ELSE
               MOVE 4 TO LN-FEED.

      *****************************************************
       IO-ROUTINE SECTION.
      *****************************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBLP/LPSPGS_SQL.CPY".
       COPY "LIBLP/LPLTCGS_SQL.CPY".
       COPY "LIBLP/LPLNGS_SQL.CPY".
       COPY "LIBLP/LPCDBGS_SQL.CPY".
       COPY "LIBLP/LPBWGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBPRINT.CPY".
       COPY "LIBLP/LPLN1RN.CPY".
       COPY "LIBLP/LPLN3RN.CPY".
       COPY "LIBLP/LPBW1RN.CPY".
       COPY "LIBLP/LPLTC1RN.CPY".
       COPY "LIBLP/LPCDB1RN.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBLP/LPSP1RN.CPY".
       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO ERRCD.

      *****************************************************
       END-PROGRAM SECTION.
      *****************************************************
       END-RUN.
           PERFORM CLOSE-FILES.
           PERFORM PRU-OUT.
       EXIT-PROG.
           MOVE "LP/LPL2MU" TO FORM-NAM.
           MOVE FORM-PATH TO FORM-PATHNAME.
       STOP-RUN.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY LNLIST-SCREEN.
           DESTROY LNLIST-WINDOW-HANDLE.
           EXIT PROGRAM.
