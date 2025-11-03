       IDENTIFICATION DIVISION.
       PROGRAM-ID.      LONPP0.
       DATE-WRITTEN.    051484
      *****************************************************************
      *                      (LP)
      *    DESCRIPTION:  LOAN/TRANSACTION INQUIRY
      *                  LEDGER CARD PRINT
      * REV:
      *  MJD 042596 ADDED GP-FILE, GP-DLR-DISP-TYPE.
      *  MJD 071196 ADDED TEST OF LN-UNITPER-CD FOR WEEKLY PDTH
      *  MJD 071696 ADDED LIBLP/LPUPWK FOR CHANGES IN LIBLP/AGEING
      *  MJD 071796 MODIFIED FOR USAGE OF 6 DIGIT PDTH DATES
      *  MJD 050697 ADDED "ST" TRCD FOR ADDONS
      *  BAH 050897 ALWAYS PRINT BALANCE, EVEN ON REVERSALS, #487
      *  MJD 082297 CHANGED LN-REC/CONPROFILE TO LN-REC/CONPROFILE(1)
      *  GLF 090897 REPLACED 'IO' ON GP-FILE WITH GET 'GPENV'
      *  BAH 092397 WAS NOT TESTING GP-NO-LC-IN-ARREARAGE FOR TODAY DUE...
      *  GLF 101697 ADDED COPY MEMBER: LIBLP/LPPDUE LIBLP/LPPDUEW
      *  BAH 123197 FIXED PRINT OF COLLATERAL CODE
      *  BAH 032098 IF GP-ASK-HARD-COPY-MEMOS = "Y", A WINDOW (LONPP)
      *             OPENS AND ASKS IF YOU WANT MEMOS Y/N..WORLD #183
      *  BLV 041598 FIXED POTS & MISPAYS PRINTING AS CURRENT
      *  BAH 060398 1STPYDATE WAS NOT MASKING UNITPER, WEEKLY, BIWEEKLY, ETC..
      *  GLF 061998 REPLACED PRINT OF SELLPR WITH PRINT OF AMT FIN, WORLD #186
      *  GLF 110998 MADE MEMO DATE 2000 COMPATIBLE
      *  JTG 121698 DISPLAYED 'R' VIA UNITPER-MASK LOGIC FOR REVOLVING ACCOUNT
      *  JTG 990218 ADDED LOGIC TO STOP LAST DUE & NEXT DUE FROM DISPLAYING
      *  BAH 000420 OTHER INSURANCE WAS NOT INCLUDING O3, FAX FROM JR (WORLD)
      *  JTG 010507 ALLOWED ENTRY OF 'TRADEIN' RE:SALE PRICE-DP-TI PAACO PR#1467
      *  JTG 011102 ADDED ENVIRONMENT VARIABLE 'LONPP0_PAYDATE_ONLY'
      *  CS  020301 AH NOT PRINTING IF NO C/L.  MISSING PERIOD [KEVIN]
      *  BAH 020712 SIGNED LN-TRADEIN, REGACC PR# 1808
      *  BAH 050628 GET-ENDORSER WAS WIPING OUT MAKER INFO AND OTHER PAGES
      *             WERE THEN PRINTING THE ENDORSER AS THE MAKER, REGWAN PL#500
      *  BAH 050713 ADDED GP-HIDE-FIRST-5-SSNO TO "X" OUT FIRST 5 OF SSNO IF
      *             SET TO "Y", REGACC PR# 109
      *  BAH 051103 ASK QUESTION TO PRINT PAYMENT REVERSALS, PR# 2628
      *  BAH 060719 ADD OPTION TO PRINT ONLY NSF REVERSALS "O", MIDWEST PR#2817
      *  BAH 060816 ASK QUESTION TO CREATE FILE ONLY IN /USR/TMP TO PRINT AT
      *             A LATER TIME THRU F6, SO THEY CAN USE ANY PRINTER, NOT
      *             JUST A DIRECT PRINT PRINTER, REGACC PR# 140
      *  BAH 061129 DONT CREATE THE WORK FILE UNLESS THEY ENTER "Y" TO ONLY
      *             CREATE OUTPUT FILE, IT WAS CLOGGING UP /USR/TMP AND LOCKING
      *             THEM UP...JUDY
      *  BLF 070702 IF GP-I-AM-WORLD, DON'T ASK QUESTION FOR "OUTPUT FILE ONLY",
      *             WORLD, PER JR
      *  BAH 070718 IF IP-PROG = "FS" (FSB), PRINT ALTERNATE FSB PAYMENTS FROM
      *             THE MPFILE, CALLED FROM WI/FSBMP  REGACC PR# 154
      *  BLF 070910 CHECK IP-BUF - IF VALUE IS "AR", SET UP ARCHIVE PATHS FOR
      *             LN, LP, LM & LT, WORLD PR# 547
      *  BAH 090119 ADDED VALUE OF "C" TO PRINT PAYMENT REVERSALS FOR A
      *             CUSTOMER VERSION THAT DOESNT PRINT REVERSALS OR BACKDATES
      *             AND PRINTS IN 132 MODE SHOWING INT & LC BROKE OUT, REGACC
      *                                                        REGACC PR# 234
      *  BAH 090209 ADDED VALUE OF "I" TO THE PRINT PAYMENT REVERSALS FLAG FOR
      *             AN INSURANCE VERSION THAT DOESNT PRINT REVERSALS OR
      *             BACKDATES, PRINTS THE STANDARD TOP HALF AND PRINTS THE
      *             CUSTOMER VERSION PAYMENTS (132 MODE) REGACC PR# 239
      *   CS 091007 ADDED PRINT OF RSV PRIN3 AND LN-COSTS(LN-ANTICERN(6))
      *              MULLEN PR# 3293
      *   CS 120328 LOCKED UP CREATING OUTPUT FILE WHEN REQUESTING MULTIPLE
      *             THREE. THE MKTEMP-CALL DID NOT COME BACK WITH A UNIQUE ID
      *             ON THE 2ND ACCT. ON THE 1ST ACCT /USR/TMP/LEDGERA002IC, ON
      *             THE 2ND ACCT ITS STILL A0002IC, BECAUSE A0002IC DOES NOT
      *             EXIST IN /USR/TMP.  INSTEAD OF MOVING MKTEMP-DEFAULT TO
      *             MKTEMP-BUF, MOVED LEDGERXXXXXX TO MKTEMP-BUF SO WE GET A
      *             UNIQUE ID... THANKS BARB   PEOPLES CREDIT PR#769
      *  BAH 160118 SPLIT OUT LTFILE, PD0003
      *
      * KEC 2016.0314 {PD#00003} PARADATA <A30> [JAY CISZEWSKI]
      *         CHANGED CD-FEE???? TO CD-GC-????.
      *
      * BAH 170629 ACTIVATE 8 DIGIT DATES, PD0006
      * BAH 2018.07.16 CHANGED CRNORC TO CRNOR2
      * BAH 181030 GLOBAL GBFILE, L2G
      * BAH 181128 GLOBAL LMFILE, L2G, COMMENTED OUT ARCHIVE
      * BAH 190102 GLOBAL LPFILE, L2G
      * BAH 190121 GLOBAL MPFILE, L2G
      * BAH 20190308 CHANGED LP-ADDON TO USE LPWSLP-LP-ADDON, MOVED LP-REFNO
      *              REDEFINITIONS TO LPWSLP COPY MEMBER
      * BAH 20190625 REMOVED LN-REVOLVING, PT0153
      * BAH 20190711 PRLNNO FORMAT NEEDS TO MATCH A15, WANT AUTO COLLATERAL TO
      *              PRINT FIRST LIKE A15, DEFERMENT MMYY OR MMDD WRONG FORMAT
      *                                                         QA0044
      * BAH 20220401 HAD TO CHANGE OPEN-WINDOW TO DISPLAY FLOATING WINDOW FOR
      *              THIS TO PRINT CORRECTLY CALLED FROM CLMENU, A WINDOW ON A
      *              WINDOW. IT WASNT EXITING CORRECTLY. WORKS IN BOTH CLMENU
      *              AND LONPF1.
      * BAH 20220803 HARD CODED START
      **************************************************************************
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
           "@(#)LP/LONPP0.CBL S35 12/09/22-14:51:25 >".
      ****************************
      *     F I L E   I F N      *
      ****************************
       COPY "LIBGB/GB01GB.CPY".
       COPY "LIBGB/GB01GB_SQL.CPY".
       COPY "LIBGB/GBWSGB.CPY".
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBLP/LP01DL.CPY".
       COPY "LIBLP/LP01DL_SQL.CPY".
       COPY "LIBLP/LPWSDL.CPY".
       COPY "LIBLP/LP01LN.CPY".
       COPY "LIBLP/LP01LN_SQL.CPY".
       COPY "LIBLP/LPWSLN.CPY".
       COPY "LIBLP/LP01LP.CPY".
       COPY "LIBLP/LP01LP_SQL.CPY".
       COPY "LIBLP/LPWSLP.CPY".
       COPY "LIBLP/LP01LTC.CPY".
       COPY "LIBLP/LP01LTC_SQL.CPY".
       COPY "LIBLP/LPWSLTC.CPY".
       COPY "LIBLP/LP01LTP.CPY".
       COPY "LIBLP/LP01LTP_SQL.CPY".
       COPY "LIBLP/LPWSLTP.CPY".
       COPY "LIBLP/LP01SP.CPY".
       COPY "LIBLP/LP01SPA.CPY".
       COPY "LIBLP/LP01SPB.CPY".
       COPY "LIBLP/LP01SPC.CPY".
       COPY "LIBLP/LP01SP_SQL.CPY".
       COPY "LIBLP/LPWSSP.CPY".
       COPY "LIBLP/LP01BW.CPY".
       COPY "LIBLP/LP01BW_SQL.CPY".
       COPY "LIBLP/LPWSBW.CPY".
       COPY "LIBLP/LP01CD.CPY".
       COPY "LIBLP/LP01CD_SQL.CPY".
       COPY "LIBLP/LPWSCD.CPY".
       COPY "LIBLP/LP01CDB.CPY".
       COPY "LIBLP/LP01CDB_SQL.CPY".
       COPY "LIBLP/LPWSCDB.CPY".
       COPY "LIBLP/LP01LM.CPY".
       COPY "LIBLP/LP01LM_SQL.CPY".
       COPY "LIBLP/LPWSLM.CPY".

      **********************
      *   LINE WORKERS     *
      **********************
       01  LN-WORKERS.
           03  LN-FEED          PIC 99       VALUE 0.
           03  LN-COUNT         PIC 99       VALUE 61.
           03  LN-WK            PIC 99       VALUE 0.
       01  PAGE-NUMBER          PIC 9(4)     VALUE 0.

      **********************
      *   MISC WORKERS     *
      **********************
       01  PAYDATE-ONLY-FG      PIC X        VALUE "N".
       01  SUB                  PIC 9(6) COMP-3.
       01  SUB2                 PIC 9(6) COMP-3.
       01  MESS-LIST            PIC X(5)     VALUE "MSEL.".
       01  WSEL-LIST            PIC X(5)     VALUE "WSEL.".

       01  NAME-WK              PIC X(24).
       01  NAME-LINE            PIC X(88).
       01  WRITE-DETAIL-HEADER  PIC X VALUE "N".
       01  PRINT-MEMOS          PIC X VALUE " ".
       01  PRINT-REVS           PIC X VALUE " ".
       01  PROFILE-NUM          PIC 9(12).
       01  PROFILE-ALP REDEFINES PROFILE-NUM
                                PIC X(12).
       01  DASH-LINE            PIC X(88) VALUE ALL "-".
       01  JOINT-SSNO           PIC 9(9)  COMP-3 VALUE 0.
       01  JOINT-NAME           PIC X(30) VALUE SPACES.
       01  ENDORSER-INFO        PIC X(88) VALUE SPACES.
       01  SAVE-LP-REC          PIC X(LP-SIZE).
       01  SAVE-RV-REC          PIC X(LP-SIZE).

      ***********************************
      *     P R I N T   B U F F E R S   *
      ***********************************
       01  HEAD1.
           03  H-DATE           PIC X(8)     VALUE " ".
           03  FILLER           PIC XX       VALUE " ".
           03  H-HR             PIC 99       VALUE 0.
           03  FILLER           PIC X        VALUE ":".
           03  H-MIN            PIC 99       VALUE 0.
           03  FILLER           PIC X(9)     VALUE " ".
           03  H-NAME           PIC X(40)    VALUE " ".
           03  FILLER           PIC X(16)    VALUE " ".
           03  FILLER           PIC X(5)     VALUE "PAGE ".
           03  H-PAGE-NO        PIC ZZ9.

       01  HEAD2.
           03  FILLER           PIC X(10)    VALUE "USER ID:".
           03  H-USERID         PIC X(10).
           03  FILLER           PIC X(4)    VALUE " ".
           03  H-TITLE            PIC X(40)    VALUE
           "- - - - - - ACCOUNT INQUIRY - - - - - - ".

       01  HEAD3.
           03  FILLER           PIC X(10)     VALUE "OWN BR.:".
           03  H-BRNO           PIC 9(4).
           03  FILLER           PIC X         VALUE " ".
           03  H-BRNAME         PIC X(20).

       01  HEAD4.
           03  FILLER           PIC X(35) VALUE
           "BORROWER:".
           03  FILLER           PIC X(8)  VALUE "ACCTNO: ".
           03  D-LOAN-NUM       PIC X(16).
           03  FILLER           PIC X  VALUE " ".
           03  FILLER           PIC X(11) VALUE "LOAN DATE: ".
           03  D-LOANDATE       PIC 99/99/99.
           03  FILLER           PIC X(7)  VALUE "  DUE: ".
           03  D-DUE            PIC 99.

       01  HEAD5.
           03  FILLER           PIC X(43) VALUE
           "    LNAMT TRM   1STPY    MDTE      REGPY   ".
           03  FILLER           PIC X(45) VALUE
           "INS    LIFE      A&H     PROP    OTHER   EFF ".

       01  HEAD6.
           03  FILLER           PIC X(43) VALUE
           "  AMT FIN   DWN-PMT    FIN-CHG    INT-CHG".
           03  FILLER           PIC X(44) VALUE
           "MNT-FEE   SRV-CHG   PROCEEDS   LT-CHG    APR".

       01  HEAD7.
           03  FILLER           PIC X(26) VALUE
           "DEALER   NAME".
           03  FILLER           PIC X(8) VALUE
           "RESERVE ".
           03  HEAD7-RSV-2      PIC X(8) VALUE
           "SPL-RSV ".
           03  HEAD7-RSV-3      PIC X(9) VALUE
           "RSV-PRIN3".
           03  HEAD7-DLR-TYP    PIC X(10) VALUE
           "DISCOUNT ".
           03  FILLER           PIC X(9) VALUE
           "PART-RSV ".
           03  HEAD7-LNCOST     PIC X(7)  VALUE SPACES.
           03  FILLER           PIC X(11) VALUE
           " PRIOR LNNO".


       01  HEAD8.
           03  FILLER           PIC X(51) VALUE
           "EMPLOYER                 POSITION".
           03  FILLER           PIC X(37) VALUE
           "YRS WORK PHONE #   EXT  PAY/MONTH".

       01  HEAD9.
           03  FILLER           PIC X(23) VALUE
           "ENDORSER".

       01  HEAD10.
           03  FILLER           PIC X(54) VALUE
           "AT-ADR  COLLATERAL             COLLATERAL-ID".
           03  FILLER           PIC X(7)  VALUE
           "MESSAGE".

       01  HEAD11A.
           03  FILLER           PIC X(33) VALUE SPACES.
           03  FILLER           PIC X(17) VALUE "- A P P L I E D -".
           03  FILLER           PIC X(16) VALUE SPACES.
           03  FILLER           PIC X(19) VALUE "- B A L A N C E S -".

       01  HEAD11.
           03  FILLER           PIC X(54) VALUE
           "  DATE   CD  REF     TRAMT   OTHER   INT/LC     PRIN".
           03  FILLER           PIC X(34) VALUE
           "PDTH LC   OTHER   INT/LC      PRIN".

       01  HEAD12.
           03  FILLER           PIC X(54) VALUE
           "  DATE   SEQ          MEMO                          ".
           03  FILLER           PIC X(34) VALUE SPACE.

       01  HEAD13               VALUE " ".
           03  D-BWNAME         PIC X(35).
           03  D-SSNOTAG        PIC X(8).
           03  D-X.
               07  D-SSNO       PIC 999/99/9999.
               07  D-SOC-X-OUT REDEFINES D-SSNO.
                   10  D-SOC1    PIC X(3).
                   10  FILLER    PIC X.
                   10  D-SOC2    PIC XX.
                   10  FILLER    PIC X.
                   10  D-SOC3    PIC X(4).
               07  FILLER       PIC X.
           03  D-Z REDEFINES D-X.
               07  D-PHONE      PIC 999/999/9999.
           03  FILLER           PIC X(7).
           03  D-CON-REC        PIC X(9).
           03  D-AGE-PROFILE.
               05  D-AGE-ALP.
                   07  D-AGE    PIC ZZZZ-.
               05  D-PROFILE    PIC X(12).

       01  HEAD14.
           03  FILLER           PIC X(41)     VALUE
           "DESC.      MAKE            MODEL         ".
           03  FILLER           PIC X(43)     VALUE
           "  YEAR STYLE    CLASS    IDENTIFICATION    ".
           03  FILLER           PIC X(48)     VALUE SPACE.

       01  HEAD15.
           03  FILLER           PIC X(30)     VALUE
           "CD DESCRIPTION        AMOUNT  ".
           03  FILLER           PIC X(30)     VALUE
           "CD DESCRIPTION        AMOUNT  ".
           03  FILLER           PIC X(28)     VALUE
           "CD DESCRIPTION        AMOUNT".

       01  DETAIL-LINE          PIC X(88)       VALUE SPACES.

       01  DETAIL1 REDEFINES DETAIL-LINE.
           03  D-LNAMT          PIC Z(5)9.99.
           03  FILLER           PIC X.
           03  D-TERM           PIC ZZ9.
           03  FILLER           PIC X.
           03  D-1STPYDATE      PIC X(8).
           03  FILLER           PIC X.
           03  D-MDTE           PIC 99/99/99     BLANK ZERO.
           03  FILLER           PIC X.
           03  D-REGPYAMT       PIC Z(4)9.99.
           03  D-REGPYAMTS      PIC X.
           03  FILLER           PIC XX.
           03  D-INS            PIC XXX.
           03  FILLER           PIC X.
           03  D-CL             PIC ZZZ9.99      BLANK ZERO.
           03  D-OURS1          PIC X.
           03  FILLER           PIC X.
           03  D-AH             PIC ZZZ9.99      BLANK ZERO.
           03  D-OURS2          PIC X.
           03  FILLER           PIC X.
           03  D-PP             PIC ZZZ9.99      BLANK ZERO.
           03  D-OURS3          PIC X.
           03  FILLER           PIC X.
           03  D-OT             PIC ZZZ9.99      BLANK ZERO.
           03  FILLER           PIC X.
           03  D-EFFRATE        PIC Z9.999       BLANK ZERO.

       01  DETAIL2 REDEFINES DETAIL-LINE.
           03  D-AMTFIN         PIC ZZZZZ9.99    BLANK ZERO.
           03  FILLER           PIC X.
           03  D-DOWNPY-TRADEIN PIC ZZZZZ9.99-   BLANK ZERO.
           03  FILLER           PIC X.
           03  D-FINCHG         PIC ZZZZZ9.99    BLANK ZERO.
           03  FILLER           PIC X(2).
           03  D-INTCHG         PIC ZZZZZ9.99    BLANK ZERO.
           03  FILLER           PIC X(2).
           03  D-MAINTFEE       PIC ZZZ9.99      BLANK ZERO.
           03  FILLER           PIC X(2).
           03  D-SRVCHG         PIC ZZZZ9.99     BLANK ZERO.
           03  FILLER           PIC X(2).
           03  D-PROCEEDS       PIC ZZZZZ9.99    BLANK ZERO.
           03  FILLER           PIC X(2).
           03  D-LATECHG        PIC ZZ9.99       BLANK ZERO.
           03  FILLER           PIC XX.
           03  D-APRATE         PIC ZZ9.999      BLANK ZERO.

       01  DETAIL3 REDEFINES DETAIL-LINE.
           03  D-FEEAMTS        OCCURS 3.
               05  D-FEECODE    PIC XX.
               05  FILLER       PIC X.
               05  D-FEEDESC    PIC X(17).
               05  FILLER       PIC X.
               05  D-FEEAMT     PIC ZZZ9.99      BLANK ZERO.
               05  FILLER       PIC XX.

       01  DETAIL5 REDEFINES DETAIL-LINE.
           03  D-DLNO           PIC 9999.99.
           03  FILLER           PIC X.
           03  D-DLNAME         PIC X(13).
           03  FILLER           PIC X.
           03  D-DLRVPRIN-1     PIC ZZZZZ9.99    BLANK ZERO.
           03  FILLER           PIC X.
           03  D-DLRVPRIN-2     PIC ZZZZZ9.99    BLANK ZERO.
           03  D-DLRVPRIN-3     PIC ZZZZZ9.99    BLANK ZERO.
           03  D-DLDISC         PIC ZZZZZ9.99    BLANK ZERO.
           03  D-DLPARVHELD     PIC ZZZZZ9.99    BLANK ZERO.
           03  D-LOANCOST       PIC ZZZZZ9.99.
           03  FILLER           PIC X.
           03  D-PRLNNO         PIC 9(6)         BLANK ZERO.
           03  D-PRLNNO-SEQ     PIC 9(2)         BLANK ZERO.

       01  DETAIL6 REDEFINES DETAIL-LINE.
           03  D-EMPLOYER       PIC X(24).
           03  FILLER           PIC X.
           03  D-POSITION       PIC X(25).
           03  FILLER           PIC XX.
           03  D-EMPYRS         PIC ZZ.
           03  FILLER           PIC X.
           03  D-EMPPHONE       PIC X(14).
           03  FILLER           PIC X.
           03  D-EXT            PIC ZZZ.
           03  FILLER           PIC X.
           03  D-MONTHLY        PIC ZZZZZ9.99    BLANK ZERO.
           03  FILLER           PIC X(4).

       01  DETAIL7 REDEFINES DETAIL-LINE.
           03  D-YRS            PIC X(4).
           03  FILLER           PIC X(3).
           03  D-COLLCD         PIC XX.
           03  FILLER           PIC X.
           03  D-COLLATERAL     PIC X(20).
           03  FILLER           PIC X(2).
           03  D-COLLID         PIC X(20).
           03  FILLER           PIC X(2).
           03  D-MSG            PIC X(30).

       01  DETAIL8 REDEFINES DETAIL-LINE.
           03  D-DATE           PIC 99/99/99     BLANK ZERO.
           03  D-BACKDATE       PIC X(10).
           03  FILLER REDEFINES D-BACKDATE.
               05  D-MISC           PIC X.
               05  D-TRCD           PIC XX.
               05  FILLER           PIC X.
               05  D-REFCD          PIC X(5).
               05  FILLER           PIC X.
           03  D-TRAMT          PIC ZZZZ9.99-    BLANK ZERO.
           03  D-TRAMT-RATE     REDEFINES D-TRAMT
                                PIC ZZZZ.999-.
           03  D-APOTH          PIC ZZZ9.99-     BLANK ZERO.
           03  D-NEW-PMT-AREA.
               05  D-PMESS      PIC X(18).
               05  D-NEWPMT     PIC ZZZZ9.99.
           03  FILLER REDEFINES D-NEW-PMT-AREA.
               05  D-APINT          PIC ZZZZ9.99-    BLANK ZERO.
               05  D-APCUR          PIC ZZZZ9.99-    BLANK ZERO.
               05  FILLER           PIC X.
               05  D-PDTHRU-MO      PIC 99.
               05  D-PDTHRU-DAYR    PIC 99.
               05  FILLER           PIC X.
               05  D-LCPDTH         PIC 99           BLANK ZERO.
           03  D-OTHBAL         PIC ZZZZ9.99-    BLANK ZERO.
           03  D-INTBAL         PIC ZZZZ9.99-    BLANK ZERO.
           03  D-CURBAL         PIC Z(5)9.99     BLANK ZERO.

       01  D-MEMO              REDEFINES DETAIL-LINE.
           03  D-MEMO-DATE     PIC 99/99/99.
           03  FILLER          PIC XX.
           03  D-MEMO-SEQ      PIC 99.
           03  FILLER          PIC X(5).
           03  D-MEMO-MEMO     PIC X(60).

       01  DETAIL9             REDEFINES DETAIL-LINE.
           03  D-DESCRIPTION   PIC X(10).
           03  FILLER          PIC X.
           03  D-MAKE          PIC X(15).
           03  FILLER          PIC X.
           03  D-MODEL         PIC X(15).
           03  FILLER          PIC X.
           03  D-YEAR          PIC X(4).
           03  FILLER          PIC X.
           03  D-STYLE         PIC X(8).
           03  FILLER          PIC X.
           03  D-CLASS         PIC X(8).
           03  FILLER          PIC X.
           03  D-ID            PIC X(20).

       01  DUE-LINE            REDEFINES DETAIL-LINE.
           03  FILLER          PIC X(50).
           03  DUE-PROMPT      PIC X(11).
           03  DUE-DATE-DUE    PIC 99/99/99  BLANK ZERO.
           03  DUE-LCAMT       PIC Z(6).ZZ.
           03  FILLER          PIC X.
           03  DUE-AMOUNT      PIC Z(6).ZZ.

       01  DEF-LINE REDEFINES DETAIL-LINE.
           03  FILLER          PIC X(50).
           03  DEF-PROMPT      PIC X(24).
           03  DEF-DATE        PIC 9(4).

      ***********************************************************
      * PRINT-REV = "C"      CUSTOMER VERSION
      *                      NO REVERSALS, NO BACKDATES, 132 COLUMNS
      *                      LC & INT BROKE OUT
      ***********************************************************

       01  CUST-DETAIL8.
           03  C-DATE           PIC 99/99/99     BLANK ZERO.
           03  FILLER           PIC XX.
           03  C-TRCD           PIC XX.
           03  FILLER           PIC XX.
           03  C-REFCD          PIC X(5).
           03  FILLER           PIC X.
           03  C-TRAMT          PIC ZZZZZZ9.99-    BLANK ZERO.
           03  C-TRAMT-RATE     REDEFINES C-TRAMT
                                PIC ZZZZZZ.999-.
           03  FILLER           PIC X.
           03  C-APOTH          PIC ZZZZZ9.99-     BLANK ZERO.
           03  FILLER           PIC X.
           03  C-APINT         PIC ZZZZZ9.99-    BLANK ZERO.
           03  FILLER          PIC X.
           03  C-APLCHG        PIC ZZZZZ9.99-    BLANK ZERO.
           03  FILLER          PIC X.
           03  C-APCUR         PIC ZZZZZZ9.99-   BLANK ZERO.
           03  FILLER          PIC X.
           03  C-PDTHRU-MO      PIC 99.
           03  C-PDTHRU-DAYR    PIC 99.
           03  FILLER           PIC X.
           03  C-LCPDTH         PIC 99           BLANK ZERO.
           03  FILLER           PIC X.
           03  C-OTHBAL         PIC ZZZZZZ9.99-    BLANK ZERO.
           03  C-INTBAL         PIC ZZZZZZ9.99-    BLANK ZERO.
           03  C-LCBAL          PIC ZZZZZZ9.99-    BLANK ZERO.
           03  C-CURBAL         PIC Z(6)9.99     BLANK ZERO.

       01  CUST-HEAD11.
           03  FILLER           PIC X(57) VALUE
           "  DATE    CD    REF      TRAMT      APOTH      APINT".
           03  FILLER           PIC X(54) VALUE
           "APLCHG      APPRIN  PDTH LC    OTH BAL    INT BAL     ".
           03  FILLER           PIC X(17) VALUE
           "LC BAL    BALANCE".


       COPY "LIBCL/CLMENUW_EXT.CPY".
       COPY "LIBWI/PFSWINDOWSW.CPY".
       COPY "LIBLP/LPAMTSW.CPY".
       COPY "LIBGB/GETPRW.CPY".
      *COPY "LIBGB/MKTEMPW.CPY".
       COPY "LIBGB/PHONE.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/GPENVW.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBLP/LPSTATW.CPY".
       COPY "LIBLP/LPMDTEW.CPY".
       COPY "LIBLP/LPPOFFW.CPY".
       COPY "LIBCL/CRNOR2W.CPY".
       COPY "LIBLP/LP01NO.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".
       01  LONPP-WINDOW-HANDLE PIC X(10).
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBLP/LONPF_DEF.CPY".
       COPY "LIBWI/LONPP_DEF.CPY".
       COPY "LIBSP/CPINQ_DEF.CPY".
       COPY "LIBLP/AUDIQ_DEF.CPY".
       COPY "LIBWI/CLMENU_DEF.CPY".
       COPY "LIBLP/LONPF_WKS.CPY".
       COPY "LIBWI/LONPP_WKS.CPY".
       COPY "LIBSP/CPINQ_WKS.CPY".
       COPY "LIBLP/AUDIQ_WKS.CPY".
       COPY "LIBWI/CLMENU_WKS.CPY".

       COPY "LIBGB/GETFMW.CPY".
       COPY "LIBLP/LPLOANW.CPY".
       COPY "LIBLP/LPIPBUF.CPY".

       SCREEN SECTION.
       COPY "LIBLP/LONPF_SCN.CPY".
       COPY "LIBWI/LONPP_SCN.CPY".
       COPY "LIBSP/CPINQ_SCN.CPY".
       COPY "LIBLP/AUDIQ_SCN.CPY".
       COPY "LIBWI/CLMENU_SCN.CPY".

       PROCEDURE DIVISION
             USING FORM-PATHNAME IP-BUF PROG-BUF EXIT-PATHNAME.
       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
               PR-FILE.
       COPY "LIBGB/DECLARE.CPY".
           PERFORM CLOSE-SP1-FILE.
           PERFORM CLOSE-LTP1-FILE.
           PERFORM CLOSE-LTC1-FILE.
           PERFORM CLOSE-LP1-FILE.
           PERFORM CLOSE-LN1-FILE.
           PERFORM CLOSE-LM1-FILE.
           PERFORM CLOSE-DL1-FILE.
           PERFORM CLOSE-CDB1-FILE.
           PERFORM CLOSE-CD1-FILE.
           PERFORM CLOSE-BW1-FILE.
           PERFORM CLOSE-GB-FILE.
           PERFORM CLOSE-BR-FILE.
           PERFORM CLOSE-PR-FILE.
       COPY "LIBGB/DECLAR2.CPY".

      *************************************************
       MAIN-MODULE SECTION.
      *************************************************

           PERFORM SQL-CONNECT.

      D    STOP MESS.
           MOVE FORM-PATHNAME TO FORM-PATH.

      * PAYMENT POSTING
           IF IP-PROG = "F1"
              MOVE "LONPF" TO VDU-FORM-NAME
           ELSE
      * F10 CPINQ
           IF IP-PROG = "Q1"
              MOVE "CPINQ" TO VDU-FORM-NAME
           ELSE
      * AUDITOR INQ
           IF IP-PROG = "A1"
              MOVE "AUDIQ" TO VDU-FORM-NAME
           ELSE
              MOVE "CLMENU" TO VDU-FORM-NAME.

           PERFORM INITIALIZATION.
           IF ERRCD NOT = " "
              GO TO END-ROUTINE.

      * TEST ENVIRONMENT VARIABLE "LONPP0_PAYDATE_ONLY" TO DETERMINE
      * OR NOT ONLY PAYDATES SHOULD PRINT INSTEAD OF TRDATE
           MOVE "LONPP0_PAYDATE_ONLY" TO GETENV-BUF.
           PERFORM GETENV-CALL.
           IF STAT = "00"
              MOVE "Y" TO PAYDATE-ONLY-FG
           ELSE
              MOVE "N" TO PAYDATE-ONLY-FG.

           MOVE LN-ACCTNO          TO CRNOR2-ACCTNO.
           MOVE TRANS-DATE         TO CRNOR2-SYSDATE.
           MOVE "CL/CRNOR2"        TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH CRNOR2-LINK NO-REC
                                 POFF-WORKERS LN-REC.
           CANCEL FORM-PROGX.

           PERFORM HEAD-PAGE.
      * "C" = CUSTOMER VERSION, REMOVED ALOT OF INFO
           IF PRINT-REVS = "C"
              PERFORM DETAIL-CUST-HEADER
           ELSE
              PERFORM LOAN-LINES
              PERFORM LT-COLLATERAL
              PERFORM SEND-DUE-DAYS
              IF PRINT-REVS = "I"
                 PERFORM DETAIL-CUST-HEADER
              ELSE
                 PERFORM DETAIL-HEADER.

           PERFORM TRANSACTION-LINES.
           IF PRINT-MEMOS = "Y"
              PERFORM MEMO-LINES.

       END-ROUTINE.
      * ADDED THE BLANK LINE SO DIRECT PRINT TO BIG PARALLEL PRUS WILL PRINT
      * LAST DETAIL LINE
           IF ERRCD = " "
              MOVE SPACES TO DETAIL-LINE
              PERFORM WRITE-DETAIL-LINE.

           MOVE "1" TO ERRCD.
           IF IP-BUF = "F1"
              MOVE "F1F" TO NEW-PROG-FORM
              MOVE "P" TO ERRCD.
           IF IP-BUF = "S1"
              MOVE "S1S" TO NEW-PROG-FORM
              MOVE "P" TO ERRCD.
           GO TO END-PROGRAM.

      ***********************************
       INITIALIZATION SECTION.
      ***********************************

      * NEEDED FOR F9
           IF IP-PROG NOT = "F1" AND NOT = "Q1" AND NOT = "A1"
              MOVE EXT-WI-CL-ACCTNO     TO LOAN-KEY
              MOVE EXT-WI-CL-TRANS-DATE TO TRANS-DATE
              MOVE EXT-WI-CL-SYSDATE    TO LPLOAN-SYSDATE.

           IF IP-PROG = "F1"
              MOVE LONPF-PRU-POS TO P-POS
           ELSE
           IF IP-PROG = "Q1"
              MOVE CPINQ-PRU-POS TO P-POS
           ELSE
           IF IP-PROG = "A1"
              MOVE AUDIQ-PRU-POS TO P-POS
           ELSE
              MOVE CLMENU-PRU-POS TO P-POS.

           MOVE "000000" TO Q-POS C-POS.
           MOVE "N" TO Q-BUF.
           MOVE "01" TO C-BUF.

           MOVE "S  A020PRU." TO VDU.
           MOVE "00" TO VDUBUF P-BUF IP-PRU.
           PERFORM SCREENIO.

           MOVE "F7-EXIT, ENTER PRINTER NO." TO MESS.
           PERFORM PROGRESS-MSG.
           MOVE "N" TO STAT.
           PERFORM PRU-IN.
           IF STAT NOT = "00"
              MOVE "X" TO ERRCD
              GO TO END-ROUTINE.

      * GET GLOBAL PARAMETER RECORD (FROM GPENV):
           PERFORM GET-GPENV.

           IF GP-ASK-HARD-COPY-MEMOS = "Y"
              PERFORM DO-ASK-MEMOS-WINDOW
           ELSE
              MOVE "Y" TO PRINT-MEMOS
              MOVE "Y" TO PRINT-REVS.

      *    IF EXT-WI-CL-ARCHIVE-FG = "Y"
      * WI-CL FIELDS ARE EXTERNAL NOW
      *    IF IP-BUF = "AR"
      *       PERFORM SETUP-ARCHIVE-PATHS
      *       IF IO-BAD
      *          MOVE "X" TO ERRCD
      *          GO TO EXIT-INITIALIZE.

           PERFORM OPEN-LN1-FILE.
           MOVE LOAN-KEY TO LN-ACCTNO.
           PERFORM READ-LN1-FILE.
           IF IO-FG = 9
              MOVE "LOAN HAS BEEN DELETED !" TO MESS
              PERFORM SEND-MESS
              MOVE "0" TO ERRCD
              GO TO EXIT-INITIALIZE.
           PERFORM CLOSE-LN1-FILE.

           MOVE LN-LOANDATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO D-LOANDATE.

           PERFORM OPEN-SP1-FILE.
           MOVE LN-ORGST TO SP-ORGST.
           MOVE LN-SPRCLASS TO SP-SPRCLASS.
           MOVE LN-SUBCLASS TO SP-SUBCLASS.
           MOVE LN-LAWCODE TO SP-LAWCODE.
           PERFORM READ-SP1-FILE.
           IF IO-FG = 9
              MOVE "STATE PARAMETER RECORD HAS BEEN DELETED !" TO MESS
              PERFORM SEND-MESS
              MOVE "0" TO ERRCD
              GO TO EXIT-INITIALIZE.
           PERFORM CLOSE-SP1-FILE.

           PERFORM OPEN-BW1-FILE.

           PERFORM LOCATE-JOINT-MAKER.
           PERFORM GET-ENDORSER.

           MOVE LN-SSNO(1) TO BW-SSNO.
           PERFORM READ-BW1-FILE.
           IF IO-FG = 9
              MOVE "BORROWER HAS BEEN DELETED" TO MESS
              PERFORM SEND-MESS
              MOVE "0" TO ERRCD
              GO TO EXIT-INITIALIZE.

           PERFORM OPEN-GB-FILE.
           MOVE GB-PATH-OWNBR TO GB-BRNO.
           MOVE 1             TO GB-NO.
           PERFORM READ-GB-FILE.
           IF IO-FG = 9
              MOVE "* INVALID *" TO GB-COMPANY-NAME.
           PERFORM CLOSE-GB-FILE.

           PERFORM OPEN-BR-FILE.
           MOVE LN-OWNBR TO BR-NO H-BRNO.
           PERFORM READ-BR-FILE.
           IF IO-FG = 9
              MOVE "* INVALID *" TO BR-NAME.
           PERFORM CLOSE-BR-FILE.

           PERFORM OPEN-DL1-FILE.
           MOVE LN-DLNO TO DL-DLNO.
           PERFORM READ-DL1-FILE.
           IF IO-FG = 9
              MOVE "* INVALID *" TO D-DLNAME.
           PERFORM CLOSE-DL1-FILE.

           MOVE BR-NAME TO H-BRNAME.
           MOVE " " TO ERRCD.
           MOVE GB-COMPANY-NAME TO H-NAME.
           MOVE EXT-CONAME-SYSDATE TO H-DATE.
           MOVE EXT-CONAME-USER-ID TO H-USERID.
           IF IP-PROG = "FS"
             MOVE "- -  ALTERNATE FSB PAYMENT INQUIRY  - - " TO H-TITLE.

           MOVE "INQUIRY PRINT IN PROGRESS..........." TO MESS.
           PERFORM PROGRESS-MSG.

           PERFORM LOAN-CALCULATIONS.

           PERFORM OPEN-PR-FILE.

       EXIT-INITIALIZE.
           EXIT.

      *******************************
       DO-ASK-MEMOS-WINDOW SECTION.
      *******************************

      *    MOVE 10 TO WINDOW-LINES
      *    MOVE 37 TO WINDOW-COLUMNS
      *    MOVE 10 TO WINDOW-BEGIN-Y
      *    MOVE 5 TO WINDOW-BEGIN-X
      *    MOVE "LONPP" TO VDU-FORM-NAME.
      *    MOVE "WI/LONPP" TO WINDOW-FORM-NAM.

      *    PERFORM OPEN-WINDOW.
      *    DISPLAY LONPP-SCREEN.

           MOVE "LONPP" TO VDU-FORM-NAME.

           DISPLAY FLOATING GRAPHICAL WINDOW
              AT  SCREEN LINE 10
              AT  SCREEN COL  5
              CELL SIZE IS LABEL FONT
              SIZE    37
              LINES   10
              BLANK SCREEN
              HANDLE IS LONPP-WINDOW-HANDLE.

           PERFORM FORM-RESET.


       DO-ASK-MEMO-ENTER.
           MOVE "S  A010FILE." TO VDU.
           MOVE SPACES TO VDUBUF.
           PERFORM SCREENIO.
           MOVE "F7-EXIT, ENTER 'Y' OR 'N'" TO MESS.
           PERFORM SEND-WSEL-LEGEND.

           MOVE "E  A010MEMOS." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO EXIT-ASK-MEMOS-WINDOW.
           IF VDUSTATUS NOT = "00" GO TO DO-ASK-MEMO-ENTER.
           IF VDUBUF NOT = "Y" AND NOT = "N"
              GO TO DO-ASK-MEMO-ENTER.
           MOVE VDUBUF TO PRINT-MEMOS.

       DO-ASK-REV-ENTER.
           MOVE "E  A010REV." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO EXIT-ASK-MEMOS-WINDOW.
           IF VDUSTATUS = "1U" GO TO DO-ASK-MEMO-ENTER.
           IF VDUSTATUS NOT = "00" GO TO DO-ASK-REV-ENTER.
      * O = ONLY NSF
      * C = CUSTOMER VERSION (NO REVERSALS OR BACKDATES, INT & LC BREAKOUT
      *      AND 132 DETAIL LINE
      * I = INSURANCE VERSION , STANDARD TOP BUT CUSTOMER VERSION PAYMENTS
           IF VDUBUF NOT = "Y" AND NOT = "N" AND NOT = "O"
                  AND NOT = "C" AND NOT = "I"
              GO TO DO-ASK-REV-ENTER.

           MOVE VDUBUF TO PRINT-REVS.

       EXIT-ASK-MEMOS-WINDOW.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY LONPP-SCREEN.
           PERFORM CLOSE-WINDOW.
           IF IP-PROG = "F1"
              MOVE "LONPF" TO VDU-FORM-NAME
           ELSE
           IF IP-PROG = "Q1"
              MOVE "CPINQ" TO VDU-FORM-NAME
           ELSE
           IF IP-PROG = "A1"
              MOVE "AUDIQ" TO VDU-FORM-NAME
           ELSE
              MOVE "CLMENU" TO VDU-FORM-NAME.


      ***********************
       LOAN-LINES SECTION.
      ***********************

           MOVE DASH-LINE TO DETAIL-LINE.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE HEAD5 TO DETAIL-LINE.
           PERFORM WRITE-DETAIL-LINE.

           MOVE LN-SUM-LNAMT TO D-LNAMT.
           MOVE LN-SUM-ORGTERM TO D-TERM.
           MOVE LN-1STPYDATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO ALP-DATE-EDIT.
           MOVE LN-UNITPER-CD TO DATER-UNITPER-CD.
           MOVE LN-UNITPER-FREQ TO DATER-UNITPER-FREQ.
           PERFORM UNITPER-MASK.
           MOVE ALP-DATE-EDIT TO D-1STPYDATE.

           PERFORM MATURITY-DATE-CALCULATION.
           MOVE MDTE-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO D-MDTE.

           MOVE NO-SCHED-PAYMENT TO D-REGPYAMT.
           IF LN-PAY-SCHLD-FG = "Y"
              MOVE "S" TO D-REGPYAMTS.

           MOVE LN-INSCODES TO D-INS.
           IF LN-INSPREM(1) NOT = 0
              MOVE LN-INSPREM(1) TO D-CL
              MOVE LN-INSOURS(1) TO D-OURS1.
           IF LN-INSPREM(2) NOT = 0
              MOVE LN-INSPREM(2) TO D-AH
              MOVE LN-INSOURS(2) TO D-OURS2.
           IF LN-INSPREM(3) NOT = 0
              MOVE LN-INSPREM(3) TO D-PP
              MOVE LN-INSOURS(3) TO D-OURS3.
           COMPUTE D-OT = LN-INSPREM(4) + LN-INSPREM(5) + LN-INSPREM(6).
           MOVE LN-EFFRATE TO D-EFFRATE.
           PERFORM WRITE-DETAIL-LINE.

           MOVE DASH-LINE TO DETAIL-LINE
           PERFORM WRITE-DETAIL-LINE.

           MOVE HEAD6 TO DETAIL-LINE.
           PERFORM WRITE-DETAIL-LINE.

           MOVE DISCLOSED-FINAMT TO D-AMTFIN.
           ADD LN-DOWNPY LN-TRADEIN GIVING D-DOWNPY-TRADEIN.
           MOVE LN-SUM-FINCHG   TO D-FINCHG.
           MOVE LN-SUM-INTCHG   TO D-INTCHG.
           MOVE LN-SUM-MAINTFEE TO D-MAINTFEE.
           MOVE LN-SUM-SERCHG   TO D-SRVCHG.
           MOVE LN-PROCEEDS     TO D-PROCEEDS.
           MOVE NO-LATE-CHG     TO D-LATECHG.
           MOVE LN-APRATE       TO D-APRATE.
           PERFORM WRITE-DETAIL-LINE.

           MOVE DASH-LINE TO DETAIL-LINE
           PERFORM WRITE-DETAIL-LINE.

           MOVE HEAD15 TO DETAIL-LINE.
           PERFORM WRITE-DETAIL-LINE.

           MOVE 1 TO SUB SUB2.
       NEXT-FEE-AMT.
           IF SUB > 10
              IF SUB2 > 1
                 PERFORM WRITE-DETAIL-LINE
                 GO TO EMP-INFO
              ELSE
                 GO TO EMP-INFO.

           IF LN-FEEAMT(SUB) = 0
              ADD 1 TO SUB
              GO TO NEXT-FEE-AMT.

           MOVE LN-FEEAMT(SUB) TO D-FEEAMT(SUB2).
           PERFORM GET-FEE-DESC.
           IF SUB2 = 3
              MOVE 1 TO SUB2
              ADD 1 TO SUB
              PERFORM WRITE-DETAIL-LINE
              GO TO NEXT-FEE-AMT
           ELSE
              ADD 1 TO SUB SUB2
              GO TO NEXT-FEE-AMT.

       EMP-INFO.
           MOVE DASH-LINE TO DETAIL-LINE
           PERFORM WRITE-DETAIL-LINE.

           IF GP-DLR-DISP-TYPE = "C"
              MOVE "COMMISSION" TO HEAD7-DLR-TYP
           ELSE
              MOVE " DISCOUNT " TO HEAD7-DLR-TYP.

           MOVE "SPL-RSV "   TO HEAD7-RSV-2.
           MOVE "RSV-PRIN3"  TO HEAD7-RSV-3.
           MOVE "OTH-INC"    TO HEAD7-LNCOST.
           MOVE HEAD7 TO DETAIL-LINE.
           PERFORM WRITE-DETAIL-LINE.

           MOVE LN-DLNO TO D-DLNO.
           MOVE DL-SHORT TO D-DLNAME.
           MOVE LN-DLRVPRIN(1) TO D-DLRVPRIN-1.
           MOVE LN-DLRVPRIN(2) TO D-DLRVPRIN-2.
           MOVE LN-DLRVPRIN(3) TO D-DLRVPRIN-3.
           MOVE LN-ANTICERN(6) TO D-LOANCOST.
           MOVE LN-DLDISC TO D-DLDISC.
           MOVE LN-DLPARVHELD TO D-DLPARVHELD.
           MOVE LN-PRLNNO TO D-PRLNNO.
      * TO MATCH A15
           MOVE 01        TO D-PRLNNO-SEQ.
           PERFORM WRITE-DETAIL-LINE.

           MOVE DASH-LINE TO DETAIL-LINE
           PERFORM WRITE-DETAIL-LINE.

           MOVE HEAD8 TO DETAIL-LINE.
           PERFORM WRITE-DETAIL-LINE.

           MOVE BW-EMPLOYER TO D-EMPLOYER.
           MOVE BW-EMPLPOS TO D-POSITION.
           MOVE BW-EMPYEARS TO D-EMPYRS.
           IF BW-EMPPHONE NOT = 0
              MOVE BW-EMPPHONE TO TEL-BUF
              PERFORM SEND-PHONE
              MOVE TEL-EDIT TO D-EMPPHONE.
           MOVE BW-EMPEXT TO D-EXT.
           MOVE BW-EMPPAY TO D-MONTHLY.
           PERFORM WRITE-DETAIL-LINE.

           MOVE DASH-LINE TO DETAIL-LINE
           PERFORM WRITE-DETAIL-LINE.

           MOVE HEAD9 TO DETAIL-LINE.
           PERFORM WRITE-DETAIL-LINE.

           MOVE ENDORSER-INFO TO DETAIL-LINE.
           PERFORM WRITE-DETAIL-LINE.

           MOVE DASH-LINE TO DETAIL-LINE
           PERFORM WRITE-DETAIL-LINE.

           MOVE HEAD10 TO DETAIL-LINE.
           PERFORM WRITE-DETAIL-LINE.

           PERFORM OPEN-CD1-FILE.
           MOVE LN-COLLCD TO CD-CODE.
           MOVE "CO"      TO CD-TYPE.
           PERFORM READ-CD1-FILE.
           IF IO-FG = 9
              MOVE "* INVALID *" TO D-COLLATERAL
           ELSE
              MOVE CD-DESC TO D-COLLATERAL.
           PERFORM CLOSE-CD1-FILE.

           MOVE BW-TIMEADDR TO D-YRS.
           MOVE LN-COLLCD TO D-COLLCD.
           MOVE LN-COLLID TO D-COLLID.
           MOVE LN-MESSAGE TO D-MSG.
           PERFORM WRITE-DETAIL-LINE.


      *****************************
       TRANSACTION-LINES SECTION.
      *****************************
           PERFORM OPEN-LP1-FILE.

           MOVE LP-PATH-OWNBR TO LP-BRNO
                                 QLP1-WBEG-BRNO
                                 QLP1-WEND-BRNO.
           MOVE LN-ACCTNO     TO LP-ACCTNO
                                 QLP1-WBEG-ACCTNO
                                 QLP1-WEND-ACCTNO.
           MOVE 0             TO LP-SEQNO
                                 QLP1-WBEG-SEQNO.
           MOVE ALL "9"       TO QLP1-WEND-SEQNO.

           PERFORM START-LP1-FILE.
       NEXT-LP-LINE.
           PERFORM READ-LP1-FILE-NEXT.
           IF IO-FG = 9
              GO TO END-TRANSACTION-LINES.

           IF (LP-BRNO NOT = LP-PATH-OWNBR) OR
                (LP-ACCTNO NOT = LN-ACCTNO)
              GO TO END-TRANSACTION-LINES.

      * PRINT "O"NLY NSF REVERSALS
           IF PRINT-REVS = "O"
              IF LP-REV = "Y"
                 MOVE LP-REC TO SAVE-LP-REC
                 GO TO NEXT-LP-LINE
              ELSE
      * IF PRINT-REVS IS NSF ONLY, YOU WILL HIT THE REVERSED PAYMENT FIRST,
      * SO YOU HAVE TO STORE IT. WHEN YOU THEN HIT THE "RV", YOU HAVE TO
      * MOVE IN THE SAVED PAYMENT AND PRINT THAT FIRST, THEN PRINT THE RV
                 IF LP-TRCD = "RV"
                    IF LP-REFNO = "NSF  "
                       MOVE LP-REC TO SAVE-RV-REC
                       MOVE SAVE-LP-REC TO LP-REC
                       PERFORM PRINT-LINE
                       MOVE SAVE-RV-REC TO LP-REC
                    ELSE
                       MOVE SPACES TO SAVE-LP-REC
                       GO TO NEXT-LP-LINE.


      * "C" = CUSTOMER VERSION, NO REVERSALS
           IF PRINT-REVS = "N" OR "C" OR "I"
              IF LP-REV = "Y" OR LP-TRCD = "RV"
                 GO TO NEXT-LP-LINE.

           IF PRINT-REVS = "C" OR "I"
              PERFORM PRINT-CUST-LINE
           ELSE
              PERFORM PRINT-LINE.

           GO TO NEXT-LP-LINE.
       END-TRANSACTION-LINES.
           PERFORM CLOSE-LP1-FILE.


      ************************
       MEMO-LINES SECTION.
      ************************

           PERFORM OPEN-LM1-FILE.

           MOVE LM-PATH-OWNBR TO LM-BRNO
                                 QLM1-WBEG-BRNO
                                 QLM1-WEND-BRNO.
           MOVE LN-ACCTNO     TO LM-ACCTNO
                                 QLM1-WBEG-ACCTNO
                                 QLM1-WEND-ACCTNO.
           MOVE 1             TO LM-DATE
                                 QLM1-WBEG-DATE-REV.
           MOVE 0             TO LM-SEQ 
                                 QLM1-WBEG-SEQ.
           MOVE ALL "9"       TO QLM1-WEND-DATE-REV
                                 QLM1-WEND-SEQ.

           PERFORM START-LM1-FILE.

           PERFORM READ-LM1-FILE-NEXT.
           IF (IO-FG NOT = 0) OR (LM-BRNO NOT = LM-PATH-OWNBR)
                               OR (LM-ACCTNO NOT = LN-ACCTNO)
                                OR (LM-DATE = 0)
                GO TO END-MEMO-LINES.

           MOVE DASH-LINE TO DETAIL-LINE.
           PERFORM WRITE-DETAIL-LINE.
           MOVE HEAD12 TO DETAIL-LINE.
           PERFORM WRITE-DETAIL-LINE.
           MOVE DASH-LINE TO DETAIL-LINE.
           PERFORM WRITE-DETAIL-LINE.

           GO TO MEMO-FOUND.

       SEND-MEMO-READ-NEXT.
           PERFORM READ-LM1-FILE-NEXT.
           IF (IO-FG NOT = 0) OR (LM-BRNO NOT = LM-PATH-OWNBR)
                               OR (LM-ACCTNO NOT = LN-ACCTNO)
                                OR (LM-DATE = 0)
               GO TO END-MEMO-LINES.

       MEMO-FOUND.
           COMPUTE DATE-YYYYMMDD = 99999999 - LM-DATE.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO D-MEMO-DATE.
           MOVE LM-SEQ      TO D-MEMO-SEQ.
           MOVE LM-MEMO     TO D-MEMO-MEMO.
           PERFORM WRITE-DETAIL-LINE.

           GO TO SEND-MEMO-READ-NEXT.

       END-MEMO-LINES.
           PERFORM CLOSE-LM1-FILE.

      ***********************
       SEND-DUE-DAYS SECTION.
      ***********************
           MOVE DASH-LINE TO PR-REC.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-PR-REC.

           MOVE "LAST  DUE:" TO DUE-PROMPT.
           MOVE NO-LDUE-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO DUE-DATE-DUE.
           MOVE NO-LDUELC TO DUE-LCAMT.
           IF GP-NO-LC-IN-ARREARAGE
              MOVE NO-LDUECONTR TO DUE-AMOUNT
           ELSE
              MOVE NO-LDUEAMT TO DUE-AMOUNT.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "TODAY DUE:" TO DUE-PROMPT.
           MOVE NO-TDUE-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO DUE-DATE-DUE.
           MOVE NO-TDUELC TO DUE-LCAMT.
           IF GP-NO-LC-IN-ARREARAGE
              MOVE NO-TDUECONTR TO DUE-AMOUNT
           ELSE
              MOVE NO-TDUEAMT TO DUE-AMOUNT.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "NEXT  DUE:" TO DUE-PROMPT
           MOVE NO-NXDUE-DATE TO DATE-YYYYMMDD
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
           MOVE DATE-MMDDYY TO DUE-DATE-DUE.
           MOVE NO-NXDUELC TO DUE-LCAMT.
           IF GP-NO-LC-IN-ARREARAGE
              MOVE NO-NXDUECONTR TO DUE-AMOUNT
           ELSE
              MOVE NO-NXDUEAMT TO DUE-AMOUNT.
           PERFORM WRITE-DETAIL-LINE.

           IF LN-CURBAL NOT = 0
              MOVE LN-UNITPER-CD TO DATER-UNITPER-CD
              MOVE LN-UNITPER-FREQ TO LN-UNITPER-FREQ
              MOVE NO-PDTH-DATE TO NDTE-DATE
              MOVE 1 TO NDTE-HOLD
              PERFORM INCREMENT-UNITPER
              MOVE 0 TO DATE-MMDDYY-MM
      * MMYY
              MOVE NDTE-MM TO DATE-MMDDYY-DD
              MOVE NDTE-YY TO DATE-MMDDYY-YY
              IF LN-UNITPER-CD-DAYS
      * MMDD
                 MOVE NDTE-MM TO DATE-MMDDYY-DD
                 MOVE NDTE-DD TO DATE-MMDDYY-YY
              END-IF
           ELSE
              MOVE 0 TO NO-DEFERMENT NDTE-DATE DATE-MMDDYY.

           MOVE "DEFERMENT CHARGE FOR " TO DEF-PROMPT.
           MOVE DATE-MMDDYY             TO DEF-DATE.
           MOVE NO-DEFERMENT            TO DUE-AMOUNT.
           PERFORM WRITE-DETAIL-LINE.

      ****************************
       WRITE-DETAIL-LINE SECTION.
      ****************************
           ADD LN-FEED LN-COUNT GIVING LN-WK.
           IF LN-WK GREATER THAN EXT-CONAME-RPT-LINES
              PERFORM HEAD-PAGE.
           MOVE DETAIL-LINE TO PR-REC.
           PERFORM WRITE-PR-REC.
           ADD LN-FEED TO LN-COUNT.
           MOVE 1 TO LN-FEED.
           MOVE SPACES TO DETAIL-LINE.

      ****************************
       LT-COLLATERAL SECTION.
      ****************************
           PERFORM OPEN-LTC1-FILE.
           MOVE LN-OWNBR  TO LTC-BRNO.
           MOVE LN-ACCTNO TO LTC-ACCTNO.
           MOVE 1         TO LTC-SEQNO.
           PERFORM READ-LTC1-FILE.
           IF IO-FG NOT = 0 
              GO TO EXIT-LT-COLLATERAL.

           MOVE DASH-LINE TO DETAIL-LINE.
           PERFORM WRITE-DETAIL-LINE.
           MOVE HEAD14 TO DETAIL-LINE.
           PERFORM WRITE-DETAIL-LINE.
           MOVE DASH-LINE TO DETAIL-LINE.
           PERFORM WRITE-DETAIL-LINE.


           IF LTC-MAKE(1) NOT = SPACES OR LTC-MODEL(1) NOT = SPACES
                OR LTC-VIN(1) NOT = SPACES
              MOVE LTC-DESC(1) TO D-DESCRIPTION
              MOVE LTC-MAKE(1) TO D-MAKE
              MOVE LTC-MODEL(1) TO D-MODEL
              MOVE LTC-YEAR(1) TO D-YEAR
              MOVE LTC-STYLE(1) TO D-STYLE
              MOVE LTC-CLASS(1) TO D-CLASS
              MOVE LTC-VIN(1) TO D-ID
              PERFORM WRITE-DETAIL-LINE.
           IF LTC-MAKE(2) NOT = SPACES OR LTC-MODEL(2) NOT = SPACES
                OR LTC-VIN(2) NOT = SPACES
              MOVE LTC-DESC(2) TO D-DESCRIPTION
              MOVE LTC-MAKE(2) TO D-MAKE
              MOVE LTC-MODEL(2) TO D-MODEL
              MOVE LTC-YEAR(2) TO D-YEAR
              MOVE LTC-STYLE(2) TO D-STYLE
              MOVE LTC-CLASS(2) TO D-CLASS
              MOVE LTC-VIN(2) TO D-ID
              PERFORM WRITE-DETAIL-LINE.

           IF LTC-HHGS(1) NOT = SPACES
              MOVE LTC-HHGS(1) TO DETAIL-LINE
              PERFORM WRITE-DETAIL-LINE.
           IF LTC-HHGS(2) NOT = SPACES
              MOVE LTC-HHGS(2) TO DETAIL-LINE
              PERFORM WRITE-DETAIL-LINE.
           IF LTC-HHGS(3) NOT = SPACES
              MOVE LTC-HHGS(3) TO DETAIL-LINE
              PERFORM WRITE-DETAIL-LINE.

       EXIT-LT-COLLATERAL.
           PERFORM CLOSE-LTC1-FILE.

      ***********************
       GET-FEE-DESC SECTION.
      ***********************
           PERFORM OPEN-CDB1-FILE.
           MOVE "GC"            TO CDB-TYPE.
           MOVE LN-FEECODE(SUB) TO CDB-CODE D-FEECODE(SUB2).
           MOVE LN-OWNBR        TO CDB-BRANCH.
           PERFORM READ-CDB1-FILE.
           IF IO-FG = 9
              MOVE " * NOT ON FILE * " TO CDB-DESC.
           MOVE CDB-DESC TO D-FEEDESC(SUB2).
       GET-FEE-EXIT.
           PERFORM CLOSE-CDB1-FILE.

      *****************************
       LOCATE-JOINT-MAKER SECTION.
      *****************************
           MOVE 4 TO SUB.
       LOCATE-NEXT.
           IF LN-MAKERCD(SUB) = "J"
              MOVE LN-SSNO(SUB) TO JOINT-SSNO BW-SSNO
              PERFORM READ-BW1-FILE
              IF IO-FG = 0
                 MOVE SPACES TO JOINT-NAME
                 STRING BW-LNAME,", "BW-FNAME DELIMITED BY "  "
                    INTO JOINT-NAME.
           IF SUB > 2
              SUBTRACT 1 FROM SUB
              GO TO LOCATE-NEXT.

      *******************************
       HEAD-PAGE SECTION.
      *******************************

           IF PRINT-REVS = "C"
              PERFORM CUSTOMER-HEAD-PAGE
              GO TO EXIT-HEAD-PAGE.

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
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-PR-REC.

           PERFORM LPSTAT-DISPLAY.
           MOVE LPS-DISPLAY-LNNO TO D-LOAN-NUM.
           MOVE LN-LOANDATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO D-LOANDATE.
           MOVE LN-1STPYDA TO D-DUE.
           MOVE HEAD4 TO PR-REC.
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-PR-REC..

           MOVE SPACES TO HEAD13.
           MOVE BW-LNAME TO NAME-LINE.
           PERFORM ADD-COMMA.
           MOVE BW-FNAME TO NAME-WK.
           PERFORM ADD-NAME.
           MOVE NAME-LINE TO D-BWNAME.
           MOVE "MAKER : " TO D-SSNOTAG.
           IF BW-SSNO NOT = 0
              MOVE BW-SSNO TO D-SSNO
              INSPECT D-SSNO REPLACING ALL "/" BY "-"
              IF GP-HIDE-FIRST-5-SSNO = "Y"
                 MOVE ALL "X" TO D-SOC1 D-SOC2.
           MOVE HEAD13 TO PR-REC.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-PR-REC.

           MOVE SPACES TO HEAD13.
           IF BW-ADR2 = SPACES
              MOVE BW-ADR1 TO D-BWNAME
           ELSE
              STRING BW-ADR1,", "BW-ADR2 DELIMITED BY "  "
                  INTO D-BWNAME.
           MOVE "PHONE : " TO D-SSNOTAG.
           IF BW-PHONE NOT = 0
              MOVE BW-PHONE TO D-PHONE
              INSPECT D-PHONE REPLACING ALL "/" BY "-".
           MOVE " AGE      PROFILE" TO D-AGE-PROFILE.
           MOVE HEAD13 TO PR-REC.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-PR-REC.

           MOVE SPACES TO HEAD13.
           MOVE BW-CITY TO NAME-LINE.
           MOVE BW-STATE TO NAME-WK.
           PERFORM ADD-NAME.
           MOVE BW-ZIP TO NAME-WK.
           PERFORM ADD-NAME.
           MOVE NAME-LINE TO D-BWNAME.
           MOVE "CONTRACT:" TO D-CON-REC.
           IF NO-CPOT30 = "Y"
              MOVE "<P>" TO D-AGE-ALP
           ELSE
           IF NO-CPOT30 = "M"
              MOVE "<M>" TO D-AGE-ALP
           ELSE
           IF NO-CPOTCHGOFF = "Y"
              MOVE "<PC>" TO D-AGE-ALP
           ELSE
           IF NO-CPOTCHGOFF = "M"
              MOVE "<PL>" TO D-AGE-ALP
           ELSE
              MOVE NO-TRUCONTR TO D-AGE.
           MOVE LN-CONPROFILE(1) TO PROFILE-NUM.
           PERFORM PROFILE-DASH.
           MOVE HEAD13 TO PR-REC.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-PR-REC.

           MOVE SPACES TO HEAD13.
           MOVE JOINT-NAME TO D-BWNAME.
           IF JOINT-NAME = SPACES
              IF BW-SPFNAME NOT = SPACES
                 STRING BW-SPLNAME,", "BW-SPFNAME DELIMITED BY "  "
                    INTO D-BWNAME.
           MOVE "JOINT : " TO D-SSNOTAG.
           IF JOINT-SSNO NOT = 0
              MOVE JOINT-SSNO TO D-SSNO
              INSPECT D-SSNO REPLACING ALL "/" BY "-"
              IF GP-HIDE-FIRST-5-SSNO = "Y"
                 MOVE ALL "X" TO D-SOC1 D-SOC2.
           MOVE "RECENCY :" TO D-CON-REC.
           IF NO-RDPOT30 = "Y"
              MOVE "<P>" TO D-AGE-ALP
           ELSE
           IF NO-RDPOT30 = "M"
              MOVE "<M>" TO D-AGE-ALP
           ELSE
           IF NO-RDPOTCHGOFF = "Y"
              MOVE "<PC>" TO D-AGE-ALP
           ELSE
           IF NO-RDPOTCHGOFF = "M"
              MOVE "<PL>" TO D-AGE-ALP
           ELSE
              MOVE NO-RECDEL TO D-AGE.
           MOVE LN-RECPROFILE(1) TO PROFILE-NUM.
           PERFORM PROFILE-DASH.
           MOVE HEAD13 TO PR-REC.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-PR-REC.

           MOVE 11 TO LN-COUNT.
           IF WRITE-DETAIL-HEADER = "Y"
              IF PRINT-REVS = "I"
                 PERFORM DETAIL-CUST-HEADER
              ELSE
                 PERFORM DETAIL-HEADER.
           MOVE 1 TO LN-FEED.

       EXIT-HEAD-PAGE.
           EXIT.

      *****************************
       CUSTOMER-HEAD-PAGE SECTION.
      *****************************

      * PRINT-REVS = "C"  CUSTOMER VERSION

      * USER ID:
           MOVE HEAD2 TO PR-REC.
           MOVE 99 TO LN-FEED.
           PERFORM WRITE-PR-REC.

      * OWN BR.:
           MOVE HEAD3 TO PR-REC.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-PR-REC.

      * BORROWER:
           PERFORM LPSTAT-DISPLAY.
           MOVE LPS-DISPLAY-LNNO TO D-LOAN-NUM.
           MOVE LN-LOANDATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO D-LOANDATE.
           MOVE LN-1STPYDA TO D-DUE.
           MOVE HEAD4 TO PR-REC.
           PERFORM WRITE-PR-REC.

           MOVE SPACES TO HEAD13.
           MOVE BW-LNAME TO NAME-LINE.
           PERFORM ADD-COMMA.
           MOVE BW-FNAME TO NAME-WK.
           PERFORM ADD-NAME.
           MOVE NAME-LINE TO D-BWNAME.
           MOVE "MAKER : " TO D-SSNOTAG.
           IF BW-SSNO NOT = 0
              MOVE BW-SSNO TO D-SSNO
              INSPECT D-SSNO REPLACING ALL "/" BY "-"
              IF GP-HIDE-FIRST-5-SSNO = "Y"
                 MOVE ALL "X" TO D-SOC1 D-SOC2.
           MOVE HEAD13 TO PR-REC.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-PR-REC.

           MOVE SPACES TO HEAD13.
           IF BW-ADR2 = SPACES
              MOVE BW-ADR1 TO D-BWNAME
           ELSE
              STRING BW-ADR1,", "BW-ADR2 DELIMITED BY "  "
                  INTO D-BWNAME.
           MOVE "PHONE : " TO D-SSNOTAG.
           IF BW-PHONE NOT = 0
              MOVE BW-PHONE TO D-PHONE
              INSPECT D-PHONE REPLACING ALL "/" BY "-".
           MOVE " AGE      PROFILE" TO D-AGE-PROFILE.
           MOVE HEAD13 TO PR-REC.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-PR-REC.

           MOVE SPACES TO HEAD13.
           MOVE BW-CITY TO NAME-LINE.
           MOVE BW-STATE TO NAME-WK.
           PERFORM ADD-NAME.
           MOVE BW-ZIP TO NAME-WK.
           PERFORM ADD-NAME.
           MOVE NAME-LINE TO D-BWNAME.
           MOVE "CONTRACT:" TO D-CON-REC.
           IF NO-CPOT30 = "Y"
              MOVE "<P>" TO D-AGE-ALP
           ELSE
           IF NO-CPOT30 = "M"
              MOVE "<M>" TO D-AGE-ALP
           ELSE
           IF NO-CPOTCHGOFF = "Y"
              MOVE "<PC>" TO D-AGE-ALP
           ELSE
           IF NO-CPOTCHGOFF = "M"
              MOVE "<PL>" TO D-AGE-ALP
           ELSE
              MOVE NO-TRUCONTR TO D-AGE.
           MOVE LN-CONPROFILE(1) TO PROFILE-NUM.
           PERFORM PROFILE-DASH.
           MOVE HEAD13 TO PR-REC.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-PR-REC.

           MOVE SPACES TO HEAD13.
           MOVE JOINT-NAME TO D-BWNAME.
           IF JOINT-NAME = SPACES
              IF BW-SPFNAME NOT = SPACES
                 STRING BW-SPLNAME,", "BW-SPFNAME DELIMITED BY "  "
                    INTO D-BWNAME.
           MOVE "JOINT : " TO D-SSNOTAG.
           IF JOINT-SSNO NOT = 0
              MOVE JOINT-SSNO TO D-SSNO
              INSPECT D-SSNO REPLACING ALL "/" BY "-"
              IF GP-HIDE-FIRST-5-SSNO = "Y"
                 MOVE ALL "X" TO D-SOC1 D-SOC2.
           MOVE "RECENCY :" TO D-CON-REC.
           IF NO-RDPOT30 = "Y"
              MOVE "<P>" TO D-AGE-ALP
           ELSE
           IF NO-RDPOT30 = "M"
              MOVE "<M>" TO D-AGE-ALP
           ELSE
           IF NO-RDPOTCHGOFF = "Y"
              MOVE "<PC>" TO D-AGE-ALP
           ELSE
           IF NO-RDPOTCHGOFF = "M"
              MOVE "<PL>" TO D-AGE-ALP
           ELSE
              MOVE NO-RECDEL TO D-AGE.
           MOVE LN-RECPROFILE(1) TO PROFILE-NUM.
           PERFORM PROFILE-DASH.
           MOVE HEAD13 TO PR-REC.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-PR-REC..

           MOVE 10 TO LN-COUNT.
           IF WRITE-DETAIL-HEADER = "Y"
              PERFORM DETAIL-CUST-HEADER.
           MOVE 1 TO LN-FEED.

      *******************************
       GET-ENDORSER SECTION.
      *******************************
           MOVE 2 TO SUB.
       NEXT-E-CHK.
           IF LN-MAKERCD(SUB) = "E"
              GO TO MAKERCD-FOUND.
           IF SUB < 4
              ADD 1 TO SUB
              GO TO NEXT-E-CHK.
           GO TO EXIT-GET-ENDORSER.
       MAKERCD-FOUND.
           MOVE LN-SSNO(SUB) TO BW-SSNO.
           PERFORM READ-BW1-FILE.
           IF IO-FG = 9
              MOVE "* INVALID *" TO DETAIL-LINE
              GO TO EXIT-GET-ENDORSER.
           MOVE BW-LNAME TO NAME-LINE.
           MOVE BW-FNAME TO NAME-WK.
           PERFORM ADD-NAME.
           PERFORM ADD-COMMA.
           MOVE BW-ADR1 TO NAME-WK.
           PERFORM ADD-NAME.
           MOVE BW-ADR2 TO NAME-WK.
           PERFORM ADD-NAME.
           PERFORM ADD-COMMA.
           MOVE BW-CITY TO NAME-WK.
           PERFORM ADD-NAME.
           MOVE BW-STATE TO NAME-WK.
           PERFORM ADD-NAME.
           MOVE BW-ZIP TO NAME-WK.
           PERFORM ADD-NAME.
           MOVE NAME-LINE TO ENDORSER-INFO.
       EXIT-GET-ENDORSER.
           EXIT.

       COPY "LIBGB/DATER.CPY".
       COPY "LIBWI/PFSWINDOWS.CPY".
       COPY "LIBGB/GPENV.CPY".
       COPY "LIBGB/TIMEIO.CPY".
       COPY "LIBLP/LPSTAT.CPY".
       COPY "LIBLP/LPMDTE.CPY".
       COPY "LIBLP/LPAMTS.CPY".
       COPY "LIBGB/SCREEN.CPY".

      *****************************************
       SETUP-LINK-INFO SECTION.
      *****************************************
           MOVE LONPF-HELP--FILE TO HELP-LINK-FILE.
           MOVE LONPF-HELP--DIR TO HELP-LINK-DIR.

      *****************************************
       VDU-CONVERT-FIELD SECTION.
      *****************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBLP/LONPF_EVA.CPY".
              COPY "LIBWI/LONPP_EVA.CPY".
              COPY "LIBSP/CPINQ_EVA.CPY".
              COPY "LIBLP/AUDIQ_EVA.CPY".
              COPY "LIBWI/CLMENU_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      **************************************
       MISC-PARA SECTION.
      **************************************
       FORM-RESET.
           DISPLAY SPACES UPON LONPP-WINDOW-HANDLE
                          LINE 1 COL 1 ERASE SCREEN.
           DISPLAY LONPP-SCREEN UPON LONPP-WINDOW-HANDLE.
           MOVE LONPP-FORM-FIELDS TO WR-BUF. 
           MOVE LONPP-FORM-LIST TO WR-LIST. 
           PERFORM CALL-WRFORM. 

       COPY "LIBGB/GETPR.CPY".
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/GETENV.CPY".
      *COPY "LIBGB/MKTEMP.CPY".
       COPY "LIBGB/PHONEIO.CPY".
       PROGRESS-MSG.
           MOVE MESS TO WR-BUF.
           MOVE MESS-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.
       SEND-WSEL-LEGEND.
           MOVE MESS TO WR-BUF.
           MOVE WSEL-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.
       ADD-ON.
           INSPECT NAME-LINE REPLACING FIRST "  " BY " +".
       ADD-COMMA.
           INSPECT NAME-LINE REPLACING FIRST "    " BY ",   ".
       ADD-NAME.
           PERFORM ADD-ON.
           INSPECT NAME-LINE REPLACING FIRST
              "+                       " BY NAME-WK.
       DETAIL-CUST-HEADER.
           MOVE ALL "-" TO PR-REC.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-PR-REC.
           MOVE CUST-HEAD11 TO PR-REC.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-PR-REC.

           MOVE ALL "-" TO PR-REC.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-PR-REC.
           ADD 3 TO LN-COUNT.
       DETAIL-HEADER.
           MOVE DASH-LINE TO PR-REC.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-PR-REC.
           MOVE HEAD11A TO PR-REC.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-PR-REC.
           MOVE HEAD11 TO PR-REC.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-PR-REC.
           MOVE DASH-LINE TO PR-REC.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-PR-REC.
           MOVE 1 TO LN-FEED.
           ADD 4 TO LN-COUNT.
           MOVE "Y" TO WRITE-DETAIL-HEADER.
       PRINT-LINE.
           IF PAYDATE-ONLY-FG = "Y"
              MOVE LP-PAYDATE TO DATE-YYYYMMDD
           ELSE
              MOVE LP-TRDATE  TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO D-DATE.
           IF LP-REV NOT = " "
              MOVE "*"      TO D-MISC.
           MOVE LP-TRCD     TO D-TRCD.

           MOVE LP-REFNO    TO D-REFCD LPWSLP-LP-REFNO.
         
           IF LP-TRCD = "JD" OR "AC" OR "IR"
              MOVE LP-NEWSTAT-RATE TO D-TRAMT-RATE
           ELSE
              MOVE LP-TRAMT TO D-TRAMT
              IF ((LP-TRCD = "RT" OR "ET" OR "LP" OR "NP" OR "ST") OR
                   (LP-TRCD = "RV" AND
                    (LPWSLP-LP-ADDON-TEXT = "ADD" OR "ADJ" OR "CNL")))
                 IF LP-IBPC = "P"
                    MOVE LP-APCUR TO D-TRAMT.
           COMPUTE D-APOTH = LP-APOTH + LP-APOT2.
           COMPUTE D-APINT = LP-APINT + LP-APLC.
           MOVE LP-APCUR TO D-APCUR.
           IF ( (LP-TRCD = "RT" OR "ET" OR "LP" OR "NP" OR "ST") OR
                 (LP-TRCD = "RV" AND
                  (LPWSLP-LP-ADDON-TEXT = "ADD" OR "ADJ" OR "CNL") ) )
               AND LP-IBPC = "P"
                   MOVE LP-TRAMT TO D-APCUR.
           IF LP-REV NOT = "Y" AND LP-TRCD NOT = "RV"
              MOVE LP-PDTH-DATE TO NUM-DATE
              MOVE NUM-MO TO D-PDTHRU-MO
              IF LN-UNITPER-CD-DAYS
                 MOVE NUM-DA TO D-PDTHRU-DAYR
              ELSE
                 MOVE NUM-YR TO D-PDTHRU-DAYR
              END-IF
      * IF WEEKLY PRINT DAY ELSE PRINT MONTH
              MOVE LP-LCPDTH-DATE TO NUM-DATE
              IF LN-UNITPER-CD-DAYS
                 MOVE NUM-DA TO D-LCPDTH
              ELSE
                 MOVE NUM-MO TO D-LCPDTH
              END-IF
              COMPUTE D-OTHBAL = LP-OTHBAL + LP-OT2BAL
              COMPUTE D-INTBAL = LP-INTBAL + LP-LCBAL.
           MOVE LP-CURBAL TO D-CURBAL.
           PERFORM WRITE-DETAIL-LINE.

           IF (LP-TRDATE NOT = LP-PAYDATE) AND
              (PAYDATE-ONLY-FG = "N")
              MOVE LP-PAYDATE TO DATE-YYYYMMDD
              PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
              MOVE DATE-MMDDYY TO D-DATE
              MOVE " BACKDATE " TO D-BACKDATE
              IF (LP-TRCD = "RT" OR "ST") AND
                 LP-REV NOT = "Y"
                 MOVE "NEW PAYMENT" TO D-PMESS
                 MOVE LP-LCPARTIALS TO D-NEWPMT
                 PERFORM WRITE-DETAIL-LINE
              ELSE
                 PERFORM WRITE-DETAIL-LINE
           ELSE
           IF (LP-TRCD = "RT" OR "ST") AND
              LP-REV NOT = "Y"
              MOVE "NEW PAYMENT" TO D-PMESS
              MOVE LP-LCPARTIALS TO D-NEWPMT
              PERFORM WRITE-DETAIL-LINE.

       PROFILE-DASH.
           INSPECT PROFILE-ALP REPLACING ALL "9" BY "-".
           MOVE PROFILE-ALP TO D-PROFILE.
       PRINT-CUST-LINE.
           MOVE LP-PAYDATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO C-DATE.
           MOVE LP-TRCD     TO C-TRCD.

           MOVE LP-REFNO    TO C-REFCD LPWSLP-LP-REFNO.

           IF LP-TRCD = "JD" OR "AC" OR "IR"
              MOVE LP-NEWSTAT-RATE TO C-TRAMT-RATE
           ELSE
              MOVE LP-TRAMT TO C-TRAMT
              IF ((LP-TRCD = "RT" OR "ET" OR "LP" OR "NP" OR "ST") OR
                   (LP-TRCD = "RV" AND
                    (LPWSLP-LP-ADDON-TEXT = "ADD" OR "ADJ" OR "CNL")))
                 IF LP-IBPC = "P"
                    MOVE LP-APCUR TO C-TRAMT.
           COMPUTE C-APOTH = LP-APOTH + LP-APOT2.
           MOVE LP-APINT TO C-APINT.
           MOVE LP-APLC TO C-APLCHG.
           MOVE LP-APCUR TO C-APCUR.
           IF ( (LP-TRCD = "RT" OR "ET" OR "LP" OR "NP" OR "ST") OR
                 (LP-TRCD = "RV" AND
                  (LPWSLP-LP-ADDON-TEXT = "ADD" OR "ADJ" OR "CNL") ) )
               AND LP-IBPC = "P"
                   MOVE LP-TRAMT TO C-APCUR.

           MOVE LP-PDTH-DATE TO NUM-DATE.
           MOVE NUM-MO TO C-PDTHRU-MO
           IF LN-UNITPER-CD-DAYS
              MOVE NUM-DA TO C-PDTHRU-DAYR
           ELSE
              MOVE NUM-YR TO C-PDTHRU-DAYR.
      * IF WEEKLY PRINT DAY ELSE PRINT MONTH
           MOVE LP-LCPDTH-DATE TO NUM-DATE.
           IF LN-UNITPER-CD-DAYS
              MOVE NUM-DA TO C-LCPDTH
           ELSE
              MOVE NUM-MO TO C-LCPDTH.

           COMPUTE C-OTHBAL = LP-OTHBAL + LP-OT2BAL
           MOVE LP-INTBAL TO C-INTBAL.
           MOVE LP-LCBAL TO C-LCBAL.
           MOVE LP-CURBAL TO C-CURBAL.

      * HAVE TO DO THE DETAIL LINE THIS WAY BECAUSE ITS 132 CHARS
      * CANT REDEFINE DETAIL-LINE
           MOVE 1 TO LN-FEED.
           ADD LN-FEED LN-COUNT GIVING LN-WK.
           IF LN-WK GREATER THAN EXT-CONAME-RPT-LINES
              PERFORM HEAD-PAGE.

           MOVE CUST-DETAIL8 TO PR-REC.
           PERFORM WRITE-PR-REC.
           ADD LN-FEED TO LN-COUNT.
           MOVE 1 TO LN-FEED.
           MOVE SPACES TO DETAIL-LINE.


      ***********************
       IO-ROUTINES SECTION.
      ***********************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBLP/LPSPGS_SQL.CPY".
       COPY "LIBLP/LPLTPGS_SQL.CPY".
       COPY "LIBLP/LPLTCGS_SQL.CPY".
       COPY "LIBLP/LPLPGS_SQL.CPY".
       COPY "LIBLP/LPLNGS_SQL.CPY".
       COPY "LIBLP/LPLMGS_SQL.CPY".
       COPY "LIBLP/LPDLGS_SQL.CPY".
       COPY "LIBLP/LPCDBGS_SQL.CPY".
       COPY "LIBLP/LPCDGS_SQL.CPY".
       COPY "LIBLP/LPBWGS_SQL.CPY".
       COPY "LIBGB/GBGBGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBPRINT.CPY".
       COPY "LIBGB/GBGBRN.CPY".
       COPY "LIBLP/LPLN1RN.CPY".
       COPY "LIBLP/LPLTC1RN.CPY".
       COPY "LIBLP/LPLTP1RN.CPY".
       COPY "LIBLP/LPBW1RN.CPY".
       COPY "LIBLP/LPLM1RN.CPY".
       COPY "LIBLP/LPCD1RN.CPY".
       COPY "LIBLP/LPCDB1RN.CPY".
       COPY "LIBLP/LPLP1RN.CPY".
       COPY "LIBLP/LPDL1RN.CPY".
       COPY "LIBLP/LPSP1RN.CPY".
       COPY "LIBGB/GBBRRN.CPY".

       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO ERRCD.

      **************************************
       END-PROGRAM SECTION.
      **************************************
       END-RUN.
           PERFORM CLOSE-FILES.
           PERFORM PRU-OUT.
       EXIT-PROG.
           DESTROY LONPP-SCREEN.
           DESTROY LONPP-WINDOW-HANDLE.
       STOP-RUN.
           EXIT PROGRAM.
