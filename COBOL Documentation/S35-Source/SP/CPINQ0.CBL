       IDENTIFICATION DIVISION.
       PROGRAM-ID.      CPINQ0.
       DATE-WRITTEN.    013184.
      ***********************************************************
      *                 (SP)
      *  DESCRIPTION:   COLLECTION PROCESSING INQUIRY - F10 INQUIRY
      *                 PROG 0 - HEADER
      *
      *         CALLED FROM CL/WIMENU F10 INTERRUPT
      *                 IF BR-NEW-COLL-INQUIRY = "N"
      *
      *         SP/OPINQ0 IS OLD INQUIRY CALLED FROM LPCPMU
      *                 IF BR-NEW-COLL-INQUIRY = "N"
      *         SP/SPINQ0 IS NEW INQUIRY CALLED FROM LPCPMU
      *                 IF BR-NEW-COLL-INQUIRY = "Y"
      *         SP/XPINQ IS NEW INQUIRY CALLED FROM F10 INTERRUPT
      *                 IF BR-NEW-COLL-INQUIRY = "Y"
      *
      * REV:
      *  012188 LS  AFTER DISPLAYING LOAN REC,IF OWNING
      *             BRANCH NOT = LOGIN BRANCH, MESSAGE
      *             WILL DISPLAY
      *  052088 BR  DISPLAY PRIORITY BW MEMO IF
      *             BR-PRIO-MEMO-FLAG = "Y"
      *  012489 SLC WHEN SCANNING WE NOW READ SPR FOR EACH
      *             LOAN TO CORRECT DELINQUENCY PROBLEM.
      *  012789 SLC REMOVED COPYFILES LPINDU AND LPINDUW.
      *             THEY ARE NOW PART OF LPRATE.
      *  013089 BAH ADDED SECOND PROMISE DATE TO SCREEN
      *  032789 JTG CHANGED TO DISPLAY LN-MESSAGE IN BOLD
      *  110289 BAH RESET AVAILABLE CREDIT ON SCANNING
      *  113089 SLC DISPLAY LATE CHARGE ALWAYS.
      *  031590 SLC ADDED LISTING OF LOANS WITH MULTIPLE SEQ
      *  040590 JTG CHANGED REFERENCE FROM RECENCY TO RECDEL
      *  040990 JTG ADDED DISPLAY OF LN-TOTNOALDEL (PACESETTER)
      *  052190 JTG ADDED DISPLAY OF AVCR-RESTRICT-FG (GA)
      *  080390 BAH DISPLAY LN-SUM-LNAMT LN-SUM-ORGTERM LN-SUM-FINCHG
      *                     LN-SUM-REGPYAMT
      *  080790 BAH ADD LT-FILE FOR ADDONS (SPPDTH)
      *  091090 BAH CHANGED REFERENCE OF RECDEL BACK TO RECENCY
      *  091390 SLC INITIALIZE LCHG-LATE TO PDTH-CUR-SCHD-PAY.
      *  091490 SLC ADDED LPSCHD(W) TO UNPACK PMT SCHD IN LATE CHARGES.
      *  102190 JTG CHANGED DISPLAY OF TRCD'S
      *  072591 JTG CHANGED LPLT1IN TO LPLT1RN
      *  052792 JTG DISPLAYED CUR-SCHD-PAY INSTEAD OF REGPYAMT
      *  060192 JTG CHANGED TO USE RECDEL NOT RECENCY
      *  072092 BAH ADDED H071 TO DISPLAY "S" IF LN-PAY-SCHLD-FG = Y
      *  100192 BAH ADDED LC-DISPLAY-ERN = 'P', PAYMNTD
      *  123094 BAH ADDED EXT-WI-CL-SYSDATE FOR HARD COPY PRINT FROM F9
      *             ALL ACCTS WERE MATURED WITHOUT SYSDATE.
      *  050995 BAH ADDED COMAKER DISPLAY, BEFORE IT TESTS FOR SPOUSE
      *  083195 KEC ADDED LIBLP/LPNOCL
      *  090495 JTG ADDED TEST FOR GP-NO-LC-IN-ARREARAGE
      *  013196 JTG STOPPED FLASHING HIGH CREDIT WHEN CREDIT LIMIT = 0
      *  MJD 042496 CHANGED TO ACCEPT 8 DIGIT ACCOUNT NUMBER FROM BWLNSC.
      *             REPLACED LNDISP WITH MORELN WINDOW.
      *  MJD 071296 ADDED TEST OF LN-UNITPER-CD FOR DISPLAY OF PDTH.
      *  MJD 071696 ADDED LIBLP/LPUPWK FOR CHANGES IN LIBLP/AGEING
      *  BLL 072596 MOVED MORELNW SO 03 LEVEL WAS UNDER WINDOWSW
      *  MJD 072996 ADDED LOGIC TO PRINT LN-1STPYDATE AS MM/DDWYY
      *             FOR WEEKLY UNIT PERIODS.
      *  BAH 111596 ADDED AVCR-CURRENT-AVAIL TO DISPLAY FOR WDT WHEN
      *             BR-AVAIL-CREDIT = "S" (335)
      *  KEC 112696 CHANGED LN-POCD FOR RESCISSION.
      *  JTG 010397 CHANGED TEST FOR LCPDTH & LCHG DISPLAY TO NEVER DISPLAY
      *             IF SP-LCFRMLA = " " AND REMOVED TEST ON JUDGEMENT
      *             SINCE ACCOUNTS IN JUDGEMENT CAN CONTINUE AS PRECOMP
      *  MJD 011097 ADDED EXT-WI-CL-PAY-SCHLD-FG
      *  GLF 050797 ADDED COPY MEMBERS: SETCTBLW AND SETCTBL
      *  MJD 051597 ADDED LPPOF2 COPY MEMBER (PREVIOUSLY WAS PART OF LPPOFF)
      *  JTG 052097 ADDED 'LIBLP/ADDONSPR' TO GET CORRECT SPR RE: INS RB
      *   CS 052197 ADDED DISPLAY OF BW-CREDIT-SCORE IF GREATER THAN ZERO
      *  MJD 060997 ADDED "F5-GLOBAL"
      *             THAT WILL CALL BILNSC. ADDED INTERBRANCH LOGIC TO
      *             RESET PATH AFTER INQUIRING ON AN ACCOUNT IN ANOTHER
      *             DATA PATH. ADDED PASSWORD PROTECTION TO GLOBAL
      *             LOAN SCAN.
      *  BAH 061897 ADDED DISPLAY OF EXPANCED COLLATERAL IF COLLID IS BLANK
      *  MJD 082297 CHANGED LN-REC/CONPROFILE TO LN-REC/CONPROFILE(1)
      *  GLF 090997 REPLACED 'IO' ON GP-FILE WITH GET 'GPENV'
      *  KEC 091897 ADDED 'DELINQ-SETTING' AND 'DELINQ-SETTING-LIST' IN
      *              ORDER TO KEEP TRACK OF THE DELINQUENCY PROFILE.
      *  GLF 101797 ADDED MOVE LN-APP-NUMBER TO EXT-WI-CL-APP-NUMBER
      *  BAH 062598 ALLOW F7-EXIT FROM ENTER DELINQUENCY CODES (CRD)
      *  MJD 072098 CHANGED TO DISPLAY AMOUNT DUE ON PAYMENT IN
      *             REVERSE VIDEO IF WORLD AND LC-PROF1 = "D" OR
      *             LC-PROF2 = "D"
      *  JTG 121698 DISPLAYED 'R' VIA UNITPER-MASK LOGIC FOR REVOLVING ACCOUNT
      *  MJD 990901 CHANGED THE FOLLOWING: IF BR-INBR-BILNSC-FG = "M"  THEN,
      *             WHEN F4-GLOBAL IS USED, THE USER WILL SEE A SMALL WINDOW
      *             PROMPTING THEM TO ENTER A BRNO AND ACCTNO.  UPON ENTRY
      *             THE USER WILL SEE THE LOAN DISPLAYED. (MULL PR# 925)
      *  BAH 001004 DISPLAY BW-REPCODE AFTER NAME FOR TAX PREREG WORLD PR# 240
      *  BAH 010628 DISPLAY "OO" IF BW-PRIVACY-OPT-OUT = "Y", TFC PR# 90
      *  CS  010823 GP-BW-PRIVACY-OPT-OUT MUST BE SET TO "Y" TO DISPLAY "OO"
      *             WHEN BW-PRIVACY-OPT-OUT = "Y"
      *  JTG 010904 ALLOWED IB TYPE LOANS TO ACCELERATE     JIM R. (ALL) PR#1582
      *  BLV 011101 ADD DISPLAY OF WCBC CREDIT LIMIT, WORLD PR# 283.  NOTE:
      *             I ADDED A 1 CHARACTER FIELDS (HFILLER) TO FORM, BUT NEW
      *             FORM ONLY NEEDED IF GP-I-AM-WORLD
      *  BAH 030602 REMOVE "AVAILABLE:" ON RIGHT HAND SIDE IF BR-AVAIL-CREDIT
      *             = "S" (FINMGM) AND CLASS 5, FINMGM PR# 2086
      *  CS  030620 VERIFY PASSWORD IF LN-BNKRPTDATE NOT ZERO[REGACC PR#31]
      *  BAH 050426 WAS NOT DISPLAYING LOAN ENDORSER, ONLY JOINT, THRIFT PL# 482
      *  BAH 050713 ADDED GP-HIDE-FIRST-5-SSNO TO "X" OUT FIRST 5 OF SSNO IF
      *             SET TO "Y", REGACC PR# 109
      *  BAH 060208 REMOVE "AVAILABLE:" ON RIGHT HAND SIDE IF BR-AVAIL-CREDIT
      *             = "S" (FINMGM) AND CLASS 8, FINMGM PR# 2702
      *  BLF 060609 IF LN-ACH = "Y" & GP-LOAN-ACH-PROCESSING = "Y"
      *             DISPLAY INVERSE PROMPT "PAY ACH" INSTEAD OF
      *             "PAYMENT", LENDMARK PR# 2711
      * BAH 070504 DISPLAY NEW INSURANCE TRACKING FIELDS FROM THE LT-A RECORD
      *            IN PLACE OF THE BM PRIORITY MEMO FOR REGACC PR# 173
      *  BLF 070802 IF RETURNING FROM F6 SCAN WITH BWLNSC-ARCHIVE-FG = "A",
      *             SET UP ARCHIVE PATHS FOR LN, LP, & LT; RESET TO NORMAL
      *             ../ PATHS AT BEGINNING OF ENTER-KEYS, WORLD PR# 547
      *  BLF 070907 WHEN ACCT # ENTERED, IF GP-PURGE-TO-ARCHIVE-FG = "Y",
      *             CHECK FOR LOAN IN ARCHIVE & DISPLAY IF FOUND, WORLD PR# 547
      *  BAH 071016 IF GP-I-AM-REGACC, DISPLAY LN-PART-RECOURSE-DATE INSTEAD
      *             OF TYPE OF RECOURSE IN BOTTOM RIGHT CORNER, PR# 184
      *  BAH 080722 DO NOT DISPLAY AVAIL CREDIT FOR WORLD IF SCFRMLA 18 AND
      *             CASH AVAIL < 10 % OF AMTFIN, WORLD PR# 593
      *  BAH 081120 REPLACE THE OV: PROMPT WITH A FIELD (P19) AND DISPLAY
      *             IR: FOR GP-I-AM-REGACC IF LN-REDURATE EXISTS, REGACC PR#229
      *   CS 110223 MODERN WANTED TO BE ABLE TO RECALL THE ACCOUNT YOU WERE
      *             ON IF THEY HIT F1.  PRESSING ENTER AT THE LOANNO WILL
      *             RECALL THE LOAN WHILE WITHIN THE INQUIRY MODERN PR#3720
      *  KEC 120212 {PR#03978} PARADATA <A15> [DAVID SINCLAIR]
      *             CREATED THE ABILITY TO CHOOSE THE WAY THE 'MESSAGE'
      *             IS DISPLAYED ON THE ACCOUNT INQUIRY, PAYMENT
      *             POSTING AND COLLECTION WORK SCREENS.
      *  BAH 120522 ADDED DEBTAID-SEQ TO PASSWORD DISPLAYING DEBTAID ACCOUNTS
      *                                              REGACC PR# 311
      *  BAH 120727 ADDED CLASS 11 TO BR-AVAIL-CREDIT "S" IF STATEMENT,
      *                                                       FINMGM PR# 4085
      *  BAH 130111 ADDED LC-DISPLAY-ERN OPTION OF "I" FOR POFF-INTDUE
      *                                              AMERICAN ACC PR# 4190
      *  BAH 130819 DONT DISPLAY "OO" FOR WORLD BW-PRIVACY-OPT-OUT PR# 919
      *  BAH 130903 ADDED BR-AVAIL-CREDIT "B" TO DISPLAY "BRANCH", BR-NO
      *             AND HIGHLIGHT (REVERSE) HONOR PR# 4358
      *  BLM 141015 SET OVERRIDE FLAGS FOR LN, LP, LT WHEN SETTING UP ARCHIVE
      *             PATH, & RESET THEM IN RESET-PATH
      *  BLM 141020 REVISE LOGIC FOR OLD & NEW DATA PATHS FOR NEW LOAD
      *             ROUTINES.  ALSO, REMOVE SOME UNUSED CODE FOR OLD SCANS &
      *             OS-DISPLAY-LOANS & THE IO COPY MEMBERS 
      *   CS 150526 FIXED HELP WHEN F4-GLOBAL SCAN SHOULD BE INBRT2
      *  BAH 160113 SPLIT OUT LTFILE, LTCFILE, LTAFILE PD0003
      *  BAH 160226 REMOVED LHFILE, PD0003
      * KEC 2016.0509 {PD#00003} PARADATA <A30> [JAY CISZEWSKI]
      *         CHANGED LN-APPNO TO USE LN-APP-NUMBER FOR MA1-KEY.
      * BLM 160914 FIX BILNSC-ACCTNO
      * BAH 161102 COMMENTED OUT TEST-IF-SC-RESTRICTION, MOVED TO LPAVCR
      *                                                     WORLD PR# 1199
      *  BAH 160419 DISPLAY REAFFIRMED AND DISCHARGED FOR WORLD PR#1150A
      *  BLM 170125 IN WORLD-CREDIT-DISPLAY, IF ACCOUNT WOULD BE A PB
      *             DELINQUENT IF RENEWED TODAY, DISPLAY A "D" NEXT TO
      *             THE AVAILABLE CREDIT, WORLD PR# 1220
      *  BAH 170320 DISPLAY LTB-DISCHARGE-DATE WITH DISCHARGE MESSAGE FROM
      *             ABOVE CHANGE WORLD PR#1150, WORLD PR#1234
      *   CS 170321 IF WORLD, IF ACCOUNT IS 'BK' PERFORM VERIFY-PASSWORD, IF
      *             STAT NOT 'OK', DISPLAY FULL SCREEN WINDOW, IF THE USER
      *             TYPES 'Y' TO CONTINUE INTO ACCOUNT, CREATE A LOAN MEMO
      *             "BK ACCOUNT ACCESSED" WITH USERID /TIME. ALSO CREATE A
      *             LOG FILE /USR/DATA/R?/LG WORLD PR#1234
      *   CS 170323 MOVED TEST ON PASSWORD FOR BK(SEQ1) AND DEBTAID. THE PASS
      *             WORD PROMPTED IN DISPLAY-LOAN BUT IF INVALID SET YOU OUT
      *             OF THE SECTION TO ENTER-KEYS SECTION PR#1234 WORLD
      *   CS 170519 CHANGED PROMPT FOR PASSWORD, IF THERE IS A REAFFDATE AND
      *             THE DISCHARGE DATE IS 0, DONT VERIFY PASSWD PR#1234A
      *                                                        WORLD
      *  BAH 170824 ACTIVATE 8 DIGIT DATES, PD0006
      * BAH 181004 GLOBAL BWFILE
      *  CS 181206 GLOBAL LTCFILE
      * BAH 181212 GLOBAL LNFILE
      *  BAH 20190813 INITIALIZE THE DELINQ-PROF1 TO "X" SINCE THE NON-DISPLAYS
      *              DONT WORK ANYMORE. PROFILES WERE NOT DISPLAYING IN A30
      *              LIKE THEY WERE IN A15. QA0076
      *20191231 CS IN CHECK-FOR-REAFFIRMATIONS WAS SETTING UP SEQ 5 TO READ
      *            THE LTB, ITS 1 RECORD SO CHANGED TO SEQ1
      * BAH 20220818 REMOVED GLOBRD, READ GB-FILE INSTEAD
      * BAH 20240122 ADDED BILNSC-CALLED-FROM
      * BAH 2024.0311 REMOVED EXT-WI-CL-APP-NUMBER
      * BAH 2025.0409 PASSWORD VIEWING A VOIDED ACCOUNT #1708 S35Q-196
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
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)SP/CPINQ0.CBL S35 04/09/25-11:39:30 >".
      **********************************
      *       F I L E   I F N          *
      **********************************
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBGB/GB01GB.CPY".
       COPY "LIBGB/GB01GB_SQL.CPY".
       COPY "LIBGB/GBWSGB.CPY".
       COPY "LIBLP/LP01LC.CPY".
       COPY "LIBLP/LP01LC_SQL.CPY".
       COPY "LIBLP/LPWSLC.CPY".
       COPY "LIBLP/LP01SP.CPY".
       COPY "LIBLP/LP01SPA.CPY".
       COPY "LIBLP/LP01SPB.CPY".
       COPY "LIBLP/LP01SPC.CPY".
       COPY "LIBLP/LP01SP_SQL.CPY".
       COPY "LIBLP/LPWSSP.CPY".
       COPY "LIBLP/LP01LN.CPY".
       COPY "LIBLP/LP01LN_SQL.CPY".
       COPY "LIBLP/LPWSLN.CPY".
       COPY "LIBLP/LP01BW.CPY".
       COPY "LIBLP/LP01BW_SQL.CPY".
       COPY "LIBLP/LPWSBW.CPY".
       COPY "LIBLP/LP01BM.CPY".
       COPY "LIBLP/LP01BM_SQL.CPY".
       COPY "LIBLP/LPWSBM.CPY".
       COPY "LIBLP/LP01LTC.CPY".
       COPY "LIBLP/LP01LTC_SQL.CPY".
       COPY "LIBLP/LPWSLTC.CPY".
       COPY "LIBLP/LP01LTA.CPY".
       COPY "LIBLP/LP01LTA_SQL.CPY".
       COPY "LIBLP/LPWSLTA.CPY".
       COPY "LIBLP/LP01LTB.CPY".
       COPY "LIBLP/LP01LTB_SQL.CPY".
       COPY "LIBLP/LPWSLTB.CPY".
       COPY "LIBLP/LP01LTP.CPY".
       COPY "LIBLP/LP01LTP_SQL.CPY".
       COPY "LIBLP/LPWSLTP.CPY".
       COPY "LIBLP/LP01LTI.CPY".
       COPY "LIBLP/LP01LTI_SQL.CPY".
       COPY "LIBLP/LPWSLTI.CPY".
       COPY "LIBLP/LP01LM.CPY".
       COPY "LIBLP/LP01LM_SQL.CPY".
       COPY "LIBLP/LPWSLM.CPY".

      *********************************************************
      *   LNFILE ARCHIVE PATH WORKERS
      *********************************************************
      *01  LN-ARCHIVE-PATH.
      *    03  FILLER                 PIC X     VALUE "/".
      *    03  LN-ARCHIVE-DIR1        PIC X(3).
      *    03  FILLER                 PIC X     VALUE "/".
      *    03  LN-ARCHIVE-DIRECTORY   PIC X(4).
      *    03  FILLER                 PIC X     VALUE "/".
      *    03  LN-ARCHIVE-MACHINE     PIC XX.
      *    03  FILLER                 PIC X     VALUE "/".
      *    03  LN-ARCHIVE-DATA        PIC X(6).
      *    03  FILLER                 PIC X(10) VALUE
      *        "/LP/LNFILE".

      *********************************************************
      *   LPFILE ARCHIVE PATH WORKERS
      *********************************************************
      *01  LP-ARCHIVE-PATH.
      *    03  FILLER                 PIC X     VALUE "/".
      *    03  LP-ARCHIVE-DIR1        PIC X(3).
      *    03  FILLER                 PIC X     VALUE "/".
      *    03  LP-ARCHIVE-DIRECTORY   PIC X(4).
      *    03  FILLER                 PIC X     VALUE "/".
      *    03  LP-ARCHIVE-MACHINE     PIC XX.
      *    03  FILLER                 PIC X     VALUE "/".
      *    03  LP-ARCHIVE-DATA        PIC X(6).
      *    03  FILLER                 PIC X(10) VALUE
      *        "/GB/LPFILE".

      *********************************************************
      *   LTCFILE ARCHIVE PATH WORKERS
      *********************************************************
      *01  LTC-ARCHIVE-PATH.
      *    03  FILLER                 PIC X     VALUE "/".
      *    03  LTC-ARCHIVE-DIR1       PIC X(3).
      *    03  FILLER                 PIC X     VALUE "/".
      *    03  LTC-ARCHIVE-DIRECTORY  PIC X(4).
      *    03  FILLER                 PIC X     VALUE "/".
      *    03  LTC-ARCHIVE-MACHINE    PIC XX.
      *    03  FILLER                 PIC X     VALUE "/".
      *    03  LTC-ARCHIVE-DATA       PIC X(6).
      *    03  FILLER                 PIC X(11) VALUE
      *        "/LP/LTCFILE".

      *********************************************************
      *   LTAFILE ARCHIVE PATH WORKERS
      *********************************************************
      *01  LTA-ARCHIVE-PATH.
      *    03  FILLER                 PIC X     VALUE "/".
      *    03  LTA-ARCHIVE-DIR1       PIC X(3).
      *    03  FILLER                 PIC X     VALUE "/".
      *    03  LTA-ARCHIVE-DIRECTORY  PIC X(4).
      *    03  FILLER                 PIC X     VALUE "/".
      *    03  LTA-ARCHIVE-MACHINE    PIC XX.
      *    03  FILLER                 PIC X     VALUE "/".
      *    03  LTA-ARCHIVE-DATA       PIC X(6).
      *    03  FILLER                 PIC X(11) VALUE
      *        "/LP/LTAFILE".

      *********************************************************
      *   LTBFILE ARCHIVE PATH WORKERS
      *********************************************************
      *01  LTB-ARCHIVE-PATH.
      *    03  FILLER                 PIC X     VALUE "/".
      *    03  LTB-ARCHIVE-DIR1       PIC X(3).
      *    03  FILLER                 PIC X     VALUE "/".
      *    03  LTB-ARCHIVE-DIRECTORY  PIC X(4).
      *    03  FILLER                 PIC X     VALUE "/".
      *    03  LTB-ARCHIVE-MACHINE    PIC XX.
      *    03  FILLER                 PIC X     VALUE "/".
      *    03  LTB-ARCHIVE-DATA       PIC X(6).
      *    03  FILLER                 PIC X(11) VALUE
      *        "/LP/LTBFILE".

      *********************************************************
      *   LTPFILE ARCHIVE PATH WORKERS
      *********************************************************
      *01  LTP-ARCHIVE-PATH.
      *    03  FILLER                 PIC X     VALUE "/".
      *    03  LTP-ARCHIVE-DIR1       PIC X(3).
      *    03  FILLER                 PIC X     VALUE "/".
      *    03  LTP-ARCHIVE-DIRECTORY  PIC X(4).
      *    03  FILLER                 PIC X     VALUE "/".
      *    03  LTP-ARCHIVE-MACHINE    PIC XX.
      *    03  FILLER                 PIC X     VALUE "/".
      *    03  LTP-ARCHIVE-DATA       PIC X(6).
      *    03  FILLER                 PIC X(11) VALUE
      *        "/LP/LTPFILE".

      *********************************************************
      *   LTIFILE ARCHIVE PATH WORKERS
      *********************************************************
      *01  LTI-ARCHIVE-PATH.
      *    03  FILLER                 PIC X     VALUE "/".
      *    03  LTI-ARCHIVE-DIR1       PIC X(3).
      *    03  FILLER                 PIC X     VALUE "/".
      *    03  LTI-ARCHIVE-DIRECTORY  PIC X(4).
      *    03  FILLER                 PIC X     VALUE "/".
      *    03  LTI-ARCHIVE-MACHINE    PIC XX.
      *    03  FILLER                 PIC X     VALUE "/".
      *    03  LTI-ARCHIVE-DATA       PIC X(6).
      *    03  FILLER                 PIC X(11) VALUE
      *        "/LP/LTIFILE".

       01  IO-TYPE           PIC X(3).

       01  WSEL-LIST                   PIC X(5)    VALUE "WSEL.".

       01  SOURCE-MACHINE              PIC X(02).
       01  LSCAN-ERR                   PIC X(01) VALUE SPACE.
       01  GET-ACCOUNT-ERR             PIC X     VALUE " ".

       01  SAVE-ACCTNO                 PIC 9(8) VALUE 0.

       01  HOLD-BW-SSNO      PIC 9(11) VALUE 0.
       01  NAME-WORKER       PIC X(40) VALUE SPACES.
       01  NAME              PIC X(16) VALUE SPACES.
       01  MAKER-CODE        PIC X     VALUE SPACES.
       01  ENTER-SEQNO       PIC X     VALUE SPACES.
       01  PHONE-EDIT        PIC 9(10) VALUE 0.
       01  PHONE-WORK REDEFINES PHONE-EDIT.
           03  PE-AREA       PIC 999.
           03  PE-PHONE      PIC 9(7).
       01  SUB               PIC 9.
       01  S1                PIC 99.
       01  BK-REAFF-STAT     PIC X(2) VALUE "XX".

       01  DIS-BRANCH.
           03  FILLER         PIC X(21)  VALUE
           "A/C OWNED BY BRANCH# ".
           03  DIS-BR         PIC ZZZ9.
       01  NUM-BW            PIC 9(9).
       01  NUM-BW-WK         REDEFINES NUM-BW.
           03  NUM-BW1       PIC 999.
           03  NUM-BW2       PIC 99.
           03  NUM-BW3       PIC 9999.
       01  ALP-BW.
           03  ALP-BW1       PIC 999.
           03  ALP-HYPHEN1   PIC X.
           03  ALP-BW2       PIC 99.
           03  ALP-HYPHEN2   PIC X.
           03  ALP-BW3       PIC 9999.

       01  PROFILE-EDIT      PIC 9(12).
       01  PROFILE-ED REDEFINES PROFILE-EDIT
                             PIC X(12).

      ********************************
      *   S C R E E N   B U F F E R  *
      ********************************

       01  WORLD-CREDIT-BUF.
           03  WORLD-HI-CREDIT-PROMPT     PIC X(8).
           03  WORLD-HI-CREDIT-AMT        PIC Z(6)B.
           03  WORLD-WCBC-CREDIT-PROMPT   PIC X(5).
           03  WORLD-WCBC-CREDIT-AMT      PIC Z(6)B.
           03  WORLD-AVAIL-CREDIT-PROMPT  PIC X(6).
           03  WORLD-AVAIL-CREDIT-AMT     PIC Z(5)9.99.
           03  WORLD-AVAIL-FLAG1          PIC X.
           03  WORLD-AVAIL-FLAG2          PIC X.

       01  WORLD-CREDIT-LIST.
           03  FILLER          PIC X(24) VALUE
               "H22 H23 HFILLER H24 H25.".
       01  WORLD-CREDIT-LIST-ATTR.
           03  FILLER          PIC X(34) VALUE
               "H22:R H23:R HFILLER:R H24:R H25:R.".
       01  WORLD-REAFFIRM-LIST.
           03  FILLER          PIC X(39) VALUE
               "H22:RF H23:RF HFILLER:RF H24:RF H25:RF.".


       01  DISPLAY-BORR-BUF  VALUE " ".
           03  D-SOC         PIC X(11).
           03  D-SOC-X-OUT REDEFINES D-SOC.
               05  D-SOC1    PIC X(3).
               05  FILLER    PIC X.
               05  D-SOC2    PIC XX.
               05  FILLER    PIC X.
               05  D-SOC3    PIC X(4).
           03  D-LOANS       PIC ZZ.
           03  D-X-NAME.
               05  D-NAME    PIC X(34) OCCURS 4.
           03  D-Y-NAME REDEFINES D-X-NAME.
               05  FILLER    OCCURS 4.
                   07  D-NAME-WORLD PIC X(25).
                   07  FILLER       PIC X.
                   07  D-REP-WORLD  PIC X(3).
                   07  FILLER       PIC X(5).
           03  D-SP0         PIC X(7).
           03  D-SP1         PIC X(11).
           03  D-SP1-X-OUT REDEFINES D-SP1.
               05  D-SP1-1   PIC X(3).
               05  FILLER    PIC X.
               05  D-SP1-2   PIC XX.
               05  FILLER    PIC X(5).
           03  D-SCOREPROMPT PIC X(6).
           03  D-CREDIT-SCORE PIC ZZZZ.
           03  D-SP2         PIC X(25).
           03  D-MEMO        PIC X(25).

       01  DISPLAY-LOAN-BUF  VALUE " ".
           03  D-LOAN-NUM    PIC X(16).
           03  D-MAKER       PIC X(8).
           03  D-H1          PIC 99/99/99  BLANK ZERO.
           03  D-H2          PIC X(8).
           03  D-H3          PIC 99/99/99  BLANK ZERO.
           03  D-H4          PIC ZZZZ9.99  BLANK ZERO.
           03  D-H5-PMPT.
               05  FILLER    PIC X(3).
               05  D-H5-P1   PIC X.
               05  D-H5-P2   PIC 999.
               05  D-H5-P3   PIC X.
           03  D-H5          PIC 999/9999.
           03  D-H5-RED REDEFINES D-H5.
               05  FILLER    PIC XXX.
               05  D-H5A     PIC X.
               05  FILLER    PIC X(4).
           03  D-H6          PIC ZZZZZ9.99.
           03  D-H7          PIC ZZZZ9.99.
           03  D-H71         PIC X.
           03  D-H8          PIC ZZZZ9.99-.
           03  D-H9          PIC ZZZZ9.99-.
           03  D-B1          PIC X(8).
           03  D-H10         PIC ZZZZ9.99-.
           03  D-PH11        PIC X(4).
           03  D-H11A.
               05  D-H11     PIC ZZZ9.
           03  D-H12         PIC X(12).
           03  D-PH13        PIC X(4).
           03  D-H13A.
               05  D-H13     PIC ZZZ9.
           03  D-H14         PIC X(12).
           03  D-H15         PIC ZZZ.ZZ.
           03  D-H16         PIC ZZZ.
           03  D-H17-MO      PIC 99.
           03  D-H17-DAYR    PIC 99.
           03  D-H18         PIC 99        BLANK ZERO.
           03  D-H181        PIC 99        BLANK ZERO.
           03  D-H19         PIC ZZ9.999.
           03  D-H20         PIC X(4).
           03  D-COLLCD      PIC XX.
           03  D-COLLID      PIC X(20).
           03  D-PROMDT1     PIC 99/99/99  BLANK ZERO.
           03  D-PROMDT2     PIC 99/99/99  BLANK ZERO.
           03  D-DLRNUM      PIC 9(4).99.
           03  D-PROMAMT     PIC Z(5).ZZ-.
           03  D-RCSTAG      PIC X(3).
           03  D-RCOURS      PIC X(8).
           03  D-NRCOURS     REDEFINES D-RCOURS PIC ZZZZ9.99.
           03  D-DELCNT      PIC ZZ.
           03  D-COLCTID     PIC XXX.

       01  DISPLAY-BAL-BUF   VALUE " ".
           03  FILLER        PIC X(31).
           03  D-PDTH-MO     PIC 99.
           03  D-PDTH-DAYR   PIC 99.
           03  D-OTHBAL      PIC Z(5).99-.
           03  D-INTBAL      PIC Z(4).99-.
           03  D-CURBAL      PIC Z(5)9.99.

       01  PMT-PROMPT-LIST   PIC X(12) VALUE "PMTPROMPT:D.".
       01  ACH-PROMPT-LIST   PIC X(12) VALUE "PMTPROMPT:R.".
       01  PMT-PROMPT-BUF    PIC X(7)  VALUE "PAYMENT".
       01  ACH-PROMPT-BUF    PIC X(7)  VALUE "PAY ACH".

       01  D-MESSAGE         PIC X(30).
       01  MESSAGE-LIST      PIC X(8)  VALUE "MESSAGE.".
       01  MESSAGE-LIST-ATTR PIC X(11) VALUE "MESSAGE:R.".
       01  MESSAGE-ATTR      PIC XX.

       01  DISPLAY-BORR-LIST.
           03  FILLER        PIC X(58) VALUE
           "SOC LOANS A1 A2 A3 A4 SP0 SP1 SCORE:S CREDITSCORE SP2 P01.".

       01  DISP-DELINQ-VALUE.
           03 DISP-DELINQ-VALUE-1  PIC X(12) VALUE SPACES.
           03 DISP-DELINQ-VALUE-2  PIC X(12) VALUE SPACES.

       01  DISPLAY-LOAN-LIST.
           03  FILLER        PIC X(55) VALUE
           "LNNO LNSEQ MAKER H01 H02 H03 H04 H05A H05 H06 H07 H071 ".
           03  FILLER        PIC X(51) VALUE
           "H08 H09 B1:S H10 PH11:S H11 H12 PH13:S H13 H14 H15 ".
           03  FILLER        PIC X(21) VALUE
           "H16 H17 H18 H181 H19 ".
           03  FILLER        PIC X(41) VALUE
           "H20 COLLCD COLLID PROMDT1 PROMDT2 DLRNUM ".
           03  FILLER        PIC X(37)  VALUE
           "PROMAMT RCSTAG RCOURS DELCNT COLCTID.".

      *-----------------------------------------------------------------
      * DO NOT CHANGE THE SIZE OF THIS DISPLAY FIELD. ITS ATRRIBUTES ARE
      * CHANGED ON THE FLY BASED UP GP/LC FLAGS.
      * IF GP-I-AM-WORLD
      *    LC-PROF1   LC-PROF2    DISPLAY-DELINQ-LIST
      *       C           R       "H12 H14.    "   (DEFAULT)
      *       D           R       "H12:R H14.  "
      *       C           D       "H12 H14:R.  "
      *       D           D       "H12:R H14:R."
      *-----------------------------------------------------------------
       01  DISPLAY-DELINQ-LIST.
           03  FILLER             PIC X(12) VALUE
               "H12 H14.    ".

       01  DISPLAY-BAL-LIST.
           03  FILLER        PIC X(9) VALUE
           "MEMODESC.".

       01  LINE-LIST.
           03  FILLER          PIC X(32)    VALUE
           "L01 L02 L03 L04 L05 L06 L07 L08.".

       COPY "LIBLP/LPTRCD_CMD.CPY".
       01  HEAD-1.
           03  FILLER        PIC X(40) VALUE
           "ACCOUNT #         DATE  LOAN AMT TRM  DE".
           03  FILLER        PIC X(16) VALUE
           "ALER COL PL/JD  ".
           03  DELINQ-PROMPT PIC X(11).
           03  FILLER        PIC X(13) VALUE
           "  BAL/PDOFF M".

       01  DISPLAY-TRACKING.
           03  FILLER           PIC X(10) VALUE "TRK ONLY: ".
           03  D-TRACK          PIC X.
           03  FILLER           PIC X(6) VALUE " INS: ".
           03  D-INS            PIC X.
           03  FILLER           PIC X(6) VALUE " CPP: ".
           03  D-CPP            PIC X.


       01  DELINQ-SETTING          PIC X(2)   VALUE "LC".

       01  DELINQ-SCAN             VALUE "X  ".
           03  DELINQ-PROF1        PIC X.
           03  DELINQ-PROF2        PIC X.
           03  DELINQ-PROF3        PIC X.

       01  DELINQ-VALUE.
           03  DELINQ-ALP.
               05  DELINQ-NUM             PIC ZZZZ,ZZ9.99- BLANK ZERO.
       01  DELINQ-FORM.
           03  FILLER                     PIC X(3) VALUE SPACES.
           03  DELINQ-CODES               PIC X(3) VALUE SPACES.
       01  DELINQ-CODE                    PIC X.
       01  PBD-FLAG                       PIC X.
       01  HOLD-BK-MEMO.
           03  HOLD-BK-TEXT        PIC X(30).
           03  HOLD-POST-USERID    PIC X(10).
           03  HOLD-POST-TIME      PIC X(8).

       01  WORK-ERRLOG-DETAIL.
           03  WORK-ERRLOG-BR      PIC 9(4).
           03  FILLER              PIC X.
           03  WORK-ERRLOG-NUMBER  PIC 9(6).
           03  FILLER              PIC X.
           03  WORK-ERRLOG-DATE    PIC 99/99/9999.
           03  FILLER              PIC X.
           03  WORK-ERRLOG-BK-MSG.
               05  WORK-EL-BK-TEXT PIC X(30).

       01  SYSDATE-YYYYMMDD        PIC 9(8).

       COPY "LIBWI/BWLNSCW.CPY".
       COPY "LIBGB/SYSTEMW.CPY".
       COPY "LIBWI/BILNSCW.CPY".
       COPY "LIBCL/CLMENUW_EXT.CPY".
       COPY "LIBWI/PFSWINDOWSW.CPY".
       COPY "LIBGB/REBATEW.CPY".
       COPY "LIBGB/GETENVW.CPY". 
       COPY "LIBLP/LPPOFFW.CPY".
       COPY "LIBLP/ADDONSPRW.CPY".
       COPY "LIBLP/LPAVCRW.CPY".
       COPY "LIBLP/LPLCHGW.CPY".
       COPY "LIBLP/LPMDTEW.CPY".
       COPY "LIBLP/LPEARNW.CPY".
       COPY "LIBLP/LPXDTEW.CPY".
       COPY "LIBGB/AGEINGW.CPY".
       COPY "LIBLP/LPUPWKW.CPY".
       COPY "LIBLP/LPCEPPW.CPY".
       COPY "LIBGB/SPPDTHW.CPY".
       COPY "LIBGB/RBACTW.CPY".

       COPY "LIBLP/LPCDELW.CPY".
       COPY "LIBLP/LPLCAPW.CPY".
       COPY "LIBLP/LPLCASW.CPY".
       COPY "LIBLP/LPIBPCW.CPY".
       COPY "LIBLP/LPRATEW.CPY".
       COPY "LIBLP/LPSTATW.CPY".

       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/GPENVW.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBLP/SETCTBLW.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBLP/LPSCHDW.CPY".
       COPY "LIBLP/LPITRMW.CPY".
       COPY "LIBLP/LPAMTSW.CPY".
       COPY "LIBLP/LPAPRSW.CPY".
       COPY "LIBLP/LPAPRZW.CPY".
       COPY "LIBLP/LPUPERW.CPY".
       COPY "LIBLP/LPPDUEW.CPY".
       COPY "LIBLP/POSTDATEW.CPY".
       COPY "LIBGB/PASSWDW.CPY".
       78  GLOBAL-SCAN   VALUE 0.
       78  BK-SEQ        VALUE 1.
       78  DEBTAID-SEQ   VALUE 2.
       78  BK-WORLD-SEQ  VALUE 3.
       78  VIEW-VOID     VALUE 4.
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBSP/CPINQ_DEF.CPY".
       COPY "LIBWI/INBRT2_DEF.CPY".
       COPY "LIBWI/WRLDBK_DEF.CPY".
       COPY "LIBSP/CPINQ_WKS.CPY".
       COPY "LIBWI/INBRT2_WKS.CPY".
       COPY "LIBWI/WRLDBK_WKS.CPY".

       COPY "LIBGB/HELPLINK.CPY".

       01  FORM-LINK-AREA.
           03  CPINQ-PROG-BUF      PIC X(356).
           03  SOC-BUF             PIC X(11)    VALUE SPACES.
           03  INTERRUPT-FG        PIC X.
           03  AVAILABLE-BUF       PIC X(12).
           03  ARCHIVE-FG          PIC X.
           03  FLEX-INQ-FORM       PIC 9(3).
           03  FLEX-INQ-LINES      PIC 9(2).
           03  CPINQ-WINDOW-HANDLE PIC X(10).
           03  HOLD-EXT-FILPATH    PIC X(19).

       COPY "LIBGB/GETFMW.CPY".
       01  LOAN-PROC-FG  PIC X.
       01  INTERRUPT-CD  PIC X.
       COPY "LIBLP/LPLOANW.CPY".

       SCREEN SECTION.
       COPY "LIBSP/CPINQ_SCN.CPY".
       COPY "LIBWI/INBRT2_SCN.CPY".
       COPY "LIBWI/WRLDBK_SCN.CPY".

       PROCEDURE DIVISION
            USING FORM-PATHNAME LOAN-PROC-FG
                                INTERRUPT-CD
                                PROG-BUF
                                EXIT-PATHNAME.

      *********************************
       MAIN-PROGRAM SECTION.
      *********************************

           PERFORM SQL-CONNECT.

      D    STOP MESS.
           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "CPINQ" TO VDU-FORM-NAME.

      * CODE FROM CPINQ.CBL
           MOVE EXT-FILPATH         TO HOLD-EXT-FILPATH.

           DISPLAY FLOATING GRAPHICAL WINDOW
                   AT SCREEN LINE 01 * VDU-WIN-L-OFFSET
                   AT SCREEN COL 01 * VDU-WIN-C-OFFSET
                   CELL SIZE IS LABEL FONT
           COLOR IS VDU-WCOLOR-TCLP,CONTROL VALUE IS VDU-WCTRL-TCLP
                   SIZE 80 + VDU-WIN-W-MOD
                   LINES 24 + VDU-WIN-H-MOD
                   BLANK SCREEN
                   HANDLE IS CPINQ-WINDOW-HANDLE.

      * NOT SURE IF THIS IS CORRECT!!
           MOVE EXT-CONAME-USER-LOC TO BRANCH.
           MOVE 0                   TO LOAN-KEY.

           PERFORM GET-GPENV.

           MOVE BRANCH TO BR-NO.
           PERFORM OPEN-BR-FILE.
           PERFORM READ-BR-FILE.
           PERFORM CLOSE-BR-FILE.
           IF IO-FG NOT = 0
              GO TO IO-ERROR.

      * NEED FOR LCAS ROUTINES (REPLACES GLOBRD)
           PERFORM OPEN-GB-FILE.
           MOVE GB-PATH-OWNBR TO GB-BRNO.
           MOVE 1             TO GB-NO.
           PERFORM READ-GB-FILE.
           PERFORM CLOSE-GB-FILE.
           IF IO-BAD
              GO TO IO-ERROR.

      * MUST LEAVE SP1-FILE OPEN BECAUSE ADDONSPR READS IT
           PERFORM OPEN-SP1-FILE.

       MAIN-MODULE.

           PERFORM FORM-RESET.

           MOVE SPACES TO ERRCD LN-REC.

           MOVE "S  A000TRDATE." TO VDU.
           MOVE EXT-CONAME-SYSDATE TO VDUBUF ALP-DATE.
           PERFORM CONVERT-ALPDATE-TO-YYYYMMDD.
           MOVE DATE-YYYYMMDD TO TRANS-DATE SYSDATE-YYYYMMDD.
           PERFORM SCREENIO.

           PERFORM SEND-DELINQ-SCAN.

           MOVE "S  A220TITLE:FR." TO VDU.
           MOVE "    ACCOUNT INQUIRY" TO VDUBUF.
           PERFORM SCREENIO.

           MOVE " " TO VDU-WIMENU-SELECTION.

           PERFORM ENTER-KEYS.
           IF IO-TYPE = "END"
              GO TO END-ROUTINE.
           IF IO-TYPE NOT = "PST"
              GO TO MAIN-MODULE.

           PERFORM GET-LC.
           IF IO-FG = 9
              MOVE "INVALID CLASS" TO LC-DESC.

           MOVE "S  A010TYP." TO VDU.
           PERFORM SCREENIO.

           MOVE "S  A160SORTKEY." TO VDU.
           MOVE LC-DESC TO VDUBUF.
           PERFORM SCREENIO.

           MOVE SPACES      TO WR-BUF.
           MOVE "MEMODESC." TO WR-LIST.
           PERFORM CALL-WRFORM.

           PERFORM SET-COLLECTOR-INFO.

           MOVE LN-ACCTNO   TO LOAN-KEY
           MOVE BW-SSNO     TO BORROWER-KEY.
           MOVE PROG-BUF    TO CPINQ-PROG-BUF.
           MOVE "SP/CPINQ1" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH
                                 FORM-LINK-AREA
                                 EXIT-PATHNAME.
           CANCEL FORM-PROGX.

           MOVE CPINQ-PROG-BUF TO PROG-BUF.
           IF ERRCD = "E" OR "X"
              GO TO END-ROUTINE.
              
           GO TO MAIN-MODULE.

      ************************************
       ENTER-KEYS SECTION.
      ************************************
       ENTER-KEYS-NO.

           PERFORM RESET-PATH.

           MOVE "S  A220TITLE:FR." TO VDU.
           MOVE "    ACCOUNT INQUIRY" TO VDUBUF.
           PERFORM SCREENIO.

           PERFORM SEND-HEAD-1.

           MOVE "Y"    TO ENTER-SEQNO.
           MOVE SPACES TO IO-TYPE LN-REC.

           MOVE "S  A000MSEL." TO VDU.
           MOVE "F7,F3-CODE, F4-GLOBAL, F5-DLNQ, F6-SCAN, LOAN#:"
                               TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "EN N140LNNO." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "00"
              IF LOAN-KEY NOT = 0
                 MOVE LOAN-KEY TO SAVE-ACCTNO
                 IF VDUNUM0 = 0
                    MOVE "SN N140LNNO." TO VDU
                    MOVE SAVE-ACCTNO TO VDUNUM0
                    PERFORM SCREENIO.

           IF VDUSTATUS = "16"
              PERFORM TRCD-DISPLAY
              GO TO ENTER-KEYS-NO.

           IF VDUSTATUS = "18"
              PERFORM GET-ACCOUNT
              IF GET-ACCOUNT-ERR = "Y"
                 GO TO ENTER-KEYS-NO
              ELSE
                GO TO READ-LOAN-FILE.

           IF VDUSTATUS = "1:"
              PERFORM GET-DELINQ-CODES
              GO TO ENTER-KEYS-NO.

           IF VDUSTATUS = "1<"
              PERFORM BWLN-SCAN
              IF IO-FG NOT = 0
                 GO TO ENTER-KEYS-NO
              ELSE
                 GO TO READ-LOAN-FILE.

           IF VDUSTATUS = "1>"
              MOVE "END" TO IO-TYPE
              MOVE "E" TO ERRCD
              GO TO END-ENTER-KEYS.

           IF VDUSTATUS NOT = "00"
              GO TO ENTER-KEYS-NO.

           IF VDUNUM0 = 0
              GO TO ENTER-KEYS-NO.

           MOVE VDUNUM0 TO LN-ACCTNO.
       READ-LOAN-FILE.
           PERFORM OPEN-LN1-FILE.
           PERFORM READ-LN1-FILE.
           PERFORM CLOSE-LN1-FILE.
           IF IO-FG = 9
              MOVE "INVALID ACCOUNT NUMBER" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-KEYS-NO.

           IF ( LN-POCD-VOID )
              MOVE "ACCOUNT HAS BEEN DELETED" TO MESS
              PERFORM SEND-MESS
              MOVE "VOIDED ACCOUNT" TO PASSWDW-PROMPT2
              MOVE VIEW-VOID        TO PASSWDW-SEQ
              PERFORM PASSWORD-MUST-EXIST
              IF PASSWDW-STAT NOT = "OK"
                 GO TO ENTER-KEYS-NO.

           IF ( LN-POCD-INRESCISSION )
              MOVE "ACCOUNT IS IN RESCISSION" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-KEYS-NO.

           IF ( LN-POCD-RESCIND )
              MOVE "ACCOUNT HAS BEEN RESCINDED" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-KEYS-NO.

           IF ( LN-POCD-PB-RESC )
              MOVE "ACCOUNT IS IN RESCISSION RENEWAL STATUS" TO MESS
              PERFORM SEND-MESS.

           IF BORROWER-KEY NOT NUMERIC
              MOVE 0 TO BORROWER-KEY
            ELSE
              IF BORROWER-KEY NOT = LN-SSNO(1)
                           AND NOT = LN-SSNO(2)
                            AND NOT = LN-SSNO(3)
                             AND NOT = LN-SSNO(4)
                 MOVE 0 TO BORROWER-KEY.
           IF BORROWER-KEY NOT = 0
              MOVE BORROWER-KEY TO BW-SSNO
              MOVE 0 TO BORROWER-KEY
           ELSE
              MOVE LN-SSNO(1) TO BW-SSNO. 

           PERFORM GET-BW.

           MOVE "PST" TO IO-TYPE.
           PERFORM DISPLAY-BORROWER.

           PERFORM DISPLAY-BORROWER.

            IF LN-BNKRPTDATE NOT = 0
               PERFORM CHECK-FOR-REAFFIRMATION
               IF BK-REAFF-STAT  NOT = "00"
                  MOVE BK-WORLD-SEQ TO PASSWDW-SEQ
                  PERFORM VERIFY-PASSWD-BK-WORLD
                  IF PASSWDW-STAT NOT = "OK"
                     PERFORM OPEN-BK-MSG-WINDOW
                     MOVE "E  A030SEL." TO VDU
                     PERFORM SCREENIO
                     PERFORM CLOSE-WINDOW
                     MOVE "CPINQ" TO VDU-FORM-NAME
                     IF VDUBUF NOT = "YES"
                        MOVE " " TO DISPLAY-BORR-BUF
                        MOVE DISPLAY-BORR-BUF TO WR-BUF
                        MOVE DISPLAY-BORR-LIST TO WR-LIST
                        PERFORM CALL-WRFORM
                        GO TO ENTER-KEYS-NO
                       ELSE
                          PERFORM CREATE-BK-ACCESSED-MEMO
                          PERFORM CREATE-BK-LOG.

             IF LN-DEBTAIDATE NOT = ZEROS
                MOVE "DEBT AID" TO PASSWDW-PROMPT2
                MOVE DEBTAID-SEQ  TO PASSWDW-SEQ
                PERFORM VERIFY-PASSWORD
                IF PASSWDW-STAT NOT = "OK"
                   MOVE " " TO DISPLAY-BORR-BUF WR-BUF
                   MOVE DISPLAY-BORR-LIST TO WR-LIST
                   PERFORM CALL-WRFORM
                   GO TO ENTER-KEYS-NO.

           PERFORM DISPLAY-LOAN.
           IF LN-OWNBR NOT = BRANCH
              MOVE LN-OWNBR TO DIS-BR
              MOVE DIS-BRANCH TO MESS
              PERFORM SEND-MESS.

           MOVE LN-ACCTNO TO SAVE-ACCTNO.
       END-ENTER-KEYS.
           EXIT.

      ******************************************
       OPEN-BK-MSG-WINDOW SECTION.
      ******************************************
           MOVE 20 TO WINDOW-LINES.
           MOVE 75 TO WINDOW-COLUMNS.
           MOVE 2  TO WINDOW-BEGIN-Y.
           MOVE 2  TO WINDOW-BEGIN-X.
           MOVE "WRLDBK" TO VDU-FORM-NAME.
           MOVE "WI/WRLDBK" TO WINDOW-FORM-NAM.
           PERFORM OPEN-WINDOW.
           DISPLAY WRLDBK-SCREEN.
       OPEN-BK-MSG-WINDOW-EXIT.
           EXIT.

      ******************************************
       CREATE-BK-ACCESSED-MEMO SECTION.
      ******************************************
           MOVE SPACES TO HOLD-BK-MEMO.
           PERFORM OPEN-LM1-FILE.
           MOVE "BK ACCOUNT ACCESSED" TO HOLD-BK-TEXT.
           PERFORM SET-POSTDATE.
           MOVE POST-USERID            TO HOLD-POST-USERID.
           PERFORM GET-TIME.
           MOVE TIME-EDIT              TO HOLD-POST-TIME.
           MOVE LN-OWNBR               TO LM-BRNO.
           MOVE LN-ACCTNO              TO LM-ACCTNO.
           SUBTRACT TRANS-DATE FROM 999999999 GIVING LM-DATE.

           MOVE HOLD-BK-MEMO TO LM-MEMO 
           MOVE 9            TO LM-MEMO-TYPE.
           MOVE 1 TO LM-SEQ.
       CREATE-BK-MEMO-AGAIN.
           PERFORM WRITE-LM1-FILE.
           IF IO-BAD
              IF LM-SEQ NOT = 999
                 ADD 1 TO LM-SEQ
                 GO TO CREATE-BK-MEMO-AGAIN.     
           PERFORM CLOSE-LM1-FILE.

       CREATE-BK-ASSESS-MEMO-EXIT.
           EXIT.

      ******************************************
       CREATE-BK-LOG SECTION.
      ******************************************
           MOVE "B" TO ERRLOGW-ACTION.
           MOVE LN-OWNBR     TO   ERRLOGW-BRNO.
           MOVE LN-ACCTNO    TO WORK-ERRLOG-NUMBER.
           MOVE HOLD-BK-TEXT TO  WORK-EL-BK-TEXT.
           MOVE TRANS-DATE   TO  DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYYYY.
           MOVE DATE-MMDDYYYY  TO  WORK-ERRLOG-DATE.
           MOVE WORK-ERRLOG-DETAIL   TO ERRLOGW-EOM-MSG.
           MOVE FORM-PATHNAME TO ERRLOGW-PROGX.
           MOVE "CL/ERRLOG" TO FORM-NAM.
           CALL FORM-PROGX  USING FORM-PATH ERRLOGW-AREA.
           CANCEL FORM-PATH.
       CREATE-BK-LOG-EXIT.
           EXIT.

      ******************************************
       SET-COLLECTOR-INFO SECTION.
      ******************************************

      * ALLOW FOR F9 COLLECTOR MENU

           MOVE SPACES        TO EXT-WI-CL-WK-PATH.
           MOVE TRANS-DATE    TO EXT-WI-CL-TRANS-DATE.
           MOVE LN-OWNBR      TO EXT-WI-CL-OWNBR.
           MOVE LN-ACCTNO     TO EXT-WI-CL-ACCTNO.
      *    MOVE LN-APP-NUMBER TO EXT-WI-CL-APP-NUMBER.
           MOVE LN-SSNO(1)    TO EXT-WI-CL-SSNO(1).
           MOVE LN-SSNO(2)    TO EXT-WI-CL-SSNO(2).
           MOVE LN-SSNO(3)    TO EXT-WI-CL-SSNO(3).
           MOVE LN-SSNO(4)    TO EXT-WI-CL-SSNO(4).
           MOVE LN-MAKERCD(1) TO EXT-WI-CL-MAKERCD(1).
           MOVE LN-MAKERCD(2) TO EXT-WI-CL-MAKERCD(2).
           MOVE LN-MAKERCD(3) TO EXT-WI-CL-MAKERCD(3).
           MOVE LN-MAKERCD(4) TO EXT-WI-CL-MAKERCD(4).
      * SET OF (WI-CL-SSNO-NO) DO DONE IN 'DISPLAY-LOAN':
           MOVE LN-DLNO                   TO EXT-WI-CL-DLNO.
           MOVE LN-SALEFIN                TO EXT-WI-CL-SALEFIN.
           MOVE LN-PAY-SCHLD-FG           TO EXT-WI-CL-PAY-SCHLD-FG.
           MOVE ARCHIVE-FG                TO EXT-WI-CL-ARCHIVE-FG.
           MOVE EXT-CONAME-SYSDATE        TO EXT-WI-CL-SYSDATE.
           MOVE EXT-CONAME-DAILY-PASSWORD TO EXT-WI-CL-PASSWD.

      ***************************************
       BWLN-SCAN SECTION.
      ***************************************
           MOVE TRANS-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-ALPDATE.
           MOVE 0        TO BWLNSC-ACCTNO.
           MOVE ALP-DATE TO BWLNSC-SYSDATE.

           MOVE "WI/BWLNSC " TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH BWLNSC-BUF EXIT-PATHNAME.
           CANCEL FORM-PROGX.

           IF BWLNSC-STATUS NOT = "00" OR BWLNSC-ACCTNO = 0
              MOVE 9 TO IO-FG
              GO TO EXIT-BWLN-SCAN.
           MOVE BWLNSC-ACCTNO TO LN-ACCTNO.
           MOVE BWLNSC-SSNO   TO BW-SSNO BORROWER-KEY.
           MOVE 0 TO IO-FG.

       EXIT-BWLN-SCAN.
           EXIT.


      *****************************************
       DISPLAY-BORROWER SECTION.
      *****************************************

           MOVE SPACES TO DISPLAY-BORR-BUF.

           MOVE BW-SSNO TO NUM-BW.
           PERFORM NUMBW-TO-ALPBW.
           MOVE ALP-BW TO D-SOC.
           IF GP-HIDE-FIRST-5-SSNO = "Y"
              MOVE ALL "X" TO D-SOC1 D-SOC2.

           MOVE BW-LNAME TO NAME-WORKER.
           MOVE BW-FNAME TO NAME.
           INSPECT NAME-WORKER REPLACING FIRST "  " BY ",?".
           INSPECT NAME-WORKER REPLACING FIRST "                "
                         BY NAME.
           IF BW-MNAME NOT = SPACES
              STRING NAME-WORKER," ",BW-MNAME DELIMITED BY "  " INTO
                 NAME-WORKER.
           INSPECT NAME-WORKER REPLACING ALL "?" BY " ".
           MOVE NAME-WORKER TO D-NAME-WORLD(1)
           MOVE BW-REPCODE  TO D-REP-WORLD(1).
           MOVE BW-ADR1     TO D-NAME(2).
           IF BW-ADR2 NOT = SPACES
              MOVE BW-ADR2  TO D-NAME(3).
           MOVE " "      TO NAME-WORKER.
           MOVE BW-CITY  TO NAME-WORKER.
           MOVE BW-STATE TO NAME.
           INSPECT NAME-WORKER REPLACING FIRST "   " BY ", *".
           INSPECT NAME-WORKER REPLACING FIRST "*               "
                         BY NAME.
           INSPECT NAME-WORKER REPLACING FIRST "  " BY " *".
           MOVE BW-ZIP TO NAME.
           INSPECT NAME-WORKER REPLACING FIRST "*               "
                         BY NAME.
           IF BW-ADR2 = SPACES
              MOVE NAME-WORKER TO D-NAME(3)
           ELSE
              MOVE NAME-WORKER TO D-NAME(4).
           MOVE BW-LOANS       TO D-LOANS.

           MOVE 1 TO SUB.
       DISPLAY-JOINT-OR-ENDORSER.
           ADD 1 TO SUB.
           IF SUB > 4 OR LN-MAKERCD(SUB) = SPACES
              GO TO CONTINUE-DISPLAY-BORROWER.

      * LOOKING FOR JOINT MAKER ANYWHERE, OTHERWISE, DISPLAY ENDORSER, IF
      * NO JOINT MAKER
           IF LN-MAKERCD(SUB) = "J"
              GO TO CONTINUE-DISPLAY-BORROWER
           ELSE
              IF LN-MAKERCD(SUB) = "E"
                 IF (LN-MAKERCD(2) NOT = "J" AND
                      LN-MAKERCD(3) NOT = "J" AND
                       LN-MAKERCD(4) NOT = "J" )
                          GO TO CONTINUE-DISPLAY-BORROWER
                 ELSE
                    GO TO DISPLAY-JOINT-OR-ENDORSER.

       CONTINUE-DISPLAY-BORROWER.
      * TEST THAT YOU DONT HAVE ACCOUNT PULLED UP BY SECOND MAKER !!
           IF LN-SSNO(SUB) NOT = 0 AND (LN-SSNO(SUB) NOT = BW-SSNO)
      * SAVE ORIGINAL BORROWER, CANT ASSUME ITS LN-SSNO(1)
              MOVE BW-SSNO TO HOLD-BW-SSNO
              MOVE LN-SSNO(SUB) TO BW-SSNO NUM-BW
              PERFORM GET-BW
              IF IO-FG = 0
                 IF LN-MAKERCD(SUB) = "J"
                    MOVE "JOINT :" TO D-SP0
                 ELSE
                    MOVE "ENDOR :" TO D-SP0
                 END-IF
                 PERFORM NUMBW-TO-ALPBW
                 MOVE ALP-BW TO D-SP1
                 IF GP-HIDE-FIRST-5-SSNO = "Y"
                    MOVE ALL "X" TO D-SP1-1 D-SP1-2
                 END-IF
                 MOVE BW-LNAME TO NAME-WORKER
                 MOVE BW-FNAME TO NAME
                 INSPECT NAME-WORKER REPLACING FIRST "  " BY ",?"
                 INSPECT NAME-WORKER REPLACING FIRST "                "
                                                         BY NAME
                 INSPECT NAME-WORKER REPLACING ALL "?" BY " "
                 MOVE NAME-WORKER TO D-SP2
      * GET ORIGINAL BORROWER BACK IN THE BUFFER
                 MOVE HOLD-BW-SSNO TO BW-SSNO
                 PERFORM GET-BW
                 GO TO END-DISPLAY-BORR
              ELSE
                 MOVE HOLD-BW-SSNO TO BW-SSNO
                 PERFORM GET-BW
                 GO TO END-DISPLAY-BORR.

           IF BW-SPSSNO = 0
              IF BW-SPLNAME = SPACES
                 IF BW-SPFNAME = SPACES
                    GO TO END-DISPLAY-BORR.

           MOVE "SPOUSE:" TO D-SP0.
           IF BW-SPSSNO NOT = 0
              MOVE BW-SPSSNO TO NUM-BW
              PERFORM NUMBW-TO-ALPBW
              MOVE ALP-BW TO D-SP1
              IF GP-HIDE-FIRST-5-SSNO = "Y"
                 MOVE ALL "X" TO D-SP1-1 D-SP1-2.
           MOVE BW-SPLNAME TO NAME-WORKER.
           MOVE BW-SPFNAME TO NAME.
           INSPECT NAME-WORKER REPLACING FIRST "  " BY ",?".
           INSPECT NAME-WORKER REPLACING FIRST "                "
             BY NAME.
           INSPECT NAME-WORKER REPLACING ALL "?" BY " ".
           MOVE NAME-WORKER TO D-SP2.
       END-DISPLAY-BORR.
      * BRFILE IS READ IN MAIN-ROUTINE
           IF LN-POCD-PB-RESC
              MOVE "(( RESCISSION RENEWAL ))" TO D-MEMO
           ELSE
              IF BR-PRIO-MEMO-FLAG = "Y"
                 PERFORM GET-PRIORITY-MEMO
              ELSE
                 IF BR-PRIO-MEMO-FLAG = "I"
                    PERFORM DISPLAY-INSURANCE-TRACKING.


           IF BW-CREDIT-SCORE NUMERIC
              AND BW-CREDIT-SCORE > 0
                 MOVE "SCORE:" TO D-SCOREPROMPT
                 MOVE BW-CREDIT-SCORE TO D-CREDIT-SCORE
           ELSE
              MOVE SPACES TO D-SCOREPROMPT
              MOVE 0 TO D-CREDIT-SCORE.

           MOVE DISPLAY-BORR-BUF TO WR-BUF.
           MOVE DISPLAY-BORR-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      *****************************************
       GET-PRIORITY-MEMO SECTION.
      *****************************************

           MOVE BW-SSNO TO BM-SSNO.
           MOVE 0 TO BM-DATE.
           MOVE 1 TO BM-SEQ.
           PERFORM OPEN-BM1-FILE.
           PERFORM READ-BM1-FILE.
           IF IO-FG = 0
              MOVE BM-MEMO TO D-MEMO.
           PERFORM CLOSE-BM1-FILE.

      *****************************************
       DISPLAY-LOAN SECTION.
      *****************************************

           MOVE " " TO DISPLAY-LOAN-BUF.
           PERFORM GET-SP.
           MOVE LN-1STPYDATE TO PDTH-DATE-WORK.
           PERFORM PAID-THRU-CALCULATION.

           PERFORM LPSTAT-DISPLAY.
           MOVE LPS-DISPLAY-LNNO TO D-LOAN-NUM.
           IF LN-SSNO(1) = BW-SSNO
              MOVE LN-MAKERCD(1) TO MAKER-CODE
              MOVE 1 TO EXT-WI-CL-SSNO-NO.
           IF LN-SSNO(2) = BW-SSNO
              MOVE LN-MAKERCD(2) TO MAKER-CODE
              MOVE 2 TO EXT-WI-CL-SSNO-NO.
           IF LN-SSNO(3) = BW-SSNO
              MOVE LN-MAKERCD(3) TO MAKER-CODE
              MOVE 3 TO EXT-WI-CL-SSNO-NO.
           IF LN-SSNO(4) = BW-SSNO
              MOVE LN-MAKERCD(4) TO MAKER-CODE
              MOVE 4 TO EXT-WI-CL-SSNO-NO.

           PERFORM DISPLAY-MAKER.


      * WORLD IS NOW USING BW-PRIVACY-OPT-OUT FOR TEXTING OPTION
      *    IF NOT GP-I-AM-WORLD
      *       IF GP-BW-PRIVACY-OPT-OUT = "Y"
      *          IF BW-PRIVACY-OPT-OUT = "Y"
      *             MOVE "S  A020OO:RF." TO VDU
      *             MOVE "OO" TO VDUBUF
      *             PERFORM SCREENIO.

           MOVE LN-LOANDATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO D-H1.
           MOVE LN-1STPYDATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO ALP-DATE-EDIT.
           MOVE LN-UNITPER-CD TO DATER-UNITPER-CD.
           MOVE LN-UNITPER-FREQ TO DATER-UNITPER-FREQ.
           PERFORM UNITPER-MASK.
           MOVE ALP-DATE-EDIT TO D-H2.
           PERFORM MATURITY-DATE-CALCULATION.
           MOVE MDTE-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO D-H3.
           MOVE LN-SUM-FINCHG TO D-H4.

           MOVE BW-PHONE TO PHONE-EDIT.
           MOVE "PH:" TO D-H5-PMPT.
           MOVE "(" TO D-H5-P1.
           MOVE PE-AREA TO D-H5-P2.
           MOVE ")" TO D-H5-P3.
           MOVE PE-PHONE TO D-H5.
           MOVE "-" TO D-H5A.

           MOVE LN-SUM-LNAMT TO D-H6.
           MOVE PDTH-CUR-SCHD-PAY TO D-H7.
           IF LN-PAY-SCHLD-FG = "Y"
              MOVE "S" TO D-H71.
           MOVE LN-LYRINTPD TO D-H8.
           MOVE LN-YTDINTPD TO D-H9.
           PERFORM GET-LC.
           PERFORM COMPUTE-EARNINGS.
           IF LC-DISPLAY-ERN = "A"
              MOVE "ACC-EARN" TO D-B1
              MOVE EARN-TOT-ACCERN TO D-H10
           ELSE
           IF LC-DISPLAY-ERN = "U"
              MOVE "UNEARNED" TO D-B1
              MOVE EARN-TOT-UNEARN TO D-H10
           ELSE
           IF LC-DISPLAY-ERN = "T"
              MOVE "TOT-EARN" TO D-B1
              MOVE LN-TOTERN TO D-H10
           ELSE
           IF LC-DISPLAY-ERN = "I"
              MOVE TRANS-DATE TO POFF-PAYDATE
              MOVE "M"        TO POFF-MAKERCD
              MOVE "PO"       TO POFF-LPTRCD
              MOVE "N"        TO POFF-DEF-STOP
              PERFORM PAYOFF-LOAN-ROUTINE
              MOVE "TDUE INT" TO D-B1
              MOVE POFF-INTDUE TO D-H10
           ELSE
              MOVE " PAYMNTD" TO D-B1
              MOVE LN-TOTPAYMNTD TO D-H10.

           MOVE TRANS-DATE TO AGEING-DATE.
           PERFORM COMPUTE-CONTRACTUAL.
           PERFORM COMPUTE-RECENCY.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      * 'LC' IS INITIAL FORM VALUE
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( DELINQ-SETTING = "LC" )
              MOVE LC-PROF TO DELINQ-SCAN
              MOVE "D" TO DELINQ-PROF3.

           IF LC-CON-PDUE-DAYS = "D"
              MOVE TRANS-DATE TO PDUE-TODAY-DATE
              MOVE LN-1STPYDATE TO PDUE-1STPYDATE
              PERFORM PAST-DUE-DAYS-CALCULATION
              MOVE "DAY:" TO D-PH11
              MOVE PDUE-DAYS TO D-H11
           ELSE
              MOVE "CON:" TO D-PH11
              IF CPOT30 = "Y"
                 MOVE "<P>" TO D-H11A
              ELSE
              IF CPOT30 = "M"
                 MOVE "<M>" TO D-H11A
              ELSE
              IF CPOTCHGOFF = "Y"
                 MOVE "<PC>" TO D-H11A
              ELSE
              IF CPOTCHGOFF = "M"
                 MOVE "<PL>" TO D-H11A
              ELSE
                 MOVE CONTRACTUAL TO D-H11.

           MOVE DELINQ-PROF1 TO DELINQ-CODE.
      D    STOP MESS.
           PERFORM GET-DELINQ-VALUE.
           MOVE DELINQ-VALUE TO D-H12 DISP-DELINQ-VALUE-1.

           IF LC-REC-PDUE-DAYS = "D"
              MOVE TRANS-DATE TO PDUE-TODAY-DATE
              MOVE LN-1STPYDATE TO PDUE-1STPYDATE
              PERFORM PAST-DUE-DAYS-CALCULATION
              MOVE "DAY:" TO D-PH13
              MOVE PDUE-DAYS TO D-H13
           ELSE
              MOVE "REC:" TO D-PH13
              IF RDPOT30 = "Y"
                 MOVE "<P>" TO D-H13A
              ELSE
              IF RDPOT30 = "M"
                 MOVE "<M>" TO D-H13A
              ELSE
              IF RDPOTCHGOFF = "Y"
                 MOVE "<PC>" TO D-H13A
              ELSE
              IF RDPOTCHGOFF = "M"
                 MOVE "<PL>" TO D-H13A
              ELSE
                 MOVE RECDEL TO D-H13.

           MOVE DELINQ-PROF2 TO DELINQ-CODE.
           PERFORM GET-DELINQ-VALUE.
           MOVE DELINQ-VALUE TO D-H14 DISP-DELINQ-VALUE-2.

           MOVE LN-LCPDTH-DATE TO LCHG-LCPDTH-DATE.
           MOVE PDTH-CUR-SCHD-PAY TO LCHG-LATE LCHG-CURPAY.
           PERFORM LATE-CHARGE-CALCULATION.

           MOVE TRANS-DATE TO IBPC-DATE.
           PERFORM IBPC-TEST.
           MOVE LN-SUM-ORGTERM TO D-H16.
           IF (IBPC-FG = "P" OR LN-LOANTYPE = "I")
                           AND (SP-LCFRMLA NOT = " ")
                            AND (LN-ACCELDATE = 0)
              MOVE LCHG-CHARGE TO D-H15
              MOVE LN-LCPDTH-DATE TO NUM-DATE
              MOVE NUM-MO TO D-H17-MO
              IF LN-UNITPER-CD-DAYS
                 MOVE NUM-DA TO D-H17-DAYR
              ELSE
                 MOVE NUM-YR TO D-H17-DAYR.
           MOVE LN-TOTNODEF TO D-H18.
           MOVE LN-TOTNOALDEL TO D-H181.
           MOVE LN-OVRDRATE TO D-H19.
           MOVE LN-ACTIONCD TO D-H20.
           MOVE LN-DLNO TO D-DLRNUM.
           MOVE LN-COLLECTOR TO D-COLCTID.
           IF LN-SALEFIN = "Y" AND LN-SERVACCT = "N"
               PERFORM RECOURSE-RTN
           ELSE
               MOVE SPACES TO D-RCSTAG D-RCOURS.
           MOVE LN-DELCNT TO D-DELCNT.
           MOVE LN-PROMDATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO D-PROMDT1.
           IF BR-SECOND-PROMISE = "Y"
              MOVE LN-PROMDATE2 TO DATE-YYYYMMDD
              PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
              MOVE DATE-MMDDYY TO D-PROMDT2
           ELSE
              MOVE 0 TO D-PROMDT2.
           MOVE LN-PROMAMT TO D-PROMAMT.
           MOVE LN-COLLCD TO D-COLLCD.

           IF LN-COLLID NOT = SPACES
              MOVE LN-COLLID TO D-COLLID
           ELSE
              MOVE LTC-PATH-OWNBR TO LTC-BRNO
              MOVE LN-ACCTNO      TO LTC-ACCTNO
              MOVE 1              TO LTC-SEQNO
              PERFORM OPEN-LTC1-FILE
              PERFORM READ-LTC1-FILE
              PERFORM CLOSE-LTC1-FILE
              IF IO-FG = 0
                 IF LTC-MAKE(1) NOT = SPACES
                    MOVE LTC-YEAR(1) TO VDU
                    INSPECT VDU REPLACING FIRST "    " BY " +++"
                    INSPECT VDU REPLACING FIRST "+++            "
                                                       BY LTC-MAKE(1)
                    INSPECT VDU REPLACING FIRST "    " BY " +++"
                    INSPECT VDU REPLACING FIRST "+++            "
                                                       BY LTC-MODEL(1)
                    INSPECT VDU REPLACING FIRST "    " BY " +++"
                    INSPECT VDU REPLACING FIRST "+++     "
                                                       BY LTC-STYLE(1)
                    INSPECT VDU REPLACING FIRST "    " BY " +++"
                    INSPECT VDU REPLACING FIRST "+++                 "
                                                       BY LTC-VIN(1)
                    MOVE VDU TO D-COLLID
                 ELSE
                    MOVE LTC-HHGS(1) TO D-COLLID
              ELSE
                 MOVE SPACES TO D-COLLID.
           MOVE DISPLAY-LOAN-BUF TO WR-BUF.
           MOVE DISPLAY-LOAN-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.
      *    CALL XFER USING STAT DISPLAY-LOAN-LIST DISPLAY-LOAN-BUF.

           PERFORM VARYING S1 FROM 1 BY 1 UNTIL S1 > 10
               IF LN-STATUSFLAGS(S1) = "C"
                  MOVE 10 TO S1
                  MOVE "S  A160LNNO:R." TO VDU
                  MOVE D-LOAN-NUM TO VDUBUF
                  PERFORM SCREENIO
               END-IF
           END-PERFORM.

           PERFORM SET-DELINQ-DISP-ATTRIBUTES.

           MOVE DISP-DELINQ-VALUE TO WR-BUF.
           MOVE DISPLAY-DELINQ-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

           IF (LN-ACH = "Y" AND GP-LOAN-ACH-PROCESSING = "Y")
              MOVE ACH-PROMPT-BUF TO WR-BUF
              MOVE ACH-PROMPT-LIST TO WR-LIST
              PERFORM CALL-WRFORM
           ELSE
              MOVE PMT-PROMPT-BUF TO WR-BUF
              MOVE PMT-PROMPT-LIST TO WR-LIST
              PERFORM CALL-WRFORM.

           MOVE LN-MESSAGE TO D-MESSAGE.
           PERFORM SET-MESSAGE-ATTR.

           IF LN-MESSAGE = SPACES
              MOVE SPACES TO MESSAGE-ATTR
              MOVE D-MESSAGE TO WR-BUF
              MOVE MESSAGE-LIST TO WR-LIST
              PERFORM CALL-WRFORM
           ELSE
              MOVE D-MESSAGE TO WR-BUF
              MOVE MESSAGE-LIST-ATTR TO WR-LIST
              PERFORM CALL-WRFORM.

           PERFORM WORLD-CREDIT-DISPLAY.

      *
      *    IF BR-AVAIL-CREDIT = "Y"
      *              AND (LN-CREDITLIM NOT = 0)
      *               AND (LN-BNKRPTDATE = 0)
      *       MOVE TRANS-DATE TO AVCR-PAYDATE
      *       MOVE MAKER-CODE TO AVCR-MAKERCD
      *       PERFORM COMPUTE-AVAILABLE-CREDIT
      *       MOVE " HIGH CREDIT:" TO HIGH-CREDIT-X-BUF
      *       MOVE " AVAILABLE:" TO AVA-BUF
      *       MOVE LN-CREDITLIM TO HIGH-CREDIT-BUF
      *       MOVE AVCR-AVAILCREDIT TO AVAIL-BUF
      *       MOVE AVCR-RESTRICT-FG TO AVAIL-BUF-RESTRICT-FG
      *       MOVE HIGH-BUF TO WR-BUF
      *       MOVE HIGH-LIST-ATTR TO WR-LIST
      *       PERFORM CALL-WRFORM
      *    ELSE
      *    IF BR-AVAIL-CREDIT = "S"
      *              AND (LN-CREDITLIM NOT = 0)
      *               AND (LN-BNKRPTDATE = 0)
      *       MOVE TRANS-DATE TO AVCR-PAYDATE
      *       MOVE MAKER-CODE TO AVCR-MAKERCD
      *       PERFORM COMPUTE-AVAILABLE-CREDIT
      *       MOVE " CUR AVAIL:" TO HIGH-CREDIT-X-BUF
      * - - - - - - SETTING CUR. AVAILABLE BY HIGH CREDIT IN LOAN RECORD
      *       MOVE AVCR-AVAILCREDIT TO HIGH-CREDIT-BUF
      *       MOVE " AVAILABLE:" TO AVA-BUF
      * - - - - - - SETTING  AVAILABLE BY A NET PAYOFF
      *       MOVE AVCR-CURRENT-AVAIL TO AVAIL-BUF
      *       MOVE AVCR-RESTRICT-FG TO AVAIL-BUF-RESTRICT-FG
      *       IF LN-CLASS = 5 OR 8 OR 11
      *          MOVE SPACES        TO AVA-BUF AVAIL-SPACE
      *          MOVE HIGH-BUF TO WR-BUF
      *          MOVE HIGH-LIST-S5 TO WR-LIST
      *          PERFORM CALL-WRFORM
      *       ELSE
      *          MOVE HIGH-BUF TO WR-BUF
      *          MOVE HIGH-LIST-ATTR TO WR-LIST
      *          PERFORM CALL-WRFORM
      *    ELSE
      * DISPLAY BRANCH AND HIGHLIGHT
      *       IF BR-AVAIL-CREDIT = "B"
      *          MOVE TRANS-DATE TO AVCR-PAYDATE
      *          MOVE MAKER-CODE TO AVCR-MAKERCD
      *          PERFORM COMPUTE-AVAILABLE-CREDIT
      *          MOVE "BRANCH:" TO HIGH-CREDIT-X-BUF
      *          MOVE BR-NO TO HIGH-CREDIT-BUF
      *          MOVE " AVAILABLE:" TO AVA-BUF
      *          MOVE AVCR-AVAILCREDIT TO AVAIL-BUF
      *          MOVE AVCR-RESTRICT-FG TO AVAIL-BUF-RESTRICT-FG
      *          MOVE HIGH-BUF TO WR-BUF
      *          MOVE HIGH-LIST-S5 TO WR-LIST
      *          PERFORM CALL-WRFORM
      *       ELSE
      *          MOVE SPACES TO HIGH-BUF
      *          MOVE HIGH-BUF TO WR-BUF
      *          MOVE HIGH-LIST TO WR-LIST
      *          PERFORM CALL-WRFORM.

           IF IO-TYPE = "PST"
              PERFORM SEND-LOAN-MESS
              GO TO DISPLAY-LOAN-EXIT.

           MOVE LN-1STPYDATE TO PDTH-DATE-WORK.
           PERFORM PAID-THRU-CALCULATION.
           MOVE PDTH-DATE-FULL TO NUM-DATE.
           MOVE NUM-MO TO D-PDTH-MO.
           IF LN-UNITPER-CD-DAYS
              MOVE NUM-DA TO D-PDTH-DAYR
           ELSE
              MOVE NUM-YR TO D-PDTH-DAYR.
           MOVE LN-OTHBAL TO D-OTHBAL.
           ADD LN-LCBAL LN-INTBAL GIVING D-INTBAL.
           MOVE LN-CURBAL TO D-CURBAL.
           MOVE DISPLAY-BAL-BUF TO WR-BUF.
           MOVE DISPLAY-BAL-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.
           PERFORM SEND-LOAN-MESS.

       DISPLAY-LOAN-EXIT.
           EXIT.

      ****************************************
       WORLD-CREDIT-DISPLAY SECTION.
      ****************************************

      * CHECK TO SEE IF ACCT WOULD BE A PBD IF PD OFF TODAY

           MOVE " " TO PBD-FLAG.
           MOVE TRANS-DATE TO PDUE-TODAY-DATE.
           MOVE LN-1STPYDATE TO PDUE-1STPYDATE.
           PERFORM PAST-DUE-DAYS-CALCULATION.
           IF SP-PBDEL-DAYS NOT = 0
              IF PDUE-DAYS > SP-PBDEL-DAYS
                 MOVE "D" TO PBD-FLAG.

      * CHECK FOR BANKRUPTS REAFFIRMATION OR DISCHARGE

           MOVE LN-OWNBR  TO LTB-BRNO.  
           MOVE LN-ACCTNO TO LTB-ACCTNO.
           MOVE 1         TO LTB-SEQNO.
           PERFORM OPEN-LTB1-FILE.
           PERFORM READ-LTB1-FILE.
           PERFORM CLOSE-LTB1-FILE.
           IF IO-FG = 0
              IF LTB-REAFF-DATE NOT = 0 AND LTB-RESCIND-DATE = 0
                 STRING "REAFFIRMED, ",LTB-WHO-FILED,
                        DELIMITED BY "  " INTO WORLD-CREDIT-BUF
                 MOVE WORLD-REAFFIRM-LIST TO WR-LIST
                 MOVE WORLD-CREDIT-BUF TO WR-BUF
                 PERFORM CALL-WRFORM
                 GO TO EXIT-WORLD-CREDIT-DISPLAY
              ELSE
              IF LTB-DISCHARGE-DATE NOT = 0
                 MOVE LTB-DISCHARGE-DATE TO DATE-YYYYMMDD
                 PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
                 MOVE DATE-MMDDYY TO VDU-DATE-EDIT
                 MOVE SPACES TO WORLD-CREDIT-BUF 
                 STRING "DISCHARGED, ", 
                        LTB-WHO-FILED,", ",
                        VDU-DATE-EDIT
                        DELIMITED BY "  " INTO WORLD-CREDIT-BUF
                 MOVE WORLD-REAFFIRM-LIST TO WR-LIST
                 MOVE WORLD-CREDIT-BUF TO WR-BUF
                 PERFORM CALL-WRFORM
                 GO TO EXIT-WORLD-CREDIT-DISPLAY.

           IF BR-AVAIL-CREDIT = "Y"
                     AND (LN-CREDITLIM NOT = 0)
                      AND (LN-BNKRPTDATE = 0)
              MOVE TRANS-DATE TO AVCR-PAYDATE
              MOVE MAKER-CODE TO AVCR-MAKERCD
              PERFORM COMPUTE-AVAILABLE-CREDIT
              MOVE "HI CRED:" TO WORLD-HI-CREDIT-PROMPT
              MOVE LN-CREDITLIM TO WORLD-HI-CREDIT-AMT
              MOVE "WCBC:" TO WORLD-WCBC-CREDIT-PROMPT
              MOVE LN-WCBC-CREDITLIM TO WORLD-WCBC-CREDIT-AMT
              MOVE "AVAIL:" TO WORLD-AVAIL-CREDIT-PROMPT
              MOVE AVCR-AVAILCREDIT TO WORLD-AVAIL-CREDIT-AMT
              IF AVCR-RESTRICT-FG = " "
                 MOVE PBD-FLAG TO WORLD-AVAIL-FLAG1
                 MOVE " "      TO WORLD-AVAIL-FLAG2
              ELSE
                 MOVE AVCR-RESTRICT-FG TO WORLD-AVAIL-FLAG1
                 MOVE PBD-FLAG         TO WORLD-AVAIL-FLAG2
              END-IF
              MOVE WORLD-CREDIT-BUF TO WR-BUF
              MOVE WORLD-CREDIT-LIST-ATTR TO WR-LIST
              PERFORM CALL-WRFORM
           ELSE
           IF BR-AVAIL-CREDIT = "S"
                     AND (LN-CREDITLIM NOT = 0)
                      AND (LN-BNKRPTDATE = 0)
              MOVE TRANS-DATE TO AVCR-PAYDATE
              MOVE MAKER-CODE TO AVCR-MAKERCD
              PERFORM COMPUTE-AVAILABLE-CREDIT
              MOVE "CUR AVL:" TO WORLD-HI-CREDIT-PROMPT
      * SETTING CUR. AVAILABLE BY HIGH CREDIT IN LOAN RECORD
              MOVE AVCR-AVAILCREDIT TO WORLD-HI-CREDIT-AMT
              MOVE "WCBC:" TO WORLD-WCBC-CREDIT-PROMPT
              MOVE LN-WCBC-CREDITLIM TO WORLD-WCBC-CREDIT-AMT
              MOVE "AVAIL:" TO WORLD-AVAIL-CREDIT-PROMPT
      * SETTING  AVAILABLE BY A NET PAYOFF
              MOVE AVCR-CURRENT-AVAIL TO WORLD-AVAIL-CREDIT-AMT
              IF AVCR-RESTRICT-FG = " "
                 MOVE PBD-FLAG TO WORLD-AVAIL-FLAG1
                 MOVE " "      TO WORLD-AVAIL-FLAG2
              ELSE
                 MOVE AVCR-RESTRICT-FG TO WORLD-AVAIL-FLAG1
                 MOVE PBD-FLAG         TO WORLD-AVAIL-FLAG2
              END-IF
              MOVE WORLD-CREDIT-BUF TO WR-BUF
              MOVE WORLD-CREDIT-LIST-ATTR TO WR-LIST
              PERFORM CALL-WRFORM
           ELSE
              MOVE SPACES TO WORLD-CREDIT-BUF
              MOVE WORLD-CREDIT-BUF TO WR-BUF
              MOVE WORLD-CREDIT-LIST TO WR-LIST
              PERFORM CALL-WRFORM.

       EXIT-WORLD-CREDIT-DISPLAY.
           EXIT.

       COPY "LIBGB/DATER.CPY".
       COPY "LIBLP/SETCTBL.CPY".
       COPY "LIBLP/LPLCHG.CPY".
       COPY "LIBLP/LPMDTE.CPY".
       COPY "LIBLP/LPXDTE.CPY".
       COPY "LIBLP/LPEARN.CPY".
       COPY "LIBGB/AGEING.CPY".
       COPY "LIBLP/LPUPWK.CPY".
       COPY "LIBLP/LPCEPP.CPY".
       COPY "LIBGB/SPPDTH.CPY".

       COPY "LIBLP/LPTDEL.CPY".
       COPY "LIBLP/LPCDEL.CPY".
       COPY "LIBLP/LPLCAP.CPY".
       COPY "LIBLP/LPLCAS.CPY".
       COPY "LIBLP/LPIBPC.CPY".
       COPY "LIBLP/LPRATE.CPY".
       COPY "LIBLP/LPSTAT.CPY".
       COPY "LIBGB/GPENV.CPY".


      **********************************
      *TEST-IF-SC-RESTRICTION SECTION.
      **********************************
      * CHECK FOR THE SOUTH CAROLINA SERVICE CHARGE FRMLA 18 RESTRICTION
      * IF WE WOULD REJECT A RENEWAL, DONT DISPLAY AVAIL CREDIT
      * IF CASH AVAILBLE IS < 10% OF LOANS AMOUNT FINANCED, RESTRICT IT
      * NOT ALLOWED TO RENEW AN ACCOUNT FOR A CASH ADVANCE LESS THAN 10%
      * MORE THAN ONCE IN 15 MONTHS.
      *    IF SP-SCFRMLA NOT = 18
      *       GO TO EXIT-TEST-IF-SC-RESTRICTION.

      *    PERFORM LOAN-CALCULATIONS.
      *    IF AVCR-AVAILCREDIT < (FINANCED-AMOUNT * .1)
      *       IF BW-LAST-RN-FOR10-DATE NOT = 0
      *          MOVE BW-LAST-RN-FOR10-DATE TO NUM-DATE
      *          MOVE TRANS-DATE TO SYS-DATE
      *          PERFORM TIM360
      *          IF ELAPSED-MONTHS < 15
      *             MOVE 0 TO WORLD-AVAIL-CREDIT-AMT.


      *EXIT-TEST-IF-SC-RESTRICTION.
      *    EXIT.

      **************************
       TRCD-DISPLAY SECTION.
      **************************
       TRCD-PAGE1.
           MOVE TRCD-LINE TO WR-BUF.
           MOVE "HEAD:S." TO WR-LIST.
           PERFORM CALL-WRFORM.

           MOVE TRCD-BUF TO WR-BUF.
           MOVE LINE-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       TRCD-ASK.
           MOVE "S  A040MSEL." TO VDU.
           MOVE "<ENTER> - NEXT PAGE, F7-DONE" TO VDUBUF.
           PERFORM SCREENIO.
           MOVE "E RA000SEL." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS NOT = "1>"
              MOVE TRCD-BUF2 TO WR-BUF
              MOVE LINE-LIST TO WR-LIST
              PERFORM CALL-WRFORM
              MOVE "S  A040MSEL." TO VDU
              MOVE "<ENTER> - NEXT PAGE, F7-DONE" TO VDUBUF
              PERFORM SCREENIO
              MOVE "E RA000SEL." TO VDU
              PERFORM SCREENIO
              IF VDUSTATUS NOT = "1>"
                 GO TO TRCD-PAGE1.

           MOVE SPACES TO WR-BUF.
           MOVE "L08 L07 L06 L05 L04 L03 L02 L01." TO WR-LIST.
           PERFORM CALL-WRFORM.

           PERFORM SEND-HEAD-1.

      ******************************************
       GET-ACCOUNT SECTION.
      ******************************************
           MOVE " " TO GET-ACCOUNT-ERR.

           IF BR-INBR-BILNSC-FG = "M"
              PERFORM LSCAN-LOCAL-SCAN
              IF LSCAN-ERR = "Y"
                 GO TO EXIT-GET-ACCOUNT-ERR
              END-IF
           ELSE
              MOVE GLOBAL-SCAN TO PASSWDW-SEQ
              MOVE "TO USE GLOBAL LOAN SCAN" TO PASSWDW-PROMPT2
              PERFORM VERIFY-PASSWORD
              IF PASSWDW-STAT NOT = "OK"
                 GO TO EXIT-GET-ACCOUNT-ERR
              ELSE
                 MOVE 0                   TO BILNSC-SSNO
                 MOVE "T"                 TO BILNSC-ERROR
                 MOVE EXT-CONAME-SYSDATE  TO BILNSC-SYSDATE
                 MOVE EXT-CONAME-USER-LOC TO BILNSC-BRANCH
                 MOVE "CPINQ0"            TO BILNSC-CALLED-FROM
                 MOVE "WI/BILNSC" TO FORM-NAM
                 CALL FORM-PROGX USING FORM-PATH BILNSC-BUF
                                       EXIT-PATHNAME
                 CANCEL FORM-PROGX
                 IF BILNSC-ERROR = "Y"
                    GO TO EXIT-GET-ACCOUNT-ERR.

      *----------------------------------------------------------------
      *  IF USERS BRANCH IS DIFFERENT FROM POSTING BRANCH THEN READ
      *     BRANCH FILE AND SET UP NEW DATA PATH.
      *----------------------------------------------------------------
           INSPECT BILNSC-ACCTNO REPLACING ALL SPACES BY ZEROS.
           INSPECT BILNSC-BRANCH REPLACING ALL SPACES BY ZEROS.

      *-----------------------------------------------------------------
      *    IF YOU PICKED AN ACCT IN YOUR OWN BRANCH, DON'T BOTHER
      *    SETTING UP NEW EXT-FILPATH, JUST GET OUT.
      *-----------------------------------------------------------------
           IF BILNSC-BRANCH = EXT-FILPATH-BRANCH
              MOVE BILNSC-ACCTNO TO LN-ACCTNO
              GO TO EXIT-GET-ACCOUNT.

      *-----------------------------------------------------------------
      * NO LONGER SAVING OLD DATA PATH, IN CPINQ WE SAVE EXT-FILPATH
      * IN HOLD-EXT-FILPATH.  SO JUST SET UP NEW BRANCH'S DATA PATH:
      *-----------------------------------------------------------------
           MOVE BILNSC-BRANCH TO BR-NO.
           PERFORM GET-BR.
           IF IO-FG NOT = 0
              PERFORM IO-ERROR
              MOVE "BRANCH &&&& DOES NOT EXIST " TO MESS
              INSPECT MESS REPLACING FIRST "&&&&" BY BILNSC-BRANCH
              PERFORM SEND-MESS
              GO TO EXIT-GET-ACCOUNT-ERR.

           MOVE BR-MACHINE         TO EXT-FILPATH-MACHINE
                                      SOURCE-MACHINE.
           MOVE BR-DATA-PATH       TO EXT-FILPATH-DATA.
           SET ENVIRONMENT "FIL"   TO EXT-FILPATH-FIL.

           MOVE BILNSC-ACCTNO TO LN-ACCTNO.
           GO TO EXIT-GET-ACCOUNT.
       EXIT-GET-ACCOUNT-ERR.
           MOVE "Y" TO GET-ACCOUNT-ERR.
       EXIT-GET-ACCOUNT.
           EXIT.

      **************************************
       RESET-PATH SECTION.
      **************************************

      * RESET DATA PATH TO POSTING BRANCH

           IF HOLD-EXT-FILPATH NOT = EXT-FILPATH
              MOVE HOLD-EXT-FILPATH   TO EXT-FILPATH
              SET ENVIRONMENT "FIL"   TO EXT-FILPATH-FIL
              MOVE EXT-FILPATH-BRANCH TO BR-NO
              PERFORM GET-BR
              IF IO-FG = 9
                 PERFORM IO-ERROR
                 MOVE "BRANCH &&&& DOES NOT EXIST " TO MESS
                 INSPECT MESS REPLACING FIRST "&&&&"
                                      BY EXT-CONAME-USER-LOC
                 PERFORM SEND-MESS.

      * GOTTA RESET PATH OVERRIDES IN CASE ARCHIVE SETUP MADE THEM 'Y'

           MOVE " " TO LN-PATH-OVERRIDE
                       LTC-PATH-OVERRIDE
                       LTP-PATH-OVERRIDE
                       LTI-PATH-OVERRIDE
                       LTB-PATH-OVERRIDE
                       LTA-PATH-OVERRIDE.
       EXIT-RESET-PATH.
           EXIT.

      ******************************************************************
       LSCAN-LOCAL-SCAN SECTION.
      ******************************************************************

      * ALTERNATIVE TO GLOBAL LOAN SCAN. ALLOWS USER TO ENTER A SINGLE
      * BRANCH AND THEN BR SENT INTO PAYMENT PROCESSING.
           MOVE 7 TO WINDOW-LINES.
           MOVE 60 TO WINDOW-COLUMNS.
           MOVE 16 TO WINDOW-BEGIN-Y.
           MOVE 10 TO WINDOW-BEGIN-X.
           MOVE " " TO WINDOW-FIT-TO.
           MOVE "WI/INBRT2" TO WINDOW-FORM-NAM.
           MOVE "INBRT2" TO VDU-FORM-NAME.

           PERFORM OPEN-WINDOW.
           DISPLAY INBRT2-SCREEN.

           MOVE SPACE TO LSCAN-ERR.

       LSCAN-ENTER-BRANCH.
           MOVE " ENTER OWNING BRANCH OF ACCOUNT:" TO MESS.
           PERFORM SEND-WSEL-LEGEND.

           MOVE "ENNN040BRNO." TO VDU.
           PERFORM SCREENIO.
           IF F7-KEY
               GO TO LSCAN-CLOSE-ERR.
           IF NOT ( ENTER-KEY OR DOWN-KEY )
               GO TO LSCAN-ENTER-BRANCH.

           MOVE VDUNUM0 TO BILNSC-BRANCH BR-NO.

           PERFORM GET-BR.
           IF IO-FG NOT = 0
              MOVE "BRANCH RECORD NOT FOUND" TO MESS
              PERFORM SEND-MESS
              GO TO LSCAN-ENTER-BRANCH.

           MOVE "S  A040NAME." TO VDU.
           MOVE BR-NAME TO VDUBUF.
           PERFORM SCREENIO.

           IF BR-MACHINE NOT = SOURCE-MACHINE
              MOVE 0 TO PASSWDW-SEQ
              MOVE "TO POST ACROSS MACHINES" TO PASSWDW-PROMPT2
              PERFORM VERIFY-PASSWORD
              IF PASSWDW-STAT NOT = "OK"
                 MOVE "POSTING TO ANOTHER MACHINE IS NOT ALLOWED"
                     TO MESS
                 PERFORM SEND-MESS
                 GO TO LSCAN-ENTER-BRANCH.

       LSCAN-ENTER-ACCTNO.
           MOVE " ENTER ACCOUNT NUMBER:" TO MESS.
           PERFORM SEND-WSEL-LEGEND.
           MOVE "ENNN060ACCTNO." TO VDU.
           PERFORM SCREENIO.

           IF NOT ( ENTER-KEY OR DOWN-KEY )
              GO TO LSCAN-ENTER-ACCTNO.
           IF F1-KEY OR UP-KEY
              GO TO LSCAN-ENTER-BRANCH.

           MOVE VDUNUM0 TO BILNSC-ACCTNO.

           MOVE " ACCEPT ACCOUNT (Y/N):" TO MESS.
           PERFORM SEND-WSEL-LEGEND.
           MOVE "ENNA010ACCEPT." TO VDU.
           PERFORM SCREENIO.
           IF F1-KEY OR UP-KEY
              GO TO LSCAN-ENTER-BRANCH.
           IF F7-KEY
              GO TO LSCAN-CLOSE-ERR.
           IF NOT (ENTER-KEY OR DOWN-KEY )
              GO TO LSCAN-ENTER-BRANCH.
           IF VDUBUF NOT = "Y"
              GO TO LSCAN-ENTER-BRANCH.

           GO TO LSCAN-LOCAL-SCAN-EXIT.
       LSCAN-CLOSE-ERR.
           MOVE "Y" TO LSCAN-ERR.
       LSCAN-LOCAL-SCAN-EXIT.

           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY INBRT2-SCREEN.
           PERFORM CLOSE-WINDOW UNTIL WINDOWS-OPEN = 0.
           MOVE "CPINQ" TO VDU-FORM-NAME.
       LSCAN-LOCAL-SCAN-REAL-EXIT.
           EXIT.


      *****************************************
       GET-DELINQ-CODES SECTION.
      *****************************************
       GET-DELINQ-ENTER.
           MOVE "S  A000MSEL." TO VDU.
           MOVE "ENTER 3 DELINQUENCY CODES (EX. CRD), F7-EXIT:"
                                         TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "R RA060SEL." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "1>"
              GO TO GET-DELINQ-CODES-EXIT.
           IF VDUSTATUS NOT = "00"
              GO TO GET-DELINQ-CODES-EXIT.
           MOVE VDUBUF TO DELINQ-FORM.

           MOVE DELINQ-CODES TO DELINQ-SCAN.
           IF DELINQ-PROF1 NOT = "C" AND NOT = "D"
              MOVE "PROFILE 1 MUST BE 'C' OR 'D' " TO MESS
              PERFORM SEND-MESS
              GO TO GET-DELINQ-ENTER.
           IF DELINQ-PROF2 NOT = "R" AND NOT = "D"
              MOVE "PROFILE 2 MUST BE 'R' OR 'D' " TO MESS
              PERFORM SEND-MESS
              GO TO GET-DELINQ-ENTER.
           IF DELINQ-PROF3 NOT = "C" AND NOT = "R" AND NOT = "D"
              MOVE "PROFILE 3 MUST BE 'C', 'R' OR 'D'" TO MESS
              PERFORM SEND-MESS
              GO TO GET-DELINQ-ENTER.

           PERFORM SEND-DELINQ-SCAN.

      * SETTING THE DELINQUENCY PROFILE

           MOVE "F4" TO DELINQ-SETTING.

       GET-DELINQ-CODES-EXIT.
           EXIT.


      *******************************************************
      * PASSWORD MUST EXIST SECTION
      *   LIKE VERIFY PASSWORD, BUT WILL GIVE BAD STATUS IF
      *   PASSWORD NOT IN FILE.  IF PASSWORD IN FILE, WILL
      *   ASK FOR PASSWORD & ACT JUST LIKE VERIFY
      *******************************************************
       PASSWORD-MUST-EXIST SECTION.
      *******************************************************
           MOVE "EX"    TO PASSWDW-STAT.
           MOVE "CPINQ" TO PASSWDW-PGM.
           MOVE "PLEASE ENTER THE PASSWORD," TO PASSWDW-PROMPT1.
           MOVE "CL/PASSWD" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH PASSWDW-BUF.
           CANCEL FORM-PROGX.

      *******************************************************
       VERIFY-PASSWORD SECTION.
      *******************************************************
           MOVE " "     TO PASSWDW-STAT.
           MOVE "CPINQ" TO PASSWDW-PGM.
           IF PASSWDW-SEQ = BK-SEQ
              MOVE "PLEASE ENTER PASSWORD OR CALL" TO PASSWDW-PROMPT1
           ELSE
              MOVE "PLEASE ENTER THE PASSWORD," TO PASSWDW-PROMPT1.
           MOVE "CL/PASSWD" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH PASSWDW-BUF.
           CANCEL FORM-PROGX.

      *******************************************************
       VERIFY-PASSWD-BK-WORLD SECTION.
      *******************************************************
           MOVE " " TO  PASSWDW-STAT.
           MOVE "CPINQ0" TO PASSWDW-PGM.
           MOVE "PLEASE ENTER PASSWORD TO ACCESS" TO
                                             PASSWDW-PROMPT1.
           MOVE "BANKRUPT ACCOUNT"     TO    PASSWDW-PROMPT2.
           MOVE "CL/PASSWD"  TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH PASSWDW-BUF.
           CANCEL FORM-PROGX.


      ******************************************************************
      * CHANGED ON THE FLY BASED UP GP/LC FLAGS.
      *    LC-PROF1   LC-PROF2    DISPLAY-DELINQ-LIST
      *       C           R       "H12 H14.    "   (DEFAULT)
      *       D           R       "H12:R H14.  "
      *       C           D       "H12 H14:R.  "
      *       D           D       "H12:R H14:R."
      ******************************************************************
       SET-DELINQ-DISP-ATTRIBUTES SECTION.
      ******************************************************************
           IF LC-PROF1 = "D" AND LC-PROF2 = "D"
              MOVE "H12:R H14:R." TO DISPLAY-DELINQ-LIST
           ELSE
              IF LC-PROF1 = "D"
                 MOVE "H12:R H14.  " TO DISPLAY-DELINQ-LIST
              ELSE
                 IF LC-PROF2 = "D"
                    MOVE "H12 H14:R.  " TO DISPLAY-DELINQ-LIST
                 ELSE
                    MOVE "H12 H14.    " TO DISPLAY-DELINQ-LIST.
       EXIT-SET-DELINQ-DISP-ATTRIBUTE.
           EXIT.


      ******************************************
       FORM-RESET SECTION.
      ******************************************
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN
           DISPLAY CPINQ-SCREEN UPON CPINQ-WINDOW-HANDLE.
           MOVE CPINQ-FORM-FIELDS TO WR-BUF.
           MOVE CPINQ-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      ******************************************
       SETUP-LINK-INFO SECTION.
      ******************************************
           IF VDU-FORM-NAME = "INBRT2"
               MOVE INBRT2-HELP--FILE TO HELP-LINK-FILE
               MOVE INBRT2-HELP--DIR  TO HELP-LINK-DIR 
           ELSE
               MOVE CPINQ-HELP--FILE TO HELP-LINK-FILE
               MOVE CPINQ-HELP--DIR TO HELP-LINK-DIR.

      ******************************************
       VDU-CONVERT-FIELD SECTION.
      ******************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBSP/CPINQ_EVA.CPY".
              COPY "LIBWI/INBRT2_EVA.CPY".
              COPY "LIBWI/WRLDBK_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ******************************************
      *   M I S C   P A R A G R A P H *
      *********************************
       MISC-PARAGRAPHS SECTION.
       COPY "LIBGB/SYSTEM.CPY".
       COPY "LIBGB/GETENV.CPY".
       GET-BR.
           PERFORM OPEN-BR-FILE.
           PERFORM READ-BR-FILE.
           PERFORM CLOSE-BR-FILE.
       GET-BW.
           PERFORM OPEN-BW1-FILE.
           PERFORM READ-BW1-FILE.
           PERFORM CLOSE-BW1-FILE.
       GET-SP.
           MOVE LN-ORGST    TO SP-ORGST.
           MOVE LN-SPRCLASS TO SP-SPRCLASS.
           MOVE LN-SUBCLASS TO SP-SUBCLASS.
           MOVE LN-LAWCODE  TO SP-LAWCODE.
           PERFORM READ-SP1-FILE.
           IF IO-FG NOT = 0
              GO TO IO-ERROR.
       GET-DELINQ-VALUE.
           IF DELINQ-CODE = "R"
              MOVE LN-RECPROFILE(1) TO PROFILE-EDIT
              INSPECT PROFILE-ED REPLACING ALL "9" BY "-"
              MOVE PROFILE-ED TO DELINQ-ALP
           ELSE IF DELINQ-CODE = "C"
              MOVE LN-CONPROFILE(1) TO PROFILE-EDIT
              INSPECT PROFILE-ED REPLACING ALL "9" BY "-"
              MOVE PROFILE-ED TO DELINQ-ALP
           ELSE
              MOVE LN-1STPYDATE TO CEPP1-DATE
              MOVE TRANS-DATE TO CEPP2-DATE
              MOVE MAKER-CODE TO CEPP-MAKERCD
              IF GP-NO-LC-IN-ARREARAGE
                 PERFORM CONTRACTUAL-DELINQUENCY
                 MULTIPLY -1 BY CDEL-DELINQUENT GIVING DELINQ-NUM
              ELSE
                 PERFORM TOTAL-DELINQUENCY
                 MULTIPLY -1 BY CDEL-ARREARAGE GIVING DELINQ-NUM.
       SEND-DELINQ-SCAN.
      *    X IS INITIAL FORM VALUE
           IF DELINQ-PROF1 = "X"
              MOVE "D" TO DELINQ-PROF3.
           MOVE DELINQ-PROF3 TO DELINQ-CODE.
           IF DELINQ-CODE = "R"
              MOVE "REC/DELINQ" TO DELINQ-PROMPT
           ELSE IF DELINQ-CODE = "C"
              MOVE "CONTRACTUAL" TO DELINQ-PROMPT
           ELSE
              MOVE "DELINQUENCY" TO DELINQ-PROMPT.
       RECOURSE-RTN.
           IF LN-DLRECOURSE = "F"
               MOVE "FULL    " TO D-RCOURS.
           IF LN-DLRECOURSE = "N"
               MOVE "NON     " TO D-RCOURS.
           IF LN-DLRECOURSE = "P"
               MOVE LN-DLPARTRCAMT TO D-NRCOURS.
           MOVE "RC:" TO D-RCSTAG.
       DISPLAY-INSURANCE-TRACKING.
           MOVE LN-OWNBR  TO LTA-BRNO.
           MOVE LN-ACCTNO TO LTA-ACCTNO.
           MOVE 1         TO LTA-SEQNO.
           PERFORM OPEN-LTA1-FILE.
           PERFORM READ-LTA1-FILE.
           PERFORM CLOSE-LTA1-FILE.
           IF IO-FG = 0
              MOVE LTA-TRACK-ONLY TO D-TRACK
              MOVE LTA-INSURANCE TO D-INS
           ELSE
              MOVE SPACES TO D-TRACK D-INS.

           MOVE "N" TO D-CPP.
           IF LN-INSEFF-DATE(5) NOT = 0
              IF LN-INSEFF-DATE(5) <= TRANS-DATE
                 IF LN-INSEXP-DATE(5) NOT = 0
                    IF LN-INSEXP-DATE(5) > TRANS-DATE
                       IF LN-O2-REBATE = " "
                          MOVE "Y" TO D-CPP.

           MOVE DISPLAY-TRACKING TO D-MEMO.
       GET-LC.
           MOVE LN-CLASS TO LC-CODE.
           PERFORM OPEN-LC1-FILE.
           PERFORM READ-LC1-FILE.
           PERFORM CLOSE-LC1-FILE.
       BLINK-STARS.
           MOVE "S  A000STAR1." TO VDU.
           MOVE "***" TO VDUBUF.
           PERFORM SCREENIO.
           MOVE "S  A000STAR2." TO VDU.
           MOVE "***" TO VDUBUF.
           PERFORM SCREENIO.
           MOVE "S  A000STAR1." TO VDU.
           PERFORM SCREENIO.
           MOVE "S  A000STAR2." TO VDU.
           PERFORM SCREENIO.
       SEND-LOAN-MESS.
           IF LN-MESSAGE NOT = SPACES
              PERFORM ALARM
              PERFORM BLINK-STARS
              PERFORM ALARM.
       DISPLAY-MAKER.
           IF MAKER-CODE = "M"
              MOVE "MAKER" TO D-MAKER.
           IF MAKER-CODE = "J"
              MOVE "JOINT" TO D-MAKER.
           IF MAKER-CODE = "E"
              MOVE "ENDORSER" TO D-MAKER.
       NUMBW-TO-ALPBW.
           MOVE NUM-BW1 TO ALP-BW1.
           MOVE NUM-BW2 TO ALP-BW2.
           MOVE NUM-BW3 TO ALP-BW3.
           MOVE "-" TO ALP-HYPHEN1 ALP-HYPHEN2.
       SEND-HEAD-1.
           MOVE "S  A000HEAD:S." TO VDU.
           MOVE HEAD-1 TO VDUBUF.
           PERFORM SCREENIO.
       SEND-WSEL-LEGEND.
           MOVE MESS TO WR-BUF.
           MOVE WSEL-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.
       SET-MESSAGE-ATTR.
           IF ( LN-MESSAGE = SPACES )
              MOVE "MESSAGE."              TO MESSAGE-LIST-ATTR
           ELSE
           IF ( GP-MESSAGE-ATTR-FLASH )
              MOVE "MESSAGE:F."            TO MESSAGE-LIST-ATTR
           ELSE
           IF ( GP-MESSAGE-ATTR-REVERSE )
              MOVE "MESSAGE:R."            TO MESSAGE-LIST-ATTR
           ELSE
           IF ( GP-MESSAGE-ATTR-BOTH )
              MOVE "MESSAGE:FR."           TO MESSAGE-LIST-ATTR
           ELSE
              MOVE "MESSAGE."              TO MESSAGE-LIST-ATTR.

        CHECK-FOR-REAFFIRMATION.
           MOVE "XX" TO BK-REAFF-STAT.
           MOVE LN-OWNBR  TO LTB-BRNO.
           MOVE LN-ACCTNO TO LTB-ACCTNO
           MOVE 1   TO LTB-SEQNO.
           PERFORM OPEN-LTB1-FILE.
           PERFORM READ-LTB1-FILE.
           PERFORM CLOSE-LTB1-FILE.
           IF IO-GOOD
              IF NOT (LTB-REAFF-DATE = 0)
                 AND (LTB-DISCHARGE-DATE = 0)
              MOVE "00" TO BK-REAFF-STAT.

       COPY "LIBGB/REBATE.CPY".
       COPY "LIBLP/LPSCHD.CPY".
       COPY "LIBLP/LPPOFF.CPY".
       COPY "LIBLP/LPPOF2.CPY".
       COPY "LIBLP/LPITRM.CPY".
       COPY "LIBLP/LPAMTS.CPY".
       COPY "LIBLP/LPAPRS.CPY".
       COPY "LIBLP/LPAPRZ.CPY".
       COPY "LIBLP/LPUPER.CPY".
       COPY "LIBLP/LPPDUE.CPY".
       COPY "LIBLP/LPAVCR.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBWI/PFSWINDOWS.CPY".
       COPY "LIBGB/RBACT.CPY".
       COPY "LIBLP/ADDONSPR.CPY".
       COPY "LIBLP/POSTDATE.CPY".
       COPY "LIBGB/TIMEIO.CPY".

      ********************************************
       IO-ROUTINES SECTION.
      ********************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/DECLARE_SQL.CPY".
           PERFORM CLOSE-SP1-FILE.
           PERFORM CLOSE-LTP1-FILE.
           PERFORM CLOSE-LTI1-FILE.
           PERFORM CLOSE-LTC1-FILE.
           PERFORM CLOSE-LTB1-FILE.
           PERFORM CLOSE-LTA1-FILE.
           PERFORM CLOSE-LN1-FILE.
           PERFORM CLOSE-LM1-FILE.
           PERFORM CLOSE-LC1-FILE.
           PERFORM CLOSE-BW1-FILE.
           PERFORM CLOSE-BM1-FILE.
           PERFORM CLOSE-BR-FILE.
           PERFORM CLOSE-GB-FILE.
       COPY "LIBLP/LPSPGS_SQL.CPY".
       COPY "LIBLP/LPLTPGS_SQL.CPY".
       COPY "LIBLP/LPLTIGS_SQL.CPY".
       COPY "LIBLP/LPLTCGS_SQL.CPY".
       COPY "LIBLP/LPLTBGS_SQL.CPY".
       COPY "LIBLP/LPLTAGS_SQL.CPY".
       COPY "LIBLP/LPLNGS_SQL.CPY".
       COPY "LIBLP/LPLMGS_SQL.CPY".
       COPY "LIBLP/LPLCGS_SQL.CPY".
       COPY "LIBLP/LPBWGS_SQL.CPY".
       COPY "LIBLP/LPBMGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBGBGS_SQL.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBGB/GBGBRN.CPY".
       COPY "LIBLP/LPLC1RN.CPY".
       COPY "LIBLP/LPSP1RN.CPY".
       COPY "LIBLP/LPLN1RN.CPY".
       COPY "LIBLP/LPBW1RN.CPY".
       COPY "LIBLP/LPBM1RN.CPY".
       COPY "LIBLP/LPLTA1RN.CPY".
       COPY "LIBLP/LPLTB1RN.CPY".
       COPY "LIBLP/LPLTC1RN.CPY".
       COPY "LIBLP/LPLTP1RN.CPY".
       COPY "LIBLP/LPLTI1RN.CPY".
       COPY "LIBLP/LPLM1IN.CPY".
       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO ERRCD.

      ********************************************
       END-ROUTINE SECTION.
      ********************************************
       END-RUN.
           IF LN-REC NOT = SPACES
              MOVE LN-ACCTNO TO LOAN-KEY
              MOVE BW-SSNO TO BORROWER-KEY.
           PERFORM CLOSE-FILES.

       EXIT-PROG.

           IF HOLD-EXT-FILPATH NOT = EXT-FILPATH
              MOVE HOLD-EXT-FILPATH TO EXT-FILPATH
              SET ENVIRONMENT "FIL" TO EXT-FILPATH-FIL.

           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN
           DESTROY CPINQ-SCREEN
           DESTROY CPINQ-WINDOW-HANDLE.

           MOVE PROG-BUF TO CPINQ-PROG-BUF.
       STOP-RUN.
           EXIT PROGRAM.
