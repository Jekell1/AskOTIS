       IDENTIFICATION DIVISION.
       PROGRAM-ID.      LONPF0.
       DATE-WRITTEN.    013184.
      ****************************************************************
      *                 (LP)
      *  DESCRIPTION:   LOAN PROCESSING
      *                 TRANSACTION POSTING
      *                 PROGRAM 0 - TRANSACTION HEADER
      * REV:
      *  SLC 042995 ADDED PLTYPE LOGIC.
      *  BAH 050995 ADDED COMAKER DISPLAY, BEFORE IT TESTS FOR SPOUSE
      *  JTG 090495 ADDED TEST FOR GP-NO-LC-IN-ARREARAGE
      *  JTG 013196 STOPPED FLASHING HIGH CREDIT WHEN CREDIT LIMIT = 0
      *  JTG 030696 ALLOWED ZERO ACCOUNT NO (LN-ACCTNO) FROM F6 SCAN
      *  MJD 042396 CHANGED TO ACCEPT 8 DIGIT ACCOUNT NUMBER FROM BWLNSC.
      *             REPLACED LNDISP WITH MORELN WINDOW.
      *  MJD 050396 REQ #E114. WHEN UPDATEING INTEROFFICE PAYMENTS, AFTER
      *             PAYMENY IS UPDATED, USER IS RETURNED TO INTERRUPT SCREEN.
      *             IF POSTING INTER BRANCH FULL-DISPLAY FLAG IS SET
      *             TO 'B' IN LONPF1.C
      *  MJD 071096 CHANGED DISPLAY OF PDTH. IF MONTHLY: MMYY OR MM/DD/YY.
      *             IF WEEKLY: MMDD OR MM/DDWYY
      *  JTG 071896 ADDED LOGIC FOR LC-CON-PDUE-DAYS & LC-REC-PDUE-DAYS
      *  BLL 072596 MOVED MORELNW SO 03 LEVEL WAS UNDER WINDOWSW
      *  BAH 111496 ADDED AVCR-CURRENT-AVAIL TO DISPLAY FOR WDT
      *             WHEN BR-AVAIL-CREDIT = "S" (335)
      *  MJD 111596 CHANGEING SCREEN DISPLAY OF RECOURSE TO COLLECTOR CODE.
      *  KEC 112596 CHANGED LN-POCD FOR RESCISSION.
      *  JTG 121296 ADDED SEND-MESS FOR RESCISSION RENEWAL STATUS
      *  JTG 010397 CHANGED TEST FOR LCPDTH & LCHG DISPLAY TO NEVER DISPLAY
      *             IF SP-LCFRMLA = " " AND REMOVED TEST ON JUDGEMENT
      *             SINCE ACCOUNTS IN JUDGEMENT CAN CONTINUE AS PRECOMP
      *  MJD 011097 ADDED EXT-WI-CL-PAY-SCHLD-FG.
      *  MJD 050697 ADDED LIBLP/SETCTBL(W).
      *  MJD 051597 ADDED LPPOF2 COPY MEMBER (PREVIOUSLY WAS PART OF LPPOFF)
      *  JTG 052097 ADDED 'LIBLP/ADDONSPR' TO GET CORRECT SPR RE: INS RB
      *   CS 052097 ADDED DISPLAY OF CREDIT SCORE IF GREATER THAN ZERO
      *  BAH 061897 ADDED DISPLAY OF EXPANDED COLLATERAL IF COLLID IS BLANK
      *  MJD 082297 CHANGED LN-REC/CONPROFILE TO LN-REC/CONPROFILE(1)
      *  GLF 090397 REPLACED 'IO' ON GP-FILE WITH GET 'GPENV'
      *  KEC 091897 ADDED 'DELINQ-SETTING' AND 'DELINQ-SETTING-LIST' IN
      *              ORDER TO KEEP TRACK OF THE DELINQUENCY PROFILE.
      *  GLF 101097 ADDED CNTRL 'N' TO CALL UP MAKER WINDOW
      *  GLF 101797 ADDED MOVE LN-APPNO TO EXT-WI-CL-APPNO
      *  BAH 032098 INTERRUPT PAYMENT POSTING (F5-PAYOFF) FROM LOAN INPUT,
      *             THE PB/D ACCT # WAS BEING PASSED ON, ZERO OUT LOAN-KEY
      *             AND CLEAR WK-SAVE-RECORD (#56 WORLD)
      *  BAH 062598 ALLOW F7-EXIT FROM ENTER DELINQUENCY CODES (CRD)
      *  MJD 071698 CHANGED TO DISPLAY AMOUNT DUE ON PAYMENT IN
      *             REVERSE VIDEO IF WORLD AND LC-PROF1 = "D" OR
      *             LC-PROF2 = "D"
      *  JTG 121698 DISPLAYED 'R' VIA UNITPER-MASK LOGIC FOR REVOLVING ACCOUNT
      *  BAH 001004 DISPLAY BW-REPCODE AFTER NAME FOR TAX PREREG WORLD PR# 240
      *  BAH 010628 DISPLAY "OO" IF BW-PRIVACY-OPT-OUT = "Y", TFC PR# 90
      *  CS  010823 DISPLAY "OO" ONLY WHEN GP-BW-PRIVACY-OPT-OUT = "Y" AND
      *             BW-PRIVACY-OPT-OUT = "Y"
      *  JTG 010904 ALLOWED IB TYPE LOANS TO ACCELERATE     JIM R. (ALL) PR#1582
      *  BLV 011101 ADD DISPLAY OF WCBC CREDIT LIMIT, WORLD PR# 283.  NOTE:
      *             I ADDED A 1 CHARACTER FIELDS (HFILLER) TO FORM, BUT NEW
      *             FORM ONLY NEEDED IF GP-I-AM-WORLD
      *  BAH 030602 REMOVE "AVAILABLE:" ON RIGHT HAND SIDE IF BR-AVAIL-CREDIT
      *             = "S" (FINMGM) AND CLASS 5, FINMGM PR# 2086
      *  CS  030620 VERIFY PASSWORD IF LN-BNKRPTDATE NOT ZERO [REGACC PR#31]
      *  JTG 030701 ACCOUNTS BYPASS LATE CHARGE ASSESS ROUTINE WHEN
      *             SP-LCFRMLA = ' ' BUT ACCOUNT IS SETUP FOR DEALER LATE
      *             CHARGE FORMULA                               MIDATL #J027
      *  BAH 040225 ADDED GP-CHBORR-PAST-DUE-DAYS TO TEST DELINQUENT CUSTOMERS
      *             AND FORCE THEM INTO CHBORR TO VERIFY INFO, WORLD PR# 337
      *  BAH 040427 WORLD WANTS TO RESTRICT ABOVE CHANGE TO JUST THE BRANCH
      *             PERSONNEL, DONT PULL UP FOR MANAGERS, SUPS, ETC..PR# 349
      *  BAH 050426 WAS NOT DISPLAYING LOAN ENDORSER, ONLY JOINT, THRIFT PL# 482
      *  BAH 050713 ADDED GP-HIDE-FIRST-5-SSNO TO "X" OUT FIRST 5 OF SSNO IF
      *             SET TO "Y", REGACC PR# 109
      *  BAH 051206 I BROKE THE F5 SCAN THEN F6 TO CONTINUE SCANNING THAT WAY
      *             WHEN I MADE THE 050426 CHANGE, I READ THE ENDORSERS AND
      *             JOINTS, THEN WHEN IT WENT BACK TO SCAN, THE READ-NEXT
      *             DID NOT WORK. I NOW ADDED A START AND READ NEXT IN THE
      *             EXIT OF DISPLAY BORROWER IF YOU WERE SCANNING, MODERN !
      *  BAH 060111 THIS F5 SCAN THING STINKS !! ENDORSERS AND JOINT CAN NOT
      *             DISPLAY WHEN USING F5 CAUSE YOU LOSE YOUR SCAN POINTER
      *             IF YOU READ IN THE END OR JOINT, IF 3 MARY JOHNSONS, YOU
      *             CANT GET BACK TO THE SECOND ONE, IT WILL KEEP STARTING
      *             ON THE FIRST MARY JOHNSON, DONT KNOW HOW TO GET AROUND
      *             IT, SO IF YOU USE THE F5, YOU WONT SEE JOINTS OR END !!
      *  BAH 060208 REMOVE "AVAILABLE:" ON RIGHT HAND SIDE IF BR-AVAIL-CREDIT
      *             = "S" (FINMGM) AND CLASS 8, FINMGM PR# 2702
      *  BLF 060612 IF LN-ACH = "Y" & GP-LOAN-ACH-PROCESSING = "Y"
      *             DISPLAY INVERSE PROMPT "PAY ACH" INSTEAD OF
      *             "PAYMENT", NEWS PR# 2711
      * BAH 070504 DISPLAY NEW INSURANCE TRACKING FIELDS FROM THE LT-A RECORD
      *            IN PLACE OF THE BM PRIORITY MEMO FOR REGACC PR# 173
      * BAH 071016 CHANGE DELCNT FIELD TO BE N(11) AND REDEFINE IT FOR REGACC
      *            FORM ALSO CHANGED. FOR NEW LN-PART-RECOURSE-DATE FIELD
      *            REGACC PR# 184
      *  BAH 080722 DO NOT DISPLAY AVAIL CREDIT FOR WORLD IF SCFRMLA 18 AND
      *             CASH AVAIL < 10 % OF AMTFIN, WORLD PR# 593
      *  BAH 081120 REPLACE THE OV: PROMPT WITH A FIELD (P19) AND DISPLAY
      *             IR: FOR GP-I-AM-REGACC IF LN-REDURATE EXISTS, REGACC PR#229
      *  BLM 091201 IF GP-I-AM-WORLD & BW-LOANS > 1, AFTER ACCT IS SELECTED
      *             OPEN THE BWLNS2 WINDOW TO DISPLAY ALL THE OPEN LOANS
      *             THE BORROWER HAS.  WORLD PR# 664
      *   CS 110301 WHEN ACCOUNT IN CEASE&DESIST STATUS(NEW STATUS CREATED FOR
      *             MODERN VIA CHANGES MENU) DISPLAY THE ACCOUNT NUMBER IN
      *             REVERSE.  PR#3720 MODERN
      *  BAH 110328 IF GP-USE-SCRA "Y" AND LS-R-SCRA-ACTIVE "Y", DISPLAY "SCRA"
      *             IN THE HIGH CREDIT AREA, REGACC PR# 289
      *  KEC 120212 {PR#03978} PARADATA <A15> [DAVID SINCLAIR]
      *             CREATED THE ABILITY TO CHOOSE THE WAY THE 'MESSAGE'
      *             IS DISPLAYED ON THE ACCOUNT INQUIRY, PAYMENT
      *             POSTING AND COLLECTION WORK SCREENS.
      *  BAH 120522 ADDED DEBTAID-SEQ TO PASSWORD DISPLAYING DEBTAID ACCOUNTS
      *                                              REGACC PR# 311
      *  BAH 120727 ADDED CLASS 11 TO BR-AVAIL-CREDIT "S" IF STATEMENT,
      *                                                       FINMGM PR# 4085
      *  MJD 121214 ADDED TCLP-MAIN-WINDOW-HANDLE TO DISPLAY SCREEN.
      *             REPLACED DESTROY LONPF-SCREEN WITH DESTROY ALL CONTROLS,
      *             THIS IS THE ONLY WAY I HAVE FOUND TO DATE TO KEEP
      *             THE MENU DISPLAYING PROPERLY WHEN EXITING THE POSTING
      *             SCREEN AFTER BRINGING UP A LOAN.
      *  BAH 130111 ADDED LC-DISPLAY-ERN OPTION OF "I" FOR POFF-INTDUE
      *                                              AMERICAN ACC PR# 4190
      *  BAH 130819 DONT DISPLAY "OO" FOR WORLD BW-PRIVACY-OPT-OUT PR# 919
      *  BAH 130903 ADDED BR-AVAIL-CREDIT "B" TO DISPLAY "BRANCH", BR-NO
      *             AND HIGHLIGHT (REVERSE) HONOR PR# 4358
      *  BAH 140811 FIXED "___ APPLIED ___"  BEING IN CAPS, C$TOLOWER
      *  BAH 140812 REDISPLAY TRDATE IN FORM-RESET, F1 WAS CLEARING
      *  BLM 140813 IN FORM-RESET, MOVE TRANS-DATE TO LONPF-TRDATE INSTEAD
      *             OF SEPARATE REDISPLAY
      * BAH 140814 ADDED TEST FOR LPSMNU IN HELP
      * MJD 140829 MOVED DESTROY ALL CONTROLS TO FORM-RESET AND
      *            REMOVED FROM END OF PROGRAM. SEE MORE DETAILED COMMENTS
      *            IN FORM-RESET SECTION.
      *  BAH 160118 SPLIT OUT LTFILE, PD0003
      *  BAH 160209 SPLIT OUT LSFILE, PD0003
      *  BAH 160419 DISPLAY REAFFIRMED AND DISCHARGED FOR WORLD PR#1150
      *
      * KEC 2016.0509 {PD#00003} PARADATA <A30> [JAY CISZEWSKI]
      *         CHANGED LN-APPNO TO USE LN-APP-NUMBER FOR MA1-KEY.
      *         CHANGED MA-APPNO TO USE MA-APP-NUMBER FOR MA1-KEY.
      *  BLM 160621 ADD GETLTRW, REMOVE MORELN, LN-SEQNO, PD0003
      *  BAH 161102 REMOVED TEST-IF-SC-RESTRICTION, PUT IN LPAVCR WORLD 1199
      *  BLM 170126 IN WORLD-CREDIT-DISPLAY, IF ACCOUNT WOULD BE A PB
      *             DELINQUENT IF RENEWED TODAY, DISPLAY A "D" NEXT TO
      *             THE AVAILABLE CREDIT, WORLD PR# 1220
      *  BAH 170313 ACTIVATE 8 DIGIT DATES, PD0006
      *  BAH 170320 DISPLAY LT-B-DISCHARGE-DATE WITH DISCHARGE MESSAGE FROM
      *             ABOVE CHANGE WORLD PR#1150, WORLD PR#1234
      *   CS 170321 IF WORLD, IF ACCOUNT IS 'BK' PERFORM VERIFY-PASSWORD, IF
      *             STAT NOT 'OK', DISPLAY FULL SCREEN WINDOW, IF THE USER 
      *             TYPES 'Y' TO CONTINUE INTO ACCOUNT, CREATE A LOAN MEMO
      *             "BK ACCOUNT ACCESSED" WITH USERID /TIME. ALSO CREATE A 
      *             LOG FILE /USR/DATA/R?/LG WORLD PR#1234
      *   CS 170323 MOVED TEST ON PASSWORD FOR BK(SEQ1) AND DEBTAID. THE PASS
      *             WORD PROMPTED IN DISPLAY-LOAN BUT IF INVALID SET YOU OUT
      *             OF THE SECTION TO ENTER-KEYS SECTION PR#1234 WORLD
      *   CS 170411 CHANGED TO PROMPT FOR THE PASSWORD FOR VERIFY-PASSWD-WORLD
      *             IT WAS SPACES  WORLD PR#1234
      *             REMOVED 'D' PBD FROM WORLD CREDIT DISPLAY IF 'BK
      *   CS 170519 CHANGED PROMPT FOR PASSWORD, IF THERE IS A REAFFDATE AND 
      *             THE DISCHARGE DATE IS 0, DONT VERIFY PASSWD PR#1234A
      *                                                        WORLD
      *  BAH 170609 ACTIVATE 8 DIGIT DATES, PD0006
      *  KEC 20180824 LOAN APP MODULE REMOVAL FROM A30 [LARM30];
      *               LEAVING LN-APP-NUMBER & EXT-WI-CL-APP-NUMBER
      *               HERE IN CASE NEEDED IN THE FUTURE.
      *  BAH 20180928 GLOBAL BWFILE
      *  BAH 20181210 GLOBAL LNFILE
      *  KEC 20190219 A32 REMOVED REDEFINE FOR LN-LC-CODEX; UNUSED
      *  BAH 20190701 REMOVED AVAILABLE-CREDIT FROM LPPFBUF
      *   CS 20191231 IN PERFORM CHECK-FOR-REAFFIRMATION WAS SETTING UP
      *               SEQ 5 TO GET THE LTB(WAS SEQ 5 IN A15), ITS NOW
      *               1 RECORD SO USE SEQ1
      *  BAH 20211209 F5-PAYOFF FROM WITHIN XONPC0 WAS GETTING A READ-WK-FILE
      *               ERROR DUE TO LONPF0 WIPING OUT THE WK-FILE. HAD TO DO
      *               AN ACCESS CALL BEFORE OPENING TO OUTPUT. THIS ALL DUE TO
      *               US REMOVING THE FLAG THAT OPEN I-O WOULD CREATE A FILE.
      *               THIS IS IN THE SET-INTERRRUPT-INFO SECTION.
      * BAH 2022.0314 REMOVED EXT-SPANISH-FORMS AND ALL MX FORMS
      * BAH 20220818 REMOVED GLOBRD, READ GB-FILE INSTEAD
      * BAH 20230713 COMMENT OUT 3RD AND 4TH BORROWER TESTS, LN6 AND LN7
      * BAH 20240202 REMOVED CRNOR2 AND COMPUTE PDUE-DAYS
      * BAH 20240311 REMOVED EXT-WI-CL-APP-NUMBER
      * BAH 20240417 CALL CHBORR INSTEAD OF A WINDOW, REMOVED CHBORRW
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "LIBLP/LPFSWK.CPY".
       I-O-CONTROL.
       DATA DIVISION.
       FILE SECTION.

       COPY "LIBLP/LPFDWK.CPY".

       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)LP/LONPF0.CBL S35 02/20/24-15:26:07 >".
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
       COPY "LIBLP/LP01LTA.CPY".
       COPY "LIBLP/LP01LTA_SQL.CPY".
       COPY "LIBLP/LPWSLTA.CPY".
       COPY "LIBLP/LP01LTB.CPY".
       COPY "LIBLP/LP01LTB_SQL.CPY".
       COPY "LIBLP/LPWSLTB.CPY".
       COPY "LIBLP/LP01LTC.CPY".
       COPY "LIBLP/LP01LTC_SQL.CPY".
       COPY "LIBLP/LPWSLTC.CPY".
       COPY "LIBLP/LP01LTI.CPY".
       COPY "LIBLP/LP01LTI_SQL.CPY".
       COPY "LIBLP/LPWSLTI.CPY".
       COPY "LIBLP/LP01LTP.CPY".
       COPY "LIBLP/LP01LTP_SQL.CPY".
       COPY "LIBLP/LPWSLTP.CPY".
       COPY "LIBLP/LP01LM.CPY".
       COPY "LIBLP/LP01LM_SQL.CPY".
       COPY "LIBLP/LPWSLM.CPY".

       01  WK-IFN            PIC X(2)   VALUE "WK".

       01  WK-KEY            PIC 9(3).

       01  BK-REAFF-STAT     PIC X(2) VALUE "XX".
       01  IO-TYPE           PIC X(3).
       01  ELE               PIC 99 VALUE 0.
       01  DIS1              PIC 9     VALUE 0.
       01  FIRST-TIME        PIC 9     VALUE 0.
       01  FIRST-SCAN        PIC 9     VALUE 0.

       01  DEFAULT1-KEY      PIC X.
       01  SCAN-FLAG         PIC 9     VALUE 0.
       01  SCAN              PIC X(2)  VALUE " ".
           88  SCAN-ON       VALUE "ON".
           88  SCAN-OFF      VALUE " ".
       01  NAME-WORKER       PIC X(40) VALUE " ".
       01  NAME              PIC X(16) VALUE " ".
       01  TYP               PIC 9     VALUE 0.
       01  BORROWER-SCAN     PIC X     VALUE " ".
       01  HOLD-BW-SSNO      PIC 9(11) VALUE 0.
       01  ARR1              PIC 99    COMP-3.
       01  ARR2              PIC 99    COMP-3.
       01  SUB               PIC 9.
       01  S1                PIC 99.

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
       01  BW-ALP-TABLE.
           03  BW-ALP-TBL    PIC X     OCCURS 70.

       01  PROFILE-EDIT      PIC 9(12).
       01  PROFILE-ED        REDEFINES PROFILE-EDIT PIC X(12).

       01  DELINQ-SETTING          PIC X(2)   VALUE "LC".

       01  DELINQ-SCAN             VALUE "X  ".
           03  DELINQ-PROF1        PIC X.
           03  DELINQ-PROF2        PIC X.
           03  DELINQ-PROF3        PIC X.

       01  DELINQ-VALUE.
           03  DELINQ-NUM          PIC ZZZZ,ZZ9.99-  BLANK ZERO.
       01  DELINQ-BUF.
           03  FILLER              PIC X(3) VALUE " ".
           03  DELINQ-CODES        PIC X(3) VALUE " ".
       01  DELINQ-CODE             PIC X.
       01  PBD-FLAG                PIC X.

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

      ********************************
      *   S C R E E N   B U F F E R  *
      ********************************
      *01  HIGH-BUF.
      *    03  HIGH-CREDIT-X-BUF         PIC X(13).
      *    03  HIGH-CREDIT-BUF           PIC Z(6)B.
      *    03  AVA-BUF                   PIC X(11).
      *    03  AVAIL-BUF                 PIC Z(7)9.99B.
      *    03  AVAIL-SPACE               REDEFINES AVAIL-BUF.
      *        05  FILLER                PIC X(11).
      *        05  AVAIL-BUF-RESTRICT-FG PIC X.

      *01  HIGH-LIST         PIC X(16) VALUE "H22 H23 H24 H25.".
      *01  HIGH-LIST-ATTR    PIC X(25) VALUE
      *                               "H22:R H23:R H24:R H25:RF.".
      *01  HIGH-LIST-S5      PIC X(25) VALUE "H22:R H23:R H24 H25.".
      *01  HIGH-LIST-SCRA    PIC X(18) VALUE
      *                               "H22:R H23 H24 H25.".

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

       01  OPT-OUT-LIST      PIC X(6) VALUE "OO:RF.".

       01  DISPLAY-LOAN-BUF  VALUE " ".
           03  D-LOAN-NUM    PIC X(16).
           03  D-MAKER       PIC X(8).
           03  D-H1          PIC 99/99/99  BLANK ZERO.
           03  D-H2          PIC X(8).
           03  D-H3          PIC 99/99/99  BLANK ZERO.
           03  D-H4          PIC ZZZZ9.99  BLANK ZERO.
           03  D-H5          PIC 99/99/99  BLANK ZERO.
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
           03  D-H17         PIC 9999.
           03  D-H18         PIC 99.
           03  D-H181        PIC 99        BLANK ZERO.
           03  D-H19         PIC ZZ9.999.
           03  D-H20         PIC X(4).
           03  D-DLRNUM      PIC 9(4).99.
           03  D-EXTRCT      PIC X(8).
           03  D-DELCNT      PIC ZZZ.
           03  D-COLCTAG     PIC X(6).
           03  D-COLCTCD     PIC X(3).
           03  D-COLLCD      PIC X(2).
           03  D-COLLID      PIC X(20).

       01  DISPLAY-BAL-BUF   VALUE " ".
           03 D-PDTHRU-MO    PIC 99.
           03 D-PDTHRU-DAYR  PIC 99.
           03  D-OTHBAL      PIC Z(5).99-.
           03  D-INTBAL      PIC Z(4).99-.
           03  D-CURBAL      PIC Z(5)9.99.

       01  PMT-PROMPT-LIST   PIC X(12) VALUE "PMTPROMPT:D.".
       01  ACH-PROMPT-LIST   PIC X(12) VALUE "PMTPROMPT:R.".
       01  PMT-PROMPT-BUF    PIC X(7)  VALUE "PAYMENT".
       01  ACH-PROMPT-BUF    PIC X(7)  VALUE "PAY ACH".

       01  D-MESSAGE         PIC X(30).

       01  MESSAGE-LIST-ATTR PIC X(11) VALUE "MESSAGE:R.".
       01  MESSAGE-ATTR      PIC XX.

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
                   07  D-NAME-WORLD   PIC X(25).
                   07  FILLER         PIC X.
                   07  D-REP-WORLD    PIC X(3).
                   07  FILLER         PIC X(5).
           03  D-SP0         PIC X(7).
           03  D-SP1         PIC X(11).
           03  D-SP1-X-OUT REDEFINES D-SP1.
               05  D-SP1-1   PIC X(3).
               05  FILLER    PIC X.
               05  D-SP1-2   PIC XX.
               05  FILLER    PIC X.
               05  D-SP1-3   PIC X(4).
           03  D-SCOREPROMPT PIC X(6).
           03  D-CREDIT-SCORE PIC ZZZZ.
           03  D-SP2         PIC X(25).
           03  D-MEMO        PIC X(25).

       01  DISPLAY-BORR-LIST.
           03  FILLER              PIC X(58) VALUE
           "SOC LOANS A1 A2 A3 A4 SP0 SP1 SCORE:S CREDITSCORE SP2 P01.".

       01  DISP-DELINQ-VALUE.
           03 DISP-DELINQ-VALUE-1  PIC X(12) VALUE SPACES.
           03 DISP-DELINQ-VALUE-2  PIC X(12) VALUE SPACES.

       01  DISPLAY-LOAN-LIST.
           03  FILLER              PIC X(50) VALUE
           "LNNO LNSEQ MAKER H01 H02 H03 H04 H05 H06 H07 H071 ".
           03  FILLER              PIC X(51) VALUE
           "H08 H09 B1:S H10 PH11:S H11 H12 PH13:S H13 H14 H15 ".
           03  FILLER              PIC X(21) VALUE
           "H16 H17 H18 H181 H19 ".
           03  FILLER              PIC X(52) VALUE
           "H20 DLRNUM DELCNT:S COLCTAG:S COLCTCD COLLCD COLLID.".

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
           03  FILLER              PIC X(16) VALUE
           "080 090 100 110.".

       01  PROMPT-INFO-LIST.
           03  FILLER              PIC X(30) VALUE
           "PMPT1 PMPT2 PMPT3 PMPT4 PMPT5.".
       01  PROMPT-INITIAL-VALUES.
           03  FILLER              PIC X(33) VALUE
               " DATE  CD REF    AMOUNT   OTHER".
           03  FILLER              PIC X(6) VALUE
               "INT/LC".
           03  FILLER              PIC X(24) VALUE
               "     PRIN PDTH   OTHER".
           03  FILLER              PIC X(6) VALUE
               "INT/LC".
           03  FILLER              PIC X(11) VALUE
               "      PRIN".

       01  LOAN-INFO-LIST.
           03  FILLER              PIC X(30) VALUE
           "PMPT1 PMPT2 PMPT3 PMPT4 PMPT5 ".
           03  FILLER              PIC X(4)  VALUE
           "LN1 ".
           03  FILLER              PIC X(24) VALUE
           "GL8 F21 GD8 F22 GA8 F23 ".
           03  FILLER              PIC X(24) VALUE
           "GL9 F31 GD9 F32 GA9 F33 ".
           03  FILLER              PIC X(24) VALUE
           "GL0 F41 GD0 F42 GA0 F43 ".
           03  FILLER              PIC X(32) VALUE
           "GL1 F51 GD1 F52 GA1 F53 GL2 F61 ".
           03  FILLER              PIC X(40) VALUE
           "GD2 F62 GA2 F63 GL3 F71 GD3 F72 GA3 F73 ".
           03  FILLER              PIC X(24) VALUE
           "GL4 F81 GD4 F82 GA4 F83.".

       01  LOAN-INFO-BUF.
           03  FILLER              PIC X(40) VALUE
               "ACCOUNT #         DATE  LOAN AMT TRM  DE".
           03  FILLER              PIC X(16) VALUE
               "ALER COL PL/JD  ".
           03  DELINQ-PROMPT       PIC X(11).
           03  FILLER              PIC X(13) VALUE
               "  BAL/PDOFF M".
           03  LOAN-INFO-TABLE.
               05  LOAN-INFO-TAB   OCCURS 8.
                   07  LI-LOAN-NUM PIC X(16).
                   07  FILLER      PIC X.
                   07  LI-LNDATE   PIC 9(6).
                   07  LI-LOAN-AMT PIC ZZZZZZ.ZZ.
                   07  FILLER      PIC X.
                   07  LI-TRM      PIC ZZZ.
                   07  FILLER      PIC X.
                   07  LI-DEALER   PIC ZZZZ.ZZ.
                   07  FILLER      PIC X.
                   07  LI-COLL     PIC XX.
                   07  FILLER      PIC X.
                   07  LI-PLFG     PIC X.
                   07  LI-JDPLDT   PIC 9(6)  BLANK ZERO.
                   07  FILLER      PIC X.
                   07  LI-CONTRACT PIC X(12).
                   07  FILLER      PIC X.
                   07  LI-BAL.
                       09  LI-LOAN-BAL PIC ZZZZZZ.ZZ-.
                       09  LI-MKR      PIC X.
                   07  LI-POFF REDEFINES LI-BAL.
                       09  LI-POCD     PIC XX.
                       09  FILLER      PIC X.
                       09  LI-PAIDOFF  PIC 9(6).
                       09  FILLER      PIC XX.
       01  PMPT-LIST.
           03  FILLER          PIC X(30)    VALUE
           "PMPT1 PMPT2 PMPT3 PMPT4 PMPT5.".

       01  DISPLAY-TRACKING.
           03  FILLER           PIC X(10) VALUE "TRK ONLY: ".
           03  D-TRACK          PIC X.
           03  FILLER           PIC X(6) VALUE " INS: ".
           03  D-INS            PIC X.
           03  FILLER           PIC X(6) VALUE " CPP: ".
           03  D-CPP            PIC X.


       COPY "LIBWI/BWLNSCW.CPY".
       COPY "LIBWI/BWLNS2W.CPY".
       COPY "LIBWI/PFSWINDOWSW.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBCL/CLMENUW_EXT.CPY".
       COPY "LIBLP/LPWKPATH.CPY".
       COPY "LIBLP/LP01NO.CPY".
       COPY "LIBLP/LPAVCRW.CPY".
       COPY "LIBLP/LPLCHGW.CPY".
       COPY "LIBLP/LPMDTEW.CPY".
       COPY "LIBGB/AGEINGW.CPY".
       COPY "LIBLP/LPCEPPW.CPY".
       COPY "LIBGB/SPPDTHW.CPY".
       COPY "LIBLP/LPCDELW.CPY".
       COPY "LIBLP/LPLCASW.CPY".
       COPY "LIBLP/LPLCAPW.CPY".
       COPY "LIBLP/LPRATEW.CPY".
       COPY "LIBLP/LPIBPCW.CPY".
       COPY "LIBLP/LPXDTEW.CPY".
       COPY "LIBLP/LPSTATW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBLP/LPPDUEW.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBGB/SYSTEMW.CPY".
       COPY "LIBGB/REBATEW.CPY".
       COPY "LIBLP/LPSCHDW.CPY".
       COPY "LIBLP/LPPOFFW.CPY".
       COPY "LIBLP/ADDONSPRW.CPY".
       COPY "LIBLP/LPEARNW.CPY".
       COPY "LIBLP/LPITRMW.CPY".
       COPY "LIBLP/LPAMTSW.CPY".
       COPY "LIBLP/LPAPRSW.CPY".
       COPY "LIBLP/LPAPRZW.CPY".
       COPY "LIBLP/LPUPERW.CPY".
       COPY "LIBLP/LPUPWKW.CPY".
       COPY "LIBLP/LPMENUW.CPY".
       COPY "LIBLP/POSTDATEW.CPY".
       COPY "LIBGB/RBACTW.CPY".
       COPY "LIBLP/SETCTBLW.CPY".
       COPY "LIBGB/GPENVW.CPY".
       COPY "LIBGB/PASSWDW.CPY".
      * TO STAY CONSISTENT WITH THE INQUIRIES
       78  BK-SEQ            VALUE 1.
       78  DEBTAID-SEQ       VALUE 2.
       78  BK-WORLD-SEQ      VALUE 3.

       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBLP/LONPF_DEF.CPY".
       COPY "LIBLP/LPSMNU_DEF.CPY".
       COPY "LIBLP/LPLNMU_DEF.CPY".
       COPY "LIBLP/LPCAMU_DEF.CPY".
       COPY "LIBLP/LPVDMU_DEF.CPY".
       COPY "LIBWI/WRLDBK_DEF.CPY".
       COPY "LIBLP/LONPF_WKS.CPY".
       COPY "LIBLP/LPSMNU_WKS.CPY".
       COPY "LIBLP/LPLNMU_WKS.CPY".
       COPY "LIBLP/LPCAMU_WKS.CPY".
       COPY "LIBLP/LPVDMU_WKS.CPY".
       COPY "LIBWI/WRLDBK_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".

      /**********************************
      *   C O N T R O L   M O D U L E   *
      ***********************************
       COPY "LIBGB/GETFMW.CPY".
       COPY "LIBLP/LPLOANW.CPY".
       COPY "LIBLP/LPPFBUF.CPY".

       SCREEN SECTION.
       COPY "LIBLP/LONPF_SCN.CPY".
       COPY "LIBLP/LPSMNU_SCN.CPY".
       COPY "LIBLP/LPLNMU_SCN.CPY".
       COPY "LIBLP/LPCAMU_SCN.CPY".
       COPY "LIBLP/LPVDMU_SCN.CPY".
       COPY "LIBWI/WRLDBK_SCN.CPY".

       PROCEDURE DIVISION
             USING FORM-PATHNAME EXIT-PATHNAME PROG-BUF UPDATE-BUF.
       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
              WK-FILE.
       COPY "LIBGB/DECLARE.CPY".
           PERFORM CLOSE-SP1-FILE.
           PERFORM CLOSE-LTP1-FILE.
           PERFORM CLOSE-LTI1-FILE.
           PERFORM CLOSE-LTC1-FILE.
           PERFORM CLOSE-LTB1-FILE.
           PERFORM CLOSE-LTA1-FILE.
      *    PERFORM CLOSE-LN7-FILE.
      *    PERFORM CLOSE-LN6-FILE.
           PERFORM CLOSE-LN5-FILE.
           PERFORM CLOSE-LN4-FILE.
           PERFORM CLOSE-LN1-FILE.
           PERFORM CLOSE-LM1-FILE.
           PERFORM CLOSE-LC1-FILE.
           PERFORM CLOSE-BW4-FILE.
           PERFORM CLOSE-BW3-FILE.
           PERFORM CLOSE-BW2-FILE.
           PERFORM CLOSE-BW1-FILE.
           PERFORM CLOSE-BM1-FILE.
           PERFORM CLOSE-BR-FILE.
           PERFORM CLOSE-GB-FILE.
           CLOSE WK-FILE.
       COPY "LIBGB/DECLAR2.CPY".

      ************************************
       MAIN-PROGRAM SECTION.
      ************************************

           PERFORM SQL-CONNECT.

      D    STOP MESS.
           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "LONPF" TO VDU-FORM-NAME.

           MOVE "C" TO VDU-WIMENU-SELECTION.

      * PASSED IN PROG-BUF FOR WK-FILE TEMP-PATH
           MOVE USER-PID TO TEMP-PID.

           PERFORM FORM-RESET.

           IF INTERRUPT-FG NOT = " "
              PERFORM SET-INTERRUPT-INFO
              IF ERRCD NOT = " "
                 GO TO END-ROUTINE.

      * ALLOW CNTRL 'N' INTERRUPT:
           MOVE "WI/MAKERS" TO VDU-CTRL-N-PROG.

           PERFORM GET-GPENV.

           MOVE BRANCH TO BR-NO.
           PERFORM OPEN-BR-FILE.
           PERFORM READ-BR-FILE.
           PERFORM CLOSE-BR-FILE.
           IF IO-FG NOT = 0
              MOVE "N" TO BR-PRIO-MEMO-FLAG
                          BR-AVAIL-CREDIT
              MOVE 01 TO BR-PRINTER(1).

      * NEED FOR LCAS ROUTINES (REPLACES GLOBRD)
           PERFORM OPEN-GB-FILE.
           MOVE GB-PATH-OWNBR TO GB-BRNO.
           MOVE 1             TO GB-NO.
           PERFORM READ-GB-FILE.
           PERFORM CLOSE-GB-FILE.
           IF IO-BAD
              MOVE "E"   TO ERRCD
              GO TO END-ROUTINE.

      * MUST LEAVE SP1-FILE OPEN BECAUSE ADDONSPR READS IT
           PERFORM OPEN-SP1-FILE.

           PERFORM SEND-DELINQ-SCAN.

           MOVE SPACES TO ERRCD LN-REC.

           PERFORM RESTORE-SCREEN.

           IF ROUTE-CD = "2"
              MOVE " " TO WK-SAVE-RECORD
              MOVE 0 TO LOAN-KEY BORROWER-KEY
              GO TO MAIN-MODULE.

           MOVE "S  8080TRDATE." TO VDU.
           MOVE TRANS-DATE       TO VDU-DATE-YYYYMMDD.
           PERFORM SCREENIO.

           MOVE "S  A020PRU." TO VDU.
           MOVE BR-PRINTER(1) TO VDUBUF.
           PERFORM SCREENIO.

       MAIN-MODULE.
           IF FIRST-TIME = 0
              MOVE " " TO ROUTE-CD
              MOVE 1 TO FIRST-TIME
           ELSE
              PERFORM FORM-RESET.

           MOVE "S  A010FULL." TO VDU.
           MOVE FULL-DISPLAY TO VDUBUF.
           PERFORM SCREENIO.

           PERFORM ENTER-KEYS.
           IF IO-TYPE = "END"
              GO TO END-ROUTINE.
           IF IO-TYPE NOT = "PST"
              GO TO MAIN-MODULE.

           PERFORM GET-LC.

           MOVE "S  A010TYP." TO VDU.
           MOVE SPACES        TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "S  A160SORTKEY." TO VDU.
           MOVE LC-DESC           TO VDUBUF.
           PERFORM SCREENIO.

           MOVE MENU-PROG-FORM(4) TO NEW-PROG-FORM.
           MOVE 1                 TO NEW-PROG-SUB.
           MOVE "P"               TO ERRCD.
           PERFORM SAVE-SCREEN.

           IF BW-LOANS > 1
              PERFORM DISPLAY-OTHER-LOANS-WINDOW.

           PERFORM SET-COLLECTOR-INFO.

           IF GP-CHBORR-PAST-DUE-DAYS NUMERIC
              IF GP-CHBORR-PAST-DUE-DAYS NOT = 0
      * WORLD ONLY WANTS BRANCH PERSONNEL TO GET THIS, NOT MANAGERS, ETC..
      * Y,V,S,R SHOULD GET IT, NOTHING BEFORE "R" SHOULD GET IT
                 IF ( EXT-CONAME-USER-PROFILE-CD < X"72")
                    NEXT SENTENCE
                 ELSE
                    MOVE TRANS-DATE TO PDUE-TODAY-DATE
                    MOVE LN-1STPYDATE TO PDUE-1STPYDATE
                    PERFORM PAST-DUE-DAYS-CALCULATION
                    MOVE PDUE-DAYS TO NO-PAST-DUE-DAYS
                    IF PDUE-DAYS NOT < GP-CHBORR-PAST-DUE-DAYS
                       MOVE
                 "ACCOUNT DELINQUENT, VERIFY INFO && REFER TO MANAGER"
                            TO MESS PERFORM SEND-MESS
                       MOVE "WI/CHBORR" TO FORM-NAM
                       CALL FORM-PROGX USING FORM-PATH EXIT-PATHNAME
                       CANCEL FORM-PROGX.

           GO TO END-ROUTINE.

      ***************************************************
       ENTER-KEYS SECTION.
      ***************************************************
       ENTER-KEYS-NO.

      * PLEASE DO NOT CLOSE THE BW1 FILE...................
      * THE SORT-SCANKEY AND OTHER AREAS NEED IT OPEN
           PERFORM OPEN-BW1-FILE.

           IF LOAN-KEY NOT = 0
              MOVE LOAN-KEY TO LN-ACCTNO
              MOVE 0        TO LOAN-KEY
              MOVE SPACES   TO WK-SAVE-RECORD
              GO TO READ-LNFILE1.

           IF INTERRUPT-FG = " "
              IF WK-SAVE-RECORD NOT = " "
                 MOVE WK-SAVE-RECORD TO LN-REC
                 MOVE 0              TO LOAN-KEY
                 MOVE SPACES         TO WK-SAVE-RECORD 
                 GO TO READ-LNFILE1
              ELSE
                 NEXT SENTENCE
           ELSE
              MOVE "S  A170ONLN:FR."  TO VDU
              MOVE " PAYMENT POSTING" TO VDUBUF
              PERFORM SCREENIO.

           MOVE SPACES TO IO-TYPE LN-REC.

           MOVE "F7-EXIT, F3-FULL/BRIEF, F4-DLNQ, F5 OR F6-SCAN"
                                                       TO MESS.
           PERFORM SEND-LEGEND.

           MOVE "EN N140LNNO." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>"
              IF INTERRUPT-FG = "Y"
                 MOVE "END" TO IO-TYPE
                 MOVE "E"   TO ERRCD
      * CLEAR THE SAVED WK RECORD SO THE ACCOUNT # DOES NOT AUTO COME UP
      * NEXT TIME
                 MOVE 0 TO LOAN-KEY BORROWER-KEY
                 PERFORM CLEAR-WK-FILE
                 GO TO END-ENTER-KEYS
              ELSE
                 IF INTERRUPT-FG = " "
                    GO TO MENU-KEY.
           IF VDUSTATUS = "16"
              MOVE "S  A010FULL." TO VDU
              PERFORM SET-FULL-BRIEF
              PERFORM SCREENIO
              PERFORM SEND-MESS
              GO TO ENTER-KEYS-NO.
           IF VDUSTATUS = "18"
              PERFORM GET-DELINQ-CODES
              GO TO ENTER-KEYS-NO.
           IF VDUSTATUS = "1:"
              PERFORM ENTER-SCANKEY
              IF IO-TYPE = "CLR"
                 PERFORM FORM-RESET
                 GO TO ENTER-KEYS-NO
              ELSE
                 GO TO END-ENTER-KEYS.
           IF VDUSTATUS = "1<"
              PERFORM BWLN-SCAN
              IF IO-FG NOT = 0
                 MOVE " " TO WK-SAVE-RECORD
                 MOVE 0 TO LOAN-KEY
                 GO TO ENTER-KEYS-NO
              ELSE
                 GO TO READ-LNFILE1.
           IF VDUSTATUS NOT = "00"
              GO TO ENTER-KEYS-NO.
           IF VDUNUM0 = 0
              GO TO ENTER-KEYS-NO.

           MOVE BR-NO   TO LN-OWNBR.
           MOVE VDUNUM0 TO LN-ACCTNO.

       READ-LNFILE1.
           MOVE PROMPT-INITIAL-VALUES TO WR-BUF.
           MOVE PMPT-LIST             TO WR-LIST.
           PERFORM CALL-WRFORM.

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

           IF LN-DEBTAIDATE NOT = 0
              MOVE DEBTAID-SEQ  TO PASSWDW-SEQ
              PERFORM VERIFY-PASSWORD
              IF PASSWDW-STAT NOT = "OK"
                 MOVE SPACES TO DISPLAY-BORR-BUF
                 PERFORM FORM-RESET
                 MOVE SPACES TO IO-TYPE
                 GO TO ENTER-KEYS-NO.

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
                     DESTROY WRLDBK-SCREEN 
                     MOVE "LONPF" TO VDU-FORM-NAME
                     IF VDUBUF NOT = "YES"
                        MOVE " " TO DISPLAY-BORR-BUF
                        MOVE DISPLAY-BORR-BUF  TO WR-BUF
                        MOVE DISPLAY-BORR-LIST TO WR-LIST
                        PERFORM CALL-WRFORM 
                        PERFORM FORM-RESET
                        GO TO ENTER-KEYS-NO
                     ELSE
                        PERFORM CREATE-BK-ACCESSED-MEMO
                        PERFORM CREATE-BK-LOG.

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
              MOVE 0            TO BORROWER-KEY
           ELSE
              MOVE LN-SSNO(1) TO BW-SSNO.

           PERFORM READ-BW1-FILE.
           IF IO-FG NOT = 0
              MOVE SPACES           TO BW-REC
              MOVE 0                TO BW-SSNO
              MOVE "BORROWER "      TO BW-LNAME
              MOVE "DOES NOT EXIST" TO BW-FNAME.

           MOVE "PST" TO IO-TYPE.
           PERFORM DISPLAY-BORROWER.

           PERFORM DISPLAY-LOAN.

           GO TO END-ENTER-KEYS.

       MENU-KEY.

           DESTROY LONPF-SCREEN.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.

           PERFORM MENU-SELECTION.

           MOVE 0 TO ELE.

           PERFORM SAVE-SCREEN.
           IF MENU-FG = 99
              GO TO ENTER-KEYS-NO.

           MOVE "END" TO IO-TYPE.

       END-ENTER-KEYS.
           EXIT.

      ***********************************************
       OPEN-BK-MSG-WINDOW SECTION.
      ***********************************************
           MOVE 20 TO WINDOW-LINES.
           MOVE 75 TO WINDOW-COLUMNS.
           MOVE 2  TO WINDOW-BEGIN-Y.
           MOVE 2  TO WINDOW-BEGIN-X.
           MOVE "WRLDBK"    TO VDU-FORM-NAME.
           MOVE "WI/WRLDBK" TO WINDOW-FORM-NAM.
           PERFORM OPEN-WINDOW.

           DISPLAY WRLDBK-SCREEN.

       OPEN-BK-MSG-WINDOW-EXIT.    
           EXIT.

      ***********************************************
       CREATE-BK-ACCESSED-MEMO SECTION.
      ***********************************************
           MOVE SPACES TO HOLD-BK-MEMO.

           PERFORM OPEN-LM1-FILE.
           MOVE "BK ACCOUNT ACCESSED" TO HOLD-BK-TEXT.
           PERFORM SET-POSTDATE.
           MOVE POST-USERID           TO HOLD-POST-USERID.
           PERFORM GET-TIME.
           MOVE TIME-EDIT             TO HOLD-POST-TIME.
           MOVE LN-OWNBR              TO LM-BRNO.
           MOVE LN-ACCTNO             TO LM-ACCTNO.
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

      ***********************************************
       CREATE-BK-LOG SECTION.
      ***********************************************
           MOVE "B"                TO ERRLOGW-ACTION.
           MOVE LN-OWNBR           TO ERRLOGW-BRNO.
           MOVE LN-ACCTNO          TO WORK-ERRLOG-NUMBER.
           MOVE HOLD-BK-TEXT       TO WORK-EL-BK-TEXT.
           MOVE TRANS-DATE         TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYYYY.
           MOVE DATE-MMDDYYYY      TO WORK-ERRLOG-DATE.
           MOVE WORK-ERRLOG-DETAIL TO ERRLOGW-EOM-MSG.
           MOVE FORM-PATHNAME      TO ERRLOGW-PROGX.
           MOVE "CL/ERRLOG"        TO FORM-NAM.
           CALL FORM-PROGX  USING FORM-PATH ERRLOGW-AREA.
           CANCEL FORM-PATH.

       CREATE-BK-LOG-EXIT.
           EXIT.

      ***********************************************
       SET-INTERRUPT-INFO SECTION.
      ***********************************************
           MOVE " " TO ERRCD.

           MOVE "S  A170ONLN:FR."  TO VDU.
           MOVE " PAYMENT POSTING" TO VDUBUF.
           PERFORM SCREENIO.

           MOVE 0 TO BORROWER-KEY.
           IF BRANCH NOT NUMERIC OR BRANCH NOT > 1
              IF EXT-CONAME-USER-LOC NOT NUMERIC
                 MOVE "E"            TO ERRCD
                 MOVE "INVALID USER" TO MESS
                 PERFORM SEND-MESS
                 GO TO EXIT-SET-INTERRUPT-INFO
              ELSE
                 MOVE EXT-CONAME-USER-LOC TO BRANCH.

           IF LOAN-KEY = 0
              MOVE EXT-CONAME-SYSDATE TO ALP-DATE
              PERFORM CONVERT-ALPDATE-TO-YYYYMMDD
              MOVE DATE-YYYYMMDD TO TRANS-DATE.

      *    ------------------------------------------------------------
      *    IF POSTING INTER BRANCH, FULL-DISPLAY IS SET TO "B" IN
      *       LONPF1.C.  WHILE INTERBRANCH POSTING, USERS CAN NOT
      *       POST MORE THAN ONE TRANSACTION OR CHOOSE ANOTHER LOAN FROM
      *       WITHIN POSTING. AFTER THEIR ONE TRANSACTION, THEY ARE
      *       TAKEN BACK TO INTER BRANCH.   (MJD 050396 REQ #E114)
      *    ------------------------------------------------------------
           IF PATH-MACHINE NOT = " "
              IF FULL-DISPLAY = "B"
                 MOVE "END" TO IO-TYPE
                 MOVE "E"   TO ERRCD
                 GO TO END-ROUTINE.

           MOVE "F" TO FULL-DISPLAY.
           MOVE " " TO VDU-WIMENU-SELECTION.

      * LONPA0 OPENS THE WK-FILE OUTPUT, BUT CALLING PAYMENT POST FROM THE
      * F10 INTERRUPT DOES NOT CALL LONPA0! AND THE MAIN REASON WE NOW NEED
      * THIS IS DUE TO IO-CREATES IN THE XCBLCONFIG IS GONE FOR THE DATABASE
      * SO THE OPEN-WK-FILE THAT IS I-O WILL NOT CREATE THE FILE.
      * HOWEVER, COMING FROM XONPC0/F5 PAYFF/ RONPF, WE CAN NOT WIPE OUT THE
      * THE WORK FILE!
           MOVE TEMP-PATH TO ACCESS-BUF.
           PERFORM ACCESS-CALL.
           IF STAT NOT = "00"
              PERFORM OPEN-WK-FILE-OUTPUT.

       EXIT-SET-INTERRUPT-INFO.
           EXIT.

      ***********************************************
       SET-COLLECTOR-INFO SECTION.
      ***********************************************
           MOVE SPACES          TO EXT-WI-CL-WK-PATH.
           MOVE TRANS-DATE      TO EXT-WI-CL-TRANS-DATE.
           MOVE LN-OWNBR        TO EXT-WI-CL-OWNBR.
           MOVE LN-ACCTNO       TO EXT-WI-CL-ACCTNO.
      *    MOVE LN-APP-NUMBER   TO EXT-WI-CL-APP-NUMBER.
           MOVE LN-SSNO(1)      TO EXT-WI-CL-SSNO(1).
           MOVE LN-SSNO(2)      TO EXT-WI-CL-SSNO(2).
           MOVE LN-SSNO(3)      TO EXT-WI-CL-SSNO(3).
           MOVE LN-SSNO(4)      TO EXT-WI-CL-SSNO(4).
           MOVE LN-MAKERCD(1)   TO EXT-WI-CL-MAKERCD(1).
           MOVE LN-MAKERCD(2)   TO EXT-WI-CL-MAKERCD(2).
           MOVE LN-MAKERCD(3)   TO EXT-WI-CL-MAKERCD(3).
           MOVE LN-MAKERCD(4)   TO EXT-WI-CL-MAKERCD(4).
      * SET OF (WI-CL-SSNO-NO) DO DONE IN 'DISPLAY-LOAN':
           MOVE LN-DLNO         TO EXT-WI-CL-DLNO.
           MOVE LN-SALEFIN      TO EXT-WI-CL-SALEFIN.
           MOVE LN-PAY-SCHLD-FG TO EXT-WI-CL-PAY-SCHLD-FG.
           MOVE LPLOAN-SYSDATE  TO EXT-WI-CL-SYSDATE.
           MOVE EXT-CONAME-DAILY-PASSWORD TO EXT-WI-CL-PASSWD.

      *****************************************
       BWLN-SCAN SECTION.
      *****************************************

           MOVE TRANS-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-ALPDATE.
           MOVE ALP-DATE   TO BWLNSC-SYSDATE.
           MOVE 0          TO BWLNSC-ACCTNO.

           MOVE "WI/BWLNSC " TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH BWLNSC-BUF EXIT-PATHNAME.
           CANCEL FORM-PROGX.

           IF BWLNSC-STATUS NOT = "00"
              MOVE 9 TO IO-FG
              GO TO EXIT-BWLN-SCAN.

           MOVE BR-NO         TO LN-ACCTNO.
           MOVE BWLNSC-ACCTNO TO LN-ACCTNO.
           MOVE BWLNSC-SSNO   TO BW-SSNO BORROWER-KEY.
           MOVE 0 TO IO-FG.

       EXIT-BWLN-SCAN.
           EXIT.

      *****************************************
       DISPLAY-OTHER-LOANS-WINDOW SECTION.
      *****************************************

           MOVE "Y"        TO BWLNS2-SCAN-ONLY.
           MOVE BW-SSNO    TO BWLNS2-KEY.
           MOVE TRANS-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-ALPDATE.
           MOVE ALP-DATE   TO BWLNS2-SYSDATE.
           MOVE "N"        TO BWLNS2-ALL-LOANS.
           MOVE "WI/BWLNS2" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH BWLNS2-BUF EXIT-PATHNAME.
           CANCEL FORM-PROGX.

      **************************************
       ENTER-SCANKEY SECTION.
      **************************************
           MOVE "Y" TO DEFAULT1-KEY.

       ENTER-SCAN-START.
           MOVE 1 TO SCAN-FLAG.
           PERFORM OPEN-LN1-FILE.

           MOVE BW-PATH-OWNBR TO BW-OWNBR.

           MOVE "1-SOC SEC, 2-NAME, 3-SPS SS#, 4-SPS NAME" TO MESS.
           PERFORM SEND-LEGEND.
           IF DEFAULT1-KEY = "N"
              GO TO ENTER-SCAN-TYPE.

           MOVE "N" TO DEFAULT1-KEY.

           MOVE "S  A050STYP." TO VDU.
           MOVE "SKEY:"        TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "SN N010TYP." TO VDU.
           MOVE 2             TO TYP VDUNUM0.
           PERFORM SCREENIO.

           MOVE "S  A160SORTKEY."  TO VDU.
           MOVE "________________" TO VDUBUF.
           PERFORM SCREENIO.

           MOVE SPACES     TO BW-NAME.

           PERFORM START-BW2-FILE.

           GO TO ENTER-SCAN-DISPLAY.

       ENTER-SCAN-TYPE.
           MOVE "EN N010TYP." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO CLOSE-SCANKEY.
           IF VDUSTATUS NOT = "00"
              GO TO ENTER-SCAN-TYPE.
           MOVE VDUNUM0 TO TYP.

           IF TYP = 1
              MOVE 0 TO BW-SSNO
              PERFORM START-BW1-FILE
           ELSE
              IF TYP = 2
                 MOVE SPACES TO BW-NAME
                 PERFORM START-BW2-FILE
              ELSE
                 IF TYP = 3
                    MOVE 0 TO BW-SPSSNO
                    PERFORM START-BW3-FILE
                 ELSE
                    IF TYP = 4
                       MOVE SPACES TO BW-SPNAME
                       PERFORM START-BW4-FILE
                    ELSE
                       GO TO ENTER-SCAN-TYPE.

       ENTER-SCAN-DISPLAY.
           IF TYP = 2
              MOVE "NAME (LAST,FIRST), F5-SCAN LOAN, F6-SCAN" TO MESS
           ELSE
           IF TYP = 1
              MOVE "SOCIAL SECURITY #, F5-SCAN LOAN, F6-SCAN" TO MESS
           ELSE
           IF TYP = 3
              MOVE "SPOUSE SOC SEC #, F5-SCAN LOAN, F6-SCAN " TO MESS
           ELSE
           IF TYP = 4
              MOVE "SPOUSE NAME (L,F), F5-SCAN LOAN, F6-SCAN" TO MESS.
           PERFORM SEND-LEGEND.

       ENTER-SCAN-KEY.
           MOVE "E RA160SORTKEY." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO CLOSE-SCANKEY.
           IF VDUSTATUS = "1U"
              PERFORM CLOSE-SCANKEY
              GO TO ENTER-SCAN-START.
           IF VDUSTATUS = "1:"
              IF FIRST-SCAN = 0
                 MOVE " PLEASE SELECT A BORROWER FIRST!" TO MESS
                 PERFORM SEND-MESS
                 GO TO ENTER-SCAN-KEY.
           IF VDUSTATUS = "1:"
              PERFORM SCAN-LOANS
              IF IO-TYPE = "CLR"
                 MOVE " " TO IO-TYPE
                 PERFORM RESET-PARTIAL-SCREEN
                 GO TO ENTER-SCAN-KEY
              ELSE
                 GO TO ENTER-SCAN-KEY.
           IF VDUSTATUS = "1<"
              GO TO ENTER-SCAN-READ-NEXT.
           IF VDUSTATUS NOT = "00"
              GO TO ENTER-SCAN-KEY.
           IF VDUBUF = " "
              IF SCAN-ON
                 MOVE "PST" TO IO-TYPE
                 MOVE PROMPT-INITIAL-VALUES TO WR-BUF
                 MOVE PMPT-LIST TO WR-LIST
                 PERFORM CALL-WRFORM
                 GO TO CLOSE-SCANKEY
              ELSE
                 GO TO ENTER-SCAN-KEY.
           IF TYP = 2 OR 4
              PERFORM ARRANGE-BW-KEY
              MOVE BW-ALP-TABLE TO BW-NAME BW-SPNAME
           ELSE
              IF TYP = 1 OR 3
                 MOVE VDUBUF TO BW-SSNO BW-SPSSNO.

           IF TYP = 2
              PERFORM START-BW2-FILE
           ELSE
              IF TYP = 1
                 PERFORM START-BW1-FILE
              ELSE
                 IF TYP = 3
                    PERFORM START-BW3-FILE
                 ELSE
                    IF TYP = 4
                       PERFORM START-BW4-FILE.
           MOVE 0 TO SCAN-FLAG.

       ENTER-SCAN-READ-NEXT.
           MOVE 0 TO FIRST-SCAN.
           IF TYP = 2
              PERFORM READ-BW2-FILE-NEXT
           ELSE
              IF TYP = 1
                 PERFORM READ-BW1-FILE-NEXT
              ELSE
                 IF TYP = 3
                    PERFORM READ-BW3-FILE-NEXT
                 ELSE
                    IF TYP = 4
                       PERFORM READ-BW4-FILE-NEXT.

           IF (IO-FG = 9) OR (BW-OWNBR NOT = BW-PATH-OWNBR)
              MOVE "CLR" TO IO-TYPE
              GO TO CLOSE-SCANKEY.

           PERFORM DISPLAY-BORROWER.
           PERFORM DISPLAY-LOAN-INFO.

           PERFORM RESET-PARTIAL-SCREEN.

           MOVE 1 TO SCAN-FLAG
                     FIRST-SCAN.

           MOVE " " TO SCAN.
           GO TO ENTER-SCAN-KEY.

       CLOSE-SCANKEY.
           PERFORM CLOSE-LN1-FILE.

           MOVE 0      TO FIRST-SCAN.
           MOVE SPACES TO MESS.
           PERFORM SEND-LEGEND.

           MOVE "SORTKEY TYP STYP." TO WR-LIST.
           MOVE SPACES              TO WR-BUF.
           PERFORM CALL-WRFORM.

       EXIT-ENTER-SCANKEY.
           EXIT.

      **************************************
       ARRANGE-BW-KEY SECTION.
      **************************************
           MOVE 1 TO ARR1 ARR2.
           MOVE " " TO BW-ALP-TABLE.
       ARR-BW-KEY-AGAIN.
           IF ARR1 NOT < 35
              GO TO ARR-BW-KEY-EXIT.
           IF VDUTAB(ARR1) NOT = ","
              MOVE VDUTAB(ARR1) TO BW-ALP-TBL(ARR2)
           ELSE
              MOVE 35 TO ARR2.
           ADD 1 TO ARR1.
           ADD 1 TO ARR2.
           GO TO ARR-BW-KEY-AGAIN.

       ARR-BW-KEY-EXIT.
           EXIT.

      **************************************
       SCAN-LOANS SECTION.
      **************************************
      *    GO TO SCAN-LN4 SCAN-LN5 SCAN-LN6 SCAN-LN7
           GO TO SCAN-LN4 SCAN-LN5
                 DEPENDING ON SCAN-FLAG.

       SCAN-LN4.
           IF SCAN-OFF
              MOVE LN-PATH-OWNBR TO LN-OWNBR
                                 QLN4-WBEG-OWNBR
                                 QLN4-WEND-OWNBR
              MOVE BW-SSNO       TO LN-SSNO(1) 
                                 QLN4-WBEG-SSNO1
                                 QLN4-WEND-SSNO1
              PERFORM START-LN4-FILE
              MOVE "ON" TO SCAN.

           PERFORM READ-LN4-FILE-NEXT.
           IF IO-FG = 0
              IF (LN-SSNO(1) = BW-SSNO) AND (LN-OWNBR = LN-PATH-OWNBR)
                 IF ( LN-POCD-NONLOAN )
                    GO TO SCAN-LN4
                 ELSE
                    PERFORM DISPLAY-LOAN
                    GO TO END-SCAN-LOANS.

           ADD 1       TO SCAN-FLAG.
           MOVE SPACES TO SCAN.
       SCAN-LN5.
           IF SCAN-OFF
              MOVE LN-PATH-OWNBR TO LN-OWNBR
                                 QLN5-WBEG-OWNBR
                                 QLN5-WEND-OWNBR
              MOVE BW-SSNO       TO LN-SSNO(2) 
                                 QLN5-WBEG-SSNO2
                                 QLN5-WEND-SSNO2
              PERFORM START-LN5-FILE
              MOVE "ON" TO SCAN.
           PERFORM READ-LN5-FILE-NEXT.
           IF IO-FG = 0
              IF (LN-SSNO(2) = BW-SSNO) AND (LN-OWNBR = LN-PATH-OWNBR)
                 IF ( LN-POCD-NONLOAN )
                    GO TO SCAN-LN5
                 ELSE
                    PERFORM DISPLAY-LOAN
                    GO TO END-SCAN-LOANS.

      *    ADD 1       TO SCAN-FLAG.
      *    MOVE SPACES TO SCAN.
      *SCAN-LN6.
      *    IF SCAN-OFF
      *       MOVE LN-PATH-OWNBR TO LN-OWNBR
      *                          QLN6-WBEG-OWNBR
      *                          QLN6-WEND-OWNBR
      *       MOVE BW-SSNO       TO LN-SSNO(3) 
      *                          QLN6-WBEG-SSNO3
      *                          QLN6-WEND-SSNO3
      *       PERFORM START-LN6-FILE
      *       MOVE "ON" TO SCAN.
      *    PERFORM READ-LN6-FILE-NEXT.
      *    IF IO-FG = 0
      *       IF (LN-SSNO(3) = BW-SSNO) AND (LN-OWNBR = LN-PATH-OWNBR)
      *          IF ( LN-POCD-NONLOAN )
      *             GO TO SCAN-LN6
      *          ELSE
      *             PERFORM DISPLAY-LOAN
      *             GO TO END-SCAN-LOANS.

      *    ADD 1       TO SCAN-FLAG.
      *    MOVE SPACES TO SCAN.
      *SCAN-LN7.
      *    IF SCAN-OFF
      *       MOVE LN-PATH-OWNBR TO LN-OWNBR
      *                          QLN7-WBEG-OWNBR
      *                          QLN7-WEND-OWNBR
      *       MOVE BW-SSNO       TO LN-SSNO(4) 
      *                          QLN7-WBEG-SSNO4
      *                          QLN7-WEND-SSNO4
      *       PERFORM START-LN7-FILE
      *       MOVE "ON" TO SCAN.

      *    PERFORM READ-LN7-FILE-NEXT.
      *    IF IO-FG = 0
      *       IF (LN-SSNO(4) = BW-SSNO) AND (LN-OWNBR = LN-PATH-OWNBR)
      *          IF ( LN-POCD-NONLOAN )
      *             GO TO SCAN-LN7
      *          ELSE
      *             PERFORM DISPLAY-LOAN
      *             GO TO END-SCAN-LOANS.

           MOVE "CLR"  TO IO-TYPE.
           MOVE 0      TO SCAN-FLAG.
           MOVE SPACES TO SCAN LN-REC.

       END-SCAN-LOANS.
           EXIT.

      *****************************************
       DISPLAY-BORROWER SECTION.
      *****************************************
           MOVE SPACES TO DISPLAY-BORR-BUF.

           MOVE BW-SSNO TO NUM-BW.
           PERFORM NUMBW-TO-ALPBW.
           MOVE ALP-BW  TO D-SOC.
           IF GP-HIDE-FIRST-5-SSNO = "Y"
              MOVE ALL "X" TO D-SOC1 D-SOC2.

           MOVE BW-LNAME TO NAME-WORKER.
           MOVE BW-FNAME TO NAME.
           INSPECT NAME-WORKER REPLACING FIRST "   " BY ", ?".
           INSPECT NAME-WORKER REPLACING FIRST "?               "
                                                          BY NAME.
           IF BW-MNAME NOT = SPACES
              STRING NAME-WORKER," ",BW-MNAME DELIMITED BY "  " INTO
                  NAME-WORKER.
           MOVE NAME-WORKER TO D-NAME-WORLD(1).
           MOVE BW-REPCODE  TO D-REP-WORLD(1).

           MOVE BW-ADR1     TO D-NAME(2).
           IF BW-ADR2 NOT = " "
              MOVE BW-ADR2  TO D-NAME(3).

           MOVE SPACES   TO NAME-WORKER.
           MOVE BW-CITY  TO NAME-WORKER.
           MOVE BW-STATE TO NAME.
           INSPECT NAME-WORKER REPLACING FIRST "   " BY ", *".
           INSPECT NAME-WORKER REPLACING FIRST "*               "
                                                          BY NAME.
           INSPECT NAME-WORKER REPLACING FIRST "  " BY " *".
           MOVE BW-ZIP   TO NAME.
           INSPECT NAME-WORKER REPLACING FIRST "*               "
                                                          BY NAME.
           IF BW-ADR2 = " "
              MOVE NAME-WORKER TO D-NAME(3)
           ELSE
              MOVE NAME-WORKER TO D-NAME(4).

           MOVE BW-LOANS TO D-LOANS.

      * CAN NOT READ BW INFO FOR JOINT OR ENDORSER IF IN MIDDLE OF
      * SCAN
           IF TYP NOT = 0
              GO TO CONTINUE-WITHOUT-READING-BORROWER.

           MOVE 1 TO SUB.
       DISPLAY-JOINT-OR-ENDORSER.
           ADD 1 TO SUB.
           IF SUB > 4 OR LN-MAKERCD(SUB) = SPACES
              GO TO CONTINUE-WITHOUT-READING-BORROWER.

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
              MOVE BW-SSNO      TO HOLD-BW-SSNO
              MOVE LN-SSNO(SUB) TO BW-SSNO NUM-BW
              PERFORM READ-BW1-FILE
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
                 PERFORM READ-BW1-FILE
                 GO TO END-DISPLAY-BORR
              ELSE
                 MOVE HOLD-BW-SSNO TO BW-SSNO
                 PERFORM READ-BW1-FILE
                 GO TO END-DISPLAY-BORR.

       CONTINUE-WITHOUT-READING-BORROWER.
           IF BW-SPSSNO = 0
              IF BW-SPLNAME = " "
                 IF BW-SPFNAME = " "
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
           IF LN-POCD-PB-RESC
              MOVE "(( RESCISSION RENEWAL ))" TO D-MEMO
           ELSE
              IF BR-PRIO-MEMO-FLAG = "Y"
                 PERFORM GET-PRIORITY-MEMO
              ELSE
                 IF BR-PRIO-MEMO-FLAG = "I"
                    PERFORM DISPLAY-INSURANCE-TRACKING.

           IF (BW-CREDIT-SCORE NUMERIC) AND (BW-CREDIT-SCORE > 0)
              MOVE "SCORE:" TO D-SCOREPROMPT
              MOVE BW-CREDIT-SCORE TO D-CREDIT-SCORE
           ELSE
              MOVE SPACES TO D-SCOREPROMPT
              MOVE 0 TO D-CREDIT-SCORE.

           MOVE DISPLAY-BORR-BUF TO WR-BUF.
           MOVE DISPLAY-BORR-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

           MOVE BW-SSNO TO NUM-BW.
           PERFORM NUMBW-TO-ALPBW.
           MOVE ALP-BW  TO D-SOC.

      ************************************************
       GET-PRIORITY-MEMO SECTION.
      ************************************************
           MOVE BW-SSNO  TO BM-SSNO.
           MOVE 0        TO BM-DATE.
           MOVE 1        TO BM-SEQ.
           PERFORM OPEN-BM1-FILE.
           PERFORM READ-BM1-FILE.
           PERFORM CLOSE-BM1-FILE.
           IF IO-FG = 0
              MOVE BM-MEMO TO D-MEMO.

      ***********************************************
       VERIFY-PASSWORD SECTION.
      ***********************************************
           MOVE SPACES                           TO PASSWDW-STAT.
           MOVE "LONPF0"                         TO PASSWDW-PGM.
           MOVE "PLEASE ENTER PASSWORD OR CALL " TO PASSWDW-PROMPT1.
           MOVE "BK DEPT FOR MORE INFO"          TO PASSWDW-PROMPT2.

           MOVE "CL/PASSWD"                      TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH PASSWDW-BUF.
           CANCEL FORM-PROGX.

      ***********************************************
       VERIFY-PASSWD-BK-WORLD SECTION.
      ***********************************************
           MOVE SPACES                           TO PASSWDW-STAT.
           MOVE "LONPF0"                         TO PASSWDW-PGM.
           MOVE "ENTER PASSWORD TO ACCESS"       TO PASSWDW-PROMPT1.
           MOVE "BANKRUPT ACCOUNT"               TO PASSWDW-PROMPT2.

           MOVE "CL/PASSWD"                      TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH PASSWDW-BUF.
           CANCEL FORM-PROGX. 

       VERIFY-PASSWD-BK-WORLD-EXIT.    
           EXIT.


      **********************************************
       DISPLAY-LOAN SECTION.
      **********************************************
           IF BORROWER-SCAN = "Y"
              GO TO DISPLAY-LOAN-EXIT.

           MOVE SPACES TO DISPLAY-LOAN-BUF.
           IF ( LN-POCD-NONLOAN )
              GO TO DISPLAY-LOAN-EXIT.

           PERFORM GET-SP.

           MOVE LN-1STPYDATE TO PDTH-DATE-WORK.
           PERFORM PAID-THRU-CALCULATION.

           PERFORM LPSTAT-DISPLAY.
           MOVE LPS-DISPLAY-LNNO TO D-LOAN-NUM.

           IF LN-SSNO(1) = BW-SSNO
              MOVE 1             TO EXT-WI-CL-SSNO-NO
              MOVE LN-MAKERCD(1) TO MAKER-CD
           ELSE
              IF LN-SSNO(2) = BW-SSNO
                 MOVE 2             TO EXT-WI-CL-SSNO-NO
                 MOVE LN-MAKERCD(2) TO MAKER-CD
                 PERFORM DISPLAY-MAKER
              ELSE
                 IF LN-SSNO(3) = BW-SSNO
                    MOVE 3             TO EXT-WI-CL-SSNO-NO
                    MOVE LN-MAKERCD(3) TO MAKER-CD
                    PERFORM DISPLAY-MAKER
                 ELSE
                    IF LN-SSNO(4) = BW-SSNO
                       MOVE 4             TO EXT-WI-CL-SSNO-NO
                       MOVE LN-MAKERCD(4) TO MAKER-CD
                       PERFORM DISPLAY-MAKER.

      * WORLD IS NOW USING BW-PRIVACY-OPT-OUT FOR TEXTING OPTION
      *    IF NOT GP-I-AM-WORLD
      *       IF GP-BW-PRIVACY-OPT-OUT = "Y"
      *          IF BW-PRIVACY-OPT-OUT = "Y"
      *             MOVE "S  A020OO:RF." TO VDU
      *             MOVE "OO"            TO VDUBUF
      *             PERFORM SCREENIO.

           MOVE LN-LOANDATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO D-H1.

           MOVE LN-1STPYDATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO ALP-DATE-EDIT.
           MOVE LN-UNITPER-CD   TO DATER-UNITPER-CD.
           MOVE LN-UNITPER-FREQ TO DATER-UNITPER-FREQ.
           PERFORM UNITPER-MASK.
           MOVE ALP-DATE-EDIT TO D-H2.

           PERFORM MATURITY-DATE-CALCULATION.
           MOVE MDTE-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY   TO D-H3.

           MOVE LN-SUM-FINCHG TO D-H4.
           MOVE LN-PROMDATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY       TO D-H5.

           MOVE LN-SUM-LNAMT      TO D-H6.
           MOVE PDTH-CUR-SCHD-PAY TO D-H7.
           IF LN-PAY-SCHLD-FG = "Y"
              MOVE "S" TO D-H71.

           MOVE LN-LYRINTPD TO D-H8.
           MOVE LN-YTDINTPD TO D-H9.

           PERFORM GET-LC.
           PERFORM COMPUTE-EARNINGS.
           IF LC-DISPLAY-ERN = "A"
              MOVE "ACC-EARN"      TO D-B1
              MOVE EARN-TOT-ACCERN TO D-H10
           ELSE
              IF LC-DISPLAY-ERN = "U"
                 MOVE "UNEARNED"      TO D-B1
                 MOVE EARN-TOT-UNEARN TO D-H10
              ELSE
                 IF LC-DISPLAY-ERN = "T"
                    MOVE "TOT-EARN" TO D-B1
                    MOVE LN-TOTERN  TO D-H10
                 ELSE
                    IF LC-DISPLAY-ERN = "I"
                       MOVE TRANS-DATE TO POFF-PAYDATE
                       MOVE "M"        TO POFF-MAKERCD
                       MOVE "PO"       TO POFF-LPTRCD
                       MOVE "N"        TO POFF-DEF-STOP
                       PERFORM PAYOFF-LOAN-ROUTINE
                       MOVE "TDUE INT"  TO D-B1
                       MOVE POFF-INTDUE TO D-H10
                    ELSE
                       MOVE " PAYMNTD"    TO D-B1
                       MOVE LN-TOTPAYMNTD TO D-H10.

           MOVE TRANS-DATE TO AGEING-DATE.
           PERFORM COMPUTE-CONTRACTUAL.
           PERFORM COMPUTE-RECENCY.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      * 'LC' IS INITIAL VALUE
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( DELINQ-SETTING = "LC")
              MOVE LC-PROF TO DELINQ-SCAN
              MOVE "D" TO DELINQ-PROF3.

           IF LC-CON-PDUE-DAYS = "D"
              MOVE TRANS-DATE   TO PDUE-TODAY-DATE
              MOVE LN-1STPYDATE TO PDUE-1STPYDATE
              PERFORM PAST-DUE-DAYS-CALCULATION
              MOVE "DAY:"    TO D-PH11
              MOVE PDUE-DAYS TO D-H11
           ELSE
              MOVE "CON:"    TO D-PH11
              IF CPOT30 = "Y"
                 MOVE "<P>"  TO D-H11A
              ELSE
              IF CPOT30 = "M"
                 MOVE "<M>"  TO D-H11A
              ELSE
              IF CPOTCHGOFF = "Y"
                 MOVE "<PC>" TO D-H11A
              ELSE
              IF CPOTCHGOFF = "M"
                 MOVE "<PL>" TO D-H11A
              ELSE
                 MOVE CONTRACTUAL TO D-H11.

           MOVE DELINQ-PROF1 TO DELINQ-CODE.
           PERFORM GET-DELINQ-VALUE.
           MOVE DELINQ-VALUE TO D-H12 DISP-DELINQ-VALUE-1.

           IF LC-REC-PDUE-DAYS = "D"
              MOVE TRANS-DATE   TO PDUE-TODAY-DATE
              MOVE LN-1STPYDATE TO PDUE-1STPYDATE
              PERFORM PAST-DUE-DAYS-CALCULATION
              MOVE "DAY:"    TO D-PH13
              MOVE PDUE-DAYS TO D-H13
           ELSE
              MOVE "REC:"    TO D-PH13
              IF RDPOT30 = "Y"
                 MOVE "<P>"  TO D-H13A
              ELSE
              IF RDPOT30 = "M"
                 MOVE "<M>"  TO D-H13A
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

           MOVE LN-LCPDTH-DATE    TO LCHG-LCPDTH-DATE.
           MOVE PDTH-CUR-SCHD-PAY TO LCHG-LATE LCHG-CURPAY.
           PERFORM LATE-CHARGE-CALCULATION.

           MOVE LN-SUM-ORGTERM TO D-H16.
           MOVE TRANS-DATE TO IBPC-DATE.
           PERFORM IBPC-TEST.
           IF ( IBPC-FG = "P" OR LN-LOANTYPE = "I" ) AND
              ( LN-ACCELDATE = 0                   ) AND
              ( SP-LCFRMLA NOT = " "           ) OR
              ( LN-LC-CODE NOT = ZEROES        )  
               MOVE LCHG-CHARGE TO D-H15
               MOVE LN-LCPDTH-DATE TO NUM-DATE
               IF LN-UNITPER-CD-DAYS
                  MOVE NUM-DA TO NUM-YR
                  MOVE NUM-MO TO NUM-DA
                  MOVE NUM-DATE TO D-H17
               ELSE
                  MOVE NUM-MO TO NUM-DA
                  MOVE NUM-DATE TO D-H17.
           MOVE LN-TOTNODEF   TO D-H18.
           MOVE LN-TOTNOALDEL TO D-H181.
           MOVE LN-OVRDRATE   TO D-H19.
           MOVE LN-ACTIONCD   TO D-H20.
           MOVE LN-DLNO       TO D-DLRNUM.
           MOVE "EXTRCT:"     TO D-EXTRCT.
           MOVE LN-DELCNT     TO D-DELCNT.

           MOVE "COLCT:"     TO D-COLCTAG.
           MOVE LN-COLLECTOR TO D-COLCTCD.

           MOVE LN-COLLCD TO D-COLLCD.
           IF LN-COLLID NOT = SPACES
              MOVE LN-COLLID TO D-COLLID
           ELSE
              PERFORM GET-COLLATERAL
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
                 MOVE LTC-HHGS(1) TO D-COLLID.

           MOVE DISPLAY-LOAN-BUF TO WR-BUF.
           MOVE DISPLAY-LOAN-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

           PERFORM VARYING S1 FROM 1 BY 1 UNTIL S1 > 10
               IF LN-STATUSFLAGS(S1) = "C"
                  MOVE 10 TO S1
                  MOVE "S  A160LNNO:R." TO VDU
                  MOVE D-LOAN-NUM       TO VDUBUF
                  PERFORM SCREENIO
               END-IF
           END-PERFORM.


           IF (LN-ACH = "Y" AND GP-LOAN-ACH-PROCESSING = "Y")
              MOVE ACH-PROMPT-BUF  TO WR-BUF
              MOVE ACH-PROMPT-LIST TO WR-LIST
           ELSE
              MOVE PMT-PROMPT-BUF  TO WR-BUF
              MOVE PMT-PROMPT-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

           PERFORM SET-DELINQ-DISP-ATTRIBUTES.
           MOVE DISP-DELINQ-VALUE   TO WR-BUF.
           MOVE DISPLAY-DELINQ-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

           PERFORM SET-MESSAGE-ATTR.

           IF LN-MESSAGE NOT = " "
              MOVE LN-MESSAGE        TO D-MESSAGE
              MOVE D-MESSAGE         TO WR-BUF
              MOVE MESSAGE-LIST-ATTR TO WR-LIST
              PERFORM CALL-WRFORM.

           PERFORM WORLD-CREDIT-DISPLAY.

      * ONLY COMPUTE AVAILABLE CREDIT IF CUSTOMER HAS IT TURNED ON
      *    IF BR-AVAIL-CREDIT = "Y" AND
      *           (LN-CREDITLIM NOT = 0) AND
      *            (LN-BNKRPTDATE = 0)
      *       MOVE TRANS-DATE TO AVCR-PAYDATE
      *       MOVE MAKER-CD   TO AVCR-MAKERCD
      *       PERFORM COMPUTE-AVAILABLE-CREDIT
      *       MOVE " HIGH CREDIT:"  TO HIGH-CREDIT-X-BUF
      *       MOVE " AVAILABLE:"    TO AVA-BUF
      *       MOVE LN-CREDITLIM     TO HIGH-CREDIT-BUF
      *       MOVE AVCR-AVAILCREDIT TO AVAIL-BUF
      *       MOVE AVCR-RESTRICT-FG TO AVAIL-BUF-RESTRICT-FG
      *       MOVE HIGH-BUF       TO WR-BUF
      *       MOVE HIGH-LIST-ATTR TO WR-LIST
      *       PERFORM CALL-WRFORM
      *    ELSE
      *    IF BR-AVAIL-CREDIT = "S" AND
      *           (LN-CREDITLIM NOT = 0) AND
      *            (LN-BNKRPTDATE = 0)
      *       MOVE TRANS-DATE TO AVCR-PAYDATE
      *       MOVE MAKER-CD   TO AVCR-MAKERCD
      *       PERFORM COMPUTE-AVAILABLE-CREDIT
      *       MOVE " CUR AVAIL:"      TO HIGH-CREDIT-X-BUF
      *- - - - - - - SETTING CUR. AVAILABLE BY HIGH CREDIT IN LOAN RECORD
      *       MOVE AVCR-AVAILCREDIT   TO HIGH-CREDIT-BUF
      *       MOVE " AVAILABLE:"      TO AVA-BUF
      *- - - - - - - SETTING AVAILABLE BY A NET PAYOFF
      *       MOVE AVCR-CURRENT-AVAIL TO AVAIL-BUF
      *       MOVE AVCR-RESTRICT-FG   TO AVAIL-BUF-RESTRICT-FG
      *       IF LN-CLASS = 5 OR 8 OR 11
      *          MOVE SPACES          TO AVA-BUF AVAIL-SPACE
      *          MOVE HIGH-BUF        TO WR-BUF
      *          MOVE HIGH-LIST-S5    TO WR-LIST
      *          PERFORM CALL-WRFORM
      *       ELSE
      *          MOVE HIGH-BUF       TO WR-BUF
      *          MOVE HIGH-LIST-ATTR TO WR-LIST
      *          PERFORM CALL-WRFORM
      *     ELSE
      * DISPLAY BRANCH AND HIGHLIGHT
      *       IF BR-AVAIL-CREDIT = "B"
      *          MOVE TRANS-DATE TO AVCR-PAYDATE
      *          MOVE MAKER-CD   TO AVCR-MAKERCD
      *          PERFORM COMPUTE-AVAILABLE-CREDIT
      *          MOVE "BRANCH:"        TO HIGH-CREDIT-X-BUF
      *          MOVE BR-NO            TO HIGH-CREDIT-BUF
      *          MOVE " AVAILABLE:"    TO AVA-BUF
      *          MOVE AVCR-AVAILCREDIT TO AVAIL-BUF
      *          MOVE AVCR-RESTRICT-FG TO AVAIL-BUF-RESTRICT-FG
      *          MOVE HIGH-BUF         TO WR-BUF
      *          MOVE HIGH-LIST-S5     TO WR-LIST
      *          PERFORM CALL-WRFORM
      *       ELSE
      *          MOVE SPACES    TO HIGH-BUF AVA-BUF
      *          MOVE HIGH-BUF  TO WR-BUF
      *          MOVE HIGH-LIST TO WR-LIST
      *          PERFORM CALL-WRFORM.

      *    IF GP-USE-SCRA = "Y"
      *       MOVE LN-OWNBR  TO LSR-BRNO
      *       MOVE LN-ACCTNO TO LSR-ACCTNO
      *       MOVE 1         TO LSR-SEQNO
      *       PERFORM OPEN-LSR1-FILE
      *       PERFORM READ-LSR1-FILE
      *       PERFORM CLOSE-LSR1-FILE
      *       IF IO-FG = 0 AND LSR-SCRA-ACTIVE = "Y"
      *          MOVE " **  SCRA **  " TO HIGH-CREDIT-X-BUF
      *          MOVE SPACES           TO AVA-BUF AVAIL-SPACE
      *          MOVE 0                TO HIGH-CREDIT-BUF
      *          MOVE HIGH-BUF       TO WR-BUF
      *          MOVE HIGH-LIST-SCRA TO WR-LIST
      *          PERFORM CALL-WRFORM.

           IF IO-TYPE = "PST"
              PERFORM SEND-LOAN-MESS
              GO TO DISPLAY-LOAN-EXIT.

           MOVE LN-1STPYDATE   TO PDTH-DATE-WORK.
           PERFORM PAID-THRU-CALCULATION.
           MOVE PDTH-DATE-FULL TO NUM-DATE.
           MOVE NUM-MO         TO D-PDTHRU-MO.
           IF LN-UNITPER-CD-DAYS
              MOVE NUM-DA      TO D-PDTHRU-DAYR
           ELSE
              MOVE NUM-YR      TO D-PDTHRU-DAYR.

           MOVE LN-OTHBAL        TO D-OTHBAL.
           ADD LN-LCBAL LN-INTBAL GIVING D-INTBAL.
           MOVE LN-CURBAL        TO D-CURBAL.

           MOVE DISPLAY-BAL-BUF  TO WR-BUF.
           MOVE DISPLAY-BAL-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

           PERFORM SEND-LOAN-MESS.

       DISPLAY-LOAN-EXIT.
           EXIT.

      *****************************************
       DISPLAY-LOAN-INFO SECTION.
      *****************************************
           MOVE 1   TO SCAN-FLAG.
           MOVE "Y" TO BORROWER-SCAN.

       DISPLAY-LN-NEXT-8.
           MOVE 0      TO DIS1.
           MOVE SPACES TO LOAN-INFO-TABLE.

       DISPLAY-LN-THIS-8.
           PERFORM SCAN-LOANS.
           IF SCAN-FLAG = 0
              GO TO DISPLAY-LN-XFER-INFO.

           ADD 1 TO DIS1.

           PERFORM GET-SP.
           PERFORM LPSTAT-DISPLAY.

           MOVE LPS-DISPLAY-LNNO TO LI-LOAN-NUM(DIS1).
           IF LN-PLCD NOT = " "
              MOVE LN-PLTYPE   TO LI-PLFG(DIS1)
              MOVE LN-PLDATE   TO DATE-YYYYMMDD
              PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
              MOVE DATE-MMDDYY TO LI-JDPLDT(DIS1)
           ELSE
              IF LN-JDDATE NOT = 0
                 MOVE "J"         TO LI-PLFG(DIS1)
                 MOVE LN-JDDATE   TO DATE-YYYYMMDD
                 PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
                 MOVE DATE-MMDDYY TO LI-JDPLDT(DIS1).

           MOVE LN-LOANDATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY    TO LI-LNDATE(DIS1).
           MOVE LN-SUM-LNAMT   TO LI-LOAN-AMT(DIS1).
           MOVE LN-SUM-ORGTERM TO LI-TRM(DIS1).
           MOVE LN-DLNO        TO LI-DEALER(DIS1).
           MOVE LN-COLLCD      TO LI-COLL(DIS1).

           MOVE DELINQ-PROF3 TO DELINQ-CODE.
           PERFORM GET-DELINQ-VALUE.
           MOVE DELINQ-VALUE   TO LI-CONTRACT(DIS1).

           IF LN-SSNO(1) = BW-SSNO
              MOVE LN-MAKERCD(1) TO LI-MKR(DIS1)
           ELSE
              IF LN-SSNO(2) = BW-SSNO
                 MOVE LN-MAKERCD(2) TO LI-MKR(DIS1)
              ELSE
                 IF LN-SSNO(3) = BW-SSNO
                    MOVE LN-MAKERCD(3) TO LI-MKR(DIS1)
                 ELSE
                    IF LN-SSNO(4) = BW-SSNO
                       MOVE LN-MAKERCD(4) TO LI-MKR(DIS1).

           IF LN-POCD NOT = " "
              MOVE LN-POCD     TO LI-POCD(DIS1)
              MOVE LN-POFFDATE TO DATE-YYYYMMDD
              PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
              MOVE LN-POFFDATE TO LI-PAIDOFF(DIS1)
           ELSE
              IF LN-POFFDATE NOT = 0
                 MOVE LN-POCD     TO LI-POCD(DIS1)
                 MOVE LN-POFFDATE TO DATE-YYYYMMDD
                 PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
                 MOVE LN-POFFDATE TO LI-PAIDOFF(DIS1)
              ELSE
                 MOVE LN-CURBAL   TO LI-LOAN-BAL(DIS1).

           IF DIS1 < 8
              GO TO DISPLAY-LN-THIS-8.

       DISPLAY-LN-XFER-INFO.
           MOVE LOAN-INFO-BUF  TO WR-BUF.
           MOVE LOAN-INFO-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

           IF SCAN-FLAG NOT = 0
              MOVE "E RA160SORTKEY." TO VDU
              PERFORM SCREENIO
              GO TO DISPLAY-LN-NEXT-8.

       DISPLAY-LOAN-INFO-EXIT.
           MOVE "N"    TO BORROWER-SCAN.
           MOVE SPACES TO IO-TYPE.

      ***********************************************
       CLEAR-WK-FILE SECTION.
      ***********************************************
           MOVE 3 TO WK-KEY.
           PERFORM OPEN-WK-FILE.
           PERFORM READ-WK-FILE.
           IF IO-FG = 9
              GO TO EXIT-CLEAR-WK-FILE.

           MOVE SPACES TO WK-SAVE-RECORD.
           MOVE 0      TO WK-ELE.
           PERFORM REWRITE-WK-FILE.

       EXIT-CLEAR-WK-FILE.
           PERFORM CLOSE-WK-FILE.

      *************************************
       SAVE-SCREEN SECTION.
      *************************************
           MOVE 3 TO WK-KEY.
           PERFORM OPEN-WK-FILE.
           PERFORM READ-WK-FILE.
           IF IO-FG = 9
              GO TO IO-ERROR.

           IF MENU-RESUME = "Y"
              MOVE "N" TO MENU-RESUME
              IF WK-OLD-PROG-FORM = "  "
                 MOVE 99 TO MENU-FG
                 GO TO EXIT-SAVE-SCREEN
              ELSE
                 MOVE WK-OLD-PROG-FORM TO NEW-PROG-FORM OLD-PROG-FORM
                 MOVE SPACES           TO WK-OLD-PROG-FORM.

           MOVE ELE     TO WK-ELE.
           MOVE LN-REC  TO WK-SAVE-RECORD.
           PERFORM REWRITE-WK-FILE.
       EXIT-SAVE-SCREEN.
           PERFORM CLOSE-WK-FILE.

      *************************************
       RESTORE-SCREEN SECTION.
      *************************************
           MOVE 3 TO WK-KEY.
           PERFORM OPEN-WK-FILE.
           PERFORM READ-WK-FILE.
           IF NEW-FORM NOT = OLD-FORM
              MOVE OLD-PROG-FORM TO WK-OLD-PROG-FORM
              MOVE NEW-PROG-FORM TO OLD-PROG-FORM.
           IF IO-FG = 0
              GO TO RESTORE-CLEAR.

           MOVE SPACES TO WK-SAVE-RECORD.
           MOVE 0      TO WK-ELE.
           PERFORM WRITE-WK-FILE.
           IF IO-FG = 9
              GO TO IO-ERROR.

           GO TO EXIT-RESTORE-SCREEN.
       RESTORE-CLEAR.
           MOVE WK-SAVE-RECORD TO LN-REC.
           MOVE 0              TO WK-ELE.
           PERFORM REWRITE-WK-FILE.

       EXIT-RESTORE-SCREEN.
           PERFORM CLOSE-WK-FILE.

      *************************************
       GET-DELINQ-CODES SECTION.
      *************************************
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
              GO TO GET-DELINQ-ENTER.
           MOVE VDUBUF TO DELINQ-BUF.

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

           MOVE "SEL." TO WR-LIST.
           MOVE SPACES TO WR-BUF.
           PERFORM CALL-WRFORM.

           MOVE "F4" TO DELINQ-SETTING.

       GET-DELINQ-CODES-EXIT.
           EXIT.

      *************************************
       SET-DELINQ-DISP-ATTRIBUTES SECTION.
      *************************************
           IF LC-PROF1 = "D" AND LC-PROF2 = "D"
              MOVE "H12:R H14:R."       TO DISPLAY-DELINQ-LIST
           ELSE
              IF LC-PROF1 = "D"
                 MOVE "H12:R H14.  "    TO DISPLAY-DELINQ-LIST
              ELSE
                 IF LC-PROF2 = "D"
                    MOVE "H12 H14:R.  " TO DISPLAY-DELINQ-LIST
                 ELSE
                    MOVE "H12 H14.    " TO DISPLAY-DELINQ-LIST.
       EXIT-SET-DELINQ-DISP-ATTRIBUTE.
           EXIT.

      ***************************************************
       WORLD-CREDIT-DISPLAY SECTION.
      ***************************************************
      * CHECK TO SEE IF ACCT WOULD BE A PBD IF PD OFF TODAY
           MOVE " "          TO PBD-FLAG.
           MOVE TRANS-DATE   TO PDUE-TODAY-DATE.
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
                 STRING "REAFFIRMED, ", LTB-WHO-FILED,
                        DELIMITED BY "  " INTO WORLD-CREDIT-BUF
                 MOVE WORLD-REAFFIRM-LIST TO WR-LIST
                 MOVE WORLD-CREDIT-BUF    TO WR-BUF
                 PERFORM CALL-WRFORM
                 GO TO EXIT-WORLD-CREDIT-DISPLAY
              ELSE
              IF LTB-DISCHARGE-DATE NOT = 0
                 MOVE LTB-DISCHARGE-DATE TO DATE-YYYYMMDD
                 PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
                 MOVE DATE-MMDDYY TO VDU-DATE-EDIT
                 STRING "DISCHARGED, ",
                        LTB-WHO-FILED, ", ",
                        VDU-DATE-EDIT
                        DELIMITED BY "  " INTO WORLD-CREDIT-BUF
                 MOVE WORLD-REAFFIRM-LIST TO WR-LIST
                 MOVE WORLD-CREDIT-BUF    TO WR-BUF
                 PERFORM CALL-WRFORM
                 GO TO EXIT-WORLD-CREDIT-DISPLAY.

           IF BR-AVAIL-CREDIT = "Y"
                     AND (LN-CREDITLIM NOT = 0)
                      AND (LN-BNKRPTDATE = 0)
              MOVE TRANS-DATE        TO AVCR-PAYDATE
              MOVE MAKER-CD          TO AVCR-MAKERCD
              PERFORM COMPUTE-AVAILABLE-CREDIT
              MOVE "HI CRED:"        TO WORLD-HI-CREDIT-PROMPT
              MOVE LN-CREDITLIM      TO WORLD-HI-CREDIT-AMT
              MOVE "WCBC:" TO WORLD-WCBC-CREDIT-PROMPT
              MOVE LN-WCBC-CREDITLIM TO WORLD-WCBC-CREDIT-AMT
              MOVE "AVAIL:"          TO WORLD-AVAIL-CREDIT-PROMPT
              MOVE AVCR-AVAILCREDIT  TO WORLD-AVAIL-CREDIT-AMT
      *       PERFORM TEST-IF-SC-RESTRICTION
              IF AVCR-RESTRICT-FG = " "
                 MOVE PBD-FLAG TO WORLD-AVAIL-FLAG1
                 MOVE " "      TO WORLD-AVAIL-FLAG2
              ELSE
                 MOVE AVCR-RESTRICT-FG TO WORLD-AVAIL-FLAG1
                 MOVE PBD-FLAG         TO WORLD-AVAIL-FLAG2
              END-IF
              MOVE WORLD-CREDIT-BUF       TO WR-BUF
              MOVE WORLD-CREDIT-LIST-ATTR TO WR-LIST
              PERFORM CALL-WRFORM
           ELSE
           IF BR-AVAIL-CREDIT = "S"
                     AND (LN-CREDITLIM NOT = 0)
                      AND (LN-BNKRPTDATE = 0)
              MOVE TRANS-DATE        TO AVCR-PAYDATE
              MOVE MAKER-CD          TO AVCR-MAKERCD
              PERFORM COMPUTE-AVAILABLE-CREDIT
              MOVE "CUR AVL:"        TO WORLD-HI-CREDIT-PROMPT
      * - - - - - - SETTING CUR. AVAILABLE BY HIGH CREDIT IN LOAN RECORD
              MOVE AVCR-AVAILCREDIT  TO WORLD-HI-CREDIT-AMT
              MOVE "WCBC:"           TO WORLD-WCBC-CREDIT-PROMPT
              MOVE LN-WCBC-CREDITLIM TO WORLD-WCBC-CREDIT-AMT
              MOVE "AVAIL:" TO WORLD-AVAIL-CREDIT-PROMPT
      * - - - - - - SETTING  AVAILABLE BY A NET PAYOFF
              MOVE AVCR-CURRENT-AVAIL TO WORLD-AVAIL-CREDIT-AMT
              IF AVCR-RESTRICT-FG = " "
                 MOVE PBD-FLAG TO WORLD-AVAIL-FLAG1
                 MOVE " "      TO WORLD-AVAIL-FLAG2
              ELSE
                 MOVE AVCR-RESTRICT-FG TO WORLD-AVAIL-FLAG1
                 MOVE PBD-FLAG         TO WORLD-AVAIL-FLAG2
              END-IF
              MOVE WORLD-CREDIT-BUF       TO WR-BUF
              MOVE WORLD-CREDIT-LIST-ATTR TO WR-LIST
              PERFORM CALL-WRFORM
           ELSE
              MOVE SPACES TO WORLD-CREDIT-BUF
              MOVE WORLD-CREDIT-BUF  TO WR-BUF
              MOVE WORLD-CREDIT-LIST TO WR-LIST
              PERFORM CALL-WRFORM.

       EXIT-WORLD-CREDIT-DISPLAY.
           EXIT.

       COPY "LIBWI/PFSWINDOWS.CPY".
       COPY "LIBLP/LPEARN.CPY".
       COPY "LIBLP/LPLCHG.CPY".
       COPY "LIBGB/REBATE.CPY".
       COPY "LIBLP/LPPOFF.CPY".
       COPY "LIBLP/LPPOF2.CPY".
       COPY "LIBLP/LPITRM.CPY".
       COPY "LIBLP/LPAMTS.CPY".
       COPY "LIBLP/LPAPRS.CPY".
       COPY "LIBLP/LPAPRZ.CPY".
       COPY "LIBLP/LPUPER.CPY".
       COPY "LIBLP/LPUPWK.CPY".
       COPY "LIBGB/DATER.CPY".
       COPY "LIBLP/LPPDUE.CPY".
       COPY "LIBLP/LPMDTE.CPY".
       COPY "LIBGB/AGEING.CPY".
       COPY "LIBLP/LPCEPP.CPY".
       COPY "LIBGB/SPPDTH.CPY".
       COPY "LIBLP/LPSCHD.CPY".
       COPY "LIBLP/LPTDEL.CPY".
       COPY "LIBLP/LPCDEL.CPY".
       COPY "LIBLP/LPLCAS.CPY".
       COPY "LIBLP/LPLCAP.CPY".
       COPY "LIBLP/LPAVCR.CPY".
       COPY "LIBLP/LPRATE.CPY".
       COPY "LIBLP/LPIBPC.CPY".
       COPY "LIBLP/LPXDTE.CPY".
       COPY "LIBLP/LPMENU.CPY".
       COPY "LIBLP/LPSTAT.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/RBACT.CPY".
       COPY "LIBLP/SETCTBL.CPY".
       COPY "LIBLP/ADDONSPR.CPY".
       COPY "LIBGB/GPENV.CPY".
       COPY "LIBLP/POSTDATE.CPY".
       COPY "LIBGB/TIMEIO.CPY".

      *******************************************************
       FORM-RESET SECTION.
      *******************************************************
           MOVE TRANS-DATE    TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY   TO VDU-DATE-EDIT.
           MOVE VDU-DATE-EDIT TO LONPF-TRDATE.

           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.

      **************************************************************************
      * MOVED DESTROY ALL CONTROL TO THIS LOCATION.
      * EVERY TIME A LOAN IS PULLED UP WE ARE REDISPLAYING ALL THIS INFO
      * BUT WE ARE NOT DESTROYING IT UNTIL WE EXIT TO THE MENU.  THE
      * RESULT IS THAT THE SCREEN WILL SIT AND FLICKER REPEATEDLY UNTIL
      * ALL COPIES OF THE SCREEN HAVE BEEN DESTROYED.  CLEARING IT HERE
      * EACH TIME APPEARS TO CORRECT THAT SITUATION. /MJD
      **************************************************************************
           DESTROY ALL CONTROLS.

           DISPLAY LONPF-SCREEN UPON TCLP-MAIN-WINDOW-HANDLE.
                 
           MOVE LONPF-FORM-FIELDS TO WR-BUF.
           MOVE LONPF-FORM-LIST   TO WR-LIST.
           PERFORM CALL-WRFORM.

           MOVE PROMPT-INFO-LIST      TO WR-LIST.
           MOVE PROMPT-INITIAL-VALUES TO WR-BUF.
           PERFORM CALL-WRFORM.

           MOVE "P19." TO WR-LIST.
           MOVE "OV:"  TO WR-BUF.
           PERFORM CALL-WRFORM.

           MOVE "APPLIED-PRMT."            TO WR-LIST.
           MOVE " _______APPLIED_________" TO WR-BUF.
           CALL "C$TOLOWER" USING WR-BUF, VALUE 25.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      ***************************************************
       SETUP-LINK-INFO SECTION.
      ***************************************************
           IF VDU-FORM-NAME = "LPSMNU"
              MOVE LPSMNU-HELP--FILE TO HELP-LINK-FILE
              MOVE LPSMNU-HELP--DIR  TO HELP-LINK-DIR
           ELSE
              MOVE LONPF-HELP--FILE TO HELP-LINK-FILE
              MOVE LONPF-HELP--DIR  TO HELP-LINK-DIR.

      ***************************************************
       VDU-CONVERT-FIELD SECTION.
      ***************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBLP/LONPF_EVA.CPY".
              COPY "LIBLP/LPSMNU_EVA.CPY".
              COPY "LIBLP/LPLNMU_EVA.CPY".
              COPY "LIBLP/LPCAMU_EVA.CPY".
              COPY "LIBLP/LPVDMU_EVA.CPY".
              COPY "LIBWI/WRLDBK_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ***********************************************
       MISC-PARAGRAPHS SECTION.
      ***********************************************
       COPY "LIBGB/SYSTEM.CPY".
       COPY "LIBGB/GETENV.CPY".
       COPY "LIBGB/ACCESS.CPY".
       GET-COLLATERAL.
           MOVE LN-OWNBR  TO LTC-BRNO.
           MOVE LN-ACCTNO TO LTC-ACCTNO.
           MOVE 1         TO LTC-SEQNO.
           PERFORM OPEN-LTC1-FILE.
           PERFORM READ-LTC1-FILE.
           PERFORM CLOSE-LTC1-FILE.
           IF IO-FG NOT = 0
              MOVE SPACES TO LTC-COLLATERAL-INFO.

       RESET-PARTIAL-SCREEN.
           MOVE DISPLAY-LOAN-LIST TO WR-LIST.
           MOVE SPACES            TO WR-BUF.
           PERFORM CALL-WRFORM.

           MOVE DISPLAY-DELINQ-LIST TO WR-LIST.
           MOVE SPACES              TO WR-BUF.
           PERFORM CALL-WRFORM.

           MOVE DISPLAY-BAL-LIST TO WR-LIST.
           MOVE SPACES           TO WR-BUF.
           PERFORM CALL-WRFORM.

      *    MOVE HIGH-LIST-ATTR TO WR-LIST.
           MOVE WORLD-CREDIT-LIST-ATTR TO WR-LIST.
           MOVE SPACES         TO WR-BUF.
           PERFORM CALL-WRFORM.

           MOVE OPT-OUT-LIST TO WR-LIST.
           MOVE SPACES       TO WR-BUF.
           PERFORM CALL-WRFORM.

       DISPLAY-INSURANCE-TRACKING.
           MOVE LN-OWNBR  TO LTA-BRNO.
           MOVE LN-ACCTNO TO LTA-ACCTNO.
           MOVE 1         TO LTA-SEQNO.
           PERFORM OPEN-LTA1-FILE.
           PERFORM READ-LTA1-FILE.
           PERFORM CLOSE-LTA1-FILE.
           IF IO-FG = 0
              MOVE LTA-TRACK-ONLY TO D-TRACK
              MOVE LTA-INSURANCE  TO D-INS
           ELSE
              MOVE SPACES TO D-TRACK D-INS.

           MOVE "N" TO D-CPP.
           IF LN-INSEFF-DATE(5) NOT = 0
              IF LN-INSEFF-DATE(5) NOT > TRANS-DATE
                 IF LN-INSEXP-DATE(5) NOT = 0
                    IF LN-INSEXP-DATE(5) > TRANS-DATE
                       IF LN-O2-REBATE = " "
                          MOVE "Y" TO D-CPP.

           MOVE DISPLAY-TRACKING TO D-MEMO.
       GET-SP.
           MOVE LN-ORGST    TO SP-ORGST.
           MOVE LN-SPRCLASS TO SP-SPRCLASS.
           MOVE LN-SUBCLASS TO SP-SUBCLASS.
           MOVE LN-LAWCODE  TO SP-LAWCODE.
           IF LPWSSP-SP1-KEY NOT = SP1-KEY
              MOVE SP1-KEY TO LPWSSP-SP1-KEY
              PERFORM READ-SP1-FILE
              IF IO-FG NOT = 0
                 GO TO IO-ERROR.
       SEND-LEGEND.
           MOVE "S  A000MSEL." TO VDU.
           MOVE MESS           TO VDUBUF.
           PERFORM SCREENIO.
       SET-FULL-BRIEF.
           IF FULL-DISPLAY = "B"
              MOVE "F" TO FULL-DISPLAY VDUBUF
              MOVE "FULL LOAN DETAIL LINE DISPLAY" TO MESS
           ELSE
              MOVE "B" TO FULL-DISPLAY VDUBUF
              MOVE "BRIEF LOAN DETAIL LINE DISPLAY" TO MESS.
       GET-DELINQ-VALUE.
           IF DELINQ-CODE = "R"
              MOVE LN-RECPROFILE(1) TO PROFILE-EDIT
              INSPECT PROFILE-ED REPLACING ALL "9" BY "-"
              MOVE PROFILE-ED       TO DELINQ-VALUE
           ELSE
           IF DELINQ-CODE = "C"
              MOVE LN-CONPROFILE(1) TO PROFILE-EDIT
              INSPECT PROFILE-ED REPLACING ALL "9" BY "-"
              MOVE PROFILE-ED       TO DELINQ-VALUE
           ELSE
              MOVE LN-1STPYDATE TO CEPP1-DATE
              MOVE TRANS-DATE   TO CEPP2-DATE
              MOVE MAKER-CD     TO CEPP-MAKERCD
              IF GP-NO-LC-IN-ARREARAGE
                 PERFORM CONTRACTUAL-DELINQUENCY
                 MULTIPLY -1 BY CDEL-DELINQUENT GIVING DELINQ-NUM
              ELSE
                 PERFORM TOTAL-DELINQUENCY
                 MULTIPLY -1 BY CDEL-ARREARAGE GIVING DELINQ-NUM.
       SEND-DELINQ-SCAN.
      *    X IS INITIAL VALUE
           IF DELINQ-PROF1 = "X"
              MOVE "D" TO DELINQ-PROF3.
           MOVE DELINQ-PROF3     TO DELINQ-CODE.
           IF DELINQ-CODE = "R"
              MOVE "REC/DELINQ"  TO DELINQ-PROMPT
           ELSE
           IF DELINQ-CODE = "C"
              MOVE "CONTRACTUAL" TO DELINQ-PROMPT
           ELSE
              MOVE "DELINQUENCY" TO DELINQ-PROMPT.
       GET-LC.
           MOVE LN-CLASS TO LC-CODE.
           PERFORM OPEN-LC1-FILE.
           PERFORM READ-LC1-FILE.
           MOVE LC-DISPLAY-ERN     TO HOLD-DISPLAY-ERN.
           IF IO-FG = 9
              MOVE "INVALID CLASS" TO LC-DESC.
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
           IF LN-MESSAGE NOT = " "
              PERFORM ALARM
              PERFORM BLINK-STARS 3 TIMES
              PERFORM ALARM.
       DISPLAY-MAKER.
           IF MAKER-CD = "M"
              MOVE "MAKER" TO D-MAKER.
           IF MAKER-CD = "J"
              MOVE "JOINT" TO D-MAKER.
           IF MAKER-CD = "E"
              MOVE "ENDORSER" TO D-MAKER.
       NUMBW-TO-ALPBW.
           MOVE NUM-BW1 TO ALP-BW1.
           MOVE NUM-BW2 TO ALP-BW2.
           MOVE NUM-BW3 TO ALP-BW3.
           MOVE "-" TO ALP-HYPHEN1 ALP-HYPHEN2.
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
           MOVE "XX"      TO BK-REAFF-STAT.
           MOVE LN-OWNBR  TO LTB-BRNO.
           MOVE LN-ACCTNO TO LTB-ACCTNO
           MOVE 1         TO LTB-SEQNO.
           PERFORM OPEN-LTB1-FILE.
           PERFORM READ-LTB1-FILE.
           PERFORM CLOSE-LTB1-FILE.
           IF IO-GOOD
              IF NOT (LTB-REAFF-DATE = 0)
                 AND (LTB-DISCHARGE-DATE = 0)
              MOVE "00" TO BK-REAFF-STAT.

      *********************************
       IO-ROUTINES SECTION.
      *********************************
       COPY "LIBGB/CONNECT_SQL.CPY".
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
       COPY "LIBLP/LPLTA1RN.CPY".
       COPY "LIBLP/LPLTC1RN.CPY".
       COPY "LIBLP/LPLTB1RN.CPY".
       COPY "LIBLP/LPLTI1RN.CPY".
       COPY "LIBLP/LPLTP1RN.CPY".
       COPY "LIBLP/LPLC1RN.CPY".
       COPY "LIBLP/LPSP1RN.CPY".
       COPY "LIBLP/LPLN1RN.CPY".
       COPY "LIBLP/LPLN4RN.CPY".
       COPY "LIBLP/LPLN5RN.CPY".
      *COPY "LIBLP/LPLN6RN.CPY".
      *COPY "LIBLP/LPLN7RN.CPY".
       COPY "LIBLP/LPBW1RN.CPY".
       COPY "LIBLP/LPBW2RN.CPY".
       COPY "LIBLP/LPBW3RN.CPY".
       COPY "LIBLP/LPBW4RN.CPY".
       COPY "LIBLP/LPWKI.CPY".
       COPY "LIBLP/LPBM1RN.CPY".
       COPY "LIBLP/LPLM1IN.CPY".
       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO ERRCD.

      *********************************
       END-ROUTINE SECTION.
      *********************************
       END-RUN.
           IF LN-REC NOT = " "
              MOVE LN-ACCTNO TO LOAN-KEY
              MOVE BW-SSNO   TO BORROWER-KEY.
           PERFORM CLOSE-FILES.
       EXIT-PROG.

      * DO NOT DESTROY ANY SCREENS HERE, CONTINUING TO LONPF1 OR
      * EXITING TO DRIVER
      * 5/2/2024 BAH I COMMENTED THIS OUT. I DONT THINK WE NEED IT. THERE
      * ARENT ANY WINDOWS OPEN. MAYBE THIS IS CAUSING THE CURSOR FLICKER?
      * I CHANGED THE BWLNSC AND BWLNS2 TO CALLED PROGRAMS AND THE BANKRUPT
      * WINDOW ALREADY DOES A CLOSE WINDOW. LETS TRY IT!
      *    PERFORM CLOSE-WINDOW UNTIL WINDOWS-OPEN = 0.
       STOP-RUN.
           EXIT PROGRAM.
