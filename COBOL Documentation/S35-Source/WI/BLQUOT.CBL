       IDENTIFICATION DIVISION.
       PROGRAM-ID.      BLQUOT.
       DATE-WRITTEN.    092597.
      *************************************************************************
      *
      *  DESCRIPTION:   BALLOON DIRECT LOAN QUOTER
      * REV:
      *  JTG 062195 EXPANDED FOR 10 INSURANCES
      *  JTG 062395 ADDED OPTION FOR BR-SFDLO3-WINDOW-FG
      *  MJD 081195 ADDED LOGIC FOR GP-THE-ONLY-SF-SRCD.
      *  JTG 093095 ADDED LOGIC FOR GP-DEFAULT-O3-INS-COMPANY
      *  JTG 011796 CHANGED SO O2 & O3 INS. DON'T COMPUTE DEFAULT EXPIRATION
      *             DATE UNLESS INITIAL INPUT
      *  MJD 021996 ADDED OPTION TO BR-SFDLO3-WINDOW-FG TO OPEN WINDOW IF
      *              FLAG = "B" (RE. RELIANCE.)
      *  MJD     96 ADDED TEST OF LC-AFP-CLASS AND DL-AFP-DEALER.
      *  MJD 041996 ADDED GP-DLR-DISP-TYPE.
      *  BAH 062896 ADDED INSEXP-DATE(5) & INSEXP-DATE(6) TO MAKE-DATES-CURRENT
      *             SO WHEN YOU RECALL A QUOTE, IT UPDATES THOSE DATES.PL#96
      *  JTG 111296 CHANGED LN-INSTBL (LEVEL'S 9 & 10) TO FILLER
      *  BLV 112296 CHANGED COMPUTE-INSCOMM TO 8 TIMES, NOT 10
      *  JTG 042197 ADDED 'LPZ3CL' ROUTINE TO CLEAR CIP/COP/CEP WORKERS
      *  JTG 042497 CHANGED TO DISPLAY UNIT PERIOD INFO IN ALL CASES
      *             EX. PD-INTDATE CHANGE, PD-1STPYDATE CHANGE,
      *                 PD-ORGTERM CHANGE, PD-UNITPER-CD CHANGE
      *  JTG 053097 CORRECTED BAD EDIT ON SPR'S (RE: AL P $100) EDIT WAS
      *             BEING TESTED BEFORE INPUT OF SELLING PRICE
      *  MJD 060397 ADDED TO SCSCAN-TRANS-DATE.
      *  JTG 062797 CHANGED TO RECOMPUTE PURCHASE FIELDS WHEN DEALER NO. CHANGES
      *  JTG 070197 CHANGED LOAN CLASS SCAN & QUOTE RECALL TO ALLOW
      *             SERVICE ACCOUNTS
      *  JTG 071797 CORRECTED BAD EDIT ON SPR'S (RE: AH P) EDIT WAS
      *             BEING TESTED BEFORE INPUT OF DOWN PAYMENT #P137
      *  BLV 072497 DON'T ALLOW PRINCIPAL 1 IF GP-DLRVPRIN-1-FG NOT "Y"
      *  JTG 081497 ADDED SET OF PD-GIFTNET WORLD A90
      *  JTG 082197 CHANGED TO USE 'LPZ3CL' TO CLEAR-CALC-AREA-Z3
      *  JTG 090497 CHANGED COPY 'LIBWI/FEEET1W' TO 'LIBWI/FEEENTW'
      *                          'LIBWI/FEEENT1' TO 'LIBWI/FEEENT'
      *                          'LIBWI/FEEENTPD1' TO 'LIBWI/FEEENTPD'
      *  GLF 090997 REPLACED 'IO' ON GP-FILE WITH GET 'GPENV'
      *  JTG 121597 ADDED TEST FOR CIP-NO-OF-TIMES-CALLED < 800, TO ALLOW RESET
      *             OF CEP FIELDS, WDT # 148, CASTLE #163 RE: RATE OVERRIDE
      *  JTG 011598 CHANGED TO EXIT BORROWER-QUOTED ROUTINE WITH IO-FG = N9,
      *             IF DUPLICATE GIFTNET ENTERED
      *  JTG 020498 SET CIP-CAN-LOAN-NO, RE: LOGIC TO FORCE ONE CALENDAR MONTH
      *             SEE CALCZ3.C
      *  JTG 032498 CHANGED TO LET SINGLE PAYS WORK WITH 'B'-BALLOON
      *  BAH 040698 GP-DEFAULT-O3-INS-COMPANY 0 WAS NOT ALLOWING ANY INS CO.
      *  JTG 040698 CORRECTED BUG RE: NOT RECALCULATING AFTER DETERMINING
      *             RESTRICTION FOR GA,SC
      *  BAH 042998 MOVE BW-ORIGIN TO PD-ORIGIN, FEDERATED PR# 646
      *   CS 042998 ADDED "RB" AND "RO" RENEW BANKRUPT & RENEW OTHER
      *  JTG 061798 CHANGED TEST ON SRCD AGAINST PRIOR LOAN PD-PO-TRCD
      *             AND PUT BACK STOP SO 'SC' IS ONLY VALID SF RENEWAL
      *  KEC 072198 ADDED CHECK FOR NEW FLAG ON 'SPR' CAN-LOAN FLAG
      *              ( SP-CAN-LOANS-FG ).  FOR WORLD          PR#: 00187
      *  JTG 081898 ALLOWED 'PB' ON BANKRUPT ACCOUNTS IF GP-I-AM-WORLD <JGW
      *  JTG 081898 ALLOWED 'PB' ON BANKRUPT ACCOUNTS IF GP-I-AM-WORLD <JGW
      *  JTG 081998 CHANGED TO NOT DISPLAY'NB' AS DEFAULT SOURCE CODE
      *  KEC 090498 ADDED 'MOVE LC-LOANTYPE TO PD-LOANTYPE';
      *              FOR REGENCY REVOLVING.
      *  JTG 092598 CHANGED CALL FROM LONPZ3 TO CALCZ3
      *  JTG 101398 ADDED SET OF LCSCAN-REVOLVING = 'N' AND STOPPED ENTRY
      *             OF REVOLVING CLASSES, REGENCY REVOL
      *  JTG 101298 SET PDSCAN-REVOLVING = 'N', REGENCY REVOL
      *  JTG 110598 CORRECTED DISPLAY-APP-BW-LISTWAS MISSING '.' END OF LIST
      *             ALLOWED RECALL WITH ERROR-IN-PRIOR-LOAN WILL BE CAUGHT IN
      *             ATTEMPT TO UPDATE
      *  JTG 122198 REPLACED SP-SCFRMLA = 1 OR 8 OR 28 TESTS
      *             WITH 88 SP-SCFRMLA-POINTS
      *  JTG 990113 ADDED SP-EDT-TYPE 'P' POINTS,                   REGENCY #012
      *  GLF 990312 ADD WI/TFCCDS FOR TFC ENTRY OF LPO,REP CODE,PROCESS CODE
      * KEC 1999.0413 ADDED CHECK OF 'SPR' FOR SELECTING A CAN LOAN.
      * JTG 990413 ADDED CHECK OF 'SPR' FOR SELECTING A NON CAN LOAN.
      *  MJD 990806 ADDED GP-INCL-OTH-IN-EARNED FLAG. IF "Y" THEN INCLUDE
      *             EARNINGS BUCKET 6 (OTHER) IN ACCUMULATED TOTALS
      *             OTHERWISE EXCLUDE BUCKET 6.
      * MJD 991109 FORCE INPUT OR EXP DATE ON O3 BY DAFAULTING TO 00/00/00
      *            INSTEAD OF MATURITY DATE
      *            IF A SAVERD LOAN IS RECALLED, DO NOT RESET EXP DATE
      *            ON O3 INS (REGACC PR #985)
      * JTG 000113 ADDED SP-INSREJECT-FG OPTION 'C', WHICH WILL CAUSE WRITING
      *            NO. OF MONTHS INSURANCE NOT IN VIOLATION, WISCONSIN #1032
      * MJD 000207 ADDED GP-OT-INS-TERM (TFC PR#48) IF 'Y' THEN ALLOW
      *            ALLOW TERM ENTRY FOR O2O3O4O5 INSURANCE.
      *  MJD 000224 ADDED PASSWORD CHECK FOR REGMGM. IF THEY ENTER
      *             A PD-OVRDRATE-FG NOT = TO "A" THEN CALL VERIFY-PASSWORD.
      *             (REGMGM PR# 1006)
      *  MJD 000228 ADDED GP-ADV-TO-1ST-NO-CORR = "A" . IF DATE OF THE LOAN
      *             IS ON THE 26 THRU 31 THEN ADVANCE THE FIRST PAYMENT
      *             DUE DATE TO THE FIRST OF THE SECOND MONTH FOLLOWING THE
      *             DATE OF THE LOAN. (REGMGM PR#1099)
      *  JTG 000315  CORRECTED BUG WITH SRCD EX. FB AND PRIOR ACCOUNT NO. ENTRY
      *              WISC #71249 J006
      *  KEC 000517 {PR#01148} PARADATA FINANCIAL [LORI MCBEE]
      *             ADDED LOGIC TO HAVE RESCISSION IN A QUOTE.
      *  MJD 000518 ADDED SCFRMLA 39 - IN THIS PROGRAM IT FOLLOES THE SAME
      *             LOGIC AS 33 (SUPPORT PR#1181)
      *          GO TO END-PROCESS-PRIOR-LOAN    JTG 000518 CAUSED PROBLEMS AJ
      *  KEC 000530 {PR#00048} TFC [JUDY BRANIFF];
      *             ADDED THE FOLLOWING 'IF' CONDITION FOR ENTERING THE
      *             INSURANCE TERM (GP-OT-INS-TERM = "Y"):
      *                   IF ( VDUNUM0 > 360 )
      *                      GO TO ENTER-IC-TERM.
      * MJD 000605 CHANGED PROCEED-VALIDATION LOGIC TO WORK LIKE XONPC0.
      *            SKIP THE TEST ON PROCEEDS AMOUNT LOW. (AS PER JOHN)
      * JTG 000607 ADDED LOGIC FOR TESTING CALCZ3.C BY SETTING ENV
      *            VARIABLE "CALL_TESTZ3" WHICH MAKE THE CALL TO
      *            "LP/TESTZ3" INSTEAD OF "LP/CALCZ3"
      * MJD 000616 CHANGED OVER-SEQ TO TEST PASSWORD ON CL,AH,PP,01,MAINT FEE,
      *            1ST PY EXT, EXT LC INFO. THIS WILL MAKE IT CONSISTENT WITH
      *            XONPC0.
      * MG  000629 ADDED LOGIC FOR TENNESSEE RATE TYPE 'M' FLAT MONTHLY
      *            CHARGE, PR#1225
      * BAH 010518 ADDED SCFRMLA 42 (SUNBELT) LIKE 18 BUT 360 YEAR, PR# 1484
      * BAH 010607 ADDED SP-SCFRMLA = 43 FOR FCF "NC", PR# 1506
      * JTG 011001 DON'T ALLOW A FIRST PAY DAY OF 31 WHEN SEMI MONTHLY PAPER
      *            ADDED 9 1STPYDATE < 30 DAYS FROM LOAN DATE     FFG PR#333
      * JTG 010831 TESTED COP-RETURN-CODE = 'V' FOR DISPLAY OF ERROR IN CALC
      *                                                        LENDMARK PR#J021
      * JTG 020220 ADDED "CT" COUNTY TAX CODE KENTUCKY AND PERCENTS
      *            ADDED "CY" CITY   TAX CODE KENTUCKY AND PERCENTS  WORLD #288
      * BAH 020418 ADDED NCSCHG-SCFRMLA TO PASS SP-SCFRMLA FOR 45, SFS #1785
      * JTG 020603 CHANGED FOR (MIP) MONTHLY INSURANCE PREMIUMS   REGENCY #1801
      * KEC 020806 {DR#00009} PARADATA <A15> [KEN]
      *            ADDED PD-RESCISSION AND PD-RESCISSION-DATE.
      * BAH 020807 ADDED SP-MFFRMLA 4 FOR "TN" MAINTFEE RESTRICTIONS, REGWAN
      *            PR# 1867
      * BLV 020820 IF GP-ENTER-LPO-FG = "Y", OPEN TFC WINDOW FOR ENTRY OF
      *            LPO, SOAUTO PR# 1885
      * CS  021008 DON'T ALLOW A FIRST PAY DAY OF 14, 15 OR 31 IF SEMI-MONTHLY
      *            PER JTG. CAUSED LATE CHARGE PROBLEM AT FFG, LATE CHARGE
      *            CYCLE DATES WERE ALTERED IN FEB, FOR EXAMPLE: IF DUE DAY
      *            15/30, FEB SET DAY TO 28, THEN MARCH SET TO 13TH INTEAD OF
      *            15TH.
      * JTG 030123 CHANGED TO GET SYSTEM DATE FROM FM/WI/LNQUOT SCREEN
      *            TO ALLOW 'SYSDATE_YYMMDD' ENV VARIABLE TO WORK
      * JTG 030430 CREATED FORMULA 12 LIKE 2 BUT SPREADS EXTENSION CHARGE
      *            OVER PAYMENTS  88  SP-PYEXFRMLA-SAME2-SPREAD VALUE 12.
      *                                                          CITIZENS #2006
      * BAH 030619 ADDED CIP-PRIOR-SUB-CLASS FOR SCFRMLA 46, LENDMARK PR# 2101
      *            AND PASS-PRIOR-SUB-CLASS (PAYOFX)
      * JTG 030910 CHANGED FOR REGENCY TO PASSWORD PROTEST RENEWALS OF LOAN
      *            CLASS 16 & 17 INTO ANY CLASS BUT CLASS 17
      *            ADDED PASSX-CURRENT-PD-CLASS TO HOLD PD-CLASS OF
      *            LOAN BEING MADE                                REGENCY #2135
      * JTG 030929 MODIFIED SCFRMLA 13 TO ONLY TAKE THE DUAL LOAN
      *            ADJUSTMENT INTO ACCOUNT WHEN THE LOANS'S CLASSES
      *            ARE THE SAME                               LENDMARK#2149
      * JTG 031001 CREATED 01  SCHG-WORKERS-2 FOR PASS FOR SCFRMLA 13
      *                                                       LENDMARK #2149
      * JTG 031015 CHANGED FOR REGENCY TO PASSWORD PROTEST RENEWALS OF LOAN
      *            CLASS 16 & 17 INTO ANY CLASS BUT CLASS (17 , 50-79)
      *            ADDED PASSX-CURRENT-PD-CLASS TO HOLD PD-CLASS OF
      *            LOAN BEING MADE  (2135)                        REGENCY #2172
      *  BAH 031023 ADDED GP-DEFAULT-O3-INSOURS, LENDMARK PR# 2180
      *  JTG 040210 ALLOWED NON MONTHLY CAN LOANS (NMQUOT.C) WORLD J043
      *  BAH 040304 ADDED SP-DEFAULT-O2-INS-COMP FOR LENDMARK PR# 2258
      *  BAH 040430 CLASS RENEWAL PASSWORDS BECAUSE OF TICO CONVERSION,
      *             SHARING THE RENEWAL-SEQ PASSWORD OPTION, REGWAN PR# 2286
      *  JTG 041206 PASSWORD PROTECTED FIELDS 27, 28, 29, 30
      *             CL, AH, PP, O1 FIELDS IN XONPC0, ZONPC0, LNQUOT AND BLQUOT
      *                                                              WORLD #J051
      *  JTG 041209 N O T E:  N O  PASSWORD TEST ON ABOVE
      *                       IF CHANGING 'PP' AND PD-ORGST = "GA"
      *                              AND GP-FORCE-INPUT-PP-COVERAGE = 'Y'
      *                                                              WORLD #J051
      *  BAH 050222 WHEN ENTERRING SUB CLASS, YOU DONT KNOW THE LOANDATE YET TO
      *             CHECK IF SPR IS EXPIRED, SO IT WAS JUST GRABBING THE FIRST
      *             LAWCODE FOUND TO DISPLAY THE DESCRIPTION, WHICH ISNT
      *             CORRECT IF THEY EXPIRE AND BUILD A NEW ONE WITH A DIFFERENT
      *             DESCRIPTION, VERY CONFUSING...AT LEAST SKIP EXPIRED SPR.
      *             RIVERSIDE PL# 467
      *   CS 05     ADD TEST SP-INSURANCE-WRITTEN LENDMARK PR#2479
      *   CS 050426 IF INSURANCE PREM = ZERO, ZERO ASSOCIATED FIELDS, INSEFF,
      *             INSEXP, INSCOVR, AND INSCOMM. PREV WHEN USING A FLAT DOLLAR
      *             COMM, COMPUTED A COMMISSION EVEN WHEN PREMIUM IS ZERO
      *                                                    [PR#479 NSA]
      *  BAH 050513 IF GP-I-AM-WDT, ONLY CLASS 7 CAN RENEW INTO CLASS 7 AND
      *             CLASS 7 CAN NOT RENEW INTO ANYTHING ELSE, PR# 2528 FINMGM
      * JTG 050819 CHANGED SAVE LOAN, ETC TO USE PD-ANTICERN(4) FOR DISPLAY
      *            OF MAINTENANCE FEES INSTEAD OF THE STORED TRUNCTATED
      *            PD-MAINTFEE * PD-ORGTERM WHICH DOSEN'T EQUAL THE COMPUTED
      *            'VAT' VALUE ADDED TAX IN PD-ANTICERN(4)           WORLD #    
      * BAH 051128 UNDO CHANGE FROM 050222 PL# 467, WOULD NOT LET YOU BOOK A
      *            LOAN WITH AN EXPIRED SPR, AND YOU SHOULD BE ABLE TO.
      *            INSTEAD OF SKIPPING EXPIRED SPR IN READ-NEXT, JUST REDISPLAY
      *            THE SUBCLASS DESCRIPTION AFTER ENTERING THE LOANDATE.
      * JTG 060108 REMOVED SET OF CHFG(13) TO 'N' 1STPYDATE
      * JTG 060504 CHANGED LOGIC FOR SINGLE PAYS LOANS > 11 MONTHS
      *            TO YIELD REMAINING DAYS FROM UPER-INTDATE PLUS
      *            FULL UNIT PERIODS TO UPER-1STPYDATE
      *            ADDED NEW LOAN CALCULATION FOR RICHARD        LENDMARK #2709
      *
      *            STOPPED OPTION TO CALCULATE BALLOONS ON SINGLE PAYS
      *                                                           LENDMARK #2709
      * MJD 061212 ADDED TEST OF PRIOR LOANS SCFRMLA IF 48. REGENCY PR#2880
      * JTG 070326 WORLD SPR'S ARE FLAGGED WITH GP-ADV-TO-1ST-NO-CORR = "Y"
      *            WHICH WILL ADVANCE TO THE NEXT MONTH IF THE DEFAULT
      *            PAYMENT DUE DATE FALLS ON A NON EXISTING DAY
      *            THIS CHANGE OVERRIDES THIS OPTION FOR LOANS BOOKED WHEN
      *            GP-I-AM-WORLD AND PD-ORGST = "TN" AND PD-CLASS = 01 AND
      *            PD-ORGTERM = 12                                   WORLD 0532
      * BAH 070730 ADDED SCFRMLA 49 FOR REGMGM PR# 3017
      * BAH 090904 ALLOW SP-INSMAXCHGABL-OVRIDE TO WORK WITH O1, FINMGM PR# 3447
      * BAH 091015 PASS SFDLO3-ORGST TO CL/SFDLO3 FOR REGMGM PR# 3475
      *  CS 100720 ADDED TEST IF GP-I-AM-WORLD COMPARE PAYCODE ENTERED TO SP-UNI      *            PERCD, MUST MATCH IF WORLD DON'T ALLOW MONTHLY FROM BALLOON
      *                                                        WORLD PR#692
      * MJD 100715 FIXED TO CORRECTLY ALLOW MAINTFEE'S OVER 99.99 FOR MEXICO.
      * BAH 110106 ADDED SCFRMLA 50 FOR WORLD "IL" PR# 731
      * BAH 110127 RENAMED CASCHG-ORGST TO CASCHG-SCFRMLA, REGMGM PR# 3721
      * BAH 110216 CHANGE TO SCFRMLA 50 FOR 3/21/11, WORLD PR# 731
      * BAH 110321 SCFRMLA 50 FOR NEW LOANS, EXIT WITH 100.00 IN
      *            CIP-RESTRICT-ADJ SO CALCZL WILL BE CORRECT, WORLD 731
      * BAH 110523 PASSWORD PROTECT ADDON SOURCE CODES, ET, LP, NP, RT, ST
      *            AND RB, REGMGM PR# 3792
      * BAH 110601 ADDED SP-SCFRMLA 52 AND 53 FOR MULLEN PR# 3799
      * BAH 111109 CHANGE SCFRMLA 47 TO BE MORE LIKE 49, CALL CASC16 TO CHECK
      *            ONLY 1 WITHIN 12 MONTH PERIOD, BAY PR# 3898
      * MJD 111209 ADDED LOGIC FOR SP-ADV-TO-1ST-NO-CORR
      *            IF SET THIS WILL OVERIDE THE GP FLAG.             71226 #3879
      * BAH 111229 ADDED PP-PREM-SEQ TO PASSWORD CHANGING PP PREM REGWAN PR#3935
      * BAH 130208 RENAMED CLASS17-SEQ TO RENEWAL-SEQ TO SHARE WITH OTHERS
      *            ADDED A RENEWAL-SEQ CASE FOR REGMGM, PR# 4209
      *  CS 130315 REMOVED CALL-TMMSG MISC PARAGRAPH ITS IN DECLARATIVES
      * BAH 130314 PR# 4209 SHOULD ONLY BE FOR "TN", REGMGM PR# 4209A
      * MJD 130604 ADDED LOGIC FOR SP-SCFRMLA = 54.  FUNCTIONS THE SAME AS 12
      *            BUT INCLUDES AN EXTRA 4% ADDED IN AT THE END BASED
      *            ON TOTAL NOTE                                     WORLD #0874
      * BAH 130814 ADDED SCFRMLA = 55, CROSS BETWEEN 23 AND 32, WORLD PR# 918
      * BAH 131213 ADDED SP-SCFRMLA 56, LIKE 6 WITH RESTRICTIONS, WORLD PR# 953
      *            ALSO ADDED NCSCHG-ORGST AND NCSCHG-CLASS
      * MJD 140429 REPLACED YIELD WITH WS-YIELD, RESERVE WORD
      * BAH 150508 ADDED SCFRMLA 57, COMBO OF 23 AND 46, NSA PR# 4747
      * BAH 151104 ADDED SP-RENEWAL-EDIT "D" AND "E" FOR REGMGM PR#4885
      *
      * KEC 2016.0310 {PD#00003} PARADATA <A30> [JAY CISZEWSKI]
      *         CHANGED CD-GROUP, CD-SECURED & CD-TRW TO BE
      *         CD-CO-GROUP, CD-CO-SECURED & CD-CO-TRW.
      *
      * KEC 2016.0318 {PD#00003} PARADATA <A30> [JAY CISZEWSKI]
      *         CHANGED CD-FEE???? TO CD-GC-????.
      *
      * KEC 2016.0328 {PD#00003} PARADATA <A30> [JAY CISZEWSKI]
      *         CHANGED CDSCAN-WINDOW-BUF TO HAVE MATCHING CD1-KEY
      *         LAYOUTS (FOR EACH CODE TYPE; CD-TYPE).
      *         CDSCAN-CD1-KEY IS THE NEW (PRIMARY) WITH REDEFINES OF
      *         CDSCAN-??-KEY (NEW).
      *
      * BAH 160425 FOUND BUG WHILE DUAL LOAN TESTING, WAS MOVING 0 TO
      *            IO-FG IN VERIFY-PROCEEDS FROM PR#4885 WHICH CAUSED
      *            THE VERIFY-REQ-FLDS TO NOT STOP YOU, ONLY DISPLAY
      *            THE ERROR MESSAGE. SUPPORT/REGMGM PR#5060
      * BAH 160427 ADDED SP-SCFRMLA 58, WORLD PR#1154
      *  CS 160805 CHANGED SCSCAN-KEY TO SCSCAN-SP1-KEY
      *            REMOVED  "LIBCL/LOANNOW.CPY".
      *            REMOVED "LIBCL/LNSEQNW.CPY".
      *            REMOVED "LIBCL/MORELNW.CPY
      *            REMOVED  PASS-LN-MORELN = "Y" TESTS, COPY LIBWI/MORELNW
      *            NOT IN A30
      *            REMOVE  CALL TO "CL/LOANNO" 
      *            REMOVED PERFORM CHECK-FOR-MORELN
      * MJD 160824 ADDED LC-CALC-HI-CREDIT = "L"              61000/1180
      * BAH 161018 CHANGED WK-SAVE-RECORD TO HOLD-PD-RECORD LIKE BARB
      *            DID IN LNQUOT
      *
      * KEC 2017.0227 {PR#00000} PARADATA <A30> [JAY CISZEWSKI]
      *         CHANGED PROCEEDS-LOW, PROCEEDS-HIGH, POINTS-LOW,
      *          POINTS-HIGH TO MATCH SIZE OF SP-EDT-AMT 9(7)V99.
      *
      *  BAH 170407 ACTIVATE 8 DIGIT DATES, PD0006
      * BAH 171228 OPERATIONS WANTS ALL BRANCHES TO HAVE ABILITY TO ADJUST PP
      *            INSURANCE WHEN BUILDING A LOAN, NOT JUST "GA", WORLD #1292
      * BAH 181011 ADDED TXSCHG-CLASS FOR SCFRMLA 58, WAS NOT TESTING
      *            CLASS, #1341
      * BAH 190114 GLOBAL PDFILE
      * BAH 2019.0529 ADDED SP-SCFRMLA 59 KY HB225 WORLD PR#1374
      * BAH 20190625 REMOVED LC-REVOLVING, PT#0153
      * BAH 20190716 SAVE QUOTE, PULL UP AGAIN, SAVE AGAIN, WOULD NOT BE
      *              IN QUOTE SCAN ANYMORE! NEEDED TO SAVE PD2-BWNAME IN
      *              SAVE-BW-NAME FROM A FLDVAL IN A15. QA0049
      * BAH 20200615 MOVED OPEN-IC1-FILE, OPEN-SP1-FILE, OPEN-CD1-FILE,
      *              OPEN-BW1-FILE TO TOP OF PROGRAM #TST001
      * BAH 20201207 CHANGE VIA-QUOTE-SCAN, DONT MOVE IN THE PDSCAN-NAME,
      *              YOU HIT END OF FILE WHEN ONLY 1 QUOTE IN THE PDFILE.
      *              MOVE SPACES TO PD2-BWNAME AND ADD A CHECK OF THE
      *              PD2-BWNAME MATCHES THE PDSCAN-BWNAME OR GO GET NEXT.
      * BAH 2021.0713 PASSWORD FIELD 31, OTHER 2 INSURANCE #1504
      * BAH 2023.0314 FIXED CHECK ON SP-EDT-NOEDITS CINDY FOUND
      * BAH 2024.0401 REMOVED CASCHG-YEARTYPE, CASCHG-ERRCD
      * BAH 2024.0416 REMOVED LNQUOT-GIFTNET-FG
      * BAH 20241010 ADDED SP-SCFRMLA 63 "WI" #1682 S35Q-141
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
           "@(#)WI/BLQUOT.CBL S35 04/19/24-12:04:31 >".
      **********************************
      *       F I L E   P A T H S      *
      **********************************
       COPY "LIBGB/GB01GB.CPY".
       COPY "LIBGB/GB01GB_SQL.CPY".
       COPY "LIBGB/GBWSGB.CPY".
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBLP/LP01CL.CPY".
       COPY "LIBLP/LP01CL_SQL.CPY".
       COPY "LIBLP/LPWSCL.CPY".
       COPY "LIBLP/LP01LC.CPY".
       COPY "LIBLP/LP01LC_SQL.CPY".
       COPY "LIBLP/LPWSLC.CPY".
       COPY "LIBLP/LP01DL.CPY".
       COPY "LIBLP/LP01DL_SQL.CPY".
       COPY "LIBLP/LPWSDL.CPY".
       COPY "LIBLP/LP01SP.CPY".
       COPY "LIBLP/LP01SPA.CPY".
       COPY "LIBLP/LP01SPB.CPY".
       COPY "LIBLP/LP01SPC.CPY".
       COPY "LIBLP/LP01SP_SQL.CPY".
       COPY "LIBLP/LPWSSP.CPY".
       COPY "LIBLP/LP01BW.CPY".
       COPY "LIBLP/LP01BW_SQL.CPY".
       COPY "LIBLP/LPWSBW.CPY".
       COPY "LIBGL/GL01GI.CPY".
       COPY "LIBGL/GL01GI_SQL.CPY".
       COPY "LIBGL/GLWSGI.CPY".
       COPY "LIBLP/LP01CD.CPY".
       COPY "LIBLP/LP01CD_SQL.CPY".
       COPY "LIBLP/LPWSCD.CPY".
       COPY "LIBLP/LP01CDB.CPY".
       COPY "LIBLP/LP01CDB_SQL.CPY".
       COPY "LIBLP/LPWSCDB.CPY".
       COPY "LIBLP/LP01IC.CPY".
       COPY "LIBLP/LP01IC_SQL.CPY".
       COPY "LIBLP/LPWSIC.CPY".
       COPY "LIBLP/LP01PD.CPY".
       COPY "LIBLP/LP01PD_SQL.CPY".
       COPY "LIBLP/LPWSPD.CPY".

       01  WK-IFN          PIC X(2) VALUE "WK".
       01  WK-KEY          PIC 9(2).


       01  PASSX-CURRENT-PD-CLASS      PIC 9(3).

       01  INITIAL-CAN-ENTRY-FG        PIC X.
       01  MSEL-LIST       PIC X(5)     VALUE "MSEL.".
       01  WSEL-LIST       PIC X(5)     VALUE "WSEL.".
       01  ONE             PIC S9.
       01  ELE             PIC 9(6) COMP.
       78  CL-ELE          VALUE 27.
       78  AH-ELE          VALUE 28.
       78  PP-ELE          VALUE 29.
       78  O1-ELE          VALUE 30.
       78  O2-ELE          VALUE 31.
       78  O3-ELE          VALUE 32.


       01  FEECODE-TABLE   VALUE " ".
           03  FEECODE     OCCURS 4 TIMES
                           PIC XX.
       01  CDCODE-FEE-FG   PIC 9.
       01  LEGEND          PIC X(70).

      *    ----------------------------------------------------
      *    SINCE QUOTERS DON'T HAVE BORROWERS THESE WORKERS WILL
      *    BE PASSED TO CALCZL/CALCZ3, '   ' == NO CODE
      *    ----------------------------------------------------
       01  STATE                      PIC X(2)   VALUE " ".
       01  CITY-TAX-CD                PIC X(3)   VALUE " ".
       01  COUNTY                     PIC X(3)   VALUE " ".
      *    ----------------------------------------------------

       01  SAVE-MAINTFEE    PIC 9(6)V99  COMP.
       01  ANTIC            PIC S9(6)V99 COMP.

       01  ACTION.
           03  CHKEY       PIC X(4).
           03  CHNO        PIC 9(2).
       01  IO-TYPE         PIC X(3).
       01  NEW-FLAG        PIC 9.
       01  FIRST-TIME      PIC 9  VALUE 0.
       01  FIRST-ELE       PIC 99 VALUE 0.
       01  NOREG-FG        PIC X  VALUE " ".
       01  SAVE-SSNO       PIC 9(10) COMP.
       01  HOLD-BWNAME         PIC X(33).
       01  HOLD-PRIOR-PO-TRCD  PIC XX.
       01  HOLD-PD-RECORD      PIC X(PD-SIZE) VALUE SPACES.
       01  ADV-TO-1ST-NO-CORR  PIC X VALUE " ".

       01  SUB             PIC 9(6) COMP.
       01  OVER-20         PIC 99.
       01  I-LEVEL         PIC 99   COMP.
       01  INSPECT-W       PIC X(86).
       01  ADD-ON          PIC X(25).
       01  ALLOW-NON-CANS  PIC X        VALUE "N".
       01  OVERRIDES-OK    PIC X        VALUE "N".
       01  APR-OK          PIC X        VALUE "N".
       01  DEFAULT-APR-FG  PIC X.
       01  CAN-LOAN-NO     PIC XX       VALUE " ".
       01  SAVE-BW-NAME    PIC X(24)    VALUE SPACES.
       01  BORR-CHG-FG     PIC X        VALUE " ".
       01  RECALL-ID-FG    PIC X        VALUE " ".
           88  RECALL-UPDATE            VALUE "Y".
       01  QUOTE-ID-FG     PIC X        VALUE " ".
           88  QUOTE-ONLY               VALUE "Q".
           88  QUOTE-CHANGED-TO-LOAN    VALUE "L".
       01  PRIOR-LOAN-STAT PIC X.
           88  ERROR-IN-PRIOR-LOAN      VALUE "E".
       01  LEGEND-01-NO-CAN.
           03  FILLER                   PIC X(30)  VALUE
           "F1, F7-EXIT, F4-QUOTE SCAN:".
       01  LEGEND-01-CAN-ONLY.
           03  FILLER                   PIC X(31)  VALUE
           "F7-EXIT, F2-CANNED LNS, F4-QUOT".
           03  FILLER                   PIC X(33)  VALUE
           "ES, ENTER 2 CHAR CANNED LOAN NO:".
       01  LEGEND-02.
           03  FILLER                   PIC X(30) VALUE
               "F7-MENU, RECALL F3-LOANS/F4-QU".
           03  FILLER                   PIC X(30) VALUE
               "OTES, ENTER LOAN CLASS:".
       01  LEGEND-03.
           03  FILLER                   PIC X(36) VALUE
               "F7, F1, F2-CAN LOANS, F4-QUOTES, ENT".
           03  FILLER                   PIC X(23) VALUE
               "ER 2 CHAR CANNED LOAN#:".
       01  LEGEND-04.
           03  FILLER                   PIC X(38) VALUE
               "F7-MENU, F4-QUOTES, ENTER LOAN CLASS:".

       01  WS-MESSAGES.
           03  MESSAGE-INVALID-SPR-CAN-LOAN.
               05  FILLER PIC X(25) VALUE "INVALID: STATE PARA. DOES".
               05  FILLER PIC X(25) VALUE " NOT ALLOW CAN LOANS!    ".

           03  MESSAGE-INVALID-SPR-CL.
               05  FILLER PIC X(25) VALUE "INVALID SUB CLASS NUMBER:".
               05  FILLER PIC X(25) VALUE " FOR CAN LOANS ONLY!     ".

           03  MESSAGE-INVALID-SPR-NON-CL.
               05  FILLER PIC X(25) VALUE "INVALID SUB CLASS NUMBER:".
               05  FILLER PIC X(25) VALUE " FOR NON CAN LOANS ONLY! ".


           03  MESSAGE-INVALID-SPR-PAY-CODE.
               05  FILLER PIC X(25) VALUE "INVALID SPR: USE DIRECT L".
               05  FILLER PIC X(25) VALUE "OAN ENTRY!               ".



       01  ALP-BW.
           03  ALP-BW123   PIC X(3).
           03  ALP-HYPHEN1 PIC X.
           03  ALP-BW45    PIC X(2).
           03  ALP-HYPHEN2 PIC X.
           03  ALP-BW6789  PIC X(4).
       01  NUM-BW          PIC 9(9).
       01  NUM-BWX REDEFINES NUM-BW.
           03  NUM-BW123   PIC X(3).
           03  NUM-BW45    PIC X(2).
           03  NUM-BW6789  PIC X(4).
       01  W-LNNO          PIC 9(8).

      *=================================================================
      *
      *    R E S C I S S I O N - W O R K E R S
      *
      *=================================================================
       01  RESCISSION-BUF                  PIC X(15) VALUE SPACES.
       01  RESCISSION-FLAG                 PIC X(01) VALUE SPACES.
       01  RESCISSION-INTDATE              PIC 9(08) VALUE ZEROES.
       01  RESCISSION-LIST                 PIC X(05) VALUE "R1:R.".
       01  RESCISSION-MESSAGE              PIC X(15)
                                               VALUE " * RESCISSION *".

       01  RESCISSION-POCD                 PIC X(02) VALUE SPACES.
           88  RESCISSION-POCD-INRESCISSION          VALUE "*I".

       01  RESCISSION-SECURED-COLLCD       PIC X(01) VALUE SPACES.
           88  RESCISSION-SECURED-COLLCD-YES         VALUE "R".

       01  RESCISSION-STATUS               PIC X(01) VALUE SPACES.
           88  RESCISSION-STATUS-BAD                 VALUE "X".
           88  RESCISSION-STATUS-GOOD                VALUE SPACES.

      ********************************
      *   S C R E E N   B U F F E R  *
      ********************************
       01  DISPLAY-BUF1        VALUE " ".
           03  D-LCNO          PIC Z9.
           03  D-LC-DESC       PIC X(16).
           03  D-010-040       OCCURS 3.
               05  D-010       PIC X(11).
               05  D-011       PIC X(33).
               05  D-012       PIC X(1).
           03  D-LOANAMT       PIC Z(6)9.99- BLANK ZERO.
           03  D-ANTIC         PIC Z(6)9.99- BLANK ZERO.
           03  D-050           PIC XX.
           03  D-051           PIC 9(4)      BLANK ZERO.
           03  D-052           PIC Z9.
           03  D-053           PIC X(12).
           03  D-060           PIC 99/99/99  BLANK ZERO.
           03  D-061           PIC 99/99/99  BLANK ZERO.
           03  D-070           PIC X(3).
           03  D-071           PIC X(15).
           03  D-080           PIC X(2).
           03  D-081           PIC X(15).
           03  D-090           PIC X(2).
           03  D-091           PIC X(15).
           03  D-100           PIC X.
           03  D-101           PIC 999.
           03  D-102           PIC X.
           03  D-110           PIC 9(6)      BLANK ZERO.
           03  D-111           PIC Z(5)9.99- BLANK ZERO.
           03  D-120           PIC ZZ9.
           03  D-130           PIC 99/99/99  BLANK ZERO.
           03  D-140           PIC Z(5)9.99- BLANK ZERO.
           03  D-141           PIC X.
           03  D-170           PIC Z(5)9.99- BLANK ZERO.
           03  D-171           PIC X.
           03  D-180           PIC X(3).
           03  D-181           PIC Z9.99     BLANK ZERO.
           03  D-182           PIC X.
           03  D-190           PIC Z(3)9.99  BLANK ZERO.
           03  D-1901          PIC X.
           03  D-220           PIC 9(4).99   BLANK ZERO.

       01  DISPLAY-BUF2        REDEFINES DISPLAY-BUF1.
           03  D-200           PIC Z(3)9.99  BLANK ZERO.
           03  D-201           PIC X.
           03  D-210           PIC Z(3)9.99  BLANK ZERO.
           03  D-211           PIC X.
           03  D-230           PIC X(20).
           03  D-231           PIC X(20).
           03  D-232           PIC X(20).
           03  D-260           PIC ZZ9.999   BLANK ZERO.
           03  D-261           PIC X.
           03  D-262           PIC ZZ9.999   BLANK ZERO.
           03  D-263           PIC ZZ9.999   BLANK ZERO.
           03  D-264           PIC ZZ9.999   BLANK ZERO.
           03  D-270-300      OCCURS 4.
               05  D-1COMP     PIC 99        BLANK ZERO.
               05  D-1OUR      PIC X.
               05  D-1TERM     PIC ZZ9       BLANK ZERO.
               05  D-1COVR     PIC Z(5)9.99  BLANK ZERO.
               05  D-1PREM     PIC Z(3)9.99  BLANK ZERO.
               05  D-INS-FG    PIC X.
           03  D-310-320      OCCURS 2.
               05  D-2COMP     PIC 99        BLANK ZERO.
               05  D-2OUR      PIC X.
               05  D-2EXP      PIC 9(6)      BLANK ZERO.
               05  D-2TERM  REDEFINES D-2EXP
                               PIC Z(6)      BLANK ZERO.
               05  D-2COVR     PIC Z(5)9.99  BLANK ZERO.
               05  D-2PREM     PIC Z(3)9.99  BLANK ZERO.
           03  D-340           PIC Z(5)9.99  BLANK ZERO.
           03  D-341           PIC X.
           03  D-350           PIC Z(5)9.99  BLANK ZERO.
           03  D-351           PIC X.
           03  D-360           PIC Z(5)9.99  BLANK ZERO.
           03  D-361           PIC X.
           03  D-370           PIC Z(6)9.99  BLANK ZERO.
           03  D-380           PIC Z(6)9.99  BLANK ZERO.
           03  D-381           PIC X.
           03  D-390           PIC Z(6)9.99  BLANK ZERO.
           03  D-400           PIC Z(5)9.99  BLANK ZERO.
           03  D-410           PIC Z(5)9.99- BLANK ZERO.
           03  D-420           PIC Z(5)9.99  BLANK ZERO.
           03  D-421           PIC X.
           03  FILLER          PIC X(131).

       01  DEF-BUF              REDEFINES DISPLAY-BUF1.
           03  DEF-ORGST        PIC XX.
           03  DEF-ORGBR        PIC 9(4).
           03  DEF-LOANDATE     PIC 99/99/99.
           03  DEF-INTDATE      PIC 99/99/99.
           03  DEF-PURCD        PIC XXX.
           03  DEF-UNITPER-CD   PIC X.
           03  DEF-UNITPER-FREQ PIC ZZ9.
           03  DEF-BALLBL       PIC X.
           03  FILLER           PIC X(350).

       01  DF2-BUF             REDEFINES DISPLAY-BUF1.
           03  DF2-SELLPR            PIC Z(5)9.99- BLANK ZERO.
           03  DF2-SELLPR-FG         PIC X.
           03  DF2-BALLOON           PIC Z(5)9.99- BLANK ZERO.
           03  DF2-BALLOON-FG        PIC X.
           03  DF2-POINTS            PIC Z9.99     BLANK ZERO.
           03  DF2-POINTS-FG         PIC X.
           03  DF2-FEE-1             PIC Z(3)9.99  BLANK ZERO.
           03  DF2-FEE-1-FG          PIC X.
           03  DF2-FEE-2             PIC Z(3)9.99  BLANK ZERO.
           03  DF2-FEE-2-FG          PIC X.
           03  DF2-OVRDRATE          PIC ZZ9.999   BLANK ZERO.
           03  DF2-OVRDRATE-FG       PIC X.
           03  DF2-EFFRATE           PIC ZZ9.999   BLANK ZERO.
           03  DF2-APRATE            PIC ZZ9.999   BLANK ZERO.
           03  DF2-YIELD             PIC ZZ9.999   BLANK ZERO.
           03  DF2-LNAMT             PIC Z(6)9.99- BLANK ZERO.
           03  DF2-ANTIC             PIC Z(6)9.99- BLANK ZERO.
           03  DF2-INS-INFO          OCCURS 4.
               05  DF2-INSCOMP       PIC 99        BLANK ZERO.
               05  DF2-INSOURS       PIC X.
               05  DF2-INSTERM       PIC ZZ9       BLANK ZERO.
               05  DF2-INSCOVR       PIC Z(5)9.99  BLANK ZERO.
               05  DF2-INSPREM       PIC Z(3)9.99  BLANK ZERO.
               05  DF2-INS-FG        PIC X.
           03  DF2-SERCHG            PIC Z(5)9.99  BLANK ZERO.
           03  DF2-SERCHG-FG         PIC X.
           03  DF2-MAINTFEE          PIC Z(5)9.99  BLANK ZERO.
           03  DF2-MAINTFEE-FG       PIC X.
           03  DF2-EXTCHG            PIC Z(5)9.99  BLANK ZERO.
           03  DF2-EXTCHG-FG         PIC X.
           03  DF2-INTCHG            PIC Z(6)9.99  BLANK ZERO.
           03  DF2-APR-FINAMT        PIC Z(6)9.99  BLANK ZERO.
           03  DF2-PROCEEDS-FG       PIC X.
           03  DF2-TREPAY            PIC Z(6)9.99  BLANK ZERO.
           03  DF2-1STPYAMT          PIC Z(5)9.99  BLANK ZERO.
           03  DF2-LASTPYAMT         PIC Z(5)9.99- BLANK ZERO.
           03  DF2-REGPYAMT          PIC Z(5)9.99  BLANK ZERO.
           03  DF2-REGPYAMT-FG       PIC X.
           03  DF2-RETURN-CODE       PIC X.
           03  DF2-LOOP-COUNT        PIC Z(3).
           03  FILLER                PIC X(88).

       01  DPUR-BUF                 REDEFINES DISPLAY-BUF1.
           03  DPUR-YIELD            PIC ZZ9.999   BLANK ZERO.
           03  DPUR-LNAMT            PIC Z(5)9.99- BLANK ZERO.
           03  DPUR-ANTIC            PIC Z(5)9.99- BLANK ZERO.

       01  DPUR-LIST.
           03  FILLER          PIC X(4)   VALUE
           "264 ".
           03  FILLER          PIC X(14)  VALUE
           "LOANAMT ANTIC.".


       01  DEF-LIST               PIC X(32)  VALUE
           "050 051 060 061 070 100 101 102.".

       01  FEE-LIST               PIC X(8)   VALUE
           "190 191.".

       01  DISPLAY-LIST1.
           03  FILLER             PIC X(49)  VALUE
           "LNKEY LCDESC 010 011 012 020 021 022 030 031 032 ".
           03  FILLER             PIC X(38)  VALUE
           "LOANAMT ANTIC 050 051 052 053 060 061 ".
           03  FILLER             PIC X(24)  VALUE
           "070 071 080 081 090 091 ".
           03  FILLER             PIC X(36)  VALUE
           "100 101 102 110 111 120 130 140 141 ".
           03  FILLER             PIC X(32)  VALUE
           "170 171 180 181 182 190 191 220.".

       01  DISPLAY-LIST2.
           03  FILLER          PIC X(32)  VALUE
           "200 201 210 211 230 231 232 260 ".
           03  FILLER          PIC X(36)  VALUE
           "261 262 263 264 270 271 272 273 274 ".
           03  FILLER          PIC X(56)  VALUE
           "275 280 281 282 283 284 285 290 291 292 293 294 295 300 ".
           03  FILLER          PIC X(56)  VALUE
           "301 302 303 304 305 310 311 312 313 314 320 321 322 323 ".
           03  FILLER          PIC X(56)  VALUE
           "324 340 341 350 351 360 361 370 380 381 390 400 410 420 ".
           03  FILLER          PIC X(4)   VALUE
           "421.".

       01  DF2-LIST.
           03  FILLER          PIC X(36)  VALUE
           "140 141 170 171 181 182 200 201 210 ".
           03  FILLER          PIC X(24)  VALUE
           "211 260 261 262 263 264 ".
           03  FILLER          PIC X(22)  VALUE
           "LOANAMT ANTIC 270 271 ".
           03  FILLER          PIC X(56)  VALUE
           "272 273 274 275 280 281 282 283 284 285 290 291 292 293 ".
           03  FILLER          PIC X(56)  VALUE
           "294 295 300 301 302 303 304 305 340 341 350 351 360 361 ".
           03  FILLER          PIC X(40)  VALUE
           "370 380 381 390 400 410 420 421 430 440.".

       01  PROMPT-FG        PIC X      VALUE " ".
       01  PROMPT-BUF.
           03  FILLER       PIC X(13)  VALUE "LOAN AMOUNT :".
           03  FILLER       PIC X(13)  VALUE "ANTIC EARN  :".
           03  FILLER       PIC X(31)  VALUE
               "CO O EX/TRM  COVERAGE PREMIUM X".
           03  FILLER       PIC X(27)  VALUE
               "---OUTPUT------------------".
           03  FILLER       PIC X(6)   VALUE "27 CL:".
           03  FILLER       PIC X(6)   VALUE "28 AH:".
           03  FILLER       PIC X(6)   VALUE "29 PP:".
           03  FILLER       PIC X(6)   VALUE "30 O1:".
           03  FILLER       PIC X(6)   VALUE "31 O2:".
           03  FILLER       PIC X(6)   VALUE "32 O3:".
           03  FILLER       PIC X(15)  VALUE "34 SERVICE CHG:".
           03  FILLER       PIC X(15)  VALUE "35 MAINT FEES :".
           03  FILLER       PIC X(15)  VALUE "36 1ST PY EXT :".
           03  FILLER       PIC X(15)  VALUE "   INT CHARGE :".
           03  FILLER       PIC X(15)  VALUE "   AMT FINANCD:".
           03  FILLER       PIC X(15)  VALUE "   TOT REPYBLE:".
           03  FILLER       PIC X(15)  VALUE "   1ST PAYMENT:".
           03  FILLER       PIC X(15)  VALUE "   LAST   ""  :".
           03  FILLER       PIC X(15)  VALUE "42 REG PAYMENT:".
           03  PROMPT-200   PIC X(2)   VALUE "20".
           03  PROMPT-201   PIC X(13)  VALUE "FEE1        :".
           03  PROMPT-210   PIC X(2)   VALUE "21".
           03  PROMPT-211   PIC X(13)  VALUE "FEE2        :".
           03  FILLER       PIC X(16)  VALUE "23 COLLATERAL ID".
           03  FILLER       PIC X(17)  VALUE "-----RATES-----FG".
           03  FILLER       PIC X(7)   VALUE "26 OVR:".
           03  FILLER       PIC X(7)   VALUE "   EFF:".
           03  FILLER       PIC X(7)   VALUE "   APR:".
           03  FILLER       PIC X(7)   VALUE "       ".
      *    03  FILLER       PIC X(7)   VALUE "   YLD:".

       01  PROMPT-LIST.
           03  FILLER          PIC X(42)  VALUE
             "PLOANAMT:S PANTIC:S INSPROMPT:S OUTPROMPT ".
           03  FILLER          PIC X(49)  VALUE
             "P270:S P280:S P290:S P300:S P310:S P320:S P340:S ".
           03  FILLER          PIC X(21)  VALUE
             "P350:S P360:S P370:S ".
           03  FILLER          PIC X(49)  VALUE
             "P380:S P390:S P400:S P410:S P420:S P200:S P201:S ".
           03  FILLER          PIC X(21)  VALUE
             "P210:S P211:S P230:S ".
           03  FILLER          PIC X(30)  VALUE
             "PRATES:S P260:S P262:S P263:S ".
           03  FILLER          PIC X(7)   VALUE
             "P264:S.".

      ********************************
      *   E L E M E N T   S P E C S  *
      ********************************
       01  MAX             PIC 99   VALUE 42.
       01  SPEC-TABLE.
      * 1  PD-SSNO(1)
           03  FILLER      PIC X(8) VALUE "E  A110 ".
      * 2  PD-SSNO(2)
           03  FILLER      PIC X(8) VALUE "E  A110 ".
      * 3  PD-SSNO(3)
           03  FILLER      PIC X(8) VALUE "E  A110 ".
      * 4  PD-SSNO(4)
           03  FILLER      PIC X(8) VALUE "E  A110 ".
      * 5  PD-ORGST
           03  FILLER      PIC X(8) VALUE "E  A020 ".
      * 6  PD-LOANDATE
           03  FILLER      PIC X(8) VALUE "ERNM060 ".
      * 7  PD-PURCD
           03  FILLER      PIC X(8) VALUE "E  A030 ".
      * 8  PD-COLLCD
           03  FILLER      PIC X(8) VALUE "E  A030 ".
      * 9  PD-SRCD
           03  FILLER      PIC X(8) VALUE "E  A030 ".
      * 10 PD-UNITPER-CD
           03  FILLER      PIC X(8) VALUE "E  A010N".
      * 11 PD-PRLNNO
           03  FILLER      PIC X(8) VALUE "ENNN060 ".
      * 12 PD-ORGTERM
           03  FILLER      PIC X(8) VALUE "ENNN030 ".
      * 13 PD-1STPYDATE
           03  FILLER      PIC X(8) VALUE "ERNM060 ".
      * 14 PD-PROCEEDS
           03  FILLER      PIC X(8) VALUE "ENNS062 ".
      * 15 OPEN
           03  FILLER      PIC X(8) VALUE "ENNS032N".
      * 16 OPEN
           03  FILLER      PIC X(8) VALUE "ENNS032N".
      * 17 PD-LASTPYAMT
           03  FILLER      PIC X(8) VALUE "ENNS062 ".
      * 18 PD-INSURED
           03  FILLER      PIC X(8) VALUE "E  A030 ".
      * 19 PD-FEEAMTS(X)
           03  FILLER      PIC X(8) VALUE "E  A020 ".
      * 20 PD-FEEAMT(1)?
           03  FILLER      PIC X(8) VALUE "ENNN042 ".
      * 21 PD-FEEAMT(2)?
           03  FILLER      PIC X(8) VALUE "ENNN042 ".
      * 22 PD-DLNO
           03  FILLER      PIC X(8) VALUE "ENNN042 ".
      * 23 COLLATERAL ID
           03  FILLER      PIC X(8) VALUE "E  A200 ".
      * 24 OPEN.........
           03  FILLER      PIC X(8) VALUE "ENNN052N".
      * 25 OPEN.......
           03  FILLER      PIC X(8) VALUE "ENNN052N".
      * 26 PD-OVRDRATE
           03  FILLER      PIC X(8) VALUE "ENNN033 ".
      * 27 PD-INSCOMP(1)
           03  FILLER      PIC X(8) VALUE "ENNN020 ".
      * 28 PD-INSCOMP(2)
           03  FILLER      PIC X(8) VALUE "ENNN020 ".
      * 29 PD-INSCOMP(3)
           03  FILLER      PIC X(8) VALUE "ENNN020 ".
      * 30 PD-INSCOMP(4)
           03  FILLER      PIC X(8) VALUE "ENNN020 ".
      * 31 PD-INSCOMP(5)
           03  FILLER      PIC X(8) VALUE "ENNN020 ".
      * 32 PD-INSCOMP(6)
           03  FILLER      PIC X(8) VALUE "ENNN020 ".
      * 33 OPEN
           03  FILLER      PIC X(8) VALUE "E  A020N".
      * 34 PD-SERCHG
           03  FILLER      PIC X(8) VALUE "ENNN062 ".
      * 35 PD-MAINTFEE
           03  FILLER      PIC X(8) VALUE "ENNN062 ".
      * 36 PD-EXTCHG
           03  FILLER      PIC X(8) VALUE "ENNN062 ".
      * 37 PD-INTCHG
           03  FILLER      PIC X(8) VALUE "ENNN062N".
      * 38 PD-FINAMT
           03  FILLER      PIC X(8) VALUE "ENNN062N".
      * 39 PD-LOANAMT
           03  FILLER      PIC X(8) VALUE "ENNN062N".
      * 40 PD-1STPYAMT
           03  FILLER      PIC X(8) VALUE "ENNN062N".
      * 41 PD-LASTPYAMT
           03  FILLER      PIC X(8) VALUE "ENNN062N".
      * 42 PD-REGPYAMT
           03  FILLER      PIC X(8) VALUE "ENNN062 ".
       01  SPEC-TABLE2     REDEFINES SPEC-TABLE.
           03  SPEC-REC    OCCURS 42 TIMES.
               05  SPEC    PIC X(7).
               05  CHFG    PIC X.

       01  BLQUOT-LINES          PIC 99  VALUE 26.
       01  BLQUOT-COLUMNS        PIC 99  VALUE 82.
       01  BLQUOT-BEGIN-Y        PIC 99  VALUE 1.
       01  BLQUOT-BEGIN-X        PIC 99  VALUE 1.
       01  ERRCD             PIC X.
       01  LOAN-QUOTE-FG     PIC X.

       01  TRANS-DATE-YYYYMMDD       PIC 9(8).

       01  DATE-STAT       PIC X.
       01  PRIOR-LNAMT     PIC S9(7)V99 COMP.
       01  COUNTER         PIC 99.
       01  PROCEEDS-FG-HLDR     PIC X.
       01  PROCEEDS-HLDR        PIC S9(9)V99 COMP.
       01  SELLPR-HLDR          PIC S9(9)V99 COMP.
       01  AMT-HOLDER           PIC S9(9)V99 COMP.
       01  MAINTFEE-HLDR        PIC S9(6)V99 COMP.
       01  TOTAL-GL-HLDR        PIC S9(5)V99 COMP.
       01  TOTAL-GL-FINCHG-HLDR PIC S9(5)V99 COMP.
       01  INSPREM-HLDR         PIC S9(4)V99 COMP.
       01  INSTERM-HLDR         PIC 9(3)     COMP.
       01  INSOURS-HLDR         PIC X.
       01  CALC-FG-HLDR         PIC X.
       01  INSCODES-HLDR        PIC XXX.

       01  HOLD-WORKER          PIC S9(7)V99.
       01  HOLD-DATE-WORKER     PIC 9(8).

       01  HOLD-AMT-EXCEEDED    PIC Z(9).99.
       01  HOLD-TERM-EXCEEDED   REDEFINES HOLD-AMT-EXCEEDED
                                PIC Z(11)9.
       01  HOLD-RATE-EXCEEDED   REDEFINES HOLD-AMT-EXCEEDED
                                PIC Z(4).9999.
       01  AMT-EXCEEDED-LIT     REDEFINES HOLD-AMT-EXCEEDED
                                PIC X(12).

       01  TYPE-OF-AMT-EXCEEDED PIC X(15).
       01  HOLD-DATE-YYYYMMDD   PIC 9(8).
       01  SAVE-QUOTE-SSN       PIC 9(10)  COMP.
       01  SCREEN-LINES.
           03  SCREEN-LINE1     PIC X(20).
           03  SCREEN-LINE2     PIC X(20).
           03  SCREEN-LINE3     PIC X(20).
       01  MSG-LINES REDEFINES SCREEN-LINES.
           03  MSG-LINE1        PIC X(30).
           03  MSG-LINE2        PIC X(30).
       01  SAVE-PD2-KEY.
           03  SAVE-PD2-KEY-ID  PIC X.
           03  SAVE-PD2-KEY-BR  PIC 9(4).
           03  SAVE-PD2-KEY-NAM PIC X(33).
       01  TERM-LOW             PIC 9(3)    COMP.
       01  TERM-HIGH            PIC 9(3)    COMP.
       01  PROCEEDS-LOW         PIC 9(7)V99 COMP.
       01  PROCEEDS-HIGH        PIC 9(7)V99 COMP.
       01  POINTS-LOW           PIC 9(7)V99 COMP.
       01  POINTS-HIGH          PIC 9(7)V99 COMP.

       01  GA-SCHG-PATH.
           03  FILLER         PIC X(7)     VALUE "CL/GASC".
           03  GA-SCHG-NO     PIC 99.
       01  SC-SCHG-PATH.
           03  FILLER         PIC X(7)     VALUE "CL/SCSC".
           03  SC-SCHG-NO     PIC 99.

       01  LONPC2-LIST           PIC X(7) VALUE "PC2BUF.".
       01  LONPC2-BUF            VALUE " ".
           03  PRLOAN-RESTRICTED PIC X.

       COPY "LIBCL/SFDLO3W.CPY".
       COPY "LIBCL/CASCHGW.CPY".
       COPY "LIBCL/NCSCHGW.CPY".
       COPY "LIBCL/GASCHGW.CPY".
       COPY "LIBCL/SCSCHGW.CPY".
       COPY "LIBCL/TXSCHGW.CPY".
       COPY "LIBCL/WISCHGW.CPY".
       COPY "LIBCL/MFEE04W.CPY".
       COPY "LIBLP/PAYOFXW.CPY".
       COPY "LIBLP/LPWKPATH.CPY".
       COPY "LIBGB/MKTEMPW_CMD.CPY".
       COPY "LIBGB/SYSTEMW.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/HASHW.CPY".
       COPY "LIBGB/GPENVW.CPY".
       COPY "LIBWI/PFSWINDOWSW.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBLP/LPUPERW.CPY".
       COPY "LIBLP/LPUPWKW.CPY".
       COPY "LIBLP/UPERTSTW.CPY".
       COPY "LIBLP/LPANTIW.CPY".
       COPY "LIBLP/LPMENUW.CPY".
       COPY "LIBLP/LPWKCIP.CPY".
       COPY "LIBLP/LPWKCEP.CPY".
       COPY "LIBLP/LPWKCOP.CPY".
       COPY "LIBLP/LPAMTSW.CPY".
       COPY "LIBLP/LPSPEDW.CPY".
      *COPY "LIBCL/FINDLNW.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBLP/LPBAGEW.CPY".
       COPY "LIBLP/RESCINDW.CPY".
       COPY "LIBWI/FEEENTW.CPY".
       COPY "LIBWI/SCHG33W.CPY".
       COPY "LIBWI/DLSCANW.CPY".
       COPY "LIBWI/BWLNSCW.CPY".
       COPY "LIBWI/LCSCANW.CPY".
       COPY "LIBWI/PDSCANW.CPY".
       COPY "LIBWI/SCSCANW.CPY".
       COPY "LIBWI/BLINPCW.CPY".
       COPY "LIBWI/CLSCANW.CPY".
       COPY "LIBWI/CDSCANW.CPY".
       01  WINDOW-WORKERS.
           03  WINDOW-LPO          PIC X(4).
           03  WINDOW-REP          PIC X(8).
           03  WINDOW-PROCESS      PIC X(1).

       COPY "LIBWI/CDBSCNW.CPY".
       COPY "LIBLP/LPAPRZW.CPY".
       COPY "LIBWI/W12EQ2W.CPY".
       COPY "LIBLP/LPICOMMW.CPY".
       COPY "LIBLP/INSELIGW.CPY".
       COPY "LIBGB/PASSWDW.CPY".
       78  CAN-SEQ              VALUE 00.
       78  1STPY30-SEQ          VALUE 01.
       78  APPROVAL-SEQ         VALUE 02.
       78  APR-SEQ              VALUE 03.
       78  OVER-SEQ             VALUE 04.
       78  C-OR-E-SEQ                 VALUE  8.
       78  CL-AH-PP-O1-SEQ      VALUE 12.
      *  PASSWORD SPECIFIC CLASS RENEWALS FOR REGENCY
       78  RENEWAL-SEQ          VALUE 17.
       78  SRCD-SEQ             VALUE 20.
       78  PP-PREM-SEQ          VALUE 21.
       78  O2-SEQ               VALUE 27.

       01  BLQUOT-WINDOW-HANDLE  PIC X(10).
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBWI/BLQUOT_DEF.CPY".
       COPY "LIBWI/FEEENT_DEF.CPY".
       COPY "LIBWI/BLQUOT_WKS.CPY".
       COPY "LIBWI/FEEENT_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".

       COPY "LIBGB/GETFMW.CPY".

       SCREEN SECTION.
       COPY "LIBWI/BLQUOT_SCN.CPY".
       COPY "LIBWI/FEEENT_SCN.CPY".

       PROCEDURE DIVISION
            USING FORM-PATHNAME EXIT-PATHNAME.
       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
                WK-FILE.
       COPY "LIBGB/DECLARE.CPY".
           PERFORM CLOSE-SP1-FILE.
           PERFORM CLOSE-PD2-FILE.
           PERFORM CLOSE-PD1-FILE.
           PERFORM CLOSE-LC1-FILE.
           PERFORM CLOSE-IC1-FILE.
           PERFORM CLOSE-DL1-FILE.
           PERFORM CLOSE-CL1-FILE.
           PERFORM CLOSE-CDB1-FILE.
           PERFORM CLOSE-CD1-FILE.
           PERFORM CLOSE-BW1-FILE.
           PERFORM CLOSE-GI1-FILE.
           PERFORM CLOSE-GB-FILE.
           PERFORM CLOSE-BR-FILE.
           CLOSE
               WK-FILE.
       COPY "LIBGB/DECLAR2.CPY".

      ***********************************
      *   C O N T R O L   M O D U L E   *
      ***********************************
       MAIN-PROGRAM SECTION.

           PERFORM SQL-CONNECT.

      D    STOP MESS.

           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "BLQUOT" TO VDU-FORM-NAME.

           MOVE " " TO VDU-WIMENU-SELECTION.

           DISPLAY FLOATING GRAPHICAL WINDOW
                      AT  SCREEN LINE BLQUOT-BEGIN-X * VDU-WIN-L-OFFSET
                      AT  SCREEN COL  BLQUOT-BEGIN-Y * VDU-WIN-C-OFFSET
                      CELL SIZE IS LABEL FONT
           COLOR IS VDU-WCOLOR-TCLP,CONTROL VALUE IS VDU-WCTRL-TCLP
                      SIZE    BLQUOT-COLUMNS + VDU-WIN-W-MOD
                      LINES   BLQUOT-LINES + VDU-WIN-H-MOD
                      BLANK SCREEN
                      HANDLE IS BLQUOT-WINDOW-HANDLE.

           MOVE EXT-CONAME-SYSDATE TO ALP-DATE.
           PERFORM CONVERT-ALPDATE-TO-YYYYMMDD.
           MOVE DATE-YYYYMMDD TO TRANS-DATE-YYYYMMDD.

           MOVE " " TO ERRCD.
           MOVE "Q" TO LOAN-QUOTE-FG.

           MOVE EXT-CONAME-USER-LOC TO BR-NO.
           PERFORM OPEN-BR-FILE.
           PERFORM READ-BR-FILE.
           PERFORM CLOSE-BR-FILE.

           PERFORM GET-GPENV.

           PERFORM OPEN-IC1-FILE.
           PERFORM OPEN-CD1-FILE.
           PERFORM OPEN-SP1-FILE.
           PERFORM OPEN-BW1-FILE.

       MAIN-MODULE.
           MOVE " " TO CAN-LOAN-NO.

           PERFORM FORM-RESET.

      * CLEAR REQUIRED RE: (GA) SP-SCFRMLA = 12 OR 13 OR 54
      *                    (SC) SP-SCFRMLA = 18
           MOVE 0 TO SPDUAL-LIAB.
           MOVE " " TO HOLD-BWNAME.

           IF FIRST-TIME = 0
              MOVE 1 TO FIRST-TIME
           ELSE
              MOVE " " TO MESS PROMPT-FG
              PERFORM CALL-TMMSG.

           PERFORM ENTER-KEYS.
           IF IO-TYPE = "CLR"
              GO TO MAIN-MODULE.
           IF IO-TYPE = "END"
              GO TO END-ROUTINE.
       ENTER-LOAN.
           PERFORM ENTRY-MODULE.
           IF IO-TYPE = "END"
              GO TO END-ROUTINE.
           GO TO MAIN-MODULE.

      **********************
       ENTER-KEYS SECTION.
      **********************
           MOVE 0 TO NEW-FLAG ELE.
           MOVE " " TO IO-TYPE.
           PERFORM NEW-RECORD-SETUP.

           IF BR-CAN-LOANS-FG = "Y" OR "O"
              GO TO RECALL-LOAN.
       ENTER-LC-CODE.
           PERFORM LOAN-CLASS-PROMPT.

           MOVE " " TO CAN-LOAN-NO.
           MOVE " " TO PD2-BWNAME.
           MOVE "F7, F1-RECALL, F6-CLASS SCAN, ENTER LOAN CLASS:"
                                                        TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "EN N020LNKEY." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10"
              PERFORM ALARM
              GO TO RECALL-LOAN.
           IF VDUSTATUS = "1<"
              PERFORM CLASS-SCAN
              IF IO-FG NOT = 0
                 GO TO ENTER-LC-CODE.
           IF VDUSTATUS = "1>"
              GO TO END-ROUTINE.
           IF VDUSTATUS NOT = "00"
              GO TO ENTER-LC-CODE.
           IF VDUNUM0 = 0 OR > 99
              GO TO ENTER-LC-CODE.

           MOVE VDUNUM0 TO LC-CODE.
           PERFORM GET-LC.
           IF IO-FG = 9
              PERFORM NOT-FOUND
              GO TO ENTER-LC-CODE.

           MOVE "S  A000LCDESC." TO VDU.
           MOVE LC-DESC TO VDUBUF.
           PERFORM SCREENIO.

           IF ( LC-SALEFIN = "Y"   )
              PERFORM NOT-DIRECT
              GO TO ENTER-LC-CODE.

           IF LC-GIFTNET = "Y"
              MOVE "CAN'T BE A WCBC CLASS" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-LC-CODE.

           MOVE EXT-CONAME-USER-LOC TO GI-BRANCH.
           MOVE LC-CODE  TO GI-CLASS.
           PERFORM OPEN-GI1-FILE.
           PERFORM READ-GI1-FILE.
           PERFORM CLOSE-GI1-FILE.
           IF IO-FG = 9
              MOVE "G/L INTERFACE RECORD NOT FOUND" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-LC-CODE.

           MOVE LC-CODE TO PD-CLASS
                           PD-SPRCLASS.
           MOVE LC-SMALLOAN TO PD-SMALLOAN.
           MOVE LC-SALEFIN TO PD-SALEFIN.
           MOVE LC-SERVACCT TO PD-SERVACCT.
           MOVE LC-LOANTYPE TO PD-LOANTYPE.
           MOVE LC-GIFTNET TO PD-GIFTNET.
           MOVE 1 TO PD-STMT-COUNTER.
           GO TO EXIT-ENTER-KEYS.

      **********************
      *   RECALL ENTRY
      **********************
       RECALL-LOAN.
       RECALL-GET-KEY.
           IF BR-CAN-LOANS-FG = "O"
              MOVE LEGEND-01-CAN-ONLY TO LEGEND
           ELSE
           IF BR-CAN-LOANS-FG = "N"
              MOVE LEGEND-01-NO-CAN TO LEGEND
           ELSE
              MOVE LEGEND-03 TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "S  A110KEYTYPE:S." TO VDU.
           IF BR-CAN-LOANS-FG = "N"
              MOVE "          :" TO VDUBUF
           ELSE
              MOVE "CAN LOAN #:" TO VDUBUF.
           PERFORM SCREENIO.

           PERFORM NEW-RECORD-SETUP.
           MOVE " " TO CAN-LOAN-NO.
           MOVE " " TO PD2-BWNAME.
           MOVE "E  A020LNKEY." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10"
              IF BR-CAN-LOANS-FG = "Y"
                 IF ALLOW-NON-CANS NOT = "Y"
                    MOVE CAN-SEQ TO PASSWDW-SEQ
                    MOVE "TO ALLOW 'NON' CAN LOANS.....:"
                                            TO PASSWDW-PROMPT2
                    PERFORM VERIFY-PASSWORD
                    IF PASSWDW-STAT NOT = "OK"
                       GO TO RECALL-GET-KEY
                    ELSE
                       MOVE "Y" TO ALLOW-NON-CANS.
           IF VDUSTATUS = "10"
              IF BR-CAN-LOANS-FG NOT = "O"
                 PERFORM ALARM
                 MOVE "ENT" TO IO-TYPE
                 GO TO ENTER-LC-CODE.
           IF VDUSTATUS = "18"
              MOVE "Q" TO PDSCAN-ID
              PERFORM QUOTE-SCAN
              IF IO-FG NOT = 0
                 GO TO RECALL-GET-KEY
              ELSE
                 GO TO VIA-QUOTE-SCAN.
           IF VDUSTATUS = "12"
              IF BR-CAN-LOANS-FG NOT = "N"
                 PERFORM CAN-LOAN-SCAN
                 IF IO-FG NOT = 0
                    GO TO RECALL-GET-KEY
                 ELSE
                    MOVE CLSCAN-CAN-NO TO CAN-LOAN-NO
                    GO TO VIA-CAN-LOAN-SCAN.
           IF VDUSTATUS = "1>"
              GO TO END-ROUTINE.
           IF ( VDUSTATUS NOT = "00"  ) OR
              ( BR-CAN-LOANS-FG = "N" )
              GO TO RECALL-LOAN.
           MOVE VDUBUF TO CAN-LOAN-NO.
           GO TO VIA-CAN-LOAN-SCAN.

      **********************************
      *    VIA QUOTE SCAN
      **********************************
       VIA-QUOTE-SCAN.
           PERFORM NEW-RECORD-SETUP.
           MOVE " " TO PRIOR-LOAN-STAT PASS-STAT-FG.
           MOVE "Q" TO QUOTE-ID-FG.

           PERFORM OPEN-PD1-FILE.

           MOVE "Q"                 TO PD2-ID SAVE-PD2-KEY-ID.
           MOVE EXT-CONAME-USER-LOC TO PD2-BRANCH SAVE-PD2-KEY-BR.

      * IF YOU MOVE THE NAME IN, YOU WILL NOT EVER GET THE LAST RECORD FOR
      * SOME REASON, ON AZURE. IF "TOMMY TESTER" IS ONLY QUOTE, YOU CAN
      * SELECT IT IN SCAN WINDOW, BUT THEN GET RECORD NOT FOUND. NOW ADD
      * ANOTHER QUOTE "BETH", YOU CAN PULL UP BETH, BUT NOT TOMMY. YOU GET
      * IO-FG = 9. IF WE MOVE SPACES AND START, YOU WILL GET IT! BIZARRE
      * PROBLEM. SO I ADDED CHECKING THAT THE NAME MATCHES WHAT YOU SELECT.

      *    MOVE PDSCAN-BWNAME       TO PD2-BWNAME
      *                                HOLD-BWNAME.

           MOVE PDSCAN-BWNAME       TO HOLD-BWNAME.
           MOVE SPACES              TO PD2-BWNAME.

           PERFORM START-PD2-FILE.
       VIA-QUOTE-SCAN-NEXT.
           PERFORM READ-PD2-FILE-NEXT.
           IF (IO-FG = 9) OR (PD2-ID NOT = "Q") OR
                             (PD2-BRANCH NOT = EXT-CONAME-USER-LOC)
              PERFORM NOT-FOUND
              MOVE "CLR" TO IO-TYPE
              GO TO EXIT-VIA-QUOTE-SCAN.

           IF PD2-BWNAME NOT = PDSCAN-BWNAME
              GO TO VIA-QUOTE-SCAN-NEXT.

           IF PD-PRLNNO NOT = 0
              PERFORM CLOSE-PD1-FILE.

           MOVE PD-LOANDATE TO HOLD-DATE-YYYYMMDD.

           PERFORM GET-SP.
           PERFORM PROTECT-FIELDS.
           PERFORM DISPLAY-FIELDS.

      *  CITIZENS TEST
           MOVE CIP-CALCULATION-INPUT-AREA
                             TO COP-CALCULATION-OUTPUT-AREA.

      *    IF PD-PRLNNO NOT = 0
      *       MOVE PD-SSNO(1) TO SAVE-SSNO
      *       PERFORM SET-SCRATCH-KEY
      *       MOVE "Q" TO PD-ID
      *       PERFORM OPEN-PD1-FILE
      *       PERFORM READ-PD1-FILE
      *       IF IO-FG NOT = 9
      *          PERFORM START-PD2-FILE-GREATER.

           MOVE "S  A330011." TO VDU.
           MOVE PD2-BWNAME TO VDUBUF SAVE-BW-NAME.
           PERFORM SCREENIO.

           MOVE "S  A010012." TO VDU.
           MOVE PD-MAKERCD(1) TO VDUBUF.
           PERFORM SCREENIO.

           MOVE PD-SOC-SEC TO SAVE-QUOTE-SSN.
           PERFORM LOAN-CLASS-PROMPT.
           IF ( LC-SALEFIN = "Y"   )
              PERFORM NOT-DIRECT
              MOVE "CLR" TO IO-TYPE
              GO TO EXIT-VIA-QUOTE-SCAN.

           IF PRIOR-LOAN-NOT-VALID
              PERFORM SEND-PRIOR-MESS.

           IF ERROR-IN-PRIOR-LOAN
              PERFORM PRIOR-LOAN-ERROR-MSG.

           MOVE 2 TO NEW-FLAG.
           MOVE "SN N020LNKEY." TO VDU.
           MOVE PD-CLASS TO VDUNUM0.
           PERFORM SCREENIO.
           IF HOLD-DATE-YYYYMMDD NOT = TRANS-DATE-YYYYMMDD
                     OR SP-LAWCODE NOT = PD-LAWCODE
                      OR NOT PD-ONE-MTH-NO-DAY-EXT
                       OR PD-PRLNNO NOT = 0
              PERFORM TEST-FOR-GEORGIA-SCHG
              PERFORM TEST-FOR-SC-SCHG
              PERFORM TEST-FOR-WI-SCHG
              PERFORM TEST-FOR-MFEE
      * CITIZENS TEST
              MOVE 800 TO CIP-NO-OF-TIMES-CALLED
              PERFORM CALL-LOAN-CALCULATIONS.
           IF ERROR-IN-PRIOR-LOAN
              PERFORM PRIOR-LOAN-ERROR-MSG.

           GO TO EXIT-VIA-QUOTE-SCAN.

      **********************************
      *    VIA CAN-LOAN SCAN
      **********************************
       VIA-CAN-LOAN-SCAN.
           PERFORM OPEN-CL1-FILE.
           PERFORM SET-CLFILE-INDEX-BY.
           MOVE CAN-LOAN-NO TO CL-CAN-NO.
           PERFORM READ-CL1-FILE.
           PERFORM CLOSE-CL1-FILE.
           IF IO-FG = 9
              PERFORM NOT-FOUND
              GO TO RECALL-LOAN.

           PERFORM NEW-RECORD-SETUP.
           MOVE CL-PD-REC TO PD-REC.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    CHECK 'CL' FOR ANY 'SPR' CHANGES
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           PERFORM GET-SP.

           IF ( IO-BAD )
              MOVE "STATE PARA. DELETED!" TO MESS
              PERFORM SEND-MESS
              GO TO RECALL-LOAN.

           IF ( SP-CAN-LOANS-NO )
              MOVE MESSAGE-INVALID-SPR-CAN-LOAN TO MESS
              PERFORM SEND-MESS
              GO TO RECALL-LOAN.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    VERIFY THAT ALL THE FEECODES WITHIN THE CAN LOAN
      *    EXIST FOR THE POSTING BRANCH
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE 0 TO IO-FG.
           MOVE EXT-CONAME-USER-LOC TO PD-OWNBR.
           PERFORM VARYING SUB FROM 1 BY 1
                   UNTIL (SUB > 10 OR IO-FG = 9)
              IF PD-FEECODE(SUB) NOT = " "
                 MOVE PD-FEECODE(SUB) TO CDB-CODE
                 PERFORM GET-CODE
              END-IF
           END-PERFORM.
           IF IO-FG = 9
              PERFORM MISSING-FEECODE
              GO TO RECALL-LOAN.
      *----------------------------------------------------

           MOVE EXT-CONAME-USER-LOC TO PD2-BRANCH PD-BRANCH
                            PD-POSTING-BR PD-OWNBR
                            PD-ORGBR.

           MOVE "Q"                 TO PD2-ID SAVE-PD2-KEY-ID.
           MOVE EXT-CONAME-USER-LOC TO PD2-BRANCH SAVE-PD2-KEY-BR.
           MOVE SPACES              TO PD2-BWNAME.

           MOVE " " TO PRIOR-LOAN-STAT
                       PASS-STAT-FG.

           MOVE PD-PROCEEDS TO PD-CAN-LOAN-MAX.
           MOVE PD-LOANDATE TO HOLD-DATE-YYYYMMDD.

           PERFORM DISPLAY-FIELDS.

           MOVE "S  A330011." TO VDU.
           MOVE CL-DESC TO VDUBUF.
           PERFORM SCREENIO.

           PERFORM LOAN-CLASS-PROMPT.
           IF ( LC-SALEFIN = "Y"   )
              PERFORM NOT-DIRECT
              MOVE "CLR" TO IO-TYPE
              GO TO EXIT-ENTER-KEYS.

           IF PRIOR-LOAN-NOT-VALID
              PERFORM SEND-PRIOR-MESS.

           IF ERROR-IN-PRIOR-LOAN
              PERFORM PRIOR-LOAN-ERROR-MSG.

           MOVE 2 TO NEW-FLAG.
           MOVE "SN N020LNKEY." TO VDU.
           MOVE PD-CLASS TO VDUNUM0.
           PERFORM SCREENIO.

           PERFORM GET-SP.

           IF CL-GIFTNET = "Y"
              MOVE "CAN NOT BE A WCBC CAN LOAN" TO MESS
              PERFORM SEND-MESS
              MOVE "CLR" TO IO-TYPE
              GO TO EXIT-ENTER-KEYS.

           IF HOLD-DATE-YYYYMMDD NOT = TRANS-DATE-YYYYMMDD
                     OR SP-LAWCODE NOT = PD-LAWCODE
                      OR NOT PD-ONE-MTH-NO-DAY-EXT
              PERFORM TEST-FOR-GEORGIA-SCHG
              PERFORM TEST-FOR-SC-SCHG
              PERFORM TEST-FOR-WI-SCHG
              PERFORM TEST-FOR-MFEE
      *  CITIZENS TEST
              MOVE 800 TO CIP-NO-OF-TIMES-CALLED
              PERFORM CALL-LOAN-CALCULATIONS.
           IF ERROR-IN-PRIOR-LOAN
              PERFORM PRIOR-LOAN-ERROR-MSG.

           IF (GP-ENTER-LPO-FG = "Y")
              MOVE PD-LPO TO WINDOW-LPO
              MOVE PD-REP-CD TO WINDOW-REP
              MOVE PD-PROCESS-CD TO WINDOW-PROCESS
              MOVE "WI/TFCCDS" TO FORM-NAM
              CALL FORM-PROGX USING FORM-PATH WINDOW-WORKERS
              CANCEL FORM-PROGX
              MOVE WINDOW-LPO TO PD-LPO
              MOVE WINDOW-REP TO PD-REP-CD
              MOVE WINDOW-PROCESS TO PD-PROCESS-CD.

       EXIT-VIA-QUOTE-SCAN.
           PERFORM CLOSE-PD1-FILE.
           GO TO EXIT-ENTER-KEYS.

       EXIT-ENTER-KEYS.
           EXIT.

      *************************************************
      *   ENTRY-MODULE
      *   NEW-FLAG = 0 - MODIFY EXISTING RECORD
      *              1 - INITIAL NEW RECORD ENTRY
      *              2 - MODIFY NEW RECORD
      ************************************************
       ENTRY-MODULE SECTION.

           MOVE "N" TO CHFG(2) CHFG(3) CHFG(4).

           MOVE "S  A000MSEL." TO VDU.
           PERFORM SCREENIO.
           MOVE " " TO IO-TYPE.
           MOVE 1 TO ONE.
           MOVE FIRST-ELE TO ELE.
           MOVE 0 TO FIRST-ELE.
           IF CAN-LOAN-NO NOT = " "
              PERFORM FORCE-CAN-LOAN-NUMBERS.
           IF CAN-LOAN-NO NOT = " "
              MOVE "Y" TO INITIAL-CAN-ENTRY-FG
           ELSE
              MOVE " " TO INITIAL-CAN-ENTRY-FG.

       CREATE-ROUTINE.
           IF NEW-FLAG NOT = 1
              GO TO MODIFY-ROUTINE.

           ADD ONE TO ELE.
           IF ELE < 1
              MOVE 1 TO ELE ONE.
           IF NOT (NEW-FLAG = 1 AND ELE > 19)
              IF ELE NOT > MAX
                 GO TO ENTER-ELE.
           MOVE 2 TO NEW-FLAG.
           PERFORM SETUP-CALC-AREA.
           PERFORM TEST-FOR-GEORGIA-SCHG.
           PERFORM TEST-FOR-SC-SCHG.
           PERFORM TEST-FOR-WI-SCHG.
           PERFORM TEST-FOR-MFEE.
           PERFORM CALL-LOAN-CALCULATIONS.
       MODIFY-ROUTINE.
           IF INITIAL-CAN-ENTRY-FG = "Y"
              MOVE " " TO INITIAL-CAN-ENTRY-FG
              MOVE ZEROS TO CHKEY
              MOVE 1 TO ELE
              GO TO CAN-INITIAL-START.
           MOVE " " TO STAT.
           PERFORM UNPROTECT-FIELDS.
           IF SP-PYEXFRMLA-SAME2-SPREAD
              MOVE
             "F1-CLEAR, F4-MEMO, F5-SPREAD, FIELD #, DELETE, S-SAVE:"
                                                           TO LEGEND
           ELSE
              MOVE "F1-CLEAR, F4-MEMO, FIELD #, DELETE, S-SAVE:"
                                                           TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "R RA060SEL." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.
           IF VDUSTATUS = "18"
              PERFORM MEMO-ENTRY
              GO TO MODIFY-ROUTINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    F5 - 1STPYEXT FORMULA 12 SPREAD WINDOW
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( VDUSTATUS = "1:"          ) AND
              ( PD-GIFTNET NOT = "Y"      ) AND
              ( SP-PYEXFRMLA-SAME2-SPREAD )
                PERFORM W12EQ2-WINDOW
                GO TO MODIFY-ROUTINE.

           IF VDUSTATUS NOT = "00"
              GO TO MODIFY-ROUTINE.
           MOVE VDUBUF TO ACTION.
           IF ACTION = "DELETE"
              PERFORM DELETE-SCRATCH
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.

           IF NOT ( ACTION = "     S")
              GO TO BYPASS-UPDATE.

           IF PRIOR-LOAN-NOT-VALID
              PERFORM SEND-PRIOR-MESS
              GO TO MODIFY-ROUTINE.

           IF ACTION = "     S"
              PERFORM VERIFY-REQD-FLDS
              IF IO-FG = 9
                 MOVE 0 TO IO-FG
                 GO TO MODIFY-ROUTINE
              END-IF
              PERFORM WRITE-SCRATCH
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.

       BYPASS-UPDATE.
           INSPECT ACTION REPLACING ALL " " BY "0".
           IF CHNO NOT NUMERIC
              GO TO MODIFY-ROUTINE.
           IF CHNO = 0
              GO TO MODIFY-ROUTINE.
           IF CHNO > MAX
              GO TO MODIFY-ROUTINE.
           MOVE CHNO TO ELE.

      *<1225J>
           IF ELE = 26
              IF SP-CAL-FLAT-MONTHLY(1)
                 MOVE "FLATMONTHLY_OVR" TO GETENV-BUF
                 PERFORM GETENV-CALL
                 IF STAT NOT = "00"
                    MOVE
                    "RATE OVERRIDES NOT ALLOWED WITH FLAT MONTHLY"
                                                           TO MESS
                 PERFORM SEND-MESS
                 GO TO MODIFY-ROUTINE.
      *<1225J>

       CAN-INITIAL-START.
      *    IF ELE NOT = 18, LNQUOT
      *       MOVE "Y" TO DEFAULT-APR-FG.

           IF CAN-LOAN-NO NOT = " "
              IF NOT (ELE = 1 OR 2 OR 3 OR 4 OR 7 OR 8
                         OR 9 OR 10 OR 18 OR 19 OR 20
                         OR 21 OR 18 OR 23 OR 31 OR 32)
                 GO TO MODIFY-ROUTINE
              ELSE
                 NEXT SENTENCE
           ELSE
      *    ----------------------------------------------------------
      *    PASSWORD ON CL,AH,PP,01,MAINT FEE, 1ST PY EXT, EXT LC INFO
      *    ----------------------------------------------------------
           IF ELE = 27 OR 28 OR 29 OR 30 OR 35 OR 36 OR 43
              IF OVERRIDES-OK = "N"
                 MOVE OVER-SEQ TO PASSWDW-SEQ
                 MOVE "TO ALLOW OVERRIDES........:"
                                      TO PASSWDW-PROMPT2
                 PERFORM VERIFY-PASSWORD
                 IF PASSWDW-STAT NOT = "OK"
                    MOVE "N" TO OVERRIDES-OK
                    GO TO MODIFY-ROUTINE
                 ELSE
                    MOVE "Y" TO OVERRIDES-OK.

      *    ----------------------------------------------------------------
      *    PASSWORD ON CL, AH, PP, 01         WORLD #J051
      *
      *    N O T E:  NO  PASSWORD TEST
      *              IF CHANGING 'PP' GP-FORCE-INPUT-PP-COVERAGE = 'Y'
      *    ----------------------------------------------------------------
      *    IF NOT ( ELE = 29 AND
      *             PD-ORGST = "GA" AND
      *             GP-FORCE-INPUT-PP-COVERAGE = "Y"
      *           )

      * WORLD PR#1292 OPERATIONS WANTS ALL BRANCHES TO HAVE THE ABILITY
      *               TO ADJUST PP INSURANCE WHEN BUILDING A LOAN
           IF NOT ( ELE = 29 AND GP-FORCE-INPUT-PP-COVERAGE = "Y" )
              IF ELE = 27 OR 28 OR 29 OR 30
                 MOVE CL-AH-PP-O1-SEQ TO PASSWDW-SEQ
                 MOVE "TO ALLOW INS. OVERRIDES ..:" TO PASSWDW-PROMPT2
                 PERFORM VERIFY-PASSWORD
                 IF PASSWDW-STAT NOT = "OK"
                    GO TO MODIFY-ROUTINE.

      * PASSWORD OTHER 2 O2 INSURANCE
           IF ELE = 31
              IF CHFG(ELE) NOT = "N"
                 MOVE O2-SEQ TO PASSWDW-SEQ
                 MOVE "TO CHANGE OTHER 2 INSURANCE:" TO PASSWDW-PROMPT2
                 PERFORM VERIFY-PASSWORD
                 IF PASSWDW-STAT NOT = "OK"
                    GO TO MODIFY-ROUTINE.

      * OVERRIDE CALCULATED APR RATE
           IF ELE = 26
              IF APR-OK = "N"
                 MOVE APR-SEQ TO PASSWDW-SEQ
                 MOVE "TO OVERRIDE RATES.........:" TO PASSWDW-PROMPT2
                 PERFORM VERIFY-PASSWORD
                 IF PASSWDW-STAT NOT = "OK"
                    MOVE "N" TO APR-OK
                    GO TO MODIFY-ROUTINE
                 ELSE
                    MOVE "Y" TO APR-OK.

       ENTER-ELE.
           IF ELE = 1
              IF LOAN-QUOTE-FG = "Q"
                 MOVE 0 TO VDUNUM0
                 GO TO F-01.
      *    IF ELE < 5
      *       MOVE
      *      "F1, F5-CREATE BORR, F6-SCAN BY NAME, ENTER BORROWER #:"
      *                                                     TO LEGEND
      *       PERFORM SEND-LEGEND.
           IF ELE = 5
              IF NEW-FLAG NOT = 1
                 GO TO MODIFY-ROUTINE
              ELSE
                 IF CHFG(5) = "N"
                    GO TO F-052.
           IF ELE = 6
              IF CHFG(6) = "N"
                 MOVE "S  A000060." TO VDU
                 MOVE TRANS-DATE-YYYYMMDD TO PD-LOANDATE
                                             DATE-YYYYMMDD
                 PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
                 MOVE DATE-MMDDYY TO VDU-DATE-EDIT
                 PERFORM SCREENIO
                 MOVE " " TO CHFG(6)
                 PERFORM DEFAULT-INT-PMT-DATES
                 IF IO-FG = 9
                    MOVE 0 TO IO-FG
                    GO TO ENTER-ELE
                 ELSE
                    IF AMT-HOLDER = 0
                       GO TO CONT1-F-06
                    ELSE
                       GO TO CREATE-ROUTINE.
           IF ELE = 7
              IF CHFG(7) = "N"
                 MOVE " " TO CHFG(7)
                 GO TO CREATE-ROUTINE.
           IF ELE = 8
              IF CHFG(8) = "N"
                 MOVE " " TO CHFG(8)
                 GO TO CREATE-ROUTINE.
           IF ELE = 9
              IF CHFG(9) = "N"
                 MOVE " " TO CHFG(9)
                 GO TO CREATE-ROUTINE.
           IF ELE = 10
              IF CHFG(10) = "N"
                 MOVE BR-DEFAULT-LN-UNITPER-CD TO VDUBUF
                 GO TO GOTO-FIELD.
           IF ELE = 12
              IF CHFG(12) = "N"
                 MOVE " " TO CHFG(12)
                 GO TO CREATE-ROUTINE.
           IF ELE = 17
              IF CHFG(17) = "N"
                 MOVE " " TO CHFG(17)
                 MOVE 0 TO PD-LASTPYAMT
                           CIP-BALLOON
                 MOVE "I" TO PD-BALLOON-FG
                             CIP-BALLOON-SW
                 GO TO CREATE-ROUTINE.
           IF ELE = 5
              MOVE "F1, ENTER THE STATE OF ORIGINATION:" TO LEGEND
              PERFORM SEND-LEGEND.
           IF ELE = 6
              MOVE "F1, ENTER THE DATE OF THE LOAN:" TO LEGEND
              PERFORM SEND-LEGEND.
           IF ELE = 7
              MOVE "F1, F6-PURPOSE SCAN, ENTER PURPOSE CODE:"
                                                       TO LEGEND
              PERFORM SEND-LEGEND.
           IF ELE = 8
              MOVE
              "F1, F6-COLLATERAL SCAN, ENTER COLLATERAL CODE:"
                                                    TO LEGEND
              PERFORM SEND-LEGEND.
           IF ELE = 9
              MOVE "F1, F6-SOURCE SCAN, ENTER SOURCE CODE:"
                                                       TO LEGEND
              PERFORM SEND-LEGEND.
           IF ELE = 10
              IF BR-UNITPER-Y-D-FG = "Y"
                 MOVE
                 "F1, ENTER THE PAYMENT PERIOD (M, W, S, Y OR D):"
                                                         TO LEGEND
                 PERFORM SEND-LEGEND
              ELSE
                 MOVE
                 "F1, ENTER THE PAYMENT PERIOD (M, W, S OR B):"
                                                         TO LEGEND
                 PERFORM SEND-LEGEND.
           IF ELE = 11
              IF PD-SRCD = "SC" OR "PB" OR "RN"
                              OR "RB" OR "RO"
                 MOVE " " TO CHFG(11)
                 MOVE "F6-SCAN, ENTER LOAN NO, BEING RENEWED:"
                                                         TO LEGEND
                 PERFORM SEND-LEGEND.
           IF ELE = 12
              MOVE " " TO CHFG(11)
              MOVE "F1, ENTER TERM OF THE LOAN:" TO LEGEND
              PERFORM SEND-LEGEND.
           IF ELE = 13
              MOVE "F1, ENTER FIRST PAYMENT DUE DATE:" TO LEGEND
              PERFORM SEND-LEGEND.
           IF ELE = 14
              MOVE " " TO CHFG(11)
              IF CAN-LOAN-NO = " "
                 MOVE " " TO CHFG(13)
              END-IF
      *JGW    IF (PD-SRCD = "RN" OR "RB") AND (NEW-FLAG = 1)
              IF (PD-SRCD = "RN")
                  MOVE "N" TO CHFG(14)
              ELSE
                 IF CAN-LOAN-NO NOT = " "
                    MOVE "N" TO CHFG(14)
                 ELSE
                    IF PD-GIFTNET NOT = "Y"
                       MOVE " " TO CHFG(14)
                       MOVE "F1, ENTER LOAN PROCEEDS:" TO LEGEND
                       PERFORM SEND-LEGEND.
      *    IF ELE = 16           ----OPEN
      *       MOVE "F1, ENTER THE PERCENT(%) DOWN:" TO LEGEND
      *       PERFORM SEND-LEGEND.
           IF ELE = 17
              IF PD-BALLOON-LAST-FG = "B"
                 MOVE "F1, ENTER THE BALLOON AMOUNT:" TO LEGEND
                 PERFORM SEND-LEGEND
              ELSE
                 MOVE "F1, ENTER THE AMOUNT OF LAST PAYMENT:"
                                                        TO LEGEND
                 PERFORM SEND-LEGEND.
           IF ELE = 18
              IF (GP-ENTER-LPO-FG = "Y")
                 MOVE "F1, F3-LPO, ENTER INSURANCE CODES (EX. MGG):"
                                                         TO LEGEND
                 PERFORM SEND-LEGEND
              ELSE
                 MOVE "F1, ENTER INSURANCE CODES (EX. MGG):"
                                                         TO LEGEND
                 PERFORM SEND-LEGEND.
           IF ELE = 18
              IF (GP-ENTER-LPO-FG = "Y")
                 IF VDUSTATUS = "16"
                    MOVE PD-LPO TO WINDOW-LPO
                    MOVE PD-REP-CD TO WINDOW-REP
                    MOVE PD-PROCESS-CD TO WINDOW-PROCESS
                    MOVE "WI/TFCCDS" TO FORM-NAM
                    CALL FORM-PROGX USING FORM-PATH WINDOW-WORKERS
                    CANCEL FORM-PROGX
                    MOVE WINDOW-LPO TO PD-LPO
                    MOVE WINDOW-REP TO PD-REP-CD
                    MOVE WINDOW-PROCESS TO PD-PROCESS-CD.
           IF ELE = 19
              IF NEW-FLAG = 1
                 MOVE
                   "F1, F3-ENTER FEES, (RTN)-BYPASS ENTRY:" TO LEGEND
                 PERFORM SEND-LEGEND
              ELSE
                 PERFORM DO-FEEENT-WINDOW
                 GO TO CREATE-ROUTINE.
           IF ELE = 20 OR 21
              MOVE "F1, ENTER FEE AMOUNT:" TO LEGEND
              PERFORM SEND-LEGEND.
           IF ELE = 22
              MOVE
              "F1, F6-DEALER SCAN, ENTER DEALER NO:" TO LEGEND
              PERFORM SEND-LEGEND.


           IF NEW-FLAG = 1
              IF ELE = 26
                 GO TO CREATE-ROUTINE.
           IF NEW-FLAG = 1
              IF ELE > 26 AND ELE < 33
                 GO TO CREATE-ROUTINE.

           IF ELE = 31
              IF SP-DEFAULT-O2-INS-COMP NUMERIC  AND
                    SP-DEFAULT-O2-INS-COMP NOT = 0
                 MOVE "SN N020310." TO VDU
                 MOVE SP-DEFAULT-O2-INS-COMP TO VDUNUM0
                 PERFORM SCREENIO.
           IF ELE = 32
              IF GP-DEFAULT-O3-INS-COMPANY NOT = 0
                 MOVE "SN N020320." TO VDU
                 MOVE GP-DEFAULT-O3-INS-COMPANY TO VDUNUM0
                 PERFORM SCREENIO.
           IF ELE = 34
              IF SP-SCFRMLA = 0
                 GO TO CREATE-ROUTINE
              ELSE
                 IF SP-SCFRMLA = 33 OR 39
                    MOVE "F1, F3-CHANGE POINTS/ORIG.FEE CHARGES:"
                                                          TO LEGEND
                 ELSE
                    MOVE "F1, ENTER THE SERVICE CHARGE:" TO LEGEND
                 END-IF
                 PERFORM SEND-LEGEND.
           IF ELE = 35
              IF SP-MFFRMLA = 0
                 GO TO CREATE-ROUTINE
              ELSE
                 MOVE "F1, ENTER THE MAINTENANCE FEE:" TO LEGEND
                 PERFORM SEND-LEGEND.
           IF ELE = 36
              IF SP-PYEXFRMLA = 0
                 GO TO CREATE-ROUTINE
              ELSE
                 MOVE "F1, ENTER THE FIRST PAYMENT EXTENSION:"
                                                         TO LEGEND
                 PERFORM SEND-LEGEND.
           IF ELE = 42
              MOVE "F1, ENTER THE REGULAR PAYMENT AMOUNT:" TO LEGEND
              PERFORM SEND-LEGEND.

           IF CHKEY NOT = ZEROS
              IF CHFG (ELE) = " "
                 IF NEW-FLAG NOT = 1
                    GO TO MODIFY-ROUTINE.

           IF CHFG(ELE) = "N"
              IF NEW-FLAG = 1
                 GO TO CREATE-ROUTINE
              ELSE
                 GO TO MODIFY-ROUTINE.

           MOVE SPEC(ELE) TO VDU.
           MOVE ELE TO VDUROW.
           MOVE 0 TO VDUCOL.
           MOVE "." TO VDUDECIMAL.
           PERFORM SCREENIO.

       TEST-F1.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.
       TEST-F7.
           IF VDUSTATUS NOT = "1>"
              GO TO TEST-F3.
           IF MENU-FG = 99
              GO TO ENTER-ELE.
           MOVE "END" TO IO-TYPE.
           GO TO END-ROUTINE.
       TEST-F3.
           IF VDUSTATUS NOT = "16"
              GO TO TEST-UP.
           IF NEW-FLAG = 1
              IF ELE = 19
                 PERFORM DO-FEEENT-WINDOW
                 GO TO CREATE-ROUTINE.
           IF ELE = 34
              IF SP-SCFRMLA = 33 OR 39
                 MOVE SP-SCMIN            TO SCHG33-SP-SCMIN
                 MOVE SP-SCMAX            TO SCHG33-SP-SCMAX
                 SUBTRACT PD-STATE-WORKER FROM PD-SERCHG
                                          GIVING SCHG33-CUR-POINTS
                 MOVE PD-STATE-WORKER TO SCHG33-CUR-ORIGFEE
                 MOVE "WI/SCHG33" TO FORM-NAM
                 CALL FORM-PROGX USING FORM-PATH SCHG33-WORKERS
                 CANCEL FORM-PROGX
                 IF SCHG33-RTNCD NOT = " "
                    GO TO ENTER-ELE
                 END-IF
                 ADD SCHG33-CHGTO-POINTS
                     SCHG33-CHGTO-ORIGFEE  GIVING PD-SERCHG
                 MOVE SCHG33-CHGTO-ORIGFEE TO PD-STATE-WORKER

                 MOVE "S  A010341." TO VDU
                 MOVE "I" TO VDUBUF PD-SERCHG-FG
                 PERFORM SCREENIO

                 MOVE PD-SERCHG       TO CIP-SERVICE-CHG
                 MOVE PD-STATE-WORKER TO CIP-STATE-WORKER
                 MOVE PD-SERCHG-FG    TO CIP-SERVICE-CHG-SW
                 PERFORM CALL-LOAN-CALCULATIONS
                 GO TO CREATE-ROUTINE.

       TEST-UP.
           IF VDUSTATUS NOT = "1U"
              GO TO TEST-DN.
           IF ELE = 6
              MOVE " " TO CHFG(5).
           IF ELE = 7
              MOVE " " TO DATE-STAT.
           IF ELE = 11
              IF NEW-FLAG NOT = 1
                 MOVE 9 TO ELE
                 GO TO ENTER-ELE.
           IF ELE = 23
              MOVE " " TO CHFG(22).
           IF NEW-FLAG = 1
              MOVE -1 TO ONE
              GO TO CREATE-ROUTINE.
       TEST-DN.
           IF VDUSTATUS NOT = "1D"
              GO TO TEST-F5.
           MOVE 1 TO ONE.
           GO TO GOTO-FIELD.
       TEST-F5.
           IF VDUSTATUS NOT = "1:"
              GO TO TEST-F6.
      *    IF ELE < 5
      *       PERFORM BORROWER-CREATE
      *       IF IO-FG = 0
      *          GO TO GOTO-FIELD
      *       ELSE
      *          GO TO ENTER-ELE.
       TEST-F6.
           IF VDUSTATUS NOT = "1<"
              GO TO TEST-00.
      *    IF ELE < 5
      *       PERFORM BORROWER-SCAN
      *       IF IO-FG = 0
      *          GO TO GOTO-FIELD
      *       ELSE
      *          GO TO ENTER-ELE.
           IF (ELE = 7 OR 8 OR 9)
               MOVE 0 TO CDCODE-FEE-FG
               PERFORM CODE-SCAN
               IF IO-FG = 0
                  GO TO GOTO-FIELD
               ELSE
                  GO TO ENTER-ELE.
           IF ELE = 11
              PERFORM BWLN-SCAN
              IF IO-FG = 0
                 GO TO GOTO-FIELD
              ELSE
                GO TO ENTER-ELE.
           IF ELE = 22
              PERFORM DEALER-SCAN
              IF IO-FG = 0
                 GO TO GOTO-FIELD
              ELSE
                 GO TO ENTER-ELE.
       TEST-00.
           IF VDUSTATUS NOT = "00"
              GO TO ENTER-ELE.
       GOTO-FIELD.
           MOVE 1 TO ONE.
           IF ELE > 20
              GO TO GOTO-FIELD-20.
           GO TO F-01 F-02 F-03 F-04 F-05 F-06 F-07 F-08 F-09 F-10
                 F-11 F-12 F-13 F-14 F-15 F-16 F-17 F-18 F-19 F-20
                 DEPENDING ON ELE.
       GOTO-FIELD-20.
           SUBTRACT 20 FROM ELE GIVING OVER-20.
           GO TO F-21 F-22 F-23 F-24 F-25 F-26 F-27 F-28 F-29 F-30
                 F-31 F-32 F-33 F-34 F-35 F-36 F-37 F-38 F-39 F-40
                 F-41 F-42
                 DEPENDING ON OVER-20.
           GO TO END-ENTRY-MODULE.

      *************************************
      *    S E T   E L E M E N T S        *
      *************************************
       F-01.
           PERFORM BORROWER-ENTRY.
           IF IO-FG = 9
              IF IO-TYPE = "CLR"
                 GO TO END-ENTRY-MODULE
              ELSE
                 GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       F-02.
           PERFORM BORROWER-ENTRY.
           IF IO-FG = 9
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       F-03.
           PERFORM BORROWER-ENTRY.
           IF IO-FG = 9
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       F-04.
           PERFORM BORROWER-ENTRY.
           IF IO-FG = 9
              GO TO ENTER-ELE.
           PERFORM BW-CHECK-ENDORSER.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-05.
           MOVE VDUBUF TO PD-ORGST
                          PD-OWNST.

      *-----------------------------------------------------------------
       F-052.
           MOVE SPACES TO CHFG(5).
           MOVE "^-BACK, F6-SCAN, ENTER SUB CLASS:" TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "ENNN020052." TO VDU.
           PERFORM SCREENIO.

           IF ( UP-KEY ) GO TO ENTER-ELE.

           IF ( F6-KEY ) PERFORM SUBCLASS-SCAN
                         IF ( IO-FG NOT = ZEROES )
                            GO TO F-052.

           IF NOT ( DOWN-KEY OR ENTER-KEY ) GO TO F-052.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE PD-ORGST    TO SP-ORGST.
           MOVE PD-SPRCLASS TO SP-SPRCLASS.
           MOVE VDUNUM0     TO PD-SUBCLASS SP-SUBCLASS.
           MOVE ZEROES      TO SP-LAWCODE.
           PERFORM START-SP1-FILE.
       F-052-NEXT-SP.
           PERFORM READ-SP1-FILE-NEXT.
           IF NOT (( SP-ORGST    = PD-ORGST    ) AND
                   ( SP-SPRCLASS = PD-SPRCLASS ) AND
                   ( SP-SUBCLASS = PD-SUBCLASS ))
              MOVE 9 TO IO-FG.

           IF ( IO-FG NOT = ZEROES )
              PERFORM SP-INVALID
              GO TO F-052.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    CHECK 'SPR' CAN LOANS FLAG!!!
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF (( SP-CAN-LOANS-ONLY ) AND ( CAN-LOAN-NO     = SPACES ))
              MOVE MESSAGE-INVALID-SPR-CL TO MESS
              PERFORM SEND-MESS
              GO TO F-052.

           IF (( SP-CAN-LOANS-NO   ) AND ( CAN-LOAN-NO NOT = SPACES ))
              MOVE MESSAGE-INVALID-SPR-NON-CL TO MESS
              PERFORM SEND-MESS
              GO TO F-052.

           IF SP-UNITPER-CD = SPACES OR "M"
              MOVE MESSAGE-INVALID-SPR-PAY-CODE    TO MESS
              PERFORM SEND-MESS
              GO TO F-052.

           MOVE "S  A015053." TO VDU.
           MOVE SP-DESC       TO VDUBUF.
           PERFORM SCREENIO.

           PERFORM RESCISSION-ROUTINE.

           IF ( RESCISSION-STATUS-BAD )
              GO TO F-052.

           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-06.
           MOVE VDU-DATE-YYYYMMDD TO PD-LOANDATE.
           IF VDU-DATE-YYYYMMDD = 0
              GO TO ENTER-ELE.

           PERFORM CHECK-SP-EFFECTIVE.
           IF IO-FG = 9
              PERFORM SP-INVALID
              GO TO ENTER-ELE
           ELSE
              MOVE "S  A015053." TO VDU
              MOVE SP-DESC       TO VDUBUF
              PERFORM SCREENIO.
           MOVE SP-LAWCODE TO PD-LAWCODE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - -{ RESCISSION }
           PERFORM RESCISSION-ROUTINE.
           IF ( RESCISSION-STATUS-BAD )
              GO TO ENTER-ELE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF PD-INTDATE = 0
              MOVE "S  A000061." TO VDU
              MOVE PD-LOANDATE TO PD-INTDATE
                                  DATE-YYYYMMDD
              PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
              MOVE DATE-MMDDYY TO VDU-DATE-EDIT
              PERFORM SCREENIO.

           IF PD-1STPYDATE NOT = 0
               PERFORM SPTEST-EXTDATE-VALID
               IF LIMIT-ERROR = "Y"
                 GO TO ENTER-ELE
              ELSE
                 GO TO CONT1-F-06.

           PERFORM SET-1ST-PAY-DATE.
           PERFORM SPTEST-EXTDATE-VALID.
           IF LIMIT-ERROR = "Y"
              GO TO ENTER-ELE.

           IF DATE-STAT = "N"
              IF NEW-FLAG NOT = 1
      * CITIZENS TEST
                 MOVE 800 TO CIP-NO-OF-TIMES-CALLED
      *A90?      MOVE 0 TO CIP-NO-OF-TIMES-CALLED
                 MOVE PD-LOANDATE  TO CIP-LOANDATE
                 MOVE PD-INTDATE   TO CIP-INTDATE
                 MOVE PD-1STPYDATE TO CIP-1STPYDATE
                 PERFORM CALL-LOAN-CALCULATIONS.
           IF DATE-STAT = "N"
              GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       CONT1-F-06.

           IF ( NEW-FLAG NOT = 1     ) AND
              ( PD-POCD-INRESCISSION )
              GO TO CREATE-ROUTINE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "^-BACK, ENTER THE INTEREST DATE OF THE LOAN:"
                                                         TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "ERNM060061." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO CONT1-F-06.
           IF VDUNUM0 = 0
              GO TO CONT1-F-06.

           MOVE PD-INTDATE TO HOLD-DATE-WORKER.
           MOVE VDU-DATE-YYYYMMDD TO PD-INTDATE.

           IF PD-INTDATE < PD-LOANDATE
              MOVE "MUST NOT BE PRIOR TO THE LOAN DATE" TO MESS
              PERFORM SEND-MESS
              MOVE HOLD-DATE-WORKER TO PD-INTDATE
              GO TO CONT1-F-06.

           IF NEW-FLAG NOT = 1
              IF PD-1STPYDATE NOT > PD-INTDATE
                 MOVE
                 "INTEREST DATE MUST BE PRIOR TO FIRST PAYMENT DATE"
                                                           TO MESS
                 PERFORM SEND-MESS
                 MOVE HOLD-DATE-WORKER TO PD-INTDATE
                 GO TO CONT1-F-06.

           PERFORM SPTEST-EXTDATE-VALID.
           IF LIMIT-ERROR = "Y"
              MOVE HOLD-DATE-WORKER TO PD-INTDATE
              GO TO CONT1-F-06.

           IF NEW-FLAG NOT = 1
              IF HOLD-DATE-WORKER NOT = PD-INTDATE
                 PERFORM SET-UNITPER-INFO.

           IF NEW-FLAG NOT = 1
      * CITIZENS TEST
              MOVE 800 TO CIP-NO-OF-TIMES-CALLED
      *A90?   MOVE 0 TO CIP-NO-OF-TIMES-CALLED
              MOVE PD-LOANDATE TO CIP-LOANDATE
              MOVE PD-INTDATE  TO CIP-INTDATE
              PERFORM CALL-LOAN-CALCULATIONS.

           GO TO CREATE-ROUTINE.
       F-07.
           MOVE "PU"   TO CD-TYPE.
           MOVE VDUBUF TO PD-PURCD
                          CD-CODE.
           PERFORM SEND-CD-DESC.
           IF IO-FG = 9
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       F-08.
           MOVE "CO"   TO CD-TYPE.
           MOVE VDUBUF TO PD-COLLCD
                          CD-CODE.
           PERFORM SEND-CD-DESC.
           IF IO-FG = 9
              GO TO ENTER-ELE.
           MOVE CD-CO-SECURED TO SPTEST-CD-SECURED.

      *- - - - - - - - - - - - - - - - - - - - - - - - - -{ RESCISSION }
           PERFORM RESCISSION-ROUTINE.

           IF ( RESCISSION-STATUS-BAD )
              GO TO ENTER-ELE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           GO TO CREATE-ROUTINE.
       F-09.
           MOVE "SR" TO CD-TYPE.
           MOVE VDUBUF TO PD-SRCD
                          CD-CODE.
           PERFORM SEND-CD-DESC.
           IF IO-FG = 9
              GO TO ENTER-ELE.

           IF NOT (PD-SRCD = "PB" OR "RN" OR "SC" OR "RB" OR "RO")
              IF SP-SCFRMLA = 63
                 MOVE 0 TO WISCHG-PRIOR-SERCHG
                           CIP-RESTRICT-SERCHG.

           IF GP-THE-ONLY-SF-SRCD NOT = " "
              IF PD-SRCD = GP-THE-ONLY-SF-SRCD
                 MOVE "INVALID FOR DIRECT LOANS" TO MESS
                 PERFORM SEND-MESS
                 GO TO ENTER-ELE.

           IF PD-SRCD = "ET" OR "LP" OR "NP" OR "RT" OR "ST" OR "RB"
              MOVE SRCD-SEQ TO PASSWDW-SEQ
              MOVE "TO USE THIS SRCD" TO PASSWDW-PROMPT2
              PERFORM VERIFY-PASSWORD
              IF PASSWDW-STAT NOT = "OK"
                 MOVE "INVALID SOURCE CODE" TO MESS
                 PERFORM SEND-MESS
                 GO TO ENTER-ELE.

           IF CIP-SOURCE-CD = "RN" OR "SC" OR "PB" OR " "
                                 OR "RB" OR "RO"
              IF NOT (PD-SRCD = "RN" OR "SC" OR "PB"
                                   OR "RB" OR "RO")
                 PERFORM BLANK-PRIOR-LOAN.

           MOVE PD-SRCD TO CIP-SOURCE-CD.
           IF NEW-FLAG NOT = 1
              PERFORM VERIFY-PROCEEDS-DOWN
              IF IO-FG = 9
                 GO TO ENTER-ELE
              END-IF
              PERFORM VERIFY-PROCEEDS
              MOVE 0 TO IO-FG
              IF PD-SRCD = "RN" OR "SC" OR "PB" OR "RB" OR "RO"
                 MOVE 11 TO ELE
                 GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       F-10.
           MOVE VDUBUF TO PD-UNITPER-CD
                          UPERTST-UNITPER-CD.
           PERFORM UPERTST-UNITPER-CODE.
           IF UPERTST-ERRCD NOT = " "
              MOVE " " TO CHFG(10)
              GO TO ENTER-ELE.
           IF CHFG(10) = "N"
              MOVE " " TO CHFG(10)
              GO TO CREATE-ROUTINE.
           IF BR-UNITPER-Y-D-FG NOT = "Y"
              MOVE "SNNN030101." TO VDU
              MOVE 1 TO PD-UNITPER-FREQ
                        VDUNUM0
              PERFORM SCREENIO
              PERFORM SET-1ST-PAY-DATE
              IF NEW-FLAG NOT = 1
                       OR PD-ORGTERM = 1
                 PERFORM SET-UNITPER-INFO
                 GO TO F-102
              ELSE
                 GO TO F-102.

       F-101.
           PERFORM ALARM.
           MOVE
           "F1, ^, ENTER THE FREQUENCY (EX. 2 = EVERY TWO MONTHS!):"
                                                         TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "ENNN030101." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO F-101.
           MOVE VDUNUM0 TO PD-UNITPER-FREQ
                           UPERTST-UNITPER-FREQ.
           MOVE PD-UNITPER-CD TO UPERTST-UNITPER-CD.
           PERFORM UPERTST-UNITPER-FREQUENCY.
           IF UPERTST-ERRCD NOT = " "
              GO TO F-101.

           PERFORM SET-1ST-PAY-DATE.
           IF NOT (PD-UNITPER-CD = "M"
                         AND PD-UNITPER-FREQ = 1)
              PERFORM UPERTST-SET-MESS
              MOVE UPERTST-MESS TO MESS
              PERFORM SEND-MESS.
           IF NEW-FLAG NOT = 1
                    OR PD-ORGTERM = 1
              PERFORM SET-UNITPER-INFO.

       F-102.
           PERFORM ALARM.
           IF NEW-FLAG NOT = 1
                   AND PD-ORGTERM = 1
              MOVE "F1, ^, L-LAST PAYMENT" TO LEGEND
           ELSE
              MOVE "F1, ^, B-BALLOON OR L-LAST PAYMENT??" TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "E  A010102." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              IF BR-UNITPER-Y-D-FG NOT = "Y"
                 GO TO ENTER-ELE
              ELSE
                 GO TO F-101.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO F-102.
           IF NOT (VDUBUF = "B" OR "L")
              GO TO F-102.
           MOVE VDUBUF TO PD-BALLOON-LAST-FG.
           PERFORM DISPLAY-BALLOON-LAST.
           PERFORM SET-1ST-PAY-DATE.
           IF NEW-FLAG NOT = 1
      *       IF PD-ORGTERM = 1
      *              AND PD-BALLOON-LAST-FG = "B"
      *          STRING "SINGLE PAYS MUST HAVE LAST "
      *                 "PAYMENT FLAG SET, NOT BALLOON FLAG!"
      *                 "                              "
      *                 DELIMITED BY SIZE INTO MESS
      *          PERFORM SEND-MESS
      *          GO TO F-102
      *       END-IF
              MOVE 800 TO CIP-NO-OF-TIMES-CALLED
              PERFORM CALL-LOAN-CALCULATIONS.
           GO TO CREATE-ROUTINE.

       F-11.
           MOVE VDUNUM0 TO W-LNNO.
           IF NOT (PD-SRCD = "RN" OR "PB" OR "SC"
                                OR "RB" OR "RO")
              IF VDUNUM0 = 0
                 MOVE "S  A060110." TO VDU
                 PERFORM SCREENIO
                 MOVE "S  A090111." TO VDU
                 PERFORM SCREENIO
                 MOVE 0 TO PD-PRLNNO   PD-PRBAL
                           PD-PRINTDUE PD-PRFINCHGRB
                           PD-PRINSRB  PD-PRPAID-AHEAD
                 GO TO CREATE-ROUTINE
              ELSE
                 MOVE W-LNNO TO PD-PRLNNO
                 GO TO CREATE-ROUTINE
           ELSE
              IF VDUNUM0 = 0
                 MOVE "PRIOR LOAN NO. REQUIRED FOR LOAN RENEWALS"
                       TO MESS
                 PERFORM SEND-MESS
                 GO TO ENTER-ELE.

           MOVE W-LNNO TO PD-PRLNNO.
           PERFORM PROCESS-PRIOR-LOAN.

           IF PRIOR-LOAN-NOT-VALID
              PERFORM SEND-PRIOR-MESS
              GO TO ENTER-ELE.

           IF ERROR-IN-PRIOR-LOAN
              PERFORM PRIOR-LOAN-ERROR-MSG
              GO TO ENTER-ELE.

           MOVE "SNNN062111." TO VDU.
           MOVE PRIOR-LNAMT TO VDUNUM2.
           PERFORM SCREENIO.

           GO TO CREATE-ROUTINE.
       F-12.
           IF VDUNUM0 = 0
              GO TO ENTER-ELE.
           MOVE PD-ORGTERM TO HOLD-WORKER.
           MOVE VDUNUM0 TO PD-ORGTERM
                           PD-SUM-ORGTERM
                           PD-ANTICTERM.
      *    IF PD-ORGTERM = 1
      *       IF PD-BALLOON-LAST-FG NOT = "L"
      *          STRING "SINGLE PAYS MUST HAVE LAST "
      *                 "PAYMENT FLAG SET, NOT BALLOON FLAG!"
      *                 "                              "
      *                 DELIMITED BY SIZE INTO MESS
      *          PERFORM SEND-MESS
      *          MOVE HOLD-WORKER TO PD-ORGTERM
      *          GO TO ENTER-ELE.
           PERFORM SP-EDT-TABLE-LOOKUP.
           IF IO-FG = 9
              MOVE 0 TO IO-FG
              MOVE HOLD-WORKER TO PD-ORGTERM
              GO TO ENTER-ELE.

           IF HOLD-WORKER NOT = PD-ORGTERM
              PERFORM SET-UNITPER-INFO.

           IF NEW-FLAG NOT = 1
      * CITIZENS TEST
              MOVE 800 TO CIP-NO-OF-TIMES-CALLED
      *A90?   MOVE 0 TO CIP-NO-OF-TIMES-CALLED
              MOVE PD-1STPYDATE TO CIP-1STPYDATE
              MOVE PD-ORGTERM TO CIP-TERM
              PERFORM CALL-LOAN-CALCULATIONS.
           GO TO CREATE-ROUTINE.
       F-13.
           IF VDUNUM0 = 0
              GO TO ENTER-ELE.
           MOVE PD-1STPYDATE TO HOLD-DATE-WORKER.
           MOVE VDU-DATE-YYYYMMDD TO PD-ORIG-1STPYDATE
                                     PD-1STPYDATE.
           PERFORM SPTEST-EXTDATE-VALID.
           IF LIMIT-ERROR = "Y"
              GO TO ENTER-ELE.

           IF PD-1STPYDATE NOT > PD-INTDATE
              MOVE "MUST BE AFTER THE INTEREST DATE" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.

      * DO NOT ALLOW A FIRST PAYMENT DATE < 30 DAYS FROM LOAN DATE
      * OR < THAN 1 UNIT PERIOD FROM INT DATE
           MOVE PD-INTDATE      TO NDTE-DATE.
           MOVE 1               TO NDTE-HOLD.
           MOVE PD-UNITPER-CD   TO DATER-UNITPER-CD.
           MOVE PD-UNITPER-FREQ TO DATER-UNITPER-FREQ.
           PERFORM INCREMENT-UNITPER.
           IF PD-1STPYDATE < NDTE-DATE
              MOVE 1STPY30-SEQ TO PASSWDW-SEQ
              MOVE "1ST PAY < 30 DAYS FROM LNDATE" TO PASSWDW-PROMPT2
              PERFORM VERIFY-PASSWORD
              IF PASSWDW-STAT NOT = "OK"
                 GO TO ENTER-ELE.

      *  JTG 011001 DON'T ALLOW A FIRST PAY DAY OF 31 WHEN SEMI MONTHLY PAPER
      *                                                            FFG PR#333
      *CS  IF PD-UNITPER-CD = "S"
      *CS     IF PD-1STPYDA = 31
      *CS        MOVE " 31ST OF MONTH INVALID FOR SEMI-MONTHLY PAPER"
      *CS                                                     TO MESS
      *CS        PERFORM SEND-MESS
      *CS        GO TO ENTER-ELE.


      *  JTG 011001 DON'T ALLOW A FIRST PAY DAY OF 31 WHEN SEMI MONTHLY PAPER
      *                                                            FFG PR#333
      *  CS  021001 PER JOHN DON'T ALLOW A FIRST PAY DAY OF 14 OR 15 EITHER IF
      *      SEMI-MONTHLY.  FEB CAUSED THE CYCLE DATES FOR LATE CHARGES TO BE
      *      OUT OF SYNC. FOR EXAMPLE: WITH PAYDATE OF 15 LCPDTH GOES TO 2/28/YY      *      MARCH WENT TO 3/13/YY INSTEAD OF 3/15/YY [DISCOVERED AT FFG]


           IF PD-UNITPER-CD = "S"
              IF PD-1STPYDA = 14 OR 15 OR 31
                 PERFORM INVALID-DUE-DAY
                 PERFORM SEND-MESS
                 GO TO ENTER-ELE.

           IF HOLD-DATE-WORKER NOT = PD-1STPYDATE
              PERFORM SET-UNITPER-INFO.

           IF NEW-FLAG NOT = 1
              MOVE PD-1STPYDATE TO CIP-1STPYDATE
              MOVE PD-ORGTERM   TO CIP-TERM
      * CITIZENS TEST
              MOVE 800 TO CIP-NO-OF-TIMES-CALLED
              PERFORM CALL-LOAN-CALCULATIONS.
           GO TO CREATE-ROUTINE.
       F-14.
           IF VDUNUM0 = 0
              GO TO ENTER-ELE.
           MOVE VDUNUM2 TO PD-SELLPR
                           PD-PROCEEDS.

      * SP-EDT-TABLE-LOOKUP:
           PERFORM SP-EDT-TABLE-LOOKUP.
           IF IO-FG = 9
              MOVE 0 TO IO-FG
              GO TO ENTER-ELE.

      * DO NOT PUT THE MOVE 0 TO IO-FG IN THE VERIFY-PROCEEDS ROUTINE,
      * THAT ROUTINE CAN NOT 0 OUT IO-FG, USED OTHER PLACES.
           MOVE 0 TO IO-FG.
           PERFORM VERIFY-PROCEEDS.
           IF IO-FG = 9
              MOVE 0 TO IO-FG
              GO TO ENTER-ELE.

           IF NEW-FLAG = 1
      *       MOVE PD-SELLPR TO PD-PROCEEDS
              MOVE "I" TO PD-SELLPR-FG
                          PD-PROCEEDS-FG
              GO TO CREATE-ROUTINE.
       F-141.
           MOVE "^-BACK, ENTER 'C' TO CALCULATE THE PROCEEDS:"
                                                     TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "S  A010141." TO VDU.
           MOVE "I" TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "E  A010141." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE.
           IF NOT (VDUBUF = "I" OR "C")
              PERFORM ICE-OR-A-MSG
              GO TO F-141.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO F-141.

           IF NOT (VDUBUF = PD-SELLPR-FG)
              IF VDUBUF = "I"
                 PERFORM SETUP-BLINPC-BUF
                 MOVE " " TO BLINPC-SELLPR-FG
                 PERFORM BLINPC-CALC-WINDOW
                 IF BLINPC-STATUS NOT = "00"
                    GO TO F-141
                 ELSE
                    PERFORM AFTER-SELLPR-CALC-OPT
              ELSE
                 IF VDUBUF = "C"
                    MOVE "I" TO PD-REGPYAMT-FG
                                PD-BALLOON-FG
                    MOVE "C" TO PD-SELLPR-FG
                                PD-PROCEEDS-FG
                 ELSE
                    PERFORM ICE-OR-A-MSG
                    GO TO F-141.

           MOVE PD-SELLPR TO CIP-SELLPR
                             CIP-PROCEEDS.
           MOVE PD-REGPYAMT-FG TO CIP-REGPYAMT-SW.
           MOVE PD-PROCEEDS-FG TO CIP-PROCEEDS-SW.
           MOVE PD-BALLOON-FG TO CIP-BALLOON-SW.
           PERFORM CALL-LOAN-CALCULATIONS.
           GO TO CREATE-ROUTINE.
       F-15.
           GO TO CREATE-ROUTINE.
       F-16.
           GO TO CREATE-ROUTINE.
       F-17.
           IF PD-ORGTERM = 1
              IF VDUNUM2 NOT = 0
                 MOVE "ENTER ZERO FOR SINGLE PAY LOANS!!" TO MESS
                 PERFORM SEND-MESS
                 GO TO ENTER-ELE.
           IF (PD-BALLOON-LAST-FG = "L")
                       AND (PD-ORGTERM NOT = 1)
              IF VDUNUM2 = 0
                 GO TO ENTER-ELE.
           MOVE VDUNUM2 TO PD-LASTPYAMT
                           CIP-BALLOON.
           IF NEW-FLAG = 1
              MOVE "I" TO PD-BALLOON-FG
                          CIP-BALLOON-SW
              GO TO CREATE-ROUTINE.
       F-171.
           MOVE "S  A010171." TO VDU.
           MOVE "I" TO VDUBUF
           PERFORM SCREENIO.

           MOVE "E  A010171." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO F-171.

           IF NOT (VDUBUF = PD-BALLOON-FG)
              IF VDUBUF = "I"
                 PERFORM SETUP-BLINPC-BUF
                 MOVE " " TO BLINPC-BALLOON-FG
                 PERFORM BLINPC-CALC-WINDOW
                 IF BLINPC-STATUS NOT = "00"
                    GO TO F-171
                 ELSE
                    PERFORM AFTER-BALLO-CALC-OPT
              ELSE
                 IF VDUBUF = "C"
                    MOVE "I" TO PD-REGPYAMT-FG
                                PD-PROCEEDS-FG
                    MOVE "C" TO PD-BALLOON-FG
                 ELSE
                    PERFORM ICE-OR-A-MSG
                    GO TO F-171.
           IF NEW-FLAG NOT = 1
      * CITIZENS TEST
              MOVE 800 TO CIP-NO-OF-TIMES-CALLED
      *       MOVE 0 TO CIP-NO-OF-TIMES-CALLED
              MOVE PD-LASTPYAMT TO CIP-BALLOON
              MOVE PD-BALLOON-FG TO CIP-BALLOON-SW
              MOVE PD-PROCEEDS-FG TO CIP-PROCEEDS-SW
              MOVE PD-REGPYAMT-FG TO CIP-REGPYAMT-SW
              PERFORM CALL-LOAN-CALCULATIONS.

           GO TO CREATE-ROUTINE.
       F-18.
           IF (GP-ENTER-LPO-FG = "Y")
              IF NEW-FLAG = 1
                 MOVE PD-LPO TO WINDOW-LPO
                 MOVE PD-REP-CD TO WINDOW-REP
                 MOVE PD-PROCESS-CD TO WINDOW-PROCESS
                 MOVE "WI/TFCCDS" TO FORM-NAM
                 CALL FORM-PROGX USING FORM-PATH WINDOW-WORKERS
                 CANCEL FORM-PROGX
                 MOVE WINDOW-LPO TO PD-LPO
                 MOVE WINDOW-REP TO PD-REP-CD
                 MOVE WINDOW-PROCESS TO PD-PROCESS-CD.
           MOVE PD-INSCODES TO INSCODES-HLDR.
           MOVE " " TO CHFG(27) CHFG(28) CHFG(29)
                       CHFG(30) CHFG(31) CHFG(32).
           MOVE VDUBUF TO PD-INSCODES.

           IF SP-INSURANCE-WRITTEN = "N"
               IF PD-INSURED NOT = "N"
                  IF PD-INSTYPES NOT = "N"
                      MOVE "INSURANCE NOT ALLOWED PER SPR"
                                                TO MESS
                      PERFORM SEND-MESS
                      GO TO ENTER-ELE.

           IF PD-INSURED NOT = "M"
              IF PD-INSURED NOT = "J"
                 IF PD-INSURED NOT = "N"
                    IF PD-INSURED NOT = "S"
                       MOVE "ENTER 'N','M','S',OR 'J'" TO MESS
                       PERFORM SEND-MESS
                       MOVE INSCODES-HLDR TO PD-INSCODES
                       GO TO ENTER-ELE.
           IF NOT PD-INSTYPES-VALID
              MOVE INSCODES-HLDR TO PD-INSCODES
              GO TO ENTER-ELE.
           IF NOT PD-INSELIGIBLE-VALID
              MOVE INSCODES-HLDR TO PD-INSCODES
              GO TO ENTER-ELE.
      * ADDED TO PREVENT ENTRY OF MNN, ETC.
           IF PD-INSTYPES = "N"
              IF PD-INSURED NOT = "N"
                 MOVE INSCODES-HLDR TO PD-INSCODES
                 GO TO ENTER-ELE.
           PERFORM VALID-INS-TYPES-ELIG.
           IF IO-FG = 9
              MOVE INSCODES-HLDR TO PD-INSCODES
              GO TO ENTER-ELE.

           IF CHFG(27) = "N"
              MOVE 0 TO PD-INSCOMP(1) PD-INSTRM(1) PD-INSCOVR(1)
                        PD-INSPREM(1) PD-INSCOMM(1)
              MOVE " " TO PD-INSOURS(1)
              MOVE "270 271 272 273 274 275." TO MESS
              PERFORM RESET-MESS.
           IF CHFG(28) = "N"
              MOVE 0 TO PD-INSCOMP(2) PD-INSTRM(2) PD-INSCOVR(2)
                        PD-INSPREM(2) PD-INSCOMM(2)
              MOVE " " TO PD-INSOURS(2)
              MOVE "280 281 282 283 284 285." TO MESS
              PERFORM RESET-MESS.
           IF CHFG(29) = "N"
              MOVE 0 TO PD-INSCOMP(3) PD-INSTRM(3) PD-INSCOVR(3)
                        PD-INSPREM(3) PD-INSCOMM(3)
              MOVE " " TO PD-INSOURS(3)
              MOVE "290 291 292 293 294 295." TO MESS
              PERFORM RESET-MESS.
           IF CHFG(30) = "N"
              MOVE 0 TO PD-INSCOMP(4) PD-INSTRM(4) PD-INSCOVR(4)
                        PD-INSPREM(4) PD-INSCOMM(4)
              MOVE " " TO PD-INSOURS(4)
              MOVE "300 301 302 303 304 305." TO MESS
              PERFORM RESET-MESS.
           IF CHFG(31) = "N"
              MOVE 0 TO PD-INSCOMP(5) PD-INSTRM(5) PD-INSCOVR(5)
                        PD-INSPREM(5) PD-INSCOMM(5) PD-INSEXP-DATE(5)
              MOVE " " TO PD-INSOURS(5)
              MOVE "310 311 312 313 314 315." TO MESS
              PERFORM RESET-MESS.
           IF CHFG(32) = "N"
              MOVE 0 TO PD-INSCOMP(6) PD-INSTRM(6) PD-INSCOVR(6)
                        PD-INSPREM(6) PD-INSCOMM(6) PD-INSEXP-DATE(6)
              MOVE " " TO PD-INSOURS(6)
              MOVE "320 321 322 323 324 325." TO MESS
              PERFORM RESET-MESS.
           IF NOT SP-SCFRMLA-POINTS
              IF NEW-FLAG = 1
                 MOVE "I" TO PD-POINTS-FG
                 GO TO CREATE-ROUTINE
              ELSE
                 GO TO CONT2-F-18.
       CONT1-F-18.
           MOVE "^-BACK, ENTER NO OF POINTS (99.99):" TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "ENNN022181." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO CONT1-F-18.
           MOVE VDUNUM2 TO PD-POINTS.
           PERFORM SP-EDT-TABLE-LOOKUP.
           IF IO-FG = 9
              MOVE 0 TO IO-FG
              GO TO CONT1-F-18.
           IF NEW-FLAG = 1
           IF PD-POINTS = 0
              MOVE "S  A050181." TO VDU
              PERFORM SCREENIO
           IF NEW-FLAG = 1
              MOVE "I" TO PD-POINTS-FG
              GO TO CREATE-ROUTINE.
       CONT2-F-18.
           MOVE "S  A010182." TO VDU.
           MOVE "I" TO VDUBUF PD-POINTS-FG.
           PERFORM SCREENIO.

           MOVE "E  A010182." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "1U"
              IF SP-SCFRMLA-POINTS
                 GO TO CONT1-F-18.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO CONT2-F-18.
           MOVE VDUBUF TO PD-POINTS-FG.
           IF NOT (PD-POINTS-FG = "I" OR "C")
              PERFORM ICE-OR-A-MSG
              GO TO CONT2-F-18.
           IF NEW-FLAG NOT = 1
              IF PD-INSCODES NOT = INSCODES-HLDR
                 MOVE 0 TO CIP-NO-OF-TIMES-CALLED
              ELSE
                 MOVE 800 TO CIP-NO-OF-TIMES-CALLED
              END-IF
              MOVE PD-INSURED TO CIP-INS-WHO
              MOVE PD-INSTYPES TO CIP-INS-TYPE
              MOVE PD-INSELIGIBLE TO CIP-INS-ELIG
              MOVE PD-POINTS TO CIP-POINTS
              MOVE PD-POINTS-FG TO CIP-POINTS-SW
              PERFORM CALL-LOAN-CALCULATIONS.
           GO TO CREATE-ROUTINE.
       F-19.
           GO TO CREATE-ROUTINE.
       F-20.
           MOVE PD-FEEAMT(1) TO AMT-HOLDER.
           MOVE VDUNUM2 TO PD-FEEAMT(1).
           PERFORM VALIDATE-FIELDS-AGAINST-MAX.
           IF IO-FG = 9
              MOVE 0 TO IO-FG
              GO TO ENTER-ELE.
           IF PD-FEEAMT(1) = AMT-HOLDER
              GO TO CREATE-ROUTINE.
       CONT-F-20.
           MOVE "S  A010201." TO VDU.
           MOVE "I" TO VDUBUF PD-FEECD-FG(1).
           PERFORM SCREENIO.

           MOVE "E  A010201." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO CONT-F-20.
           MOVE VDUBUF TO PD-FEECD-FG(1).
           IF PD-FEECD-FG(1) NOT = "I"
              IF PD-FEECD-FG(1) NOT = "C"
                 PERFORM ICE-OR-A-MSG
                 GO TO CONT-F-20.
           IF NEW-FLAG NOT = 1
              MOVE PD-FEEAMT(1) TO CIP-FEE-CHG(1)
              MOVE PD-FEECD-FG(1) TO CIP-FEE-CHG-SW(1)
              PERFORM CALL-LOAN-CALCULATIONS.
           GO TO CREATE-ROUTINE.
       F-21.
           MOVE PD-FEEAMT(2) TO AMT-HOLDER.
           MOVE VDUNUM2 TO PD-FEEAMT(2).
           PERFORM VALIDATE-FIELDS-AGAINST-MAX.
           IF IO-FG = 9
              MOVE 0 TO IO-FG
              GO TO ENTER-ELE.
           IF PD-FEEAMT(2) = AMT-HOLDER
              GO TO CREATE-ROUTINE.
       CONT-F-21.
           MOVE "S  A010211." TO VDU.
           MOVE "I" TO VDUBUF PD-FEECD-FG(2).
           PERFORM SCREENIO.

           MOVE "E  A010211." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO CONT-F-21.
           MOVE VDUBUF TO PD-FEECD-FG(2).
           IF PD-FEECD-FG(2) NOT = "I"
              IF PD-FEECD-FG(2) NOT = "C"
                 PERFORM ICE-OR-A-MSG
                 GO TO CONT-F-21.
           IF NEW-FLAG NOT = 1
              MOVE PD-FEEAMT(2) TO CIP-FEE-CHG(2)
              MOVE PD-FEECD-FG(2) TO CIP-FEE-CHG-SW(2)
              PERFORM CALL-LOAN-CALCULATIONS.
           GO TO CREATE-ROUTINE.
       F-22.
           MOVE VDUNUM2 TO PD-DLNO
                           DL-DLNO.
           PERFORM GET-DL1.
           IF IO-FG = 9
              GO TO ENTER-ELE.
           IF DL-TERMDATE NOT = 0
              IF DL-TERMDATE NOT > PD-LOANDATE
                 MOVE "DEALER HAS BEEN TERMINATED" TO MESS
                 PERFORM SEND-MESS
                 GO TO ENTER-ELE.
           MOVE "SIGN:" TO INSPECT-W.
           MOVE DL-SIGN1 TO ADD-ON.
           PERFORM ADD-IT2.
           MOVE DL-SIGN2 TO ADD-ON.
           PERFORM ADD-IT2.
           MOVE DL-SIGN3 TO ADD-ON.
           PERFORM ADD-IT.
           MOVE INSPECT-W TO MESS.
           PERFORM CALL-TMMSG.
           GO TO CREATE-ROUTINE.
       F-23.
           MOVE VDUBUF TO PD-COLLID.
           IF PD-COLLID = " "
              MOVE SPACES TO WK-HHGS-1
              MOVE "230 231 232." TO MESS
              PERFORM RESET-MESS
              GO TO CREATE-ROUTINE.
       CONT1-F-23.
           MOVE "E  A200231." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO CONT1-F-23.
           MOVE VDUBUF TO SCREEN-LINE1 WK-HHGS-1.
           IF SCREEN-LINE1 = " "
              MOVE "231 232." TO MESS
              PERFORM RESET-MESS
              GO TO CREATE-ROUTINE.
       CONT2-F-23.
           MOVE "E  A200232." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO CONT1-F-23.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO CONT2-F-23.
           MOVE VDUBUF TO SCREEN-LINE2.
           IF SCREEN-LINE2 = " "
              MOVE "232." TO MESS
              PERFORM RESET-MESS
              GO TO F-23-EXIT.
           STRING SCREEN-LINE1,SCREEN-LINE2
                  DELIMITED BY "   " INTO WK-HHGS-1.
       F-23-EXIT.
           MOVE MSG-LINE1 TO PD-MSG1.
           MOVE MSG-LINE2 TO PD-MSG2.
           GO TO CREATE-ROUTINE.
       F-24.
       F-25.
           GO TO CREATE-ROUTINE.
       F-26.
           MOVE VDUNUM3 TO PD-OVRDRATE.
       CONT-F-26.
           MOVE "E  A010261." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO CONT-F-26.
           MOVE VDUBUF TO PD-OVRDRATE-FG.

      *<1225J>
           IF SP-CAL-FLAT-MONTHLY(1)
              IF PD-OVRDRATE-FG = "E"
                 MOVE "EFFECTIVE RATE OVERRIDE NOT ALLOWED" TO MESS
                 PERFORM SEND-MESS
                 GO TO CONT-F-26.
           IF (NOT (PD-OVRDRATE-FG = "C" OR "E" OR "A"))
              PERFORM ICE-OR-A-MSG
              GO TO CONT-F-26.

           PERFORM VALIDATE-FIELDS-AGAINST-MAX.
           IF IO-FG = 9
              MOVE 0 TO IO-FG
              GO TO ENTER-ELE.
           IF NEW-FLAG NOT = 1
              MOVE PD-OVRDRATE TO CIP-OVR-RATE
              MOVE PD-OVRDRATE-FG TO CIP-OVR-RATE-SW
              MOVE 800 TO CIP-NO-OF-TIMES-CALLED
              PERFORM CALL-LOAN-CALCULATIONS.
           GO TO CREATE-ROUTINE.
       F-27.
           PERFORM INSURANCE-ENTRY.
           IF IO-FG = 9
              MOVE 0 TO IO-FG
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       F-28.
           PERFORM INSURANCE-ENTRY.
           IF IO-FG = 9
              MOVE 0 TO IO-FG
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       F-29.
           PERFORM INSURANCE-ENTRY.
           IF IO-FG = 9
              MOVE 0 TO IO-FG
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       F-30.
           PERFORM INSURANCE-ENTRY.
           IF IO-FG = 9
              MOVE 0 TO IO-FG
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       F-31.
           PERFORM INSURANCE-ENTRY.
           IF IO-FG = 9
              MOVE 0 TO IO-FG
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       F-32.
           PERFORM INSURANCE-ENTRY.
           IF IO-FG = 9
              MOVE 0 TO IO-FG
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       F-33.
           GO TO CREATE-ROUTINE.
       F-34.
           MOVE PD-SERCHG TO AMT-HOLDER.
           MOVE VDUNUM2 TO PD-SERCHG
                           PD-SUM-SERCHG.
           PERFORM VALIDATE-FIELDS-AGAINST-MAX.
           IF IO-FG = 9
              MOVE 0 TO IO-FG
              GO TO ENTER-ELE.
           IF PD-SERCHG = AMT-HOLDER
              GO TO CREATE-ROUTINE.
       CONT-F-34.
           MOVE "S  A010341." TO VDU.
           MOVE "I" TO VDUBUF PD-SERCHG-FG.
           PERFORM SCREENIO.

           MOVE "E  A010341." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO CONT-F-34.
           MOVE VDUBUF TO PD-SERCHG-FG.
           IF PD-SERCHG-FG NOT = "I"
              IF PD-SERCHG-FG NOT = "C"
                 PERFORM ICE-OR-A-MSG
                 GO TO CONT-F-34.
           IF NEW-FLAG NOT = 1
      *       --> NOTE: SCFRMLA 33 AND 39 HAVE 2 COMPONENTS, MUST USE F3
      *                 TO INDICATE DISTRIBUTION, RE: WI/SCHG33.C
              IF (PD-SERCHG-FG = "I") AND
                 (SP-SCFRMLA = 33 OR 39   )
                 MOVE "MUST USE F3 FOR DISTRIBUTION WINDOW" TO MESS
                 PERFORM SEND-MESS
                 MOVE "SNNN062340." TO VDU
                 MOVE AMT-HOLDER    TO PD-SERCHG
                                       VDUNUM2
                 PERFORM SCREENIO
                 GO TO ENTER-ELE
              ELSE
                 MOVE PD-SERCHG TO CIP-SERVICE-CHG
                 MOVE PD-SERCHG-FG TO CIP-SERVICE-CHG-SW
                 PERFORM CALL-LOAN-CALCULATIONS.
           GO TO CREATE-ROUTINE.
       F-35.
           MOVE VDUNUM2 TO SAVE-MAINTFEE.
           DIVIDE VDUNUM2 BY PD-ORGTERM GIVING PD-MAINTFEE ROUNDED.
           MOVE PD-MAINTFEE TO PD-SUM-MAINTFEE.
           PERFORM VALIDATE-FIELDS-AGAINST-MAX.
           IF IO-FG = 9
              MOVE 0 TO IO-FG
              GO TO ENTER-ELE.
           IF VDUNUM2 = MAINTFEE-HLDR
              GO TO CREATE-ROUTINE.
       CONT-F-35.
           MOVE "S  A010351." TO VDU.
           MOVE "I" TO VDUBUF PD-MAINTFEE-FG.
           PERFORM SCREENIO.

           MOVE "E  A010351." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO CONT-F-35.
           MOVE VDUBUF TO PD-MAINTFEE-FG.
           IF PD-MAINTFEE-FG NOT = "I"
              IF PD-MAINTFEE-FG NOT = "C"
                 PERFORM ICE-OR-A-MSG
                 GO TO CONT-F-35.
           IF NEW-FLAG NOT = 1
              IF SP-MFFRMLA-05
                 MOVE SAVE-MAINTFEE TO CIP-MAINT-FEES
              ELSE
                 MULTIPLY PD-MAINTFEE BY PD-ORGTERM GIVING
                    CIP-MAINT-FEES ROUNDED.
              MOVE PD-MAINTFEE-FG TO CIP-MAINT-FEES-SW
              PERFORM CALL-LOAN-CALCULATIONS.
           GO TO CREATE-ROUTINE.
       F-36.
           MOVE PD-EXTCHG TO AMT-HOLDER.
           MOVE VDUNUM2 TO PD-EXTCHG
                           PD-SUM-EXTCHG.
           PERFORM VALIDATE-FIELDS-AGAINST-MAX.
           IF IO-FG = 9
              MOVE 0 TO IO-FG
              GO TO ENTER-ELE.
           IF PD-EXTCHG = AMT-HOLDER
              GO TO CREATE-ROUTINE.
       CONT-F-36.
           MOVE "S  A010361." TO VDU.
           MOVE "I" TO VDUBUF PD-EXTCHG-FG.
           PERFORM SCREENIO.

           MOVE "E  A010361." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO CONT-F-36.
           MOVE VDUBUF TO PD-EXTCHG-FG.
           IF PD-EXTCHG-FG NOT = "I"
              IF PD-EXTCHG-FG NOT = "C"
                 PERFORM ICE-OR-A-MSG
                 GO TO CONT-F-36.
           IF NEW-FLAG NOT = 1
              MOVE PD-EXTCHG TO CIP-1ST-PY-EXT
              MOVE PD-EXTCHG-FG TO CIP-1ST-PY-EXT-SW
              PERFORM CALL-LOAN-CALCULATIONS.
           GO TO CREATE-ROUTINE.
       F-37.
           MOVE VDUNUM2 TO PD-INTCHG
                           PD-SUM-INTCHG.
           GO TO CREATE-ROUTINE.
       F-38.
           MOVE VDUNUM2 TO PD-FINAMT.
           GO TO CREATE-ROUTINE.
       F-39.
           GO TO CREATE-ROUTINE.
       F-40.
           GO TO CREATE-ROUTINE.
       F-41.
           GO TO CREATE-ROUTINE.
       F-42.
           MOVE PD-REGPYAMT TO AMT-HOLDER.
           IF VDUNUM2 = AMT-HOLDER AND COP-TERM = 1
              GO TO CREATE-ROUTINE.
           MOVE VDUNUM2     TO PD-REGPYAMT
                               PD-SUM-REGPYAMT.
           IF VDUNUM2 = 0
              GO TO ENTER-ELE.
       F-421.
           MOVE "S  A010421." TO VDU.
           MOVE "I" TO VDUBUF
           PERFORM SCREENIO.

           MOVE "E  A010421." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO F-421.
           IF NOT (VDUBUF = PD-REGPYAMT-FG)
              IF VDUBUF = "I"
                 PERFORM SETUP-BLINPC-BUF
                 MOVE " " TO BLINPC-REGPYAMT-FG
                 PERFORM BLINPC-CALC-WINDOW
                 IF BLINPC-STATUS NOT = "00"
                    GO TO F-421
                 ELSE
                    PERFORM AFTER-REGPY-CALC-OPT
              ELSE
                 IF VDUBUF = "C"
                    MOVE "I" TO PD-PROCEEDS-FG
                                PD-BALLOON-FG
                    MOVE "C" TO PD-REGPYAMT-FG
                 ELSE
                    PERFORM ICE-OR-A-MSG
                    GO TO F-421.

      * CITIZENS TEST
           MOVE 800 TO CIP-NO-OF-TIMES-CALLED.
      *    MOVE 0 TO CIP-NO-OF-TIMES-CALLED.
           MOVE PD-REGPYAMT TO CIP-REGPYAMT.
           MOVE PD-REGPYAMT-FG TO CIP-REGPYAMT-SW.
           MOVE PD-PROCEEDS-FG TO CIP-PROCEEDS-SW.
           MOVE PD-BALLOON-FG TO CIP-BALLOON-SW.
           PERFORM CALL-LOAN-CALCULATIONS.
           GO TO CREATE-ROUTINE.

       END-ENTRY-MODULE.
           EXIT.

      *****************************************************
       SET-UNITPER-INFO SECTION.
      *****************************************************
           IF PD-ORGTERM > 1
              IF PD-UNITPER-CD = "D"
                 MOVE "M" TO PD-UNITPER-CD
                 MOVE 1 TO PD-UNITPER-FREQ
                 MOVE "NOTE: ------> SWITCHING BACK TO MONTHLY PAPER!!"
                                                          TO MESS
                 PERFORM SEND-MESS.

           MOVE PD-UNITPER-CD TO UPER-UNITPER-CD.
           MOVE PD-UNITPER-FREQ TO UPER-UNITPER-FREQ.
           MOVE PD-INTDATE TO UPER-INTDATE.
           MOVE PD-ORIG-1STPYDATE TO UPER-1STPYDATE.
           MOVE PD-ORGTERM TO UPER-TERM.
           PERFORM UPER-CALCULATION.

           MOVE UPER-UNITPER-PER-YEAR TO PD-UNITPER-PER-YEAR.
           MOVE UPER-DAYS-IN-UNITPER TO PD-DAYS-IN-UNITPER.
           IF PD-ORGTERM = 1
              MOVE UPER-SINGLE-PAY-CD TO PD-UNITPER-CD
              MOVE UPER-SINGLE-PAY-FREQ TO PD-UNITPER-FREQ.

           MOVE "S  A010100." TO VDU.
           MOVE PD-UNITPER-CD TO VDUBUF.
           PERFORM SCREENIO.
           MOVE "SN N030101." TO VDU.
           MOVE PD-UNITPER-FREQ TO VDUNUM0.
           PERFORM SCREENIO.

           IF PD-ORGTERM = 1
              MOVE "DEFAULTS FOR SINGLE PAY LOANS SET!!" TO MESS
              PERFORM SEND-MESS.

       SET-UNITPER-INFO-EXIT.
           EXIT.

      *****************************************************
       SUBCLASS-SCAN SECTION.
      *****************************************************
           MOVE SPACES              TO SCSCAN-SP1-KEY.
           MOVE PD-ORGST            TO SCSCAN-ORGST.
           MOVE PD-CLASS            TO SCSCAN-SPRCLASS.
           MOVE 0                   TO SCSCAN-SUBCLASS
                                       SCSCAN-LAWCODE.
           MOVE "Y"                 TO SCSCAN-LAST-LAW.
           MOVE TRANS-DATE-YYYYMMDD TO SCSCAN-TRANS-DATE.
           MOVE "WI/SCSCAN" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH SCSCAN-BUF EXIT-PATHNAME.
           CANCEL FORM-PROGX.
           IF SCSCAN-STATUS NOT = "00" OR SCSCAN-SP1-KEY = " "
              MOVE 9 TO IO-FG
              GO TO EXIT-SUBCLASS-SCAN
           ELSE
              MOVE 0 TO IO-FG.
           MOVE "SNNN020052." TO VDU.
           MOVE SCSCAN-SP-SUBCLASS TO VDUNUM0.
           PERFORM SCREENIO.
           MOVE SCSCAN-SP-SUBCLASS TO VDUNUM0.

       EXIT-SUBCLASS-SCAN.
           EXIT.

      *****************************************************
       BORROWER-ENTRY SECTION.
      *****************************************************

      * SET BORR-CHG-FG FOR GEORGIA SERVICE CHARGE CALC:
           IF NEW-FLAG NOT = 1
              MOVE "Y" TO BORR-CHG-FG.
           MOVE 0 TO IO-FG.
           MOVE " " TO IO-TYPE.
           MOVE VDUBUF TO ALP-BW.
           MOVE ALP-BW123 TO NUM-BW123.
           MOVE ALP-BW45 TO NUM-BW45.
           MOVE ALP-BW6789 TO NUM-BW6789.
           IF NUM-BW NOT NUMERIC
              MOVE 0 TO NUM-BW.
           MOVE NUM-BW TO PD-SSNO(ELE) BW-SSNO.

           IF ELE = 1
              IF LOAN-QUOTE-FG = "L"
                 IF NUM-BW = 0
                    MOVE 9 TO IO-FG
                    GO TO EXIT-ENTER-BORROWER
                 ELSE
                    GO TO GET-BORROWER.
           IF ELE = 1
              IF LOAN-QUOTE-FG = "Q"
                 IF NUM-BW NOT = 0
                    MOVE 9 TO IO-FG
                    GO TO EXIT-ENTER-BORROWER
                 ELSE
                    PERFORM BORROWER-QUOTED
                    IF IO-FG NOT = 9
                       MOVE "Q" TO QUOTE-ID-FG PD2-ID
                       MOVE EXT-CONAME-USER-LOC TO PD2-BRANCH
                       GO TO EXIT-ENTER-BORROWER
                    ELSE
                       MOVE "CLR" TO IO-TYPE
                       GO TO EXIT-ENTER-BORROWER.

           IF NUM-BW NOT = 0
              GO TO GET-BORROWER.
           MOVE 0 TO BW-SSNO.
           MOVE " " TO INSPECT-W.
       NEXT-BW-CLEAR.
           MOVE 0 TO PD-SSNO(ELE).
           MOVE " " TO PD-MAKERCD(ELE).
           PERFORM SEND-BW-NO-NAME.
           PERFORM SEND-BW-MAKERCD.
           IF ELE < 4
              ADD 1 TO ELE
              GO TO NEXT-BW-CLEAR.
           PERFORM BW-CHECK-ENDORSER.
           MOVE 0 TO IO-FG.
           GO TO EXIT-ENTER-BORROWER.
       GET-BORROWER.
           PERFORM BW-TABLE-CHECK.
           IF IO-FG = 9
              GO TO EXIT-ENTER-BORROWER.
           PERFORM GET-BW1.
           PERFORM SEND-BW-NAME.
           IF IO-FG = 9
              GO TO EXIT-ENTER-BORROWER.
           MOVE " " TO CHFG(2) CHFG(3) CHFG(4) CHFG(6)
      *                CHFG(8) CHFG(9) CHFG(13).
      *                CHFG(8) CHFG(13).
                       CHFG(8).
           IF ELE = 1 AND QUOTE-ONLY
              MOVE "L" TO QUOTE-ID-FG.
           IF ELE = 1
              MOVE "M" TO PD-MAKERCD(1)
              PERFORM SEND-BW-MAKERCD
              MOVE BW-CONFIDENTIAL TO PD-CONFIDENTCD
              MOVE BW-ORIGIN TO PD-ORIGIN
              GO TO EXIT-ENTER-BORROWER.
       ENTER-MAKER-CODE.
           MOVE "E  A010  2." TO VDU.
           MOVE ELE TO VDUROW.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              MOVE 9 TO IO-FG
              GO TO EXIT-ENTER-BORROWER.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO ENTER-MAKER-CODE.
           IF VDUBUF NOT = "J"
              IF VDUBUF NOT = "E"
                 MOVE "(M-MAKER),J-JOINT,E-ENDORSER" TO MESS
                 PERFORM SEND-MESS
                 GO TO ENTER-MAKER-CODE.
           MOVE VDUBUF TO PD-MAKERCD(ELE).
       EXIT-ENTER-BORROWER.
           EXIT.

      *****************************************************
       BW-TABLE-CHECK SECTION.
      *****************************************************
           IF ELE NOT = 1
              SUBTRACT 1 FROM ELE GIVING I-LEVEL
              IF PD-SSNO(I-LEVEL) = 0
                 MOVE 9 TO IO-FG
                 MOVE "ENTER SERIALLY" TO MESS
                 PERFORM SEND-MESS
                 GO TO EXIT-BW-TABLE-CHECK.
           MOVE 4 TO I-LEVEL.
       NEXT-BW-LEVEL-CHECK.
           IF PD-SSNO(I-LEVEL) = PD-SSNO(ELE) AND I-LEVEL NOT = ELE
              MOVE 9 TO IO-FG
              MOVE "NO DUPLICATES" TO MESS
              PERFORM SEND-MESS
              GO TO EXIT-BW-TABLE-CHECK.
           SUBTRACT 1 FROM I-LEVEL.
           IF I-LEVEL > 0
              GO TO NEXT-BW-LEVEL-CHECK.
       EXIT-BW-TABLE-CHECK.
           EXIT.


      ********************************
       BWLN-SCAN SECTION.
      ********************************
           MOVE TRANS-DATE-YYYYMMDD TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-ALPDATE.
           MOVE ALP-DATE TO BWLNSC-SYSDATE.
           MOVE 0        TO BWLNSC-ACCTNO.
           MOVE "WI/BWLNSC " TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH BWLNSC-BUF EXIT-PATHNAME.
           CANCEL FORM-PROGX.
           IF BWLNSC-STATUS NOT = "00" OR BWLNSC-ACCTNO = 0
              MOVE 9 TO IO-FG
              GO TO EXIT-BWLN-SCAN.
           MOVE "SNNN060110." TO VDU.
           MOVE BWLNSC-ACCTNO TO VDUNUM0.
           PERFORM SCREENIO.

           MOVE BWLNSC-ACCTNO TO VDUNUM0.
           MOVE 0 TO IO-FG.
       EXIT-BWLN-SCAN.
           EXIT.

      ******************************************************************
       FEECODE-SCAN SECTION.
      ******************************************************************
           MOVE "GC"                TO CDBSCN-TYPE.
           MOVE EXT-CONAME-USER-LOC TO CDBSCN-BRANCH.
           MOVE SPACES              TO CDBSCN-CODE.

           MOVE "WI/CDBSCN" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH CDBSCN-BUF EXIT-PATHNAME.
           CANCEL FORM-PROGX.
           IF ( CDBSCN-STATUS    NOT = "00"    ) OR
              ((CDBSCN-TYPE = "GC") AND (CDBSCN-CODE = SPACES))
              MOVE 9 TO IO-FG
              GO TO EXIT-FEECODE-SCAN.

      * PERFORMED FROM FEEENT WINDOW:
           MOVE "S  A020  0." TO VDU.
           MOVE FW-ELE      TO VDUROW.
           MOVE CDBSCN-CODE TO VDUBUF.
           PERFORM SCREENIO.

           MOVE CDBSCN-CODE TO VDUBUF.

           MOVE 0 TO IO-FG.
       EXIT-FEECODE-SCAN.
           EXIT.


      ******************************************************************
       CODE-SCAN SECTION.
      ******************************************************************

           MOVE SPACES TO CDSCAN-CODE.

           IF ELE = 7
              MOVE "PU"   TO CDSCAN-TYPE
           ELSE
           IF ELE = 8
              MOVE "CO"   TO CDSCAN-TYPE
           ELSE
           IF ELE = 9
              MOVE "SR"   TO CDSCAN-TYPE
           ELSE
              PERFORM FEECODE-SCAN
              GO TO EXIT-CODE-SCAN.

           MOVE "WI/CDSCAN" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH CDSCAN-BUF EXIT-PATHNAME.
           CANCEL FORM-PROGX.
           IF ( CDSCAN-STATUS    NOT = "00"    ) OR
              (( CDSCAN-TYPE = "PU" ) AND ( CDSCAN-CODE = SPACES )) OR
              (( CDSCAN-TYPE = "CO" ) AND ( CDSCAN-CODE = SPACES )) OR
              (( CDSCAN-TYPE = "SR" ) AND ( CDSCAN-CODE = SPACES ))
              MOVE 9 TO IO-FG
              GO TO EXIT-CODE-SCAN.

           MOVE "S  A020  0." TO VDU.
           MOVE ELE TO VDUROW.
           MOVE CDSCAN-CODE TO VDUBUF.
           PERFORM SCREENIO.

           MOVE CDSCAN-CODE TO VDUBUF.
           MOVE 0 TO IO-FG.

       EXIT-CODE-SCAN.
           EXIT.

      ********************************************
       QUOTE-SCAN SECTION.
      ********************************************
           MOVE "Q"                 TO PDSCAN-ID.
           MOVE EXT-CONAME-USER-LOC TO PDSCAN-BRANCH.
           MOVE SPACES              TO PDSCAN-VERIFY.
           MOVE "N"                 TO PDSCAN-SALEFIN.
           MOVE "Y"                 TO PDSCAN-BALLOON.
           MOVE  SPACES             TO PDSCAN-BWNAME.
           MOVE "WI/PDSCAN" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH PDSCAN-BUF EXIT-PATHNAME.
           CANCEL FORM-PROGX.
           IF PDSCAN-STATUS NOT = "00" OR PDSCAN-BWNAME = SPACES
               MOVE 9 TO IO-FG
            ELSE
               MOVE 0 TO IO-FG.

       EXIT-QUOTE-SCAN.
           EXIT.

      ***************************************************
       CAN-LOAN-SCAN SECTION.
      ***************************************************
           PERFORM SET-CLFILE-INDEX-BY.

           MOVE "Y" TO CLSCAN-NON-MONTHLY.
           MOVE 1   TO CLSCAN-SCAN-TYPE.
           MOVE "WI/CLSCAN" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH CLSCAN-BUF EXIT-PATHNAME.
           CANCEL FORM-PROGX.
           IF CLSCAN-STATUS NOT = "00" OR CLSCAN-CAN-NO = " "
               MOVE 9 TO IO-FG
            ELSE
               MOVE 0 TO IO-FG.

       EXIT-CAN-LOAN-SCAN.
           EXIT.


      ***************************************************
       CLASS-SCAN SECTION.
      ***************************************************
           MOVE 0      TO LCSCAN-KEY.
           MOVE SPACES TO LCSCAN-VERIFY.
           MOVE "N"    TO LCSCAN-SALEFIN.
           MOVE "WI/LCSCAN" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH LCSCAN-BUF EXIT-PATHNAME.
           CANCEL FORM-PROGX.
           IF LCSCAN-STATUS NOT = "00" OR LCSCAN-KEY = 0
               MOVE 9 TO IO-FG
               GO TO EXIT-CLASS-SCAN.
           MOVE "SN N020LNKEY." TO VDU.
           MOVE LCSCAN-KEY TO VDUNUM0.
           PERFORM SCREENIO.
           MOVE LCSCAN-KEY TO VDUNUM0.
           MOVE 0 TO IO-FG.

       EXIT-CLASS-SCAN.
           EXIT.

      ***************************************************
       DEALER-SCAN SECTION.
      ***************************************************
           MOVE 0 TO DLSCAN-NUM.
           MOVE "WI/DLSCAN " TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH DLSCAN-BUF EXIT-PATHNAME.
           CANCEL FORM-PROGX.
           IF DLSCAN-STATUS NOT = "00" OR DLSCAN-NUM = " "
               MOVE 9 TO IO-FG
               GO TO EXIT-DEALER-SCAN.
           MOVE "SN N042220." TO VDU.
           MOVE DLSCAN-DLNO TO VDUNUM2.
           PERFORM SCREENIO.
           MOVE DLSCAN-DLNO TO VDUNUM2.
           MOVE 0 TO IO-FG.

       EXIT-DEALER-SCAN.
           EXIT.

      ***************************************************
       VERIFY-PASSWORD SECTION.
      ***************************************************
           MOVE " " TO PASSWDW-STAT.
           MOVE "XONPC0" TO PASSWDW-PGM.
           MOVE "PLEASE ENTER THE PASSWORD," TO PASSWDW-PROMPT1.
           MOVE "CL/PASSWD" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH PASSWDW-BUF.
           CANCEL FORM-PROGX.

      ***************************************************
       BORROWER-QUOTED SECTION.
      ***************************************************
       BORROWER-QUOTED-ENTER.
           MOVE "F1-CLEAR, ENTER THE QUOTE'S ID:" TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "E  A330011." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10"
              MOVE 9 TO IO-FG
              GO TO EXIT-BORROWER-QUOTED.
           IF VDUSTATUS NOT = "00"
                     OR VDUBUF = " "
              GO TO BORROWER-QUOTED-ENTER.

           MOVE VDUBUF TO PD2-BWNAME SAVE-BW-NAME.

           IF NEW-FLAG = 1 OR PD2-BWNAME NOT = HOLD-BWNAME
              NEXT SENTENCE
           ELSE
              MOVE SAVE-QUOTE-SSN TO PD-SOC-SEC PD-SSNO(1)
              GO TO BORROWER-QUOTED-CONT.

      * BUILD-QUOTE-SSN:
           MOVE PD2-BWNAME TO HASH-BUF.
           MOVE 33 TO HASH-SIZE.
           PERFORM HASH-ROUTINE.
           MOVE HASH-TOTAL TO PD-SSNO(1).

      * TEST FOR DUPLICATES:
           MOVE PD-RECORD TO HOLD-PD-RECORD.
           MOVE "Q" TO PD-ID.
           MOVE EXT-CONAME-USER-LOC TO PD-BRANCH.
           MOVE PD-SSNO(1) TO PD-SOC-SEC.
           PERFORM OPEN-PD1-FILE.
           PERFORM READ-PD1-FILE.
           PERFORM CLOSE-PD1-FILE.
           MOVE HOLD-PD-RECORD TO PD-RECORD.
           IF IO-FG = 0
              MOVE "A QUOTE ALREADY EXISTS UNDER THIS ID!" TO MESS
              PERFORM SEND-MESS
              GO TO BORROWER-QUOTED.

       BORROWER-QUOTED-CONT.
           MOVE 0 TO IO-FG.
           MOVE "M" TO PD-MAKERCD(1).
           PERFORM SEND-BW-MAKERCD.

           MOVE "N" TO CHFG(2) CHFG(3) CHFG(4) CHFG(6)
                       CHFG(7) CHFG(8) CHFG(10) CHFG(13).

       EXIT-BORROWER-QUOTED.
           EXIT.

      ************************************************
       DO-FEEENT-WINDOW SECTION.
      ************************************************
           MOVE SP-FEEFRMLA(1) TO FW-PROTECT-FEE1-FG.
           MOVE SP-FEEFRMLA(2) TO FW-PROTECT-FEE2-FG.
           PERFORM FEEENT-WIN.
           MOVE 0 TO COUNTER
                     TOTAL-GL-HLDR
                     TOTAL-GL-FINCHG-HLDR.
           PERFORM COMPUTE-GL-FEEAMT.
           PERFORM SP-EDT-TABLE-LOOKUP.
           IF IO-FG = 9
              MOVE 0 TO IO-FG
              GO TO DO-FEEENT-WINDOW.
           PERFORM SETUP-FEES-FOR-DISPLAY.
           MOVE D-190 TO WR-BUF.
           MOVE FEE-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.
           MOVE TOTAL-GL-HLDR TO CIP-GL-AMOUNT-TOTAL.
           MOVE TOTAL-GL-FINCHG-HLDR TO CIP-GL-AMOUNT-TOTAL-FINCHG.
           IF NEW-FLAG NOT = 1
              PERFORM CALL-LOAN-CALCULATIONS.

      ************************************************
       INSURANCE-ENTRY SECTION.
      ************************************************
           MOVE 0 TO IO-FG.
           SUBTRACT 26 FROM ELE GIVING I-LEVEL.
           MOVE VDUNUM0 TO PD-INSCOMP(I-LEVEL) IC-CODE.
           IF IC-CODE = 0
              IF I-LEVEL > 4
                 GO TO ENTER-IC-OURS
              ELSE
                 MOVE 9 TO IO-FG
                 GO TO EXIT-ENTER-INSURANCE.
           PERFORM READ-IC1-FILE.
           IF IO-FG = 9
              MOVE "INVALID COMPANY" TO MESS
              PERFORM SEND-MESS
              GO TO EXIT-ENTER-INSURANCE.
           IF I-LEVEL > 4
              IF PD-INSEXP-DATE(I-LEVEL) = 0
                 IF GP-OT-INS-TERM = "Y"
                    MOVE "I" TO PD-INS-FG(I-LEVEL)
                    MOVE "SNNN060  2." TO VDU
                    MOVE ELE TO VDUROW
                    MOVE PD-ORGTERM TO VDUNUM0 PD-INSEXP-DATE(I-LEVEL)
                    PERFORM SCREENIO
                 ELSE
                    MOVE PD-INTDATE TO NDTE-DATE
                    MOVE PD-ORGTERM TO NDTE-HOLD
                    PERFORM INCREMENT-MONTHS
                    MOVE "SNNN060  2." TO VDU
                    MOVE ELE TO VDUROW
                    MOVE NDTE-DATE TO DATE-YYYYMMDD
                                      PD-INSEXP-DATE(I-LEVEL)
                    PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
                    MOVE DATE-MMDDYY TO VDUNUM0
                    PERFORM SCREENIO.
       ENTER-IC-OURS.
           IF IC-CODE = 0
              MOVE " " TO PD-INSOURS(I-LEVEL)
              GO TO SEND-IC-OURS.

           IF ELE = 32
              IF GP-DEFAULT-O3-INSOURS NOT = " "
                 MOVE "S  A010321." TO VDU
                 MOVE GP-DEFAULT-O3-INSOURS TO VDUBUF
                 PERFORM SCREENIO.

           MOVE "SPACE = Y" TO MESS.
           PERFORM SEND-LEGEND.

           MOVE "R  A010  1." TO VDU.
           MOVE ELE TO VDUROW.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              MOVE 9 TO IO-FG
              GO TO EXIT-ENTER-INSURANCE.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO ENTER-IC-OURS.
           IF VDUBUF = " "
              MOVE "Y" TO VDUBUF.
           IF NOT (VDUBUF = "Y" OR "N" OR "M")
              MOVE "Y-YES, N-NO, M-MEMO" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-IC-OURS.
           IF GP-DEFAULT-O3-INSOURS NOT = " "
              IF ELE = 32
                 IF GP-DEFAULT-O3-INSOURS NOT = VDUBUF
                    MOVE " " TO MESS
                    STRING "INSURANCE OURS MUST BE '"
                               GP-DEFAULT-O3-INSOURS
                                "', PLEASE CHANGE!"
                           DELIMITED BY SIZE INTO MESS
                    PERFORM SEND-MESS
                    GO TO ENTER-IC-OURS.
           MOVE PD-INSOURS(I-LEVEL) TO INSOURS-HLDR.
           MOVE VDUBUF TO PD-INSOURS(I-LEVEL).
       SEND-IC-OURS.
           MOVE "S  A010  1." TO VDU.
           MOVE ELE TO VDUROW.
           MOVE PD-INSOURS(I-LEVEL) TO VDUBUF.
           PERFORM SCREENIO.

           PERFORM RESET-LEGEND.
           IF IC-CODE NOT = 0
              IF SP-DEFAULT-O2-INS-COMP NUMERIC AND
                   SP-DEFAULT-O2-INS-COMP NOT = 0
                 IF ELE = 31
                   IF PD-INSOURS(I-LEVEL) = "Y"
                    IF IC-CODE NOT = SP-DEFAULT-O2-INS-COMP
                       MOVE " " TO MESS
                       STRING "INSURANCE COMPANY MUST BE "
                               SP-DEFAULT-O2-INS-COMP
                                ", PLEASE CHANGE!"
                           DELIMITED BY SIZE INTO MESS
                       PERFORM SEND-MESS
                       MOVE 9 TO IO-FG
                       GO TO EXIT-ENTER-INSURANCE.
           IF IC-CODE NOT = 0 AND GP-DEFAULT-O3-INS-COMPANY NOT = 0
              IF ELE = 32
                 IF PD-INSOURS(I-LEVEL) = "Y"
                    IF IC-CODE NOT = GP-DEFAULT-O3-INS-COMPANY
                       MOVE " " TO MESS
                       STRING "INSURANCE COMPANY MUST BE "
                               GP-DEFAULT-O3-INS-COMPANYX
                                ", PLEASE CHANGE!"
                           DELIMITED BY SIZE INTO MESS
                       PERFORM SEND-MESS
                       MOVE 9 TO IO-FG
                       GO TO EXIT-ENTER-INSURANCE.
       ENTER-IC-TERM.
           IF I-LEVEL > 4 AND GP-OT-INS-TERM NOT = "Y"
              GO TO ENTER-IC-EXP.
           IF IC-CODE = 0
              MOVE 0 TO PD-INSTRM(I-LEVEL)
              GO TO SEND-IC-TERM.
           IF I-LEVEL > 4 AND GP-OT-INS-TERM = "Y"
              MOVE "RNNN060  2." TO VDU
           ELSE
              MOVE "RNNN030  2." TO VDU.
           MOVE ELE TO VDUROW.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-IC-OURS.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO ENTER-IC-TERM.
           IF ( VDUNUM0 > 360 )
              GO TO ENTER-IC-TERM.
           MOVE PD-INSTRM(I-LEVEL) TO INSTERM-HLDR.
           MOVE VDUNUM0 TO PD-INSTRM(I-LEVEL).
       SEND-IC-TERM.
           IF I-LEVEL > 4 AND GP-OT-INS-TERM = "Y"
              MOVE "SN N060  2." TO VDU
           ELSE
              MOVE "SN N030  2." TO VDU.
           MOVE ELE TO VDUROW.
           MOVE PD-INSTRM(I-LEVEL) TO VDUNUM0.
           PERFORM SCREENIO.
           GO TO ENTER-IC-COVR.
       ENTER-IC-EXP.
           IF IC-CODE = 0
              MOVE 0 TO PD-INSEXP-DATE(I-LEVEL)
              GO TO SEND-IC-EXP.

           MOVE "RNNN060  2." TO VDU.
           MOVE ELE TO VDUROW.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-IC-OURS.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO ENTER-IC-EXP.
           IF VDUNUM0 = 0
              GO TO ENTER-IC-EXP.

      * NOT ENTERED AS SCREENIO DATE
           MOVE VDUNUM0 TO DATE-MMDDYY.
           PERFORM CONVERT-MMDDYY-TO-YYYYMMDD.
           MOVE DATE-YYYYMMDD TO PD-INSEXP-DATE(I-LEVEL) NUM-DATE.
           PERFORM TSTDAT.
           IF NUM-DATE = 0
              MOVE "INVALID DATE" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-IC-EXP.
       SEND-IC-EXP.
           MOVE "SNNN060  2." TO VDU.
           MOVE ELE TO VDUROW.
           MOVE PD-INSEXP-DATE(I-LEVEL) TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO VDUNUM0.
           PERFORM SCREENIO.

       ENTER-IC-COVR.
           IF IC-CODE = 0
              MOVE 0 TO PD-INSCOVR(I-LEVEL)
              GO TO SEND-IC-COVR.
           MOVE "RNNN062  3." TO VDU.
           MOVE ELE TO VDUROW.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-IC-TERM.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO ENTER-IC-COVR.
           MOVE PD-INSCOVR(I-LEVEL) TO AMT-HOLDER.
           MOVE VDUNUM2 TO PD-INSCOVR(I-LEVEL).
       SEND-IC-COVR.
           MOVE "SN N062  3." TO VDU.
           MOVE ELE TO VDUROW.
           MOVE PD-INSCOVR(I-LEVEL) TO VDUNUM2.
           PERFORM SCREENIO.
       ENTER-IC-PREM.
           IF IC-CODE = 0
              MOVE 0 TO PD-INSPREM(I-LEVEL)
                        PD-INSCOMM(I-LEVEL)
                        PD-O3INS-DLPROC
              GO TO SEND-IC-PREM.
           IF ELE = 32
              IF ((BR-SFDLO3-WINDOW-FG NOT =  "N")
                             AND (PD-INSOURS(I-LEVEL) = "Y"))
                             OR (BR-SFDLO3-WINDOW-FG = "B")
                 MOVE PD-INSPREM(I-LEVEL) TO SFDLO3-PREM
                 MOVE PD-INSCOMM(I-LEVEL) TO SFDLO3-COMM
                 MOVE PD-O3INS-DLPROC TO SFDLO3-DLPROC
                 MOVE PD-ORGST TO SFDLO3-ORGST
                 MOVE "CL/SFDLO3" TO FORM-NAM
                 CALL FORM-PROGX USING FORM-PATH SFDLO3-WORKERS
                 CANCEL FORM-PROGX
                 MOVE SFDLO3-PREM TO PD-INSPREM(I-LEVEL)
                 MOVE SFDLO3-COMM TO PD-INSCOMM(I-LEVEL)
                 MOVE SFDLO3-DLPROC TO PD-O3INS-DLPROC
                 GO TO SEND-IC-PREM
              ELSE
                 MOVE 0 TO PD-INSCOMM(I-LEVEL)
                           PD-O3INS-DLPROC.

           MOVE "^-BACK, ENTER THE INSURANCE PREMIUM:" TO MESS.
           PERFORM SEND-LEGEND.
           MOVE "RNNN042  4." TO VDU.
           MOVE ELE TO VDUROW.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-IC-COVR.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO ENTER-IC-PREM.

      * REGWAN PR# 3935
      * TEST PASSWD TO CHANGE CALCULATED PREMIUM ON PP
           IF I-LEVEL = 3
              IF VDUNUM2 NOT = PD-INSPREM(I-LEVEL)
                 MOVE PP-PREM-SEQ TO PASSWDW-SEQ
                 MOVE "TO CHANGE INS. PREMIUM...:" TO PASSWDW-PROMPT2
                 PERFORM VERIFY-PASSWORD
                 IF PASSWDW-STAT NOT = "OK"
                    MOVE "SN N042  4." TO VDU
                    MOVE ELE TO VDUROW
                    MOVE PD-INSPREM(I-LEVEL) TO VDUNUM2
                    PERFORM SCREENIO
                    MOVE 9 TO IO-FG
                    GO TO ENTER-IC-PREM.

           MOVE PD-INSPREM(I-LEVEL) TO INSPREM-HLDR.
           MOVE VDUNUM2 TO PD-INSPREM(I-LEVEL).
       SEND-IC-PREM.
           MOVE "SN N042  4." TO VDU.
           MOVE ELE TO VDUROW.
           MOVE PD-INSPREM(I-LEVEL) TO VDUNUM2.
           PERFORM SCREENIO.
           IF I-LEVEL > 4
              IF GP-OT-INS-TERM NOT = "Y"
                 MOVE "I" TO PD-INS-FG(I-LEVEL)
                 MOVE PD-INTDATE              TO NUM-DATE
                 MOVE PD-INSEXP-DATE(I-LEVEL) TO SYS-DATE
                 PERFORM TIM360
                 MOVE ELAPSED-MONTHS TO PD-INSTRM(I-LEVEL)
              END-IF
              GO TO CALC-ON-INS-ENTRY.
       SEND-IC-CALC-FG.
           MOVE "S  A010  5." TO VDU.
           MOVE ELE TO VDUROW.
           MOVE PD-INS-FG(I-LEVEL) TO CALC-FG-HLDR.
           IF NOT (
                    PD-INSOURS(I-LEVEL) = INSOURS-HLDR
                        AND PD-INSTRM(I-LEVEL) = INSTERM-HLDR
                         AND PD-INSCOVR(I-LEVEL) = AMT-HOLDER
                          AND PD-INSPREM(I-LEVEL) = INSPREM-HLDR
                  )
              MOVE "I" TO VDUBUF PD-INS-FG(I-LEVEL)
           ELSE
              MOVE CALC-FG-HLDR TO VDUBUF PD-INS-FG(I-LEVEL).
           PERFORM SCREENIO.
       ENTER-IC-CALC-FG.
           MOVE "E  A010  5." TO VDU.
           MOVE ELE TO VDUROW.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-IC-PREM.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO ENTER-IC-CALC-FG.
           MOVE VDUBUF TO PD-INS-FG(I-LEVEL).
           IF PD-INS-FG(I-LEVEL) NOT = "I"
              IF PD-INS-FG(I-LEVEL) NOT = "C"
                 PERFORM ICE-OR-A-MSG
                 GO TO ENTER-IC-CALC-FG.

       CALC-ON-INS-ENTRY.
           PERFORM VALIDATE-FIELDS-AGAINST-MAX.
           IF IO-FG = 9
                 OR NEW-FLAG = 1
              GO TO EXIT-ENTER-INSURANCE.

           PERFORM COMPUTE-INSCOMM.
           MOVE PD-INSCOMP(I-LEVEL) TO CIP-INS-CO(I-LEVEL).
           MOVE PD-INSOURS(I-LEVEL) TO CIP-INS-USE-SW(I-LEVEL).
           MOVE PD-INSTRM(I-LEVEL) TO CIP-INS-TERM(I-LEVEL).
           MOVE PD-INSCOVR(I-LEVEL) TO CIP-INS-COVERAGE(I-LEVEL).
           MOVE PD-INSPREM(I-LEVEL) TO CIP-INS-PREMIUM(I-LEVEL).
           MOVE PD-INS-FG(I-LEVEL) TO CIP-INS-SW(I-LEVEL).
           MOVE PD-INSFINCHG(I-LEVEL) TO CIP-INSFINCHG-SW(I-LEVEL).
           MOVE PD-INS-TERM-SW(I-LEVEL) TO CIP-INS-TERM-SW(I-LEVEL).
           MOVE PD-COVERAGE-SW(I-LEVEL) TO CIP-COVERAGE-SW(I-LEVEL).
           MOVE PD-PREMIUM-SW(I-LEVEL) TO CIP-PREMIUM-SW(I-LEVEL).

           IF I-LEVEL > 4
              MOVE 0 TO CIP-NO-OF-TIMES-CALLED.
           PERFORM CALL-LOAN-CALCULATIONS.

       EXIT-ENTER-INSURANCE.
           EXIT.

      ************************************************
       COMPUTE-INSCOMM SECTION.
      ************************************************
           IF NOT (I-LEVEL = 6 AND (BR-SFDLO3-WINDOW-FG = "Y" OR "B"))
              MOVE I-LEVEL TO ICOMM
              MOVE PD-INSCOMP(I-LEVEL) TO ICOMM-INSCOMP
              MOVE PD-INSPREM(I-LEVEL) TO ICOMM-INSPREM
              MOVE PD-INSOURS(I-LEVEL) TO ICOMM-INSOURS
              PERFORM ICOMM-CALCULATION
              MOVE ICOMM-INSCOMM TO PD-INSCOMM(I-LEVEL).

       VERIFY-IC-PREM.
           IF PD-INSPREM(I-LEVEL) = ZEROS
              MOVE ZEROS TO PD-INSCOMP(I-LEVEL)
                            PD-INSEFF-DATE(I-LEVEL)
                            PD-INSEXP-DATE(I-LEVEL)
                            PD-INSCOVR(I-LEVEL)
                            PD-INSCOMM(I-LEVEL)
              MOVE SPACES TO PD-INSOURS(I-LEVEL).


      **************************************************
       SETUP-BLINPC-BUF SECTION.
      **************************************************
           MOVE SPACES             TO BLINPC-BUF.
           MOVE PD-BALLOON-LAST-FG TO BLINPC-BALLOON-LAST-FG.
           MOVE PD-REGPYAMT-FG     TO BLINPC-REGPYAMT-FG.
           MOVE PD-BALLOON-FG      TO BLINPC-BALLOON-FG.

      * ON SINGLE PAY LOANS STOP OPTION TO SELECT CALCULATE BALLOON
           IF COP-TERM = 1
              MOVE " "             TO BLINPC-BALLOON-FG.

           MOVE PD-SELLPR-FG       TO BLINPC-SELLPR-FG.
           MOVE PD-SELLPR          TO BLINPC-SELLPR.
           MOVE PD-LASTPYAMT       TO BLINPC-LASTPYAMT.
           MOVE COP-REGPYAMT       TO BLINPC-REGPYAMT.

      **************************************************
       BLINPC-CALC-WINDOW SECTION.
      **************************************************
           MOVE "WI/BLINPC" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH BLINPC-BUF EXIT-PATHNAME.
           CANCEL FORM-PROGX.

      *****************************************************************
      *                    COMBINATION CHART                          |
      *                                                               |
      *  NOTE:                                                        |
      *       FLAGS ARE IN STATUS, PER CHART, WHEN THIS ROUTINE       |
      *       IS PERFORMED.                                           |
      *----------------------------------------------------------------
      *             | COMBO 1     | COMBO 2         | COMBO 3         |
      *-------------|--------------------------------------------------
      *             |  REALLY ---->  REALLY --> THIS|  REALLY --> THIS|
      *             |--------------------------------------------------
      * SELLPR-FG   | I OR I OR C | I OR I OR C | I | I OR C OR C | C |
      * BALLOON-FG  | I           | C           |C/I| I           | I |
      * REGPYAMT-FG | C           | I           |I/C| I           | I |
      *****************************************************************
       AFTER-BALLO-CALC-OPT SECTION.

      * SELLING PRICE    (CALCULATE)

           IF BLINPC-CALC-SELECT-KEY = "P"
              MOVE "C" TO PD-SELLPR-FG
                          PD-PROCEEDS-FG
              MOVE "I" TO PD-BALLOON-FG.

      * REGULAR PAYMENT   (CALCULATE)

           IF BLINPC-CALC-SELECT-KEY = "R"
              MOVE "C" TO PD-REGPYAMT-FG
              MOVE "I" TO PD-BALLOON-FG.

       AFTER-BALLO-CALC-OPT-EXIT.
           EXIT.

      **************************************
       AFTER-REGPY-CALC-OPT SECTION.
      **************************************

      * LOAN PROCEEDS    (CALCULATE)

           IF BLINPC-CALC-SELECT-KEY = "P"
              MOVE "C" TO PD-SELLPR-FG
                          PD-PROCEEDS-FG
              MOVE "I" TO PD-REGPYAMT-FG.

      * BALLOON OR LAST PAYMENT   (CALCULATE)

           IF BLINPC-CALC-SELECT-KEY = "B" OR "L"
              MOVE "C" TO PD-BALLOON-FG
              MOVE "I" TO PD-REGPYAMT-FG.

       AFTER-REGPY-CALC-OPT-EXIT.
           EXIT.

      **************************************
       AFTER-SELLPR-CALC-OPT SECTION.
      **************************************

      * REGULAR PAYMENT    (CALCULATE)

           IF BLINPC-CALC-SELECT-KEY = "R"
              MOVE "I" TO PD-SELLPR-FG
                          PD-PROCEEDS-FG
              MOVE "C" TO PD-REGPYAMT-FG.

      * BALLOON OR LAST PAYMENT   (CALCULATE)

           IF BLINPC-CALC-SELECT-KEY = "B" OR "L"
              MOVE "I" TO PD-SELLPR-FG
                          PD-PROCEEDS-FG
              MOVE "C" TO PD-BALLOON-FG.

       AFTER-REGPY-CALC-OPT-EXIT.
           EXIT.

      ********************************************
       CHECK-SP-EFFECTIVE SECTION.
      ********************************************

      * CHECK AND GET EFFECTIVE SPR RECORD FOR GIVEN LOAN'S LOANDATE

           MOVE PD-ORGST TO SP-ORGST.
           MOVE PD-SPRCLASS TO SP-SPRCLASS.
           MOVE PD-SUBCLASS TO SP-SUBCLASS.
           MOVE 0 TO SP-LAWCODE.
           PERFORM START-SP1-FILE.
       CHECK-SP-EFFECTIVE-NEXT.
           PERFORM READ-SP1-FILE-NEXT.
           IF IO-FG = 9
              GO TO CHECK-SP-EFFECTIVE-EXIT.
           IF NOT (SP-ORGST = PD-ORGST
                       AND SP-SPRCLASS = PD-SPRCLASS
                          AND SP-SUBCLASS = PD-SUBCLASS)
              MOVE 9 TO IO-FG
              GO TO CHECK-SP-EFFECTIVE-EXIT.

           IF SP-EFFDATE > PD-LOANDATE
              GO TO CHECK-SP-EFFECTIVE-NEXT
           ELSE
              IF SP-EXPDATE NOT = 0
                 IF SP-EXPDATE NOT > PD-LOANDATE
                    GO TO CHECK-SP-EFFECTIVE-NEXT.

       CHECK-SP-EFFECTIVE-EXIT.
           EXIT.

      ********************************************
       MEMO-ENTRY SECTION.
      ********************************************
           PERFORM SEND-MEMO.
       ENTER-MEMO.
           MOVE "E  A300M2." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS NOT = "00"
              GO TO ENTER-MEMO.
           MOVE VDUBUF TO PD-MESSAGE.
           IF PD-MESSAGE = " "
              MOVE "M1 M2." TO MESS
              PERFORM RESET-MESS.

      ********************************************
       NEW-RECORD-SETUP SECTION.
      ********************************************
           MOVE 1 TO NEW-FLAG.
           PERFORM CLEAR-PD-FILE.
           MOVE " " TO DISPLAY-BUF1
                        SCREEN-LINES
                         PD2-KEY
                          SAVE-PD2-KEY
                           PRIOR-LOAN-STAT
                            HOLD-BWNAME
                             PASS-STAT-FG.

           MOVE TRANS-DATE-YYYYMMDD TO PD-LOANDATE DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO DEF-LOANDATE.

           MOVE BR-STATE TO DEF-ORGST
                            PD-ORGST
                            PD-OWNST.
           MOVE EXT-CONAME-USER-LOC TO DEF-ORGBR
                               PD-ORGBR
                               PD-OWNBR.

      * IF I DONT DO WEEKLY/BIWEEKLY
           IF BR-UNITPER-FG NOT = "Y"
              MOVE "M" TO BR-DEFAULT-LN-UNITPER-CD
              MOVE BR-DEFAULT-LN-UNITPER-CD TO DEF-UNITPER-CD
                                               PD-UNITPER-CD
           ELSE
              IF BR-DEFAULT-LN-UNITPER-CD = " "
                 MOVE "_" TO DEF-UNITPER-CD
              ELSE
                 IF NOT (BR-DEFAULT-LN-UNITPER-CD = "M"
                                                  OR "W"
                                                   OR "B"
                                                    OR "S")
                    MOVE "M" TO BR-DEFAULT-LN-UNITPER-CD
                    MOVE BR-DEFAULT-LN-UNITPER-CD TO DEF-UNITPER-CD
                                                     PD-UNITPER-CD
                 ELSE
                    MOVE BR-DEFAULT-LN-UNITPER-CD TO DEF-UNITPER-CD
                                                     PD-UNITPER-CD.
           MOVE 1 TO DEF-UNITPER-FREQ
                     PD-UNITPER-FREQ.
           MOVE "B" TO DEF-BALLBL
                       PD-BALLOON-LAST-FG.

           IF BR-DEFAULT-LN-PURCD = " "
              MOVE "___" TO DEF-PURCD
              MOVE " " TO CHFG(7)
           ELSE
              MOVE "N" TO CHFG(7)
              MOVE "PU" TO CD-TYPE
              MOVE BR-DEFAULT-LN-PURCD TO DEF-PURCD
                                          PD-PURCD
                                          CD-CODE
              MOVE 7 TO ELE
              PERFORM SEND-CD-DESC
              IF IO-FG = 9
                 MOVE " " TO CHFG(7).

           MOVE DEF-BUF TO WR-BUF.
           MOVE DEF-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

           MOVE 0 TO PD-LASTPYAMT.
           MOVE "I" TO PD-BALLOON-FG.
           MOVE "N" TO CHFG(10).

           MOVE 999.99 TO PD-SERVPER.
           MOVE 9999.99 TO PD-DLNO.
           MOVE 999.999 TO PD-ENDORATE
                           PD-OVRDRATE.
           MOVE "C" TO PD-OVRDRATE-FG
                       PD-INS-FG(1)
                       PD-INS-FG(2)
                       PD-INS-FG(3)
                       PD-INS-FG(4)
                       PD-SERCHG-FG
                       PD-MAINTFEE-FG
                       PD-EXTCHG-FG
                       PD-REGPYAMT-FG
                       PD-DLRVPRIN-FG(1)
                       PD-DLRVPRIN-FG(2)
                       PD-DLRVPRIN-FG(3)
                       PD-DLRVCHGS-FG(1)
                       PD-DLRVCHGS-FG(2)
                       PD-DLRVCHGS-FG(3)
                       PD-DLPARVHELD-FG
                       PD-DLPARVPAID-FG
                       PD-DLDISC-FG.
           MOVE 0 TO TOTAL-GL-HLDR
                     TOTAL-GL-FINCHG-HLDR
                     TERM-LOW
                     TERM-HIGH
                     PROCEEDS-LOW
                     PROCEEDS-HIGH
                     POINTS-LOW
                     POINTS-HIGH
                     PRIOR-LNAMT.
           MOVE "Q" TO QUOTE-ID-FG
                       RECALL-ID-FG.

           MOVE "Y" TO DEFAULT-APR-FG.
           MOVE "N" TO STAT.
           PERFORM PROTECT-FIELDS.
           MOVE "N" TO CHFG(11).

           MOVE 0 TO ELE PRIOR-LNAMT PD-CAN-LOAN-MAX.
           PERFORM CLEAR-CALC-AREA-Z3.

      ********************************************
       DISPLAY-FIELDS SECTION.
      ********************************************
           MOVE " " TO DISPLAY-BUF1.
           PERFORM GET-SP.
           MOVE PD-CLASS TO LC-CODE D-LCNO.
           PERFORM GET-LC.
           MOVE LC-DESC TO D-LC-DESC.

           MOVE 1 TO ELE.
       DISP-NEXT-BW.
           MOVE PD-SSNO(ELE) TO BW-SSNO.
           PERFORM NUMBW-TO-ALPBW.
           MOVE " " TO BW-LNAME BW-FNAME.
           IF BW-SSNO NOT = 0 AND NOT QUOTE-ONLY
              PERFORM GET-BW1
              MOVE ALP-BW TO D-010(ELE)
              MOVE INSPECT-W TO D-011(ELE)
              MOVE PD-MAKERCD(ELE) TO D-012(ELE).
           IF ELE < 3
              ADD 1 TO ELE
              GO TO DISP-NEXT-BW.

           MOVE PD-LNAMT TO D-LOANAMT.
           COMPUTE ANTIC = PD-ANTICERN(1) + PD-ANTICERN(2)
                         + PD-ANTICERN(3) + PD-ANTICERN(4)
                         + PD-ANTICERN(5).
           IF NOT GP-INCL-OTH-IN-EARNED-N
              ADD PD-ANTICERN(6) TO ANTIC.
           MOVE ANTIC TO D-ANTIC.
           MOVE PD-ORGST TO D-050.
           MOVE PD-ORGBR TO D-051.
           MOVE PD-SUBCLASS TO D-052.
           MOVE SP-DESC TO D-053.
           PERFORM MAKE-DATES-CURRENT.

           MOVE "PU"     TO CD-TYPE.
           MOVE PD-PURCD TO D-070 CD-CODE.
           PERFORM GET-CD.
           MOVE CD-DESC TO D-071.

           MOVE "CO"      TO CD-TYPE.
           MOVE PD-COLLCD TO CD-CODE  D-080.
           PERFORM GET-CD.
           MOVE CD-DESC   TO D-081.
           MOVE CD-CO-SECURED TO SPTEST-CD-SECURED.

           MOVE "SR" TO CD-TYPE.
           MOVE PD-SRCD TO D-090 CD-CODE.
           PERFORM GET-CD.
           MOVE CD-DESC TO D-091.

           MOVE PD-UNITPER-CD TO D-100.
           MOVE PD-UNITPER-FREQ TO D-101.
           MOVE PD-BALLOON-LAST-FG TO D-102.
           PERFORM DISPLAY-BALLOON-LAST.

           MOVE PD-PRLNNO TO D-110.
           IF PD-PRLNNO NOT  = 0
              PERFORM PROCESS-PRIOR-LOAN
              IF ERROR-IN-PRIOR-LOAN
                 MOVE 9999999.99 TO D-111
              ELSE
                 MOVE PRIOR-LNAMT TO D-111.

           MOVE PD-ORGTERM TO D-120.

           MOVE PD-ORIG-1STPYDATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO D-130.

           MOVE PD-SELLPR TO D-140.
           MOVE PD-SELLPR-FG TO D-141.


      *  DEFENSE IF
           IF PD-ORGTERM = 1
              MOVE 0 TO PD-LASTPYAMT D-170
              MOVE "I" TO PD-BALLOON-FG D-171
           ELSE
              IF PD-BALLOON-LAST-FG = "L"
                 MOVE PD-LASTPYAMT TO D-170
              ELSE
                 SUBTRACT PD-REGPYAMT FROM PD-LASTPYAMT
                                             GIVING D-170
              END-IF
              MOVE PD-BALLOON-FG TO D-171.

           MOVE PD-INSCODES TO D-180.

           MOVE PD-POINTS TO D-181.
           MOVE PD-POINTS-FG TO D-182.
           IF PD-POINTS = 0
              MOVE " " TO D-182.

           PERFORM SETUP-FEES-FOR-DISPLAY.

           MOVE PD-DLNO TO D-220
                           DL-DLNO.
           PERFORM GET-DL1.

           MOVE DISPLAY-BUF1 TO WR-BUF.
           MOVE DISPLAY-LIST1 TO WR-LIST.
           PERFORM CALL-WRFORM.

           MOVE 0 TO COUNTER
                     TOTAL-GL-HLDR
                     TOTAL-GL-FINCHG-HLDR
           PERFORM COMPUTE-GL-FEEAMT.
           MOVE 800 TO CIP-NO-OF-TIMES-CALLED.
           PERFORM SETUP-CALC-AREA.

       DISP-INTERMEDIATE-FIELDS.
           PERFORM DISPLAY-PROMPTS.
           MOVE " " TO DISPLAY-BUF2.
           MOVE PD-COLLID TO D-230.

           PERFORM MOVE-MSG.

           MOVE PD-OVRDRATE TO D-260
                               CIP-OVR-RATE.
           MOVE PD-OVRDRATE-FG TO D-261
                                  CIP-OVR-RATE-SW.
           MOVE PD-EFFRATE TO D-262
                              CIP-EFF-RATE
                              CEP-EFF-RATE.
           MOVE PD-APRATE TO D-263
                             CIP-APR-RATE
                             CEP-APR-RATE.
           PERFORM VARYING ELE FROM 1 BY 1 UNTIL ELE > 4
              MOVE PD-INSCOMP(ELE)     TO D-1COMP(ELE)
                                          CIP-INS-CO(ELE)
              MOVE PD-INSPREM(ELE)     TO D-1PREM(ELE)
                                          CIP-INS-PREMIUM(ELE)
                                          CEP-INS-PREMIUM(ELE)
              MOVE PD-INSTRM(ELE)      TO D-1TERM(ELE)
                                          CIP-INS-TERM(ELE)
                                          CEP-INS-TERM(ELE)
              MOVE PD-INSCOVR(ELE)     TO D-1COVR(ELE)
                                          CIP-INS-COVERAGE(ELE)
                                          CEP-INS-COVERAGE(ELE)
              MOVE PD-INSOURS(ELE)     TO D-1OUR(ELE)
                                          CIP-INS-USE-SW(ELE)
              MOVE PD-INS-FG(ELE)      TO D-INS-FG(ELE)
                                          CIP-INS-SW(ELE)
              MOVE PD-INSFINCHG(ELE)   TO CIP-INSFINCHG-SW(ELE)
              MOVE PD-INS-TERM-SW(ELE) TO CIP-INS-TERM-SW(ELE)
              MOVE PD-COVERAGE-SW(ELE) TO CIP-COVERAGE-SW(ELE)
              MOVE PD-PREMIUM-SW(ELE)  TO CIP-PREMIUM-SW(ELE)
           END-PERFORM.

           MOVE 1 TO SUB.
           MOVE 5 TO ELE.
       DISP-NEXT-2IC.
           MOVE PD-INSCOMP(ELE)     TO D-2COMP(SUB)
                                       CIP-INS-CO(ELE).
           MOVE PD-INSPREM(ELE)     TO D-2PREM(SUB)
                                       CIP-INS-PREMIUM(ELE)
                                       CEP-INS-PREMIUM(ELE).
           MOVE PD-INSTRM(ELE)      TO CIP-INS-TERM(ELE)
                                       CEP-INS-TERM(ELE).
           IF GP-OT-INS-TERM = "Y"
              MOVE PD-INSTRM(ELE)      TO D-2TERM(SUB)
           ELSE
              MOVE PD-INSEXP-DATE(ELE) TO DATE-YYYYMMDD
              PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
              MOVE DATE-MMDDYY      TO D-2EXP(SUB).
           MOVE PD-INSCOVR(ELE)     TO D-2COVR(SUB)
                                       CIP-INS-COVERAGE(ELE)
                                       CEP-INS-COVERAGE(ELE).
           MOVE PD-INSOURS(ELE)     TO D-2OUR(SUB)
                                       CIP-INS-USE-SW(ELE).
           MOVE PD-INS-FG(ELE)      TO CIP-INS-SW(ELE).
           MOVE PD-INSFINCHG(ELE)   TO CIP-INSFINCHG-SW(ELE).
           MOVE PD-INS-TERM-SW(ELE) TO CIP-INS-TERM-SW(ELE).
           MOVE PD-COVERAGE-SW(ELE) TO CIP-COVERAGE-SW(ELE).
           MOVE PD-PREMIUM-SW(ELE)  TO CIP-PREMIUM-SW(ELE).
           IF ELE < 6
              ADD 1 TO ELE SUB
              GO TO DISP-NEXT-2IC.

           MOVE 1 TO ELE.

       DISP-OUTPUT-FIELDS.
           IF SP-FEEFRMLA(1) NOT = " "
              MOVE PD-FEEAMT(1) TO D-200
                                   CIP-FEE-CHG(1)
                                   CEP-FEE-CHG(1)
              MOVE PD-FEECD-FG(1) TO D-201
                                      CIP-FEE-CHG-SW(1).

           IF SP-FEEFRMLA(2) NOT = " "
              MOVE PD-FEEAMT(2) TO D-210
                                   CIP-FEE-CHG(2)
                                   CEP-FEE-CHG(2)
              MOVE PD-FEECD-FG(2) TO D-211
                                      CIP-FEE-CHG-SW(2).

           MOVE PD-SERCHG TO D-340
                             CIP-SERVICE-CHG
                             CEP-SERVICE-CHG.
           MOVE PD-STATE-WORKER TO CIP-STATE-WORKER.
           MOVE PD-SERCHG-FG TO D-341 CIP-SERVICE-CHG-SW.
           IF PD-ORGST = "MX"
              MOVE PD-ANTICERN(4) TO D-350
                                     MAINTFEE-HLDR
                                     CIP-MAINT-FEES
                                     CEP-MAINT-FEES
           ELSE
              MULTIPLY PD-MAINTFEE BY PD-ORGTERM GIVING D-350
                 CIP-MAINT-FEES CEP-MAINT-FEES ROUNDED.
           MOVE PD-MAINTFEE-FG TO D-351 CIP-MAINT-FEES-SW.
           MOVE PD-EXTCHG TO D-360 CIP-1ST-PY-EXT CEP-1ST-PY-EXT.
           MOVE PD-EXTCHG-FG TO D-361 CIP-1ST-PY-EXT-SW.
           MOVE PD-INTCHG TO D-370 CIP-INT-CHARGE.
           MOVE PD-APR-FINAMT TO D-380
                                 CIP-APR-FINANCD.
           MOVE PD-FINAMT TO CIP-AMT-FINANCD.
           MOVE PD-PROCEEDS-FG TO D-381.
           MOVE PD-LNAMT TO CIP-LOAN-AMOUNT.
           IF PD-LOANTYPE = "P"
              MOVE PD-LNAMT TO D-390
           ELSE
              PERFORM LOAN-CALCULATIONS
              MOVE TOTAL-REPAY TO D-390.
           ADD PD-ANTICERN(1)
                PD-ANTICERN(2)
                 PD-ANTICERN(3)
                  PD-ANTICERN(4)
                   PD-ANTICERN(5) GIVING CIP-ANTIC-EARNS.
           IF NOT GP-INCL-OTH-IN-EARNED-N
              ADD PD-ANTICERN(6) TO CIP-ANTIC-EARNS.

      * SETUP FIRST PAYMENT AMOUNT:
           MOVE PD-1STPYAMT TO D-400.
           IF PD-1STPYAMT = PD-REGPYAMT
              MOVE 0 TO CIP-ODD-1ST-PMT.

      * SETUP LAST PAYMENT AMOUNT:
           MOVE PD-LASTPYAMT TO D-410.
           IF PD-BALLOON-LAST-FG = "L"
              MOVE PD-LASTPYAMT TO CIP-BALLOON
           ELSE
      *       DEFENSE IF
              IF PD-ORGTERM = 1
                 MOVE 0 TO PD-LASTPYAMT CIP-BALLOON
              ELSE
                 SUBTRACT PD-REGPYAMT FROM PD-LASTPYAMT
                                         GIVING CIP-BALLOON.
           MOVE PD-BALLOON-FG TO CIP-BALLOON-SW.

      * SETUP REGUALR PAYMENT AMOUNT:
           MOVE PD-REGPYAMT TO D-420
                               CIP-REGPYAMT.
           MOVE PD-REGPYAMT-FG TO D-421
                                  CIP-REGPYAMT-SW.

           IF PD-LOANTYPE = "I"
              MOVE PD-SERCHG TO PD-ANTICERN(3)
              MOVE 0 TO PD-ANTICERN(1)
                        PD-ANTICERN(2)
                        PD-ANTICERN(4)
                        PD-ANTICERN(5)
                        PD-ANTICERN(6)
           ELSE
              ADD PD-INTCHG PD-EXTCHG GIVING PD-ANTICERN(1)
              MOVE PD-SERCHG TO PD-ANTICERN(3)
              COMPUTE PD-ANTICERN(4) = PD-ORGTERM * PD-MAINTFEE
              MOVE 0 TO PD-ANTICERN(5)
                        PD-ANTICERN(6).

            MOVE PD-ANTICERN(1) TO WK-ANTICERN(1).
            MOVE PD-ANTICERN(2) TO WK-ANTICERN(2).
            MOVE PD-ANTICERN(3) TO WK-ANTICERN(3).
            MOVE PD-ANTICERN(4) TO WK-ANTICERN(4).
            MOVE PD-ANTICERN(5) TO WK-ANTICERN(5).
            MOVE PD-ANTICERN(6) TO WK-ANTICERN(6).
            PERFORM ADJUST-ANTICERN.
            MOVE WK-ANTICERN(1) TO PD-ANTICERN(1).
            MOVE WK-ANTICERN(2) TO PD-ANTICERN(2).
            MOVE WK-ANTICERN(3) TO PD-ANTICERN(3).
            MOVE WK-ANTICERN(4) TO PD-ANTICERN(4).
            MOVE WK-ANTICERN(5) TO PD-ANTICERN(5).
            MOVE WK-ANTICERN(6) TO PD-ANTICERN(6).

           MOVE DISPLAY-BUF2 TO WR-BUF.
           MOVE DISPLAY-LIST2 TO WR-LIST.
           PERFORM CALL-WRFORM.


           IF PD-MESSAGE NOT = " "
              PERFORM SEND-MEMO
              PERFORM ALARM.

      *---------------------------------------------------{ RESCISSION }
       DISPLAY-FIELDS-RESCISSION.

           IF ( PD-POCD-INRESCISSION )
              MOVE RESCISSION-MESSAGE TO RESCISSION-BUF
              PERFORM SEND-RESCISSION.

       DISPLAY-FIELDS-EXIT.
           EXIT.

      ********************************************
       SETUP-FEES-FOR-DISPLAY SECTION.
      ********************************************
           ADD PD-FEEAMT(1) PD-FEEAMT(2)
               PD-FEEAMT(3) PD-FEEAMT(4)
               PD-FEEAMT(5) PD-FEEAMT(6)
               PD-FEEAMT(7) PD-FEEAMT(8)
               PD-FEEAMT(9) PD-FEEAMT(10)
                                  GIVING HOLD-WORKER.
           IF SP-FEEFRMLA(1) NOT = " "
              SUBTRACT PD-FEEAMT(1) FROM HOLD-WORKER
              MOVE PD-FEEAMT(1) TO D-200
              MOVE PD-FEECD-FG(1) TO D-201.
           IF SP-FEEFRMLA(2) NOT = " "
              SUBTRACT PD-FEEAMT(2) FROM HOLD-WORKER
              MOVE PD-FEEAMT(2) TO D-210
              MOVE PD-FEECD-FG(2) TO D-211.
           MOVE HOLD-WORKER TO D-190.


      ********************************************
       WRITE-SCRATCH SECTION.
      ********************************************
           MOVE "LOAN TEMP SAVE IN PROGRESS.........." TO LEGEND.
           PERFORM SEND-LEGEND.

           PERFORM OPEN-PD1-FILE.
           MOVE PD-RECORD TO HOLD-PD-RECORD.
           MOVE PD-SSNO(1) TO SAVE-SSNO.
           PERFORM SET-SCRATCH-KEY.
           IF QUOTE-ONLY
              MOVE "Q" TO PD-ID
              MOVE PD2-KEY TO SAVE-PD2-KEY.
           PERFORM READ-PD1-FILE.

           MOVE HOLD-PD-RECORD TO PD-RECORD.
           IF QUOTE-ONLY
              MOVE "Q" TO PD-ID
              MOVE SAVE-PD2-KEY TO PD2-KEY.
           MOVE PD-ID TO PD2-ID.
           MOVE PD-BRANCH TO PD2-BRANCH.

           MOVE SAVE-BW-NAME TO PD2-BWNAME.
           IF IO-FG = 9
              GO TO CREATE-NEW-SCRATCH.

           PERFORM REWRITE-PD1-FILE.
           GO TO EXIT-WRITE-SCRATCH.
       CREATE-NEW-SCRATCH.
           PERFORM SET-SCRATCH-KEY.
           IF QUOTE-ONLY
              MOVE "Q" TO PD-ID.

           PERFORM WRITE-PD1-FILE.
           IF IO-FG = 9
              MOVE "RECORD EXISTS FOR RECALL # !"
                TO MESS
              PERFORM SEND-MESS
              MOVE "PLEASE REENTER RECALL # AND RE-INPUT"
                TO MESS
              PERFORM SEND-MESS.
       EXIT-WRITE-SCRATCH.
           PERFORM CLOSE-PD1-FILE.

      ********************************************
       DELETE-SCRATCH SECTION.
      ********************************************
           MOVE "LOAN TEMP DELETE IN PROGRESS........" TO LEGEND.
           PERFORM SEND-LEGEND.

           PERFORM OPEN-PD1-FILE.
           MOVE PD-SSNO(1) TO SAVE-SSNO.
           PERFORM SET-SCRATCH-KEY.
           IF QUOTE-ONLY OR
              QUOTE-CHANGED-TO-LOAN
              MOVE "Q" TO PD-ID.

           PERFORM READ-PD1-FILE.
           IF IO-FG = 0
              PERFORM DELETE-PD1-FILE.
           PERFORM CLOSE-PD1-FILE.

      ********************************************
       DEFAULT-INT-PMT-DATES SECTION.
      ********************************************
           PERFORM CHECK-SP-EFFECTIVE.
           IF IO-FG = 9
              PERFORM SP-INVALID
              GO TO EXIT-DEFAULT-INT-PMT-DATES.

           MOVE SP-LAWCODE TO PD-LAWCODE.
           IF PD-INTDATE = 0
              MOVE "S  8000061." TO VDU
              MOVE PD-LOANDATE TO PD-INTDATE VDU-DATE-YYYYMMDD
              PERFORM SCREENIO.
           IF PD-1STPYDATE NOT = 0
              MOVE 0 TO AMT-HOLDER
              GO TO EXIT-DEFAULT-INT-PMT-DATES.

           MOVE 1 TO AMT-HOLDER.
           PERFORM SET-1ST-PAY-DATE.

       EXIT-DEFAULT-INT-PMT-DATES.
           EXIT.

      ********************************************
       PROCESS-PRIOR-LOAN SECTION.
      ********************************************
           MOVE " " TO PASS-STAT-FG.

           IF ( PD-PRLNNO = 0 ) OR
              ( NOT ( PD-SRCD = "PB" OR "RN" OR "SC" OR "RB" OR "RO" ))
              GO TO EXIT-PROCESS-PRIOR-LOAN.

           MOVE PD-PRLNNO TO W-LNNO PASS-PRLNNO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    WILL USE INTEREST START DATE FOR RECISION (CITIZENS):
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *--->MOVE TRANS-DATE-YYYYMMDD TO PASS-TRANS-DATE.
           MOVE PD-INTDATE TO PASS-TRANS-DATE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    NEED THIS FLAG TO TEST IF READ ON PD1 IS SUCCESSFUL, THEY
      *    MANUALLY PAID OUT THE PRIOR LOAN, NOT ALLOWED, BUT THEN
      *    YOU LOSE THE PD-POCD TO TEST IF ENTERING A RESCISSION LOAN...
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( PD-POCD-INRESCISSION )
              MOVE PD-INTDATE TO PASS-TRANS-DATE
              MOVE "Y"        TO RESCISSION-FLAG
           ELSE
              MOVE SPACES     TO RESCISSION-FLAG.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE PD-SRCD  TO PASS-CURRENT-PD-SRCD.
           MOVE PD-CLASS TO PASSX-CURRENT-PD-CLASS.
           MOVE PD-REC TO HOLD-PD-RECORD.
           MOVE "R" TO PD-ID.
           MOVE EXT-CONAME-USER-LOC TO PD-BRANCH.
           MOVE PD-PRLNNO TO PD-SOC-SEC.
           PERFORM GET-PD1.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    IF MAKING A RESCISSION LOAN, AND YOU FIND THE 'PD' FOR A
      *    MANUAL PAYOFF, DON'T ALLOW IT!
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( IO-BAD )
              IF ( NOT PD-POCD-INRESCISSION )
                 GO TO PROCESS-PRIOR-PAYOFF
      *          GO TO END-PROCESS-PRIOR-LOAN  JTG 000518 CAUSED PROBLEMS AJ
              ELSE
                 GO TO PROCESS-PRIOR-PAYOFF
           ELSE
           IF ( RESCISSION-FLAG = "Y" )
              MOVE "Z" TO PASS-STAT-FG
              GO TO END-PROCESS-PRIOR-LOAN.

           IF PD-PLDATE NOT = 0
                   OR PD-CURBAL = 0
              MOVE "Z" TO PASS-STAT-FG
              GO TO END-PROCESS-PRIOR-LOAN.
           IF PASS-CURRENT-PD-SRCD = "SC"
              IF PD-SALEFIN NOT = "Y" OR
                 PD-SERVACCT NOT = "N"
                 MOVE "S" TO PASS-STAT-FG
                 GO TO END-PROCESS-PRIOR-LOAN.
           IF PASS-CURRENT-PD-SRCD NOT = "SC"
              IF PD-SALEFIN = "Y"
                 MOVE "N" TO PASS-STAT-FG
                 GO TO END-PROCESS-PRIOR-LOAN.

      *---------------------------------------------------------------
      *    INSURE THAT TRCD OF RENEWED LOAN MATCHS THE SOURCE CODE
      *    OF THIS LOAN BEING BOOKED NOW
      *---------------------------------------------------------------
           MOVE PD-PO-TRCD TO HOLD-PRIOR-PO-TRCD.
           IF PD-PO-TRCD NOT = PASS-CURRENT-PD-SRCD
              MOVE "J" TO PASS-STAT-FG
              GO TO END-PROCESS-PRIOR-LOAN.

           MOVE PD-SSNO(1) TO SAVE-SSNO.

           COMPUTE PRIOR-LNAMT ROUNDED =
                (PD-PRBAL + PD-PRINTDUE)
                                 - (PD-PRFINCHGRB + PD-PRINSRB).

           MOVE PD-ORGST TO CIP-PRIOR-LOAN-STATE.
           MOVE PD-CLASS TO CIP-PRIOR-LOAN-CLASS.
           MOVE PD-SUBCLASS TO CIP-PRIOR-SUB-CLASS.
           MOVE PD-PRBAL TO CIP-PRBAL.
           MOVE PD-PRINTDUE TO CIP-PRINTDUE.

           MOVE PD-PRFINCHGRB TO CIP-PRFINCHGRB.
           MOVE PD-PRINSRB TO CIP-PRINSRB.
           MOVE PD-LOANDATE TO CIP-PRIOR-LOAN-DATE.
           MOVE PD-PRPAID-AHEAD TO CIP-PRPAID-AHEAD.

           GO TO END-PROCESS-PRIOR-LOAN.

       PROCESS-PRIOR-PAYOFF.
           MOVE 0 TO IO-FG.

           MOVE "LP/PAYOFX" TO FORM-NAM.
           MOVE "CALCULATING - PLEASE WAIT" TO LEGEND.
           PERFORM SEND-LEGEND.
           CALL FORM-PROGX USING FORM-PATH PASS-PAYOFF-FLDS.
           CANCEL FORM-PROGX.

           PERFORM TEST-FOR-GEORGIA-SCHG.
           PERFORM TEST-FOR-SC-SCHG.
           PERFORM TEST-FOR-WI-SCHG.
           PERFORM TEST-FOR-MFEE.
           PERFORM RESET-LEGEND.

           IF ERROR-IN-PAYOFF
              MOVE 9 TO IO-FG
              GO TO END-PROCESS-PRIOR-LOAN.

           COMPUTE PRIOR-LNAMT ROUNDED =
               (PASS-PRBAL + PASS-PRINTDUE)
                        - (PASS-PRFINCHGRB + PASS-PRINSRB).

           MOVE PASS-PRIOR-LOAN-STATE TO CIP-PRIOR-LOAN-STATE.
           MOVE PASS-PRIOR-LOAN-CLASS TO CIP-PRIOR-LOAN-CLASS.
           MOVE PASS-PRIOR-SUB-CLASS TO CIP-PRIOR-SUB-CLASS.
           MOVE PASS-PRBAL TO CIP-PRBAL.
           MOVE PASS-PRINTDUE TO CIP-PRINTDUE.

           MOVE PASS-PRFINCHGRB TO CIP-PRFINCHGRB.
           MOVE PASS-PRINSRB TO CIP-PRINSRB.
           MOVE PASS-PRIOR-LOAN-DATE TO CIP-PRIOR-LOAN-DATE.
           IF PASS-CDEL-ARREARAGE > 0
              MOVE PASS-CDEL-ARREARAGE TO CIP-PRPAID-AHEAD
           ELSE
              MOVE 0 TO CIP-PRPAID-AHEAD.

       END-PROCESS-PRIOR-LOAN.
           MOVE HOLD-PD-RECORD TO PD-REC.
           IF IO-FG = 9
              MOVE "E" TO PRIOR-LOAN-STAT
              GO TO EXIT-PROCESS-PRIOR-LOAN.
           MOVE CIP-PRBAL TO PD-PRBAL.
           MOVE CIP-PRINTDUE TO PD-PRINTDUE.
           MOVE CIP-PRFINCHGRB TO PD-PRFINCHGRB.
           MOVE CIP-PRINSRB TO PD-PRINSRB.
           MOVE CIP-PRPAID-AHEAD TO PD-PRPAID-AHEAD.
           IF NOT PRIOR-LOAN-NOT-VALID
              IF CAN-LOAN-NO NOT = " "
                 IF PRIOR-LNAMT > PD-CAN-LOAN-MAX
                    MOVE "PRIOR LOAN AMOUNT EXCEEDS CANNED LOAN MAX"
                                                          TO MESS
                    PERFORM SEND-MESS
                    MOVE 9 TO IO-FG
                    GO TO END-PROCESS-PRIOR-LOAN
                 ELSE
                    MOVE "SNNN062140." TO VDU
                    SUBTRACT PRIOR-LNAMT FROM PD-CAN-LOAN-MAX
                         GIVING VDUNUM2 PD-PROCEEDS CIP-PROCEEDS
                    PERFORM SCREENIO
                 END-IF
              END-IF
              IF (NEW-FLAG NOT = 1)
      * CITIZENS TEST
                 MOVE 800 TO CIP-NO-OF-TIMES-CALLED
      * A90      MOVE 0 TO CIP-NO-OF-TIMES-CALLED
                 PERFORM CALL-LOAN-CALCULATIONS.

           MOVE " " TO PRIOR-LOAN-STAT.
           GO TO EXIT-PROCESS-PRIOR-LOAN.

       PRIOR-LOAN-ERROR-MSG.
           IF ERROR-IN-PAYOFF
              MOVE "PRIOR LOAN INVALID" TO MESS
              PERFORM SEND-MESS
           ELSE
              MOVE "INITIATE RENEWAL IN TRANSACTION POSTING" TO MESS
              PERFORM SEND-MESS.

       EXIT-PROCESS-PRIOR-LOAN.
           EXIT.

      ********************************************
       VALIDATION SECTION.
      ********************************************
           IF ELE = 20
              IF PD-FEEAMT(1) > CEP-FEE-CHG(1)
                 MOVE SP-FEEID(1) TO TYPE-OF-AMT-EXCEEDED
                 MOVE CEP-FEE-CHG(1) TO HOLD-AMT-EXCEEDED
                 GO TO VALIDATE-EXCEEDS-MAX.
           IF ELE = 21
              IF PD-FEEAMT(2) > CEP-FEE-CHG(2)
                 MOVE SP-FEEID(2) TO TYPE-OF-AMT-EXCEEDED
                 MOVE CEP-FEE-CHG(2) TO HOLD-AMT-EXCEEDED
                 GO TO VALIDATE-EXCEEDS-MAX.
           IF ELE = 26
              IF PD-OVRDRATE-FG = "E"
                         AND PD-OVRDRATE > CEP-EFF-RATE
                 MOVE "EFF. RATE" TO TYPE-OF-AMT-EXCEEDED
                 MOVE CEP-EFF-RATE TO HOLD-RATE-EXCEEDED
                 GO TO VALIDATE-EXCEEDS-MAX.
           IF ELE = 26
              IF PD-OVRDRATE-FG = "A"
                         AND PD-OVRDRATE > CEP-APR-RATE
                 MOVE "APR RATE" TO TYPE-OF-AMT-EXCEEDED
                 MOVE CEP-APR-RATE TO HOLD-RATE-EXCEEDED
                 GO TO VALIDATE-EXCEEDS-MAX.
           IF ELE = 27 OR 28 OR 29 OR 30
              SUBTRACT 26 FROM ELE GIVING I-LEVEL
              IF PD-INSTRM(I-LEVEL) > CEP-INS-TERM(I-LEVEL)
                MOVE "INS. TERM" TO TYPE-OF-AMT-EXCEEDED
                MOVE CEP-INS-TERM(I-LEVEL) TO HOLD-TERM-EXCEEDED
                GO TO VALIDATE-EXCEEDS-MAX
              ELSE
              IF PD-INSPREM(I-LEVEL) > CEP-INS-PREMIUM(I-LEVEL)
                MOVE "INS. PREMIUM" TO TYPE-OF-AMT-EXCEEDED
                MOVE CEP-INS-PREMIUM(I-LEVEL) TO HOLD-AMT-EXCEEDED
                GO TO VALIDATE-EXCEEDS-MAX
              ELSE
      * PERSONAL PROPERTY MUST CHECK SP-INSMAXCHGABL FOR 888888
              IF ELE NOT = 29 AND NOT = 30
                IF PD-INSCOVR(I-LEVEL) > CEP-INS-COVERAGE(I-LEVEL)
                  MOVE "INS. COVERAGE" TO TYPE-OF-AMT-EXCEEDED
                  MOVE CEP-INS-COVERAGE(I-LEVEL) TO HOLD-AMT-EXCEEDED
                  GO TO VALIDATE-EXCEEDS-MAX
                ELSE
                  NEXT SENTENCE
              ELSE
                PERFORM VALIDATE-TEST-PP
      * PERSONAL PROPERTY OF 888888, SKIP MAX TEST
                IF NOT SP-INSMAXCHGABL-OVRIDE(ILEV)
                 IF PD-INSCOVR(I-LEVEL) > CEP-INS-COVERAGE(I-LEVEL)
                  MOVE "INS. COVERAGE" TO TYPE-OF-AMT-EXCEEDED
                  MOVE CEP-INS-COVERAGE(I-LEVEL) TO HOLD-AMT-EXCEEDED
                  GO TO VALIDATE-EXCEEDS-MAX.

           IF ELE = 34
              IF PD-SERCHG > CEP-SERVICE-CHG
                 MOVE "SERVICE CHG" TO TYPE-OF-AMT-EXCEEDED
                 MOVE CEP-SERVICE-CHG TO HOLD-AMT-EXCEEDED
                 GO TO VALIDATE-EXCEEDS-MAX.
           IF ELE = 35
              IF SP-MFFRMLA-05
                 MOVE SAVE-MAINTFEE TO AMT-HOLDER
              ELSE
                 MULTIPLY PD-MAINTFEE BY PD-ORGTERM GIVING AMT-HOLDER
                    ROUNDED
              END-IF
              IF AMT-HOLDER > CEP-MAINT-FEES
                 MOVE "MAINT. FEE" TO TYPE-OF-AMT-EXCEEDED
                 MOVE CEP-MAINT-FEES TO HOLD-AMT-EXCEEDED
                 GO TO VALIDATE-EXCEEDS-MAX.
           IF ELE = 36
              IF PD-EXTCHG > CEP-1ST-PY-EXT
                 MOVE "EXT. CHG" TO TYPE-OF-AMT-EXCEEDED
                 MOVE CEP-1ST-PY-EXT TO HOLD-AMT-EXCEEDED
                 GO TO VALIDATE-EXCEEDS-MAX.

           GO TO EXIT-VALIDATION.

       VALIDATE-TEST-PP.
           IF PD-INSTYPES-PP
              SET ILEV TO 6
           ELSE
              SET ILEV TO 7.
           IF ELE = 30
              SET ILEV TO 8.

       VALIDATE-EXCEEDS-MAX.
           UNSTRING AMT-EXCEEDED-LIT DELIMITED BY ALL " "
              INTO MESS AMT-EXCEEDED-LIT.
           STRING TYPE-OF-AMT-EXCEEDED DELIMITED BY "  "
              " EXCEEDS CURRENT MAXIMUM OF ",
              AMT-EXCEEDED-LIT DELIMITED BY SIZE INTO MESS.
           PERFORM SEND-MESS.
           MOVE " " TO MESS AMT-EXCEEDED-LIT.
           MOVE 9 TO IO-FG.

       EXIT-VALIDATION.
           EXIT.

      ********************************************
       W12EQ2-WINDOW SECTION.
      ********************************************

      * FORMULA 12 LIKE 2 BUT SPREADS 1STPAY EXTENSION CHARGE OVER PAYMENTS
      * CITIZENS #2006

           MOVE PD-OVRDRATE         TO W12EQ2-OVRDRATE.
           MOVE PD-OVRDRATE-FG      TO W12EQ2-OVRDRATE-FG.
           MOVE PD-EFFRATE          TO W12EQ2-EFFRATE.
           MOVE PD-EXTCHG           TO W12EQ2-EXTCHG.
    
           MOVE PD-ORGTERM          TO W12EQ2-ORGTERM.
           MOVE PD-1STPYAMT         TO W12EQ2-1STPYAMT.
           MOVE PD-REGPYAMT         TO W12EQ2-REGPYAMT.
           MOVE PD-LASTPYAMT        TO W12EQ2-LASTPYAMT.
           PERFORM GET-W12EQ2-NEW-PAYMENTS.
    
           MOVE PD-APR-FINAMT       TO APR-FINANCED.
           MOVE PD-INTDATE          TO APR-INTDATE.
           MOVE PD-1STPYDATE        TO APR-1STPYDATE.
           MOVE PD-ORGTERM          TO APR-ORGTERM.
           MOVE W12EQ2-1STPYAMT     TO APR-1STPYAMT.
           MOVE W12EQ2-REGPYAMT     TO APR-REGPYAMT.
           MOVE W12EQ2-LASTPYAMT    TO APR-LASTPYAMT.
           MOVE PD-UNITPER-CD       TO APR-UNITPER-CD.
           MOVE PD-UNITPER-FREQ     TO APR-UNITPER-FREQ.
           MOVE SP-DISFRMLA         TO APR-DISFRMLA.
           PERFORM APR-REGZ.
           MOVE APR-APR             TO W12EQ2-APRATE.
           MOVE "WI/W12EQ2"  TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH
                                W12EQ2-BUF.
           CANCEL FORM-PROGX.
    
       W12EQ2-WINDOW-EXIT.
           EXIT.

      *****************************
       SP-EDT-TABLE-LOOKUP SECTION.
      *****************************
           SET ETBL TO 1.
           MOVE 0 TO COUNTER.
           IF ELE = 14 OR 19 OR 20 OR 21
              MOVE 999 TO TERM-LOW
              MOVE 999999.99 TO PROCEEDS-LOW
              MOVE 0 TO TERM-HIGH PROCEEDS-HIGH
              COMPUTE AMT-HOLDER ROUNDED =
                   PRIOR-LNAMT + PD-PROCEEDS + TOTAL-GL-HLDR
              GO TO PROCEEDS-VALIDATION.
           IF ELE = 18
              MOVE 999999.99 TO POINTS-LOW
              GO TO POINTS-VALIDATION.

           IF PD-ORGTERM < 1
              GO TO OUT-OF-RANGE-MSG.

       TERM-VALIDATION.
           ADD 1 TO COUNTER.

           IF COUNTER > SP-EDT-NOEDITS(ETBL)
              GO TO TERM-TABLE2.

           IF SP-EDT-TYPE(ETBL,COUNTER) = "T"
              IF SP-EDT-HL(ETBL,COUNTER) = "L"
                 IF SP-EDT-TERM(ETBL,COUNTER) < TERM-LOW
                    MOVE SP-EDT-TERM(ETBL,COUNTER) TO TERM-LOW
                 ELSE
                    NEXT SENTENCE
              ELSE
                 IF SP-EDT-TERM(ETBL,COUNTER) > TERM-HIGH
                    MOVE SP-EDT-TERM(ETBL,COUNTER) TO TERM-HIGH.

           IF COUNTER NOT > SP-EDT-NOEDITS(ETBL)
              GO TO TERM-VALIDATION.

       TERM-TABLE2.
           IF (SP-EDT-NOEDITS(ETBL) NOT < 6) AND (ETBL NOT = 2)
              MOVE 0 TO COUNTER
              SET ETBL TO 2
              GO TO TERM-VALIDATION.

           IF ((PD-ORGTERM < TERM-LOW) AND (TERM-LOW NOT = 999))
                  OR
              ((PD-ORGTERM > TERM-HIGH) AND (TERM-HIGH NOT = 0))
              GO TO OUT-OF-RANGE-MSG.

           GO TO EXIT-SP-EDT-TABLE-LOOKUP.

       POINTS-VALIDATION.
           ADD 1 TO COUNTER.

           IF COUNTER > SP-EDT-NOEDITS(ETBL)
              GO TO POINTS-TABLE2.

           IF SP-EDT-TYPE(ETBL COUNTER) = "P"
              IF SP-EDT-HL(ETBL COUNTER) = "L"
                 IF SP-EDT-AMT(ETBL COUNTER) < POINTS-LOW
                    MOVE SP-EDT-AMT(ETBL COUNTER) TO POINTS-LOW
                 END-IF
              ELSE
              IF SP-EDT-HL(ETBL COUNTER) = "H"
                 IF SP-EDT-AMT(ETBL COUNTER) > POINTS-HIGH
                    MOVE SP-EDT-AMT(ETBL COUNTER) TO POINTS-HIGH.

           IF COUNTER NOT > SP-EDT-NOEDITS(ETBL)
              GO TO POINTS-VALIDATION.

       POINTS-TABLE2.
           IF SP-EDT-NOEDITS(ETBL) NOT < 6 AND ETBL NOT = 2
              MOVE 0 TO COUNTER
              SET ETBL TO 2
              GO TO POINTS-VALIDATION.

           IF ((PD-POINTS < POINTS-LOW) AND
                                      (POINTS-LOW NOT = 999999.99))
                    OR
              ((PD-POINTS > POINTS-HIGH) AND (POINTS-HIGH NOT = 0))
              GO TO POINTS-OUT-OF-RANGE-MSG.

           MOVE 0 TO COUNTER.
           GO TO EXIT-SP-EDT-TABLE-LOOKUP.

       POINTS-OUT-OF-RANGE-MSG.
           MOVE "THE NO. OF POINTS IS OUT OF SP RANGE" TO MESS.
           PERFORM SEND-MESS.
           MOVE 9 TO IO-FG.
           GO TO EXIT-SP-EDT-TABLE-LOOKUP.

       PROCEEDS-VALIDATION.
           ADD 1 TO COUNTER.

           IF COUNTER > SP-EDT-NOEDITS(ETBL)
              GO TO PROCEEDS-TABLE2.

      *--->IF SP-EDT-TYPE(ETBL,COUNTER) = "A"
      *       IF SP-EDT-HL(ETBL,COUNTER) = "L"
      *          IF SP-EDT-AMTYPE(ETBL,COUNTER) = "P"
      *             IF SP-EDT-AMT(ETBL,COUNTER) < PROCEEDS-LOW
      *--------------->MOVE SP-EDT-AMT(ETBL,COUNTER) TO PROCEEDS-LOW.
           IF SP-EDT-TYPE(ETBL,COUNTER) = "A"
              IF SP-EDT-HL(ETBL,COUNTER) = "H"
      *--------->IF SP-EDT-AMTYPE(ETBL,COUNTER) = "P"
                    IF SP-EDT-AMT(ETBL,COUNTER) > PROCEEDS-HIGH
                       MOVE SP-EDT-AMT(ETBL,COUNTER) TO PROCEEDS-HIGH.

           IF COUNTER NOT > SP-EDT-NOEDITS(ETBL)
              GO TO PROCEEDS-VALIDATION.

       PROCEEDS-TABLE2.
           IF SP-EDT-NOEDITS(ETBL) NOT < 6 AND ETBL NOT = 2
              MOVE 0 TO COUNTER
              SET ETBL TO 2
              GO TO PROCEEDS-VALIDATION.
      *--->IF ((AMT-HOLDER < PROCEEDS-LOW)
      *                 AND (PROCEEDS-LOW NOT = 999999.99))
      *---------->OR ((AMT-HOLDER > PROCEEDS-HIGH)
           IF ((AMT-HOLDER > PROCEEDS-HIGH)
                               AND (PROCEEDS-HIGH NOT = 0))
              GO TO PROCEEDS-OUT-OF-RANGE-MSG.

           MOVE 0 TO AMT-HOLDER COUNTER.
           GO TO EXIT-SP-EDT-TABLE-LOOKUP.

       PROCEEDS-OUT-OF-RANGE-MSG.
           MOVE "PRIOR LOAN BAL + PROCEEDS + G/L FEES OUT OF SP RANGE"
                                                               TO MESS.
           PERFORM SEND-MESS.
           MOVE 9 TO IO-FG.
           GO TO EXIT-SP-EDT-TABLE-LOOKUP.

       OUT-OF-RANGE-MSG.
           MOVE "VALUE OUT OF RANGE SET IN SP RECORD" TO MESS.
           PERFORM SEND-MESS.
           MOVE 9 TO IO-FG.

       EXIT-SP-EDT-TABLE-LOOKUP.
           EXIT.

      *****************************
       DISPLAY-PROMPTS SECTION.
      *****************************
           IF PROMPT-FG = "Y"
              GO TO DISPLAY-PROMPTS-EXIT.

           MOVE "Y" TO PROMPT-FG.
           IF SP-FEEFRMLA(1) = " "
              MOVE " " TO PROMPT-200
                          PROMPT-201
           ELSE
              MOVE "20" TO PROMPT-200
              STRING SP-FEEID(1) "     :" DELIMITED BY SIZE
                         INTO PROMPT-201.

           IF SP-FEEFRMLA(2) = " "
              MOVE " " TO PROMPT-210
                          PROMPT-211
           ELSE
              MOVE "21" TO PROMPT-210
              STRING SP-FEEID(2) "     :" DELIMITED BY SIZE
                          INTO PROMPT-211.


           MOVE PROMPT-BUF TO WR-BUF.
           MOVE PROMPT-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       DISPLAY-PROMPTS-EXIT.
           EXIT.


      ******************************************
       CALL-LOAN-CALCULATIONS SECTION.
      ******************************************
           MOVE PD-UNITPER-CD TO CIP-UNITPER-CD.
           MOVE PD-UNITPER-FREQ TO CIP-UNITPER-FREQ.
           MOVE PD-BALLOON-LAST-FG TO CIP-BALLOON-LAST-FG.
           PERFORM MATURITY-DATE-CALCULATION.
           MOVE NDTE-DATE TO CIP-MATURITY-DATE.

           MOVE PD-SELLPR-FG TO CIP-SELLPR-FG.
           MOVE "I" TO CIP-DOWNPY-FG
                       CIP-PERDOWN-FG.
           MOVE PD-SELLPR TO CIP-SELLPR.
           MOVE 0 TO CIP-DOWNPY
                     CIP-PERDOWN.
           MOVE TOTAL-GL-HLDR TO CIP-GL-AMOUNT-TOTAL.
           MOVE TOTAL-GL-FINCHG-HLDR TO CIP-GL-AMOUNT-TOTAL-FINCHG.

           MOVE "CALCULATING - PLEASE WAIT" TO LEGEND.
           PERFORM SEND-LEGEND.

      * SETUP VERIFY-REJECT-TERMS RE: SP-INSREJECT-FG = "C":
           PERFORM ESTABLISH-REJECT-TERMS.

           MOVE CAN-LOAN-NO TO CIP-CAN-LOAN-NO.
      *----------------------------------------------------------
      *    ENVIRONMENT VARIABLE 'CALL_TESTZ3' IS USED TO CALL
      *    ANOTHER VERSION OF CALCZ3,
      *    NOTE:
      *          HELPFULL WHEN TRYING TO RUN CALCZ3.C IN DEBUG
      *          FROM TWO TERMINALS AT THE SAME TIME.
      *          THE DEBUGGER DOSN'T ACT CORRECTLY WITH TWO
      *          SESSIONS RUN TOGETHER.
      *          OR
      *          WHEN MAKING LARGE CHANGES TO CALCZ3.C
      *
      *    MOVE "CALL_TESTZ3" TO GETENV-BUF.
      *    PERFORM GETENV-CALL.
      *    IF STAT = "00"
      *       MOVE "LP/TESTZ3" TO FORM-NAM
      *    ELSE
      *-------------------------------------------------------------

           MOVE "LP/CALCZ3" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH
                                CIP-CALCULATION-INPUT-AREA
                                CEP-CALCULATION-EDIT-AREA
                                COP-CALCULATION-OUTPUT-AREA
                                SP-REC
                                BR-REC STATE CITY-TAX-CD COUNTY.
           CANCEL FORM-PROGX.

           MOVE COP-PRLOAN-RESTRICTED TO PRLOAN-RESTRICTED.
           MOVE COP-PROCEEDS TO PD-PROCEEDS
                                PROCEEDS-HLDR.
      *
      ****************************************************************
           MOVE COP-SELLPR-FG TO PD-SELLPR-FG.
           MOVE "I" TO PD-DOWNPY-FG.
           MOVE "I" TO PD-PERDOWN-FG.
           MOVE COP-SELLPR TO PD-SELLPR.
           MOVE 0 TO PD-DOWNPY.
           MOVE 0 TO PD-PERDOWN.

           MOVE COP-PROCEEDS-SW TO PD-PROCEEDS-FG
                                   PROCEEDS-FG-HLDR
                                   DF2-PROCEEDS-FG.

           MOVE PD-SELLPR TO DF2-SELLPR
                             SELLPR-HLDR.
           MOVE PD-SELLPR-FG TO DF2-SELLPR-FG.

      *    MOVE PD-DOWNPY TO DF2-DOWNPY
      *                      DOWNPY-HLDR.
      *    MOVE PD-DOWNPY-FG TO DF2-DOWNPY-FG.
      *    MOVE PD-PERDOWN TO DF2-PERDOWN
      *                       PERDOWN-HLDR.
      *    MOVE PD-PERDOWN-FG TO DF2-PERDOWN-FG.
      *    IF PD-PERDOWN NOT = 0
      *       MOVE "%" TO DF2-PERDOWNP
      *    ELSE
      *       MOVE " " TO DF2-PERDOWNP.

           MOVE COP-BALLOON TO DF2-BALLOON.
           MOVE COP-BALLOON-SW TO PD-BALLOON-FG
                                 DF2-BALLOON-FG.

           MOVE COP-POINTS TO PD-POINTS DF2-POINTS.
           MOVE COP-POINTS-SW TO PD-POINTS-FG DF2-POINTS-FG.
           IF COP-POINTS = 0
              MOVE " " TO DF2-POINTS-FG.

           MOVE COP-OVR-RATE TO PD-OVRDRATE DF2-OVRDRATE.
           MOVE COP-OVR-RATE-SW TO PD-OVRDRATE-FG DF2-OVRDRATE-FG.
           MOVE COP-EFF-RATE TO PD-EFFRATE DF2-EFFRATE.
           MOVE COP-SIM-RATE TO PD-SMPRATE.
           MOVE COP-APR-RATE TO PD-APRATE DF2-APRATE.
           MOVE COP-TOT-REPYBLE TO DF2-TREPAY.
           PERFORM MOVE-INSURANCE-TO-PDREC
                VARYING COUNTER FROM 1 BY 1
                   UNTIL COUNTER > 4.

           IF SP-FEEFRMLA(1) = " "
              MOVE 0 TO DF2-FEE-1
              MOVE " " TO DF2-FEE-1-FG
           ELSE
              MOVE SP-FEECD(1) TO PD-FEECODE(1)
              MOVE COP-FEE-CHG(1) TO PD-FEEAMT(1)
                                     DF2-FEE-1
              MOVE COP-FEE-CHG-SW(1) TO PD-FEECD-FG(1)
                                        DF2-FEE-1-FG
              IF NOT (BR-OUR-FEE = "Y" OR "N")
                 MOVE "Y" TO PD-FEEOURS(1)
              ELSE
                 MOVE BR-OUR-FEE TO PD-FEEOURS(1).

           IF SP-FEEFRMLA(2) = " "
              MOVE 0 TO DF2-FEE-2
              MOVE " " TO DF2-FEE-2-FG
           ELSE
              MOVE SP-FEECD(2) TO PD-FEECODE(2)
              MOVE COP-FEE-CHG(2) TO PD-FEEAMT(2)
                                     DF2-FEE-2
              MOVE COP-FEE-CHG-SW(2) TO PD-FEECD-FG(2)
                                        DF2-FEE-2-FG
              IF NOT (BR-OUR-FEE = "Y" OR "N")
                 MOVE "Y" TO PD-FEEOURS(2)
              ELSE
                 MOVE BR-OUR-FEE TO PD-FEEOURS(2).

           MOVE COP-SERVICE-CHG TO PD-SERCHG
                                   PD-SUM-SERCHG
                                   DF2-SERCHG.
           MOVE COP-STATE-WORKER TO PD-STATE-WORKER.
           MOVE COP-SERVICE-CHG-SW TO PD-SERCHG-FG DF2-SERCHG-FG.
           MOVE COP-MAINT-FEES TO DF2-MAINTFEE MAINTFEE-HLDR.
           DIVIDE COP-MAINT-FEES BY COP-TERM GIVING
              PD-MAINTFEE ROUNDED.
           MOVE PD-MAINTFEE TO PD-SUM-MAINTFEE.
           MOVE COP-MAINT-FEES-SW TO PD-MAINTFEE-FG DF2-MAINTFEE-FG.
           MOVE COP-1ST-PY-EXT TO PD-EXTCHG
                                  PD-SUM-EXTCHG
                                  DF2-EXTCHG.

           MOVE COP-1ST-PY-EXT-SW TO PD-EXTCHG-FG DF2-EXTCHG-FG.
           MOVE COP-INT-CHARGE TO PD-INTCHG
                                  PD-SUM-INTCHG
                                  DF2-INTCHG.
           MOVE COP-APR-FINANCD TO PD-APR-FINAMT DF2-APR-FINAMT.
           MOVE COP-AMT-FINANCD TO PD-FINAMT.
           MOVE COP-LOAN-AMOUNT TO PD-LNAMT
                                   PD-SUM-LNAMT
                                   PD-PURBAL
                                   PD-ORIGPURAMT
                                   PD-SUM-ORIGPURAMT
                                   DF2-LNAMT.
           IF PD-LOANTYPE = "I"
              MOVE PD-SERCHG TO PD-ANTICERN(3)
              MOVE 0 TO PD-ANTICERN(1)
                        PD-ANTICERN(2)
                        PD-ANTICERN(4)
           ELSE
              ADD COP-INT-CHARGE COP-1ST-PY-EXT GIVING PD-ANTICERN(1)
              MOVE COP-SERVICE-CHG TO PD-ANTICERN(3)
              MOVE COP-MAINT-FEES TO PD-ANTICERN(4).
           MOVE 0 TO PD-ANTICERN(5) PD-ANTICERN(6).

            MOVE PD-ANTICERN(1) TO WK-ANTICERN(1).
            MOVE PD-ANTICERN(2) TO WK-ANTICERN(2).
            MOVE PD-ANTICERN(3) TO WK-ANTICERN(3).
            MOVE PD-ANTICERN(4) TO WK-ANTICERN(4).
            MOVE PD-ANTICERN(5) TO WK-ANTICERN(5).
            MOVE PD-ANTICERN(6) TO WK-ANTICERN(6).
            PERFORM ADJUST-ANTICERN.
            MOVE WK-ANTICERN(1) TO PD-ANTICERN(1).
            MOVE WK-ANTICERN(2) TO PD-ANTICERN(2).
            MOVE WK-ANTICERN(3) TO PD-ANTICERN(3).
            MOVE WK-ANTICERN(4) TO PD-ANTICERN(4).
            MOVE WK-ANTICERN(5) TO PD-ANTICERN(5).
            MOVE WK-ANTICERN(6) TO PD-ANTICERN(6).

           MOVE COP-REGPYAMT TO PD-REGPYAMT
                                PD-SUM-REGPYAMT
                                DF2-REGPYAMT.
           MOVE COP-REGPYAMT-SW TO PD-REGPYAMT-FG
                                   DF2-REGPYAMT-FG.

           IF COP-ODD-1ST-PMT = 0
              MOVE PD-REGPYAMT TO PD-1STPYAMT
                                  DF2-1STPYAMT
           ELSE
              MOVE COP-ODD-1ST-PMT TO PD-1STPYAMT
                                      DF2-1STPYAMT.

           IF COP-BALLOON-LAST-FG = "L"
              MOVE COP-BALLOON TO PD-LASTPYAMT
                                 DF2-LASTPYAMT
           ELSE
      *       DEFENSE IF
              IF PD-ORGTERM = 1
                 MOVE 0 TO PD-LASTPYAMT DF2-LASTPYAMT
              ELSE
                 ADD COP-REGPYAMT COP-BALLOON GIVING PD-LASTPYAMT
                                                     DF2-LASTPYAMT.

           SUBTRACT 1 FROM PD-ORGTERM GIVING PD-NOREGPY.
           IF PD-LASTPYAMT NOT = 0
              SUBTRACT 1 FROM PD-NOREGPY.

           MOVE COP-FINCHG TO PD-FINCHG
                              PD-SUM-FINCHG.
           MOVE COP-RETURN-CODE TO DF2-RETURN-CODE.
           MOVE COP-LOOP-COUNT TO DF2-LOOP-COUNT.

           IF COP-1ST-PMT-MTHS = 1 AND
              COP-1ST-PMT-DAYS = 0
              MOVE "Y" TO PD-ONE-MTH-NO-DAY-EXT-SW
           ELSE
              MOVE " " TO PD-ONE-MTH-NO-DAY-EXT-SW.

           PERFORM DISPLAY-PROMPTS.
           COMPUTE ANTIC = PD-ANTICERN(1) + PD-ANTICERN(2)
                         + PD-ANTICERN(3) + PD-ANTICERN(4)
                         + PD-ANTICERN(5).
           IF NOT GP-INCL-OTH-IN-EARNED-N
              ADD PD-ANTICERN(6) TO ANTIC.
           MOVE ANTIC TO DF2-ANTIC.

           PERFORM COMPUTE-INSCOMM
                VARYING I-LEVEL FROM 1 BY 1
                    UNTIL I-LEVEL > 8.

           MOVE DF2-BUF TO WR-BUF.
           MOVE DF2-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

           PERFORM RESET-LEGEND.
           MOVE COP-CALCULATION-OUTPUT-AREA TO
                CIP-CALCULATION-INPUT-AREA.
           PERFORM VERIFY-SETUP-PAYMENTS.
           IF NOREG-FG NOT = " "
              PERFORM ERROR-IN-CALCULATIONS-MSG
           ELSE
              IF COP-CALCULATION-OK
                 PERFORM VERIFY-AGAINST-SPEDT
              ELSE
                 PERFORM ERROR-IN-CALCULATIONS-MSG.

           MOVE 0 TO IO-FG.

      ***********************************************
       MAKE-DATES-CURRENT SECTION.
      ***********************************************

      * SET NEW INTEREST DATE:
           MOVE PD-LOANDATE TO NUM-DATE.
           MOVE PD-INTDATE  TO SYS-DATE.
           PERFORM TIM.
           MOVE ELAPSED-DAYS        TO NDTE-HOLD.
           MOVE TRANS-DATE-YYYYMMDD TO NDTE-DATE.
           PERFORM INCREMENT-DAYS.
           MOVE NDTE-DATE TO PD-INTDATE DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO D-061.

      * SET NEW LOAN DATE:
           MOVE TRANS-DATE-YYYYMMDD TO PD-LOANDATE DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO D-060.

      * SET NEW 1ST PAY DATE:
           MOVE PD-INTDATE TO NDTE-DATE
                              SYS-DATE.
           MOVE 1 TO NDTE-HOLD.
           MOVE PD-UNITPER-CD TO DATER-UNITPER-CD.
           MOVE PD-UNITPER-FREQ TO DATER-UNITPER-FREQ.
           PERFORM INCREMENT-UNITPER.
           MOVE NDTE-DATE TO NUM-DATE.

      * WORLD OPTION TO ADVANCE TO 1ST OF NEXT MONTH
      * IF NO CORRESPONDING DAY IN NEW MONTH:
           PERFORM SET-ADV-TO-1ST-NO-CORR.
           IF ADV-TO-1ST-NO-CORR = "Y"
              IF NOT ( PD-ORGST   = "TN" AND
                       PD-CLASS   = 01   AND
                       PD-ORGTERM = 12 )
                 IF PD-UNITPER-CD = "M" AND PD-UNITPER-FREQ = 1
                    IF S-DD > NDTE-DD
                       MOVE 1 TO NDTE-DD
                                 NDTE-HOLD
                       PERFORM INCREMENT-MONTHS
                       MOVE NDTE-DATE TO NUM-DATE.
      * WORLD OPTION TO ADVANCE TO 1ST OF NEXT MONTH
      * IF NO CORRESPONDING DAY IN NEW MONTH:

      * IF DATE OF THE LOAN IS ON THE 26 THRU 31 THEN ADVANCE THE
      *   FIRST PAYMENT DUE DATE TO THE FIRST OF THE SECOND MONTH
      *   FOLLOWING THE DATE OF THE LOAN. (REGMGM PR#1099)
           IF ADV-TO-1ST-NO-CORR = "A"
              IF PD-UNITPER-CD = "M" AND PD-UNITPER-FREQ = 1
                 IF S-DD > 25
                    MOVE 1 TO NDTE-DD
                              NDTE-HOLD
                    PERFORM INCREMENT-MONTHS
                    MOVE NDTE-DATE TO NUM-DATE.

      * TEST FOR ORIGINAL SAVED QUOTE HAVING AN EXTENDED FIRST PAY
           IF NUM-DATE > PD-1STPYDATE
              MOVE NUM-DATE TO PD-1STPYDATE
                               PD-ORIG-1STPYDATE.
           MOVE PD-1STPYDATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO D-130.

      * SET INSURANCE EXPIRATION DATES, IF NECESSARY
      * 06/28/96 BAH
           MOVE 5 TO SUB.
       MAKE-NEXT-INS-CURRENT.
           IF PD-INSEXP-DATE(SUB) NOT = 0 AND
               PD-INSPREM(SUB) NOT = 0
                 MOVE PD-INTDATE TO NDTE-DATE
                 MOVE PD-ORGTERM TO NDTE-HOLD
                 PERFORM INCREMENT-MONTHS
                 MOVE "SNNN060  2." TO VDU
                 COMPUTE VDUROW = SUB + 26
                 MOVE NDTE-DATE TO PD-INSEXP-DATE(SUB) DATE-YYYYMMDD
                 PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
                 MOVE DATE-MMDDYY TO VDUNUM0
                 PERFORM SCREENIO.
           IF SUB < 6
              ADD 1 TO SUB
              GO TO MAKE-NEXT-INS-CURRENT.


      **********************************************
       TEST-FOR-GEORGIA-SCHG SECTION.
      **********************************************
           IF QUOTE-ONLY AND PD-SSNO(1) = 0
              GO TO TEST-FOR-GEORGIA-SCHG-EXIT.

      * WORLD 1154
           IF SP-SCFRMLA = 58
              GO TO TEST-FOR-TEXAS-SCHG.

           IF (SP-SCFRMLA = 4 OR 16 OR 29 OR 47 OR 49 OR 52 OR 53 OR 59)
              GO TO TEST-FOR-CALIF-SCHG.

           IF (SP-SCFRMLA = 23 OR 43 OR 45 OR 48 OR 50 OR 55
                                OR 56 OR 57)
              GO TO TEST-FOR-NC-SCHG.

           IF NOT (SP-SCFRMLA = 12 OR 13 OR 54)
              GO TO TEST-FOR-GEORGIA-SCHG-EXIT.

           MOVE " " TO SCHG-UPDATE-FG.
           MOVE 0 TO SCHG-NEW-ACCTNO.
           MOVE PD-SSNO(1) TO SCHG-LN-SSNO(1).
           MOVE PD-SSNO(2) TO SCHG-LN-SSNO(2).
           MOVE PD-SSNO(3) TO SCHG-LN-SSNO(3).
           MOVE PD-SSNO(4) TO SCHG-LN-SSNO(4).
           MOVE TRANS-DATE-YYYYMMDD TO SCHG-TRANS-DATE.
           IF PD-SRCD = "PB" OR "RN" OR "SC" OR "RB" OR "RO"
              MOVE PD-PRLNNO TO SCHG-PRIOR-ACCTNO
           ELSE
              MOVE 0 TO SCHG-PRIOR-ACCTNO.
           MOVE PD-ORGST TO SCHG-ORGST.
           MOVE PD-CLASS TO SCHG-LN-CLASS.
           MOVE SP-SCFRMLA TO GA-SCHG-NO.
           MOVE GA-SCHG-PATH TO FORM-NAM.
           IF GA-SCHG-NO = 13
              CALL FORM-PROGX USING FORM-PATH SCHG-WORKERS
                                             SCHG-WORKERS-2
           ELSE
              CALL FORM-PROGX USING FORM-PATH SCHG-WORKERS.
           CANCEL FORM-PROGX.
           MOVE SCHG-RESTRICT-ADJ  TO CIP-RESTRICT-ADJ
                                      PD-RESTRICT-ADJ.
           MOVE SCHG-DUAL-LOAN-ADJ TO CIP-DUAL-LOAN-ADJ
                                      PD-DUAL-LOAN-ADJ.
           MOVE SCHG-DUAL-LIAB TO SPDUAL-LIAB.

           GO TO TEST-FOR-GEORGIA-SCHG-EXIT.

      ***************************************************************
      * WORLD, TX CLASS 5 (58)
      ***************************************************************
       TEST-FOR-TEXAS-SCHG.
           IF NOT (SP-SCFRMLA = 58)
              GO TO TEST-FOR-GEORGIA-SCHG-EXIT.

            MOVE SP-SCFRMLA   TO TXSCHG-SCFRMLA.
            IF PD-SRCD = "PB" OR "RN" OR "SC" OR "RB" OR "RO"
               MOVE PD-PRLNNO TO TXSCHG-PRIOR-ACCTNO
            ELSE
               MOVE 0         TO TXSCHG-PRIOR-ACCTNO.
            MOVE PD-LOANDATE  TO TXSCHG-LOANDATE.
            MOVE PD-SSNO(1)   TO TXSCHG-SSNO.
            MOVE PD-ORGST     TO TXSCHG-ORGST.
            MOVE PD-CLASS     TO TXSCHG-CLASS.
            MOVE " "          TO TXSCHG-RESTRICTED.

            MOVE "CL/TXSCHG" TO FORM-NAM.
            CALL FORM-PROGX USING FORM-PATH TXSCHG-WORKERS.
            CANCEL FORM-PROGX.

      *  USE CIP-RESTRICT-ADJ TO FLAG CALCZL THAT YOU CANT TAKE
      *  ANOTHER SERVICE CHARGE IF ANYTHING IN HERE

            IF TXSCHG-RESTRICTED = "Y"
      *  RESTRICTED FLAG = "Y" MEANS THAT YOU CANT TAKE ANOTHER CHARGE
                MOVE 50.00  TO CIP-RESTRICT-ADJ
            ELSE
                MOVE 0      TO CIP-RESTRICT-ADJ.

            GO TO TEST-FOR-GEORGIA-SCHG-EXIT.


      * COSMO, CA 2-8-95 (4)
      * MULLEN, CA 12-11-95 (16)
      * MULLEN, CA 12-30-97 (29)
      * BAYC, MD   11-09-11 (47) ADDED 12 MONTH CHECK
      * REGMGM, TX 07-30-07 (49)
      * MULLEN, CA 06-02-11 (52,53)
      * WORLD,  KY 05-29-19 (59)
       TEST-FOR-CALIF-SCHG.
           IF NOT (SP-SCFRMLA = 4 OR 16 OR 29 OR 47 OR 49 OR 52 OR 53 OR
                                                             59)
               GO TO TEST-FOR-GEORGIA-SCHG-EXIT.

           MOVE SP-SCFRMLA   TO CASCHG-SCFRMLA.
           IF PD-SRCD = "PB" OR "RN" OR "SC" OR "RB" OR "RO"
              MOVE PD-PRLNNO TO CASCHG-PRIOR-ACCTNO
           ELSE
              MOVE 0         TO CASCHG-PRIOR-ACCTNO.
           MOVE PD-LOANDATE  TO CASCHG-LOANDATE.
           MOVE " "          TO CASCHG-RESTRICTED.

           MOVE "CL/CASC16" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH CASCHG-WORKERS.
           CANCEL FORM-PROGX.
      * USE CIP-RESTRICT-ADJ TO FLAG CALCZ3 THAT YOU CANT TAKE
      * ANOTHER SERVICE CHARGE IF ANYTHING IN HERE
           IF CASCHG-RESTRICTED = "Y"
      * RESTRICTED FLAG = "Y" MEANS THAT YOU CANT TAKE ANOTHER CHARGE
              MOVE 50.00  TO CIP-RESTRICT-ADJ
           ELSE
              MOVE 0 TO CIP-RESTRICT-ADJ.

           GO TO TEST-FOR-GEORGIA-SCHG-EXIT.

       TEST-FOR-NC-SCHG.
           IF NOT (SP-SCFRMLA = 23 OR 43 OR 45 OR 48 OR 50
                                    OR 55 OR 56 OR 57)
              GO TO TEST-FOR-GEORGIA-SCHG-EXIT.

           MOVE 0 TO NCSCHG-LN-ACCTNO.

           IF SP-SCFRMLA = 48
              IF PD-SRCD = "PB" OR "RN" OR "SC" OR "RB" OR "RO"
                 MOVE PD-PRLNNO TO NCSCHG-LN-ACCTNO.

           IF SP-SCFRMLA = 50
              IF PD-SRCD = "PB" OR "RN" OR "SC" OR "RB" OR "RO"
                 MOVE PD-PRLNNO TO NCSCHG-LN-ACCTNO
              ELSE
      * IF SCFRMLA 50 AND NOT A RENEWAL, THEN NO RESTRICTION TEST
      *   IS EVEN NECESSARY, SO SKIP IT, SCHG WILL BE $100
                 MOVE 100.00 TO CIP-RESTRICT-ADJ
                 GO TO TEST-FOR-GEORGIA-SCHG-EXIT.

           MOVE PD-SSNO(1)          TO NCSCHG-LN-SSNO.
           MOVE PD-SSNO(2)          TO NCSCHG-LN-SSNO-2.
           MOVE PD-SSNO(3)          TO NCSCHG-LN-SSNO-3.
           MOVE PD-SSNO(4)          TO NCSCHG-LN-SSNO-4.
           MOVE PD-ORGST            TO NCSCHG-ORGST.
           MOVE PD-CLASS            TO NCSCHG-CLASS.
           MOVE TRANS-DATE-YYYYMMDD TO NCSCHG-TRANS-DATE.
           MOVE " "                 TO NCSCHG-RESTRICTED.
           MOVE 0                   TO NCSCHG-NO-SERCHG.
           MOVE SP-SCFRMLA          TO NCSCHG-SCFRMLA.

           MOVE "CL/NCSC23" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH NCSCHG-WORKERS.
           CANCEL FORM-PROGX.

      * USE CIP-RESTRICT-ADJ TO FLAG CALCZL THAT YOU CANT TAKE
      * ANOTHER SERVICE CHARGE IF ANYTHING IN THERE
           IF NCSCHG-RESTRICTED = "Y"
      * RESTRICTED FLAG = "Y" MEANS THAT YOU CANT TAKE ANOTHER CHARGE
              MOVE 50.00  TO CIP-RESTRICT-ADJ
           ELSE
              MOVE 0 TO CIP-RESTRICT-ADJ.

      * NCSCHG-RESTRICTED "1" = CHARGE $100
      * NCSCHG-RESTRICTED "2" = CHARGE $50
      * NCSCHG-RESTRICTED "3" = CHARGE $25
           IF SP-SCFRMLA = 50
              MOVE 100.00 TO CIP-RESTRICT-ADJ
              IF NCSCHG-RESTRICTED = "3"
                 MOVE 25.00 TO CIP-RESTRICT-ADJ
              ELSE
              IF NCSCHG-RESTRICTED = "2"
                 MOVE 50.00 TO CIP-RESTRICT-ADJ.

       TEST-FOR-GEORGIA-SCHG-EXIT.
           EXIT.

      *******************************************
       TEST-FOR-MFEE SECTION.
      *******************************************

      * TEST FOR TENNESSEE MAINTFEE RESTRICTIONS, CAN NOT CHARGE A
      * MAINTFEE IF AN OPEN LOAN EXIST WITH A MAINTFEE ALREADY

           IF (QUOTE-ONLY)
                  OR (NOT (SP-MFFRMLA = 4))
              MOVE 0 TO CIP-MFEE-RESTRICT
              GO TO TEST-MFEE-EXIT.

           MOVE PD-SSNO(1) TO MFEE04-LN-SSNO(1).
           MOVE PD-SSNO(2) TO MFEE04-LN-SSNO(2).
           MOVE PD-SSNO(3) TO MFEE04-LN-SSNO(3).
           MOVE PD-SSNO(4) TO MFEE04-LN-SSNO(4).
           MOVE TRANS-DATE-YYYYMMDD TO MFEE04-TRANS-DATE.

           IF PD-SRCD = "PB" OR "RN" OR "SC" OR "RB" OR "RO"
              MOVE PD-PRLNNO TO MFEE04-PRIOR-ACCTNO
           ELSE
              MOVE 0 TO MFEE04-PRIOR-ACCTNO.

           MOVE SP-MFFRMLA TO MFEE04-MFFRMLA.
           MOVE "Y" TO MFEE04-CHARGE-MFEE.

           MOVE "CL/MFEE04" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH MFEE04-WORKERS.
           CANCEL FORM-PROGX.

           IF MFEE04-CHARGE-MFEE = "Y"
              MOVE 0 TO CIP-MFEE-RESTRICT
           ELSE
              MOVE 50.00 TO CIP-MFEE-RESTRICT.

       TEST-MFEE-EXIT.
           EXIT.

      *********************************************
      *    TEST FOR WISCONSIN SERVICE CHARGE
      *********************************************
       TEST-FOR-WI-SCHG SECTION.
      *    IF (QUOTE-ONLY) OR SP-SCFRMLA NOT = 63
           IF SP-SCFRMLA NOT = 63
              GO TO TEST-FOR-WI-EXIT.

           MOVE 0 TO WISCHG-PRIOR-SERCHG
                     CIP-RESTRICT-SERCHG.

           IF NOT (PD-SRCD = "PB" OR "RN" OR "SC" OR "RB" OR "RO")
              GO TO TEST-FOR-WI-EXIT.

           MOVE PD-SSNO(1) TO WISCHG-LN-SSNO(1).
           MOVE PD-SSNO(2) TO WISCHG-LN-SSNO(2).
           MOVE PD-SSNO(3) TO WISCHG-LN-SSNO(3).
           MOVE PD-SSNO(4) TO WISCHG-LN-SSNO(4).
           MOVE PD-LOANDATE TO WISCHG-TRANS-DATE.
           MOVE PD-PRLNNO  TO WISCHG-PRIOR-ACCTNO.
           MOVE PD-ORGST   TO WISCHG-ORGST.
           MOVE SP-SCFRMLA TO WISCHG-SCFRMLA.

           MOVE "CL/WISCHG" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH WISCHG-WORKERS.
           CANCEL FORM-PATH.

      * COMES OUT WITH PRIOR SERCHG IF RESTRICTED AND IT MUST
      * BE SUBTRACTED FROM NEW SERCHG
           MOVE WISCHG-PRIOR-SERCHG TO CIP-RESTRICT-SERCHG.

       TEST-FOR-WI-EXIT.
           EXIT.

      *********************************************
       TEST-FOR-SC-SCHG SECTION.
      *********************************************
           IF (QUOTE-ONLY)
                  OR (NOT (SP-SCFRMLA = 18 OR 42))
              GO TO TEST-FOR-SC-SCHG-EXIT.

           MOVE " "        TO SCSCHG-UPDATE-FG.
           MOVE 0          TO SCSCHG-NEW-ACCTNO.
           MOVE PD-SSNO(1) TO SCSCHG-LN-SSNO(1).
           MOVE PD-SSNO(2) TO SCSCHG-LN-SSNO(2).
           MOVE PD-SSNO(3) TO SCSCHG-LN-SSNO(3).
           MOVE PD-SSNO(4) TO SCSCHG-LN-SSNO(4).
           MOVE TRANS-DATE-YYYYMMDD TO SCSCHG-TRANS-DATE.
           IF PD-SRCD = "PB" OR "RN" OR "SC" OR "RB" OR "RO"
              MOVE PD-PRLNNO TO SCSCHG-PRIOR-ACCTNO
           ELSE
              MOVE 0         TO SCSCHG-PRIOR-ACCTNO.
           MOVE PD-ORGST     TO SCSCHG-ORGST.
           MOVE SP-SCFRMLA   TO SC-SCHG-NO SCSCHG-SCFRMLA.
           IF SP-SCFRMLA = 42
              MOVE 18 TO SC-SCHG-NO.
           MOVE SC-SCHG-PATH TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH SCSCHG-WORKERS.
           CANCEL FORM-PROGX.
           MOVE SCSCHG-PRIOR-CASHADV TO CIP-PRIOR-CASHADV.
           MOVE SCSCHG-RESTRICT-SERCHG TO CIP-RESTRICT-SERCHG.
           MOVE SCSCHG-RESTRICT-HIGH-CASHADV
                                 TO CIP-RESTRICT-HIGH-CASHADV.
           MOVE 0 TO SPDUAL-LIAB.

       TEST-FOR-SC-SCHG-EXIT.
           EXIT.

      ******************************************************************
      *    IN : PD-COLLCD
      *         PD-INTDATE
      *         PD-LOANDATE
      *         CURRENT SPR RECORD
      *    OUT: RESCISSION-STATUS
      ******************************************************************
       RESCISSION-ROUTINE SECTION.

           MOVE SPACES      TO RESCISSION-BUF.
           MOVE SPACES      TO RESCISSION-FLAG.
           MOVE PD-LOANDATE TO RESCISSION-INTDATE.
           MOVE SPACES      TO RESCISSION-POCD.
           MOVE SPACES      TO RESCISSION-SECURED-COLLCD.
           MOVE SPACES      TO RESCISSION-STATUS.

           MOVE "CO"      TO CD-TYPE.
           MOVE PD-COLLCD TO CD-CODE.
           PERFORM READ-CD1-FILE.
           IF ( IO-BAD )
              MOVE SPACES TO CD-CO-SECURED.

           MOVE CD-CO-SECURED TO RESCISSION-SECURED-COLLCD.

           IF ( RESCISSION-INTDATE = ZEROES )
              GO TO RESCISSION-ROUTINE-EXIT.

           IF ( NOT RESCISSION-SECURED-COLLCD-YES ) OR
              ( NOT SP-RESCIND-BUS-DAYS-VALID     ) OR
              (     SP-RESCIND-BUS-DAYS = "0"     )
              GO TO RESCISSION-ROUTINE-SKIP.

      * QUOTE IS FOR RESCISSION  
           PERFORM OPEN-GB-FILE.
           MOVE PD-OWNBR TO GB-BRNO.
           MOVE 1        TO GB-NO.
           PERFORM READ-GB-FILE.
           PERFORM CLOSE-GB-FILE.

           MOVE PD-LOANDATE TO RESCIND-LOANDATE.
           PERFORM RESCIND-CALCULATION.

           MOVE RESCISSION-MESSAGE TO RESCISSION-BUF.
           MOVE "*I"               TO RESCISSION-POCD.
           MOVE RESCIND-OUTDATE    TO RESCISSION-INTDATE.

       RESCISSION-ROUTINE-SKIP.

           MOVE RESCISSION-POCD    TO PD-POCD.
           MOVE RESCISSION-INTDATE TO PD-INTDATE.

           IF ( PD-POCD-INRESCISSION )
              MOVE "Y"             TO PD-RESCISSION
              MOVE PD-INTDATE      TO PD-RESCISSION-DATE
           ELSE
              MOVE "N"             TO PD-RESCISSION
              MOVE ZEROES          TO PD-RESCISSION-DATE.

           MOVE "S  A000061." TO VDU.
           MOVE PD-INTDATE    TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY   TO VDU-DATE-EDIT.
           PERFORM SCREENIO.

           PERFORM PROCESS-PRIOR-LOAN.

           MOVE "SNNN062111." TO VDU.
           MOVE PRIOR-LNAMT   TO VDUNUM2.
           PERFORM SCREENIO.

           PERFORM RESCISSION-SET-1STPYDATE.
           PERFORM SEND-RESCISSION.

       RESCISSION-ROUTINE-EXIT.
           EXIT.

      ******************************************************************
       RESCISSION-SET-1STPYDATE SECTION.
      ******************************************************************

           MOVE PD-INTDATE      TO NDTE-DATE SYS-DATE.
           MOVE 1               TO NDTE-HOLD.
           MOVE PD-UNITPER-CD   TO DATER-UNITPER-CD.
           MOVE PD-UNITPER-FREQ TO DATER-UNITPER-FREQ.
           PERFORM INCREMENT-UNITPER.

      * WORLD OPTION TO ADVANCE TO 1ST OF NEXT MONTH
      * IF NO CORRESPONDING DAY IN NEW MONTH:

           PERFORM SET-ADV-TO-1ST-NO-CORR.
           IF ADV-TO-1ST-NO-CORR = "Y"
              IF NOT ( PD-ORGST   = "TN" AND
                       PD-CLASS   = 01   AND
                       PD-ORGTERM = 12)
                 IF PD-UNITPER-CD = "M" AND PD-UNITPER-FREQ = 1
                    IF S-DD > NDTE-DD
                       MOVE 1 TO NDTE-DD
                                 NDTE-HOLD
                       PERFORM INCREMENT-MONTHS
                       MOVE NDTE-DATE TO NUM-DATE.

      * IF DATE OF THE LOAN IS ON THE 26 THRU 31 THEN ADVANCE THE
      * FIRST PAYMENT DUE DATE TO THE FIRST OF THE SECOND MONTH
      * FOLLOWING THE DATE OF THE LOAN. (REGMGM PR#1099)

           IF ( ADV-TO-1ST-NO-CORR = "A" ) AND
              ( PD-UNITPER-CD         = "M" ) AND
              ( PD-UNITPER-FREQ       = 1   ) AND
              ( S-DD > 25                   )
              MOVE 1         TO NDTE-DD NDTE-HOLD
              PERFORM INCREMENT-MONTHS
              MOVE NDTE-DATE TO NUM-DATE.

           IF PD-UNITPER-CD = "S"
              IF NDTE-DD = 14 OR 15
                 MOVE 16 TO NDTE-DD.

           MOVE "S  A000130." TO VDU.
           MOVE NDTE-DATE     TO PD-1STPYDATE
                                 PD-ORIG-1STPYDATE
                                 CIP-1STPYDATE
                                 DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO VDU-DATE-EDIT.
           PERFORM SCREENIO.

       RESCISSION-SET-1STPY-EXIT.
           EXIT.

      ******************************************************************
      *
      *    C O P Y - L I B R A R Y - S E C T I O N S
      *
      ******************************************************************
       COPY "LIBLP/INSELIG.CPY".
       COPY "LIBLP/LPANTI.CPY".
       COPY "LIBGB/HASH.CPY".
       COPY "LIBGB/DATER.CPY".
       COPY "LIBLP/LPUPER.CPY".
       COPY "LIBLP/LPUPWK.CPY".
       COPY "LIBLP/UPERTST.CPY".
       COPY "LIBLP/LPDAMTS.CPY".
       COPY "LIBLP/LPSPEDP.CPY".
       COPY "LIBLP/LPPDCL.CPY".
       COPY "LIBWI/PFSWINDOWS.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/GPENV.CPY".
       COPY "LIBLP/LPBAGE.CPY".
       COPY "LIBLP/LPAPRZ.CPY".
       COPY "LIBLP/RESCIND.CPY".
       COPY "LIBWI/FEEENT.CPY".
       COPY "LIBWI/FEEENTPD.CPY".
       COPY "LIBLP/LPICOMM.CPY".
       COPY "LIBLP/LPZ3CL.CPY".
       COPY "LIBWI/W12EQ2PMT.CPY".

      ********************************************************
       FORM-RESET SECTION.
      ********************************************************
           MOVE EXT-CONAME-SYSDATE TO BLQUOT-SYSDATE.
           DISPLAY SPACES UPON BLQUOT-WINDOW-HANDLE
                          LINE 1 COL 1 ERASE SCREEN.
           DISPLAY BLQUOT-SCREEN UPON BLQUOT-WINDOW-HANDLE.
           MOVE BLQUOT-FORM-FIELDS TO WR-BUF.
           MOVE BLQUOT-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

           MOVE "S  A200TITLE:FR." TO VDU.
           MOVE " BALLOON LOAN QUOTER" TO VDUBUF.
           PERFORM SCREENIO.
       FORM-RESET-EXIT.
           EXIT.

      ********************************************************
       SETUP-LINK-INFO SECTION.
      ********************************************************
           MOVE BLQUOT-HELP--FILE TO HELP-LINK-FILE.
           MOVE BLQUOT-HELP--DIR TO HELP-LINK-DIR.

      ********************************************************
       VDU-CONVERT-FIELD SECTION.
      ********************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBWI/BLQUOT_EVA.CPY".
              COPY "LIBWI/FEEENT_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ********************************************************
       MISC-PARAGRAPHS SECTION.
      ********************************************************
       BW-CHECK-ENDORSER.
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/SYSTEM.CPY".
       COPY "LIBGB/GETENV.CPY".
       COPY "LIBGB/MKTEMP.CPY".
      *----------------------------------------------------
       SET-CLFILE-INDEX-BY.
           MOVE BR-CAN-GROUP-ID TO CL-GROUP-ID  CLSCAN-GROUP-ID.
      *----------------------------------------------------------------
      *    IF MAKING CHANGES, CALCULATE PROCEEDS AND NOT PYAMT
      * REV:
      *  JTG 022797 SKIPPED THIS WHEN STATE IS 'NM', SINCE CAN LOANS
      *             FOR THIS STATE, WHEN RECALCULATED CAUSE ROUNDING
      *             ON THE PROCEEDS & PAYMENT
      *  JTG 031297 SKIPPED THIS WHEN STATE IS 'TX'
      *-----------------------------------------------------------------
       FORCE-CAN-LOAN-NUMBERS.
           IF NOT (SP-ORGST = "NM" OR "TX")
              MOVE "S  A010141." TO VDU
              MOVE "C" TO VDUBUF PD-PROCEEDS-FG
                          CIP-PROCEEDS-SW
              PERFORM SCREENIO
              MOVE "S  A010421." TO VDU
              MOVE "I" TO VDUBUF
                          PD-REGPYAMT-FG
                          CIP-REGPYAMT-SW
              PERFORM SCREENIO.
       SET-1ST-PAY-DATE.
           MOVE PD-INTDATE TO NDTE-DATE
                              SYS-DATE.
           MOVE 1 TO NDTE-HOLD.
           MOVE PD-UNITPER-CD TO DATER-UNITPER-CD.
           MOVE PD-UNITPER-FREQ TO DATER-UNITPER-FREQ.
           PERFORM INCREMENT-UNITPER.

      * WORLD OPTION TO ADVANCE TO 1ST OF NEXT MONTH
      * IF NO CORRESPONDING DAY IN NEW MONTH:
           PERFORM SET-ADV-TO-1ST-NO-CORR.
           IF ADV-TO-1ST-NO-CORR = "Y"
              IF NOT ( PD-ORGST   = "TN" AND
                       PD-CLASS   = 01   AND
                       PD-ORGTERM = 12)
                 IF PD-UNITPER-CD = "M" AND PD-UNITPER-FREQ = 1
                    IF S-DD > NDTE-DD
                       MOVE 1 TO NDTE-DD
                                 NDTE-HOLD
                       PERFORM INCREMENT-MONTHS
                       MOVE NDTE-DATE TO NUM-DATE.

      * IF DATE OF THE LOAN IS ON THE 26 THRU 31 THEN ADVANCE THE
      *   FIRST PAYMENT DUE DATE TO THE FIRST OF THE SECOND MONTH
      *   FOLLOWING THE DATE OF THE LOAN. (REGMGM PR#1099)
           IF ADV-TO-1ST-NO-CORR = "A"
              IF PD-UNITPER-CD = "M" AND PD-UNITPER-FREQ = 1
                 IF S-DD > 25
                    MOVE 1 TO NDTE-DD
                              NDTE-HOLD
                    PERFORM INCREMENT-MONTHS
                    MOVE NDTE-DATE TO NUM-DATE.

      *  JTG 011001 DON'T ALLOW A FIRST PAY DAY OF 31 WHEN SEMI MONTHLY PAPER
      *                                                            FFG PR#333
      *  CS  021001 PER JOHN DON'T ALLOW A FIRST PAY DAY OF 14 OR 15 EITHER IF
      *      SEMI-MONTHLY.  FEB CAUSED THE CYCLE DATES FOR LATE CHARGES TO BE
      *      OUT OF SYNC. FOR EXAMPLE: WITH PAYDATE OF 15 LCPDTH GOES TO 2/28/YY      *      MARCH WENT TO 3/13/YY INSTEAD OF 3/15/YY [DISCOVERED AT FFG]

           IF PD-UNITPER-CD = "S"
              IF NDTE-DD = 14 OR 15
                 MOVE 16 TO NDTE-DD.

           MOVE "S  8000130." TO VDU.
           MOVE NDTE-DATE TO PD-1STPYDATE
                             PD-ORIG-1STPYDATE
                             CIP-1STPYDATE
                             VDU-DATE-YYYYMMDD.
           PERFORM SCREENIO.

       DISPLAY-BALLOON-LAST.
           MOVE "S  A160P170:P." TO VDU.
           IF PD-BALLOON-LAST-FG = "B"
              MOVE "17 BALLOON AMT :" TO VDUBUF
           ELSE
              MOVE "17 LAST PAYMENT:" TO VDUBUF.
           PERFORM SCREENIO.

       INVALID-DUE-DAY.
           IF PD-1STPYDA = 31
              MOVE " 31ST OF MONTH INVALID FOR SEMI-MONTHLY PAPER"
                                                           TO MESS
           ELSE
              STRING
            PD-1STPYDA,"TH OF MONTH INVALID FOR SEMI-MONTHLY PAPER"
                                       DELIMITED BY "  " INTO MESS.

       SEND-BW-MAKERCD.
           MOVE "S  A010  2." TO VDU.
           MOVE ELE TO VDUROW.
           MOVE PD-MAKERCD(ELE) TO VDUBUF.
           PERFORM SCREENIO.

       GET-CODE.
           MOVE "GC"     TO CDB-TYPE.
           MOVE PD-OWNBR TO CDB-BRANCH.
      *--> SET BEFORE       CDB-CODE.
           PERFORM OPEN-CDB1-FILE.
           PERFORM READ-CDB1-FILE.
           PERFORM CLOSE-CDB1-FILE.
           IF IO-FG = 9
              MOVE " * NOT ON FILE * " TO CDB-DESC.
       GET-LC.
           PERFORM OPEN-LC1-FILE.
           PERFORM READ-LC1-FILE.
           PERFORM CLOSE-LC1-FILE.
           IF IO-FG = 9
              MOVE "INVALID" TO LC-DESC.
       GET-CD.
           PERFORM READ-CD1-FILE.
           IF IO-FG = 9
              MOVE " INVALID CODE" TO CD-DESC.
       GET-BW1.
           PERFORM READ-BW1-FILE.
           IF IO-FG = 9
              MOVE " " TO BW-FNAME
              MOVE "NOT FOUND" TO BW-LNAME.
           PERFORM APPEND-BW-NAMES.
       GET-PD1.
           PERFORM OPEN-PD1-FILE.
           PERFORM READ-PD1-FILE.
           PERFORM CLOSE-PD1-FILE.
       GET-DL1.
           PERFORM OPEN-DL1-FILE.
           PERFORM READ-DL1-FILE.
           PERFORM CLOSE-DL1-FILE.
           IF IO-FG = 9
              MOVE "NOT ON FILE" TO DL-SHORT.
       GET-SP.
           MOVE PD-ORGST TO SP-ORGST.
           MOVE PD-SPRCLASS TO SP-SPRCLASS.
           MOVE PD-SUBCLASS TO SP-SUBCLASS.
           MOVE PD-LAWCODE TO SP-LAWCODE.
           PERFORM READ-SP1-FILE.
           IF IO-FG = 9
              MOVE "STATE PARA DELETED" TO MESS
              PERFORM SEND-MESS
              GO TO IO-ERROR.

       MATURITY-DATE-CALCULATION.
           MOVE PD-UNITPER-CD TO DATER-UNITPER-CD.
           MOVE PD-UNITPER-FREQ TO DATER-UNITPER-FREQ.
           MOVE PD-1STPYDATE TO NDTE-DATE.
           COMPUTE NDTE-HOLD = (PD-ORGTERM - 1).
           PERFORM INCREMENT-UNITPER.
       REMOVE-SCRATCH-IF-RECALL.
           IF RECALL-UPDATE
              PERFORM DELETE-SCRATCH.
       UPDATE-OVR-RATE.
           IF PD-LOANTYPE = "P" OR
              COP-OVR-RATE-SW = "C"
              MOVE 999.999 TO PD-OVRDRATE
           ELSE
              IF COP-OVR-RATE-SW = "E"
                 MOVE COP-OVR-RATE TO PD-OVRDRATE
              ELSE
                 MOVE COP-EFF-RATE TO PD-OVRDRATE.

       MOVE-MSG.
           MOVE PD-MSG1 TO MSG-LINE1.
           MOVE PD-MSG2 TO MSG-LINE2.
           MOVE SCREEN-LINE1 TO D-231.
           MOVE SCREEN-LINE2 TO D-232.

       VALIDATE-FIELDS-AGAINST-MAX.
           MOVE 0 TO IO-FG.
           IF NEW-FLAG NOT = 1
              PERFORM VALIDATION.

       VERIFY-REQD-FLDS.
           MOVE 0 TO IO-FG.

           IF ( SP-CAN-LOANS-ONLY ) AND
              ( CAN-LOAN-NO = " " )
              MOVE
              " THIS SPR IS FOR CAN LOANS ONLY, UPDATE NOT ALLOWED!"
                                                             TO MESS
              PERFORM SEND-MESS
              MOVE 9 TO IO-FG.

           PERFORM VERIFY-SETUP-PAYMENTS.
           IF NOREG-FG NOT = " "
              MOVE 9 TO IO-FG.

           PERFORM VERIFY-PROCEEDS.

           IF NOT GP-INCL-OTH-IN-EARNED-N
              IF PD-ANTICERN(1) < 0
                     OR PD-ANTICERN(2) < 0
                      OR PD-ANTICERN(3) < 0
                       OR PD-ANTICERN(4) < 0
                        OR PD-ANTICERN(5) < 0
                         OR PD-ANTICERN(6) < 0
                 MOVE "ANTICIPATED EARNINGS CAN'T BE LESS THAN ZERO!"
                                                             TO MESS
                 PERFORM SEND-MESS
                 MOVE 9 TO IO-FG
              END-IF
           ELSE
              IF PD-ANTICERN(1) < 0
                     OR PD-ANTICERN(2) < 0
                      OR PD-ANTICERN(3) < 0
                       OR PD-ANTICERN(4) < 0
                        OR PD-ANTICERN(5) < 0
                 MOVE "ANTICIPATED EARNINGS CAN'T BE LESS THAN ZERO!"
                                                             TO MESS
                 PERFORM SEND-MESS
                 MOVE 9 TO IO-FG.

      * SP-EDT-TABLE-LOOKUP:
           IF IO-FG = 0
      *       VALIDIATE TERM:
              MOVE 0 TO ELE
              PERFORM SP-EDT-TABLE-LOOKUP
      *       VALIDIATE PROCEEDS:
              IF IO-FG = 0
                 MOVE 11 TO ELE
                 PERFORM SP-EDT-TABLE-LOOKUP.
           IF IO-FG = 0
              IF NOT COP-CALCULATION-OK
                 PERFORM ERROR-IN-CALCULATIONS-MSG
                 MOVE 9 TO IO-FG.

       VERIFY-PROCEEDS.
           IF PD-SRCD = "RN"
              IF PD-PROCEEDS NOT = 0
                 MOVE "PROCEEDS MUST BE ZERO FOR RENEWALS" TO MESS
                 PERFORM SEND-MESS
                 MOVE 9 TO IO-FG.
           IF PD-SRCD = "PB" OR "SC"
              IF PD-PROCEEDS < 0
                 MOVE "PROCEEDS < ZERO - INVALID" TO MESS
                 PERFORM SEND-MESS
                 MOVE 9 TO IO-FG.

      * REGMGM 4885
      * "D" PROCEEDS OF NEW LOAN HAS TO BE AT LEAST X% OF THE PAYOFF AMT
      *     PROCEEDS OF NEW LOAN HAVE TO BE > THAN SP-AMT-PROCEEDS-REQ
           IF (PD-SRCD = "PB" OR "SC") AND (SP-RENEWAL-EDIT = "D")
              IF PD-PROCEEDS < (PRIOR-LNAMT * (SP-NET-BAL-REQ/100))
                 MOVE "PROCEEDS NOT > THAN PERCENT REQUIRED" TO MESS
                 PERFORM SEND-MESS
                 MOVE 9 TO IO-FG
              ELSE
                 IF PD-PROCEEDS < SP-AMT-PROCEEDS-REQ
                    MOVE "PROCEEDS NOT > THAN MINIMUM" TO MESS
                    PERFORM SEND-MESS
                    MOVE 9 TO IO-FG.

      * REGMGM 4885
      * "E" PROCEEDS OF NEW LOAN HAS TO BE AT LEAST X% OF THE PAYOFF AMT
      *     PROCEEDS OF NEW LOAN HAVE TO BE < THAN SP-AMT-PROCEEDS-REQ
           IF (PD-SRCD = "PB" OR "SC") AND (SP-RENEWAL-EDIT = "E")
              IF PD-PROCEEDS > (PRIOR-LNAMT * (SP-NET-BAL-REQ/100))
                 MOVE "PROCEEDS ARE NOT < THAN PERCENT REQUIRED" TO MESS
                 PERFORM SEND-MESS
                 MOVE 9 TO IO-FG
              ELSE
                 IF PD-PROCEEDS > SP-AMT-PROCEEDS-REQ
                    MOVE "PROCEEDS ARE > THAN MAXIMUM" TO MESS
                    PERFORM SEND-MESS
                    MOVE 9 TO IO-FG.

       VERIFY-PROCEEDS-DOWN.
           IF PD-SELLPR NOT > 0
              MOVE "SELLING PRICE MUST BE MORE THAN ZERO!"
                                                     TO MESS
              PERFORM SEND-MESS.

       COMPUTE-GL-FEEAMT.
           ADD 1 TO COUNTER.
           IF COUNTER < 3
              IF SP-FEEFRMLA(COUNTER) NOT = " "
                 GO TO COMPUTE-GL-FEEAMT.
           IF COUNTER NOT > 10
              IF PD-FEEOURS(COUNTER) NOT = "M"
                 ADD PD-FEEAMT(COUNTER) TO TOTAL-GL-HLDR
                 IF PD-FEEFINCHG(COUNTER) = "Y"
                    ADD PD-FEEAMT(COUNTER) TO TOTAL-GL-FINCHG-HLDR
                 END-IF
              END-IF
              GO TO COMPUTE-GL-FEEAMT.
           MOVE 0 TO COUNTER.

       BLANK-PRIOR-LOAN.
           MOVE 0 TO CIP-PRIOR-LOAN-CLASS
                      CIP-PRIOR-SUB-CLASS
                       CIP-PRBAL
                        CIP-PRINTDUE
                         CIP-PRFINCHGRB
                          CIP-PRINSRB
                           CIP-PRPAID-AHEAD
                            CIP-PRIOR-LOAN-DATE
                             PRIOR-LNAMT
                              PD-PRLNNO
                               PD-PRBAL
                                PD-PRINTDUE
                                 PD-PRFINCHGRB
                                  PD-PRINSRB
                                   PD-PRPAID-AHEAD.
           MOVE " " TO CIP-PRIOR-LOAN-STATE.
           MOVE PD-SRCD TO CIP-SOURCE-CD.

           MOVE "SNNN060110." TO VDU.
           MOVE PD-PRLNNO TO VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN062111." TO VDU.
           MOVE PRIOR-LNAMT TO VDUNUM2.
           PERFORM SCREENIO.

           IF NEW-FLAG NOT = 1
      * CITIZENS TEST
              MOVE 800 TO CIP-NO-OF-TIMES-CALLED
      *       MOVE 0 TO CIP-NO-OF-TIMES-CALLED
              PERFORM CALL-LOAN-CALCULATIONS.

       SETUP-CALC-AREA.
           MOVE "C" TO CIP-OVR-RATE-SW
                       CIP-INS-SW(1)
                       CIP-INS-SW(2)
                       CIP-INS-SW(3)
                       CIP-INS-SW(4)
                       CIP-FEE-CHG-SW(1)
                       CIP-FEE-CHG-SW(2)
                       CIP-SERVICE-CHG-SW
                       CIP-MAINT-FEES-SW
                       CIP-1ST-PY-EXT-SW
                       CIP-REGPYAMT-SW.
           MOVE "I" TO CIP-INS-SW(5)
                       CIP-INS-SW(6).

           MOVE PD-LOANTYPE TO CIP-LOANTYPE.
           MOVE PD-SMALLOAN TO CIP-SMALLOAN.
           MOVE PD-SALEFIN TO CIP-SALEFIN.
           MOVE PD-SERVACCT TO CIP-SERVACCT.

           MOVE PD-LOANDATE  TO CIP-LOANDATE.
           MOVE PD-INTDATE   TO CIP-INTDATE.
           MOVE PD-FILEDATE  TO CIP-FILEDATE.
           MOVE PD-1STPYDATE TO CIP-1STPYDATE.
           MOVE PD-ORGTERM TO CIP-TERM.

           MOVE PD-PURCD TO CIP-PURPOSE-CD.
           MOVE PD-COLLCD TO CIP-COLLTRL-CD.
           MOVE PD-SRCD TO CIP-SOURCE-CD.
           SUBTRACT PD-DOWNPY FROM PD-SELLPR
                             GIVING PD-PROCEEDS
                                    CIP-PROCEEDS.
           MOVE PD-PROCEEDS-FG TO CIP-PROCEEDS-SW.

           MOVE PD-INSURED TO CIP-INS-WHO.
           MOVE PD-INSTYPES TO CIP-INS-TYPE.
           MOVE PD-INSELIGIBLE TO CIP-INS-ELIG.
           MOVE PD-POINTS TO CIP-POINTS.
           MOVE PD-POINTS-FG TO CIP-POINTS-SW.
           MOVE TOTAL-GL-HLDR TO CIP-GL-AMOUNT-TOTAL.
           MOVE TOTAL-GL-FINCHG-HLDR TO CIP-GL-AMOUNT-TOTAL-FINCHG.

       MOVE-INSURANCE-TO-PDREC.
           MOVE COP-INS-CO(COUNTER) TO PD-INSCOMP(COUNTER)
                                       DF2-INSCOMP(COUNTER).
           MOVE COP-INS-USE-SW(COUNTER) TO PD-INSOURS(COUNTER)
                                           DF2-INSOURS(COUNTER).
           MOVE COP-INS-TERM(COUNTER) TO PD-INSTRM(COUNTER)
                                         DF2-INSTERM(COUNTER).
           MOVE COP-INS-COVERAGE(COUNTER) TO PD-INSCOVR(COUNTER)
                                             DF2-INSCOVR(COUNTER).
           MOVE COP-INS-PREMIUM(COUNTER) TO PD-INSPREM(COUNTER)
                                            DF2-INSPREM(COUNTER).
           MOVE COP-INS-SW(COUNTER) TO PD-INS-FG(COUNTER)
                                       DF2-INS-FG(COUNTER).
           MOVE COP-INSFINCHG-SW(COUNTER) TO PD-INSFINCHG(COUNTER).
           MOVE COP-INS-TERM-SW(COUNTER) TO PD-INS-TERM-SW(COUNTER).
           MOVE COP-COVERAGE-SW(COUNTER) TO PD-COVERAGE-SW(COUNTER).
           MOVE COP-PREMIUM-SW(COUNTER) TO PD-PREMIUM-SW(COUNTER).

       ICE-OR-A-MSG.
           IF ELE = 26
              IF SP-CAL-FLAT-MONTHLY(1)
                 MOVE "MUST BE 'C' OR 'A'" TO MESS
              ELSE
                 MOVE "MUST BE 'C', 'E', OR 'A'" TO MESS
              END-IF
           ELSE
              MOVE "MUST BE 'I' OR 'C'" TO MESS.
           PERFORM SEND-MESS.

       VERIFY-SETUP-PAYMENTS.
           MOVE " " TO NOREG-FG.
           SUBTRACT 1 FROM PD-ORGTERM GIVING PD-NOREGPY.
           IF PD-LASTPYAMT NOT = 0
              SUBTRACT 1 FROM PD-NOREGPY.
           IF PD-ORGTERM = 1
              IF PD-LASTPYAMT NOT = 0
                 MOVE "E" TO NOREG-FG
                 MOVE "LAST PAYMENT MUST BE ZERO" TO MESS
                 PERFORM SEND-MESS.
           IF PD-ORGTERM > 1
              IF NOT (PD-1STPYAMT > 0
                          AND PD-REGPYAMT > 0
                            AND PD-LASTPYAMT > 0
                     )
                 MOVE "E" TO NOREG-FG
                 MOVE
            "FIRST, REGULAR AND LAST PAYMENT MUST BE GREATER ZERO"
                                                               TO MESS
                 PERFORM SEND-MESS.

       ERROR-IN-CALCULATIONS-MSG.
           IF COP-RETURN-CODE = "V"
              MOVE "ERROR IN CALCULATIONS - AMOUNT EXCEEDS MAX"
                                                        TO MESS
           ELSE
              MOVE "ERROR IN CALCULATIONS - INACCURATE RESULTS"
                                                        TO MESS.
           PERFORM SEND-MESS.
           MOVE "X" TO COP-RETURN-CODE.

       SEND-CD-DESC.
           PERFORM GET-CD.
           MOVE "S  A000  1." TO VDU.
           MOVE ELE TO VDUROW.
           MOVE CD-DESC TO VDUBUF.
           PERFORM SCREENIO.
       APPEND-BW-NAMES.
           MOVE BW-LNAME TO INSPECT-W.
           MOVE BW-FNAME TO ADD-ON.
           INSPECT INSPECT-W REPLACING FIRST "   " BY ", +".
           INSPECT INSPECT-W REPLACING FIRST
              "+                        " BY ADD-ON.
       ADD-IT.
           INSPECT INSPECT-W REPLACING FIRST "  " BY " +".
           INSPECT INSPECT-W REPLACING FIRST
              "+                        " BY ADD-ON.
       ADD-IT2.
           PERFORM ADD-IT.
           MOVE "/" TO ADD-ON.
           PERFORM ADD-IT.

       NUMBW-TO-ALPBW.
           MOVE " " TO ALP-BW.
           IF BW-SSNO NOT = 0
              MOVE BW-SSNO TO NUM-BW
              MOVE NUM-BW123 TO ALP-BW123
              MOVE NUM-BW45 TO ALP-BW45
              MOVE NUM-BW6789 TO ALP-BW6789
              MOVE "-" TO ALP-HYPHEN1 ALP-HYPHEN2.

       SEND-BW-NO-NAME.
           PERFORM NUMBW-TO-ALPBW.
           MOVE "S  A110  0." TO VDU.
           MOVE ELE TO VDUROW.
           MOVE ALP-BW TO VDUBUF.
           PERFORM SCREENIO.
           PERFORM SEND-BW-NAME.

       SEND-BW-NAME.
           MOVE "S  A330  1." TO VDU.
           MOVE ELE TO VDUROW.
           MOVE INSPECT-W TO VDUBUF.
           PERFORM SCREENIO.
           IF ELE = 1
              MOVE INSPECT-W TO PD2-BWNAME SAVE-BW-NAME.
       SEND-MEMO.
           MOVE "S  A060M1." TO VDU.
           MOVE "MEMO: " TO VDUBUF.
           PERFORM SCREENIO.
           MOVE "S  A300M2." TO VDU.
           MOVE PD-MESSAGE TO VDUBUF.
           PERFORM SCREENIO.
       MISSING-FEECODE.
           STRING "YOUR BRANCH IS MISSING A FEE "
                  "CODES NEEDED FOR THIS CAN LOAN!      "
                  DELIMITED BY SIZE INTO MESS.
           PERFORM SEND-MESS.
           MOVE " PLEASE INFORM HOME OFFICE!!" TO MESS
           PERFORM SEND-MESS.
       NOT-FOUND.
           MOVE "RECORD NOT FOUND" TO MESS.
           PERFORM SEND-MESS.
       SP-INVALID.
           MOVE "ENTRY INCONSISTENT WITH STATE PARAMETERS" TO MESS.
           PERFORM SEND-MESS.
       NOT-DIRECT.
           MOVE "INVALID FOR BALLOON (DIRECT) LOAN ENTRY!" TO MESS.
           PERFORM SEND-MESS.
       SEND-WSEL-LEGEND.
           MOVE MESS TO WR-BUF.
           MOVE WSEL-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.
       SEND-LEGEND.
           MOVE LEGEND TO WR-BUF.
           MOVE MSEL-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.
       RESET-LEGEND.
           MOVE " " TO LEGEND.
           PERFORM SEND-LEGEND.
       LOAN-CLASS-PROMPT.
           MOVE "S  A110KEYTYPE." TO VDU.
           MOVE "LOAN CLASS:" TO VDUBUF.
           PERFORM SCREENIO.
       SET-SCRATCH-KEY.
           MOVE SAVE-SSNO TO PD-SOC-SEC.
           MOVE EXT-CONAME-USER-LOC TO PD-BRANCH PD2-BRANCH.
           MOVE "L" TO PD-ID.
      *-----------------------------------------------------------------
       VERIFY-AGAINST-SPEDT.
           MOVE PD-EFFRATE TO VERIFY-TERMCEIL-EFF.
           MOVE PD-APRATE TO VERIFY-TERMCEIL-APR.
           MOVE "Y" TO VERIFY-TERMCEIL-EFF-FG.
           MOVE TRANS-DATE-YYYYMMDD TO VERIFY-TODAY-DATE.

           PERFORM SET-BW1-FOR-SPEDT.

           MOVE BW-BDATE TO VERIFY-AGE.
           MOVE BW-REFDATE TO VERIFY-REFDATE.
           MOVE " " TO VERIFY-REJECT-TERM-FG.
           PERFORM VERIFY-TERMCEIL.

           IF COP-OTINS-COV-ERR
              MOVE "INSURANCE COVERAGE ERROR" TO MESS
              PERFORM SEND-MESS
              MOVE "N" TO VALID-LOAN.

      *-----------------------------------------------------------------
       SET-BW1-FOR-SPEDT.
           IF PD-SSNO(1) = 0
              MOVE 0 TO BW-BDATE
                        BW-REFDATE
                        BW-CREDIT-SCORE
           ELSE
      * IF SECONDARY (SPOUSE) INSURANCE, TEST SECOND MAKER'S AGE EDITS...
              IF PD-INSURED = "S"
                 IF PD-SSNO(2) NOT = BW-SSNO
                    MOVE PD-SSNO(2) TO BW-SSNO
                    PERFORM READ-BW1-FILE
                    IF IO-FG = 9
                       MOVE 0 TO BW-BDATE BW-REFDATE
                    END-IF
                 END-IF
              ELSE
                 IF PD-SSNO(1) NOT = BW-SSNO
                    MOVE PD-SSNO(1) TO BW-SSNO
                    PERFORM READ-BW1-FILE
                    IF IO-FG = 9
                       MOVE 0 TO BW-BDATE BW-REFDATE.
      *-----------------------------------------------------------------
       ESTABLISH-REJECT-TERMS.
           MOVE "Y"        TO VERIFY-TERMCEIL-EFF-FG.
           PERFORM MATURITY-DATE-CALCULATION.
           MOVE NDTE-DATE TO VERIFY-TODAY-DATE.
           PERFORM SET-BW1-FOR-SPEDT.

           MOVE BW-BDATE     TO VERIFY-AGE.
           MOVE BW-REFDATE TO VERIFY-REFDATE.
           MOVE "Y"        TO VERIFY-REJECT-TERM-FG.

           PERFORM VERIFY-TERMCEIL.

           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 4
              MOVE VERIFY-REJECT-TERM(SUB) TO CIP-INS-REJECT-TERM(SUB)
           END-PERFORM.

      *-----------------------------------------------------------------
       PROTECT-FIELDS.
           MOVE STAT TO CHFG(5)
                        CHFG(34) CHFG(35) CHFG(36) CHFG(42).

           IF SP-FEEFRMLA(1) = " "
              MOVE "N" TO CHFG(20)
           ELSE
              MOVE " " TO CHFG(20).
           IF SP-FEEFRMLA(2) = " "
              MOVE "N" TO CHFG(21)
           ELSE
              MOVE " " TO CHFG(21).

           MOVE STAT TO DATE-STAT.

       UNPROTECT-FIELDS.
           MOVE STAT TO CHFG(10) CHFG(11)
                        CHFG(34) CHFG(35) CHFG(36) CHFG(42).

           IF SP-FEEFRMLA(1) = " "
              MOVE "N" TO CHFG(20)
           ELSE
              MOVE " " TO CHFG(20).
           IF SP-FEEFRMLA(2) = " "
              MOVE "N" TO CHFG(21)
           ELSE
              MOVE " " TO CHFG(21).

           MOVE STAT TO DATE-STAT.

       SEND-PRIOR-MESS.
           IF PASS-STAT-FG = "Z"
              MOVE "PRIOR LOAN PAID OFF OR BALANCE EXPENSED" TO MESS
           ELSE
           IF PASS-STAT-FG = "S"
              MOVE "PRIOR LOAN NOT SALES FINANCE" TO MESS
           ELSE
           IF PASS-STAT-FG = "J"
              STRING "PRIOR LOAN PAID OFF USING TRANSACTION CODE '"
                     HOLD-PRIOR-PO-TRCD
                     "' - NOT ALLOWED"
                 DELIMITED BY SIZE INTO MESS
           ELSE
              MOVE "PRIOR LOAN IS SALES FINANCE - NOT ALLOWED" TO MESS.
           PERFORM SEND-MESS.

      *---------------------------------------------------{ RESCISSION }
       SEND-RESCISSION.

           IF ( RESCISSION-BUF = SPACES )
              MOVE "R1.  " TO RESCISSION-LIST
           ELSE
              MOVE "R1:R." TO RESCISSION-LIST.

           MOVE RESCISSION-BUF TO WR-BUF.
           MOVE RESCISSION-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      *-----------------------------------------------------------------
      * SET ADV-TO-1ST-NO-CORR. IF THE SP FLAG IS SET, ALLOW IT TO
      * OVERRIDE THE GP FOR SPECIFIC CLASSES.
      *-----------------------------------------------------------------
       SET-ADV-TO-1ST-NO-CORR.
           IF NOT (SP-ADV-TO-1ST-NO-CORR = "N" OR " ")
              MOVE SP-ADV-TO-1ST-NO-CORR TO ADV-TO-1ST-NO-CORR
           ELSE
              MOVE GP-ADV-TO-1ST-NO-CORR TO ADV-TO-1ST-NO-CORR.

      *********************************
       IO-ROUTINE SECTION.
      *********************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBLP/LPSPGS_SQL.CPY".
       COPY "LIBLP/LPPDGS_SQL.CPY".
       COPY "LIBLP/LPLCGS_SQL.CPY".
       COPY "LIBLP/LPICGS_SQL.CPY".
       COPY "LIBLP/LPDLGS_SQL.CPY".
       COPY "LIBLP/LPCLGS_SQL.CPY".
       COPY "LIBLP/LPCDBGS_SQL.CPY".
       COPY "LIBLP/LPCDGS_SQL.CPY".
       COPY "LIBLP/LPBWGS_SQL.CPY".
       COPY "LIBGL/GLGIGS_SQL.CPY".
       COPY "LIBGB/GBGBGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBLP/LPLC1RN.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBGB/GBGBIN.CPY".
       COPY "LIBLP/LPDL1RN.CPY".
       COPY "LIBLP/LPCL1RN.CPY".
       COPY "LIBLP/LPPD1IN.CPY".
       COPY "LIBLP/LPPD2IN.CPY".
       COPY "LIBLP/LPSP1RN.CPY".
       COPY "LIBLP/LPBW1RN.CPY".
       COPY "LIBLP/LPCD1RN.CPY".
       COPY "LIBLP/LPCDB1RN.CPY".
       COPY "LIBLP/LPIC1RN.CPY".
       COPY "LIBGL/GLGI1RN.CPY".

       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO ERRCD.

      *********************************
       END-ROUTINE SECTION.
      *********************************
           PERFORM CLOSE-FILES.
       EXIT-PROG.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY BLQUOT-SCREEN.
           DESTROY BLQUOT-WINDOW-HANDLE.
       STOP-RUN.
           EXIT PROGRAM.
