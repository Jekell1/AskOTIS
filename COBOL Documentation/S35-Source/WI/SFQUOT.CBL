       IDENTIFICATION DIVISION.
       PROGRAM-ID.      SFQUOT.
       DATE-WRITTEN.    080492.
      *************************************************************************
      *
      *  DESCRIPTION:   SALES FINANCE QUOTER
      *                 CALLED FROM CL/WIMENU (F10)
      * REV:
      *  JTG 062195 EXPANDED FOR 10 INSURANCES
      *  JTG 062395 ADDED OPTION FOR BR-SFDLO3-WINDOW-FG
      *  MJD 081195 ADDED LOGIC FOR GP-THE-ONLY-SF-SRCD.
      *  JTG 093095 ADDED LOGIC FOR GP-DEFAULT-O3-INS-COMPANY
      *  JTG 011796 CHANGED SO O2 & O3 INS. DON'T COMPUTE DEFAULT EXPIRATION
      *             DATE UNLESS INITIAL INPUT
      *  MJD 021996 ADDED OPTION TO BR-SFDLO3-WINDOW-FG TO OPEN WINDOW IF
      *              FLAG = "B" (RE. RELIANCE.)
      *  MJD 032896 ADDED LIBLP/PURFLDW : CONTAINS THE PD-REC FIELDS THAT ARE
      *             CHANGED MODIFIED IN PURFLDPD, SO THAT PD-REC IS NOT
      *             MODIFIED UNTIL AFTER CALL IS MADE.
      *  JTG 032896 ADDED SET OF PURFLD--FG FIELDS
      *  MJD 040896 ADDED TEST OF LC-AFP-CLASS AND DL-AFP-DEALER.
      *  MJD 041996 ADDED GP-DLR-DISP-TYPE.
      *  BAH 062896 ADDED INSEXP(5) AND INSEXP(6) TO MAKE-DATES-CURRENT
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
      *  JTG 020498 SET CIP-CAN-LOAN-NO, RE: LOGIC TO FORCE ONE CALENDAR MONTH
      *             SEE CALCZ3.C
      *  JTG 032498 CHANGED TO LET SINGLE PAYS WORK WITH 'B'-BALLOON
      *  BAH 040698 GP-DEFAULT-O3-INS-COMPANY 0 WAS NOT ALLOWING ANY INS CO.
      *  BAH 042998 MOVE BW-ORIGIN TO PD-ORIGIN, FEDERATED PR# 646
      *   CS 042998 ADDED RENEWAL CODES "RB" RENEW BK AND "RO" RENEWAL OTHER
      *  KEC 090498 ADDED 'MOVE LC-LOANTYPE TO PD-LOANTYPE';
      *              FOR REGENCY REVOLVING.
      *  JTG 092598 CHANGED CALL FROM LONPZ3 TO CALCZ3
      *  JTG 101398 ADDED SET OF LCSCAN-REVOLVING = 'N' AND STOPPED ENTRY
      *             OF REVOLVING CLASSES, REGENCY REVOL
      *  JTG 101298 SET PDSCAN-REVOLVING = 'N', REGENCY REVOL
      *  JTG 102098 ADDED SET OF RSVENT-LOANTYPE
      *  JTG 122198 REPLACED SP-SCFRMLA = 1 OR 8 OR 28 TESTS
      *             WITH 88 SP-SCFRMLA-POINTS
      *  JTG 990113 ADDED SP-EDT-TYPE 'P' POINTS,                   REGENCY #012
      *  GLF 990312 ADD WI/TFCCDS FOR TFC ENTRY OF LPO,REP CODE,PROCESS CODE
      *  JTG 990317 ADDED SP-DLDISC-RVPRIN1-PCNT,                       TFC #008
      *                   SP-LNCOST-FRMLA-VALID VALUE "D"
      *  GLF 990607 PROVIDE FOR F6 SCAN OF INSURANCE COMPANY NUMBER
      *  MJD 990722 FIXED AFP-DEALER/CLASS TEST TO COUNT " " AS "N".
      *  MJD 990727 STOPPED ENTRY OF DLRVCHGS(1-3) IF IB ACCOUNT. ADDED
      *             PASSWORD TO DLPARVHELD IF IB ACCOUNT
      *  MJD 990806 ADDED GP-INCL-OTH-IN-EARNED FLAG. IF "Y" THEN INCLUDE
      *             EARNINGS BUCKET 6 (OTHER) IN ACCUMULATED TOTALS
      *             OTHERWISE EXCLUDE BUCKET 6.
      *  MJD 990817 ADDED TEST OF PURFLD-STAT AFTER PERFORMING
      *             COMPUTE-PURCHASE-FIELDS
      *  BAH 991028 WAS TESTING SP-ERNFRMLA(5) FOR DISCOUNT ENTRY BEFORE THE
      *             SPR WAS EVEN READ !! MOVED FROM TOP OF ENTRY-MODULE TO
      *             ENTER-ELE.  REGENCY LORI, PL# 249
      * MJD 991109 FORCE INPUT OR EXP DATE ON O3 BY DAFAULTING TO 00/00/00
      *            INSTEAD OF MATURITY DATE
      *            IF A SAVERD LOAN IS RECALLED, DO NOT RESET EXP DATE
      *            ON O3 INS (REGACC PR #985)
      * BAH 991111 PASSWORD COVERAGE FIELD FOR CALCULATED INSURANCES,
      *            CL,AH,O1  REGACC PR# 984
      * JTG 991215 ADDED SP-DLDISC-RVPRIN1-AF-PCNT, TFC #058
      * JTG 000104 CHANGED SP-DLDISC-RVPRIN1-AF-PCNT, TFC #058
      *            TO ACCEPT THE AMOUNT INPUTED IN RVPRIN1 WHEN IT IS LESS
      *            THAN TO COMPUTED AMOUNT
      * JTG 000113 ADDED SP-INSREJECT-FG OPTION 'C', WHICH WILL CAUSE WRITING
      *            NO. OF MONTHS INSURANCE NOT IN VIOLATION, WISCONSIN #1032
      * MJD 000207 ADDED GP-OT-INS-TERM (TFC PR#48) IF 'Y' THEN ALLOW
      *            ALLOW TERM ENTRY FOR O2O3O4O5 INSURANCE.
      *  MJD 000224 ADDED PASSWORD CHECK FOR REGMGM. IF THEY ENTER
      *             A PD-OVRDRATE-FG NOT = TO "A" THEN CALL VERIFY-PASSWORD.
      *             (REGMGM PR# 1006)
      *  MJD 000229 ADDED GP-ADV-TO-1ST-NO-CORR = "A". IF DATE OF THE LOAN
      *             IS ON THE 26 THRU 31 THEN ADVANCE THE FIRST PAYMENT
      *             DUE DATE TO THE FIRST OF THE SECOND MONTH FOLLOWING THE
      *             DATE OF THE LOAN. (REGMGM PR#1099)
      *  BAH 000313 TEST SP-DLDISC-RVPRIN1-PCNT AND SP-DLDISC-RVPRIN1-AF-PCNT
      *              NOT = 202.02, KEVIN
      *  JTG 000315  CORRECTED BUG WITH SRCD EX. FB AND PRIOR ACCOUNT NO. ENTRY
      *              WISC #71249 J006
      *  MJD 000518 ADDED SCFRMLA 39 - IN THIS PROGRAM IT FOLLOES THE SAME
      *             LOGIC AS 33 (SUPPORT PR#1181)
      *  KEC 000530 {PR#00048} TFC [JUDY BRANIFF];
      *             ADDED THE FOLLOWING 'IF' CONDITION FOR ENTERING THE
      *             INSURANCE TERM (GP-OT-INS-TERM = "Y"):
      *                   IF ( VDUNUM0 > 360 )
      *                      GO TO ENTER-IC-TERM.
      * MJD 000605 CHANGED PROCEED-VALIDATION LOGIC TO WORK LIKE XONPC0.
      *            SKIP THE TEST ON PROCEEDS AMOUNT LOW. (AS PER JOHN)
      *
      * JTG 000607 ADDED LOGIC FOR TESTING CALCZ3.C BY SETTING ENVIRONMENT
      *            VARIABLE "CALL_TESTZ3" WHICH MAKE THE CALL TO
      *            "LP/TESTZ3" INSTEAD OF "LP/CALCZ3"
      * MJD 000615 ADDED PASSWORD TO APR RATE (OVERRID RATE)
      * MG  000629 ADDED LOGIC FOR TENNESSEE RATE TYPE 'M' FLAT MONTHLY
      *            CHARGE, PR#1225
      * JTG 011001 DON'T ALLOW A FIRST PAY DAY OF 31 WHEN SEMI MONTHLY PAPER
      *            ADDED 9 1STPYDATE < 30 DAYS FROM LOAN DATE     FFG PR#333
      * JTG 010831 TESTED COP-RETURN-CODE = 'V' FOR DISPLAY OF ERROR IN CALC
      *                                                        LENDMARK PR#J021
      * JTG 020220 ADDED "CT" COUNTY TAX CODE KENTUCKY AND PERCENTS
      *            ADDED "CY" CITY   TAX CODE KENTUCKY AND PERCENTS  WORLD #288
      * BLV 020820 IF GP-ENTER-LPO-FG = "Y", OPEN TFC WINDOW FOR ENTRY OF
      *            LPO, SOAUTO PR# 1885
      * JTG 020905 ADDED LN-DLCOMM-03-CMSI LN-DLCOMM-PP-CMSI  REGACC #1846
      * CS  021008 DON'T ALLOW A FIRST PAY DAY OF 14, 15 OR 31 IF SEMI-MONTHLY
      *            PER JTG. CAUSED LATE CHARGE PROBLEM AT FFG, LATE CHARGE
      *            CYCLE DAYS WERE ALTERED IN FEB, FOR EXAMPLE: IF DUE DAY
      *            15/30, FEB SET DAY TO 28, THEN MARCH SET TO 13TH INTEAD OF
      *            15TH.
      *  JTG 031010 ADDED LOGIC FOR IB SALES FINANCE AMORTIZATED PARTICIPATION
      *             RESERVE PAID EXPENSE, DIFFERENCE BETWEEN A BUY RATE
      *             AND THE CONTRACT RATE                WISCONSIN FINANCE #2049
      *  BAH 031023 ADDED GP-DEFAULT-O3-INSOURS, LENDMARK PR# 2180
      *  BAH 040304 ADDED SP-DEFAULT-O2-INS-COMP FOR LENDMARK PR# 2258
      *  BAH 050222 WHEN ENTERRING SUB CLASS, YOU DONT KNOW THE LOANDATE YET TO
      *             CHECK IF SPR IS EXPIRED, SO IT WAS JUST GRABBING THE FIRST
      *             LAWCODE FOUND TO DISPLAY THE DESCRIPTION, WHICH ISNT
      *             CORRECT IF THEY EXPIRE AND BUILD A NEW ONE WITH A DIFFERENT
      *             DESCRIPTION, VERY CONFUSING...AT LEAST SKIP EXPIRED SPR.
      *             RIVERSIDE PL# 467
      *  BAH 050404 FIXED MISTAKE FROM 050222...
      *   CS 050426 IF INSURANCE PREMIUM IS ZERO, ZERO OUT ASSOICATED FIELDS
      *             INSEFF, INSEXP, INSCOVR, INSCOMM, OURS,... PREVIOUSLY
      *             WHEN USING A FLAT RATE WE COMPUTED A COMMISSION WHEN
      *             PREMIUM WAS ZERO... NOT SURE WHY WE ARE COMPUTING A
      *             COMMISSION IN THE QUOTERS?
      * JTG 050819 CHANGED SAVE LOAN, ETC TO USE PD-ANTICERN(4) FOR DISPLAY
      *            OF MAINTENANCE FEES INSTEAD OF THE STORED TRUNCTATED
      *            PD-MAINTFEE * PD-ORGTERM WHICH DOSEN'T EQUAL THE COMPUTED
      *            'VAT' VALUE ADDED TAX IN PD-ANTICERN(4)           WORLD #0408
      * BAH 051128 UNDO CHANGE FROM 050222 PL# 467, WOULD NOT LET YOU BOOK A
      *            LOAN WITH AN EXPIRED SPR, AND YOU SHOULD BE ABLE TO.
      *            INSTEAD OF SKIPPING EXPIRED SPR IN READ-NEXT, JUST REDISPLAY
      *            THE SUBCLASS DESCRIPTION AFTER ENTERING THE LOANDATE.
      *  CS 070320 WHEN YOU SAVED A QUOTE & LATER RECALLED IT THE SAME DAY, YOU
      *            GOT 'ERROR IN CALCULATIONS', IT NEVER WENT THRU CALCULATOR
      *            AND THE COP-RETURN-CODE WAS SPACES, GOOD IS 'A'
      * JTG 070326 WORLD SPR'S ARE FLAGGED WITH GP-ADV-TO-1ST-NO-CORR = "Y"
      *            WHICH WILL ADVANCE TO THE NEXT MONTH IF THE DEFAULT
      *            PAYMENT DUE DATE FALLS ON A NON EXISTING DAY
      *            THIS CHANGE OVERRIDES THIS OPTION FOR LOANS BOOKED WHEN
      *            GP-I-AM-WORLD AND PD-ORGST = "TN" AND PD-CLASS = 01 AND
      *            PD-ORGTERM = 12                                   WORLD 0532
      * BAH 070920 WAS NOT VERIFYING THAT THE GI RECORD EXIST ! EXCEL PL# 603
      *  CS 080508 ADDED CHECK-SP-EFFECTIVE TO VIA-QUOTE-SCAN. WHEN A QUOTE
      *            WAS SAVED UNDER A PRIOR LAW CODE & RECALLED AT A LATER DATE
      *            WHEN A NEW LAWCODE WAS IN EFFECTIVE & SAVED, CHANGED THE
      *            DATE OF THE QUOTE, BUT NOT LAWCODE.  WHEN THAT SAVED QUOTE
      *            WAS RECALLED IN S/F & NO CHANGE TO LOAN DATE WAS ENTERED,
      *            LOAN GOT UPDATED WITH WRONG LAWCODE, THIS WORKS BECUASE
      *            IF PD-LOANDATE NOT = TRDATE, IT GOES THRU CALCULATOR
      *                                                    PR#627 REGMGM
      *  CS 081121 ADD TEST GP-INCL-OTH-IN-EARNED-R. IF 'R' EXCLUDE OTHER
      *            (ANTICERN 6) AND MAINTFEE(ANTICERN(4) FROM ANTICIPATED
      *                                                    REGACC PR#210
      *  CS 081218 ADD BR-FILE REGACC PR#210
      * BAH 090904 ALLOW SP-INSMAXCHGABL-OVRIDE TO WORK WITH O1, FINMGM PR# 3447
      * BAH 091015 PASS SFDLO3-ORGST TO CL/SFDLO3 FOR REGMGM PR# 3475
      * MJD 100715 FIXED TO CORRECTLY ALLOW MAINTFEE'S OVER 99.99 FOR MEXICO.
      *                WORLD #692
      * BAH 110523 PASSWORD PROTECT ADDON SOURCE CODES, ET, LP, NP, RT, ST
      *            AND RB, REGMGM PR# 3792
      * MJD 111213 ADDED LOGIC FOR SP-ADV-TO-1ST-NO-CORR
      *            IF SET THIS WILL OVERIDE THE GP FLAG.             71226 #3879
      * BAH 111229 ADDED PP-PREM-SEQ TO PASSWORD CHANGING PP PREM REGWAN PR#3935
      * BAH 130131 A30 CONVERSION
      * MJD 140429 REPLACED YIELD WITH WS-YIELD, RESERVE WORD
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
      * CS 160805 CHANGED SCSCAN-KEY TO SCSCAN-SP1-KEY
      *           REMOVED LOANNOW,LNSEQNW
      *           REMOVE TEST ON PASS-LN-MORELN
      * MJD 160824 ADDED LC-CALC-HI-CREDIT = "L"              61000/1180
      *
      * KEC 2017.0227 {PR#00000} PARADATA <A30> [JAY CISZEWSKI]
      *         CHANGED PROCEEDS-LOW, PROCEEDS-HIGH, POINTS-LOW,
      *          POINTS-HIGH TO MATCH SIZE OF SP-EDT-AMT 9(7)V99.
      *  BAH 170424 ACTIVATE 8 DIGIT DATES, PD0006
      *   CS 181017 WHEN ENTERING DEAL, GOT DOWN TO SELL PRICE, UP-ARROWED
      *             TO PAYCD/FREQ, CHANGED TO 'B','S','W' AND SAVED LOAN.
      *             WHEN RECALLING IN SF INPUT, AND UPDATING, IT SET THE
      *             CORRECT UNITPER CODE IN THE LOAN RECORD BUT THE PAYS
      *             PER YEAR/DAYS PER PERIOD WERE THE SAME AS THE DEFAULT
      *             IN THIS CASE 'M'. NOT SURE WHAT THAT MIGHT EFFECT.
      *             SO FOR MY BI-WEEKLY LOAN I HAD PAYFREQ 1, PAYS PER YEAR
      *             12, DAYS PER PERIOD 30.  I CHANGED ENTRY IN F-11 TO SAVE
      *             THE PD-UNITPER-CD TO HOLD-UNITPER-CD AND THEN COMPARED
      *             IT TO THE NEW PD-UNITPER-CD. IF NOT THE SAME PERFORMS
      *             SET-UNITPER-INFO.
      * BAH 20190625 REMOVED LC-REVOLVING, PT#0153
      * BAH 20200615 MOVED OPEN-IC1-FILE, OPEN-SP1-FILE, OPEN-CD1-FILE,
      *              OPEN-BW1-FILE TO TOP OF PROGRAM #TST001
      * BAH 20201207 CHANGE VIA-QUOTE-SCAN, DONT MOVE IN THE PDSCAN-NAME,
      *              YOU HIT END OF FILE WHEN ONLY 1 QUOTE IN THE PDFILE.
      *              MOVE SPACES TO PD2-BWNAME AND ADD A CHECK OF THE
      *              PD2-BWNAME MATCHES THE PDSCAN-BWNAME OR GO GET NEXT.
      * BAH 20230314 FIXED CHECK ON SP-EDT-NOEDITS CINDY FOUND
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
      * ONLY USED FOR WK-SAVE-RECORD AS A HOLDING AREA
       COPY "LIBLP/LPFSWK.CPY".
       I-O-CONTROL.
       DATA DIVISION.
       FILE SECTION.

      * ONLY USED FOR WK-SAVE-RECORD AS A HOLDING AREA
       COPY "LIBLP/LPFDWK.CPY".

       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(37) VALUE
           "@(#)WI/SFQUOT S35 05/03/24-14:27:22 >".
      **********************************
      *       F I L E   P A T H S      *
      **********************************
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
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
       COPY "LIBGL/GL01GI.CPY".
       COPY "LIBGL/GL01GI_SQL.CPY".
       COPY "LIBGL/GLWSGI.CPY".

       01  SFQUOT-LINES          PIC 99  VALUE 26.
       01  SFQUOT-COLUMNS        PIC 99  VALUE 82.
       01  SFQUOT-BEGIN-Y        PIC 99  VALUE 1.
       01  SFQUOT-BEGIN-X        PIC 99  VALUE 1.

       01  WK-KEY          PIC 9(2).
       01  HOLD-UNITPER-CD PIC X.

       01  MSEL-LIST       PIC X(5)     VALUE "MSEL.".
       01  WSEL-LIST       PIC X(5)     VALUE "WSEL.".
       01  WS-YIELD        PIC 999V999.
       01  ONE             PIC S9.
       01  ELE             PIC 9(6) COMP.
       78  CL-ELE          VALUE 27.
       78  AH-ELE          VALUE 28.
       78  PP-ELE          VALUE 29.
       78  O1-ELE          VALUE 30.
       78  O2-ELE          VALUE 31.
       78  O3-ELE          VALUE 32.

       01  APR-OK          PIC X         VALUE "N".


       01  FEECODE-TABLE   VALUE " ".
           03  FEECODE     OCCURS 4 TIMES
                           PIC XX.
       01  CDCODE-FEE-FG   PIC 9.
       01  LEGEND          PIC X(70).

       01  INS-COMM         PIC 9(6)V99  COMP.
       01  NON-MEMO-CHGS    PIC 9(6)V99  COMP.
       01  NOT-OUR-CHGS     PIC 9(6)V99  COMP.
       01  OUR-CHGS         PIC 9(6)V99  COMP.
       01  WORK-RATE        PIC 99V9(4)  COMP.
       01  TEST-PROCEEDS    PIC S9(7)V99 COMP.
       01  HOLD-TOTAL-REPAY PIC S9(7)V99 COMP.
       01  FORCE-ENTERED-PRIN1     PIC X    VALUE "N".
       01  FORCE-ENTERED-PRIN2     PIC X    VALUE "N".
       01  FORCE-ENTERED-DISC      PIC X    VALUE "N".

       01  HOLD-VDUSTATUS                       PIC X(02)  VALUE "00".
       01  ADV-TO-1ST-NO-CORR    PIC X       VALUE " ".

       01  SFQUOT-WINDOW-HANDLE  PIC X(10).
       01  ACTION.
           03  CHKEY       PIC X(4).
           03  CHNO        PIC 9(2).
       01  ANTIC           PIC S9(6)V99 COMP.
       01  IO-TYPE         PIC X(3).
       01  NEW-FLAG        PIC 9.
       01  FIRST-TIME      PIC 9  VALUE 0.
       01  FIRST-ELE       PIC 99 VALUE 0.
       01  NOREG-FG        PIC X  VALUE " ".
       01  SAVE-SSNO       PIC 9(10) COMP.
       01  HOLD-BWNAME        PIC X(33).

      *    ----------------------------------------------------
      *    SINCE QUOTERS DON'T HAVE BORROWERS THESE WORKERS WILL
      *    BE PASSED TO CALCZL/CALCZ3, '   ' == NO CODE
      *    ----------------------------------------------------
       01  STATE                      PIC X(2)   VALUE " ".
       01  CITY-TAX-CD                PIC X(3)   VALUE " ".
       01  COUNTY                     PIC X(3)   VALUE " ".

       01  RVNO            PIC 9(6) COMP.
       01  SUB             PIC 9(6) COMP.
       01  OVER-20         PIC 99.
       01  I-LEVEL         PIC 99   COMP.
       01  INSPECT-W       PIC X(86).
       01  ADD-ON          PIC X(25).
       01  SAVE-BW-NAME    PIC X(24)    VALUE SPACES.
       01  BORR-CHG-FG     PIC X        VALUE " ".
       01  RECALL-ID-FG    PIC X        VALUE " ".
           88  RECALL-UPDATE            VALUE "Y".
       01  QUOTE-ID-FG     PIC X        VALUE " ".
           88  QUOTE-ONLY               VALUE "Q".
           88  QUOTE-CHANGED-TO-LOAN    VALUE "L".
       01  PRIOR-LOAN-STAT PIC X.
           88  ERROR-IN-PRIOR-LOAN      VALUE "E".
       01  LEGEND-01.
           03  FILLER                   PIC X(30) VALUE
               "F7-MENU, RECALL F3-LOANS/F4-QU".
           03  FILLER                   PIC X(30) VALUE
               "OTES, F6-SCAN, ENTER LN CLASS:".
       01  LEGEND-02.
           03  FILLER                   PIC X(30) VALUE
               "F7-MENU, RECALL F3-LOANS/F4-QU".
           03  FILLER                   PIC X(30) VALUE
               "OTES, ENTER LOAN CLASS:".
       01  LEGEND-03.
           03  FILLER                   PIC X(30) VALUE
               "F7-MENU, F4-RECALL QUOTES, F6-".
           03  FILLER                   PIC X(21) VALUE
               "SCAN, ENTER LN CLASS:".
       01  LEGEND-04.
           03  FILLER                   PIC X(38) VALUE
               "F7-MENU, F4-QUOTES, ENTER LOAN CLASS:".

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

       01  CALL-LOAN-CALC-FG  PIC X      VALUE "Y".
      ********************************
      *   S C R E E N   B U F F E R  *
      ********************************
       01  DISPLAY-BUF1        VALUE " ".
           03  D-LCNO          PIC Z(8)9.
           03  D-LC-DESC       PIC X(16).
           03  D-010-040       OCCURS 3.
               05  D-010       PIC X(11).
               05  D-011       PIC X(33).
               05  D-012       PIC X(1).
           03  D-LOANAMT       PIC Z(6)9.99- BLANK ZERO.
           03  D-DLCK          PIC Z(5)9.99- BLANK ZERO.
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
           03  D-090           PIC 9(4).99   BLANK ZERO.

           03  D-091           PIC X(15).
           03  D-100           PIC X(2).
           03  D-101           PIC X(15).
           03  D-110           PIC X.
           03  D-111           PIC 999.
           03  D-112           PIC X.
           03  D-120           PIC ZZ9.
           03  D-130           PIC 99/99/99  BLANK ZERO.
           03  D-140           PIC Z(5)9.99- BLANK ZERO.
           03  D-141           PIC X.
           03  D-150           PIC Z(5)9.99- BLANK ZERO.
           03  D-151           PIC X.
           03  D-160           PIC ZZ9.99-   BLANK ZERO.
           03  D-16P           PIC X.
           03  D-161           PIC X.
           03  D-170           PIC Z(5)9.99- BLANK ZERO.
           03  D-171           PIC X.
           03  D-180           PIC X(3).
           03  D-181           PIC Z9.99     BLANK ZERO.
           03  D-182           PIC X.
           03  D-190           PIC Z(3)9.99  BLANK ZERO.
           03  D-1901          PIC X.

       01  DISPLAY-BUF2        REDEFINES DISPLAY-BUF1.
           03  D-200           PIC Z(3)9.99  BLANK ZERO.
           03  D-201           PIC X.
           03  D-210           PIC Z(3)9.99  BLANK ZERO.
           03  D-211           PIC X.
           03  D-DLRVPRIN      PIC Z(4)9.99  BLANK ZERO.
           03  D-DLRVPRIN-FG   PIC X.
           03  D-DLRVCHGS      PIC Z(4)9.99  BLANK ZERO.
           03  D-DLRVCHGS-FG   PIC X.
           03  D-DLPARV        PIC Z(4)9.99  BLANK ZERO.
           03  D-DLPARV-FG     PIC X.
           03  D-DLDISC        PIC Z(4)9.99  BLANK ZERO.
           03  D-DLDISC-FG     PIC X.
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
           03  DEF-SRCD         PIC XX.
           03  DEF-UNITPER-CD   PIC X.
           03  DEF-UNITPER-FREQ PIC ZZ9.
           03  DEF-BALLBL       PIC X.
           03  FILLER           PIC X(350).

       01  DF2-BUF             REDEFINES DISPLAY-BUF1.
           03  DF2-SELLPR            PIC Z(5)9.99- BLANK ZERO.
           03  DF2-SELLPR-FG         PIC X.
           03  DF2-DOWNPY            PIC Z(5)9.99- BLANK ZERO.
           03  DF2-DOWNPY-FG         PIC X.
           03  DF2-PERDOWN           PIC ZZ9.99-   BLANK ZERO.
           03  DF2-PERDOWNP          PIC X.
           03  DF2-PERDOWN-FG        PIC X.
           03  DF2-BALLOON           PIC Z(5)9.99- BLANK ZERO.
           03  DF2-BALLOON-FG        PIC X.
           03  DF2-POINTS            PIC Z9.99     BLANK ZERO.
           03  DF2-POINTS-FG         PIC X.
           03  DF2-FEE-1             PIC Z(3)9.99  BLANK ZERO.
           03  DF2-FEE-1-FG          PIC X.
           03  DF2-FEE-2             PIC Z(3)9.99  BLANK ZERO.
           03  DF2-FEE-2-FG          PIC X.
           03  DF2-DLRVPRIN          PIC Z(4)9.99  BLANK ZERO.
           03  DF2-DLRVPRIN-FG       PIC X.
           03  DF2-DLRVCHGS          PIC Z(4)9.99  BLANK ZERO.
           03  DF2-DLRVCHGS-FG       PIC X.
           03  DF2-DLPARV            PIC Z(4)9.99  BLANK ZERO.
           03  DF2-DLPARV-FG         PIC X.
           03  DF2-DLDISC            PIC Z(4)9.99  BLANK ZERO.
           03  DF2-DLDISC-FG         PIC X.
           03  DF2-OVRDRATE          PIC ZZ9.999   BLANK ZERO.
           03  DF2-OVRDRATE-FG       PIC X.
           03  DF2-EFFRATE           PIC ZZ9.999   BLANK ZERO.
           03  DF2-APRATE            PIC ZZ9.999   BLANK ZERO.
           03  DF2-YIELD             PIC ZZ9.999   BLANK ZERO.
           03  DF2-LNAMT             PIC Z(6)9.99- BLANK ZERO.
           03  DF2-DLCK              PIC Z(5)9.99- BLANK ZERO.
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
           03  DPUR-DLRVPRIN         PIC Z(4)9.99  BLANK ZERO.
           03  DPUR-DLRVPRIN-FG      PIC X.
           03  DPUR-DLRVCHGS         PIC Z(4)9.99  BLANK ZERO.
           03  DPUR-DLRVCHGS-FG      PIC X.
           03  DPUR-DLPARV           PIC Z(4)9.99  BLANK ZERO.
           03  DPUR-DLPARV-FG        PIC X.
           03  DPUR-DLDISC           PIC Z(4)9.99  BLANK ZERO.
           03  DPUR-DLDISC-FG        PIC X.
           03  DPUR-YIELD            PIC ZZ9.999   BLANK ZERO.
           03  DPUR-LNAMT            PIC Z(5)9.99- BLANK ZERO.
           03  DPUR-DLCK             PIC Z(5)9.99- BLANK ZERO.
           03  DPUR-ANTIC            PIC Z(5)9.99- BLANK ZERO.

       01  DPUR-LIST.
           03  FILLER          PIC X(36)  VALUE
           "220 221 230 231 240 241 250 251 264 ".
           03  FILLER          PIC X(19)  VALUE
           "LOANAMT DLCK ANTIC.".


       01  DEF-LIST               PIC X(36)  VALUE
           "050 051 060 061 070 080 110 111 112.".

       01  FEE-LIST               PIC X(8)   VALUE
           "190 191.".

       01  DISPLAY-LIST1.
           03  FILLER             PIC X(49)  VALUE
           "LNKEY LCDESC 010 011 012 020 021 022 030 031 032 ".
           03  FILLER             PIC X(43)  VALUE
           "LOANAMT DLCK ANTIC 050 051 052 053 060 061 ".
           03  FILLER             PIC X(20)  VALUE
           "070 071 080 081 090 ".
           03  FILLER             PIC X(52)  VALUE
           "091 100 101 110 111 112 120 130 140 141 150 151 160 ".
           03  FILLER             PIC X(36)  VALUE
           "16P 161 170 171 180 181 182 190 191.".

       01  DISPLAY-LIST2.
           03  FILLER          PIC X(52)  VALUE
           "200 201 210 211 220 221 230 231 240 241 250 251 260 ".
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
           03  FILLER          PIC X(56)  VALUE
           "140 141 150 151 160 16P 161 170 171 181 182 200 201 210 ".
           03  FILLER          PIC X(56)  VALUE
           "211 220 221 230 231 240 241 250 251 260 261 262 263 264 ".
           03  FILLER          PIC X(27)  VALUE
           "LOANAMT DLCK ANTIC 270 271 ".
           03  FILLER          PIC X(56)  VALUE
           "272 273 274 275 280 281 282 283 284 285 290 291 292 293 ".
           03  FILLER          PIC X(56)  VALUE
           "294 295 300 301 302 303 304 305 340 341 350 351 360 361 ".
           03  FILLER          PIC X(40)  VALUE
           "370 380 381 390 400 410 420 421 430 440.".

       01  PROMPT-FG        PIC X      VALUE " ".
       01  PROMPT-BUF.
           03  FILLER       PIC X(13)  VALUE "LOAN AMOUNT :".
           03  FILLER       PIC X(13)  VALUE "DEALER CHECK:".
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
           03  PROMPT-201   PIC X(13)   VALUE "FEE1        :".
           03  PROMPT-210   PIC X(2)   VALUE "21".
           03  PROMPT-211   PIC X(8)   VALUE "FEE2   :".
           03  FILLER       PIC X(2)   VALUE "22".
           03  FILLER       PIC X(8)   VALUE "RVPRIN :".
           03  FILLER       PIC X(2)   VALUE "23".
           03  FILLER       PIC X(8)   VALUE "RVCHGS :".
           03  FILLER       PIC X(2)   VALUE "24".
           03  FILLER       PIC X(8)   VALUE "PARTIC :".
           03  FILLER       PIC X(2)   VALUE "25".
           03  PROMPT-251   PIC X(8)   VALUE "DLDISC :".
           03  FILLER       PIC X(17)  VALUE "-----RATES-----FG".
           03  FILLER       PIC X(7)   VALUE "26 OVR:".
           03  FILLER       PIC X(7)   VALUE "   EFF:".
           03  FILLER       PIC X(7)   VALUE "   APR:".
           03  FILLER       PIC X(7)   VALUE "   YLD:".

       01  PROMPT-LIST.
           03  FILLER          PIC X(50)  VALUE
             "PLOANAMT:S PDLCK:S PANTIC:S INSPROMPT:S OUTPROMPT ".
           03  FILLER          PIC X(49)  VALUE
             "P270:S P280:S P290:S P300:S P310:S P320:S P340:S ".
           03  FILLER          PIC X(21)  VALUE
             "P350:S P360:S P370:S ".
           03  FILLER          PIC X(49)  VALUE
             "P380:S P390:S P400:S P410:S P420:S P200:S P201:S ".
           03  FILLER          PIC X(51)  VALUE
             "P210:S P211:S P220:S P221:S P230:S P231:S P240:S ".
           03  FILLER          PIC X(52)  VALUE
             "P241:S P250:S P251:S PRATES:S P260:S P262:S P263:S ".
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
      * 8  PD-SRCD
           03  FILLER      PIC X(8) VALUE "E  A030 ".
      * 9  PD-DLNO
           03  FILLER      PIC X(8) VALUE "ENNN042 ".
      * 10 PD-COLLCD
           03  FILLER      PIC X(8) VALUE "E  A030 ".
      * 11 PD-UNITPER-CD
           03  FILLER      PIC X(8) VALUE "E  A010N".
      * 12 PD-ORGTERM
           03  FILLER      PIC X(8) VALUE "ENNN030 ".
      * 13 PD-1STPYDATE
           03  FILLER      PIC X(8) VALUE "ERNM060N".
      * 14 PD-SALE PRICE
           03  FILLER      PIC X(8) VALUE "ENNS062 ".
      * 15 PD-DOWN PAYMENT
           03  FILLER      PIC X(8) VALUE "ENNS062 ".
      * 16 PD-%DOWN
           03  FILLER      PIC X(8) VALUE "ENNS032 ".
      * 17 PD-LASTPYAMT
           03  FILLER      PIC X(8) VALUE "ENNS062N".
      * 18 PD-INSURED
           03  FILLER      PIC X(8) VALUE "E  A030 ".
      * 19 PD-FEEAMTS(X)
           03  FILLER      PIC X(8) VALUE "E  A020 ".
      * 20 PD-FEEAMT(1)?
           03  FILLER      PIC X(8) VALUE "ENNN042 ".
      * 21 PD-FEEAMT(2)?
           03  FILLER      PIC X(8) VALUE "ENNN042 ".
      * 22 PD-DLRVPRIN
           03  FILLER      PIC X(8) VALUE "ENNN052 ".
      * 23 PD-DLRVCHGS
           03  FILLER      PIC X(8) VALUE "ENNN052 ".
      * 24 PD-DLPARV
           03  FILLER      PIC X(8) VALUE "ENNN052 ".
      * 25 PD-DLDISC
           03  FILLER      PIC X(8) VALUE "ENNN052 ".
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
           03  FILLER      PIC X(8) VALUE "ENNN062 ".
      * 38 PD-FINAMT
           03  FILLER      PIC X(8) VALUE "ENNN062 ".
      * 39 PD-LOANAMT
           03  FILLER      PIC X(8) VALUE "ENNN062 ".
      * 40 PD-1STPYAMT
           03  FILLER      PIC X(8) VALUE "ENNN062 ".
      * 41 PD-LASTPYAMT
           03  FILLER      PIC X(8) VALUE "ENNN062 ".
      * 42 PD-REGPYAMT
           03  FILLER      PIC X(8) VALUE "ENNN062 ".
       01  SPEC-TABLE2     REDEFINES SPEC-TABLE.
           03  SPEC-REC    OCCURS 42 TIMES.
               05  SPEC    PIC X(7).
               05  CHFG    PIC X.

       01  ERRCD             PIC X.
       01  LOAN-QUOTE-FG     PIC X.

       01  TRANS-DATE        PIC 9(8).

       01  DATE-STAT       PIC X.
       01  PRIOR-LNAMT     PIC S9(7)V99 COMP.
       01  COUNTER         PIC 99.
       01  COMPARE-TO-HOLDERS.
           03  PROCEEDS-FG-HLDR     PIC X.
           03  PROCEEDS-HLDR        PIC S9(9)V99 COMP.
           03  SELLPR-HLDR          PIC S9(9)V99 COMP.
           03  DOWNPY-HLDR          PIC S9(9)V99 COMP.
           03  PERDOWN-HLDR         PIC S9(3)V99 COMP.
           03  AMT-HOLDER           PIC S9(9)V99 COMP.
           03  MAINTFEE-HLDR        PIC S9(6)V99 COMP.
           03  TOTAL-GL-HLDR        PIC S9(5)V99 COMP.
           03  TOTAL-GL-FINCHG-HLDR PIC S9(5)V99 COMP.
           03  INSPREM-HLDR         PIC S9(4)V99 COMP.
           03  INSTERM-HLDR         PIC 9(3)     COMP.
           03  INSOURS-HLDR         PIC X.
           03  CALC-FG-HLDR         PIC X.
           03  INSCODES-HLDR        PIC X.

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
       01  SAVE-MAINTFEE        PIC 9(6)V99    COMP.
       01  TERM-LOW             PIC 9(3)    COMP.
       01  TERM-HIGH            PIC 9(3)    COMP.
       01  PROCEEDS-LOW         PIC 9(7)V99 COMP.
       01  PROCEEDS-HIGH        PIC 9(7)V99 COMP.
       01  POINTS-LOW           PIC 9(7)V99 COMP.
       01  POINTS-HIGH          PIC 9(7)V99 COMP.


       COPY "LIBCL/SFDLO3W.CPY".
       COPY "LIBLP/PAYOFXW.CPY".
       COPY "LIBLP/PURFLDW.CPY".
       COPY "LIBLP/LPWKPATH.CPY".
       COPY "LIBGB/MKTEMPW_CMD.CPY".
       COPY "LIBGB/SYSTEMW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/HASHW.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/GPENVW.CPY".
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
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBLP/LPBAGEW.CPY".

       COPY "LIBWI/FEEENTW.CPY".
       COPY "LIBWI/RSVENTW.CPY".
       COPY "LIBWI/SCHG33W.CPY".
       COPY "LIBWI/DLSCANW.CPY".
       COPY "LIBWI/ICSCANW.CPY".
       COPY "LIBWI/LCSCANW.CPY".
       COPY "LIBWI/PDSCANW.CPY".
       COPY "LIBWI/SFINPCW.CPY".
       COPY "LIBWI/SCSCANW.CPY".
       COPY "LIBWI/SFCALCW.CPY".
       COPY "LIBWI/CDSCANW.CPY".
       COPY "LIBWI/CDBSCNW.CPY".
       01  WINDOW-WORKERS.
           03  WINDOW-LPO          PIC X(4).
           03  WINDOW-REP          PIC X(8).
           03  WINDOW-PROCESS      PIC X(1).

      ************(WINDOW INFORMATION):
       COPY "LIBWI/PFSWINDOWSW.CPY".

       COPY "LIBLP/CALCPVW.CPY".
       COPY "LIBLP/LPAPRZW.CPY".
       COPY "LIBWI/BUYRTEW.CPY".
       COPY "LIBGB/PASSWDW.CPY".
       78  DLRSV-SEQ                   VALUE 1.
       78  APR-SEQ                     VALUE 3.
       78  INSCO-SEQ                   VALUE 4.
       78  INSCOVR-SEQ                 VALUE 6.
       78  C-OR-E-SEQ                  VALUE 8.
       78  1STPY30-SEQ                 VALUE 9.
       78  SRCD-SEQ                    VALUE 17.
       78  PP-PREM-SEQ                 VALUE 18.

       COPY "LIBLP/LPICOMMW.CPY".
       COPY "LIBLP/INSELIGW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBWI/SFQUOT_DEF.CPY".
       COPY "LIBWI/RSVENT_DEF.CPY".
       COPY "LIBWI/FEEENT_DEF.CPY".
       COPY "LIBWI/SFQUOT_WKS.CPY".
       COPY "LIBWI/RSVENT_WKS.CPY".
       COPY "LIBWI/FEEENT_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".

       COPY "LIBGB/GETFMW.CPY".

       SCREEN SECTION.
       COPY "LIBWI/SFQUOT_SCN.CPY".
       COPY "LIBWI/RSVENT_SCN.CPY".
       COPY "LIBWI/FEEENT_SCN.CPY".

       PROCEDURE DIVISION
            USING FORM-PATHNAME EXIT-PATHNAME.

      ***********************************
      *   C O N T R O L   M O D U L E   *
      ***********************************
       MAIN-PROGRAM SECTION.

           PERFORM SQL-CONNECT.

      D    STOP MESS.
           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "SFQUOT" TO VDU-FORM-NAME.

           MOVE "WI/SFQUOT" TO WINDOW-FORM-NAM.

           MOVE " " TO VDU-WIMENU-SELECTION.

           DISPLAY FLOATING GRAPHICAL WINDOW
               AT  SCREEN LINE SFQUOT-BEGIN-X * VDU-WIN-L-OFFSET
               AT  SCREEN COL  SFQUOT-BEGIN-Y * VDU-WIN-C-OFFSET
               CELL SIZE IS LABEL FONT
           COLOR IS VDU-WCOLOR-TCLP,CONTROL VALUE IS VDU-WCTRL-TCLP
               SIZE    SFQUOT-COLUMNS + VDU-WIN-W-MOD
               LINES   SFQUOT-LINES + VDU-WIN-H-MOD
               BLANK SCREEN
               HANDLE IS SFQUOT-WINDOW-HANDLE.

           MOVE EXT-CONAME-SYSDATE TO ALP-DATE.
           PERFORM CONVERT-ALPDATE-TO-YYYYMMDD.
           MOVE DATE-YYYYMMDD TO TRANS-DATE.

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
      * CLEAR REQUIRED RE: (GA) SP-SCFRMLA = 12 OR 13
           MOVE 0 TO SPDUAL-LIAB.

           PERFORM FORM-RESET.

           IF FIRST-TIME = 0
              MOVE 1 TO FIRST-TIME
           ELSE
              MOVE " " TO MESS PROMPT-FG
              PERFORM CALL-TMMSG.

           PERFORM RESET-TITLE.

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

      ***************************************
       ENTER-KEYS SECTION.
      ***************************************
       ENTER-KEYS-NO.
           MOVE 0 TO NEW-FLAG ELE.
           MOVE " " TO IO-TYPE.
           PERFORM NEW-RECORD-SETUP.
           MOVE LEGEND-03 TO LEGEND.
           PERFORM SEND-LEGEND.
           PERFORM LOAN-CLASS-PROMPT.
       ENTER-LC-CODE.
           MOVE "EN N090LNKEY." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "18"
              PERFORM QUOTE-SCAN
              IF IO-FG NOT = 0
                 GO TO ENTER-LC-CODE
              ELSE
                 GO TO VIA-QUOTE-SCAN.
           IF VDUSTATUS = "1<"
              PERFORM CLASS-SCAN
              IF IO-FG NOT = 0
                 GO TO ENTER-LC-CODE.
           IF VDUSTATUS = "1>"
              MOVE "END" TO IO-TYPE
              GO TO EXIT-ENTER-KEYS.
           IF VDUSTATUS NOT = "00"
              GO TO ENTER-LC-CODE.
           IF VDUNUM0 = 0
             OR VDUNUM0 > 99
                GO TO ENTER-LC-CODE.
           MOVE VDUNUM0 TO LC-CODE.
           PERFORM GET-LC.
           IF IO-FG = 9
              PERFORM NOT-FOUND
              GO TO ENTER-LC-CODE.

           MOVE "S  A000LCDESC." TO VDU.
           MOVE LC-DESC TO VDUBUF.
           PERFORM SCREENIO.

           IF ( LC-SALEFIN NOT = "Y" )
              PERFORM NOT-SALEFIN
              GO TO ENTER-LC-CODE.

           MOVE EXT-CONAME-USER-LOC TO GI-BRANCH.
           MOVE LC-CODE TO GI-CLASS.
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

      **************************************
      *    VIA LOAN SCAN
      **************************************
       VIA-LOAN-SCAN.
           PERFORM NEW-RECORD-SETUP.
           MOVE "L" TO PD-ID.
           MOVE EXT-CONAME-USER-LOC TO PD-BRANCH.
           MOVE PDSCAN-NO           TO PD-SOC-SEC.
           PERFORM GET-PD1.
           IF IO-FG = 9
              PERFORM NOT-FOUND
              GO TO EXIT-VIA-LOAN-SCAN.

           PERFORM DISPLAY-FIELDS.
      *  CITIZENS TEST
           MOVE CIP-CALCULATION-INPUT-AREA
                             TO COP-CALCULATION-OUTPUT-AREA.
           PERFORM LOAN-CLASS-PROMPT.
           IF ( LC-SALEFIN NOT = "Y" )
              PERFORM NOT-SALEFIN
              MOVE "CLR" TO IO-TYPE
              GO TO EXIT-ENTER-KEYS.
           MOVE 2 TO NEW-FLAG.
           MOVE "SN N090LNKEY." TO VDU.
           MOVE PD-CLASS TO VDUNUM0.
           PERFORM SCREENIO.
           PERFORM GET-SP.
      *  CITIZENS TEST
           MOVE 800 TO CIP-NO-OF-TIMES-CALLED.
      *    MOVE 0 TO CIP-NO-OF-TIMES-CALLED.
           PERFORM CALL-LOAN-CALCULATIONS.
           MOVE "Y" TO RECALL-ID-FG.

       EXIT-VIA-LOAN-SCAN.
           GO TO EXIT-ENTER-KEYS.

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

           MOVE SPACES              TO PD2-BWNAME.
           MOVE PDSCAN-BWNAME       TO HOLD-BWNAME.

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

           PERFORM PROTECT-FIELDS.
           PERFORM DISPLAY-FIELDS.

      *WHEN QUOTE MADE UNDER PRIOR LAW CODE, RECALLED, SAVED & RECALLED
      *INTO S/F INPUT, WITH NO CHANGE TO LN-DATE FIELD, LOAN GOT UPDATED
      *WITH THE WRONG LAW CODE REGMGM PR#627.
      *CHECK-SP-EFFECTIVE GET THE LAW CODE IN EFFECT & GETS PAST TO THE
      *CALCULATOR IF HOLD-DATE-YYYYMMDD NOT = TRANSACTION DATE AND RECALCULATES
      *THE QUOTE
           PERFORM GET-SP.
           PERFORM CHECK-SP-EFFECTIVE.
           IF SP-LAWCODE NOT = PD-LAWCODE
              MOVE "NEW LAW RECORD EXISTS FOR LOAN DATE!" TO MESS
              PERFORM SEND-MESS
              MOVE "RETRIEVING NEW LAW RECORD, RECALCULATING...!"
                                                          TO MESS
              PERFORM SEND-MESS.


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
           MOVE PD2-BWNAME TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "S  A010012." TO VDU.
           MOVE PD-MAKERCD(1) TO VDUBUF.
           PERFORM SCREENIO.

           MOVE PD-SOC-SEC TO SAVE-QUOTE-SSN.
           PERFORM LOAN-CLASS-PROMPT.
           IF ( LC-SALEFIN NOT = "Y" )
              PERFORM NOT-SALEFIN
              MOVE "CLR" TO IO-TYPE
              GO TO EXIT-VIA-QUOTE-SCAN.

           MOVE 2 TO NEW-FLAG.
           MOVE "SN N090LNKEY." TO VDU.
           MOVE PD-CLASS TO VDUNUM0.
           PERFORM SCREENIO.


           IF HOLD-DATE-YYYYMMDD NOT = TRANS-DATE
                     OR SP-LAWCODE NOT = PD-LAWCODE
                      OR NOT PD-ONE-MTH-NO-DAY-EXT
                       OR PD-PRLNNO NOT = 0
      *  CITIZENS TEST
              MOVE 800 TO CIP-NO-OF-TIMES-CALLED
              PERFORM CALL-LOAN-CALCULATIONS.


       EXIT-VIA-QUOTE-SCAN.
           MOVE SP-LAWCODE TO PD-LAWCODE.
           PERFORM CLOSE-PD1-FILE.
           GO TO EXIT-ENTER-KEYS.

       EXIT-ENTER-KEYS.
           EXIT.

      *************************************************
      *   ENTRY-MODULE
      *   NEW-FLAG = 0 - MODIFY EXISTING RECORD
      *              1 - INITIAL NEW RECORD ENTRY
      *              2 - MODIFY NEW RECORD
      *************************************************
       ENTRY-MODULE SECTION.
           IF LOAN-QUOTE-FG = "Q"
              MOVE "N" TO CHFG(2) CHFG(3) CHFG(4).


      * STOP RESERVE ENTRY FOR SERVICE ACCOUNTS:
           IF PD-SERVACCT = "Y"
              MOVE "N" TO STAT
           ELSE
              MOVE " " TO STAT.
           MOVE STAT TO CHFG(22) CHFG(23) CHFG(24).

      * STOP DLRVCHGS ENTRY FOR IB ACCOUNTS
           IF PD-LOANTYPE = "I"
              MOVE "N" TO CHFG(23)
           ELSE
              MOVE " " TO CHFG(23).

           MOVE "S  A000MSEL." TO VDU.
           PERFORM SCREENIO.
           MOVE " " TO IO-TYPE.
           MOVE 1 TO ONE.
           MOVE FIRST-ELE TO ELE.
           MOVE 0 TO FIRST-ELE.

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
           PERFORM CALL-LOAN-CALCULATIONS.
       MODIFY-ROUTINE.
           MOVE "N" TO CALL-LOAN-CALC-FG.
           IF (BORR-CHG-FG = "Y" AND NOT QUOTE-ONLY)
               MOVE " " TO BORR-CHG-FG
               MOVE "Y" TO CALL-LOAN-CALC-FG
               PERFORM CALL-LOAN-CALCULATIONS.
           MOVE " " TO STAT.
           PERFORM UNPROTECT-FIELDS.
           IF ( DL-PARVPAID-FRMLA    =  3  ) OR
              ( DL-PARVPAID-FRMLA-IB = "3" )
              MOVE "F1, F4-MEMO, F5-BUY, FIELD #, DELETE, S-SAVE:"
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

      * F5 - BUYRATE WINDOW
      
           IF ( VDUSTATUS = "1:" )
              IF ( DL-PARVPAID-FRMLA    =  3  ) OR
                 ( DL-PARVPAID-FRMLA-IB = "3" )
                 PERFORM BUYRATE-WINDOW
                 MOVE "C" TO PD-DLPARVPAID-FG
                 PERFORM SEND-PURCHASE-FIELDS
                 MOVE "I" TO PD-DLPARVPAID-FG
                 MOVE 800 TO CIP-NO-OF-TIMES-CALLED
                 PERFORM CALL-LOAN-CALCULATIONS
                 GO TO MODIFY-ROUTINE.

           IF VDUSTATUS NOT = "00"
              GO TO MODIFY-ROUTINE.
           MOVE VDUBUF TO ACTION.

           IF ACTION = "     S" OR "S     "
              PERFORM VERIFY-REQD-FLDS
              IF IO-FG = 9
                MOVE 0 TO IO-FG
                GO TO MODIFY-ROUTINE.

           IF ACTION = "     S" OR "S     "
              PERFORM WRITE-SCRATCH
              PERFORM REMOVE-QUOTE-IF-LOAN
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.
           IF ACTION = "DELETE"
              PERFORM DELETE-SCRATCH
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.
           INSPECT ACTION REPLACING ALL " " BY "0".
           IF CHNO NOT NUMERIC
              GO TO MODIFY-ROUTINE.
           IF CHNO = 0
              GO TO MODIFY-ROUTINE.
           IF CHNO > MAX
              GO TO MODIFY-ROUTINE.
           MOVE CHNO TO ELE.
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

      * OVERRIDE CALCULATED APR RATE
           IF ELE = 26
              IF APR-OK = "N"
                 MOVE APR-SEQ TO PASSWDW-SEQ
                 MOVE "TO OVERRIDE APR RATE.......:" TO PASSWDW-PROMPT2
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
           IF ELE < 5
              MOVE
             "F1, F5-CREATE BORR, F6-SCAN BY NAME, ENTER BORROWER #:"
                                                            TO LEGEND
              PERFORM SEND-LEGEND.
           IF ELE = 5
              IF NEW-FLAG NOT = 1
                 GO TO MODIFY-ROUTINE
              ELSE
                 IF CHFG(5) = "N"
                    GO TO F-052.
           IF ELE = 6
              IF CHFG(6) = "N"
                 MOVE "S  A000060." TO VDU
                 MOVE TRANS-DATE TO PD-LOANDATE
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
           IF ELE = 10
              IF CHFG(10) = "N"
                 MOVE " " TO CHFG(10)
                 GO TO CREATE-ROUTINE.
           IF ELE = 11
              IF CHFG(11) = "N"
                 MOVE BR-DEFAULT-SF-UNITPER-CD TO VDUBUF
                 GO TO GOTO-FIELD.
           IF ELE = 13
              IF CHFG(13) = "N"
                 MOVE " " TO CHFG(13)
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
              MOVE "F1, F6-SOURCE SCAN, ENTER SOURCE CODE:"
                                                       TO LEGEND
              PERFORM SEND-LEGEND.
           IF ELE = 9
              MOVE
              "F1, F6-DEALER SCAN, ENTER DEALER NO:" TO LEGEND
              PERFORM SEND-LEGEND.
           IF ELE = 10
              MOVE
              "F1, F6-COLLATERAL SCAN, ENTER COLLATERAL CODE:"
                                                    TO LEGEND
              PERFORM SEND-LEGEND.
           IF ELE = 11
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
           IF ELE = 12
              MOVE "F1, ENTER TERM OF THE LOAN:" TO LEGEND
              PERFORM SEND-LEGEND.
           IF ELE = 13
              MOVE "F1, ENTER FIRST PAYMENT DUE DATE:" TO LEGEND
              PERFORM SEND-LEGEND.
           IF ELE = 14
              MOVE " " TO CHFG(12)
              MOVE "F1, ENTER THE SELLING PRICE:" TO LEGEND
              PERFORM SEND-LEGEND.
           IF ELE = 15
              MOVE "F1, ENTER THE DOWN PAYMENT AMOUNT:" TO LEGEND
              PERFORM SEND-LEGEND.
           IF ELE = 16
              MOVE "F1, ENTER THE PERCENT(%) DOWN:" TO LEGEND
              PERFORM SEND-LEGEND.
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
                 MOVE "F1, F3-LPO, ENTER INSURANCE CODES :" TO LEGEND
                 PERFORM SEND-LEGEND
              ELSE
                MOVE "F1, ENTER INSURANCE CODES (EX. MGG):" TO LEGEND
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

      * CALL BUYRATE WINDOW
      
           IF NEW-FLAG = 1
              IF ELE = 19
                 IF ( DL-PARVPAID-FRMLA    =  3  ) OR
                    ( DL-PARVPAID-FRMLA-IB = "3" )
                    PERFORM BUYRATE-WINDOW.

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
              IF CHFG(22) = " "
                 IF PD-DLRVPRIN(2) = 0
                         AND PD-DLRVPRIN(3) = 0
                    MOVE
                 "F1, F3-RESERVE WINDOW, ENTER PRINCIPAL RESERVE #1:"
                                                            TO LEGEND
                    PERFORM SEND-LEGEND
                 ELSE
                    PERFORM DO-RSVENT-WINDOW
                    GO TO CREATE-ROUTINE.
           IF ELE = 23
              IF CHFG(23) = " "
                 IF PD-DLRVCHGS(2) = 0
                         AND PD-DLRVCHGS(3) = 0
                    MOVE
                 "F1, F3-RESERVE WINDOW, ENTER CHARGES RESERVE #1:"
                                                            TO LEGEND
                    PERFORM SEND-LEGEND
                 ELSE
                    PERFORM DO-RSVENT-WINDOW
                    GO TO CREATE-ROUTINE.

           IF ELE = 24
              IF CHFG(24) = " "
                 IF PD-DLPARVPAID = 0
                    MOVE
                 "F1, F3-RESERVE WINDOW, ENTER PARTICIPATION HELD:"
                                                            TO LEGEND
                    PERFORM SEND-LEGEND
                 ELSE
                    PERFORM DO-RSVENT-WINDOW
                    GO TO CREATE-ROUTINE.


           IF ELE = 25
      * STOP DISCOUNT ENTRY IF NO EARNINGS FORMULA OR SERV ACCT:
              IF SP-ERNFRMLA(5) = 0 OR PD-SERVACCT = "Y"
                 GO TO CREATE-ROUTINE
              ELSE
      * STOP INPUT OF DEALER DISC IF SP-DLDISC-RVPRIN1-PCNT    NOT = 0, TFC #008
      * STOP INPUT OF DEALER DISC IF SP-DLDISC-RVPRIN1-AF-PCNT NOT = 0, TFC #058
                 IF (SP-DLDISC-RVPRIN1-PCNT NOT = 0 AND
                     SP-DLDISC-RVPRIN1-PCNT NOT = 202.02) OR
                    (SP-DLDISC-RVPRIN1-AF-PCNT NOT = 0 AND
                     SP-DLDISC-RVPRIN1-AF-PCNT NOT = 202.02)
                    GO TO CREATE-ROUTINE
                 ELSE
                    MOVE "F1, F3-RESERVE WINDOW, ENTER DEALER DISCOUNT:"
                                                     TO LEGEND
                    PERFORM SEND-LEGEND.

           IF ELE = 27
              MOVE VDUSTATUS TO HOLD-VDUSTATUS
              MOVE "F6-SCAN, ENTER INSURANCE COMPANY"
              TO LEGEND
              PERFORM SEND-LEGEND
              IF HOLD-VDUSTATUS = "1<"
                 PERFORM INSCOMP-SCAN
                 MOVE "SNNN020270." TO VDU
                 MOVE ICSCAN-CODE TO VDUNUM0
                 PERFORM SCREENIO
                 MOVE ICSCAN-CODE TO VDUNUM0
                 IF IO-FG = 9
                    GO TO ENTER-ELE.

           IF ELE = 28
              MOVE VDUSTATUS TO HOLD-VDUSTATUS
              MOVE "F6-SCAN, ENTER INSURANCE COMPANY"
              TO LEGEND
              PERFORM SEND-LEGEND
              IF HOLD-VDUSTATUS = "1<"
                 PERFORM INSCOMP-SCAN
                 MOVE "SNNN020280." TO VDU
                 MOVE ICSCAN-CODE TO VDUNUM0
                 PERFORM SCREENIO
                 MOVE ICSCAN-CODE TO VDUNUM0
                 IF IO-FG = 9
                    GO TO ENTER-ELE.

           IF ELE = 29
              MOVE VDUSTATUS TO HOLD-VDUSTATUS
              MOVE "F6-SCAN, ENTER INSURANCE COMPANY"
              TO LEGEND
              PERFORM SEND-LEGEND
              IF HOLD-VDUSTATUS = "1<"
                 PERFORM INSCOMP-SCAN
                 MOVE "SNNN020290." TO VDU
                 MOVE ICSCAN-CODE TO VDUNUM0
                 PERFORM SCREENIO
                 MOVE ICSCAN-CODE TO VDUNUM0
                 IF IO-FG = 9
                    GO TO ENTER-ELE.

           IF ELE = 30
              MOVE VDUSTATUS TO HOLD-VDUSTATUS
              MOVE "F6-SCAN, ENTER INSURANCE COMPANY"
              TO LEGEND
              PERFORM SEND-LEGEND
              IF HOLD-VDUSTATUS = "1<"
                 PERFORM INSCOMP-SCAN
                 MOVE "SNNN020300." TO VDU
                 MOVE ICSCAN-CODE TO VDUNUM0
                 PERFORM SCREENIO
                 MOVE ICSCAN-CODE TO VDUNUM0
                 IF IO-FG = 9
                    GO TO ENTER-ELE.

           IF ELE = 31
              MOVE VDUSTATUS TO HOLD-VDUSTATUS
              MOVE "F6-SCAN, ENTER INSURANCE COMPANY"
              TO LEGEND
              PERFORM SEND-LEGEND
              IF HOLD-VDUSTATUS = "1<"
                 PERFORM INSCOMP-SCAN
                 MOVE "SNNN020310." TO VDU
                 MOVE ICSCAN-CODE TO VDUNUM0
                 PERFORM SCREENIO
                 MOVE ICSCAN-CODE TO VDUNUM0
                 IF IO-FG = 9
                    GO TO ENTER-ELE.

           IF ELE = 32
              MOVE VDUSTATUS TO HOLD-VDUSTATUS
              MOVE "F6-SCAN, ENTER INSURANCE COMPANY"
              TO LEGEND
              PERFORM SEND-LEGEND
              IF HOLD-VDUSTATUS = "1<"
                 PERFORM INSCOMP-SCAN
                 MOVE "SNNN020320." TO VDU
                 MOVE ICSCAN-CODE TO VDUNUM0
                 PERFORM SCREENIO
                 MOVE ICSCAN-CODE TO VDUNUM0
                 IF IO-FG = 9
                    GO TO ENTER-ELE.

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
           MOVE "END" TO IO-TYPE.
           GO TO END-ROUTINE.
       TEST-F3.
           IF VDUSTATUS NOT = "16"
              GO TO TEST-UP.
           IF ELE = 22
              IF CHFG(22) = " "
                 PERFORM DO-RSVENT-WINDOW
                 GO TO CREATE-ROUTINE.
           IF ELE = 23
              IF CHFG(23) = " "
                 PERFORM DO-RSVENT-WINDOW
                 GO TO CREATE-ROUTINE.
           IF ELE = 24
              IF CHFG(24) = " "
                 PERFORM DO-RSVENT-WINDOW
                 GO TO CREATE-ROUTINE.
           IF ELE = 25
              IF CHFG(25) = " "
                 PERFORM DO-RSVENT-WINDOW
                 GO TO CREATE-ROUTINE.
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
           IF (ELE = 7 OR 8 OR 10)
               MOVE 0 TO CDCODE-FEE-FG
               PERFORM CODE-SCAN
               IF IO-FG = 0
                  GO TO GOTO-FIELD
               ELSE
                  GO TO ENTER-ELE.
           IF ELE = 9
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
           GO TO CREATE-ROUTINE.
       F-05.
           MOVE VDUBUF TO PD-ORGST
                          PD-OWNST.
       F-052.
           MOVE " " TO CHFG(5).
           MOVE "^-BACK, F6-SCAN, ENTER SUB CLASS:" TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "EN N020052." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE.
           IF VDUSTATUS = "1<"
              PERFORM SUBCLASS-SCAN
              IF IO-FG = 9
                 GO TO F-052.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO F-052.
           MOVE PD-ORGST TO SP-ORGST.
           MOVE PD-SPRCLASS TO SP-SPRCLASS.
           MOVE VDUNUM0 TO PD-SUBCLASS SP-SUBCLASS.
           MOVE 0 TO SP-LAWCODE.
           PERFORM START-SP1-FILE.
       F-052-NEXT-SP.
           PERFORM READ-SP1-FILE-NEXT.
           IF NOT (SP-ORGST = PD-ORGST
                     AND SP-SPRCLASS = PD-SPRCLASS
                       AND SP-SUBCLASS = PD-SUBCLASS)
              MOVE 9 TO IO-FG.
           IF IO-FG = 9
              PERFORM SP-INVALID
              GO TO F-052.
           MOVE "S  A015053." TO VDU.
           MOVE SP-DESC TO VDUBUF.
           PERFORM SCREENIO.

           GO TO CREATE-ROUTINE.
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
      *  CITIZENS TEST
                 MOVE 800 TO CIP-NO-OF-TIMES-CALLED
      *          MOVE 0 TO CIP-NO-OF-TIMES-CALLED
                 MOVE PD-LOANDATE  TO CIP-LOANDATE
                 MOVE PD-INTDATE   TO CIP-INTDATE
                 MOVE PD-1STPYDATE TO CIP-1STPYDATE
                 PERFORM CALL-LOAN-CALCULATIONS.
           IF DATE-STAT = "N"
              GO TO CREATE-ROUTINE.
       CONT1-F-06.
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
      *  CITIZENS TEST
              MOVE 800 TO CIP-NO-OF-TIMES-CALLED
      *       MOVE 0 TO CIP-NO-OF-TIMES-CALLED
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
           MOVE "SR"   TO CD-TYPE.
           MOVE VDUBUF TO PD-SRCD
                          CD-CODE.
           IF PD-SRCD = "RN"
                      OR "PB"
                       OR "SC"
                        OR "RB"
                         OR "RO"
              MOVE "INVALID FOR SALES FINANCE CONTRACTS" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.

           PERFORM SEND-CD-DESC.
           IF IO-FG = 9
              GO TO ENTER-ELE.

           IF GP-THE-ONLY-SF-SRCD NOT = " "
              IF PD-SRCD NOT = GP-THE-ONLY-SF-SRCD
                 MOVE "INVALID FOR SALES FINANCE CONTRACTS" TO MESS
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

           MOVE PD-SRCD TO CIP-SOURCE-CD.
           IF NEW-FLAG NOT = 1
              PERFORM SETUP-PROCEEDS-DOWN
              PERFORM VERIFY-PROCEEDS-DOWN
              IF IO-FG = 9
                 GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       F-09.
           MOVE VDUNUM2 TO PD-DLNO
                           DL-DLNO.
           PERFORM GET-DL1.
           MOVE "S  A150091." TO VDU.
           MOVE DL-SHORT TO VDUBUF.
           PERFORM SCREENIO.
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
           IF (LC-AFP-CLASS-N AND DL-AFP-DEALER-Y) OR
              (LC-AFP-CLASS-Y AND DL-AFP-DEALER-N)
              MOVE "AFP MISMATCH" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           IF NEW-FLAG = 1
              IF CHFG(10) = "N"
                 IF BR-SET-COLLCD-DL-MFGCD = "Y"
                        AND DL-MFGCODE NOT = " "
                    MOVE "CO"       TO CD-TYPE
                    MOVE DL-MFGCODE TO PD-COLLCD
                                       CD-CODE
                    MOVE "S  A200100." TO VDU
                    MOVE PD-COLLCD TO VDUBUF
                    PERFORM SCREENIO
                    MOVE 10 TO ELE
                    PERFORM SEND-CD-DESC
                    MOVE 9 TO ELE
                    IF IO-FG = 9
                       MOVE " " TO CHFG(10)
                    ELSE
                       NEXT SENTENCE
                 ELSE
                    MOVE " " TO CHFG(10).

      * MUST RECALCULATE PURCHASE FIELDS WHEN DEALER NO CHANGES:
           IF NEW-FLAG NOT = 1
              PERFORM SEND-PURCHASE-FIELDS.
           GO TO CREATE-ROUTINE.
       F-10.
           MOVE "CO"   TO CD-TYPE.
           MOVE VDUBUF TO PD-COLLCD
                          CD-CODE.
           PERFORM SEND-CD-DESC.
           IF IO-FG = 9
              GO TO ENTER-ELE.
           MOVE CD-CO-SECURED TO SPTEST-CD-SECURED.
           GO TO CREATE-ROUTINE.
       F-11.
           MOVE PD-UNITPER-CD TO HOLD-UNITPER-CD.
           MOVE VDUBUF TO PD-UNITPER-CD
                          UPERTST-UNITPER-CD.
           IF HOLD-UNITPER-CD NOT = PD-UNITPER-CD
                PERFORM SET-UNITPER-INFO.

      *IF YOU SET TERM TO 1 AND THEN COME BACK TO FIELD 11, UPERTST-ERRCD
      *IS NOT SPACES. WHEN YOU SET THE TERM TO 1, IT SET THE DEFAULTS
      *FOR SINGLE PAY(31 FREQ, 11.777.. PAYS PER YEAR, 31 DAYS IN PERIOD
      *THIS HAPPENS REGARDLESS OF MY CHANGE ABOVE

           PERFORM UPERTST-UNITPER-CODE.
           IF UPERTST-ERRCD NOT = " "
              MOVE " " TO CHFG(11)
              GO TO ENTER-ELE.
           IF CHFG(11) = "N"
              MOVE " " TO CHFG(11)
              GO TO CREATE-ROUTINE.
           IF BR-UNITPER-Y-D-FG NOT = "Y"
              MOVE "SNNN030111." TO VDU
              MOVE 1 TO PD-UNITPER-FREQ
                        VDUNUM0
              PERFORM SCREENIO
              PERFORM SET-1ST-PAY-DATE
              IF NEW-FLAG NOT = 1
                       OR PD-ORGTERM = 1
                 PERFORM SET-UNITPER-INFO
                 GO TO F-112
              ELSE
                 GO TO F-112.

       F-111.
           PERFORM ALARM.
           MOVE
           "F1, ^, ENTER THE FREQUENCY (EX. 2 = EVERY TWO MONTHS!):"
                                                         TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "ENNN030111." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO F-111.
           MOVE VDUNUM0 TO PD-UNITPER-FREQ
                           UPERTST-UNITPER-FREQ.
           MOVE PD-UNITPER-CD TO UPERTST-UNITPER-CD.
           PERFORM UPERTST-UNITPER-FREQUENCY.
           IF UPERTST-ERRCD NOT = " "
              GO TO F-111.

           PERFORM SET-1ST-PAY-DATE.
           IF NOT (PD-UNITPER-CD = "M"
                         AND PD-UNITPER-FREQ = 1)
              PERFORM UPERTST-SET-MESS
              MOVE UPERTST-MESS TO MESS
              PERFORM SEND-MESS.
           IF NEW-FLAG NOT = 1
                    OR PD-ORGTERM = 1
              PERFORM SET-UNITPER-INFO.

       F-112.
           PERFORM ALARM.
           IF NEW-FLAG NOT = 1
                   AND PD-ORGTERM = 1
              MOVE "F1, ^, L-LAST PAYMENT" TO LEGEND
           ELSE
              MOVE "F1, ^, B-BALLOON OR L-LAST PAYMENT??" TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "E  A010112." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              IF BR-UNITPER-Y-D-FG NOT = "Y"
                 GO TO ENTER-ELE
              ELSE
                 GO TO F-111.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO F-112.
           IF NOT (VDUBUF = "B" OR "L")
              GO TO F-112.
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
      *          GO TO F-112
      *       END-IF
      * CITIZENS TEST
              MOVE 800 TO CIP-NO-OF-TIMES-CALLED
      *       MOVE 0 TO CIP-NO-OF-TIMES-CALLED
              PERFORM CALL-LOAN-CALCULATIONS.
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
      *  CITIZENS TEST
              MOVE 800 TO CIP-NO-OF-TIMES-CALLED
      *       MOVE 0 TO CIP-NO-OF-TIMES-CALLED
              MOVE PD-1STPYDATE TO CIP-1STPYDATE
              MOVE PD-ORGTERM   TO CIP-TERM
              PERFORM CALL-LOAN-CALCULATIONS.
           GO TO CREATE-ROUTINE.
       F-13.
           IF VDUNUM0 = 0
              GO TO ENTER-ELE.

           MOVE PD-1STPYDATE      TO HOLD-DATE-WORKER.
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
              MOVE PD-ORGTERM TO CIP-TERM
              PERFORM CALL-LOAN-CALCULATIONS.
           GO TO CREATE-ROUTINE.
       F-14.
           IF VDUNUM0 = 0
              GO TO ENTER-ELE.
           MOVE VDUNUM2 TO PD-SELLPR.

      * SP-EDT-TABLE-LOOKUP:
           PERFORM SP-EDT-TABLE-LOOKUP.
           IF IO-FG = 9
              MOVE 0 TO IO-FG
              GO TO ENTER-ELE.

           IF NEW-FLAG = 1
              MOVE PD-SELLPR TO PD-PROCEEDS
              MOVE "I" TO PD-SELLPR-FG
                          PD-PROCEEDS-FG
              GO TO CREATE-ROUTINE.
       CONT-F-14.
           MOVE "^-BACK, ENTER 'C' TO CALCULATE SELL PRICE:" TO LEGEND.
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
              GO TO CONT-F-14.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO CONT-F-14.

           IF VDUBUF = PD-SELLPR-FG
                    AND SELLPR-HLDR = PD-SELLPR
                      AND PROCEEDS-FG-HLDR = PD-PROCEEDS-FG
              GO TO CREATE-ROUTINE
           ELSE
      *       PROCEEDS ARE CALCULATED:
              IF PD-PROCEEDS-FG = "C"
                 IF VDUBUF = "C"
                    PERFORM SETUP-SFCALC-BUF
                    MOVE " " TO SFCALC-SELLPR-FG
                    PERFORM SFCALC-CALC-WINDOW
                    IF SFCALC-STATUS NOT = "00"
                       GO TO CONT-F-14
                    ELSE
                       IF SFCALC-SELECT-KEY = "%"
                          MOVE "I" TO PD-DOWNPY-FG
                          MOVE "C" TO PD-PERDOWN-FG
                       ELSE
                          MOVE "C" TO PD-DOWNPY-FG
                          MOVE "I" TO PD-PERDOWN-FG
                 ELSE
                    MOVE "C" TO PD-DOWNPY-FG
                                PD-PERDOWN-FG
              ELSE
      *          PROCEEDS ARE INPUTED:
                 IF VDUBUF = "I"
                    IF PD-SELLPR-FG = "I"
                       IF PD-DOWNPY-FG = "I"
                          COMPUTE PD-PROCEEDS =
                                     PD-SELLPR - PD-DOWNPY
                       ELSE
                          COMPUTE PD-PROCEEDS =
                             PD-SELLPR
                              - (PD-SELLPR * PD-PERDOWN *.01)
                    ELSE
                       PERFORM SETUP-SFCALC-BUF
                       MOVE " " TO SFCALC-SELLPR-FG
                       PERFORM SFCALC-CALC-WINDOW
                       IF SFCALC-STATUS NOT = "00"
                          GO TO CONT-F-14
                       ELSE
                          IF SFCALC-SELECT-KEY = "%"
                             MOVE "I" TO PD-DOWNPY-FG
                             MOVE "C" TO PD-PERDOWN-FG
                             COMPUTE PD-PROCEEDS =
                                        PD-SELLPR - PD-DOWNPY
                          ELSE
                             MOVE "C" TO PD-DOWNPY-FG
                             MOVE "I" TO PD-PERDOWN-FG
                             COMPUTE PD-PROCEEDS =
                                PD-SELLPR
                                 - (PD-SELLPR * PD-PERDOWN *.01)
                 ELSE
                    IF PD-DOWNPY-FG = "C"
                       MOVE "I" TO PD-DOWNPY-FG
                    ELSE
                       MOVE "I" TO PD-PERDOWN-FG.

           MOVE VDUBUF TO PD-SELLPR-FG.

           IF PD-PROCEEDS-FG = "C"
              MOVE "N" TO CALL-LOAN-CALC-FG
           ELSE
              MOVE PD-PROCEEDS TO CIP-PROCEEDS
              MOVE "Y" TO CALL-LOAN-CALC-FG.
           PERFORM CALL-LOAN-CALCULATIONS.
           GO TO CREATE-ROUTINE.
       F-15.
           MOVE VDUNUM2 TO PD-DOWNPY.

           IF NEW-FLAG = 1
              PERFORM VERIFY-PROCEEDS-DOWN
              IF IO-FG = 9
                 MOVE 0 TO IO-FG
                 GO TO ENTER-ELE.

           IF NEW-FLAG = 1
              MOVE "I" TO PD-DOWNPY-FG
              MOVE "C" TO PD-PERDOWN-FG
              COMPUTE PD-PERDOWN ROUNDED =
                          PD-DOWNPY / PD-SELLPR * 100
              MOVE "SNNS032160." TO VDU
              MOVE PD-PERDOWN TO VDUNUM2
              PERFORM SCREENIO
              MOVE "S  A01016P." TO VDU
              MOVE "%" TO VDUBUF
              PERFORM SCREENIO
              ADD 1 TO ELE
              GO TO CREATE-ROUTINE.
       CONT-F-15.
           MOVE "^-BACK, ENTER 'C' TO CALCULATE DOWN PAYMENT:"
                                                          TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "S  A010151." TO VDU.
           MOVE "I" TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "E  A010151." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE.
           IF NOT (VDUBUF = "I" OR "C")
              PERFORM ICE-OR-A-MSG
              GO TO CONT-F-15.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO CONT-F-15.

           IF VDUBUF = PD-DOWNPY-FG
                   AND PD-DOWNPY = DOWNPY-HLDR
                      AND PROCEEDS-FG-HLDR = PD-PROCEEDS-FG
              GO TO CREATE-ROUTINE
           ELSE
      *       PROCEEDS ARE CALCULATED:
              IF PD-PROCEEDS-FG = "C"
                 IF VDUBUF = "C"
                    PERFORM SETUP-SFCALC-BUF
                    MOVE " " TO SFCALC-DOWNPY-FG
                    PERFORM SFCALC-CALC-WINDOW
                    IF SFCALC-STATUS NOT = "00"
                       GO TO CONT-F-15
                    ELSE
                       IF SFCALC-SELECT-KEY = "%"
                          MOVE "I" TO PD-SELLPR-FG
                          MOVE "C" TO PD-PERDOWN-FG
                       ELSE
                          MOVE "C" TO PD-SELLPR-FG
                          MOVE "I" TO PD-PERDOWN-FG
                 ELSE
                    MOVE "C" TO PD-PERDOWN-FG
                                PD-SELLPR-FG
              ELSE
      *          PROCEEDS ARE INPUTED:
                 IF VDUBUF = "I"
                    IF PD-DOWNPY-FG = "I"
                       IF PD-SELLPR-FG = "I"
                          COMPUTE PD-PROCEEDS =
                                     PD-SELLPR - PD-DOWNPY
                       ELSE
                          COMPUTE PD-PROCEEDS =
                             PD-DOWNPY / (PD-PERDOWN * .01)
                                                - PD-DOWNPY
                    ELSE
                       PERFORM SETUP-SFCALC-BUF
                       MOVE " " TO SFCALC-DOWNPY-FG
                       PERFORM SFCALC-CALC-WINDOW
                       IF SFCALC-STATUS NOT = "00"
                          GO TO CONT-F-15
                       ELSE
                          IF SFCALC-SELECT-KEY = "%"
                             MOVE "I" TO PD-SELLPR-FG
                             MOVE "C" TO PD-PERDOWN-FG
                             COMPUTE PD-PROCEEDS =
                                        PD-SELLPR - PD-DOWNPY
                          ELSE
                             MOVE "C" TO PD-SELLPR-FG
                             MOVE "I" TO PD-PERDOWN-FG
                             COMPUTE PD-PROCEEDS =
                                PD-DOWNPY / (PD-PERDOWN * .01)
                                                - PD-DOWNPY
                 ELSE
                    IF PD-PERDOWN-FG = "C"
                       MOVE "I" TO PD-PERDOWN-FG
                    ELSE
                       MOVE "I" TO PD-SELLPR-FG.

           MOVE VDUBUF TO PD-DOWNPY-FG.

           IF PD-PROCEEDS-FG = "C"
              MOVE "N" TO CALL-LOAN-CALC-FG
           ELSE
              MOVE PD-PROCEEDS TO CIP-PROCEEDS
              MOVE "Y" TO CALL-LOAN-CALC-FG.
           PERFORM CALL-LOAN-CALCULATIONS.
           GO TO CREATE-ROUTINE.
       F-16.
           MOVE VDUNUM2 TO PD-PERDOWN.

       CONT-F-16.
           MOVE "^-BACK, ENTER 'C' TO CALCULATE PERCENT DOWN:"
                                                          TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "S  A010161." TO VDU.
           MOVE "I" TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "E  A010161." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE.
           IF NOT (VDUBUF = "I" OR "C")
              PERFORM ICE-OR-A-MSG
              GO TO CONT-F-16.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO CONT-F-16.

           IF VDUBUF = PD-PERDOWN-FG
                   AND PD-PERDOWN = PERDOWN-HLDR
                      AND PROCEEDS-FG-HLDR = PD-PROCEEDS-FG
              GO TO CREATE-ROUTINE
           ELSE
      *       PROCEEDS ARE CALCULATED:
              IF PD-PROCEEDS-FG = "C"
                 IF VDUBUF = "C"
                    PERFORM SETUP-SFCALC-BUF
                    MOVE " " TO SFCALC-PERDOWN-FG
                    PERFORM SFCALC-CALC-WINDOW
                    IF SFCALC-STATUS NOT = "00"
                       GO TO CONT-F-16
                    ELSE
                       IF SFCALC-SELECT-KEY = "D"
                          MOVE "I" TO PD-SELLPR-FG
                          MOVE "C" TO PD-DOWNPY-FG
                       ELSE
                          MOVE "C" TO PD-SELLPR-FG
                          MOVE "I" TO PD-DOWNPY-FG
                 ELSE
                    MOVE "C" TO PD-DOWNPY-FG
                                PD-SELLPR-FG
              ELSE
      *          PROCEEDS ARE INPUTED:
                 IF VDUBUF = "I"
                    IF PD-PERDOWN-FG = "I"
                       IF PD-SELLPR-FG = "I"
                          COMPUTE PD-PROCEEDS =
                             PD-SELLPR
                              - (PD-SELLPR * PD-PERDOWN * .01)
                       ELSE
                          COMPUTE PD-PROCEEDS =
                             (PD-DOWNPY / (PD-PERDOWN * .01))
                                                         - PD-DOWNPY
                    ELSE
                       PERFORM SETUP-SFCALC-BUF
                       MOVE " " TO SFCALC-PERDOWN-FG
                       PERFORM SFCALC-CALC-WINDOW
                       IF SFCALC-STATUS NOT = "00"
                          GO TO CONT-F-16
                       ELSE
                          IF SFCALC-SELECT-KEY = "D"
                             MOVE "I" TO PD-SELLPR-FG
                             MOVE "C" TO PD-DOWNPY-FG
                             COMPUTE PD-PROCEEDS =
                                PD-SELLPR
                                 - (PD-SELLPR * PD-PERDOWN * .01)
                          ELSE
                             MOVE "C" TO PD-SELLPR-FG
                             MOVE "I" TO PD-DOWNPY-FG
                             COMPUTE PD-PROCEEDS =
                                (PD-DOWNPY / (PD-PERDOWN * .01))
                                                         - PD-DOWNPY
                 ELSE
                    IF PD-DOWNPY-FG = "C"
                       MOVE "I" TO PD-DOWNPY-FG
                    ELSE
                       MOVE "I" TO PD-SELLPR-FG.

           MOVE VDUBUF TO PD-PERDOWN-FG.

           IF PD-PROCEEDS-FG = "C"
              MOVE "N" TO CALL-LOAN-CALC-FG
           ELSE
              MOVE PD-PROCEEDS TO CIP-PROCEEDS
              MOVE "Y" TO CALL-LOAN-CALC-FG.
           PERFORM CALL-LOAN-CALCULATIONS.
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
                 PERFORM SETUP-SFINPC-BUF
                 MOVE " " TO SFINPC-BALLOON-FG
                             SFINPC-PERDOWN-FG
                 PERFORM SFINPC-CALC-WINDOW
                 IF SFINPC-STATUS NOT = "00"
                    GO TO F-171
                 ELSE
                    PERFORM AFTER-BALLO-CALC-OPT
              ELSE
                 IF VDUBUF = "C"
                    IF PD-PROCEEDS-FG = "C"
                       PERFORM SETUP-SFINPC-BUF
                       MOVE " " TO SFINPC-BALLOON-FG
                                   SFINPC-REGPYAMT-FG
                       PERFORM SFINPC-CALC-WINDOW
                       IF SFINPC-STATUS NOT = "00"
                          GO TO F-171
                       ELSE
                          MOVE "C" TO PD-BALLOON-FG
                          MOVE "I" TO PD-PROCEEDS-FG
                                      PD-SELLPR-FG
                                      PD-DOWNPY-FG
                                      PD-PERDOWN-FG
                                      PD-REGPYAMT-FG
                          IF SFINPC-CALC-SELECT-KEY = "S"
                             MOVE "C" TO PD-SELLPR-FG
                          ELSE
                             IF SFINPC-CALC-SELECT-KEY = "D"
                                MOVE "C" TO PD-DOWNPY-FG
                             ELSE
                                MOVE "C" TO PD-PERDOWN-FG
                    ELSE
                       MOVE "I" TO PD-REGPYAMT-FG
                                   PD-PROCEEDS-FG
                       MOVE "C" TO PD-BALLOON-FG
                 ELSE
                    PERFORM ICE-OR-A-MSG
                    GO TO F-171.
           IF NEW-FLAG NOT = 1
      *  CITIZENS TEST
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
              MOVE "290 291  293 294 295." TO MESS
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
           MOVE PD-DLRVPRIN(1) TO AMT-HOLDER.
           IF (SP-DLDISC-RVPRIN1-PCNT NOT = 0   ) OR
              (SP-DLDISC-RVPRIN1-AF-PCNT NOT = 0)
              IF (SP-DLDISC-RVPRIN1-PCNT NOT = 0 AND
                  SP-DLDISC-RVPRIN1-PCNT NOT = 202.02)
                 COMPUTE PD-DLDISC ROUNDED =
                      VDUNUM2 * SP-DLDISC-RVPRIN1-PCNT * 0.01
                 COMPUTE PD-DLRVPRIN(1) = VDUNUM2 - PD-DLDISC
              ELSE
                 PERFORM RSV-CALC-AMTFIN
                 IF (SP-DLDISC-RVPRIN1-AF-PCNT NOT = 0 AND
                     SP-DLDISC-RVPRIN1-AF-PCNT NOT = 202.02)
                    COMPUTE PD-DLRVPRIN(1) ROUNDED =
                         RSVENT-AMTFIN
                           * SP-DLDISC-RVPRIN1-AF-PCNT * 0.01
                    IF VDUNUM2 < PD-DLRVPRIN(1)
                       MOVE 0       TO PD-DLDISC
                       MOVE VDUNUM2 TO PD-DLRVPRIN(1)
                       MOVE
            " AMOUNT ENTERED IS LESS THAN COMPUTED RSV PRIN RE: SPR%"
                                                       TO MESS
                       PERFORM SEND-MESS
                    ELSE
                       COMPUTE PD-DLDISC = VDUNUM2 - PD-DLRVPRIN(1)
                    END-IF
                 END-IF
              END-IF
              MOVE "SNNN052220." TO VDU
              MOVE PD-DLRVPRIN(1) TO VDUNUM2
              PERFORM SCREENIO
              MOVE "SNNN052250." TO VDU
              MOVE PD-DLDISC TO VDUNUM2
              PERFORM SCREENIO
              MOVE "S  A010251." TO VDU
              MOVE "I" TO VDUBUF PD-DLDISC-FG
              PERFORM SCREENIO
           ELSE
              MOVE VDUNUM2 TO PD-DLRVPRIN(1).
           IF PD-DLRVPRIN(1) = AMT-HOLDER
              GO TO CREATE-ROUTINE.
           IF GP-DLRVPRIN-1-FG NOT = "Y"
              IF VDUNUM2 NOT = 0
                 MOVE "RESERVE PRINCIPAL 1 MUST BE 0" TO MESS
                 PERFORM SEND-MESS
                 GO TO ENTER-ELE.
       CONT-F-22.
           MOVE "S  A010221." TO VDU.
           MOVE "I" TO VDUBUF PD-DLRVPRIN-FG(1).
           PERFORM SCREENIO.

           MOVE "E  A010221." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO CONT-F-22.
           MOVE VDUBUF TO PD-DLRVPRIN-FG(1).
           IF PD-DLRVPRIN-FG(1) NOT = "I"
              IF PD-DLRVPRIN-FG(1) NOT = "C"
                 PERFORM ICE-OR-A-MSG
                 GO TO CONT-F-22.
           IF NEW-FLAG NOT = 1
              PERFORM SEND-PURCHASE-FIELDS.
           GO TO CREATE-ROUTINE.
       F-23.
           IF PD-LOANTYPE = "I"
              MOVE 0 TO PD-DLRVCHGS(1)
                        PD-DLRVCHGS(2)
                        PD-DLRVCHGS(3)
              GO TO CREATE-ROUTINE.

           MOVE PD-DLRVCHGS(1) TO AMT-HOLDER.
           IF VDUNUM2 = AMT-HOLDER
              GO TO CREATE-ROUTINE.
           MOVE VDUNUM2 TO PD-DLRVCHGS(1).
       CONT-F-23.
           MOVE "S  A010231." TO VDU.
           MOVE "I" TO VDUBUF PD-DLRVCHGS-FG(1).
           PERFORM SCREENIO.

           MOVE "E  A010231." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO CONT-F-23.
           MOVE VDUBUF TO PD-DLRVCHGS-FG(1).
           IF PD-DLRVCHGS-FG(1) NOT = "I"
              IF PD-DLRVCHGS-FG(1) NOT = "C"
                 PERFORM ICE-OR-A-MSG
                 GO TO CONT-F-23.
           IF NEW-FLAG NOT = 1
              PERFORM SEND-PURCHASE-FIELDS.
           GO TO CREATE-ROUTINE.
       F-24.
           IF PD-LOANTYPE = "I" AND VDUNUM2 NOT = 0
              MOVE DLRSV-SEQ TO PASSWDW-SEQ
              MOVE "TO ALLOW PARTIC ON IB ACCT:" TO PASSWDW-PROMPT2
              PERFORM VERIFY-PASSWORD
              IF PASSWDW-STAT NOT = "OK"
                 MOVE "PARTIC. NOT ALLOWED ON 'IB' ACCOUNTS" TO MESS
                 PERFORM SEND-MESS
                 GO TO ENTER-ELE.

           MOVE PD-DLPARVHELD TO AMT-HOLDER.
           IF VDUNUM2 = AMT-HOLDER
              GO TO CREATE-ROUTINE.
           MOVE VDUNUM2 TO PD-DLPARVHELD.
       CONT-F-24.
           MOVE "S  A010241." TO VDU.
           MOVE "I" TO VDUBUF PD-DLPARVHELD-FG.
           PERFORM SCREENIO.

           MOVE "E  A010241." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO CONT-F-24.
           MOVE VDUBUF TO PD-DLPARVHELD-FG.
           IF PD-DLPARVHELD-FG NOT = "I"
              IF PD-DLPARVHELD-FG NOT = "C"
                 PERFORM ICE-OR-A-MSG
                 GO TO CONT-F-24.
           IF NEW-FLAG NOT = 1
              PERFORM SEND-PURCHASE-FIELDS.
           GO TO CREATE-ROUTINE.
       F-25.
           MOVE PD-DLDISC TO AMT-HOLDER.
           IF VDUNUM2 = AMT-HOLDER
              GO TO CREATE-ROUTINE.
           MOVE VDUNUM2 TO PD-DLDISC.
       CONT-F-25.
           MOVE "S  A010251." TO VDU.
           MOVE "I" TO VDUBUF PD-DLDISC-FG.
           PERFORM SCREENIO.

           MOVE "E  A010251." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO CONT-F-25.
           MOVE VDUBUF TO PD-DLDISC-FG.
           IF PD-DLDISC-FG NOT = "I"
              IF PD-DLDISC-FG NOT = "C"
                 PERFORM ICE-OR-A-MSG
                 GO TO CONT-F-25.
           IF NEW-FLAG NOT = 1
              PERFORM SEND-PURCHASE-FIELDS.
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
           IF NEW-FLAG NOT = 1
              PERFORM SEND-PURCHASE-FIELDS.
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
           MOVE VDUNUM2 TO PD-1STPYAMT.
           GO TO CREATE-ROUTINE.
       F-41.
           MOVE VDUNUM2 TO PD-LASTPYAMT.
           GO TO CREATE-ROUTINE.
       F-42.
           MOVE PD-REGPYAMT TO AMT-HOLDER.
           MOVE VDUNUM2 TO PD-REGPYAMT
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
                 PERFORM SETUP-SFINPC-BUF
                 MOVE " " TO SFINPC-REGPYAMT-FG
                             SFINPC-PERDOWN-FG
                 PERFORM SFINPC-CALC-WINDOW
                 IF SFINPC-STATUS NOT = "00"
                    GO TO F-421
                 ELSE
                    PERFORM AFTER-REGPY-CALC-OPT
              ELSE
                 IF VDUBUF = "C"
                    IF PD-PROCEEDS-FG = "C"
                       PERFORM SETUP-SFINPC-BUF
                       MOVE " " TO SFINPC-BALLOON-FG
                                   SFINPC-REGPYAMT-FG
                       PERFORM SFINPC-CALC-WINDOW
                       IF SFINPC-STATUS NOT = "00"
                          GO TO F-421
                       ELSE
                          MOVE "C" TO PD-REGPYAMT-FG
                          MOVE "I" TO PD-PROCEEDS-FG
                                      PD-BALLOON-FG
                                      PD-SELLPR-FG
                                      PD-DOWNPY-FG
                                      PD-PERDOWN-FG
                          IF SFINPC-CALC-SELECT-KEY = "S"
                             MOVE "C" TO PD-SELLPR-FG
                          ELSE
                             IF SFINPC-CALC-SELECT-KEY = "D"
                                MOVE "C" TO PD-DOWNPY-FG
                             ELSE
                                MOVE "C" TO PD-PERDOWN-FG
                    ELSE
                       MOVE "I" TO PD-PROCEEDS-FG
                                   PD-BALLOON-FG
                       MOVE "C" TO PD-REGPYAMT-FG
                 ELSE
                    PERFORM ICE-OR-A-MSG
                    GO TO F-421.

      *  CITIZENS TEST
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

           MOVE PD-UNITPER-CD     TO UPER-UNITPER-CD.
           MOVE PD-UNITPER-FREQ   TO UPER-UNITPER-FREQ.
           MOVE PD-INTDATE        TO UPER-INTDATE.
           MOVE PD-ORIG-1STPYDATE TO UPER-1STPYDATE.
           MOVE PD-ORGTERM        TO UPER-TERM.
           PERFORM UPER-CALCULATION.

           MOVE UPER-UNITPER-PER-YEAR TO PD-UNITPER-PER-YEAR.
           MOVE UPER-DAYS-IN-UNITPER TO PD-DAYS-IN-UNITPER.
           IF PD-ORGTERM = 1
              MOVE UPER-SINGLE-PAY-CD TO PD-UNITPER-CD
              MOVE UPER-SINGLE-PAY-FREQ TO PD-UNITPER-FREQ.

           MOVE "S  A010110." TO VDU.
           MOVE PD-UNITPER-CD TO VDUBUF.
           PERFORM SCREENIO.
           MOVE "SN N030111." TO VDU.
           MOVE PD-UNITPER-FREQ TO VDUNUM0.
           PERFORM SCREENIO.

           IF PD-ORGTERM = 1
              MOVE "DEFAULTS FOR SINGLE PAY LOANS SET!!" TO MESS
              PERFORM SEND-MESS.

       SET-UNITPER-INFO-EXIT.
           EXIT.

      **********************************************
       SUBCLASS-SCAN SECTION.
      **********************************************
           MOVE SPACES     TO SCSCAN-SP1-KEY.
           MOVE PD-ORGST   TO SCSCAN-ORGST.
           MOVE PD-CLASS   TO SCSCAN-SPRCLASS.
           MOVE 0          TO SCSCAN-SUBCLASS
                              SCSCAN-LAWCODE.
           MOVE "Y"        TO SCSCAN-LAST-LAW.
           MOVE TRANS-DATE TO SCSCAN-TRANS-DATE.
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


      ***********************************
       BORROWER-ENTRY SECTION.
      ***********************************

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
                       CHFG(8) CHFG(10).
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

      ***********************************
       BW-TABLE-CHECK SECTION.
      ***********************************
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

      *****************************************
       FEECODE-SCAN SECTION.
      *****************************************
           MOVE "GC"                TO CDBSCN-TYPE.
           MOVE EXT-CONAME-USER-LOC TO CDBSCN-BRANCH.
           MOVE SPACES              TO CDBSCN-CODE.

           MOVE "WI/CDBSCN" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH CDBSCN-BUF EXIT-PATHNAME.
           CANCEL FORM-PROGX.
           IF ( CDBSCN-STATUS NOT = "00") OR
              ((CDBSCN-TYPE = "GC") AND (CDBSCN-CODE = SPACES ))
               MOVE 9 TO IO-FG
               GO TO EXIT-FEECODE-SCAN.

           MOVE "S  A020  0." TO VDU
           MOVE FW-ELE TO VDUROW
           MOVE CDBSCN-CODE TO VDUBUF
           PERFORM SCREENIO.

           MOVE CDBSCN-CODE TO VDUBUF.
           MOVE 0 TO IO-FG.

       EXIT-FEECODE-SCAN.
           EXIT.

      **********************************************
       CODE-SCAN SECTION.
      **********************************************

           MOVE SPACES TO CDSCAN-CODE.

           IF ELE = 7
              MOVE "PU"   TO CDSCAN-TYPE
           ELSE
           IF ELE = 8
              MOVE "SR"   TO CDSCAN-TYPE
           ELSE
           IF ELE = 10
              MOVE "CO"   TO CDSCAN-TYPE
           ELSE
              PERFORM FEECODE-SCAN
              GO TO EXIT-CODE-SCAN.

           MOVE "WI/CDSCAN" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH CDSCAN-BUF EXIT-PATHNAME.
           CANCEL FORM-PROGX.
           IF ( CDSCAN-STATUS NOT = "00") OR
              ((CDSCAN-TYPE = "PU") AND (CDSCAN-CODE = SPACES)) OR
              ((CDSCAN-TYPE = "CO") AND (CDSCAN-CODE = SPACES )) OR
              ((CDSCAN-TYPE = "SR") AND (CDSCAN-CODE = SPACES ))
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

      ***********************************
       QUOTE-SCAN SECTION.
      ***********************************
           MOVE "Q"                 TO PDSCAN-ID.
           MOVE EXT-CONAME-USER-LOC TO PDSCAN-BRANCH.
           MOVE SPACES              TO PDSCAN-VERIFY.
           MOVE "Y"                 TO PDSCAN-SALEFIN.
           MOVE SPACES              TO PDSCAN-BWNAME.
           MOVE "WI/PDSCAN" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH PDSCAN-BUF EXIT-PATHNAME.
           CANCEL FORM-PROGX.
           IF PDSCAN-STATUS NOT = "00" OR PDSCAN-BWNAME = SPACES
               MOVE 9 TO IO-FG
            ELSE
               MOVE 0 TO IO-FG.

       EXIT-QUOTE-SCAN.
           EXIT.

      *********************************************
       CLASS-SCAN SECTION.
      *********************************************
           MOVE 0      TO LCSCAN-KEY.
           MOVE SPACES TO LCSCAN-VERIFY.
           MOVE "Y"    TO LCSCAN-SALEFIN.
           MOVE "WI/LCSCAN" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH LCSCAN-BUF EXIT-PATHNAME.
           CANCEL FORM-PROGX.
           IF LCSCAN-STATUS NOT = "00" OR LCSCAN-KEY = 0
               MOVE 9 TO IO-FG
               GO TO EXIT-CLASS-SCAN.
           MOVE "SN N090LNKEY." TO VDU.
           MOVE LCSCAN-KEY TO VDUNUM0.
           PERFORM SCREENIO.
           MOVE LCSCAN-KEY TO VDUNUM0.
           MOVE 0 TO IO-FG.

       EXIT-CLASS-SCAN.
           EXIT.

      ***********************************
       DEALER-SCAN SECTION.
      ***********************************
           MOVE 0 TO DLSCAN-NUM.
           MOVE "WI/DLSCAN " TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH DLSCAN-BUF EXIT-PATHNAME.
           CANCEL FORM-PROGX.
           IF DLSCAN-STATUS NOT = "00" OR DLSCAN-NUM = " "
               MOVE 9 TO IO-FG
               GO TO EXIT-DEALER-SCAN.
           MOVE "SN N042090." TO VDU.
           MOVE DLSCAN-DLNO TO VDUNUM2.
           PERFORM SCREENIO.
           MOVE DLSCAN-DLNO TO VDUNUM2.
           MOVE 0 TO IO-FG.

       EXIT-DEALER-SCAN.
           EXIT.

      ***********************************
       INSCOMP-SCAN SECTION.
      ***********************************
           MOVE 0 TO ICSCAN-KEY.
           MOVE "WI/ICSCAN" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH ICSCAN-BUF EXIT-PATHNAME.
           CANCEL FORM-PROGX.
           IF ICSCAN-STATUS NOT = "00" OR ICSCAN-KEY = ZEROES
              MOVE 9 TO IO-FG.

       EXIT-INSCOMP-SCAN.
           EXIT.

      ****************************
       BORROWER-QUOTED SECTION.
      ****************************
       BORROWER-QUOTED-ENTER.
           MOVE "F1-CLEAR, ENTER THE QUOTE'S ID:" TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "E  A330011." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10"
              MOVE 9 TO IO-FG
              GO TO EXIT-BORROWER-QUOTED.
           IF VDUSTATUS NOT = "00" OR
                           VDUBUF = " "
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
           MOVE PD-RECORD TO WK-SAVE-RECORD.

           MOVE "Q"                 TO PD-ID.
           MOVE EXT-CONAME-USER-LOC TO PD-BRANCH.
           MOVE PD-SSNO(1)          TO PD-SOC-SEC.
           PERFORM OPEN-PD1-FILE.
           PERFORM READ-PD1-FILE.
           PERFORM CLOSE-PD1-FILE.
           MOVE WK-SAVE-RECORD TO PD-RECORD.
           IF IO-FG = 0
              MOVE "A QUOTE ALREADY EXISTS UNDER THIS ID!" TO MESS
              PERFORM SEND-MESS
              GO TO BORROWER-QUOTED-ENTER.

       BORROWER-QUOTED-CONT.
           MOVE 0 TO IO-FG.
           MOVE "M" TO PD-MAKERCD(1).
           PERFORM SEND-BW-MAKERCD.

           MOVE "N" TO CHFG(2) CHFG(3) CHFG(4) CHFG(6)
                       CHFG(8) CHFG(10).

       EXIT-BORROWER-QUOTED.
           EXIT.

      ************************************************
       DO-FEEENT-WINDOW SECTION.
      ************************************************

           MOVE SP-FEEFRMLA(1) TO FW-PROTECT-FEE1-FG.
           MOVE SP-FEEFRMLA(2) TO FW-PROTECT-FEE2-FG.

           PERFORM FEEENT-WIN.
           MOVE "SFQUOT" TO VDU-FORM-NAME.

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

      *********************************************
       DO-RSVENT-WINDOW SECTION.
      *********************************************
           MOVE 1 TO ONE.
           PERFORM RSV-CALC-AMTFIN.
           COMPUTE RSVENT-ANTICERN =
                     PD-ANTICERN(1) + PD-ANTICERN(2)
                   + PD-ANTICERN(3) + PD-ANTICERN(5).

           IF NOT (GP-INCL-OTH-IN-EARNED-R)
              ADD  PD-ANTICERN(4) TO RSVENT-ANTICERN.

           IF NOT (GP-INCL-OTH-IN-EARNED-N OR GP-INCL-OTH-IN-EARNED-R)
              ADD  PD-ANTICERN(6) TO RSVENT-ANTICERN.

           MOVE PD-PROCEEDS          TO RSVENT-PROCEEDS.
           MOVE PD-YLDRATE           TO RSVENT-YLDRATE.
           MOVE PD-DLRVPRIN(1)       TO RSVENT-DLRVPRIN(1).
           MOVE PD-DLRVPRIN(2)       TO RSVENT-DLRVPRIN(2).
           MOVE PD-DLRVPRIN(3)       TO RSVENT-DLRVPRIN(3).
           MOVE PD-DLRVCHGS(1)       TO RSVENT-DLRVCHGS(1).
           MOVE PD-DLRVCHGS(2)       TO RSVENT-DLRVCHGS(2).
           MOVE PD-DLRVCHGS(3)       TO RSVENT-DLRVCHGS(3).
           MOVE PD-DLPARVHELD        TO RSVENT-DLPARVHELD.
           MOVE PD-DLPARVPAID        TO RSVENT-DLPARVPAID.
           MOVE PD-DLDISC            TO RSVENT-DLDISC.
           MOVE PD-ANTICERN(6)       TO RSVENT-LNCOST.
           MOVE PD-INSPREM-DLADJ     TO RSVENT-INSPREM-DLADJ.
           MOVE PD-DLCOMM-PP-CMSI    TO RSVENT-DLCOMM-PP-CMSI.
           MOVE PD-DLCOMM-O3-CMSI    TO RSVENT-DLCOMM-O3-CMSI.

           MOVE PD-DLRVPRIN-FG(1)    TO RSVENT-DLRVPRIN-FG(1).
           MOVE PD-DLRVPRIN-FG(2)    TO RSVENT-DLRVPRIN-FG(2).
           MOVE PD-DLRVPRIN-FG(3)    TO RSVENT-DLRVPRIN-FG(3).
           MOVE PD-DLRVCHGS-FG(1)    TO RSVENT-DLRVCHGS-FG(1).
           MOVE PD-DLRVCHGS-FG(2)    TO RSVENT-DLRVCHGS-FG(2).
           MOVE PD-DLRVCHGS-FG(3)    TO RSVENT-DLRVCHGS-FG(3).
           MOVE PD-DLPARVHELD-FG     TO RSVENT-DLPARVHELD-FG.
           MOVE PD-DLPARVPAID-FG     TO RSVENT-DLPARVPAID-FG.
           MOVE PD-DLDISC-FG         TO RSVENT-DLDISC-FG.
           MOVE PD-DLCOMM-PP-CMSI-FG TO RSVENT-DLCOMM-PP-CMSI-FG.
           MOVE PD-DLCOMM-O3-CMSI-FG TO RSVENT-DLCOMM-O3-CMSI-FG.
           MOVE PD-LOANTYPE          TO RSVENT-LOANTYPE.

           PERFORM RSVENT-WIN.
           MOVE "SFQUOT" TO VDU-FORM-NAME.

           MOVE RSVENT-DLRVPRIN(1)    TO PD-DLRVPRIN(1).
           MOVE RSVENT-DLRVPRIN(2)    TO PD-DLRVPRIN(2).
           MOVE RSVENT-DLRVPRIN(3)    TO PD-DLRVPRIN(3).
           MOVE RSVENT-DLRVCHGS(1)    TO PD-DLRVCHGS(1).
           MOVE RSVENT-DLRVCHGS(2)    TO PD-DLRVCHGS(2).
           MOVE RSVENT-DLRVCHGS(3)    TO PD-DLRVCHGS(3).
           MOVE RSVENT-DLPARVHELD     TO PD-DLPARVHELD.
           MOVE RSVENT-DLPARVPAID     TO PD-DLPARVPAID.
           MOVE RSVENT-DLDISC         TO PD-DLDISC.
           MOVE RSVENT-LNCOST         TO PD-ANTICERN(6).
           MOVE RSVENT-INSPREM-DLADJ  TO PD-INSPREM-DLADJ.
           MOVE RSVENT-DLCOMM-PP-CMSI TO PD-DLCOMM-PP-CMSI.
           MOVE RSVENT-DLCOMM-O3-CMSI TO PD-DLCOMM-O3-CMSI.

           MOVE RSVENT-DLRVPRIN-FG(1)    TO PD-DLRVPRIN-FG(1).
           MOVE RSVENT-DLRVPRIN-FG(2)    TO PD-DLRVPRIN-FG(2).
           MOVE RSVENT-DLRVPRIN-FG(3)    TO PD-DLRVPRIN-FG(3).
           MOVE RSVENT-DLRVCHGS-FG(1)    TO PD-DLRVCHGS-FG(1).
           MOVE RSVENT-DLRVCHGS-FG(2)    TO PD-DLRVCHGS-FG(2).
           MOVE RSVENT-DLRVCHGS-FG(3)    TO PD-DLRVCHGS-FG(3).
           MOVE RSVENT-DLPARVHELD-FG     TO PD-DLPARVHELD-FG.
           MOVE RSVENT-DLPARVPAID-FG     TO PD-DLPARVPAID-FG.
           MOVE RSVENT-DLDISC-FG         TO PD-DLDISC-FG.
           MOVE RSVENT-DLCOMM-PP-CMSI-FG TO PD-DLCOMM-PP-CMSI-FG.
           MOVE RSVENT-DLCOMM-O3-CMSI-FG TO PD-DLCOMM-O3-CMSI-FG.

           PERFORM SEND-PURCHASE-FIELDS.

       DO-RSVENT-WINDOW-EXIT.
           EXIT.

      ***********************************
       INSURANCE-ENTRY SECTION.
      ***********************************
           MOVE 0 TO IO-FG.
           SUBTRACT 26 FROM ELE GIVING I-LEVEL.

      * TEST PASSWD TO CHANGE DEFAULT INSURANCE COMPANY ON CL,AH,PP,O1
           IF I-LEVEL < 5
              IF VDUNUM0 NOT = PD-INSCOMP(I-LEVEL)
                 MOVE INSCO-SEQ TO PASSWDW-SEQ
                 MOVE "TO CHANGE INS. COMPANY....:" TO PASSWDW-PROMPT2
                 PERFORM VERIFY-PASSWORD
                 IF PASSWDW-STAT NOT = "OK"
                    MOVE "SNNN020  0." TO VDU
                    MOVE ELE TO VDUROW
                    MOVE PD-INSCOMP(I-LEVEL) TO VDUNUM0
                    PERFORM SCREENIO
                    MOVE 9 TO IO-FG
                    GO TO EXIT-ENTER-INSURANCE.

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
      *       ----------------------------------------------------------
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
           MOVE "SPACE = Y" TO LEGEND.
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
           MOVE VDU-DATE-YYYYMMDD TO PD-INSEXP-DATE(I-LEVEL) NUM-DATE.
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

      * REGACC PR# 984
      * TEST PASSWD TO CHANGE CALCULATED COVERAGE ON CL,AH,O1
           IF I-LEVEL = 1 OR 2 OR 4
              IF VDUNUM2 NOT = PD-INSCOVR(I-LEVEL)
                 MOVE INSCOVR-SEQ TO PASSWDW-SEQ
                 MOVE "TO CHANGE INS. COVERAGE...:" TO PASSWDW-PROMPT2
                 PERFORM VERIFY-PASSWORD
                 IF PASSWDW-STAT NOT = "OK"
                    MOVE "SN N062  3." TO VDU
                    MOVE ELE TO VDUROW
                    MOVE PD-INSCOVR(I-LEVEL) TO VDUNUM2
                    PERFORM SCREENIO
                    MOVE 9 TO IO-FG
                    GO TO ENTER-IC-COVR.

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
      * DISBURSE OTHER3 WARRANTY PREMIUM
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

      ****************************************
       COMPUTE-INSCOMM SECTION.
      ****************************************
           IF NOT (I-LEVEL = 6 AND (BR-SFDLO3-WINDOW-FG = "Y" OR "B"))
              MOVE I-LEVEL TO ICOMM
              MOVE PD-INSCOMP(I-LEVEL) TO ICOMM-INSCOMP
              MOVE PD-INSPREM(I-LEVEL) TO ICOMM-INSPREM
              MOVE PD-INSOURS(I-LEVEL) TO ICOMM-INSOURS
              PERFORM ICOMM-CALCULATION
              MOVE ICOMM-INSCOMM TO PD-INSCOMM(I-LEVEL).

      *-IF INSURANCE PREMIUM IS ZERO, DONT COMPUTE COMMISSION AMOUNT AND
      * ZERO OUT ASSOCIATED FIELDS FOR THAT INSURANCE
       VERIFY-IC-PREM.
           IF PD-INSPREM(I-LEVEL) = ZEROS
              MOVE ZEROS TO PD-INSCOMP(I-LEVEL)
                            PD-INSEFF-DATE(I-LEVEL)
                            PD-INSEXP-DATE(I-LEVEL)
                            PD-INSCOVR(I-LEVEL)
                            PD-INSCOMM(I-LEVEL)
              MOVE SPACES TO PD-INSOURS(I-LEVEL).


      **************************************************
       SETUP-SFINPC-BUF SECTION.
      **************************************************
           MOVE SPACES             TO SFINPC-BUF.
           MOVE PD-BALLOON-LAST-FG TO SFINPC-BALLOON-LAST-FG.
           MOVE PD-REGPYAMT-FG     TO SFINPC-REGPYAMT-FG.
           MOVE PD-BALLOON-FG      TO SFINPC-BALLOON-FG.
           MOVE PD-SELLPR-FG       TO SFINPC-SELLPR-FG.
           MOVE PD-DOWNPY-FG       TO SFINPC-DOWNPY-FG.
           MOVE PD-PERDOWN-FG      TO SFINPC-PERDOWN-FG.
           MOVE PD-SELLPR          TO SFINPC-SELLPR.
           MOVE PD-DOWNPY          TO SFINPC-DOWNPY.
           MOVE PD-PERDOWN         TO SFINPC-PERDOWN.
           MOVE PD-LASTPYAMT       TO SFINPC-LASTPYAMT.
           MOVE COP-REGPYAMT       TO SFINPC-REGPYAMT.
           MOVE PD-ORGTERM         TO SFINPC-ORGTERM.

      **************************************************
       SFINPC-CALC-WINDOW SECTION.
      **************************************************
           MOVE "WI/SFINPC" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH SFINPC-BUF EXIT-PATHNAME.
           CANCEL FORM-PROGX.

      **************************************************
       SETUP-SFCALC-BUF SECTION.
      **************************************************
           MOVE SPACES        TO SFCALC-BUF.
           MOVE PD-SELLPR-FG  TO SFCALC-SELLPR-FG.
           MOVE PD-DOWNPY-FG  TO SFCALC-DOWNPY-FG.
           MOVE PD-PERDOWN-FG TO SFCALC-PERDOWN-FG.

           MOVE PD-SELLPR     TO SFCALC-SELLPR.
           MOVE PD-DOWNPY     TO SFCALC-DOWNPY.
           MOVE PD-PERDOWN    TO SFCALC-PERDOWN.

      **************************************************
       SFCALC-CALC-WINDOW SECTION.
      **************************************************
           MOVE "WI/SFCALC" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH SFCALC-BUF EXIT-PATHNAME.
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
      * DOWNPY-FG   | I OR C OR I | I OR C OR I |   | C OR I OR C |   |
      * % DOWN-FG   | C OR I OR I | C OR I OR I |   | C OR C OR I |   |
      * BALLOON-FG  | I           | C           |C/I| I           | I |
      * REGPYAMT-FG | C           | I           |I/C| I           | I |
      *****************************************************************
       AFTER-BALLO-CALC-OPT SECTION.
      *****************************************************************

      * SELLING PRICE    (CALCULATE)

           IF SFINPC-CALC-SELECT-KEY = "S"
              MOVE "C" TO PD-SELLPR-FG
                          PD-PROCEEDS-FG
                          PD-PERDOWN-FG
              MOVE "I" TO PD-BALLOON-FG
                          PD-DOWNPY-FG.

      * DOWN PAYMENT    (CALCULATE)

           IF SFINPC-CALC-SELECT-KEY = "D"
              MOVE "C" TO PD-DOWNPY-FG
                          PD-PROCEEDS-FG
                          PD-PERDOWN-FG
              MOVE "I" TO PD-BALLOON-FG
                          PD-SELLPR-FG.

      * REGULAR PAYMENT   (CALCULATE)

           IF SFINPC-CALC-SELECT-KEY = "R"
              MOVE "C" TO PD-REGPYAMT-FG
              MOVE "I" TO PD-BALLOON-FG.

       AFTER-BALLO-CALC-OPT-EXIT.
           EXIT.

      **************************************
       AFTER-REGPY-CALC-OPT SECTION.
      **************************************

      * SELLING PRICE    (CALCULATE)

           IF SFINPC-CALC-SELECT-KEY = "S"
              MOVE "C" TO PD-SELLPR-FG
                          PD-PROCEEDS-FG
                          PD-PERDOWN-FG
              MOVE "I" TO PD-REGPYAMT-FG
                          PD-DOWNPY-FG.

      * DOWN PAYMENT    (CALCULATE)

           IF SFINPC-CALC-SELECT-KEY = "D"
              MOVE "C" TO PD-DOWNPY-FG
                          PD-PROCEEDS-FG
                          PD-PERDOWN-FG
              MOVE "I" TO PD-REGPYAMT-FG
                          PD-SELLPR-FG.

      * BALLOON OR LAST PAYMENT   (CALCULATE)

           IF SFINPC-CALC-SELECT-KEY = "B" OR "L"
              MOVE "C" TO PD-BALLOON-FG
              MOVE "I" TO PD-REGPYAMT-FG.

       AFTER-REGPY-CALC-OPT-EXIT.
           EXIT.

      ********************************************
       CHECK-SP-EFFECTIVE SECTION.
      ********************************************

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

      *********************************
       MEMO-ENTRY SECTION.
      *********************************
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

      *********************************
       NEW-RECORD-SETUP SECTION.
      *********************************
           MOVE 1 TO NEW-FLAG.
           PERFORM CLEAR-PD-FILE.
           MOVE " " TO DISPLAY-BUF1
                        SCREEN-LINES
                         PD2-KEY
                          SAVE-PD2-KEY
                           PRIOR-LOAN-STAT
                            HOLD-BWNAME
                             PASS-STAT-FG.

           MOVE TRANS-DATE TO PD-LOANDATE DATE-YYYYMMDD.
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
              MOVE "M" TO BR-DEFAULT-SF-UNITPER-CD
              MOVE BR-DEFAULT-SF-UNITPER-CD TO DEF-UNITPER-CD
                                               PD-UNITPER-CD
           ELSE
              IF BR-DEFAULT-SF-UNITPER-CD = " "
                 MOVE "_" TO DEF-UNITPER-CD
              ELSE
                 IF NOT (BR-DEFAULT-SF-UNITPER-CD = "M"
                                                  OR "W"
                                                   OR "B"
                                                    OR "S")
                    MOVE "M" TO BR-DEFAULT-SF-UNITPER-CD
                    MOVE BR-DEFAULT-SF-UNITPER-CD TO DEF-UNITPER-CD
                                                     PD-UNITPER-CD
                 ELSE
                    MOVE BR-DEFAULT-SF-UNITPER-CD TO DEF-UNITPER-CD
                                                     PD-UNITPER-CD.
           MOVE 1 TO DEF-UNITPER-FREQ
                     PD-UNITPER-FREQ.
           MOVE "B" TO DEF-BALLBL
                       PD-BALLOON-LAST-FG.

           IF BR-DEFAULT-SF-PURCD = " "
              MOVE "___" TO DEF-PURCD
              MOVE " " TO CHFG(7)
           ELSE
              MOVE "N" TO CHFG(7)
              MOVE "PU" TO CD-TYPE
              MOVE BR-DEFAULT-SF-PURCD TO DEF-PURCD
                                          PD-PURCD
                                          CD-CODE
              MOVE 7 TO ELE
              PERFORM SEND-CD-DESC
              IF IO-FG = 9
                 MOVE " " TO  CHFG(7).


           MOVE "SR" TO CD-TYPE.
           IF GP-THE-ONLY-SF-SRCD NOT = " "
              MOVE GP-THE-ONLY-SF-SRCD TO DEF-SRCD
                                          PD-SRCD
                                          CD-CODE
              MOVE "N" TO  CHFG(7)
           ELSE
              MOVE "NB" TO DEF-SRCD
                           PD-SRCD
                           CD-CODE
              MOVE " " TO  CHFG(8).

              MOVE 8 TO ELE.
              PERFORM SEND-CD-DESC.

           MOVE DEF-BUF TO WR-BUF.
           MOVE DEF-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

           MOVE 0 TO PD-LASTPYAMT.
           MOVE "I" TO PD-BALLOON-FG.
           MOVE "N" TO CHFG(11).

           MOVE 999.99 TO PD-SERVPER.
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
           IF GP-DLRVPRIN-1-FG NOT = "Y"
              MOVE "I" TO PD-DLRVPRIN-FG(1).
           IF GP-DLRVPRIN-2-FG NOT = "Y"
              MOVE "I" TO PD-DLRVPRIN-FG(2).
           IF GP-DLRVPRIN-3-FG NOT = "Y"
              MOVE "I" TO PD-DLRVPRIN-FG(3).
           IF GP-DLRVCHGS-3-FG NOT = "Y"
              MOVE "I" TO PD-DLRVCHGS-FG(3).
           MOVE "N" TO PD-DLRECOURSE.
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

           MOVE "N" TO STAT.
           PERFORM PROTECT-FIELDS.

           MOVE 0 TO ELE.
           PERFORM CLEAR-CALC-AREA-Z3.

      *****************************
       DISPLAY-FIELDS SECTION.
      *****************************
           MOVE " " TO DISPLAY-BUF1.
           PERFORM GET-SP.

           MOVE PD-LNAMT TO D-LOANAMT.
           MOVE PD2-BWNAME TO SAVE-BW-NAME.
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

           MOVE PD-PROCEEDS TO D-DLCK.
           COMPUTE ANTIC = PD-ANTICERN(1) + PD-ANTICERN(2)
                         + PD-ANTICERN(3)
                         + PD-ANTICERN(5).
           IF NOT (GP-INCL-OTH-IN-EARNED-N OR
                                  GP-INCL-OTH-IN-EARNED-R)
              ADD PD-ANTICERN(6) TO ANTIC.

           IF NOT (GP-INCL-OTH-IN-EARNED-R)
              ADD PD-ANTICERN(4) TO ANTIC.

           MOVE ANTIC TO D-ANTIC.

           MOVE PD-ORGST TO D-050
           MOVE PD-ORGBR TO D-051.
           MOVE PD-SUBCLASS TO D-052.
           MOVE SP-DESC TO D-053.


           PERFORM MAKE-DATES-CURRENT.

           MOVE "PU" TO CD-TYPE.
           MOVE PD-PURCD TO D-070 CD-CODE.
           PERFORM GET-CD.
           MOVE CD-DESC TO D-071.

           MOVE "SR" TO CD-TYPE.
           MOVE PD-SRCD TO D-080 CD-CODE.
           PERFORM GET-CD.
           MOVE CD-DESC TO D-081.

           MOVE PD-DLNO TO D-090
                           DL-DLNO.
           PERFORM GET-DL1.
           MOVE DL-SHORT TO D-091.

           MOVE "CO"      TO CD-TYPE.
           MOVE PD-COLLCD TO CD-CODE  D-100.
           PERFORM GET-CD.
           MOVE CD-DESC   TO D-101.
           MOVE CD-CO-SECURED TO SPTEST-CD-SECURED.

           MOVE PD-UNITPER-CD TO D-110.
           MOVE PD-UNITPER-FREQ TO D-111.
           MOVE PD-BALLOON-LAST-FG TO D-112.
           PERFORM DISPLAY-BALLOON-LAST.

           MOVE PD-ORGTERM TO D-120.

           MOVE PD-ORIG-1STPYDATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO D-130.

           MOVE PD-SELLPR TO D-140.
           MOVE PD-SELLPR-FG TO D-141.

           MOVE PD-DOWNPY TO D-150.
           MOVE PD-DOWNPY-FG TO D-151.

           MOVE PD-PERDOWN TO D-160.
           MOVE "%" TO D-16P.
           MOVE PD-PERDOWN-FG TO D-161.

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
           PERFORM CALC-YIELD.
           MOVE WS-YIELD TO D-264.

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
           ADD PD-ANTICERN(1) PD-ANTICERN(2)
               PD-ANTICERN(3) PD-ANTICERN(4)
               PD-ANTICERN(5) GIVING CIP-ANTIC-EARNS.
      *JTG   + PD-ANTICERN(5) + PD-ANTICERN(6). PERSP-LNCOST-FRMLA 'D'

      * SETUP FIRST PAYMENT AMOUNT:
           MOVE PD-1STPYAMT TO D-400.
           IF PD-1STPYAMT = PD-REGPYAMT
              MOVE 0 TO CIP-ODD-1ST-PMT.

      * SETUP LAST PAYMENT AMOUNT:
           MOVE PD-LASTPYAMT TO D-410.
           IF PD-BALLOON-LAST-FG = "L"
              MOVE PD-LASTPYAMT TO CIP-BALLOON
           ELSE
      *       DEFENSE IF,
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
            PERFORM ADJUST-ANTICERN.
            MOVE WK-ANTICERN(1) TO PD-ANTICERN(1).
            MOVE WK-ANTICERN(2) TO PD-ANTICERN(2).
            MOVE WK-ANTICERN(3) TO PD-ANTICERN(3).
            MOVE WK-ANTICERN(4) TO PD-ANTICERN(4).
            MOVE WK-ANTICERN(5) TO PD-ANTICERN(5).

           IF PD-DLRVPRIN(2) = 0
                   AND PD-DLRVPRIN(3) = 0
              MOVE PD-DLRVPRIN(1) TO D-DLRVPRIN
           ELSE
              ADD PD-DLRVPRIN(1) PD-DLRVPRIN(2) PD-DLRVPRIN(3)
                                         GIVING D-DLRVPRIN
              INSPECT D-DLRVPRIN REPLACING LEADING " " BY "*".
           IF PD-DLRVPRIN-FG(1) = "C"
                   AND PD-DLRVPRIN-FG(2) = "C"
                    AND PD-DLRVPRIN-FG(3) = "C"
              MOVE "C" TO D-DLRVPRIN-FG
           ELSE
              MOVE "I" TO D-DLRVPRIN-FG.

           IF PD-DLRVCHGS(2) = 0
                   AND PD-DLRVCHGS(3) = 0
              MOVE PD-DLRVCHGS(1) TO D-DLRVCHGS
           ELSE
              ADD PD-DLRVCHGS(1) PD-DLRVCHGS(2) PD-DLRVCHGS(3)
                                         GIVING D-DLRVCHGS
              INSPECT D-DLRVCHGS REPLACING LEADING " " BY "*".
           IF PD-DLRVCHGS-FG(1) = "C"
                   AND PD-DLRVCHGS-FG(2) = "C"
                    AND PD-DLRVCHGS-FG(3) = "C"
              MOVE "C" TO D-DLRVCHGS-FG
           ELSE
              MOVE "I" TO D-DLRVCHGS-FG.

           IF PD-DLPARVPAID = 0
              MOVE PD-DLPARVHELD TO D-DLPARV
           ELSE
              ADD PD-DLPARVHELD PD-DLPARVPAID GIVING D-DLPARV
              INSPECT D-DLPARV REPLACING LEADING " " BY "*".
           IF PD-DLPARVHELD-FG = "C"
                   AND PD-DLPARVPAID-FG = "C"
              MOVE "C" TO D-DLPARV-FG
           ELSE
              MOVE "I" TO D-DLPARV-FG.

           MOVE PD-DLDISC TO D-DLDISC.
           MOVE PD-DLDISC-FG TO D-DLDISC-FG.

           MOVE DISPLAY-BUF2 TO WR-BUF.
           MOVE DISPLAY-LIST2 TO WR-LIST.
           PERFORM CALL-WRFORM.

           PERFORM VALID-INS-TYPES-ELIG.
      *    IF IO-FG = 9
      *       GO TO XXXXXXXXXXXX.

           IF PD-MESSAGE NOT = " "
              PERFORM SEND-MEMO
              PERFORM ALARM.

      ************************************
       SETUP-FEES-FOR-DISPLAY SECTION.
      ************************************
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


      ***********************************
       WRITE-SCRATCH SECTION.
      ***********************************
           MOVE "LOAN TEMP SAVE IN PROGRESS.........." TO LEGEND.
           PERFORM SEND-LEGEND.

           PERFORM OPEN-PD1-FILE.
           MOVE PD-RECORD TO WK-SAVE-RECORD.
           MOVE PD-SSNO(1) TO SAVE-SSNO.
           PERFORM SET-SCRATCH-KEY.
           IF QUOTE-ONLY
              MOVE "Q" TO PD-ID
              MOVE PD2-KEY TO SAVE-PD2-KEY.
           PERFORM READ-PD1-FILE.

           MOVE WK-SAVE-RECORD TO PD-RECORD.
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

      ***********************************
       DELETE-SCRATCH SECTION.
      ***********************************
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

      ***********************************
       DEFAULT-INT-PMT-DATES SECTION.
      ***********************************
           PERFORM CHECK-SP-EFFECTIVE.
           IF IO-FG = 9
              PERFORM SP-INVALID
              GO TO EXIT-DEFAULT-INT-PMT-DATES.

           MOVE SP-LAWCODE TO PD-LAWCODE.
           IF PD-INTDATE = 0
              MOVE "S  A000061." TO VDU
              MOVE PD-LOANDATE TO DATE-YYYYMMDD PD-INTDATE
              PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
              MOVE DATE-MMDDYY TO VDU-DATE-EDIT
              PERFORM SCREENIO.
           IF PD-1STPYDATE NOT = 0
              MOVE 0 TO AMT-HOLDER
              GO TO EXIT-DEFAULT-INT-PMT-DATES.

           MOVE 1 TO AMT-HOLDER.
           PERFORM SET-1ST-PAY-DATE.

       EXIT-DEFAULT-INT-PMT-DATES.
           EXIT.

      *************************
       VALIDATION SECTION.
      *************************
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

      *****************************
       SP-EDT-TABLE-LOOKUP SECTION.
      *****************************
           SET ETBL TO 1.
           MOVE 0 TO COUNTER.
           IF ELE = 18
              MOVE 999999.99 TO POINTS-LOW
              GO TO POINTS-VALIDATION.

           IF ELE = 12 OR 13 OR 14 OR (ELE > 15 AND ELE < 21)
              MOVE 999 TO TERM-LOW
              MOVE 999999.99 TO PROCEEDS-LOW
              MOVE 0 TO TERM-HIGH PROCEEDS-HIGH
              COMPUTE AMT-HOLDER ROUNDED = PRIOR-LNAMT
                       + PD-SELLPR - PD-DOWNPY + TOTAL-GL-HLDR
              GO TO PROCEEDS-VALIDATION.

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
           IF ((PD-ORGTERM < TERM-LOW)
                           AND (TERM-LOW NOT = 999))
                 OR ((PD-ORGTERM > TERM-HIGH)
                           AND (TERM-HIGH NOT = 0))
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
           IF ( (PD-POINTS < POINTS-LOW    ) AND
                (POINTS-LOW NOT = 999999.99)
              ) OR
              ( (PD-POINTS > POINTS-HIGH   ) AND
                (POINTS-HIGH NOT = 0       )
              )
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

      *--->IF NOT (ELE = 12 AND NEW-FLAG = 1)
      *       IF SP-EDT-TYPE(ETBL,COUNTER) = "A"
      *          IF SP-EDT-HL(ETBL,COUNTER) = "L"
      *             IF SP-EDT-AMTYPE(ETBL,COUNTER) = "P"
      *                IF SP-EDT-AMT(ETBL,COUNTER) < PROCEEDS-LOW
      *------------------>MOVE SP-EDT-AMT(ETBL,COUNTER) TO PROCEEDS-LOW.
           IF NOT (ELE = 14 AND NEW-FLAG = 1)
              IF SP-EDT-TYPE(ETBL,COUNTER) = "A"
                 IF SP-EDT-HL(ETBL,COUNTER) = "H"
      *------------>IF SP-EDT-AMTYPE(ETBL,COUNTER) = "P"
                       IF SP-EDT-AMT(ETBL,COUNTER) > PROCEEDS-HIGH
                          MOVE SP-EDT-AMT(ETBL,COUNTER) TO PROCEEDS-HIGH.

           IF COUNTER NOT > SP-EDT-NOEDITS(ETBL)
              GO TO PROCEEDS-VALIDATION.

       PROCEEDS-TABLE2.
           IF SP-EDT-NOEDITS(ETBL) NOT < 6 AND ETBL NOT = 2
              MOVE 0 TO COUNTER
              SET ETBL TO 2
              GO TO PROCEEDS-VALIDATION.

      *--->IF ((AMT-HOLDER < PROCEEDS-LOW) AND
      *------>(PROCEEDS-LOW NOT = 999999.99)) OR
           IF ((AMT-HOLDER > PROCEEDS-HIGH) AND
              (PROCEEDS-HIGH NOT = 0))
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
              STRING SP-FEEID(2) ":" DELIMITED BY SIZE
                          INTO PROMPT-211.

           IF GP-DLR-DISP-TYPE = "C"
              MOVE "DLCOMM :" TO PROMPT-251
           ELSE
              MOVE "DLDISC :" TO PROMPT-251.



           MOVE PROMPT-BUF TO WR-BUF.
           MOVE PROMPT-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       DISPLAY-PROMPTS-EXIT.
           EXIT.


      *****************************
       GET-PURCHASE-FIELDS SECTION.
      *****************************

           PERFORM COMPUTE-PRINCIPLE.
           PERFORM COMPUTE-ANTICTERM.
           PERFORM COMPUTE-PURBAL.
           PERFORM SETUP-PURCHASE-FIELDS.
           PERFORM COMPUTE-ANTICIPATED.
           PERFORM COMPUTE-DEALER-CHECK.
           PERFORM CALC-YIELD.

       EXIT-GET-PURCHASE-FIELDS.
           EXIT.

      *********************************
       CALC-YIELD SECTION.
      *********************************
           PERFORM COMPUTE-YIELD.
           MOVE APR-APR TO WS-YIELD.

      ******************************************
       CALL-LOAN-CALCULATIONS SECTION.
      ******************************************
           MOVE PD-UNITPER-CD      TO CIP-UNITPER-CD.
           MOVE PD-UNITPER-FREQ    TO CIP-UNITPER-FREQ.
           MOVE PD-BALLOON-LAST-FG TO CIP-BALLOON-LAST-FG.
           PERFORM MATURITY-DATE-CALCULATION.
           MOVE NDTE-DATE          TO CIP-MATURITY-DATE.
           MOVE PD-SELLPR-FG       TO CIP-SELLPR-FG.
           MOVE PD-DOWNPY-FG       TO CIP-DOWNPY-FG.
           MOVE PD-PERDOWN-FG      TO CIP-PERDOWN-FG.
           MOVE PD-SELLPR          TO CIP-SELLPR.
           MOVE PD-DOWNPY          TO CIP-DOWNPY.
           MOVE PD-PERDOWN         TO CIP-PERDOWN.
           MOVE TOTAL-GL-HLDR      TO CIP-GL-AMOUNT-TOTAL.
           MOVE TOTAL-GL-FINCHG-HLDR TO CIP-GL-AMOUNT-TOTAL-FINCHG.

           MOVE "CALCULATING - PLEASE WAIT" TO LEGEND.
           PERFORM SEND-LEGEND.

      * SETUP VERIFY-REJECT-TERMS RE: SP-INSREJECT-FG = "C":
           PERFORM ESTABLISH-REJECT-TERMS.

           MOVE " " TO CIP-CAN-LOAN-NO.

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
      *       MOVE "CALL_TESTZ3" TO GETENV-BUF
      *       PERFORM GETENV-CALL
      *       IF STAT = "00"
      *          MOVE "LP/TESTZ3"  TO FORM-NAM
      *       ELSE
      *-------------------------------------------------------------

           MOVE "LP/CALCZ3"  TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH
                                CIP-CALCULATION-INPUT-AREA
                                CEP-CALCULATION-EDIT-AREA
                                COP-CALCULATION-OUTPUT-AREA
                                SP-REC
                                BR-REC STATE CITY-TAX-CD COUNTY
           CANCEL FORM-PROGX.
           MOVE "Y" TO CALL-LOAN-CALC-FG.

      *    MOVE COP-PRLOAN-RESTRICTED TO PRLOAN-RESTRICTED.

           MOVE COP-PROCEEDS TO PD-PROCEEDS
                                PROCEEDS-HLDR.

           MOVE COP-SELLPR-FG  TO PD-SELLPR-FG.
           MOVE COP-DOWNPY-FG  TO PD-DOWNPY-FG.
           MOVE COP-PERDOWN-FG TO PD-PERDOWN-FG.
           MOVE COP-SELLPR     TO PD-SELLPR.
           MOVE COP-DOWNPY     TO PD-DOWNPY.
           MOVE COP-PERDOWN    TO PD-PERDOWN.

           MOVE COP-PROCEEDS-SW TO PD-PROCEEDS-FG
                                   PROCEEDS-FG-HLDR
                                   DF2-PROCEEDS-FG.

           MOVE PD-SELLPR TO DF2-SELLPR
                             SELLPR-HLDR.
           MOVE PD-SELLPR-FG TO DF2-SELLPR-FG.

           MOVE PD-DOWNPY TO DF2-DOWNPY
                             DOWNPY-HLDR.
           MOVE PD-DOWNPY-FG TO DF2-DOWNPY-FG.

           MOVE PD-PERDOWN TO DF2-PERDOWN
                              PERDOWN-HLDR.
           MOVE PD-PERDOWN-FG TO DF2-PERDOWN-FG.
           IF PD-PERDOWN NOT = 0
              MOVE "%" TO DF2-PERDOWNP
           ELSE
              MOVE " " TO DF2-PERDOWNP.

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
            PERFORM ADJUST-ANTICERN.
            MOVE WK-ANTICERN(1) TO PD-ANTICERN(1).
            MOVE WK-ANTICERN(2) TO PD-ANTICERN(2).
            MOVE WK-ANTICERN(3) TO PD-ANTICERN(3).
            MOVE WK-ANTICERN(4) TO PD-ANTICERN(4).
            MOVE WK-ANTICERN(5) TO PD-ANTICERN(5).

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
      *       DEFENSE IF,
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
           PERFORM GET-PURCHASE-FIELDS.
           MOVE PD-PROCEEDS TO DF2-DLCK.
           COMPUTE DF2-ANTIC = PD-ANTICERN(1) + PD-ANTICERN(2)
                             + PD-ANTICERN(3) + PD-ANTICERN(4)
                             + PD-ANTICERN(5).
      *JTG   + PD-ANTICERN(5) + PD-ANTICERN(6). PERSP-LNCOST-FRMLA 'D'
           IF PD-DLRVPRIN(2) = 0
                   AND PD-DLRVPRIN(3) = 0
              MOVE PD-DLRVPRIN(1) TO DF2-DLRVPRIN
           ELSE
              ADD PD-DLRVPRIN(1) PD-DLRVPRIN(2) PD-DLRVPRIN(3)
                                         GIVING DF2-DLRVPRIN
              INSPECT DF2-DLRVPRIN REPLACING LEADING " " BY "*".
           IF PD-DLRVPRIN-FG(1) = "C"
                   AND PD-DLRVPRIN-FG(2) = "C"
                    AND PD-DLRVPRIN-FG(3) = "C"
              MOVE "C" TO DF2-DLRVPRIN-FG
           ELSE
              MOVE "I" TO DF2-DLRVPRIN-FG.

           IF PD-DLRVCHGS(2) = 0
                   AND PD-DLRVCHGS(3) = 0
              MOVE PD-DLRVCHGS(1) TO DF2-DLRVCHGS
           ELSE
              ADD PD-DLRVCHGS(1) PD-DLRVCHGS(2) PD-DLRVCHGS(3)
                                         GIVING DF2-DLRVCHGS
              INSPECT DF2-DLRVCHGS REPLACING LEADING " " BY "*".
           IF PD-DLRVCHGS-FG(1) = "C"
                   AND PD-DLRVCHGS-FG(2) = "C"
                    AND PD-DLRVCHGS-FG(3) = "C"
              MOVE "C" TO DF2-DLRVCHGS-FG
           ELSE
              MOVE "I" TO DF2-DLRVCHGS-FG.

           IF PD-DLPARVPAID = 0
              MOVE PD-DLPARVHELD TO DF2-DLPARV
           ELSE
              ADD PD-DLPARVHELD PD-DLPARVPAID GIVING DF2-DLPARV
              INSPECT DF2-DLPARV REPLACING LEADING " " BY "*".
           IF PD-DLPARVHELD-FG = "C"
                   AND PD-DLPARVPAID-FG = "C"
              MOVE "C" TO DF2-DLPARV-FG
           ELSE
              MOVE "I" TO DF2-DLPARV-FG.

           MOVE PD-DLDISC TO DF2-DLDISC.
           MOVE PD-DLDISC-FG TO DF2-DLDISC-FG.

           PERFORM COMPUTE-INSCOMM
                VARYING I-LEVEL FROM 1 BY 1
                    UNTIL I-LEVEL > 8.
           PERFORM CALC-YIELD.
           MOVE WS-YIELD TO DF2-YIELD
                         PD-YLDRATE.

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

      ******************************************
       SETUP-PURCHASE-FIELDS SECTION.
      ******************************************

      * SETUP PURFLDW FIELDS AND CALL COMPUTE-PURCHASE-FIELDS.

      * SET RESERVE FIELDS INTO PURFLD FIELDS:
           MOVE PD-DLRVPRIN(1) TO PURFLD-DLRVPRIN(1).
           MOVE PD-DLRVPRIN(2) TO PURFLD-DLRVPRIN(2).
           MOVE PD-DLRVPRIN(3) TO PURFLD-DLRVPRIN(3).
           MOVE PD-DLRVCHGS(1) TO PURFLD-DLRVCHGS(1).
           MOVE PD-DLRVCHGS(2) TO PURFLD-DLRVCHGS(2).
           MOVE PD-DLRVCHGS(3) TO PURFLD-DLRVCHGS(3).
           MOVE PD-DLPARVHELD  TO PURFLD-DLPARVHELD.
           MOVE PD-DLPARVPAID  TO PURFLD-DLPARVPAID.
           MOVE PD-DLDISC      TO PURFLD-DLDISC.

      * SET RESERVE FLAG FIELDS INTO PURFLD FIELDS:
           MOVE PD-DLRVPRIN-FG(1) TO PURFLD-DLRVPRIN-FG(1).
           MOVE PD-DLRVPRIN-FG(2) TO PURFLD-DLRVPRIN-FG(2).
           MOVE PD-DLRVPRIN-FG(3) TO PURFLD-DLRVPRIN-FG(3).
           MOVE PD-DLRVCHGS-FG(1) TO PURFLD-DLRVCHGS-FG(1).
           MOVE PD-DLRVCHGS-FG(2) TO PURFLD-DLRVCHGS-FG(2).
           MOVE PD-DLRVCHGS-FG(3) TO PURFLD-DLRVCHGS-FG(3).
           MOVE PD-DLPARVHELD-FG  TO PURFLD-DLPARVHELD-FG.
           MOVE PD-DLPARVPAID-FG  TO PURFLD-DLPARVPAID-FG.
           MOVE PD-DLDISC-FG      TO PURFLD-DLDISC-FG.

           PERFORM COMPUTE-PURCHASE-FIELDS.
           IF PURFLD-STAT = "DL"
              MOVE "DEALER RECORD NOT FOUND" TO MESS
              PERFORM SEND-MESS
              GO TO IO-ERROR.

      * SET NEW VALUES FOR PD RESERVE FIELDS:
           MOVE PURFLD-DLRVPRIN(1) TO PD-DLRVPRIN(1).
           MOVE PURFLD-DLRVPRIN(2) TO PD-DLRVPRIN(2).
           MOVE PURFLD-DLRVPRIN(3) TO PD-DLRVPRIN(3).
           MOVE PURFLD-DLRVCHGS(1) TO PD-DLRVCHGS(1).
           MOVE PURFLD-DLRVCHGS(2) TO PD-DLRVCHGS(2).
           MOVE PURFLD-DLRVCHGS(3) TO PD-DLRVCHGS(3).
           MOVE PURFLD-DLPARVHELD  TO PD-DLPARVHELD.
           MOVE PURFLD-DLPARVPAID  TO PD-DLPARVPAID.
           MOVE PURFLD-DLDISC      TO PD-DLDISC.

      *************************************************
       MAKE-DATES-CURRENT SECTION.
      *************************************************
      * SET NEW INTEREST DATE:
           MOVE PD-LOANDATE TO NUM-DATE.
           MOVE PD-INTDATE  TO SYS-DATE.
           PERFORM TIM.
           MOVE ELAPSED-DAYS        TO NDTE-HOLD.
           MOVE TRANS-DATE          TO NDTE-DATE.
           PERFORM INCREMENT-DAYS.
           MOVE NDTE-DATE TO PD-INTDATE DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO D-061.

      * SET NEW LOAN DATE:
           MOVE TRANS-DATE TO PD-LOANDATE DATE-YYYYMMDD.
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

      ******************************************
       CALCULATE-APR SECTION.
      ******************************************

      * CALCULATE APR DUMMY APR CALCULATED IN CALCZ3.C

      ******************************************************************
       VERIFY-PASSWORD SECTION.
      ******************************************************************
           MOVE " "                          TO PASSWDW-STAT.
           MOVE "SFQUOT"                     TO PASSWDW-PGM.
           MOVE "PLEASE ENTER THE PASSWORD," TO PASSWDW-PROMPT1.
           MOVE "CL/PASSWD"                  TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH PASSWDW-BUF.
           CANCEL FORM-PROGX.

      ***********************************************************************
      *    BUYRATE WINDOW
      *          FOR DL-PARVPAID-FRMLA    =  3  AND
      *              DL-PARVPAID-FRMLA-IB = "3"
      *                                                       WISCONSIN #2049
      ***********************************************************************
       BUYRATE-WINDOW SECTION.
           MOVE PURFLD-BUYRATE    TO BUYRTE-BUYRATE.

           MOVE "WI/BUYRTE"  TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH
                                BUYRTE-BUF.
           CANCEL FORM-PROGX.
           MOVE BUYRTE-BUYRATE    TO PURFLD-BUYRATE.

       BUYRATE-WINDOW-EXIT.
           EXIT.

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
       COPY "LIBLP/CALCPV.CPY".
       COPY "LIBLP/LPAPRZ.CPY".

       COPY "LIBLP/YIELDPD.CPY".
       COPY "LIBLP/OURSPD.CPY".
       COPY "LIBLP/PURFLDPD.CPY".
       COPY "LIBLP/DLCKPD.CPY".
       COPY "LIBWI/RSVENT.CPY".
       COPY "LIBWI/RSVENTPD.CPY".
       COPY "LIBWI/FEEENT.CPY".
       COPY "LIBWI/FEEENTPD.CPY".
       COPY "LIBLP/LPICOMM.CPY".
       COPY "LIBLP/LPZ3CL.CPY".

      **************************************************************************
       FORM-RESET SECTION.
      **************************************************************************
           MOVE EXT-CONAME-SYSDATE TO SFQUOT-SYSDATE.
           DISPLAY SPACES UPON SFQUOT-WINDOW-HANDLE
                          LINE 1 COL 1 ERASE SCREEN.
           DISPLAY SFQUOT-SCREEN UPON SFQUOT-WINDOW-HANDLE.
           MOVE SFQUOT-FORM-FIELDS TO WR-BUF.
           MOVE SFQUOT-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

           MOVE "S  A220TITLE:FR." TO VDU.
           MOVE "SALES FINANCE QUOTER" TO VDUBUF.
           PERFORM SCREENIO.
       FORM-RESET-EXIT.
           EXIT.

      **************************************************************************
       SETUP-LINK-INFO SECTION.
      **************************************************************************
           MOVE SFQUOT-HELP--FILE TO HELP-LINK-FILE.
           MOVE SFQUOT-HELP--DIR TO HELP-LINK-DIR.

      **************************************************************************
       VDU-CONVERT-FIELD SECTION.
      **************************************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBWI/SFQUOT_EVA.CPY".
              COPY "LIBWI/RSVENT_EVA.CPY".
              COPY "LIBWI/FEEENT_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      *********************************
       MISC-PARAGRAPHS SECTION.
      *********************************
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/SYSTEM.CPY".
       COPY "LIBGB/GETENV.CPY".
       COPY "LIBGB/MKTEMP.CPY".

       SEND-PURCHASE-FIELDS.
           PERFORM GET-PURCHASE-FIELDS.
           MOVE PD-LNAMT TO DPUR-LNAMT.
           MOVE PD-PROCEEDS TO DPUR-DLCK.
           COMPUTE DPUR-ANTIC = PD-ANTICERN(1) + PD-ANTICERN(2)
                              + PD-ANTICERN(3) + PD-ANTICERN(4)
                              + PD-ANTICERN(5).
      *JTG   + PD-ANTICERN(5) + PD-ANTICERN(6). PERSP-LNCOST-FRMLA 'D'
           IF PD-DLRVPRIN(2) = 0 AND
              PD-DLRVPRIN(3) = 0
              MOVE PD-DLRVPRIN(1) TO DPUR-DLRVPRIN
           ELSE
              ADD PD-DLRVPRIN(1) PD-DLRVPRIN(2) PD-DLRVPRIN(3)
                                         GIVING DPUR-DLRVPRIN
              INSPECT DPUR-DLRVPRIN REPLACING LEADING " " BY "*".
           IF PD-DLRVPRIN-FG(1) = "C" AND
              PD-DLRVPRIN-FG(2) = "C" AND
              PD-DLRVPRIN-FG(3) = "C"
              MOVE "C" TO DPUR-DLRVPRIN-FG
           ELSE
              MOVE "I" TO DPUR-DLRVPRIN-FG.

           IF PD-DLRVCHGS(2) = 0 AND
              PD-DLRVCHGS(3) = 0
              MOVE PD-DLRVCHGS(1) TO DPUR-DLRVCHGS
           ELSE
              ADD PD-DLRVCHGS(1) PD-DLRVCHGS(2) PD-DLRVCHGS(3)
                                         GIVING DPUR-DLRVCHGS
              INSPECT DPUR-DLRVCHGS REPLACING LEADING " " BY "*".
           IF PD-DLRVCHGS-FG(1) = "C" AND
              PD-DLRVCHGS-FG(2) = "C" AND
              PD-DLRVCHGS-FG(3) = "C"
              MOVE "C" TO DPUR-DLRVCHGS-FG
           ELSE
              MOVE "I" TO DPUR-DLRVCHGS-FG.

           IF PD-DLPARVPAID = 0
              MOVE PD-DLPARVHELD TO DPUR-DLPARV
           ELSE
              ADD PD-DLPARVHELD PD-DLPARVPAID GIVING DPUR-DLPARV
              INSPECT DPUR-DLPARV REPLACING LEADING " " BY "*".
           IF PD-DLPARVHELD-FG = "C" AND
              PD-DLPARVPAID-FG = "C"
              MOVE "C" TO DPUR-DLPARV-FG
           ELSE
              MOVE "I" TO DPUR-DLPARV-FG.

           MOVE PD-DLDISC TO DPUR-DLDISC.
           MOVE PD-DLDISC-FG TO DPUR-DLDISC-FG.

           MOVE WS-YIELD TO DPUR-YIELD
                         PD-YLDRATE.

           MOVE DPUR-BUF TO WR-BUF.
           MOVE DPUR-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       RESET-TITLE.
      *    IF LOAN-QUOTE-FG = "Q"
      *       MOVE "S  A170 TITLE." TO VDU
      *       MOVE "** SALES QUOTES **" TO VDUBUF
      *       PERFORM SCREENIO.

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
                       PD-ORGTERM = 12 )
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

           MOVE "S  A000130." TO VDU.
           MOVE NDTE-DATE TO PD-1STPYDATE
                             PD-ORIG-1STPYDATE
                             CIP-1STPYDATE
                             DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO VDU-DATE-EDIT
           PERFORM SCREENIO.

       DISPLAY-BALLOON-LAST.
           MOVE "S  A160P170:P." TO VDU.
           IF PD-BALLOON-LAST-FG = "B"
              MOVE "17 BALLOON AMT :" TO VDUBUF
           ELSE
              MOVE "17 LAST PAYMENT:" TO VDUBUF.
           PERFORM SCREENIO.


       SEND-BW-MAKERCD.
           MOVE "S  A010  2." TO VDU.
           MOVE ELE TO VDUROW.
           MOVE PD-MAKERCD(ELE) TO VDUBUF.
           PERFORM SCREENIO.

       GET-CODE.
           MOVE "GC"     TO CDB-TYPE.
           MOVE PD-OWNBR TO CDB-BRANCH.
      *--> SET BEFORE       CBD-CODE.
           PERFORM OPEN-CDB1-FILE.
           PERFORM READ-CDB1-FILE.
           PERFORM CLOSE-CDB1-FILE.
           IF IO-FG = 9
              MOVE " * NOT ON FILE * " TO CDB-DESC.

      *----------------------------------------------------------------
      *   INVALID DUE DAY FOR SEMI MONTHLY ACCOUNTS
      *---------------------------------------------------------------
       INVALID-DUE-DAY.
           IF PD-1STPYDA = 31
              MOVE " 31ST OF MONTH INVALID FOR SEMI-MONTHLY PAPER"
                                                           TO MESS
           ELSE
              STRING
            PD-1STPYDA,"TH OF MONTH INVALID FOR SEMI-MONTHLY PAPER"
                                       DELIMITED BY "  " INTO MESS.

      *-----------------------------------------------------------------

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
           MOVE PD-UNITPER-CD   TO DATER-UNITPER-CD.
           MOVE PD-UNITPER-FREQ TO DATER-UNITPER-FREQ.
           MOVE PD-1STPYDATE    TO NDTE-DATE.
           COMPUTE NDTE-HOLD = (PD-ORGTERM - 1).
           PERFORM INCREMENT-UNITPER.
       REMOVE-SCRATCH-IF-RECALL.
           IF RECALL-UPDATE
              PERFORM DELETE-SCRATCH.
       REMOVE-QUOTE-IF-LOAN.
           IF QUOTE-CHANGED-TO-LOAN
              MOVE SAVE-QUOTE-SSN TO PD-SSNO(1)
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
      *    MOVE SCREEN-LINE1 TO D-211.
      *    MOVE SCREEN-LINE2 TO D-212.
      *    MOVE SCREEN-LINE3 TO D-213.

       VALIDATE-FIELDS-AGAINST-MAX.
           MOVE 0 TO IO-FG.
           IF NEW-FLAG NOT = 1
              PERFORM VALIDATION.

       VERIFY-REQD-FLDS.
           PERFORM VERIFY-PROCEEDS-DOWN.
           PERFORM VERIFY-SETUP-PAYMENTS.
           IF NOREG-FG NOT = " "
              MOVE 9 TO IO-FG.
           PERFORM COMPUTE-DEALER-CHECK.
           IF TEST-PROCEEDS < 0
              MOVE "DEALER CHECK CAN'T BE NEGATIVE!" TO MESS
              PERFORM SEND-MESS
              MOVE 9 TO IO-FG.
           IF PD-ANTICERN(1) < 0
                  OR PD-ANTICERN(2) < 0
                   OR PD-ANTICERN(3) < 0
                    OR PD-ANTICERN(4) < 0
                     OR PD-ANTICERN(5) < 0
      *JTG   + PD-ANTICERN(5) + PD-ANTICERN(6). PERSP-LNCOST-FRMLA 'D'
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
                 MOVE 12 TO ELE
                 PERFORM SP-EDT-TABLE-LOOKUP.
           IF IO-FG = 0
              IF NOT (COP-CALCULATION-OK)
                IF CALL-LOAN-CALC-FG = "Y"
                 PERFORM ERROR-IN-CALCULATIONS-MSG
                 MOVE 9 TO IO-FG.

       SETUP-PROCEEDS-DOWN.
           IF NOT (PD-BALLOON-FG = "I" AND PD-REGPYAMT-FG = "I")
              IF PD-DOWNPY-FG = "C"
                 COMPUTE PD-DOWNPY ROUNDED =
                                PD-SELLPR * PD-PERDOWN / 100.
       VERIFY-PROCEEDS-DOWN.
           IF PD-SELLPR NOT > PD-DOWNPY
              MOVE "DOWN PAYMENT CAN'T BE MORE THAN SELL PRICE!"
                                                           TO MESS
              PERFORM SEND-MESS
              MOVE 9 TO IO-FG
           ELSE
           IF PD-SELLPR NOT > 0
              MOVE "SELLING PRICE MUST BE MORE THAN ZERO!"
                                                     TO MESS
              PERFORM SEND-MESS
           ELSE
           IF PD-DOWNPY < 0
              MOVE "DOWN PAYMENT CAN'T BE NEGATIVE!"
                                                     TO MESS
              PERFORM SEND-MESS
              MOVE 9 TO IO-FG
           ELSE
           IF PD-PERDOWN < 0
              MOVE "PERCENT DOWN CAN'T BE NEGATIVE!"
                                                     TO MESS
              PERFORM SEND-MESS
              MOVE 9 TO IO-FG.

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
           MOVE PD-ORGTERM   TO CIP-TERM.

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
      *<1225J>
              IF SP-CAL-FLAT-MONTHLY(1)
                 MOVE "MUST BE 'C' OR 'A'" TO MESS
              ELSE
                 MOVE "MUST BE 'C', 'E', OR 'A'" TO MESS
              END-IF
      *<1225J>
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
       NOT-FOUND.
           MOVE "RECORD NOT FOUND" TO MESS.
           PERFORM SEND-MESS.
       SP-INVALID.
           MOVE "ENTRY INCONSISTENT WITH STATE PARAMETERS" TO MESS.
           PERFORM SEND-MESS.
       NOT-SALEFIN.
           MOVE "INVALID FOR SALES FINANCE ENTRY!" TO MESS.
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
           MOVE PD-APRATE  TO VERIFY-TERMCEIL-APR.
           MOVE "Y"        TO VERIFY-TERMCEIL-EFF-FG.
           MOVE TRANS-DATE TO VERIFY-TODAY-DATE.

           PERFORM SET-BW1-FOR-SPEDT.

           MOVE BW-BDATE   TO VERIFY-AGE.
           MOVE BW-REFDATE TO VERIFY-REFDATE.
           MOVE " "        TO VERIFY-REJECT-TERM-FG.
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

           MOVE BW-BDATE      TO VERIFY-AGE.
           MOVE BW-REFDATE    TO VERIFY-REFDATE.
           MOVE "Y"           TO VERIFY-REJECT-TERM-FG.

           PERFORM VERIFY-TERMCEIL.

           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 4
              MOVE VERIFY-REJECT-TERM(SUB) TO CIP-INS-REJECT-TERM(SUB)
           END-PERFORM.

      *-----------------------------------------------------------------
       PROTECT-FIELDS.
           MOVE STAT TO CHFG(5)  CHFG(20) CHFG(21)
                        CHFG(34) CHFG(35) CHFG(36) CHFG(37) CHFG(38)
                        CHFG(39) CHFG(40) CHFG(41) CHFG(42).

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
           MOVE STAT TO CHFG(11) CHFG(14) CHFG(15) CHFG(16) CHFG(17)
                        CHFG(18) CHFG(19)
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
       COPY "LIBGB/DECLARE_SQL.CPY".
           PERFORM CLOSE-SP1-FILE.
           PERFORM CLOSE-PD2-FILE.
           PERFORM CLOSE-PD1-FILE.
           PERFORM CLOSE-LC1-FILE.
           PERFORM CLOSE-IC1-FILE.
           PERFORM CLOSE-DL1-FILE.
           PERFORM CLOSE-CDB1-FILE.
           PERFORM CLOSE-CD1-FILE.
           PERFORM CLOSE-BW1-FILE.
           PERFORM CLOSE-GI1-FILE.
           PERFORM CLOSE-BR-FILE.
       COPY "LIBLP/LPSPGS_SQL.CPY".
       COPY "LIBLP/LPPDGS_SQL.CPY".
       COPY "LIBLP/LPLCGS_SQL.CPY".
       COPY "LIBLP/LPICGS_SQL.CPY".
       COPY "LIBLP/LPDLGS_SQL.CPY".
       COPY "LIBLP/LPCDBGS_SQL.CPY".
       COPY "LIBLP/LPCDGS_SQL.CPY".
       COPY "LIBLP/LPBWGS_SQL.CPY".
       COPY "LIBGL/GLGIGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBLP/LPLC1RN.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBLP/LPDL1RN.CPY".
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

      **************************************
       END-ROUTINE SECTION.
      **************************************
           PERFORM CLOSE-FILES.
       EXIT-PROG.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY SFQUOT-SCREEN.
           DESTROY SFQUOT-WINDOW-HANDLE.
       STOP-RUN.
           EXIT PROGRAM.
