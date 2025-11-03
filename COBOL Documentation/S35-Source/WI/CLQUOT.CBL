       IDENTIFICATION DIVISION.
       PROGRAM-ID.      CLQUOT.
       DATE-WRITTEN.    032587.
      ******************************************************************
      *
      *  DESCRIPTION:   CAN LOAN BUILDER   (INCLUDING GIFT NET CANS)
      *                  CALLED FROM LPM2MU MAINTENANCE MENU
      *
      *  ---------------------------------------------------------
      *  |                    N  O  T  E:                        |
      *  |                                                       |
      *  |       THIS PROGRAM USES A48 PAYMENT SCHEDULE LOGIC    |
      *  ---------------------------------------------------------
      *
      * REV:
      *  051388 JPN MOVED COP-SIM-RATE TO PD-SMPRATE.
      *  082288 BAW ADDED 2 OTHER INSURANCES
      *  093088 BAH VERIFICATION ON INSURED
      *  103188 BAH ALLOW 0 PROCEEDS ON 'SC'
      *  111088 JTG ADDED CLEAR OF PD-STATE-WORKER
      *  010989 SLC ADDED ANTICIPATED CALC.
      *  020189 BAH ADDED PD-PROMDATE2
      *  031089 SLC ADDED PD-PRIOR-UNEARNED.
      *  031389 BAH CHANGED CIP-PRBAL IN VERIFICATION OF PROCEEDS
      *             TO BE PRIOR-LNAMT
      *  032389 JTG REMOVED CLEAR OF LN-EXTDAYS LN-GRACEDAYS
      *             ADDED SET OF LN-ORIG-1STPYDATE
      *  081889 JTG ADDED LOGIC FOR PD-PRPAID-AHEAD (GA)
      *                             PD-RESTRICT-ADJ
      *  090189 BAH CHANGED PASS-PRLNNO TO 8
      *  120889 JTG CORRECTED BUG CLEAR STATECIP-STATE-W
      *  011990 JTG ADDED UPDATE STOP IF INS-COVERAGE ERROR
      *  020790 JTG ADDED LOGIC FOR "IA" ADDON INSURANCE FEATURE
      *  021290 SLC ADDED LOGIC FOR ADDPROG FEATURE FOR COLLATERAL.
      *  051890 JTG ADDED LOGIC FOR PD-RESTRICT-ADJ (GA)
      *  102990 BAH ADDED AUTO PRLNNO DISPLAY FOR PB, RN, AND SC
      *  050692 JTG MODIFIED FOR LPSPED(INS AGE / BENEFIT TEST)
      *  083193 BAH EXT CHRGS WERE GOING TO SLOT 2 INSTEAD OF BEING
      *             ADDED INTO INTEREST SLOT 1
      *  111593 BAH ORIG-1STPAY NOT GETTING SET IN ALL CASES
      *  031894 JTG CHGED BACK SO IF INSURANCE CODES 'NNN' IS CHANGED
      *             CALCZL WILL RECALCULATE, WITH 0 TO CIP-NO-TIMES-CALLED
      *  061494 JTG CORRECTED DISPLAY OF TOTAL REPAY ON QUOTE RECALL
      *  021695 JTG ADDED LOGIC TO ALLOW INSURANCE COVERAGE OVERRIDES
      *             TO REMAIN AFTER QUOTE ARE RECALLED
      *  031395 JTG ADDED SET OF COP-SMP-RATE RE: SP-INS-ADJ-PRIN-HALFINT
      *  032195 JTG STOPPED SAVE IF CALCULATION ERRORS
      *  062195 JTG EXPANDED FOR 10 INSURANCES
      *  062995 JTG ADDED LOGIC FOR O3 INSURANCE AND CALC OF O1
      *  081195 MJD ADDED LOGIC FOR GP-THE-ONLY-SF-SRCD
      *  093095 JTG ADDED LOGIC FOR GP-DEFAULT-O3-INS-COMPANY
      *  011796 JTG CHANGED SO O2 & O3 INS. DON'T COMPUTE DEFAULT EXPIRATION
      *             DATE UNLESS INITIAL INPUT
      *  MJD 042496 CHANGED TO ACCEPT 8 DIGIT ACCOUNT NUMBER FROM BWLNSC.
      *             ADDED MORELN WINDOW LOGIC. ADDED SCREEN DISPLAY OS SEQNO IF
      *             OTHER THAN 1.
      *  JTG 052996 FIXED BUG THAT LEFT PD-PRBAL WITH A VALUE, WHEN SWITCHING
      *             FROM "PB" OR "RN" OR "SC" TO --> "NB"
      *  BAH 062896 ADDED INSEXP(5) AND INSEXP(6) TO MAKE-DATES-CURRENT
      *             SO WHEN YOU RECALL A QUOTE, IT UPDATES THOSE DATES.PL#96
      *  JTG 111296 CORRECTED DISPLAY BUG RE: MISSING FIELD 305
      *  JTG 111296 CHANGED LN-INSTBL (LEVEL'S 9 & 10) TO FILLER
      *  JTG 112196 CHANGED TO USE HASHING ON SAVE QUOTES
      *  BLV 032497 F7 ON PAYOFF INQUIRIES; READ PD BEFORE DELETING IT
      *  MJD 060397 ADDED SCSCAN-TRANS-DATE.
      *  JTG 072297 ADDED LOGIC FOR CAN LOAN FILE
      *  JTG 082197 CHANGED TO USE 'LPZLCL' TO CLEAR-CALC-AREA-ZL
      *  GLF 090997 REPLACED 'IO' ON GP-FILE WITH GET 'GPENV'
      *  JTG 121597 ADDED TEST FOR CIP-NO-OF-TIMES-CALLED < 800, TO ALLOW RESET
      *             OF CEP FIELDS, WDT # 148, CASTLE #163 RE: RATE OVERRIDE
      *  JTG 020498 SET CIP-CAN-LOAN-NO, RE: LOGIC TO FORCE ONE CALENDAR MONTH
      *             SEE CALCZL.C
      *  BLV 020598 CHANGED CANNED LOAN OPTION FROM F5 TO F2
      *  JTG 032498 STOPPED SINGLE PAYS LOANS, NO SOLUTION YET
      *  BAH 040698 GP-DEFAULT-O3-INS-COMPANY 0 WAS NOT ALLOWING ANY INS CO.
      *  JTG 042198 CORRECTED SETTING OF BRANCH NO'S, USING THE-GROUP-REFBR
      *  JTG 042298 ADDED EDIT ON THE-GROUP-REFBR'S, BR-CAN-GROUP-ID TO MATCH
      *             THE ENTERED 'THE-GROUP-ID'
      *   CS 042998 ADDED "RB" AND "RO" (RENEW BANKRUPT AND RENEW OTHER)
      *  KEC 072098 ADDED CHECK FOR NEW FLAG ON 'SPR' CAN-LOAN FLAG
      *              ( SP-CAN-LOANS-FG ).  FOR WORLD          PR#: 00187
      *  KEC 090498 ADDED 'MOVE LC-LOANTYPE TO PD-LOANTYPE';
      *              FOR REGENCY REVOLVING.
      *  JTG 092598 CHANGED CALL FROM LONPZL TO CALCZL
      *  JTG 101398 ADDED SET OF LCSCAN-REVOLVING = 'N' AND STOPPED ENTRY
      *             OF REVOLVING CLASSES, REGENCY REVOL
      *  JTG 122198 REPLACED SP-SCFRMLA = 1 OR 8 OR 28 TESTS
      *             WITH 88 SP-SCFRMLA-POINTS
      *  JTG 990113 ADDED SP-EDT-TYPE 'P' POINTS,                   REGENCY #012
      *  JTG 000113 ADDED SP-INSREJECT-FG OPTION 'C', WHICH WILL CAUSE WRITING
      *             NO. OF MONTHS INSURANCE NOT IN VIOLATION, WISCONSIN #1032
      *  MJD 000207 ADDED GP-OT-INS-TERM (TFC PR#48) IF 'Y' THEN ALLOW
      *             ALLOW TERM ENTRY FOR O2O3O4O5 INSURANCE.
      *  MJD 000228 ADDED GP-ADV-TO-1ST-NO-CORR = "A" . IF DATE OF THE LOAN
      *             IS ON THE 26 THRU 31 THEN ADVANCE THE FIRST PAYMENT
      *             DUE DATE TO THE FIRST OF THE SECOND MONTH FOLLOWING THE
      *             DATE OF THE LOAN. (REGMGM PR#1099)
      *  JTG 000315 CORRECTED BUG WITH SRCD EX. FB AND PRIOR ACCOUNT NO. ENTRY
      *             WISC #71249 J006  NOTE: NO CHANGES REQUIRED
      *  BLV 000324 BARB & JOHN, CHANGED TO CHECK-SP-EFFDATE WHEN CAN IS
      *             CALLED UP & FIX PD-LAWCODE IF SPR HAS EXPIRED; NOW
      *             YOU JUST NEED TO CALL UP CAN & UPDATE TO GET NEW
      *             LAW CODE IN; USED TO HAVE TO GO UP & ENTER LOAN DATE
      *  BLV 000324 BARB & JOHN, TAKE CONDITIONS OFF MOVING PD-PROCEEDS TO
      *             PD-CAN-LOAN-MAX AFTER CALCULATION; NEW CANS WERE GETTING
      *             WRITTEN WITH MAX OF 0
      *  KEC 000530 {PR#00048} TFC [JUDY BRANIFF];
      *             ADDED THE FOLLOWING 'IF' CONDITION FOR ENTERING THE
      *             INSURANCE TERM (GP-OT-INS-TERM = "Y"):
      *                   IF ( VDUNUM0 > 360 )
      *                      GO TO ENTER-IC-TERM.
      *  JTG 020111 ADDED LOGIC FOR TESTING CALCZL.C BY SETTING ENVIRONMENT
      *             VARIABLE "CALL_TESTZL" WHICH MAKE THE CALL TO
      *             "LP/TESTZL" INSTEAD OF "LP/CALCZL"
      *  JTG 010831 TESTED COP-RETURN-CODE = 'V' FOR DISPLAY OF ERROR IN CALC
      *                                                         LENDMARK PR#J021
      *  JTG 020220 ADDED "CT" COUNTY TAX CODE KENTUCKY AND PERCENTS
      *             ADDED "CY" CITY   TAX CODE KENTUCKY AND PERCENTS  WORLD #288
      *
      *  KEC 020624 {PR#01823} REGIONAL ACCEPTANCE <A15> [KEVIN MULLEN]
      *             ADDED LOGIC FOR USING 'CD-GC-FEE-MAXAMT'
      *  JTG 030806 CORRECTED TEST FOR PROCEEDS < 0 WHEN MEMO INSURANCE
      *             PREMIUMS ARE ENTERED THEY WERE PART OF THE TEST
      *             AND THEY SHOULD NOT BE CONSIDERED            LENDMARK #J032
      *  BAH 031023 ADDED GP-DEFAULT-O3-INSOURS, LENDMARK PR# 2180
      *  JTG 040213 STOPPED DISPLAY OF NON MONTHLY CAN LOANS IN CLSCAN
      *             WINDOW                                            WORLD J043
      *  BAH 040304 ADDED SP-DEFAULT-O2-INS-COMP FOR LENDMARK PR# 2258
      *  JTG 040922 ADDED NEW LOGIC FOR SET OF SYSTEM DATE
      *             LIFE OF THE SOUTH  SYSDATE_YYMMDD                 WORLD JLOS
      *  BAH 050222 WHEN ENTERRING SUB CLASS, YOU DONT KNOW THE LOANDATE YET TO
      *             CHECK IF SPR IS EXPIRED, SO IT WAS JUST GRABBING THE FIRST
      *             LAWCODE FOUND TO DISPLAY THE DESCRIPTION, WHICH ISNT
      *             CORRECT IF THEY EXPIRE AND BUILD A NEW ONE WITH A DIFFERENT
      *             DESCRIPTION, VERY CONFUSING...AT LEAST SKIP EXPIRED SPR.
      *             RIVERSIDE PL# 467
      *   CS 050425 ADD TEST ON SP-INSURANCE-WRITTEN TO LOCK/UNLOCK 10 FEE CODES
      *                                                 [PR#2479 LENDMARK]
      *   CS 050427 IF INSPREM = ZEROS, SET CORRESPONDING FIELDS TO ZERO...
      *             PREVIOUSLY WE COMPUTED COMMISSION ON A ZERO PREMIUM WHEN
      *             COMM WAS FLAT DOLLAR AMOUNT PR#479 NSA
      * JTG 050819 CHANGED SAVE LOAN, ETC TO USE PD-ANTICERN(4) FOR DISPLAY
      *            OF MAINTENANCE FEES INSTEAD OF THE STORED TRUNCTATED
      *            PD-MAINTFEE * PD-ORGTERM WHICH DOSEN'T EQUAL THE COMPUTED
      *            'VAT' VALUE ADDED TAX IN PD-ANTICERN(4)           WORLD #0408
      * JTG 050919 CHANGED SAVE LOAN, ETC CHANGED TO USE THE COP-MAINT-FEE
      *            VALUE FROM CALCZL 'VAT' FOR PD-ANTICERN(4)        WORLD #0408
      * BAH 051128 UNDO CHANGE FROM 050222 PL# 467, WOULD NOT LET YOU BOOK A
      *            LOAN WITH AN EXPIRED SPR, AND YOU SHOULD BE ABLE TO.
      *            INSTEAD OF SKIPPING EXPIRED SPR IN READ-NEXT, JUST REDISPLAY
      *            THE SUBCLASS DESCRIPTION AFTER ENTERING THE LOANDATE.
      * JTG 070326 WORLD SPR'S ARE FLAGGED WITH GP-ADV-TO-1ST-NO-CORR = "Y"
      *            WHICH WILL ADVANCE TO THE NEXT MONTH IF THE DEFAULT
      *            PAYMENT DUE DATE FALLS ON A NON EXISTING DAY
      *            THIS CHANGE OVERRIDES THIS OPTION FOR LOANS BOOKED WHEN
      *            GP-I-AM-WORLD AND PD-ORGST = "TN" AND PD-CLASS = 01 AND
      *            PD-ORGTERM = 12                                   WORLD 0532
      * MJD 070430 FIXING PROBLEM WITH MEXICO LOANS USING MFFRMLA 05.
      *            BASICALLY IF THE MFFEE CALCULATED IS NOT DEVISIBLE
      *            BY THE TERM, TO WITHIN .XX THEN THE LOAN IS OUT OF
      *            BALANCE.   IN ORDER TO WORK AROUND THIS WE HAVE DECIDED
      *            TO FORCE THE LOAN PAYMENT TO BE INPUT WHEN THE CAN IS MADE.
      *            I HAVE ADDED LOGIC TO PREVENT THE SAVING OF THE CAN IF
      *            THE MFFRMLA = 5 AND THE PAYMENT IS NOT INPUT.
      * BAH 090904 ALLOW SP-INSMAXCHGABL-OVRIDE TO WORK WITH O1, FINMGM PR# 3447
      * MJD 091204 ADDED LOGIC TO ALLOW ZERO PROCEEDS LOANS    REGMGM #3460
      *            (NOTE: THIS DOES NOT WORK CONSISTENTLY WITH MEXICO VAT TAX
      *            DUE TO THE NUMBER OF BLACK FLIPS WE HAVE IMPLEMENTED
      *            TO MAKE THE NUMBERS FOR AND PROVIDE EVEN PAYMENT AMOUNTS.)
      * BAH 110523 PASSWORD PROTECT ADDON SOURCE CODES, ET, LP, NP, RT, ST
      *            AND RB, REGMGM PR# 3792
      * MJD 111212 ADDED LOGIC FOR SP-ADV-TO-1ST-NO-CORR
      *            IF SET THIS WILL OVERIDE THE GP FLAG.             71226 #3879
      * BAH 111229 ADDED PP-PREM-SEQ TO PASSWORD CHANGING PP PREM REGWAN PR#3935
      *  CS 150814 IF GP-I-AM-EXCEL, TEST THE FEECODE AMOUNT AGAINST CD-
      *            GC-BRANCH-COMM-AMOUNT  EXCEL PR#4821
      *  CS 150922 ADD TEST FOR 202020/020202 IF EXCEL PR#4821
      * KEC 2016.0309 CHANGED CD-CL???? TO BE CD-CL-????.
      *               CHANGED CD-GROUP, CD-SECURED & CD-TRW TO BE
      *               CD-CO-GROUP, CD-CO-SECURED & CD-CO-TRW.
      * KEC 2016.0318 CHANGED CD-FEE???? TO CD-GC-????.
      * KEC 2016.0328 CHANGED CDSCAN-WINDOW-BUF TO HAVE MATCHING CD1-KEY
      *               LAYOUTS (FOR EACH CODE TYPE; CD-TYPE).
      *               CDSCAN-CD1-KEY IS THE NEW (PRIMARY) WITH REDEFINES OF
      *               CDSCAN-??-KEY (NEW).
      *   CS 160808 CHANGED SCSCAN-KEY TO SCSCAN-SP1-KEY
      *             COMMENT %% CHECK-FOR-MORELN
      * KEC 2017.0227 CHANGED PROCEEDS-LOW, PROCEEDS-HIGH, POINTS-LOW,
      *               POINTS-HIGH TO MATCH SIZE OF SP-EDT-AMT 9(7)V99.
      * BAH 20170412 ACTIVATE 8 DIGIT DATES, PD0006
      * BAH 20190128 SPLIT CDFILE
      * BAH 20190625 REMOVED LC-REVOLVING, PT#0153
      * BAH 20200624 ADDED OPEN-CD1-FILE AND MOVED OPEN-IC1-FILE TO TOP
      *              REMOVED CLOSE-IC1-FILE #TST001
      * BAH 20221205 DONT ALLOW "TM" TO BE ENTERED AS A VALID FEECODE, THIS
      *              IS UNITED MOTOR CLUB SALES TAX #1582
      * BAH 20230314 FIXED CHECK ON SP-EDT-NOEDITS CINDY FOUND
      * BAH 20240221 WHEN PULLING UP A CAN WITH AN EXPIRED SPR, IT WAS LOSING
      *              THE PROGRAM FLOW FROM A "GO TO ENTER-ELE". I REMOVED
      *              THAT AND LET IT GO TO EXIT SECTION AND GO TO BOTTOM
      *              SELECTION TO ALLOW THEM TO DELETE THE CAN. #1645
      * BAH 20240415 REMOVED CDBSCN-GC-FEE-GIFTNET-ISO
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
           "@(#)WI/CLQUOT.CBL S35 02/23/24-11:36:04 >".
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBLP/LP01DL.CPY".
       COPY "LIBLP/LP01DL_SQL.CPY".
       COPY "LIBLP/LPWSDL.CPY".
       COPY "LIBLP/LP01BW.CPY".
       COPY "LIBLP/LP01BW_SQL.CPY".
       COPY "LIBLP/LPWSBW.CPY".
       COPY "LIBLP/LP01CL.CPY".
       COPY "LIBLP/LP01CL_SQL.CPY".
       COPY "LIBLP/LPWSCL.CPY".
       COPY "LIBLP/LP01LC.CPY".
       COPY "LIBLP/LP01LC_SQL.CPY".
       COPY "LIBLP/LPWSLC.CPY".
       COPY "LIBLP/LP01SP.CPY".
       COPY "LIBLP/LP01SPA.CPY".
       COPY "LIBLP/LP01SPB.CPY".
       COPY "LIBLP/LP01SPC.CPY".
       COPY "LIBLP/LP01SP_SQL.CPY".
       COPY "LIBLP/LPWSSP.CPY".
       COPY "LIBLP/LP01CD.CPY".
       COPY "LIBLP/LP01CD_SQL.CPY".
       COPY "LIBLP/LPWSCD.CPY".
       COPY "LIBLP/LP01CDB.CPY".
       COPY "LIBLP/LP01CDB_SQL.CPY".
       COPY "LIBLP/LPWSCDB.CPY".
       COPY "LIBLP/LP01IC.CPY".
       COPY "LIBLP/LP01IC_SQL.CPY".
       COPY "LIBLP/LPWSIC.CPY".
       01  WK-IFN          PIC X(2) VALUE "WK".

       01  WK-KEY          PIC 99.

       01  ORIGINAL-CL-REC    PIC X(CL-SIZE).
       01  CHANGED-CL-REC     PIC X(CL-SIZE).

       COPY "LIBLP/LP01PD.CPY".
       01  THE-GROUP-ID                        VALUE " ".
           03  THE-GROUP-ID-NUM  PIC 9(4).
       01  THE-GROUP-REFBR         PIC 9(4)      VALUE 0.

       01  CLQUOT-LINES              PIC 99   VALUE 23.
       01  CLQUOT-COLUMNS            PIC 99   VALUE 80.
       01  CLQUOT-BEGIN-Y            PIC 99   VALUE 1.
       01  CLQUOT-BEGIN-X            PIC 99   VALUE 1.

       01  ONE             PIC S9.
       01  ELE             PIC 9(6) COMP.
       01  CL-ELE          PIC 99 VALUE 25.
       01  AH-ELE          PIC 99 VALUE 26.
       01  PP-ELE          PIC 99 VALUE 27.
       01  O1-ELE          PIC 99 VALUE 28.
       01  O2-ELE          PIC 99 VALUE 29.
       01  O3-ELE          PIC 99 VALUE 30.
       01  FEECODE-TABLE   VALUE " ".
           03  FEECODE     OCCURS 4 TIMES
                           PIC XX.
       01  CDCODE-FEE-FG   PIC 9.
       01  VOIDGN-FG       PIC X.
       01  ACTION.
           03  CHKEY       PIC X(4).
           03  CHNO        PIC 9(2).
       01  IO-TYPE         PIC X(3).
       01  NEW-FLAG        PIC 9.
       01  FIRST-ELE       PIC 99    VALUE 0.

       01 LOAN-CALCULATOR-CALLED-FG    PIC X       VALUE " ".

      *    ----------------------------------------------------
      *    SINCE QUOTERS DON'T HAVE BORROWERS THESE WORKERS WILL
      *    BE PASSED TO CALCZL/CALCZ3, '   ' == NO CODE
      *    ----------------------------------------------------
       01  STATE                      PIC X(2)   VALUE " ".
       01  CITY-TAX-CD                PIC X(3)   VALUE " ".
       01  COUNTY                     PIC X(3)   VALUE " ".
      *    ----------------------------------------------------

       01  CAN-LOAN-NO        PIC XX    VALUE " ".
       01  SAVE-SSNO          PIC 9(10) COMP.
       01  MSEL-LIST              PIC X(5) VALUE "MSEL.".
       01  ADV-TO-1ST-NO-CORR    PIC X       VALUE " ".

       01  SUB             PIC 9(6) COMP.
       01  OVER-20         PIC 99   COMP.
       01  I-LEVEL         PIC 9(6) COMP.
       01  INSPECT-W       PIC X(86).
       01  ADD-ON          PIC X(25).
       01  DEFAULT-APR-FG  PIC X.
       01  QUOTE-ID-FG     PIC X        VALUE " ".
           88  QUOTE-ONLY               VALUE "Q".
           88  QUOTE-CHANGED-TO-LOAN    VALUE "L".
       01  PRIOR-LOAN-STAT PIC X.
           88  ERROR-IN-PRIOR-LOAN      VALUE "E".
       01  LEGEND-01-NO-CAN.
           03  FILLER      PIC X(30)  VALUE
           "F1, F7-EXIT, F4-QUOTE SCAN:".
       01  LEGEND-01-CAN-ONLY.
           03  FILLER      PIC X(31)  VALUE
           "F7-EXIT, F2-CANNED LNS, F4-QUOT".
           03  FILLER      PIC X(33)  VALUE
           "ES, ENTER 2 CHAR CANNED LOAN NO:".
       01  LEGEND-02.
           03  FILLER      PIC X(36)  VALUE
           "F7-EXIT, F1-NEW, F2-CANNED LOAN SCAN".
           03  FILLER      PIC X(31)  VALUE
           ", ENTER 2 CHAR CANNED LOAN NO:".
       01  LEGEND-03.
           03  FILLER      PIC X(31)  VALUE
           "F7, F1, F4-QUOTE SCAN, F2-CANNE".
           03  FILLER      PIC X(34)  VALUE
           "D LNS, ENTER 2 CHAR CANNED LOAN#:".
       01  LEGEND                     PIC X(70).

       01  WS-MESSAGES.
           03  MESSAGE-INVALID-SPR-CL.
               05  FILLER PIC X(25) VALUE "INVALID SUB CLASS NUMBER:".
               05  FILLER PIC X(25) VALUE " FOR CAN LOANS ONLY!     ".

           03  MESSAGE-INVALID-SPR-NON-CL.
               05  FILLER PIC X(25) VALUE "INVALID SUB CLASS NUMBER:".
               05  FILLER PIC X(25) VALUE " FOR NON CAN LOANS ONLY! ".

      ********************************
      *   S C R E E N   B U F F E R  *
      ********************************
       01  DISPLAY-BUF1        VALUE " ".
           03  D-LCNO          PIC Z9.
           03  D-LC-DESC       PIC X(16).
           03  D-010           PIC X(2).
           03  D-011           PIC X(33).
           03  D-LOANAMT       PIC Z(6)9.99  BLANK ZERO.
           03  D-ANTIC         PIC Z(6)9.99  BLANK ZERO.
           03  D-050           PIC XX.
           03  D-051           PIC 9(4)      BLANK ZERO.
           03  D-052           PIC Z9.
           03  D-053           PIC X(13).
           03  D-060           PIC 99/99/99  BLANK ZERO.
           03  D-061           PIC 99/99/99  BLANK ZERO.
           03  D-062           PIC 99/99/99  BLANK ZERO.
           03  D-070           PIC 99/99/99  BLANK ZERO.
           03  D-071           PIC ZZZ.
           03  D-080           PIC X(3).
           03  D-081           PIC X(15).
           03  D-090           PIC X(2).
           03  D-091           PIC X(15).
           03  D-100           PIC X(2).
           03  D-101           PIC X(15).
           03  D-110           PIC 9(6)      BLANK ZERO.
           03  D-111           PIC Z(5)9.99  BLANK ZERO.
           03  D-120           PIC Z(5)9.99  BLANK ZERO.
           03  D-121           PIC X.
           03  D-130           PIC X(3).
           03  D-131           PIC Z9.99     BLANK ZERO.
           03  D-132           PIC X.
           03  D-140-180       OCCURS 5.
               05  D-1FEECD    PIC XX.
               05  D-1FEEAMT   PIC Z(3)9.99  BLANK ZERO.
               05  D-2FEECD    PIC XX.
               05  D-2FEEAMT   PIC Z(3)9.99  BLANK ZERO.
           03  D-190           PIC 9(4).99   BLANK ZERO.
           03  D-191           PIC X(15).

       01  DISPLAY-BUF2       REDEFINES DISPLAY-BUF1.
           03  D-200           PIC X(20).
           03  D-201           PIC X(20).
           03  D-202           PIC X(20).
           03  D-210           PIC ZZ9.999   BLANK ZERO.
           03  D-211           PIC X.
           03  D-220           PIC ZZ9.999   BLANK ZERO.
           03  D-230           PIC ZZ9.999   BLANK ZERO.
           03  D-240           PIC ZZ9.999   BLANK ZERO.
           03  D-250-280       OCCURS 4.
               05  D-1COMP     PIC 99        BLANK ZERO.
               05  D-1OUR      PIC X.
               05  D-1TERM     PIC ZZ9       BLANK ZERO.
               05  D-1COVR     PIC Z(5)9.99  BLANK ZERO.
               05  D-1PREM     PIC Z(3)9.99  BLANK ZERO.
               05  D-INS-FG    PIC X.
           03  D-290-300       OCCURS 2.
               05  D-2COMP     PIC 99        BLANK ZERO.
               05  D-2OUR      PIC X.
               05  D-2EXP      PIC 9(6)      BLANK ZERO.
               05  D-2TERM  REDEFINES D-2EXP
                               PIC Z(6)      BLANK ZERO.
               05  D-2COVR     PIC Z(5)9.99  BLANK ZERO.
               05  D-2PREM     PIC Z(3)9.99  BLANK ZERO.
           03  D-310           PIC Z(5)9.99  BLANK ZERO.
           03  D-311           PIC X.
           03  D-320           PIC Z(5)9.99  BLANK ZERO.
           03  D-321           PIC X.
           03  D-330           PIC Z(5)9.99  BLANK ZERO.
           03  D-331           PIC X.
           03  D-340           PIC Z(6)9.99  BLANK ZERO.
           03  D-350           PIC Z(6)9.99  BLANK ZERO.
           03  D-360           PIC Z(6)9.99  BLANK ZERO.
           03  D-370           PIC Z(5)9.99  BLANK ZERO.
           03  D-380           PIC Z(5)9.99  BLANK ZERO.
           03  D-381           PIC X.
           03  FILLER          PIC X(217).

       01  DEF-BUF            REDEFINES DISPLAY-BUF1.
           03  DEF-ORGST       PIC XX.
           03  DEF-ORGBR       PIC 9(4).
           03  DEF-LOANDATE    PIC 99/99/99.
           03  FILLER          PIC X(462).

       01  DF2-BUF            REDEFINES DISPLAY-BUF1.
           03  DF2-PROCEEDS          PIC Z(5)9.99  BLANK ZERO.
           03  DF2-PROCEEDS-FG       PIC X.
           03  DF2-POINTS            PIC Z9.99     BLANK ZERO.
           03  DF2-POINTS-FG         PIC X.
           03  DF2-OVRDRATE          PIC ZZ9.999   BLANK ZERO.
           03  DF2-OVRDRATE-FG       PIC X.
           03  DF2-EFFRATE           PIC ZZ9.999   BLANK ZERO.
           03  DF2-APRATE            PIC ZZ9.999   BLANK ZERO.
           03  DF2-ENDORATE          PIC ZZ9.999   BLANK ZERO.
           03  DF2-LNAMT             PIC Z(6)9.99  BLANK ZERO.
           03  DF2-ANTICIPATED       PIC Z(6)9.99  BLANK ZERO.
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
           03  DF2-TREPAY            PIC Z(6)9.99  BLANK ZERO.
           03  DF2-1STPYAMT          PIC Z(5)9.99  BLANK ZERO.
           03  DF2-REGPYAMT          PIC Z(5)9.99  BLANK ZERO.
           03  DF2-REGPYAMT-FG       PIC X.
           03  DF2-RETURN-CODE       PIC X.
           03  DF2-LOOP-COUNT        PIC Z(3).

       01  DEF-LIST            PIC X(12) VALUE
           "050 051 060.".

       01  DISPLAY-LIST1.
           03  FILLER          PIC X(13)  VALUE
           "LNKEY LCDESC ".
           03  FILLER          PIC X(54)  VALUE
           "010 011 LOANAMT ANTIC 050 051 052 053 060 061 062 070 ".
           03  FILLER          PIC X(12)  VALUE
           "071 080 081 ".
           03  FILLER          PIC X(56)  VALUE
           "090 091 100 101 110 111 120 121 130 131 132 140 141 142 ".
           03  FILLER          PIC X(56)  VALUE
           "143 150 151 152 153 160 161 162 163 170 171 172 173 180 ".
           03  FILLER          PIC X(20)  VALUE
           "181 182 183 190 191.".
       01  DISPLAY-LIST2.
           03  FILLER          PIC X(52)  VALUE
           "200 201 202 210 211 220 230 240 250 251 252 253 254 ".
           03  FILLER          PIC X(56)  VALUE
           "255 260 261 262 263 264 265 270 271 272 273 274 275 280 ".
           03  FILLER          PIC X(56)  VALUE
           "281 282 283 284 285 290 291 292 293 294 300 301 302 303 ".
           03  FILLER          PIC X(52)  VALUE
           "304 310 311 320 321 330 331 340 350 360 370 380 381.".

       01  DF2-LIST.
           03  FILLER          PIC X(50)  VALUE
           "120 121 131 132 210 211 220 230 240 LOANAMT ANTIC ".
           03  FILLER          PIC X(8)   VALUE
           "250 251 ".
           03  FILLER          PIC X(56)  VALUE
           "252 253 254 255 260 261 262 263 264 265 270 271 272 273 ".
           03  FILLER          PIC X(56)  VALUE
           "274 275 280 281 282 283 284 285 310 311 320 321 330 331 ".
           03  FILLER          PIC X(32)  VALUE
           "340 350 360 370 380 381 390 400.".

       01  PROMPT-FG      PIC X      VALUE " ".
       01  PROMPT-BUF.
           03  FILLER     PIC X(12)  VALUE "LOAN AMOUNT:".
           03  FILLER     PIC X(12)  VALUE "ANTIC. EARN:".
           03  FILLER     PIC X(31)  VALUE
               "CO O EX/TRM  COVERAGE PREMIUM  ".
           03  FILLER     PIC X(27)  VALUE
               "---OUTPUT------------------".
           03  FILLER     PIC X(16)  VALUE "20 COLLATERAL ID".
           03  FILLER     PIC X(6)   VALUE "25 CL:".
           03  FILLER     PIC X(6)   VALUE "26 AH:".
           03  FILLER     PIC X(6)   VALUE "27 PP:".
           03  FILLER     PIC X(6)   VALUE "28 O1:".
           03  FILLER     PIC X(6)   VALUE "29 O2:".
           03  FILLER     PIC X(6)   VALUE "30 O3:".
           03  FILLER     PIC X(15)  VALUE "31 SERVICE CHG:".
           03  FILLER     PIC X(15)  VALUE "32 MAINT FEES :".
           03  FILLER     PIC X(15)  VALUE "33 1ST PY EXT :".
           03  FILLER     PIC X(15)  VALUE "   INT CHARGE :".
           03  FILLER     PIC X(15)  VALUE "   AMT FINANCD:".
           03  FILLER     PIC X(15)  VALUE "   TOT REPYBLE:".
           03  FILLER     PIC X(15)  VALUE "   ODD 1ST PMT:".
           03  FILLER     PIC X(15)  VALUE "38 REG PAYMENT:".
           03  FILLER     PIC X(17)  VALUE "-----RATES-----  ".
           03  FILLER     PIC X(7)   VALUE "21 OVR:".
           03  FILLER     PIC X(7)   VALUE "22 EFF:".
           03  FILLER     PIC X(7)   VALUE "23 APR:".
           03  FILLER     PIC X(7)   VALUE "24 END:".

       01  PROMPT-LIST.
           03  FILLER          PIC X(49)  VALUE
             "PLOANAMT:S PANTIC:S INSPROMPT:S OUTPROMPT P200:S ".
           03  FILLER          PIC X(7)   VALUE
             "P250:S ".
           03  FILLER          PIC X(49)  VALUE
             "P260:S P270:S P280:S P290:S P300:S P310:S P320:S ".
           03  FILLER          PIC X(42)  VALUE
             "P330:S P340:S P350:S P360:S P370:S P380:S ".
           03  FILLER          PIC X(37)  VALUE
             "PRATES:S P210:S P220:S P230:S P240:S.".

      ********************************
      *   E L E M E N T   S P E C S  *
      ********************************
       01  MAX             PIC 99   VALUE 38.
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
      * 7  PD-1STPYDATE
           03  FILLER      PIC X(8) VALUE "ERNM060 ".
      * 8  PD-PURCD
           03  FILLER      PIC X(8) VALUE "E  A030 ".
      * 9  PD-COLLCD
           03  FILLER      PIC X(8) VALUE "E  A030 ".
      * 10 PD-SRCD
           03  FILLER      PIC X(8) VALUE "E  A030 ".
      * 11 PD-PRLNNO
           03  FILLER      PIC X(8) VALUE "ENNN060 ".
      * 12 PD-PROCEEDS
           03  FILLER      PIC X(8) VALUE "ENNN062 ".
      * 13 PD-INSURED
           03  FILLER      PIC X(8) VALUE "E  A030 ".
      * 14  PD-FEECODE(1), (2)
           03  FILLER      PIC X(8) VALUE "E  A020 ".
      * 15  PD-FEECODE(3), (4)
           03  FILLER      PIC X(8) VALUE "E  A020 ".
      * 16 PD-FEECODE(5), (6)
           03  FILLER      PIC X(8) VALUE "E  A020 ".
      * 17 PD-FEECODE(7), (8)
           03  FILLER      PIC X(8) VALUE "E  A020 ".
      * 18 PD-FEECODE(9), (10)
           03  FILLER      PIC X(8) VALUE "E  A020 ".
      * 19 PD-DLNO
           03  FILLER      PIC X(8) VALUE "ENNN042 ".
      * 20 PD-COLLID
           03  FILLER      PIC X(8) VALUE "E  A200 ".
      * 21 PD-OVRDRATE
           03  FILLER      PIC X(8) VALUE "ENNN033 ".
      * 22 PD-EFFRATE
           03  FILLER      PIC X(8) VALUE "ENNN033 ".
      * 23 PD-APRATE
           03  FILLER      PIC X(8) VALUE "ENNN033 ".
      * 24 PD-ENDORATE
           03  FILLER      PIC X(8) VALUE "ENNN033 ".
      * 25 PD-INSCOMP(1)
           03  FILLER      PIC X(8) VALUE "ENNN020 ".
      * 26 PD-INSCOMP(2)
           03  FILLER      PIC X(8) VALUE "ENNN020 ".
      * 27 PD-INSCOMP(3)
           03  FILLER      PIC X(8) VALUE "ENNN020 ".
      * 28 PD-INSCOMP(4)
           03  FILLER      PIC X(8) VALUE "ENNN020 ".
      * 29 PD-INSCOMP(5)
           03  FILLER      PIC X(8) VALUE "ENNN020 ".
      * 30 PD-INSCOMP(6)
           03  FILLER      PIC X(8) VALUE "ENNN020 ".
      * 31 PD-SERCHG
           03  FILLER      PIC X(8) VALUE "ENNN062 ".
      * 32 PD-MAINTFEE
           03  FILLER      PIC X(8) VALUE "ENNN062 ".
      * 33 PD-EXTCHG
           03  FILLER      PIC X(8) VALUE "ENNN062 ".
      * 34 PD-INTCHG
           03  FILLER      PIC X(8) VALUE "ENNN062 ".
      * 35 PD-FINAMT
           03  FILLER      PIC X(8) VALUE "ENNN062 ".
      * 36 PD-LNAMT
           03  FILLER      PIC X(8) VALUE "ENNN062 ".
      * 37 PD-1STPYAMT
           03  FILLER      PIC X(8) VALUE "ENNN062 ".
      * 38 PD-REGPYAMT
           03  FILLER      PIC X(8) VALUE "ENNN062 ".
       01  SPEC-TABLE2    REDEFINES SPEC-TABLE.
           03  SPEC-REC    OCCURS 38 TIMES.
               05  SPEC    PIC X(7).
               05  CHFG    PIC X.

       01  DATE-STAT       PIC X.
       01  PRIOR-LNAMT     PIC S9(7)V99 COMP.
       01  COUNTER         PIC 99.
       01  COMPARE-TO-HOLDERS.
           03  PROCEEDS-HLDR         PIC S9(9)V99 COMP.
           03  AMT-HOLDER            PIC S9(9)V99 COMP.
           03  MAINTFEE-HLDR         PIC S9(6)V99 COMP.
           03  TOTAL-GL-HLDR         PIC S9(5)V99 COMP.
           03  TOTAL-GL-FINCHG-HLDR  PIC S9(5)V99 COMP.
           03  INSPREM-HLDR          PIC S9(4)V99 COMP.
           03  INSTERM-HLDR          PIC 9(3)     COMP.
           03  INSOURS-HLDR          PIC X.
           03  CALC-FG-HLDR          PIC X.
           03  INSCODES-HLDR         PIC X.

       01  HOLD-AMT-EXCEEDED    PIC Z(9).99.
       01  HOLD-TERM-EXCEEDED  REDEFINES HOLD-AMT-EXCEEDED
                                PIC Z(11)9.
       01  HOLD-RATE-EXCEEDED  REDEFINES HOLD-AMT-EXCEEDED
                                PIC Z(4).9999.
       01  AMT-EXCEEDED-LIT    REDEFINES HOLD-AMT-EXCEEDED
                                PIC X(12).

       01  TYPE-OF-AMT-EXCEEDED PIC X(15).
       01  HOLD-DATE            PIC 9(8).
       01  SCREEN-LINES.
           03  SCREEN-LINE1     PIC X(20).
           03  SCREEN-LINE2     PIC X(20).
           03  SCREEN-LINE3     PIC X(20).
       01  MSG-LINES           REDEFINES SCREEN-LINES.
           03  MSG-LINE1        PIC X(30).
           03  MSG-LINE2        PIC X(30).
       01  SAVE-PD2-KEY.
           03  SAVE-PD2-KEY-ID  PIC X.
           03  SAVE-PD2-KEY-BR  PIC 9(4).
           03  SAVE-PD2-KEY-NAM PIC X(33).
       01  HOLD-BWNAME          PIC X(33).
       01  TERM-LOW             PIC 9(3)    COMP.
       01  TERM-HIGH            PIC 9(3)    COMP.
       01  PROCEEDS-LOW         PIC 9(7)V99 COMP.
       01  PROCEEDS-HIGH        PIC 9(7)V99 COMP.
       01  POINTS-LOW           PIC 9(7)V99 COMP.
       01  POINTS-HIGH          PIC 9(7)V99 COMP.
       01  ERRCD                PIC X.
       01  LOAN-QUOTE-FG        PIC X.

       01  D-DATE-YYMMDD        PIC X(6) VALUE " ".
       01  TRANS-DATE           PIC 9(8).

       COPY "LIBLP/PAYOFXW.CPY".
       COPY "LIBWI/DLSCANW.CPY".
       COPY "LIBLP/LPWKPATH.CPY".
       COPY "LIBGB/SYSTEMW.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/HASHW.CPY".
       COPY "LIBGB/GPENVW.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBLP/LPANTIW.CPY".
       COPY "LIBLP/LPWKCIPL.CPY".
       COPY "LIBLP/LPWKCEPL.CPY".
       COPY "LIBLP/LPWKCOPL.CPY".
       COPY "LIBLP/LPAMTSW.CPY".
       COPY "LIBLP/LPSPEDW.CPY".
       COPY "LIBLP/LPBAGEW.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/TSTCANIDW.CPY".
       COPY "LIBWI/LCSCANW.CPY".
       COPY "LIBWI/SCSCANW.CPY".
       COPY "LIBWI/CLSCANW.CPY".
       COPY "LIBWI/CDSCANW.CPY".
       COPY "LIBWI/CDBSCNW.CPY".
       COPY "LIBLP/INSELIGW.CPY".
       COPY "LIBGB/PASSWDW.CPY".
       78  ALLOW-ZERO-PROC-SEQ  VALUE 19.
       78  SRCD-SEQ             VALUE 20.
       78  PP-PREM-SEQ          VALUE 21.
       01  CAN-SEQ             PIC 99 VALUE 00.
       01  1STPY30-SEQ         PIC 99 VALUE 01.
       01  APPROVAL-SEQ        PIC 99 VALUE 02.
       01  APR-SEQ             PIC 99 VALUE 03.
       01  OVER-SEQ            PIC 99 VALUE 04.
       01  CLQUOT-WINDOW-HANDLE  PIC X(10).
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBWI/CLQUOT_DEF.CPY".
       COPY "LIBWI/CLQUOT_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".

       COPY "LIBGB/GETFMW.CPY".

       SCREEN SECTION.
       COPY "LIBWI/CLQUOT_SCN.CPY".

       PROCEDURE DIVISION
           USING FORM-PATHNAME EXIT-PATHNAME.
       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
               WK-FILE.
       COPY "LIBGB/DECLARE.CPY".
           PERFORM CLOSE-SP1-FILE.
           PERFORM CLOSE-LC1-FILE.
           PERFORM CLOSE-IC1-FILE.
           PERFORM CLOSE-DL1-FILE.
           PERFORM CLOSE-CL1-FILE.
           PERFORM CLOSE-CDB1-FILE.
           PERFORM CLOSE-CD1-FILE.
           PERFORM CLOSE-BW1-FILE.
           PERFORM CLOSE-BR-FILE.
           CLOSE
               WK-FILE.
       COPY "LIBGB/DECLAR2.CPY".

      ***********************************
       MAIN-PROGRAM SECTION.
      ***********************************

           PERFORM SQL-CONNECT.

       COPY "LIBGB/CHKSEC.CPY".
      D    STOP MESS.
           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "CLQUOT" TO VDU-FORM-NAME.

           MOVE " " TO VDU-WIMENU-SELECTION
                       VDU-PGMENU-SELECTION.

              DISPLAY FLOATING GRAPHICAL WINDOW
                      AT  SCREEN LINE CLQUOT-BEGIN-X * VDU-WIN-L-OFFSET
                      AT  SCREEN COL  CLQUOT-BEGIN-Y * VDU-WIN-C-OFFSET
                   CELL SIZE IS LABEL FONT
           COLOR IS VDU-WCOLOR-TCLP,CONTROL VALUE IS VDU-WCTRL-TCLP
                      SIZE    CLQUOT-COLUMNS + VDU-WIN-W-MOD
                      LINES   CLQUOT-LINES + VDU-WIN-H-MOD
                      BLANK SCREEN
                      HANDLE IS CLQUOT-WINDOW-HANDLE.

           PERFORM FORM-RESET.

           MOVE EXT-CONAME-USER-LOC TO BR-NO.
           PERFORM GET-BR.
           IF IO-FG = 9
              GO TO IO-ERROR.

           IF BR-CAN-LOANS-FG NOT = "Y" AND NOT = "O"
              MOVE "CAN LOANS NOT ACTIVE" TO MESS
              PERFORM SEND-MESS
              GO TO END-ROUTINE.

      * SETUP SYSTEM DATE:
           MOVE "SYSDATE_YYMMDD" TO GETENV-BUF.
           PERFORM GETENV-CALL.
           IF STAT = "00"
              MOVE GETENV-BUF TO D-DATE-YYMMDD
              MOVE D-DATE-YYMMDD TO DATE-YYMMDD
              PERFORM CONVERT-YYMMDD-TO-YYYYMMDD
           ELSE
              MOVE EXT-CONAME-SYSDATE TO ALP-DATE
              PERFORM CONVERT-ALPDATE-TO-YYYYMMDD.
           MOVE DATE-YYYYMMDD TO TRANS-DATE.

           MOVE " " TO ERRCD.
           MOVE "Q" TO LOAN-QUOTE-FG.

           PERFORM OPEN-CD1-FILE.
           PERFORM OPEN-IC1-FILE.

       ENTER-BUILD-KEY.

           MOVE
           "F7, F6-SCAN, ENTER THE CAN LOAN GROUP ID TO BUILD FOR:"
                                                       TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "E  A040BUILD." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1<"
              PERFORM GROUPID-SCAN
              IF IO-FG NOT = 0
                 GO TO ENTER-BUILD-KEY
              ELSE
                 MOVE "00" TO VDUSTATUS.
           IF VDUSTATUS = "1>"
              GO TO END-ROUTINE.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO ENTER-BUILD-KEY.
           MOVE VDUBUF TO TCAN-WORD.
           PERFORM TEST-CAN-GROUP.
           IF TCAN-ERR NOT = 0
              MOVE "INVALID CAN LOAN GROUP ID" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-BUILD-KEY.
           MOVE TCAN-WORD TO THE-GROUP-ID.
           PERFORM SEND-BUILD.
           IF IO-FG = 9
              GO TO ENTER-BUILD-KEY.
           IF THE-GROUP-ID NUMERIC
              MOVE THE-GROUP-ID-NUM TO THE-GROUP-REFBR
              PERFORM SEND-REFBR
              MOVE THE-GROUP-REFBR TO BR-NO
              PERFORM GET-BR
              IF IO-FG = 9
                 MOVE "BRANCH NOT ON FILE!" TO MESS
                 PERFORM SEND-MESS
                 GO TO ENTER-BUILD-KEY
              END-IF
              GO TO MAIN-MODULE.

       ENTER-REFBR-KEY.
           MOVE "F7-EXIT, ENTER A BRANCH FOR REFERENCE:" TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "ENNN040REFBR." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>"
              GO TO END-ROUTINE.
           IF VDUSTATUS = "1U"
              GO TO ENTER-BUILD-KEY.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO ENTER-REFBR-KEY.
           MOVE VDUNUM0 TO THE-GROUP-REFBR.
           PERFORM SEND-REFBR.
           IF IO-FG = 9
              GO TO ENTER-REFBR-KEY.
           IF BR-CAN-GROUP-ID NOT = THE-GROUP-ID
              STRING "THE BRANCH'S CAN LOAN GROUP ID IS "
                     BR-CAN-GROUP-ID
                     ", SELECT ANOTHER BRANCH FRO REFERENCE!"
                     DELIMITED BY SIZE INTO MESS
              PERFORM SEND-MESS
              GO TO ENTER-REFBR-KEY.

       MAIN-MODULE.

           PERFORM FORM-RESET.

           MOVE " " TO CAN-LOAN-NO.
      * CLEAR REQUIRED RE: (GA) SP-SCFRMLA = 12 OR 13
      *                    (SC) SP-SCFRMLA = 18
           MOVE 0 TO SPDUAL-LIAB.
           MOVE " " TO HOLD-BWNAME.
           MOVE " " TO LOAN-CALCULATOR-CALLED-FG.

           MOVE " " TO MESS PROMPT-FG.
           PERFORM CALL-TMMSG.

      * GET GLOBAL PARAMETER RECORD (FROM GPENV):
           PERFORM GET-GPENV.

           PERFORM ENTER-KEYS.
           IF IO-TYPE = "STA"
              GO TO ENTER-BUILD-KEY.
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
           MOVE SPACES TO IO-TYPE.

           PERFORM NEW-RECORD-SETUP.

           IF BR-CAN-LOANS-FG = "Y" OR "O"
              GO TO RECALL-LOAN.
       ENTER-KEYS-LC-CODE.
           PERFORM LOAN-CLASS-PROMPT.

           MOVE " " TO CAN-LOAN-NO.
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
                 GO TO ENTER-KEYS-LC-CODE.
           IF VDUSTATUS = "1>"
              GO TO END-ROUTINE.
           IF VDUSTATUS NOT = "00"
              GO TO ENTER-KEYS-LC-CODE.

           IF VDUNUM0 = 0
                  OR VDUNUM0 > 99
              GO TO ENTER-KEYS-LC-CODE.

           MOVE VDUNUM0 TO LC-CODE.
           PERFORM GET-LC.
           IF IO-FG = 9
              PERFORM NOT-FOUND
              GO TO ENTER-KEYS-LC-CODE.
           MOVE "S  A000LCDESC." TO VDU.
           MOVE LC-DESC TO VDUBUF.
           PERFORM SCREENIO.

           IF ( LC-SALEFIN = "Y"   )
              PERFORM NOT-DIRECT
              GO TO ENTER-KEYS-LC-CODE.

           MOVE LC-CODE TO PD-CLASS
                           PD-SPRCLASS.
           MOVE LC-SMALLOAN TO PD-SMALLOAN.
           MOVE LC-SALEFIN TO PD-SALEFIN.
           MOVE LC-SERVACCT TO PD-SERVACCT.
           MOVE LC-LOANTYPE TO PD-LOANTYPE.
           MOVE LC-GIFTNET TO PD-GIFTNET.
           MOVE 1 TO PD-STMT-COUNTER.

       ENTER-KEYS-CL-CODE.
           MOVE " F1-CLEAR, ENTER CAN LOAN NO:" TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "E  A110010." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10"
              PERFORM ALARM
              MOVE "ENT" TO IO-TYPE
              GO TO EXIT-ENTER-KEYS.
           IF VDUSTATUS NOT = "00"
              GO TO ENTER-KEYS-CL-CODE.
           IF VDUBUF = " "
              GO TO ENTER-KEYS-CL-CODE.

           MOVE VDUBUF TO CAN-LOAN-NO.

           PERFORM SET-CLFILE-INDEX-BY.
           MOVE CAN-LOAN-NO TO CL-CAN-NO.
           PERFORM OPEN-CL1-FILE.
           PERFORM READ-CL1-FILE.
           PERFORM CLOSE-CL1-FILE.
           IF IO-FG = 0
              STRING "CAN LOAN '"
                     CAN-LOAN-NO
                     "' "
                     LC-DESC
                     ", ALREADY EXISTS!!!!"
                      DELIMITED BY SIZE INTO MESS
              PERFORM SEND-MESS
              GO TO ENTER-KEYS-CL-CODE.

           GO TO EXIT-ENTER-KEYS.

      **********************
      *   RECALL ENTRY
      **********************
       RECALL-LOAN.
       RECALL-GET-KEY.
           MOVE LEGEND-02 TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "S  A110KEYTYPE:S." TO VDU.
           MOVE "CAN LOAN #:" TO VDUBUF.
           PERFORM SCREENIO.

           PERFORM NEW-RECORD-SETUP.
           MOVE " " TO CAN-LOAN-NO.
           MOVE " " TO PD2-BWNAME.
           MOVE "S  A020LNKEY." TO VDU.
           PERFORM SCREENIO.
           MOVE "E  A020LNKEY." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10"
              PERFORM ALARM
              MOVE "ENT" TO IO-TYPE
              GO TO ENTER-KEYS-LC-CODE.
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
           IF VDUSTATUS NOT = "00"
              GO TO RECALL-LOAN.
           IF VDUBUF = " "
              GO TO RECALL-LOAN.
           MOVE VDUBUF TO CAN-LOAN-NO.

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

           MOVE CL-REC TO ORIGINAL-CL-REC.

           PERFORM NEW-RECORD-SETUP.
           MOVE CL-PD-REC TO PD-REC.

           MOVE THE-GROUP-REFBR TO PD2-BRANCH PD-BRANCH
                                   PD-POSTING-BR PD-OWNBR
                                   PD-ORGBR.


           MOVE "Q"             TO PD2-ID SAVE-PD2-KEY-ID.
           MOVE THE-GROUP-REFBR TO PD2-BRANCH SAVE-PD2-KEY-BR.
           MOVE SPACE           TO PD2-BWNAME.

           MOVE " " TO PRIOR-LOAN-STAT
                       PASS-STAT-FG.

           MOVE PD-PROCEEDS TO PD-CAN-LOAN-MAX.

           MOVE PD-LOANDATE TO HOLD-DATE.
           PERFORM DISPLAY-FIELDS.

           MOVE "S  A330011." TO VDU.
           MOVE CL-DESC TO VDUBUF.
           PERFORM SCREENIO.

           PERFORM LOAN-CLASS-PROMPT.
           IF ( LC-SALEFIN = "Y"   )
              PERFORM NOT-DIRECT
              MOVE "CLR" TO IO-TYPE
              GO TO EXIT-ENTER-KEYS.

           MOVE 0 TO NEW-FLAG.
           MOVE "SN N020LNKEY." TO VDU.
           MOVE PD-CLASS TO VDUNUM0.
           PERFORM SCREENIO.

           PERFORM CHECK-SP-EFFDATE.
           IF IO-FG = 9
              MOVE "SPR IS EITHER EXPIRED OR MISSING" TO MESS
              PERFORM SEND-MESS
              GO TO EXIT-ENTER-KEYS.

           MOVE SP-LAWCODE TO PD-LAWCODE.

           IF HOLD-DATE NOT = TRANS-DATE
                     OR SP-LAWCODE NOT = PD-LAWCODE
                      OR NOT PD-ONE-MTH-NO-DAY-EXT
      * CITIZENS TEST:
              MOVE 800 TO CIP-NO-OF-TIMES-CALLED
              PERFORM CALL-LOAN-CALCULATIONS.
      *    IF PASS-LN-MORELN = "Y"
      *       MOVE "LOAN HAS SPLIT JUDGEMENTS" TO MESS
      *       PERFORM SEND-MESS.

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
           MOVE " " TO VOIDGN-FG.
           MOVE "N" TO CHFG(2) CHFG(3) CHFG(4).
           MOVE "S  A000MSEL." TO VDU.
           PERFORM SCREENIO.
           MOVE SPACES TO IO-TYPE.
           MOVE 1 TO ONE.
           MOVE FIRST-ELE TO ELE.
           MOVE 0 TO FIRST-ELE.
       CREATE-ROUTINE.
           IF NEW-FLAG NOT = 1
              GO TO MODIFY-ROUTINE.
           ADD ONE TO ELE.
           IF ELE < 1
              MOVE 1 TO ELE ONE.
           IF ELE NOT > MAX
              GO TO ENTER-ELE.
           MOVE 2 TO NEW-FLAG.
           PERFORM SETUP-CALC-AREA.
           PERFORM CALL-LOAN-CALCULATIONS.
       MODIFY-ROUTINE.
           MOVE " " TO STAT.
           PERFORM UNPROTECT-FIELDS.
           IF QUOTE-ONLY
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
           IF VDUSTATUS NOT = "00"
              GO TO MODIFY-ROUTINE.
           MOVE VDUBUF TO ACTION.

           IF ACTION = "     S"
              IF SP-MFFRMLA = 5
                 IF NOT COP-REG-PAYMENT-IS-INPUT
                    MOVE "PAYMENT MUST BE INPUT - (MFFRMLA=5)" TO MESS
                    PERFORM SEND-MESS
                    GO TO MODIFY-ROUTINE
                 END-IF
              END-IF
              PERFORM VERIFY-WRITE
              IF IO-FG NOT = 0
                 GO TO MODIFY-ROUTINE
              ELSE
                 PERFORM WRITE-MODULE
                 MOVE "CLR" TO IO-TYPE
                 GO TO END-ENTRY-MODULE.
           IF ACTION = "DELETE"
              PERFORM VERIFY-DELETE
              IF IO-FG = 9
                 MOVE "Y" TO VOIDGN-FG
                 GO TO MODIFY-ROUTINE
              ELSE
                 IF IO-FG NOT = 0
                    GO TO MODIFY-ROUTINE
                 ELSE
                    PERFORM DELETE-MODULE
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
           IF ELE NOT = 18
              MOVE "Y" TO DEFAULT-APR-FG.

       ENTER-ELE.
           IF ELE = 1
              GO TO F-01.
           IF ELE = 5
              IF NEW-FLAG NOT = 1
                 GO TO CREATE-ROUTINE
              ELSE
                 GO TO F-052.
           IF ELE = 6 OR 7
              IF PD-GIFTNET = "Y"
                 MOVE " " TO CHFG(6) CHFG(7)
              ELSE
                 IF ELE = 6
                    IF CHFG(6) = "N"
                       MOVE "S  A000060." TO VDU
                       MOVE TRANS-DATE TO PD-LOANDATE DATE-YYYYMMDD
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
                             GO TO CREATE-ROUTINE
                    ELSE
                       NEXT SENTENCE
                 ELSE
                    IF ELE = 7
                       IF CHFG(7) = "N"
                          MOVE " " TO CHFG(7)
                          GO TO CONT-F-07.
           IF ELE < 5
              MOVE "F1, ENTER 2 CHARC CAN LOAN #:" TO LEGEND
              PERFORM SEND-LEGEND.
           IF ELE = 5
              MOVE "ENTER THE STATE OF ORIGINATION:" TO LEGEND
              PERFORM SEND-LEGEND.
           IF ELE = 6
              MOVE "ENTER THE DATE OF THE LOAN:" TO LEGEND
              PERFORM SEND-LEGEND.
           IF ELE = 7
              MOVE "ENTER FIRST PAYMENT DUE DATE:" TO LEGEND
              PERFORM SEND-LEGEND.
           IF ELE = 8
              MOVE "F6-PURPOSE SCAN, ENTER PURPOSE CODE:" TO LEGEND
              PERFORM SEND-LEGEND.
           IF ELE = 9
              MOVE
              "F6-COLLATERAL SCAN, ENTER COLLATERAL CODE:" TO LEGEND
              PERFORM SEND-LEGEND.
           IF ELE = 10
              MOVE "F6-SOURCE SCAN, ENTER SOURCE CODE:" TO LEGEND
              PERFORM SEND-LEGEND.
           IF ELE = 11
              MOVE "N" TO CHFG(11).
           IF ELE = 12
              MOVE "F1, ENTER LOAN PROCEEDS:" TO LEGEND
              PERFORM SEND-LEGEND.
           IF ELE = 13
              MOVE "ENTER INSURANCE CODES (EX. MGG):" TO LEGEND
              PERFORM SEND-LEGEND.
           IF ELE > 13 AND ELE < 19
              IF CHFG(14) NOT = "N"
                 MOVE "F1, F6-FEE CODE SCAN, ENTER CODE, <ENTER>=NONE:"
                                                            TO LEGEND
                 PERFORM SEND-LEGEND.
           IF ELE > 13 AND ELE < 19
              IF NOT QUOTE-ONLY
                 MOVE " " TO CHFG(14) CHFG(15) CHFG(16)
                             CHFG(17) CHFG(18).
           IF ELE > 13 AND ELE < 19
              IF PD-GIFTNET = "Y"
                 MOVE " " TO CHFG(14) CHFG(15) CHFG(16)
                                      CHFG(17) CHFG(18).
           IF ELE = 19
              IF CHFG(19) = "N"
                 PERFORM RESET-LEGEND
              ELSE
                 MOVE "F1, F6-DEALER SCAN, ENTER DEALER NO:" TO LEGEND
                 PERFORM SEND-LEGEND.
           IF NEW-FLAG = 1
              IF ELE > 24 AND ELE < 31
                 GO TO CREATE-ROUTINE.
           IF ELE = 29
              IF SP-DEFAULT-O2-INS-COMP NUMERIC  AND
                  SP-DEFAULT-O2-INS-COMP NOT = 0
                 MOVE "SN N020290." TO VDU
                 MOVE SP-DEFAULT-O2-INS-COMP TO VDUNUM0
                 PERFORM SCREENIO.
           IF ELE = 30
              IF GP-DEFAULT-O3-INS-COMPANY NOT = 0
                 MOVE "SN N020300." TO VDU
                 MOVE GP-DEFAULT-O3-INS-COMPANY TO VDUNUM0
                 PERFORM SCREENIO.
           IF ELE = 31
              IF SP-SCFRMLA = 0
                 GO TO CREATE-ROUTINE.
           IF ELE = 32
              IF SP-MFFRMLA = 0
                 GO TO CREATE-ROUTINE.
           IF ELE = 33
              IF SP-PYEXFRMLA = 0
                 GO TO CREATE-ROUTINE.

      *    - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    WHEN A BRACH IS FLAGED FOR INS. SALES TAX
      *       VIA BR-FORCE-VALID-CITY-COUNTY = "Y"
      *    ONLY 4 FEE CODES ARE AVAILABLE FOR THE BRANCH
      *    UNLESS NO INSURANCE WRITTEN PER SPR
      *    - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF BR-FORCE-VALID-CITY-COUNTY = "Y"
             IF SP-INSURANCE-WRITTEN NOT = "N"
               IF ELE = 16 OR 17 OR 18
                 GO TO CREATE-ROUTINE.

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
       TEST-UP.
           IF VDUSTATUS NOT = "1U"
              GO TO TEST-DN.
           IF ELE = 7
              MOVE " " TO DATE-STAT.
           IF ELE = 20
              MOVE " " TO CHFG(19).
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
           MOVE 1 TO ONE.
           GO TO ENTER-ELE.
       TEST-F6.
           IF VDUSTATUS NOT = "1<"
              GO TO TEST-00.
           IF (ELE = 8 OR 9 OR 10)
                        OR (ELE > 13 AND ELE < 19)
               MOVE 0 TO CDCODE-FEE-FG
               PERFORM CODE-SCAN
               IF IO-FG = 0
                  GO TO GOTO-FIELD
               ELSE
                  GO TO ENTER-ELE.
           IF ELE = 19
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
                 F-31 F-32 F-33 F-34 F-35 F-36 F-37 F-38
                 DEPENDING ON OVER-20.
           GO TO END-ENTRY-MODULE.

      *************************************
      *    S E T   E L E M E N T S        *
      *************************************
       F-01.
           PERFORM BORROWER-QUOTED.
           IF IO-FG = 9
              IF IO-TYPE = "CLR"
                 GO TO END-ENTRY-MODULE
              ELSE
                 GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       F-02.
           GO TO CREATE-ROUTINE.
       F-03.
           GO TO CREATE-ROUTINE.
       F-04.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-05.
           MOVE VDUBUF TO PD-ORGST
                          PD-OWNST.

      *-----------------------------------------------------------------
       F-052.
           MOVE " F6-SCAN, ENTER SUB CLASS:" TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "ENNN020052." TO VDU.
           PERFORM SCREENIO.

           IF ( F6-KEY ) PERFORM SUBCLASS-SCAN
                         IF ( IO-FG NOT = ZEROES )
                            GO TO F-052.
           IF UP-KEY MOVE SPACES TO IO-TYPE
                     GO TO END-ENTRY-MODULE.

           IF NOT ( DOWN-KEY OR ENTER-KEY ) GO TO F-052.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE PD-ORGST TO SP-ORGST.
           MOVE PD-SPRCLASS TO SP-SPRCLASS.
           MOVE VDUNUM0 TO PD-SUBCLASS SP-SUBCLASS.
           MOVE 0 TO SP-LAWCODE.
           PERFORM OPEN-SP1-FILE.
           PERFORM START-SP1-FILE.
       F-052-NEXT-SP.
           PERFORM READ-SP1-FILE-NEXT.
           IF NOT (SP-ORGST = PD-ORGST
                     AND SP-SPRCLASS = PD-SPRCLASS
                       AND SP-SUBCLASS = PD-SUBCLASS)
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

           MOVE "S  A015053." TO VDU.
           MOVE SP-DESC       TO VDUBUF.
           PERFORM SCREENIO.

           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-06.
           MOVE VDU-DATE-YYYYMMDD TO PD-LOANDATE.
           IF VDUNUM0 = 0
              GO TO ENTER-ELE.

           PERFORM CHECK-SP-EFFDATE.
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
              MOVE PD-LOANDATE TO PD-INTDATE DATE-YYYYMMDD
              PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
              MOVE DATE-MMDDYY TO VDU-DATE-EDIT
              PERFORM SCREENIO.
           IF PD-1STPYDATE NOT = 0
               PERFORM SPTEST-EXTDATE-VALID
               IF LIMIT-ERROR = "Y"
                 GO TO ENTER-ELE
              ELSE
                 GO TO CONT1-F-06.
           MOVE PD-INTDATE TO NDTE-DATE
                              SYS-DATE.
           MOVE 1 TO NDTE-HOLD.
           PERFORM INCREMENT-MONTHS.

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

           MOVE "S  A000070." TO VDU.
           MOVE NDTE-DATE TO PD-1STPYDATE PD-ORIG-1STPYDATE
                             DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO VDU-DATE-EDIT.
           PERFORM SCREENIO.

           PERFORM SPTEST-EXTDATE-VALID.
           IF LIMIT-ERROR = "Y"
              GO TO ENTER-ELE.
           IF DATE-STAT = "N"
              IF NEW-FLAG NOT = 1
      * CITIZENS TEST
                 MOVE 800 TO CIP-NO-OF-TIMES-CALLED
      * A90?     MOVE 0 TO CIP-NO-OF-TIMES-CALLED
                 MOVE PD-LOANDATE  TO CIP-LN-DT
                 MOVE PD-INTDATE   TO CIP-IN-DT
                 MOVE PD-1STPYDATE TO CIP-PMT-DT
                 PERFORM CALL-LOAN-CALCULATIONS.
           IF DATE-STAT = "N"
              GO TO CREATE-ROUTINE.
       CONT1-F-06.
           MOVE "ENTER THE INTEREST DATE OF THE LOAN:" TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "ERNM060061." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO CONT1-F-06.
           IF VDUNUM0 = 0
              GO TO CONT1-F-06.
           MOVE VDU-DATE-YYYYMMDD TO PD-INTDATE.

           IF PD-INTDATE < PD-LOANDATE
              MOVE "MUST NOT BE PRIOR TO LOAN DATE" TO LEGEND
              PERFORM SEND-MESS
              GO TO CONT1-F-06.

            PERFORM SPTEST-EXTDATE-VALID.
            IF LIMIT-ERROR = "Y"
              GO TO CONT1-F-06.

           IF NEW-FLAG NOT = 1
      * CITIZENS TEST
              MOVE 800 TO CIP-NO-OF-TIMES-CALLED
              MOVE PD-LOANDATE TO CIP-LN-DT
              MOVE PD-INTDATE  TO CIP-IN-DT
              PERFORM CALL-LOAN-CALCULATIONS.
           GO TO CREATE-ROUTINE.
       F-07.
           MOVE VDU-DATE-YYYYMMDD TO PD-ORIG-1STPYDATE
                                     PD-1STPYDATE.
           IF VDUNUM0 = 0
              GO TO ENTER-ELE.

           PERFORM SPTEST-EXTDATE-VALID.
           IF LIMIT-ERROR = "Y"
              GO TO ENTER-ELE.

           IF PD-1STPYDATE NOT > PD-INTDATE
              MOVE "FIRST PAY MUST AFTER THE INTEREST DATE" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
       CONT-F-07.
           MOVE "ENTER THE TERM OF THE LOAN:" TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "ENNN030071." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO CONT-F-07.
           IF VDUNUM0 = 0
              GO TO CONT-F-07.
           IF VDUNUM0 = 1
              MOVE
              "CAN'T ENTER SINGLE PAY LOAN CANS!!!!" TO MESS
              PERFORM SEND-MESS
              GO TO CONT-F-07.
           MOVE VDUNUM0 TO PD-ORGTERM PD-ANTICTERM PD-SUM-ORGTERM.
           PERFORM SP-EDT-TABLE-LOOKUP.
           IF IO-FG = 9
              MOVE 0 TO IO-FG
              GO TO CONT-F-07.
           IF NEW-FLAG NOT = 1
      * CITIZENS TEST
              MOVE 800 TO CIP-NO-OF-TIMES-CALLED
              MOVE PD-1STPYDATE TO CIP-PMT-DT
              MOVE PD-ORGTERM   TO CIP-TERM
              PERFORM CALL-LOAN-CALCULATIONS.
           GO TO CREATE-ROUTINE.
       F-08.
           MOVE "PU"   TO CD-TYPE.
           MOVE VDUBUF TO CD-CODE PD-PURCD.
           PERFORM SEND-CD-DESC.
           IF IO-FG = 9
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       F-09.
           MOVE "CO"   TO CD-TYPE.
           MOVE VDUBUF TO CD-CODE PD-COLLCD.
           PERFORM SEND-CD-DESC.
           IF IO-FG = 9
              GO TO ENTER-ELE.
           MOVE CD-CO-SECURED TO SPTEST-CD-SECURED.
           GO TO CREATE-ROUTINE.
       F-10.
           IF VDUBUF = "PB" OR "RN" OR "SC"
                          OR "RB" OR "RO"
              GO TO ENTER-ELE.
           MOVE "SR"   TO CD-TYPE.
           MOVE VDUBUF TO CD-CODE PD-SRCD.
           PERFORM SEND-CD-DESC.
           IF IO-FG = 9
              GO TO ENTER-ELE.

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

           MOVE PD-SRCD TO CIP-SOURCE-CD.
           GO TO CREATE-ROUTINE.
       F-11.
           GO TO CREATE-ROUTINE.

       F-12.
      *    MOVE PD-PROCEEDS TO AMT-HOLDER.
      * NEEDS ITS OWN HOLDER BECAUSE ITS RECALC'D IN
      *   SP-EDT-TABLE-LOOKUP
           MOVE PD-PROCEEDS TO PROCEEDS-HLDR.
           MOVE VDUNUM2 TO PD-PROCEEDS.
           IF PD-GIFTNET = "Y"
              IF PD-PROCEEDS NOT = 0
                 GO TO ENTER-ELE
              ELSE
                 NEXT SENTENCE
           ELSE
           IF NOT (PD-SRCD = "RN" OR "SC" OR "RB" OR "RO")
              IF PD-PROCEEDS = 0
                 IF SP-ALLOW-ZERO-PROC = "Y" AND NOT (PD-SRCD = "PB")
                    MOVE ALLOW-ZERO-PROC-SEQ TO PASSWDW-SEQ
                    MOVE "TO ENTER ZERO PROCEEDS" TO PASSWDW-PROMPT2
                    PERFORM VERIFY-PASSWORD
                    IF PASSWDW-STAT NOT = "OK"
                       GO TO ENTER-ELE
                    END-IF
                 ELSE
                     GO TO ENTER-ELE.

           PERFORM SP-EDT-TABLE-LOOKUP.
           IF IO-FG = 9
              MOVE 0 TO IO-FG
              GO TO ENTER-ELE.
           PERFORM VERIFY-PROCEEDS.
           IF IO-FG = 9
              MOVE 0 TO IO-FG
              GO TO ENTER-ELE.
           IF PD-PROCEEDS = PROCEEDS-HLDR
                      OR NEW-FLAG = 1
              MOVE "I" TO PD-PROCEEDS-FG
              GO TO CREATE-ROUTINE.
       CONT-F-12.
           MOVE "S  A010121." TO VDU.
           MOVE "I" TO VDUBUF PD-PROCEEDS-FG.
           PERFORM SCREENIO.

           MOVE "E  A010121." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO CONT-F-12.
           MOVE VDUBUF TO PD-PROCEEDS-FG.
           IF NOT (PD-PROCEEDS-FG = "I" OR "C")
              PERFORM ICE-OR-A-MSG
              GO TO CONT-F-12.
           IF NEW-FLAG NOT = 1
      *  CITIZENS TEST
              MOVE 800 TO CIP-NO-OF-TIMES-CALLED
      *       MOVE 0 TO CIP-NO-OF-TIMES-CALLED
              MOVE PD-PROCEEDS TO CIP-PROCEEDS
              MOVE PD-PROCEEDS-FG TO CIP-PROCEEDS-SW
              PERFORM CALL-LOAN-CALCULATIONS.
           GO TO CREATE-ROUTINE.
       F-13.
           MOVE PD-INSCODES TO INSCODES-HLDR.
           MOVE " " TO CHFG(25) CHFG(26) CHFG(27)
                       CHFG(28) CHFG(29) CHFG(30).
           MOVE VDUBUF TO PD-INSCODES.

           IF SP-INSURANCE-WRITTEN = "N"
              IF PD-INSURED NOT = "N"
                 IF PD-INSTYPES NOT = "N"
                   MOVE "INSURANCE NOT ALLOWED PER SPR" TO MESS
                   PERFORM SEND-MESS
                   GO TO ENTER-ELE.


           IF NOT (PD-INSURED = "M" OR "J" OR "N" OR "S")
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

           IF CHFG(25) = "N"
              MOVE 0 TO PD-INSCOMP(1) PD-INSTRM(1) PD-INSCOVR(1)
                        PD-INSPREM(1)
              MOVE SPACES TO PD-INSOURS(1)
              MOVE "250 251 252 253 254 255." TO MESS
              PERFORM RESET-MESS.
           IF CHFG(26) = "N"
              MOVE 0 TO PD-INSCOMP(2) PD-INSTRM(2) PD-INSCOVR(2)
                        PD-INSPREM(2)
              MOVE SPACES TO PD-INSOURS(2)
              MOVE "260 261 262 263 264 265." TO MESS
              PERFORM RESET-MESS.
           IF CHFG(27) = "N"
              MOVE 0 TO PD-INSCOMP(3) PD-INSTRM(3) PD-INSCOVR(3)
                        PD-INSPREM(3)
              MOVE SPACES TO PD-INSOURS(3)
              MOVE "270 271 272 273 274 275." TO MESS
              PERFORM RESET-MESS.
           IF CHFG(28) = "N"
              MOVE 0 TO PD-INSCOMP(4) PD-INSTRM(4) PD-INSCOVR(4)
                        PD-INSPREM(4)
              MOVE SPACES TO PD-INSOURS(4)
              MOVE "280 281 282 283 284 285." TO MESS
              PERFORM RESET-MESS.
           IF CHFG(29) = "N"
              MOVE 0 TO PD-INSCOMP(5) PD-INSTRM(5) PD-INSCOVR(5)
                        PD-INSPREM(5) PD-INSEXP-DATE(5)
              MOVE SPACES TO PD-INSOURS(5)
              MOVE "290 291 292 293 294." TO MESS
              PERFORM RESET-MESS.
           IF CHFG(30) = "N"
              MOVE 0 TO PD-INSCOMP(6) PD-INSTRM(6) PD-INSCOVR(6)
                        PD-INSPREM(6) PD-INSEXP-DATE(6)
              MOVE SPACES TO PD-INSOURS(6)
              MOVE "300 301 302 303 304." TO MESS
              PERFORM RESET-MESS.
           IF NOT SP-SCFRMLA-POINTS
              IF NEW-FLAG = 1
                 MOVE "I" TO PD-POINTS-FG
                 GO TO CREATE-ROUTINE
              ELSE
                 GO TO CONT2-F-13.
       CONT1-F-13.
           MOVE "ENTER NO OF POINTS (99.99):" TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "ENNN022131." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO CONT1-F-13.
           MOVE VDUNUM2 TO PD-POINTS.
           PERFORM SP-EDT-TABLE-LOOKUP.
           IF IO-FG = 9
              MOVE 0 TO IO-FG
              GO TO CONT1-F-13.
           IF NEW-FLAG = 1
           IF PD-POINTS = 0
              MOVE "S  A050131." TO VDU
              PERFORM SCREENIO
              IF NEW-FLAG = 1
                 MOVE "I" TO PD-POINTS-FG
                 GO TO CREATE-ROUTINE.
       CONT2-F-13.
           MOVE "S  A010132." TO VDU.
           MOVE "I" TO VDUBUF PD-POINTS-FG.
           PERFORM SCREENIO.

           MOVE "E  A010132." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "1U"
              IF SP-SCFRMLA-POINTS
                 GO TO CONT1-F-13.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO CONT2-F-13.
           MOVE VDUBUF TO PD-POINTS-FG.
           IF NOT (PD-POINTS-FG = "I" OR "C")
              PERFORM ICE-OR-A-MSG
              GO TO CONT2-F-13.
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
       F-14.
           MOVE 1 TO I-LEVEL.
           PERFORM FEE-ENTRY.
           IF IO-FG = 9
              GO TO ENTER-ELE.
           IF PD-FEECODE(1) = SPACES
              GO TO CREATE-ROUTINE.
       F-142.
           MOVE "F6-FEE SCAN, ENTER FEE CODE, <ENTER> = NONE:"
                                                        TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "E  A020142." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE.
           IF VDUSTATUS = "1<"
              MOVE 1 TO CDCODE-FEE-FG
              PERFORM CODE-SCAN
              MOVE "00" TO VDUSTATUS
              IF IO-FG NOT = 0
                 GO TO F-142.
           IF VDUSTATUS NOT = "00"
              GO TO F-142.
           MOVE 2 TO I-LEVEL.
           PERFORM FEE-ENTRY.
           IF IO-FG = 9
              GO TO F-142.
           GO TO CREATE-ROUTINE.
       F-15.
           MOVE 3 TO I-LEVEL.
           PERFORM FEE-ENTRY.
           IF IO-FG = 9
              GO TO ENTER-ELE.
           IF PD-FEECODE(3) = SPACES
              GO TO CREATE-ROUTINE.
       F-152.
           MOVE "F6-FEE SCAN, ENTER FEE CODE, <ENTER> = NONE:"
                                                        TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "E  A020152." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE.
           IF VDUSTATUS = "1<"
              MOVE 1 TO CDCODE-FEE-FG
              PERFORM CODE-SCAN
              MOVE "00" TO VDUSTATUS
              IF IO-FG NOT = 0
                 GO TO F-152.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO F-152.
           MOVE 4 TO I-LEVEL.
           PERFORM FEE-ENTRY.
           IF IO-FG = 9
              GO TO F-152.
           GO TO CREATE-ROUTINE.
       F-16.
           MOVE 5 TO I-LEVEL.
           PERFORM FEE-ENTRY.
           IF IO-FG = 9
              GO TO ENTER-ELE.
           IF PD-FEECODE(5) = SPACES
              GO TO CREATE-ROUTINE.
       F-162.
           MOVE "F6-FEE SCAN, ENTER FEE CODE, <ENTER> = NONE:"
                                                        TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "E  A020162." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE.
           IF VDUSTATUS = "1<"
              MOVE 1 TO CDCODE-FEE-FG
              PERFORM CODE-SCAN
              MOVE "00" TO VDUSTATUS
              IF IO-FG NOT = 0
                 GO TO F-162.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO F-162.
           MOVE 6 TO I-LEVEL.
           PERFORM FEE-ENTRY.
           IF IO-FG = 9
              GO TO F-162.
           GO TO CREATE-ROUTINE.
       F-17.
           MOVE 7 TO I-LEVEL.
           PERFORM FEE-ENTRY.
           IF IO-FG = 9
              GO TO ENTER-ELE.
           IF PD-FEECODE(7) = SPACES
              GO TO CREATE-ROUTINE.
       F-172.
           MOVE "F6-FEE SCAN, ENTER FEE CODE, <ENTER> = NONE:"
                                                        TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "E  A020172." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE.
           IF VDUSTATUS = "1<"
              MOVE 1 TO CDCODE-FEE-FG
              PERFORM CODE-SCAN
              MOVE "00" TO VDUSTATUS
              IF IO-FG NOT = 0
                 GO TO F-172.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO F-172.
           MOVE 8 TO I-LEVEL.
           PERFORM FEE-ENTRY.
           IF IO-FG = 9
              GO TO F-172.
           GO TO CREATE-ROUTINE.
       F-18.
           MOVE 9 TO I-LEVEL.
           PERFORM FEE-ENTRY.
           IF IO-FG = 9
              GO TO ENTER-ELE.
           IF PD-FEECODE(9) = SPACES
              GO TO CREATE-ROUTINE.
       F-182.
           MOVE "F6-FEE SCAN, ENTER FEE CODE, <ENTER> = NONE:"
                                                        TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "E  A020182." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE.
           IF VDUSTATUS = "1<"
              MOVE 1 TO CDCODE-FEE-FG
              PERFORM CODE-SCAN
              MOVE "00" TO VDUSTATUS
              IF IO-FG NOT = 0
                 GO TO F-182.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO F-182.
           MOVE 10 TO I-LEVEL.
           PERFORM FEE-ENTRY.
           IF IO-FG = 9
              GO TO F-182.
           GO TO CREATE-ROUTINE.
       F-19.
           MOVE VDUNUM2 TO PD-DLNO DL-DLNO.
           PERFORM GET-DL1.
           MOVE "S  A150191." TO VDU.
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
           GO TO CREATE-ROUTINE.
       F-20.
           MOVE VDUBUF TO PD-COLLID.
           IF PD-COLLID = " "
              MOVE SPACES TO WK-HHGS-1
              MOVE "200 201 202." TO MESS
              PERFORM RESET-MESS
              GO TO CREATE-ROUTINE.
       CONT1-F-20.
           MOVE "E  A200201." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO CONT1-F-20.
           MOVE VDUBUF TO SCREEN-LINE1 WK-HHGS-1.
           IF SCREEN-LINE1 = " "
              MOVE "201 202." TO MESS
              PERFORM RESET-MESS
              GO TO CREATE-ROUTINE.
       CONT2-F-20.
           MOVE "E  A200202." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO CONT1-F-20.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO CONT2-F-20.
           MOVE VDUBUF TO SCREEN-LINE2.
           IF SCREEN-LINE2 = " "
              MOVE "202." TO MESS
              PERFORM RESET-MESS
              GO TO F-20-EXIT.
           STRING SCREEN-LINE1,SCREEN-LINE2
                  DELIMITED BY "   " INTO WK-HHGS-1.
       F-20-EXIT.
           MOVE MSG-LINE1 TO PD-MSG1.
           MOVE MSG-LINE2 TO PD-MSG2.
           GO TO CREATE-ROUTINE.
       F-21.
           MOVE VDUNUM3 TO PD-OVRDRATE.
       CONT-F-21.
           MOVE "E  A010211." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO CONT-F-21.
           MOVE VDUBUF TO PD-OVRDRATE-FG.
           IF NOT (PD-OVRDRATE-FG = "C" OR "E" OR "A")
              PERFORM ICE-OR-A-MSG
              GO TO CONT-F-21.

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
       F-22.
           MOVE VDUNUM3 TO PD-EFFRATE.
           GO TO CREATE-ROUTINE.
       F-23.
           MOVE VDUNUM3 TO PD-APRATE.
           GO TO CREATE-ROUTINE.
       F-24.
           MOVE VDUNUM3 TO PD-ENDORATE.
           GO TO CREATE-ROUTINE.
       F-25.
           PERFORM INSURANCE-ENTRY.
           IF IO-FG = 9
              MOVE 0 TO IO-FG
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       F-26.
           PERFORM INSURANCE-ENTRY.
           IF IO-FG = 9
              MOVE 0 TO IO-FG
              GO TO ENTER-ELE.
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
           MOVE PD-SERCHG TO AMT-HOLDER.
           MOVE VDUNUM2 TO PD-SERCHG PD-SUM-SERCHG.
           PERFORM VALIDATE-FIELDS-AGAINST-MAX.
           IF IO-FG = 9
              MOVE 0 TO IO-FG
              GO TO ENTER-ELE.
           IF PD-SERCHG = AMT-HOLDER
              GO TO CREATE-ROUTINE.
       CONT-F-31.
           MOVE "S  A010311." TO VDU.
           MOVE "I" TO VDUBUF PD-SERCHG-FG.
           PERFORM SCREENIO.

           MOVE "E  A010311." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO CONT-F-31.
           MOVE VDUBUF TO PD-SERCHG-FG.
           IF NOT (PD-SERCHG-FG = "I" OR "C")
              PERFORM ICE-OR-A-MSG
              GO TO CONT-F-31.
           IF NEW-FLAG NOT = 1
              MOVE PD-SERCHG TO CIP-SERVICE-CHG
              MOVE PD-SERCHG-FG TO CIP-SERVICE-CHG-SW
              PERFORM CALL-LOAN-CALCULATIONS.
           GO TO CREATE-ROUTINE.
       F-32.
           DIVIDE VDUNUM2 BY PD-ORGTERM GIVING PD-MAINTFEE ROUNDED.
           COMPUTE PD-SUM-MAINTFEE = PD-MAINTFEE * PD-ORGTERM.
           PERFORM VALIDATE-FIELDS-AGAINST-MAX.
           IF IO-FG = 9
              MOVE 0 TO IO-FG
              GO TO ENTER-ELE.
           IF VDUNUM2 = MAINTFEE-HLDR
              GO TO CREATE-ROUTINE.
       CONT-F-32.
           MOVE "S  A010321." TO VDU.
           MOVE "I" TO VDUBUF PD-MAINTFEE-FG.
           PERFORM SCREENIO.

           MOVE "E  A010321." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO CONT-F-32.
           MOVE VDUBUF TO PD-MAINTFEE-FG.
           IF NOT (PD-MAINTFEE-FG = "I" OR "C")
              PERFORM ICE-OR-A-MSG
              GO TO CONT-F-32.
           IF NEW-FLAG NOT = 1
              MULTIPLY PD-MAINTFEE BY PD-ORGTERM
                               GIVING CIP-MAINT-FEES ROUNDED.
              MOVE PD-MAINTFEE-FG TO CIP-MAINT-FEES-SW
              PERFORM CALL-LOAN-CALCULATIONS.
           GO TO CREATE-ROUTINE.
       F-33.
           MOVE PD-EXTCHG TO AMT-HOLDER.
           MOVE VDUNUM2 TO PD-EXTCHG PD-EXTCHG.
           PERFORM VALIDATE-FIELDS-AGAINST-MAX.
           IF IO-FG = 9
              MOVE 0 TO IO-FG
              GO TO ENTER-ELE.
           IF PD-EXTCHG = AMT-HOLDER
              GO TO CREATE-ROUTINE.
       CONT-F-33.
           MOVE "S  A010331." TO VDU.
           MOVE "I" TO VDUBUF PD-EXTCHG-FG.
           PERFORM SCREENIO.

           MOVE "E  A010331." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO CONT-F-33.
           MOVE VDUBUF TO PD-EXTCHG-FG.
           IF NOT (PD-EXTCHG-FG = "I" OR "C")
              PERFORM ICE-OR-A-MSG
              GO TO CONT-F-33.
           IF NEW-FLAG NOT = 1
              MOVE PD-EXTCHG TO CIP-1ST-PY-EXT
              MOVE PD-EXTCHG-FG TO CIP-1ST-PY-EXT-SW
              PERFORM CALL-LOAN-CALCULATIONS.
           GO TO CREATE-ROUTINE.
       F-34.
           GO TO CREATE-ROUTINE.
       F-35.
           GO TO CREATE-ROUTINE.
       F-36.
           GO TO CREATE-ROUTINE.
       F-37.
           GO TO CREATE-ROUTINE.
       F-38.
           MOVE PD-REGPYAMT TO AMT-HOLDER.
           MOVE VDUNUM2 TO PD-REGPYAMT PD-SUM-REGPYAMT.
           IF IO-FG = 9
              GO TO ENTER-ELE.
           IF VDUNUM2 = 0
              GO TO ENTER-ELE.
           IF PD-REGPYAMT = AMT-HOLDER
              GO TO CREATE-ROUTINE.
       CONT-F-38.
           MOVE "S  A010381." TO VDU.
           MOVE "I" TO VDUBUF PD-REGPYAMT-FG.
           PERFORM SCREENIO.

           MOVE "E  A010381." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO CONT-F-38.
           MOVE VDUBUF TO PD-REGPYAMT-FG.
           IF NOT (PD-REGPYAMT-FG = "I" OR "C")
              PERFORM ICE-OR-A-MSG
              GO TO CONT-F-38.
           IF NEW-FLAG NOT = 1
      *  CITIZENS TEST
              MOVE 800 TO CIP-NO-OF-TIMES-CALLED
      *       MOVE 0 TO CIP-NO-OF-TIMES-CALLED
              MOVE PD-REGPYAMT TO CIP-REG-PAYMENT
              MOVE PD-REGPYAMT-FG TO CIP-REG-PAYMENT-SW
              PERFORM CALL-LOAN-CALCULATIONS.
           GO TO CREATE-ROUTINE.
       END-ENTRY-MODULE.
           EXIT.

      *************************************************
       SUBCLASS-SCAN SECTION.
      *************************************************
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

      ****************************************
       FEECODE-SCAN SECTION.
      ****************************************
           MOVE "GC"            TO CDBSCN-TYPE.
           MOVE THE-GROUP-REFBR TO CDBSCN-BRANCH.
           MOVE SPACES          TO CDBSCN-CODE.

           MOVE "WI/CDBSCN" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH CDBSCN-BUF EXIT-PATHNAME.
           CANCEL FORM-PROGX.
           IF ( CDBSCN-STATUS NOT = "00" ) OR
              ((CDBSCN-TYPE = "GC") AND (CDBSCN-CODE = SPACES))
              MOVE 9 TO IO-FG
              GO TO EXIT-FEECODE-SCAN.

           MOVE "S  A020  2." TO VDU.
           MOVE ELE TO VDUROW.
           MOVE CDBSCN-CODE TO VDUBUF.
           PERFORM SCREENIO.

           MOVE CDBSCN-CODE TO VDUBUF.
           MOVE 0 TO IO-FG.

       EXIT-FEECODE-SCAN.
           EXIT.

      ****************************************
       CODE-SCAN SECTION.
      ****************************************

           MOVE SPACES TO CDSCAN-CODE.

           IF ( ELE = 8 )
              MOVE "PU"   TO CDSCAN-TYPE
           ELSE
           IF ( ELE = 9 )
              MOVE "CO"   TO CDSCAN-TYPE
           ELSE
           IF ( ELE = 10 )
              MOVE "SR"   TO CDSCAN-TYPE
           ELSE
              PERFORM FEECODE-SCAN
              GO TO EXIT-CODE-SCAN.

           MOVE "WI/CDSCAN" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH CDSCAN-BUF EXIT-PATHNAME.
           CANCEL FORM-PROGX.
           IF ( CDSCAN-STATUS    NOT = "00"    ) OR
              ((CDSCAN-TYPE = "PU") AND (CDSCAN-CODE = SPACES)) OR
              ((CDSCAN-TYPE = "CO") AND (CDSCAN-CODE = SPACES)) OR
              ((CDSCAN-TYPE = "SR") AND (CDSCAN-CODE = SPACES)) 
               MOVE 9 TO IO-FG
               GO TO EXIT-CODE-SCAN.

           MOVE "S  A030  0." TO VDU.
           MOVE ELE TO VDUROW.
           MOVE CDSCAN-CODE TO VDUBUF.
           PERFORM SCREENIO.

           MOVE CDSCAN-CODE TO VDUBUF.
           MOVE 0 TO IO-FG.

       EXIT-CODE-SCAN.
           EXIT.

      *************************************************
       GROUPID-SCAN SECTION.
      *************************************************

           MOVE "CL"   TO CDSCAN-TYPE.
           MOVE SPACES TO CDSCAN-CODE.

           MOVE "WI/CDSCAN" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH CDSCAN-BUF EXIT-PATHNAME.
           CANCEL FORM-PROGX.
           IF CDSCAN-STATUS NOT = "00" OR CDSCAN-CODE = SPACES
              MOVE 9 TO IO-FG
              GO TO EXIT-GROUPID-SCAN.

           MOVE "S  A040BUILD." TO VDU.
           MOVE CDSCAN-CODE     TO VDUBUF.
           PERFORM SCREENIO.

           MOVE ZEROES             TO IO-FG.
           MOVE CDSCAN-CODE        TO VDUBUF.

       EXIT-GROUPID-SCAN.
           EXIT.


      ***************************************************
       CAN-LOAN-SCAN SECTION.
      ***************************************************
           PERFORM SET-CLFILE-INDEX-BY.

           MOVE " " TO CLSCAN-NON-MONTHLY.
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
           MOVE "SN N042190." TO VDU.
           MOVE DLSCAN-DLNO TO VDUNUM2.
           PERFORM SCREENIO.
           MOVE DLSCAN-DLNO TO VDUNUM2.
           MOVE 0 TO IO-FG.

       EXIT-DEALER-SCAN.
           EXIT.


      ****************************
       BORROWER-QUOTED SECTION.
      ****************************
       BORROWER-QUOTED-ENTER.
           MOVE "F1-CLEAR, ENTER CAN LOAN DESCRIPTION:" TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "E  A330011." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              MOVE "RTN" TO IO-TYPE
              MOVE 9 TO IO-FG
              GO TO EXIT-BORROWER-QUOTED.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              MOVE 9 TO IO-FG
              GO TO EXIT-BORROWER-QUOTED.
           IF VDUSTATUS NOT = "00"
                     OR VDUBUF = SPACES
               GO TO BORROWER-QUOTED-ENTER.
           MOVE VDUBUF TO PD2-BWNAME
                          CL-DESC.

           MOVE "M" TO PD-MAKERCD(1).
           IF NEW-FLAG = 1
              MOVE "N" TO CHFG(14) CHFG(15) CHFG(16)
                          CHFG(17) CHFG(18).

      * BUILD-QUOTE-SSN:
           MOVE PD2-BWNAME TO HASH-BUF.
           MOVE 33 TO HASH-SIZE.
           PERFORM HASH-ROUTINE.
           MOVE HASH-TOTAL TO PD-SSNO(1).

           MOVE 0 TO IO-FG.

           MOVE "N" TO CHFG(2) CHFG(3) CHFG(4) CHFG(6) CHFG(7).

       EXIT-BORROWER-QUOTED.
           EXIT.

      ***********************************
       INSURANCE-ENTRY SECTION.
      ***********************************
           MOVE 0 TO IO-FG.
           SUBTRACT 24 FROM ELE GIVING I-LEVEL.
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
                    MOVE PD-ORGTERM TO VDUNUM0 PD-INSTRM(I-LEVEL)
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

           IF ELE = 30
              IF GP-DEFAULT-O3-INSOURS NOT = " "
                 MOVE "S  A010301." TO VDU
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
           IF NOT (VDUBUF = "Y" OR "M")
              MOVE "Y-YES, M-MEMO" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-IC-OURS.
           IF GP-DEFAULT-O3-INSOURS NOT = " "
              IF ELE = 30
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
                 IF ELE = 29
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
              IF ELE = 30
                 IF PD-INSOURS(I-LEVEL) = "Y"
                    IF IC-CODE NOT = GP-DEFAULT-O3-INS-COMPANY
                       MOVE " " TO MESS
                       STRING "INSURANCE COMPANY BUT BE "
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
              GO TO SEND-IC-PREM.
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
           IF PD-INSOURS(I-LEVEL) NOT = INSOURS-HLDR OR
              PD-INSTRM(I-LEVEL) NOT = INSTERM-HLDR OR
              PD-INSCOVR(I-LEVEL) NOT = AMT-HOLDER   OR
              PD-INSPREM(I-LEVEL) NOT = INSPREM-HLDR
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
           IF NOT (PD-INS-FG(I-LEVEL) = "I" OR "C")
              PERFORM ICE-OR-A-MSG
              GO TO ENTER-IC-CALC-FG.

       CALC-ON-INS-ENTRY.
           PERFORM VALIDATE-FIELDS-AGAINST-MAX.
           IF IO-FG = 9
                   OR NEW-FLAG = 1
              GO TO EXIT-ENTER-INSURANCE.

      *IF PREMIUM IS ZERO SET ALL FIELDS TO ZEROS
           IF PD-INSPREM(I-LEVEL) = ZEROS
              MOVE ZEROS TO PD-INSCOMP(I-LEVEL)
                            PD-INSEFF-DATE(I-LEVEL)
                            PD-INSEXP-DATE(I-LEVEL)
                            PD-INSCOVR(I-LEVEL)
                            PD-INSCOMM(I-LEVEL)
              MOVE SPACES TO PD-INSOURS(I-LEVEL).

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

      ***********************************
       FEE-ENTRY SECTION.
      ***********************************
       ENTER-FEEGLNO.
           MOVE VDUBUF TO CDB-CODE PD-FEECODE(I-LEVEL).

      * WHEN A BRANCH IS FLAGED FOR INS. SALES TAX VIA 
      * BR-FORCE-VALID-CITY-COUNTY = "Y", DONT ALLOW ENTRY OF INS.
      * SALES TAX CODES! THEY HAVE TO ENTER "TM" MANUALLY IN KENTUCKY

           IF CDB-CODE = "TM"
              IF (BR-FORCE-VALID-CITY-COUNTY = "Y" AND PD-ORGST = "KY")
                 MOVE "USE WITH CAUTION, THIS IS UMC SALES TAX!" TO MESS
                 PERFORM SEND-MESS
              ELSE
                 MOVE "UMC SALES TAX CODE IS NOT ALLOWED" TO MESS
                 PERFORM SEND-MESS
                 MOVE 9 TO IO-FG
                 GO TO EXIT-FEE-ENTRY.

           IF CDB-CODE NOT = SPACES
              IF BR-FORCE-VALID-CITY-COUNTY = "Y"
                 IF CDB-CODE = "TC" OR "TA" OR "TP"
                            OR "T1" OR "T2" OR "T3"
                    MOVE "INSURANCE SALES TAX CODES ARE NOT ALLOWED"
                                                            TO MESS
                    PERFORM SEND-MESS
                    MOVE 9 TO IO-FG
                    GO TO EXIT-FEE-ENTRY
                 END-IF
              END-IF
              PERFORM GET-CODE
              MOVE CDB-DESC TO LEGEND
              PERFORM SEND-LEGEND
              IF IO-FG = 9
                 GO TO EXIT-FEE-ENTRY
              ELSE
                 MOVE CDB-GC-GLFEEFINCHG TO PD-FEEFINCHG(I-LEVEL)
                 IF PD-GIFTNET NOT = "Y"
                    GO TO ENTER-FEEAMT
                 ELSE
                 IF I-LEVEL = 2 OR 4 OR 6 OR 8 OR 10
                    MOVE "SNNN042  3." TO VDU
                    MOVE ELE TO VDUROW
                    MOVE CDB-GC-FEE-PRICE TO VDUNUM2
                                         PD-FEEAMT(I-LEVEL)
                    PERFORM SCREENIO
                    GO TO CALC-ON-FEE-ENTRY
                 ELSE
                    MOVE "SNNN042  1." TO VDU
                    MOVE ELE TO VDUROW
                    MOVE CDB-GC-FEE-PRICE TO VDUNUM2
                                         PD-FEEAMT(I-LEVEL)
                    PERFORM SCREENIO
                    GO TO CALC-ON-FEE-ENTRY.

           IF I-LEVEL = 2 OR 4 OR 6 OR 8 OR 10
              GO TO GL-CLEAR-2.
       NEXT-GL-CLEAR.
           MOVE "S  A000  0." TO VDU.
           MOVE ELE TO VDUROW.
           MOVE SPACES TO PD-FEECODE(I-LEVEL) VDUBUF.
           PERFORM SCREENIO.
           MOVE "S  A070  1." TO VDU.
           MOVE ELE TO VDUROW.
           MOVE 0 TO PD-FEEAMT(I-LEVEL).
           PERFORM SCREENIO.
           MOVE " " TO PD-FEEFINCHG(I-LEVEL).
           ADD 1 TO I-LEVEL.
       GL-CLEAR-2.
           MOVE "S  A020  2." TO VDU.
           MOVE ELE TO VDUROW.
           MOVE SPACES TO PD-FEECODE(I-LEVEL) VDUBUF.
           PERFORM SCREENIO.
           MOVE "S  A070  3." TO VDU.
           MOVE ELE TO VDUROW.
           MOVE 0 TO PD-FEEAMT(I-LEVEL).
           PERFORM SCREENIO.
           MOVE " " TO PD-FEEFINCHG(I-LEVEL).
           IF I-LEVEL < 10
              ADD 1 TO I-LEVEL ELE
              GO TO NEXT-GL-CLEAR.
           MOVE 0 TO IO-FG.
           GO TO CALC-ON-FEE-ENTRY.
       ENTER-FEEAMT.
           IF I-LEVEL > 2
              SUBTRACT 1 FROM I-LEVEL GIVING SUB
              IF PD-FEECODE(SUB) = SPACES
                 MOVE 9 TO IO-FG
                 GO TO EXIT-FEE-ENTRY.
           IF I-LEVEL = 2 OR 4 OR 6 OR 8 OR 10
              MOVE "ENNN042  3." TO VDU
           ELSE
              MOVE "ENNN042  1." TO VDU.
           MOVE ELE TO VDUROW.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              MOVE 9 TO IO-FG
              GO TO EXIT-FEE-ENTRY.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO ENTER-FEEAMT.
           IF VDUNUM2 = 0
              GO TO ENTER-FEEAMT.
           MOVE VDUNUM2 TO PD-FEEAMT(I-LEVEL).

       CALC-ON-FEE-ENTRY.

           IF ( PD-FEEAMT(I-LEVEL) > CDB-GC-FEE-MAXAMT )
              MOVE "INVALID: AMOUNT IS GREATER THAN THE LIMIT" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-FEEAMT.

           MOVE 0 TO COUNTER
                     TOTAL-GL-HLDR
                     TOTAL-GL-FINCHG-HLDR.
           PERFORM COMPUTE-GL-FEEAMT.
           PERFORM SP-EDT-TABLE-LOOKUP.
           IF IO-FG = 9
              MOVE 0 TO IO-FG
              GO TO ENTER-FEEAMT.
           IF NEW-FLAG NOT = 1
              IF NOT (I-LEVEL = 1 OR 3 OR 5 OR 7 OR 9)
                 MOVE TOTAL-GL-HLDR TO CIP-GL-AMOUNT-TOTAL
                 MOVE TOTAL-GL-FINCHG-HLDR TO CIP-GL-AMOUNT-TOTAL-FINCHG
                 IF CAN-LOAN-NO NOT = " "
                    MOVE 0 TO CIP-NO-OF-TIMES-CALLED
                    PERFORM CALL-LOAN-CALCULATIONS
                 ELSE
      * CITIZENS TEST
                    MOVE 800 TO CIP-NO-OF-TIMES-CALLED
      *             MOVE 0 TO CIP-NO-OF-TIMES-CALLED
                    PERFORM CALL-LOAN-CALCULATIONS.

       EXIT-FEE-ENTRY.
           EXIT.

      ***********************************
       CHECK-SP-EFFDATE SECTION.
      ***********************************
           MOVE PD-ORGST TO SP-ORGST.
           MOVE PD-SPRCLASS TO SP-SPRCLASS.
           MOVE PD-SUBCLASS TO SP-SUBCLASS.
           MOVE 0 TO SP-LAWCODE.

           PERFORM OPEN-SP1-FILE.
           PERFORM START-SP1-FILE.
       CHECK-SP-EFFDATE-NEXT.
           PERFORM READ-SP1-FILE-NEXT.
           IF IO-FG = 9
              GO TO EXIT-SP-1STPY.

           IF SP-ORGST NOT = PD-ORGST
              OR SP-SPRCLASS NOT = PD-SPRCLASS
                 OR SP-SUBCLASS NOT = PD-SUBCLASS
              MOVE 9 TO IO-FG
              GO TO EXIT-SP-1STPY.

           IF SP-EFFDATE > PD-LOANDATE
              GO TO CHECK-SP-EFFDATE-NEXT
           ELSE
              IF SP-EXPDATE NOT = 0
                 IF SP-EXPDATE NOT > PD-LOANDATE
                    GO TO CHECK-SP-EFFDATE-NEXT.

       EXIT-SP-1STPY.
           PERFORM CLOSE-SP1-FILE.

      ***********************************
       MEMO-ENTRY SECTION.
      ***********************************
           PERFORM SEND-MEMO.
       ENTER-MEMO.
           MOVE "E  A300M2." TO VDU.
           PERFORM SCREENIO.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO ENTER-MEMO.
           MOVE VDUBUF TO PD-MESSAGE.
           IF PD-MESSAGE = " "
              MOVE "M1 M2." TO MESS
              PERFORM RESET-MESS.

      ***********************************
       NEW-RECORD-SETUP SECTION.
      ***********************************
           MOVE 1 TO NEW-FLAG.
           PERFORM CLEAR-PD-FILE.
           MOVE " " TO SCREEN-LINES
                        PD2-KEY
                         SAVE-PD2-KEY
                          PRIOR-LOAN-STAT
                           PASS-STAT-FG.

           MOVE TRANS-DATE TO PD-LOANDATE DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO DEF-LOANDATE.

           MOVE BR-STATE   TO DEF-ORGST PD-ORGST PD-OWNST.
           MOVE THE-GROUP-REFBR TO DEF-ORGBR PD-ORGBR PD-OWNBR.
           MOVE 999.99 TO PD-SERVPER.
           MOVE 999.999 TO PD-ENDORATE PD-OVRDRATE.
           MOVE "C" TO PD-OVRDRATE-FG
                       PD-INS-FG(1) PD-INS-FG(2)
                       PD-INS-FG(3) PD-INS-FG(4)
                       PD-SERCHG-FG PD-MAINTFEE-FG
                       PD-EXTCHG-FG PD-REGPYAMT-FG.
           MOVE 0 TO TOTAL-GL-HLDR
                     TOTAL-GL-FINCHG-HLDR
                     TERM-LOW
                     TERM-HIGH
                     PROCEEDS-LOW
                     PROCEEDS-HIGH
                     POINTS-LOW
                     POINTS-HIGH.

           MOVE "Q" TO QUOTE-ID-FG.
           MOVE DEF-BUF TO WR-BUF.
           MOVE DEF-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

           MOVE "N" TO STAT.
           MOVE "Y" TO DEFAULT-APR-FG.
           PERFORM PROTECT-FIELDS.
           MOVE 9999.99 TO PD-DLNO.
           MOVE 0 TO ELE PRIOR-LNAMT PD-CAN-LOAN-MAX.
           PERFORM CLEAR-CALC-AREA-ZL.

      ***********************************
       DISPLAY-FIELDS SECTION.
      ***********************************
           MOVE " " TO DISPLAY-BUF1.
           PERFORM GET-SP.
           MOVE PD-LNAMT TO D-LOANAMT.
           MOVE PD2-BWNAME TO HOLD-BWNAME.
           MOVE PD-CLASS TO LC-CODE D-LCNO.
           PERFORM GET-LC.
           MOVE LC-DESC TO D-LC-DESC.

           ADD PD-ANTICERN(1) PD-ANTICERN(2) PD-ANTICERN(3)
               PD-ANTICERN(4) PD-ANTICERN(5) PD-ANTICERN(6)
                                                  GIVING D-ANTIC.
           MOVE CL-CAN-NO TO D-010.
           MOVE CL-DESC TO D-011.

           MOVE PD-ORGST TO D-050.
           MOVE PD-ORGBR TO D-051.
           MOVE PD-SUBCLASS TO D-052.
           MOVE SP-DESC TO D-053.
           MOVE PD-LOANDATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO D-060.
           MOVE PD-INTDATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO D-061.
           MOVE PD-FILEDATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO D-062.
           MOVE PD-1STPYDATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO D-070.

           PERFORM MAKE-DATES-CURRENT.

           MOVE PD-ORGTERM TO D-071.

           MOVE "PU"     TO CD-TYPE.
           MOVE PD-PURCD TO CD-CODE D-080.
           PERFORM GET-CD.
           MOVE CD-DESC TO D-081.

           MOVE "CO"      TO CD-TYPE.
           MOVE PD-COLLCD TO CD-CODE D-090.
           PERFORM GET-CD.
           MOVE CD-DESC TO D-091.
           MOVE CD-CO-SECURED TO SPTEST-CD-SECURED.

           MOVE "SR"    TO CD-TYPE.
           MOVE PD-SRCD TO CD-CODE D-100.
           PERFORM GET-CD.
           MOVE CD-DESC TO D-101.

           MOVE PD-PROCEEDS TO D-120.
           MOVE PD-PROCEEDS-FG TO D-121.

           MOVE PD-INSCODES TO D-130.

           MOVE PD-POINTS TO D-131.
           MOVE PD-POINTS-FG TO D-132.

           PERFORM SETUP-FEES-FOR-DISP.

           MOVE PD-DLNO TO D-190 DL-DLNO.
           PERFORM GET-DL1.
           MOVE DL-SHORT TO D-191.

           PERFORM DISPLAY-PROMPTS.

           MOVE DISPLAY-BUF1 TO WR-BUF.
           MOVE DISPLAY-LIST1 TO WR-LIST.
           PERFORM CALL-WRFORM.

           MOVE 0 TO COUNTER
                     TOTAL-GL-HLDR
                     TOTAL-GL-FINCHG-HLDR.
           PERFORM COMPUTE-GL-FEEAMT.
           MOVE 800 TO CIP-NO-OF-TIMES-CALLED.
           PERFORM SETUP-CALC-AREA.

       DISP-INTERMEDIATE-FIELDS.
           MOVE " " TO DISPLAY-BUF2.
           MOVE PD-COLLID TO D-200.

           PERFORM MOVE-MSG.

           MOVE PD-OVRDRATE TO D-210 CIP-OVR-RATE.
           MOVE PD-OVRDRATE-FG TO D-211 CIP-OVR-RATE-SW.
           MOVE PD-EFFRATE TO D-220 CIP-EFF-RATE CEP-EFF-RATE.
           MOVE PD-SMPRATE TO CIP-SIM-RATE.
           MOVE PD-APRATE TO D-230 CIP-APR-RATE CEP-APR-RATE.
           MOVE PD-ENDORATE TO D-240.

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
              MOVE DATE-MMDDYY TO D-2EXP(SUB).
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
           MOVE PD-SERCHG TO D-310 CIP-SERVICE-CHG CEP-SERVICE-CHG.
           MOVE PD-STATE-WORKER TO CIP-STATE-WORKER.
           MOVE PD-SERCHG-FG TO D-311 CIP-SERVICE-CHG-SW.
           IF PD-ORGST = "MX"
              MOVE PD-ANTICERN(4) TO D-320
                                     MAINTFEE-HLDR
                                     CIP-MAINT-FEES
                                     CEP-MAINT-FEES
           ELSE
              MULTIPLY PD-MAINTFEE BY PD-ORGTERM
                   GIVING D-320 ROUNDED
                          MAINTFEE-HLDR ROUNDED
                          CIP-MAINT-FEES ROUNDED
                          CEP-MAINT-FEES ROUNDED.
           MOVE PD-MAINTFEE-FG TO D-321 CIP-MAINT-FEES-SW.
           MOVE PD-EXTCHG TO D-330 CIP-1ST-PY-EXT CEP-1ST-PY-EXT.
           MOVE PD-EXTCHG-FG TO D-331 CIP-1ST-PY-EXT-SW.
           MOVE PD-INTCHG TO D-340 CIP-INT-CHARGE.
           MOVE PD-APR-FINAMT TO D-350 CIP-AMT-FINANCD.
           MOVE PD-LNAMT TO CIP-LOAN-AMOUNT.
           IF PD-LOANTYPE = "P"
              MOVE PD-LNAMT TO D-360
           ELSE
              PERFORM LOAN-CALCULATIONS
              MOVE TOTAL-REPAY TO D-360.
           ADD PD-ANTICERN(1) PD-ANTICERN(2) PD-ANTICERN(3)
               PD-ANTICERN(4) PD-ANTICERN(5) PD-ANTICERN(6)
               GIVING CIP-ANTIC-EARNS.
           MOVE PD-1STPYAMT TO D-370 CIP-ODD-1ST-PMT.
           MOVE PD-REGPYAMT TO D-380 CIP-REG-PAYMENT.
           MOVE PD-REGPYAMT-FG TO D-381 CIP-REG-PAYMENT-SW.

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
              IF PD-ORGST NOT = "MX"
                 COMPUTE PD-ANTICERN(4) = PD-ORGTERM * PD-MAINTFEE
              END-IF
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

      ***********************************
       SETUP-FEES-FOR-DISP SECTION.
      ***********************************
           MOVE 1 TO ELE.
           MOVE 1 TO SUB.
       SETUP-FEES-FOR-DISP-NEXT.
           MOVE PD-FEECODE(SUB) TO D-1FEECD(ELE).
           MOVE PD-FEEAMT(SUB) TO D-1FEEAMT(ELE).
           ADD 1 TO SUB.
           MOVE PD-FEECODE(SUB) TO D-2FEECD(ELE).
           MOVE PD-FEEAMT(SUB) TO D-2FEEAMT(ELE).
           IF SUB < 10
              ADD 1 TO ELE SUB
              GO TO SETUP-FEES-FOR-DISP-NEXT.

      *****************************************
       WRITE-MODULE SECTION.
      *****************************************
           PERFORM OPEN-CL1-FILE.
           IF PD-GIFTNET = " "
              MOVE "N" TO PD-GIFTNET.

           MOVE PD-GIFTNET TO CL-GIFTNET.

           MOVE PD-REC TO CL-PD-REC.
           IF NEW-FLAG NOT = 0
              GO TO WRITE-RECORD.

           MOVE CL-REC TO CHANGED-CL-REC.
           IF ORIGINAL-CL-REC = CHANGED-CL-REC
              GO TO END-WRITE-MODULE.

           PERFORM READ-CL1-FILE.
           IF IO-FG NOT = 0
              PERFORM UNDERGONE-CHANGE
              PERFORM RECORD-DELETED
              GO TO END-WRITE-MODULE.
           IF CL-REC NOT = ORIGINAL-CL-REC
              PERFORM UNDERGONE-CHANGE
              PERFORM CHANGE-AGAIN
              GO TO END-WRITE-MODULE.

           MOVE CHANGED-CL-REC TO CL-REC.
           PERFORM REWRITE-CL1-FILE.
           GO TO END-WRITE-MODULE.

       WRITE-RECORD.
           PERFORM WRITE-CL1-FILE.
           IF IO-FG NOT = 0
              PERFORM ALREADY-CREATED.

       END-WRITE-MODULE.
           PERFORM CLOSE-CL1-FILE.

      *****************************************
       DELETE-MODULE SECTION.
      *****************************************
           IF NEW-FLAG NOT = 0
              GO TO END-DELETE-MODULE.
           PERFORM OPEN-CL1-FILE.
           PERFORM READ-CL1-FILE.
           IF IO-FG NOT = 0
              PERFORM UNDERGONE-CHANGE
              PERFORM RECORD-DELETED
              GO TO END-DELETE-MODULE.
           IF CL-REC NOT = ORIGINAL-CL-REC
              PERFORM UNDERGONE-CHANGE
              PERFORM RECORD-CHANGED
              GO TO END-DELETE-MODULE.
           PERFORM DELETE-CL1-FILE.

       END-DELETE-MODULE.
           PERFORM CLOSE-CL1-FILE.


      *****************************************
       DEFAULT-INT-PMT-DATES SECTION.
      *****************************************
           PERFORM CHECK-SP-EFFDATE.
           IF IO-FG = 9
              PERFORM SP-INVALID
              GO TO EXIT-DEFAULT-INT-PMT-DATES.

           MOVE SP-LAWCODE TO PD-LAWCODE.
           IF PD-INTDATE = 0
              MOVE "S  A000061." TO VDU
              MOVE PD-LOANDATE TO PD-INTDATE DATE-YYYYMMDD
              PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
              MOVE DATE-MMDDYY TO VDU-DATE-EDIT
              PERFORM SCREENIO.
           IF PD-1STPYDATE NOT = 0
              MOVE 0 TO AMT-HOLDER
              GO TO EXIT-DEFAULT-INT-PMT-DATES.

           MOVE 1 TO AMT-HOLDER.
           MOVE PD-INTDATE TO NDTE-DATE.
           MOVE 1          TO NDTE-HOLD.
           PERFORM INCREMENT-MONTHS.
           MOVE "S  A000070." TO VDU.
           MOVE NDTE-DATE TO PD-1STPYDATE PD-ORIG-1STPYDATE
                             DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO VDU-DATE-EDIT.
           PERFORM SCREENIO.

       EXIT-DEFAULT-INT-PMT-DATES.
           EXIT.

      *****************************************
       VALIDATION SECTION.
      *****************************************
           IF ELE = 25 OR 26 OR 27 OR 28
              SUBTRACT 24 FROM ELE GIVING I-LEVEL
              IF PD-INS-FG(I-LEVEL) NOT = "C"
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
                 IF ELE NOT = 27 AND NOT = 28
                    IF PD-INSCOVR(I-LEVEL) > CEP-INS-COVERAGE(I-LEVEL)
                       MOVE "INS. COVERAGE" TO TYPE-OF-AMT-EXCEEDED
                       MOVE CEP-INS-COVERAGE(I-LEVEL)
                                           TO HOLD-AMT-EXCEEDED
                       GO TO VALIDATE-EXCEEDS-MAX
                    ELSE
                       NEXT SENTENCE
                 ELSE
                    PERFORM VALIDATE-TEST-PP
      * PERSONAL PROPERTY OF 888888, SKIP MAX TEST
                    IF NOT SP-INSMAXCHGABL-OVRIDE(ILEV)
                       IF PD-INSCOVR(I-LEVEL)
                                           > CEP-INS-COVERAGE(I-LEVEL)
                          MOVE "INS. COVERAGE" TO TYPE-OF-AMT-EXCEEDED
                          MOVE CEP-INS-COVERAGE(I-LEVEL)
                                              TO HOLD-AMT-EXCEEDED
                          GO TO VALIDATE-EXCEEDS-MAX.

           IF ELE = 31
              IF PD-SERCHG > CEP-SERVICE-CHG
                 MOVE "SERVICE CHG" TO TYPE-OF-AMT-EXCEEDED
                 MOVE CEP-SERVICE-CHG TO HOLD-AMT-EXCEEDED
                 GO TO VALIDATE-EXCEEDS-MAX.
           IF ELE = 32
              MULTIPLY PD-MAINTFEE BY PD-ORGTERM
                               GIVING AMT-HOLDER ROUNDED
              IF AMT-HOLDER > CEP-MAINT-FEES
                 MOVE "MAINT. FEE" TO TYPE-OF-AMT-EXCEEDED
                 MOVE CEP-MAINT-FEES TO HOLD-AMT-EXCEEDED
                 GO TO VALIDATE-EXCEEDS-MAX.
           IF ELE = 33
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
           IF ELE = 28
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

      *****************************************
       SP-EDT-TABLE-LOOKUP SECTION.
      *****************************************
           SET ETBL TO 1.
           MOVE 0 TO COUNTER.
           IF ELE = 13
              MOVE 999999.99 TO POINTS-LOW
              GO TO POINTS-VALIDATION.

           IF ELE = 12 OR
              (ELE > 13 AND ELE < 19)
              MOVE 999 TO TERM-LOW
              MOVE 999999.99 TO PROCEEDS-LOW
              MOVE 0 TO TERM-HIGH PROCEEDS-HIGH
              COMPUTE AMT-HOLDER ROUNDED =
                   PRIOR-LNAMT + PD-PROCEEDS + TOTAL-GL-HLDR
              GO TO PROCEEDS-VALIDATION.

           IF PD-ORGTERM < 1
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

           IF SP-EDT-TYPE(ETBL,COUNTER) = "A"
              IF SP-EDT-HL(ETBL,COUNTER) = "L"
                 IF SP-EDT-AMTYPE(ETBL,COUNTER) = "P"
                    IF SP-EDT-AMT(ETBL,COUNTER) < PROCEEDS-LOW
                       MOVE SP-EDT-AMT(ETBL,COUNTER) TO PROCEEDS-LOW.
           IF SP-EDT-TYPE(ETBL,COUNTER) = "A"
              IF SP-EDT-HL(ETBL,COUNTER) = "H"
                 IF SP-EDT-AMTYPE(ETBL,COUNTER) = "P"
                    IF SP-EDT-AMT(ETBL,COUNTER) > PROCEEDS-HIGH
                       MOVE SP-EDT-AMT(ETBL,COUNTER) TO PROCEEDS-HIGH.

           IF COUNTER NOT > SP-EDT-NOEDITS(ETBL)
              GO TO PROCEEDS-VALIDATION.

       PROCEEDS-TABLE2.
           IF SP-EDT-NOEDITS(ETBL) NOT < 6 AND ETBL NOT = 2
              MOVE 0 TO COUNTER
              SET ETBL TO 2
              GO TO PROCEEDS-VALIDATION.

           IF ((AMT-HOLDER < PROCEEDS-LOW)
                        AND (PROCEEDS-LOW NOT = 999999.99))
                  OR ((AMT-HOLDER > PROCEEDS-HIGH)
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

      *****************************************
       MAKE-DATES-CURRENT SECTION.
      *****************************************
           IF PD-LOANDATE NOT = PD-INTDATE
              MOVE PD-LOANDATE TO DATE-YYYYMMDD
              PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
              MOVE DATE-MMDDYY TO D-060
              MOVE PD-INTDATE TO DATE-YYYYMMDD
              PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
              MOVE DATE-MMDDYY TO D-061
              MOVE PD-1STPYDATE TO DATE-YYYYMMDD
              PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
              MOVE DATE-MMDDYY TO D-070
              GO TO MAKE-DATES-CURRENT-EXIT.

      * SET NEW INTEREST DATE:
           MOVE PD-LOANDATE TO NUM-DATE.
           MOVE PD-INTDATE TO SYS-DATE.
           PERFORM TIM.
           MOVE ELAPSED-DAYS TO NDTE-HOLD.
           MOVE TRANS-DATE   TO NDTE-DATE.
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
           MOVE DATE-MMDDYY TO D-070.

      * SET INSURANCE EXPIRATION DATES, IF NECESSARY
      * 06/28/96 BAH
           MOVE 5 TO SUB.
       MAKE-NEXT-INS-CURRENT.
           IF PD-INSEXP-DATE(SUB) NOT = 0
                    AND PD-INSPREM(SUB) NOT = 0
              MOVE PD-INTDATE TO NDTE-DATE
              MOVE PD-ORGTERM TO NDTE-HOLD
              PERFORM INCREMENT-MONTHS
              MOVE "SNNN060  2." TO VDU
              COMPUTE VDUROW = SUB + 24
              MOVE NDTE-DATE TO PD-INSEXP-DATE(SUB) DATE-YYYYMMDD
              PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
              MOVE DATE-MMDDYY TO VDUNUM0
              PERFORM SCREENIO.
           IF SUB < 6
              ADD 1 TO SUB
              GO TO MAKE-NEXT-INS-CURRENT.

       MAKE-DATES-CURRENT-EXIT.
           EXIT.

      *****************************************
       VERIFY-PASSWORD SECTION.
      *****************************************
           MOVE " " TO PASSWDW-STAT.
           MOVE "CLQUOT" TO PASSWDW-PGM.
           MOVE "PLEASE ENTER THE PASSWORD," TO PASSWDW-PROMPT1.
           MOVE "CL/PASSWD" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH PASSWDW-BUF.
           CANCEL FORM-PROGX.

       COPY "LIBGB/GPENV.CPY".
       COPY "LIBGB/DATER.CPY".
       COPY "LIBLP/INSELIG.CPY".
       COPY "LIBLP/LPANTI.CPY".
       COPY "LIBGB/HASH.CPY".
       COPY "LIBLP/LPDAMTS.CPY".
       COPY "LIBLP/LPSPEDP.CPY".
       COPY "LIBLP/LPBAGE.CPY".
       COPY "LIBLP/LPPDCL.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBLP/LPZLCL.CPY".
       COPY "LIBGB/TSTCANID.CPY".

      *********************************
       FORM-RESET SECTION.
      *********************************
           MOVE EXT-CONAME-SYSDATE TO CLQUOT-SYSDATE.
           DISPLAY SPACES UPON CLQUOT-WINDOW-HANDLE
                          LINE 1 COL 1 ERASE SCREEN.
           DISPLAY CLQUOT-SCREEN UPON CLQUOT-WINDOW-HANDLE.
           MOVE CLQUOT-FORM-FIELDS TO WR-BUF.
           MOVE CLQUOT-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

           PERFORM SEND-BUILD-PROMPT.
           PERFORM SEND-BUILD.
           PERFORM SEND-REFBR.

      *********************************************
       SETUP-LINK-INFO SECTION.
      *********************************************
           MOVE CLQUOT-HELP--FILE TO HELP-LINK-FILE.
           MOVE CLQUOT-HELP--DIR TO HELP-LINK-DIR.

      *********************************************
       VDU-CONVERT-FIELD SECTION.
      *********************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBWI/CLQUOT_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      *****************************************
       MISC-PARAGRAPHS SECTION.
      *****************************************
       COPY "LIBGB/SYSTEM.CPY".
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/GETENV.CPY".
       VERIFY-WRITE.
           MOVE 0 TO IO-FG.
       VERIFY-DELETE.
           MOVE 0 TO IO-FG.
       SEND-BUILD-PROMPT.
           MOVE "S  A150BUILDP:S." TO VDU
           MOVE "BUILD FOR GROUP ID:" TO VDUBUF
           PERFORM SCREENIO.
           MOVE "S  A150REFBRP:S." TO VDU
           MOVE "REFERENCE BRANCH  :" TO VDUBUF
           PERFORM SCREENIO.
       SEND-BUILD.
           MOVE "CL"         TO CD-TYPE.
           MOVE THE-GROUP-ID TO CD-CODE.
           PERFORM GET-CD.
           IF IO-FG = 0
              MOVE "S  A020BUILD." TO VDU
              MOVE THE-GROUP-ID TO VDUBUF
              PERFORM SCREENIO
              MOVE "S  A160BUILDD." TO VDU
              MOVE CD-DESC          TO VDUBUF
              PERFORM SCREENIO.
       SEND-REFBR.
           MOVE THE-GROUP-REFBR TO BR-NO.
           PERFORM GET-BR.
           IF IO-FG = 0
              MOVE "SNNN040REFBR." TO VDU
              MOVE THE-GROUP-REFBR TO VDUNUM0
              PERFORM SCREENIO
              MOVE "S  A160REFBRD." TO VDU
              MOVE BR-NAME TO VDUBUF
              PERFORM SCREENIO.

      *----------------------------------------------------
       SET-CLFILE-INDEX-BY.
           MOVE THE-GROUP-ID TO CL-GROUP-ID CLSCAN-GROUP-ID.
      *----------------------------------------------------
      * IF MAKING CHANGES, CALCULATE PROCEEDS AND NOT PYAMT
      *FORCE-CAN-LOAN-NUMBERS.
      *    MOVE "S  A010121." TO VDU.
      *    MOVE "C" TO VDUBUF PD-PROCEEDS-FG
      *                       CIP-PROCEEDS-SW.
      *    PERFORM SCREENIO.
      *    MOVE "S  A010381." TO VDU.
      *    MOVE "I" TO VDUBUF PD-REGPYAMT-FG
      *                       CIP-REG-PAYMENT-SW.
      *    PERFORM SCREENIO.


       DISPLAY-PROMPTS.
           IF PROMPT-FG = " "
              MOVE "Y" TO PROMPT-FG
              MOVE PROMPT-BUF TO WR-BUF
              MOVE PROMPT-LIST TO WR-LIST
              PERFORM CALL-WRFORM.

       GET-CODE.
           MOVE "GC"     TO CDB-TYPE.
           MOVE PD-OWNBR TO CDB-BRANCH.
      *--> SET BEFORE       CDB-CODE.
           PERFORM OPEN-CDB1-FILE.
           PERFORM READ-CDB1-FILE.
           PERFORM CLOSE-CDB1-FILE.
           IF IO-FG = 9
              MOVE " * NOT ON FILE * " TO CDB-DESC.
           IF PD-GIFTNET = "Y"
              IF CDB-GC-FEE-GIFTNET = "Y"
                 NEXT SENTENCE
              ELSE
                 MOVE 9 TO IO-FG
           ELSE
              IF CDB-GC-FEE-GIFTNET = "Y"
                 MOVE 9 TO IO-FG.
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
       GET-DL1.
           PERFORM OPEN-DL1-FILE.
           PERFORM READ-DL1-FILE.
           PERFORM CLOSE-DL1-FILE.
           IF IO-FG = 9
              MOVE " INVALID DEALER" TO DL-SHORT.
       GET-BR.
           PERFORM OPEN-BR-FILE.
           PERFORM READ-BR-FILE.
           PERFORM CLOSE-BR-FILE.
       GET-SP.
           MOVE PD-ORGST TO SP-ORGST.
           MOVE PD-SPRCLASS TO SP-SPRCLASS.
           MOVE PD-SUBCLASS TO SP-SUBCLASS.
           MOVE PD-LAWCODE TO SP-LAWCODE.
           PERFORM OPEN-SP1-FILE.
           PERFORM READ-SP1-FILE.
           IF IO-FG = 9
              GO TO IO-ERROR.
           PERFORM CLOSE-SP1-FILE.

       MATURITY-DATE-CALCULATION.
           MOVE PD-1STPYDATE TO NDTE-DATE.
           COMPUTE NDTE-HOLD = PD-ORGTERM - 1.
           PERFORM INCREMENT-MONTHS.

       UPDATE-OVR-RATE.
           IF PD-LOANTYPE = "P"
                    OR COP-OVR-RATE-SW = "C"
              MOVE 999.999 TO PD-OVRDRATE
           ELSE
              IF COP-OVR-RATE-SW = "E"
                 MOVE COP-OVR-RATE TO PD-OVRDRATE
              ELSE
                 MOVE COP-EFF-RATE TO PD-OVRDRATE.
       RECORD-DELETED.
           MOVE " THIS RECORD HAS BEEN DELETED!" TO MESS.
           PERFORM SEND-MESS.
       RECORD-CHANGED.
           MOVE " CHANGES WERE MADE TO THIS RECORD!" TO MESS.
           PERFORM SEND-MESS.
       CHANGE-AGAIN.
           MOVE " PLEASE RE-INPUT YOUR CHANGES!" TO MESS.
           PERFORM SEND-MESS.
       ALREADY-CREATED.
           MOVE " RECORD HAS BEEN ALREADY CREATED!" TO MESS.
           PERFORM SEND-MESS.
       UNDERGONE-CHANGE.
           MOVE
           " THIS RECORD HAS UNDERGONE CHANGE BY ANOTHER USER,"
           TO MESS.
           PERFORM SEND-MESS.

       MOVE-MSG.
           MOVE PD-MSG1 TO MSG-LINE1.
           MOVE PD-MSG2 TO MSG-LINE2.
           MOVE SCREEN-LINE1 TO D-201.
           MOVE SCREEN-LINE2 TO D-202.

       VALIDATE-FIELDS-AGAINST-MAX.
           IF NEW-FLAG NOT = 1
              PERFORM VALIDATION.

       VERIFY-REQD-FLDS.
           IF ACTION = "     U"
              IF PD-LOANDATE NOT = TRANS-DATE
                 MOVE 9 TO IO-FG
                 MOVE "LOAN DATE MUST EQUAL TRANSACTION DATE" TO MESS
                 PERFORM SEND-MESS.
           IF PD-PURCD = " "
              MOVE 9 TO IO-FG
              MOVE "PURPOSE CODE REQUIRED BEFORE SAVE/UPDATE" TO MESS
              PERFORM SEND-MESS.
           IF PD-COLLCD = " "
              MOVE 9 TO IO-FG
            MOVE "COLLATERAL CODE REQUIRED BEFORE SAVE/UPDATE" TO MESS
              PERFORM SEND-MESS.
           PERFORM VERIFY-PROCEEDS.

       VERIFY-PROCEEDS.

       COMPUTE-GL-FEEAMT.
           ADD 1 TO COUNTER.
           IF COUNTER NOT > 10
              ADD PD-FEEAMT(COUNTER) TO TOTAL-GL-HLDR
              IF PD-FEEFINCHG(COUNTER) = "Y"
                 ADD PD-FEEAMT(COUNTER) TO TOTAL-GL-FINCHG-HLDR
              END-IF
              GO TO COMPUTE-GL-FEEAMT.
           MOVE 0 TO COUNTER.

       SETUP-CALC-AREA.
           MOVE "C" TO CIP-OVR-RATE-SW
                       CIP-INS-SW(1) CIP-INS-SW(2)
                       CIP-INS-SW(3) CIP-INS-SW(4)
                       CIP-SERVICE-CHG-SW CIP-MAINT-FEES-SW
                       CIP-1ST-PY-EXT-SW CIP-REG-PAYMENT-SW.
           MOVE "I" TO CIP-INS-SW(5) CIP-INS-SW(6).

           MOVE PD-LOANTYPE TO CIP-LOANTYPE.
           MOVE PD-SMALLOAN TO CIP-SMALLOAN.
           MOVE PD-SALEFIN TO CIP-SALEFIN.
           MOVE PD-SERVACCT TO CIP-SERVACCT.

           MOVE PD-LOANDATE  TO CIP-LN-DT.
           MOVE PD-INTDATE   TO CIP-IN-DT.
           MOVE PD-FILEDATE  TO CIP-FL-DT.
           MOVE PD-1STPYDATE TO CIP-PMT-DT.
           MOVE PD-ORGTERM   TO CIP-TERM.

           MOVE PD-PURCD TO CIP-PURPOSE-CD.
           MOVE PD-COLLCD TO CIP-COLLTRL-CD.
           MOVE PD-SRCD TO CIP-SOURCE-CD.
           MOVE PD-PROCEEDS TO CIP-PROCEEDS.
           MOVE PD-PROCEEDS-FG TO CIP-PROCEEDS-SW.

           MOVE PD-INSURED TO CIP-INS-WHO.
           MOVE PD-INSTYPES TO CIP-INS-TYPE.
           MOVE PD-INSELIGIBLE TO CIP-INS-ELIG.
           MOVE PD-POINTS TO CIP-POINTS.
           MOVE PD-POINTS-FG TO CIP-POINTS-SW.
           MOVE TOTAL-GL-HLDR TO CIP-GL-AMOUNT-TOTAL.
           MOVE TOTAL-GL-FINCHG-HLDR TO CIP-GL-AMOUNT-TOTAL-FINCHG.

       CALL-LOAN-CALCULATIONS.
           MOVE "Y" TO LOAN-CALCULATOR-CALLED-FG.
           MOVE "CALCULATING - PLEASE WAIT" TO LEGEND.
           PERFORM SEND-LEGEND.

      * SETUP VERIFY-REJECT-TERMS RE: SP-INSREJECT-FG = "C":
           PERFORM ESTABLISH-REJECT-TERMS.

           MOVE CAN-LOAN-NO TO CIP-CAN-LOAN-NO.
      *----------------------------------------------------------
      *    ENVIRONMENT VARIABLE 'CALL_TESTZL' IS USED TO CALL
      *    ANOTHER VERSION OF CALCZL,
      *    NOTE:
      *         HELPFULL WHEN TRYING TO RUN CALCZL.C IN DEBUG
      *         FROM TWO TERMINALS AT THE SAME TIME.
      *         THE DEBUGGER DOSN'T ACT CORRECTLY WITH TWO
      *         SESSIONS RUN TOGETHER
      *         OR
      *         WHEN MAKING LARGE CHANGES TO CALCZL.C
      *
      *    MOVE "CALL_TESTZL" TO GETENV-BUF.
      *    PERFORM GETENV-CALL.
      *    IF STAT = "00"
      *       MOVE "LP/TESTZL" TO FORM-NAM
      *    ELSE
      *----------------------------------------------------------
           MOVE "LP/CALCZL" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH
                                CIP-CALCULATION-INPUT-AREA
                                CEP-CALCULATION-EDIT-AREA
                                COP-CALCULATION-OUTPUT-AREA
                                SP-REC
                                BR-REC STATE CITY-TAX-CD COUNTY.
           CANCEL FORM-PROGX.

           MOVE COP-PROCEEDS TO PD-PROCEEDS DF2-PROCEEDS.
      *    IF PD-CAN-LOAN-MAX NOT = 0
      *       IF PRIOR-LNAMT = 0
                 MOVE PD-PROCEEDS TO PD-CAN-LOAN-MAX.
           MOVE COP-PROCEEDS-SW TO PD-PROCEEDS-FG DF2-PROCEEDS-FG.
           MOVE COP-POINTS TO PD-POINTS DF2-POINTS.
           MOVE COP-POINTS-SW TO PD-POINTS-FG DF2-POINTS-FG.

           MOVE COP-OVR-RATE TO PD-OVRDRATE DF2-OVRDRATE.
           MOVE COP-OVR-RATE-SW TO PD-OVRDRATE-FG DF2-OVRDRATE-FG.
           MOVE COP-EFF-RATE TO PD-EFFRATE DF2-EFFRATE.
           MOVE COP-SIM-RATE TO PD-SMPRATE.
           MOVE COP-APR-RATE TO PD-APRATE DF2-APRATE.
           MOVE PD-ENDORATE TO DF2-ENDORATE.
           MOVE COP-TOT-REPYBLE TO DF2-TREPAY.
           PERFORM MOVE-INSURANCE-TO-PDREC VARYING COUNTER
              FROM 1 BY 1 UNTIL COUNTER > 4.

           MOVE COP-SERVICE-CHG TO PD-SERCHG DF2-SERCHG
                                              PD-SUM-SERCHG.
           MOVE COP-STATE-WORKER TO PD-STATE-WORKER.
           MOVE COP-SERVICE-CHG-SW TO PD-SERCHG-FG DF2-SERCHG-FG.
           MOVE COP-MAINT-FEES TO DF2-MAINTFEE MAINTFEE-HLDR
                                  PD-SUM-MAINTFEE.
           DIVIDE COP-MAINT-FEES BY COP-TERM GIVING
                                               PD-MAINTFEE ROUNDED.
           MOVE COP-MAINT-FEES-SW TO PD-MAINTFEE-FG DF2-MAINTFEE-FG.
           MOVE COP-1ST-PY-EXT TO PD-EXTCHG DF2-EXTCHG
                                         PD-SUM-EXTCHG.

           MOVE COP-1ST-PY-EXT-SW TO PD-EXTCHG-FG DF2-EXTCHG-FG.
           MOVE COP-INT-CHARGE TO PD-INTCHG DF2-INTCHG
                                            PD-SUM-INTCHG.
           MOVE COP-APR-FINANCD TO PD-APR-FINAMT
                                   DF2-APR-FINAMT.
           MOVE COP-AMT-FINANCD TO PD-FINAMT.
           MOVE COP-LOAN-AMOUNT TO PD-LNAMT PD-PURBAL
                                   PD-ORIGPURAMT DF2-LNAMT
                                   PD-SUM-LNAMT
                                   PD-SUM-ORIGPURAMT.
           MOVE COP-ANTIC-EARNS TO DF2-ANTICIPATED.
           IF PD-LOANTYPE = "I"
              MOVE PD-SERCHG TO PD-ANTICERN(3)
              MOVE 0 TO PD-ANTICERN(1)
                        PD-ANTICERN(2)
                        PD-ANTICERN(4)
           ELSE
              ADD COP-INT-CHARGE COP-1ST-PY-EXT GIVING PD-ANTICERN(1)
              MOVE COP-SERVICE-CHG TO PD-ANTICERN(3)
              MOVE COP-MAINT-FEES  TO PD-ANTICERN(4).
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

           MOVE COP-ODD-1ST-PMT TO PD-1STPYAMT DF2-1STPYAMT.
           MOVE COP-REG-PAYMENT TO PD-REGPYAMT DF2-REGPYAMT
                                       PD-SUM-REGPYAMT.
           MOVE COP-REG-PAYMENT-SW TO PD-REGPYAMT-FG DF2-REGPYAMT-FG.
           MOVE COP-FINCHG TO PD-FINCHG PD-SUM-FINCHG.
           MOVE COP-RETURN-CODE TO DF2-RETURN-CODE.
           MOVE COP-LOOP-COUNT TO DF2-LOOP-COUNT.

           IF COP-1ST-PMT-MTHS = 1
                    AND COP-1ST-PMT-DAYS = 0
              MOVE "Y" TO PD-ONE-MTH-NO-DAY-EXT-SW
           ELSE
              MOVE " " TO PD-ONE-MTH-NO-DAY-EXT-SW.

           PERFORM DISPLAY-PROMPTS.
           MOVE DF2-BUF TO WR-BUF.
           MOVE DF2-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

           PERFORM RESET-LEGEND.
           MOVE COP-CALCULATION-OUTPUT-AREA
                        TO CIP-CALCULATION-INPUT-AREA.
           PERFORM CALC-NO-OF-PAYMENTS.
           IF COP-AMT-FINANCD > COP-LOAN-AMOUNT
              MOVE "X" TO COP-RETURN-CODE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    TEST FOR PROCEEDS < 0
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           COMPUTE VDUNUM2 = COP-TOTAL-NOTE
                             - COP-NEW-LOAN-PROCEEDS
                             - COP-SERVICE-CHG.
           IF NOT COP-INS-USE-IS-MEMO(1)
              SUBTRACT COP-INS-PREMIUM(1) FROM VDUNUM2.
           IF NOT COP-INS-USE-IS-MEMO(2)
              SUBTRACT COP-INS-PREMIUM(2) FROM VDUNUM2.
           IF NOT COP-INS-USE-IS-MEMO(3)
              SUBTRACT COP-INS-PREMIUM(3) FROM VDUNUM2.
           IF NOT COP-INS-USE-IS-MEMO(4)
              SUBTRACT COP-INS-PREMIUM(4) FROM VDUNUM2.
           IF NOT COP-INS-USE-IS-MEMO(5)
              SUBTRACT COP-INS-PREMIUM(5) FROM VDUNUM2.
           IF NOT COP-INS-USE-IS-MEMO(6)
              SUBTRACT COP-INS-PREMIUM(6) FROM VDUNUM2.
           IF NOT COP-INS-USE-IS-MEMO(7)
              SUBTRACT COP-INS-PREMIUM(7) FROM VDUNUM2.
           IF NOT COP-INS-USE-IS-MEMO(8)
              SUBTRACT COP-INS-PREMIUM(8) FROM VDUNUM2.
           IF VDUNUM2 < 0
              MOVE "X" TO COP-RETURN-CODE.

           IF COP-CALCULATION-OK
              PERFORM VERIFY-AGAINST-SPEDT
           ELSE
              PERFORM ERROR-IN-CALCULATIONS-MSG.
           MOVE 0 TO IO-FG.

       MOVE-INSURANCE-TO-PDREC.
           MOVE COP-INS-CO(COUNTER)       TO PD-INSCOMP(COUNTER)
                                             DF2-INSCOMP(COUNTER).
           MOVE COP-INS-USE-SW(COUNTER)   TO PD-INSOURS(COUNTER)
                                             DF2-INSOURS(COUNTER).
           MOVE COP-INS-TERM(COUNTER)     TO PD-INSTRM(COUNTER)
                                             DF2-INSTERM(COUNTER).
           MOVE COP-INS-COVERAGE(COUNTER) TO PD-INSCOVR(COUNTER)
                                             DF2-INSCOVR(COUNTER).
           MOVE COP-INS-PREMIUM(COUNTER)  TO PD-INSPREM(COUNTER)
                                             DF2-INSPREM(COUNTER).
           MOVE COP-INS-SW(COUNTER)       TO PD-INS-FG(COUNTER)
                                             DF2-INS-FG(COUNTER).
           MOVE COP-INSFINCHG-SW(COUNTER) TO PD-INSFINCHG(COUNTER).
           MOVE COP-INS-TERM-SW(COUNTER) TO PD-INS-TERM-SW(COUNTER).
           MOVE COP-COVERAGE-SW(COUNTER) TO PD-COVERAGE-SW(COUNTER).
           MOVE COP-PREMIUM-SW(COUNTER) TO PD-PREMIUM-SW(COUNTER).

       ICE-OR-A-MSG.
           IF ELE = 21
              MOVE "MUST BE 'C', 'E', OR 'A'" TO MESS
           ELSE
              MOVE "MUST BE 'I' OR 'C'" TO MESS.
           PERFORM SEND-MESS.

       CALC-NO-OF-PAYMENTS.
           IF PD-1STPYAMT = 0
              MOVE 0 TO PD-NOODDPY
              MOVE PD-ORGTERM TO PD-NOREGPY
           ELSE
              MOVE 1 TO PD-NOODDPY
              SUBTRACT 1 FROM PD-ORGTERM GIVING PD-NOREGPY.

       ERROR-IN-CALCULATIONS-MSG.
           IF COP-RETURN-CODE = "V"
              MOVE "ERROR IN CALCULATIONS - AMOUNT EXCEEDS MAX"
                                                        TO MESS
           ELSE
              MOVE "ERROR IN CALCULATIONS - INACCURATE RESULTS"
                                                        TO MESS.
           PERFORM SEND-MESS.

       SEND-CD-DESC.
           PERFORM GET-CD.
           MOVE "S  A000  1." TO VDU.
           MOVE ELE TO VDUROW.
           MOVE CD-DESC TO VDUBUF.
           PERFORM SCREENIO.
       ADD-IT.
           INSPECT INSPECT-W REPLACING FIRST "  " BY " +".
           INSPECT INSPECT-W REPLACING FIRST
              "+                        " BY ADD-ON.
       ADD-IT2.
           PERFORM ADD-IT.
           MOVE "/" TO ADD-ON.
           PERFORM ADD-IT.

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
       NOT-DIRECT.
           MOVE "INVALID FOR DIRECT LOAN ENTRY" TO MESS.
           PERFORM SEND-MESS.
       SEND-LEGEND.
           MOVE LEGEND TO WR-BUF.
           MOVE MSEL-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.
       RESET-LEGEND.
           MOVE " " TO LEGEND.
           PERFORM SEND-LEGEND.
       LOAN-CLASS-PROMPT.
           MOVE "S  A110KEYTYPE:S." TO VDU.
           MOVE "LOAN CLASS:" TO VDUBUF.
           PERFORM SCREENIO.
      *SET-SCRATCH-KEY.
      *    MOVE SAVE-SSNO       TO PD-SOC-SEC.
      *    MOVE THE-GROUP-REFBR TO PD-BRANCH PD2-BRANCH.
      *    MOVE "L"             TO PD-ID.
      *-----------------------------------------------------------------
       VERIFY-AGAINST-SPEDT.
           MOVE PD-EFFRATE TO VERIFY-TERMCEIL-EFF.
           MOVE PD-APRATE  TO VERIFY-TERMCEIL-APR.
           MOVE "Y" TO VERIFY-TERMCEIL-EFF-FG.
           MOVE TRANS-DATE TO VERIFY-TODAY-DATE.

           PERFORM SET-BW1-FOR-SPEDT.

           MOVE BW-BDATE   TO VERIFY-AGE.
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
                    PERFORM OPEN-BW1-FILE
                    PERFORM READ-BW1-FILE
                    PERFORM CLOSE-BW1-FILE
                    IF IO-FG = 9
                       MOVE 0 TO BW-BDATE BW-REFDATE
                    END-IF
                 END-IF
              ELSE
                 IF PD-SSNO(1) NOT = BW-SSNO
                    MOVE PD-SSNO(1) TO BW-SSNO
                    PERFORM OPEN-BW1-FILE
                    PERFORM READ-BW1-FILE
                    PERFORM CLOSE-BW1-FILE
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
           MOVE STAT TO CHFG(19) CHFG(20) CHFG(21) CHFG(22)
                        CHFG(23) CHFG(24) CHFG(31) CHFG(32) CHFG(33)
                        CHFG(34) CHFG(35) CHFG(36) CHFG(37) CHFG(38).
           MOVE STAT TO DATE-STAT.

       UNPROTECT-FIELDS.
           MOVE STAT TO CHFG(11) CHFG(14) CHFG(15) CHFG(16) CHFG(17)
                        CHFG(18) CHFG(19) CHFG(20) CHFG(21) CHFG(24)
                        CHFG(31) CHFG(32) CHFG(33) CHFG(38).
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

      *****************************************
       IO-ROUTINE SECTION.
      *****************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBLP/LPSPGS_SQL.CPY".
       COPY "LIBLP/LPLCGS_SQL.CPY".
       COPY "LIBLP/LPICGS_SQL.CPY".
       COPY "LIBLP/LPDLGS_SQL.CPY".
       COPY "LIBLP/LPCLGS_SQL.CPY".
       COPY "LIBLP/LPCDBGS_SQL.CPY".
       COPY "LIBLP/LPCDGS_SQL.CPY".
       COPY "LIBLP/LPBWGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBLP/LPDL1RN.CPY".
       COPY "LIBLP/LPCL1IN.CPY".
       COPY "LIBLP/LPLC1RN.CPY".
       COPY "LIBLP/LPSP1RN.CPY".
       COPY "LIBLP/LPBW1RN.CPY".
       COPY "LIBLP/LPCD1RN.CPY".
       COPY "LIBLP/LPCDB1RN.CPY".
       COPY "LIBLP/LPIC1RN.CPY".
       COPY "LIBLP/LPWKI.CPY".
       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO ERRCD.

      *****************************************
       END-ROUTINE SECTION.
      *****************************************
           PERFORM CLOSE-FILES.
       EXIT-PROG.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY CLQUOT-SCREEN.
           DESTROY CLQUOT-WINDOW-HANDLE.
       STOP-RUN.
           EXIT PROGRAM.
