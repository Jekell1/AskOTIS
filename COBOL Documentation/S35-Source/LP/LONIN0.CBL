       IDENTIFICATION DIVISION.
       PROGRAM-ID.      LONIN0.
       DATE-WRITTEN.    012484.
      ******************************************************************
      *
      *                 (LP)
      *  DESCRIPTION:   DIRECT LOANS (NON SALES FINANCE)
      *                 CONVERSION - PAGE 1
      *
      *=================================================================
      * REV :
      * SLC 042995 ADDED PLTYPE LOGIC.
      * SLC 051295 ADDED INSURANCE CHECK. MUST HAVE A&H AND LIFE IF
      *            BRANCH OPTION IS ON.
      * MJD 081095 ADDED LOGIC FOR GP-THE-ONLY-SF-SRCD
      * <1> MJD 010996 ADDED LOGIC TO COMPUTE COMMISSION ONLY IF
      *            LN-INSOURS() = "Y".
      *            ADDED LOGIC TO PREVENT ENTRY OF "MN_" AS INS COVERAGE.
      * <1> BAH 012596 PASSWORDED 1STPYDATE < 30 DAYS FROM LOANDATE, E097.
      * <1> MJD 031196 ADDED LOGIC TO STOP 0 AS ACCOUNT NUMBER.
      * MJD 032896 ADDED FOUND THAT LONIN0 HAD BEEN REWRITTEN BY LONIJ0
      *            SOMETIME BETWEEN 010996 AND 031196. RESTORED FROM
      *            010996 SAVE FILE AND ADDED ALL CHANGES THAT WERE
      *            IN PROGRAM.DOC (NOTED ABOVE BY <1>).
      *            ADDED SCAN FOR PURPOSE, SOURCE, AND COLLATERAL.
      * MJD 071996 ADDED LOGIC TO PRINT LN-1STPYDATE AS MM/DDWYY
      *            FOR WEEKLY UNIT PERIODS.
      * MJD 101596 ADDED MOVE OF LN-REC TO ORIGINAL-REC WHEN
      *            LOAN IS CALLED FROM SCAN. CHANGED TO ONLY MOVE
      *            SP-JUDGRATE TO LN-JDRATE IF LN-JDDATE = 0.
      *            ADDED F3-LOAN STATUS OPTION TO CALL WI/LNSTAT.C
      * JTG 112196 CORRECTED BUG WITH WRONG SUBSCRIPT ON COMPUTE OF COMMISSION
      *            NEW COPY MEMBERS LIBLP/LPIEXP/W
      *            (IEXP-SUB/SUB1 --> IEXP-LN/IC)
      * KEC 112596 CHANGED LN-POCD FOR RESCISSION.
      * JTG 120296 REMOVED F7-EXIT FROM BOTTOM TO STOP EXITING AFTER F3-STATUS
      * JTG 051297 ADDED USE OF 'SETCTBL' AND 'SETCTBLW'
      * MJD 060397 ADDED SCSCAN-TRANS-DATE.
      * MJD 072397 REQ #163. ADD BRANCH FLAG, BR-AUTO-LNSTAT-WINDOW, TO
      *            AUTOMATICALLY OPEN THE LNSTAT WINDOW.
      * JTG 081497 ADDED SET OF LN-GIFTNET WORLD A90
      * GLF 090897 REPLACED 'IO' ON GP-FILE WITH GET 'GPENV'
      * MJD 112197 EXPANDED LN-REC TO 1800 BYTES
      * BAH 012698 IF INSURED IS "S", TEST SECOND MAKER'S AGE EDITS, PR#641
      *  CS 042398 UPDATE LN-ORIGIN WITH BW-ORIGIN
      * BAH 090198 ADDED SP-INS-ADJ-PRIN-3PMTS, NC ALSO ADDED
      *                  SP-INS-ADJ-CASH-ADVANCE
      * KEC 090498 ADDED 'MOVE LC-LOANTYPE TO LN-LOANTYPE';
      *             FOR REGENCY REVOLVING.
      *  CS 120798 ADD TEST OF GP-I-AM-MERC, DONT ALLOW CONVERTED ACCOUNT
      *            UNLESS IN P&L STATUS
      * JTG 121698 DISPLAYED 'R' VIA UNITPER-MASK LOGIC FOR REVOLVING ACCOUNT
      * GLF 031299 ADD WI/TFCCDS FOR TFC ENTRY OF LPO,REP CODE,PROCESS CODE
      * MJD 990803 ADDED GP-INCL-OTH-IN-EARNED FLAG. IF "Y" THEN INCLUDE
      *            EARNINGS BUCKET 6 (OTHER) IN ACCUMULATED TOTALS
      *            OTHERWISE EXCLUDE BUCKET 6.
      * GLF 990805 ADDED GP-VALID-BDATE. IF "Y" AND THERE IS NOT A VALID
      *            BIRTH DATE FOR SSNO BEING ENTERED, DO NOT ALLOW ENTRY
      *            OF THE SSNO, REGENCY #57
      *  CS 991011 ADDED TEST OF BULK-CONVERSION-FG TO DETERMINE IF NORMAL
      *            CONVERSION OR BULK PURCHASE. IF CONVERSION UPDATE
      *            LN-CONVERCD WITH 'Y', IF BULK UPDATE 'B' AND DONT ALLOW
      *            CHANGES IF BULK-CONVERSION-FG NOT = LN-CONVERCD
      * MJD 991103 O1 INSURANCE WAS NOT PART OF CALCULATED INSURANCE., LAURA
      *            FIXED ALL CODE REGARDING O1 INSURANCE, CALC'ING TERM, ETC.
      * CS  991130 REMOVED TEST OF LN-REPODATE & LN-PLDATE AND ADDED TO LONIF0
      * JTG 000113 ADDED SP-INSREJECT-FG OPTION 'C', WHICH WILL CAUSE WRITING
      *            NO. OF MONTHS INSURANCE NOT IN VIOLATION, WISCONSIN #1032
      * MJD 000208 ADDED GP-OT-INS-TERM (TFC PR#48) IF 'Y' THEN ALLOW
      *            ALLOW TERM ENTRY FOR O2O3O4O5 INSURANCE.
      * KEC 000530 {PR#00048} TFC [JUDY BRANIFF];
      *            ADDED THE FOLLOWING 'IF' CONDITION FOR ENTERING THE
      *            INSURANCE TERM (GP-OT-INS-TERM = "Y"):
      *                  IF ( VDUNUM0 > 360 )
      *                     GO TO ENTER-IC-TERM.
      * JTG 000614 CORRECTED CONVERSION PROBLEM WITH INCORRECT AMOUNTS FOR
      *            LN-ANTICERN, LN-ANTICADJ & LN-ACCERN WHEN REFUNDS ARE
      *            SET VIA WI/LNSTAT.C                             #1203
      * MJD 000912 ADDED LOGIC TO ASK LOCK BOX QUESTION  (MFC PR#227)
      * JTG 010112 ADDED NEW 88 SP-CAL-SET-SMPRATE-OVRRIDE VALUE 4 MERC PR#J016
      * CS  010320 SET DEFAULT LC-POOLID TO LN-DLPOOL
      * CS  010326 ADDED TOTAL-FEEFINCHG AND TOTAL-INSFINCHG LOGIC.  HOWEVER,
      *            THE OPERATOR WILL NEED TO KNOW CORRECT APR AS ITS INPUT
      * JTG 010329 CORRECTED SET OF LN-ANTICERN() AND LN-ACCERN() WHEN
      *            ENTRY OF REBATE FLAG AND REFUND AMOUNTS WERE ENTERED
      *            RE: LN-ANTICERN IS ALWAYS REDUCED BY REFUNDED AMOUNT
      *                FROM IT'S ORIGINAL VALUE            WORLD #258
      * BLV 010810 ADDED CONDITIONAL-SEND-MESS FOR CHANGE TO LPSPEDL,
      *            MERC PR# 253
      * KEC 020621 {PR#01823} REGIONAL ACCEPTANCE <A15> [KEVIN MULLEN]
      *            ADDED LOGIC FOR USING 'CD-FEE-MAXAMT'
      * BLV 020703 FIX BAD IF STATEMENT:
      *           IF ( ELE = 23 OR 25 AND LN-INT-REBATE NOT = " " )
      *      THIS GAVE AN ERROR EVERY TIME SOMEBODY TRIED TO CHANGE
      *      FIELD 23, CHANGED TO
      *           IF ( ELE = 23 AND LN-INT-REBATE NOT = " " ) OR
      *              ( ELE = 25 AND LN-INT-REBATE NOT = " " )
      * BLV 020820 IF GP-ENTER-LPO-FG = "Y", OPEN TFC WINDOW FOR ENTRY OF
      *            LPO, SOAUTO PR# 1885
      * BAH 031001 ADDED SP-INSCHGABLE '2' & '1',SP-INS-AMTFIN-2PMTS &
      *             SP-INS-AMTFIN-1PMTS FOR SIGNAL (WISC) PR# 2157
      *  CS 041108 CHECK FOR GI WHEN ENTERING CLASS.. IF GI DOES NOT EXIST
      *            DONT ALLOW ANY FURTHER PROCESSING [SUPPORT PR#451]
      * BAH 050222 WHEN ENTERRING SUB CLASS, YOU DONT KNOW THE LOANDATE YET TO
      *            CHECK IF SPR IS EXPIRED, SO IT WAS JUST GRABBING THE FIRST
      *            LAWCODE FOUND TO DISPLAY THE DESCRIPTION, WHICH ISNT
      *            CORRECT IF THEY EXPIRE AND BUILD A NEW ONE WITH A DIFFERENT
      *            DESCRIPTION, VERY CONFUSING...AT LEAST SKIP EXPIRED SPR.
      *            RIVERSIDE PL# 467
      *  CS 050425 IF INSURANCE PREMIUM = ZERO, ZERO THE INSCOMP,INSCOMM,
      *            EFFDATE,EXPDATE, AND OUR FLAG[PR#479 NSA]
      * BAH 051128 UNDO CHANGE FROM 050222 PL# 467, WOULD NOT LET YOU BOOK A
      *            LOAN WITH AN EXPIRED SPR, AND YOU SHOULD BE ABLE TO.
      *            INSTEAD OF SKIPPING EXPIRED SPR IN READ-NEXT, JUST REDISPLAY
      *            THE SUBCLASS DESCRIPTION AFTER ENTERING THE LOANDATE.
      * KEC 060721 PR#00127 REGIONAL ACCEPT. ADDED LOGIC FOR "AUTOPC".
      * BAH 090821 ADDED SP-INSCHGABLE "E" FOR REGMGM PR# 3437
      *  CS 150922 MISSING FIELD COMING OUT OF FEEENT-WIN, VDU-FORM-NAME NOT
      *            SET.FIXED SSNO ENTRY
      * BAH 160118 SPLIT OUT LTFILE, PD0003
      * KEC 2016.0309 CHANGED CD-GROUP, CD-SECURED & CD-TRW TO BE
      *               CD-CO-GROUP, CD-CO-SECURED & CD-CO-TRW.
      * KEC 2016.0314 CHANGED CD-FEE???? TO CD-GC-????.
      * KEC 2016.0325 CHANGED CDSCAN-WINDOW-BUF TO HAVE MATCHING CD1-KEY
      *         LAYOUTS (FOR EACH CODE TYPE; CD-TYPE).
      *         CDSCAN-CD1-KEY IS THE NEW (PRIMARY) WITH REDEFINES OF
      *         CDSCAN-??-KEY (NEW).
      * KEC 2016.0329 CHANGED ELSCAN WORKERS TO MATCH CD1-KEY FOR EL LAYOUT.
      * MJD 160707 REMOVED MORELN, LN-SEQNO, CORRECTED FORM TO REMOVE SEQNO
      *            AND USE 8 DIGIC ACCTNO
      *  CS 160805 CHANGED SCSCAN-KEY TO SCSCAN-SP1-KEY
      * BLM 161227 CHANGED ORIG & CHANGED RECS FROM 1800 TO LN-SIZE
      * KEC 2017.0526 ADDED 'LN' FILE PREFIX FOR WORKING STORAGE FIELDS.
      * BAH 170612 ACTIVATE 8 DIGIT DATES, PD0006
      * BAH 171204 UPDATE LN-GIFTNET WITH "X" IF SP-NON-LOAN-FLAG = "Y"
      *            FOR WORLD TAX ADVANCE LAONS TO NOT BE CONSIDERED DUAL
      *            LOANS, AND IF SP-SKIP-TRW = "Y", SET LN-SKIP-TRW TO "Y"
      *                                            WORLD PR#1288
      * BAH 181001 GLOBAL BWFILE
      *  CS 181205 GLOBAL LTC
      * BAH 181210 GLOBAL LNFILE
      * BAH 190122 SPLIT CDFILE
      * BAH 190215 CHANGED WAY COLLATERAL DISPLAYED
      * BAH 20190625 REMOVED LC-REVOLVING, PT#0153
      * BAH 20190625 REMOVED LN-REVOLVING, PT0153
      * BAH 20200617 CHANGED PC-LT-RECORD TO PC-LTA-RECORD, ADDED LPPCCL CLEAR
      * BAH 20200615 MOVED OPEN-IC1-FILE, OPEN-SP1-FILE, OPEN-CD1-FILE,
      *              OPEN-BW1-FILE TO TOP OF PROGRAM #TST001
      * BAH 20210713 PASSWORD FIELD 31, OTHER 2 INSURANCE #1504
      * BAH 20220803 HARD CODED START
      * BAH 20230713 COMMENT OUT 3RD AND 4TH BORROWER TESTS, LN6 AND LN7
      * BAH 2024.0131 DELETE NOTICE IF EXIST
      * BAH 2025.0407 CLEAR 3RD LEVEL OF AUTO COLLATERAL, WK-AUTO-3 S35Q-194
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
           "@(#)LP/LONIN0.CBL S35 02/20/24-15:24:40 >".
      **********************************
      *       F I L E   I F N          *
      **********************************
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBGL/GL01GI.CPY".
       COPY "LIBGL/GL01GI_SQL.CPY".
       COPY "LIBGL/GLWSGI.CPY".
       COPY "LIBLP/LP01DL.CPY".
       COPY "LIBLP/LP01DL_SQL.CPY".
       COPY "LIBLP/LPWSDL.CPY".
       COPY "LIBLP/LP01LC.CPY".
       COPY "LIBLP/LP01LC_SQL.CPY".
       COPY "LIBLP/LPWSLC.CPY".
       COPY "LIBLP/LP01LN.CPY".
       COPY "LIBLP/LP01LN_SQL.CPY".
       COPY "LIBLP/LPWSLN.CPY".
       COPY "LIBLP/LP01LP.CPY".
       COPY "LIBLP/LP01LP_SQL.CPY".
       COPY "LIBLP/LPWSLP.CPY".
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
       COPY "LIBLP/LP01LTA.CPY".
       COPY "LIBLP/LP01LTA_SQL.CPY".
       COPY "LIBLP/LPWSLTA.CPY".
       COPY "LIBLP/LP01LTC.CPY".
       COPY "LIBLP/LP01LTC_SQL.CPY".
       COPY "LIBLP/LPWSLTC.CPY".
       COPY "LIBLP/LP01NO.CPY".
       COPY "LIBLP/LP01NO_SQL.CPY".
       COPY "LIBLP/LPWSNO.CPY".
       COPY "LIBLP/LP01PC.CPY".
       COPY "LIBLP/LP01PC_SQL.CPY".
       COPY "LIBLP/LPWSPC.CPY".
       01  WK-IFN          PIC X(4) VALUE "A2WK".
       01  WK-KEY          PIC 9(2).

       01  MSEL-LIST       PIC X(5)    VALUE "MSEL.".
       01  WSEL-LIST       PIC X(5)    VALUE "WSEL.".
       01  ONE             PIC S9.
       01  ELE             PIC 9(6).
       01  ACTION.
           03  CHKEY       PIC X(4).
           03  CHNO        PIC 9(2).
       01  IO-TYPE         PIC X(3).
       01  NEW-FLAG        PIC 9       VALUE 0.
       01  CDCODE-FEE-FG   PIC 9       VALUE 0.
       01  FIRST-ELE       PIC 99      VALUE 0.
       01  HOLD-LN1-KEY    PIC 9(12).
       01  ORIGINAL-LN-REC PIC X(LN-SIZE).
       01  CHANGED-LN-REC  PIC X(LN-SIZE).
       01  LAST-READ       PIC X(4).
       01  SAVE-BW-KEY     PIC X(32).
       01  VALID-DELETE    PIC X.
       01  SAVE-SSNO       PIC 9(10)     COMP-3.
       01  AMT-HOLDER      PIC 9(6)V99   COMP-3.
       01  PREM-CHANGE     PIC X VALUE " ".
       01  PREM-AMOUNT     PIC 9(4)V99   COMP-3 VALUE 0.

       01  SUB             PIC 9(6) COMP-3.
       01  SUB2            PIC 9(6) COMP-3.
       01  SP-SUB          PIC 9(6) COMP-3.
       01  LN-SUB          PIC 9(6) COMP-3.
       01  OVER-20         PIC 99.
       01  I-LEVEL         PIC 9(6) COMP-3.
       01  INSPECT-W       PIC X(86).
       01  INSPECT-X REDEFINES INSPECT-W.
           03  INSPECT-Z   PIC X OCCURS 86 TIMES.
       01  ADD-ON          PIC X(25).
       01  OUR-CHGS        PIC 9(6)V99 COMP-3.

       01  LC-INFO-LIST      PIC X(10)   VALUE "37P:S 370.".
       01  LC-INFO-BUF       PIC X(14).

       01  FIRST-SCAN        PIC 9     VALUE 0.
       01  SCAN-FLAG         PIC 9     VALUE 0.
       01  SCAN              PIC X(2)  VALUE SPACES.
           88  SCAN-ON       VALUE "ON".
       01  TYP               PIC 9     VALUE 0.
       01  ALLOT-BUF.
           03  FILLER        PIC X(5).
           03  ALLOT-CODE    PIC X.
       01  LBOX-BUF.
           03  FILLER        PIC X(5).
           03  LBOX-CODE     PIC X.

       01  MEMO-WK.
           03  MEMO-WK1    PIC X(30).
           03  MEMO-WK2    PIC X(30).

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
       01  BW-ALP-TABLE.
           03  BW-ALP-TBL  OCCURS 70 TIMES
                           PIC X.

       01  COLLATERAL-BREAKOUT.
           03 COLLATERAL-30-1    PIC X(30).
           03 COLLATERAL-30-2    PIC X(30).
           03 FILLER             PIC X(20).

       01  W-LN.
           03  W-ACCOUNT  PIC 9(6).
           03  W-SEQUENCE PIC 9(2).
       01  W-LNNO REDEFINES W-LN
                          PIC 9(8).

      ********************************
      *   S C R E E N   B U F F E R  *
      ********************************
       01  DISPLAY-BUF1        VALUE " ".
           03  D-LNNO          PIC 9(8).
           03  D-P             PIC XX.
           03  D-PLTYPE        PIC XX.
           03  D-J             PIC X(3).
           03  D-LCNO          PIC 99.
           03  D-LC-DESC       PIC X(16).
           03  D-LNAMT         PIC ZZZZZ9.99.
           03  D-PROCEEDS      PIC ZZZZZ9.99.
           03  D-010-040       OCCURS 4.
               05  D-010       PIC X(11).
               05  D-011       PIC X(33).
               05  D-012       PIC X(1).
           03  D-050           PIC XX.
           03  D-051           PIC 9(4)      BLANK ZERO.
           03  D-052           PIC 99        BLANK ZERO.
           03  D-053           PIC X(15).
           03  D-060           PIC 99/99/99  BLANK ZERO.
           03  D-070           PIC X(3).
           03  D-071           PIC X(15).
           03  D-080           PIC X(3).
           03  D-081           PIC X(15).
           03  D-090           PIC X(3).
           03  D-091           PIC X(15).
           03  D-100           PIC X(20).
           03  D-110           PIC X(30).
           03  D-111           PIC X(30).
       01  DISPLAY-BUF2        REDEFINES DISPLAY-BUF1.
           03  D-370           PIC ZZ.
           03  D-120           PIC 9(4).99   BLANK ZERO.
           03  D-121           PIC X(16).
           03  D-130           PIC ZZZ.
           03  D-140           PIC ZZ.
           03  D-150           PIC 99/99/99  BLANK ZERO.
           03  D-160           PIC 99/99/99  BLANK ZERO.
      *--->03  D-170           PIC 99/99/99  BLANK ZERO.
           03  D-170           PIC X(8).
           03  D-180           PIC ZZ9.999   BLANK ZERO.
           03  D-190           PIC ZZ9.999   BLANK ZERO.
           03  D-200           PIC ZZ9.999   BLANK ZERO.
           03  D-210           PIC Z(5)9.99  BLANK ZERO.
           03  D-220           PIC Z(5)9.99  BLANK ZERO.
           03  D-230           PIC Z(3)9.99  BLANK ZERO.
           03  D-240           PIC Z(3)9.99  BLANK ZERO.
           03  D-241           PIC ZZZ.99     BLANK ZERO.
           03  D-250           PIC Z(5)9.99  BLANK ZERO.
           03  D-260           PIC XXX.
           03  D-270-290       OCCURS 4.
               05  D-1COMP      PIC 99        BLANK ZERO.
               05  D-1IOURS     PIC X.
               05  D-1TERM      PIC ZZ9       BLANK ZERO.
               05  D-1COVR      PIC Z(5)9.99  BLANK ZERO.
               05  D-1PREM      PIC Z(3)9.99  BLANK ZERO.
           03  D-300-310        OCCURS 2.
               05  D-2COMP      PIC 99        BLANK ZERO.
               05  D-2IOURS     PIC X.
               05  D-2EXP       PIC 9(6)       BLANK ZERO.
               05  D-2TERM  REDEFINES D-2EXP
                                PIC Z(6)       BLANK ZERO.
               05  D-2COVR      PIC Z(5)9.99  BLANK ZERO.
               05  D-2PREM      PIC Z(3)9.99  BLANK ZERO.
           03  D-330-340.
               05  D-330-340X       OCCURS 2.
                   07  D-1FEECD     PIC XX.
                   07  D-1FEEAMT    PIC Z(3)9.99  BLANK ZERO.
                   07  D-2FEECD     PIC XX.
                   07  D-2FEEAMT    PIC Z(3)9.99  BLANK ZERO.
           03  D-350            PIC 9(6)      BLANK ZERO.
           03  D-360            PIC Z(5)9.99  BLANK ZERO.
       01  DISPLAY-BUF3 REDEFINES DISPLAY-BUF1.
           03  D-X010-X040      OCCURS 4.
               05  D-X010       PIC X(11).
               05  D-X011       PIC X(33).
       01  DEF-BUF REDEFINES DISPLAY-BUF1.
           03  DEF-ORGST       PIC XX.
           03  DEF-ORGBR       PIC 9(4).
           03  DEF-PAYFREQ     PIC ZZ.

       01  DEF-LIST            PIC X(12) VALUE
           "050 051 140.".

       01  D-BW-WK.
           03  FILLER          PIC X(23).
           03  D-BW-PROMPT     PIC X(8).
           03  D-BW-LOANS      PIC 99.

       01  DISPLAY-LIST1.
           03  FILLER          PIC X(30)  VALUE
           "LNNO PJ LNKEY LCDESC PRIN FIN ".
           03  FILLER          PIC X(40)  VALUE
           "010 011 012 020 021 022 030 031 032 040 ".
           03  FILLER          PIC X(56)  VALUE
           "041 042 050 051 052 053 060 070 071 080 081 090 091 ".
           03  FILLER          PIC X(12)  VALUE
           "100 110 111.".
       01  DISPLAY-LIST2.
           03  FILLER          PIC X(4)   VALUE
           "370 ".
           03  FILLER          PIC X(52)  VALUE
           "120 121 130 140 150 160 170 180 190 200 210 220 230 ".
           03  FILLER          PIC X(52)  VALUE
           "240 241 250 260 270 271 272 273 274 280 281 282 283 ".
           03  FILLER          PIC X(52)  VALUE
           "284 290 291 292 293 294 300 301 302 303 304 310 311 ".
           03  FILLER          PIC X(56)  VALUE
           "312 313 314 320 321 322 323 324 330 331 332 333 340 341 ".
           03  FILLER          PIC X(16)  VALUE
           "342 343 350 360.".
       01  DISPLAY-LIST3.
           03  FILLER          PIC X(32) VALUE
           "010 011 020 021 030 031 040 041.".
       01  FEE-LIST.
           03  FILLER           PIC X(16)  VALUE
           "330 331 332 333 ".
           03  FILLER           PIC X(16)  VALUE
           "340 341 342 343.".

      ********************************
      *   E L E M E N T   S P E C S  *
      ********************************
       01  MAX             PIC 99   VALUE 37.
       01  SPEC-TABLE.
      * 1  LN-SSNO(1)
           03  FILLER      PIC X(8) VALUE "ERNX090 ".
      * 2  LN-SSNO(2)
           03  FILLER      PIC X(8) VALUE "ERNX090 ".
      * 3  LN-SSNO(3)
           03  FILLER      PIC X(8) VALUE "ERNX090 ".
      * 4  LN-SSNO(4)
           03  FILLER      PIC X(8) VALUE "ERNX090 ".
      * 5  LN-ORGST
           03  FILLER      PIC X(8) VALUE "E  A020 ".
      * 6  LN-LOANDATE
           03  FILLER      PIC X(8) VALUE "ERNM060 ".
      * 7  LN-PURCD
           03  FILLER      PIC X(8) VALUE "E  A030 ".
      * 8  LN-SRCD
           03  FILLER      PIC X(8) VALUE "E  A030 ".
      * 9  LN-COLLCD
           03  FILLER      PIC X(8) VALUE "E  A030 ".
      * 10 LN-COLLID
           03  FILLER      PIC X(8) VALUE "E  A200 ".
      * 11 LT-COLLATERAL-INFO
           03  FILLER      PIC X(8) VALUE "E  A300 ".
      * 12 LN-DLNO
           03  FILLER      PIC X(8) VALUE "ENNN042 ".
      * 13 LN-ORGTERM
           03  FILLER      PIC X(8) VALUE "ENNN030 ".
      * 14 LN-PAYFREQ
           03  FILLER      PIC X(8) VALUE "ENNN020N".
      * 15 LN-FILEDATE
           03  FILLER      PIC X(8) VALUE "ERNM060 ".
      * 16 LN-INTDATE
           03  FILLER      PIC X(8) VALUE "ERNM060 ".
      * 17 LN-1STPYDATE
           03  FILLER      PIC X(8) VALUE "ERNM060 ".
      * 18 LN-APRATE
           03  FILLER      PIC X(8) VALUE "ENNN033 ".
      * 19 LN-OVRDRATE
           03  FILLER      PIC X(8) VALUE "ENNN033 ".
      * 20 LN-ENDORATE
           03  FILLER      PIC X(8) VALUE "ENNN033 ".
      * 21 LN-REGPYAMT
           03  FILLER      PIC X(8) VALUE "ENNN062 ".
      * 22 LN-1STPYAMT
           03  FILLER      PIC X(8) VALUE "ENNN062 ".
      * 23 LN-EXTCHG
           03  FILLER      PIC X(8) VALUE "ENNN042 ".
      * 24 LN-SERCHG
           03  FILLER      PIC X(8) VALUE "ENNN042 ".
      * 25 LN-INTCHG
           03  FILLER      PIC X(8) VALUE "ENNN062 ".
      * 26 LN-INSURED
           03  FILLER      PIC X(8) VALUE "E  A030 ".
      * 27 LN-INSCOMP(1)
           03  FILLER      PIC X(8) VALUE "ENNN020 ".
      * 28 LN-INSCOMP(2)
           03  FILLER      PIC X(8) VALUE "ENNN020 ".
      * 29 LN-INSCOMP(3)
           03  FILLER      PIC X(8) VALUE "ENNN020 ".
      * 30 LN-INSCOMP(4)
           03  FILLER      PIC X(8) VALUE "ENNN020 ".
      * 31 LN-INSCOMP(5)
           03  FILLER      PIC X(8) VALUE "ENNN020 ".
      * 32 LN-INSCOMP(6)
           03  FILLER      PIC X(8) VALUE "ENNN020 ".
      * 33 LN-FEEGLNO(1), (2)
           03  FILLER      PIC X(8) VALUE "E  A020 ".
      * 34 LN-FEEGLNO(3), (4)
           03  FILLER      PIC X(8) VALUE "E  A020N".
      * 35 LN-PRLNNO
           03  FILLER      PIC X(8) VALUE "ENNN060 ".
      * 36 LN-ANTICERN(1)
           03  FILLER      PIC X(8) VALUE "ENNN062N".
      * 37 LN-LC-CODE
           03  FILLER      PIC X(8) VALUE "ENNN020 ".
       01  SPEC-TABLE2     REDEFINES SPEC-TABLE.
           03  SPEC-REC    OCCURS 37 TIMES.
               05  SPEC    PIC X(7).
               05  CHFG    PIC X.

       COPY "LIBLP/LPIEXPW.CPY".
       COPY "LIBLP/LPWKPATH.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBCL/CREAIXW.CPY".
       COPY "LIBWI/PFSWINDOWSW.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBGB/SYSTEMW.CPY".
       COPY "LIBLP/LPANTIW.CPY".
       COPY "LIBLP/LPITRMW.CPY".
       COPY "LIBLP/LPAMTSW.CPY".
       COPY "LIBLP/LPMNUIW.CPY".
       COPY "LIBGB/GPENVW.CPY".
       COPY "LIBLP/LPSPEDW.CPY".
       COPY "LIBLP/LPBAGEW.CPY".
       COPY "LIBLP/LPAPRSW.CPY".
       COPY "LIBLP/SETCTBLW.CPY".
       COPY "LIBLP/LPAPRZW.CPY".
       COPY "LIBLP/LPUPERW.CPY".
       COPY "LIBLP/LPEDAYW.CPY".
       COPY "LIBWI/ELSCANW.CPY".
       COPY "LIBWI/SCSCANW.CPY".
       COPY "LIBWI/CDSCANW.CPY".
       01  WINDOW-WORKERS.
           03  WINDOW-LPO          PIC X(4).
           03  WINDOW-REP          PIC X(8).
           03  WINDOW-PROCESS      PIC X(1).
       COPY "LIBWI/CDBSCNW.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBWI/FEEENTW.CPY".
       COPY "LIBGB/PASSWDW.CPY".
       01  1STPY30-SEQ          PIC 9 VALUE 0.
       01  O2-SEQ               PIC 99 VALUE 27.
       COPY "LIBWI/AUTOPCW.CPY".
       COPY "LIBWI/LTCREAW.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBLP/LONIN_DEF.CPY".
       COPY "LIBLP/LPCMNU_DEF.CPY".
       COPY "LIBWI/FEEENT_DEF.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBLP/LONIN_WKS.CPY".
       COPY "LIBLP/LPCMNU_WKS.CPY".
       COPY "LIBWI/FEEENT_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".

       COPY "LIBGB/GETFMW.CPY".
       COPY "LIBLP/LPLONIW.CPY".

       SCREEN SECTION.
       COPY "LIBLP/LONIN_SCN.CPY".
       COPY "LIBLP/LPCMNU_SCN.CPY".
       COPY "LIBWI/FEEENT_SCN.CPY".

       PROCEDURE DIVISION
           USING FORM-PATHNAME EXIT-PATHNAME PROG-BUF.
       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
               WK-FILE.
       COPY "LIBGB/DECLARE.CPY".
           PERFORM CLOSE-SP1-FILE.
           PERFORM CLOSE-PC1-FILE.
           PERFORM CLOSE-NO1-FILE.
           PERFORM CLOSE-LTC1-FILE.
           PERFORM CLOSE-LTA1-FILE.
           PERFORM CLOSE-LP1-FILE.
      *    PERFORM CLOSE-LN7-FILE.
      *    PERFORM CLOSE-LN6-FILE.
           PERFORM CLOSE-LN5-FILE.
           PERFORM CLOSE-LN4-FILE.
           PERFORM CLOSE-LN1-FILE.
           PERFORM CLOSE-LC1-FILE.
           PERFORM CLOSE-IC1-FILE.
           PERFORM CLOSE-DL1-FILE.
           PERFORM CLOSE-CDB1-FILE.
           PERFORM CLOSE-CD1-FILE.
           PERFORM CLOSE-BW4-FILE.
           PERFORM CLOSE-BW3-FILE.
           PERFORM CLOSE-BW2-FILE.
           PERFORM CLOSE-BW1-FILE.
           PERFORM CLOSE-GI1-FILE.
           PERFORM CLOSE-BR-FILE.
           CLOSE WK-FILE.
       COPY "LIBGB/DECLAR2.CPY".

      ***********************************
      *   C O N T R O L   M O D U L E   *
      ***********************************
       MAIN-PROGRAM SECTION.

           PERFORM SQL-CONNECT.

      D    STOP MESS.
           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "LONIN" TO VDU-FORM-NAME.

           MOVE USER-PID TO TEMP-PID.
           MOVE " " TO ERRCD.

           MOVE BRANCH TO BR-NO.
           PERFORM OPEN-BR-FILE.
           PERFORM READ-BR-FILE.
           PERFORM CLOSE-BR-FILE.
           IF IO-FG NOT = 0
              GO TO IO-ERROR.

           PERFORM GET-GPENV.

           PERFORM OPEN-IC1-FILE.
           PERFORM OPEN-CD1-FILE.
           PERFORM OPEN-SP1-FILE.
           PERFORM OPEN-BW1-FILE.

       MAIN-MODULE.
           PERFORM CREATE-TRAILER.

           PERFORM FORM-RESET.
           PERFORM DISPLAY-BULK-CONVERSION.

           MOVE " " TO MESS.
           PERFORM MSG-LINE.

           IF BR-LC-CODE = "Y"
              MOVE " " TO CHFG(37)
              MOVE "37. LC CODE:__" TO LC-INFO-BUF
           ELSE
              MOVE "N" TO CHFG(37)
              MOVE " " TO LC-INFO-BUF.
           MOVE LC-INFO-BUF TO WR-BUF.
           MOVE LC-INFO-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

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

      ************************************
       ENTER-KEYS SECTION.
      ************************************
       LOAN-GET-NUMBER.
           MOVE 0 TO ELE NEW-FLAG.
           MOVE SPACES TO IO-TYPE.
           IF LOAN-KEY NOT = 0
              MOVE LOAN-KEY TO LN-ACCTNO
              MOVE 0 TO LOAN-KEY
              MOVE "SN N060LNNO." TO VDU
              MOVE LN-ACCTNO TO VDUNUM0
              PERFORM SCREENIO
              GO TO START-LN-LOOKUP.
           MOVE "F7-MENU, F6-SCAN, ENTER LOAN NO.:" TO MESS.
           PERFORM SEND-LEGEND.
           MOVE "EN N060LNNO." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>"
              GO TO MENU-KEY.
           IF VDUSTATUS = "1<"
              PERFORM ENTER-SCANKEY
              GO TO CHECK-SALEFIN.
           IF NOT (VDUSTATUS = "00" OR "16")
              GO TO LOAN-GET-NUMBER.
           IF VDUNUM0 = 0
              GO TO LOAN-GET-NUMBER.
           MOVE VDUNUM0 TO LN-ACCTNO.
      * DONT STOP ON SEQNO......ONLY IF F4 FROM CLASS
      *                         OR F3 WAS DEPRESSED
           MOVE 0 TO NEW-FLAG.

      * VVVVV REMOVED AS PART OF REMOVING MORELN/SEQNO
      *     IF VDUSTATUS NOT = "16"
      *        GO TO LOAN-SEND-SEQ.
      *
      * LOAN-GET-SEQ.
      *     MOVE 0 TO NEW-FLAG.
      *     MOVE "F1-CLEAR, ENTER LOAN SEQ NO:" TO MESS.
      *     PERFORM SEND-LEGEND.
      *     MOVE "EN N020LNSQ." TO VDU.
      *     PERFORM SCREENIO.
      *     IF VDUSTATUS = "10"
      *        MOVE "CLR" TO IO-TYPE
      *        GO TO EXIT-ENTER-KEYS.
      *     IF VDUSTATUS = "1U"
      *        GO TO LOAN-GET-NUMBER.
      *     IF VDUSTATUS NOT = "00"
      *        GO TO LOAN-GET-SEQ.
      *     IF VDUNUM0 = 0
      *        MOVE "SN N020LNSQ." TO VDU
      *        MOVE 1 TO VDUNUM0
      *        PERFORM SCREENIO.
      *     MOVE VDUNUM0 TO LN-SEQNO.
      *     IF VDUNUM0 NOT = 1
      *        MOVE "SEQUENCE NUMBER SHOULD BE A 1" TO MESS
      *        PERFORM SEND-MESS.
      *     MOVE VDUNUM0 TO LN-SEQNO.
      *     GO TO START-LN-LOOKUP.
      * LOAN-SEND-SEQ.
      *     MOVE 0 TO NEW-FLAG.
      *     MOVE "SN N020LNSQ." TO VDU.
      *     MOVE 1 TO VDUNUM0 LN-SEQNO.
      *     PERFORM SCREENIO.
      * ^^^^^ REMOVED AS PART OF REMOVING MORELN/SEQNO

       START-LN-LOOKUP.
           MOVE SPACES TO MESS.
           PERFORM SEND-LEGEND.
           PERFORM OPEN-LN1-FILE.
           PERFORM READ-LN1-FILE.
           MOVE LN1-KEY TO HOLD-LN1-KEY.
           PERFORM CLOSE-LN1-FILE.
           IF IO-FG = 9
              PERFORM NEW-RECORD-SETUP
              GO TO ENTER-CLASS.

           IF ( LN-POCD-VOID )
              MOVE "ACCOUNT HAS BEEN DELETED" TO MESS
              PERFORM SEND-MESS
              GO TO LOAN-GET-NUMBER.

           IF ( LN-POCD-INRESCISSION )
              MOVE "ACCOUNT IS IN RESCISSION" TO MESS
              PERFORM SEND-MESS
              GO TO LOAN-GET-NUMBER.

           IF ( LN-POCD-RESCIND )
              MOVE "ACCOUNT HAS BEEN RESCINDED" TO MESS
              PERFORM SEND-MESS
              GO TO LOAN-GET-NUMBER.

      *---{CS}----
           IF BULK-CONVERSION-FG = "B"
              IF LN-CONVERCD NOT = "B"
                 MOVE "ACCOUNT NOT INPUT AS BULK PURCHASE ACCOUNT"
                                                           TO MESS
                 PERFORM SEND-MESS
                 GO TO LOAN-GET-NUMBER.

           IF BULK-CONVERSION-FG = "C"
              IF LN-CONVERCD NOT = "Y"
                 MOVE "ACCOUNT NOT INPUT AS CONVERSION ACCOUNT"
                                               TO MESS
                 PERFORM SEND-MESS
                 GO TO LOAN-GET-NUMBER.
      *--{CS}---

      * MUST BE HERE BECAUSE DISPLAY-FIELDS CHANGES VALUE OF
      * LN-INSTRM
           MOVE LN-REC TO ORIGINAL-LN-REC.

           PERFORM DISPLAY-FIELDS.
           GO TO CHECK-SALEFIN.
       ENTER-CLASS.
           MOVE "EN N020LNKEY." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO EXIT-ENTER-KEYS.
           IF VDUSTATUS = "1>"
              GO TO MENU-KEY.
           IF VDUSTATUS = "1U"
              GO TO LOAN-GET-NUMBER.
           IF VDUSTATUS NOT = "00"
              GO TO ENTER-CLASS.
           MOVE VDUNUM0 TO LN-CLASS
                           LN-SPRCLASS
                           LC-CODE.
           PERFORM GET-LC.
           IF IO-FG = 9
              PERFORM NOT-FOUND
              GO TO ENTER-CLASS.
           MOVE "S  A000LCDESC." TO VDU.
           MOVE LC-DESC TO VDUBUF.
           PERFORM SCREENIO.
           IF LC-SALEFIN = "Y"
              PERFORM NOT-DIRECT
              GO TO ENTER-CLASS.

           MOVE LC-SMALLOAN TO LN-SMALLOAN.
           MOVE LC-SALEFIN  TO LN-SALEFIN.
           MOVE LC-SERVACCT TO LN-SERVACCT.
           MOVE LC-LOANTYPE TO LN-LOANTYPE.
           MOVE LC-GIFTNET  TO LN-GIFTNET.

      * SET DEFAULT POOLID FROM CLASS FILE MERC(03/16/01)
           IF NOT (LC-POOLID = 20202 OR 02020)
              MOVE LC-POOLID TO LN-DLPOOL.

      * CHECK FOR GI RECORD FOR CLASS ENTERED
            MOVE LC-CODE   TO GI-CLASS.
            MOVE BR-NO    TO GI-BRANCH.
            PERFORM GET-GI.
            IF IO-BAD
              PERFORM GI-NOT-FOUND
              GO TO ENTER-CLASS.

           MOVE 1 TO LN-STMT-COUNTER.
           GO TO EXIT-ENTER-KEYS.
       MENU-KEY.
           IF LN-ACCTNO NUMERIC
              MOVE LN-ACCTNO TO LOAN-KEY.

           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY LONIN-SCREEN.

           PERFORM MENU-SELECTION.
           IF MENU-FG = 99
              GO TO LOAN-GET-NUMBER.
           MOVE "END" TO IO-TYPE.
           GO TO EXIT-ENTER-KEYS.
       CHECK-SALEFIN.
           IF LC-SALEFIN = "Y"
              PERFORM NOT-DIRECT
              MOVE "CLR" TO IO-TYPE.
       EXIT-ENTER-KEYS.

      ***********************************
       ENTER-SCANKEY SECTION.
      ***********************************
       ENTER-SCAN.
           MOVE 1 TO SCAN-FLAG.
           MOVE "NEXT" TO LAST-READ.
           MOVE "N" TO VALID-LOAN.
           PERFORM OPEN-LN1-FILE.

           MOVE BW-PATH-OWNBR TO BW-OWNBR.

           MOVE "1-SS#, 2-NAME, 3-SP SS#, 4-SP NAME" TO MESS.
           PERFORM SEND-LEGEND.
       ENTER-TYPE.
           MOVE "EN N010TYP." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO CLOSE-SCANKEY.
           IF VDUSTATUS = "1<"
              MOVE "S" TO VDUACTION
              MOVE 1 TO TYP VDUNUM0
              PERFORM SCREENIO
              PERFORM DISPLAY-TYPE
              GO TO READ-FILES-NEXT.
           IF VDUSTATUS NOT = "00"
              GO TO ENTER-TYPE.
           MOVE VDUNUM0 TO TYP.
           IF TYP < 1 OR TYP > 4
              GO TO ENTER-TYPE.
       DISPLAY-TYPE.

           IF TYP = 1
              MOVE 0 TO BW-SSNO
              PERFORM START-BW1-FILE
              MOVE "SS#, F5-SCAN LN, F6-SCAN SS#" TO MESS.
           IF TYP = 2
              MOVE SPACES TO BW-NAME
              PERFORM START-BW2-FILE
              MOVE "LAST NAME, F5-SCAN LN, F6-SCAN NAME" TO MESS.
           IF TYP = 3
              MOVE 0 TO BW-SPSSNO
              PERFORM START-BW3-FILE
              MOVE "SP SS#, F5-SCAN LN, F6-SCAN SP SS#" TO MESS.
           IF TYP = 4
              MOVE SPACES TO BW-SPNAME
              PERFORM START-BW4-FILE
              MOVE "SP NAME, F5-SCAN LN, F6-SCAN SP NAME" TO MESS.
           PERFORM SEND-LEGEND.
       ENTER-KEY.
           MOVE "E RA000SORTKEY." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO CLOSE-SCANKEY.
           IF VDUSTATUS = "1U"
              PERFORM CLOSE-SCANKEY
              GO TO ENTER-SCAN.
           IF VDUSTATUS = "1D"
              IF FIRST-SCAN = 0
                 MOVE " ENTER BORROWER # FIRST" TO MESS
                 PERFORM SEND-MESS
                 GO TO ENTER-KEY.
           IF VDUSTATUS = "1D"
              PERFORM SCAN-LOANS
              IF IO-TYPE = "CLR"
                 MOVE SPACES TO IO-TYPE
                 GO TO ENTER-KEY
              ELSE
                 GO TO ENTER-KEY.
           IF VDUSTATUS = "1<"
              IF LAST-READ = "NEXT"
                 GO TO READ-FILES-NEXT
              ELSE
                 MOVE SAVE-BW-KEY TO VDUBUF
                 GO TO START-FILES.
           IF VDUSTATUS NOT = "00"
              GO TO ENTER-KEY.
           IF VDUBUF = SPACES
              IF VALID-LOAN = "Y"
                 MOVE "   " TO IO-TYPE
                 GO TO CLOSE-SCANKEY
              ELSE
                 GO TO ENTER-KEY.
       START-FILES.

           MOVE BW-PATH-OWNBR TO BW-OWNBR.

           IF TYP = 1 OR 3
              MOVE VDUBUF TO BW-SSNO BW-SPSSNO
           ELSE
              IF TYP = 2 OR 4
                 PERFORM ARRANGE-BW-KEY
                 MOVE BW-ALP-TABLE TO BW-NAME BW-SPNAME. 
           IF TYP = 1
              PERFORM START-BW1-FILE
           ELSE
              IF TYP = 2
                 PERFORM START-BW2-FILE
              ELSE
                 IF TYP = 3
                    PERFORM START-BW3-FILE
                 ELSE
                    IF TYP = 4
                       PERFORM START-BW4-FILE.
           MOVE 0 TO SCAN-FLAG.
       READ-FILES-NEXT.
           MOVE 0 TO FIRST-SCAN.
           IF TYP = 1 PERFORM READ-BW1-FILE-NEXT.
           IF TYP = 2 PERFORM READ-BW2-FILE-NEXT.
           IF TYP = 3 PERFORM READ-BW3-FILE-NEXT.
           IF TYP = 4 PERFORM READ-BW4-FILE-NEXT.
           IF (IO-FG = 9) OR (BW-OWNBR NOT = BW-PATH-OWNBR)
              MOVE "CLR" TO IO-TYPE
              GO TO CLOSE-SCANKEY.

           MOVE BW-SSNO TO SAVE-SSNO.
           PERFORM DISPLAY-BORROWER.
           MOVE "NEXT" TO LAST-READ.
           MOVE "N" TO VALID-LOAN.
           IF TYP = 1
              MOVE BW-SSNO TO SAVE-BW-KEY.
           IF TYP = 2
              MOVE BW-NAME TO SAVE-BW-KEY.
           IF TYP = 3
              MOVE BW-SPSSNO TO SAVE-BW-KEY.
           IF TYP = 4
              MOVE BW-SPNAME TO SAVE-BW-KEY.
           MOVE 1 TO SCAN-FLAG.
           MOVE 1 TO FIRST-SCAN.
           MOVE " " TO SCAN.
           GO TO ENTER-KEY.

       CLOSE-SCANKEY.
           PERFORM CLOSE-LN1-FILE.
       END-SCANKEY.
           MOVE 0 TO FIRST-SCAN.
           MOVE "S  A010TYP." TO VDU.
           PERFORM SCREENIO.
           MOVE "S  A160SORTKEY." TO VDU.
           PERFORM SCREENIO.
       EXIT-ENTER-SCANKEY.
           EXIT.

      ***********************************
       ARRANGE-BW-KEY SECTION.
      ***********************************
           MOVE 1 TO VDU-I VDU-J.
           MOVE SPACES TO BW-ALP-TABLE.
       ARR-BW-KEY-AGAIN.
           IF VDU-I NOT < 35
              GO TO ARR-BW-KEY-EXIT.
           IF VDUTAB(VDU-I) NOT = ","
              MOVE VDUTAB(VDU-I) TO BW-ALP-TBL(VDU-J)
            ELSE
              MOVE 35 TO VDU-J.
           ADD 1 TO VDU-I VDU-J.
           GO TO ARR-BW-KEY-AGAIN.
       ARR-BW-KEY-EXIT.
           EXIT.

      **************************************
       SCAN-LOANS SECTION.
      **************************************
      *    GO TO START-LN4 START-LN5 START-LN6 START-LN7
           GO TO START-LN4 START-LN5 
                 DEPENDING ON SCAN-FLAG.
       START-LN4.
           IF SCAN-ON
              GO TO READ-LN4.

           MOVE LN-OWNBR    TO QLN4-WBEG-OWNBR
                               QLN4-WEND-OWNBR.
           MOVE SAVE-SSNO   TO LN-SSNO(1)
                               QLN4-WBEG-SSNO1
                               QLN4-WEND-SSNO1.
           PERFORM START-LN4-FILE.
           MOVE "ON" TO SCAN.
       READ-LN4.
           PERFORM READ-LN4-FILE-NEXT.
           IF IO-FG = 0
              IF (LN-OWNBR = LN-PATH-OWNBR) AND (LN-SSNO(1) = SAVE-SSNO)
                 IF ( LN-POCD-NONLOAN )
                    GO TO READ-LN4
                 ELSE
      * MUST BE HERE BECAUSE DISPLAY-FIELDS CHANGES VALUE OF LN-INSTRM
                    MOVE LN-REC TO ORIGINAL-LN-REC
                    PERFORM DISPLAY-FIELDS
                    GO TO END-SCAN-LOANS.
           ADD 1 TO SCAN-FLAG.
           MOVE SPACES TO SCAN.
       START-LN5.
           IF SCAN-ON
              GO TO READ-LN5.

           MOVE LN-OWNBR    TO QLN5-WBEG-OWNBR
                               QLN5-WEND-OWNBR.
           MOVE SAVE-SSNO   TO LN-SSNO(2)
                               QLN5-WBEG-SSNO2
                               QLN5-WEND-SSNO2.

           PERFORM START-LN5-FILE.
           MOVE "ON" TO SCAN.
       READ-LN5.
           PERFORM READ-LN5-FILE-NEXT.
           IF IO-FG = 0
              IF (LN-OWNBR = LN-PATH-OWNBR) AND (LN-SSNO(2) = SAVE-SSNO)
                 IF ( LN-POCD-NONLOAN )
                    GO TO READ-LN5
                 ELSE
      * MUST BE HERE BECAUSE DISPLAY-FIELDS CHANGES VALUE OF LN-INSTRM
                    MOVE LN-REC TO ORIGINAL-LN-REC
                    PERFORM DISPLAY-FIELDS
                    GO TO END-SCAN-LOANS.

      *    ADD 1 TO SCAN-FLAG.
      *    MOVE SPACES TO SCAN.
      *START-LN6.
      *    IF SCAN-ON
      *       GO TO READ-LN6.

      *    MOVE LN-OWNBR    TO QLN6-WBEG-OWNBR
      *                        QLN6-WEND-OWNBR.
      *    MOVE SAVE-SSNO   TO LN-SSNO(3)
      *                        QLN6-WBEG-SSNO3
      *                        QLN6-WEND-SSNO3.
      *    PERFORM START-LN6-FILE.
      *    MOVE "ON" TO SCAN.
      *READ-LN6.
      *    PERFORM READ-LN6-FILE-NEXT.
      *    IF IO-FG = 0
      *       IF (LN-OWNBR = LN-PATH-OWNBR) AND (LN-SSNO(3) = SAVE-SSNO)
      *          IF ( LN-POCD-NONLOAN )
      *             GO TO READ-LN6
      *          ELSE
      * MUST BE HERE BECAUSE DISPLAY-FIELDS CHANGES VALUE OF LN-INSTRM
      *             MOVE LN-REC TO ORIGINAL-LN-REC
      *             PERFORM DISPLAY-FIELDS
      *             GO TO END-SCAN-LOANS.
      *    ADD 1 TO SCAN-FLAG.
      *    MOVE SPACES TO SCAN.
      *START-LN7.
      *    IF SCAN-ON
      *       GO TO READ-LN7.

      *    MOVE LN-OWNBR    TO QLN7-WBEG-OWNBR
      *                        QLN7-WEND-OWNBR.
      *    MOVE SAVE-SSNO   TO LN-SSNO(4)
      *                        QLN7-WBEG-SSNO4
      *                        QLN7-WEND-SSNO4.
      *    PERFORM START-LN7-FILE.
      *    MOVE "ON" TO SCAN.
      *READ-LN7.
      *    PERFORM READ-LN7-FILE-NEXT.
      *    IF IO-FG = 0
      *       IF (LN-OWNBR = LN-PATH-OWNBR) AND (LN-SSNO(4) = SAVE-SSNO)
      *          IF ( LN-POCD-NONLOAN )
      *             GO TO READ-LN7
      *          ELSE
      * MUST BE HERE BECAUSE DISPLAY-FIELDS CHANGES VALUE OF LN-INSTRM
      *             MOVE LN-REC TO ORIGINAL-LN-REC
      *             PERFORM DISPLAY-FIELDS
      *             GO TO END-SCAN-LOANS.

           MOVE "CLR" TO IO-TYPE.
           MOVE 1 TO SCAN-FLAG.
           MOVE SPACES TO SCAN LN-REC.
       END-SCAN-LOANS.
           EXIT.

      ************************************
       DISPLAY-BORROWER SECTION.
      ************************************
           MOVE " " TO DISPLAY-BUF3.
           MOVE BW-SSNO TO NUM-BW.
           PERFORM NUMBW-TO-ALPBW.
           MOVE ALP-BW TO D-X010(1).
           MOVE BW-LNAME TO INSPECT-W.
           MOVE BW-FNAME TO ADD-ON.
           PERFORM ADD-IT.
           MOVE INSPECT-W TO D-BW-WK.
           MOVE "# LOANS:" TO D-BW-PROMPT.
           MOVE BW-LOANS TO D-BW-LOANS.
           MOVE D-BW-WK TO D-X011(1).
           MOVE BW-ADR1 TO INSPECT-W.
           MOVE BW-ADR2 TO ADD-ON.
           PERFORM ADD-IT.
           MOVE INSPECT-W TO D-X011(2).
           MOVE BW-CITY TO INSPECT-W.
           MOVE BW-STATE TO ADD-ON.
           PERFORM ADD-IT.
           MOVE BW-ZIP TO ADD-ON.
           PERFORM ADD-IT.
           MOVE INSPECT-W TO D-X011(3).
           MOVE BW-SPSSNO TO NUM-BW.
           PERFORM NUMBW-TO-ALPBW.
           MOVE ALP-BW TO D-X010(4).
           MOVE BW-SPLNAME TO INSPECT-W.
           MOVE BW-SPFNAME TO ADD-ON.
           PERFORM ADD-IT.
           MOVE INSPECT-W TO D-BW-WK.
           MOVE "(SPOUSE)" TO D-BW-PROMPT.
           MOVE D-BW-WK TO D-X011(4).
           MOVE DISPLAY-BUF3 TO WR-BUF.
           MOVE DISPLAY-LIST3 TO WR-LIST.
           PERFORM CALL-WRFORM.

      ***********************************************
      *   ENTRY-MODULE                              *
      *   NEW-FLAG = 0 - MODIFY EXISTING RECORD     *
      *              1 - INITIAL NEW RECORD ENTRY   *
      *              2 - MODIFY NEW RECORD          *
      ***********************************************
       ENTRY-MODULE SECTION.
       BEGIN-ENTRY.
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
      * FOR THE FIRST TIME AFTER INPUT
           PERFORM COMPUTE-PRINCIPLE.
           PERFORM COMPUTE-INS-COVERAGE.
           IF BR-AUTO-LNSTAT-WINDOW = "Y"
              PERFORM CALL-LNSTAT-WINDOW.
           MOVE 2 TO NEW-FLAG.
       MODIFY-ROUTINE.
           MOVE "N" TO CHFG(33).
           PERFORM UNPROTECT-FIELDS.
           PERFORM COMPUTE-PRINCIPLE.
           IF ELE = 13 OR 17 OR 21 OR 22 OR 23 OR 24 OR 25 OR
              PREM-CHANGE = "Y"
              PERFORM COMPUTE-INS-COVERAGE.
           IF ELE = 13 OR 17
              PERFORM COMPUTE-INS-TERM.
           PERFORM COMPUTE-PROCEEDS.
           PERFORM COMPUTE-ANTICIPATED.
           MOVE "F1,F4,F3-STATUS,FLD#,DELETE,P2,P3,U-UPD" TO MESS.
           PERFORM SEND-LEGEND.
           MOVE "R RA060SEL." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.
           IF VDUSTATUS = "16"
              PERFORM CALL-LNSTAT-WINDOW
              GO TO MODIFY-ROUTINE.
           IF VDUSTATUS = "18"
              PERFORM MEMO-ENTRY
              GO TO MODIFY-ROUTINE.
           IF VDUSTATUS NOT = "00"
              GO TO MODIFY-ROUTINE.
           MOVE VDUBUF TO ACTION.
           IF ACTION = "DELETE"
              MOVE "Y" TO VALID-DELETE
              PERFORM DELETE-MODULE
              IF VALID-DELETE NOT = "Y"
                 GO TO MODIFY-ROUTINE
              ELSE
                 PERFORM DELETE-NOTICE
                 PERFORM DELETE-TRAILER-COLLATERAL
                 MOVE "CLR" TO IO-TYPE
                 GO TO END-ENTRY-MODULE.
           IF ACTION NOT = "    P2" AND ACTION NOT = "     U" AND
              ACTION NOT = "    P3"
              GO TO MODIFY-FIELD.

           PERFORM COMPUTE-FINANCE-CHARGE.
           IF LN-FINCHG < SP-MINSF-CHG
              MOVE "FINANCE CHARGE BELOW MINIMUM" TO MESS
              PERFORM SEND-MESS
              GO TO MODIFY-ROUTINE.

           PERFORM VERIFY-MODULE.
           IF VALID-LOAN = "N"
              GO TO MODIFY-ROUTINE.

           IF LN-JDDATE = 0
              MOVE SP-JUDGRATE TO LN-JDRATE.

           IF ACTION = "    P2"
              MOVE LN-ACCTNO TO LOAN-KEY
              PERFORM WRITE-MODULE
              MOVE "O0O" TO NEW-PROG-FORM
              MOVE "END" TO IO-TYPE
              GO TO END-ENTRY-MODULE.
           IF ACTION = "    P3"
              MOVE LN-ACCTNO TO LOAN-KEY
              PERFORM WRITE-MODULE
              MOVE "V0V" TO NEW-PROG-FORM
              MOVE "END" TO IO-TYPE
              GO TO END-ENTRY-MODULE.
           IF ACTION = "     U"
              PERFORM TEST-ALLOT-LBOX
              IF IO-TYPE = "RTN"
                 MOVE SPACES TO IO-TYPE
                 GO TO MODIFY-ROUTINE
              ELSE
                 PERFORM WRITE-MODULE
                 MOVE "CLR" TO IO-TYPE
                 GO TO END-ENTRY-MODULE.
       MODIFY-FIELD.
           INSPECT ACTION REPLACING ALL " " BY "0".
           IF CHNO NOT NUMERIC
              GO TO MODIFY-ROUTINE.
           IF CHNO = 0
              GO TO MODIFY-ROUTINE.
           IF CHNO > MAX
              GO TO MODIFY-ROUTINE.
           MOVE CHNO TO ELE.

      * PASSWORD OTHER 2 O2 INSURANCE
           IF ELE = 31
              IF CHFG(ELE) NOT = "N"
                 MOVE O2-SEQ TO PASSWDW-SEQ
                 MOVE "TO CHANGE OTHER 2 INSURANCE:" TO PASSWDW-PROMPT2
                 PERFORM VERIFY-PASSWORD
                 IF PASSWDW-STAT NOT = "OK"
                    GO TO MODIFY-ROUTINE.

      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    TEST IF REFUNDS WERE SET IN WI/LNSTAT.C AND IF SO
      *    DO NOT ALLOW CHANGE TO THESE COMPONENTS
      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( ELE = 23 AND LN-INT-REBATE NOT = " " ) OR
              ( ELE = 25 AND LN-INT-REBATE NOT = " " ) OR
              ( ELE = 24 AND LN-SC-REBATE  NOT = " " ) OR
              ( ELE = 24 AND LN-MF-REBATE  NOT = " " )
              STRING "A REFUND FOR THIS COMPONENT WAS SET, "
                     "CHANGE NOT ALLOWED"
                    DELIMITED BY SIZE INTO MESS
              PERFORM SEND-MESS
              GO TO MODIFY-ROUTINE.
      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

       ENTER-ELE.
           IF LN-CURBAL NOT = 0
              OR LN-INTBAL NOT = 0
              OR LN-LCBAL NOT = 0
              OR LN-OTHBAL NOT = 0
              MOVE "CURRENT, INT, LCHG, AND OTH BALS MUST ALL BE 0.00"
                TO MESS
              PERFORM SEND-MESS
              GO TO MODIFY-ROUTINE.
           IF ELE < 5
              MOVE "BORROWER #, F6-SCAN BY NAME" TO MESS
              PERFORM SEND-LEGEND.
           IF ELE = 5
              IF NEW-FLAG NOT = 1
                 GO TO MODIFY-ROUTINE
              ELSE
                 IF CHFG(5) = "N"
                    GO TO F-052.

           IF ELE = 8
              IF NEW-FLAG = 0
                 GO TO CREATE-ROUTINE.

           IF ELE = 10 AND BR-TRAILER-REC = "Y" AND VDUSTATUS = "16"
              MOVE "N" TO CHFG(11)
              MOVE WK-HHGS-1 TO INSPECT-X
              PERFORM COLLATERAL-CREATE
              PERFORM RESTORE-TRAILER-REC.

           IF ELE = 11
      * NEW RECORD
              IF NEW-FLAG = 1
                 IF LN-COLLID = SPACES
                    GO TO CREATE-ROUTINE.
           IF ELE = 23
              IF SP-PYEXFRMLA = 0
                 GO TO CREATE-ROUTINE.
           IF ELE = 24
              IF SP-SCFRMLA = 0 AND SP-MFFRMLA = 0
                 GO TO CREATE-ROUTINE.
           IF ELE = 26
              PERFORM COMPUTE-PRINCIPLE.
           IF ELE = 26
              IF (GP-ENTER-LPO-FG = "Y")
                 MOVE "F1-CLEAR, F3-LPO, ENTER INSURANCE CODES" TO MESS
                 PERFORM SEND-LEGEND
                 IF VDUSTATUS = "16"
                    MOVE LN-LPO TO WINDOW-LPO
                    MOVE LN-REP-CD TO WINDOW-REP
                    MOVE LN-PROCESS-CD TO WINDOW-PROCESS
                    MOVE "WI/TFCCDS" TO FORM-NAM
                    CALL FORM-PROGX USING FORM-PATH WINDOW-WORKERS
                    CANCEL FORM-PROGX
                    MOVE WINDOW-LPO TO LN-LPO
                    MOVE WINDOW-REP TO LN-REP-CD
                    MOVE WINDOW-PROCESS TO LN-PROCESS-CD.
           IF ELE > 32 AND ELE < 35
              MOVE "<ENTER> = NONE" TO MESS
              PERFORM SEND-LEGEND.
           IF ELE = 35
              PERFORM RESET-LEGEND.

           IF ELE = 27
             MOVE
             "F1-CLEAR, ENTER CREDIT LIFE INSURANCE COMPANY:"
                                                           TO MESS
             PERFORM SEND-LEGEND.
           IF ELE = 28
             MOVE
             "F1-CLEAR, ENTER ACCIDENT HEALTH INSURANCE COMPANY:"
                                                           TO MESS
             PERFORM SEND-LEGEND.
           IF ELE = 29
             MOVE
             "F1-CLEAR, ENTER PERSONAL PROPERTY INSURANCE COMPANY:"
                                                           TO MESS
             PERFORM SEND-LEGEND.
           IF ELE = 30
             MOVE
             "F1-CLEAR, ENTER OTHER #1 INSURANCE COMPANY:" TO MESS
             PERFORM SEND-LEGEND.
           IF ELE = 31
             MOVE
             "F1-CLEAR, ENTER OTHER #2 INSURANCE COMPANY:" TO MESS
             PERFORM SEND-LEGEND.
           IF ELE = 32
             MOVE
             "F1-CLEAR, ENTER OTHER #3 INSURANCE COMPANY:" TO MESS
             PERFORM SEND-LEGEND.
      * FEES
           IF ELE = 33
              IF CHFG(33) = " "
                 MOVE "F1-CLEAR, F3-ENTER FEES, (RTN)-SKIP" TO MESS
                 PERFORM SEND-LEGEND
              ELSE
                 PERFORM DO-FEEENT-WINDOW
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
       TEST-F3.
           IF VDUSTATUS = "16"
              IF ELE = 33 AND CHFG(33) = " "
                    PERFORM DO-FEEENT-WINDOW
                    GO TO CREATE-ROUTINE.
       TEST-F4.
           IF VDUSTATUS NOT = "1U"
              GO TO TEST-F5.
           IF ELE = 6
              MOVE " " TO CHFG(5).
           IF ELE = 10
              PERFORM AUTOPC-ROUTINE.
           IF NEW-FLAG = 1
              MOVE -1 TO ONE
              GO TO CREATE-ROUTINE.
       TEST-F5.
           IF VDUSTATUS NOT = "1D"
              GO TO TEST-F6.
           MOVE 1 TO ONE.
           GO TO GOTO-FIELD.
       TEST-F6.
           IF VDUSTATUS NOT = "1<"
              GO TO TEST-00.
           IF ELE < 5
              PERFORM BORROWER-SCAN
              IF IO-FG = 0
                 GO TO GOTO-FIELD
              ELSE
                 GO TO ENTER-ELE.
           IF ELE = 7 OR 8 OR 9
              MOVE 0 TO CDCODE-FEE-FG
              PERFORM CD-SCAN
              IF IO-FG = 0
                 GO TO GOTO-FIELD
              ELSE
                 GO TO ENTER-ELE.
           IF ELE = 37
              PERFORM CODE-SCAN
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
                 F-31 F-32 F-33 F-34 F-35 F-36 F-37
                 DEPENDING ON OVER-20.
           GO TO END-ENTRY-MODULE.

      *************************************
      *    S E T   E L E M E N T S        *
      *************************************
       F-01.
           PERFORM BORROWER-ENTRY.
           IF IO-FG = 9
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
       F-05.
           MOVE VDUBUF TO LN-ORGST
                          LN-OWNST.
       F-052.
           MOVE " " TO CHFG(5).
           MOVE "^-BACK, F6-SCAN, ENTER SUB CLASS:" TO MESS.
           PERFORM SEND-LEGEND.
           MOVE "ENNN020052." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE.
           IF VDUSTATUS = "1<"
              PERFORM SUBCLASS-SCAN
              IF IO-FG = 9
                 GO TO F-052.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO F-052.

           MOVE LN-ORGST    TO SP-ORGST
                               QSP1-WBEG-ORGST
                               QSP1-WEND-ORGST.
           MOVE LN-SPRCLASS TO SP-SPRCLASS
                               QSP1-WBEG-SPRCLASS
                               QSP1-WEND-SPRCLASS.
           MOVE VDUNUM0     TO SP-SUBCLASS LN-SUBCLASS
                               QSP1-WBEG-SUBCLASS
                               QSP1-WEND-SUBCLASS.
           MOVE 0           TO SP-LAWCODE
                               QSP1-WBEG-LAWCODE.
           MOVE ALL "9"     TO QSP1-WEND-LAWCODE.
           PERFORM START-SP1-FILE.
       F-052-NEXT-SP.
           PERFORM READ-SP1-FILE-NEXT.
           IF NOT (SP-ORGST = LN-ORGST
                     AND SP-SPRCLASS = LN-SPRCLASS
                       AND SP-SUBCLASS = LN-SUBCLASS)
              MOVE 9 TO IO-FG.

           IF IO-FG = 9
              PERFORM SP-INVALID
              GO TO F-052.

           MOVE "S  A015053." TO VDU.
           MOVE SP-DESC TO VDUBUF.
           PERFORM SCREENIO.

           GO TO CREATE-ROUTINE.
       F-06.
           MOVE VDU-DATE-YYYYMMDD TO LN-LOANDATE.
           IF VDUNUM0 = 0
              GO TO ENTER-ELE.

           PERFORM CHECK-EFFECTIVE-SPR.
           IF IO-FG = 9
              PERFORM SP-INVALID
              GO TO ENTER-ELE
           ELSE
              MOVE "S  A015053." TO VDU
              MOVE SP-DESC       TO VDUBUF
              PERFORM SCREENIO.

           MOVE SP-LAWCODE TO LN-LAWCODE.
           IF LN-INTDATE = 0
              MOVE "S  8000160." TO VDU
              MOVE LN-LOANDATE TO VDU-DATE-YYYYMMDD LN-INTDATE
              PERFORM SCREENIO.
           IF LN-1STPYDATE NOT = 0
              GO TO CREATE-ROUTINE.
           MOVE LN-INTDATE TO NDTE-DATE.
           MOVE 1          TO NDTE-HOLD.
           MOVE LN-UNITPER-CD TO DATER-UNITPER-CD.
           MOVE LN-UNITPER-FREQ TO DATER-UNITPER-FREQ.
           PERFORM INCREMENT-UNITPER.
           MOVE "S  8000170." TO VDU.
           MOVE NDTE-DATE TO VDU-DATE-YYYYMMDD LN-1STPYDATE.
           PERFORM SCREENIO.

           GO TO CREATE-ROUTINE.
       F-07.
           MOVE "PU"   TO CD-TYPE.
           MOVE VDUBUF TO CD-CODE LN-PURCD.
           PERFORM SEND-CD-DESC.
           IF IO-FG = 9
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       F-08.
           MOVE "SR"   TO CD-TYPE.
           MOVE VDUBUF TO CD-CODE LN-SRCD.
           PERFORM SEND-CD-DESC.
           IF IO-FG = 9
              GO TO ENTER-ELE.
           IF GP-THE-ONLY-SF-SRCD NOT = " "
              IF LN-SRCD = GP-THE-ONLY-SF-SRCD
                 MOVE "INVALID FOR DIRECT LOANS" TO MESS
                 PERFORM SEND-MESS
                 GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       F-09.
           MOVE "CO"   TO CD-TYPE.
           MOVE VDUBUF TO CD-CODE LN-COLLCD.
           PERFORM SEND-CD-DESC.
           IF IO-FG = 9
              GO TO ENTER-ELE.
           MOVE CD-CO-SECURED TO SPTEST-CD-SECURED.
           GO TO CREATE-ROUTINE.
       F-10.
           MOVE VDUBUF TO LN-COLLID.
           GO TO CREATE-ROUTINE.
       F-11.
           MOVE VDUBUF TO WK-HHGS-1 MEMO-WK1.
           IF MEMO-WK1 = " "
              MOVE SPACES TO VDUBUF
              MOVE "S  A300111." TO VDU
              PERFORM SCREENIO
              PERFORM SAVE-TRAILER-REC
              GO TO CREATE-ROUTINE.
       CONT-F-11.
           MOVE "E  A330111." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE.
           IF VDUSTATUS NOT = "00" AND VDUSTATUS NOT = "1D"
              GO TO CONT-F-11.
           MOVE VDUBUF TO MEMO-WK2.
           STRING MEMO-WK1,MEMO-WK2 DELIMITED BY "   " INTO
                 WK-HHGS-1.
           PERFORM SAVE-TRAILER-REC.
           GO TO CREATE-ROUTINE.
       F-12.
           MOVE VDUNUM2 TO DL-DLNO LN-DLNO.
           PERFORM GET-DL1.
           MOVE "S  A160121." TO VDU.
           MOVE DL-SHORT TO VDUBUF.
           PERFORM SCREENIO.
           IF IO-FG = 9
              GO TO ENTER-ELE.

           IF DL-TERMDATE NOT = 0
              IF DL-TERMDATE NOT > LN-LOANDATE
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
           PERFORM MSG-LINE.
           IF BR-LC-CODE = "Y"
              MOVE "SNNN020370." TO VDU
              MOVE DL-LC-CODE TO VDUNUM0 LN-LC-CODE
              PERFORM SCREENIO.
           GO TO CREATE-ROUTINE.
       F-13.
           IF VDUNUM0 = 0
              GO TO ENTER-ELE.
           MOVE VDUNUM0 TO LN-ORGTERM LN-ANTICTERM LN-SUM-ORGTERM.
           GO TO CREATE-ROUTINE.
       F-14.
           IF VDUNUM0 = 0
              MOVE 1 TO VDUNUM0.
           IF VDUNUM0 NOT = 1
              GO TO ENTER-ELE.
           MOVE "SN N020140." TO VDU.
           MOVE 1 TO VDUNUM0.
           PERFORM SCREENIO.
           GO TO CREATE-ROUTINE.
       F-15.
           MOVE VDU-DATE-YYYYMMDD TO LN-FILEDATE.
           GO TO CREATE-ROUTINE.
       F-16.
           IF VDUNUM0 = 0
              GO TO ENTER-ELE.
           MOVE VDU-DATE-YYYYMMDD TO LN-INTDATE.

           IF LN-INTDATE < LN-LOANDATE
              MOVE "MUST NOT BE PRIOR TO LOAN DATE" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       F-17.
           MOVE VDU-DATE-YYYYMMDD TO LN-ORIG-1STPYDATE LN-1STPYDATE.
           IF VDUNUM0 = 0
              GO TO ENTER-ELE.

           IF LN-1STPYDATE NOT > LN-INTDATE
              MOVE "FIRST PAY MUST BE AFTER THE INTEREST DATE" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.

      * DO NOT ALLOW A FIRST PAYMENT DATE < 30 DAYS FROM LOAN DATE
           MOVE LN-INTDATE TO NDTE-DATE.
           MOVE 1 TO NDTE-HOLD.
           MOVE LN-UNITPER-CD TO DATER-UNITPER-CD.
           MOVE LN-UNITPER-FREQ TO DATER-UNITPER-FREQ.
           PERFORM INCREMENT-UNITPER.

           IF LN-1STPYDATE < NDTE-DATE
              MOVE 1STPY30-SEQ TO PASSWDW-SEQ
              MOVE "1ST PAY < 30 DAYS FROM LNDATE" TO PASSWDW-PROMPT2
              PERFORM VERIFY-PASSWORD
              IF PASSWDW-STAT NOT = "OK"
                 GO TO ENTER-ELE.

           GO TO CREATE-ROUTINE.

       F-18.
           MOVE VDUNUM3 TO LN-APRATE.
           GO TO CREATE-ROUTINE.
       F-19.
           MOVE VDUNUM3 TO LN-OVRDRATE.
           GO TO CREATE-ROUTINE.
       F-20.
           MOVE VDUNUM3 TO LN-ENDORATE.
           GO TO CREATE-ROUTINE.
       F-21.
           MOVE VDUNUM2 TO LN-REGPYAMT LN-SUM-REGPYAMT.
           IF VDUNUM2 = 0
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       F-22.
           MOVE VDUNUM2 TO LN-1STPYAMT.
           IF VDUNUM2 = 0
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       F-23.
           MOVE VDUNUM2 TO LN-EXTCHG LN-SUM-EXTCHG.
           GO TO CREATE-ROUTINE.
       F-24.
           MOVE VDUNUM2 TO LN-SERCHG LN-SUM-SERCHG.
       F-241.
           IF SP-MFFRMLA = 0
              GO TO CREATE-ROUTINE.
           MOVE "ENNN032241." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE.
           IF VDUSTATUS NOT = "00" AND NOT = "1D"
              GO TO F-241.
           DIVIDE VDUNUM2 BY LN-ORGTERM GIVING LN-MAINTFEE
             REMAINDER AMT-HOLDER.
           IF AMT-HOLDER NOT = 0
              MOVE "AMOUNT MUST BE EVENLY DIVISABLE BY TERM" TO MESS
              PERFORM SEND-MESS
              GO TO F-241.
           COMPUTE LN-SUM-MAINTFEE = LN-MAINTFEE * LN-ORGTERM.
           GO TO CREATE-ROUTINE.
       F-25.
           MOVE VDUNUM2 TO LN-INTCHG LN-SUM-INTCHG.
           GO TO CREATE-ROUTINE.
       F-26.
           MOVE " " TO CHFG(27) CHFG(28) CHFG(29) CHFG(30) CHFG(31).
           MOVE VDUBUF TO LN-INSCODES.
           IF LN-INSURED NOT = "M"
              IF LN-INSURED NOT = "J"
                 IF LN-INSURED NOT = "N"
                    IF LN-INSURED NOT = "S"
                       MOVE "ENTER 'N','M','S',OR 'J'" TO MESS
                       PERFORM SEND-MESS
                       GO TO ENTER-ELE.
           IF NOT LN-INSTYPES-VALID
              GO TO ENTER-ELE.
           IF NOT LN-INSELIGIBLE-VALID
              GO TO ENTER-ELE.


      * ADDED TO PREVENT ENTRY OF MNN, ETC.
           IF LN-INSTYPES = "N"
              IF LN-INSURED NOT = "N"
                 GO TO ENTER-ELE.

           IF LN-INSURED = "N"
              PERFORM VERIFY-INSURANCE
              IF IO-FG = 9
                 GO TO ENTER-ELE.

           PERFORM VALID-INS-TYPES-ELIG.
           IF IO-FG = 9
              GO TO ENTER-ELE.

           PERFORM COMPUTE-INS-COVERAGE.

           IF CHFG(27) = "N"
              MOVE 0 TO LN-INSCOMP(1) LN-INSTRM(1) LN-INSCOVR(1)
                        LN-INSPREM(1) LN-INSEXP-DATE(1) LN-INSCOMM(1)
              MOVE SPACES TO LN-INSOURS(1)
              MOVE "270 271 272 273 274." TO MESS
              PERFORM RESET-MESS.
           IF CHFG(28) = "N"
              MOVE 0 TO LN-INSCOMP(2) LN-INSTRM(2) LN-INSCOVR(2)
                        LN-INSPREM(2) LN-INSEXP-DATE(2) LN-INSCOMM(2)
              MOVE SPACES TO LN-INSOURS(2)
              MOVE "280 281 282 283 284." TO MESS
              PERFORM RESET-MESS.
           IF CHFG(29) = "N"
              MOVE 0 TO LN-INSCOMP(3) LN-INSTRM(3) LN-INSCOVR(3)
                        LN-INSPREM(3) LN-INSEXP-DATE(3) LN-INSCOMM(3)
              MOVE SPACES TO LN-INSOURS(3)
              MOVE "290 291 292 293 294." TO MESS
              PERFORM RESET-MESS.
           IF CHFG(30) = "N"
              MOVE 0 TO LN-INSCOMP(4) LN-INSTRM(4) LN-INSCOVR(4)
                        LN-INSPREM(4) LN-INSEXP-DATE(4) LN-INSCOMM(4)
              MOVE SPACES TO LN-INSOURS(4)
              MOVE "300 301 302 303 304." TO MESS
              PERFORM RESET-MESS.
           IF CHFG(31) = "N"
              MOVE 0 TO LN-INSCOMP(5) LN-INSTRM(5) LN-INSCOVR(5)
                        LN-INSPREM(5) LN-INSEXP-DATE(5) LN-INSCOMM(5)
              MOVE SPACES TO LN-INSOURS(5)
              MOVE "310 311 312 313 314." TO MESS
              PERFORM RESET-MESS.
           IF CHFG(32) = "N"
              MOVE 0 TO LN-INSCOMP(6) LN-INSTRM(6) LN-INSCOVR(6)
                        LN-INSPREM(6) LN-INSEXP-DATE(6) LN-INSCOMM(6)
              MOVE SPACES TO LN-INSOURS(6)
              MOVE "320 321 322 323 324." TO MESS
              PERFORM RESET-MESS.
           IF (GP-ENTER-LPO-FG = "Y")
              IF NEW-FLAG = 1
                 MOVE LN-LPO TO WINDOW-LPO
                 MOVE LN-REP-CD TO WINDOW-REP
                 MOVE LN-PROCESS-CD TO WINDOW-PROCESS
                 MOVE "WI/TFCCDS" TO FORM-NAM
                 CALL FORM-PROGX USING FORM-PATH WINDOW-WORKERS
                 CANCEL FORM-PROGX
                 MOVE WINDOW-LPO TO LN-LPO
                 MOVE WINDOW-REP TO LN-REP-CD
                 MOVE WINDOW-PROCESS TO LN-PROCESS-CD.
           GO TO CREATE-ROUTINE.
       F-27.
           PERFORM INSURANCE-ENTRY.
           IF IO-FG = 9
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       F-28.
           PERFORM INSURANCE-ENTRY.
           IF IO-FG = 9
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       F-29.
           PERFORM INSURANCE-ENTRY.
           IF IO-FG = 9
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       F-30.
           PERFORM INSURANCE-ENTRY.
           IF IO-FG = 9
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       F-31.
           PERFORM INSURANCE-ENTRY.
           IF IO-FG = 9
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       F-32.
           PERFORM INSURANCE-ENTRY.
           IF IO-FG = 9
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       F-33.
           GO TO CREATE-ROUTINE.
       F-34.
           GO TO CREATE-ROUTINE.
       F-35.
           MOVE VDUNUM0 TO W-ACCOUNT.
           IF VDUNUM0 = 0
              MOVE 0 TO LN-PRLNNO
              GO TO CREATE-ROUTINE.
           MOVE 1 TO W-SEQUENCE.
           MOVE W-LNNO TO LN-PRLNNO.
           GO TO CREATE-ROUTINE.
       F-36.
      *    MOVE VDUNUM2 TO LN-ANTICERN(1).
           GO TO CREATE-ROUTINE.
       F-37.
           MOVE "EL"    TO CD-TYPE.
           MOVE VDUNUM0 TO CD-EL-CODE LN-LC-CODE.
           MOVE SPACES  TO CD-EL-FILLER.
           PERFORM GET-CD.
           IF IO-FG NOT = 0 AND LN-LC-CODE NOT = 0
              MOVE "INVALID CODE" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       END-ENTRY-MODULE.
           EXIT.

      ***********************************
       BORROWER-ENTRY SECTION.
      ***********************************
           MOVE 0 TO IO-FG.

           MOVE VDUNUM0 TO NUM-BW.
           MOVE NUM-BW TO LN-SSNO(ELE) BW-SSNO.
           IF NUM-BW NOT = 0
              GO TO GET-BORROWER.
           IF ELE = 1
              MOVE 9 TO IO-FG
              GO TO EXIT-ENTER-BORROWER.
           MOVE 0 TO BW-SSNO.
           MOVE " " TO INSPECT-W.
       NEXT-BW-CLEAR.
           MOVE 0 TO LN-SSNO(ELE).
           MOVE " " TO LN-MAKERCD(ELE).
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

           IF GP-VALID-BDATE = "Y"
              MOVE BW-BDATE TO NUM-DATE
              PERFORM TSTDAT
              IF NUM-DATE = 0
                 MOVE "BORROWER HAS AN INVALID BIRTHDATE" TO MESS
                 PERFORM SEND-MESS
                 MOVE 9 TO IO-FG
                 GO TO EXIT-ENTER-BORROWER.

           IF ELE = 1
              MOVE "M"             TO LN-MAKERCD(1)
              PERFORM SEND-BW-MAKERCD
              MOVE BW-CONFIDENTIAL TO LN-CONFIDENTCD
              MOVE BW-ORIGIN       TO LN-ORIGIN
              GO TO EXIT-ENTER-BORROWER.

       ENTER-MAKER-CODE.
           MOVE "E  A010  2." TO VDU.
           MOVE ELE TO VDUROW.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              MOVE 9 TO IO-FG
              GO TO EXIT-ENTER-BORROWER.
           IF VDUSTATUS NOT = "00"
              GO TO ENTER-MAKER-CODE.
           IF VDUBUF NOT = "J"
              IF VDUBUF NOT = "E"
                 MOVE "(M-MAKER),J-JOINT,E-ENDORSER" TO MESS
                 PERFORM SEND-MESS
                 GO TO ENTER-MAKER-CODE.
           MOVE VDUBUF TO LN-MAKERCD(ELE).
       EXIT-ENTER-BORROWER.
           EXIT.

      ***********************************
       BW-TABLE-CHECK SECTION.
      ***********************************
           IF ELE NOT = 1
              SUBTRACT 1 FROM ELE GIVING I-LEVEL
              IF LN-SSNO(I-LEVEL) = 0
                 MOVE 9 TO IO-FG
                 MOVE "ENTER SERIALLY" TO MESS
                 PERFORM SEND-MESS
                 GO TO EXIT-BW-TABLE-CHECK.
           MOVE 4 TO I-LEVEL.
       NEXT-BW-LEVEL-CHECK.
           IF LN-SSNO(I-LEVEL) = LN-SSNO(ELE) AND I-LEVEL NOT = ELE
              MOVE 9 TO IO-FG
              MOVE "NO DUPLICATES" TO MESS
              PERFORM SEND-MESS
              GO TO EXIT-BW-TABLE-CHECK.
           SUBTRACT 1 FROM I-LEVEL.
           IF I-LEVEL > 0
              GO TO NEXT-BW-LEVEL-CHECK.
       EXIT-BW-TABLE-CHECK.
           EXIT.

      ***********************************
       BW-CHECK-ENDORSER SECTION.
      ***********************************
           MOVE 0 TO I-LEVEL.
       NEXT-ENDORSER-CHECK.
           ADD 1 TO I-LEVEL.
           IF LN-MAKERCD(I-LEVEL) = "E"
              MOVE " " TO CHFG(20)
              GO TO EXIT-CHECK-ENDORSER.
           IF I-LEVEL < 4
              GO TO NEXT-ENDORSER-CHECK.
       EXIT-CHECK-ENDORSER.
           EXIT.

      *******************************************
       SUBCLASS-SCAN SECTION.
      *******************************************
           MOVE SPACES      TO SCSCAN-SP1-KEY.
           MOVE LN-ORGST    TO SCSCAN-ORGST.
           MOVE LN-SPRCLASS TO SCSCAN-SPRCLASS.
           MOVE 0           TO SCSCAN-SUBCLASS
                               SCSCAN-LAWCODE.
           MOVE "Y"         TO SCSCAN-LAST-LAW.
           MOVE TRANS-DATE  TO SCSCAN-TRANS-DATE.
           MOVE "WI/SCSCAN" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH SCSCAN-BUF EXIT-PATHNAME.
           CANCEL FORM-PROGX.
           IF SCSCAN-STATUS NOT = "00" OR SCSCAN-SP1-KEY = SPACES
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

      *************************************
       BORROWER-SCAN SECTION.
      *************************************
           MOVE "NAME, F6-SCAN, <ENTER>-ACCEPT" TO MESS.
           PERFORM SEND-LEGEND.
           MOVE 0 TO IO-FG.
           MOVE " " TO ALP-BW.

           MOVE BW-PATH-OWNBR TO BW-OWNBR.
           MOVE SPACES        TO BW-NAME.

           PERFORM START-BW2-FILE.
       BORROWER-GET-KEY.
           MOVE "E RA150KEY." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1<"
              GO TO BORROWER-READ-NEXT.
           IF VDUSTATUS = "10" OR VDUSTATUS = "1U"
              MOVE 9 TO IO-FG
              GO TO EXIT-BORROWER-SCAN.
           IF VDUSTATUS NOT = "00"
              GO TO BORROWER-GET-KEY.
           IF VDUBUF = SPACE
              MOVE ALP-BW TO VDUBUF
              GO TO EXIT-BORROWER-SCAN.

           MOVE BW-PATH-OWNBR TO BW-OWNBR.
           MOVE VDUBUF        TO BW-NAME.

           PERFORM START-BW2-FILE.
       BORROWER-READ-NEXT.
           PERFORM READ-BW2-FILE-NEXT.
           IF (IO-FG = 9) OR (BW-OWNBR NOT = BW-PATH-OWNBR)
              GO TO EXIT-BORROWER-SCAN.

           PERFORM APPEND-BW-NAMES.
           PERFORM SEND-BW-NO-NAME.

           GO TO BORROWER-GET-KEY.
       EXIT-BORROWER-SCAN.
           EXIT.

      *********************************************
       CODE-SCAN SECTION.
      *********************************************

           MOVE "EL"   TO ELSCAN-TYPE.
           MOVE ZEROES TO ELSCAN-CODE.
           MOVE "WI/ELSCAN " TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH ELSCAN-BUF EXIT-PATHNAME.
           CANCEL FORM-PROGX.

           IF ( ELSCAN-STATUS NOT = "00") OR (ELSCAN-CODE = 0 )
               MOVE 9 TO IO-FG
               GO TO EXIT-CODE-SCAN.

           MOVE "SNNN020370." TO VDU.
           MOVE ELSCAN-CODE   TO VDUNUM0.
           PERFORM SCREENIO.

           MOVE ZEROES        TO IO-FG.
           MOVE ELSCAN-CODE   TO VDUNUM0.

       EXIT-CODE-SCAN.
           EXIT.

      ********************************************
       FEECODE-SCAN SECTION.
      ********************************************
           MOVE "GC"   TO CDBSCN-TYPE.
           MOVE BRANCH TO CDBSCN-BRANCH.
           MOVE SPACES TO CDBSCN-CODE.

           MOVE "WI/CDBSCN" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH CDBSCN-BUF EXIT-PATHNAME.
           CANCEL FORM-PROGX.
           IF ( CDBSCN-STATUS    NOT = "00"    ) OR 
              ((CDBSCN-TYPE = "GC") AND (CDBSCN-CODE = SPACES))
              MOVE 9 TO IO-FG
              GO TO EXIT-FEECODE-SCAN.

           MOVE "S  A030  0." TO VDU
           MOVE ELE TO VDUROW
           MOVE CDBSCN-CODE TO VDUBUF
           PERFORM SCREENIO.

           MOVE CDBSCN-CODE TO VDUBUF.
           MOVE 0 TO IO-FG.

       EXIT-FEECODE-SCAN.
           EXIT.

      ********************************************
       CD-SCAN SECTION.
      ********************************************

           MOVE SPACES TO CDSCAN-CODE.
           IF ELE = 7
              MOVE "PU"   TO CDSCAN-TYPE
           ELSE
           IF ELE = 8
              MOVE "SR"   TO CDSCAN-TYPE
           ELSE
           IF ELE = 9
              MOVE "CO"   TO CDSCAN-TYPE
           ELSE
              PERFORM FEECODE-SCAN
              GO TO EXIT-CODE-SCAN.

           MOVE "WI/CDSCAN" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH CDSCAN-BUF EXIT-PATHNAME.
           CANCEL FORM-PROGX.
           IF ( CDSCAN-STATUS    NOT = "00"    ) OR 
              ((CDSCAN-TYPE = "PU") AND (CDSCAN-CODE = SPACES)) OR
              ((CDSCAN-TYPE = "CO") AND (CDSCAN-CODE = SPACES )) OR
              ((CDSCAN-TYPE = "SR") AND (CDSCAN-CODE = SPACES )) 
              MOVE 9 TO IO-FG
              GO TO EXIT-CD-SCAN.

           MOVE "S  A030  0." TO VDU
           MOVE ELE TO VDUROW
           MOVE CDSCAN-CODE TO VDUBUF
           PERFORM SCREENIO.

           MOVE CDSCAN-CODE TO VDUBUF.
           MOVE 0 TO IO-FG.

       EXIT-CD-SCAN.
           EXIT.

      ***********************************
       INSURANCE-ENTRY SECTION.
      ***********************************
           MOVE 0 TO IO-FG.
           SUBTRACT 26 FROM ELE GIVING I-LEVEL.
           MOVE VDUNUM0 TO LN-INSCOMP(I-LEVEL) IC-CODE.
           IF IC-CODE = 0
              GO TO ENTER-IC-OURS.

      * PASSWORD OTHER 2 O2 INSURANCE
           IF I-LEVEL = 5
              MOVE O2-SEQ TO PASSWDW-SEQ
              MOVE "TO CHANGE OTHER 2 INSURANCE:" TO PASSWDW-PROMPT2
              PERFORM VERIFY-PASSWORD
              IF PASSWDW-STAT NOT = "OK"
                 MOVE 9 TO IO-FG
                 GO TO EXIT-ENTER-INSURANCE.

           PERFORM READ-IC1-FILE.
           IF IO-FG = 9
              MOVE "INVALID COMPANY" TO MESS
              PERFORM SEND-MESS
              GO TO EXIT-ENTER-INSURANCE.
           PERFORM COMPUTE-INS-TERM-I-LEVEL.
       ENTER-IC-OURS.
           IF IC-CODE = 0
              MOVE " " TO LN-INSOURS(I-LEVEL)
              GO TO SEND-IC-OURS.
           MOVE "SPACE = Y" TO MESS.
           PERFORM SEND-LEGEND.
           MOVE "R  A010  1." TO VDU.
           MOVE ELE TO VDUROW.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              MOVE 9 TO IO-FG
              GO TO EXIT-ENTER-INSURANCE.
           IF VDUSTATUS NOT = "00" AND VDUSTATUS NOT = "1D"
              GO TO ENTER-IC-OURS.
           IF VDUBUF = " "
              MOVE "Y" TO VDUBUF.
           IF VDUBUF NOT = "Y"
              IF VDUBUF NOT = "M"
                 MOVE "Y-YES, M-MEMO" TO MESS
                 PERFORM SEND-MESS
                 GO TO ENTER-IC-OURS.
           MOVE VDUBUF TO LN-INSOURS(I-LEVEL).
       SEND-IC-OURS.
           MOVE "S  A010  1." TO VDU.
           MOVE ELE TO VDUROW.
           MOVE LN-INSOURS(I-LEVEL) TO VDUBUF.
           PERFORM SCREENIO.
           MOVE SPACES TO MESS
           PERFORM SEND-LEGEND.
           IF IC-CODE NOT = 0
              IF I-LEVEL > 4
                 GO TO ENTER-IC-COVR
              ELSE
                 GO TO ENTER-IC-PREM.
       ENTER-IC-TERM.
           IF I-LEVEL > 4 AND GP-OT-INS-TERM NOT = "Y"
              GO TO ENTER-IC-EXP.
           IF IC-CODE = 0
              MOVE 0 TO LN-INSTRM(I-LEVEL)
              GO TO SEND-IC-TERM.
           IF I-LEVEL > 4 AND GP-OT-INS-TERM = "Y"
              MOVE "RNNN060  2." TO VDU
           ELSE
              MOVE "RNNN030  2." TO VDU.
           MOVE ELE TO VDUROW.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-IC-OURS.
           IF VDUSTATUS NOT = "00" AND NOT = "1D"
              GO TO ENTER-IC-TERM.
           IF ( VDUNUM0 > 360 )
              GO TO ENTER-IC-TERM.
           MOVE VDUNUM0 TO LN-INSTRM(I-LEVEL).
       SEND-IC-TERM.
           IF I-LEVEL > 4 AND GP-OT-INS-TERM = "Y"
              MOVE "SN N060  2." TO VDU
           ELSE
              MOVE "SN N030  2." TO VDU.
           MOVE ELE TO VDUROW.
           MOVE LN-INSTRM(I-LEVEL) TO VDUNUM0.
           PERFORM SCREENIO.
           GO TO ENTER-IC-COVR.
       ENTER-IC-EXP.
           IF IC-CODE = 0
              MOVE 0 TO LN-INSEXP-DATE(I-LEVEL)
              GO TO SEND-IC-EXP.
           MOVE "RNNN060  2." TO VDU.
           MOVE ELE TO VDUROW.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-IC-OURS.
           IF VDUSTATUS NOT = "00" AND NOT = "1D"
              GO TO ENTER-IC-EXP.
           IF VDUNUM0 = 0
              GO TO ENTER-IC-EXP.
           MOVE VDUNUM0 TO DATE-MMDDYY.
           PERFORM CONVERT-MMDDYY-TO-YYYYMMDD.
           MOVE DATE-YYYYMMDD TO LN-INSEXP-DATE(I-LEVEL) NUM-DATE.
           PERFORM TSTDAT.
           IF NUM-DATE  = 0
              MOVE "INVALID DATE" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-IC-EXP.
       SEND-IC-EXP.
           MOVE "SNNN060  2." TO VDU.
           MOVE ELE TO VDUROW.
           MOVE LN-INSEXP-DATE(I-LEVEL) TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO VDUNUM0.
           PERFORM SCREENIO.
       ENTER-IC-COVR.
           IF IC-CODE = 0
              MOVE 0 TO LN-INSCOVR(I-LEVEL)
              GO TO SEND-IC-COVR.
           MOVE "RNNN062  3." TO VDU.
           MOVE ELE TO VDUROW.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-IC-TERM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D"
              GO TO ENTER-IC-COVR.
           MOVE VDUNUM2 TO LN-INSCOVR(I-LEVEL).
       SEND-IC-COVR.
           MOVE "SN N062  3." TO VDU.
           MOVE ELE TO VDUROW.
           MOVE LN-INSCOVR(I-LEVEL) TO VDUNUM2.
           PERFORM SCREENIO.
       ENTER-IC-PREM.
           MOVE " " TO PREM-CHANGE.
           MOVE LN-INSPREM(I-LEVEL) TO PREM-AMOUNT.
           IF IC-CODE = 0
              MOVE 0 TO LN-INSPREM(I-LEVEL)
              GO TO SEND-IC-PREM.
           MOVE "RNNN042  4." TO VDU.
           MOVE ELE TO VDUROW.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-IC-COVR.
           IF VDUSTATUS NOT = "00" AND VDUSTATUS NOT = "1D"
              GO TO ENTER-IC-PREM.
           MOVE VDUNUM2 TO LN-INSPREM(I-LEVEL).
       SEND-IC-PREM.
           MOVE "SN N042  4." TO VDU.
           MOVE ELE TO VDUROW.
           MOVE LN-INSPREM(I-LEVEL) TO VDUNUM2.
           PERFORM SCREENIO.
           IF LN-INSPREM(I-LEVEL) NOT = PREM-AMOUNT
              MOVE "Y" TO PREM-CHANGE.
       EXIT-ENTER-INSURANCE.
           EXIT.

      ***********************************
       CHECK-EFFECTIVE-SPR SECTION.
      ***********************************
           MOVE LN-ORGST    TO SP-ORGST
                               QSP1-WBEG-ORGST
                               QSP1-WEND-ORGST.
           MOVE LN-SPRCLASS TO SP-SPRCLASS
                               QSP1-WBEG-SPRCLASS
                               QSP1-WEND-SPRCLASS.
           MOVE LN-SUBCLASS TO SP-SUBCLASS
                               QSP1-WBEG-SUBCLASS
                               QSP1-WEND-SUBCLASS.
           MOVE 0           TO SP-LAWCODE
                               QSP1-WBEG-LAWCODE.
           MOVE ALL "9"     TO QSP1-WEND-LAWCODE.

           PERFORM START-SP1-FILE.
       CHECK-EFF-SPR-NEXT.
           PERFORM READ-SP1-FILE-NEXT.
           IF IO-FG = 9
              GO TO CHECK-EFF-SPR-EXIT.

           IF NOT (SP-ORGST = LN-ORGST
                   AND SP-SPRCLASS = LN-SPRCLASS
                    AND SP-SUBCLASS = LN-SUBCLASS)
              MOVE 9 TO IO-FG
              GO TO CHECK-EFF-SPR-EXIT.

           IF SP-EFFDATE > LN-LOANDATE
              GO TO CHECK-EFF-SPR-NEXT
           ELSE
              IF SP-EXPDATE = 0
                 GO TO CHECK-EFF-SPR-EXIT.

           IF SP-EXPDATE NOT > LN-LOANDATE
              GO TO CHECK-EFF-SPR-NEXT.

       CHECK-EFF-SPR-EXIT.
           EXIT.

      ***********************************
       COMPUTE-FINANCE-CHARGE SECTION.
      ***********************************
           MOVE LN-INTCHG TO LN-FINCHG.
           IF SP-FINCHG = 1
              COMPUTE LN-FINCHG = LN-INTCHG + LN-EXTCHG
           ELSE IF SP-FINCHG = 2
              COMPUTE LN-FINCHG = LN-INTCHG + LN-SERCHG
           ELSE IF SP-FINCHG = 3
              COMPUTE LN-FINCHG = LN-INTCHG +
                                  (LN-MAINTFEE * LN-ORGTERM)
           ELSE IF SP-FINCHG = 4
              COMPUTE LN-FINCHG = LN-INTCHG + LN-EXTCHG
                                + LN-SERCHG
           ELSE IF SP-FINCHG = 5
              COMPUTE LN-FINCHG = LN-INTCHG + LN-EXTCHG
                                + (LN-MAINTFEE * LN-ORGTERM)
           ELSE IF SP-FINCHG = 6
              COMPUTE LN-FINCHG = LN-INTCHG + LN-EXTCHG
                          + LN-SERCHG
                          + (LN-MAINTFEE * LN-ORGTERM)
           ELSE IF SP-FINCHG = 7
              COMPUTE LN-FINCHG = LN-INTCHG
                                + LN-SERCHG
                         + (LN-MAINTFEE * LN-ORGTERM)
           ELSE IF SP-FINCHG = 8
              COMPUTE LN-FINCHG = LN-SERCHG +
                      (LN-MAINTFEE * LN-ORGTERM).
      *CS 03/26/01
           ADD TOTAL-FEEFINCHG TOTAL-INSFINCHG TO LN-FINCHG.

           MOVE LN-FINCHG TO LN-SUM-FINCHG.

      *********************************
       COMPUTE-PRINCIPLE SECTION.
      *********************************
           PERFORM COMPUTE-NOREGPY.
           PERFORM LOAN-CALCULATIONS.
           MOVE "SN N062PRIN." TO VDU.
           MOVE PRINCIPLE TO VDUNUM2 LN-LNAMT LN-PURBAL LN-ORIGPURAMT
                             LN-SUM-LNAMT LN-SUM-ORIGPURAMT.
           PERFORM SCREENIO.

      ************************************
       COMPUTE-INS-COVERAGE SECTION.
      ************************************
           IF LN-INSTYPES-LL OR LN-INSTYPES-DL
              IF LN-INSCOMP(1) NOT = 0
                 MOVE "SN N062273." TO VDU
                 MOVE 1 TO LN-SUB
                 IF LN-INSTYPES-LL
                    MOVE 3 TO SP-SUB
                    PERFORM SET-COVERAGE
                 ELSE
                    MOVE 1 TO SP-SUB
                    PERFORM SET-COVERAGE.
           IF LN-INSTYPES-AH
              IF LN-INSCOMP(2) NOT = 0
                 MOVE "SN N062283." TO VDU
                 MOVE 5 TO SP-SUB
                 MOVE 2 TO LN-SUB
                 PERFORM SET-COVERAGE.
           IF LN-INSTYPES-PP OR LN-INSTYPES-PP2
              IF LN-INSCOMP(3) NOT = 0
                 MOVE "SN N062293." TO VDU
                 MOVE 3 TO LN-SUB
                 IF LN-INSTYPES-PP
                    MOVE 6 TO SP-SUB
                    PERFORM SET-COVERAGE
              ELSE
                 MOVE 7 TO SP-SUB
                 PERFORM SET-COVERAGE.
           IF LN-INSTYPES-O1
              IF LN-INSCOMP(4) NOT = 0
                 MOVE "SN N062303." TO VDU
                 MOVE 8 TO SP-SUB
                 MOVE 4 TO LN-SUB
                 PERFORM SET-COVERAGE.
           GO TO COMPUTE-INS-COVERAGE-EXIT.
       SET-COVERAGE.
           IF LN-INSURED = "J"
              IF SP-SUB = 1 OR 3
                 ADD 1 TO SP-SUB.
           IF SP-INS-TOTAL-NOTE(SP-SUB)
              MOVE TOTAL-NOTE TO VDUNUM2 LN-INSCOVR(LN-SUB)
           ELSE
           IF SP-INS-PRINCIPAL(SP-SUB) OR SP-INS-ADJ-PRIN-NO1INT(SP-SUB)
              MOVE DISCLOSED-FINAMT TO VDUNUM2 LN-INSCOVR(LN-SUB)
           ELSE
           IF SP-INS-CASH-ADVANCE(SP-SUB)
              MOVE CASH-ADVANCE TO VDUNUM2 LN-INSCOVR(LN-SUB)
           ELSE
           IF SP-INS-SP-CASH-ADVANCE(SP-SUB)
              MOVE SPCASH-ADVANCE TO VDUNUM2 LN-INSCOVR(LN-SUB)
           ELSE
           IF SP-INS-CASH-ADV-FEES(SP-SUB)
              COMPUTE LN-INSCOVR(LN-SUB) = CASH-ADVANCE - NONMEMO-FEES
              MOVE LN-INSCOVR(LN-SUB) TO VDUNUM2
           ELSE
           IF SP-INS-TOTAL-NOTE-EXT(SP-SUB)
              ADD TOTAL-NOTE LN-EXTCHG GIVING VDUNUM2
                                              LN-INSCOVR(LN-SUB)
           ELSE
           IF SP-INS-ALL(SP-SUB)
              MOVE TOTAL-REPAY TO VDUNUM2 LN-INSCOVR(LN-SUB)
           ELSE
           IF SP-INS-ADJ-PRINCIPAL(SP-SUB)
              MOVE ADJ-PRINCIPAL TO VDUNUM2 LN-INSCOVR(LN-SUB)
           ELSE
           IF SP-INS-ADJ-PRIN-3PMTS(SP-SUB)
              COMPUTE LN-INSCOVR(LN-SUB) =
                     DISCLOSED-FINAMT + (3 * LN-REGPYAMT)
                     MOVE LN-INSCOVR(LN-SUB) TO VDUNUM2
           ELSE
           IF SP-INS-AMTFIN-2PMTS(SP-SUB)
              COMPUTE LN-INSCOVR(LN-SUB) =
                     DISCLOSED-FINAMT + (2 * LN-REGPYAMT)
                     MOVE LN-INSCOVR(LN-SUB) TO VDUNUM2
           ELSE
           IF SP-INS-AMTFIN-1PMTS(SP-SUB)
              COMPUTE LN-INSCOVR(LN-SUB) = DISCLOSED-FINAMT
                                          + LN-REGPYAMT
                            MOVE LN-INSCOVR(LN-SUB) TO VDUNUM2
           ELSE
           IF SP-INS-ADJ-CASH-ADVANCE(SP-SUB)
              MOVE HIGH-CREDIT TO VDUNUM2 LN-INSCOVR(LN-SUB)
           ELSE
           IF SP-INS-BENEFIT-TERM(SP-SUB)
              COMPUTE LN-INSCOVR(LN-SUB) = TOTAL-NOTE / LN-ORGTERM
                                         * LN-INSTRM(LN-SUB)
              MOVE LN-INSCOVR(LN-SUB) TO VDUNUM2
           ELSE
           IF SP-INS-TN-INT-CL(SP-SUB)
              COMPUTE LN-INSCOVR(LN-SUB) =
                 LN-PROCEEDS + NONMEMO-FEES + LN-SERCHG
                    + (LN-PRBAL + LN-PRINTDUE - LN-PRFINCHGRB
                         - LN-PRINSRB) + LN-INSPREM(2) + LN-INSPREM(3)
                    + LN-INSPREM(4) + LN-INSPREM(5) + LN-INSPREM(6)
              MOVE LN-INSCOVR(LN-SUB) TO VDUNUM2
           ELSE
           IF SP-INS-TN-INT-SC-CL(SP-SUB)
              COMPUTE LN-INSCOVR(LN-SUB) = LN-PROCEEDS + NONMEMO-FEES
                    + LN-INSPREM(2) + LN-INSPREM(3)
                    + LN-INSPREM(4) + LN-INSPREM(5) + LN-INSPREM(6)
              MOVE LN-INSCOVR(LN-SUB) TO VDUNUM2
           ELSE
           IF SP-INS-NET-PAYOFF(SP-SUB)
              MOVE DISCLOSED-FINAMT TO LN-INSCOVR(LN-SUB) VDUNUM2
           ELSE
           IF SP-INS-MIP(SP-SUB)
              IF LN-SUB = 1
                 MOVE SPCASH-ADVANCE TO VDUNUM2 LN-INSCOVR(LN-SUB)
              ELSE
                 COMPUTE LN-INSCOVR(LN-SUB) = TOTAL-NOTE / LN-ORGTERM
                                            * LN-INSTRM(LN-SUB)
                 MOVE LN-INSCOVR(LN-SUB) TO VDUNUM2
              END-IF
           ELSE
              MOVE 0 TO VDUNUM2 LN-INSCOVR(LN-SUB).

           IF SP-INS-MAX-TERM-BENEFIT(SP-SUB)
              IF LN-ORGTERM > SP-INSMAXTERM(SP-SUB)
                 IF LN-REGPYAMT > SP-INSMAXBENIF(SP-SUB)
      * BOTH TERM AND PAYMENT EXCEED MAX
                    COMPUTE LN-INSCOVR(LN-SUB) =
                     ( SP-INSMAXTERM(SP-SUB) * SP-INSMAXBENIF(SP-SUB) )
                                - LN-SUM-MAINTFEE - LN-EXTCHG
                    MOVE LN-INSCOVR(LN-SUB) TO VDUNUM2
                 ELSE
      * TERM EXCEEDS MAX BUT PAYMENT DOES NOT
                    COMPUTE LN-INSCOVR(LN-SUB) =
                          ( SP-INSMAXTERM(SP-SUB) * LN-REGPYAMT )
                                - LN-SUM-MAINTFEE - LN-EXTCHG
                    MOVE LN-INSCOVR(LN-SUB) TO VDUNUM2
              ELSE
                 IF LN-REGPYAMT > SP-INSMAXBENIF(SP-SUB)
      * TERM DOES NOT EXCEED MAX BUT PAYMENT DOES
                    COMPUTE LN-INSCOVR(LN-SUB) =
                          ( LN-ORGTERM * SP-INSMAXBENIF(SP-SUB) )
                                - LN-SUM-MAINTFEE - LN-EXTCHG
                    MOVE LN-INSCOVR(LN-SUB) TO VDUNUM2
                  ELSE
                     MOVE TOTAL-NOTE TO LN-INSCOVR(LN-SUB) VDUNUM2.
           PERFORM SCREENIO.
       COMPUTE-INS-COVERAGE-EXIT.
           EXIT.

      *********************************
       COMPUTE-INS-TERM SECTION.
      *********************************
           MOVE 0 TO I-LEVEL.
       COMPUTE-INS-TRM.
           IF I-LEVEL < 5
              ADD 1 TO I-LEVEL
              PERFORM COMPUTE-INS-TERM-I-LEVEL
              GO TO COMPUTE-INS-TRM.

      *********************************
       COMPUTE-INS-TERM-I-LEVEL SECTION.
      *********************************
           IF I-LEVEL = 1
              IF LN-INSCOMP(1) NOT = 0
                 MOVE 0 TO SUB
                 IF LN-INSURED = "J"
                    MOVE 1 TO SUB.

           IF I-LEVEL = 1
              IF LN-INSCOMP(1) NOT = 0
                 MOVE "SN N030272." TO VDU
                 IF LN-INSTYPES-DL
                    ADD 1 TO SUB
                    PERFORM SET-INSURANCE-TERM-VDU
                    MOVE VDUNUM0 TO LN-INSTRM(I-LEVEL)
                    PERFORM SCREENIO
                 ELSE
                    IF LN-INSTYPES-LL
                       ADD 3 TO SUB
                       PERFORM SET-INSURANCE-TERM-VDU
                       MOVE VDUNUM0 TO LN-INSTRM(I-LEVEL)
                       PERFORM SCREENIO.

           IF I-LEVEL = 2
              IF LN-INSCOMP(2) NOT = 0
                 IF LN-INSTYPES-AH
                    MOVE "SN N030282." TO VDU
                    MOVE 5 TO SUB
                    PERFORM SET-INSURANCE-TERM-VDU
                    MOVE VDUNUM0 TO LN-INSTRM(I-LEVEL)
                    PERFORM SCREENIO.

           IF I-LEVEL = 3
              IF LN-INSCOMP(3) NOT = 0
                 MOVE "SN N030292." TO VDU
                 IF LN-INSTYPES-PP
                    MOVE 6 TO SUB
                    PERFORM SET-INSURANCE-TERM-VDU
                    MOVE VDUNUM0 TO LN-INSTRM(I-LEVEL)
                    PERFORM SCREENIO
                 ELSE
                    IF LN-INSTYPES-PP2
                       MOVE 7 TO SUB
                       PERFORM SET-INSURANCE-TERM-VDU
                       MOVE VDUNUM0 TO LN-INSTRM(I-LEVEL)
                       PERFORM SCREENIO.

           IF I-LEVEL = 4
              IF LN-INSCOMP(4) NOT = 0
                 IF LN-INSTYPES-O1
                    MOVE "SN N030302." TO VDU
                    MOVE 8 TO SUB
                    PERFORM SET-INSURANCE-TERM-VDU
                    MOVE VDUNUM0 TO LN-INSTRM(I-LEVEL)
                    PERFORM SCREENIO.

           IF I-LEVEL = 5
              IF LN-INSCOMP(5) NOT = 0
                 IF GP-OT-INS-TERM = "Y"
                    MOVE "SNNN060312." TO VDU
                    MOVE 9 TO SUB
                    PERFORM SET-INSURANCE-TERM-VDU
                    MOVE VDUNUM0 TO LN-INSTRM(I-LEVEL)
                    PERFORM SCREENIO
                 ELSE
                    MOVE LN-INTDATE TO NDTE-DATE
                    MOVE LN-ORGTERM TO NDTE-HOLD
                    MOVE LN-UNITPER-CD TO DATER-UNITPER-CD
                    MOVE LN-UNITPER-FREQ TO DATER-UNITPER-FREQ
                    PERFORM INCREMENT-UNITPER
                    MOVE "SNNN060312." TO VDU
                    MOVE NDTE-DATE TO DATE-YYYYMMDD LN-INSEXP-DATE(5)
                    PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
                    MOVE DATE-MMDDYY TO VDUNUM0
                    PERFORM SCREENIO.

           IF I-LEVEL = 6
              IF LN-INSCOMP(6) NOT = 0
                 IF GP-OT-INS-TERM = "Y"
                    MOVE "SNNN060322." TO VDU
                    MOVE 10 TO SUB
                    PERFORM SET-INSURANCE-TERM-VDU
                    MOVE VDUNUM0 TO LN-INSTRM(I-LEVEL)
                    PERFORM SCREENIO
                 ELSE
                    MOVE LN-INTDATE TO NDTE-DATE
                    MOVE LN-ORGTERM TO NDTE-HOLD
                    MOVE LN-UNITPER-CD TO DATER-UNITPER-CD
                    MOVE LN-UNITPER-FREQ TO DATER-UNITPER-FREQ
                    PERFORM INCREMENT-UNITPER
                    MOVE "SNNN060322." TO VDU
                    MOVE NDTE-DATE TO DATE-YYYYMMDD LN-INSEXP-DATE(6)
                    PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
                    MOVE DATE-MMDDYY TO VDUNUM0
                    PERFORM SCREENIO.

      *********************************
       SET-INSURANCE-TERM-VDU SECTION.
      *********************************
           IF SP-INSURE-EXT-PERIOD(SUB)
              PERFORM EXTENSION-DAYS-CALCULATION
              IF EDAY-MONTHS > 0
                 COMPUTE VDUNUM0 = EDAY-MONTHS + LN-ORGTERM
              ELSE
                 MOVE LN-ORGTERM TO VDUNUM0
           ELSE
              MOVE LN-ORGTERM TO VDUNUM0.

      *********************************
       COMPUTE-PROCEEDS SECTION.
      *********************************
           MOVE 0 TO SUB OUR-CHGS.
       NEXT-INSPREM.
           ADD 1 TO SUB.
           IF SUB NOT > 5
              IF LN-INSOURS(SUB) NOT = "M"
                 ADD LN-INSPREM(SUB) TO OUR-CHGS.
           ADD LN-FEEAMT(SUB) TO OUR-CHGS.
           IF SUB < 6
              GO TO NEXT-INSPREM.

           MOVE "SN N062FIN." TO VDU.
           COMPUTE VDUNUM2 = TOTAL-REPAY - OUR-CHGS
                           - LN-EXTCHG - LN-INTCHG
                           - LN-SERCHG - (LN-ORGTERM * LN-MAINTFEE).
           MOVE VDUNUM2 TO LN-PROCEEDS.
           PERFORM SCREENIO.

      ****************************************************************
      *    COMPUTE ANTICIPATED EARNINGS
      *    NOTE: THE FOLLOWINF IF IS TO ASSURE THAT WHEN LNSTAT.C
      *          IS CALLED AND REBATES ARE SET THAT THOSE ADJUSTMENTS
      *          ARE NOT OVER WRITTEN HERE, JTG 010329
      ****************************************************************
       COMPUTE-ANTICIPATED SECTION.
           IF ( LN-INT-REBATE = " " OR "N") AND
              ( LN-SC-REBATE  = " " OR "N") AND
              ( LN-MF-REBATE  = " " OR "N") AND
              ( LN-ANTICADJ(1) = 0        ) AND
              ( LN-ANTICADJ(3) = 0        ) AND
              ( LN-ANTICADJ(4) = 0        )
              IF LN-LOANTYPE = "I"
                 MOVE LN-SERCHG TO LN-ANTICERN(3)
                 MOVE 0 TO LN-ANTICERN(1) LN-ANTICERN(2)
                           LN-ANTICERN(4) LN-ANTICERN(5)
                           LN-ANTICERN(6)
              ELSE
                 ADD LN-INTCHG LN-EXTCHG GIVING LN-ANTICERN(1)
                 MOVE LN-SERCHG TO LN-ANTICERN(3)
                 COMPUTE LN-ANTICERN(4) = LN-ORGTERM * LN-MAINTFEE
                 MOVE 0 TO LN-ANTICERN(5) LN-ANTICERN(6)
              END-IF
              MOVE LN-ANTICERN(1) TO WK-ANTICERN(1)
              MOVE LN-ANTICERN(2) TO WK-ANTICERN(2)
              MOVE LN-ANTICERN(3) TO WK-ANTICERN(3)
              MOVE LN-ANTICERN(4) TO WK-ANTICERN(4)
              MOVE LN-ANTICERN(5) TO WK-ANTICERN(5)
              MOVE LN-ANTICERN(6) TO WK-ANTICERN(6)
              PERFORM ADJUST-ANTICERN
              MOVE WK-ANTICERN(1) TO LN-ANTICERN(1)
              MOVE WK-ANTICERN(2) TO LN-ANTICERN(2)
              MOVE WK-ANTICERN(3) TO LN-ANTICERN(3)
              MOVE WK-ANTICERN(4) TO LN-ANTICERN(4)
              MOVE WK-ANTICERN(5) TO LN-ANTICERN(5)
              MOVE WK-ANTICERN(6) TO LN-ANTICERN(6).

           MOVE "SN N062360." TO VDU.
           COMPUTE VDUNUM2 = LN-ANTICERN(1) + LN-ANTICERN(2)
                           + LN-ANTICERN(3) + LN-ANTICERN(4)
                           + LN-ANTICERN(5).
           IF NOT GP-INCL-OTH-IN-EARNED-N
              ADD LN-ANTICERN(6) TO VDUNUM2.
           PERFORM SCREENIO.

      *********************************
       MEMO-ENTRY SECTION.
      *********************************
           PERFORM SEND-MEMO.
       ENTER-MEMO.
           MOVE "E  A300M2." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS NOT = "00"
              GO TO ENTER-MEMO.
           MOVE VDUBUF TO LN-MESSAGE.
           IF LN-MESSAGE = " "
              MOVE "M1 M2." TO MESS
              PERFORM RESET-MESS.

      *********************************
       NEW-RECORD-SETUP SECTION.
      *********************************
           MOVE 1 TO NEW-FLAG.
           MOVE SPACES TO MEMO-WK.
           MOVE "S  A000SORTKEY." TO VDU.
           MOVE "* NEW *" TO VDUBUF.
           PERFORM SCREENIO.

           PERFORM CLEAR-LN-FILE.

           MOVE BR-STATE TO DEF-ORGST LN-ORGST LN-OWNST.
           MOVE BRANCH TO DEF-ORGBR LN-ORGBR.
           MOVE 1 TO DEF-PAYFREQ.
           MOVE 999.99 TO LN-SERVPER.
           MOVE 999.999 TO LN-ENDORATE LN-OVRDRATE.
           MOVE DEF-BUF TO WR-BUF.
           MOVE DEF-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.
           MOVE "N" TO STAT.


      *--{CS}---
           IF BULK-CONVERSION-FG = "B"
              MOVE "B" TO LN-CONVERCD
           ELSE
              MOVE "Y" TO LN-CONVERCD.
      *--{CS}--
           PERFORM PROTECT-FIELDS.
           MOVE 9999.99 TO LN-DLNO DL-DLNO.
           PERFORM GET-DL1.
           IF BR-LC-CODE = "Y"
              MOVE "SNNN020370." TO VDU
              MOVE DL-LC-CODE TO VDUNUM0 LN-LC-CODE
              PERFORM SCREENIO.
           MOVE 0 TO ELE.

      ***************************************************
       UPDATE-LN1-INSURANCE SECTION.
      ***************************************************

           PERFORM VARYING IEXP-LN FROM 1 BY 1 UNTIL IEXP-LN > 8
              PERFORM IEXP-INS-CALCULATIONS
              MOVE IEXP-EFFDATE TO LN-INSEFF-DATE(IEXP-LN)
              MOVE IEXP-EXPDATE TO LN-INSEXP-DATE(IEXP-LN)
              MOVE IEXP-INSCOMM TO LN-INSCOMM(IEXP-LN)

      *- WHEN PREMIUM IS ZERO SET ALL FIELDS TO ZEROS AND SPACES
              IF LN-INSPREM(IEXP-LN) = ZEROS
                 MOVE ZEROS TO LN-INSCOMP(IEXP-LN)
                               LN-INSEFF-DATE(IEXP-LN)
                               LN-INSEXP-DATE(IEXP-LN)
                               LN-INSCOVR(IEXP-LN)
                               LN-INSPREM(IEXP-LN)
                               LN-INSCOMM(IEXP-LN)
                 MOVE SPACES TO LN-INSOURS(IEXP-LN)
              END-IF
           END-PERFORM.

      *****************************
       DISPLAY-FIELDS SECTION.
      *****************************
           MOVE " " TO DISPLAY-BUF1.
           MOVE "Y" TO VALID-LOAN.
           MOVE "NVLD" TO LAST-READ.
           MOVE LN-ACCTNO TO D-LNNO.
           IF LN-PLCD = "P"
              MOVE "PL" TO D-P
              IF LN-PLTYPE NOT = "P"
                 MOVE LN-PLTYPE TO D-PLTYPE.
           IF LN-JDDATE NOT = 0
              MOVE "JDG" TO D-J.
           MOVE LN-CLASS TO LC-CODE D-LCNO.
           PERFORM GET-LC.
           MOVE LC-DESC TO D-LC-DESC.
           PERFORM GET-SP.

           MOVE 1 TO SUB.
       DISP-NEXT-BW.
           MOVE LN-SSNO(SUB) TO NUM-BW.
           PERFORM NUMBW-TO-ALPBW.
           MOVE " " TO BW-LNAME BW-FNAME.
           IF NUM-BW NOT = 0
              MOVE NUM-BW TO BW-SSNO
              PERFORM GET-BW1
              MOVE ALP-BW TO D-010(SUB)
              MOVE INSPECT-W TO D-011(SUB)
              MOVE LN-MAKERCD(SUB) TO D-012(SUB).
           IF SUB < 4
              ADD 1 TO SUB
              GO TO DISP-NEXT-BW.

           MOVE LN-ORGST TO D-050.
           MOVE LN-ORGBR TO D-051.
           MOVE LN-LOANDATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO D-060.

           MOVE "PU"     TO CD-TYPE.
           MOVE LN-PURCD TO CD-CODE D-070.
           PERFORM GET-CD.
           MOVE CD-DESC TO D-071.

           MOVE "SR"    TO CD-TYPE.
           MOVE LN-SRCD TO CD-CODE D-080.
           PERFORM GET-CD.
           MOVE CD-DESC TO D-081.

           MOVE "CO"      TO CD-TYPE.
           MOVE LN-COLLCD TO CD-CODE D-090.
           PERFORM GET-CD.
           MOVE CD-DESC TO D-091.
           MOVE CD-CO-SECURED TO SPTEST-CD-SECURED.

           MOVE LN-COLLID TO D-100.

           PERFORM GET-COLLATERAL.
           MOVE LTC-HHGS(1) TO COLLATERAL-BREAKOUT.
           MOVE COLLATERAL-30-1 TO D-110.
           MOVE COLLATERAL-30-2 TO D-111.

           MOVE DISPLAY-BUF1 TO WR-BUF.
           MOVE DISPLAY-LIST1 TO WR-LIST.
           PERFORM CALL-WRFORM.

           MOVE " " TO DISPLAY-BUF2.
           IF BR-LC-CODE = "Y"
              MOVE LN-LC-CODE TO D-370.
           MOVE LN-DLNO TO D-120 DL-DLNO.
           PERFORM GET-DL1.
           MOVE DL-SHORT TO D-121.
           MOVE LN-SUM-ORGTERM TO D-130.
           MOVE 1 TO D-140.
           MOVE LN-FILEDATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO D-150.
           MOVE LN-INTDATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO D-160.
           MOVE LN-1STPYDATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY   TO ALP-DATE-EDIT.
           MOVE LN-UNITPER-CD TO DATER-UNITPER-CD.
           MOVE LN-UNITPER-FREQ TO DATER-UNITPER-FREQ.
           PERFORM UNITPER-MASK.
           MOVE ALP-DATE-EDIT TO D-170.
           MOVE LN-APRATE TO D-180.
           MOVE LN-OVRDRATE TO D-190.
           MOVE LN-ENDORATE TO D-200.
           MOVE LN-REGPYAMT TO D-210.
           MOVE LN-1STPYAMT TO D-220.
           MOVE LN-SUM-EXTCHG TO D-230.
           MOVE LN-SUM-SERCHG TO D-240.
           MOVE LN-SUM-MAINTFEE TO D-241.
           MOVE LN-SUM-INTCHG TO D-250.
           MOVE LN-INSCODES TO D-260.

           MOVE 1 TO SUB.
       DISP-NEXT-IC.
           MOVE LN-INSCOMP(SUB) TO D-1COMP(SUB).
           MOVE LN-INSPREM(SUB) TO D-1PREM(SUB).
           MOVE SUB TO ITRM-SUB.
           PERFORM INS-TERM-CALCULATION.
           MOVE ITRM-INSTERM TO D-1TERM(SUB) LN-INSTRM(SUB).
           MOVE LN-INSCOVR(SUB) TO D-1COVR(SUB).
           MOVE LN-INSOURS(SUB) TO D-1IOURS(SUB).
           IF SUB < 4
              ADD 1 TO SUB
              GO TO DISP-NEXT-IC.

           MOVE 1 TO SUB.
           MOVE 5 TO ELE.
       DISP-NEXT-2IC.
           MOVE LN-INSCOMP(ELE) TO D-2COMP(SUB).
           MOVE LN-INSPREM(ELE) TO D-2PREM(SUB).
           IF GP-OT-INS-TERM = "Y"
              MOVE ELE TO ITRM-SUB
              PERFORM INS-TERM-CALCULATION
              MOVE ITRM-INSTERM TO D-2TERM(SUB) LN-INSTRM(ELE)
           ELSE
              MOVE LN-INSEXP-DATE(ELE) TO DATE-YYYYMMDD
              PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
              MOVE DATE-MMDDYY TO D-2EXP(SUB).
           MOVE LN-INSCOVR(ELE) TO D-2COVR(SUB).
           MOVE LN-INSOURS(ELE) TO D-2IOURS(SUB).
           IF ELE < 6
              ADD 1 TO SUB ELE
              GO TO DISP-NEXT-2IC.

           PERFORM SETUP-FEES-FOR-DISPLAY.

           MOVE LN-PRLNNO TO W-LNNO.
           MOVE W-ACCOUNT TO D-350.
           IF NOT GP-INCL-OTH-IN-EARNED-N
              ADD LN-ANTICERN(1) LN-ANTICERN(2) LN-ANTICERN(3)
                  LN-ANTICERN(4) LN-ANTICERN(5) LN-ANTICERN(6)
                 GIVING D-360
           ELSE
              ADD LN-ANTICERN(1) LN-ANTICERN(2) LN-ANTICERN(3)
                  LN-ANTICERN(4) LN-ANTICERN(5) GIVING D-360.
           MOVE DISPLAY-BUF2 TO WR-BUF.
           MOVE DISPLAY-LIST2 TO WR-LIST.
           PERFORM CALL-WRFORM.

           IF LN-MESSAGE NOT = " "
              PERFORM SEND-MEMO
              PERFORM ALARM.

           PERFORM CREATE-PC-A-RECORD.
           PERFORM SET-COLLATERAL-WORK.
           IF BR-AUTO-LNSTAT-WINDOW = "Y"
              PERFORM CALL-LNSTAT-WINDOW.

      ************************************
       SETUP-FEES-FOR-DISPLAY SECTION.
      ************************************
           MOVE 1 TO SUB2
                     SUB.
       DISPLAY-NEXT-FEE.
           MOVE LN-FEECODE(SUB) TO D-1FEECD(SUB2).
           MOVE LN-FEEAMT(SUB) TO D-1FEEAMT(SUB2).
           ADD 1 TO SUB.
           MOVE LN-FEECODE(SUB) TO D-2FEECD(SUB2).
           MOVE LN-FEEAMT(SUB) TO D-2FEEAMT(SUB2).
           IF SUB2 < 2
              ADD 1 TO SUB2
                       SUB
              GO TO DISPLAY-NEXT-FEE.
           IF LN-FEECODE(5) NOT = " "
              MOVE "->" TO D-2FEECD(2)
              ADD LN-FEEAMT(5) LN-FEEAMT(6) LN-FEEAMT(7)
                  LN-FEEAMT(8) LN-FEEAMT(9) LN-FEEAMT(10)
                         GIVING D-2FEEAMT(2).

      ******************************************************************
       TEST-ALLOT-LBOX SECTION.
      ******************************************************************
           PERFORM ACCOUNT-LBOX.
           IF IO-TYPE = "RTN"
              GO TO TEST-ALLOT-LBOX-EXIT.
           IF LBOX-CODE NOT = "Y"
              PERFORM ACCOUNT-ALLOTMENTS.
       TEST-ALLOT-LBOX-EXIT.
           EXIT.

      ******************************************************************
       ACCOUNT-ALLOTMENTS SECTION.
      ******************************************************************
       ACCT-ALLOT.
           MOVE SPACES TO LN-ALLOTCD IO-TYPE ALLOT-BUF.
           IF BR-ALLOTMNTS NOT = "Y"
              GO TO ACCOUNT-ALLOT-EXIT.
           MOVE "^, ACCOUNT UNDER AN ALLOTMENT PLAN ?" TO MESS
           PERFORM SEND-LEGEND.
           MOVE "R RA060SEL." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              MOVE "RTN" TO IO-TYPE
              GO TO ACCOUNT-ALLOT-EXIT.
           IF VDUSTATUS NOT = "00"
              GO TO ACCT-ALLOT.
           MOVE VDUBUF TO ALLOT-BUF.
           IF ALLOT-CODE = "Y"
              MOVE "Y" TO LN-ALLOTCD
           ELSE
              IF ALLOT-CODE NOT = "N"
                 GO TO ACCT-ALLOT.
           MOVE SPACES TO MESS.
           PERFORM SEND-LEGEND.
       ACCOUNT-ALLOT-EXIT.
           EXIT.

      ******************************************************************
       ACCOUNT-LBOX SECTION.
      ******************************************************************
       ACCT-LBOX.
           MOVE SPACES TO LN-LBOX IO-TYPE LBOX-BUF.
           IF BR-LBOX NOT = "Y"
              GO TO ACCOUNT-LBOX-EXIT.
           MOVE "^, ACCOUNT UNDER A LOCK BOX PLAN ?" TO MESS
           PERFORM SEND-LEGEND.
           MOVE "R RA060SEL." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              MOVE "RTN" TO IO-TYPE
              GO TO ACCOUNT-LBOX-EXIT.
           IF VDUSTATUS NOT = "00"
              GO TO ACCT-LBOX.
           MOVE VDUBUF TO LBOX-BUF.
           IF LBOX-CODE = "Y"
              MOVE "Y" TO LN-LBOX
           ELSE
              IF LBOX-CODE NOT = "N"
                 GO TO ACCT-LBOX.
           MOVE SPACES TO MESS.
           PERFORM SEND-LEGEND.
       ACCOUNT-LBOX-EXIT.
           EXIT.

      *******************************
       WRITE-MODULE SECTION.
      *******************************
           PERFORM OPEN-LN1-FILE.
           IF NEW-FLAG NOT = 0
              GO TO WRITE-RECORD.

           MOVE LN-REC TO CHANGED-LN-REC.
           IF ORIGINAL-LN-REC = CHANGED-LN-REC
              PERFORM UPDATE-LT
              GO TO END-WRITE-MODULE.
           PERFORM UPDATE-MSG.
           PERFORM READ-LN1-FILE.
           IF IO-FG NOT = 0
              PERFORM UNDERGONE-CHANGE
              PERFORM RECORD-DELETED
              GO TO END-WRITE-MODULE.
           IF LN-REC NOT = ORIGINAL-LN-REC
              PERFORM UNDERGONE-CHANGE
              PERFORM CHANGE-AGAIN
              GO TO END-WRITE-MODULE.
           MOVE CHANGED-LN-REC TO LN-REC.
           PERFORM UPDATE-LN1-INSURANCE.
           PERFORM REWRITE-LN1-FILE.

           PERFORM UPDATE-LT.
           GO TO END-WRITE-MODULE.

       WRITE-RECORD.
           PERFORM UPDATE-MSG.
           MOVE TRANS-DATE TO LN-ENTDATE.

           PERFORM UPDATE-LN1-INSURANCE.

           PERFORM APRS-RATE.
           MOVE APRS-SMPRATE TO LN-SMPRATE.
           MOVE APRS-EFFRATE TO LN-EFFRATE.
           IF SP-CAL-SET-EFFECTIVE(APRS-CTBL)
              MOVE LN-EFFRATE TO LN-OVRDRATE.
      *  JTG 010112 ADDED NEW 88  SP-CAL-SET-SMPRATE-OVRRIDE VALUE 4 MERC PR#J0
           IF SP-CAL-SET-SMPRATE-OVRRIDE(APRS-CTBL)
              MOVE LN-SMPRATE TO LN-OVRDRATE.

      * WORLD PR#1288 12/04/2017
      *  LN-GIFTNET "X" WILL SET BI-GIFTNET TO "X" WHICH WILL NOT
      *  CONSIDER THESE AS DUAL LOANS, WORLD USING THIS FOR TAX ADVANCE
      *  LOANS
           IF SP-NON-LOAN-FLAG = "Y"
              MOVE "X" TO LN-GIFTNET.
           IF SP-SKIP-TRW = "Y"
              MOVE SP-SKIP-TRW TO LN-GIFTNET.

           MOVE 0 TO LN-CREDITLIM.
           IF BR-AVAIL-CREDIT = "Y" OR "S"
              IF LC-CALC-HI-CREDIT NOT = "I"
                 PERFORM LOAN-CALCULATIONS
                 MOVE HIGH-CREDIT TO LN-CREDITLIM.

           PERFORM WRITE-LN1-FILE.
           IF IO-FG = 9
              PERFORM ALREADY-CREATED
           ELSE
              PERFORM UPDATE-LT.
       END-WRITE-MODULE.
           PERFORM CLOSE-LN1-FILE.

      ***************************************
       UPDATE-LT SECTION.
      ***************************************
           MOVE 6 TO WK-KEY.
           PERFORM OPEN-WK-FILE.
           PERFORM READ-WK-FILE.
           IF IO-FG = 9 OR WK-SAVE-RECORD = SPACES
              GO TO UPDATE-LT-EXIT.

           PERFORM CLEAR-LTC-FILE.

           PERFORM OPEN-LTC1-FILE.

           IF WK-DESC-1 NOT = SPACES OR WK-DESC-2 NOT = SPACES OR
               WK-HHGS-1 NOT = SPACES OR WK-HHGS-2 NOT = SPACES OR
                WK-HHGS-3 NOT = SPACES
              MOVE LTC-PATH-OWNBR TO LTC-BRNO
              MOVE LN-ACCTNO      TO LTC-ACCTNO
              MOVE 1              TO LTC-SEQNO

              MOVE WK-DESC-1      TO LTC-DESC(1)
              MOVE WK-MAKE-1      TO LTC-MAKE(1)
              MOVE WK-MODEL-1     TO LTC-MODEL(1)
              MOVE WK-YEAR-1      TO LTC-YEAR(1)
              MOVE WK-STYLE-1     TO LTC-STYLE(1)
              MOVE WK-CLASS-1     TO LTC-CLASS(1)
              MOVE WK-VIN-1       TO LTC-VIN(1)
              MOVE WK-TITLE-NO-1  TO LTC-TITLE-NO(1)
              MOVE WK-MILEAGE-1   TO LTC-MILEAGE(1)
              MOVE WK-VALUE-1     TO LTC-VALUE(1)
              MOVE WK-SELLPR-1    TO LTC-SELLPR(1)
              MOVE WK-MISC-AMT-1  TO LTC-MISC-AMT(1)
              MOVE 0              TO LTC-VIN-CHG-DATE(1)

              MOVE WK-DESC-2      TO LTC-DESC(2)
              MOVE WK-MAKE-2      TO LTC-MAKE(2)
              MOVE WK-MODEL-2     TO LTC-MODEL(2)
              MOVE WK-YEAR-2      TO LTC-YEAR(2)
              MOVE WK-STYLE-2     TO LTC-STYLE(2)
              MOVE WK-CLASS-2     TO LTC-CLASS(2)
              MOVE WK-VIN-2       TO LTC-VIN(2)
              MOVE WK-TITLE-NO-2  TO LTC-TITLE-NO(2)
              MOVE WK-MILEAGE-2   TO LTC-MILEAGE(2)
              MOVE WK-VALUE-2     TO LTC-VALUE(2)
              MOVE WK-SELLPR-2    TO LTC-SELLPR(2)
              MOVE WK-MISC-AMT-2  TO LTC-MISC-AMT(2)
              MOVE 0              TO LTC-VIN-CHG-DATE(2)

              MOVE WK-HHGS-1      TO LTC-HHGS(1)
              MOVE WK-HHGS-2      TO LTC-HHGS(2)
              MOVE WK-HHGS-3      TO LTC-HHGS(3)
              PERFORM WRITE-LTC1-FILE
              IF IO-FG NOT = 0
                 PERFORM REWRITE-LTC1-FILE.

           PERFORM CLOSE-LTC1-FILE.
       UPDATE-LT-EXIT.
           EXIT.


      *******************************
       DELETE-MODULE SECTION.
      *******************************
           IF NEW-FLAG NOT = 0
              MOVE "N" TO VALID-DELETE
              MOVE "ONLY DELETE AN EXISTING LOAN" TO MESS
              PERFORM SEND-MESS
              GO TO END-DELETE-MODULE.
           MOVE "VERIFICATION OF DELETE IN PROGRESS...." TO MESS.
           PERFORM SEND-LEGEND.
           PERFORM OPEN-LP1-FILE.
           PERFORM OPEN-LN1-FILE.
           PERFORM READ-LN1-FILE.
           IF IO-FG NOT = 0
              PERFORM UNDERGONE-CHANGE
              PERFORM RECORD-DELETED
              MOVE "N" TO VALID-DELETE
              GO TO END-DELETE-MODULE.
           IF LN-REC NOT = ORIGINAL-LN-REC
              PERFORM UNDERGONE-CHANGE
              PERFORM RECORD-CHANGED
              MOVE "N" TO VALID-DELETE
              GO TO END-DELETE-MODULE.

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
           PERFORM READ-LP1-FILE-NEXT.
           IF IO-FG = 9
              GO TO CONTINUE-DELETE.

           IF (LP-BRNO = LN-OWNBR) AND (LP-ACCTNO = LN-ACCTNO)
              MOVE "ACCOUNT HAS PAYMENT RECORD - CANT DELETE" TO MESS
              PERFORM SEND-MESS
              MOVE "N" TO VALID-DELETE
              GO TO END-DELETE-MODULE.

       CONTINUE-DELETE.
           MOVE "DELETING LOAN MASTER............" TO MESS.
           PERFORM SEND-LEGEND.

           PERFORM DELETE-LN1-FILE.

       END-DELETE-MODULE.
           PERFORM CLOSE-LN1-FILE.
           PERFORM CLOSE-LP1-FILE.

      ******************************************************
       DELETE-TRAILER-COLLATERAL SECTION.
      ******************************************************
           MOVE "DELETING LOAN TRAILER..............." TO MESS.
           PERFORM SEND-LEGEND.

           PERFORM OPEN-LTC1-FILE.

           MOVE LTC-PATH-OWNBR  TO LTC-BRNO
                                   QLTC1-WBEG-BRNO
                                   QLTC1-WEND-BRNO.
           MOVE LN-ACCTNO       TO LTC-ACCTNO
                                   QLTC1-WBEG-ACCTNO
                                   QLTC1-WEND-ACCTNO.
           MOVE 1               TO LTC-SEQNO
                                   QLTC1-WBEG-SEQNO.
           MOVE ALL "9"         TO QLTC1-WEND-SEQNO.

           PERFORM START-LTC1-FILE.
       NEXT-LT-DELETE.
           PERFORM READ-LTC1-FILE-NEXT.
           IF IO-FG = 9 OR (LTC-BRNO NOT = LTC-PATH-OWNBR) OR
                            (LTC-ACCTNO NOT = LN-ACCTNO)
              GO TO EXIT-DELETE-TRAILER.

           PERFORM DELETE-LTC1-FILE.

           GO TO NEXT-LT-DELETE.
       EXIT-DELETE-TRAILER.
           PERFORM CLOSE-LTC1-FILE.

      ****************************************************
       DELETE-NOTICE SECTION.
      ****************************************************
           PERFORM OPEN-NO1-FILE.
           MOVE LN-OWNBR  TO NO-BRNO.
           MOVE LN-ACCTNO TO NO-ACCTNO.
           PERFORM READ-NO1-FILE.
           IF IO-FG = 0
              PERFORM DELETE-NO1-FILE.

           PERFORM CLOSE-NO1-FILE.
       EXIT-DELETE-NOTICE.
           EXIT.

      ***************************
       VERIFY-MODULE SECTION.
      ***************************
           MOVE TRANS-DATE TO VERIFY-TODAY-DATE.
           IF LN-SSNO(1) = 0
              MOVE 0 TO BW-BDATE BW-REFDATE
           ELSE
              IF LN-INSURED = "S"
                 IF LN-SSNO(2) NOT = BW-SSNO
                    MOVE LN-SSNO(2) TO BW-SSNO
                    PERFORM READ-BW1-FILE
                    IF IO-FG = 9
                       MOVE 0 TO BW-BDATE BW-REFDATE
                    END-IF
                 END-IF
              ELSE
                 IF LN-SSNO(1) NOT = BW-SSNO
                    MOVE LN-SSNO(1) TO BW-SSNO
                    PERFORM READ-BW1-FILE
                    IF IO-FG = 9
                       MOVE 0 TO BW-BDATE BW-REFDATE.

           MOVE BW-BDATE   TO VERIFY-AGE.
           MOVE BW-REFDATE TO VERIFY-REFDATE.
           MOVE LN-APRATE  TO VERIFY-TERMCEIL-APR.
           MOVE 0          TO VERIFY-TERMCEIL-EFF.
           MOVE "N"        TO VERIFY-TERMCEIL-EFF-FG.
           MOVE " "        TO VERIFY-REJECT-TERM-FG.
           PERFORM VERIFY-TERMCEIL.

           PERFORM VERIFY-DATES.
           PERFORM VERIFY-PAYMENTS.
           PERFORM VERIFY-INSURANCE.

      ******************************************
       VERIFY-DATES SECTION.
      ******************************************

           IF LN-INTDATE < LN-LOANDATE
              MOVE "INTEREST DATE MAY NOT BE < LOAN DATE" TO MESS
              PERFORM SEND-MESS
              MOVE "N" TO VALID-LOAN.

           IF LN-1STPYDATE < LN-INTDATE
              MOVE "1ST PMT DATE MAY NOT BE < INTEREST DATE" TO MESS
              PERFORM SEND-MESS
              MOVE "N" TO VALID-LOAN.

           IF LN-CASHDATE NOT = 0
              IF LN-CASHDATE < LN-LOANDATE
                 MOVE "CASH DATE MAY NOT BE < LOAN-DATE" TO MESS
                 PERFORM SEND-MESS
                 MOVE "N" TO VALID-LOAN.

       VERIFY-DATES-EXIT.
           EXIT.

      ******************************************
       VERIFY-PAYMENTS SECTION.
      ******************************************
           COMPUTE N-WK1 = LN-REGPYAMT + LN-EXTCHG +
                           (LN-ORGTERM - 1) / 100.
           IF LN-1STPYAMT > N-WK1
              MOVE "INVALID ODD PAYMENT AMOUNT" TO MESS
              PERFORM SEND-MESS
              MOVE "N" TO VALID-LOAN.

           PERFORM COMPUTE-NOREGPY.
           COMPUTE N-WK1 =
                LN-1STPYAMT + LN-NOREGPY * LN-REGPYAMT
                                                + LN-LASTPYAMT.
           IF LN-LOANTYPE = "I"
              COMPUTE N-WK1 = N-WK1 - LN-EXTCHG - LN-INTCHG
                              - (LN-ORGTERM * LN-MAINTFEE).
           IF N-WK1 NOT = LN-LNAMT
              MOVE "PAYMENTS DO NOT EQUAL LOAN AMOUNT" TO MESS
              PERFORM SEND-MESS
              MOVE "N" TO VALID-LOAN.
           MOVE 1 TO SUB.
       NEXT-FEE.
           SUBTRACT LN-FEEAMT(SUB) FROM N-WK1.
           IF SUB < 6
              ADD 1 TO SUB
              GO TO NEXT-FEE.
           MOVE 1 TO SUB.
       NEXT-INS.
           IF LN-INSOURS(SUB) NOT = "M"
              SUBTRACT LN-INSPREM(SUB) FROM N-WK1.
           IF SUB < 5
              ADD 1 TO SUB
              GO TO NEXT-INS.
           SUBTRACT LN-SERCHG FROM N-WK1.
           IF N-WK1 < 0
              MOVE "AMOUNT FINANCED MAY NOT BE < 0" TO MESS
              PERFORM SEND-MESS
              MOVE "N" TO VALID-LOAN.

      ******************************************************
       VERIFY-INSURANCE SECTION.
      ******************************************************
           IF LN-INSURED NOT = "N"
              GO TO EXIT-VERIFY-INS.
           MOVE 0 TO IO-FG.
           IF LN-INSCOMP(1) NOT = 0 OR LN-INSPREM(1) NOT = 0
              MOVE 9 TO IO-FG.
           IF LN-INSCOMP(2) NOT = 0 OR LN-INSPREM(2) NOT = 0
              MOVE 9 TO IO-FG.
           IF LN-INSCOMP(3) NOT = 0 OR LN-INSPREM(3) NOT = 0
              MOVE 9 TO IO-FG.
           IF LN-INSCOMP(4) NOT = 0 OR LN-INSPREM(4) NOT = 0
              MOVE 9 TO IO-FG.
           IF IO-FG = 9
              MOVE "INS CO/PREMIUM MUST BE 0 FOR INS CD 'N'" TO MESS
              PERFORM SEND-MESS
              MOVE "N" TO VALID-LOAN.
       EXIT-VERIFY-INS.
           EXIT.

      *******************************************************
       VALID-INS-TYPES-ELIG SECTION.
      *******************************************************
           MOVE 0 TO IO-FG.
           IF LN-INSURED = "S" OR "J"
              IF LN-SSNO(2) = 0
                 MOVE 9 TO IO-FG
                 GO TO EXIT-VALID-INS-TYPES-ELIG.

      *  READ CURRENT STATE PARAMETER RECORD INORDER
      *  TO CHECK INSURANCE COMPANIES (SUBSCRIPTED) = 0.
           PERFORM GET-SP.

      * CHECK IF ADDITIONAL INSURANCES WERE PURCHASED
           IF BR-OTH-INS-LEVELS NOT = "Y"
              MOVE "N" TO CHFG(30) CHFG(31).

           IF LN-INSURED = "N"
              IF LN-INSTYPES NOT = "N"
                 MOVE 9 TO IO-FG
                 GO TO EXIT-VALID-INS-TYPES-ELIG
              ELSE
                 MOVE "N" TO CHFG(27) CHFG(28) CHFG(29) CHFG(30)
                 GO TO EXIT-VALID-INS-TYPES-ELIG.

           MOVE "N" TO CHFG(27) CHFG(28) CHFG(29) CHFG(30).

           IF LN-INSTYPES-LL
              MOVE " " TO CHFG(27)
              PERFORM LEVEL-INSURANCE.
           IF LN-INSTYPES-DL
              MOVE " " TO CHFG(27)
              PERFORM DECREASING-INSURANCE.
           IF LN-INSTYPES-AH
              MOVE " " TO CHFG(28)
              PERFORM ACCIDENT-INSURANCE.
           IF LN-INSTYPES-PP OR LN-INSTYPES-PP2
              MOVE " " TO CHFG(29)
              PERFORM PROPERTY-INSURANCE.
           IF LN-INSTYPES-O1
              MOVE " " TO CHFG(30)
              PERFORM O1-INSURANCE.
       EXIT-VALID-INS-TYPES-ELIG.
           EXIT.

      ******************************************************
       LEVEL-INSURANCE SECTION.
      ******************************************************
           IF LN-INSURED = "J"
              IF SP-INSCOM(4) = 0
                 MOVE "N" TO CHFG(27)
                 MOVE 9 TO IO-FG
                 MOVE "JOINT LEVEL LIFE NOT ALLOWED" TO MESS
                 PERFORM SEND-MESS
              ELSE
                 MOVE "S  N020270." TO VDU
                 MOVE SP-INSCOM(4) TO VDUNUM0 LN-INSCOMP(1)
                 PERFORM SCREENIO
      *CS 03/26/01
                 MOVE SP-INSFINCHG(4) TO LN-INSFINCHG(1)
           ELSE
              IF SP-INSCOM(3) = 0
                 MOVE "N" TO CHFG(27)
                 MOVE 9 TO IO-FG
                 MOVE "LEVEL LIFE NOT ALLOWED" TO MESS
                 PERFORM SEND-MESS
              ELSE
                 MOVE "S  N020270." TO VDU
                 MOVE SP-INSCOM(3) TO VDUNUM0 LN-INSCOMP(1)
                 PERFORM SCREENIO
      *CS 03/26/01
                 MOVE SP-INSFINCHG(3) TO LN-INSFINCHG(1).

           IF BR-AH-AND-CL = "Y"
              IF LN-INSTYPES-AH
                 IF NOT(LN-INSTYPES-DL OR LN-INSTYPES-LL)
                    MOVE "N" TO CHFG(27) CHFG(28)
                    MOVE 9 TO IO-FG
                    MOVE "MUST HAVE LIFE AND A&H" TO MESS
                    PERFORM SEND-MESS.

      ******************************************************
       DECREASING-INSURANCE SECTION.
      ******************************************************
           IF LN-INSURED = "J"
              IF SP-INSCOM(2) = 0
                 MOVE "N" TO CHFG(27)
                 MOVE 9 TO IO-FG
                 MOVE "JOINT DECLINING LIFE NOT ALLOWED" TO MESS
                 PERFORM SEND-MESS
              ELSE
                 MOVE "S  N020270." TO VDU
                 MOVE SP-INSCOM(2) TO VDUNUM0 LN-INSCOMP(1)
                 PERFORM SCREENIO
      *CS 03/26/01
                 MOVE SP-INSFINCHG(2) TO LN-INSFINCHG(1)
           ELSE
              IF SP-INSCOM(1) = 0
                 MOVE "N" TO CHFG(27)
                 MOVE 9 TO IO-FG
                 MOVE "DECLINING LIFE NOT ALLOWED" TO MESS
                 PERFORM SEND-MESS
              ELSE
                 MOVE "S  N020270." TO VDU
                 MOVE SP-INSCOM(1) TO VDUNUM0 LN-INSCOMP(1)
                 PERFORM SCREENIO
      *CS 03/26/01
                  MOVE SP-INSFINCHG(1) TO LN-INSFINCHG(1).

           IF BR-AH-AND-CL = "Y"
              IF LN-INSTYPES-AH
                 IF NOT(LN-INSTYPES-DL OR LN-INSTYPES-LL)
                    MOVE "N" TO CHFG(27) CHFG(28)
                    MOVE 9 TO IO-FG
                    MOVE "MUST HAVE LIFE AND A&H" TO MESS
                    PERFORM SEND-MESS.

      ******************************************************
       ACCIDENT-INSURANCE SECTION.
      ******************************************************
           IF SP-INSCOM(5) = 0
              MOVE "N" TO CHFG(28)
              MOVE 9 TO IO-FG
              MOVE "A & H NOT ALLOWED" TO MESS
              PERFORM SEND-MESS
           ELSE
              MOVE "S  N020280." TO VDU
              MOVE SP-INSCOM(5) TO VDUNUM0 LN-INSCOMP(2)
              PERFORM SCREENIO
      *CS  03/26/01
              MOVE SP-INSFINCHG(5) TO LN-INSFINCHG(2).

           IF BR-AH-AND-CL = "Y"
              IF LN-INSTYPES-AH
                 IF NOT(LN-INSTYPES-DL OR LN-INSTYPES-LL)
                 MOVE "N" TO CHFG(27) CHFG(28)
                 MOVE 9 TO IO-FG
                 MOVE "MUST HAVE LIFE AND A&H" TO MESS
                 PERFORM SEND-MESS.

      ******************************************************
       PROPERTY-INSURANCE SECTION.
      ******************************************************
           IF LN-INSTYPES-PP
              IF SP-INSCOM(6) = 0
                 MOVE "N" TO CHFG(29)
                 MOVE 9 TO IO-FG
                 MOVE "PROPERTY NOT ALLOWED" TO MESS
                 PERFORM SEND-MESS
              ELSE
                 MOVE "S  N020290." TO VDU
                 MOVE SP-INSCOM(6) TO VDUNUM0 LN-INSCOMP(3)
                 PERFORM SCREENIO
      *CS  03/26/01
                 MOVE SP-INSFINCHG(6)   TO LN-INSFINCHG(3)
           ELSE
              IF SP-INSCOM(7) = 0
                 MOVE "N" TO CHFG(29)
                 MOVE 9 TO IO-FG
                 MOVE "PROPERTY NOT ALLOWED" TO MESS
                 PERFORM SEND-MESS
              ELSE
                 MOVE "S  N020290." TO VDU
                 MOVE SP-INSCOM(7) TO VDUNUM0 LN-INSCOMP(3)
                 PERFORM SCREENIO
      *CS  03/26/01
                 MOVE SP-INSFINCHG(7) TO LN-INSFINCHG(3).

      ******************************************************
       O1-INSURANCE SECTION.
      ******************************************************
           IF SP-INSCOM(8) = 0
              MOVE "N" TO CHFG(30)
              MOVE 9 TO IO-FG
              MOVE "OTHER 1 INSURANCE NOT ALLOWED" TO MESS
              PERFORM SEND-MESS
           ELSE
              MOVE "S  N020300." TO VDU
              MOVE SP-INSCOM(8) TO VDUNUM0 LN-INSCOMP(4)
              PERFORM SCREENIO
      *CS  03/26/01
              MOVE SP-INSFINCHG(8) TO LN-INSFINCHG(4).

      ******************************************************
       RESTORE-TRAILER-REC SECTION.
      ******************************************************
           MOVE 6 TO WK-KEY.
           PERFORM OPEN-WK-FILE.
           PERFORM READ-WK-FILE.
           PERFORM CLOSE-WK-FILE.
      * INSPECT-X HOLDS FREE FROM BEFORE CALL
           IF INSPECT-X = WK-HHGS-1
              GO TO RESTORE-TRAILER-EXIT.
           MOVE "S  A300110." TO VDU.
           MOVE WK-HHGS-1 TO INSPECT-X.
           MOVE 1 TO VDU-I VDU-J.
       RESTORE-FREE-1A.
           MOVE INSPECT-Z(VDU-I) TO VDUTAB(VDU-J).
           IF VDU-I < 30
              ADD 1 TO VDU-I VDU-J
              GO TO RESTORE-FREE-1A.
           PERFORM SCREENIO.
           MOVE "S  A330111." TO VDU.
           MOVE 31 TO VDU-I.
           MOVE 1 TO VDU-J.
       RESTORE-FREE-1B.
           MOVE INSPECT-Z(VDU-I) TO VDUTAB(VDU-J).
           IF VDU-I < 63
              ADD 1 TO VDU-I VDU-J
              GO TO RESTORE-FREE-1B.
           PERFORM SCREENIO.

       RESTORE-TRAILER-EXIT.
           EXIT.

      ******************************************************
       CREATE-TRAILER SECTION.
      ******************************************************
           MOVE 6 TO WK-KEY.
           PERFORM OPEN-WK-FILE.
           PERFORM READ-WK-FILE.
           IF IO-FG = 0
              MOVE SPACE TO WK-SAVE-RECORD
              PERFORM REWRITE-WK-FILE
              GO TO CLOSE-TRAILER.
           MOVE " " TO WK-SAVE-RECORD.
           MOVE 0 TO WK-ELE.
           PERFORM WRITE-WK-FILE.
           IF IO-FG = 9
              GO TO IO-ERROR.
       CLOSE-TRAILER.
           PERFORM CLOSE-WK-FILE.
           MOVE " " TO CHFG(11).

      **************************************************
       SET-COLLATERAL-WORK SECTION.
      **************************************************
           PERFORM OPEN-LTC1-FILE.
           MOVE LTC-PATH-OWNBR TO LTC-BRNO.
           MOVE LN-ACCTNO      TO LTC-ACCTNO.
           MOVE 1              TO LTC-SEQNO.
           PERFORM READ-LTC1-FILE.
           IF IO-FG = 0
              MOVE LTC-DESC(1)         TO WK-DESC-1
              MOVE LTC-MAKE(1)         TO WK-MAKE-1
              MOVE LTC-MODEL(1)        TO WK-MODEL-1
              MOVE LTC-YEAR(1)         TO WK-YEAR-1
              MOVE LTC-STYLE(1)        TO WK-STYLE-1
              MOVE LTC-CLASS(1)        TO WK-CLASS-1
              MOVE LTC-VIN(1)          TO WK-VIN-1
              MOVE LTC-TITLE-NO(1)     TO WK-TITLE-NO-1
              MOVE LTC-MILEAGE(1)      TO WK-MILEAGE-1
              MOVE LTC-VALUE(1)        TO WK-VALUE-1
              MOVE LTC-SELLPR(1)       TO WK-SELLPR-1
              MOVE LTC-MISC-AMT(1)     TO WK-MISC-AMT-1
              MOVE LTC-VIN-CHG-DATE(1) TO WK-VIN-CHG-DATE-1
              MOVE LTC-DESC(2)         TO WK-DESC-2
              MOVE LTC-MAKE(2)         TO WK-MAKE-2
              MOVE LTC-MODEL(2)        TO WK-MODEL-2
              MOVE LTC-YEAR(2)         TO WK-YEAR-2
              MOVE LTC-STYLE(2)        TO WK-STYLE-2
              MOVE LTC-CLASS(2)        TO WK-CLASS-2
              MOVE LTC-VIN(2)          TO WK-VIN-2
              MOVE LTC-TITLE-NO(2)     TO WK-TITLE-NO-2
              MOVE LTC-MILEAGE(2)      TO WK-MILEAGE-2
              MOVE LTC-VALUE(2)        TO WK-VALUE-2
              MOVE LTC-SELLPR(2)       TO WK-SELLPR-2
              MOVE LTC-MISC-AMT(2)     TO WK-MISC-AMT-2
              MOVE LTC-VIN-CHG-DATE(2) TO WK-VIN-CHG-DATE-2
              MOVE SPACES              TO WK-AUTO-3
              MOVE 0      TO WK-MILEAGE-3 WK-VALUE-3 WK-SELLPR-3
                             WK-MISC-AMT-3 WK-VIN-CHG-DATE-3

              MOVE LTC-HHGS(1)         TO WK-HHGS-1
              MOVE LTC-HHGS(2)         TO WK-HHGS-2
              MOVE LTC-HHGS(3)         TO WK-HHGS-3
           ELSE
              MOVE SPACES TO WK-AUTO-1
              MOVE 0      TO WK-MILEAGE-1 WK-VALUE-1 WK-SELLPR-1
                             WK-MISC-AMT-1 WK-VIN-CHG-DATE-1
              MOVE SPACES TO WK-AUTO-2
              MOVE 0      TO WK-MILEAGE-2 WK-VALUE-2 WK-SELLPR-2
                             WK-MISC-AMT-2 WK-VIN-CHG-DATE-2
              MOVE SPACES TO WK-AUTO-3
              MOVE 0      TO WK-MILEAGE-3 WK-VALUE-3 WK-SELLPR-3
                             WK-MISC-AMT-3 WK-VIN-CHG-DATE-3
              MOVE SPACES TO WK-HHGS-1
              MOVE SPACES TO WK-HHGS-2
              MOVE SPACES TO WK-HHGS-3.

           PERFORM SAVE-TRAILER-REC.

           PERFORM CLOSE-LTC1-FILE.

       COPY "LIBLP/LPMNUI.CPY".
           IF LN-ACCTNO NUMERIC AND MENU-FG NOT = 99
              MOVE LN-ACCTNO TO LOAN-KEY.

       COPY "LIBWI/PFSWINDOWS.CPY".
       COPY "LIBGB/DATER.CPY".
       COPY "LIBLP/LPLTCCL.CPY".
       COPY "LIBLP/LPLNCL.CPY".
       COPY "LIBLP/LPPCCL.CPY".
       COPY "LIBLP/LPANTI.CPY".
       COPY "LIBLP/LPITRM.CPY".
       COPY "LIBLP/LPAMTS.CPY".
       COPY "LIBLP/LPSPEDL.CPY".
       COPY "LIBLP/LPBAGE.CPY".
       COPY "LIBLP/LPAPRS.CPY".
       COPY "LIBLP/SETCTBL.CPY".
       COPY "LIBLP/LPAPRZ.CPY".
       COPY "LIBLP/LPUPER.CPY".
       COPY "LIBLP/LPEDAY.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBWI/FEEENT.CPY".
       COPY "LIBWI/FEEENTLN.CPY".
       COPY "LIBLP/LPIEXP.CPY".
       COPY "LIBGB/GPENV.CPY".

      ******************************************************************
       AUTOPC-ROUTINE SECTION.
      ******************************************************************

           MOVE "A"                        TO AUTOPC-PC-ID.
           MOVE BRANCH                     TO AUTOPC-PC-BRANCH.
           MOVE LN-SSNO(1)                 TO AUTOPC-PC-SOC-SEC.
           MOVE "WI/AUTOPC" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH AUTOPC-BUF EXIT-PATHNAME.
           CANCEL FORM-PROGX.

       AUTOPC-ROUTINE-EXIT.
           EXIT.

      ******************************************************************
       CREATE-PC-A-RECORD SECTION.
      ******************************************************************

           PERFORM OPEN-LTA1-FILE.
           PERFORM OPEN-PC1-FILE.

           MOVE SPACES                     TO WK-LTA-RECORD.

      * CREATE AUTO INSURANCE TRAILER RECORD

           MOVE LTA-PATH-OWNBR             TO LTA-BRNO.
           MOVE LN-ACCTNO                  TO LTA-ACCTNO.
           MOVE 01                         TO LTA-SEQNO.
           PERFORM READ-LTA1-FILE.
           IF ( IO-BAD  )
              GO TO CREATE-PC-A-RECORD-WRITE.

           MOVE LTA-DLR-REQ               TO WK-LTA-DLR-REQ.
           MOVE LTA-INS-TYPE              TO WK-LTA-INS-TYPE.
           MOVE LTA-INS-CO                TO WK-LTA-INS-CO.
           MOVE LTA-POLICY-NO             TO WK-LTA-POLICY-NO.
           MOVE LTA-AGENCY                TO WK-LTA-AGENCY.
           MOVE LTA-AGENT                 TO WK-LTA-AGENT.

           MOVE LTA-ADR1                  TO WK-LTA-ADR1.
           MOVE LTA-ADR2                  TO WK-LTA-ADR2.
           MOVE LTA-CITY                  TO WK-LTA-CITY.
           MOVE LTA-STATE                 TO WK-LTA-STATE.
           MOVE LTA-ZIP                   TO WK-LTA-ZIP.
           MOVE LTA-PHONE                 TO WK-LTA-PHONE.
           MOVE LTA-EFF-DATE              TO WK-LTA-EFF-DATE.
           MOVE LTA-EXP-DATE              TO WK-LTA-EXP-DATE.
           MOVE LTA-CAN-DATE              TO WK-LTA-CAN-DATE.
           MOVE LTA-CONF-DATE             TO WK-LTA-CONF-DATE.
           MOVE LTA-LAST-FORM             TO WK-LTA-LAST-FORM.

           MOVE LTA-COV-TYPE              TO WK-LTA-COV-TYPE.
           MOVE LTA-BIL-COV               TO WK-LTA-BIL-COV.
           MOVE LTA-PDL-COV               TO WK-LTA-PDL-COV.
           MOVE LTA-PIP-COV               TO WK-LTA-PIP-COV.
           MOVE LTA-UMB-COV               TO WK-LTA-UMB-COV.
           MOVE LTA-COL-COV               TO WK-LTA-COL-COV.
           MOVE LTA-LOSS-PAYEE            TO WK-LTA-LOSS-PAYEE.
           MOVE LTA-COM-COV               TO WK-LTA-COM-COV.
           MOVE LTA-TRACK-ONLY            TO WK-LTA-TRACK-ONLY.
           MOVE LTA-INSURANCE             TO WK-LTA-INSURANCE.
           MOVE LTA-TRACK-MONTHS          TO WK-LTA-TRACK-MONTHS.
           MOVE LTA-INS-MONTHS            TO WK-LTA-INS-MONTHS.
           MOVE LTA-CPP-MONTHS            TO WK-LTA-CPP-MONTHS.

       CREATE-PC-A-RECORD-WRITE.

           IF ( WK-LTA-RECORD = SPACES )
              GO TO CREATE-PC-A-RECORD-END.

           PERFORM CLEAR-PC-FILE.

           MOVE "A"                        TO PC-ID.
           MOVE BRANCH                     TO PC-BRANCH.
           MOVE LN-SSNO(1)                 TO PC-SOC-SEC.
           MOVE WK-LTA-RECORD              TO PC-LTA-AUTO-RECORD.
           MOVE LN-LOANDATE                TO PC-LOANDATE.
           MOVE LN-ORGTERM                 TO PC-ORGTERM.
           MOVE LN-REGPYAMT                TO PC-REGPYAMT.
           MOVE PC-ID                      TO PC2-ID.
           MOVE BRANCH                     TO PC2-BRANCH.
           MOVE LN-SSNO(1)                 TO PC2-BWNAME.

           PERFORM WRITE-PC1-FILE.
           IF ( IO-BAD )
              PERFORM REWRITE-PC1-FILE.

       CREATE-PC-A-RECORD-END.

           PERFORM CLOSE-LTA1-FILE.
           PERFORM CLOSE-PC1-FILE.

       CREATE-PC-A-RECORD-EXIT.
           EXIT.

      ***********************************
       COLLATERAL-CREATE SECTION.
      ***********************************
           MOVE TEMP-PATH TO LTCREA-PATH.
           MOVE "WI/LTCREA" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH LTCREA-BUF EXIT-PATHNAME.
           CANCEL FORM-PROGX.
       EXIT-COLLATERAL-CREATE.
           EXIT.

      ***********************************
       COMPUTE-NOREGPY SECTION.
      ***********************************
           IF LN-ORGTERM = 1
              MOVE 0 TO LN-NOREGPY
           ELSE
              IF LN-ORGTERM = 2
                 MOVE 0 TO LN-NOREGPY
                 MOVE LN-REGPYAMT TO LN-LASTPYAMT
              ELSE
                 MOVE LN-REGPYAMT TO LN-LASTPYAMT
                 SUBTRACT 2 FROM LN-ORGTERM GIVING LN-NOREGPY.

      ******************************************************************
       CALL-LNSTAT-WINDOW SECTION.
      ******************************************************************
           MOVE "WI/LNSTAT" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH LN-REC EXIT-PATHNAME.
           CANCEL FORM-PROGX.

      *********************************************
       DO-FEEENT-WINDOW SECTION.
      *********************************************
           MOVE 1 TO ONE.
           MOVE SP-FEEFRMLA(1) TO FW-PROTECT-FEE1-FG.
           MOVE SP-FEEFRMLA(2) TO FW-PROTECT-FEE2-FG.
           PERFORM FEEENT-WIN.
           MOVE "LONIN" TO VDU-FORM-NAME.
           PERFORM SETUP-FEES-FOR-DISPLAY.
           MOVE D-330-340 TO WR-BUF.
           MOVE FEE-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      *******************************************************
       VERIFY-PASSWORD SECTION.
      *******************************************************
           MOVE " " TO PASSWDW-STAT.
           MOVE "LONIN0" TO PASSWDW-PGM.
           MOVE "PLEASE ENTER THE PASSWORD," TO PASSWDW-PROMPT1.
           MOVE "CL/PASSWD" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH PASSWDW-BUF.
           CANCEL FORM-PROGX.

      ***************************************************
       FORM-RESET SECTION.
      ***************************************************
           MOVE LPLONI-SYSDATE TO LONIN-SYSDATE.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DISPLAY LONIN-SCREEN UPON TCLP-MAIN-WINDOW-HANDLE.
           MOVE LONIN-FORM-FIELDS TO WR-BUF.
           MOVE LONIN-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      ***************************************************
       SETUP-LINK-INFO SECTION.
      ***************************************************
           MOVE LONIN-HELP--FILE TO HELP-LINK-FILE.
           MOVE LONIN-HELP--DIR TO HELP-LINK-DIR.

      ***************************************************
       VDU-CONVERT-FIELD SECTION.
      ***************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBLP/LONIN_EVA.CPY".
              COPY "LIBLP/LPCMNU_EVA.CPY".
              COPY "LIBWI/FEEENT_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      *********************************
       MISC-PARAGRAPHS SECTION.
      *********************************
       COPY "LIBGB/CHKSEC.CPY".
       COPY "LIBGB/SYSTEM.CPY".
       COPY "LIBGB/ACCESS.CPY".
       GET-CODE.
           MOVE "GC"     TO CDB-TYPE.
           MOVE LN-OWNBR TO CDB-BRANCH.
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
       GET-GI.
           PERFORM OPEN-GI1-FILE.
           PERFORM READ-GI1-FILE.
           PERFORM CLOSE-GI1-FILE.

       SAVE-TRAILER-REC.
           MOVE 6 TO WK-KEY.
           PERFORM OPEN-WK-FILE.
           PERFORM REWRITE-WK-FILE.
           PERFORM CLOSE-WK-FILE.
       GET-CD.
           PERFORM READ-CD1-FILE.
           IF IO-FG = 9
              MOVE " INVALID CODE" TO CD-DESC.
       DISPLAY-BULK-CONVERSION.
      *-{CS}----
           MOVE "S  A100TITLE." TO VDU.
           IF BULK-CONVERSION-FG = "B"
              MOVE "BULK PURCH"  TO VDUBUF
           ELSE
              MOVE "CONVERSION" TO VDUBUF.
           PERFORM SCREENIO.
       GET-DL1.
           PERFORM OPEN-DL1-FILE.
           PERFORM READ-DL1-FILE.
           PERFORM CLOSE-DL1-FILE.
           IF IO-FG = 9
              MOVE " INVALID DEALER" TO DL-SHORT.
       GET-BW1.
           PERFORM READ-BW1-FILE.
           IF IO-FG = 9
              MOVE " " TO BW-FNAME
              MOVE "NOT FOUND" TO BW-LNAME.
           PERFORM APPEND-BW-NAMES.
       GET-SP.
           MOVE LN-ORGST TO SP-ORGST.
           MOVE LN-SPRCLASS TO SP-SPRCLASS.
           MOVE LN-SUBCLASS TO SP-SUBCLASS.
           MOVE LN-LAWCODE TO SP-LAWCODE.
           PERFORM READ-SP1-FILE.
           IF IO-FG = 9
              MOVE "STATE PARA DELETED" TO MESS
              PERFORM SEND-MESS
              GO TO IO-ERROR.
       SEND-CD-DESC.
           PERFORM GET-CD.
           MOVE "S  A000  1." TO VDU.
           MOVE ELE TO VDUROW.
           MOVE CD-DESC TO VDUBUF.
           PERFORM SCREENIO.
       GET-COLLATERAL.
           PERFORM OPEN-LTC1-FILE.
           MOVE LTC-PATH-OWNBR TO LTC-BRNO.
           MOVE LN-ACCTNO      TO LTC-ACCTNO.
           MOVE 1              TO LTC-SEQNO.
           PERFORM READ-LTC1-FILE.
           IF IO-FG = 9
              MOVE SPACES TO LTC-HHGS(1).
           PERFORM CLOSE-LTC1-FILE.

       APPEND-BW-NAMES.
           MOVE BW-LNAME TO INSPECT-W.
           MOVE BW-FNAME TO ADD-ON.
           PERFORM ADD-IT.
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
           IF NUM-BW NOT = 0
              MOVE NUM-BW123 TO ALP-BW123
              MOVE NUM-BW45 TO ALP-BW45
              MOVE NUM-BW6789 TO ALP-BW6789
              MOVE "-" TO ALP-HYPHEN1 ALP-HYPHEN2.
       SEND-BW-NO-NAME.
           MOVE BW-SSNO TO NUM-BW.
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
       SEND-BW-MAKERCD.
           MOVE "S  A010  2." TO VDU.
           MOVE ELE TO VDUROW.
           MOVE LN-MAKERCD(ELE) TO VDUBUF.
           PERFORM SCREENIO.
       SEND-MEMO.
           MOVE "S  A060M1." TO VDU.
           MOVE "MEMO: " TO VDUBUF.
           PERFORM SCREENIO.
           MOVE "S  A300M2." TO VDU.
           MOVE LN-MESSAGE TO VDUBUF.
           PERFORM SCREENIO.
       NOT-FOUND.
           MOVE "NOT FOUND" TO MESS.
           PERFORM SEND-MESS.
       GI-NOT-FOUND.
           MOVE "G/L INTERFACE RECORD NOT FOUND" TO MESS
           PERFORM SEND-MESS.
       SP-INVALID.
           MOVE "ENTRY INCONSISTENT WITH STATE PARAMETERS" TO MESS.
           PERFORM SEND-MESS.
       NOT-DIRECT.
           MOVE "INVALID FOR DIRECT LOAN ENTRY" TO MESS.
           PERFORM SEND-MESS.
       UPDATE-MSG.
           MOVE "UPDATING LOAN MASTER.........." TO MESS.
           PERFORM SEND-LEGEND.
       MSG-LINE.
           MOVE MESS TO TMMSG-MESS.
           PERFORM CALL-TMMSG.
       SEND-LEGEND.
           MOVE MESS TO WR-BUF.
           MOVE MSEL-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.
       RESET-LEGEND.
           MOVE " " TO MESS.
           PERFORM SEND-LEGEND.
       SEND-WSEL-LEGEND.
           MOVE MESS TO WR-BUF.
           MOVE WSEL-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.
       RECORD-DELETED.
           MOVE "RECORD WAS DELETED!" TO MESS.
           PERFORM SEND-MESS.
       RECORD-CHANGED.
           MOVE "CHANGES WERE MADE TO THIS RECORD!" TO MESS.
           PERFORM SEND-MESS.
       CHANGE-AGAIN.
           MOVE "PLEASE RE-INPUT YOUR CHANGES!" TO MESS.
           PERFORM SEND-MESS.
       ALREADY-CREATED.
           MOVE "RECORD HAS BEEN ALREADY CREATED!" TO MESS.
           PERFORM SEND-MESS.
       UNDERGONE-CHANGE.
           MOVE
           "THIS RECORD HAS UNDERGONE CHANGE BY ANOTHER USER,"
                TO MESS
           PERFORM SEND-MESS.
       PROTECT-FIELDS.
           MOVE "N" TO  CHFG(5) CHFG(12) CHFG(15) CHFG(16)
                        CHFG(19) CHFG(20) CHFG(35).
       UNPROTECT-FIELDS.
           MOVE " " TO   CHFG(12) CHFG(15) CHFG(16)
                         CHFG(19) CHFG(20) CHFG(35).
      *-----------------------------------------------------------------
       CONDITIONAL-SEND-MESS.
           PERFORM SEND-MESS.

      *********************************
       IO-ROUTINE SECTION.
      *********************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBLP/LPSPGS_SQL.CPY".
       COPY "LIBLP/LPPCGS_SQL.CPY".
       COPY "LIBLP/LPLTCGS_SQL.CPY".
       COPY "LIBLP/LPNOGS_SQL.CPY".
       COPY "LIBLP/LPLTAGS_SQL.CPY".
       COPY "LIBLP/LPLPGS_SQL.CPY".
       COPY "LIBLP/LPLNGS_SQL.CPY".
       COPY "LIBLP/LPLCGS_SQL.CPY".
       COPY "LIBLP/LPICGS_SQL.CPY".
       COPY "LIBLP/LPDLGS_SQL.CPY".
       COPY "LIBLP/LPCDBGS_SQL.CPY".
       COPY "LIBLP/LPCDGS_SQL.CPY".
       COPY "LIBLP/LPBWGS_SQL.CPY".
       COPY "LIBGL/GLGIGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBGL/GLGI1RN.CPY".
       COPY "LIBLP/LPLTA1IN.CPY".
       COPY "LIBLP/LPLTC1IN.CPY".
       COPY "LIBLP/LPNO1IN.CPY".
       COPY "LIBLP/LPPC1IN.CPY".
       COPY "LIBLP/LPWKI.CPY".
       COPY "LIBLP/LPDL1RN.CPY".
       COPY "LIBLP/LPLC1RN.CPY".
       COPY "LIBLP/LPLN1IN.CPY".
       COPY "LIBLP/LPLN4RN.CPY".
       COPY "LIBLP/LPLN5RN.CPY".
      *COPY "LIBLP/LPLN6RN.CPY".
      *COPY "LIBLP/LPLN7RN.CPY".
       COPY "LIBLP/LPLP1IN.CPY".
       COPY "LIBLP/LPSP1RN.CPY".
       COPY "LIBLP/LPBW1RN.CPY".
       COPY "LIBLP/LPBW2RN.CPY".
       COPY "LIBLP/LPBW3RN.CPY".
       COPY "LIBLP/LPBW4RN.CPY".
       COPY "LIBLP/LPIC1RN.CPY".
       COPY "LIBLP/LPCD1RN.CPY".
       COPY "LIBLP/LPCDB1RN.CPY".
       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO ERRCD.

      *********************************
       END-ROUTINE SECTION.
      *********************************
           PERFORM CLOSE-FILES.
       EXIT-PROG.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY LONIN-SCREEN.
           PERFORM CLOSE-WINDOW UNTIL WINDOWS-OPEN = 0.
       STOP-RUN.
           EXIT PROGRAM.
