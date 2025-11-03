       IDENTIFICATION DIVISION.
       PROGRAM-ID.      LONPC0.
       DATE-WRITTEN.    012484.
      ******************************************************************
      *                 (LP)
      *  DESCRIPTION:   DIRECT LOANS (NON SALES FINANCE)
      *                 NEW LOAN ENTRY - PAGE 1
      *  N O T E:
      *          THIS PROGRAM USES A48 PAYMENT SCHEDULE LOGIC
      * REV :
      *  MJD 081095 ADDED LOGIC FOR GP-THE-ONLY-SF-SRCD
      *  MJD 010996 ADDED LOGIC TO PREVENT ENTRY OF "MN_" INS CODE.
      *  BLL 012496 CANNOT BOOK LOAN IF FINANCE AMT > GI APPROVAL AMT
      *  BAH 012596 PASSWORDED 1STPYDATE < 30 DAYS FROM LOANDATE, E097
      *  JTG 050896 ADDED LOGIC FOR "LR" LOAN RE-SCHEDULE TRANSACTION
      *  MJD 071996 ADDED LOGIC TO PRINT LN-1STPYDATE AS MM/DDWYY
      *             FOR WEEKLY UNIT PERIODS.
      *  KEC 040897 REVISED TO PROPERLY ACCESS NEW APPLICATION MODULE.
      *  MJD 042397 CHANGED UPDATE TO CALL CL/LNVERI.C INSTEAD OF
      *             LP/LONPC1.C. ADDED ERRORS-EXIST TO WORKING STORAGE.
      *  MJD 060397 ADDED SCICAN-TRANS-DATE.
      *  JTG 060397 ADDED TEST OF 'O3' RE: BR-OTH-INS-LEVELS
      *  JTG 061697 CHANGED TO PERFORM VERIFY-INSURANCE VARYING SUB
      *             FROM 1 BY 1 UNTIL SUB > 4 (O1)
      *  JTG 081497 ADDED SET OF LN-GIFTNET WORLD A90
      *  KEC 090397 ***CONVERSION-A90-TO-A10***
      *                ADDED SP-DUAL-IN-BRANCH AND SP-DUAL-IN-STATE
      *                ALONG WITH ADDITIONAL ROUTINES IN ORDER TO USE
      *                THE NEW FIELDS.
      *  GLF 090897 REPLACED 'IO' ON GP-FILE WITH GET 'GPENV'
      *  KEC 031198 PREVENTED USE OF 'F5-CREATE BORROWER' ON ELE = 1
      *              WITH A LOAN APPLICATION.
      *   CS 031398 ADDED TEST OF LC-APP-REQ-FOR-LOAN IF BR-APP-REQUIRED-
      *             FOR-INPUT = "Y"
      *  KEC 031698 REVISED THE LOAN APPROVAL CHECKS/WINDOW SO TO
      *              INCORPORATE 'GR-LN-APPROVAL-LIMIT'.
      *   CS 041798 MOVE BW-ORIGIN TO PD-ORIGIN TO UPDATE LN-ORIGIN
      *   CS 042998 ADDED RENEWAL SOURCE CODES "RB" AND "RO"
      *  KEC 090498 ADDED 'MOVE LC-LOANTYPE TO PD-LOANTYPE';
      *              FOR REGENCY REVOLVING.
      *  JTG 101398 ADDED SET OF PD-REVOLVING FROM LC-REVOLVING, REGENCY REVOL
      *  JTG 121698 DISPLAYED 'R' VIA UNITPER-MASK LOGIC FOR REVOLVING ACCOUNT
      *  GLF 031299 ADD WI/TFCCDS FOR TFC ENTRY OF LPO,REP CODE,PROCESS CODE
      *  BAH 990525 FORCE A PRIOR LOAN # IF A RENEWAL SOURCE CODE, CALCULATE
      *             PRIOR LNAMT, AND ADJUST PD-PROCEEDS
      *  MJD 990803 ADDED GP-INCL-OTH-IN-EARNED FLAG. IF "Y" THEN INCLUDE
      *             EARNINGS BUCKET 6 (OTHER) IN ACCUMULATED TOTALS
      *             OTHERWISE EXCLUDE BUCKET 6.
      *  GLF 990805 ADDED GP-VALID-BDATE. IF "Y" AND THERE IS NOT A VALID
      *             BIRTH DATE FOR SSNO BEING ENTERED, DO NOT ALLOW ENTRY
      *             OF THE SSNO, REGENCY #57
      *  JTG 990915 CHANGED COPY FROM LPAPRZWL TO LPAPRZLW
      * KEC 2000.1221 {PR#00000} ADDED CHECK TO NOT ALLOW OUTSIDE VENDOR
      *                LOAN APPLICATION TO BE USE IN THE F5-KEY SCAN.
      *
      * KEC 2000.0128 {PR#00000} ADDED NEW ROUTINE FOR VERIFYING THE
      *                LOAN APPLICATION BEFORE UPDATING THE LOAN;
      *                LIBLP/VERIFYAPP(W)
      * MJD 000207 ADDED GP-OT-INS-TERM (TFC PR#48) IF 'Y' THEN ALLOW
      *            ALLOW TERM ENTRY FOR O2O3O4O5 INSURANCE.
      * BLV 000313 ADD ENTRY OF WCBC-CREDITLIM IF LC-CALC-HI-CREDIT = W,
      *            WORLD PR# 225
      * BAH 000329 O1 INSURANCE WAS LOOKING AT SP-INSCOM(6) WHICH IS PROPERTY,
      *            SHOULD HAVE BEEN (8).  ANDRIA, EXCEL
      * KEC 000530 {PR#00048} TFC [JUDY BRANIFF];
      *            ADDED THE FOLLOWING 'IF' CONDITION FOR ENTERING THE
      *            INSURANCE TERM (GP-OT-INS-TERM = "Y"):
      *                  IF ( VDUNUM0 > 360 )
      *                     GO TO ENTER-IC-TERM.
      *  MJD 000914 ADDED LOGIC TO ASK LOCK BOX QUESTION  (MFC PR#227)
      *  CS  010326 ADDED TOTAL-FEEFINCHG AND TOTAL-INSFINCHG IN
      *             COMPUTE-FINANCE CHARGE AND CALCULATION OF APR-FINANCED
      *  BAH 011203 ADDED SP-INSEXCEPT 14, REGENCY  (SP-ORGTERM-MAXIMUM) PR#1657
      *
      * KEC 020621 {PR#01823} REGIONAL ACCEPTANCE <A15> [KEVIN MULLEN]
      *            ADDED LOGIC FOR USING 'CD-GC-FEE-MAXAMT'
      * BLV 020820 IF GP-ENTER-LPO-FG = "Y", OPEN TFC WINDOW FOR ENTRY OF
      *            LPO, SOAUTO PR# 1885
      *  BAH 031001 ADDED SP-INSCHGABLE '2' & '1',SP-INS-AMTFIN-2PMTS &
      *             SP-INS-AMTFIN-1PMTS FOR SIGNAL (WISC) PR# 2157
      *  BAH 050222 WHEN ENTERRING SUB CLASS, YOU DONT KNOW THE LOANDATE YET TO
      *             CHECK IF SPR IS EXPIRED, SO IT WAS JUST GRABBING THE FIRST
      *             LAWCODE FOUND TO DISPLAY THE DESCRIPTION, WHICH ISNT
      *             CORRECT IF THEY EXPIRE AND BUILD A NEW ONE WITH A DIFFERENT
      *             DESCRIPTION, VERY CONFUSING...AT LEAST SKIP EXPIRED SPR.
      *             RIVERSIDE PL# 467
      *
      * KEC 050524 {PR#02516} PREMIER ADVISORY <A15> [JUDY]
      *            ADDED PRIOR TO 'PERFORM VERIFY-LOAN-APPROVAL':
      *
      *    IF ( MA-EXISTS-YES )
      *       MOVE LNAPPROVAL---CREATION-APP  TO LNAPPROVAL-CREATION
      *    ELSE
      *       MOVE LNAPPROVAL---CREATION-LONP TO LNAPPROVAL-CREATION.
      *
      * BAH 051128 UNDO CHANGE FROM 050222 PL# 467, WOULD NOT LET YOU BOOK A
      *            LOAN WITH AN EXPIRED SPR, AND YOU SHOULD BE ABLE TO.
      *            INSTEAD OF SKIPPING EXPIRED SPR IN READ-NEXT, JUST REDISPLAY
      *            THE SUBCLASS DESCRIPTION AFTER ENTERING THE LOANDATE.
      * KEC 060721 PR#00127 REGIONAL ACCEPT. ADDED LOGIC FOR "AUTOPC".
      * MJD 080926 ADDED LOGIC TO ALLOW MAINT FEES ON IB LOANS. THE TRIGGER
      *            IS GP-ALLOW-MF-ON-IB.  IF IB AND FLAG IS YESS THEN ALLOW
      *            MAINT FEES TO BE ACCUMULATED IN ANTICERN(4). WORLD PR#0590
      * BAH 090821 ADDED SP-INSCHGABLE "E" FOR REGMGM PR# 3437
      * BAH 110523 PASSWORD PROTECT ADDON SOURCE CODES, ET, LP, NP, RT, ST
      *            AND RB, REGMGM PR# 3792
      * BAH 121206 ADDED PC1-FILE FOR SAVING COLLATERAL, PEOPLES PR#4170
      * KEC 140211 PR#967 WORLD ADDED LOGIC FOR WORLD TO DO LOAN APPS.
      *            ADDED LIBLP/WRLDAPP(W)
      *            ADDED LOANAPP-* FIELDS
      *            MODIFIED EXISTING FIELDS FOR WORLD'S NEEDS.
      * BAH 140814 SET LPLOAN-CALLED-FROM FOR LNVERI, CAUSING MISSING FIELD
      * BAH 140814 ADDED TEST FOR LPSMNU IN HELP
      *  CS 141112 AT END OF BORROWER-CREATE SECTION, FORCED BWCREA-NO TO 
      *            VDUNUM0 WHICH IS MOVED TO NUM-BW AND THEN TO PD-SSNO,
      *            BW-SSNO. WHEN YOU F5-BORROWER-CREATE AND COME BACK TO
      *            LOAN INPUT, DIDNT HAVE CORRECT VALUE FOR VDUNUM0, IT WAS
      *            ALPHA
      *  CS 150814 IF GP-I-AM-EXCEL, TEST THE FEECODE AMOUNT AGAINST CD-
      *            GC-BRANCH-COMM-AMOUNT  EXCEL PR#4821
      *  CS 150922 ADD TEST FOR CD-GC-BRANCH-COMM-AMOUNT TO TEST FOR 202020 OR
      *            020202 PR#4821
      * KEC 2016.0315 {PD#00003} PARADATA <A30> [JAY CISZEWSKI]
      *         CHANGED CD-FEE???? TO CD-GC-????.
      * KEC 2016.0329 {PD#00003} PARADATA <A30> [JAY CISZEWSKI]
      *         CHANGED ELSCAN WORKERS TO MATCH CD1-KEY FOR EL LAYOUT.
      * KEC 2016.0516 {PD#00003} PARADATA <A30> [JAY CISZEWSKI]
      *         CHANGED USE OF APPNO TO APP-NUMBER FOR MA1-KEY USAGE.
      * KEC 2016.0621 {PD#00003} PARADATA <A30> [JAY CISZEWSKI]
      *         REMOVED USE OF THE MX-FILE [LIBLA/LAFDMX].
      *         REMOVED USE OF OLD STATUS ROUTINES [LIBLA/MASTATUS(W)].
      *         ADDED USE OF NEW MA-STATUS-INFO FIELDS; AS WELL AS
      *         ADDED USE OF NEW ROUTINES [LIBLA/LAFRMA & LAWSMA].
      *  CS 160805 CHANGED SCSCAN-KEY TO SCSCAN-SP1-KEY
      * KEC 2016.0818 {PD#00003} PARADATA <A30> [JAY CISZEWSKI]
      *         REMOVED USE OF THE BA-FILE [LIBLA/LAFDMX].
      *         REVISED LA-REC LAYOUT.
      * MJD 160823 ADDED LC-CALC-HI-CREDIT = "L"              61000/1180
      * BLM 160824 ADDED UPDATE OF SAVE-PRPDUE-DAYS & SAVE-PRLNAMT WHICH ARE NOW
      *            USED IN LNAPPROVAL FOR WORLD, WORLD PR# 1182
      * BAH 170612 ACTIVATE 8 DIGIT DATES, PD0006
      * KEC 180824 LOAN APP MODULE REMOVAL FROM A30 [LARM30]
      * BAH 181001 GLOBAL BWFILE
      * BAH 190122 SPLIT CDFILE
      * BAH 20190627 REMOVED PD-REVOLVING, PT0153
      * BAH 20191209 NEW SP-DUAL-IN-STATE VALUE "C" FOR CLASS, STOP LOANS
      *              WITHIN SAME CLASS (TAX ADVANCE LOANS), #1407
      * BAH 20200617 CHANGED PC-LT-RECORD TO PC-LTC-RECORD, ADDED LPPCCL CLEAR
      * BAH 2022.0314 REMOVED EXT-SPANISH-FORMS AND ALL MX FORMS
      * BAH 2024.0315 REMOVED BISCAN-LIASMLN AND BISCAN-PRLOANS
      * BAH 2024.1107 PUT LNAPPROVALW AND LNAPPROVAL BACK IN HERE LIKE A15,
      *               NOT SURE WHY IT GOT REMOVED, THINK BY MISTAKE S35Q-143
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
           "@(#)LP/LONPC0.CBL S35 02/20/24-15:29:21 >".
      **********************************
      *       F I L E   I F N          *
      **********************************
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBGB/GB01GR.CPY".
       COPY "LIBGB/GB01GR_SQL.CPY".
       COPY "LIBGB/GBWSGR.CPY".
       COPY "LIBLP/LP01DL.CPY".
       COPY "LIBLP/LP01DL_SQL.CPY".
       COPY "LIBLP/LPWSDL.CPY".
       COPY "LIBLP/LP01LC.CPY".
       COPY "LIBLP/LP01LC_SQL.CPY".
       COPY "LIBLP/LPWSLC.CPY".
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
       COPY "LIBLP/LP01PC.CPY".
       COPY "LIBLP/LP01PC_SQL.CPY".
       COPY "LIBLP/LPWSPC.CPY".
       COPY "LIBGL/GL01GI.CPY".
       COPY "LIBGL/GL01GI_SQL.CPY".
       COPY "LIBGL/GLWSGI.CPY".

       01  WK-KEY          PIC 9(2).

       01  HOLD-VDUSTATUS                       PIC X(02)  VALUE "00".

      *-----------------------------------------------------------------
       01  WS-MESSAGES.
           03  MESSAGE-CANNOT-CHANGE-SSNO.
               05  FILLER                       PIC X(43)  VALUE
                   "CANNOT CHANGE THIS NUMBER --- 'F1' TO CLEAR".
           03  MESSAGE-MUST-CREATE-BW-1.
               05  FILLER                       PIC X(38)  VALUE
                   "MUST CREATE BORROWER BEFORE CONTINUING".

      *----------------------------------------------------------------
      * REPLACED SEVERAL OCCURANCES OF IO-FG WITH BW-ERROR-YN. IO-FG WAS
      * USED AS AN ERROR FLAG UNRELATED TO I/O. ADDING LA-FILE READ
      * CHANGED IO-FG WHICH AFFECTED FLOW OF PROGRAM.
      *----------------------------------------------------------------
       01  BW-ERROR-YN                   PIC X        VALUE "N".
           88  BW-ERROR-NO                            VALUE "N".
           88  BW-ERROR-YES                           VALUE "Y".

      *-----------------------------------------------------------------
       01  ONE             PIC S9   COMP.
       01  ELE             PIC 9(6) COMP.
       01  ACTION.
           03  CHKEY       PIC X(4).
           03  CHNO        PIC 9(2).
       01  IO-TYPE         PIC X(3).
       01  NEW-FLAG        PIC 9     COMP.
       01  ERRORS-EXIST    PIC X     VALUE " ".
       01  FIRST-TIME      PIC 9     COMP   VALUE 0.
       01  FIRST-ELE       PIC 99    COMP   VALUE 0.
       01  SAVE-SSNO       PIC 9(10) COMP.

       01  IX1             PIC 99   COMP.
       01  IX2             PIC 99   COMP.
       01  SUB             PIC 9(6) COMP.
       01  SP-SUB          PIC 9(6) COMP.
       01  PD-SUB          PIC 9(6) COMP.
       01  OVER-20         PIC 9(6).
       01  I-LEVEL         PIC 9(6) COMP.
       01  INSPECT-W       PIC X(86).
       01  INSPECT-X REDEFINES INSPECT-W.
           03  INSPECT-Z   PIC X OCCURS 86 TIMES.
       01  ADD-ON          PIC X(25).
       01  NON-MEMO-INS    PIC 9(6)V99 COMP.
       01  DEFAULT-APR-FG  PIC X.
       01  F3-COLLATERAL         PIC X       VALUE " ".
           88  F3-YES                        VALUE "Y".
       01  AMT-HOLDER      PIC 9(6)V99  COMP.
       01  PREM-CHANGE     PIC X        VALUE " ".
       01  PREM-AMOUNT     PIC 9(4)V99  COMP  VALUE 0.
       01  TEST-COLLCD     PIC XX.
           88  AUTO-COLLCD VALUE "AU" "AC" "AL" "RP" "HP" "AW".

       01  LC-INFO-LIST      PIC X(10)   VALUE "38P:S 380.".
       01  LC-INFO-BUF       PIC X(14).

       01  USERID-GROUP-NAME.
           03  USER-GROUP           PIC X(3).
           03  FILLER               PIC X(2).
       01  CASE-BUF                 PIC X(4).
       01  SRCD-WORKER              PIC S9(7)V99 COMP.

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
       01  SAVE-PRLNAMT    PIC 9(6)V99 VALUE 0.
       01  SAVE-PRPDUE-DAYS PIC S9(5) VALUE 0.

      ********************************
      *   S C R E E N   B U F F E R  *
      ********************************
       01  DISPLAY-BUF1        VALUE " ".
           03  D-LCNO          PIC Z(8)9.
           03  D-LC-DESC       PIC X(16).
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
           03  D-380           PIC ZZ.
           03  D-120           PIC 9(4).99  BLANK ZERO.
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
           03  D-240           PIC Z(5)9.99  BLANK ZERO.
           03  D-241           PIC ZZZ.99     BLANK ZERO.
           03  D-250           PIC Z(5)9.99  BLANK ZERO.
           03  D-260           PIC XXX.
           03  D-270-300       OCCURS 4.
               05  D-1COMP      PIC 99        BLANK ZERO.
               05  D-1IOUR      PIC X.
               05  D-1TERM      PIC ZZ9       BLANK ZERO.
               05  D-1COVR      PIC Z(5)9.99  BLANK ZERO.
               05  D-1PREM      PIC Z(3)9.99  BLANK ZERO.
           03  D-310-320       OCCURS 2.
               05  D-2COMP      PIC 99        BLANK ZERO.
               05  D-2IOUR      PIC X.
               05  D-2EXP       PIC 9(6)      BLANK ZERO.
               05  D-2TERM  REDEFINES D-2EXP
                                PIC Z(6)      BLANK ZERO.
               05  D-2COVR      PIC Z(5)9.99  BLANK ZERO.
               05  D-2PREM      PIC Z(3)9.99  BLANK ZERO.
           03  D-330-350       OCCURS 3.
               05  D-1FEECD     PIC XX.
               05  D-1FEEAMT    PIC Z(3)9.99  BLANK ZERO.
               05  D-2FEECD     PIC XX.
               05  D-2FEEAMT    PIC Z(3)9.99  BLANK ZERO.
           03  D-360           PIC 9(6)      BLANK ZERO.
           03  D-370           PIC Z(5)9.99   BLANK ZERO.

       01  DEF-BUF             REDEFINES DISPLAY-BUF1.
           03  DEF-ORGST       PIC XX.
           03  DEF-ORGBR       PIC 9(4).
           03  DEF-LOANDATE    PIC 99/99/99.
           03  DEF-PAYFREQ     PIC Z9.

       01  DEF-LIST            PIC X(16) VALUE
           "050 051 060 140.".

       01  DISPLAY-LIST1.
           03  FILLER          PIC X(53)  VALUE
           "LNKEY LCDESC 010 011 012 020 021 022 030 031 032 040 ".
           03  FILLER          PIC X(52)  VALUE
           "041 042 050 051 052 053 060 070 071 080 081 090 091 ".
           03  FILLER          PIC X(12)  VALUE
           "100 110 111.".
       01  DISPLAY-LIST2.
           03  FILLER          PIC X(4)   VALUE
           "380 ".
           03  FILLER          PIC X(52)  VALUE
           "120 121 130 140 150 160 170 180 190 200 210 220 230 ".
           03  FILLER          PIC X(52)  VALUE
           "240 241 250 260 270 271 272 273 274 280 281 282 283 ".
           03  FILLER          PIC X(52)  VALUE
           "284 290 291 292 293 294 300 301 302 303 304 310 311 ".
           03  FILLER          PIC X(52)  VALUE
           "312 313 314 320 321 322 323 324 330 331 332 333 340 ".
           03  FILLER          PIC X(36)  VALUE
           "341 342 343 350 351 352 353 360 370.".

      ********************************
      *   E L E M E N T   S P E C S  *
      ********************************
       01  MAX             PIC 99 COMP  VALUE 38.
       01  SPEC-TABLE.
      * 1  PD-SSNO(1)
           03  FILLER      PIC X(8) VALUE "ERNX110 ".
      * 2  PD-SSNO(2)
           03  FILLER      PIC X(8) VALUE "ERNX110 ".
      * 3  PD-SSNO(3)
           03  FILLER      PIC X(8) VALUE "ERNX110 ".
      * 4  PD-SSNO(4)
           03  FILLER      PIC X(8) VALUE "ERNX110 ".
      * 5  PD-ORGST
           03  FILLER      PIC X(8) VALUE "E  A020 ".
      * 6  PD-LOANDATE
           03  FILLER      PIC X(8) VALUE "ERNM060 ".
      * 7  PD-PURCD
           03  FILLER      PIC X(8) VALUE "E  A030 ".
      * 8  PD-SRCD
           03  FILLER      PIC X(8) VALUE "E  A030 ".
      * 9  PD-COLLCD
           03  FILLER      PIC X(8) VALUE "E  A030 ".
      * 10 PD-COLLID
           03  FILLER      PIC X(8) VALUE "E  A200 ".
      * 11 LT-COLLATERAL-INFO
           03  FILLER      PIC X(8) VALUE "E  A300 ".
      * 12 PD-DLNO
           03  FILLER      PIC X(8) VALUE "ENNN042 ".
      * 13 PD-ORGTERM
           03  FILLER      PIC X(8) VALUE "ENNN030 ".
      * 14 PD-PAYFREQ---JUNK
           03  FILLER      PIC X(8) VALUE "ENNN020N".
      * 15 PD-FILEDATE
           03  FILLER      PIC X(8) VALUE "ERNM060 ".
      * 16 PD-INTDATE
           03  FILLER      PIC X(8) VALUE "ERNM060 ".
      * 17 PD-1STPYDATE
           03  FILLER      PIC X(8) VALUE "ERNM060 ".
      * 18 PD-APRATE
           03  FILLER      PIC X(8) VALUE "ENNN033 ".
      * 19 PD-OVRDRATE
           03  FILLER      PIC X(8) VALUE "ENNN033 ".
      * 20 PD-ENDORATE
           03  FILLER      PIC X(8) VALUE "ENNN033 ".
      * 21 PD-REGPYAMT
           03  FILLER      PIC X(8) VALUE "ENNN062 ".
      * 22 PD-1STPYAMT
           03  FILLER      PIC X(8) VALUE "ENNN062 ".
      * 23 PD-EXTCHG
           03  FILLER      PIC X(8) VALUE "ENNN042 ".
      * 24 PD-SERCHG
           03  FILLER      PIC X(8) VALUE "ENNN062 ".
      * 25 PD-INTCHG
           03  FILLER      PIC X(8) VALUE "ENNN062 ".
      * 26 PD-INSURED
           03  FILLER      PIC X(8) VALUE "E  A030 ".
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
      * 33 PD-FEECODE(1), (2)
           03  FILLER      PIC X(8) VALUE "E  A020 ".
      * 34 PD-FEECODE(3), (4)
           03  FILLER      PIC X(8) VALUE "E  A020 ".
      * 35 PD-FEECODE(5), (6)
           03  FILLER      PIC X(8) VALUE "E  A020 ".
      * 36 PD-PRLNNO
           03  FILLER      PIC X(8) VALUE "ENNN060 ".
      * 37 PD-ANTICERN(1)
           03  FILLER      PIC X(8) VALUE "ENNN062 ".
      * 38 PD-LC-CODE
           03  FILLER      PIC X(8) VALUE "ENNN020".
       01  SPEC-TABLE2     REDEFINES SPEC-TABLE.
           03  SPEC-REC    OCCURS 38 TIMES.
               05  SPEC    PIC X(7).
               05  CHFG    PIC X.

       01  PRIOR-LNAMT          PIC S9(7)V99 COMP.
       01  STOP-UPDATE-FLAG     PIC X VALUE " ".
       01  LNCOST-WORKER.
           03  LNCOST-COST         PIC S9(6)V99.


       COPY "LIBLP/LPWKPATH.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/SYSTEMW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBGB/GPENVW.CPY".
       COPY "LIBLP/LPANTIW.CPY".
       COPY "LIBLP/LPAPRZLW.CPY".
       COPY "LIBLP/LPMENUW.CPY".
       COPY "LIBLP/LPAMTSW.CPY".
       COPY "LIBCL/BISCANW.CPY".
       COPY "LIBCL/BICLASW.CPY".
       COPY "LIBLP/LPEDAYW.CPY".
       COPY "LIBWI/ELSCANW.CPY".
       COPY "LIBWI/ICSCANW.CPY".
       COPY "LIBWI/SCSCANW.CPY".
       COPY "LIBWI/BWCREAW.CPY".
       01  WINDOW-WORKERS.
           03  WINDOW-LPO          PIC X(4).
           03  WINDOW-REP          PIC X(8).
           03  WINDOW-PROCESS      PIC X(1).

       COPY "LIBWI/PFSWINDOWSW.CPY".
       COPY "LIBGB/GRWORKW.CPY".
       COPY "LIBWI/LNAPPROVALW.CPY".
       COPY "LIBGB/PASSWDW.CPY".
       01  1STPY30-SEQ            PIC 9  VALUE 0.
       01  APPROVAL-SEQ           PIC 99 VALUE 02.
       01  APPROVAL-SEQ2          PIC 99 VALUE 05.
       01  SRCD-SEQ               PIC 99 VALUE 20.
       COPY "LIBWI/AUTOPCW.CPY".
       COPY "LIBWI/LTCREAW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBLP/LONPC_DEF.CPY".
       COPY "LIBLP/LPSMNU_DEF.CPY".
       COPY "LIBLP/LPLNMU_DEF.CPY".
       COPY "LIBLP/LPCAMU_DEF.CPY".
       COPY "LIBLP/LPVDMU_DEF.CPY".
       COPY "LIBWI/GLIMI2_DEF.CPY".
       COPY "LIBWI/GLIMI3_DEF.CPY".
       COPY "LIBLP/LONPC_WKS.CPY".
       COPY "LIBLP/LPSMNU_WKS.CPY".
       COPY "LIBLP/LPLNMU_WKS.CPY".
       COPY "LIBLP/LPCAMU_WKS.CPY".
       COPY "LIBLP/LPVDMU_WKS.CPY".
       COPY "LIBWI/GLIMI2_WKS.CPY".
       COPY "LIBWI/GLIMI3_WKS.CPY".

       COPY "LIBGB/GETFMW.CPY".
       COPY "LIBLP/LPLOANW.CPY".
       COPY "LIBLP/LPPFBUF.CPY".

       SCREEN SECTION.
       COPY "LIBLP/LONPC_SCN.CPY".
       COPY "LIBLP/LPSMNU_SCN.CPY".
       COPY "LIBLP/LPLNMU_SCN.CPY".
       COPY "LIBLP/LPCAMU_SCN.CPY".
       COPY "LIBLP/LPVDMU_SCN.CPY".
       COPY "LIBWI/GLIMI2_SCN.CPY".
       COPY "LIBWI/GLIMI3_SCN.CPY".


       PROCEDURE DIVISION
           USING FORM-PATHNAME EXIT-PATHNAME PROG-BUF UPDATE-BUF.
       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
               WK-FILE.
       COPY "LIBGB/DECLARE.CPY".
           PERFORM CLOSE-SP1-FILE.
           PERFORM CLOSE-PD1-FILE.
           PERFORM CLOSE-PC1-FILE.
           PERFORM CLOSE-LC1-FILE.
           PERFORM CLOSE-IC1-FILE.
           PERFORM CLOSE-DL1-FILE.
           PERFORM CLOSE-CDB1-FILE.
           PERFORM CLOSE-CD1-FILE.
           PERFORM CLOSE-BW2-FILE.
           PERFORM CLOSE-BW1-FILE.
           PERFORM CLOSE-GI1-FILE.
           PERFORM CLOSE-BR-FILE.
           PERFORM CLOSE-GR1-FILE.
           CLOSE WK-FILE.
       COPY "LIBGB/DECLAR2.CPY".

      ***********************************
      *   C O N T R O L   M O D U L E   *
      ***********************************
       MAIN-PROGRAM SECTION.

           PERFORM SQL-CONNECT.

      D    STOP MESS.
           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "LONPC" TO VDU-FORM-NAME.

           MOVE USER-PID TO TEMP-PID.
           MOVE " " TO ERRCD.

           MOVE BRANCH TO BR-NO.
           PERFORM GET-BR.
           IF IO-FG = 9
              GO TO IO-ERROR.

           PERFORM GET-GPENV.

           PERFORM RESTORE-SCREEN.
           PERFORM CREATE-TRAILER.

           IF ROUTE-CD = "1"
              MOVE " " TO ROUTE-CD
              MOVE 2 TO NEW-FLAG
              PERFORM TEST-GET-SP
              GO TO ENTER-LOAN.

           IF NEW-FLAG = 1
              PERFORM DISPLAY-FIELDS
              PERFORM TEST-GET-SP
              GO TO ENTER-LOAN.
       MAIN-MODULE.

       COPY "LIBGB/CHKSEC.CPY".
           PERFORM FORM-RESET.

           MOVE " " TO MESS.
           PERFORM MSG-LINE.

           IF BR-LC-CODE = "Y"
              MOVE " " TO CHFG(38)
              MOVE "38. LC CODE:__" TO LC-INFO-BUF
           ELSE
              MOVE "N" TO CHFG(38)
              MOVE " " TO LC-INFO-BUF.
           MOVE LC-INFO-LIST TO WR-LIST.
           MOVE LC-INFO-BUF  TO WR-BUF.
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

      ********************************************
       ENTER-KEYS SECTION.
      ********************************************
       ENTER-KEYS-NO.
           MOVE 0 TO NEW-FLAG ELE.
           MOVE SPACES TO IO-TYPE.
           PERFORM NEW-RECORD-SETUP.
           MOVE "F1-RECALL #, F5-APPS, F7-MENU" TO MESS.
           PERFORM SEND-LEGEND.
           PERFORM LOAN-CLASS-PROMPT.

       ENTER-LC-CODE.

           MOVE "EN N090LNKEY." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10"
              GO TO RECALL-LOAN.

           IF VDUSTATUS = "1>"
              GO TO MENU-KEY.
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

           MOVE BRANCH TO GI-BRANCH.
           MOVE LC-CODE TO GI-CLASS.
           PERFORM GET-GI.
           IF IO-FG = 9
              MOVE "G/L INTERFACE RECORD NOT FOUND" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-LC-CODE.

           IF LC-SALEFIN = "Y"
              PERFORM NOT-DIRECT
              GO TO ENTER-LC-CODE.

           MOVE LC-CODE     TO PD-CLASS
                               PD-SPRCLASS.
           MOVE LC-SMALLOAN TO PD-SMALLOAN.
           MOVE LC-SALEFIN  TO PD-SALEFIN.
           MOVE LC-SERVACCT TO PD-SERVACCT.
           MOVE LC-LOANTYPE TO PD-LOANTYPE.
           MOVE LC-GIFTNET  TO PD-GIFTNET.
           MOVE 1 TO PD-STMT-COUNTER.
           GO TO EXIT-ENTER-KEYS.

      **********************
      *   VIA RECALL #     *
      **********************
       RECALL-LOAN.
           PERFORM NEW-RECORD-SETUP.
       RECALL-GET-KEY.
           MOVE "F1-LOAN #, F7-MENU" TO MESS.
           PERFORM SEND-LEGEND.

           MOVE "S  A110KEYTYPE." TO VDU.
           MOVE "RECALL SS#:" TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "EN N090LNKEY." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10"
              GO TO ENTER-KEYS-NO.
           IF VDUSTATUS = "1>"
              GO TO MENU-KEY.
           IF VDUSTATUS NOT = "00"
              GO TO RECALL-LOAN.
           IF VDUNUM0 = 0
              GO TO RECALL-LOAN.

      * LOOK FOR SCRATCH SAVED F3 COLLATERAL RECORD
           MOVE "C" TO PC-ID.
           MOVE BRANCH TO PC-BRANCH.
           MOVE VDUNUM0 TO PC-SOC-SEC.
           PERFORM OPEN-PC1-FILE.
           PERFORM READ-PC1-FILE.
           PERFORM CLOSE-PC1-FILE.
           IF IO-FG = 0
              MOVE "Y" TO F3-COLLATERAL
              PERFORM RECALL-COLLATERAL.

           MOVE "L" TO PD-ID.
           MOVE BRANCH TO PD-BRANCH.
           MOVE VDUNUM0 TO PD-SOC-SEC.
           PERFORM GET-PD1.
           IF IO-FG = 9
              PERFORM NOT-FOUND
              GO TO RECALL-LOAN.

           PERFORM DISPLAY-FIELDS.

           PERFORM LOAN-CLASS-PROMPT.
           IF LC-SALEFIN = "Y"
              PERFORM NOT-DIRECT
              MOVE "CLR" TO IO-TYPE
              GO TO EXIT-ENTER-KEYS.

           MOVE 2 TO NEW-FLAG.

           MOVE "SN N090LNKEY." TO VDU.
           MOVE PD-CLASS TO VDUNUM0.
           PERFORM SCREENIO.

           PERFORM GET-SP.

           GO TO EXIT-ENTER-KEYS.

      **********************
      *   END KEY ENTRY    *
      **********************
       MENU-KEY.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY LONPC-SCREEN.

           PERFORM MENU-SELECTION.
           MOVE 0 TO ELE.
           PERFORM SAVE-SCREEN.
           IF MENU-FG = 99
              GO TO ENTER-KEYS.
           MOVE "END" TO IO-TYPE.
       EXIT-ENTER-KEYS.
           EXIT.

      ********************************************
       BORROWER-CREATE SECTION.
      ********************************************
           IF VDUBUF = " "
                    AND PD-SSNO(ELE) NOT = 0
              MOVE PD-SSNO(ELE) TO BWCREA-NO
           ELSE
              MOVE VDUNUM0 TO BWCREA-NO.

           MOVE PD-SSNO(1) TO BWCREA-SSNO(1).
           MOVE PD-SSNO(2) TO BWCREA-SSNO(2).
           MOVE PD-SSNO(3) TO BWCREA-SSNO(3).
           MOVE PD-SSNO(4) TO BWCREA-SSNO(4).
           MOVE BWCREA-NO TO BWCREA-SSNO(ELE).

           MOVE "N"        TO BWCREA-ALLOW-CHG.
           MOVE TRANS-DATE TO BWCREA-TRANS-DATE.
           MOVE BRANCH     TO BWCREA-BRANCH.
           MOVE "WI/BWCREA" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH BWCREA-BUF EXIT-PATHNAME.
           CANCEL FORM-PROGX.
           IF BWCREA-STATUS NOT = "00" OR BWCREA-NO = 0
              MOVE 9 TO IO-FG
              GO TO EXIT-BORROWER-CREATE.

           MOVE BWCREA-NO TO BW-SSNO.
           PERFORM NUMBW-TO-ALPBW.
           MOVE "S  A110  0." TO VDU.
           MOVE ELE TO VDUROW.
           MOVE ALP-BW TO VDUBUF.
           PERFORM SCREENIO.
           MOVE BWCREA-NO TO VDUNUM0
           MOVE 0 TO IO-FG.

       EXIT-BORROWER-CREATE.
           EXIT.

      *    ************************************************
      *    *   ENTRY-MODULE
      *    *   NEW-FLAG = 0 - MODIFY EXISTING RECORD
      *    *              1 - INITIAL NEW RECORD ENTRY
      *    *              2 - MODIFY NEW RECORD
      *    ************************************************
       ENTRY-MODULE SECTION.
       BEGIN-ENTRY.
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
           PERFORM COMPUTE-PRINCIPLE.
           PERFORM COMPUTE-INS-COVERAGE.
       MODIFY-ROUTINE.
           MOVE " " TO STAT.
           PERFORM PROTECT-UNPROTECT.
           PERFORM COMPUTE-PRINCIPLE.
           IF ELE = 13 OR 17 OR 21 OR 22 OR 23 OR 24 OR 25 OR
              PREM-CHANGE = "Y"
              PERFORM COMPUTE-INS-COVERAGE.
           IF ELE = 13 OR 17
              PERFORM COMPUTE-INS-TERM.
           PERFORM COMPUTE-PROCEEDS.
           PERFORM COMPUTE-ANTICIPATED.
           PERFORM COMPUTE-FINANCE-CHARGE.
           PERFORM GET-EARN-ORDER.
           MOVE "F4-MEMO,FIELD #,DELETE,S-SAVE,U-UPDATE:" TO MESS.
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
              PERFORM WRITE-SCRATCH
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.
           IF ACTION = "DELETE"
              PERFORM DELETE-SCRATCH
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.

           IF NOT ( ACTION = "     U" )
              GO TO BYPASS-UPDATE.

      * ADDED STOP-UPDATE-FLAG TO TEST IF PRIOR-LNAMT > PD-PROCEEDS
           IF STOP-UPDATE-FLAG = "Y"
              MOVE "INVALID PROCEEDS" TO MESS
              PERFORM SEND-MESS
              GO TO MODIFY-ROUTINE.

           MOVE LNAPPROVAL---CREATION-LONP TO LNAPPROVAL-CREATION.

           PERFORM VERIFY-LOAN-APPROVAL.
           IF ( LNAPPROVAL-STATUS-UNAPPROVED )
              GO TO MODIFY-ROUTINE.

           MOVE 0 TO SUB BISCAN-CALL-CODE.
           PERFORM DUAL-CHECK 4 TIMES.
           IF IO-TYPE = "RTN"
              MOVE SPACES TO IO-TYPE
              GO TO MODIFY-ROUTINE
           ELSE
           PERFORM TEST-ALLOT-LBOX
           IF IO-TYPE = "RTN"
              MOVE SPACES TO IO-TYPE
              GO TO MODIFY-ROUTINE
           ELSE
              PERFORM ENTER-HI-CREDIT
              IF IO-TYPE = "RTN"
                 MOVE SPACES TO IO-TYPE
                 GO TO MODIFY-ROUTINE
              ELSE
                 PERFORM SAVE-SCREEN
                 MOVE " " TO ERRORS-EXIST
                 MOVE "LONPC0" TO LPLOAN-CALLED-FROM
      D          STOP MESS
                 MOVE "CL/LNVERI" TO FORM-NAM
                 CALL FORM-PROGX USING FORM-PATH ERRORS-EXIST PROG-BUF
                 CANCEL FORM-PROGX
                 IF ERRORS-EXIST = "Y"
                    GO TO ENTRY-MODULE
                 END-IF
                 MOVE MENU-PROG-FORM(3) TO NEW-PROG-FORM
                 MOVE 2 TO NEW-PROG-SUB
                 MOVE "P" TO ERRCD
                 MOVE "END" TO IO-TYPE
                 GO TO END-ENTRY-MODULE.

      *-----------------------------------------------------------------
       BYPASS-UPDATE.

           INSPECT ACTION REPLACING ALL " " BY "0".
           IF CHNO NOT NUMERIC
              GO TO MODIFY-ROUTINE.
           IF CHNO = 0
              GO TO MODIFY-ROUTINE.
           IF CHNO > MAX
              GO TO MODIFY-ROUTINE.
           MOVE CHNO TO ELE.

      *    APR IS DEFAULTED INITIALLY IN COMPUTE-PROCEEDS.
      *    IF ANY FIELD OTHER THAN PD-APRATE IS MODIFIED
      *    APR IS RECALCULATED, THUS ALLOWING FOR MANUAL APR
      *    OVERRIDE IN CASE OF ERROR.
           IF ELE NOT = 18
              MOVE "Y" TO DEFAULT-APR-FG.

      *-----------------------------------------------------------------
       ENTER-ELE.
           MOVE "F5-CREATE, F6-SCAN, ENTER BORROWER SSNO#:" TO MESS.

           IF ( ELE < 5 )
              PERFORM SEND-LEGEND.

           IF ELE = 5
              IF NEW-FLAG = 1
                    AND CHFG(5) = "N"
                 GO TO F-052
              ELSE
                 PERFORM RESET-LEGEND.

           IF ELE = 10
              MOVE PD-COLLCD TO TEST-COLLCD
              IF (BR-TRAILER-REC = "Y" AND VDUSTATUS = "16")
               OR (BR-TRAILER-REC = "Y" AND AUTO-COLLCD)
              MOVE "Y" TO F3-COLLATERAL
              MOVE "N" TO CHFG(11)
              MOVE WK-HHGS-1 TO INSPECT-X
              PERFORM COLLATERAL-CREATE
              PERFORM RESTORE-TRAILER-REC.

           IF ELE = 11
              IF NEW-FLAG = 1
                 IF PD-COLLID = SPACES
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
                 MOVE "F1-CLEAR, F3-LPO, ENTER INSURANCE CODES"
                                                            TO MESS
                 PERFORM SEND-LEGEND
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

           IF ELE = 27
              MOVE VDUSTATUS TO HOLD-VDUSTATUS
              MOVE "F6-SCAN, ENTER INSURANCE COMPANY" TO MESS
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
              TO MESS
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
              TO MESS
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
              TO MESS
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
              TO MESS
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
              TO MESS
              PERFORM SEND-LEGEND
              IF HOLD-VDUSTATUS = "1<"
                 PERFORM INSCOMP-SCAN
                 MOVE "SNNN020320." TO VDU
                 MOVE ICSCAN-CODE TO VDUNUM0
                 PERFORM SCREENIO
                 MOVE ICSCAN-CODE TO VDUNUM0
                 IF IO-FG = 9
                    GO TO ENTER-ELE.

           IF ELE > 32 AND ELE < 36
              MOVE "ENTER = NONE" TO MESS
              PERFORM SEND-LEGEND.
           IF ELE = 36
              PERFORM RESET-LEGEND.

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
       TEST-F2.
      *    IF VDUSTATUS NOT = "12"
      *       GO TO TEST-UP.
      *    PERFORM MENU-SELECTION.
      *    PERFORM SAVE-SCREEN.
      *    IF MENU-FG = 99
      *       GO TO ENTER-ELE.
      *    MOVE "END" TO IO-TYPE.
      *    GO TO END-ROUTINE.
       TEST-UP.
           IF VDUSTATUS NOT = "1U"
              GO TO TEST-DOWN.
           IF ELE = 6
              MOVE " " TO CHFG(5).
           IF ELE = 13
              MOVE " " TO CHFG(12).
           IF ELE = 16
              MOVE " " TO CHFG(15).
           IF ELE = 17
              MOVE " " TO CHFG(16).
           IF ELE = 20
              MOVE " " TO CHFG(19).
           IF ELE = 21
              MOVE " " TO CHFG(20).
           IF NEW-FLAG = 1
              MOVE -1 TO ONE
              GO TO CREATE-ROUTINE.
       TEST-DOWN.
           IF VDUSTATUS NOT = "1D"
              GO TO TEST-F4.
           MOVE 1 TO ONE.
           GO TO GOTO-FIELD.

      *-----------------------------------------------------------------
       TEST-F4.

           IF ( NOT F4-KEY ) GO TO TEST-F5.

           IF ( ELE = 10 )
              PERFORM AUTOPC-ROUTINE.

      *-----------------------------------------------------------------
       TEST-F5.

           IF ( NOT F5-KEY ) GO TO TEST-F6.

           IF ( ELE < 5 )
              PERFORM BORROWER-CREATE
              IF IO-FG = 0
                 GO TO GOTO-FIELD.

      *-----------------------------------------------------------------
       TEST-F6.
           IF ( NOT F6-KEY ) GO TO TEST-00.

           IF ELE < 5
              PERFORM BORROWER-SCAN
              IF IO-FG = 0
                 GO TO GOTO-FIELD
              ELSE
                 GO TO ENTER-ELE.
           IF ELE = 38
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
                 F-31 F-32 F-33 F-34 F-35 F-36 F-37 F-38
                 DEPENDING ON OVER-20.
           GO TO END-ENTRY-MODULE.

      *************************************
      *    S E T   E L E M E N T S        *
      *************************************
       F-01.
           PERFORM BORROWER-ENTRY.

           IF ( BW-ERROR-YES )
              GO TO ENTER-ELE.

           IF ( ( BW-LIASMLN     = ZEROES     ) AND
                ( BW-PRLOANS     = ZEROES     ) AND
                ( BW-REFDATE NOT = TRANS-DATE ) ) OR
              ( ( BW-LIASMLN     = ZEROES     ) AND
                ( BW-PRLOANS NOT = ZEROES     ) )
              MOVE BW-SSNO TO BISCAN-SSNO
              MOVE 1       TO BISCAN-CALL-CODE
              PERFORM CALL-SCAN.

           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-02.
           PERFORM BORROWER-ENTRY.

           IF ( BW-ERROR-YES )
              GO TO ENTER-ELE.

           IF ( BW-SSNO NOT = ZEROES )
              MOVE BW-SSNO TO BISCAN-SSNO
              IF ( ( BW-LIASMLN     = ZEROES     ) AND
                   ( BW-PRLOANS     = ZEROES     ) AND
                   ( BW-REFDATE NOT = TRANS-DATE ) ) OR
                 ( ( BW-LIASMLN     = ZEROES     ) AND
                   ( BW-PRLOANS NOT = ZEROES     ) )
                 MOVE BW-SSNO TO BISCAN-SSNO
                 MOVE 1       TO BISCAN-CALL-CODE
                 PERFORM CALL-SCAN.

           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-03.
           PERFORM BORROWER-ENTRY.

           IF ( BW-ERROR-YES )
              GO TO ENTER-ELE.

           IF ( BW-SSNO NOT = ZEROES )
              MOVE BW-SSNO TO BISCAN-SSNO
              IF ( ( BW-LIASMLN     = ZEROES     ) AND
                   ( BW-PRLOANS     = ZEROES     ) AND
                   ( BW-REFDATE NOT = TRANS-DATE ) ) OR
                 ( ( BW-LIASMLN     = ZEROES     ) AND
                   ( BW-PRLOANS NOT = ZEROES     ) )
                 MOVE BW-SSNO TO BISCAN-SSNO
                 MOVE 1       TO BISCAN-CALL-CODE
                 PERFORM CALL-SCAN.

           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-04.
           PERFORM BORROWER-ENTRY.

           IF ( BW-ERROR-YES )
              GO TO ENTER-ELE.

           IF ( BW-SSNO NOT = ZEROES )
              MOVE BW-SSNO TO BISCAN-SSNO
              IF ( ( BW-LIASMLN     = ZEROES     ) AND
                   ( BW-PRLOANS     = ZEROES     ) AND
                   ( BW-REFDATE NOT = TRANS-DATE ) ) OR
                 ( ( BW-LIASMLN     = ZEROES     ) AND
                   ( BW-PRLOANS NOT = ZEROES     ) )
                 MOVE BW-SSNO TO BISCAN-SSNO
                 MOVE 1       TO BISCAN-CALL-CODE
                 PERFORM CALL-SCAN.

           PERFORM BW-CHECK-ENDORSER.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-05.
           MOVE VDUBUF TO PD-ORGST
                          PD-OWNST.
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

           MOVE PD-ORGST    TO SP-ORGST.
           MOVE PD-SPRCLASS TO SP-SPRCLASS.
           MOVE VDUNUM0     TO PD-SUBCLASS SP-SUBCLASS.
           MOVE 0           TO SP-LAWCODE.

           PERFORM OPEN-SP1-FILE.
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

           MOVE SP-LAWCODE TO PD-LAWCODE.
           IF PD-INTDATE = 0
              MOVE "S  8000160." TO VDU
              MOVE PD-LOANDATE TO VDU-DATE-YYYYMMDD PD-INTDATE
              PERFORM SCREENIO.
           IF PD-1STPYDATE NOT = 0
              GO TO CREATE-ROUTINE.

           MOVE PD-INTDATE TO NDTE-DATE.
           MOVE 1          TO NDTE-HOLD.
           MOVE PD-UNITPER-CD TO DATER-UNITPER-CD.
           MOVE PD-UNITPER-FREQ TO DATER-UNITPER-FREQ.
           PERFORM INCREMENT-UNITPER.
           MOVE "S  8000170." TO VDU.
           MOVE NDTE-DATE TO VDU-DATE-YYYYMMDD PD-1STPYDATE
                                           PD-ORIG-1STPYDATE.
           PERFORM SCREENIO.

           GO TO CREATE-ROUTINE.
       F-07.
           MOVE "PU"   TO CD-TYPE.
           MOVE VDUBUF TO CD-CODE PD-PURCD.
           PERFORM SEND-CD-DESC.
           IF IO-FG = 9
              GO TO ENTER-ELE.

           IF SP-LNCOST-FRMLA = "M"
              MOVE PD-ANTICERN(6) TO LNCOST-COST
              MOVE "WI/LNCOST" TO FORM-NAM
              CALL FORM-PROGX USING FORM-PATH LNCOST-COST
              CANCEL FORM-PROGX
              MOVE LNCOST-COST TO PD-ANTICERN(6).

           GO TO CREATE-ROUTINE.

       F-08.
           MOVE "SR"   TO CD-TYPE.
           MOVE VDUBUF TO CD-CODE PD-SRCD.
           PERFORM SEND-CD-DESC.
           IF IO-FG = 9
              GO TO ENTER-ELE.

           IF PD-SRCD = "LR"
              MOVE "'LR' SOURCE CODE NOT ALLOWED" TO MESS
              PERFORM SEND-MESS
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

           GO TO CREATE-ROUTINE.
       F-09.
           MOVE "CO"   TO CD-TYPE.
           MOVE VDUBUF TO CD-CODE PD-COLLCD.
           PERFORM SEND-CD-DESC.
           IF IO-FG = 9
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       F-10.
           MOVE VDUBUF TO PD-COLLID.
           GO TO CREATE-ROUTINE.
       F-11.
           MOVE VDUBUF TO WK-HHGS-1 PD-MSG1.
           IF PD-MSG1 = " "
              MOVE SPACES TO PD-MSG2 VDUBUF
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
           MOVE VDUBUF TO PD-MSG2.
           STRING PD-MSG1,PD-MSG2 DELIMITED BY "   " INTO
              WK-HHGS-1.
           PERFORM SAVE-TRAILER-REC.
           GO TO CREATE-ROUTINE.
       F-12.
           MOVE VDUNUM2 TO PD-DLNO DL-DLNO.
           PERFORM GET-DL1.
           MOVE "S  A160121." TO VDU.
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
           PERFORM MSG-LINE.
           IF BR-LC-CODE = "Y"
              MOVE "SNNN020380." TO VDU
              MOVE DL-LC-CODE TO VDUNUM0 PD-LC-CODE
              PERFORM SCREENIO.
           GO TO CREATE-ROUTINE.
       F-13.
           IF VDUNUM0 = 0
              GO TO ENTER-ELE.
           MOVE VDUNUM0 TO PD-ORGTERM PD-ANTICTERM PD-SUM-ORGTERM.
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
           MOVE VDU-DATE-YYYYMMDD TO PD-FILEDATE.
           GO TO CREATE-ROUTINE.
       F-16.
           IF VDUNUM0 = 0
              GO TO ENTER-ELE.
           MOVE VDU-DATE-YYYYMMDD TO PD-INTDATE.

           IF PD-INTDATE < PD-LOANDATE
              MOVE "MUST NOT BE PRIOR TO  LOAN DATE" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       F-17.
           IF VDUNUM0 = 0
              GO TO ENTER-ELE.
           MOVE VDU-DATE-YYYYMMDD TO PD-ORIG-1STPYDATE
                                     PD-1STPYDATE.
           IF PD-1STPYDATE NOT > PD-INTDATE
              MOVE "FIRST PAY MUST BE AFTER THE INTEREST DATE" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.

      * DO NOT ALLOW A FIRST PAYMENT DATE < ORIGINALLY CALCD DATE !
           MOVE PD-INTDATE TO NDTE-DATE.
           MOVE 1 TO NDTE-HOLD.
           MOVE PD-UNITPER-CD TO DATER-UNITPER-CD.
           MOVE PD-UNITPER-FREQ TO DATER-UNITPER-FREQ.
           PERFORM INCREMENT-UNITPER.
           IF PD-1STPYDATE < NDTE-DATE
              MOVE 1STPY30-SEQ TO PASSWDW-SEQ
              MOVE "1ST PAY < 30 DAYS FROM LNDATE" TO PASSWDW-PROMPT2
              PERFORM VERIFY-PASSWORD
              IF PASSWDW-STAT NOT = "OK"
                 GO TO ENTER-ELE.
      *
           GO TO CREATE-ROUTINE.
       F-18.
           IF VDUNUM4 = 0
              GO TO ENTER-ELE.
           MOVE VDUNUM3 TO PD-APRATE.
           GO TO CREATE-ROUTINE.
       F-19.
           MOVE VDUNUM3 TO PD-OVRDRATE.
           GO TO CREATE-ROUTINE.
       F-20.
           MOVE VDUNUM3 TO PD-ENDORATE.
           GO TO CREATE-ROUTINE.
       F-21.
           MOVE VDUNUM2 TO PD-REGPYAMT PD-SUM-REGPYAMT.
           IF VDUNUM2 = 0
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       F-22.
           MOVE VDUNUM2 TO PD-1STPYAMT.
           IF PD-1STPYAMT = 0
              MOVE 0 TO PD-NOODDPY
           ELSE
              MOVE 1 TO PD-NOODDPY.
           GO TO CREATE-ROUTINE.
       F-23.
           MOVE VDUNUM2 TO PD-EXTCHG PD-SUM-EXTCHG.
           GO TO CREATE-ROUTINE.
       F-24.
           MOVE VDUNUM2 TO PD-SERCHG PD-SUM-SERCHG.
       F-241.
           IF SP-MFFRMLA = 0
              GO TO CREATE-ROUTINE.
           MOVE "ENNN032241." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE.
           IF VDUSTATUS NOT = "00" AND NOT = "1D"
              GO TO F-241.
           DIVIDE VDUNUM2 BY PD-ORGTERM GIVING PD-MAINTFEE
             REMAINDER AMT-HOLDER.
           IF AMT-HOLDER NOT = 0
              MOVE "AMOUNT MUST BE EVENLY DIVISABLE BY TERM" TO MESS
              PERFORM SEND-MESS
              GO TO F-241.
           COMPUTE PD-SUM-MAINTFEE = PD-MAINTFEE * PD-ORGTERM.
           GO TO CREATE-ROUTINE.
       F-25.
           MOVE VDUNUM2 TO PD-INTCHG PD-SUM-INTCHG.
           GO TO CREATE-ROUTINE.
       F-26.
           MOVE " " TO CHFG(27) CHFG(28) CHFG(29)
                       CHFG(30) CHFG(31) CHFG(32).
           MOVE VDUBUF TO PD-INSCODES.
           IF PD-INSURED NOT = "M"
              IF PD-INSURED NOT = "J"
                 IF PD-INSURED NOT = "N"
                    IF PD-INSURED NOT = "S"
                       MOVE "ENTER 'N','M','S',OR 'J'" TO MESS
                       PERFORM SEND-MESS
                       GO TO ENTER-ELE.
           IF NOT PD-INSTYPES-VALID
              GO TO ENTER-ELE.
           IF NOT PD-INSELIGIBLE-VALID
              GO TO ENTER-ELE.

      * ADDED TO PREVENT ENTRY OF MNN, ETC.
           IF PD-INSTYPES = "N"
              IF PD-INSURED NOT = "N"
                 GO TO ENTER-ELE.

           IF PD-INSURED = "N"
              MOVE 0 TO IO-FG
              PERFORM VERIFY-INSURANCE VARYING SUB
                       FROM 1 BY 1 UNTIL SUB > 4
      *JTG             FROM 1 BY 1 UNTIL SUB > 3
              IF IO-FG = 9
                 MOVE "INS CO/PREMIUM MUST BE 0 FOR INC CD 'N'" TO MESS
                 PERFORM SEND-MESS
                 GO TO ENTER-ELE.

           PERFORM VALID-INS-TYPES-ELIG.
           IF IO-FG = 9
              GO TO ENTER-ELE.
           IF CHFG(27) = "N"
              MOVE 0 TO PD-INSCOMP(1) PD-INSTRM(1) PD-INSCOVR(1)
                        PD-INSPREM(1)
              MOVE SPACES TO PD-INSOURS(1)
              MOVE "270 271 272 273 274." TO MESS
              PERFORM RESET-MESS.
           IF CHFG(28) = "N"
              MOVE 0 TO PD-INSCOMP(2) PD-INSTRM(2) PD-INSCOVR(2)
                        PD-INSPREM(2)
              MOVE SPACES TO PD-INSOURS(2)
              MOVE "280 281 282 283 284." TO MESS
              PERFORM RESET-MESS.
           IF CHFG(29) = "N"
              MOVE 0 TO PD-INSCOMP(3) PD-INSTRM(3) PD-INSCOVR(3)
                        PD-INSPREM(3)
              MOVE SPACES TO PD-INSOURS(3)
              MOVE "290 291 292 293 294." TO MESS
              PERFORM RESET-MESS.
           IF CHFG(30) = "N"
              MOVE 0 TO PD-INSCOMP(4) PD-INSTRM(4) PD-INSCOVR(4)
                        PD-INSPREM(4)
              MOVE SPACES TO PD-INSOURS(4)
              MOVE "300 301 302 303 304." TO MESS
              PERFORM RESET-MESS.
           IF CHFG(31) = "N"
              MOVE 0 TO PD-INSCOMP(5) PD-INSTRM(5) PD-INSCOVR(5)
                        PD-INSPREM(5)
              MOVE SPACES TO PD-INSOURS(5)
              MOVE "310 311 312 313 314." TO MESS
              PERFORM RESET-MESS.
           IF CHFG(32) = "N"
              MOVE 0 TO PD-INSCOMP(6) PD-INSTRM(6) PD-INSCOVR(6)
                        PD-INSPREM(6)
              MOVE SPACES TO PD-INSOURS(5)
              MOVE "320 321 322 323 324." TO MESS
              PERFORM RESET-MESS.
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
           MOVE 1 TO I-LEVEL.
           PERFORM FEE-ENTRY.
           IF IO-FG = 9
              GO TO ENTER-ELE.
           IF PD-FEECODE(1) = SPACES
              GO TO CREATE-ROUTINE.
       F-332.
           MOVE "E  A020332." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE.
           IF VDUSTATUS NOT = "00"
              GO TO F-332.
           MOVE 2 TO I-LEVEL.
           PERFORM FEE-ENTRY.
           IF IO-FG = 9
              GO TO F-332.
           GO TO CREATE-ROUTINE.
       F-34.
           MOVE 3 TO I-LEVEL.
           PERFORM FEE-ENTRY.
           IF IO-FG = 9
              GO TO ENTER-ELE.
           IF PD-FEECODE(3) = SPACES
              GO TO CREATE-ROUTINE.
       F-342.
           MOVE "E  A020342." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE.
           IF VDUSTATUS NOT = "00"
              GO TO F-342.
           MOVE 4 TO I-LEVEL.
           PERFORM FEE-ENTRY.
           IF IO-FG = 9
              GO TO F-342.
           GO TO CREATE-ROUTINE.
       F-35.
           MOVE 5 TO I-LEVEL.
           PERFORM FEE-ENTRY.
           IF IO-FG = 9
              GO TO ENTER-ELE.
           IF PD-FEECODE(5) = SPACES
              GO TO CREATE-ROUTINE.
       F-352.
           MOVE "E  A020352." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE.
           IF VDUSTATUS NOT = "00"
              GO TO F-352.
           MOVE 6 TO I-LEVEL.
           PERFORM FEE-ENTRY.
           IF IO-FG = 9
              GO TO F-352.
           GO TO CREATE-ROUTINE.
       F-36.
           MOVE VDUNUM0 TO PD-PRLNNO.
           MOVE 0 TO PRIOR-LNAMT.
           IF NOT (PD-SRCD = "PB" OR "RN" OR "SC" OR "RB" OR "RO")
              IF VDUNUM0 = 0
                 MOVE 0 TO PD-PRLNNO   PD-PRBAL
                           PD-PRINTDUE PD-PRFINCHGRB
                           PD-PRINSRB  PD-PRPAID-AHEAD
                 GO TO CREATE-ROUTINE
              ELSE
                 GO TO CREATE-ROUTINE
           ELSE
              IF VDUNUM0 = 0
                 MOVE "PRIOR LOAN NO. REQUIRED FOR LOAN RENEWALS"
                                                           TO MESS
                 PERFORM SEND-MESS
                 GO TO ENTER-ELE.

           MOVE PD-REC TO WK-SAVE-RECORD.
           MOVE "R" TO PD-ID.
           MOVE BRANCH TO PD-BRANCH.
           MOVE PD-PRLNNO TO PD-SOC-SEC.
           PERFORM GET-PD1.
           IF IO-FG = 9
              MOVE "INITIATE RENEWAL IN TRANSACTION POSTING" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.

           COMPUTE PRIOR-LNAMT ROUNDED = (PD-PRBAL + PD-PRINTDUE)
                               - (PD-PRFINCHGRB + PD-PRINSRB).

      *    SAVE THESE FOR WORLD IN LNAPPROVAL, 8/24/16, PR# 1182
           MOVE PD-PRPDUE-DAYS TO SAVE-PRPDUE-DAYS.
           MOVE PD-PRLNAMT TO SAVE-PRLNAMT.

           MOVE WK-SAVE-RECORD TO PD-REC.

           GO TO CREATE-ROUTINE.
       F-37.
      *    MOVE VDUNUM2 TO PD-ANTICERN(1).
           GO TO CREATE-ROUTINE.
       F-38.
           MOVE "EL"    TO CD-TYPE.
           MOVE VDUNUM0 TO CD-EL-CODE PD-LC-CODE.
           MOVE SPACES  TO CD-EL-FILLER.
           PERFORM GET-CD.
           IF IO-FG NOT = 0 AND PD-LC-CODE NOT = 0
              MOVE "INVALID CODE" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       END-ENTRY-MODULE.
           EXIT.

      ******************************************************************
       BORROWER-ENTRY SECTION.
      ******************************************************************

           MOVE "N"        TO BW-ERROR-YN.
           MOVE VDUNUM0 TO NUM-BW.
           MOVE NUM-BW TO PD-SSNO(ELE) BW-SSNO.

           IF ( NUM-BW NOT = 0 )
              GO TO GET-BORROWER.

           IF ( ELE = 1 )
              MOVE "Y" TO BW-ERROR-YN
              GO TO EXIT-ENTER-BORROWER.

           MOVE 0 TO BW-SSNO.
           MOVE " " TO INSPECT-W.

      *-----------------------------------------------------------------
       NEXT-BW-CLEAR.
           MOVE 0   TO PD-SSNO(ELE).
           MOVE " " TO PD-MAKERCD(ELE).
           PERFORM SEND-BW-NO-NAME.
           PERFORM SEND-BW-MAKERCD.

           IF ( ELE < 4 )
              ADD 1 TO ELE
              GO TO NEXT-BW-CLEAR.

           PERFORM BW-CHECK-ENDORSER.
           MOVE "N" TO BW-ERROR-YN.
           GO TO EXIT-ENTER-BORROWER.

      *-----------------------------------------------------------------
       GET-BORROWER.

           PERFORM BW-TABLE-CHECK.

           IF ( BW-ERROR-YES )
              GO TO EXIT-ENTER-BORROWER.

           PERFORM GET-BW1.
           PERFORM SEND-BW-NAME.

           IF ( BW-ERROR-YES )
              GO TO EXIT-ENTER-BORROWER
           ELSE
           IF GP-VALID-BDATE = "Y"
              MOVE BW-BDATE TO NUM-DATE
              PERFORM TSTDAT
              IF NUM-DATE = 0
                 MOVE "BORROWER HAS AN INVALID BIRTHDATE" TO MESS
                 PERFORM SEND-MESS
                 MOVE "Y" TO BW-ERROR-YN
                 GO TO EXIT-ENTER-BORROWER.

           IF ( ELE = 1 )
              MOVE "M" TO PD-MAKERCD(1)
              PERFORM SEND-BW-MAKERCD
              MOVE BW-CONFIDENTIAL TO PD-CONFIDENTCD
              MOVE BW-ORIGIN TO PD-ORIGIN
              GO TO EXIT-ENTER-BORROWER.

      *-----------------------------------------------------------------
       ENTER-MAKER-CODE.

           MOVE "E  A010  2." TO VDU.
           MOVE ELE TO VDUROW.
           PERFORM SCREENIO.

           IF VDUSTATUS = "1U"
              MOVE "Y" TO BW-ERROR-YN
              GO TO EXIT-ENTER-BORROWER.

           IF VDUSTATUS NOT = "00"
              GO TO ENTER-MAKER-CODE.

           IF VDUBUF NOT = "J"
              IF VDUBUF NOT = "E"
                 MOVE "(M-MAKER),J-JOINT,E-ENDORSER" TO MESS
                 PERFORM SEND-MESS
                 GO TO ENTER-MAKER-CODE.

           MOVE VDUBUF TO PD-MAKERCD(ELE).

       EXIT-ENTER-BORROWER.
           EXIT.

      ******************************************************************
       BW-TABLE-CHECK SECTION.
      ******************************************************************
           IF ELE NOT = 1
              SUBTRACT 1 FROM ELE GIVING I-LEVEL
              IF PD-SSNO(I-LEVEL) = 0
                 MOVE "Y" TO BW-ERROR-YN
                 MOVE "ENTER SERIALLY" TO MESS
                 PERFORM SEND-MESS
                 GO TO EXIT-BW-TABLE-CHECK.
           MOVE 4 TO I-LEVEL.
       NEXT-BW-LEVEL-CHECK.
           IF PD-SSNO(I-LEVEL) = PD-SSNO(ELE) AND I-LEVEL NOT = ELE
              MOVE "Y" TO BW-ERROR-YN
              MOVE "NO DUPLICATES" TO MESS
              PERFORM SEND-MESS
              GO TO EXIT-BW-TABLE-CHECK.
           SUBTRACT 1 FROM I-LEVEL.
           IF I-LEVEL > 0
              GO TO NEXT-BW-LEVEL-CHECK.
       EXIT-BW-TABLE-CHECK.
           EXIT.

      ******************************************************************
       BW-CHECK-ENDORSER SECTION.
      ******************************************************************
           MOVE 0 TO I-LEVEL.
       NEXT-ENDORSER-CHECK.
           ADD 1 TO I-LEVEL.
           IF PD-MAKERCD(I-LEVEL) = "E"
              MOVE " " TO CHFG(20)
              GO TO EXIT-CHECK-ENDORSER.
           IF I-LEVEL < 4
              GO TO NEXT-ENDORSER-CHECK.
       EXIT-CHECK-ENDORSER.
           EXIT.

      ******************************************************************
       SUBCLASS-SCAN SECTION.
      ******************************************************************
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
               GO TO EXIT-SUBCLASS-SCAN.

           MOVE "SNNN020052." TO VDU.
           MOVE SCSCAN-SP-SUBCLASS TO VDUNUM0.
           PERFORM SCREENIO.
           MOVE SCSCAN-SP-SUBCLASS TO VDUNUM0.

       EXIT-SUBCLASS-SCAN.
           EXIT.

      ********************************************
       BORROWER-SCAN SECTION.
      ********************************************
           MOVE "NAME, F1-CLEAR, F6-SCAN, <ENTER>-ACCEPT" TO MESS.
           PERFORM SEND-LEGEND.

           MOVE 0 TO IO-FG.
           MOVE " " TO ALP-BW.
           PERFORM OPEN-BW2-FILE.

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
      *       MOVE ALP-BW TO VDUBUF
              MOVE ALP-BW123  TO NUM-BW123
              MOVE ALP-BW45   TO NUM-BW45
              MOVE ALP-BW6789 TO NUM-BW6789
              MOVE NUM-BW TO VDUNUM0
              GO TO EXIT-BORROWER-SCAN.

           PERFORM ARRANGE-BW-KEY.

           MOVE BW-PATH-OWNBR TO BW-OWNBR.
           MOVE BW-ALP-TABLE  TO BW-NAME.

           PERFORM START-BW2-FILE.
       BORROWER-READ-NEXT.
           PERFORM READ-BW2-FILE-NEXT.
           IF (IO-FG = 9) OR (BW-OWNBR NOT = BW-PATH-OWNBR)
              GO TO EXIT-BORROWER-SCAN.

           PERFORM APPEND-BW-NAMES.
           PERFORM SEND-BW-NO-NAME.

           GO TO BORROWER-GET-KEY.
       EXIT-BORROWER-SCAN.
           PERFORM CLOSE-BW2-FILE.

      ******************************************************************
       ARRANGE-BW-KEY SECTION.
      ******************************************************************
           MOVE 1 TO IX1 IX2.
           MOVE SPACES TO BW-ALP-TABLE.
       ARR-BW-KEY-AGAIN.
           IF IX1 NOT < 35
              GO TO ARR-BW-KEY-EXIT.
           IF VDUTAB(IX1) NOT = ","
              MOVE VDUTAB(IX1) TO BW-ALP-TBL(IX2)
           ELSE
              MOVE 35 TO IX2.
           ADD 1 TO IX1.
           ADD 1 TO IX2.
           GO TO ARR-BW-KEY-AGAIN.
       ARR-BW-KEY-EXIT.
           EXIT.

      ******************************************************************
       CODE-SCAN SECTION.

           MOVE "EL"   TO ELSCAN-TYPE.
           MOVE ZEROES TO ELSCAN-CODE.
           MOVE "WI/ELSCAN " TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH ELSCAN-BUF EXIT-PATHNAME.
           CANCEL FORM-PROGX.

           IF ( ELSCAN-STATUS NOT = "00") OR ( ELSCAN-CODE = 0 )
               MOVE 9 TO IO-FG
               GO TO EXIT-CODE-SCAN.

           MOVE "SNNN020380." TO VDU.
           MOVE ELSCAN-CODE   TO VDUNUM0.
           PERFORM SCREENIO.

           MOVE ZEROES        TO IO-FG.
           MOVE ELSCAN-CODE   TO VDUNUM0.

       EXIT-CODE-SCAN.
           EXIT.

      ******************************************************************
       INSCOMP-SCAN SECTION.
      ******************************************************************
           MOVE 0 TO ICSCAN-KEY.
           MOVE "WI/ICSCAN" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH ICSCAN-BUF EXIT-PATHNAME.
           CANCEL FORM-PROGX.
           IF ICSCAN-STATUS NOT = "00" OR ICSCAN-KEY = ZEROES
               MOVE 9 TO IO-FG.

       EXIT-INSCOMP-SCAN.
           EXIT.

      ******************************************************************
       INSURANCE-ENTRY SECTION.
      ******************************************************************
           MOVE 0 TO IO-FG.
           SUBTRACT 26 FROM ELE GIVING I-LEVEL.
           MOVE VDUNUM0 TO PD-INSCOMP(I-LEVEL) IC-CODE.
           IF IC-CODE = 0
              GO TO ENTER-IC-OURS.
           PERFORM OPEN-IC1-FILE.
           PERFORM READ-IC1-FILE.
           PERFORM CLOSE-IC1-FILE.
           IF IO-FG = 9
              MOVE "INVALID COMPANY" TO MESS
              PERFORM SEND-MESS
              GO TO EXIT-ENTER-INSURANCE.
           PERFORM COMPUTE-INS-TERM-I-LEVEL.
       ENTER-IC-OURS.
           IF IC-CODE = 0
              MOVE " " TO PD-INSOURS(I-LEVEL)
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
           MOVE VDUBUF TO PD-INSOURS(I-LEVEL).
       SEND-IC-OURS.
           MOVE "S  A010  1." TO VDU.
           MOVE ELE TO VDUROW.
           MOVE PD-INSOURS(I-LEVEL) TO VDUBUF.
           PERFORM SCREENIO.
           PERFORM RESET-LEGEND.
           IF IC-CODE NOT = 0
              IF I-LEVEL > 4
                 GO TO ENTER-IC-COVR
              ELSE
                 GO TO ENTER-IC-PREM.
       ENTER-IC-TERM.
           IF I-LEVEL > 4 AND GP-OT-INS-TERM NOT = "Y"
              GO TO ENTER-IC-EXP.
           IF IC-CODE = 0
              MOVE 0 TO PD-INSTRM(I-LEVEL)
              GO TO SEND-IC-TERM.
           MOVE "RNNN030  2." TO VDU.
           MOVE ELE TO VDUROW.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-IC-OURS.
           IF VDUSTATUS NOT = "00" AND NOT = "1D"
              GO TO ENTER-IC-TERM.
           IF ( VDUNUM0 > 360 )
              GO TO ENTER-IC-TERM.
           MOVE VDUNUM0 TO PD-INSTRM(I-LEVEL).
       SEND-IC-TERM.
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
           IF VDUSTATUS NOT = "00" AND NOT = "1D"
              GO TO ENTER-IC-EXP.
           IF VDUNUM0 = 0
              GO TO ENTER-IC-EXP.
           MOVE VDUNUM0 TO DATE-MMDDYY.
           PERFORM CONVERT-MMDDYY-TO-YYYYMMDD.
           MOVE DATE-YYYYMMDD TO PD-INSEXP-DATE(I-LEVEL)
                                 NUM-DATE.
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
           IF VDUSTATUS NOT = "00" AND NOT = "1D"
              GO TO ENTER-IC-COVR.
           MOVE VDUNUM2 TO PD-INSCOVR(I-LEVEL).
       SEND-IC-COVR.
           MOVE "SN N062  3." TO VDU.
           MOVE ELE TO VDUROW.
           MOVE PD-INSCOVR(I-LEVEL) TO VDUNUM2.
           PERFORM SCREENIO.
       ENTER-IC-PREM.
           MOVE " " TO PREM-CHANGE.
           MOVE PD-INSPREM(I-LEVEL) TO PREM-AMOUNT.
           IF IC-CODE = 0
              MOVE 0 TO PD-INSPREM(I-LEVEL)
              GO TO SEND-IC-PREM.
           MOVE "RNNN042  4." TO VDU.
           MOVE ELE TO VDUROW.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-IC-COVR.
           IF VDUSTATUS NOT = "00" AND VDUSTATUS NOT = "1D"
              GO TO ENTER-IC-PREM.
           MOVE VDUNUM2 TO PD-INSPREM(I-LEVEL).
       SEND-IC-PREM.
           MOVE "SN N042  4." TO VDU.
           MOVE ELE TO VDUROW.
           MOVE PD-INSPREM(I-LEVEL) TO VDUNUM2.
           PERFORM SCREENIO.
           IF PD-INSPREM(I-LEVEL) NOT = PREM-AMOUNT
              MOVE "Y" TO PREM-CHANGE.
       EXIT-ENTER-INSURANCE.
           EXIT.

      ******************************************************************
       FEE-ENTRY SECTION.
      ******************************************************************
           MOVE VDUBUF TO CDB-CODE PD-FEECODE(I-LEVEL).

           IF CDB-CODE NOT = SPACES
              PERFORM GET-CODE
              MOVE CDB-DESC TO MESS
              PERFORM SEND-LEGEND
              IF IO-FG = 9
                 GO TO EXIT-FEE-ENTRY
              ELSE
                 MOVE CDB-GC-GLFEEFINCHG TO PD-FEEFINCHG(I-LEVEL)
                 GO TO ENTER-FEEAMT.

           IF I-LEVEL = 2 OR 4 OR 6
              GO TO GL-CLEAR-2.

       NEXT-GL-CLEAR.
           MOVE "S  A000  0." TO VDU.
           MOVE ELE TO VDUROW.
           MOVE SPACES TO PD-FEECODE(I-LEVEL).
           PERFORM SCREENIO.
           MOVE "SN N042  1." TO VDU.
           MOVE ELE TO VDUROW.
           MOVE 0 TO PD-FEEAMT(I-LEVEL) VDUNUM0.
           PERFORM SCREENIO.
      *CS 03/26/01
           MOVE SPACES TO PD-FEEFINCHG(I-LEVEL).
           ADD 1 TO I-LEVEL.

       GL-CLEAR-2.
           MOVE "S  A020  2." TO VDU.
           MOVE ELE TO VDUROW.
           MOVE SPACES TO PD-FEECODE(I-LEVEL).
           PERFORM SCREENIO.
           MOVE "SN N042  3." TO VDU.
           MOVE ELE TO VDUROW.
           MOVE 0 TO PD-FEEAMT(I-LEVEL) VDUNUM0.
           PERFORM SCREENIO.
      *CS  03/26/01
           MOVE SPACES TO PD-FEEFINCHG(I-LEVEL).
           IF I-LEVEL < 6
              ADD 1 TO I-LEVEL ELE
              GO TO NEXT-GL-CLEAR.
           MOVE 0 TO IO-FG.
           GO TO EXIT-FEE-ENTRY.
       ENTER-FEEAMT.
           IF I-LEVEL > 2
              SUBTRACT 1 FROM I-LEVEL GIVING SUB
              IF PD-FEECODE(SUB) = SPACES
                 MOVE 9 TO IO-FG
                 GO TO EXIT-FEE-ENTRY.
           IF I-LEVEL = 2 OR 4 OR 6
              MOVE "ENNN042  3." TO VDU
           ELSE
              MOVE "ENNN042  1." TO VDU.
           MOVE ELE TO VDUROW.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              MOVE 9 TO IO-FG
              GO TO EXIT-FEE-ENTRY.
           IF VDUSTATUS NOT = "00"
              GO TO ENTER-FEEAMT.
           IF VDUNUM2 = 0
              GO TO ENTER-FEEAMT.
           MOVE VDUNUM2 TO PD-FEEAMT(I-LEVEL).

           IF ( PD-FEEAMT(I-LEVEL) > CDB-GC-FEE-MAXAMT )
              MOVE "INVALID: AMOUNT IS GREATER THAN THE LIMIT" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-FEEAMT.

           MOVE SPACES TO MESS.
           PERFORM SEND-LEGEND.

       EXIT-FEE-ENTRY.
           EXIT.

      ******************************************************************
       CHECK-EFFECTIVE-SPR SECTION.
      ******************************************************************
           MOVE PD-ORGST    TO SP-ORGST.
           MOVE PD-SPRCLASS TO SP-SPRCLASS.
           MOVE PD-SUBCLASS TO SP-SUBCLASS.
           MOVE 0           TO SP-LAWCODE.

           PERFORM OPEN-SP1-FILE.
           PERFORM START-SP1-FILE.
       CHECK-EFF-SPR-NEXT.
           PERFORM READ-SP1-FILE-NEXT.
           IF IO-FG = 9
              GO TO CHECK-EFF-SPR-EXIT.
           IF NOT (SP-ORGST = PD-ORGST
                   AND SP-SPRCLASS = PD-SPRCLASS
                    AND SP-SUBCLASS = PD-SUBCLASS)
              MOVE 9 TO IO-FG
              GO TO CHECK-EFF-SPR-EXIT.

           IF SP-EFFDATE > PD-LOANDATE
              GO TO CHECK-EFF-SPR-NEXT
           ELSE
              IF SP-EXPDATE = 0
                 GO TO CHECK-EFF-SPR-EXIT.
           IF SP-EXPDATE NOT > PD-LOANDATE
              GO TO CHECK-EFF-SPR-NEXT.

       CHECK-EFF-SPR-EXIT.
           PERFORM CLOSE-SP1-FILE.

      ******************************************************************
       COMPUTE-PRINCIPLE SECTION.
      ******************************************************************
           SUBTRACT PD-NOODDPY FROM PD-ORGTERM GIVING PD-NOREGPY.
           PERFORM LOAN-CALCULATIONS.
           PERFORM LOAN-CALCULATIONS.
           MOVE "SN N062PRIN." TO VDU.
           MOVE PRINCIPLE TO VDUNUM2 PD-LNAMT PD-PURBAL PD-ORIGPURAMT
                             PD-SUM-LNAMT PD-SUM-ORIGPURAMT.
           PERFORM SCREENIO.

      ******************************************************************
       COMPUTE-INS-COVERAGE SECTION.
      ******************************************************************
           IF PD-INSTYPES-LL OR PD-INSTYPES-DL
              IF PD-INSPREM(1) NOT = 0
                 MOVE "SN N062273." TO VDU
                 MOVE 1 TO PD-SUB
                 IF PD-INSTYPES-LL
                    MOVE 3 TO SP-SUB
                    PERFORM SET-COVERAGE
                 ELSE
                    MOVE 1 TO SP-SUB
                    PERFORM SET-COVERAGE.
           IF PD-INSTYPES-AH
              IF PD-INSPREM(2) NOT = 0
                 MOVE "SN N062283." TO VDU
                 MOVE 5 TO SP-SUB
                 MOVE 2 TO PD-SUB
                 PERFORM SET-COVERAGE.
           IF PD-INSTYPES-PP OR PD-INSTYPES-PP2
              IF PD-INSPREM(3) NOT = 0
                 MOVE "SN N062293." TO VDU
                 MOVE 3 TO PD-SUB
                 IF PD-INSTYPES-PP
                    MOVE 6 TO SP-SUB
                    PERFORM SET-COVERAGE
                 ELSE
                    MOVE 7 TO SP-SUB
                    PERFORM SET-COVERAGE.
           IF PD-INSTYPES-O1
              IF PD-INSCOMP(4) NOT = 0
                 MOVE "SN N062303." TO VDU
                 MOVE 8 TO SP-SUB
                 MOVE 4 TO PD-SUB
                 PERFORM SET-COVERAGE.
           GO TO COMPUTE-INS-COVERAGE-EXIT.
       SET-COVERAGE.
           IF PD-INSURED = "J"
              IF SP-SUB = 1 OR 3
                 ADD 1 TO SP-SUB.

           IF SP-INS-TOTAL-NOTE(SP-SUB)
              MOVE TOTAL-NOTE TO VDUNUM2 PD-INSCOVR(PD-SUB)
           ELSE
           IF SP-INS-PRINCIPAL(SP-SUB)
              MOVE DISCLOSED-FINAMT TO VDUNUM2 PD-INSCOVR(PD-SUB)
           ELSE
           IF SP-INS-CASH-ADVANCE(SP-SUB)
              MOVE CASH-ADVANCE TO VDUNUM2 PD-INSCOVR(PD-SUB)
           ELSE
           IF SP-INS-SP-CASH-ADVANCE(SP-SUB)
              MOVE SPCASH-ADVANCE TO VDUNUM2 PD-INSCOVR(PD-SUB)
           ELSE
           IF SP-INS-CASH-ADV-FEES(SP-SUB)
              COMPUTE PD-INSCOVR(PD-SUB) = CASH-ADVANCE - NONMEMO-FEES
              MOVE PD-INSCOVR(PD-SUB) TO VDUNUM2
           ELSE
           IF SP-INS-TOTAL-NOTE-EXT(SP-SUB)
              ADD TOTAL-NOTE PD-EXTCHG GIVING VDUNUM2
                                              PD-INSCOVR(PD-SUB)
           ELSE
           IF SP-INS-ALL(SP-SUB)
              MOVE TOTAL-REPAY TO VDUNUM2 PD-INSCOVR(PD-SUB)
           ELSE
           IF SP-INS-ADJ-PRINCIPAL(SP-SUB)
              MOVE ADJ-PRINCIPAL TO VDUNUM2 PD-INSCOVR(PD-SUB)
           ELSE
           IF SP-INS-BENEFIT-TERM(SP-SUB)
              COMPUTE PD-INSCOVR(PD-SUB) = TOTAL-NOTE / PD-ORGTERM
                                         * PD-INSTRM(PD-SUB)
              MOVE PD-INSCOVR(PD-SUB) TO VDUNUM2
           ELSE
           IF SP-INS-ADJ-CASH-ADVANCE(SP-SUB)
              MOVE HIGH-CREDIT TO VDUNUM2 PD-INSCOVR(PD-SUB)
           ELSE
           IF SP-INS-ADJ-PRIN-3PMTS(SP-SUB)
              COMPUTE PD-INSCOVR(PD-SUB) =
                  DISCLOSED-FINAMT + (3 * PD-REGPYAMT)
              MOVE PD-INSCOVR(PD-SUB) TO VDUNUM2
           ELSE
           IF SP-INS-AMTFIN-2PMTS(SP-SUB)
              COMPUTE PD-INSCOVR(PD-SUB) =
                  DISCLOSED-FINAMT + (2 * PD-REGPYAMT)
              MOVE PD-INSCOVR(PD-SUB) TO VDUNUM2
           ELSE
           IF SP-INS-AMTFIN-1PMTS(SP-SUB)
              COMPUTE PD-INSCOVR(PD-SUB) =
                  DISCLOSED-FINAMT + PD-REGPYAMT
              MOVE PD-INSCOVR(PD-SUB) TO VDUNUM2
           ELSE
           IF SP-INS-MIP(SP-SUB)
              IF PD-SUB = 1
                 MOVE SPCASH-ADVANCE TO VDUNUM2
                                        PD-INSCOVR(PD-SUB)
              ELSE
                 COMPUTE PD-INSCOVR(PD-SUB) =
                     TOTAL-NOTE / PD-ORGTERM
                               * PD-INSTRM(PD-SUB)
                 MOVE PD-INSCOVR(PD-SUB) TO VDUNUM2
              END-IF
           ELSE
           IF SP-INS-TN-INT-CL(SP-SUB)
              COMPUTE PD-INSCOVR(PD-SUB) =
                 PD-PROCEEDS + NONMEMO-FEES + PD-SERCHG
                    + (PD-PRBAL + PD-PRINTDUE - PD-PRFINCHGRB
                         - PD-PRINSRB) + PD-INSPREM(2) + PD-INSPREM(3)
                    + PD-INSPREM(4) + PD-INSPREM(5) + PD-INSPREM(6)
           ELSE
           IF SP-INS-TN-INT-SC-CL(SP-SUB)
              COMPUTE PD-INSCOVR(PD-SUB) = PD-PROCEEDS + NONMEMO-FEES
                    + PD-INSPREM(2) + PD-INSPREM(3)
                    + PD-INSPREM(4) + PD-INSPREM(5) + PD-INSPREM(6)
           ELSE
           IF SP-INS-NET-PAYOFF(SP-SUB)
              MOVE DISCLOSED-FINAMT TO PD-INSCOVR(PD-SUB)
           ELSE
              MOVE 0 TO VDUNUM2 PD-INSCOVR(PD-SUB).

           IF SP-INS-MAX-TERM-BENEFIT(SP-SUB)
              IF PD-ORGTERM > SP-INSMAXTERM(SP-SUB)
                 IF PD-REGPYAMT > SP-INSMAXBENIF(SP-SUB)
      * BOTH TERM AND PAYMENT EXCEED MAX
                    COMPUTE PD-INSCOVR(PD-SUB) =
                     ( SP-INSMAXTERM(SP-SUB) * SP-INSMAXBENIF(SP-SUB) )
                                - PD-SUM-MAINTFEE - PD-EXTCHG
                    MOVE PD-INSCOVR(PD-SUB) TO VDUNUM2
                 ELSE
      * TERM EXCEEDS MAX BUT PAYMENT DOES NOT
                    COMPUTE PD-INSCOVR(PD-SUB) =
                          ( SP-INSMAXTERM(SP-SUB) * PD-REGPYAMT )
                                - PD-SUM-MAINTFEE - PD-EXTCHG
                    MOVE PD-INSCOVR(PD-SUB) TO VDUNUM2
              ELSE
                 IF PD-REGPYAMT > SP-INSMAXBENIF(SP-SUB)
      * TERM DOES NOT EXCEED MAX BUT PAYMENT DOES
                    COMPUTE PD-INSCOVR(PD-SUB) =
                          ( PD-ORGTERM * SP-INSMAXBENIF(SP-SUB) )
                                - PD-SUM-MAINTFEE - PD-EXTCHG
                    MOVE PD-INSCOVR(PD-SUB) TO VDUNUM2
                  ELSE
                     MOVE TOTAL-NOTE TO PD-INSCOVR(PD-SUB) VDUNUM2.

           PERFORM SCREENIO.

       COMPUTE-INS-COVERAGE-EXIT.
           EXIT.

      ******************************************************************
       COMPUTE-INS-TERM SECTION.
      ******************************************************************
           MOVE 0 TO I-LEVEL.
       COMPUTE-INS-TRM.
           IF I-LEVEL < 5
              ADD 1 TO I-LEVEL
              PERFORM COMPUTE-INS-TERM-I-LEVEL
              GO TO COMPUTE-INS-TRM.

      ******************************************************************
       COMPUTE-INS-TERM-I-LEVEL SECTION.
      ******************************************************************
           IF I-LEVEL = 1
              IF PD-INSCOMP(1) NOT = 0
                 MOVE 0 TO SUB
                 IF PD-INSURED = "J"
                    MOVE 1 TO SUB.

           IF I-LEVEL = 1
              IF PD-INSCOMP(1) NOT = 0
                 MOVE "SN N030272." TO VDU
                 IF PD-INSTYPES-DL
                    ADD 1 TO SUB
                    PERFORM SET-INSURANCE-TERM-VDU
                    MOVE VDUNUM0 TO PD-INSTRM(1)
                    PERFORM SCREENIO
                 ELSE
                    IF PD-INSTYPES-LL
                       ADD 3 TO SUB
                       PERFORM SET-INSURANCE-TERM-VDU
                       MOVE VDUNUM0 TO PD-INSTRM(1)
                       PERFORM SCREENIO.

           IF I-LEVEL = 2
              IF PD-INSCOMP(2) NOT = 0
                 IF PD-INSTYPES-AH
                    MOVE "SN N030282." TO VDU
                    MOVE 5 TO SUB
                    PERFORM SET-INSURANCE-TERM-VDU
                    MOVE VDUNUM0 TO PD-INSTRM(2)
                    PERFORM SCREENIO.

           IF I-LEVEL = 3
              IF PD-INSCOMP(3) NOT = 0
                 MOVE "SN N030292." TO VDU
                 IF PD-INSTYPES-PP
                    MOVE 6 TO SUB
                    PERFORM SET-INSURANCE-TERM-VDU
                    MOVE VDUNUM0 TO PD-INSTRM(3)
                    PERFORM SCREENIO
                 ELSE
                    IF PD-INSTYPES-PP2
                       MOVE 7 TO SUB
                       PERFORM SET-INSURANCE-TERM-VDU
                       MOVE VDUNUM0 TO PD-INSTRM(3)
                       PERFORM SCREENIO.

           IF I-LEVEL = 4
              IF PD-INSCOMP(4) NOT = 0
                 IF PD-INSTYPES-O1
                    MOVE "SN N030302." TO VDU
                    MOVE 8 TO SUB
                    PERFORM SET-INSURANCE-TERM-VDU
                    MOVE VDUNUM0 TO PD-INSTRM(4)
                    PERFORM SCREENIO.

           IF I-LEVEL = 5
              IF PD-INSCOMP(5) NOT = 0
                 IF GP-OT-INS-TERM = "Y"
                    MOVE "SNNN060312." TO VDU
                    MOVE 9 TO SUB
                    PERFORM SET-INSURANCE-TERM-VDU
                    MOVE VDUNUM0 TO PD-INSTRM(5)
                    PERFORM SCREENIO
                 ELSE
                    MOVE PD-INTDATE TO NDTE-DATE
                    MOVE PD-ORGTERM TO NDTE-HOLD
                    MOVE PD-UNITPER-CD TO DATER-UNITPER-CD
                    MOVE PD-UNITPER-FREQ TO DATER-UNITPER-FREQ
                    PERFORM INCREMENT-UNITPER
                    MOVE "SNNN060312." TO VDU
                    MOVE NDTE-DATE TO DATE-YYYYMMDD
                                      PD-INSEXP-DATE(5)
                    PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
                    MOVE DATE-MMDDYY TO VDUNUM0 
                    PERFORM SCREENIO.

           IF I-LEVEL = 6
              IF PD-INSCOMP(6) NOT = 0
                 IF GP-OT-INS-TERM = "Y"
                    MOVE "SNNN060322." TO VDU
                    MOVE 10 TO SUB
                    PERFORM SET-INSURANCE-TERM-VDU
                    MOVE VDUNUM0 TO PD-INSTRM(I-LEVEL)
                    PERFORM SCREENIO
                 ELSE
                    MOVE PD-INTDATE TO NDTE-DATE
                    MOVE PD-ORGTERM TO NDTE-HOLD
                    MOVE PD-UNITPER-CD TO DATER-UNITPER-CD
                    MOVE PD-UNITPER-FREQ TO DATER-UNITPER-FREQ
                    PERFORM INCREMENT-UNITPER
                    MOVE "SNNN060322." TO VDU
                    MOVE NDTE-DATE TO DATE-YYYYMMDD
                                      PD-INSEXP-DATE(6)
                    PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
                    MOVE DATE-MMDDYY TO VDUNUM0
                    PERFORM SCREENIO.

      ******************************************************************
       SET-INSURANCE-TERM-VDU SECTION.
      ******************************************************************
           IF SP-INSURE-EXT-PERIOD(SUB)
              PERFORM EXTENSION-DAYS-CALCULATION
              IF EDAY-MONTHS > 0
                 COMPUTE VDUNUM0 = EDAY-MONTHS + PD-ORGTERM
              ELSE
                 MOVE PD-ORGTERM TO VDUNUM0
           ELSE
              MOVE PD-ORGTERM TO VDUNUM0.

      ******************************************************************
       COMPUTE-PROCEEDS SECTION.
      ******************************************************************
           MOVE " " TO STOP-UPDATE-FLAG.
           MOVE 0 TO SUB NON-MEMO-INS.
       NEXT-INSPREM.
           ADD 1 TO SUB.
           IF PD-INSOURS(SUB) NOT = "M"
              ADD PD-INSPREM(SUB) TO NON-MEMO-INS.
           IF SUB < 6
              GO TO NEXT-INSPREM.

           MOVE "SN N062FIN." TO VDU.
           COMPUTE VDUNUM2 = TOTAL-REPAY - NON-MEMO-INS - PD-EXTCHG
                           - PD-INTCHG - PD-SERCHG
                           - (PD-ORGTERM * PD-MAINTFEE).

           MOVE 0 TO SUB.
       NEXT-FEEAMT.
           ADD 1 TO SUB.
           SUBTRACT PD-FEEAMT(SUB) FROM VDUNUM2.
           IF SUB < 6
              GO TO NEXT-FEEAMT.

           IF PRIOR-LNAMT > VDUNUM2
              MOVE "PRIOR LOAN BALANCE EXCEEDS NEW PROCEEDS" TO MESS
              PERFORM SEND-MESS
              MOVE "Y" TO STOP-UPDATE-FLAG
              GO TO COMPUTE-BORROWER-PROC-EXIT.

           SUBTRACT PRIOR-LNAMT FROM VDUNUM2.
           MOVE VDUNUM2 TO PD-PROCEEDS.
           PERFORM SCREENIO.

      *    DEFAULT EXCEPT WHEN MANUAL OVERRIDE OCCURS
           IF DEFAULT-APR-FG = "N"
              GO TO COMPUTE-BORROWER-PROC-EXIT.
           MOVE "N" TO DEFAULT-APR-FG.
           COMPUTE APR-FINANCED = (PD-REGPYAMT * PD-NOREGPY) +
                                  (PD-1STPYAMT * PD-NOODDPY) -
                                  PD-EXTCHG - PD-SERCHG - PD-INTCHG -
                                  (PD-MAINTFEE * PD-ORGTERM).
           SUBTRACT TOTAL-FEEFINCHG TOTAL-INSFINCHG
                          FROM APR-FINANCED.
           MOVE 1 TO APR-PAYFREQ.
           MOVE PD-NOREGPY TO APR-NOREGPY.
           MOVE PD-NOODDPY TO APR-NOODDPY.
           MOVE PD-REGPYAMT TO APR-REGPYAMT.
           MOVE PD-1STPYAMT TO APR-1STPYAMT.
           MOVE PD-INTDATE TO APR-INTDATE.
           MOVE PD-1STPYDATE TO APR-1STPYDATE.
           MOVE PD-EXTCHG TO APR-EXTCHG.
           PERFORM APR-REGZ.
           MOVE "SN N033180." TO VDU.
           MOVE APR-APR TO VDUNUM3 PD-APRATE.
           PERFORM SCREENIO.
       COMPUTE-BORROWER-PROC-EXIT.
           EXIT.

      ******************************************************************
       COMPUTE-ANTICIPATED SECTION.
      ******************************************************************
           IF PD-LOANTYPE = "I"
              MOVE PD-SERCHG TO PD-ANTICERN(3)
              MOVE 0 TO PD-ANTICERN(1)
                        PD-ANTICERN(2)
                        PD-ANTICERN(5)
                        PD-ANTICERN(6)
              IF GP-ALLOW-MF-ON-IB = "Y"
                 COMPUTE PD-ANTICERN(4) = PD-ORGTERM * PD-MAINTFEE
              ELSE
                 MOVE 0 TO PD-ANTICERN(4)
              END-IF
           ELSE
              ADD PD-INTCHG PD-EXTCHG GIVING PD-ANTICERN(1)
              MOVE PD-SERCHG TO PD-ANTICERN(3)
              COMPUTE PD-ANTICERN(4) = PD-ORGTERM * PD-MAINTFEE
              MOVE 0 TO PD-ANTICERN(5).
            IF NOT (SP-LNCOST-FRMLA = "M")
               MOVE ZEROS TO  PD-ANTICERN(6).

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

           MOVE "SN N062370." TO VDU.
           COMPUTE VDUNUM2 = PD-ANTICERN(1) + PD-ANTICERN(2)
                           + PD-ANTICERN(3) + PD-ANTICERN(4)
                           + PD-ANTICERN(5).
           IF NOT GP-INCL-OTH-IN-EARNED-N
              ADD PD-ANTICERN(6) TO VDUNUM2.
           PERFORM SCREENIO.

      ******************************************************************
       COMPUTE-FINANCE-CHARGE SECTION.
      ******************************************************************
           MOVE PD-INTCHG TO PD-FINCHG.
           IF SP-FINCHG = 1
              COMPUTE PD-FINCHG = PD-INTCHG + PD-EXTCHG
           ELSE IF SP-FINCHG = 2
              COMPUTE PD-FINCHG = PD-INTCHG + PD-SERCHG
           ELSE IF SP-FINCHG = 3
              COMPUTE PD-FINCHG = PD-INTCHG +
                                  (PD-MAINTFEE * PD-ORGTERM)
           ELSE IF SP-FINCHG = 4
              COMPUTE PD-FINCHG = PD-INTCHG + PD-EXTCHG
                                + PD-SERCHG
           ELSE IF SP-FINCHG = 5
              COMPUTE PD-FINCHG = PD-INTCHG + PD-EXTCHG
                                + (PD-MAINTFEE * PD-ORGTERM)
           ELSE IF SP-FINCHG = 6
              COMPUTE PD-FINCHG = PD-INTCHG + PD-EXTCHG
                          + PD-SERCHG
                          + (PD-MAINTFEE * PD-ORGTERM)
           ELSE IF SP-FINCHG = 7
              COMPUTE PD-FINCHG = PD-INTCHG
                                + PD-SERCHG
                         + (PD-MAINTFEE * PD-ORGTERM)
           ELSE IF SP-FINCHG = 8
              COMPUTE PD-FINCHG = PD-SERCHG +
                      (PD-MAINTFEE * PD-ORGTERM).
      *CS  03/26/01
           ADD TOTAL-INSFINCHG TOTAL-FEEFINCHG TO PD-FINCHG.

           MOVE PD-FINCHG TO PD-SUM-FINCHG.
           IF PD-FINCHG < SP-MINSF-CHG
              MOVE "FINANCE CHARGE BELOW MINIMUM" TO MESS
              PERFORM SEND-MESS.

      ******************************************************************
       MEMO-ENTRY SECTION.
      ******************************************************************
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

      ******************************************************************
       NEW-RECORD-SETUP SECTION.
      ******************************************************************
           MOVE 1 TO NEW-FLAG.
           PERFORM CLEAR-PD-FILE.
      * CANT BE IN COPY MEMBER BECAUSE THESE ARE IN REDEFINE OF
      * FREE TYPE WHICH MUST BE SPACES IN "LONPB0"
           MOVE 0 TO PD-PRPAID-AHEAD PD-DUAL-LOAN-ADJ.

           MOVE TRANS-DATE TO DEF-LOANDATE PD-LOANDATE.
           MOVE BR-STATE TO DEF-ORGST PD-ORGST PD-OWNST.
           MOVE BRANCH TO DEF-ORGBR PD-ORGBR PD-OWNBR.
           MOVE 1 TO DEF-PAYFREQ.
           MOVE 999.99 TO PD-SERVPER.
           MOVE 999.999 TO PD-ENDORATE PD-OVRDRATE.
           MOVE DEF-BUF  TO WR-BUF.
           MOVE DEF-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

           MOVE "N" TO STAT.
           MOVE "Y" TO DEFAULT-APR-FG.
           PERFORM PROTECT-UNPROTECT.
           MOVE 9999.99 TO PD-DLNO DL-DLNO.
           PERFORM GET-DL1.
           IF BR-LC-CODE = "Y"
              MOVE "SNNN020380." TO VDU
              MOVE DL-LC-CODE TO VDUNUM0 PD-LC-CODE
              PERFORM SCREENIO.
           MOVE 0 TO ELE PRIOR-LNAMT.
      * CLEAR WK-SAVE-RECORD FOR COLLATERAL
           PERFORM CREATE-TRAILER.

      ******************************************************************
       DISPLAY-FIELDS SECTION.
      ******************************************************************
           MOVE " " TO DISPLAY-BUF1.
           MOVE PD-CLASS TO LC-CODE D-LCNO.
           PERFORM GET-LC.
           MOVE LC-DESC TO D-LC-DESC.
           PERFORM GET-SP.

           MOVE 1 TO ELE.
       DISP-NEXT-BW.
           MOVE PD-SSNO(ELE) TO BW-SSNO.
           PERFORM NUMBW-TO-ALPBW.
           MOVE " " TO BW-LNAME BW-FNAME.
           IF BW-SSNO NOT = 0
              PERFORM GET-BW1
              MOVE ALP-BW TO D-010(ELE)
              MOVE INSPECT-W TO D-011(ELE)
              MOVE PD-MAKERCD(ELE) TO D-012(ELE).
           IF ELE < 4
              ADD 1 TO ELE
              GO TO DISP-NEXT-BW.

           MOVE PD-ORGST TO D-050.
           MOVE PD-ORGBR TO D-051.
           MOVE PD-SUBCLASS TO D-052.
           MOVE SP-DESC TO D-053.
           MOVE PD-LOANDATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO D-060.

           MOVE "PU"     TO CD-TYPE.
           MOVE PD-PURCD TO CD-CODE D-070.
           PERFORM GET-CD.
           MOVE CD-DESC TO D-071.
           MOVE "SR"    TO CD-TYPE.
           MOVE PD-SRCD TO CD-CODE D-080.
           PERFORM GET-CD.
           MOVE CD-DESC TO D-081.
           MOVE "CO"      TO CD-TYPE.
           MOVE PD-COLLCD TO CD-CODE D-090.
           PERFORM GET-CD.
           MOVE CD-DESC TO D-091.
           MOVE PD-COLLID TO D-100.
           MOVE PD-MSG1 TO D-110.
           MOVE PD-MSG2 TO D-111.
           STRING PD-MSG1,PD-MSG2 DELIMITED BY "   " INTO
              WK-HHGS-1.
           MOVE DISPLAY-BUF1 TO WR-BUF.
           MOVE DISPLAY-LIST1 TO WR-LIST.
           PERFORM CALL-WRFORM.

           MOVE " " TO DISPLAY-BUF2.
           IF BR-LC-CODE = "Y"
              MOVE PD-LC-CODE TO D-380.
           MOVE PD-DLNO TO D-120 DL-DLNO.
           PERFORM GET-DL1.
           MOVE DL-SHORT TO D-121.
           MOVE PD-SUM-ORGTERM TO D-130.
           MOVE 1 TO D-140.
           MOVE PD-FILEDATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO D-150.
           MOVE PD-INTDATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO D-160.
           MOVE PD-1STPYDATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO ALP-DATE-EDIT.
           MOVE PD-UNITPER-CD TO DATER-UNITPER-CD.
           MOVE PD-UNITPER-FREQ TO DATER-UNITPER-FREQ.
           PERFORM UNITPER-MASK.
           MOVE ALP-DATE-EDIT TO D-170.
           MOVE PD-APRATE TO D-180.
           MOVE PD-OVRDRATE TO D-190.
           MOVE PD-ENDORATE TO D-200.
           MOVE PD-REGPYAMT TO D-210.
           MOVE PD-1STPYAMT TO D-220.
           MOVE PD-SUM-EXTCHG TO D-230.
           MOVE PD-SUM-SERCHG TO D-240.
           MOVE PD-SUM-MAINTFEE TO D-241.
           MOVE PD-SUM-INTCHG TO D-250.
           MOVE PD-INSCODES TO D-260.

           MOVE 1 TO ELE.
       DISP-NEXT-IC.
           MOVE PD-INSCOMP(ELE) TO D-1COMP(ELE).
           MOVE PD-INSPREM(ELE) TO D-1PREM(ELE).
           MOVE PD-INSTRM(ELE) TO D-1TERM(ELE).
           MOVE PD-INSCOVR(ELE) TO D-1COVR(ELE).
           MOVE PD-INSOURS(ELE) TO D-1IOUR(ELE).
           IF ELE < 4
              ADD 1 TO ELE
              GO TO DISP-NEXT-IC.
           MOVE 1 TO SUB.
           MOVE 5 TO ELE.
       DISP-NEXT-2IC.
           MOVE PD-INSCOMP(ELE) TO D-2COMP(SUB).
           MOVE PD-INSPREM(ELE) TO D-2PREM(SUB).
           IF GP-OT-INS-TERM = "Y"
              MOVE PD-INSTRM(ELE) TO D-2TERM(SUB)
           ELSE
              MOVE PD-INSEXP-DATE(ELE) TO DATE-YYYYMMDD
              PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
              MOVE DATE-MMDDYY TO D-2EXP(SUB).
           MOVE PD-INSCOVR(ELE) TO D-2COVR(SUB).
           MOVE PD-INSOURS(ELE) TO D-2IOUR(SUB).
           IF ELE < 6
              ADD 1 TO ELE SUB
              GO TO DISP-NEXT-2IC.

           MOVE 1 TO ELE SUB.
       DISP-NEXT-GL.
           MOVE PD-FEECODE(SUB) TO D-1FEECD(ELE).
           MOVE PD-FEEAMT(SUB) TO D-1FEEAMT(ELE).
           ADD 1 TO SUB.
           MOVE PD-FEECODE(SUB) TO D-2FEECD(ELE).
           MOVE PD-FEEAMT(SUB) TO D-2FEEAMT(ELE).
           IF SUB < 6
              ADD 1 TO ELE SUB
              GO TO DISP-NEXT-GL.

           MOVE PD-PRLNNO TO D-360.
           IF NOT GP-INCL-OTH-IN-EARNED-N
              COMPUTE D-370 = PD-ANTICERN(1) + PD-ANTICERN(2)
                            + PD-ANTICERN(3) + PD-ANTICERN(4)
                            + PD-ANTICERN(5) + PD-ANTICERN(6)
           ELSE
              COMPUTE D-370 = PD-ANTICERN(1) + PD-ANTICERN(2)
                            + PD-ANTICERN(3) + PD-ANTICERN(4)
                            + PD-ANTICERN(5).
           MOVE DISPLAY-BUF2 TO WR-BUF.
           MOVE DISPLAY-LIST2 TO WR-LIST.
           PERFORM CALL-WRFORM.
           IF PD-MESSAGE NOT = " "
              PERFORM SEND-MEMO
              PERFORM ALARM.


      ************************************************
       ENTER-HI-CREDIT SECTION.
      ************************************************
       HI-CREDIT-ENTRY.
           MOVE SPACE TO IO-TYPE.
           IF BR-AVAIL-CREDIT NOT = "Y" AND NOT = "S"
              MOVE 0 TO PD-CREDITLIM PD-WCBC-CREDITLIM
              GO TO EXIT-ENTER-HI-CREDIT.

           IF LC-CALC-HI-CREDIT NOT = "W"
              MOVE 0 TO PD-WCBC-CREDITLIM
              GO TO SKIP-WCBC-CREDITLIM.

       ENTER-WCBC-CREDITLIM.
           MOVE "^-BACK, ENTER WCBC CREDIT LIMIT:" TO MESS.
           PERFORM SEND-LEGEND.
           MOVE "S  A060SEL." TO VDU.
           PERFORM SCREENIO.
           MOVE "EN N060CKNO." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              MOVE "RTN" TO IO-TYPE
              MOVE "S  A060CKNO." TO VDU
              PERFORM SCREENIO
              GO TO EXIT-ENTER-HI-CREDIT.
           IF VDUSTATUS NOT = "00"
              MOVE "S  A060CKNO." TO VDU
              PERFORM SCREENIO
              GO TO HI-CREDIT-ENTRY.
           MOVE VDUNUM0 TO PD-WCBC-CREDITLIM.
           MOVE "S  A060CKNO." TO VDU.
           PERFORM SCREENIO.
           MOVE 0 TO PD-CREDITLIM.
           GO TO EXIT-ENTER-HI-CREDIT.
       SKIP-WCBC-CREDITLIM.

           IF LC-CALC-HI-CREDIT = "C" OR "L" OR SPACE
              MOVE 0 TO PD-CREDITLIM
              GO TO EXIT-ENTER-HI-CREDIT.

           MOVE "^-BACK, ENTER CREDIT LIMIT:" TO MESS.
           PERFORM SEND-LEGEND.
           MOVE "S  A060SEL." TO VDU.
           PERFORM SCREENIO.
           MOVE "EN N060CKNO." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              MOVE "RTN" TO IO-TYPE
              MOVE "S  A060CKNO." TO VDU
              PERFORM SCREENIO
              GO TO EXIT-ENTER-HI-CREDIT.
           IF VDUSTATUS NOT = "00"
              MOVE "S  A060CKNO." TO VDU
              PERFORM SCREENIO
              GO TO SKIP-WCBC-CREDITLIM.
           MOVE VDUNUM0 TO PD-CREDITLIM.
           MOVE "S  A060CKNO." TO VDU.
           PERFORM SCREENIO.

       EXIT-ENTER-HI-CREDIT.
           EXIT.

      ******************************************************************
       SAVE-SCREEN SECTION.
      ******************************************************************
           MOVE 2 TO WK-KEY.
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
                 MOVE " " TO WK-OLD-PROG-FORM.
           MOVE ELE TO WK-ELE.
           MOVE PD-RECORD TO WK-SAVE-RECORD.
           PERFORM REWRITE-WK-FILE.
       EXIT-SAVE-SCREEN.
           PERFORM CLOSE-WK-FILE.

      ******************************************************************
       RESTORE-SCREEN SECTION.
      ******************************************************************
           MOVE 2 TO WK-KEY.
           PERFORM OPEN-WK-FILE.
           PERFORM READ-WK-FILE.
           IF NEW-PROG-FORM NOT = OLD-PROG-FORM
              MOVE OLD-PROG-FORM TO WK-OLD-PROG-FORM.
           MOVE NEW-PROG-FORM TO OLD-PROG-FORM.
           IF IO-FG = 0
              GO TO RESTORE-CLEAR.
           MOVE " " TO WK-SAVE-RECORD.
           MOVE 0 TO WK-ELE.
           PERFORM WRITE-WK-FILE.
           IF IO-FG = 9
              GO TO IO-ERROR.
           GO TO EXIT-RESTORE-SCREEN.
       RESTORE-CLEAR.
           IF WK-ELE NOT = 0
              SUBTRACT 1 FROM WK-ELE GIVING FIRST-ELE.
           MOVE WK-SAVE-RECORD TO PD-RECORD.
           MOVE 0 TO WK-ELE.
           PERFORM REWRITE-WK-FILE.
       EXIT-RESTORE-SCREEN.
           IF FIRST-ELE = 0
              MOVE 0 TO NEW-FLAG
           ELSE
              MOVE 1 TO NEW-FLAG.
           PERFORM CLOSE-WK-FILE.

      ************************************
       RESTORE-TRAILER-REC SECTION.
      ************************************
           MOVE 6 TO WK-KEY.
           PERFORM OPEN-WK-FILE.
           PERFORM READ-WK-FILE.
           PERFORM CLOSE-WK-FILE.

      * INSPECT-X HOLDS FREE FROM BEFORE CALL
           IF INSPECT-X = WK-HHGS-1
              GO TO RESTORE-TRAILER-EXIT.

           MOVE "S  A300110." TO VDU.
           MOVE WK-HHGS-1 TO INSPECT-X.
           MOVE 1 TO IX1 IX2.
       RESTORE-FREE-1A.
           MOVE INSPECT-Z(IX1) TO VDUTAB(IX2).
           IF IX1 < 30
              ADD 1 TO IX1 IX2
              GO TO RESTORE-FREE-1A.
           PERFORM SCREENIO.
           MOVE "S  A330111." TO VDU.
           MOVE 31 TO IX1.
           MOVE 1 TO IX2.
       RESTORE-FREE-1B.
           MOVE INSPECT-Z(IX1) TO VDUTAB(IX2).
           IF IX1 < 63
              ADD 1 TO IX1 IX2
              GO TO RESTORE-FREE-1B.
           PERFORM SCREENIO.

       RESTORE-TRAILER-EXIT.
           EXIT.

      ******************************************************************
       CREATE-TRAILER SECTION.
      ******************************************************************
           MOVE 6 TO WK-KEY.
           PERFORM OPEN-WK-FILE.
           PERFORM READ-WK-FILE.
           MOVE " " TO WK-SAVE-RECORD.
           IF IO-FG = 0
              PERFORM REWRITE-WK-FILE
           ELSE
              MOVE 0 TO WK-ELE
              PERFORM WRITE-WK-FILE
              IF IO-FG = 9
                 GO TO IO-ERROR.

           PERFORM CLOSE-WK-FILE.
           MOVE " " TO CHFG(11).
           MOVE " " TO F3-COLLATERAL.

      ******************************************************************
       TEST-ALLOT-LBOX SECTION.
      ******************************************************************
           PERFORM ACCOUNT-LBOX.
           IF IO-TYPE = "RTN"
              GO TO TEST-ALLOT-LBOX-EXIT.
           IF PD-LBOX NOT = "Y"
              PERFORM ACCOUNT-ALLOTMENTS.

       TEST-ALLOT-LBOX-EXIT.
           EXIT.

      ******************************************************************
       ACCOUNT-ALLOTMENTS SECTION.
      ******************************************************************
       ACCT-ALLOT.
           MOVE SPACES TO PD-ALLOTCD IO-TYPE.
           IF BR-ALLOTMNTS NOT = "Y"
              GO TO ACCOUNT-ALLOT-EXIT.

           MOVE "^-BACK, IS ACCOUNT UNDER AN ALLOTMENT PLAN ?" TO MESS.
           PERFORM SEND-LEGEND.

           MOVE "E  A060SEL." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              MOVE "RTN" TO IO-TYPE
              GO TO ACCOUNT-ALLOT-EXIT.
           IF VDUSTATUS NOT = "00"
              GO TO ACCT-ALLOT.
           MOVE VDUBUF TO ACTION.
           IF ACTION = "     Y"
              MOVE "Y" TO PD-ALLOTCD
           ELSE
              IF ACTION NOT = "     N"
                 GO TO ACCT-ALLOT.
           MOVE SPACES TO MESS.
           PERFORM SEND-LEGEND.
       ACCOUNT-ALLOT-EXIT.
           EXIT.

      ******************************************************************
       ACCOUNT-LBOX SECTION.
      ******************************************************************
       ACCT-LBOX.
           MOVE SPACES TO PD-LBOX IO-TYPE.
           IF BR-LBOX NOT = "Y"
              GO TO ACCOUNT-LBOX-EXIT.
           MOVE "^-BACK, IS ACCOUNT UNDER A LOCK BOX PLAN ?" TO MESS.
           PERFORM SEND-LEGEND.
           MOVE "E  A060SEL." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              MOVE "RTN" TO IO-TYPE
              GO TO ACCOUNT-LBOX-EXIT.
           IF VDUSTATUS NOT = "00"
              GO TO ACCT-LBOX.
           MOVE VDUBUF TO ACTION.
           IF ACTION = "     Y"
              MOVE "Y" TO PD-LBOX
           ELSE
              IF ACTION NOT = "     N"
                 GO TO ACCT-LBOX.
           MOVE SPACES TO MESS.
           PERFORM SEND-LEGEND.
       ACCOUNT-LBOX-EXIT.
           EXIT.

      ******************************************************************
       WRITE-SCRATCH SECTION.
      ******************************************************************
           MOVE "LOAN TEMP SAVE IN PROGRESS.........." TO MESS.
           PERFORM SEND-LEGEND.

           PERFORM OPEN-PD1-FILE.
           MOVE PD-RECORD TO WK-SAVE-RECORD.
           MOVE PD-SSNO(1) TO SAVE-SSNO.
           PERFORM SET-SCRATCH-KEY.
           PERFORM READ-PD1-FILE.
           MOVE WK-SAVE-RECORD TO PD-RECORD.
           IF IO-FG = 9
              GO TO CREATE-NEW-SCRATCH.
           PERFORM REWRITE-PD1-FILE.
           GO TO EXIT-WRITE-SCRATCH.
       CREATE-NEW-SCRATCH.
           PERFORM SET-SCRATCH-KEY.
           PERFORM WRITE-PD1-FILE.
           IF IO-FG = 9
              MOVE "RECALL # USED BY ANOTHER USER !" TO MESS
              PERFORM SEND-MESS
              MOVE "PLEASE REENTER RECALL # AND REINPUT" TO MESS
              PERFORM SEND-MESS.
       EXIT-WRITE-SCRATCH.
           IF F3-YES
              PERFORM CREATE-LT-SCRATCH.

           PERFORM CLOSE-PD1-FILE.

      ******************************************************************
       DELETE-SCRATCH SECTION.
      ******************************************************************
           MOVE "LOAN TEMP DELETE IN PROGRESS........" TO MESS.
           PERFORM SEND-LEGEND.
           PERFORM OPEN-PD1-FILE.
           MOVE PD-SSNO(1) TO SAVE-SSNO.
           PERFORM SET-SCRATCH-KEY.
           PERFORM READ-PD1-FILE.
           IF IO-FG = 0
              PERFORM DELETE-PD1-FILE.
           PERFORM CLOSE-PD1-FILE.

           MOVE PD-SSNO(1) TO PC-SOC-SEC.
           MOVE BRANCH TO PC-BRANCH
                          PC2-BRANCH.
           MOVE "C" TO PC-ID.
           PERFORM OPEN-PC1-FILE.
           PERFORM READ-PC1-FILE.
           IF IO-FG = 0
              PERFORM DELETE-PC1-FILE.
           PERFORM CLOSE-PC1-FILE.

      ******************************************************************
       CREATE-LT-SCRATCH SECTION.
      ******************************************************************
           MOVE 6 TO WK-KEY.
           PERFORM OPEN-WK-FILE.
           PERFORM READ-WK-FILE.
           PERFORM CLOSE-WK-FILE.
           IF IO-FG NOT = 0
              GO TO CREATE-LT-SCRATCH-EXIT.

           PERFORM OPEN-PC1-FILE.

           PERFORM CLEAR-PC-FILE.

           MOVE "C"          TO PC-ID.
           MOVE PD-SSNO(1)   TO PC-SOC-SEC.
           MOVE BRANCH       TO PC-BRANCH
                                PC2-BRANCH.

           MOVE WK-HHGS-1    TO PC-LTC-HHGS(1).
           MOVE WK-HHGS-2    TO PC-LTC-HHGS(2).
           MOVE WK-HHGS-3    TO PC-LTC-HHGS(3).
           MOVE WK-AUTO-1    TO PC-LTC-AUTO(1).
           MOVE WK-AUTO-2    TO PC-LTC-AUTO(2).
           MOVE WK-AUTO-3    TO PC-LTC-AUTO(3).

           MOVE PD-LOANDATE  TO PC-LOANDATE.
           MOVE PD-ORGTERM   TO PC-ORGTERM.
           MOVE PD-REGPYAMT  TO PC-REGPYAMT.

           PERFORM WRITE-PC1-FILE.
           IF IO-FG NOT = 0
              PERFORM REWRITE-PC1-FILE.

       CREATE-LT-SCRATCH-EXIT.
           PERFORM CLOSE-PC1-FILE.

      ***********************************
       VALID-INS-TYPES-ELIG SECTION.
      ***********************************
           MOVE 0 TO IO-FG.
           IF PD-INSURED = "S" OR "J"
              IF PD-SSNO(2) = 0
                 MOVE 9 TO IO-FG
                 GO TO EXIT-VALID-INS-TYPES-ELIG.

      * READ CURRENT STATE PARAMETER RECORD INORDER
      * TO CHECK INSURANCE COMPANIES (SUBSCRIPTED) = 0.
           PERFORM GET-SP.

      * CHECK IF ADDITIONAL INSURANCES WERE PURCHASED
           IF BR-OTH-INS-LEVELS NOT = "Y"
              MOVE "N" TO CHFG(30) CHFG(31) CHFG(32).

           IF PD-INSURED = "N"
              IF PD-INSTYPES NOT = "N"
                 MOVE 9 TO IO-FG
                 GO TO EXIT-VALID-INS-TYPES-ELIG
              ELSE
                 MOVE "N" TO CHFG(27) CHFG(28) CHFG(29) CHFG(30)
                 GO TO EXIT-VALID-INS-TYPES-ELIG.

           MOVE "N" TO CHFG(27) CHFG(28) CHFG(29) CHFG(30).

           IF PD-INSTYPES-LL
              MOVE " " TO CHFG(27)
              PERFORM LEVEL-INSURANCE.
           IF PD-INSTYPES-DL
              MOVE " " TO CHFG(27)
              PERFORM DECREASING-INSURANCE.
           IF PD-INSTYPES-AH
              MOVE " " TO CHFG(28)
              PERFORM ACCIDENT-INSURANCE.
           IF PD-INSTYPES-PP OR PD-INSTYPES-PP2
              MOVE " " TO CHFG(29)
              PERFORM PROPERTY-INSURANCE.
           IF PD-INSTYPES-O1
              MOVE " " TO CHFG(30)
              PERFORM O1-INSURANCE.
       EXIT-VALID-INS-TYPES-ELIG.
           EXIT.

      *********************************************
       LEVEL-INSURANCE SECTION.
      *********************************************
           IF PD-INSURED = "J"
              IF SP-INSCOM(4) = 0
                 MOVE "N" TO CHFG(27)
                 MOVE 9 TO IO-FG
                 MOVE "JOINT LEVEL LIFE NOT ALLOWED" TO MESS
                 PERFORM SEND-MESS
              ELSE
                 IF SP-CL-AH-MAXIMUMS(4) OR SP-ORGTERM-MAXIMUM(4)
                    IF PD-ORGTERM > SP-INSMAXTERM(4)
                       MOVE "N" TO CHFG(27)
                       MOVE 9 TO IO-FG
                       MOVE "JOINT LEVEL LIFE EXCEEDS MAX TERM" TO MESS
                       PERFORM SEND-MESS
                    END-IF
                 END-IF
                 MOVE "S  N020270." TO VDU
                 MOVE SP-INSCOM(4) TO VDUNUM0 PD-INSCOMP(1)
                 PERFORM SCREENIO
                 MOVE SP-INSFINCHG(4) TO PD-INSFINCHG(1)
           ELSE
              IF SP-INSCOM(3) = 0
                 MOVE "N" TO CHFG(27)
                 MOVE 9 TO IO-FG
                 MOVE "LEVEL LIFE NOT ALLOWED" TO MESS
                 PERFORM SEND-MESS
              ELSE
                 IF SP-CL-AH-MAXIMUMS(3) OR SP-ORGTERM-MAXIMUM(3)
                    IF PD-ORGTERM > SP-INSMAXTERM(3)
                       MOVE "N" TO CHFG(27)
                       MOVE 9 TO IO-FG
                       MOVE "LEVEL LIFE EXCEEDS MAX TERM" TO MESS
                       PERFORM SEND-MESS
                    END-IF
                 END-IF
                 MOVE "S  N020270." TO VDU
                 MOVE SP-INSCOM(3) TO VDUNUM0 PD-INSCOMP(1)
                 PERFORM SCREENIO
                 MOVE SP-INSFINCHG(3) TO PD-INSFINCHG(1).

           IF BR-AH-AND-CL = "Y"
              IF PD-INSTYPES-AH
                 IF NOT (PD-INSTYPES-DL OR PD-INSTYPES-LL)
                    MOVE "N" TO CHFG(27) CHFG(28)
                    MOVE 9 TO IO-FG
                    MOVE "MUST HAVE LIFE AND A&H" TO MESS
                    PERFORM SEND-MESS.

      *********************************************
       DECREASING-INSURANCE SECTION.
      *********************************************
           IF PD-INSURED = "J"
              IF SP-INSCOM(2) = 0
                 MOVE "N" TO CHFG(27)
                 MOVE 9 TO IO-FG
                 MOVE "JOINT DECLINING LIFE NOT ALLOWED" TO MESS
                 PERFORM SEND-MESS
              ELSE
                 IF SP-CL-AH-MAXIMUMS(2) OR SP-ORGTERM-MAXIMUM(2)
                    IF PD-ORGTERM > SP-INSMAXTERM(2)
                       MOVE "N" TO CHFG(27)
                       MOVE 9 TO IO-FG
                       MOVE "JOINT DECLINING LIFE EXCEEDS MAX TERM"
                             TO MESS
                       PERFORM SEND-MESS
                    END-IF
                 END-IF
                 MOVE "S  N020270." TO VDU
                 MOVE SP-INSCOM(2) TO VDUNUM0 PD-INSCOMP(1)
                 PERFORM SCREENIO
                 MOVE SP-INSFINCHG(2) TO PD-INSFINCHG(1)
           ELSE
              IF SP-INSCOM(1) = 0
                 MOVE "N" TO CHFG(27)
                 MOVE 9 TO IO-FG
                 MOVE "DECLINING LIFE NOT ALLOWED" TO MESS
                 PERFORM SEND-MESS
              ELSE
                 IF SP-CL-AH-MAXIMUMS(1) OR SP-ORGTERM-MAXIMUM(1)
                    IF PD-ORGTERM > SP-INSMAXTERM(1)
                       MOVE "N" TO CHFG(27)
                       MOVE 9 TO IO-FG
                       MOVE "DECLINING LIFE EXCEEDS MAX TERM" TO MESS
                       PERFORM SEND-MESS
                    END-IF
                 END-IF
                 MOVE "S  N020270." TO VDU
                 MOVE SP-INSCOM(1) TO VDUNUM0 PD-INSCOMP(1)
                 PERFORM SCREENIO
                 MOVE SP-INSFINCHG(1) TO PD-INSFINCHG(1).
           IF BR-AH-AND-CL = "Y"
              IF PD-INSTYPES-AH
                 IF NOT(PD-INSTYPES-DL OR PD-INSTYPES-LL)
                    MOVE "N" TO CHFG(27) CHFG(28)
                    MOVE 9 TO IO-FG
                    MOVE "MUST WRITE LIFE TO WRITE A&H" TO MESS
                    PERFORM SEND-MESS.

      *********************************************
       ACCIDENT-INSURANCE SECTION.
      *********************************************
           IF SP-INSCOM(5) = 0
              MOVE "N" TO CHFG(28)
              MOVE 9 TO IO-FG
              MOVE "A & H NOT ALLOWED" TO MESS
              PERFORM SEND-MESS
           ELSE
              IF SP-CL-AH-MAXIMUMS(5) OR SP-ORGTERM-MAXIMUM(5)
                 IF PD-ORGTERM > SP-INSMAXTERM(5)
                    MOVE "N" TO CHFG(28)
                    MOVE 9 TO IO-FG
                    MOVE "A & H EXCEEDS MAX TERM" TO MESS
                    PERFORM SEND-MESS
                 END-IF
              END-IF
              MOVE "S  N020280." TO VDU
              MOVE SP-INSCOM(5) TO VDUNUM0 PD-INSCOMP(2)
              PERFORM SCREENIO
              MOVE SP-INSFINCHG(5) TO PD-INSFINCHG(2).
           IF BR-AH-AND-CL = "Y"
              IF PD-INSTYPES-AH
                 IF NOT(PD-INSTYPES-DL OR PD-INSTYPES-LL)
                    MOVE "N" TO CHFG(27) CHFG(28)
                    MOVE 9 TO IO-FG
                    MOVE "MUST WRITE LIFE TO WRITE A&H" TO MESS
                    PERFORM SEND-MESS.

      *********************************************
       PROPERTY-INSURANCE SECTION.
      *********************************************
           IF PD-INSTYPES-PP
              IF SP-INSCOM(6) = 0
                 MOVE "N" TO CHFG(29)
                 MOVE 9 TO IO-FG
                 MOVE "PROPERTY NOT ALLOWED" TO MESS
                 PERFORM SEND-MESS
              ELSE
                 MOVE "S  N020290." TO VDU
                 MOVE SP-INSCOM(6) TO VDUNUM0 PD-INSCOMP(3)
                 PERFORM SCREENIO
                 MOVE SP-INSFINCHG(6) TO PD-INSFINCHG(3).

           IF PD-INSTYPES-PP2
              IF SP-INSCOM(7) = 0
                 MOVE "N" TO CHFG(29)
                 MOVE 9 TO IO-FG
                 MOVE "PROPERTY NOT ALLOWED" TO MESS
                 PERFORM SEND-MESS
              ELSE
                 MOVE "S  N020290." TO VDU
                 MOVE SP-INSCOM(7) TO VDUNUM0 PD-INSCOMP(3)
                 PERFORM SCREENIO
                 MOVE SP-INSFINCHG(7) TO PD-INSFINCHG(3).

      *********************************************
       O1-INSURANCE SECTION.
      *********************************************
           IF SP-INSCOM(8) = 0
              MOVE "N" TO CHFG(30)
              MOVE 9 TO IO-FG
              MOVE "O1 INS. NOT ALLOWED" TO MESS
              PERFORM SEND-MESS
           ELSE
              MOVE "S  N020300." TO VDU
              MOVE SP-INSCOM(8) TO VDUNUM0 PD-INSCOMP(4)
              PERFORM SCREENIO
              MOVE SP-INSFINCHG(8) TO PD-INSFINCHG(4).

      *********************************************
       AUTOPC-ROUTINE SECTION.
      *********************************************

           MOVE "A"                        TO AUTOPC-PC-ID.
           MOVE BRANCH                     TO AUTOPC-PC-BRANCH.
           MOVE PD-SSNO(1)                 TO AUTOPC-PC-SOC-SEC.
           MOVE "WI/AUTOPC" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH AUTOPC-BUF EXIT-PATHNAME.
           CANCEL FORM-PROGX.


       AUTOPC-ROUTINE-EXIT.
           EXIT.

      *********************************************
       COLLATERAL-CREATE SECTION.
      *********************************************
           MOVE TEMP-PATH TO LTCREA-PATH.
           MOVE "WI/LTCREA" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH LTCREA-BUF EXIT-PATHNAME.
           CANCEL FORM-PROGX.
           MOVE "Y" TO F3-COLLATERAL.

       EXIT-COLLATERAL-CREATE.
           EXIT.

      *********************************************
       VERIFY-PASSWORD SECTION.
      *********************************************
           MOVE " " TO PASSWDW-STAT.
           MOVE "LONPC0" TO PASSWDW-PGM.
           MOVE "PLEASE ENTER THE PASSWORD," TO PASSWDW-PROMPT1.
           MOVE "CL/PASSWD" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH PASSWDW-BUF.
           CANCEL FORM-PROGX.

      ******************************************************************
       DUAL-CHECK SECTION.
      ******************************************************************

           ADD 1 TO SUB.

           IF ( PD-SSNO(SUB)     = ZEROES ) OR
              ( BISCAN-CALL-CODE = 9      )
              GO TO EXIT-DUAL-CHECK.

           MOVE PD-SSNO(SUB) TO BISCAN-SSNO.
           MOVE PD-SRCD      TO BISCAN-SRCD.
           MOVE PD-PRLNNO    TO BISCAN-PRLNNO.

      * VALUE "C" WILL CALL-SCAN BUT CALLS BICLAS, CHECKING FOR DUAL IN CLASS
           IF ( SP-DUAL-IN-STATE  NOT = "Y" ) AND
              ( SP-DUAL-IN-BRANCH NOT = "Y" )
              MOVE 2 TO BISCAN-CALL-CODE
              PERFORM CALL-SCAN.

           IF ( BISCAN-CALL-CODE = 9 )
              MOVE "RTN" TO IO-FG.

       EXIT-DUAL-CHECK.
           EXIT.

      ******************************************************************
       CALL-SCAN SECTION.
      ******************************************************************

      * NO DUAL LOANS WITHIN THE SAME CLASS (TAX ADVANCE)
           IF (SP-DUAL-IN-STATE = "C") OR (SP-DUAL-IN-BRANCH = "C")
              MOVE 0                 TO BISCAN-CALL-CODE
      * USE BISCAN-SSNO BECAUSE IT GETS SET PROPERLY THROUGHOUT THE PROGRAM
              MOVE BISCAN-SSNO       TO BICLAS-SSNO
              MOVE PD-OWNBR          TO BICLAS-BRANCH
              MOVE PD-CLASS          TO BICLAS-CLASS
              MOVE "CL/BICLAS"       TO FORM-NAM
              CALL FORM-PROGX USING FORM-PATH BICLAS-WORKERS
              CANCEL FORM-PROGX
              IF BICLAS-RESTRICTED = "Y"
                 MOVE SPACES TO MESS
                 STRING "SAME CLASS LOAN STILL OPEN IN BRANCH; ",
                        BICLAS-BRANCH INTO MESS
                 PERFORM SEND-MESS
                 MOVE 9 TO BISCAN-CALL-CODE
                 GO TO EXIT-CALL-SCAN
              ELSE
                 GO TO EXIT-CALL-SCAN.

           MOVE SP-DUAL-IN-STATE  TO BISCAN-STATE-FG.
           MOVE SP-DUAL-IN-BRANCH TO BISCAN-BRANCH-FG.
           MOVE PD-OWNBR          TO BISCAN-BRANCH.
           MOVE PD-ORGST          TO BISCAN-STATE.
           MOVE TRANS-DATE        TO BISCAN-TRANS-DATE.
           MOVE "CL/BISCAN"       TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH BISCAN-WORKERS.
           CANCEL FORM-PROGX.

       EXIT-CALL-SCAN.
           EXIT.

      ******************************************************************

       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/GPENV.CPY".
       COPY "LIBLP/LPANTI.CPY".
       COPY "LIBLP/LPMENU.CPY".
       COPY "LIBLP/LPAPRZL.CPY".
       COPY "LIBLP/LPDAMTS.CPY".
       COPY "LIBLP/LPPDCL.CPY".
       COPY "LIBLP/LPPCCL.CPY".
       COPY "LIBLP/LPEDAYP.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/GRWORK.CPY".
       COPY "LIBGB/GRSERCH.CPY".
       COPY "LIBWI/PFSWINDOWS.CPY".
       COPY "LIBWI/LNAPPROVAL.CPY".

      ******************************************************
       FORM-RESET SECTION.
      ******************************************************
           MOVE LPLOAN-SYSDATE TO LONPC-SYSDATE.
      * NEED TO CLEAR SCREEN ON F1
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DISPLAY LONPC-SCREEN UPON TCLP-MAIN-WINDOW-HANDLE.
           MOVE LONPC-FORM-FIELDS TO WR-BUF.
           MOVE LONPC-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      ******************************************************
       SETUP-LINK-INFO SECTION.
      ******************************************************
           IF VDU-FORM-NAME = "LPSMNU"
              MOVE LPSMNU-HELP--FILE TO HELP-LINK-FILE
              MOVE LPSMNU-HELP--DIR  TO HELP-LINK-DIR
           ELSE
              MOVE LONPC-HELP--FILE TO HELP-LINK-FILE
              MOVE LONPC-HELP--DIR TO HELP-LINK-DIR.

      ******************************************************
       VDU-CONVERT-FIELD SECTION.
      ******************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBLP/LONPC_EVA.CPY".
              COPY "LIBLP/LPSMNU_EVA.CPY".
              COPY "LIBLP/LPLNMU_EVA.CPY".
              COPY "LIBLP/LPCAMU_EVA.CPY".
              COPY "LIBLP/LPVDMU_EVA.CPY".
              COPY "LIBWI/GLIMI2_EVA.CPY".
              COPY "LIBWI/GLIMI3_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ******************************************************************
       CHECK-FOR-PASSWORD SECTION.
      ******************************************************************

      * CHECKING TO SEE IF LOGIN ID IS A GROUP MEMBER

           MOVE CASE-BUF TO USER-GROUP WGR-WORD.
           PERFORM CHECK-MEMBER.
           IF ( WGR-ERR > 0 )
              GO TO PASSWORD-REQUIRED-BR.

      * CHECKING TO SEE IF BRANCH ENTERED IS WITHIN THE MEMBER.

           PERFORM OPEN-GR1-FILE.
           MOVE USER-GROUP TO GR-ID.
           PERFORM SEARCH-GR-INIT.
           MOVE BRANCH TO WGR-SRESULT.
           PERFORM SEARCH-GR-VERIFY.
           IF ( WGR-ERR > 0 )
              GO TO PASSWORD-REQUIRED-BR.

      * CHECK FOR SEQUENCE 2 PASSWORD - MERCURY HAS SECRETARY LOGINS
      * SUCH AS P0158VP1 WHO NEED TO RUN REPORTS FOR VP'S BUT SHOULDN'T
      * BE ALLOWED TO APPROVE; PASSWD = LONPB0 SEQ 2

           MOVE APPROVAL-SEQ2 TO PASSWDW-SEQ.
           PERFORM VERIFY-PASSWORD.
           IF ( PASSWDW-STAT NOT = "OK" )
              MOVE MESSAGE-NEED-PASSWORD TO MESS
              PERFORM SEND-MESS.

           GO TO EXIT-CHECK-FOR-PASSWORD.

      * LOGIN ID OR BRANCH ENTERED IS NOT VALID,
      * MUST ENTER PASSWORD TO MAKE CHANGE TO LIMIT.

       PASSWORD-REQUIRED-BR.
           MOVE APPROVAL-SEQ TO PASSWDW-SEQ.
           PERFORM VERIFY-PASSWORD.
           IF ( PASSWDW-STAT NOT = "OK" )
              MOVE MESSAGE-NEED-PASSWORD TO MESS
              PERFORM SEND-MESS.

       EXIT-CHECK-FOR-PASSWORD.
           EXIT.

      ******************************************************************
       MISC-PARAGRAPHS SECTION.
      ******************************************************************
       COPY "LIBGB/SYSTEM.CPY".
       RECALL-COLLATERAL.
           MOVE 6 TO WK-KEY.
           PERFORM OPEN-WK-FILE.
           PERFORM READ-WK-FILE.
           MOVE PC-LTC-HHGS(1) TO WK-HHGS-1.
           MOVE PC-LTC-HHGS(2) TO WK-HHGS-2.
           MOVE PC-LTC-HHGS(3) TO WK-HHGS-3.
           MOVE PC-LTC-AUTO(1) TO WK-AUTO-1.
           MOVE PC-LTC-AUTO(2) TO WK-AUTO-2.
           MOVE PC-LTC-AUTO(3) TO WK-AUTO-3.
           PERFORM REWRITE-WK-FILE.

       TEST-GET-SP.
           MOVE 1 TO FIRST-TIME.
           IF FIRST-ELE > 5
              PERFORM GET-SP.
       GET-GI.
           PERFORM OPEN-GI1-FILE.
           PERFORM READ-GI1-FILE.
           PERFORM CLOSE-GI1-FILE.
       GET-EARN-ORDER.
           PERFORM GET-SP.
           MOVE SP-EARN-ORDER(1) TO PD-EARN-ORDER(1).
           MOVE SP-EARN-ORDER(2) TO PD-EARN-ORDER(2).
           MOVE SP-EARN-ORDER(3) TO PD-EARN-ORDER(3).
           MOVE SP-EARN-ORDER(4) TO PD-EARN-ORDER(4).
           MOVE SP-EARN-ORDER(5) TO PD-EARN-ORDER(5).
           MOVE SP-EARN-ORDER(6) TO PD-EARN-ORDER(6).
       SAVE-TRAILER-REC.
           MOVE 6 TO WK-KEY.
           PERFORM OPEN-WK-FILE.
           PERFORM REWRITE-WK-FILE.
           PERFORM CLOSE-WK-FILE.
       GET-CODE.
           MOVE "GC"     TO CDB-TYPE.
           MOVE PD-OWNBR TO CDB-BRANCH.
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
           PERFORM OPEN-CD1-FILE.
           PERFORM READ-CD1-FILE.
           PERFORM CLOSE-CD1-FILE.
           IF IO-FG = 9
              MOVE " INVALID CODE" TO CD-DESC.
       GET-DL1.
           PERFORM OPEN-DL1-FILE.
           PERFORM READ-DL1-FILE.
           PERFORM CLOSE-DL1-FILE.
           IF IO-FG = 9
              MOVE " INVALID DEALER" TO DL-SHORT.

      *-----------------------------------------------------------------
       GET-BW1.
           PERFORM OPEN-BW1-FILE.
           PERFORM READ-BW1-FILE.
           IF IO-FG = 9
              MOVE " " TO BW-FNAME
              MOVE "NOT FOUND" TO BW-LNAME
              MOVE "Y" TO BW-ERROR-YN.    

           PERFORM APPEND-BW-NAMES.
           PERFORM CLOSE-BW1-FILE.

      *-----------------------------------------------------------------
       GET-PD1.
           PERFORM OPEN-PD1-FILE.
           PERFORM READ-PD1-FILE.
           PERFORM CLOSE-PD1-FILE.
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
              MOVE "STATE PARA DELETED" TO MESS
              PERFORM SEND-MESS
              GO TO IO-ERROR.
           PERFORM CLOSE-SP1-FILE.
       VERIFY-INSURANCE.
           IF PD-INSCOMP(SUB) NOT = 0 OR PD-INSPREM(SUB) NOT = 0
              MOVE 9 TO IO-FG.
       SEND-CD-DESC.
           PERFORM GET-CD.
           MOVE "S  A000  1." TO VDU.
           MOVE ELE TO VDUROW.
           MOVE CD-DESC TO VDUBUF.
           PERFORM SCREENIO.
       APPEND-BW-NAMES.
           MOVE BW-LNAME TO INSPECT-W.
           MOVE BW-FNAME TO ADD-ON.
           PERFORM ADD-IT.

      *-----------------------------------------------------------------
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
       SEND-BW-MAKERCD.
           MOVE "S  A010  2." TO VDU.
           MOVE ELE TO VDUROW.
           MOVE PD-MAKERCD(ELE) TO VDUBUF.
           PERFORM SCREENIO.

      *-----------------------------------------------------------------
       SEND-MEMO.
           MOVE "S  A060M1." TO VDU.
           MOVE "MEMO: " TO VDUBUF.
           PERFORM SCREENIO.
           MOVE "S  A300M2." TO VDU.
           MOVE PD-MESSAGE TO VDUBUF.
           PERFORM SCREENIO.
       NOT-FOUND.
           MOVE "NOT FOUND" TO MESS.
           PERFORM SEND-MESS.
       SP-INVALID.
           MOVE "ENTRY INCONSISTENT WITH STATE PARAMETERS" TO MESS.
           PERFORM SEND-MESS.
       NOT-DIRECT.
           MOVE "INVALID FOR DIRECT LOAN ENTRY" TO MESS.
           PERFORM SEND-MESS.
       MSG-LINE.
           MOVE MESS TO TMMSG-MESS.
           PERFORM CALL-TMMSG.
       SEND-LEGEND.
           MOVE "S  A400MSEL." TO VDU.
           MOVE MESS TO VDUBUF.
           PERFORM SCREENIO.
       RESET-LEGEND.
           MOVE " " TO MESS.
           PERFORM SEND-LEGEND.
       LOAN-CLASS-PROMPT.
           MOVE "S  A110KEYTYPE." TO VDU.
           MOVE "LOAN CLASS:" TO VDUBUF.
           PERFORM SCREENIO.
       SET-SCRATCH-KEY.
           MOVE SAVE-SSNO TO PD-SOC-SEC.
           MOVE BRANCH TO PD-BRANCH.
           MOVE "L" TO PD-ID.
       PROTECT-UNPROTECT.
           MOVE STAT TO CHFG(5) CHFG(12) CHFG(15) CHFG(16)
              CHFG(18) CHFG(19) CHFG(20) CHFG(37).

      ******************************************************************
       IO-ROUTINE SECTION.
      ******************************************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBLP/LPSPGS_SQL.CPY".
       COPY "LIBLP/LPPDGS_SQL.CPY".
       COPY "LIBLP/LPPCGS_SQL.CPY".
       COPY "LIBLP/LPLCGS_SQL.CPY".
       COPY "LIBLP/LPICGS_SQL.CPY".
       COPY "LIBLP/LPDLGS_SQL.CPY".
       COPY "LIBLP/LPCDBGS_SQL.CPY".
       COPY "LIBLP/LPCDGS_SQL.CPY".
       COPY "LIBLP/LPBWGS_SQL.CPY".
       COPY "LIBGL/GLGIGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBGRGS_SQL.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBGB/GBGR1RN.CPY".
       COPY "LIBLP/LPDL1RN.CPY".
       COPY "LIBLP/LPLC1RN.CPY".
       COPY "LIBLP/LPPD1IN.CPY".
       COPY "LIBLP/LPPC1IN.CPY".
       COPY "LIBLP/LPSP1RN.CPY".
       COPY "LIBLP/LPBW1RN.CPY".
       COPY "LIBLP/LPBW2RN.CPY".
       COPY "LIBLP/LPCD1RN.CPY".
       COPY "LIBLP/LPCDB1RN.CPY".
       COPY "LIBLP/LPIC1RN.CPY".
       COPY "LIBLP/LPWKI.CPY".
       COPY "LIBGL/GLGI1RN.CPY".
       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO ERRCD.

      ******************************************************************
       END-ROUTINE SECTION.
      ******************************************************************
           PERFORM CLOSE-FILES.
       EXIT-PROG.
       STOP-RUN.
           EXIT PROGRAM.
