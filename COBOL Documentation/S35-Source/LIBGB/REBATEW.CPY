      * COPYMEMBER: LIBGB/REBATEW

      **************************************************************************
      *    REBATE CALCULATION WORKERS
      *
      * REV:
      *  022388 JTG ADDED REB-DEDCHG FIELD
      *  042888 BAW ADDED REB-AHSUB, REB-HUNDREDS
      *             AND REB-MANYDECIMALS FOR FORMULA 8
      *  042988 JPN ADDED REB-LPTRCD, REB-HALF-ORGTERM
      *  062188 JTG INCREASED PERCISION ON MPRATE
      *  102188 JTG ADDED FORM-PATH FOR ALLOWING CALLS
      *             FROM WITHIN REBATE ROUTINE.
      *  121388 JTG ADDED WORKER FOR IOWA
      *  011789 JTG CHANGE ELAPSED-REM TO S9(5)V9(10)
      *  022889 JTG CHANGE CALL WORKERS TENNESSEE DEF REB
      *  041989 JTG ADDED WORKERS FOR FRMLA - F (IDEAL)
      *  051089 JTG CJANGED WORKERS TO COMP
      *  072789 JTG ADDED REB-INSSUB FOR IDEAL (MA)
      *  100489 JTG ADDED REB-SWING FOR IDEAL FORMULA F
      *  102489 JTG ADDED REB-STATEWK FOR (GA) FRMLA H
      *                   REB-STATEWK2
      *  021690 JTG ADDED REB-DEFSUB = 10
      *  042690 JTG ADDED REB-TRIGGER-DATE (SP-RBEARLY = 8) PACESETTER
      *  052990 JTG ADDED CALL WORKERS INDIANA DEF REB
      *  080990 JTG ADDED FIELDS FOR ADDON INS.
      *  121090 SLC ADDED REB-RECAST.
      *  012191 JTG ADDED REB-WORK2 RE: MULLEN ADDONS.
      *  052191 JTG ADDED REB-HOLD-ORGTERM (PACESETTER) SP-RBEARLY = 8
      *  123091 JTG ADDED REB-PRES-VALUE (EDDIE GOLDSTICKER) SP-ERNFRMLA = 11
      *  071392 JTG ADDED B01-FRMLA & B01-PAY-SCHLD-FG
      *                   FOR TENNEESSEE DEFERMENT REBATE
      *                   (COLONIAL, TN) ADDON/PAYMENT SCHEDULES
      *  082092 JTG ADDED REB-DONT-STOP & B01-DONT-STOP (COLONIAL, TN)
      *  082292 JTG ADDED REB-LN-PAYFREQ, REB-LN-APRATE, REB-LN-ORGTERM,
      *                   REB-LN-1STPYAMT, REB-LN-REGPYAMT
      *  090192 BAH CHANGED REB-DONT-STOP TO REB-DEF-STOP
      *             ONLY STOP IN INQUIRIES AND POST
      *  090292 JTG ADDED FIELDS FOR SP-RBFRMLA = 'T'
      *  012293 JTG ADDED REB-UPR-WHOLE FOR TIME FINANCE (CA)
      *                    TO BE USED WITH REFUND FORMULA 'L'
      *  090193 JTG MOVED REB-REMTERM TO AN OUTPUT FIELD
      *  021095 JTG ADDED FIELDS FOR SP-RBFRMLA = 'S' TIME LOAN, CA CL REFUND
      *  112495 JTG ADDED REBT02, FOR SP-RBEARLY = 13, MERCURY 'NV'
      *             PP REFUND ACTION 8
      *  010995 JTG ADDED REBF-ADJ, 360/350 ADJUSTMENT FACTOR RE: RBFRMLA = 'F'
      *  021497 BAH ADDED A01-PAYDATE FOR REBA01
      *  051997 JTG ADDED REB-SP1-KEY, RE: 'ADDONSPR'
      *  092697 JTG ADDED REB-LN-LASTPYAMT/REB-LN-SMPRATE FOR ACTUARIAL ADDON RB
      *  021098 JTG ADDED REB-EARN-FULL-MONTH TFC, MERCURY #637
      *  022699 BAH ADDED REB-HALF-ORGTERM-DEC FOR 9999.04 (WORLD, #201)
      *
      *  000523 KEC {PR#00000} MERCURY FINANCE [CINDY SHAFFER];
      *              ADDED 'B02-DEF-STOP' FOR THE PURPOSE OF NOT
      *              ALLOWING THE 'SEND-MESS' WITHIN CL/REBB02.C FROM
      *              HAPPENING SO THAT MERCURY CAN HAVE THEIR CUSTOMER
      *              STATEMENTS TO WORK (NOT STOPPING IN BATCH).
      *  JTG 020219 ADDED "CT" COUNTY TAX CODE KENTUCKY AND PERCENTS
      *            ADDED "CY" CITY   TAX CODE KENTUCKY AND PERCENTS
      *            ADDED INS-TAX                                  WORLD #288
      *  JTG 020402 CHANGED INS-TAX TO INS-TAX-RB                 WORLD #288
      *  JTG 020930 ADDED NEW SHORT RATE FORMULA SP-RBEARLY = 24 (PP)    TFC #96
      *  JTG 030305 ALLOW RBFRMLA '8' TO WORK FOR 'AH' & 'ALL COMPUTABLE INS'
      *                                                         PACESETTER #2012
      *  JTG 030710 NEW INTEREST REFUND FORMULA (0 9) STATE OF TEXAS BASED
      *             ON DAILY INTEREST USING 365 DAYS AND SIMPLE      EXCEL #2111
      *  JTG 040608 ADDED NEW SP-RBFRMLA (0 C) SHORT RATE FORMULA   MULLEN #2302
      *  JTG 040917 ADDED NEW FIELDS FOR SP-RBFRMLA (0 D)
      *             RULE 78THS USING DAYS IN TERM AND ELAPSED DAYS INSTEAD
      *             OF UNIT PERIODS IN TERM AND ELAPSE UNIT PERIODS
      *             ADDED 'REB-LN-INSEFF' & 'REB-LN-INSEXP'
      *             ALSO ADDED SP-RBSPOPT2 = 11 NEW ROUNDING OPTION TO
      *             TAKE COMPUTED REBATE AND ROUNDED UP OR DOWN TO
      *             WHOLE DOLLARS ADDED 'REB-REBATE-CENTS'          REGACC #0074
      *  JTG 041102 ADDED ALAMAMA REFUND FRMLA ACTUARIAL (0 E)      WORLD  #J050
      *   CS 050520 ADDED REBTX-STEP, REBTX-RATE-BAL, REBTX-RATE-AMT
      *             NEW SP-RBFRMLA2 = "H" [REGIONAL MGMT PR#2527]
      *  JTG 050708 ADDED REB-LN-INT-REBATE REB-LN-SC-REBATE REB-LN-MF-REBATE
      *             REB-LN-DF-REBATE
      *             ADDED NEW FORMULAS FOR MEXICO
      *             SP-MFFRMLA-05 SP-RBFRMLA2 (G) SP-ERNFRMLA (24)   WORLD #0408
      *  CS  060203 ADDED FOR PEOPLES... NO ROUND TO WHOLE DOLLAR PEOPLE #2670
      *             REBX-DAILY-COST-NO-ROUND,REBX-EARNED-COST-NO-ROUND
      *             REBX-PRO-RATA-REFUND-NO-ROUND,REBX-SHORT-RATE-REFUND-NO-ROUN
      *  JTG 071030 NEW INSURANCE REFUND FORMULA (0 I) FOR CPI POLICYS
      *             WHICH ARE ALWAYS 1 (ONE) YEAR TERMS          CITIZENS #3097
      *  BAH 150914 ADDED REB-SP-CONTRFRMLA TO HOLD ORIGINAL SPR VALUE, ADDONS
      *             READ THEIR OWN SPR THEN READ THE ORIGINAL AND THE
      *             SP-CONTRFRMLA COULD HAVE BEEN CHANGED IN CRNO, AMACC PL#901
      *  MJD 160122 ADDED REB-EP54-AMT. THIS IS THE AMOUNT OF THE SERVICE CHARGE
      *             THAT IS THE GEORGIA 4% CLOSING FEE EARLY PROVISION CALCED 
      *             CALCULATED IN REBATE-EP54                       REGMGT #4915
      *  MJD 160511 ADDED REB-LT-ADDON-FG,,REB-TEST-LT-TYPE AND 
      *             REB-HOLD-LNORIG1STPAY,REB-HOLD-LT-REC             71105#0918
      *   CS 160909 CHANGED REB-SP1-KEY FROM PIC X(8) TO PIC X(9)
      *  BAH 161025 CHANGED ALL DATES TO 9(8)
      *
      * KEC 2017.0307 {PR#00000} PARADATA <A30> [JAY CISZEWSKI]
      *          CHANGED, WITHIN REB-B01, B01-FRMLA FROM 1 TO 2 BYTES
      *          SO THAT B01-FRMLA MATCHES SP-RBFRMLA.
      *
      * KEC 2017.0519 {PR#00000} PARADATA <A30> [JAY CISZEWSKI]
      *          CHANGED REB-HOLD-LTI-REC FROM BEING HARDCODED
      *          [PIC X(400)] TO USE LTI-SIZE.  AFTER CHECKING,
      *          ALL PROGRAMS THAT DO NOT USE LIBGB/REBATE (WHICH USES
      *          LTI-REC) DOES USE LIBLP/LPRATE (ALSO USES LTI-REC)
      *          AND USES THE LTI-FILE (FS/FD/WS) OR HAS THE LAYOUT
      *          (LP01LTI).
      *
      * BAH 2018.07.02 REMOVED REB-SP-FLAG, REB-SP1-KEY, REB-SP-CONTRFRMLA
      *                CREATE ADDONSPRW FOR THESE FIELDS, ONLY USED IN
      *                FIND-ADDON-SPR, WHICH IS A COPY MEMBER ADDONSPR
      * BAH 2024.0321 REMOVED REBF-ADJ, REBP1, REBN-1, REBPV, REBTX-CALC-INT-X,
      *               REBTX-NEW-INT,
      **************************************************************************
      * INPUT FIELDS:
       01  REB-PAYDATE            PIC 9(8)     COMP    VALUE 0.
       01  REB-ORGTERM            PIC S9(5)    COMP    VALUE 0.
       01  REB-SUB                PIC 99       COMP    VALUE 0.
       01  REB-TOTCHG             PIC S9(6)V99 COMP    VALUE 0.
       01  REB-LPTRCD             PIC XX               VALUE " ".
       01  REB-SKIPDEF-FG         PIC X                VALUE " ".
       01  REB-ADDON-FG           PIC X                VALUE " ".
       01  REB-RECAST             PIC X                VALUE " ".
       01  REB-DEF-STOP           PIC X                VALUE " ".
       01  REB-EARN-FULL-MONTH    PIC X                VALUE " ".

      * INPUT FIELDS FOR ADDON INSURANCE:
       01  REB-LN-PURDATE         PIC 9(8)     COMP    VALUE 0.
       01  REB-LN-LOANDATE        PIC 9(8)     COMP    VALUE 0.
       01  REB-LN-INTDATE         PIC 9(8)     COMP    VALUE 0.
       01  REB-LN-ORIG-1STPYDATE     PIC 9(8)     COMP    VALUE 0.
       01  REB-LN-TOTNODEF        PIC S9(3)    COMP    VALUE 0.
       01  REB-LN-INSCOVR         PIC 9(6)V99  COMP    VALUE 0.
       01  REB-LN-SALEFIN         PIC X                VALUE " ".
       01  REB-LN-INSEFF          PIC 9(8)     COMP    VALUE 0.
       01  REB-LN-INSEXP          PIC 9(8)     COMP    VALUE 0.

       01  REB-LN-APRATE          PIC 9(3)V9(3) COMP   VALUE 0.
       01  REB-LN-SMPRATE         PIC 9(3)V9(3) COMP   VALUE 0.
       01  REB-LN-ORGTERM         PIC 9(3)      COMP   VALUE 0.
       01  REB-LN-1STPYAMT        PIC 9(6)V99   COMP   VALUE 0.
       01  REB-LN-REGPYAMT        PIC 9(6)V99   COMP   VALUE 0.
       01  REB-LN-LASTPYAMT       PIC 9(6)V99   COMP   VALUE 0.
       01  REB-LN-INT-REBATE      PIC 9(6)V99   COMP   VALUE 0.
       01  REB-LN-SC-REBATE       PIC 9(6)V99   COMP   VALUE 0.
       01  REB-LN-MF-REBATE       PIC 9(6)V99   COMP   VALUE 0.
       01  REB-LN-DF-REBATE       PIC 9(6)V99   COMP   VALUE 0.

      * OUTPUT WORKERS:
       01  REB-REBATE             PIC S9(7)V99.
       01  FILLER                REDEFINES REB-REBATE.
           03  REB-REBATE-WHOLE   PIC S9(7).
           03  REB-REBATE-CENTS   PIC 99.
       01  REB-PRES-VALUE         PIC S9(7)V99    COMP.
       01  REB-REMTERM            PIC S9(5)       COMP.
       01  REB-MS-PENALTY         PIC S9(5)V99    COMP.
       01  INS-TAX-RB             PIC S9(7)V99    COMP.
       01  REB-EP54-AMT           PIC S9(7)V99    COMP.

      * INTERNAL WORKERS:
       01  REB-REFDATE            PIC 9(8)        COMP.
       01  REB-TRIGGER-DATE       PIC 9(8)        COMP.
       01  REB-HOLD-RBDTE         PIC X           VALUE " ".
       01  REB-DEDCHG             PIC S9(5)V99    COMP.
       01  REB-MPRATE             PIC 9(3)V9(10)  COMP.
       01  REB-FACTOR             PIC 9(3)V9(10)  COMP.
       01  REB-PRATE              PIC 9(3)V9(10)  COMP.
       01  REB-BAL                PIC S9(6)V9(9)  COMP.
       01  REB-INT                PIC 9(6)V99     COMP.
       01  REB-SUMORG             PIC S9(8)V9(8)  COMP.
       01  REB-SUMREM             PIC S9(8)V9(8)  COMP.
       01  REB-HOLD-ORGTERM       PIC S9(5)       COMP.
       01  REB-ELAPSED-MONTHS     PIC S9(5)       COMP.
       01  REB-ELAPSED-REM        PIC S9(5)V9(10) COMP.
       01  REB-ELAPSED-DAYS       PIC S9(5)       COMP.
       01  REB-SWING              PIC X           VALUE " ".
       01  REB-EARN-FOR-1MON      PIC S9(5)V99    COMP.
       01  REB-PRORATA-30DAY-PCT  PIC 9V999       COMP.
       01  REB-UNUSED-EXTDAYS     PIC S9(5)       COMP.
       01  REB-TOTAL-EXTDAYS      PIC S9(5)       COMP.
       01  REB-INTSUB             PIC 99          COMP VALUE 7.
       01  REB-DEFSUB             PIC 99          COMP VALUE 10.
       01  REB-AHSUB              PIC 9           COMP VALUE 5.
       01  REB-INSSUB             PIC 99          COMP VALUE 0.
       01  REB-SUB2               PIC 99          COMP.
           88  PODATE-B4-1STPYDATE                     VALUE 1.
       01  REB-HUNDREDS           PIC 9(5)V99     COMP.
       01  REB-MANYDECIMALS       PIC 9(5)V9(4)   COMP.
       01  REB-HALF-ORGTERM       PIC S9(5)       COMP.
       01  REB-HALF-ORGTERM-DEC   PIC S9(5)V99    COMP.
       01  REB-WORKER             PIC S9(7)V99    COMP.
       01  REB-WORK2              PIC S9(7)V99    COMP.
       01  REB-STATEWK            PIC S9(7)V99    COMP.
       01  REB-STATEWK2           PIC S9(7)V99    COMP.

       01  REBWK1                 PIC S999V9(15)  COMP.
       01  REBWK2                 PIC S999V9(15)  COMP.
       01  REBAN-M-1              PIC S999V9(15)  COMP.
       01  REBAN-M-1-T            PIC S999V9(15)  COMP.
       01  REBF                   PIC S9V9(15)    COMP.
       01  REBN                   PIC S999        COMP.
       01  REBM                   PIC S999        COMP.
       01  REBR                   PIC S999        COMP.
       01  REBN-M                 PIC S999        COMP.
       01  REBAN                  PIC S999V9(15)  COMP.
       01  REBAN-M                PIC S999V9(15)  COMP.
       01  REBAN-MPLUSR           PIC S999V9(15)  COMP.
       01  REBM-1                 PIC S999        COMP.
       01  REBM-T                 PIC S999        COMP.
       01  REBT                   PIC S999        COMP.
       01  REBP                   PIC S9(6)V99    COMP.
       01  REBI1                  PIC S999V9(15)  COMP.
       01  REBJ                   PIC S999V9(15)  COMP.
       01  REBJ1                  PIC S999V9(15)  COMP.
       01  REBI                   PIC S999V9(15)  COMP.
       01  REB-RCF1               REDEFINES REBI
                                  PIC S999V9(15)  COMP.
       01  REBADJ                 PIC S999V9(15)  COMP.
       01  REB-RCF                REDEFINES REBADJ
                                  PIC S999V9(15)  COMP.
       01  REBPW                  PIC S999V9(15)  COMP.
       01  REB-WFAC               REDEFINES REBPW
                                  PIC S999V9(15)  COMP.

       01  REB-UPR                PIC S9(7)V9999  COMP.
       01  REB-UPR-WHOLE          PIC S9(7)       COMP.

       01  REB-A--P-J             PIC S99999V9(13) COMP.
       01  REB-A--P-I             PIC S99999V9(13) COMP.
       01  REB-WK                 PIC S9(6)V9(11)  COMP.

       01  REBTX-CALC-INT-TOT     PIC S9(6)V99     COMP.
       01  REBTX-CALC-INT         PIC S9(6)V99     COMP.
       01  REBTX-AMTFIN           PIC S9(6)V99     COMP.
       01  REBTX-PYAMT            PIC S9(6)V99     COMP.
       01  REBTX-SERCHG-SLICE     PIC S9(6)V9(3)   COMP.
       01  REBTX-FROM-DATE        PIC 9(8)         COMP.
       01  REBTX-TO-DATE          PIC 9(8)         COMP.
       01  REBTX-RATE-BAL         PIC S9(6)V99     COMP.
       01  REBTX-RATE-AMT         PIC S9(6)V99     COMP.
       01  REBTX-STEP             PIC 9.     
      *----------------------------------------------------------
      * FIELDS USED IN 'LIBLP/ADDONSPR'
      *01  REB-SP-FLAG            PIC X           VALUE " ".
      *01  REB-SP1-KEY            PIC X(9).
      *01  REB-SP-CONTRFRMLA      PIC X           VALUE " ".
       
      * FIELDS USED FOR SP-RBDTE = "D"
       01  REB-LTI-ADDON-FG       PIC X.
       88  REB-LTI-ADDON-NOT-FOUND VALUE "N".
       01  REB-TEST-LTI-TYPE      PIC 99 COMP-3.
       01  REB-HOLD-LNORIG1STPAY  PIC 9(8) COMP-3.
       01  REB-HOLD-LTI-REC       PIC X(LTI-SIZE).

      ***********************************************************************
      *    A R E A  FOR PROGRAM WORKERS THAT GET REDEFINED TO GIVE
      *    NAMES THAT MAKE SENSE FOR THE ROUTINE.
      *
      *   'REBX' FIELDS ARE PREFIXED WITH REBX TO REFLECT THEY ARE REDEFINED
      ***********************************************************************
       01  REBX-REDEFINED-FIELDS.
           03  REBX-DAYS-IN-TERM         PIC S9(5)    COMP.
           03  REBX-DAYS-IN-FORCE        PIC S9(5)    COMP.
           03  REBX-DAYS-REMAINING       PIC S9(5)    COMP.
           03  REBX-EARNED-COST          PIC S9(7)    COMP.
           03  REBX-SHORT-RATE-REFUND    PIC S9(7)    COMP.

           03  REBX-INS-POLICY-FEE       PIC S9(7)V99 COMP.
           03  REBX-NET-CONTRACT-COST    PIC S9(7)V99 COMP.
           03  REBX-DAILY-COST           PIC S9(7)V99 COMP.
           03  REBX-PRO-RATA-REFUND      PIC S9(7)V99 COMP.
      *USED BY PEOPLE NO ROUND TO WHOLE DOLLARS
           03  REBX-DAILY-COST-NO-ROUND  PIC S9(7)V9(7) COMP.
           03  REBX-EARNED-COST-NO-ROUND PIC S9(7)V9(7) COMP.
           03  REBX-PRO-RATA-REFUND-NO-ROUND PIC S9(7)V9(7) COMP.
           03  REBX-SHORT-RATE-REFUND-NO-ROUND    PIC S9(7)V99.
      *    03  REBX-UNUSED
      *-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

      * FIELDS USED FOR CALLING SUBPROGRAMS:
       01  REB-FORM-PATH.
           03  FILLER                 PIC X(6)     VALUE "CL/REB".
           03  REB-FORM-PROG-TYPE     PIC X.
           03  REB-FORM-PROG-NO       PIC 99.
       01  REB-CANCEL-FG              PIC X        VALUE "Y".
       01  REB-COMMON.
           03  REB-COMMON-RTNCD       PIC X        VALUE SPACES.
           03  REB-COMMON-REBATE      PIC S9(7)V99 COMP-3 VALUE 0.
           03  REB-COMMON-AREA        PIC X(40)    VALUE SPACES.
           03  FILLER REDEFINES REB-COMMON-AREA.
               05  REB-COMMON-PAYDATE PIC 9(8).
               05  FILLER             PIC X(32).

      * USED FOR MISSISSIPPI REDUCTION CODE:
           03  REB-A01                REDEFINES REB-COMMON-AREA.
               05  A01-ELAPSED-MO     PIC S9(3)    COMP-3.
               05  A01-MS-PENALTY     PIC S9(5)V99 COMP-3.
               05  A01-PAYDATE        PIC 9(8)     COMP-3.
               05  FILLER             PIC X(29).
      * USED FOR TENNESSEE DEFERMENT REBATE FORMULA:
           03  REB-B01                REDEFINES REB-COMMON-AREA.
               05  B01-PAYDATE        PIC 9(8)     COMP-3.
               05  B01-FRMLA          PIC X(2).
               05  B01-PAY-SCHLD-FG   PIC X.
               05  B01-DEF-STOP       PIC X.
               05  FILLER             PIC X(31).
      * USED FOR INDIANA DEFERMENT REBATE FORMULA:
           03  REB-B02                REDEFINES REB-COMMON-AREA.
               05  B02-PAYDATE        PIC 9(8)     COMP-3.
               05  B02-DEF-STOP       PIC X.
               05  FILLER             PIC X(34).
      * USED FOR SP-RBFRMLA = 'T', TABLE REFUND FORMULA:
           03  REB-T01                REDEFINES REB-COMMON-AREA.
               05  T01-TBL-NO         PIC 99.
               05  T01-ORGTERM        PIC 9(3).
               05  T01-ELAPSED-DAYS   PIC S9(4).
               05  T01-ELAPSED-MONTHS PIC S9(4).
               05  T01-REFUND-RATE    PIC 999V9.
               05  FILLER             PIC X(23).
      * USED FOR SP-RBRBEARLY = 13 TABLE REFUND FORMULA
      * MERCURY 'NV', PER PROPERTY REFUND ACTION 8:
           03  REB-T02                REDEFINES REB-COMMON-AREA.
               05  T02-ELAPSED-DAYS   PIC S9(5).
               05  T02-INSTERM        PIC 9(5).
               05  FILLER             PIC X(30).

      * NEW SHORT RATE FORMULA SP-RBEARLY = 24 (PP)    TFC #96
           03  REB-T04                REDEFINES REB-COMMON-AREA.
               05  T04-ELAPSED-DAYS   PIC S9(5).
               05  FILLER             PIC X(35).

           03  REB-IRREBT             REDEFINES REB-COMMON-AREA.
               05  IRREBT-REMTERM     PIC S9(5).
               05  IRREBT-INSSUB      PIC 99.
               05  IRREBT-MANYDECIMALS PIC 9(5)V9(4).
               05  FILLER             PIC X(23).
