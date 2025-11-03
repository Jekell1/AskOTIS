      * COPYMEMBER: LIBLP/LP01CD
      ******************************************************************
      *
      *   DESCRIPTION:  CODE DESCRIPTION FILE
      *
      *  CD1-KEY = CD-TYPE, CD-CODE
      *
      *=================================================================
      *  TYPE CODE* NUM DESCRIPTION
      *  ---- ----- --- ----------------------------------------------
      *   AC    4    0  ACTION  [CD-CODE WAS "{" IN A15/A30]
      *   A2    4    0  ACTION CODE 2 SECURITIZATION
      *   AT    3    0  ACTIVITY
      *   BR    5    0  BATCH (PAYMENT) REFERENCE
      *   BS    5    0  PAYMENT REFERENCE CODE LP-REFNO
      *   BW    4    0  BORROWER HOUSING 
      *   CH    2    0  CHARGE OFF (P&L) REASON
      *   CL    4    0  CAN LOAN GROUP ID
      *   CO    2    0  COLLATERAL
      *   CT    5    0  COUNTY INSURANCE TAX RATES FOR KENTUCKY
      *   CY    5    0  CITY INSURANCE TAX RATES FOR KENTUCKY
      *   DF    2    0  DEFERMENT REASON  
      *   EA    6    0  EXTRACTION/ACTION GROUP ID
      *   EL    0    2  EXTENDED LATE CHARGE INFO
      *   FC    1    0  FORM CHARGES (AMT TO OTHBAL)
      *   FU    4    0  FOLLOW-UP             
      *   FZ    2    0  FROZEN REASON
      *   ID    2    0  ID TYPE (KNOW YOUR CUSTOMER SCREENS)  
      *   JT    5    0  JOB TYPE DESCRIPTIONS
      *   LO    4    0  LPO  (TFC)     
      *   LS    2    0  REPO REASON                         
      *   LV    0    1  GROUP LEVELS
      *   OR    4    0  BORROWER ORIGIN
      *** PA    1    0  PAYROLL CODES FOR TIMESHEET PROCESSING (REMOVED)
      *   PC    1    0  PROCESS (TFC)
      *   PH    2    0  PHONE NUMBER DESCRIPTIONS
      *   PL    0    5  POOL ID
      *   PO    2    0  PAYOFF
      *   PS    2    0  PAYMENT RESCHEDULE
      *   PU    3    0  PURPOSE
      *   PX    1    0  P&L
      *   RC    8    0  REP (TFC) 
      *   RE    3    0  COLLECTION RESULT
      *   SR    2    0  SOURCE
      *   TC    2    0  TRAN CODE REFERENCE PAYMENTS
      *   TS    2    0  TRW SPECIAL
      *   UW    2    0  UNWIND SALES FINANCE VOID                 
      *   XL    2    0  COLLATERAL LOCATION (REPO) CHREP4    
      *
      *   * = NUMBER FOR THE CODE REPRESENTS THE MAX LENGTH/BYTES
      *       OF THE CODE FOR GIVEN TYPE.
      *
      *=================================================================
      * REV:
      * SLC 042995 SLC ADDED PLTYPE LOGIC.
      * KEC 020596 KEC ADDED COLLECTOR QUEUE CODE GROUP ID.
      * KEC 020896 KEC ADDED COLLECTOR ID CODE.
      * KEC 031496 KEC ADDED THREE FIELDS:   CD-RE-WORKED;
      *                   CD-RE-RESCHEDULE-REQ; CD-RE-RESCHEDULE-TIME.
      * KEC 071096 KEC ADDED CD-ELUNITPER-CD FOR CONVERSION FROM A50.
      * KEC 092796 KEC ADDED BORROWER HOUSING CODE LAYOUT.
      * KEC 111596 KEC REDEFINED 'CD-CODE' FOR CD-POCD1 AND CD-POCD2.
      * JTG 021797 JTG ADDED CD-FCREC, FORM CHARGES (BHFC)
      * JTG 032797 JTG ADDED CD-RF.....FIELDS FOR APPLICATION
      *                 STATUS CODES
      * JTG 050797 JTG CHANGED CD-ELREC TO MAP TO SP-REC COMP FIELDS
      *                 NOT COMP-3
      * BAH 071197 BAH ADDED CD-GLFEEFINCHG FROM A90 FOR WORLD
      * MJD 071497 MJD ADDED CD-CLREC FOR CAN GROUP ID.
      * JTG 071497 JTG ADDED CODES FOR GIFTNET IN CD-FEEREC
      *                    CD-GLFEE-INT-CHARGABLE
      *                    CD-FEE-GIFTNET
      *                    CD-FEE-PRODNO
      *                    CD-FEE-PRICE
      *                    CD-NEW-PRODNO
      * KEC 092198 ADDED CODE TYPE [ 'PR' PRODUCT CODES (REVOLVING) ]
      * KEC 100198 ADDED CODE TYPE [ 'RP' REVOLVING PROGRAM CODES   ]
      *             INCREASED RECORD SIZE BY 14 BYTES          RSZ: 0070
      * KEC 110398 REMOVED CODE TYPE [ 'RP' REVOLVING PROGRAM CODES   ]
      *             RECORD SIZE REMAINED THE SAME              RSZ: 0070
      * JTG 120798 ADDED CODE TYPE 'RO' -  REVOLVING OTHER CHARGE CODES
      * MJD 990125 ----- UPDATED IN FLEXGEN -----
      * MJD 990303 ADDES "BS" REDEFINITION FOR VALID REFNO'S
      * GLF 990403 ADDED "LO" LOAN PROCESSING OFFICE, "RC"
      *            REP CODE, AND "PC" PROCESS CODE - TFC
      * KEC 990510 >>>>>>>> 'A60-TO-A15' CONVERSION UPDATED <<<<<<<<
      * BAH 990720 CHANGE ELFRMLA-VALID TO INCLUDE J AND K
      * JTG 990729 ADDED CD-RO-NUM TO PROVIDE AN ALPHA KEY RE: CD-RO-CODE
      * KEC 990805 >>>>>>>> 'A10-TO-A15' CONVERSION UPDATED <<<<<<<<
      * KEC 990811 >>>>>>>> 'A60-TO-A15' CONVERSION UPDATED <<<<<<<<
      * GLB 990818 ADD GL ACCOUNTS FOR REVOLVING OTHER CHARGES
      * GLF 990830 ADD TRW SPECIAL CODES (TS) FOR TFC PR #51
      * KEC 000410 >>>>>>>> 'A10-TO-A15' CONVERSION UPDATED <<<<<<<<
      * BLV 112700 ADDED CD-EA-GQ-FG TO DESIGNATE EXTRACTION GROUP AS GLOBAL
      *            QUEUING RATHER THAN REGULAR LOCAL QUEUING,  TFC PR# 60
      * BLV 010829 ADDED "FU" REDEFINITION FOR LOAN FOLLOWUP CODE, PR# 1487
      * JTG 020201 ADDED "CT" COUNTY TAX CODE KENTUCKY AND PERCENTS
      *            ADDED "CY" CITY   TAX CODE KENTUCKY AND PERCENTS   WORLD #288
      * CS  020213 ADDED CD-CY-BOTH-TAX-FG TO CD-CY-REC. USED TO IDENTIFY
      *            IF BOTH CITY & COUNTY TAX SHOULD BE CHARGED   [WORLD PR#288]
      * JTG 020220 CREATED COPY MEMBERS LIBLP/LPCDCT AND LIBLP/LPCDCY
      *            FOR CALCZL.C AND CALCZ3.C                          WORLD #288
      * JTG 020304 REMOVED COPY MEMBERS LIBLP/LPCDCT AND LIBLP/LPCDCY
      *            FOR CALCZL.C AND CALCZ3.C                          WORLD #288
      *
      * KEC 020617 {PR#01823} REGIONAL ACCEPTANCE <A15> [KEVIN MULLEN]
      *            ADDED CD-FEE-MAXAMTPIC 9(5)V99 COMP-3.
      * BAH 020625 ADDED TYPES OF "LS" AND "CH" FOR REPO REASON AND CHARGE
      *            OFF (P&L) REASON, REGACC PR# 1775
      * JTG 021010 ADDED NEW CODE FILE TYPE 'BR' - BATCH REFERENCE PAYMENT CODES
      *                                                             REGACC #0006
      * JTG 021114 ADDED CD-BR-LC-FG                                REGACC #0006
      * BAH 030424 ADDED CD-ALT-GLFEENO FOR LENDMARK PR# 2048
      * CS  031107 ADDED CD-COMM-GLFEENO CD-COMM-PERCENT (GC)
      * JTG 040330 ADDED LOGIC FOR POSTING BATCH PAYMENTS TO ACCOUNT'S
      *            WITH 'LN-OTBAL'                                 CENTRIX #2253
      * CS  040805 ADDED NEW CODE TYPE 'TC' TRANSACTION CODE REF CODES
      *                                        [LENDMARK PR#2353]
      *
      * KEC 041117 {PR#02407} LENDMARK <A15> [KEVIN MULLEN]
      *            ADDED NEW CODE TYPE OF "AS".
      *
      * KEC 050103 {PR#02407} LENDMARK <A15> [KEVIN MULLEN]
      *            CHANGED NEW CODE TYPE OF "AS" FROM X(2) TO X(6).
      *
      * JTG 050608 ADDED CHANGES FOR REPORTING AND TAKING FEE CODE EARNINGS
      *            CREATED CD-FEE-EARNINGS-FG                     LENDMARK #2542
      *
      * BAH 050914 ADDED "DF" - DEFEREMENT REASON CODES, LENDMARK PR# 2568
      * JTG 050927 ADDED NEW CODE TYPE "UW" UNWIND SALES FINANCE VOID
      *            LN-POCD-UNWIND "*U" A LN-POCD-NONLOAN
      *                  AND LN-UNWIND-REASON-CODE                  REGACC #0105
      * BAH 051027 ADDED NEW CODE TYPE "FZ" FROZEN REASON CODE REGACC PR# 114
      * BLF 051107 ADDED CD-BR-MULTIPLE-REFCDS, REGACC PR# 107
      * BLF 060320 ADDED CD-BR-ACH-FG, LENDMARK PR# 2711
      * BAH 060501 ADDED CD-GROUP FOR COLLATERAL CODES, TO GROUP THEM;
      *            LENDMARK PR# 2777
      *  CS 060531 ADD EVENT CODE PR# 2503 SECURITY
      * BLF 060908 ADDED NEW CODE TYPE "PA", PAYROLL CODES FOR TIME SHEET
      *            PROCESSING, WORLD PR# 488	
      * BLF 070215 ADDED CD-BR-PAYOFF-FG, WILL COMPUTE PAYOFF AMT FOR
      *            BT RECORDS, REGACC PR# 160
      *  CS 070417 ADDED CD-BR-POST-SS-TRCD  TO POST 'SS' SALE OF SECURITY
      *                                                REGACC PR#168
      * MJD 070815 ADDED CD-ID-REC                     REGENCY PR#3032
      * BLM 080930 RENAMED CD-BR-ACH-FG TO CD-BR-COLLECT-LC, WASN'T USING
      *            THE ACH FLAG??  WORLD NEEDS TO HAVE SOME INS CLAIM PMTS
      *            LIKE A&H POST PMT BUT DON'T APPLY TO LATE CHARGES
      *            OWING, WORLD PR# 603
      * BLM 081009 ADDED CD-CN-GROUP-ID, REGACC PR# 220
      *
      * KEC 081223 {PR#00000} PARADATA <A15> [JAY CISZEWSKI]
      *            HAD TO RENAME THE 78 LEVELS 'SET-CD-LR-...' TO
      *            'CD---LR-...'.
      *
      *  CS 090923 ADDED CD-ACTIVITY-REQUIRED TO RESULT (CD-AC-RE-REC). IF
      *            CD-ACTIVITY-REQUIRED = "Y", DONT ALLOW A SPACE AS THE
      *            ACTIVITY   PR#255 REGACC
      * BLM 110127 ADDED NEW TYPE "JT" FOR JOB TYPE DESCRIPTIONS, WORLD PR# 739
      * BAH 110411 ADDED NEW TYPE "PS" FOR PAYMENT RESCHEDULE, USED THIS CODE
      *            TO MATCH PAYMENT POSTING CODE OF "PS", REGACC PR# 290
      * BAH 120904 ADDED CD-TITLE-HOLD-DAYSPIC 9(2) FOR PAYOFF CODE ONLY
      *            REGACC PR# 319
      * BAH 130524 ADDED CD-CN-PHONE AND CD-CN-PHONE-EXT, NEWPORT PR# 4272
      *  CS 150819 ADD CD-BRANCH-COMM-AMOUNT EXCEL PR#4821
      *  CS 150922 CHANGED CD-BRANCH-COMM-AMOUNT TO COMP3 PR#4821
      *
      * KEC 2016.0303 {PD#00003} PARADATA <A30> [JAY CISZEWSKI]
      *         - INCREASE CD-REC; CD-SIZE = 250; CD1-KEY = 20
      *         - ADDED CD-LTOUCH-DATE.
      *         - CHANGED CD-REC THAT EACH CODE TYPE HAS ITS OWN
      *           REDEFINITION (CD-??-REC) TO OMIT CONFUSION.
      *           FOR CONSISTENCY, EACH REDEFINITION (CD-??-REC)
      *           WILL HAVE THE FOLLOWING:
      *            - KEY (20 BYTES);
      *            - LTOUCH-DATE (5 BYTES);
      *            - DESC (35 BYTES);
      *            - UNIQUE FIELDS PERTAINING TO CODE TYPE (190 BYTES).
      *         - CHANGED COMP-6 TO BE COMP-3.
      *         - DUE TO BUG FOUND, CREATED CD-AT-REC FOR TYPE "AT" AND
      *           SPLIT OUT CD-AT-REC FIELDS FROM CD-AC-RE-REC FIELDS.
      *         - CHANGED CD-AC-RE-REC TO BE CD-RE-REC, AS WELL AS
      *           CHANGED ITS FIELD TO HAVE A CD-RE- PREFIX.
      *         - CODE TYPE EL, CHANGED COMP TO COMP-3 WHICH WILL NEED
      *           TO MATCH TO SP-LC-INFO FIELDS.
      *         - CODE TYPE RC, REMOVED HASH ON CD-RC-CODE.
      *         - CODE TYPE RL (RELATION CODES) NO LONGER IN USE.
      *
      * KEC 2016.0803 {PD#00003} PARADATA <A30> [JAY CISZEWSKI]
      *         PER BARB/CINDY, INCREASE CD-GC-COMM-PERCENT A BYTE
      *         DUE TO A RECENT WORLD NEED. [PIC 999V99     COMP-3.]
      * BAH 2017.1117 FIXED CD-EL-REC FIELDS SIZES (ELREAD.CBL)
      *
      *=============================[ A15 & A30 ABOVE; A32 & A34 BELOW ]
      * KEC 2018-1120 A32 & A34 DEVELOPMENT.
      *         THESE ACTIONS WERE DONE TO THE 47 LAYOUTS OF CDFILE
      *         FROM THE A30 LAYOUT:
      *          'JOINED' TO USE THE STANDARD KEY (CD-TYPE OF 2,
      *           CD-CODE OF 8, CD-NUMBER OF 8) OF THE CDFILE;
      *          'REMOVED' LAYOUTS BECAUSE WORLD DOES NOT USE;
      *          'MOVED' TO BE A NEW FILE BECAUSE THE KEY DID NOT FIT
      *           THE STANDARD KEY OF THE CDFILE; MOVED TO
      *           LIBLP/LP01CDB (CDBFILE; CN & GC).
      *
      *         TYPE  DESC                                      ACTION
      *         ----  ----------------------------------------  -------
      *          AC  ACTION                                     JOINED 
      *          AS  ADVERTISING SOURCE                         REMOVED
      *          AT  ACTIVITY                                   JOINED
      *          BR  BATCH (PAYMENT) REFERENCE                  JOINED
      *          BS  REFERENCE  (LP-REFNO)                      JOINED
      *          BW  BORROWER HOUSING                           JOINED
      *          CH  CHARGE OFF (P&L) REASON                    JOINED
      *          CL  CAN LOAN GROUP ID                          JOINED
      *          CN  COLLECTOR ID                               MOVED
      *          CO  COLLATERAL                                 JOINED
      *          CT  COUNTY TAX PERCENTS (KENTUCKY)             JOINED
      *          CY  CITY   TAX PERCENTS (KENTUCKY)             JOINED
      *          DF  DEFERMENT REASON                           JOINED
      *          EA  EXTRACTION/ACTION GROUP ID                 JOINED
      *          EL  EXTENDED LATE CHARGE INFO                  JOINED
      *          FC  FORM CHARGES                               JOINED
      *          FU  FOLLOW-UP                                  JOINED
      *          FZ  FROZEN REASON                              JOINED
      *          GC  FEE                                        MOVED
      *          ID  ID TYPE (KNOW YOUR CUSTOMER SCREENS)       JOINED
      *          JT  JOB TYPE                                   JOINED
      *          LO  LPO  (TFC)                                 JOINED
      *          LS  REPO REASON                                JOINED
      *          LV  GROUP LEVELS                               JOINED
      *          PA  PAYROLL (TIME SHEET PROCESSING)            JOINED
      *          PC  PROCESS (TFC)                              JOINED
      *          PH  PHONE NUMBER DESCRIPTIONS                  JOINED
      *          PL  POOL ID                                    JOINED
      *          PO  PAYOFF                                     JOINED
      *          PR  PRODUCTS (REVOLVING)                       REMOVED
      *          PS  PAYMENT RESCHEDULE                         JOINED
      *          PU  PURPOSE                                    JOINED
      *          PX  P&L                                        JOINED
      *          RC  REP (TFC)                                  JOINED
      *          RE  COLLECTION RESULT                          JOINED
      *          RF  TURN DOWN (REFUSAL CODES/LOAN APPS)        REMOVED
      *          RL  RELATION                                   REMOVED
      *          RO  REVOLVING OTHER CHARGE                     REMOVED
      *          SR  SOURCE                                     JOINED
      *          TC  TRAN CODE REFERENCE PAYMENTS               JOINED
      *          TS  TRW SPECIAL                                JOINED
      *          UW  UNWIND SALES FINANCE VOID                  JOINED
      *          XL  COLLATERAL LOCATION (REPO) CHREP4          JOINED
      *
      *         NOTE: THE CD-TYPE OF "{" FOR ACTION CODES IN A30 WAS
      *               CHANGED TO "AC".
      *
      * BAH 2020-0210 ADDED NEW TYPE "OR"; BORROWER ORIGIN (4) #1421
      * BAH 2020-0323 CLEANED UP FUTURE FIELDS, REMOVED GROUP LEVELS
      * JKC 2021-1101 CHANGED CD-ELUNITPER-CD FROM PIC 9 TO PIC X.
      *  BAH 20230600 ADDED FILLER
      ******************************************************************
       78  CD-SIZE                             VALUE 390.
       01  CD-REC.
           03  CD1-KEY.
               05  CD-TYPE                   PIC X(2).
               05  CD-CODE                   PIC X(8).
               05  CD-CODE-CT        REDEFINES CD-CODE.
                   07  CD-CT-STATE           PIC X(2).
                   07  CD-CT-COUNTY          PIC X(3).
                   07  CD-CT-FILLER          PIC X(3).
               05  CD-CODE-CY        REDEFINES CD-CODE.
                   07  CD-CY-STATE           PIC X(2).
                   07  CD-CY-CITY            PIC X(3).
                   07  CD-CY-FILLER          PIC X(3).
               05  CD-CODE-EL        REDEFINES CD-CODE.
                   07  CD-EL-CODE            PIC 9(2).
                   07  CD-EL-FILLER          PIC X(6).
               05  CD-CODE-LV        REDEFINES CD-CODE.
                   07  CD-LV-LEVEL           PIC 9(1).
                   07  CD-LV-FILLER          PIC X(7).
               05  CD-CODE-PL        REDEFINES CD-CODE.
                   07  CD-PL-POOLID          PIC 9(5).
                   07  CD-PL-FILLER          PIC X(3).

      $XFD DATE=YYYYMMDD
           03  CD-LTOUCH-DATE                PIC 9(08)      COMP-3.
           03  CD-DESC                       PIC X(35).

      *-----------------------------------------------------{ ACTIVITY }
           03  CD-AT-ACTIONCD                PIC X(04).
           03  CD-AT-MEMO-REQUIRED           PIC X(01).
           03  CD-AT-SUPERVISOR-ONLY         PIC X(01).
           03  CD-AT-RESULT-REQUIRED         PIC X(01).

      *------------------------------------{ BATCH (PAYMENT) REFERENCE }
      * CD-BR-FILE-TYPE  VALID;
      * S  - STANDARD FORMAT
      * LC - LATE CHARGE ACCESS OPTION
      * B  - BANKRUPT FILE FORMAT
      * C  - CASHLESS DEBIT CARD UMC PURCHASE
           03  CD-BR-FILE-TYPE               PIC XX.
           03  CD-BR-FILE-NAME               PIC X(6).
           03  CD-BR-PAYOFF-FG               PIC X.
           03  CD-BR-POST-SS-TRCD            PIC X.
           03  CD-BR-GL-ACCOUNTS.
               05  CD-BR-GL-BRANCH           PIC 9(4).
               05  CD-BR-GL-ACCT             PIC 9(7).
           03  CD-BR-ALLOT-OPTION            PIC X.
           03  CD-BR-LBOX-OPTION             PIC X.
           03  CD-BR-LC-FG                   PIC X.
           03  CD-BR-OT-GL-ACCOUNTS.
               05  CD-BR-OT-GL-BRANCH        PIC 9(4).
               05  CD-BR-OT-GL-ACCT          PIC 9(7).
           03  CD-BR-MULTIPLE-REFCDS         PIC X.
           03  CD-BR-COLLECT-LC              PIC X.

      *---------------------------------------------------{ COLLATERAL }
           03  CD-CO-GROUP                   PIC X(01).
           03  CD-CO-SECURED                 PIC X(01).
           03  CD-CO-TRW                     PIC X(02).

      *----------------------{ COUNTY INSURANCE TAX RATES FOR KENTUCKY }
      *    01 = CL DEC           05 = AH      09 = O2
      *    02 = CL DEC JOINT     06 = PP      10 = O3
      *    03 = CL LEV           07 = PP2     11 = O4
      *    04 = CL LEV JOINT     08 = O1      12 = O5
      *-----------------------------------------------------------------
           03  CD-CT-INSTBL                    OCCURS 12.
               05  CD-CT-INSTAXFRMLA         PIC X.
               05  CD-CT-INSTAXAMT           PIC 99V99      COMP-3.
           03  CD-CT-BOTH-TAX-FG             PIC X.

      *------------------------{ CITY INSURANCE TAX RATES FOR KENTUCKY }
      *    01 = CL DEC           05 = AH      09 = O2
      *    02 = CL DEC JOINT     06 = PP      10 = O3
      *    03 = CL LEV           07 = PP2     11 = O4
      *    04 = CL LEV JOINT     08 = O1      12 = O5
      *-----------------------------------------------------------------
           03  CD-CY-INSTBL                    OCCURS 12.
               05  CD-CY-INSTAXFRMLA         PIC X.
               05  CD-CY-INSTAXAMT           PIC 99V99      COMP-3.
           03  CD-CY-BOTH-TAX-FG             PIC X.

      *-----------------------------------{ EXTRACTION/ACTION GROUP ID }
           03  CD-EA-CQ-FG                   PIC X(1).
           03  CD-EA-GQ-FG                   PIC X(1).

      *------------------------------------{ EXTENDED LATE CHARGE INFO }
      *    LATE CHARGE FIELDS                  (22 BYTES)
      *
      *    NOTE:  CD-EL-INFO AREA MAPS TO THE SP-REC FIELD SP-LC-INFO
      *           (FIRST 22 BYTES; NOT INCLUDING SP-LC-CODE WHICH IS
      *            THE LAST 2 BYTES).
      *           IF CD-EL-INFO CHANGES YOU  ***MUST*** CHANGE
      *           THE SP-LC-INFO FIELDS TO MATCH.
      *-----------------------------------------------------------------
           03  CD-EL-INFO.
               05  CD-ELGRACE                PIC 9(2).
               05  CD-ELGRACECD              PIC X(1).
                   88 CD-ELGRACECD-VALID       VALUE " " "S" "W".
               05  CD-ELTYPE                 PIC X(2).
                   88  CD-ELTYPE-VALID         VALUE " " "A" THRU "O"
                                                     "Q".
               05  CD-ELRATE                 PIC 9(4)V9(4)  COMP-3.
               05  CD-ELFRMLA                PIC X(2).
                   88  CD-ELFRMLA-VALID        VALUE " " "A" THRU "N".
               05  CD-ELMINCHG               PIC 9(2)V99    COMP-3.
               05  CD-ELMAXAMT               PIC 9(4)V99    COMP-3.
               05  CD-ELDELFAC               PIC 9(3)       COMP-3.
                   88  CD-ELDELFAC-VALID       VALUE 0 THRU 100.
               05  CD-ELYRTYPE               PIC 9(4)       COMP-3.
                   88  CD-ELYRTYPE-VALID       VALUE 358 360 365 367.
           03  CD-ELUNITPER-CD               PIC X(1).

      *-------------------------------------------------------{ PAYOFF }
           03  CD-PO-TITLE-HOLD-DAYS         PIC 9(02).

      *--------------------------------------------{ COLLECTION RESULT }
           03  CD-RE-ACTIONCD                PIC X(04).
           03  CD-RE-MEMO-REQUIRED           PIC X(01).
           03  CD-RE-SUPERVISOR-ONLY         PIC X(01).
           03  CD-RE-ACTIVITY-REQUIRED       PIC X(01).
           03  CD-RE-RESCHEDULE-REQ          PIC X(01).
           03  CD-RE-RESCHEDULE-TIME         PIC 9(03).
           03  CD-RE-WORKED-ACCOUNT          PIC X(01).

      *--------------------------------------------{ FORM CHARGES }
           03  CD-FC-FEEAMT                  PIC S9(7)V99 COMP-3.

      *--------------------------------------{ TRANSACTION REFERENCE CODES }
           03  CD-TC-REFNO                   PIC X(05) OCCURS 10.

      *----------------------------------{ PAYMENT RESCHEDULE REASON CODES }
           03  CD-PS-TDR-FLAG                PIC X(01).

           03  FILLER                        PIC X(97).
