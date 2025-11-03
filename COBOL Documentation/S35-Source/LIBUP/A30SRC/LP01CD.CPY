      * COPYMEMBER: LIBLP/LP01CD
      ******************************************************************
      *
      *   DESCRIPTION:  CODE DESCRIPTION FILE
      *
      *=================================================================
      *   KEY - TYPE, CODE
      *
      *   TYPE:
      *   {  - ACTION CODES
      *   AS - ADVERTISING SOURCE CODE
      *   AT - ACTIVITY CODES
      *   BR - BATCH (PAYMENT) REFERENCE CODES
      *   BS - REFERENCE  (LP-REFNO)
      *   BW - BORROWER HOUSING CODES
      *   CH - CHARGE OFF (P&L) REASON CODE
      *   CL - CAN LOAN GROUP ID
      *   CN - COLLECTOR ID
      *   CO - COLLATERAL CODES
      *   CT - COUNTY TAX PERCENTS CODES (KENTUCKY)
      *   CY - CITY   TAX PERCENTS CODES (KENTUCKY)
      *   DF - DEFERMENT REASON CODES
      *   DI - DISPOSAL/HOLD COLLATERAL (REPO)
      *   DR - DORMANT REASON CODES (NOTES PAYABLE)
      *   EA - EXTRACTION/ACTION GROUP ID
      *   EL - EXTENDED LATE CHARGE INFO
      *   FC - FORM CHARGES
      *   FU - FOLLOW-UP CODES
      *   FZ - FROZEN REASON CODES
      *   GC - FEE CODES
      *   ID - ID TYPE (KNOW YOUR CUSTOMER SCREENS)
      *   JT - JOB TYPE
      *   LO - LPO (TFC)
      *   LR - TRIAD SCORING
      *   LS - REPO REASON CODE
      *   LV - GROUP LEVELS
      *   ML - MAIL LIST SOURCE CODES
      *   PA - PAYROLL CODES (TIME SHEET PROCESSING)
      *   PC - PROCESS CODE   (TFC)
      *   PH - PHONE NUMBER DESCRIPTIONS
      *   PL - POOL ID CODES
      *   PO - PAYOFF CODES
      *   PR - PRODUCTS CODES (REVOLVING)
      *   PS - PAYMENT RESCHEDULE CODE (USED THIS TO MATCH POSTING CODE)
      *   PU - PURPOSE CODES
      *   PX - P&L CODES
      *   RC - REP CODE  (TFC)
      *   RE - COLLECTION RESULT CODE
      *   RF - TURN DOWN (REFUSAL CODES/LOAN APPLICATION STATUS)
      *   RL - RELATION CODES   [NO LONGER IN USE; KEC 2016.0303]
      *   RO - REVOLVING OTHER CHARGE CODES
      *   SR - SOURCE CODES
      *   TC - TRAN CODE REFERENCE PAYMENTS
      *   TS - TRW SPECIAL CODES
      *   UW - UNWIND SALES FINANCE VOID
      *   XL - COLLATERA LOCATION (REPO) CHREP4
      *
      *   CODE:  TYPE DEPENDENT
      *
      *-----------------------------------------------------------------
      *
      *   NOTE:  ON ACTION CODES CD-TYPE WILL BEGIN WITH {
      *          AND THE 2ND CHARACTER OF CD-TYPE WILL
      *          BE PART OF CD-CODE
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
      *            ADDED CD-FEE-MAXAMT PIC 9(5)V99 COMP-3.
      * BAH 020625 ADDED TYPES OF "LS" AND "CH" FOR REPO REASON AND CHARGE
      *            OFF (P&L) REASON, REGACC PR# 1775
      * JTG 021010 ADDED NEW CODE FILE TYPE 'BR' - BATCH REFERENCE PAYMENT CODES
      *                                                             REGACC #0006
      * JTG 021114 ADDED CD-BR-LC-FG                                REGACC #0006
      * BAH 030424 ADDED CD-ALT-GLFEENO FOR LENDMARK PR# 2048
      * KEC 030924 {PR#00040} REGIONAL ACCEPTANCE <A15> [KEVIN MULLEN]
      *            ADDED NEW TYPE OF 'LR' FOR TRIAD SCORING
      * CS  031107 ADDED CD-COMM-GLFEENO CD-COMM-PERCENT (GC)
2253  * JTG 040330 ADDED LOGIC FOR POSTING BATCH PAYMENTS TO ACCOUNT'S
2253  *            WITH 'LN-OTBAL'    (MN/CDBRMN)                  CENTRIX #2253
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
      * BAH 120904 ADDED CD-TITLE-HOLD-DAYS PIC 9(2) FOR PAYOFF CODE ONLY
      *            REGACC PR# 319
      * BLM 121203 ADDED NEW TYPE "DR" FOR NOTES PAYABLE DORMANT REASON,
      *            REGWAN PR# 4150
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
      ******************************************************************
      *
      *    BASE FORMAT
      *
      *=================================================================
      *    THE FOLLOWING CODE TYPES (CD-TYPE) WORK WITH USING THE KEY
      *    (CD-CODE) OF THE BASE FORMAT (CD-FILLER MUST BE SPACES) BUT
      *    SHOULD CHANGE TO USING THEIR OWN REDEFINITION GOING FORWARD:
      *
      *    AT - ACTIVITY CODES                          CODE=3
      *    CH - CHARGE OFF (P&L) REASON CODE            CODE=2
      *    CO - COLLATERAL CODES                        CODE=2
      *    DF - DEFERMENT REASON CODES                  CODE=2
      *    DI - DISPOSAL/HOLD COLLATERAL (REPO)         CODE=2
      *    DR - DORMANT REASON CODES (NOTES PAYABLE)    CODE=3
      *    FC - FORM CHARGES                            CODE=1
      *    FZ - FROZEN REASON CODES                     CODE=2
      *    ID - ID TYPE (KNOW YOUR CUSTOMER SCREENS)    CODE=2
      *    LR - TRIAD SCORING                           CODE=2
      *    LS - REPO REASON CODE                        CODE=2
      *    ML - MAIL LIST SOURCE CODES                  CODE=2
      *    PA - PAYROLL CODES (TIME SHEET PROCESSING)   CODE=1
      *    PC - PROCESS CODE   (TFC)                    CODE=1
      *    PH - PHONE NUMBER DESCRIPTIONS               CODE=2
      *    PO - PAYOFF CODES                            CODE=2
      *    PU - PURPOSE CODES                           CODE=3
      *    PX - P&L CODES                               CODE=1
      *    RE - COLLECTION RESULT CODE                  CODE=3
      *    TC - TRAN CODE REFERENCE PAYMENTS            CODE=2
      *    TS - TRW SPECIAL CODES                       CODE=2
      *    UW - UNWIND SALES FINANCE VOID               CODE=2
      *    XL - COLLATERA LOCATION (REPO) CHREP4        CODE=2
      ******************************************************************
       78  CD-SIZE                             VALUE 250.
       01  CD-REC.
           03  CD1-KEY.
               05  CD-TYPE                     PIC X(02).
               05  CD-CODE                     PIC X(03).
               05  CD-FILLER                   PIC X(15).

      *-----------------------------------------------------------------
      *>>XDD DATE=YYYYMMDD
           03  CD-LTOUCH-DATE                  PIC 9(08)      COMP-3.
           03  CD-DESC                         PIC X(35).
           03  FILLER                          PIC X(190).

      ******************************************************************
      *
      *    { - ACTION CODES
      *
      ******************************************************************
       01  CD-ACTION-REC.
           03  CD-ACTION-KEY.
               05  CD-ACTION-TYPE              PIC X(01).
               05  CD-ACTION-CODE              PIC X(04).
               05  CD-ACTION-FILLER            PIC X(15).

      *-----------------------------------------------------------------
      *>>XDD DATE=YYYYMMDD
           03  CD-ACTION-LTOUCH-DATE           PIC 9(08)      COMP-3.
           03  CD-ACTION-DESC                  PIC X(35).
           03  FILLER                          PIC X(190).

      ******************************************************************
      *
      *    AS - ADVERTISING SOURCE CODE
      *
      ******************************************************************
       01  CD-AS-REC.
           03  CD-AS-KEY.
               05  CD-AS-TYPE                  PIC X(02).
               05  CD-AS-CODE                  PIC X(06).
               05  CD-AS-FILLER                PIC X(12).

      *-----------------------------------------------------------------
      *>>XDD DATE=YYYYMMDD
           03  CD-AS-LTOUCH-DATE               PIC 9(08)      COMP-3.
           03  CD-AS-DESC                      PIC X(35).
           03  FILLER                          PIC X(190).

      ******************************************************************
      *
      *    AT - ACTIVITY CODES
      *
      ******************************************************************
       01  CD-AT-REC.
           03  CD-AT-KEY.
               05  CD-AT-TYPE                  PIC X(02).
               05  CD-AT-CODE                  PIC X(03).
               05  CD-AT-FILLER                PIC X(15).

      *-----------------------------------------------------------------
      *>>XDD DATE=YYYYMMDD
           03  CD-AT-LTOUCH-DATE               PIC 9(08)      COMP-3.
           03  CD-AT-DESC                      PIC X(35).

      *-----------------------------------------------------------------
      *    THESE FIELDS ARE MATCHING (LOCATION, NAME, SIZE) IN CD-RE-REC
      *    THEY MUST MATCH; WORKING TOGETHER WITH CD-RE-REC FIELDS
      *-----------------------------------------------------------------
           03  CD-AT-ACTIONCD                  PIC X(04).
           03  CD-AT-MEMO-REQUIRED             PIC X(01).
           03  CD-AT-SUPERVISOR-ONLY           PIC X(01).

      *-----------------------------------------------------------------
           03  CD-AT-RESULT-REQUIRED           PIC X(01).

           03  FILLER                          PIC X(183).

      ******************************************************************
      *
      *    BR - BATCH (PAYMENT) REFERENCE CODES
      *
      ******************************************************************
       01  CD-BR-REC.
           03  CD-BR-KEY.
               05  CD-BR-TYPE                  PIC X(02).
               05  CD-BR-REFCD.
                   07  CD-BR-REFCDX  OCCURS 5  PIC X(01).
               05  CD-BR-FILLER                PIC X(13).

      *-----------------------------------------------------------------
      *>>XDD DATE=YYYYMMDD
           03  CD-BR-LTOUCH-DATE               PIC 9(08)      COMP-3.
           03  CD-BR-DESC                      PIC X(35).

           03  CD-BR-FILE-TYPE                 PIC XX.
           03  CD-BR-FILE-NAME                 PIC X(6).
           03  CD-BR-PAYOFF-FG                 PIC X.
           03  CD-BR-POST-SS-TRCD              PIC X.
           03  CD-BR-GL-ACCOUNTS.
               05  CD-BR-GL-BRANCH             PIC 9(4).
               05  CD-BR-GL-ACCT               PIC 9(7).
           03  CD-BR-ALLOT-OPTION              PIC X.
           03  CD-BR-LBOX-OPTION               PIC X.
           03  CD-BR-LC-FG                     PIC X.
2253       03  CD-BR-OT-GL-ACCOUNTS.
2253           05  CD-BR-OT-GL-BRANCH          PIC 9(4).
2253           05  CD-BR-OT-GL-ACCT            PIC 9(7).
           03  CD-BR-MULTIPLE-REFCDS           PIC X.
      *    03  CD-BR-ACH-FG                    PIC X.
           03  CD-BR-COLLECT-LC                PIC X.
           03  FILLER                          PIC X(153).

      ******************************************************************
      *
      *    BS - PAYMENT REFERENCE CODES     (LP-REFNO)
      *
      ******************************************************************
       01  CD-BS-REC.
           03  CD-BS-KEY.
               05  CD-BS-TYPE                  PIC X(02).
               05  CD-BS-REFNO.
                   07 CD-BS-REFNOX   OCCURS 5  PIC X(01).
               05  CD-BS-FILLER                PIC X(13).

      *-----------------------------------------------------------------
      *>>XDD DATE=YYYYMMDD
           03  CD-BS-LTOUCH-DATE               PIC 9(08)      COMP-3.
           03  CD-BS-DESC                      PIC X(35).
           03  FILLER                          PIC X(190).

      ******************************************************************
      *
      *    BW - BORROWER HOUSING CODES        (BUYING,OWNS,RENTS,NONE)
      *
      ******************************************************************
       01  CD-BW-REC.
           03  CD-BW-KEY.
               05  CD-BW-TYPE                  PIC X(02).
               05  CD-BW-CODE                  PIC X(04).
               05  CD-BW-FILLER                PIC X(14).

      *-----------------------------------------------------------------
      *>>XDD DATE=YYYYMMDD
           03  CD-BW-LTOUCH-DATE               PIC 9(08)      COMP-3.
           03  CD-BW-DESC                      PIC X(35).
           03  FILLER                          PIC X(190).

      ******************************************************************
      *
      *    CH - CHARGE OFF (P&L) REASON CODE
      *
      ******************************************************************
       01  CD-CH-REC.
           03  CD-CH-KEY.
               05  CD-CH-TYPE                  PIC X(02).
               05  CD-CH-CODE                  PIC X(02).
               05  CD-CH-FILLER                PIC X(16).

      *-----------------------------------------------------------------
      *>>XDD DATE=YYYYMMDD
           03  CD-CH-LTOUCH-DATE               PIC 9(08)      COMP-3.
           03  CD-CH-DESC                      PIC X(35).
           03  FILLER                          PIC X(190).

      ******************************************************************
      *
      *    CL - CAN LOAN GROUP ID
      *
      *-----------------------------------------------------------------
      *    CD-CL???? WERE OLD FIELDS FROM A15; CD-CL-???? NEW FORMAT.
      ******************************************************************
       01  CD-CL-REC.
           03  CD-CL-KEY.
               05  CD-CL-TYPE                  PIC X(02).
               05  CD-CL-GROUP-ID              PIC X(04).
               05  CD-CL-FILLER                PIC X(14).

      *-----------------------------------------------------------------
      *>>XDD DATE=YYYYMMDD
           03  CD-CL-LTOUCH-DATE               PIC 9(08)      COMP-3.
           03  CD-CL-DESC                      PIC X(35).
           03  FILLER                          PIC X(190).

      ******************************************************************
      *
      *    CN - COLLECTOR ID
      *
      ******************************************************************
       01  CD-CN-REC.
           03  CD-CN-KEY.
               05  CD-CN-TYPE                  PIC X(02).
               05  CD-CN-BRANCH                PIC 9(04).
               05  CD-CN-CODE                  PIC X(03).
               05  CD-CN-FILLER                PIC X(11).

      *-----------------------------------------------------------------
      *>>XDD DATE=YYYYMMDD
           03  CD-CN-LTOUCH-DATE               PIC 9(08)      COMP-3.
           03  CD-CN-DESC                      PIC X(35).
           03  CD-CN-GROUP-ID                  PIC X(6).
           03  CD-CN-PHONE                     PIC 9(10)      COMP-3.
           03  CD-CN-PHONE-EXT                 PIC 9(5)       COMP-3.
           03  FILLER                          PIC X(175).

      ******************************************************************
      *
      *    CO - COLLATERAL CODES
      *
      ******************************************************************
       01  CD-CO-REC.
           03  CD-CO-KEY.
               05  CD-CO-TYPE                  PIC X(02).
               05  CD-CO-CODE                  PIC X(02).
               05  CD-CO-FILLER                PIC X(16).

      *-----------------------------------------------------------------
      *>>XDD DATE=YYYYMMDD
           03  CD-CO-LTOUCH-DATE               PIC 9(08)      COMP-3.
           03  CD-CO-DESC                      PIC X(35).
           03  CD-CO-GROUP                     PIC X(01).
           03  CD-CO-SECURED                   PIC X(01).
           03  CD-CO-TRW                       PIC X(02).
           03  FILLER                          PIC X(186).

      ******************************************************************
      *
      *    CT - COUNTY INSURANCE TAX RATES FOR KENTUCKY
      *
      *    LEVEL: 1  CL DEC             8  O1
      *           2  CL DEC JOINT       9  O2
      *           3  CL LEV            10  O3
      *           4  CL LEV JOINT      11  O4
      *           5  AH                12  O5
      *           6  PP
      *           7  PP 2
      *
      ******************************************************************
       01  CD-CT-REC.
           03  CD-CT-KEY.
               05  CD-CT-TYPE                  PIC X(02).
               05  CD-CT-STATE                 PIC X(02).
               05  CD-CT-COUNTY                PIC X(03).
               05  CD-CT-FILLER                PIC X(13).

      *-----------------------------------------------------------------
      *>>XDD DATE=YYYYMMDD
           03  CD-CT-LTOUCH-DATE               PIC 9(08)      COMP-3.
           03  CD-CT-COUNTY-DESC               PIC X(35).
           03  CD-CT-INSTBL                    OCCURS 12.
               05  CD-CT-INSTAXFRMLA           PIC X.
               05  CD-CT-INSTAXAMT             PIC 99V99      COMP-3.
           03  CD-CT-BOTH-TAX-FG               PIC X.
           03  FILLER                          PIC X(141).

      ******************************************************************
      *
      *    CY - CITY INSURANCE TAX RATES FOR KENTUCKY
      *
      *    LEVEL: 1  CL DEC             8  O1
      *           2  CL DEC JOINT       9  O2
      *           3  CL LEV            10  O3
      *           4  CL LEV JOINT      11  O4
      *           5  AH                12  O5
      *           6  PP
      *           7  PP 2
      ******************************************************************
       01  CD-CY-REC.
           03  CD-CY-KEY.
               05  CD-CY-TYPE                  PIC X(02).
               05  CD-CY-STATE                 PIC X(02).
               05  CD-CY-CITY                  PIC X(03).
               05  CD-CY-FILLER                PIC X(13).

      *-----------------------------------------------------------------
      *>>XDD DATE=YYYYMMDD
           03  CD-CY-LTOUCH-DATE               PIC 9(08)      COMP-3.
           03  CD-CY-CITY-DESC                 PIC X(35).
           03  CD-CY-INSTBL                    OCCURS 12.
               05  CD-CY-INSTAXFRMLA           PIC X.
               05  CD-CY-INSTAXAMT             PIC 99V99      COMP-3.
           03  CD-CY-BOTH-TAX-FG               PIC X.
           03  FILLER                          PIC X(141).

      ******************************************************************
      *
      *    DF - DEFERMENT REASON CODES
      *
      ******************************************************************
       01  CD-DF-REC.
           03  CD-DF-KEY.
               05  CD-DF-TYPE                  PIC X(02).
               05  CD-DF-CODE                  PIC X(02).
               05  CD-DF-FILLER                PIC X(16).

      *-----------------------------------------------------------------
      *>>XDD DATE=YYYYMMDD
           03  CD-DF-LTOUCH-DATE               PIC 9(08)      COMP-3.
           03  CD-DF-DESC                      PIC X(35).
           03  FILLER                          PIC X(190).

      ******************************************************************
      *
      *    DI - DISPOSAL/HOLD COLLATERAL (REPO)
      *
      ******************************************************************
       01  CD-DI-REC.
           03  CD-DI-KEY.
               05  CD-DI-TYPE                  PIC X(02).
               05  CD-DI-CODE                  PIC X(02).
               05  CD-DI-FILLER                PIC X(16).

      *-----------------------------------------------------------------
      *>>XDD DATE=YYYYMMDD
           03  CD-DI-LTOUCH-DATE               PIC 9(08)      COMP-3.
           03  CD-DI-DESC                      PIC X(35).
           03  FILLER                          PIC X(190).

      ******************************************************************
      *
      *    DR - DORMANT REASON DESCRIPTIONS (NOTES PAYABLE)
      *
      ******************************************************************
       01  CD-DR-REC.
           03  CD-DR-KEY.
               05  CD-DR-TYPE                  PIC X(02).
               05  CD-DR-CODE                  PIC X(03).
               05  CD-DR-FILLER                PIC X(15).

      *-----------------------------------------------------------------
      *>>XDD DATE=YYYYMMDD
           03  CD-DR-LTOUCH-DATE               PIC 9(08)      COMP-3.
           03  CD-DR-DESC                      PIC X(35).
           03  FILLER                          PIC X(190).

      ******************************************************************
      *
      *    EA - EXTRACTION/ACTION GROUP ID
      *
      *-----------------------------------------------------------------
      *    COLLECTOR QUEUE GROUP CODE ID:  TIED TO EA-REC.
      ******************************************************************
       01  CD-EA-REC.
           03  CD-EA-KEY.
               05  CD-EA-TYPE                  PIC X(02).
               05  CD-EA-ID                    PIC X(06).
               05  CD-EA-FILLER                PIC X(12).

      *-----------------------------------------------------------------
      *>>XDD DATE=YYYYMMDD
           03  CD-EA-LTOUCH-DATE               PIC 9(08)      COMP-3.
           03  CD-EA-DESC                      PIC X(35).
           03  CD-EA-CQ-FG                     PIC X(1).
           03  CD-EA-GQ-FG                     PIC X(1).
           03  FILLER                          PIC X(188).

      ******************************************************************
      *
      *    EL - EXTENDED LATE CHARGE INFO
      *
      ******************************************************************
       01  CD-ELREC.
           03  CD-EL-KEY.
               05  CD-EL-TYPE                  PIC X(2).
               05  CD-EL-CODE                  PIC 9(2).
               05  CD-EL-FILLER                PIC X(16).

      *-----------------------------------------------------------------
      *>>XDD DATE=YYYYMMDD
           03  CD-EL-LTOUCH-DATE               PIC 9(08)      COMP-3.
           03  CD-EL-DESC                      PIC X(35).

      *-----------------------------------------------------------------
      *    LATE CHARGE FIELDS                  (22 BYTES)
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    NOTE:  THIS AREA MAPS THE SP-REC FIELD SP-LC-INFO
      *           (FIRST 22 BYTES; NOT INCLUDING SP-LC-CODE WHICH IS
      *            THE LAST 2 BYTES).
      *           IF CD-EL-INFO CHANGES YOU  ***MUST*** CHANGE
      *           THE SP-LC-INFO FIELDS TO MATCH.
      *-----------------------------------------------------------------
           03  CD-EL-INFO.
               05  CD-ELGRACE                  PIC 9(2).

               05  CD-ELGRACECD                PIC X(1).
                   88 CD-ELGRACECD-VALID       VALUE " " "S" "W".

               05  CD-ELTYPE                   PIC X(2).
                   88  CD-ELTYPE-VALID         VALUE " " "A" THRU "O"
                                                     "Q".

               05  CD-ELRATE                   PIC 9(4)V9(4)  COMP-3.

               05  CD-ELFRMLA                  PIC X(2).
                   88  CD-ELFRMLA-VALID        VALUE " " "A" THRU "N".

               05  CD-ELMINCHG                 PIC 9(2)V99    COMP-3.
               05  CD-ELMAXAMT                 PIC 9(4)V99    COMP-3.

               05  CD-ELDELFAC                 PIC 9(3)       COMP-3.
                   88  CD-ELDELFAC-VALID       VALUE 0 THRU 100.

               05  CD-ELYRTYPE                 PIC 9(4)       COMP-3.
                   88  CD-ELYRTYPE-VALID       VALUE 358 360 365 367.

      *-----------------------------------------------------------------
           03  CD-ELUNITPER-CD                 PIC 9(1).
           03  FILLER                          PIC X(165).

      ******************************************************************
      *
      *    FC - FORM CHARGES
      *
      ******************************************************************
       01  CD-FCREC.
           03  CD-FC-KEY.
               05  CD-FC-TYPE                  PIC X(02).
               05  CD-FC-FORM-CHG-CD           PIC X(01).
               05  CD-FC-FILLER                PIC X(17).

      *-----------------------------------------------------------------
      *>>XDD DATE=YYYYMMDD
           03  CD-FC-LTOUCH-DATE               PIC 9(08)      COMP-3.
           03  CD-FC-DESC                      PIC X(35).
           03  CD-FC-FEEAMT                    PIC S9(6)V99.
           03  FILLER                          PIC X(182).

      ******************************************************************
      *
      *    FU - FOLLOW-UP REASON CODES
      *
      ******************************************************************
       01  CD-FU-REC.
           03  CD-FU-KEY.
               05  CD-FU-TYPE                  PIC X(02).
               05  CD-FU-CODE                  PIC X(04).
               05  CD-FU-FILLER                PIC X(14).

      *-----------------------------------------------------------------
      *>>XDD DATE=YYYYMMDD
           03  CD-FU-LTOUCH-DATE               PIC 9(08)      COMP-3.
           03  CD-FU-DESC                      PIC X(35).
           03  FILLER                          PIC X(190).

      ******************************************************************
      *
      *    FZ - FROZEN REASON CODE
      *
      ******************************************************************
       01  CD-FZ-REC.
           03  CD-FZ-KEY.
               05  CD-FZ-TYPE                  PIC X(02).
               05  CD-FZ-CODE                  PIC X(02).
               05  CD-FZ-FILLER                PIC X(16).

      *-----------------------------------------------------------------
      *>>XDD DATE=YYYYMMDD
           03  CD-FZ-LTOUCH-DATE               PIC 9(08)      COMP-3.
           03  CD-FZ-DESC                      PIC X(35).
           03  FILLER                          PIC X(190).

      ******************************************************************
      *
      *    GC - FEE CODES
      *
      ******************************************************************
       01  CD-GC-REC.
           03  CD-GC-KEY.
               05  CD-GC-TYPE                  PIC X(02).
               05  CD-GC-BRNO                  PIC 9(04).
               05  CD-GC-FEECODE               PIC X(02).
               05  CD-GC-FILLER                PIC X(12).

      *-----------------------------------------------------------------
      *>>XDD DATE=YYYYMMDD
           03  CD-GC-LTOUCH-DATE               PIC 9(08)      COMP-3.
           03  CD-GC-DESC                      PIC X(35).
      *    03  CD-GC-GLFEE-DESC      REDEFINES CD-GC-DESC
      *                                        PIC X(35).

           03  CD-GC-GLFEENO                   PIC 9(7)       COMP-3.
           03  CD-GC-GLFEEFINCHG               PIC X(1).
           03  CD-GC-GLFEE-INT-CHARGABLE       PIC X(1).
           03  CD-GC-FEE-GIFTNET               PIC X(1).
           03  CD-GC-FEE-PRODNO                PIC 9(5)       COMP-3.
           03  CD-GC-FEE-PRICE                 PIC 9(4)V99    COMP-3.
           03  CD-GC-NEW-PRODNO                PIC 9(5)       COMP-3.
           03  CD-GC-FEE-MAXAMT                PIC 9(5)V99    COMP-3.
           03  CD-GC-ALT-GLFEENO               PIC 9(7)       COMP-3.
           03  CD-GC-COMM-GLFEENO              PIC 9(11)      COMP-3.
           03  CD-GC-COMM-PERCENT              PIC 999V99     COMP-3.
           03  CD-GC-FEE-EARNINGS-FG           PIC X.
           03  CD-GC-BRANCH-COMM-AMOUNT        PIC 9(6)       COMP-3.
           03  FILLER                          PIC X(151).

      ******************************************************************
      *
      *    ID - ID TYPE (KNOW YOUR CUSTOMER SCREENS)
      *
      ******************************************************************
       01  CD-ID-REC.
           03  CD-ID-KEY.
               05  CD-ID-TYPE                  PIC X(02).
               05  CD-ID-CODE                  PIC X(02).
               05  CD-ID-FILLER                PIC X(16).

      *-----------------------------------------------------------------
      *>>XDD DATE=YYYYMMDD
           03  CD-ID-LTOUCH-DATE               PIC 9(08)   COMP-3.
           03  CD-ID-DESC                      PIC X(35).
           03  FILLER                          PIC X(190).

      ******************************************************************
      *
      *    JT - JOB TYPE DESCRIPTIONS
      *
      ******************************************************************
       01  CD-JT-REC.
           03  CD-JT-KEY.
               05  CD-JT-TYPE                  PIC X(02).
               05  CD-JT-CODE                  PIC X(05).
               05  CD-JT-FILLER                PIC X(13).

      *-----------------------------------------------------------------
      *>>XDD DATE=YYYYMMDD
           03  CD-JT-LTOUCH-DATE               PIC 9(08)   COMP-3.
           03  CD-JT-DESC                      PIC X(35).
           03  FILLER                          PIC X(190).

      ******************************************************************
      *
      *    LO - LPO (TFC) LOAN PROCESSING OFFICES
      *
      ******************************************************************
       01  CD-LO-REC.
           03  CD-LO-KEY.
               05  CD-LO-TYPE                  PIC X(02).
               05  CD-LO-CODE                  PIC X(04).
               05  CD-LO-FILLER                PIC X(14).

      *-----------------------------------------------------------------
      *>>XDD DATE=YYYYMMDD
           03  CD-LO-LTOUCH-DATE               PIC 9(08)   COMP-3.
           03  CD-LO-DESC                      PIC X(35).
           03  FILLER                          PIC X(190).

      ******************************************************************
      *
      *    LR - TRIAD SCORING
      *
      ******************************************************************
       78  CD---LR-CODE-BEHAVIOR               VALUE "BE".
       78  CD---LR-CODE-BANKRUPTCY             VALUE "BK".
       78  CD---LR-CODE-CREDIT-BUREAU          VALUE "CB".
      *-----------------------------------------------------------------
       01  CD-LR-REC.
           03  CD-LR-KEY.
               05  CD-LR-TYPE                  PIC X(02).
               05  CD-LR-CODE                  PIC X(02).
                   88  CD-LR-CODE-BEHAVIOR         VALUE "BE".
                   88  CD-LR-CODE-BANKRUPTCY       VALUE "BK".
                   88  CD-LR-CODE-CREDIT-BUREAU    VALUE "CB".
                   88  CD-LR-CODE-VALID            VALUE "BE" "BK"
                                                         "CB".
               05  CD-LR-FILLER                PIC X(16).

      *-----------------------------------------------------------------
      *>>XDD DATE=YYYYMMDD
           03  CD-LR-LTOUCH-DATE               PIC 9(08)   COMP-3.
           03  CD-LR-DESC                      PIC X(35).

      *-----------------------------------------------------------------
       78  CD---LR-GRADE-HIGH                  VALUE 1.
       78  CD---LR-GRADE-MEDIUM                VALUE 2.
       78  CD---LR-GRADE-LOW                   VALUE 3.
       78  CD---LR-GRADE-MAX                   VALUE 3.
       78  CD---LR-RANGE-LOW                   VALUE 1.
       78  CD---LR-RANGE-HIGH                  VALUE 2.
       78  CD---LR-RANGE-MAX                   VALUE 2.
       78  CD---LR-SCORE-MAX                   VALUE 6.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           03  CD-LR-SCORE-TABLE.
               05  CD-LR-GRADE-OCCURS     OCCURS CD---LR-GRADE-MAX.
                   07  CD-LR-RANGE-OCCURS OCCURS CD---LR-RANGE-MAX.
                       09  CD-LR-SCORE         PIC 9(04)      COMP-3.

           03  FILLER                  REDEFINES CD-LR-SCORE-TABLE.
               05  CD-LR-SCORE-OCCURS     OCCURS CD---LR-SCORE-MAX.
                   07  CD-LR-SCORE-ELE         PIC 9(04)      COMP-3.

      *-----------------------------------------------------------------
           03  FILLER                          PIC X(172).

      ******************************************************************
      *
      *    LS - REPO REASON CODE
      *
      ******************************************************************
       01  CD-LS-REC.
           03  CD-LS-KEY.
               05  CD-LS-TYPE                  PIC X(02).
               05  CD-LS-CODE                  PIC X(02).
               05  CD-LS-FILLER                PIC X(16).

      *-----------------------------------------------------------------
      *>>XDD DATE=YYYYMMDD
           03  CD-LS-LTOUCH-DATE               PIC 9(08)   COMP-3.
           03  CD-LS-DESC                      PIC X(35).
           03  FILLER                          PIC X(190).

      ******************************************************************
      *
      *    LV - GROUP LEVELS
      *
      ******************************************************************
       01  CD-LVREC.
           03  CD-LV-KEY.
               05  CD-LV-TYPE                  PIC X(02).
               05  CD-LV-LEVEL                 PIC 9(01).
               05  CD-LV-FILLER                PIC X(17).

      *-----------------------------------------------------------------
      *>>XDD DATE=YYYYMMDD
           03  CD-LV-LTOUCH-DATE               PIC 9(08)   COMP-3.
           03  CD-LV-DESC                      PIC X(35).
           03  FILLER                          PIC X(190).

      ******************************************************************
      *
      *    ML - MAIL LIST SOURCE CODES
      *
      ******************************************************************
       01  CD-ML-REC.
           03  CD-ML-KEY.
               05  CD-ML-TYPE                  PIC X(02).
               05  CD-ML-CODE                  PIC X(02).
               05  CD-ML-FILLER                PIC X(16).

      *-----------------------------------------------------------------
      *>>XDD DATE=YYYYMMDD
           03  CD-ML-LTOUCH-DATE               PIC 9(08)   COMP-3.
           03  CD-ML-DESC                      PIC X(35).
           03  FILLER                          PIC X(190).

      ******************************************************************
      *
      *    PA - PAYROLL CODES FOR TIMESHEET PROCESSING
      *
      ******************************************************************
       01  CD-PAREC.
           03  CD-PA-KEY.
               05  CD-PA-TYPE                  PIC X(2).
               05  CD-PA-CODE                  PIC X(1).
               05  CD-PA-FILLER                PIC X(17).

      *-----------------------------------------------------------------
      *>>XDD DATE=YYYYMMDD
           03  CD-PA-LTOUCH-DATE               PIC 9(08)   COMP-3.
           03  CD-PA-DESC                      PIC X(35).
           03  FILLER                          PIC X(190).

      ******************************************************************
      *
      *    PC - PROCESSING CODE (TFC)
      *
      ******************************************************************
       01  CD-PC-REC.
           03  CD-PC-KEY.
               05  CD-PC-TYPE                  PIC X(02).
               05  CD-PC-CODE                  PIC X(01).
               05  CD-PC-FILLER                PIC X(17).

      *-----------------------------------------------------------------
      *>>XDD DATE=YYYYMMDD
           03  CD-PC-LTOUCH-DATE               PIC 9(08)   COMP-3.
           03  CD-PC-DESC                      PIC X(35).
           03  FILLER                          PIC X(190).

      ******************************************************************
      *
      *    PH - PHONE NUMBER DESCRIPTIONS
      *
      ******************************************************************
       01  CD-PH-REC.
           03  CD-PH-KEY.
               05  CD-PH-TYPE                  PIC X(02).
               05  CD-PH-CODE                  PIC X(02).
               05  CD-PH-FILLER                PIC X(16).

      *-----------------------------------------------------------------
      *>>XDD DATE=YYYYMMDD
           03  CD-PH-LTOUCH-DATE               PIC 9(08)   COMP-3.
           03  CD-PH-DESC                      PIC X(35).
           03  FILLER                          PIC X(190).

      ******************************************************************
      *
      *    PL - POOL ID CODES
      *
      ******************************************************************
       01  CD-PL-REC.
           03  CD-PL-KEY.
               05  CD-PL-TYPE                  PIC X(02).
               05  CD-PL-POOLID                PIC 9(05).
               05  CD-PL-FILLER                PIC X(13).

      *-----------------------------------------------------------------
      *>>XDD DATE=YYYYMMDD
           03  CD-PL-LTOUCH-DATE               PIC 9(08)   COMP-3.
           03  CD-PL-DESC                      PIC X(35).
           03  FILLER                          PIC X(190).

      ******************************************************************
      *
      *    PO - PAYOFF CODES
      *
      ******************************************************************
       01  CD-PO-REC.
           03  CD-PO-KEY.
               05  CD-PO-TYPE                  PIC X(02).

               05  CD-PO-POCD                  PIC X(02).
                   88  CD-POCD-NONLOAN         VALUE "**" "*I" "*R".
                   88  CD-POCD-OPEN            VALUE "  ".
                   88  CD-POCD-POFF            VALUE "1 " THRU "ZZ".
                   88  CD-POCD-INRESCISSION    VALUE "*I".
                   88  CD-POCD-RESCIND         VALUE "*R".
                   88  CD-POCD-VOID            VALUE "**".

               05  FILLER                      REDEFINES CD-PO-POCD.
                   07  CD-PO-POCD1             PIC X(1).
                   07  CD-PO-POCD2             PIC X(1).

               05  CD-PO-FILLER                PIC X(16).

      *-----------------------------------------------------------------
      *>>XDD DATE=YYYYMMDD
           03  CD-PO-LTOUCH-DATE               PIC 9(08)   COMP-3.
           03  CD-PO-DESC                      PIC X(35).
           03  CD-PO-TITLE-HOLD-DAYS           PIC 9(02).
           03  FILLER                          PIC X(188).

      ******************************************************************
      *
      *    PR - PRODUCTS CODES (REVOLVING)
      *
      ******************************************************************
       01  CD-PR-REC.
           03  CD-PR-KEY.
               05  CD-PR-TYPE                  PIC X(02).
               05  CD-PR-CODE                  PIC X(05).
               05  CD-PR-FILLER                PIC X(13).

      *-----------------------------------------------------------------
      *>>XDD DATE=YYYYMMDD
           03  CD-PR-LTOUCH-DATE               PIC 9(08)   COMP-3.
           03  CD-PR-DESC                      PIC X(35).
           03  FILLER                          PIC X(190).

      ******************************************************************
      *
      *    PS - PAYMENT RESCHEDULE REASON CODE
      *
      ******************************************************************
       01  CD-PS-REC.
           03  CD-PS-KEY.
               05  CD-PS-TYPE                  PIC X(02).
               05  CD-PS-CODE                  PIC X(05).
               05  CD-PS-FILLER                PIC X(13).

      *-----------------------------------------------------------------
      *>>XDD DATE=YYYYMMDD
           03  CD-PS-LTOUCH-DATE               PIC 9(08)   COMP-3.
           03  CD-PS-DESC                      PIC X(35).
           03  CD-PS-TDR-FLAG                  PIC X(01).
           03  FILLER                          PIC X(189).

      ******************************************************************
      *
      *    PU - PURPOSE CODES
      *
      ******************************************************************
       01  CD-PU-REC.
           03  CD-PU-KEY.
               05  CD-PU-TYPE                  PIC X(02).
               05  CD-PU-CODE                  PIC X(03).
               05  CD-PU-FILLER                PIC X(15).

      *-----------------------------------------------------------------
      *>>XDD DATE=YYYYMMDD
           03  CD-PU-LTOUCH-DATE               PIC 9(08)   COMP-3.
           03  CD-PU-DESC                      PIC X(35).
           03  FILLER                          PIC X(190).

      ******************************************************************
      *
      *    PX - P&L CODES
      *
      ******************************************************************
       01  CD-PX-REC.
           03  CD-PX-KEY.
               05  CD-PX-TYPE                  PIC X(02).
               05  CD-PX-PLTYPE                PIC X(01).
               05  CD-PX-FILLER                PIC X(17).

      *-----------------------------------------------------------------
      *>>XDD DATE=YYYYMMDD
           03  CD-PX-LTOUCH-DATE               PIC 9(08)   COMP-3.
           03  CD-PX-DESC                      PIC X(35).
           03  FILLER                          PIC X(190).

      ******************************************************************
      *
      *    RC - REP CODE (TFC)  * * * * USES HASH TO STORE KEY * * * *
      *                         AND STORES THE ALPHA VERSION IN
      *                             CD-RC-CODE-ALPHA FOR DISPLAY
      *                             PURPOSES IN THE SCAN...
      *
      ******************************************************************
       01  CD-RC-REC.
           03  CD-RC-KEY.
               05  CD-RC-TYPE                  PIC X(02).
               05  CD-RC-CODE                  PIC X(08).
               05  CD-RC-FILLER                PIC X(10).

      *-----------------------------------------------------------------
      *>>XDD DATE=YYYYMMDD
           03  CD-RC-LTOUCH-DATE               PIC 9(08)   COMP-3.
           03  CD-RC-DESC                      PIC X(35).
           03  FILLER                          PIC X(190).

      ******************************************************************
      *
      *    RE - COLLECTION RESULT CODE
      *
      ******************************************************************
       01  CD-RE-REC.
           03  CD-RE-KEY.
               05  CD-RE-TYPE                  PIC X(02).
               05  CD-RE-CODE                  PIC X(03).
               05  CD-RE-FILLER                PIC X(15).

      *-----------------------------------------------------------------
      *>>XDD DATE=YYYYMMDD
           03  CD-RE-LTOUCH-DATE               PIC 9(08)   COMP-3.
           03  CD-RE-DESC                      PIC X(35).

      *-----------------------------------------------------------------
      *    THESE FIELDS ARE MATCHING (LOCATION, NAME, SIZE) IN CD-AT-REC
      *    THEY MUST MATCH; WORKING TOGETHER WITH CD-AT-REC FIELDS
      *-----------------------------------------------------------------
           03  CD-RE-ACTIONCD                  PIC X(04).
           03  CD-RE-MEMO-REQUIRED             PIC X(01).
           03  CD-RE-SUPERVISOR-ONLY           PIC X(01).

      *-----------------------------------------------------------------
           03  CD-RE-ACTIVITY-REQUIRED         PIC X(01).
           03  CD-RE-RESCHEDULE-REQ            PIC X(01).
           03  CD-RE-RESCHEDULE-TIME           PIC 9(03).
           03  CD-RE-WORKED-ACCOUNT            PIC X(01).
           03  FILLER                          PIC X(178).


      ******************************************************************
      *
      *    RF - TURN DOWN (REFUSAL CODES/LOAN APPLICATION STATUS)
      *
      ******************************************************************
       01  CD-RF-REC.
           03  CD-RF-KEY.
               05  CD-RF-TYPE                  PIC X(02).
               05  CD-RF-CODE                  PIC X(03).
               05  CD-RF-STATCD                PIC X(03).
               05  CD-RF-FILLER                PIC X(12).

      *-----------------------------------------------------------------
      *>>XDD DATE=YYYYMMDD
           03  CD-RF-LTOUCH-DATE               PIC 9(08)   COMP-3.
           03  CD-RF-DESC                      PIC X(35).
           03  FILLER                          PIC X(190).

      ******************************************************************
      *
      *    RO - REVOLVING OTHER CHARGE CODES
      *
      ******************************************************************
       01  CD-RO-REC.
           03  CD-RO-KEY.
               05  CD-RO-TYPE                  PIC X(02).
               05  CD-RO-CODE                  PIC X(05).
               05  CD-RO-FILLER                PIC X(13).

      *-----------------------------------------------------------------
      *>>XDD DATE=YYYYMMDD
           03  CD-RO-LTOUCH-DATE               PIC 9(08)   COMP-3.
           03  CD-RO-DESC                      PIC X(35).
           03  CD-RO-NUM                       PIC 9(2).
      *    03  CD-RO-GL-ACCT                   PIC 9(11).
           03  FILLER                          PIC X(188).

      ******************************************************************
      *
      *    SR - SOURCE CODES
      *
      ******************************************************************
       01  CD-SR-REC.
           03  CD-SR-KEY.
               05  CD-SR-TYPE                  PIC X(02).
               05  CD-SR-CODE                  PIC X(02).
               05  CD-SR-FILLER                PIC X(16).

      *-----------------------------------------------------------------
      *>>XDD DATE=YYYYMMDD
           03  CD-SR-LTOUCH-DATE               PIC 9(08)   COMP-3.
           03  CD-SR-DESC                      PIC X(35).
           03  FILLER                          PIC X(190).

      ******************************************************************
      *
      *    TC - TRANSACTION REFERENCE CODESS
      *
      ******************************************************************
       01  CD-TC-REC.
           03  CD-TC-KEY.
               05  CD-TC-TYPE                  PIC X(02).
               05  CD-TC-TRCD                  PIC X(02).
               05  CD-TC-FILLER                PIC X(16).

      *-----------------------------------------------------------------
      *>>XDD DATE=YYYYMMDD
           03  CD-TC-LTOUCH-DATE               PIC 9(08)   COMP-3.
           03  CD-TC-DESC                      PIC X(35).
           03  CD-TC-REFNO    OCCURS 10 TIMES  PIC X(05).
           03  FILLER                          PIC X(140).

      ******************************************************************
      *
      *    TS - TRW SPECIAL CODES
      *
      ******************************************************************
       01  CD-TS-REC.
           03  CD-TS-KEY.
               05  CD-TS-TYPE                  PIC X(02).
               05  CD-TS-CODE                  PIC X(02).
               05  CD-TS-FILLER                PIC X(16).

      *-----------------------------------------------------------------
      *>>XDD DATE=YYYYMMDD
           03  CD-TS-LTOUCH-DATE               PIC 9(08)   COMP-3.
           03  CD-TS-DESC                      PIC X(35).
           03  FILLER                          PIC X(190).

      ******************************************************************
      *
      *    UW - UNWIND CODE (REGACC)
      *
      *-----------------------------------------------------------------
      *    SALES FINANCE (VOID) UNWIND
      ******************************************************************
       01  CD-UW-REC.
           03  CD-UW-KEY.
               05  CD-UW-TYPE                  PIC X(02).
               05  CD-UW-CODE                  PIC X(02).
               05  CD-UW-FILLER                PIC X(16).

      *-----------------------------------------------------------------
      *>>XDD DATE=YYYYMMDD
           03  CD-UW-LTOUCH-DATE               PIC 9(08)   COMP-3.
           03  CD-UW-DESC                      PIC X(35).
           03  FILLER                          PIC X(190).

      ******************************************************************
      *
      *    XL - COLLATERA LOCATION (REPO) CHREP4
      *
      ******************************************************************
       01  CD-XL-REC.
           03  CD-XL-KEY.
               05  CD-XL-TYPE                  PIC X(02).
               05  CD-XL-CODE                  PIC X(02).
               05  CD-XL-FILLER                PIC X(16).

      *-----------------------------------------------------------------
      *>>XDD DATE=YYYYMMDD
           03  CD-XL-LTOUCH-DATE               PIC 9(08)   COMP-3.
           03  CD-XL-DESC                      PIC X(35).
           03  FILLER                          PIC X(190).

