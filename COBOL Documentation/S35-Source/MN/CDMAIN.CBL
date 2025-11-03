       IDENTIFICATION DIVISION.
       PROGRAM-ID.      CDMAIN.
      ******************************************************************
      *
      *                 (MN)
      *  DESCRIPTION:   CODE FILE MAINTENANCE
      *
      * "CN" USES CDBMAN AND THE CDBFILE
      *
      *=================================================================
      *  MN/CDMAIN IS USING CDMAIN-TYPE (LINKAGE) TO KNOW WHICH
      *  CODE TYPE IS BEING MAINTENANCED.
      *
      *  FOLLOWING ARE THE CODE TYPES (CD-TYPE) & WHICH MAINTENANCE
      *  PROGRAM IS USED:
      *
      *  [CDMAIN] AC - ACTION CODES
      *  [CDMAIN] AT - ACTIVITY CODES
      *  [CDMAIN] BR - BATCH (PAYMENT) REFERENCE CODES
      *  [CDMAIN] BS - REFERENCE  (LP-REFNO)
      *  [CDMAIN] BW - BORROWER HOUSING CODES
      *  [CDMAIN] CH - CHARGE OFF (P&L) REASON CODE
      *  [CDMAIN] CL - CAN LOAN GROUP ID
      *  [CDMAIN] CO - COLLATERAL CODES
      *  [CTMAIN] CT - COUNTY TAX PERCENTS CODES (KENTUCKY)
      *  [CYMAIN] CY - CITY   TAX PERCENTS CODES (KENTUCKY)
      *  [CDMAIN] DF - DEFERMENT REASON CODES
      *  [CDMAIN] EA - EXTRACTION/ACTION GROUP ID
      *  [ELMAIN] EL - EXTENDED LATE CHARGE INFO
      *  [CDMAIN] FC - FORM CHARGES
      *  [CDMAIN] FU - FOLLOW-UP CODES
      *  [CDMAIN] FZ - FROZEN REASON CODES
      *  [GCMAIN] GC - FEE CODES
      *  [CDMAIN] ID - ID TYPE (KNOW YOUR CUSTOMER SCREENS)
      *  [CDMAIN] JT - JOB TYPE
      *  [CDMAIN] LO - LPO (TFC)
      *  [CDMAIN] LS - REPO REASON CODE
      *  [CDMAIN] LV - GROUP LEVELS
      *  [CDMAIN] OR - BORROWER ORIGIN
      *  [CDMAIN] PC - PROCESS CODE   (TFC)
      *  [CDMAIN] PH - PHONE NUMBER DESCRIPTIONS
      *  [CDMAIN] PL - POOL ID CODES
      *  [CDMAIN] PO - PAYOFF CODES
      *  [CDMAIN] PR - PRODUCTS CODES (REVOLVING)
      *  [CDMAIN] PS - PAYMENT RESCHEDULE CODE
      *  [CDMAIN] PU - PURPOSE CODES
      *  [CDMAIN] PX - P&L CODES
      *  [CDMAIN] RC - REP CODE  (TFC)
      *  [CDMAIN] RE - COLLECTION RESULT CODE
      *  [CDMAIN] RF - TURN DOWN (REFUSAL CODES/LOAN APPLICATION STATUS)
      *  [XXXXXX] RL - RELATION CODES  [NO LONGER IN USE; KEC 2016.0303]
      *  [CDMAIN] SR - SOURCE CODES
      *  [TCMAIN] TC - TRAN CODE REFERENCE PAYMENTS
      *  [CDMAIN] TS - TRW SPECIAL CODES
      *  [CDMAIN] UW - UNWIND SALES FINANCE VOID
      *  [CDMAIN] XL - COLLATERA LOCATION (REPO) CHREP4
      *
      *=================================================================
      * REV:
      * JTG 022889 ALLOWED COLL FILE MAINT F4/F5/F1
      * JTG 122691 ADDED CD-MEMO-REQUIRED FOR ATMAIN.C & REMAIN.C
      *            ADDED CD-ACTIONCD + WINDOW SCAN LOGIC
      * JTG 121592 ADDED LOGIC FOR LV GROUP LEVEL LVMAIN
      * KEC 020996 ***NOTE*** MERGED SP/CSMAIN INTO MN/CDMAIN!!!
      *            CHANGED THE CALLING OF THIS PROGRAM.  DRIVERS ARE
      *            NO LONGER NEEDED.
      * MJD 030696 ADDED "MOVE 0 TO FIRST-TIME" AT TOP OF PROGRAM.
      *            THIS MOVE IS TO PREVENT PROBLEMS WITH SCAN WINDOWS
      *            WHEN CALLED FROM THE INTERRUPT. (F12 WINDOW DOES
      *            NOT CANCEL PROPERLY SO FIRST-TIME IS NOT RESET TO
      *            DEFAULT VALUE.
      * KEC 031196 FIXED THE "LV" PART BY MOVING THE ENTERED LEVEL
      *            INTO A HOLD-KEY.
      * KEC 031396 CHANGES MADE FOR RESULT CODES.  ADDED
      *            CQ-RE-WORKED-ACCOUNT, CQ-RE-RESCHEDULE-REQ,
      *            CQ-RE-RESCHEDULE-TIME.
      * JFF 081396 FIXED F6-SCAN FOR "EA" BY REQUIRING 'Y' OR 'N' AT
      *            QUEUE PROMPT TO SET HOLD-EA-CQ-FG BEFORE
      *            WI/CDSCAN IS CALLED.
      * JFF 082796 CHANGED F6-SCAN TO ENTER "Y" OR "N" AT QUEUE PROMPT
      *            ONLY IF CDTYPE IS "EA".
      * JFF 093096 ADDED NEW CODE TYPE 'BORROWER HOUSING CODE'
      *            CDMAIN-TYPE = "BW".
      * KEC 111896 REVISED FOR CODE TYPE "PO"; ADDED CHECKS FOR VALID
      *            CODES THAT ARE NOT SPACES OR NOT 'CD-POCD-NONLOAN'.
      * JTG 021797 ADDED NEW CODE TYPE 'FORM CHARGE CODE' CDMAIN-TYPE
      *             = "FC".
      * JTG 032797 ADDED LOGIC FOR 'RF' CD-RF-STATCD
      *  CS 060797 ON DELETE OF COLLECTOR ID, CHECKS FOR RESCHEDULES
      *            AND CHOICE TO DELETE OR REASSIGN RESCHEDULES
      * MJD 071497 ADDED LOGIC FOR "CL" CAN LOAN GROUP ID
      * BLV 012398 FIXED DELETE ROUTINES FOR COLLECTOR QUEUE
      * MJD 021798 ADDED TYPE "PH" FOR PHONE NUMBER TYPES
      * KEC 092898 ADDED TYPE "PR" FOR PRODUCT CODE TYPE (REVOLVING);
      *            ADDED TYPE "RP" FOR REVOLVING PROGRAM CODE.
      * JTG 102098 REMOVED TYPE 'RP' AND MADE SEPERATE ENTRY PROGRAM
      *            MN/RPMAIN.C SO HELP WOULD WORK
      * KEC 110398 REMOVED THE CODE TYPE OF 'RP' FROM THE FILE LAYOUT
      *             (LIBLP/LP01CD) BUT KEPT THE RECORD LAYOUT SIZE
      *             AT 70 BYTES.
      * KEC 120298 ADDED FIXED FOR "PL".  WHEN ENTERING A "PL" CODE
      *             IT WAS DEFAULTING TO:
      *                MOVE VDUBUF TO CD-CODE
      *             NOW IS
      *                MOVE VDUNUM0 TO CD-POOLID
      *            THIS IS A PROBLEM FOUND BY CINDY/MERCURY(JUST A10).
      * JTG 120798 ADDED TYPE "RO" OTHER CHARGES CODE TYPE (REVOLVING);
      * MJD 990303 ADDED "BS" CODE FOR REFNO:
      * GLF 990305 ADDED "LO", "PC" AND "RC" FOR TFC CODES
      * JTG 990729 ADDED CD-RO-NUM TO PROVIDE AN ALPHA KEY RE: CD-RO-CODE
      * GLF 990830 ADDED "TS" TRW SPECIAL CODES, FOR TFC PR#51
      * GLB 990913 ADDED G/L ACCOUNT NUMBER CD-RO-GL-ACCT
      * GLB 990920 REMOVED G/L ACCOUNT NUMBER; NOT 1 TO 1 RELATIONSHIP
      * JTG 000224 CHANGED MAX-RO FROM 3 TO 2 TO MATCH SCREEN
      * BLV 112800 ADDED CD-EA-GQ-FG TO DESIGNATE GROUP AS USING GLOBAL
      *            QUEUING LOGIC, ALSO CLEANED UP A LITTLE, TFC PR# 60
      * MJD 001205 ADDED LOGIC FOR BR-GLOBAL-QUEUE = "Y" (TFC PR#60).
      * BLV 010924 ADDED LOGIC FOR CD-FU-REC, FOLLOW-UP CODE, PR# 1487
      * BAH 020626 ADDED REPO REASON (LS) AND CHARGE OFF REASON (CH), REGACC
      *            PR# 1775
      * JTG 021011 ADDED NEW CODE FILE TYPE 'BR' - BATCH PAYMENT REFERENCE CODES
      *            BATCH PAYMENT REVISIONS                          REGACC #0006
      * JTG 021114 ADDED ENTRY OF CD-BR-LC-FG                       REGACC #0006
      * JTG 021121 ALLOWED ENTRY OF 'LC' FILE FORMAT                REGACC #0006
      * JTG 021209 REMOVED TEST WHEN 'LC' FILE FORMAT
      *            "LOCK BOX OPTION CAN'T = ALLOTMENT OPTION"       REGACC #0006
      * JTG 021210 CORRECTED SIZE OF FBR-7X                         REGACC #0006
      * JTG 040330 ADDED LOGIC FOR POSTING BATCH PAYMENTS TO ACCOUNT'S
      *            WITH 'LN-OTBAL'                                 CENTRIX #2253
      * BLV 040623 FIX START ON CQ2-FILE, REGACC PR# 59
      * JTG 050927 ADDED 32. SF VOID (UNWIND) CODES
      * BAH 051027 ADDED "FZ" FROZEN REASON CODES, REGACC PR# 114
      * BLF 051107 ADDED CD-BR-MULTIPLE-REFCDS AS FIELD 4, SHIFTED OTHER
      *            CD-BR FIELDS DOWN 1, REGACC PR# 107
      * BAH 060501 ADDED CD-GROUP FOR COLLATERAL GROUPS, LENDMARK PR#2777
      *  CS 060531 ADDED "EV" EVENT CODES SECURITY PR#2503
      *  CS 060608 REMOVED "EV" EVENT CODE
      * BLF 070219 ADDED CD-BR-PAYOFF-FG, REGACC PR# 160
      * BLF 070416 FIXED BUG (CREATED 11/07/05 WHEN MULTIPLE-REFCDS ADDED):
      *            IN ACTION CODE FILE MAINT CD-RESULT-REQUIRED WOULD NOT
      *            GET MOVED IN;  IN RESULT CODE MAINTENANCE,
      *            CD-RE-WORKED-ACCOUNT WOULD NOT GET MOVED IN
      *  CS 070417 ADDED CD-BR-POST-TRCD AND ALSO FIXED CD-BR-PAYOFF-FG, WOULD
      *            NOT ACCEPT A 'N' VALUE  PR#168 REGACC
      * MJD 070815 ADDED CD-ID-REC - KNOW YOUR CUSTOMER ID TYPES REGENCY#3032
      * BLM 080930 ADDED CD-BR-COLLECT-LC, WHETHER TO COLLECT LATE CHARGES
      *            OWING, WORLD PR# 603
      * CLS 090427 CHANGED CD-BR-POST-SS-TRCD TO ALLOW ANOTHER OPTION. IF 'Y'
      *            POST 'SS', IF 'N' POST AS 'PA' IF 'R' REJECT IF ACCOUNT IN
      *            REPO STATUS   PR#244 REGACC
      *  CS 090925 ADDED CD-RE-ACTIVITY-REQUIRED FOR RESULT CODES PR#255 REGACC
      * BAH 101006 ALLOW "**" FOR "TS" TRW SPECIAL CODE
      * BLM 110128 ADDED CD TYPE "JT" FOR JOB TYPES, WORLD PR# 739
      * BAH 110411 ADDED CD TYPE "PS" FOR PAYMENT RESCHEDULE REASON CODE,
      *                                                          REGACC PR# 290
      * BAH 120904 ADDED CD-TITLE-HOLD-DAYS PIC 9(2) FOR PAYOFF CODE ONLY,
      *                                                          REGACC PR# 319
      * BLM 121203 ADDED CD TYPE "DR" FOR NOTES DORMANT REASON CODES,
      *            REGWAN PR# 4150
      *  CS 130506 IF REGACC DON'T ALLOW A REFCD OF "UNE??" REGACC PR#338
      * BLM 131121 CHANGE CD-SUPERVISOR-ONLY TO BE A SECURITY LEVEL TO
      *            ALLOW REQUIRING CERTAIN USERS TO ENTER A PASSWORD TO
      *            USE A ACTIVITY OR RESULT CODE, DEVELOPMENT PR# 4391
      * BAH 160303 REMOVED TEST ON CD-TRW FORCING IT TO BE NUMERIC,
      *            NEEDS TO ALLOW ALPHANUMERIC VALUES, MODERN PR#5008
      * KEC 2016.0315 {PD#00003} PARADATA <A30> [JAY CISZEWSKI]
      *         CHANGED CD-RC-CODE TO NO LONGER USE HASH.
      *         REMOVED CD-RC-CODE-ALPHA; USING CD-RC-CODE NOW.
      *         REMOVED USING CODE-TYPE (CDMAIN-TYPE = "??") AND NOW
      *          USING THE FIELD CDMAIN-TYPE (LINKAGE) TO KNOW WHICH
      *          CODE TYPE IS BEING MAINTENANCED.
      * BLM 20160425 ALLOW CD-BR-FILE-TYPE OF "B" FOR BANKRUPT FILE
      *              FORMAT, WORLD PR# 1151
      * KEC 20160624 PD#00003 PARADATA <A30> [JAY CISZEWSKI]
      *              REMOVED USE OF OLD STATUS ROUTINES [LIBLA/MASTATUS(W)].
      * KEC 20161004 PD#00003 PARADATA <A30> [JAY CISZEWSKI]
      *              FIXED SAVE-CQ2-KEY FOR NEW LAYOUT & SIZE.
      * BAH 20190129 REMOVED CD-CN, CREATE CDBMAN FOR CN CODE, TOO
      *               CONFUSING IN HERE
      * BAH 20190129 REMOVED "RF", "RO" AND "PR" REVOLVING
      * BAH 20200210 ADDED CD TYPE "OR"; BORROWER ORIGIN #1421
      * BAH 20200624 ADD OPEN-CD1-FILE AND CLOSE FOR TEST-CAN-GROUP
      *              REMOVED THEM FROM THE TSTCANID COPY, #TST001
      * BAH 20210202 ADD NEW CD-BR-FILE-TYPE "C" CASHLESS DEBIT CARD
      *              PURCHASE OF UMC UNITED MOTOR CLUB, PR#1490
      * BLM 20210708 FIX F-02, F-03, F-05 & VERIFY-ACTION-CODE ROUTINES,
      *              "RE" RESULT CODE FIELD VALUES WERE GETTING MOVED
      *              TO "AT" ACTIVITY FIELDS SO RESULT FIELDS NOT 
      *              UPDATING
      * BLM 20220323 ALLOW CD-BR-FILE-TYPE OF "P" FOR PL'S, P2'S, P3'S, 
      *              WORLD PR# 1548
      * BAH 20220420 ADDED CDTYPE "A2" FOR ACTIONCODE 2 SECURITIZATION #1551
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

      ******************************************************************
      *
      *    W O R K I N G - S T O R A G E - S E C T I O N
      *
      ******************************************************************
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)MN/CDMAIN.CBL S35 06/06/23-13:31:25 >".

      ******************************************************************
      *
      *    F I L E - R E C - W O R K E R S
      *
      ******************************************************************
       COPY "LIBLP/LP01CD.CPY".
       COPY "LIBLP/LP01CD_SQL.CPY".
       COPY "LIBLP/LPWSCD.CPY".
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".

      *=================================================================
       01  CHANGED-CD-REC              PIC X(CD-SIZE) VALUE SPACES.
       01  ORIGINAL-CD-REC             PIC X(CD-SIZE) VALUE SPACES.

      *=================================================================
       01  HOLD-CD1-KEY.
           03  HOLD-CD-TYPE                    PIC X(2).
           03  HOLD-CD-CODE                    PIC X(8).

       01  HOLD-CD-LV-KEY            REDEFINES HOLD-CD1-KEY.
           03  HOLD-CD-LV-TYPE                 PIC X(2).
           03  HOLD-CD-LV-LEVEL                PIC 9(1).
           03  HOLD-CD-LV-FILLER               PIC X(7).

       01  HOLD-CD-PL-KEY            REDEFINES HOLD-CD1-KEY.
           03  HOLD-CD-PL-TYPE                 PIC X(2).
           03  HOLD-CD-PL-POOLID               PIC 9(5).
           03  HOLD-CD-PL-FILLER               PIC X(3).



      ******************************************************************
      *
      *    M I S C E L L A N E O U S - W O R K E R S
      *
      ******************************************************************
       01  ACTION.
           03  CHKEY                   PIC X(04)   VALUE SPACES.
           03  CHNO                    PIC 9(02)   VALUE ZEROES.

       01  CHAN                        PIC X(04).

       01  ELE                         PIC 9(02)   VALUE ZEROES.

       01  TEST-REFNO.
           03  FILLER                  PIC X(3).
           03  TEST-REF-LAST2          PIC X(2).

       01  TEST-POCD                   PIC X(2).
           88  TEST-POCD-NONLOAN         VALUE "**" "*I" "*R".
           88  TEST-POCD-OPEN            VALUE "  ".
           88  TEST-POCD-POFF            VALUE "1 " THRU "ZZ".
           88  TEST-POCD-INRESCISSION    VALUE "*I".
           88  TEST-POCD-RESCIND         VALUE "*R".
           88  TEST-POCD-VOID            VALUE "**".
       01  FILLER                      REDEFINES TEST-POCD.
           03  TEST-POCD1             PIC X(1).
           03  FILLER                 PIC X(1).

       01  TEST-REFCD.
           03  TEST-REFCD-BYTE        OCCURS 5 PIC X.

       01  ERRCD                       PIC X(01)   VALUE SPACES.
       01  FIRST-TIME                  PIC 9(01)   VALUE ZEROES.
       01  IO-TYPE                     PIC X(03)   VALUE SPACES.
       01  LEGEND                      PIC X(70)   VALUE SPACES.
       01  NEW-FLAG                    PIC 9(01)   VALUE ZEROES.
       01  ONE                         PIC S9(1)   VALUE ZEROES.
       01  HOLD-BR-FILE-NAME.
           03  HOLD-BR-FILE-NAME-TAB  OCCURS 6 TIMES
                                       PIC X.
       01  LOWER-A                     PIC X    VALUE X'61'.
       01  LOWER-Z                     PIC X    VALUE X'7A'.

       01  SCR-PROMPT-1            PIC X(40)     VALUE
           "F6-SCAN, F7-EXIT, ENTER CODE:".

       01  SCR-PROMPT-3            PIC X(40)     VALUE
           "ENTER 'Y' OR 'N' TO SCAN:".

       01  SUB                         PIC 9(03)     VALUE ZEROES.

       01  HOLD-FORM-PATHNAME          PIC X(22)     VALUE SPACES.
       01  HOLD-SCR-PROMPT             PIC X(40)     VALUE SPACES.

      ******************************************************************
      *
      *    S C R E E N - D I S P L A Y - B U F F E R
      *
      ******************************************************************
       01  DISPLAY-BUF                               VALUE SPACES.
           03  F01                     PIC X(35).

       01  DISPLAY-LIST.
           03  FILLER                  PIC X(04)     VALUE "010.".

       01  DISPLAY-BUF2                              VALUE SPACES.
           03  F02-1                   PIC X(26).
           03  F02-2                   PIC X(01).
           03  F02-3                   PIC X(01).
           03  F02-4                   PIC X(01).
           03  F02-5                   PIC X(04).
           03  F02-51                  PIC X(30).
           03  F02-6                   PIC X(01).
           03  F02-7                   PIC 9(03).
           03  F02-8                   PIC X.

       01  DISPLAY-LIST2.
           03  FILLER PIC X(20) VALUE "010 020 030 040 050 ".
           03  FILLER PIC X(16) VALUE "051 060 070 080.".

       01  DISPLAY-BUF3                              VALUE SPACES.
           03  F03-1                   PIC X(15).
           03  F03-2                   PIC ZZZZZ9.99.

       01  DISPLAY-LIST3.
           03  FILLER PIC X(8) VALUE "010 020.".

       01  DISPLAY-BUF-BR             VALUE SPACES.
           03  FBR-1                   PIC X(25).
           03  FBR-2                   PIC XX.
           03  FBR-21                  PIC X(30).
           03  FBR-3                   PIC X(6).
           03  FBR-4                   PIC X.
           03  FBR-5                   PIC X.
           03  FBR-6                   PIC X.
           03  FBR-7                   PIC X.
           03  FBR-8X.
               05  FBR-8               PIC 9999.
               05  FBR-81              PIC 9(7).
           03  FBR-9X.
               05  FBR-9               PIC 9999.
               05  FBR-91              PIC 9(7).
           03  FBR-10                  PIC X.
           03  FBR-11                  PIC X.
           03  FBR-12                  PIC X.
    
       01  DISPLAY-LIST-BR.
           03  FILLER PIC X(56) VALUE
           "010 020 021 030 040 050 060 070 080 081 090 091 100 110 ".
           03  FILLER PIC X(4) VALUE
           "120.".

       01  DISPLAY-BUF-PS              VALUE SPACES.
           03  F01-PS                  PIC X(35).
           03  F02-PS                  PIC X.

       01  DISPLAY-LIST-PS.
           03  FILLER                  PIC X(08)  VALUE "010 020.".

      *-----------------------------------------------------------------

      *=================================================================
      *
      *    E L E M E N T - S P E C I F I C A T I O N S
      *
      *=================================================================
       01  SPEC-TABLE-STD.
      *-------------------------------------- 1. SOURCE CODE DESCRIPTION
           03  FILLER PIC X(8) VALUE "E  A350 ".

      *-------------------------------------- 2. MEMO REQUIRED
           03  FILLER PIC X(8) VALUE "E  A010 ".

      *-------------------------------------- 3. SUPERVISOR ONLY
           03  FILLER PIC X(8) VALUE "E  A010 ".

      *-------------------------------------- 4. WORKED
           03  FILLER PIC X(8) VALUE "E  A010 ".

      *-------------------------------------- 5. RESULT CODE REQUIRED
           03  FILLER PIC X(8) VALUE "E  A050 ".

      *-------------------------------------- 6. RESCHEDULE REQUIRED
           03  FILLER PIC X(8) VALUE "E  A010 ".

      *-------------------------------------- 7. RESCHEDULE TIME
           03  FILLER PIC X(8) VALUE "ENNN030 ".

      *---------------------------------------8. ACTIVITY REQUIRED
           03  FILLER PIC X(8) VALUE "E  A010 ".

      *-----------------------------------------------------------------

       01  SPEC-TABLE-STD2            REDEFINES SPEC-TABLE-STD.
           03  SPEC-REC-STD            OCCURS 8 TIMES.
               05  SPEC-STD            PIC X(07).
               05  CHFG-STD            PIC X(01).

      *=================================================================
      *
      *    M A X
      *
      *=================================================================
       01  MAX                         PIC 9(02)     VALUE 1.
       01  MAX-STD                     PIC 9(02)     VALUE 1.
       01  MAX-AT                      PIC 9(02)     VALUE 5.

       01  MAX-BR                      PIC 9(02)     VALUE 12.
       01  MAX-BR-MULTIPLE             PIC 9(02)     VALUE 4.
       01  MAX-BR-TYPE-LC              PIC 9(02)     VALUE 5.

       01  MAX-EA                      PIC 9(02)     VALUE 3.
       01  MAX-FC                      PIC 9(02)     VALUE 2.
       01  MAX-RE                      PIC 9(02)     VALUE 8.
       01  MAX-PS                      PIC 9(02)     VALUE 2.
       01  MAX-PO                      PIC 9(02)     VALUE 2.

      ******************************************************************
      *
      *    C O P Y - L I B R A R Y - W O R K E R S
      *
      ******************************************************************
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/PHON400.CPY".
       COPY "LIBGB/TSTCANIDW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBWI/CDSCANW.CPY".

       01  CDMAIN-WINDOW-HANDLE  PIC X(10).
       COPY "LIBGB/CONAME_EXT.CPY".

       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBMN/CNMAIN_DEF.CPY".
       COPY "LIBMN/EACDMN_DEF.CPY".
       COPY "LIBMN/PSCDMN_DEF.CPY".
       COPY "LIBMN/XLMAIN_DEF.CPY".
       COPY "LIBMN/LVMAIN_DEF.CPY".
       COPY "LIBMN/COMAIN_DEF.CPY".
       COPY "LIBMN/ACMAIN_DEF.CPY".
       COPY "LIBMN/A2MAIN_DEF.CPY".
       COPY "LIBMN/ATMAIN_DEF.CPY".
       COPY "LIBMN/CDBWMN_DEF.CPY".
       COPY "LIBMN/CDBRMN_DEF.CPY".
       COPY "LIBMN/CDBSMN_DEF.CPY".
       COPY "LIBMN/CHMAIN_DEF.CPY".
       COPY "LIBMN/CLMAIN_DEF.CPY".
       COPY "LIBMN/DFCDMN_DEF.CPY".
       COPY "LIBMN/FCMAIN_DEF.CPY".
       COPY "LIBMN/FUMAIN_DEF.CPY".
       COPY "LIBMN/IDMAIN_DEF.CPY".
       COPY "LIBMN/JTMAIN_DEF.CPY".
       COPY "LIBMN/LOMAIN_DEF.CPY".
       COPY "LIBMN/LSMAIN_DEF.CPY".
       COPY "LIBMN/CDORMN_DEF.CPY".
       COPY "LIBMN/PCMAIN_DEF.CPY".
       COPY "LIBMN/PHMAIN_DEF.CPY".
       COPY "LIBMN/PLMAIN_DEF.CPY".
       COPY "LIBMN/POMAIN_DEF.CPY".
       COPY "LIBMN/PUMAIN_DEF.CPY".
       COPY "LIBMN/PXMAIN_DEF.CPY".
       COPY "LIBMN/RCCDMN_DEF.CPY".
       COPY "LIBMN/REMAIN_DEF.CPY".
       COPY "LIBMN/SRMAIN_DEF.CPY".
       COPY "LIBMN/TSMAIN_DEF.CPY".
       COPY "LIBMN/UWMAIN_DEF.CPY".
       COPY "LIBMN/FZMAIN_DEF.CPY".
       COPY "LIBMN/CNMAIN_WKS.CPY".
       COPY "LIBMN/EACDMN_WKS.CPY".
       COPY "LIBMN/XLMAIN_WKS.CPY".
       COPY "LIBMN/LVMAIN_WKS.CPY".
       COPY "LIBMN/COMAIN_WKS.CPY".
       COPY "LIBMN/ACMAIN_WKS.CPY".
       COPY "LIBMN/A2MAIN_WKS.CPY".
       COPY "LIBMN/ATMAIN_WKS.CPY".
       COPY "LIBMN/CDBWMN_WKS.CPY".
       COPY "LIBMN/CDBRMN_WKS.CPY".
       COPY "LIBMN/CDBSMN_WKS.CPY".
       COPY "LIBMN/CHMAIN_WKS.CPY".
       COPY "LIBMN/CLMAIN_WKS.CPY".
       COPY "LIBMN/DFCDMN_WKS.CPY".
       COPY "LIBMN/FCMAIN_WKS.CPY".
       COPY "LIBMN/FUMAIN_WKS.CPY".
       COPY "LIBMN/IDMAIN_WKS.CPY".
       COPY "LIBMN/JTMAIN_WKS.CPY".
       COPY "LIBMN/LOMAIN_WKS.CPY".
       COPY "LIBMN/LSMAIN_WKS.CPY".
       COPY "LIBMN/CDORMN_WKS.CPY".
       COPY "LIBMN/PCMAIN_WKS.CPY".
       COPY "LIBMN/PHMAIN_WKS.CPY".
       COPY "LIBMN/PLMAIN_WKS.CPY".
       COPY "LIBMN/POMAIN_WKS.CPY".
       COPY "LIBMN/PSCDMN_WKS.CPY".
       COPY "LIBMN/PUMAIN_WKS.CPY".
       COPY "LIBMN/PXMAIN_WKS.CPY".
       COPY "LIBMN/RCCDMN_WKS.CPY".
       COPY "LIBMN/REMAIN_WKS.CPY".
       COPY "LIBMN/SRMAIN_WKS.CPY".
       COPY "LIBMN/TSMAIN_WKS.CPY".
       COPY "LIBMN/UWMAIN_WKS.CPY".
       COPY "LIBMN/FZMAIN_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".

      *=================================================================
      *     L I N K A G E - S E C T I O N
      *=================================================================
       COPY "LIBGB/GETFMW.CPY".
       01  CDMAIN-TYPE                 PIC XX.
       01  CHANGE-BRNO-FG              PIC X.

       SCREEN SECTION.
       COPY "LIBMN/CNMAIN_SCN.CPY".
       COPY "LIBMN/EACDMN_SCN.CPY".
       COPY "LIBMN/XLMAIN_SCN.CPY".
       COPY "LIBMN/LVMAIN_SCN.CPY".
       COPY "LIBMN/COMAIN_SCN.CPY".
       COPY "LIBMN/ACMAIN_SCN.CPY".
       COPY "LIBMN/A2MAIN_SCN.CPY".
       COPY "LIBMN/ATMAIN_SCN.CPY".
       COPY "LIBMN/CDBWMN_SCN.CPY".
       COPY "LIBMN/CDBRMN_SCN.CPY".
       COPY "LIBMN/CDBSMN_SCN.CPY".
       COPY "LIBMN/CHMAIN_SCN.CPY".
       COPY "LIBMN/CLMAIN_SCN.CPY".
       COPY "LIBMN/DFCDMN_SCN.CPY".
       COPY "LIBMN/FCMAIN_SCN.CPY".
       COPY "LIBMN/FUMAIN_SCN.CPY".
       COPY "LIBMN/IDMAIN_SCN.CPY".
       COPY "LIBMN/JTMAIN_SCN.CPY".
       COPY "LIBMN/LOMAIN_SCN.CPY".
       COPY "LIBMN/LSMAIN_SCN.CPY".
       COPY "LIBMN/CDORMN_SCN.CPY".
       COPY "LIBMN/PCMAIN_SCN.CPY".
       COPY "LIBMN/PHMAIN_SCN.CPY".
       COPY "LIBMN/PLMAIN_SCN.CPY".
       COPY "LIBMN/POMAIN_SCN.CPY".
       COPY "LIBMN/PSCDMN_SCN.CPY".
       COPY "LIBMN/PUMAIN_SCN.CPY".
       COPY "LIBMN/PXMAIN_SCN.CPY".
       COPY "LIBMN/RCCDMN_SCN.CPY".
       COPY "LIBMN/REMAIN_SCN.CPY".
       COPY "LIBMN/SRMAIN_SCN.CPY".
       COPY "LIBMN/TSMAIN_SCN.CPY".
       COPY "LIBMN/UWMAIN_SCN.CPY".
       COPY "LIBMN/FZMAIN_SCN.CPY".

      ******************************************************************
      *
      *    P R O C E D U R E - D I V I S I O N
      *
      ******************************************************************
       PROCEDURE DIVISION
            USING FORM-PATHNAME EXIT-PATHNAME
                  CDMAIN-TYPE CHANGE-BRNO-FG.

      ******************************************************************
      *
      *    M A I N - P R O G R A M - M O D U L E
      *
      ******************************************************************
       MAIN-PROGRAM SECTION.
           PERFORM SQL-CONNECT.
      D    STOP MESS.

           MOVE FORM-PATHNAME TO FORM-PATH.

           PERFORM ACCESS-CODE-TYPE.
           IF ( STAT NOT = "00" )
              GO TO END-PROGRAM.

      * HAD TO USE FLOATING BECAUSE I AM NOT EXITING THE MENU, NEEDS TO GO
      * OVERTOP OF IT
           DISPLAY FLOATING GRAPHICAL WINDOW
                   SIZE 80 + VDU-WIN-W-MOD
                   LINES 24 + VDU-WIN-H-MOD
           COLOR IS VDU-WCOLOR-TCLP,CONTROL VALUE IS VDU-WCTRL-TCLP
                   CELL SIZE IS LABEL FONT
                   BLANK SCREEN
                   HANDLE IS CDMAIN-WINDOW-HANDLE.

           MOVE EXT-CONAME-DAILY-PASSWORD TO CHAN.

      * THIS MOVE IS TO PREVENT PROBLEMS WITH SCAN WINDOWS WHEN CALLED
      * FROM THE INTERRUPT. (F12 WINDOW DOES NOT CANCEL PROPERLY SO
      * FIRST-TIME IS NOT RESET TO DEFAULT VALUE. MJD 030696.
           MOVE ZEROES TO FIRST-TIME.

       MAIN-MODULE.
           PERFORM FORM-RESET.

           MOVE "Y" TO VDU-WIMENU-SELECTION.

           IF ( FIRST-TIME = ZEROES )
              MOVE 1 TO FIRST-TIME.

           PERFORM REC-CD-MODULE.

           IF ( IO-TYPE = "END" ) GO TO EXIT-MAIN-PROGRAM.

           PERFORM ENTRY-MODULE.

           IF ( IO-TYPE = "RWR" ) PERFORM WRITE-MODULE.
           IF ( IO-TYPE = "DEL" ) PERFORM DELETE-MODULE.

           GO TO MAIN-MODULE.

       EXIT-MAIN-PROGRAM.
           GO TO END-PROGRAM.


      ******************************************************************
       REC-CD-MODULE SECTION.
      ******************************************************************
       REC-CD-MODULE-NO.

           MOVE 0 TO NEW-FLAG.
           MOVE SPACES TO IO-TYPE.

       GET-CODE-ENTRY.

           MOVE SCR-PROMPT-1 TO HOLD-SCR-PROMPT.
           PERFORM SEND-SCREEN-PROMPT.

           IF ( CDMAIN-TYPE = "BR" )
              MOVE "E  A050REFCD." TO VDU
           ELSE
           IF ( CDMAIN-TYPE = "BS" )
              MOVE "E  A050REFNO." TO VDU
           ELSE
           IF ( CDMAIN-TYPE = "BW" OR "FU" OR "LO" OR "OR" OR "A2")
              MOVE "ER A040SOURCE." TO VDU
           ELSE
           IF ( CDMAIN-TYPE = "CL" )
              MOVE "E  A040GROUPID." TO VDU
           ELSE
           IF ( CDMAIN-TYPE = "EA" )
              MOVE "ER A010GROUP-ID." TO VDU
           ELSE
           IF ( CDMAIN-TYPE = "FC" OR "PC" OR "PX")
              MOVE "ER A010SOURCE." TO VDU
           ELSE
           IF ( CDMAIN-TYPE = "JT" OR "PS" )
              MOVE "E  A050SOURCE." TO VDU
           ELSE
           IF ( CDMAIN-TYPE = "LV" )
              MOVE "ENNN010LEVEL." TO VDU
           ELSE
           IF ( CDMAIN-TYPE = "PL" )
              MOVE "EN N050POOL." TO VDU
           ELSE
           IF ( CDMAIN-TYPE = "RC" )
              MOVE "E  A080SOURCE." TO VDU
           ELSE
              MOVE "ER A030SOURCE." TO VDU.

           PERFORM SCREENIO.

           IF ( VDUSTATUS = "1<" )
              IF ( CDMAIN-TYPE NOT = "EA" )
                   PERFORM CODE-SCAN
                   IF ( IO-FG NOT = 0 )
                       GO TO GET-CODE-ENTRY
                   ELSE
                      MOVE "00" TO VDUSTATUS
                   END-IF
              ELSE
                   MOVE SCR-PROMPT-3 TO HOLD-SCR-PROMPT
                   PERFORM SEND-SCREEN-PROMPT
                   MOVE "R  A010020." TO VDU
                   PERFORM SCREENIO
                   MOVE VDUBUF TO CD-EA-CQ-FG
                   IF ( NOT ( CD-EA-CQ-FG = "Y" OR "N" ) )
                       MOVE "MUST ENTER 'Y' OR 'N' !!!" TO MESS
                       PERFORM SEND-MESS
                       MOVE "S  A010020." TO VDU
                       MOVE SPACES TO VDUBUF
                       PERFORM SCREENIO
                       GO TO GET-CODE-ENTRY
                   ELSE
                       PERFORM CODE-SCAN
                       IF ( IO-FG NOT = 0 )
                          GO TO GET-CODE-ENTRY
                       ELSE
                          MOVE "00" TO VDUSTATUS.

           IF ( VDUSTATUS = "1>" )
              MOVE "END" TO IO-TYPE
              GO TO END-REC-CD-MODULE.

           IF ( VDUSTATUS NOT = "00" )
              GO TO GET-CODE-ENTRY.
 
           IF ( CDMAIN-TYPE = "PO"  OR "TS")
              MOVE VDUBUF TO CD-CODE TEST-POCD
              IF ( TEST-POCD-OPEN )
                 MOVE "CODE CANNOT BE SPACES" TO MESS
                 PERFORM SEND-MESS
                 GO TO GET-CODE-ENTRY
              ELSE
                 IF ( CDMAIN-TYPE = "PO" )
                    IF ( TEST-POCD1 = "*" )
                       MOVE "INVALID CODE" TO MESS
                       PERFORM SEND-MESS
                       GO TO GET-CODE-ENTRY.

           IF ( CDMAIN-TYPE = "PL" OR "LV")
              IF ( VDUNUM0 = 0 )
                 GO TO GET-CODE-ENTRY.

           IF NOT ( CDMAIN-TYPE = "PL" OR "LV" ) AND
              (     VDUBUF = SPACES               )
              GO TO GET-CODE-ENTRY.

      * HOLD-CD1-KEY - SETTING HOLD FIELDS FOR THE CD1-KEY BASED
      *                ON WHICH TYPE (CD-TYPE|CODE-TYPE)

           IF ( CDMAIN-TYPE = "LV" )
              MOVE CDMAIN-TYPE TO HOLD-CD-TYPE      CD-TYPE
              MOVE VDUNUM0     TO HOLD-CD-LV-LEVEL  CD-LV-LEVEL
              MOVE SPACES      TO HOLD-CD-LV-FILLER CD-LV-FILLER
           ELSE
           IF ( CDMAIN-TYPE = "PL" )
              MOVE CDMAIN-TYPE TO HOLD-CD-TYPE       CD-TYPE
              MOVE VDUNUM0     TO HOLD-CD-PL-POOLID  CD-PL-POOLID
              MOVE SPACES      TO HOLD-CD-PL-FILLER  CD-PL-FILLER
           ELSE
              MOVE CDMAIN-TYPE TO HOLD-CD-TYPE     CD-TYPE
              MOVE VDUBUF      TO HOLD-CD-CODE     CD-CODE.

      * CHECK HOLD-CD-BR-KEY TO MAKE SURE IF VALID

           IF CDMAIN-TYPE = "BR"
              MOVE HOLD-CD-CODE TO TEST-REFCD
              IF (( TEST-REFCD-BYTE(1) = SPACES )  OR
                  ( TEST-REFCD-BYTE(2) = SPACES )  OR
                  ( TEST-REFCD-BYTE(3) = SPACES )  OR
                  ( TEST-REFCD-BYTE(4) = SPACES ))
              MOVE "SPACES NOT ALLOWED IN 1ST 4 POSITIONS" TO MESS
              PERFORM SEND-MESS
              GO TO GET-CODE-ENTRY.

      * CHECK HOLD-CD-CL-KEY TO MAKE SURE IF VALID

           IF ( CDMAIN-TYPE NOT = "CL" )
              GO TO VERIFY-HOLD-CD-PO-KEY.

           MOVE HOLD-CD-CODE TO TCAN-WORD.
           MOVE "Y" TO TCAN-SKIP-CD-VERIFY.
           PERFORM OPEN-CD1-FILE.
           PERFORM TEST-CAN-GROUP.
           PERFORM CLOSE-CD1-FILE.
           IF TCAN-ERR NOT = 0
              MOVE "INVALID CODE" TO MESS
              PERFORM SEND-MESS
              GO TO REC-CD-MODULE-NO.

           MOVE "S  A040GROUPID." TO VDU.
           MOVE TCAN-WORD TO VDUBUF CD-CODE.
           PERFORM SCREENIO.

           IF TCAN-WORD NUMERIC
              MOVE TCAN-NUMB TO BR-NO
              PERFORM GET-BR
              IF IO-FG NOT = 0
                 STRING "BRANCH " TCAN-WORD " DOES NOT EXIST"
                        DELIMITED BY SIZE INTO MESS
                 PERFORM SEND-MESS
                 GO TO REC-CD-MODULE-NO.

       VERIFY-HOLD-CD-PO-KEY.

      * DONT ALLOW 'BT' AS PAYOFF CODE - IT IDENTIFIES TRANSFERRED ACCTS
           IF ( HOLD-CD-TYPE = "PO" ) AND ( HOLD-CD-CODE = "BT" )
              MOVE "'BT' RESERVED FOR BRANCH TRANSFERS" TO MESS
              PERFORM SEND-MESS
              GO TO GET-CODE-ENTRY.

      * CHECK HOLD-CD-PX-KEY TO MAKE SURE IF VALID
           IF ( CDMAIN-TYPE = "PX"            ) AND
              (( HOLD-CD-CODE NOT = "P" )  AND
               ( HOLD-CD-CODE NOT = "2" )  AND
               ( HOLD-CD-CODE NOT = "3" )  AND
               ( HOLD-CD-CODE NOT = "4" )  AND
               ( HOLD-CD-CODE NOT = "5" ))
              GO TO GET-CODE-ENTRY.

      * DONT ALLOW 'BF' AS A SOURCE CODE - IT IDENTIFIES CONVERTED ACCTS
           IF ( HOLD-CD-TYPE = "SR" ) AND ( HOLD-CD-CODE = "BF" )
              MOVE "INVALID CODE" TO MESS
              PERFORM SEND-MESS
              GO TO GET-CODE-ENTRY.

       READ-CDFILE.

           MOVE SPACES TO HOLD-SCR-PROMPT.
           PERFORM SEND-SCREEN-PROMPT.

           MOVE HOLD-CD1-KEY TO CD1-KEY.
           PERFORM OPEN-CD1-FILE.
           PERFORM READ-CD1-FILE.
           PERFORM CLOSE-CD1-FILE.
           IF ( IO-FG = 0 )
              MOVE CD-REC TO ORIGINAL-CD-REC
              PERFORM DISPLAY-FIELDS
            ELSE
              PERFORM NEW-RECORD-SETUP.

       END-REC-CD-MODULE.
           EXIT.

      ******************************************************************
       CODE-SCAN SECTION.
      ******************************************************************

           MOVE " " TO CDSCAN-WINDOW-BUF.

           MOVE CDMAIN-TYPE TO CDSCAN-TYPE.
           MOVE CD-EA-CQ-FG TO CDSCAN-EA-CQ-FG.

           MOVE "WI/CDSCAN" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH CDSCAN-BUF EXIT-PATHNAME.
           CANCEL FORM-PROGX.
           IF ( CDSCAN-STATUS NOT = "00" )
              MOVE 9 TO IO-FG
              GO TO EXIT-CODE-SCAN.

           IF (( CDMAIN-TYPE = "LV") AND (CDSCAN-LV-LEVEL = 0)) OR
              (( CDMAIN-TYPE = "PL") AND (CDSCAN-PL-POOLID = 0)) 
              MOVE 9 TO IO-FG
              GO TO EXIT-CODE-SCAN.

      * EVERY OTHER CODE EXCEPT THE 2 ABOVE JUST USE CDSCAN-CODE
           IF CDSCAN-CODE = SPACES
              MOVE 9 TO IO-FG
              GO TO EXIT-CODE-SCAN.

           MOVE 0 TO IO-FG.

           IF ( CDMAIN-TYPE = "BR" )
              MOVE "S  A050REFCD."   TO VDU
              MOVE CDSCAN-CODE       TO VDUBUF
           ELSE
           IF ( CDMAIN-TYPE = "BS" )
              MOVE "S  A050REFNO."   TO VDU
              MOVE CDSCAN-CODE       TO VDUBUF
           ELSE
           IF ( CDMAIN-TYPE = "CL" )
              MOVE "S  A040GROUPID." TO VDU
              MOVE CDSCAN-CODE       TO VDUBUF
           ELSE
           IF ( CDMAIN-TYPE = "EA" )
              MOVE "S  A060GROUP-ID." TO VDU
              MOVE CDSCAN-CODE        TO VDUBUF 
           ELSE
           IF ( CDMAIN-TYPE = "FC" )
              MOVE "S  A010SOURCE."   TO VDU
              MOVE CDSCAN-CODE        TO VDUBUF 
           ELSE
           IF ( CDMAIN-TYPE = "JT" OR "PS")
              MOVE "S  A050SOURCE."   TO VDU
              MOVE CDSCAN-CODE        TO VDUBUF
           ELSE
           IF ( CDMAIN-TYPE = "LV" )
              MOVE "SNNN010LEVEL."         TO VDU
              MOVE CDSCAN-LV-LEVEL         TO VDUNUM0
           ELSE
           IF ( CDMAIN-TYPE = "PL" )
              MOVE "SNNN050POOL."          TO VDU
              MOVE CDSCAN-PL-POOLID        TO VDUNUM0
           ELSE
           IF ( CDMAIN-TYPE = "RC" )
              MOVE "S  A080SOURCE."     TO VDU
              MOVE CDSCAN-CODE          TO VDUBUF
           ELSE
              MOVE "S  A040SOURCE."        TO VDU
              MOVE CDSCAN-CODE             TO VDUBUF.

           PERFORM SCREENIO.

           IF ( CDMAIN-TYPE = "LV" )
              MOVE CDSCAN-LV-LEVEL         TO VDUNUM0
           ELSE
           IF ( CDMAIN-TYPE = "PL" )
              MOVE CDSCAN-PL-POOLID        TO VDUNUM0
           ELSE
           MOVE CDSCAN-CODE                TO VDUBUF.

       EXIT-CODE-SCAN.
           EXIT.

      ******************************************************************
      *
      *    E N T R Y - M O D U L E
      *
      *-----------------------------------------------------------------
      *    NEW-FLAG = 0 - MODIFY EXISTING RECORD
      *               1 - INITIAL NEW RECORD ENTRY
      *               2 - MODIFY NEW RECORD
      ******************************************************************
       ENTRY-MODULE SECTION.

           MOVE SPACES TO CHFG-STD(4).

           IF ( CDMAIN-TYPE = "AT" ) MOVE MAX-AT  TO MAX
           ELSE
           IF ( CDMAIN-TYPE = "BR" ) MOVE MAX-BR  TO MAX
           ELSE
           IF ( CDMAIN-TYPE = "BR" AND CD-BR-FILE-TYPE = "LC")
              MOVE MAX-BR-TYPE-LC TO MAX
           ELSE
           IF ( CDMAIN-TYPE = "BR" AND CD-BR-MULTIPLE-REFCDS = "Y")
                 MOVE MAX-BR-MULTIPLE TO MAX
           ELSE
           IF ( CDMAIN-TYPE = "FC" ) MOVE MAX-FC  TO MAX
           ELSE
           IF ( CDMAIN-TYPE = "RE" ) MOVE MAX-RE  TO MAX
           ELSE
           IF ( CDMAIN-TYPE = "EA" ) MOVE MAX-EA  TO MAX
           ELSE
           IF ( CDMAIN-TYPE = "PS" ) MOVE MAX-PS  TO MAX
           ELSE
           IF ( CDMAIN-TYPE = "PO" ) MOVE MAX-PO  TO MAX
           ELSE
                                     MOVE MAX-STD TO MAX.

           MOVE SPACES TO IO-TYPE.
           MOVE 1      TO ONE.

       CREATE-ROUTINE.

           IF ( CDMAIN-TYPE = "BR" AND CD-BR-FILE-TYPE = "LC")
              MOVE MAX-BR-TYPE-LC TO MAX
           ELSE
           IF ( CDMAIN-TYPE = "BR" AND CD-BR-MULTIPLE-REFCDS = "Y")
              MOVE MAX-BR-MULTIPLE TO MAX
           ELSE
           IF CDMAIN-TYPE = "BR"
              MOVE MAX-BR TO MAX.

           IF ( NEW-FLAG NOT = 1 )
              GO TO MODIFY-ROUTINE.

           ADD ONE TO ELE.

           IF ELE < 1
              MOVE 1 TO ELE ONE.
           IF ELE NOT > MAX
              GO TO ENTER-ELE.

           MOVE 2 TO NEW-FLAG.

      *-----------------------------------------------------------------
       MODIFY-ROUTINE.

           MOVE "R RA060SEL." TO VDU.
           PERFORM SCREENIO.

           IF ( F1-KEY ) MOVE "CLR" TO IO-TYPE
                         GO TO END-ENTRY-MODULE.

           IF NOT ( ENTER-KEY ) GO TO MODIFY-ROUTINE.

           MOVE VDUBUF TO ACTION.
           CALL "C$JUSTIFY" USING ACTION "R".

           IF ( ACTION = SPACES )
              IF ( CDMAIN-TYPE = "BR"               ) AND
                 ( CD-BR-FILE-TYPE       NOT = "LC" ) AND
                 ( CD-BR-MULTIPLE-REFCDS NOT = "Y"  )
                 IF CD-BR-LBOX-OPTION = CD-BR-ALLOT-OPTION
                    MOVE "LOCK BOX OPTION CAN'T = ALLOTMENT OPTION"
                                                            TO MESS
                    PERFORM SEND-MESS
                    GO TO MODIFY-ROUTINE
                 END-IF
              END-IF

              IF (CDMAIN-TYPE = "BR" AND CD-BR-FILE-TYPE = "B" AND
                            CD-BR-MULTIPLE-REFCDS NOT = "Y")
                  MOVE CD-CODE TO TEST-REFNO
                  IF NOT (TEST-REF-LAST2 = "07" OR "11"
                          OR "12" OR "13" OR "28")
                     MOVE "UNLESS BD, LAST 2 OF REFCD SHOULD BE CHAPTER"
                         TO MESS
                     PERFORM SEND-MESS
                  END-IF
              END-IF

              MOVE "RWR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.

           IF ( ACTION = "DELETE" )
              IF ( CD-TYPE     = "LV" ) AND
                 ( CD-LV-LEVEL = 9    )
                 MOVE "LEVEL 9 CAN'T BE DELETED" TO MESS
                 PERFORM SEND-MESS
                 GO TO MODIFY-ROUTINE
              ELSE
                 MOVE "DEL" TO IO-TYPE
                 GO TO END-ENTRY-MODULE.

           INSPECT ACTION REPLACING ALL SPACES BY ZEROS.

           IF ( CHNO NOT NUMERIC ) GO TO MODIFY-ROUTINE.
           IF ( CHNO   = ZEROES  ) GO TO MODIFY-ROUTINE.
           IF ( CHNO   > MAX     ) GO TO MODIFY-ROUTINE.

           MOVE CHNO TO ELE.

           IF ( CD-TYPE     = "LV" ) AND
              ( CD-LV-LEVEL = "Z"  )
              MOVE "LEVEL 'Z' CAN'T BE CHANGED" TO MESS
              PERFORM SEND-MESS
              GO TO MODIFY-ROUTINE.

      *-----------------------------------------------------------------
       ENTER-ELE.
           IF ( CD-TYPE     = "LV" ) AND
              ( CD-LV-LEVEL = 9    )
              MOVE "S  A200010." TO VDU
              MOVE "BRANCH"      TO VDUBUF
              PERFORM SCREENIO
              MOVE "BRANCH"      TO VDUBUF
              GO TO GOTO-FIELD.

           IF ( CD-TYPE NOT = "BR"     ) AND
              ( CHKEY NOT     = ZEROES ) AND
              ( CHFG-STD(ELE) = SPACES ) AND
              ( NEW-FLAG  NOT = 1      )
              GO TO MODIFY-ROUTINE.

           IF ( CD-TYPE NOT = "BR"  ) AND
              ( CHFG-STD(ELE) = "N" )
              IF ( NEW-FLAG = 1 )
                 GO TO CREATE-ROUTINE
              ELSE
              IF ( CHKEY NOT = CHAN )
                 GO TO MODIFY-ROUTINE.

           IF ( CDMAIN-TYPE = "BR" )
              IF ELE < 8
                 MOVE "E  A000 " TO VDU
              ELSE
                  IF ELE = 8
                     MOVE "ENNN040 " TO VDU
                  ELSE
                     IF ELE = 9
                        MOVE "ENNN040 " TO VDU
                     ELSE
                        IF ELE = 10 OR ELE = 11
                           MOVE "E  A000 " TO VDU
                        END-IF
                     END-IF
                 END-IF
              END-IF
           ELSE
           IF ( CDMAIN-TYPE = "FC" ) AND ( ELE = 2 )
              MOVE "ENNN062 " TO VDU
           ELSE
           IF ( CDMAIN-TYPE = "PS" ) AND ( ELE = 2 )
              MOVE "E  A010 " TO VDU
           ELSE
           IF ( CDMAIN-TYPE = "PO" ) AND ( ELE = 2 )
              MOVE "ENNN020 " TO VDU
           ELSE
              MOVE SPEC-STD(ELE) TO VDU.

           MOVE ELE    TO VDUROW.
           MOVE ZEROES TO VDUCOL.
           MOVE "."    TO VDUDECIMAL.
           PERFORM SCREENIO.

      *-----------------------------------------------------------------
       TEST-F1-KEY.

           IF NOT ( F1-KEY ) GO TO TEST-UP-KEY.

           MOVE "CLR" TO IO-TYPE.
           GO TO END-ENTRY-MODULE.

      *-----------------------------------------------------------------
       TEST-UP-KEY.

           IF NOT ( UP-KEY ) GO TO TEST-DOWN-KEY.

           IF ( NEW-FLAG = 1 )
              MOVE -1 TO ONE
              GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       TEST-DOWN-KEY.

           IF NOT ( DOWN-KEY ) GO TO TEST-F6-KEY.

           MOVE 1 TO ONE.
           GO TO GOTO-FIELD.

      *-----------------------------------------------------------------
       TEST-F6-KEY.

      *-----------------------------------------------------------------
       TEST-ENTER-KEY.

           IF NOT ( ENTER-KEY ) GO TO ENTER-ELE.
           MOVE 1 TO ONE.

      *-----------------------------------------------------------------
       GOTO-FIELD.

              GO TO F-01 F-02 F-03 F-04 F-05 F-06 F-07 F-08 F-09
                    F-10 F-11 F-12
                    DEPENDING ON ELE.

      *=================================================================
      *
      *    S E T - R E C O R D - E L E M E N T S
      *
      *=================================================================
      *    F-01 IS POPULATING THE CD-DESC (35 BYTES) WHICH IS STANDARD
      *    IN ALL CODE TYPE REDEFINITIONS (CD-??-DESC) THAT ARE
      *    USING CDMAIN FOR MAINTENANCE (USING DIFFERENT FM SCREEN).
      *=================================================================
       F-01.

           MOVE VDUBUF TO CD-DESC.
           IF ( CDMAIN-TYPE = "CO" )
              GO TO F-TRW.

           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-02.
      D    STOP MESS.
           IF CDMAIN-TYPE = "BR"
              IF VDUBUF = " "
                 GO TO ENTER-ELE
              ELSE
                 MOVE VDUBUF TO CD-BR-FILE-TYPE
                 IF NOT (CD-BR-FILE-TYPE = "S" OR "LC" OR "B"
                                                OR "C" OR "P")
                    MOVE "ENTER ONLY 'S', 'B', 'C', 'P' OR 'LC'"
                                                         TO MESS
                    PERFORM SEND-MESS
                    GO TO ENTER-ELE
                 ELSE
                    IF CD-CODE = "ALLOT" OR "LBOX" OR "TPAY"
                                         OR "TCOLL" OR "MGRAM"
                       IF CD-BR-FILE-TYPE NOT = "LC"
                          MOVE "MUST BE 'LC' FILE TYPE" TO MESS
                          PERFORM SEND-MESS
                          GO TO ENTER-ELE
                       END-IF
                    END-IF
                    MOVE "S  A300021." TO VDU
                    IF CD-BR-FILE-TYPE = "S"
                       MOVE "STANDARD FILE FORMAT" TO VDUBUF
                       PERFORM SCREENIO
                       GO TO CREATE-ROUTINE
                    ELSE
                    IF CD-BR-FILE-TYPE = "B"
                       MOVE "BANKRUPT FILE FORMAT" TO VDUBUF
                       PERFORM SCREENIO
                       GO TO CREATE-ROUTINE
                    ELSE
                    IF CD-BR-FILE-TYPE = "C"
                       MOVE "CASHLESS RECEIPT/DISBURSE" TO VDUBUF
                       PERFORM SCREENIO
                       GO TO CREATE-ROUTINE
                    ELSE
                    IF CD-BR-FILE-TYPE = "P"
                       MOVE "P&L FILE FORMAT" TO VDUBUF
                       PERFORM SCREENIO
                       GO TO CREATE-ROUTINE
                    ELSE
                       MOVE " LATE CHARGE ASSESS OPTION" TO VDUBUF
                       PERFORM SCREENIO
                       GO TO CREATE-ROUTINE.
           IF CDMAIN-TYPE = "FC"
              IF VDUNUM2 = 0
                 GO TO ENTER-ELE
              ELSE
                 MOVE VDUNUM2 TO CD-FC-FEEAMT
                 GO TO CREATE-ROUTINE.

           IF CDMAIN-TYPE = "AT" 
              MOVE VDUBUF TO CD-AT-MEMO-REQUIRED
              IF ( NOT (CD-AT-MEMO-REQUIRED = "Y" OR "N") )
                 MOVE "ENTER 'Y' OR 'N'" TO MESS
                 PERFORM SEND-MESS
                 GO TO ENTER-ELE.

           IF CDMAIN-TYPE = "RE"
              MOVE VDUBUF TO CD-RE-MEMO-REQUIRED
              IF ( NOT (CD-RE-MEMO-REQUIRED = "Y" OR "N") )
                 MOVE "ENTER 'Y' OR 'N'" TO MESS
                 PERFORM SEND-MESS
                 GO TO ENTER-ELE.

           IF CDMAIN-TYPE = "EA"
              MOVE VDUBUF TO CD-EA-CQ-FG
              IF ( NOT (CD-EA-CQ-FG = "Y" OR "N") )
                 MOVE "ENTER 'Y' OR 'N'" TO MESS
                 PERFORM SEND-MESS
                 GO TO ENTER-ELE
              ELSE
                 GO TO CREATE-ROUTINE.


           IF CDMAIN-TYPE = "PS"
              MOVE VDUBUF TO CD-PS-TDR-FLAG
              IF CD-PS-TDR-FLAG NOT = "Y" AND NOT = "N"
                 MOVE "ENTER 'Y' OR 'N'" TO MESS
                 PERFORM SEND-MESS
                 GO TO ENTER-ELE
              ELSE
                 GO TO CREATE-ROUTINE.

           IF CDMAIN-TYPE = "PO"
              MOVE VDUNUM0 TO CD-PO-TITLE-HOLD-DAYS.

           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-03.
           IF CDMAIN-TYPE = "BR"
              MOVE VDUBUF TO CD-BR-FILE-NAME HOLD-BR-FILE-NAME
              IF CD-BR-FILE-TYPE = "LC"
                 IF CD-BR-FILE-NAME NOT = " "
                    MOVE "MUST BE BLANK FOR FILE TYPE 'LC'" TO MESS
                    PERFORM SEND-MESS
                    GO TO ENTER-ELE
                 ELSE
                    GO TO CREATE-ROUTINE
                 END-IF
              END-IF
              IF CD-BR-FILE-TYPE NOT = "LC"
                 IF HOLD-BR-FILE-NAME-TAB(1) = " " OR
                    HOLD-BR-FILE-NAME-TAB(2) = " " OR
                    HOLD-BR-FILE-NAME-TAB(3) = " " OR
                    HOLD-BR-FILE-NAME-TAB(4) = " " OR
                    HOLD-BR-FILE-NAME-TAB(5) = " " OR
                    HOLD-BR-FILE-NAME-TAB(6) = " "
                    MOVE "SPACES ARE NOT ALLOWED" TO MESS
                    PERFORM SEND-MESS
                    GO TO ENTER-ELE
                 ELSE
                    GO TO CREATE-ROUTINE.

           IF CDMAIN-TYPE = "EA"
              MOVE VDUBUF TO CD-EA-GQ-FG
              IF ( NOT (CD-EA-GQ-FG = "Y" OR "N") )
                 MOVE "ENTER 'Y' OR 'N'" TO MESS
                 PERFORM SEND-MESS
                 GO TO ENTER-ELE
              ELSE
                 GO TO CREATE-ROUTINE.

           IF CDMAIN-TYPE = "AT" 
              MOVE VDUBUF TO CD-AT-SUPERVISOR-ONLY
              IF CD-AT-SUPERVISOR-ONLY NOT = " "
                 IF (CD-AT-SUPERVISOR-ONLY < LOWER-A
                       OR CD-AT-SUPERVISOR-ONLY > LOWER-Z)
                    STRING "PLEASE ENTER '"
                           LOWER-A
                           "' THRU '"
                           LOWER-Z
                           "'"
                           " OR SPACE"
                           DELIMITED BY SIZE INTO MESS
                    PERFORM SEND-MESS
                    GO TO ENTER-ELE.

           IF CDMAIN-TYPE = "RE" 
              MOVE VDUBUF TO CD-RE-SUPERVISOR-ONLY
              IF CD-RE-SUPERVISOR-ONLY NOT = " "
                 IF (CD-RE-SUPERVISOR-ONLY < LOWER-A
                       OR CD-RE-SUPERVISOR-ONLY > LOWER-Z)
                    STRING "PLEASE ENTER '"
                           LOWER-A
                           "' THRU '"
                           LOWER-Z
                           "'"
                           " OR SPACE"
                           DELIMITED BY SIZE INTO MESS
                    PERFORM SEND-MESS
                    GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-04.
           IF NOT (VDUBUF = "Y" OR "N")
              MOVE "ENTER 'Y' OR 'N'" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.

           IF ( CDMAIN-TYPE = "BR" )
              MOVE VDUBUF TO CD-BR-MULTIPLE-REFCDS
           ELSE
           IF ( CDMAIN-TYPE = "AT" )
              MOVE VDUBUF TO CD-AT-RESULT-REQUIRED
           ELSE
           IF ( CDMAIN-TYPE = "RE" )
              MOVE VDUBUF TO CD-RE-WORKED-ACCOUNT.

           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-05.
           IF CDMAIN-TYPE = "BR"
              IF NOT (VDUBUF = "Y" OR "N")
                 MOVE "ENTER 'Y' OR 'N'" TO MESS
                 PERFORM SEND-MESS
                 GO TO ENTER-ELE
              END-IF
              MOVE VDUBUF TO CD-BR-LC-FG
              GO TO CREATE-ROUTINE.

           IF CDMAIN-TYPE = "AT"
              MOVE VDUBUF TO CD-AT-ACTIONCD
              IF ( CD-AT-ACTIONCD NOT = " " )
                 PERFORM VERIFY-ACTION-CODE
                 IF ( IO-FG NOT = 0 )
                    MOVE "INVALID ACTION CODE" TO MESS
                    PERFORM SEND-MESS
                    GO TO ENTER-ELE.

           IF CDMAIN-TYPE = "RE"
              MOVE VDUBUF TO CD-RE-ACTIONCD
              IF ( CD-RE-ACTIONCD NOT = " " )
                 PERFORM VERIFY-ACTION-CODE
                 IF ( IO-FG NOT = 0 )
                    MOVE "INVALID ACTION CODE" TO MESS
                    PERFORM SEND-MESS
                    GO TO ENTER-ELE.

           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-06.
           IF CDMAIN-TYPE = "BR"
              IF NOT (VDUBUF = "Y" OR "N")
                 MOVE "ENTER 'Y' OR 'N'" TO MESS
                 PERFORM SEND-MESS
                 GO TO ENTER-ELE
              END-IF
              MOVE VDUBUF TO CD-BR-ALLOT-OPTION
              GO TO CREATE-ROUTINE.
           IF CDMAIN-TYPE NOT = "BR"
              MOVE VDUBUF TO CD-RE-RESCHEDULE-REQ
              IF ( NOT ( CD-RE-RESCHEDULE-REQ = "Y" OR "N" OR "A" ) )
                 MOVE "ENTER 'Y' OR 'N' OR 'A'" TO MESS
                 PERFORM SEND-MESS
                 GO TO ENTER-ELE
              END-IF
              GO TO CREATE-ROUTINE.


      *-----------------------------------------------------------------
       F-07.
           IF CDMAIN-TYPE NOT = "BR"
              MOVE VDUNUM0 TO CD-RE-RESCHEDULE-TIME
              GO TO CREATE-ROUTINE.
           IF CDMAIN-TYPE = "BR"
              IF NOT (VDUBUF = "Y" OR "N")
                 MOVE "ENTER 'Y' OR 'N'" TO MESS
                 PERFORM SEND-MESS
                 GO TO ENTER-ELE
              END-IF
              MOVE VDUBUF TO CD-BR-LBOX-OPTION
              GO TO CREATE-ROUTINE.
      *-----------------------------------------------------------------
       F-08.
           IF CDMAIN-TYPE NOT = "BR" AND CDMAIN-TYPE NOT = "RE"
              GO TO CREATE-ROUTINE.

           IF CDMAIN-TYPE = "BR"
              MOVE VDUNUM0 TO CD-BR-GL-BRANCH.

           IF CDMAIN-TYPE = "RE"
              MOVE VDUBUF TO CD-RE-ACTIVITY-REQUIRED
              IF NOT (CD-RE-ACTIVITY-REQUIRED = "Y" OR "N")
                 MOVE "ENTER 'Y' OR 'N'" TO MESS
                 PERFORM SEND-MESS
                 GO TO ENTER-ELE
              ELSE
                 GO TO CREATE-ROUTINE.

       F-081.
           MOVE "ENNN070081." TO VDU.
           PERFORM SCREENIO.
           IF UP-KEY
              GO TO ENTER-ELE.
           IF NOT (ENTER-KEY)
              GO TO F-081.
           MOVE VDUNUM0 TO CD-BR-GL-ACCT.
    
           GO TO CREATE-ROUTINE.
      *-----------------------------------------------------------------
       F-09.
           IF CDMAIN-TYPE NOT = "BR"
              GO TO CREATE-ROUTINE.

           IF CDMAIN-TYPE = "BR"
              MOVE VDUNUM0 TO CD-BR-OT-GL-BRANCH.
       F-091.
           MOVE "ENNN070091." TO VDU.
           PERFORM SCREENIO.
           IF UP-KEY
              GO TO ENTER-ELE.
           IF NOT (ENTER-KEY)
              GO TO F-091.
           MOVE VDUNUM0 TO CD-BR-OT-GL-ACCT.
    
           GO TO CREATE-ROUTINE.
      *-----------------------------------------------------------------
       F-10.
           IF CDMAIN-TYPE NOT = "BR"
              GO TO CREATE-ROUTINE.
           IF NOT (VDUBUF = "Y" OR "N")
              MOVE "ENTER 'Y' OR 'N'" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.

           MOVE VDUBUF TO CD-BR-PAYOFF-FG.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-11.
           IF CDMAIN-TYPE NOT = "BR"
              GO TO CREATE-ROUTINE.
      *Y POSTS AS 'SS' MUST BE IN REPO STATUS
      *N POST     'PA' REPO AND NON-REPO
      *R REJECT IF IN REPO ON HAND STATUS
           IF NOT (VDUBUF = "Y" OR "N" OR "R")
              MOVE "ENTER 'Y' OR 'N' OR 'R'" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.

           MOVE VDUBUF TO CD-BR-POST-SS-TRCD.

           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-12.
           IF CDMAIN-TYPE NOT = "BR"
              GO TO CREATE-ROUTINE.

           IF NOT (VDUBUF = "Y" OR "N")
              MOVE "ENTER 'Y' OR 'N'" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.

           MOVE VDUBUF TO CD-BR-COLLECT-LC.

           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-TRW.
           MOVE "E  A020020." TO VDU.
           PERFORM SCREENIO.
           MOVE VDUBUF TO CD-CO-TRW.

           IF ( VDUSTATUS = "10" )
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.

           IF ( VDUSTATUS = "1U" )
              GO TO ENTER-ELE.

           IF ( NOT (VDUSTATUS = "00" OR "1D") )
              GO TO F-TRW.

      *-----------------------------------------------------------------
       F-SECURED.
           MOVE "E  A010030." TO VDU.
           PERFORM SCREENIO.

           IF ( VDUSTATUS = "10" )
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.

           IF ( VDUSTATUS = "1U" )
              GO TO F-TRW.

           IF ( NOT (VDUSTATUS = "00" OR "1D") )
              GO TO F-SECURED.

           MOVE VDUBUF TO CD-CO-SECURED.

           IF ( NOT ( CD-CO-SECURED = "U" OR "S" OR "R" ) )
              MOVE "MUST BE 'U' 'S' OR 'R'" TO MESS
              PERFORM SEND-MESS
              GO TO F-SECURED.

      * COLLATERAL GROUP
       F-GROUP.
           MOVE "E  A010040." TO VDU.
           PERFORM SCREENIO.

           IF ( VDUSTATUS = "10" )
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.
           IF ( VDUSTATUS = "1U" )
              GO TO F-SECURED.
           IF ( NOT (VDUSTATUS = "00" OR "1D") )
              GO TO F-GROUP.

           MOVE VDUBUF TO CD-CO-GROUP.
           GO TO CREATE-ROUTINE.

       END-ENTRY-MODULE.
           EXIT.

      ******************************************************************
       WRITE-MODULE SECTION.
      ******************************************************************

           PERFORM OPEN-CD1-FILE.

           IF ( NEW-FLAG NOT = ZEROES )
              GO TO WRITE-RECORD.

           MOVE CD-REC TO CHANGED-CD-REC.

           IF ( ORIGINAL-CD-REC = CHANGED-CD-REC )
              GO TO END-WRITE-MODULE.

           PERFORM READ-CD1-FILE.

           IF ( IO-FG NOT = ZEROES )
              PERFORM UNDERGONE-CHANGE
              PERFORM RECORD-DELETED
              GO TO END-WRITE-MODULE.

           IF ( CD-REC NOT = ORIGINAL-CD-REC )
              PERFORM UNDERGONE-CHANGE
              PERFORM CHANGE-AGAIN
              GO TO END-WRITE-MODULE.

           MOVE CHANGED-CD-REC TO CD-REC.
           PERFORM REWRITE-CD1-FILE.
           GO TO END-WRITE-MODULE.

      *-----------------------------------------------------------------
       WRITE-RECORD.

           PERFORM WRITE-CD1-FILE.

           IF ( IO-FG NOT = ZEROES )
              PERFORM ALREADY-CREATED
           ELSE
              IF ( CD-TYPE = "LV" )
                 MOVE 9 TO CD-CODE
                 MOVE "BRANCH" TO CD-DESC
                 PERFORM WRITE-CD1-FILE.

       END-WRITE-MODULE.
           PERFORM CLOSE-CD1-FILE.

       EXIT-WRITE-MODULE.
           EXIT.

      ******************************************************************
       DELETE-MODULE SECTION.
      ******************************************************************

           IF ( NEW-FLAG NOT = ZEROES )
              GO TO END-DELETE-MODULE.

           PERFORM OPEN-CD1-FILE.
           PERFORM READ-CD1-FILE.
           IF ( IO-FG NOT = ZEROES )
              PERFORM UNDERGONE-CHANGE
              PERFORM RECORD-DELETED
              GO TO END-DELETE-MODULE.

           IF ( CD-REC NOT = ORIGINAL-CD-REC )
              PERFORM UNDERGONE-CHANGE
              PERFORM RECORD-CHANGED
              GO TO END-DELETE-MODULE.

           IF ( IO-TYPE = "CLR" )
              GO TO END-DELETE-MODULE.

           PERFORM DELETE-CD1-FILE.

       END-DELETE-MODULE.
           PERFORM CLOSE-CD1-FILE.

       EXIT-DELETE-MODULE.
           EXIT.

      ******************************************************************
       NEW-RECORD-SETUP SECTION.
      ******************************************************************

           MOVE 1                    TO NEW-FLAG.
           MOVE "S  A000NEW."        TO VDU.
           MOVE "*** NEW RECORD ***" TO VDUBUF.
           PERFORM SCREENIO.

           PERFORM CLEAR-CD-FILE.

           MOVE HOLD-CD1-KEY TO CD1-KEY.

           MOVE ZEROES   TO ELE.

       EXIT-NEW-RECORD-SETUP.
           EXIT.

      ******************************************************************
       DISPLAY-FIELDS SECTION.
      ******************************************************************

           MOVE SPACES TO DISPLAY-BUF.

           IF ( CDMAIN-TYPE = "BR" ) GO TO DISPLAY-FIELDS-BR.
           IF ( CDMAIN-TYPE = "AT" ) GO TO DISPLAY-FIELDS-AT.
           IF ( CDMAIN-TYPE = "CO" )
              MOVE CD-DESC TO F01
              MOVE DISPLAY-BUF TO WR-BUF
              MOVE DISPLAY-LIST TO WR-LIST
              PERFORM CALL-WRFORM
              GO TO DISPLAY-FIELDS-CO.
           IF ( CDMAIN-TYPE = "EA" ) GO TO DISPLAY-FIELDS-EA.
           IF ( CDMAIN-TYPE = "FC" ) GO TO DISPLAY-FIELDS-FC.
           IF ( CDMAIN-TYPE = "RE" ) GO TO DISPLAY-FIELDS-RE.
           IF ( CDMAIN-TYPE = "PS" ) GO TO DISPLAY-FIELDS-PS.
           IF ( CDMAIN-TYPE = "PO" ) GO TO DISPLAY-FIELDS-PO.

           GO TO DISPLAY-FIELDS-STD.

      *=================================================================
       DISPLAY-FIELDS-AT.

           MOVE CD-DESC               TO F02-1.
           MOVE CD-AT-MEMO-REQUIRED   TO F02-2.
           MOVE CD-AT-SUPERVISOR-ONLY TO F02-3.
           MOVE CD-AT-RESULT-REQUIRED TO F02-4.
           MOVE CD-AT-ACTIONCD        TO F02-5.
           PERFORM VERIFY-ACTION-CODE.
           MOVE MESS                  TO F02-51.

           MOVE DISPLAY-BUF2 TO WR-BUF.
           MOVE DISPLAY-LIST2 TO WR-LIST.
           PERFORM CALL-WRFORM.
           GO TO EXIT-DISPLAY-FIELDS.

      *=================================================================
       DISPLAY-FIELDS-CO.

           MOVE "S  A000020." TO VDU
           MOVE CD-CO-TRW     TO VDUBUF
           PERFORM SCREENIO

           MOVE "S  A000030." TO VDU
           MOVE CD-CO-SECURED TO VDUBUF
           PERFORM SCREENIO.

           MOVE "S  A000040." TO VDU
           MOVE CD-CO-GROUP   TO VDUBUF.
           PERFORM SCREENIO.

           GO TO EXIT-DISPLAY-FIELDS.

      *=================================================================
       DISPLAY-FIELDS-PO.

           MOVE "S  A035010." TO VDU.
           MOVE CD-DESC    TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "S  A000020." TO VDU.
           MOVE CD-PO-TITLE-HOLD-DAYS TO VDUBUF.
           PERFORM SCREENIO.

           GO TO EXIT-DISPLAY-FIELDS.

      *=================================================================
       DISPLAY-FIELDS-EA.

           MOVE "S  A030010." TO VDU.
           MOVE CD-DESC       TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "S  A010020." TO VDU.
           MOVE CD-EA-CQ-FG   TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "S  A010030." TO VDU.
           MOVE CD-EA-GQ-FG   TO VDUBUF.
           PERFORM SCREENIO.

           GO TO EXIT-DISPLAY-FIELDS.

      *=================================================================
       DISPLAY-FIELDS-FC.

           MOVE CD-DESC      TO F03-1.
           MOVE CD-FC-FEEAMT TO F03-2.

           MOVE DISPLAY-BUF3 TO WR-BUF.
           MOVE DISPLAY-LIST3 TO WR-LIST.
           PERFORM CALL-WRFORM.
           GO TO EXIT-DISPLAY-FIELDS.

      *=================================================================
       DISPLAY-FIELDS-RE.

           MOVE CD-DESC               TO F02-1
           MOVE CD-RE-MEMO-REQUIRED   TO F02-2
           MOVE CD-RE-SUPERVISOR-ONLY TO F02-3
           MOVE CD-RE-WORKED-ACCOUNT  TO F02-4
           MOVE CD-RE-ACTIONCD        TO F02-5
           PERFORM VERIFY-ACTION-CODE
           MOVE MESS                  TO F02-51
           MOVE CD-RE-RESCHEDULE-REQ  TO F02-6
           MOVE CD-RE-RESCHEDULE-TIME TO F02-7
           MOVE CD-RE-ACTIVITY-REQUIRED TO F02-8

           MOVE DISPLAY-BUF2 TO WR-BUF.
           MOVE DISPLAY-LIST2 TO WR-LIST.
           PERFORM CALL-WRFORM.
           GO TO EXIT-DISPLAY-FIELDS.

      *=================================================================
       DISPLAY-FIELDS-BR.
           MOVE CD-DESC                         TO FBR-1.
           MOVE CD-BR-FILE-TYPE                 TO FBR-2.
           IF CD-BR-FILE-TYPE = "S"
              MOVE "STANDARD FILE FORMAT"       TO FBR-21
           ELSE
           IF CD-BR-FILE-TYPE = "B"
              MOVE " BANKRUPT FILE FORMAT"      TO FBR-21
           ELSE
           IF CD-BR-FILE-TYPE = "C"
              MOVE " CASHLESS RECEIPT/DISBURSE" TO FBR-21
           ELSE
           IF CD-BR-FILE-TYPE = "P"
              MOVE " P&L FILE FORMAT"           TO FBR-21
           ELSE
           IF CD-BR-FILE-TYPE = "LC"
              MOVE " LATE CHARGE ASSESS OPTION" TO FBR-21.
           MOVE CD-BR-FILE-NAME                 TO FBR-3.
           MOVE CD-BR-MULTIPLE-REFCDS           TO FBR-4.
           MOVE CD-BR-LC-FG                     TO FBR-5.
           IF CD-BR-FILE-TYPE = "LC"
              MOVE " "                          TO FBR-6
                                                   FBR-7
                                                   FBR-8
                                                   FBR-8X
                                                   FBR-9
                                                   FBR-9X
           ELSE
              MOVE CD-BR-ALLOT-OPTION           TO FBR-6
              MOVE CD-BR-LBOX-OPTION            TO FBR-7
              MOVE CD-BR-GL-BRANCH              TO FBR-8
              MOVE CD-BR-GL-ACCT                TO FBR-81
              MOVE CD-BR-OT-GL-BRANCH           TO FBR-9
              MOVE CD-BR-OT-GL-ACCT             TO FBR-91
              MOVE CD-BR-PAYOFF-FG              TO FBR-10
              MOVE CD-BR-POST-SS-TRCD           TO FBR-11.
              MOVE CD-BR-COLLECT-LC             TO FBR-12.
    
           MOVE DISPLAY-BUF-BR TO WR-BUF.
           MOVE DISPLAY-LIST-BR TO WR-LIST.
           PERFORM CALL-WRFORM.
           GO TO EXIT-DISPLAY-FIELDS.

      *=================================================================
       DISPLAY-FIELDS-PS.

           MOVE CD-DESC        TO F01-PS.
           MOVE CD-PS-TDR-FLAG TO F02-PS.

           MOVE DISPLAY-BUF-PS TO WR-BUF.
           MOVE DISPLAY-LIST-PS TO WR-LIST.
           PERFORM CALL-WRFORM.
           GO TO EXIT-DISPLAY-FIELDS.

      *=================================================================
       DISPLAY-FIELDS-STD.

           MOVE CD-DESC TO F01.

           MOVE DISPLAY-BUF TO WR-BUF.
           MOVE DISPLAY-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.
           GO TO EXIT-DISPLAY-FIELDS.

       EXIT-DISPLAY-FIELDS.
           EXIT.

      ******************************************************************
       VERIFY-ACTION-CODE SECTION.
      ******************************************************************

           IF CDMAIN-TYPE = "AT"
              IF CD-AT-ACTIONCD = " "
                 MOVE " " TO MESS
                 GO TO VERIFY-ACTION-CODE-EXIT.

           IF CDMAIN-TYPE = "RE"
              IF CD-RE-ACTIONCD = " "
                 MOVE " " TO MESS
                 GO TO VERIFY-ACTION-CODE-EXIT.

           MOVE CD-REC TO CHANGED-CD-REC.

           MOVE "AC"           TO CD-TYPE.
           IF CDMAIN-TYPE = "AT"
              MOVE CD-AT-ACTIONCD TO CD-CODE
           ELSE
              MOVE CD-RE-ACTIONCD TO CD-CODE.
           PERFORM OPEN-CD1-FILE.
           PERFORM READ-CD1-FILE.
           PERFORM CLOSE-CD1-FILE.
           IF ( IO-FG NOT = 0 )
              MOVE "NOT ON FILE" TO CD-DESC.

           MOVE CD-DESC TO MESS.
           MOVE CHANGED-CD-REC TO CD-REC.

       VERIFY-ACTION-CODE-EXIT.
           EXIT.

      ******************************************************************
      *
      *    A C C E S S - C O D E - T Y P E --- ( CODE DESCRIPTION FILE )
      *
      *-----------------------------------------------------------------
      *          TYPE: AC -  ACTION CODES
      *                AT -  ACTIVITY CODES
      *                BW -  BORROWER HOUSING CODES
      *                CH -  CHARGE OFF (P&L) REASON CODE
      *                CN -  COLLECTOR ID
      *                CO -  COLLATERAL CODES
      *                DI -  DISPOSAL / HOLD COLLATERAL (REPO)
      *                EA -  EXTRACTION/ACTION GROUP ID
      *                EL -  EXTENDED LATE CHARGE INFO
      *                FU -  FOLLOW-UP CODES
      *                GC -  FEE CODES
      *                ID -  ID TYPE (KNOW YOUR CUSTOMER SCREENS)
      *                LV -  GROUP LEVELS
      *                LS -  REPO REASON CODES
      *                PL -  POOL ID CODES
      *                PO -  PAYOFF CODES
      *                PR -  PRODUCT CODES (REVOLVING)
      *                PS -  PAYMENT RESCHEDULE REASON
      *                PU -  PURPOSE CODES
      *                PX -  P&L CODES
      *                RE -  RESULT CODES
      *                RF -  TURNDOWN (REFUSAL CODES)
      *                RL -  RELATION CODES
      *                SR -  SOURCE CODES
      ******************************************************************
       ACCESS-CODE-TYPE SECTION.

           MOVE FORM-PATHNAME TO HOLD-FORM-PATHNAME FORM-PATH.

           IF ( CDMAIN-TYPE = "AC" )
              MOVE "MN/ACMAIN" TO FORM-NAM
           ELSE
           IF ( CDMAIN-TYPE = "A2" )
              MOVE "MN/A2MAIN" TO FORM-NAM
           ELSE
           IF ( CDMAIN-TYPE = "AT" )
              MOVE "MN/ATMAIN" TO FORM-NAM
           ELSE
           IF ( CDMAIN-TYPE = "BR" )
              MOVE "MN/CDBRMN" TO FORM-NAM
           ELSE
           IF ( CDMAIN-TYPE = "BS" )
              MOVE "MN/CDBSMN" TO FORM-NAM
           ELSE
           IF ( CDMAIN-TYPE = "BW" )
              MOVE "MN/CDBWMN" TO FORM-NAM
           ELSE
           IF ( CDMAIN-TYPE = "CH" )
              MOVE "MN/CHMAIN" TO FORM-NAM
           ELSE
           IF ( CDMAIN-TYPE = "CL" )
              MOVE "MN/CLMAIN" TO FORM-NAM
           ELSE
           IF ( CDMAIN-TYPE = "CO" )
              MOVE "MN/COMAIN" TO FORM-NAM
           ELSE
           IF ( CDMAIN-TYPE = "DF" )
              MOVE "MN/DFCDMN" TO FORM-NAM
           ELSE
           IF ( CDMAIN-TYPE = "EA" )
              MOVE "MN/EACDMN" TO FORM-NAM
           ELSE
           IF ( CDMAIN-TYPE = "FC" )
              MOVE "MN/FCMAIN" TO FORM-NAM
           ELSE
           IF ( CDMAIN-TYPE = "FU" )
              MOVE "MN/FUMAIN" TO FORM-NAM
           ELSE
           IF ( CDMAIN-TYPE = "FZ" )
              MOVE "MN/FZMAIN" TO FORM-NAM
           ELSE
           IF ( CDMAIN-TYPE = "ID" )
              MOVE "MN/IDMAIN" TO FORM-NAM
           ELSE
           IF ( CDMAIN-TYPE = "JT" )
              MOVE "MN/JTMAIN" TO FORM-NAM
           ELSE
           IF ( CDMAIN-TYPE = "LO" )
              MOVE "MN/LOMAIN" TO FORM-NAM
           ELSE
           IF ( CDMAIN-TYPE = "LS" )
              MOVE "MN/LSMAIN" TO FORM-NAM
           ELSE
           IF ( CDMAIN-TYPE = "LV" )
              MOVE "MN/LVMAIN" TO FORM-NAM
           ELSE
           IF ( CDMAIN-TYPE = "OR" )
              MOVE "MN/CDORMN" TO FORM-NAM
           ELSE
           IF ( CDMAIN-TYPE = "PC" )
              MOVE "MN/PCMAIN" TO FORM-NAM
           ELSE
           IF ( CDMAIN-TYPE = "PH" )
              MOVE "MN/PHMAIN" TO FORM-NAM
           ELSE
           IF ( CDMAIN-TYPE = "PL" )
              MOVE "MN/PLMAIN" TO FORM-NAM
           ELSE
           IF ( CDMAIN-TYPE = "PO" )
              MOVE "MN/POMAIN" TO FORM-NAM
           ELSE
           IF ( CDMAIN-TYPE = "PU" )
              MOVE "MN/PUMAIN" TO FORM-NAM
           ELSE
           IF ( CDMAIN-TYPE = "PS" )
              MOVE "MN/PSCDMN" TO FORM-NAM
           ELSE
           IF ( CDMAIN-TYPE = "PX" )
              MOVE "MN/PXMAIN" TO FORM-NAM
           ELSE
           IF ( CDMAIN-TYPE = "RC" )
              MOVE "MN/RCCDMN" TO FORM-NAM
           ELSE
           IF ( CDMAIN-TYPE = "RE" )
              MOVE "MN/REMAIN" TO FORM-NAM
           ELSE
           IF ( CDMAIN-TYPE = "SR" )
              MOVE "MN/SRMAIN" TO FORM-NAM
           ELSE
           IF ( CDMAIN-TYPE = "TS" )
              MOVE "MN/TSMAIN" TO FORM-NAM
           ELSE
           IF ( CDMAIN-TYPE = "UW" )
              MOVE "MN/UWMAIN" TO FORM-NAM
           ELSE
           IF ( CDMAIN-TYPE = "XL" )
              MOVE "MN/XLMAIN" TO FORM-NAM
           ELSE
              MOVE "XX" TO STAT
              GO TO EXIT-ACCESS-CODE-TYPE.

       END-ACCESS-CODE-TYPE.
           MOVE FORM-PATH TO FORM-PATHNAME.

           MOVE FORM-PROGX TO VDU-FORM-NAME.

      * CHECK SECURITY
       COPY "LIBGB/CHKSEC.CPY".

           MOVE HOLD-FORM-PATHNAME TO FORM-PATHNAME FORM-PATH.

       EXIT-ACCESS-CODE-TYPE.
           EXIT.


      ******************************************************************
      *
      *    C O P Y - L I B R A R Y - S E C T I O N S
      *
      ******************************************************************
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/TSTCANID.CPY".
       COPY "LIBLP/LPCDCL.CPY".
       COPY "LIBGB/DATER.CPY".

      ****************************************************
       FORM-RESET SECTION.
      ****************************************************
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           EVALUATE CDMAIN-TYPE
              WHEN "AC" DISPLAY ACMAIN-SCREEN UPON CDMAIN-WINDOW-HANDLE
                        MOVE ACMAIN-FORM-FIELDS TO WR-BUF
                        MOVE ACMAIN-FORM-LIST   TO WR-LIST
              WHEN "A2" DISPLAY A2MAIN-SCREEN UPON CDMAIN-WINDOW-HANDLE
                        MOVE A2MAIN-FORM-FIELDS TO WR-BUF
                        MOVE A2MAIN-FORM-LIST   TO WR-LIST
              WHEN "AT" DISPLAY ATMAIN-SCREEN UPON CDMAIN-WINDOW-HANDLE
                        MOVE ATMAIN-FORM-FIELDS TO WR-BUF
                        MOVE ATMAIN-FORM-LIST   TO WR-LIST
              WHEN "BR" DISPLAY CDBRMN-SCREEN UPON CDMAIN-WINDOW-HANDLE
                        MOVE CDBRMN-FORM-FIELDS TO WR-BUF
                        MOVE CDBRMN-FORM-LIST   TO WR-LIST
              WHEN "BS" DISPLAY CDBSMN-SCREEN UPON CDMAIN-WINDOW-HANDLE
                        MOVE CDBSMN-FORM-FIELDS TO WR-BUF
                        MOVE CDBSMN-FORM-LIST   TO WR-LIST
              WHEN "BW" DISPLAY CDBWMN-SCREEN UPON CDMAIN-WINDOW-HANDLE
                        MOVE CDBWMN-FORM-FIELDS TO WR-BUF
                        MOVE CDBWMN-FORM-LIST   TO WR-LIST
              WHEN "CH" DISPLAY CHMAIN-SCREEN UPON CDMAIN-WINDOW-HANDLE
                        MOVE CHMAIN-FORM-FIELDS TO WR-BUF
                        MOVE CHMAIN-FORM-LIST   TO WR-LIST
              WHEN "CL" DISPLAY CLMAIN-SCREEN UPON CDMAIN-WINDOW-HANDLE
                        MOVE CLMAIN-FORM-FIELDS TO WR-BUF
                        MOVE CLMAIN-FORM-LIST   TO WR-LIST
              WHEN "CO" DISPLAY COMAIN-SCREEN UPON CDMAIN-WINDOW-HANDLE
                        MOVE COMAIN-FORM-FIELDS TO WR-BUF
                        MOVE COMAIN-FORM-LIST   TO WR-LIST
              WHEN "DF" DISPLAY DFCDMN-SCREEN UPON CDMAIN-WINDOW-HANDLE
                        MOVE DFCDMN-FORM-FIELDS TO WR-BUF
                        MOVE DFCDMN-FORM-LIST   TO WR-LIST
              WHEN "EA" DISPLAY EACDMN-SCREEN UPON CDMAIN-WINDOW-HANDLE
                        MOVE EACDMN-FORM-FIELDS TO WR-BUF
                        MOVE EACDMN-FORM-LIST   TO WR-LIST
              WHEN "FC" DISPLAY FCMAIN-SCREEN UPON CDMAIN-WINDOW-HANDLE
                        MOVE FCMAIN-FORM-FIELDS TO WR-BUF
                        MOVE FCMAIN-FORM-LIST   TO WR-LIST
              WHEN "FU" DISPLAY FUMAIN-SCREEN UPON CDMAIN-WINDOW-HANDLE
                        MOVE FUMAIN-FORM-FIELDS TO WR-BUF
                        MOVE FUMAIN-FORM-LIST   TO WR-LIST
              WHEN "ID" DISPLAY IDMAIN-SCREEN UPON CDMAIN-WINDOW-HANDLE
                        MOVE IDMAIN-FORM-FIELDS TO WR-BUF
                        MOVE IDMAIN-FORM-LIST   TO WR-LIST
              WHEN "JT" DISPLAY JTMAIN-SCREEN UPON CDMAIN-WINDOW-HANDLE
                        MOVE JTMAIN-FORM-FIELDS TO WR-BUF
                        MOVE JTMAIN-FORM-LIST   TO WR-LIST
              WHEN "LO" DISPLAY LOMAIN-SCREEN UPON CDMAIN-WINDOW-HANDLE
                        MOVE LOMAIN-FORM-FIELDS TO WR-BUF
                        MOVE LOMAIN-FORM-LIST   TO WR-LIST
              WHEN "LS" DISPLAY LSMAIN-SCREEN UPON CDMAIN-WINDOW-HANDLE
                        MOVE LSMAIN-FORM-FIELDS TO WR-BUF
                        MOVE LSMAIN-FORM-LIST   TO WR-LIST
              WHEN "LV" DISPLAY LVMAIN-SCREEN UPON CDMAIN-WINDOW-HANDLE
                        MOVE LVMAIN-FORM-FIELDS TO WR-BUF
                        MOVE LVMAIN-FORM-LIST   TO WR-LIST
              WHEN "OR" DISPLAY CDORMN-SCREEN UPON CDMAIN-WINDOW-HANDLE
                        MOVE CDORMN-FORM-FIELDS TO WR-BUF
                        MOVE CDORMN-FORM-LIST   TO WR-LIST
              WHEN "PC" DISPLAY PCMAIN-SCREEN UPON CDMAIN-WINDOW-HANDLE
                        MOVE PCMAIN-FORM-FIELDS TO WR-BUF
                        MOVE PCMAIN-FORM-LIST   TO WR-LIST
              WHEN "PH" DISPLAY PHMAIN-SCREEN UPON CDMAIN-WINDOW-HANDLE
                        MOVE PHMAIN-FORM-FIELDS TO WR-BUF
                        MOVE PHMAIN-FORM-LIST   TO WR-LIST
              WHEN "PL" DISPLAY PLMAIN-SCREEN UPON CDMAIN-WINDOW-HANDLE
                        MOVE PLMAIN-FORM-FIELDS TO WR-BUF
                        MOVE PLMAIN-FORM-LIST   TO WR-LIST
              WHEN "PO" DISPLAY POMAIN-SCREEN UPON CDMAIN-WINDOW-HANDLE
                        MOVE POMAIN-FORM-FIELDS TO WR-BUF
                        MOVE POMAIN-FORM-LIST   TO WR-LIST
              WHEN "PS" DISPLAY PSCDMN-SCREEN UPON CDMAIN-WINDOW-HANDLE
                        MOVE PSCDMN-FORM-FIELDS TO WR-BUF
                        MOVE PSCDMN-FORM-LIST   TO WR-LIST
              WHEN "PU" DISPLAY PUMAIN-SCREEN UPON CDMAIN-WINDOW-HANDLE
                        MOVE PUMAIN-FORM-FIELDS TO WR-BUF
                        MOVE PUMAIN-FORM-LIST   TO WR-LIST
              WHEN "PX" DISPLAY PXMAIN-SCREEN UPON CDMAIN-WINDOW-HANDLE
                        MOVE PXMAIN-FORM-FIELDS TO WR-BUF
                        MOVE PXMAIN-FORM-LIST   TO WR-LIST
              WHEN "RC" DISPLAY RCCDMN-SCREEN UPON CDMAIN-WINDOW-HANDLE
                        MOVE RCCDMN-FORM-FIELDS TO WR-BUF
                        MOVE RCCDMN-FORM-LIST   TO WR-LIST
              WHEN "RE" DISPLAY REMAIN-SCREEN UPON CDMAIN-WINDOW-HANDLE
                        MOVE REMAIN-FORM-FIELDS TO WR-BUF
                        MOVE REMAIN-FORM-LIST   TO WR-LIST
              WHEN "SR" DISPLAY SRMAIN-SCREEN UPON CDMAIN-WINDOW-HANDLE
                        MOVE SRMAIN-FORM-FIELDS TO WR-BUF
                        MOVE SRMAIN-FORM-LIST   TO WR-LIST
              WHEN "TS" DISPLAY TSMAIN-SCREEN UPON CDMAIN-WINDOW-HANDLE
                        MOVE TSMAIN-FORM-FIELDS TO WR-BUF
                        MOVE TSMAIN-FORM-LIST   TO WR-LIST
              WHEN "UW" DISPLAY UWMAIN-SCREEN UPON CDMAIN-WINDOW-HANDLE
                        MOVE UWMAIN-FORM-FIELDS TO WR-BUF
                        MOVE UWMAIN-FORM-LIST   TO WR-LIST
              WHEN "FZ" DISPLAY FZMAIN-SCREEN UPON CDMAIN-WINDOW-HANDLE
                        MOVE FZMAIN-FORM-FIELDS TO WR-BUF
                        MOVE FZMAIN-FORM-LIST   TO WR-LIST
              WHEN "XL" DISPLAY XLMAIN-SCREEN UPON CDMAIN-WINDOW-HANDLE
                        MOVE XLMAIN-FORM-FIELDS TO WR-BUF
                        MOVE XLMAIN-FORM-LIST   TO WR-LIST
           END-EVALUATE.
           PERFORM CALL-WRFORM.

           MOVE EXT-CONAME-NAM-DATA TO WR-BUF.
           MOVE NAM-LST TO WR-LIST.
           PERFORM CALL-WRFORM.
       FORM-RESET-EXIT.
           EXIT.

      *************************************************
       SETUP-LINK-INFO SECTION.
      *************************************************
           EVALUATE CDMAIN-TYPE
              WHEN "AC" MOVE ACMAIN-HELP--FILE TO HELP-LINK-FILE
                        MOVE ACMAIN-HELP--DIR TO HELP-LINK-DIR
              WHEN "A2" MOVE A2MAIN-HELP--FILE TO HELP-LINK-FILE
                        MOVE A2MAIN-HELP--DIR TO HELP-LINK-DIR
              WHEN "AT" MOVE ATMAIN-HELP--FILE TO HELP-LINK-FILE
                        MOVE ATMAIN-HELP--DIR TO HELP-LINK-DIR
              WHEN "BR" MOVE CDBRMN-HELP--FILE TO HELP-LINK-FILE
                        MOVE CDBRMN-HELP--DIR TO HELP-LINK-DIR
              WHEN "BS" MOVE CDBSMN-HELP--FILE TO HELP-LINK-FILE
                        MOVE CDBSMN-HELP--DIR TO HELP-LINK-DIR
              WHEN "BW" MOVE CDBWMN-HELP--FILE TO HELP-LINK-FILE
                        MOVE CDBWMN-HELP--DIR TO HELP-LINK-DIR
              WHEN "CH" MOVE CHMAIN-HELP--FILE TO HELP-LINK-FILE
                        MOVE CHMAIN-HELP--DIR TO HELP-LINK-DIR
              WHEN "CL" MOVE CLMAIN-HELP--FILE TO HELP-LINK-FILE
                        MOVE CLMAIN-HELP--DIR TO HELP-LINK-DIR
              WHEN "CO" MOVE COMAIN-HELP--FILE TO HELP-LINK-FILE
                        MOVE COMAIN-HELP--DIR TO HELP-LINK-DIR
              WHEN "DF" MOVE DFCDMN-HELP--FILE TO HELP-LINK-FILE
                        MOVE DFCDMN-HELP--DIR TO HELP-LINK-DIR
              WHEN "EA" MOVE EACDMN-HELP--FILE TO HELP-LINK-FILE
                        MOVE EACDMN-HELP--DIR TO HELP-LINK-DIR
              WHEN "FC" MOVE FCMAIN-HELP--FILE TO HELP-LINK-FILE
                        MOVE FCMAIN-HELP--DIR TO HELP-LINK-DIR
              WHEN "FU" MOVE FUMAIN-HELP--FILE TO HELP-LINK-FILE
                        MOVE FUMAIN-HELP--DIR TO HELP-LINK-DIR
              WHEN "ID" MOVE IDMAIN-HELP--FILE TO HELP-LINK-FILE
                        MOVE IDMAIN-HELP--DIR TO HELP-LINK-DIR
              WHEN "JT" MOVE JTMAIN-HELP--FILE TO HELP-LINK-FILE
                        MOVE JTMAIN-HELP--DIR TO HELP-LINK-DIR
              WHEN "LO" MOVE LOMAIN-HELP--FILE TO HELP-LINK-FILE
                        MOVE LOMAIN-HELP--DIR TO HELP-LINK-DIR
              WHEN "LS" MOVE LSMAIN-HELP--FILE TO HELP-LINK-FILE
                        MOVE LSMAIN-HELP--DIR TO HELP-LINK-DIR
              WHEN "LV" MOVE LVMAIN-HELP--FILE TO HELP-LINK-FILE
                        MOVE LVMAIN-HELP--DIR TO HELP-LINK-DIR
              WHEN "OR" MOVE CDORMN-HELP--FILE TO HELP-LINK-FILE
                        MOVE CDORMN-HELP--DIR TO HELP-LINK-DIR
              WHEN "PC" MOVE PCMAIN-HELP--FILE TO HELP-LINK-FILE
                        MOVE PCMAIN-HELP--DIR TO HELP-LINK-DIR
              WHEN "PH" MOVE PHMAIN-HELP--FILE TO HELP-LINK-FILE
                        MOVE PHMAIN-HELP--DIR TO HELP-LINK-DIR
              WHEN "PL" MOVE PLMAIN-HELP--FILE TO HELP-LINK-FILE
                        MOVE PLMAIN-HELP--DIR TO HELP-LINK-DIR
              WHEN "PO" MOVE POMAIN-HELP--FILE TO HELP-LINK-FILE
                        MOVE POMAIN-HELP--DIR TO HELP-LINK-DIR
              WHEN "PS" MOVE PSCDMN-HELP--FILE TO HELP-LINK-FILE
                        MOVE PSCDMN-HELP--DIR TO HELP-LINK-DIR
              WHEN "PU" MOVE PUMAIN-HELP--FILE TO HELP-LINK-FILE
                        MOVE PUMAIN-HELP--DIR TO HELP-LINK-DIR
              WHEN "PX" MOVE PXMAIN-HELP--FILE TO HELP-LINK-FILE
                        MOVE PXMAIN-HELP--DIR TO HELP-LINK-DIR
              WHEN "RC" MOVE RCCDMN-HELP--FILE TO HELP-LINK-FILE
                        MOVE RCCDMN-HELP--DIR TO HELP-LINK-DIR
              WHEN "RE" MOVE REMAIN-HELP--FILE TO HELP-LINK-FILE
                        MOVE REMAIN-HELP--DIR TO HELP-LINK-DIR
              WHEN "SR" MOVE SRMAIN-HELP--FILE TO HELP-LINK-FILE
                        MOVE SRMAIN-HELP--DIR TO HELP-LINK-DIR
              WHEN "TS" MOVE TSMAIN-HELP--FILE TO HELP-LINK-FILE
                        MOVE TSMAIN-HELP--DIR TO HELP-LINK-DIR
              WHEN "UW" MOVE UWMAIN-HELP--FILE TO HELP-LINK-FILE
                        MOVE UWMAIN-HELP--DIR TO HELP-LINK-DIR
              WHEN "FZ" MOVE FZMAIN-HELP--FILE TO HELP-LINK-FILE
                        MOVE FZMAIN-HELP--DIR TO HELP-LINK-DIR
              WHEN "XL" MOVE XLMAIN-HELP--FILE TO HELP-LINK-FILE
                        MOVE XLMAIN-HELP--DIR TO HELP-LINK-DIR
           END-EVALUATE.

      *************************************************
       VDU-CONVERT-FIELD SECTION.
      *************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBMN/ACMAIN_EVA.CPY".
              COPY "LIBMN/A2MAIN_EVA.CPY".
              COPY "LIBMN/ATMAIN_EVA.CPY".
              COPY "LIBMN/CDBRMN_EVA.CPY".
              COPY "LIBMN/CDBSMN_EVA.CPY".
              COPY "LIBMN/CDBWMN_EVA.CPY".
              COPY "LIBMN/CHMAIN_EVA.CPY".
              COPY "LIBMN/CLMAIN_EVA.CPY".
              COPY "LIBMN/COMAIN_EVA.CPY".
              COPY "LIBMN/DFCDMN_EVA.CPY".
              COPY "LIBMN/EACDMN_EVA.CPY".
              COPY "LIBMN/FCMAIN_EVA.CPY".
              COPY "LIBMN/FUMAIN_EVA.CPY".
              COPY "LIBMN/IDMAIN_EVA.CPY".
              COPY "LIBMN/JTMAIN_EVA.CPY".
              COPY "LIBMN/LOMAIN_EVA.CPY".
              COPY "LIBMN/LSMAIN_EVA.CPY".
              COPY "LIBMN/LVMAIN_EVA.CPY".
              COPY "LIBMN/CDORMN_EVA.CPY".
              COPY "LIBMN/PCMAIN_EVA.CPY".
              COPY "LIBMN/PHMAIN_EVA.CPY".
              COPY "LIBMN/PLMAIN_EVA.CPY".
              COPY "LIBMN/POMAIN_EVA.CPY".
              COPY "LIBMN/PSCDMN_EVA.CPY".
              COPY "LIBMN/PUMAIN_EVA.CPY".
              COPY "LIBMN/PXMAIN_EVA.CPY".
              COPY "LIBMN/RCCDMN_EVA.CPY".
              COPY "LIBMN/REMAIN_EVA.CPY".
              COPY "LIBMN/SRMAIN_EVA.CPY".
              COPY "LIBMN/TSMAIN_EVA.CPY".
              COPY "LIBMN/UWMAIN_EVA.CPY".
              COPY "LIBMN/FZMAIN_EVA.CPY".
              COPY "LIBMN/XLMAIN_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ******************************************************************
       MISC-PARAGRAPHS SECTION.
      ******************************************************************
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/PHONEIO.CPY".
       GET-BR.
           PERFORM OPEN-BR-FILE.
           PERFORM READ-BR-FILE.
           PERFORM CLOSE-BR-FILE.
       SEND-SCREEN-PROMPT.
           MOVE "S  A700MSEL." TO VDU.
           MOVE HOLD-SCR-PROMPT      TO VDUBUF.
           PERFORM SCREENIO.
       SEND-LEGEND.
           MOVE "MSEL." TO WR-LIST.
           MOVE LEGEND TO WR-BUF.
           PERFORM CALL-WRFORM.
       RECORD-DELETED.
           MOVE " RECORD WAS DELETED!" TO MESS.
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
           MOVE " THIS RECORD HAS UNDERGONE CHANGE BY ANOTHER USER,"
                                TO MESS.
           PERFORM SEND-MESS.
       ACTIVE-RECORD.
           MOVE " ACTIVE CUSTOMER CANNOT BE DELETED" TO MESS.
           PERFORM SEND-MESS.

      ******************************************************************
       IO-ROUTINES SECTION.
      ******************************************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/DECLARE_SQL.CPY".
           PERFORM CLOSE-CD1-FILE.
           PERFORM CLOSE-BR-FILE.
       COPY "LIBLP/LPCDGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBLP/LPCD1IN.CPY".

       COPY "LIBGB/FERRORS.CPY".

      ******************************************************************
       END-PROGRAM SECTION.
      ******************************************************************

           PERFORM CLOSE-FILES.

       EXIT-PROG.

       STOP-RUN.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           EVALUATE CDMAIN-TYPE
              WHEN "AC" DESTROY ACMAIN-SCREEN
              WHEN "A2" DESTROY A2MAIN-SCREEN
              WHEN "AT" DESTROY ATMAIN-SCREEN
              WHEN "BR" DESTROY CDBRMN-SCREEN
              WHEN "BS" DESTROY CDBSMN-SCREEN
              WHEN "BW" DESTROY CDBWMN-SCREEN
              WHEN "CH" DESTROY CHMAIN-SCREEN
              WHEN "CL" DESTROY CLMAIN-SCREEN
              WHEN "CO" DESTROY COMAIN-SCREEN
      *       WHEN "DF" DESTROY DFCDMN-SCREEN
              WHEN "PS" DESTROY PSCDMN-SCREEN
              WHEN "XL" DESTROY XLMAIN-SCREEN
              WHEN "LV" DESTROY LVMAIN-SCREEN
              WHEN "EA" DESTROY EACDMN-SCREEN
      *       WHEN "FC" DESTROY FCMAIN-SCREEN
              WHEN "OR" DESTROY CDORMN-SCREEN
           END-EVALUATE.

           DESTROY CDMAIN-WINDOW-HANDLE.

           MOVE EXIT-PATHNAME TO FORM-PATH.
           EXIT PROGRAM.
