       IDENTIFICATION DIVISION.
       PROGRAM-ID.      LONPG3.
       DATE-WRITTEN.    062790.
      **************************************************************************
      *   ADD-ON FEE & INSURANCE ENTRY AND CALCULATIONS
      *   CALLED FROM LONPG0
      *   ...USED WITH LT-RECORD CREATION
      *
      * REV:
      *  051295 SLC ADDED INSURANCE CHECK. MUST HAVE A&H AND LIFE IF
      *             BRANCH OPTION IS ON.
      *  051895 SLC ADDED IF REPODATE NOT = 0, DISALLOW ADDON.
      *  071995 JTG REMOVED BOGUS CLEAR OF PD-PRLNAMT
      *  KEC 111496 CHANGED LN-POCD FOR RESCISSION.
      *  MJD 011797 ADDED UNITPER
      *  MJD 050897 DO NOT ALLOW "ST"  ADDONS FOR REMAINING TERM.
      *  MJD 051397 USE LN/PD-SMPRATE INSTEAD OF LN/PD-EFFRATE.
      *  MJD 052297 ALLOW USER TO CHANGE TYPE OF ADDON WITHOUT REENTERING
      *             ACCOUNT NUMBER. IF ACCOUNT IS IB THEN ALLOW RATE CHANGE
      *             TO 0.
      *  MJD 052397 CHANGED TO PERFORM CALL-LOAN-CALCULATIONS AT ONE
      *             CENTRAL LOCATION. CHANGED TO PASS ACCOUNT NUMBER BACK
      *             TO LONPG0.C VIA LOAN-KEY WHEN "F1" IS USED.
      *  GLF 090997 REPLACED 'IO' ON GP-FILE WITH GET 'GPENV'
      *  GLF 101797 ADDED MOVE LN-APPNO TO EXT-WI-CL-APPNO
      *  JTG 120497 CHANGED VALID-INS-TYPES-ELIG SECTION TO INSURE THE
      *             NEW ENTERED 3 DIGIT INS. FIELD (EX. MGG) IS VALID
      *             AND INCLUDES EXISTING INS. TYPES PRIOR TO ADDON
      *  BAH 022598 ADDED BR-ADDON-FEECD-O3 PIC XX OCCURS 2
      *  JTG 030698 CORRECTED DISPLAY WHEN USING FIXED ANNUAL RATES, CASTLE #660
      *  BAH 040698 GP-DEFAULT-O3-INS-COMPANY 0 WAS NOT ALLOWING ANY INS CO.
      *   CS 042898 ADDED RENEWAL CODES "RB" AND "RO"
      *  BAH 042998 REMOVED BR-ADDON-FEECD(1) AND (2) AND RENAMED (3) AND (4)
      *             TO BR-ADDON-FEECD-O2(1) AND (2)
      *  BAH 110398 CHANGED TEST ON INVALID-TYPE, SEE COMMENTS
      *  GLF 990607 PROVIDED FOR F6 SCAN OF INSURANCE COMPANY NUMBER
      *  JTG 991109 ALLOWED ACCOUNTS TO HAVE ADDON'S WHEN IN REPO STATUS,
      *             CHANGED UPD OF DSFILE, TYFILE & LPEOD5TR, NEWPORT #244, A15
      *  JTG 000113 ADDED SP-INSREJECT-FG OPTION 'C', WHICH WILL CAUSE WRITING
      *             NO. OF MONTHS INSURANCE NOT IN VIOLATION, WISCONSIN #1032
      *  MJD 000207 ADDED GP-OT-INS-TERM (TFC PR#48) IF 'Y' THEN ALLOW
      *             ALLOW TERM ENTRY FOR O2O3O4O5 INSURANCE.
      *  KEC 000530 {PR#00048} TFC [JUDY BRANIFF];
      *             ADDED THE FOLLOWING 'IF' CONDITION FOR ENTERING THE
      *             INSURANCE TERM (GP-OT-INS-TERM = "Y"):
      *                   IF ( VDUNUM0 > 360 )
      *                      GO TO ENTER-IC-TERM.
      *  JTG 000818 IN THE CORRECTED SET OF LT-ANTICTERM TO FACTOR IN ODD
      *             LAST PAYMENTS, I REALIZED THAT THE SET-NP-PAYMENT LOGIC
      *             WAS NOT CORRECT  REGACC #288
      *  JTG 000919 ADDED SP-ADDON-EXCEPT-FG REGACC #71225 PR#1297
      *  JTG 020514 ADDED "CT" COUNTY TAX CODE KENTUCKY AND PERCENTS
      *             ADDED "CY" CITY   TAX CODE KENTUCKY AND PERCENTS  WORLD #288
      *  KEC 020624 {PR#01823} REGIONAL ACCEPTANCE <A15> [KEVIN MULLEN]
      *             ADDED LOGIC FOR USING 'CD-GC-FEE-MAXAMT'
      *  JTG 021125 CLEARED TO BW-1-INFO FIELDS                      WORLD #288
      *             MUST CLEAR/RESET THESE FIELDS WHICH ARE PASSED TO THE CALCZL
      *             TO DETERMINE KY SALES TAX. PROBLEM WITH CAN LOAN'S QUOTES
      *             WHICH INITIALLY DON'T HAVE BORROWERS.
      *  BLV 021211 CHECK OF PD-FEECODE FOR INSURANCE SALES TAX WAS CHECKING
      *             T5 TWICE & MISSING T4, WORLD PR# 288
      *  JTG 030929 MODIFIED SCFRMLA 13 TO ONLY TAKE THE DUAL LOAN
      *             ADJUSTMENT INTO ACCOUNT WHEN THE LOANS'S CLASSES
      *             ARE THE SAME                               LENDMARK#2149
      *  JTG 031001 CREATED 01  SCHG-WORKERS-2 FOR PASS FOR SCFRMLA 13
      *                                                       LENDMARK #2149
      *  BAH 031023 ADDED GP-DEFAULT-O3-INSOURS, LENDMARK PR# 2180
      *  BAH 040304 ADDED SP-DEFAULT-O2-INS-COMP FOR LENDMARK PR# 2258
      *  BAH 041026 THERE WERE NO EDITS ON INSURANCE EXPIRATION DATE, COULD
      *             ENTER A DATE PRIOR TO ADDON DATE EVEN !! WAS CAUSING A
      *             ZERO TERM WHICH MUCKED UP REBATES...REGACC PL# 448
      *   CS 100825 DON'T ALLOW A 'ET' FOR A TERM OF 1, IF THE BALLOON
      *             POSITIVE, NO LOGIC IN PLACE TO COVER SCENARIO, JR SAID
      *             DON'T FIX IT, JUST DONT ALLOW UPDATE   PL#691  COMMUNITY
      *                                                            LENDERS
      * MJD 130604 ADDED LOGIC FOR SP-SCFRMLA = 54.  FUNCTIONS THE SAME AS 12
      *            BUT INCLUDES AN EXTRA 4% ADDED IN AT THE END BASED
      *            ON TOTAL NOTE                                     WORLD #0874
      * MJD 140723 ADDED SP-ADDON-RATE-FG = "A"           MULLEN/KSTREET PR#4556
      * BAH 140814 ADDED TEST FOR LPSMNU IN HELP
      *  CS 150428 COLONIAL TN DOES ADDONS FOR 'NP' AND CLAIMS BY LAW THEY CAN
      *            NOT CHARGE INTEREST, IN SET-NP-PAYMENT ADDED
      *            IF PD-SMPRATE = 0 GO TO SET-NP-PAYMENT-PC WHICH IS WHAT 
      *            "LP" & "RT DOES. PREV IT FELL THRU TO SET-NP-PAYMENT-IB WHICH      *            PERFORMS CALC-EPAY-CALC BUT GETS OUT BECAUSE THE RATE IS 0
      *            AND THEN TRIED TO CALCULATE A FC BY TAKING REGPAY WHICH IS 0
      *            AND SUBTRACTING THE PREMIUM LEADING TO A BAD FC CHARGE
      *  CS 150814 IF GP-I-AM-EXCEL, TEST THE FEECODE AMOUNT AGAINST CD-
      *            GC-BRANCH-COMM-AMOUNT  EXCEL PR#4821
      *  CS 150922 REMOVED GP-I-AM-EXCEL TEST ON CD-GC-BRANCH-COMM-AMOUNT, PER
      *            ANDRIA DONT NEED EXCEL PR#4821
      * BAH 160118 SPLIT OUT LTFILE, PD0003
      * KEC 2016.0307 CHANGED WK-CCY-TAX TO BE WK-CY-TAX.
      *               CHANGED CD-CCY-REC FROM BEING HARDCODED TO RATHER
      *               USE "REPLACE LEADING" ON LP01CD (WSCD-REC).
      * KEC 2016.0314 CHANGED CD-FEE???? TO CD-GC-????.
      * KEC 2016.0509 CHANGED LN-APPNO TO USE LN-APP-NUMBER FOR MA1-KEY.
      *  CS 170405 IF GP-I-AM-COLTN,IF THE EXPIRATION DATE ENTERED ON
      *            RT IS > MATURITY DATE OF LOAN, GIVE A WARNING COLTN PL970
      * BAH 170619 ACTIVATE 8 DIGIT DATES, PD0006
      * KEC 2018-0824 LOAN APP MODULE REMOVAL FROM A30 [LARM30];
      *         LEAVING LN-APP-NUMBER & EXT-WI-CL-APP-NUMBER
      *         HERE IN CASE NEEDED IN THE FUTURE.
      * BAH 190122 SPLIT CDFILE
      * BAH 20190625 REMOVED LC-REVOLVING, PT#0153
      * BAH 20200625 ADDED OPEN-LTP1-FILE, REMOVED FROM GETSCH, #TST001
      * BAH 2022.0314 REMOVED EXT-SPANISH-FORMS AND ALL MX FORMS
      * BAH 20221205 DONT ALLOW "TM" TO BE ENTERED AS A VALID FEECODE, THIS
      *              IS UNITED MOTOR CLUB SALES TAX #1582
      * BAH 2024.0311 REMOVED EXT-WI-CL-APP-NUMBER
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
           "@(#)LP/LONPG3.CBL S35 02/23/24-10:15:29 >".
      **********************************
      *       F I L E   I F N          *
      **********************************
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
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
       COPY "LIBLP/LP01LN.CPY".
       COPY "LIBLP/LP01LN_SQL.CPY".
       COPY "LIBLP/LPWSLN.CPY".
       COPY "LIBLP/LP01LTC.CPY".
       COPY "LIBLP/LP01LTC_SQL.CPY".
       COPY "LIBLP/LPWSLTC.CPY".
       COPY "LIBLP/LP01LTP.CPY".
       COPY "LIBLP/LP01LTP_SQL.CPY".
       COPY "LIBLP/LPWSLTP.CPY".

      ******************************************************************
      *    COPIED FROM LP01CD   
      *
      *    CCY -  CITY INSURANCE TAX RATES FOR KENTUCKY
      *
      *    LEVEL: 1  CL DEC             8  O1
      *           2  CL DEC JOINT       9  O2
      *           3  CL LEV            10  O3
      *           4  CL LEV JOINT      11  O4
      *           5  AH                12  O5
      *           6  PP
      *           7  PP 2
      ******************************************************************
       01  CD-CCY-REC.
           03  CD-CCY-KEY.
               05  CD-CCY-TYPE                   PIC X(2).
               05  CD-CCY-STATE                  PIC X(2).
               05  CD-CCY-CITY                   PIC X(3).
               05  CD-CCY-FILLER                 PIC X(3).
      *-----------------------------------------------------------------
           03  CD-CCY-INSTBL                     OCCURS 12.
               05  CD-CCY-INSTAXFRMLA            PIC X.
               05  CD-CCY-INSTAXAMT              PIC 99V99 COMP-3.
           03  CD-CCY-BOTH-TAX-FG                PIC X.

       01  CD-CCT-REC.
           03  CD-CCT-KEY.
               05  CD-CCT-TYPE                   PIC X(2).
               05  CD-CCT-STATE                  PIC X(2).
               05  CD-CCT-COUNTY                 PIC X(3).
               05  CD-CCT-FILLER                 PIC X(3).
      *-----------------------------------------------------------------
           03  CD-CCT-INSTBL                     OCCURS 12.
               05  CD-CCT-INSTAXFRMLA            PIC X.
               05  CD-CCT-INSTAXAMT              PIC 99V99 COMP-3.
           03  CD-CCT-BOTH-TAX-FG                PIC X.

       01  WK-KEY          PIC 9(2) COMP.

       01  ONE             PIC S9   COMP.
       01  ELE             PIC 9(6) COMP.
       01  ACTION.
           03  CHKEY       PIC X(4).
           03  CHNO        PIC 9(2).
       01  IO-TYPE         PIC X(3).
       01  NEW-FLAG        PIC 9    COMP.
       01  FIRST-TIME      PIC 9    COMP   VALUE 0.
       01  FIRST-ELE       PIC 99   COMP   VALUE 0.
       01  GA-SCHG-PATH.
           03  FILLER         PIC X(7)     VALUE "CL/GASC".
           03  GA-SCHG-NO     PIC 99.

       01  IX1                PIC 99        COMP.
       01  IX2                PIC 99        COMP.
       01  SUB                PIC 9(6)      COMP.
       01  WK-QUOTE           PIC X         VALUE X"22".
       01  ET-SUB             PIC 9(2)      COMP.
       01  I-LEVEL            PIC 9(6)      COMP.
       01  WHOLE-DOLLARS      PIC S9(7)     COMP.
       01  FINANCE-CHG        PIC S9(7)V99  COMP.
       01  TOTAL-REPAYABLE    PIC S9(7)V99  COMP.
       01  WS-RATE            PIC 999V9(15) COMP.
       01  ASCHD-SUB          PIC S999      COMP.
       01  ASCHD-AMTDUE       PIC S9(7)V99  COMP.
       01  AMTDUE             PIC S9(7)V99  COMP.
       01  WK-AMOUNT          PIC S9(7)V99  COMP.
       01  WK-CY-TAX          PIC S9(7)V99  COMP.
       01  WK-CT-TAX          PIC S9(7)V99  COMP.
       01  WK-INS-PREMIUM     PIC S9(7)V99  COMP.
       01  WK-INS-TAX         PIC S9(7)V99  COMP.
       01  DISP-NUM0          PIC ZZ9.
       01  DISP-NUM0-ALP     REDEFINES DISP-NUM0
                              PIC X(3).
       01  COMPUTE-LOAN-FG    PIC X         VALUE "Y".

       01  INSPECT-W          PIC X(86).
       01  ADD-ON             PIC X(25).
       01  DEFAULT-APR-FG     PIC X.
       01  ONLY-1STPYDATE-FG  PIC X        VALUE " ".
       01  BW-1-INFO.
           03  BW-1-STATE              PIC XX.
           03  BW-1-CITY-TAX-CD        PIC XXX.
           03  BW-1-COUNTY             PIC XXX.


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

       01  HOLD-VDUSTATUS                       PIC X(02)  VALUE "00".

       01  SCHEDULED-TERM PIC 9(4)  VALUE 0.
      *----------------------------------------------------------------
      * LONPGW CONTAINS  DISPLAY-ORIG-BUF
      *                  DISPLAY-ORIG-LIST
      *                  1STPY-BUF
      *                  1STPY-LIST
      *                  DISPLAY-BUF1
      *                  DEF-BUF
      *                  DEF-LIST
      *                  DF2-BUF
      *                  DF2-LIST
      *                  LONPC2-BUF
      *                  LONPC2-LIST
      *                  MESSAGE-BUF
      *                  MESSAGE-LIST
      *                  MESS-LIST
      *                  MAX
      *                  SPEC-TABLE
      *                  SPEC-TABLE2 (SPEC, CHFG)
      *----------------------------------------------------------------
       COPY "LIBLP/LONPGW.CPY".

       01  AMT-HOLDER                      PIC S9(9)V99 COMP.
       01  TOTAL-GL-HLDR                   PIC S9(5)V99 COMP.
       01  TOTAL-GL-HLDR-SALESTAX          PIC S9(5)V99 COMP.
       01  TOTAL-GL-FINCHG-HLDR            PIC S9(5)V99 COMP.
       01  TOTAL-GL-FINCHG-HLDR-SALESTAX   PIC S9(5)V99 COMP.
       01  INSPREM-HLDR                    PIC S9(4)V99 COMP.
       01  INSTERM-HLDR                    PIC 9(3)     COMP.
       01  INSOURS-HLDR                    PIC X.

       01  ERROREFF.
           03  FILLER            PIC X(36) VALUE
               "CAN'T EXCEED ORIGNAL LOANS RATE OF: ".
           03  ERROREFF-EFF      PIC ZZ9.999.

       01  ERRORSPR.
           03  FILLER            PIC X(32) VALUE
               "CAN'T EXCEED SPR FIXED RATE OF: ".
           03  ERRORSPR-SPR      PIC ZZ9.999.

       01  TERM-LOW             PIC 9(3)    COMP.
       01  TERM-HIGH            PIC 9(3)    COMP.
       01  PROCEEDS-LOW         PIC 9(6)V99 COMP.
       01  PROCEEDS-HIGH        PIC 9(6)V99 COMP.


       COPY "LIBCL/GASCHGW.CPY".
       COPY "LIBWI/MEMOSCW.CPY".
       COPY "LIBLP/LPWKPATH.CPY".
       COPY "LIBGB/SYSTEMW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/GPENVW.CPY".
       COPY "LIBLP/LPUPERW.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBLP/LPANTIW.CPY".
       COPY "LIBLP/LPMDTEW.CPY".
       COPY "LIBWI/BWLNS2W.CPY".
       COPY "LIBWI/ICSCANW.CPY".
       COPY "LIBLP/LPMENUW.CPY".
       COPY "LIBLP/LPSPEDW.CPY".
       COPY "LIBLP/LPSCHDW.CPY".
       COPY "LIBLP/LPCEPPW.CPY".
       COPY "LIBLP/LPEPAYW.CPY".
       COPY "LIBLP/LPCARTW.CPY".
       COPY "LIBLP/LPITRMW.CPY".
       COPY "LIBCL/CLMENUW_EXT.CPY".

       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBLP/LONPG_DEF.CPY".
       COPY "LIBLP/LPSMNU_DEF.CPY".
       COPY "LIBLP/LPLNMU_DEF.CPY".
       COPY "LIBLP/LPCAMU_DEF.CPY".
       COPY "LIBLP/LPVDMU_DEF.CPY".
       COPY "LIBLP/LONPG_WKS.CPY".
       COPY "LIBLP/LPSMNU_WKS.CPY".
       COPY "LIBLP/LPLNMU_WKS.CPY".
       COPY "LIBLP/LPCAMU_WKS.CPY".
       COPY "LIBLP/LPVDMU_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".

      /*************************************************
      *   C O N T R O L   M O D U L E                  *
      **************************************************
       COPY "LIBGB/GETFMW.CPY".
       COPY "LIBLP/LPLOANW.CPY".
       COPY "LIBLP/LPPFBUF.CPY".

       SCREEN SECTION.
       COPY "LIBLP/LONPG_SCN.CPY".
       COPY "LIBLP/LPSMNU_SCN.CPY".
       COPY "LIBLP/LPLNMU_SCN.CPY".
       COPY "LIBLP/LPCAMU_SCN.CPY".
       COPY "LIBLP/LPVDMU_SCN.CPY".

       PROCEDURE DIVISION
             USING FORM-PATHNAME EXIT-PATHNAME PROG-BUF UPDATE-BUF.
       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
               WK-FILE.
       COPY "LIBGB/DECLARE.CPY".
           PERFORM CLOSE-SP1-FILE.
           PERFORM CLOSE-LTP1-FILE.
           PERFORM CLOSE-LTC1-FILE.
           PERFORM CLOSE-LN1-FILE.
           PERFORM CLOSE-LC1-FILE.
           PERFORM CLOSE-IC1-FILE.
           PERFORM CLOSE-DL1-FILE.
           PERFORM CLOSE-CDB1-FILE.
           PERFORM CLOSE-CD1-FILE.
           PERFORM CLOSE-BW1-FILE.
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
           MOVE "LONPG" TO VDU-FORM-NAME.

           MOVE USER-PID TO TEMP-PID.
           MOVE " " TO ERRCD.

      * ALLOW F9-INTERRUPT
           MOVE "F" TO VDU-CLMENU-SELECTION.

      * ALLOW CTRL 'N' INTERRUPT:
           MOVE "WI/MAKERS" TO VDU-CTRL-N-PROG.

           PERFORM RESTORE-SCREEN.

           PERFORM GET-GPENV.

           PERFORM OPEN-IC1-FILE.
           PERFORM OPEN-CDB1-FILE.
           PERFORM OPEN-CD1-FILE.
           PERFORM OPEN-SP1-FILE.
           PERFORM OPEN-LTP1-FILE.

       MAIN-MODULE.

      * CLEAR REQUIRED RE: (GA) SP-SCFRMLA = 12 OR 13
           MOVE 0 TO SPDUAL-LIAB.

           IF FIRST-TIME = 0
              MOVE 1 TO FIRST-TIME
           ELSE
              MOVE " " TO MESS
              PERFORM MSG-LINE.

      * MUST CLEAR/RESET THESE FIELDS WHICH ARE PASSED TO THE CALCZL
      * TO DETERMINE KY SALES TAX. PROBLEM WITH CAN LOAN'S QUOTES
      * WHICH INITIALLY DON'T HAVE BORROWERS.
           MOVE " " TO BW-1-INFO.

           MOVE 0 TO NEW-FLAG ELE.
           MOVE " " TO IO-TYPE.
           PERFORM NEW-RECORD-SETUP.

       ENTER-LOAN.
      * PASSED IN PROG-BUF (LPLOANW)
           MOVE BRANCH          TO LN-OWNBR.
           MOVE ADDON-LN-ACCTNO TO LN-ACCTNO.
           PERFORM PROCESS-ORIGINAL-LOAN.
           IF IO-FG = 8
              MOVE "ADDON IS NOT ALLOWED ON THIS LOAN" TO MESS
              PERFORM SEND-MESS
              MOVE 0 TO NEW-PROG-SUB
              MOVE "P" TO ERRCD
              GO TO END-ROUTINE.

           IF IO-FG = 9
              MOVE "INVALID LOAN NUMBER" TO MESS
              PERFORM SEND-MESS
              MOVE 0 TO NEW-PROG-SUB
              MOVE "P" TO ERRCD
              GO TO END-ROUTINE.

           PERFORM ENTRY-MODULE.
           IF IO-TYPE = "CLR"
              MOVE PD-ACCTNO TO LOAN-KEY
              MOVE 0 TO NEW-PROG-SUB
              MOVE "P" TO ERRCD
              GO TO END-ROUTINE.

           IF IO-TYPE = "END"
              GO TO END-ROUTINE.
           GO TO MAIN-MODULE.

      *************************************************
      *       ENTRY-MODULE
      *       NEW-FLAG = 0 - MODIFY EXISTING RECORD
      *                  1 - INITIAL NEW RECORD ENTRY
      *                  2 - MODIFY NEW RECORD
      *************************************************
       ENTRY-MODULE SECTION.

           MOVE " " TO VDUBUF VDU-ATTRIBUTES.
           MOVE "S  A050MSEL." TO VDU.
           PERFORM SCREENIO.

           MOVE " " TO IO-TYPE.
           MOVE 1 TO ONE.
           MOVE FIRST-ELE TO ELE.
           MOVE 0 TO FIRST-ELE.
       CREATE-ROUTINE.
           IF NEW-FLAG NOT = 1
              GO TO MODIFY-ROUTINE.
           MOVE "Y" TO COMPUTE-LOAN-FG.
           ADD ONE TO ELE.
           IF ELE < 1
              MOVE 1 TO ELE ONE.
           IF ELE NOT > MAX
              GO TO ENTER-ELE.

           MOVE 2 TO NEW-FLAG.

       MODIFY-ROUTINE.

      * ALLOW A FEW CHANGES TO OVRDRATE
      * ALLOW RESTRICTED ENTRY OF RATE FOR IB ACCTS: 0 OR LN-SMPRATE

           MOVE " " TO CHFG(7).

      * DON'T ALLOW CHANGE TO ADDON DATE/CODE/TYPE:
      
           MOVE "N" TO CHFG(1) CHFG(3) CHFG(5).

           IF COMPUTE-LOAN-FG = "Y"
              IF PD-SRCD = "ET"
                 PERFORM CALCULATE-1STPYDATE-TERM
                 MOVE PD-1STPYDATE TO PD-ORIG-1STPYDATE DATE-YYYYMMDD
                 PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
                 MOVE DATE-MMDDYY TO D-1STPYDATE
                 MOVE PD-ORGTERM TO D-ORGTERM
                 MOVE "<--" TO D-ARROW1
                 MOVE 1STPY-BUF TO WR-BUF
                 MOVE 1STPY-LIST TO WR-LIST
                 PERFORM CALL-WRFORM
              END-IF
              PERFORM TEST-FOR-GEORGIA-SCHG
              PERFORM CALL-LOAN-CALCULATIONS.
           MOVE " " TO COMPUTE-LOAN-FG.

           MOVE "FIELD #, U - UPDATE:" TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "R RA060SEL." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.
           IF VDUSTATUS = "12"
              PERFORM CALL-MEMO
              GO TO MODIFY-ROUTINE.
           IF VDUSTATUS = "16"
              PERFORM DISPLAY-ALL-LOANS
              GO TO MODIFY-ROUTINE.
           IF VDUSTATUS = "18"
              PERFORM SEND-PAYMENT-LINES
              GO TO MODIFY-ROUTINE.
           IF VDUSTATUS = "1:"
              PERFORM SEND-PAYOFF
              GO TO MODIFY-ROUTINE.
           IF VDUSTATUS NOT = "00"
              GO TO MODIFY-ROUTINE.
           MOVE VDUBUF TO ACTION.

           IF ACTION = "     U"
              PERFORM VERIFY-REQD-FLDS
              IF IO-FG = 9
                 MOVE 0 TO IO-FG
                 GO TO MODIFY-ROUTINE.

           IF ACTION = "     U"
              PERFORM SAVE-SCREEN
              MOVE 4 TO NEW-PROG-SUB
              MOVE "P" TO ERRCD
              MOVE "END" TO IO-TYPE
              GO TO END-ENTRY-MODULE.

           INSPECT ACTION REPLACING ALL " " BY "0".
           IF CHNO NOT NUMERIC
              GO TO MODIFY-ROUTINE.
           IF CHNO = 0
              GO TO MODIFY-ROUTINE.
           IF CHNO > MAX
              GO TO MODIFY-ROUTINE.
           MOVE CHNO TO ELE.
           IF ELE NOT = 6
              MOVE "Y" TO DEFAULT-APR-FG.

       ENTER-ELE.

      * FEES
      
           IF ELE = 6 AND CHFG(6) NOT = "N"
              MOVE "<ENTER> = NONE, ENTER FEE CODE:" TO LEGEND
              PERFORM SEND-LEGEND.

           IF ELE = 7
              PERFORM RESET-LEGEND.

           IF ELE = 8
              MOVE VDUSTATUS TO HOLD-VDUSTATUS
              MOVE "F6-SCAN, ENTER INSURANCE COMPANY"
              TO LEGEND
              PERFORM SEND-LEGEND
              IF HOLD-VDUSTATUS = "1<"
                 PERFORM INSCOMP-SCAN
                 MOVE "SNNN020080." TO VDU
                 MOVE ICSCAN-CODE TO VDUNUM0
                 PERFORM SCREENIO
                 MOVE ICSCAN-CODE TO VDUNUM0
                 IF IO-FG = 9
                    GO TO ENTER-ELE.

           IF ELE = 9
              MOVE VDUSTATUS TO HOLD-VDUSTATUS
              MOVE "F6-SCAN, ENTER INSURANCE COMPANY"
              TO LEGEND
              PERFORM SEND-LEGEND
              IF HOLD-VDUSTATUS = "1<"
                 PERFORM INSCOMP-SCAN
                 MOVE "SNNN020090." TO VDU
                 MOVE ICSCAN-CODE TO VDUNUM0
                 PERFORM SCREENIO
                 MOVE ICSCAN-CODE TO VDUNUM0
                 IF IO-FG = 9
                    GO TO ENTER-ELE.

           IF ELE = 10
              MOVE VDUSTATUS TO HOLD-VDUSTATUS
              MOVE "F6-SCAN, ENTER INSURANCE COMPANY"
              TO LEGEND
              PERFORM SEND-LEGEND
              IF HOLD-VDUSTATUS = "1<"
                 PERFORM INSCOMP-SCAN
                 MOVE "SNNN020100." TO VDU
                 MOVE ICSCAN-CODE TO VDUNUM0
                 PERFORM SCREENIO
                 MOVE ICSCAN-CODE TO VDUNUM0
                 IF IO-FG = 9
                    GO TO ENTER-ELE.

           IF ELE = 11
              MOVE VDUSTATUS TO HOLD-VDUSTATUS
              MOVE "F6-SCAN, ENTER INSURANCE COMPANY"
              TO LEGEND
              PERFORM SEND-LEGEND
              IF HOLD-VDUSTATUS = "1<"
                 PERFORM INSCOMP-SCAN
                 MOVE "SNNN020110." TO VDU
                 MOVE ICSCAN-CODE TO VDUNUM0
                 PERFORM SCREENIO
                 MOVE ICSCAN-CODE TO VDUNUM0
                 IF IO-FG = 9
                    GO TO ENTER-ELE.

           IF ELE = 12
              MOVE VDUSTATUS TO HOLD-VDUSTATUS
              MOVE "F6-SCAN, ENTER INSURANCE COMPANY"
              TO LEGEND
              PERFORM SEND-LEGEND
              IF HOLD-VDUSTATUS = "1<"
                 PERFORM INSCOMP-SCAN
                 MOVE "SNNN020120." TO VDU
                 MOVE ICSCAN-CODE TO VDUNUM0
                 PERFORM SCREENIO
                 MOVE ICSCAN-CODE TO VDUNUM0
                 IF IO-FG = 9
                    GO TO ENTER-ELE.

           IF ELE = 13
              MOVE VDUSTATUS TO HOLD-VDUSTATUS
              MOVE "F6-SCAN, ENTER INSURANCE COMPANY"
              TO LEGEND
              PERFORM SEND-LEGEND
              IF HOLD-VDUSTATUS = "1<"
                 PERFORM INSCOMP-SCAN
                 MOVE "SNNN020130." TO VDU
                 MOVE ICSCAN-CODE TO VDUNUM0
                 PERFORM SCREENIO
                 MOVE ICSCAN-CODE TO VDUNUM0
                 IF IO-FG = 9
                    GO TO ENTER-ELE.

           IF CHKEY NOT = ZEROS
              IF CHFG (ELE) = " "
                 IF NEW-FLAG NOT = 1
                    GO TO MODIFY-ROUTINE.

           IF CHFG(ELE) = "N"
              IF NEW-FLAG = 1
                 GO TO CREATE-ROUTINE
              ELSE
                 GO TO MODIFY-ROUTINE.

           PERFORM SEND-PROMPT.

           MOVE SPEC(ELE) TO VDU.
           MOVE ELE TO VDUROW.
           MOVE 0 TO VDUCOL.
           MOVE "." TO VDUDECIMAL.
           PERFORM SCREENIO.

      * ALLOW UP ARROW TO ADDON DATE:
           IF NEW-FLAG = 1
              MOVE " " TO CHFG(1).
           PERFORM RESET-LEGEND.
       TEST-F1.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.
       TEST-F2.
           IF VDUSTATUS = "12"
              PERFORM CALL-MEMO
              GO TO ENTER-ELE.
       TEST-F3.
           IF VDUSTATUS = "16"
              PERFORM DISPLAY-ALL-LOANS
              GO TO ENTER-ELE.
       TEST-F4.
           IF VDUSTATUS = "18"
              PERFORM SEND-PAYMENT-LINES
              GO TO ENTER-ELE.
       TEST-F5.
           IF VDUSTATUS = "1:"
              PERFORM SEND-PAYOFF
              GO TO ENTER-ELE.
       TEST-UP.
           IF VDUSTATUS NOT = "1U"
              GO TO TEST-DOWN.
           IF NEW-FLAG = 1
              MOVE -1 TO ONE
              GO TO CREATE-ROUTINE.
       TEST-DOWN.
           IF VDUSTATUS NOT = "1D"
              GO TO TEST-F6.
           MOVE 1 TO ONE.
           GO TO GOTO-FIELD.
       TEST-F6.
           IF VDUSTATUS NOT = "1<"
              GO TO TEST-F7.
       TEST-F7.
           IF VDUSTATUS NOT = "1>"
              GO TO TEST-00.

           DESTROY ALL.
           PERFORM MENU-SELECTION.
           PERFORM SAVE-SCREEN.
           IF MENU-FG = 99
              GO TO ENTER-ELE.

           MOVE "END" TO IO-TYPE.
           GO TO END-ROUTINE.

       TEST-00.
           IF VDUSTATUS NOT = "00"
              GO TO ENTER-ELE.
       GOTO-FIELD.
           MOVE "Y" TO COMPUTE-LOAN-FG.
           MOVE 1 TO ONE.
           GO TO F-01 F-02 F-03 F-04 F-05 F-06 F-07
                 F-08 F-09 F-10 F-11 F-12 F-13
                 DEPENDING ON ELE.
           GO TO END-ENTRY-MODULE.

      *************************************
      *    S E T   E L E M E N T S        *
      *************************************
       F-01.

      * ADDON DATE
      
           MOVE VDU-DATE-YYYYMMDD TO SYS-DATE PD-LOANDATE.
           IF SYS-DATE > TRANS-DATE
              PERFORM RESET-LEGEND
              MOVE "FUTURE DATING IS NOT ALLOWED!!" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.

           IF SP-ADDON-EXCEPT-FG = "T"
              MOVE TRANS-DATE  TO PD-INTDATE
           ELSE
              MOVE PD-LOANDATE TO PD-INTDATE.

           GO TO CREATE-ROUTINE.
       F-02.
           MOVE VDUBUF TO PD-SRCD CD-CODE.
           IF NOT (PD-SRCD = "RT" OR "ET" OR "NP" OR "LP" OR "ST")
              MOVE " " TO LEGEND
              PERFORM SEND-LEGEND
              MOVE "ENTER: 'RT', 'NP', 'LP', 'ET' OR 'ST' FOR CANCEL"
                                                             TO MESS
              INSPECT MESS REPLACING ALL "'" BY WK-QUOTE
              PERFORM SEND-MESS
              GO TO ENTER-ELE.

           MOVE "SR" TO CD-TYPE.
           PERFORM GET-CD.
           MOVE "S  A000021." TO VDU.
           MOVE ELE TO VDUROW.
           MOVE CD-DESC TO VDUBUF.
           PERFORM SCREENIO.
           IF IO-FG = 9
              GO TO ENTER-ELE.

           IF PD-SRCD = "ET"
              MOVE "Y" TO ONLY-1STPYDATE-FG.

           PERFORM CALCULATE-1STPYDATE-TERM.
           MOVE PD-1STPYDATE TO PD-ORIG-1STPYDATE DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO D-1STPYDATE.
           
           MOVE PD-ORGTERM TO D-ORGTERM.
           MOVE "<--" TO D-ARROW1
           MOVE 1STPY-BUF TO WR-BUF
           MOVE 1STPY-LIST TO WR-LIST
           PERFORM CALL-WRFORM

      * TEST FOR LAST PAYMENT DATE > MATURITY DATE
      
           IF SP-ADDON-ORG-MATURITY = "O"
              MOVE "Y" TO MDTE-ORGTERM-FG.
           PERFORM MATURITY-DATE-CALCULATION.
           IF TRANS-DATE NOT < MDTE-DATE
              PERFORM RESET-LEGEND
              MOVE "ADDONS NOT ALLOWED ON OR AFTER MATURITY " TO MESS
              PERFORM SEND-MESS
              MOVE 0 TO NEW-PROG-SUB
              MOVE "P" TO ERRCD
              GO TO END-ROUTINE.

      * TEST FOR 1ST PAYMENT DATE > MATURITY DATE
      
           IF PD-SRCD = "RT" OR "NP" OR "LP" OR "ST"
              IF MDTE-DATE < PD-1STPYDATE
                 PERFORM RESET-LEGEND
                 MOVE "FIRST PAYMENT WILL BE AFTER MATURITY" TO MESS
                 PERFORM SEND-MESS
                 MOVE "SHOULD USE 'ET' TO EXTEND TERM" TO MESS
                 INSPECT MESS REPLACING ALL "'" BY WK-QUOTE
                 PERFORM SEND-MESS
                 GO TO ENTER-ELE.

           IF PD-SRCD NOT = "ST"
              MOVE "S  A030022." TO VDU
              MOVE " "           TO VDUBUF
              PERFORM SCREENIO
              GO TO CREATE-ROUTINE.

       F-022.
           MOVE PD-1STPYDATE TO NUM-DATE.
           MOVE MDTE-DATE TO SYS-DATE.
           PERFORM TIMUPER.
           ADD 1 TO ELAPSED-UNITPER GIVING DISP-NUM0.
           MOVE " " TO LEGEND.
           STRING  "^, REMAIN TERM: " DISP-NUM0
                   ",  ENTER # OF PERIODS"
                   DELIMITED BY SIZE INTO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "ENNN030022." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U" GO TO ENTER-ELE.
           IF VDUSTATUS = "10" GO TO ENTER-ELE.
           MOVE VDUNUM0 TO SCHEDULED-TERM.
           IF SCHEDULED-TERM = 0
              GO TO F-022.
           IF SCHEDULED-TERM = 1
              PERFORM RESET-LEGEND
              MOVE "USE NEXT PAY 'NP' ADDON TYPE" TO MESS
              INSPECT MESS REPLACING ALL "'" BY WK-QUOTE
              PERFORM SEND-MESS
              GO TO F-022.

      * TEST ENTERED TERM > REMAINING TERM
      *                   = REMAINING TERM
      
           MOVE LN-UNITPER-CD TO DATER-UNITPER-CD.
           MOVE LN-UNITPER-FREQ TO DATER-UNITPER-FREQ.
           MOVE PD-1STPYDATE TO NDTE-DATE.
           SUBTRACT 1 FROM SCHEDULED-TERM GIVING NDTE-HOLD.
           PERFORM INCREMENT-UNITPER.
           IF NDTE-DATE > MDTE-DATE
              PERFORM RESET-LEGEND
              MOVE PD-1STPYDATE TO NUM-DATE
              MOVE MDTE-DATE TO SYS-DATE
              PERFORM TIMUPER
              ADD 1 TO ELAPSED-UNITPER GIVING DISP-NUM0
              MOVE " " TO MESS
              STRING "SCHEDULED TERM EXCEEDS REMAINING TERM OF "
                      DISP-NUM0 DELIMITED BY SIZE INTO MESS
              PERFORM SEND-MESS
              GO TO F-022.
           IF SYS-IS-EQ
              PERFORM RESET-LEGEND
              MOVE "SCHEDULED TERM EQUALS REMAINING TERM - USE 'RT'"
                                                             TO MESS
              INSPECT MESS REPLACING ALL "'" BY WK-QUOTE
              PERFORM SEND-MESS
              GO TO F-022.

           GO TO CREATE-ROUTINE.

       F-03.
           MOVE "N" TO CHFG(8)  CHFG(9)  CHFG(10)
                       CHFG(11) CHFG(12) CHFG(13).

      * ENTER TYPE OF ADDON

           MOVE VDUNUM0 TO PD-PROCASH.
           IF PD-PROCASH < 0 OR > 6
              GO TO ENTER-ELE.

           IF PD-PROCASH NOT = 0
              MOVE PD-PROCASH TO ITRM-SUB
              MOVE PD-LOANDATE TO ITRM-TODAY
              PERFORM INS-TERM-CALCULATION
              IF ITRM-INEFFECT-FG = "Y"
                 PERFORM RESET-LEGEND
                 MOVE "INSURANCE IS STILL IN EFFECT!!" TO MESS
                 PERFORM SEND-MESS
                 GO TO ENTER-ELE.

           IF PD-PROCASH = 0
              MOVE "******   L O A N   A D D O N   ******"
                    TO MESSAGE-BUF
           ELSE
              MOVE "**  I N S U R A N C E   A D D O N  **"
                    TO MESSAGE-BUF.
           MOVE MESSAGE-BUF TO WR-BUF.
           MOVE MESSAGE-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

           PERFORM RESET-LEGEND.

      * ONLY ALLOW ENTRY OF SELECTED INSURANCE
      
           IF PD-PROCASH NOT = 0
              MOVE " " TO CHFG(ITRM-SUB + 7).

      * IF CL, AH OR PP, MUST ENTER TYPE:
      * IF CL, AH OR PP, DO NOT ALLOW FEES:
      
           IF PD-PROCASH = 1 OR 2 OR 3 OR 4
              MOVE " " TO CHFG(5)
              MOVE "N" TO CHFG(6)
           ELSE
              MOVE "N" TO CHFG(5)
              MOVE " " TO CHFG(6).

      * CLEAR INSURANCE CODES FOR O2 AND O3

           IF PD-PROCASH = 5 OR 6
              MOVE "S  A030050." TO VDU
              MOVE " "           TO VDUBUF
              PERFORM SCREENIO.

           GO TO CREATE-ROUTINE.

       F-04.
           GO TO CREATE-ROUTINE.
       F-05.
           MOVE VDUBUF TO PD-INSCODES.
           IF PD-INSURED NOT = "M" AND
                          NOT = "J" AND
                           NOT = "S"
              PERFORM RESET-LEGEND
              MOVE "ENTER: 'M', 'S' OR 'J'" TO MESS
              INSPECT MESS REPLACING ALL "'" BY WK-QUOTE
              PERFORM SEND-MESS
              GO TO ENTER-ELE.

           IF NOT PD-INSTYPES-VALID
              GO TO ENTER-ELE.

           IF NOT PD-INSELIGIBLE-VALID
              GO TO ENTER-ELE.

           PERFORM VALID-INS-TYPES-ELIG.
           IF IO-FG = 9
              GO TO ENTER-ELE.

           GO TO CREATE-ROUTINE.
       F-06.
           MOVE 1 TO I-LEVEL.
           PERFORM FEE-ENTRY.
           IF IO-FG = 9
              GO TO ENTER-ELE.
           IF PD-FEECODE(1) = " "
              GO TO CREATE-ROUTINE.
           IF BR-FORCE-VALID-CITY-COUNTY = "Y"
              GO TO CREATE-ROUTINE.
       F-062.
           MOVE "E  A020062." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE.
           IF VDUSTATUS NOT = "00"
              GO TO F-062.
           MOVE 2 TO I-LEVEL.
           PERFORM FEE-ENTRY.
           IF IO-FG = 9
              GO TO F-062.
           GO TO CREATE-ROUTINE.
       F-07.
           MOVE VDUNUM3 TO PD-OVRDRATE.
           IF LN-LOANTYPE = "I"
              IF PD-OVRDRATE NOT = 0 AND NOT = LN-SMPRATE
                 PERFORM RESET-LEGEND
                 MOVE "MUST ENTER 0 OR RATE OF ORIGINAL LOAN" TO MESS
                 PERFORM SEND-MESS
                 MOVE "SNNN033070." TO VDU
                 MOVE LN-SMPRATE TO VDUNUM3 PD-OVRDRATE
                 PERFORM SCREENIO
                 GO TO ENTER-ELE.
       CONT-F-07.
           MOVE "E  A010071." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO CONT-F-07.
           MOVE VDUBUF TO PD-OVRDRATE-FG.
           IF PD-OVRDRATE-FG = "A"
              PERFORM RESET-LEGEND
              MOVE "NO APR OVERRIDE ! " TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           IF SP-ADDON-RATE-FG = "F"
              IF NOT (PD-OVRDRATE-FG = "F" OR "C")
                 MOVE "ENTER 'C' CALCULATE OR 'F' FIXED RATE ONLY"
                                                           TO MESS
                 PERFORM SEND-MESS
                 GO TO CONT-F-07
              END-IF
           ELSE
              IF NOT (PD-OVRDRATE-FG = "C" OR "E" OR "A") 
                 PERFORM ICE-OR-A-MSG
                 GO TO CONT-F-07.

           IF PD-OVRDRATE-FG = "C"
              IF SP-ADDON-RATE-FG = "F"
                 MOVE "SNNN033070." TO VDU
                 MOVE SP-ADDON-RATE TO PD-OVRDRATE
                                       VDUNUM3
                 PERFORM SCREENIO
                 MOVE "S  A010071." TO VDU
                 MOVE "F" TO PD-OVRDRATE-FG VDUBUF
                 PERFORM SCREENIO
              ELSE
              IF SP-ADDON-RATE-FG = "A"
                 MOVE "SNNN033070." TO VDU
                 MOVE LN-APRATE TO PD-OVRDRATE
                                   PD-SMPRATE
                                   VDUNUM3
                                   PD-APRATE
                 PERFORM SCREENIO
                 MOVE "S  A010071." TO VDU
                 MOVE "A" TO PD-OVRDRATE-FG VDUBUF
                 PERFORM SCREENIO
              ELSE
                 MOVE LN-SMPRATE TO PD-OVRDRATE PD-SMPRATE.
           IF PD-OVRDRATE-FG = "A"
              IF PD-OVRDRATE > LN-APRATE
                 PERFORM RESET-LEGEND
                 MOVE LN-APRATE TO ERROREFF-EFF
                 MOVE ERROREFF TO MESS
                 PERFORM SEND-MESS
                 GO TO ENTER-ELE.
           IF PD-OVRDRATE-FG = "E"
              IF PD-OVRDRATE > LN-SMPRATE
                 PERFORM RESET-LEGEND
                 MOVE LN-SMPRATE TO ERROREFF-EFF
                 MOVE ERROREFF TO MESS
                 PERFORM SEND-MESS
                 GO TO ENTER-ELE.
           IF PD-OVRDRATE-FG = "F"
              IF PD-OVRDRATE > SP-ADDON-RATE
                 PERFORM RESET-LEGEND
                 MOVE SP-ADDON-RATE TO ERRORSPR-SPR
                 MOVE ERRORSPR TO MESS
                 PERFORM SEND-MESS
                 GO TO ENTER-ELE.

           MOVE PD-OVRDRATE TO PD-SMPRATE.
           GO TO CREATE-ROUTINE.
       F-08.

      * CL CREDIT LIFE

           PERFORM INSURANCE-ENTRY.
           IF IO-FG = 9
              MOVE 0 TO IO-FG
              GO TO ENTER-ELE.

           GO TO CREATE-ROUTINE.
       F-09.

      * A&H ACCIDENT & HEALTH
      
           PERFORM INSURANCE-ENTRY.
           IF IO-FG = 9
              MOVE 0 TO IO-FG
              GO TO ENTER-ELE.

           GO TO CREATE-ROUTINE.
       F-10.

      * PP PERSONAL PROPERTY
      
           PERFORM INSURANCE-ENTRY.
           IF IO-FG = 9
              MOVE 0 TO IO-FG
              GO TO ENTER-ELE.

           GO TO CREATE-ROUTINE.
       F-11.

      * O1 OTHER 1
      
           PERFORM INSURANCE-ENTRY.
           IF IO-FG = 9
              MOVE 0 TO IO-FG
              GO TO ENTER-ELE.

           GO TO CREATE-ROUTINE.
       F-12.

      * O2 OTHER 2
      
           PERFORM INSURANCE-ENTRY.
           IF IO-FG = 9
              MOVE 0 TO IO-FG
              GO TO ENTER-ELE.

           GO TO CREATE-ROUTINE.
       F-13.
      
      * O3 OTHER 3
      
           PERFORM INSURANCE-ENTRY.
           IF IO-FG = 9
              MOVE 0 TO IO-FG
              GO TO ENTER-ELE.

           GO TO CREATE-ROUTINE.

       END-ENTRY-MODULE.
           EXIT.

     **********************************************************************
       INSCOMP-SCAN SECTION.
     **********************************************************************
           MOVE 0 TO ICSCAN-KEY.

           MOVE "WI/ICSCAN" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH ICSCAN-BUF EXIT-PATHNAME.
           CANCEL FORM-PROGX.

           IF ICSCAN-STATUS NOT = "00" OR ICSCAN-KEY = ZEROES
              MOVE 9 TO IO-FG.

       EXIT-INSCOMP-SCAN.
           EXIT.

      ***********************************
       INSURANCE-ENTRY SECTION.
      ***********************************
           MOVE 0 TO IO-FG.
           SUBTRACT 7 FROM ELE GIVING I-LEVEL.
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
           IF I-LEVEL > 4 AND GP-OT-INS-TERM NOT = "Y"
              MOVE 0 TO PD-INSEXP-DATE(I-LEVEL).
       ENTER-IC-OURS.
           IF IC-CODE = 0
              MOVE " " TO PD-INSOURS(I-LEVEL)
              GO TO SEND-IC-OURS.
           IF ELE = 13
              IF GP-DEFAULT-O3-INSOURS NOT = " "
                 MOVE "S  A010131." TO VDU
                 MOVE GP-DEFAULT-O3-INSOURS TO VDUBUF
                 PERFORM SCREENIO.

           MOVE "IS THE INSURANCE OURS? (Y/N) SPACE = Y" TO LEGEND.
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
           IF VDUBUF NOT = "Y"
              IF VDUBUF NOT = "M"
                 MOVE "Y-YES, M-MEMO" TO MESS
                 PERFORM SEND-MESS
                 GO TO ENTER-IC-OURS.
           IF GP-DEFAULT-O3-INSOURS NOT = " "
              IF ELE = 13
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

           IF ELE = 12
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
           IF ELE = 13
              IF GP-DEFAULT-O3-INS-COMPANY NOT = 0
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
           MOVE "ENTER THE TERM OF INSURANCE IN MONTHS" TO LEGEND.
           PERFORM SEND-LEGEND.
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
           MOVE "ENTER THE EXPIRATION DATE OF THE INSURANCE" TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "RNNN060  2." TO VDU.
           MOVE ELE TO VDUROW.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-IC-OURS.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO ENTER-IC-EXP.
           IF VDUNUM0 = 0
              GO TO ENTER-IC-EXP.
           MOVE VDUNUM0 TO DATE-MMDDYY.
           PERFORM CONVERT-MMDDYY-TO-YYYYMMDD.
           MOVE DATE-YYYYMMDD TO PD-INSEXP-DATE(I-LEVEL) NUM-DATE.
           PERFORM TSTDAT.
           IF NUM-DATE = 0
              MOVE "INVALID DATE" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-IC-EXP.
           IF GP-OT-INS-TERM NOT = "Y"
              IF PD-INSEXP-DATE(I-LEVEL) NOT > PD-LOANDATE
                 MOVE "EXPIRATION DATE MUST BE AFTER ADDON DATE" TO
                   MESS PERFORM SEND-MESS
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
           MOVE "ENTER THE AMOUNT OF INSURANCE COVERAGE" TO LEGEND.
           PERFORM SEND-LEGEND.
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
           MOVE "ENTER THE AMOUNT OF INSURANCE PREMIUM" TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "RNNN042  4." TO VDU.
           MOVE ELE TO VDUROW.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-IC-COVR.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO ENTER-IC-PREM.
           MOVE PD-INSPREM(I-LEVEL) TO INSPREM-HLDR.
           MOVE VDUNUM2 TO PD-INSPREM(I-LEVEL).


       SEND-IC-PREM.
           MOVE "SN N042  4." TO VDU.
           MOVE ELE TO VDUROW.
           MOVE PD-INSPREM(I-LEVEL) TO VDUNUM2.
           PERFORM SCREENIO.
           IF I-LEVEL > 4
              MOVE "I" TO PD-INS-FG(I-LEVEL)
              IF GP-OT-INS-TERM NOT = "Y"
                 MOVE PD-LOANDATE TO NUM-DATE
                 MOVE PD-INSEXP-DATE(I-LEVEL) TO SYS-DATE
                 PERFORM TIM360
                 MOVE ELAPSED-MONTHS TO PD-INSTRM(I-LEVEL).

      * SEE IF THERE WILL BE TAX ON INSURANCE PREMIUMS
      * NOTE:
      *       CD-CY-TYPE = " " -> INDICATES NO CITY SALES TAX
      *       CD-CT-TYPE = " " -> INDICATES NO COUNTY SALES TAX
      
           IF BR-FORCE-VALID-CITY-COUNTY = "Y"
              MOVE "CY"             TO CD-CCY-TYPE
              MOVE BW-1-STATE       TO CD-CCY-STATE
              MOVE BW-1-CITY-TAX-CD TO CD-CCY-CITY
              MOVE " "              TO CD-CCY-FILLER
              MOVE "CT"             TO CD-CCT-TYPE
              MOVE BW-1-STATE       TO CD-CCT-STATE
              MOVE BW-1-COUNTY      TO CD-CCT-COUNTY
              MOVE " "              TO CD-CCT-FILLER
              MOVE "CL/INSTAX" TO FORM-NAM
              CALL FORM-PROGX USING FORM-PATH CD-CCY-REC CD-CCT-REC
              CANCEL FORM-PROGX
              MOVE PD-INSPREM(I-LEVEL) TO WK-INS-PREMIUM
              PERFORM SET-SPR-SUB
              PERFORM COW-INS-TAX
              IF WK-INS-TAX = 0
                 MOVE 0   TO PD-FEECODE(2)
                             PD-FEEAMT(2)
                 MOVE " " TO PD-FEEOURS(2)
                             PD-FEEFINCHG(2)
                 MOVE "S  A000062." TO VDU
                 PERFORM SCREENIO
                 MOVE "S  A000063." TO VDU
                 PERFORM SCREENIO
              ELSE
                 MOVE "Y"        TO PD-FEEOURS(2)
                 MOVE " "        TO PD-FEEFINCHG(2)
                 MOVE "S  A000062." TO VDU
                 IF I-LEVEL = 1
                    MOVE "TC"    TO PD-FEECODE(2) VDUBUF
                 ELSE
                  IF I-LEVEL = 2
                     MOVE "TA"    TO PD-FEECODE(2) VDUBUF
                  ELSE
                   IF I-LEVEL = 3
                      MOVE "TP"    TO PD-FEECODE(2) VDUBUF
                   ELSE
                    IF I-LEVEL = 4
                       MOVE "T1"    TO PD-FEECODE(2) VDUBUF
                    ELSE
                     IF I-LEVEL = 5
                        MOVE "T2"    TO PD-FEECODE(2) VDUBUF
                     ELSE
                      IF I-LEVEL = 6
                         MOVE "T3"    TO PD-FEECODE(2) VDUBUF
                      END-IF
                     END-IF
                    END-IF
                   END-IF
                  END-IF
                 END-IF
                 PERFORM SCREENIO
                 MOVE "SN N042063." TO VDU
                 MOVE WK-INS-TAX    TO PD-FEEAMT(2)
                                       VDUNUM2
                 PERFORM SCREENIO
              END-IF
           ELSE
              MOVE SPACES TO CD-CCY-REC CD-CCT-REC.
   
       EXIT-ENTER-INSURANCE.
           EXIT.

      ***********************************
       FEE-ENTRY SECTION.
      ***********************************

           MOVE VDUBUF TO CDB-CODE
                          PD-FEECODE(I-LEVEL).

      * WHEN A BRANCH IS FLAGED FOR INS. SALES TAX VIA
      * BR-FORCE-VALID-CITY-COUNTY = "Y", DONT ALLOW ENTRY OF INS.
      * SALES TAX CODES! THEY HAVE TO ENTER "TM" MANUALLY IN KENTUCKY

      * DONT ALLOW AT ALL RIGHT NOW!
           IF CDB-CODE = "TM"
              MOVE "UMC SALES TAX CODE NOT ALLOWED" TO MESS
              PERFORM SEND-MESS
              MOVE 9 TO IO-FG
              GO TO EXIT-FEE-ENTRY.

           IF CDB-CODE NOT = " "
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
                 GO TO ENTER-FEEAMT
           ELSE
              IF PD-PROCASH = 0 AND I-LEVEL = 1
                 MOVE 9 TO IO-FG
                 GO TO EXIT-FEE-ENTRY.

           IF I-LEVEL = 2 OR 4 OR 6 OR 8 OR 10
              GO TO GL-CLEAR-2.
       NEXT-GL-CLEAR.
           MOVE "S  A000  0." TO VDU.
           MOVE ELE           TO VDUROW.
           MOVE " " TO PD-FEECODE(I-LEVEL) VDUBUF.
           PERFORM SCREENIO.
           MOVE "SN N042  1." TO VDU.
           MOVE ELE           TO VDUROW.
           MOVE 0 TO PD-FEEAMT(I-LEVEL) VDUNUM0.
           PERFORM SCREENIO.
           MOVE " " TO PD-FEEFINCHG(I-LEVEL).
           ADD 1 TO I-LEVEL.
       GL-CLEAR-2.
           IF BR-FORCE-VALID-CITY-COUNTY NOT = "Y"
              MOVE "S  A020  2." TO VDU
              MOVE ELE           TO VDUROW
              MOVE " " TO PD-FEECODE(I-LEVEL) VDUBUF
              PERFORM SCREENIO
              MOVE "SN N042  3." TO VDU
              MOVE ELE           TO VDUROW
              MOVE 0 TO PD-FEEAMT(I-LEVEL) VDUNUM0
              PERFORM SCREENIO
              MOVE " " TO PD-FEEFINCHG(I-LEVEL).
           IF I-LEVEL < 2
              ADD 1 TO I-LEVEL ELE
              GO TO NEXT-GL-CLEAR.
           MOVE 0 TO IO-FG.
           GO TO CALC-ON-FEE-ENTRY.
       ENTER-FEEAMT.
           IF BR-FORCE-VALID-CITY-COUNTY = "Y"
              IF I-LEVEL NOT = 1
                 GO TO CALC-ON-FEE-ENTRY.
           IF I-LEVEL > 2
              SUBTRACT 1 FROM I-LEVEL GIVING SUB
              IF PD-FEECODE(SUB) = " "
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

           IF ( PD-FEEAMT(I-LEVEL) > CDB-GC-FEE-MAXAMT )
              MOVE "INVALID: AMOUNT IS GREATER THAN THE LIMIT" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-FEEAMT.

       CALC-ON-FEE-ENTRY.
           PERFORM COMPUTE-GL-FEEAMT.

           MOVE " " TO LEGEND.
           PERFORM SEND-LEGEND.

       EXIT-FEE-ENTRY.
           EXIT.

      *********************************
       NEW-RECORD-SETUP SECTION.
      *********************************
           MOVE 1 TO NEW-FLAG.
           PERFORM CLEAR-PD-FILE.

           MOVE BRANCH TO BR-NO.
           PERFORM OPEN-BR-FILE.
           PERFORM READ-BR-FILE.
           PERFORM CLOSE-BR-FILE.
           IF IO-FG = 9
              GO TO IO-ERROR.

           MOVE TRANS-DATE TO PD-LOANDATE DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO DEF-LOANDATE.
     
           MOVE BR-STATE TO DEF-ORGST PD-ORGST PD-OWNST.
           MOVE BRANCH TO DEF-ORGBR PD-ORGBR PD-OWNBR.
           MOVE 999.99 TO PD-SERVPER.
           MOVE 999.999 TO PD-ENDORATE PD-OVRDRATE.
           MOVE "E" TO PD-OVRDRATE-FG.
           MOVE "I" TO PD-INS-FG(1) PD-INS-FG(2) PD-INS-FG(3).
           MOVE "C" TO PD-SERCHG-FG PD-MAINTFEE-FG
                       PD-EXTCHG-FG PD-REGPYAMT-FG.
           MOVE 0 TO TOTAL-GL-HLDR
                     TOTAL-GL-HLDR-SALESTAX
                     TOTAL-GL-FINCHG-HLDR
                     TOTAL-GL-FINCHG-HLDR-SALESTAX
                     TERM-LOW
                     TERM-HIGH
                     PROCEEDS-LOW
                     PROCEEDS-HIGH.

           MOVE "******   L O A N   A D D O N   ******" TO MESSAGE-BUF.
           MOVE MESSAGE-BUF TO WR-BUF.
           MOVE MESSAGE-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.
           MOVE "00" TO STAT.

           MOVE "Y" TO DEFAULT-APR-FG.
           MOVE 0 TO ELE.

      *************************************
       SAVE-SCREEN SECTION.
      *************************************
           MOVE 8 TO WK-KEY.
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

      *************************************
       RESTORE-SCREEN SECTION.
      *************************************
           MOVE 8 TO WK-KEY.
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

      **********************************
       PROCESS-ORIGINAL-LOAN SECTION.
      **********************************

           PERFORM OPEN-LN1-FILE.
           PERFORM READ-LN1-FILE.
           PERFORM CLOSE-LN1-FILE.
           IF IO-FG = 9
              GO TO END-PROCESS-ORIGINAL-LOAN.

           IF ( LN-POCD-NONLOAN )
            OR (LN-CURBAL = 0)
             OR (LN-JDDATE NOT = 0)
              OR (LN-PLDATE NOT = 0)
               OR (LN-ACCELDATE NOT = 0)
                OR (LN-INT-REBATE = "Y" OR "R")
                    MOVE 8 TO IO-FG
                    GO TO END-PROCESS-ORIGINAL-LOAN.

           MOVE LN-ACCTNO TO PD-ACCTNO.

      * STORE THE ORIGINAL LOAN SOURCE CODE IN POCD FOR THE UPDATE PROGRAM
      
           MOVE LN-SRCD TO PD-POCD.

           MOVE LN-CLASS TO LC-CODE.
           PERFORM OPEN-LC1-FILE.
           PERFORM READ-LC1-FILE.
           PERFORM CLOSE-LC1-FILE.
           IF IO-FG = 9
              MOVE "INVALID" TO LC-DESC.

           MOVE LC-CODE     TO PD-CLASS.
           MOVE LC-SMALLOAN TO PD-SMALLOAN.
           MOVE LC-SALEFIN  TO PD-SALEFIN.
           MOVE LC-SERVACCT TO PD-SERVACCT.
           MOVE LC-LOANTYPE TO PD-LOANTYPE.
           MOVE 1 TO PD-STMT-COUNTER.

           PERFORM OPEN-BW1-FILE.
           MOVE 1 TO IX1.
       PROCESS-NEXT-BW.
           MOVE LN-SSNO(IX1) TO BW-SSNO PD-SSNO(IX1).
           PERFORM NUMBW-TO-ALPBW.
           MOVE " " TO BW-LNAME BW-FNAME.
           IF BW-SSNO NOT = 0
              PERFORM GET-BW1
              MOVE ALP-BW TO D-SSNO(IX1)
              MOVE INSPECT-W TO D-BORR(IX1)
              MOVE LN-MAKERCD(IX1) TO  D-MAKERCD(IX1)
                                      PD-MAKERCD(IX1).
           IF IX1 < 4
              ADD 1 TO IX1
              GO TO PROCESS-NEXT-BW.
           PERFORM CLOSE-BW1-FILE.

           MOVE LN-ORGST    TO PD-ORGST.
           MOVE LN-SPRCLASS TO PD-SPRCLASS.
           MOVE LN-SUBCLASS TO PD-SUBCLASS.
           MOVE LN-LAWCODE  TO PD-LAWCODE.
           MOVE PD-LOANDATE TO PD-INTDATE.
           MOVE LN-FILEDATE TO PD-FILEDATE.

           PERFORM GET-SP.

           MOVE LN-SMPRATE TO PD-SMPRATE
                              D-EFF
                              D-SMP.
           MOVE LN-EFFRATE TO PD-EFFRATE.
           MOVE LN-APRATE  TO PD-APRATE
                              D-APR.

           IF SP-ADDON-RATE-FG = "F"
              MOVE SP-ADDON-RATE TO PD-OVRDRATE
              MOVE "F" TO PD-OVRDRATE-FG
           ELSE
           IF SP-ADDON-RATE-FG = "A"
              MOVE LN-APRATE TO PD-OVRDRATE
                                PD-SMPRATE
                                D-EFF
                                D-SMP
              MOVE "A" TO PD-OVRDRATE-FG
           ELSE
              MOVE "E" TO PD-OVRDRATE-FG.

           MOVE PD-OVRDRATE    TO D-OVR.
           MOVE PD-OVRDRATE-FG TO D-OVR-FG.

           MOVE PD-1STPYDATE TO PD-ORIG-1STPYDATE DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO D-1STPYDATE.

           MOVE PD-ORGTERM TO D-ORGTERM.

           MOVE LN-ORGST    TO D-ORGST PD-ORGST PD-OWNST.
           MOVE LN-ORGBR    TO D-ORGBR PD-ORGBR PD-OWNBR.
           MOVE SP-SUBCLASS TO D-SUBCLASS.
           MOVE SP-DESC     TO D-SUBDESC.
           MOVE LN-PURCD    TO CD-CODE PD-PURCD.
           MOVE "PU"        TO CD-TYPE.
           PERFORM GET-CD.

           MOVE LN-COLLCD TO CD-CODE D-COLCD PD-COLLCD.
           MOVE "CO"      TO CD-TYPE.
           PERFORM GET-CD.
           MOVE CD-DESC   TO D-COLDESC.
           MOVE CD-CO-SECURED TO SPTEST-CD-SECURED.

           MOVE 0 TO PD-PROCEEDS.

      * SETUP SO INSURANCE ENTRY WILL BE CORRECT
      
           MOVE LN-INSCODES TO D-INSCODE PD-INSCODES.

           MOVE LN-DLNO TO D-DLNO DL-DLNO PD-DLNO.
           PERFORM GET-DL1.
           MOVE DL-SHORT TO D-DLDESC.

           MOVE LN-COLLID TO PD-COLLID.

           MOVE SPACES TO D-COLLATERAL-01.

           PERFORM GET-COLLATERAL.
           IF IO-FG NOT = 0
              GO TO DISPLAY-IC.

           IF LTC-MAKE(1) NOT = SPACES OR
               LTC-MODEL(1) NOT = SPACES OR
                LTC-VIN(1) NOT = SPACES
                   STRING LTC-YEAR(1) "  "
                          LTC-MAKE(1) "  "
                          LTC-MODEL(1) "  "
                          LTC-VIN(1) "  "
                       DELIMITED BY "   " INTO D-COLLATERAL-01
                       GO TO DISPLAY-IC.

           MOVE LTC-COLL-INFO1(1) TO D-COLID01.
           MOVE LTC-COLL-INFO2(1) TO D-COLID02.
           MOVE LTC-COLL-INFO3(1) TO D-COLID03.
           MOVE LTC-COLL-INFO4(1) TO D-COLID04.

       DISPLAY-IC.
           PERFORM VARYING IX1 FROM 1 BY 1 UNTIL IX1 > 4
              MOVE LN-INSCOMP(IX1) TO D-1COMP(IX1)
              MOVE LN-INSPREM(IX1) TO D-1PREM(IX1)
              MOVE IX1 TO ITRM-SUB
              PERFORM INS-TERM-CALCULATION
              MOVE ITRM-INSTERM TO D-1TERM(IX1)
              MOVE LN-INSCOVR(IX1) TO D-1COVR(IX1)
              MOVE LN-INSOURS(IX1) TO D-1OUR(IX1)
           END-PERFORM.

           MOVE 1 TO IX1.
           MOVE 5 TO IX2.
       DISP-NEXT-2IC.
           MOVE LN-INSCOMP(IX2) TO D-2COMP(IX1).
           MOVE LN-INSPREM(IX2) TO D-2PREM(IX1).
           IF GP-OT-INS-TERM = "Y"
              MOVE LN-INSTRM(IX2) TO D-2TERM(IX1)
           ELSE
              MOVE LN-INSEXP-DATE(IX2) TO DATE-YYYYMMDD
              PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
              MOVE DATE-MMDDYY         TO D-2EXP(IX1).
           MOVE LN-INSCOVR(IX2) TO D-2COVR(IX1).
           MOVE LN-INSOURS(IX2) TO D-2OUR(IX1).
           MOVE IX2 TO ITRM-SUB.
           MOVE PD-LOANDATE TO ITRM-TODAY.
           PERFORM INS-TERM-CALCULATION.
           IF ITRM-INEFFECT-FG = "Y"
              MOVE "N" TO CHFG(IX2 + 7).

           IF IX2 < 6
              ADD 1 TO IX1 IX2
              GO TO DISP-NEXT-2IC.

           MOVE LN-CONFIDENTCD TO PD-CONFIDENTCD.
           MOVE LN-RECASTCD    TO PD-RECASTCD.
           MOVE LN-STATUSFG    TO PD-STATUSFG.
           MOVE LN-STATFLAGS   TO PD-STATFLAGS.

           MOVE LN-REDUDATE    TO PD-REDUDATE.
           MOVE LN-LEGALDATE   TO PD-LEGALDATE.
           MOVE LN-BNKRPTDATE  TO PD-BNKRPTDATE.
           MOVE LN-DEBTAIDATE  TO PD-DEBTAIDATE.
           MOVE LN-REPODATE    TO PD-REPODATE.
           MOVE LN-REPOCD      TO PD-REPOCD.

           MOVE LN-PURDATE     TO PD-PURDATE.
           MOVE LN-SUM-REGPYAMT TO PD-SUM-REGPYAMT.
           MOVE 0 TO IO-FG.

       END-PROCESS-ORIGINAL-LOAN.
           EXIT.

      *********************************************
       CALCULATE-1STPY-NEXT SECTION.
      *********************************************

      * CALCULATE THE NEXT PAYMENT DUE ON THE ORIGINAL LOAN
      * BASED ON THE REMAINING TERM, AND SET PD-1STPYDATE WITH DATE
      * SET NEW 1STPYDATE = TODAY:

           MOVE TRANS-DATE TO NDTE-DATE PD-1STPYDATE.

      * IF THE ORIGINAL FIRST PAY DAY IS > (PAST) OR EQUAL TO TODAY,
      * BUMP TO NEXT MONTH

           IF LN-1STPYDA NOT < NDTE-DD
              MOVE LN-1STPYDA TO PD-1STPYDA
           ELSE
              IF LN-UNITPER-CD = "M" AND LN-UNITPER-FREQ = 1
                 MOVE LN-1STPYDA TO NDTE-DD
                 MOVE 1 TO NDTE-HOLD
                 MOVE LN-UNITPER-CD TO DATER-UNITPER-CD
                 MOVE LN-UNITPER-FREQ TO DATER-UNITPER-FREQ
                 PERFORM INCREMENT-UNITPER
                 MOVE NDTE-DATE TO PD-1STPYDATE
              ELSE
                 MOVE LN-1STPYDATE TO NUM-DATE
                 MOVE LN-UNITPER-CD TO DATER-UNITPER-CD
                 MOVE LN-UNITPER-FREQ TO DATER-UNITPER-FREQ
                 MOVE 99 TO ELAPSED-UNITPER-REM
                 PERFORM UNTIL ELAPSED-UNITPER-REM = 0
                     MOVE 1 TO NDTE-HOLD
                     PERFORM INCREMENT-DAYS
                     MOVE NDTE-DATE TO SYS-DATE
                     PERFORM TIMUPER
                 END-PERFORM
                 MOVE NDTE-DATE TO PD-1STPYDATE.

           IF PD-1STPYDATE < LN-1STPYDATE
              MOVE LN-1STPYDATE TO PD-1STPYDATE.

           PERFORM TEST-1STPY-ENOUGH-TIME.

      *************************************************************
       TEST-1STPY-ENOUGH-TIME SECTION.
      *************************************************************

      * COMPARE THE NEW 1STPYDATE TO SP-1STPY-ADDON-DAYS TO SEE
      * IF THERE IS ENOUGH TIME TO NOTIFY CUSTOMER.

           MOVE TRANS-DATE TO NUM-DATE.
           MOVE PD-1STPYDATE TO SYS-DATE.
           MOVE SP-1STPY-ADDON-YRTYPE TO ELAPSED-YRTYPE.
           PERFORM TIMALL.
           IF ELAPSED-DAYS < SP-1STPY-ADDON-DAYS
              MOVE PD-1STPYDATE TO NDTE-DATE-WORK
              MOVE 1 TO NDTE-HOLD
              MOVE LN-UNITPER-CD TO DATER-UNITPER-CD
              MOVE LN-UNITPER-FREQ TO DATER-UNITPER-FREQ
              PERFORM INCREMENT-UNITPER
              MOVE NDTE-DATE TO PD-1STPYDATE
              GO TO TEST-1STPY-ENOUGH-TIME.


      ********************************************************
       CALCULATE-1STPYDATE-TERM SECTION.
      ********************************************************
           MOVE 0 TO PD-LASTPYAMT.

      * LAST PAYMENT (LP):
           IF PD-SRCD = "LP"
              PERFORM MATURITY-DATE-CALCULATION
              MOVE MDTE-DATE TO PD-1STPYDATE
              PERFORM TEST-1STPY-ENOUGH-TIME

      * SET ORGTERM:
              MOVE 1 TO PD-ORGTERM

      * SET ANTICTERM:
              MOVE PD-LOANDATE TO NUM-DATE
              MOVE PD-1STPYDATE TO SYS-DATE
              MOVE LN-UNITPER-CD TO DATER-UNITPER-CD
              MOVE LN-UNITPER-FREQ TO DATER-UNITPER-FREQ
              PERFORM TIMUPER
              MOVE ELAPSED-UNITPER TO PD-ANTICTERM

              PERFORM SET-LP-PAYMENT
              GO TO CALCULATE-1STPYDATE-TERM-EXIT.

      * NEXT PAYMENT (NP):
           IF PD-SRCD = "NP"
              PERFORM CALCULATE-1STPY-NEXT
              MOVE 1 TO PD-ORGTERM
                        PD-ANTICTERM

              PERFORM SET-NP-PAYMENT
              GO TO CALCULATE-1STPYDATE-TERM-EXIT.

      * SCHEDULED TERM (ST):
           IF PD-SRCD = "ST"
              PERFORM CALCULATE-1STPY-NEXT

      * CALCULATE ORGTERM:
              MOVE SCHEDULED-TERM TO PD-ORGTERM

      * CALCULATE ANTICTERM:
              MOVE SCHEDULED-TERM TO PD-ANTICTERM

              MOVE PD-INTDATE   TO NUM-DATE
              MOVE PD-1STPYDATE TO SYS-DATE
              PERFORM TIM360
              IF ELAPSED-MONTHS > 0
                     AND ELAPSED-REM > 0
                      AND PD-1STPYDATE NOT = LN-1STPYDATE
                 ADD 1 TO PD-ANTICTERM
              END-IF

              PERFORM SET-RT-PAYMENT
              GO TO CALCULATE-1STPYDATE-TERM-EXIT.

      * REMAINING TERM (RT):
           IF PD-SRCD = "RT"
              PERFORM CALCULATE-1STPY-NEXT
      * CALCULATE ORGTERM:
              MOVE LN-1STPYDATE TO CEPP1-DATE
              MOVE PD-1STPYDATE TO CEPP2-DATE
              PERFORM CONTRACTUAL-ELAPSED-PAYPER
              COMPUTE PD-ORGTERM =
                    LN-SUM-ORGTERM - CEPP-PERIODS + 1

      * CALCULATE ANTICTERM:
              IF SP-ADDON-ORG-MATURITY = "O"
                 COMPUTE PD-ANTICTERM =
                          LN-ORGTERM - CEPP-PERIODS + 1
              ELSE
                 COMPUTE PD-ANTICTERM =
                          LN-SUM-ORGTERM - CEPP-PERIODS + 1
              END-IF
              MOVE PD-INTDATE   TO NUM-DATE
              MOVE PD-1STPYDATE TO SYS-DATE
              PERFORM TIM360
              IF ELAPSED-MONTHS > 0
                     AND ELAPSED-REM > 0
                      AND PD-1STPYDATE NOT = LN-1STPYDATE
                 ADD 1 TO PD-ANTICTERM
              END-IF

              PERFORM SET-RT-PAYMENT
              GO TO CALCULATE-1STPYDATE-TERM-EXIT.

      * EXTEND EXISTING TERM (ET):

       CALCULATE-1STPYDATE-TERM-ET.
           IF PD-SRCD = "ET"
              PERFORM MATURITY-DATE-CALCULATION
              MOVE MDTE-DATE TO NDTE-DATE
              MOVE 1 TO NDTE-HOLD
              MOVE LN-UNITPER-CD TO DATER-UNITPER-CD
              MOVE LN-UNITPER-FREQ TO DATER-UNITPER-FREQ
              PERFORM INCREMENT-UNITPER
              MOVE NDTE-DATE TO PD-1STPYDATE
              PERFORM TEST-1STPY-ENOUGH-TIME
              IF ONLY-1STPYDATE-FG = "Y"
                 MOVE 0 TO PD-ORGTERM
                 GO TO CALCULATE-1STPYDATE-TERM-EXIT
              END-IF

              PERFORM SET-ET-TERM-AND-PAYMENT
              IF SP-ADDON-ORG-MATURITY = "O"
                 MOVE "Y" TO MDTE-ORGTERM-FG
              END-IF
              PERFORM MATURITY-DATE-CALCULATION
              MOVE PD-LOANDATE TO NUM-DATE
              MOVE MDTE-DATE TO SYS-DATE
              PERFORM TIM360
              IF ELAPSED-MONTHS > 0
                 MOVE ELAPSED-MONTHS TO PD-ANTICTERM
              ELSE
                 MOVE 1 TO PD-ANTICTERM
              END-IF
              IF SP-ADDON-ORG-MATURITY = "U"
                 ADD PD-ORGTERM TO PD-ANTICTERM.

       CALCULATE-1STPYDATE-TERM-EXIT.
           MOVE " " TO ONLY-1STPYDATE-FG.

      *************************************************************
       SET-NP-PAYMENT SECTION.
      *************************************************************
           PERFORM GET-EPAY-PVALUE.
           IF EPAY-PVALUE = 0
              GO TO SET-NP-PAYMENT-EXIT.

           IF PD-SMPRATE = 0
              MOVE 0 TO WS-RATE
              GO TO SET-NP-PAYMENT-PC
           ELSE
              IF PD-LOANTYPE = "P"
                 IF PD-OVRDRATE-FG = "F"
                    COMPUTE WS-RATE ROUNDED = PD-OVRDRATE / 1200
                    GO TO SET-NP-PAYMENT-PC.

      * SET-NP-PAYMENT-IB:
              MOVE PD-ORGTERM TO EPAY-ORGTERM.
              PERFORM CALC-EPAY-CALC.
              MOVE EPAY-PAYMNT TO PD-REGPYAMT.
              GO TO SET-NP-PAYMENT-EXIT.

       SET-NP-PAYMENT-PC.
           COMPUTE FINANCE-CHG = EPAY-PVALUE * WS-RATE
                                              * PD-ORGTERM.
           IF SP-ADDON-WHOLE-FG = "A"
              ADD .5 FINANCE-CHG GIVING WHOLE-DOLLARS
              MOVE WHOLE-DOLLARS TO FINANCE-CHG.
           COMPUTE PD-REGPYAMT = EPAY-PVALUE + FINANCE-CHG.

       SET-NP-PAYMENT-EXIT.
           EXIT.

      *************************************************************
       SET-LP-PAYMENT SECTION.
      *************************************************************
           PERFORM GET-EPAY-PVALUE.
           IF EPAY-PVALUE = 0
              GO TO SET-LP-PAYMENT-EXIT.

           IF PD-SMPRATE = 0
              MOVE 0 TO WS-RATE
              GO TO SET-LP-PAYMENT-PC
           ELSE
              IF PD-LOANTYPE = "P"
                 IF PD-OVRDRATE-FG = "F"
                    COMPUTE WS-RATE ROUNDED = PD-OVRDRATE / 1200
                    GO TO SET-LP-PAYMENT-PC.

      * SET-LP-PAYMENT-IB:
           MOVE PD-ORGTERM TO EPAY-ORGTERM.
           PERFORM CALC-EPAY-CALC.
           MOVE EPAY-PAYMNT TO PD-REGPYAMT.
           IF SP-ADDON-WHOLE-FG = "A"
              COMPUTE WHOLE-DOLLARS = EPAY-PAYMNT + 0.99
              MOVE WHOLE-DOLLARS TO PD-REGPYAMT.
           GO TO SET-LP-PAYMENT-EXIT.

      * THE FOLLOWING (PC) METHOD IS REQUESTED PER MULLEN:
       SET-LP-PAYMENT-PC.
           COMPUTE FINANCE-CHG =
                      EPAY-PVALUE * WS-RATE * PD-ANTICTERM.

           IF SP-ADDON-WHOLE-FG = "A"
              ADD .5 FINANCE-CHG GIVING WHOLE-DOLLARS
              MOVE WHOLE-DOLLARS TO FINANCE-CHG.

           COMPUTE PD-REGPYAMT = EPAY-PVALUE + FINANCE-CHG.

       SET-LP-PAYMENT-EXIT.
           EXIT.

      *************************************************************
       SET-RT-PAYMENT SECTION.
      *************************************************************
           PERFORM GET-EPAY-PVALUE.
           IF EPAY-PVALUE = 0
              GO TO SET-RT-PAYMENT-EXIT.

           IF PD-SMPRATE = 0
              MOVE 0 TO WS-RATE FINANCE-CHG
              GO TO SET-RT-PAYMENT-CONT
           ELSE
              IF PD-LOANTYPE = "P"
                 IF PD-OVRDRATE-FG = "F"
                    COMPUTE WS-RATE ROUNDED = PD-OVRDRATE / 1200
                    COMPUTE FINANCE-CHG = EPAY-PVALUE * WS-RATE
                                                     * PD-ANTICTERM
                    IF SP-ADDON-WHOLE-FG = "A"
                       ADD .5 FINANCE-CHG GIVING WHOLE-DOLLARS
                       MOVE WHOLE-DOLLARS TO FINANCE-CHG
                       GO TO SET-RT-PAYMENT-CONT
                    ELSE
                       GO TO SET-RT-PAYMENT-CONT.

           IF PD-ORGTERM = 1
              MOVE PD-ORGTERM TO EPAY-ORGTERM
              PERFORM CALC-EPAY-CALC
              COMPUTE FINANCE-CHG = EPAY-PAYMNT - EPAY-PVALUE
              IF SP-ADDON-WHOLE-FG = "A"
                 ADD .99 FINANCE-CHG GIVING WHOLE-DOLLARS
                 MOVE WHOLE-DOLLARS TO FINANCE-CHG
                 GO TO SET-RT-PAYMENT-CONT
              ELSE
                 GO TO SET-RT-PAYMENT-CONT.

           IF SP-CONSOLIDATE-RT-FG = "C"
              PERFORM GET-LT-SCHLD
              PERFORM GET-1ST-PAYMNT-INDEX
              MOVE SCHD-SUB        TO ASCHD-SUB
              PERFORM ASCHD-AMOUNT-DUE
              MOVE ASCHD-AMTDUE    TO CART-AMTDUE
              MOVE LN-SUM-REGPYAMT TO CART-SUM-REGPYAMT
           ELSE
              MOVE 0 TO CART-AMTDUE
                        CART-SUM-REGPYAMT.

           MOVE EPAY-PVALUE       TO CART-FINANCED.
           MOVE PD-ORGTERM        TO CART-TERM.

           MOVE PD-INTDATE        TO CART-INTDATE.

           MOVE PD-1STPYDATE      TO CART-1STPYDATE.
           MOVE LN-UNITPER-CD     TO CART-UNITPER-CD.
           MOVE LN-UNITPER-FREQ   TO CART-UNITPER-FREQ.
           MOVE SP-ADDON-DISFRMLA TO CART-DISFRMLA.
           MOVE PD-SMPRATE        TO CART-RATE.
           IF SP-CAL-INTMETHOD-DIS(1)
              IF SP-INTMETHOD-EXACT
                 MOVE 365 TO CART-INTMETHOD.
           PERFORM CART.
           MOVE CART-PAYMENT TO PD-REGPYAMT.
           MOVE CART-BALLOON TO PD-LASTPYAMT.
           GO TO SET-RT-PAYMENT-EXIT.

       SET-RT-PAYMENT-CONT.
           IF SP-CONSOLIDATE-RT-FG = "C"
              PERFORM GET-LT-SCHLD
              PERFORM GET-1ST-PAYMNT-INDEX
              MOVE SCHD-SUB TO ASCHD-SUB
              PERFORM ASCHD-AMOUNT-DUE
              COMPUTE WK-AMOUNT ROUNDED = ASCHD-AMTDUE +
                                    (EPAY-PVALUE + FINANCE-CHG)
              COMPUTE PD-REGPYAMT ROUNDED = LN-SUM-REGPYAMT
                                       - WK-AMOUNT / PD-ORGTERM
              COMPUTE PD-LASTPYAMT =
                 (EPAY-PVALUE + FINANCE-CHG)
                     - ((PD-ORGTERM - 1) * PD-REGPYAMT)
           ELSE
              COMPUTE PD-REGPYAMT ROUNDED =
                  (EPAY-PVALUE + FINANCE-CHG) / PD-ORGTERM
              COMPUTE PD-LASTPYAMT =
                 (EPAY-PVALUE + FINANCE-CHG)
                     - ((PD-ORGTERM - 1) * PD-REGPYAMT).

       SET-RT-PAYMENT-EXIT.
           EXIT.

      *************************************************************
       SET-ET-TERM-AND-PAYMENT SECTION.
      *************************************************************
           PERFORM GET-EPAY-PVALUE.
           IF EPAY-PVALUE = 0
              GO TO SET-ET-PAYMENT-EXIT.

           MOVE 0 TO CART-AMTDUE AMTDUE.
           MOVE LN-SUM-REGPYAMT TO PD-REGPYAMT.

           IF PD-SMPRATE = 0
              GO TO SET-ET-PAYMENT-FLAT
           ELSE
              IF PD-LOANTYPE = "P"
                 IF PD-OVRDRATE-FG = "F"
                    GO TO SET-ET-PAYMENT-FLAT.

      * SET-ET-PAYMENT-EFFECTIVE-RATE:
           MOVE EPAY-PVALUE       TO CART-FINANCED.
           MOVE PD-REGPYAMT       TO CART-PAYMENT.

           MOVE PD-INTDATE        TO CART-INTDATE.

           MOVE PD-1STPYDATE      TO CART-1STPYDATE.
           MOVE LN-UNITPER-CD     TO CART-UNITPER-CD.
           MOVE LN-UNITPER-FREQ   TO CART-UNITPER-FREQ.
           MOVE SP-ADDON-DISFRMLA TO CART-DISFRMLA.
           MOVE PD-SMPRATE        TO CART-RATE.
           MOVE 0                 TO CART-AMTDUE.
           IF SP-CAL-INTMETHOD-DIS(1)
              IF SP-INTMETHOD-EXACT
                 MOVE 365 TO CART-INTMETHOD.
           PERFORM CARTET.
           MOVE CART-TERM TO PD-ORGTERM.
           IF CART-TERM = 1
              MOVE CART-BALLOON TO PD-1STPYAMT
                                   PD-REGPYAMT
           ELSE
              MOVE CART-BALLOON TO PD-LASTPYAMT.
           MOVE CART-AMTDUE TO AMTDUE.
           GO TO SET-ET-PAYMENT-EXIT.


       SET-ET-PAYMENT-FLAT.
           COMPUTE PD-ORGTERM = EPAY-PVALUE / PD-REGPYAMT + 1.

           MOVE 0 TO ET-SUB.
       SET-ET-PAYMENT-AGAIN.
           MOVE LN-1STPYDATE TO CEPP1-DATE.
           MOVE PD-INTDATE   TO CEPP2-DATE.
           MOVE "Y" TO CEPP-INCDEF-FG CEPP-INCALDEL-FG.
           PERFORM CONTRACTUAL-ELAPSED-PAYPER.

           IF SP-ADDON-ORG-MATURITY = "O"
              COMPUTE EPAY-ORGTERM = PD-ORGTERM +
                           (LN-ORGTERM - CEPP-PERIODS)
           ELSE
              COMPUTE EPAY-ORGTERM = PD-ORGTERM +
                           (LN-SUM-ORGTERM - CEPP-PERIODS).


           IF PD-SMPRATE = 0
              MOVE 0 TO WS-RATE
           ELSE
              IF PD-LOANTYPE = "P"
                 IF PD-OVRDRATE-FG = "F"
                    COMPUTE WS-RATE ROUNDED = PD-OVRDRATE / 1200.

           COMPUTE FINANCE-CHG =
               EPAY-PVALUE * WS-RATE * EPAY-ORGTERM.
           IF SP-ADDON-WHOLE-FG = "A"
              ADD .5 FINANCE-CHG GIVING WHOLE-DOLLARS
              MOVE WHOLE-DOLLARS TO FINANCE-CHG.

           COMPUTE PD-LASTPYAMT =
              (EPAY-PVALUE + FINANCE-CHG)
                     - ((PD-ORGTERM - 1) * PD-REGPYAMT).

       SET-ET-PAYMENT-BALLOON.
           IF PD-LASTPYAMT < 0
              SUBTRACT 1 FROM PD-ORGTERM
              GO TO SET-ET-PAYMENT-AGAIN
           ELSE
              IF ET-SUB < 3
                 IF PD-LASTPYAMT > PD-REGPYAMT
                    COMPUTE PD-ORGTERM = PD-ORGTERM
                                 + (PD-LASTPYAMT / PD-REGPYAMT)
                    ADD 1 TO ET-SUB
                    GO TO SET-ET-PAYMENT-AGAIN.

           IF PD-ORGTERM = 1
              MOVE PD-LASTPYAMT TO PD-1STPYAMT
                                   PD-REGPYAMT
              MOVE 0 TO PD-LASTPYAMT.
       SET-ET-PAYMENT-EXIT.
           EXIT.

      **********************************************************
       GET-EPAY-PVALUE SECTION.
      **********************************************************
           COMPUTE EPAY-PVALUE =
                 PD-INSPREM(1) + PD-INSPREM(2) + PD-INSPREM(3)
               + PD-INSPREM(4) + PD-INSPREM(5) + PD-INSPREM(6)
               + PD-FEEAMT(1)  + PD-FEEAMT(2)  + PD-FEEAMT(3)
               + PD-FEEAMT(4)  + PD-FEEAMT(5)  + PD-FEEAMT(6)
               + PD-FEEAMT(7)  + PD-FEEAMT(8)  + PD-FEEAMT(9)
               + PD-FEEAMT(10).

      ***************************************************
       CALC-EPAY-CALC SECTION.
      ***************************************************
           MOVE PD-SMPRATE        TO EPAY-RATE.

           MOVE PD-INTDATE        TO EPAY-INTDATE.

           MOVE PD-1STPYDATE      TO EPAY-1STPYDATE.
           MOVE LN-UNITPER-CD     TO EPAY-UNITPER-CD.
           MOVE LN-UNITPER-FREQ   TO EPAY-UNITPER-FREQ.
           MOVE SP-ADDON-DISFRMLA TO EPAY-DISFRMLA.
           IF SP-CAL-INTMETHOD-DIS(1)
              IF SP-INTMETHOD-EXACT
                 MOVE 365 TO EPAY-INTMETHOD.
           PERFORM EPAY-ACTUARIAL-CALC.


      ***************************************************************
       VALID-INS-TYPES-ELIG SECTION.
      ***************************************************************
           MOVE 0 TO IO-FG.
           IF PD-INSURED = "S" OR "J"
              IF PD-SSNO(2) = 0
                 MOVE 9 TO IO-FG
                 GO TO EXIT-VALID-INS-TYPES-ELIG.

      * READ CURRENT STATE PARAMETER RECORD INORDER
      * TO CHECK INSURANCE COMPANIES (SUBSCRIPTED) = 0.
           PERFORM GET-SP.

           MOVE PD-LOANDATE TO ITRM-TODAY.
           IF PD-INSTYPES-LL
              IF PD-PROCASH = 1
                 PERFORM LEVEL-INSURANCE
              ELSE
                 MOVE 1 TO ITRM-SUB
                 PERFORM INS-TERM-CALCULATION
                 IF ITRM-INSTERM = 0 OR
      * DONT DISPLAY MESSAGE IF INSURANCE IS STILL IN EFFECT PER < EXP DATE
      * DONT DISPLAY MESSAGE IF INSURANCE HAS BEEN REFUNDED
      * EXAMPLE:
      *   CL BOOKED WITH LOAN AND REFUNDED  LN-INSURANCES CODES = MCC
      *   TRYING TO DO AN "O1" ADDON, MUST ENTER "MII", BUT WAS GETTING
      *   INVALID TYPE BECAUSE CREDIT LIFE WAS REBATED
                    (ITRM-INEFFECT-FG = " " AND LN-CL-REBATE = " ")
                       PERFORM INVALID-TYPE
                       GO TO EXIT-VALID-INS-TYPES-ELIG.

           IF PD-INSTYPES-DL
              IF PD-PROCASH = 1
                 PERFORM DECREASING-INSURANCE
              ELSE
                 MOVE 1 TO ITRM-SUB
                 PERFORM INS-TERM-CALCULATION
                 IF ITRM-INSTERM = 0 OR
      * DONT DISPLAY MESSAGE IF INSURANCE IS STILL IN EFFECT PER < EXP DATE
      * DONT DISPLAY MESSAGE IF INSURANCE HAS BEEN REFUNDED
                    (ITRM-INEFFECT-FG = " " AND LN-CL-REBATE = " ")
                       PERFORM INVALID-TYPE
                       GO TO EXIT-VALID-INS-TYPES-ELIG.

           IF PD-INSTYPES-AH
              IF PD-PROCASH = 2
                 PERFORM ACCIDENT-INSURANCE
              ELSE
                 MOVE 2 TO ITRM-SUB
                 PERFORM INS-TERM-CALCULATION
                 IF ITRM-INSTERM = 0 OR
      * DONT DISPLAY MESSAGE IF INSURANCE IS STILL IN EFFECT PER < EXP DATE
      * DONT DISPLAY MESSAGE IF INSURANCE HAS BEEN REFUNDED
                    (ITRM-INEFFECT-FG = " " AND LN-AH-REBATE = " ")
                       PERFORM INVALID-TYPE
                       GO TO EXIT-VALID-INS-TYPES-ELIG.

           IF PD-INSTYPES-PP OR PD-INSTYPES-PP2
              IF PD-PROCASH = 3
                 PERFORM PROPERTY-INSURANCE
              ELSE
                 MOVE 3 TO ITRM-SUB
                 PERFORM INS-TERM-CALCULATION
                 IF ITRM-INSTERM = 0 OR
      * DONT DISPLAY MESSAGE IF INSURANCE IS STILL IN EFFECT PER < EXP DATE
      * DONT DISPLAY MESSAGE IF INSURANCE HAS BEEN REFUNDED
                    (ITRM-INEFFECT-FG = " " AND LN-AH-REBATE = " ")
                       PERFORM INVALID-TYPE
                       GO TO EXIT-VALID-INS-TYPES-ELIG.

           IF PD-INSTYPES-O1
              IF PD-PROCASH = 4
                 PERFORM OTHER-INSURANCE
              ELSE
                 MOVE 4 TO ITRM-SUB
                 PERFORM INS-TERM-CALCULATION
                 IF ITRM-INSTERM = 0 OR
      * DONT DISPLAY MESSAGE IF INSURANCE IS STILL IN EFFECT PER < EXP DATE
      * DONT DISPLAY MESSAGE IF INSURANCE HAS BEEN REFUNDED
                    (ITRM-INEFFECT-FG = " " AND LN-O1-REBATE = " ")
                    PERFORM INVALID-TYPE
                    GO TO EXIT-VALID-INS-TYPES-ELIG.

           IF PD-PROCASH = 1
              IF NOT ( PD-INSTYPES-DL OR PD-INSTYPES-LL )
                 PERFORM INVALID-TYPE
                 GO TO EXIT-VALID-INS-TYPES-ELIG
              END-IF
              IF ( LN-INSTYPES-AH AND NOT PD-INSTYPES-AH )
                     OR ( LN-INSTYPES-PP AND (NOT PD-INSTYPES-PP) )
                      OR ( LN-INSTYPES-PP2 AND (NOT PD-INSTYPES-PP2) )
                       OR ( LN-INSTYPES-O1 AND (NOT PD-INSTYPES-O1) )
                 PERFORM INVALID-TYPE
                 GO TO EXIT-VALID-INS-TYPES-ELIG.
           IF PD-PROCASH = 2
              IF NOT PD-INSTYPES-AH
                 PERFORM INVALID-TYPE
                 GO TO EXIT-VALID-INS-TYPES-ELIG
              END-IF
              IF ( LN-INSTYPES-DL AND (NOT PD-INSTYPES-DL) )
                     OR ( LN-INSTYPES-LL AND (NOT PD-INSTYPES-LL) )
                      OR ( LN-INSTYPES-PP AND (NOT PD-INSTYPES-PP) )
                       OR ( LN-INSTYPES-PP2 AND (NOT PD-INSTYPES-PP2) )
                        OR ( LN-INSTYPES-O1 AND (NOT PD-INSTYPES-O1) )
                 PERFORM INVALID-TYPE
                 GO TO EXIT-VALID-INS-TYPES-ELIG.
           IF PD-PROCASH = 3
              IF NOT (PD-INSTYPES-PP OR PD-INSTYPES-PP2)
                 PERFORM INVALID-TYPE
                 GO TO EXIT-VALID-INS-TYPES-ELIG
              END-IF
              IF ( LN-INSTYPES-DL AND (NOT PD-INSTYPES-DL) )
                     OR ( LN-INSTYPES-LL AND (NOT PD-INSTYPES-LL) )
                      OR ( LN-INSTYPES-AH AND (NOT PD-INSTYPES-AH) )
                       OR ( LN-INSTYPES-O1 AND (NOT PD-INSTYPES-O1) )
                 PERFORM INVALID-TYPE
                 GO TO EXIT-VALID-INS-TYPES-ELIG.
           IF PD-PROCASH = 4
              IF NOT PD-INSTYPES-O1
                 PERFORM INVALID-TYPE
                 GO TO EXIT-VALID-INS-TYPES-ELIG
              END-IF
              IF ( LN-INSTYPES-DL AND (NOT PD-INSTYPES-DL) )
                     OR ( LN-INSTYPES-LL AND (NOT PD-INSTYPES-LL) )
                      OR ( LN-INSTYPES-AH AND (NOT PD-INSTYPES-AH) )
                       OR ( LN-INSTYPES-PP AND (NOT PD-INSTYPES-PP) )
                        OR ( LN-INSTYPES-PP2 AND (NOT PD-INSTYPES-PP2) )
                 PERFORM INVALID-TYPE
                 GO TO EXIT-VALID-INS-TYPES-ELIG.

      * IF LL OR DL
           IF PD-INSTYPES = "B" OR "C"
              IF NOT ( PD-INSPREM(2) = 0
                          AND PD-INSPREM(3) = 0
                           AND PD-INSPREM(4) = 0
                            AND LN-INSPREM(2) = 0
                             AND LN-INSPREM(3) = 0
                              AND LN-INSPREM(4) = 0
                     )
                 PERFORM INVALID-TYPE
                 GO TO EXIT-VALID-INS-TYPES-ELIG.

      * IF AH W/ NO CL
           IF PD-INSTYPES = "D"
              IF NOT ( PD-INSPREM(1) = 0
                          AND PD-INSPREM(3) = 0
                           AND PD-INSPREM(4) = 0
                            AND LN-INSPREM(1) = 0
                             AND LN-INSPREM(3) = 0
                              AND LN-INSPREM(4) = 0
                     )
                 PERFORM INVALID-TYPE
                 GO TO EXIT-VALID-INS-TYPES-ELIG.

      * IF PP W/ NO CL OR PP2 W/ NO CL
           IF PD-INSTYPES = "E" OR "R"
              IF NOT ( PD-INSPREM(1) = 0
                          AND PD-INSPREM(2) = 0
                           AND PD-INSPREM(4) = 0
                            AND LN-INSPREM(1) = 0
                             AND LN-INSPREM(2) = 0
                              AND LN-INSPREM(4) = 0
                     )
                 PERFORM INVALID-TYPE
                 GO TO EXIT-VALID-INS-TYPES-ELIG.

      * IF O1
           IF PD-INSTYPES = "1"
              IF NOT ( PD-INSPREM(1) = 0
                          AND PD-INSPREM(2) = 0
                           AND PD-INSPREM(3) = 0
                            AND LN-INSPREM(1) = 0
                             AND LN-INSPREM(2) = 0
                              AND LN-INSPREM(3) = 0
                     )
                 PERFORM INVALID-TYPE
                 GO TO EXIT-VALID-INS-TYPES-ELIG.

       EXIT-VALID-INS-TYPES-ELIG.
           EXIT.

      ******************************************
       LEVEL-INSURANCE SECTION.
      ******************************************
           IF PD-INSURED = "J"
              IF SP-INSCOM(4) = 0
                 MOVE "N" TO CHFG(8)
                 MOVE 9 TO IO-FG
                 MOVE "JOINT LEVEL LIFE NOT ALLOWED" TO MESS
                 PERFORM SEND-MESS
              ELSE
                  NEXT SENTENCE
           ELSE
              IF SP-INSCOM(3) = 0
                 MOVE "N" TO CHFG(8)
                 MOVE 9 TO IO-FG
                 MOVE "LEVEL LIFE NOT ALLOWED" TO MESS
                 PERFORM SEND-MESS.
           IF BR-AH-AND-CL = "Y"
              IF PD-INSTYPES-AH
                 IF NOT(PD-INSTYPES-DL OR PD-INSTYPES-LL)
                    MOVE "N" TO CHFG(10) CHFG(11)
                    MOVE 9 TO IO-FG
                    MOVE "MUST HAVE LIFE AND A&H" TO MESS
                    PERFORM SEND-MESS.

      ******************************************
       DECREASING-INSURANCE SECTION.
      ******************************************
           IF PD-INSURED = "J"
              IF SP-INSCOM(2) = 0
                 MOVE "N" TO CHFG(8)
                 MOVE 9 TO IO-FG
                 MOVE "JOINT DECLINING LIFE NOT ALLOWED" TO MESS
                 PERFORM SEND-MESS
              ELSE
                  NEXT SENTENCE
           ELSE
              IF SP-INSCOM(1) = 0
                 MOVE "N" TO CHFG(8)
                 MOVE 9 TO IO-FG
                 MOVE "DECLINING LIFE NOT ALLOWED" TO MESS
                 PERFORM SEND-MESS.
           IF BR-AH-AND-CL = "Y"
              IF PD-INSTYPES-AH
                 IF NOT(PD-INSTYPES-DL OR PD-INSTYPES-LL)
                    MOVE "N" TO CHFG(10) CHFG(11)
                    MOVE 9 TO IO-FG
                    MOVE "MUST HAVE LIFE AND A&H" TO MESS
                    PERFORM SEND-MESS.

      ******************************************
       ACCIDENT-INSURANCE SECTION.
      ******************************************
           IF SP-INSCOM(5) = 0
              MOVE "N" TO CHFG(9)
              MOVE 9 TO IO-FG
              MOVE "A & H NOT ALLOWED" TO MESS
              PERFORM SEND-MESS.
           IF BR-AH-AND-CL = "Y"
              IF PD-INSTYPES-AH
                 IF NOT(PD-INSTYPES-DL OR PD-INSTYPES-LL)
                    MOVE "N" TO CHFG(10) CHFG(11)
                    MOVE 9 TO IO-FG
                    MOVE "MUST HAVE LIFE AND A&H" TO MESS
                    PERFORM SEND-MESS.

      ******************************************
       PROPERTY-INSURANCE SECTION.
      ******************************************
           IF PD-INSTYPES-PP
              IF SP-INSCOM(6) = 0
                 MOVE "N" TO CHFG(10)
                 MOVE 9 TO IO-FG
                 MOVE "PROPERTY NOT ALLOWED" TO MESS
                 PERFORM SEND-MESS.
           IF PD-INSTYPES-PP2
              IF SP-INSCOM(7) = 0
                 MOVE "N" TO CHFG(10)
                 MOVE 9 TO IO-FG
                 MOVE "PROPERTY NOT ALLOWED" TO MESS
                 PERFORM SEND-MESS.

      ******************************************
       OTHER-INSURANCE SECTION.
      ******************************************
           IF PD-INSTYPES-O1
              IF SP-INSCOM(8) = 0
                 MOVE "N" TO CHFG(11)
                 MOVE 9 TO IO-FG
                 MOVE "OTHER 1 NOT ALLOWED" TO MESS
                 PERFORM SEND-MESS.

      ******************************************
       VERIFY-REQD-FLDS SECTION.
      ******************************************
           IF EPAY-PVALUE = 0
              MOVE 9 TO IO-FG
              GO TO EXIT-VERIFY-REQD-FIELDS.
           IF PD-PURCD = " "
              MOVE 9 TO IO-FG
              MOVE "PURPOSE CODE REQUIRED BEFORE UPDATE" TO MESS
              PERFORM SEND-MESS.
           IF PD-COLLCD = " "
              MOVE 9 TO IO-FG
              MOVE "COLLATERAL CODE REQUIRED BEFORE UPDATE" TO MESS
              PERFORM SEND-MESS.

           IF PD-SRCD = "ET"
              IF PD-ORGTERM = 1
                 MOVE 9 TO IO-FG
                 MOVE "'ET' NOT ALLOWED WITH TERM OF 1" TO MESS
                 PERFORM SEND-MESS.

      * VALIDATE THAT LOANDATE(EFFECTIVE) IS NOT BEFORE ORIGINAL LOANDATE
      
           IF PD-LOANDATE < LN-LOANDATE
              MOVE "INS. EFFECTIVE DATE PRIOR TO ORIGINAL LOAN DATE"
                TO MESS
              PERFORM SEND-MESS
              MOVE 9 TO IO-FG.

           IF (PD-PROCASH = 1 AND PD-INSPREM(1) = 0) OR
               (PD-PROCASH = 2 AND PD-INSPREM(2) = 0) OR
                (PD-PROCASH = 3 AND PD-INSPREM(3) = 0) OR
                 (PD-PROCASH = 4 AND PD-INSPREM(4) = 0) OR
                  (PD-PROCASH = 5 AND PD-INSPREM(5) = 0) OR
                   (PD-PROCASH = 6 AND PD-INSPREM(6) = 0)
              MOVE 9 TO IO-FG
              MOVE "INSURANCE DOES NOT MATCH TYPE" TO MESS
              PERFORM SEND-MESS.

      * IF WE ARE JUST ADDING A FEE.....DMV, EXIT THIS VALIDATION ROUTINE

           IF PD-PROCASH NOT = 5 AND NOT = 6
              GO TO EXIT-VERIFY-REQD-FIELDS.

      * VALIDATE GL FEE CODES ARE IN BRANCH RECORD
      
      * OTHER 3
           IF PD-PROCASH = 6
              IF PD-FEECODE(1) NOT = " "
                 IF PD-FEECODE(1) NOT = BR-ADDON-FEECD-O3(1) AND
                    PD-FEECODE(1) NOT = BR-ADDON-FEECD-O3(2)
                       MOVE "INVALID FEE CODE" TO MESS
                       PERFORM SEND-MESS
                       MOVE 9 TO IO-FG.
           IF PD-PROCASH = 6
            IF BR-FORCE-VALID-CITY-COUNTY NOT = "Y"
              IF PD-FEECODE(2) NOT = " "
                 IF PD-FEECODE(2) NOT = BR-ADDON-FEECD-O3(1) AND
                    PD-FEECODE(2) NOT = BR-ADDON-FEECD-O3(2)
                       MOVE "INVALID FEE CODE" TO MESS
                       PERFORM SEND-MESS
                       MOVE 9 TO IO-FG.

           IF PD-PROCASH = 5
              IF PD-FEECODE(1) NOT = " "
                 IF PD-FEECODE(1) NOT = BR-ADDON-FEECD-O2(1) AND
                     PD-FEECODE(1) NOT = BR-ADDON-FEECD-O2(2)
                       MOVE "INVALID FEE CODE" TO MESS
                       PERFORM SEND-MESS
                       MOVE 9 TO IO-FG.
           IF PD-PROCASH = 5
            IF BR-FORCE-VALID-CITY-COUNTY NOT = "Y"
              IF PD-FEECODE(2) NOT = " "
                 IF PD-FEECODE(2) NOT = BR-ADDON-FEECD-O2(1) AND
                     PD-FEECODE(2) NOT = BR-ADDON-FEECD-O2(2)
                       MOVE "INVALID FEE CODE" TO MESS
                       PERFORM SEND-MESS
                       MOVE 9 TO IO-FG.

       EXIT-VERIFY-REQD-FIELDS.

      *******************************************
       SET-SPR-SUB SECTION.
      *******************************************
           IF I-LEVEL = 1
              IF LN-INSURED = "J"
                 MOVE 2 TO ILEV
              ELSE
                 MOVE 1 TO ILEV
              END-IF
              IF LN-INSTYPES-LL
                 ADD 2 TO ILEV
              END-IF
           ELSE
      * A&H
              IF I-LEVEL = 2
                 MOVE 5 TO ILEV
              ELSE
      * PROPERTY
                 IF I-LEVEL = 3
                    IF LN-INSTYPES-PP
                       MOVE 6 TO ILEV
                    ELSE
                       MOVE 7 TO ILEV
                    END-IF
                 ELSE
      * OTHER 1
                    IF I-LEVEL = 4
                       MOVE 8 TO ILEV
                    ELSE
      * OTHER 2
                       IF I-LEVEL = 5
                          MOVE 9 TO ILEV
                       ELSE
      * OTHER 3
                          MOVE 10 TO ILEV.


      *******************************************
       TEST-FOR-GEORGIA-SCHG SECTION.
      *******************************************
           IF (NOT (SP-SCFRMLA = 12 OR 13 OR 54))
              GO TO TEST-FOR-GEORGIA-SCHG-EXIT.

           MOVE " " TO SCHG-UPDATE-FG.
           MOVE 0 TO SCHG-NEW-ACCTNO.
           MOVE PD-SSNO(1) TO SCHG-LN-SSNO(1).
           MOVE PD-SSNO(2) TO SCHG-LN-SSNO(2).
           MOVE PD-SSNO(3) TO SCHG-LN-SSNO(3).
           MOVE PD-SSNO(4) TO SCHG-LN-SSNO(4).
           MOVE TRANS-DATE TO SCHG-TRANS-DATE.
           IF PD-SRCD = "PB" OR "RN" OR "SC"
                           OR "RB" OR "RO"
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
           MOVE SCHG-RESTRICT-ADJ TO PD-RESTRICT-ADJ.
           MOVE SCHG-DUAL-LOAN-ADJ TO PD-DUAL-LOAN-ADJ.
           MOVE SCHG-DUAL-LIAB TO SPDUAL-LIAB.

       TEST-FOR-GEORGIA-SCHG-EXIT.
           EXIT.

      ******************************************
       CALL-LOAN-CALCULATIONS SECTION.
      ******************************************
           MOVE "CALCULATING - PLEASE WAIT" TO LEGEND.
           PERFORM SEND-LEGEND.

           PERFORM CALCULATE-1STPYDATE-TERM.

           MOVE PD-1STPYDATE TO PD-ORIG-1STPYDATE DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO D-1STPYDATE.

           MOVE PD-ORGTERM TO D-ORGTERM.
           MOVE "<--" TO D-ARROW1
           MOVE 1STPY-BUF TO WR-BUF.
           MOVE 1STPY-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

           MOVE PD-OVRDRATE TO DF2-OVRDRATE.
           MOVE PD-OVRDRATE-FG TO DF2-OVRDRATE-FG.
           MOVE PD-SMPRATE TO DF2-SMPRATE.

           IF PD-SRCD = "ET"
              IF PD-ORGTERM = 1
                 MOVE PD-REGPYAMT TO TOTAL-REPAYABLE
              ELSE
                 COMPUTE TOTAL-REPAYABLE =
                       PD-REGPYAMT - AMTDUE
                             + PD-REGPYAMT * (PD-ORGTERM - 2)
                               + PD-LASTPYAMT.

           IF PD-SRCD NOT = "ET"
              IF PD-ORGTERM = 1
                 MOVE PD-REGPYAMT TO TOTAL-REPAYABLE
              ELSE
                 COMPUTE TOTAL-REPAYABLE =
                    PD-REGPYAMT * (PD-ORGTERM - 1) + PD-LASTPYAMT.

           COMPUTE PD-FINAMT =
                 PD-INSPREM(1) + PD-INSPREM(2) + PD-INSPREM(3)
               + PD-INSPREM(4) + PD-INSPREM(5) + PD-INSPREM(6)
               + PD-FEEAMT(1)  + PD-FEEAMT(2)  + PD-FEEAMT(3)
               + PD-FEEAMT(4)  + PD-FEEAMT(5)  + PD-FEEAMT(6)
               + PD-FEEAMT(7)  + PD-FEEAMT(8)  + PD-FEEAMT(9)
               + PD-FEEAMT(10).
           COMPUTE PD-INTCHG = TOTAL-REPAYABLE - PD-FINAMT.
           IF PD-LOANTYPE = "P"
              MOVE TOTAL-REPAYABLE TO PD-LNAMT
           ELSE
              MOVE PD-FINAMT TO PD-LNAMT.

           IF PD-LOANTYPE = "I"
              MOVE 0 TO DF2-ANTICIPATED
           ELSE
              MOVE PD-INTCHG TO DF2-ANTICIPATED.

           MOVE PD-PROCEEDS TO DF2-PROCEEDS.
           MOVE PD-PROCEEDS-FG TO DF2-PROCEEDS-FG.
           MOVE 0 TO PD-POINTS.
           MOVE "I" TO PD-POINTS-FG.
           MOVE TOTAL-REPAYABLE TO DF2-TREPAY.


           MOVE 0 TO PD-SERCHG
                     PD-STATE-WORKER
                     PD-MAINTFEE
                     PD-EXTCHG.
           MOVE "C" TO PD-SERCHG-FG
                       PD-MAINTFEE-FG
                       PD-EXTCHG-FG.
           MOVE PD-INTCHG TO DF2-INTCHG.
           MOVE PD-FINAMT TO DF2-FINAMT.
           MOVE PD-LNAMT TO DF2-LNAMT
                            PD-PURBAL
                            PD-SUM-LNAMT
                            PD-ORIGPURAMT.
           IF PD-LOANTYPE = "I"
              MOVE 0 TO PD-ANTICERN(1)
           ELSE
              MOVE PD-INTCHG TO PD-ANTICERN(1).
           MOVE 0 TO PD-ANTICERN(2) PD-ANTICERN(3)
                     PD-ANTICERN(4) PD-ANTICERN(5)
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

           PERFORM CALC-NO-OF-PAYMENTS.
           MOVE PD-INTCHG TO PD-FINCHG.
           MOVE PD-1STPYAMT TO DF2-1STPYAMT.
           MOVE PD-REGPYAMT TO DF2-REGPYAMT.
           MOVE PD-LASTPYAMT TO DF2-LASTPYAMT.

           MOVE "A" TO DF2-RETURN-CODE.
           MOVE 0 TO DF2-LOOP-COUNT.

           MOVE " " TO PD-ONE-MTH-NO-DAY-EXT-SW.

           PERFORM RESET-LEGEND.

           MOVE DF2-BUF TO WR-BUF.
           MOVE DF2-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

           MOVE 0 TO IO-FG.
           PERFORM DO-VERIFY-TERMCEIL.

      **************************************************
       GET-1ST-PAYMNT-INDEX SECTION.
      **************************************************
           MOVE LN-1STPYDATE TO NUM-DATE.
           MOVE PD-1STPYDATE TO SYS-DATE.
           PERFORM TIM360.

           COMPUTE SCHD-SUB = ELAPSED-MONTHS + 1
                         - LN-TOTNODEF - LN-TOTNOALDEL.

      *********************************************************
       ASCHD-AMOUNT-DUE SECTION.
      *********************************************************
           MOVE 0 TO ASCHD-AMTDUE.
           IF ASCHD-SUB NOT < 1
              PERFORM UNTIL SCHD-SUB > 360
                               OR SCHD-AMT(SCHD-SUB) = 0
                 ADD SCHD-AMT(SCHD-SUB) TO ASCHD-AMTDUE
                 ADD 1 TO SCHD-SUB
              END-PERFORM.

       ASCHD-AMOUNT-DUE-EXIT.
           EXIT.

      ************************************************
       SEND-PROMPT SECTION.
      ************************************************
           IF ELE = 1
              MOVE "ENTER ADDON DATE" TO LEGEND.
           IF ELE = 2
              STRING "'RT', 'ET', 'NP',"
                     " 'LP' OR 'ST' FOR ADD-ON"
                     "             "
                     DELIMITED BY SIZE INTO LEGEND
              INSPECT LEGEND REPLACING ALL "'" BY WK-QUOTE.
           IF ELE = 3
              STRING  "0=FEE, "
                      "1=CL, "
                      "2=AH, "
                      "3=PP, "
                      "4=O1, "
                      "5=O2, "
                      "6=O3"
                       DELIMITED BY SIZE INTO LEGEND.
           IF ELE = 4
              MOVE "ENTER ADJUSTMENT " TO LEGEND.
           IF ELE = 5
              MOVE "ENTER INSURANCE CODE" TO LEGEND.
           PERFORM SEND-LEGEND.


      ***********************************************************
      *            PAYMENT AMOUNT SETUP FOR A60
      *  EXAMPLE:
      *                       TERM = 1     TERM = 2     TERM = 6
      *                     -----------  -----------  -----------
      *   LN-1STPYAMT    :  $100.00      $100.00      $100.00
      *   LN-REGPYAMT    :  $100.00 X 0  $120.00 X 0  $120.00 X 4
      *   LN-LASTPYAMT   :  $  0.00      $120.00      $130.00
      *                      ------       ------       ------
      *     TOTAL REPAY  :  $100.00      $220.00      $710.00
      *
      *  NOTE:
      *       THE RESULTS OF ADDON CALCULATIONS ARE ONLY
      *       COMPUTING REG.PAY & LAST.PAY
      ***********************************************************
       CALC-NO-OF-PAYMENTS SECTION.
           MOVE PD-REGPYAMT TO PD-1STPYAMT.
           IF PD-ORGTERM = 1
              MOVE 0 TO PD-NOREGPY
                        PD-LASTPYAMT
           ELSE
           IF PD-ORGTERM = 2
              MOVE 0 TO PD-NOREGPY
           ELSE
              SUBTRACT 2 FROM PD-ORGTERM GIVING PD-NOREGPY.

      ***************************************************************
      *    DETERMINE INSURANCE SALES TAX
      *
      *    ROUTINE ASSUMES 'ILEV' AND 'COPIX' HAVE BEEN SET
      *
      *    NOTE:
      *          CD-CCY-TYPE = " " -> INDICATES NO CITY SALES TAX
      *            CD-CCT-TYPE  = " " -> INDICATES NO COUNTY SALES TAX
      ***************************************************************
       COW-INS-TAX SECTION.
           IF CD-CCY-TYPE = " " AND CD-CCT-TYPE  = " "
              MOVE 0 TO WK-INS-TAX
              GO TO COW-INS-TAX-EXIT.
   
      * DETERMINE AMOUNT OF COUNTY TAX:
           IF CD-CCT-TYPE NOT = " "
              IF CD-CCT-INSTAXFRMLA(ILEV) = " "
                 COMPUTE WK-CT-TAX ROUNDED =
                     WK-INS-PREMIUM
                         * CD-CCT-INSTAXAMT(ILEV) * 0.01
              ELSE
                 MOVE CD-CCT-INSTAXAMT(ILEV) TO WK-CT-TAX
              END-IF
           ELSE
              MOVE 0 TO WK-CT-TAX.
   
      * DETERMINE AMOUNT OF CITY TAX:
           IF CD-CCY-TYPE NOT = " "
              IF CD-CCY-INSTAXFRMLA(ILEV) = " "
                 COMPUTE WK-CY-TAX ROUNDED =
                     WK-INS-PREMIUM
                         * CD-CCY-INSTAXAMT(ILEV) * 0.01
              ELSE
                 MOVE CD-CCY-INSTAXAMT(ILEV) TO WK-CY-TAX
              END-IF
           ELSE
              MOVE 0 TO WK-CY-TAX.
   
      * DETERMINE IF THE COUNTY TAX AND CITY TAX SHOULD BE COMBINED:
           IF CD-CCY-TYPE NOT = " "
              IF CD-CCY-BOTH-TAX-FG = "Y"
                 COMPUTE WK-INS-TAX =  WK-CT-TAX + WK-CY-TAX
              ELSE
                 IF WK-CT-TAX > WK-CY-TAX
                    MOVE WK-CT-TAX TO WK-INS-TAX
                 ELSE
                    MOVE WK-CY-TAX TO WK-INS-TAX
                 END-IF
              END-IF
           ELSE
              MOVE WK-CT-TAX TO WK-INS-TAX.
   
       COW-INS-TAX-EXIT.
           EXIT.


       COPY "LIBGB/GPENV.CPY".
       COPY "LIBGB/DATER.CPY".
       COPY "LIBLP/LPUPER.CPY".
       COPY "LIBLP/LPANTI.CPY".
       COPY "LIBLP/LPMENU.CPY".
       COPY "LIBLP/LPMDTE.CPY".
       COPY "LIBLP/LPSPEDA.CPY".
       COPY "LIBLP/LPCEPP.CPY".
       COPY "LIBLP/LPEPAY.CPY".
       COPY "LIBLP/LPCART.CPY".
       COPY "LIBLP/LPITRM.CPY".
       COPY "LIBLP/LPSCHD.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBLP/GETSCH.CPY".
       COPY "LIBLP/LPPDCL.CPY".


      ****************************************************************
       SETUP-LINK-INFO SECTION.
      ****************************************************************
           IF VDU-FORM-NAME = "LPSMNU"
              MOVE LPSMNU-HELP--FILE TO HELP-LINK-FILE
              MOVE LPSMNU-HELP--DIR  TO HELP-LINK-DIR
           ELSE
              MOVE LONPG-HELP--FILE TO HELP-LINK-FILE
              MOVE LONPG-HELP--DIR TO HELP-LINK-DIR.

      ****************************************************************
       VDU-CONVERT-FIELD SECTION.
      ****************************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBLP/LONPG_EVA.CPY".
              COPY "LIBLP/LPSMNU_EVA.CPY".
              COPY "LIBLP/LPLNMU_EVA.CPY".
              COPY "LIBLP/LPCAMU_EVA.CPY".
              COPY "LIBLP/LPVDMU_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      *********************************
       MISC-PARAGRAPHS SECTION.
      *********************************
       COPY "LIBGB/SYSTEM.CPY".
       SET-EXTERNALS.
           MOVE LN-OWNBR      TO EXT-WI-CL-OWNBR.
           MOVE LN-ACCTNO     TO EXT-WI-CL-ACCTNO.
      *    MOVE LN-APP-NUMBER TO EXT-WI-CL-APP-NUMBER.
           MOVE 1             TO EXT-WI-CL-SSNO-NO.
           MOVE LN-SSNO(1)    TO EXT-WI-CL-SSNO(1).
           MOVE TRANS-DATE    TO EXT-WI-CL-TRANS-DATE.
       FORM-RESET.
       CALL-MEMO.
           MOVE LN-OWNBR      TO EXT-WI-CL-OWNBR.
           MOVE LN-ACCTNO     TO EXT-WI-CL-ACCTNO.
      *    MOVE LN-APP-NUMBER TO EXT-WI-CL-APP-NUMBER.
           MOVE 1             TO EXT-WI-CL-SSNO-NO.
           MOVE LN-SSNO(1)    TO EXT-WI-CL-SSNO(1).
           MOVE TRANS-DATE    TO EXT-WI-CL-TRANS-DATE.
           MOVE "T"           TO MEMOSC-ONLY.
           MOVE "WI/MEMOSC" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH MEMOSC-BUF EXIT-PATHNAME.
           CANCEL FORM-PROGX.
       DISPLAY-ALL-LOANS.
           MOVE "Y"                               TO BWLNS2-SCAN-ONLY.
           MOVE EXT-WI-CL-SSNO(EXT-WI-CL-SSNO-NO) TO BWLNS2-KEY.
           MOVE TRANS-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-ALPDATE.
           MOVE ALP-DATE                          TO BWLNS2-SYSDATE.
           MOVE "WI/BWLNS2" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH BWLNS2-BUF EXIT-PATHNAME.
           CANCEL FORM-PROGX.
       SEND-PAYMENT-LINES.
           PERFORM SET-EXTERNALS.
           MOVE "WI/LPLINE" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH EXIT-PATHNAME.
           CANCEL FORM-PROGX.
       SEND-PAYOFF.
           PERFORM SET-EXTERNALS.
           MOVE "WI/WIPOFF" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH EXIT-PATHNAME.
           CANCEL FORM-PROGX.
       DO-VERIFY-TERMCEIL.
           MOVE " " TO VERIFY-REJECT-TERM-FG.
           PERFORM VERIFY-TERMCEIL.
           IF VALID-LOAN = "N"
              MOVE "INSURANCE PREMIUM ERROR" TO MESS
              PERFORM SEND-MESS.
       INVALID-TYPE.
           MOVE 9 TO IO-FG.
           MOVE "INVALID TYPE FOR SELECTED ADDON" TO MESS.
           PERFORM SEND-MESS.
       GET-CODE.
           MOVE "GC"     TO CDB-TYPE.
           MOVE PD-OWNBR TO CDB-BRANCH.
           PERFORM READ-CDB1-FILE.
           IF IO-FG = 9
              MOVE " * NOT ON FILE * " TO CDB-DESC.
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
       GET-BW1.
           PERFORM READ-BW1-FILE.
           IF IO-GOOD
              IF BW-SSNO = PD-SSNO(1)
                 MOVE BW-STATE       TO BW-1-STATE
                 MOVE BW-CITY-TAX-CD TO BW-1-CITY-TAX-CD
                 MOVE BW-COUNTY      TO BW-1-COUNTY
              END-IF
           ELSE
              MOVE " "               TO BW-FNAME
              MOVE "NOT FOUND"       TO BW-LNAME.
           PERFORM APPEND-BW-NAMES.

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

       GET-COLLATERAL.
           MOVE LN-OWNBR  TO LTC-BRNO.
           MOVE LN-ACCTNO TO LTC-ACCTNO.
           MOVE 1         TO LTC-SEQNO.
           PERFORM OPEN-LTC1-FILE.
           PERFORM READ-LTC1-FILE.
           PERFORM CLOSE-LTC1-FILE.

       COMPUTE-GL-FEEAMT.
           MOVE 0 TO TOTAL-GL-HLDR
                     TOTAL-GL-HLDR-SALESTAX
                     TOTAL-GL-FINCHG-HLDR
                     TOTAL-GL-FINCHG-HLDR-SALESTAX.
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 10
              ADD PD-FEEAMT(SUB) TO TOTAL-GL-HLDR
              IF PD-FEEFINCHG(SUB) = "Y"
                 ADD PD-FEEAMT(SUB) TO TOTAL-GL-FINCHG-HLDR
              END-IF
      * ACCUMULATE INS. SALES TAX
              IF BR-FORCE-VALID-CITY-COUNTY = "Y"
                 IF PD-FEECODE(SUB) = "TC" OR "TA" OR "TP" OR "T1"
                                   OR "T2" OR "T3" OR "T4" OR "T5"
                    ADD PD-FEEAMT(SUB) TO TOTAL-GL-HLDR-SALESTAX
                 END-IF
              END-IF
           END-PERFORM.

       ICE-OR-A-MSG.
           IF ELE = 7
              MOVE "MUST BE 'C', 'E' OR 'A'" TO MESS
           ELSE
              MOVE "MUST BE 'I' OR 'C'" TO MESS.
           INSPECT MESS REPLACING ALL "'" BY WK-QUOTE.
           PERFORM SEND-MESS.

       ERROR-IN-CALCULATIONS-MSG.
           MOVE "ERROR IN CALCULATIONS - INACCURATE RESULTS" TO MESS.
           PERFORM SEND-MESS.

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
       NOT-FOUND.
           MOVE "RECORD NOT FOUND" TO MESS.
           PERFORM SEND-MESS.
       SP-INVALID.
           MOVE "ENTRY INCONSISTENT WITH STATE PARAMETERS" TO MESS.
           PERFORM SEND-MESS.
       MSG-LINE.
           PERFORM CALL-TMMSG.
       SEND-LEGEND.
           MOVE LEGEND TO WR-BUF.
           MOVE MESS-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.
       RESET-LEGEND.
           MOVE " " TO LEGEND.
           PERFORM SEND-LEGEND.

      *********************************
       IO-ROUTINE SECTION.
      *********************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBLP/LPSPGS_SQL.CPY".
       COPY "LIBLP/LPPDGS_SQL.CPY".
       COPY "LIBLP/LPLTPGS_SQL.CPY".
       COPY "LIBLP/LPLTCGS_SQL.CPY".
       COPY "LIBLP/LPLNGS_SQL.CPY".
       COPY "LIBLP/LPLCGS_SQL.CPY".
       COPY "LIBLP/LPICGS_SQL.CPY".
       COPY "LIBLP/LPDLGS_SQL.CPY".
       COPY "LIBLP/LPCDBGS_SQL.CPY".
       COPY "LIBLP/LPCDGS_SQL.CPY".
       COPY "LIBLP/LPBWGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBLP/LPDL1RN.CPY".
       COPY "LIBLP/LPLN1RN.CPY".
       COPY "LIBLP/LPLTC1RN.CPY".
       COPY "LIBLP/LPLTP1RN.CPY".
       COPY "LIBLP/LPLC1RN.CPY".
       COPY "LIBLP/LPSP1RN.CPY".
       COPY "LIBLP/LPBW1RN.CPY".
       COPY "LIBLP/LPCD1RN.CPY".
       COPY "LIBLP/LPCDB1RN.CPY".
       COPY "LIBLP/LPIC1RN.CPY".
       COPY "LIBLP/LPWKI.CPY".
       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO ERRCD.

      ******************************************
       END-ROUTINE SECTION.
      ******************************************
           PERFORM CLOSE-FILES.
       EXIT-PROG.
       STOP-RUN.
           EXIT PROGRAM.
