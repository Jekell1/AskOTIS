       IDENTIFICATION DIVISION.
       PROGRAM-ID.      LONPG7.
       DATE-WRITTEN.    121996.
      **************************************************************************
      *
      *   ADD-ON CANCEL ENTRY W/FRONT-END CALCULATIONS
      *   ADD-ON INSURANCE CANCEL ENTRY AND CALCULATIONS
      *   CALLED FROM LONPG0
      *   ...USED WITH LT-RECORD CREATION
      * REV:
      *  051895 SLC ADDED, IF REPODATE NOT = 0, DISALLOW THE ADDON
      *  071995 JTG REMOVED CLEAR OF BOGUS PD-PRLNAMT
      *  072996 MJD ADDED LIBLP/LPUPWK(W).
      *  KEC 111496 CHANGED LN-POCD FOR RESCISSION.
      *  MJD 011797 ADDED UNITPER
      *  MJD 043097 CHANGED TOTAL-REPAYABLE FROM S9(7) TO S9(7)V99.
      *  MJD 050797 ADDED LIBLP/SETCTBL(W) COPY MEMBERS
      *  MJD 051397 USE LN/PD-SMPRATE INSTEAD OF LN/PD-EFFRATE.
      *  MJD 051697 ADDED LPPOF2 LOGIC TO COMPUTE INTEREST REBATED
      *             ON ADDON.(RT TRANSCTIONS ONLY).
      *             DO NOT ALLOW CHANGES TO RATE.
      *  MJD 052097 ADDED LPPOF2 LOGIC TO COMPUTE INTEREST REBATED
      *             ON ADDON.(ALL TRANSCTIONS EXCEPT "ST").
      *  JTG 052097 ADDED 'LIBLP/ADDONSPR' TO GET CORRECT SPR RE: INS RB
      *  MJD 052797 CHANGED TO PERFORM CALL-LOAN-CALCULATIONS FROM ONE
      *             CENTRAL LOCATION. PASS LOAN NUMBER BACK TO LONPG0
      *             IF "F1" IS USED.
      *  MJD 060297 ALLOW ENTRY OF "ST" TRANSACTION
      *  MJD 060397 ALLOW CANCEL OF OTHER 3 INSURANCE.
      *  JTG 061897 ADDED LOGIC TO TEST FOR ONLY ADDON RECORDS THAT
      *             ARE PART OF THE CANCEL
      *  GLF 101797 ADDED MOVE LN-APPNO TO EXT-WI-CL-APPNO
      *  BAH 022598 ADDED BR-ADDON-FEECD-O3
      *  JTG 030598 CHANGED TO SET PAYMENT INCREMENT TO WHAT IT WAS
      *             WHEN ADDON INSURANCE WAS POSTED FOR 'NP' 'LP' AND 'RT'
      *             BUT ONLY IF FULL CANCEL AND CANCEL IS EFFECTIVE WHEN
      *             ADDON WAS MADE, MULLEN PL#168
      *  JTG 030698 CORRECTED DISPLAY WHEN USING FIXED ANNUAL RATES, CASTLE #660
      *  BAH 042998 REMOVED BR-ADDON-FEECD(1) AND (2) AND RENAMED (3) AND (4)
      *             TO BR-ADDON-FEECD-O2(1) AND (2)
      *   CS 042998 ADDED "RB" AND "RO" TO RENWEWAL TEST
      *  JTG 072898 ADDED POFF-O2DUE RE: RENEWAL O2 MONIES MERCURY #192
      *  KEC 090498 ADDED 'MOVE LC-LOANTYPE TO PD-LOANTYPE';
      *              FOR REGENCY REVOLVING.
      *  JTG 102698 #194 CASTLE, #204 SOUTHERN AUTO, EFFECTIVE DATE CANCEL
      *             FOR 'ST' NOT SETTING THE CORRECT 1ST PAY DATE IN SCHEDULE
      *             IF SEARCH.... MISSING FOR SET TERM LOGIC LIKE, RT,NP,LP,ET
      *  BAH 990524 ADDED AN IF FOR I-AM-REGACC WHEN SEARCHING FOR LP RECORDS
      *             THE CONVERSION CREATED LTS BUT NOT LPS.
      *  JTG 990617 CHANGED LOGIC TO COMPAIR ON ADDON-PREMIUM-NOFEES
      *             IN TEMPORAY LOGIC PUT IN 06/17/96
      *             OR (LN-INSPREM(INS-SUB) NOT = ADDON-PREMIUM-NOFEES)
      *  JTG 991109 ALLOWED ACCOUNTS TO HAVE ADDON'S WHEN IN REPO STATUS,
      *             CHANGED UPD OF DSFILE, TYFILE & LPEOD5TR, NEWPORT #244, A15
      *  MJD 000606 ADDED LOGIC TO STOP UPDATE IF CANCEL AMOUNT > LN-CURBAL
      *  JTG 000919 ADDED SP-ADDON-EXCEPT-FG REGACC #71225 PR#1297
      *  JTG 020307 ADDED "CT" COUNTY TAX CODE KENTUCKY AND PERCENTS
      *             ADDED "CY" CITY   TAX CODE KENTUCKY AND PERCENTS
      *             ADDED  FOR POFF-INS-TAX OCCURS 8                  WORLD #288
      *  JTG 020402 CHANGED POFF-INS-TAX TO POFF-INS-TAX-RB           WORLD #288
      *  JTG 020419 PUT A48 LOGIC BACK AS AN OPTION FOR COMPUTING THE INTEREST
      *             REFUNDS FOR ALL RE: SP-CANCEL-ADDON-FG            COSMO #355
      *  JTG 020520 ADDED "CT" COUNTY TAX CODE KENTUCKY AND PERCENTS
      *             ADDED "CY" CITY   TAX CODE KENTUCKY AND PERCENTS
      *             ADDED LOGIC FOR REFUND OF INS. SALES TAX          WORLD #288
      *  N O T E    N O T E
      *  JTG 020521 THE LOGIC FOR REFUND OF SALES TAX WAS NOT ADDED,
      *             BUT MUST BE.
      *  N O T E    N O T E
      *
      *  JTG 030305 ALLOW RBFRMLA '8' TO WORK FOR 'AH' & 'ALL COMPUTABLE INS'
      *                                                         PACESETTER #2012
      *  JTG 030404 FORCED CANCEL TO BE 'ST' WHEN FOUND-ST-ADDON-FG = 'Y'
      *                                                 SOUTHERN AUTOMOTIVE #401
      *  JTG 030929 MODIFIED SCFRMLA 13 TO ONLY TAKE THE DUAL LOAN
      *             ADJUSTMENT INTO ACCOUNT WHEN THE LOANS'S CLASSES
      *             ARE THE SAME                               LENDMARK#2149
      *  JTG 031001 CREATED 01  SCHG-WORKERS-2 FOR PASS FOR SCFRMLA 13
      *                                                       LENDMARK #2149
      *  BAH 040603 THE TEST TO NOT ALLOW BALANCE TO GO NEGATIVE WAS SETTING
      *             IO-FG INSTEAD OF ERR-FG, WAS ALLOWING ACCOUNT TO STILL
      *             GO NEGATIVE !!! REGACC PL# 443
      *  JTG 040713 CORRECTED NEW SP-RBFRMLA (0 C) SHORT RATE FORMULA
      *             LOGIC WAS NOT SETTING REB-LN-ORGTERM            MULLEN #2302
      *  JTG 040913 ADDED NEW FIELDS FOR SP-RBFRMLA (0 D)
      *             RULE 78THS USING DAYS IN TERM AND ELAPSED DAYS INSTEAD
      *             OF UNIT PERIODS IN TERM AND ELAPSE UNIT PERIODS REGACC #0074
      *   CS 060919 TODAYS TOTALS WERE NOT UPDATED ON A CANCEL OF A TRANSFERRED
      *             ACCOUNT. CHANGED THE MOVE OF LN-ORGBR TO PD-OWNBR, PD-ORGBR
      *             TO JUST PD-ORGBR AND ADDED MOVE OF LN-OWNBR TO PD-OWNBR
      *             WITHIN PROCESS-ORIGINAL-LOAN LIBERAL PR#552
      *   CS 110208 DO NOT ALLOW A SOURCE CODE OTHER THAN LAST PAY(LP) IF ADDON
      *             WAS LAST PAY.  HAD AN ADDON LP AND CANCELLED REMAIN TERM(RT)
      *             THE ADDON & PAY SCHEDULE LOOKED GOOD, BUT SUM-REGPYAMT WAS
      *             WRONG.  THE CANCEL SUBTRACTED THE LAST PAYMENT AMOUNT FROM
      *             THE SUM-REGPYAMT AND AN ADDON DONE AFTER CREATED A REDUCED
      *             PAYMENT THAT WAS INCORRECT, IN FACT IT WAS NEGATIVE
      *                                      CAROLINA FINANCE PR#723
      * MJD 130604 ADDED LOGIC FOR SP-SCFRMLA = 54.  FUNCTIONS THE SAME AS 12
      *            BUT INCLUDES AN EXTRA 4% ADDED IN AT THE END BASED
      *            ON TOTAL NOTE                                     WORLD #0874
      * MJD 140723 ADDED SP-ADDON-RATE-FG = "A"           MULLEN/KSTREET PR#4556
      *  BAH 140814 ADDED TEST FOR LPSMNU IN HELP
      *  CS 150428 COLONIAL TN DOES ADDONS FOR 'NP' AND CLAIMS BY LAW THEY CAN
      *            NOT CHARGE INTEREST, IN SET-NP-PAYMENT ADDED
      *            IF PD-SMPRATE = 0 GO TO SET-NP-PAYMENT-PC WHICH IS WHAT 
      *            "LP" & "RT DOES. PREV IT FELL THRU TO SET-NP-PAYMENT-IB WHICH      *            PERFORMS CALC-EPAY-CALC BUT GETS OUT BECAUSE THE RATE IS 0
      *            AND THEN TRIED TO CALCULATE A FC BY TAKING REGPAY WHICH IS 0
      *            AND SUBTRACTING THE PREMIUM LEADING TO A BAD FC CHARGE
      *  CS 150723 ADDED OPEN-WINDOW ENTER-CANCEL-REASON-WINDOW TO ENTER A 3
      *            CHARACTER UNVERIFIED CODE THAT WILL UPDATE THE LT-I REC AND
      *            ALSO THE TRAN RECORD (TRP) CANCEL O3 KSTREET PR#4770
      *  CS 150825 CHANGED GP-I-AM-KSTREET/MULL TO JUST MULL PR#4770 PER JUDY
      * BAH 160118 SPLIT OUT LTFILE, PD0003
      *
      * KEC 2016.0309 {PD#00003} PARADATA <A30> [JAY CISZEWSKI]
      *         CHANGED CD-GROUP, CD-SECURED & CD-TRW TO BE
      *         CD-CO-GROUP, CD-CO-SECURED & CD-CO-TRW.
      *
      * KEC 2016.0314 {PD#00003} PARADATA <A30> [JAY CISZEWSKI]
      *         CHANGED CD-FEE???? TO CD-GC-????.
      *  CS 170407 ACTIVATE 8 DIGIT DATES
      * BAH 170516 REMOVED LPSPEDW, NOT USED, FOUND DURING REGMGM PR#5291
      * BAH 170623 ACTIVATE 8 DIGIT DATES, PD0006
      * BAH 2018.07.02 ADDED ADDONSPRW AND SET ADDONSPR-SUB
      *                DONT SHARE REBATEW WORKERS ANYMORE!
      * KEC 2018-0824 LOAN APP MODULE REMOVAL FROM A30 [LARM30];
      *         LEAVING LN-APP-NUMBER & EXT-WI-CL-APP-NUMBER
      *         HERE IN CASE NEEDED IN THE FUTURE.
      * BAH 20181231 GLOBAL LPFILE
      *  CS 20190103 GLOBAL LTIFILEL2G
      * BAH 20190308 CHANGED LP-ADDON TO USE LPWSLP-LP-ADDON, MOVED LP-REFNO
      *              REDEFINITIONS TO LPWSLP COPY MEMBER
      * BAH 20190625 REMOVED LC-REVOLVING, PT#0153
      * BAH 20200625 ADDED OPEN-LTP1-FILE, REMOVED FROM GETSCH, #TST001
      * BAH 2022.0314 REMOVED EXT-SPANISH-FORMS AND ALL MX FORMS
      * BAH 20220803 HARD CODED START
      * BAH 2024.0311 REMOVED EXT-WI-CL-APP-NUMBER
      **************************************************************************
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
           "@(#)LP/LONPG7.CBL S35 02/20/24-15:29:50 >".
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
       COPY "LIBLP/LP01PD.CPY".
       COPY "LIBLP/LP01PD_SQL.CPY".
       COPY "LIBLP/LPWSPD.CPY".
       COPY "LIBLP/LP01LN.CPY".
       COPY "LIBLP/LP01LN_SQL.CPY".
       COPY "LIBLP/LPWSLN.CPY".
       COPY "LIBLP/LP01LP.CPY".
       COPY "LIBLP/LP01LP_SQL.CPY".
       COPY "LIBLP/LPWSLP.CPY".
       COPY "LIBLP/LP01LTC.CPY".
       COPY "LIBLP/LP01LTC_SQL.CPY".
       COPY "LIBLP/LPWSLTC.CPY".
       COPY "LIBLP/LP01LTI.CPY".
       COPY "LIBLP/LP01LTI_SQL.CPY".
       COPY "LIBLP/LPWSLTI.CPY".
       COPY "LIBLP/LP01LTP.CPY".
       COPY "LIBLP/LP01LTP_SQL.CPY".
       COPY "LIBLP/LPWSLTP.CPY".

       01  WK-KEY          PIC 9(2) COMP.

       01  WSEL-LIST       PIC X(5) VALUE "WSEL".

      * USED TO SET BEGINNING LP-SEQNO IN SEARCH-FOR-ADDON ROUTINE:
       01  BEG-LP-SEQNO    PIC 9(5).

       01  ONE             PIC S9   COMP.
       01  ONE-2           PIC S9   COMP.
       01  ELE             PIC 9(6) COMP.
       01  WK-QUOTE        PIC X    VALUE  X"22".
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

       01  ERR-FG             PIC 9 VALUE 0.
       01  WORK-AMT           PIC S9(7)V99.
       01  DISP-NUM2          PIC Z(6)9.99-.
       01  DISP-NUM2-ALP REDEFINES DISP-NUM2
                              PIC X(11).
       01  DISP-NUM0          PIC ZZ9.
       01  DISP-NUM0-ALP REDEFINES DISP-NUM0
                              PIC X(3).

       01  RTRM                    PIC 999       COMP.
       01  OTRM                    PIC 999       COMP.
       01  IX1                     PIC 99        COMP.
       01  IX2                     PIC 99        COMP.
       01  INS-SUB                 PIC 99        COMP  VALUE 0.
       01  SUB                     PIC 9(6)      COMP.
       01  SUB1                    PIC 9(6)      COMP.
       01  ET-SUB                  PIC 9(2)      COMP.
       01  WS-RATE                 PIC 999V9(15) COMP.
       01  WHOLE-DOLLARS           PIC S9(7)     COMP.
       01  TOTAL-REPAYABLE         PIC S9(7)V99  COMP.
       01  REAL-MONEY              PIC S9(7)V99  COMP.
       01  FINANCE-CHG             PIC S9(7)V99  COMP.
       01  COMPUTE-LOAN-FG         PIC X         VALUE "Y".

       01  ASCHD-SUB               PIC S999      COMP.
       01  ASCHD-AMTDUE            PIC S9(7)V99  COMP.

       01  SEARCH-FOR-ADDON-LP-FG  PIC X VALUE " ".
       01  GET-ADDON-FINCHG-MDTE-FLAG
                                   PIC X VALUE " ".
       01  FOUND-ADDON-FG          PIC X VALUE " ".
       01  FOUND-NP-ADDON-FG       PIC X VALUE " ".
       01  FOUND-LP-ADDON-FG       PIC X VALUE " ".
       01  FOUND-RT-ADDON-FG       PIC X VALUE " ".
       01  FOUND-ST-ADDON-FG       PIC X VALUE " ".
       01  FOUND-ET-ADDON-FG       PIC X VALUE " ".
       01  FOUND-ADDON-REC-FG      PIC X VALUE " ".
       01  FIND-LOAN-FEES-FG       PIC X VALUE " ".
       01  ZERO-ADDON-FINCHG-FOUND PIC X VALUE " ".
       01  ADDON-FINCHG            PIC S9(7)V99  COMP-3  VALUE 0.
       01  ADDON-FINCHG-WK         PIC S9(7)V99  COMP-3  VALUE 0.
       01  ADDON-PREMIUM           PIC S9(7)V99  COMP-3  VALUE 0.
       01  ADDON-PREMIUM-NOFEES    PIC S9(7)V99  COMP-3  VALUE 0.
       01  ADDON-ORIG-1STPYDATE    PIC 9(8)      COMP-3  VALUE 0.
       01  ADDON-INS-PAYMNT-INCR   PIC S9(7)V99  COMP-3  VALUE 0.
       01  ADDON-INS-NO-LT-REC     PIC S9(5)     COMP-3  VALUE 0.
       01  ADDON-INS-ANTICTERM     PIC S9(5)     COMP-3  VALUE 0.

       01  ADDON-ANTICTERM         PIC 9(6)      COMP-3  VALUE 0.
       01  HOLD-LN-TOTNODEF        PIC S9(3)     COMP-3  VALUE 0.
       01  HOLD-LN-TOTNOALDEL      PIC S9(3)     COMP-3  VALUE 0.
       01  LOAN-FEES               PIC S9(7)V99  COMP-3  VALUE 0.
       01  INSPECT-W               PIC X(86).
       01  ADD-ON                  PIC X(25).
       01  DEFAULT-APR-FG          PIC X.
       01  ONLY-1STPYDATE-FG       PIC X        VALUE " ".
       01  USE-EFFDATE-FG          PIC X        VALUE " ".

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

       01 SCHEDULED-TERM  PIC 9(4)   VALUE 0.

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

       01  TOTAL-GL-HLDR    PIC S9(5)V99 COMP.

       01 ERROREFF.
          03  FILLER            PIC X(36) VALUE
              "CAN'T EXCEED ORIGNAL LOANS RATE OF: ".
          03  ERROREFF-EFF      PIC ZZ9.999.

       01 ERRORSPR.
          03  FILLER            PIC X(32) VALUE
              "CAN'T EXCEED SPR FIXED RATE OF: ".
          03  ERRORSPR-SPR      PIC ZZ9.999.


       01  TERM-LOW             PIC 9(3)    COMP.
       01  TERM-HIGH            PIC 9(3)    COMP.
       01  PROCEEDS-LOW         PIC 9(6)V99 COMP.
       01  PROCEEDS-HIGH        PIC 9(6)V99 COMP.

       COPY "LIBWI/MEMOSCW.CPY".
       COPY "LIBCL/GASCHGW.CPY".
       COPY "LIBLP/LPWKPATH.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBWI/BWLNS2W.CPY".
       COPY "LIBLP/LPAPRSW.CPY".
       COPY "LIBLP/LPAPRZW.CPY".
       COPY "LIBLP/LPUPERW.CPY".
       COPY "LIBLP/LPANTIW.CPY".
       COPY "LIBGB/REBATEW.CPY".
       COPY "LIBLP/LPMDTEW.CPY".
       COPY "LIBLP/LPMENUW.CPY".
       COPY "LIBLP/LPAMTSW.CPY".
       COPY "LIBLP/ADDONSPRW.CPY".
       COPY "LIBLP/LPCEPPW.CPY".
       COPY "LIBLP/LPEPAYW.CPY".
       COPY "LIBLP/LPCARTW.CPY".
       COPY "LIBLP/LPITRMW.CPY".
       COPY "LIBLP/LPSCHDW.CPY".
       COPY "LIBGB/RBACTW.CPY".
       COPY "LIBLP/LPUPWKW.CPY".
       COPY "LIBLP/SETCTBLW.CPY".
       COPY "LIBLP/LPPOFFW.CPY".
       COPY "LIBCL/CLMENUW_EXT.CPY".
       COPY "LIBGB/GPENVW.CPY".

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
           PERFORM CLOSE-LTI1-FILE.
           PERFORM CLOSE-LTC1-FILE.
           PERFORM CLOSE-LP1-FILE.
           PERFORM CLOSE-LN1-FILE.
           PERFORM CLOSE-LC1-FILE.
           PERFORM CLOSE-DL1-FILE.
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

           PERFORM GET-GPENV.

      * MUST LEAVE SP1-FILE OPEN BECAUSE ADDONSPR READS IT
           PERFORM OPEN-SP1-FILE.
           PERFORM OPEN-LTP1-FILE.
           PERFORM OPEN-LTI1-FILE.
           PERFORM OPEN-CD1-FILE.

      * ALLOW F9-INTERRUPT
           MOVE "F" TO VDU-CLMENU-SELECTION.

      * ALLOW CTRL 'N' INTERRUPT:
           MOVE "WI/MAKERS" TO VDU-CTRL-N-PROG.

           PERFORM RESTORE-SCREEN.

       MAIN-MODULE.
      *    -------------------------------------------------------------
      *    CLEAR REQUIRED RE: (GA) SP-SCFRMLA = 12 OR 13 OR 54
      *    -------------------------------------------------------------

           IF FIRST-TIME = 0
              MOVE 1 TO FIRST-TIME
           ELSE
              MOVE " " TO MESS
              PERFORM MSG-LINE.

           MOVE 0   TO NEW-FLAG ELE.
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
      *      ENTRY-MODULE
      *      NEW-FLAG = 0 - MODIFY EXISTING RECORD
      *                 1 - INITIAL NEW RECORD ENTRY
      *                 2 - MODIFY NEW RECORD
      *************************************************
       ENTRY-MODULE SECTION.
      *************************************************
           MOVE "S  A000MSEL." TO VDU.
           MOVE " " TO VDUBUF VDU-ATTRIBUTES.
           PERFORM SCREENIO.
           MOVE " " TO IO-TYPE.
           MOVE 1 TO ONE.
           MOVE FIRST-ELE TO ELE.
           MOVE 0 TO FIRST-ELE.
      * ALLOW ENTRY OF CANCEL AMOUNT
           MOVE " " TO CHFG(4).
       CREATE-ROUTINE.
           IF NEW-FLAG NOT = 1
              GO TO MODIFY-ROUTINE.
           ADD ONE TO ELE.
           IF ELE < 1
              MOVE 1 TO ELE ONE.
           IF ELE NOT > MAX
              GO TO ENTER-ELE.

           MOVE 2 TO NEW-FLAG.

       MODIFY-ROUTINE.

      *    -------------------------------------------------------------
      *    ALLOW A FEW CHANGES TO OVRDRATE
      *    DO NOT ALLOW CHANGE TO OVERRIDE RATE. MJD 051597.
      *    -------------------------------------------------------------
      *    IF LN-LOANTYPE = "P"
      *       MOVE " " TO CHFG(7).

      *    -------------------------------------------------------------
      *    DON'T ALLOW CHANGE TO ADDON DATE, TYPE
      *    -------------------------------------------------------------
           MOVE "N" TO CHFG(1) CHFG(3).

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
           MOVE 0 TO ELE.

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
              IF ERR-FG = 9
                 GO TO MODIFY-ROUTINE
              END-IF
              PERFORM SAVE-SCREEN
              PERFORM UPDATE-CONTINUE
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
           IF NEW-FLAG = 1
              IF ELE = 4
                 PERFORM COMPUTE-INS-CANCEL
                 MOVE "SN N062040." TO VDU
                 MOVE REB-REBATE TO VDUNUM2
                 PERFORM SCREENIO
                 IF REB-REBATE = LN-INSPREM(INS-SUB)
                    PERFORM FIND-ALL-ADDON-FEES
                    PERFORM FIND-ALL-LOAN-FEES
                    ADD PD-FEEAMT(1) PD-FEEAMT(2) GIVING TOTAL-GL-HLDR
                    PERFORM DISPLAY-FEES.
           IF NEW-FLAG = 1
              IF ELE = 5 OR 6
                 GO TO CREATE-ROUTINE.
           IF NEW-FLAG NOT = 1
              IF ELE = 1 OR 5 OR 6
                 GO TO MODIFY-ROUTINE.

           IF ELE = 7
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

           PERFORM SEND-PROMPT.

           MOVE SPEC(ELE) TO VDU.
           MOVE ELE       TO VDUROW.
           MOVE 0         TO VDUCOL.
           MOVE "."       TO VDUDECIMAL.
           PERFORM SCREENIO.

      * ALLOW UP ARROW TO ADDON DATE:
           IF NEW-FLAG = 1
              MOVE " " TO CHFG(1).

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
      * DESTROY LONPG-SCREEN DOES NOT CLEAR THE SCREEN
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
      *    -------------------------------------------------------------
      *    LOAN DATE, INTEREST DATE, FILE DATE (ADDON DATE)
      *    -------------------------------------------------------------
           IF VDU-DATE-YYYYMMDD > TRANS-DATE
              PERFORM RESET-LEGEND
              MOVE "FUTURE DATING IS NOT ALLOWED!!" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.

           MOVE VDU-DATE-YYYYMMDD TO PD-LOANDATE.

           IF SP-ADDON-EXCEPT-FG = "T"
              MOVE TRANS-DATE  TO PD-INTDATE
           ELSE
              MOVE PD-LOANDATE TO PD-INTDATE.

           GO TO CREATE-ROUTINE.
       F-02.
           MOVE VDUBUF TO PD-SRCD CD-CODE.
           IF NOT (PD-SRCD = "RT" OR "NP" OR "LP" OR "ET" OR "ST")
              PERFORM RESET-LEGEND
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
           MOVE 1STPY-BUF TO WR-BUF.
           MOVE 1STPY-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

           IF NEW-FLAG NOT = 1
              PERFORM TEST-ADDON-TYPE
              IF ERR-FG = 9
                 GO TO ENTER-ELE.

      *    -------------------------------------------------------------
      *    TEST FOR LAST PAYMENT DATE > MATURITY DATE
      *    -------------------------------------------------------------
           IF SP-ADDON-ORG-MATURITY = "O"
              MOVE "Y" TO MDTE-ORGTERM-FG.
           PERFORM MATURITY-DATE-CALCULATION.
           IF TRANS-DATE NOT < MDTE-DATE
              PERFORM RESET-LEGEND
              MOVE "ADDONS NOT ALLOWED ON OR AFTER MATURITY " TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.

      *    -------------------------------------------------------------
      *    TEST FOR 1ST PAYMENT DATE > MATURITY DATE
      *    -------------------------------------------------------------
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
           MOVE MDTE-DATE    TO SYS-DATE.
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

      *    -------------------------------------------------------------
      *    TEST ENTERED TERM > REMAINING TERM
      *                      = REMAINING TERM
      *    -------------------------------------------------------------
           MOVE LN-UNITPER-CD TO DATER-UNITPER-CD.
           MOVE LN-UNITPER-FREQ TO DATER-UNITPER-FREQ.
           MOVE PD-1STPYDATE TO NDTE-DATE.
           SUBTRACT 1 FROM SCHEDULED-TERM GIVING NDTE-HOLD.
           PERFORM INCREMENT-UNITPER.

           IF NDTE-DATE > MDTE-DATE
              PERFORM RESET-LEGEND
              MOVE PD-1STPYDATE TO NUM-DATE
              MOVE MDTE-DATE    TO SYS-DATE
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
      *    -------------------------------------------------------------
      *    ENTER TYPE OF CANCEL
      *    -------------------------------------------------------------
           MOVE "N" TO CHFG(8)  CHFG(9)  CHFG(10)
                       CHFG(11) CHFG(12) CHFG(13).
           MOVE VDUNUM0 TO PD-PROCASH.
           IF PD-PROCASH < 1 OR > 6
              GO TO ENTER-ELE.

           MOVE PD-PROCASH  TO ITRM-SUB INS-SUB.
           MOVE PD-LOANDATE TO ITRM-TODAY.
           PERFORM INS-TERM-CALCULATION.
           IF ( LN-INSPREM(ITRM-SUB) = 0   ) OR
              ( ITRM-INEFFECT-FG NOT = "Y" )
              PERFORM RESET-LEGEND
              MOVE "INSURANCE IS NOT CURRENT" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.

      *    -------------------------------------------------------------
      *    GET TOTAL FIN CHG FOR ADDON RECORDS:
      *    -------------------------------------------------------------
           PERFORM FIND-ADDON-RECORDS.
           PERFORM TEST-ADDON-TYPE.
           IF ERR-FG = 9
              GO TO ENTER-ELE.


           MOVE " " TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE " " TO PD-FEECODE(1) PD-FEECODE(2).
           MOVE 0   TO PD-FEEAMT(1)  PD-FEEAMT(2).

           GO TO CREATE-ROUTINE.

       F-04.
      *    -------------------------------------------------------------
      *    PROCEEDS
      *    -------------------------------------------------------------
           MOVE VDUNUM2 TO PD-PROCEEDS.
           IF PD-PROCEEDS = 0
              GO TO ENTER-ELE.
           IF PD-PROCEEDS > LN-INSPREM(INS-SUB)
              PERFORM RESET-LEGEND
              MOVE "CAN NOT EXCEED PREMIUM" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
      *    -------------------------------------------------------------
      *    IF REB-REBATE = PD-PROCEEDS, THEN THE FEES WERE ALREADY
      *    FOUND AND DISPLAYED WHERE THE REBATE WAS CALC'D
      *    -------------------------------------------------------------
           IF PD-PROCEEDS = LN-INSPREM(INS-SUB)
              IF REB-REBATE NOT = PD-PROCEEDS
                 PERFORM FIND-ALL-ADDON-FEES
                 PERFORM FIND-ALL-LOAN-FEES
                 ADD PD-FEEAMT(1) PD-FEEAMT(2) GIVING TOTAL-GL-HLDR
                 PERFORM DISPLAY-FEES
              ELSE
                 MOVE 0 TO REB-REBATE
           ELSE
              MOVE 0 TO PD-FEEAMT(1) PD-FEEAMT(2) TOTAL-GL-HLDR
                        REB-REBATE
              MOVE " " TO PD-FEECODE(1) PD-FEECODE(2)
              PERFORM DISPLAY-FEES.
           GO TO CREATE-ROUTINE.
       F-05.
      *    -------------------------------------------------------------
      *    INSURED CODES
      *    -------------------------------------------------------------
           MOVE "N" TO CHFG(8)  CHFG(9)  CHFG(10)
                       CHFG(11) CHFG(12) CHFG(13).
           GO TO CREATE-ROUTINE.
       F-06.
           GO TO CREATE-ROUTINE.
       F-07.
           MOVE VDUNUM3 TO PD-OVRDRATE.
      * ALLOW ENTRY OF 0 IF ACCOUNT IS IB.
      *--->IF LN-LOANTYPE = "I"
      *       IF PD-OVRDRATE NOT = 0 AND NOT = LN-SMPRATE
      *          PERFORM RESET-LEGEND
      *          MOVE "MUST ENTER 0 OR RATE OF ORIGINAL LOAN" TO MESS
      *          PERFORM SEND-MESS
      *          MOVE "SNNN033070." TO VDU
      *          MOVE LN-SMPRATE TO VDUNUM3 PD-OVRDRATE
      *          PERFORM SCREENIO
      *--------->GO TO ENTER-ELE.
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
      * CL
           GO TO CREATE-ROUTINE.
       F-09.
      * AH
           GO TO CREATE-ROUTINE.
       F-10.
      * PP
           GO TO CREATE-ROUTINE.
       F-11.
      * O1
           GO TO CREATE-ROUTINE.
       F-12.
      * O2
           GO TO CREATE-ROUTINE.
       F-13.
      * O3
           GO TO CREATE-ROUTINE.
       END-ENTRY-MODULE.
           EXIT.


      ******************************************
       FIND-ALL-ADDON-FEES SECTION.
      ******************************************

      * IF OTHER INS IS CANCELLED WITH A FULL REBATE, WE MUST
      * ALSO REBATE ALL FEES

           PERFORM CLEAR-PD-FEES.

           MOVE LTI-PATH-OWNBR  TO LTI-BRNO
                                   QLTI1-WBEG-BRNO
                                   QLTI1-WEND-BRNO.
           MOVE LN-ACCTNO       TO LTI-ACCTNO
                                   QLTI1-WBEG-ACCTNO
                                   QLTI1-WEND-ACCTNO.
           MOVE 0               TO LTI-SEQNO
                                   QLTI1-WBEG-SEQNO.
           MOVE ALL "9"         TO QLTI1-WEND-SEQNO.

           PERFORM START-LTI1-FILE.
       FIND-NEXT-FEE.
           PERFORM READ-LTI1-FILE-NEXT.
           IF IO-FG NOT = 0 OR (LTI-BRNO NOT = LTI-PATH-OWNBR) OR
             (LTI-ACCTNO NOT = LN-ACCTNO)
              GO TO EXIT-FIND-ALL-ADDON-FEES.

      * 1 = CL, 2 = AH, 3 = PP, 4 = O1
           IF LTI-INS-TYPE NOT = INS-SUB
              GO TO FIND-NEXT-FEE.

           IF LTI-INSFEECD(1) = " " AND
              LTI-INSFEECD(2) = " "
              GO TO FIND-NEXT-FEE.

           IF LTI-INS-REBATE NOT = " "
              GO TO FIND-NEXT-FEE.

      * VALIDATE THAT INSURANCE RECORD IS NOT EXPIRED
           IF LTI-INSEXP-DATE NOT > PD-LOANDATE
              GO TO FIND-NEXT-FEE.

           MOVE 1 TO IX1.
           PERFORM DOES-FEE-MATCH.
           MOVE 2 TO IX1.
           PERFORM DOES-FEE-MATCH.

           GO TO FIND-NEXT-FEE.

       EXIT-FIND-ALL-ADDON-FEES.
           EXIT.

      ******************************************
       FIND-ALL-LOAN-FEES SECTION.
      ******************************************

      * IF INSURANCE BOOKED WITH LOAN !!
      * IF OTHER INS IS CANCELLED WITH A FULL REBATE, WE MUST
      * REBATE ALL FEES 

           MOVE 0 TO LOAN-FEES.
           IF LN-INSEFF-DATE(INS-SUB) NOT = LN-INTDATE
              GO TO EXIT-FIND-ALL-LOAN-FEES.

           MOVE 0 TO IX2.
       FIND-ALL-LOAN-FEES-NEXT.
           ADD 1 TO IX2.
           IF IX2 > 10
              GO TO EXIT-FIND-ALL-LOAN-FEES.

      * CANT DO FEES ON OTHER 1 ANYMORE...
      *    IF INS-SUB = 4
      *       IF LN-FEECODE(IX2) NOT = BR-ADDON-FEECD(1)
      *                       AND NOT = BR-ADDON-FEECD(2)
      *          GO TO FIND-ALL-LOAN-FEES-NEXT.

           IF INS-SUB = 5
              IF LN-FEECODE(IX2) NOT = BR-ADDON-FEECD-O2(1)
                              AND NOT = BR-ADDON-FEECD-O2(2)
                 GO TO FIND-ALL-LOAN-FEES-NEXT.
           IF INS-SUB = 6
              IF LN-FEECODE(IX2) NOT = BR-ADDON-FEECD-O3(1)
                              AND NOT = BR-ADDON-FEECD-O3(2)
                 GO TO FIND-ALL-LOAN-FEES-NEXT.

      *    IF INS-SUB > 3
           IF INS-SUB > 4
              IF FIND-LOAN-FEES-FG = " "
                 MOVE 1 TO IX1
                 MOVE LN-FEECODE(IX2) TO LTI-INSFEECD(IX1)
                 MOVE LN-FEEAMT(IX2) TO LTI-INSFEE(IX1)
                 PERFORM DOES-FEE-MATCH
              ELSE
                 ADD LN-FEEAMT(IX2) TO LOAN-FEES.

           GO TO FIND-ALL-LOAN-FEES-NEXT.

       EXIT-FIND-ALL-LOAN-FEES.
           MOVE " " TO FIND-LOAN-FEES-FG.

      ******************************************
       FIND-ADDON-RECORDS SECTION.
      ******************************************
           MOVE " " TO FOUND-ADDON-FG
                       FOUND-NP-ADDON-FG
                       FOUND-LP-ADDON-FG
                       FOUND-RT-ADDON-FG
                       FOUND-ET-ADDON-FG
                       FOUND-ST-ADDON-FG
                       FOUND-ADDON-REC-FG
                       ZERO-ADDON-FINCHG-FOUND.
           MOVE 0   TO ADDON-FINCHG
                       ADDON-PREMIUM
                       ADDON-PRIOR-NODEF
                       ADDON-PREMIUM-NOFEES
                       ADDON-ORIG-1STPYDATE
                       ADDON-ANTICTERM
                       ADDON-INS-PAYMNT-INCR
                       ADDON-INS-NO-LT-REC
                       ADDON-INS-ANTICTERM.

           MOVE LTI-PATH-OWNBR  TO LTI-BRNO
                                   QLTI1-WBEG-BRNO
                                   QLTI1-WEND-BRNO.
           MOVE LN-ACCTNO       TO LTI-ACCTNO
                                   QLTI1-WBEG-ACCTNO
                                   QLTI1-WEND-ACCTNO.
           MOVE 0               TO LTI-SEQNO
                                   QLTI1-WBEG-SEQNO.
           MOVE ALL "9"         TO QLTI1-WEND-SEQNO.

           PERFORM START-LTI1-FILE.
       FIND-ADDON-RECORDS-NEXT.
           PERFORM READ-LTI1-FILE-NEXT.
           IF ( IO-FG NOT = 0             ) OR
              (LTI-BRNO NOT = LTI-PATH-OWNBR) OR
              ( LTI-ACCTNO NOT = LN-ACCTNO )
              GO TO FIND-ADDON-RECORDS-TEST-ORIG.

      * 1 = CL, 2 = AH, 3 = PP, 4 = O1
           IF ( LTI-INS-TYPE NOT = INS-SUB   ) OR
              ( LTI-INT-REBATE NOT = " " ) OR
              ( LTI-INS-REBATE NOT = " " )
              GO TO FIND-ADDON-RECORDS-NEXT.

           IF ( LN-INSEFF-DATE(INS-SUB) NOT = LTI-INSEFF-DATE ) OR
              ( LN-INSEXP-DATE(INS-SUB) NOT = LTI-INSEXP-DATE )
              GO TO FIND-ADDON-RECORDS-NEXT.

           MOVE "Y" TO FOUND-ADDON-FG.

           IF LTI-INSPREM < 0
              MOVE -1 TO ONE-2
           ELSE
              MOVE 1  TO ONE-2.

      * TEST FOR CONVERTED ADDON RECORDS:
           IF ( LTI-ANTICERN(3) = 0     ) AND
              ( LTI-INS-TOT-ADDON = 0   ) AND
              ( LTI-INS-PAYMNT-INCR = 0 )
              MOVE "Y"               TO ZERO-ADDON-FINCHG-FOUND
              PERFORM GET-ADDON-FINCHG
              ADD ADDON-FINCHG-WK    TO ADDON-FINCHG
              MOVE "Y"               TO FOUND-ADDON-REC-FG
              MOVE LTI-PRIOR-NODEF    TO ADDON-PRIOR-NODEF
           ELSE
              ADD LTI-ANTICERN(3)     TO ADDON-FINCHG
              ADD 1                   TO ADDON-INS-NO-LT-REC
              ADD LTI-INS-PAYMNT-INCR TO ADDON-INS-PAYMNT-INCR
              ADD LTI-ANTICTERM       TO ADDON-INS-ANTICTERM
              IF FOUND-ADDON-REC-FG = " "
                 MOVE "Y" TO FOUND-ADDON-REC-FG
                 MOVE 2 TO BEG-LP-SEQNO
                 PERFORM SEARCH-FOR-ADDON-LP
                 IF ( SEARCH-FOR-ADDON-LP-FG = "Y" ) OR
                    ( LN-CONVERCD NOT = "X"        )
                    MOVE LTI-ORIG-1STPYDATE TO ADDON-ORIG-1STPYDATE
                    MOVE LTI-ANTICTERM   TO ADDON-ANTICTERM
                    MOVE LTI-PRIOR-NODEF TO ADDON-PRIOR-NODEF.

           COMPUTE ADDON-PREMIUM = ADDON-PREMIUM
               + LTI-INSPREM + (LTI-INSFEE(1) * ONE-2)
                             + (LTI-INSFEE(2) * ONE-2).

           ADD LTI-INSPREM TO ADDON-PREMIUM-NOFEES.

           IF LTI-TRCD = "NP"
              MOVE "Y" TO FOUND-NP-ADDON-FG
           ELSE
           IF LTI-TRCD = "LP"
              MOVE "Y" TO FOUND-LP-ADDON-FG
           ELSE
           IF LTI-TRCD = "RT"
              MOVE "Y" TO FOUND-RT-ADDON-FG
           ELSE
           IF LTI-TRCD = "ET"
              MOVE "Y" TO FOUND-ET-ADDON-FG
           ELSE
           IF LTI-TRCD = "ST"
              MOVE "Y" TO FOUND-ST-ADDON-FG.

           GO TO FIND-ADDON-RECORDS-NEXT.

       FIND-ADDON-RECORDS-TEST-ORIG.
           IF ADDON-PREMIUM-NOFEES = LN-INSPREM(INS-SUB)
              GO TO EXIT-FIND-ADDON-RECORDS.

           IF INS-SUB > 3
              MOVE "N" TO FIND-LOAN-FEES-FG
              PERFORM FIND-ALL-LOAN-FEES
           ELSE
              MOVE 0 TO LOAN-FEES.
           MOVE 1 TO ONE-2.
           MOVE LN-INSEFF-DATE(INS-SUB) TO LTI-INSEFF-DATE.
           COMPUTE LTI-INSPREM =
               LN-INSPREM(INS-SUB) - ADDON-PREMIUM-NOFEES.
           MOVE LOAN-FEES TO LTI-INSFEE(1).
           MOVE 0 TO LTI-INSFEE(2).
           MOVE "Y" TO GET-ADDON-FINCHG-MDTE-FLAG.
           PERFORM GET-ADDON-FINCHG.
           ADD ADDON-FINCHG-WK TO ADDON-FINCHG.
           COMPUTE ADDON-PREMIUM =
               ADDON-PREMIUM + LN-INSPREM(INS-SUB)
                       - ADDON-PREMIUM-NOFEES + LOAN-FEES.

       EXIT-FIND-ADDON-RECORDS.
           EXIT.

      *****************************************************
      *    SEARCH FOR MATCHING LP REC FOR THE LT REC
      *
      *  SEARCH-FOR-ADDON-LP-FG:
      *      " "  -  NO LP-RECORD FOUND FOR LT-REC
      *      "Y"  -  LP-RECORD FOUND & WAS ADDON
      *      "N"  -  LP-RECORD FOUND & WAS CANCEL OR ADJ
      *****************************************************
       SEARCH-FOR-ADDON-LP SECTION.
           MOVE " " TO SEARCH-FOR-ADDON-LP-FG.

           PERFORM OPEN-LP1-FILE.

      * NOTE:
      *  SET OF BEG-LP-SEQNO MUST BE DONE BEFORE THIS ROUTINE
      *  IS PERFORMED:

           MOVE LP-PATH-OWNBR TO LP-BRNO
                                 QLP1-WBEG-BRNO
                                 QLP1-WEND-BRNO.
           MOVE LN-ACCTNO     TO LP-ACCTNO
                                 QLP1-WBEG-ACCTNO
                                 QLP1-WEND-ACCTNO.
           MOVE BEG-LP-SEQNO  TO LP-SEQNO
                                 QLP1-WBEG-SEQNO.
           MOVE ALL "9"       TO QLP1-WEND-SEQNO.

           PERFORM START-LP1-FILE.

       SEARCH-FOR-ADDON-LP-NEXT.
           PERFORM READ-LP1-FILE-NEXT.
           IF ( IO-FG NOT = 0             ) OR
              ( LP-BRNO NOT = LP-PATH-OWNBR) OR
              ( LP-ACCTNO NOT = LN-ACCTNO )
              GO TO SEARCH-FOR-ADDON-LP-EXIT.

           IF LP-TRCD = "RV" OR LP-REV = "Y"
              GO TO SEARCH-FOR-ADDON-LP-NEXT.
           IF NOT (LP-TRCD = "RT" OR "LP" OR "NP" OR "ET" OR "ST")
              GO TO SEARCH-FOR-ADDON-LP-NEXT.
           IF LP-LT-SEQNO NOT = LTI-SEQNO
              GO TO SEARCH-FOR-ADDON-LP-NEXT.

           MOVE LP-REFNO TO LPWSLP-LP-REFNO.
           IF LPWSLP-LP-ADDON-TEXT = "ADD"
              MOVE "Y" TO SEARCH-FOR-ADDON-LP-FG
           ELSE
              MOVE "N" TO SEARCH-FOR-ADDON-LP-FG.

       SEARCH-FOR-ADDON-LP-EXIT.
           PERFORM CLOSE-LP1-FILE.

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

           MOVE BR-STATE   TO DEF-ORGST PD-ORGST PD-OWNST.
           MOVE BRANCH     TO DEF-ORGBR PD-ORGBR PD-OWNBR.
           MOVE 999.99     TO PD-SERVPER.
           MOVE 999.999    TO PD-ENDORATE PD-OVRDRATE.
           MOVE "E"        TO PD-OVRDRATE-FG.
           MOVE "C"        TO PD-INS-FG(1) PD-INS-FG(2) PD-INS-FG(3)
                              PD-SERCHG-FG PD-MAINTFEE-FG
                              PD-EXTCHG-FG PD-REGPYAMT-FG.
           MOVE 0 TO TOTAL-GL-HLDR TERM-LOW TERM-HIGH
                     PROCEEDS-LOW PROCEEDS-HIGH.

           MOVE DEF-BUF TO WR-BUF.
           MOVE DEF-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

           MOVE "** I N S U R A N C E   C A N C E L **" TO MESSAGE-BUF.
           MOVE MESSAGE-BUF TO WR-BUF.
           MOVE MESSAGE-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

           MOVE "Y" TO DEFAULT-APR-FG.
           MOVE 0 TO ELE.

      **********************************************
       COMPUTE-INS-CANCEL SECTION.
      **********************************************

      * CALCULATE PREMIUM TO CANCEL
           MOVE INS-SUB TO IX1 ITRM-SUB.

      * TEST FOR SINGLE OR JOINT LIFE:
           IF INS-SUB = 1
              IF LN-INSURED = "J"
                 MOVE 2 TO REB-SUB
              ELSE
                 MOVE 1 TO REB-SUB.

      * TEST FOR LEVEL LIFE:
           IF INS-SUB = 1
              IF LN-INSTYPES-LL
                 ADD 2 TO REB-SUB.

           IF INS-SUB = 2
              MOVE 5 TO REB-SUB
           ELSE
              IF INS-SUB = 3
                 MOVE 6 TO REB-SUB
              ELSE
                 IF INS-SUB = 4
                    MOVE 12 TO REB-SUB
                 ELSE
                    IF INS-SUB = 5
                       MOVE 13 TO REB-SUB
                    ELSE
                       IF INS-SUB = 6
                          MOVE 14 TO REB-SUB.

           IF LN-INSOURS(IX1) NOT = "M"
              MOVE PD-LOANDATE     TO REB-PAYDATE
                                      ITRM-TODAY
              PERFORM INS-TERM-CALCULATION
              MOVE ITRM-INSTERM    TO REB-ORGTERM
                                      REB-LN-ORGTERM
              MOVE LN-INSPREM(IX1) TO REB-TOTCHG
              MOVE LN-INSEFF-DATE(IX1)  TO REB-LN-LOANDATE
                                           REB-LN-PURDATE
                                           REB-LN-INTDATE
                                           REB-LN-INSEFF
                                           NDTE-DATE
              MOVE LN-INSEXP-DATE(IX1)  TO REB-LN-INSEXP

              MOVE 1 TO NDTE-HOLD
              MOVE LN-UNITPER-CD TO DATER-UNITPER-CD
              MOVE LN-UNITPER-FREQ TO DATER-UNITPER-FREQ
              PERFORM INCREMENT-UNITPER
              MOVE NDTE-DATE       TO REB-LN-ORIG-1STPYDATE
              MOVE 0 TO  REB-LN-TOTNODEF
              MOVE LN-INSCOVR(IX1) TO REB-LN-INSCOVR
              MOVE "N"             TO REB-LN-SALEFIN
              MOVE "Y"             TO REB-ADDON-FG
              MOVE IX1             TO REB-SUB2 ADDONSPR-SUB
              PERFORM FIND-ADDON-SPR
              PERFORM REBATE
              PERFORM FIND-ADDON-SPR-RESTORE
           ELSE
              MOVE 0 TO REB-REBATE.

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

           IF ( LN-POCD-NONLOAN            ) OR
              ( LN-CURBAL = 0              ) OR
              ( LN-JDDATE NOT = 0          ) OR
              ( LN-PLDATE NOT = 0          ) OR
              ( LN-ACCELDATE NOT = 0       ) OR
              ( LN-INT-REBATE = "Y" OR "R" )
              MOVE 8 TO IO-FG
              GO TO END-PROCESS-ORIGINAL-LOAN.

           MOVE LN-ACCTNO TO PD-ACCTNO.

      *    -------------------------------------------------------------
      *    STORE THE ORIGINAL LOAN SOURCE CODE IN POCD FOR THE UPDATE
      *    PROGRAM.
      *    -------------------------------------------------------------
           MOVE LN-SRCD TO PD-POCD.

           MOVE LN-CLASS TO LC-CODE.
           PERFORM OPEN-LC1-FILE.
           PERFORM READ-LC1-FILE.
           PERFORM CLOSE-LC1-FILE.
           IF IO-FG = 9
              MOVE "INVALID" TO LC-DESC.

           MOVE LC-CODE      TO PD-CLASS.
           MOVE LC-SMALLOAN  TO PD-SMALLOAN.
           MOVE LC-SALEFIN   TO PD-SALEFIN.
           MOVE LC-SERVACCT  TO PD-SERVACCT.
           MOVE LC-LOANTYPE  TO PD-LOANTYPE.
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

      *---------------- DISPLAY RATES -----------------------------
      * DISPLAY OF PD-SMPRATE FOR EFFECTIVE IS CORRECT:
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
      *------------------------------------------------------------

           MOVE PD-1STPYDATE TO PD-ORIG-1STPYDATE DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO D-1STPYDATE.
 
           MOVE PD-ORGTERM TO D-ORGTERM.

           MOVE LN-ORGST TO D-ORGST PD-ORGST PD-OWNST.
           MOVE LN-ORGBR TO D-ORGBR PD-ORGBR.
           MOVE LN-OWNBR TO PD-OWNBR.
           MOVE SP-SUBCLASS TO D-SUBCLASS.
           MOVE SP-DESC TO D-SUBDESC.
           MOVE LN-PURCD TO CD-CODE PD-PURCD.
           MOVE "PU"     TO CD-TYPE.
           PERFORM GET-CD.

           MOVE LN-COLLCD TO CD-CODE D-COLCD PD-COLLCD.
           MOVE "CO"      TO CD-TYPE.
           PERFORM GET-CD.
           MOVE CD-DESC TO D-COLDESC.

      *    -------------------------------------------------------------
      *    SETUP SO INSURANCE ENTRY WILL BE CORRECT
      *       MOVE "NNN" TO D-130 PD-INSCODES.
      *    -------------------------------------------------------------
           MOVE LN-INSCODES TO D-INSCODE PD-INSCODES.
      *--->MOVE "I" TO D-INSFG.
           MOVE "N" TO CHFG(8)  CHFG(9)  CHFG(10)
                       CHFG(11) CHFG(12) CHFG(13).

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
           MOVE LN-INSEXP-DATE(IX2) TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO D-2EXP(IX1).
           MOVE LN-INSCOVR(IX2) TO D-2COVR(IX1).
           MOVE LN-INSOURS(IX2) TO D-2OUR(IX1).
           IF IX2 < 6
              ADD 1 TO IX1 IX2
              GO TO DISP-NEXT-2IC.


      *    -------------------------------------------------------------
      *    MOVE REST OF CODES AND DATES NEEDED FROM ORIGINAL LOAN
      *    -------------------------------------------------------------
           MOVE LN-CONFIDENTCD  TO PD-CONFIDENTCD.
           MOVE LN-RECASTCD     TO PD-RECASTCD.
           MOVE LN-STATUSFG     TO PD-STATUSFG.
           MOVE LN-STATFLAGS    TO PD-STATFLAGS.

           MOVE LN-REDUDATE     TO PD-REDUDATE.
           MOVE LN-LEGALDATE    TO PD-LEGALDATE.
           MOVE LN-BNKRPTDATE   TO PD-BNKRPTDATE.
           MOVE LN-DEBTAIDATE   TO PD-DEBTAIDATE.
           MOVE LN-REPODATE     TO PD-REPODATE.
           MOVE LN-REPOCD       TO PD-REPOCD.

           MOVE LN-PURDATE      TO PD-PURDATE.
           MOVE LN-SUM-REGPYAMT TO PD-SUM-REGPYAMT.
           MOVE 0 TO IO-FG.

       END-PROCESS-ORIGINAL-LOAN.
           EXIT.

      *********************************************
       CALCULATE-1STPY-NEXT SECTION.
      *********************************************

      * CALCULATE THE NEXT PAYMENT DUE ON THE
      * ORIGINAL LOAN BASED ON THE REMAINING
      * TERM, AND SET PD-1STPYDATE WITH DATE

      * SET NEW 1STPYDATE = TODAY:
           MOVE " " TO USE-EFFDATE-FG.
           MOVE TRANS-DATE TO NUM-DATE PD-1STPYDATE.
           IF PD-LOANDATE NOT = TRANS-DATE
              IF INS-SUB NOT = 0
                 IF LN-INSEFF-DATE(INS-SUB) = PD-LOANDATE
                    IF LN-INSPREM(INS-SUB) = PD-PROCEEDS
                       MOVE PD-LOANDATE TO NUM-DATE PD-1STPYDATE
                       MOVE "Y" TO USE-EFFDATE-FG.

      * IF THE ORIGINAL FIRST PAY DAY IS PRIOR TO TODAY'S DAY,
      * BUMP TO NEXT MONTH
           IF LN-1STPYDA >= NUM-DA
              MOVE LN-1STPYDA TO PD-1STPYDA
           ELSE
              MOVE LN-1STPYDA TO NUM-DA
              MOVE NUM-DATE TO NDTE-DATE-WORK
              MOVE 1 TO NDTE-HOLD
              MOVE LN-UNITPER-CD TO DATER-UNITPER-CD
              MOVE LN-UNITPER-FREQ TO DATER-UNITPER-FREQ
              PERFORM INCREMENT-UNITPER
              MOVE NDTE-DATE TO PD-1STPYDATE.

      * IF ADDON 1STPYDATE IS LESS THAN ORIGINAL LOAN 1STPYDATE, USE
      *   ORIGINAL LOAN 1STPYDATE
           IF PD-1STPYDATE < LN-1STPYDATE
              MOVE LN-1STPYDATE TO PD-1STPYDATE.

           PERFORM TEST-1STPY-ENOUGH-TIME.

      *************************************************************
       TEST-1STPY-ENOUGH-TIME SECTION.
      *************************************************************

      * COMPARE THE NEW 1STPYDATE TO SP-1STPY-ADDON-DAYS TO SEE
      * IF THERE IS ENOUGH TIME TO NOTIFY CUSTOMER.
           IF USE-EFFDATE-FG = "Y"
              MOVE PD-LOANDATE        TO NUM-DATE
           ELSE
              MOVE TRANS-DATE         TO NUM-DATE.
           MOVE PD-1STPYDATE          TO SYS-DATE.
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

      * CALCULATE PD-1STPYDATE, PD-LASTPYAMT & PD-ORGTERM
      * BASED UPON THE SOURCE CODE
           MOVE 0 TO PD-LASTPYAMT.
           PERFORM GET-EPAY-PVALUE.

      * LAST PAYMENT:
           IF PD-SRCD NOT = "LP"
              GO TO CALCULATE-1STPYDATE-TERM-NP.

      * SET ORGTERM:
           MOVE 1 TO PD-ORGTERM.

           IF SEARCH-FOR-ADDON-LP-FG = "Y"
              IF EPAY-PVALUE = ADDON-PREMIUM
                 MOVE ADDON-ORIG-1STPYDATE TO PD-1STPYDATE
                 MOVE ADDON-ANTICTERM TO PD-ANTICTERM
                 PERFORM SET-LP-PAYMENT
                 GO TO CALCULATE-1STPYDATE-TERM-EXIT.

           PERFORM MATURITY-DATE-CALCULATION.
           MOVE MDTE-DATE TO PD-1STPYDATE.
           PERFORM TEST-1STPY-ENOUGH-TIME.

      * SET ANTICTERM:
           MOVE PD-LOANDATE TO NUM-DATE.
           MOVE PD-1STPYDATE TO SYS-DATE.
           MOVE LN-UNITPER-CD TO DATER-UNITPER-CD.
           MOVE LN-UNITPER-FREQ TO DATER-UNITPER-FREQ.
           PERFORM TIMUPER.
           MOVE ELAPSED-UNITPER TO PD-ANTICTERM.

           PERFORM SET-LP-PAYMENT.
           GO TO CALCULATE-1STPYDATE-TERM-EXIT.

      * NEXT PAYMENT:

       CALCULATE-1STPYDATE-TERM-NP.
           IF PD-SRCD NOT = "NP"
              GO TO CALCULATE-1STPYDATE-TERM-ST.

           MOVE 1 TO PD-ORGTERM PD-ANTICTERM.
           IF SEARCH-FOR-ADDON-LP-FG = "Y"
              IF EPAY-PVALUE = ADDON-PREMIUM
                 MOVE ADDON-ORIG-1STPYDATE TO PD-1STPYDATE
                 PERFORM SET-NP-PAYMENT
                 GO TO CALCULATE-1STPYDATE-TERM-EXIT.

           PERFORM CALCULATE-1STPY-NEXT.
           PERFORM SET-NP-PAYMENT.
           GO TO CALCULATE-1STPYDATE-TERM-EXIT.

      * SCHEDULED TERM (ST):

       CALCULATE-1STPYDATE-TERM-ST.
           IF PD-SRCD NOT = "ST"
              GO TO CALCULATE-1STPYDATE-TERM-RT.

           IF SEARCH-FOR-ADDON-LP-FG = "Y"
                      AND EPAY-PVALUE = ADDON-PREMIUM
              MOVE ADDON-ORIG-1STPYDATE TO PD-1STPYDATE
           ELSE
              PERFORM CALCULATE-1STPY-NEXT.


      * CALCULATE ORGTERM:
           MOVE LN-1STPYDATE TO CEPP1-DATE.
           MOVE PD-1STPYDATE TO CEPP2-DATE.
           PERFORM CONTRACTUAL-ELAPSED-PAYPER.
           MOVE SCHEDULED-TERM TO PD-ORGTERM.

      * CALCULATE ANTICTERM:
           MOVE SCHEDULED-TERM TO PD-ANTICTERM.
           MOVE PD-INTDATE   TO NUM-DATE.
           MOVE PD-1STPYDATE TO SYS-DATE.
           PERFORM TIM360.
           IF ELAPSED-MONTHS > 0
                  AND ELAPSED-REM > 0
                   AND PD-1STPYDATE NOT = LN-1STPYDATE
              ADD 1 TO PD-ANTICTERM.

           PERFORM SET-ST-PAYMENT.
           GO TO CALCULATE-1STPYDATE-TERM-EXIT.

      * REMAINING TERM:

       CALCULATE-1STPYDATE-TERM-RT.
           IF PD-SRCD NOT = "RT"
              GO TO CALCULATE-1STPYDATE-TERM-ET.

           IF SEARCH-FOR-ADDON-LP-FG = "Y"
                      AND EPAY-PVALUE = ADDON-PREMIUM
              MOVE ADDON-ORIG-1STPYDATE TO PD-1STPYDATE
           ELSE
              PERFORM CALCULATE-1STPY-NEXT.

      * CALCULATE ORGTERM:
           MOVE "Y"  TO CEPP-INCDEF-FG CEPP-INCALDEL-FG.
           MOVE LN-1STPYDATE TO CEPP1-DATE.
           MOVE PD-1STPYDATE TO CEPP2-DATE.
           PERFORM CONTRACTUAL-ELAPSED-PAYPER.
           COMPUTE PD-ORGTERM =
               LN-SUM-ORGTERM - CEPP-PERIODS + 1
                         + ADDON-PRIOR-NODEF.
           IF PD-ORGTERM > LN-SUM-ORGTERM
              MOVE LN-SUM-ORGTERM TO PD-ORGTERM.

      * CALCULATE ANTICTERM:
           IF SEARCH-FOR-ADDON-LP-FG = "Y"
                      AND EPAY-PVALUE = ADDON-PREMIUM
              MOVE ADDON-ANTICTERM TO PD-ANTICTERM
              PERFORM SET-RT-PAYMENT
              GO TO CALCULATE-1STPYDATE-TERM-EXIT.

           MOVE "Y"  TO CEPP-INCDEF-FG CEPP-INCALDEL-FG.
           MOVE LN-1STPYDATE TO CEPP1-DATE.
           MOVE PD-1STPYDATE TO CEPP2-DATE.
           PERFORM CONTRACTUAL-ELAPSED-PAYPER.
           IF SP-ADDON-ORG-MATURITY = "O"
              COMPUTE PD-ANTICTERM =
                  LN-ORGTERM - CEPP-PERIODS + 1
                                       + ADDON-PRIOR-NODEF
              IF PD-ANTICTERM > LN-ORGTERM
                 MOVE LN-ORGTERM TO PD-ANTICTERM
              ELSE
                 NEXT SENTENCE
           ELSE
              COMPUTE PD-ANTICTERM =
                  LN-SUM-ORGTERM - CEPP-PERIODS + 1
                                       + ADDON-PRIOR-NODEF
              IF PD-ANTICTERM > LN-SUM-ORGTERM
                 MOVE LN-SUM-ORGTERM TO PD-ANTICTERM.
           MOVE PD-INTDATE   TO NUM-DATE.
           MOVE PD-1STPYDATE TO SYS-DATE.
           PERFORM TIM360.
           IF ELAPSED-MONTHS > 0
                  AND ELAPSED-REM > 0
                   AND PD-1STPYDATE NOT = LN-1STPYDATE
              ADD 1 TO PD-ANTICTERM.

           PERFORM SET-RT-PAYMENT.
           GO TO CALCULATE-1STPYDATE-TERM-EXIT.

      * EXTEND EXISTING TERM:

       CALCULATE-1STPYDATE-TERM-ET.
           IF PD-SRCD NOT = "ET"
              GO TO CALCULATE-1STPYDATE-TERM-EXIT.

           IF ONLY-1STPYDATE-FG NOT = "Y"
                     AND SEARCH-FOR-ADDON-LP-FG = "Y"
                        AND EPAY-PVALUE = ADDON-PREMIUM
              MOVE ADDON-ORIG-1STPYDATE TO PD-1STPYDATE
              PERFORM SET-ET-TERM-AND-PAYMENT
              MOVE ADDON-ANTICTERM TO PD-ANTICTERM
              GO TO CALCULATE-1STPYDATE-TERM-EXIT
           ELSE
              MOVE "Y" TO MDTE-ORGTERM-FG
              PERFORM MATURITY-DATE-CALCULATION
              MOVE MDTE-DATE TO NDTE-DATE
              MOVE 1 TO NDTE-HOLD
              MOVE LN-UNITPER-CD TO DATER-UNITPER-CD
              MOVE LN-UNITPER-FREQ TO DATER-UNITPER-FREQ
              PERFORM INCREMENT-UNITPER
              MOVE NDTE-DATE TO PD-1STPYDATE
              PERFORM TEST-1STPY-ENOUGH-TIME.
           IF ONLY-1STPYDATE-FG = "Y"
              MOVE 0 TO PD-ORGTERM
              GO TO CALCULATE-1STPYDATE-TERM-EXIT.

           PERFORM SET-ET-TERM-AND-PAYMENT.
           IF SP-ADDON-ORG-MATURITY = "O"
              MOVE "Y" TO MDTE-ORGTERM-FG.
           PERFORM MATURITY-DATE-CALCULATION.
           MOVE PD-LOANDATE TO NUM-DATE.
           MOVE MDTE-DATE TO SYS-DATE.
           PERFORM TIM360.
           IF ELAPSED-MONTHS > 0
              MOVE ELAPSED-MONTHS TO PD-ANTICTERM
           ELSE
              MOVE 1 TO PD-ANTICTERM.
           IF SP-ADDON-ORG-MATURITY = "U"
              ADD PD-ORGTERM TO PD-ANTICTERM.

       CALCULATE-1STPYDATE-TERM-EXIT.
           MOVE " " TO ONLY-1STPYDATE-FG.

      *************************************************************
       SET-NP-PAYMENT SECTION.
      *************************************************************

      * COMPUTE SCHEDULE OF PAYMENTS FOR NP:

           IF PD-SMPRATE = 0
              MOVE 0 TO WS-RATE
              GO TO SET-NP-PAYMENT-PC
           ELSE
              IF PD-LOANTYPE = "P"
                 IF PD-OVRDRATE-FG = "F"
                    COMPUTE WS-RATE ROUNDED = PD-OVRDRATE / 1200
                    GO TO SET-NP-PAYMENT-PC.

           IF EPAY-PVALUE = ADDON-PREMIUM
              GO TO SET-NP-PAYMENT-PC.

      * TEST SP-CANCEL-ADDON-FG:
      *      WHEN = " " DEFAULT, DETERMINE INTEREST ADJUSTMENT FOR CANCEL
      *                 BY COMPUTING AN ADDON REFUND THE DAY OF
      *                 THE CANCEL
      *             "F" DETERMINE INTEREST ADJUSTMENT USING 'CART'
      *                 ROUTINE WHICH DETERMINES THE AMOUNT NEEDED
      *                 TO FINANCE THE ADJUSTMENT AMOUNT A48

           IF SP-CANCEL-ADDON-FG = "F"
              GO TO SET-NP-PAYMENT-IB-OR-ORIGLN.

      * ADDON WITH 0 INTEREST
      * WE DONT STORE THE RATE OF THE ADDON IF IT WAS OVERWRITTEN
      * SO TESTING FOR ADDON-FINCHG = 0  

           IF (LN-LOANTYPE = "I" AND ADDON-FINCHG = 0)
               COMPUTE PD-REGPYAMT = PD-PROCEEDS
               GO TO SET-NP-PAYMENT-EXIT
           ELSE
           IF ( LN-LOANTYPE = "I"        ) OR
              ( FOUND-ADDON-FG NOT = "Y" ) OR
              ( LN-INSPREM(INS-SUB) NOT = ADDON-PREMIUM-NOFEES)
              GO TO SET-NP-PAYMENT-IB-OR-ORIGLN.

      * COMPUTE INTEREST REBATE, REGPYAMT, AND LASTPYAMT USING
      *  LPPOF2 LOGIC.   (LIBGB/REBATE).

           PERFORM GET-INTEREST-REBATE-LPPOF2.

      * COMPUTE REGPYAMT AND LASTPYAMT AS FOLLOWS:
      *  REGPYAMT  ROUNDED  =   PROCEEDS + REBATE

           COMPUTE PD-REGPYAMT = PD-PROCEEDS +
                                 POFF-INT-REB-TBL(POFF-SUB).

           GO TO SET-NP-PAYMENT-EXIT.

      *
      * EXECUTE THIS IF ACCOUNT IS IB OR INSURANCE WAS BOOKED WITH ORIGINAL LOAN
      *
       SET-NP-PAYMENT-IB-OR-ORIGLN.
           MOVE PD-ORGTERM TO EPAY-ORGTERM.
           PERFORM CALC-EPAY-CALC.
           MOVE EPAY-PAYMNT TO PD-REGPYAMT.
           GO TO SET-NP-PAYMENT-EXIT.

       SET-NP-PAYMENT-PC.

      * THE FOLLOWING METHOD IS REQUESTED PER MULLEN:

           IF EPAY-PVALUE = ADDON-PREMIUM
              IF ZERO-ADDON-FINCHG-FOUND = "Y"
                 PERFORM ENTER-ADDON-FINCHG
                 MOVE ADDON-FINCHG TO REAL-MONEY
                 COMPUTE PD-REGPYAMT = EPAY-PVALUE + REAL-MONEY
              ELSE
                 MOVE ADDON-FINCHG TO REAL-MONEY
                 IF ADDON-INS-NO-LT-REC = 1
                    MOVE ADDON-INS-PAYMNT-INCR TO PD-REGPYAMT
                 ELSE
                    COMPUTE PD-REGPYAMT = EPAY-PVALUE + REAL-MONEY
                 END-IF
           ELSE
              COMPUTE REAL-MONEY ROUNDED =
                       ADDON-PREMIUM * WS-RATE * PD-ORGTERM
              COMPUTE PD-REGPYAMT = EPAY-PVALUE + REAL-MONEY.

       SET-NP-PAYMENT-EXIT.
           EXIT.

      *************************************************************
       SET-LP-PAYMENT SECTION.
      *************************************************************

      * COMPUTE SCHEDULE OF PAYMENTS FOR LP:

           IF PD-SMPRATE = 0
              MOVE 0 TO WS-RATE
              GO TO SET-LP-PAYMENT-PC
           ELSE
              IF PD-LOANTYPE = "P"
                 IF PD-OVRDRATE-FG = "F"
                    COMPUTE WS-RATE ROUNDED = PD-OVRDRATE / 1200
                    GO TO SET-LP-PAYMENT-PC.

           IF EPAY-PVALUE = ADDON-PREMIUM
              GO TO SET-LP-PAYMENT-PC.

      * TEST SP-CANCEL-ADDON-FG:
      *      WHEN = " " DEFAULT, DETERMINE INTEREST ADJUSTMENT FOR CANCEL
      *                 BY COMPUTING AN ADDON REFUND THE DAY OF
      *                 THE CANCEL
      *             "F" DETERMINE INTEREST ADJUSTMENT USING 'CART'
      *                 ROUTINE WHICH DETERMINES THE AMOUNT NEEDED
      *                 TO FINANCE THE ADJUSTMENT AMOUNT A48
      
           IF SP-CANCEL-ADDON-FG = "F"
              GO TO SET-LP-PAYMENT-IB-OR-ORIGLN.

           IF ( LN-LOANTYPE = "I"        ) OR
              ( FOUND-ADDON-FG NOT = "Y" ) OR
              ( LN-INSPREM(INS-SUB) NOT = ADDON-PREMIUM-NOFEES )
              GO TO SET-LP-PAYMENT-IB-OR-ORIGLN.

      * COMPUTE INTEREST REBATE, REGPYAMT, AND LASTPYAMT USING
      *  LPPOF2 LOGIC.   (LIBGB/REBATE).

           PERFORM GET-INTEREST-REBATE-LPPOF2.

      *-----------------------------------------------------------------
      * COMPUTE REGPYAMT AND LASTPYAMT AS FOLLOWS:
      *
      *                        PROCEEDS + REBATE
      *  REGPYAMT  ROUNDED  =  -----------------
      *                             ORGTERM
      *
      *  LASTPYAMT = (PROCEEDS + REBATE) - (ORGTERM * REGPYAMT) + REGPYAMT
      *
      *-----------------------------------------------------------------
           COMPUTE PD-REGPYAMT  =  PD-PROCEEDS +
                                   POFF-INT-REB-TBL(POFF-SUB).

           GO TO SET-LP-PAYMENT-EXIT.


      *
      * EXECUTE THIS IF ACCOUNT IS IB OR INSURANCE WAS BOOKED WITH ORIGINAL LOAN
      *

       SET-LP-PAYMENT-IB-OR-ORIGLN.
           MOVE PD-ORGTERM TO EPAY-ORGTERM.
           PERFORM CALC-EPAY-CALC.
           MOVE EPAY-PAYMNT TO PD-REGPYAMT.
           IF SP-ADDON-WHOLE-FG = "A"
              COMPUTE WHOLE-DOLLARS = EPAY-PAYMNT + 0.99
              MOVE WHOLE-DOLLARS TO PD-REGPYAMT.
           GO TO SET-LP-PAYMENT-EXIT.

       SET-LP-PAYMENT-PC.

      * TEST FOR FULL CANCEL:

           IF EPAY-PVALUE = ADDON-PREMIUM
              IF ZERO-ADDON-FINCHG-FOUND = "Y"
                 PERFORM ENTER-ADDON-FINCHG
                 MOVE ADDON-FINCHG TO REAL-MONEY
                 COMPUTE PD-REGPYAMT = EPAY-PVALUE + REAL-MONEY
              ELSE
                 MOVE ADDON-FINCHG TO REAL-MONEY
                 IF ADDON-INS-NO-LT-REC = 1
                    MOVE ADDON-INS-PAYMNT-INCR TO PD-REGPYAMT
                 ELSE
                    COMPUTE PD-REGPYAMT = EPAY-PVALUE + REAL-MONEY
                 END-IF
           ELSE
              PERFORM CALCULATE-FINCHG-REFUND
              COMPUTE PD-REGPYAMT = EPAY-PVALUE + REAL-MONEY.

       SET-LP-PAYMENT-EXIT.
           EXIT.

      *************************************************************
       SET-ST-PAYMENT SECTION.
      *************************************************************

      * COMPUTE SCHEDULE OF PAYMENTS FOR ST:

           MOVE 0 TO WHOLE-DOLLARS REAL-MONEY FINANCE-CHG.

           IF PD-SMPRATE = 0
              MOVE 0 TO WS-RATE FINANCE-CHG
              GO TO SET-ST-PAYMENT-CONT
           ELSE
              IF PD-LOANTYPE = "P"
                 IF PD-OVRDRATE-FG = "F"
      * MULLEN FIXED RATE
                    COMPUTE WS-RATE ROUNDED = PD-OVRDRATE / 1200
                    COMPUTE FINANCE-CHG = EPAY-PVALUE * WS-RATE
                                                 * PD-ANTICTERM
                    IF SP-ADDON-WHOLE-FG = "A"
                       ADD .5 FINANCE-CHG GIVING WHOLE-DOLLARS
                       MOVE WHOLE-DOLLARS TO FINANCE-CHG
                       GO TO SET-ST-PAYMENT-CONT
                    ELSE
                       GO TO SET-ST-PAYMENT-CONT.

           IF EPAY-PVALUE = ADDON-PREMIUM
              GO TO SET-ST-PAYMENT-CONT.

      * TEST SP-CANCEL-ADDON-FG:
      *      WHEN = " " DEFAULT, DETERMINE INTEREST ADJUSTMENT FOR CANCEL
      *                 BY COMPUTING AN ADDON REFUND THE DAY OF
      *                 THE CANCEL
      *             "F" DETERMINE INTEREST ADJUSTMENT USING 'CART'
      *                 ROUTINE WHICH DETERMINES THE AMOUNT NEEDED
      *                 TO FINANCE THE ADJUSTMENT AMOUNT A48
      
           IF SP-CANCEL-ADDON-FG = "F"
              GO TO SET-ST-PAYMENT-IB-OR-ORIGLN.

           IF ( LN-LOANTYPE = "I"        ) OR
              ( FOUND-ADDON-FG NOT = "Y" ) OR
              ( LN-INSPREM(INS-SUB) NOT = ADDON-PREMIUM-NOFEES )
              GO TO SET-ST-PAYMENT-IB-OR-ORIGLN.

      * COMPUTE INTEREST REBATE, REGPYAMT, AND LASTPYAMT USING
      *  LPPOF2 LOGIC.   (LIBGB/REBATE).

           PERFORM GET-INTEREST-REBATE-LPPOF2.

      *-----------------------------------------------------------------
      * COMPUTE REGPYAMT AND LASTPYAMT AS FOLLOWS:
      *
      *                        PROCEEDS + REBATE
      *  REGPYAMT  ROUNDED  =  -----------------
      *                             ORGTERM
      *
      *  LASTPYAMT = (PROCEEDS + REBATE) - (ORGTERM * REGPYAMT) + REGPYAMT
      *
      *-----------------------------------------------------------------
           COMPUTE PD-REGPYAMT ROUNDED =
               (PD-PROCEEDS + POFF-INT-REB-TBL(POFF-SUB)) /
                   PD-ORGTERM.
           COMPUTE PD-LASTPYAMT =
               (PD-PROCEEDS + POFF-INT-REB-TBL(POFF-SUB)) -
                (PD-ORGTERM * PD-REGPYAMT) + PD-REGPYAMT.

           GO TO SET-ST-PAYMENT-EXIT.


      *
      * EXECUTE THIS IF ACCOUNT IS IB OR INSURANCE WAS BOOKED WITH ORIGINAL LOAN
      *

       SET-ST-PAYMENT-IB-OR-ORIGLN.
           IF SP-CONSOLIDATE-RT-FG = "C"
              PERFORM GET-LT-SCHLD
              PERFORM GET-1ST-PAYMNT-INDEX
              MOVE SCHD-SUB TO ASCHD-SUB
              PERFORM ASCHD-AMOUNT-DUE
              MOVE ASCHD-AMTDUE TO CART-AMTDUE
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
           MOVE "Y" TO CART-NEG-FG.
           PERFORM CART.
           MOVE CART-PAYMENT TO PD-REGPYAMT.
           MOVE CART-BALLOON TO PD-LASTPYAMT.
           GO TO SET-ST-PAYMENT-EXIT.

       SET-ST-PAYMENT-CONT.

      * TEST FOR FULL CANCEL:

           MOVE 0 TO FINANCE-CHG.
           IF EPAY-PVALUE = ADDON-PREMIUM
              IF ZERO-ADDON-FINCHG-FOUND = "Y"
                 PERFORM ENTER-ADDON-FINCHG
                 MOVE ADDON-FINCHG TO REAL-MONEY
                 COMPUTE PD-REGPYAMT ROUNDED =
                     (EPAY-PVALUE + FINANCE-CHG + REAL-MONEY)
                                                 / PD-ORGTERM
              ELSE
                 MOVE ADDON-FINCHG TO REAL-MONEY
                 IF ADDON-INS-NO-LT-REC = 1
                           AND ADDON-INS-ANTICTERM = PD-ANTICTERM
                    MOVE ADDON-INS-PAYMNT-INCR TO PD-REGPYAMT
                 ELSE
                    COMPUTE PD-REGPYAMT ROUNDED =
                        (EPAY-PVALUE + FINANCE-CHG + REAL-MONEY)
                                                       / PD-ORGTERM
                 END-IF
           ELSE
              PERFORM CALCULATE-FINCHG-REFUND
              COMPUTE PD-REGPYAMT ROUNDED =
                  (EPAY-PVALUE + FINANCE-CHG + REAL-MONEY)
                                                 / PD-ORGTERM.
           COMPUTE PD-LASTPYAMT =
              (EPAY-PVALUE + FINANCE-CHG + REAL-MONEY)
                  - ((PD-ORGTERM - 1) * PD-REGPYAMT).

       SET-ST-PAYMENT-EXIT.
           EXIT.

      *************************************************************
       SET-RT-PAYMENT SECTION.
      *************************************************************

      *    COMPUTE SCHEDULE OF PAYMENTS FOR RT:

           MOVE 0 TO WHOLE-DOLLARS REAL-MONEY FINANCE-CHG.

           IF PD-SMPRATE = 0
              MOVE 0 TO WS-RATE FINANCE-CHG
              GO TO SET-RT-PAYMENT-CONT
           ELSE
              IF PD-LOANTYPE = "P"
                 IF PD-OVRDRATE-FG = "F"
      * MULLEN FIXED RATE
                    COMPUTE WS-RATE ROUNDED = PD-OVRDRATE / 1200
                    COMPUTE FINANCE-CHG =
                               EPAY-PVALUE * WS-RATE * PD-ANTICTERM
                    IF SP-ADDON-WHOLE-FG = "A"
                       ADD 0.5 FINANCE-CHG GIVING WHOLE-DOLLARS
                       MOVE WHOLE-DOLLARS TO FINANCE-CHG
                    END-IF
                    GO TO SET-RT-PAYMENT-CONT.

           IF EPAY-PVALUE = ADDON-PREMIUM
              GO TO SET-RT-PAYMENT-CONT.

           IF PD-ORGTERM = 1
              MOVE PD-ORGTERM TO EPAY-ORGTERM
              PERFORM CALC-EPAY-CALC
              COMPUTE FINANCE-CHG = EPAY-PAYMNT - EPAY-PVALUE
              IF SP-ADDON-WHOLE-FG = "A"
                 ADD 0.99 FINANCE-CHG GIVING WHOLE-DOLLARS
                 MOVE WHOLE-DOLLARS TO FINANCE-CHG
              END-IF
              GO TO SET-RT-PAYMENT-CONT.


      * TEST SP-CANCEL-ADDON-FG:
      *      WHEN = " " DEFAULT, DETERMINE INTEREST ADJUSTMENT FOR CANCEL
      *                 BY COMPUTING AN ADDON REFUND THE DAY OF
      *                 THE CANCEL
      *             "F" DETERMINE INTEREST ADJUSTMENT USING 'CART'
      *                 ROUTINE WHICH DETERMINES THE AMOUNT NEEDED
      *                 TO FINANCE THE ADJUSTMENT AMOUNT A48
      
           IF SP-CANCEL-ADDON-FG = "F"
              GO TO SET-RT-PAYMENT-IB-OR-ORIGLN.

           IF ( LN-LOANTYPE = "I"        ) OR
              ( FOUND-ADDON-FG NOT = "Y" ) OR
              ( LN-INSPREM(INS-SUB) NOT = ADDON-PREMIUM-NOFEES )
              GO TO SET-RT-PAYMENT-IB-OR-ORIGLN.

      *  COMPUTE INTEREST REBATE, REGPYAMT, AND LASTPYAMT USING
      *  LPPOF2 LOGIC.   (LIBGB/REBATE).

           PERFORM GET-INTEREST-REBATE-LPPOF2.

      *  COMPUTE REGPYAMT AND LASTPYAMT AS FOLLOWS:
      *
      *                      PROCEEDS + REBATE
      *  REGPYAMT ROUNDED =  -----------------
      *                           ORGTERM
      *
      *  LASTPYAMT = (PROCEEDS + REBATE) - (ORGTERM * REGPYAMT) + REGPYAMT

           COMPUTE PD-REGPYAMT ROUNDED =
               (PD-PROCEEDS + POFF-INT-REB-TBL(POFF-SUB)) /
                   PD-ORGTERM.
           COMPUTE PD-LASTPYAMT =
               (PD-PROCEEDS + POFF-INT-REB-TBL(POFF-SUB)) -
                (PD-ORGTERM * PD-REGPYAMT) + PD-REGPYAMT.

           GO TO SET-RT-PAYMENT-EXIT.

      *
      * EXECUTE THIS IF ACCOUNT IS IB OR INSURANCE WAS BOOKED WITH ORIGINAL LOAN
      *

       SET-RT-PAYMENT-IB-OR-ORIGLN.
           IF SP-CONSOLIDATE-RT-FG = "C"
              PERFORM GET-LT-SCHLD
              PERFORM GET-1ST-PAYMNT-INDEX
              MOVE SCHD-SUB TO ASCHD-SUB
              PERFORM ASCHD-AMOUNT-DUE
              MOVE ASCHD-AMTDUE TO CART-AMTDUE
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
           MOVE "Y" TO CART-NEG-FG.
           PERFORM CART.
           MOVE CART-PAYMENT TO PD-REGPYAMT.
           MOVE CART-BALLOON TO PD-LASTPYAMT.
           GO TO SET-RT-PAYMENT-EXIT.

       SET-RT-PAYMENT-CONT.

      * TEST FOR FULL CANCEL:

           MOVE 0 TO FINANCE-CHG.
           IF EPAY-PVALUE = ADDON-PREMIUM
              IF ZERO-ADDON-FINCHG-FOUND = "Y"
                 PERFORM ENTER-ADDON-FINCHG
                 MOVE ADDON-FINCHG TO REAL-MONEY
                 COMPUTE PD-REGPYAMT ROUNDED =
                     (EPAY-PVALUE + FINANCE-CHG + REAL-MONEY)
                                                 / PD-ORGTERM
              ELSE
                 MOVE ADDON-FINCHG TO REAL-MONEY
                 IF ADDON-INS-NO-LT-REC = 1
                    MOVE ADDON-INS-PAYMNT-INCR TO PD-REGPYAMT
                 ELSE
                    COMPUTE PD-REGPYAMT ROUNDED =
                        (EPAY-PVALUE + FINANCE-CHG + REAL-MONEY)
                                                       / PD-ORGTERM
                 END-IF
           ELSE
              PERFORM CALCULATE-FINCHG-REFUND
              COMPUTE PD-REGPYAMT ROUNDED =
                  (EPAY-PVALUE + FINANCE-CHG + REAL-MONEY)
                                                 / PD-ORGTERM.
           COMPUTE PD-LASTPYAMT =
              (EPAY-PVALUE + FINANCE-CHG + REAL-MONEY)
                  - ((PD-ORGTERM - 1) * PD-REGPYAMT).

       SET-RT-PAYMENT-EXIT.
           EXIT.

      *************************************************************
       SET-ET-TERM-AND-PAYMENT SECTION.
      *************************************************************

      * COMPUTE TERM OF EXTENTION AND SCHEDULE OF PAYMENTS:

           MOVE 0 TO WHOLE-DOLLARS REAL-MONEY CART-AMTDUE.
           MOVE LN-SUM-REGPYAMT TO PD-REGPYAMT.

           IF PD-SMPRATE = 0
              GO TO SET-ET-PAYMENT-FLAT
           ELSE
              IF PD-LOANTYPE = "P"
                 IF PD-OVRDRATE-FG = "F"
                    GO TO SET-ET-PAYMENT-FLAT.

           IF EPAY-PVALUE = ADDON-PREMIUM
              IF ZERO-ADDON-FINCHG-FOUND = "Y"
                 GO TO SET-ET-PAYMENT-FLAT.

      * TEST SP-CANCEL-ADDON-FG:
      *      WHEN = " " DEFAULT, DETERMINE INTEREST ADJUSTMENT FOR CANCEL
      *                 BY COMPUTING AN ADDON REFUND THE DAY OF
      *                 THE CANCEL
      *             "F" DETERMINE INTEREST ADJUSTMENT USING 'CART'
      *                 ROUTINE WHICH DETERMINES THE AMOUNT NEEDED
      *                 TO FINANCE THE ADJUSTMENT AMOUNT A48
      
           IF SP-CANCEL-ADDON-FG = "F"
              GO TO SET-ET-PAYMENT-IB-OR-ORIGLN.

           IF ( LN-LOANTYPE = "I"        ) OR
              ( FOUND-ADDON-FG NOT = "Y" ) OR
              ( LN-INSPREM(INS-SUB) NOT = ADDON-PREMIUM-NOFEES )
              GO TO SET-ET-PAYMENT-IB-OR-ORIGLN.

      * COMPUTE INTEREST REBATE, REGPYAMT, AND LASTPYAMT USING
      *  LPPOF2 LOGIC.   (LIBGB/REBATE).

           PERFORM GET-INTEREST-REBATE-LPPOF2.

      *-----------------------------------------------------------------
      * COMPUTE REGPYAMT AND LASTPYAMT AS FOLLOWS:
      *
      *  ORGTERM  =  (PROCEEDS + REBATE) / REGPYAMT + 1
      *
      *  IF ORGTERM  =  1
      *     LASTPYAMT  =  PROCEEDS + REBATE
      *  ELSE
      *     LASTPYAMT  =  (PROCEEDS + REBATE) - (ORGTERM * REGPYAMT)
      *
      *-----------------------------------------------------------------
           COMPUTE PD-ORGTERM =
              (PD-PROCEEDS + POFF-INT-REB-TBL(POFF-SUB)) /
                  PD-REGPYAMT + 1.
           IF PD-ORGTERM = 1
              COMPUTE PD-LASTPYAMT =
                  PD-PROCEEDS + POFF-INT-REB-TBL(POFF-SUB)
           ELSE
              COMPUTE PD-LASTPYAMT =
                  (PD-PROCEEDS + POFF-INT-REB-TBL(POFF-SUB)) -
                   ((PD-ORGTERM - 1) * PD-REGPYAMT).

           GO TO SET-ET-PAYMENT-EXIT.


      *
      * EXECUTE THIS IF ACCOUNT IS IB OR INSURANCE WAS BOOKED WITH ORIGINAL LOAN
      *
       SET-ET-PAYMENT-IB-OR-ORIGLN.

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
           MOVE "Y" TO CART-NEG-FG.
           PERFORM CARTET.
           IF CART-TERM = 1
              MOVE CART-BALLOON TO PD-1STPYAMT
                                   PD-REGPYAMT
           ELSE
              MOVE CART-BALLOON TO PD-LASTPYAMT.
           MOVE CART-TERM TO PD-ORGTERM.
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

      * TEST FOR FULL CANCEL:

           IF EPAY-PVALUE = ADDON-PREMIUM
              IF ZERO-ADDON-FINCHG-FOUND = "Y"
                 PERFORM ENTER-ADDON-FINCHG
                 MOVE ADDON-FINCHG TO REAL-MONEY
              ELSE
                 MOVE ADDON-FINCHG TO REAL-MONEY
           ELSE
              PERFORM CALCULATE-FINCHG-REFUND.

           COMPUTE PD-LASTPYAMT =
              (EPAY-PVALUE + REAL-MONEY )
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

      ********************************************
       GET-EPAY-PVALUE SECTION.
      ********************************************
           COMPUTE EPAY-PVALUE = PD-PROCEEDS
              + PD-FEEAMT(1)  + PD-FEEAMT(2)  + PD-FEEAMT(3)
              + PD-FEEAMT(4)  + PD-FEEAMT(5)  + PD-FEEAMT(6)
              + PD-FEEAMT(7)  + PD-FEEAMT(8)  + PD-FEEAMT(9)
              + PD-FEEAMT(10).

      ********************************************
       CALC-EPAY-CALC SECTION.
      ********************************************
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

      *************************************************
       ENTER-ADDON-FINCHG SECTION.
      *************************************************
       ENTER-ADDON-FC.
           PERFORM RESET-LEGEND.
           MOVE "ADDON CHARGES MISSING FROM CONVERSION!!" TO MESS.
           PERFORM SEND-MESS.
           MOVE "ENTER ORIGINAL ADDON FINANCE CHARGE :" TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "ENNN072INTCHG." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS NOT = "00"
              GO TO ENTER-ADDON-FC.

           MOVE VDUNUM2 TO ADDON-FINCHG.

           MOVE " "     TO LEGEND.
           PERFORM SEND-LEGEND.

      ******************************************
       CALCULATE-FINCHG-REFUND SECTION.
      ******************************************

      * CALCULATE REFUND OF INTEREST
      * MULLEN RULE 78THS

           MOVE LN-TOTNODEF       TO HOLD-LN-TOTNODEF.
           MOVE LN-TOTNOALDEL     TO HOLD-LN-TOTNOALDEL.

           MOVE ADDON-PRIOR-NODEF TO LN-TOTNODEF.
           MOVE 0                 TO LN-TOTNOALDEL.

           MOVE "Y"               TO MDTE-ORGTERM-FG.
           PERFORM MATURITY-DATE-CALCULATION.

           MOVE HOLD-LN-TOTNODEF   TO LN-TOTNODEF.
           MOVE HOLD-LN-TOTNOALDEL TO LN-TOTNOALDEL.

           MOVE LN-INSEFF-DATE(INS-SUB) TO NUM-DATE.
           MOVE MDTE-DATE               TO SYS-DATE.
           PERFORM TIM.
           IF ELAPSED-MONTHS NOT > 0
              MOVE ADDON-FINCHG TO REAL-MONEY
              GO TO CALCULATE-FINCHG-REFUND-EXIT.
           MOVE ELAPSED-MONTHS TO OTRM.

           MOVE LN-INSEFF-DATE(INS-SUB) TO NUM-DATE.
           MOVE PD-LOANDATE             TO SYS-DATE.
           PERFORM TIM.
           IF ELAPSED-REM > 14
              ADD 1 TO ELAPSED-MONTHS.
           IF ELAPSED-MONTHS < 1
              MOVE ADDON-FINCHG TO REAL-MONEY
              GO TO CALCULATE-FINCHG-REFUND-EXIT.
           IF ELAPSED-MONTHS > OTRM
              MOVE 0 TO REAL-MONEY
              GO TO CALCULATE-FINCHG-REFUND-EXIT.
           SUBTRACT ELAPSED-MONTHS FROM OTRM GIVING RTRM.

           COMPUTE REAL-MONEY ROUNDED =
              ADDON-FINCHG * (RTRM * (RTRM + 1))
                                       / (OTRM * (OTRM + 1)).

       CALCULATE-FINCHG-REFUND-EXIT.
           EXIT.

      ********************************************
       TEST-FOR-GEORGIA-SCHG SECTION.
      ********************************************

           IF NOT (SP-SCFRMLA = 12 OR 13 OR 54)
              GO TO TEST-FOR-GEORGIA-SCHG-EXIT.

           MOVE " " TO SCHG-UPDATE-FG.
           MOVE 0 TO SCHG-NEW-ACCTNO.
           MOVE PD-SSNO(1) TO SCHG-LN-SSNO(1).
           MOVE PD-SSNO(2) TO SCHG-LN-SSNO(2).
           MOVE PD-SSNO(3) TO SCHG-LN-SSNO(3).
           MOVE PD-SSNO(4) TO SCHG-LN-SSNO(4).
           MOVE TRANS-DATE TO SCHG-TRANS-DATE.
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
           MOVE SCHG-RESTRICT-ADJ TO PD-RESTRICT-ADJ.
           MOVE SCHG-DUAL-LOAN-ADJ TO PD-DUAL-LOAN-ADJ.

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
                    PD-REGPYAMT * (PD-ORGTERM - 1)
                           + PD-LASTPYAMT - CART-AMTDUE.

           IF PD-SRCD NOT = "ET"
              IF PD-ORGTERM = 1
                 MOVE PD-REGPYAMT TO TOTAL-REPAYABLE
              ELSE
                 COMPUTE TOTAL-REPAYABLE =
                    PD-REGPYAMT * (PD-ORGTERM - 1) + PD-LASTPYAMT.

           COMPUTE PD-FINAMT = PD-PROCEEDS
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

           PERFORM SET-NEW-INSTYPES.

           MOVE DF2-BUF TO WR-BUF.
           MOVE DF2-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

           PERFORM RESET-LEGEND.

      *********************************************************
       SET-NEW-INSTYPES SECTION.
      *********************************************************
           IF PD-INSCODES = "NNN" OR PD-PROCASH > 4
              GO TO SET-NEW-INSCODES-EXIT.

      * CANCEL CL  (DL/LL)

           IF PD-PROCASH = 1
                   IF PD-INSTYPES = "W" OR "X"
                      MOVE "V" TO PD-INSTYPES
              ELSE IF PD-INSTYPES = "O" OR "P"
                      MOVE "Q" TO PD-INSTYPES
              ELSE IF PD-INSTYPES = "A" OR "I"
                      MOVE "1" TO PD-INSTYPES
              ELSE IF PD-INSTYPES = "2" OR "5"
                      MOVE "8" TO PD-INSTYPES
              ELSE IF PD-INSTYPES = "3" OR "6"
                      MOVE "9" TO PD-INSTYPES
              ELSE IF PD-INSTYPES = "4" OR "7"
                      MOVE "0" TO PD-INSTYPES
              ELSE IF PD-INSTYPES = "U" OR "Y"
                      MOVE "Z" TO PD-INSTYPES
              ELSE IF PD-INSTYPES = "S" OR "T"
                      MOVE "R" TO PD-INSTYPES
              ELSE IF PD-INSTYPES = "L" OR "M"
                      MOVE "K" TO PD-INSTYPES
              ELSE IF PD-INSTYPES = "H" OR "J"
                      MOVE "E" TO PD-INSTYPES
              ELSE IF PD-INSTYPES = "F" OR "G"
                      MOVE "D" TO PD-INSTYPES
              ELSE IF PD-INSTYPES = "B" OR "C"
                      MOVE "N" TO PD-INSTYPES.

      * CANCEL AH  (ACCIDENT & HEALTH)

           IF PD-PROCASH = 2
                   IF PD-INSTYPES = "W"
                      MOVE "S" TO PD-INSTYPES
              ELSE IF PD-INSTYPES = "X"
                      MOVE "T" TO PD-INSTYPES
              ELSE IF PD-INSTYPES = "V"
                      MOVE "R" TO PD-INSTYPES
              ELSE IF PD-INSTYPES = "L"
                      MOVE "H" TO PD-INSTYPES
              ELSE IF PD-INSTYPES = "O"
                      MOVE "A" TO PD-INSTYPES
              ELSE IF PD-INSTYPES = "P"
                      MOVE "I" TO PD-INSTYPES
              ELSE IF PD-INSTYPES = "Q"
                      MOVE "1" TO PD-INSTYPES
              ELSE IF PD-INSTYPES = "3"
                      MOVE "U" TO PD-INSTYPES
              ELSE IF PD-INSTYPES = "6"
                      MOVE "Y" TO PD-INSTYPES
              ELSE IF PD-INSTYPES = "9"
                      MOVE "Z" TO PD-INSTYPES
              ELSE IF PD-INSTYPES = "4"
                      MOVE "2" TO PD-INSTYPES
              ELSE IF PD-INSTYPES = "7"
                      MOVE "5" TO PD-INSTYPES
              ELSE IF PD-INSTYPES = "0"
                      MOVE "8" TO PD-INSTYPES
              ELSE IF PD-INSTYPES = "M"
                      MOVE "J" TO PD-INSTYPES
              ELSE IF PD-INSTYPES = "K"
                      MOVE "E" TO PD-INSTYPES
              ELSE IF PD-INSTYPES = "F"
                      MOVE "B" TO PD-INSTYPES
              ELSE IF PD-INSTYPES = "G"
                      MOVE "C" TO PD-INSTYPES
              ELSE IF PD-INSTYPES = "D"
                      MOVE "N" TO PD-INSTYPES.

      * CANCEL PP  (PERSONAL PROPERTY)

           IF PD-PROCASH = 3
                   IF PD-INSTYPES = "E" OR "R"
                      MOVE "N" TO PD-INSTYPES
              ELSE IF PD-INSTYPES = "H" OR "S"
                      MOVE "B" TO PD-INSTYPES
              ELSE IF PD-INSTYPES = "J" OR "T"
                      MOVE "C" TO PD-INSTYPES
              ELSE IF PD-INSTYPES = "K" OR "V"
                      MOVE "D" TO PD-INSTYPES
              ELSE IF PD-INSTYPES = "L" OR "W"
                      MOVE "F" TO PD-INSTYPES
              ELSE IF PD-INSTYPES = "3" OR "4"
                      MOVE "O" TO PD-INSTYPES
              ELSE IF PD-INSTYPES = "6" OR "7"
                      MOVE "P" TO PD-INSTYPES
              ELSE IF PD-INSTYPES = "9" OR "0"
                      MOVE "Q" TO PD-INSTYPES
              ELSE IF PD-INSTYPES = "U" OR "2"
                      MOVE "A" TO PD-INSTYPES
              ELSE IF PD-INSTYPES = "Y" OR "5"
                      MOVE "I" TO PD-INSTYPES
              ELSE IF PD-INSTYPES = "Z" OR "8"
                      MOVE "1" TO PD-INSTYPES
              ELSE IF PD-INSTYPES = "M" OR "X"
                      MOVE "G" TO PD-INSTYPES.

      * CANCEL O1  (OTHER #1)

           IF PD-PROCASH = 4
                   IF PD-INSTYPES = "1"
                      MOVE "N" TO PD-INSTYPES
              ELSE IF PD-INSTYPES = "A"
                      MOVE "B" TO PD-INSTYPES
              ELSE IF PD-INSTYPES = "I"
                      MOVE "C" TO PD-INSTYPES
              ELSE IF PD-INSTYPES = "O"
                      MOVE "F" TO PD-INSTYPES
              ELSE IF PD-INSTYPES = "P"
                      MOVE "G" TO PD-INSTYPES
              ELSE IF PD-INSTYPES = "Q"
                      MOVE "D" TO PD-INSTYPES
              ELSE IF PD-INSTYPES = "3"
                      MOVE "L" TO PD-INSTYPES
              ELSE IF PD-INSTYPES = "6"
                      MOVE "M" TO PD-INSTYPES
              ELSE IF PD-INSTYPES = "9"
                      MOVE "K" TO PD-INSTYPES
              ELSE IF PD-INSTYPES = "4"
                      MOVE "W" TO PD-INSTYPES
              ELSE IF PD-INSTYPES = "7"
                      MOVE "X" TO PD-INSTYPES
              ELSE IF PD-INSTYPES = "0"
                      MOVE "V" TO PD-INSTYPES
              ELSE IF PD-INSTYPES = "U"
                      MOVE "H" TO PD-INSTYPES
              ELSE IF PD-INSTYPES = "Y"
                      MOVE "J" TO PD-INSTYPES
              ELSE IF PD-INSTYPES = "Z"
                      MOVE "E" TO PD-INSTYPES
              ELSE IF PD-INSTYPES = "2"
                      MOVE "S" TO PD-INSTYPES
              ELSE IF PD-INSTYPES = "5"
                      MOVE "T" TO PD-INSTYPES
              ELSE IF PD-INSTYPES = "8"
                      MOVE "R" TO PD-INSTYPES.


       SET-NEW-INSCODES-EXIT.
           EXIT.

      *****************************************
       GET-ADDON-FINCHG SECTION.
      *****************************************
           MOVE 0 TO ADDON-FINCHG-WK.
           MOVE "Y" TO MDTE-ORGTERM-FG.
           MOVE GET-ADDON-FINCHG-MDTE-FLAG TO MDTE-FLAG.
           PERFORM MATURITY-DATE-CALCULATION.
           MOVE LTI-INSEFF-DATE TO NUM-DATE.
           MOVE MDTE-DATE       TO SYS-DATE.
           PERFORM TIM.
           IF ELAPSED-DAYS NOT < 0
              COMPUTE ADDON-FINCHG-WK =
                 ( LTI-INSPREM + (LTI-INSFEE(1) * ONE-2)
                                       + (LTI-INSFEE(2) * ONE-2)
                 ) * ELAPSED-MONTHS * .01.

           MOVE " " TO GET-ADDON-FINCHG-MDTE-FLAG.

      **************************************************
       GET-1ST-PAYMNT-INDEX SECTION.
      **************************************************

      * DETERMINE 1STPAYMENT INDEX TO UNPACKED TABLE

           MOVE LN-1STPYDATE TO NUM-DATE.
           MOVE PD-1STPYDATE TO SYS-DATE.
           PERFORM TIM360.

           COMPUTE SCHD-SUB = ELAPSED-MONTHS + 1
                         - ADDON-PRIOR-NODEF.
           IF SCHD-SUB < 1
              MOVE 1 TO SCHD-SUB.

      *********************************************************
       ASCHD-AMOUNT-DUE SECTION.
      *********************************************************

      * GET REMAINING SCHEDULED AMOUNT DUE

           MOVE 0 TO ASCHD-AMTDUE.
           IF ASCHD-SUB < 1
              GO TO ASCHD-AMOUNT-DUE-EXIT.

           PERFORM ASCHD-AMOUNT-DUE-TALLY
              UNTIL SCHD-SUB > 360 OR
                    SCHD-AMT(SCHD-SUB) = 0.
           GO TO ASCHD-AMOUNT-DUE-EXIT.

       ASCHD-AMOUNT-DUE-TALLY.
           ADD SCHD-AMT(SCHD-SUB) TO ASCHD-AMTDUE.
           ADD 1 TO SCHD-SUB.

       ASCHD-AMOUNT-DUE-EXIT.
           EXIT.

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
      ***********************************************************
           MOVE PD-REGPYAMT TO PD-1STPYAMT.
           IF PD-ORGTERM = 1
              MOVE 0 TO PD-NOREGPY
                        PD-LASTPYAMT
           ELSE
           IF PD-ORGTERM = 2
              MOVE 0 TO PD-NOREGPY
           ELSE
              SUBTRACT 2 FROM PD-ORGTERM GIVING PD-NOREGPY.

      ***************************************************
       SEND-PROMPT SECTION.
      ***************************************************
           IF ELE = 1
              MOVE "ENTER CANCELLATION DATE" TO LEGEND.
           IF ELE = 2
              MOVE
           "'RT', 'ET', 'NP', 'LP' OR 'ST' FOR CANCEL" TO LEGEND
              INSPECT MESS REPLACING ALL "'" BY WK-QUOTE.
           IF ELE = 3
              MOVE
           "1=CL, 2=AH, 3=PP, 4=O1, 5=O2, 6=O3" TO LEGEND.
           IF ELE = 4
              MOVE "ENTER AMOUNT OF CANCEL " TO LEGEND.
           IF ELE = 5
              MOVE "ENTER INSURANCE CODE" TO LEGEND.
           PERFORM SEND-LEGEND.

      ***************************************************
       VERIFY-REQD-FLDS SECTION.
      ***************************************************
           MOVE 0 TO ERR-FG.
           IF EPAY-PVALUE = 0
              MOVE 9 TO ERR-FG.
           IF PD-PURCD = " "
              MOVE 9 TO ERR-FG
              MOVE "PURPOSE CODE REQUIRED BEFORE UPDATE" TO MESS
              PERFORM SEND-MESS.
           IF PD-COLLCD = " "
              MOVE 9 TO ERR-FG
              MOVE "COLLATERAL CODE REQUIRED BEFORE UPDATE" TO MESS
              PERFORM SEND-MESS.
           IF PD-INSCOMP(4) NOT = 0 AND PD-INSCOMP(5) NOT = 0
              MOVE 9 TO ERR-FG
              MOVE "ONLY 1 INSURANCE MAY BE ADDED" TO MESS
              PERFORM SEND-MESS.

      *    -------------------------------------------------------------
      *    TEST CURBAL <= 0
      *    -------------------------------------------------------------
           IF (LN-CURBAL - LN-OT2BAL) - PD-LNAMT < 0
              MOVE 9 TO ERR-FG
              MOVE " CANNOT TAKE PRINCIPLE BALANCE BELOW $0" TO MESS
              PERFORM SEND-MESS.
           IF (LN-CURBAL - LN-OT2BAL) - PD-LNAMT = 0
              MOVE 9 TO ERR-FG
              MOVE " CANNOT TAKE PRINCIPLE BALANCE TO $0" TO MESS
              PERFORM SEND-MESS.
      *    -------------------------------------------------------------
      *    TEST NEGATIVE ADJUSTMENT ON NP AND LP TRANSACTIONS:
      *         IF NP, TEST REGPYAMT AGAINST NEXT SCHEDULED PAYMENT
      *         IF LP, TEST REGPYAMT AGAINST LAST SCHEDULED PAYMENT
      *      EXAMPLE 1:
      *        "NP" TRANSACTION FOR -300.00.
      *        PMT #  SCHD PMT    ADJUSTMENT         TEST        RESULT
      *        -----  --------    ---------- ------------------- ------
      *          1     295.00      300.00    295.00 - 300.00 < 0 FAILED
      *          2     295.00
      *          3     295.00 ...
      *
      *      EXAMPLE 2:
      *        "LP" TRANSACTION FOR -300.00.
      *        PMT #  SCHD PMT    ADJUSTMENT         TEST        RESULT
      *        -----  --------    ---------- ------------------- ------
      *      ...22     295.00
      *         23     295.00
      *         24     294.00      300.00    295.00 - 300.00 < 0 FAILED
      *    -------------------------------------------------------------
           IF PD-SRCD = "NP" OR "LP"
              PERFORM GET-LT-SCHLD
              PERFORM GET-1ST-PAYMNT-INDEX
              IF PD-LNAMT > SCHD-AMT(SCHD-SUB)
                 MOVE 9 TO ERR-FG
                 MOVE SCHD-AMT(SCHD-SUB) TO DISP-NUM2
                 IF PD-SRCD = "NP"
                   STRING "CANCEL AMOUNT EXCEEDS NEXT PAYMENT OF: "
                           DISP-NUM2 DELIMITED BY SIZE INTO MESS
                 ELSE
                   STRING "CANCEL AMOUNT EXCEEDS LAST PAYMENT OF: "
                           DISP-NUM2 DELIMITED BY SIZE INTO MESS
                 END-IF
                 PERFORM SEND-MESS.

      *    -------------------------------------------------------------
      *    TEST NEGATIVE ADJUSTMENT ON ST TRANSACTIONS:
      *         1. TEST REGPAYAMT AGAINST SCHEDULED PAYMENTS
      *         2. TEST LASTPAYAMT AGAINST SCHEDULED PAYMENT
      *         3. TEST LOAN AMOUNT AGAINST TOTAL OF SCHEDULED PAYMENTS
      *                AFFECTED BY ADDON.
      *      EXAMPLE:
      *         "ST" FOR TERM OF 4 STARTING AT PAYMENT 11 FOR 1185.00
      *         PAYMENTS 11 AND 12 AFFECTED BY PREVIOUS ADDON.
      *        PMT #  SCHD PMT    ADJUSTMENT       TEST         RESULT
      *        -----  --------    ---------- -----------------  ------
      *      ... 10     295.00
      *                -------    -------
      *          11     300.00     297.00   300.00 - 297.00 > 0 PASSED
      *          12     300.00     297.00   300.00 - 297.00 > 0 PASSED
      *          13     295.00     297.00   295.00 - 297.00 < 0 FAILED
      *          14     295.00     294.00   295.00 - 294.00 > 0 PASSED
      *                =======    =======
      *      TOTAL     1190.00    1185.00   300.00 - 297.00 > 0 PASSED
      *                -------    -------
      *          15     295.00 ...
      *    -------------------------------------------------------------
           IF PD-SRCD = "ST"
              MOVE 0 TO WORK-AMT
              PERFORM GET-LT-SCHLD
              PERFORM GET-1ST-PAYMNT-INDEX
              PERFORM VARYING SUB1 FROM SCHD-SUB BY 1
                       UNTIL SUB1 > (SCHD-SUB + PD-ORGTERM - 1)
                 ADD SCHD-AMT(SUB1) TO WORK-AMT
                 IF (SUB1 < (SCHD-SUB + PD-ORGTERM - 1) AND
                                     PD-REGPYAMT > SCHD-AMT(SUB1)) OR
                    (SUB1 = (SCHD-SUB + PD-ORGTERM - 1) AND
                                     PD-LASTPYAMT > SCHD-AMT(SUB1))
                       MOVE 9 TO ERR-FG
                       MOVE "PAYMENT &&& WILL GO BELOW ZERO" TO MESS
                       MOVE SUB1 TO DISP-NUM0
                       INSPECT MESS REPLACING ALL "&&&" BY DISP-NUM0
                       PERFORM SEND-MESS
                 END-IF
              END-PERFORM
              IF PD-LNAMT > WORK-AMT
                   MOVE 9 TO ERR-FG
                   MOVE WORK-AMT TO DISP-NUM2
                   STRING "CANCEL AMOUNT EXCEEDS TOTAL OF"
                       " NEXT &&& PAYMENTS: "
                       DISP-NUM2 DELIMITED BY SIZE INTO MESS
                   SUBTRACT 1 FROM SUB1
                   MOVE SUB1 TO DISP-NUM0
                   INSPECT MESS REPLACING ALL "&&&" BY DISP-NUM0
                   PERFORM SEND-MESS.

      *    -------------------------------------------------------------
      *    TEST NEGATIVE ADJUSTMENT ON RT TRANSACTIONS:
      *         1. TEST LOAN AMOUNT AGAINST TOTAL OF SCHEDULED PAYMENTS
      *                AFFECTED BY ADDON.
      *      EXAMPLE:
      *         "RT" ADDON STARTING AT PAYMENT 21 FOR 1185.00
      *        PMT #  SCHD PMT    ADJUSTMENT       TEST         RESULT
      *        -----  --------    ---------- -----------------  ------
      *      ... 20     295.00
      *                -------    -------
      *          21     295.00     297.00
      *          22     295.00     297.00
      *          23     295.00     297.00
      *          24     295.00     294.00
      *                =======    =======
      *      TOTAL     1180.00    1185.00 1180.00 - 1185.00 < 0 PASSED
      *    -------------------------------------------------------------
      D    STOP MESS.
           IF PD-SRCD = "RT"
              MOVE 0 TO WORK-AMT
              PERFORM GET-LT-SCHLD
              PERFORM GET-1ST-PAYMNT-INDEX
              PERFORM VARYING SUB1 FROM SCHD-SUB BY 1
                       UNTIL SUB1 > (SCHD-SUB + PD-ORGTERM - 1)
                 ADD SCHD-AMT(SUB1) TO WORK-AMT
              END-PERFORM
              IF PD-LNAMT > WORK-AMT
                   MOVE 9 TO ERR-FG
                   MOVE WORK-AMT TO DISP-NUM2
                   STRING "CANCEL AMOUNT EXCEEDS TOTAL OF"
                       " REMAINING &&& PAYMENTS: "
                       DISP-NUM2 DELIMITED BY SIZE INTO MESS
                   SUBTRACT 1 FROM SUB1
                   MOVE SUB1 TO DISP-NUM0
                   INSPECT MESS REPLACING ALL "&&&" BY DISP-NUM0
                   PERFORM SEND-MESS.

      *    -------------------------------------------------------------
      *    VALIDATE THAT LOANDATE(EFFECTIVE) IS NOT BEFORE ORIGINAL
      *    INSURANCE EFFECTIVE DATE
      *    -------------------------------------------------------------
           IF PD-LOANDATE < LN-INSEFF-DATE(INS-SUB)
              MOVE "CANCEL DATE PRIOR TO ORIGINAL INS. EFFECT. DATE"
                TO MESS
              PERFORM SEND-MESS
              MOVE 9 TO ERR-FG.
       EXIT-VERIFY-REQD-FLDS.
           EXIT.


      ********************************************************
      *                  COPY MEMBERS
      ********************************************************
      *
      *-------------------------------------------------
       COPY "LIBLP/LPPOF2.CPY".
      *    THE PERFORM STATEMENT AND ADDITIONAL LOGIC
      *    MUST FOLLOW THE COPY "LIBLP/LPPOF2" STATEMENT
      *
           PERFORM SEARCH-FOR-ADDON-LP.

           MOVE LP-REFNO TO LPWSLP-LP-REFNO
           IF SEARCH-FOR-ADDON-LP-FG = " " OR
                LPWSLP-LP-ADDON-TEXT = "CNL"
              MOVE "Y" TO POFF-RTNCD
           ELSE
              MOVE " " TO POFF-RTNCD.
           MOVE LP-SEQNO TO BEG-LP-SEQNO.

       PAYOFF-INT-ADDONS-GET-LP-REC-E.
           EXIT.

       COPY "LIBGB/DATER.CPY".
       COPY "LIBLP/LPAPRS.CPY".
       COPY "LIBLP/LPAPRZ.CPY".
       COPY "LIBLP/LPUPER.CPY".
       COPY "LIBGB/REBATE.CPY".
       COPY "LIBLP/LPANTI.CPY".
       COPY "LIBLP/LPMENU.CPY".
       COPY "LIBLP/LPMDTE.CPY".
       COPY "LIBLP/LPCEPP.CPY".
       COPY "LIBLP/LPDAMTS.CPY".
       COPY "LIBLP/LPPDCL.CPY".
       COPY "LIBLP/LPEPAY.CPY".
       COPY "LIBLP/LPCART.CPY".
       COPY "LIBLP/LPITRM.CPY".
       COPY "LIBLP/LPSCHD.CPY".
       COPY "LIBGB/RBACT.CPY".
       COPY "LIBLP/LPUPWK.CPY".
       COPY "LIBLP/GETSCH.CPY".
       COPY "LIBLP/SETCTBL.CPY".
       COPY "LIBLP/ADDONSPR.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/GPENV.CPY".


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
       SET-EXTERNALS.
           MOVE LN-OWNBR      TO EXT-WI-CL-OWNBR.
           MOVE LN-ACCTNO     TO EXT-WI-CL-ACCTNO.
      *    MOVE LN-APP-NUMBER TO EXT-WI-CL-APP-NUMBER.
           MOVE 1             TO EXT-WI-CL-SSNO-NO.
           MOVE LN-SSNO(1)    TO EXT-WI-CL-SSNO(1).
           MOVE TRANS-DATE    TO EXT-WI-CL-TRANS-DATE.
       FORM-RESET.
       GET-INTEREST-REBATE-LPPOF2.
      * COMPUTE INTEREST REBATE, REGPYAMT, AND LASTPYAMT USING
      * LPPOF2 LOGIC.   (LIBGB/REBATE).
           PERFORM POFF-CLEAR-FIELDS.
           MOVE PD-LOANDATE              TO POFF-PAYDATE.
           MOVE LN-MAKERCD(1)            TO POFF-MAKERCD.
           MOVE " "                      TO POFF-LONPFA-UPD-FG.
           MOVE " "                      TO POFF-DEF-STOP.
           MOVE " "                      TO POFF-LPTRCD.
           MOVE PD-PROCASH               TO POFF-SUB.
           MOVE LN-INSEFF-DATE(POFF-SUB) TO POFF-INSEFF-TEST.
           MOVE LN-INSEXP-DATE(POFF-SUB) TO POFF-INSEXP-TEST.
           MOVE 2                        TO BEG-LP-SEQNO.
           PERFORM PAYOFF-INT-ADDONS.
       TEST-ADDON-TYPE.
           MOVE 0 TO ERR-FG.
           IF FOUND-ST-ADDON-FG = "Y"
              IF PD-SRCD NOT = "ST"
                 PERFORM RESET-LEGEND
                 MOVE "ADDON WAS DONE (ST) SET TERM, MUST CANCEL 'ST'"
                                                     TO MESS
                 PERFORM SEND-MESS
                 MOVE 9 TO ERR-FG.

           IF FOUND-LP-ADDON-FG = "Y"
              IF PD-SRCD NOT = "LP"
                 PERFORM RESET-LEGEND
                 MOVE "ADDON WAS DONE (LP) LAST PAY, MUST CANCEL 'LP'"
                                                     TO MESS
                 PERFORM SEND-MESS
                 MOVE 9 TO ERR-FG.
           IF PD-SRCD = "LP"
              IF FOUND-LP-ADDON-FG NOT = "Y"
                 PERFORM RESET-LEGEND
                 MOVE "LAST PAY ADDON NOT FOUND" TO MESS
                 PERFORM SEND-MESS
                 MOVE 9 TO ERR-FG.
           IF PD-SRCD = "ET"
              IF FOUND-ET-ADDON-FG NOT = "Y"
                 PERFORM RESET-LEGEND
                 MOVE "EXTEND TERM ADDON NOT FOUND" TO MESS
                 PERFORM SEND-MESS
                 MOVE 9 TO ERR-FG.
           IF PD-SRCD = "RT"
              IF FOUND-ET-ADDON-FG = "Y"
                 PERFORM RESET-LEGEND
                 MOVE "EXTEND TERM ADDON WAS FOUND" TO MESS
                 PERFORM SEND-MESS
                 MOVE 9 TO ERR-FG.
       CALL-MEMO.
           MOVE LN-OWNBR  TO EXT-WI-CL-OWNBR.
           MOVE LN-ACCTNO TO EXT-WI-CL-ACCTNO.
      *    MOVE LN-APP-NUMBER TO EXT-WI-CL-APP-NUMBER.
           MOVE 1 TO EXT-WI-CL-SSNO-NO.
           MOVE LN-SSNO(1) TO EXT-WI-CL-SSNO(1).
           MOVE TRANS-DATE TO EXT-WI-CL-TRANS-DATE.
           MOVE "T" TO MEMOSC-ONLY.

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

       POFF-CLEAR-FIELDS.
           MOVE 0 TO POFF-REBATE(1)  POFF-REBATE(2)
                     POFF-REBATE(3)  POFF-REBATE(4)
                     POFF-REBATE(5)  POFF-REBATE(6)
                     POFF-REBATE(7)  POFF-REBATE(8)
                     POFF-REBATE(9)  POFF-REBATE(10)
                     POFF-REBATE(11) POFF-REBATE(12)
                     POFF-REBATE(13)
                     POFF-INS-TAX-RB(1) POFF-INS-TAX-RB(2)
                     POFF-INS-TAX-RB(3) POFF-INS-TAX-RB(4)
                     POFF-INS-TAX-RB(5) POFF-INS-TAX-RB(6)
                     POFF-INS-TAX-RB(7) POFF-INS-TAX-RB(8)
                     POFF-O2DUE
                     POFF-OTDUE     POFF-LCDUE
                     POFF-INTDUE    POFF-NETDUE
                     POFF-PARTIALS  POFF-LCPAID.
           MOVE 0 TO POFF-ADDON-INT-EARNED
                     POFF-ADDON-INT-REBATE.
           MOVE 0 TO POFF-INT-REB-TBL(1)  POFF-INT-REB-TBL(5)
                     POFF-INT-REB-TBL(2)  POFF-INT-REB-TBL(6)
                     POFF-INT-REB-TBL(3)  POFF-INT-REB-TBL(7)
                     POFF-INT-REB-TBL(4)  POFF-INT-REB-TBL(8).

       CLEAR-PD-FEES.
           MOVE " " TO PD-FEECODE(1)   PD-FEECODE(2)
                       PD-FEEFINCHG(1) PD-FEEFINCHG(2).
           MOVE 0   TO PD-FEEAMT(1)    PD-FEEAMT(2).
       DISPLAY-FEES.
           MOVE PD-FEECODE(1) TO D-FEECD1.
           MOVE PD-FEEAMT(1)  TO D-FEEAMT1.
           MOVE PD-FEECODE(2) TO D-FEECD2.
           MOVE PD-FEEAMT(2)  TO D-FEEAMT2.
           MOVE FEE-BUF TO WR-BUF.
           MOVE FEE-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       DOES-FEE-MATCH.
           IF PD-FEECODE(1) = " " AND PD-FEECODE(2) = " "
              MOVE LTI-INSFEECD(IX1) TO PD-FEECODE(1)
              MOVE LTI-INSFEE(IX1)   TO PD-FEEAMT(1)
           ELSE
              IF LTI-INSFEECD(IX1) = PD-FEECODE(1)
                 ADD LTI-INSFEE(IX1) TO PD-FEEAMT(1)
              ELSE
                 IF LTI-INSFEECD(IX1) = PD-FEECODE(2)
                    ADD LTI-INSFEE(IX1) TO PD-FEEAMT(2)
                 ELSE
                    IF PD-FEECODE(2) = " "
                       MOVE LTI-INSFEECD(IX1) TO PD-FEECODE(2)
                       MOVE LTI-INSFEE(IX1) TO PD-FEEAMT(2).
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
           IF IO-FG = 9
              MOVE " " TO BW-FNAME
              MOVE "NOT FOUND" TO BW-LNAME.
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

       UPDATE-CONTINUE.
           MOVE 8 TO NEW-PROG-SUB.
           MOVE "P" TO ERRCD.
           MOVE "END" TO IO-TYPE.

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
 
       SEND-WSEL-LEGEND.
           MOVE MESS TO WR-BUF.
           MOVE WSEL-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      *********************************
       IO-ROUTINE SECTION.
      *********************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBLP/LPSPGS_SQL.CPY".
       COPY "LIBLP/LPPDGS_SQL.CPY".
       COPY "LIBLP/LPLTPGS_SQL.CPY".
       COPY "LIBLP/LPLTIGS_SQL.CPY".
       COPY "LIBLP/LPLTCGS_SQL.CPY".
       COPY "LIBLP/LPLPGS_SQL.CPY".
       COPY "LIBLP/LPLNGS_SQL.CPY".
       COPY "LIBLP/LPLCGS_SQL.CPY".
       COPY "LIBLP/LPDLGS_SQL.CPY".
       COPY "LIBLP/LPCDGS_SQL.CPY".
       COPY "LIBLP/LPBWGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBLP/LPDL1RN.CPY".
       COPY "LIBLP/LPLN1RN.CPY".
       COPY "LIBLP/LPLP1RN.CPY".
       COPY "LIBLP/LPLTI1RN.CPY".
       COPY "LIBLP/LPLTC1RN.CPY".
       COPY "LIBLP/LPLTP1RN.CPY".
       COPY "LIBLP/LPLC1RN.CPY".
       COPY "LIBLP/LPSP1RN.CPY".
       COPY "LIBLP/LPBW1RN.CPY".
       COPY "LIBLP/LPCD1RN.CPY".
       COPY "LIBLP/LPWKI.CPY".
       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO ERRCD.

      *********************************
       END-ROUTINE SECTION.
      *********************************
           PERFORM CLOSE-FILES.
       EXIT-PROG.
       STOP-RUN.
           EXIT PROGRAM.
