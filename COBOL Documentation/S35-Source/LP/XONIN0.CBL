       IDENTIFICATION DIVISION.
       PROGRAM-ID.      XONIN0.
       DATE-WRITTEN.    060287.
      ******************************************************************
      *                 (LP)
      *  DESCRIPTION:   DIRECT LOANS (NON SALES FINANCE)
      *                 CONVERSION - PAGE 1
      *                 MODIFIED TO USE FRONT-END CALC SCREEN
      * REV :
      *  051295 SLC ADDED INSURANCE CHECK. MUST HAVE A&H AND LIFE IF
      *             BRANCH OPTION IS ON.
      *  081095 MJD ADDED LOGIC FOR GP-THE-ONLY-SF-SRCD
      *  MJD 010996 ADDED LOGIC TO COMPUTE INSURANCE COMMISSION ONLY
      *             IF LN-INSOURS() = "Y"
      *             ADDED LOGIC TO PREVENT ENTRY OF "MN_" AS INSURANCE
      *              TYPE.
      *  JFF 011796 ADDED CODE SCAN FOR PURPOSE, COLLATERAL,
      *             AND SOURCE.
      *  BAH 012596 PASSWORDED 1STPYDATE < 30 DAYS FROM LOANDATE, E097
      *  MJD 072996 ADDED LOGIC TO PRINT LN-1STPYDATE AS MM/DDWYY
      *             FOR WEEKLY UNIT PERIODS.
      *  MJD 101596 ADDED MOVE OF LN-REC TO ORIGINAL-REC WHEN
      *             LOAN IS CALLED FROM SCAN. CHANGED TO ONLY MOVE
      *             SP-JUDGRATE TO LN-JDRATE IF LN-JDDATE = 0.
      *             ADDED F3-LOAN STATUS OPTION THAT CALL LLNSTAT.C
      *  JTG 112196 CORRECTED BUG WITH WRONG SUBSCRIPT ON COMPUTE OF COMMISSION
      *             NEW COPY MEMBERS LIBLP/LPIEXP/W
      *             (IEXP-SUB/SUB1 --> IEXP-LN/IC)
      *  KEC 112596 CHANGED LN-POCD FOR RESCISSION.
      *  JTG 120296 REMOVED F7-EXIT FROM BOTTOM TO STOP EXITING AFTER F3-STATUS
      *  MJD 060396 ADDED SCSCAN-TRANS-DATE.
      *  JTG 060397 ADDED TEST OF 'O3' RE: BR-OTH-INS-LEVELS
      *  MJD 072497 ADDED, IF BR-AUTO-LNSTAT-WINDOW = "Y"
      *             PERFORM CALL-LNSTAT-WINDOW.
      *  JTG 073197 ADDED LOGIC FOR TOTAL-FEEFINCHG AND TOTAL-INSFINCHG
      *  JTG 081497 ADDED SET OF LN-GIFTNET WORLD A90
      *  JTG 090497 REMOVED COPY "LIBWI/FEEENTW" AND REFERENCE TO FW-FLDSEL-NUM
      *  GLF 090497 REPLACED 'IO' ON GP-FILE WITH GET 'GPENV'
      *  BAH 090597 WAS NOT CALLING F3-EXPANDED COLLATERAL
      *  BAH 111897 ADDED BR-CONVERT-AUTO-ACCTNO LIKE LONIJ0
      *  MJD 112197 EXPANDED LN-REC TO 1800 BYTES
      *  BAH 012698 IF INSURED IS "S", TEST SECOND MAKER'S AGE EDITS, PR#641
      *   CS 042398 UPDATE LN-ORIGIN WITH BW-ORIGIN
      *  BAH 090198 ADDED SP-INS-ADJ-PRIN-3PMTS, NC ALSO ADDED
      *                   SP-INS-ADJ-CASH-ADVANCE
      *  KEC 090498 ADDED 'MOVE LC-LOANTYPE TO LN-LOANTYPE' REGENCY REVOLVING.
      *  JTG 101398 ADDED SET OF LCSCAN-REVOLVING = 'N' AND STOPPED ENTRY
      *             OF REVOLVING CLASSES, REGENCY REVOL
      *   CS 120798 ADD TEST OF GP-I-AM-MERC, DONT ALLOW CONVERSION OF ACCOUNT
      *             UNLESS IN P&L STATUS
      *  JTG 121698 DISPLAYED 'R' VIA UNITPER-MASK LOGIC FOR REVOLVING ACCOUNT
      *  BAH 022299 CD-FILLER NEEDED TO GET CLEARED FOR PU, CO, AND SR
      *             CD-EL-FILLER NEEDED TO GET CLEARED FOR "EL"
      *  GLF 030199 ADDED AUTOMATIC CALCULATION OF APR
      *             ADDED PASSWORD TO CHANGE APR
      *  GLF 031199 ADDED WI/TFCCDS FOR TFC ENTRY OF LPO, REP CODE, PROCESS CODE
      *  BAH 990716 ADDED NEW PASSWORD OPTION 2 (INSCO-SEQ) TO CHANGE DEFAULT
      *            INSURANCE COMPANY ON CL,AH,PP,O1, REGENCY PR# 56
      *  GLF 990805 ADDED GP-VALID-BDATE. IF "Y" AND THERE IS NOT A VALID
      *             BIRTH DATE FOR SSNO BEING ENTERED, DO NOT ALLOW ENTRY
      *             OF THE SSNO, REGENCY #57
      *  MJD 990806 ADDED GP-INCL-OTH-IN-EARNED FLAG. IF "Y" THEN INCLUDE
      *             EARNINGS BUCKET 6 (OTHER) IN ACCUMULATED TOTALS
      *             OTHERWISE EXCLUDE BUCKET 6.
      *   CS 991013 ADDED TEST OF BULK-CONVERSION0FG TO DETERMINE IF NORMAL
      *             CONVERSION OR BULK PURCHASE, IF CONVERSION UPDATE
      *             LN-CONVERCD WITH 'Y' IF BULK UPDATE 'B' AND DONT ALLOW
      *             CHANGES IF BULK-CONVERSION-FG DOESNT MAP TO LN-CONVERCD
      *  BLV 991013 FIX ALLOTMENT QUESTION - WAS MOVING QUESTION TO MESS &
      *             DOING SEND-LEGEND SO SEEMED LIKE YOU WERE STUCK, FIXED
      *             GO MOVE QUESTION TO LEGEND
      *  CS  991130 REMOVED TEST OF LN-REPODATE AND LN-PLDATE -ADDED TO LONIF0
      *  JTG 000113 ADDED SP-INSREJECT-FG OPTION 'C', WHICH WILL CAUSE WRITING
      *             NO. OF MONTHS INSURANCE NOT IN VIOLATION, WISCONSIN #1032
      * MJD 000207 ADDED GP-OT-INS-TERM (TFC PR#48) IF 'Y' THEN ALLOW
      *            ALLOW TERM ENTRY FOR O2O3O4O5 INSURANCE.
      * BLV 000314 CHECK FOR LC-CALC-HI-CREDIT = W, WORLD PR# 225
      * BAH 000329 COMPUTE-INS-TERM-I-LEVEL HAD WRONG SP SUBSCRIPT FOR O1
      *            INSURANCE, EXCEL ANDRIA
      * KEC 000530 {PR#00048} TFC [JUDY BRANIFF];
      *            ADDED THE FOLLOWING 'IF' CONDITION FOR ENTERING THE
      *            INSURANCE TERM (GP-OT-INS-TERM = "Y"):
      *                  IF ( VDUNUM0 > 360 )
      *                     GO TO ENTER-IC-TERM.
      *  JTG 000614 CORRECTED CONVERSION PROBLEM WITH INCORRECT AMOUNTS FOR
      *             LN-ANTICERN, LN-ANTICADJ & LN-ACCERN WHEN REFUNDS ARE
      *             SET VIA WI/LNSTAT.C                             #1203
      *  MJD 000912 ADDED LOGIC TO ASK LOCK BOX QUESTION IF ALLOTMENTS = "N"
      *             AND BR-LBOX = "Y".  (MFC PR#227)
      *  JTG 010112 ADDED NEW 88 SP-CAL-SET-SMPRATE-OVRRIDE VALUE 4 MERC PR#J016
      *  BAH 010222 WAS TESTING ELE NOT = 18 (NON FRONT END APR FIELD) TO NOT
      *             RECALCULATE APR, SHOULD BE NOT = 23 !! LORI, HUMBOLT
      *  CS  010320 SET LC-POOLID TO LN-DLPOOL.
      *  CS  010327 ADDED SET OF DEFAULT-APR-FG TO "Y" IN NEW RECORD SETUP.
      *             IT WAS SET TO 'N' IN COMPUTE-PROCEEDS AND NOT RESET TO "Y"
      *             UNLESS MODIFYING LOAN.
      *  CS  010327 LOGIC FOR FEE CODES AND INSURANCE CHARGES TO BE INCLUDED IN
      *             FINANCE CHARGE DID NOT WORK.
      *  JTG 010329 CORRECTED SET OF LN-ANTICERN() AND LN-ACCERN() WHEN
      *             ENTRY OF REBATE FLAG AND REFUND AMOUNTS WERE ENTERED
      *             RE: LN-ANTICERN IS ALWAYS REDUCED BY REFUNDED AMOUNT
      *                 FROM IT'S ORIGINAL VALUE          WORLD #258
      *  CS  010719 CORRECTED INSFINCHG FOR PROPERTY INSURANCE
      *  BLV 010810 ADDED CONDITIONAL-SEND-MESS FOR CHANGE TO LPSPEDL,
      *             MERC PR# 253
      *
      * KEC 020624 {PR#01823} REGIONAL ACCEPTANCE <A15> [KEVIN MULLEN]
      *            ADDED LOGIC FOR USING 'CD-GC-FEE-MAXAMT'
      *
      *  BLV 020702 FIX BAD IF STATEMENT:
      *           IF ( ELE = 33 OR 34 AND LN-INT-REBATE NOT = " " )
      *      THIS GAVE AN ERROR EVERY TIME SOMEBODY TRIED TO CHANGE
      *      FIELD 33, CHANGED TO
      *           IF ( ELE = 33 AND LN-INT-REBATE NOT = " " ) OR
      *              ( ELE = 34 AND LN-INT-REBATE NOT = " " )
      *  BLV 020820 IF GP-ENTER-LPO-FG = "Y", OPEN TFC WINDOW FOR ENTRY OF
      *             LPO, SOAUTO PR# 1885
      *  BAH 031001 ADDED SP-INSCHGABLE '2' & '1',SP-INS-AMTFIN-2PMTS &
      *             SP-INS-AMTFIN-1PMTS FOR SIGNAL (WISC) PR# 2157
      *   CS 041109 CHECK FOR GI WHEN CLASS IS ENTERED, IF THE GI DOES NOT
      *             EXIST, DONT ALLOW TO PROCEED [SUPPORT PR#451]
      *  BAH 050222 WHEN ENTERRING SUB CLASS, YOU DONT KNOW THE LOANDATE YET TO
      *             CHECK IF SPR IS EXPIRED, SO IT WAS JUST GRABBING THE FIRST
      *             LAWCODE FOUND TO DISPLAY THE DESCRIPTION, WHICH ISNT
      *             CORRECT IF THEY EXPIRE AND BUILD A NEW ONE WITH A DIFFERENT
      *             DESCRIPTION, VERY CONFUSING...AT LEAST SKIP EXPIRED SPR.
      *             RIVERSIDE PL# 467
      *  CS 050425 IF INSURANCE PREMIUM = ZERO, ZERO THE INSCOMP,INSCOMM,
      *            EFFDATE,EXPDATE, AND OUR FLAG... PREVIOUSLY WE WERE
      *            COMPUTING A COMMISSION [PR#479 NSA]
      * BAH 051128 UNDO CHANGE FROM 050222 PL# 467, WOULD NOT LET YOU BOOK A
      *            LOAN WITH AN EXPIRED SPR, AND YOU SHOULD BE ABLE TO.
      *            INSTEAD OF SKIPPING EXPIRED SPR IN READ-NEXT, JUST REDISPLAY
      *            THE SUBCLASS DESCRIPTION AFTER ENTERING THE LOANDATE.
      *  KEC 060721 PR#00127 REGIONAL ACCEPT. ADDED LOGIC FOR "AUTOPC".
      *  MJD 061011 CORRECTED BAD LOGIC IN THE "COMPUTE-ANTICIPATED"
      *             SECTION FIRST "IF" STATEMENT INCORRECTLY TESTED
      *             LN-ANTICERN(4) INSTEAD OF LN-ANTICADJ(4) WORLD PR#499
      *   CS 080611 JR CALLED. WORLD CONVERTING ON IB LOANS. WE ARE NOT
      *             COMPUTING SAME EFF RATE AS ORIG CO... SO JIM IS HAVING
      *             WORLD SET RATE TO OVERRIDE RATE, FORCE THEM TO ENTER
      *             AN OVERRIDE RATE OTHER THAN 999.999. VERY SPECIFIC,
      *             ST MUST BE AL, CL=21, SUBCLASS= 2, PR# 588
      * BAH 090821 ADDED SP-INSCHGABLE "E" FOR REGMGM PR# 3437
      * BAH 100902 DO NOT ALLOW BACK UP TO SUBCLASS, FIELD 5, IF YOU ENTER A
      *            DIFFERENT SUBCLASS WITHOUT ENTERING LOANDATE AGAIN, YOU WILL
      *            NOT GET THE CORRECT SPR BASED UPON LAWCODE AND LOANDATE,
      *            KEEPS THE LAWCODE FROM THE FIRST SUBCLASS YOU ENTERED !
      *                                             WORLD PL# 702
      * BAH 110523 PASSWORD PROTECT ADDON SOURCE CODES, ET, LP, NP, RT, ST
      *            AND RB, REGMGM PR# 3792
      *  CS 119727 CHANGED VERIFY-PAYMENTS TO SUBTRACT MAINTEE & INT IF PRECOMPT      *            TO TEST AMOUNT FINANCE, LOANS WERE UPDATED WITH NEGATIVE
      *            AMOUNT FINANCED  WORLD PL#743
      * BLM 120222 IF GP-PURGE-TO-ARCHIVE = "Y", CHECK TO SEE IF ACCT #
      *            ALREADY EXISTS IN ARCHIVED LOAN FILE; IF SO, REJECT OR
      *            SKIP TO NEXT NUMBER, REGACC PR# 304
      * BAH 120905 ADDED LT-VIN-CHG-DATE FOR REGACC PR# 319
      * BAH 130923 ALLOW GP-I-AM-WDT TO USE LT-SEQ 5 FOR A 3RD HHGS PR#4095
      *  CS 150814 IF GP-I-AM-EXCEL, TEST THE FEECODE AMOUNT AGAINST CD-
      *            BRANCH-COMM-AMOUNT  EXCEL PR#4821
      *  CS 150922 ADD TEST ON BRANCH-COMM-AMOUNT FOR 202020/020202 PR#4821
      *   CS 151130 MOVE LN-OWNBR TO LP-BRNO, BRNO NOW PART OF LP1-KEY
      *   CS 151204 REMOVED LP-BRNO FROM KEY
      * BAH 160114 SPLIT OUT LTFILE, PD0003
      *
      * KEC 2016.0309 {PD#00003} PARADATA <A30> [JAY CISZEWSKI]
      *         CHANGED CD-GROUP, CD-SECURED & CD-TRW TO BE
      *         CD-CO-GROUP, CD-CO-SECURED & CD-CO-TRW.
      * KEC 2016.0314 {PD#00003} PARADATA <A30> [JAY CISZEWSKI]
      *         CHANGED CD-FEE???? TO CD-GC-????.
      * KEC 2016.0325 {PD#00003} PARADATA <A30> [JAY CISZEWSKI]
      *         CHANGED CDSCAN-WINDOW-BUF TO HAVE MATCHING CD1-KEY
      *         LAYOUTS (FOR EACH CODE TYPE; CD-TYPE).
      *         CDSCAN-CD1-KEY IS THE NEW (PRIMARY) WITH REDEFINES OF
      *         CDSCAN-??-KEY (NEW).
      * KEC 2016.0329 {PD#00003} PARADATA <A30> [JAY CISZEWSKI]
      *         CHANGED ELSCAN WORKERS TO MATCH CD1-KEY FOR EL LAYOUT.
      *  CS 160401 WHEN DOING A PL ON ACCOUNT, USER GOT THE MESSAGE "MUST
      *            REDUCE PURBAL TO 0, IF THERE IS A PURCHASE BALANCE
      *            NOT = TO LN-ORIGPURAMT, CANNOT ALLOW THEM TO REWRITE THE
      *            RECORD BY CALLING UP THE LOAN & U TO UPDATE
      *            (IN COMPUTE-PRINCIPLE) WORLD PR# 1147  
      *             THIS HAS BEEN HAPPENING AS LONG AS I CAN REMEMBER AND WE
      *            WERE NEVER ABLE TO IDENTIFY WHAT CAUSEED IT, THANKS KM
      *            IN COMPUTE-PRINCIPLE, ONLY SET UP LN-PURBAL IF LN-CURBAL
      *            IS 0   WORLD PR1147 
      *   160407  ADDED CHECK WHEN ACTION "U", IF CURBAL NOT = 0
      *           OR ACCT PAID OFF, DONT ALLOW UPDATE
      * KEC 2016.0513 {PD#00003} PARADATA <A30> [JAY CISZEWSKI]
      *         CHANGED USE OF APPNO TO APP-NUMBER FOR MA1-KEY USAGE.
      *  CS 160805 CHANGED SCSCAN-KEY TO SCSCAN-SP1-KEY
      * BAH 160811 ADDED PASSWORD OPT 25 TO PROTECT 3RD AND 4TH BORROWERS
      *            COSIGNER-3-AND-4-SEQ, WORLD PR#1175
      * MJD 160823 ADDED LC-CALC-HI-CREDIT = "L"              61000/1180
      * BLM 161227 CHANGED ORIG & CHANGED RECS FROM 1800 TO LN-SIZE
      *
      * KEC 2017.0526 {PR#00000} PARADATA <A30> [JAY CISZEWSKI]
      *          ADDED 'LN' FILE PREFIX FOR WORKING STORAGE FIELDS.
      *  CS 170606 ACTIVATE 8 DIGIT DATES
      * BAH 171204 UPDATE LN-GIFTNET WITH "X" IF SP-NON-LOAN-FLAG = "Y"
      *            FOR WORLD TAX ADVANCE LAONS TO NOT BE CONSIDERED DUAL
      *            LOANS, AND IF SP-SKIP-TRW = "Y", SET LN-SKIP-TRW TO "Y"
      *                                            WORLD PR#1288
      * KEC 180827 LOAN APP MODULE REMOVAL FROM A30 [LARM30]
      * BAH 181002 GLOBAL BWFILE
      * BAH 181030 GLOBAL GBFILE
      *  CS 181206 GLOBAL LTCFILE
      *  CS 181206 GLOBAL LTAFILE
      * BAH 190103 GLOBAL LPFILE
      * BAH 190124 SPLIT CDFILE
      * BAH 20190625 REMOVED LC-REVOLVING, PT#0153
      * BAH 20190625 REMOVED LN-REVOLVING, PT0153
      * BAH 20200617 CHANGED PC-LT-RECORD TO PC-LTA-RECORD, ADDED LPPCCL
      * BAH 20200615 MOVED OPEN-IC1-FILE, OPEN-SP1-FILE, OPEN-CD1-FILE,
      *              OPEN-BW1-FILE TO TOP OF PROGRAM #TST001
      * BAH 20210713 PASSWORD FIELD 29, OTHER 2 INSURANCE #1504
      * BAH 20220803 HARD CODED START
      * BAH 20230623 OF THE 4 AUTO COLLATERAL CODES THAT WORLD USES, ONLY "AU"
      *              AND "WA" ARE ALLOWED IN "IL". EXCEPT CLASS 8 CAN HAVE ANY.
      *              CLASS 5, SUB 10 MUST HAVE "AU" OR "WA" ONLY #1614
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

      ********************************************************************
      *   ARCHIVED LOAN MASTER FILE
      ********************************************************************
      *FD  ARCH-LN-FILE
      *    LABEL RECORDS ARE STANDARD.
      *COPY "LIBLP/LP01LN.CPY" REPLACING LEADING "LN" BY "ARCH-LN"
      *                              TRAILING "STFG" BY "ARCH-LN-STFG".

       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(37) VALUE
           "@(#)LP/XONIN0 S35 05/02/24-15:15:27 >".
      **********************************
      *       F I L E   I F N          *
      **********************************
       COPY "LIBGB/GB01GB.CPY".
       COPY "LIBGB/GB01GB_SQL.CPY".
       COPY "LIBGB/GBWSGB.CPY".
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
       01  WK-KEY          PIC 9(2).

      *********************************************************
      *   DATA PATH FROM GETENV ON "FIL"
      *********************************************************
      *01  DATA-PATH                           PIC X(35).
      *01  DATA-PATH-FIL-BUFFER      REDEFINES DATA-PATH.
      *    03  DATA-PATH-DIR1                  PIC X(3).
      *    03  FILLER                          PIC X(32).
      *********************************************************
      *   LNFILE ARCHIVE PATH WORKERS
      *********************************************************
      *01  LN-ARCHIVE-PATH.
      *    03  FILLER                     PIC X     VALUE "/".
      *    03  LN-ARCHIVE-PATH1.
      *        05  LN-ARCHIVE-DIR1        PIC X(3).
      *        05  FILLER                 PIC X     VALUE "/".
      *        05  LN-ARCHIVE-DIRECTORY   PIC X(4).
      *        05  FILLER                 PIC X     VALUE "/".
      *        05  LN-ARCHIVE-MACHINE     PIC XX.
      *        05  FILLER                 PIC X     VALUE "/".
      *        05  LN-ARCHIVE-DATA        PIC X(6).
      *    03  FILLER                     PIC X(10) VALUE
      *        "/LP/LNFILE".

       01  ONE             PIC S9.
       01  ELE             PIC 9(6).
       01  ACTION.
           03  CHKEY       PIC X(4).
           03  CHNO        PIC 9(2).
       01  IO-TYPE         PIC X(3).
       01  NEW-FLAG        PIC 9       VALUE 0.
       01  FIRST-ELE       PIC 99      VALUE 0.
       01  F3-COLLATERAL   PIC X        VALUE " ".
           88  F3-YES                   VALUE "Y".
       01  TEST-COLLCD     PIC XX.
           88  AUTO-COLLCD VALUE "AU" "AC" "AL" "RP" "HP" "AW"
                                      "MU" "MV" "AP" "WA" "WB".
       01  HOLD-LN1-KEY    PIC 9(12).
       01  ORIGINAL-LN-REC PIC X(LN-SIZE).
       01  CHANGED-LN-REC  PIC X(LN-SIZE).
       01  LAST-READ       PIC X(4).
       01  PREM-CHANGE     PIC X VALUE " ".
       01  PREM-AMOUNT     PIC 9(4)V99   COMP-3 VALUE 0.
       01  VALID-DELETE    PIC X.
       01  SET-NUMBER      PIC 9(8).
       01  CDCODE-FEE-FG   PIC 9.
       01  ANTIC           PIC S9(6)V99  COMP-3.

       01  SUB             PIC 9(6) COMP-3.
       01  SP-SUB          PIC 9(6) COMP-3.
       01  LN-SUB          PIC 9(6) COMP-3.
       01  OVER-20         PIC 99.
       01  I-LEVEL         PIC 9(6) COMP-3.
       01  INSPECT-W       PIC X(86).
       01  INSPECT-X REDEFINES INSPECT-W.
           03  INSPECT-Z   PIC X OCCURS 86 TIMES.
       01  ADD-ON          PIC X(25).
       01  NON-MEMO-INS    PIC 9(6)V99 COMP-3 VALUE 0.
       01  DEFAULT-APR-FG  PIC X.

       01  SCAN              PIC X(2)  VALUE SPACES.
           88  SCAN-ON       VALUE "ON".
       01  ALLOT-BUF.
           03  FILLER        PIC X(5).
           03  ALLOT-CODE    PIC X.

       01  LBOX-BUF.
           03  FILLER        PIC X(5).
           03  LBOX-CODE     PIC X.

       01  LC-INFO-LIST      PIC X(10)   VALUE "39P:S 390.".
       01  LC-INFO-BUF       PIC X(14).

       01  LEGEND-LIST       PIC X(8)   VALUE "MSEL.".
       01  LEGEND            PIC X(70).
       01  ACCTNO-WAS-SET-FG          PIC X   VALUE " ".
       01  MODIFY-LEGEND.
           03  FILLER        PIC X(32)  VALUE
           "F1, F4-MEMO, F5-SCHED, F6-ADDON,".
           03  FILLER        PIC X(45)  VALUE
           " F3-LOAN STATUS, P2, P3, DELETE, FLD#, U-UPD:".
       01  WI-MODIFY-LEGEND.
           03  FILLER        PIC X(27)  VALUE
           "F1, F4-MEMO, F3-LOAN STATUS".
           03  FILLER        PIC X(30)  VALUE
           ", P2, P3, DELETE, FLD#, U-UPD:".

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


      ********************************
      *   S C R E E N   B U F F E R  *
      ********************************
       01  DISPLAY-BUF1        VALUE " ".
           03  D-LNNO          PIC 9(6).
           03  FILLER          PIC ZZ.
           03  D-P             PIC XX.
           03  D-PTYPE         PIC XX.
           03  D-J             PIC X(3).
           03  D-LCNO          PIC 99.
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
           03  D-061           PIC 99/99/99  BLANK ZERO.
           03  D-070           PIC X(8).
           03  D-071           PIC ZZZ.
           03  D-080           PIC X(3).
           03  D-081           PIC X(15).
           03  D-090           PIC X(2).
           03  D-091           PIC X(15).
           03  D-100           PIC X(2).
           03  D-101           PIC X(15).
           03  D-110           PIC 9(6)      BLANK ZERO.
           03  FILLER          PIC X(9).
           03  D-120           PIC Z(5)9.99  BLANK ZERO.
           03  D-130           PIC X(3).
           03  FILLER          PIC X(5).
           03  D-140-180       OCCURS 5.
               05  D-1FEECD    PIC XX.
               05  D-1FEEAMT   PIC Z(3)9.99  BLANK ZERO.
               05  D-2FEECD    PIC XX.
               05  D-2FEEAMT   PIC Z(3)9.99  BLANK ZERO.
           03  D-190           PIC 9(4).99   BLANK ZERO.
           03  D-191           PIC X(16).

       01  DISPLAY-BUF2        REDEFINES DISPLAY-BUF1.
           03  D-390           PIC ZZ.
           03  D-200           PIC X(20).
           03  D-201           PIC X(20).
           03  D-202           PIC X(20).
           03  D-210           PIC ZZ9.999   BLANK ZERO.
           03  D-220           PIC ZZ9.999   BLANK ZERO.
           03  D-230           PIC ZZ9.999   BLANK ZERO.
           03  D-240           PIC ZZ9.999   BLANK ZERO.
           03  D-LOANAMT       PIC Z(5)9.99  BLANK ZERO.
           03  D-ANTIC         PIC Z(5)9.99  BLANK ZERO.
           03  D-250-280       OCCURS 4.
               05  D-1COMP      PIC 99        BLANK ZERO.
               05  D-1OUR       PIC X.
               05  D-1TERM      PIC ZZ9       BLANK ZERO.
               05  D-1COVR      PIC Z(5)9.99  BLANK ZERO.
               05  D-1PREM      PIC Z(3)9.99  BLANK ZERO.
           03  D-290-300       OCCURS 2.
               05  D-2COMP      PIC 99        BLANK ZERO.
               05  D-2OUR       PIC X.
               05  D-2EXP       PIC 9(6)      BLANK ZERO.
               05  D-2TERM REDEFINES D-2EXP
                                PIC Z(6)      BLANK ZERO.
               05  D-2COVR      PIC Z(5)9.99  BLANK ZERO.
               05  D-2PREM      PIC Z(3)9.99  BLANK ZERO.
           03  D-310           PIC Z(5)9.99  BLANK ZERO.
           03  D-320           PIC Z(5)9.99  BLANK ZERO.
           03  D-330           PIC Z(5)9.99  BLANK ZERO.
           03  D-340           PIC Z(5)9.99  BLANK ZERO.
           03  D-350           PIC Z(5)9.99  BLANK ZERO.
           03  D-360           PIC Z(5)9.99  BLANK ZERO.
           03  D-370           PIC Z(5)9.99  BLANK ZERO.
           03  D-380           PIC Z(5)9.99  BLANK ZERO.
       01  DEF-BUF REDEFINES DISPLAY-BUF1.
           03  DEF-ORGST       PIC XX.
           03  DEF-ORGBR       PIC 9(4).

       01  DEF-LIST            PIC X(8) VALUE
           "050 051.".

       01  DISPLAY-LIST1.
           03  FILLER          PIC X(26)  VALUE
           "LNNO LNSQ PJ LNKEY LCDESC ".
           03  FILLER          PIC X(40)  VALUE
           "010 011 012 020 021 022 030 031 032 040 ".
           03  FILLER          PIC X(52)  VALUE
           "041 042 050 051 052 053 060 061 070 071 080 081 090 ".
           03  FILLER          PIC X(8)   VALUE
           "091 100 ".
           03  FILLER          PIC X(56)  VALUE
           "101 110 111 120 130 131 140 141 142 143 150 151 152 153 ".
           03  FILLER          PIC X(56)  VALUE
           "160 161 162 163 170 171 172 173 180 181 182 183 190 191.".
       01  DISPLAY-LIST2.
           03  FILLER          PIC X(4)  VALUE
           "390 ".
           03  FILLER          PIC X(58)  VALUE
           "200 201 202 210 220 230 240 LOANAMT ANTIC 250 251 252 253 ".
           03  FILLER          PIC X(56)  VALUE
           "254 260 261 262 263 264 270 271 272 273 274 280 281 282 ".
           03  FILLER          PIC X(56)  VALUE
           "283 284 290 291 292 293 294 300 301 302 303 304 310 320 ".
           03  FILLER          PIC X(24)  VALUE
           "330 340 350 360 370 380.".
       01  DISPLAY-LIST3.
           03  FILLER          PIC X(32) VALUE
           "010 011 020 021 030 031 040 041.".

      ********************************
      *   E L E M E N T   S P E C S  *
      ********************************
       01  MAX             PIC 99   VALUE 39.
       01  SPEC-TABLE.
      * 1  LN-SSNO(1)
           03  FILLER      PIC X(8) VALUE "ERNX110 ".
      * 2  LN-SSNO(2)
           03  FILLER      PIC X(8) VALUE "ERNX110 ".
      * 3  LN-SSNO(3)
           03  FILLER      PIC X(8) VALUE "ERNX110 ".
      * 4  LN-SSNO(4)
           03  FILLER      PIC X(8) VALUE "ERNX110 ".
      * 5  LN-ORGST
           03  FILLER      PIC X(8) VALUE "E  A020 ".
      * 6  LN-LOANDATE
           03  FILLER      PIC X(8) VALUE "ERNM060 ".
      * 7  LN-1STPYDATE
           03  FILLER      PIC X(8) VALUE "ERNM060 ".
      * 8  LN-PURCD
           03  FILLER      PIC X(8) VALUE "E  A030 ".
      * 9  LN-COLLCD
           03  FILLER      PIC X(8) VALUE "E  A030 ".
      * 10 LN-SRCD
           03  FILLER      PIC X(8) VALUE "E  A030 ".
      * 11 LN-PRLNNO
           03  FILLER      PIC X(8) VALUE "ENNN060 ".
      * 12 LN-PROCEEDS
           03  FILLER      PIC X(8) VALUE "ENNN062N".
      * 13 LN-INSURED
           03  FILLER      PIC X(8) VALUE "E  A030 ".
      * 14  LN-FEECODE(1), (2)
           03  FILLER      PIC X(8) VALUE "E  A020 ".
      * 15  LN-FEECODE(3), (4)
           03  FILLER      PIC X(8) VALUE "E  A020 ".
      * 16  LN-FEECODE(5), (6)
           03  FILLER      PIC X(8) VALUE "E  A020 ".
      * 17  LN-FEECODE(7), (8)
           03  FILLER      PIC X(8) VALUE "E  A020 ".
      * 18  LN-FEECODE(9), (10)
           03  FILLER      PIC X(8) VALUE "E  A020 ".
      * 19 LN-DLNO
           03  FILLER      PIC X(8) VALUE "ENNN042 ".
      * 20 LN-COLLID
           03  FILLER      PIC X(8) VALUE "E  A200 ".
      * 21 LN-OVRDRATE
           03  FILLER      PIC X(8) VALUE "ENNN033 ".
      * 22 LN-EFFRATE
           03  FILLER      PIC X(8) VALUE "ENNN033N".
      * 23 LN-APRATE
           03  FILLER      PIC X(8) VALUE "ENNN033N".
      * 24 LN-ENDORATE
           03  FILLER      PIC X(8) VALUE "ENNN033 ".
      * 25 LN-INSCOMP(1)
           03  FILLER      PIC X(8) VALUE "ENNN020 ".
      * 26 LN-INSCOMP(2)
           03  FILLER      PIC X(8) VALUE "ENNN020 ".
      * 27 LN-INSCOMP(3)
           03  FILLER      PIC X(8) VALUE "ENNN020 ".
      * 28 LN-INSCOMP(4)
           03  FILLER      PIC X(8) VALUE "ENNN020 ".
      * 29 LN-INSCOMP(5)
           03  FILLER      PIC X(8) VALUE "ENNN020 ".
      * 30 LN-INSCOMP(6)
           03  FILLER      PIC X(8) VALUE "ENNN020 ".
      * 31 LN-SERCHG
           03  FILLER      PIC X(8) VALUE "ENNN062 ".
      * 32 LN-MAINTFEE
           03  FILLER      PIC X(8) VALUE "ENNN062 ".
      * 33 LN-EXTCHG
           03  FILLER      PIC X(8) VALUE "ENNN062 ".
      * 34 LN-INTCHG
           03  FILLER      PIC X(8) VALUE "ENNN062 ".
      * 35 LN-FINAMT
           03  FILLER      PIC X(8) VALUE "ENNN062N".
      * 36 TOTAL REPAYABLE
           03  FILLER      PIC X(8) VALUE "ENNN062N".
      * 37 LN-1STPYAMT
           03  FILLER      PIC X(8) VALUE "ENNN062 ".
      * 38 LN-REGPYAMT
           03  FILLER      PIC X(8) VALUE "ENNN062 ".
      * 39 LN-LC-CODE
           03  FILLER      PIC X(8) VALUE "ENNN020 ".
       01  SPEC-TABLE2     REDEFINES SPEC-TABLE.
           03  SPEC-REC    OCCURS 39 TIMES.
               05  SPEC    PIC X(7).
               05  CHFG    PIC X.
       01  AMT-HOLDER                 PIC 9(6)V99  COMP-3.
       01  SCREEN-LINES.
           03  SCREEN-LINE1           PIC X(20).
           03  SCREEN-LINE2           PIC X(20).
           03  SCREEN-LINE3           PIC X(20).
       01  DATE-FG                    PIC X.

       COPY "LIBWI/PAYSCHW.CPY".
       COPY "LIBWI/ELSCANW.CPY".
       COPY "LIBWI/BWLNSCW.CPY".
       COPY "LIBWI/LCSCANW.CPY".
       COPY "LIBWI/SCSCANW.CPY".
       COPY "LIBWI/BWCREAW.CPY".
       COPY "LIBWI/BWSCANW.CPY".
       COPY "LIBWI/ADONMNW.CPY".
       COPY "LIBWI/CDSCANW.CPY".
       01  WINDOW-WORKERS.
           03  WINDOW-LPO          PIC X(4).
           03  WINDOW-REP          PIC X(8).
           03  WINDOW-PROCESS      PIC X(1).
       COPY "LIBWI/CDBSCNW.CPY".
       COPY "LIBLP/LPIEXPW.CPY".
       COPY "LIBLP/LPWKPATH.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBCL/CREAIXW.CPY".
       COPY "LIBWI/LTCREAW.CPY".
       COPY "LIBWI/AUTOPCW.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBGB/GPENVW.CPY".
       COPY "LIBGB/SYSTEMW.CPY".
       COPY "LIBLP/LPANTIW.CPY".
       COPY "LIBLP/LPITRMW.CPY".
       COPY "LIBLP/LPMNUIW.CPY".
       COPY "LIBLP/LPAMTSW.CPY".
       COPY "LIBLP/LPSPEDW.CPY".
       COPY "LIBLP/LPBAGEW.CPY".
       COPY "LIBLP/LPAPRSW.CPY".
       COPY "LIBLP/LPAPRZW.CPY".
       COPY "LIBLP/SETCTBLW.CPY".
       COPY "LIBLP/LPUPERW.CPY".
       COPY "LIBLP/LPEDAYW.CPY".
       COPY "LIBGB/PASSWDW.CPY".
       COPY "LIBGB/GETENVW.CPY".
       01  1STPY30-SEQ          PIC 9 VALUE 0.
       01  OVERRIDE-APR         PIC 9 VALUE 1.
       01  INSCO-SEQ            PIC 9 VALUE 2.
       01  SRCD-SEQ             PIC 99 VALUE 20.
       01  COSIGNER-3-AND-4-SEQ PIC 99 VALUE 25.
       01  O2-SEQ               PIC 99 VALUE 27.

       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBLP/XONIN_DEF.CPY".
       COPY "LIBLP/LPCMNU_DEF.CPY".
       COPY "LIBWI/FEEENT_DEF.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBLP/XONIN_WKS.CPY".
       COPY "LIBLP/LPCMNU_WKS.CPY".
       COPY "LIBWI/FEEENT_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".

       COPY "LIBGB/GETFMW.CPY".
       COPY "LIBLP/LPLONIW.CPY".

       SCREEN SECTION.
       COPY "LIBLP/XONIN_SCN.CPY".
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
           PERFORM CLOSE-LTC1-FILE.
           PERFORM CLOSE-LTA1-FILE.
           PERFORM CLOSE-LP1-FILE.
           PERFORM CLOSE-LN1-FILE.
           PERFORM CLOSE-LC1-FILE.
           PERFORM CLOSE-IC1-FILE.
           PERFORM CLOSE-DL1-FILE.
           PERFORM CLOSE-CDB1-FILE.
           PERFORM CLOSE-CD1-FILE.
           PERFORM CLOSE-BW1-FILE.
           PERFORM CLOSE-GI1-FILE.
           PERFORM CLOSE-GB-FILE.
           PERFORM CLOSE-BR-FILE.
           CLOSE WK-FILE.
       COPY "LIBGB/DECLAR2.CPY".

      ***********************************
      *   C O N T R O L   M O D U L E   *
      ***********************************
       MAIN-PROGRAM SECTION.

           PERFORM SQL-CONNECT.

           IF EXT-ROUTE-BUF NOT = "00"                                          
              MOVE "PROGRAM NOT BATCHABLE" TO MESS                              
              PERFORM SEND-MESS        
              MOVE "99" TO EXT-ROUTE-BUF                                        
              GO TO END-ROUTINE.

      D    STOP MESS.
           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "XONIN" TO VDU-FORM-NAME.

           MOVE " " TO ERRCD.
           MOVE USER-PID TO TEMP-PID.

           MOVE BRANCH TO BR-NO.
           PERFORM OPEN-BR-FILE.
           PERFORM READ-BR-FILE.
           PERFORM CLOSE-BR-FILE.
           IF IO-FG = 9
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
              MOVE " " TO CHFG(39)
              MOVE "39. LC CODE:__" TO LC-INFO-BUF
           ELSE
              MOVE "N" TO CHFG(39)
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
           MOVE SPACES TO IO-TYPE ACCTNO-WAS-SET-FG.
           IF LOAN-KEY NOT = 0
              MOVE LOAN-KEY TO LN-ACCTNO
              MOVE 0 TO LOAN-KEY
              MOVE "SN N060LNNO." TO VDU
              MOVE LN-ACCTNO TO VDUNUM0
              PERFORM SCREENIO
              GO TO START-LN-LOOKUP.
           MOVE "F7-MENU, F6-SCAN, ENTER LOAN NO:" TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "EN N060LNNO." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>"
              GO TO MENU-KEY.
           IF VDUSTATUS = "1<"
              PERFORM BWLN-SCAN
              IF IO-FG NOT = 0
                 MOVE 0 TO LOAN-KEY
                 GO TO LOAN-GET-NUMBER
              ELSE
                 GO TO START-LN-LOOKUP.
           IF NOT (VDUSTATUS = "00" OR "16")
              GO TO LOAN-GET-NUMBER.
           IF VDUNUM0 = 0
              IF BR-CONVERT-AUTO-ACCTNO = "Y"
                 PERFORM GET-ACCOUNT-NUMBER
                 MOVE LN-ACCTNO TO VDUNUM0
                 MOVE "00" TO VDUSTATUS
                 PERFORM ALARM
                 PERFORM ALARM
              ELSE
                 GO TO LOAN-GET-NUMBER.

           MOVE BRANCH  TO LN-OWNBR.
           MOVE VDUNUM0 TO LN-ACCTNO.
       START-LN-LOOKUP.
           MOVE LN1-KEY TO HOLD-LN1-KEY.
           PERFORM OPEN-LN1-FILE.
           PERFORM READ-LN1-FILE.
           PERFORM CLOSE-LN1-FILE.

      *    IF IO-FG = 9 AND GP-PURGE-TO-ARCHIVE-FG = "Y"
      *       MOVE LN-OWNBR  TO ARCH-LN-OWNBR
      *       MOVE LN-ACCTNO TO ARCH-LN-ACCTNO
      *       PERFORM OPEN-ARCH-LN1-FILE
      *       PERFORM READ-ARCH-LN1-FILE
      *       PERFORM CLOSE-ARCH-LN1-FILE
      *       IF IO-GOOD
      *          MOVE "ACCOUNT NUMBER EXISTS IN ARCHIVE" TO MESS
      *          PERFORM SEND-MESS
      *          GO TO LOAN-GET-NUMBER.

           IF IO-FG = 9
              IF BR-CONVERT-AUTO-ACCTNO = "Y" AND
                  ACCTNO-WAS-SET-FG NOT = "Y"
                 GO TO LOAN-GET-NUMBER
              ELSE
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
           MOVE "F1, ^, F6-CLASS SCAN, ENTER LOAN CLASS;" TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "EN N020LNKEY." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO EXIT-ENTER-KEYS.
           IF VDUSTATUS = "1>"
              GO TO MENU-KEY.
           IF VDUSTATUS = "1<"
              PERFORM CLASS-SCAN
              IF IO-FG NOT = 0
                 GO TO ENTER-CLASS.
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
           IF ( LC-SALEFIN = "Y"   )
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
           DESTROY XONIN-SCREEN.
           PERFORM MENU-SELECTION.
           IF MENU-FG = 99
              GO TO LOAN-GET-NUMBER.
           MOVE "END" TO IO-TYPE.
           GO TO EXIT-ENTER-KEYS.

       CHECK-SALEFIN.
           IF ( LC-SALEFIN = "Y"   ) 
              PERFORM NOT-DIRECT
              MOVE "CLR" TO IO-TYPE.

       EXIT-ENTER-KEYS.


      *********************************************
       ADDON-CREATE SECTION.
      *********************************************
           MOVE BRANCH      TO ADONMN-BRANCH.
           MOVE LN-ACCTNO   TO ADONMN-ACCTNO.
           MOVE LN-PLCD     TO ADONMN-PLCD.
           MOVE LN-SERVACCT TO ADONMN-SERVACCT.
           MOVE LN-SALEFIN  TO ADONMN-SALEFIN.
           MOVE LN-LOANTYPE TO ADONMN-LOANTYPE.
           MOVE LN-LOANDATE TO ADONMN-LOANDATE.
           MOVE LN-ORIG-1STPYDATE TO ADONMN-ORIG-1STPYDATE.
           MOVE NEW-FLAG          TO ADONMN-NEW-FLAG.
           PERFORM TEST-BAL.
           MOVE IO-FG             TO ADONMN-TEST-BAL.
           MOVE "WI/ADONMN" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH ADONMN-BUF EXIT-PATHNAME.
           CANCEL FORM-PROGX.

       EXIT-ADDON-CREATE.
           EXIT.

      **********************************
       PAYSCH-CREATE SECTION.
      **********************************
           MOVE TRANS-DATE        TO PAYSCH-TRANS-DATE.
           MOVE BRANCH            TO PAYSCH-BRANCH.
           MOVE LN-ACCTNO         TO PAYSCH-ACCTNO.
           MOVE LN-PLCD           TO PAYSCH-PLCD.
           MOVE LN-SERVACCT       TO PAYSCH-SERVACCT.
           MOVE LN-SALEFIN        TO PAYSCH-SALEFIN.
           MOVE LN-LOANTYPE       TO PAYSCH-LOANTYPE.
           MOVE LN-1STPYAMT       TO PAYSCH-1STPYAMT.
           MOVE LN-NOREGPY        TO PAYSCH-NOREGPY.
           MOVE LN-REGPYAMT       TO PAYSCH-REGPYAMT.
           MOVE LN-LASTPYAMT      TO PAYSCH-LASTPYAMT.
           MOVE NEW-FLAG          TO PAYSCH-NEW-FLAG.
           PERFORM TEST-BAL.
           MOVE IO-FG             TO PAYSCH-TEST-BAL.
           MOVE "WI/PAYSCH" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH PAYSCH-BUF EXIT-PATHNAME.
           CANCEL FORM-PROGX.

       EXIT-PAYSCH-CREATE.
           EXIT.

      ************************************
       ENTRY-MODULE SECTION.
      ************************************
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
      * TO CALCULATE THE FIRST TIME AFTER SCREEN INPUT
           PERFORM COMPUTE-PRINCIPLE.
           PERFORM COMPUTE-INS-COVERAGE.
           IF BR-AUTO-LNSTAT-WINDOW = "Y"
              PERFORM CALL-LNSTAT-WINDOW.
           MOVE 2 TO NEW-FLAG.
       MODIFY-ROUTINE.
           PERFORM UNPROTECT-FIELDS.
           PERFORM COMPUTE-PRINCIPLE.
           IF ELE = 7 OR 31 OR 32 OR 33 OR 34 OR 37 OR 38
                       OR PREM-CHANGE = "Y"
              PERFORM COMPUTE-INS-COVERAGE.
           IF ELE = 7
              PERFORM COMPUTE-INS-TERM.
           PERFORM COMPUTE-PROCEEDS.
           PERFORM COMPUTE-ANTICIPATED.

           IF BR-INS-ADDON = "Y" OR "O"
              MOVE MODIFY-LEGEND TO LEGEND
           ELSE
              MOVE WI-MODIFY-LEGEND TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "R RA060SEL." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10"
              IF NEW-FLAG = 2
                 PERFORM DELETE-TRAILER-LTC
                 MOVE "CLR" TO IO-TYPE
                 GO TO END-ENTRY-MODULE
              ELSE
                 MOVE "CLR" TO IO-TYPE
                 GO TO END-ENTRY-MODULE.
           IF VDUSTATUS = "18"
              PERFORM MEMO-ENTRY
              GO TO MODIFY-ROUTINE.
           IF BR-INS-ADDON = "Y" OR "O"
             IF VDUSTATUS = "1:"
                 PERFORM PAYSCH-CREATE
                 GO TO MODIFY-ROUTINE.
           IF VDUSTATUS = "16"
              PERFORM CALL-LNSTAT-WINDOW
              GO TO MODIFY-ROUTINE.
           IF BR-INS-ADDON = "Y" OR "O"
              IF VDUSTATUS = "1<"
                 PERFORM ADDON-CREATE
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
                 PERFORM DELETE-TRAILER-LTC
                 MOVE "CLR" TO IO-TYPE
                 GO TO END-ENTRY-MODULE.
           IF ACTION NOT = "    P2" AND ACTION NOT = "     U" AND
              ACTION NOT = "    P3"
              GO TO MODIFY-FIELD.

            IF ACTION = "     U"
              IF LN-CURBAL NOT = 0 OR
                 (LN-CURBAL = 0 AND LN-POFFDATE NOT = 0)
                 MOVE  "UPDATE NOT ALLOWED" TO MESS 
                 PERFORM SEND-MESS 
                 GO TO MODIFY-ROUTINE.


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

           IF CHNO = 23
              MOVE OVERRIDE-APR TO PASSWDW-SEQ
              MOVE "        TO CHANGE APR        " TO PASSWDW-PROMPT2
              PERFORM VERIFY-PASSWORD
              IF PASSWDW-STAT NOT = "OK"
                 GO TO MODIFY-ROUTINE
              ELSE
                 MOVE " " TO CHFG(23).

           MOVE CHNO TO ELE.

      * PASSWORD OTHER 2 O2 INSURANCE
           IF ELE = 29
              IF CHFG(ELE) NOT = "N"
                 MOVE O2-SEQ TO PASSWDW-SEQ
                 MOVE "TO CHANGE OTHER 2 INSURANCE:" TO PASSWDW-PROMPT2
                 PERFORM VERIFY-PASSWORD
                 IF PASSWDW-STAT NOT = "OK"
                    GO TO MODIFY-ROUTINE.

      *    APR IS DEFAULTED INITIALLY IN COMPUTE-PROCEEDS.
      *    IF ANY FIELD OTHER THAN PD-APRATE IS MODIFIED
      *    APR IS RECALCULATED, THUS ALLOWING FOR MANUAL APR
      *    OVERRIDE IN CASE OF ERROR.
           IF ELE NOT = 23
              MOVE "Y" TO DEFAULT-APR-FG.

      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    TEST IF REFUNDS WERE SET IN WI/LNSTAT.C AND IF SO
      *    DO NOT ALLOW CHANGE TO THESE COMPONENTS
      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( ELE = 33 AND LN-INT-REBATE NOT = " " ) OR
              ( ELE = 34 AND LN-INT-REBATE NOT = " " ) OR
              ( ELE = 31 AND LN-SC-REBATE  NOT = " " ) OR
              ( ELE = 32 AND LN-MF-REBATE  NOT = " " )
              STRING "A REFUND FOR THIS COMPONENT WAS SET, "
                     "CHANGE NOT ALLOWED"
                    DELIMITED BY SIZE INTO MESS
              PERFORM SEND-MESS
              GO TO MODIFY-ROUTINE.
      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

       ENTER-ELE.
           PERFORM TEST-BAL.
           IF IO-FG NOT = 0
              MOVE "CURRENT, INT, LCHG AND OTH BALS MUST ALL BE 0.00"
                TO MESS
              PERFORM SEND-MESS
              GO TO MODIFY-ROUTINE.

           IF ELE < 5
              MOVE "ENTER BORROWER #, F6-SCAN, F5-CREATE" TO LEGEND
              PERFORM SEND-LEGEND.

           IF ELE = 5
              IF NEW-FLAG NOT = 1
                 GO TO MODIFY-ROUTINE
              ELSE
                 IF CHFG(5) = "N"
                    GO TO F-052.

           IF ELE = 6
              MOVE "F1-CLEAR, ENTER THE DATE OF THE LOAN:" TO LEGEND
              PERFORM SEND-LEGEND.
           IF ELE = 7
              MOVE "F1-CLEAR, ENTER THE FIRST PAY DUE DATE:" TO LEGEND
              PERFORM SEND-LEGEND.
           IF ELE = 8
              MOVE
              "F1-CLEAR, F6-PURPOSE SCAN, ENTER PURPOSE CODE:"
               TO LEGEND
               PERFORM SEND-LEGEND.
           IF ELE = 9
              MOVE
              "F1-CLEAR, F6-COLLATERAL SCAN, ENTER COLLATERAL CODE:"
               TO LEGEND
               PERFORM SEND-LEGEND.
           IF ELE = 10
              IF NEW-FLAG = 0
                 GO TO CREATE-ROUTINE.
           IF ELE = 10
              MOVE
              "F1-CLEAR, F6-SOURCE SCAN, ENTER SOURCE CODE:"
               TO LEGEND
               PERFORM SEND-LEGEND.
           IF ELE = 13
              IF GP-ENTER-LPO-FG NOT = "Y"
                 MOVE "F1-CLEAR, ENTER INSURANCE CODE (EX. MCC):"
                                                             TO LEGEND
                 PERFORM SEND-LEGEND
              ELSE
                MOVE "F1-CLEAR, F3-LPO, ENTER INSURANCE CODE (EX. MCC):"
                                                             TO LEGEND
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

           IF ELE = 20 AND BR-TRAILER-REC = "Y" AND VDUSTATUS = "16"
              MOVE "Y" TO F3-COLLATERAL
              MOVE WK-HHGS-1 TO INSPECT-X
              PERFORM COLLATERAL-CREATE
              PERFORM RESTORE-TRAILER-REC.

           IF ELE > 24 AND ELE < 31 AND
              LN-INSCODES NOT = "NNN"
              MOVE "ENTER = NONE" TO MESS
              PERFORM SEND-LEGEND.
           IF ELE = 14 OR 31
              PERFORM RESET-LEGEND.
           IF ELE = 31
              IF SP-SCFRMLA = 0
                 GO TO CREATE-ROUTINE.
           IF ELE = 32
              IF SP-MFFRMLA = 0
                 GO TO CREATE-ROUTINE.
           IF ELE = 33
              IF SP-PYEXFRMLA = 0
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
              IF ELE > 13 AND ELE < 19
                 MOVE "SN N070  0." TO VDU
                 MOVE ELE TO VDUROW
                 MOVE 9999999 TO VDUNUM0
                 PERFORM SCREENIO
                 GO TO GOTO-FIELD.
       TEST-UP.
           IF VDUSTATUS NOT = "1U"
              GO TO TEST-F4.
           IF ELE = 6
              MOVE " " TO CHFG(5).
           IF NEW-FLAG = 1
              MOVE -1 TO ONE
              GO TO CREATE-ROUTINE.
       TEST-F4.
           IF ( NOT F4-KEY )
              GO TO TEST-F6.
           IF ( ELE = 20 )
              PERFORM AUTOPC-ROUTINE.
       TEST-F5.
           IF VDUSTATUS NOT = "1:"
              GO TO TEST-F6.

           IF ELE < 5
              PERFORM BORROWER-CREATE
              IF IO-FG = 0
                 GO TO GOTO-FIELD.

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
           IF ELE = 8 OR 9 OR 10
              MOVE 0 TO CDCODE-FEE-FG
              PERFORM CD-SCAN
              IF IO-FG = 0
                 GO TO GOTO-FIELD
              ELSE
                 GO TO ENTER-ELE.
           IF ELE = 39
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
                 F-31 F-32 F-33 F-34 F-35 F-36 F-37 F-38 F-39
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
           MOVE "^-BACK, F6-SCAN, ENTER SUB CLASS:" TO LEGEND.
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
              MOVE "S  8000061." TO VDU
              MOVE LN-LOANDATE TO LN-INTDATE VDU-DATE-YYYYMMDD
              PERFORM SCREENIO.
           IF LN-1STPYDATE NOT = 0
              GO TO F-061.

           MOVE LN-INTDATE TO NDTE-DATE.
           MOVE 1          TO NDTE-HOLD.
           MOVE LN-UNITPER-CD TO DATER-UNITPER-CD.
           MOVE LN-UNITPER-FREQ TO DATER-UNITPER-FREQ.
           PERFORM INCREMENT-UNITPER.
           MOVE "S  A000070." TO VDU.
           MOVE NDTE-DATE TO LN-1STPYDATE LN-ORIG-1STPYDATE
                             DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO ALP-DATE-EDIT.
           MOVE LN-UNITPER-CD TO DATER-UNITPER-CD.
           MOVE LN-UNITPER-FREQ TO DATER-UNITPER-FREQ.
           PERFORM UNITPER-MASK.
           MOVE ALP-DATE-EDIT TO VDUBUF.
           PERFORM SCREENIO.

           IF DATE-FG = "N"
              GO TO CREATE-ROUTINE.
       F-061.
           MOVE "ERNM060061." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO F-061.
           IF VDUNUM0 = 0
              GO TO ENTER-ELE.
           MOVE VDU-DATE-YYYYMMDD TO LN-INTDATE.

           IF LN-INTDATE < LN-LOANDATE
               MOVE "MUST NOT BE PRIOR TO LOAN DATE" TO MESS
               PERFORM SEND-MESS
               GO TO ENTER-ELE.
              
           GO TO CREATE-ROUTINE.

       F-07.
           IF VDUNUM0 = 0
               GO TO ENTER-ELE.
           MOVE VDU-DATE-YYYYMMDD TO LN-ORIG-1STPYDATE
                                     LN-1STPYDATE.

           IF LN-1STPYDATE < LN-INTDATE
              MOVE "FIRST PAY MUST BE AFTER THE INTEREST DATE" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.


      * DO NOT ALLOW A FIRST PAYMENT DATE < ORIGINALLY CALCD DATE !
           MOVE LN-INTDATE TO NDTE-DATE.
           MOVE 1          TO NDTE-HOLD.
           MOVE LN-UNITPER-CD TO DATER-UNITPER-CD.
           MOVE LN-UNITPER-FREQ TO DATER-UNITPER-FREQ.
           PERFORM INCREMENT-UNITPER.
           IF LN-1STPYDATE < NDTE-DATE
              MOVE 1STPY30-SEQ TO PASSWDW-SEQ
              MOVE "1ST PAY < 30 DAYS FROM LNDATE" TO PASSWDW-PROMPT2
              PERFORM VERIFY-PASSWORD
              IF PASSWDW-STAT NOT = "OK"
                 GO TO ENTER-ELE.
      *
       F-071.
           MOVE "ENNN030071." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO F-071.
           IF VDUNUM0 = 0
              GO TO F-071.
           MOVE VDUNUM0 TO LN-ORGTERM LN-ANTICTERM LN-SUM-ORGTERM.
           GO TO CREATE-ROUTINE.
       F-08.
           MOVE VDUBUF TO LN-PURCD CD-CODE.
           MOVE "PU"   TO CD-TYPE.
           PERFORM SEND-CD-DESC.
           IF IO-FG = 9
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       F-09.
           MOVE VDUBUF TO LN-COLLCD CD-CODE.
           MOVE "CO" TO CD-TYPE.
           PERFORM SEND-CD-DESC.
           IF IO-FG = 9
              GO TO ENTER-ELE.

      * OF THE 4 AUTO COLLATERAL CODES THAT WORLD USES, ONLY "AU" AND "WA"
      * ARE ALLOWED IN THE STATE OF "IL". EXCEPT CLASS 8 WHICH CAN HAVE ANY.
      * CLASS 5, SUBCLASS 10 MUST HAVE "AU" OR "WA" ONLY!
           MOVE LN-COLLCD TO TEST-COLLCD.
           IF LN-ORGST = "IL"
              IF ( LN-CLASS = 5 AND LN-SUBCLASS = 10 )
                  IF LN-COLLCD NOT = "AU" AND NOT = "WA"
                     MOVE "MUST HAVE TITLE SECURED COLLATERAL" TO MESS
                     PERFORM SEND-MESS
                     GO TO ENTER-ELE.
           IF LN-ORGST = "IL"
              IF NOT ( LN-CLASS = 5 AND LN-SUBCLASS = 10 )
                 IF LN-CLASS NOT = 8
                    IF AUTO-COLLCD
                     MOVE "TITLE SECURED COLLATERAL NOT ALLOWED" TO MESS
                     PERFORM SEND-MESS
                     GO TO ENTER-ELE.

           GO TO CREATE-ROUTINE.
       F-10.
           MOVE VDUBUF TO LN-SRCD CD-CODE.
           MOVE "SR" TO CD-TYPE.
           PERFORM SEND-CD-DESC.
           IF IO-FG = 9
              GO TO ENTER-ELE.

           IF GP-THE-ONLY-SF-SRCD NOT = " "
              IF LN-SRCD = GP-THE-ONLY-SF-SRCD
                 MOVE "INVALID FOR DIRECT LOANS" TO MESS
                 PERFORM SEND-MESS
                 GO TO ENTER-ELE.

           IF LN-SRCD = "ET" OR "LP" OR "NP" OR "RT" OR "ST" OR "RB"
              MOVE SRCD-SEQ TO PASSWDW-SEQ
              MOVE "TO USE THIS SRCD" TO PASSWDW-PROMPT2
              PERFORM VERIFY-PASSWORD
              IF PASSWDW-STAT NOT = "OK"
                 MOVE "INVALID SOURCE CODE" TO MESS
                 PERFORM SEND-MESS
                 GO TO ENTER-ELE.

           GO TO CREATE-ROUTINE.
       F-11.
           MOVE VDUNUM0 TO LN-PRLNNO.
           GO TO CREATE-ROUTINE.
       F-12.
      *    DUMMY-PARAGRAPH.
       F-13.
           MOVE " " TO CHFG(25) CHFG(26) CHFG(27)
                       CHFG(28) CHFG(29) CHFG(30).
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
           IF CHFG(25) = "N"
              MOVE 0 TO LN-INSCOMP(1) LN-INSTRM(1) LN-INSCOVR(1)
                        LN-INSPREM(1) LN-INSEXP-DATE(1) LN-INSCOMM(1)
              MOVE SPACES TO LN-INSOURS(1)
              MOVE "250 251 252 253 254." TO MESS
              PERFORM RESET-MESS.
           IF CHFG(26) = "N"
              MOVE 0 TO LN-INSCOMP(2) LN-INSTRM(2) LN-INSCOVR(2)
                        LN-INSPREM(2) LN-INSEXP-DATE(2) LN-INSCOMM(2)
              MOVE SPACES TO LN-INSOURS(2)
              MOVE "260 261 262 263 264." TO MESS
              PERFORM RESET-MESS.
           IF CHFG(27) = "N"
              MOVE 0 TO LN-INSCOMP(3) LN-INSTRM(3) LN-INSCOVR(3)
                        LN-INSPREM(3) LN-INSEXP-DATE(3) LN-INSCOMM(3)
              MOVE SPACES TO LN-INSOURS(3)
              MOVE "270 271 272 273 274." TO MESS
              PERFORM RESET-MESS.
           IF CHFG(28) = "N"
              MOVE 0 TO LN-INSCOMP(4) LN-INSTRM(4) LN-INSCOVR(4)
                        LN-INSPREM(4) LN-INSEXP-DATE(4) LN-INSCOMM(4)
              MOVE SPACES TO LN-INSOURS(4)
              MOVE "280 281 282 283 284." TO MESS
              PERFORM RESET-MESS.
           IF CHFG(29) = "N"
              MOVE 0 TO LN-INSCOMP(5) LN-INSTRM(5) LN-INSCOVR(5)
                        LN-INSPREM(5) LN-INSEXP-DATE(5) LN-INSCOMM(5)
              MOVE SPACES TO LN-INSOURS(5)
              MOVE "290 291 292 293 294." TO MESS
              PERFORM RESET-MESS.
           IF CHFG(30) = "N"
              MOVE 0 TO LN-INSCOMP(6) LN-INSTRM(6) LN-INSCOVR(6)
                        LN-INSPREM(6) LN-INSEXP-DATE(6) LN-INSCOMM(6)
              MOVE SPACES TO LN-INSOURS(6)
              MOVE "300 301 302 303 304." TO MESS
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
       F-14.
           MOVE 1 TO I-LEVEL.
           PERFORM FEE-ENTRY.
           IF IO-FG = 9
              GO TO ENTER-ELE.
           IF LN-FEECODE(1) = SPACES
              GO TO CREATE-ROUTINE.
       F-142.
           MOVE "E  A020142." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE.
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
           IF LN-FEECODE(3) = SPACES
              GO TO CREATE-ROUTINE.
       F-152.
           MOVE "E  A020152." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE.
           IF VDUSTATUS NOT = "00"
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
           IF LN-FEECODE(5) = SPACES
              GO TO CREATE-ROUTINE.
       F-162.
           MOVE "E  A020162." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE.
           IF VDUSTATUS NOT = "00"
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
           IF LN-FEECODE(7) = SPACES
              GO TO CREATE-ROUTINE.
       F-172.
           MOVE "E  A020172." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE.
           IF VDUSTATUS NOT = "00"
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
           IF LN-FEECODE(9) = SPACES
              GO TO CREATE-ROUTINE.
       F-182.
           MOVE "E  A020182." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE.
           IF VDUSTATUS NOT = "00"
              GO TO F-182.
           MOVE 10 TO I-LEVEL.
           PERFORM FEE-ENTRY.
           IF IO-FG = 9
              GO TO F-182.
           GO TO CREATE-ROUTINE.
       F-19.
           MOVE VDUNUM2 TO DL-DLNO LN-DLNO.
           PERFORM GET-DL1.
           MOVE "S  A160191." TO VDU.
           MOVE DL-SHORT TO VDUBUF.
           PERFORM SCREENIO.
           IF IO-FG = 9
              GO TO ENTER-ELE.

           IF DL-TERMDATE <= LN-LOANDATE AND DL-TERMDATE NOT = 0
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
              MOVE "SNNN020390." TO VDU
              MOVE DL-LC-CODE TO VDUNUM0 LN-LC-CODE
              PERFORM SCREENIO.
           GO TO CREATE-ROUTINE.
       F-20.
           MOVE VDUBUF TO LN-COLLID.
           IF F3-YES
              GO TO CREATE-ROUTINE.
           IF LN-COLLID = " "
              MOVE SPACES TO SCREEN-LINE1 WK-HHGS-1
              MOVE "201 202." TO MESS
              PERFORM RESET-MESS
              PERFORM SAVE-TRAILER-REC
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
              PERFORM SAVE-TRAILER-REC
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
              PERFORM SAVE-TRAILER-REC
              GO TO F-20-EXIT.
           MOVE SPACES TO WK-HHGS-1.
           STRING SCREEN-LINE1,SCREEN-LINE2 DELIMITED BY SIZE
                                   INTO WK-HHGS-1.
       F-20-EXIT.
           PERFORM SAVE-TRAILER-REC.
           GO TO CREATE-ROUTINE.
       F-21.
           MOVE VDUNUM3 TO LN-OVRDRATE.
           GO TO CREATE-ROUTINE.
       F-22.
      *    DUMMY-PARAGRAPH.
           GO TO CREATE-ROUTINE.
       F-23.
           MOVE VDUNUM3 TO LN-APRATE.
           GO TO CREATE-ROUTINE.
       F-24.
           MOVE VDUNUM3 TO LN-ENDORATE.
           GO TO CREATE-ROUTINE.
       F-241.
      *    DUMMY-PARAGRAPH.
       F-25.
           PERFORM INSURANCE-ENTRY.
           IF IO-FG = 9
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       F-26.
           PERFORM INSURANCE-ENTRY.
           IF IO-FG = 9
              GO TO ENTER-ELE.
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
           MOVE VDUNUM2 TO LN-SERCHG LN-SUM-SERCHG.
           GO TO CREATE-ROUTINE.
       F-32.
           IF SP-MFFRMLA = 0
              GO TO CREATE-ROUTINE.
           DIVIDE VDUNUM2 BY LN-ORGTERM GIVING LN-MAINTFEE
             REMAINDER AMT-HOLDER.
           IF AMT-HOLDER NOT = 0
              MOVE "AMOUNT MUST BE EVENLY DIVISABLE BY TERM" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           COMPUTE LN-SUM-MAINTFEE = LN-MAINTFEE * LN-ORGTERM.
           GO TO CREATE-ROUTINE.
       F-33.
           MOVE VDUNUM2 TO LN-EXTCHG LN-SUM-EXTCHG.
           GO TO CREATE-ROUTINE.
       F-34.
           MOVE VDUNUM2 TO LN-INTCHG LN-SUM-INTCHG.
           GO TO CREATE-ROUTINE.
       F-35.
      *    DUMMY-PARAGRAPH.
           GO TO CREATE-ROUTINE.
       F-36.
      *    DUMMY-PARAGRAPH.
           GO TO CREATE-ROUTINE.
       F-37.
           MOVE VDUNUM2 TO LN-1STPYAMT.
           IF LN-1STPYAMT = 0
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       F-38.
           MOVE VDUNUM2 TO LN-REGPYAMT LN-SUM-REGPYAMT.
           IF VDUNUM2 = 0
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       F-39.
           MOVE VDUNUM0 TO LN-LC-CODE CD-EL-CODE.
           MOVE "EL"    TO CD-TYPE.
           MOVE SPACES  TO CD-EL-FILLER.
           PERFORM GET-CD.
           IF IO-FG NOT = 0 AND LN-LC-CODE NOT = 0
              MOVE "INVALID CODE" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       END-ENTRY-MODULE.
           EXIT.

      ************************************
       AUTOPC-ROUTINE SECTION.
      ************************************

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

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
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
           MOVE "Y" TO F3-COLLATERAL.

       EXIT-COLLATERAL-CREATE.
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

      * ONLY HERE IF YOU ENTERED A SSNO, NOT 0
       GET-BORROWER.
           IF ELE = 3 OR 4
              MOVE COSIGNER-3-AND-4-SEQ TO PASSWDW-SEQ
              MOVE "TO ADD/CHANGE COSIGNER....:" TO PASSWDW-PROMPT2
              PERFORM VERIFY-PASSWORD
              IF PASSWDW-STAT NOT = "OK"
                 MOVE 9 TO IO-FG
                 GO TO EXIT-ENTER-BORROWER.

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
              MOVE "M" TO LN-MAKERCD(1)
              PERFORM SEND-BW-MAKERCD
              MOVE BW-CONFIDENTIAL TO LN-CONFIDENTCD
              MOVE BW-ORIGIN TO LN-ORIGIN
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

      **************************************
       BW-TABLE-CHECK SECTION.
      **************************************
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

      **************************************
       BW-CHECK-ENDORSER SECTION.
      **************************************
           MOVE 0 TO I-LEVEL.
       NEXT-ENDORSER-CHECK.
           ADD 1 TO I-LEVEL.
           IF LN-MAKERCD(I-LEVEL) = "E"
              MOVE " " TO CHFG(24)
              GO TO EXIT-CHECK-ENDORSER.
           IF I-LEVEL < 4
              GO TO NEXT-ENDORSER-CHECK.
       EXIT-CHECK-ENDORSER.
           EXIT.

      **************************************
       SUBCLASS-SCAN SECTION.
      **************************************
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

      *********************************************
       FEECODE-SCAN SECTION.
      *********************************************
           MOVE "GC"   TO CDBSCN-TYPE.
           MOVE BRANCH TO CDBSCN-BRANCH.
           MOVE SPACES TO CDBSCN-CODE.

           MOVE "WI/CDBSCN" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH CDBSCN-BUF EXIT-PATHNAME.
           CANCEL FORM-PROGX.
           IF ( CDBSCN-STATUS NOT = "00" ) OR
              ((CDBSCN-TYPE = "GC") AND (CDBSCN-CODE = SPACES))
              MOVE 9 TO IO-FG
              GO TO EXIT-FEECODE-SCAN.

           IF CDCODE-FEE-FG = 0
              MOVE "S  A030  0." TO VDU
              MOVE ELE TO VDUROW
              MOVE CDBSCN-CODE TO VDUBUF
              PERFORM SCREENIO.

           MOVE CDBSCN-CODE TO VDUBUF.
           MOVE 0 TO IO-FG.

       EXIT-FEECODE-SCAN.
           EXIT.

      **************************************
       CD-SCAN SECTION.
      **************************************

           MOVE SPACES TO CDSCAN-CODE.

           IF ELE = 8
              MOVE "PU"   TO CDSCAN-TYPE
           ELSE
           IF ELE = 9
              MOVE "CO"   TO CDSCAN-TYPE
           ELSE
           IF ELE = 10
              MOVE "SR"   TO CDSCAN-TYPE
           ELSE
              PERFORM FEECODE-SCAN
              GO TO EXIT-CD-SCAN.

           MOVE "WI/CDSCAN" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH CDSCAN-BUF EXIT-PATHNAME.
           CANCEL FORM-PROGX.
           IF ( CDSCAN-STATUS    NOT = "00"    ) OR 
              (( CDSCAN-TYPE = "PU") AND (CDSCAN-CODE = SPACES )) OR
              (( CDSCAN-TYPE = "CO") AND (CDSCAN-CODE = SPACES )) OR
              (( CDSCAN-TYPE = "SR") AND (CDSCAN-CODE = SPACES )) 
              MOVE 9 TO IO-FG
              GO TO EXIT-CD-SCAN.

           IF CDCODE-FEE-FG = 0
              MOVE "S  A030  0." TO VDU
              MOVE ELE TO VDUROW
              MOVE CDSCAN-CODE TO VDUBUF
              PERFORM SCREENIO.

           MOVE CDSCAN-CODE TO VDUBUF.
           MOVE 0 TO IO-FG.

       EXIT-CD-SCAN.
           EXIT.

      **************************************
       CODE-SCAN SECTION.
      **************************************

           MOVE "EL"   TO ELSCAN-TYPE.
           MOVE ZEROES TO ELSCAN-CODE.

           MOVE "WI/ELSCAN " TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH ELSCAN-BUF EXIT-PATHNAME.
           CANCEL FORM-PROGX.

           IF ( ELSCAN-STATUS NOT = "00") OR ( ELSCAN-CODE = 0 )
               MOVE 9 TO IO-FG
               GO TO EXIT-CODE-SCAN.

           MOVE "SNNN020390." TO VDU.
           MOVE ELSCAN-CODE   TO VDUNUM0.
           PERFORM SCREENIO.

           MOVE 0             TO IO-FG.
           MOVE ELSCAN-CODE   TO VDUNUM0.

       EXIT-CODE-SCAN.
           EXIT.

      *******************************************
       CLASS-SCAN SECTION.
      *******************************************
           MOVE 0      TO LCSCAN-KEY.
           MOVE SPACES TO LCSCAN-VERIFY.
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

      ********************************************
       BWLN-SCAN SECTION.
      ********************************************
           MOVE TRANS-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-ALPDATE.
           MOVE ALP-DATE   TO BWLNSC-SYSDATE.
           MOVE 0          TO BWLNSC-ACCTNO.
           MOVE "WI/BWLNSC " TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH BWLNSC-BUF EXIT-PATHNAME.
           CANCEL FORM-PROGX.
           IF BWLNSC-STATUS NOT = "00" OR BWLNSC-ACCTNO = 0
               MOVE 9 TO IO-FG
               GO TO EXIT-BWLN-SCAN.

           MOVE BWLNSC-ACCTNO TO LN-ACCTNO.
           MOVE 0 TO IO-FG.
       EXIT-BWLN-SCAN.
           EXIT.

      **************************************
       BORROWER-CREATE SECTION.
      **************************************
           IF VDUBUF = " "
                    AND LN-SSNO(ELE) NOT = 0
              MOVE LN-SSNO(ELE) TO BWCREA-NO
           ELSE
              MOVE VDUBUF TO ALP-BW
              MOVE ALP-BW123 TO NUM-BW123
              MOVE ALP-BW45 TO NUM-BW45
              MOVE ALP-BW6789 TO NUM-BW6789
              IF NUM-BW NOT NUMERIC
                 MOVE 0 TO BWCREA-NO
              ELSE
                 MOVE NUM-BW TO BWCREA-NO.

           MOVE LN-SSNO(1) TO BWCREA-SSNO(1).
           MOVE LN-SSNO(2) TO BWCREA-SSNO(2).
           MOVE LN-SSNO(3) TO BWCREA-SSNO(3).
           MOVE LN-SSNO(4) TO BWCREA-SSNO(4).
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
           MOVE ALP-BW TO VDUBUF.
           MOVE 0 TO IO-FG.

       EXIT-BORROWER-CREATE.
           EXIT.


      **************************************
       BORROWER-SCAN SECTION.
      **************************************
           MOVE 0   TO BWSCAN-NO.
           MOVE "S" TO BWSCAN-SCAN-TYPE.
           MOVE "WI/BWSCAN" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH BWSCAN-BUF EXIT-PATHNAME.
           CANCEL FORM-PROGX.
           IF BWSCAN-STATUS NOT = "00" OR BWSCAN-NO = 0
              MOVE 9 TO IO-FG
              GO TO EXIT-BORROWER-SCAN.
           MOVE BWSCAN-NO TO BW-SSNO.
           PERFORM NUMBW-TO-ALPBW.
           MOVE "S  A110  0." TO VDU.
           MOVE ELE TO VDUROW.
           MOVE ALP-BW TO VDUBUF.
           PERFORM SCREENIO.
           MOVE BWSCAN-NO TO VDUNUM0.
           MOVE 0 TO IO-FG.

       EXIT-BORROWER-SCAN.
           EXIT.

      **************************************
       INSURANCE-ENTRY SECTION.
      **************************************
           MOVE 0 TO IO-FG.
           SUBTRACT 24 FROM ELE GIVING I-LEVEL.
           IF VDUNUM0 = 0
              MOVE VDUNUM0 TO LN-INSCOMP(I-LEVEL) IC-CODE
              GO TO ENTER-IC-OURS.

      * TEST PASSWD TO CHANGE DEFAULT INSURANCE COMPANY ON CL,AH,PP,O1
           IF I-LEVEL < 5
              IF VDUNUM0 NOT = LN-INSCOMP(I-LEVEL)
                 MOVE INSCO-SEQ TO PASSWDW-SEQ
                 MOVE "TO CHANGE INS. COMPANY....:" TO PASSWDW-PROMPT2
                 PERFORM VERIFY-PASSWORD
                 IF PASSWDW-STAT NOT = "OK"
                    MOVE "SNNN020  0." TO VDU
                    MOVE ELE TO VDUROW
                    MOVE LN-INSCOMP(I-LEVEL) TO VDUNUM0
                    PERFORM SCREENIO
                    MOVE 9 TO IO-FG
                    GO TO EXIT-ENTER-INSURANCE.

      * PASSWORD OTHER 2 O2 INSURANCE
           IF I-LEVEL = 5
              MOVE O2-SEQ TO PASSWDW-SEQ
              MOVE "TO CHANGE OTHER 2 INSURANCE:" TO PASSWDW-PROMPT2
              PERFORM VERIFY-PASSWORD
              IF PASSWDW-STAT NOT = "OK"
                 MOVE 9 TO IO-FG
                 GO TO EXIT-ENTER-INSURANCE.

           MOVE VDUNUM0 TO LN-INSCOMP(I-LEVEL) IC-CODE.
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
           IF NOT (VDUSTATUS = "00" OR "1D")
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
           IF NOT (VDUSTATUS = "00" OR "1D")
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
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO ENTER-IC-EXP.
           IF VDUNUM0 = 0
              GO TO ENTER-IC-EXP.
      * NOT ENTERED WITH DATE SCREENIO SETTINGS
           MOVE VDUNUM0 TO DATE-MMDDYY.
           PERFORM CONVERT-MMDDYY-TO-YYYYMMDD.
           MOVE DATE-YYYYMMDD TO LN-INSEXP-DATE(I-LEVEL) NUM-DATE.           
           PERFORM TSTDAT.
           IF NUM-DATE = 0
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
           IF NOT (VDUSTATUS = "00" OR "1D")
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
           IF NOT (VDUSTATUS = "00" OR "1D")
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

      **************************************
       FEE-ENTRY SECTION.
      **************************************
           MOVE VDUBUF TO CDB-CODE LN-FEECODE(I-LEVEL).
           IF CDB-CODE NOT = SPACES
              PERFORM GET-CODE
              MOVE CDB-DESC TO LEGEND
              PERFORM SEND-LEGEND
              IF IO-FG = 9
                 GO TO EXIT-FEE-ENTRY
              ELSE
                 MOVE CDB-GC-GLFEEFINCHG TO LN-FEEFINCHG(I-LEVEL)
                 GO TO ENTER-FEEAMT.

           IF I-LEVEL = 2 OR 4 OR 6 OR 8 OR 10
              GO TO GL-CLEAR-2.

       NEXT-GL-CLEAR.
           MOVE "S  A000  0." TO VDU.
           MOVE ELE TO VDUROW.
           MOVE SPACES TO LN-FEECODE(I-LEVEL).
           PERFORM SCREENIO.
           MOVE "SN N042  1." TO VDU.
           MOVE ELE TO VDUROW.
           MOVE 0 TO LN-FEEAMT(I-LEVEL) VDUNUM0.
           PERFORM SCREENIO.
      *CS  03/26/01
           MOVE SPACES TO LN-FEEFINCHG(I-LEVEL)
           ADD 1 TO I-LEVEL.
       GL-CLEAR-2.
           MOVE "S  A020  2." TO VDU.
           MOVE ELE TO VDUROW.
           MOVE SPACES TO LN-FEECODE(I-LEVEL).
           PERFORM SCREENIO.
           MOVE "SN N042  3." TO VDU.
           MOVE ELE TO VDUROW.
           MOVE 0 TO LN-FEEAMT(I-LEVEL) VDUNUM0.
           PERFORM SCREENIO.
      *CS  03/26/01
           MOVE SPACES TO LN-FEEFINCHG(I-LEVEL)
           IF I-LEVEL < 10
              ADD 1 TO I-LEVEL ELE
              GO TO NEXT-GL-CLEAR.
           MOVE 0 TO IO-FG.
           GO TO EXIT-FEE-ENTRY.
       ENTER-FEEAMT.
           IF I-LEVEL > 2
              SUBTRACT 1 FROM I-LEVEL GIVING SUB
              IF LN-FEECODE(SUB) = SPACES
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
           IF VDUSTATUS NOT = "00"
              GO TO ENTER-FEEAMT.
           IF VDUNUM2 = 0
              GO TO ENTER-FEEAMT.
           MOVE VDUNUM2 TO LN-FEEAMT(I-LEVEL).

           IF ( LN-FEEAMT(I-LEVEL) > CDB-GC-FEE-MAXAMT )
              MOVE "INVALID: AMOUNT IS GREATER THAN THE LIMIT" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-FEEAMT.

       EXIT-FEE-ENTRY.
           EXIT.

      **************************************
       CHECK-EFFECTIVE-SPR SECTION.
      **************************************

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
           IF (SP-ORGST NOT = LN-ORGST)
                   OR (SP-SPRCLASS NOT = LN-SPRCLASS)
                    OR (SP-SUBCLASS NOT = LN-SUBCLASS)
              MOVE 9 TO IO-FG
              GO TO CHECK-EFF-SPR-EXIT.


           IF SP-EFFDATE > LN-LOANDATE
               GO TO CHECK-EFF-SPR-NEXT
           ELSE
           IF SP-EXPDATE = 0
               GO TO CHECK-EFF-SPR-EXIT.

           IF SP-EXPDATE <= LN-LOANDATE
               GO TO CHECK-EFF-SPR-NEXT.

       CHECK-EFF-SPR-EXIT.
           EXIT.

      **************************************
       COMPUTE-FINANCE-CHARGE SECTION.
      **************************************
           COMPUTE LN-FINCHG = LN-INTCHG + LN-EXTCHG
                                + LN-SERCHG
                                + (LN-MAINTFEE * LN-ORGTERM).
           PERFORM LOAN-CALCULATIONS.
           ADD TOTAL-INSFINCHG TOTAL-FEEFINCHG TO LN-FINCHG.
           MOVE LN-FINCHG TO LN-SUM-FINCHG.

      **************************************
       COMPUTE-PRINCIPLE SECTION.
      **************************************
           PERFORM COMPUTE-NOREGPY.
           PERFORM LOAN-CALCULATIONS.
           MOVE "SN N062LOANAMT." TO VDU.
           MOVE PRINCIPLE TO VDUNUM2 LN-LNAMT LN-ORIGPURAMT
                             LN-SUM-LNAMT LN-SUM-ORIGPURAMT.
      *IF THERE IS A BF ON ACCOUNT AND YOU GO BACK TO LOOK AT THE
      *LOAN, 'U' TO UPDATE WITHOUT CHANGE, PRINCIPLE GETS MOVES TO
      *PURCHASE BALANCE OVERWRITING THE PURCHASE BALANCE CAUSING
      *"MUST REDUCE PURCHASE BALANCE" AT CHARGEOFF (PL)
           IF LN-CURBAL = 0
                  MOVE PRINCIPLE TO LN-PURBAL.
           PERFORM SCREENIO.

           MOVE "SN N062350." TO VDU.
           MOVE FINANCED-AMOUNT TO VDUNUM2.
           PERFORM SCREENIO.

           MOVE "SN N062360." TO VDU.
           MOVE TOTAL-REPAY TO VDUNUM2.
           PERFORM SCREENIO.

      **************************************
       COMPUTE-INS-COVERAGE SECTION.
      **************************************
           IF LN-INSTYPES-LL OR LN-INSTYPES-DL
              IF LN-INSPREM(1) NOT = 0
                 MOVE "SN N062253." TO VDU
                 MOVE 1 TO LN-SUB
                 IF LN-INSTYPES-LL
                    MOVE 3 TO SP-SUB
                    PERFORM SET-COVERAGE
                 ELSE
                    MOVE 1 TO SP-SUB
                    PERFORM SET-COVERAGE.
           IF LN-INSTYPES-AH
              IF LN-INSPREM(2) NOT = 0
                 MOVE "SN N062263." TO VDU
                 MOVE 5 TO SP-SUB
                 MOVE 2 TO LN-SUB
                 PERFORM SET-COVERAGE.
           IF LN-INSTYPES-PP OR LN-INSTYPES-PP2
              IF LN-INSCOMP(3) NOT = 0
                 MOVE "SN N062273." TO VDU
                 MOVE 3 TO LN-SUB
                 IF LN-INSTYPES-PP
                    MOVE 6 TO SP-SUB
                    PERFORM SET-COVERAGE
                 ELSE
                    MOVE 7 TO SP-SUB
                    PERFORM SET-COVERAGE.
           IF LN-INSTYPES-O1
              IF LN-INSPREM(4) NOT = 0
                 MOVE "SN N062283." TO VDU
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
              MOVE DISCLOSED-FINAMT TO LN-INSCOVR(LN-SUB)
              MOVE LN-INSCOVR(LN-SUB) TO VDUNUM2
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

      **************************************
       COMPUTE-INS-TERM SECTION.
      **************************************
           MOVE 0 TO I-LEVEL.
       COMPUTE-INS-TRM.
           IF I-LEVEL < 5
              ADD 1 TO I-LEVEL
              PERFORM COMPUTE-INS-TERM-I-LEVEL
              GO TO COMPUTE-INS-TRM.

      **************************************
       COMPUTE-INS-TERM-I-LEVEL SECTION.
      **************************************
           IF I-LEVEL = 1
              IF LN-INSCOMP(1) NOT = 0
                 MOVE 0 TO SUB
                 IF LN-INSURED = "J"
                    MOVE 1 TO SUB.

           IF I-LEVEL = 1
              IF LN-INSCOMP(1) NOT = 0
                 MOVE "SN N030252." TO VDU
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
                    MOVE "SN N030262." TO VDU
                    MOVE 5 TO SUB
                    PERFORM SET-INSURANCE-TERM-VDU
                    MOVE VDUNUM0 TO LN-INSTRM(I-LEVEL)
                    PERFORM SCREENIO.

           IF I-LEVEL = 3
              IF LN-INSCOMP(3) NOT = 0
                 MOVE "SN N030272." TO VDU
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
                    MOVE "SN N030282." TO VDU
                    MOVE 8 TO SUB
                    PERFORM SET-INSURANCE-TERM-VDU
                    MOVE VDUNUM0 TO LN-INSTRM(I-LEVEL)
                    PERFORM SCREENIO.

           IF I-LEVEL = 5
              IF LN-INSCOMP(5) NOT = 0
                 IF GP-OT-INS-TERM = "Y"
                    MOVE "SN N030282." TO VDU
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
                    MOVE "SNNN060292." TO VDU
                    MOVE NDTE-DATE TO LN-INSEXP-DATE(5) DATE-YYYYMMDD
                    PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
                    MOVE DATE-MMDDYY TO VDUNUM0               
                    PERFORM SCREENIO.
                

           IF I-LEVEL = 6
              IF LN-INSCOMP(6) NOT = 0
                 IF GP-OT-INS-TERM = "Y"
                    MOVE "SN N030302." TO VDU
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
                    MOVE "SNNN060302." TO VDU
                    MOVE NDTE-DATE TO LN-INSEXP-DATE(6) DATE-YYYYMMDD
                    PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
                    MOVE DATE-MMDDYY TO VDUNUM0               
                    PERFORM SCREENIO.

      **************************************
       SET-INSURANCE-TERM-VDU SECTION.
      **************************************
           IF SP-INSURE-EXT-PERIOD(SUB)
              PERFORM EXTENSION-DAYS-CALCULATION
              IF EDAY-MONTHS > 0
                 COMPUTE VDUNUM0 = EDAY-MONTHS + LN-ORGTERM
              ELSE
                 MOVE LN-ORGTERM TO VDUNUM0
           ELSE
              MOVE LN-ORGTERM TO VDUNUM0.

      **************************************
       COMPUTE-PROCEEDS SECTION.
      **************************************
           MOVE 0 TO SUB NON-MEMO-INS.
       NEXT-INSPREM.
           ADD 1 TO SUB.
           IF LN-INSOURS(SUB) NOT = "M"
              ADD LN-INSPREM(SUB) TO NON-MEMO-INS.
           IF SUB < 6
              GO TO NEXT-INSPREM.
           MOVE "SN N062120." TO VDU.
           COMPUTE VDUNUM2 = TOTAL-REPAY - NON-MEMO-INS - LN-EXTCHG
                           - LN-INTCHG - LN-SERCHG
                           - (LN-ORGTERM * LN-MAINTFEE).
           MOVE 0 TO SUB.
       NEXT-FEEAMT.
           ADD 1 TO SUB.
           SUBTRACT LN-FEEAMT(SUB) FROM VDUNUM2.
           IF SUB < 10
              GO TO NEXT-FEEAMT.
           MOVE VDUNUM2 TO LN-PROCEEDS.
           PERFORM SCREENIO.

      *    DEFAULT EXCEPT WHEN MANUAL OVERRIDE OCCURS
           IF DEFAULT-APR-FG = "N"
              GO TO COMPUTE-PROCEEDS-EXIT.
           MOVE "N" TO DEFAULT-APR-FG.
           COMPUTE APR-FINANCED = (LN-REGPYAMT * LN-NOREGPY) +
                                  (LN-1STPYAMT + LN-LASTPYAMT)  -
                                  LN-EXTCHG - LN-SERCHG - LN-INTCHG -
                                  (LN-MAINTFEE * LN-ORGTERM).
           SUBTRACT TOTAL-FEEFINCHG TOTAL-INSFINCHG FROM APR-FINANCED.


           MOVE 1 TO APR-PAYFREQ.
           MOVE LN-ORGTERM TO APR-ORGTERM.
           MOVE LN-LASTPYAMT TO APR-LASTPYAMT.
           MOVE LN-REGPYAMT TO APR-REGPYAMT.
           MOVE LN-1STPYAMT TO APR-1STPYAMT.
           MOVE LN-INTDATE TO APR-INTDATE.
           MOVE LN-1STPYDATE TO APR-1STPYDATE.
           MOVE LN-EXTCHG TO APR-EXTCHG.
           MOVE LN-UNITPER-CD TO APR-UNITPER-CD.
           MOVE LN-UNITPER-FREQ TO APR-UNITPER-FREQ.
           MOVE SP-DISFRMLA TO APR-DISFRMLA.
           PERFORM APR-REGZ.
           MOVE "SN N033230." TO VDU.
           MOVE APR-APR TO VDUNUM3 LN-APRATE.
           PERFORM SCREENIO.

       COMPUTE-PROCEEDS-EXIT.
           EXIT.

      **************************************
       COMPUTE-ANTICIPATED SECTION.
      **************************************
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

           MOVE "SN N062ANTIC." TO VDU.
           COMPUTE VDUNUM2 = LN-ANTICERN(1) + LN-ANTICERN(2)
                           + LN-ANTICERN(3) + LN-ANTICERN(4)
                           + LN-ANTICERN(5).
           IF NOT GP-INCL-OTH-IN-EARNED-N
              ADD LN-ANTICERN(6) TO VDUNUM2.
           PERFORM SCREENIO.

      **************************************
       MEMO-ENTRY SECTION.
      **************************************
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

      **************************************
       NEW-RECORD-SETUP SECTION.
      **************************************
           MOVE 1 TO NEW-FLAG.
           MOVE "S  A000SORTKEY." TO VDU.
           MOVE "* NEW *" TO VDUBUF.
           PERFORM SCREENIO.

           PERFORM CLEAR-LN-FILE.

           MOVE BR-STATE TO DEF-ORGST LN-ORGST LN-OWNST.
           MOVE BRANCH TO DEF-ORGBR LN-ORGBR LN-OWNBR.
           MOVE 999.99 TO LN-SERVPER.
           MOVE 999.999 TO LN-ENDORATE LN-OVRDRATE.
           MOVE DEF-BUF TO WR-BUF.
           MOVE DEF-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.
           MOVE "N" TO STAT.
           MOVE "Y" TO DEFAULT-APR-FG.

      *--{CS}---
           IF BULK-CONVERSION-FG = "B"
              MOVE "B" TO LN-CONVERCD
           ELSE
              MOVE "Y" TO LN-CONVERCD.

           PERFORM PROTECT-FIELDS.
           MOVE 9999.99 TO LN-DLNO DL-DLNO.
           PERFORM GET-DL1.
           IF BR-LC-CODE = "Y"
              MOVE "SNNN020390." TO VDU
              MOVE DL-LC-CODE TO VDUNUM0 LN-LC-CODE
              PERFORM SCREENIO.
           MOVE 0 TO ELE.

      **************************************
       UPDATE-LN1-INSURANCE SECTION.
      **************************************

           PERFORM VARYING IEXP-LN FROM 1 BY 1 UNTIL IEXP-LN > 8
              PERFORM IEXP-INS-CALCULATIONS
              MOVE IEXP-EFFDATE TO LN-INSEFF-DATE(IEXP-LN)
              MOVE IEXP-EXPDATE TO LN-INSEXP-DATE(IEXP-LN)
              MOVE IEXP-INSCOMM TO LN-INSCOMM(IEXP-LN)

      *- IF PREMIUM IS ZEROS SET ALL FIELDS BACK TO ZEROS AND SPACES
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

      **************************************
       DISPLAY-FIELDS SECTION.
      **************************************
           MOVE SPACES TO DISPLAY-BUF1.
           PERFORM GET-SP.
           MOVE "Y"       TO VALID-LOAN.
           MOVE "NVLD"    TO LAST-READ.
           MOVE LN-ACCTNO TO D-LNNO.

           IF LN-PLCD = "P"
              MOVE "PL"  TO D-P
              IF LN-PLTYPE NOT = "P"
                 MOVE LN-PLTYPE TO D-PTYPE.
           IF LN-JDDATE NOT = 0
              MOVE "JDG" TO D-J.
           MOVE LN-CLASS TO LC-CODE D-LCNO.
           PERFORM GET-LC.
           MOVE LC-DESC TO D-LC-DESC.
           PERFORM GET-SP.

           MOVE 1 TO SUB.
       DISP-NEXT-BW.
           MOVE LN-SSNO(SUB) TO BW-SSNO.
           PERFORM NUMBW-TO-ALPBW.
           MOVE " " TO BW-LNAME BW-FNAME.
           IF BW-SSNO NOT = 0
              PERFORM GET-BW1
              MOVE ALP-BW TO D-010(SUB)
              MOVE INSPECT-W TO D-011(SUB)
              MOVE LN-MAKERCD(SUB) TO D-012(SUB).
           IF SUB < 4
              ADD 1 TO SUB
              GO TO DISP-NEXT-BW.

           MOVE LN-ORGST TO D-050.
           MOVE LN-ORGBR TO D-051.
           MOVE LN-SUBCLASS TO D-052.
           MOVE SP-DESC TO D-053.
           MOVE LN-LOANDATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO D-060.
           MOVE LN-INTDATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO D-061.

           MOVE LN-1STPYDATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO ALP-DATE-EDIT.
           MOVE LN-UNITPER-CD TO DATER-UNITPER-CD.
           MOVE LN-UNITPER-FREQ TO DATER-UNITPER-FREQ.
           PERFORM UNITPER-MASK.
           MOVE ALP-DATE-EDIT TO D-070.
           MOVE LN-ORGTERM    TO D-071.

           MOVE LN-PURCD TO CD-CODE D-080.
           MOVE "PU"     TO CD-TYPE.
           PERFORM GET-CD.
           MOVE CD-DESC    TO D-081.

           MOVE LN-COLLCD TO CD-CODE D-090.
           MOVE "CO"      TO CD-TYPE.
           PERFORM GET-CD.
           MOVE CD-DESC   TO D-091.

           MOVE LN-SRCD TO CD-CODE D-100.
           MOVE "SR"    TO CD-TYPE.
           PERFORM GET-CD.
           MOVE CD-DESC TO D-101.

           MOVE LN-PRLNNO TO D-110.
           MOVE LN-PROCEEDS TO D-120.
           MOVE LN-INSCODES TO D-130.

           MOVE 1 TO ELE SUB.
       DISP-NEXT-GL.
           MOVE LN-FEECODE(SUB) TO D-1FEECD(ELE).
           MOVE LN-FEEAMT(SUB) TO D-1FEEAMT(ELE).
           ADD 1 TO SUB.
           MOVE LN-FEECODE(SUB) TO D-2FEECD(ELE).
           MOVE LN-FEEAMT(SUB) TO D-2FEEAMT(ELE).
           IF SUB < 10
              ADD 1 TO ELE SUB
              GO TO DISP-NEXT-GL.

           MOVE LN-DLNO TO D-190 DL-DLNO.
           PERFORM GET-DL1.
           MOVE DL-SHORT TO D-191.
           MOVE DISPLAY-BUF1 TO WR-BUF.
           MOVE DISPLAY-LIST1 TO WR-LIST.
           PERFORM CALL-WRFORM.

           MOVE " " TO DISPLAY-BUF2.
           IF BR-LC-CODE = "Y"
              MOVE LN-LC-CODE TO D-390.

           MOVE LN-COLLID TO D-200.

           PERFORM GET-COLLATERAL.

           MOVE LTC-COLL-INFO1(1) TO D-201.
           MOVE LTC-COLL-INFO2(1) TO D-202.

           MOVE LN-OVRDRATE TO D-210.
           MOVE LN-EFFRATE TO D-220.
           MOVE LN-APRATE TO D-230.
           MOVE LN-ENDORATE TO D-240.
           MOVE LN-LNAMT TO D-LOANAMT.
           COMPUTE ANTIC = LN-ANTICERN(1) + LN-ANTICERN(2)
                         + LN-ANTICERN(3) + LN-ANTICERN(4)
                         + LN-ANTICERN(5).
           IF NOT GP-INCL-OTH-IN-EARNED-N
              ADD LN-ANTICERN(6) TO ANTIC.
           MOVE ANTIC TO D-ANTIC.

           MOVE 1 TO SUB.
       DISP-NEXT-IC.
           MOVE LN-INSCOMP(SUB) TO D-1COMP(SUB).
           MOVE LN-INSPREM(SUB) TO D-1PREM(SUB).
           MOVE SUB TO ITRM-SUB.
           PERFORM INS-TERM-CALCULATION.
           MOVE ITRM-INSTERM TO D-1TERM(SUB) LN-INSTRM(SUB).
           MOVE LN-INSCOVR(SUB) TO D-1COVR(SUB).
           MOVE LN-INSOURS(SUB) TO D-1OUR(SUB).
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
           MOVE LN-INSOURS(ELE) TO D-2OUR(SUB).
           IF ELE < 6
              ADD 1 TO ELE SUB
              GO TO DISP-NEXT-2IC.

           MOVE LN-SERCHG TO D-310.
           COMPUTE D-320 = LN-MAINTFEE * LN-ORGTERM.
           MOVE LN-EXTCHG TO D-330.
           MOVE LN-INTCHG TO D-340.
           PERFORM LOAN-CALCULATIONS.
           MOVE FINANCED-AMOUNT TO D-350.
           MOVE TOTAL-REPAY TO D-360.
           MOVE LN-1STPYAMT TO D-370.
           MOVE LN-REGPYAMT TO D-380.

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

      **************************************
       TEST-ALLOT-LBOX SECTION.
      **************************************

           PERFORM ACCOUNT-LBOX.
           IF IO-TYPE = "RTN"
              GO TO TEST-ALLOT-LBOX-EXIT.

           IF LBOX-CODE NOT = "Y"
              PERFORM ACCOUNT-ALLOTMENTS.
       TEST-ALLOT-LBOX-EXIT.
           EXIT.

      **************************************
       ACCOUNT-ALLOTMENTS SECTION.
      **************************************
       ACCT-ALLOT.
           MOVE SPACES TO LN-ALLOTCD IO-TYPE ALLOT-BUF.
           IF BR-ALLOTMNTS NOT = "Y"
              GO TO ACCOUNT-ALLOT-EXIT.

           MOVE "^, ACCOUNT UNDER AN ALLOTMENT PLAN ?" TO LEGEND.
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
       ACCOUNT-ALLOT-EXIT.
           EXIT.

      **************************************
       ACCOUNT-LBOX SECTION.
      **************************************
       ACCT-LBOX.
           MOVE SPACES TO LN-LBOX IO-TYPE LBOX-BUF.
           IF BR-LBOX NOT = "Y"
              GO TO ACCOUNT-LBOX-EXIT.

           MOVE "^, ACCOUNT UNDER A LOCK BOX PLAN ?" TO LEGEND.
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
       ACCOUNT-LBOX-EXIT.
           EXIT.

      **************************************
       WRITE-MODULE SECTION.
      **************************************
           PERFORM OPEN-LN1-FILE.
           IF NEW-FLAG NOT = 0
              GO TO WRITE-RECORD.

           PERFORM APRS-RATE.
           MOVE APRS-SMPRATE TO LN-SMPRATE.
           MOVE APRS-EFFRATE TO LN-EFFRATE.
           IF SP-CAL-SET-EFFECTIVE(APRS-CTBL)
              MOVE LN-EFFRATE TO LN-OVRDRATE.
      *  JTG 010112 ADDED NEW 88  SP-CAL-SET-SMPRATE-OVRRIDE VALUE 4 MERC PR#J0
           IF SP-CAL-SET-SMPRATE-OVRRIDE(APRS-CTBL)
              MOVE LN-SMPRATE TO LN-OVRDRATE.

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
           GO TO END-WRITE-MODULE.

       WRITE-RECORD.
           PERFORM UPDATE-MSG.
           MOVE TRANS-DATE TO LN-ENTDATE.
           IF BR-CONVERT-AUTO-ACCTNO = "Y"
              IF ACCTNO-WAS-SET-FG = "Y"
                 PERFORM SET-ACCOUNT-NUMBER.

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
              MOVE SP-SKIP-TRW TO LN-SKIP-TRW.

      ****************************************
      *    ESTABLISH CREDIT LIMIT
      ****************************************
           IF BR-AVAIL-CREDIT = "Y" OR "S"
              IF LC-CALC-HI-CREDIT = "C" OR "L" OR SPACE OR "W"
                 PERFORM LOAN-CALCULATIONS
                 MOVE HIGH-CREDIT TO LN-CREDITLIM.

           PERFORM WRITE-LN1-FILE.
           IF IO-FG = 9
              PERFORM ALREADY-CREATED
           ELSE
              PERFORM UPDATE-LT.
       END-WRITE-MODULE.
           PERFORM CLOSE-LN1-FILE.

      ****************************************
       UPDATE-LT SECTION.
      ****************************************

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
              MOVE LN-OWNBR  TO LTC-BRNO
              MOVE LN-ACCTNO TO LTC-ACCTNO
              MOVE 1         TO LTC-SEQNO

              MOVE WK-DESC-1  TO LTC-DESC(1)
              MOVE WK-MAKE-1  TO LTC-MAKE(1)
              MOVE WK-MODEL-1 TO LTC-MODEL(1)
              MOVE WK-YEAR-1  TO LTC-YEAR(1)
              MOVE WK-STYLE-1 TO LTC-STYLE(1)
              MOVE WK-CLASS-1 TO LTC-CLASS(1)
              MOVE WK-VIN-1   TO LTC-VIN(1)
              MOVE WK-TITLE-NO-1 TO LTC-TITLE-NO(1)
              MOVE WK-MILEAGE-1  TO LTC-MILEAGE(1)
              MOVE WK-VALUE-1    TO LTC-VALUE(1)
              MOVE WK-SELLPR-1   TO LTC-SELLPR(1)
              MOVE WK-MISC-AMT-1 TO LTC-MISC-AMT(1)
              MOVE 0             TO LTC-VIN-CHG-DATE(1)

              MOVE WK-DESC-2  TO LTC-DESC(2)
              MOVE WK-MAKE-2  TO LTC-MAKE(2)
              MOVE WK-MODEL-2 TO LTC-MODEL(2)
              MOVE WK-YEAR-2  TO LTC-YEAR(2)
              MOVE WK-STYLE-2 TO LTC-STYLE(2)
              MOVE WK-CLASS-2 TO LTC-CLASS(2)
              MOVE WK-VIN-2   TO LTC-VIN(2)
              MOVE WK-TITLE-NO-2 TO LTC-TITLE-NO(2)
              MOVE WK-MILEAGE-2  TO LTC-MILEAGE(2)
              MOVE WK-VALUE-2    TO LTC-VALUE(2)
              MOVE WK-SELLPR-2   TO LTC-SELLPR(2)
              MOVE WK-MISC-AMT-2 TO LTC-MISC-AMT(2)
              MOVE 0             TO LTC-VIN-CHG-DATE(2)

              MOVE WK-HHGS-1 TO LTC-HHGS(1)
              MOVE WK-HHGS-2 TO LTC-HHGS(2)
              MOVE WK-HHGS-3 TO LTC-HHGS(3)
              PERFORM WRITE-LTC1-FILE
              IF IO-FG NOT = 0
                 PERFORM REWRITE-LTC1-FILE.


           PERFORM CLOSE-LTC1-FILE.

       UPDATE-LT-EXIT.
           EXIT.

      **************************************
       DELETE-MODULE SECTION.
      **************************************
           IF NEW-FLAG NOT = 0
              MOVE "N" TO VALID-DELETE
              MOVE "ONLY DELETE AN EXISTING LOAN" TO MESS
              PERFORM SEND-MESS
              GO TO END-DELETE-MODULE.

           MOVE "VERIFICATION OF DELETE IN PROGRESS...." TO LEGEND.
           PERFORM SEND-LEGEND.

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

           PERFORM OPEN-LP1-FILE.

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
           IF IO-FG = 9 OR (LP-BRNO NOT = LP-PATH-OWNBR)
              GO TO CONTINUE-DELETE.

           PERFORM CLOSE-LP1-FILE.

           IF (LP-BRNO = LP-PATH-OWNBR) AND (LP-ACCTNO = LN-ACCTNO)
              MOVE "ACCOUNT HAS PAYMENT RECORD - CANT DELETE" TO MESS
              PERFORM SEND-MESS
              MOVE "N" TO VALID-DELETE
              GO TO END-DELETE-MODULE.

       CONTINUE-DELETE.
           MOVE "DELETING LOAN MASTER............" TO LEGEND.
           PERFORM SEND-LEGEND.
           PERFORM DELETE-LN1-FILE.

       END-DELETE-MODULE.
           PERFORM CLOSE-LN1-FILE.

      ****************************************************
       DELETE-NOTICE SECTION.
      ****************************************************
           MOVE "DELETING NOTICE.............." TO LEGEND.
           PERFORM SEND-LEGEND.

           PERFORM OPEN-NO1-FILE.
           MOVE LN-OWNBR  TO NO-BRNO.
           MOVE LN-ACCTNO TO NO-ACCTNO.
           PERFORM READ-NO1-FILE.
           IF IO-FG = 0
              PERFORM DELETE-NO1-FILE.

           PERFORM CLOSE-NO1-FILE.
       EXIT-DELETE-NOTICE.
           EXIT.

      **************************************
       DELETE-TRAILER-LTC SECTION.
      **************************************

           MOVE "DELETING LOAN TRAILER..............." TO LEGEND.
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
           IF IO-FG = 9 OR (LTC-BRNO NOT = LTC-PATH-OWNBR)
                OR (LTC-ACCTNO NOT = LN-ACCTNO)
              GO TO EXIT-DELETE-TRAILER.

           PERFORM DELETE-LTC1-FILE.

           GO TO NEXT-LT-DELETE.
       EXIT-DELETE-TRAILER.
           PERFORM CLOSE-LTC1-FILE.

      **********************************************
       VERIFY-MODULE SECTION.
      **********************************************

           MOVE TRANS-DATE TO VERIFY-TODAY-DATE.
           IF LN-SSNO(1) = 0
              MOVE 0       TO BW-BDATE BW-REFDATE
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
           MOVE 0 TO VERIFY-TERMCEIL-EFF.
           MOVE "N" TO VERIFY-TERMCEIL-EFF-FG.
           MOVE " " TO VERIFY-REJECT-TERM-FG.
           PERFORM VERIFY-TERMCEIL.

           PERFORM VERIFY-DATES.
           PERFORM VERIFY-PAYMENTS.
           PERFORM VERIFY-INSURANCE.


           IF LN-ORGST = "AL"
              IF LN-CLASS = 21
                 IF LN-SUBCLASS =2
                    IF LN-LAWCODE = 1
                       IF LN-OVRDRATE = 999.999
                    MOVE "MUST INPUT OVERRIDE RATE ON IB LOANS" TO MESS
                    PERFORM SEND-MESS
                    MOVE "N" TO VALID-LOAN.

      **************************************
       VERIFY-DATES SECTION.
      **************************************

           IF LN-INTDATE < LN-LOANDATE
              MOVE "INTEREST DATE MAY NOT BE PRIOR TO LOAN DATE"
                     TO MESS
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

      **************************************
       VERIFY-PAYMENTS SECTION.
      **************************************
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

      *LOANS WERE UPDATED WITH NEGATIVE AMOUNT FINANCED
      *WORLD PROBLEM #743
      *NEED AMOUNT FINANCED ON PRECOMP.
           IF LN-LOANTYPE = "P"
              COMPUTE N-WK1 = N-WK1 - LN-INTCHG -
                                 (LN-ORGTERM * LN-MAINTFEE).

           MOVE 1 TO SUB.
       NEXT-FEE.
           SUBTRACT LN-FEEAMT(SUB) FROM N-WK1.
           IF SUB < 10
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
       EXIT-VERIFY-PAYMENTS.
           EXIT.

      **************************************
       VERIFY-INSURANCE SECTION.
      **************************************
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

      **************************************
       RESTORE-TRAILER-REC SECTION.
      **************************************
           MOVE 6 TO WK-KEY.
           PERFORM OPEN-WK-FILE.
           PERFORM READ-WK-FILE.
           PERFORM CLOSE-WK-FILE.
      * INSPECT-X HOLDS FREE FROM BEFORE CALL
           IF INSPECT-X = WK-HHGS-1
              GO TO RESTORE-TRAILER-EXIT.
           MOVE "S  A200201." TO VDU.
           MOVE WK-HHGS-1 TO INSPECT-X.
           MOVE 1 TO VDU-I VDU-J.
       RESTORE-FREE-1A.
           MOVE INSPECT-Z(VDU-I) TO VDUTAB(VDU-J).
           IF VDU-I < 20
              ADD 1 TO VDU-I VDU-J
              GO TO RESTORE-FREE-1A.
           PERFORM SCREENIO.
           MOVE "S  A200202." TO VDU.
           MOVE 21 TO VDU-I.
           MOVE 1 TO VDU-J.
       RESTORE-FREE-1B.
           MOVE INSPECT-Z(VDU-I) TO VDUTAB(VDU-J).
           IF VDU-I < 40
              ADD 1 TO VDU-I VDU-J
              GO TO RESTORE-FREE-1B.
           PERFORM SCREENIO.

       RESTORE-TRAILER-EXIT.
           EXIT.

      **************************************
       CREATE-TRAILER SECTION.
      **************************************
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
           MOVE " " TO F3-COLLATERAL.

      ********************************************
       SET-COLLATERAL-WORK SECTION.
      ********************************************
           PERFORM OPEN-LTC1-FILE.
           MOVE LN-OWNBR  TO LTC-BRNO.
           MOVE LN-ACCTNO TO LTC-ACCTNO.
           MOVE 1         TO LTC-SEQNO.
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

              MOVE LTC-HHGS(1)     TO WK-HHGS-1
              MOVE LTC-HHGS(2)     TO WK-HHGS-2
              MOVE LTC-HHGS(3)     TO WK-HHGS-3
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

       COPY "LIBGB/DATER.CPY".
       COPY "LIBLP/LPLNCL.CPY".
       COPY "LIBLP/LPLTCCL.CPY".
       COPY "LIBLP/LPPCCL.CPY".
       COPY "LIBLP/LPANTI.CPY".
       COPY "LIBLP/LPAMTS.CPY".
       COPY "LIBLP/LPITRM.CPY".
       COPY "LIBLP/LPSPEDL.CPY".
       COPY "LIBLP/LPBAGE.CPY".
       COPY "LIBLP/LPAPRS.CPY".
       COPY "LIBLP/SETCTBL.CPY".
       COPY "LIBLP/LPIEXP.CPY".
       COPY "LIBLP/LPAPRZ.CPY".
       COPY "LIBLP/LPUPER.CPY".
       COPY "LIBLP/LPEDAY.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/GPENV.CPY".

      **************************************
       GET-ACCOUNT-NUMBER SECTION.
      **************************************
           MOVE "Y" TO ACCTNO-WAS-SET-FG.

           PERFORM OPEN-GB-FILE.
           MOVE BR-NO TO GB-BRNO.
           MOVE 1     TO GB-NO.
           PERFORM READ-GB-FILE.
           PERFORM CLOSE-GB-FILE.
           IF IO-FG = 9
              GO TO IO-ERROR.
           IF LN-SMALLOAN = "Y"
              IF GB-NEXT-SMALL = 0
                 ADD 1 TO GB-NEXT-SMALL
              END-IF
              MOVE GB-NEXT-SMALL TO LN-ACCTNO
           ELSE
              IF GB-NEXT-OTHER = 0
                 ADD 1 TO GB-NEXT-OTHER
              END-IF
              MOVE GB-NEXT-OTHER TO LN-ACCTNO.

      **************************************
       COMPUTE-NOREGPY SECTION.
      **************************************
           IF LN-ORGTERM = 1
              MOVE 0 TO LN-NOREGPY
           ELSE
              IF LN-ORGTERM = 2
                 MOVE 0 TO LN-NOREGPY
                 MOVE LN-REGPYAMT TO LN-LASTPYAMT
              ELSE
                 MOVE LN-REGPYAMT TO LN-LASTPYAMT
                 SUBTRACT 2 FROM LN-ORGTERM GIVING LN-NOREGPY.

      **************************************
       CALL-LNSTAT-WINDOW SECTION.
      **************************************
           MOVE "WI/LNSTAT" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH LN-REC EXIT-PATHNAME.
           CANCEL FORM-PROGX.

      *******************************************************
       VERIFY-PASSWORD SECTION.
      *******************************************************
           MOVE " " TO PASSWDW-STAT.
           MOVE "XONIN0" TO PASSWDW-PGM.
           MOVE "PLEASE ENTER THE PASSWORD," TO PASSWDW-PROMPT1.
           MOVE "CL/PASSWD" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH PASSWDW-BUF.
           CANCEL FORM-PROGX.


      *******************************************************
       SET-ACCOUNT-NUMBER SECTION.
      *******************************************************
           PERFORM OPEN-GB-FILE.
           MOVE BR-NO TO GB-BRNO.
           MOVE 1     TO GB-NO.
           PERFORM READ-GB-FILE.
           IF IO-FG = 9
              GO TO IO-ERROR.
           IF LN-SMALLOAN = "Y"
              IF GB-NEXT-SMALL = 0
                 ADD 1 TO GB-NEXT-SMALL
              END-IF
              MOVE GB-NEXT-SMALL TO SET-NUMBER
              ADD 1 TO GB-NEXT-SMALL
           ELSE
              IF GB-NEXT-OTHER = 0
                 ADD 1 TO GB-NEXT-OTHER
              END-IF
              MOVE GB-NEXT-OTHER TO SET-NUMBER
              ADD 1 TO GB-NEXT-OTHER.
           PERFORM REWRITE-GB-FILE.
           PERFORM CLOSE-GB-FILE.
           IF SET-NUMBER NOT = LN-ACCTNO
              MOVE "ACCOUNT NUMBER WILL BE CHANGING!!!!!!!!!!!!!"
                                                        TO MESS
              PERFORM SEND-MESS
              MOVE "SN N060LNNO." TO VDU
              MOVE SET-NUMBER TO VDUNUM0
                                 LN-ACCTNO
              PERFORM SCREENIO
              MOVE "MAKE (N O T E) OF THE NEW NUMBER !!!!!!!!!!!!"
                                                        TO MESS
              PERFORM SEND-MESS.

      ***************************************************
       FORM-RESET SECTION.
      ***************************************************
           MOVE LPLONI-SYSDATE TO XONIN-SYSDATE.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DISPLAY XONIN-SCREEN UPON TCLP-MAIN-WINDOW-HANDLE.
           MOVE XONIN-FORM-FIELDS TO WR-BUF.
           MOVE XONIN-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.


      ***************************************************
       SETUP-LINK-INFO SECTION.
      ***************************************************
           MOVE XONIN-HELP--FILE TO HELP-LINK-FILE.
           MOVE XONIN-HELP--DIR TO HELP-LINK-DIR.

      ***************************************************
       VDU-CONVERT-FIELD SECTION.
      ***************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBLP/XONIN_EVA.CPY".
              COPY "LIBLP/LPCMNU_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      *******************************************************
       MISC-PARAGRAPHS SECTION.
      *******************************************************
       COPY "LIBGB/SYSTEM.CPY".
       COPY "LIBGB/GETENV.CPY".
       TEST-BAL.
           MOVE 9 TO IO-FG.
           IF LN-CURBAL = 0
              IF LN-INTBAL = 0
                 IF LN-LCBAL = 0
                    IF LN-OTHBAL = 0
                       IF LN-POCD = " "
                          MOVE 0 TO IO-FG.
       GET-LC.
           PERFORM OPEN-LC1-FILE.
           PERFORM READ-LC1-FILE.
           PERFORM CLOSE-LC1-FILE.
           IF IO-FG = 9
              MOVE "INVALID" TO LC-DESC.
       GET-CODE.
           MOVE "GC"     TO CDB-TYPE.
           MOVE LN-OWNBR TO CDB-BRANCH.
      *--> SET BEFORE       CDB-CODE.
           PERFORM OPEN-CDB1-FILE.
           PERFORM READ-CDB1-FILE.
           PERFORM CLOSE-CDB1-FILE.
           IF IO-FG = 9
              MOVE " * NOT ON FILE * " TO CDB-DESC.

       GET-GI.
           PERFORM OPEN-GI1-FILE.
           PERFORM READ-GI1-FILE.
           PERFORM CLOSE-GI1-FILE.

       GET-CD.
           PERFORM READ-CD1-FILE.
           IF IO-FG = 9
              MOVE " INVALID CODE" TO CD-DESC.
       SAVE-TRAILER-REC.
           MOVE 6 TO WK-KEY.
           PERFORM OPEN-WK-FILE.
           PERFORM REWRITE-WK-FILE.
           PERFORM CLOSE-WK-FILE.
       GET-DL1.
           PERFORM OPEN-DL1-FILE.
           PERFORM READ-DL1-FILE.
           PERFORM CLOSE-DL1-FILE.
           IF IO-FG = 9
              MOVE " INVALID DEALER" TO DL-SHORT.
       DISPLAY-BULK-CONVERSION.
           MOVE "S  A100TITLE." TO VDU.
           IF BULK-CONVERSION-FG = "B"
              MOVE "BULK PURCH"  TO VDUBUF
           ELSE
              MOVE "CONVERSION" TO VDUBUF.
           PERFORM SCREENIO.
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
           MOVE ELE           TO VDUROW.
           MOVE CD-DESC       TO VDUBUF.
           PERFORM SCREENIO.
       GET-COLLATERAL.
           PERFORM OPEN-LTC1-FILE.
           MOVE LN-OWNBR  TO LTC-BRNO.
           MOVE LN-ACCTNO TO LTC-ACCTNO.
           MOVE 1         TO LTC-SEQNO.
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
           IF BW-SSNO NOT = 0
              MOVE BW-SSNO    TO NUM-BW
              MOVE NUM-BW123  TO ALP-BW123
              MOVE NUM-BW45   TO ALP-BW45
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
           MOVE "UPDATING LOAN MASTER.........." TO LEGEND.
           PERFORM SEND-LEGEND.
       MSG-LINE.
           MOVE MESS TO TMMSG-MESS.
           PERFORM CALL-TMMSG.
       SEND-LEGEND.
           MOVE LEGEND TO WR-BUF.
           MOVE LEGEND-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.
       RESET-LEGEND.
           MOVE " " TO LEGEND.
           PERFORM SEND-LEGEND.
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
       UNPROTECT-FIELDS.
           MOVE " " TO CHFG(11) CHFG(19) CHFG(21)
             CHFG(24) CHFG(35) DATE-FG.
       PROTECT-FIELDS.
           MOVE "N" TO CHFG(5) CHFG(11) CHFG(19) CHFG(21)
             CHFG(24) CHFG(35) DATE-FG.
       CONDITIONAL-SEND-MESS.
           PERFORM SEND-MESS.

      *******************************************************
       VALID-INS-TYPES-ELIG SECTION.
      *******************************************************
           MOVE 0 TO IO-FG.
           IF LN-INSURED = "S" OR "J"
              IF LN-SSNO(2) = 0
                 MOVE 9 TO IO-FG
                 GO TO EXIT-VALID-INS-TYPES-ELIG.

      * READ CURRENT STATE PARAMETER RECORD INORDER
      * TO CHECK INSURANCE COMPANIES (SUBSCRIPTED) = 0.

           PERFORM GET-SP.

      * CHECK IF ADDITIONAL INSURANCES WERE PURCHASED
           IF BR-OTH-INS-LEVELS NOT = "Y"
              MOVE "N" TO CHFG(29) CHFG(30).

           IF LN-INSURED = "N"
              IF LN-INSTYPES NOT = "N"
                 MOVE 9 TO IO-FG
                 GO TO EXIT-VALID-INS-TYPES-ELIG
              ELSE
                 MOVE "N" TO CHFG(25) CHFG(26) CHFG(27) CHFG(28)
                 GO TO EXIT-VALID-INS-TYPES-ELIG.

           MOVE "N" TO CHFG(25) CHFG(26) CHFG(27) CHFG(28).

           IF LN-INSTYPES-LL
              MOVE " " TO CHFG(25)
              PERFORM LEVEL-INSURANCE.
           IF LN-INSTYPES-DL
              MOVE " " TO CHFG(25)
              PERFORM DECREASING-INSURANCE.
           IF LN-INSTYPES-AH
              MOVE " " TO CHFG(26)
              PERFORM ACCIDENT-INSURANCE.
           IF LN-INSTYPES-PP OR LN-INSTYPES-PP2
              MOVE " " TO CHFG(27)
              PERFORM PROPERTY-INSURANCE.
           IF LN-INSTYPES-O1
              MOVE " " TO CHFG(28)
              PERFORM OTHER-1-INSURANCE.
       EXIT-VALID-INS-TYPES-ELIG.
           EXIT.

      *******************************************************
       LEVEL-INSURANCE SECTION.
      *******************************************************
           IF LN-INSURED = "J"
              IF SP-INSCOM(4) = 0
                 MOVE "N" TO CHFG(25)
                 MOVE 9 TO IO-FG
                 MOVE "JOINT LEVEL LIFE NOT ALLOWED" TO MESS
                 PERFORM SEND-MESS
              ELSE
                 MOVE "S  N020250." TO VDU
                 MOVE SP-INSCOM(4) TO VDUNUM0 LN-INSCOMP(1)
                 PERFORM SCREENIO
                 MOVE SP-INSFINCHG(4)  TO LN-INSFINCHG(1)
           ELSE
              IF SP-INSCOM(3) = 0
                 MOVE "N" TO CHFG(25)
                 MOVE 9 TO IO-FG
                 MOVE "LEVEL LIFE NOT ALLOWED" TO MESS
                 PERFORM SEND-MESS
              ELSE
                 MOVE "S  N020250." TO VDU
                 MOVE SP-INSCOM(3) TO VDUNUM0 LN-INSCOMP(1)
                 PERFORM SCREENIO
                 MOVE SP-INSFINCHG(3)  TO LN-INSFINCHG(1).

           IF BR-AH-AND-CL = "Y"
              IF LN-INSTYPES-AH
                 IF NOT(LN-INSTYPES-DL OR LN-INSTYPES-LL)
                    MOVE "N" TO CHFG(27) CHFG(28)
                    MOVE 9 TO IO-FG
                    MOVE "MUST HAVE LIFE AND A&H" TO MESS
                    PERFORM SEND-MESS.

      *******************************************************
       DECREASING-INSURANCE SECTION.
      *******************************************************
           IF LN-INSURED = "J"
              IF SP-INSCOM(2) = 0
                 MOVE "N" TO CHFG(25)
                 MOVE 9 TO IO-FG
                 MOVE "JOINT DECLINING LIFE NOT ALLOWED" TO MESS
                 PERFORM SEND-MESS
              ELSE
                 MOVE "S  N020250." TO VDU
                 MOVE SP-INSCOM(2) TO VDUNUM0 LN-INSCOMP(1)
                 PERFORM SCREENIO
                 MOVE SP-INSFINCHG(2)  TO LN-INSFINCHG(1)
           ELSE
              IF SP-INSCOM(1) = 0
                 MOVE "N" TO CHFG(25)
                 MOVE 9 TO IO-FG
                 MOVE "DECLINING LIFE NOT ALLOWED" TO MESS
                 PERFORM SEND-MESS
              ELSE
                 MOVE "S  N020250." TO VDU
                 MOVE SP-INSCOM(1) TO VDUNUM0 LN-INSCOMP(1)
                 PERFORM SCREENIO
                 MOVE SP-INSFINCHG(1)  TO LN-INSFINCHG(1).

           IF BR-AH-AND-CL = "Y"
              IF LN-INSTYPES-AH
                 IF NOT(LN-INSTYPES-DL OR LN-INSTYPES-LL)
                    MOVE "N" TO CHFG(27) CHFG(28)
                    MOVE 9 TO IO-FG
                    MOVE "MUST HAVE LIFE AND A&H" TO MESS
                    PERFORM SEND-MESS.

      *******************************************************
       ACCIDENT-INSURANCE SECTION.
      *******************************************************
           IF SP-INSCOM(5) = 0
              MOVE "N" TO CHFG(26)
              MOVE 9 TO IO-FG
              MOVE "A & H NOT ALLOWED" TO MESS
              PERFORM SEND-MESS
           ELSE
              MOVE "S  N020260." TO VDU
              MOVE SP-INSCOM(5) TO VDUNUM0 LN-INSCOMP(2)
              PERFORM SCREENIO
              MOVE SP-INSFINCHG(5)  TO LN-INSFINCHG(2).

           IF BR-AH-AND-CL = "Y"
              IF LN-INSTYPES-AH
                 IF NOT(LN-INSTYPES-DL OR LN-INSTYPES-LL)
                    MOVE "N" TO CHFG(27) CHFG(28)
                    MOVE 9 TO IO-FG
                    MOVE "MUST HAVE LIFE AND A&H" TO MESS
                    PERFORM SEND-MESS.

      *******************************************************
       PROPERTY-INSURANCE SECTION.
      *******************************************************
           IF LN-INSTYPES-PP
              IF SP-INSCOM(6) = 0
                 MOVE "N" TO CHFG(27)
                 MOVE 9 TO IO-FG
                 MOVE "PROPERTY NOT ALLOWED" TO MESS
                 PERFORM SEND-MESS
              ELSE
                 MOVE "S  N020270." TO VDU
                 MOVE SP-INSCOM(6) TO VDUNUM0 LN-INSCOMP(3)
                 PERFORM SCREENIO
                 MOVE SP-INSFINCHG(6)  TO LN-INSFINCHG(3)
           ELSE
              IF SP-INSCOM(7) = 0
                 MOVE "N" TO CHFG(27)
                 MOVE 9 TO IO-FG
                 MOVE "PROPERTY NOT ALLOWED" TO MESS
                 PERFORM SEND-MESS
              ELSE
                 MOVE "S  N020270." TO VDU
                 MOVE SP-INSCOM(7) TO VDUNUM0 LN-INSCOMP(3)
                 PERFORM SCREENIO
                 MOVE SP-INSFINCHG(7)  TO LN-INSFINCHG(3).

      *******************************************************
       OTHER-1-INSURANCE SECTION.
      *******************************************************
           IF SP-INSCOM(8) = 0
              MOVE "N" TO CHFG(28)
              MOVE 9 TO IO-FG
              MOVE "OTHER 1 INS. NOT ALLOWED" TO MESS
              PERFORM SEND-MESS
           ELSE
              MOVE "S  N020280." TO VDU
              MOVE SP-INSCOM(8) TO VDUNUM0 LN-INSCOMP(4)
              PERFORM SCREENIO
              MOVE SP-INSFINCHG(8)  TO LN-INSFINCHG(4).

      *******************************************************
       IO-ROUTINE SECTION.
      *******************************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBLP/LPSPGS_SQL.CPY".
       COPY "LIBLP/LPPCGS_SQL.CPY".
       COPY "LIBLP/LPNOGS_SQL.CPY".
       COPY "LIBLP/LPLTCGS_SQL.CPY".
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
       COPY "LIBGB/GBGBGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBGBIN.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBGL/GLGI1RN.CPY".
       COPY "LIBLP/LPDL1RN.CPY".
       COPY "LIBLP/LPWKI.CPY".
       COPY "LIBLP/LPNO1IN.CPY".
       COPY "LIBLP/LPLTA1IN.CPY".
       COPY "LIBLP/LPLTC1IN.CPY".
       COPY "LIBLP/LPPC1IN.CPY".
       COPY "LIBLP/LPLC1RN.CPY".
       COPY "LIBLP/LPLN1IN.CPY".
       COPY "LIBLP/LPLP1IN.CPY".
       COPY "LIBLP/LPSP1RN.CPY".
       COPY "LIBLP/LPBW1RN.CPY".
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
       STOP-RUN.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY XONIN-SCREEN.
           EXIT PROGRAM.
