       IDENTIFICATION DIVISION.
       PROGRAM-ID.      GPMAN2.
       DATE-WRITTEN.    070395.
      ******************************************************************
      *  DIR : GB
      *  DESC: GLOBAL PARAMETER MAINT - PAGE 2
      *        RELATIVE RECORD 1
      *
      * REV:
      *  MJD 070695 ADDED GP-FED-TAX-ID.
      *  BLL 071495 ADDED GP-CORP-NAME, GP-SHORT-NAME
      *  JTG 072095 ADDED GP-BANK-DEPOSIT-GL
      *  JTG 072595 ADDED GP-AGE-BUCKET-CD LOGIC
      *  JTG 081095 ADDED GP-THE-ONLY-DF-SRCD
      *  BAH 081495 ADDED GP-ALLOW-BATCH-CHECKS
      *  MJD 082495 ADDED GP-CASH-METHOD-ONLY
      *                   GP-IB-ACCRUALS-DONE.
      *  BLL 082895 ADDED GP-SPL-RETAIN-MONTHS
      *  KEC 082895 ADDED GP-NO-LC-IN-ARREARAGE-FG
      *  JTG 092995 ADDED GP-DEFAULT-O3-INS-COMPANY
      *  MJD 121195 ADDED GP-STOP-ON-PRIOR-FG.
      *  BAH 032796 ADDED GP-DEFAULT-ACCRATE
      *  MJD 041996 ADDED GP-DLR-DISP-TYPE.
      *  JTG 052396 ADDED GP-ALLOW-PI-WITH-SE-BAL
      *  BAH 071596 ADDED GP-ALLOW-MORE-COLLECT, PR# E128
      *  BLV 101496 FIXED SIZE OF ORIG & CHANGED-REC
      *  JTG 121196 ADDED GP-LOAN-CONVERSION-UPD-FG
      *  JTG 021897 ADDED GP-FORM-CHG-CD, BHFC
      *  BLV 031097 ADDED GP-PROGRP-PRINT-SOLD
      *  BAH 031397 ADDED GP-BW-HOUSING-CODE
      *  MJD 050997 ADDED GP-BW-CREDIT-SCORE
      *   CS 051297 CHANGED GP-PROGRP-PRINT-SOLD TO GP-PROGRP-PRINT-REDEEM
      *  MJD 071197 ADDED GP-PROGRP-DL-CONT
      *   CS 071897 ADDED GP-DLRVPRIN-1, GP-DLRVPRIN-2
      *  JTG 082097 ADDED GP-ADV-TO-1ST-NO-CORR WORLD A90
      *  JTG 082697 ADDED GP-I-AM-WORLD, A90
      *  MJD 082997 ADDED GP-COMPANY-FLAG WITH GP-I-AM-WORLD AS AN 88 LEVEL
      *  JTG 092497 ADDED TO GP-INTERBR-AR-AP-FULLGL TO ALLOW DETAIL FROM/TO
      *             BRANCH ON INTERBRANCH PAYMENTS
      *  BAH 032098 ADDED GP-ASK-HARD-COPY-MEMOS SO LONPP0 WILL POP A WINDOW
      *             TO ASK IF YOU WANT MEMOS TO PRINT, WORLD # 183
      *  BAH 040798 ADDED GP-RENEWAL-COLLATERAL FOR WORLD TO BRING COLLATERAL
      *              FORWARD FROM RENEWED LOAN, IT DID IN A90
      *  BLV 052298 ADDED GP-PROGRP-LN-CLASS(TABLE OF 3) & GP-PROGRP-SF-CLASS
      *             (TABLE OF 4): WHICH CLASSES TO PRINT ON PROGRP
      *  JTG 063098 ADDED EDIT AGAINST STANDARD SRCD'S FOR GP-THE-ONLY-DF-SRCD
      *  JTG 082198 ADDED GP-RENEW-DISC-UNERN FOR MERCURY TO INDICATED IF
      *             THE NEW LOAN SHOULD RE-ESTABLISH THE UNEARNED DISCOUNT
      *             OF THE LOAN RENEWED
      *  MJD 990304 ADDED GP-REFNO-VERIFY
      *  GLF 990322 ADDED GP-ACTIONCD-VERIFY
      *  BAH 990507 ADDED GP-I-AM-REGACC, REGIONAL ACCEPTANCE, NC
      *  JTG 990618 ADDED GP-I-AM-REGMGM, REGIONAL MANAGEMENT, SC
      *  MG  991006 FIXED VDUSTATUS/IO-FG/STAT PER DEVELOPMENT MEETING.
      *  JTG 000125 ADDED GP-I-AM-WISC, WISCONSION FINANCE, WI
      *  MJD 000228 ADDED "A" OPTION TO GP-ADV-TO-1ST-NO-CORR. IF THE DATE
      *             OF THE LOAN IS THE 26TH-31ST THEN ADVASNCE THE NEXT
      *             PAYMENT DUE DATE TO THE FIRST OF THE SECOND MONTH
      *             FOLLOWING THE DATE OF THE LOAN.
      *  BAH 001222 ADDED GP-I-AM-SUNBELT, SUNBELT SC PR# 1387
      *  BAH 010112 ADDED GP-I-AM-MODERN
      *  BAH 010311 ADDED GP-I-AM-SOAUTO, SOUTHERN AUTOMOTIVE FINANCE
      *  BAH 020604 ADDED GP-I-AM-MIDATL, MIDATLANTIC
      *  BLV 040728 ALLOW GP-REFNO-VERIFY TO HAVE A VALUE OF "A" FOR
      *             ONLY ALLOWING ALPHA OR NUMBERS, NO SPECIAL
      *             CHARACTERS, IN REFNO, CENTRIX PR# 2350
      *  CS  040809 ALLOW GP-REFNO-VERIFY TO HAVE A VALUE OF "T". IF 'T'
      *             ONLY ALLOW A REFCD DEFINED FOR THAT POSTING CODE. CREATE
      *             VALID REFNO CODES IN TCMAIN [LENDMARK PR# 2353]
      *  BAH 041026 ADDED GP-I-AM-MIDWEST FOR O2MORP CHANGE, MIDWEST PR# 2397
      *  BAH 060214 ADDED GP-I-AM-AUTOMOTIVE-CREDIT, "ACC " PR# 2704
      *  BAH 060313 ADDED GP-I-AM-FUTURE, "FUTR" PR# 2735
      *  BAH 070418 ADDED GP-I-AM-UNITED, "UNIT" PR# 2959
      *  JTG 080129 ADDED GP-I-AM-FINAUTO, "FIAU" PR# 3141
      *  BAH 111017 ADDED GP-I-AM-CARFIN, "CARF" PR# 3887
      *  BAH 121005 ADDED GP-I-AM-AMER-ACC, "AMAC" PR# 4117
      *  MJD 121025 A30 CONVERSION COMPLETE.
      *  BAH 131213 ADDED GP-I-AM-KSTREET, "KSTR" PR# 4428
      *  BLM 131216 ADDED GP-I-AM-BLACKHAWK, "BHWK" PR# 4414
      *  BAH 150331 ADDED GP-I-AM-FIRST-METRO, "FMET" PR# 4671
      *  BAH 151030 ADDED GP-I-AM-HONOR, "HONR", PR#4898
      *  BAH 151208 ADDED GP-I-AM-BARNETT, "BARN", PR#4914
      *  BAH 161017 ADDED GP-I-AM-MAS, "MAS ", PR#5186
      *  BAH 161114 ADDED GP-I-AM-CFSC, "CFSC", PR#5328
      *  BAH 180515 ADDED ERRLOG
      *  BAH 190123 SPLIT CDFILE
      *  KEC 2019-0123 A32 - GPFILE GP-REC MODIFICATION
      *                ADDED PAGE 5 (GB/GPMAN5) FOR ACCOUNTS PAYABLE (AP)
      **************************************************************************
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
      *    W O R K I N G - S T O R A G E     S E C T I O N
      *
      ******************************************************************
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)GB/GPMAN2.CBL S35 02/22/24-14:43:16 >".
       COPY "LIBGB/GB01GP.CPY".
       COPY "LIBGB/GB01GP_SQL.CPY".
       COPY "LIBGB/GBWSGP.CPY".
       COPY "LIBGL/GL01GL.CPY".
       COPY "LIBGL/GL01GL_SQL.CPY".
       COPY "LIBGL/GLWSGL.CPY".
       COPY "LIBLP/LP01CD.CPY".
       COPY "LIBLP/LP01CD_SQL.CPY".
       COPY "LIBLP/LPWSCD.CPY".

      ******************************************************************
      *    P R O G R A M     W O R K E R S
      ******************************************************************
       01  SUB             PIC 999     VALUE 0.
       01  ONE             PIC S9.
       01  ELE             PIC 9(2).
       01  ACTION.
           03  CHKEY       PIC X(4).
           03  CHNO        PIC 9(2).
       01  FORM-NUM REDEFINES ACTION.
           03  FORM-KEY    PIC X(4).
           03  FORM-P      PIC X.
           03  FORM-SEL    PIC 9.

       01  IO-TYPE            PIC X(3).
       01  NEW-FLAG           PIC 9.
       01  FIRST-TIME         PIC 9       VALUE 0.

      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    ORIGINAL RECORD GPOR-REC
      *    CHANGED RECORD GPCH-REC
      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       COPY "LIBGB/GB01GP.CPY" REPLACING LEADING "GP" BY "GPOR".
       COPY "LIBGB/GB01GP.CPY" REPLACING LEADING "GP" BY "GPCH".

      ******************************************************************
      *    DESC: LOG-MAINTENANCE-CHGS WORKERS
      ******************************************************************
       01  LMC-DETAIL                  PIC X(55)      VALUE SPACES.
       01  FILLER                      REDEFINES LMC-DETAIL.
           03  LMC-FIELD               PIC X(03).
           03  FILLER                  PIC X(01).
           03  LMC-DESC                PIC X(14).
           03  FILLER                  PIC X(01).

           03  LMC-ORG-TEXT            PIC X(16).
           03  LMC-ORG-AMT             REDEFINES LMC-ORG-TEXT
                                       PIC ZZZ,ZZZ,ZZ9.99-.
           03  LMC-ORG-NUM             REDEFINES LMC-ORG-TEXT
                                       PIC ZZZZZZZZZ9-.

           03  LMC-TO-X                PIC X(04).

           03  LMC-CHG-TEXT            PIC X(16).
           03  LMC-CHG-AMT             REDEFINES LMC-CHG-TEXT
                                       PIC ZZZ,ZZZ,ZZ9.99-.
           03  LMC-CHG-NUM             REDEFINES LMC-CHG-TEXT
                                       PIC ZZZZZZZZZ9-.
      ******************************************************************
      *    C O N A M E
      ******************************************************************
       01  GPMAIN-LINK.
          03  SAVEKEY            PIC 99.
          03  NEW-REC            PIC X.
          03  ERRCD              PIC X.
          03  FORMNUM            PIC 9.
          03  GPMAIN-WINDOW-HANDLE PIC X(10).


      ******************************************************************
      *    S C R E E N    D I S P L A Y    B U F F E R
      ******************************************************************
       01  DISPLAY-BUF     VALUE SPACES.
           03  F01         PIC X.
           03  F02         PIC XX.
           03  F021        PIC X(35).
           03  F03         PIC X.
           03  F04         PIC X.
           03  F05         PIC X.
           03  F06         PIC X.
           03  F07         PIC X.
           03  F08         PIC X.
           03  F09         PIC 99.
           03  F10         PIC X.
           03  F11         PIC 999.999.
           03  F12         PIC X.
           03  F13         PIC X.
           03  F14         PIC X.
           03  F15         PIC X.
           03  F16         PIC X.
           03  F17         PIC X.
           03  F18         PIC X.
           03  F19         PIC X.
           03  F20         PIC X.
           03  F21         PIC X.
           03  F22         PIC X.
           03  F23         PIC X.
           03  F24         PIC X.
           03  F25         PIC X.
           03  F26         PIC X(4).
           03  F27         PIC X.
           03  F28         PIC X.
           03  F29         PIC X.
           03  F30         PIC X.
           03  F31         PIC Z9.
           03  F311        PIC Z9.
           03  F312        PIC Z9.
           03  F32         PIC Z9.
           03  F321        PIC Z9.
           03  F322        PIC Z9.
           03  F323        PIC Z9.
           03  F33         PIC X.
           03  F34         PIC X(3).
           03  F35         PIC X.
           03  F36         PIC X.
      *-----------------------------------------------------------------
       01  DISPLAY-LIST.
           03  FILLER      PIC X(32) VALUE
           "010 020 021 030 040 050 060 070 ".
           03  FILLER      PIC X(32) VALUE
           "080 090 100 110 120 130 140 150 ".
           03  FILLER      PIC X(32) VALUE
           "160 170 180 190 200 210 220 230 ".
           03  FILLER      PIC X(28) VALUE
           "240 250 260 270 280 290 300 ".
           03  FILLER      PIC X(28) VALUE
           "310 311 312 320 321 322 323 ".
           03  FILLER      PIC X(16) VALUE
           "330 340 350 360.".


      ******************************************************************
      *    E L E M E N T    S P E C I F I C A T I O N S
      ******************************************************************
       78  MAX                      VALUE 36.
       01  SPEC-TABLE.
      * 01. AGEING BUCKET CODE
           03  FILLER      PIC X(8)    VALUE "E NA010 ".
      * 02. THE ONLY SALES FIN SRCD
           03  FILLER      PIC X(8)    VALUE "E  A020 ".
      * 03. ALLOW BATCH CHECKS
           03  FILLER      PIC X(8)    VALUE "E  A010 ".
      * 04. ALLOW INPUT OF LN-DLRVPRIN(3)
           03  FILLER      PIC X(8)    VALUE "E  A010 ".
      * 05. ALLOW INPUT OF LN-DLRVCHGS(3)
           03  FILLER      PIC X(8)    VALUE "E  A010 ".
      * 06. CASH METHOD ONLY FLAG.
           03  FILLER      PIC X(8)    VALUE "E  A010 ".
      * 07. IB ACCRUALS ONLY FLAG.
           03  FILLER      PIC X(8)    VALUE "E  A010 ".
      * 08. NO LATE CHARGE IN ARREARAGE
           03  FILLER      PIC X(8)    VALUE "E  A010 ".
      * 09. DEFAULT 03 INS. COMPANY NO
           03  FILLER      PIC X(8)    VALUE "ENNN020 ".
      * 10. STOP ON PRIOT ACCOUNT FG
           03  FILLER      PIC X(8)    VALUE "E  A010 ".
      * 11. DEFAULT ACCEL RATE
           03  FILLER      PIC X(8)    VALUE "ENNN033 ".
      * 12. DEALER DISPLAY-TYPE
           03  FILLER      PIC X(8)    VALUE "ENNA010 ".
      * 13. AUTP REPO WINDOW
           03  FILLER      PIC X(8)    VALUE "ENNA010 ".
      * 14. ALLOW 'PI' ON 'SE' ACCOUNT WITH BAL
           03  FILLER      PIC X(8)    VALUE "ENNA010 ".
      * 15. ALLOW MORE THAN 1 COLLECTION FORM REQUEST
           03  FILLER      PIC X(8)    VALUE "ENNA010 ".
      * 16. IN LOAN CONVERSION WHAT TO UPDATE FG
           03  FILLER      PIC X(8)    VALUE "ENNA010 ".
      * 17. FORM CHARGE CODE
           03  FILLER      PIC X(8)    VALUE "ENNA010 ".
      * 18. PRINT REPO SOLD ON PROGRESS REPORT
           03  FILLER      PIC X(8)    VALUE "ENNA010 ".
      * 19. USE BORROWER HOUSING CODES
           03  FILLER      PIC X(8)    VALUE "ENNA010 ".
      * 20. FORCE ENTRY OF BORROWER CREDIT SCORE
           03  FILLER      PIC X(8)    VALUE "ENNA010 ".
      * 21. PRINT CONTRACTUAL 150+ FOR DIRECT LOAND ON PROGRESS REPORT
           03  FILLER      PIC X(8)    VALUE "ENNA010 ".
      * 22. ALLOW RESERVE PRIN 1
           03  FILLER      PIC X(8)    VALUE "ENNA010 ".
      * 23. ALLOW RESERVE PRIN 2
           03  FILLER      PIC X(8)    VALUE "ENNA010 ".
      * 24. ALLOW RESERVE PRIN 3
           03  FILLER      PIC X(8)    VALUE "ENNA010 ".
      * 25. ADVANCE TO FIRST OF MONTH IF NO CORRESPONDING DAY
           03  FILLER      PIC X(8)    VALUE "ENNA010 ".
      * 26. I AM WORLD FLAG, USED FOR SPECIAL THINGS WORLD DOES THAT
      *                      SHOULD NOT BE OPTIONS
           03  FILLER      PIC X(8)    VALUE "ENNA040 ".
      * 27. GP-INTERBR-AR-AP-FULLGL
           03  FILLER      PIC X(8)    VALUE "ENNA010 ".
      * 28. GP-SKIP-BI-UPDATE
           03  FILLER      PIC X(8)    VALUE "ENNA010 ".
      * 29. GP-ASK-HARD-COPY-MEMOS
           03  FILLER      PIC X(8)    VALUE "ENNA010 ".
      * 30. GP-RENEWAL-COLLATERAL
           03  FILLER      PIC X(8)    VALUE "ENNA010 ".
      * 31. GP-PROGRP-LN-CLASS (TABLE OF 3)
           03  FILLER      PIC X(8)    VALUE "ENNN020 ".
      * 32. GP-PROGRP-SF-CLASS (TABLE OF 4)
           03  FILLER      PIC X(8)    VALUE "ENNN020 ".
      * 33. GP-RENEW-DISC-UNERN
           03  FILLER      PIC X(8)    VALUE "ENNA010 ".
      * 34. GP-NOTES-EOD-BATCH-ID
           03  FILLER      PIC X(8)    VALUE "ENNA030 ".
      * 35. GP-REFNO-VERIFY
           03  FILLER      PIC X(8)    VALUE "ENNA010 ".
      * 36. GP-ACTIONCD-VERIFY
           03  FILLER      PIC X(8)    VALUE "ENNA010 ".

       01  SPEC-TABLE2     REDEFINES SPEC-TABLE.
           02  SPEC-REC    OCCURS MAX TIMES.
               05  SPEC    PIC X(7).
               05  CHFG    PIC X.

       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBGB/GPMAN2_DEF.CPY".
       COPY "LIBGB/GPMAN2_WKS.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/ACCESSW.CPY".

      ******************************************************************
      *     P R O C E D U R E    D I V I S I O N
      ******************************************************************
       COPY "LIBGB/GETFMW.CPY".
       01 FORM-LINK-AREA    PIC X(2000).
       SCREEN SECTION.
       COPY "LIBGB/GPMAN2_SCN.CPY".
       PROCEDURE DIVISION
            USING FORM-PATHNAME FORM-LINK-AREA EXIT-PATHNAME.

      ******************************************************************
      *     M A I N     P R O G R A M      M O D U L E
      ******************************************************************
       MAIN-PROGRAM SECTION.

           PERFORM SQL-CONNECT.

      D    STOP MESS.
           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "GPMAN2" TO VDU-FORM-NAME.
           MOVE FORM-LINK-AREA TO GPMAIN-LINK.
       MAIN-MODULE.
           PERFORM FORM-RESET.
           IF FIRST-TIME = 0
              MOVE 1 TO FIRST-TIME.

           PERFORM REC-GP-MODULE.
           IF IO-TYPE = "END"
              MOVE "X" TO ERRCD
              GO TO END-ROUTINE.

           PERFORM ENTRY-MODULE.
           IF IO-TYPE = "END"
              MOVE "X" TO ERRCD
              GO TO END-ROUTINE.
           IF IO-TYPE = "RWR"
              PERFORM WRITE-MODULE
              MOVE "X" TO ERRCD
              GO TO END-ROUTINE.
           IF IO-TYPE = "PAG"
              PERFORM WRITE-MODULE.

           IF NEW-REC = "Y"
              MOVE "P" TO FORM-P
              MOVE 3 TO FORM-SEL.

           IF FORM-SEL NOT = 0
              IF FORM-P = "P"
                 MOVE GP-KEY TO SAVEKEY
                 MOVE FORM-SEL TO FORMNUM
                 GO TO END-ROUTINE.

           MOVE "N" TO NEW-REC.

           GO TO MAIN-MODULE.

      ******************************************************************
       REC-GP-MODULE SECTION.
      ******************************************************************
       REC-GP-MODULE-NO.
           MOVE SPACES TO IO-TYPE.

           IF SAVEKEY NOT = "  "
              MOVE SAVEKEY TO GP-KEY
              MOVE "  " TO SAVEKEY
              GO TO READ-GLOBAL.

           MOVE 0 TO NEW-FLAG.
           MOVE 1 TO GP-KEY.
      *-----------------------------------------------------------------
       READ-GLOBAL.
           PERFORM OPEN-GP-FILE.
           PERFORM READ-GP-FILE.
           PERFORM CLOSE-GP-FILE.

           IF IO-BAD
              MOVE "END" TO IO-TYPE
              GO TO END-REC-GP-MODULE.

           MOVE GP-REC TO GPOR-REC.
           PERFORM DISPLAY-FIELDS.
           MOVE 0 TO NEW-FLAG.
           IF NEW-REC = "Y"
              MOVE 1 TO NEW-FLAG.

       END-REC-GP-MODULE.
           EXIT.

      ******************************************************************
      *
      *    E N T R Y - M O D U L E
      *
      *    NEW-FLAG = 0 - MODIFY EXISTING RECORD
      *               1 - INITIAL NEW RECORD ENTRY
      *               2 - MODIFY NEW RECORD
      *
      ******************************************************************
       ENTRY-MODULE SECTION.
           MOVE SPACES TO IO-TYPE.
           MOVE 1 TO ONE.
           MOVE 0 TO ELE.

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
           MOVE "R RA060SEL." TO VDU.
           PERFORM SCREENIO.

           IF F1-KEY
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.

           IF F7-KEY
              MOVE "RWR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.

           IF NOT ENTER-KEY
              GO TO MODIFY-ROUTINE.

           MOVE VDUBUF TO ACTION.
           IF FORM-KEY = " " AND FORM-P = "P" AND FORM-SEL NUMERIC
              IF NOT (FORM-SEL = 1 OR 3 OR 4)
                 GO TO MODIFY-ROUTINE
              ELSE
                 MOVE "PAG" TO IO-TYPE
                 GO TO EXIT-ENTRY-MODULE.

           INSPECT ACTION REPLACING ALL SPACES
              BY ZEROS.

           IF CHNO NOT NUMERIC
              GO TO MODIFY-ROUTINE.

           IF CHNO = 0
              GO TO MODIFY-ROUTINE.

           IF CHNO > MAX
              GO TO MODIFY-ROUTINE.

           MOVE CHNO TO ELE.

       ENTER-ELE.
           IF CHKEY NOT = ZEROS
              IF CHFG (ELE) = " "
                 IF NEW-FLAG NOT = 1
                    GO TO MODIFY-ROUTINE.

           IF CHFG(ELE) = "N"
              IF NEW-FLAG = 1
                 GO TO CREATE-ROUTINE
            ELSE
              IF CHKEY NOT = EXT-CONAME-DAILY-PASSWORD
                 GO TO MODIFY-ROUTINE.

           MOVE SPEC(ELE) TO VDU.
           MOVE ELE TO VDUROW.
           MOVE 0 TO VDUCOL.
           MOVE "." TO VDUDECIMAL.
           PERFORM SCREENIO.

       TEST-F7.
           IF F7-KEY
              MOVE "END" TO IO-TYPE
              GO TO END-ENTRY-MODULE.

       TEST-F4.
           IF UP-KEY
             IF NEW-FLAG = 1
              MOVE -1 TO ONE
              GO TO CREATE-ROUTINE.

       TEST-F5.
           IF F5-KEY
              MOVE 1 TO ONE
              GO TO CREATE-ROUTINE.

       TEST-F1.
           IF F1-KEY
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.

           IF NOT ENTER-KEY
              GO TO ENTER-ELE.

           MOVE 1 TO ONE.

       GOTO-FIELD.
           GO TO F-01 F-02 F-03 F-04 F-05 F-06 F-07
                 F-08 F-09 F-10 F-11 F-12 F-13 F-14
                 F-15 F-16 F-17 F-18 F-19 F-20 F-21
                 F-22 F-23 F-24 F-25 F-26 F-27 F-28
                 F-29 F-30 F-31 F-32 F-33 F-34 F-35
                 F-36
               DEPENDING ON ELE.

           GO TO END-ENTRY-MODULE.

       F-01.
           MOVE VDUBUF TO GP-AGE-BUCKET-CD.

           IF NOT GP-AGE-BUCKET-CD-VALID
              MOVE "ENTER ' ' OR 'A' ONLY!" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.

           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-02.
           IF VDUBUF = GP-THE-ONLY-SF-SRCD
              GO TO CREATE-ROUTINE.

           MOVE VDUBUF TO GP-THE-ONLY-SF-SRCD.

           IF GP-THE-ONLY-SF-SRCD = " "
              MOVE "S  A035021." TO VDU
              PERFORM SCREENIO
              GO TO CREATE-ROUTINE
           ELSE
           IF GP-THE-ONLY-SF-SRCD = "PB" OR "RN" OR "SC"
                                         OR "RB" OR "RO"
              STRING "DEFAULT CODE OF 'PB' 'RN' 'SC' 'RB' 'RO'"
                     " IS NOT ALLOWED"
                 DELIMITED BY SIZE INTO MESS
              PERFORM SEND-MESS
              MOVE " " TO GP-THE-ONLY-SF-SRCD
              GO TO ENTER-ELE
           ELSE
              MOVE "SR"                TO CD-TYPE
              MOVE GP-THE-ONLY-SF-SRCD TO CD-CODE
              PERFORM GET-CD
              IF IO-GOOD
                 MOVE "SOURCE CODE ON FILE, INVALID ENTRY"
                                                      TO MESS
                 PERFORM SEND-MESS
                 MOVE " " TO GP-THE-ONLY-SF-SRCD
                 GO TO ENTER-ELE.

      *-----------------------------------------------------------------
       F-021.
           MOVE "E  A350021." TO VDU.
           PERFORM SCREENIO.

           IF UP-KEY
              GO TO ENTER-ELE.

           IF NOT ENTER-KEY
              GO TO F-021.

           PERFORM CLEAR-CD-FILE.
           MOVE "SR"                TO CD-TYPE.
           MOVE GP-THE-ONLY-SF-SRCD TO CD-CODE.
           MOVE VDUBUF              TO CD-DESC.
           PERFORM OPEN-CD1-FILE.
           PERFORM WRITE-CD1-FILE.
           PERFORM CLOSE-CD1-FILE.

           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-03.
           MOVE VDUBUF TO GP-ALLOW-BATCH-CHECKS.

           IF GP-ALLOW-BATCH-CHECKS NOT = "Y" AND NOT = "N"
              MOVE "ENTER 'Y' OR 'N' ONLY!" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.

           GO TO CREATE-ROUTINE.

       F-04.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
      * CHANGED TO SEPERATE FLAG FOR RESERVE PRIN1, PRIN2, PRIN3 7/18/97
      *F-04.
      *    MOVE VDUBUF TO GP-DLRVPRIN-3-FG.

      *    IF GP-DLRVPRIN-3-FG NOT = "Y" AND NOT = "N"
      *       MOVE "ENTER 'Y' OR 'N' ONLY!" TO MESS
      *       PERFORM SEND-MESS
      *       GO TO ENTER-ELE.

      *    GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-05.
           MOVE VDUBUF TO GP-DLRVCHGS-3-FG.

           IF GP-DLRVCHGS-3-FG NOT = "Y" AND NOT = "N"
              MOVE "ENTER 'Y' OR 'N' ONLY!" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.

           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-06.
           MOVE VDUBUF TO GP-CASH-METHOD-ONLY.

           IF GP-CASH-METHOD-ONLY NOT = "Y" AND NOT = "N"
              MOVE "ENTER 'Y' OR 'N' ONLY!" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.

           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-07.
           MOVE VDUBUF TO GP-IB-ACCRUALS-DONE.

           IF GP-IB-ACCRUALS-DONE NOT = "Y" AND NOT = "N"
              MOVE "ENTER 'Y' OR 'N' ONLY!" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.

           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-08.
           MOVE VDUBUF TO GP-NO-LC-IN-ARREARAGE-FG.
           IF NOT (GP-NO-LC-IN-ARREARAGE-FG = "Y" OR "N")
              MOVE "ENTER 'Y' OR 'N' ONLY!" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-09.
           MOVE VDUNUM0 TO GP-DEFAULT-O3-INS-COMPANY.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-10.
           MOVE VDUBUF TO GP-STOP-ON-PRIOR-FG.
           IF GP-STOP-ON-PRIOR-FG NOT = "Y" AND NOT = "N"
              MOVE "ENTER 'Y' OR 'N' ONLY!" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.

       F-11.
           MOVE VDUNUM3 TO GP-DEFAULT-ACCRATE.
           GO TO CREATE-ROUTINE.

       F-12.
           MOVE VDUBUF TO GP-DLR-DISP-TYPE.
           IF GP-DLR-DISP-TYPE NOT = "D" AND NOT = "C" AND NOT = " "
              MOVE "ENTER 'D'-DISCOUNT OR 'C'-COMMISSION ONLY!" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.

       F-13.
           MOVE VDUBUF TO GP-AUTO-REPO-WINDOW.
           IF GP-AUTO-REPO-WINDOW NOT = "Y" AND NOT = "N"
              MOVE "ENTER 'Y' OR 'N' ONLY!" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.

       F-14.
           MOVE VDUBUF TO GP-ALLOW-PI-WITH-SE-BAL.
           IF NOT (GP-ALLOW-PI-WITH-SE-BAL = "Y" OR "N")
              MOVE "ENTER 'Y' OR 'N' ONLY!" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.

       F-15.
           MOVE VDUBUF TO GP-ALLOW-MORE-COLLECT.
           IF NOT (GP-ALLOW-MORE-COLLECT = "Y" OR "N")
              MOVE "ENTER 'Y' OR 'N' ONLY!" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.

       F-16.
           MOVE VDUBUF TO GP-LOAN-CONVERSION-UPD-FG.
           IF NOT (GP-LOAN-CONVERSION-UPD-FG = "Y" OR "N" OR "S" OR "G")
              MOVE "ENTER 'Y' OR 'N' OR 'S' OR 'G' ONLY!" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       F-17.
           MOVE VDUBUF TO GP-FORM-CHG-CD.
           IF NOT (GP-FORM-CHG-CD = "Y" OR "N")
              MOVE "ENTER 'Y' OR 'N' ONLY!" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       F-18.
           MOVE VDUBUF TO GP-PROGRP-PRINT-REDEEM.
           IF NOT (GP-PROGRP-PRINT-REDEEM = "Y" OR "N")
              MOVE "ENTER 'Y' OR 'N' ONLY!" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       F-19.
           MOVE VDUBUF TO GP-BW-HOUSING-CODE.
           IF NOT (GP-BW-HOUSING-CODE = "Y" OR "N")
              MOVE "ENTER 'Y' OR 'N' ONLY!" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       F-20.
           MOVE VDUBUF TO GP-BW-CREDIT-SCORE.
           IF NOT (GP-BW-CREDIT-SCORE = "Y" OR "N")
              MOVE "ENTER 'Y' OR 'N' ONLY!" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       F-21.
           MOVE VDUBUF TO GP-PROGRP-DL-CONT.
           IF NOT (GP-PROGRP-DL-CONT = "Y" OR "N")
              MOVE "ENTER 'Y' OR 'N' ONLY!" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       F-22.
           MOVE VDUBUF TO GP-DLRVPRIN-1-FG.

           IF GP-DLRVPRIN-1-FG NOT = "Y" AND NOT = "N"
              MOVE "ENTER 'Y' OR 'N' ONLY!" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.

           GO TO CREATE-ROUTINE.
       F-23.
           MOVE VDUBUF TO GP-DLRVPRIN-2-FG.

           IF GP-DLRVPRIN-2-FG NOT = "Y" AND NOT = "N"
              MOVE "ENTER 'Y' OR 'N' ONLY!" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.

           GO TO CREATE-ROUTINE.
       F-24.
           MOVE VDUBUF TO GP-DLRVPRIN-3-FG.

           IF GP-DLRVPRIN-3-FG NOT = "Y" AND NOT = "N"
              MOVE "ENTER 'Y' OR 'N' ONLY!" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.

           GO TO CREATE-ROUTINE.

       F-25.
           MOVE VDUBUF TO GP-ADV-TO-1ST-NO-CORR.

           IF NOT (GP-ADV-TO-1ST-NO-CORR = "Y" OR "N" OR "A")
              MOVE "ENTER 'Y' OR 'N' OR 'A' ONLY!" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.

           GO TO CREATE-ROUTINE.

       F-26.
           MOVE VDUBUF TO GP-COMPANY-FLAG.

           IF NOT GP-COMPANY-VALID
              MOVE "INVALID COMPANY NAME, SEE HELP FOR VALID OPTIONS"
                    TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.

           GO TO CREATE-ROUTINE.

       F-27.
           MOVE VDUBUF TO GP-INTERBR-AR-AP-FULLGL.

           IF NOT (GP-INTERBR-AR-AP-FULLGL = "Y" OR "N")
              MOVE "ENTER 'Y' OR 'N' ONLY!" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.

           GO TO CREATE-ROUTINE.


       F-28.
           MOVE VDUBUF TO GP-SKIP-BI-UPDATE.

           IF NOT (GP-SKIP-BI-UPDATE = "Y" OR "N")
              MOVE "ENTER 'Y' OR 'N' ONLY!" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.

           GO TO CREATE-ROUTINE.

       F-29.
           MOVE VDUBUF TO GP-ASK-HARD-COPY-MEMOS.

           IF NOT (GP-ASK-HARD-COPY-MEMOS = "Y" OR "N")
              MOVE "ENTER 'Y' OR 'N' ONLY!" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.

           GO TO CREATE-ROUTINE.

       F-30.
           MOVE VDUBUF TO GP-RENEWAL-COLLATERAL.

           IF NOT (GP-RENEWAL-COLLATERAL = "Y" OR "N")
              MOVE "ENTER 'Y' OR 'N' ONLY!" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.

       F-31.
           MOVE VDUNUM0 TO GP-PROGRP-LN-CLASS(1).
       F-311.
           MOVE "ENNN020311." TO VDU.
           PERFORM SCREENIO.
           IF UP-KEY
              GO TO ENTER-ELE.
           IF NOT ENTER-KEY
              GO TO F-311.
           MOVE VDUNUM0 TO GP-PROGRP-LN-CLASS(2).
       F-312.
           MOVE "ENNN020312." TO VDU.
           PERFORM SCREENIO.
           IF UP-KEY
              GO TO F-311.
           IF NOT ENTER-KEY
              GO TO F-312.
           MOVE VDUNUM0 TO GP-PROGRP-LN-CLASS(3).
           GO TO CREATE-ROUTINE.
       F-32.
           MOVE VDUNUM0 TO GP-PROGRP-SF-CLASS(1).
       F-321.
           MOVE "ENNN020321." TO VDU.
           PERFORM SCREENIO.
           IF UP-KEY
              GO TO ENTER-ELE.
           IF NOT ENTER-KEY
              GO TO F-321.
           MOVE VDUNUM0 TO GP-PROGRP-SF-CLASS(2).
       F-322.
           MOVE "ENNN020322." TO VDU.
           PERFORM SCREENIO.
           IF UP-KEY
              GO TO F-321.
           IF NOT ENTER-KEY
              GO TO F-322.
           MOVE VDUNUM0 TO GP-PROGRP-SF-CLASS(3).
       F-323.
           MOVE "ENNN020323." TO VDU.
           PERFORM SCREENIO.
           IF UP-KEY
              GO TO F-322.
           IF NOT ENTER-KEY
              GO TO F-323.
           MOVE VDUNUM0 TO GP-PROGRP-SF-CLASS(4).
           GO TO CREATE-ROUTINE.
       F-33.
           MOVE VDUBUF TO GP-RENEW-DISC-UNERN.

           IF NOT (GP-RENEW-DISC-UNERN = "Y" OR "N")
              MOVE "ENTER 'Y' OR 'N' ONLY!" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.
       F-34.
           MOVE VDUBUF TO GP-NOTES-EOD-BATCH-ID.
           GO TO CREATE-ROUTINE.

       F-35.
           MOVE VDUBUF TO GP-REFNO-VERIFY.
           IF NOT (GP-REFNO-VERIFY = "Y" OR "N" OR "A" OR "T")
              MOVE "ENTER 'Y' OR 'N' OR 'A' OR 'T' ONLY!" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.

       F-36.
           MOVE VDUBUF TO GP-ACTIONCD-VERIFY.
           IF NOT (GP-ACTIONCD-VERIFY = "Y" OR "N")
              MOVE "ENTER 'Y' OR 'N' ONLY!" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-ELE.
           GO TO CREATE-ROUTINE.

       END-ENTRY-MODULE.
           MOVE 0 TO FORM-SEL.
       EXIT-ENTRY-MODULE.
           EXIT.

      ******************************************************************
       WRITE-MODULE SECTION.
      ******************************************************************
           PERFORM OPEN-GP-FILE.

           IF NEW-FLAG NOT = 0
              GO TO WRITE-RECORD.

           MOVE GP-REC TO GPCH-REC.

           IF GPOR-REC = GPCH-REC
              GO TO END-WRITE-MODULE.

           PERFORM READ-GP-FILE.
           IF IO-BAD
              PERFORM UNDERGONE-CHANGE
              PERFORM RECORD-DELETED
              GO TO END-WRITE-MODULE.

           IF GP-REC NOT = GPOR-REC
              PERFORM UNDERGONE-CHANGE
              PERFORM CHANGE-AGAIN
              GO TO END-WRITE-MODULE.

           PERFORM LOG-MAINTENANCE-CHGS.

           MOVE GPCH-REC TO GP-REC.
           PERFORM REWRITE-GP-FILE.

           GO TO END-WRITE-MODULE.

       WRITE-RECORD.
           MOVE 1 TO GP-KEY.
           PERFORM WRITE-GP-FILE.
           IF IO-BAD
              PERFORM ALREADY-CREATED.

       END-WRITE-MODULE.
           PERFORM CLOSE-GP-FILE.

      ******************************************************************
       LOG-MAINTENANCE-CHGS SECTION.
      ******************************************************************

           MOVE SPACES                     TO LMC-DETAIL.
           MOVE " TO "                     TO LMC-TO-X.

           IF GPOR-AGE-BUCKET-CD NOT = GPCH-AGE-BUCKET-CD
              MOVE "01 "                   TO LMC-FIELD
              MOVE "AGE BUCKET CD "        TO LMC-DESC
              MOVE GPOR-AGE-BUCKET-CD      TO LMC-ORG-TEXT
              MOVE GPCH-AGE-BUCKET-CD      TO LMC-CHG-TEXT
              PERFORM LOG-FIELD-CHANGE.
           IF GPOR-THE-ONLY-SF-SRCD NOT = GPCH-THE-ONLY-SF-SRCD
              MOVE "02 "                   TO LMC-FIELD
              MOVE "ONLY SF CODE  "          TO LMC-DESC
              MOVE GPOR-THE-ONLY-SF-SRCD    TO LMC-ORG-TEXT
              MOVE GPCH-THE-ONLY-SF-SRCD    TO LMC-CHG-TEXT
              PERFORM LOG-FIELD-CHANGE.
           IF GPOR-ALLOW-BATCH-CHECKS NOT = GPCH-ALLOW-BATCH-CHECKS
              MOVE "03 "                   TO LMC-FIELD
              MOVE "BATCH CHECKS  "        TO LMC-DESC
              MOVE GPOR-ALLOW-BATCH-CHECKS      TO LMC-ORG-TEXT
              MOVE GPCH-ALLOW-BATCH-CHECKS      TO LMC-CHG-TEXT
              PERFORM LOG-FIELD-CHANGE.
      * THERE IS NO 4
           IF GPOR-DLRVCHGS-3-FG NOT = GPCH-DLRVCHGS-3-FG
              MOVE "05 "                   TO LMC-FIELD
              MOVE "RESRV CHARGS 3"        TO LMC-DESC
              MOVE GPOR-DLRVCHGS-3-FG      TO LMC-ORG-TEXT
              MOVE GPCH-DLRVCHGS-3-FG      TO LMC-CHG-TEXT
              PERFORM LOG-FIELD-CHANGE.
           IF GPOR-CASH-METHOD-ONLY NOT = GPCH-CASH-METHOD-ONLY
              MOVE "06 "                   TO LMC-FIELD
              MOVE "CASH EARN ONLY"        TO LMC-DESC
              MOVE GPOR-CASH-METHOD-ONLY      TO LMC-ORG-TEXT
              MOVE GPCH-CASH-METHOD-ONLY      TO LMC-CHG-TEXT
              PERFORM LOG-FIELD-CHANGE.
           IF GPOR-IB-ACCRUALS-DONE NOT = GPCH-IB-ACCRUALS-DONE
              MOVE "07 "                   TO LMC-FIELD
              MOVE "IB ACCRUALS   "        TO LMC-DESC
              MOVE GPOR-IB-ACCRUALS-DONE      TO LMC-ORG-TEXT
              MOVE GPCH-IB-ACCRUALS-DONE      TO LMC-CHG-TEXT
              PERFORM LOG-FIELD-CHANGE.
           IF GPOR-NO-LC-IN-ARREARAGE-FG NOT =
                                     GPCH-NO-LC-IN-ARREARAGE-FG
              MOVE "08 "                   TO LMC-FIELD
              MOVE "NO LCHG ARREAR"        TO LMC-DESC
              MOVE GPOR-NO-LC-IN-ARREARAGE-FG      TO LMC-ORG-TEXT
              MOVE GPCH-NO-LC-IN-ARREARAGE-FG      TO LMC-CHG-TEXT
              PERFORM LOG-FIELD-CHANGE.
           IF GPOR-DEFAULT-O3-INS-COMPANY NOT =
                                     GPCH-DEFAULT-O3-INS-COMPANY
              MOVE "09 "                   TO LMC-FIELD
              MOVE "DEFAULT O3 INS"        TO LMC-DESC
              MOVE GPOR-DEFAULT-O3-INS-COMPANY      TO LMC-ORG-TEXT
              MOVE GPCH-DEFAULT-O3-INS-COMPANY      TO LMC-CHG-TEXT
              PERFORM LOG-FIELD-CHANGE.
           IF GPOR-STOP-ON-PRIOR-FG NOT = GPCH-STOP-ON-PRIOR-FG
              MOVE "10 "                   TO LMC-FIELD
              MOVE "STOP PRIOR LN#"        TO LMC-DESC
              MOVE GPOR-STOP-ON-PRIOR-FG      TO LMC-ORG-TEXT
              MOVE GPCH-STOP-ON-PRIOR-FG      TO LMC-CHG-TEXT
              PERFORM LOG-FIELD-CHANGE.
           IF GPOR-DEFAULT-ACCRATE NOT = GPCH-DEFAULT-ACCRATE
              MOVE "11 "                   TO LMC-FIELD
              MOVE "DEFAULT ACCRTE"        TO LMC-DESC
              MOVE GPOR-DEFAULT-ACCRATE      TO LMC-ORG-AMT
              MOVE GPCH-DEFAULT-ACCRATE      TO LMC-CHG-AMT
              PERFORM LOG-FIELD-CHANGE.
           IF GPOR-DLR-DISP-TYPE NOT = GPCH-DLR-DISP-TYPE
              MOVE "12 "                   TO LMC-FIELD
              MOVE "DL DISC PROMPT"        TO LMC-DESC
              MOVE GPOR-DLR-DISP-TYPE      TO LMC-ORG-TEXT
              MOVE GPCH-DLR-DISP-TYPE      TO LMC-CHG-TEXT
              PERFORM LOG-FIELD-CHANGE.
           IF GPOR-AUTO-REPO-WINDOW NOT = GPCH-AUTO-REPO-WINDOW
              MOVE "13 "                   TO LMC-FIELD
              MOVE "REPO WINDOW   "        TO LMC-DESC
              MOVE GPOR-AUTO-REPO-WINDOW      TO LMC-ORG-TEXT
              MOVE GPCH-AUTO-REPO-WINDOW      TO LMC-CHG-TEXT
              PERFORM LOG-FIELD-CHANGE.
           IF GPOR-ALLOW-PI-WITH-SE-BAL NOT = GPCH-ALLOW-PI-WITH-SE-BAL
              MOVE "14 "                   TO LMC-FIELD
              MOVE "PI ON SE W/BAL"        TO LMC-DESC
              MOVE GPOR-ALLOW-PI-WITH-SE-BAL      TO LMC-ORG-TEXT
              MOVE GPCH-ALLOW-PI-WITH-SE-BAL      TO LMC-CHG-TEXT
              PERFORM LOG-FIELD-CHANGE.
           IF GPOR-ALLOW-MORE-COLLECT NOT = GPCH-ALLOW-MORE-COLLECT
              MOVE "15 "                   TO LMC-FIELD
              MOVE "ALLOW >1 CFORM"        TO LMC-DESC
              MOVE GPOR-ALLOW-MORE-COLLECT      TO LMC-ORG-TEXT
              MOVE GPCH-ALLOW-MORE-COLLECT      TO LMC-CHG-TEXT
              PERFORM LOG-FIELD-CHANGE.
           IF GPOR-LOAN-CONVERSION-UPD-FG NOT =
                                        GPCH-LOAN-CONVERSION-UPD-FG
              MOVE "16 "                   TO LMC-FIELD
              MOVE "LOAN CONV UPDT"        TO LMC-DESC
              MOVE GPOR-LOAN-CONVERSION-UPD-FG      TO LMC-ORG-TEXT
              MOVE GPCH-LOAN-CONVERSION-UPD-FG      TO LMC-CHG-TEXT
              PERFORM LOG-FIELD-CHANGE.
           IF GPOR-FORM-CHG-CD NOT = GPCH-FORM-CHG-CD
              MOVE "17 "                   TO LMC-FIELD
              MOVE "OTCHG FORM PRT"        TO LMC-DESC
              MOVE GPOR-FORM-CHG-CD      TO LMC-ORG-TEXT
              MOVE GPCH-FORM-CHG-CD      TO LMC-CHG-TEXT
              PERFORM LOG-FIELD-CHANGE.
           IF GPOR-PROGRP-PRINT-REDEEM NOT = GPCH-PROGRP-PRINT-REDEEM
              MOVE "18 "                   TO LMC-FIELD
              MOVE "PRINT REDEEMED"        TO LMC-DESC
              MOVE GPOR-PROGRP-PRINT-REDEEM      TO LMC-ORG-TEXT
              MOVE GPCH-PROGRP-PRINT-REDEEM      TO LMC-CHG-TEXT
              PERFORM LOG-FIELD-CHANGE.
           IF GPOR-BW-HOUSING-CODE NOT = GPCH-BW-HOUSING-CODE
              MOVE "19 "                   TO LMC-FIELD
              MOVE "HOUSING CODES "        TO LMC-DESC
              MOVE GPOR-BW-HOUSING-CODE      TO LMC-ORG-TEXT
              MOVE GPCH-BW-HOUSING-CODE      TO LMC-CHG-TEXT
              PERFORM LOG-FIELD-CHANGE.
           IF GPOR-BW-CREDIT-SCORE NOT = GPCH-BW-CREDIT-SCORE
              MOVE "20 "                   TO LMC-FIELD
              MOVE "REQUIRE CRSCOR"        TO LMC-DESC
              MOVE GPOR-BW-CREDIT-SCORE      TO LMC-ORG-TEXT
              MOVE GPCH-BW-CREDIT-SCORE      TO LMC-CHG-TEXT
              PERFORM LOG-FIELD-CHANGE.
           IF GPOR-PROGRP-DL-CONT NOT = GPCH-PROGRP-DL-CONT
              MOVE "21 "                   TO LMC-FIELD
              MOVE "PROG DLCON 150"        TO LMC-DESC
              MOVE GPOR-PROGRP-DL-CONT      TO LMC-ORG-TEXT
              MOVE GPCH-PROGRP-DL-CONT      TO LMC-CHG-TEXT
              PERFORM LOG-FIELD-CHANGE.
           IF GPOR-DLRVPRIN-1-FG NOT = GPCH-DLRVPRIN-1-FG
              MOVE "22 "                   TO LMC-FIELD
              MOVE "ALLOW RSV PR 1"        TO LMC-DESC
              MOVE GPOR-DLRVPRIN-1-FG      TO LMC-ORG-TEXT
              MOVE GPCH-DLRVPRIN-1-FG      TO LMC-CHG-TEXT
              PERFORM LOG-FIELD-CHANGE.
           IF GPOR-DLRVPRIN-2-FG NOT = GPCH-DLRVPRIN-2-FG
              MOVE "23 "                   TO LMC-FIELD
              MOVE "ALLOW RSV PR 2"        TO LMC-DESC
              MOVE GPOR-DLRVPRIN-2-FG      TO LMC-ORG-TEXT
              MOVE GPCH-DLRVPRIN-2-FG      TO LMC-CHG-TEXT
              PERFORM LOG-FIELD-CHANGE.
           IF GPOR-DLRVPRIN-3-FG NOT = GPCH-DLRVPRIN-3-FG
              MOVE "24 "                   TO LMC-FIELD
              MOVE "ALLOW RSV PR 3"        TO LMC-DESC
              MOVE GPOR-DLRVPRIN-3-FG      TO LMC-ORG-TEXT
              MOVE GPCH-DLRVPRIN-3-FG      TO LMC-CHG-TEXT
              PERFORM LOG-FIELD-CHANGE.
           IF GPOR-ADV-TO-1ST-NO-CORR NOT = GPCH-ADV-TO-1ST-NO-CORR
              MOVE "25 "                   TO LMC-FIELD
              MOVE "ADVNC 1STPY OP"        TO LMC-DESC
              MOVE GPOR-ADV-TO-1ST-NO-CORR      TO LMC-ORG-TEXT
              MOVE GPCH-ADV-TO-1ST-NO-CORR      TO LMC-CHG-TEXT
              PERFORM LOG-FIELD-CHANGE.
           IF GPOR-COMPANY-FLAG NOT = GPCH-COMPANY-FLAG
              MOVE "26 "                   TO LMC-FIELD
              MOVE "COMPANY OPTION"        TO LMC-DESC
              MOVE GPOR-COMPANY-FLAG      TO LMC-ORG-TEXT
              MOVE GPCH-COMPANY-FLAG      TO LMC-CHG-TEXT
              PERFORM LOG-FIELD-CHANGE.
           IF GPOR-INTERBR-AR-AP-FULLGL NOT =
                                     GPCH-INTERBR-AR-AP-FULLGL
              MOVE "27 "                   TO LMC-FIELD
              MOVE "INTERBR GLNO  "        TO LMC-DESC
              MOVE GPOR-INTERBR-AR-AP-FULLGL      TO LMC-ORG-TEXT
              MOVE GPCH-INTERBR-AR-AP-FULLGL      TO LMC-CHG-TEXT
              PERFORM LOG-FIELD-CHANGE.
           IF GPOR-SKIP-BI-UPDATE NOT = GPCH-SKIP-BI-UPDATE
              MOVE "28 "                   TO LMC-FIELD
              MOVE "SKIP UPDATE BI"        TO LMC-DESC
              MOVE GPOR-SKIP-BI-UPDATE      TO LMC-ORG-TEXT
              MOVE GPCH-SKIP-BI-UPDATE      TO LMC-CHG-TEXT
              PERFORM LOG-FIELD-CHANGE.
           IF GPOR-ASK-HARD-COPY-MEMOS NOT = GPCH-ASK-HARD-COPY-MEMOS
              MOVE "29 "                   TO LMC-FIELD
              MOVE "ASK HARD COPY "        TO LMC-DESC
              MOVE GPOR-ASK-HARD-COPY-MEMOS      TO LMC-ORG-TEXT
              MOVE GPCH-ASK-HARD-COPY-MEMOS      TO LMC-CHG-TEXT
              PERFORM LOG-FIELD-CHANGE.
           IF GPOR-RENEWAL-COLLATERAL NOT = GPCH-RENEWAL-COLLATERAL
              MOVE "30 "                   TO LMC-FIELD
              MOVE "RENEWAL COLLAT"        TO LMC-DESC
              MOVE GPOR-RENEWAL-COLLATERAL      TO LMC-ORG-TEXT
              MOVE GPCH-RENEWAL-COLLATERAL      TO LMC-CHG-TEXT
              PERFORM LOG-FIELD-CHANGE.
           IF GPOR-PROGRP-LN-CLASS(1) NOT = GPCH-PROGRP-LN-CLASS(1)
              MOVE "31 "                   TO LMC-FIELD
              MOVE "PROG RPT LOANS"        TO LMC-DESC
              MOVE GPOR-PROGRP-LN-CLASS(1)      TO LMC-ORG-TEXT
              MOVE GPCH-PROGRP-LN-CLASS(1)      TO LMC-CHG-TEXT
              PERFORM LOG-FIELD-CHANGE.
           IF GPOR-PROGRP-LN-CLASS(2) NOT = GPCH-PROGRP-LN-CLASS(2)
              MOVE "311"                   TO LMC-FIELD
              MOVE "PROG RPT LOANS"        TO LMC-DESC
              MOVE GPOR-PROGRP-LN-CLASS(2)      TO LMC-ORG-TEXT
              MOVE GPCH-PROGRP-LN-CLASS(2)      TO LMC-CHG-TEXT
              PERFORM LOG-FIELD-CHANGE.
           IF GPOR-PROGRP-LN-CLASS(3) NOT = GPCH-PROGRP-LN-CLASS(3)
              MOVE "312"                   TO LMC-FIELD
              MOVE "PROG RPT LOANS"        TO LMC-DESC
              MOVE GPOR-PROGRP-LN-CLASS(3)      TO LMC-ORG-TEXT
              MOVE GPCH-PROGRP-LN-CLASS(3)      TO LMC-CHG-TEXT
              PERFORM LOG-FIELD-CHANGE.
           IF GPOR-PROGRP-SF-CLASS(1) NOT = GPCH-PROGRP-SF-CLASS(1)
              MOVE "32 "                   TO LMC-FIELD
              MOVE "PROG RPT SALES"        TO LMC-DESC
              MOVE GPOR-PROGRP-SF-CLASS(1)      TO LMC-ORG-TEXT
              MOVE GPCH-PROGRP-SF-CLASS(1)      TO LMC-CHG-TEXT
              PERFORM LOG-FIELD-CHANGE.
           IF GPOR-PROGRP-SF-CLASS(2) NOT = GPCH-PROGRP-SF-CLASS(2)
              MOVE "321"                   TO LMC-FIELD
              MOVE "PROG RPT SALES"        TO LMC-DESC
              MOVE GPOR-PROGRP-SF-CLASS(2)      TO LMC-ORG-TEXT
              MOVE GPCH-PROGRP-SF-CLASS(2)      TO LMC-CHG-TEXT
              PERFORM LOG-FIELD-CHANGE.
           IF GPOR-PROGRP-SF-CLASS(3) NOT = GPCH-PROGRP-SF-CLASS(3)
              MOVE "322"                   TO LMC-FIELD
              MOVE "PROG RPT SALES"        TO LMC-DESC
              MOVE GPOR-PROGRP-SF-CLASS(3)      TO LMC-ORG-TEXT
              MOVE GPCH-PROGRP-SF-CLASS(3)      TO LMC-CHG-TEXT
              PERFORM LOG-FIELD-CHANGE.
           IF GPOR-PROGRP-SF-CLASS(4) NOT = GPCH-PROGRP-SF-CLASS(4)
              MOVE "323"                   TO LMC-FIELD
              MOVE "PROG RPT SALES"        TO LMC-DESC
              MOVE GPOR-PROGRP-SF-CLASS(4)      TO LMC-ORG-TEXT
              MOVE GPCH-PROGRP-SF-CLASS(4)      TO LMC-CHG-TEXT
              PERFORM LOG-FIELD-CHANGE.
           IF GPOR-RENEW-DISC-UNERN NOT = GPCH-RENEW-DISC-UNERN
              MOVE "33 "                   TO LMC-FIELD
              MOVE "DLDISC RENEWAL"        TO LMC-DESC
              MOVE GPOR-RENEW-DISC-UNERN      TO LMC-ORG-TEXT
              MOVE GPCH-RENEW-DISC-UNERN      TO LMC-CHG-TEXT
              PERFORM LOG-FIELD-CHANGE.
           IF GPOR-NOTES-EOD-BATCH-ID NOT = GPCH-NOTES-EOD-BATCH-ID
              MOVE "34 "                   TO LMC-FIELD
              MOVE "NOTE EOD BATCH"        TO LMC-DESC
              MOVE GPOR-NOTES-EOD-BATCH-ID      TO LMC-ORG-TEXT
              MOVE GPCH-NOTES-EOD-BATCH-ID      TO LMC-CHG-TEXT
              PERFORM LOG-FIELD-CHANGE.
           IF GPOR-REFNO-VERIFY NOT = GPCH-REFNO-VERIFY
              MOVE "35 "                   TO LMC-FIELD
              MOVE "VERIFY REFNO  "        TO LMC-DESC
              MOVE GPOR-REFNO-VERIFY      TO LMC-ORG-TEXT
              MOVE GPCH-REFNO-VERIFY      TO LMC-CHG-TEXT
              PERFORM LOG-FIELD-CHANGE.
           IF GPOR-ACTIONCD-VERIFY NOT = GPCH-ACTIONCD-VERIFY
              MOVE "36 "                   TO LMC-FIELD
              MOVE "VERIFY ACTION "        TO LMC-DESC
              MOVE GPOR-ACTIONCD-VERIFY      TO LMC-ORG-TEXT
              MOVE GPCH-ACTIONCD-VERIFY      TO LMC-CHG-TEXT
              PERFORM LOG-FIELD-CHANGE.

      ******************************************************************
       LOG-FIELD-CHANGE SECTION.
      ******************************************************************

           MOVE "F"           TO ERRLOGW-ACTION.
           MOVE LMC-DETAIL    TO ERRLOGW-DETAIL.
           MOVE FORM-PATHNAME TO ERRLOGW-PROGX.

           MOVE "CL/ERRLOG" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH ERRLOGW-AREA.
           CANCEL FORM-PROGX.

       LOG-FIELD-CHANGE-EXIT.
           EXIT.

      ******************************************************************
       DISPLAY-FIELDS SECTION.
      ******************************************************************
           MOVE SPACES TO DISPLAY-BUF.

           MOVE GP-AGE-BUCKET-CD TO F01.

           IF GP-THE-ONLY-SF-SRCD NOT = " "
              MOVE GP-THE-ONLY-SF-SRCD TO F02
              MOVE "SR"                TO CD-TYPE
              MOVE GP-THE-ONLY-SF-SRCD TO CD-CODE
              PERFORM GET-CD
              IF IO-BAD
                 MOVE "****** NOT ON FILE *******" TO CD-DESC
              END-IF
              MOVE CD-DESC TO F021.

           MOVE GP-ALLOW-BATCH-CHECKS TO F03.
           MOVE SPACES TO F04.
           MOVE GP-DLRVCHGS-3-FG TO F05.
           MOVE GP-CASH-METHOD-ONLY TO F06.
           MOVE GP-IB-ACCRUALS-DONE TO F07.
           MOVE GP-NO-LC-IN-ARREARAGE-FG TO F08.
           MOVE GP-DEFAULT-O3-INS-COMPANY TO F09.
           MOVE GP-STOP-ON-PRIOR-FG TO F10.
           MOVE GP-DEFAULT-ACCRATE TO F11.
           MOVE GP-DLR-DISP-TYPE TO F12.
           MOVE GP-AUTO-REPO-WINDOW TO F13.
           MOVE GP-ALLOW-PI-WITH-SE-BAL TO F14.
           MOVE GP-ALLOW-MORE-COLLECT TO F15.
           MOVE GP-LOAN-CONVERSION-UPD-FG TO F16.
           MOVE GP-FORM-CHG-CD TO F17.
           MOVE GP-PROGRP-PRINT-REDEEM TO F18.
           MOVE GP-BW-HOUSING-CODE TO F19.
           MOVE GP-BW-CREDIT-SCORE TO F20.
           MOVE GP-PROGRP-DL-CONT TO F21.
           MOVE GP-DLRVPRIN-1-FG TO F22.
           MOVE GP-DLRVPRIN-2-FG TO F23.
           MOVE GP-DLRVPRIN-3-FG TO F24.
           MOVE GP-ADV-TO-1ST-NO-CORR TO F25.
           MOVE GP-COMPANY-FLAG TO F26.
           MOVE GP-INTERBR-AR-AP-FULLGL TO F27.
           MOVE GP-SKIP-BI-UPDATE TO F28.
           MOVE GP-ASK-HARD-COPY-MEMOS TO F29.
           MOVE GP-RENEWAL-COLLATERAL TO F30.
           MOVE GP-PROGRP-LN-CLASS(1) TO F31.
           MOVE GP-PROGRP-LN-CLASS(2) TO F311.
           MOVE GP-PROGRP-LN-CLASS(3) TO F312.
           MOVE GP-PROGRP-SF-CLASS(1) TO F32.
           MOVE GP-PROGRP-SF-CLASS(2) TO F321.
           MOVE GP-PROGRP-SF-CLASS(3) TO F322.
           MOVE GP-PROGRP-SF-CLASS(4) TO F323.
           MOVE GP-RENEW-DISC-UNERN   TO F33.
           MOVE GP-NOTES-EOD-BATCH-ID TO F34.
           MOVE GP-REFNO-VERIFY       TO F35.
           MOVE GP-ACTIONCD-VERIFY    TO F36.

           MOVE DISPLAY-BUF TO WR-BUF.
           MOVE DISPLAY-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       DISPLAY-FIELDS-EXIT.
           EXIT.

       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBLP/LPCDCL.CPY".
       COPY "LIBGB/DATER.CPY".

      ***********************************************
       FORM-RESET SECTION.
      ***********************************************
           MOVE EXT-CONAME-CONAME TO GPMAN2-CONAME.
           MOVE EXT-CONAME-SYSDATE TO GPMAN2-SYSDATE.
           DISPLAY GPMAN2-SCREEN UPON GPMAIN-WINDOW-HANDLE.
           MOVE GPMAN2-FORM-FIELDS TO WR-BUF.
           MOVE GPMAN2-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      ***********************************************
       SETUP-LINK-INFO SECTION.
      ***********************************************
           MOVE GPMAN2-HELP--FILE TO HELP-LINK-FILE.
           MOVE GPMAN2-HELP--DIR TO HELP-LINK-DIR.

      ***********************************************
       VDU-CONVERT-FIELD SECTION.
      ***********************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBGB/GPMAN2_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ******************************************************************
       MISC-PARAGRAPHS SECTION.
      ******************************************************************
       COPY "LIBGB/GETENV.CPY".
       COPY "LIBGB/ACCESS.CPY".
       GET-CD.
           PERFORM OPEN-CD1-FILE.
           PERFORM READ-CD1-FILE.
           PERFORM CLOSE-CD1-FILE.

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
           MOVE
           " THIS RECORD HAS UNDERGONE CHANGE BY ANOTHER USER,"
           TO MESS.
           PERFORM SEND-MESS.

      ******************************************************************
       IO-ROUTINES SECTION.
      ******************************************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/DECLARE_SQL.CPY".
           PERFORM CLOSE-CD1-FILE.
           PERFORM CLOSE-GP-FILE.
       COPY "LIBLP/LPCDGS_SQL.CPY".
       COPY "LIBGL/GLGLGS_SQL.CPY".
       COPY "LIBGB/GBGPGS_SQL.CPY".
       COPY "LIBGB/GBGPIN.CPY".
       COPY "LIBLP/LPCD1IN.CPY".
       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO ERRCD.

      ******************************************************************
       END-ROUTINE SECTION.
      ******************************************************************
           PERFORM CLOSE-FILES.
       EXIT-PROG.
       STOP-RUN.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY GPMAN2-SCREEN.
           MOVE FORM-PATH TO FORM-PATHNAME.
           MOVE GPMAIN-LINK TO FORM-LINK-AREA.
           EXIT PROGRAM.
