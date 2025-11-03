       IDENTIFICATION DIVISION.
       PROGRAM-ID.   DSMAIN.
      ******************************************************************
      *                 (MN)
      *
      *  DESCRIPTION:   DEALER STATISTICS FILE MAINTENANCE
      *
      * REV :
      *  061395 MJD ADDED PAGE 4, REPOSSESSIONS.
      *  083195 KEC ADDED PAGE 5, DELINQUENCY
      *  MJD 091495 ADDED DS-DATE TO DS1-KEY.
      *  MJD 042696 ADDED GP-DLR-DISP-TYPE
      *  KEC 043096 ADDED DS-AFPDUEDL-MEMO.
      *  JTG 093096 REMOVED REFERENCE TO PAGE 5 DELINQUENCE 8-18
      *  JFF 101196 CHANGED #11 'DS-SUMWGTTM' ON FIRST PAGE TO
      *              ACCEPT INPUT OF SIGNED NUMBER.
      *  KEC 031997 REVISED FOR NEW FILE FORMAT:
      *                ADDED   NEW APPLICATION STATISTICS, PAGE 5;
      *                REMOVED OLD APPLICATION STATISTICS,
      *                AVAILABLE SPACE ON PAGE 1.
      *  JTG 032497 CHANGED 'GETFM CALL' TO USE CUR-FORM NO.
      *             AND NOT DEFAULT TO 1 RE:F12 ^ ^ F12
      *  BLV 051497 REVISIONS TO PAGE 5; ADD APPS MADE
      *  MJD 070697 ADDED DS-DLDISC-VOL
      *  MJD 071097 FIXED BUG. IF EXIT FROM PAGE 3 AND RENTER, THEN
      *             FORM DISPLAYED IS PAGE ONE BUT CUR FORM IS STILL SET TO 3.
      *  GLF 090997 REPLACED 'IO' ON GP-FILE WITH GET 'GPENV'
      *  MJD 091297 CHANGED PAGE 5 TO DISPLAY
      *             CUR/30/60/90/120/150/180/210+/<M>/<P>
      *  MJD 100997 CHANGED FD. CHANGED DEL TABLE FROM OCCURS 18 TO OCCURS 10
      *  JTG 012198 CHANGED PAGE 3 FOR S9(6) OUTSUNIT & S9(9)V99 OUTSAMT
      *  MJD 051998 REMOVED DS-PASTDUE.
      *  KEC 061198 ADDED BANKRUPTCY-DELINQUENCY-TABLE TO PAGE 5.
      *  JTG 063098 EXPANDED DS-APP-TYPE-TBL TO 3 LEVELS
      *  MJD 000323 REMOVED DS-CNEARN/EARNTAKN/EARNDUE/CNUNEARN/ERNOTTAK
      *             ADDED DS-UNEARNED-AMT AND DS-UNEARNED-AMT2 (MERC #223)
      *  MJD 000330 ADDED DS-PROCEEDS MTD/YTD/LYR TO PAGE 1 (MERC PR#223)
      *  GLB 000908 ADD DS-UNPAID-MANUAL TO PAGE 1
      *  GLB 001024 RE-DO SCAN TO START WITH HOLD-KEY
      * MJD 131119 A30 CONVERSION COMPLETED.
      * BLM 160513 FIX FOR NEW DS-DATE FORMAT, FIX DS-UNEARNED, PD0003
      * KEC 2017.0105 {PR#00000} PARADATA <A30> [JAY CISZEWSKI]
      *          CHANGED DS-DATE TO DS-DATE-CCYYMM FOR CONSISTENCY
      *          WITH THE DATE FIELDS IN BY & UD FILE.
      *
      * BAH 181011 GLOBAL DLFILE
      * BAH 181015 GLOBAL DSFILE
      *  CS 190423 REMOVE APP STATS
      *  CS 190428 FIXED DSMAN5 (APP STATS DISPLAY)
      *  CS 191126 DSFILE LOCAL IN A15, BUT MAINTENANCE ALLOWS FOR ALL
      *            BRANCHES TO BE ACCESSED IN A15, CHANGED A32 TO ALLOW,
      *            EVEN THOUGH I TYPED IN BRANCH 10, IT WAS GETTING MY
      *            BRANCH 1 EXT-FILPATH-DATA
      *  CS 191203 REMOVED THE SET OF DS-PATH-OWNBR IF SCAN = 0, I ENTERED
      *            BRANCH AS PART OF SCAN
      *  CS 191211 SCAN ALLOWS ACCESS TO ALL BRANCHES, IF YOU ENTERED
      *            KEY DIRECTLY, WORKED FINE, BUT SCAN WAS REALLY MUCKED
      *            IF YOU WOULD KEY ENTRY, F1, AND THEN SCAN, BECAUSE
      *            IT WAS MOVING HOLD-KEY TO DS1-KEY AND READING NEXT.
      *            REMOVED THE HOLD-KEY STUFF AT THE TOP OF ENTER-SCAN
      *  CS 200219 FIXED SCANKEY
      * BLM 210920 AT START OF REC-DS-MODULE, ONLY ZERO DS1-KEY IF 
      *            IF FORM-SEL = 0, OTHER PAGES DIDN'T WORK
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       DATA DIVISION.
       FILE SECTION.

      ******************************************************************
      *
      *    W O R K I N G - S T O R A G E    S E C T I O N
      *
      ******************************************************************
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)MN/DSMAIN.CBL S35 02/21/24-12:31:34 >".

      ******************************************************************
      *
      *     P R O G R A M    W O R K E R S
      *
      ******************************************************************
       COPY "LIBLP/LP01DS.CPY".
       COPY "LIBLP/LP01DS_SQL.CPY".
       COPY "LIBLP/LPWSDS.CPY".
       COPY "LIBLP/LP01DL.CPY".
       COPY "LIBLP/LP01DL_SQL.CPY".
       COPY "LIBLP/LPWSDL.CPY".
       COPY "LIBLP/LP01LC.CPY".
       COPY "LIBLP/LP01LC_SQL.CPY".
       COPY "LIBLP/LPWSLC.CPY".
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
      *-----------------------------------------------------------------

       01  ERRCD             PIC X     VALUE SPACES.
       01  CHAN              PIC X(4)  VALUE "CHAN".
       01  ONE               PIC S9.
       01  SUB               PIC 99    COMP   VALUE 0.
       01  SUB2              PIC 99    COMP   VALUE 0.
       01  SUB3              PIC 99    COMP   VALUE 0.
       01  SUB4              PIC 99    COMP   VALUE 0.
       01  SUB5              PIC 99    COMP   VALUE 0.
       01  SUB6              PIC 99    COMP   VALUE 0.
       01  SUB-P5            PIC 99    COMP   VALUE 0.
       01  NET-NEW           PIC 99    COMP   VALUE 0.
       01  POS-HORIZ         PIC 99    COMP-3 VALUE 0.
       01  POS-VERT          PIC 99    COMP-3 VALUE 0.
       01  TYP              PIC 9(02) VALUE 0.
       01  ACT               PIC 9(02) VALUE 0.
       01  ELE               PIC 99    VALUE 0.
       01  ACTION.
           03  CHKEY         PIC X(4).
           03  CHNO          PIC 9(2).
       01  IO-TYPE           PIC X(3).
       01  NEW-FLAG          PIC 9.
       01  FIRST-TIME        PIC 9     VALUE 0.
       01  FIRST-SCAN        PIC 9     VALUE 0.
       01 DSMAIN-WINDOW-HANDLE PIC X(10) VALUE SPACES.

      *-----------------------------------------------------------------
       01  HEADER-BUFFER               PIC X(80)     VALUE SPACES.
       01  HEADER-LIST                 PIC X(07)     VALUE "HEADER.".

       01  HEADER-LAYOUT.
           03  FILLER                  PIC X(23)     VALUE SPACES.
           03  HEADER-DEL-TYPE         PIC X(34)     VALUE SPACES.
           03  FILLER                  PIC X(09)     VALUE SPACES.
           03  HEADER-BANKRUPT-STD     PIC X(14)     VALUE SPACES.

       01  HEADER-LEV                  PIC 9(01)     VALUE 1.
           88  HEADER-LEV-STD                        VALUE 1.
           88  HEADER-LEV-BANKRUPTCY                 VALUE 2.

       01  HEADER-LEVEL                PIC 9(01)     VALUE 1.
           88  HEADER-LEVEL-STD                      VALUE 1.
           88  HEADER-LEVEL-BANKRUPTCY               VALUE 2.

       01  HEADER-TYPE-TABLE.
           03  FILLER                  PIC X(14)     VALUE
               "              ".
           03  FILLER                  PIC X(14)     VALUE
               "[ BANKRUPTCY ]".
       01  HEADER-TYPES      REDEFINES HEADER-TYPE-TABLE.
           03  HEADER-TYPE             PIC X(14)     OCCURS 2 TIMES.

       01  DEL-TYPE-TABLE.
           03  FILLER                  PIC X(34)     VALUE
               " ****  CURRENT INFORMATION  ****  ".
           03  FILLER                  PIC X(34)     VALUE
               "****  BEGINNING OF THE MONTH  ****".
           03  FILLER                  PIC X(34)     VALUE
               "   ****  END OF LAST MONTH ****   ".
       01  DEL-TYPES         REDEFINES DEL-TYPE-TABLE.
           03  DEL-TYPE                PIC X(34)     OCCURS 3 TIMES.

       01  DELINQ                      PIC 9(01)     VALUE 1.
       01  DELTYP-COD                  PIC 9(01)     VALUE ZEROES.

      *-----------------------------------------------------------------

       01  HOLD-DS1-KEY.
           05  HOLD-DS-DATE-CCYYMM     PIC 9(6).
           05  HOLD-DS-DATEX-CCYYMM    REDEFINES HOLD-DS-DATE-CCYYMM
                                       PIC X(6).
           05  HOLD-DS2-KEY.
               07  HOLD-DS-BRANCH      PIC 9(4).
               07  HOLD-DS-NUM         PIC 9(6).
               07  HOLD-DS-DLNO        REDEFINES HOLD-DS-NUM
                                       PIC 9(4)V99.
               07  HOLD-DS-CLASS       PIC 9(3).

       01  WS-DS1-KEY.
           05  WS-DS-DATE-MMYY       PIC 9(4).
           05  WS-DS2-KEY.
               07  WS-DS-BRANCH      PIC 9(4).
               07  WS-DS-NUM         PIC 9(6).
               07  WS-DS-DLNO        REDEFINES WS-DS-NUM
                                       PIC 9(4)V99.
               07  WS-DS-CLASS       PIC 9(2).




       01  ORIGINAL-DS-REC      PIC X(DS-SIZE).
       01  CHANGED-DS-REC       PIC X(DS-SIZE).

       01 DISPLAY-DATE       PIC 9(4).
       01 DISPLAY-DATEX  REDEFINES DISPLAY-DATE.
          03  DISPLAY-MO     PIC 99.
          03  DISPLAY-YR     PIC 99.
       01  DATE-LIST         PIC X(5) VALUE "MMYY.".

      *-----------------------------------------------------------------
       01  DLR-TYP-LIST      PIC X(5)   VALUE "9P:P.".
       01  DLR-TYP-BUF       PIC X(4).

       01  FORM-BUF.
           03  FILLER        PIC X(9) VALUE "MN/DSMAN1".
           03  FILLER        PIC X(9) VALUE "MN/DSMAN2".
           03  FILLER        PIC X(9) VALUE "MN/DSMAN3".
           03  FILLER        PIC X(9) VALUE "MN/DSMAN4".
           03  FILLER        PIC X(9) VALUE "MN/DSMAN5".
       01  FORM-BUF-RED REDEFINES FORM-BUF.
           03  FORM-NAME     PIC X(9) OCCURS 5.

       01  FORM-SEL          PIC 9     VALUE 0.
       01  CUR-FORM          PIC 9     VALUE 1.

      *-----------------------------------------------------------------
      *
      *    PERIOD TYP ----- FOR PAGE 5
      *
      *-----------------------------------------------------------------
       01  PER-LIST.
           03  FILLER                  PIC X(20)   VALUE
               "PER_TYPE1 PER_TYPE2.".

       01  PER-BUFFER.
           03  PER-BUF1                PIC X(24)   VALUE " ".
           03  PER-BUF2                PIC X(24)   VALUE " ".

       01  PER-TYPE-TABLE.
           03  FILLER                  PIC X(24) VALUE
               "CURRENT OUTSTANDING    :".
           03  FILLER                  PIC X(24) VALUE
               "END LAST MO OUTSTANDING:".
           03  FILLER                  PIC X(24) VALUE
               "END LAST YR OUTSTANDING:".
           03  FILLER                  PIC X(24) VALUE
               "MONTH-TO-DATE VOLUME   :".
           03  FILLER                  PIC X(24) VALUE
               "YEAR-TO-DATE VOLUME    :".
           03  FILLER                  PIC X(24) VALUE
               "LAST YEAR VOLUME       :".
       01  PER-TYPES         REDEFINES PER-TYPE-TABLE.
           03  PER-TYPE1               PIC X(24) OCCURS 3 TIMES.
           03  PER-TYPE2               PIC X(24) OCCURS 3 TIMES.

       01  PERIOD                      PIC 9       VALUE 1.
       01  PERIOD-F5                   PIC 9       VALUE 1.

      ******************************************************************
      *    *      SCREEN DISPLAY BUFFER1
      ******************************************************************
       01  DISPLAY-BUF1      VALUE SPACES.
           03  A01           PIC ZZZZZZZZ.ZZ-.
           03  A02           PIC ZZZZZZZZ.ZZ-.
           03  A03           PIC ZZZZZZZZ.ZZ-.
           03  A04           PIC ZZZZZZZZ.ZZ-.
           03  A05           PIC ZZZZZZZZ.ZZ-.
           03  A06           PIC ZZZZZZZZ.ZZ-.
           03  A07           PIC ZZZZZZZZ.ZZ-.
           03  A08           PIC ZZZZZZZZ.ZZ-.
           03  A081          PIC ZZZZZZZZ.ZZ-.
           03  A09           PIC ZZZZZZZZ.ZZ-.
           03  A10           PIC ZZZZZZZZ.ZZ-.
           03  A11           PIC ZZZZZZZZZ.ZZ-.
           03  A111          PIC ZZZZZZZZ-.
           03  A12           PIC ZZZZZZZZZZZ.ZZ-.
           03  A13           PIC ZZZZZZZZ.ZZ-.
           03  A14           PIC ZZZZZZZZ.ZZ-.
           03  A15           PIC ZZZZZZZZ.ZZ-.
           03  A16           PIC ZZZZZZZZ.ZZ-.
           03  A17           PIC ZZZZZZZZ.ZZ-.
           03  A18           PIC ZZZZZZZZ.ZZ-.
           03  A19           PIC ZZZZZ-.
           03  A20           PIC ZZZZZZZZZ.ZZ-.
           03  A21           PIC ZZZZZ-.
           03  A22           PIC ZZZZZZZZZ.ZZ-.
           03  A23           PIC ZZZZZ-.
           03  A24           PIC ZZZZZZZZZ.ZZ-.
           03  PROMPT-A25    PIC X(18).
           03  A25           PIC ZZZZZZZZ.ZZ-.

       01  DISPLAY-LIST1.
           03  FILLER        PIC X(48) VALUE
           "010 020 030 040 050 060 070 080 081 090 100 110 ".
           03  FILLER        PIC X(40) VALUE
           "111 120 130 140 150 160 170 180 190 200 ".
           03  FILLER        PIC X(31) VALUE
           "210 220 230 240 PROMPT-250 250.".

      ******************************************************************
      *    *      SCREEN DISPLAY BUFFER2
      ******************************************************************
       01  DISPLAY-BUF2.
           03  P2-COLUMNS  OCCURS 3.
               05  P2-ROWS OCCURS 12.
                   07  P2-DISP-AMT      PIC ZZ,ZZZ,ZZZ.ZZ-  BLANK ZERO.
           03  B13                      PIC 99/99/99        BLANK ZERO.
       01  DISPLAY-LIST2.
           03  FILLER        PIC X(48) VALUE
           "010 020 030 040 050 060 070 080 090 100 110 120 ".
           03  FILLER        PIC X(48) VALUE
           "011 021 031 041 051 061 071 081 091 101 111 121 ".
           03  FILLER        PIC X(48) VALUE
           "012 022 032 042 052 062 072 082 092 102 112 122 ".
           03  FILLER        PIC X(04) VALUE
           "130.".

      ******************************************************************
      *    *      SCREEN DISPLAY BUFFER3
      ******************************************************************
       01  DISPLAY-BUF3A      VALUE SPACES.
           03  DISPLAY-BUF3A-TAB  OCCURS 9.
               05  C01       PIC Z(6)-       OCCURS 3.
       01  DISPLAY-BUF3B      VALUE SPACES.
           03  DISPLAY-BUF3B-TAB  OCCURS 9.
               05  C02       PIC Z(9).ZZ-    OCCURS 3.

       01  DISPLAY-LIST3A.
           03  FILLER        PIC X(24) VALUE
           "010 011 012 020 021 022 ".
           03  FILLER        PIC X(24) VALUE
           "030 031 032 040 041 042 ".
           03  FILLER        PIC X(24) VALUE
           "050 051 052 060 061 062 ".
           03  FILLER        PIC X(24) VALUE
           "070 071 072 080 081 082 ".
           03  FILLER        PIC X(12) VALUE
           "090 091 092.".
       01  DISPLAY-LIST3B.
           03  FILLER        PIC X(24) VALUE
           "013 014 015 023 024 025".
           03  FILLER        PIC X(24) VALUE
           "033 034 035 043 044 045".
           03  FILLER        PIC X(24) VALUE
           "053 054 055 063 064 065".
           03  FILLER        PIC X(24) VALUE
           "073 074 075 083 084 085".
           03  FILLER        PIC X(12) VALUE
           "093 094 095.".


      ******************************************************************
      *    *      SCREEN DISPLAY BUFFER4
      ******************************************************************

       01  DISPLAY-BUF4A      VALUE SPACES.
           03  DISPLAY-BUF4A-TABLE  OCCURS 2.
               05  DISPLAY-BUF4A-TAB  OCCURS 5.
                   07  D01       PIC ZZZZZ-      OCCURS 3.
       01  DISPLAY-BUF4B      VALUE SPACES.
           03  DISPLAY-BUF4B-TABLE  OCCURS 2.
               05  DISPLAY-BUF4B-TAB  OCCURS 5.
                   07  D02       PIC ZZZZZZZ.ZZ- OCCURS 3.


       01  DISPLAY-LIST4A.
           03  FILLER        PIC X(40) VALUE
           "010 020 030 070 080 090 130 140 150 190 ".
           03  FILLER        PIC X(40) VALUE
           "200 210 250 260 270 310 320 330 370 380 ".
           03  FILLER        PIC X(28) VALUE
           "390 430 440 450 490 500 510 ".
           03  FILLER        PIC X(12) VALUE
           "550 560 570.".

       01  DISPLAY-LIST4B.
           03  FILLER        PIC X(40) VALUE
           "040 050 060 100 110 120 160 170 180 220 ".
           03  FILLER        PIC X(40) VALUE
           "230 240 280 290 300 340 350 360 400 410 ".
           03  FILLER        PIC X(28) VALUE
           "420 460 470 480 520 530 540 ".
           03  FILLER        PIC X(12) VALUE
           "580 590 600.".

      ******************************************************************
      *    SCREEN DISPLAY BUFFER ----- PAGE 5
      ******************************************************************
       01  DISPLAY-BUF5.
           03  DEL-TABLE                  OCCURS 10.
               05  DEL-REC-CON            OCCURS 2.
                   07  DEL-UNITS          PIC Z(6)9-.
                   07  DEL-OUTBAL         PIC Z(8)9.99-.
      **THESE ARE APPLICATION FIELDS FOR DISPLAY
      *    03  APP-GNLS-TABLE             OCCURS 2.
      *        05  DATA-GNLS-PENDING      PIC ZZZZ9-.
      *        05  DATA-GNLS-COND-APP     PIC ZZZZ9-.
      *        05  DATA-GNLS-APPROVED     PIC ZZZZ9-.
      *        05  DATA-GNLS-MADE         PIC ZZZZ9-.
      *        05  DATA-GNLS-TURNDOWN     PIC ZZZZ9-.
      *        05  DATA-GNLS-NOT-TAKEN    PIC ZZZZ9-.
      *    03  APP-NVOL-TABLE             OCCURS 2.
      *        05  DATA-NVOL-PENDING      PIC ZZZZ9-.
      *        05  DATA-NVOL-COND-APP     PIC ZZZZ9-.
      *        05  DATA-NVOL-APPROVED     PIC ZZZZ9-.
      *        05  DATA-NVOL-MADE         PIC ZZZZ9-.
      *        05  DATA-NVOL-TURNDOWN     PIC ZZZZ9-.
      *        05  DATA-NVOL-NOT-TAKEN    PIC ZZZZ9-.

      *-----------------------------------------------------------------
       01  DISPLAY-LIST5.
           03  FILLER                     PIC X(16) VALUE
               "010 011 012 013 ".
           03  FILLER                     PIC X(16) VALUE
               "020 021 022 023 ".
           03  FILLER                     PIC X(16) VALUE
               "030 031 032 033 ".
           03  FILLER                     PIC X(16) VALUE
               "040 041 042 043 ".
           03  FILLER                     PIC X(16) VALUE
               "050 051 052 053 ".
           03  FILLER                     PIC X(16) VALUE
               "060 061 062 063 ".
           03  FILLER                     PIC X(16) VALUE
               "070 071 072 073 ".
           03  FILLER                     PIC X(16)     VALUE
               "080 081 082 083 ".
           03  FILLER                     PIC X(16)     VALUE
               "090 091 092 093 ".
           03  FILLER                     PIC X(16)     VALUE
               "100 101 102 103 ".
      *THESE ARE  APPLICATION FIELDS FOR DISPLAY
      *    03  FILLER                     PIC X(24)     VALUE
      *        "110 111 112 113 114 115 ".
      *    03  FILLER                     PIC X(24)     VALUE
      *        "120 121 122 123 124 125 ".
      *    03  FILLER                     PIC X(24)     VALUE
      *        "130 131 132 133 134 135 ".
      *    03  FILLER                     PIC X(24)     VALUE
      *        "140 141 142 143 144 145.".

      *=================================================================
       01  MAX-ENTRY.
           03  FILLER        PIC X(10) VALUE "2513096010".
       01  MAX-TABLE REDEFINES MAX-ENTRY.
           03  MAX           PIC 99   OCCURS 5.

      ******************************************************************
      *    *   ELEMENT SPECIFICATIONS #1
      ******************************************************************
       01  SPEC-TABLE1.
           03  FILLER        PIC X(8) VALUE "ENNS082 ".
           03  FILLER        PIC X(8) VALUE "ENNS082 ".
           03  FILLER        PIC X(8) VALUE "ENNS082 ".
           03  FILLER        PIC X(8) VALUE "ENNS082 ".
           03  FILLER        PIC X(8) VALUE "ENNS082 ".
           03  FILLER        PIC X(8) VALUE "ENNS082 ".
           03  FILLER        PIC X(8) VALUE "ENNS082 ".
           03  FILLER        PIC X(8) VALUE "ENNS082 ".
           03  FILLER        PIC X(8) VALUE "ENNS082 ".
           03  FILLER        PIC X(8) VALUE "ENNS082 ".

           03  FILLER        PIC X(8) VALUE "ENNS092 ".
           03  FILLER        PIC X(8) VALUE "ENNS112 ".

      * DLR DISC VOLUME
           03  FILLER        PIC X(8) VALUE "ENNS082 ".
           03  FILLER        PIC X(8) VALUE "ENNS082 ".
           03  FILLER        PIC X(8) VALUE "ENNS082 ".

           03  FILLER        PIC X(8) VALUE "ENNS082 ".
           03  FILLER        PIC X(8) VALUE "ENNS082 ".
           03  FILLER        PIC X(8) VALUE "ENNS082 ".

           03  FILLER        PIC X(8) VALUE "ENNS050 ".
           03  FILLER        PIC X(8) VALUE "ENNS092 ".
           03  FILLER        PIC X(8) VALUE "ENNS050 ".
           03  FILLER        PIC X(8) VALUE "ENNS092 ".
           03  FILLER        PIC X(8) VALUE "ENNS050 ".
           03  FILLER        PIC X(8) VALUE "ENNS092 ".
           03  FILLER        PIC X(8) VALUE "ENNS082 ".
       01  SPEC-TAB1       REDEFINES SPEC-TABLE1.
           03  SPEC-REC1   OCCURS 25 TIMES.
               05  SPEC1     PIC X(7).
               05  CHFG1     PIC X.

      ******************************************************************
      *    *   ELEMENT SPECIFICATIONS #2
      ******************************************************************
       01  SPEC-TABLE2.
           03  FILLER    PIC X(8)    VALUE "ENNS082 ".
           03  FILLER    PIC X(8)    VALUE "ENNS082 ".
           03  FILLER    PIC X(8)    VALUE "ENNS082 ".
           03  FILLER    PIC X(8)    VALUE "ENNS082 ".
           03  FILLER    PIC X(8)    VALUE "ENNS082 ".
           03  FILLER    PIC X(8)    VALUE "ENNS082 ".
           03  FILLER    PIC X(8)    VALUE "ENNS082 ".
           03  FILLER    PIC X(8)    VALUE "ENNS082 ".
           03  FILLER    PIC X(8)    VALUE "ENNS082 ".
           03  FILLER    PIC X(8)    VALUE "ENNS082 ".
           03  FILLER    PIC X(8)    VALUE "ENNS082 ".
           03  FILLER    PIC X(8)    VALUE "ENNS082 ".
      * 13. DS-DELUNEARN-DATE
           03  FILLER    PIC X(8)    VALUE "ERNM060 ".

       01  SPEC-TAB2         REDEFINES SPEC-TABLE2.
           03  SPEC-REC2     OCCURS 13 TIMES.
               05  SPEC2     PIC X(7).
               05  CHFG2     PIC X.

      ******************************************************************
      *    *   ELEMENT SPECIFICATIONS #3
      ******************************************************************
       01  SPEC-TABLE3.
           03  FILLER    PIC X(8)    VALUE "ENNS060 ".
           03  FILLER    PIC X(8)    VALUE "ENNS060 ".
           03  FILLER    PIC X(8)    VALUE "ENNS060 ".
           03  FILLER    PIC X(8)    VALUE "ENNS060 ".
           03  FILLER    PIC X(8)    VALUE "ENNS060 ".
           03  FILLER    PIC X(8)    VALUE "ENNS060 ".
           03  FILLER    PIC X(8)    VALUE "ENNS060 ".
           03  FILLER    PIC X(8)    VALUE "ENNS060 ".
           03  FILLER    PIC X(8)    VALUE "ENNS060 ".

       01  SPEC-TAB3         REDEFINES SPEC-TABLE3.
           03  SPEC-REC3     OCCURS 9 TIMES.
               05  SPEC3     PIC X(7).
               05  CHFG3     PIC X.

      ******************************************************************
      *    *   ELEMENT SPECIFICATIONS #4
      ******************************************************************
       01  SPEC-TABLE4.
           03  FILLER    PIC X(8)    VALUE "ENNS050 ".
           03  FILLER    PIC X(8)    VALUE "ENNS050 ".
           03  FILLER    PIC X(8)    VALUE "ENNS050 ".
           03  FILLER    PIC X(8)    VALUE "ENNS072 ".
           03  FILLER    PIC X(8)    VALUE "ENNS072 ".
           03  FILLER    PIC X(8)    VALUE "ENNS072 ".

           03  FILLER    PIC X(8)    VALUE "ENNS050 ".
           03  FILLER    PIC X(8)    VALUE "ENNS050 ".
           03  FILLER    PIC X(8)    VALUE "ENNS050 ".
           03  FILLER    PIC X(8)    VALUE "ENNS072 ".
           03  FILLER    PIC X(8)    VALUE "ENNS072 ".
           03  FILLER    PIC X(8)    VALUE "ENNS072 ".

           03  FILLER    PIC X(8)    VALUE "ENNS050 ".
           03  FILLER    PIC X(8)    VALUE "ENNS050 ".
           03  FILLER    PIC X(8)    VALUE "ENNS050 ".
           03  FILLER    PIC X(8)    VALUE "ENNS072 ".
           03  FILLER    PIC X(8)    VALUE "ENNS072 ".
           03  FILLER    PIC X(8)    VALUE "ENNS072 ".

           03  FILLER    PIC X(8)    VALUE "ENNS050 ".
           03  FILLER    PIC X(8)    VALUE "ENNS050 ".
           03  FILLER    PIC X(8)    VALUE "ENNS050 ".
           03  FILLER    PIC X(8)    VALUE "ENNS072 ".
           03  FILLER    PIC X(8)    VALUE "ENNS072 ".
           03  FILLER    PIC X(8)    VALUE "ENNS072 ".

           03  FILLER    PIC X(8)    VALUE "ENNS050 ".
           03  FILLER    PIC X(8)    VALUE "ENNS050 ".
           03  FILLER    PIC X(8)    VALUE "ENNS050 ".
           03  FILLER    PIC X(8)    VALUE "ENNS072 ".
           03  FILLER    PIC X(8)    VALUE "ENNS072 ".
           03  FILLER    PIC X(8)    VALUE "ENNS072 ".

           03  FILLER    PIC X(8)    VALUE "ENNS050 ".
           03  FILLER    PIC X(8)    VALUE "ENNS050 ".
           03  FILLER    PIC X(8)    VALUE "ENNS050 ".
           03  FILLER    PIC X(8)    VALUE "ENNS072 ".
           03  FILLER    PIC X(8)    VALUE "ENNS072 ".
           03  FILLER    PIC X(8)    VALUE "ENNS072 ".

           03  FILLER    PIC X(8)    VALUE "ENNS050 ".
           03  FILLER    PIC X(8)    VALUE "ENNS050 ".
           03  FILLER    PIC X(8)    VALUE "ENNS050 ".
           03  FILLER    PIC X(8)    VALUE "ENNS072 ".
           03  FILLER    PIC X(8)    VALUE "ENNS072 ".
           03  FILLER    PIC X(8)    VALUE "ENNS072 ".

           03  FILLER    PIC X(8)    VALUE "ENNS050 ".
           03  FILLER    PIC X(8)    VALUE "ENNS050 ".
           03  FILLER    PIC X(8)    VALUE "ENNS050 ".
           03  FILLER    PIC X(8)    VALUE "ENNS072 ".
           03  FILLER    PIC X(8)    VALUE "ENNS072 ".
           03  FILLER    PIC X(8)    VALUE "ENNS072 ".

           03  FILLER    PIC X(8)    VALUE "ENNS050 ".
           03  FILLER    PIC X(8)    VALUE "ENNS050 ".
           03  FILLER    PIC X(8)    VALUE "ENNS050 ".
           03  FILLER    PIC X(8)    VALUE "ENNS072 ".
           03  FILLER    PIC X(8)    VALUE "ENNS072 ".
           03  FILLER    PIC X(8)    VALUE "ENNS072 ".

           03  FILLER    PIC X(8)    VALUE "ENNS050 ".
           03  FILLER    PIC X(8)    VALUE "ENNS050 ".
           03  FILLER    PIC X(8)    VALUE "ENNS050 ".
           03  FILLER    PIC X(8)    VALUE "ENNS072 ".
           03  FILLER    PIC X(8)    VALUE "ENNS072 ".
           03  FILLER    PIC X(8)    VALUE "ENNS072 ".

       01  SPEC-TAB4         REDEFINES SPEC-TABLE4.
           03  SPEC-REC4     OCCURS 60 TIMES.
               05  SPEC4     PIC X(7).
               05  CHFG4     PIC X.

      ******************************************************************
      *    *   ELEMENT SPECIFICATIONS #5
      ******************************************************************
       01  SPEC-TABLE5.
      * 1. CUR:
           03  FILLER     PIC X(8) VALUE "ENNS070 ".
      * 2. 30:
           03  FILLER     PIC X(8) VALUE "ENNS070 ".
      * 3. 60:
           03  FILLER     PIC X(8) VALUE "ENNS070 ".
      * 4. 90:
           03  FILLER     PIC X(8) VALUE "ENNS070 ".
      * 5. 120:
           03  FILLER     PIC X(8) VALUE "ENNS070 ".
      * 6. 150:
           03  FILLER     PIC X(8) VALUE "ENNS070 ".
      * 7. 180:
           03  FILLER     PIC X(8) VALUE "ENNS070 ".
      * 8. 210+:
           03  FILLER     PIC X(8) VALUE "ENNS070 ".
      * 9. <M>:
           03  FILLER     PIC X(8) VALUE "ENNS070 ".
      * 10. <P>:
           03  FILLER     PIC X(8) VALUE "ENNS070 ".

       01  SPEC-TAB5       REDEFINES SPEC-TABLE5.
           03  SPEC-REC5   OCCURS 10 TIMES.
               05  SPEC5   PIC X(7).
               05  CHFG5   PIC X.

      ******************************************************************
      *    *   ELEMENT SPECIFICATION WORKER
      ******************************************************************
       01  SPEC-TAB.
           03  SPEC-REC.
               05  SPEC      PIC X(7).
               05  CHFG      PIC X.

      ******************************************************************
      *
      *     C O P Y    L I B R A R Y    W O R K E R S
      *
      ******************************************************************
       COPY "LIBWI/DLSCANW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/PHON400.CPY".
       COPY "LIBLP/LPDSCLW.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBMN/DSMAN1_DEF.CPY".
       COPY "LIBMN/DSMAN2_DEF.CPY".
       COPY "LIBMN/DSMAN3_DEF.CPY".
       COPY "LIBMN/DSMAN4_DEF.CPY".
       COPY "LIBMN/DSMAN5_DEF.CPY".
       COPY "LIBMN/DSMAN1_WKS.CPY".
       COPY "LIBMN/DSMAN2_WKS.CPY".
       COPY "LIBMN/DSMAN3_WKS.CPY".
       COPY "LIBMN/DSMAN4_WKS.CPY".
       COPY "LIBMN/DSMAN5_WKS.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/GPENVW.CPY".
       COPY "LIBGB/ACCESSW.CPY".

       COPY "LIBGB/GETFMW.CPY".

       SCREEN SECTION.
       COPY "LIBMN/DSMAN1_SCN.CPY".
       COPY "LIBMN/DSMAN2_SCN.CPY".
       COPY "LIBMN/DSMAN3_SCN.CPY".
       COPY "LIBMN/DSMAN4_SCN.CPY".
       COPY "LIBMN/DSMAN5_SCN.CPY".

       PROCEDURE DIVISION
            USING FORM-PATHNAME EXIT-PATHNAME.

      ******************************************************************
      *
      *    M A I N   P R O G R A M    M O D U L E
      *
      ******************************************************************
       MAIN-PROGRAM SECTION.
           PERFORM SQL-CONNECT.

      D    STOP MESS.

           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE FORM-NAME(1) TO FORM-NAM.
           MOVE FORM-PATH TO FORM-PATHNAME.
           MOVE 1 TO CUR-FORM.

       COPY "LIBGB/CHKSEC.CPY".

           DISPLAY FLOATING GRAPHICAL WINDOW
                   SCREEN LINE 01 * VDU-WIN-L-OFFSET
                   SCREEN COL 01 * VDU-WIN-C-OFFSET
           COLOR IS VDU-WCOLOR-TCLP,CONTROL VALUE IS VDU-WCTRL-TCLP
                   CELL SIZE IS LABEL FONT
                   SIZE 80 + VDU-WIN-W-MOD
                   LINES 24 + VDU-WIN-H-MOD
                   BLANK SCREEN
                   HANDLE IS DSMAIN-WINDOW-HANDLE.

       MAIN-MODULE.

           MOVE 1 TO HEADER-LEV HEADER-LEVEL DELINQ DELTYP-COD.

           PERFORM GET-GPENV.

           PERFORM FORM-RESET.

           IF GP-DLR-DISP-TYPE = "C"
              MOVE "COMM" TO DLR-TYP-BUF
           ELSE
              MOVE "DISC" TO DLR-TYP-BUF.
           MOVE DLR-TYP-BUF TO WR-BUF.
           MOVE DLR-TYP-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

           PERFORM REC-DS-MODULE.

           IF IO-TYPE = "END"
              GO TO END-ROUTINE.

           IF IO-TYPE = "CLR"
             GO TO MAIN-MODULE.

           PERFORM ENTRY-MODULE.
           IF IO-TYPE = "RWR"
              PERFORM WRITE-MODULE.
           IF IO-TYPE = "DEL"
             PERFORM DELETE-MODULE.

           IF FORM-SEL NOT = 0
              PERFORM SETUP-NEW-FORM.

           GO TO MAIN-MODULE.

       SETUP-NEW-FORM.
           PERFORM FORM-DESTROY.
           MOVE FORM-SEL TO CUR-FORM.
           MOVE 0 TO FIRST-TIME.
           MOVE FORM-NAME(CUR-FORM) TO FORM-NAM.
           MOVE FORM-PATH TO FORM-PATHNAME.
           PERFORM FORM-RESET.

           MOVE DISPLAY-DATE TO WR-BUF.
           MOVE DATE-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.


      ******************************************************************
       REC-DS-MODULE SECTION.
      ******************************************************************
           IF FORM-SEL = 0
              MOVE ZEROS TO DS1-KEY.

       REC-DS-MODULE-NO.

           MOVE SPACES TO IO-TYPE.
           MOVE 1      TO PERIOD.

           IF FORM-SEL = 0
              PERFORM GET-TODAYS-DATE.
              GO TO SETUP-DS-DATE-CCYYMM.

           GO TO ENTER-BRANCH.

       ENTER-DATE.
           MOVE " ENTER DATE (MMYY):" TO MESS.
           PERFORM CALL-TMMSG.
           MOVE "EN N040MMYY." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "1>"
              MOVE "END" TO IO-TYPE
              GO TO END-REC-DS-MODULE.

           IF VDUSTATUS = "16"
              MOVE SPACES TO DISPLAY-DATEX HOLD-DS-DATEX-CCYYMM
              GO TO ENTER-BRANCH.

           IF VDUSTATUS = "1<"
             MOVE 0 TO HOLD-DS-DATE-CCYYMM HOLD-DS-BRANCH
                       HOLD-DS-DLNO HOLD-DS-CLASS
             MOVE 0 TO DS-DATE-CCYYMM DS-BRANCH
                       DS-DLNO DS-CLASS 

             PERFORM ENTER-SCANKEY
             GO TO END-REC-DS-MODULE.

           IF VDUSTATUS NOT = "00"
              GO TO REC-DS-MODULE-NO.

           IF VDUNUM0 = 0
              GO TO REC-DS-MODULE-NO.

           MOVE VDUNUM0 TO DISPLAY-DATE.

       SETUP-DS-DATE-CCYYMM.
           MOVE DISPLAY-MO TO DATE-MMDDYY-MM.
           MOVE DISPLAY-YR TO DATE-MMDDYY-YY.
           PERFORM CONVERT-MMDDYY-TO-YYYYMM.
           MOVE DATE-YYYYMM TO HOLD-DS-DATE-CCYYMM DS-DATE-CCYYMM.

       ENTER-BRANCH.

           IF FORM-SEL NOT = 0
              PERFORM DISPLAY-SCAN-KEY
              GO TO READ-DSFILE.

           MOVE 0 TO NEW-FLAG.
           MOVE "CLSDESC CLASS DLNAME DLNO BRNO." TO WR-LIST.
           MOVE SPACES TO WR-BUF.
           PERFORM CALL-WRFORM.

           IF CUR-FORM NOT = 2
              MOVE "BRDESC." TO WR-LIST
              MOVE SPACES TO WR-BUF
              PERFORM CALL-WRFORM.

           MOVE " ENTER BRANCH:" TO MESS.
           PERFORM CALL-TMMSG.
           MOVE "EN N040BRNO." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "1>"
             MOVE "END" TO IO-TYPE
             GO TO END-REC-DS-MODULE.

           IF VDUSTATUS = "1U"
              GO TO ENTER-DATE.

           IF VDUSTATUS = "1<"
             MOVE 0 TO HOLD-DS-BRANCH HOLD-DS-DLNO HOLD-DS-CLASS
             PERFORM ENTER-SCANKEY
             GO TO END-REC-DS-MODULE.

           IF VDUSTATUS NOT = "00"
             GO TO ENTER-BRANCH.

           IF VDUNUM0 = 0
              MOVE "SN N040BRNO." TO VDU
              MOVE HOLD-DS-BRANCH TO VDUNUM0
              PERFORM SCREENIO.
           MOVE VDUNUM0 TO DS-BRANCH HOLD-DS-BRANCH.
           PERFORM VERIFY-BRNO.

           IF IO-FG NOT = 0
             GO TO ENTER-BRANCH.

           GO TO REC-DS-DLNO.

       VERIFY-BRNO.
           PERFORM OPEN-BR-FILE.
           MOVE DS-BRANCH TO BR-NO.
           PERFORM READ-BR-FILE.
           PERFORM CLOSE-BR-FILE.

           IF IO-FG NOT = 0
              MOVE "* NOT ON FILE *" TO BR-NAME.

           IF CUR-FORM NOT = 2
              MOVE "S  A200BRDESC." TO VDU
              MOVE BR-NAME TO VDUBUF
              PERFORM SCREENIO.

       REC-DS-DLNO.
           MOVE "DLNAME." TO WR-LIST.
           MOVE SPACES TO WR-BUF.
           PERFORM CALL-WRFORM.
           MOVE " ENTER DEALER, F2-DEALER SCAN, F6-SCAN NEXT" TO MESS.
           PERFORM CALL-TMMSG.
           MOVE "ENNN042DLNO." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "10"
              GO TO REC-DS-MODULE-NO.

           IF VDUSTATUS = "1U"
              GO TO ENTER-BRANCH.

           IF VDUSTATUS = "1<"
             MOVE 0 TO HOLD-DS-DLNO HOLD-DS-CLASS
             PERFORM ENTER-SCANKEY
             GO TO END-REC-DS-MODULE.

           IF VDUSTATUS = "12"
              PERFORM DEALER-SCAN
                 IF IO-FG = 9
                    GO TO REC-DS-DLNO
                 ELSE
                    GO TO CONTINUE-DEALER.

           IF VDUSTATUS NOT = "00"
             GO TO REC-DS-DLNO.

       CONTINUE-DEALER.
           MOVE VDUNUM2 TO DS-DLNO HOLD-DS-DLNO.
           PERFORM VERIFY-DLNO.

           GO TO REC-DS-CLASS.

       VERIFY-DLNO.

           MOVE EXT-FILPATH-BASE   TO FILPATH-BASE.
           MOVE BR-MACHINE         TO FILPATH-MACHINE.
           MOVE BR-DATA-PATH       TO FILPATH-DATA.
           MOVE "B"                TO DL-PATH-OVERRIDE.

           PERFORM OPEN-DL1-FILE.
           MOVE DS-NUM TO DL-NUM.
           PERFORM READ-DL1-FILE.
           PERFORM CLOSE-DL1-FILE.
           IF IO-FG NOT = 0
              MOVE "* NOT ON FILE *" TO DL-NAME.

           MOVE "S  A250DLNAME." TO VDU.
           MOVE DL-NAME TO VDUBUF.
           PERFORM SCREENIO.

       REC-DS-CLASS.
           MOVE "CLSDESC." TO WR-LIST.
           MOVE SPACES     TO WR-BUF.
           PERFORM CALL-WRFORM.

           MOVE SPACES TO MESS.
           PERFORM CALL-TMMSG.
           MOVE "EN N020CLASS." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "10"
              GO TO REC-DS-MODULE-NO.

           IF VDUSTATUS = "1U"
              GO TO REC-DS-DLNO.
           IF VDUSTATUS = "1<"
             MOVE 0 TO HOLD-DS-CLASS
             PERFORM ENTER-SCANKEY
             GO TO END-REC-DS-MODULE.

           IF VDUSTATUS NOT = "00"
             GO TO REC-DS-CLASS.

           MOVE VDUNUM0 TO DS-CLASS HOLD-DS-CLASS.
           PERFORM VERIFY-CLASS.

           IF IO-FG NOT = 0
              GO TO REC-DS-CLASS.

           GO TO READ-DSFILE.

       VERIFY-CLASS.
           MOVE DS-CLASS TO LC-CODE.
           PERFORM OPEN-LC1-FILE.
           PERFORM READ-LC1-FILE.
           PERFORM CLOSE-LC1-FILE.
           IF IO-FG NOT = 0
              MOVE "* NOT ON FILE *" TO LC-DESC.

           MOVE "S  A160CLSDESC." TO VDU.
           MOVE LC-DESC TO VDUBUF.
           PERFORM SCREENIO.

       READ-DSFILE.

           MOVE EXT-FILPATH-BASE    TO FILPATH-BASE.
           MOVE BR-MACHINE          TO FILPATH-MACHINE.
           MOVE BR-DATA-PATH        TO FILPATH-DATA.

           MOVE "B"                 TO DS-PATH-OVERRIDE.

           PERFORM OPEN-DS1-FILE.
           MOVE DS1-KEY TO HOLD-DS1-KEY.
           PERFORM READ-DS1-FILE.
           PERFORM CLOSE-DS1-FILE.
           IF IO-FG = 0
              MOVE DS-REC TO ORIGINAL-DS-REC
              PERFORM DISPLAY-FIELDS
              MOVE 0 TO NEW-FLAG
              GO TO END-REC-DS-MODULE.

           IF CUR-FORM NOT = 1
              IF FORM-SEL = 0
                 GO TO REC-DS-MODULE-NO
              ELSE
                 MOVE 1 TO NEW-FLAG
                 PERFORM DISPLAY-FIELDS
                 GO TO END-REC-DS-MODULE.

           PERFORM NEW-RECORD-SETUP.

       END-REC-DS-MODULE.
           MOVE 0 TO FORM-SEL.

      ******************************************************************
       ENTER-SCANKEY SECTION.
      ******************************************************************

           MOVE " F1-CLEAR, F6-SCAN NEXT" TO MESS.
           PERFORM CALL-TMMSG.

       ENTER-KEY.
           MOVE "E RA160SCANKEY." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO CLOSE-SCANKEY.

           IF VDUSTATUS = "1<"
              IF FIRST-SCAN NOT = 0
                 GO TO READ-FILES-NEXT
              ELSE
                 MOVE EXT-FILPATH-BRNO TO BR-NO
                 PERFORM OPEN-BR-FILE
                 PERFORM READ-BR-FILE
                 PERFORM CLOSE-BR-FILE
                 IF IO-FG NOT = 0
                    MOVE "BRANCH NOT ON FILE" TO MESS
                    PERFORM SEND-MESS
                    GO TO CLOSE-SCANKEY
                 END-IF
                 MOVE EXT-FILPATH-BASE   TO FILPATH-BASE
                 MOVE BR-MACHINE         TO FILPATH-MACHINE
                 MOVE BR-DATA-PATH       TO FILPATH-DATA
                 MOVE "B"                TO DS-PATH-OVERRIDE
                 PERFORM OPEN-DS1-FILE
                 PERFORM START-DS1-FILE
                 MOVE 1 TO FIRST-SCAN
                 GO TO READ-FILES-NEXT.

           IF VDUSTATUS NOT = "00"
              GO TO ENTER-KEY.

           IF VDUBUF = SPACES
              IF FIRST-SCAN NOT = 0
                 GO TO CLOSE-SCANKEY
              ELSE
                 GO TO ENTER-KEY.

           MOVE VDUBUF TO WS-DS1-KEY.
           MOVE WS-DS-DATE-MMYY TO DATE-MMYY.
           PERFORM CONVERT-MMYY-TO-MMDDYY.
           PERFORM CONVERT-MMDDYY-TO-YYYYMM.

           MOVE DATE-YYYYMM     TO DS-DATE-CCYYMM.
           MOVE WS-DS-BRANCH    TO DS-BRANCH.
           MOVE WS-DS-DLNO      TO DS-DLNO.
           MOVE WS-DS-CLASS     TO DS-CLASS.
  
           PERFORM OPEN-BR-FILE.
           MOVE DS-BRANCH TO BR-NO.
           PERFORM READ-BR-FILE.
           PERFORM CLOSE-BR-FILE.
           IF IO-FG NOT = 0
              MOVE "BRANCH NOT ON FILE" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-KEY.
 
           MOVE EXT-FILPATH-BASE   TO FILPATH-BASE.
           MOVE BR-MACHINE         TO FILPATH-MACHINE.
           MOVE BR-DATA-PATH       TO FILPATH-DATA.
           MOVE "B"                TO DS-PATH-OVERRIDE.

           PERFORM OPEN-DS1-FILE.

           PERFORM START-DS1-FILE.

           MOVE 1 TO FIRST-SCAN.

       READ-FILES-NEXT.

           PERFORM READ-DS1-FILE-NEXT.
           IF IO-FG = 9
              MOVE "CLR" TO IO-TYPE
              GO TO CLOSE-SCANKEY.
      *    UNLOCK DS-FILE.

      * DS-BRANCH IS BELOW THE DATE, MOVES 0 TO ALL FIELDS FOR SCAN
      * SO IF BRANCH 2, YOU WILL GET BRANCH 1 FIRST, KEEP READING, DONT EXIT
      * I DONT LIKE THIS, THE MAINTENANCE ALLOWS ACCESS TO ANY BRANCH AND
      * I KEYED IN BRANCH AS PART OF KEY, NOT TO START AT BEGINNING
           IF (DS-BRANCH NOT = DS-PATH-OWNBR)
              GO TO READ-FILES-NEXT.

           MOVE DS-REC TO ORIGINAL-DS-REC.
           MOVE DS1-KEY TO HOLD-DS1-KEY.
           PERFORM DISPLAY-SCAN-KEY.
           PERFORM DISPLAY-FIELDS.
           MOVE 1 TO FIRST-SCAN.
           GO TO ENTER-KEY.

      *-----------------------------------------------------------------
       DISPLAY-SCAN-KEY.

           MOVE "SNNN040MMYY." TO VDU.
           MOVE DS-DATE-CCYYMM TO DATE-YYYYMM.
           MOVE DATE-YYYYMM-YY TO DISPLAY-YR.
           MOVE DATE-YYYYMM-MM TO DISPLAY-MO.
           MOVE DISPLAY-DATE TO VDUNUM0.
           PERFORM SCREENIO.

           MOVE "SN N040BRNO." TO VDU.
           MOVE DS-BRANCH TO VDUNUM0.
           PERFORM SCREENIO.

           MOVE "SN N040BRNO." TO VDU.
           MOVE DS-BRANCH TO VDUNUM0.
           PERFORM SCREENIO.

           IF CUR-FORM NOT = 2
              PERFORM VERIFY-BRNO.

           MOVE "SNNN042DLNO." TO VDU.
           MOVE DS-DLNO TO VDUNUM2.
           PERFORM SCREENIO.
           PERFORM VERIFY-DLNO.
           MOVE "S  A250DLNAME." TO VDU.
           MOVE DL-NAME TO VDUBUF.
           PERFORM SCREENIO.
           MOVE "SN N020CLASS." TO VDU.
           MOVE DS-CLASS TO VDUNUM0.
           PERFORM SCREENIO.
           PERFORM VERIFY-CLASS.

       CLOSE-SCANKEY.
           MOVE SPACES TO DS-PATH-OVERRIDE.

           MOVE 0 TO FIRST-SCAN.
           PERFORM CLOSE-DS1-FILE.
           MOVE SPACES TO MESS.
           PERFORM CALL-TMMSG.

       END-SCANKEY.
           EXIT.

      ****************************************
       DEALER-SCAN SECTION.
      ****************************************
           MOVE 0 TO DLSCAN-NUM.
           MOVE "WI/DLSCAN " TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH DLSCAN-BUF EXIT-PATHNAME.
           CANCEL FORM-PROGX.
           IF DLSCAN-STATUS NOT = "00" OR DLSCAN-NUM = " "
               MOVE 9 TO IO-FG
               GO TO EXIT-DEALER-SCAN.
           MOVE "SN N042DLNO." TO VDU.
           MOVE DLSCAN-DLNO TO VDUNUM2.
           PERFORM SCREENIO.
           MOVE DLSCAN-DLNO TO VDUNUM2.
           MOVE 0 TO IO-FG.

       EXIT-DEALER-SCAN.
           EXIT.


      ******************************************************************
      *    ENTRY-MODULE
      *
      *    NEW-FLAG = 0 - MODIFY EXISTING RECORD
      *               1 - INITIAL NEW RECORD ENTRY
      *               2 - MODIFY NEW RECORD
      ******************************************************************
       ENTRY-MODULE SECTION.
       BEGIN-ENTRY.
           MOVE SPACES TO IO-TYPE.
           MOVE 1 TO ONE.
           MOVE 0 TO ELE.

       CREATE-ROUTINE.
           IF NEW-FLAG NOT = 1
              GO TO MODIFY-ROUTINE.

           ADD ONE TO ELE.

           IF ELE < 1
              MOVE 1 TO ELE ONE.

           IF ( CUR-FORM = 1 ) AND ( ELE = 25 )
              IF ( DL-AFP-DEALER NOT = "Y" )
                 GO TO CREATE-ROUTINE.

           IF CUR-FORM = 1 AND ELE = 18
                 GO TO CREATE-ROUTINE.

           IF CUR-FORM = 5 AND ELE = 11 OR 12 OR 13 OR 14
                 GO TO CREATE-ROUTINE.

           IF ELE NOT > MAX(CUR-FORM)
              GO TO ENTER-ELE.

           MOVE 2 TO NEW-FLAG.

       MODIFY-ROUTINE.
           MOVE "R RA060SEL." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.

           IF ( CUR-FORM = 5                                   ) AND
              ( F2-KEY OR F3-KEY OR F4-KEY OR F5-KEY OR F6-KEY )
              PERFORM TOGGLE-ON-DELINQ
              GO TO MODIFY-ROUTINE.

           IF VDUSTATUS NOT = "00"
              GO TO MODIFY-ROUTINE.

           MOVE VDUBUF TO ACTION.

           IF ACTION = SPACES
              MOVE "RWR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.

           IF ACTION = "DELETE"
              IF CUR-FORM = 1
                 MOVE "DEL" TO IO-TYPE
                 GO TO END-ENTRY-MODULE.

           IF ACTION = "    P1"
              IF CUR-FORM NOT = 1
                 MOVE "RWR" TO IO-TYPE
                 MOVE 1 TO FORM-SEL
                 GO TO END-ENTRY-MODULE.

           IF ACTION = "    P2"
              IF CUR-FORM NOT = 2
                 MOVE 2 TO FORM-SEL
                 IF NEW-FLAG = 0
                    MOVE "RWR" TO IO-TYPE
                    GO TO END-ENTRY-MODULE
                 ELSE
                    GO TO END-ENTRY-MODULE.

           IF ACTION = "    P3"
              IF CUR-FORM NOT = 3
                 MOVE 3 TO FORM-SEL
                 IF NEW-FLAG = 0
                    MOVE "RWR" TO IO-TYPE
                    GO TO END-ENTRY-MODULE
                 ELSE
                    GO TO END-ENTRY-MODULE.

           IF ACTION = "    P4"
              IF CUR-FORM NOT = 4
                 MOVE 4 TO FORM-SEL
                 IF NEW-FLAG = 0
                    MOVE "RWR" TO IO-TYPE
                    GO TO END-ENTRY-MODULE
                 ELSE
                    GO TO END-ENTRY-MODULE.

           IF ACTION = "    P5"
              IF CUR-FORM NOT = 5
                 MOVE 5 TO FORM-SEL
                 IF NEW-FLAG = 0
                    MOVE "RWR" TO IO-TYPE
                    GO TO END-ENTRY-MODULE
                 ELSE
                    GO TO END-ENTRY-MODULE.

           INSPECT ACTION REPLACING ALL SPACES
              BY ZEROS.

           IF CHNO NOT NUMERIC
              GO TO MODIFY-ROUTINE.

           IF CHNO = 0
              GO TO MODIFY-ROUTINE.

           IF CHNO > MAX(CUR-FORM)
              GO TO MODIFY-ROUTINE.

           MOVE CHNO TO ELE.

           IF ( CUR-FORM = 1 ) AND ( ELE = 25 )
              IF ( DL-AFP-DEALER NOT = "Y" )
                 GO TO MODIFY-ROUTINE.

           IF CUR-FORM = 1 AND ELE = 18
                 GO TO MODIFY-ROUTINE.

      *-----------------------------------------------------------------
       ENTER-ELE.
           IF CUR-FORM = 1
              MOVE SPEC-REC1(ELE) TO SPEC-REC.

           IF CUR-FORM = 2
              MOVE SPEC-REC2(ELE) TO SPEC-REC.

           IF CUR-FORM = 3
              MOVE SPEC-REC3(ELE) TO SPEC-REC.

           IF CUR-FORM = 4
              MOVE SPEC-REC4(ELE) TO SPEC-REC.

           IF CUR-FORM = 5
              MOVE SPEC-REC5(ELE) TO SPEC-REC.

           IF CHKEY NOT = ZEROS
              IF CHFG = " "
                 IF NEW-FLAG NOT = 1
                    GO TO MODIFY-ROUTINE.

           IF CHFG = "N"
              IF NEW-FLAG = 1
                 GO TO CREATE-ROUTINE
            ELSE
              IF CHKEY NOT = CHAN
                 GO TO MODIFY-ROUTINE.

           MOVE SPEC TO VDU.
           MOVE ELE TO VDUROW.
           MOVE 0 TO VDUCOL.
           MOVE "." TO VDUDECIMAL.

           IF ( CUR-FORM = 5 ) AND ( ELE = 5 )
              GO TO GOTO-FIELD5.

           PERFORM SCREENIO.

       TEST-F4.
           IF VDUSTATUS = "1U"
             IF NEW-FLAG = 1
              MOVE -1 TO ONE
              GO TO CREATE-ROUTINE.

       TEST-F5.
           IF VDUSTATUS = "1D"
              MOVE 1 TO ONE
              GO TO GOTO-FIELD.

       TEST-F1.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.

           IF VDUSTATUS NOT = "00"
              GO TO ENTER-ELE.

           MOVE 1 TO ONE.

      *=================================================================
      *
      *    G O T O - F I E L D
      *
      *=================================================================
       GOTO-FIELD.
           GO TO GOTO-FIELD1
                 GOTO-FIELD2
                 GOTO-FIELD3
                 GOTO-FIELD4
                 GOTO-FIELD5
                 DEPENDING ON CUR-FORM.

      *=================================================================
      *
      *    G O T O - F I E L D ----- PAGE 1
      *
      *=================================================================
       GOTO-FIELD1.
           GO TO A-01 A-02 A-03 A-04 A-05 A-06 A-07 A-08 A-09 A-10
                 A-11 A-12 A-13 A-14 A-15 A-16 A-17 A-18 A-19 A-20
                 A-21 A-22 A-23 A-24 A-25
                 DEPENDING ON ELE.
           GO TO END-ENTRY-MODULE.

      *=================================================================
      *
      *    G O T O - F I E L D ----- PAGE 2
      *
      *=================================================================
       GOTO-FIELD2.
           IF ELE < 7
              MOVE 1 TO ACT
              MOVE ELE TO TYP
           ELSE
              MOVE 2 TO ACT
              COMPUTE TYP = ELE - 6.

           GO TO B-01 B-01 B-01 B-01 B-01 B-01 B-01
                 B-01 B-01 B-01 B-01 B-01 B-13
                 DEPENDING ON ELE.
           GO TO END-ENTRY-MODULE.

      *=================================================================
      *
      *    G O T O - F I E L D ----- PAGE 3
      *
      *=================================================================
       GOTO-FIELD3.

           GO TO FIND-P3-01 FIND-P3-02 FIND-P3-03
                 FIND-P3-04 FIND-P3-05 FIND-P3-06
                 FIND-P3-07 FIND-P3-08 FIND-P3-09
                 DEPENDING ON ELE.

      *-----------------------------------------------------------------
      *    DS-OUTSUNIT DSCUR
      *-----------------------------------------------------------------
        FIND-P3-01.
           MOVE VDUNUM0 TO DS-OUTSUNIT(DSCUR).
      *-----------------------------------------------
      *    DS-OUTSUNIT DSELM
      *-----------------------------------------------
        FIND-P3-011.
           MOVE "ENNS060011." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO FIND-P3-011.
           MOVE VDUNUM0 TO DS-OUTSUNIT(DSELM).
      *-----------------------------------------------
      *    DS-OUTSUNIT DSELY
      *-----------------------------------------------
        FIND-P3-012.
           MOVE "ENNS060012." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO FIND-P3-011.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO FIND-P3-012.
           MOVE VDUNUM0 TO DS-OUTSUNIT(DSELY).
      *-----------------------------------------------
      *    DS-OUTSAMT DSCUR
      *-----------------------------------------------
        FIND-P3-013.
           MOVE "ENNS092013." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO FIND-P3-012.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO FIND-P3-013.
           MOVE VDUNUM2 TO DS-OUTSAMT(DSCUR).
      *-----------------------------------------------
      *    DS-OUTSAMT DSELM
      *-----------------------------------------------
        FIND-P3-014.
           MOVE "ENNS092014." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO FIND-P3-013.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO FIND-P3-014.
           MOVE VDUNUM2 TO DS-OUTSAMT(DSELM).
      *-----------------------------------------------
      *    DS-OUTSAMT DSELY
      *-----------------------------------------------
        FIND-P3-015.
           MOVE "ENNS092015." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO FIND-P3-014.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO FIND-P3-015.
           MOVE VDUNUM2 TO DS-OUTSAMT(DSELY).

           GO TO CREATE-ROUTINE.

      *----------------------------------------------------------------
      *    DS-PLOSUNIT DSCUR
      *----------------------------------------------------------------
        FIND-P3-02.
           MOVE VDUNUM0 TO DS-PLOSUNIT(DSCUR).
      *-----------------------------------------------
      *    DS-PLOSUNIT DSELM
      *-----------------------------------------------
        FIND-P3-021.
           MOVE "ENNS060021." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO FIND-P3-021.
           MOVE VDUNUM0 TO DS-PLOSUNIT(DSELM).
      *-----------------------------------------------
      *    DS-PLOSUNIT DSELY
      *-----------------------------------------------
        FIND-P3-022.
           MOVE "ENNS060022." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO FIND-P3-021.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO FIND-P3-022.
           MOVE VDUNUM0 TO DS-PLOSUNIT(DSELY).
      *-----------------------------------------------
      *    DS-PLOSAMT DSCUR
      *-----------------------------------------------
        FIND-P3-023.
           MOVE "ENNS092023." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO FIND-P3-022.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO FIND-P3-023.
           MOVE VDUNUM2 TO DS-PLOSAMT(DSCUR).
      *-----------------------------------------------
      *    DS-PLOSAMT DSELM
      *-----------------------------------------------
        FIND-P3-024.
           MOVE "ENNS092024." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO FIND-P3-023.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO FIND-P3-024.
           MOVE VDUNUM2 TO DS-PLOSAMT(DSELM).
      *-----------------------------------------------
      *    DS-PLOSAMT DSELY
      *-----------------------------------------------
        FIND-P3-025.
           MOVE "ENNS092025." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO FIND-P3-024.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO FIND-P3-025.
           MOVE VDUNUM2 TO DS-PLOSAMT(DSELY).

           GO TO CREATE-ROUTINE.

      *--------------------------------------------------------------
      *    DS-RCPLUNIT DSCUR
      *--------------------------------------------------------------
        FIND-P3-03.
           MOVE VDUNUM0 TO DS-RCPLUNIT(DSCUR).
      *-----------------------------------------------
      *    DS-RCPLUNIT DSELM
      *-----------------------------------------------
        FIND-P3-031.
           MOVE "ENNS060031." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO FIND-P3-031.
           MOVE VDUNUM0 TO DS-RCPLUNIT(DSELM).
      *-----------------------------------------------
      *    DS-RCPLUNIT DSELY
      *-----------------------------------------------
        FIND-P3-032.
           MOVE "ENNS060032." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO FIND-P3-031.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO FIND-P3-032.
           MOVE VDUNUM0 TO DS-RCPLUNIT(DSELY).
      *-----------------------------------------------
      *    DS-RCPLAMT DSCUR
      *-----------------------------------------------
        FIND-P3-033.
           MOVE "ENNS092033." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO FIND-P3-032.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO FIND-P3-033.
           MOVE VDUNUM2 TO DS-RCPLAMT(DSCUR).
      *-----------------------------------------------
      *    DS-RCPLAMT DSELM
      *-----------------------------------------------
        FIND-P3-034.
           MOVE "ENNS092034." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO FIND-P3-033.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO FIND-P3-034.
           MOVE VDUNUM2 TO DS-RCPLAMT(DSELM).
      *-----------------------------------------------
      *    DS-RCPLAMT DSELY
      *-----------------------------------------------
        FIND-P3-035.
           MOVE "ENNS092035." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO FIND-P3-034.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO FIND-P3-035.
           MOVE VDUNUM2 TO DS-RCPLAMT(DSELY).

           GO TO CREATE-ROUTINE.


      *--------------------------------------------------------------
      *    DS-JDOSUNIT DSCUR
      *--------------------------------------------------------------
        FIND-P3-04.
           MOVE VDUNUM0 TO DS-JDOSUNIT(DSCUR).
      *-----------------------------------------------
      *    DS-JDOSUNIT DSELM
      *-----------------------------------------------
        FIND-P3-041.
           MOVE "ENNS060041." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO FIND-P3-041.
           MOVE VDUNUM0 TO DS-JDOSUNIT(DSELM).
      *-----------------------------------------------
      *    DS-JDOSUNIT DSELY
      *-----------------------------------------------
        FIND-P3-042.
           MOVE "ENNS060042." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO FIND-P3-041.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO FIND-P3-042.
           MOVE VDUNUM0 TO DS-JDOSUNIT(DSELY).
      *-----------------------------------------------
      *    DS-JDOSAMT DSCUR
      *-----------------------------------------------
        FIND-P3-043.
           MOVE "ENNS092043." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO FIND-P3-042.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO FIND-P3-043.
           MOVE VDUNUM2 TO DS-JDOSAMT(DSCUR).
      *-----------------------------------------------
      *    DS-JDOSAMT DSELM
      *-----------------------------------------------
        FIND-P3-044.
           MOVE "ENNS092044." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO FIND-P3-043.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO FIND-P3-044.
           MOVE VDUNUM2 TO DS-JDOSAMT(DSELM).
      *-----------------------------------------------
      *    DS-JDOSAMT DSELY
      *-----------------------------------------------
        FIND-P3-045.
           MOVE "ENNS092045." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO FIND-P3-044.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO FIND-P3-045.
           MOVE VDUNUM2 TO DS-JDOSAMT(DSELY).

           GO TO CREATE-ROUTINE.


      *--------------------------------------------------------------
      *    DS-NPURUNIT DSMTD
      *--------------------------------------------------------------
        FIND-P3-05.
           MOVE VDUNUM0 TO DS-NPURUNIT(DSMTD).
      *-----------------------------------------------
      *    DS-NPURUNIT DSYTD
      *-----------------------------------------------
        FIND-P3-051.
           MOVE "ENNS060051." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO FIND-P3-051.
           MOVE VDUNUM0 TO DS-NPURUNIT(DSYTD).
      *-----------------------------------------------
      *    DS-NPURUNIT DSLYR
      *-----------------------------------------------
        FIND-P3-052.
           MOVE "ENNS060052." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO FIND-P3-051.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO FIND-P3-052.
           MOVE VDUNUM0 TO DS-NPURUNIT(DSLYR).
      *-----------------------------------------------
      *    DS-NPURAMT DSMTD
      *-----------------------------------------------
        FIND-P3-053.
           MOVE "ENNS092053." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO FIND-P3-052.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO FIND-P3-053.
           MOVE VDUNUM2 TO DS-NPURAMT(DSMTD).
      *-----------------------------------------------
      *    DS-NPURAMT DSYTD
      *-----------------------------------------------
        FIND-P3-054.
           MOVE "ENNS092054." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO FIND-P3-053.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO FIND-P3-054.
           MOVE VDUNUM2 TO DS-NPURAMT(DSYTD).
      *-----------------------------------------------
      *    DS-NPURAMT DSLYR
      *-----------------------------------------------
        FIND-P3-055.
           MOVE "ENNS092055." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO FIND-P3-054.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO FIND-P3-055.
           MOVE VDUNUM2 TO DS-NPURAMT(DSLYR).


           GO TO CREATE-ROUTINE.

      *--------------------------------------------------------------
      *    DS-OPURUNIT DSMTD
      *--------------------------------------------------------------
        FIND-P3-06.
           MOVE VDUNUM0 TO DS-OPURUNIT(DSMTD).
      *-----------------------------------------------
      *    DS-OPURUNIT DSYTD
      *-----------------------------------------------
        FIND-P3-061.
           MOVE "ENNS060061." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO FIND-P3-061.
           MOVE VDUNUM0 TO DS-OPURUNIT(DSYTD).
      *-----------------------------------------------
      *    DS-OPURUNIT DSLYR
      *-----------------------------------------------
        FIND-P3-062.
           MOVE "ENNS060062." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO FIND-P3-061.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO FIND-P3-062.
           MOVE VDUNUM0 TO DS-OPURUNIT(DSLYR).
      *-----------------------------------------------
      *    DS-OPURAMT DSMTD
      *-----------------------------------------------
        FIND-P3-063.
           MOVE "ENNS092063." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO FIND-P3-062.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO FIND-P3-063.
           MOVE VDUNUM2 TO DS-OPURAMT(DSMTD).
      *-----------------------------------------------
      *    DS-OPURAMT DSYTD
      *-----------------------------------------------
        FIND-P3-064.
           MOVE "ENNS092064." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO FIND-P3-063.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO FIND-P3-064.
           MOVE VDUNUM2 TO DS-OPURAMT(DSYTD).
      *-----------------------------------------------
      *    DS-OPURAMT DSLYR
      *-----------------------------------------------
        FIND-P3-065.
           MOVE "ENNS092065." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO FIND-P3-064.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO FIND-P3-065.
           MOVE VDUNUM2 TO DS-OPURAMT(DSLYR).

           GO TO CREATE-ROUTINE.


      *--------------------------------------------------------------
      *    DS-RNEWUNIT DSMTD
      *--------------------------------------------------------------
        FIND-P3-07.
           MOVE VDUNUM0 TO DS-RNEWUNIT(DSMTD).
      *-----------------------------------------------
      *    DS-RNEWUNIT DSYTD
      *-----------------------------------------------
        FIND-P3-071.
           MOVE "ENNS060071." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO FIND-P3-071.
           MOVE VDUNUM0 TO DS-RNEWUNIT(DSYTD).
      *-----------------------------------------------
      *    DS-RNEWUNIT DSLYR
      *-----------------------------------------------
        FIND-P3-072.
           MOVE "ENNS060072." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO FIND-P3-071.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO FIND-P3-072.
           MOVE VDUNUM0 TO DS-RNEWUNIT(DSLYR).
      *-----------------------------------------------
      *    DS-RNEWAMT DSMTD
      *-----------------------------------------------
        FIND-P3-073.
           MOVE "ENNS092073." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO FIND-P3-072.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO FIND-P3-073.
           MOVE VDUNUM2 TO DS-RNEWAMT(DSMTD).
      *-----------------------------------------------
      *    DS-RNEWAMT DSYTD
      *-----------------------------------------------
        FIND-P3-074.
           MOVE "ENNS092074." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO FIND-P3-073.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO FIND-P3-074.
           MOVE VDUNUM2 TO DS-RNEWAMT(DSYTD).
      *-----------------------------------------------
      *    DS-RNEWAMT DSLYR
      *-----------------------------------------------
        FIND-P3-075.
           MOVE "ENNS092075." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO FIND-P3-074.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO FIND-P3-075.
           MOVE VDUNUM2 TO DS-RNEWAMT(DSLYR).

           GO TO CREATE-ROUTINE.

      *--------------------------------------------------------------
      *    DS-PAIDUNIT DSMTD
      *--------------------------------------------------------------
        FIND-P3-08.
           MOVE VDUNUM0 TO DS-PAIDUNIT(DSMTD).
      *-----------------------------------------------
      *    DS-PAIDUNIT DSYTD
      *-----------------------------------------------
        FIND-P3-081.
           MOVE "ENNS060081." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO FIND-P3-081.
           MOVE VDUNUM0 TO DS-PAIDUNIT(DSYTD).
      *-----------------------------------------------
      *    DS-PAIDUNIT DSLYR
      *-----------------------------------------------
        FIND-P3-082.
           MOVE "ENNS060082." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO FIND-P3-081.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO FIND-P3-082.
           MOVE VDUNUM0 TO DS-PAIDUNIT(DSLYR).
      *-----------------------------------------------
      *    DS-PAIDAMT DSMTD
      *-----------------------------------------------
        FIND-P3-083.
           MOVE "ENNS092083." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO FIND-P3-082.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO FIND-P3-083.
           MOVE VDUNUM2 TO DS-PAIDAMT(DSMTD).
      *-----------------------------------------------
      *    DS-PAIDAMT DSYTD
      *-----------------------------------------------
        FIND-P3-084.
           MOVE "ENNS092084." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO FIND-P3-083.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO FIND-P3-084.
           MOVE VDUNUM2 TO DS-PAIDAMT(DSYTD).
      *-----------------------------------------------
      *    DS-PAIDAMT DSLYR
      *-----------------------------------------------
        FIND-P3-085.
           MOVE "ENNS092085." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO FIND-P3-084.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO FIND-P3-085.
           MOVE VDUNUM2 TO DS-PAIDAMT(DSLYR).

           GO TO CREATE-ROUTINE.

      *--------------------------------------------------------------
      *    DS-TODLUNIT DSMTD
      *--------------------------------------------------------------
        FIND-P3-09.
           MOVE VDUNUM0 TO DS-TODLUNIT(DSMTD).
      *-----------------------------------------------
      *    DS-TODLUNIT DSYTD
      *-----------------------------------------------
        FIND-P3-091.
           MOVE "ENNS060091." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO FIND-P3-091.
           MOVE VDUNUM0 TO DS-TODLUNIT(DSYTD).
      *-----------------------------------------------
      *    DS-TODLUNIT DSLYR
      *-----------------------------------------------
        FIND-P3-092.
           MOVE "ENNS060092." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO FIND-P3-091.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO FIND-P3-092.
           MOVE VDUNUM0 TO DS-TODLUNIT(DSLYR).
      *-----------------------------------------------
      *    DS-TODLAMT DSMTD
      *-----------------------------------------------
        FIND-P3-093.
           MOVE "ENNS092093." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO FIND-P3-092.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO FIND-P3-093.
           MOVE VDUNUM2 TO DS-TODLAMT(DSMTD).
      *-----------------------------------------------
      *    DS-TODLAMT DSYTD
      *-----------------------------------------------
        FIND-P3-094.
           MOVE "ENNS092094." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO FIND-P3-093.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO FIND-P3-094.
           MOVE VDUNUM2 TO DS-TODLAMT(DSYTD).
      *-----------------------------------------------
      *    DS-TODLAMT DSLYR
      *-----------------------------------------------
        FIND-P3-095.
           MOVE "ENNS092095." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO FIND-P3-094.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO FIND-P3-095.
           MOVE VDUNUM2 TO DS-TODLAMT(DSLYR).


           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       TAB-GROUP4.

           GO TO CREATE-ROUTINE.

      *=================================================================
      *
      *    G O T O - F I E L D ----- PAGE 4
      *
      *=================================================================
       GOTO-FIELD4.
           MOVE 0 TO POS-HORIZ POS-VERT.
           MOVE 6 TO SUB.

      *-----------------------------------------------------------------
       FIND-P4-ELEMENT.
           IF ELE > POS-HORIZ
              ADD SUB TO POS-HORIZ
              ADD 1 TO POS-VERT
              GO TO FIND-P4-ELEMENT.

           IF ELE > 30
              MOVE 2 TO NET-NEW
           ELSE
              MOVE 1 TO NET-NEW.
           COMPUTE POS-HORIZ = ELE - (POS-HORIZ - SUB).

              IF POS-VERT > 5
                 SUBTRACT 5 FROM POS-VERT.

              IF POS-HORIZ > 3
                 SUBTRACT 3 FROM POS-HORIZ
                 MOVE VDUNUM2 TO
                   DS-AMT-REPO(POS-HORIZ NET-NEW POS-VERT)
              ELSE
                 MOVE VDUNUM0 TO
                   DS-NO-REPO(POS-HORIZ NET-NEW POS-VERT).

      *-----------------------------------------------------------------
       TAB-GROUP5.
           GO TO CREATE-ROUTINE.

       GOTO-FIELD5.
           MOVE DELINQ TO SUB-P5.

           IF ELE = 5
              GO TO H-012.

           GO TO H-01 H-01 H-01 H-01 H-01 H-01 H-01
                 H-01 H-01 H-01 H-08 H-08 H-08 H-08
                 DEPENDING ON ELE.

           GO TO END-ENTRY-MODULE.


       A-01.
           MOVE VDUNUM2 TO DS-DLRVPRIN(1).
           GO TO CREATE-ROUTINE.

      *---------------------------------------------------------------
       A-02.
           MOVE VDUNUM2 TO DS-DLRVPRIN(2).
           GO TO CREATE-ROUTINE.

      *---------------------------------------------------------------
       A-03.
           MOVE VDUNUM2 TO DS-DLRVPRIN(3).
           GO TO CREATE-ROUTINE.

      *---------------------------------------------------------------
       A-04.
           MOVE VDUNUM2 TO DS-DLRVCHGS(1).
           GO TO CREATE-ROUTINE.

      *---------------------------------------------------------------
       A-05.
           MOVE VDUNUM2 TO DS-DLRVCHGS(2).
           GO TO CREATE-ROUTINE.

      *---------------------------------------------------------------
       A-06.
           MOVE VDUNUM2 TO DS-DLRVCHGS(3).
           GO TO CREATE-ROUTINE.

      *---------------------------------------------------------------
       A-07.
           MOVE VDUNUM2 TO DS-DLPARVHELD.
           GO TO CREATE-ROUTINE.

      *---------------------------------------------------------------
       A-08.
           MOVE VDUNUM2 TO DS-UNPAIDCK.
           MOVE "ENNS082081." TO VDU.
           PERFORM SCREENIO.
      *    IF VDUNUM2 = 0
      *       MOVE "SNNS082081." TO VDU
      *       MOVE DS-UNPAID-MANUAL TO VDUNUM2
      *       PERFORM SCREENIO.
           MOVE VDUNUM2 TO DS-UNPAID-MANUAL.
           GO TO CREATE-ROUTINE.

      *---------------------------------------------------------------
       A-09.
           MOVE VDUNUM2 TO DS-DLDISC.
           GO TO CREATE-ROUTINE.

      *---------------------------------------------------------------
       A-10.
           MOVE VDUNUM2 TO DS-DLPARVPAID.
           GO TO CREATE-ROUTINE.

      *---------------------------------------------------------------
       A-11.
           MOVE VDUNUM2 TO DS-SUMWGTAV.

      *---------------------------------------------------------------
       A-111.
           MOVE "EN S080111." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10"
              GO TO TEST-F1.
           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO A-111.
           MOVE VDUNUM0 TO DS-SUMWGTTM.
           GO TO CREATE-ROUTINE.

      *---------------------------------------------------------------
       A-12.
           MOVE VDUNUM2 TO DS-SUMWGTTA.
           GO TO CREATE-ROUTINE.

      *---------------------------------------------------------------
       A-13.
           MOVE VDUNUM2 TO DS-DLDISC-VOL(1).
           GO TO CREATE-ROUTINE.

      *---------------------------------------------------------------
       A-14.
           MOVE VDUNUM2 TO DS-DLDISC-VOL(2).
           GO TO CREATE-ROUTINE.

      *---------------------------------------------------------------
       A-15.
           MOVE VDUNUM2 TO DS-DLDISC-VOL(3).
           GO TO CREATE-ROUTINE.

      *---------------------------------------------------------------
       A-16.
           MOVE VDUNUM2 TO DS-PROCEEDS(1).
           GO TO CREATE-ROUTINE.

      *---------------------------------------------------------------
       A-17.
           MOVE VDUNUM2 TO DS-PROCEEDS(2).
           GO TO CREATE-ROUTINE.

      *---------------------------------------------------------------
       A-18.
           GO TO CREATE-ROUTINE.

      *---------------------------------------------------------------
       A-19.
           MOVE VDUNUM0 TO DS-RCOURSE-UNITS(1).
           GO TO CREATE-ROUTINE.

      *---------------------------------------------------------------
       A-20.
           MOVE VDUNUM2 TO DS-RCOURSE-AMT(1).
           GO TO CREATE-ROUTINE.

      *---------------------------------------------------------------
       A-21.
           MOVE VDUNUM0 TO DS-RCOURSE-UNITS(2).
           GO TO CREATE-ROUTINE.

      *---------------------------------------------------------------
       A-22.
           MOVE VDUNUM2 TO DS-RCOURSE-AMT(2).
           GO TO CREATE-ROUTINE.

      *---------------------------------------------------------------
       A-23.
           MOVE VDUNUM0 TO DS-RCOURSE-UNITS(3).
           GO TO CREATE-ROUTINE.

      *---------------------------------------------------------------
       A-24.
           MOVE VDUNUM2 TO DS-RCOURSE-AMT(3).
           GO TO CREATE-ROUTINE.

      *---------------------------------------------------------------
       A-25.
           MOVE VDUNUM2 TO DS-AFPDUEDL-MEMO.
           GO TO CREATE-ROUTINE.

      *=================================================================
      *
      *    SET RECORD ELEMENTS ----- PAGE 2
      *
      *=================================================================
       B-01.
           MOVE VDUNUM2 TO DS-UNEARNED-AMT(1 ACT TYP).
       B-011.
           MOVE "ENNN082  1." TO VDU.
           MOVE ELE TO VDUROW.
           PERFORM SCREENIO.
           IF UP-KEY GO TO ENTER-ELE.
           IF NOT (ENTER-KEY OR DOWN-KEY) GO TO B-011.
           MOVE VDUNUM2 TO DS-UNEARNED-AMT(2 ACT TYP).
       B-012.
           MOVE "ENNN082  2." TO VDU.
           MOVE ELE TO VDUROW.
           PERFORM SCREENIO.
           IF UP-KEY GO TO B-011.
           IF NOT (ENTER-KEY OR DOWN-KEY) GO TO B-012.
           MOVE VDUNUM2 TO DS-UNEARNED-AMT(3 ACT TYP).
           GO TO CREATE-ROUTINE.
       B-13.
           MOVE VDU-DATE-YYYYMMDD TO DS-DELUNEARN-DATE.
           GO TO CREATE-ROUTINE.

      *=================================================================
      *
      *    SET RECORD ELEMENTS ----- PAGE 5
      *
      *=================================================================
       H-01.
           IF ( HEADER-LEV-STD )
              MOVE VDUNUM0 TO DS-UNITS(DELINQ 1 ELE)
           ELSE
              MOVE VDUNUM0 TO DS-UNITS-BNKRPT(DELINQ 1 ELE).

      *-----------------------------------------------------------------
       H-011.
           MOVE "ENNS092011." TO VDU.
           MOVE ELE TO VDUROW.
           PERFORM SCREENIO.

           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE.

           IF VDUSTATUS = "10"
              GO TO ENTER-ELE.

           IF VDUSTATUS NOT = "00"
              GO TO H-011.

           IF ( HEADER-LEV-STD )
              MOVE VDUNUM2 TO DS-OUTBAL(DELINQ 1 ELE)
           ELSE
              MOVE VDUNUM2 TO DS-OUTBAL-BNKRPT(DELINQ 1 ELE).

      *-----------------------------------------------------------------
       H-012.
           MOVE "ENNS070012." TO VDU.
           MOVE ELE TO VDUROW.
           PERFORM SCREENIO.

           IF VDUSTATUS = "1U"
              IF ELE = 5
                 GO TO H-012
              ELSE
                 GO TO H-011.

           IF VDUSTATUS = "10"
              GO TO ENTER-ELE.

           IF VDUSTATUS NOT = "00"
              GO TO H-012.

           IF ( HEADER-LEV-STD )
              MOVE VDUNUM0 TO DS-UNITS(DELINQ 2 ELE)
           ELSE
              MOVE VDUNUM0 TO DS-UNITS-BNKRPT(DELINQ 2 ELE).

      *-----------------------------------------------------------------
       H-013.
           MOVE "ENNS092013." TO VDU.
           MOVE ELE TO VDUROW.
           PERFORM SCREENIO.

           IF VDUSTATUS = "1U"
              GO TO H-012.

           IF VDUSTATUS = "10"
              GO TO ENTER-ELE.

           IF VDUSTATUS NOT = "00"
              GO TO H-013.

           IF ( HEADER-LEV-STD )
              MOVE VDUNUM2 TO DS-OUTBAL(DELINQ 2 ELE)
           ELSE
              MOVE VDUNUM2 TO DS-OUTBAL(DELINQ 2 ELE).

           GO TO CREATE-ROUTINE.

       H-08.
           GO TO CREATE-ROUTINE.


       END-ENTRY-MODULE.
           EXIT.

      ******************************************************************
       WRITE-MODULE SECTION.
      ******************************************************************
           MOVE BR-DATA-PATH TO DS-ALL-DATA.
           MOVE BR-MACHINE TO DS-ALL-MACHINE.
           MOVE DS-ALL-PATH TO DS-PATH.
           PERFORM OPEN-DS1-FILE.

           IF NEW-FLAG NOT = 0
              GO TO WRITE-RECORD.

           MOVE DS-REC TO CHANGED-DS-REC.

           IF ORIGINAL-DS-REC = CHANGED-DS-REC
              GO TO END-WRITE-MODULE.

           PERFORM READ-DS1-FILE.

           IF IO-FG NOT = 0
              PERFORM UNDERGONE-CHANGE
              PERFORM RECORD-DELETED
              GO TO END-WRITE-MODULE.

           IF DS-REC NOT = ORIGINAL-DS-REC
              PERFORM UNDERGONE-CHANGE
              PERFORM CHANGE-AGAIN
              GO TO END-WRITE-MODULE.

           MOVE CHANGED-DS-REC TO DS-REC.

           PERFORM REWRITE-DS1-FILE.
           GO TO END-WRITE-MODULE.

       WRITE-RECORD.
           PERFORM WRITE-DS1-FILE.

           IF IO-FG NOT = 0
              PERFORM ALREADY-CREATED.

       END-WRITE-MODULE.
           PERFORM CLOSE-DS1-FILE.

      ******************************************************************
       DELETE-MODULE SECTION.
      ******************************************************************
           IF NEW-FLAG NOT = 0
              GO TO END-DELETE-MODULE.

           MOVE BR-DATA-PATH TO DS-ALL-DATA.
           MOVE BR-MACHINE TO DS-ALL-MACHINE.
           MOVE DS-ALL-PATH TO DS-PATH.
           PERFORM OPEN-DS1-FILE.
           PERFORM READ-DS1-FILE.

           IF IO-FG NOT = 0
              PERFORM UNDERGONE-CHANGE
              PERFORM RECORD-DELETED
              GO TO END-DELETE-MODULE.

           IF DS-REC NOT = ORIGINAL-DS-REC
              PERFORM UNDERGONE-CHANGE
              PERFORM RECORD-CHANGED
              GO TO END-DELETE-MODULE.

           IF DS-DLRVPRIN(1) = 0
                 AND DS-DLRVPRIN(2) = 0
                  AND DS-DLRVPRIN(3) = 0
                   AND DS-DLRVCHGS(1) = 0
                    AND DS-DLRVCHGS(2) = 0
                     AND DS-DLRVCHGS(3) = 0
                      AND DS-DLPARVHELD = 0
                       AND DS-UNPAIDCK = 0
                        AND DS-DLDISC = 0
                         AND DS-DLPARVPAID = 0
                          AND DS-OUTSAMT (1) = 0
                           AND DS-NPURAMT (1) = 0
                            AND DS-OPURAMT (1) = 0
                             AND DS-RNEWAMT (1) = 0
                              AND DS-JDOSAMT (1) = 0
                               AND DS-PLOSAMT (1) = 0
                                AND DS-RCPLAMT (1) = 0
                                 AND DS-TODLAMT (1) = 0
              PERFORM DELETE-DS1-FILE
           ELSE
              PERFORM NO-DELETE.

       END-DELETE-MODULE.
           PERFORM CLOSE-DS1-FILE.

      ******************************************************************
       NEW-RECORD-SETUP SECTION.
      ******************************************************************
           MOVE 1 TO NEW-FLAG.
           MOVE "S  A000NEW." TO VDU.
           MOVE "*** NEW RECORD ***" TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "S  A180PROMPT-250." TO VDU.

           IF ( DL-AFP-DEALER = "Y" )
              MOVE "25. DUE DLR MEMO :" TO VDUBUF
           ELSE
              MOVE "25. N/A          :" TO VDUBUF.

           PERFORM SCREENIO.
           MOVE "S  A120250."        TO VDU.
           MOVE ALL "_"              TO VDUBUF.
           PERFORM SCREENIO.

           PERFORM CLEAR-DS-FILE.
           MOVE HOLD-DS1-KEY TO DS1-KEY.

       NEW-RECORD-EXIT.
           EXIT.

      ******************************************************************
       DISPLAY-FIELDS SECTION.
      ******************************************************************
           MOVE SPACES               TO DISPLAY-BUF1.
           MOVE SPACES               TO DISPLAY-BUF2.
           MOVE SPACES               TO DISPLAY-BUF3A.
           MOVE SPACES               TO DISPLAY-BUF4A.

           IF CUR-FORM NOT = 1
              GO TO SETUP-BUFFER2.

           MOVE DS-DLRVPRIN(1)       TO A01.
           MOVE DS-DLRVPRIN(2)       TO A02.
           MOVE DS-DLRVPRIN(3)       TO A03.
           MOVE DS-DLRVCHGS(1)       TO A04.
           MOVE DS-DLRVCHGS(2)       TO A05.
           MOVE DS-DLRVCHGS(3)       TO A06.
           MOVE DS-DLPARVHELD        TO A07.
           MOVE DS-UNPAIDCK          TO A08.
           MOVE DS-UNPAID-MANUAL     TO A081.
           MOVE DS-DLDISC            TO A09.
           MOVE DS-DLPARVPAID        TO A10.
           MOVE DS-SUMWGTAV          TO A11.
           MOVE DS-SUMWGTTM          TO A111.
           MOVE DS-SUMWGTTA          TO A12.
           MOVE DS-DLDISC-VOL(1)     TO A13.
           MOVE DS-DLDISC-VOL(2)     TO A14.
           MOVE DS-DLDISC-VOL(3)     TO A15.
           MOVE DS-PROCEEDS(1)       TO A16.
           MOVE DS-PROCEEDS(2)       TO A17.
      *----UNUSED FIELD------------->TO A18.
           MOVE DS-RCOURSE-UNITS(1)  TO A19.
           MOVE DS-RCOURSE-AMT(1)    TO A20.
           MOVE DS-RCOURSE-UNITS(2)  TO A21.
           MOVE DS-RCOURSE-AMT(2)    TO A22.
           MOVE DS-RCOURSE-UNITS(3)  TO A23.
           MOVE DS-RCOURSE-AMT(3)    TO A24.

           IF ( DL-AFP-DEALER = "Y" )
              MOVE "25. DUE DLR MEMO :" TO PROMPT-A25
              MOVE DS-AFPDUEDL-MEMO     TO A25
           ELSE
              MOVE "25. N/A          :" TO PROMPT-A25
              MOVE ZEROES               TO A25.

           MOVE DISPLAY-BUF1 TO WR-BUF.
           MOVE DISPLAY-LIST1 TO WR-LIST.
           PERFORM CALL-WRFORM.
           GO TO DISPLAY-FIELDS-EXIT.

      *-----------------------------------------------------------------
       SETUP-BUFFER2.
           IF CUR-FORM NOT = 2
              GO TO SETUP-BUFFER3.

           IF NEW-FLAG = 1
              GO TO DISPLAY-FIELDS-EXIT.

      * NET CURRENT
           MOVE DS-UNEARNED-AMT(1 1 1) TO P2-DISP-AMT(1  1).
           MOVE DS-UNEARNED-AMT(1 1 2) TO P2-DISP-AMT(1  2).
           MOVE DS-UNEARNED-AMT(1 1 3) TO P2-DISP-AMT(1  3).
           MOVE DS-UNEARNED-AMT(1 1 4) TO P2-DISP-AMT(1  4).
           MOVE DS-UNEARNED-AMT(1 1 5) TO P2-DISP-AMT(1  5).
           MOVE DS-UNEARNED-AMT(1 1 6) TO P2-DISP-AMT(1  6).
      * VOL MTD
           MOVE DS-UNEARNED-AMT(1 2 1) TO P2-DISP-AMT(1  7).
           MOVE DS-UNEARNED-AMT(1 2 2) TO P2-DISP-AMT(1  8).
           MOVE DS-UNEARNED-AMT(1 2 3) TO P2-DISP-AMT(1  9).
           MOVE DS-UNEARNED-AMT(1 2 4) TO P2-DISP-AMT(1 10).
           MOVE DS-UNEARNED-AMT(1 2 5) TO P2-DISP-AMT(1 11).
           MOVE DS-UNEARNED-AMT(1 2 6) TO P2-DISP-AMT(1 12).
      * NET ELM
           MOVE DS-UNEARNED-AMT(2 1 1) TO P2-DISP-AMT(2  1).
           MOVE DS-UNEARNED-AMT(2 1 2) TO P2-DISP-AMT(2  2).
           MOVE DS-UNEARNED-AMT(2 1 3) TO P2-DISP-AMT(2  3).
           MOVE DS-UNEARNED-AMT(2 1 4) TO P2-DISP-AMT(2  4).
           MOVE DS-UNEARNED-AMT(2 1 5) TO P2-DISP-AMT(2  5).
           MOVE DS-UNEARNED-AMT(2 1 6) TO P2-DISP-AMT(2  6).
      * VOL YTD
           MOVE DS-UNEARNED-AMT(2 2 1) TO P2-DISP-AMT(2  7).
           MOVE DS-UNEARNED-AMT(2 2 2) TO P2-DISP-AMT(2  8).
           MOVE DS-UNEARNED-AMT(2 2 3) TO P2-DISP-AMT(2  9).
           MOVE DS-UNEARNED-AMT(2 2 4) TO P2-DISP-AMT(2 10).
           MOVE DS-UNEARNED-AMT(2 2 5) TO P2-DISP-AMT(2 11).
           MOVE DS-UNEARNED-AMT(2 2 6) TO P2-DISP-AMT(2 12).
      * NET ELY
           MOVE DS-UNEARNED-AMT(3 1 1) TO P2-DISP-AMT(3  1).
           MOVE DS-UNEARNED-AMT(3 1 2) TO P2-DISP-AMT(3  2).
           MOVE DS-UNEARNED-AMT(3 1 3) TO P2-DISP-AMT(3  3).
           MOVE DS-UNEARNED-AMT(3 1 4) TO P2-DISP-AMT(3  4).
           MOVE DS-UNEARNED-AMT(3 1 5) TO P2-DISP-AMT(3  5).
           MOVE DS-UNEARNED-AMT(3 1 6) TO P2-DISP-AMT(3  6).
      * VOL LYR
           MOVE DS-UNEARNED-AMT(3 2 1) TO P2-DISP-AMT(3  7).
           MOVE DS-UNEARNED-AMT(3 2 2) TO P2-DISP-AMT(3  8).
           MOVE DS-UNEARNED-AMT(3 2 3) TO P2-DISP-AMT(3  9).
           MOVE DS-UNEARNED-AMT(3 2 4) TO P2-DISP-AMT(3 10).
           MOVE DS-UNEARNED-AMT(3 2 5) TO P2-DISP-AMT(3 11).
           MOVE DS-UNEARNED-AMT(3 2 6) TO P2-DISP-AMT(3 12).

           MOVE DS-DELUNEARN-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY       TO B13.

       P2-TABLE-EXIT.
           MOVE DISPLAY-BUF2 TO WR-BUF.
           MOVE DISPLAY-LIST2 TO WR-LIST.
           PERFORM CALL-WRFORM.

           GO TO DISPLAY-FIELDS-EXIT.

      *-----------------------------------------------------------------
       SETUP-BUFFER3.
           IF CUR-FORM NOT = 3
              GO TO SETUP-BUFFER4.

           IF NEW-FLAG = 1
              GO TO DISPLAY-FIELDS-EXIT.

           MOVE 0 TO SUB.
           PERFORM DISPLAY-P3-TABLE 2 TIMES.

      *-----------------------------------------------------------------
       DISPLAY-P3-TABLE.
           ADD 1 TO SUB.
           MOVE DS-OUTSUNIT(SUB) TO C01(1, SUB).
           MOVE DS-PLOSUNIT(SUB) TO C01(2, SUB).
           MOVE DS-RCPLUNIT(SUB) TO C01(3, SUB).
           MOVE DS-JDOSUNIT(SUB) TO C01(4, SUB).
           MOVE DS-NPURUNIT(SUB) TO C01(5, SUB).
           MOVE DS-OPURUNIT(SUB) TO C01(6, SUB).
           MOVE DS-RNEWUNIT(SUB) TO C01(7, SUB).
           MOVE DS-PAIDUNIT(SUB) TO C01(8, SUB).
           MOVE DS-TODLUNIT(SUB) TO C01(9, SUB).

           MOVE DS-OUTSAMT(SUB) TO C02(1, SUB).
           MOVE DS-PLOSAMT(SUB) TO C02(2, SUB).
           MOVE DS-RCPLAMT(SUB) TO C02(3, SUB).
           MOVE DS-JDOSAMT(SUB) TO C02(4, SUB).
           MOVE DS-NPURAMT(SUB) TO C02(5, SUB).
           MOVE DS-OPURAMT(SUB) TO C02(6, SUB).
           MOVE DS-RNEWAMT(SUB) TO C02(7, SUB).
           MOVE DS-PAIDAMT(SUB) TO C02(8, SUB).
           MOVE DS-TODLAMT(SUB) TO C02(9, SUB).

      *-----------------------------------------------------------------
       P3-TABLE-EXIT.
           MOVE DISPLAY-BUF3A TO WR-BUF.
           MOVE DISPLAY-LIST3A TO WR-LIST.
           PERFORM CALL-WRFORM.
           MOVE DISPLAY-BUF3B TO WR-BUF.
           MOVE DISPLAY-LIST3B TO WR-LIST.
           PERFORM CALL-WRFORM.
           GO TO DISPLAY-FIELDS-EXIT.

      *-----------------------------------------------------------------
      *  DISPLAY PAGE 4
      *
      *  SUB4 --> 1 : NET GAIN/LOSS
      *           2 : NEW-VOL
      *  SUB5 --> 1 : CURRENT         |  MTD
      *           2 : END LAST MONTH  |  YTD
      *           3 : END LAST YEAR   |  LYR
      *  SUB6 --> 1 : REPO ON HAND
      *           2 : REPO REDEEMED
      *           3 : REPO SOLD LIVE RECEICABLE
      *           4 : REPO SOLD P&L ACTIVE
      *           5 : REPO SOLD P&L INACTIVE
       SETUP-BUFFER4.
           IF CUR-FORM NOT = 4
              GO TO SETUP-BUFFER5.

           PERFORM DISPLAY-P4-TABLE VARYING SUB4 FROM 1 BY 1
                                            UNTIL SUB4 > 2
                                    AFTER   SUB6 FROM 1 BY 1
                                            UNTIL SUB6 > 5
                                    AFTER   SUB5 FROM 1 BY 1
                                            UNTIL SUB5 > 3.

           GO TO P4-TABLE-EXIT.

       DISPLAY-P4-TABLE.
           MOVE DS-NO-REPO(SUB5 SUB4 SUB6)  TO D01(SUB4 SUB6 SUB5).
           MOVE DS-AMT-REPO(SUB5 SUB4 SUB6) TO D02(SUB4 SUB6 SUB5).

       P4-TABLE-EXIT.
           MOVE DISPLAY-BUF4A TO WR-BUF.
           MOVE DISPLAY-LIST4A TO WR-LIST.
           PERFORM CALL-WRFORM.

           MOVE DISPLAY-BUF4B TO WR-BUF.
           MOVE DISPLAY-LIST4B TO WR-LIST.
           PERFORM CALL-WRFORM.

           GO TO DISPLAY-FIELDS-EXIT.

       SETUP-BUFFER5.

           PERFORM SETUP-HEADER-DEL-TYPE.
           MOVE HEADER-BUFFER TO WR-BUF.
           MOVE HEADER-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

           PERFORM SETUP5
             VARYING SUB3 FROM 1 BY 1 UNTIL SUB3 > 10
                AFTER SUB2 FROM 1 BY 1 UNTIL SUB2 > 2.


           MOVE DISPLAY-BUF5 TO WR-BUF.
           MOVE DISPLAY-LIST5 TO WR-LIST.
           PERFORM CALL-WRFORM.
           GO TO DISPLAY-FIELDS-EXIT.

      *-----------------------------------------------------------------
       SETUP5.

           IF ( HEADER-LEV-STD )
              MOVE DS-UNITS (DELINQ SUB2 SUB3) TO DEL-UNITS (SUB3 SUB2)
              MOVE DS-OUTBAL(DELINQ SUB2 SUB3) TO DEL-OUTBAL(SUB3 SUB2)
           ELSE
              MOVE DS-UNITS-BNKRPT (DELINQ SUB2 SUB3) TO
                                                  DEL-UNITS (SUB3 SUB2)
              MOVE DS-OUTBAL-BNKRPT(DELINQ SUB2 SUB3) TO
                                                  DEL-OUTBAL(SUB3 SUB2).

       DISPLAY-FIELDS-EXIT.
           EXIT.

      *****************************************************************
       TOGGLE-ON-DELINQ SECTION.
      *****************************************************************

           IF ( F2-KEY )
              MOVE 1 TO DELINQ
           ELSE
           IF ( F3-KEY )
              MOVE 2 TO DELINQ
           ELSE
           IF ( F4-KEY )
              MOVE 3 TO DELINQ
           ELSE
           IF ( F5-KEY )
              MOVE 1 TO HEADER-LEV
           ELSE
           IF ( F6-KEY )
              MOVE 2 TO HEADER-LEV.

           IF ( HEADER-LEV = HEADER-LEVEL ) AND
              ( DELINQ     = DELTYP-COD   )
              GO TO EXIT-TOGGLE-ON-DELINQ.

           MOVE HEADER-LEV TO HEADER-LEVEL.
           MOVE DELINQ     TO DELTYP-COD.

           PERFORM SETUP-HEADER-DEL-TYPE.

       END-TOGGLE-ON-DELINQ.

           MOVE HEADER-BUFFER TO WR-BUF.
           MOVE HEADER-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.
           PERFORM DISPLAY-FIELDS.

       EXIT-TOGGLE-ON-DELINQ.
           EXIT.

      *****************************************************************
       SETUP-HEADER-DEL-TYPE SECTION.
      *****************************************************************

           MOVE HEADER-TYPE(HEADER-LEV) TO HEADER-BANKRUPT-STD.
           MOVE DEL-TYPE   (DELINQ)     TO HEADER-DEL-TYPE.
           MOVE HEADER-LAYOUT           TO HEADER-BUFFER.

       EXIT-SETUP-HEADER-DEL-TYP.
           EXIT.

      **************************************************************************
       FORM-RESET SECTION.
      **************************************************************************
           DISPLAY SPACES UPON DSMAIN-WINDOW-HANDLE
                               LINE 0 COL 0 ERASE SCREEN.
           EVALUATE CUR-FORM
              WHEN 1 MOVE EXT-CONAME-CONAME TO DSMAN1-CONAME
                     MOVE EXT-CONAME-SYSDATE TO DSMAN1-SYSDATE
                     MOVE "DSMAN1" TO VDU-FORM-NAME
                     DISPLAY DSMAN1-SCREEN UPON DSMAIN-WINDOW-HANDLE
                     MOVE DSMAN1-FORM-FIELDS TO WR-BUF 
                     MOVE DSMAN1-FORM-LIST   TO WR-LIST
              WHEN 2 MOVE EXT-CONAME-CONAME TO DSMAN2-CONAME
                     MOVE EXT-CONAME-SYSDATE TO DSMAN2-SYSDATE
                     MOVE "DSMAN2" TO VDU-FORM-NAME
                     DISPLAY DSMAN2-SCREEN UPON DSMAIN-WINDOW-HANDLE
                     MOVE DSMAN2-FORM-FIELDS TO WR-BUF 
                     MOVE DSMAN2-FORM-LIST   TO WR-LIST
              WHEN 3 MOVE EXT-CONAME-CONAME TO DSMAN3-CONAME
                     MOVE EXT-CONAME-SYSDATE TO DSMAN3-SYSDATE
                     MOVE "DSMAN3" TO VDU-FORM-NAME
                     DISPLAY DSMAN3-SCREEN UPON DSMAIN-WINDOW-HANDLE
                     MOVE DSMAN3-FORM-FIELDS TO WR-BUF 
                     MOVE DSMAN3-FORM-LIST   TO WR-LIST
              WHEN 4 MOVE EXT-CONAME-CONAME TO DSMAN4-CONAME
                     MOVE EXT-CONAME-SYSDATE TO DSMAN4-SYSDATE
                     MOVE "DSMAN4" TO VDU-FORM-NAME
                     DISPLAY DSMAN4-SCREEN UPON DSMAIN-WINDOW-HANDLE
                     MOVE DSMAN4-FORM-FIELDS TO WR-BUF 
                     MOVE DSMAN4-FORM-LIST   TO WR-LIST
              WHEN OTHER MOVE EXT-CONAME-SYSDATE TO DSMAN5-SYSDATE
                         MOVE "DSMAN5" TO VDU-FORM-NAME
                         DISPLAY DSMAN5-SCREEN UPON DSMAIN-WINDOW-HANDLE
                         MOVE DSMAN5-FORM-FIELDS TO WR-BUF 
                         MOVE DSMAN5-FORM-LIST   TO WR-LIST
           END-EVALUATE.
           PERFORM CALL-WRFORM.
           
       FORM-RESET-EXIT.
           EXIT.

      **************************************************************************
       FORM-DESTROY SECTION.
      **************************************************************************
           EVALUATE CUR-FORM
              WHEN  1 DESTROY DSMAN1-SCREEN
              WHEN  2 DESTROY DSMAN2-SCREEN
              WHEN  3 DESTROY DSMAN3-SCREEN
              WHEN  4 DESTROY DSMAN4-SCREEN
              WHEN OTHER DESTROY DSMAN5-SCREEN
           END-EVALUATE.

      **************************************************************************
       SETUP-LINK-INFO SECTION.
      **************************************************************************
           EVALUATE CUR-FORM
              WHEN  1 MOVE DSMAN1-HELP--FILE TO HELP-LINK-FILE
                      MOVE DSMAN1-HELP--DIR  TO HELP-LINK-DIR
              WHEN  2 MOVE DSMAN2-HELP--FILE TO HELP-LINK-FILE
                      MOVE DSMAN2-HELP--DIR  TO HELP-LINK-DIR
              WHEN  3 MOVE DSMAN3-HELP--FILE TO HELP-LINK-FILE
                      MOVE DSMAN3-HELP--DIR  TO HELP-LINK-DIR
              WHEN  4 MOVE DSMAN4-HELP--FILE TO HELP-LINK-FILE
                      MOVE DSMAN4-HELP--DIR  TO HELP-LINK-DIR
              WHEN  OTHER MOVE DSMAN5-HELP--FILE TO HELP-LINK-FILE
                          MOVE DSMAN5-HELP--DIR  TO HELP-LINK-DIR
           END-EVALUATE.

      **************************************************************************
       VDU-CONVERT-FIELD SECTION.
      **************************************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBMN/DSMAN1_EVA.CPY".
              COPY "LIBMN/DSMAN2_EVA.CPY".
              COPY "LIBMN/DSMAN3_EVA.CPY".
              COPY "LIBMN/DSMAN4_EVA.CPY".
              COPY "LIBMN/DSMAN5_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ******************************************************************
      *
      *    C O P Y - L I B R A R Y - S E C T I O N S
      *
      ******************************************************************
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/GPENV.CPY".
       COPY "LIBGB/DATER.CPY".
       COPY "LIBLP/LPDSCL.CPY".

      ******************************************************************
       MISC-PARAGRAPHS SECTION.
      ******************************************************************
       COPY "LIBGB/PHONEIO.CPY".
       COPY "LIBGB/ACCESS.CPY".
       NEW-PAGE5-SETUP.
           PERFORM NEW-PERIOD-SETUP.
           PERFORM NEW-PERIOD-DISPLAY.
       NEW-PERIOD-SETUP.

           IF ( F9-KEY )
              ADD  1 TO PERIOD
           ELSE
              MOVE 1 TO PERIOD.

           IF ( PERIOD < 1 )
              MOVE 3 TO PERIOD
              MOVE 1 TO PERIOD-F5
           ELSE
           IF ( PERIOD > 3 )
              MOVE 1 TO PERIOD
              MOVE 2 TO PERIOD-F5
           ELSE
           IF ( PERIOD = 1 )
              MOVE 2 TO PERIOD-F5
           ELSE
           IF ( PERIOD = 2 )
              MOVE 3 TO PERIOD-F5
           ELSE
              MOVE 1 TO PERIOD-F5.

      *-----------------------------------------------------------------
       NEW-PERIOD-DISPLAY.

           MOVE PER-TYPE1(PERIOD)              TO PER-BUF1.
           MOVE PER-TYPE2(PERIOD)              TO PER-BUF2.
           MOVE PER-BUFFER TO WR-BUF.
           MOVE PER-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      *-----------------------------------------------------------------
       NO-DELETE.
           MOVE " ACTIVE BALANCES - UNABLE TO DELETE!" TO MESS.
           PERFORM SEND-MESS.

      *-----------------------------------------------------------------
       RECORD-DELETED.
           MOVE " RECORD WAS DELETED!" TO MESS.
           PERFORM SEND-MESS.

      *-----------------------------------------------------------------
       RECORD-CHANGED.
           MOVE " CHANGES WERE MADE TO THIS RECORD!" TO MESS.
           PERFORM SEND-MESS.

      *-----------------------------------------------------------------
       CHANGE-AGAIN.
           MOVE " PLEASE RE-INPUT YOUR CHANGES!" TO MESS.
           PERFORM SEND-MESS.

      *-----------------------------------------------------------------
       ALREADY-CREATED.
           MOVE " RECORD HAS BEEN ALREADY CREATED!" TO MESS.
           PERFORM SEND-MESS.

      *-----------------------------------------------------------------
       UNDERGONE-CHANGE.
           MOVE
           " THIS RECORD HAS UNDERGONE CHANGE BY ANOTHER USER,"
           TO MESS.
           PERFORM SEND-MESS.

      *-----------------------------------------------------------------
       ACTIVE-RECORD.
           MOVE " ACTIVE CUSTOMER CANNOT BE DELETED" TO MESS.
           PERFORM SEND-MESS.

      *-----------------------------------------------------------------
       GET-TODAYS-DATE.
           MOVE EXT-CONAME-SYSDATE TO ALP-DATE.
           MOVE ALP-MO TO DISPLAY-MO.
           MOVE ALP-YR TO DISPLAY-YR.
           MOVE DISPLAY-DATE TO WR-BUF.
           MOVE DATE-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.


      ******************************************************************
       IO-ROUTINES SECTION.
      ******************************************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/DECLARE_SQL.CPY".
           PERFORM CLOSE-LC1-FILE.
           PERFORM CLOSE-DS1-FILE.
           PERFORM CLOSE-DL1-FILE.
           PERFORM CLOSE-BR-FILE.
       COPY "LIBLP/LPLCGS_SQL.CPY".
       COPY "LIBLP/LPDSGS_SQL.CPY".
       COPY "LIBLP/LPDLGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBLP/LPDS1IN.CPY".
       COPY "LIBLP/LPDL1RN.CPY".
       COPY "LIBLP/LPLC1RN.CPY".
       COPY "LIBGB/GBBRRN.CPY".

       COPY "LIBGB/FERRORS.CPY".

      ******************************************************************
       END-ROUTINE SECTION.
      ******************************************************************
           PERFORM CLOSE-FILES.

       EXIT-PROG.
           MOVE "LP/LPM2MU" TO FORM-NAM.
           MOVE FORM-PATH TO FORM-PATHNAME.

       STOP-RUN.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN  
           DESTROY DSMAIN-WINDOW-HANDLE.
           MOVE FORM-PATH TO FORM-PATHNAME.
           EXIT PROGRAM.
