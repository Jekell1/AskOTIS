       IDENTIFICATION DIVISION.
       PROGRAM-ID.      BYMAIN.
       DATE-WRITTEN.    122784.
      ******************************************************************
      *
      *                 (MN)
      *  DESCRIPTION:   BRANCH STATISTICS FILE MAINTENANCE
      *
      * REV:
      *  JTG 101993 ADDED LOGIC FOR BY-DEL-BUCKETS
      *  JTG 011694 CHANGED FOR WORLD NEW REQUIRED FIELDS
      *  BAH 091294 ADDED SCAN
      *  BLL 122094 ADDED DATE TO KEY
      *  MJD 062195 ADDED BROKE PAGE 1 DOWN INTO TWO PAGES.
      *              PAGE 1 IS NOW AMOUNTS COLLECTED AND
      *              PAGE 7 IS AMOUNTS REBATED. ADDED ALL
      *              FIELDS THAT DID NOT APPEAR IN MAINT.
      *  BLL 073095 MISSING STATEMENT, PAGE 6 ENTER ELE WENT TO $
      *  MJD 081495 ADDED LOGIC FOR GP-THE-ONLY-SF-SRCD.
      *  JTG 092195 ADDED SIGNS TO PAGE 3 DELINQUENCY FIELDS
      *  MJD 100695 ADDED BY-IB-ACCRUED TO PAGE 2.
      *  MJD 111095 COMMENTED OUT INPUT/OUTPUT OF VOLUME PORTION OF
      *             DELINQ TABLE.
      *  KEC 121295 FIXED SCANKEY TO SCAN THROUGH ALL BRANCHS CROSSING
      *             BACK AND FORTH TO BOTH MACHINES.
      *  MJD 122995 PAGE 7 ALLOWS ENTRY OF FIELDS 37, 38. CHANGED MAX7 FROM
      *             38, TO 36.
      *  MJD 011796-011996 ENLARGED HOLD RECS FOR NEW RECORD SIZE.
      *             ADDED PAGE 8: PERIOD TABLE O2 AND INTEREST.
      *             ADDED PAGE 9: PL TABLE NO AND AMT.
      *             ADDED PAGE 10: PL TABLE O2 AND INTEREST.
      *             ADDED O2 AND INT TO REPO TABLE PAGE 6.
      *             ADDED DEALER RESERVE AND SOL/TRANS/PURCH LOAN INFO
      *              TO PAGE 1.
      *  MJD 012496 ADDED PAGE 11 PL COLLECTED PRIN/INT/LATE CHGS.
      *             MODIFIED NEW PAGE SELECTION TO MAKE IT MORE EFFICIENT.
      *             COPYIED FOR SEL LOGIC FROM SPMANX AND MODIFIED IT.
      *  MJD 012996 ADDED PAGE 12 DEALER RESERVE INFO.
      *  KEC 031197 CHANGED PAGE4 : AVAILABLE SPACE ON #3 - #8 (OLD APP)
      *             ADDED   PAGE13: DISPLAYING NEW APPLICATION STATS.
      *  JTG 032097 ADDED APP NOT TAKEN FIELDS
      *  BLV 051597 ADDED APPS MADE FIELDS - PAGE 13
      *  MJD 070197 ADDED BY-DLDISC TO PAGE 12
      *  MJD 071497 ADDED O3 INS TO PAGE 5.
      *  GLF 090997 REPLACED 'IO' ON GP-FILE WITH GET 'GPENV'
      *  MJD 091297 CHANGED PAGE 3 TO DISPLAY
      *             CUR/30/60/90/120/150/180/210+/<P>/<M>
      *  BLV 091597 DO ACCESS CALLS BEFORE OPENING BY
      *  MJD 091797 CHANGED PAGE 3 TO DISPLAY 'AGEING' ORDER
      *  MJD 112497 EXPANDED BY-REC TO 6153 BYTES
      *  JTG 021998 CORRECTED INPUT OF REPO 02 FIELDS PAGE 6,
      *             WAS MOVING VDUNUM0 TO 9(7)V99 FIELDS
      *  MJD 030498 ADDED BY-INS-NO-ELIG/WROTE/NOTWROTE AND BY-INS-AMT-WROTE
      *  MJD 031298 EXPANDED BY-SUMWGTTM AND BY-SUMWGTTA
      *  KEC 032498 USED THE EMPTY BUCKETS 10 & 11 IN
      *              '[B/T]Y-PERIOD-TBL' & '[B/T]Y-APP-PERIOD-TBL' FOR
      *              NEW SOURCE CODES 'RB' & 'RO'.
      *  MJD 051898 REMOVED BY-PASTDUE. CHANGED BY-EXREB TO BY-PART-CHGOFF
      *  KEC 061198 ADDED BANKRUPTCY-DELINQUENCY-TABLE TO PAGE 3.
      *             ADDED BY-PART-CHGOFF TO PAGE 9 & STILL IS ON PAGE 1.
      *  BLV 990823 ON PAGE 6, ALLOW ENTRY OF FIELDS 4, 5, 9, 10 - WERE
      *             NOT BEING USED, NOW ACTIVE
      * MJD 000323 ADDED PAGE 15, BY-UNEARNED-AMT (MERC #223)
      * MJD 000330 ADDED BY-PROCEEDS(1-2) TO PAGE 1. (MERC PR#223)
      * MJD 000929 ADDED BY-LOCKBOX (MFC PR#237)
      * BLV 010731 PAGE 3, WASN'T ALLOWING ACCESS TO FIELD 5: 150 DAY
      *            RECENCY UNITS & $, LOGIC WAS FOR RECENCY MISPAYS,
      *            CHANGED FROM FIELD 5 TO FIELD 8 (MISPAYS)
      * JTG 030318 ADDED GP-NO-SC-USE-DM -NO SALESFIN USE DIRECT MAIL WORLD #307
      * BLF 050114 CHANGED WORKER "TYPE" TO "TYPEX", RESERVED WORD
      * BLF 050513 MOVE GET-GPENV TO BEINNING OF PROCEDURE DIVISION, THERE WAS
      *            A CHECK ON GP-NO-SC-USE-DM BEFORE GET-GPENV
      * MJD 131119 A30 CONVERSION COMPLETED
      * BLM 160322 PAGE 5 HAD 6 INSURANCES, PAGE 14 HAD 10.  PUT 10 INSURANCES
      *            PLUS 3 FEE CODE INSURANCES ON PAGE 5, MOVED PAGE 15 TO 
      *            PAGE 14 (EARNINGS).  INCREASED CLASS. FIXED BY-DATE
      *            FOR NEW FORMAT
      * BAH 181026 GLOBAL BYFILE
      * KEC 190213 A32 F2SL - REVISED FOR SPLITTING THE BYFILE.
      *            REMOVED LOAN APP FIELDS IN BY & TY.
      * BAH 20190930 REMOVED ACCESS-CALL BYAFILE, BYBFILE
      *  CS 20191216 CHANGED WORK KEY BACK TO 9(7), SCREEN CHANGED TO 9(3) FOR
      *              CLASS, ADDED LOAD OF BYFILE BACK TO LOAD-BRANCH-ACCESS
      *              BECAUSE BYA-PATH/BYB-PATH WAS ALWAYS SPACES
      *  CS 20191218 SINCE YOU CANT SCAN ON ENTRY OF KEY AT THE BRANCH, I
      *              TALKED WITH KEN AND REMOVED AS PART OF THE SCANKEY ENTRY
      *              SCANKEY ENTRY IS JUST CLASS.
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
      *    W O R K I N G - S T O R A G E    S E C T I O N
      *
      ******************************************************************
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)MN/BYMAIN.CBL S35 02/23/24-10:24:42 >".

      ******************************************************************
      *
      *     P R O G R A M    W O R K E R S
      *
      ******************************************************************
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBLP/LP01LC.CPY".
       COPY "LIBLP/LP01LC_SQL.CPY".
       COPY "LIBLP/LPWSLC.CPY".
       COPY "LIBLP/LP01BY.CPY".
       COPY "LIBLP/LP01BYA.CPY".
       COPY "LIBLP/LP01BYB.CPY".
       COPY "LIBLP/LP01BY_SQL.CPY".
       COPY "LIBLP/LPWSBY.CPY".
      *----------------------------------------------------------------

       01 BYMAIN-WINDOW-HANDLE PIC X(10) VALUE SPACES.
       01  ERRCD           PIC X       VALUE SPACES.
       01  SUB1            PIC 9(2)    VALUE 0.
       01  SUB2            PIC 9(2)    VALUE 0.
       01  SUB3            PIC 9(2)    VALUE 0.
       01  SUB11           PIC 9(2)    VALUE 0.
       01  PLSUB           PIC 9(2)    VALUE 0.
       01  BYSUB1          PIC 9(2)    VALUE 0.
       01  BYSUB2          PIC 9(2)    VALUE 0.
       01  BYSUB3          PIC 9(2)    VALUE 0.

       01  TYPEX           PIC 9(02)   VALUE 0.
      *----------------------------------------------------------------
       01  ONE             PIC S9.
       01  ELE1            PIC 9(2).
       01  ELE2            PIC 9(2).
       01  ELE3            PIC 9(2).
       01  ELE4            PIC 9(2).
       01  ELE5            PIC 9(2).
       01  ELE6            PIC 9(2).
       01  ELE7            PIC 9(2).
       01  ELE8            PIC 9(2).
       01  ELE9            PIC 9(2).
       01  ELE10           PIC 9(2).
       01  ELE11           PIC 9(2).
       01  ELE12           PIC 9(2).
       01  ELE13           PIC 9(2).
       01  ELE14           PIC 9(2).
       01  ELEMENT         PIC 9(2).
       01  ACTION                VALUE SPACES.
           03  CHKEY             PIC X(4).
           03  CHNO              PIC 9(2).
           03  CHNO-X           REDEFINES CHNO
                                 PIC X(2).
       01  FORM-NUM1            REDEFINES ACTION.
           03  FORM-KEY1         PIC X(4).
           03  FORM-P1           PIC X.
           03  FORM-SEL1         PIC 9.
               88 FORM-1-DIGIT   VALUE 1 2 3 4 5 6 7 8 9.
       01  FORM-NUM2            REDEFINES ACTION.
           03  FORM-KEY2         PIC X(3).
           03  FORM-P2           PIC X.
           03  FORM-SEL2         PIC 99.
               88 FORM-2-DIGITS  VALUE 10 11 12 13 14.
      * 01  ACTION.
      *     03  CHKEY       PIC X(4).
      *     03  CHNO        PIC 9(2).
       01  IO-TYPE         PIC X(3).
       01  NEW-FLAG        PIC 9.
       01  NEW-FLAG-SCR2   PIC 9 VALUE 0.
       01  NEW-FLAG-SCR3   PIC 9 VALUE 0.
       01  NEW-FLAG-SCR4   PIC 9 VALUE 0.
       01  NEW-FLAG-SCR5   PIC 9 VALUE 0.
       01  NEW-FLAG-SCR6   PIC 9 VALUE 0.
       01  NEW-FLAG-SCR7   PIC 9 VALUE 0.
       01  NEW-FLAG-SCR8   PIC 9 VALUE 0.
       01  NEW-FLAG-SCR9   PIC 9 VALUE 0.
       01  NEW-FLAG-SCR10  PIC 9 VALUE 0.
       01  NEW-FLAG-SCR11  PIC 9 VALUE 0.
       01  NEW-FLAG-SCR12  PIC 9 VALUE 0.
       01  NEW-FLAG-SCR13  PIC 9 VALUE 0.
       01  NEW-FLAG-SCR14  PIC 9 VALUE 0.
       01  FIRST-SCAN      PIC 9 VALUE 0.

       01  WS-FIELDS.
           03  WS-STOP-ON-SCANKEY-FG   PIC X(1) VALUE "Y".
           03  WS-HOLD-DATE-CCYYMM     PIC 9(6) VALUE ZEROES.

      ******************************************************************
      *    H O L D - K E Y
      ******************************************************************
       01  HOLD-BY1-KEY.
           03  HOLD-BY-DATE-CCYYMM   PIC 9(6).
           03  HOLD-BY-BRNO          PIC 9(4).
           03  HOLD-BY-CLASS         PIC 9(3).
       01  HOLD-BRNAME     PIC X(20).
       01  HOLD-LCDESC     PIC X(20).

       01  ORIGINAL-BY-REC    PIC X(BY-SIZE).
       01  CHANGED-BY-REC     PIC X(BY-SIZE).

       01  DISPLAY-DATE    PIC 9(4).
       01  DISPLAY-DATEX   REDEFINES DISPLAY-DATE.
           03  DISPLAY-MO  PIC 99.
           03  DISPLAY-YR  PIC 99.
       01  DATE-LIST       PIC X(5)  VALUE "MMYY.".

      ******************************************************************
      *    F O R M
      ******************************************************************
       01  FORM-BUF.
           03  FORM-1       PIC X(9) VALUE "MN/BYMAN1".
           03  FORM-2       PIC X(9) VALUE "MN/BYMAN2".
           03  FORM-3       PIC X(9) VALUE "MN/BYMAN3".
           03  FORM-4       PIC X(9) VALUE "MN/BYMAN4".
           03  FORM-5       PIC X(9) VALUE "MN/BYMAN5".
           03  FORM-6       PIC X(9) VALUE "MN/BYMAN6".
           03  FORM-7       PIC X(9) VALUE "MN/BYMAN7".
           03  FORM-8       PIC X(9) VALUE "MN/BYMAN8".
           03  FORM-9       PIC X(9) VALUE "MN/BYMAN9".
           03  FORM-10      PIC X(9) VALUE "MN/BYMANA".
           03  FORM-11      PIC X(9) VALUE "MN/BYMANB".
           03  FORM-12      PIC X(9) VALUE "MN/BYMANC".
           03  FORM-13      PIC X(9) VALUE "MN/BYMAND".
           03  FORM-14      PIC X(9) VALUE "MN/BYMANE".
       01  FORM-BUFX REDEFINES FORM-BUF.
           03  FORM-NAMEX OCCURS 14.
               05 FORM-DIR   PIC X(2).
               05 FORM-SLASH PIC X(1).
               05 FORM-VFN  PIC X(6).

      ******************************************************************
      *    D I S P L A Y
      ******************************************************************
       01  DISPLAY-KEY.
           03  DISP-BRNO        PIC 9(4)  VALUE 0.
           03  DISP-BRNAME      PIC X(20) VALUE " ".
           03  DISP-LC          PIC 9(3)  VALUE 0.
           03  DISP-LCDESC      PIC X(20) VALUE " ".
       01  DISP1-KEY-LIST.
           03  FILLER     PIC X(18) VALUE
           "BRANCH BRX LC LCX.".

      ******************************************************************
      *    SCREEN DISPLAY BUFFER ----- PAGE 1
      ******************************************************************
       01  DISPLAY-BUF1    VALUE " ".
           03  F01     PIC ZZ,ZZZ,ZZ9.99-.
           03  F02     PIC ZZ,ZZZ,ZZ9.99-.
           03  F03     PIC ZZ,ZZZ,ZZ9.99-.
           03  F04     PIC ZZ,ZZZ,ZZ9.99-.
           03  F05     PIC ZZ,ZZZ,ZZ9.99-.
           03  F06     PIC ZZ,ZZZ,ZZ9.99-.
           03  F07     PIC ZZ,ZZZ,ZZ9.99-.
           03  F08     PIC ZZ,ZZZ,ZZ9.99-.
           03  F09     PIC ZZ,ZZZ,ZZ9.99-.
           03  F10     PIC ZZ,ZZZ,ZZ9.99-.
           03  F11     PIC ZZ,ZZZ,ZZ9.99-.
           03  F12     PIC ZZ,ZZZ,ZZ9.99-.
           03  F13     PIC ZZ,ZZZ,ZZ9.99-.
           03  F14     PIC ZZ,ZZZ,ZZ9.99-.
           03  F15     PIC ZZ,ZZZ,ZZ9.99-.
           03  F16     PIC ZZ,ZZZ,ZZ9.99-.
           03  F17     PIC ZZZZ9-.
           03  F171    PIC ZZ,ZZZ,ZZ9.99-.
           03  F18     PIC ZZZZ9-.
           03  F181    PIC ZZ,ZZZ,ZZ9.99-.
           03  F19     PIC ZZZZ9-.
           03  F191    PIC ZZ,ZZZ,ZZ9.99-.
           03  F20     PIC ZZZZ9-.
           03  F201    PIC ZZ,ZZZ,ZZ9.99-.
           03  F21     PIC ZZZZ9-.
           03  F211    PIC ZZ,ZZZ,ZZ9.99-.
           03  F22     PIC ZZZZ9-.
           03  F221    PIC ZZ,ZZZ,ZZ9.99-.
           03  F23     PIC ZZ,ZZZ,ZZ9.99-.
           03  F24     PIC ZZ,ZZZ,ZZ9.99-.

       01  DISPLAY-LIST1.
           03  FILLER     PIC X(48) VALUE
               "010 020 030 040 050 060 070 080 090 100 110 120 ".
           03  FILLER     PIC X(48) VALUE
               "130 140 150 160 170 171 180 181 190 191 ".
           03  FILLER     PIC X(32) VALUE
               "200 201 210 211 220 221 230 240.".

      ******************************************************************
      *    SCREEN DISPLAY BUFFER ----- PAGE 2
      ******************************************************************
       01  DISPLAY-BUF2.
           03  DATA-TYPE-TABLE  OCCURS 11.
               05  DATA-NOS     OCCURS 3 TIMES.
                   07  DATA-NO  PIC ZZZZ9-.
               05  DATA-AMTS    OCCURS 3.
                   07  DATA-AMT PIC ZZZZZZZ9.99-.
           03  DATA-120         PIC ZZ,ZZZ,ZZ9-.
           03  DATA-130         PIC ZZZ,ZZZ,ZZ9.99-.
           03  DATA-140         PIC ZZ,ZZZ,ZZZ,ZZ9.99-.
           03  DATA-150         PIC Z,ZZZ,ZZ9.99-.

       01  DISPLAY-LIST2.
           03  FILLER       PIC X(48) VALUE
           "010 011 012 013 014 015 020 021 022 023 024 025 ".
           03  FILLER       PIC X(48) VALUE
           "030 031 032 033 034 035 040 041 042 043 044 045 ".
           03  FILLER       PIC X(48) VALUE
           "050 051 052 053 054 055 060 061 062 063 064 065 ".
           03  FILLER       PIC X(48) VALUE
           "070 071 072 073 074 075 080 081 082 083 084 085 ".
           03  FILLER       PIC X(48) VALUE
           "090 091 092 093 094 095 100 101 102 103 104 105 ".
           03  FILLER       PIC X(40) VALUE
           "110 111 112 113 114 115 120 130 140 150.".

      ******************************************************************
      *    SCREEN DISPLAY BUFFER ----- PAGE 3
      ******************************************************************
       01  DISPLAY-BUF3.
      *     03  DEL-TABLE               OCCURS 18.
           03  DEL-TABLE               OCCURS 10.
               05  DEL-REC-CON         OCCURS 2.
                   07  DEL-UNITS       PIC Z(5)9-.
                   07  DEL-OUTBAL      PIC Z(8)9.99-.

       01  DISPLAY-LIST3.
           03  FILLER       PIC X(16) VALUE
           "010 011 012 013 ".
           03  FILLER       PIC X(16) VALUE
           "020 021 022 023 ".
           03  FILLER       PIC X(16) VALUE
           "030 031 032 033 ".
           03  FILLER       PIC X(16) VALUE
           "040 041 042 043 ".
           03  FILLER       PIC X(16) VALUE
           "050 051 052 053 ".
           03  FILLER       PIC X(16) VALUE
           "060 061 062 063 ".
           03  FILLER       PIC X(16) VALUE
           "070 071 072 073 ".
           03  FILLER       PIC X(16) VALUE
           "080 081 082 083 ".
           03  FILLER       PIC X(16) VALUE
           "090 091 092 093 ".
           03  FILLER       PIC X(16) VALUE
           "100 101 102 103.".

      ******************************************************************
      *    SCREEN DISPLAY BUFFER ----- PAGE 4
      ******************************************************************
       01  DISPLAY-BUF4    VALUE " ".
           03  I01     PIC ZZ,ZZZ,ZZ9.99-.
           03  I02     PIC ZZ,ZZZ,ZZ9.99-.
           03  I03     PIC ZZ,ZZZ,ZZ9.99-.
           03  I04     PIC ZZ,ZZZ,ZZ9.99-.
           03  I05     PIC ZZZZZ,ZZ9-.
           03  I06     PIC ZZZZZ,ZZ9-.
           03  I07     PIC ZZZZZ,ZZ9-.
           03  I08     PIC ZZZZZ,ZZ9-.
           03  I09     PIC ZZ,ZZZ,ZZ9-.
           03  I10     PIC ZZ,ZZZ,ZZ9-.
           03  I11     PIC ZZ,ZZZ,ZZ9-.
           03  I12     PIC ZZ,ZZZ,ZZ9-.
           03  I13     PIC ZZ,ZZZ,ZZ9-.
           03  I14     PIC ZZ,ZZZ,ZZ9-.
           03  I15     PIC ZZ,ZZZ,ZZ9-.
           03  I16     PIC ZZ,ZZZ,ZZ9-.
           03  I17     PIC ZZ,ZZZ,ZZ9-.
           03  I18     PIC ZZ,ZZZ,ZZ9-.
           03  I19     PIC ZZ,ZZZ,ZZ9-.
           03  I20     PIC ZZ,ZZZ,ZZ9-.
           03  I21     PIC ZZ,ZZZ,ZZ9-.
           03  I22     PIC ZZ,ZZZ,ZZ9-.
           03  I23     PIC ZZ,ZZZ,ZZ9-.
           03  I24     PIC ZZ,ZZZ,ZZ9-.
           03  I25     PIC ZZ,ZZZ,ZZ9.99-.
           03  I26     PIC ZZ,ZZZ,ZZ9.99-.
           03  I27     PIC ZZ,ZZZ,ZZ9.99-.
           03  I28     PIC ZZ,ZZZ,ZZ9.99-.
           03  I29     PIC ZZ,ZZZ,ZZ9.99-.
           03  I30     PIC ZZ,ZZZ,ZZ9.99-.
           03  I31     PIC ZZ,ZZZ,ZZ9.99-.
           03  I32     PIC ZZ,ZZZ,ZZ9.99-.

       01  DISPLAY-LIST4.
           03  FILLER     PIC X(32) VALUE
               "010 020 030 040 050 060 070 080 ".
           03  FILLER     PIC X(32) VALUE
               "090 100 110 120 130 140 150 160 ".
           03  FILLER     PIC X(32) VALUE
               "170 180 190 200 210 220 230 240 ".
           03  FILLER     PIC X(32) VALUE
               "250 260 270 280 290 300 310 320.".
      *=================================================================
      *    SCREEN DISPLAY BUFFER ----- PAGE 5
      *=================================================================
       01  DISPLAY-BUF5.
           03  DATA-MTD-YTD-TABLE         OCCURS 2.
               05  DATA-INS-TABLE             OCCURS 13.
                   07  DATA-INS-NO-ELIG       PIC ZZZZZ9-.
                   07  DATA-INS-NO-WROTE      PIC ZZZZZ9-.
                   07  DATA-INS-NO-NOTWROTE   PIC ZZZZZ9-.
                   07  DATA-INS-AMT-WROTE     PIC ZZZZZZZ9.99-.
       01  DISPLAY-LIST5.
           03  FILLER                     PIC X(32)     VALUE
               "010 011 012 013 020 021 022 023 ".
           03  FILLER                     PIC X(32)     VALUE
               "030 031 032 033 040 041 042 043 ".
           03  FILLER                     PIC X(32)     VALUE
               "050 051 052 053 060 061 062 063 ".
           03  FILLER                     PIC X(32)     VALUE
               "070 071 072 073 080 081 082 083 ".
           03  FILLER                     PIC X(32)     VALUE
               "090 091 092 093 100 101 102 103 ".
           03  FILLER                     PIC X(32)     VALUE
               "110 111 112 113 120 121 122 123 ".
           03  FILLER                     PIC X(16)     VALUE
               "130 131 132 133 ".
           03  FILLER                     PIC X(32)     VALUE
               "014 015 016 017 024 025 026 027 ".
           03  FILLER                     PIC X(32)     VALUE
               "034 035 036 037 044 045 046 047 ".
           03  FILLER                     PIC X(32)     VALUE
               "054 055 056 057 064 065 066 067 ".
           03  FILLER                     PIC X(32)     VALUE
               "074 075 076 077 084 085 086 087 ".
           03  FILLER                     PIC X(32)     VALUE
               "094 095 096 097 104 105 106 107 ".
           03  FILLER                     PIC X(32)     VALUE
               "114 115 116 117 124 125 126 127 ".
           03  FILLER                     PIC X(16)     VALUE
               "134 135 136 137.".
      ******************************************************************
      *    SCREEN DISPLAY BUFFER ----- PAGE 6
      ******************************************************************
       01  DISPLAY-BUF6.
           03  DATA-REPO-TYPE-TABLE  OCCURS 5.
               05  DATA-REPO-NOS     OCCURS 3 TIMES.
                   07  DATA-REPO-NO  PIC ZZZZ9-.
               05  DATA-REPO-AMTS    OCCURS 3.
                   07  DATA-REPO-AMT PIC ZZZZZZZ9.99-.

       01  DISPLAY-BUF6A.
           03  DATA-REPO-TYP-TABLE  OCCURS 5.
               05  DATA-O2S-REPO     OCCURS 3 TIMES.
                   07  DATA-O2-REPO  PIC ZZZZZZ9.99-.
               05  DATA-INTS-REPO    OCCURS 3.
                   07  DATA-INT-REPO PIC ZZZZZZ9.99-.

       01  DISPLAY-LIST6.
           03  FILLER       PIC X(48) VALUE
           "010 011 012 013 014 015 020 021 022 023 024 025 ".
           03  FILLER       PIC X(48) VALUE
           "030 031 032 033 034 035 040 041 042 043 044 045 ".
           03  FILLER       PIC X(24) VALUE
           "050 051 052 053 054 055.".

       01  DISPLAY-LIST6A.
           03  FILLER       PIC X(48) VALUE
           "060 061 062 063 064 065 070 071 072 073 074 075 ".
           03  FILLER       PIC X(48) VALUE
           "080 081 082 083 084 085 090 091 092 093 094 095 ".
           03  FILLER       PIC X(24) VALUE
           "100 101 102 103 104 105.".

      ******************************************************************
      *    SCREEN DISPLAY BUFFER ----- PAGE 7
      ******************************************************************
       01  DISPLAY-BUF7    VALUE " ".
           03  L01     PIC ZZ,ZZZ,ZZ9.99-.
           03  L02     PIC ZZ,ZZZ,ZZ9.99-.
           03  L03     PIC ZZ,ZZZ,ZZ9.99-.
           03  L04     PIC ZZ,ZZZ,ZZ9.99-.
           03  L05     PIC ZZ,ZZZ,ZZ9.99-.
           03  L06     PIC ZZ,ZZZ,ZZ9.99-.
           03  L07     PIC ZZ,ZZZ,ZZ9.99-.
           03  L08     PIC ZZ,ZZZ,ZZ9.99-.
           03  L09     PIC ZZ,ZZZ,ZZ9.99-.
           03  L10     PIC ZZ,ZZZ,ZZ9.99-.
           03  L11     PIC ZZ,ZZZ,ZZ9.99-.
           03  L12     PIC ZZ,ZZZ,ZZ9.99-.
           03  L13     PIC ZZ,ZZZ,ZZ9.99-.
           03  L14     PIC ZZ,ZZZ,ZZ9.99-.
           03  L15     PIC ZZ,ZZZ,ZZ9.99-.
           03  L16     PIC ZZ,ZZZ,ZZ9.99-.
           03  L17     PIC ZZ,ZZZ,ZZ9.99-.
           03  L18     PIC ZZ,ZZZ,ZZ9.99-.
           03  L19     PIC ZZ,ZZZ,ZZ9.99-.
           03  L20     PIC ZZ,ZZZ,ZZ9.99-.
           03  L21     PIC ZZ,ZZZ,ZZ9.99-.
           03  L22     PIC ZZ,ZZZ,ZZ9.99-.
           03  L23     PIC ZZ,ZZZ,ZZ9.99-.
           03  L24     PIC ZZ,ZZZ,ZZ9.99-.
           03  L25     PIC ZZ,ZZZ,ZZ9.99-.
           03  L26     PIC ZZ,ZZZ,ZZ9.99-.
           03  L27     PIC ZZ,ZZZ,ZZ9.99-.
           03  L28     PIC ZZ,ZZZ,ZZ9.99-.
           03  L29     PIC ZZ,ZZZ,ZZ9.99-.
           03  L30     PIC ZZ,ZZZ,ZZ9.99-.
           03  L31     PIC ZZ,ZZZ,ZZ9.99-.
           03  L32     PIC ZZ,ZZZ,ZZ9.99-.
           03  L33     PIC ZZ,ZZZ,ZZ9.99-.
           03  L34     PIC ZZ,ZZZ,ZZ9.99-.

       01  DISPLAY-LIST7.
           03  FILLER     PIC X(32) VALUE
               "010 020 030 040 050 060 070 080 ".
           03  FILLER     PIC X(32) VALUE
               "090 100 110 120 130 140 150 160 ".
           03  FILLER     PIC X(32) VALUE
               "170 180 190 200 210 220 230 240 ".
           03  FILLER     PIC X(32) VALUE
               "250 260 270 280 290 300 310 320 ".
           03  FILLER     PIC X(8) VALUE
               "330 340.".

      ******************************************************************
      *    SCREEN DISPLAY BUFFER ----- PAGE 8
      ******************************************************************
       01  DISPLAY-BUF8.
           03  DATA-TYPE-TABLE8  OCCURS 11.
               05  DATA-O2S     OCCURS 3 TIMES.
                   07  DATA-O2  PIC ZZZZZZZ9.99-.
               05  DATA-INTS    OCCURS 3.
                   07  DATA-INT PIC ZZZZZZZ9.99-.

       01  DISPLAY-LIST8.
           03  FILLER       PIC X(48) VALUE
           "010 011 012 013 014 015 020 021 022 023 024 025 ".
           03  FILLER       PIC X(48) VALUE
           "030 031 032 033 034 035 040 041 042 043 044 045 ".
           03  FILLER       PIC X(48) VALUE
           "050 051 052 053 054 055 060 061 062 063 064 065 ".
           03  FILLER       PIC X(48) VALUE
           "070 071 072 073 074 075 080 081 082 083 084 085 ".
           03  FILLER       PIC X(48) VALUE
           "090 091 092 093 094 095 100 101 102 103 104 105 ".
           03  FILLER       PIC X(24) VALUE
           "110 111 112 113 114 115.".

      ******************************************************************
      *    SCREEN DISPLAY BUFFER ----- PAGE 9
      ******************************************************************
       01  DISPLAY-BUF9.
           03  DATA-TYPE-TABLE9       OCCURS 10.
               05  DATA-NOS-PL        OCCURS 3 TIMES.
                   07  DATA-NO-PL     PIC ZZZZ9-.
               05  DATA-AMTS-PL       OCCURS 3.
                   07  DATA-AMT-PL    PIC ZZZZZZZ9.99-.
           03  DATA-PART-CHGOFF-MTD   PIC ZZ,ZZZ,ZZ9.99-.
           03  DATA-PART-CHGOFF-YTD   PIC ZZ,ZZZ,ZZ9.99-.

       01  DISPLAY-LIST9.
           03  FILLER       PIC X(48) VALUE
           "010 011 012 013 014 015 020 021 022 023 024 025 ".
           03  FILLER       PIC X(48) VALUE
           "030 031 032 033 034 035 040 041 042 043 044 045 ".
           03  FILLER       PIC X(48) VALUE
           "050 051 052 053 054 055 060 061 062 063 064 065 ".
           03  FILLER       PIC X(48) VALUE
           "070 071 072 073 074 075 080 081 082 083 084 085 ".
           03  FILLER       PIC X(48) VALUE
           "090 091 092 093 094 095 100 101 102 103 104 105 ".
           03  FILLER       PIC X(08) VALUE
           "110 111.".

      ******************************************************************
      *    SCREEN DISPLAY BUFFER ----- PAGE 10
      ******************************************************************
       01  DISPLAY-BUF10.
           03  DATA-TYPE-TABLE10  OCCURS 10.
               05  DATA-O2S-PL     OCCURS 3 TIMES.
                   07  DATA-O2-PL  PIC ZZZZZZZ9.99-.
               05  DATA-INTS-PL    OCCURS 3.
                   07  DATA-INT-PL PIC ZZZZZZZ9.99-.

       01  DISPLAY-LIST10.
           03  FILLER       PIC X(48) VALUE
           "010 011 012 013 014 015 020 021 022 023 024 025 ".
           03  FILLER       PIC X(48) VALUE
           "030 031 032 033 034 035 040 041 042 043 044 045 ".
           03  FILLER       PIC X(48) VALUE
           "050 051 052 053 054 055 060 061 062 063 064 065 ".
           03  FILLER       PIC X(48) VALUE
           "070 071 072 073 074 075 080 081 082 083 084 085 ".
           03  FILLER       PIC X(48) VALUE
           "090 091 092 093 094 095 100 101 102 103 104 105.".

      ******************************************************************
      *    SCREEN DISPLAY BUFFER ----- PAGE 11
      ******************************************************************
       01  DISPLAY-BUF11.
           03  DATA-TYPE-TABLE11  OCCURS 5.
               05  DATA-PL-PRINCOL  PIC ZZZZZZZ9.99-.
               05  DATA-PL-INTCOL   PIC ZZZZZZZ9.99-.
               05  DATA-PL-LATECOL  PIC ZZZZZZZ9.99-.
               05  DATA-PL-O2COL    PIC ZZZZZZZ9.99-.

       01  DISPLAY-LIST11.
           03  FILLER       PIC X(40) VALUE
           "010 011 020 021 030 031 040 041 050 051 ".
           03  FILLER       PIC X(40) VALUE
           "060 061 070 071 080 081 090 091 100 101.".

      ******************************************************************
      *    SCREEN DISPLAY BUFFER ----- PAGE 12
      ******************************************************************
       01  DISPLAY-BUF12.
           03  DATA-DLRV-TABLE  OCCURS 3.
               05  DATA-DLRVPRINX    OCCURS 3.
                   07 DATA-DLRVPRIN  PIC ZZZZZZZ9.99-.
               05  DATA-DLRVCHGSX    OCCURS 3.
                   07 DATA-DLRVCHGS  PIC ZZZZZZZ9.99-.
               05  DATA-DLPARVHELD   PIC ZZZZZZZ9.99-.
               05  DATA-DLDISC-ALP.
                   07  DATA-DLDISC   PIC ZZZZZZZ9.99-.

       01  DISPLAY-LIST12.
           03  FILLER       PIC X(32) VALUE
           "010 011 012 040 041 042 070 080 ".
           03  FILLER       PIC X(32) VALUE
           "020 021 022 050 051 052 071 081 ".
           03  FILLER       PIC X(32) VALUE
           "030 031 032 060 061 062 072 082.".

      *=================================================================
      *
      *    SCREEN DISPLAY BUFFER ----- PAGE 13
      *
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    GNLS = GAIN/LOSS     NVOL = NEW VOLUME
      *=================================================================
       01  DISPLAY-BUF13 PIC X(6).
       01  DISPLAY-LIST13.
           03  FILLER                     PIC X(48)     VALUE
               "010.".

      *=================================================================
      *    SCREEN DISPLAY BUFFER ----- PAGE 14
      *=================================================================
       01  DISPLAY-BUF14.
           03  P14-COLUMNS  OCCURS 3.
               05  P14-ROWS OCCURS 10.
                   07  P14-DISP-AMT   PIC ZZ,ZZZ,ZZZ.ZZ-  BLANK ZERO.
       01  DISPLAY-LIST14.
           03  FILLER        PIC X(40) VALUE
           "010 020 030 040 050 060 070 080 090 100 ".
           03  FILLER        PIC X(40) VALUE
           "011 021 031 041 051 061 071 081 091 101 ".
           03  FILLER        PIC X(40) VALUE
           "012 022 032 042 052 062 072 082 092 102.".
      ******************************************************************
      *    ELEMENT SPECIFICATIONS ----- PAGE 1
      ******************************************************************
       01  MAX              PIC 99  VALUE 0.
       78  MAX1                     VALUE 24.
       01  SPEC-TABLE1.
           03  FILLER      PIC X(8) VALUE "E NS082 ".
           03  FILLER      PIC X(8) VALUE "E NS082 ".
           03  FILLER      PIC X(8) VALUE "E NS082 ".
           03  FILLER      PIC X(8) VALUE "E NS082 ".
           03  FILLER      PIC X(8) VALUE "E NS082 ".
           03  FILLER      PIC X(8) VALUE "E NS082 ".
           03  FILLER      PIC X(8) VALUE "E NS082 ".
           03  FILLER      PIC X(8) VALUE "E NS082 ".
           03  FILLER      PIC X(8) VALUE "E NS082 ".
           03  FILLER      PIC X(8) VALUE "E NS082 ".
           03  FILLER      PIC X(8) VALUE "E NS082 ".
           03  FILLER      PIC X(8) VALUE "E NS082 ".
           03  FILLER      PIC X(8) VALUE "E NS082 ".
           03  FILLER      PIC X(8) VALUE "E NS082 ".
           03  FILLER      PIC X(8) VALUE "E NS082 ".
           03  FILLER      PIC X(8) VALUE "E NS082 ".
           03  FILLER      PIC X(8) VALUE "ENNS050 ".
           03  FILLER      PIC X(8) VALUE "ENNS050 ".
           03  FILLER      PIC X(8) VALUE "ENNS050 ".
           03  FILLER      PIC X(8) VALUE "ENNS050 ".

           03  FILLER      PIC X(8) VALUE "ENNS050 ".
           03  FILLER      PIC X(8) VALUE "ENNS050 ".
           03  FILLER      PIC X(8) VALUE "ENNS082 ".
           03  FILLER      PIC X(8) VALUE "ENNS082 ".

       01  SPEC-TAB1       REDEFINES SPEC-TABLE1.
           03  SPEC-REC1   OCCURS MAX1 TIMES.
               05  SPEC1   PIC X(7).
               05  CHFG1   PIC X.

      ******************************************************************
      *    ELEMENT SPECIFICATIONS ----- PAGE 2
      ******************************************************************
       01  MAX2             PIC 99   VALUE 15.
       01  SPEC-TABLE2.
           03  FILLER     PIC X(8) VALUE "ENNS050 ".
           03  FILLER     PIC X(8) VALUE "ENNS050 ".
           03  FILLER     PIC X(8) VALUE "ENNS050 ".
           03  FILLER     PIC X(8) VALUE "ENNS050 ".
           03  FILLER     PIC X(8) VALUE "ENNS050 ".
           03  FILLER     PIC X(8) VALUE "ENNS050 ".
           03  FILLER     PIC X(8) VALUE "ENNS050 ".
           03  FILLER     PIC X(8) VALUE "ENNS050 ".
           03  FILLER     PIC X(8) VALUE "ENNS050 ".
           03  FILLER     PIC X(8) VALUE "ENNS050 ".
           03  FILLER     PIC X(8) VALUE "ENNS050 ".
           03  FILLER     PIC X(8) VALUE "E NS080 ".
           03  FILLER     PIC X(8) VALUE "E NS092 ".
           03  FILLER     PIC X(8) VALUE "E NS112 ".
           03  FILLER     PIC X(8) VALUE "E NS072 ".
       01  SPEC-TAB2       REDEFINES SPEC-TABLE2.
           03  SPEC-REC2   OCCURS 15 TIMES.
               05  SPEC2   PIC X(7).
               05  CHFG2   PIC X.

      ******************************************************************
      *    ELEMENT SPECIFICATIONS ----- PAGE 3
      ******************************************************************
       01  MAX3           PIC 99   VALUE 10.
       01  SPEC-TABLE3.
           03  FILLER     PIC X(8) VALUE "ENNS060 ".
           03  FILLER     PIC X(8) VALUE "ENNS060 ".
           03  FILLER     PIC X(8) VALUE "ENNS060 ".
           03  FILLER     PIC X(8) VALUE "ENNS060 ".
           03  FILLER     PIC X(8) VALUE "ENNS060 ".
           03  FILLER     PIC X(8) VALUE "ENNS060 ".
           03  FILLER     PIC X(8) VALUE "ENNS060 ".
           03  FILLER     PIC X(8) VALUE "ENNS060 ".
           03  FILLER     PIC X(8) VALUE "ENNS060 ".
           03  FILLER     PIC X(8) VALUE "ENNS060 ".
      *     03  FILLER     PIC X(8) VALUE "ENNS060 ".
      *     03  FILLER     PIC X(8) VALUE "ENNS060 ".
      *     03  FILLER     PIC X(8) VALUE "ENNS060 ".
      *     03  FILLER     PIC X(8) VALUE "ENNS060 ".
      *     03  FILLER     PIC X(8) VALUE "ENNS060 ".
      *     03  FILLER     PIC X(8) VALUE "ENNS060 ".
      *     03  FILLER     PIC X(8) VALUE "ENNS060 ".
      *     03  FILLER     PIC X(8) VALUE "ENNS060 ".

       01  SPEC-TAB3       REDEFINES SPEC-TABLE3.
           03  SPEC-REC3   OCCURS 10 TIMES.
               05  SPEC3   PIC X(7).
               05  CHFG3   PIC X.

      ******************************************************************
      *    ELEMENT SPECIFICATIONS ----- PAGE 4
      ******************************************************************
       01  MAX4            PIC 99  VALUE 32.
       01  SPEC-TABLE4.
           03  FILLER      PIC X(8) VALUE "E NS082 ".
           03  FILLER      PIC X(8) VALUE "E NS082 ".
           03  FILLER      PIC X(8) VALUE "E NS082 ".
           03  FILLER      PIC X(8) VALUE "E NS082 ".
           03  FILLER      PIC X(8) VALUE "E NS050 ".
           03  FILLER      PIC X(8) VALUE "E NS050 ".
           03  FILLER      PIC X(8) VALUE "E NS050 ".
           03  FILLER      PIC X(8) VALUE "E NS050 ".
           03  FILLER      PIC X(8) VALUE "E NS060 ".
           03  FILLER      PIC X(8) VALUE "E NS060 ".
           03  FILLER      PIC X(8) VALUE "E NS060 ".
           03  FILLER      PIC X(8) VALUE "E NS060 ".
           03  FILLER      PIC X(8) VALUE "E NS060 ".
           03  FILLER      PIC X(8) VALUE "E NS060 ".
           03  FILLER      PIC X(8) VALUE "E NS060 ".
           03  FILLER      PIC X(8) VALUE "E NS060 ".
           03  FILLER      PIC X(8) VALUE "E NS060 ".
           03  FILLER      PIC X(8) VALUE "E NS060 ".
           03  FILLER      PIC X(8) VALUE "E NS060 ".
           03  FILLER      PIC X(8) VALUE "E NS060 ".
           03  FILLER      PIC X(8) VALUE "E NS060 ".
           03  FILLER      PIC X(8) VALUE "E NS060 ".
           03  FILLER      PIC X(8) VALUE "E NS060 ".
           03  FILLER      PIC X(8) VALUE "E NS060 ".
           03  FILLER      PIC X(8) VALUE "E NS082 ".
           03  FILLER      PIC X(8) VALUE "E NS082 ".
           03  FILLER      PIC X(8) VALUE "E NS082 ".
           03  FILLER      PIC X(8) VALUE "E NS082 ".
           03  FILLER      PIC X(8) VALUE "E NS082 ".
           03  FILLER      PIC X(8) VALUE "E NS082 ".
           03  FILLER      PIC X(8) VALUE "E NS082 ".
           03  FILLER      PIC X(8) VALUE "E NS082 ".
           03  FILLER      PIC X(8) VALUE "E NS082 ".
       01  SPEC-TAB4       REDEFINES SPEC-TABLE4.
           03  SPEC-REC4   OCCURS 32 TIMES.
               05  SPEC4   PIC X(7).
               05  CHFG4   PIC X.
      *=================================================================
      *    ELEMENT SPECIFICATIONS ----- PAGE 5
      *=================================================================
       01  MAX5           PIC 99   VALUE 13.
       01  SPEC-TABLE5.
           03  FILLER     PIC X(8) VALUE "ENNS070 ".
           03  FILLER     PIC X(8) VALUE "ENNS070 ".
           03  FILLER     PIC X(8) VALUE "ENNS070 ".
           03  FILLER     PIC X(8) VALUE "ENNS070 ".
           03  FILLER     PIC X(8) VALUE "ENNS070 ".
           03  FILLER     PIC X(8) VALUE "ENNS070 ".
           03  FILLER     PIC X(8) VALUE "ENNS070 ".
           03  FILLER     PIC X(8) VALUE "ENNS070 ".
           03  FILLER     PIC X(8) VALUE "ENNS070 ".
           03  FILLER     PIC X(8) VALUE "ENNS070 ".
           03  FILLER     PIC X(8) VALUE "ENNS070 ".
           03  FILLER     PIC X(8) VALUE "ENNS070 ".
           03  FILLER     PIC X(8) VALUE "ENNS070 ".
       01  SPEC-TAB5      REDEFINES SPEC-TABLE5.
           03  SPEC-REC5  OCCURS 13 TIMES.
               05  SPEC5  PIC X(7).
               05  CHFG5  PIC X.
      ******************************************************************
      *    ELEMENT SPECIFICATIONS ----- PAGE 6
      ******************************************************************
       01  MAX6             PIC 99   VALUE 10.
       01  SPEC-TABLE6.
           03  FILLER     PIC X(8) VALUE "ENNS050 ".
           03  FILLER     PIC X(8) VALUE "ENNS050 ".
           03  FILLER     PIC X(8) VALUE "ENNS050 ".
           03  FILLER     PIC X(8) VALUE "ENNS050 ".
           03  FILLER     PIC X(8) VALUE "ENNS050 ".
           03  FILLER     PIC X(8) VALUE "ENNS072 ".
           03  FILLER     PIC X(8) VALUE "ENNS072 ".
           03  FILLER     PIC X(8) VALUE "ENNS072 ".
           03  FILLER     PIC X(8) VALUE "ENNS072 ".
           03  FILLER     PIC X(8) VALUE "ENNS072 ".
       01  SPEC-TAB6       REDEFINES SPEC-TABLE6.
           03  SPEC-REC6   OCCURS 10 TIMES.
               05  SPEC6   PIC X(7).
               05  CHFG6   PIC X.

      ******************************************************************
      *    ELEMENT SPECIFICATIONS ----- PAGE 7
      ******************************************************************
       01  MAX7             PIC 99  VALUE 34.
       01  SPEC-TABLE7.
           03  FILLER      PIC X(8) VALUE "E NS082 ".
           03  FILLER      PIC X(8) VALUE "E NS082 ".
           03  FILLER      PIC X(8) VALUE "E NS082 ".
           03  FILLER      PIC X(8) VALUE "E NS082 ".
           03  FILLER      PIC X(8) VALUE "E NS082 ".
           03  FILLER      PIC X(8) VALUE "E NS082 ".
           03  FILLER      PIC X(8) VALUE "E NS082 ".
           03  FILLER      PIC X(8) VALUE "E NS082 ".
           03  FILLER      PIC X(8) VALUE "E NS082 ".
           03  FILLER      PIC X(8) VALUE "E NS082 ".
           03  FILLER      PIC X(8) VALUE "E NS082 ".
           03  FILLER      PIC X(8) VALUE "E NS082 ".
           03  FILLER      PIC X(8) VALUE "E NS082 ".
           03  FILLER      PIC X(8) VALUE "E NS082 ".
           03  FILLER      PIC X(8) VALUE "E NS082 ".
           03  FILLER      PIC X(8) VALUE "E NS082 ".
           03  FILLER      PIC X(8) VALUE "E NS082 ".
           03  FILLER      PIC X(8) VALUE "E NS082 ".
           03  FILLER      PIC X(8) VALUE "E NS082 ".
           03  FILLER      PIC X(8) VALUE "E NS082 ".
           03  FILLER      PIC X(8) VALUE "E NS082 ".
           03  FILLER      PIC X(8) VALUE "E NS082 ".
           03  FILLER      PIC X(8) VALUE "E NS082 ".
           03  FILLER      PIC X(8) VALUE "E NS082 ".
           03  FILLER      PIC X(8) VALUE "E NS082 ".
           03  FILLER      PIC X(8) VALUE "E NS082 ".
           03  FILLER      PIC X(8) VALUE "E NS082 ".
           03  FILLER      PIC X(8) VALUE "E NS082 ".
           03  FILLER      PIC X(8) VALUE "E NS082 ".
           03  FILLER      PIC X(8) VALUE "E NS082 ".
           03  FILLER      PIC X(8) VALUE "E NS082 ".
           03  FILLER      PIC X(8) VALUE "E NS082 ".
           03  FILLER      PIC X(8) VALUE "E NS082 ".
           03  FILLER      PIC X(8) VALUE "E NS082 ".

       01  SPEC-TAB7       REDEFINES SPEC-TABLE7.
           03  SPEC-REC7   OCCURS 34 TIMES.
               05  SPEC7   PIC X(7).
               05  CHFG7   PIC X.

      ******************************************************************
      *    ELEMENT SPECIFICATIONS ----- PAGE 8
      ******************************************************************
       01  MAX8             PIC 99   VALUE 11.
       01  SPEC-TABLE8.
           03  FILLER     PIC X(8) VALUE "ENNS082 ".
           03  FILLER     PIC X(8) VALUE "ENNS082 ".
           03  FILLER     PIC X(8) VALUE "ENNS082 ".
           03  FILLER     PIC X(8) VALUE "ENNS082 ".
           03  FILLER     PIC X(8) VALUE "ENNS082 ".
           03  FILLER     PIC X(8) VALUE "ENNS082 ".
           03  FILLER     PIC X(8) VALUE "ENNS082 ".
           03  FILLER     PIC X(8) VALUE "ENNS082 ".
           03  FILLER     PIC X(8) VALUE "ENNS082 ".
           03  FILLER     PIC X(8) VALUE "ENNS082 ".
           03  FILLER     PIC X(8) VALUE "ENNS082 ".
       01  SPEC-TAB8       REDEFINES SPEC-TABLE8.
           03  SPEC-REC8   OCCURS 11 TIMES.
               05  SPEC8   PIC X(7).
               05  CHFG8   PIC X.

      ******************************************************************
      *    ELEMENT SPECIFICATIONS ----- PAGE 9
      ******************************************************************
       01  MAX9             PIC 99   VALUE 11.
       01  SPEC-TABLE9.
           03  FILLER     PIC X(8) VALUE "ENNS050 ".
           03  FILLER     PIC X(8) VALUE "ENNS050 ".
           03  FILLER     PIC X(8) VALUE "ENNS050 ".
           03  FILLER     PIC X(8) VALUE "ENNS050 ".
           03  FILLER     PIC X(8) VALUE "ENNS050 ".
           03  FILLER     PIC X(8) VALUE "ENNS050 ".
           03  FILLER     PIC X(8) VALUE "ENNS050 ".
           03  FILLER     PIC X(8) VALUE "ENNS050 ".
           03  FILLER     PIC X(8) VALUE "ENNS050 ".
           03  FILLER     PIC X(8) VALUE "ENNS050 ".
           03  FILLER     PIC X(8) VALUE "E NS082 ".
       01  SPEC-TAB9       REDEFINES SPEC-TABLE9.
           03  SPEC-REC9   OCCURS 11 TIMES.
               05  SPEC9   PIC X(7).
               05  CHFG9   PIC X.

      ******************************************************************
      *    ELEMENT SPECIFICATIONS ----- PAGE 10
      ******************************************************************
       01  MAX10             PIC 99   VALUE 10.
       01  SPEC-TABLE10.
           03  FILLER     PIC X(8) VALUE "ENNS082 ".
           03  FILLER     PIC X(8) VALUE "ENNS082 ".
           03  FILLER     PIC X(8) VALUE "ENNS082 ".
           03  FILLER     PIC X(8) VALUE "ENNS082 ".
           03  FILLER     PIC X(8) VALUE "ENNS082 ".
           03  FILLER     PIC X(8) VALUE "ENNS082 ".
           03  FILLER     PIC X(8) VALUE "ENNS082 ".
           03  FILLER     PIC X(8) VALUE "ENNS082 ".
           03  FILLER     PIC X(8) VALUE "ENNS082 ".
           03  FILLER     PIC X(8) VALUE "ENNS082 ".
       01  SPEC-TAB10       REDEFINES SPEC-TABLE10.
           03  SPEC-REC10   OCCURS 10 TIMES.
               05  SPEC10   PIC X(7).
               05  CHFG10   PIC X.

      ******************************************************************
      *    ELEMENT SPECIFICATIONS ----- PAGE 11
      ******************************************************************
       01  MAX11             PIC 99   VALUE 10.
       01  SPEC-TABLE11.
           03  FILLER     PIC X(8) VALUE "ENNS082 ".
           03  FILLER     PIC X(8) VALUE "ENNS082 ".
           03  FILLER     PIC X(8) VALUE "ENNS082 ".
           03  FILLER     PIC X(8) VALUE "ENNS082 ".
           03  FILLER     PIC X(8) VALUE "ENNS082 ".
           03  FILLER     PIC X(8) VALUE "ENNS082 ".
           03  FILLER     PIC X(8) VALUE "ENNS082 ".
           03  FILLER     PIC X(8) VALUE "ENNS082 ".
           03  FILLER     PIC X(8) VALUE "ENNS082 ".
           03  FILLER     PIC X(8) VALUE "ENNS082 ".
       01  SPEC-TAB11       REDEFINES SPEC-TABLE11.
           03  SPEC-REC11   OCCURS 10 TIMES.
               05  SPEC11   PIC X(7).
               05  CHFG11   PIC X.

      ******************************************************************
      *    ELEMENT SPECIFICATIONS ----- PAGE 12
      ******************************************************************
       01  MAX12             PIC 99   VALUE 8.
       01  SPEC-TABLE12.
           03  FILLER     PIC X(8) VALUE "ENNS082 ".
           03  FILLER     PIC X(8) VALUE "ENNS082 ".
           03  FILLER     PIC X(8) VALUE "ENNS082 ".
           03  FILLER     PIC X(8) VALUE "ENNS082 ".
           03  FILLER     PIC X(8) VALUE "ENNS082 ".
           03  FILLER     PIC X(8) VALUE "ENNS082 ".
           03  FILLER     PIC X(8) VALUE "ENNS082 ".
           03  FILLER     PIC X(8) VALUE "ENNS082 ".
       01  SPEC-TAB12       REDEFINES SPEC-TABLE12.
           03  SPEC-REC12   OCCURS 8 TIMES.
               05  SPEC12   PIC X(7).
               05  CHFG12   PIC X.

      *=================================================================
      *
      *    ELEMENT SPECIFICATIONS ----- PAGE 13
      *
      *=================================================================
       01  MAX13            PIC 99   VALUE 1.
       01  SPEC-TABLE13.
           03  FILLER     PIC X(8) VALUE "E  A010 ".
       01  SPEC-TAB13      REDEFINES SPEC-TABLE13.
           03  SPEC-REC13  OCCURS 1 TIMES.
               05  SPEC13  PIC X(7).
               05  CHFG13  PIC X.
      *=================================================================
      *    ELEMENT SPECIFICATIONS ----- PAGE 14
      *=================================================================
       78  MAX14                     VALUE 10.
       01  SPEC-TABLE14.
           03  FILLER     PIC X(8) VALUE "E NS082 ".
           03  FILLER     PIC X(8) VALUE "E NS082 ".
           03  FILLER     PIC X(8) VALUE "E NS082 ".
           03  FILLER     PIC X(8) VALUE "E NS082 ".
           03  FILLER     PIC X(8) VALUE "E NS082 ".
           03  FILLER     PIC X(8) VALUE "E NS082 ".
           03  FILLER     PIC X(8) VALUE "E NS082 ".
           03  FILLER     PIC X(8) VALUE "E NS082 ".
           03  FILLER     PIC X(8) VALUE "E NS082 ".
           03  FILLER     PIC X(8) VALUE "E NS082 ".
       01  SPEC-TAB14      REDEFINES SPEC-TABLE14.
           03  SPEC-REC14  OCCURS MAX14 TIMES.
               05  SPEC14  PIC X(7).
               05  CHFG14  PIC X.
      ******************************************************************
      *            HOLD/WORK AREAS
      ******************************************************************
       01  ACT-TYPE-TABLE.
           03  FILLER     PIC X(21) VALUE
           "*** NET GAIN/LOSS ***".
           03  FILLER     PIC X(21) VALUE
           "*** NEW VOLUME ***   ".
           03  FILLER     PIC X(43) VALUE
           "       TODATE     L-MON      L-YR".
           03  FILLER     PIC X(37) VALUE
           "TODATE          L-MON           L-YR".
           03  FILLER     PIC X(43) VALUE
           "          MTD       YTD       LYR".
           03  FILLER     PIC X(37) VALUE
           "   MTD            YTD            LYR".
       01  ACT-TYPES REDEFINES ACT-TYPE-TABLE.
           03  ACT-TYPE   PIC X(21) OCCURS 2 TIMES.
           03  ACT-TYPEL  PIC X(80) OCCURS 2 TIMES.

       01  ACT-TYPE-TABLE8.
           03  FILLER     PIC X(43) VALUE
           "         TODATE       L-MON       L-YR     ".
           03  FILLER     PIC X(37) VALUE
           "    TODATE       L-MON        L-YR   ".
           03  FILLER     PIC X(43) VALUE
           "          MTD          YTD         LYR     ".
           03  FILLER     PIC X(37) VALUE
           "     MTD          YTD         LYR    ".
       01  ACT-TYPES8 REDEFINES ACT-TYPE-TABLE8.
           03  ACT-TYPEL8  PIC X(80) OCCURS 2 TIMES.

       01  ACT-TYPE-TABLE12.
           03  FILLER     PIC X(37) VALUE SPACES.
           03  FILLER     PIC X(43) VALUE
           "   TODATE          L-MON           L-YR    ".
           03  FILLER     PIC X(37) VALUE SPACES.
           03  FILLER     PIC X(43) VALUE
           "      MTD            YTD            LYR    ".
       01  ACT-TYPES12 REDEFINES ACT-TYPE-TABLE12.
           03  ACT-TYPEL12  PIC X(80) OCCURS 2 TIMES.

       01  ACT-TYPE-TABLE14.
           03  FILLER     PIC X(22) VALUE 
           "     GAIN / LOSS      ".
           03  FILLER     PIC X(49) VALUE
           "      CURRENT     END-LAST-MONTH    END-LAST-YEAR".
           03  FILLER     PIC X(9)  VALUE SPACES.
           03  FILLER     PIC X(22) VALUE 
           "        VOLUME        ".
           03  FILLER     PIC X(49) VALUE
           "MONTH-TO-DATE       YEAR-TO-DATE        LAST-YEAR".
           03  FILLER     PIC X(9)  VALUE SPACES.
       01  ACT-TYPES14 REDEFINES ACT-TYPE-TABLE14.
           03  ACT-TYPE14  PIC X(80) OCCURS 2 TIMES.

       01  ACT-MTD-TABLE.
           03  FILLER     PIC X(3) VALUE "MTD".
           03  FILLER     PIC X(3) VALUE "YTD".
       01  MTD-YTD-TYPE REDEFINES ACT-MTD-TABLE.
           03  MTD-TYPE   PIC X(3) OCCURS 2.

       01  ACT-FNAME                   PIC X(9)    VALUE "ACT_TYPE.".
       01  MTD-FNAME                   PIC X(9)    VALUE "MTDYTD:P.".
       01  ACT-FNAMEL                  PIC X(11)   VALUE "ACT_LINE:P.".
       01  ACT-FNAMEL2                 PIC X(12)   VALUE "ACT_LINE2:P.".
       01  ACT-BUFFER                  PIC X(80)   VALUE " ".
       01  MTD-BUFFER                  PIC X(3)    VALUE " ".
       01  ACTIVITY                    PIC 9       VALUE 1.
       01  ACTYP-COD                   PIC 9       VALUE ZERO.

      *-----------------------------------------------------------------
      *
      *    PERIOD TYPE ----- FOR PAGE 13
      *
      *-----------------------------------------------------------------
      *01  PER-LIST.
      *    03  FILLER                  PIC X(31)   VALUE
      *        "PER_GNLS PER_NVOL.".

      *01  PER-BUFFER.
      *    03  PER-BUF-GNLS            PIC X(22)   VALUE " ".
      *    03  PER-BUF-NVOL            PIC X(22)   VALUE " ".

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *01  PER-TYPE-TABLE.
      *    03  FILLER                  PIC X(22) VALUE
      *        "   *** CURRENT ***    ".
      *    03  FILLER                  PIC X(22) VALUE
      *        "   ***   MTD   ***    ".
      *    03  FILLER                  PIC X(22) VALUE
      *        "*** END LAST MONTH ***".
      *    03  FILLER                  PIC X(22) VALUE
      *        "***      YTD       ***".
      *    03  FILLER                  PIC X(22) VALUE
      *        "*** END LAST YEAR *** ".
      *    03  FILLER                  PIC X(22) VALUE
      *        "***      LYR      *** ".
      *01  PER-TYPES                  REDEFINES PER-TYPE-TABLE.
      *    03  PER-TYPE                PIC X(22) OCCURS 6 TIMES.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       01  PERIOD                      PIC 9       VALUE 1.

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

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
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
       01  PAGE-FLAG      PIC 99 VALUE 1.
           88  PAGE1      VALUE 1.
           88  PAGE2      VALUE 2.
           88  PAGE3      VALUE 3.
           88  PAGE4      VALUE 4.
           88  PAGE5      VALUE 5.
           88  PAGE6      VALUE 6.
           88  PAGE7      VALUE 7.
           88  PAGE8      VALUE 8.
           88  PAGE9      VALUE 9.
           88  PAGE10     VALUE 10.
           88  PAGE11     VALUE 11.
           88  PAGE12     VALUE 12.
           88  PAGE13     VALUE 13.
           88  PAGE14     VALUE 14.
      ******************************************************************
      *     C O P Y    L I B R A R Y    W O R K E R S
      ******************************************************************
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBMN/BYMAN1_DEF.CPY".
       COPY "LIBMN/BYMAN2_DEF.CPY".
       COPY "LIBMN/BYMAN3_DEF.CPY".
       COPY "LIBMN/BYMAN4_DEF.CPY".
       COPY "LIBMN/BYMAN5_DEF.CPY".
       COPY "LIBMN/BYMAN6_DEF.CPY".
       COPY "LIBMN/BYMAN7_DEF.CPY".
       COPY "LIBMN/BYMAN8_DEF.CPY".
       COPY "LIBMN/BYMAN9_DEF.CPY".
       COPY "LIBMN/BYMANA_DEF.CPY".
       COPY "LIBMN/BYMANB_DEF.CPY".
       COPY "LIBMN/BYMANC_DEF.CPY".
       COPY "LIBMN/BYMAND_DEF.CPY".
       COPY "LIBMN/BYMANE_DEF.CPY".
       COPY "LIBMN/BYMAN1_WKS.CPY".
       COPY "LIBMN/BYMAN2_WKS.CPY".
       COPY "LIBMN/BYMAN3_WKS.CPY".
       COPY "LIBMN/BYMAN4_WKS.CPY".
       COPY "LIBMN/BYMAN5_WKS.CPY".
       COPY "LIBMN/BYMAN6_WKS.CPY".
       COPY "LIBMN/BYMAN7_WKS.CPY".
       COPY "LIBMN/BYMAN8_WKS.CPY".
       COPY "LIBMN/BYMAN9_WKS.CPY".
       COPY "LIBMN/BYMANA_WKS.CPY".
       COPY "LIBMN/BYMANB_WKS.CPY".
       COPY "LIBMN/BYMANC_WKS.CPY".
       COPY "LIBMN/BYMAND_WKS.CPY".
       COPY "LIBMN/BYMANE_WKS.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/GPENVW.CPY".
       COPY "LIBGB/ACCESSW.CPY".

       COPY "LIBGB/GETFMW.CPY".

       SCREEN SECTION.
       COPY "LIBMN/BYMAN1_SCN.CPY".
       COPY "LIBMN/BYMAN2_SCN.CPY".
       COPY "LIBMN/BYMAN3_SCN.CPY".
       COPY "LIBMN/BYMAN4_SCN.CPY".
       COPY "LIBMN/BYMAN5_SCN.CPY".
       COPY "LIBMN/BYMAN6_SCN.CPY".
       COPY "LIBMN/BYMAN7_SCN.CPY".
       COPY "LIBMN/BYMAN8_SCN.CPY".
       COPY "LIBMN/BYMAN9_SCN.CPY".
       COPY "LIBMN/BYMANA_SCN.CPY".
       COPY "LIBMN/BYMANB_SCN.CPY".
       COPY "LIBMN/BYMANC_SCN.CPY".
       COPY "LIBMN/BYMAND_SCN.CPY".
       COPY "LIBMN/BYMANE_SCN.CPY".

       PROCEDURE DIVISION
            USING FORM-PATHNAME EXIT-PATHNAME.

      ******************************************************************
      *
      *    M A I N    P R O G R A M
      *
      ******************************************************************
       MAIN-PROGRAM SECTION.

           PERFORM SQL-CONNECT.

      D    STOP MESS.

           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE FORM-NAMEX(1) TO FORM-NAM.
           MOVE FORM-VFN(1)   TO VDU-FORM-NAME.
           MOVE FORM-PATH     TO FORM-PATHNAME.

           MOVE 1 TO PAGE-FLAG.

       COPY "LIBGB/CHKSEC.CPY".

           MOVE FORM-PATHNAME TO FORM-PATH.
           DISPLAY FLOATING GRAPHICAL WINDOW
                   SCREEN LINE 01 * VDU-WIN-L-OFFSET
                   SCREEN COL 01 * VDU-WIN-C-OFFSET
           COLOR IS VDU-WCOLOR-TCLP,CONTROL VALUE IS VDU-WCTRL-TCLP
                   CELL SIZE IS LABEL FONT
                   SIZE 80 + VDU-WIN-W-MOD
                   LINES 24 + VDU-WIN-H-MOD
                   BLANK SCREEN
                   HANDLE IS BYMAIN-WINDOW-HANDLE.


       MAIN-MODULE.

           PERFORM GET-GPENV.

           MOVE 1 TO HEADER-LEV HEADER-LEVEL DELINQ DELTYP-COD.

           PERFORM FORM-RESET.

      *KC  IF ( PAGE2 OR PAGE8 OR PAGE13 )  
           IF ( PAGE2 OR PAGE8 )            
              MOVE "S  A020SRCD:S." TO VDU
              IF ( GP-THE-ONLY-SF-SRCD NOT = " " )
                 MOVE GP-THE-ONLY-SF-SRCD TO VDUBUF
              ELSE
                 MOVE "RF" TO VDUBUF
              END-IF
              PERFORM SCREENIO

              MOVE "S  A020SCDM:S." TO VDU
              IF GP-NO-SC-USE-DM = "Y"
                 MOVE "DM" TO VDUBUF
              ELSE
                 MOVE "SC" TO VDUBUF
              END-IF
              PERFORM SCREENIO.

           PERFORM REC-BY-MODULE.

           IF IO-TYPE = "END"
              GO TO END-ROUTINE.

           IF IO-TYPE = "CLR"
              GO TO MAIN-MODULE.

           PERFORM ENTRY-MODULE.

           IF IO-TYPE = "RWR"
              PERFORM WRITE-MODULE.

           IF IO-TYPE = "DEL"
             PERFORM DELETE-MODULE.

           GO TO MAIN-MODULE.

      ******************************************************************
       REC-BY-MODULE SECTION.
      ******************************************************************

       REC-BY-MODULE-NO.
           MOVE 0 TO NEW-FLAG
                     NEW-FLAG-SCR2
                     NEW-FLAG-SCR3
                     NEW-FLAG-SCR4
                     NEW-FLAG-SCR5
                     NEW-FLAG-SCR6
                     NEW-FLAG-SCR7
                     NEW-FLAG-SCR8
                     NEW-FLAG-SCR9
                     NEW-FLAG-SCR10
                     NEW-FLAG-SCR11
                     NEW-FLAG-SCR12
                     NEW-FLAG-SCR13
                     NEW-FLAG-SCR14.

           MOVE " " TO IO-TYPE.

           PERFORM GET-TODAYS-DATE.

           GO TO SETUP-BY-DATE.

       ENTER-DATE.
           MOVE "EN N040MMYY." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "1<"
              PERFORM SCAN-MESS
              GO TO ENTER-DATE.

           IF VDUSTATUS = "1>"
              MOVE "END" TO IO-TYPE
              GO TO END-REC-BY-MODULE.

           IF VDUSTATUS NOT = "00"
              GO TO REC-BY-MODULE-NO.

           IF VDUNUM0 = 0
              GO TO REC-BY-MODULE-NO.

           MOVE VDUNUM0 TO DISPLAY-DATE.
 
       SETUP-BY-DATE.
           MOVE DISPLAY-MO TO DATE-MMDDYY-MM.
           MOVE DISPLAY-YR TO DATE-MMDDYY-YY.
           PERFORM CONVERT-MMDDYY-TO-YYYYMM.
           MOVE DATE-YYYYMM TO BY-DATE-CCYYMM HOLD-BY-DATE-CCYYMM.

       ENTER-BRANCH.
           MOVE "EN N040BRANCH." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "1<"
              PERFORM SCAN-MESS
              GO TO ENTER-BRANCH.

           IF VDUSTATUS = "1>"
              MOVE "END" TO IO-TYPE
              GO TO END-REC-BY-MODULE.

           IF VDUSTATUS = "1U"
              GO TO ENTER-DATE.

           IF VDUSTATUS NOT = "00"
              GO TO ENTER-BRANCH.

           IF VDUNUM0 = 0
              GO TO REC-BY-MODULE-NO.

           MOVE VDUNUM0 TO BR-NO BR-NO
                           BY-BRNO
                           HOLD-BY-BRNO.

           PERFORM LOAD-BRANCH-MODULE.

           IF ( BR-PATH = SPACES )
              MOVE "INVALID BRANCH" TO MESS
              PERFORM SEND-MESS
              GO TO REC-BY-MODULE-NO.

           MOVE BR-NAME TO HOLD-BRNAME.

           IF NOT PAGE1
              MOVE "S  A000BRX." TO VDU
              MOVE BR-NAME TO VDUBUF
              PERFORM SCREENIO.

       GET-CLASS.
           MOVE "EN N030LC." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "1U"
              GO TO ENTER-BRANCH.

           IF ( VDUSTATUS = "1<" )
              PERFORM ENTER-SCANKEY
              GO TO END-REC-BY-MODULE.

           IF VDUSTATUS = "1>"
             MOVE "END" TO IO-TYPE
             GO TO END-REC-BY-MODULE.

           IF VDUSTATUS NOT = "00"
             GO TO GET-CLASS.

           IF VDUNUM0 = 0
             GO TO GET-CLASS.

           MOVE VDUNUM0 TO BY-CLASS
                           HOLD-BY-CLASS
                           LC-CODE.

           PERFORM OPEN-LC1-FILE.
           PERFORM READ-LC1-FILE.
           PERFORM CLOSE-LC1-FILE.

           IF IO-FG NOT = 0
              MOVE "INVALID CLASS" TO MESS
              PERFORM SEND-MESS
              GO TO GET-CLASS.

           MOVE LC-DESC TO HOLD-LCDESC.

           IF NOT PAGE1
              MOVE "S  A020LCX." TO VDU
              MOVE LC-DESC TO VDUBUF
              PERFORM SCREENIO.

           PERFORM OPEN-BY1-FILE.
           PERFORM READ-BY1-FILE.
           PERFORM CLOSE-BY1-FILE.
           IF IO-FG = 0
              MOVE BY-REC TO ORIGINAL-BY-REC
              PERFORM DISPLAY-PAGE
            ELSE
              IF PAGE1
                 PERFORM NEW-RECORD-SETUP
              ELSE
                 GO TO REC-BY-MODULE-NO.

       END-REC-BY-MODULE.
           EXIT.

      ******************************************************************
      *    E N T R Y - M O D U L E
      *
      *    NEW-FLAG = 0 - MODIFY EXISTING RECORD
      *               1 - INITIAL NEW RECORD ENTRY
      *               2 - MODIFY NEW RECORD
      *
      ******************************************************************
       ENTRY-MODULE SECTION.
       BEGIN-ENTRY.
           MOVE " " TO IO-TYPE.
           MOVE 1 TO ONE.
           IF PAGE1  MOVE MAX1 TO MAX
           ELSE IF PAGE2  MOVE MAX2 TO MAX
           ELSE IF PAGE3  MOVE MAX3 TO MAX
           ELSE IF PAGE4  MOVE MAX4 TO MAX
           ELSE IF PAGE5  MOVE MAX5 TO MAX
           ELSE IF PAGE6  MOVE MAX6 TO MAX
           ELSE IF PAGE7  MOVE MAX7 TO MAX
           ELSE IF PAGE8  MOVE MAX8 TO MAX
           ELSE IF PAGE9  MOVE MAX9 TO MAX
           ELSE IF PAGE10 MOVE MAX10 TO MAX
           ELSE IF PAGE11 MOVE MAX11 TO MAX
           ELSE IF PAGE12 MOVE MAX12 TO MAX
           ELSE IF PAGE13 MOVE MAX13 TO MAX
           ELSE MOVE MAX14 TO MAX.

      *-----------------------------------------------------------------
       CREATE-ROUTINE.
           IF PAGE1 AND NEW-FLAG NOT = 1
              OR (PAGE2 AND NEW-FLAG-SCR2 NOT = 1)
              OR (PAGE3 AND NEW-FLAG-SCR3 NOT = 1)
              OR (PAGE4 AND NEW-FLAG-SCR4 NOT = 1)
              OR (PAGE5 AND NEW-FLAG-SCR5 NOT = 1)
              OR (PAGE6 AND NEW-FLAG-SCR6 NOT = 1)
              OR (PAGE7 AND NEW-FLAG-SCR7 NOT = 1)
              OR (PAGE8 AND NEW-FLAG-SCR8 NOT = 1)
              OR (PAGE9 AND NEW-FLAG-SCR9 NOT = 1)
              OR (PAGE10 AND NEW-FLAG-SCR10 NOT = 1)
              OR (PAGE11 AND NEW-FLAG-SCR11 NOT = 1)
              OR (PAGE12 AND NEW-FLAG-SCR12 NOT = 1)
              OR (PAGE13 AND NEW-FLAG-SCR13 NOT = 1)
              OR (PAGE14 AND NEW-FLAG-SCR14 NOT = 1)
                 GO TO MODIFY-ROUTINE.

           ADD ONE TO ELEMENT.

           IF ELEMENT < 1
              MOVE 1 TO ELEMENT ONE.
           IF ELEMENT > MAX AND PAGE2
               MOVE 2 TO NEW-FLAG-SCR2.
           IF ELEMENT > MAX AND PAGE3
               MOVE 2 TO NEW-FLAG-SCR3.
           IF ELEMENT > MAX AND PAGE4
               MOVE 2 TO NEW-FLAG-SCR4.
           IF ELEMENT > MAX AND PAGE5
               MOVE 2 TO NEW-FLAG-SCR5.
           IF ELEMENT > MAX AND PAGE6
               MOVE 2 TO NEW-FLAG-SCR6.
           IF ELEMENT > MAX AND PAGE7
               MOVE 2 TO NEW-FLAG-SCR7.
           IF ELEMENT > MAX AND PAGE8
               MOVE 2 TO NEW-FLAG-SCR8.
           IF ELEMENT > MAX AND PAGE9
               MOVE 2 TO NEW-FLAG-SCR9.
           IF ELEMENT > MAX AND PAGE10
               MOVE 2 TO NEW-FLAG-SCR10.
           IF ELEMENT > MAX AND PAGE11
               MOVE 2 TO NEW-FLAG-SCR11.
           IF ELEMENT > MAX AND PAGE12
               MOVE 2 TO NEW-FLAG-SCR12.
           IF ELEMENT > MAX AND PAGE13
               MOVE 2 TO NEW-FLAG-SCR13.
           IF ELEMENT > MAX AND PAGE14
               MOVE 2 TO NEW-FLAG-SCR14.
           IF ELEMENT > MAX
               MOVE 2 TO NEW-FLAG
               GO TO MODIFY-ROUTINE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF PAGE1
               MOVE ELEMENT TO ELE1
               GO TO ENTER-ELE1
           ELSE IF PAGE2
                   MOVE ELEMENT TO ELE2
                   GO TO ENTER-ELE2
           ELSE IF PAGE3
                   MOVE ELEMENT TO ELE3
                   GO TO ENTER-ELE3
           ELSE IF PAGE4
                   MOVE ELEMENT TO ELE4
                   GO TO ENTER-ELE4
           ELSE IF PAGE5
                   MOVE ELEMENT TO ELE5
                   GO TO ENTER-ELE5
           ELSE IF PAGE6
                   MOVE ELEMENT TO ELE6
                   GO TO ENTER-ELE6
           ELSE IF PAGE7
                   MOVE ELEMENT TO ELE7
                   GO TO ENTER-ELE7
           ELSE IF PAGE8
                   MOVE ELEMENT TO ELE8
                   GO TO ENTER-ELE8
           ELSE IF PAGE9
                   MOVE ELEMENT TO ELE9
                   GO TO ENTER-ELE9
           ELSE  IF PAGE10
                   MOVE ELEMENT TO ELE10
                   GO TO ENTER-ELE10
           ELSE  IF PAGE11
                   MOVE ELEMENT TO ELE11
                   GO TO ENTER-ELE11
           ELSE  IF PAGE12
                   MOVE ELEMENT TO ELE12
                   GO TO ENTER-ELE12
           ELSE  IF PAGE13
                   MOVE ELEMENT TO ELE13
                   GO TO ENTER-ELE13
           ELSE  IF PAGE14
                   MOVE ELEMENT TO ELE14
                   GO TO ENTER-ELE14.

       MODIFY-ROUTINE.
           MOVE "R RA060SEL." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.

      * IF ON PAGE 13, JUST RE-DISPLAY SCREEN, PERIOD WILL CHANGE THERE
           IF ( F4-KEY )
              IF ( PAGE13 )
                 PERFORM DISPLAY-PAGE
                 GO TO MODIFY-ROUTINE.

           IF VDUSTATUS = "18" OR "1:"
              IF PAGE2 OR PAGE6  OR PAGE8  OR PAGE9 OR PAGE10
                       OR PAGE11 OR PAGE12 OR PAGE14
                 PERFORM NEW-ACTIVITY
                 GO TO MODIFY-ROUTINE.

           IF ( PAGE3                                          ) AND
              ( F2-KEY OR F3-KEY OR F4-KEY OR F5-KEY OR F6-KEY )
              PERFORM TOGGLE-ON-DELINQ
              GO TO MODIFY-ROUTINE.

           IF VDUSTATUS NOT = "00"
              GO TO MODIFY-ROUTINE.

           MOVE VDUBUF TO ACTION.

           IF ACTION = " "
              MOVE "RWR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.

           IF PAGE1
              IF ACTION = "DELETE"
                 MOVE "DEL" TO IO-TYPE
                 GO TO END-ENTRY-MODULE.

      * FORM-NUM1 CHECKS FOR PAGES 1 THRU 9
           IF FORM-KEY1 = " " AND FORM-P1 = "P" AND FORM-SEL1 NUMERIC
              IF NOT FORM-1-DIGIT OR FORM-SEL1 = PAGE-FLAG
                 GO TO MODIFY-ROUTINE
              ELSE
                 GO TO CHECK-PAGE.
      * FORM-NUM2 CHECKS FOR PAGE 10+
           IF FORM-KEY2 = " " AND FORM-P2 = "P" AND FORM-SEL2 NUMERIC
              IF NOT FORM-2-DIGITS
                 GO TO MODIFY-ROUTINE
              ELSE
                 GO TO CHECK-PAGE.
           GO TO CHK-CHNO.
       CHECK-PAGE.
           PERFORM SWITCH-FORM
           GO TO BEGIN-ENTRY.
       CHK-CHNO.
           INSPECT ACTION REPLACING ALL SPACES
              BY ZEROS.
           IF CHNO-X NOT NUMERIC
              GO TO MODIFY-ROUTINE.
           IF CHNO = 0
              GO TO MODIFY-ROUTINE.
           IF CHNO > MAX
              GO TO MODIFY-ROUTINE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF PAGE1
              MOVE CHNO TO ELE1
              GO TO ENTER-ELE1
           ELSE IF PAGE2
                   MOVE CHNO TO ELE2
                   GO TO ENTER-ELE2
           ELSE IF PAGE3
                   MOVE CHNO TO ELE3
                   GO TO ENTER-ELE3
           ELSE IF PAGE4
                   MOVE CHNO TO ELE4
                   GO TO ENTER-ELE4
           ELSE IF PAGE5
                   MOVE CHNO TO ELE5
                   GO TO ENTER-ELE5
           ELSE IF PAGE6
                   MOVE CHNO TO ELE6
                   GO TO ENTER-ELE6
           ELSE IF PAGE7
                   MOVE CHNO TO ELE7
                   GO TO ENTER-ELE7
           ELSE IF PAGE8
                   MOVE CHNO TO ELE8
                   GO TO ENTER-ELE8
           ELSE IF PAGE9
                   MOVE CHNO TO ELE9
                   GO TO ENTER-ELE9
           ELSE IF PAGE10
                   MOVE CHNO TO ELE10
                   GO TO ENTER-ELE10
           ELSE IF PAGE11
                   MOVE CHNO TO ELE11
                   GO TO ENTER-ELE11
           ELSE IF PAGE12
                   MOVE CHNO TO ELE12
                   GO TO ENTER-ELE12
           ELSE IF PAGE13
                   MOVE CHNO TO ELE13
                   GO TO ENTER-ELE13
           ELSE IF PAGE14
                   MOVE CHNO TO ELE14
                   GO TO ENTER-ELE14.

      *-----------------------------------------------------------------
       ENTER-ELE1.
           IF CHKEY NOT = ZEROS
              IF CHFG1(ELE1) = " "
                 IF NEW-FLAG NOT = 1
                    GO TO MODIFY-ROUTINE.

           IF CHFG1(ELE1) = "N"
              IF CHKEY NOT = EXT-CONAME-DAILY-PASSWORD
                 GO TO MODIFY-ROUTINE.

           MOVE SPEC1(ELE1) TO VDU.
           MOVE ELE1 TO VDUROW.
           GO TO ACTUAL-READ.

      *-----------------------------------------------------------------
       ENTER-ELE2.
           IF CHKEY NOT = ZEROS
              IF CHFG2(ELE2) = " "
                 IF NEW-FLAG-SCR2 NOT = 1
                    GO TO MODIFY-ROUTINE.

           IF CHFG2(ELE2) = "N"
              IF CHKEY NOT = EXT-CONAME-DAILY-PASSWORD
                 GO TO MODIFY-ROUTINE.

           MOVE SPEC2(ELE2) TO VDU.
           MOVE ELE2 TO VDUROW.
           GO TO ACTUAL-READ.

      *-----------------------------------------------------------------
       ENTER-ELE3.
           IF CHKEY NOT = ZEROS
              IF CHFG3(ELE3) = " "
                 IF NEW-FLAG-SCR3 NOT = 1
                    GO TO MODIFY-ROUTINE.

           IF CHFG3(ELE3) = "N"
              IF CHKEY NOT = EXT-CONAME-DAILY-PASSWORD
                 GO TO MODIFY-ROUTINE.

           MOVE SPEC3(ELE3) TO VDU.
           MOVE ELE3 TO VDUROW.

           IF ELE3 = 8
              GO TO GOTO-FIELD3.

           GO TO ACTUAL-READ.

      *-----------------------------------------------------------------
       ENTER-ELE4.
           IF CHKEY NOT = ZEROS
              IF CHFG4(ELE4) = " "
                 IF NEW-FLAG-SCR4 NOT = 1
                    GO TO MODIFY-ROUTINE.

           IF CHFG4(ELE4) = "N"
              IF CHKEY NOT = EXT-CONAME-DAILY-PASSWORD
                 GO TO MODIFY-ROUTINE.

           MOVE SPEC4(ELE4) TO VDU.
           MOVE ELE4 TO VDUROW.
           GO TO ACTUAL-READ.

      *-----------------------------------------------------------------
       ENTER-ELE5.
           IF CHKEY NOT = ZEROS
              IF CHFG5(ELE5) = " "
                 IF NEW-FLAG-SCR5 NOT = 1
                    GO TO MODIFY-ROUTINE.

           IF CHFG5(ELE5) = "N"
              IF CHKEY NOT = EXT-CONAME-DAILY-PASSWORD
                 GO TO MODIFY-ROUTINE.

           MOVE SPEC5(ELE5) TO VDU.
           MOVE ELE5 TO VDUROW.
           GO TO ACTUAL-READ.

      *-----------------------------------------------------------------
       ENTER-ELE6.
           IF CHKEY NOT = ZEROS
              IF CHFG6(ELE6) = " "
                 IF NEW-FLAG-SCR6 NOT = 1
                    GO TO MODIFY-ROUTINE.

           IF CHFG6(ELE6) = "N"
              IF CHKEY NOT = EXT-CONAME-DAILY-PASSWORD
                 GO TO MODIFY-ROUTINE.

           MOVE SPEC6(ELE6) TO VDU.
           MOVE ELE6 TO VDUROW.
           GO TO ACTUAL-READ.

      *-----------------------------------------------------------------
       ENTER-ELE7.
           IF CHKEY NOT = ZEROS
              IF CHFG7(ELE7) = " "
                 IF NEW-FLAG NOT = 1
                    GO TO MODIFY-ROUTINE.

           IF CHFG7(ELE7) = "N"
              IF CHKEY NOT = EXT-CONAME-DAILY-PASSWORD
                 GO TO MODIFY-ROUTINE.

           MOVE SPEC7(ELE7) TO VDU.
           MOVE ELE7 TO VDUROW.
           GO TO ACTUAL-READ.

      *-----------------------------------------------------------------
       ENTER-ELE8.
           IF CHKEY NOT = ZEROS
              IF CHFG8(ELE8) = " "
                 IF NEW-FLAG-SCR8 NOT = 1
                    GO TO MODIFY-ROUTINE.

           IF CHFG8(ELE8) = "N"
              IF CHKEY NOT = EXT-CONAME-DAILY-PASSWORD
                 GO TO MODIFY-ROUTINE.

           MOVE SPEC8(ELE8) TO VDU.
           MOVE ELE8 TO VDUROW.
           GO TO ACTUAL-READ.

      *-----------------------------------------------------------------
       ENTER-ELE9.
           IF CHKEY NOT = ZEROS
              IF CHFG9(ELE9) = " "
                 IF NEW-FLAG-SCR9 NOT = 1
                    GO TO MODIFY-ROUTINE.

           IF CHFG9(ELE9) = "N"
              IF CHKEY NOT = EXT-CONAME-DAILY-PASSWORD
                 GO TO MODIFY-ROUTINE.

           MOVE SPEC9(ELE9) TO VDU.
           MOVE ELE9 TO VDUROW.
           GO TO ACTUAL-READ.

      *-----------------------------------------------------------------
       ENTER-ELE10.
           IF CHKEY NOT = ZEROS
              IF CHFG10(ELE10) = " "
                 IF NEW-FLAG-SCR10 NOT = 1
                    GO TO MODIFY-ROUTINE.

           IF CHFG10(ELE10) = "N"
              IF CHKEY NOT = EXT-CONAME-DAILY-PASSWORD
                 GO TO MODIFY-ROUTINE.

           MOVE SPEC10(ELE10) TO VDU.
           MOVE ELE10 TO VDUROW.
           GO TO ACTUAL-READ.

      *-----------------------------------------------------------------
       ENTER-ELE11.
           IF CHKEY NOT = ZEROS
              IF CHFG11(ELE11) = " "
                 IF NEW-FLAG-SCR11 NOT = 1
                    GO TO MODIFY-ROUTINE.

           IF CHFG11(ELE11) = "N"
              IF CHKEY NOT = EXT-CONAME-DAILY-PASSWORD
                 GO TO MODIFY-ROUTINE.

           MOVE SPEC11(ELE11) TO VDU.
           MOVE ELE11 TO VDUROW.
           GO TO ACTUAL-READ.
      *-----------------------------------------------------------------
       ENTER-ELE12.

      * ONLY ALLOW MODIFICATION OF VOLUME.
           IF ELE12 = 8
              IF ACTIVITY NOT = 2
                 GO TO MODIFY-ROUTINE.

           IF CHKEY NOT = ZEROS
              IF CHFG12(ELE12) = " "
                 IF NEW-FLAG-SCR12 NOT = 1
                    GO TO MODIFY-ROUTINE.

           IF CHFG12(ELE12) = "N"
              IF CHKEY NOT = EXT-CONAME-DAILY-PASSWORD
                 GO TO MODIFY-ROUTINE.

           MOVE SPEC12(ELE12) TO VDU.
           MOVE ELE12 TO VDUROW.
           GO TO ACTUAL-READ.

      *-----------------------------------------------------------------
       ENTER-ELE13.
           IF CHKEY NOT = ZEROS
              IF CHFG13(ELE13) = " "
                 IF NEW-FLAG-SCR13 NOT = 1
                    GO TO MODIFY-ROUTINE.

           IF CHFG13(ELE13) = "N"
              IF CHKEY NOT = EXT-CONAME-DAILY-PASSWORD
                 GO TO MODIFY-ROUTINE.

           MOVE SPEC13(ELE13) TO VDU.
           MOVE ELE13         TO VDUROW.
           GO TO ACTUAL-READ.

      *-----------------------------------------------------------------
       ENTER-ELE14.
           IF CHKEY NOT = ZEROS
              IF CHFG14(ELE14) = " "
                 IF NEW-FLAG-SCR14 NOT = 1
                    GO TO MODIFY-ROUTINE.

           IF CHFG14(ELE14) = "N"
              IF CHKEY NOT = EXT-CONAME-DAILY-PASSWORD
                 GO TO MODIFY-ROUTINE.

           MOVE SPEC14(ELE14) TO VDU.
           MOVE ELE14         TO VDUROW.
           GO TO ACTUAL-READ.
      *-----------------------------------------------------------------
       ACTUAL-READ.
           MOVE 0 TO VDUCOL.
           MOVE "." TO VDUDECIMAL.
           PERFORM SCREENIO.

      *-----------------------------------------------------------------
       TEST-UP.
           IF VDUSTATUS = "1U"
              IF (PAGE1 AND NEW-FLAG = 1)
                  OR (PAGE2 AND NEW-FLAG-SCR2 = 1)
                  OR (PAGE3 AND NEW-FLAG-SCR3 = 1)
                  OR (PAGE4 AND NEW-FLAG-SCR4 = 1)
                  OR (PAGE5 AND NEW-FLAG-SCR5 = 1)
                  OR (PAGE6 AND NEW-FLAG-SCR6 = 1)
                  OR (PAGE7 AND NEW-FLAG-SCR7 = 1)
                  OR (PAGE8 AND NEW-FLAG-SCR8 = 1)
                  OR (PAGE9 AND NEW-FLAG-SCR9 = 1)
                  OR (PAGE10 AND NEW-FLAG-SCR10 = 1)
                  OR (PAGE11 AND NEW-FLAG-SCR11 = 1)
                  OR (PAGE12 AND NEW-FLAG-SCR12 = 1)
                  OR (PAGE13 AND NEW-FLAG-SCR13 = 1)
                  OR (PAGE14 AND NEW-FLAG-SCR14 = 1)
                 MOVE -1 TO ONE
                 GO TO CREATE-ROUTINE
              ELSE
                 GO TO CREATE-ROUTINE
           ELSE
              NEXT SENTENCE.

      *-----------------------------------------------------------------
       TEST-DOWN.
           IF VDUSTATUS = "1D"
              MOVE 1 TO ONE
              GO TO GOTO-FIELD-123456-OR-7.

      *-----------------------------------------------------------------
       TEST-F1.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.
           IF VDUSTATUS NOT = "00"
              GO TO ENTER-ELE1.
           MOVE 1 TO ONE.

      *-----------------------------------------------------------------
       GOTO-FIELD-123456-OR-7.
           IF PAGE2  GO TO GOTO-FIELD2.
           IF PAGE3  GO TO GOTO-FIELD3.
           IF PAGE4  GO TO GOTO-FIELD4.
           IF PAGE5  GO TO GOTO-FIELD5.
           IF PAGE6  GO TO GOTO-FIELD6.
           IF PAGE7  GO TO GOTO-FIELD7.
           IF PAGE8  GO TO GOTO-FIELD8.
           IF PAGE9  GO TO GOTO-FIELD9.
           IF PAGE10  GO TO GOTO-FIELD10.
           IF PAGE11  GO TO GOTO-FIELD11.
           IF PAGE12  GO TO GOTO-FIELD12.
           IF PAGE13  GO TO GOTO-FIELD13.
           IF PAGE14  GO TO GOTO-FIELD14.

      *-----------------------------------------------------------------
       GOTO-FIELD1.
           GO TO F-01 F-02 F-03 F-04 F-05
                 F-06 F-07 F-08 F-09 F-10
                 F-11 F-12 F-13 F-14 F-15
                 F-16 F-17 F-18 F-19 F-20
                 F-21 F-22 F-23 F-24
               DEPENDING ON ELE1.

           GO TO END-ENTRY-MODULE.

      *-----------------------------------------------------------------
       GOTO-FIELD2.
           MOVE ACTIVITY TO SUB2.

           GO TO G-01 G-01 G-01 G-01 G-01 G-01
                 G-01 G-01 G-01 G-01 G-01 G-12
                 G-13 G-14 G-15 DEPENDING ON ELE2.

           GO TO END-ENTRY-MODULE.

      *-----------------------------------------------------------------
       GOTO-FIELD3.
           MOVE DELINQ TO SUB3.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      * NO MISS PAYS ON RECENCY
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

           IF ELE3 = 8
              GO TO H-013.

           GO TO H-01 H-01 H-01 H-01 H-01 H-01 H-01 H-01 H-01 H-01
                 DEPENDING ON ELE3.

           GO TO END-ENTRY-MODULE.

      *-----------------------------------------------------------------
       GOTO-FIELD4.
           GO TO I-01 I-02 I-03 I-04 I-05 I-06 I-07 I-08 I-09 I-10
                 I-11 I-12 I-13 I-14 I-15 I-16 I-17 I-18 I-19 I-20
                 I-21 I-22 I-23 I-24 I-25 I-26 I-27 I-28 I-29 I-30
                 I-31 I-32
                 DEPENDING ON ELE4.

           GO TO END-ENTRY-MODULE.

      *-----------------------------------------------------------------
       GOTO-FIELD5.
           MOVE PERIOD TO SUB1.
           GO TO J-01 J-01 J-01 J-01 J-01
                 J-01 J-01 J-01 J-01 J-01
                 J-01 J-01 J-01
                   DEPENDING ON ELE5.
           GO TO END-ENTRY-MODULE.
      *-----------------------------------------------------------------
       GOTO-FIELD6.
           MOVE ACTIVITY TO SUB2.

           GO TO K-01 K-01 K-01 K-01 K-01
                 K-02 K-02 K-02 K-02 K-02
                 DEPENDING ON ELE6.

           GO TO END-ENTRY-MODULE.

      *-----------------------------------------------------------------
       GOTO-FIELD7.
           GO TO L-01 L-02 L-03 L-04 L-05
                 L-06 L-07 L-08 L-09 L-10
                 L-11 L-12 L-13 L-14 L-15
                 L-16 L-17 L-18 L-19 L-20
                 L-21 L-22 L-23 L-24 L-25
                 L-26 L-27 L-28 L-29 L-30
                 L-31 L-32 L-33 L-34
               DEPENDING ON ELE7.

           GO TO END-ENTRY-MODULE.

      *-----------------------------------------------------------------
       GOTO-FIELD8.
           MOVE ACTIVITY TO SUB2.

           GO TO M-01 M-01 M-01 M-01 M-01 M-01
                 M-01 M-01 M-01 M-01 M-01
                 DEPENDING ON ELE8.

           GO TO END-ENTRY-MODULE.

      *-----------------------------------------------------------------
       GOTO-FIELD9.
           MOVE ACTIVITY TO SUB2.

           GO TO N-01 N-01 N-01 N-01 N-01 N-01 N-01 N-01 N-01 N-01 N-11
                 DEPENDING ON ELE9.

           GO TO END-ENTRY-MODULE.

      *-----------------------------------------------------------------
       GOTO-FIELD10.
           MOVE ACTIVITY TO SUB2.

           GO TO P-01 P-01 P-01 P-01 P-01 P-01
                 P-01 P-01 P-01 P-01
                 DEPENDING ON ELE10.

           GO TO END-ENTRY-MODULE.

      *-----------------------------------------------------------------
       GOTO-FIELD11.

           MOVE ACTIVITY TO SUB11.

           GO TO Q-01 Q-02 Q-03 Q-04 Q-05
                 Q-06 Q-07 Q-08 Q-09 Q-10
                 DEPENDING ON ELE11.

           GO TO END-ENTRY-MODULE.

      *-----------------------------------------------------------------
       GOTO-FIELD12.

           MOVE ACTIVITY TO SUB2.

           GO TO R-01 R-01 R-01 R-04 R-04 R-04 R-07 R-08
                 DEPENDING ON ELE12.

           GO TO END-ENTRY-MODULE.

      *-----------------------------------------------------------------
       GOTO-FIELD13.

           MOVE PERIOD TO SUB1.

           GO TO S-01
                 DEPENDING ON ELE13.

           GO TO END-ENTRY-MODULE.

      *-----------------------------------------------------------------
       GOTO-FIELD14.
           MOVE ACTIVITY TO SUB2.
           MOVE ELE14 TO TYPEX.
           GO TO T-01 T-01 T-01 T-01 T-01 T-01
                 T-01 T-01 T-01 T-01 T-01 T-01
                   DEPENDING ON ELE14.
           GO TO END-ENTRY-MODULE.

      *-----------------------------------------------------------------
      *
      *    SET RECORD ELEMENTS ----- PAGE 1
      *
      *-----------------------------------------------------------------
       F-01.
           MOVE VDUNUM2 TO BY-OTHCOL(1).
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-02.
           MOVE VDUNUM2 TO BY-OTHCOL(2).
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-03.
           MOVE VDUNUM2 TO BY-OT2COL(1).
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-04.
           MOVE VDUNUM2 TO BY-OT2COL(2).
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-05.
           MOVE VDUNUM2 TO BY-INTCOL(1).
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-06.
           MOVE VDUNUM2 TO BY-INTCOL(2).
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-07.
           MOVE VDUNUM2 TO BY-LATECOL(1).
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-08.
           MOVE VDUNUM2 TO BY-LATECOL(2).
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-09.
           MOVE VDUNUM2 TO BY-PRINCOL(1).
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-10.
           MOVE VDUNUM2 TO BY-PRINCOL(2).
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-11.
           MOVE VDUNUM2 TO BY-DEFCOL(1).
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-12.
           MOVE VDUNUM2 TO BY-DEFCOL(2).
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-13.
           MOVE VDUNUM2 TO BY-RNPRIN(1).
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-14.
           MOVE VDUNUM2 TO BY-RNPRIN(2).
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-15.
           MOVE VDUNUM2 TO BY-PART-CHGOFF(1).
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-16.
           MOVE VDUNUM2 TO BY-PART-CHGOFF(2).
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-17.
           MOVE VDUNUM0 TO BY-NO-SOLD (1).

      *-----------------------------------------------------------------
       F-171.
           MOVE "E NS082  1." TO VDU.
           MOVE ELE1 TO VDUROW.
           PERFORM SCREENIO.

           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE1.

           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO F-171.

           MOVE VDUNUM2 TO BY-AMT-SOLD(1).
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-18.
           MOVE VDUNUM0 TO BY-NO-SOLD (2).

      *-----------------------------------------------------------------
       F-181.
           MOVE "E NS082  1." TO VDU.
           MOVE ELE1 TO VDUROW.
           PERFORM SCREENIO.

           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE1.

           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO F-181.

           MOVE VDUNUM2 TO BY-AMT-SOLD(2).
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-19.
           MOVE VDUNUM0 TO BY-NO-TRANS (1).

      *-----------------------------------------------------------------
       F-191.
           MOVE "E NS082  1." TO VDU.
           MOVE ELE1 TO VDUROW.
           PERFORM SCREENIO.

           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE1.

           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO F-191.

           MOVE VDUNUM2 TO BY-AMT-TRANS(1).
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-20.
           MOVE VDUNUM0 TO BY-NO-TRANS (2).

      *-----------------------------------------------------------------
       F-201.
           MOVE "E NS082  1." TO VDU.
           MOVE ELE1 TO VDUROW.
           PERFORM SCREENIO.

           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE1.

           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO F-201.

           MOVE VDUNUM2 TO BY-AMT-TRANS(2).
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-21.
           MOVE VDUNUM0 TO BY-NO-PURCH (1).

      *-----------------------------------------------------------------
       F-211.
           MOVE "E NS082  1." TO VDU.
           MOVE ELE1 TO VDUROW.
           PERFORM SCREENIO.

           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE1.

           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO F-211.

           MOVE VDUNUM2 TO BY-AMT-PURCH(1).
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       F-22.
           MOVE VDUNUM0 TO BY-NO-PURCH (2).

      *-----------------------------------------------------------------
       F-221.
           MOVE "E NS082  1." TO VDU.
           MOVE ELE1 TO VDUROW.
           PERFORM SCREENIO.

           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE1.

           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO F-221.

           MOVE VDUNUM2 TO BY-AMT-PURCH(2).
           GO TO CREATE-ROUTINE.
       F-23.
           MOVE VDUNUM2 TO BY-PROCEEDS(1).
           GO TO CREATE-ROUTINE.
       F-24.
           MOVE VDUNUM2 TO BY-PROCEEDS(2).
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
      *
      *    SET RECORD ELEMENTS ----- PAGE 2
      *
      *-----------------------------------------------------------------
       G-01.
           MOVE VDUNUM0 TO BY-NO(1 SUB2 ELE2).

      *-----------------------------------------------------------------
       G-011.
           MOVE "ENNS050  1." TO VDU.
           MOVE ELE2 TO VDUROW.
           PERFORM SCREENIO.

           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE2.

           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO G-011.

           MOVE VDUNUM0 TO BY-NO(2 SUB2 ELE2).

      *-----------------------------------------------------------------
       G-012.
           MOVE "ENNS050  2." TO VDU.
           MOVE ELE2 TO VDUROW.
           PERFORM SCREENIO.

           IF VDUSTATUS = "10"
              GO TO G-011.

           IF VDUSTATUS = "1U"
              GO TO G-011.

           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO G-012.

           MOVE VDUNUM0 TO BY-NO(3 SUB2 ELE2).

      *-----------------------------------------------------------------
       G-013.
           MOVE "ENNS082  3." TO VDU.
           MOVE ELE2 TO VDUROW.
           PERFORM SCREENIO.

           IF VDUSTATUS = "10"
              GO TO G-011.

           IF VDUSTATUS = "1U"
              GO TO G-012.

           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO G-013.

           MOVE VDUNUM2 TO BY-AMT(1 SUB2 ELE2).

      *-----------------------------------------------------------------
       G-014.
           MOVE "ENNS082  4." TO VDU.
           MOVE ELE2 TO VDUROW.
           PERFORM SCREENIO.

           IF VDUSTATUS = "10"
              GO TO G-011.

           IF VDUSTATUS = "1U"
              GO TO G-013.

           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO G-014.

           MOVE VDUNUM2 TO BY-AMT(2 SUB2 ELE2).

      *-----------------------------------------------------------------
       G-015.
           MOVE "ENNS082  5." TO VDU.
           MOVE ELE2 TO VDUROW.
           PERFORM SCREENIO.

           IF VDUSTATUS = "10"
              GO TO G-011.

           IF VDUSTATUS = "1U"
              GO TO G-014.

           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO G-015.

           MOVE VDUNUM2 TO BY-AMT(3 SUB2 ELE2).
           GO TO CREATE-ROUTINE.

      *----------------------------------------------------------------
       G-12.
           MOVE VDUNUM0 TO BY-SUMWGTTM.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       G-13.
           MOVE VDUNUM2 TO BY-SUMWGTAV.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       G-14.
           MOVE VDUNUM2 TO BY-SUMWGTTA.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       G-15.
           MOVE VDUNUM2 TO BY-IB-ACCRUED.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
      *
      *    SET RECORD ELEMENTS ----- PAGE 3
      *
      *-----------------------------------------------------------------
       H-01.
           IF ( HEADER-LEV-STD )
              MOVE VDUNUM0 TO BY-UNITS       (DELINQ 1 ELE3)
           ELSE
              MOVE VDUNUM0 TO BY-UNITS-BNKRPT(DELINQ 1 ELE3).

      *-----------------------------------------------------------------
       H-011.
           MOVE "ENNS092011." TO VDU.
           MOVE ELE3 TO VDUROW.
           PERFORM SCREENIO.

           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE3.

           IF VDUSTATUS = "10"
              GO TO ENTER-ELE3.

           IF VDUSTATUS NOT = "00"
              GO TO H-011.

           IF ( HEADER-LEV-STD )
              MOVE VDUNUM2 TO BY-OUTBAL       (DELINQ 1 ELE3)
           ELSE
              MOVE VDUNUM2 TO BY-OUTBAL-BNKRPT(DELINQ 1 ELE3).

      *-----------------------------------------------------------------
       H-012.
           MOVE "ENNS060012." TO VDU.
           MOVE ELE3 TO VDUROW.
           PERFORM SCREENIO.

           IF VDUSTATUS = "1U"
              IF ELE3 = 8
                 GO TO H-012
              ELSE
                 GO TO H-011.

           IF VDUSTATUS = "10"
              GO TO ENTER-ELE3.

           IF VDUSTATUS NOT = "00"
              GO TO H-012.

           IF ( HEADER-LEV-STD )
              MOVE VDUNUM0 TO BY-UNITS       (DELINQ 2 ELE3)
           ELSE
              MOVE VDUNUM0 TO BY-UNITS-BNKRPT(DELINQ 2 ELE3).

      *-----------------------------------------------------------------
       H-013.
           MOVE "ENNS092013." TO VDU.
           MOVE ELE3 TO VDUROW.
           PERFORM SCREENIO.

           IF VDUSTATUS = "1U"
              GO TO H-012.

           IF VDUSTATUS = "10"
              GO TO ENTER-ELE3.

           IF VDUSTATUS NOT = "00"
              GO TO H-013.

           IF ( HEADER-LEV-STD )
              MOVE VDUNUM2 TO BY-OUTBAL       (DELINQ 2 ELE3)
           ELSE
              MOVE VDUNUM2 TO BY-OUTBAL-BNKRPT(DELINQ 2 ELE3).

           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
      *
      *    SET RECORD ELEMENTS ----- PAGE 4
      *
      *-----------------------------------------------------------------
       I-01.
           MOVE VDUNUM2 TO BY-ALLOTMENTS(1).
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       I-02.
           MOVE VDUNUM2 TO BY-ALLOTMENTS(2).
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       I-03.
           MOVE VDUNUM2 TO BY-LOCKBOX(1).
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       I-04.
           MOVE VDUNUM2 TO BY-LOCKBOX(2).
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       I-05.
      *- - - - - - - - - - - - - - - - - - - - - - - - - AVAILABLE SPACE
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       I-06.
      *- - - - - - - - - - - - - - - - - - - - - - - - - AVAILABLE SPACE
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       I-07.
      *- - - - - - - - - - - - - - - - - - - - - - - - - AVAILABLE SPACE
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       I-08.
      *- - - - - - - - - - - - - - - - - - - - - - - - - AVAILABLE SPACE
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       I-09.
           MOVE VDUNUM0 TO BY-OTH-NO-COL(1).
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       I-10.
           MOVE VDUNUM0 TO BY-OTH-NO-COL(2).
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       I-11.
           MOVE VDUNUM0 TO BY-OT2-NO-COL(1).
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       I-12.
           MOVE VDUNUM0 TO BY-OT2-NO-COL(2).
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       I-13.
           MOVE VDUNUM0 TO BY-INT-NO-COL(1).
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       I-14.
           MOVE VDUNUM0 TO BY-INT-NO-COL(2).
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       I-15.
           MOVE VDUNUM0 TO BY-LATE-NO-COL(1).
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       I-16.
           MOVE VDUNUM0 TO BY-LATE-NO-COL(2).
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       I-17.
           MOVE VDUNUM0 TO BY-OTH-NO-WAIVED(1).
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       I-18.
           MOVE VDUNUM0 TO BY-OTH-NO-WAIVED(2).
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       I-19.
           MOVE VDUNUM0 TO BY-OT2-NO-WAIVED(1).
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       I-20.
           MOVE VDUNUM0 TO BY-OT2-NO-WAIVED(2).
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       I-21.
           MOVE VDUNUM0 TO BY-INT-NO-WAIVED(1).
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       I-22.
           MOVE VDUNUM0 TO BY-INT-NO-WAIVED(2).
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       I-23.
           MOVE VDUNUM0 TO BY-LATE-NO-WAIVED(1).
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       I-24.
           MOVE VDUNUM0 TO BY-LATE-NO-WAIVED(2).
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       I-25.
           MOVE VDUNUM2 TO BY-OTH-AMT-WAIVED(1).
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       I-26.
           MOVE VDUNUM2 TO BY-OTH-AMT-WAIVED(2).
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       I-27.
           MOVE VDUNUM2 TO BY-OT2-AMT-WAIVED(1).
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       I-28.
           MOVE VDUNUM2 TO BY-OT2-AMT-WAIVED(2).
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       I-29.
           MOVE VDUNUM2 TO BY-INT-AMT-WAIVED(1).
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       I-30.
           MOVE VDUNUM2 TO BY-INT-AMT-WAIVED(2).
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       I-31.
           MOVE VDUNUM2 TO BY-LATE-AMT-WAIVED(1).
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       I-32.
           MOVE VDUNUM2 TO BY-LATE-AMT-WAIVED(2).
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
      *    SET RECORD ELEMENTS FOR PAGE 5
      *    NOTE IF ELE5 IS FROM 1 - 10, MOVE TO INS TABLE 1-10
      *    IF ELE5 11-13, MOVE TO FEE TABLE 1 - 3 (ELE5 MINUS 10)
      *-----------------------------------------------------------------
       J-01.
           IF ELE5 < 11
              MOVE VDUNUM0 TO BY-INS-NO-ELIG(PERIOD ELE5)
           ELSE
              MOVE VDUNUM0 TO BY-FEE-NO-ELIG(PERIOD ELE5 - 10).
      *----------------------------------------------------------------
       J-011.
           MOVE "ENNS070  1." TO VDU.
           MOVE ELE5          TO VDUROW.
           PERFORM SCREENIO.
           IF ( VDUSTATUS = "1U" )
              GO TO ENTER-ELE5.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO J-011.
           IF ELE5 < 11
              MOVE VDUNUM0 TO BY-INS-NO-WROTE(1 ELE5)
           ELSE
              MOVE VDUNUM0 TO BY-FEE-NO-WROTE(1 ELE5 - 10).
      *----------------------------------------------------------------
       J-012.
           MOVE "ENNS070  2." TO VDU.
           MOVE ELE5          TO VDUROW.
           PERFORM SCREENIO.
           IF ( VDUSTATUS = "1U" )
              GO TO J-011.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO J-012.
           IF ELE5 < 11
              MOVE VDUNUM0 TO BY-INS-NO-NOTWROTE(1 ELE5)
           ELSE
              MOVE VDUNUM0 TO BY-FEE-NO-NOTWROTE(1 ELE5 - 10).
      *----------------------------------------------------------------
       J-013.
           MOVE "ENNS082  3." TO VDU.
           MOVE ELE5          TO VDUROW.
           PERFORM SCREENIO.
           IF ( VDUSTATUS = "1U" )
              GO TO J-012.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO J-013.
           IF ELE5 < 11
              MOVE VDUNUM2 TO BY-INS-AMT-WROTE(1 ELE5)
           ELSE
              MOVE VDUNUM2 TO BY-FEE-AMT-WROTE(1 ELE5 - 10).
      *----------------------------------------------------------------
       J-014.
           MOVE "ENNS070  4." TO VDU.
           MOVE ELE5          TO VDUROW.
           PERFORM SCREENIO.
           IF ( VDUSTATUS = "1U" )
              GO TO J-013.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO J-014.
           IF ELE5 < 11
              MOVE VDUNUM0 TO BY-INS-NO-ELIG(2 ELE5)
           ELSE
              MOVE VDUNUM0 TO BY-FEE-NO-ELIG(2 ELE5 - 10).
      *----------------------------------------------------------------
       J-015.
           MOVE "ENNS070  5." TO VDU.
           MOVE ELE5         TO VDUROW.
           PERFORM SCREENIO.
           IF ( VDUSTATUS = "1U" )
              GO TO J-014.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO J-015.
           IF ELE5 < 11
              MOVE VDUNUM0 TO BY-INS-NO-WROTE(2 ELE5)
           ELSE
              MOVE VDUNUM0 TO BY-FEE-NO-WROTE(2 ELE5 - 10).
      *----------------------------------------------------------------
       J-016.
           MOVE "ENNS070  6." TO VDU.
           MOVE ELE5         TO VDUROW.
           PERFORM SCREENIO.
           IF ( VDUSTATUS = "1U" )
              GO TO J-015.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO J-016.
           IF ELE5 < 11
              MOVE VDUNUM0 TO BY-INS-NO-NOTWROTE(2 ELE5)
           ELSE
              MOVE VDUNUM0 TO BY-FEE-NO-NOTWROTE(2 ELE5 - 10).
      *----------------------------------------------------------------
       J-017.
           MOVE "ENNS082  7." TO VDU.
           MOVE ELE5         TO VDUROW.
           PERFORM SCREENIO.
           IF ( VDUSTATUS = "1U" )
              GO TO J-016.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO J-017.
           IF ELE5 < 11
              MOVE VDUNUM2 TO BY-INS-AMT-WROTE(2 ELE5)
           ELSE
              MOVE VDUNUM2 TO BY-FEE-AMT-WROTE(2 ELE5 - 10).
           GO TO CREATE-ROUTINE.
      *-----------------------------------------------------------------
      *
      *    SET RECORD ELEMENTS ----- PAGE 6
      *
      *-----------------------------------------------------------------
       K-01.
           MOVE VDUNUM0 TO BY-NO-REPO(1 SUB2 ELE6).

      *-----------------------------------------------------------------
       K-011.
           MOVE "ENNS050  1." TO VDU.
           MOVE ELE6 TO VDUROW.
           PERFORM SCREENIO.

           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE6.

           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO K-011.

           MOVE VDUNUM0 TO BY-NO-REPO(2 SUB2 ELE6).

      *-----------------------------------------------------------------
       K-012.
           MOVE "ENNS050  2." TO VDU.
           MOVE ELE6 TO VDUROW.
           PERFORM SCREENIO.

           IF VDUSTATUS = "10"
              GO TO K-011.

           IF VDUSTATUS = "1U"
              GO TO K-011.

           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO K-012.

           MOVE VDUNUM0 TO BY-NO-REPO(3 SUB2 ELE6).

      *-----------------------------------------------------------------
       K-013.
           MOVE "ENNS082  3." TO VDU.
           MOVE ELE6 TO VDUROW.
           PERFORM SCREENIO.

           IF VDUSTATUS = "10"
              GO TO K-011.

           IF VDUSTATUS = "1U"
              GO TO K-012.

           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO K-013.

           MOVE VDUNUM2 TO BY-AMT-REPO(1 SUB2 ELE6).

      *-----------------------------------------------------------------
       K-014.
           MOVE "ENNS082  4." TO VDU.
           MOVE ELE6 TO VDUROW.
           PERFORM SCREENIO.

           IF VDUSTATUS = "10"
              GO TO K-011.

           IF VDUSTATUS = "1U"
              GO TO K-013.

           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO K-014.

           MOVE VDUNUM2 TO BY-AMT-REPO(2 SUB2 ELE6).

      *-----------------------------------------------------------------
       K-015.
           MOVE "ENNS082  5." TO VDU.
           MOVE ELE6 TO VDUROW.
           PERFORM SCREENIO.

           IF VDUSTATUS = "10"
              GO TO K-011.

           IF VDUSTATUS = "1U"
              GO TO K-014.

           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO K-015.

           MOVE VDUNUM2 TO BY-AMT-REPO(3 SUB2 ELE6).
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       K-02.
           COMPUTE PLSUB = ELE6 - 5.
           MOVE VDUNUM2 TO BY-O2-REPO(1 SUB2 PLSUB).

      *-----------------------------------------------------------------
       K-021.
           MOVE "ENNS072  1." TO VDU.
           MOVE ELE6 TO VDUROW.
           PERFORM SCREENIO.

           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE6.

           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO K-021.

           MOVE VDUNUM2 TO BY-O2-REPO(2 SUB2 PLSUB).

      *-----------------------------------------------------------------
       K-022.
           MOVE "ENNS072  2." TO VDU.
           MOVE ELE6 TO VDUROW.
           PERFORM SCREENIO.

           IF VDUSTATUS = "10"
              GO TO K-021.

           IF VDUSTATUS = "1U"
              GO TO K-021.

           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO K-022.

           MOVE VDUNUM2 TO BY-O2-REPO(3 SUB2 PLSUB).

      *-----------------------------------------------------------------
       K-023.
           MOVE "ENNS072  3." TO VDU.
           MOVE ELE6 TO VDUROW.
           PERFORM SCREENIO.

           IF VDUSTATUS = "10"
              GO TO K-021.

           IF VDUSTATUS = "1U"
              GO TO K-022.

           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO K-023.

           MOVE VDUNUM2 TO BY-INT-REPO(1 SUB2 PLSUB).

      *-----------------------------------------------------------------
       K-024.
           MOVE "ENNS072  4." TO VDU.
           MOVE ELE6 TO VDUROW.
           PERFORM SCREENIO.

           IF VDUSTATUS = "10"
              GO TO K-021.

           IF VDUSTATUS = "1U"
              GO TO K-023.

           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO K-024.

           MOVE VDUNUM2 TO BY-INT-REPO(2 SUB2 PLSUB).

      *-----------------------------------------------------------------
       K-025.
           MOVE "ENNS072  5." TO VDU.
           MOVE ELE6 TO VDUROW.
           PERFORM SCREENIO.

           IF VDUSTATUS = "10"
              GO TO K-021.

           IF VDUSTATUS = "1U"
              GO TO K-024.

           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO K-025.

           MOVE VDUNUM2 TO BY-INT-REPO(3 SUB2 PLSUB).
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
      *
      *    SET RECORD ELEMENTS ----- PAGE 7
      *
      *-----------------------------------------------------------------
       L-01.
           MOVE VDUNUM2 TO BY-INTREB(1).
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       L-02.
           MOVE VDUNUM2 TO BY-INTREB(2).
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       L-03.
           MOVE VDUNUM2 TO BY-CLREB1(1).
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       L-04.
           MOVE VDUNUM2 TO BY-CLREB1(2).
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       L-05.
           MOVE VDUNUM2 TO BY-CLREB2(1).
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       L-06.
           MOVE VDUNUM2 TO BY-CLREB2(2).
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       L-07.
           MOVE VDUNUM2 TO BY-CLREB3(1).
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       L-08.
           MOVE VDUNUM2 TO BY-CLREB3(2).
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       L-09.
           MOVE VDUNUM2 TO BY-CLREB4(1).
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       L-10.
           MOVE VDUNUM2 TO BY-CLREB4(2).
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       L-11.
           MOVE VDUNUM2 TO BY-AHREB(1).
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       L-12.
           MOVE VDUNUM2 TO BY-AHREB(2).
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       L-13.
           MOVE VDUNUM2 TO BY-PPREB(1).
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       L-14.
           MOVE VDUNUM2 TO BY-PPREB(2).
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       L-15.
           MOVE VDUNUM2 TO BY-O1REB(1).
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       L-16.
           MOVE VDUNUM2 TO BY-O1REB(2).
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       L-17.
           MOVE VDUNUM2 TO BY-O2REB(1).
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       L-18.
           MOVE VDUNUM2 TO BY-O2REB(2).
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       L-19.
           MOVE VDUNUM2 TO BY-O3REB(1).
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       L-20.
           MOVE VDUNUM2 TO BY-O3REB(2).
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       L-21.
           MOVE VDUNUM2 TO BY-O4REB(1).
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       L-22.
           MOVE VDUNUM2 TO BY-O4REB(2).
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       L-23.
           MOVE VDUNUM2 TO BY-O5REB(1).
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       L-24.
           MOVE VDUNUM2 TO BY-O5REB(2).
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       L-25.
           MOVE VDUNUM2 TO BY-O6REB(1).
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       L-26.
           MOVE VDUNUM2 TO BY-O6REB(2).
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       L-27.
           MOVE VDUNUM2 TO BY-O7REB(1).
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       L-28.
           MOVE VDUNUM2 TO BY-O7REB(2).
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       L-29.
           MOVE VDUNUM2 TO BY-SCREB(1).
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       L-30.
           MOVE VDUNUM2 TO BY-SCREB(2).
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       L-31.
           MOVE VDUNUM2 TO BY-MFREB(1).
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       L-32.
           MOVE VDUNUM2 TO BY-MFREB(2).
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       L-33.
           MOVE VDUNUM2 TO BY-DFREB(1).
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       L-34.
           MOVE VDUNUM2 TO BY-DFREB(2).
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
      *
      *    SET RECORD ELEMENTS ----- PAGE 8
      *
      *-----------------------------------------------------------------
       M-01.
           MOVE VDUNUM2 TO BY-O2(1 SUB2 ELE8).

      *-----------------------------------------------------------------
       M-011.
           MOVE "ENNS082  1." TO VDU.
           MOVE ELE8 TO VDUROW.
           PERFORM SCREENIO.

           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE8.

           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO M-011.

           MOVE VDUNUM2 TO BY-O2(2 SUB2 ELE8).

      *-----------------------------------------------------------------
       M-012.
           MOVE "ENNS082  2." TO VDU.
           MOVE ELE8 TO VDUROW.
           PERFORM SCREENIO.

           IF VDUSTATUS = "10"
              GO TO M-011.

           IF VDUSTATUS = "1U"
              GO TO M-011.

           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO M-012.

           MOVE VDUNUM2 TO BY-O2(3 SUB2 ELE8).

      *-----------------------------------------------------------------
       M-013.
           MOVE "ENNS082  3." TO VDU.
           MOVE ELE8 TO VDUROW.
           PERFORM SCREENIO.

           IF VDUSTATUS = "10"
              GO TO M-011.

           IF VDUSTATUS = "1U"
              GO TO M-012.

           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO M-013.

           MOVE VDUNUM2 TO BY-INT(1 SUB2 ELE8).

      *-----------------------------------------------------------------
       M-014.
           MOVE "ENNS082  4." TO VDU.
           MOVE ELE8 TO VDUROW.
           PERFORM SCREENIO.

           IF VDUSTATUS = "10"
              GO TO M-011.

           IF VDUSTATUS = "1U"
              GO TO M-013.

           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO M-014.

           MOVE VDUNUM2 TO BY-INT(2 SUB2 ELE8).

      *-----------------------------------------------------------------
       M-015.
           MOVE "ENNS082  5." TO VDU.
           MOVE ELE8 TO VDUROW.
           PERFORM SCREENIO.

           IF VDUSTATUS = "10"
              GO TO M-011.

           IF VDUSTATUS = "1U"
              GO TO M-014.

           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO M-015.

           MOVE VDUNUM2 TO BY-INT(3 SUB2 ELE8).
           GO TO CREATE-ROUTINE.

      *----------------------------------------------------------------
      *
      *    SET RECORD ELEMENTS ----- PAGE 9
      *
      *-----------------------------------------------------------------
       N-01.
           MOVE VDUNUM0 TO BY-NO-PL(1 SUB2 ELE9).

      *-----------------------------------------------------------------
       N-011.
           MOVE "ENNS050  1." TO VDU.
           MOVE ELE9 TO VDUROW.
           PERFORM SCREENIO.

           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE9.

           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO N-011.

           MOVE VDUNUM0 TO BY-NO-PL(2 SUB2 ELE9).

      *-----------------------------------------------------------------
       N-012.
           MOVE "ENNS050  2." TO VDU.
           MOVE ELE9 TO VDUROW.
           PERFORM SCREENIO.

           IF VDUSTATUS = "10"
              GO TO N-011.

           IF VDUSTATUS = "1U"
              GO TO N-011.

           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO N-012.

           MOVE VDUNUM0 TO BY-NO-PL(3 SUB2 ELE9).

      *-----------------------------------------------------------------
       N-013.
           MOVE "ENNS082  3." TO VDU.
           MOVE ELE9 TO VDUROW.
           PERFORM SCREENIO.

           IF VDUSTATUS = "10"
              GO TO N-011.

           IF VDUSTATUS = "1U"
              GO TO N-012.

           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO N-013.

           MOVE VDUNUM2 TO BY-AMT-PL(1 SUB2 ELE9).

      *-----------------------------------------------------------------
       N-014.
           MOVE "ENNS082  4." TO VDU.
           MOVE ELE9 TO VDUROW.
           PERFORM SCREENIO.

           IF VDUSTATUS = "10"
              GO TO N-011.

           IF VDUSTATUS = "1U"
              GO TO N-013.

           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO N-014.

           MOVE VDUNUM2 TO BY-AMT-PL(2 SUB2 ELE9).

      *-----------------------------------------------------------------
       N-015.
           MOVE "ENNS082  5." TO VDU.
           MOVE ELE9 TO VDUROW.
           PERFORM SCREENIO.

           IF VDUSTATUS = "10"
              GO TO N-011.

           IF VDUSTATUS = "1U"
              GO TO N-014.

           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO N-015.

           MOVE VDUNUM2 TO BY-AMT-PL(3 SUB2 ELE9).
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       N-11.
           MOVE VDUNUM2 TO BY-PART-CHGOFF(1).

      *-----------------------------------------------------------------
       N-111.
           MOVE "E NS082111." TO VDU.
           PERFORM SCREENIO.

           IF (                    UP-KEY ) GO TO ENTER-ELE9.
           IF NOT ( DOWN-KEY OR ENTER-KEY ) GO TO N-111.

           MOVE VDUNUM2 TO BY-PART-CHGOFF(2).
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
      *
      *    SET RECORD ELEMENTS ----- PAGE 10
      *
      *-----------------------------------------------------------------
       P-01.
           MOVE VDUNUM2 TO BY-O2-PL(1 SUB2 ELE10).

      *-----------------------------------------------------------------
       P-011.
           MOVE "ENNS082  1." TO VDU.
           MOVE ELE10 TO VDUROW.
           PERFORM SCREENIO.

           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE10.

           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO P-011.

           MOVE VDUNUM2 TO BY-O2-PL(2 SUB2 ELE10).

      *-----------------------------------------------------------------
       P-012.
           MOVE "ENNS082  2." TO VDU.
           MOVE ELE10 TO VDUROW.
           PERFORM SCREENIO.

           IF VDUSTATUS = "10"
              GO TO P-011.

           IF VDUSTATUS = "1U"
              GO TO P-011.

           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO P-012.

           MOVE VDUNUM2 TO BY-O2-PL(3 SUB2 ELE10).

      *-----------------------------------------------------------------
       P-013.
           MOVE "ENNS082  3." TO VDU.
           MOVE ELE10 TO VDUROW.
           PERFORM SCREENIO.

           IF VDUSTATUS = "10"
              GO TO P-011.

           IF VDUSTATUS = "1U"
              GO TO P-012.

           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO P-013.

           MOVE VDUNUM2 TO BY-INT-PL(1 SUB2 ELE10).

      *-----------------------------------------------------------------
       P-014.
           MOVE "ENNS082  4." TO VDU.
           MOVE ELE10 TO VDUROW.
           PERFORM SCREENIO.

           IF VDUSTATUS = "10"
              GO TO P-011.

           IF VDUSTATUS = "1U"
              GO TO P-013.

           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO P-014.

           MOVE VDUNUM2 TO BY-INT-PL(2 SUB2 ELE10).

      *-----------------------------------------------------------------
       P-015.
           MOVE "ENNS082  5." TO VDU.
           MOVE ELE10 TO VDUROW.
           PERFORM SCREENIO.

           IF VDUSTATUS = "10"
              GO TO P-011.

           IF VDUSTATUS = "1U"
              GO TO P-014.

           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO P-015.

           MOVE VDUNUM2 TO BY-INT-PL(3 SUB2 ELE10).
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
      *
      *    SET RECORD ELEMENTS ----- PAGE 11
      *
      *-----------------------------------------------------------------
       Q-01.
           MOVE VDUNUM2 TO BY-PL-PRINCOL(SUB11 1).
       Q-011.
           MOVE "ENNS082  1." TO VDU.
           MOVE ELE11 TO VDUROW.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE11.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO Q-011.
           MOVE VDUNUM2 TO BY-PL-INTCOL(SUB11 1).
           GO TO CREATE-ROUTINE.
       Q-02.
           MOVE VDUNUM2 TO BY-PL-LATECOL(SUB11 1).
       Q-021.
           MOVE "ENNS082  1." TO VDU.
           MOVE ELE11 TO VDUROW.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE11.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO Q-021.
           MOVE VDUNUM2 TO BY-PL-O2COL(SUB11 1).
           GO TO CREATE-ROUTINE.
       Q-03.
           MOVE VDUNUM2 TO BY-PL-PRINCOL(SUB11 2).
       Q-031.
           MOVE "ENNS082  1." TO VDU.
           MOVE ELE11 TO VDUROW.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE11.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO Q-031.
           MOVE VDUNUM2 TO BY-PL-INTCOL(SUB11 2).
           GO TO CREATE-ROUTINE.
       Q-04.
           MOVE VDUNUM2 TO BY-PL-LATECOL(SUB11 2).
       Q-041.
           MOVE "ENNS082  1." TO VDU.
           MOVE ELE11 TO VDUROW.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE11.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO Q-041.
           MOVE VDUNUM2 TO BY-PL-O2COL(SUB11 2).
           GO TO CREATE-ROUTINE.
       Q-05.
           MOVE VDUNUM2 TO BY-PL-PRINCOL(SUB11 3).
       Q-051.
           MOVE "ENNS082  1." TO VDU.
           MOVE ELE11 TO VDUROW.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE11.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO Q-051.
           MOVE VDUNUM2 TO BY-PL-INTCOL(SUB11 3).
           GO TO CREATE-ROUTINE.
       Q-06.
           MOVE VDUNUM2 TO BY-PL-LATECOL(SUB11 3).
       Q-061.
           MOVE "ENNS082  1." TO VDU.
           MOVE ELE11 TO VDUROW.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE11.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO Q-061.
           MOVE VDUNUM2 TO BY-PL-O2COL(SUB11 3).
           GO TO CREATE-ROUTINE.
       Q-07.
           MOVE VDUNUM2 TO BY-PL-PRINCOL(SUB11 4).
       Q-071.
           MOVE "ENNS082  1." TO VDU.
           MOVE ELE11 TO VDUROW.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE11.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO Q-071.
           MOVE VDUNUM2 TO BY-PL-INTCOL(SUB11 4).
           GO TO CREATE-ROUTINE.
       Q-08.
           MOVE VDUNUM2 TO BY-PL-LATECOL(SUB11 4).
       Q-081.
           MOVE "ENNS082  1." TO VDU.
           MOVE ELE11 TO VDUROW.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE11.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO Q-081.
           MOVE VDUNUM2 TO BY-PL-O2COL(SUB11 4).
           GO TO CREATE-ROUTINE.
       Q-09.
           MOVE VDUNUM2 TO BY-PL-PRINCOL(SUB11 5).
       Q-091.
           MOVE "ENNS082  1." TO VDU.
           MOVE ELE11 TO VDUROW.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE11.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO Q-091.
           MOVE VDUNUM2 TO BY-PL-INTCOL(SUB11 5).
           GO TO CREATE-ROUTINE.
       Q-10.
           MOVE VDUNUM2 TO BY-PL-LATECOL(SUB11 5).
       Q-101.
           MOVE "ENNS082  1." TO VDU.
           MOVE ELE11 TO VDUROW.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE11.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO Q-101.
           MOVE VDUNUM2 TO BY-PL-O2COL(SUB11 5).
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
      *
      *    SET RECORD ELEMENTS ----- PAGE 12
      *
      *-----------------------------------------------------------------
       R-01.
           MOVE VDUNUM2 TO BY-DLRVPRIN(1 SUB2 ELE12).

      *-----------------------------------------------------------------
       R-011.
           MOVE "ENNS082  1." TO VDU.
           MOVE ELE12 TO VDUROW.
           PERFORM SCREENIO.

           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE12.

           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO R-011.

           MOVE VDUNUM2 TO BY-DLRVPRIN(2 SUB2 ELE12).

      *-----------------------------------------------------------------
       R-012.
           MOVE "ENNS082  2." TO VDU.
           MOVE ELE12 TO VDUROW.
           PERFORM SCREENIO.

           IF VDUSTATUS = "10"
              GO TO R-011.

           IF VDUSTATUS = "1U"
              GO TO R-011.

           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO R-012.

           MOVE VDUNUM2 TO BY-DLRVPRIN(3 SUB2 ELE12).
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       R-04.
           SUBTRACT 3 FROM ELE12 GIVING SUB3.
           MOVE VDUNUM2 TO BY-DLRVCHGS(1 SUB2 SUB3).

      *-----------------------------------------------------------------
       R-041.
           MOVE "ENNS082  1." TO VDU.
           MOVE ELE12 TO VDUROW.
           PERFORM SCREENIO.

           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE12.

           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO R-041.

           MOVE VDUNUM2 TO BY-DLRVCHGS(2 SUB2 SUB3).

      *-----------------------------------------------------------------
       R-042.
           MOVE "ENNS082  2." TO VDU.
           MOVE ELE12 TO VDUROW.
           PERFORM SCREENIO.

           IF VDUSTATUS = "10"
              GO TO R-041.

           IF VDUSTATUS = "1U"
              GO TO R-041.

           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO R-042.

           MOVE VDUNUM2 TO BY-DLRVCHGS(3 SUB2 SUB3).
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
       R-07.
           MOVE VDUNUM2 TO BY-DLPARVHELD(1 SUB2).

      *-----------------------------------------------------------------
       R-071.
           MOVE "ENNS082  1." TO VDU.
           MOVE ELE12 TO VDUROW.
           PERFORM SCREENIO.

           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE12.

           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO R-071.

           MOVE VDUNUM2 TO BY-DLPARVHELD(2 SUB2).

      *-----------------------------------------------------------------
       R-072.
           MOVE "ENNS082  2." TO VDU.
           MOVE ELE12 TO VDUROW.
           PERFORM SCREENIO.

           IF VDUSTATUS = "10"
              GO TO R-071.

           IF VDUSTATUS = "1U"
              GO TO R-071.

           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO R-072.

           MOVE VDUNUM2 TO BY-DLPARVHELD(3 SUB2).
           GO TO CREATE-ROUTINE.
      *-----------------------------------------------------------------
       R-08.
           IF ACTIVITY NOT = 2
              GO TO CREATE-ROUTINE.

           MOVE VDUNUM2 TO BY-DLDISC(1).

      *-----------------------------------------------------------------
       R-081.
           MOVE "ENNS082  1." TO VDU.
           MOVE ELE12 TO VDUROW.
           PERFORM SCREENIO.

           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE12.

           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO R-081.

           MOVE VDUNUM2 TO BY-DLDISC(2).

      *-----------------------------------------------------------------
       R-082.
           MOVE "ENNS082  2." TO VDU.
           MOVE ELE12 TO VDUROW.
           PERFORM SCREENIO.

           IF VDUSTATUS = "10"
              GO TO R-081.

           IF VDUSTATUS = "1U"
              GO TO R-081.

           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO R-082.

           MOVE VDUNUM2 TO BY-DLDISC(3).
           GO TO CREATE-ROUTINE.


      *-----------------------------------------------------------------
      *
      *    SET RECORD ELEMENTS FOR PAGE 13
      *
      *-----------------------------------------------------------------
       S-01.
           GO TO CREATE-ROUTINE.

      *-----------------------------------------------------------------
      *    SET RECORD ELEMENTS FOR PAGE 14
      *-----------------------------------------------------------------
       T-01.
           MOVE VDUNUM2 TO BY-UNEARNED-AMT(1 SUB2 TYPEX).
       T-011.
           MOVE "E NS082  1." TO VDU.
           MOVE ELE14 TO VDUROW.
           PERFORM SCREENIO.
           IF UP-KEY GO TO ENTER-ELE14.
           IF NOT (ENTER-KEY OR DOWN-KEY) GO TO T-011.
           MOVE VDUNUM2 TO BY-UNEARNED-AMT(2 SUB2 TYPEX).
       T-012.
           MOVE "E NS082  2." TO VDU.
           MOVE ELE14 TO VDUROW.
           PERFORM SCREENIO.
           IF UP-KEY GO TO T-011.
           IF NOT (ENTER-KEY OR DOWN-KEY) GO TO T-012.
           MOVE VDUNUM2 TO BY-UNEARNED-AMT(3 SUB2 TYPEX).

           GO TO CREATE-ROUTINE.

       END-ENTRY-MODULE.
           EXIT.

      ******************************************************************
       ENTER-SCANKEY SECTION.
      ******************************************************************

           MOVE "Y" TO WS-STOP-ON-SCANKEY-FG.
           MOVE BY-BRNO TO BR-NO.
           MOVE BY-DATE-CCYYMM TO WS-HOLD-DATE-CCYYMM.
           PERFORM OPEN-BR-FILE.
           PERFORM START-BR-FILE.

       READ-BR-NEXT.
           PERFORM READ-BR-FILE-NEXT.
           IF ( IO-FG = 9 )
              MOVE "CLR" TO IO-TYPE
              GO TO CLOSE-SCANKEY.

           IF BR-MACHINE = SPACES
              GO TO READ-BR-NEXT.

           MOVE 0 TO FIRST-SCAN.

       ENTER-SCAN.

           PERFORM LOAD-BRANCH-ACCESS.
           IF ( BYA-PATH = SPACES ) OR
              ( BYB-PATH = SPACES )
              GO TO READ-BR-NEXT.

           MOVE ZEROES       TO BY-CLASS.
           MOVE BR-NO       TO BY-BRNO.

           MOVE WS-HOLD-DATE-CCYYMM TO BY-DATE-CCYYMM.
           PERFORM OPEN-BY1-FILE.

           IF ( IO-FG = 9 )
              GO TO READ-BR-NEXT.

       ENTER-KEY.
           IF ( WS-STOP-ON-SCANKEY-FG = "Y" )
              MOVE "EN N030SCANKEY." TO VDU
              PERFORM SCREENIO
           ELSE
              GO TO READ-BYFILE-NEXT.

           IF ( VDUSTATUS = "10" )
              MOVE "CLR" TO IO-TYPE
              GO TO CLOSE-SCANKEY.

           IF ( VDUSTATUS = "1<" )
              GO TO READ-BYFILE-NEXT.

           IF ( VDUSTATUS NOT = "00" )
              GO TO ENTER-KEY.

           IF ( VDUNUM0 = 0 )
              IF ( FIRST-SCAN NOT = 0 )
                 MOVE BY1-KEY TO HOLD-BY1-KEY
                 MOVE BY-REC TO ORIGINAL-BY-REC
                 GO TO CLOSE-SCANKEY
              ELSE
                 GO TO ENTER-KEY.

           MOVE VDUNUM0    TO BY-CLASS.

           PERFORM START-BY1-FILE.

       READ-BYFILE-NEXT.
           IF ( FIRST-SCAN = 0 )
              PERFORM START-BY1-FILE
              MOVE 1 TO FIRST-SCAN.

           PERFORM READ-BY1-FILE-NEXT.

           IF ( IO-FG = 9 )
              MOVE "N" TO WS-STOP-ON-SCANKEY-FG
              GO TO READ-BR-NEXT.

           IF ( BY-DATE-CCYYMM NOT = HOLD-BY-DATE-CCYYMM )
              MOVE "N" TO WS-STOP-ON-SCANKEY-FG
              GO TO READ-BR-NEXT.

           IF ( BY-BRNO NOT = BR-NO )
                MOVE "N" TO WS-STOP-ON-SCANKEY-FG
                GO TO READ-BR-NEXT.

           MOVE "Y" TO WS-STOP-ON-SCANKEY-FG.
      *    UNLOCK BYA-FILE.     
      *    UNLOCK BYB-FILE.    
           PERFORM DISPLAY-SCAN-KEY.
           PERFORM DISPLAY-PAGE.
           GO TO ENTER-KEY.
 
       DISPLAY-SCAN-KEY.
           MOVE "SN N040BRANCH." TO VDU.
           MOVE BY-BRNO TO VDUNUM0.
           PERFORM SCREENIO.

           MOVE "SN N030LC." TO VDU.
           MOVE BY-CLASS TO VDUNUM0.
           PERFORM SCREENIO.

       CLOSE-SCANKEY.
           PERFORM CLOSE-BY1-FILE.
           MOVE 0 TO FIRST-SCAN.

       END-SCANKEY.
           EXIT.

      *****************************************************************
       WRITE-MODULE SECTION.
      *****************************************************************
           PERFORM OPEN-BY1-FILE.

           IF PAGE1 AND NEW-FLAG = 0
              OR  (PAGE2 AND NEW-FLAG-SCR2 = 0)
              OR  (PAGE3 AND NEW-FLAG-SCR3 = 0)
              OR  (PAGE4 AND NEW-FLAG-SCR4 = 0)
              OR  (PAGE5 AND NEW-FLAG-SCR5 = 0)
              OR  (PAGE6 AND NEW-FLAG-SCR6 = 0)
              OR  (PAGE7 AND NEW-FLAG-SCR7 = 0)
              OR  (PAGE8 AND NEW-FLAG-SCR8 = 0)
              OR  (PAGE9 AND NEW-FLAG-SCR9 = 0)
              OR  (PAGE10 AND NEW-FLAG-SCR10 = 0)
              OR  (PAGE11 AND NEW-FLAG-SCR11 = 0)
              OR  (PAGE12 AND NEW-FLAG-SCR12 = 0)
              OR  (PAGE13 AND NEW-FLAG-SCR13 = 0)
              OR  (PAGE14 AND NEW-FLAG-SCR14 = 0)
                   NEXT SENTENCE
           ELSE
                   GO TO WRITE-RECORD.

           MOVE BY-REC TO CHANGED-BY-REC.

           IF ORIGINAL-BY-REC = CHANGED-BY-REC
              GO TO END-WRITE-MODULE.

           PERFORM READ-BY1-FILE.

           IF IO-FG NOT = 0
              PERFORM UNDERGONE-CHANGE
              PERFORM RECORD-DELETED
              GO TO END-WRITE-MODULE.

           IF BY-REC NOT = ORIGINAL-BY-REC
              PERFORM UNDERGONE-CHANGE
              PERFORM CHANGE-AGAIN
              GO TO END-WRITE-MODULE.

           MOVE CHANGED-BY-REC TO BY-REC.
           PERFORM REWRITE-BY1-FILE.

      * RECORD THAT THERE WAS A CHANGE IN A CHANGE FILE.
           MOVE "C" TO ERRLOGW-ACTION.
           MOVE BY1-KEY TO ERRLOGW-KEY.
           MOVE FORM-PATHNAME TO ERRLOGW-PROGX.
           MOVE "CL/ERRLOG" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH ERRLOGW-AREA.
           CANCEL FORM-PROGX.

           GO TO END-WRITE-MODULE.

       WRITE-RECORD.
           PERFORM WRITE-BY1-FILE.
           IF IO-FG NOT = 0
              PERFORM ALREADY-CREATED.

       END-WRITE-MODULE.
           PERFORM CLOSE-BY1-FILE.

      *****************************************************************
       DELETE-MODULE SECTION.
      *****************************************************************
           IF PAGE1 AND NEW-FLAG = 0
             OR (PAGE2 AND NEW-FLAG-SCR2 = 0)
             OR (PAGE3 AND NEW-FLAG-SCR3 = 0)
             OR (PAGE4 AND NEW-FLAG-SCR4 = 0)
             OR (PAGE5 AND NEW-FLAG-SCR5 = 0)
             OR (PAGE6 AND NEW-FLAG-SCR6 = 0)
             OR (PAGE7 AND NEW-FLAG-SCR7 = 0)
             OR (PAGE8 AND NEW-FLAG-SCR8 = 0)
             OR (PAGE9 AND NEW-FLAG-SCR9 = 0)
             OR (PAGE10 AND NEW-FLAG-SCR10 = 0)
             OR (PAGE11 AND NEW-FLAG-SCR11 = 0)
             OR (PAGE12 AND NEW-FLAG-SCR12 = 0)
             OR (PAGE13 AND NEW-FLAG-SCR13 = 0)
             OR (PAGE14 AND NEW-FLAG-SCR14 = 0)
                   NEXT SENTENCE
           ELSE
                   GO TO END-DELETE-MODULE.

           PERFORM OPEN-BY1-FILE.
           PERFORM READ-BY1-FILE.

           IF IO-FG NOT = 0
              PERFORM UNDERGONE-CHANGE
              PERFORM RECORD-DELETED
              GO TO END-DELETE-MODULE.

           IF BY-REC NOT = ORIGINAL-BY-REC
              PERFORM UNDERGONE-CHANGE
              PERFORM RECORD-CHANGED
              GO TO END-DELETE-MODULE.

           PERFORM DELETE-BY1-FILE.

       END-DELETE-MODULE.
           PERFORM CLOSE-BY1-FILE.

      *****************************************************************
       NEW-RECORD-SETUP SECTION.
      *****************************************************************
           MOVE 1 TO NEW-FLAG
                     NEW-FLAG-SCR2
                     NEW-FLAG-SCR3
                     NEW-FLAG-SCR4
                     NEW-FLAG-SCR5
                     NEW-FLAG-SCR6
                     NEW-FLAG-SCR7
                     NEW-FLAG-SCR8
                     NEW-FLAG-SCR9
                     NEW-FLAG-SCR10
                     NEW-FLAG-SCR11
                     NEW-FLAG-SCR12
                     NEW-FLAG-SCR13
                     NEW-FLAG-SCR14.

           MOVE "S  A000NEW." TO VDU.
           MOVE "*** NEW RECORD ***" TO VDUBUF.
           PERFORM SCREENIO.

      *-----------------------------------------------------------------
       CLEAR-NEW-RECORD.
           PERFORM CLEAR-BY-FILE.

           MOVE HOLD-BY1-KEY TO BY1-KEY.
           MOVE 0 TO ELE1
                     ELE2
                     ELE3
                     ELE4
                     ELE5
                     ELE6
                     ELE7
                     ELE8
                     ELE9
                     ELE10
                     ELE11
                     ELE12
                     ELE13
                     ELE14
                     ELEMENT.

      *****************************************************************
       DISPLAY-PAGE SECTION.
      *****************************************************************

           IF PAGE1  PERFORM DISPLAY-FIELDS
            ELSE IF PAGE2  PERFORM DISPLAY-FIELDS-2
            ELSE IF PAGE3  PERFORM DISPLAY-FIELDS-3
            ELSE IF PAGE4  PERFORM DISPLAY-FIELDS-4
            ELSE IF PAGE5  PERFORM DISPLAY-FIELDS-5
            ELSE IF PAGE6  PERFORM DISPLAY-FIELDS-6
            ELSE IF PAGE7  PERFORM DISPLAY-FIELDS-7
            ELSE IF PAGE8  PERFORM DISPLAY-FIELDS-8
            ELSE IF PAGE9  PERFORM DISPLAY-FIELDS-9
            ELSE IF PAGE10  PERFORM DISPLAY-FIELDS-10
            ELSE IF PAGE11  PERFORM DISPLAY-FIELDS-11
            ELSE IF PAGE12  PERFORM DISPLAY-FIELDS-12
            ELSE IF PAGE13  PERFORM DISPLAY-FIELDS-13
            ELSE IF PAGE14  PERFORM DISPLAY-FIELDS-14.

       END-DISPLAY-PAGE.
           EXIT.

      *****************************************************************
       DISPLAY-FIELDS SECTION.
      *****************************************************************
           MOVE " " TO DISPLAY-BUF1.

           MOVE BY-OTHCOL  (1) TO F01.
           MOVE BY-OTHCOL  (2) TO F02.
           MOVE BY-OT2COL  (1) TO F03.
           MOVE BY-OT2COL  (2) TO F04.
           MOVE BY-INTCOL  (1) TO F05.
           MOVE BY-INTCOL  (2) TO F06.
           MOVE BY-LATECOL (1) TO F07.
           MOVE BY-LATECOL (2) TO F08.
           MOVE BY-PRINCOL (1) TO F09.
           MOVE BY-PRINCOL (2) TO F10.
           MOVE BY-DEFCOL  (1) TO F11.
           MOVE BY-DEFCOL  (2) TO F12.
           MOVE BY-RNPRIN  (1) TO F13.
           MOVE BY-RNPRIN  (2) TO F14.
           MOVE BY-PART-CHGOFF  (1) TO F15.
           MOVE BY-PART-CHGOFF  (2) TO F16.

           MOVE BY-NO-SOLD     (1) TO F17.
           MOVE BY-AMT-SOLD    (1) TO F171.
           MOVE BY-NO-SOLD     (2) TO F18.
           MOVE BY-AMT-SOLD    (2) TO F181.

           MOVE BY-NO-TRANS     (1) TO F19.
           MOVE BY-AMT-TRANS    (1) TO F191.
           MOVE BY-NO-TRANS     (2) TO F20.
           MOVE BY-AMT-TRANS    (2) TO F201.

           MOVE BY-NO-PURCH     (1) TO F21.
           MOVE BY-AMT-PURCH    (1) TO F211.
           MOVE BY-NO-PURCH     (2) TO F22.
           MOVE BY-AMT-PURCH    (2) TO F221.

           MOVE BY-PROCEEDS     (1) TO F23.
           MOVE BY-PROCEEDS     (2) TO F24.

           MOVE DISPLAY-BUF1 TO WR-BUF.
           MOVE DISPLAY-LIST1 TO WR-LIST.
           PERFORM CALL-WRFORM.

       DISPLAY-FIELDS-EXIT.
           EXIT.

      *****************************************************************
       DISPLAY-FIELDS-2 SECTION.
      *****************************************************************
           MOVE ACT-TYPE(ACTIVITY) TO ACT-BUFFER.
           MOVE ACT-BUFFER TO WR-BUF.
           MOVE ACT-FNAME TO WR-LIST.
           PERFORM CALL-WRFORM.

           MOVE ACT-TYPEL(ACTIVITY) TO ACT-BUFFER.
           MOVE ACT-BUFFER TO WR-BUF.
           MOVE ACT-FNAMEL TO WR-LIST.
           PERFORM CALL-WRFORM.

           MOVE ACTIVITY TO SUB2.
           PERFORM SETUP2 VARYING SUB3 FROM 1 BY 1
                  UNTIL SUB3 > 11
                      AFTER SUB1 FROM 1 BY 1 UNTIL SUB1 > 3.

           MOVE BY-SUMWGTTM TO DATA-120.
           MOVE BY-SUMWGTAV TO DATA-130.
           MOVE BY-SUMWGTTA TO DATA-140.
           MOVE BY-IB-ACCRUED TO DATA-150.

           MOVE DISPLAY-BUF2 TO WR-BUF.
           MOVE DISPLAY-LIST2 TO WR-LIST.
           PERFORM CALL-WRFORM.

           GO TO DISPLAY-FIELDS-EXIT-2.

       SETUP2.
              MOVE BY-NO(SUB1 SUB2 SUB3) TO DATA-NO(SUB3 SUB1).
              MOVE BY-AMT(SUB1 SUB2 SUB3) TO DATA-AMT(SUB3 SUB1).

       DISPLAY-FIELDS-EXIT-2.
           EXIT.

      *****************************************************************
       DISPLAY-FIELDS-3 SECTION.
      *****************************************************************

           PERFORM SETUP-HEADER-DEL-TYPE.
           MOVE HEADER-BUFFER TO WR-BUF.
           MOVE HEADER-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

           PERFORM SETUP3
                   VARYING SUB3 FROM 1 BY 1 UNTIL SUB3 > 10
                     AFTER SUB2 FROM 1 BY 1 UNTIL SUB2 > 2.

           MOVE DISPLAY-BUF3 TO WR-BUF.
           MOVE DISPLAY-LIST3 TO WR-LIST.
           PERFORM CALL-WRFORM.

           GO TO EXIT-DISPLAY-FIELDS-3.

       SETUP3.

           IF ( HEADER-LEV-STD )
              MOVE BY-UNITS (DELINQ SUB2 SUB3) TO DEL-UNITS (SUB3 SUB2)
              MOVE BY-OUTBAL(DELINQ SUB2 SUB3) TO DEL-OUTBAL(SUB3 SUB2)
           ELSE
              MOVE BY-UNITS-BNKRPT (DELINQ SUB2 SUB3) TO
                                                  DEL-UNITS(SUB3 SUB2)
              MOVE BY-OUTBAL-BNKRPT(DELINQ SUB2 SUB3) TO
                                                  DEL-OUTBAL(SUB3 SUB2).

       EXIT-DISPLAY-FIELDS-3.
           EXIT.

      *****************************************************************
       DISPLAY-FIELDS-4 SECTION.
      *****************************************************************
           MOVE " " TO DISPLAY-BUF4.

           MOVE BY-ALLOTMENTS       (1) TO I01.
           MOVE BY-ALLOTMENTS       (2) TO I02.
           MOVE BY-LOCKBOX          (1) TO I03.
           MOVE BY-LOCKBOX          (2) TO I04.
      *---------AVAILABLE SPACES------->TO I05.
      *---------AVAILABLE SPACES------->TO I06.
      *---------AVAILABLE SPACES------->TO I07.
      *---------AVAILABLE SPACES------->TO I08.
           MOVE BY-OTH-NO-COL       (1) TO I09.
           MOVE BY-OTH-NO-COL       (2) TO I10.
           MOVE BY-OT2-NO-COL       (1) TO I11.
           MOVE BY-OT2-NO-COL       (2) TO I12.
           MOVE BY-INT-NO-COL       (1) TO I13.
           MOVE BY-INT-NO-COL       (2) TO I14.
           MOVE BY-LATE-NO-COL      (1) TO I15.
           MOVE BY-LATE-NO-COL      (2) TO I16.
           MOVE BY-OTH-NO-WAIVED    (1) TO I17.
           MOVE BY-OTH-NO-WAIVED    (2) TO I18.
           MOVE BY-OT2-NO-WAIVED    (1) TO I19.
           MOVE BY-OT2-NO-WAIVED    (2) TO I20.
           MOVE BY-INT-NO-WAIVED    (1) TO I21.
           MOVE BY-INT-NO-WAIVED    (2) TO I22.
           MOVE BY-LATE-NO-WAIVED   (1) TO I23.
           MOVE BY-LATE-NO-WAIVED   (2) TO I24.
           MOVE BY-OTH-AMT-WAIVED   (1) TO I25.
           MOVE BY-OTH-AMT-WAIVED   (2) TO I26.
           MOVE BY-OT2-AMT-WAIVED   (1) TO I27.
           MOVE BY-OT2-AMT-WAIVED   (2) TO I28.
           MOVE BY-INT-AMT-WAIVED   (1) TO I29.
           MOVE BY-INT-AMT-WAIVED   (2) TO I30.
           MOVE BY-LATE-AMT-WAIVED  (1) TO I31.
           MOVE BY-LATE-AMT-WAIVED  (2) TO I32.

           MOVE DISPLAY-BUF4 TO WR-BUF.
           MOVE DISPLAY-LIST4 TO WR-LIST.
           PERFORM CALL-WRFORM.

       DISPLAY-FIELDS-EXIT-4.
           EXIT.

      ******************************************************************
       DISPLAY-FIELDS-5 SECTION.
      ******************************************************************
           MOVE " " TO DISPLAY-BUF5.

           PERFORM DISPLAY-FIELDS-SETUP5
                   VARYING SUB1 FROM 1 BY 1 UNTIL SUB1 > 2
                     AFTER SUB2 FROM 1 BY 1 UNTIL SUB2 > 13.

           MOVE DISPLAY-BUF5 TO WR-BUF.
           MOVE DISPLAY-LIST5 TO WR-LIST.
           PERFORM CALL-WRFORM.

           GO TO DISPLAY-FIELDS-EXIT-5.

       DISPLAY-FIELDS-SETUP5.
           IF SUB2 < 11
              MOVE BY-INS-NO-ELIG(SUB1 SUB2) TO
                 DATA-INS-NO-ELIG(SUB1 SUB2)
              MOVE BY-INS-NO-WROTE(SUB1 SUB2) TO
                 DATA-INS-NO-WROTE(SUB1 SUB2)
              MOVE BY-INS-NO-NOTWROTE(SUB1 SUB2) TO
                 DATA-INS-NO-NOTWROTE(SUB1 SUB2)
              MOVE BY-INS-AMT-WROTE(SUB1 SUB2) TO
                 DATA-INS-AMT-WROTE(SUB1 SUB2)
           ELSE
              MOVE BY-FEE-NO-ELIG(SUB1 SUB2 - 10) TO
                 DATA-INS-NO-ELIG(SUB1 SUB2)
              MOVE BY-FEE-NO-WROTE(SUB1 SUB2 - 10) TO
                 DATA-INS-NO-WROTE(SUB1 SUB2)
              MOVE BY-FEE-NO-NOTWROTE(SUB1 SUB2 - 10) TO
                 DATA-INS-NO-NOTWROTE(SUB1 SUB2)
              MOVE BY-FEE-AMT-WROTE(SUB1 SUB2 - 10) TO
                 DATA-INS-AMT-WROTE(SUB1 SUB2).

       DISPLAY-FIELDS-EXIT-5.
           EXIT.

      *****************************************************************
       DISPLAY-FIELDS-6 SECTION.
      *****************************************************************
           MOVE ACT-TYPE(ACTIVITY) TO ACT-BUFFER.
           MOVE ACT-BUFFER TO WR-BUF.
           MOVE ACT-FNAME TO WR-LIST.
           PERFORM CALL-WRFORM.

           MOVE ACT-TYPEL(ACTIVITY) TO ACT-BUFFER.
           MOVE ACT-BUFFER TO WR-BUF.
           MOVE ACT-FNAMEL TO WR-LIST.
           PERFORM CALL-WRFORM.

           MOVE ACT-TYPEL8(ACTIVITY) TO ACT-BUFFER.
           MOVE ACT-BUFFER TO WR-BUF.
           MOVE ACT-FNAMEL2 TO WR-LIST.
           PERFORM CALL-WRFORM.

           MOVE ACTIVITY TO SUB2.
           PERFORM SETUP6 VARYING SUB3 FROM 1 BY 1
                  UNTIL SUB3 > 5
                      AFTER SUB1 FROM 1 BY 1 UNTIL SUB1 > 3.

           MOVE DISPLAY-BUF6 TO WR-BUF.
           MOVE DISPLAY-LIST6 TO WR-LIST.
           PERFORM CALL-WRFORM.
           MOVE DISPLAY-BUF6A TO WR-BUF.
           MOVE DISPLAY-LIST6A TO WR-LIST.
           PERFORM CALL-WRFORM.

           GO TO DISPLAY-FIELDS-EXIT-6.

       SETUP6.
           MOVE BY-NO-REPO(SUB1 SUB2 SUB3) TO
                DATA-REPO-NO(SUB3 SUB1).
           MOVE BY-AMT-REPO(SUB1 SUB2 SUB3) TO
                DATA-REPO-AMT(SUB3 SUB1).
           MOVE BY-O2-REPO(SUB1 SUB2 SUB3) TO
                DATA-O2-REPO(SUB3 SUB1).
           MOVE BY-INT-REPO(SUB1 SUB2 SUB3) TO
                DATA-INT-REPO(SUB3 SUB1).

       DISPLAY-FIELDS-EXIT-6.
           EXIT.

      *****************************************************************
       DISPLAY-FIELDS-7 SECTION.
      *****************************************************************
           MOVE " " TO DISPLAY-BUF7.

           MOVE BY-INTREB  (1) TO L01.
           MOVE BY-INTREB  (2) TO L02.
           MOVE BY-CLREB1  (1) TO L03.
           MOVE BY-CLREB1  (2) TO L04.
           MOVE BY-CLREB2  (1) TO L05.
           MOVE BY-CLREB2  (2) TO L06.
           MOVE BY-CLREB3  (1) TO L07.
           MOVE BY-CLREB3  (2) TO L08.
           MOVE BY-CLREB4  (1) TO L09.
           MOVE BY-CLREB4  (2) TO L10.
           MOVE BY-AHREB   (1) TO L11.
           MOVE BY-AHREB   (2) TO L12.
           MOVE BY-PPREB   (1) TO L13.
           MOVE BY-PPREB   (2) TO L14.
           MOVE BY-O1REB   (1) TO L15.
           MOVE BY-O1REB   (2) TO L16.
           MOVE BY-O2REB   (1) TO L17.
           MOVE BY-O2REB   (2) TO L18.
           MOVE BY-O3REB   (1) TO L19.
           MOVE BY-O3REB   (2) TO L20.
           MOVE BY-O4REB   (1) TO L21.
           MOVE BY-O4REB   (2) TO L22.
           MOVE BY-O5REB   (1) TO L23.
           MOVE BY-O5REB   (2) TO L24.
           MOVE BY-O6REB   (1) TO L25.
           MOVE BY-O6REB   (2) TO L26.
           MOVE BY-O7REB   (1) TO L27.
           MOVE BY-O7REB   (2) TO L28.
           MOVE BY-SCREB   (1) TO L29.
           MOVE BY-SCREB   (2) TO L30.
           MOVE BY-MFREB   (1) TO L31.
           MOVE BY-MFREB   (2) TO L32.
           MOVE BY-DFREB   (1) TO L33.
           MOVE BY-DFREB   (2) TO L34.

           MOVE DISPLAY-BUF7 TO WR-BUF.
           MOVE DISPLAY-LIST7 TO WR-LIST.
           PERFORM CALL-WRFORM.

       DISPLAY-FIELDS-7-EXIT.
           EXIT.

      *****************************************************************
       DISPLAY-FIELDS-8 SECTION.
      *****************************************************************
           MOVE ACT-TYPE(ACTIVITY) TO ACT-BUFFER.
           MOVE ACT-BUFFER TO WR-BUF.
           MOVE ACT-FNAME TO WR-LIST.
           PERFORM CALL-WRFORM.

           MOVE ACT-TYPEL8(ACTIVITY) TO ACT-BUFFER.
           MOVE ACT-BUFFER TO WR-BUF.
           MOVE ACT-FNAMEL TO WR-LIST.
           PERFORM CALL-WRFORM.

           MOVE ACTIVITY TO SUB2.
           PERFORM SETUP8 VARYING SUB3 FROM 1 BY 1
                  UNTIL SUB3 > 11
                      AFTER SUB1 FROM 1 BY 1 UNTIL SUB1 > 3.

           MOVE DISPLAY-BUF8 TO WR-BUF.
           MOVE DISPLAY-LIST8 TO WR-LIST.
           PERFORM CALL-WRFORM.

           GO TO DISPLAY-FIELDS-EXIT-8.

       SETUP8.
              MOVE BY-O2(SUB1 SUB2 SUB3) TO DATA-O2(SUB3 SUB1).
              MOVE BY-INT(SUB1 SUB2 SUB3) TO DATA-INT(SUB3 SUB1).

       DISPLAY-FIELDS-EXIT-8.
           EXIT.

      *****************************************************************
       DISPLAY-FIELDS-9 SECTION.
      *****************************************************************
           MOVE ACT-TYPE(ACTIVITY) TO ACT-BUFFER.
           MOVE ACT-BUFFER TO WR-BUF.
           MOVE ACT-FNAME TO WR-LIST.
           PERFORM CALL-WRFORM.

           MOVE ACT-TYPEL8(ACTIVITY) TO ACT-BUFFER.
           MOVE ACT-BUFFER TO WR-BUF.
           MOVE ACT-FNAMEL TO WR-LIST.
           PERFORM CALL-WRFORM.

           MOVE ACTIVITY TO SUB2.
           PERFORM SETUP9 VARYING SUB3 FROM 1 BY 1
                  UNTIL SUB3 > 10
                      AFTER SUB1 FROM 1 BY 1 UNTIL SUB1 > 3.
           MOVE BY-PART-CHGOFF(1) TO DATA-PART-CHGOFF-MTD.
           MOVE BY-PART-CHGOFF(2) TO DATA-PART-CHGOFF-YTD.

           MOVE DISPLAY-BUF9 TO WR-BUF.
           MOVE DISPLAY-LIST9 TO WR-LIST.
           PERFORM CALL-WRFORM.

           GO TO DISPLAY-FIELDS-EXIT-9.

       SETUP9.
           MOVE BY-NO-PL(SUB1 SUB2 SUB3) TO DATA-NO-PL(SUB3 SUB1).
           MOVE BY-AMT-PL(SUB1 SUB2 SUB3) TO DATA-AMT-PL(SUB3 SUB1).

       DISPLAY-FIELDS-EXIT-9.
           EXIT.

      *****************************************************************
       DISPLAY-FIELDS-10 SECTION.
      *****************************************************************
           MOVE ACT-TYPE(ACTIVITY) TO ACT-BUFFER.
           MOVE ACT-BUFFER TO WR-BUF.
           MOVE ACT-FNAME TO WR-LIST.
           PERFORM CALL-WRFORM.

           MOVE ACT-TYPEL8(ACTIVITY) TO ACT-BUFFER.
           MOVE ACT-BUFFER TO WR-BUF.
           MOVE ACT-FNAMEL TO WR-LIST.
           PERFORM CALL-WRFORM.

           MOVE ACTIVITY TO SUB2.
           PERFORM SETUP10 VARYING SUB3 FROM 1 BY 1
                  UNTIL SUB3 > 10
                      AFTER SUB1 FROM 1 BY 1 UNTIL SUB1 > 3.


           MOVE DISPLAY-BUF10 TO WR-BUF.
           MOVE DISPLAY-LIST10 TO WR-LIST.
           PERFORM CALL-WRFORM.

           GO TO DISPLAY-FIELDS-EXIT-10.

       SETUP10.
           MOVE BY-O2-PL(SUB1 SUB2 SUB3) TO DATA-O2-PL(SUB3 SUB1).
           MOVE BY-INT-PL(SUB1 SUB2 SUB3) TO DATA-INT-PL(SUB3 SUB1).

       DISPLAY-FIELDS-EXIT-10.
           EXIT.

      *****************************************************************
       DISPLAY-FIELDS-11 SECTION.
      *****************************************************************

           MOVE MTD-TYPE(ACTIVITY) TO MTD-BUFFER.
           MOVE MTD-BUFFER TO WR-BUF.
           MOVE MTD-FNAME TO WR-LIST.
           PERFORM CALL-WRFORM.
           MOVE ACTIVITY TO SUB11.
           PERFORM SETUP11 VARYING SUB3 FROM 1 BY 1 UNTIL SUB3 > 5

           MOVE DISPLAY-BUF11 TO WR-BUF.
           MOVE DISPLAY-LIST11 TO WR-LIST.
           PERFORM CALL-WRFORM.

           GO TO DISPLAY-FIELDS-EXIT-11.

       SETUP11.
           MOVE BY-PL-PRINCOL(SUB11 SUB3) TO DATA-PL-PRINCOL(SUB3).
           MOVE BY-PL-INTCOL(SUB11 SUB3) TO DATA-PL-INTCOL(SUB3).
           MOVE BY-PL-LATECOL(SUB11 SUB3) TO DATA-PL-LATECOL(SUB3).
           MOVE BY-PL-O2COL(SUB11 SUB3) TO DATA-PL-O2COL(SUB3).

       DISPLAY-FIELDS-EXIT-11.
           EXIT.

      *****************************************************************
       DISPLAY-FIELDS-12 SECTION.
      *****************************************************************
           MOVE ACT-TYPE(ACTIVITY) TO ACT-BUFFER.
           MOVE ACT-BUFFER TO WR-BUF.
           MOVE ACT-FNAME TO WR-LIST.
           PERFORM CALL-WRFORM.

           MOVE ACT-TYPEL12(ACTIVITY) TO ACT-BUFFER.
           MOVE ACT-BUFFER TO WR-BUF.
           MOVE ACT-FNAMEL TO WR-LIST.
           PERFORM CALL-WRFORM.

           MOVE ACTIVITY TO SUB2.
           PERFORM SETUP12
                    VARYING SUB3 FROM 1 BY 1 UNTIL SUB3 > 3
                      AFTER SUB1 FROM 1 BY 1 UNTIL SUB1 > 3.


           MOVE DISPLAY-BUF12 TO WR-BUF.
           MOVE DISPLAY-LIST12 TO WR-LIST.
           PERFORM CALL-WRFORM.

           GO TO DISPLAY-FIELDS-EXIT-12.

       SETUP12.
           MOVE BY-DLRVPRIN(SUB1 SUB2 SUB3) TO DATA-DLRVPRIN(SUB3 SUB1).
           MOVE BY-DLRVCHGS(SUB1 SUB2 SUB3) TO DATA-DLRVCHGS(SUB3 SUB1).
           IF SUB3 = 1
              MOVE BY-DLPARVHELD(SUB1 SUB2)
                               TO DATA-DLPARVHELD(SUB1)
              IF SUB2 = 2
                 MOVE BY-DLDISC(SUB1) TO DATA-DLDISC(SUB1)
              ELSE
                 MOVE SPACES TO DATA-DLDISC-ALP(SUB1).

       DISPLAY-FIELDS-EXIT-12.
           EXIT.

      ******************************************************************
       DISPLAY-FIELDS-13 SECTION.
      ******************************************************************

           MOVE DISPLAY-BUF13 TO WR-BUF.
           MOVE DISPLAY-LIST13 TO WR-LIST.
           PERFORM CALL-WRFORM.

           GO TO DISPLAY-FIELDS-EXIT-13.

       DISPLAY-FIELDS-EXIT-13.
           EXIT.

      ******************************************************************
       DISPLAY-FIELDS-14 SECTION.
      ******************************************************************
           MOVE ACT-TYPE14(ACTIVITY) TO ACT-BUFFER.
           MOVE ACT-BUFFER TO WR-BUF.
           MOVE ACT-FNAMEL TO WR-LIST.
           PERFORM CALL-WRFORM.

           MOVE ACTIVITY TO SUB2.
      * CURRENT/MTD
           MOVE BY-UNEARNED-AMT(1 SUB2 1) TO P14-DISP-AMT(1  1).
           MOVE BY-UNEARNED-AMT(1 SUB2 2) TO P14-DISP-AMT(1  2).
           MOVE BY-UNEARNED-AMT(1 SUB2 3) TO P14-DISP-AMT(1  3).
           MOVE BY-UNEARNED-AMT(1 SUB2 4) TO P14-DISP-AMT(1  4).
           MOVE BY-UNEARNED-AMT(1 SUB2 5) TO P14-DISP-AMT(1  5).
           MOVE BY-UNEARNED-AMT(1 SUB2 6) TO P14-DISP-AMT(1  6).
           MOVE BY-UNEARNED-AMT(1 SUB2 7) TO P14-DISP-AMT(1  7).
           MOVE BY-UNEARNED-AMT(1 SUB2 8) TO P14-DISP-AMT(1  8).
           MOVE BY-UNEARNED-AMT(1 SUB2 9) TO P14-DISP-AMT(1  9).
           MOVE BY-UNEARNED-AMT(1 SUB2 10) TO P14-DISP-AMT(1 10).
      * NET ELM/YTD
           MOVE BY-UNEARNED-AMT(2 SUB2 1) TO P14-DISP-AMT(2  1).
           MOVE BY-UNEARNED-AMT(2 SUB2 2) TO P14-DISP-AMT(2  2).
           MOVE BY-UNEARNED-AMT(2 SUB2 3) TO P14-DISP-AMT(2  3).
           MOVE BY-UNEARNED-AMT(2 SUB2 4) TO P14-DISP-AMT(2  4).
           MOVE BY-UNEARNED-AMT(2 SUB2 5) TO P14-DISP-AMT(2  5).
           MOVE BY-UNEARNED-AMT(2 SUB2 6) TO P14-DISP-AMT(2  6).
           MOVE BY-UNEARNED-AMT(2 SUB2 7) TO P14-DISP-AMT(2  7).
           MOVE BY-UNEARNED-AMT(2 SUB2 8) TO P14-DISP-AMT(2  8).
           MOVE BY-UNEARNED-AMT(2 SUB2 9) TO P14-DISP-AMT(2  9).
           MOVE BY-UNEARNED-AMT(2 SUB2 10) TO P14-DISP-AMT(2 10).
      * NET ELY/LYR
           MOVE BY-UNEARNED-AMT(3 SUB2 1) TO P14-DISP-AMT(3  1).
           MOVE BY-UNEARNED-AMT(3 SUB2 2) TO P14-DISP-AMT(3  2).
           MOVE BY-UNEARNED-AMT(3 SUB2 3) TO P14-DISP-AMT(3  3).
           MOVE BY-UNEARNED-AMT(3 SUB2 4) TO P14-DISP-AMT(3  4).
           MOVE BY-UNEARNED-AMT(3 SUB2 5) TO P14-DISP-AMT(3  5).
           MOVE BY-UNEARNED-AMT(3 SUB2 6) TO P14-DISP-AMT(3  6).
           MOVE BY-UNEARNED-AMT(3 SUB2 7) TO P14-DISP-AMT(3  7).
           MOVE BY-UNEARNED-AMT(3 SUB2 8) TO P14-DISP-AMT(3  8).
           MOVE BY-UNEARNED-AMT(3 SUB2 9) TO P14-DISP-AMT(3  9).
           MOVE BY-UNEARNED-AMT(3 SUB2 10) TO P14-DISP-AMT(3 10).

           MOVE DISPLAY-BUF14 TO WR-BUF.
           MOVE DISPLAY-LIST14 TO WR-LIST.
           PERFORM CALL-WRFORM.

       DISPLAY-FIELDS-EXIT-14.
           EXIT.

      *****************************************************************
       NEW-ACTIVITY SECTION.
      *****************************************************************
           IF VDUSTATUS = "18"
               MOVE 1 TO ACTIVITY
           ELSE
               IF VDUSTATUS = "1:"
                   MOVE 2 TO ACTIVITY.

           IF ACTIVITY = ACTYP-COD
               GO TO NEW-ACTIVITY-EXIT.

              MOVE ACTIVITY TO ACTYP-COD

           IF PAGE11
              MOVE MTD-TYPE(ACTIVITY) TO MTD-BUFFER
              MOVE MTD-BUFFER TO WR-BUF
              MOVE MTD-FNAME TO WR-LIST
              PERFORM CALL-WRFORM
           ELSE
              MOVE ACT-TYPE(ACTIVITY) TO ACT-BUFFER
              MOVE ACT-BUFFER TO WR-BUF
              MOVE ACT-FNAME TO WR-LIST
              PERFORM CALL-WRFORM.

           PERFORM DISPLAY-PAGE.

       NEW-ACTIVITY-EXIT.
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
           PERFORM DISPLAY-PAGE.

       EXIT-TOGGLE-ON-DELINQ.
           EXIT.

      *****************************************************************
       SETUP-HEADER-DEL-TYPE SECTION.
      *****************************************************************

      * FOR PAGE 3

           MOVE HEADER-TYPE(HEADER-LEV) TO HEADER-BANKRUPT-STD.
           MOVE DEL-TYPE   (DELINQ)     TO HEADER-DEL-TYPE.
           MOVE HEADER-LAYOUT           TO HEADER-BUFFER.

       EXIT-SETUP-HEADER-DEL-TYP.
           EXIT.

      **************************************************************************
       FORM-RESET SECTION.
      **************************************************************************
           DISPLAY SPACES UPON BYMAIN-WINDOW-HANDLE
                               LINE 0 COL 0 ERASE SCREEN.
           EVALUATE PAGE-FLAG
              WHEN  1 MOVE EXT-CONAME-CONAME TO BYMAN1-CONAME
                      MOVE EXT-CONAME-SYSDATE TO BYMAN1-SYSDATE
                      DISPLAY BYMAN1-SCREEN UPON BYMAIN-WINDOW-HANDLE
                      MOVE BYMAN1-FORM-FIELDS TO WR-BUF 
                      MOVE BYMAN1-FORM-LIST   TO WR-LIST 
              WHEN  2 MOVE EXT-CONAME-CONAME TO BYMAN2-CONAME
                      MOVE EXT-CONAME-SYSDATE TO BYMAN2-SYSDATE
                      DISPLAY BYMAN2-SCREEN UPON BYMAIN-WINDOW-HANDLE
                      MOVE BYMAN2-FORM-FIELDS TO WR-BUF 
                      MOVE BYMAN2-FORM-LIST   TO WR-LIST 
              WHEN  3 MOVE EXT-CONAME-SYSDATE TO BYMAN3-SYSDATE
                      DISPLAY BYMAN3-SCREEN UPON BYMAIN-WINDOW-HANDLE
                      MOVE BYMAN3-FORM-FIELDS TO WR-BUF 
                      MOVE BYMAN3-FORM-LIST   TO WR-LIST 
              WHEN  4 MOVE EXT-CONAME-CONAME TO BYMAN4-CONAME
                      MOVE EXT-CONAME-SYSDATE TO BYMAN4-SYSDATE
                      DISPLAY BYMAN4-SCREEN UPON BYMAIN-WINDOW-HANDLE
                      MOVE BYMAN4-FORM-FIELDS TO WR-BUF 
                      MOVE BYMAN4-FORM-LIST   TO WR-LIST 
              WHEN  5 MOVE EXT-CONAME-CONAME TO BYMAN5-CONAME
                      MOVE EXT-CONAME-SYSDATE TO BYMAN5-SYSDATE
                      DISPLAY BYMAN5-SCREEN UPON BYMAIN-WINDOW-HANDLE
                      MOVE BYMAN5-FORM-FIELDS TO WR-BUF 
                      MOVE BYMAN5-FORM-LIST   TO WR-LIST 
              WHEN  6 MOVE EXT-CONAME-CONAME TO BYMAN6-CONAME
                      MOVE EXT-CONAME-SYSDATE TO BYMAN6-SYSDATE
                      DISPLAY BYMAN6-SCREEN UPON BYMAIN-WINDOW-HANDLE
                      MOVE BYMAN6-FORM-FIELDS TO WR-BUF 
                      MOVE BYMAN6-FORM-LIST   TO WR-LIST 
              WHEN  7 MOVE EXT-CONAME-CONAME TO BYMAN7-CONAME
                      MOVE EXT-CONAME-SYSDATE TO BYMAN7-SYSDATE
                      DISPLAY BYMAN7-SCREEN UPON BYMAIN-WINDOW-HANDLE
                      MOVE BYMAN7-FORM-FIELDS TO WR-BUF 
                      MOVE BYMAN7-FORM-LIST   TO WR-LIST 
              WHEN  8 MOVE EXT-CONAME-CONAME TO BYMAN8-CONAME
                      MOVE EXT-CONAME-SYSDATE TO BYMAN8-SYSDATE
                      DISPLAY BYMAN8-SCREEN UPON BYMAIN-WINDOW-HANDLE
                      MOVE BYMAN8-FORM-FIELDS TO WR-BUF 
                      MOVE BYMAN8-FORM-LIST   TO WR-LIST 
              WHEN  9 MOVE EXT-CONAME-CONAME TO BYMAN9-CONAME
                      MOVE EXT-CONAME-SYSDATE TO BYMAN9-SYSDATE
                      DISPLAY BYMAN9-SCREEN UPON BYMAIN-WINDOW-HANDLE
                      MOVE BYMAN9-FORM-FIELDS TO WR-BUF 
                      MOVE BYMAN9-FORM-LIST   TO WR-LIST 
              WHEN 10 MOVE EXT-CONAME-CONAME TO BYMANA-CONAME
                      MOVE EXT-CONAME-SYSDATE TO BYMANA-SYSDATE
                      DISPLAY BYMANA-SCREEN UPON BYMAIN-WINDOW-HANDLE
                      MOVE BYMANA-FORM-FIELDS TO WR-BUF 
                      MOVE BYMANA-FORM-LIST   TO WR-LIST 
              WHEN 11 MOVE EXT-CONAME-CONAME TO BYMANB-CONAME
                      MOVE EXT-CONAME-SYSDATE TO BYMANB-SYSDATE
                      DISPLAY BYMANB-SCREEN UPON BYMAIN-WINDOW-HANDLE
                      MOVE BYMANB-FORM-FIELDS TO WR-BUF 
                      MOVE BYMANB-FORM-LIST   TO WR-LIST 
              WHEN 12 MOVE EXT-CONAME-CONAME TO BYMANC-CONAME
                      MOVE EXT-CONAME-SYSDATE TO BYMANC-SYSDATE
                      DISPLAY BYMANC-SCREEN UPON BYMAIN-WINDOW-HANDLE
                      MOVE BYMANC-FORM-FIELDS TO WR-BUF 
                      MOVE BYMANC-FORM-LIST   TO WR-LIST 
              WHEN 13 MOVE EXT-CONAME-CONAME TO BYMAND-CONAME
                      MOVE EXT-CONAME-SYSDATE TO BYMAND-SYSDATE
                      DISPLAY BYMAND-SCREEN UPON BYMAIN-WINDOW-HANDLE
                      MOVE BYMAND-FORM-FIELDS TO WR-BUF 
                      MOVE BYMAND-FORM-LIST   TO WR-LIST 
              WHEN OTHER MOVE EXT-CONAME-CONAME TO BYMANE-CONAME
                      MOVE EXT-CONAME-SYSDATE TO BYMANE-SYSDATE
                      DISPLAY BYMANE-SCREEN UPON BYMAIN-WINDOW-HANDLE
                      MOVE BYMANE-FORM-FIELDS TO WR-BUF 
                      MOVE BYMANE-FORM-LIST   TO WR-LIST 
           END-EVALUATE.
           PERFORM CALL-WRFORM.
           
       FORM-RESET-EXIT.
           EXIT.

      **************************************************************************
       FORM-DESTROY SECTION.
      **************************************************************************

      * DESTROY FORM (SCREEN). PERFORMED PRIOR TO OPENING UP A NEW FORM

           EVALUATE PAGE-FLAG
              WHEN  1 DESTROY BYMAN1-SCREEN
              WHEN  2 DESTROY BYMAN2-SCREEN
              WHEN  3 DESTROY BYMAN3-SCREEN
              WHEN  4 DESTROY BYMAN4-SCREEN
              WHEN  5 DESTROY BYMAN5-SCREEN
              WHEN  6 DESTROY BYMAN6-SCREEN
              WHEN  7 DESTROY BYMAN7-SCREEN
              WHEN  8 DESTROY BYMAN8-SCREEN
              WHEN  9 DESTROY BYMAN9-SCREEN
              WHEN  10 DESTROY BYMANA-SCREEN
              WHEN  11 DESTROY BYMANB-SCREEN
              WHEN  12 DESTROY BYMANC-SCREEN
              WHEN  13 DESTROY BYMAND-SCREEN
              WHEN  OTHER DESTROY BYMANE-SCREEN
           END-EVALUATE.

      **************************************************************************
       SETUP-LINK-INFO SECTION.
      **************************************************************************
           EVALUATE PAGE-FLAG
              WHEN  1 MOVE BYMAN1-HELP--FILE TO HELP-LINK-FILE
                      MOVE BYMAN1-HELP--DIR  TO HELP-LINK-DIR
              WHEN  2 MOVE BYMAN2-HELP--FILE TO HELP-LINK-FILE
                      MOVE BYMAN2-HELP--DIR  TO HELP-LINK-DIR
              WHEN  3 MOVE BYMAN3-HELP--FILE TO HELP-LINK-FILE
                      MOVE BYMAN3-HELP--DIR  TO HELP-LINK-DIR
              WHEN  4 MOVE BYMAN4-HELP--FILE TO HELP-LINK-FILE
                      MOVE BYMAN4-HELP--DIR  TO HELP-LINK-DIR
              WHEN  5 MOVE BYMAN5-HELP--FILE TO HELP-LINK-FILE
                      MOVE BYMAN5-HELP--DIR  TO HELP-LINK-DIR
              WHEN  6 MOVE BYMAN6-HELP--FILE TO HELP-LINK-FILE
                      MOVE BYMAN6-HELP--DIR  TO HELP-LINK-DIR
              WHEN  7 MOVE BYMAN7-HELP--FILE TO HELP-LINK-FILE
                      MOVE BYMAN7-HELP--DIR  TO HELP-LINK-DIR
              WHEN  8 MOVE BYMAN8-HELP--FILE TO HELP-LINK-FILE
                      MOVE BYMAN8-HELP--DIR  TO HELP-LINK-DIR
              WHEN  9 MOVE BYMAN9-HELP--FILE TO HELP-LINK-FILE
                      MOVE BYMAN9-HELP--DIR  TO HELP-LINK-DIR
              WHEN 10 MOVE BYMANA-HELP--FILE TO HELP-LINK-FILE
                      MOVE BYMANA-HELP--DIR  TO HELP-LINK-DIR
              WHEN 11 MOVE BYMANB-HELP--FILE TO HELP-LINK-FILE
                      MOVE BYMANB-HELP--DIR  TO HELP-LINK-DIR
              WHEN 12 MOVE BYMANC-HELP--FILE TO HELP-LINK-FILE
                      MOVE BYMANC-HELP--DIR  TO HELP-LINK-DIR
              WHEN 13 MOVE BYMAND-HELP--FILE TO HELP-LINK-FILE
                      MOVE BYMAND-HELP--DIR  TO HELP-LINK-DIR
              WHEN OTHER MOVE BYMANE-HELP--FILE TO HELP-LINK-FILE
                      MOVE BYMANE-HELP--DIR  TO HELP-LINK-DIR
           END-EVALUATE.

      **************************************************************************
       VDU-CONVERT-FIELD SECTION.
      **************************************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.

      * FORCED TO BREAK APART EVALUATE STATEMENT.
      * TO MANY ARGUMENTS WERE CAUSING COMPILE ERROR.

           IF PAGE-FLAG < 10
              EVALUATE TRUE
                 COPY "LIBMN/BYMAN1_EVA.CPY".
                 COPY "LIBMN/BYMAN2_EVA.CPY".
                 COPY "LIBMN/BYMAN3_EVA.CPY".
                 COPY "LIBMN/BYMAN4_EVA.CPY".
                 COPY "LIBMN/BYMAN5_EVA.CPY".
                 COPY "LIBMN/BYMAN6_EVA.CPY".
                 COPY "LIBMN/BYMAN7_EVA.CPY".
                 COPY "LIBMN/BYMAN8_EVA.CPY".
                 COPY "LIBMN/BYMAN9_EVA.CPY".
                 WHEN OTHER       MOVE   "2A" TO STAT
              END-EVALUATE
            ELSE
              EVALUATE TRUE
                 COPY "LIBMN/BYMANA_EVA.CPY".
                 COPY "LIBMN/BYMANB_EVA.CPY".
                 COPY "LIBMN/BYMANC_EVA.CPY".
                 COPY "LIBMN/BYMAND_EVA.CPY".
                 COPY "LIBMN/BYMANE_EVA.CPY".
                 WHEN OTHER       MOVE   "2A" TO STAT
              END-EVALUATE
           END-IF.

      ******************************************************************
       LOAD-BRANCH-MODULE SECTION.
      ******************************************************************
     
           PERFORM OPEN-BR-FILE.
           PERFORM READ-BR-FILE.
           PERFORM CLOSE-BR-FILE.
     
           IF ( IO-BAD )
              MOVE SPACES                  TO BR-PATH
              GO TO LOAD-BRANCH-MODULE-EXIT.
     
           PERFORM LOAD-BRANCH-ACCESS.
     
       LOAD-BRANCH-MODULE-EXIT.
           EXIT.

      ******************************************************************
       LOAD-BRANCH-ACCESS SECTION.
      ******************************************************************
     
           MOVE EXT-FILPATH-BASE           TO FILPATH-BASE.
           MOVE BR-MACHINE                 TO FILPATH-MACHINE.
           MOVE BR-DATA-PATH               TO FILPATH-DATA.
     
           MOVE "B"                        TO BY-PATH-OVERRIDE.
           PERFORM LOAD-BY1-FILE.

       LOAD-BRANCH-ACCESS-EXIT.
           EXIT.


      *****************************************************************
      *
      *     C O P Y    L I B R A R Y    S E C T I O N S
      *
      *****************************************************************
       COPY "LIBLP/LPBYCL.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/GPENV.CPY".

      *****************************************************************
       MISC-PARAGRAPHS SECTION.
      *****************************************************************
       COPY "LIBGB/ACCESS.CPY".

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
           TO MESS.
           PERFORM SEND-MESS.

       ACTIVE-RECORD.
           MOVE "ACTIVE CUSTOMER CANNOT BE DELETED" TO MESS.
           PERFORM SEND-MESS.

       GET-TODAYS-DATE.
           MOVE EXT-CONAME-SYSDATE TO ALP-DATE
           MOVE ALP-MO TO DISPLAY-MO.
           MOVE ALP-YR TO DISPLAY-YR.
           MOVE DISPLAY-DATE TO WR-BUF.
           MOVE DATE-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       SWITCH-FORM.
           PERFORM WRITE-MODULE.
           MOVE BY-REC TO ORIGINAL-BY-REC.

           MOVE 0 TO NEW-FLAG
                     NEW-FLAG-SCR2
                     NEW-FLAG-SCR3
                     NEW-FLAG-SCR4
                     NEW-FLAG-SCR5
                     NEW-FLAG-SCR6
                     NEW-FLAG-SCR7
                     NEW-FLAG-SCR8
                     NEW-FLAG-SCR9
                     NEW-FLAG-SCR10
                     NEW-FLAG-SCR11
                     NEW-FLAG-SCR12
                     NEW-FLAG-SCR13
                     NEW-FLAG-SCR14.

           PERFORM FORM-DESTROY.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ACTION = "   P14"
              MOVE 1  TO ACTIVITY
              MOVE 14 TO PAGE-FLAG
           ELSE
           IF ACTION = "   P13"
              MOVE 1  TO PERIOD
              MOVE 13 TO PAGE-FLAG
           ELSE
           IF ACTION = "   P12"
              MOVE 1 TO ACTIVITY ACTYP-COD
              MOVE 12 TO PAGE-FLAG
           ELSE
           IF ACTION = "   P11"
              MOVE 1 TO ACTIVITY
                        ACTYP-COD
              MOVE 11 TO PAGE-FLAG
           ELSE
           IF ACTION = "   P10"
              MOVE 1 TO ACTIVITY
                        ACTYP-COD
              MOVE 10 TO PAGE-FLAG
           ELSE
           IF ACTION = "    P9"
              MOVE 1 TO ACTIVITY
                        ACTYP-COD
              MOVE 9 TO PAGE-FLAG
           ELSE
           IF ACTION = "    P8"
              MOVE 1 TO ACTIVITY
                        ACTYP-COD
              MOVE 8 TO PAGE-FLAG
           ELSE
           IF ACTION = "    P7"
              MOVE 7 TO PAGE-FLAG
           ELSE
           IF ACTION = "    P6"
              MOVE 6 TO PAGE-FLAG
           ELSE
           IF ACTION = "    P5"
              MOVE 5 TO PAGE-FLAG
           ELSE
           IF ACTION = "    P4"
              MOVE 4 TO PAGE-FLAG
           ELSE
           IF ACTION = "    P3"
              MOVE 1 TO DELINQ
                        DELTYP-COD
              MOVE 3 TO PAGE-FLAG
           ELSE
           IF ACTION = "    P2"
              MOVE 1 TO ACTIVITY
                        ACTYP-COD
              MOVE 2 TO PAGE-FLAG
           ELSE
           IF ACTION = "    P1"
              MOVE 1 TO PAGE-FLAG.

           MOVE FORM-NAMEX(PAGE-FLAG) TO FORM-NAM.
           MOVE FORM-VFN(PAGE-FLAG) TO VDU-FORM-NAME.
           MOVE FORM-PATH TO FORM-PATHNAME.
           PERFORM FORM-RESET.

      *KC  IF ( PAGE2 OR PAGE8 OR PAGE13)   
           IF ( PAGE2 OR PAGE8 )            
              MOVE "S  A020SRCD:S." TO VDU
              IF GP-THE-ONLY-SF-SRCD NOT = " "
                 MOVE GP-THE-ONLY-SF-SRCD TO VDUBUF
              ELSE
                 MOVE "RF" TO VDUBUF
              END-IF
              PERFORM SCREENIO

              MOVE "S  A020SCDM:S." TO VDU
              IF GP-NO-SC-USE-DM = "Y"
                 MOVE "DM" TO VDUBUF
              ELSE
                 MOVE "SC" TO VDUBUF
              END-IF
              PERFORM SCREENIO.

           MOVE HOLD-BY-BRNO TO DISP-BRNO.
           MOVE HOLD-BRNAME TO DISP-BRNAME.
           MOVE HOLD-BY-CLASS TO DISP-LC.
           MOVE HOLD-LCDESC TO DISP-LCDESC.
           MOVE DISPLAY-KEY TO WR-BUF.
           MOVE DISP1-KEY-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

           MOVE DISPLAY-DATE TO WR-BUF.
           MOVE DATE-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.
           PERFORM DISPLAY-PAGE.

       SCAN-MESS.
           MOVE "SCAN ONLY ON 'CLASS'" TO MESS.
           PERFORM SEND-MESS.

      *****************************************************************
       IO-ROUTINES SECTION.
      *****************************************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/DECLARE_SQL.CPY".
           PERFORM CLOSE-LC1-FILE.
           PERFORM CLOSE-BY1-FILE.
           PERFORM CLOSE-BR-FILE.
       COPY "LIBLP/LPLCGS_SQL.CPY".
       COPY "LIBLP/LPBYGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBLP/LPLC1RN.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBLP/LPBY1IN.CPY".
       COPY "LIBGB/FERRORS.CPY".

      *****************************************************************
       END-ROUTINE SECTION.
      *****************************************************************
       END-RUN.
           PERFORM CLOSE-FILES.

       EXIT-PROG.
           MOVE "LP/LPM2MU" TO FORM-NAM.
           MOVE FORM-PATH TO FORM-PATHNAME.

       STOP-RUN.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN  
           DESTROY BYMAIN-WINDOW-HANDLE
           MOVE FORM-PATH TO FORM-PATHNAME.
           EXIT PROGRAM.
