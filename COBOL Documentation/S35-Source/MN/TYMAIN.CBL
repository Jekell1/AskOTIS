       IDENTIFICATION DIVISION.
       PROGRAM-ID.      TYMAIN.
       DATE-WRITTEN.    050495.
      ******************************************************************
      *
      *                 (MN)
      *  DESCRIPTION: TODAY'S TOTALS FILE MAINTENANCE
      *
      *  REV
      *   MJD 061495 ADDED TY-CKWRITTEN(1 - 8) TO PAGE 5.
      *   MJD 062095 ADDED OTHER 2 INFO TO PG 1 AND PG 4.
      *   MJD 081495 ADDED LOGIC FOR GP-THE-ONLY-SF-SRCD.
      *   KEC 110395 FIXED F6-SCAN TO CROSS BRANCH PATHS.
      *   MJD 111095 COMMENTED OUT DISPLAY/ACCESS TO VOLUME PORTION
      *              OF DELINQUENCY TABLE.
      *   MJD 012296 ADDED O2 AND INTEREST TO PERIOD AND REPO TABLE.
      *              ADDED PL TABLE, DEALER RESERVE BUCKETS AND
      *              SOLD-TRANSFERRED/PURCHASED LOANS.
      *              CHANGED SIZE OF HOLD RECS TO ACCOMODATE NEW RECORD SIZE.
      *   MJD 012496 ADDED PL PRIN/INT/LATE COLLECTED.
      *              MODIFIED NEW PAGE SELECTION TO MAKE IT MORE EFFICIENT.
      *              USED LOGIC FROM SPMANX AND MODIFIED IT.
      *  MJD 012696 CHANGED DLR RESERVE INTO A NET GAIN/LOSS-VOLUME TABLE
      *             ON PAGR 6. MOVED LOANS SOLD-TRANS-PURCH TO PAGE 5.
      *             ADDED O2COL TO PAGE 8.
      *  JFF 110596 FIXED ARROW UP ON PAGE 3.
      *  KEC 030797 CHANGED PAGE4: AVAILABLE SPACE ON #2 - #4 (OLD APP)
      *             ADDED   PAGE9: DISPLAYING NEW APPLICATION STATS.
      *  JTG 032097 ADDED INFO FOR TY-NO-TURNUSDOWN
      *  MJD 071497 ADDED O3 INSURANCE TO PAGE 5.
      *             ADDED O3 INSURANCE REBATED TO PAGE 5.
      *  GLF 090997 REPLACED 'IO' ON GP-FILE WITH GET 'GPENV'
      *  MJD 091297 CHANGED PAGE 3 TO DISPLAY
      *             CUR/30/60/90/120/150/180/210+/<P>/<M>
      *  MJD 100897 CHANGED RECORD SIZE TO 2253. EXPANDED ACTIVITY AND REPO
      *             COUNTERS TO 9(5) AND AMOUNTS TO 9(8)V99.
      *  MJD 030598 ADDED PAGE 10, DISPLAYS IN NO ELIG/WROTE/NOT WROTE
      *             AND AMT WROTE
      *  MJD 031298 EXPANDED TY-SUMWGTTM AND TY-SUMWGTTA
      *  KEC 032498 USED THE EMPTY BUCKETS 10 & 11 IN
      *              '[B/T]Y-PERIOD-TBL' & '[B/T]Y-APP-PERIOD-TBL' FOR
      *              NEW SOURCE CODES 'RB' & 'RO'.
      *  MJD 050898 FIXED BUG WITH PAGE 10. GETTING MISSING FIELD MESSAGE.
      *  MJD 051998 REMOVED REFERENCE TO TY-PASTDUE.
      *  KEC 061198 ADDED BANKRUPTCY-DELINQUENCY-TABLE TO PAGE 3.
      *             ADDED TY-PART-CHGOFF TO PAGE 7 & IS STILL ON PAGE 1.
      * MJD 990723 ADDES SIGN TO DELINQUENCY FIELDS ON PAGE 3.
      * MJD 000323 ADDED TY-UNEARNED-AMT (MERC #223)
      * MJD 000330 ADDED TY-PROCEEDS (MERC #223)
      * MJD 000929 ADDED TY-LOCKBOX (MERC #237)
      * JTG 030318 ADDED GP-NO-SC-USE-DM -NO SALESFIN USE DIRECT MAIL WORLD #307
      * MJD 131121 A30 CONVERSION COMPLETED
      * BLM 160316 FIX FOR NON-FILE, AD&D, & ROADGARD, NOW 1-3 OF TY-FEE TABLE
      *            INSTEAD OF 8-10 OF TY-INS TABLE, PD0003
      * BLM 160331 EXPAND TY-CKWRITTEN TABLE FROM 8 TO 16.  ADD FIELDS TO
      *            PAGE 5, GET RID OF INS ON PAGE 5 SINCE IT'S ALL ON 
      *            10 ANYWAY, PD0003
      *
      * KEC 2016.1024 {PD#00003} PARADATA <A30> [JAY CISZEWSKI]
      *         RENAMED OF ORIGINAL-TY-REC & CHANGED-TY-REC.
      *         REMOVED USE OF DISPLAY-DATE (CALL WRFORM NOT VALID;
      *          FIELD LIST 'MMDDYY' WAS NOT ON ANY SCREENS).
      *         CREATED CURR-TY-DATE AND CURR-TY-DATE-MMDDYY TO
      *          REPLACE THE USE OF DISPLAY-DATE.
      * CS 170511 ACTIVATE 8 DIGIT DATES, ALSO FIXED HOLD-TY1-KEY FROM13 TO 15
      *           AND WORK-TY1-KEY FROM 13 TO 15
      * 20171030 BAH ACTIVATE 8 DIGIT DATES, PD0006, REMOVED CURR-TY-DATES
      *
      * KEC 2018.0323 {PD#00003} PARADATA <A30> [JAY CISZEWSKI]
      *         LORI FOUND AN ISSUE WITH PAGE 10 WHERE SHE COULD
      *         NOT GET ACCESS TO FIELDS 11 - 13.  TURNS OUT
      *         SOMEONE FORGOT TO CHANGE SPEC-TABLE1 AND MAX10.
      *
      * BAH 2018.0503 SOMEONE CHANGE THE FORM FIELD "LC" TO N(3) BUT DIDNT
      *               CHANGE DISP-LC TO BE 9(3).
      * BAH 181017 GLOBAL TYFILE
      * KEC 190218 A32 F2SL - REVISED FOR SPLITTING THE BYFILE.
      *            REMOVED LOAN APP FIELDS IN BY & TY.
      *  CS 190515 MISSING FIELD ERROR, J-16, MISSING GO TO CREATE-ROUTINE
      *                                                  QA0015
      * BAH 20190930 REMOVED ACCESS-CALL TYFILE
      *  CS 20190102 FIXED SCAN-KEY, ADDED "B" TO TY-PATH-OVERRIDE 
      *  CS 20190110 IN A15, YOU COULD ENTER ANY BRANCH IN KEY ENTRY AND
      *              WOULD GET ACCURATE INFO, BUT IF YOU TRIED TO SCAN AND
      *              ENTERED A BRANCH OTHER THAN YOUR OWN, NO GOOD.
      *              ALLOWED THE SCAN TO WORK FOR OTHER BRANCHES IN A32
      *  CS 20200217 FORCE THEM TO ENTER A BRANCH, AND STOP WHEN THE
      *              TY-BRNO IS NOT = PATH-OWNBR
      * BLM 20211129 IN SWITCH-FORM, SAVE REC TO ORIG AFTER WRITE SINCE
      *              TOUCH DATE CHANGES, CAUSED REC NOT = ORIG-REC:F
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


       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)MN/TYMAIN.CBL S35 02/21/24-12:34:23 >".

      ******************************************************************
      *
      *    F I L E - R E C O R D - W O R K E R S
      *
      ******************************************************************
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBLP/LP01LC.CPY".
       COPY "LIBLP/LP01LC_SQL.CPY".
       COPY "LIBLP/LPWSLC.CPY".
       COPY "LIBLP/LP01TY.CPY".
       COPY "LIBLP/LP01TY_SQL.CPY".
       COPY "LIBLP/LPWSTY.CPY".

       01  ORIGINAL-TY-REC             PIC X(TY-SIZE).
       01  CHANGED-TY-REC              PIC X(TY-SIZE).

      *-----------------------------------------------------------------
       01  HOLD-TY1-KEY                PIC 9(15).
       01  FILLER            REDEFINES HOLD-TY1-KEY.
           03  HOLD-TY-BRNO            PIC 9(4).
           03  HOLD-TY-DATE            PIC 9(8).
           03  HOLD-TY-CLASS           PIC 9(3).

       01  WORK-TY1-KEY                PIC 9(9).
       01  FILLER            REDEFINES WORK-TY1-KEY.
           03  WORK-TY-DATE            PIC 9(6).
           03  WORK-TY-CLASS           PIC 9(3).

      ******************************************************************
      *
      *    M I S C E L L A N E O U S - W O R K E R S
      *
      ******************************************************************
       01  ERRCD           PIC X       VALUE " ".
       01  SUB1            PIC 9(2)    VALUE 0.
       01  SUB2            PIC 9(2)    VALUE 0.
       01  SUB3            PIC 9(2)    VALUE 0.
       01  SUB5            PIC 9(2)    VALUE 0.
       01  SUB6            PIC 9(2)    VALUE 0.
       01  SUB7            PIC 9(2)    VALUE 0.

       01  CHAN            PIC X(4)    VALUE "CHAN".
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
       01  ELEMENT         PIC 9(2).
       01  ACTION                VALUE SPACES.
           03  CHKEY             PIC X(4).
           03  CHNO              PIC 9(2).
           03  CHNO-X           REDEFINES CHNO
                                 PIC X(2).
      *  FORM SELECT FOR PAGES 1 - 9.
       01  FORM-NUM1            REDEFINES ACTION.
           03  FORM-KEY1         PIC X(4).
           03  FORM-P1           PIC X.
           03  FORM-SEL1         PIC 9.
               88 FORM-1-DIGIT   VALUE 1 2 3 4 5 6 7 8 9.
      *  FORM SELECT FOR P10 - P11.
        01  FORM-NUM2            REDEFINES ACTION.
            03  FORM-KEY2         PIC X(3).
            03  FORM-P2           PIC X.
            03  FORM-SEL2         PIC 99.
                88 FORM-2-DIGITS  VALUE 10 11.

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
       01  FIRST-SCAN      PIC 9 VALUE 0.

       01  HOLD-BRNAME     PIC X(20).
       01  HOLD-LCDESC     PIC X(20).

      *-----------------------------------------------------------------
       01  HEADER-BUFFER               PIC X(14)     VALUE SPACES.
       01  HEADER-LIST                 PIC X(07)     VALUE "HEADER.".

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

      *-----------------------------------------------------------------
       01  FORM-BUF.
           03  FORM-1       PIC X(9) VALUE "MN/TYMAN1".
           03  FORM-2       PIC X(9) VALUE "MN/TYMAN2".
           03  FORM-3       PIC X(9) VALUE "MN/TYMAN3".
           03  FORM-4       PIC X(9) VALUE "MN/TYMAN4".
           03  FORM-5       PIC X(9) VALUE "MN/TYMAN5".
           03  FORM-6       PIC X(9) VALUE "MN/TYMAN6".
           03  FORM-7       PIC X(9) VALUE "MN/TYMAN7".
           03  FORM-8       PIC X(9) VALUE "MN/TYMAN8".
           03  FORM-9       PIC X(9) VALUE "MN/TYMAN9".
           03  FORM-10      PIC X(9) VALUE "MN/TYMANA".
           03  FORM-11      PIC X(9) VALUE "MN/TYMANB".
       01  FORM-BUFX REDEFINES FORM-BUF.
           03  FORM-NAMEX OCCURS 11.
               05 FORM-DIR   PIC X(2).
               05 FORM-SLASH PIC X(1).
               05 FORM-VFN  PIC X(6).

       01  DISPLAY-KEY.
           03  DISP-BRNO        PIC 9(4)  VALUE 0.
           03  DISP-BRNAME      PIC X(20) VALUE " ".
           03  DISP-LC          PIC 9(3)  VALUE 0.
           03  DISP-LCDESC      PIC X(20) VALUE " ".
       01  DISP1-KEY-LIST.
           03  FILLER     PIC X(18) VALUE
           "BRANCH BRX LC LCX.".

      *    **********************************
      *    *      SCREEN DISPLAY BUFFER
      *    **********************************
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
           03  F17     PIC ZZ,ZZZ,ZZ9.99-.
           03  F18     PIC ZZ,ZZZ,ZZ9.99-.
           03  F19     PIC ZZ,ZZZ,ZZ9.99-.
           03  F20     PIC ZZ,ZZZ,ZZ9.99-.
           03  F21     PIC ZZ,ZZZ,ZZ9.99-.
           03  F22     PIC ZZ,ZZZ,ZZ9.99-.
           03  F23     PIC ZZ,ZZZ,ZZ9.99-.
           03  F24     PIC ZZ,ZZZ,ZZ9.99-.
           03  F25     PIC ZZ,ZZZ,ZZ9.99-.
           03  F26     PIC X.

       01  DISPLAY-LIST1.
           03  FILLER     PIC X(32) VALUE
               "010 020 030 040 050 060 070 080 ".
           03  FILLER     PIC X(32) VALUE
               "090 100 110 120 130 140 150 160 ".
           03  FILLER     PIC X(32) VALUE
               "170 180 190 200 210 220 230 240 ".
           03  FILLER     PIC X(8)  VALUE
               "250 260.".

       01  DISPLAY-BUF2.
           03  DATA-TYPE-TABLE  OCCURS 11.
               05  DATA-NO  PIC ZZZZ9-.
               05  DATA-AMT PIC ZZZZZZZ9.99-.
               05  DATA-O2  PIC ZZZZZZZ9.99-.
               05  DATA-INT PIC ZZZZZZZ9.99-.
           03  DATA-120         PIC ZZ,ZZZ,ZZ9-.
           03  DATA-130         PIC ZZZ,ZZZ,ZZ9.99-.
           03  DATA-140         PIC ZZ,ZZZ,ZZZ,ZZ9.99-.

       01  DISPLAY-LIST2.
           03  FILLER       PIC X(48) VALUE
           "010 011 012 013 020 021 022 023 030 031 032 033 ".
           03  FILLER       PIC X(48) VALUE
           "040 041 042 043 050 051 052 053 060 061 062 063 ".
           03  FILLER       PIC X(48) VALUE
           "070 071 072 073 080 081 082 083 090 091 092 093 ".
           03  FILLER       PIC X(44) VALUE
           "100 101 102 103 110 111 112 113 120 130 140.".

       01  DISPLAY-BUF3.
           03  DEL-TABLE               OCCURS 10.
               05  DEL-REC-CON         OCCURS 2.
                   07  DEL-UNITS       PIC Z(6)9-.
                   07  DEL-OUTBAL      PIC Z(8)9.99-.

       01  DISPLAY-LIST3.
           03  FILLER       PIC X(24) VALUE
           "010 011 012 013 ".
           03  FILLER       PIC X(24) VALUE
           "020 021 022 023 ".
           03  FILLER       PIC X(24) VALUE
           "030 031 032 033 ".
           03  FILLER       PIC X(24) VALUE
           "040 041 042 043 ".
           03  FILLER       PIC X(24) VALUE
           "050 051 052 053 ".
           03  FILLER       PIC X(24) VALUE
           "060 061 062 063 ".
           03  FILLER       PIC X(24) VALUE
           "070 071 072 073 ".
           03  FILLER       PIC X(24) VALUE
           "080 081 082 083 ".
           03  FILLER       PIC X(24) VALUE
           "090 091 092 093 ".
           03  FILLER       PIC X(24) VALUE
           "100 101 102 103.".

       01  DISPLAY-BUF4    VALUE " ".
           03  I01     PIC ZZ,ZZZ,ZZ9.99-.
           03  I02     PIC ZZ,ZZZ,ZZ9.99-.
           03  I03     PIC ZZ,ZZZ,ZZ9.99-.
           03  I04     PIC ZZ,ZZZ,ZZ9-.
           03  I05     PIC ZZ,ZZZ,ZZ9-.
           03  I06     PIC ZZ,ZZZ,ZZ9-.
           03  I07     PIC ZZ,ZZZ,ZZ9-.
           03  I08     PIC ZZ,ZZZ,ZZ9-.
           03  I09     PIC ZZ,ZZZ,ZZ9-.
           03  I10     PIC ZZ,ZZZ,ZZ9-.
           03  I11     PIC ZZ,ZZZ,ZZ9-.
           03  I12     PIC ZZ,ZZZ,ZZ9.99-.
           03  I13     PIC ZZ,ZZZ,ZZ9.99-.
           03  I14     PIC ZZ,ZZZ,ZZ9.99-.
           03  I15     PIC ZZ,ZZZ,ZZ9.99-.

       01  DISPLAY-LIST4.
           03  FILLER     PIC X(32) VALUE
               "010 020 030 040 050 060 070 080 ".
           03  FILLER     PIC X(28) VALUE
               "090 100 110 120 130 140 150.".

       01  DISPLAY-BUF5    VALUE " ".
            03 CHECKX-WRITTEN     OCCURS 16.
               05 CHECKS-WRITTEN     PIC Z,ZZZ,ZZ9.99-.
           03  DATA-NO-SOLD        PIC ZZZZ9-.
           03  DATA-AMT-SOLD       PIC ZZZZZZZ9.99-.
           03  DATA-NO-TRANS       PIC ZZZZ9-.
           03  DATA-AMT-TRANS      PIC ZZZZZZZ9.99-.
           03  DATA-NO-PURCH       PIC ZZZZ9-.
           03  DATA-AMT-PURCH      PIC ZZZZZZZ9.99-.

       01  DISPLAY-LIST5.
           03  FILLER     PIC X(32) VALUE
               "010 020 030 040 050 060 070 080 ".
           03  FILLER     PIC X(32) VALUE
               "090 100 110 120 130 140 150 160 ".
           03  FILLER     PIC X(24) VALUE
               "170 171 180 181 190 191.".

       01  DISPLAY-BUF6.
           03  DATA-TYPE-TABLE     OCCURS 5.
               05  DATA-NO-REPO    PIC ZZZZ9-.
               05  DATA-AMT-REPO   PIC ZZZZZZZ9.99-.
               05  DATA-O2-REPO    PIC ZZZZZZZ9.99-.
               05  DATA-INT-REPO   PIC ZZZZZZZ9.99-.
           03  DATA-DLRVPRIN       OCCURS 3
                                   PIC ZZZZZZZ9.99-.
           03  DATA-DLRVCHGS       OCCURS 3
                                   PIC ZZZZZZZ9.99-.
           03  DATA-DLPARVHELD     PIC ZZZZZZZ9.99-.
           03  DATA-DLDISC         PIC ZZZZZZZ9.99-.

       01  DISPLAY-LIST6.
           03  FILLER       PIC X(48) VALUE
           "010 011 012 013 020 021 022 023 030 031 032 033 ".
           03  FILLER       PIC X(32) VALUE
           "040 041 042 043 050 051 052 053 ".
           03  FILLER       PIC X(32) VALUE
           "060 061 062 070 071 072 080 090.".


       01  DISPLAY-BUF7.
           03  DATA-TYPE-TABLE-PL  OCCURS 10.
               05  DATA-NO-PL      PIC ZZZZ9-.
               05  DATA-AMT-PL     PIC ZZZZZZZ9.99-.
               05  DATA-O2-PL      PIC ZZZZZZZ9.99-.
               05  DATA-INT-PL     PIC ZZZZZZZ9.99-.
           03  DATA-PART-CHGOFF    PIC ZZ,ZZZ,ZZ9.99-.

       01  DISPLAY-LIST7.
           03  FILLER       PIC X(48) VALUE
           "010 011 012 013 020 021 022 023 030 031 032 033 ".
           03  FILLER       PIC X(48) VALUE
           "040 041 042 043 050 051 052 053 060 061 062 063 ".
           03  FILLER       PIC X(48) VALUE
           "070 071 072 073 080 081 082 083 090 091 092 093 ".
           03  FILLER       PIC X(20) VALUE
           "100 101 102 103 110.".

       01  DISPLAY-BUF8.
           03  DATA-TYPE-TABLE-PL  OCCURS 5.
               05  DATA-PL-PRINCOL  PIC ZZ,ZZZ,ZZ9.99-.
               05  DATA-PL-INTCOL   PIC ZZ,ZZZ,ZZ9.99-.
               05  DATA-PL-LATECOL  PIC ZZ,ZZZ,ZZ9.99-.
               05  DATA-PL-O2COL    PIC ZZ,ZZZ,ZZ9.99-.

       01  DISPLAY-LIST8.
           03  FILLER       PIC X(40) VALUE
           "010 011 020 021 030 031 040 041 050 051 ".
           03  FILLER       PIC X(40) VALUE
           "060 061 070 071 080 081 090 091 100 101.".

      * GNLS = GAIN/LOSS     NVOL = NEW VOLUME
      
       01  DISPLAY-BUF9.
           03  DB9-F01   PIC X(1). 

       01  DISPLAY-LIST9.
           03  FILLER PIC X(04) VALUE "010.".  

      * SCREEN DISPLAY BUFFER ----- PAGE 10
      
       01  DISPLAY-BUF10.
           05  DATA-INS-TABLE             OCCURS 10.
               07  DATA-INS-NO-ELIG       PIC ZZZZZ9-.
               07  DATA-INS-NO-WROTE      PIC ZZZZZ9-.
               07  DATA-INS-NO-NOTWROTE   PIC ZZZZZ9-.
               07  DATA-INS-AMT-WROTE     PIC ZZZZZZZ9.99-.
           05  DATA-FEE-TABLE             OCCURS 3.
               07  DATA-FEE-NO-ELIG       PIC ZZZZZ9-.
               07  DATA-FEE-NO-WROTE      PIC ZZZZZ9-.
               07  DATA-FEE-NO-NOTWROTE   PIC ZZZZZ9-.
               07  DATA-FEE-AMT-WROTE     PIC ZZZZZZZ9.99-.

       01  DISPLAY-LIST10.
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
               "130 131 132 133.".

      *
      *    SCREEN DISPLAY BUFFER ----- PAGE 11
      *
       01  DISPLAY-BUF11.
           05  DATA-UNE-TABLE        OCCURS 6.
               07  DATA-UNE-TABLEB   OCCURS 2.
                   09 P11-DISP-AMT    PIC ZZ,ZZZ,ZZZ.ZZ-  BLANK ZERO.

       01  DISPLAY-LIST11.
           03  FILLER                     PIC X(24)     VALUE
               "010 011 020 021 030 031 ".
           03  FILLER                     PIC X(24)     VALUE
               "040 041 050 051 060 061.".

      *    **********************************
      *    *   ELEMENT SPECIFICATIONS #1
      *    **********************************
       01  MAX              PIC 99  VALUE 0.
       01  MAX1             PIC 99  VALUE 26.
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
           03  FILLER      PIC X(8) VALUE "E NS082 ".
           03  FILLER      PIC X(8) VALUE "E NS082 ".
           03  FILLER      PIC X(8) VALUE "E NS082 ".
           03  FILLER      PIC X(8) VALUE "E NS082 ".
           03  FILLER      PIC X(8) VALUE "E NS082 ".
           03  FILLER      PIC X(8) VALUE "E NS082 ".
           03  FILLER      PIC X(8) VALUE "E NS082 ".
           03  FILLER      PIC X(8) VALUE "E NS082 ".
           03  FILLER      PIC X(8) VALUE "E NS082 ".
           03  FILLER      PIC X(8) VALUE "E NA010 ".

       01  SPEC-TAB1       REDEFINES SPEC-TABLE1.
           03  SPEC-REC1   OCCURS 26 TIMES.
               05  SPEC1   PIC X(7).
               05  CHFG1   PIC X.

      *    **********************************
      *    *   ELEMENT SPECIFICATIONS #2
      *    **********************************
       01  MAX2             PIC 99   VALUE 14.
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
       01  SPEC-TAB2       REDEFINES SPEC-TABLE2.
           03  SPEC-REC2   OCCURS 14 TIMES.
               05  SPEC2   PIC X(7).
               05  CHFG2   PIC X.

      *    **********************************
      *    *   ELEMENT SPECIFICATIONS #3
      *    **********************************
       01  MAX3           PIC 99   VALUE 10.
       01  SPEC-TABLE3.
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
      *     03  FILLER     PIC X(8) VALUE "ENNS070 ".
      *     03  FILLER     PIC X(8) VALUE "ENNS070 ".
      *     03  FILLER     PIC X(8) VALUE "ENNS070 ".
      *     03  FILLER     PIC X(8) VALUE "ENNS070 ".
      *     03  FILLER     PIC X(8) VALUE "ENNS070 ".
      *     03  FILLER     PIC X(8) VALUE "ENNS070 ".
      *     03  FILLER     PIC X(8) VALUE "ENNS070 ".
      *     03  FILLER     PIC X(8) VALUE "ENNS070 ".

       01  SPEC-TAB3       REDEFINES SPEC-TABLE3.
           03  SPEC-REC3   OCCURS 10 TIMES.
               05  SPEC3   PIC X(7).
               05  CHFG3   PIC X.

       01  MAX4            PIC 99  VALUE 15.
       01  SPEC-TABLE4.
           03  FILLER      PIC X(8) VALUE "E NS082 ".
           03  FILLER      PIC X(8) VALUE "E NS082 ".
           03  FILLER      PIC X(8) VALUE "E NS082 ".
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
       01  SPEC-TAB4       REDEFINES SPEC-TABLE4.
           03  SPEC-REC4   OCCURS 15 TIMES.
               05  SPEC4   PIC X(7).
               05  CHFG4   PIC X.


       01  MAX5            PIC 99  VALUE 19.
       01  SPEC-TABLE5.
      * - CHECKS WRITTEN
           03  FILLER      PIC X(8) VALUE "E NS072 ".
           03  FILLER      PIC X(8) VALUE "E NS072 ".
           03  FILLER      PIC X(8) VALUE "E NS072 ".
           03  FILLER      PIC X(8) VALUE "E NS072 ".
           03  FILLER      PIC X(8) VALUE "E NS072 ".
           03  FILLER      PIC X(8) VALUE "E NS072 ".
           03  FILLER      PIC X(8) VALUE "E NS072 ".
           03  FILLER      PIC X(8) VALUE "E NS072 ".
           03  FILLER      PIC X(8) VALUE "E NS072 ".
           03  FILLER      PIC X(8) VALUE "E NS072 ".
           03  FILLER      PIC X(8) VALUE "E NS072 ".
           03  FILLER      PIC X(8) VALUE "E NS072 ".
           03  FILLER      PIC X(8) VALUE "E NS072 ".
           03  FILLER      PIC X(8) VALUE "E NS072 ".
           03  FILLER      PIC X(8) VALUE "E NS072 ".
           03  FILLER      PIC X(8) VALUE "E NS072 ".
      * - LOANS SOLD/TRANS/PURCH
           03  FILLER      PIC X(8) VALUE "ENNS050 ".
           03  FILLER      PIC X(8) VALUE "ENNS050 ".
           03  FILLER      PIC X(8) VALUE "ENNS050 ".

       01  SPEC-TAB5       REDEFINES SPEC-TABLE5.
           03  SPEC-REC5   OCCURS 19 TIMES.
               05  SPEC5   PIC X(7).
               05  CHFG5   PIC X.

      *    **********************************
      *    *   ELEMENT SPECIFICATIONS #6
      *    **********************************
       01  MAX6             PIC 99   VALUE 9.
       01  SPEC-TABLE6.
           03  FILLER     PIC X(8) VALUE "ENNS050 ".
           03  FILLER     PIC X(8) VALUE "ENNS050 ".
           03  FILLER     PIC X(8) VALUE "ENNS050 ".
           03  FILLER     PIC X(8) VALUE "ENNS050 ".
           03  FILLER     PIC X(8) VALUE "ENNS050 ".
           03  FILLER     PIC X(8) VALUE "ENNS082 ".
           03  FILLER     PIC X(8) VALUE "ENNS082 ".
           03  FILLER     PIC X(8) VALUE "ENNS082 ".
           03  FILLER     PIC X(8) VALUE "ENNS082 ".

       01  SPEC-TAB6       REDEFINES SPEC-TABLE6.
           03  SPEC-REC6   OCCURS 9 TIMES.
               05  SPEC6   PIC X(7).
               05  CHFG6   PIC X.

      *    **********************************
      *    *   ELEMENT SPECIFICATIONS #7
      *    **********************************
       01  MAX7             PIC 99   VALUE 11.
       01  SPEC-TABLE7.
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
       01  SPEC-TAB7       REDEFINES SPEC-TABLE7.
           03  SPEC-REC7   OCCURS 11 TIMES.
               05  SPEC7   PIC X(7).
               05  CHFG7   PIC X.

      *    **********************************
      *    *   ELEMENT SPECIFICATIONS #8
      *    **********************************
       01  MAX8             PIC 99  VALUE 10.
       01  SPEC-TABLE8.
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

       01  SPEC-TAB8       REDEFINES SPEC-TABLE8.
           03  SPEC-REC8   OCCURS 10 TIMES.
               05  SPEC8   PIC X(7).
               05  CHFG8   PIC X.

      ******************************************************************
      *    E L E M E N T - S P E C I F I C A T I O N S   # 9
      ******************************************************************
       01  MAX9             PIC 99   VALUE 01.
       01  SPEC-TABLE9.
           03  FILLER       PIC X(8) VALUE "E  A010 ". 
       01  SPEC-TAB9        REDEFINES SPEC-TABLE9.
           03  SPEC-REC9    OCCURS 01 TIMES.
               05  SPEC9    PIC X(7).
               05  CHFG9    PIC X.

      *    ELEMENT SPECIFICATIONS ----- PAGE 10
       01  MAX10            PIC 99   VALUE 13.
       01  SPEC-TABLE10.
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
       01  SPEC-TAB10      REDEFINES SPEC-TABLE10.
           03  SPEC-REC10  OCCURS 13 TIMES.
               05  SPEC10  PIC X(7).
               05  CHFG10  PIC X.

      *    ELEMENT SPECIFICATIONS ----- PAGE 11
       78  MAX11                     VALUE 6.
       01  SPEC-TABLE11.
           03  FILLER     PIC X(8) VALUE "ENNS082 ".
           03  FILLER     PIC X(8) VALUE "ENNS082 ".
           03  FILLER     PIC X(8) VALUE "ENNS082 ".
           03  FILLER     PIC X(8) VALUE "ENNS082 ".
           03  FILLER     PIC X(8) VALUE "ENNS082 ".
           03  FILLER     PIC X(8) VALUE "ENNS082 ".
       01  SPEC-TAB11     REDEFINES SPEC-TABLE11.
           03  SPEC-REC11  OCCURS MAX11 TIMES.
               05  SPEC11  PIC X(7).
               05  CHFG11  PIC X.

      *     **************************************************
      *     *       HOLD/WORK AREAS
      *     **************************************************
       01  ACT-TYPE-TABLE.
           03  FILLER     PIC X(21) VALUE
           "*** NET GAIN/LOSS ***".
           03  FILLER     PIC X(21) VALUE
           "*** NEW VOLUME ***   ".
           03  FILLER     PIC X(43) VALUE
           "                    TODATE       ".
           03  FILLER     PIC X(37) VALUE
           "              TODATE                ".
           03  FILLER     PIC X(43) VALUE
           "                    TODATE       ".
           03  FILLER     PIC X(37) VALUE
           "              TODATE                ".
       01  ACT-TYPES REDEFINES ACT-TYPE-TABLE.
           03  ACT-TYPE   PIC X(21) OCCURS 2 TIMES.
           03  ACT-TYPEL  PIC X(80) OCCURS 2 TIMES.

       01  ACT-FNAME      PIC X(9)  VALUE "ACT_TYPE.".
       01  ACT-FNAMEL     PIC X(11) VALUE "ACT_LINE:P.".
       01  ACT-BUFFER     PIC X(80) VALUE " ".
       01  ACTIVITY       PIC 9 VALUE 1.
       01  ACTYP-COD      PIC 9 VALUE ZERO.

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

      ******************************************************************
      *
      *    C O P Y - L I B R A R Y - W O R K E R S
      *
      ******************************************************************
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBLP/LPSETTYW.CPY".
       COPY "LIBGB/DATERW.CPY".
       01 TYMAIN-WINDOW-HANDLE PIC X(10) VALUE SPACES.
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBMN/TYMAN1_DEF.CPY".
       COPY "LIBMN/TYMAN2_DEF.CPY".
       COPY "LIBMN/TYMAN3_DEF.CPY".
       COPY "LIBMN/TYMAN4_DEF.CPY".
       COPY "LIBMN/TYMAN5_DEF.CPY".
       COPY "LIBMN/TYMAN6_DEF.CPY".
       COPY "LIBMN/TYMAN7_DEF.CPY".
       COPY "LIBMN/TYMAN8_DEF.CPY".
       COPY "LIBMN/TYMAN9_DEF.CPY".
       COPY "LIBMN/TYMANA_DEF.CPY".
       COPY "LIBMN/TYMANB_DEF.CPY".
       COPY "LIBMN/TYMAN1_WKS.CPY".
       COPY "LIBMN/TYMAN2_WKS.CPY".
       COPY "LIBMN/TYMAN3_WKS.CPY".
       COPY "LIBMN/TYMAN4_WKS.CPY".
       COPY "LIBMN/TYMAN5_WKS.CPY".
       COPY "LIBMN/TYMAN6_WKS.CPY".
       COPY "LIBMN/TYMAN7_WKS.CPY".
       COPY "LIBMN/TYMAN8_WKS.CPY".
       COPY "LIBMN/TYMAN9_WKS.CPY".
       COPY "LIBMN/TYMANA_WKS.CPY".
       COPY "LIBMN/TYMANB_WKS.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/GPENVW.CPY".
       COPY "LIBGB/ACCESSW.CPY".

       COPY "LIBGB/GETFMW.CPY".

       SCREEN SECTION.
       COPY "LIBMN/TYMAN1_SCN.CPY".
       COPY "LIBMN/TYMAN2_SCN.CPY".
       COPY "LIBMN/TYMAN3_SCN.CPY".
       COPY "LIBMN/TYMAN4_SCN.CPY".
       COPY "LIBMN/TYMAN5_SCN.CPY".
       COPY "LIBMN/TYMAN6_SCN.CPY".
       COPY "LIBMN/TYMAN7_SCN.CPY".
       COPY "LIBMN/TYMAN8_SCN.CPY".
       COPY "LIBMN/TYMAN9_SCN.CPY".
       COPY "LIBMN/TYMANA_SCN.CPY".
       COPY "LIBMN/TYMANB_SCN.CPY".

       PROCEDURE DIVISION
            USING FORM-PATHNAME EXIT-PATHNAME.

      ******************************************************************
      *
      *    M A I N - P R O G R A M - M O D U L E
      *
      ******************************************************************
       MAIN-PROGRAM SECTION.
           PERFORM SQL-CONNECT.
      D    STOP MESS.

           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE FORM-NAMEX(1)  TO FORM-NAM.
           MOVE FORM-VFN(1) TO VDU-FORM-NAME.
           MOVE FORM-PATH     TO FORM-PATHNAME.
           MOVE 1             TO PAGE-FLAG.

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
                   HANDLE IS TYMAIN-WINDOW-HANDLE.

      *-----------------------------------------------------------------
       MAIN-MODULE.

           MOVE 1 TO HEADER-LEV HEADER-LEVEL.

              PERFORM FORM-RESET.
              IF ( PAGE2 )                  
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

      * GET GLOBAL PARAMETER RECORD (FROM GPENV):
           PERFORM GET-GPENV.

           PERFORM REC-TY-MODULE.

           IF ( IO-TYPE = "END" )
              GO TO END-ROUTINE.

           IF ( IO-TYPE = "CLR" )
              GO TO MAIN-MODULE.

           PERFORM ENTRY-MODULE.

           IF ( IO-TYPE = "RWR" )
              PERFORM WRITE-MODULE.

           IF ( IO-TYPE = "DEL" )
             PERFORM DELETE-MODULE.

           GO TO MAIN-MODULE.

      ******************************************************************
       REC-TY-MODULE SECTION.
      ******************************************************************
       REC-TY-MODULE-NO.
           MOVE 0 TO NEW-FLAG-SCR2  NEW-FLAG-SCR3 NEW-FLAG-SCR4
                     NEW-FLAG-SCR5  NEW-FLAG-SCR6 NEW-FLAG-SCR7
                     NEW-FLAG-SCR8  NEW-FLAG-SCR9 NEW-FLAG-SCR10
                     NEW-FLAG-SCR11 NEW-FLAG.

      * INITIALIZE KEY SO THAT SCAN WILL WORK MORE THAN ONCE.

           MOVE 0   TO TY1-KEY.
           MOVE " " TO IO-TYPE.

      * GET TODAYS DATE FROM SYSTEM DATE (SYSDATE).
      * SET TY-DATE     FROM SYSTEM DATE (SYSDATE).

           MOVE EXT-CONAME-SYSDATE         TO ALP-DATE.
           PERFORM CONVERT-ALPDATE-TO-YYYYMMDD.
           MOVE DATE-YYYYMMDD              TO TY-DATE
                                              HOLD-TY-DATE.

       ENTER-BRANCH.
           MOVE "EN N040BRANCH." TO VDU.
           PERFORM SCREENIO.

           IF ( VDUSTATUS = "1>" )
              MOVE "END" TO IO-TYPE
              GO TO END-REC-TY-MODULE.

           IF ( VDUSTATUS NOT = "00" )
              GO TO ENTER-BRANCH.

           IF ( VDUNUM0 = 0 )
              GO TO REC-TY-MODULE-NO.

           MOVE VDUNUM0 TO BR-NO TY-BRNO HOLD-TY-BRNO.

           PERFORM OPEN-BR-FILE.
           PERFORM READ-BR-FILE.
           PERFORM CLOSE-BR-FILE.
           IF ( IO-FG = 9 )
              MOVE "INVALID BRANCH" TO MESS
              PERFORM SEND-MESS
              GO TO REC-TY-MODULE-NO.

           MOVE EXT-FILPATH-BASE           TO FILPATH-BASE.
           MOVE BR-MACHINE                 TO FILPATH-MACHINE.
           MOVE BR-DATA-PATH               TO FILPATH-DATA.

           MOVE "B"                        TO TY-PATH-OVERRIDE.

           MOVE BR-NAME TO HOLD-BRNAME.

           IF ( NOT PAGE1 )
              MOVE "S  A000BRX." TO VDU
              MOVE BR-NAME TO VDUBUF
              PERFORM SCREENIO.

      *-----------------------------------------------------------------
       ENTER-DATE.
           MOVE "ERNM060DATE." TO VDU.
           PERFORM SCREENIO.

           IF ( VDUSTATUS = "1<" )
              MOVE ZEROES    TO TY1-KEY
              MOVE HOLD-TY-BRNO TO TY-BRNO
              PERFORM ENTER-SCANKEY
              GO TO END-REC-TY-MODULE.

           IF ( VDUSTATUS = "1>" )
              MOVE "END" TO IO-TYPE
              GO TO END-REC-TY-MODULE.

           IF ( VDUSTATUS = "1U" )
              GO TO ENTER-BRANCH.

           IF ( VDUSTATUS = "10" )
              GO TO ENTER-BRANCH.

           IF ( VDUSTATUS NOT = "00" )
              GO TO ENTER-DATE.

           IF ( VDUNUM0 = 0 )
              GO TO REC-TY-MODULE-NO.

           MOVE VDU-DATE-YYYYMMDD TO TY-DATE
                                     HOLD-TY-DATE.
      *-----------------------------------------------------------------
       GET-CLASS.
           MOVE "EN N030LC." TO VDU.
           PERFORM SCREENIO.

           IF ( VDUSTATUS = "1<" )
              MOVE ZEROES    TO TY1-KEY
              MOVE HOLD-TY-BRNO TO TY-BRNO
              MOVE HOLD-TY-DATE TO TY-DATE
              PERFORM ENTER-SCANKEY
              GO TO END-REC-TY-MODULE.

           IF ( VDUSTATUS = "1U" )
              GO TO ENTER-DATE.

           IF ( VDUSTATUS = "1>" )
              MOVE "END" TO IO-TYPE
              GO TO END-REC-TY-MODULE.

           IF ( VDUSTATUS NOT = "00" )
              GO TO GET-CLASS.

           MOVE VDUNUM0 TO TY-CLASS HOLD-TY-CLASS LC-CODE.

           IF ( TY-CLASS = 0 )
              MOVE SPACES TO LC-DESC
           ELSE
              PERFORM OPEN-LC1-FILE
              PERFORM READ-LC1-FILE
              PERFORM CLOSE-LC1-FILE
              IF ( IO-FG NOT = 0 )
                 MOVE "INVALID CLASS" TO MESS
                 PERFORM SEND-MESS
                 GO TO GET-CLASS.

           MOVE LC-DESC TO HOLD-LCDESC.

           IF ( NOT PAGE1 )
              MOVE "S  A020LCX." TO VDU
              MOVE LC-DESC       TO VDUBUF
              PERFORM SCREENIO.

           PERFORM OPEN-TY1-FILE.
           PERFORM READ-TY1-FILE.
           PERFORM CLOSE-TY1-FILE.

           IF ( IO-FG = 0 )
              MOVE TY-REC TO ORIGINAL-TY-REC
              PERFORM DISPLAY-PAGE
            ELSE
              IF ( PAGE1 )
                 PERFORM NEW-RECORD-SETUP
              ELSE
                 GO TO REC-TY-MODULE-NO.

       END-REC-TY-MODULE.
           EXIT.

      ******************************************************************
      *
      *    E N T R Y - M O D U L E
      *
      *-----------------------------------------------------------------
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
           ELSE  MOVE MAX11 TO MAX.

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
           IF ELEMENT > MAX
               MOVE 2 TO NEW-FLAG
               GO TO MODIFY-ROUTINE.

           IF PAGE1
              MOVE ELEMENT TO ELE1
              GO TO ENTER-ELE1
           ELSE
           IF PAGE2
              MOVE ELEMENT TO ELE2
              GO TO ENTER-ELE2
           ELSE
           IF PAGE3
              MOVE ELEMENT TO ELE3
              GO TO ENTER-ELE3
           ELSE
           IF PAGE4
              MOVE ELEMENT TO ELE4
              GO TO ENTER-ELE4
           ELSE 
           IF PAGE5
              MOVE ELEMENT TO ELE5
              GO TO ENTER-ELE5
           ELSE
           IF PAGE6
              MOVE ELEMENT TO ELE6
              GO TO ENTER-ELE6
           ELSE
           IF PAGE7
              MOVE ELEMENT TO ELE7
              GO TO ENTER-ELE7
           ELSE
           IF PAGE8
              MOVE ELEMENT TO ELE8
              GO TO ENTER-ELE8
           ELSE 
           IF PAGE9
              MOVE ELEMENT TO ELE9
              GO TO ENTER-ELE9
           ELSE
           IF PAGE10
              MOVE ELEMENT TO ELE10
              GO TO ENTER-ELE10
           ELSE
              MOVE ELEMENT TO ELE11
              GO TO ENTER-ELE11.

       MODIFY-ROUTINE.
           MOVE "R RA060SEL." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.
           IF VDUSTATUS = "18" OR "1:"
              IF PAGE2 OR PAGE6 OR PAGE7
                 PERFORM NEW-ACTIVITY
                 GO TO MODIFY-ROUTINE.

           IF ( PAGE3            ) AND
              ( F2-KEY OR F3-KEY )
              PERFORM TOGGLE-ON-HEADER
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
           IF NEW-FLAG = 0
              PERFORM WRITE-MODULE.

           PERFORM SWITCH-FORM.

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
           ELSE
              MOVE CHNO TO ELE11
              GO TO ENTER-ELE11.
      *----------------------------------------------------------------
       ENTER-ELE1.
           IF CHKEY NOT = ZEROS
              IF CHFG1(ELE1) = " "
                 IF NEW-FLAG NOT = 1
                    GO TO MODIFY-ROUTINE.
           IF CHFG1(ELE1) = "N"
              IF CHKEY NOT = EXT-CONAME-DAILY-PASSWORD
                 GO TO MODIFY-ROUTINE.

      * BUCKET 1 IS NOT USED BUT HERE FOR FUTURE USE.
           IF ELE1 = 1 GO TO CREATE-ROUTINE.

           MOVE SPEC1(ELE1) TO VDU.
           MOVE ELE1 TO VDUROW.
           GO TO ACTUAL-READ.

      *----------------------------------------------------------------
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

      *----------------------------------------------------------------
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
           IF ELE3 = 5
              GO TO GOTO-FIELD3.
           GO TO ACTUAL-READ.

      *----------------------------------------------------------------
       ENTER-ELE4.
           IF CHKEY NOT = ZEROS
              IF CHFG4(ELE4) = " "
                 IF NEW-FLAG-SCR4 NOT = 1
                    GO TO MODIFY-ROUTINE.
           IF CHFG4(ELE4) = "N"
              IF CHKEY NOT = EXT-CONAME-DAILY-PASSWORD
                 GO TO MODIFY-ROUTINE.

      * BUCKETS 4 THRU 4 NOT CORRENTLY USED BY RETAINED FOR FUTURE USE.
           IF ELE4 = 4
              GO TO MODIFY-ROUTINE.

           MOVE SPEC4(ELE4) TO VDU.
           MOVE ELE4 TO VDUROW.
           GO TO ACTUAL-READ.

      *----------------------------------------------------------------
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

      *----------------------------------------------------------------
       ENTER-ELE6.
           IF CHKEY NOT = ZEROS
              IF CHFG6(ELE6) = " "
                 IF NEW-FLAG-SCR6 NOT = 1
                    GO TO MODIFY-ROUTINE.
           IF CHFG6(ELE6) = "N"
              IF CHKEY NOT = EXT-CONAME-DAILY-PASSWORD
                 GO TO MODIFY-ROUTINE.

      * BUCKETS 4 AND 5 NOT CORRENTLY USED BY RETAINED FOR FUTURE USE.
           IF ELE6 = 4 OR 5
              GO TO MODIFY-ROUTINE.

           MOVE SPEC6(ELE6) TO VDU.
           MOVE ELE6 TO VDUROW.
           GO TO ACTUAL-READ.

      *----------------------------------------------------------------
       ENTER-ELE7.
           IF CHKEY NOT = ZEROS
              IF CHFG7(ELE7) = " "
                 IF NEW-FLAG-SCR7 NOT = 1
                    GO TO MODIFY-ROUTINE.
           IF CHFG7(ELE7) = "N"
              IF CHKEY NOT = EXT-CONAME-DAILY-PASSWORD
                 GO TO MODIFY-ROUTINE.

           MOVE SPEC7(ELE7) TO VDU.
           MOVE ELE7 TO VDUROW.
           GO TO ACTUAL-READ.

      *----------------------------------------------------------------
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

      *----------------------------------------------------------------
       ENTER-ELE9.
           IF ( CHKEY NOT = ZEROS )
              IF ( CHFG9(ELE9) = " " )
                 IF ( NEW-FLAG-SCR9 NOT = 1 )
                    GO TO MODIFY-ROUTINE.

           IF ( CHFG9(ELE9) = "N" )
              IF ( CHKEY NOT = CHAN )
                 GO TO MODIFY-ROUTINE.

           MOVE SPEC9(ELE9) TO VDU.
           MOVE ELE9        TO VDUROW.
           GO TO ACTUAL-READ.

      *----------------------------------------------------------------
       ENTER-ELE10.
           IF ( CHKEY NOT = ZEROS )
              IF ( CHFG10(ELE10) = " " )
                 IF ( NEW-FLAG-SCR10 NOT = 1 )
                    GO TO MODIFY-ROUTINE.

           IF ( CHFG10(ELE10) = "N" )
              IF ( CHKEY NOT = CHAN )
                 GO TO MODIFY-ROUTINE.

           MOVE SPEC10(ELE10) TO VDU.
           MOVE ELE10        TO VDUROW.
           GO TO ACTUAL-READ.

      *----------------------------------------------------------------
       ENTER-ELE11.
           IF ( CHKEY NOT = ZEROS )
              IF ( CHFG11(ELE11) = " " )
                 IF ( NEW-FLAG-SCR11 NOT = 1 )
                    GO TO MODIFY-ROUTINE.

           IF ( CHFG11(ELE11) = "N" )
              IF ( CHKEY NOT = CHAN )
                 GO TO MODIFY-ROUTINE.

           MOVE SPEC11(ELE11) TO VDU.
           MOVE ELE11        TO VDUROW.
           GO TO ACTUAL-READ.

      *----------------------------------------------------------------
       ACTUAL-READ.
           MOVE 0 TO VDUCOL.
           MOVE "." TO VDUDECIMAL.
           PERFORM SCREENIO.

       TEST-F4.
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
                   MOVE -1 TO ONE
                   GO TO CREATE-ROUTINE.
       TEST-F5.
           IF VDUSTATUS = "1D"
              MOVE 1 TO ONE
              GO TO GOTO-FIELD-1234-OR-56.
       TEST-F1.
           IF VDUSTATUS = "10"
              MOVE "CLR" TO IO-TYPE
              GO TO END-ENTRY-MODULE.
           IF VDUSTATUS NOT = "00"
              IF PAGE1 GO TO ENTER-ELE1
              ELSE IF PAGE2 GO TO ENTER-ELE2
              ELSE IF PAGE3 GO TO ENTER-ELE3
              ELSE IF PAGE4 GO TO ENTER-ELE4
              ELSE IF PAGE5 GO TO ENTER-ELE5
              ELSE IF PAGE6 GO TO ENTER-ELE6
              ELSE IF PAGE7 GO TO ENTER-ELE7
              ELSE IF PAGE8 GO TO ENTER-ELE8
              ELSE IF PAGE9 GO TO ENTER-ELE9
              ELSE IF PAGE10 GO TO ENTER-ELE10
              ELSE IF PAGE11 GO TO ENTER-ELE11.
           MOVE 1 TO ONE.
      /
       GOTO-FIELD-1234-OR-56.
           IF PAGE2 GO TO GOTO-FIELD2.
           IF PAGE3 GO TO GOTO-FIELD3.
           IF PAGE4 GO TO GOTO-FIELD4.
           IF PAGE5 GO TO GOTO-FIELD5.
           IF PAGE6 GO TO GOTO-FIELD6.
           IF PAGE7 GO TO GOTO-FIELD7.
           IF PAGE8 GO TO GOTO-FIELD8.
           IF PAGE9 GO TO GOTO-FIELD9.
           IF PAGE10 GO TO GOTO-FIELD10.
           IF PAGE11 GO TO GOTO-FIELD11.
      *----------------------------------------------------------------
       GOTO-FIELD1.
           GO TO F-01 F-02 F-03 F-04 F-05
                 F-06 F-07 F-08 F-09 F-10
                 F-11 F-12 F-13 F-14 F-15
                 F-16 F-17 F-18 F-19 F-20
                 F-21 F-22 F-23 F-24 F-25
                 F-26
               DEPENDING ON ELE1.
           GO TO END-ENTRY-MODULE.
      *----------------------------------------------------------------
       GOTO-FIELD2.
           MOVE ACTIVITY TO SUB2.
           GO TO G-01 G-01 G-01 G-01 G-01 G-01
                 G-01 G-01 G-01 G-01 G-01 G-12
                 G-13 G-14 DEPENDING ON ELE2.
           GO TO END-ENTRY-MODULE.
      *----------------------------------------------------------------
       GOTO-FIELD3.
      * NO MISS PAYS ON RECENCY
           IF ELE3 = 5
              GO TO H-012.
           GO TO H-01 H-01 H-01 H-01 H-01 H-01 H-01 H-01 H-01 H-01
                 DEPENDING ON ELE3.
           GO TO END-ENTRY-MODULE.
      *----------------------------------------------------------------
       GOTO-FIELD4.
           GO TO I-01 I-02 I-03 I-04 I-05 I-06 I-07 I-08 I-09 I-10
                 I-11 I-12 I-13 I-14 I-15
                 DEPENDING ON ELE4.
           GO TO END-ENTRY-MODULE.
      *----------------------------------------------------------------
       GOTO-FIELD5.
           GO TO J-01 J-02 J-03 J-04 J-05 J-06 J-07 J-08 J-09 J-10
                 J-11 J-12 J-13 J-14 J-15 J-16 J-17 J-18 J-19
                 DEPENDING ON ELE5.
           GO TO END-ENTRY-MODULE.
      *----------------------------------------------------------------
       GOTO-FIELD6.
           MOVE ACTIVITY TO SUB6.
           GO TO K-01 K-01 K-01 K-01 K-01 K-06
                 K-07 K-08 K-09
                 DEPENDING ON ELE6.
           GO TO END-ENTRY-MODULE.

      *----------------------------------------------------------------
       GOTO-FIELD7.
           MOVE ACTIVITY TO SUB7.
           GO TO M-01 M-01 M-01 M-01 M-01 M-01 M-01 M-01 M-01 M-01 M-11
                 DEPENDING ON ELE7.
           GO TO END-ENTRY-MODULE.
      *----------------------------------------------------------------
       GOTO-FIELD8.
           GO TO N-01 N-02 N-03 N-04 N-05
                 N-06 N-07 N-08 N-09 N-10
                 DEPENDING ON ELE8.
           GO TO END-ENTRY-MODULE.

      *----------------------------------------------------------------
       GOTO-FIELD9.
           GO TO O-01     
                 DEPENDING ON ELE9.

           GO TO END-ENTRY-MODULE.

      *-----------------------------------------------------------------
       GOTO-FIELD10.

           GO TO P-01 P-01 P-01 P-01 P-01
                 P-01 P-01 P-01 P-01 P-01
                 P-FEE P-FEE P-FEE
                   DEPENDING ON ELE10.

           GO TO END-ENTRY-MODULE.

      *-----------------------------------------------------------------
       GOTO-FIELD11.

           GO TO Q-01 Q-01 Q-01 Q-01 Q-01 Q-01
                   DEPENDING ON ELE11.

           GO TO END-ENTRY-MODULE.

      *----------------------------------------------------------------
      *          SET RECORD ELEMENTS FOR PAGE 1
      *----------------------------------------------------------------
       F-01.
           MOVE VDUNUM2 TO TY-OTHCOL.
           GO TO CREATE-ROUTINE.
       F-02.
           MOVE VDUNUM2 TO TY-OT2COL.
           GO TO CREATE-ROUTINE.
       F-03.
           MOVE VDUNUM2 TO TY-INTCOL.
           GO TO CREATE-ROUTINE.
       F-04.
           MOVE VDUNUM2 TO TY-LATECOL.
           GO TO CREATE-ROUTINE.
       F-05.
           MOVE VDUNUM2 TO TY-PRINCOL.
           GO TO CREATE-ROUTINE.
       F-06.
           MOVE VDUNUM2 TO TY-DEFCOL.
           GO TO CREATE-ROUTINE.
       F-07.
           MOVE VDUNUM2 TO TY-RNPRIN.
           GO TO CREATE-ROUTINE.
       F-08.
           MOVE VDUNUM2 TO TY-PART-CHGOFF.
           GO TO CREATE-ROUTINE.
       F-09.
           MOVE VDUNUM2 TO TY-INTREB.
           GO TO CREATE-ROUTINE.
       F-10.
           MOVE VDUNUM2 TO TY-CLREB1.
           GO TO CREATE-ROUTINE.
       F-11.
           MOVE VDUNUM2 TO TY-CLREB2.
           GO TO CREATE-ROUTINE.
       F-12.
           MOVE VDUNUM2 TO TY-CLREB3.
           GO TO CREATE-ROUTINE.
       F-13.
           MOVE VDUNUM2 TO TY-CLREB4.
           GO TO CREATE-ROUTINE.
       F-14.
           MOVE VDUNUM2 TO TY-AHREB.
           GO TO CREATE-ROUTINE.
       F-15.
           MOVE VDUNUM2 TO TY-PPREB.
           GO TO CREATE-ROUTINE.
       F-16.
           MOVE VDUNUM2 TO TY-O1REB.
           GO TO CREATE-ROUTINE.
       F-17.
           MOVE VDUNUM2 TO TY-O2REB.
           GO TO CREATE-ROUTINE.
       F-18.
           MOVE VDUNUM2 TO TY-O3REB.
           GO TO CREATE-ROUTINE.
       F-19.
           MOVE VDUNUM2 TO TY-O4REB.
           GO TO CREATE-ROUTINE.
       F-20.
           MOVE VDUNUM2 TO TY-O5REB.
           GO TO CREATE-ROUTINE.
       F-21.
           MOVE VDUNUM2 TO TY-O6REB.
           GO TO CREATE-ROUTINE.
       F-22.
           MOVE VDUNUM2 TO TY-O7REB.
           GO TO CREATE-ROUTINE.
       F-23.
           MOVE VDUNUM2 TO TY-SCREB.
           GO TO CREATE-ROUTINE.
       F-24.
           MOVE VDUNUM2 TO TY-MFREB.
           GO TO CREATE-ROUTINE.
       F-25.
           MOVE VDUNUM2 TO TY-DFREB.
           GO TO CREATE-ROUTINE.
       F-26.
           MOVE VDUBUF TO TY-BRFG.
           GO TO CREATE-ROUTINE.
      *----------------------------------------------------------------
      *          SET RECORD ELEMENTS FOR PAGE 2
      *----------------------------------------------------------------
       G-01.
           MOVE VDUNUM0 TO TY-NO(SUB2 ELE2).
       G-011.
           MOVE "ENNS082  1." TO VDU.
           MOVE ELE2 TO VDUROW.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U" GO TO ENTER-ELE2.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO G-011.
           MOVE VDUNUM2 TO TY-AMT(SUB2 ELE2).
       G-012.
           MOVE "ENNS082  2." TO VDU.
           MOVE ELE2 TO VDUROW.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U" GO TO G-011.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO G-012.
           MOVE VDUNUM2 TO TY-O2(SUB2 ELE2).
       G-013.
           MOVE "ENNS082  3." TO VDU.
           MOVE ELE2 TO VDUROW.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U" GO TO G-012.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO G-013.
           MOVE VDUNUM2 TO TY-INT(SUB2 ELE2).
           GO TO CREATE-ROUTINE.
       G-12.
           MOVE VDUNUM0 TO TY-SUMWGTTM.
           GO TO CREATE-ROUTINE.
       G-13.
           MOVE VDUNUM2 TO TY-SUMWGTAV.
           GO TO CREATE-ROUTINE.
       G-14.
           MOVE VDUNUM2 TO TY-SUMWGTTA.
           GO TO CREATE-ROUTINE.

      *----------------------------------------------------------------
      *          SET RECORD ELEMENTS FOR PAGE 3
      *----------------------------------------------------------------
       H-01.
           IF ( HEADER-LEV-STD )
              MOVE VDUNUM0 TO TY-UNITS       (1 ELE3)
           ELSE
              MOVE VDUNUM0 TO TY-UNITS-BNKRPT(1 ELE3).
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
              MOVE VDUNUM2 TO TY-OUTBAL(1 ELE3)
           ELSE
              MOVE VDUNUM2 TO TY-OUTBAL-BNKRPT(1 ELE3).

       H-012.
           MOVE "ENNS070012." TO VDU.
           MOVE ELE3 TO VDUROW.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              IF ELE3 = 5
                 GO TO H-012
              ELSE
                 GO TO H-011.
           IF VDUSTATUS = "10"
              GO TO ENTER-ELE3.
           IF VDUSTATUS NOT = "00"
              GO TO H-012.

           IF ( HEADER-LEV-STD )
              MOVE VDUNUM0 TO TY-UNITS(2 ELE3)
           ELSE
              MOVE VDUNUM0 TO TY-UNITS-BNKRPT(2 ELE3).

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
              MOVE VDUNUM2 TO TY-OUTBAL(2 ELE3)
           ELSE
              MOVE VDUNUM2 TO TY-OUTBAL(2 ELE3).

           GO TO CREATE-ROUTINE.

      *----------------------------------------------------------------
      *          SET RECORD ELEMENTS FOR PAGE 4
      *----------------------------------------------------------------
       I-01.
           MOVE VDUNUM2 TO TY-ALLOTMENTS.
           GO TO CREATE-ROUTINE.
       I-02.
           MOVE VDUNUM2 TO TY-PROCEEDS.
           GO TO CREATE-ROUTINE.
       I-03.
           MOVE VDUNUM2 TO TY-LOCKBOX.
           GO TO CREATE-ROUTINE.
       I-04.
           MOVE VDUNUM0 TO TY-OTH-NO-COL.
           GO TO CREATE-ROUTINE.
       I-05.
           MOVE VDUNUM0 TO TY-OT2-NO-COL.
           GO TO CREATE-ROUTINE.
       I-06.
           MOVE VDUNUM0 TO TY-INT-NO-COL.
           GO TO CREATE-ROUTINE.
       I-07.
           MOVE VDUNUM0 TO TY-LATE-NO-COL.
           GO TO CREATE-ROUTINE.
       I-08.
           MOVE VDUNUM0 TO TY-OTH-NO-WAIVED.
           GO TO CREATE-ROUTINE.
       I-09.
           MOVE VDUNUM0 TO TY-OT2-NO-WAIVED.
           GO TO CREATE-ROUTINE.
       I-10.
           MOVE VDUNUM0 TO TY-INT-NO-WAIVED.
           GO TO CREATE-ROUTINE.
       I-11.
           MOVE VDUNUM0 TO TY-LATE-NO-WAIVED.
           GO TO CREATE-ROUTINE.
       I-12.
           MOVE VDUNUM2 TO TY-OTH-AMT-WAIVED.
           GO TO CREATE-ROUTINE.
       I-13.
           MOVE VDUNUM2 TO TY-OT2-AMT-WAIVED.
           GO TO CREATE-ROUTINE.
       I-14.
           MOVE VDUNUM2 TO TY-INT-AMT-WAIVED.
           GO TO CREATE-ROUTINE.
       I-15.
           MOVE VDUNUM2 TO TY-LATE-AMT-WAIVED.
           GO TO CREATE-ROUTINE.
      *----------------------------------------------------------------
      *          SET RECORD ELEMENTS FOR PAGE 5
      *----------------------------------------------------------------
       J-01.
           MOVE VDUNUM2 TO TY-CKWRITTEN(1).
           GO TO CREATE-ROUTINE.
       J-02.
           MOVE VDUNUM2 TO TY-CKWRITTEN(2).
           GO TO CREATE-ROUTINE.
       J-03.
           MOVE VDUNUM2 TO TY-CKWRITTEN(3).
           GO TO CREATE-ROUTINE.
       J-04.
           MOVE VDUNUM2 TO TY-CKWRITTEN(4).
           GO TO CREATE-ROUTINE.
       J-05.
           MOVE VDUNUM2 TO TY-CKWRITTEN(5).
           GO TO CREATE-ROUTINE.
       J-06.
           MOVE VDUNUM2 TO TY-CKWRITTEN(6).
           GO TO CREATE-ROUTINE.
       J-07.
           MOVE VDUNUM2 TO TY-CKWRITTEN(7).
           GO TO CREATE-ROUTINE.
       J-08.
           MOVE VDUNUM2 TO TY-CKWRITTEN(8).
           GO TO CREATE-ROUTINE.
       J-09.
           MOVE VDUNUM2 TO TY-CKWRITTEN(9).
           GO TO CREATE-ROUTINE.
       J-10.
           MOVE VDUNUM2 TO TY-CKWRITTEN(10).
           GO TO CREATE-ROUTINE.
       J-11.
           MOVE VDUNUM2 TO TY-CKWRITTEN(11).
           GO TO CREATE-ROUTINE.
       J-12.
           MOVE VDUNUM2 TO TY-CKWRITTEN(12).
           GO TO CREATE-ROUTINE.
       J-13.
           MOVE VDUNUM2 TO TY-CKWRITTEN(13).
           GO TO CREATE-ROUTINE.
       J-14.
           MOVE VDUNUM2 TO TY-CKWRITTEN(14).
           GO TO CREATE-ROUTINE.
       J-15.
           MOVE VDUNUM2 TO TY-CKWRITTEN(15).
           GO TO CREATE-ROUTINE.
       J-16.
           MOVE VDUNUM2 TO TY-CKWRITTEN(16).
           GO TO CREATE-ROUTINE.
       J-17.
           MOVE VDUNUM0 TO TY-NO-SOLD.
       J-171.
           MOVE "ENNS082  1." TO VDU.
           MOVE ELE5 TO VDUROW.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U" GO TO ENTER-ELE5.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO J-171.
           MOVE VDUNUM2 TO TY-AMT-SOLD.
           GO TO CREATE-ROUTINE.
       J-18.
           MOVE VDUNUM0 TO TY-NO-TRANS.
       J-181.
           MOVE "ENNS082  1." TO VDU.
           MOVE ELE5 TO VDUROW.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U" GO TO ENTER-ELE5.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO J-181.
           MOVE VDUNUM2 TO TY-AMT-TRANS.
           GO TO CREATE-ROUTINE.
       J-19.
           MOVE VDUNUM0 TO TY-NO-PURCH.
       J-191.
           MOVE "ENNS082  1." TO VDU.
           MOVE ELE5 TO VDUROW.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U" GO TO ENTER-ELE5.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO J-191.
           MOVE VDUNUM2 TO TY-AMT-PURCH.
           GO TO CREATE-ROUTINE.
      *----------------------------------------------------------------
      *          SET RECORD ELEMENTS FOR PAGE 6
      *----------------------------------------------------------------
       K-01.
           MOVE VDUNUM0 TO TY-NO-REPO(SUB6 ELE6).
       K-011.
           MOVE "ENNS082  1." TO VDU.
           MOVE ELE6 TO VDUROW.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U" GO TO ENTER-ELE6.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO K-011.
           MOVE VDUNUM2 TO TY-AMT-REPO(SUB6 ELE6).
       K-012.
           MOVE "ENNS082  2." TO VDU.
           MOVE ELE6 TO VDUROW.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U" GO TO K-011.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO K-012.
           MOVE VDUNUM2 TO TY-O2-REPO(SUB6 ELE6).
       K-013.
           MOVE "ENNS082  3." TO VDU.
           MOVE ELE6 TO VDUROW.
           PERFORM SCREENIO.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO K-013.
           MOVE VDUNUM2 TO TY-INT-REPO(SUB6 ELE6).
           GO TO CREATE-ROUTINE.
       K-06.
           MOVE VDUNUM2 TO TY-DLRVPRIN(SUB6 1).
       K-061.
           MOVE "ENNS082  1." TO VDU.
           MOVE ELE6 TO VDUROW.
           PERFORM SCREENIO.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO K-061.
           MOVE VDUNUM2 TO TY-DLRVPRIN(SUB6 2).
       K-062.
           MOVE "ENNS082  2." TO VDU.
           MOVE ELE6 TO VDUROW.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U" GO TO ENTER-ELE6.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO K-062.
           MOVE VDUNUM2 TO TY-DLRVPRIN(SUB6 3).
           GO TO CREATE-ROUTINE.
       K-07.
           MOVE VDUNUM2 TO TY-DLRVCHGS(SUB6 1).
       K-071.
           MOVE "ENNS082  1." TO VDU.
           MOVE ELE6 TO VDUROW.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U" GO TO ENTER-ELE6.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO K-071.
           MOVE VDUNUM2 TO TY-DLRVCHGS(SUB6 2).
       K-072.
           MOVE "ENNS082  2." TO VDU.
           MOVE ELE6 TO VDUROW.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U" GO TO ENTER-ELE6.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO K-072.
           MOVE VDUNUM2 TO TY-DLRVCHGS(SUB6 3).
           GO TO CREATE-ROUTINE.
       K-08.
           MOVE VDUNUM2 TO TY-DLPARVHELD(SUB6).
           GO TO CREATE-ROUTINE.
       K-09.
           MOVE VDUNUM2 TO TY-DLDISC.
           GO TO CREATE-ROUTINE.

      *----------------------------------------------------------------
      *          SET RECORD ELEMENTS FOR PAGE 7
      *----------------------------------------------------------------
       M-01.
           MOVE VDUNUM0 TO TY-NO-PL(SUB7 ELE7).
       M-011.
           MOVE "ENNS082  1." TO VDU.
           MOVE ELE7 TO VDUROW.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE7.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO M-011.
           MOVE VDUNUM2 TO TY-AMT-PL(SUB7 ELE7).
       M-012.
           MOVE "ENNS082  2." TO VDU.
           MOVE ELE7 TO VDUROW.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO M-011.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO M-012.
           MOVE VDUNUM2 TO TY-O2-PL(SUB7 ELE7).
       M-013.
           MOVE "ENNS082  3." TO VDU.
           MOVE ELE7 TO VDUROW.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO M-012.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO M-013.
           MOVE VDUNUM2 TO TY-INT-PL(SUB7 ELE7).
           GO TO CREATE-ROUTINE.

       M-11.
           MOVE VDUNUM2 TO TY-PART-CHGOFF.
           GO TO CREATE-ROUTINE.

      *----------------------------------------------------------------
      *          SET RECORD ELEMENTS FOR PAGE 8
      *----------------------------------------------------------------
       N-01.
           MOVE VDUNUM2 TO TY-PL-PRINCOL(1).
       N-011.
           MOVE "E NS082  1." TO VDU.
           MOVE ELE8 TO VDUROW.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE8.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO N-011.
           MOVE VDUNUM2 TO TY-PL-INTCOL(1).
           GO TO CREATE-ROUTINE.
       N-02.
           MOVE VDUNUM2 TO TY-PL-LATECOL(1).
       N-021.
           MOVE "E NS082  1." TO VDU.
           MOVE ELE8 TO VDUROW.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE8.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO N-021.
           MOVE VDUNUM2 TO TY-PL-O2COL(1).
           GO TO CREATE-ROUTINE.
       N-03.
           MOVE VDUNUM2 TO TY-PL-PRINCOL(2).
       N-031.
           MOVE "E NS082  1." TO VDU.
           MOVE ELE8 TO VDUROW.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE8.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO N-031.
           MOVE VDUNUM2 TO TY-PL-INTCOL(2).
           GO TO CREATE-ROUTINE.
       N-04.
           MOVE VDUNUM2 TO TY-PL-LATECOL(2).
       N-041.
           MOVE "E NS082  1." TO VDU.
           MOVE ELE8 TO VDUROW.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE8.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO N-041.
           MOVE VDUNUM2 TO TY-PL-O2COL(2).
           GO TO CREATE-ROUTINE.
       N-05.
           MOVE VDUNUM2 TO TY-PL-PRINCOL(3).
       N-051.
           MOVE "E NS082  1." TO VDU.
           MOVE ELE8 TO VDUROW.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE8.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO N-051.
           MOVE VDUNUM2 TO TY-PL-INTCOL(3).
           GO TO CREATE-ROUTINE.
       N-06.
           MOVE VDUNUM2 TO TY-PL-LATECOL(3).
       N-061.
           MOVE "E NS082  1." TO VDU.
           MOVE ELE8 TO VDUROW.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE8.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO N-061.
           MOVE VDUNUM2 TO TY-PL-O2COL(3).
           GO TO CREATE-ROUTINE.
       N-07.
           MOVE VDUNUM2 TO TY-PL-PRINCOL(4).
       N-071.
           MOVE "E NS082  1." TO VDU.
           MOVE ELE8 TO VDUROW.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE8.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO N-071.
           MOVE VDUNUM2 TO TY-PL-INTCOL(4).
           GO TO CREATE-ROUTINE.
       N-08.
           MOVE VDUNUM2 TO TY-PL-LATECOL(4).
       N-081.
           MOVE "E NS082  1." TO VDU.
           MOVE ELE8 TO VDUROW.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE8.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO N-081.
           MOVE VDUNUM2 TO TY-PL-O2COL(4).
           GO TO CREATE-ROUTINE.
       N-09.
           MOVE VDUNUM2 TO TY-PL-PRINCOL(5).
       N-091.
           MOVE "E NS082  1." TO VDU.
           MOVE ELE8 TO VDUROW.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE8.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO N-091.
           MOVE VDUNUM2 TO TY-PL-INTCOL(5).
           GO TO CREATE-ROUTINE.
       N-10.
           MOVE VDUNUM2 TO TY-PL-LATECOL(5).
       N-101.
           MOVE "E NS082  1." TO VDU.
           MOVE ELE8 TO VDUROW.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-ELE8.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO N-101.
           MOVE VDUNUM2 TO TY-PL-O2COL(5).
           GO TO CREATE-ROUTINE.

      * SET RECORD ELEMENTS FOR PAGE 9
       O-01.
           GO TO CREATE-ROUTINE. 

      * SET RECORD ELEMENTS FOR PAGE 10
       P-01.
           MOVE VDUNUM0 TO TY-INS-NO-ELIG(ELE10).
      
       P-011.
           MOVE "ENNS070  1." TO VDU.
           MOVE ELE10         TO VDUROW.
           PERFORM SCREENIO.
           IF ( VDUSTATUS = "1U" )
              GO TO ENTER-ELE10.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO P-011.
           MOVE VDUNUM0 TO TY-INS-NO-WROTE(ELE10).
      *----------------------------------------------------------------
       P-012.
           MOVE "ENNS070  2." TO VDU.
           MOVE ELE10         TO VDUROW.
           PERFORM SCREENIO.
           IF ( VDUSTATUS = "1U" )
              GO TO P-011.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO P-012.
           MOVE VDUNUM0 TO TY-INS-NO-NOTWROTE(ELE10).
      *----------------------------------------------------------------
       P-013.
           MOVE "ENNS082  3." TO VDU.
           MOVE ELE10         TO VDUROW.
           PERFORM SCREENIO.
           IF ( VDUSTATUS = "1U" )
              GO TO P-012.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO P-013.
           MOVE VDUNUM2 TO TY-INS-AMT-WROTE(ELE10).
           GO TO CREATE-ROUTINE.
       P-FEE.
           MOVE VDUNUM0 TO TY-FEE-NO-ELIG(ELE10 - 10).
      *----------------------------------------------------------------
       P-FEE-011.
           MOVE "ENNS070  1." TO VDU.
           MOVE ELE10         TO VDUROW.
           PERFORM SCREENIO.
           IF ( VDUSTATUS = "1U" )
              GO TO ENTER-ELE10.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO P-FEE-011.
           MOVE VDUNUM0 TO TY-FEE-NO-WROTE(ELE10 - 10).
      *----------------------------------------------------------------
       P-FEE-012.
           MOVE "ENNS070  2." TO VDU.
           MOVE ELE10         TO VDUROW.
           PERFORM SCREENIO.
           IF ( VDUSTATUS = "1U" )
              GO TO P-FEE-011.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO P-FEE-012.
           MOVE VDUNUM0 TO TY-FEE-NO-NOTWROTE(ELE10 - 10).
      *----------------------------------------------------------------
       P-FEE-013.
           MOVE "ENNS082  3." TO VDU.
           MOVE ELE10         TO VDUROW.
           PERFORM SCREENIO.
           IF ( VDUSTATUS = "1U" )
              GO TO P-FEE-012.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO P-FEE-013.
           MOVE VDUNUM2 TO TY-FEE-AMT-WROTE(ELE10 - 10).
           GO TO CREATE-ROUTINE.
      *-----------------------------------------------------------------
      *
      *    SET RECORD ELEMENTS FOR PAGE 11
      *
      *-----------------------------------------------------------------
       Q-01.
           MOVE VDUNUM2 TO TY-UNEARNED-AMT(1 ELE11).
       Q-011.
           MOVE "ENNN082  1." TO VDU.
           MOVE ELE11 TO VDUROW.
           PERFORM SCREENIO.
           IF UP-KEY GO TO ENTER-ELE11.
           IF NOT (ENTER-KEY OR DOWN-KEY) GO TO Q-011.
           MOVE VDUNUM2 TO TY-UNEARNED-AMT(2 ELE11).
           GO TO CREATE-ROUTINE.

       END-ENTRY-MODULE.
           EXIT.

      ******************************************************************
       ENTER-SCANKEY SECTION.
      ******************************************************************
       ENTER-SCAN.
           PERFORM OPEN-TY1-FILE.

       ENTER-KEY.
           MOVE "EN N090SCANKEY." TO VDU.
           PERFORM SCREENIO.

           IF ( VDUSTATUS = "10" )
              MOVE "CLR" TO IO-TYPE
              GO TO CLOSE-SCANKEY.

           IF ( VDUSTATUS = "1<" )
               GO TO READ-TYFILE-NEXT.

           IF ( VDUSTATUS = "1>" )
              MOVE "END" TO IO-TYPE
              GO TO END-REC-TY-MODULE.

           IF ( VDUSTATUS NOT = "00" )
              GO TO ENTER-KEY.

           IF ( VDUNUM0 = 0 )
              IF FIRST-SCAN NOT = 0
                 MOVE TY1-KEY TO HOLD-TY1-KEY
                 MOVE TY-REC TO ORIGINAL-TY-REC
                 GO TO CLOSE-SCANKEY
              ELSE
                 GO TO ENTER-KEY.

           MOVE VDUNUM0             TO WORK-TY1-KEY.

           MOVE HOLD-TY-BRNO        TO TY-BRNO.
           MOVE WORK-TY-DATE        TO DATE-MMDDYY.
           PERFORM CONVERT-MMDDYY-TO-YYYYMMDD.
           MOVE DATE-YYYYMMDD       TO TY-DATE.
           MOVE WORK-TY-CLASS       TO TY-CLASS.
          
           PERFORM START-TY1-FILE.
 
      *-----------------------------------------------------------------
       READ-TYFILE-NEXT.

           IF ( FIRST-SCAN = 0 )
              PERFORM START-TY1-FILE
              MOVE 1 TO FIRST-SCAN.

           PERFORM READ-TY1-FILE-NEXT.
           IF ( IO-FG = 9 ) OR ( TY-BRNO NOT = TY-PATH-OWNBR )
              MOVE "CLR" TO IO-TYPE
              GO TO CLOSE-SCANKEY.

      *    UNLOCK TY-FILE.
           PERFORM DISPLAY-SCAN-KEY.

      * TEST FOR PAGE NUMBER TO ASSURE PROPER DISPLAY.

           IF PAGE1 PERFORM DISPLAY-FIELDS
           ELSE IF PAGE2 PERFORM DISPLAY-FIELDS-2
           ELSE IF PAGE3 PERFORM DISPLAY-FIELDS-3
           ELSE IF PAGE4 PERFORM DISPLAY-FIELDS-4
           ELSE IF PAGE5 PERFORM DISPLAY-FIELDS-5
           ELSE IF PAGE6 PERFORM DISPLAY-FIELDS-6
           ELSE IF PAGE7 PERFORM DISPLAY-FIELDS-7
           ELSE IF PAGE8 PERFORM DISPLAY-FIELDS-8
           ELSE IF PAGE9 PERFORM DISPLAY-FIELDS-9
           ELSE IF PAGE10 PERFORM DISPLAY-FIELDS-10
           ELSE IF PAGE11 PERFORM DISPLAY-FIELDS-11
           ELSE
              GO TO ENTER-SCAN.

           GO TO ENTER-KEY.

       DISPLAY-SCAN-KEY.
           MOVE "SN N040BRANCH." TO VDU.
           MOVE TY-BRNO          TO VDUNUM0.
           PERFORM SCREENIO.
      
           MOVE "SN 8060DATE."   TO VDU.
           MOVE TY-DATE          TO VDU-DATE-YYYYMMDD.
           PERFORM SCREENIO.
      
           MOVE "SN N030LC."     TO VDU.
           MOVE TY-CLASS         TO VDUNUM0.
           PERFORM SCREENIO.
      
       CLOSE-SCANKEY.
           PERFORM CLOSE-TY1-FILE.
           MOVE 0 TO FIRST-SCAN.

       END-SCANKEY.
           EXIT.

      ******************************************************************
       WRITE-MODULE SECTION.
      ******************************************************************
           PERFORM OPEN-TY1-FILE.
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
                  NEXT SENTENCE
           ELSE
               GO TO WRITE-RECORD.

           MOVE TY-REC TO CHANGED-TY-REC.
           IF ORIGINAL-TY-REC = CHANGED-TY-REC
              GO TO END-WRITE-MODULE.
           PERFORM READ-TY1-FILE.
           IF IO-FG NOT = 0
              PERFORM UNDERGONE-CHANGE
              PERFORM RECORD-DELETED
              GO TO END-WRITE-MODULE.
           IF TY-REC NOT = ORIGINAL-TY-REC
              PERFORM UNDERGONE-CHANGE
              PERFORM CHANGE-AGAIN
              GO TO END-WRITE-MODULE.
           MOVE CHANGED-TY-REC TO TY-REC.
           PERFORM REWRITE-TY1-FILE.

           MOVE "C" TO ERRLOGW-ACTION.
           MOVE TY1-KEY TO ERRLOGW-KEY.
           MOVE FORM-PATHNAME TO ERRLOGW-PROGX.
           MOVE "CL/ERRLOG" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH ERRLOGW-AREA.
           CANCEL FORM-PROGX.

           GO TO END-WRITE-MODULE.
       WRITE-RECORD.
           PERFORM WRITE-TY1-FILE.
           IF IO-FG NOT = 0
              PERFORM ALREADY-CREATED.
       END-WRITE-MODULE.
           PERFORM CLOSE-TY1-FILE.

      ****************************************
       DELETE-MODULE SECTION.
      ****************************************
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
                  NEXT SENTENCE
           ELSE
               GO TO END-DELETE-MODULE.

           PERFORM OPEN-TY1-FILE.
           PERFORM READ-TY1-FILE.
           IF IO-FG NOT = 0
              PERFORM UNDERGONE-CHANGE
              PERFORM RECORD-DELETED
              GO TO END-DELETE-MODULE.
           IF TY-REC NOT = ORIGINAL-TY-REC
              PERFORM UNDERGONE-CHANGE
              PERFORM RECORD-CHANGED
              GO TO END-DELETE-MODULE.
           PERFORM DELETE-TY1-FILE.

      *WRITE CHANGES FILE
           MOVE "C" TO ERRLOGW-ACTION.
           MOVE " " TO ERRLOGW-KEY
           STRING TY1-KEY, " ", "DELETED" DELIMITED BY SIZE
                INTO ERRLOGW-KEY.

           MOVE FORM-PATHNAME TO ERRLOGW-PROGX.

           MOVE "CL/ERRLOG" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH ERRLOGW-AREA.
           CANCEL FORM-PROGX.
       END-DELETE-MODULE.
           PERFORM CLOSE-TY1-FILE.

      ********************************************
       NEW-RECORD-SETUP SECTION.
      ********************************************
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
                     NEW-FLAG-SCR11.

           MOVE "S  A000NEW." TO VDU.
           MOVE "*** NEW RECORD ***" TO VDUBUF.
           PERFORM SCREENIO.
       CLEAR-NEW-RECORD.
           PERFORM CLEAR-TY-FILE.
           MOVE HOLD-TY1-KEY TO TY1-KEY.
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
                     ELEMENT.

      *************************************
       DISPLAY-PAGE SECTION.
      *************************************

           IF PAGE1 PERFORM DISPLAY-FIELDS
           ELSE IF PAGE2 PERFORM DISPLAY-FIELDS-2
           ELSE IF PAGE3 PERFORM DISPLAY-FIELDS-3
           ELSE IF PAGE4 PERFORM DISPLAY-FIELDS-4
           ELSE IF PAGE5 PERFORM DISPLAY-FIELDS-5
           ELSE IF PAGE6 PERFORM DISPLAY-FIELDS-6
           ELSE IF PAGE7 PERFORM DISPLAY-FIELDS-7
           ELSE IF PAGE8 PERFORM DISPLAY-FIELDS-8
           ELSE IF PAGE9 PERFORM DISPLAY-FIELDS-9
           ELSE IF PAGE10 PERFORM DISPLAY-FIELDS-10
           ELSE IF PAGE11 PERFORM DISPLAY-FIELDS-11.

       END-DISPLAY-PAGE.
           EXIT.

      *************************************
       DISPLAY-FIELDS SECTION.
      *************************************
           MOVE " " TO DISPLAY-BUF1.
           MOVE 0           TO F01.
           MOVE TY-OTHCOL   TO F01.
           MOVE TY-OT2COL   TO F02.
           MOVE TY-INTCOL   TO F03.
           MOVE TY-LATECOL  TO F04.
           MOVE TY-PRINCOL  TO F05.
           MOVE TY-DEFCOL   TO F06.
           MOVE TY-RNPRIN   TO F07.
           MOVE TY-PART-CHGOFF TO F08.
           MOVE TY-INTREB   TO F09.
           MOVE TY-CLREB1   TO F10.
           MOVE TY-CLREB2   TO F11.
           MOVE TY-CLREB3   TO F12.
           MOVE TY-CLREB4   TO F13.
           MOVE TY-AHREB    TO F14.
           MOVE TY-PPREB    TO F15.
           MOVE TY-O1REB    TO F16.
           MOVE TY-O2REB    TO F17.
           MOVE TY-O3REB    TO F18.
           MOVE TY-O4REB    TO F19.
           MOVE TY-O5REB    TO F20.
           MOVE TY-O6REB    TO F21.
           MOVE TY-O7REB    TO F22.
           MOVE TY-SCREB    TO F23.
           MOVE TY-MFREB    TO F24.
           MOVE TY-DFREB    TO F25.
           MOVE TY-BRFG     TO F26.

           MOVE DISPLAY-BUF1 TO WR-BUF.
           MOVE DISPLAY-LIST1 TO WR-LIST.
           PERFORM CALL-WRFORM.

       DISPLAY-FIELDS-EXIT.
           EXIT.

      *************************************
       DISPLAY-FIELDS-2 SECTION.
      *************************************
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
                  UNTIL SUB3 > 11.

           MOVE TY-SUMWGTTM TO DATA-120.
           MOVE TY-SUMWGTAV TO DATA-130.
           MOVE TY-SUMWGTTA TO DATA-140.

           MOVE DISPLAY-BUF2 TO WR-BUF.
           MOVE DISPLAY-LIST2 TO WR-LIST.
           PERFORM CALL-WRFORM.

           GO TO DISPLAY-FIELDS-EXIT-2.

       SETUP2.
           MOVE TY-NO(SUB2 SUB3) TO DATA-NO(SUB3).
           MOVE TY-AMT(SUB2 SUB3) TO DATA-AMT(SUB3).
           MOVE TY-O2(SUB2 SUB3) TO DATA-O2(SUB3).
           MOVE TY-INT(SUB2 SUB3) TO DATA-INT(SUB3).

       DISPLAY-FIELDS-EXIT-2.
           EXIT.

      *************************************
       DISPLAY-FIELDS-3 SECTION.
      *************************************

           PERFORM SETUP3 VARYING SUB3 FROM 1 BY 1
                                    UNTIL SUB3 > 10
                   AFTER SUB2 FROM 1 BY 1
                            UNTIL SUB2 > 2.

           MOVE DISPLAY-BUF3 TO WR-BUF.
           MOVE DISPLAY-LIST3 TO WR-LIST.
           PERFORM CALL-WRFORM.
           GO TO DISPLAY-FIELDS-EXIT-3.

       SETUP3.
           IF ( HEADER-LEV-STD )
              MOVE TY-UNITS (SUB2 SUB3)        TO DEL-UNITS (SUB3 SUB2)
              MOVE TY-OUTBAL(SUB2 SUB3)        TO DEL-OUTBAL(SUB3 SUB2)
           ELSE
              MOVE TY-UNITS-BNKRPT (SUB2 SUB3) TO DEL-UNITS (SUB3 SUB2)
              MOVE TY-OUTBAL-BNKRPT(SUB2 SUB3) TO DEL-OUTBAL(SUB3 SUB2).

       DISPLAY-FIELDS-EXIT-3.
           EXIT.

      *************************************
       DISPLAY-FIELDS-4 SECTION.
      *************************************
           MOVE " " TO DISPLAY-BUF4.
           MOVE TY-ALLOTMENTS        TO I01.
           MOVE TY-PROCEEDS          TO I02.
           MOVE TY-LOCKBOX           TO I03.
           MOVE TY-OTH-NO-COL        TO I04.
           MOVE TY-OT2-NO-COL        TO I05.
           MOVE TY-INT-NO-COL        TO I06.
           MOVE TY-LATE-NO-COL       TO I07.
           MOVE TY-OTH-NO-WAIVED     TO I08.
           MOVE TY-OT2-NO-WAIVED     TO I09.
           MOVE TY-INT-NO-WAIVED     TO I10.
           MOVE TY-LATE-NO-WAIVED    TO I11.
           MOVE TY-OTH-AMT-WAIVED    TO I12.
           MOVE TY-OT2-AMT-WAIVED    TO I13.
           MOVE TY-INT-AMT-WAIVED    TO I14.
           MOVE TY-LATE-AMT-WAIVED   TO I15.

           MOVE DISPLAY-BUF4 TO WR-BUF.
           MOVE DISPLAY-LIST4 TO WR-LIST.
           PERFORM CALL-WRFORM.

       DISPLAY-FIELDS-EXIT-4.
           EXIT.

      *************************************
       DISPLAY-FIELDS-5 SECTION.
      *************************************
           MOVE " " TO DISPLAY-BUF5.
           PERFORM VARYING SUB5 FROM 1 BY 1 UNTIL SUB5 > 16
              MOVE TY-CKWRITTEN(SUB5) TO CHECKS-WRITTEN(SUB5)
           END-PERFORM.

           MOVE TY-NO-SOLD TO DATA-NO-SOLD.
           MOVE TY-AMT-SOLD TO DATA-AMT-SOLD.
           MOVE TY-NO-TRANS TO DATA-NO-TRANS.
           MOVE TY-AMT-TRANS TO DATA-AMT-TRANS.
           MOVE TY-NO-PURCH TO DATA-NO-PURCH.
           MOVE TY-AMT-PURCH TO DATA-AMT-PURCH.

           MOVE DISPLAY-BUF5 TO WR-BUF.
           MOVE DISPLAY-LIST5 TO WR-LIST.
           PERFORM CALL-WRFORM.

       DISPLAY-FIELDS-EXIT-5.
           EXIT.

      *************************************
       DISPLAY-FIELDS-6 SECTION.
      *************************************
           MOVE ACT-TYPE(ACTIVITY) TO ACT-BUFFER.
           MOVE ACT-BUFFER TO WR-BUF.
           MOVE ACT-FNAME TO WR-LIST.
           PERFORM CALL-WRFORM.

           MOVE ACT-TYPEL(ACTIVITY) TO ACT-BUFFER.
           MOVE ACT-BUFFER TO WR-BUF.
           MOVE ACT-FNAMEL TO WR-LIST.
           PERFORM CALL-WRFORM.

           MOVE ACTIVITY TO SUB2.
           PERFORM SETUP6 VARYING SUB3 FROM 1 BY 1
                  UNTIL SUB3 > 5.

           PERFORM SETUP6B VARYING SUB3 FROM 1 BY 1
                  UNTIL SUB3 > 3.
           MOVE TY-DLPARVHELD(SUB2) TO DATA-DLPARVHELD.
           MOVE TY-DLDISC TO DATA-DLDISC.

           MOVE DISPLAY-BUF6 TO WR-BUF.
           MOVE DISPLAY-LIST6 TO WR-LIST.
           PERFORM CALL-WRFORM.

           GO TO DISPLAY-FIELDS-EXIT-6.

       SETUP6.
           MOVE TY-NO-REPO(SUB2 SUB3) TO DATA-NO-REPO(SUB3).
           MOVE TY-AMT-REPO(SUB2 SUB3) TO DATA-AMT-REPO(SUB3).
           MOVE TY-O2-REPO(SUB2 SUB3) TO DATA-O2-REPO(SUB3).
           MOVE TY-INT-REPO(SUB2 SUB3) TO DATA-INT-REPO(SUB3).
       SETUP6B.
           MOVE TY-DLRVPRIN(SUB2 SUB3) TO DATA-DLRVPRIN(SUB3).
           MOVE TY-DLRVCHGS(SUB2 SUB3) TO DATA-DLRVCHGS(SUB3).
       DISPLAY-FIELDS-EXIT-6.
           EXIT.

      *************************************
       DISPLAY-FIELDS-7 SECTION.
      *************************************
           MOVE ACT-TYPE(ACTIVITY) TO ACT-BUFFER.
           MOVE ACT-BUFFER TO WR-BUF.
           MOVE ACT-FNAME TO WR-LIST.
           PERFORM CALL-WRFORM.

           MOVE ACT-TYPEL(ACTIVITY) TO ACT-BUFFER.
           MOVE ACT-BUFFER TO WR-BUF.
           MOVE ACT-FNAMEL TO WR-LIST.
           PERFORM CALL-WRFORM.

           MOVE ACTIVITY TO SUB7.
           PERFORM SETUP7
                   VARYING SUB3 FROM 1 BY 1 UNTIL SUB3 > 10.
           MOVE TY-PART-CHGOFF TO DATA-PART-CHGOFF.

           MOVE DISPLAY-BUF7 TO WR-BUF.
           MOVE DISPLAY-LIST7 TO WR-LIST.
           PERFORM CALL-WRFORM.

           GO TO DISPLAY-FIELDS-EXIT-7.

       SETUP7.
           MOVE TY-NO-PL(SUB7 SUB3) TO DATA-NO-PL(SUB3).
           MOVE TY-AMT-PL(SUB7 SUB3) TO DATA-AMT-PL(SUB3).
           MOVE TY-O2-PL(SUB7 SUB3) TO DATA-O2-PL(SUB3).
           MOVE TY-INT-PL(SUB7 SUB3) TO DATA-INT-PL(SUB3).
       DISPLAY-FIELDS-EXIT-7.
           EXIT.

      *************************************
       DISPLAY-FIELDS-8 SECTION.
      *************************************

           PERFORM SETUP8 VARYING SUB3 FROM 1 BY 1
                  UNTIL SUB3 > 5.

           MOVE DISPLAY-BUF8 TO WR-BUF.
           MOVE DISPLAY-LIST8 TO WR-LIST.
           PERFORM CALL-WRFORM.

           GO TO DISPLAY-FIELDS-EXIT-8.

       SETUP8.
           MOVE TY-PL-PRINCOL(SUB3) TO DATA-PL-PRINCOL(SUB3).
           MOVE TY-PL-INTCOL(SUB3) TO DATA-PL-INTCOL(SUB3).
           MOVE TY-PL-LATECOL(SUB3) TO DATA-PL-LATECOL(SUB3).
           MOVE TY-PL-O2COL(SUB3) TO DATA-PL-O2COL(SUB3).
       DISPLAY-FIELDS-EXIT-8.
           EXIT.

      *************************************
       DISPLAY-FIELDS-9 SECTION.
      *************************************

           MOVE SPACES TO DISPLAY-BUF9.

           MOVE DISPLAY-BUF9 TO WR-BUF.
           MOVE DISPLAY-LIST9 TO WR-LIST.
           PERFORM CALL-WRFORM.

       DISPLAY-FIELDS-EXIT-9.
           EXIT.

      *************************************
       DISPLAY-FIELDS-10 SECTION.
      *************************************

           PERFORM DISPLAY-FIELDS-SETUP10
                   VARYING SUB1 FROM 1 BY 1 UNTIL SUB1 > 10.

           MOVE DISPLAY-BUF10 TO WR-BUF.
           MOVE DISPLAY-LIST10 TO WR-LIST.
           PERFORM CALL-WRFORM.

           GO TO DISPLAY-FIELDS-EXIT-10.

       DISPLAY-FIELDS-SETUP10.
           MOVE TY-INS-NO-ELIG(SUB1) TO DATA-INS-NO-ELIG(SUB1).
           MOVE TY-INS-NO-WROTE(SUB1) TO DATA-INS-NO-WROTE(SUB1).
           MOVE TY-INS-NO-NOTWROTE(SUB1) TO DATA-INS-NO-NOTWROTE(SUB1).
           MOVE TY-INS-AMT-WROTE(SUB1) TO DATA-INS-AMT-WROTE(SUB1).
           IF SUB1 < 4
             MOVE TY-FEE-NO-ELIG(SUB1)     TO DATA-FEE-NO-ELIG(SUB1)
             MOVE TY-FEE-NO-WROTE(SUB1)    TO DATA-FEE-NO-WROTE(SUB1)
             MOVE TY-FEE-NO-NOTWROTE(SUB1) TO DATA-FEE-NO-NOTWROTE(SUB1)
             MOVE TY-FEE-AMT-WROTE(SUB1)   TO DATA-FEE-AMT-WROTE(SUB1).

       DISPLAY-FIELDS-EXIT-10.
           EXIT.

      *************************************
       DISPLAY-FIELDS-11 SECTION.
      *************************************

           PERFORM VARYING SUB1 FROM 1 BY 1 UNTIL SUB1 > 2
                     AFTER SUB2 FROM 1 BY 1 UNTIL SUB2 > 6
              MOVE TY-UNEARNED-AMT(SUB1 SUB2) TO
                                          P11-DISP-AMT(SUB2 SUB1)
           END-PERFORM.

           MOVE DISPLAY-BUF11 TO WR-BUF.
           MOVE DISPLAY-LIST11 TO WR-LIST.
           PERFORM CALL-WRFORM.

       DISPLAY-FIELDS-EXIT-11.
           EXIT.

      *************************************
       NEW-ACTIVITY SECTION.
      *************************************
           IF VDUSTATUS = "18"
               MOVE 1 TO ACTIVITY
           ELSE
               IF VDUSTATUS = "1:"
                   MOVE 2 TO ACTIVITY.
           IF ACTIVITY = ACTYP-COD
               GO TO NEW-ACTIVITY-EXIT.
           MOVE ACTIVITY TO ACTYP-COD.
           MOVE ACT-TYPE(ACTIVITY) TO ACT-BUFFER.
           MOVE ACT-BUFFER TO WR-BUF.
           MOVE ACT-FNAME TO WR-LIST.
           PERFORM CALL-WRFORM.
           PERFORM DISPLAY-PAGE.

       NEW-ACTIVITY-EXIT.
           EXIT.

      *************************************
       TOGGLE-ON-HEADER SECTION.
      *************************************

           IF ( F2-KEY )
              MOVE 1 TO HEADER-LEV
           ELSE
           IF ( F3-KEY )
              MOVE 2 TO HEADER-LEV.

           IF ( HEADER-LEV = HEADER-LEVEL )
              GO TO EXIT-TOGGLE-ON-HEADER.

           MOVE HEADER-LEV TO HEADER-LEVEL.

           MOVE HEADER-TYPE(HEADER-LEV) TO HEADER-BUFFER.

       END-TOGGLE-ON-HEADER.

           MOVE HEADER-BUFFER TO WR-BUF.
           MOVE HEADER-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.
           PERFORM DISPLAY-PAGE.

       EXIT-TOGGLE-ON-HEADER.
           EXIT.

      *******************************************************
       FORM-RESET SECTION.
      *******************************************************
           DISPLAY SPACES UPON TYMAIN-WINDOW-HANDLE
                               LINE 0 COL 0 ERASE SCREEN.
           EVALUATE PAGE-FLAG
              WHEN  1 MOVE EXT-CONAME-CONAME TO TYMAN1-CONAME
                      MOVE EXT-CONAME-SYSDATE TO TYMAN1-SYSDATE
                      DISPLAY TYMAN1-SCREEN UPON TYMAIN-WINDOW-HANDLE
                      MOVE TYMAN1-FORM-FIELDS TO WR-BUF 
                      MOVE TYMAN1-FORM-LIST   TO WR-LIST 
              WHEN  2 MOVE EXT-CONAME-CONAME TO TYMAN2-CONAME
                      MOVE EXT-CONAME-SYSDATE TO TYMAN2-SYSDATE
                      DISPLAY TYMAN2-SCREEN UPON TYMAIN-WINDOW-HANDLE
                      MOVE TYMAN2-FORM-FIELDS TO WR-BUF 
                      MOVE TYMAN2-FORM-LIST   TO WR-LIST 
              WHEN  3 MOVE EXT-CONAME-SYSDATE TO TYMAN3-SYSDATE
                      DISPLAY TYMAN3-SCREEN UPON TYMAIN-WINDOW-HANDLE
                      MOVE TYMAN3-FORM-FIELDS TO WR-BUF 
                      MOVE TYMAN3-FORM-LIST   TO WR-LIST 
              WHEN  4 MOVE EXT-CONAME-CONAME TO TYMAN4-CONAME
                      MOVE EXT-CONAME-SYSDATE TO TYMAN4-SYSDATE
                      DISPLAY TYMAN4-SCREEN UPON TYMAIN-WINDOW-HANDLE
                      MOVE TYMAN4-FORM-FIELDS TO WR-BUF 
                      MOVE TYMAN4-FORM-LIST   TO WR-LIST 
              WHEN  5 MOVE EXT-CONAME-CONAME TO TYMAN5-CONAME
                      MOVE EXT-CONAME-SYSDATE TO TYMAN5-SYSDATE
                      DISPLAY TYMAN5-SCREEN UPON TYMAIN-WINDOW-HANDLE
                      MOVE TYMAN5-FORM-FIELDS TO WR-BUF 
                      MOVE TYMAN5-FORM-LIST   TO WR-LIST 
              WHEN  6 MOVE EXT-CONAME-CONAME TO TYMAN6-CONAME
                      MOVE EXT-CONAME-SYSDATE TO TYMAN6-SYSDATE
                      DISPLAY TYMAN6-SCREEN UPON TYMAIN-WINDOW-HANDLE
                      MOVE TYMAN6-FORM-FIELDS TO WR-BUF 
                      MOVE TYMAN6-FORM-LIST   TO WR-LIST 
              WHEN  7 MOVE EXT-CONAME-CONAME TO TYMAN7-CONAME
                      MOVE EXT-CONAME-SYSDATE TO TYMAN7-SYSDATE
                      DISPLAY TYMAN7-SCREEN UPON TYMAIN-WINDOW-HANDLE
                      MOVE TYMAN7-FORM-FIELDS TO WR-BUF 
                      MOVE TYMAN7-FORM-LIST   TO WR-LIST 
              WHEN  8 MOVE EXT-CONAME-CONAME TO TYMAN8-CONAME
                      MOVE EXT-CONAME-SYSDATE TO TYMAN8-SYSDATE
                      DISPLAY TYMAN8-SCREEN UPON TYMAIN-WINDOW-HANDLE
                      MOVE TYMAN8-FORM-FIELDS TO WR-BUF 
                      MOVE TYMAN8-FORM-LIST   TO WR-LIST 
              WHEN  9 MOVE EXT-CONAME-CONAME TO TYMAN9-CONAME
                      MOVE EXT-CONAME-SYSDATE TO TYMAN9-SYSDATE
                      DISPLAY TYMAN9-SCREEN UPON TYMAIN-WINDOW-HANDLE
                      MOVE TYMAN9-FORM-FIELDS TO WR-BUF 
                      MOVE TYMAN9-FORM-LIST   TO WR-LIST 
              WHEN 10 MOVE EXT-CONAME-CONAME TO TYMANA-CONAME
                      MOVE EXT-CONAME-SYSDATE TO TYMANA-SYSDATE
                      DISPLAY TYMANA-SCREEN UPON TYMAIN-WINDOW-HANDLE
                      MOVE TYMANA-FORM-FIELDS TO WR-BUF 
                      MOVE TYMANA-FORM-LIST   TO WR-LIST 
              WHEN OTHER MOVE EXT-CONAME-CONAME TO TYMANB-CONAME
                      MOVE EXT-CONAME-SYSDATE TO TYMANB-SYSDATE
                      DISPLAY TYMANB-SCREEN UPON TYMAIN-WINDOW-HANDLE
                      MOVE TYMANB-FORM-FIELDS TO WR-BUF 
                      MOVE TYMANB-FORM-LIST   TO WR-LIST 
           END-EVALUATE.
           PERFORM CALL-WRFORM.
           
       FORM-RESET-EXIT.
           EXIT.

      *******************************************************
       FORM-DESTROY SECTION.
      *******************************************************
           EVALUATE PAGE-FLAG
              WHEN  1 DESTROY TYMAN1-SCREEN
              WHEN  2 DESTROY TYMAN2-SCREEN
              WHEN  3 DESTROY TYMAN3-SCREEN
              WHEN  4 DESTROY TYMAN4-SCREEN
              WHEN  5 DESTROY TYMAN5-SCREEN
              WHEN  6 DESTROY TYMAN6-SCREEN
              WHEN  7 DESTROY TYMAN7-SCREEN
              WHEN  8 DESTROY TYMAN8-SCREEN
              WHEN  9 DESTROY TYMAN9-SCREEN
              WHEN  10 DESTROY TYMANA-SCREEN
              WHEN OTHER DESTROY TYMANB-SCREEN
           END-EVALUATE.

      *******************************************************
       SETUP-LINK-INFO SECTION.
      *******************************************************
           EVALUATE PAGE-FLAG
              WHEN  1 MOVE TYMAN1-HELP--FILE TO HELP-LINK-FILE
                      MOVE TYMAN1-HELP--DIR  TO HELP-LINK-DIR
              WHEN  2 MOVE TYMAN2-HELP--FILE TO HELP-LINK-FILE
                      MOVE TYMAN2-HELP--DIR  TO HELP-LINK-DIR
              WHEN  3 MOVE TYMAN3-HELP--FILE TO HELP-LINK-FILE
                      MOVE TYMAN3-HELP--DIR  TO HELP-LINK-DIR
              WHEN  4 MOVE TYMAN4-HELP--FILE TO HELP-LINK-FILE
                      MOVE TYMAN4-HELP--DIR  TO HELP-LINK-DIR
              WHEN  5 MOVE TYMAN5-HELP--FILE TO HELP-LINK-FILE
                      MOVE TYMAN5-HELP--DIR  TO HELP-LINK-DIR
              WHEN  6 MOVE TYMAN6-HELP--FILE TO HELP-LINK-FILE
                      MOVE TYMAN6-HELP--DIR  TO HELP-LINK-DIR
              WHEN  7 MOVE TYMAN7-HELP--FILE TO HELP-LINK-FILE
                      MOVE TYMAN7-HELP--DIR  TO HELP-LINK-DIR
              WHEN  8 MOVE TYMAN8-HELP--FILE TO HELP-LINK-FILE
                      MOVE TYMAN8-HELP--DIR  TO HELP-LINK-DIR
              WHEN  9 MOVE TYMAN9-HELP--FILE TO HELP-LINK-FILE
                      MOVE TYMAN9-HELP--DIR  TO HELP-LINK-DIR
              WHEN 10 MOVE TYMANA-HELP--FILE TO HELP-LINK-FILE
                      MOVE TYMANA-HELP--DIR  TO HELP-LINK-DIR
              WHEN OTHER MOVE TYMANB-HELP--FILE TO HELP-LINK-FILE
                      MOVE TYMANB-HELP--DIR  TO HELP-LINK-DIR
           END-EVALUATE.

      *******************************************************
       VDU-CONVERT-FIELD SECTION.
      *******************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.

      * FORCED TO BREAK APART EVALUATE STATEMENT.
      * TO MANY ARGUMENTS WERE CAUSING COMPILE ERROR.

              EVALUATE TRUE
                 COPY "LIBMN/TYMAN1_EVA.CPY".
                 COPY "LIBMN/TYMAN2_EVA.CPY".
                 COPY "LIBMN/TYMAN3_EVA.CPY".
                 COPY "LIBMN/TYMAN4_EVA.CPY".
                 COPY "LIBMN/TYMAN5_EVA.CPY".
                 COPY "LIBMN/TYMAN6_EVA.CPY".
                 COPY "LIBMN/TYMAN7_EVA.CPY".
                 COPY "LIBMN/TYMAN8_EVA.CPY".
                 COPY "LIBMN/TYMAN9_EVA.CPY".
                 COPY "LIBMN/TYMANA_EVA.CPY".
                 COPY "LIBMN/TYMANB_EVA.CPY".
                 WHEN OTHER       MOVE   "2A" TO STAT
              END-EVALUATE.

      ******************************************************************
      *
      *    M I S C E L L E N O U S - P A R A G R A P H S
      *
      ******************************************************************
       COPY "LIBLP/LPTYCL.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/GPENV.CPY".

      ******************************************************************
       MISC-PARAGRAPHS SECTION.
      ******************************************************************
       COPY "LIBGB/ACCESS.CPY".
      *-----------------------------------------------------------------
       RECORD-DELETED.
           MOVE "RECORD WAS DELETED!" TO MESS.
           PERFORM SEND-MESS.

      *-----------------------------------------------------------------
       RECORD-CHANGED.
           MOVE "CHANGES WERE MADE TO THIS RECORD!" TO MESS.
           PERFORM SEND-MESS.

      *-----------------------------------------------------------------
       CHANGE-AGAIN.
           MOVE "PLEASE RE-INPUT YOUR CHANGES!" TO MESS.
           PERFORM SEND-MESS.

      *-----------------------------------------------------------------
       ALREADY-CREATED.
           MOVE "RECORD HAS BEEN ALREADY CREATED!" TO MESS.
           PERFORM SEND-MESS.

      *-----------------------------------------------------------------
       UNDERGONE-CHANGE.
           MOVE
           "THIS RECORD HAS UNDERGONE CHANGE BY ANOTHER USER,"
           TO MESS.
           PERFORM SEND-MESS.

      *-----------------------------------------------------------------
       SWITCH-FORM.
           PERFORM WRITE-MODULE.
           MOVE TY-REC TO ORIGINAL-TY-REC.
           MOVE 0 TO NEW-FLAG-SCR2
                     NEW-FLAG-SCR3
                     NEW-FLAG-SCR4
                     NEW-FLAG-SCR5
                     NEW-FLAG-SCR6
                     NEW-FLAG-SCR7
                     NEW-FLAG-SCR8
                     NEW-FLAG-SCR9
                     NEW-FLAG-SCR10
                     NEW-FLAG-SCR11
                     NEW-FLAG.

           PERFORM FORM-DESTROY.
           IF ( ACTION = "   P11" )
              MOVE 11 TO PAGE-FLAG
           ELSE
           IF ( ACTION = "   P10" )
              MOVE 10 TO PAGE-FLAG
           ELSE
           IF ( ACTION = "    P9" )
              MOVE 9 TO PAGE-FLAG
           ELSE
           IF ( ACTION = "    P8" )
              MOVE 8 TO PAGE-FLAG
           ELSE
           IF ( ACTION = "    P7" )
              MOVE 1 TO ACTIVITY ACTYP-COD
              MOVE 7 TO PAGE-FLAG
           ELSE
           IF ( ACTION = "    P6" )
              MOVE 1 TO ACTIVITY ACTYP-COD
              MOVE 6 TO PAGE-FLAG
           ELSE
           IF ( ACTION = "    P5" )
              MOVE 5 TO PAGE-FLAG
           ELSE
           IF ( ACTION = "    P4" )
              MOVE 4 TO PAGE-FLAG
           ELSE
           IF ( ACTION = "    P3" )
              MOVE 3 TO PAGE-FLAG
           ELSE
           IF ( ACTION = "    P2" )
              MOVE 1 TO ACTIVITY ACTYP-COD
              MOVE 2 TO PAGE-FLAG
           ELSE
           IF ( ACTION = "    P1" )
              MOVE 1 TO PAGE-FLAG.

           MOVE FORM-NAMEX(PAGE-FLAG) TO FORM-NAM.
           MOVE FORM-VFN(PAGE-FLAG) TO VDU-FORM-NAME.
           MOVE FORM-PATH            TO FORM-PATHNAME.
           PERFORM FORM-RESET.

           IF ( PAGE2 )   
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

           MOVE "SN 8060DATE."             TO VDU.
           MOVE HOLD-TY-DATE               TO VDU-DATE-YYYYMMDD.
           PERFORM SCREENIO.

           MOVE HOLD-TY-BRNO      TO DISP-BRNO.
           MOVE HOLD-BRNAME    TO DISP-BRNAME.
           MOVE HOLD-TY-CLASS     TO DISP-LC.
           MOVE HOLD-LCDESC    TO DISP-LCDESC.

           MOVE DISPLAY-KEY TO WR-BUF.
           MOVE DISP1-KEY-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

           PERFORM DISPLAY-PAGE.

      ******************************************************************
       IO-ROUTINES SECTION.
      ******************************************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/DECLARE_SQL.CPY".
           PERFORM CLOSE-TY1-FILE.
           PERFORM CLOSE-LC1-FILE.
           PERFORM CLOSE-BR-FILE.
       COPY "LIBLP/LPTYGS_SQL.CPY".
       COPY "LIBLP/LPLCGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBLP/LPLC1RN.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBLP/LPTY1IN.CPY".
       COPY "LIBGB/FERRORS.CPY".

      ******************************************************************
       END-ROUTINE SECTION.
      ******************************************************************
       END-RUN.
           PERFORM CLOSE-FILES.

       EXIT-PROG.
           MOVE "LP/LPM2MU" TO FORM-NAM.
           MOVE FORM-PATH TO FORM-PATHNAME.

       STOP-RUN.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN  
           DESTROY TYMAIN-WINDOW-HANDLE
           MOVE FORM-PATH TO FORM-PATHNAME.
           EXIT PROGRAM.
