      * COPYMEMBER: LIBGB/DATERW
      *****************************************************************
      *      (DATERW)   DATE ROUTINE WORKERS
      *
      * REV:
      *  JTG 030295 REMOVED JULIAN-YY & JULIAN-DD
      *  MJD 103097 ADDED 88 LEVELS SYSDATE-??-NUMDATE.
      *  JTG 080398 CHANGED 'DATE-IS-..' TO 'SYS-IS..'
      *  KEC 100898 ADDED WORKERS FOR "YEAR 2000" ROUTINES.
      *  JTG 121698 ADDE DATER-REVOLVING FOR DISPLAY OF 'R' REVOLVING
      *  MJD 990224 ADDED DATE-MMDDYYYY FOR Y2K CONVERSION
      *  KEC 990330 ADDED SOME MORE DATE CONVERSION WORKERS
      *  KEC 000204 ADDED SOME MORE DATE CONVERSION WORKERS
      *              DATE-ONE DATE-TWO   (MMYY/MMYYYY/YYMM/YYYMM/YYYYMM)
      *  JTG 061122 CHANGED THE INITIAL VALUE OF ELAPSED-YRTYPE VALUE '0'
      *             TO ELAPSED-YRTYPE VALUE 365
      * MJD 130130 REPLACED "C" CALLS WITH VERYAND COBOL VERSIONS OF
      *            PARADATA DATE ROUTINES.   THIS IS AN INTERNAL
      *            CHANGE, THE WAY IN WHICH WE CALL AND USE THE
      *            DATE ROUTINES REMAINS UNCHANGED. DATE WORKERS
      *            FOR THESE ROUTINES HAVE BEEN ADDED TO THIS WORKER.
      * MJD 130213 ADDED DATER-HOLD-NDTE-DATE.  TIMALL REUSES NDTE DATE
      *            EVEN THOUGH IT IS NOT AN INPUT FOR IT BECAUSE IT CALLS
      *            NEWDATE.  THIS CAUSES ISSUES BECAUSE THE CALLING
      *            PROGRAM HAS ALREADY SET NDTE-DATE AND DOES NOT EXPECT IT
      *            TO CHANGE WHEN CALLING TIMALL.  TO FIX THIS I SAVE
      *            NDTE-DATE AND RESTORE IT AFTER THE CALL.
      * MJD 130219 CHANGED THE PIC FOR CJWS-MOD-JUL FROM S(2) TO S(4).
      *            THE CALCULATION WAS OVERFLOWING AND RETURING ERRANT RESULTS.
      * MJD 120313 ADDED NOT-YEAR.  NEEDED FOR A CHANGE VERYANT MADE TO CORRECT
      *            A PROBLEM WITH ELAPSED TIME AND LEAP YEARS.
      * MJD 130509 ADDED WS-JUL-WK TO REPLACE JULIAN-DATE FOR INTERNAL
      *            CALCULATIONS. EDWIN (VERYANT) WAS USING JULIAN-DATE FROM
      *            BUFFER AND WE DO NOT WANT TO CHANGE THE VALUE UNLESS
      *            IT IS ACTUALLY PART OF THE INPUT/OUTPUT OF THE ROUTINE
      *            CALLED.
      * MJD 100413 ADDED WS-DATE1-YYYY AND WS-DATE2-YYYY.  NEEDED FOR ELAPSED
      *            CALCS THAT SPAN A CENTURY DATE.
      *            EXPANDED WS-NDTE-YY-S, WS-UP-WORK TO 4 DIGITS
      * MJD 140507 ADDED WS-NDTE-MM-VALID AND WS-NDTE-DD-VALID.
      *            NEEDED TO TEST VALID MON AND DAY ON TSTDAT.
      * MJD 151023 ADAPTING ROUTINE TO WORK WITH CCYYMMDD FORMATTED DATE TYPES.
      * MJD 170207 COMMENTED OUT WS-DATE1-YYYY AND WS-DATE2-YYYY
      *            IT HAS BEEN REPLACED WITH WS-DATE1-CCYY AND WS-DATE2-CCYY
      * BLM 170309 BROUGHT OVER FROM A39 (CAME FROM DATER8W WITH A FEW
      *            TWEAKS), PD0006
      * MJD 170505 EXPANDED ELAPSED-???? FIELDS TO MATCH INTERNAL
      *            WORKERS AND ADDED TWO BYTED TO DAYS/UNITPER FOR
      *            GOOD MEASURE.   INTERNAL WORKERS WERE EXPANDED FOR
      *            8 DIGIT DATES BUT EXTERNAL BUFFER WAS NOT LARGE ENOUGH
      *            TO HANDLE EXPANDED RANGE IN WHICH WE CAN NOW CALCULATE
      *            ELAPSED-TIME. EXPANDED WS-HOLD-MOS WORKER FROM 9(3) TO 9(7)
      *            THIS IS ONLY USED WITH 362 YEARTYPES
      * BAH 20190625 REMOVED DATER-REVOLVING, PT0153
      * BLM 20210830 CHANGE SEVERAL DATE WORKERS TO BE 9(8) INSTEAD OF
      *              GROUP (ALPHA) FIELDS.  GROUP ITEMS DID NOT PASS
      *              COMPARISON TO 0.  WS-NDTE, WS-DATE1, WS-DATE2, 
      *              WS-DATE1W, WS-DATE2W, WS-DATEX, WS-CJUL-OUT,
      *              CJUL-OUT, ND-WS-NDTE, CALL-DATE
      * JKC 20240113 COMMENT OUT JULIAN-BEG, JULIAN-END, JULIAN-CC;
      *              THEY ARE NOW EXTERNAL FIELDS POPULATED IN
      *              GB/SETENV SO TO WE CAN MORE EASILY CHANGE
      *              'SLIDING CENTURY WINDOW' IN THE FUTURE.
      * BAH 2024.0808 UNCOMMENTED NOT-YEAR FOR LEAP YEAR FIX S35Q-105
      *****************************************************************
      *----------------------------------------------------------------
      *    COBOL BUFFER USED IN 'C' ROUTINES
      *----------------------------------------------------------------
       01  DATER-STAT                  PIC XX    VALUE "11".
       01  DATER-BUFFER.
           03  DATER-VECTOR            PIC XX    VALUE "13".
           03  DATER-ACTION-CODE       PIC X     VALUE " ".
      * DO NOT CLEAR JULIAN-DATE ANYWHERE!!! CODE MUST STAY LIKE A15
      * THERE ARE ROUTINES THAT EXPECT BACK SPACES OR COMPILED DEFAULT
      * LIKE REBATE/SELECT-SP-ELAPSED-TIMTBL, WAS NOT GETTING ANY REBATES
           03  JULIAN-DATE             PIC  9(5).
           03  NUM-DATE                PIC  9(8)  VALUE 0.
           03  FILLER                 REDEFINES NUM-DATE.
               05 NUM-CCYY               PIC 9(4).
               05 NUM-CCYYX REDEFINES NUM-CCYY.
                  07  NUM-CC              PIC  9(2).  
                  07  NUM-YR              PIC  9(2).
               05  NUM-MO              PIC  9(2).
               05  NUM-DA              PIC  9(2).
           03  SYS-DATE                PIC  9(8)  VALUE 0.
           03  FILLER                 REDEFINES SYS-DATE.
               05 S-CCYY               PIC 9(4).
               05 S-CCYYX REDEFINES S-CCYY.
                  07  S-CC                PIC  9(2).
                  07  S-YY                PIC  9(2).
               05  S-MM                PIC  9(2).
               05  S-DD                PIC  9(2).
           03  NDTE-DATE               PIC  9(8)  VALUE 0.
           03  NDTE-DATE-WORK         REDEFINES NDTE-DATE
                                       PIC  9(8).
           03  FILLER                 REDEFINES NDTE-DATE.
               05 NDTE-CCYY            PIC 9(4).
               05 NDTE-CCYYX REDEFINES NDTE-CCYY.
                  07  NDTE-CC             PIC  9(2).
                  07  NDTE-YY             PIC  9(2).
               05  NDTE-MM             PIC  9(2).
               05  NDTE-DD             PIC  9(2).
           03  NDTE-HOLD               PIC S9(5) LEADING SEPARATE
                                                          VALUE 0.
           03  DATER-UNITPER-INFO.
               05  DATER-UNITPER-CD    PIC X              VALUE "M".
               05  DATER-UNITPER-FREQ  PIC  9(3)          VALUE 1.
           03  ELAPSED-YRTYPE          PIC  9(3)          VALUE 365.
           03  ELAPSED-MONTHS          PIC S9(7) COMP-3   VALUE 0.
           03  ELAPSED-DAYS            PIC S9(9) COMP-3   VALUE 0.
           03  ELAPSED-REM             PIC S9(7) COMP-3   VALUE 0.
           03  ELAPSED-UNITPER         PIC S9(9) COMP-3   VALUE 0.
           03  ELAPSED-UNITPER-REM     PIC S9(7) COMP-3   VALUE 0.

           03  ELAPSED-RESULTS         PIC X              VALUE " ".
               88  SYS-IS-EQ                              VALUE "E".
               88  SYS-IS-GE                              VALUE "E" "G".
               88  SYS-IS-GR                              VALUE "G".
               88  SYS-IS-GT                              VALUE "G".
               88  SYS-IS-LE                              VALUE "L" "E".
               88  SYS-IS-LT                              VALUE "L".
               88  SYS-IS-NE                              VALUE "L" "G".
               88  SYSDATE-EQ-NUMDATE                     VALUE "E".
               88  SYSDATE-GE-NUMDATE                     VALUE "E" "G".
               88  SYSDATE-GR-NUMDATE                     VALUE "G".
               88  SYSDATE-GT-NUMDATE                     VALUE "G".
               88  SYSDATE-LE-NUMDATE                     VALUE "L" "E".
               88  SYSDATE-LT-NUMDATE                     VALUE "L".
               88  SYSDATE-NE-NUMDATE                     VALUE "L" "G".

           03  DAY--OF-WEEK            PIC  9             VALUE 0.
               88  DAY-IS-SUN          VALUE 1.
               88  DAY-IS-MON          VALUE 2.
               88  DAY-IS-TUE          VALUE 3.
               88  DAY-IS-WED          VALUE 4.
               88  DAY-IS-THU          VALUE 5.
               88  DAY-IS-FRI          VALUE 6.
               88  DAY-IS-SAT          VALUE 7.
      *----------------------------------------------------------------
           03  SYS-FG                  PIC X(1).
           03  NUM-FG                  PIC X(1).
           03  NDTE-FG                  PIC X(1).

      *----------------------------------------------------------------
      *  GENERAL WORKERS FOR VERYANT COBOL VERSION OF DATER CALCULATION
      *----------------------------------------------------------------
      *COPY "LIBGB/DATERDYTOM.CPY".
      ************************************************
      *  VERYANT
      *
      *  DYTOM TABLE TO BE USED ON DATER ROUTINES
      *
      ************************************************
       01 WS-DYTOM.
          05 DYTOM-VALUES.
             10 FILLER     PIC 9(3) VALUE 0.
             10 FILLER     PIC 9(3) VALUE 31.
             10 FILLER     PIC 9(3) VALUE 59.
             10 FILLER     PIC 9(3) VALUE 90.
             10 FILLER     PIC 9(3) VALUE 120.
             10 FILLER     PIC 9(3) VALUE 151.
             10 FILLER     PIC 9(3) VALUE 181.
             10 FILLER     PIC 9(3) VALUE 212.
             10 FILLER     PIC 9(3) VALUE 243.
             10 FILLER     PIC 9(3) VALUE 273.
             10 FILLER     PIC 9(3) VALUE 304.
             10 FILLER     PIC 9(3) VALUE 334.
             10 FILLER     PIC 9(3) VALUE 365.
          05 FILLER REDEFINES DYTOM-VALUES.
             10 DYTOM OCCURS 13 TIMES PIC 9(3).
      ***************************************
      *
      * VERYANT
      * INCLUDE ALL COPYBOOSKS FOR COBOL
      * DATER RELATED ROUTINES
      *
      ***************************************
      *COPY "LIBGB/DATERJUL.CPY".
      *************************************************
      *
      *  VERYANT
      *  PARAMETERS FOR THE JUL ROUTINE
      *
      *************************************************
       01 P-JUL.
          05 P-DATE-IN     PIC 9(8).
          05 P-JULIAN-DATE PIC 9(7).

      *COPY "LIBGB/DATERCJUL.CPY".
      *************************************************
      *
      *  VERYANT
      *  PARAMETERS FOR THE CJUL ROUTINE
      *
      *************************************************
       01 P-CJUL.
          05 P-CJUL-JULIAN-DATE  PIC 9(7).
          05 P-CJUL-DATE-OUT     PIC 9(8).

      *----------------------------------------------------------------
      *  WORKERS FOR VERYANT COBOL VERSION OF DATER CALCULATION
      *----------------------------------------------------------------
       77 SV-DATER-ACTION-CODE PIC X.
       77 WS-MOD               PIC S9(2).
       77 WS-MOD-7             PIC S9(2).
       77 WS-MOD-2             PIC S9(2).
       77 WS-MOD-4             PIC S9(2).
       77 WS-MOD-12            PIC S9(2).
       77 WS-DIV-RESULT        PIC S9(4).
       77 WS-DIV-TMP           PIC S9(7).
      *77 WS-UP-WORK           PIC S9(3).
       77 WS-UP-WORK           PIC S9(4).
       77 WS-HOLD-DD           PIC S9(3).
       77 WS-HOLD-MOS          PIC S9(7).
       77 WS-JUL-WK            PIC 9(5).
       77 WS-JUL               PIC 9(9).
       77 WS-JUL-1             PIC 9(9).
       77 WS-JUL-2             PIC 9(9).
       77 WS-TMP-JUL-1         PIC 9(9).
       77 WS-TMP-JUL-2         PIC 9(9).
       77 WS-JUL-TMP           PIC 9(9).
       77 WS-HOLDJUL           PIC 9(9).
       77 WS-TIM-RESULTS       PIC X.
       77 SV-NUM-DATE          PIC 9(8).
       77 SV-SYS-DATE          PIC 9(8).
       77 WS-HOLDDATE          PIC 9(8).

       77 WS-XMONTHS           PIC S9(7).
       77 WS-XDAYS             PIC S9(9).
       77 WS-XREM              PIC S9(7).
       77 WS-XUNITPER          PIC S9(9).
       77 WS-XUNITPER-REM      PIC S9(7).
       77 WS-XUNITPER-FREQ     PIC S9(7).
       77 WS-XSIGN             PIC S9 VALUE 1.
       77 WS-XUNITPER-CODE     PIC X.

       77 WS-WORK              PIC S9(7).
       77 WS-TMP-WORK          PIC S9(7).

       01 DATER-HOLD-NDTE-DATE PIC  9(8)  VALUE 0.
       01 WS-NDTE-SIGN.
          05 WS-NDTE-MM-S      PIC S9(2).
          05 WS-NDTE-DD-S      PIC S9(2).
      *   05 WS-NDTE-YY-S      PIC S9(3).
          05 WS-NDTE-CCYY-S    PIC S9(5).

      * USED FOR C-DATE-TEST
       01 WS-NDTE              PIC 9(8) VALUE 0.
       01 FILLER REDEFINES WS-NDTE.
          03  WS-NDTE-CCYY     PIC 9(4).
          03  FILLER REDEFINES WS-NDTE-CCYY.
              05 WS-NDTE-CC        PIC 9(2).
              05 WS-NDTE-YY        PIC 9(2).
          03 WS-NDTE-MM        PIC 9(2).
          88 WS-NDTE-MM-VALID  VALUE 1 THRU 12.
          03 WS-NDTE-DD        PIC 9(2).
          88 WS-NDTE-DD-VALID  VALUE 1 THRU 31.


       01 WS-DATE1             PIC 9(8) VALUE 0.
       01 FILLER REDEFINES WS-DATE1.
          05 WS-DATE1-CCYY     PIC 9(4).
          05 WS-DATE1-CCYYX REDEFINES WS-DATE1-CCYY.
             07 WS-DATE1-CC       PIC 9(2).
             07 WS-DATE1-YY       PIC 9(2).
          05 WS-DATE1-MM       PIC 9(2).
          05 WS-DATE1-DD       PIC 9(2).

       01 WS-DATE2             PIC 9(8) VALUE 0.
       01 FILLER REDEFINES WS-DATE2.
          05 WS-DATE2-CCYY     PIC 9(4).
          05 WS-DATE2-CCYYX REDEFINES WS-DATE2-CCYY.
             07 WS-DATE2-CC       PIC 9(2).
             07 WS-DATE2-YY       PIC 9(2).
          05 WS-DATE2-MM       PIC 9(2).
          05 WS-DATE2-DD       PIC 9(2).

       01 WS-DATE1W            PIC 9(8) VALUE 0.
       01 FILLER REDEFINES WS-DATE1W.
          05 WS-DATE1W-CCYY    PIC 9(4).
          05 WS-DATE1W-CCYYX REDEFINES WS-DATE1W-CCYY.
             07 WS-DATE1W-CC   PIC 9(2).
             07 WS-DATE1W-YY   PIC 9(2).
          05 WS-DATE1W-MM      PIC 9(2).
          05 WS-DATE1W-DD      PIC 9(2).

       01 WS-DATE2W            PIC 9(8) VALUE 0.
       01 FILLER REDEFINES WS-DATE2W.
          05 WS-DATE2W-CCYY    PIC 9(4).
          05 WS-DATE2W-CCYYX REDEFINES WS-DATE2W-CCYY.
             07 WS-DATE2W-CC   PIC 9(2).
             07 WS-DATE2W-YY   PIC 9(2).
          05 WS-DATE2W-MM      PIC 9(2).
          05 WS-DATE2W-DD      PIC 9(2).

       01 WS-DATEX             PIC 9(8) VALUE 0.
       01 FILLER REDEFINES WS-DATEX.
          05 WS-DATEX-CCYY     PIC 9(4).
          05 WS-DATEX-CCYYX REDEFINES WS-DATEX-CCYY.
             07 WS-DATEX-CC       PIC 9(2).
             07 WS-DATEX-YY       PIC 9(2).
          05 WS-DATEX-MM       PIC 9(2).
          05 WS-DATEX-DD       PIC 9(2).

       77 NOT-YEAR             PIC 9(3).
       77 TA-YRTYPE            PIC 9(3).
      *----------------------------------------------------------------
      *  WORKERS FOR VERYANT COBOL VERSION OF JUL CALCULATION
      *----------------------------------------------------------------
       77 JWS-EXTRA-DAY     PIC 9.
       77 JWS-MOD-JUL       PIC S9(2).
       77 JWS-DIV-RESULT    PIC S9(4).
       77 JWS-JUL-IN-CCYY   PIC 9(4).
       77 J-WS-JULIAN-DATE  PIC 9(7).
       01 JJUL-IN.
          05 JJUL-IN-CCYY PIC 9(4).
          05 FILLER REDEFINES JJUL-IN-CCYY.
             07 JJUL-IN-CC  PIC 9(2).
             07 JJUL-IN-YY  PIC 9(2).
          05 JJUL-IN-MM  PIC 9(2).
          05 JJUL-IN-DD  PIC 9(2).
      *----------------------------------------------------------------
      *  WORKERS FOR VERYANT COBOL VERSION OF CJUL CALCULATION
      *----------------------------------------------------------------
       77 CJWS-MOD-JUL       PIC S9(4).
       77 CJWS-DIV-RESULT    PIC S9(4).
       77 CJWS-JUL-BY4       PIC 9(9).
       77 CJWS-WORK          PIC 9.
       77 CJWS-MM            PIC 9(2).
       01 WS-CJUL-OUT        PIC 9(12) VALUE 0.
       01 FILLER REDEFINES WS-CJUL-OUT.
          05 WS-CJUL-OUT-CCYY PIC 9(4).
          05 FILLER REDEFINES WS-CJUL-OUT-CCYY.
             07 WS-CJUL-OUT-CC  PIC 9(2).
             07 WS-CJUL-OUT-YY  PIC 9(2).
          05 WS-CJUL-OUT-MM  PIC 9(4).
          05 WS-CJUL-OUT-DD  PIC 9(4).

       01 CJUL-IN         PIC 9(9).
       01 CJUL-OUT        PIC 9(8) VALUE 0.
       01 FILLER REDEFINES CJUL-OUT.
          05 CJUL-OUT-CC  PIC 9(2).
          05 CJUL-OUT-YY  PIC 9(2).
          05 CJUL-OUT-MM  PIC 9(2).
          05 CJUL-OUT-DD  PIC 9(2).
      *----------------------------------------------------------------
      *  WORKERS FOR VERYANT COBOL VERSION OF NEWDATE CALCULATION
      *----------------------------------------------------------------
       01 ND-WS-NDTE          PIC 9(8) VALUE 0.
       01 FILLER REDEFINES ND-WS-NDTE.
          05 ND-WS-NDTE-CCYY  PIC 9(4).
      *   05 ND-WS-NDTE-YY    PIC 9(2).
          05 ND-WS-NDTE-MM    PIC 9(2).
          05 ND-WS-NDTE-DD    PIC 9(2).
       77 ND-WS-DIV-RESULT    PIC S9(4).
       77 ND-WS-MOD-4         PIC S9(2).

       01  ALP-DATE-EDIT           PIC 99/99/99.
       01  UNITPER-MASK-DATE      REDEFINES ALP-DATE-EDIT.
           03  FILLER              PIC X(5).
           03  DATER-UNITPER-IT    PIC X.
           03  FILLER              PIC XX.

       01  ELAPSED-MMREM-FG        PIC X.
       01  ELAPSED-COUNT           PIC  9     COMP.
       01  CALL-DATE               PIC 9(8) VALUE ZEROS.
       01  FILLER REDEFINES CALL-DATE.
           03  CALL-CC             PIC  99.
           03  CALL-YY             PIC  99.
           03  CALL-MM             PIC  99.
           03  CALL-DD             PIC  99.
       01  ALP-DATE.
           03  ALP-MO              PIC  99.
           03  FILLER              PIC X      VALUE "/".
           03  ALP-DA              PIC  99.
           03  FILLER              PIC X      VALUE "/".
           03  ALP-YR              PIC  99.
       01  DTE-HOLD                PIC  99999 COMP.

       01  HOLD-UNITPER-INFO       PIC X(4).

      * 20151110 ADDED FOR DATE CONVERSION SECTIONS.
       01  CONVERT-DATE            PIC 9(8).
       01  CONVERT-DATEX REDEFINES CONVERT-DATE.
           03  CONVERT-DATE-CC     PIC 9(2).
           03  CONVERT-DATE-YY     PIC 9(2).
           03  CONVERT-DATE-MM     PIC 9(2).
           03  CONVERT-DATE-DD     PIC 9(2).

      ************************************************************
      *01  JULIAN-BEG              PIC 9(8)   COMP  VALUE 19500101.
      *01  JULIAN-END              PIC 9(8)   COMP  VALUE 20491231.
      *01  JULIAN-CC               PIC 9(2)   COMP  VALUE 50.
       01  JULIAN-WORKER           PIC 9(4)   COMP  VALUE 0.
       01  JULIAN-WORKER2          PIC 9(4)   COMP  VALUE 0.
       01  JULIAN-WK               PIC 9            VALUE 0.

       01  LEAP-YEAR-CCYY          PIC 9(4).
       01  LEAP-YEAR-FG            PIC X(1) VALUE "F".
       88  LEAP-YEAR-TRUE          VALUE "T".
       88  LEAP-YEAR-FALSE         VALUE "F".

      *=================================================================
      *    YEAR 2000 FORMAT --- 'MMDDYYYY' OR 'YYYYMMDD'
      *=================================================================
       01  DATE-MMDDYY                           PIC 9(06) VALUE ZEROES.
       01  FILLER                      REDEFINES DATE-MMDDYY.
           03  DATE-MMDDYY-MM                    PIC 9(02).
           03  DATE-MMDDYY-DD                    PIC 9(02).
           03  DATE-MMDDYY-YY                    PIC 9(02).

       01  DATE-MMDDYYYY                         PIC 9(08) VALUE ZEROES.
       01  FILLER                      REDEFINES DATE-MMDDYYYY.
           03  DATE-MMDDYYYY-MM                  PIC 9(02).
           03  DATE-MMDDYYYY-DD                  PIC 9(02).
           03  DATE-MMDDYYYY-YYYY                PIC 9(04).
           03  DATE-MMDDYYYY-CCYY      REDEFINES DATE-MMDDYYYY-YYYY.
               05  DATE-MMDDYYYY-CC              PIC 9(02).
               05  DATE-MMDDYYYY-YY              PIC 9(02).

       01  DATE-DDMMYYYY                         PIC 9(08) VALUE ZEROES.
       01  FILLER                      REDEFINES DATE-DDMMYYYY.
           03  DATE-DDMMYYYY-DD                  PIC 9(02).
           03  DATE-DDMMYYYY-MM                  PIC 9(02).
           03  DATE-DDMMYYYY-YYYY                PIC 9(04).
           03  DATE-DDMMYYYY-CCYY      REDEFINES DATE-DDMMYYYY-YYYY.
               05  DATE-DDMMYYYY-CC              PIC 9(02).
               05  DATE-DDMMYYYY-YY              PIC 9(02).

       01  DATE-MMSDDSYY                         PIC X(08) VALUE SPACES.
       01  FILLER                      REDEFINES DATE-MMSDDSYY.
           03  DATE-MMSDDSYY-MM                  PIC 9(02).
           03  DATE-MMSDDSYY-S1                  PIC X(01).
           03  DATE-MMSDDSYY-DD                  PIC 9(02).
           03  DATE-MMSDDSYY-S2                  PIC X(01).
           03  DATE-MMSDDSYY-YY                  PIC 9(02).

       01  DATE-MMSDDSYYYY                       PIC X(10) VALUE SPACES.
       01  FILLER                      REDEFINES DATE-MMSDDSYYYY.
           03  DATE-MMSDDSYYYY-MM                PIC 9(02).
           03  DATE-MMSDDSYYYY-S1                PIC X(01).
           03  DATE-MMSDDSYYYY-DD                PIC 9(02).
           03  DATE-MMSDDSYYYY-S2                PIC X(01).
           03  DATE-MMSDDSYYYY-YYYY              PIC 9(04).
           03  DATE-MMSDDSYYYY-CCYY    REDEFINES DATE-MMSDDSYYYY-YYYY.
               05  DATE-MMSDDSYYYY-CC            PIC 9(02).
               05  DATE-MMSDDSYYYY-YY            PIC 9(02).

       01  DATE-MMSYY                            PIC X(05) VALUE SPACES.
       01  FILLER                      REDEFINES DATE-MMSYY.
           03  DATE-MMSYY-MM                     PIC 9(02).
           03  DATE-MMSYY-S1                     PIC X(01).
           03  DATE-MMSYY-YY                     PIC 9(02).

       01  DATE-MMSYYYY                          PIC X(07) VALUE SPACES.
       01  FILLER                      REDEFINES DATE-MMSYYYY.
           03  DATE-MMSYYYY-MM                   PIC 9(02).
           03  DATE-MMSYYYY-S1                   PIC X(01).
           03  DATE-MMSYYYY-YYYY                 PIC 9(04).
           03  DATE-MMSYYYY-CCYY        REDEFINES DATE-MMSYYYY-YYYY.
               05  DATE-MMSYYYY-CC               PIC 9(02).
               05  DATE-MMSYYYY-YY               PIC 9(02).

       01  DATE-MMYY                             PIC 9(04) VALUE ZEROES.
       01  FILLER                      REDEFINES DATE-MMYY.
           03  DATE-MMYY-MM                      PIC 9(02).
           03  DATE-MMYY-YY                      PIC 9(02).

       01  DATE-MMYYY                            PIC 9(05) VALUE ZEROES.
       01  FILLER                      REDEFINES DATE-MMYYY.
           03  DATE-MMYYY-MM                     PIC 9(02).
           03  DATE-MMYYY-YYY                    PIC 9(03).

       01  DATE-MMYYYY                           PIC 9(06) VALUE ZEROES.
       01  FILLER                      REDEFINES DATE-MMYYYY.
           03  DATE-MMYYYY-MM                    PIC 9(02).
           03  DATE-MMYYYY-YYYY                  PIC 9(04).
           03  DATE-MMYYYY-CCYY        REDEFINES DATE-MMYYYY-YYYY.
               05  DATE-MMYYYY-CC                PIC 9(02).
               05  DATE-MMYYYY-YY                PIC 9(02).

       01  DATE-YYMM                             PIC 9(04) VALUE ZEROES.
       01  FILLER                      REDEFINES DATE-YYMM.
           03  DATE-YYMM-YY                      PIC 9(02).
           03  DATE-YYMM-MM                      PIC 9(02).

       01  DATE-YYMMDD                           PIC 9(06) VALUE ZEROES.
       01  DATE-YYMMDD-ALP                       REDEFINES DATE-YYMMDD.
           03  DATE-YYMMDD-YY                    PIC 9(02).
           03  DATE-YYMMDD-MM                    PIC 9(02).
           03  DATE-YYMMDD-DD                    PIC 9(02).

       01  DATE-DDMMYY                           PIC 9(06) VALUE ZEROES.
       01  FILLER                      REDEFINES DATE-DDMMYY.
           03  DATE-DDMMYY-DD                    PIC 9(02).
           03  DATE-DDMMYY-MM                    PIC 9(02).
           03  DATE-DDMMYY-YY                    PIC 9(02).

       01  DATE-YYYMM                            PIC 9(05) VALUE ZEROES.
       01  FILLER                      REDEFINES DATE-YYYMM.
           03  DATE-YYYMM-YYY                    PIC 9(03).
           03  DATE-YYYMM-MM                     PIC 9(02).

       01  DATE-YYYMMDD                          PIC 9(07) VALUE ZEROES.
       01  FILLER                      REDEFINES DATE-YYYMMDD.
           03  DATE-YYYMMDD-YYY                  PIC 9(03).
           03  DATE-YYYMMDD-MM                   PIC 9(02).
           03  DATE-YYYMMDD-DD                   PIC 9(02).

       01  DATE-YYYYMM                           PIC 9(06) VALUE ZEROES.
       01  FILLER                      REDEFINES DATE-YYYYMM.
           03  DATE-YYYYMM-YYYY                  PIC 9(04).
           03  DATE-YYYYMM-CCYY        REDEFINES DATE-YYYYMM-YYYY.
               05  DATE-YYYYMM-CC                PIC 9(02).
               05  DATE-YYYYMM-YY                PIC 9(02).
           03  DATE-YYYYMM-MM                    PIC 9(02).

       01  DATE-YYYYMMDD                         PIC 9(08) VALUE ZEROES.
       01  FILLER                      REDEFINES DATE-YYYYMMDD.
           03  DATE-YYYYMMDD-YYYY                PIC 9(04).
           03  DATE-YYYYMMDD-CCYY      REDEFINES DATE-YYYYMMDD-YYYY.
               05  DATE-YYYYMMDD-CC              PIC 9(02).
               05  DATE-YYYYMMDD-YY              PIC 9(02).
           03  DATE-YYYYMMDD-MM                  PIC 9(02).
           03  DATE-YYYYMMDD-DD                  PIC 9(02).


      *-----------------------------------------------------{ DATE-ONE }
       01  DATE-ONE                                           PIC 9(08).

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       01  DATE-ONE-MMDDYY-RDF                       REDEFINES DATE-ONE.
           03  DATE-ONE-MMDDYY                                PIC 9(06).
           03  FILLER                                REDEFINES
                                                        DATE-ONE-MMDDYY.
               05  DATE-ONE-MMDDYY-MM                         PIC 9(02).
               05  DATE-ONE-MMDDYY-DD                         PIC 9(02).
               05  DATE-ONE-MMDDYY-YY                         PIC 9(02).

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       01  DATE-ONE-MMDDYYYY-RDF                     REDEFINES DATE-ONE.
           03  DATE-ONE-MMDDYYYY                              PIC 9(08).
           03  FILLER                                REDEFINES
                                                      DATE-ONE-MMDDYYYY.
               05  DATE-ONE-MMDDYYYY-MM                       PIC 9(02).
               05  DATE-ONE-MMDDYYYY-DD                       PIC 9(02).
               05  DATE-ONE-MMDDYYYY-YYYY                     PIC 9(04).
               05  DATE-ONE-MMDDYYYY-CCYY            REDEFINES
                                                 DATE-ONE-MMDDYYYY-YYYY.
                   07  DATE-ONE-MMDDYYYY-CC                   PIC 9(02).
                   07  DATE-ONE-MMDDYYYY-YY                   PIC 9(02).

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       01  DATE-ONE-MMYY-RDF                         REDEFINES DATE-ONE.
           03  DATE-ONE-MMYY                                  PIC 9(04).
           03  FILLER                                REDEFINES
                                                          DATE-ONE-MMYY.
               05  DATE-ONE-MMYY-MM                           PIC 9(02).
               05  DATE-ONE-MMYY-YY                           PIC 9(02).

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       01  DATE-ONE-MMYYY-RDF                        REDEFINES DATE-ONE.
           03  DATE-ONE-MMYYY                                 PIC 9(05).
           03  FILLER                                REDEFINES
                                                         DATE-ONE-MMYYY.
               05  DATE-ONE-MMYYY-MM                          PIC 9(02).
               05  DATE-ONE-MMYYY-YYY                         PIC 9(03).

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       01  DATE-ONE-MMYYYY-RDF                       REDEFINES DATE-ONE.
           03  DATE-ONE-MMYYYY                                PIC 9(06).
           03  FILLER                                REDEFINES
                                                        DATE-ONE-MMYYYY.
               05  DATE-ONE-MMYYYY-MM                         PIC 9(02).
               05  DATE-ONE-MMYYYY-YYYY                       PIC 9(04).
               05  DATE-ONE-MMYYYY-CCYY              REDEFINES
                                                   DATE-ONE-MMYYYY-YYYY.
                   07  DATE-ONE-MMYYYY-CC                     PIC 9(02).
                   07  DATE-ONE-MMYYYY-YY                     PIC 9(02).

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       01  DATE-ONE-YYMM-RDF                         REDEFINES DATE-ONE.
           03  DATE-ONE-YYMM                                  PIC 9(04).
           03  FILLER                                REDEFINES
                                                          DATE-ONE-YYMM.
               05  DATE-ONE-YYMM-YY                           PIC 9(02).
               05  DATE-ONE-YYMM-MM                           PIC 9(02).

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       01  DATE-ONE-YYMMDD-RDF                       REDEFINES DATE-ONE.
           03  DATE-ONE-YYMMDD                                PIC 9(06).
           03  FILLER                                REDEFINES
                                                        DATE-ONE-YYMMDD.
               05  DATE-ONE-YYMMDD-YY                         PIC 9(02).
               05  DATE-ONE-YYMMDD-MM                         PIC 9(02).
               05  DATE-ONE-YYMMDD-DD                         PIC 9(02).

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       01  DATE-ONE-YYYMM-RDF                        REDEFINES DATE-ONE.
           03  DATE-ONE-YYYMM                                 PIC 9(05).
           03  FILLER                                REDEFINES
                                                         DATE-ONE-YYYMM.
               05  DATE-ONE-YYYMM-YYY                         PIC 9(03).
               05  DATE-ONE-YYYMM-MM                          PIC 9(02).

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       01  DATE-ONE-YYYMMDD-RDF                      REDEFINES DATE-ONE.
           03  DATE-ONE-YYYMMDD                               PIC 9(07).
           03  FILLER                                REDEFINES
                                                       DATE-ONE-YYYMMDD.
               05  DATE-ONE-YYYMMDD-YYY                       PIC 9(03).
               05  DATE-ONE-YYYMMDD-MM                        PIC 9(02).
               05  DATE-ONE-YYYMMDD-DD                        PIC 9(02).

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       01  DATE-ONE-YYYYMM-RDF                       REDEFINES DATE-ONE.
           03  DATE-ONE-YYYYMM                                PIC 9(06).
           03  FILLER                                REDEFINES
                                                        DATE-ONE-YYYYMM.
               05  DATE-ONE-YYYYMM-YYYY                       PIC 9(04).
               05  DATE-ONE-YYYYMM-CCYY              REDEFINES
                                                   DATE-ONE-YYYYMM-YYYY.
                   07  DATE-ONE-YYYYMM-CC                     PIC 9(02).
                   07  DATE-ONE-YYYYMM-YY                     PIC 9(02).
               05  DATE-ONE-YYYYMM-MM                         PIC 9(02).

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       01  DATE-ONE-YYYYMMDD-RDF                     REDEFINES DATE-ONE.
           03  DATE-ONE-YYYYMMDD                              PIC 9(08).
           03  FILLER                                REDEFINES
                                                      DATE-ONE-YYYYMMDD.
               05  DATE-ONE-YYYYMMDD-YYYY                     PIC 9(04).
               05  DATE-ONE-YYYYMMDD-CCYY            REDEFINES
                                                 DATE-ONE-YYYYMMDD-YYYY.
                   07  DATE-ONE-YYYYMMDD-CC                   PIC 9(02).
                   07  DATE-ONE-YYYYMMDD-YY                   PIC 9(02).
               05  DATE-ONE-YYYYMMDD-MM                       PIC 9(02).
               05  DATE-ONE-YYYYMMDD-DD                       PIC 9(02).

      *-----------------------------------------------------{ DATE-TWO }
       01  DATE-TWO                                           PIC 9(08).

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       01  DATE-TWO-MMDDYY-RDF                       REDEFINES DATE-TWO.
           03  DATE-TWO-MMDDYY                                PIC 9(06).
           03  FILLER                                REDEFINES
                                                        DATE-TWO-MMDDYY.
               05  DATE-TWO-MMDDYY-MM                         PIC 9(02).
               05  DATE-TWO-MMDDYY-DD                         PIC 9(02).
               05  DATE-TWO-MMDDYY-YY                         PIC 9(02).

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       01  DATE-TWO-MMDDYYYY-RDF                     REDEFINES DATE-TWO.
           03  DATE-TWO-MMDDYYYY                              PIC 9(08).
           03  FILLER                                REDEFINES
                                                      DATE-TWO-MMDDYYYY.
               05  DATE-TWO-MMDDYYYY-MM                       PIC 9(02).
               05  DATE-TWO-MMDDYYYY-DD                       PIC 9(02).
               05  DATE-TWO-MMDDYYYY-YYYY                     PIC 9(04).
               05  DATE-TWO-MMDDYYYY-CCYY            REDEFINES
                                                 DATE-TWO-MMDDYYYY-YYYY.
                   07  DATE-TWO-MMDDYYYY-CC                   PIC 9(02).
                   07  DATE-TWO-MMDDYYYY-YY                   PIC 9(02).

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       01  DATE-TWO-MMYY-RDF                         REDEFINES DATE-TWO.
           03  DATE-TWO-MMYY                                  PIC 9(04).
           03  FILLER                                REDEFINES
                                                          DATE-TWO-MMYY.
               05  DATE-TWO-MMYY-MM                           PIC 9(02).
               05  DATE-TWO-MMYY-YY                           PIC 9(02).

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       01  DATE-TWO-MMYYY-RDF                        REDEFINES DATE-TWO.
           03  DATE-TWO-MMYYY                                 PIC 9(05).
           03  FILLER                                REDEFINES
                                                         DATE-TWO-MMYYY.
               05  DATE-TWO-MMYYY-MM                          PIC 9(02).
               05  DATE-TWO-MMYYY-YYY                         PIC 9(03).

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       01  DATE-TWO-MMYYYY-RDF                       REDEFINES DATE-TWO.
           03  DATE-TWO-MMYYYY                                PIC 9(06).
           03  FILLER                                REDEFINES
                                                        DATE-TWO-MMYYYY.
               05  DATE-TWO-MMYYYY-MM                         PIC 9(02).
               05  DATE-TWO-MMYYYY-YYYY                       PIC 9(04).
               05  DATE-TWO-MMYYYY-CCYY              REDEFINES
                                                   DATE-TWO-MMYYYY-YYYY.
                   07  DATE-TWO-MMYYYY-CC                     PIC 9(02).
                   07  DATE-TWO-MMYYYY-YY                     PIC 9(02).

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       01  DATE-TWO-YYMM-RDF                         REDEFINES DATE-TWO.
           03  DATE-TWO-YYMM                                  PIC 9(04).
           03  FILLER                                REDEFINES
                                                          DATE-TWO-YYMM.
               05  DATE-TWO-YYMM-YY                           PIC 9(02).
               05  DATE-TWO-YYMM-MM                           PIC 9(02).

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       01  DATE-TWO-YYMMDD-RDF                       REDEFINES DATE-TWO.
           03  DATE-TWO-YYMMDD                                PIC 9(06).
           03  FILLER                                REDEFINES
                                                        DATE-TWO-YYMMDD.
               05  DATE-TWO-YYMMDD-YY                         PIC 9(02).
               05  DATE-TWO-YYMMDD-MM                         PIC 9(02).
               05  DATE-TWO-YYMMDD-DD                         PIC 9(02).

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       01  DATE-TWO-YYYMM-RDF                        REDEFINES DATE-TWO.
           03  DATE-TWO-YYYMM                                 PIC 9(05).
           03  FILLER                                REDEFINES
                                                         DATE-TWO-YYYMM.
               05  DATE-TWO-YYYMM-YYY                         PIC 9(03).
               05  DATE-TWO-YYYMM-MM                          PIC 9(02).

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       01  DATE-TWO-YYYMMDD-RDF                      REDEFINES DATE-TWO.
           03  DATE-TWO-YYYMMDD                               PIC 9(07).
           03  FILLER                                REDEFINES
                                                       DATE-TWO-YYYMMDD.
               05  DATE-TWO-YYYMMDD-YYY                       PIC 9(03).
               05  DATE-TWO-YYYMMDD-MM                        PIC 9(02).
               05  DATE-TWO-YYYMMDD-DD                        PIC 9(02).

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       01  DATE-TWO-YYYYMM-RDF                       REDEFINES DATE-TWO.
           03  DATE-TWO-YYYYMM                                PIC 9(06).
           03  FILLER                                REDEFINES
                                                        DATE-TWO-YYYYMM.
               05  DATE-TWO-YYYYMM-YYYY                       PIC 9(04).
               05  DATE-TWO-YYYYMM-CCYY              REDEFINES
                                                   DATE-TWO-YYYYMM-YYYY.
                   07  DATE-TWO-YYYYMM-CC                     PIC 9(02).
                   07  DATE-TWO-YYYYMM-YY                     PIC 9(02).
               05  DATE-TWO-YYYYMM-MM                         PIC 9(02).

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       01  DATE-TWO-YYYYMMDD-RDF                     REDEFINES DATE-TWO.
           03  DATE-TWO-YYYYMMDD                              PIC 9(08).
           03  FILLER                                REDEFINES
                                                      DATE-TWO-YYYYMMDD.
               05  DATE-TWO-YYYYMMDD-YYYY                     PIC 9(04).
               05  DATE-TWO-YYYYMMDD-CCYY            REDEFINES
                                                 DATE-TWO-YYYYMMDD-YYYY.
                   07  DATE-TWO-YYYYMMDD-CC                   PIC 9(02).
                   07  DATE-TWO-YYYYMMDD-YY                   PIC 9(02).
               05  DATE-TWO-YYYYMMDD-MM                       PIC 9(02).
               05  DATE-TWO-YYYYMMDD-DD                       PIC 9(02).

