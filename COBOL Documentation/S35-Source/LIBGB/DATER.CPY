      ****************************************************************
      * COPYMEMBER: LIBGB/DATER
      *
      * DESC: WILL ACCEPT DATES IN THE 8 DIGIT FORMAT IN WITH
      *       EITHER  "00MMDDYY" FORMAT OR "CCYYMMDD" FORMAT
      *
      *  INPUT: SYS-DATE, NUM-DATE AND NDTE-DATE ARE 9(8).
      *
      *  OUTPUT: OUTPUT DATE FORMAT WILL MATCH THAT OF THE INPUT FORMAT.
      *          IF INPUT = "00MMDDYY" THEN OUTPUT WILL BE "00MMDDYY".
      *          IF INPUT = "CCYYMMDD" THEN OUTPUT WILL BE "CCYYMMDD".
      *
      *=================================================================
      * NOTE: THE MOVE OF NUMERIC FIELDS WORKS DIFFERENTLY BETWEEN
      *       COMP AND NON-COMP FIELDS.
      *       IF MOVING A NON-COMP FIELD (SOURCE) TO ANOTHER FIELD
      *       WILL CAUSE THE FOLLOWING:
      *          MOVE SOURCE TO NON-COMP. [RESULT IS SPACES]
      *          MOVE SOURCE TO COMP-3.   [RESULT IS ZEROES]
      *          MOVE SOURCE TO COMP-6.   [RESULT IS ZEROES]
      *       WITHIN A 'MOVE' STATEMENT, THE CONVERSION ROUTINE FOR
      *       COMPRESSION OF THE 'MOVE' DOES A VERIFICATION WHICH MOVES
      *       ZEROES TO NON-NUMERIC FIELDS.  THIS WAS FOUND IN DEBUG
      *       WITHIN CONVERT-MMDDYY-TO-YYYYMMDD SECTION WHICH
      *       DATE-MMDDYY WAS SPACES GIVING A RESULT OF "2000    ".
      *       BETH & JAY DECIDED NOT TO ALTER THE ROUTINE BUT TO ADD A
      *       NUMERIC CHECK IN THE PROGRAM BEFORE CALLING TO
      *       CONVERT-MMDDYY-TO-YYYYMMDD.
      *       SEE BT-PAYDATE IN SP/BPREF AS EXAMPLE.      JAY 2017-1113
      *
      **************************************************************************
      *
      * MJD 130130 REPLACED "C" CALLS WITH VERYAND COBOL VERSIONS OF
      *            PARADATA DATE ROUTINES.   THIS IS AN INTERNAL
      *            CHANGE, THE WAY IN WHICH WE CALL AND USE THE
      *            DATE ROUTINES REMAINS UNCHANGED.
      *            ADDED A FEW INITIALZE  XXXX TO MAKE OUTPUT
      *            MATCH A15.   IN EACH CASE THE FIELD CLEARED WAS NOT
      *            PART OF THE OUTPUT SET FOR THAT FUNCTION. VERYANT
      *            USED THEM INTERNALLY FOR CALCULATIONS.
      * MJD 130212 REMOVED THE INITIALIZE STATEMENTS I ADDED.
      *            WHILE THESE WERE NOT PART OF THE OUTPUT I WAS
      *            AFFECTING OUTSIDE CALCULATIONS THAT USED THEM.
      * MJD 130313 ADDED LOGIC FROM VERYANT TO CORRECT AN ISSUE WITH
      *            ELASED TIME AND LEAP YEARS.
      * BAH 130507 DO NOT CLEAR OR INITIALIZE OR ZERO JULIAN-DATE
      *             REBATE COPY MEMBER EXPECTS BACK SPACES OR COMPILED DEFAULT
      * MJD 130509 ADDED WS-JUL-WK TO REPLACE JULIAN-DATE FOR INTERNAL
      *            CALCULATIONS. EDWIN (VERYANT) WAS USING JULIAN-DATE FROM
      *            BUFFER AND WE DO NOT WANT TO CHANGE THE VALUE UNLESS
      *            IT IS ACTUALLY PART OF THE INPUT/OUTPUT OF THE ROUTINE
      *            CALLED.
      * MJD 130514 IN C-DATE-COMPARE DO NOT CONVERT TO JULIAN
      *            IF NUM-DATE AND/OR SYS-DATE ARE ZERO.
      *            ZERO DATES WERE CAUSING AN INDEX OUT OF BOUNDS ERROR.
      * MJD 131004 ADDED TEST OF 4 DIGIT YEAR WHEN PERFORMING ELAPSED
      *            TIME ROUTINES.  PROBLEMS AROSE WHEN SPANNING CENTURY DATES
      *            NEW CODE USES WS-DATE1-YYYY AND WS-DATE2-YYYY. ADDED NEW
      *            SECTION TO SET THESE DATES: C-SET-WS-DATE-YYYY
      *            EXPANDED WS-NDTE-YY-S TO 4 DIGITS
      *            MADE CORRECTIONS FOR SWING DATE IN INCREMENT-MONTHS
      *  MJD 140124 ADDED LOGIC TO ALTER ELAPSED RESULTS WHEN ZERO DATES
      *             ARE USED TO MATCH RESULTS GENERATEDIN A15 (C-DATE-COMPARE).
      *         IN A15:
      *            IF SYS-DATE = 0
      *              IF NUM-YR < 50 THEN ELAPSED-RESULTS = "L"  FOR ALL MM/DD
      *              IF NUM-YR >= 50 THAN ELAPSED RESULTS = "G" FOR ALL MM/DD
      *              IF NUM-DATE = 123199 THEN ELAPSED RESULTS = "E"
      *              IF NUM-DATE = 0 THEN ELAPSED RESULTS = "E"
      *
      *            IF NUM-DATE = 0
      *              IF SYS-YY < 50 THEN ELAPSED-RESULTS = "G" FOR ALL MM/DD
      *              IF SYS-YY >= 50 THAN ELAPSED RESULTS = "L" FOR ALL MM/DD
      *              IF SYS-DATE = 123199 THEN ELAPSED RESULTS = "E"
      *              IF SYS-DATE = 0 THEN ELAPSED RESULTS = "E"
      *  MJD 140507 ADDED TEST OF VALID MONTH AND DAY TO BEGINNING OF
      *              C-DATE-TEST (TSTDAT)
      *  MJD 141014 REMOVED UNUSED PARA NAMES IN SECTIONS WHERE
      *              "EXIT SECTION" WAS BEING USED.  ATTEMPTING TO
      *              DEBUG A STACK OVERFLOW ERROE.
      *  MJD 141015 MADE ALL PARAGRAPHS SECTIONS.  REMOVED "EXIT SECTION"
      *             AND REPLACED WITH JUST EXIT WHERE NEEDED.
      *  MJD 1500123 CHANGE C-INCR-DAYS TO SV AND RESTORE NUM/SYS-DATE
      *              UPON ENTRY/EXIT
      *  MJD 160614 REMOVED DATER-STAT, NO LONGER NEEDED.
      *  BLM 170201 ADDED CONVERT-YYYYMMDD-TO-MMDDYYYY SECTION:
      *             WAS ADDED TO DATER BUT NOT DATER8
      *  MJD 170202 FIXED BUG IN LEAP-YEAR CALCULATION, REPLACED
      *             ND-WS-NDTE-CCYY WITH LEAP-YEAR-CCYY. FIELD WAS BEING
      *             SET BUT CODE WITHIN THE SECTION WAS MISSED.
      *             PER BARBS REQUEST CHANGED CMPDAT TO PERFORM A
      *             STRAIGHT <,>,= COMPARISON BETWEEN SYS-DATE AND NUM-DATE.
      *  MJD 170207 REMOVED C-SET-WS-DATE-YYYY AND REFERENCES TO WS-DATE1-YYYY
      *             AND WS-DATE2-YYYY AS THEY ARE NO LONGER USED AFTER HAVE
      *             BEEN REPLACED WITH WS-DATE1-CCYY AND WS-DATE2-CCYY.
      *  BLM 170213 ADD CONVERT-ALPDATE-TO-YYYYMMDD
      *  BLM 170309 BROUGHT OVER FROM A39, CAME FROM DATER8 WITH A
      *             FEW TWEAKS, PD0006
      *   CS 170315 ADD BACK CONVERT-MMDDYY-TO-YYYYMM 
      *  BLM 180312 FIXED CONVERT-ALPDATE-TO-YYMMDD, WAS CONVERTING TO
      *             DATE-MMDDYY INSTEAD OF DATE-YYMMDD 
      *
      *  JKC 200612 COMMENTED OUT THE USAGE OF YYYMMDD SINCE NO LONGER
      *             USED; IT WAS A15 FORMAT.
      *
      * JKC 20240113 COMMENT OUT JULIAN-BEG, JULIAN-END, JULIAN-CC;
      *              THEY ARE NOW EXTERNAL FIELDS POPULATED IN
      *              GB/SETENV SO TO WE CAN MORE EASILY CHANGE
      *              'SLIDING CENTURY WINDOW' IN THE FUTURE.
      * BAH 2024.0313 CHANGED 010101 TO 19000101 
      *       MOVE 19000101 TO WS-DATE1 WS-DATE2 WS-DATE1W WS-DATE2W
      * BAH 2024.0808 S35Q-105 ADDED BRIAN'S FIX FOR YEAR TYPE 365/367
      *               INVOLVING LEAP YEARS. WAS NOT MATCHING A15 C-CODE.
      *               ELAPSED DAYS WERE NOT MATCHING CAUSING REBATES TO
      *               BE OFF AND NOT IN FAVOR OF THE CUSTOMER.
      *
      ****************************************************************
      *
      * NOTE:
      * COMMENTED OUT FOR TESTING IN UT/TSTDTR8.CBL
      * PROGRAM USED IT'S OUN DATER-ROUTINE SO IT CAN PERFORM
      * VARIABLE CAPTURES BEFORE AND AFTER FOR WRITING TO DETAIL LINE.
       
      *********************************************************
        DATER-ROUTINE  SECTION.
      *********************************************************
            PERFORM C-DATER-MAIN.

      *********************************************************
       C-DATER-MAIN SECTION.
      *********************************************************
           PERFORM CONVERT-SYS-NUM-NDTE-DATES-IN.
           MOVE DATER-ACTION-CODE TO SV-DATER-ACTION-CODE.

           EVALUATE SV-DATER-ACTION-CODE
           WHEN "E" PERFORM C-ELAPSED-TIME
           WHEN "N"
      *             OK CCYY
                    PERFORM C-CALL-NEWDATE
           WHEN "I"
                    PERFORM C-INCREMENT-PERIODS
           WHEN "C"
      *             OK CCYY
                    MOVE JULIAN-DATE     TO P-CJUL-JULIAN-DATE
                    PERFORM C-CALL-CJUL
                    MOVE P-CJUL-DATE-OUT TO NUM-DATE
           WHEN "J"
      *             OK CCYY
                    MOVE NUM-DATE      TO P-DATE-IN
                    PERFORM C-CALL-JUL
                    MOVE P-JULIAN-DATE TO JULIAN-DATE
           WHEN "W"
      *             OK CCYY
                    PERFORM C-WEEKDAY
           WHEN "D"
      *             OK CCYY
                    PERFORM C-DATE-COMPARE
           WHEN "T"
                    PERFORM C-DATE-TEST
           END-EVALUATE
           MOVE SV-DATER-ACTION-CODE TO DATER-ACTION-CODE
           PERFORM CONVERT-SYS-NUM-NDTE-DATES-OUT.

      *****************************************************************
      *          INCREMENT A DATE BY A NUMBER OF UNIT PERIODS
      *
      *   DESC:  INCREMENT NDTE-DATE BY NDTE-HOLD UNIT PERIODS;
      *          CORRECT DAY AND MONTH IF REQUIRED.
      *   IN  :  DATER-UNITPER-CD   = M, D, Y, S, W, B
      *          DATER-UNITPER-FREQ = FREQUENCY OF UNIT PERIODS
      *          NDTE-DATE          = YYYYMMDD TO INCREMENT
      *          NDTE-HOLD          = INCREMENT IN UNIT PERIODS (+ OR -)
      *   OUT :  NDTE-DATE = INCREMENTED DATE
      * REV:
      * BLM 100208 ADD CONVERT-YYMMDD-TO-YYYYMMDD
      *****************************************************************
       INCREMENT-UNITPER SECTION.
           MOVE "I" TO DATER-ACTION-CODE.
           PERFORM DATER-ROUTINE.

      *********************************************************
       INCREMENT-COMMON SECTION.
      *********************************************************
           MOVE 1 TO DATER-UNITPER-FREQ.
           PERFORM INCREMENT-UNITPER.
           MOVE HOLD-UNITPER-INFO TO DATER-UNITPER-INFO.

      ****************************************************
      *          INCREMENT A DATE BY A NUMBER OF YEARS
      *
      *   DESC:  INCREMENT NDTE-DATE BY NDTE-HOLD YEARS;
      *          CORRECT DAY AND MONTH IF REQUIRED.
      *   IN  :  NDTE-DATE = YYYYMMDD TO INCREMENT
      *          NDTE-HOLD = INCREMENT IN UNIT YEARS (+ OR -)
      *   OUT :  NDTE-DATE = INCREMENTED DATE
      * REV:
      ****************************************************
       INCREMENT-YEARS SECTION.
           MOVE DATER-UNITPER-INFO TO HOLD-UNITPER-INFO.
           MOVE "Y" TO DATER-UNITPER-CD.
           PERFORM INCREMENT-COMMON.

      ****************************************************
      *          INCREMENT A DATE BY A NUMBER OF MONTHS
      *
      *   DESC:  INCREMENT NDTE-DATE BY NDTE-HOLD MONTHS;
      *          CORRECT DAY AND MONTH IF REQUIRED.
      *   IN  :  NDTE-DATE = YYYYMMDD TO INCREMENT
      *          NDTE-HOLD = INCREMENT IN MONTHS (+ OR -)
      *   OUT :  NDTE-DATE = INCREMENTED DATE
      * REV:
      ****************************************************
       INCREMENT-MONTHS SECTION.
           MOVE DATER-UNITPER-INFO TO HOLD-UNITPER-INFO.
           MOVE "M" TO DATER-UNITPER-CD.
           PERFORM INCREMENT-COMMON.

      ****************************************************
      *          INCREMENT A DATE BY A NUMBER OF WEEKS
      *
      *   DESC:  INCREMENT NDTE-DATE BY NDTE-HOLD WEEKS;
      *          CORRECT DAY AND MONTH IF REQUIRED.
      *   IN  :  NDTE-DATE = YYYYMMDD TO INCREMENT
      *          NDTE-HOLD = INCREMENT IN WEEKS (+ OR -)
      *   OUT :  NDTE-DATE = INCREMENTED DATE
      * REV:
      ****************************************************
       INCREMENT-WEEKS SECTION.
           MOVE DATER-UNITPER-INFO TO HOLD-UNITPER-INFO.
           MOVE "W" TO DATER-UNITPER-CD.
           PERFORM INCREMENT-COMMON.

      ****************************************************
      *          INCREMENT A DATE BY A NUMBER OF DAYS
      *
      *   DESC:  INCREMENT NDTE-DATE BY NDTE-HOLD DAYS;
      *          CORRECT DAY AND MONTH IF REQUIRED.
      *   IN  :  NDTE-DATE = YYYYMMDD TO INCREMENT
      *          NDTE-HOLD = INCREMENT IN DAYS (+ OR -)
      *   OUT :  NDTE-DATE = INCREMENTED DATE
      * REV:
      ****************************************************
       INCREMENT-DAYS SECTION.
           MOVE DATER-UNITPER-INFO TO HOLD-UNITPER-INFO.
           MOVE "D" TO DATER-UNITPER-CD.
           PERFORM INCREMENT-COMMON.

      *******************************************************
      *          INCREMENT A DATE BY A NUMBER OF SEMIMONTHS
      *
      *   DESC:  INCREMENT NDTE-DATE BY NDTE-HOLD SEMIMONTHS;
      *          CORRECT DAY AND MONTH IF REQUIRED.
      *   IN  :  NDTE-DATE = YYYYMMDD TO INCREMENT
      *          NDTE-HOLD = INCREMENT IN SEMIMONTHS (+ OR -)
      *   OUT :  NDTE-DATE = INCREMENTED DATE
      * REV:
      ********************************************************
       INCREMENT-SEMIMONTHS SECTION.
           MOVE DATER-UNITPER-INFO TO HOLD-UNITPER-INFO.
           MOVE "S" TO DATER-UNITPER-CD.
           PERFORM INCREMENT-COMMON.

      ************************************************
      *         NEW DATE CALCULATION
      *
      *   DESC: ADJUST NDTE-DD WHEN INCONSISTENT
      *         WITH NDTE-MM TO LAST DAY IN NDTE-MM.
      *   IN  : NDTE-DATE-WORK
      *   OUT : NDTE-DATE
      * REV:
      ************************************************
       NEW-DATE-CALCULATION SECTION.
           MOVE "N" TO DATER-ACTION-CODE.
           PERFORM DATER-ROUTINE.

      ***************************************************
      *         CALENDER TO JULIAN DATE CONVERSION
      *
      *   DESC: COMPUTE JULIAN DATE;
      *         IF NUM-YR < EXT-JULIAN-CC
      *            DATE IS IN YEAR 2000
      *   IN  : NUM-DATE (YYYYMMDD)
      *   OUT : JULIAN-DATE IS THE NO. OF DAYS
      *         SINCE 12/31/1899; IF ERROR = 0
      * REV:
      ***************************************************
       JUL SECTION.
           MOVE "J" TO DATER-ACTION-CODE.
           PERFORM DATER-ROUTINE.


      ********************************************************
      *         JULIAN TO CALENDER DATE CONVERSION
      *
      *   DESC: CONVERT JULIAN DATE TO NORMALIZED FORMAT
      *         ADJUST FOR ERRONEOUS JULIAN-DD
      *         MAY BE USED WITH JUL TO INCREMENT DAYS
      *   IN  : JULIAN-DATE IS THE NO. OF DAYS
      *         SINCE 12/31/1899; JULIAN-DD MAY NOT = 0
      *   OUT : NUM-DATE (YYYYMMDD); IF ERROR = 0
      * REV:
      ********************************************************
       CJUL SECTION.
           MOVE "C" TO DATER-ACTION-CODE.
           PERFORM DATER-ROUTINE.

      ********************************************************
      *         DETERMINE DAY OF WEEK
      *
      *   DESC: THIS ROUTINE DETERMINES THE DAY OF THE WEEK.
      *
      *   IN  : NUM-DATE (YYYYMMDD)
      *   OUT : DAY--OF-WEEK
      *              DAY-IS-SUN
      *              DAY-IS-MON
      *                  ..
      *                  ..
      *              DAY-IS-SAT
      * REV:
      ********************************************************
       WEEKDAY SECTION.
           MOVE "W" TO DATER-ACTION-CODE.
           PERFORM DATER-ROUTINE.

      ********************************************************
      *          COMPARE DATE ROUTINE
      *
      *   DESC : DETERMINE IF SYS-DATE IS < > OR = TO NUM-DATE
      *     IN : NUM-DATE SYS-DATE (BOTH YYYYMMDD)
      *    OUT : ELAPSED-RESULTS   (L, E, G, LE, GE SEE 88'S)
      *             SYS-IS-LT
      *             SYS-IS-GT
      *             SYS-IS-GR
      *             SYS-IS-EQ
      *             SYS-IS-LE
      *             SYS-IS-GE
      * REV:
      * BLV 990512 REMOVED SPECIAL CHECKS FOR ZERO DATES IN
      *            CMPDAT ROUTINE
      ********************************************************
       CMPDAT SECTION.
           MOVE "D" TO DATER-ACTION-CODE.
           PERFORM DATER-ROUTINE.

      ***************************************************
      *         CALENDER DATE TEST
      *
      *   DESC: TEST FOR A VALID DATE
      *   IN  : NUM-DATE (YYYYMMDD)
      *   OUT : IF ERROR NUM-DATE = 0
      * REV:
      ***************************************************
       TSTDAT SECTION.
           MOVE "T" TO DATER-ACTION-CODE.
           PERFORM DATER-ROUTINE.


      ********************************************************
      *          ESTABLISH UNITPERIOD LOWERCASE MASK
      *
      *   DESC : ESTABLISH UNIT PERIOD MASK
      *     IN : ALP-DATE-EDIT         DATE 99/99/99
      *          DATER-UNITPER-CD      M, D, Y, S, W
      *          DATER-UNITPER-FREQ    FREQUENCY OF UNIT PERIODS
      *    OUT : ALP-DATE-EDIT         DATE 99/99W99
      *
      *             M - '/'
      *             W - 'W'  IF FREQ = 2  'B'
      *             S - 'S'
      *             Y - 'Y'
      *             D - 'D'
      * REV:
      ********************************************************
       UNITPER-MASK SECTION.
           IF DATER-UNITPER-CD = "M"
              MOVE X"2F" TO DATER-UNITPER-IT
           ELSE
           IF DATER-UNITPER-CD = "W"
              IF DATER-UNITPER-FREQ = 2
                 MOVE X"62" TO DATER-UNITPER-IT
              ELSE
                 MOVE X"77" TO DATER-UNITPER-IT
              END-IF
           ELSE
           IF DATER-UNITPER-CD = "S"
              MOVE X"73" TO DATER-UNITPER-IT
           ELSE
           IF DATER-UNITPER-CD = "Y"
              MOVE X"79" TO DATER-UNITPER-IT
           ELSE
           IF DATER-UNITPER-CD = "D"
              MOVE X"64" TO DATER-UNITPER-IT
           ELSE
           IF DATER-UNITPER-CD = "B"
              MOVE X"62" TO DATER-UNITPER-IT.

      ***********************************************************
      *               (TIMALL MONTHLY ONLY)
      *         DRIVER FOR ALL TIME CALCULATIONS
      *               (360,361,362,358,999,365,367)
      *
      *   DESC: COMPUTE ELAPSED TIME BETWEEN NUM-DATE AND SYS-DATE
      *            - CALCULATIONS ARE DEFAULTED TO:
      *                   DATER-UNITPER-CD    =  M
      *                   DATER-UNITPER-FREQ  =  1
      *            - INPUT DATES NOT ALTERED
      *            - VERIFIES DATES
      *            - 360/361/362/358/999/365/367 YEARTYPE
      *            - OUTPUT MAY BE NEGATIVE
      *   IN     : NUM-DATE               FROM        YYYYMMDD
      *            SYS-DATE               TO          YYYYMMDD
      *            ELAPSED-YRTYPE         360/361/362/358/999/365/367
      *   DEFAULT:
      *            DATER-UNITPER-CD       =  M
      *            DATER-UNITPER-FREQ     =  1
      *   OUT    : ELAPSED-DAYS
      *            ELAPSED-MONTHS
      *            ELAPSED-REM
      *            ELAPSED-RESULT         SYS-IS-LT,GT,EQ,LE,GE,GR
      * REV:
      *  JTG 032487 ADDED LOGIC FOR TIM999.
      *  JTG 101795 ADDED TIM362, MERCURY ALABAMA
      *  JTG 040397 ADDED TIM361, JEFFERSON NJ     NOTE SYSTEM
      * MJD 130213 ADDED DATER-HOLD-NDTE-DATE.  TIMALL REUSES NDTE DATE
      *            EVEN THOUGH IT IS NOT AN INPUT FOR IT BECAUSE IT CALLS
      *            NEWDATE.  THIS CAUSES ISSUES BECAUSE THE CALLING
      *            PROGRAM HAS ALREADY SET NDTE-DATE AND DOES NOT EXPECT IT
      *            TO CHANGE WHEN CALLING TIMALL.  TO FIX THIS I SAVE
      *            NDTE-DATE AND RESTORE IT AFTER THE CALL.
      * MJD 130218 ADDED TEST OF NUM-DATE/SYS-DATE = 0.   VERYANTS
      *            ROUTINES GENERATING SOME ERRORS IF WE PASSED IN BAD DATA.
      *            A15 JUST RETURNED 0'S SO I MIMICKED IT HERE.
      ***********************************************************
       TIMALL SECTION.
           IF ( NUM-DATE = 0 ) OR ( SYS-DATE = 0 )
              MOVE 0 TO ELAPSED-DAYS
                        ELAPSED-MONTHS
                        ELAPSED-REM
              MOVE SPACES TO ELAPSED-RESULTS
           ELSE
              MOVE NDTE-DATE          TO DATER-HOLD-NDTE-DATE
              MOVE DATER-UNITPER-INFO TO HOLD-UNITPER-INFO
              MOVE "M"                TO DATER-UNITPER-CD
              MOVE 1                  TO DATER-UNITPER-FREQ
              MOVE "E"                TO DATER-ACTION-CODE
              PERFORM DATER-ROUTINE
              MOVE HOLD-UNITPER-INFO  TO DATER-UNITPER-INFO
              MOVE DATER-HOLD-NDTE-DATE TO NDTE-DATE.

      ***************************************
       TIM SECTION.
      ***************************************
           MOVE 365 TO ELAPSED-YRTYPE.
           PERFORM TIMALL.

      ***************************************
       TIM999 SECTION.
      ***************************************
           MOVE 999 TO ELAPSED-YRTYPE.
           PERFORM TIMALL.

      ***************************************
       TIM360 SECTION.
      ***************************************
           MOVE 360 TO ELAPSED-YRTYPE.
           PERFORM TIMALL.

      ***************************************
       TIM361 SECTION.
      ***************************************
           MOVE 361 TO ELAPSED-YRTYPE.
           PERFORM TIMALL.

      ***************************************
       TIM362 SECTION.
      ***************************************
           MOVE 362 TO ELAPSED-YRTYPE.
           PERFORM TIMALL.

      ***************************************
       TIM365 SECTION.
      ***************************************
           MOVE 365 TO ELAPSED-YRTYPE.
           PERFORM TIMALL.

      ***************************************
       TIM358 SECTION.
      ***************************************
           MOVE 358 TO ELAPSED-YRTYPE.
           PERFORM TIMALL.

      ***************************************
       TIM367 SECTION.
      ***************************************
           MOVE 367 TO ELAPSED-YRTYPE.
           PERFORM TIMALL.

      ***********************************************************
      *         TIME IN UNIT PERIODS (TIMUPER) ROUTINE
      *
      *   DESC: COMPUTE ELAPSED DAYS BETWEEN NUM-DATE AND SYS-DATE
      *         - INPUT DATES ARE NOT ALTERED
      *         - OUTPUT MAY BE NEGATIVE
      *
      *   IN  : NUM-DATE                FROM        YYYYMMDD
      *         SYS-DATE                TO          YYYYMMDD
      *         DATER-UNITPER-CD        M, D, Y, S, W, B
      *         DATER-UNITPER-FREQ      FREQUENCY OF UNIT PERIODS
      *   OUT : ELAPSED-UNITPER         ELAPSED UNIT PERIODS
      *         ELAPSED-UNITPER-REM     REMAINING DAYS
      *         ELAPSED-RESULT          SYS-IS-LT,GT,GR,EQ,LE,GE
      *         ELAPSED-DAYS
      *         ELAPSED-MONTHS
      *         ELAPSED-REM
      * REV:
      *  JTG 091693 CHANGED TIMUPER TO SET YRTYPE = 360
      *  JTG 031797 CHANGED TIMUPER BACK TO SET YRTYPE = 999
      *             RELIANCE PROBLEM:
      *                UNITPER-CD   = M
      *                UNITPER-FREQ = 1
      *                  TIMUPER YIELDS....(48M & 17REM) 360
      *                SHOULD BE:
      *                  TIMUPER...........(48M & 15REM) 999
      * MJD 170227 ADDED DATER-HOLD-NDTE-DATE.  TIMUPER REUSES NDTE DATE
      *            EVEN THOUGH IT IS NOT AN INPUT FOR IT BECAUSE IT CALLS
      *            NEWDATE.  THIS CAUSES ISSUES BECAUSE THE CALLING
      *            PROGRAM HAS ALREADY SET NDTE-DATE AND DOES NOT EXPECT IT
      *            TO CHANGE WHEN CALLING TIMUPER.  TO FIX THIS I SAVE
      *            NDTE-DATE AND RESTORE IT AFTER THE CALL.
      *            THIS IS THE SAME FIX AS USED FOR TIMALL ABOVE.
      ***********************************************************
       TIMUPER SECTION.
      *   ELAPSED-YRTYPE IS SET FOR (DUMMY) MONTHLY ELAPSED CALCULATIONS
           MOVE NDTE-DATE TO DATER-HOLD-NDTE-DATE.
           MOVE 999       TO ELAPSED-YRTYPE.
           MOVE "E"       TO DATER-ACTION-CODE.
           PERFORM DATER-ROUTINE.
           MOVE DATER-HOLD-NDTE-DATE TO NDTE-DATE.

      ******************************************************************
      *   DESC:  CONVERT ALPHA DATE FORMAT 'MM/DD/YY' TO 'MMDDYY'
      *   IN  :  ALP-DATE
      *   OUT :  DATE-MMDDYY
      *-----------------------------------------------------------------
      * REV:
      * BAH 020416 ***NEW***
      ******************************************************************
       CONVERT-ALPDATE-TO-MMDDYY SECTION.
           MOVE ALP-MO TO DATE-MMDDYY-MM.
           MOVE ALP-DA TO DATE-MMDDYY-DD.
           MOVE ALP-YR TO DATE-MMDDYY-YY.

      ******************************************************************
      *   DESC:  CONVERT ALPHA DATE FORMAT 'MM/DD/YY' TO 'YYMMDD'
      *   IN  :  ALP-DATE
      *   OUT :  DATE-YYMMDD
      *-----------------------------------------------------------------
      * REV:
      * BAH 11/17/2014 NEW
      ******************************************************************
       CONVERT-ALPDATE-TO-YYMMDD SECTION.
           MOVE ALP-YR TO DATE-YYMMDD-YY.
           MOVE ALP-MO TO DATE-YYMMDD-MM.
           MOVE ALP-DA TO DATE-YYMMDD-DD.

      ******************************************************************
      *   DESC:  CONVERT ALPHA DATE FORMAT 'MM/DD/YY' TO 'CCYYMMDD'
      *   IN  :  ALP-DATE
      *   OUT :  DATE-YYYYMMDD
      *-----------------------------------------------------------------
      * REV:
      * BLM 2/13/2017 NEW
      ******************************************************************
       CONVERT-ALPDATE-TO-YYYYMMDD SECTION.
           MOVE ALP-MO TO DATE-YYYYMMDD-MM.
           MOVE ALP-DA TO DATE-YYYYMMDD-DD.
           MOVE ALP-YR TO DATE-YYYYMMDD-YYYY.
           IF ( DATE-YYYYMMDD NOT = ZEROES )
              IF ( DATE-YYYYMMDD-YYYY < EXT-JULIAN-CC )
                 ADD 2000 TO DATE-YYYYMMDD-YYYY
              ELSE
                 ADD 1900 TO DATE-YYYYMMDD-YYYY.

      ******************************************************************
       CONVERT-YYYYMMDD-TO-ALPDATE SECTION.
      ******************************************************************
           MOVE DATE-YYYYMMDD-MM TO ALP-MO.
           MOVE DATE-YYYYMMDD-DD TO ALP-DA.
           MOVE DATE-YYYYMMDD-YY TO ALP-YR.

      ******************************************************************
      *   DESC:  CONVERT DATE FORMAT 'MMDDYY' TO 'MMSDDSYY'
      *   IN  :  DATE-MMDDYY
      *   OUT :  DATE-MMSDDSYY
      *-----------------------------------------------------------------
      * REV:
      * KEC 2000.0425 ***NEW***
      *
      ******************************************************************
       CONVERT-MMDDYY-TO-MMSDDSYY SECTION.

           MOVE DATE-MMDDYY-MM TO DATE-MMSDDSYY-MM.
           MOVE "/"            TO DATE-MMSDDSYY-S1.
           MOVE DATE-MMDDYY-DD TO DATE-MMSDDSYY-DD.
           MOVE "/"            TO DATE-MMSDDSYY-S2.
           MOVE DATE-MMDDYY-YY TO DATE-MMSDDSYY-YY.

       CV-MMDDYY-TO-MMSDDSYY-EXIT.
           EXIT.

      ******************************************************************
      *   DESC:  CONVERT DATE FORMAT 'MMDDYY' TO 'MMSDDSYYYY'
      *   IN  :  DATE-MMDDYY
      *   OUT :  DATE-MMSDDSYYYY
      *-----------------------------------------------------------------
      * REV:
      * KEC 2000.0425 ***NEW***
      *
      ******************************************************************
       CONVERT-MMDDYY-TO-MMSDDSY4 SECTION.

           MOVE DATE-MMDDYY-MM TO DATE-MMSDDSYYYY-MM.
           MOVE "/"            TO DATE-MMSDDSYYYY-S1.
           MOVE DATE-MMDDYY-DD TO DATE-MMSDDSYYYY-DD.
           MOVE "/"            TO DATE-MMSDDSYYYY-S2.
           MOVE DATE-MMDDYY-YY TO DATE-MMSDDSYYYY-YYYY.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *  NOTE: IF DATE-MMDDYY IS SPACES THEN THE FOLLOWING WILL MAKE
      *        DATE-MMSDDSYYYY TO BE "  /  /2000".  MUST ADD NUMERIC
      *        CHECK IN GIVEN PROGRAM PRIOR TO PERFORMING THIS ROUTINE
      *        IF YOU KNOW THAT DATE-MMDDYY COULD BE SPACES/NON-NUMERIC.
      *        SEE THE NOTE AT THE TOP OF LIBGB/DATER FOR MORE INFO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( DATE-MMSDDSYYYY-MM   NOT = ZEROES ) AND
              ( DATE-MMSDDSYYYY-DD   NOT = ZEROES )
              IF ( DATE-MMSDDSYYYY-YYYY < EXT-JULIAN-CC )
                 ADD 2000 TO DATE-MMSDDSYYYY-YYYY
              ELSE
                 ADD 1900 TO DATE-MMSDDSYYYY-YYYY.

       CV-MMDDYY-TO-MMSDDSY4-EXIT.
           EXIT.

      ******************************************************************
      *   DESC:  CONVERT DATE FORMAT 'MMDDYY' TO 'MMYY'
      *   IN  :  DATE-MMDDYY
      *   OUT :  DATE-MMYY
      *-----------------------------------------------------------------
      * REV:
      * KEC 2000.0204 ***NEW***
      *
      ******************************************************************
       CONVERT-MMDDYY-TO-MMYY SECTION.

           MOVE DATE-MMDDYY-MM TO DATE-MMYY-MM.
           MOVE DATE-MMDDYY-YY TO DATE-MMYY-YY.

       CV-MMDDYY-TO-MMYY-EXIT.
           EXIT.

      ******************************************************************
      *   DESC:  CONVERT DATE FORMAT 'MMDDYY' TO 'YYMM'
      *   IN  :  DATE-MMDDYY
      *   OUT :  DATE-YYMM
      *-----------------------------------------------------------------
      * REV:
      * KEC 2000.0204 ***NEW***
      *
      ******************************************************************
       CONVERT-MMDDYY-TO-YYMM SECTION.

           MOVE DATE-MMDDYY-MM TO DATE-YYMM-MM.
           MOVE DATE-MMDDYY-YY TO DATE-YYMM-YY.

       CV-MMDDYY-TO-YYMM-EXIT.
           EXIT.


      ******************************************************************
      *   DESC:  CONVERT DATE FORMAT 'YYYYMMDD' TO 'YYYYMM'
      *   IN  :  DATE-YYYYMMDD
      *   OUT :  DATE-YYYYMM
      ******************************************************************
       CONVERT-YYYYMMDD-TO-YYYYMM SECTION.

           MOVE DATE-YYYYMMDD-MM   TO DATE-YYYYMM-MM.
           MOVE DATE-YYYYMMDD-YYYY TO DATE-YYYYMM-YYYY.

       CV-YYYYMMDD-TO-YYYYMM-EXIT.
           EXIT.

      ******************************************************************
       CONVERT-YYYYMMDD-TO-YYMMDD SECTION.
      ******************************************************************

           MOVE DATE-YYYYMMDD-MM TO DATE-YYMMDD-MM.
           MOVE DATE-YYYYMMDD-DD TO DATE-YYMMDD-DD.
           MOVE DATE-YYYYMMDD-YY TO DATE-YYMMDD-YY.

       CV-YYYYMMDD-TO-YYMMDD-EXIT.
           EXIT.

      ******************************************************************
      *   DESC:  CONVERT DATE FORMAT 'MMDDYY' TO 'YYYYMMDD'
      *   IN  :  DATE-MMDDYY
      *   OUT :  DATE-YYYYMMDD
      *-----------------------------------------------------------------
      * REV:
      * KEC 033099 ***NEW***
      ******************************************************************
       CONVERT-MMDDYY-TO-YYYYMMDD SECTION.

           MOVE DATE-MMDDYY-MM TO DATE-YYYYMMDD-MM.
           MOVE DATE-MMDDYY-DD TO DATE-YYYYMMDD-DD.
           MOVE DATE-MMDDYY-YY TO DATE-YYYYMMDD-YYYY.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *  NOTE: IF DATE-MMDDYY IS SPACES THEN THE FOLLOWING WILL MAKE
      *        DATE-YYYYMMDD TO BE "2000    ".  MUST ADD NUMERIC CHECK
      *        IN GIVEN PROGRAM PRIOR TO PERFORMING THIS ROUTINE IF
      *        YOU KNOW THAT DATE-MMDDYY COULD BE SPACES/NON-NUMERIC.
      *        SEE THE NOTE AT THE TOP OF LIBGB/DATER FOR MORE INFO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( DATE-YYYYMMDD NOT = ZEROES )
              IF ( DATE-YYYYMMDD-YYYY < EXT-JULIAN-CC )
                 ADD 2000 TO DATE-YYYYMMDD-YYYY
              ELSE
                 ADD 1900 TO DATE-YYYYMMDD-YYYY.

       CV-MMDDYY-TO-YYYYMMDD-EXIT.
           EXIT.

      ******************************************************************
      *   DESC:  CONVERT DATE FORMAT 'MMDDYY' TO 'YYYYMM'
      *   IN  :  DATE-MMDDYY
      *   OUT :  DATE-YYYYMM
      *-----------------------------------------------------------------
      * REV:
      * KEC 2000.0204 ***NEW***
      *
      ******************************************************************
       CONVERT-MMDDYY-TO-YYYYMM SECTION.

           MOVE DATE-MMDDYY-MM TO DATE-YYYYMM-MM.
           MOVE DATE-MMDDYY-YY TO DATE-YYYYMM-YYYY.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *  NOTE: IF DATE-MMDDYY IS SPACES THEN THE FOLLOWING WILL MAKE
      *        DATE-YYYYMM TO BE "2000  ".  MUST ADD NUMERIC CHECK
      *        IN GIVEN PROGRAM PRIOR TO PERFORMING THIS ROUTINE IF
      *        YOU KNOW THAT DATE-MMDDYY COULD BE SPACES/NON-NUMERIC.
      *        SEE THE NOTE AT THE TOP OF LIBGB/DATER FOR MORE INFO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( DATE-YYYYMM NOT = ZEROES )
              IF ( DATE-YYYYMM-YYYY < EXT-JULIAN-CC )
                 ADD 2000 TO DATE-YYYYMM-YYYY
              ELSE
                 ADD 1900 TO DATE-YYYYMM-YYYY.

      ******************************************************************
      *   DESC:  CONVERT DATE FORMAT 'MMYY' TO 'MMDDYY'
      *   IN  :  DATE-MMYY
      *   OUT :  DATE-MMDDYY
      *-----------------------------------------------------------------
      * REV:
      * KEC 2000.0204 ***NEW***
      *
      ******************************************************************
       CONVERT-MMYY-TO-MMDDYY SECTION.

           MOVE DATE-MMYY-MM TO DATE-MMDDYY-MM.
           MOVE ZEROES       TO DATE-MMDDYY-DD.
           MOVE DATE-MMYY-YY TO DATE-MMDDYY-YY.

       CV-MMYY-TO-MMDDYY-EXIT.
           EXIT.

      ******************************************************************
      *   DESC:  CONVERT DATE FORMAT 'YYMMDD' TO 'MMDDYY'
      *   IN  :  DATE-YYMMDD
      *   OUT :  DATE-MMDDYY
      *-----------------------------------------------------------------
      * REV:
      * KEC 033099 ***NEW***
      ******************************************************************
       CONVERT-YYMMDD-TO-MMDDYY SECTION.

           MOVE DATE-YYMMDD-MM TO DATE-MMDDYY-MM.
           MOVE DATE-YYMMDD-DD TO DATE-MMDDYY-DD.
           MOVE DATE-YYMMDD-YY TO DATE-MMDDYY-YY.

       CV-YYMMDD-TO-MMDDYY-EXIT.
           EXIT.


      ******************************************************************
      *   DESC:  CONVERT DATE FORMAT 'YYYYMM' TO 'MMDDYY'
      *   IN  :  DATE-YYYYMM
      *   OUT :  DATE-MMDDYY
      *-----------------------------------------------------------------
      * REV:
      * KEC 2000.0204 ***NEW***
      *
      ******************************************************************
       CONVERT-YYYYMM-TO-MMDDYY SECTION.

           MOVE DATE-YYYYMM-MM TO DATE-MMDDYY-MM.
           MOVE ZEROES         TO DATE-MMDDYY-DD.
           MOVE DATE-YYYYMM-YY TO DATE-MMDDYY-YY.

       CV-YYYYMM-TO-MMDDYY-EXIT.
           EXIT.

      ******************************************************************
      *   DESC:  CONVERT DATE FORMAT 'YYYYMMDD' TO 'MMDDYY'
      *   IN  :  DATE-YYYYMMDD
      *   OUT :  DATE-MMDDYY
      *-----------------------------------------------------------------
      * REV:
      * KEC 033099 ***NEW***
      ******************************************************************
       CONVERT-YYYYMMDD-TO-MMDDYY SECTION.

           MOVE DATE-YYYYMMDD-MM TO DATE-MMDDYY-MM.
           MOVE DATE-YYYYMMDD-DD TO DATE-MMDDYY-DD.
           MOVE DATE-YYYYMMDD-YY TO DATE-MMDDYY-YY.

       CV-YYYYMMDD-TO-MMDDYY-EXIT.
           EXIT.

      ******************************************************************
      *   DESC:  CONVERT DATE FORMAT 'YYYYMMDD' TO 'MMDDYYYY'
      *   IN  :  DATE-YYYYMMDD
      *   OUT :  DATE-MMDDYYYY
      *-----------------------------------------------------------------
      * REV:
      *  BAH 10/20/2016  ***NEW***
      ******************************************************************
       CONVERT-YYYYMMDD-TO-MMDDYYYY SECTION.
           MOVE DATE-YYYYMMDD-MM   TO DATE-MMDDYYYY-MM.
           MOVE DATE-YYYYMMDD-DD   TO DATE-MMDDYYYY-DD.
           MOVE DATE-YYYYMMDD-YYYY TO DATE-MMDDYYYY-YYYY.

      ******************************************************************
      *   DESC:  CONVERT DATE FORMAT 'YYMMDD' TO 'YYYYMMDD'
      *   IN  :  DATE-YYMMDD
      *   OUT :  DATE-YYYYMMDD
      *-----------------------------------------------------------------
      * REV:
      * BLM 100208 ***NEW***
      ******************************************************************
       CONVERT-YYMMDD-TO-YYYYMMDD SECTION.

           MOVE DATE-YYMMDD-MM TO DATE-YYYYMMDD-MM.
           MOVE DATE-YYMMDD-DD TO DATE-YYYYMMDD-DD.
           MOVE DATE-YYMMDD-YY TO DATE-YYYYMMDD-YYYY.

           IF ( DATE-YYYYMMDD NOT = ZEROES )
              IF ( DATE-YYYYMMDD-YYYY < EXT-JULIAN-CC )
                 ADD 2000 TO DATE-YYYYMMDD-YYYY
              ELSE
                 ADD 1900 TO DATE-YYYYMMDD-YYYY.

       CV-YYMMDD-TO-YYYYMMDD-EXIT.
           EXIT.

      ******************************************************************
      *   DESC:  CONVERT DATE FORMAT 'MMDDYYYY' TO 'YYYYMMDD'
      *   IN  :  DATE-MMDDYYYY
      *   OUT :  DATE-YYYYMMDD
      ******************************************************************
       CONVERT-MMDDYYYY-TO-YYYYMMDD SECTION.

           MOVE DATE-MMDDYYYY-MM TO DATE-YYYYMMDD-MM.
           MOVE DATE-MMDDYYYY-DD TO DATE-YYYYMMDD-DD.
           MOVE DATE-MMDDYYYY-YYYY TO DATE-YYYYMMDD-YYYY.

       CV-MMDDYYYY-TO-YYYYMMDD-EXIT.
           EXIT.

      ***********************************************************
      *
      *   DESC: COMPUTE ELAPSED TIME BETWEEN NUM-DATE AND SYS-DATE
      *            - CALCULATIONS ARE DEFAULTED TO:
      *                   DATER-UNITPER-CD    =  M
      *                   DATER-UNITPER-FREQ  =  1
      *            - INPUT DATES NOT ALTERED
      *            - VERIFIES DATES
      *            - 360/361/362/358/999/365/367 YEARTYPE
      *            - OUTPUT MAY BE NEGATIVE
      *   IN     : NUM-DATE               FROM        MMDDYY
      *            SYS-DATE               TO          MMDDYY
      *            ELAPSED-YRTYPE         360/361/362/358/999/365/367
      *   DEFAULT:
      *            DATER-UNITPER-CD       =  M
      *            DATER-UNITPER-FREQ     =  1
      *   OUT    : ELAPSED-DAYS
      *            ELAPSED-MONTHS
      *            ELAPSED-REM
      *            ELAPSED-RESULT         SYS-IS-LT,GT,EQ,LE,GE,GR
      ***********************************************************
       C-ELAPSED-TIME SECTION.
           MOVE DATER-UNITPER-FREQ TO WS-XUNITPER-FREQ.
           MOVE DATER-UNITPER-CD   TO WS-XUNITPER-CODE.
           MOVE NUM-DATE           TO WS-DATE1 WS-DATE1W.
           MOVE SYS-DATE           TO WS-DATE2 WS-DATE2W.
      *----------------------------------------------------------------------
      *      TEST NUM-DATE AND/OR SYS-DATE = 0
      *      IF SO SET ALL OUTPUT = 0
      *          NOTE:
      *                CDTOI(   YR = 0, YIELDS 100   )
      *----------------------------------------------------------------------*/
           IF WS-DATE1 = 0 OR WS-DATE2 = 0
      *       MOVE 010101 TO WS-DATE1 WS-DATE2 WS-DATE1W WS-DATE2W.
              MOVE 19000101 TO WS-DATE1 WS-DATE2 WS-DATE1W WS-DATE2W.

           MOVE ELAPSED-YRTYPE TO TA-YRTYPE.
      *-----------------------------------------------------
      *       BASED ON YEAR TYPE
      *       DETERMINE:
      *                 ELAPSED MONTHS
      *                 ELAPSED DAYS
      *                 ELAPSED REM
      *                 RESULTS OF COMPARE
      *-----------------------------------------------------*
           PERFORM C-TIMALL
           PERFORM C-TIM-RESULTS
           MOVE WS-TIM-RESULTS TO ELAPSED_RESULTS
           MOVE WS-XMONTHS     TO ELAPSED-MONTHS
           MOVE WS-XDAYS       TO ELAPSED-DAYS
           MOVE WS-XREM        TO ELAPSED-REM
      *-----------------------------------------------------
      *       BASED ON UNIT PERIOD CODE & FREQ
      *       DETERMINE:
      *                 ELAPSED UNIT PERIODS
      *                 ELAPSED REM
      *-----------------------------------------------------*
           MOVE WS-DATE1W TO WS-DATE1
           MOVE WS-DATE2W TO WS-DATE2
           IF WS-XUNITPER-CODE = "Y" OR WS-XUNITPER-CODE = "M"
              MOVE 998 TO TA-YRTYPE
              PERFORM C-TIMALL
           ELSE
              IF WS-XUNITPER-CODE = "S"
                 MOVE 360 TO TA-YRTYPE
                 PERFORM C-TIMALL
              ELSE
                 MOVE 365 TO TA-YRTYPE
                 PERFORM C-TIMALL
              END-IF
           END-IF

           EVALUATE WS-XUNITPER-CODE
           WHEN "M"
                   IF WS-XUNITPER-FREQ = 1
                      MOVE WS-XMONTHS TO WS-XUNITPER
                      MOVE WS-XREM    TO WS-XUNITPER-REM
                   ELSE
                      COMPUTE WS-XDAYS = WS-XREM + (WS-XMONTHS * 30)
                      COMPUTE WS-DIV-TMP = WS-XUNITPER-FREQ * 30
                      DIVIDE  WS-XDAYS BY WS-DIV-TMP
                                         GIVING     WS-XUNITPER
                                         REMAINDER  WS-XUNITPER-REM
                   END-IF
           WHEN "S"
                   COMPUTE WS-XDAYS = (WS-XMONTHS * 30) + WS-XREM
                   COMPUTE WS-DIV-TMP = WS-XUNITPER-FREQ * 15
                   DIVIDE  WS-XDAYS BY WS-DIV-TMP
                                       GIVING     WS-XUNITPER
                                       REMAINDER  WS-XUNITPER-REM
           WHEN "W"
                   COMPUTE WS-DIV-TMP = WS-XUNITPER-FREQ * 7
                   DIVIDE  WS-XDAYS BY WS-DIV-TMP
                                       GIVING     WS-XUNITPER
                                       REMAINDER  WS-XUNITPER-REM
           WHEN "B"
                   COMPUTE WS-DIV-TMP = WS-XUNITPER-FREQ * 14
                   DIVIDE  WS-XDAYS BY WS-DIV-TMP
                                       GIVING     WS-XUNITPER
                                       REMAINDER  WS-XUNITPER-REM
           WHEN "D"
                   DIVIDE  WS-XDAYS BY WS-XUNITPER-FREQ
                                       GIVING     WS-XUNITPER
                                       REMAINDER  WS-XUNITPER-REM
           WHEN "Y"
                   COMPUTE WS-DIV-TMP = WS-XUNITPER-FREQ * 12
                   DIVIDE  WS-XMONTHS  BY WS-DIV-TMP
                                       GIVING     WS-XUNITPER
                                       REMAINDER  WS-XUNITPER-REM
           END-EVALUATE
           MOVE WS-XUNITPER     TO ELAPSED-UNITPER
           MOVE WS-XUNITPER-REM TO ELAPSED-UNITPER-REM

      *    MJD NOT PART OF OUTPUT
      *    INITIALIZE NDTE-DATE
           .

      **********************************************************
      *              (358,360,361,362,365,367,999 & 998 UNITPER)
      *
      *   NAME:  C-TIMALL
      *   DESC:  PERFORMS SELECTED TIME ROUTINE:
      *          358/360/361/362/365/367/999  & 998 UNITPER
      *   IN  :  TA-YRTYPE
      *   OUT :
      *********************************************************
       C-TIMALL SECTION.
           EVALUATE TA-YRTYPE
           WHEN 358 PERFORM C-TIM360-358
           WHEN 360 PERFORM C-TIM360-358
           WHEN 361 PERFORM C-TIM361
           WHEN 362 PERFORM C-TIM362
           WHEN 365 PERFORM C-TIM365-367
           WHEN 367 PERFORM C-TIM365-367
           WHEN 998 PERFORM C-TIM999-998
           WHEN 999 PERFORM C-TIM999-998
           END-EVALUATE
           .

      ***********************************************************
      *         COMPUTE ELAPSED TIME USING 360 OR 358 YRTYPE
      *
      *   NAME: C-TIM360_358
      *   DESC: COMPUTE ELAPSED TIMES BETWEEN DATE1 AND DATE2
      *
      *   IN  : YEAR TYPE
      *   OUT : ELAPSED_MONTHS, ELAPSED_DAYS AND ELAPSED_REM
      ***********************************************************
       C-TIM360-358 SECTION.
           PERFORM C-TIMBEG.
           IF WS-DATE1-DD > 30
              MOVE 30 TO WS-DATE1-DD.
           IF WS-DATE2-DD > 30
              MOVE 30 TO WS-DATE2-DD.

           MOVE WS-DATE2-CCYY TO LEAP-YEAR-CCYY.
           PERFORM C-LEAP-YEAR-TEST.

           IF WS-DATE2-MM = 2 AND WS-DATE2-DD > WS-DATE1-DD
              IF WS-DATE2-DD = 29 AND LEAP-YEAR-TRUE
                 MOVE 30 TO WS-DATE2-DD
              ELSE
                IF WS-DATE2-DD = 28 AND LEAP-YEAR-FALSE
                   MOVE 30 TO WS-DATE2-DD.

           IF WS-DATE2-MM = 2 AND WS-DATE2-DD < WS-DATE1-DD
              IF WS-DATE2-DD = 29 AND LEAP-YEAR-TRUE
                 MOVE 29 TO WS-DATE1-DD
              ELSE
                IF WS-DATE2-DD = 28 AND LEAP-YEAR-FALSE
                   MOVE 28 TO WS-DATE1-DD.

           IF TA-YRTYPE = 358
              MOVE WS-DATE1-CCYY TO LEAP-YEAR-CCYY
              PERFORM C-LEAP-YEAR-TEST
              IF WS-DATE1-MM = 2 AND WS-DATE1-DD > WS-DATE2-DD
                 IF LEAP-YEAR-FALSE
                    ADD 2 TO WS-DATE1-DD
                 ELSE
                    ADD 1 TO WS-DATE1-DD.

           COMPUTE WS-XDAYS = ( (WS-DATE2-CCYY - WS-DATE1-CCYY) * 360 )
                              + ( (WS-DATE2-MM - WS-DATE1-MM) * 30 )
                              + (WS-DATE2-DD - WS-DATE1-DD).
           PERFORM C-TIMEND.

      ******************************************************************
      *         COMPUTE ELAPSED TIME USING 361 YRTYPE
      *   NOTE:
      *         THIS IS A VERSION OF 358 YEAR-TYPE MODIFY FOR NOTES.
      *         IT CAUSES TIME FROM FEB. 28 TO THE (29TH, 30TH OR 31ST)
      *         OF ANOTHER MONTH, TO VIEW FEB. 28 AS THE 30TH.
      *
      *   NAME: C-TIM361
      *   DESC: COMPUTE ELAPSED TIME BETWEEN DATE1 AND DATE2
      *
      *   IN  :
      *   OUT : ELAPSED_MONTHS, ELAPSED_DAYS AND ELAPSED_REM
      ******************************************************************
       C-TIM361 SECTION.
           PERFORM C-TIMBEG.
           IF WS-DATE1-DD > 30
              MOVE 30 TO WS-DATE1-DD.
           IF WS-DATE2-DD > 30
              MOVE 30 TO WS-DATE2-DD.
           IF WS-DATE2-MM = 2 AND WS-DATE2-DD > WS-DATE1-DD
              MOVE WS-DATE2-CCYY TO LEAP-YEAR-CCYY
              PERFORM C-LEAP-YEAR-TEST
              IF LEAP-YEAR-TRUE AND WS-DATE2-DD = 29
                 MOVE 30 TO WS-DATE2-DD
              ELSE
                IF LEAP-YEAR-FALSE AND WS-DATE2-DD = 28
                   MOVE 30 TO WS-DATE2-DD.

           IF WS-DATE2-MM = 2 AND WS-DATE2-DD < WS-DATE1-DD
              MOVE WS-DATE2-CCYY TO LEAP-YEAR-CCYY
              PERFORM C-LEAP-YEAR-TEST
              IF LEAP-YEAR-TRUE AND WS-DATE2-DD = 29
                 MOVE 29 TO WS-DATE1-DD
              ELSE
                IF LEAP-YEAR-FALSE AND WS-DATE2-DD = 28
                   MOVE 28 TO WS-DATE1-DD.

           IF NOT (
              WS-DATE1-MM NOT = 2 OR
              (WS-DATE1-DD < 28 AND WS-DATE1-DD NOT > WS-DATE2-DD) OR
              (WS-DATE1-DD = WS-DATE2-DD AND WS-DATE1-MM = WS-DATE2-MM
               AND WS-DATE1-CCYY = WS-DATE2-CCYY)
                  )
              MOVE WS-DATE1-CCYY TO LEAP-YEAR-CCYY
              PERFORM C-LEAP-YEAR-TEST
              IF LEAP-YEAR-FALSE
                 ADD 2 TO WS-DATE1-DD
              ELSE
                 ADD 1 TO WS-DATE1-DD.

           COMPUTE WS-XDAYS = ((WS-DATE2-CCYY - WS-DATE1-CCYY) * 360)
                            + ((WS-DATE2-MM - WS-DATE1-MM) * 30)
                            + (WS-DATE2-DD - WS-DATE1-DD ).

           PERFORM C-TIMEND.

      ***********************************************************
      *         COMPUTE ELAPSED TIME USING 362 YRTYPE
      *
      *   NAME: C-TIM362
      *   DESC: COMPUTE ELAPSED TIME BETWEEN DATE1 AND DATE2
      *
      *   IN  :
      *   OUT : ELAPSED_MONTHS, ELAPSED_DAYS AND ELAPSED_REM
      ***********************************************************
       C-TIM362 SECTION.
           MOVE WS-DATE2 TO WS-DATEX.
           PERFORM C-TIMBEG.
           COMPUTE WS-XREM    = WS-DATE2-CCYY - WS-DATE1-CCYY.
           COMPUTE WS-XMONTHS = WS-DATE2-MM - WS-DATE1-MM.
           COMPUTE WS-XDAYS   = WS-DATE2-DD - WS-DATE1-DD.

           IF ( WS-XDAYS NOT < 0 )
              COMPUTE WS-XDAYS = (( (WS-XREM * 12) + WS-XMONTHS) * 30 )
                                 + WS-XDAYS
           ELSE
              ADD -1 TO WS-XMONTHS
              COMPUTE WS-HOLD-MOS = (WS-XREM * 12) + WS-XMONTHS
              MOVE -1 TO WS-UP-WORK
              MOVE WS-DATEX-CCYY TO WS-NDTE-CCYY-S
              MOVE WS-DATEX-MM TO WS-NDTE-MM-S
              MOVE WS-DATEX-DD TO WS-NDTE-DD-S
              PERFORM C-INCR-MONTHS
              MOVE WS-NDTE-CCYY-S TO WS-DATEX-CCYY
              MOVE WS-NDTE-MM-S TO WS-DATEX-MM
              MOVE WS-NDTE-DD-S TO WS-DATEX-DD
              MOVE WS-DATE1-DD TO WS-DATEX-DD
              IF WS-DATE1-DD > 28
                 COMPUTE WS-TMP-WORK = DYTOM(WS-DATEX-MM + 1) -
                                       DYTOM(WS-DATEX-MM)
                 MOVE WS-DATEX-CCYY TO LEAP-YEAR-CCYY
                 PERFORM C-LEAP-YEAR-TEST
                 IF LEAP-YEAR-FALSE AND WS-DATEX-MM = 2
                    ADD 1 TO WS-TMP-WORK
                 END-IF
                 IF WS-DATE1-DD > WS-TMP-WORK
                    COMPUTE WS-DATEX-MM = WS-DATE2-MM
                    MOVE 1 TO WS-DATEX-DD
                    MOVE WS-DATE2-CCYY TO WS-DATEX-CCYY
                 END-IF
              END-IF

              MOVE WS-DATE2 TO P-DATE-IN
              PERFORM C-CALL-JUL
              MOVE P-JULIAN-DATE TO WS-TMP-JUL-1

              MOVE WS-DATEX TO P-DATE-IN
              PERFORM C-CALL-JUL
              MOVE P-JULIAN-DATE TO WS-TMP-JUL-2

              COMPUTE WS-XDAYS = WS-TMP-JUL-1 - WS-TMP-JUL-2
              IF WS-XDAYS > 30
                 COMPUTE WS-XDAYS = WS-XDAYS + ((WS-HOLD-MOS - 1) * 30)
              ELSE
                 COMPUTE WS-XDAYS = WS-XDAYS + (WS-HOLD-MOS * 30)
              END-IF
           END-IF.

           PERFORM C-TIMEND.

      ***********************************************************
      *         COMPUTE ELAPSED TIME USING 365 OR 367 YRTYPE
      *
      *   NAME: C-TIM365_367
      *   DESC: COMPUTE ELAPSED TIME BETWEEN DATE1 AND DATE2
      *
      *   IN  :
      *   OUT : ELAPSED_MONTHS, ELAPSED_DAYS AND ELAPSED_REM
      ***********************************************************
       C-TIM365-367 SECTION.
           PERFORM C-TIMBEG.
           COMPUTE WS-XREM    = WS-DATE2-CCYY - WS-DATE1-CCYY.
           COMPUTE WS-XMONTHS = WS-DATE2-MM - WS-DATE1-MM.
           COMPUTE WS-XDAYS   = WS-DATE2-DD - WS-DATE1-DD.
           IF WS-XDAYS < 0
              MOVE WS-DATE2-MM TO WS-DATE1-MM
              MOVE WS-DATE2-CCYY TO WS-DATE1-CCYY
              COMPUTE WS-WORK = DYTOM(WS-DATE2-MM + 1)
                                - DYTOM(WS-DATE2-MM)
              DIVIDE WS-DATE2-YY BY 4 GIVING WS-DIV-RESULT
                                  REMAINDER WS-MOD-4

              MOVE WS-DATE2-CCYY TO LEAP-YEAR-CCYY
              PERFORM C-LEAP-YEAR-TEST

      *****************
      *  BHO - THE ORIGINAL CHECK CLOSELY CORRESPONDS TO THE C CODE,
      *        SO THE LEAP DAY CHECK WOULD TYPICALLY EVALUATE THE
      *        SAME WAY FOR MOST YEARS DIVISIBLE BY 4. 
      *****************
              IF WS-MOD-4 = 0 AND WS-DATE2-MM > 1
                ADD 1 TO WS-WORK
              END-IF
      *****************
      *       IF LEAP-YEAR-TRUE AND WS-DATE2-MM > 1
      *           ADD 1 TO WS-WORK
      *       END-IF

              IF WS-DATE2-DD NOT < WS-WORK
                 MOVE 0 TO WS-XDAYS
              END-IF

              ADD -1 TO WS-XMONTHS
              ADD -1 TO WS-DATE1-MM
              IF WS-DATE1-MM = 0
                 MOVE 12 TO WS-DATE1-MM
                 ADD -1  TO WS-DATE1-CCYY
              END-IF

              COMPUTE WS-WORK = DYTOM(WS-DATE1-MM + 1)
                                - DYTOM(WS-DATE1-MM)

      *****************
      * 2024/08/01
      *  BHO - I RESTORED VERYANT'S CODE FOR THE SECOND SET OF DATE
      *        CALCULATIONS BECAUSE IT APPEARS THAT MIKE (MJD) MAY HAVE
      *        MISUNDERSTOOD WHAT THE C CODE IS DOING. THE C CODE
      *        ISN'T ACTUALLY CHECKING TO SEE IF THE SECOND DATE IS
      *        A LEAP YEAR IN ORDER TO ADD A DAY LIKE MIKE ASSUMED.
      *
      *        IT IS ACTUALLY TESTING THE OPPOSITE TO MAKE SURE 
      *        THAT IT DOESN'T ADD MORE DAYS THAN NECESSARY.
      *        IT'S NOT EASILY APPARENT WHAT THE C CODE IS DOING 
      *        BECAUSE OF THE STYLE IN WHICH IT WAS WITTEN.
      *
      *        THE FOLLOWING COMMENT IS FROM VERYANT...

      ***************
      *
      *  THE FOLLOWING ADJUSTMENT IS TO APPLY THE MODULUS 4 OPERATION
      *  TO THE NEGATED VALUE OF THE DATE1 YEAR
      *  ALTHOUGH IT IS A STRANGE WAY OF EVALUATING THIS EXPRESSION
      *  THIS WILL BE THE ACCURATE CONVERSION OF THE ORIGINAL C EXPRESSION
      *
      ***************
               IF WS-DATE1-YY = ZERO
                   MOVE 1 TO NOT-YEAR
               ELSE
                   MOVE 0 TO NOT-YEAR
               END-IF

               DIVIDE NOT-YEAR    BY 4 GIVING WS-DIV-RESULT
                                       REMAINDER WS-MOD-4

               IF WS-MOD-4 NOT = 0 AND WS-DATE1-MM > 1
                  ADD 1 TO WS-WORK
               END-IF

              IF WS-DATE1-DD NOT < WS-WORK
                  MOVE WS-DATE2-DD TO WS-XDAYS
              ELSE
                  COMPUTE WS-XDAYS = WS-DATE2-DD + WS-WORK
                                                 - WS-DATE1-DD
              END-IF

      *****************
      *  BHO - PRESERVING MIKE'S REASONING BELOW. HIS CODE HAS
      *        BEEN COMMENTED OUT AND REPLACED WITH VERYANT'S
      *        ORIGINAL CODE ABOVE. WANTED SOMEONE TO KNOW WHAT
      *        IT LOOKED LIKE IN CASE THERE'S A BUG IN THE ABOVE.
      ***************
      * MJD 20151118 NOT REALLY SURE WHAT IS GOING ON HERE
      *  EFFECTIVELY IT IS SAYING THAT IF YEAR IS "2000" AND MONTH IS 2 OR LATER
      *    THEN INCREASE WORK BY ONE.  I GET THE LEAR YEAR TEST BECAUSE
      *    THIS IS BASED ON REAL DAYS BUT NOT SURE WHY THE STD LEAP YEAR TEST
      *    STD TEST DID NOT WORK OR WHY IT ONLY LOOKS FOR 2000.
      *    MAY NEED TO REVISIT THIS IN TESTING.
      ***************
      *        IF WS-DATE1-YY = ZERO MOVE 1 TO NOT-YEAR
      *        ELSE                  MOVE 0 TO NOT-YEAR
      *        END-IF
      **       DIVIDE WS-DATE1-YY BY 4 GIVING WS-DIV-RESULT
      *        DIVIDE NOT-YEAR    BY 4 GIVING WS-DIV-RESULT
      *                             REMAINDER WS-MOD-4

      **       IF WS-MOD-4     = 0 AND WS-DATE1-MM > 1
      *        IF WS-MOD-4 NOT = 0 AND WS-DATE1-MM > 1
      *           ADD 1 TO WS-WORK
      *        END-IF
      ***************
      * ADDING LOGIG AS IT APPEAR IN DATER.C
      * REALLY NEED TO LOOK AT THIS IN TESTING!!!!!!
      *       MOVE WS-DATE1-CCYY TO LEAP-YEAR-CCYY
      *       PERFORM C-LEAP-YEAR-TEST
      *       IF LEAP-YEAR-TRUE AND WS-DATE1-MM > 1
      *          ADD 1 TO WS-WORK
      *       END-IF
      ***************

      *       IF WS-DATE1-DD NOT < WS-WORK
      *          MOVE WS-DATE2-DD TO WS-XDAYS
      *       ELSE
      *          COMPUTE WS-XDAYS = WS-DATE2-DD + WS-WORK - WS-DATE1-DD
      *       END-IF

           END-IF
      * C-TIM365-367-COMPUTE (IF WS-XDAYS NOT < 0, LOGIC WILL FALL THRU HERE)
           COMPUTE WS-XDAYS = (WS-XREM * 360) + (WS-XMONTHS * 30)
                              + WS-XDAYS

           PERFORM C-TIMEND.

      ***********************************************************
      * ORIGINAL REMARKS
      *         COMPUTE ELAPSED TIME USING 999 YRTYPE
      *                TRUTH AND LENDING METHOD
      *           AND
      *                 ELAPSED UNIT PERIODS 998 YRTYPE
      *
      *   NAME: TIM999_998
      *   DESC: COMPUTE ELAPSED TIMES BETWEEN DATE1 AND DATE2
      *         BASED ON TRUTH AND LENDING REG. Z
      *
      *         COMPUTE ELAPSED UNIT PERIODS BETWEEN DATE1
      *         AND DATE2
      *
      *   IN  :
      *   OUT : ELAPSED_MONTHS, ELAPSED_DAYS AND ELAPSED_REM
      *         IF 998 ELAPSED_MONTHS = ELAPSED UNIT PERIODS
      * REV:
      *  JTG 050297 CHANGED TO PASS YRTYPE TO C-TIMEND FOR 998
      *             ELAPSED UNIT PERIODS
      *             EXAMPLE:
      *                     NUM-DATE:  030397
      *                     SYS-DATE:  050297
      *      
      *                                     999     998
      *                                     ---     ---
      *                     ELAPSED_MONTHS:   2       1
      *                     ELAPSED_DAYS  :  60      60
      *                     ELAPSED_REM   :   0      30
      ***********************************************************
       C-TIM999-998 SECTION.
           PERFORM C-TIMBEG.
           COMPUTE WS-XMONTHS = (
                           (WS-DATE2-CCYY - WS-DATE1-CCYY) * 12 ) +
                           (  WS-DATE2-MM - WS-DATE1-MM ).

           IF WS-DATE1-DD > WS-DATE2-DD
              ADD -1 TO WS-XMONTHS.

           COMPUTE WS-UP-WORK = 0 - WS-XMONTHS.
           MOVE WS-DATE2-MM TO WS-NDTE-MM-S.
           MOVE WS-DATE2-DD TO WS-NDTE-DD-S.
           MOVE WS-DATE2-CCYY TO WS-NDTE-CCYY-S.
           PERFORM C-INCR-MONTHS.
           MOVE WS-NDTE-CCYY-S TO WS-DATE2-CCYY.
           MOVE WS-NDTE-MM-S TO WS-DATE2-MM.
           MOVE WS-NDTE-DD-S TO WS-DATE2-DD.

           MOVE WS-DATE2 TO P-DATE-IN.
           PERFORM C-CALL-JUL.
           MOVE P-JULIAN-DATE TO WS-JUL-TMP.
           COMPUTE WS-XDAYS = (WS-XMONTHS * 30) + WS-JUL-TMP - WS-JUL-1.

           PERFORM C-TIMEND.

      *********************************************************
      *          INITIAL ROUTINES FOR ALL ELAPSED TIME ROUTINES
      *
      *   NAME:  C-TIMBEG
      *   DESC:  THIS ROUTINE SETS UP THE DATES IN REVERSE
      *          ORDER IF NUMDATE IS AFTER SYSDATE
      *          THIS ROUTINE ALSO CONVERTS DATES TO JULIAN
      *   IN  :
      *   OUT :
      *********************************************************
       C-TIMBEG SECTION.
           MOVE NUM-DATE TO SV-NUM-DATE.
           MOVE SYS-DATE TO SV-SYS-DATE.

           MOVE WS-DATE1 TO P-DATE-IN.
           PERFORM C-CALL-JUL.
           MOVE P-JULIAN-DATE TO WS-JUL-1.

           MOVE WS-DATE2 TO P-DATE-IN.
           PERFORM C-CALL-JUL.
           MOVE P-JULIAN-DATE TO WS-JUL-2.

           IF WS-JUL-1 > WS-JUL-2
              MOVE WS-DATE1 TO WS-HOLDDATE
              MOVE WS-DATE2 TO WS-DATE1
              MOVE WS-HOLDDATE TO WS-DATE2
              MOVE WS-JUL-1    TO WS-HOLDJUL
              MOVE WS-JUL-2    TO WS-JUL-1
              MOVE WS-HOLDJUL  TO WS-JUL-2
              MOVE -1 TO WS-XSIGN
           ELSE
              MOVE  1 TO WS-XSIGN.

           MOVE SV-NUM-DATE TO NUM-DATE.
           MOVE SV-SYS-DATE TO SYS-DATE.

      **********************************************************
      *          END ROUTINES FOR ALL ELAPSED TIME ROUTINE
      *
      *   NAME:  C-TIMEND
      *   DESC:  THIS ROUTINE SETS THE CORRECT SIGN ON ELAPSED
      *   IN  :
      *   OUT :
      * REV:
      *  JTG 050297 CHANGED FOR 998 ELAPSED UNIT PERIODS
      *********************************************************/
       C-TIMEND SECTION.
           COMPUTE WS-XDAYS = WS-XSIGN * WS-XDAYS.
           IF TA-YRTYPE = 998
              COMPUTE WS-XMONTHS = WS-XMONTHS * WS-XSIGN
              COMPUTE WS-XREM    = WS-XDAYS - (WS-XMONTHS * 30)
           ELSE
              DIVIDE WS-XDAYS BY 30 GIVING WS-XMONTHS
                                 REMAINDER WS-XREM.

           IF TA-YRTYPE = 365
              COMPUTE WS-XDAYS = WS-XSIGN * (WS-JUL-2 - WS-JUL-1).

           IF (WS-XSIGN < 0)
              MOVE WS-DATE2    TO WS-HOLDDATE
              MOVE WS-DATE1    TO WS-DATE2
              MOVE WS-HOLDDATE TO WS-DATE1
              MOVE WS-JUL-2    TO WS-HOLDJUL
              MOVE WS-JUL-1    TO WS-JUL-2
              MOVE WS-HOLDJUL  TO WS-JUL-1.

      *****************************************************************
      *          INCREMENT A DATE BY A NUMBER OF UNIT PERIODS
      *
      *   DESC:  INCREMENT NDTE-DATE BY NDTE-HOLD UNIT PERIODS;
      *          CORRECT DAY AND MONTH IF REQUIRED.
      *   IN  :  DATER-UNITPER-CD   = M, D, Y, S, W, B
      *          DATER-UNITPER-FREQ = FREQUENCY OF UNIT PERIODS
      *          NDTE-DATE          = MMDDYY TO INCREMENT
      *          NDTE-HOLD          = INCREMENT IN UNIT PERIODS (+ OR -)
      *   OUT :  NDTE-DATE = INCREMENTED DATE
      *****************************************************************

       C-INCREMENT-PERIODS SECTION.
           MOVE NDTE-MM TO WS-NDTE-MM-S.
           MOVE NDTE-DD TO WS-NDTE-DD-S.
           MOVE NDTE-CCYY TO WS-NDTE-CCYY-S.
           COMPUTE WS-UP-WORK = NDTE-HOLD * DATER-UNITPER-FREQ.

           EVALUATE DATER-UNITPER-CD
           WHEN "M"
                    PERFORM C-INCR-MONTHS
           WHEN "S"
                    PERFORM C-INCR-HALF-MONTHS
           WHEN "W"
                    COMPUTE WS-UP-WORK = WS-UP-WORK * 7
                    PERFORM C-INCR-DAYS
           WHEN "B"
                    COMPUTE WS-UP-WORK = WS-UP-WORK * 14
                    PERFORM C-INCR-DAYS
           WHEN "D"
                    PERFORM C-INCR-DAYS
           WHEN "Y"
                    COMPUTE WS-UP-WORK = WS-UP-WORK * 12
                    PERFORM C-INCR-MONTHS
           END-EVALUATE.

           MOVE WS-NDTE-MM-S TO NDTE-MM.
           MOVE WS-NDTE-DD-S TO NDTE-DD.
           MOVE WS-NDTE-CCYY-S TO NDTE-CCYY.
           MOVE "N" TO DATER-ACTION-CODE.
           PERFORM C-CALL-NEWDATE.

      *********************************************************
      *          INCREMENT DAYS TO A DATE
      *********************************************************
       C-INCR-DAYS SECTION.
           MOVE NUM-DATE TO SV-NUM-DATE.
           MOVE SYS-DATE TO SV-SYS-DATE.
           MOVE NDTE-DATE TO NUM-DATE.
           MOVE NUM-DATE TO P-DATE-IN.
           PERFORM C-CALL-JUL.
           MOVE P-JULIAN-DATE TO WS-JUL-WK.
           ADD WS-UP-WORK TO WS-JUL-WK.

           MOVE WS-JUL-WK       TO P-CJUL-JULIAN-DATE.
           PERFORM C-CALL-CJUL.
           MOVE P-CJUL-DATE-OUT TO NUM-DATE NDTE-DATE.

           MOVE NDTE-CCYY TO WS-NDTE-CCYY-S.
           MOVE NDTE-MM TO WS-NDTE-MM-S.
           MOVE NDTE-DD TO WS-NDTE-DD-S.

           MOVE SV-NUM-DATE TO NUM-DATE.
           MOVE SV-SYS-DATE TO SYS-DATE.

      *********************************************************
      *          INCREMENT DATES BY UNITS OF 1/2 MONTHS
      *          USES C-INCR-MONTHS BASE ROUTINE FOR IT
      *********************************************************
       C-INCR-HALF-MONTHS SECTION.
           MOVE WS-NDTE-DD-S TO WS-HOLD-DD.
           DIVIDE WS-UP-WORK BY 2 GIVING    WS-DIV-RESULT
                                  REMAINDER WS-MOD-2.
           MOVE WS-DIV-RESULT TO WS-UP-WORK.
           IF WS-UP-WORK NOT = 0
              PERFORM C-INCR-MONTHS.

           IF WS-MOD-2 > 0
              IF WS-NDTE-DD-S > 15
                 COMPUTE WS-NDTE-DD-S = WS-HOLD-DD - 15
                 MOVE 1 TO WS-UP-WORK
                 PERFORM C-INCR-MONTHS
              ELSE
                 ADD 15 TO WS-NDTE-DD-S
              END-IF
           ELSE
              IF WS-MOD-2 < 0
                 IF WS-NDTE-DD-S < 16
                    ADD 15 TO WS-NDTE-DD-S
                    MOVE -1 TO WS-UP-WORK
                    PERFORM C-INCR-MONTHS
                 ELSE
                    COMPUTE WS-NDTE-DD-S = WS-HOLD-DD - 15.

      *********************************************************
      *          INCREMENT MONTH ROUTINE
      *
      *          INCREMENTS A DATE BY THE NO.
      *          OF MONTHS FOUND IN INCREMENT.
      *          THE CORRECT LAST DAY OF MONTH IS ADJUSTED.
      *********************************************************
       C-INCR-MONTHS SECTION.

      * CHECK TO SEE HOW MANY YEARS & MONTHS TO ADVANCE

           DIVIDE WS-UP-WORK BY 12 GIVING    WS-DIV-RESULT
                                   REMAINDER WS-MOD-12.
           ADD WS-MOD-12     TO WS-NDTE-MM-S.
           ADD WS-DIV-RESULT TO WS-NDTE-CCYY-S.
           IF WS-NDTE-MM-S < 1
              ADD 12 TO WS-NDTE-MM-S
              ADD -1 TO WS-NDTE-CCYY-S
           ELSE
             IF WS-NDTE-MM-S > 12
                ADD -12 TO WS-NDTE-MM-S
                ADD 1 TO WS-NDTE-CCYY-S.

      * THIS PART WAS IGNORED FROM THE ORIGINAL C CODE NOT TO CHANGE
      * THE YEAR WHEN IT IS LESS THAN 50
      * CHANGED FOR 4 DIGIT YEAR AND CHANGED BACK TO 50
      *   ELSE IT DOES NOT MATCH A15 DON'T KNOW WHY EDWIN LEFT THIS OUT
      *    IF WS-NDTE-YY-S < 50
      *       MOVE 50 TO WS-NDTE-YY-S
      *    END-IF

      * COMMENTING OUT NOW THAT WE ARE USING FULL 8 DIGIT YEARS
      *     IF WS-NDTE-YY-S < 1950
      *        MOVE 50 TO WS-NDTE-YY-S
      *     END-IF

      * THIS PART WAS CHANGED FROM THE ORIGINAL C CODE TO LIMIT
      * THE YEAR RESULT TO 99 INSTEAD OF 149 BECAUSE THE OUTPUT
      * DATE FORMAT HAS 2 DIGITS ONLY
      * CHANGED FOR 4 DIGIT YEAR AND CHANGED BACK TO 49
      *   ELSE IT DOES NOT MATCH A15 NOT SURE WHY EDWIN CHANGED THIS
      *MJD IF WS-NDTE-YY-S > 99

      * COMMENTING OUT NOW THAT WE ARE USING FULL 8 DIGIT YEARS
      *    IF WS-NDTE-YY-S > 2049
      *       MOVE 49 TO WS-NDTE-YY-S
      *    END-IF

      * NOW CALL NEW-DATE
           IF WS-NDTE-DD-S > 99
              MOVE 99 TO NDTE-DD
           ELSE
              MOVE WS-NDTE-DD-S TO NDTE-DD.

           MOVE WS-NDTE-CCYY-S TO NDTE-CCYY.
           MOVE WS-NDTE-MM-S   TO NDTE-MM.
           MOVE "N" TO DATER-ACTION-CODE.
           PERFORM C-CALL-NEWDATE.
           MOVE NDTE-CCYY TO WS-NDTE-CCYY-S.
           MOVE NDTE-MM TO WS-NDTE-MM-S.
           MOVE NDTE-DD TO WS-NDTE-DD-S.

      **********************************************
      *  RETURN NUMBER OF DAY OF THE WEEK FOR A DATE
      *     1 - SUNDAY, 2 - MONDAY, 3 - TUESDAY, ETC.
      **********************************************
       C-WEEKDAY SECTION.
           MOVE NUM-DATE TO P-DATE-IN.
           PERFORM C-CALL-JUL.
           MOVE P-JULIAN-DATE TO WS-JUL-WK.
           COMPUTE WS-JUL = WS-JUL-WK   + 1.
           DIVIDE WS-JUL BY 7 GIVING WS-DIV-RESULT
                              REMAINDER WS-MOD-7.
           COMPUTE DAY--OF-WEEK = WS-MOD-7 + 1.

      **********************************************
      *  COMPARES TWO DATES
      *  DESC COMPARES SYS-DATE TO NUM-DATE
      *  MJD 130514 DO NOT CALL JUL IF DATES ARE ZERO
      *  MJD 140124 ADDED LOGIC TO ALTER ELAPSED RESULTS WHEN ZERO DATES
      *             ARE USED TO MATCH RESULTS GENERATEDIN A15.
      *         IN A15:
      *            IF SYS-DATE = 0
      *              IF NUM-YR < 50 THEN ELAPSED-RESULTS = "L"  FOR ALL MM/DD
      *              IF NUM-YR >= 50 THAN ELAPSED RESULTS = "G" FOR ALL MM/DD
      *              IF NUM-DATE = 123199 THEN ELAPSED RESULTS = "E"
      *
      *            IF NUM-DATE = 0
      *              IF SYS-YY < 50 THEN ELAPSED-RESULTS = "G" FOR ALL MM/DD
      *              IF SYS-YY >= 50 THAN ELAPSED RESULTS = "L" FOR ALL MM/DD
      *              IF SYS-DATE = 123199 THEN ELAPSED RESULTS = "E"
      * MJD 151110 FIXED FOR CCYY NO LONGER NEED SWING DATE TEST.
      **********************************************
       C-DATE-COMPARE SECTION.
      * NEW LOGIC ADDED 2017/02/02
            IF SYS-DATE = NUM-DATE
               MOVE "E" TO ELAPSED-RESULTS
            ELSE
            IF SYS-DATE > NUM-DATE
               MOVE "G" TO ELAPSED-RESULTS
            ELSE
               MOVE "L" TO ELAPSED-RESULTS.

      * OLD LOGIC COMMENTED OUT 2017/02/02
      * ===================================
      *     IF NUM-DATE = 0
      *        MOVE 0 TO WS-JUL-1
      *     ELSE
      *        MOVE NUM-DATE TO P-DATE-IN
      *        PERFORM C-CALL-JUL
      *        MOVE P-JULIAN-DATE TO WS-JUL-1
      *     END-IF.

      *     IF SYS-DATE = 0
      *        MOVE 0 TO WS-JUL-2
      *     ELSE
      *        MOVE SYS-DATE TO P-DATE-IN
      *        PERFORM C-CALL-JUL
      *        MOVE P-JULIAN-DATE TO WS-JUL-2
      *     END-IF.

      *     IF (NUM-DATE = 0 AND SYS-DATE = 0)
      **  NOTE SURE ABOUT THIS 123199 TEST.  LOOKS LIKE IT WAS ADDED TO 
      ** FIX A BUG WITH JULIAN WHERE 0 PASSED BACK SAME # AS 123199.
      **       (NUM-DATE = 0 AND SYS-DATE = 123199) OR
      **       (SYS-DATE = 0 AND NUM-DATE = 123199)
      *           MOVE "E" TO ELAPSED-RESULTS
      *     ELSE
      **       IF NUM-DATE = 0 AND S-YY < 50
      **          MOVE "G" TO ELAPSED-RESULTS
      **    ELSE
      **       IF NUM-DATE = 0 AND S-YY >= 50
      **          MOVE "L" TO ELAPSED-RESULTS
      **    ELSE
      **       IF SYS-DATE = 0 AND NUM-YR < 50
      **          MOVE "L" TO ELAPSED-RESULTS
      **    ELSE
      **       IF SYS-DATE = 0 AND NUM-YR >= 50
      **          MOVE "G" TO ELAPSED-RESULTS
      **    ELSE
      *        PERFORM C-TIM-RESULTS
      *        MOVE WS-TIM-RESULTS TO ELAPSED-RESULTS.
      * ===================================

      ***********************************************************
      *         DETERMINE THE RESULTS OF A DATE COMPARE
      *
      *   NAME: TIM_RESULTS
      *   DESC: DETERMINE IF DATE2 IS <, >, OR = TO DATE1
      *
      *   IN  : WS-JUL-1 AND WS-JUL-2
      *   OUT : WS-TIM-RESULTS
      ***********************************************************
       C-TIM-RESULTS SECTION.
           MOVE "L" TO WS-TIM-RESULTS.
           IF WS-JUL-2 > WS-JUL-1
              MOVE "G" TO WS-TIM-RESULTS
           ELSE
           IF WS-JUL-2 = WS-JUL-1
              MOVE "E" TO WS-TIM-RESULTS.

      ***********************************************************
      *   NAME: C-DATE-TEST
      *   DESC: IF A DATE IS VALID, RETURNS THE SAME
      *         DATE WHEN VALID, OR 0 WHEN NOT VALID
      *   IN  : NUM-DATE (CCYYMMDD)
      *   OUT : NUM-DATE (CCYYMMDD)
      *
      * MJD 141015 MADE SECTION
      ***********************************************************
       C-DATE-TEST SECTION.
           MOVE NUM-DATE TO WS-NDTE.
           IF (NOT WS-NDTE-MM-VALID) OR (NOT WS-NDTE-DD-VALID)
              MOVE 0 TO NUM-DATE
           ELSE
              MOVE NUM-DATE TO P-DATE-IN
              PERFORM C-CALL-JUL
              MOVE P-JULIAN-DATE TO WS-JUL-WK

              MOVE 0 TO NUM-DATE

              MOVE WS-JUL-WK       TO P-CJUL-JULIAN-DATE
              PERFORM C-CALL-CJUL
              MOVE P-CJUL-DATE-OUT TO NUM-DATE

              IF WS-NDTE NOT = NUM-DATE
                 MOVE 0 TO NUM-DATE
                 ELSE
                 MOVE WS-NDTE TO NUM-DATE
              END-IF
           END-IF.

      *********************************************************************
      *  NAME: C-CALL-JUL
      *  DESC: CONVERT GREGORIAN CALANDER DATE TO A JULIAN DATE
      *  IN  : DATE IN CCYYMMDD FORMAT
      *  OUT : JULIAN DATE FOR NUMBER OF DAYS SINCE 12/31/1899
      *********************************************************************
       C-CALL-JUL SECTION.
           MOVE P-DATE-IN TO JJUL-IN.
           COMPUTE JWS-JUL-IN-CCYY = JJUL-IN-CCYY - 1900.

           MOVE JJUL-IN-CCYY TO LEAP-YEAR-CCYY.
           PERFORM C-LEAP-YEAR-TEST.

           IF JJUL-IN-MM > 2 AND LEAP-YEAR-TRUE
              MOVE 1 TO JWS-EXTRA-DAY
           ELSE
              MOVE 0 TO JWS-EXTRA-DAY.

           COMPUTE P-JULIAN-DATE = ( (JWS-JUL-IN-CCYY - 1) * 1461 / 4 )
                                + DYTOM(JJUL-IN-MM) + JJUL-IN-DD +
                                JWS-EXTRA-DAY.

      *********************************************************************
      *  NAME: C-CALL-CJUL
      *  DESC: CONVERT JULIAN DATE TO A GREGORIAN CALANDER DATE
      *  IN  : JULIAN DATE FOR NUMBER OF DAYS SINCE 12/31/1899
      *  OUT : DATE IN CCYYMMDD FORMAT
      *********************************************************************
       C-CALL-CJUL SECTION.
           MOVE P-CJUL-JULIAN-DATE TO CJUL-IN.
           COMPUTE CJWS-JUL-BY4 = CJUL-IN * 4.
           DIVIDE CJWS-JUL-BY4 BY 1461 GIVING    CJWS-DIV-RESULT
                                     REMAINDER CJWS-MOD-JUL.
           IF CJWS-MOD-JUL NOT = 0
              MOVE 1 TO CJWS-MOD-JUL.

           COMPUTE WS-CJUL-OUT-CCYY = (CJUL-IN * 4 / 1461) +
                                                          CJWS-MOD-JUL.
           COMPUTE WS-CJUL-OUT-DD ROUNDED = CJUL-IN -
                                  ( (WS-CJUL-OUT-CCYY - 1) * 1461 / 4 ).
           IF WS-CJUL-OUT-DD <
              ( CJUL-IN - ( (WS-CJUL-OUT-CCYY - 1) * 1461 / 4 ) )
              ADD 1 TO WS-CJUL-OUT-DD.

           DIVIDE WS-CJUL-OUT-CCYY BY 4 GIVING    CJWS-DIV-RESULT
                                      REMAINDER CJWS-MOD-JUL.
           IF CJWS-MOD-JUL NOT > 0 AND WS-CJUL-OUT-DD NOT < 60
              MOVE 1 TO CJWS-WORK
           ELSE
              MOVE 0 TO CJWS-WORK.

           COMPUTE WS-CJUL-OUT-DD = WS-CJUL-OUT-DD - CJWS-WORK.
           MOVE 13 TO CJWS-MM.
           PERFORM UNTIL (DYTOM(CJWS-MM) < WS-CJUL-OUT-DD)
                                            OR (CJWS-MM = 1)
              ADD -1 TO CJWS-MM
           END-PERFORM.

           IF WS-CJUL-OUT-DD = 59
              ADD CJWS-WORK TO WS-CJUL-OUT-DD.

           SUBTRACT DYTOM(CJWS-MM) FROM WS-CJUL-OUT-DD.
           MOVE CJWS-MM TO WS-CJUL-OUT-MM.
           ADD 1900     TO WS-CJUL-OUT-CCYY.
           MOVE WS-CJUL-OUT-MM TO CJUL-OUT-MM.
           MOVE WS-CJUL-OUT-DD TO CJUL-OUT-DD.
           MOVE WS-CJUL-OUT-YY TO CJUL-OUT-YY.
           MOVE WS-CJUL-OUT-CC TO CJUL-OUT-CC.
           MOVE CJUL-OUT TO P-CJUL-DATE-OUT.

      *********************************************************************
      *  NAME: C-CALL-NEWDATE
      *  DESC: THIS ROUTINE RETURNS A VALID DATE,
      *        THE DAY IS ADJUSTED BACK TO THE LAST VALID
      *        DAY OF THAT MONTH.
      *   IN  : NDTE-DATE-WORK (CCYYMMDD)
      *   OUT : NDTE-DATE (CCYYMMDD)
      *
      * MJD UPDATED FOR 4 DIGIT YEAR.
      *********************************************************************
       C-CALL-NEWDATE SECTION.
           MOVE NDTE-DATE-WORK TO ND-WS-NDTE.
           MOVE ND-WS-NDTE-CCYY TO LEAP-YEAR-CCYY.
           PERFORM C-LEAP-YEAR-TEST.
           IF (ND-WS-NDTE-MM > 0 AND ND-WS-NDTE-MM < 13) AND
              (ND-WS-NDTE-DD >
                   ( DYTOM(ND-WS-NDTE-MM + 1) - DYTOM(ND-WS-NDTE-MM) ) )
              IF ND-WS-NDTE-MM NOT = 2
                 COMPUTE ND-WS-NDTE-DD = DYTOM(ND-WS-NDTE-MM + 1)
                                     - DYTOM(ND-WS-NDTE-MM)
              ELSE
                 IF LEAP-YEAR-TRUE
                    MOVE 29 TO ND-WS-NDTE-DD
                 ELSE
                    MOVE 28 TO ND-WS-NDTE-DD
                 END-IF
              END-IF
           END-IF.
           MOVE ND-WS-NDTE TO NDTE-DATE.

      * COMMENTED OUT MJD 2017/02/07
      * IT WAS ALREADY COMMENTED OUT EVERYWHERE IT WAS CALLED.
      * IT IS NOT NBEEDED WHEN USING 8 DIGIT YEAR IN CCYYMMDD FORMAT.
      * C-SET-WS-DATE-YYYY SECTION.
      *     IF WS-DATE1-YY NOT < EXT-JULIAN-CC
      *        ADD 1900 WS-DATE1-YY GIVING WS-DATE1-YYYY
      *     ELSE
      *        ADD 2000 WS-DATE1-YY GIVING WS-DATE1-YYYY.

      *     IF WS-DATE2-YY NOT < EXT-JULIAN-CC
      *        ADD 1900 WS-DATE2-YY GIVING WS-DATE2-YYYY
      *     ELSE
      *        ADD 2000 WS-DATE2-YY GIVING WS-DATE2-YYYY.

      * NOT NEEDED W/ 8 DIGIT YEAR
      *C-ADJ-WS-NDTE-YY-S SECTION.
      *    IF WS-NDTE-YY-S NOT < EXT-JULIAN-CC
      *       ADD 1900 TO  WS-NDTE-YY-S
      *    ELSE
      *       ADD 2000 TO WS-NDTE-YY-S.

      ***************************************************************
      * NAME: C-LEAP-YEAR-TEST
      * DESC: TEST A GIVEN YEAR YEAR TO DETERMINE IF IT A LEAP YEAR
      * IN  : LEAP-YEAR-CCYY
      * OUT : LEAP-YEAR-TRUE / LEAP-YEAR-FALSE
      *
      *
      * TO DETERMINE WHETHER A YEAR IS A LEAP YEAR, FOLLOW THESE STEPS:
      * 1. IF THE YEAR IS EVENLY DIVISIBLE BY 4, GO TO STEP 2.
      *           OTHERWISE, GO TO STEP 5.
      * 2. IF THE YEAR IS EVENLY DIVISIBLE BY 100, GO TO STEP 3.
      *           OTHERWISE, GO TO STEP 4.
      * 3. IF THE YEAR IS EVENLY DIVISIBLE BY 400, GO TO STEP 4.
      *           OTHERWISE, GO TO STEP 5.
      * 4. THE YEAR IS A LEAP YEAR (IT HAS 366 DAYS).
      * 5. THE YEAR IS NOT A LEAP YEAR (IT HAS 365 DAYS).
      ***************************************************************
       C-LEAP-YEAR-TEST SECTION.
           MOVE "F" TO LEAP-YEAR-FG.
           DIVIDE LEAP-YEAR-CCYY BY 4
                  GIVING ND-WS-DIV-RESULT REMAINDER ND-WS-MOD-4.
           IF ND-WS-MOD-4 NOT = 0
              MOVE "F" TO LEAP-YEAR-FG
           ELSE
              DIVIDE LEAP-YEAR-CCYY BY 100
                     GIVING ND-WS-DIV-RESULT REMAINDER ND-WS-MOD-4
              IF ND-WS-MOD-4 NOT = 0
                     MOVE "T" TO LEAP-YEAR-FG
              ELSE
                 DIVIDE LEAP-YEAR-CCYY BY 400
                     GIVING ND-WS-DIV-RESULT REMAINDER ND-WS-MOD-4
                 IF ND-WS-MOD-4 NOT = 0
                        MOVE "F" TO LEAP-YEAR-FG
                 ELSE
                        MOVE "T" TO LEAP-YEAR-FG
                 END-IF
              END-IF
           END-IF.

      ***************************************************************
      * NAME: CONVERT-00MMDDYY-TO-CCYYMMDD
      * DESC: CONVERT A DATE IN THE FORMATE OF 00MMDDYY TO CCYYMMDD
      * IN  : CONVERT-DATE (00MMDDYY WHERE CONVERT-DATE NOT = 0)
      * OUT : CONVERT-DATE (CCYYMMDD)
      ***************************************************************
       CONVERT-00MMDDYY-TO-CCYYMMDD SECTION.
           IF CONVERT-DATE-CC = 00 AND CONVERT-DATE NOT = 0
              MOVE CONVERT-DATE-DD TO CONVERT-DATE-CC
              MOVE CONVERT-DATE-MM TO CONVERT-DATE-DD
              MOVE CONVERT-DATE-YY TO CONVERT-DATE-MM
              MOVE CONVERT-DATE-CC TO CONVERT-DATE-YY
              IF CONVERT-DATE-YY < EXT-JULIAN-CC
                 MOVE 20 TO CONVERT-DATE-CC
              ELSE
                 MOVE 19 TO CONVERT-DATE-CC
              END-IF
           END-IF.

      ***************************************************************
      * NAME: CONVERT-CCYYMMDD-TO-00MMDDYY
      * DESC: CONVERT A DATE IN THE FORMATE OF CCYYMMDD TO 00MMDDYY
      * IN  : CONVERT-DATE (CCYYMMDD)
      * OUT : CONVERT-DATE (00MMDDYY)
      ***************************************************************
       CONVERT-CCYYMMDD-TO-00MMDDYY SECTION.
           MOVE CONVERT-DATE-YY TO CONVERT-DATE-CC.
           MOVE CONVERT-DATE-MM TO CONVERT-DATE-YY.
           MOVE CONVERT-DATE-DD TO CONVERT-DATE-MM.
           MOVE CONVERT-DATE-CC TO CONVERT-DATE-DD.
           MOVE 00              TO CONVERT-DATE-CC.
     

      ***************************************************************
      * NAME: CONVERT-SYS-NUM-NDTE-DATES-IN
      * DESC: CONVERT SYS/NUM/NDTE-DATE DATE FROM 00MMDDYY TO CCYYMMDD
      *       USED STRICTLY FOR TESTING WHILE INCEMENTALLY CHANING DATE
      *       FIELDS AND FORMATS.
      *       ALLOWS INTERNAL USAGE OF CCYYMMDD WHILE EXTERNAL FORMAT
      *       CAN BE CCYYMMDD OR 00MMDDYY
      * IN  : SYS-DATE (00MMDDYY)
      *       NUM-DATE (00MMDDYY)
      *       NDTE-DATE (00MMDDYY)
      * OUT : SYS-DATE (CCYYMMDD)
      *       NUM-DATE (CCYYMMDD)
      *       NDTE-DATE (CCYYMMDD)
      ***************************************************************
       CONVERT-SYS-NUM-NDTE-DATES-IN SECTION.
           MOVE "N" TO SYS-FG NUM-FG NDTE-FG.
           IF S-CC = 0 AND SYS-DATE NOT = 0
              MOVE SYS-DATE TO CONVERT-DATE
              PERFORM CONVERT-00MMDDYY-TO-CCYYMMDD
              MOVE CONVERT-DATE TO SYS-DATE
              MOVE "Y" TO SYS-FG.

           IF NUM-CC = 0 AND NUM-DATE NOT = 0
              MOVE NUM-DATE TO CONVERT-DATE
              PERFORM CONVERT-00MMDDYY-TO-CCYYMMDD
              MOVE CONVERT-DATE TO NUM-DATE
              MOVE "Y" TO NUM-FG.

           IF NDTE-CC = 0 AND NDTE-DATE NOT = 0
              MOVE NDTE-DATE TO CONVERT-DATE
              PERFORM CONVERT-00MMDDYY-TO-CCYYMMDD
              MOVE CONVERT-DATE TO NDTE-DATE
              MOVE "Y" TO NDTE-FG.

      ***************************************************************
      * NAME: CONVERT-SYS-NUM-NDTE-DATES-OUT
      * DESC: CONVERT SYS/NUM/NDTE-DATE DATE FROM 00MMDDYY TO CCYYMMDD
      *       USED STRICTLY FOR TESTING WHILE INCEMENTALLY CHANING DATE
      *       FIELDS AND FORMATS.
      *       ALLOWS INTERNAL USAGE OF CCYYMMDD WHILE EXTERNAL FORMAT
      *       CAN BE CCYYMMDD OR 00MMDDYY
      * IN  : SYS-DATE (CCYYMMDD)
      *       NUM-DATE (CCYYMMDD)
      *       NDTE-DATE (CCYYMMDD)
      * OUT : SYS-DATE (00MMDDYY)
      *       NUM-DATE (00MMDDYY)
      *       NDTE-DATE (00MMDDYY)
      ***************************************************************
       CONVERT-SYS-NUM-NDTE-DATES-OUT SECTION.
           IF SYS-FG = "Y"
              MOVE SYS-DATE TO CONVERT-DATE
              PERFORM CONVERT-CCYYMMDD-TO-00MMDDYY
              MOVE CONVERT-DATE TO SYS-DATE.

           IF NUM-FG = "Y"
              MOVE NUM-DATE TO CONVERT-DATE
              PERFORM CONVERT-CCYYMMDD-TO-00MMDDYY
              MOVE CONVERT-DATE TO NUM-DATE.

           IF NDTE-FG = "Y"
              MOVE NDTE-DATE TO CONVERT-DATE
              PERFORM CONVERT-CCYYMMDD-TO-00MMDDYY
              MOVE CONVERT-DATE TO NDTE-DATE.
