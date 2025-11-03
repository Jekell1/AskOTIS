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
