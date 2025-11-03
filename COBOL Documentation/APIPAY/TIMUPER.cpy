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
