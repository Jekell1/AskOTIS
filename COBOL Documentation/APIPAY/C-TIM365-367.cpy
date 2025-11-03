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
