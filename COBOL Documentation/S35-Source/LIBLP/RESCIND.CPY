      * COPYMEMBER: LIBLP/RESCIND
      ******************************************************************
      *
      *         R E S C I S S I O N - R O U T I N E S
      *
      ******************************************************************
      *   NAME: RESCIND
      *   DESC: CALCULATE RESCISSION (DATES)
      *
      *   IN  : RESCIND-LOANDATE; GB-HOLIDAY(1-16)
      *
      *   NOTE: MUST HAVE READ GB-FILE PRIOR TO CALLING COPY MEMBER;
      *         MUST HAVE READ SP-FILE PRIOR TO CALLING COPY MEMBER.
      *
      *   OUT : RESCIND-DATE(1-9); RESCIND-1STDATE, RESCIND-LASTDATE,
      *         RESCIND-OUTDATE.
      *
      *-----------------------------------------------------------------
      *   REV:
      *   KEC 031297 ADDED SP-RESCIND-SATURDAY; PR#459.
      *   KEC 021199 CHANGED SP-RESCIND-BUS-DAYS-VALID FROM A MAX OF
      *               5 DAYS TO A MAX OF 8 DAYS.       REGENCY PR# 00015
      *
      ******************************************************************
      *
      *    R E S C I N D - C A L C U L A T I O N
      *
      *-----------------------------------------------------------------
      *    DRIVER TO CALCULATE THE: RESCISSION DATES;
      ******************************************************************
       RESCIND-CALCULATION SECTION.

           PERFORM RESCIND-CALC-DATES.

      *=================================================================
       EXIT-RESCIND-CALCULATION.
           EXIT.
      ******************************************************************
      *
      *    R E S C I N D - C A L C - D A T E S
      *
      *-----------------------------------------------------------------
      *    THIS WILL FIND DATES NEEDED PER RESCISSION; 1ST, LAST,
      *    OUT OF RESCISSION.  THIS WILL GO BY THE NUMBER OF BUSINESS
      *    DAYS (RESCIND-BUSINESS), WITH NO SATURDAYS OR SUNDAYS, AND NO
      *    HOLIDAYS (GB-HOLIDAYS).
      ******************************************************************
       RESCIND-CALC-DATES SECTION.
      *-----------------------------------------------------------------
       RESCIND-SETUP-DATES.
      *    STOP SYS-DATE.

           MOVE SP-RESCIND-BUS-DAYS TO RESCIND-BUSINESS-X.
           MOVE RESCIND-LOANDATE    TO RESCIND-WORKDATE.
           MOVE ZEROES              TO RESCIND-DATE(1) RESCIND-DATE(2)
                                       RESCIND-DATE(3) RESCIND-DATE(4)
                                       RESCIND-DATE(5) RESCIND-DATE(6)
                                       RESCIND-DATE(7) RESCIND-DATE(8)
                                       RESCIND-DATE(9)
                                       RESCIND-1STDATE RESCIND-LASTDATE
                                       RESCIND-OUTDATE RESCIND-DAY-SUB.

           IF ( RESCIND-BUSINESS NOT NUMERIC )
              GO TO EXIT-RESCIND-CALC-DATES.

      *=================================================================
       RESCIND-FIND-DATES.

           IF ( RESCIND-DAY-SUB >= (RESCIND-BUSINESS + 1))
              GO TO END-RESCIND-CALC-DATES.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    INCREMENTING THE DAY.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

           MOVE RESCIND-WORKDATE TO NDTE-DATE.
           MOVE 1                TO NDTE-HOLD.
           PERFORM INCREMENT-DAYS.
           MOVE NDTE-DATE        TO RESCIND-WORKDATE RESCIND-EDITDATE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    VERIFY VALID BUSINESS DAY.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

           MOVE RESCIND-WORKDATE TO NUM-DATE.
           PERFORM WEEKDAY.

           IF ( DAY-IS-SAT ) AND ( NOT SP-RESCIND-SATURDAY-YES )
              GO TO RESCIND-FIND-DATES.

           IF ( DAY-IS-SUN )
              GO TO RESCIND-FIND-DATES.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    VERIFY NOT A HOLIDAY.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

           PERFORM RESCIND-FIND-HOLIDAY.

           IF ( RESCIND-HOLIDAY-FLAG = "Y" )
              GO TO RESCIND-FIND-DATES.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    VALID DAY ( NOT A SATURDAY, SUNDAY, OR A HOLIDAY )
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

           ADD  1                TO RESCIND-DAY-SUB.
           MOVE RESCIND-WORKDATE TO RESCIND-DATE(RESCIND-DAY-SUB).
           GO TO RESCIND-FIND-DATES.

      *=================================================================
       END-RESCIND-CALC-DATES.

           MOVE RESCIND-DATE(          1         ) TO RESCIND-1STDATE.
           MOVE RESCIND-DATE(RESCIND-BUSINESS    ) TO RESCIND-LASTDATE.
           MOVE RESCIND-DATE(RESCIND-BUSINESS + 1) TO RESCIND-OUTDATE.

      *-----------------------------------------------------------------
       EXIT-RESCIND-CALC-DATES.
           EXIT.
      ******************************************************************
      *
      *    R E S C I N D - F I N D - H O L I D A Y
      *
      *-----------------------------------------------------------------
      *    COMPARING THE CURRENT INCREMENTED DATE IS = TO A HOLIDAY(GB).
      ******************************************************************
       RESCIND-FIND-HOLIDAY SECTION.
      *-----------------------------------------------------------------
       RESCIND-SETUP-HOLIDAY.

           MOVE "N" TO RESCIND-HOLIDAY-FLAG.
           MOVE 1   TO RESCIND-HOLIDAY-SUB.

      *=================================================================
       RESCIND-HOLIDAY-LOOP.

           IF ( RESCIND-HOLIDAY-SUB > 16 )
              GO TO EXIT-RESCIND-FIND-HOLIDAY.

           IF ( GB-HOLIDAY(RESCIND-HOLIDAY-SUB) = ZEROES )
              GO TO EXIT-RESCIND-FIND-HOLIDAY.

           IF ( GB-HOLIDAY(RESCIND-HOLIDAY-SUB) = RESCIND-EDITMMDD )
              MOVE "Y" TO RESCIND-HOLIDAY-FLAG
              GO TO EXIT-RESCIND-FIND-HOLIDAY.

           ADD 1 TO RESCIND-HOLIDAY-SUB.
           GO TO RESCIND-HOLIDAY-LOOP.

      *=================================================================
       EXIT-RESCIND-FIND-HOLIDAY.
           EXIT.
