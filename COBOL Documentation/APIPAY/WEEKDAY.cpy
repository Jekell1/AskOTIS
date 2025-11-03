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
