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
