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
