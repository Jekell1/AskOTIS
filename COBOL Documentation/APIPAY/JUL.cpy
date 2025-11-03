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
