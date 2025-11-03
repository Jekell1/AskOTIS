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
