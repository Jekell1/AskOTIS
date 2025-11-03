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
