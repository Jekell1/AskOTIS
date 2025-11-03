      ****************************************************
      *          INCREMENT A DATE BY A NUMBER OF MONTHS
      *
      *   DESC:  INCREMENT NDTE-DATE BY NDTE-HOLD MONTHS;
      *          CORRECT DAY AND MONTH IF REQUIRED.
      *   IN  :  NDTE-DATE = YYYYMMDD TO INCREMENT
      *          NDTE-HOLD = INCREMENT IN MONTHS (+ OR -)
      *   OUT :  NDTE-DATE = INCREMENTED DATE
      * REV:
      ****************************************************
       INCREMENT-MONTHS SECTION.
           MOVE DATER-UNITPER-INFO TO HOLD-UNITPER-INFO.
           MOVE "M" TO DATER-UNITPER-CD.
           PERFORM INCREMENT-COMMON.

      ****************************************************
      *          INCREMENT A DATE BY A NUMBER OF WEEKS
      *
      *   DESC:  INCREMENT NDTE-DATE BY NDTE-HOLD WEEKS;
      *          CORRECT DAY AND MONTH IF REQUIRED.
      *   IN  :  NDTE-DATE = YYYYMMDD TO INCREMENT
      *          NDTE-HOLD = INCREMENT IN WEEKS (+ OR -)
      *   OUT :  NDTE-DATE = INCREMENTED DATE
      * REV:
      ****************************************************
