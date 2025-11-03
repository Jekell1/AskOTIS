      *****************************************************************
      *          INCREMENT A DATE BY A NUMBER OF UNIT PERIODS
      *
      *   DESC:  INCREMENT NDTE-DATE BY NDTE-HOLD UNIT PERIODS;
      *          CORRECT DAY AND MONTH IF REQUIRED.
      *   IN  :  DATER-UNITPER-CD   = M, D, Y, S, W, B
      *          DATER-UNITPER-FREQ = FREQUENCY OF UNIT PERIODS
      *          NDTE-DATE          = YYYYMMDD TO INCREMENT
      *          NDTE-HOLD          = INCREMENT IN UNIT PERIODS (+ OR -)
      *   OUT :  NDTE-DATE = INCREMENTED DATE
      * REV:
      * BLM 100208 ADD CONVERT-YYMMDD-TO-YYYYMMDD
      *****************************************************************
       INCREMENT-UNITPER SECTION.
           MOVE "I" TO DATER-ACTION-CODE.
           PERFORM DATER-ROUTINE.

      *********************************************************
