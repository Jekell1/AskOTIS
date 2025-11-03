      *********************************************************
       C-DATER-MAIN SECTION.
      *********************************************************
           PERFORM CONVERT-SYS-NUM-NDTE-DATES-IN.
           MOVE DATER-ACTION-CODE TO SV-DATER-ACTION-CODE.

           EVALUATE SV-DATER-ACTION-CODE
           WHEN "E" PERFORM C-ELAPSED-TIME
           WHEN "N"
      *             OK CCYY
                    PERFORM C-CALL-NEWDATE
           WHEN "I"
                    PERFORM C-INCREMENT-PERIODS
           WHEN "C"
      *             OK CCYY
                    MOVE JULIAN-DATE     TO P-CJUL-JULIAN-DATE
                    PERFORM C-CALL-CJUL
                    MOVE P-CJUL-DATE-OUT TO NUM-DATE
           WHEN "J"
      *             OK CCYY
                    MOVE NUM-DATE      TO P-DATE-IN
                    PERFORM C-CALL-JUL
                    MOVE P-JULIAN-DATE TO JULIAN-DATE
           WHEN "W"
      *             OK CCYY
                    PERFORM C-WEEKDAY
           WHEN "D"
      *             OK CCYY
                    PERFORM C-DATE-COMPARE
           WHEN "T"
                    PERFORM C-DATE-TEST
           END-EVALUATE
           MOVE SV-DATER-ACTION-CODE TO DATER-ACTION-CODE
           PERFORM CONVERT-SYS-NUM-NDTE-DATES-OUT.

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
