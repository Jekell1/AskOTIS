      *****************************************************************
      *    CALCULATE UNIT PERIOD WORKER FOR CONTRACTUAL
      *****************************************************************
       UPWK-CALC-CONTR-WK SECTION.
           PERFORM UPWK-GET-UPER-INFO.
           IF UPWK-ERRCD = " "
              COMPUTE UPWK-UNITPER-CONTR-WK =
                             UPWK-UNITPER-PER-YEAR / 12.

      *****************************************************************
      *         CALCULATE DEFERMENT ADJUSTMENT FOR UNIT PERIODS
      *         FROM ESTABLISHED LOAN RECORD
      *
      *   NAME: LPUPWK
      *   DESC: DETERMINE AJUSTMENT FOR LATE CHARGE USAGE
      *
      *   IN  : UPWK-UNITPER-CD         (LN-UNITPER-CD)
      *         UPWK-UNITPER-FREQ       (LN-UNITPER-FREQ)
      *
      *   OUT : UPWK-UNITPER-WK
      *         UPWK-ERRCD
      *
      *   COPY:
      * REV:
      *  071696 UNCOMMENTED CODE TO WORK WITH WEEKLY (A50 CONV)
      *****************************************************************
