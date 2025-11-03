      *****************************************************************
      *    CALCULATE UNIT PERIOD WORKERS FOR LATE CHARGES
      *****************************************************************
       UPWK-CALC-LC-WK SECTION.
           PERFORM UPWK-GET-UPER-INFO.
           IF UPWK-ERRCD = " "
              COMPUTE UPWK-LC-PER-FAC
                      UPWK-LC-MAX-FAC
                      UPWK-LC-GRACE-FAC = 12 / UPWK-UNITPER-PER-YEAR.


      *****************************************************************
      *         GET INFORMATION ON UNIT PERIODS
      *         FROM ESTABLISHED LOAN RECORD
      *
      *   NAME: LPUPWK
      *   DESC: DETERMINE NO. OF UNIT PERIODS PER YEAR
      *                   NO. OF DAYS IN THE UNIT PERIOD
      *
      *   IN  : UPWK-UNITPER-CD         (LN-UNITPER-CD)
      *         UPWK-UNITPER-FREQ       (LN-UNITPER-FREQ)
      *
      *   OUT : UPWK-UNITPER-PER-YEAR
      *         UPWK-DAYS-IN-UNITPER
      *         UPWK-ERRCD
      *
      *   COPY:
      * REV:
      *  071696 UNCOMMENTED CODE TO WORK WITH WEEKLY (A50 CONV)
      *****************************************************************
