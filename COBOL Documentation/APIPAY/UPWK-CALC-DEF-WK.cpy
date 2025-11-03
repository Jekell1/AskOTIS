      *****************************************************************
      *    CALCULATE UNIT PERIOD WORKER FOR DEFERMENTS
      *****************************************************************
       UPWK-CALC-DEF-WK SECTION.
           PERFORM UPWK-GET-UPER-INFO.
           IF UPWK-ERRCD = " "
              COMPUTE UPWK-UNITPER-WK =
                             UPWK-UNITPER-PER-YEAR / 12.

      ************************************************************************
      *         CALCULATE CONTRACTUAL ADJUSTMENT FOR UNIT PERIODS
      *         FROM ESTABLISHED LOAN RECORD
      *
      *   NAME: LPUPWK
      *   DESC: DETERMINE AJUSTMENT FOR CONTRACTUAL USAGE
      *
      *   IN  : UPWK-UNITPER-CD         (LN-UNITPER-CD)
      *         UPWK-UNITPER-FREQ       (LN-UNITPER-FREQ)
      *
      *   OUT : UPWK-UNITPER-WK
      *         UPWK-ERRCD
      *
      *   COPY:
      * REV:
      *  MLD 071696 UNCOMMENTED CODE TO WORK WITH WEEKLY (A50 CONV)
      *  JTG 071696 ADDED SP-BANKUNITPER-CD & SP-TRWBANKUNITPER-CD = "B"
      *             (FL AUTO)
      *  JTG 090897 CORRECTED TEST ON SP-BANKUNITPER-CD TO INCLUDE 'C' & 'D'
      ************************************************************************
