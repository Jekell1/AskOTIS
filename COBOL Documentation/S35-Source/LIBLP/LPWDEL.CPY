      * COPYMEMBER: LIBLP/LPWDEL
      *********************************************************************
      *         COMPUTE TOTAL WAGE ASSIGNMENT CONTRACTUAL DELINQUENCY
      *                 AND TOTAL ARREARAGE
      *         (INCLUDING INT AND LATE CHGS IF APPLICABLE)
      *
      *   NAME: LPWDEL
      *   DESC: COMPUTES TOTAL WAGE ASSIGNMENT CONTRACTUAL DELINQUECY
      *                  AND TOTAL ARREARAGE
      *         IN DOLLARS, INCLUDING PRINCIPAL, INTEREST,
      *         AND LATE CHARGES (WHEN APPLICABLE).
      *         CONTRAST WITH LPCDEL.IN.C
      *
      *   IN  : WDEL-1STPYDATE        LN-1STPYDATE
      *         WDEL-PAYDATE          TODAYS DATE
      *         WDEL-MAKERCD          LN-MAKERCD(X)
      *
      *   OUT : WDEL-DELINQUENT
      *         WDEL-ARREARAGE
      *               +  PAID AHEAD
      *               0  CURRENT
      *               -  DELINQUENT
      *   COPY: LIBLP/LPWDELW
      * REV:
      *   CS 20220110 CHANGED WDEL-MAKERCD FROM S999 TO PIC X TO MATCH
      *               ??-MAKERCD EVERYWHERE ELSE IT IS DEFINED SUCH AS
      *               CEPP-MAKERCD, CRNOR-MAKERCD, RATE-MAKERCD, ETC
      *********************************************************************
       WAGE-ASSIGN-DELINQ SECTION.
           MOVE 0 TO WDEL-DELINQUENT WDEL-ARREARAGE.
           IF LN-CURBAL = 0
              GO TO WAGE-ASSIGN-DELINQ-EXIT.

           MOVE WDEL-PAYDATE TO NDTE-DATE.
           MULTIPLY -1 BY SP-WAGE-ASSIGN-DAYS GIVING NDTE-HOLD.
           PERFORM INCREMENT-DAYS.

           MOVE NDTE-DATE      TO CEPP2-DATE.
           MOVE WDEL-1STPYDATE TO CEPP1-DATE.
           MOVE WDEL-MAKERCD   TO CEPP-MAKERCD.
           PERFORM TOTAL-DELINQUENCY.
           MOVE CDEL-DELINQUENT TO WDEL-DELINQUENT.
           MOVE CDEL-ARREARAGE  TO WDEL-ARREARAGE.

       WAGE-ASSIGN-DELINQ-EXIT.
           EXIT.
