      ************************************************
      *    PREPARE-PMT-2 ROUTINE
      ************************************************
       LPRATE-PREPARE-PMT-2 SECTION.
           MOVE INDU-INTEREST TO RATE-EXP-INT INDU-INTEREST-2.
           PERFORM LPRATE-SET-INTPDTH-DATE.
           MOVE INDU-DATE-1 TO NUM-DATE.
           MOVE XDTE-DATE TO SYS-DATE.
           IF SYS-DATE > NUM-DATE
              MOVE XDTE-DATE TO INDU-DATE-1.
           COMPUTE INDU-CURBAL = LN-CURBAL - LN-OT2BAL.
           MOVE RATE-LPTRCD TO INDU-LPTRCD.
           PERFORM LPRATE-EXPIRED-INT.
           IF (IBPC-STAT = "J" OR IBPC-STAT = "N") AND
              (IBPC-STAT-2 = "E" AND SP-EXPR-FRMLA NOT = "C")
              MOVE " " TO IBPC-STAT-2
              MOVE INDU-INTEREST-2 TO RATE-EXP-INT
              MOVE INDU-INTEREST TO INDU-INTEREST-2
              MOVE 0 TO INDU-INTEREST
              PERFORM LPRATE-COMPARE-RATES
              PERFORM INTEREST-DUE-CALCULATION
              IF (INDU-INTEREST-2 < INDU-INTEREST + RATE-EXP-INT)
                 MOVE INDU-INTEREST-2 TO INDU-INTEREST
              ELSE
                 ADD RATE-EXP-INT TO INDU-INTEREST.

      ************************************************
      *    COMPARE RATES ROUTINE
      ************************************************
