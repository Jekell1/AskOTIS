      ****************************************************
      *    GET ALL INTEREST CHARGES DUE THRU PAYOFF DATE:
      *    NOTE:
      *          NEW FIELDS SHOULD HAVE BEEN CREATED FOR
      *          THE LINK BETWEEN CL/LFPPO.C AND LPPOFF
      *          BUT THIS WOULD FORSE A RECOMPILE, SO
      *          POFF-O5-INT-REB WAS USED. JTG 010925
      ****************************************************
       PAYOFF-SET-INT-CHARGES SECTION.
           IF SP-CAL-RATETYPE(1) = "Z"
              MOVE "CL/LFPPO" TO FORM-NAM
              CALL FORM-PROGX USING FORM-PATH
                                   POFF-WORKERS
                                   LN-REC
                                   SP-REC
              CANCEL FORM-PROGX
              MOVE POFF-INTDUE     TO INDU-INTEREST
              MOVE POFF-O5-INT-REB TO INDU-PENALTY
              MOVE 0               TO POFF-O5-INT-REB
           ELSE
              MOVE POFF-PAYDATE TO RATE-DATE IBPC-DATE
              MOVE POFF-MAKERCD TO RATE-MAKERCD
              MOVE POFF-LPTRCD TO RATE-LPTRCD
              PERFORM SET-PY-RATE.

      * WORLD DOES NOT USE. COMMENTED 5/17/2023
      *    TEST FOR CALIFORNIA RECAST:
      *    IF SP-RBRECAST(7) = 2
      *       IF LN-OVRDRATE NOT = 999.999
      *          MOVE LN-ORIG-1STPYDATE TO NDTE-DATE
      *          MOVE 2 TO NDTE-HOLD
      *          MOVE LN-UNITPER-CD TO DATER-UNITPER-CD
      *          MOVE LN-UNITPER-FREQ TO DATER-UNITPER-FREQ
      *          PERFORM INCREMENT-UNITPER
      *          IF POFF-PAYDATE NOT > NDTE-DATE
      *             MOVE "C" TO POFF-FORM-PROG-NO
      *             ADD LN-INTBAL INDU-INTEREST
      *                             GIVING POFF-ORIG-INTDUE
      *             PERFORM POFF-CALL-SUBPROG
      *             IF POFF-COMMON-RTNCD = " "
      *                SUBTRACT POFF-COMMON-INTADJ
      *                                  FROM INDU-INTEREST.

      * CALIFORNIA "CA" STATE LAW  1/15/2013 MULLEN & COSMO
      * WHEN AN ACCOUNT IS PAID IN FULL (REGARDLESS OF THE PAYOFF METHOD)
      * ON OR BEFORE THE 3RD INSTALLMENT DUE DATE (CURRENT 1ST PAY DUE DATE
      * PLUS 2 MONTHS) BASED ON A 360 DAY YEAR TYPE, AS FOLLOWS:

           IF SP-RBSPOPT1(7) = 27
              PERFORM CALIFORNIA-SPECIAL-OPTION.

           ADD LN-INTBAL INDU-INTEREST GIVING POFF-INTDUE.
           ADD INDU-INTEREST TO POFF-NETDUE.

      **********************************
      *    GET THE REBATE:
      **********************************
