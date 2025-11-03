      **************************************************
      *    GET ALL LATE CHARGES DUE THRU PAYOFF DATE:
      **************************************************
       PAYOFF-SET-LATE-CHARGES SECTION.
           MOVE POFF-PAYDATE                TO LCAP-PAYDATE.
           MOVE LN-1STPYDATE                TO LCAP-1STPYDATE.
           MOVE "AL"                        TO LCAP-LPTRCD.
           MOVE POFF-LPTRCD                 TO LCAP-POFF-LPTRCD.
           MOVE POFF-LCAP-BATCH-REFCD       TO LCAP-BATCH-REFCD.
           MOVE POFF-LCAP-BATCH-REFCD-LC-FG TO LCAP-BATCH-REFCD-LC-FG.
           MOVE 0                           TO LCAP-LPAPLC
                                               LCAP-LPAPINT.
           PERFORM LATE-CHARGE-APPLY.
           MOVE LCAP-PARTIALS               TO POFF-PARTIALS.
           MOVE LCAP-LCPAID                 TO POFF-LCPAID.
           ADD LN-LCBAL LCAP-OWE GIVING POFF-LCDUE.
           ADD LCAP-OWE                     TO POFF-NETDUE.

      ****************************************************
      *    GET ALL INTEREST CHARGES DUE THRU PAYOFF DATE:
      *    NOTE:
      *          NEW FIELDS SHOULD HAVE BEEN CREATED FOR
      *          THE LINK BETWEEN CL/LFPPO.C AND LPPOFF
      *          BUT THIS WOULD FORSE A RECOMPILE, SO
      *          POFF-O5-INT-REB WAS USED. JTG 010925
      ****************************************************
