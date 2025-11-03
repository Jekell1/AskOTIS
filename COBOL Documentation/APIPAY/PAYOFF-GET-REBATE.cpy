      **********************************
      *    GET THE REBATE:
      **********************************
       PAYOFF-GET-REBATE SECTION.
           MOVE POFF-PAYDATE TO REB-PAYDATE.
           MOVE POFF-LPTRCD TO REB-LPTRCD.

           MOVE 0 TO REB-SUB2.
      * CL:
           IF REB-SUB < 5
              MOVE 1 TO REB-SUB2
           ELSE
      * AH&PP:
           IF REB-SUB < 7
              SUBTRACT 3 FROM REB-SUB GIVING REB-SUB2
           ELSE
      * O1 & O2 & O3:
           IF REB-SUB = 12 OR 13 OR 14 OR 15 OR 16
              SUBTRACT 8 FROM REB-SUB GIVING REB-SUB2.

           IF REB-SUB2 NOT = 0
              MOVE REB-SUB2 TO ADDONSPR-SUB
              PERFORM FIND-ADDON-SPR
              PERFORM REBATE
              PERFORM FIND-ADDON-SPR-RESTORE
           ELSE
              PERFORM REBATE.

           IF REB-SUB = 8
              MOVE REB-EP54-AMT TO POFF-LXR-REB-AMT
           END-IF.
           
           SUBTRACT REB-REBATE FROM POFF-NETDUE.

      **********************************
      *    CALCULATE SERCHG REBATE:
      **********************************
