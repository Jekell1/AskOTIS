      ******************************************************************
      * EARLY PROVISION FOR SERVICE CHARGE ILLINOIS WORLD PR#733
      *
      *   1/60TH FOR EACH DAY FROM DATE OF PREPAYMENT TO THE SIXTIETH
      *   DAY OF LOAN (PRO RATA)
      ******************************************************************
       REBATE-RBEARLY-26 SECTION.
           MOVE LN-LOANDATE TO NUM-DATE.
           MOVE REB-PAYDATE TO SYS-DATE.
           PERFORM TIM365.
           IF REB-LPTRCD = "PO"
              IF ELAPSED-DAYS < 60
      *       ----------------------------------------------------------
      *       REBATE = UNELAPSED DAYS
      *                * ( SERVICE CHARGE - DEDUCT PRECHARGE ) / 60
      *       ----------------------------------------------------------
                   PERFORM REBATE-DEDUCT-CHARGE
                   COMPUTE REB-REBATE ROUNDED =
                        ( 60 - ELAPSED-DAYS )
                         * ( REB-TOTCHG - REB-DEDCHG ) / 60
              ELSE
      *       ----------------------------------------------------------
      *       IF MORE THAN 60 DAYS HAVE ELAPSED THEN THERE IS NO REFUND
      *       ----------------------------------------------------------
                   MOVE 0 TO REB-REBATE
           ELSE
               MOVE 0 TO REB-REBATE.

       REBATE-RBEARLY-26-EXIT.
           EXIT.

      ********************************************************************
      * CHECK TO SEE IF THE THE INSURANCE IN QUESTION WAS PUT ON AS AN
      * ADDON OR AS PART OF THE ORIGINAL LOAN.
      * IF NO LT INS RECORDS EXIST THEN IT WAS PUT ON WITH THE LOAN ELSE IT
      * WAS ADDED, ADJUSTED, CANCELLED AFTER THE LOAN WAS BOOKED.
      * 71105#0918
      ********************************************************************
