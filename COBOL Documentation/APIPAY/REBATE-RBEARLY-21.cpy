      ******************************************************************
      * EARLY PROVISION FOR ACQUISITION CHARGES - OKLAHOMA    (REQ #163)
      *
      *   1/60TH FOR EACH DAY FROM DATE OF PREPAYMENT TO THE SIXTIETH
      *   DAY OF LOAN (PRO RATA)
      ******************************************************************
       REBATE-RBEARLY-21 SECTION.
           MOVE LN-LOANDATE TO NUM-DATE.
           MOVE REB-PAYDATE TO SYS-DATE.
           PERFORM TIM365.
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
              MOVE 0 TO REB-REBATE.

       REBATE-RBEARLY-21-EXIT.
           EXIT.

      ******************************************************************
      * EARLY PROVISION FOR SERVICE CHARGE ILLINOIS WORLD PR#733
      *
      *   1/60TH FOR EACH DAY FROM DATE OF PREPAYMENT TO THE SIXTIETH
      *   DAY OF LOAN (PRO RATA)
      ******************************************************************
