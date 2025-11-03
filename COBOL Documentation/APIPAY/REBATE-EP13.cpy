      *******************************************************
      * MERCURY, NEVADA EARLY REFUND PROVISION
      * WHEN PAYOFF IN DONE BY CUSTOMER REFUND IS
      * BASED ON THE ACTION DATA SHORT RATE TABLE:
      *******************************************************
       REBATE-EP13 SECTION.
           IF REB-LPTRCD = "PB" OR "RN" OR "SC"
                              OR "RB" OR "RO"
              GO TO REBATE-EP13-EXIT.

           MOVE "T" TO REB-FORM-PROG-TYPE.
           MOVE 02 TO REB-FORM-PROG-NO.
           MOVE REB-REFDATE TO NUM-DATE.
           MOVE REB-PAYDATE TO SYS-DATE.
           PERFORM TIM365.
           MOVE ELAPSED-DAYS TO T02-ELAPSED-DAYS.
           MOVE 3 TO ITRM-SUB.
           PERFORM INS-TERM-CALCULATION.
           MOVE ITRM-INSTERM TO T02-INSTERM.
           PERFORM REBATE-CALL-SUBPROG.
           IF REB-COMMON-RTNCD = " "
              MOVE REB-COMMON-REBATE TO REB-REBATE
           ELSE
              MOVE 0 TO REB-REBATE REB-SUB.

           IF REB-REBATE < 0
              MOVE 0 TO REB-REBATE.
           MOVE 0 TO REB-SUB.

       REBATE-EP13-EXIT.
           EXIT.

      ********************************************
      * ALABAMA ANY PAYOFF WITHIN 90 DAYS (14)
      ********************************************
