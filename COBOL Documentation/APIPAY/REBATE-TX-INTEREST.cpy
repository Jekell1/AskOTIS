      ***********************************************
      *              ( 0 9 )
      *    TEXAS INTEREST REFUND FORMULA
      ***********************************************
       REBATE-TX-INTEREST SECTION.
           MOVE 0 TO REBTX-CALC-INT-TOT.
    
      * IF REBATE DATE IS >= MATURITY, FORCE A ZERO REBATE:
           PERFORM MATURITY-DATE-CALCULATION.
           IF REB-PAYDATE NOT < MDTE-DATE
              MOVE 0 TO REB-REBATE
              GO TO REBATE-TX-INTEREST-EXIT.
    
           PERFORM LOAN-CALCULATIONS.
           MOVE DISCLOSED-FINAMT TO REBTX-AMTFIN.
    
           COMPUTE REBI ROUNDED = REB-LN-SMPRATE * 0.01.
           COMPUTE REBTX-SERCHG-SLICE ROUNDED =
                          LN-SERCHG / REB-LN-ORGTERM.
    
      * SEE IF REBATE DATE IS ON OR PRIOR TO 1STPAY,
      * IF SO DETERMINE INTEREST THRU REBATE DATE
      
           IF REB-PAYDATE NOT > REB-LN-ORIG-1STPYDATE
              MOVE REB-LN-INTDATE  TO NUM-DATE
              MOVE REB-PAYDATE     TO SYS-DATE
              PERFORM TIM365
      * WHY ARE WE MULTIPLYING THE INTEREST BY -1, THIS ALWAYS YIELDS
      * A ZERO REBATE
      *       COMPUTE REBTX-CALC-INT ROUNDED =
      *           REBTX-AMTFIN * REBI / 365 * ELAPSED-DAYS * -1
              COMPUTE REBTX-CALC-INT ROUNDED =
                  REBTX-AMTFIN * REBI / 365 * ELAPSED-DAYS
              ADD REBTX-CALC-INT TO REBTX-CALC-INT-TOT
              GO TO REBATE-TX-INTEREST-END.
    
           MOVE REB-LN-1STPYAMT    TO REBTX-PYAMT.
           MOVE REB-LN-INTDATE     TO REBTX-FROM-DATE.
           MOVE REB-LN-ORIG-1STPYDATE TO REBTX-TO-DATE.
    
       REBATE-TX-INTEREST-LOOP.
           MOVE REBTX-FROM-DATE TO NUM-DATE.
           MOVE REBTX-TO-DATE   TO SYS-DATE.
           PERFORM TIM365.
           COMPUTE REBTX-CALC-INT ROUNDED =
               REBTX-AMTFIN * REBI / 365 * ELAPSED-DAYS.
           ADD REBTX-CALC-INT TO REBTX-CALC-INT-TOT.
           COMPUTE REBTX-AMTFIN =
              REBTX-AMTFIN - REBTX-PYAMT
               + REBTX-CALC-INT + REBTX-SERCHG-SLICE.
           IF REBTX-AMTFIN < 0
              MOVE 0 TO REBTX-AMTFIN.
    
           MOVE REBTX-TO-DATE   TO REBTX-FROM-DATE
                                   NDTE-DATE.
           MOVE 1               TO NDTE-HOLD.
           PERFORM INCREMENT-UNITPER.
           MOVE NDTE-DATE       TO REBTX-TO-DATE.
           MOVE REB-LN-REGPYAMT TO REBTX-PYAMT.
    
           IF REB-PAYDATE > REBTX-TO-DATE
              GO TO REBATE-TX-INTEREST-LOOP.
    
           MOVE REBTX-FROM-DATE TO NUM-DATE.
           MOVE REB-PAYDATE     TO SYS-DATE.
           PERFORM TIM365.
           COMPUTE REBTX-CALC-INT ROUNDED =
               REBTX-AMTFIN * REBI / 365 * ELAPSED-DAYS.
           ADD REBTX-CALC-INT TO REBTX-CALC-INT-TOT.
    
       REBATE-TX-INTEREST-END.
           COMPUTE REB-REBATE =
              1.50 + LN-INTCHG + LN-EXTCHG - REBTX-CALC-INT-TOT.
           IF REB-REBATE < 0
              MOVE 0 TO REB-REBATE.
           IF REB-REBATE > (LN-INTCHG + LN-EXTCHG)
              COMPUTE REB-REBATE = LN-INTCHG + LN-EXTCHG.
    
       REBATE-TX-INTEREST-EXIT.
           EXIT.


      ***********************************************
      *              ( 0 L )
      *    TEXAS INTEREST REFUND FORMULA FOR UNITED
      ***********************************************
