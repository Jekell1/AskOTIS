      ********************************************************
      *    COMPARE LATE CHARGE AGAINST MINIMUM AND MAXIMUM
      ********************************************************
       LATE-CHARGE-CALC-END.
           IF SP-LCUNITPER-CD NOT = "A"
              GO TO LATE-CHARGE-CALC-NORMAL-END.

           MULTIPLY .01 BY UPWK-LC-MIN-FAC.
           MULTIPLY .01 BY UPWK-LC-MAX-FAC.
           IF LCHG-CHARGE < (SP-LCMINCHG * UPWK-LC-MIN-FAC)
              MULTIPLY SP-LCMINCHG BY UPWK-LC-MIN-FAC
                                      GIVING LCHG-CHARGE.
           IF LCHG-CHARGE > (SP-LCMAXAMT * UPWK-LC-MAX-FAC)
              MULTIPLY SP-LCMAXAMT BY UPWK-LC-MAX-FAC
                                      GIVING LCHG-CHARGE.
           GO TO LATE-CHARGE-CALC-EXIT.

