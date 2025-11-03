      * COPYMEMBER: LIBGB/GBARCCL
      ***********************************************************************
      *       ASCII GLOBAL RECAP (DAILY) FILE CLEAR ROUTINE
      *
      ***********************************************************************
       CLEAR-ARC-FILE SECTION.
           MOVE ALL X"09" TO ARC-REC.

           MOVE 0 TO ARC-BRNO ARC-TRANS-DATE.

           MOVE 0 TO ARC-DEPOSIT ARC-OVER-SHORT ARC-CHECKS.

           MOVE 0 TO ARC-OPENED-DATE
                     ARC-OPENED-TIME
                     ARC-CLOSED-DATE
                     ARC-CLOSED-TIME
                     ARC-UPDATED-DATE
                     ARC-UPDATED-TIME
                     ARC-EOD-BATCH-DATE
                     ARC-EOD-BATCH-TIME
                     ARC-FUNDS-DATE
                     ARC-FUNDS-TIME
                     ARC-CHECK-DATE
                     ARC-CHECK-TIME
                     ARC-EARNINGS-DATE
                     ARC-EARNINGS-TIME
                     ARC-EOM-UPDATED-DATE
                     ARC-EOM-UPDATED-TIME
                     ARC-EOM2-BATCH-DATE
                     ARC-EOM2-BATCH-TIME
                     ARC-BANK-BAL
                     ARC-LAST-ACCRUE
                     ARC-CHECK-DEPOSIT.

           MOVE SPACES TO ARC-STATUS ARC-RECAP-FG ARC-GL-FG              
                          ARC-EOD-BATCH-FG ARC-NIGHTLY-FUNDS-FG   
                          ARC-EOM1-BATCH-FG ARC-EOM-UPDATE-FG      
                          ARC-EOM2-BATCH-FG ARC-BS-P2P3-INBAL-FG.

           MOVE SPACES TO ARC-OP-USERID ARC-CL-USERID
                          ARC-UPDATED-USERID ARC-ERROR-MSG.

       CLEAR-RC-FILE-EXIT.
           EXIT.
