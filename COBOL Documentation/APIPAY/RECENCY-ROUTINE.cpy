      ************************************************************
       RECENCY-ROUTINE SECTION.
      ************************************************************
           MOVE 0 TO RECENCY
                     RECDEL.
           MOVE 1 TO RSUB
                     RDSUB.
           MOVE " " TO RDPOT30
                       RDPOTCHGOFF.

           IF LN-CURBAL = 0
              GO TO RECENCY-ROUTINE-EXIT.

           IF LN-DATE-PAID-LAST = 0
              MOVE LN-LOANDATE       TO NUM-DATE
           ELSE
              MOVE LN-DATE-PAID-LAST TO NUM-DATE.
           MOVE AGEING-DATE          TO SYS-DATE.

           PERFORM COMPUTE-TRUE-RECENCY.
           IF SP-RECFRMLA = " " AND
              (SP-BANKRULE NOT = "D") AND (SP-BANKRULE NOT = "F")
                  MOVE RECENCY TO RECDEL
                  MOVE RSUB TO RDSUB
           ELSE
              PERFORM COMPUTE-RECENCY-DELINQ.

       RECENCY-ROUTINE-EXIT.
           EXIT.

      ****************************************************************
