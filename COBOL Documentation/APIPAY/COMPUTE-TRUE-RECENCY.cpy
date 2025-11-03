      ****************************************************************
       COMPUTE-TRUE-RECENCY SECTION.
      ****************************************************************
           PERFORM TIM.
           IF ELAPSED-DAYS < 0
              MOVE 0 TO ELAPSED-DAYS.
           MOVE ELAPSED-DAYS TO RECENCY.
           COMPUTE RSUB = (ELAPSED-DAYS / 30) + 1.

           IF RSUB > 8
              MOVE 8 TO RSUB.

      *************************************************************************
      *    RECENCY / DELINQUENCY
      * REV :
      *  JTG 072195 ADDED LOGIC FOR SP-RDPOT30-CD = 'A'
      *                             SP-RDPOTCHGOFF-CD = 'A'
      *  JTG 091597 ADDED LOGIC FOR NEW STAT/AGEING BUCKETS
      *  JTG 091697 ADDED LOGIC FOR 'D' WORLD A90, SP-BANKRULE & SP-TRWBANKRULE
      *  JTG 990419 ADDED SP-RECFRMLA-CALENDAR-DUEDAY  "D"         FROM REL: A50
      *                   SP-RECFRMLA-PERCNT-PMT       "P"         MERCURY  #183
      *************************************************************************
