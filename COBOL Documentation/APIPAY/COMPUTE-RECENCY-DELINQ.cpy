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
       COMPUTE-RECENCY-DELINQ SECTION.
           IF (SP-BANKRULE = "D")
              PERFORM COMPUTE-RECENCY-DELINQ-D
           ELSE
           IF (SP-BANKRULE = "F")
              PERFORM COMPUTE-RECENCY-DELINQ-F
           ELSE
              IF SP-RECFRMLA-PERCNT-PMT
                 IF SP-RECPDTH = 0
                    MOVE .01 TO AGEING-WORK
                 ELSE
                    COMPUTE AGEING-WORK ROUNDED =
                         LN-REGPYAMT * SP-RECPDTH * .01
                 END-IF
                 IF LN-MTD-PAYS-RECDEL >= AGEING-WORK
                    MOVE 1 TO RDSUB
                 ELSE
                    IF LN-PY-YYYYMM-RECDEL NOT = 0
                       MOVE LN-PY-YYYYMM-RECDEL TO DATE-YYYYMM
                       MOVE DATE-YYYYMM-MM      TO NUM-MO
                       MOVE DATE-YYYYMM-CC      TO NUM-CC
                       MOVE DATE-YYYYMM-YY      TO NUM-YR
                    ELSE
                       MOVE LN-LOANDATE TO NUM-DATE
                    END-IF
                    MOVE AGEING-DATE    TO SYS-DATE
                    MOVE 5              TO NUM-DA S-DD
                    PERFORM TIM360
                    MOVE ELAPSED-MONTHS TO RDSUB
                 END-IF
              ELSE
                 IF SP-RECFRMLA-CALENDAR-DUEDAY
                    IF S-DD < LN-1STPYDA
                       MOVE 5 TO NUM-DA
                       MOVE 4 TO S-DD
                    ELSE
                       MOVE 5 TO NUM-DA S-DD
                    END-IF
                 ELSE
                    MOVE 5 TO NUM-DA S-DD
                 END-IF
                 PERFORM TIM360
                 MOVE ELAPSED-MONTHS TO RDSUB.

      ****************************************************************
      * TEST FOR POTENTIAL 30'S, SET RDPOT30
      ****************************************************************
      
      * CURRENTLY RDPOT30 IS ONLY SET IN COMPUTE-RECENCY-DELINQ-D,
      * IF WORLD SP-BANKRULE = 'D'
      
           IF SP-BANKRULE = "F"
              IF RDSUB > 2
                 DIVIDE RDSUB BY 2 GIVING AGEING-SUB-WORKER
                                   REMAINDER AGEING-SUB-REMAIN
                 IF AGEING-SUB-REMAIN NOT = 0
                    ADD 1 TO AGEING-SUB-WORKER
                 END-IF
                 MOVE AGEING-SUB-WORKER TO RDSUB.

      ****************************************************************
      *    TEST FOR POTENTIAL CHARGE OFF'S
      ****************************************************************

      * MERCURY POTENTIAL CHARGE OFF'S CODE = 'A'

           IF SP-RDPOTCHGOFF-CD = "A"
              IF RDSUB > 5
                 IF LN-PLCD = " "
                    IF RDSUB = 6
                       MOVE "Y" TO RDPOTCHGOFF
                    ELSE
                    IF RDSUB > 6
                       MOVE "M" TO RDPOTCHGOFF.

           IF ELAPSED-MONTHS < 1
              MOVE 1 TO RDSUB.

           COMPUTE RECDEL = RDSUB * 30 - 30.

           IF RDSUB > 8
              MOVE 8 TO RDSUB
           ELSE
              IF RDPOT30 = "Y"
                 MOVE 10 TO RDSUB.

       COMPUTE-RECENCY-DELINQ-EXIT.
           EXIT.

      *****************************************************************
      * RECENCY/DELINQUENCY WORLD FORMULA 'D':
      *
      * REV:
      *  111293 JTG CORRECTED LOGIC FOR LN-1STPYDA PAST 25TH
      *  120493 JTG MODIFIED TO CONFORM TO CHARLIE WALTERS DEFINITION
      *****************************************************************
