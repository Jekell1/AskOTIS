      *****************************************************************
      * RECENCY/DELINQUENCY WORLD FORMULA 'D':
      *
      * REV:
      *  111293 JTG CORRECTED LOGIC FOR LN-1STPYDA PAST 25TH
      *  120493 JTG MODIFIED TO CONFORM TO CHARLIE WALTERS DEFINITION
      *****************************************************************
       COMPUTE-RECENCY-DELINQ-D SECTION.

      * N O T E:
      *     MUST HAVE COMPUTED CONTRACTUAL BEFORE THIS ROUTINE
      *     WILL WORK!
      *
      *     RECENCY IS CALLED FROM COMPUTE-CONTRACTUAL-D.
      
      * TEST FOR CURRENT ACCOUNT:
           IF CSUB = 1
              MOVE 1 TO RDSUB
              IF LN-MTD-PAYS-RECDEL NOT < LN-REGPYAMT
                 GO TO COMPUTE-RECENCY-DELINQ-D-EXIT
              ELSE
                 MOVE CPOT30 TO RDPOT30
                 GO TO COMPUTE-RECENCY-DELINQ-D-EXIT.

      * ACCOUNT IS PAST DUE:
           IF LN-MTD-PAYS-RECDEL NOT < LN-REGPYAMT
              MOVE 1 TO RDSUB
              GO TO COMPUTE-RECENCY-DELINQ-D-EXIT.

           MOVE AGEING-DATE TO NDTE-DATE.
           MOVE 35          TO NDTE-DD.
           MOVE -1          TO NDTE-HOLD.
           PERFORM INCREMENT-MONTHS.
           MOVE NDTE-DATE TO SYS-DATE.

           IF LN-PY-YYYYMM-RECDEL NOT = 0
              MOVE LN-PY-YYYYMM-RECDEL TO DATE-YYYYMM
              MOVE DATE-YYYYMM-MM     TO NDTE-MM
              MOVE DATE-YYYYMM-CC     TO NDTE-CC
              MOVE DATE-YYYYMM-YY     TO NDTE-YY
              MOVE S-DD               TO NDTE-DD
           ELSE
              MOVE LN-1STPYDATE TO NDTE-DATE.

           MOVE -1 TO NDTE-HOLD.
           PERFORM INCREMENT-MONTHS.
           MOVE NDTE-DATE TO NUM-DATE.

           PERFORM TIM360.
           IF ELAPSED-MONTHS = 1
              MOVE "Y" TO RDPOT30.
           MOVE ELAPSED-MONTHS TO RDSUB.
           IF RDSUB < 1
              MOVE 1 TO RDSUB.

           IF RDSUB > CSUB
              MOVE CSUB TO RDSUB.

       COMPUTE-RECENCY-DELINQ-D-EXIT.
           EXIT.

      *****************************************************************
      * RECENCY/DELINQUENCY WORLD FORMULA 'F':
      *
      * REV:
      *  111293 JTG CORRECTED LOGIC FOR LN-1STPYDA PAST 25TH
      *  120493 JTG MODIFIED TO CONFORM TO CHARLIE WALTERS DEFINITION
      *****************************************************************
