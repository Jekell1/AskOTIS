      **************************************************
       COMPUTE-RECENCY-SET-EXTERNAL SECTION.
      **************************************************

      * SET EXTERNAL INDEXES FROM INTERNAL:
      
           IF RSUB = 1
              MOVE 10 TO RSUB
           ELSE
              SUBTRACT 1 FROM RSUB.

           IF RDSUB = 1
              MOVE 10 TO RDSUB
           ELSE
              SUBTRACT 1 FROM RDSUB.

      ************************************************************
