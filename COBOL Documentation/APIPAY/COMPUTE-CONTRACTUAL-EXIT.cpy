       COMPUTE-CONTRACTUAL-EXIT.
           MOVE " " TO CSUB-ALDEL-FG.

      * SET EXTERNAL INDEXES FROM INTERNAL:
      
           IF CSUB = 1
              MOVE 10 TO CSUB
           ELSE
              SUBTRACT 1 FROM CSUB.

      ************************************************
      *    COMPUTE CONTRACURAL PAYS THRU TODAY
      ************************************************
