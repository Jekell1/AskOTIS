      * COPYMEMBER: LIBLP/RNFOR10LN

      *********************************************************
      *   NAME:  RNFOR10LN
      *   DESC:  THIS CALCULATION IS USED TO DETERMINE,
      *          IF THE PROCEEDS TO THE CUSTOMER IS LESS THAN 10%
      *          OF THE AMOUNT BEING RENEWED.
      *
      *          NOTE:
      *               A COPY OF THIS EXISTS FOR PD-RECORDS
      *               REMEMBER TO CHANGE 'RNFOR10PD'
      *   IN  :
      *   OUT :  RNFOR10-FG   - 'Y' RENEWAL IS FOR < 10%
      *          RNFOR10-AMT  - 10% OF AMOUNT RENEWED
      * REV :
      *       CS 042798 ADDED "RB" AND "RO" RENEWAL CODES
      *********************************************************
       COMPUTE-RNFOR10 SECTION.
           MOVE " " TO RNFOR10-FG.
           IF LN-SRCD = "PB" OR "RN" OR "RB" OR "RO"
              COMPUTE RNFOR10-AMT ROUNDED =
                    ( LN-PRBAL + LN-PRINTDUE
                                  - (LN-PRFINCHGRB + LN-PRINSRB)
                    ) * 0.1
              IF LN-PROCEEDS < RNFOR10-AMT
                 MOVE "Y" TO RNFOR10-FG.
