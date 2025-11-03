      * COPYMEMBER: LIBLP/LPPCCL
      **************************************************************************
      *      SAVED COLLATERAL FILE CLEAR ROUTINE
      *
      * REV:
      *  06/17/2020 BAH NEW
      **************************************************************************
       CLEAR-PC-FILE SECTION.

           MOVE SPACES TO PC-REC.

           MOVE 0 TO PC-BRANCH PC-SOC-SEC PC-LOANDATE
                     PC-ORGTERM PC-REGPYAMT PC2-BRANCH
                     PC-LTOUCH-DATE.
 
           MOVE 0 TO PC-LTC-BRNO PC-LTC-ACCTNO PC-LTC-SEQNO
                     PC-LTC-LTOUCH-DATE.

          
           PERFORM VARYING PCSUB FROM 1 BY 1 UNTIL PCSUB > 3
                  MOVE 0 TO PC-LTC-MILEAGE(PCSUB)
                            PC-LTC-VALUE(PCSUB)
                            PC-LTC-SELLPR(PCSUB)
                            PC-LTC-MISC-AMT(PCSUB)
                            PC-LTC-VIN-CHG-DATE(PCSUB)
           END-PERFORM.
 
           MOVE 0 TO PC-LTA-BRNO PC-LTA-ACCTNO PC-LTA-SEQNO
                     PC-LTA-LTOUCH-DATE PC-LTA-PHONE
                     PC-LTA-EFF-DATE PC-LTA-EXP-DATE
                     PC-LTA-CAN-DATE PC-LTA-CONF-DATE
                     PC-LTA-LAST-FORM PC-LTA-BIL-COV
                     PC-LTA-PDL-COV PC-LTA-PIP-COV
                     PC-LTA-UMB-COV PC-LTA-COL-COV
                     PC-LTA-COM-COV.

