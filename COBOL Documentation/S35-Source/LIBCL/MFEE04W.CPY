      * COPYMEMBER: LIBCL/MFEE04W
      ****************************************************
      *    MFEE04
      *    WORKERS FOR TENNESSEE MAINTENANCE CHARGE FORMULA 4
      * REV:
      *  BAH 170407 CHANGED MFEE04-TRANS-DATE TO 9(8)
      ****************************************************
       01  MFEE04-WORKERS.
           03  MFEE04-LN-SSNO             OCCURS 4 TIMES
                                          PIC 9(9).
           03  MFEE04-TRANS-DATE          PIC 9(8).
           03  MFEE04-CHARGE-MFEE         PIC X.
           03  MFEE04-MFFRMLA             PIC 9(2).
           03  MFEE04-PRIOR-ACCTNO        PIC 9(8).
