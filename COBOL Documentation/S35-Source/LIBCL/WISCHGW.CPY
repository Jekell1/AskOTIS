      * COPYMEMBER: LIBCL/WISCHGW
      **********************************************************
      *    WISCHGW       
      *    WORKERS FOR WISCONSIN SERVICE CHARGE 63
      * REV: 
      *  2024.1008 #1682  S35Q-141
      **********************************************************
       01 WISCHG-WORKERS.
           03 WISCHG-ORGST                  PIC XX.
           03 WISCHG-LN-SSNO                OCCURS 4 TIMES
                                            PIC 9(9).
           03 WISCHG-PRIOR-ACCTNO           PIC 9(8).
           03 WISCHG-TRANS-DATE             PIC 9(8).
           03 WISCHG-ERRCD                  PIC XX.
           03 WISCHG-SCFRMLA                PIC 99.
           03 WISCHG-PRIOR-SERCHG           PIC S9(7)V99.
