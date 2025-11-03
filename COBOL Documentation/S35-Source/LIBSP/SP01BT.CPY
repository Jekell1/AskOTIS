      * COPYMEMBER: LIBSP/SP01BT
      ***************************************************
      *                                                 *
      *     BATCH PROCESSING FILE               *
      * RSZ 69
      *                                                 *
      ***************************************************
       01  BT-REC.
      * 1 - 5
           03  BT-CODE                   PIC 9(5).
      * 6 - 11
           03  BT-BRANCH                 PIC 9(6).
      * 12 - 19   ACCTNO = 6 DIGITS OF OUR LOAN NUMBERS 
           03  BT-ACCTNO                 PIC 9(8).
      * 20 - 28
           03  BT-SSNO                   PIC 9(9).
      * 29 - 37
           03  BT-AMT                    PIC 9(7)V99.
           03  BT-AMTX REDEFINES BT-AMT PIC X(9).
      * 38 - 38
           03  BT-SIGN                   PIC 9.
      * 39 - 44
           03  BT-DATE                   PIC 9(6).
      * 45 - 69
           03  BT-NAME                   PIC X(25).
