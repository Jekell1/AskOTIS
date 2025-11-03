      * COPYMEMBER: LIBCL/BICLASW
      *********************************************************************
      *   BICLASW
      *                 (CL)
      *   DESCRIPTION:  WORKERS FOR BICLAS.C
      *                 TEST FOR DUAL LOAN WITHIN SAME CLASS
      *
      * REV:
      * 2019.1205 BAH NEW TO TEST FOR DUAL TAX ADVANCE LOANS WHICH ARE
      *               NON LOANS (GIFTNET "X"), CANT HAVE 2 TAX ADVANCE
      *               LOANS IN SAME CLASS (50).
      * 2024.0315 REMOVED BICLAS-STATE, BICLAS-TRANS-DATE AND BICLAS-SRCD
      *********************************************************************
       01  BICLAS-WORKERS.
           03  BICLAS-BRANCH         PIC 9(4).
           03  BICLAS-CLASS          PIC 9(3).
           03  BICLAS-SSNO           PIC 9(9).
      * THIS WILL RETURN WITH "Y" IF WE FOUND ANOTHER LOAN IN SAME CLASS
           03  BICLAS-RESTRICTED     PIC X.
