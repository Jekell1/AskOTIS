      * COPYMEMBER: LIBCL/GASCHGW
      *********************************************************************
      *    GASCHGW
      *    WORKERS FOR GEORGIA SERVICE CHARGE FORMULA:
      *REV:
      * JTG 051890 ADDED DUAL-LOAN-ADJ
      * JTG 102490 ADDED PRIOR-NET-BALANCE
      * JTG
      * JTG 030929 MODIFIED SCFRMLA 13 TO ONLY TAKE THE DUAL LOAN
      *            ADJUSTMENT INTO ACCOUNT WHEN THE LOANS'S CLASSES
      *            ARE THE SAME                               LENDMARK #2149
      * JTG 031001 CREATED 01  SCHG-WORKERS-2 FOR PASS FOR SCFRMLA 13
      *                                                       LENDMARK #2149
      * BAH 2015/12/08 EXPANDED DATES TO 9(8) AND EXPANDED CLASS TO 9(3)
      *********************************************************************
       01  SCHG-WORKERS.
      * LONPC2 IS ONLY PROGRAM THAT SETS THIS FLAG = "Y", TO CREATE LE
           03  SCHG-UPDATE-FG           PIC X.
           03  SCHG-ORGST               PIC XX.
           03  SCHG-LN-SSNO             OCCURS 4 TIMES
                                        PIC 9(9).
           03  SCHG-NEW-ACCTNO          PIC 9(8).
           03  SCHG-TRANS-DATE          PIC 9(8).
           03  SCHG-RESTRICT-ADJ        PIC S9(7)V99.
           03  SCHG-DUAL-LOAN-ADJ       PIC S9(7)V99.
           03  SCHG-DUAL-ACCTS          PIC S99.
           03  SCHG-DUAL-LIAB           PIC S9(7)V99.
           03  SCHG-PRIOR-ACCTNO        PIC 9(8).
           03  SCHG-PRIOR-RESTRICT      PIC S9(7)V99.
           03  SCHG-PRIOR-NET-BALANCE   PIC S9(7)V99.
           03  SCHG-PRIOR-LOANDATE      PIC 9(8).

       01  SCHG-WORKERS-2.
           03  SCHG-LN-CLASS            PIC 9(3).
