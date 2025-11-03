      * COPYMEMBER: LIBEX/EX01XLSJ
      ******************************************************************
      *    LOAN JUDGMENT INFORMATION
      *
      * REV:
      *  170410 BAH NEW JUDGMENT TRAILER DATA WAREHOUSE PR#5277
      ******************************************************************
       78  XLSJ-SIZE                     VALUE 585.
       01  XLSJ-REC.
           03  XLS-J-BRACCTNO             PIC 9(10).
           03  XLS-J-ACCTNO               PIC 9(8).
           03  XLS-J-CODE                 PIC X.
           03  XLS-J-SEQNO                PIC 9.
           
           03  XLS-J-FILEDATE         PIC 9(8).
           03  XLS-J-CASE-NO          PIC X(15).
           03  XLS-J-AMT              PIC +9(7).99.
           03  XLS-J-COUNTY           PIC X(25).
           03  XLS-J-CLERK-NAME       PIC X(25).
           03  XLS-J-CLERK-ADDR       PIC X(25).
           03  XLS-J-CLERK-CITY       PIC X(15).
           03  XLS-J-CLERK-STATE      PIC X(2).
           03  XLS-J-CLERK-ZIP        PIC X(10).
           03  XLS-J-CLERK-PHONE      PIC 9(10).

           03  XLS-J-SHERIFF-FEE      PIC +9(7).99.
           03  XLS-J-COURT-FEE        PIC +9(7).99.
           03  XLS-J-ATTY-FEE         PIC +9(7).99.

           03  XLS-J-DISMISS-DATE     PIC 9(8).
           03  XLS-J-GARN-INFO        PIC X(25).
           03  XLS-J-EMPLOY-NAME      PIC X(25).
           03  XLS-J-EMPLOY-ADDR      PIC X(25).
           03  XLS-J-EMPLOY-CITY      PIC X(15).
           03  XLS-J-EMPLOY-STATE     PIC X(2).
           03  XLS-J-EMPLOY-ZIP       PIC X(10).
           03  XLS-J-GARN-DATE        PIC 9(8).

           03  XLS-J-JD-FILED-ON      PIC X.
           03  XLS-J-GARN-FILED-ON    PIC X.
           03  XLS-J-ATTY-NAME        PIC X(25).
           03  XLS-J-ATTY-ADDR        PIC X(25).
           03  XLS-J-ATTY-CITY        PIC X(15).
           03  XLS-J-ATTY-STATE       PIC X(2).
           03  XLS-J-ATTY-ZIP         PIC X(10).
           03  XLS-J-ATTY-PHONE       PIC 9(10).

           03  FILLER               PIC X(42).
           03  XLS-J-CR             PIC X.
           03  XLS-J-LF             PIC X.
