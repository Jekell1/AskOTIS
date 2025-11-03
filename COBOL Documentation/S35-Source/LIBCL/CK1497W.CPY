      * COPYMEMBER: LIBCL/CK1497W
      **********************************************************
      *    CK1497W
      *    WORKERS CL/CK1497, CUSTOM WORLD PROGRAM CALLED FROM
      *    LONPL0 TO VERIFY GL ACCT 1497 IN BALANCE, WORLD PR# 304
      * REV:
      **********************************************************
       01 CK1497-WORKERS.
           03 CK1497-BRNO                   PIC 9(4).
           03 CK1497-TRANS-DATE             PIC 9(8).
           03 FILLER REDEFINES CK1497-TRANS-DATE.
               05 CK1497-CC                 PIC 9(2).
               05 CK1497-YY                 PIC 9(2).
               05 CK1497-MM                 PIC 9(2).
               05 CK1497-DD                 PIC 9(2).
           03 CK1497-AMOUNT                 PIC S9(9)V99.
