      * COPYMEMBER: LIBGL/ALLENTW
       01  ALLENT-BUF.
           03  ALLENT-STATUS           PIC X.
           03  ALLENT-DISBURSE         PIC S9(7)V99.
           03  ALLENT-BRNO             PIC 9(4).
           03  ALLENT-OTHER-BR-FG      PIC X.
           03  ALLENT-GL-ENTRY-DEFAULT PIC X.
           03  ALLENT-GL-TYPE-ALLOWED  PIC X.
           03  ALLENT-NO-OF-ACCTS      PIC 99.
           03  ALLENT-UNUSED           PIC X(10).
           03  ALLENT-TABLE            OCCURS 5.
               05  ALLENT-ACCT.
                   07  ALLENT-BRANCH   PIC 9(4).
                   07  ALLENT-ACCOUNT  PIC 9(7).
               05  ALLENT-BR-ACCOUNT REDEFINES ALLENT-ACCT PIC 9(11).
               05  ALLENT-AMT          PIC S9(7)V99.
