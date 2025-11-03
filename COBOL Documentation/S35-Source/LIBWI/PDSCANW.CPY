      * COPYMEMBER: LIBWI/PDSCANW
      *
      * BAH 20190627 REMOVED PD-REVOLVING, PT0153
      *  2024.0502 BAH MADE LEVEL 01, CALL NOT WINDOW
       01  PDSCAN-BUF.
           03  PDSCAN-ID             PIC X.
           03  PDSCAN-BRANCH         PIC 9(4).
           03  PDSCAN-VERIFY.
               05  PDSCAN-SMALLOAN   PIC X.
               05  PDSCAN-SALEFIN    PIC X.
               05  PDSCAN-SERVACCT   PIC X.
               05  PDSCAN-LOANTYPE   PIC X.
               05  PDSCAN-GIFTNET    PIC X.
      * PDSCAN-BALLOON IS USED TO SELECT ONLY QUOTES/SAVED LOANS
      * THAT WENT TO THE BALLOON CALCUATE DIRECT LOAN PROGRAMS:
               05  PDSCAN-BALLOON    PIC X.
           03  PDSCAN-KEY            PIC X(9).
           03  PDSCAN-NO            REDEFINES PDSCAN-KEY
                                     PIC 9(9).
           03  PDSCAN-BWNAME         PIC X(33).
           03  PDSCAN-STATUS         PIC XX.
