      ******************************************************************
      *  ASCII LOAN DEBIT CARD TRAILER FILE 
      *  TAB DELMITED
      *
      ******************************************************************
       78  ALTD-SIZE                      VALUE 91.
       01  ALTD-REC.
           03  ALTD-BRNO                  PIC 9(4).
           03  FILLER                     PIC X.
           03  ALTD-ACCTNO                PIC 9(8).
           03  FILLER                     PIC X.
           03  ALTD-SEQNO                 PIC 999.
           03  FILLER                     PIC X.
           03  ALTD-LTOUCH-DATE           PIC 9(8).
           03  FILLER                     PIC X.
           03  ALTD-DEBIT-CARD-NUMBER     PIC 9(10).
           03  FILLER                     PIC X.
           03  ALTD-DEBIT-CARD-AMT        PIC +9(7).99.
           03  FILLER                     PIC X.
           03  ALTD-LOANDATE              PIC 9(8).
           03  FILLER                     PIC X.
           03  ALTD-VOID-DATE             PIC 9(8).
           03  FILLER                     PIC X.
           03  ALTD-CLEARED-DATE          PIC 9(8).
           03  FILLER                     PIC X.
           03  ALTD-CLEARED-AMT           PIC +9(7).99.
           03  FILLER                     PIC X.

           03  ALTD-CR                  PIC X.
           03  ALTD-LF                  PIC X.
