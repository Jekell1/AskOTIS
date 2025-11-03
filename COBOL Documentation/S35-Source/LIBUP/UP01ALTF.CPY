      ******************************************************************
      *  ASCII  LOAN FEE CODE TRAILER FILE 
      *
      ******************************************************************
       78  ALTF-SIZE                     VALUE 179.
       01  ALTF-REC.
           03  ALTF-BRNO                          PIC 9(4).
           03  FILLER                             PIC X.
           03  ALTF-ACCTNO                        PIC 9(8).
           03  FILLER                             PIC X.
           03  ALTF-SEQNO                         PIC 999.
           03  FILLER                             PIC X.
           03  ALTF-LTOUCH-DATE                   PIC 9(8).
           03  FILLER                             PIC X.
           03  ALTF-FEECODE-TBL                OCCURS 10.
               05  ALTF-FEECODE                   PIC XX.
               05  FILLER                         PIC X.
               05  ALTF-FEECODE-PAYABLE           PIC +9(7).99.
               05  FILLER                         PIC X.

           03  ALTF-CR                  PIC X.
           03  ALTF-LF                  PIC X.
