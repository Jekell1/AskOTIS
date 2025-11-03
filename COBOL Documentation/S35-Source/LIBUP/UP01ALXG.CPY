      ******************************************************************
      * ASCII LOAN PAYMENT TRAILER GENERAL LEDGER DIST FILE:
      *  TAB DELMITED
      ******************************************************************
       78  ALXG-SIZE                     VALUE 103. 
       01  ALXG-REC.
           03  ALXG-BRNO                 PIC 9(4).
           03  FILLER                    PIC X.
           03  ALXG-ACCTNO               PIC 9(8).
           03  FILLER                    PIC X.
           03  ALXG-SEQNO                PIC 9(5).
           03  FILLER                    PIC X.
           03  ALXG-LTOUCH-DATE          PIC 9(8).
           03  FILLER                    PIC X.
           03  ALXG-GLTBL              OCCURS 3 TIMES.
               05  ALXG-GLNO             PIC 9(11).
               05  FILLER                PIC X.
               05  ALXG-GLAMT            PIC +9(7).99.
               05  FILLER                PIC X.
           03  ALXG-CR                   PIC X.
           03  ALXG-LF                   PIC X.
