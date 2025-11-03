      ******************************************************************
      *  ASCII  LOAN PAYMENT SCHEDULE TRAILER FILE 
      *  TAB DELMITED
      *
      *******************************************************************
       78  ALTP-SIZE                      VALUE 644.
       01  ALTP-REC.
           03  ALTP-BRNO                 PIC 9(4).
           03  FILLER                    PIC X.
           03  ALTP-ACCTNO               PIC 9(8).
           03  FILLER                    PIC X.
           03  ALTP-SEQNO                PIC 999.
           03  FILLER                    PIC X.
           03  ALTP-LTOUCH-DATE          PIC 9(8).
           03  FILLER                    PIC X.
           03  ALTP-PAY-SCHLD-TBL      OCCURS 41.
               05  ALTP-SCHLD-TERM       PIC 9(4).
               05  FILLER                PIC X.
               05  ALTP-SCHLD-PAYMNT     PIC 9(6).99.
               05  FILLER                PIC X.

           03  ALTP-CR                  PIC X.
           03  ALTP-LF                  PIC X.

