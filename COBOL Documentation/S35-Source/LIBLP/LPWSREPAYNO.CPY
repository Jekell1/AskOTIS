      ***********************************************
      * REPAY NOTICE FROM PAYMENTS
      * /USR/DATA/R1/FTP/OUT/PAYS/NO.BRNO.NUMBER
      *                              1234.123456
      ***********************************************
       01  REPAY-NO-PATH.
           03  REPAY-NO-DIR.
               05  FILLER             PIC X      VALUE "/".
               05  REPAY-NO-PATH-BASE PIC X(12).
               05  REPAY-NO-PATH-DIR  PIC X(13)   VALUE "FTP/OUT/PAYS/".
           03  REPAY-NO-FILENAME      PIC X(3)   VALUE "NO.".
           03  REPAY-NO-PATH-BRNO     PIC 9999.
           03  FILLER                 PIC X VALUE ".".
           03  REPAY-NO-PATH-NUMBER   PIC 9(6).

       01  REPAY-NO-IFN               PIC X(7) VALUE "REPAYNO".
