      ***********************************************
      * REPAY PAYMENTS
      * /USR/DATA/R1/FTP/OUT/PAYS/TRP.BRNO.NUMBER.SEQ
      *                               1234.123456.SEQ
      ***********************************************
       01  REPAY-TRP-PATH.
           03  REPAY-TRP-DIR.
               05  FILLER              PIC X      VALUE "/".
               05  REPAY-TRP-PATH-BASE PIC X(12).
               05  REPAY-TRP-PATH-DIR  PIC X(13)  VALUE "FTP/OUT/PAYS/".
           03  REPAY-TRP-FILENAME      PIC X(4)   VALUE "TRP.".
           03  REPAY-TRP-PATH-BRNO     PIC 9999.
           03  FILLER                  PIC X VALUE ".".
           03  REPAY-TRP-PATH-NUMBER   PIC 9(6).
           03  FILLER                  PIC X VALUE ".".
           03  REPAY-TRP-PATH-SEQ      PIC 9(3).

       01  REPAY-TRP-IFN               PIC X(8) VALUE "REPAYTRP".
       01  REPAY-TRP-SUB               PIC 99.

      * TC-TILLNO IS 9(2) IN A30. XTC-TILL IS STILL PIC X LIKE A15.
      * TL-TILLNO, TP-TILLNO TOO
       01  WS-TILLNO           PIC 99.
       01  WS-TILLNOZ REDEFINES WS-TILLNO.
           03  FILLER          PIC X.
           03  WS-TILLNO-X     PIC X.
