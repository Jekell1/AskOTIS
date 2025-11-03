      * COPYMEMBER: LIBWI/TRXSCNW
      ******************************************************************
      *
      * DESCRIPTION: TRANSACTION FILE "S" SALES FINANCE SCAN WORKERS
      *
      * REV:
      *  2024.0501 BAH MADE LEVEL 01, CALL NOT WINDOW
      ******************************************************************
       01  TRXSCN-BUF.
           03  TRXSCN-TRX1-KEY       PIC X(28).
           03  TRXSCN-BRNO           PIC 9(4).
           03  TRXSCNW-MMYY          PIC 9999.
           03  TRXSCNW-MMYYX REDEFINES TRXSCNW-MMYY.
               05  TRXSCNW-MM        PIC 99.
               05  TRXSCNW-YY        PIC 99.
           03  TRXSCN-RETURN         PIC X.
           03  TRXSCN-STATUS         PIC XX.
