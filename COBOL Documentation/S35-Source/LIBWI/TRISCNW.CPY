      * COPYMEMBER: LIBWI/TRISCNW
      ******************************************************************
      *
      * DESCRIPTION: TRANSACTION FILE "C" CASH/CHECK SCAN WORKERS
      *
      * REV:
      *  2024.0501 BAH MADE LEVEL 01, CALL NOT WINDOW
      ******************************************************************
       01  TRISCN-BUF.
           03  TRISCN-TRI1-KEY       PIC X(17).
           03  TRISCN-BRNO           PIC 9(4).
           03  TRISCNW-MMYY          PIC 9999.
           03  TRISCNW-MMYYX REDEFINES TRISCNW-MMYY.
               07  TRISCNW-MM        PIC 99.
               07  TRISCNW-YY        PIC 99.
           03  TRISCN-RETURN         PIC X.
           03  TRISCN-STATUS         PIC XX.
