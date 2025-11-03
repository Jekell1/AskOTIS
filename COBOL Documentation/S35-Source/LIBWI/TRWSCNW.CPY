      * COPYMEMBER: LIBWI/TRWSCNW
      ******************************************************************
      *
      * DESCRIPTION: TRANSACTION FILE "C" CASH/CHECK SCAN WORKERS
      *
      * REV:
      *  2024.0501 BAH MADE LEVEL 01, CALL NOT WINDOW
      ******************************************************************
       01  TRWSCN-BUF.
           03  TRWSCN-TRW1-KEY       PIC X(14).
           03  TRWSCN-BRNO           PIC 9(4).
           03  TRWSCNW-MMYY          PIC 9999.
           03  TRWSCNW-MMYYX REDEFINES TRWSCNW-MMYY.
               05  TRWSCNW-MM        PIC 99.
               05  TRWSCNW-YY        PIC 99.
           03  TRWSCN-RETURN         PIC X.
           03  TRWSCN-STATUS         PIC XX.
