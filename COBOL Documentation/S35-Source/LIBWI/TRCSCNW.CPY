      * COPYMEMBER: LIBWI/TRCSCNW
      ******************************************************************
      *
      * DESCRIPTION: TRANSACTION FILE "C" CASH/CHECK SCAN WORKERS
      *
      * REV:
      *  2024.0501 BAH MADE LEVEL 01, CALL NOT WINDOW
      ******************************************************************
       01  TRCSCN-BUF.
           03  TRCSCN-TRC1-KEY       PIC X(26).
           03  TRCSCN-BRNO           PIC 9(4).
           03  TRCSCNW-MMYY          PIC 9999.
           03  TRCSCNW-MMYYX REDEFINES TRCSCNW-MMYY.
               05  TRCSCNW-MM        PIC 99.
               05  TRCSCNW-YY        PIC 99.
           03  TRCSCN-RETURN         PIC X.
           03  TRCSCN-STATUS         PIC XX.
