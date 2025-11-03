      * COPYMEMBER: LIBWI/TRPSCNW
      ******************************************************************
      *
      * DESCRIPTION: TRANSACTION FILE "L" LOAN SCAN WORKERS
      *
      * REV:
      *  2024.0501 BAH MADE LEVEL 01, CALL NOT WINDOW
      ******************************************************************
       01  TRPSCN-BUF.
           03  TRPSCN-TRP1-KEY       PIC X(26).
           03  TRPSCN-BRNO           PIC 9(4).
           03  TRPSCNW-MMYY          PIC 9999.
           03  TRPSCNW-MMYYX REDEFINES TRPSCNW-MMYY.
               07  TRPSCNW-MM        PIC 99.
               07  TRPSCNW-YY        PIC 99.
           03  TRPSCN-RETURN         PIC X.
           03  TRPSCN-STATUS         PIC XX.
