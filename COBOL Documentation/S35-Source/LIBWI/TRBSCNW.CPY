      * COPYMEMBER: LIBWI/TRBSCNW
      ******************************************************************
      *
      * DESCRIPTION: TRANSACTION FILE "B" BRANCH TRANSFER
      *
      * REV:
      *  2024.0501 BAH MADE 01 LEVEL, CALL NOT WINDOW
      ******************************************************************
       01  TRBSCN-BUF.
           03  TRBSCN-TRB1-KEY       PIC X(28).
           03  TRBSCN-BRNO           PIC 9(4).
           03  TRBSCNW-MMYY          PIC 9999.
           03  TRBSCNW-MMYYX REDEFINES TRBSCNW-MMYY.
               05  TRBSCNW-MM        PIC 99.
               05  TRBSCNW-YY        PIC 99.
           03  TRBSCN-RETURN         PIC X.
           03  TRBSCN-STATUS         PIC XX.
