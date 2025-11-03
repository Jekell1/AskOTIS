      * COPYMEMBER: LIBWI/EASCANW
      * KEC 2016.0229 CHANGED EASCAN-CLASS FROM 2 TO 3 BYTES.
      * BAH 2021.0120 EASCAN-KEY WAS 14, NEEDS TO BE 15
      * BAH 2024.0424 MADE LEVEL 01, CALL NOT WINDOW
       01  EASCAN-BUF.
           03  EASCAN-KEY            PIC X(15).
           03  EASCAN-VERIFY.
               05  EASCAN-BRNO       PIC 9(4).
               05  EASCAN-CLASS      PIC 9(3).
               05  EASCAN-GROUP      PIC X(6).
               05  EASCAN-CQ-FG      PIC X.
           03  EASCAN-STATUS         PIC XX.
