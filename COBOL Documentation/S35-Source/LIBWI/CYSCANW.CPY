      * COPYMEMBER: LIBWI/CYSCANW
      *  2024.0502 BAH MADE LEVEL 01, CALL NOT WINDOW
       01  CYSCAN-BUF.
           03  CYSCAN-CD1-KEY         PIC X(10).
           03  CYSCAN-CY-KEY          REDEFINES CYSCAN-CD1-KEY.
               05  CYSCAN-KEY-TYPE    PIC X(2).
               05  CYSCAN-KEY-STATE   PIC X(2).
               05  CYSCAN-KEY-CITY    PIC X(3).
               05  CYSCAN-KEY-FILLER  PIC X(3).
           03  CYSCAN-ST-DESC         PIC X(16).
           03  CYSCAN-STATUS          PIC XX.
