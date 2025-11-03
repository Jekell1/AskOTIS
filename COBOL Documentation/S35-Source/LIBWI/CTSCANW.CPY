      * COPYMEMBER: LIBWI/CTSCANW   COUNTY SALES TAX CODES
      *  2024.0501 BAH MADE LEVEL 01, CALL NOT WINDOW
       01  CTSCAN-BUF.
           03  CTSCAN-CD1-KEY         PIC X(10).
           03  CTSCAN-CT-KEY          REDEFINES CTSCAN-CD1-KEY.
               05  CTSCAN-KEY-TYPE    PIC X(2).
               05  CTSCAN-KEY-STATE   PIC X(2).
               05  CTSCAN-KEY-COUNTY  PIC X(3).
               05  CTSCAN-KEY-FILLER  PIC X(3).
           03  CTSCAN-ST-DESC         PIC X(16).
           03  CTSCAN-STATUS          PIC XX.
