      * COPYMEMBER: LIBWI/CASCANW
      ******************************************************************
      *
      *    CASCAN-TYPE - 'L' = FROM LP/LONPL0 - OPEN/CLOSE CASH DRAWER
      *                  'M' = FROM MN/CAMAIN - MAINTENANCE
      *                  'S' = FROM           - SCAN ONLY
      *
      * 062498 BAH CHANGED CASCAN-DRAWER TO CASCAN-TILLNO AND FROM PIC 99
      *            TO PIC X WITH CASCAN-TILLNO-ZERO ABOVE IT SO THE OLD
      *            TILLNO'S WOULD STILL BE IN THE CORRECT POSITION.
      * 090199 GLF MOVED WINDOW DOWN 3 LINES PER JTG
      * 2016.0307 BAH MADE TILLNO 9(2) INSTEAD OF X. PD0003
      * 2024.0503 BAH MADE LEVEL 01, CALL NOT WINDOW
      ******************************************************************
       01  CASCAN-BUF.
           03  CASCAN-CA-PATH           PIC X(35).
           03  CASCAN-CA-KEY.
               05  CASCANW-BRNO         PIC 9(4).
               05  CASCAN-DATE          PIC 9(8).
               05  CASCAN-TILLNO        PIC 99.
           03  CASCAN-TYPE              PIC X.
           03  CASCAN-STATUS            PIC XX.
