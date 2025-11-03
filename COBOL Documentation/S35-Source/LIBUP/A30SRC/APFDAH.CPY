      * COPYMEMBER: LIBAP/APFDAH
      ***************************************************
      *  AP TRANSACTION HISTORY FILE
      *  KEY 1 - VENDOR/DATE/TYPE/CHECK#/INVOICE/SUF/SEQ
      *  KEY 2 - TYPE/CHECK#/INVOICE/SUF/SEQ
      *  KEY 3 - DATE
      ***************************************************
       FD  OAH-FILE
           LABEL RECORDS ARE STANDARD.
       COPY "LIBUP/A30SRC/AP01AH.CPY" REPLACING LEADING "AH" BY "OAH".
