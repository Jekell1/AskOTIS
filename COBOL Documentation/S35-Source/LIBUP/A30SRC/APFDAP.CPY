      * COPYMEMBER: LIBAP/APFDAP
      **************************************************
      *    ACCOUNTS PAYABLE INVOICE FILE               *
      **************************************************
       FD  OAP-FILE
           LABEL RECORDS ARE STANDARD.
       COPY "LIBUP/A30SRC/AP01AP.CPY" REPLACING LEADING "AP" BY "OAP".
