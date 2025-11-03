      * COPYMEMBER: LIBAP/APFDRI
      **************************************************
      *    ACCOUNTS PAYABLE RECURRING INVOICE FILE     *
      *    KEY1 = VENDOR NO. / INVOICE NUMBER          *
      **************************************************
       FD  ORI-FILE
           LABEL RECORDS ARE STANDARD.
       COPY "LIBUP/A30SRC/AP01RI.CPY" REPLACING LEADING "RI" BY "ORI".
