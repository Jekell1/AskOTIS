      * COPYMEMBER: LIBAP/APFDAD
      **************************************************
      *    ACCTS PAYABLE TRANSACTION RELEASE DETAIL    *
      *    KEY1 = LOC, VENDOR, CHECK SEQ, INVOICE, SUFF*
      **************************************************
       FD  OAD-FILE
           LABEL RECORDS ARE STANDARD.
       COPY "LIBUP/A30SRC/AP01AD.CPY" REPLACING LEADING "AD" BY "OAD".
