      * COPYMEMBER: LIBAP/APFDAT
      **************************************************
      *    ACCTS PAYABLE TRANSACTION HISTORY DETAIL    *
      **************************************************
       FD  OAT-FILE
           LABEL RECORDS ARE STANDARD.
       COPY "LIBUP/A30SRC/AP01AT.CPY" REPLACING LEADING "AT" BY "OAT".
