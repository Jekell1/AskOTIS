      * COPYMEMBER: LIBAP/APFDCK
      **************************************************
      *           ACCOUNTS PAYABLE CHECK FILE          *
      **************************************************
       FD  OCKAP-FILE
           LABEL RECORDS ARE STANDARD.
       COPY "LIBUP/A30SRC/AP01CK.CPY" REPLACING LEADING "CK" BY "OCKAP".
