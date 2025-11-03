      * COPYMEMBER: LIBSP/SPFDEP
      **************************************************
      *        LOAN ELECTRONIC PAYMENT FILE
      *        KEY - ACCTNO
      **************************************************
       FD  OEP-FILE
           LABEL RECORDS ARE STANDARD.
       COPY "LIBUP/A30SRC/SP01EP.CPY" REPLACING LEADING "EP" BY "OEP".
