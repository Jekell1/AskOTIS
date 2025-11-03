      * COPYMEMBER: LIBLP/LPFDLM
      **************************************************
      *        LOAN MEMO FILE
      *        KEY 1 - ACCTNO, DATE, SEQ
      * REV:
      **************************************************
       FD  OLM-FILE
           LABEL RECORDS ARE STANDARD.
       COPY "LIBUP/A30SRC/LP01LM.CPY" REPLACING LEADING "LM" BY "OLM".
