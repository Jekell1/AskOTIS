      * COPYMEMBER: LIBLP/LPFDLH
      **************************************************
      *        LOAN HISTORY FILE
      *        KEY - SSNO, ACCTNO
      **************************************************
       FD  OLH-FILE
           LABEL RECORDS ARE STANDARD.
       COPY "LIBUP/A30SRC/LP01LH.CPY" REPLACING LEADING "LH" BY "OLH".
