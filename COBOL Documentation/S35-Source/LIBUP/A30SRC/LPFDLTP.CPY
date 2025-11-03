      * COPYMEMBER: LIBLP/LPFDLTP
      ******************************************************************
      *    LOAN PAYMENT SCHEDULE TRAILER FILE : (250) BYTES
      *
      * BAH 06/19/2015 CREATED NEW FROM LPFDLT
      ******************************************************************
       FD  OLTP-FILE
           LABEL RECORDS ARE STANDARD.
       COPY "LIBUP/A30SRC/LP01LTP.CPY"
                          REPLACING LEADING "LTP" BY "OLTP".
