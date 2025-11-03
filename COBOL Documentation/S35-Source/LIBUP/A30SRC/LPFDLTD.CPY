      * COPYMEMBER: LIBLP/LPFDLTD
      ******************************************************************
      *    LOAN DEBIT CARD TRAILER FILE : (90) BYTES
      *       F4 - DEBIT CARD PROCEEDS - LONPD0 DISBURSEMENTS
      *
      *
      * BAH 02/20/2018 NEW FOR NETSPEND WORLD PR#1306
      ******************************************************************
       FD  OLTD-FILE
           LABEL RECORDS ARE STANDARD.
       COPY "LIBUP/A30SRC/LP01LTD.CPY"
                          REPLACING LEADING "LTD" BY "OLTD".
