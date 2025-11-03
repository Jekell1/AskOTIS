      * COPYMEMBER: LIBLP/LPFDCA
      **************************************************
      *             ( CASH DRAWER FILE )
      *    KEY 1 - BRANCH, DATE, TILL #
      * REV:
      *
      **************************************************
       FD  OCA-FILE
           LABEL RECORDS ARE STANDARD.
       COPY "LIBUP/A30SRC/LP01CA.CPY" REPLACING LEADING "CA" BY "OCA".
