      * COPYMEMBER: LIBLP/LPFDLX
      ******************************************************************
      *    LOAN PAYMENT ADDON FILE: (63 BYTES)
      *    KEY 1 -  LOAN #, SEQUENCE #, CODE
      *
      *    CODE:
      *         G - GENERAL LEDGER DISTRIBUTION RECORD
      *         E - EARNINGS TRAILER RECORD
      *         A - ADDON TRAILER RECORD
      *         N - NON SPECIFIC GENERAL INFORMATION RECORD
      *
      *    REV:
      ******************************************************************
       FD  OLX-FILE
           LABEL RECORDS ARE STANDARD.
       COPY "LIBUP/A30SRC/LP01LX.CPY"
                          REPLACING LEADING "LX" BY "OLX".
