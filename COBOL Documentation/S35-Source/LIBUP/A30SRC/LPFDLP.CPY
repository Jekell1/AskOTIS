      * COPYMEMBER: LIBLP/LPFDLP
      *************************************************************
      *    LOAN PAYMENT FILE:   (200 BYTES)
      *
      *         KEY 1 - LOAN #, SEQUENCE #  (ASCENDING)
      *
      *  REV:
      *************************************************************
       FD  OLP-FILE
           LABEL RECORDS ARE STANDARD.
       COPY "LIBUP/A30SRC/LP01LP.CPY" REPLACING LEADING "LP" BY "OLP".
