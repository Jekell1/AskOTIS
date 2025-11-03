      * COPYMEMBER: LIBUP/UPFDALP
      ********************************************************************
      *   LOAN PAYMENT EXTRACTION FILE
      ********************************************************************
      $XFD  FILE=ALPFILE
       FD  ALP-FILE
           LABEL RECORDS ARE STANDARD.
       COPY "LIBUP/UP01ALP.CPY".

       FD  ALP-DONE-FILE
           LABEL RECORDS ARE STANDARD.
       01  ALP-DONE-REC   PIC X.

