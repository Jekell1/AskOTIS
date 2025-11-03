      * COPYMEMBER: LIBUP/UPFDACA
      ********************************************************************
      *   CASH DRAWER EXTRACTION FILE
      ********************************************************************
      $XFD  FILE=ACAFILE
       FD  ACA-FILE
           LABEL RECORDS ARE STANDARD.
       COPY "LIBUP/UP01ACA.CPY".

       FD  ACA-DONE-FILE
           LABEL RECORDS ARE STANDARD.
       01  ACA-DONE-REC   PIC X.

