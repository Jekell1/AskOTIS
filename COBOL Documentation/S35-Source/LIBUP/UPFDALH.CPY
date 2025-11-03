      * COPYMEMBER: LIBUP/UPFDALH
      ********************************************************************
      *   LOAN HISTORY EXTRACTION FILE
      ********************************************************************
      $XFD  FILE=ALHFILE
       FD  ALH-FILE
           LABEL RECORDS ARE STANDARD.
       COPY "LIBUP/UP01ALH.CPY".

       FD  ALH-DONE-FILE
           LABEL RECORDS ARE STANDARD.
       01  ALH-DONE-REC   PIC X.

