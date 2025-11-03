      * COPYMEMBER: LIBUP/UPFDALM
      ********************************************************************
      *   LOAN MEMO EXTRACTION FILE
      ********************************************************************
      $XFD  FILE=ALMFILE
       FD  ALM-FILE
           LABEL RECORDS ARE STANDARD.
       COPY "LIBUP/UP01ALM.CPY".

       FD  ALM-DONE-FILE
           LABEL RECORDS ARE STANDARD.
       01  ALM-DONE-REC   PIC X.

