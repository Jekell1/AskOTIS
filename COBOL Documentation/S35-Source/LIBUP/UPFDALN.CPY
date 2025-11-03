      * COPYMEMBER: LIBUP/UPFDALN
      ********************************************************************
      *   LOAN MASTER EXTRACTION FILE
      ********************************************************************
      $XFD  FILE=ALNFILE
       FD  ALN-FILE
           LABEL RECORDS ARE STANDARD.
       COPY "LIBUP/UP01ALN.CPY".

       FD  ALN-DONE-FILE
           LABEL RECORDS ARE STANDARD.
       01  ALN-DONE-REC   PIC X.

