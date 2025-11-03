      * COPYMEMBER: LIBUP/UPFDALTI
      ********************************************************************
      *   ASCII LOAN TRAILER FILE
      ********************************************************************
      $XFD  FILE=ALTIFILE
       FD  ALTI-FILE
           LABEL RECORDS ARE STANDARD.
       COPY "LIBUP/UP01ALTI.CPY".

       FD  ALTI-DONE-FILE
           LABEL RECORDS ARE STANDARD.
       01  ALTI-DONE-REC   PIC X.
