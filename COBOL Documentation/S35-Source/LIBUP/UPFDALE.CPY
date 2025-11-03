      * COPYMEMBER: LIBUP/UPFDALE
      ********************************************************************
      *   LOAN ENDORSER EXTRACTION FILE
      ********************************************************************
      $XFD  FILE=ALEFILE
       FD  ALE-FILE
           LABEL RECORDS ARE STANDARD.
       COPY "LIBUP/UP01ALE.CPY".

       FD  ALE-DONE-FILE
           LABEL RECORDS ARE STANDARD.
       01  ALE-DONE-REC   PIC X.

