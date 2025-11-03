      * COPYMEMBER: LIBUP/UPFDAGT
      ********************************************************************
      *   GENERAL LEDGER TRANSACTION EXTRACTION FILE
      ********************************************************************
      $XFD  FILE=AGTFILE
       FD  AGT-FILE
           LABEL RECORDS ARE STANDARD.
       COPY "LIBUP/UP01AGT.CPY".

       FD  AGT-DONE-FILE
           LABEL RECORDS ARE STANDARD.
       01  AGT-DONE-REC   PIC X.

