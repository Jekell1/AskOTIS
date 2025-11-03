      * COPYMEMBER: LIBUP/UPFDAGL
      ********************************************************************
      *   GENERAL LEDGER MASTER EXTRACTION FILE
      ********************************************************************
      $XFD  FILE=AGLFILE
       FD  AGL-FILE
           LABEL RECORDS ARE STANDARD.
       COPY "LIBUP/UP01AGL.CPY".

       FD  AGL-DONE-FILE
           LABEL RECORDS ARE STANDARD.
       01  AGL-DONE-REC   PIC X.

