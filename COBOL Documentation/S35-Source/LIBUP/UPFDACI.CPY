      * COPYMEMBER: LIBUP/UPFDACI
      ********************************************************************
      *   CUSTOMER INFO EXTRACTION FILE
      ********************************************************************
      $XFD  FILE=ACIFILE
       FD  ACI-FILE
           LABEL RECORDS ARE STANDARD.
       COPY "LIBUP/UP01ACI.CPY".

       FD  ACI-DONE-FILE
           LABEL RECORDS ARE STANDARD.
       01  ACI-DONE-REC   PIC X.

