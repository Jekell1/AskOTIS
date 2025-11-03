      * COPYMEMBER: LIBUP/UPFDATRI
      ********************************************************************
      *   TRANSACTION FILE EXTRACT
      ********************************************************************
      $XFD  FILE=ATRIFILE
       FD  ATRI-FILE
           LABEL RECORDS ARE STANDARD.
       COPY "LIBUP/UP01ATRI.CPY".
       FD  ATRI-DONE-FILE
           LABEL RECORDS ARE STANDARD.
       01  ATRI-DONE-REC   PIC X.

