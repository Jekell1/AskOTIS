      * COPYMEMBER: LIBUP/UPFDATRD
      ********************************************************************
      *   TRANSACTION FILE EXTRACT
      ********************************************************************
      $XFD  FILE=ATRDFILE
       FD  ATRD-FILE
           LABEL RECORDS ARE STANDARD.
       COPY "LIBUP/UP01ATRD.CPY".
       FD  ATRD-DONE-FILE
           LABEL RECORDS ARE STANDARD.
       01  ATRD-DONE-REC   PIC X.

