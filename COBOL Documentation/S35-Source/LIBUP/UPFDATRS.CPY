      * COPYMEMBER: LIBUP/UPFDATRS
      ********************************************************************
      *   TRANSACTION FILE EXTRACT
      ********************************************************************
      $XFD  FILE=ATRSFILE
       FD  ATRS-FILE
           LABEL RECORDS ARE STANDARD.
       COPY "LIBUP/UP01ATRS.CPY".
       FD  ATRS-DONE-FILE
           LABEL RECORDS ARE STANDARD.
       01  ATRS-DONE-REC   PIC X.

