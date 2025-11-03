      * COPYMEMBER: LIBUP/UPFDATRL
      ********************************************************************
      *   TRANSACTION FILE EXTRACT
      ********************************************************************
      $XFD  FILE=ATRLFILE
       FD  ATRL-FILE
           LABEL RECORDS ARE STANDARD.
       COPY "LIBUP/UP01ATRL.CPY".
       FD  ATRL-DONE-FILE
           LABEL RECORDS ARE STANDARD.
       01  ATRL-DONE-REC   PIC X.

