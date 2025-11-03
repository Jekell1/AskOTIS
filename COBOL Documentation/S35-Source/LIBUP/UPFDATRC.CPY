      * COPYMEMBER: LIBUP/UPFDATRC
      ********************************************************************
      *   TRANSACTION FILE EXTRACT
      ********************************************************************
      $XFD  FILE=ATRCFILE
       FD  ATRC-FILE
           LABEL RECORDS ARE STANDARD.
       COPY "LIBUP/UP01ATRC.CPY".
       FD  ATRC-DONE-FILE
           LABEL RECORDS ARE STANDARD.
       01  ATRC-DONE-REC   PIC X.

