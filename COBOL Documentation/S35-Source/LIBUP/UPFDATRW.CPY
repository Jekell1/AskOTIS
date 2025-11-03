      * COPYMEMBER: LIBUP/UPFDATRW
      ********************************************************************
      *   TRANSACTION FILE EXTRACT
      ********************************************************************
      $XFD  FILE=ATRWFILE
       FD  ATRW-FILE
           LABEL RECORDS ARE STANDARD.
       COPY "LIBUP/UP01ATRW.CPY".
       FD  ATRW-DONE-FILE
           LABEL RECORDS ARE STANDARD.
       01  ATRW-DONE-REC   PIC X.

