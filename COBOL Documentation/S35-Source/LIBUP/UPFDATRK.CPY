      * COPYMEMBER: LIBUP/UPFDATRK
      ********************************************************************
      *   TRANSACTION FILE EXTRACT
      ********************************************************************
      $XFD  FILE=ATRKFILE
       FD  ATRK-FILE
           LABEL RECORDS ARE STANDARD.
       COPY "LIBUP/UP01ATRK.CPY".
       FD  ATRK-DONE-FILE
           LABEL RECORDS ARE STANDARD.
       01  ATRK-DONE-REC   PIC X.

