      * COPYMEMBER: LIBUP/UPFDATRJ
      ********************************************************************
      *   TRANSACTION FILE EXTRACT
      ********************************************************************
      $XFD  FILE=ATRJFILE
       FD  ATRJ-FILE
           LABEL RECORDS ARE STANDARD.
       COPY "LIBUP/UP01ATRJ.CPY".
       FD  ATRJ-DONE-FILE
           LABEL RECORDS ARE STANDARD.
       01  ATRJ-DONE-REC   PIC X.

