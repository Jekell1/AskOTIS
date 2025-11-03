      * COPYMEMBER: LIBUP/UPFDATRX
      ********************************************************************
      *   TRANSACTION FILE EXTRACT
      ********************************************************************
      $XFD  FILE=ATRXFILE
       FD  ATRX-FILE
           LABEL RECORDS ARE STANDARD.
       COPY "LIBUP/UP01ATRX.CPY".
       FD  ATRX-DONE-FILE
           LABEL RECORDS ARE STANDARD.
       01  ATRX-DONE-REC   PIC X.

