      * COPYMEMBER: LIBUP/UPFDATRP
      ********************************************************************
      *   TRANSACTION FILE EXTRACT
      ********************************************************************
      $XFD  FILE=ATRPFILE
       FD  ATRP-FILE
           LABEL RECORDS ARE STANDARD.
       COPY "LIBUP/UP01ATRP.CPY".
       FD  ATRP-DONE-FILE
           LABEL RECORDS ARE STANDARD.
       01  ATRP-DONE-REC   PIC X.

