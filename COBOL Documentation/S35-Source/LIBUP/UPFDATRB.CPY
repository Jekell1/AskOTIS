      * COPYMEMBER: LIBUP/UPFDATRB
      ********************************************************************
      *   TRANSACTION FILE EXTRACT
      ********************************************************************
      $XFD  FILE=ATRBFILE
       FD  ATRB-FILE
           LABEL RECORDS ARE STANDARD.
       COPY "LIBUP/UP01ATRB.CPY".

       FD  ATRB-DONE-FILE
           LABEL RECORDS ARE STANDARD.
       01  ATRB-DONE-REC   PIC X.

