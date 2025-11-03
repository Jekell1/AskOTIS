      * COPYMEMBER: LIBUP/UPFDABW
      ********************************************************************
      *   BORROWER MASTER EXTRACTION FILE
      ********************************************************************
      $XFD  FILE=ABWFILE
       FD  ABW-FILE
           LABEL RECORDS ARE STANDARD.
       COPY "LIBUP/UP01ABW.CPY".

       FD  ABW-DONE-FILE
           LABEL RECORDS ARE STANDARD.
       01  ABW-DONE-REC   PIC X.

