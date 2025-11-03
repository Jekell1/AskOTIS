      * COPYMEMBER: LIBUP/UPFDABX
      ********************************************************************
      *   BORROWER TRAILER EXTRACTION FILE
      ********************************************************************
      $XFD  FILE=ABXFILE
       FD  ABX-FILE
           LABEL RECORDS ARE STANDARD.
       COPY "LIBUP/UP01ABX.CPY".

       FD  ABX-DONE-FILE
           LABEL RECORDS ARE STANDARD.
       01  ABX-DONE-REC   PIC X.

