      * COPYMEMBER: LIBUP/UPFDABM
      ********************************************************************
      *   BORROWER MEMO EXTRACTION FILE
      ********************************************************************
      $XFD  FILE=ABMFILE
       FD  ABM-FILE
           LABEL RECORDS ARE STANDARD.
       COPY "LIBUP/UP01ABM.CPY".

       FD  ABM-DONE-FILE
           LABEL RECORDS ARE STANDARD.
       01  ABM-DONE-REC   PIC X.

