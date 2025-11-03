      * COPYMEMBER: LIBUP/UPFDAML
      ********************************************************************
      *   MAIL LIST EXTRACTION FILE
      ********************************************************************
      $XFD  FILE=AMLFILE
       FD  AML-FILE
           LABEL RECORDS ARE STANDARD.
       COPY "LIBUP/UP01AML.CPY".

       FD  AML-DONE-FILE
           LABEL RECORDS ARE STANDARD.
       01  AML-DONE-REC   PIC X.

