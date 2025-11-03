      *COPYMEMBER: LIBUP/UPFDABY
      ******************************************************************
      * BRANCH STATISTICS EXTRACTION FILE
      ******************************************************************
      $XFD  FILE=ABYAFILE
       FD  ABYA-FILE
           LABEL RECORDS ARE STANDARD.
       COPY "LIBUP/UP01ABYA.CPY".

      $XFD  FILE=ABYBFILE
       FD  ABYB-FILE
           LABEL RECORDS ARE STANDARD.
       COPY "LIBUP/UP01ABYB.CPY".


       FD  ABYA-DONE-FILE
           LABEL RECORDS ARE STANDARD.
       01  ABYA-DONE-REC   PIC X.
       FD  ABYB-DONE-FILE
           LABEL RECORDS ARE STANDARD.
       01  ABYB-DONE-REC   PIC X.
