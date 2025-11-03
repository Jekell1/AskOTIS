      * COPYMEMBER: LIBUP/UPFDACH
      ********************************************************************
      *   AUDIT CHANGES ASCII EXTRACTION FILE
      ********************************************************************
      $XFD  FILE=ACHFILE
       FD  ACH-FILE
           LABEL RECORDS ARE STANDARD.
       COPY "LIBUP/UP01ACH.CPY".
       FD  ACH-DONE-FILE
           LABEL RECORDS ARE STANDARD.
       01  ACH-DONE-REC   PIC X.

