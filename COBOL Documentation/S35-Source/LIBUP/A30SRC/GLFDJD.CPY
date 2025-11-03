      * COPYMEMBER: LIBGL/GLFDJD
      ***************************************************
      *  GENERAL LEDGER JOURNAL DETAIL LINE
      *  KEY 1 - SOURCE/DATE/BRANCH/ACCT/DATE/LINE
      *  KEY 3 - REFERENCE/DATE
      *  KEY 4 - BRANCH/ACCT/DATE/LINE
      ***************************************************
       FD  OJD-FILE
           LABEL RECORDS ARE STANDARD.
       COPY "LIBUP/A30SRC/GL01JD.CPY" REPLACING LEADING "JD" BY "OJD".
