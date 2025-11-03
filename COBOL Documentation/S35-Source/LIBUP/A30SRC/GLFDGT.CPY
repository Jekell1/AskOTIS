      * COPYMEMBER: LIBGL/GLFDGT
      ******************************************************************
      *
      * GENERAL LEDGER TRANSACTION FILE
      *
      *         PRIMARY KEY:  GT-SOURCE
      *                       GT-DATE2
      *                       GT-BRANCH
      *                       GT-ACCTNO 
      *                       GT-DATE3
      *                       GT-DRCRFG
      *                       GT-LINE
      *
      *    SECONDARY KEY (2): GT-DATE1
      *
      *    SECONDARY KEY (3): GT-BRANCH
      *                       GT-ACCTNO 
      *                       GT-DATE3
      *                       GT-DRCRFG
      *                       GT-LINE
      *
      *  NOTE: GT-DATE1 GT-DATE2 GT-DATE3 ARE ACTIVE CCYYMMDD DATES.
      *        IN A15, GT-DATE1 GT-DATE2 GT-DATE3 ARE YYMMDD DATES.
      *
      ******************************************************************
       FD  OGT-FILE
           BLOCK CONTAINS 1024 TO 1024 CHARACTERS
           LABEL RECORDS ARE STANDARD.
       COPY "LIBUP/A30SRC/GL01GT.CPY" REPLACING LEADING "GT" BY "OGT".
