      * COPYMEMBER: LIBLP/LPFDDT
      ****************************************************
      *          (DEALER TRANSACTION FILE RECORD)
      *    KEY 1 - BRANCH #, NUM, CLASS, ACCOUNT #,
      *            DATE, SEQUENCE #
      * REV :
      ****************************************************
       FD  ODT-FILE
           LABEL RECORDS ARE STANDARD.
       COPY "LIBUP/A30SRC/LP01DT.CPY" REPLACING LEADING "DT" BY "ODT".
