      * COPYMEMBER: LIBLP/LPFDDS
      **************************************************
      *          ( DEALER STATISTICS FILE )     2150
      *    KEY 1 - DATE, BRANCH, DLNO, CLASS
      *    KEY 2 - DLNO, CLASS
      **************************************************
       FD  ODS-FILE
           LABEL RECORDS ARE STANDARD.
       COPY "LIBUP/A30SRC/LP01DS.CPY" REPLACING LEADING "DS" BY "ODS".
