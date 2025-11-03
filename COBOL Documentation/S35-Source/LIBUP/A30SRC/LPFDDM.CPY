      * COPYMEMBER: LIBLP/LPFDDM
      **************************************************
      *             ( DEALER MEMO FILE )
      *    KEY 1 - DEALER NUMBER, DATE, SEQUENCE
      * REV:
      *  JTG 102892 ADDED DL-MEMO-TYPE
      **************************************************
       FD  ODM-FILE
           LABEL RECORDS ARE STANDARD.
       COPY "LIBUP/A30SRC/LP01DM.CPY" REPLACING LEADING "DM" BY "ODM".
