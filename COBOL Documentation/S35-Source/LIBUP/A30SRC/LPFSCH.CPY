      * COPYMEMBER: LIBLP/LPFSCH
      ******************************************************************
      * NOTE: THIS IS HARDCODED FOR USE IN UP/TR3032.CBL
      ******************************************************************
      *    SELECT CH-FILE ASSIGN TO CHANGE-PATH
           SELECT OCH-FILE ASSIGN TO OCH-PATH
                  ORGANIZATION INDEXED
                  ACCESS DYNAMIC
                  LOCK MODE AUTOMATIC WITH LOCK ON RECORD
                  RECORD KEY OCH-KEY
                  FILE STATUS FILE-STAT.
