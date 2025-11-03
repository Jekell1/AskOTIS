      * COPYMEMBER: LIBLP/LPFSTR
      ******************************************************************
      * NOTE: THIS IS HARDCODED FOR USE IN UP/TR3032.CBL
      ******************************************************************
      *    SELECT TR-FILE ASSIGN TO TRANS-PATH
           SELECT OTR-FILE ASSIGN TO OTR-PATH
                  ORGANIZATION INDEXED
                  ACCESS DYNAMIC
                  LOCK MODE AUTOMATIC WITH LOCK ON RECORD
                  RECORD KEY OTR1-KEY
                  FILE STATUS FILE-STAT.
