      * COPYMEMBER: LIBLP/LPFSLM
           SELECT LM-FILE ASSIGN TO LM-PATH
                  WITH COMPRESSION
                  ORGANIZATION INDEXED
                  ACCESS DYNAMIC
                  LOCK MODE AUTOMATIC WITH LOCK ON RECORD
                  RECORD KEY LM1-KEY
                  FILE STATUS FILE-STAT.
