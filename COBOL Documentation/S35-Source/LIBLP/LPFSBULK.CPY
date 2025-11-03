      * COPYMEMBER: LIBLP/LPFSBULK
      ***********************************************************
      *           (LPFSBULK)
      *    FILE SELECT FOR (BULKTR.C) (BULKT1.C) (BULKT2.C)
      ***********************************************************
           SELECT WK-FILE ASSIGN BULK-PATH
                  ORGANIZATION INDEXED
                  ACCESS DYNAMIC
                  LOCK MODE AUTOMATIC WITH LOCK ON RECORD
                  RECORD KEY WK-KEY
                  FILE STATUS FILE-STAT.
