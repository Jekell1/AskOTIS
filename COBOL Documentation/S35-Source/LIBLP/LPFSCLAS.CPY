      * COPYMEMBER: LIBLP/LPFSCLAS
      ***********************************************************
      *           (LPFSCLAS)
      *    FILE SELECT FOR (CLASTR.C) (CLAST1.C) (CLAST2.C)
      ***********************************************************
           SELECT WK-FILE ASSIGN CLAS-PATH
                  ORGANIZATION INDEXED
                  ACCESS DYNAMIC
                  LOCK MODE AUTOMATIC WITH LOCK ON RECORD
                  RECORD KEY WK-KEY
                  FILE STATUS FILE-STAT.
