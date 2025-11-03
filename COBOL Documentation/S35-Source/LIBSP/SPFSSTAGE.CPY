      * LOCAL FILE IN ED
           SELECT STAGE-FILE ASSIGN TO STAGE-ALL-PATH
                  ORGANIZATION INDEXED
                  ACCESS DYNAMIC
                  LOCK MODE AUTOMATIC WITH LOCK ON RECORD
                  RECORD KEY STAGE-KEY
                  ALTERNATE RECORD KEY STAGE2-KEY
                               = STAGE-BRNO
                                 STAGE-LN-NUMBER
                  FILE STATUS FILE-STAT.
