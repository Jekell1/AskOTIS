      * COPYMEMBER: LIBSP/SPFSCJ
           SELECT CJ-FILE ASSIGN TO CJ-PATH
                  ORGANIZATION INDEXED
                  ACCESS DYNAMIC
                  LOCK MODE AUTOMATIC WITH LOCK ON RECORD
                  RECORD KEY CJ1-KEY
                  ALTERNATE RECORD KEY CJ2-KEY 
                             = CJ-BRNO
                               CJ-ACCTNO
                               CJ-DATE
                               CJ-START-TIME
                             WITH DUPLICATES
                  FILE STATUS FILE-STAT.
