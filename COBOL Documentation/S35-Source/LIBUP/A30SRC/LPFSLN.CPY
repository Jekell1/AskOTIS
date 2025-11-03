      * COPYMEMBER: LIBLP/LPFSLN
           SELECT LN-FILE ASSIGN TO LN-PATH
                  ORGANIZATION INDEXED
                  ACCESS DYNAMIC
                  LOCK MODE AUTOMATIC WITH LOCK ON RECORD
                  RECORD KEY LN1-KEY
                  ALTERNATE RECORD KEY LN3-KEY
                                     = LN-OWNBR
                                       LN-CLASS
                                       LN-ACCTNO
                                               WITH DUPLICATES
                  ALTERNATE RECORD KEY LN4-KEY WITH DUPLICATES
                  ALTERNATE RECORD KEY LN5-KEY WITH DUPLICATES
                  ALTERNATE RECORD KEY LN6-KEY WITH DUPLICATES
                  ALTERNATE RECORD KEY LN7-KEY WITH DUPLICATES
                  FILE STATUS FILE-STAT.
