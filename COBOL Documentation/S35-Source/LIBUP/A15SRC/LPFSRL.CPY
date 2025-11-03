      * COPYMEMBER: LIBLP/LPFSRL
           SELECT RL-FILE ASSIGN TO RL-PATH
                  ORGANIZATION INDEXED
                  ACCESS DYNAMIC
                  LOCK MODE AUTOMATIC WITH LOCK ON RECORD
                  RECORD KEY RL1-KEY
                  ALTERNATE RECORD KEY RL2-KEY
                          = RL-ACCTNO
                            RL-PU-RT-SEQNO
                            RL-PY-RT-SEQNO
                           WITH DUPLICATES
                  FILE STATUS FILE-STAT.
