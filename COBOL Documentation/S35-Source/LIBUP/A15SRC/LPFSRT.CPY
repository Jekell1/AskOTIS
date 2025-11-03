      * COPYMEMBER: LIBLP/LPFSRT
           SELECT RT-FILE ASSIGN TO RT-PATH
                  ORGANIZATION INDEXED
                  ACCESS DYNAMIC
                  LOCK MODE AUTOMATIC WITH LOCK ON RECORD
                  RECORD KEY RT1-KEY
                  ALTERNATE RECORD KEY RT2-KEY
                          = RT-ACCTNO
                            RT-TYPE
                            RT-STATUS
                            RT-TRDATE
                           WITH DUPLICATES
                  ALTERNATE RECORD KEY RT3-KEY
                          = RT-ACCTNO
                            RT-TRDATE
                            RT-SEQNO
                           WITH DUPLICATES
                  FILE STATUS FILE-STAT.
