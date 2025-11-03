      * COPYMEMBER: LIBLP/LPFSCL
           SELECT CL-FILE ASSIGN TO CL-PATH
                  ORGANIZATION INDEXED
                  ACCESS DYNAMIC
                  LOCK MODE AUTOMATIC WITH LOCK ON RECORD
                  RECORD KEY CL1-KEY
                  ALTERNATE RECORD KEY CL2-KEY
                               = CL-GROUP-ID
                                 CL-DESC
                            WITH DUPLICATES
                  FILE STATUS FILE-STAT.
