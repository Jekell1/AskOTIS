      * COPYMEMBER: LIBLP/LPFSAE
      * NEW REGACC PR#299
           SELECT AE-FILE ASSIGN TO AE-PATH
                  ORGANIZATION INDEXED
                  ACCESS DYNAMIC
                  LOCK MODE AUTOMATIC WITH LOCK ON RECORD
                  RECORD KEY AE1-KEY
                  ALTERNATE RECORD KEY AE2-KEY
                     = AE-YYMMDD
                       AE-ACCTNO
                  WITH DUPLICATES
                  FILE STATUS FILE-STAT.
