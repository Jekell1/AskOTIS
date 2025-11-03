      * COPYMEMBER: LIBLP/LPFSLRR
      ******************************************************************
      *
      * IDENTIFICATION:  LIBLP/LPFSLRR
      *
      * DESCRIPTION   :  LOAN T(R)IAD RISK FILE 'FS'
      *
      *=================================================================
      * REV:
      * KEC 2006.1006 {PR#00150} REGIONAL ACCEPTANCE <A15> [KEVIN M]
      *               ***NEW***
      *
      ******************************************************************
           SELECT LRR-FILE ASSIGN TO LRR-PATH
                  ORGANIZATION INDEXED
                  ACCESS DYNAMIC
                  LOCK MODE AUTOMATIC WITH LOCK ON RECORD
                  RECORD KEY LRR1-KEY
                  FILE STATUS FILE-STAT.
