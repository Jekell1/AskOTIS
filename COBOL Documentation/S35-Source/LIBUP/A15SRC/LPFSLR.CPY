      * COPYMEMBER: LIBLP/LPFSLR
      ******************************************************************
      *
      * IDENTIFICATION:  LIBLP/LPFSLR
      *
      * DESCRIPTION   :  LOAN T(R)IAD FILE 'FS'
      *
      *=================================================================
      * REV:
      * KEC 2003.0321 {PR#00026} REGIONAL ACCEPTANCE <A15> [KEVIN M]
      *               ***NEW***
      *
      ******************************************************************
           SELECT LR-FILE ASSIGN TO LR-PATH
                  ORGANIZATION INDEXED
                  ACCESS DYNAMIC
                  LOCK MODE AUTOMATIC WITH LOCK ON RECORD
                  RECORD KEY LR1-KEY
                  FILE STATUS FILE-STAT.
