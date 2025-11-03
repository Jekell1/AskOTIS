      * COPYMEMBER: LIBLP/LPFSLRE
      ******************************************************************
      *
      * IDENTIFICATION:  LIBLP/LPFSLRE
      *
      * DESCRIPTION   :  LOAN T(R)IAD EXPORT FILE 'FS'
      *
      *=================================================================
      * REV:
      * KEC 2003.0321 {PR#00026} REGIONAL ACCEPTANCE <A15> [KEVIN M]
      *               ***NEW***
      *
      ******************************************************************
           SELECT LRE-FILE ASSIGN TO LRE-PATH
                  ORGANIZATION RELATIVE
                  ACCESS MODE DYNAMIC
                  LOCK MODE AUTOMATIC WITH LOCK ON RECORD
                  RELATIVE KEY LRE-KEY
                  FILE STATUS FILE-STAT.

