      * COPYMEMBER: LIBLP/LPFSLRJ
      ******************************************************************
      *
      * IDENTIFICATION:  LIBLP/LPFSLRJ
      *
      * DESCRIPTION   :  LOAN T(R)IAD 9 IMPORT FILE 'FS'
      *
      *=================================================================
      * REV:
      * KEC 2014.0911 {PR#00356} REGIONAL ACCEPTANCE <A15> [KEVIN M]
      *               ***NEW***
      *
      ******************************************************************
           SELECT LRJ-FILE ASSIGN TO LRJ-PATH
                  ORGANIZATION RELATIVE
                  ACCESS MODE DYNAMIC
                  LOCK MODE AUTOMATIC WITH LOCK ON RECORD
                  RELATIVE KEY LRJ-KEY
                  FILE STATUS FILE-STAT.

