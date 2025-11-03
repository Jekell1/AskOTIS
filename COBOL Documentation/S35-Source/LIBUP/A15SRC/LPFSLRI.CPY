      * COPYMEMBER: LIBLP/LPFSLRI
      ******************************************************************
      *
      * IDENTIFICATION:  LIBLP/LPFSLRI
      *
      * DESCRIPTION   :  LOAN T(R)IAD IMPORT FILE 'FS'
      *
      *=================================================================
      * REV:
      * KEC 2003.0321 {PR#00026} REGIONAL ACCEPTANCE <A15> [KEVIN M]
      *               ***NEW***
      *
      ******************************************************************
           SELECT LRI-FILE ASSIGN TO LRI-PATH
                  ORGANIZATION RELATIVE
                  ACCESS MODE DYNAMIC
                  LOCK MODE AUTOMATIC WITH LOCK ON RECORD
                  RELATIVE KEY LRI-KEY
                  FILE STATUS FILE-STAT.

