      * COPYMEMBER: LIBLP/LPFSLRF
      ******************************************************************
      *
      * IDENTIFICATION:  LIBLP/LPFSLRF
      *
      * DESCRIPTION   :  LOAN T(R)IAD 9 EXPORT FILE 'FS'
      *
      *=================================================================
      * REV:
      * KEC 2014.0911 {PR#00356} REGIONAL ACCEPTANCE <A15> [KEVIN M]
      *               ***NEW***
      *
      ******************************************************************
           SELECT LRF-FILE ASSIGN TO LRF-PATH
                  ORGANIZATION RELATIVE
                  ACCESS MODE DYNAMIC
                  LOCK MODE AUTOMATIC WITH LOCK ON RECORD
                  RELATIVE KEY LRF-KEY
                  FILE STATUS FILE-STAT.

