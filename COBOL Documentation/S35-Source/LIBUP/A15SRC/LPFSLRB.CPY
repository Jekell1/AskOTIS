      * COPYMEMBER: LIBLP/LPFSLRB
      ******************************************************************
      *
      * IDENTIFICATION:  LIBLP/LPFSLRB
      *
      * DESCRIPTION   :  LOAN T(R)IAD BEHAVIOR FILE 'FS'
      *
      *=================================================================
      * REV:
      * KEC 2004.0109 {PR#00046} REGIONAL ACCEPTANCE <A15> [KEVIN M]
      *               ***NEW***
      *
      ******************************************************************
           SELECT LRB-FILE ASSIGN TO LRB-PATH
                  ORGANIZATION INDEXED
                  ACCESS DYNAMIC
                  LOCK MODE AUTOMATIC WITH LOCK ON RECORD
                  RECORD KEY LRB1-KEY
                  FILE STATUS FILE-STAT.
