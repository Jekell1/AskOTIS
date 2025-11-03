      * COPYMEMBER: LIBSP/SPFSSORT
      ******************************************************************
      *
      * IDENTIFICATION:  LIBSP/SPFSSORT
      *
      * DESCRIPTION   :  STANDARD SORT FILE (FS)
      *
      *=================================================================
      * REV:
      * KEC 2003.1023 {PR#00000} PARADATA <A15> [KEN CISZEWSKI]
      *               ***NEW***
      *
      ******************************************************************
           SELECT SORT-FILE ASSIGN SORT-PATH
                  ORGANIZATION INDEXED
                  ACCESS DYNAMIC
                  LOCK MODE AUTOMATIC WITH LOCK ON RECORD
                  RECORD KEY SORT-KEY
                  FILE STATUS FILE-STAT.
