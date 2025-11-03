      * COPYMEMBER: LIBSP/SPFSREPORT
      ******************************************************************
      *
      * IDENTIFICATION:  LIBSP/SPFSREPORT
      *
      * DESCRIPTION   :  STANDARD REPORT FILE (FS)
      *
      *=================================================================
      * REV:
      * KEC 2003.1023 {PR#00000} PARADATA <A15> [KEN CISZEWSKI]
      *               ***NEW***
      *
      ******************************************************************
           SELECT REPORT-FILE ASSIGN REPORT-PATH
                  ORGANIZATION INDEXED
                  ACCESS DYNAMIC
                  LOCK MODE AUTOMATIC WITH LOCK ON RECORD
                  RECORD KEY REPORT-KEY
                  FILE STATUS FILE-STAT.
