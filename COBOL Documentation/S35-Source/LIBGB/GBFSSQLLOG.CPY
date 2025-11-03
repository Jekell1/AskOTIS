      * COPYMEMBER: LIBGB/GBFSSQLLOG
      ******************************************************************
      *
      * IDENTIFICATION: LIBGB/GBFSSQLLOG.CPY
      *
      * DESCRIPTION   : FILE SELECT FOR SQLLOG
      *
      *=================================================================
      * REV:
      * JKC 2024-1009 ***NEW***
      *
      ******************************************************************
           SELECT SQLLOG-FILE ASSIGN OUTPUT SQLLOG-PATH
                  ORGANIZATION LINE SEQUENTIAL
                  LOCK MODE AUTOMATIC WITH LOCK ON RECORD
                  FILE STATUS FILE-STAT.
