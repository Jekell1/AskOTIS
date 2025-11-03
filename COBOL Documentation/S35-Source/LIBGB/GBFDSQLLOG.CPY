      * COPYMEMBER: LIBGB/GBFDSQLLOG
      ******************************************************************
      *
      * IDENTIFICATION: LIBGB/GBFDSQLLOG.CPY
      *
      * DESCRIPTION   : FILE DESCRIPTOR FOR SQLLOG
      *
      *=================================================================
      * REV:
      * JKC 2024-1009 ***NEW***
      *
      ******************************************************************
       FD  SQLLOG-FILE
           RECORD IS VARYING IN SIZE FROM 10 TO 300 CHARACTERS
           LABEL RECORDS ARE STANDARD.
       COPY "LIBGB/GB01SQLLOG.CPY".

