      * COPYMEMBER: LIBGB/GBWSSQLLOG
      ******************************************************************
      *
      * IDENTIFICATION: LIBGB/GBWSSQLLOG.CPY
      *
      * DESCRIPTION   : FILE WORKERS FOR SQLLOG
      *
      *=================================================================
      * REV:
      * JKC 2024-1009 ***NEW***
      *
      ******************************************************************
       01  SQLLOG-IFN                  PIC X(6)      VALUE "SQLLOG".

       01  SQLLOG-PATH                 PIC X(40)     VALUE SPACES.
       01  FILLER                      REDEFINES SQLLOG-PATH.
           03  FILLER                  PIC X(13).
           03  SQLLOG-PATH-LG          PIC X(2).
           03  SQLLOG-PATH-SLASH       PIC X(1).
           03  SQLLOG-PATH-FILENAME    PIC X(7).
           03  SQLLOG-PATH-DATE        PIC 9(6).

       01  SQLLOG-DATE-YYMMDD          PIC 9(8)      VALUE ZEROES.
       01  SQLLOG-MESS                 PIC X(150)    VALUE SPACES.
       01  SQLLOG-PROGRAM              PIC X(22)     VALUE SPACES.
       01  SQLLOG-RANGES               PIC X(18)     VALUE SPACES.
       01  SQLLOG-STATUS               PIC X(4)      VALUE SPACES.

