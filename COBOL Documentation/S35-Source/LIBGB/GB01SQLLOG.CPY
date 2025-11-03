      * COPYMEMBER: LIBGB/GB01SQLLOG
      ******************************************************************
      *
      * IDENTIFICATION: LIBGB/GB01SQLLOG.CPY
      *
      * DESCRIPTION   : FILE 01 LEVEL FOR SQLLOG
      *
      *=================================================================
      * REV:
      * JKC 2024-1009 ***NEW***
      *
      ******************************************************************
       78  SQLLOG-SIZE                               VALUE 276.
       01  SQLLOG-REC.
           03  SQLLOG-DATE-YYYY-MM-DD  PIC 9999/99/99.
           03  FILLER                  PIC X(1).
           03  SQLLOG-TIME             PIC 99/99/99.
           03  FILLER                  PIC X(1).
           03  SQLLOG-PROGRAM-NAME     PIC X(40).
           03  FILLER                  PIC X(1).
           03  SQLLOG-PROGRAM-RANGES   PIC X(18).
           03  FILLER                  PIC X(1).
           03  SQLLOG-STATUS-CODE      PIC X(4).
           03  FILLER                  PIC X(1).
           03  SQLLOG-DESCRIPTION      PIC X(150).
           03  FILLER                  PIC X(1).
           03  SQLLOG-DB-NAME          PIC X(40).

