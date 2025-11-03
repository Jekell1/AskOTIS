      ******************************************************************
      *
      * IDENTIFICATION: LIBLP/LPWSLN_SQL.CPY
      * DESCRIPTION   : FILE & SQL PATH WORKERS (LNFILE)
      *
      * LN-PATH = /USR/DATA/R#/XX####/LP/LNFILE
      *           |----BEG----||-MID-||-----FILE----|
      *                                  |---SQL----|
      *
      *=================================================================
      * REV:
      * JKC 2021-0128 ***NEW***
      * BAH 2024.0130 ADDED HOLD-LN-REC
      *
      ******************************************************************
       01  LN-PATH-OVERRIDE          PIC X(01)   VALUE SPACES.

       01  LN-PATH                   PIC X(35)   VALUE SPACES.
       01  FILLER                    REDEFINES LN-PATH.
           03  LN-PATH-BEG           PIC X(13).
           03  LN-PATH-OWNST         PIC X(02).
           03  LN-PATH-OWNBR         PIC 9(04).
           03  FILLER                PIC X(01).
           03  LN-PATH-FILE          PIC X(15).
           03  FILLER                REDEFINES LN-PATH-FILE.
               05  FILLER            PIC X(03).
               05  LN-PATH-SQL       PIC X(12).

       01  LN-ALL-PATH.
           03  LN-ALL-BASE           PIC X(09).
           03  FILLER                REDEFINES LN-ALL-BASE.
               05  LN-ALL-BASE-USR   PIC X(05).
               05  LN-ALL-BASE-DIR   PIC X(04).
           03  FILLER                PIC X(01)   VALUE "/".
           03  LN-ALL-MACHINE        PIC X(02).
           03  FILLER                PIC X(01)   VALUE "/".
           03  LN-ALL-DATA           PIC X(06).
           03  FILLER                PIC X(10)   VALUE "/LP/LNFILE".

       01  LN-BI-FG                  PIC X     VALUE " ".
       01  LN-BASE-ARCHIVE           PIC X(04) VALUE " ".

      *=================================================================
      * IDEAS: PRE-COMPILE EXEC SQL WORKERS (QLN-REC) AND PUT CODE INTO
      *        END OF LIBLP/LPWSLN.CPY
      *=================================================================
       01  HOLD-LN-REC               PIC X(LN-SIZE).
