      * COPYMEMBER: LIBLP/LPWSFDB
      ******************************************************************
      *
      * IDENTIFICATION:  LIBLP/LPWSFDB
      *
      * DESCRIPTION   :  FDBFILE PATH WORKERS
      *
      *=================================================================
      * REV:
      * KEC 2016.0114 {PD#00003} PARADATA <A30> [JAY CISZEWSKI]
      *         ***NEW*** ADDED NEW FDB-FILE; WAS IN FD-FILE.
      *
      ******************************************************************
      *    /USR/DATA/R#/%%/FDBFILE
      *-----------------------------------------------------------------
       01  FDB-PATH-OVERRIDE         PIC X(01)   VALUE SPACES.

       01  FDB-PATH                  PIC X(40)   VALUE SPACES.
       01  FILLER                    REDEFINES FDB-PATH.
           03  FDB-PATH-BEG           PIC X(13).
           03  FDB-PATH-FILE          PIC X(22).
           03  FILLER                REDEFINES FDB-PATH-FILE.
               05  FILLER            PIC X(03).
               05  FDB-PATH-SQL       PIC X(19).

       01  FDB-ALL-PATH.
           03  FDB-ALL-BASE          PIC X(09).
           03  FILLER                REDEFINES FDB-ALL-BASE.
               05  FDB-ALL-BASE-USR  PIC X(05).
               05  FDB-ALL-BASE-DIR  PIC X(04).
           03  FILLER                PIC X(01)   VALUE "/".
           03  FDB-ALL-MACHINE       PIC X(02).
           03  FILLER                PIC X(28)   VALUE "/LP/FDBFILE".

      *=================================================================
       01  LPWSFDB-SUB               PIC 9(03)   COMP-3.
