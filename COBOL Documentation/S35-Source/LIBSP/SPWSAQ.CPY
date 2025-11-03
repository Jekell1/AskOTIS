      * COPYMEMBER: LIBSP/SPWSAQ
      ******************************************************************
      *    LIBSP/SPWSAQ: AQFILE PATH WORKERS
      *
      * AQ-PATH = /USR/DATA/R#/SSBBBB/%%/%%FILE
      *
      ******************************************************************
       01  AQ-PATH-OVERRIDE          PIC X(01)   VALUE SPACES.
       01  AQ-PATH                   PIC X(35)   VALUE SPACES.
       01  FILLER                    REDEFINES AQ-PATH.
           03  AQ-PATH-BEG           PIC X(13).
           03  AQ-PATH-OWNST         PIC X(02).
           03  AQ-PATH-OWNBR         PIC 9(04).
           03  FILLER                PIC X(01).
           03  AQ-PATH-FILE          PIC X(15).
           03  FILLER                REDEFINES AQ-PATH-FILE.
               05  FILLER            PIC X(03).
               05  AQ-PATH-SQL       PIC X(12).

       01  AQ-ALL-PATH.
           03  AQ-ALL-BASE           PIC X(09).
           03  FILLER                REDEFINES AQ-ALL-BASE.
               05  AQ-ALL-BASE-USR   PIC X(05).
               05  AQ-ALL-BASE-DIR   PIC X(04).
           03  FILLER                PIC X(01)   VALUE "/".
           03  AQ-ALL-MACHINE        PIC X(02).
           03  FILLER                PIC X(01)   VALUE "/".
           03  AQ-ALL-DATA           PIC X(06).
           03  FILLER                PIC X(10)   VALUE "/SP/AQFILE".
