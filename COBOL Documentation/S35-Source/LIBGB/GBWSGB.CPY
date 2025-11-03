      * COPYMEMBER: LIBGB/GBWSGB
      *********************************************************
      *    LIBGB/GBWSGB: GBFILE PATH WORKERS
      *    GB-PATH = /USR/DATA/XX/XX####/GB/GBFILE
      *              |----BEG----||-MID-||-----FILE----|
      *
      ******************************************************************
       01  GB-PATH-OVERRIDE          PIC X(01)   VALUE SPACES.

       01  GB-PATH                   PIC X(35)   VALUE SPACES.
       01  FILLER                    REDEFINES GB-PATH.
           03  GB-PATH-BEG           PIC X(13).
           03  GB-PATH-OWNST         PIC X(02).
           03  GB-PATH-OWNBR         PIC 9(04).
           03  FILLER                PIC X(01).
           03  GB-PATH-FILE          PIC X(15).
           03  FILLER                REDEFINES GB-PATH-FILE.
               05  FILLER            PIC X(03).
               05  GB-PATH-SQL       PIC X(12).

       01  GB-ALL-PATH.
           03  GB-ALL-BASE           PIC X(09).
           03  FILLER                REDEFINES GB-ALL-BASE.
               05  GB-ALL-BASE-USR   PIC X(05).
               05  GB-ALL-BASE-DIR   PIC X(04).
           03  FILLER                PIC X(01)   VALUE "/".
           03  GB-ALL-MACHINE        PIC X(02).
           03  FILLER                PIC X(01)   VALUE "/".
           03  GB-ALL-DATA           PIC X(06).
           03  FILLER                PIC X(10)   VALUE "/GB/GBFILE".
