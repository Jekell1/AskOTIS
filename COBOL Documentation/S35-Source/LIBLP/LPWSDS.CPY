      * COPYMEMBER: LIBLP/LPWSDS
      *********************************************************
      *        LIBLP/LPWSDS: DSFILE PATH WORKERS
      *
      *    DS-PATH = /USR/DATA/XX/XX####/LP/DSFILE
      *              |----BEG----||-MID-||-----FILE----|
      *
      *********************************************************
       01  DS-PATH-OVERRIDE          PIC X(01)  VALUE SPACES.

       01  DS-PATH                   PIC X(35)  VALUE SPACES.
       01  FILLER                    REDEFINES DS-PATH.
           03  DS-PATH-BEG           PIC X(13).
           03  DS-PATH-OWNST         PIC X(02).
           03  DS-PATH-OWNBR         PIC 9(04).
           03  FILLER                PIC X(01).
           03  DS-PATH-FILE          PIC X(15).
           03  FILLER                REDEFINES DS-PATH-FILE.
               05  FILLER            PIC X(03).
               05  DS-PATH-SQL       PIC X(12).


       01  DS-ALL-PATH.
           03  DS-ALL-BASE           PIC X(09).
           03  FILLER                REDEFINES DS-ALL-BASE.
               05  DS-ALL-BASE-USR   PIC X(05).
               05  DS-ALL-BASE-DIR   PIC X(04).
           03  FILLER                PIC X(01)  VALUE "/".
           03  DS-ALL-MACHINE        PIC X(02).
           03  FILLER                PIC X(01)  VALUE "/".
           03  DS-ALL-DATA           PIC X(06).
           03  FILLER                PIC X(10)  VALUE "/LP/DSFILE".
