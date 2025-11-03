      * COPYMEMBER: LIBLP/LPWSLX
      *********************************************************
      *        LIBLP/LPWSLX: LXFILE PATH WORKERS
      * LX-PATH = /USR/DATA/R#/XX####/GB/LXFILE
      *           |----BEG----||-MID-||-----FILE----|
      *                                  |---SQL----|
      *
      *********************************************************
       01  LX-PATH-OVERRIDE          PIC X(01)  VALUE SPACES.

       01  LX-PATH                   PIC X(35)  VALUE SPACES.
       01  FILLER                    REDEFINES LX-PATH.
           03  LX-PATH-BEG           PIC X(13).
           03  LX-PATH-OWNST         PIC X(02).
           03  LX-PATH-OWNBR         PIC 9(04).
           03  FILLER                PIC X(01).
           03  LX-PATH-FILE          PIC X(15).
           03  FILLER                REDEFINES LX-PATH-FILE.
               05  FILLER            PIC X(03).
               05  LX-PATH-SQL       PIC X(12).

       01  LX-ALL-PATH.
           03  LX-ALL-BASE           PIC X(09).
           03  FILLER                REDEFINES LX-ALL-BASE.
               05  LX-ALL-BASE-USR   PIC X(05).
               05  LX-ALL-BASE-DIR   PIC X(04).
           03  FILLER                PIC X(01)  VALUE "/".
           03  LX-ALL-MACHINE        PIC X(02).
           03  FILLER                PIC X(01)  VALUE "/".
           03  LX-ALL-DATA           PIC X(06).
           03  FILLER                PIC X(10)  VALUE "/GB/LXFILE".

      * ADDED THIS BECAUSE IT WAS USING SUB IN THE CLEAR-LX-FILE AND THAT
      * TOTALLY MESSED UP LONPF2 USING SUB IN THE TR1 FILE UPDATE ROUTINE!
       01  LX-SUB                    PIC 99.
