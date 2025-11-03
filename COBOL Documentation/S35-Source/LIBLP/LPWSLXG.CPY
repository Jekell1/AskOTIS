      * COPYMEMBER: LIBLP/LPWSLXG
      *********************************************************
      *        LIBLP/LPWSLXG: LXGFILE PATH WORKERS
      * LXG-PATH = /USR/DATA/R#/XX####/GB/LXGFILE
      *           |----BEG----||-MID-||-----FILE----|
      *                                  |---SQL----|
      *
      *********************************************************
       01  LXG-PATH-OVERRIDE          PIC X(01)  VALUE SPACES.

       01  LXG-PATH                   PIC X(35)  VALUE SPACES.
       01  FILLER                     REDEFINES LXG-PATH.
           03  LXG-PATH-BEG           PIC X(13).
           03  LXG-PATH-OWNST         PIC X(02).
           03  LXG-PATH-OWNBR         PIC 9(04).
           03  FILLER                 PIC X(01).
           03  LXG-PATH-FILE          PIC X(15).
           03  FILLER                REDEFINES LXG-PATH-FILE.
               05  FILLER             PIC X(03).
               05  LXG-PATH-SQL       PIC X(12).

       01  LXG-ALL-PATH.
           03  LXG-ALL-BASE           PIC X(09).
           03  FILLER                REDEFINES LXG-ALL-BASE.
               05  LXG-ALL-BASE-USR   PIC X(05).
               05  LXG-ALL-BASE-DIR   PIC X(04).
           03  FILLER                 PIC X(01)  VALUE "/".
           03  LXG-ALL-MACHINE        PIC X(02).
           03  FILLER                 PIC X(01)  VALUE "/".
           03  LXG-ALL-DATA           PIC X(06).
           03  FILLER                 PIC X(11)  VALUE "/GB/LXGFILE".
       01  LXG-SUB                    PIC 99.
