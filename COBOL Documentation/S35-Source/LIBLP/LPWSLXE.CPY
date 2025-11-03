      * COPYMEMBER: LIBLP/LPWSLXE
      *********************************************************
      *        LIBLP/LPWSLXE: LXEFILE PATH WORKERS
      * LXE-PATH = /USR/DATA/R#/XX####/GB/LXEFILE
      *           |----BEG----||-MID-||-----FILE----|
      *                                  |---SQL----|
      *
      *********************************************************
       01  LXE-PATH-OVERRIDE          PIC X(01)  VALUE SPACES.

       01  LXE-PATH                   PIC X(35)  VALUE SPACES.
       01  FILLER                     REDEFINES LXE-PATH.
           03  LXE-PATH-BEG           PIC X(13).
           03  LXE-PATH-OWNST         PIC X(02).
           03  LXE-PATH-OWNBR         PIC 9(04).
           03  FILLER                 PIC X(01).
           03  LXE-PATH-FILE          PIC X(15).
           03  FILLER                REDEFINES LXE-PATH-FILE.
               05  FILLER             PIC X(03).
               05  LXE-PATH-SQL       PIC X(12).

       01  LXE-ALL-PATH.
           03  LXE-ALL-BASE           PIC X(09).
           03  FILLER                REDEFINES LXE-ALL-BASE.
               05  LXE-ALL-BASE-USR   PIC X(05).
               05  LXE-ALL-BASE-DIR   PIC X(04).
           03  FILLER                 PIC X(01)  VALUE "/".
           03  LXE-ALL-MACHINE        PIC X(02).
           03  FILLER                 PIC X(01)  VALUE "/".
           03  LXE-ALL-DATA           PIC X(06).
           03  FILLER                 PIC X(11)  VALUE "/GB/LXEFILE".
       01  LXE-SUB                    PIC 99.
