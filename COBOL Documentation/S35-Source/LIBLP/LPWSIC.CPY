      * COPYMEMBER: LIBLP/LPWSIC
      ******************************************************************
      *
      * LIBLP/LPWSIC: ICFILE PATH WORKERS
      *
      * IC-PATH = /USR/DATA/R#/XX####/LP/ICFILE
      *           |----BEG----||-MID-||-----FILE----|
      *                                  |---SQL----|
      *
      ******************************************************************
       01  IC-PATH-OVERRIDE          PIC X(01)  VALUE SPACES.

       01  IC-PATH                   PIC X(35)  VALUE SPACES.
       01  FILLER                    REDEFINES IC-PATH.
           03  IC-PATH-BEG           PIC X(13).
           03  IC-PATH-OWNST         PIC X(02).
           03  IC-PATH-OWNBR         PIC 9(04).
           03  FILLER                PIC X(01).
           03  IC-PATH-FILE          PIC X(15).
           03  FILLER                REDEFINES IC-PATH-FILE.
               05  FILLER            PIC X(03).
               05  IC-PATH-SQL       PIC X(12).

       01  IC-ALL-PATH.
           03  IC-ALL-BASE           PIC X(09).
           03  FILLER                REDEFINES IC-ALL-BASE.
               05  IC-ALL-BASE-USR   PIC X(05).
               05  IC-ALL-BASE-DIR   PIC X(04).
           03  FILLER                PIC X(01)  VALUE "/".
           03  IC-ALL-MACHINE        PIC X(02).
           03  FILLER                PIC X(01)  VALUE "/".
           03  IC-ALL-DATA           PIC X(06).
           03  FILLER                PIC X(10)  VALUE "/LP/ICFILE".
