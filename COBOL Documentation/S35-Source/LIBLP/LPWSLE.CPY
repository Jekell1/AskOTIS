      * COPYMEMBER: LIBLP/LPWSLE
      *********************************************************
      *        LIBLP/LPWSLE: LEFILE PATH WORKERS
      *    LE-PATH = /USR/DATA/XX/XX####/LP/LEFILE
      *              |----BEG----||-MID-||-----FILE----|
      *
      *********************************************************
       01  LE-PATH-OVERRIDE          PIC X(01)  VALUE SPACES.

       01  LE-PATH                   PIC X(35)  VALUE SPACES.
       01  FILLER                    REDEFINES LE-PATH.
           03  LE-PATH-BEG           PIC X(13).
           03  LE-PATH-OWNST         PIC X(02).
           03  LE-PATH-OWNBR         PIC 9(04).
           03  FILLER                PIC X(01).
           03  LE-PATH-FILE          PIC X(15).
           03  FILLER                REDEFINES LE-PATH-FILE.
               05  FILLER            PIC X(03).
               05  LE-PATH-SQL       PIC X(12).

       01  LE-ALL-PATH.
           03  LE-ALL-BASE           PIC X(09).
           03  FILLER                REDEFINES LE-ALL-BASE.
               05  LE-ALL-BASE-USR   PIC X(05).
               05  LE-ALL-BASE-DIR   PIC X(04).
           03  FILLER                PIC X(01)  VALUE "/".
           03  LE-ALL-MACHINE        PIC X(02).
           03  FILLER                PIC X(01)  VALUE "/".
           03  LE-ALL-DATA           PIC X(06).
           03  FILLER                PIC X(10)  VALUE "/LP/LEFILE".

       01  LESUB                     PIC 9 VALUE ZEROS.
