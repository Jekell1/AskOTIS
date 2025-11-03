      * COPYMEMBER: LIBLP/LPWSES
      *********************************************************
      *        LIBLP/LPWSES: ESCROW FILE PATH WORKERS
      *        /USR/DATA/XX/XX####/LP/ESFILE
      *********************************************************
       01  ES-PATH-OVERRIDE          PIC X(01)  VALUE SPACES.
       01  ES-PATH                   PIC X(35)  VALUE SPACES.
       01  FILLER                    REDEFINES ES-PATH.
           03  ES-PATH-BEG           PIC X(13).
           03  ES-PATH-OWNST         PIC X(02).
           03  ES-PATH-OWNBR         PIC 9(04).
           03  FILLER                PIC X(01).
           03  ES-PATH-FILE          PIC X(15).
           03  FILLER                REDEFINES ES-PATH-FILE.
               05  FILLER            PIC X(03).
               05  ES-PATH-SQL       PIC X(12).


       01  ES-ALL-PATH.
           03  ES-ALL-BASE           PIC X(09).
           03  FILLER                REDEFINES ES-ALL-BASE.
               05  ES-ALL-BASE-USR   PIC X(05).
               05  ES-ALL-BASE-DIR   PIC X(04).
           03  FILLER                PIC X(01)  VALUE "/".
           03  ES-ALL-MACHINE        PIC X(02).
           03  FILLER                PIC X(01)  VALUE "/".
           03  ES-ALL-DATA           PIC X(06).
           03  FILLER                PIC X(10)  VALUE "/LP/ESFILE".
