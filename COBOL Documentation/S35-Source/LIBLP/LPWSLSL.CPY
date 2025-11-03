      * COPYMEMBER: LIBLP/LPWSLSL
      *********************************************************
      *        LIBLP/LPWSLSL: LSLFILE PATH WORKERS
      *        /USR/DATA/XX/XX####/LP/LSLFILE
      *********************************************************
       01  LSL-PATH-OVERRIDE          PIC X(01)  VALUE SPACES.
       01  LSL-PATH                   PIC X(35)  VALUE SPACES.
       01  FILLER                    REDEFINES LSL-PATH.
           03  LSL-PATH-BEG           PIC X(13).
           03  LSL-PATH-OWNST         PIC X(02).
           03  LSL-PATH-OWNBR         PIC 9(04).
           03  FILLER                PIC X(01).
           03  LSL-PATH-FILE          PIC X(15).
           03  FILLER                REDEFINES LSL-PATH-FILE.
               05  FILLER            PIC X(03).
               05  LSL-PATH-SQL       PIC X(12).


       01  LSL-ALL-PATH.
           03  LSL-ALL-BASE          PIC X(09).
           03  FILLER                REDEFINES LSL-ALL-BASE.
               05  LSL-ALL-BASE-USR  PIC X(05).
               05  LSL-ALL-BASE-DIR  PIC X(04).
           03  FILLER                PIC X(01)  VALUE "/".
           03  LSL-ALL-MACHINE       PIC X(02).
           03  FILLER                PIC X(01)  VALUE "/".
           03  LSL-ALL-DATA          PIC X(06).
           03  FILLER                PIC X(11)  VALUE "/LP/LSLFILE".

       01  LSLSUB                    PIC 99.
