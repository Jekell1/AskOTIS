      * COPYMEMBER: LIBLP/LPWSLSL
      *********************************************************
      *        LIBLP/LPWSLSL: LSLFILE PATH WORKERS
      *        /USR/DATA/XX/XX####/LP/LSLFILE
      *********************************************************
       01  LSL-PATH-OVERRIDE          PIC X(01)  VALUE SPACES.
       01  LSL-PATH                   PIC X(35)  VALUE SPACES.
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
