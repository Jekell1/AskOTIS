      * COPYMEMBER: LIBLP/LPWSLSP
      *********************************************************
      *        LIBLP/LPWSLSP: LSPFILE PATH WORKERS
      *        /USR/DATA/XX/XX####/LP/LSPFILE
      *********************************************************
       01  LSP-PATH-OVERRIDE          PIC X(01)  VALUE SPACES.
       01  LSP-PATH                   PIC X(35)  VALUE SPACES.
       01  LSP-ALL-PATH.
           03  LSP-ALL-BASE          PIC X(09).
           03  FILLER                REDEFINES LSP-ALL-BASE.
               05  LSP-ALL-BASE-USR  PIC X(05).
               05  LSP-ALL-BASE-DIR  PIC X(04).
           03  FILLER                PIC X(01)  VALUE "/".
           03  LSP-ALL-MACHINE       PIC X(02).
           03  FILLER                PIC X(01)  VALUE "/".
           03  LSP-ALL-DATA          PIC X(06).
           03  FILLER                PIC X(11)  VALUE "/LP/LSPFILE".
