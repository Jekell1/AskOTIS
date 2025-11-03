      * COPYMEMBER: LIBLP/LPWSLTL
      *********************************************************
      *        LIBLP/LPWSLTL: LIENHOLDER PATH WORKERS
      *        /USR/DATA/XX/XX####/LP/LTLFILE
      *********************************************************
       01  LTL-PATH-OVERRIDE   PIC X(01)  VALUE SPACES.
       01  LTL-PATH            PIC X(35)  VALUE SPACES.
       01  LTL-ALL-PATH.
           03  LTL-ALL-BASE           PIC X(09).
           03  FILLER REDEFINES LTL-ALL-BASE.
               05  LTL-ALL-BASE-USR   PIC X(05).
               05  LTL-ALL-BASE-DIR   PIC X(04).
           03  FILLER                 PIC X(01)  VALUE "/".
           03  LTL-ALL-MACHINE        PIC X(02).
           03  FILLER                 PIC X(01)  VALUE "/".
           03  LTL-ALL-DATA           PIC X(06).
           03  FILLER                 PIC X(11)  VALUE "/LP/LTLFILE".
