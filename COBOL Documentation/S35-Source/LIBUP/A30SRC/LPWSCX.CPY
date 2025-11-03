      * COPYMEMBER: LIBLP/LPWSCX
      ******************************************************************
      *    LIBLP/LPWSCX: CXFILE PATH WORKERS
      *    /USR/DATA/XX/XX####/LP/CXFILE
      ******************************************************************
       01  CX-PATH-OVERRIDE          PIC X(01)   VALUE SPACES.
       01  CX-PATH                   PIC X(35)   VALUE SPACES.
       01  CX-ALL-PATH.
           03  CX-ALL-BASE           PIC X(09).
           03  FILLER                REDEFINES CX-ALL-BASE.
               05  CX-ALL-BASE-USR   PIC X(05).
               05  CX-ALL-BASE-DIR   PIC X(04).
           03  FILLER                PIC X(01)   VALUE "/".
           03  CX-ALL-MACHINE        PIC X(02).
           03  FILLER                PIC X(01)   VALUE "/".
           03  CX-ALL-DATA           PIC X(06).
           03  FILLER                PIC X(10)   VALUE "/LP/CXFILE".

