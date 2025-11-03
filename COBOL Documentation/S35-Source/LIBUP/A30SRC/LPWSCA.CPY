      * COPYMEMBER: LIBLP/LPWSCA
      ******************************************************************
      *    LIBLP/LPWSCA: CAFILE PATH WORKERS
      *    /USR/DATA/XX/XX####/LP/CAFILE
      ******************************************************************
       01  CA-PATH-OVERRIDE          PIC X(01)   VALUE SPACES.
       01  CA-PATH                   PIC X(35)   VALUE SPACES.
       01  CA-ALL-PATH.
           03  CA-ALL-BASE           PIC X(09).
           03  FILLER                REDEFINES CA-ALL-BASE.
               05  CA-ALL-BASE-USR   PIC X(05).
               05  CA-ALL-BASE-DIR   PIC X(04).
           03  FILLER                PIC X(01)   VALUE "/".
           03  CA-ALL-MACHINE        PIC X(02).
           03  FILLER                PIC X(01)   VALUE "/".
           03  CA-ALL-DATA           PIC X(06).
           03  FILLER                PIC X(10)   VALUE "/LP/CAFILE".
