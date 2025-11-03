      * COPYMEMBER: LIBLP/LPWSIX
      *********************************************************
      *    LIBLP/LPWSIX: IXFILE PATH WORKERS
      *    /USR/DATA/XX/XX####/LP/IXFILE
      ******************************************************************
       01  IX-PATH-OVERRIDE          PIC X(01)   VALUE SPACES.
       01  IX-PATH                   PIC X(35)   VALUE SPACES.
       01  IX-ALL-PATH.
           03  IX-ALL-BASE           PIC X(09).
           03  FILLER                REDEFINES IX-ALL-BASE.
               05  IX-ALL-BASE-USR   PIC X(05).
               05  IX-ALL-BASE-DIR   PIC X(04).
           03  FILLER                PIC X(01)   VALUE "/".
           03  IX-ALL-MACHINE        PIC X(02).
           03  FILLER                PIC X(01)   VALUE "/".
           03  IX-ALL-DATA           PIC X(06).
           03  FILLER                PIC X(10)   VALUE "/LP/IXFILE".
