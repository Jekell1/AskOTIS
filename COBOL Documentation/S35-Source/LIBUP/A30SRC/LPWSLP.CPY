      * COPYMEMBER: LIBLP/LPWSLP
      *********************************************************
      *    LIBLP/LPWSLP: LPFILE PATH WORKERS
      *    /USR/DATA/XX/XX####/GB/LPFILE
      ******************************************************************
       01  LP-PATH-OVERRIDE          PIC X(01)   VALUE SPACES.
       01  LP-PATH                   PIC X(35)   VALUE SPACES.
       01  LP-ALL-PATH.
           03  LP-ALL-BASE           PIC X(09).
           03  FILLER                REDEFINES LP-ALL-BASE.
               05  LP-ALL-BASE-USR   PIC X(05).
               05  LP-ALL-BASE-DIR   PIC X(04).
           03  FILLER                PIC X(01)   VALUE "/".
           03  LP-ALL-MACHINE        PIC X(02).
           03  FILLER                PIC X(01)   VALUE "/".
           03  LP-ALL-DATA           PIC X(06).
           03  FILLER                REDEFINES LP-ALL-DATA.
               05  LP-ALL-STATE      PIC X(02).
               05  LP-ALL-BRNO       PIC 9(04).
           03  FILLER                PIC X(10)   VALUE "/GB/LPFILE".
