      * COPYMEMBER: LIBLP/LPWSBW
      *********************************************************
      *    LIBLP/LPWSBW: BWFILE PATH WORKERS
      *    /USR/DATA/XX/XX####/GB/BWFILE
      ******************************************************************
       01  BW-PATH-OVERRIDE          PIC X(01)   VALUE SPACES.
       01  BW-PATH                   PIC X(35)   VALUE SPACES.
       01  BW-ALL-PATH.
           03  BW-ALL-BASE           PIC X(09).
           03  FILLER                REDEFINES BW-ALL-BASE.
               05  BW-ALL-BASE-USR   PIC X(05).
               05  BW-ALL-BASE-DIR   PIC X(04).
           03  FILLER                PIC X(01)   VALUE "/".
           03  BW-ALL-MACHINE        PIC X(02).
           03  FILLER                PIC X(01)   VALUE "/".
           03  BW-ALL-DATA           PIC X(06).
           03  FILLER                PIC X(10)   VALUE "/GB/BWFILE".
