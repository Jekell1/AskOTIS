      ******************************************************************
      *    LIBLP/LPWSLN: LNFILE (LOAN FILE ) PATH WORKERS
      *    /USR/DATA/XX/XX####/LP/LNFILE
      ******************************************************************
       01  LN-PATH-OVERRIDE          PIC X(01)   VALUE SPACES.
       01  LN-PATH                   PIC X(35)   VALUE SPACES.
       01  LN-ALL-PATH.
           03  LN-ALL-BASE           PIC X(09).
           03  FILLER                REDEFINES LN-ALL-BASE.
               05  LN-ALL-BASE-USR   PIC X(05).
               05  LN-ALL-BASE-DIR   PIC X(04).
           03  FILLER                PIC X(01)   VALUE "/".
           03  LN-ALL-MACHINE        PIC X(02).
           03  FILLER                PIC X(01)   VALUE "/".
           03  LN-ALL-DATA           PIC X(06).
           03  FILLER                PIC X(10)   VALUE "/LP/LNFILE".

       01  LN-BI-FG                  PIC X     VALUE " ".
       01  LN-BASE-ARCHIVE           PIC X(04) VALUE " ".
