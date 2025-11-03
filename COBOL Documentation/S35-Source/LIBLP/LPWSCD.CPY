      * COPYMEMBER: LIBLP/LPWSCD
      *********************************************************
      *    LIBLP/LPWSCD: CDFILE PATH WORKERS
      *    /USR/DATA/XX/LP/CDFILE
      ******************************************************************
       01  CD-PATH-OVERRIDE          PIC X(01)   VALUE SPACES.

       01  CD-PATH                   PIC X(35)   VALUE SPACES.
       01  FILLER                    REDEFINES CD-PATH.
           03  CD-PATH-BEG           PIC X(13).
           03  CD-PATH-FILE          PIC X(22).
           03  FILLER                REDEFINES CD-PATH-FILE.
               05  FILLER            PIC X(03).
               05  CD-PATH-SQL       PIC X(19).

       01  CD-ALL-PATH.
           03  CD-ALL-BASE           PIC X(09).
           03  FILLER                REDEFINES CD-ALL-BASE.
               05  CD-ALL-BASE-USR   PIC X(05).
               05  CD-ALL-BASE-DIR   PIC X(04).
           03  FILLER                PIC X(01)   VALUE "/".
           03  CD-ALL-MACHINE        PIC X(02).
           03  FILLER                PIC X(23)   VALUE "/LP/CDFILE".
