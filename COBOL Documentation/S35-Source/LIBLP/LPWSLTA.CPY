      * COPYMEMBER: LIBLP/LPWSLTA
      *********************************************************
      *        LIBLP/LPWSLTA: LIENHOLDER PATH WORKERS
      *        /USR/DATA/XX/XX####/LP/LTAFILE
      *********************************************************
       01  LTA-PATH-OVERRIDE   PIC X(01)  VALUE SPACES.
       01  LTA-PATH            PIC X(35)  VALUE SPACES.
       01  FILLER                    REDEFINES LTA-PATH.
           03  LTA-PATH-BEG           PIC X(13).
           03  LTA-PATH-OWNST         PIC X(02).
           03  LTA-PATH-OWNBR         PIC 9(04).
           03  FILLER                PIC X(01).
           03  LTA-PATH-FILE          PIC X(15).
           03  FILLER                REDEFINES LTA-PATH-FILE.
               05  FILLER            PIC X(03).
               05  LTA-PATH-SQL       PIC X(12).

       01  LTA-ALL-PATH.
           03  LTA-ALL-BASE           PIC X(09).
           03  FILLER REDEFINES LTA-ALL-BASE.
               05  LTA-ALL-BASE-USR   PIC X(05).
               05  LTA-ALL-BASE-DIR   PIC X(04).
           03  FILLER                 PIC X(01)  VALUE "/".
           03  LTA-ALL-MACHINE        PIC X(02).
           03  FILLER                 PIC X(01)  VALUE "/".
           03  LTA-ALL-DATA           PIC X(06).
           03  FILLER                 PIC X(11)  VALUE "/LP/LTAFILE".
