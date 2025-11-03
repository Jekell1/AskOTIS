      * COPYMEMBER: LIBLP/LPWSST
      *********************************************************
      *        LIBLP/LPWSST: STFILE PATH WORKERS
      *        /USR/DATA/XX/LP/STFILE
      *********************************************************
       01  ST-PATH-OVERRIDE          PIC X(01)  VALUE SPACES.
       01  ST-PATH                   PIC X(35)  VALUE SPACES.
       01  FILLER                    REDEFINES ST-PATH.
           03  ST-PATH-BEG           PIC X(13).
           03  ST-PATH-FILE          PIC X(22).
           03  FILLER                REDEFINES ST-PATH-FILE.
               05  FILLER            PIC X(03).
               05  ST-PATH-SQL       PIC X(19).

       01  ST-ALL-PATH.
           03  ST-ALL-BASE           PIC X(09).
           03  FILLER                REDEFINES ST-ALL-BASE.
               05  ST-ALL-BASE-USR   PIC X(05).
               05  ST-ALL-BASE-DIR   PIC X(04).
           03  FILLER                PIC X(01)  VALUE "/".
           03  ST-ALL-MACHINE        PIC X(02).
           03  FILLER                PIC X(23)  VALUE "/LP/STFILE".
