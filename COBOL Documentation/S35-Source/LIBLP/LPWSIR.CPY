      * COPYMEMBER: LIBLP/LPWSIR
      *********************************************************
      *        LIBLP/LPWSIR: IRFILE PATH WORKERS
      *        /USR/DATA/XX/LP/IRFILE
      *********************************************************
       01  IR-PATH-OVERRIDE          PIC X(01)  VALUE SPACES.
       01  IR-PATH                   PIC X(35)  VALUE SPACES.
       01  FILLER                    REDEFINES IR-PATH.
           03  IR-PATH-BEG           PIC X(13).
           03  IR-PATH-FILE          PIC X(22).
           03  FILLER                REDEFINES IR-PATH-FILE.
               05  FILLER            PIC X(03).
               05  IR-PATH-SQL       PIC X(19).

       01  IR-ALL-PATH.
           03  IR-ALL-BASE           PIC X(09).
           03  FILLER                REDEFINES IR-ALL-BASE.
               05  IR-ALL-BASE-USR   PIC X(05).
               05  IR-ALL-BASE-DIR   PIC X(04).
           03  FILLER                PIC X(01)  VALUE "/".
           03  IR-ALL-MACHINE        PIC X(02).
           03  FILLER                PIC X(23)  VALUE "/LP/IRFILE".
