      * COPYMEMBER: LIBLP/LPWSRB
      *********************************************************
      *        LIBLP/LPWSRB: RBFILE PATH WORKERS
      *        /USR/DATA/XX/LP/RBFILE
      *********************************************************
       01  RB-PATH-OVERRIDE          PIC X(01)  VALUE SPACES.
       01  RB-PATH                   PIC X(35)  VALUE SPACES.
       01  FILLER                    REDEFINES RB-PATH.
           03  RB-PATH-BEG           PIC X(13).
           03  RB-PATH-FILE          PIC X(22).
           03  FILLER                REDEFINES RB-PATH-FILE.
               05  FILLER            PIC X(03).
               05  RB-PATH-SQL       PIC X(19).
 
       01  RB-ALL-PATH.
           03  RB-ALL-BASE           PIC X(09).
           03  FILLER                REDEFINES RB-ALL-BASE.
               05  RB-ALL-BASE-USR   PIC X(05).
               05  RB-ALL-BASE-DIR   PIC X(04).
           03  FILLER                PIC X(01)  VALUE "/".
           03  RB-ALL-MACHINE        PIC X(02).
           03  FILLER                PIC X(23)  VALUE "/LP/RBFILE".
