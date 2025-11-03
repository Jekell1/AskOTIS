      * COPYMEMBER: LIBLP/LPWSTZ
      *********************************************************
      *        LIBLP/LPWSTZ: TZFILE PATH WORKERS
      *        /USR/DATA/XX/LP/TZFILE
      *********************************************************
       01  TZ-PATH-OVERRIDE          PIC X(01)  VALUE SPACES.
       01  TZ-PATH                   PIC X(35)  VALUE SPACES.
       01  TZ-ALL-PATH.
           03  TZ-ALL-BASE           PIC X(09).
           03  FILLER                REDEFINES TZ-ALL-BASE.
               05  TZ-ALL-BASE-USR   PIC X(05).
               05  TZ-ALL-BASE-DIR   PIC X(04).
           03  FILLER                PIC X(01)  VALUE "/".
           03  TZ-ALL-MACHINE        PIC X(02).
           03  FILLER                PIC X(10)  VALUE "/LP/TZFILE".
