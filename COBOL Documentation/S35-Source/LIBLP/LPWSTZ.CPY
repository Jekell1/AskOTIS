      * COPYMEMBER: LIBLP/LPWSTZ
      *********************************************************
      *        LIBLP/LPWSTZ: TZFILE PATH WORKERS
      *        /USR/DATA/XX/LP/TZFILE
      *********************************************************
       01  TZ-PATH-OVERRIDE          PIC X(01)  VALUE SPACES.
       01  TZ-PATH                   PIC X(35)  VALUE SPACES.
       01  FILLER                    REDEFINES TZ-PATH.
           03  TZ-PATH-BEG           PIC X(13).
           03  TZ-PATH-FILE          PIC X(22).
           03  FILLER                REDEFINES TZ-PATH-FILE.
               05  FILLER            PIC X(03).
               05  TZ-PATH-SQL       PIC X(19).


       01  TZ-ALL-PATH.
           03  TZ-ALL-BASE           PIC X(09).
           03  FILLER                REDEFINES TZ-ALL-BASE.
               05  TZ-ALL-BASE-USR   PIC X(05).
               05  TZ-ALL-BASE-DIR   PIC X(04).
           03  FILLER                PIC X(01)  VALUE "/".
           03  TZ-ALL-MACHINE        PIC X(02).
           03  FILLER                PIC X(10)  VALUE "/LP/TZFILE".
