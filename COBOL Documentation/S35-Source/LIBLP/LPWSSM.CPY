      * COPYMEMBER: LIBLP/LPWSSM
      *********************************************************
      *        LIBLP/LPWSSM: SMFILE PATH WORKERS
      *        /USR/DATA/XX/LP/SMFILE
      *
      *REV:
      *********************************************************
       01  SM-PATH-OVERRIDE          PIC X(01)  VALUE SPACES.
       01  SM-PATH                   PIC X(35)  VALUE SPACES.
       01  FILLER                    REDEFINES SM-PATH.
           03  SM-PATH-BEG           PIC X(13).
           03  SM-PATH-FILE          PIC X(22).
           03  FILLER                REDEFINES SM-PATH-FILE.
               05  FILLER            PIC X(03).
               05  SM-PATH-SQL       PIC X(19).

       01  SM-ALL-PATH.
           03  SM-ALL-BASE           PIC X(09).
           03  FILLER                REDEFINES SM-ALL-BASE.
               05  SM-ALL-BASE-USR   PIC X(05).
               05  SM-ALL-BASE-DIR   PIC X(04).
           03  FILLER                PIC X(01)  VALUE "/".
           03  SM-ALL-MACHINE        PIC X(02).
           03  FILLER                PIC X(23)  VALUE "/LP/SMFILE".

       01  WS-SM-LTOUCH-TIME         PIC 9(8).
       01  FILLER REDEFINES WS-SM-LTOUCH-TIME.
           03  WS-SM-LTOUCH-HHMM     PIC 9(4).
           03  FILLER                PIC 9(4).
