      * COPYMEMBER: LIBLP/LPWSCL
      *********************************************************
      *                    CAN LOAN FILE
      *        LIBLP/LPWSCL: CLFILE PATH WORKERS
      *        /USR/DATA/XX/LP/CLFILE
      *********************************************************
       01  CL-PATH-OVERRIDE          PIC X(01)  VALUE SPACES.

       01  CL-PATH                   PIC X(35)  VALUE SPACES.
       01  FILLER                    REDEFINES CL-PATH.
           03  CL-PATH-BEG           PIC X(13).
           03  CL-PATH-FILE          PIC X(22).
           03  FILLER                REDEFINES CL-PATH-FILE.
               05  FILLER            PIC X(03).
               05  CL-PATH-SQL       PIC X(19).

       01  CL-ALL-PATH.
           03  CL-ALL-BASE           PIC X(09).
           03  FILLER                REDEFINES CL-ALL-BASE.
               05  CL-ALL-BASE-USR   PIC X(05).
               05  CL-ALL-BASE-DIR   PIC X(04).
           03  FILLER                PIC X(01)  VALUE "/".
           03  CL-ALL-MACHINE        PIC X(02).
           03  FILLER                PIC X(23)  VALUE "/LP/CLFILE".

