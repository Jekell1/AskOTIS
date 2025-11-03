      * COPYMEMBER: LIBLP/LPWSMP
      *********************************************************
      *        LIBLP/LPWSMP: MEMO PAYMENTS MPFILE PATH WORKERS
      *        /USR/DATA/XX/XX####/GB/MPFILE
      *********************************************************
       01  MP-PATH-OVERRIDE          PIC X(01)  VALUE SPACES.
       01  MP-PATH                   PIC X(35)  VALUE SPACES.
       01  MP-ALL-PATH.
           03  MP-ALL-BASE           PIC X(09).
           03  FILLER                REDEFINES MP-ALL-BASE.
               05  MP-ALL-BASE-USR   PIC X(05).
               05  MP-ALL-BASE-DIR   PIC X(04).
           03  FILLER                PIC X(01)  VALUE "/".
           03  MP-ALL-MACHINE        PIC X(02).
           03  FILLER                PIC X(01)  VALUE "/".
           03  MP-ALL-DATA           PIC X(06).
           03  FILLER                PIC X(10)  VALUE "/GB/MPFILE".
