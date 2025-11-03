      * COPYMEMBER: LIBGB/GBWSSN
      *********************************************************
      *        LIBGB/GBWSSN: SNFILE PATH WORKERS
      *        /USR/DATA/XX/GB/SNFILE
      *********************************************************
       01  SN-PATH-OVERRIDE          PIC X(01)   VALUE SPACES.
       01  SN-PATH                   PIC X(35)   VALUE SPACES.
       01  FILLER                    REDEFINES SN-PATH.
           03  SN-PATH-BEG           PIC X(13).
           03  SN-PATH-FILE          PIC X(22).
           03  FILLER                REDEFINES SN-PATH-FILE.
               05  FILLER            PIC X(03).
               05  SN-PATH-SQL       PIC X(19).
 

       01  SN-ALL-PATH.
           03  SN-ALL-BASE           PIC X(09).
           03  FILLER                REDEFINES SN-ALL-BASE.
               05  SN-ALL-BASE-USR   PIC X(05).
               05  SN-ALL-BASE-DIR   PIC X(04).
           03  FILLER                PIC X(01)   VALUE "/".
           03  SN-ALL-MACHINE        PIC X(02).
           03  FILLER                PIC X(23)   VALUE "/GB/SNFILE".
