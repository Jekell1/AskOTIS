      * COPYMEMBER: LIBGL/GLWSAL
      *********************************************************
      *        LIBGL/GLWSAL: ALFILE PATH WORKERS
      *        /USR/DATA/XX/GL/ALFILE
      *********************************************************
       01  AL-PATH-OVERRIDE          PIC X(01)   VALUE SPACES.
       01  AL-PATH                   PIC X(35)   VALUE SPACES.
       01  FILLER                    REDEFINES AL-PATH.
           03  AL-PATH-BEG           PIC X(13).
           03  AL-PATH-FILE          PIC X(22).
           03  FILLER                REDEFINES AL-PATH-FILE.
               05  FILLER            PIC X(03).
               05  AL-PATH-SQL       PIC X(19).
 
       01  AL-ALL-PATH.
           03  AL-ALL-BASE           PIC X(09).
           03  FILLER                REDEFINES AL-ALL-BASE.
               05  AL-ALL-BASE-USR   PIC X(05).
               05  AL-ALL-BASE-DIR   PIC X(04).
           03  FILLER                PIC X(01)   VALUE "/".
           03  AL-ALL-MACHINE        PIC X(02).
           03  FILLER                PIC X(23)   VALUE "/GL/ALFILE".
