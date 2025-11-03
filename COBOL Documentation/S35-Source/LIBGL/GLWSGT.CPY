      * COPYMEMBER: LIBGL/GLWSGT
      *********************************************************
      *        LIBGL/GLWSGT: GTFILE PATH WORKERS
      *        /USR/DATA/XX/GL/GTFILE
      *********************************************************
       01  GT-PATH-OVERRIDE          PIC X(01)   VALUE SPACES.
       01  GT-PATH                   PIC X(35)   VALUE SPACES.
       01  FILLER                    REDEFINES GT-PATH.
           03  GT-PATH-BEG           PIC X(13).
           03  GT-PATH-FILE          PIC X(22).
           03  FILLER                REDEFINES GT-PATH-FILE.
               05  FILLER            PIC X(03).
               05  GT-PATH-SQL       PIC X(19).
 
       01  GT-ALL-PATH.
           03  GT-ALL-BASE           PIC X(09).
           03  FILLER                REDEFINES GT-ALL-BASE.
               05  GT-ALL-BASE-USR   PIC X(05).
               05  GT-ALL-BASE-DIR   PIC X(04).
           03  FILLER                PIC X(01)   VALUE "/".
           03  GT-ALL-MACHINE        PIC X(02).
           03  FILLER                PIC X(23)   VALUE "/GL/GTFILE".

