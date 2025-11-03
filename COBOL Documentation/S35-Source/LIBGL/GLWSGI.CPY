      * COPYMEMBER: LIBGL/GLWSGI
      *********************************************************
      *        LIBGL/GLWSGI: GIFILE PATH WORKERS
      *        /USR/DATA/XX/GL/GIFILE
      *********************************************************
       01  GI-PATH-OVERRIDE          PIC X(01)   VALUE SPACES.

       01  GI-PATH                   PIC X(35)   VALUE SPACES.
       01  FILLER                    REDEFINES GI-PATH.
           03  GI-PATH-BEG           PIC X(13).
           03  GI-PATH-FILE          PIC X(22).
           03  FILLER                REDEFINES GI-PATH-FILE.
               05  FILLER            PIC X(03).
               05  GI-PATH-SQL       PIC X(19).


       01  GI-ALL-PATH.
           03  GI-ALL-BASE           PIC X(09).
           03  FILLER                REDEFINES GI-ALL-BASE.
               05  GI-ALL-BASE-USR   PIC X(05).
               05  GI-ALL-BASE-DIR   PIC X(04).
           03  FILLER                PIC X(01)   VALUE "/".
           03  GI-ALL-MACHINE        PIC X(02).
           03  FILLER                PIC X(23)   VALUE "/GL/GIFILE".
