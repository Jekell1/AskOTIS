      * COPYMEMBER: LIBGL/GLWSGL
      *********************************************************
      *        LIBGL/GLWSGL: GLFILE PATH WORKERS
      *        /USR/DATA/XX/GL/GLFILE
      *********************************************************
       01  GL-PATH-OVERRIDE          PIC X(01)   VALUE SPACES.
       01  GL-PATH                   PIC X(35)   VALUE SPACES.
       01  GL-ALL-PATH.
           03  GL-ALL-BASE           PIC X(09).
           03  FILLER                REDEFINES GL-ALL-BASE.
               05  GL-ALL-BASE-USR   PIC X(05).
               05  GL-ALL-BASE-DIR   PIC X(04).
           03  FILLER                PIC X(01)   VALUE "/".
           03  GL-ALL-MACHINE        PIC X(02).
           03  FILLER                PIC X(23)   VALUE "/GL/GLFILE".
