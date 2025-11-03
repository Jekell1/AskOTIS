      * COPYMEMBER: LIBGL/GLWSJH
      *********************************************************
      *        LIBGL/GLWSJH: JHFILE PATH WORKERS
      *        /USR/DATA/XX/GL/JHFILE
      *********************************************************
       01  JH-PATH-OVERRIDE          PIC X(01)   VALUE SPACES.
       01  JH-PATH                   PIC X(30)   VALUE SPACES.
       01  JH-ALL-PATH.
           03  JH-ALL-BASE           PIC X(09).
           03  FILLER                REDEFINES JH-ALL-BASE.
               05  JH-ALL-BASE-USR   PIC X(05).
               05  JH-ALL-BASE-DIR   PIC X(04).
           03  FILLER                PIC X(01)   VALUE "/".
           03  JH-ALL-MACHINE        PIC X(02).
           03  FILLER                PIC X(18)   VALUE "/GL/JHFILE".
