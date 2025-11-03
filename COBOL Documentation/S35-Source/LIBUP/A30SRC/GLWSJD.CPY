      * COPYMEMBER: LIBGL/GLWSJD
      *********************************************************
      *        LIBGL/GLWSJD: JDFILE PATH WORKERS
      *        /USR/DATA/XX/GL/JDFILE
      *********************************************************
       01  JD-PATH-OVERRIDE          PIC X(01)   VALUE SPACES.
       01  JD-PATH                   PIC X(30)   VALUE SPACES.
       01  JD-ALL-PATH.
           03  JD-ALL-BASE           PIC X(09).
           03  FILLER                REDEFINES JD-ALL-BASE.
               05  JD-ALL-BASE-USR   PIC X(05).
               05  JD-ALL-BASE-DIR   PIC X(04).
           03  FILLER                PIC X(01)   VALUE "/".
           03  JD-ALL-MACHINE        PIC X(02).
           03  FILLER                PIC X(18)   VALUE "/GL/JDFILE".
