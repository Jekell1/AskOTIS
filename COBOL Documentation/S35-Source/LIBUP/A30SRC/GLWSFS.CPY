      * COPYMEMBER: LIBGL/GLWSFS
      *********************************************************
      *        LIBGL/GLWSFS: FSFILE PATH WORKERS
      *        /USR/DATA/XX/GL/FSFILE
      *********************************************************
       01  FS-PATH-OVERRIDE          PIC X(01)   VALUE SPACES.
       01  FS-PATH                   PIC X(35)   VALUE SPACES.
       01  FS-ALL-PATH.
           03  FS-ALL-BASE           PIC X(09).
           03  FILLER                REDEFINES FS-ALL-BASE.
               05  FS-ALL-BASE-USR   PIC X(05).
               05  FS-ALL-BASE-DIR   PIC X(04).
           03  FILLER                PIC X(01)   VALUE "/".
           03  FS-ALL-MACHINE        PIC X(02).
           03  FILLER                PIC X(23)   VALUE "/GL/FSFILE".
