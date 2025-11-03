      * COPYMEMBER: LIBGB/GBWSER
      *********************************************************
      *        LIBGB/GBWSER: ERRLOG PATH WORKERS
      *        ERROR FILES ARE ERR.YYMMDD
      *        MAINTENANCE CHANGE FILES ARE CHG.YYMMDD
      * REV:
      * BLF 070116 ADDED ERRLOG-GL-PATH TO LOG GL STUFF IN
      *            /USR/DATA/R1/LG/GL.YYMMDD, SUPPORT PR# 2780
      * KEC 130425 CHANGED FOR FULL FILEPATHS (A30)
      *  CS 170323 ADD ERRLOG-BK-PATH WORLD PR#1234
      *********************************************************
       01  ERRLOG-PATH-OVERRIDE   PIC X(01)   VALUE SPACES.
       01  ERRLOG-PATH            PIC X(40)   VALUE SPACES.
       01  ERRLOG-PATH-FORMAT.
           03  ERRLOG-BASE        PIC  X(09).
           03  FILLER             REDEFINES ERRLOG-BASE.
               05  ERRLOG-BASE-USR   PIC X(05).
               05  ERRLOG-BASE-DIR   PIC X(04).
           03  FILLER                PIC X(01)   VALUE "/".
           03  ERRLOG-MACHINE     PIC X(02).
           03  FILLER             PIC X(4)  VALUE "/LG/".
           03  ERRLOG-LOG-TYPE    PIC XXX.
           03  FILLER             PIC X     VALUE ".".
           03  ERRLOG-DATE.
               05  ERRLOG-YY      PIC 99.
               05  ERRLOG-MM      PIC 99.
               05  ERRLOG-DD      PIC 99.

       01  ERRLOG-GL-PATH         PIC X(40)   VALUE SPACES.
       01  ERRLOG-GL-PATH-FORMAT.
           03  ERRLOG-GL-BASE        PIC  X(09).
           03  FILLER             REDEFINES ERRLOG-GL-BASE.
               05  ERRLOG-GL-BASE-USR   PIC X(05).
               05  ERRLOG-GL-BASE-DIR   PIC X(04).
           03  FILLER                PIC X(01)   VALUE "/".
           03  ERRLOG-GL-MACHINE     PIC X(02).
           03  FILLER             PIC X(4)  VALUE "/LG/".
           03  ERRLOG-GL-LOG-TYPE PIC XX    VALUE "GL".
           03  FILLER             PIC X     VALUE ".".
           03  ERRLOG-GL-DATE.
               05  ERRLOG-GL-YY   PIC 99.
               05  ERRLOG-GL-MM   PIC 99.
               05  ERRLOG-GL-DD   PIC 99.

       01  ERRLOG-BK-PATH-FORMAT.
           03  ERRLOG-BK-BASE        PIC  X(09).
           03  FILLER             REDEFINES ERRLOG-BK-BASE.
               05  ERRLOG-BK-BASE-USR   PIC X(05).
               05  ERRLOG-BK-BASE-DIR   PIC X(04).
           03  FILLER                PIC X(01)   VALUE "/".
           03  ERRLOG-BK-MACHINE     PIC X(02).
           03  FILLER                PIC X(4)  VALUE "/LG/".
           03  ERRLOG-BK-LOG-TYPE    PIC XX   VALUE "BK".
           03  FILLER                PIC X     VALUE ".".
           03  ERRLOG-BK-DATE.
               05  ERRLOG-BK-YY      PIC 99.
               05  ERRLOG-BK-MM      PIC 99.
