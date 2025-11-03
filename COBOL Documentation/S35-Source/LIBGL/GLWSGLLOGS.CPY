      * COPYMEMBER: LIBGL/GLWSGLLOGS
      *********************************************************
      *        LIBGL/GLWSGLLOGS: GLLOGS  PATH WORKERS
      *        /USR/DATA/XX/GL/GLLOGS
      *  CS 20190716 *NEW* TO LOG WHEN GL/ENDMTH IS RUN
      *  CS 20190719 CHANGED NAME OF FILE TO GLLOGS
      *********************************************************
       01  GLLOGS-PATH-OVERRIDE          PIC X(01)   VALUE SPACES.
       01  GLLOGS-PATH                   PIC X(35)   VALUE SPACES.
       01  FILLER                    REDEFINES GLLOGS-PATH.
           03  GLLOGS-PATH-BEG           PIC X(13).
           03  GLLOGS-PATH-FILE          PIC X(22).
           03  FILLER                REDEFINES GLLOGS-PATH-FILE.
               05  FILLER                PIC X(03).
               05  GLLOGS-PATH-SQL       PIC X(19).
 
       01  GLLOGS-ALL-PATH.
           03  GLLOGS-ALL-BASE           PIC X(09).
           03  FILLER                REDEFINES GLLOGS-ALL-BASE.
               05  GLLOGS-ALL-BASE-USR   PIC X(05).
               05  GLLOGS-ALL-BASE-DIR   PIC X(04).
           03  FILLER                PIC X(01)   VALUE "/".
           03  GLLOGS-ALL-MACHINE        PIC X(02).
           03  FILLER           PIC X(23)   VALUE "/GL/GLLOGSFILE".
