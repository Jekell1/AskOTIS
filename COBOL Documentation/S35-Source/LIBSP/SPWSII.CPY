      * COPYMEMBER: LIBSP/SPWSII
      ******************************************************************
      *    LIBSP/SPWSII: IIFILE PATH WORKERS
      *    II-PATH = /USR/DATA/XX/XX####/SP/IIFILE
      *              |----BEG----||-MID-||-----FILE----|
      *
      ******************************************************************
       01  II-PATH-OVERRIDE          PIC X(01)   VALUE SPACES.

       01  II-PATH                   PIC X(35)   VALUE SPACES.
       01  FILLER                    REDEFINES II-PATH.
           03  II-PATH-BEG           PIC X(13).
           03  II-PATH-FILE          PIC X(22).
           03  FILLER                REDEFINES II-PATH-FILE.
               05  FILLER            PIC X(03).
               05  II-PATH-SQL       PIC X(19).

       01  II-ALL-PATH.
           03  II-ALL-BASE           PIC X(09).
           03  FILLER                REDEFINES II-ALL-BASE.
               05  II-ALL-BASE-USR   PIC X(05).
               05  II-ALL-BASE-DIR   PIC X(04).
           03  FILLER                PIC X(01)   VALUE "/".
           03  II-ALL-MACHINE        PIC X(02).
           03  FILLER                PIC X(23)   VALUE "/GB/IIFILE".

