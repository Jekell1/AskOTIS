      * COPYMEMBER: LIBAP/APWSVM
      ******************************************************************
      *    LIBAP/APWSVM: VMFILE PATH WORKERS
      *    /USR/DATA/R#/AP/VMFILE
      ******************************************************************
       01  VM-PATH-OVERRIDE          PIC X(01)   VALUE SPACES.
       01  VM-PATH                   PIC X(35)   VALUE SPACES.
       01  VM-ALL-PATH.
           03  VM-ALL-BASE           PIC X(09).
           03  FILLER                REDEFINES VM-ALL-BASE.
               05  VM-ALL-BASE-USR   PIC X(05).
               05  VM-ALL-BASE-DIR   PIC X(04).
           03  FILLER                PIC X(01)   VALUE "/".
           03  VM-ALL-MACHINE        PIC X(02).
           03  FILLER                PIC X(23)   VALUE "/AP/VMFILE".
