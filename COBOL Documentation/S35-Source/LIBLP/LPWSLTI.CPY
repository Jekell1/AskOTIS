      * COPYMEMBER: LIBLP/LPWSLTI
      *********************************************************
      *        LIBLP/LPWSLTI: INSURANCE ADDON PATH WORKERS
      *        /USR/DATA/XX/XX####/LP/LTIFILE
      *********************************************************
       01  LTI-PATH-OVERRIDE   PIC X(01)  VALUE SPACES.
       01  LTI-PATH            PIC X(35)  VALUE SPACES.
       01  FILLER                    REDEFINES LTI-PATH.
           03  LTI-PATH-BEG           PIC X(13).
           03  LTI-PATH-OWNST         PIC X(02).
           03  LTI-PATH-OWNBR         PIC 9(04).
           03  FILLER                PIC X(01).
           03  LTI-PATH-FILE          PIC X(15).
           03  FILLER                REDEFINES LTI-PATH-FILE.
               05  FILLER            PIC X(03).
               05  LTI-PATH-SQL       PIC X(12).


       01  LTI-ALL-PATH.
           03  LTI-ALL-BASE           PIC X(09).
           03  FILLER REDEFINES LTI-ALL-BASE.
               05  LTI-ALL-BASE-USR   PIC X(05).
               05  LTI-ALL-BASE-DIR   PIC X(04).
           03  FILLER                 PIC X(01)  VALUE "/".
           03  LTI-ALL-MACHINE        PIC X(02).
           03  FILLER                 PIC X(01)  VALUE "/".
           03  LTI-ALL-DATA           PIC X(06).
           03  FILLER                 PIC X(11)  VALUE "/LP/LTIFILE".

       01  LTISUB                    PIC 9(3).
