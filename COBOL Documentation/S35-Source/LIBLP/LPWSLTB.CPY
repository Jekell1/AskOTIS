      * COPYMEMBER: LIBLP/LPWSLTB
      *********************************************************
      *        LIBLP/LPWSLTB: BANKRUPTCY PATH WORKERS
      *        /USR/DATA/XX/XX####/LP/LTBFILE
      *********************************************************
       01  LTB-PATH-OVERRIDE   PIC X(01)  VALUE SPACES.
       01  LTB-PATH            PIC X(35)  VALUE SPACES.
       01  FILLER                    REDEFINES LTB-PATH.
           03  LTB-PATH-BEG           PIC X(13).
           03  LTB-PATH-OWNST         PIC X(02).
           03  LTB-PATH-OWNBR         PIC 9(04).
           03  FILLER                PIC X(01).
           03  LTB-PATH-FILE          PIC X(15).
           03  FILLER                REDEFINES LTB-PATH-FILE.
               05  FILLER            PIC X(03).
               05  LTB-PATH-SQL       PIC X(12).

       01  LTB-ALL-PATH.
           03  LTB-ALL-BASE           PIC X(09).
           03  FILLER REDEFINES LTB-ALL-BASE.
               05  LTB-ALL-BASE-USR   PIC X(05).
               05  LTB-ALL-BASE-DIR   PIC X(04).
           03  FILLER                 PIC X(01)  VALUE "/".
           03  LTB-ALL-MACHINE        PIC X(02).
           03  FILLER                 PIC X(01)  VALUE "/".
           03  LTB-ALL-DATA           PIC X(06).
           03  FILLER                 PIC X(11)  VALUE "/LP/LTBFILE".

       01  LTBSUB                    PIC 9(3).
