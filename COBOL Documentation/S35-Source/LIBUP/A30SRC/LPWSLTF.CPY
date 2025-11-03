      * COPYMEMBER: LIBLP/LPWSLTF
      *********************************************************
      *        LIBLP/LPWSLTF: FEE CODE PAYABLE PATH WORKERS
      *        /USR/DATA/XX/XX####/LP/LTFFILE
      *********************************************************
       01  LTF-PATH-OVERRIDE   PIC X(01)  VALUE SPACES.
       01  LTF-PATH            PIC X(35)  VALUE SPACES.
       01  LTF-ALL-PATH.
           03  LTF-ALL-BASE           PIC X(09).
           03  FILLER REDEFINES LTF-ALL-BASE.
               05  LTF-ALL-BASE-USR   PIC X(05).
               05  LTF-ALL-BASE-DIR   PIC X(04).
           03  FILLER                 PIC X(01)  VALUE "/".
           03  LTF-ALL-MACHINE        PIC X(02).
           03  FILLER                 PIC X(01)  VALUE "/".
           03  LTF-ALL-DATA           PIC X(06).
           03  FILLER                 PIC X(11)  VALUE "/LP/LTFFILE".
