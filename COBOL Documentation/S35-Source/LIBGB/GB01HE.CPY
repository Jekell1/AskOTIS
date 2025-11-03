      ***********************************
      *             (UT)
      *    HE FILE DEFINITION
      *           (80 BYTES)
      ***********************************
       01  HE-REC.
           03  HE-LINE            PIC X(80).
           03  HE-LINEX  REDEFINES HE-LINE.
               05  HE-CHAR        OCCURS 80
                                  PIC X(01).
