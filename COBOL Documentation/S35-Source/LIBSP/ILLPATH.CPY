      * COPYMEMBER: LIBSP/ILLPATH_CMD
      *****************************************************************
      *  SPECIAL WORKERS FOR ILNEXT & ILUEXT (ILLINOIS LOAN EXTRACTIONS)
      *****************************************************************
       01  ILN-PATH-OVERRIDE          PIC X(01)   VALUE SPACES.
       01  ILN-PATH                   PIC X(50)   VALUE SPACES.
       01  ILN-PATH-FORMAT.
           03  ILN-PATH-BASE          PIC X(09).
           03  FILLER                 REDEFINES ILN-PATH-BASE.
               05  ILN-PATH-BASE-USR  PIC X(05).
               05  ILN-PATH-BASE-DIR  PIC X(04).
           03  FILLER                 PIC X(01) VALUE "/".
           03  ILN-PATH-MACHINE       PIC X(02).
           03  ILN-FTP-PATH           PIC X(9)  VALUE "/FTP/ILL/".
           03  FILLER                 PIC X(7)  VALUE "WFCILN.".
           03  ILN-PATH-DATE          PIC 9(6).
           03  FILLER REDEFINES ILN-PATH-DATE.
               05  ILN-PATH-YY        PIC 99.
               05  ILN-PATH-MM        PIC 99.
               05  ILN-PATH-DD        PIC 99.

      * STANDARD UPDATES
       01  ILU-PATH-OVERRIDE          PIC X(01)   VALUE SPACES.
       01  ILU-PATH                   PIC X(50)   VALUE SPACES.
       01  ILU-PATH-FORMAT.
           03  ILU-PATH-BASE          PIC X(09).
           03  FILLER                 REDEFINES ILU-PATH-BASE.
               05  ILU-PATH-BASE-USR  PIC X(05).
               05  ILU-PATH-BASE-DIR  PIC X(04).
           03  FILLER                 PIC X(01) VALUE "/".
           03  ILU-PATH-MACHINE       PIC X(02).
           03  ILU-FTP-PATH           PIC X(9)  VALUE "/FTP/ILL/".
           03  FILLER                 PIC X(7)  VALUE "WFCILU.".
           03  ILU-PATH-DATE          PIC 9(6).
           03  FILLER REDEFINES ILU-PATH-DATE.
               05  ILU-PATH-YY        PIC 99.
               05  ILU-PATH-MM        PIC 99.
               05  ILU-PATH-DD        PIC 99.

      * MISPAY UPDATES
       01  ILM-PATH-OVERRIDE          PIC X(01)   VALUE SPACES.
       01  ILM-PATH                   PIC X(50)   VALUE SPACES.
       01  ILM-PATH-FORMAT.
           03  ILM-PATH-BASE          PIC X(09).
           03  FILLER                 REDEFINES ILM-PATH-BASE.
               05  ILM-PATH-BASE-USR  PIC X(05).
               05  ILM-PATH-BASE-DIR  PIC X(04).
           03  FILLER                 PIC X(01) VALUE "/".
           03  ILM-PATH-MACHINE       PIC X(02).
           03  ILM-FTP-PATH           PIC X(9)  VALUE "/FTP/ILL/".
           03  FILLER                 PIC X(7)  VALUE "WFCILM.".
           03  ILM-PATH-DATE          PIC 9(6).
           03  FILLER REDEFINES ILM-PATH-DATE.
               05  ILM-PATH-YY        PIC 99.
               05  ILM-PATH-MM        PIC 99.
               05  ILM-PATH-DD        PIC 99.
