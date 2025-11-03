      * COPYMEMBER: LIBGB/RLOGINW
       01  RLOGIN-BUF.
           03  RLOGIN-FLAGS.
               05  RLOGIN-FG              PIC X.
               05  RLOGIN-ALLOW-REMOTE    PIC X.
               05  RLOGIN-ALLOW-LOCAL     PIC X.
               05  RLOGIN-STATUS.
                   07  RLOGIN-IS-REMOTE   PIC X.
                   07  RLOGIN-IS-LOCAL    PIC X.
           03  RLOGIN-HOST                PIC X(1).
           03  RLOGIN-USERID.
               05  RLOGIN-SECURITY        PIC X.
               05  RLOGIN-BR              PIC 9(4).
               05  RLOGIN-NAME            PIC X(5).
