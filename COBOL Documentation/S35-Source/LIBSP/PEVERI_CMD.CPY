      * COPYMEMBER: LIBWI/PEVERI.CMD #1659 s35q-60

       01  GREP-BUF.
           03  FILLER          PIC X(5)  VALUE "grep ".
           03  GREP-USERID     PIC X(10).
           03  FILLER          PIC X(15) VALUE " /etc/passwd > ".
           03  GREP-WK-PATH    PIC X(15).
 
       01  RM-CMD              PIC XX VALUE "rm".
