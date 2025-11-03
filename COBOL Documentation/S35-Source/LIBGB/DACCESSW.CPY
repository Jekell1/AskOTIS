      * REV:
      *  130424 BAH ADDED DIR-ACCESS-SOURCE AND TARGET
      *  140519 BLM CHANGED COMMAND TO DO LS -LD INSTEAD OF CALLING SHELL
      **********************************************************************
      * LIBGB/DACCESSW: WORKERS FOR DACCESS, CHECK IF DIRECTORY EXISTS
      **********************************************************************
       01  CK-DIR-EXISTS-CMD.
           03  CK-DIR-LS      PIC X(7)  VALUE "LS -LD ".
           03  DIR-ACCESS-BUF PIC X(40).
           03  FILLER         PIC X(4)  VALUE " 1>/".
           03  CK-DIR-DEVNULL PIC X(8).

       01  DIR-ACCESS-SOURCE           PIC X(40).
       01  DIR-ACCESS-TARGET           PIC X(40).
