      ******************************************************************
      *
      *    S Q L - C O N N E C T - E R R O R
      *
      *=================================================================
      * IN  : SQLCA WORKERS
      * OUT : NONE
      * DESC: PROCESS ERROR FOR SQL SERVER CONNECTION
      ******************************************************************
       SQL-CONNECT-ERROR.

           MOVE "CONNECTION"            TO E-MSG.
           MOVE "SQL SERVER CONNECTION" TO E-FILE.
           PERFORM SQL-ERROR.

      ******************************************************************
      *
      *    S Q L - C O N N E C T - M A K E
      *
      *=================================================================
      * IN  : SQL-CONNECT-WORKERS
      * OUT : SQLCA WORKERS
      * DESC: MAKE CONNECTION TO THE SQL SERVER
      ******************************************************************
