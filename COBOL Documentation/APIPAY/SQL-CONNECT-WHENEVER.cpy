      ******************************************************************
      *
      *    S Q L - C O N N E C T - W H E N E V E R
      *
      *=================================================================
      * IN  : NONE
      * OUT : NONE
      * DESC: DECLARE PROCEDURE FOR SQL ERRORS WHEN THEY OCCUR
      *       GPFILE SINCE IT SHOULD BE QUICK WITH ONLY HAVING ONE ROW.
      * NOTE: WHENEVER SQLERROR DOES NOT CATCH ALL ERRORS (LIKE A
      *          CASTING ERROR) SO WE STILL DO THE SQL-IO-VALIDATION
      *          AFTER EACH EXEC SQL IN ALL IO COPY MEMBERS!!!
      ******************************************************************
       SQL-CONNECT-WHENEVER.

           EXEC SQL WHENEVER SQLERROR
              PERFORM SQL-IO-VALIDATION
           END-EXEC.

      ******************************************************************
      *
      *    S Q L - D I S C O N N E C T
      *
      *=================================================================
      * IN  : NOTHING
      * OUT : SQL-CONNECT-WORKERS
      * DESC: MAKE CONNECTION TO THE SQL SERVER
      *=================================================================
      * ***NOTE*** NEED TO HAVE SQL-DISCONNECT IN LIBGB/CONNECT_SQL AND
      *            LIBGB/DISCONNECT_SQL
      ******************************************************************
