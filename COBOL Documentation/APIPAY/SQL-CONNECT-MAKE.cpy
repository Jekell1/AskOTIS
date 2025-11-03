      ******************************************************************
      *
      *    S Q L - C O N N E C T - M A K E
      *
      *=================================================================
      * IN  : SQL-CONNECT-WORKERS
      * OUT : SQLCA WORKERS
      * DESC: MAKE CONNECTION TO THE SQL SERVER
      ******************************************************************
       SQL-CONNECT-MAKE.

           EXEC SQL
                CONNECT TO :SQL-CONNECT-SERVER AS C1
                      USER :SQL-CONNECT-USERID USING :SQL-CONNECT-PSWD
           END-EXEC.

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
