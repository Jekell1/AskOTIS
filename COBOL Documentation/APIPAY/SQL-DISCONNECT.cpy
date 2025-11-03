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
       SQL-DISCONNECT.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    NEED TO HAVE WHENEVER SQLERROR NOT CALL TO SQL-IO-VALIDATION
      *    FOR CLOSING THE CONNECTION. THE USE IN LIBGB/DISCONNECT_SQL
      *    WILL NOT HAVE SQL-IO-VALIDATION SO NEED TO SHUT IT OFF
      *    (CONTINUE) AND DISCONNECT THEN PUT BACK.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           
           EXEC SQL WHENEVER SQLERROR CONTINUE END-EXEC.

           EXEC SQL COMMIT END-EXEC.
           EXEC SQL DISCONNECT ALL END-EXEC.

      * NO NEED TO CHECK SINCE EXITING APPLICATION VIA 'STOP RUN'
      *    IF ( SQLCODE < 0 )   
      *       PERFORM SQL-ERROR. 

      * NEED THE FOLLOWING FOR LIBGB/CONNECT_SQL
      * NEED TO COMMENT OUT THE FOLLOWING FOR LIBGB/DISCONNECT_SQL
      
           MOVE SPACES     TO SQL-CONNECT-WORKERS.
           MOVE STAT---BAD TO EXT-ACUSQL-CONNECT-STAT.

      * DECLARE PROCEDURE FOR SQL ERRORS WHEN THEY OCCUR
      * NOTE: WHENEVER SQLERROR DOES NOT CATCH ALL ERRORS (LIKE A
      *       CASTING ERROR) SO WE STILL DO THE SQL-IO-VALIDATION
      *       AFTER EACH EXEC SQL IN ALL IO COPY MEMBERS!!!
           
           EXEC SQL WHENEVER SQLERROR
              PERFORM SQL-IO-VALIDATION
           END-EXEC.

      ******************************************************************
      *
      *    S Q L - D E B U G
      *
      *=================================================================
      * IN  : SQLCA
      * OUT : NOTHING
      * DESC: DISPLAY MESSAGE BOX OF SQLCA SETTINGS FOR DEBUG
      ******************************************************************
