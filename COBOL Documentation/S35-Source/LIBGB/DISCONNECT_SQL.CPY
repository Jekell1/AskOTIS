      * COPYMEMBER: LIBGB/DISCONNECT_SQL.CPY
      ******************************************************************
      *
      * IDENTIFICATION: LIBGB/DISCONNECT_SQL
      * DESCRIPTION   : SQL SERVER DISCONNECTION ROUTINE
      *
      *=================================================================
      * REV:
      * JKC 2021-0702 ***NEW*** NEEDED TO CREATE A ROUTINE THAT IS
      *               JUST THE SQL-DISCONNECT PARAGRAPH FOR WHEN EXITING
      *               THE APPLICATION VIA `STOP RUN` USUALLY IN MENU
      *               PROGRAMS THAT HAVE NOT SQL IO ROUTINES.
      *               WE FOUND THAT THE USE OF SQL-DISCONNECT WHEN
      *               EXITING THE APPLICATION IS ONLY NEEDED; DOING A
      *               SQL-DISCONNECT WHEN EXITING A PROGRAM WILL CAUSE
      *               ALL CURSORS TO BE CLOSED FROM THE CALLING PROGRAM
      *               WHICH EFFECTS IT WHEN CONTINUING WITH A READ-NEXT.
      *
      * JKC 2021-0817 UPDATED FROM XPC/IOCONN.PRE FOR LATEST CHANGES.
      * JKC 2022-0622 MOVED THIS FROM LIBIO TO LIBGB; LIBIO WAS REMOVED
      *
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
      
      *    MOVE SPACES     TO SQL-CONNECT-WORKERS.

      *    DECLARE PROCEDURE FOR SQL ERRORS WHEN THEY OCCUR
      *    NOTE: WHENEVER SQLERROR DOES NOT CATCH ALL ERRORS (LIKE A
      *          CASTING ERROR) SO WE STILL DO THE SQL-IO-VALIDATION
      *          AFTER EACH EXEC SQL IN ALL IO COPY MEMBERS!!!
      
           EXEC SQL WHENEVER SQLERROR
              PERFORM SQL-IO-VALIDATION
           END-EXEC.

