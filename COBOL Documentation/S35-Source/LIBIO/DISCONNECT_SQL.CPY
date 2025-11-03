      * COPYMEMBER: LIBIO/DISCONNECT_SQL.CPY
      ******************************************************************
      *
      * IDENTIFICATION: LIBIO/DISCONNECT_SQL
      * DESCRIPTION   : SQL SERVER DISCONNECTION ROUTINE
      * PRE-COMPILE   : IO/IOCONN.CBL
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
      * ***NOTE*** NEED TO HAVE SQL-DISCONNECT IN LIBIO/CONNECT_SQL AND
      *            LIBIO/DISCONNECT_SQL
      ******************************************************************
       SQL-DISCONNECT.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    NEED TO HAVE WHENEVER SQLERROR NOT CALL TO SQL-IO-VALIDATION
      *    FOR CLOSING THE CONNECTION. THE USE IN LIBIO/DISCONNECT_SQL
      *    WILL NOT HAVE SQL-IO-VALIDATION SO NEED TO SHUT IT OFF
      *    (CONTINUE) AND DISCONNECT THEN PUT BACK.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           
           CONTINUE
           .

           
           PERFORM CALL "SQL$START" END-CALL 

           CALL "SQL$COMMIT"  END-CALL 

            END-PERFORM
           .
           
           PERFORM CALL "SQL$START" END-CALL 

           CALL "SQL$DISCONNECTALL"  END-CALL 

            END-PERFORM
           .

      *KC  IF ( SQLCODE < 0 )     *> NO NEED TO CHECK SINCE EXITING
      *KC     PERFORM SQL-ERROR.  *> APPLICATION VIA `STOP RUN`

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    NEED THE FOLLOWING FOR LIBIO/CONNECT_SQL
      *    NEED TO COMMENT OUT THE FOLLOWING FOR LIBIO/DISCONNECT_SQL
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    MOVE SPACES     TO SQL-CONNECT-WORKERS.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    DECLARE PROCEDURE FOR SQL ERRORS WHEN THEY OCCUR
      *    NOTE: WHENEVER SQLERROR DOES NOT CATCH ALL ERRORS (LIKE A
      *          CASTING ERROR) SO WE STILL DO THE SQL-IO-VALIDATION
      *          AFTER EACH EXEC SQL IN ALL IO COPY MEMBERS!!!
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           
           CONTINUE
           .

