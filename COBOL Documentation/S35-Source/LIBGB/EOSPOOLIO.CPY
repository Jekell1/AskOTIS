      * COPYMEMBER: LIBLP/EOSPOOLIO
      *-----------------------------------------------------------------
       LOAD-EO-SPOOL.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( EO-SPOOL-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( EO-SPOOL-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO EO-SPOOL-BASE
              MOVE EXT-FILPATH-MACHINE     TO EO-SPOOL-MACHINE
              MOVE EXT-FILPATH-DATA        TO EO-SPOOL-DATA
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( EO-SPOOL-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO EO-SPOOL-BASE
              MOVE FILPATH-MACHINE         TO EO-SPOOL-MACHINE
              MOVE FILPATH-DATA            TO EO-SPOOL-DATA.

