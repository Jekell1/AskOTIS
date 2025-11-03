      * COPYMEMBER: LIBLP/LPEDPATHIO
      *-----------------------------------------------------------------
       LOAD-OPEN-FILE.

      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( OPEN-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( OPEN-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO OPEN-PATH-BASE
              MOVE EXT-FILPATH-MACHINE     TO OPEN-PATH-MACHINE
              MOVE OPEN-PATH-FORMAT        TO OPEN-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( OPEN-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO OPEN-PATH-BASE
              MOVE FILPATH-MACHINE         TO OPEN-PATH-MACHINE
              MOVE OPEN-PATH-FORMAT        TO OPEN-PATH.

      *-----------------------------------------------------------------
       LOAD-CLOSE-FILE.

      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
      *    IF ( CLOSE-PATH-OVERRIDE = "Y" ) 
      *       NEXT SENTENCE
      *    ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
      *    IF ( CLOSE-PATH-OVERRIDE = SPACES ) 
      *       MOVE EXT-FILPATH-BASE        TO CLOSE-PATH-BASE
      *       MOVE EXT-FILPATH-MACHINE     TO CLOSE-PATH-MACHINE
      *       MOVE CLOSE-PATH-FORMAT       TO CLOSE-PATH
      *    ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
      *    IF ( CLOSE-PATH-OVERRIDE = "B" ) 
      *       MOVE FILPATH-BASE            TO CLOSE-PATH-BASE
      *       MOVE FILPATH-MACHINE         TO CLOSE-PATH-MACHINE
      *       MOVE CLOSE-PATH-FORMAT       TO CLOSE-PATH.

      *-----------------------------------------------------------------
       LOAD-UPDATE-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
      *    IF ( UPDATE-PATH-OVERRIDE = "Y" ) 
      *       NEXT SENTENCE
      *    ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
      *    IF ( UPDATE-PATH-OVERRIDE = SPACES ) 
      *       MOVE EXT-FILPATH-BASE        TO UPDATE-PATH-BASE
      *       MOVE EXT-FILPATH-MACHINE     TO UPDATE-PATH-MACHINE
      *       MOVE UPDATE-PATH-FORMAT      TO UPDATE-PATH
      *    ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
      *    IF ( UPDATE-PATH-OVERRIDE = "B" ) 
      *       MOVE FILPATH-BASE            TO UPDATE-PATH-BASE
      *       MOVE FILPATH-MACHINE         TO UPDATE-PATH-MACHINE
      *       MOVE UPDATE-PATH-FORMAT      TO UPDATE-PATH.
