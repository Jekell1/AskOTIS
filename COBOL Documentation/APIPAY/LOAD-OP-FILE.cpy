      *================================================================*
      * END COPYBOOK: LIBGB\GBRC2RN.CPY                                *
      *================================================================*
      *================================================================*
      * EMBEDDED COPYBOOK: LIBLP\LPOPIN.CPY                           *
      *================================================================*
      * COPYMEMBER: LIBLP/LPOPIN
       LOAD-OP-FILE.
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
