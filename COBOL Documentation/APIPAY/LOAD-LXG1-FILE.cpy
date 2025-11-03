      *================================================================*
      * END COPYBOOK: LIBLP\LPLXE1RN.CPY                                *
      *================================================================*
      *================================================================*
      * EMBEDDED COPYBOOK: LIBLP\LPLXG1RN.CPY                           *
      *================================================================*
      * COPYMEMBER: LIBLP/LPLXGE1RN
       LOAD-LXG1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( LXG-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( LXG-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO LXG-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO LXG-ALL-MACHINE
              MOVE EXT-FILPATH-DATA        TO LXG-ALL-DATA
              MOVE LXG-ALL-PATH             TO LXG-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( LXG-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO LXG-ALL-BASE
              MOVE FILPATH-MACHINE         TO LXG-ALL-MACHINE
              MOVE FILPATH-DATA            TO LXG-ALL-DATA
              MOVE LXG-ALL-PATH             TO LXG-PATH.

      *-----------------------------------------------------------------
