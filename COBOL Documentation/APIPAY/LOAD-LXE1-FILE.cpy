      *================================================================*
      * END COPYBOOK: LIBLP\LPLP1RN.CPY                                *
      *================================================================*
      *================================================================*
      * EMBEDDED COPYBOOK: LIBLP\LPLXE1RN.CPY                           *
      *================================================================*
      * COPYMEMBER: LIBLP/LPLXE1RN
       LOAD-LXE1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( LXE-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( LXE-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO LXE-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO LXE-ALL-MACHINE
              MOVE EXT-FILPATH-DATA        TO LXE-ALL-DATA
              MOVE LXE-ALL-PATH             TO LXE-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( LXE-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO LXE-ALL-BASE
              MOVE FILPATH-MACHINE         TO LXE-ALL-MACHINE
              MOVE FILPATH-DATA            TO LXE-ALL-DATA
              MOVE LXE-ALL-PATH             TO LXE-PATH.

      *-----------------------------------------------------------------
