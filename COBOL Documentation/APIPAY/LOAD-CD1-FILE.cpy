      *================================================================*
      * END COPYBOOK: LIBGL\GLGI1RN.CPY                                *
      *================================================================*
      *================================================================*
      * EMBEDDED COPYBOOK: LIBLP\LPCD1RN.CPY                           *
      *================================================================*
      * COPYMEMBER: LIBLP/LPCD1RN
       LOAD-CD1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( CD-PATH-OVERRIDE = "Y" )
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( CD-PATH-OVERRIDE = SPACES )
              MOVE EXT-FILPATH-BASE        TO CD-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO CD-ALL-MACHINE
              MOVE CD-ALL-PATH             TO CD-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( CD-PATH-OVERRIDE = "B" )
              MOVE FILPATH-BASE            TO CD-ALL-BASE
              MOVE FILPATH-MACHINE         TO CD-ALL-MACHINE
              MOVE CD-ALL-PATH             TO CD-PATH.

      *-----------------------------------------------------------------
