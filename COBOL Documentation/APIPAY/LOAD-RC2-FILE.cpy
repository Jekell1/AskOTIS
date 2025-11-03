      *================================================================*
      * END COPYBOOK: LIBLP\LPWKI.CPY                                *
      *================================================================*
      *================================================================*
      * EMBEDDED COPYBOOK: LIBGB\GBRC2RN.CPY                           *
      *================================================================*
      * COPYMEMBER: LIBGB/GBRC2RN
       LOAD-RC2-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( RC-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( RC-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO RC-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO RC-ALL-MACHINE
              MOVE RC-ALL-PATH             TO RC-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( RC-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO RC-ALL-BASE
              MOVE FILPATH-MACHINE         TO RC-ALL-MACHINE
              MOVE RC-ALL-PATH             TO RC-PATH.

      *-----------------------------------------------------------------
