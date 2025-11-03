      *================================================================*
      * END COPYBOOK: LIBLP\LPLN1RN.CPY                                *
      *================================================================*
      *================================================================*
      * EMBEDDED COPYBOOK: LIBLP\LPLTI1RN.CPY                           *
      *================================================================*
      * COPYMEMBER: LIBLP/LPLTI1RN
       LOAD-LTI1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( LTI-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( LTI-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO LTI-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO LTI-ALL-MACHINE
              MOVE EXT-FILPATH-DATA        TO LTI-ALL-DATA
              MOVE LTI-ALL-PATH             TO LTI-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( LTI-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO LTI-ALL-BASE
              MOVE FILPATH-MACHINE         TO LTI-ALL-MACHINE
              MOVE FILPATH-DATA            TO LTI-ALL-DATA
              MOVE LTI-ALL-PATH             TO LTI-PATH.

      *-----------------------------------------------------------------
